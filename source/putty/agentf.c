/*
 * SSH agent forwarding.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"
#include "pageant.h"
#include "sshchan.h"

typedef struct agentf {
    SshChannel *c;
    bufchain inbuffer;
    agent_pending_query *pending;
    bool input_wanted;
    bool rcvd_eof;

    Channel chan;
} agentf;

static void agentf_got_response(agentf *af, void *reply, int replylen)
{
    af->pending = NULL;

    if (!reply) {
        /* The real agent didn't send any kind of reply at all for
         * some reason, so fake an SSH_AGENT_FAILURE. */
        reply = "\0\0\0\1\5";
        replylen = 5;
    }

    sshfwd_write(af->c, reply, replylen);
}

static void agentf_callback(void *vctx, void *reply, int replylen);

static void agentf_try_forward(agentf *af)
{
    size_t datalen, length;
    strbuf *message;
    unsigned char msglen[4];
    void *reply;
    int replylen;

    /*
     * Don't try to parallelise agent requests. Wait for each one to
     * return before attempting the next.
     */
    if (af->pending)
        return;

    /*
     * If the outgoing side of the channel connection is currently
     * throttled, don't submit any new forwarded requests to the real
     * agent. This causes the input side of the agent forwarding not
     * to be emptied, exerting the required back-pressure on the
     * remote client, and encouraging it to read our responses before
     * sending too many more requests.
     */
    if (!af->input_wanted)
        return;

    while (1) {
        /*
         * Try to extract a complete message from the input buffer.
         */
        datalen = bufchain_size(&af->inbuffer);
        if (datalen < 4)
            break;         /* not even a length field available yet */

        bufchain_fetch(&af->inbuffer, msglen, 4);
        length = GET_32BIT_MSB_FIRST(msglen);

        if (length > AGENT_MAX_MSGLEN-4) {
            /*
             * If the remote has sent a message that's just _too_
             * long, we should reject it in advance of seeing the rest
             * of the incoming message, and also close the connection
             * for good measure (which avoids us having to faff about
             * with carefully ignoring just the right number of bytes
             * from the overlong message).
             */
            agentf_got_response(af, NULL, 0);
            sshfwd_write_eof(af->c);
            return;
        }

        if (length > datalen - 4)
            break;          /* a whole message is not yet available */

        bufchain_consume(&af->inbuffer, 4);

        message = strbuf_new_for_agent_query();
        bufchain_fetch_consume(
            &af->inbuffer, strbuf_append(message, length), length);
        af->pending = agent_query(
            message, &reply, &replylen, agentf_callback, af);
        strbuf_free(message);

        if (af->pending)
            return;   /* agent_query promised to reply in due course */

        /*
         * If the agent gave us an answer immediately, pass it
         * straight on and go round this loop again.
         */
        agentf_got_response(af, reply, replylen);
        sfree(reply);
    }

    /*
     * If we get here (i.e. we left the above while loop via 'break'
     * rather than 'return'), that means we've determined that the
     * input buffer for the agent forwarding connection doesn't
     * contain a complete request.
     *
     * So if there's potentially more data to come, we can return now,
     * and wait for the remote client to send it. But if the remote
     * has sent EOF, it would be a mistake to do that, because we'd be
     * waiting a long time. So this is the moment to check for EOF,
     * and respond appropriately.
     */
    if (af->rcvd_eof)
        sshfwd_write_eof(af->c);
}

static void agentf_callback(void *vctx, void *reply, int replylen)
{
    agentf *af = (agentf *)vctx;

    agentf_got_response(af, reply, replylen);
    sfree(reply);

    /*
     * Now try to extract and send further messages from the channel's
     * input-side buffer.
     */
    agentf_try_forward(af);
}

static void agentf_free(Channel *chan);
static size_t agentf_send(Channel *chan, bool is_stderr, const void *, size_t);
static void agentf_send_eof(Channel *chan);
static char *agentf_log_close_msg(Channel *chan);
static void agentf_set_input_wanted(Channel *chan, bool wanted);

static const struct ChannelVtable agentf_channelvt = {
    agentf_free,
    chan_remotely_opened_confirmation,
    chan_remotely_opened_failure,
    agentf_send,
    agentf_send_eof,
    agentf_set_input_wanted,
    agentf_log_close_msg,
    chan_default_want_close,
    chan_no_exit_status,
    chan_no_exit_signal,
    chan_no_exit_signal_numeric,
    chan_no_run_shell,
    chan_no_run_command,
    chan_no_run_subsystem,
    chan_no_enable_x11_forwarding,
    chan_no_enable_agent_forwarding,
    chan_no_allocate_pty,
    chan_no_set_env,
    chan_no_send_break,
    chan_no_send_signal,
    chan_no_change_window_size,
    chan_no_request_response,
};

Channel *agentf_new(SshChannel *c)
{
    agentf *af = snew(agentf);
    af->c = c;
    af->chan.vt = &agentf_channelvt;
    af->chan.initial_fixed_window_size = 0;
    af->rcvd_eof = false;
    bufchain_init(&af->inbuffer);
    af->pending = NULL;
    af->input_wanted = true;
    return &af->chan;
}

static void agentf_free(Channel *chan)
{
    assert(chan->vt == &agentf_channelvt);
    agentf *af = container_of(chan, agentf, chan);

    if (af->pending)
        agent_cancel_query(af->pending);
    bufchain_clear(&af->inbuffer);
    sfree(af);
}

static size_t agentf_send(Channel *chan, bool is_stderr,
                          const void *data, size_t length)
{
    assert(chan->vt == &agentf_channelvt);
    agentf *af = container_of(chan, agentf, chan);
    bufchain_add(&af->inbuffer, data, length);
    agentf_try_forward(af);

    /*
     * We exert back-pressure on an agent forwarding client if and
     * only if we're waiting for the response to an asynchronous agent
     * request. This prevents the client running out of window while
     * receiving the _first_ message, but means that if any message
     * takes time to process, the client will be discouraged from
     * sending an endless stream of further ones after it.
     */
    return (af->pending ? bufchain_size(&af->inbuffer) : 0);
}

static void agentf_send_eof(Channel *chan)
{
    assert(chan->vt == &agentf_channelvt);
    agentf *af = container_of(chan, agentf, chan);

    af->rcvd_eof = true;

    /* Call try_forward, which will respond to the EOF now if
     * appropriate, or wait until the queue of outstanding requests is
     * dealt with if not. */
    agentf_try_forward(af);
}

static char *agentf_log_close_msg(Channel *chan)
{
    return dupstr("Agent-forwarding connection closed");
}

static void agentf_set_input_wanted(Channel *chan, bool wanted)
{
    assert(chan->vt == &agentf_channelvt);
    agentf *af = container_of(chan, agentf, chan);

    af->input_wanted = wanted;

    /* Agent forwarding channels are buffer-managed by not asking the
     * agent questions if the SSH channel isn't accepting input. So if
     * it's started again, we should ask a question if we have one
     * pending.. */
    if (wanted)
        agentf_try_forward(af);
}
