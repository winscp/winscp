/*
 * Packet protocol layer for the SSH-1 'connection protocol', i.e.
 * everything after authentication finishes.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshppl.h"
#include "sshchan.h"
#include "sshcr.h"
#include "ssh1connection.h"

static int ssh1_rportfwd_cmp(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;
    int i;
    if ( (i = strcmp(a->dhost, b->dhost)) != 0)
	return i < 0 ? -1 : +1;
    if (a->dport > b->dport)
	return +1;
    if (a->dport < b->dport)
	return -1;
    return 0;
}

static void ssh1_connection_free(PacketProtocolLayer *); 
static void ssh1_connection_process_queue(PacketProtocolLayer *);
static void ssh1_connection_special_cmd(PacketProtocolLayer *ppl,
                                        SessionSpecialCode code, int arg);
static bool ssh1_connection_want_user_input(PacketProtocolLayer *ppl);
static void ssh1_connection_got_user_input(PacketProtocolLayer *ppl);
static void ssh1_connection_reconfigure(PacketProtocolLayer *ppl, Conf *conf);

static const struct PacketProtocolLayerVtable ssh1_connection_vtable = {
    ssh1_connection_free,
    ssh1_connection_process_queue,
    ssh1_common_get_specials,
    ssh1_connection_special_cmd,
    ssh1_connection_want_user_input,
    ssh1_connection_got_user_input,
    ssh1_connection_reconfigure,
    NULL /* no layer names in SSH-1 */,
};

static void ssh1_rportfwd_remove(
    ConnectionLayer *cl, struct ssh_rportfwd *rpf);
static SshChannel *ssh1_lportfwd_open(
    ConnectionLayer *cl, const char *hostname, int port,
    const char *description, const SocketPeerInfo *pi, Channel *chan);
static struct X11FakeAuth *ssh1_add_x11_display(
    ConnectionLayer *cl, int authtype, struct X11Display *disp);
static bool ssh1_agent_forwarding_permitted(ConnectionLayer *cl);
static void ssh1_terminal_size(ConnectionLayer *cl, int width, int height);
static void ssh1_stdout_unthrottle(ConnectionLayer *cl, size_t bufsize);
static size_t ssh1_stdin_backlog(ConnectionLayer *cl);
static void ssh1_throttle_all_channels(ConnectionLayer *cl, bool throttled);
static bool ssh1_ldisc_option(ConnectionLayer *cl, int option);
static void ssh1_set_ldisc_option(ConnectionLayer *cl, int option, bool value);
static void ssh1_enable_x_fwd(ConnectionLayer *cl);
static void ssh1_enable_agent_fwd(ConnectionLayer *cl);
static void ssh1_set_wants_user_input(ConnectionLayer *cl, bool wanted);

static const struct ConnectionLayerVtable ssh1_connlayer_vtable = {
    ssh1_rportfwd_alloc,
    ssh1_rportfwd_remove,
    ssh1_lportfwd_open,
    ssh1_session_open,
    ssh1_serverside_x11_open,
    ssh1_serverside_agent_open,
    ssh1_add_x11_display,
    NULL /* add_sharing_x11_display */,
    NULL /* remove_sharing_x11_display */,
    NULL /* send_packet_from_downstream */,
    NULL /* alloc_sharing_channel */,
    NULL /* delete_sharing_channel */,
    NULL /* sharing_queue_global_request */,
    NULL /* sharing_no_more_downstreams */,
    ssh1_agent_forwarding_permitted,
    ssh1_terminal_size,
    ssh1_stdout_unthrottle,
    ssh1_stdin_backlog,
    ssh1_throttle_all_channels,
    ssh1_ldisc_option,
    ssh1_set_ldisc_option,
    ssh1_enable_x_fwd,
    ssh1_enable_agent_fwd,
    ssh1_set_wants_user_input,
};

static size_t ssh1channel_write(
    SshChannel *c, bool is_stderr, const void *buf, size_t len);
static void ssh1channel_write_eof(SshChannel *c);
static void ssh1channel_initiate_close(SshChannel *c, const char *err);
static void ssh1channel_unthrottle(SshChannel *c, size_t bufsize);
static Conf *ssh1channel_get_conf(SshChannel *c);
static void ssh1channel_window_override_removed(SshChannel *c) { /* ignore */ }

static const struct SshChannelVtable ssh1channel_vtable = {
    ssh1channel_write,
    ssh1channel_write_eof,
    ssh1channel_initiate_close,
    ssh1channel_unthrottle,
    ssh1channel_get_conf,
    ssh1channel_window_override_removed,
    NULL /* x11_sharing_handover is only used by SSH-2 connection sharing */,
    NULL /* send_exit_status */,
    NULL /* send_exit_signal */,
    NULL /* send_exit_signal_numeric */,
    NULL /* request_x11_forwarding */,
    NULL /* request_agent_forwarding */,
    NULL /* request_pty */,
    NULL /* send_env_var */,
    NULL /* start_shell */,
    NULL /* start_command */,
    NULL /* start_subsystem */,
    NULL /* send_serial_break */,
    NULL /* send_signal */,
    NULL /* send_terminal_size_change */,
    NULL /* hint_channel_is_simple */,
};

static void ssh1_channel_try_eof(struct ssh1_channel *c);
static void ssh1_channel_close_local(struct ssh1_channel *c,
                                     const char *reason);
static void ssh1_channel_destroy(struct ssh1_channel *c);
static void ssh1_channel_check_close(struct ssh1_channel *c);

static int ssh1_channelcmp(void *av, void *bv)
{
    const struct ssh1_channel *a = (const struct ssh1_channel *) av;
    const struct ssh1_channel *b = (const struct ssh1_channel *) bv;
    if (a->localid < b->localid)
	return -1;
    if (a->localid > b->localid)
	return +1;
    return 0;
}

static int ssh1_channelfind(void *av, void *bv)
{
    const unsigned *a = (const unsigned *) av;
    const struct ssh1_channel *b = (const struct ssh1_channel *) bv;
    if (*a < b->localid)
	return -1;
    if (*a > b->localid)
	return +1;
    return 0;
}

void ssh1_channel_free(struct ssh1_channel *c)
{
    if (c->chan)
        chan_free(c->chan);
    sfree(c);
}

PacketProtocolLayer *ssh1_connection_new(
    Ssh *ssh, Conf *conf, ConnectionLayer **cl_out)
{
    struct ssh1_connection_state *s = snew(struct ssh1_connection_state);
    memset(s, 0, sizeof(*s));
    s->ppl.vt = &ssh1_connection_vtable;

    s->conf = conf_copy(conf);

    s->channels = newtree234(ssh1_channelcmp);

    s->x11authtree = newtree234(x11_authcmp);

    /* Need to get the log context for s->cl now, because we won't be
     * helpfully notified when a copy is written into s->ppl by our
     * owner. */
    s->cl.vt = &ssh1_connlayer_vtable;
    s->cl.logctx = ssh_get_logctx(ssh);

    s->portfwdmgr = portfwdmgr_new(&s->cl);
    s->rportfwds = newtree234(ssh1_rportfwd_cmp);

    *cl_out = &s->cl;
    return &s->ppl;
}

static void ssh1_connection_free(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);
    struct X11FakeAuth *auth;
    struct ssh1_channel *c;
    struct ssh_rportfwd *rpf;

    conf_free(s->conf);

    while ((c = delpos234(s->channels, 0)) != NULL)
        ssh1_channel_free(c);
    freetree234(s->channels);

    if (s->x11disp)
	x11_free_display(s->x11disp);
    while ((auth = delpos234(s->x11authtree, 0)) != NULL)
        x11_free_fake_auth(auth);
    freetree234(s->x11authtree);

    while ((rpf = delpos234(s->rportfwds, 0)) != NULL)
        free_rportfwd(rpf);
    freetree234(s->rportfwds);
    portfwdmgr_free(s->portfwdmgr);

    delete_callbacks_for_context(s);

    sfree(s);
}

void ssh1_connection_set_protoflags(PacketProtocolLayer *ppl,
                                    int local, int remote)
{
    assert(ppl->vt == &ssh1_connection_vtable);
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);
    s->local_protoflags = local;
    s->remote_protoflags = remote;
}

static bool ssh1_connection_filter_queue(struct ssh1_connection_state *s)
{
    PktIn *pktin;
    ptrlen data;
    struct ssh1_channel *c;
    unsigned localid;
    bool expect_halfopen;

    while (1) {
        if (ssh1_common_filter_queue(&s->ppl))
            return true;
        if ((pktin = pq_peek(s->ppl.in_pq)) == NULL)
            return false;

        switch (pktin->type) {
          case SSH1_MSG_CHANNEL_DATA:
          case SSH1_MSG_CHANNEL_OPEN_CONFIRMATION:
          case SSH1_MSG_CHANNEL_OPEN_FAILURE:
          case SSH1_MSG_CHANNEL_CLOSE:
          case SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION:
            /*
             * Common preliminary code for all the messages from the
             * server that cite one of our channel ids: look up that
             * channel id, check it exists, and if it's for a sharing
             * downstream, pass it on.
             */
            localid = get_uint32(pktin);
            c = find234(s->channels, &localid, ssh1_channelfind);

            expect_halfopen = (
                pktin->type == SSH1_MSG_CHANNEL_OPEN_CONFIRMATION ||
                pktin->type == SSH1_MSG_CHANNEL_OPEN_FAILURE);

            if (!c || c->halfopen != expect_halfopen) {
                ssh_remote_error(
                    s->ppl.ssh, "Received %s for %s channel %u",
                    ssh1_pkt_type(pktin->type),
                    !c ? "nonexistent" : c->halfopen ? "half-open" : "open",
                    localid);
                return true;
            }
 
            switch (pktin->type) {
              case SSH1_MSG_CHANNEL_OPEN_CONFIRMATION:
                assert(c->halfopen);
                c->remoteid = get_uint32(pktin);
                c->halfopen = false;
                c->throttling_conn = false;

                chan_open_confirmation(c->chan);

                /*
                 * Now that the channel is fully open, it's possible
                 * in principle to immediately close it. Check whether
                 * it wants us to!
                 *
                 * This can occur if a local socket error occurred
                 * between us sending out CHANNEL_OPEN and receiving
                 * OPEN_CONFIRMATION. If that happens, all we can do
                 * is immediately initiate close proceedings now that
                 * we know the server's id to put in the close
                 * message. We'll have handled that in this code by
                 * having already turned c->chan into a zombie, so its
                 * want_close method (which ssh1_channel_check_close
                 * will consult) will already be returning true.
                 */
                ssh1_channel_check_close(c);

                if (c->pending_eof)
                    ssh1_channel_try_eof(c); /* in case we had a pending EOF */
                break;

              case SSH1_MSG_CHANNEL_OPEN_FAILURE:
                assert(c->halfopen);

                chan_open_failed(c->chan, NULL);
                chan_free(c->chan);

                del234(s->channels, c);
                ssh1_channel_free(c);
                break;

              case SSH1_MSG_CHANNEL_DATA:
                data = get_string(pktin);
                if (!get_err(pktin)) {
                    int bufsize = chan_send(
                        c->chan, false, data.ptr, data.len);

                    if (!c->throttling_conn && bufsize > SSH1_BUFFER_LIMIT) {
                        c->throttling_conn = true;
                        ssh_throttle_conn(s->ppl.ssh, +1);
                    }
                }
                break;

              case SSH1_MSG_CHANNEL_CLOSE:
                if (!(c->closes & CLOSES_RCVD_CLOSE)) {
                    c->closes |= CLOSES_RCVD_CLOSE;
                    chan_send_eof(c->chan);
                    ssh1_channel_check_close(c);
                }
                break;

              case SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION:
                if (!(c->closes & CLOSES_RCVD_CLOSECONF)) {
                    if (!(c->closes & CLOSES_SENT_CLOSE)) {
                        ssh_remote_error(
                            s->ppl.ssh,
                            "Received CHANNEL_CLOSE_CONFIRMATION for channel"
                            " %u for which we never sent CHANNEL_CLOSE\n",
                            c->localid);
                        return true;
                    }

                    c->closes |= CLOSES_RCVD_CLOSECONF;
                    ssh1_channel_check_close(c);
                }
                break;
            }

            pq_pop(s->ppl.in_pq);
            break;

          default:
            if (ssh1_handle_direction_specific_packet(s, pktin)) {
                pq_pop(s->ppl.in_pq);
                if (ssh1_check_termination(s))
                    return true;
            } else {
                return false;
            }
        }
    }
}

static PktIn *ssh1_connection_pop(struct ssh1_connection_state *s)
{
    ssh1_connection_filter_queue(s);
    return pq_pop(s->ppl.in_pq);
}

static void ssh1_connection_process_queue(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);
    PktIn *pktin;

    if (ssh1_connection_filter_queue(s)) /* no matter why we were called */
        return;

    crBegin(s->crState);

    portfwdmgr_config(s->portfwdmgr, s->conf);
    s->portfwdmgr_configured = true;

    while (!s->finished_setup) {
        ssh1_connection_direction_specific_setup(s);
        crReturnV;
    }

    while (1) {

	/*
	 * By this point, most incoming packets are already being
	 * handled by filter_queue, and we need only pay attention to
	 * the unusual ones.
	 */

	if ((pktin = ssh1_connection_pop(s)) != NULL) {
            ssh_proto_error(s->ppl.ssh, "Unexpected packet received, "
                            "type %d (%s)", pktin->type,
                            ssh1_pkt_type(pktin->type));
            return;
	}
	crReturnV;
    }

    crFinishV;
}

static void ssh1_channel_check_close(struct ssh1_channel *c)
{
    struct ssh1_connection_state *s = c->connlayer;
    PktOut *pktout;

    if (c->halfopen) {
        /*
         * If we've sent out our own CHANNEL_OPEN but not yet seen
         * either OPEN_CONFIRMATION or OPEN_FAILURE in response, then
         * it's too early to be sending close messages of any kind.
         */
        return;
    }

    if ((!((CLOSES_SENT_CLOSE | CLOSES_RCVD_CLOSE) & ~c->closes) ||
         chan_want_close(c->chan, (c->closes & CLOSES_SENT_CLOSE),
                         (c->closes & CLOSES_RCVD_CLOSE))) &&
	!(c->closes & CLOSES_SENT_CLOSECONF)) {
        /*
         * We have both sent and received CLOSE (or the channel type
         * doesn't need us to), which means the channel is in final
         * wind-up. Send CLOSE and/or CLOSE_CONFIRMATION, whichever we
         * haven't sent yet.
         */
        if (!(c->closes & CLOSES_SENT_CLOSE)) {
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_CHANNEL_CLOSE);
            put_uint32(pktout, c->remoteid);
            pq_push(s->ppl.out_pq, pktout);
            c->closes |= CLOSES_SENT_CLOSE;
        }
        if (c->closes & CLOSES_RCVD_CLOSE) {
            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION);
            put_uint32(pktout, c->remoteid);
            pq_push(s->ppl.out_pq, pktout);
            c->closes |= CLOSES_SENT_CLOSECONF;
        }
    }

    if (!((CLOSES_SENT_CLOSECONF | CLOSES_RCVD_CLOSECONF) & ~c->closes)) {
        /*
         * We have both sent and received CLOSE_CONFIRMATION, which
         * means we're completely done with the channel.
         */
        ssh1_channel_destroy(c);
    }
}

static void ssh1_channel_try_eof(struct ssh1_channel *c)
{
    struct ssh1_connection_state *s = c->connlayer;
    PktOut *pktout;
    assert(c->pending_eof);          /* precondition for calling us */
    if (c->halfopen)
        return;                 /* can't close: not even opened yet */

    c->pending_eof = false;            /* we're about to send it */

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_CHANNEL_CLOSE);
    put_uint32(pktout, c->remoteid);
    pq_push(s->ppl.out_pq, pktout);
    c->closes |= CLOSES_SENT_CLOSE;

    ssh1_channel_check_close(c);
}

/*
 * Close any local socket and free any local resources associated with
 * a channel.  This converts the channel into a zombie.
 */
static void ssh1_channel_close_local(struct ssh1_channel *c,
                                     const char *reason)
{
    struct ssh1_connection_state *s = c->connlayer;
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    const char *msg = chan_log_close_msg(c->chan);

    if (msg != NULL)
        ppl_logevent("%s%s%s", msg, reason ? " " : "", reason ? reason : "");

    chan_free(c->chan);
    c->chan = zombiechan_new();
}

static void ssh1_check_termination_callback(void *vctx)
{
    struct ssh1_connection_state *s = (struct ssh1_connection_state *)vctx;
    ssh1_check_termination(s);
}

static void ssh1_channel_destroy(struct ssh1_channel *c)
{
    struct ssh1_connection_state *s = c->connlayer;

    ssh1_channel_close_local(c, NULL);
    del234(s->channels, c);
    ssh1_channel_free(c);

    /*
     * If that was the last channel left open, we might need to
     * terminate. But we'll be a bit cautious, by doing that in a
     * toplevel callback, just in case anything on the current call
     * stack objects to this entire PPL being freed.
     */
    queue_toplevel_callback(ssh1_check_termination_callback, s);
}

bool ssh1_check_termination(struct ssh1_connection_state *s)
{
    /*
     * Decide whether we should terminate the SSH connection now.
     * Called after a channel goes away, or when the main session
     * returns SSH1_SMSG_EXIT_STATUS; we terminate when none of either
     * is left.
     */
    if (s->session_terminated && count234(s->channels) == 0) {
        PktOut *pktout = ssh_bpp_new_pktout(
            s->ppl.bpp, SSH1_CMSG_EXIT_CONFIRMATION);
        pq_push(s->ppl.out_pq, pktout);

        ssh_user_close(s->ppl.ssh, "Session finished");
        return true;
    }

    return false;
}

/*
 * Set up most of a new ssh1_channel. Leaves chan untouched (since it
 * will sometimes have been filled in before calling this).
 */
void ssh1_channel_init(struct ssh1_channel *c)
{
    struct ssh1_connection_state *s = c->connlayer;
    c->closes = 0;
    c->pending_eof = false;
    c->throttling_conn = false;
    c->sc.vt = &ssh1channel_vtable;
    c->sc.cl = &s->cl;
    c->localid = alloc_channel_id(s->channels, struct ssh1_channel);
    add234(s->channels, c);
}

static Conf *ssh1channel_get_conf(SshChannel *sc)
{
    struct ssh1_channel *c = container_of(sc, struct ssh1_channel, sc);
    struct ssh1_connection_state *s = c->connlayer;
    return s->conf;
}

static void ssh1channel_write_eof(SshChannel *sc)
{
    struct ssh1_channel *c = container_of(sc, struct ssh1_channel, sc);

    if (c->closes & CLOSES_SENT_CLOSE)
        return;

    c->pending_eof = true;
    ssh1_channel_try_eof(c);
}

static void ssh1channel_initiate_close(SshChannel *sc, const char *err)
{
    struct ssh1_channel *c = container_of(sc, struct ssh1_channel, sc);
    char *reason;

    reason = err ? dupprintf("due to local error: %s", err) : NULL;
    ssh1_channel_close_local(c, reason);
    sfree(reason);
    c->pending_eof = false;   /* this will confuse a zombie channel */

    ssh1_channel_check_close(c);
}

static void ssh1channel_unthrottle(SshChannel *sc, size_t bufsize)
{
    struct ssh1_channel *c = container_of(sc, struct ssh1_channel, sc);
    struct ssh1_connection_state *s = c->connlayer;

    if (c->throttling_conn && bufsize <= SSH1_BUFFER_LIMIT) {
	c->throttling_conn = false;
	ssh_throttle_conn(s->ppl.ssh, -1);
    }
}

static size_t ssh1channel_write(
    SshChannel *sc, bool is_stderr, const void *buf, size_t len)
{
    struct ssh1_channel *c = container_of(sc, struct ssh1_channel, sc);
    struct ssh1_connection_state *s = c->connlayer;

    assert(!(c->closes & CLOSES_SENT_CLOSE));

    PktOut *pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_CHANNEL_DATA);
    put_uint32(pktout, c->remoteid);
    put_string(pktout, buf, len);
    pq_push(s->ppl.out_pq, pktout);

    /*
     * In SSH-1 we can return 0 here - implying that channels are
     * never individually throttled - because the only circumstance
     * that can cause throttling will be the whole SSH connection
     * backing up, in which case _everything_ will be throttled as a
     * whole.
     */
    return 0;
}

static struct X11FakeAuth *ssh1_add_x11_display(
    ConnectionLayer *cl, int authtype, struct X11Display *disp)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    struct X11FakeAuth *auth = x11_invent_fake_auth(s->x11authtree, authtype);
    auth->disp = disp;
    return auth;
}

static SshChannel *ssh1_lportfwd_open(
    ConnectionLayer *cl, const char *hostname, int port,
    const char *description, const SocketPeerInfo *pi, Channel *chan)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh1_channel *c = snew(struct ssh1_channel);
    PktOut *pktout;

    c->connlayer = s;
    ssh1_channel_init(c);
    c->halfopen = true;
    c->chan = chan;

    ppl_logevent("Opening connection to %s:%d for %s",
                 hostname, port, description);

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_PORT_OPEN);
    put_uint32(pktout, c->localid);
    put_stringz(pktout, hostname);
    put_uint32(pktout, port);
    /* originator string would go here, but we didn't specify
     * SSH_PROTOFLAG_HOST_IN_FWD_OPEN */
    pq_push(s->ppl.out_pq, pktout);

    return &c->sc;
}

static void ssh1_rportfwd_remove(ConnectionLayer *cl, struct ssh_rportfwd *rpf)
{
    /*
     * We cannot cancel listening ports on the server side in SSH-1!
     * There's no message to support it.
     */
}

static bool ssh1_agent_forwarding_permitted(ConnectionLayer *cl)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    return conf_get_bool(s->conf, CONF_agentfwd) && agent_exists();
}

static void ssh1_connection_special_cmd(PacketProtocolLayer *ppl,
                                        SessionSpecialCode code, int arg)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);
    PktOut *pktout;

    if (code == SS_PING || code == SS_NOP) {
        if (!(s->ppl.remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_IGNORE);
            put_stringz(pktout, "");
            pq_push(s->ppl.out_pq, pktout);
        }
    } else if (s->mainchan) {
        mainchan_special_cmd(s->mainchan, code, arg);
    }
}

static void ssh1_terminal_size(ConnectionLayer *cl, int width, int height)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    s->term_width = width;
    s->term_height = height;
    if (s->mainchan)
        mainchan_terminal_size(s->mainchan, width, height);
}

static void ssh1_stdout_unthrottle(ConnectionLayer *cl, size_t bufsize)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    if (s->stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
        s->stdout_throttling = false;
        ssh_throttle_conn(s->ppl.ssh, -1);
    }
}

static size_t ssh1_stdin_backlog(ConnectionLayer *cl)
{
    return 0;
}

static void ssh1_throttle_all_channels(ConnectionLayer *cl, bool throttled)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    struct ssh1_channel *c;
    int i;

    for (i = 0; NULL != (c = index234(s->channels, i)); i++)
        chan_set_input_wanted(c->chan, !throttled);
}

static bool ssh1_ldisc_option(ConnectionLayer *cl, int option)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    return s->ldisc_opts[option];
}

static void ssh1_set_ldisc_option(ConnectionLayer *cl, int option, bool value)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    s->ldisc_opts[option] = value;
}

static void ssh1_enable_x_fwd(ConnectionLayer *cl)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    s->X11_fwd_enabled = true;
}

static void ssh1_enable_agent_fwd(ConnectionLayer *cl)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    s->agent_fwd_enabled = true;
}

static void ssh1_set_wants_user_input(ConnectionLayer *cl, bool wanted)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);

    s->want_user_input = wanted;
    s->finished_setup = true;
}

static bool ssh1_connection_want_user_input(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);

    return s->want_user_input;
}

static void ssh1_connection_got_user_input(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);

    while (s->mainchan && bufchain_size(s->ppl.user_input) > 0) {
        /*
         * Add user input to the main channel's buffer.
         */
        ptrlen data = bufchain_prefix(s->ppl.user_input);
        if (data.len > 512)
            data.len = 512;
        sshfwd_write(&s->mainchan_sc, data.ptr, data.len);
        bufchain_consume(s->ppl.user_input, data.len);
    }
}

static void ssh1_connection_reconfigure(PacketProtocolLayer *ppl, Conf *conf)
{
    struct ssh1_connection_state *s =
        container_of(ppl, struct ssh1_connection_state, ppl);

    conf_free(s->conf);
    s->conf = conf_copy(conf);

    if (s->portfwdmgr_configured)
        portfwdmgr_config(s->portfwdmgr, s->conf);
}
