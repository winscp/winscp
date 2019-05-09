/*
 * Client-specific parts of the SSH-1 connection layer.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshppl.h"
#include "sshchan.h"
#include "sshcr.h"
#include "ssh1connection.h"

void ssh1_connection_direction_specific_setup(
    struct ssh1_connection_state *s)
{
    if (!s->mainchan) {
        /*
         * Start up the main session, by telling mainchan.c to do it
         * all just as it would in SSH-2, and translating those
         * concepts to SSH-1's non-channel-shaped idea of the main
         * session.
         */
        s->mainchan = mainchan_new(
            &s->ppl, &s->cl, s->conf, s->term_width, s->term_height,
            false /* is_simple */, NULL);
    }
}

typedef void (*sf_handler_fn_t)(struct ssh1_connection_state *s,
                                bool success, void *ctx);

struct outstanding_succfail {
    sf_handler_fn_t handler;
    void *ctx;
    struct outstanding_succfail *next;

    /*
     * The 'trivial' flag is set if this handler is in response to a
     * request for which the SSH-1 protocol doesn't actually specify a
     * response packet. The client of this system (mainchan.c) will
     * expect to get an acknowledgment regardless, so we arrange to
     * send that ack immediately after the rest of the queue empties.
     */
    bool trivial;
};

static void ssh1_connection_process_trivial_succfails(void *vs);

static void ssh1_queue_succfail_handler(
    struct ssh1_connection_state *s, sf_handler_fn_t handler, void *ctx,
    bool trivial)
{
    struct outstanding_succfail *osf = snew(struct outstanding_succfail);
    osf->handler = handler;
    osf->ctx = ctx;
    osf->trivial = trivial;
    osf->next = NULL;
    if (s->succfail_tail)
        s->succfail_tail->next = osf;
    else
        s->succfail_head = osf;
    s->succfail_tail = osf;

    /* In case this one was trivial and the queue was already empty,
     * we should make sure we run the handler promptly, and the
     * easiest way is to queue it anyway and then run a trivials pass
     * by callback. */
    queue_toplevel_callback(ssh1_connection_process_trivial_succfails, s);
}

static void ssh1_connection_process_succfail(
    struct ssh1_connection_state *s, bool success)
{
    struct outstanding_succfail *prevhead = s->succfail_head;
    s->succfail_head = s->succfail_head->next;
    if (!s->succfail_head)
        s->succfail_tail = NULL;
    prevhead->handler(s, success, prevhead->ctx);
    sfree(prevhead);
}

static void ssh1_connection_process_trivial_succfails(void *vs)
{
    struct ssh1_connection_state *s = (struct ssh1_connection_state *)vs;
    while (s->succfail_head && s->succfail_head->trivial)
        ssh1_connection_process_succfail(s, true);
}

bool ssh1_handle_direction_specific_packet(
    struct ssh1_connection_state *s, PktIn *pktin)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */

    PktOut *pktout;
    struct ssh1_channel *c;
    unsigned remid;
    struct ssh_rportfwd pf, *pfp;
    ptrlen host, data;
    int port;

    switch (pktin->type) {
      case SSH1_SMSG_SUCCESS:
      case SSH1_SMSG_FAILURE:
        if (!s->succfail_head) {
            ssh_remote_error(s->ppl.ssh,
                             "Received %s with no outstanding request",
                             ssh1_pkt_type(pktin->type));
            return true;
        }

        ssh1_connection_process_succfail(
            s, pktin->type == SSH1_SMSG_SUCCESS);
        queue_toplevel_callback(
            ssh1_connection_process_trivial_succfails, s);

        return true;

      case SSH1_SMSG_X11_OPEN:
        remid = get_uint32(pktin);

        /* Refuse if X11 forwarding is disabled. */
        if (!s->X11_fwd_enabled) {
            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
            put_uint32(pktout, remid);
            pq_push(s->ppl.out_pq, pktout);
            ppl_logevent("Rejected X11 connect request");
        } else {
            c = snew(struct ssh1_channel);
            c->connlayer = s;
            ssh1_channel_init(c);
            c->remoteid = remid;
            c->chan = x11_new_channel(s->x11authtree, &c->sc,
                                      NULL, -1, false);
            c->remoteid = remid;
            c->halfopen = false;

            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
            put_uint32(pktout, c->remoteid);
            put_uint32(pktout, c->localid);
            pq_push(s->ppl.out_pq, pktout);
            ppl_logevent("Opened X11 forward channel");
        }

        return true;

      case SSH1_SMSG_AGENT_OPEN:
        remid = get_uint32(pktin);

        /* Refuse if agent forwarding is disabled. */
        if (!s->agent_fwd_enabled) {
            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
            put_uint32(pktout, remid);
            pq_push(s->ppl.out_pq, pktout);
        } else {
            c = snew(struct ssh1_channel);
            c->connlayer = s;
            ssh1_channel_init(c);
            c->remoteid = remid;
            c->chan = agentf_new(&c->sc);
            c->halfopen = false;

            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
            put_uint32(pktout, c->remoteid);
            put_uint32(pktout, c->localid);
            pq_push(s->ppl.out_pq, pktout);
        }

        return true;

      case SSH1_MSG_PORT_OPEN:
        remid = get_uint32(pktin);
        host = get_string(pktin);
        port = toint(get_uint32(pktin));

        pf.dhost = mkstr(host);
        pf.dport = port;
        pfp = find234(s->rportfwds, &pf, NULL);

        if (!pfp) {
            ppl_logevent("Rejected remote port open request for %s:%d",
                         pf.dhost, port);
            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
            put_uint32(pktout, remid);
            pq_push(s->ppl.out_pq, pktout);
        } else {
            char *err;

            c = snew(struct ssh1_channel);
            c->connlayer = s;
            ppl_logevent("Received remote port open request for %s:%d",
                         pf.dhost, port);
            err = portfwdmgr_connect(
                s->portfwdmgr, &c->chan, pf.dhost, port,
                &c->sc, pfp->addressfamily);

            if (err) {
                ppl_logevent("Port open failed: %s", err);
                sfree(err);
                ssh1_channel_free(c);
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
                put_uint32(pktout, remid);
                pq_push(s->ppl.out_pq, pktout);
            } else {
                ssh1_channel_init(c);
                c->remoteid = remid;
                c->halfopen = false;
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
                put_uint32(pktout, c->remoteid);
                put_uint32(pktout, c->localid);
                pq_push(s->ppl.out_pq, pktout);
                ppl_logevent("Forwarded port opened successfully");
            }
        }

        sfree(pf.dhost);

        return true;

      case SSH1_SMSG_STDOUT_DATA:
      case SSH1_SMSG_STDERR_DATA:
        data = get_string(pktin);
        if (!get_err(pktin)) {
            int bufsize = seat_output(
                s->ppl.seat, pktin->type == SSH1_SMSG_STDERR_DATA,
                data.ptr, data.len);
            if (!s->stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
                s->stdout_throttling = true;
                ssh_throttle_conn(s->ppl.ssh, +1);
            }
        }

        return true;

      case SSH1_SMSG_EXIT_STATUS:
        {
            int exitcode = get_uint32(pktin);
            ppl_logevent("Server sent command exit status %d", exitcode);
            ssh_got_exitcode(s->ppl.ssh, exitcode);

            s->session_terminated = true;
        }
        return true;

      default:
        return false;
    }
}

static void ssh1mainchan_succfail_wantreply(struct ssh1_connection_state *s,
                                            bool success, void *ctx)
{
    chan_request_response(s->mainchan_chan, success);
}

static void ssh1mainchan_succfail_nowantreply(struct ssh1_connection_state *s,
                                              bool success, void *ctx)
{
}

static void ssh1mainchan_queue_response(struct ssh1_connection_state *s,
                                        bool want_reply, bool trivial)
{
    sf_handler_fn_t handler = (want_reply ? ssh1mainchan_succfail_wantreply :
                               ssh1mainchan_succfail_nowantreply);
    ssh1_queue_succfail_handler(s, handler, NULL, trivial);
}

static void ssh1mainchan_request_x11_forwarding(
    SshChannel *sc, bool want_reply, const char *authproto,
    const char *authdata, int screen_number, bool oneshot)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_X11_REQUEST_FORWARDING);
    put_stringz(pktout, authproto);
    put_stringz(pktout, authdata);
    if (s->local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER)
        put_uint32(pktout, screen_number);
    pq_push(s->ppl.out_pq, pktout);

    ssh1mainchan_queue_response(s, want_reply, false);
}

static void ssh1mainchan_request_agent_forwarding(
    SshChannel *sc, bool want_reply)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(
        s->ppl.bpp, SSH1_CMSG_AGENT_REQUEST_FORWARDING);
    pq_push(s->ppl.out_pq, pktout);

    ssh1mainchan_queue_response(s, want_reply, false);
}

static void ssh1mainchan_request_pty(
    SshChannel *sc, bool want_reply, Conf *conf, int w, int h)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_REQUEST_PTY);
    put_stringz(pktout, conf_get_str(s->conf, CONF_termtype));
    put_uint32(pktout, h);
    put_uint32(pktout, w);
    put_uint32(pktout, 0); /* width in pixels */
    put_uint32(pktout, 0); /* height in pixels */
    write_ttymodes_to_packet(
        BinarySink_UPCAST(pktout), 1,
        get_ttymodes_from_conf(s->ppl.seat, conf));
    pq_push(s->ppl.out_pq, pktout);

    ssh1mainchan_queue_response(s, want_reply, false);
}

static bool ssh1mainchan_send_env_var(
    SshChannel *sc, bool want_reply, const char *var, const char *value)
{
    return false;              /* SSH-1 doesn't support this at all */
}

static void ssh1mainchan_start_shell(SshChannel *sc, bool want_reply)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EXEC_SHELL);
    pq_push(s->ppl.out_pq, pktout);

    ssh1mainchan_queue_response(s, want_reply, true);
}

static void ssh1mainchan_start_command(
    SshChannel *sc, bool want_reply, const char *command)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EXEC_CMD);
    put_stringz(pktout, command);
    pq_push(s->ppl.out_pq, pktout);

    ssh1mainchan_queue_response(s, want_reply, true);
}

static bool ssh1mainchan_start_subsystem(
    SshChannel *sc, bool want_reply, const char *subsystem)
{
    return false;              /* SSH-1 doesn't support this at all */
}

static bool ssh1mainchan_send_serial_break(
    SshChannel *sc, bool want_reply, int length)
{
    return false;              /* SSH-1 doesn't support this at all */
}

static bool ssh1mainchan_send_signal(
    SshChannel *sc, bool want_reply, const char *signame)
{
    return false;              /* SSH-1 doesn't support this at all */
}

static void ssh1mainchan_send_terminal_size_change(
    SshChannel *sc, int w, int h)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_WINDOW_SIZE);
    put_uint32(pktout, h);
    put_uint32(pktout, w);
    put_uint32(pktout, 0); /* width in pixels */
    put_uint32(pktout, 0); /* height in pixels */
    pq_push(s->ppl.out_pq, pktout);
}

static void ssh1mainchan_hint_channel_is_simple(SshChannel *sc)
{
}

static size_t ssh1mainchan_write(
    SshChannel *sc, bool is_stderr, const void *data, size_t len)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_STDIN_DATA);
    put_string(pktout, data, len);
    pq_push(s->ppl.out_pq, pktout);

    return 0;
}

static void ssh1mainchan_write_eof(SshChannel *sc)
{
    struct ssh1_connection_state *s =
        container_of(sc, struct ssh1_connection_state, mainchan_sc);
    PktOut *pktout;

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EOF);
    pq_push(s->ppl.out_pq, pktout);
}

static const struct SshChannelVtable ssh1mainchan_vtable = {
    ssh1mainchan_write,
    ssh1mainchan_write_eof,
    NULL /* unclean_close */,
    NULL /* unthrottle */,
    NULL /* get_conf */,
    NULL /* window_override_removed is only used by SSH-2 sharing */,
    NULL /* x11_sharing_handover, likewise */,
    NULL /* send_exit_status */,
    NULL /* send_exit_signal */,
    NULL /* send_exit_signal_numeric */,
    ssh1mainchan_request_x11_forwarding,
    ssh1mainchan_request_agent_forwarding,
    ssh1mainchan_request_pty,
    ssh1mainchan_send_env_var,
    ssh1mainchan_start_shell,
    ssh1mainchan_start_command,
    ssh1mainchan_start_subsystem,
    ssh1mainchan_send_serial_break,
    ssh1mainchan_send_signal,
    ssh1mainchan_send_terminal_size_change,
    ssh1mainchan_hint_channel_is_simple,
};

static void ssh1_session_confirm_callback(void *vctx)
{
    struct ssh1_connection_state *s = (struct ssh1_connection_state *)vctx;
    chan_open_confirmation(s->mainchan_chan);
}

SshChannel *ssh1_session_open(ConnectionLayer *cl, Channel *chan)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    s->mainchan_sc.vt = &ssh1mainchan_vtable;
    s->mainchan_sc.cl = &s->cl;
    s->mainchan_chan = chan;
    queue_toplevel_callback(ssh1_session_confirm_callback, s);
    return &s->mainchan_sc;
}

static void ssh1_rportfwd_response(struct ssh1_connection_state *s,
                                   bool success, void *ctx)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh_rportfwd *rpf = (struct ssh_rportfwd *)ctx;

    if (success) {
	ppl_logevent("Remote port forwarding from %s enabled",
                     rpf->log_description);
    } else {
	ppl_logevent("Remote port forwarding from %s refused",
                     rpf->log_description);

	struct ssh_rportfwd *realpf = del234(s->rportfwds, rpf);
	assert(realpf == rpf);
        portfwdmgr_close(s->portfwdmgr, rpf->pfr);
	free_rportfwd(rpf);
    }
}

struct ssh_rportfwd *ssh1_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx)
{
    struct ssh1_connection_state *s =
        container_of(cl, struct ssh1_connection_state, cl);
    struct ssh_rportfwd *rpf = snew(struct ssh_rportfwd);

    rpf->shost = dupstr(shost);
    rpf->sport = sport;
    rpf->dhost = dupstr(dhost);
    rpf->dport = dport;
    rpf->addressfamily = addressfamily;
    rpf->log_description = dupstr(log_description);
    rpf->pfr = pfr;

    if (add234(s->rportfwds, rpf) != rpf) {
        free_rportfwd(rpf);
        return NULL;
    }

    PktOut *pktout = ssh_bpp_new_pktout(
        s->ppl.bpp, SSH1_CMSG_PORT_FORWARD_REQUEST);
    put_uint32(pktout, rpf->sport);
    put_stringz(pktout, rpf->dhost);
    put_uint32(pktout, rpf->dport);
    pq_push(s->ppl.out_pq, pktout);

    ssh1_queue_succfail_handler(s, ssh1_rportfwd_response, rpf, false);

    return rpf;
}

SshChannel *ssh1_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketPeerInfo *pi)
{
    unreachable("Should never be called in the client");
}

SshChannel *ssh1_serverside_agent_open(ConnectionLayer *cl, Channel *chan)
{
    unreachable("Should never be called in the client");
}
