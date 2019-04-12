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

struct ssh1_channel;

struct outstanding_succfail;

struct ssh1_connection_state {
    int crState;

    Ssh ssh;

    Conf *conf;
    int local_protoflags;

    tree234 *channels;		       /* indexed by local id */

    int got_pty;
    int echoedit;
    int ospeed, ispeed;
    int stdout_throttling;
    int session_ready;
    int session_eof_pending, session_eof_sent, session_terminated;
    int term_width, term_height, term_width_orig, term_height_orig;

    int X11_fwd_enabled;
    struct X11Display *x11disp;
    struct X11FakeAuth *x11auth;
    tree234 *x11authtree;

    int agent_fwd_enabled;

    tree234 *rportfwds;
    PortFwdManager *portfwdmgr;
    int portfwdmgr_configured;

    int finished_setup;

    /*
     * These store the list of requests that we're waiting for
     * SSH_SMSG_{SUCCESS,FAILURE} replies to. (Those messages don't
     * come with any indication of what they're in response to, so we
     * have to keep track of the queue ourselves.)
     */
    struct outstanding_succfail *succfail_head, *succfail_tail;

    ConnectionLayer cl;
    PacketProtocolLayer ppl;
};

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
static int ssh1_connection_want_user_input(PacketProtocolLayer *ppl);
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

static struct ssh_rportfwd *ssh1_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx);
static void ssh1_rportfwd_remove(
    ConnectionLayer *cl, struct ssh_rportfwd *rpf);
static SshChannel *ssh1_lportfwd_open(
    ConnectionLayer *cl, const char *hostname, int port,
    const char *org, Channel *chan);
static int ssh1_agent_forwarding_permitted(ConnectionLayer *cl);
static void ssh1_terminal_size(ConnectionLayer *cl, int width, int height);
static void ssh1_stdout_unthrottle(ConnectionLayer *cl, int bufsize);
static int ssh1_stdin_backlog(ConnectionLayer *cl);
static void ssh1_throttle_all_channels(ConnectionLayer *cl, int throttled);
static int ssh1_ldisc_option(ConnectionLayer *cl, int option);

static const struct ConnectionLayerVtable ssh1_connlayer_vtable = {
    ssh1_rportfwd_alloc,
    ssh1_rportfwd_remove,
    ssh1_lportfwd_open,
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
};

struct ssh1_channel {
    struct ssh1_connection_state *connlayer;

    unsigned remoteid, localid;
    int type;
    /* True if we opened this channel but server hasn't confirmed. */
    int halfopen;

    /* Bitmap of whether we've sent/received CHANNEL_CLOSE and
     * CHANNEL_CLOSE_CONFIRMATION. */
#define CLOSES_SENT_CLOSE      1
#define CLOSES_SENT_CLOSECONF  2
#define CLOSES_RCVD_CLOSE      4
#define CLOSES_RCVD_CLOSECONF  8
    int closes;

    /*
     * This flag indicates that an EOF is pending on the outgoing side
     * of the channel: that is, wherever we're getting the data for
     * this channel has sent us some data followed by EOF. We can't
     * actually send the EOF until we've finished sending the data, so
     * we set this flag instead to remind us to do so once our buffer
     * is clear.
     */
    int pending_eof;

    /*
     * True if this channel is causing the underlying connection to be
     * throttled.
     */
    int throttling_conn;

    /*
     * True if we currently have backed-up data on the direction of
     * this channel pointing out of the SSH connection, and therefore
     * would prefer the 'Channel' implementation not to read further
     * local input if possible.
     */
    int throttled_by_backlog;

    Channel *chan;      /* handle the client side of this channel, if not */
    SshChannel sc;      /* entry point for chan to talk back to */
};

static int ssh1channel_write(SshChannel *c, const void *buf, int len);
static void ssh1channel_write_eof(SshChannel *c);
static void ssh1channel_unclean_close(SshChannel *c, const char *err);
static void ssh1channel_unthrottle(SshChannel *c, int bufsize);
static Conf *ssh1channel_get_conf(SshChannel *c);

static const struct SshChannelVtable ssh1channel_vtable = {
    ssh1channel_write,
    ssh1channel_write_eof,
    ssh1channel_unclean_close,
    ssh1channel_unthrottle,
    ssh1channel_get_conf,
    NULL /* window_override_removed is only used by SSH-2 sharing */,
    NULL /* x11_sharing_handover, likewise */,
};

static void ssh1_channel_init(struct ssh1_channel *c);
static void ssh1_channel_try_eof(struct ssh1_channel *c);
static void ssh1_channel_close_local(struct ssh1_channel *c,
                                     const char *reason);
static void ssh1_channel_destroy(struct ssh1_channel *c);
static void ssh1_channel_check_close(struct ssh1_channel *c);

static int ssh1_check_termination(struct ssh1_connection_state *s);

typedef void (*sf_handler_fn_t)(struct ssh1_connection_state *s,
                                PktIn *pktin, void *ctx);
struct outstanding_succfail {
    sf_handler_fn_t handler;
    void *ctx;
    struct outstanding_succfail *next;
};
static void ssh1_queue_succfail_handler(
    struct ssh1_connection_state *s, sf_handler_fn_t handler, void *ctx)
{
    struct outstanding_succfail *osf =
        snew(struct outstanding_succfail);
    osf->handler = handler;
    osf->ctx = ctx;
    if (s->succfail_tail)
        s->succfail_tail->next = osf;
    else
        s->succfail_head = osf;
    s->succfail_tail = osf;
}

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

static void ssh1_channel_free(struct ssh1_channel *c)
{
    if (c->chan)
        chan_free(c->chan);
    sfree(c);
}

PacketProtocolLayer *ssh1_connection_new(
    Ssh ssh, Conf *conf, ConnectionLayer **cl_out)
{
    struct ssh1_connection_state *s = snew(struct ssh1_connection_state);
    memset(s, 0, sizeof(*s));
    s->ppl.vt = &ssh1_connection_vtable;

    s->conf = conf_copy(conf);

    s->channels = newtree234(ssh1_channelcmp);

    s->x11authtree = newtree234(x11_authcmp);

    /* Need to get the frontend for s->cl now, because we won't be
     * helpfully notified when a copy is written into s->ppl by our
     * owner. */
    s->cl.vt = &ssh1_connlayer_vtable;
    s->cl.frontend = ssh_get_frontend(ssh);

    s->portfwdmgr = portfwdmgr_new(&s->cl);
    s->rportfwds = newtree234(ssh1_rportfwd_cmp);

    *cl_out = &s->cl;
    return &s->ppl;
}

static void ssh1_connection_free(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
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

    sfree(s);
}

void ssh1_connection_set_local_protoflags(PacketProtocolLayer *ppl, int flags)
{
    assert(ppl->vt == &ssh1_connection_vtable);
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
    s->local_protoflags = flags;
}

static int ssh1_connection_filter_queue(struct ssh1_connection_state *s)
{
    PktIn *pktin;
    PktOut *pktout;
    ptrlen data, host;
    struct ssh1_channel *c;
    unsigned localid, remid;
    int port, expect_halfopen;
    struct ssh_rportfwd pf, *pfp;
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */

    /* Cross-reference to ssh1login.c to handle the common packets
     * between login and connection: DISCONNECT, DEBUG and IGNORE. */
    extern int ssh1_common_filter_queue(PacketProtocolLayer *ppl);

    while (1) {
        if (ssh1_common_filter_queue(&s->ppl))
            return TRUE;
        if ((pktin = pq_peek(s->ppl.in_pq)) == NULL)
            return FALSE;

        switch (pktin->type) {
          case SSH1_SMSG_SUCCESS:
          case SSH1_SMSG_FAILURE:
            if (!s->finished_setup) {
                /* During initial setup, these messages are not
                 * filtered out, but go back to the main coroutine. */
                return FALSE;
            }

            if (!s->succfail_head) {
                ssh_remote_error(s->ppl.ssh,
                                 "Received %s with no outstanding request",
                                 ssh1_pkt_type(pktin->type));
                return TRUE;
            }

            s->succfail_head->handler(s, pktin, s->succfail_head->ctx);
            {
                struct outstanding_succfail *tmp = s->succfail_head;
                s->succfail_head = s->succfail_head->next;
                sfree(tmp);
            }

            pq_pop(s->ppl.in_pq);
            break;

          case SSH1_SMSG_X11_OPEN:
            remid = get_uint32(pktin);

            /* Refuse if X11 forwarding is disabled. */
            if (!s->X11_fwd_enabled) {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
                put_uint32(pktout, remid);
                pq_push(s->ppl.out_pq, pktout);
                ppl_logevent(("Rejected X11 connect request"));
            } else {
                c = snew(struct ssh1_channel);
                c->connlayer = s;
                ssh1_channel_init(c);
                c->remoteid = remid;
                c->chan = x11_new_channel(s->x11authtree, &c->sc,
                                          NULL, -1, FALSE);
                c->remoteid = remid;
                c->halfopen = FALSE;

                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
                put_uint32(pktout, c->remoteid);
                put_uint32(pktout, c->localid);
                pq_push(s->ppl.out_pq, pktout);
                ppl_logevent(("Opened X11 forward channel"));
            }

            pq_pop(s->ppl.in_pq);
            break;

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
                c->halfopen = FALSE;

                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
                put_uint32(pktout, c->remoteid);
                put_uint32(pktout, c->localid);
                pq_push(s->ppl.out_pq, pktout);
            }

            pq_pop(s->ppl.in_pq);
            break;

          case SSH1_MSG_PORT_OPEN:
            remid = get_uint32(pktin);
            host = get_string(pktin);
            port = toint(get_uint32(pktin));

            pf.dhost = mkstr(host);
            pf.dport = port;
            pfp = find234(s->rportfwds, &pf, NULL);

            if (!pfp) {
                ppl_logevent(("Rejected remote port open request for %s:%d",
                              pf.dhost, port));
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
                put_uint32(pktout, remid);
                pq_push(s->ppl.out_pq, pktout);
            } else {
                char *err;

                c = snew(struct ssh1_channel);
                c->connlayer = s;
                ppl_logevent(("Received remote port open request for %s:%d",
                              pf.dhost, port));
                err = portfwdmgr_connect(
                    s->portfwdmgr, &c->chan, pf.dhost, port,
                    &c->sc, pfp->addressfamily);

                if (err) {
                    ppl_logevent(("Port open failed: %s", err));
                    sfree(err);
                    ssh1_channel_free(c);
                    pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_FAILURE);
                    put_uint32(pktout, remid);
                    pq_push(s->ppl.out_pq, pktout);
                } else {
                    ssh1_channel_init(c);
                    c->remoteid = remid;
                    c->halfopen = FALSE;
                    pktout = ssh_bpp_new_pktout(
                        s->ppl.bpp, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION);
                    put_uint32(pktout, c->remoteid);
                    put_uint32(pktout, c->localid);
                    pq_push(s->ppl.out_pq, pktout);
                    ppl_logevent(("Forwarded port opened successfully"));
                }
            }

            sfree(pf.dhost);

            pq_pop(s->ppl.in_pq);
            break;

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
                return TRUE;
            }
 
            switch (pktin->type) {
              case SSH1_MSG_CHANNEL_OPEN_CONFIRMATION:
                assert(c->halfopen);
                c->remoteid = get_uint32(pktin);
                c->halfopen = FALSE;
                c->throttling_conn = FALSE;

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
                 * will consult) will already be returning TRUE.
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
                        c->chan, FALSE, data.ptr, data.len);

                    if (!c->throttling_conn && bufsize > SSH1_BUFFER_LIMIT) {
                        c->throttling_conn = TRUE;
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
                        return TRUE;
                    }

                    c->closes |= CLOSES_RCVD_CLOSECONF;
                    ssh1_channel_check_close(c);
                }
                break;
            }

            pq_pop(s->ppl.in_pq);
            break;

          case SSH1_SMSG_STDOUT_DATA:
          case SSH1_SMSG_STDERR_DATA:
            data = get_string(pktin);
            if (!get_err(pktin)) {
                int bufsize = from_backend(
                    s->ppl.frontend, pktin->type == SSH1_SMSG_STDERR_DATA,
                    data.ptr, data.len);
                if (!s->stdout_throttling && bufsize > SSH1_BUFFER_LIMIT) {
                    s->stdout_throttling = 1;
                    ssh_throttle_conn(s->ppl.ssh, +1);
                }
            }

            pq_pop(s->ppl.in_pq);
            break;

          case SSH1_SMSG_EXIT_STATUS:
            {
                int exitcode = get_uint32(pktin);
                ppl_logevent(("Server sent command exit status %d", exitcode));
                ssh_got_exitcode(s->ppl.ssh, exitcode);

                s->session_terminated = TRUE;
                if (ssh1_check_termination(s))
                    return TRUE;
            }
            pq_pop(s->ppl.in_pq);
            break;

          default:
            return FALSE;
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
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
    PktIn *pktin;
    PktOut *pktout;

    if (ssh1_connection_filter_queue(s)) /* no matter why we were called */
        return;

    crBegin(s->crState);

    if (ssh_agent_forwarding_permitted(&s->cl)) {
	ppl_logevent(("Requesting agent forwarding"));
        pktout = ssh_bpp_new_pktout(s->ppl.bpp,
                                    SSH1_CMSG_AGENT_REQUEST_FORWARDING);
        pq_push(s->ppl.out_pq, pktout);
        crMaybeWaitUntilV((pktin = ssh1_connection_pop(s)) != NULL);
	if (pktin->type == SSH1_SMSG_SUCCESS) {
	    ppl_logevent(("Agent forwarding enabled"));
	    s->agent_fwd_enabled = TRUE;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    ppl_logevent(("Agent forwarding refused"));
	} else {
            ssh_proto_error(s->ppl.ssh, "Unexpected packet received"
                            " in response to agent forwarding request, "
                            "type %d (%s)", pktin->type,
                            ssh1_pkt_type(pktin->type));
            return;
	}
    }

    if (conf_get_int(s->conf, CONF_x11_forward)) {
        s->x11disp =
            x11_setup_display(conf_get_str(s->conf, CONF_x11_display),
                              s->conf);
        if (!s->x11disp) {
            /* FIXME: return an error message from x11_setup_display */
            ppl_logevent(("X11 forwarding not enabled: unable to"
                          " initialise X display"));
        } else {
            s->x11auth = x11_invent_fake_auth
                (s->x11authtree, conf_get_int(s->conf, CONF_x11_auth));
            s->x11auth->disp = s->x11disp;

            ppl_logevent(("Requesting X11 forwarding"));
            pktout = ssh_bpp_new_pktout(
                s->ppl.bpp, SSH1_CMSG_X11_REQUEST_FORWARDING);
            put_stringz(pktout, s->x11auth->protoname);
            put_stringz(pktout, s->x11auth->datastring);
            if (s->local_protoflags & SSH1_PROTOFLAG_SCREEN_NUMBER)
                put_uint32(pktout, s->x11disp->screennum);
            pq_push(s->ppl.out_pq, pktout);
            crMaybeWaitUntilV((pktin = ssh1_connection_pop(s)) != NULL);
            if (pktin->type == SSH1_SMSG_SUCCESS) {
                ppl_logevent(("X11 forwarding enabled"));
                s->X11_fwd_enabled = TRUE;
            } else if (pktin->type == SSH1_SMSG_FAILURE) {
                ppl_logevent(("X11 forwarding refused"));
            } else {
                ssh_proto_error(s->ppl.ssh, "Unexpected packet received"
                                " in response to X11 forwarding request, "
                                "type %d (%s)", pktin->type,
                                ssh1_pkt_type(pktin->type));
                return;
            }
        }
    }

    portfwdmgr_config(s->portfwdmgr, s->conf);
    s->portfwdmgr_configured = TRUE;

    if (!conf_get_int(s->conf, CONF_nopty)) {
	/* Unpick the terminal-speed string. */
	/* XXX perhaps we should allow no speeds to be sent. */
	s->ospeed = 38400; s->ispeed = 38400; /* last-resort defaults */
	sscanf(conf_get_str(s->conf, CONF_termspeed), "%d,%d",
               &s->ospeed, &s->ispeed);
	/* Send the pty request. */
	pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_REQUEST_PTY);
	put_stringz(pktout, conf_get_str(s->conf, CONF_termtype));
	put_uint32(pktout, s->term_height);
	put_uint32(pktout, s->term_width);
        s->term_width_orig = s->term_width;
        s->term_height_orig = s->term_height;
	put_uint32(pktout, 0); /* width in pixels */
	put_uint32(pktout, 0); /* height in pixels */
        write_ttymodes_to_packet_from_conf(
            BinarySink_UPCAST(pktout), s->ppl.frontend, s->conf,
            1, s->ospeed, s->ispeed);
        pq_push(s->ppl.out_pq, pktout);
        crMaybeWaitUntilV((pktin = ssh1_connection_pop(s)) != NULL);
	if (pktin->type == SSH1_SMSG_SUCCESS) {
            ppl_logevent(("Allocated pty (ospeed %dbps, ispeed %dbps)",
                          s->ospeed, s->ispeed));
            s->got_pty = TRUE;
	} else if (pktin->type == SSH1_SMSG_FAILURE) {
	    ppl_printf(("Server refused to allocate pty\r\n"));
	    s->echoedit = TRUE;
	} else {
            ssh_proto_error(s->ppl.ssh, "Unexpected packet received"
                            " in response to pty request, "
                            "type %d (%s)", pktin->type,
                            ssh1_pkt_type(pktin->type));
            crStopV;
        }
    } else {
	s->echoedit = TRUE;
    }

    /*
     * Start the shell or command.
     * 
     * Special case: if the first-choice command is an SSH-2
     * subsystem (hence not usable here) and the second choice
     * exists, we fall straight back to that.
     */
    {
	char *cmd = conf_get_str(s->conf, CONF_remote_cmd);
	
	if (conf_get_int(s->conf, CONF_ssh_subsys) &&
	    conf_get_str(s->conf, CONF_remote_cmd2)) {
	    cmd = conf_get_str(s->conf, CONF_remote_cmd2);
	    ssh_got_fallback_cmd(s->ppl.ssh);
	}
	if (*cmd) {
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EXEC_CMD);
            put_stringz(pktout, cmd);
        } else {
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EXEC_SHELL);
        }
        pq_push(s->ppl.out_pq, pktout);
	ppl_logevent(("Started session"));
    }

    s->session_ready = TRUE;
    ssh_ppl_got_user_input(&s->ppl); /* in case any input is already queued */

    /* If an EOF or a window-size change arrived before we were ready
     * to handle either one, handle them now. */
    if (s->session_eof_pending) {
        ssh_ppl_special_cmd(&s->ppl, SS_EOF, 0);
        s->session_eof_pending = FALSE;
    }
    if (s->term_width_orig != s->term_width ||
        s->term_height_orig != s->term_height)
	ssh_terminal_size(&s->cl, s->term_width, s->term_height);

    ssh_ldisc_update(s->ppl.ssh);
    s->finished_setup = TRUE;

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
        while (bufchain_size(s->ppl.user_input) > 0) {
            void *data;
            int len;
            bufchain_prefix(s->ppl.user_input, &data, &len);
            if (len > 512)
                len = 512;
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_STDIN_DATA);
            put_string(pktout, data, len);
            pq_push(s->ppl.out_pq, pktout);
            bufchain_consume(s->ppl.user_input, len);
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

    c->pending_eof = FALSE;            /* we're about to send it */

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
        ppl_logevent(("%s%s%s", msg, reason ? " " : "", reason ? reason : ""));

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

static int ssh1_check_termination(struct ssh1_connection_state *s)
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
        return TRUE;
    }

    return FALSE;
}

/*
 * Set up most of a new ssh1_channel. Leaves chan untouched (since it
 * will sometimes have been filled in before calling this).
 */
static void ssh1_channel_init(struct ssh1_channel *c)
{
    struct ssh1_connection_state *s = c->connlayer;
    c->closes = 0;
    c->pending_eof = FALSE;
    c->throttling_conn = FALSE;
    c->sc.vt = &ssh1channel_vtable;
    c->localid = alloc_channel_id(s->channels, struct ssh1_channel);
    add234(s->channels, c);
}

static Conf *ssh1channel_get_conf(SshChannel *sc)
{
    struct ssh1_channel *c = FROMFIELD(sc, struct ssh1_channel, sc);
    struct ssh1_connection_state *s = c->connlayer;
    return s->conf;
}

static void ssh1channel_write_eof(SshChannel *sc)
{
    struct ssh1_channel *c = FROMFIELD(sc, struct ssh1_channel, sc);

    if (c->closes & CLOSES_SENT_CLOSE)
        return;

    c->pending_eof = TRUE;
    ssh1_channel_try_eof(c);
}

static void ssh1channel_unclean_close(SshChannel *sc, const char *err)
{
    struct ssh1_channel *c = FROMFIELD(sc, struct ssh1_channel, sc);
    char *reason;

    reason = dupprintf("due to local error: %s", err);
    ssh1_channel_close_local(c, reason);
    sfree(reason);
    c->pending_eof = FALSE;   /* this will confuse a zombie channel */

    ssh1_channel_check_close(c);
}

static void ssh1channel_unthrottle(SshChannel *sc, int bufsize)
{
    struct ssh1_channel *c = FROMFIELD(sc, struct ssh1_channel, sc);
    struct ssh1_connection_state *s = c->connlayer;

    if (c->throttling_conn && bufsize <= SSH1_BUFFER_LIMIT) {
	c->throttling_conn = 0;
	ssh_throttle_conn(s->ppl.ssh, -1);
    }
}

static int ssh1channel_write(SshChannel *sc, const void *buf, int len)
{
    struct ssh1_channel *c = FROMFIELD(sc, struct ssh1_channel, sc);
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

static SshChannel *ssh1_lportfwd_open(
    ConnectionLayer *cl, const char *hostname, int port,
    const char *org, Channel *chan)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh1_channel *c = snew(struct ssh1_channel);
    PktOut *pktout;

    c->connlayer = s;
    ssh1_channel_init(c);
    c->halfopen = TRUE;
    c->chan = chan;

    ppl_logevent(("Opening connection to %s:%d for %s", hostname, port, org));

    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_PORT_OPEN);
    put_uint32(pktout, c->localid);
    put_stringz(pktout, hostname);
    put_uint32(pktout, port);
    /* originator string would go here, but we didn't specify
     * SSH_PROTOFLAG_HOST_IN_FWD_OPEN */
    pq_push(s->ppl.out_pq, pktout);

    return &c->sc;
}

static void ssh1_rportfwd_response(struct ssh1_connection_state *s,
                                   PktIn *pktin, void *ctx)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh_rportfwd *rpf = (struct ssh_rportfwd *)ctx;

    if (pktin->type == SSH1_SMSG_SUCCESS) {
	ppl_logevent(("Remote port forwarding from %s enabled",
                      rpf->log_description));
    } else {
	ppl_logevent(("Remote port forwarding from %s refused",
                      rpf->log_description));

	struct ssh_rportfwd *realpf = del234(s->rportfwds, rpf);
	assert(realpf == rpf);
        portfwdmgr_close(s->portfwdmgr, rpf->pfr);
	free_rportfwd(rpf);
    }
}

static struct ssh_rportfwd *ssh1_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);
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

    ssh1_queue_succfail_handler(s, ssh1_rportfwd_response, rpf);

    return rpf;
}

static void ssh1_rportfwd_remove(ConnectionLayer *cl, struct ssh_rportfwd *rpf)
{
    /*
     * We cannot cancel listening ports on the server side in SSH-1!
     * There's no message to support it.
     */
}

static int ssh1_agent_forwarding_permitted(ConnectionLayer *cl)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);
    return conf_get_int(s->conf, CONF_agentfwd) && agent_exists();
}

static void ssh1_connection_special_cmd(PacketProtocolLayer *ppl,
                                        SessionSpecialCode code, int arg)
{
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
    PktOut *pktout;

    if (code == SS_PING || code == SS_NOP) {
        if (!(s->ppl.remote_bugs & BUG_CHOKES_ON_SSH1_IGNORE)) {
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_MSG_IGNORE);
            put_stringz(pktout, "");
            pq_push(s->ppl.out_pq, pktout);
        }
    } else if (code == SS_EOF) {
	if (!s->session_ready) {
	    /*
	     * Buffer the EOF to send as soon as the main session is
	     * fully set up.
	     */
            s->session_eof_pending = TRUE;
	} else if (!s->session_eof_sent) {
	    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_EOF);
            pq_push(s->ppl.out_pq, pktout);
            ppl_logevent(("Sent EOF message"));
            s->session_eof_sent = TRUE;
	}
    }
}

static void ssh1_terminal_size(ConnectionLayer *cl, int width, int height)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);

    s->term_width = width;
    s->term_height = height;

    if (s->session_ready) {
        PktOut *pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH1_CMSG_WINDOW_SIZE);
        put_uint32(pktout, s->term_height);
        put_uint32(pktout, s->term_width);
        put_uint32(pktout, 0);
        put_uint32(pktout, 0);
	pq_push(s->ppl.out_pq, pktout);
    }
}

static void ssh1_stdout_unthrottle(ConnectionLayer *cl, int bufsize)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);

    if (s->stdout_throttling && bufsize < SSH1_BUFFER_LIMIT) {
        s->stdout_throttling = 0;
        ssh_throttle_conn(s->ppl.ssh, -1);
    }
}

static int ssh1_stdin_backlog(ConnectionLayer *cl)
{
    return 0;
}

static void ssh1_throttle_all_channels(ConnectionLayer *cl, int throttled)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);
    struct ssh1_channel *c;
    int i;

    for (i = 0; NULL != (c = index234(s->channels, i)); i++)
        chan_set_input_wanted(c->chan, !throttled);
}

static int ssh1_ldisc_option(ConnectionLayer *cl, int option)
{
    struct ssh1_connection_state *s =
        FROMFIELD(cl, struct ssh1_connection_state, cl);

    /* We always return the same value for LD_ECHO and LD_EDIT */
    return s->echoedit;
}

static int ssh1_connection_want_user_input(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
    return s->session_ready && !s->session_eof_sent;
}

static void ssh1_connection_got_user_input(PacketProtocolLayer *ppl)
{
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);
    if (s->session_ready && !s->session_eof_sent)
        queue_idempotent_callback(&s->ppl.ic_process_queue);
}

static void ssh1_connection_reconfigure(PacketProtocolLayer *ppl, Conf *conf)
{
    struct ssh1_connection_state *s =
        FROMFIELD(ppl, struct ssh1_connection_state, ppl);

    conf_free(s->conf);
    s->conf = conf_copy(conf);

    if (s->portfwdmgr_configured)
        portfwdmgr_config(s->portfwdmgr, s->conf);
}
