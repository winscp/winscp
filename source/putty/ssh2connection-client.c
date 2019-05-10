/*
 * Client-specific parts of the SSH-2 connection layer.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshppl.h"
#include "sshchan.h"
#include "sshcr.h"
#include "ssh2connection.h"

static ChanopenResult chan_open_x11(
    struct ssh2_connection_state *s, SshChannel *sc,
    ptrlen peeraddr, int peerport)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    char *peeraddr_str;
    Channel *ch;

    ppl_logevent("Received X11 connect request from %.*s:%d",
                 PTRLEN_PRINTF(peeraddr), peerport);

    if (!s->X11_fwd_enabled && !s->connshare) {
        CHANOPEN_RETURN_FAILURE(
            SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED,
            ("X11 forwarding is not enabled"));
    }

    peeraddr_str = peeraddr.ptr ? mkstr(peeraddr) : NULL;
    ch = x11_new_channel(
        s->x11authtree, sc, peeraddr_str, peerport, s->connshare != NULL);
    sfree(peeraddr_str);
    ppl_logevent("Opened X11 forward channel");
    CHANOPEN_RETURN_SUCCESS(ch);
}

static ChanopenResult chan_open_forwarded_tcpip(
    struct ssh2_connection_state *s, SshChannel *sc,
    ptrlen fwdaddr, int fwdport, ptrlen peeraddr, int peerport)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh_rportfwd pf, *realpf;
    Channel *ch;
    char *err;

    ppl_logevent("Received remote port %.*s:%d open request from %.*s:%d",
                 PTRLEN_PRINTF(fwdaddr), fwdport,
                 PTRLEN_PRINTF(peeraddr), peerport);

    pf.shost = mkstr(fwdaddr);
    pf.sport = fwdport;
    realpf = find234(s->rportfwds, &pf, NULL);
    sfree(pf.shost);

    if (realpf == NULL) {
        CHANOPEN_RETURN_FAILURE(
            SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED,
            ("Remote port is not recognised"));
    }

    if (realpf->share_ctx) {
        /*
         * This port forwarding is on behalf of a connection-sharing
         * downstream.
         */
        CHANOPEN_RETURN_DOWNSTREAM(realpf->share_ctx);
    }

    err = portfwdmgr_connect(
        s->portfwdmgr, &ch, realpf->dhost, realpf->dport,
        sc, realpf->addressfamily);
    ppl_logevent("Attempting to forward remote port to %s:%d",
                 realpf->dhost, realpf->dport);
    if (err != NULL) {
        ppl_logevent("Port open failed: %s", err);
        sfree(err);
        CHANOPEN_RETURN_FAILURE(
            SSH2_OPEN_CONNECT_FAILED,
            ("Port open failed"));
    }

    ppl_logevent("Forwarded port opened successfully");
    CHANOPEN_RETURN_SUCCESS(ch);
}

static ChanopenResult chan_open_auth_agent(
    struct ssh2_connection_state *s, SshChannel *sc)
{
    if (!s->agent_fwd_enabled) {
        CHANOPEN_RETURN_FAILURE(
            SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED,
            ("Agent forwarding is not enabled"));
    }

    CHANOPEN_RETURN_SUCCESS(agentf_new(sc));
}

ChanopenResult ssh2_connection_parse_channel_open(
    struct ssh2_connection_state *s, ptrlen type,
    PktIn *pktin, SshChannel *sc)
{
    if (ptrlen_eq_string(type, "x11")) {
        ptrlen peeraddr = get_string(pktin);
        int peerport = get_uint32(pktin);

        return chan_open_x11(s, sc, peeraddr, peerport);
    } else if (ptrlen_eq_string(type, "forwarded-tcpip")) {
        ptrlen fwdaddr = get_string(pktin);
        int fwdport = toint(get_uint32(pktin));
        ptrlen peeraddr = get_string(pktin);
        int peerport = toint(get_uint32(pktin));

        return chan_open_forwarded_tcpip(
            s, sc, fwdaddr, fwdport, peeraddr, peerport);
    } else if (ptrlen_eq_string(type, "auth-agent@openssh.com")) {
        return chan_open_auth_agent(s, sc);
    } else {
        CHANOPEN_RETURN_FAILURE(
            SSH2_OPEN_UNKNOWN_CHANNEL_TYPE,
            ("Unsupported channel type requested"));
    }
}

bool ssh2_connection_parse_global_request(
    struct ssh2_connection_state *s, ptrlen type, PktIn *pktin)
{
    /*
     * We don't know of any global requests that an SSH client needs
     * to honour.
     */
    return false;
}

PktOut *ssh2_portfwd_chanopen(
    struct ssh2_connection_state *s, struct ssh2_channel *c,
    const char *hostname, int port,
    const char *description, const SocketPeerInfo *peerinfo)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    PktOut *pktout;

    /*
     * In client mode, this function is called by portfwdmgr in
     * response to PortListeners that were set up in
     * portfwdmgr_config, which means that the hostname and port
     * parameters will indicate the host we want to tell the server to
     * connect _to_.
     */

    ppl_logevent("Opening connection to %s:%d for %s",
                 hostname, port, description);

    pktout = ssh2_chanopen_init(c, "direct-tcpip");
    {
        char *trimmed_host = host_strduptrim(hostname);
        put_stringz(pktout, trimmed_host);
        sfree(trimmed_host);
    }
    put_uint32(pktout, port);

    /*
     * We make up values for the originator data; partly it's too much
     * hassle to keep track, and partly I'm not convinced the server
     * should be told details like that about my local network
     * configuration. The "originator IP address" is syntactically a
     * numeric IP address, and some servers (e.g., Tectia) get upset
     * if it doesn't match this syntax.
     */
    put_stringz(pktout, "0.0.0.0");
    put_uint32(pktout, 0);

    return pktout;
}

static int ssh2_rportfwd_cmp(void *av, void *bv)
{
    struct ssh_rportfwd *a = (struct ssh_rportfwd *) av;
    struct ssh_rportfwd *b = (struct ssh_rportfwd *) bv;
    int i;
    if ( (i = strcmp(a->shost, b->shost)) != 0)
	return i < 0 ? -1 : +1;
    if (a->sport > b->sport)
	return +1;
    if (a->sport < b->sport)
	return -1;
    return 0;
}

static void ssh2_rportfwd_globreq_response(struct ssh2_connection_state *s,
                                           PktIn *pktin, void *ctx)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh_rportfwd *rpf = (struct ssh_rportfwd *)ctx;

    if (pktin->type == SSH2_MSG_REQUEST_SUCCESS) {
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

struct ssh_rportfwd *ssh2_rportfwd_alloc(
    ConnectionLayer *cl,
    const char *shost, int sport, const char *dhost, int dport,
    int addressfamily, const char *log_description, PortFwdRecord *pfr,
    ssh_sharing_connstate *share_ctx)
{
    struct ssh2_connection_state *s =
        container_of(cl, struct ssh2_connection_state, cl);
    struct ssh_rportfwd *rpf = snew(struct ssh_rportfwd);

    if (!s->rportfwds)
        s->rportfwds = newtree234(ssh2_rportfwd_cmp);

    rpf->shost = dupstr(shost);
    rpf->sport = sport;
    rpf->dhost = dupstr(dhost);
    rpf->dport = dport;
    rpf->addressfamily = addressfamily;
    rpf->log_description = dupstr(log_description);
    rpf->pfr = pfr;
    rpf->share_ctx = share_ctx;

    if (add234(s->rportfwds, rpf) != rpf) {
        free_rportfwd(rpf);
        return NULL;
    }

    if (!rpf->share_ctx) {
        PktOut *pktout = ssh_bpp_new_pktout(
            s->ppl.bpp, SSH2_MSG_GLOBAL_REQUEST);
        put_stringz(pktout, "tcpip-forward");
        put_bool(pktout, true);       /* want reply */
        put_stringz(pktout, rpf->shost);
        put_uint32(pktout, rpf->sport);
        pq_push(s->ppl.out_pq, pktout);

        ssh2_queue_global_request_handler(
            s, ssh2_rportfwd_globreq_response, rpf);
    }

    return rpf;
}

void ssh2_rportfwd_remove(ConnectionLayer *cl, struct ssh_rportfwd *rpf)
{
    struct ssh2_connection_state *s =
        container_of(cl, struct ssh2_connection_state, cl);

    if (rpf->share_ctx) {
        /*
         * We don't manufacture a cancel-tcpip-forward message for
         * remote port forwardings being removed on behalf of a
         * downstream; we just pass through the one the downstream
         * sent to us.
         */
    } else {
        PktOut *pktout = ssh_bpp_new_pktout(
            s->ppl.bpp, SSH2_MSG_GLOBAL_REQUEST);
        put_stringz(pktout, "cancel-tcpip-forward");
        put_bool(pktout, false);           /* _don't_ want reply */
        put_stringz(pktout, rpf->shost);
        put_uint32(pktout, rpf->sport);
        pq_push(s->ppl.out_pq, pktout);
    }

    assert(s->rportfwds);
    struct ssh_rportfwd *realpf = del234(s->rportfwds, rpf);
    assert(realpf == rpf);
    free_rportfwd(rpf);
}

SshChannel *ssh2_session_open(ConnectionLayer *cl, Channel *chan)
{
    struct ssh2_connection_state *s =
        container_of(cl, struct ssh2_connection_state, cl);
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    struct ssh2_channel *c = snew(struct ssh2_channel);
    PktOut *pktout;

    c->connlayer = s;
    ssh2_channel_init(c);
    c->halfopen = true;
    c->chan = chan;

    ppl_logevent("Opening main session channel");

    pktout = ssh2_chanopen_init(c, "session");
    pq_push(s->ppl.out_pq, pktout);

    return &c->sc;
}

SshChannel *ssh2_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketPeerInfo *pi)
{
    unreachable("Should never be called in the client");
}

SshChannel *ssh2_serverside_agent_open(ConnectionLayer *cl, Channel *chan)
{
    unreachable("Should never be called in the client");
}

static void ssh2_channel_response(
    struct ssh2_channel *c, PktIn *pkt, void *ctx)
{
    chan_request_response(c->chan, pkt->type == SSH2_MSG_CHANNEL_SUCCESS);
}

void ssh2channel_start_shell(SshChannel *sc, bool want_reply)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "shell", want_reply ? ssh2_channel_response : NULL, NULL);
    pq_push(s->ppl.out_pq, pktout);
}

void ssh2channel_start_command(
    SshChannel *sc, bool want_reply, const char *command)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "exec", want_reply ? ssh2_channel_response : NULL, NULL);
    put_stringz(pktout, command);
    pq_push(s->ppl.out_pq, pktout);
}

bool ssh2channel_start_subsystem(
    SshChannel *sc, bool want_reply, const char *subsystem)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "subsystem", want_reply ? ssh2_channel_response : NULL, NULL);
    put_stringz(pktout, subsystem);
    pq_push(s->ppl.out_pq, pktout);

    return true;
}

void ssh2channel_send_exit_status(SshChannel *sc, int status)
{
    unreachable("Should never be called in the client");
}

void ssh2channel_send_exit_signal(
    SshChannel *sc, ptrlen signame, bool core_dumped, ptrlen msg)
{
    unreachable("Should never be called in the client");
}

void ssh2channel_send_exit_signal_numeric(
    SshChannel *sc, int signum, bool core_dumped, ptrlen msg)
{
    unreachable("Should never be called in the client");
}

void ssh2channel_request_x11_forwarding(
    SshChannel *sc, bool want_reply, const char *authproto,
    const char *authdata, int screen_number, bool oneshot)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "x11-req", want_reply ? ssh2_channel_response : NULL, NULL);
    put_bool(pktout, oneshot);
    put_stringz(pktout, authproto);
    put_stringz(pktout, authdata);
    put_uint32(pktout, screen_number);
    pq_push(s->ppl.out_pq, pktout);
}

void ssh2channel_request_agent_forwarding(SshChannel *sc, bool want_reply)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "auth-agent-req@openssh.com",
        want_reply ? ssh2_channel_response : NULL, NULL);
    pq_push(s->ppl.out_pq, pktout);
}

void ssh2channel_request_pty(
    SshChannel *sc, bool want_reply, Conf *conf, int w, int h)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;
    strbuf *modebuf;

    PktOut *pktout = ssh2_chanreq_init(
        c, "pty-req", want_reply ? ssh2_channel_response : NULL, NULL);
    put_stringz(pktout, conf_get_str(conf, CONF_termtype));
    put_uint32(pktout, w);
    put_uint32(pktout, h);
    put_uint32(pktout, 0);	       /* pixel width */
    put_uint32(pktout, 0);	       /* pixel height */
    modebuf = strbuf_new();
    write_ttymodes_to_packet(
        BinarySink_UPCAST(modebuf), 2,
        get_ttymodes_from_conf(s->ppl.seat, conf));
    put_stringsb(pktout, modebuf);
    pq_push(s->ppl.out_pq, pktout);
}

bool ssh2channel_send_env_var(
    SshChannel *sc, bool want_reply, const char *var, const char *value)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "env", want_reply ? ssh2_channel_response : NULL, NULL);
    put_stringz(pktout, var);
    put_stringz(pktout, value);
    pq_push(s->ppl.out_pq, pktout);

    return true;
}

bool ssh2channel_send_serial_break(SshChannel *sc, bool want_reply, int length)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "break", want_reply ? ssh2_channel_response : NULL, NULL);
    put_uint32(pktout, length);
    pq_push(s->ppl.out_pq, pktout);

    return true;
}

bool ssh2channel_send_signal(
    SshChannel *sc, bool want_reply, const char *signame)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(
        c, "signal", want_reply ? ssh2_channel_response : NULL, NULL);
    put_stringz(pktout, signame);
    pq_push(s->ppl.out_pq, pktout);

    return true;
}

void ssh2channel_send_terminal_size_change(SshChannel *sc, int w, int h)
{
    struct ssh2_channel *c = container_of(sc, struct ssh2_channel, sc);
    struct ssh2_connection_state *s = c->connlayer;

    PktOut *pktout = ssh2_chanreq_init(c, "window-change", NULL, NULL);
    put_uint32(pktout, w);
    put_uint32(pktout, h);
    put_uint32(pktout, 0);	       /* pixel width */
    put_uint32(pktout, 0);	       /* pixel height */
    pq_push(s->ppl.out_pq, pktout);
}

bool ssh2_connection_need_antispoof_prompt(struct ssh2_connection_state *s)
{
    return !seat_set_trust_status(s->ppl.seat, false);
}
