/*
 * SSH main session channel handling.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "putty.h"
#include "ssh.h"
#include "sshppl.h"
#include "sshchan.h"

static void mainchan_free(Channel *chan);
static void mainchan_open_confirmation(Channel *chan);
static void mainchan_open_failure(Channel *chan, const char *errtext);
static int mainchan_send(Channel *chan, int is_stderr, const void *, int);
static void mainchan_send_eof(Channel *chan);
static void mainchan_set_input_wanted(Channel *chan, int wanted);
static char *mainchan_log_close_msg(Channel *chan);
static int mainchan_rcvd_exit_status(Channel *chan, int status);
static int mainchan_rcvd_exit_signal(
    Channel *chan, ptrlen signame, int core_dumped, ptrlen msg);
static int mainchan_rcvd_exit_signal_numeric(
    Channel *chan, int signum, int core_dumped, ptrlen msg);
static void mainchan_request_response(Channel *chan, int success);

static const struct ChannelVtable mainchan_channelvt = {
    mainchan_free,
    mainchan_open_confirmation,
    mainchan_open_failure,
    mainchan_send,
    mainchan_send_eof,
    mainchan_set_input_wanted,
    mainchan_log_close_msg,
    chan_default_want_close,
    mainchan_rcvd_exit_status,
    mainchan_rcvd_exit_signal,
    mainchan_rcvd_exit_signal_numeric,
    mainchan_request_response,
};

typedef enum MainChanType {
    MAINCHAN_SESSION, MAINCHAN_DIRECT_TCPIP
} MainChanType;

typedef struct mainchan {
    SshChannel *sc;
    Conf *conf;
    PacketProtocolLayer *ppl;
    ConnectionLayer *cl;

    MainChanType type;
    int is_simple;

    int req_x11, req_agent, req_pty, req_cmd_primary, req_cmd_fallback;
    int n_req_env, n_env_replies, n_env_fails;
    int eof_pending, eof_sent, got_pty, ready;

    int term_width, term_height;

    Channel chan;
} mainchan;

mainchan *mainchan_new(
    PacketProtocolLayer *ppl, ConnectionLayer *cl, Conf *conf,
    int term_width, int term_height, int is_simple, SshChannel **sc_out)
{
    mainchan *mc;

    if (conf_get_int(conf, CONF_ssh_no_shell))
        return NULL;                   /* no main channel at all */

    mc = snew(mainchan);
    memset(mc, 0, sizeof(mainchan));
    mc->ppl = ppl;
    mc->cl = cl;
    mc->conf = conf_copy(conf);
    mc->term_width = term_width;
    mc->term_height = term_height;
    mc->is_simple = is_simple;

    mc->sc = NULL;
    mc->chan.vt = &mainchan_channelvt;
    mc->chan.initial_fixed_window_size = 0;

    if (*conf_get_str(mc->conf, CONF_ssh_nc_host)) {
        const char *host = conf_get_str(mc->conf, CONF_ssh_nc_host);
        int port = conf_get_int(mc->conf, CONF_ssh_nc_port);

        mc->sc = ssh_lportfwd_open(cl, host, port, "main channel",
                                   NULL, &mc->chan);
        mc->type = MAINCHAN_DIRECT_TCPIP;
    } else {
        mc->sc = ssh_session_open(cl, &mc->chan);
        mc->type = MAINCHAN_SESSION;
    }

    if (sc_out) *sc_out = mc->sc;
    return mc;
}

static void mainchan_free(Channel *chan)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    conf_free(mc->conf);
    sfree(mc);
}

static void mainchan_try_fallback_command(mainchan *mc);
static void mainchan_ready(mainchan *mc);

static void mainchan_open_confirmation(Channel *chan)
{
    mainchan *mc = container_of(chan, mainchan, chan);
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */

    seat_update_specials_menu(mc->ppl->seat);
    ppl_logevent(("Opened main channel"));

    if (mc->is_simple)
        sshfwd_hint_channel_is_simple(mc->sc);

    if (mc->type == MAINCHAN_SESSION) {
	/*
	 * Send the CHANNEL_REQUESTS for the main session channel.
         */
        char *key, *val, *cmd;
        struct X11Display *x11disp;
        struct X11FakeAuth *x11auth;
        int retry_cmd_now = FALSE;

	if (conf_get_int(mc->conf, CONF_x11_forward)) {
            char *x11_setup_err;
            if ((x11disp = x11_setup_display(
                     conf_get_str(mc->conf, CONF_x11_display),
                     mc->conf, &x11_setup_err)) == NULL) {
                ppl_logevent(("X11 forwarding not enabled: unable to"
                              " initialise X display: %s", x11_setup_err));
                sfree(x11_setup_err);
            } else {
                x11auth = ssh_add_x11_display(
                    mc->cl, conf_get_int(mc->conf, CONF_x11_auth), x11disp);

                sshfwd_request_x11_forwarding(
                    mc->sc, TRUE, x11auth->protoname, x11auth->datastring,
                    x11disp->screennum, FALSE);
                mc->req_x11 = TRUE;
            }
        }

	if (ssh_agent_forwarding_permitted(mc->cl)) {
            sshfwd_request_agent_forwarding(mc->sc, TRUE);
            mc->req_agent = TRUE;
        }

	if (!conf_get_int(mc->conf, CONF_nopty)) {
            sshfwd_request_pty(
                mc->sc, TRUE, mc->conf, mc->term_width, mc->term_height);
            mc->req_pty = TRUE;
        }

        for (val = conf_get_str_strs(mc->conf, CONF_environmt, NULL, &key);
             val != NULL;
             val = conf_get_str_strs(mc->conf, CONF_environmt, key, &key)) {
            sshfwd_send_env_var(mc->sc, TRUE, key, val);
            mc->n_req_env++;
        }
        if (mc->n_req_env)
            ppl_logevent(("Sent %d environment variables", mc->n_req_env));

        cmd = conf_get_str(mc->conf, CONF_remote_cmd);
        if (conf_get_int(mc->conf, CONF_ssh_subsys)) {
            retry_cmd_now = !sshfwd_start_subsystem(mc->sc, TRUE, cmd);
        } else if (*cmd) {
            sshfwd_start_command(mc->sc, TRUE, cmd);
        } else {
            sshfwd_start_shell(mc->sc, TRUE);
        }

        if (retry_cmd_now)
            mainchan_try_fallback_command(mc);
        else
            mc->req_cmd_primary = TRUE;

    } else {
        ssh_set_ldisc_option(mc->cl, LD_ECHO, TRUE);
        ssh_set_ldisc_option(mc->cl, LD_EDIT, TRUE);
        mainchan_ready(mc);
    }
}

static void mainchan_try_fallback_command(mainchan *mc)
{
    const char *cmd = conf_get_str(mc->conf, CONF_remote_cmd2);
    if (conf_get_int(mc->conf, CONF_ssh_subsys2)) {
        sshfwd_start_subsystem(mc->sc, TRUE, cmd);
    } else {
        sshfwd_start_command(mc->sc, TRUE, cmd);
    }
    mc->req_cmd_fallback = TRUE;
}

static void mainchan_request_response(Channel *chan, int success)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */

    if (mc->req_x11) {
        mc->req_x11 = FALSE;

        if (success) {
            ppl_logevent(("X11 forwarding enabled"));
            ssh_enable_x_fwd(mc->cl);
        } else {
            ppl_logevent(("X11 forwarding refused"));
        }
        return;
    }

    if (mc->req_agent) {
        mc->req_agent = FALSE;

        if (success) {
            ppl_logevent(("Agent forwarding enabled"));
            ssh_enable_agent_fwd(mc->cl);
        } else {
            ppl_logevent(("Agent forwarding refused"));
        }
        return;
    }

    if (mc->req_pty) {
        mc->req_pty = FALSE;

        if (success) {
            ppl_logevent(("Allocated pty"));
            mc->got_pty = TRUE;
        } else {
            ppl_logevent(("Server refused to allocate pty"));
            ppl_printf(("Server refused to allocate pty\r\n"));
            ssh_set_ldisc_option(mc->cl, LD_ECHO, TRUE);
            ssh_set_ldisc_option(mc->cl, LD_EDIT, TRUE);
        }
        return;
    }

    if (mc->n_env_replies < mc->n_req_env) {
        int j = mc->n_env_replies++;
        if (!success) {
            ppl_logevent(("Server refused to set environment variable %s",
                          conf_get_str_nthstrkey(mc->conf,
                                                 CONF_environmt, j)));
            mc->n_env_fails++;
        }

        if (mc->n_env_replies == mc->n_req_env) {
            if (mc->n_env_fails == 0) {
                ppl_logevent(("All environment variables successfully set"));
            } else if (mc->n_env_fails == mc->n_req_env) {
                ppl_logevent(("All environment variables refused"));
                ppl_printf(("Server refused to set environment "
                            "variables\r\n"));
            } else {
                ppl_printf(("Server refused to set all environment "
                            "variables\r\n"));
            }
        }
        return;
    }

    if (mc->req_cmd_primary) {
        mc->req_cmd_primary = FALSE;

        if (success) {
            ppl_logevent(("Started a shell/command"));
            mainchan_ready(mc);
        } else if (*conf_get_str(mc->conf, CONF_remote_cmd2) || conf_get_int(mc->conf, CONF_force_remote_cmd2)) { // WINSCP
            ppl_logevent(("Primary command failed; attempting fallback"));
            mainchan_try_fallback_command(mc);
        } else {
            /*
             * If there's no remote_cmd2 configured, then we have no
             * fallback command, so we've run out of options.
             */
            ssh_sw_abort(mc->ppl->ssh,
                         "Server refused to start a shell/command");
        }
        return;
    }

    if (mc->req_cmd_fallback) {
        mc->req_cmd_fallback = FALSE;

        if (success) {
            ppl_logevent(("Started a shell/command"));
            ssh_got_fallback_cmd(mc->ppl->ssh);
            mainchan_ready(mc);
        } else {
            ssh_sw_abort(mc->ppl->ssh,
                         "Server refused to start a shell/command");
        }
        return;
    }
}

static void mainchan_ready(mainchan *mc)
{
    mc->ready = TRUE;

    ssh_set_wants_user_input(mc->cl, TRUE);
    ssh_ppl_got_user_input(mc->ppl); /* in case any is already queued */

    /* If an EOF arrived before we were ready, handle it now. */
    if (mc->eof_pending) {
        mc->eof_pending = FALSE;
	mainchan_special_cmd(mc, SS_EOF, 0);
    }

    ssh_ldisc_update(mc->ppl->ssh);
    queue_idempotent_callback(&mc->ppl->ic_process_queue);
}

struct mainchan_open_failure_abort_ctx {
    Ssh *ssh;
    char *abort_message;
};

static void mainchan_open_failure_abort(void *vctx)
{
    struct mainchan_open_failure_abort_ctx *ctx =
        (struct mainchan_open_failure_abort_ctx *)vctx;
    ssh_sw_abort(
        ctx->ssh, "Server refused to open main channel: %s",
        ctx->abort_message);
    sfree(ctx->abort_message);
    sfree(ctx);
}

static void mainchan_open_failure(Channel *chan, const char *errtext)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);

    struct mainchan_open_failure_abort_ctx *ctx =
        snew(struct mainchan_open_failure_abort_ctx);

    ctx->ssh = mc->ppl->ssh;
    ctx->abort_message = dupstr(errtext);
    queue_toplevel_callback(get_log_callback_set(mc->cl->logctx), mainchan_open_failure_abort, ctx);
}

static int mainchan_send(Channel *chan, int is_stderr,
                         const void *data, int length)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    return seat_output(mc->ppl->seat, is_stderr, data, length);
}

static void mainchan_send_eof(Channel *chan)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */

    if (!mc->eof_sent & (seat_eof(mc->ppl->seat) || mc->got_pty)) {
        /*
         * Either seat_eof told us that the front end wants us to
         * close the outgoing side of the connection as soon as we see
         * EOF from the far end, or else we've unilaterally decided to
         * do that because we've allocated a remote pty and hence EOF
         * isn't a particularly meaningful concept.
         */
        sshfwd_write_eof(mc->sc);
        ppl_logevent(("Sent EOF message"));
    }
    mc->eof_sent = TRUE;
    ssh_set_wants_user_input(mc->cl, FALSE); /* now stop reading from stdin */
}

static void mainchan_set_input_wanted(Channel *chan, int wanted)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);

    /*
     * This is the main channel of the SSH session, i.e. the one tied
     * to the standard input (or GUI) of the primary SSH client user
     * interface. So ssh->send_ok is how we control whether we're
     * reading from that input.
     */
    ssh_set_wants_user_input(mc->cl, wanted);
}

static char *mainchan_log_close_msg(Channel *chan)
{
    return dupstr("Main session channel closed");
}

static int mainchan_rcvd_exit_status(Channel *chan, int status)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */

    ssh_got_exitcode(mc->ppl->ssh, status);
    ppl_logevent(("Session sent command exit status %d", status));
    return TRUE;
}

static void mainchan_log_exit_signal_common(
    mainchan *mc, const char *sigdesc,
    int core_dumped, ptrlen msg)
{
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */

    const char *core_msg = core_dumped ? " (core dumped)" : "";
    const char *msg_pre = (msg.len ? " (" : "");
    const char *msg_post = (msg.len ? ")" : "");
    ppl_logevent(("Session exited on %s%s%s%.*s%s",
                  sigdesc, core_msg, msg_pre, PTRLEN_PRINTF(msg), msg_post));
}

static int mainchan_rcvd_exit_signal(
    Channel *chan, ptrlen signame, int core_dumped, ptrlen msg)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    int exitcode;
    char *signame_str;

    /*
     * Translate the signal description back into a locally meaningful
     * number, or 128 if the string didn't match any we recognise.
     */
    exitcode = 128;

    #define SIGNAL_SUB(s) \
        if (ptrlen_eq_string(signame, #s))      \
            exitcode = 128 + SIG ## s;
    #define SIGNAL_MAIN(s, text) SIGNAL_SUB(s)
    #define SIGNALS_LOCAL_ONLY
    #include "sshsignals.h"
    #undef SIGNAL_SUB
    #undef SIGNAL_MAIN
    #undef SIGNALS_LOCAL_ONLY

    ssh_got_exitcode(mc->ppl->ssh, exitcode);
    if (exitcode == 128)
        signame_str = dupprintf("unrecognised signal \"%.*s\"",
                                PTRLEN_PRINTF(signame));
    else
        signame_str = dupprintf("signal SIG%.*s", PTRLEN_PRINTF(signame));
    mainchan_log_exit_signal_common(mc, signame_str, core_dumped, msg);
    sfree(signame_str);
    return TRUE;
}

static int mainchan_rcvd_exit_signal_numeric(
    Channel *chan, int signum, int core_dumped, ptrlen msg)
{
    pinitassert(chan->vt == &mainchan_channelvt);
    mainchan *mc = container_of(chan, mainchan, chan);
    char *signum_str;

    ssh_got_exitcode(mc->ppl->ssh, 128 + signum);
    signum_str = dupprintf("signal %d", signum);
    mainchan_log_exit_signal_common(mc, signum_str, core_dumped, msg);
    sfree(signum_str);
    return TRUE;
}

void mainchan_get_specials(
    mainchan *mc, add_special_fn_t add_special, void *ctx)
{
    /* FIXME: this _does_ depend on whether these services are supported */

    add_special(ctx, "Break", SS_BRK, 0);

    #define SIGNAL_MAIN(name, desc) \
    add_special(ctx, "SIG" #name " (" desc ")", SS_SIG ## name, 0);
    #define SIGNAL_SUB(name)
    #include "sshsignals.h"
    #undef SIGNAL_MAIN
    #undef SIGNAL_SUB

    add_special(ctx, "More signals", SS_SUBMENU, 0);

    #define SIGNAL_MAIN(name, desc)
    #define SIGNAL_SUB(name) \
    add_special(ctx, "SIG" #name, SS_SIG ## name, 0);
    #include "sshsignals.h"
    #undef SIGNAL_MAIN
    #undef SIGNAL_SUB

    add_special(ctx, NULL, SS_EXITMENU, 0);
}

static const char *ssh_signal_lookup(SessionSpecialCode code)
{
    #define SIGNAL_SUB(name) \
    if (code == SS_SIG ## name) return #name;
    #define SIGNAL_MAIN(name, desc) SIGNAL_SUB(name)
    #include "sshsignals.h"
    #undef SIGNAL_MAIN
    #undef SIGNAL_SUB

    /* If none of those clauses matched, fail lookup. */
    return NULL;
}

void mainchan_special_cmd(mainchan *mc, SessionSpecialCode code, int arg)
{
    PacketProtocolLayer *ppl = mc->ppl; /* for ppl_logevent */
    const char *signame;

    if (code == SS_EOF) {
        if (!mc->ready) {
            /*
             * Buffer the EOF to send as soon as the main channel is
             * fully set up.
             */
            mc->eof_pending = TRUE;
        } else if (!mc->eof_sent) {
            sshfwd_write_eof(mc->sc);
            mc->eof_sent = TRUE;
        }
    } else if (code == SS_BRK) {
        sshfwd_send_serial_break(
            mc->sc, FALSE, 0 /* default break length */);
    } else if ((signame = ssh_signal_lookup(code)) != NULL) {
        /* It's a signal. */
        sshfwd_send_signal(mc->sc, FALSE, signame);
        ppl_logevent(("Sent signal SIG%s", signame));
    }
}

void mainchan_terminal_size(mainchan *mc, int width, int height)
{
    mc->term_width = width;
    mc->term_height = height;

    if (mc->req_pty || mc->got_pty)
        sshfwd_send_terminal_size_change(mc->sc, width, height);
}
