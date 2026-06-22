/*
 * "Telnet" proxy negotiation.
 *
 * (This is for ad-hoc proxies where you connect to the proxy's
 * telnet port and send a command such as `connect host port'. The
 * command is configurable, since this proxy type is typically not
 * standardised or at all well-defined.)
 */

#include "putty.h"
#include "network.h"
#include "proxy.h"
#include "sshcr.h"

char *format_telnet_command(SockAddr *addr, int port, Conf *conf,
                            unsigned *flags_out)
{
    char *fmt = conf_get_str(conf, CONF_proxy_telnet_command);
    int so = 0, eo = 0;
    strbuf *buf = strbuf_new();
    unsigned flags = 0;

    /* we need to escape \\, \%, \r, \n, \t, \x??, \0???,
     * %%, %host, %port, %user, and %pass
     */

    while (fmt[eo] != 0) {

        /* scan forward until we hit end-of-line,
         * or an escape character (\ or %) */
        while (fmt[eo] != 0 && fmt[eo] != '%' && fmt[eo] != '\\')
            eo++;

        /* if we hit eol, break out of our escaping loop */
        if (fmt[eo] == 0) break;

        /* if there was any unescaped text before the escape
         * character, send that now */
        if (eo != so)
            put_data(buf, fmt + so, eo - so);

        so = eo++;

        /* if the escape character was the last character of
         * the line, we'll just stop and send it. */
        if (fmt[eo] == 0) break;

        if (fmt[so] == '\\') {

            /* we recognize \\, \%, \r, \n, \t, \x??.
             * anything else, we just send unescaped (including the \).
             */

            switch (fmt[eo]) {

              case '\\':
                put_byte(buf, '\\');
                eo++;
                break;

              case '%':
                put_byte(buf, '%');
                eo++;
                break;

              case 'r':
                put_byte(buf, '\r');
                eo++;
                break;

              case 'n':
                put_byte(buf, '\n');
                eo++;
                break;

              case 't':
                put_byte(buf, '\t');
                eo++;
                break;

              case 'x':
              case 'X': {
                /* escaped hexadecimal value (ie. \xff) */
                unsigned char v = 0;
                int i = 0;

                for (;;) {
                    eo++;
                    if (fmt[eo] >= '0' && fmt[eo] <= '9')
                        v += fmt[eo] - '0';
                    else if (fmt[eo] >= 'a' && fmt[eo] <= 'f')
                        v += fmt[eo] - 'a' + 10;
                    else if (fmt[eo] >= 'A' && fmt[eo] <= 'F')
                        v += fmt[eo] - 'A' + 10;
                    else {
                        /* non hex character, so we abort and just
                         * send the whole thing unescaped (including \x)
                         */
                        put_byte(buf, '\\');
                        eo = so + 1;
                        break;
                    }

                    /* we only extract two hex characters */
                    if (i == 1) {
                        put_byte(buf, v);
                        eo++;
                        break;
                    }

                    i++;
                    v <<= 4;
                }
                break;
              }

              default:
                put_data(buf, fmt + so, 2);
                eo++;
                break;
            }
        } else {

            /* % escape. we recognize %%, %host, %port, %user, %pass.
             * %proxyhost, %proxyport. Anything else we just send
             * unescaped (including the %).
             */

            if (fmt[eo] == '%') {
                put_byte(buf, '%');
                eo++;
            }
            else if (strnicmp(fmt + eo, "host", 4) == 0) {
                char dest[512];
                sk_getaddr(addr, dest, lenof(dest));
                put_data(buf, dest, strlen(dest));
                eo += 4;
            }
            else if (strnicmp(fmt + eo, "port", 4) == 0) {
                put_fmt(buf, "%d", port);
                eo += 4;
            }
            else if (strnicmp(fmt + eo, "user", 4) == 0) {
                const char *username = conf_get_str(conf, CONF_proxy_username);
                put_data(buf, username, strlen(username));
                eo += 4;
                if (!*username)
                    flags |= TELNET_CMD_MISSING_USERNAME;
            }
            else if (strnicmp(fmt + eo, "pass", 4) == 0) {
                const char *password = conf_get_str(conf, CONF_proxy_password);
                put_data(buf, password, strlen(password));
                eo += 4;
                if (!*password)
                    flags |= TELNET_CMD_MISSING_PASSWORD;
            }
            else if (strnicmp(fmt + eo, "proxyhost", 9) == 0) {
                const char *host = conf_get_str(conf, CONF_proxy_host);
                put_data(buf, host, strlen(host));
                eo += 9;
            }
            else if (strnicmp(fmt + eo, "proxyport", 9) == 0) {
                int port = conf_get_int(conf, CONF_proxy_port);
                put_fmt(buf, "%d", port);
                eo += 9;
            }
            else {
                /* we don't escape this, so send the % now, and
                 * don't advance eo, so that we'll consider the
                 * text immediately following the % as unescaped.
                 */
                put_byte(buf, '%');
            }
        }

        /* resume scanning for additional escapes after this one. */
        so = eo;
    }

    /* if there is any unescaped text at the end of the line, send it */
    if (eo != so) {
        put_data(buf, fmt + so, eo - so);
    }

    if (flags_out)
        *flags_out = flags;
    return strbuf_to_str(buf);
}

typedef struct TelnetProxyNegotiator {
    int crLine;
    Conf *conf;
    char *formatted_cmd;
    prompts_t *prompts;
    int username_prompt_index, password_prompt_index;
    ProxyNegotiator pn;
} TelnetProxyNegotiator;

static ProxyNegotiator *proxy_telnet_new(const ProxyNegotiatorVT *vt)
{
    TelnetProxyNegotiator *s = snew(TelnetProxyNegotiator);
    memset(s, 0, sizeof(*s));
    s->pn.vt = vt;
    return &s->pn;
}

static void proxy_telnet_free(ProxyNegotiator *pn)
{
    TelnetProxyNegotiator *s = container_of(pn, TelnetProxyNegotiator, pn);
    if (s->conf)
        conf_free(s->conf);
    if (s->prompts)
        free_prompts(s->prompts);
    burnstr(s->formatted_cmd);
    delete_callbacks_for_context(get_seat_callback_set(pn->ps->clientseat), s);
    sfree(s);
}

static void proxy_telnet_process_queue_callback(void *vctx)
{
    TelnetProxyNegotiator *s = (TelnetProxyNegotiator *)vctx;
    proxy_negotiator_process_queue(&s->pn);
}

static void proxy_telnet_process_queue(ProxyNegotiator *pn)
{
    TelnetProxyNegotiator *s = container_of(pn, TelnetProxyNegotiator, pn);

    crBegin(s->crLine);

    s->conf = conf_copy(pn->ps->conf);

    /*
     * Make an initial attempt to figure out the command we want, and
     * see if it tried to include a username or password that we don't
     * have.
     */
    {
        unsigned flags;
        s->formatted_cmd = format_telnet_command(
            pn->ps->remote_addr, pn->ps->remote_port, s->conf, &flags);

        if (pn->itr && (flags & (TELNET_CMD_MISSING_USERNAME |
                                 TELNET_CMD_MISSING_PASSWORD))) {
            burnstr(s->formatted_cmd);
            s->formatted_cmd = NULL;

            /*
             * We're missing at least one of the two parts, and we
             * have an Interactor we can use to prompt for them, so
             * try it.
             */
            s->prompts = proxy_new_prompts(pn->ps);
            s->prompts->to_server = true;
            s->prompts->from_server = false;
            s->prompts->name = dupstr("Telnet proxy authentication");
            if (flags & TELNET_CMD_MISSING_USERNAME) {
                s->username_prompt_index = s->prompts->n_prompts;
                add_prompt(s->prompts, dupstr("Proxy username: "), true);
            } else {
                s->username_prompt_index = -1;
            }
            if (flags & TELNET_CMD_MISSING_PASSWORD) {
                s->password_prompt_index = s->prompts->n_prompts;
                add_prompt(s->prompts, dupstr("Proxy password: "), false);
            } else {
                s->password_prompt_index = -1;
            }

            /*
             * This prompt is presented extremely early in PuTTY's
             * setup. (Very promptly, you might say.)
             *
             * In particular, we can get here through a chain of
             * synchronous calls from backend_init, which means (in
             * GUI PuTTY) that the terminal we'll be sending this
             * prompt to may not have its Ldisc set up yet (due to
             * cyclic dependencies among all the things that have to
             * be initialised).
             *
             * So we'll start by having ourself called back via a
             * toplevel callback, to make sure we don't call
             * seat_get_userpass_input until we've returned from
             * backend_init and the frontend has finished getting
             * everything ready.
             */
            queue_toplevel_callback(get_seat_callback_set(pn->ps->clientseat), proxy_telnet_process_queue_callback, s);
            crReturnV;

            while (true) {
                SeatPromptResult spr = seat_get_userpass_input(
                    interactor_announce(pn->itr), s->prompts);
                if (spr.kind == SPRK_OK) {
                    break;
                } else if (spr_is_abort(spr)) {
                    proxy_spr_abort(pn, spr);
                    crStopV;
                }
                crReturnV;
            }

            if (s->username_prompt_index != -1) {
                conf_set_str(
                    s->conf, CONF_proxy_username,
                    prompt_get_result_ref(
                        s->prompts->prompts[s->username_prompt_index]));
            }

            if (s->password_prompt_index != -1) {
                conf_set_str(
                    s->conf, CONF_proxy_password,
                    prompt_get_result_ref(
                        s->prompts->prompts[s->password_prompt_index]));
            }

            free_prompts(s->prompts);
            s->prompts = NULL;
        }

        /*
         * Now format the command a second time, with the results of
         * those prompts written into s->conf.
         */
        s->formatted_cmd = format_telnet_command(
            pn->ps->remote_addr, pn->ps->remote_port, s->conf, NULL);
    }

    /*
     * Log the command, with some changes. Firstly, we regenerate it
     * with the password masked; secondly, we escape control
     * characters so that the log message is printable.
     */
    conf_set_str(s->conf, CONF_proxy_password, "*password*");
    {
        char *censored_cmd = format_telnet_command(
            pn->ps->remote_addr, pn->ps->remote_port, s->conf, NULL);

        strbuf *logmsg = strbuf_new();
        put_datapl(logmsg, PTRLEN_LITERAL("Sending Telnet proxy command: "));
        put_c_string_literal(logmsg, ptrlen_from_asciz(censored_cmd));

        plug_log(pn->ps->plug, &pn->ps->sock, PLUGLOG_PROXY_MSG, NULL, 0,
                 logmsg->s, 0);
        strbuf_free(logmsg);
        sfree(censored_cmd);
    }

    /*
     * Actually send the command.
     */
    put_dataz(pn->output, s->formatted_cmd);

    /*
     * Unconditionally report success. We don't hang around waiting
     * for error messages from the proxy, because this proxy type is
     * so ad-hoc that we wouldn't know how to even recognise an error
     * message if we saw one, let alone what to do about it.
     */
    pn->done = true;

    crFinishV;
}

const struct ProxyNegotiatorVT telnet_proxy_negotiator_vt = {
    // WINSCP
    /*.new =*/ proxy_telnet_new,
    /*.process_queue =*/ proxy_telnet_process_queue,
    /*.free =*/ proxy_telnet_free,
    /*.type =*/ "Telnet",
};
