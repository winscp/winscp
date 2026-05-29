/*
 * Formats a command line using the formatting language common to
 * Telnet proxies, local proxy commands, and pre-connection hooks.
 *
 * Returns the formatted command, dynamically allocated. Also,
 * optionally, returns flags indicating whether %user or %pass were
 * referred to but not provided in the Conf.
 */

#include "putty.h"

char *format_connection_setup_command(
    const char *fmt, SockAddr *addr, int port, Conf *conf, unsigned *flags_out)
{
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
