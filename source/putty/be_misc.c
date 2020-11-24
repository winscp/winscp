/*
 * be_misc.c: helper functions shared between main network backends.
 */

#include <assert.h>
#include <string.h>

#include "putty.h"
#include "network.h"

void backend_socket_log(Seat *seat, LogContext *logctx,
                        int type, SockAddr *addr, int port,
                        const char *error_msg, int error_code, Conf *conf,
                        bool session_started)
{
    char addrbuf[256], *msg;

    switch (type) {
      case 0:
        sk_getaddr(addr, addrbuf, lenof(addrbuf));
        if (sk_addr_needs_port(addr)) {
            msg = dupprintf("Connecting to %s port %d", addrbuf, port);
        } else {
            msg = dupprintf("Connecting to %s", addrbuf);
        }
        break;
      case 1:
        sk_getaddr(addr, addrbuf, lenof(addrbuf));
        msg = dupprintf("Failed to connect to %s: %s", addrbuf, error_msg);
        break;
      case 2:
        /* Proxy-related log messages have their own identifying
         * prefix already, put on by our caller. */
        {
            int len, log_to_term;

            /* Suffix \r\n temporarily, so we can log to the terminal. */
            msg = dupprintf("%s\r\n", error_msg);
            len = strlen(msg);
            assert(len >= 2);

            log_to_term = conf_get_int(conf, CONF_proxy_log_to_term);
            if (log_to_term == AUTO)
                log_to_term = session_started ? FORCE_OFF : FORCE_ON;
            if (log_to_term == FORCE_ON)
                seat_stderr(seat, msg, len);

            msg[len-2] = '\0';         /* remove the \r\n again */
        }
        break;
      default:
        msg = NULL;  /* shouldn't happen, but placate optimiser */
        break;
    }

    if (msg) {
        logevent(logctx, msg);
        sfree(msg);
    }
}

void psb_init(ProxyStderrBuf *psb)
{
    psb->size = 0;
}

void log_proxy_stderr(Plug *plug, ProxyStderrBuf *psb,
                      const void *vdata, size_t len)
{
    const char *data = (const char *)vdata;

    /*
     * This helper function allows us to collect the data written to a
     * local proxy command's standard error in whatever size chunks we
     * happen to get from its pipe, and whenever we have a complete
     * line, we pass it to plug_log.
     *
     * (We also do this when the buffer in psb fills up, to avoid just
     * allocating more and more memory forever, and also to keep Event
     * Log lines reasonably bounded in size.)
     *
     * Prerequisites: a plug to log to, and a ProxyStderrBuf stored
     * somewhere to collect any not-yet-output partial line.
     */

    while (len > 0) {
        /*
         * Copy as much data into psb->buf as will fit.
         */
        assert(psb->size < lenof(psb->buf));
        size_t to_consume = lenof(psb->buf) - psb->size;
        if (to_consume > len)
            to_consume = len;
        memcpy(psb->buf + psb->size, data, to_consume);
        data += to_consume;
        len -= to_consume;
        psb->size += to_consume;

        /*
         * Output any full lines in psb->buf.
         */
        size_t pos = 0;
        while (pos < psb->size) {
            char *nlpos = memchr(psb->buf + pos, '\n', psb->size - pos);
            if (!nlpos)
                break;

            /*
             * Found a newline in the buffer, so we can output a line.
             */
            size_t endpos = nlpos - psb->buf;
            while (endpos > pos && (psb->buf[endpos-1] == '\n' ||
                                    psb->buf[endpos-1] == '\r'))
                endpos--;
            char *msg = dupprintf(
                "proxy: %.*s", (int)(endpos - pos), psb->buf + pos);
            plug_log(plug, 2, NULL, 0, msg, 0);
            sfree(msg);

            pos = nlpos - psb->buf + 1;
            assert(pos <= psb->size);
        }

        /*
         * If the buffer is completely full and we didn't output
         * anything, then output the whole thing, flagging it as a
         * truncated line.
         */
        if (pos == 0 && psb->size == lenof(psb->buf)) {
            char *msg = dupprintf(
                "proxy (partial line): %.*s", (int)psb->size, psb->buf);
            plug_log(plug, 2, NULL, 0, msg, 0);
            sfree(msg);

            pos = psb->size = 0;
        }

        /*
         * Now move any remaining data up to the front of the buffer.
         */
        size_t newsize = psb->size - pos;
        if (newsize)
            memmove(psb->buf, psb->buf + pos, newsize);
        psb->size = newsize;

        /*
         * And loop round again if there's more data to be read from
         * our input.
         */
    }
}
