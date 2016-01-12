/*
 * be_misc.c: helper functions shared between main network backends.
 */

#include <assert.h>
#include <string.h>

#define DEFINE_PLUG_METHOD_MACROS
#include "putty.h"
#include "network.h"

void backend_socket_log(void *frontend, int type, SockAddr addr, int port,
                        const char *error_msg, int error_code, Conf *conf,
                        int session_started)
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
                from_backend(frontend, TRUE, msg, len);

            msg[len-2] = '\0';         /* remove the \r\n again */
        }
        break;
      default:
        msg = NULL;  /* shouldn't happen, but placate optimiser */
        break;
    }

    if (msg) {
        logevent(frontend, msg);
        sfree(msg);
    }
}

void log_proxy_stderr(Plug plug, bufchain *buf, const void *vdata, int len)
{
    const char *data = (const char *)vdata;
    int pos = 0;
    int msglen;
    char *nlpos, *msg, *fullmsg;

    /*
     * This helper function allows us to collect the data written to a
     * local proxy command's standard error in whatever size chunks we
     * happen to get from its pipe, and whenever we have a complete
     * line, we pass it to plug_log.
     *
     * Prerequisites: a plug to log to, and a bufchain stored
     * somewhere to collect the data in.
     */

    while (pos < len && (nlpos = memchr(data+pos, '\n', len-pos)) != NULL) {
        /*
         * Found a newline in the current input buffer. Append it to
         * the bufchain (which may contain a partial line from last
         * time).
         */
        bufchain_add(buf, data + pos, nlpos - (data + pos));

        /*
         * Collect the resulting line of data and pass it to plug_log.
         */
        msglen = bufchain_size(buf);
        msg = snewn(msglen+1, char);
        bufchain_fetch(buf, msg, msglen);
        bufchain_consume(buf, msglen);
        msg[msglen] = '\0';
        fullmsg = dupprintf("proxy: %s", msg);
        plug_log(plug, 2, NULL, 0, fullmsg, 0);
        sfree(fullmsg);
        sfree(msg);

        /*
         * Advance past the newline.
         */
        pos += nlpos+1 - (data + pos);
    }

    /*
     * Now any remaining data is a partial line, which we save for
     * next time.
     */
    bufchain_add(buf, data + pos, len - pos);
}
