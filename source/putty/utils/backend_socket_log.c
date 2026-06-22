#include <assert.h>
#include <string.h>

#include "putty.h"
#include "network.h"

void backend_socket_log(Seat *seat, LogContext *logctx, Socket *sock,
                        PlugLogType type, SockAddr *addr, int port,
                        const char *error_msg, int error_code, Conf *conf,
                        bool session_started)
{
    char addrbuf[256], *msg;

    switch (type) {
      case PLUGLOG_CONNECT_TRYING:
        sk_getaddr(addr, addrbuf, lenof(addrbuf));
        if (sk_addr_needs_port(addr)) {
            msg = dupprintf("Connecting to %s port %d", addrbuf, port);
        } else {
            msg = dupprintf("Connecting to %s", addrbuf);
        }
        break;
      case PLUGLOG_CONNECT_FAILED:
        sk_getaddr(addr, addrbuf, lenof(addrbuf));
        msg = dupprintf("Failed to connect to %s: %s", addrbuf, error_msg);
        break;
      case PLUGLOG_CONNECT_SUCCESS:
        if (addr)
            sk_getaddr(addr, addrbuf, lenof(addrbuf));
        else /* fallback if address unavailable */
            sprintf(addrbuf, "remote host");
        msg = dupprintf("Connected to %s", addrbuf);
        if (sock) {
            SocketEndpointInfo *local_end = sk_endpoint_info(sock, false);
            if (local_end) {
                char *newmsg = dupprintf("%s (from %s)", msg,
                                         local_end->log_text);
                sfree(msg);
                msg = newmsg;
                sk_free_endpoint_info(local_end);
            }
        }
        break;
      case PLUGLOG_PROXY_MSG: {
        /* Proxy-related log messages have their own identifying
         * prefix already, put on by our caller. */
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
        break;
      }
      default:
        msg = NULL;  /* shouldn't happen, but placate optimiser */
        break;
    }

    if (msg) {
        logevent(logctx, msg);
        sfree(msg);
    }
}
