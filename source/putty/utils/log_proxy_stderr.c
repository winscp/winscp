#include <assert.h>
#include <string.h>

#include "putty.h"
#include "network.h"

void psb_init(ProxyStderrBuf *psb)
{
    psb->size = 0;
    psb->prefix = "proxy";
}

void psb_set_prefix(ProxyStderrBuf *psb, const char *prefix)
{
    psb->prefix = prefix;
}

void log_proxy_stderr(Plug *plug, Socket *sock, ProxyStderrBuf *psb,
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
        pinitassert(psb->size < lenof(psb->buf));
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
        { // WINSCP
        size_t pos = 0;
        while (pos < psb->size) {
            char *nlpos = memchr(psb->buf + pos, '\n', psb->size - pos);
            if (!nlpos)
                break;

            /*
             * Found a newline in the buffer, so we can output a line.
             */
            { // WINSCP
            size_t endpos = nlpos - psb->buf;
            while (endpos > pos && (psb->buf[endpos-1] == '\n' ||
                                    psb->buf[endpos-1] == '\r'))
                endpos--;
            { // WINSCP
            char *msg = dupprintf(
                "%s: %.*s", psb->prefix, (int)(endpos - pos), psb->buf + pos);
            plug_log(plug, sock, PLUGLOG_PROXY_MSG, NULL, 0, msg, 0);
            sfree(msg);

            pos = nlpos - psb->buf + 1;
            assert(pos <= psb->size);
            } // WINSCP
            } // WINSCP
        }

        /*
         * If the buffer is completely full and we didn't output
         * anything, then output the whole thing, flagging it as a
         * truncated line.
         */
        if (pos == 0 && psb->size == lenof(psb->buf)) {
            char *msg = dupprintf(
                "%s (partial line): %.*s", psb->prefix, (int)psb->size,
                psb->buf);
            plug_log(plug, sock, PLUGLOG_PROXY_MSG, NULL, 0, msg, 0);
            sfree(msg);

            pos = psb->size = 0;
        }

        /*
         * Now move any remaining data up to the front of the buffer.
         */
        { // WINSCP
        size_t newsize = psb->size - pos;
        if (newsize)
            memmove(psb->buf, psb->buf + pos, newsize);
        psb->size = newsize;

        /*
         * And loop round again if there's more data to be read from
         * our input.
         */
        } // WINSCP
        } // WINSCP
    }
}
