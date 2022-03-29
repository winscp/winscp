/*
 * Packet-censoring code for SSH-2, used to identify sensitive fields
 * like passwords so that the logging system can avoid writing them
 * into log files.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"

int ssh2_censor_packet(
    const PacketLogSettings *pls, int type, bool sender_is_client,
    ptrlen pkt, logblank_t *blanks)
{
    int nblanks = 0;
    ptrlen str;
    BinarySource src[1];

    BinarySource_BARE_INIT_PL(src, pkt);

    if (pls->omit_data &&
        (type == SSH2_MSG_CHANNEL_DATA ||
         type == SSH2_MSG_CHANNEL_EXTENDED_DATA)) {
        /* "Session data" packets - omit the data string. */
        get_uint32(src);              /* skip channel id */
        if (type == SSH2_MSG_CHANNEL_EXTENDED_DATA)
            get_uint32(src);          /* skip extended data type */
        str = get_string(src);
        if (!get_err(src)) {
            assert(nblanks < MAX_BLANKS);
            blanks[nblanks].offset = src->pos - str.len;
            blanks[nblanks].type = PKTLOG_OMIT;
            blanks[nblanks].len = str.len;
            nblanks++;
        }
    }

    if (sender_is_client && pls->omit_passwords) {
        if (type == SSH2_MSG_USERAUTH_REQUEST) {
            /* If this is a password packet, blank the password(s). */
            get_string(src);              /* username */
            get_string(src);              /* service name */
            str = get_string(src);        /* auth method */
            if (ptrlen_eq_string(str, "password")) {
                get_bool(src);
                /* Blank the password field. */
                str = get_string(src);
                if (!get_err(src)) {
                    assert(nblanks < MAX_BLANKS);
                    blanks[nblanks].offset = src->pos - str.len;
                    blanks[nblanks].type = PKTLOG_BLANK;
                    blanks[nblanks].len = str.len;
                    nblanks++;
                    /* If there's another password field beyond it
                     * (change of password), blank that too. */
                    str = get_string(src);
                    if (!get_err(src))
                        blanks[nblanks-1].len =
                            src->pos - blanks[nblanks].offset;
                }
            }
        } else if (pls->actx == SSH2_PKTCTX_KBDINTER &&
                   type == SSH2_MSG_USERAUTH_INFO_RESPONSE) {
            /* If this is a keyboard-interactive response packet,
             * blank the responses. */
            get_uint32(src);
            assert(nblanks < MAX_BLANKS);
            blanks[nblanks].offset = src->pos;
            blanks[nblanks].type = PKTLOG_BLANK;
            do {
                str = get_string(src);
            } while (!get_err(src));
            blanks[nblanks].len = src->pos - blanks[nblanks].offset;
            nblanks++;
        } else if (type == SSH2_MSG_CHANNEL_REQUEST) {
            /*
             * If this is an X forwarding request packet, blank the
             * fake auth data.
             *
             * Note that while we blank the X authentication data
             * here, we don't take any special action to blank the
             * start of an X11 channel, so using MIT-MAGIC-COOKIE-1
             * and actually opening an X connection without having
             * session blanking enabled is likely to leak your cookie
             * into the log.
             */
            get_uint32(src);
            str = get_string(src);
            if (ptrlen_eq_string(str, "x11-req")) {
                get_bool(src);
                get_bool(src);
                get_string(src);
                str = get_string(src);
                if (!get_err(src)) {
                    assert(nblanks < MAX_BLANKS);
                    blanks[nblanks].offset = src->pos - str.len;
                    blanks[nblanks].type = PKTLOG_BLANK;
                    blanks[nblanks].len = str.len;
                    nblanks++;
                }
            }
        }
    }

    return nblanks;
}
