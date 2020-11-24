/*
 * Packet-censoring code for SSH-1, used to identify sensitive fields
 * like passwords so that the logging system can avoid writing them
 * into log files.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"

int ssh1_censor_packet(
    const PacketLogSettings *pls, int type, bool sender_is_client,
    ptrlen pkt, logblank_t *blanks)
{
    int nblanks = 0;
    ptrlen str;
    BinarySource src[1];

    BinarySource_BARE_INIT_PL(src, pkt);

    if (pls->omit_data &&
        (type == SSH1_SMSG_STDOUT_DATA ||
         type == SSH1_SMSG_STDERR_DATA ||
         type == SSH1_CMSG_STDIN_DATA ||
         type == SSH1_MSG_CHANNEL_DATA)) {
        /* "Session data" packets - omit the data string. */
        if (type == SSH1_MSG_CHANNEL_DATA)
            get_uint32(src);           /* skip channel id */
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
        if (type == SSH1_CMSG_AUTH_PASSWORD ||
            type == SSH1_CMSG_AUTH_TIS_RESPONSE ||
            type == SSH1_CMSG_AUTH_CCARD_RESPONSE) {
            /* If this is a password or similar packet, blank the
             * password(s). */
            assert(nblanks < MAX_BLANKS);
            blanks[nblanks].offset = 0;
            blanks[nblanks].len = pkt.len;
            blanks[nblanks].type = PKTLOG_BLANK;
            nblanks++;
        } else if (type == SSH1_CMSG_X11_REQUEST_FORWARDING) {
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
            get_string(src);              /* skip protocol name */
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

    return nblanks;
}
