/*
 * Trivial binary packet protocol for the 'bare' ssh-connection
 * protocol used in PuTTY's SSH-2 connection sharing system.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshcr.h"

struct ssh2_bare_bpp_state {
    int crState;
    long packetlen, maxlen;
    unsigned char *data;
    unsigned long incoming_sequence, outgoing_sequence;
    PktIn *pktin;

    BinaryPacketProtocol bpp;
};

static void ssh2_bare_bpp_free(BinaryPacketProtocol *bpp);
static void ssh2_bare_bpp_handle_input(BinaryPacketProtocol *bpp);
static void ssh2_bare_bpp_handle_output(BinaryPacketProtocol *bpp);
static PktOut *ssh2_bare_bpp_new_pktout(int type);

static const struct BinaryPacketProtocolVtable ssh2_bare_bpp_vtable = {
    ssh2_bare_bpp_free,
    ssh2_bare_bpp_handle_input,
    ssh2_bare_bpp_handle_output,
    ssh2_bare_bpp_new_pktout,
    ssh2_bpp_queue_disconnect, /* in sshcommon.c */
};

BinaryPacketProtocol *ssh2_bare_bpp_new(LogContext *logctx)
{
    struct ssh2_bare_bpp_state *s = snew(struct ssh2_bare_bpp_state);
    memset(s, 0, sizeof(*s));
    s->bpp.vt = &ssh2_bare_bpp_vtable;
    s->bpp.logctx = logctx;
    ssh_bpp_common_setup(&s->bpp);
    return &s->bpp;
}

static void ssh2_bare_bpp_free(BinaryPacketProtocol *bpp)
{
    struct ssh2_bare_bpp_state *s =
        container_of(bpp, struct ssh2_bare_bpp_state, bpp);
    sfree(s->pktin);
    sfree(s);
}

#define BPP_READ(ptr, len) do                                           \
    {                                                                   \
        bool success;                                                   \
        crMaybeWaitUntilV((success = bufchain_try_fetch_consume(        \
                               s->bpp.in_raw, ptr, len)) ||             \
                          s->bpp.input_eof);                            \
        if (!success)                                                   \
            goto eof;                                                   \
        ssh_check_frozen(s->bpp.ssh);                                   \
    } while (0)

static void ssh2_bare_bpp_handle_input(BinaryPacketProtocol *bpp)
{
    struct ssh2_bare_bpp_state *s =
        container_of(bpp, struct ssh2_bare_bpp_state, bpp);

    crBegin(s->crState);

    while (1) {
        /* Read the length field. */
        {
            unsigned char lenbuf[4];
            BPP_READ(lenbuf, 4);
            s->packetlen = toint(GET_32BIT_MSB_FIRST(lenbuf));
        }

        if (s->packetlen <= 0 || s->packetlen >= (long)OUR_V2_PACKETLIMIT) {
            ssh_sw_abort(s->bpp.ssh, "Invalid packet length received");
            crStopV;
        }

        /*
         * Allocate the packet to return, now we know its length.
         */
        s->pktin = snew_plus(PktIn, s->packetlen);
        s->pktin->qnode.prev = s->pktin->qnode.next = NULL;
        s->pktin->qnode.on_free_queue = false;
        s->maxlen = 0;
        s->data = snew_plus_get_aux(s->pktin);

        s->pktin->sequence = s->incoming_sequence++;

        /*
         * Read the remainder of the packet.
         */
        BPP_READ(s->data, s->packetlen);

        /*
         * The data we just read is precisely the initial type byte
         * followed by the packet payload.
         */
        s->pktin->type = s->data[0];
        s->data++;
        s->packetlen--;
        BinarySource_INIT(s->pktin, s->data, s->packetlen);

        /*
         * Log incoming packet, possibly omitting sensitive fields.
         */
        if (s->bpp.logctx) {
            logblank_t blanks[MAX_BLANKS];
            int nblanks = ssh2_censor_packet(
                s->bpp.pls, s->pktin->type, false,
                make_ptrlen(s->data, s->packetlen), blanks);
            log_packet(s->bpp.logctx, PKT_INCOMING, s->pktin->type,
                       ssh2_pkt_type(s->bpp.pls->kctx, s->bpp.pls->actx,
                                     s->pktin->type),
                       get_ptr(s->pktin), get_avail(s->pktin), nblanks, blanks,
                       &s->pktin->sequence, 0, NULL);
        }

        if (ssh2_bpp_check_unimplemented(&s->bpp, s->pktin)) {
            sfree(s->pktin);
            s->pktin = NULL;
            continue;
        }

        pq_push(&s->bpp.in_pq, s->pktin);
        s->pktin = NULL;
    }

  eof:
    if (!s->bpp.expect_close) {
        ssh_remote_error(s->bpp.ssh,
                         "Remote side unexpectedly closed network connection");
    } else {
        ssh_remote_eof(s->bpp.ssh, "Remote side closed network connection");
    }
    return;  /* avoid touching s now it's been freed */

    crFinishV;
}

static PktOut *ssh2_bare_bpp_new_pktout(int pkt_type)
{
    PktOut *pkt = ssh_new_packet();
    pkt->length = 4; /* space for packet length */
    pkt->type = pkt_type;
    put_byte(pkt, pkt_type);
    return pkt;
}

static void ssh2_bare_bpp_format_packet(struct ssh2_bare_bpp_state *s,
                                        PktOut *pkt)
{
    if (s->bpp.logctx) {
        ptrlen pktdata = make_ptrlen(pkt->data + 5, pkt->length - 5);
        logblank_t blanks[MAX_BLANKS];
        int nblanks = ssh2_censor_packet(
            s->bpp.pls, pkt->type, true, pktdata, blanks);
        log_packet(s->bpp.logctx, PKT_OUTGOING, pkt->type,
                   ssh2_pkt_type(s->bpp.pls->kctx, s->bpp.pls->actx,
                                 pkt->type),
                   pktdata.ptr, pktdata.len, nblanks, blanks,
                   &s->outgoing_sequence,
                   pkt->downstream_id, pkt->additional_log_text);
    }

    s->outgoing_sequence++;        /* only for diagnostics, really */

    PUT_32BIT_MSB_FIRST(pkt->data, pkt->length - 4);
    bufchain_add(s->bpp.out_raw, pkt->data, pkt->length);
}

static void ssh2_bare_bpp_handle_output(BinaryPacketProtocol *bpp)
{
    struct ssh2_bare_bpp_state *s =
        container_of(bpp, struct ssh2_bare_bpp_state, bpp);
    PktOut *pkt;

    while ((pkt = pq_pop(&s->bpp.out_pq)) != NULL) {
        ssh2_bare_bpp_format_packet(s, pkt);
        ssh_free_pktout(pkt);
    }
}
