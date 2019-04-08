/*
 * Binary packet protocol for SSH-1.
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshcr.h"

struct ssh1_bpp_state {
    int crState;
    long len, pad, biglen, length, maxlen;
    unsigned char *data;
    unsigned long realcrc, gotcrc;
    int chunk;
    PktIn *pktin;

    ssh1_cipher *cipher;

    struct crcda_ctx *crcda_ctx;

    int pending_compression_request;
    ssh_compressor *compctx;
    ssh_decompressor *decompctx;

    BinaryPacketProtocol bpp;
};

static void ssh1_bpp_free(BinaryPacketProtocol *bpp);
static void ssh1_bpp_handle_input(BinaryPacketProtocol *bpp);
static void ssh1_bpp_handle_output(BinaryPacketProtocol *bpp);
static void ssh1_bpp_queue_disconnect(BinaryPacketProtocol *bpp,
                                      const char *msg, int category);
static PktOut *ssh1_bpp_new_pktout(int type);

static const struct BinaryPacketProtocolVtable ssh1_bpp_vtable = {
    ssh1_bpp_free,
    ssh1_bpp_handle_input,
    ssh1_bpp_handle_output,
    ssh1_bpp_new_pktout,
    ssh1_bpp_queue_disconnect,
};

BinaryPacketProtocol *ssh1_bpp_new(Frontend *frontend)
{
    struct ssh1_bpp_state *s = snew(struct ssh1_bpp_state);
    memset(s, 0, sizeof(*s));
    s->bpp.vt = &ssh1_bpp_vtable;
    s->bpp.frontend = frontend;
    ssh_bpp_common_setup(&s->bpp);
    return &s->bpp;
}

static void ssh1_bpp_free(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = FROMFIELD(bpp, struct ssh1_bpp_state, bpp);
    if (s->cipher)
        ssh1_cipher_free(s->cipher);
    if (s->compctx)
        ssh_compressor_free(s->compctx);
    if (s->decompctx)
        ssh_decompressor_free(s->decompctx);
    if (s->crcda_ctx)
        crcda_free_context(s->crcda_ctx);
    sfree(s->pktin);
    sfree(s);
}

void ssh1_bpp_new_cipher(BinaryPacketProtocol *bpp,
                         const struct ssh1_cipheralg *cipher,
                         const void *session_key)
{
    struct ssh1_bpp_state *s;
    assert(bpp->vt == &ssh1_bpp_vtable);
    s = FROMFIELD(bpp, struct ssh1_bpp_state, bpp);

    assert(!s->cipher);

    if (cipher) {
        s->cipher = ssh1_cipher_new(cipher);
        ssh1_cipher_sesskey(s->cipher, session_key);

        assert(!s->crcda_ctx);
        s->crcda_ctx = crcda_make_context();
    }
}

#define BPP_READ(ptr, len) do                                   \
    {                                                           \
        crMaybeWaitUntilV(bufchain_try_fetch_consume(           \
                              s->bpp.in_raw, ptr, len));        \
    } while (0)

static void ssh1_bpp_handle_input(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = FROMFIELD(bpp, struct ssh1_bpp_state, bpp);

    crBegin(s->crState);

    while (1) {
        s->maxlen = 0;
        s->length = 0;

        {
            unsigned char lenbuf[4];
            BPP_READ(lenbuf, 4);
            s->len = toint(GET_32BIT_MSB_FIRST(lenbuf));
        }

        if (s->len < 0 || s->len > 262144) { /* SSH1.5-mandated max size */
            s->bpp.error = dupprintf(
                "Extremely large packet length from server suggests"
                " data stream corruption");
            crStopV;
        }

        s->pad = 8 - (s->len % 8);
        s->biglen = s->len + s->pad;
        s->length = s->len - 5;

        /*
         * Allocate the packet to return, now we know its length.
         */
        s->pktin = snew_plus(PktIn, s->biglen);
        s->pktin->qnode.prev = s->pktin->qnode.next = NULL;
        s->pktin->qnode.on_free_queue = FALSE;
        s->pktin->type = 0;

        s->maxlen = s->biglen;
        s->data = snew_plus_get_aux(s->pktin);

        BPP_READ(s->data, s->biglen);

        if (s->cipher && detect_attack(s->crcda_ctx,
                                       s->data, s->biglen, NULL)) {
            s->bpp.error = dupprintf(
                "Network attack (CRC compensation) detected!");
            crStopV;
        }

        if (s->cipher)
            ssh1_cipher_decrypt(s->cipher, s->data, s->biglen);

        s->realcrc = crc32_compute(s->data, s->biglen - 4);
        s->gotcrc = GET_32BIT(s->data + s->biglen - 4);
        if (s->gotcrc != s->realcrc) {
            s->bpp.error = dupprintf(
                "Incorrect CRC received on packet");
            crStopV;
        }

        if (s->decompctx) {
            unsigned char *decompblk;
            int decomplen;
            if (!ssh_decompressor_decompress(
                    s->decompctx, s->data + s->pad, s->length + 1,
                    &decompblk, &decomplen)) {
                s->bpp.error = dupprintf(
                    "Zlib decompression encountered invalid data");
                crStopV;
            }

            if (s->maxlen < s->pad + decomplen) {
                PktIn *old_pktin = s->pktin;

                s->maxlen = s->pad + decomplen;
                s->pktin = snew_plus(PktIn, s->maxlen);
                *s->pktin = *old_pktin; /* structure copy */
                s->data = snew_plus_get_aux(s->pktin);

                smemclr(old_pktin, s->biglen);
                sfree(old_pktin);
            }

            memcpy(s->data + s->pad, decompblk, decomplen);
            sfree(decompblk);
            s->length = decomplen - 1;
        }

        /*
         * Now we can find the bounds of the semantic content of the
         * packet, and the initial type byte.
         */
        s->data += s->pad;
        s->pktin->type = *s->data++;
        BinarySource_INIT(s->pktin, s->data, s->length);

        if (s->bpp.logctx) {
            logblank_t blanks[MAX_BLANKS];
            int nblanks = ssh1_censor_packet(
                s->bpp.pls, s->pktin->type, FALSE,
                make_ptrlen(s->data, s->length), blanks);
            log_packet(s->bpp.logctx, PKT_INCOMING, s->pktin->type,
                       ssh1_pkt_type(s->pktin->type),
                       get_ptr(s->pktin), get_avail(s->pktin), nblanks, blanks,
                       NULL, 0, NULL);
        }

        pq_push(&s->bpp.in_pq, s->pktin);

        {
            int type = s->pktin->type;
            s->pktin = NULL;

            switch (type) {
              case SSH1_MSG_DISCONNECT:
                s->bpp.seen_disconnect = TRUE;
                break;

              case SSH1_SMSG_SUCCESS:
              case SSH1_SMSG_FAILURE:
                if (s->pending_compression_request) {
                    /*
                     * This is the response to
                     * SSH1_CMSG_REQUEST_COMPRESSION.
                     */
                    if (type == SSH1_SMSG_SUCCESS) {
                        /*
                         * If the response was positive, start
                         * compression.
                         */
                        assert(!s->compctx);
                        assert(!s->decompctx);

                        s->compctx = ssh_compressor_new(&ssh_zlib);
                        s->decompctx = ssh_decompressor_new(&ssh_zlib);
                    }

                    /*
                     * Either way, cancel the pending flag, and
                     * schedule a run of our output side in case we
                     * had any packets queued up in the meantime.
                     */
                    s->pending_compression_request = FALSE;
                    queue_idempotent_callback(&s->bpp.ic_out_pq);
                }
                break;
            }
        }
    }
    crFinishV;
}

static PktOut *ssh1_bpp_new_pktout(int pkt_type)
{
    PktOut *pkt = ssh_new_packet();
    pkt->length = 4 + 8;	    /* space for length + max padding */
    put_byte(pkt, pkt_type);
    pkt->prefix = pkt->length;
    pkt->type = pkt_type;
    pkt->downstream_id = 0;
    pkt->additional_log_text = NULL;
    return pkt;
}

static void ssh1_bpp_format_packet(struct ssh1_bpp_state *s, PktOut *pkt)
{
    int pad, biglen, i, pktoffs;
    unsigned long crc;
    int len;

    if (s->bpp.logctx) {
        ptrlen pktdata = make_ptrlen(pkt->data + pkt->prefix,
                                     pkt->length - pkt->prefix);
        logblank_t blanks[MAX_BLANKS];
        int nblanks = ssh1_censor_packet(
            s->bpp.pls, pkt->type, TRUE, pktdata, blanks);
        log_packet(s->bpp.logctx, PKT_OUTGOING, pkt->type,
                   ssh1_pkt_type(pkt->type),
                   pktdata.ptr, pktdata.len, nblanks, blanks,
                   NULL, 0, NULL);
    }

    if (s->compctx) {
        unsigned char *compblk;
        int complen;
        ssh_compressor_compress(s->compctx, pkt->data + 12, pkt->length - 12,
                                &compblk, &complen, 0);
        /* Replace the uncompressed packet data with the compressed
         * version. */
        pkt->length = 12;
        put_data(pkt, compblk, complen);
        sfree(compblk);
    }

    put_uint32(pkt, 0); /* space for CRC */
    len = pkt->length - 4 - 8;  /* len(type+data+CRC) */
    pad = 8 - (len % 8);
    pktoffs = 8 - pad;
    biglen = len + pad;         /* len(padding+type+data+CRC) */

    for (i = pktoffs; i < 4+8; i++)
        pkt->data[i] = random_byte();
    crc = crc32_compute(pkt->data + pktoffs + 4,
                        biglen - 4); /* all ex len */
    PUT_32BIT(pkt->data + pktoffs + 4 + biglen - 4, crc);
    PUT_32BIT(pkt->data + pktoffs, len);

    if (s->cipher)
        ssh1_cipher_encrypt(s->cipher, pkt->data + pktoffs + 4, biglen);

    bufchain_add(s->bpp.out_raw, pkt->data + pktoffs,
                 biglen + 4); /* len(length+padding+type+data+CRC) */
}

static void ssh1_bpp_handle_output(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = FROMFIELD(bpp, struct ssh1_bpp_state, bpp);
    PktOut *pkt;

    if (s->pending_compression_request) {
        /*
         * Don't send any output packets while we're awaiting a
         * response to SSH1_CMSG_REQUEST_COMPRESSION, because if they
         * cross over in transit with the responding SSH1_CMSG_SUCCESS
         * then the other end could decode them with the wrong
         * compression settings.
         */
        return;
    }

    while ((pkt = pq_pop(&s->bpp.out_pq)) != NULL) {
        int type = pkt->type;
        ssh1_bpp_format_packet(s, pkt);
        ssh_free_pktout(pkt);

        if (type == SSH1_CMSG_REQUEST_COMPRESSION) {
            /*
             * When we see the actual compression request go past, set
             * the pending flag, and stop processing packets this
             * time.
             */
            s->pending_compression_request = TRUE;
            break;
        }
    }
}

static void ssh1_bpp_queue_disconnect(BinaryPacketProtocol *bpp,
                                      const char *msg, int category)
{
    PktOut *pkt = ssh_bpp_new_pktout(bpp, SSH1_MSG_DISCONNECT);
    put_stringz(pkt, msg);
    pq_push(&bpp->out_pq, pkt);
}

#ifdef MPEXT
const ssh1_cipher * ssh1_bpp_get_cipher(BinaryPacketProtocol *bpp)
{
    return FROMFIELD(bpp, struct ssh1_bpp_state, bpp)->cipher;
}
#endif
