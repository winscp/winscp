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
    uint32_t realcrc, gotcrc;
    int chunk;
    PktIn *pktin;

    ssh_cipher *cipher_in, *cipher_out;

    struct crcda_ctx *crcda_ctx;
    uint8_t iv[8];                     /* for crcda */

    bool pending_compression_request;
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

BinaryPacketProtocol *ssh1_bpp_new(LogContext *logctx)
{
    struct ssh1_bpp_state *s = snew(struct ssh1_bpp_state);
    memset(s, 0, sizeof(*s));
    s->bpp.vt = &ssh1_bpp_vtable;
    s->bpp.logctx = logctx;
    ssh_bpp_common_setup(&s->bpp);
    return &s->bpp;
}

static void ssh1_bpp_free(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = container_of(bpp, struct ssh1_bpp_state, bpp);
    if (s->cipher_in)
        ssh_cipher_free(s->cipher_in);
    if (s->cipher_out)
        ssh_cipher_free(s->cipher_out);
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
                         const ssh_cipheralg *cipher,
                         const void *session_key)
{
    struct ssh1_bpp_state *s;
    assert(bpp->vt == &ssh1_bpp_vtable);
    s = container_of(bpp, struct ssh1_bpp_state, bpp);

    assert(!s->cipher_in);
    assert(!s->cipher_out);

    if (cipher) {
        s->cipher_in = ssh_cipher_new(cipher);
        s->cipher_out = ssh_cipher_new(cipher);
        ssh_cipher_setkey(s->cipher_in, session_key);
        ssh_cipher_setkey(s->cipher_out, session_key);

        assert(!s->crcda_ctx);
        s->crcda_ctx = crcda_make_context();

        bpp_logevent("Initialised %s encryption", cipher->text_name);

        memset(s->iv, 0, sizeof(s->iv));

        assert(cipher->blksize <= sizeof(s->iv));
        ssh_cipher_setiv(s->cipher_in, s->iv);
        ssh_cipher_setiv(s->cipher_out, s->iv);
    }
}

void ssh1_bpp_start_compression(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s;
    assert(bpp->vt == &ssh1_bpp_vtable);
    s = container_of(bpp, struct ssh1_bpp_state, bpp);

    assert(!s->compctx);
    assert(!s->decompctx);

    s->compctx = ssh_compressor_new(&ssh_zlib);
    s->decompctx = ssh_decompressor_new(&ssh_zlib);

    bpp_logevent("Started zlib (RFC1950) compression");
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

static void ssh1_bpp_handle_input(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = container_of(bpp, struct ssh1_bpp_state, bpp);

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
            ssh_sw_abort(s->bpp.ssh,
                         "Extremely large packet length from remote suggests"
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
        s->pktin->qnode.on_free_queue = false;
        s->pktin->type = 0;

        s->maxlen = s->biglen;
        s->data = snew_plus_get_aux(s->pktin);

        BPP_READ(s->data, s->biglen);

        if (s->cipher_in && detect_attack(s->crcda_ctx,
                                          s->data, s->biglen, s->iv)) {
            ssh_sw_abort(s->bpp.ssh,
                         "Network attack (CRC compensation) detected!");
            crStopV;
        }
        /* Save the last cipher block, to be passed to the next call
         * to detect_attack */
        assert(s->biglen >= 8);
        memcpy(s->iv, s->data + s->biglen - 8, sizeof(s->iv));

        if (s->cipher_in)
            ssh_cipher_decrypt(s->cipher_in, s->data, s->biglen);

        s->realcrc = crc32_ssh1(make_ptrlen(s->data, s->biglen - 4));
        s->gotcrc = GET_32BIT_MSB_FIRST(s->data + s->biglen - 4);
        if (s->gotcrc != s->realcrc) {
            ssh_sw_abort(s->bpp.ssh, "Incorrect CRC received on packet");
            crStopV;
        }

        if (s->decompctx) {
            unsigned char *decompblk;
            int decomplen;
            if (!ssh_decompressor_decompress(
                    s->decompctx, s->data + s->pad, s->length + 1,
                    &decompblk, &decomplen)) {
                ssh_sw_abort(s->bpp.ssh,
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
                s->bpp.pls, s->pktin->type, false,
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
                        ssh1_bpp_start_compression(&s->bpp);
                    }

                    /*
                     * Either way, cancel the pending flag, and
                     * schedule a run of our output side in case we
                     * had any packets queued up in the meantime.
                     */
                    s->pending_compression_request = false;
                    queue_idempotent_callback(&s->bpp.ic_out_pq);
                }
                break;
            }
        }
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
    int pad, biglen, pktoffs;
    uint32_t crc;
    int len;

    if (s->bpp.logctx) {
        ptrlen pktdata = make_ptrlen(pkt->data + pkt->prefix,
                                     pkt->length - pkt->prefix);
        logblank_t blanks[MAX_BLANKS];
        int nblanks = ssh1_censor_packet(
            s->bpp.pls, pkt->type, true, pktdata, blanks);
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

    random_read(pkt->data + pktoffs, 4+8 - pktoffs);
    crc = crc32_ssh1(
        make_ptrlen(pkt->data + pktoffs + 4, biglen - 4)); /* all ex len */
    PUT_32BIT_MSB_FIRST(pkt->data + pktoffs + 4 + biglen - 4, crc);
    PUT_32BIT_MSB_FIRST(pkt->data + pktoffs, len);

    if (s->cipher_out)
        ssh_cipher_encrypt(s->cipher_out, pkt->data + pktoffs + 4, biglen);

    bufchain_add(s->bpp.out_raw, pkt->data + pktoffs,
                 biglen + 4); /* len(length+padding+type+data+CRC) */
}

static void ssh1_bpp_handle_output(BinaryPacketProtocol *bpp)
{
    struct ssh1_bpp_state *s = container_of(bpp, struct ssh1_bpp_state, bpp);
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
            s->pending_compression_request = true;
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
