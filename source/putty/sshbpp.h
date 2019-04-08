/*
 * Abstraction of the binary packet protocols used in SSH.
 */

#ifndef PUTTY_SSHBPP_H
#define PUTTY_SSHBPP_H

typedef struct BinaryPacketProtocol BinaryPacketProtocol;

struct BinaryPacketProtocolVtable {
    void (*free)(BinaryPacketProtocol *); 
    void (*handle_input)(BinaryPacketProtocol *);
    PktOut *(*new_pktout)(int type);
    void (*format_packet)(BinaryPacketProtocol *, PktOut *);
};

struct BinaryPacketProtocol {
    const struct BinaryPacketProtocolVtable *vt;
    bufchain *in_raw, *out_raw;
    PktInQueue *in_pq;
    PacketLogSettings *pls;
    LogContext *logctx;

    int seen_disconnect;
    char *error;
};

#define ssh_bpp_free(bpp) ((bpp)->vt->free(bpp))
#define ssh_bpp_handle_input(bpp) ((bpp)->vt->handle_input(bpp))
#define ssh_bpp_new_pktout(bpp, type) ((bpp)->vt->new_pktout(type))
#define ssh_bpp_format_packet(bpp, pkt) ((bpp)->vt->format_packet(bpp, pkt))

BinaryPacketProtocol *ssh1_bpp_new(void);
void ssh1_bpp_new_cipher(BinaryPacketProtocol *bpp,
                         const struct ssh1_cipheralg *cipher,
                         const void *session_key);
/* requested_compression() notifies the SSH-1 BPP that we've just sent
 * a request to enable compression, which means that on receiving the
 * next SSH1_SMSG_SUCCESS or SSH1_SMSG_FAILURE message, it should set
 * up zlib compression if it was SUCCESS. */
void ssh1_bpp_requested_compression(BinaryPacketProtocol *bpp);

/*
 * Structure that tracks how much data is sent and received, for
 * purposes of triggering an SSH-2 rekey when either one gets over a
 * configured limit. In each direction, the flag 'running' indicates
 * that we haven't hit the limit yet, and 'remaining' tracks how much
 * longer until we do. The macro DTS_CONSUME subtracts a given amount
 * from the counter in a particular direction, and evaluates to a
 * boolean indicating whether the limit has been hit.
 *
 * The limit is sticky: once 'running' has flipped to false,
 * 'remaining' is no longer decremented, so it shouldn't dangerously
 * wrap round.
 */
struct DataTransferStats {
    struct {
        int running;
        unsigned long remaining;
    } in, out;
};
#define DTS_CONSUME(stats, direction, size)             \
    ((stats)->direction.running &&                      \
     (stats)->direction.remaining <= (size) ?           \
     ((stats)->direction.running = FALSE, TRUE) :       \
     ((stats)->direction.remaining -= (size), FALSE))

BinaryPacketProtocol *ssh2_bpp_new(struct DataTransferStats *stats);
void ssh2_bpp_new_outgoing_crypto(
    BinaryPacketProtocol *bpp,
    const struct ssh2_cipheralg *cipher, const void *ckey, const void *iv,
    const struct ssh2_macalg *mac, int etm_mode, const void *mac_key,
    const struct ssh_compression_alg *compression);
void ssh2_bpp_new_incoming_crypto(
    BinaryPacketProtocol *bpp,
    const struct ssh2_cipheralg *cipher, const void *ckey, const void *iv,
    const struct ssh2_macalg *mac, int etm_mode, const void *mac_key,
    const struct ssh_compression_alg *compression);

BinaryPacketProtocol *ssh2_bare_bpp_new(void);

/*
 * The initial code to handle the SSH version exchange is also
 * structured as an implementation of BinaryPacketProtocol, because
 * that makes it easy to switch from that to the next BPP once it
 * tells us which one we're using.
 */
struct ssh_version_receiver {
    void (*got_ssh_version)(struct ssh_version_receiver *rcv,
                            int major_version);
};
BinaryPacketProtocol *ssh_verstring_new(
    Conf *conf, Frontend *frontend, int bare_connection_mode,
    const char *protoversion, struct ssh_version_receiver *rcv);
const char *ssh_verstring_get_remote(BinaryPacketProtocol *);
const char *ssh_verstring_get_local(BinaryPacketProtocol *);
int ssh_verstring_get_bugs(BinaryPacketProtocol *);

#endif /* PUTTY_SSHBPP_H */
