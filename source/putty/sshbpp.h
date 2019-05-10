/*
 * Abstraction of the binary packet protocols used in SSH.
 */

#ifndef PUTTY_SSHBPP_H
#define PUTTY_SSHBPP_H

struct BinaryPacketProtocolVtable {
    void (*free)(BinaryPacketProtocol *); 
    void (*handle_input)(BinaryPacketProtocol *);
    void (*handle_output)(BinaryPacketProtocol *);
    PktOut *(*new_pktout)(int type);
    void (*queue_disconnect)(BinaryPacketProtocol *,
                             const char *msg, int category);
};

struct BinaryPacketProtocol {
    const struct BinaryPacketProtocolVtable *vt;
    bufchain *in_raw, *out_raw;
    bool input_eof;   /* set this if in_raw will never be added to again */
    PktInQueue in_pq;
    PktOutQueue out_pq;
    PacketLogSettings *pls;
    LogContext *logctx;
    Ssh *ssh;

    /* ic_in_raw is filled in by the BPP (probably by calling
     * ssh_bpp_common_setup). The BPP's owner triggers it when data is
     * added to in_raw, and also when the BPP is newly created. */
    IdempotentCallback ic_in_raw;

    /* ic_out_pq is entirely internal to the BPP itself; it's used as
     * the callback on out_pq. */
    IdempotentCallback ic_out_pq;

    int remote_bugs;

    /* Set this if remote connection closure should not generate an
     * error message (either because it's not to be treated as an
     * error at all, or because some other error message has already
     * been emitted). */
    bool expect_close;
};

static inline void ssh_bpp_handle_input(BinaryPacketProtocol *bpp)
{ bpp->vt->handle_input(bpp); }
static inline void ssh_bpp_handle_output(BinaryPacketProtocol *bpp)
{ bpp->vt->handle_output(bpp); }
static inline PktOut *ssh_bpp_new_pktout(BinaryPacketProtocol *bpp, int type)
{ return bpp->vt->new_pktout(type); }
static inline void ssh_bpp_queue_disconnect(BinaryPacketProtocol *bpp,
                                            const char *msg, int category)
{ bpp->vt->queue_disconnect(bpp, msg, category); }

/* ssh_bpp_free is more than just a macro wrapper on the vtable; it
 * does centralised parts of the freeing too. */
void ssh_bpp_free(BinaryPacketProtocol *bpp);

BinaryPacketProtocol *ssh1_bpp_new(LogContext *logctx);
void ssh1_bpp_new_cipher(BinaryPacketProtocol *bpp,
                         const ssh_cipheralg *cipher,
                         const void *session_key);
/* This is only called from outside the BPP in server mode; in client
 * mode the BPP detects compression start time automatically by
 * snooping message types */
void ssh1_bpp_start_compression(BinaryPacketProtocol *bpp);

/* Helper routine which does common BPP initialisation, e.g. setting
 * up in_pq and out_pq, and initialising input_consumer. */
void ssh_bpp_common_setup(BinaryPacketProtocol *);

/* Common helper functions between the SSH-2 full and bare BPPs */
void ssh2_bpp_queue_disconnect(BinaryPacketProtocol *bpp,
                               const char *msg, int category);
bool ssh2_bpp_check_unimplemented(BinaryPacketProtocol *bpp, PktIn *pktin);

/* Convenience macro for BPPs to send formatted strings to the Event
 * Log. Assumes a function parameter called 'bpp' is in scope. */
#define bpp_logevent(...) ( \
    logevent_and_free((bpp)->logctx, dupprintf(__VA_ARGS__)))

/*
 * Structure that tracks how much data is sent and received, for
 * purposes of triggering an SSH-2 rekey when either one gets over a
 * configured limit. In each direction, the flag 'running' indicates
 * that we haven't hit the limit yet, and 'remaining' tracks how much
 * longer until we do. The function dts_consume() subtracts a given
 * amount from the counter in a particular direction, and sets
 * 'expired' if the limit has been hit.
 *
 * The limit is sticky: once 'running' has flipped to false,
 * 'remaining' is no longer decremented, so it shouldn't dangerously
 * wrap round.
 */
struct DataTransferStatsDirection {
    bool running, expired;
    unsigned long remaining;
};
struct DataTransferStats {
    struct DataTransferStatsDirection in, out;
};
static inline void dts_consume(struct DataTransferStatsDirection *s,
                               unsigned long size_consumed)
{
    if (s->running) {
        if (s->remaining <= size_consumed) {
            s->running = false;
            s->expired = true;
        } else {
            s->remaining -= size_consumed;
        }
    }
}
static inline void dts_reset(struct DataTransferStatsDirection *s,
                             unsigned long starting_size)
{
    s->expired = false;
    s->remaining = starting_size;
    /*
     * The semantics of setting CONF_ssh_rekey_data to zero are to
     * disable data-volume based rekeying completely. So if the
     * starting size is actually zero, we don't set 'running' to true
     * in the first place, which means we won't ever set the expired
     * flag.
     */
    s->running = (starting_size != 0);
}

BinaryPacketProtocol *ssh2_bpp_new(
    LogContext *logctx, struct DataTransferStats *stats, bool is_server);
void ssh2_bpp_new_outgoing_crypto(
    BinaryPacketProtocol *bpp,
    const ssh_cipheralg *cipher, const void *ckey, const void *iv,
    const ssh2_macalg *mac, bool etm_mode, const void *mac_key,
    const ssh_compression_alg *compression, bool delayed_compression);
void ssh2_bpp_new_incoming_crypto(
    BinaryPacketProtocol *bpp,
    const ssh_cipheralg *cipher, const void *ckey, const void *iv,
    const ssh2_macalg *mac, bool etm_mode, const void *mac_key,
    const ssh_compression_alg *compression, bool delayed_compression);

/*
 * A query method specific to the interface between ssh2transport and
 * ssh2bpp. If true, it indicates that we're potentially in the
 * race-condition-prone part of delayed compression setup and so
 * asynchronous outgoing transport-layer packets are currently not
 * being sent, which means in particular that it would be a bad idea
 * to start a rekey because then we'd stop responding to anything
 * _other_ than transport-layer packets and deadlock the protocol.
 */
bool ssh2_bpp_rekey_inadvisable(BinaryPacketProtocol *bpp);

BinaryPacketProtocol *ssh2_bare_bpp_new(LogContext *logctx);

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
    Conf *conf, LogContext *logctx, bool bare_connection_mode,
    const char *protoversion, struct ssh_version_receiver *rcv,
    bool server_mode, const char *impl_name);
const char *ssh_verstring_get_remote(BinaryPacketProtocol *);
const char *ssh_verstring_get_local(BinaryPacketProtocol *);
int ssh_verstring_get_bugs(BinaryPacketProtocol *);

#endif /* PUTTY_SSHBPP_H */
