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
    PacketQueue *in_pq;
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
void ssh1_bpp_start_compression(BinaryPacketProtocol *bpp);

BinaryPacketProtocol *ssh2_bpp_new(void);
void ssh2_bpp_new_outgoing_crypto(
    BinaryPacketProtocol *bpp,
    const struct ssh2_cipheralg *cipher, const void *ckey, const void *iv,
    const struct ssh2_macalg *mac, int etm_mode, const void *mac_key,
    const struct ssh_compress *compression);
void ssh2_bpp_new_incoming_crypto(
    BinaryPacketProtocol *bpp,
    const struct ssh2_cipheralg *cipher, const void *ckey, const void *iv,
    const struct ssh2_macalg *mac, int etm_mode, const void *mac_key,
    const struct ssh_compress *compression);

BinaryPacketProtocol *ssh2_bare_bpp_new(void);

#ifdef MPEXT
const ssh1_cipher * ssh1_bpp_get_cipher(BinaryPacketProtocol *bpp);
int ssh1_bpp_get_compressing(BinaryPacketProtocol *bpp);
const ssh2_cipher * ssh2_bpp_get_cscipher(BinaryPacketProtocol *bpp);
const ssh2_cipher * ssh2_bpp_get_sccipher(BinaryPacketProtocol *bpp);
const struct ssh_compress * ssh2_bpp_get_cscomp(BinaryPacketProtocol *bpp);
const struct ssh_compress * ssh2_bpp_get_sccomp(BinaryPacketProtocol *bpp);
#endif

#endif /* PUTTY_SSHBPP_H */
