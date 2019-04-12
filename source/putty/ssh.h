#include <stdio.h>
#include <string.h>

#include "puttymem.h"
#include "tree234.h"
#include "network.h"
#include "int64.h"
#include "misc.h"

struct ssh_channel;

/*
 * Buffer management constants. There are several of these for
 * various different purposes:
 *
 *  - SSH1_BUFFER_LIMIT is the amount of backlog that must build up
 *    on a local data stream before we throttle the whole SSH
 *    connection (in SSH-1 only). Throttling the whole connection is
 *    pretty drastic so we set this high in the hope it won't
 *    happen very often.
 *
 *  - SSH_MAX_BACKLOG is the amount of backlog that must build up
 *    on the SSH connection itself before we defensively throttle
 *    _all_ local data streams. This is pretty drastic too (though
 *    thankfully unlikely in SSH-2 since the window mechanism should
 *    ensure that the server never has any need to throttle its end
 *    of the connection), so we set this high as well.
 *
 *  - OUR_V2_WINSIZE is the default window size we present on SSH-2
 *    channels.
 *
 *  - OUR_V2_BIGWIN is the window size we advertise for the only
 *    channel in a simple connection.  It must be <= INT_MAX.
 *
 *  - OUR_V2_MAXPKT is the official "maximum packet size" we send
 *    to the remote side. This actually has nothing to do with the
 *    size of the _packet_, but is instead a limit on the amount
 *    of data we're willing to receive in a single SSH2 channel
 *    data message.
 *
 *  - OUR_V2_PACKETLIMIT is actually the maximum size of SSH
 *    _packet_ we're prepared to cope with.  It must be a multiple
 *    of the cipher block size, and must be at least 35000.
 */

#define SSH1_BUFFER_LIMIT 32768
#define SSH_MAX_BACKLOG 32768
#define OUR_V2_WINSIZE 16384
#define OUR_V2_BIGWIN 0x7fffffff
#define OUR_V2_MAXPKT 0x4000UL
#define OUR_V2_PACKETLIMIT 0x9000UL

typedef struct PacketQueueNode PacketQueueNode;
struct PacketQueueNode {
    PacketQueueNode *next, *prev;
    int on_free_queue;     /* is this packet scheduled for freeing? */
};

typedef struct PktIn {
    int type;
    unsigned long sequence; /* SSH-2 incoming sequence number */
    PacketQueueNode qnode;  /* for linking this packet on to a queue */
    BinarySource_IMPLEMENTATION;
} PktIn;

typedef struct PktOut {
    long prefix;            /* bytes up to and including type field */
    long length;            /* total bytes, including prefix */
    int type;
    long minlen;            /* SSH-2: ensure wire length is at least this */
    unsigned char *data;    /* allocated storage */
    long maxlen;	    /* amount of storage allocated for `data' */

    /* Extra metadata used in SSH packet logging mode, allowing us to
     * log in the packet header line that the packet came from a
     * connection-sharing downstream and what if anything unusual was
     * done to it. The additional_log_text field is expected to be a
     * static string - it will not be freed. */
    unsigned downstream_id;
    const char *additional_log_text;

    PacketQueueNode qnode;  /* for linking this packet on to a queue */
    BinarySink_IMPLEMENTATION;
} PktOut;

typedef struct PacketQueueBase {
    PacketQueueNode end;
    struct IdempotentCallback *ic;
} PacketQueueBase;

typedef struct PktInQueue {
    PacketQueueBase pqb;
    PktIn *(*get)(PacketQueueBase *, int pop);
} PktInQueue;

typedef struct PktOutQueue {
    PacketQueueBase pqb;
    PktOut *(*get)(PacketQueueBase *, int pop);
} PktOutQueue;

void pq_base_push(PacketQueueBase *pqb, PacketQueueNode *node);
void pq_base_push_front(PacketQueueBase *pqb, PacketQueueNode *node);
void pq_base_concatenate(PacketQueueBase *dest,
                         PacketQueueBase *q1, PacketQueueBase *q2);

void pq_in_init(PktInQueue *pq);
void pq_out_init(PktOutQueue *pq);
void pq_in_clear(PktInQueue *pq);
void pq_out_clear(PktOutQueue *pq);

#define pq_push(pq, pkt)                                \
    TYPECHECK((pq)->get(&(pq)->pqb, FALSE) == pkt,      \
              pq_base_push(&(pq)->pqb, &(pkt)->qnode))
#define pq_push_front(pq, pkt)                                  \
    TYPECHECK((pq)->get(&(pq)->pqb, FALSE) == pkt,              \
              pq_base_push_front(&(pq)->pqb, &(pkt)->qnode))
#define pq_peek(pq) ((pq)->get(&(pq)->pqb, FALSE))
#define pq_pop(pq) ((pq)->get(&(pq)->pqb, TRUE))
#define pq_concatenate(dst, q1, q2)                                      \
    TYPECHECK((q1)->get(&(q1)->pqb, FALSE) ==                           \
              (dst)->get(&(dst)->pqb, FALSE) &&                         \
              (q2)->get(&(q2)->pqb, FALSE) ==                           \
              (dst)->get(&(dst)->pqb, FALSE),                           \
              pq_base_concatenate(&(dst)->pqb, &(q1)->pqb, &(q2)->pqb))

/*
 * Packet type contexts, so that ssh2_pkt_type can correctly decode
 * the ambiguous type numbers back into the correct type strings.
 */
typedef enum {
    SSH2_PKTCTX_NOKEX,
    SSH2_PKTCTX_DHGROUP,
    SSH2_PKTCTX_DHGEX,
    SSH2_PKTCTX_ECDHKEX,
    SSH2_PKTCTX_GSSKEX,
    SSH2_PKTCTX_RSAKEX
} Pkt_KCtx;
typedef enum {
    SSH2_PKTCTX_NOAUTH,
    SSH2_PKTCTX_PUBLICKEY,
    SSH2_PKTCTX_PASSWORD,
    SSH2_PKTCTX_GSSAPI,
    SSH2_PKTCTX_KBDINTER
} Pkt_ACtx;

typedef struct PacketLogSettings {
    int omit_passwords, omit_data;
    Pkt_KCtx kctx;
    Pkt_ACtx actx;
} PacketLogSettings;

#define MAX_BLANKS 4 /* no packet needs more censored sections than this */
int ssh1_censor_packet(
    const PacketLogSettings *pls, int type, int sender_is_client,
    ptrlen pkt, logblank_t *blanks);
int ssh2_censor_packet(
    const PacketLogSettings *pls, int type, int sender_is_client,
    ptrlen pkt, logblank_t *blanks);

PktOut *ssh_new_packet(void);
void ssh_free_pktout(PktOut *pkt);

extern Socket ssh_connection_sharing_init(
    const char *host, int port, Conf *conf, Frontend *frontend,
    Plug sshplug, ssh_sharing_state **state);
void ssh_connshare_provide_connlayer(ssh_sharing_state *sharestate,
                                     ConnectionLayer *cl);
int ssh_share_test_for_upstream(const char *host, int port, Conf *conf);
void share_got_pkt_from_server(ssh_sharing_connstate *ctx, int type,
                               const void *pkt, int pktlen);
void share_activate(ssh_sharing_state *sharestate,
                    const char *server_verstring);
void sharestate_free(ssh_sharing_state *state);
int share_ndownstreams(ssh_sharing_state *state);

void ssh_connshare_log(Ssh ssh, int event, const char *logtext,
                       const char *ds_err, const char *us_err);
void share_setup_x11_channel(ssh_sharing_connstate *cs, share_channel *chan,
                             unsigned upstream_id, unsigned server_id,
                             unsigned server_currwin, unsigned server_maxpkt,
                             unsigned client_adjusted_window,
                             const char *peer_addr, int peer_port, int endian,
                             int protomajor, int protominor,
                             const void *initial_data, int initial_len);

/* Structure definition centralised here because the SSH-1 and SSH-2
 * connection layers both use it. But the client module (portfwd.c)
 * should not try to look inside here. */
struct ssh_rportfwd {
    unsigned sport, dport;
    char *shost, *dhost;
    int addressfamily;
    char *log_description; /* name of remote listening port, for logging */
    ssh_sharing_connstate *share_ctx;
    PortFwdRecord *pfr;
};
void free_rportfwd(struct ssh_rportfwd *rpf);

struct ConnectionLayerVtable {
    /* Allocate and free remote-to-local port forwardings, called by
     * PortFwdManager or by connection sharing */
    struct ssh_rportfwd *(*rportfwd_alloc)(
        ConnectionLayer *cl,
        const char *shost, int sport, const char *dhost, int dport,
        int addressfamily, const char *log_description, PortFwdRecord *pfr,
        ssh_sharing_connstate *share_ctx);
    void (*rportfwd_remove)(ConnectionLayer *cl, struct ssh_rportfwd *rpf);

    /* Open a local-to-remote port forwarding channel, called by
     * PortFwdManager */
    SshChannel *(*lportfwd_open)(
        ConnectionLayer *cl, const char *hostname, int port,
        const char *org, Channel *chan);

    /* Add and remove X11 displays for connection sharing downstreams */
    struct X11FakeAuth *(*add_sharing_x11_display)(
        ConnectionLayer *cl, int authtype, ssh_sharing_connstate *share_cs,
        share_channel *share_chan);
    void (*remove_sharing_x11_display)(
        ConnectionLayer *cl, struct X11FakeAuth *auth);

    /* Pass through an outgoing SSH packet from a downstream */
    void (*send_packet_from_downstream)(
        ConnectionLayer *cl, unsigned id, int type,
        const void *pkt, int pktlen, const char *additional_log_text);

    /* Allocate/free an upstream channel number associated with a
     * sharing downstream */
    unsigned (*alloc_sharing_channel)(ConnectionLayer *cl,
                                      ssh_sharing_connstate *connstate);
    void (*delete_sharing_channel)(ConnectionLayer *cl, unsigned localid);

    /* Indicate that a downstream has sent a global request with the
     * want-reply flag, so that when a reply arrives it will be passed
     * back to that downstrean */
    void (*sharing_queue_global_request)(
        ConnectionLayer *cl, ssh_sharing_connstate *connstate);

    /* Indicate that the last downstream has disconnected */
    void (*sharing_no_more_downstreams)(ConnectionLayer *cl);

    /* Query whether the connection layer is doing agent forwarding */
    int (*agent_forwarding_permitted)(ConnectionLayer *cl);

    /* Set the size of the main terminal window (if any) */
    void (*terminal_size)(ConnectionLayer *cl, int width, int height);

    /* Indicate that the backlog on standard output has cleared */
    void (*stdout_unthrottle)(ConnectionLayer *cl, int bufsize);

    /* Query the size of the backlog on standard _input_ */
    int (*stdin_backlog)(ConnectionLayer *cl);

    /* Tell the connection layer that the SSH connection itself has
     * backed up, so it should tell all currently open channels to
     * cease reading from their local input sources if they can. (Or
     * tell it that that state of affairs has gone away again.) */
    void (*throttle_all_channels)(ConnectionLayer *cl, int throttled);

    /* Ask the connection layer about its current preference for
     * line-discipline options. */
    int (*ldisc_option)(ConnectionLayer *cl, int option);
};

struct ConnectionLayer {
    Frontend *frontend;
    const struct ConnectionLayerVtable *vt;
};

#define ssh_rportfwd_alloc(cl, sh, sp, dh, dp, af, ld, pfr, share) \
    ((cl)->vt->rportfwd_alloc(cl, sh, sp, dh, dp, af, ld, pfr, share))
#define ssh_rportfwd_remove(cl, rpf) ((cl)->vt->rportfwd_remove(cl, rpf))
#define ssh_lportfwd_open(cl, h, p, org, chan) \
    ((cl)->vt->lportfwd_open(cl, h, p, org, chan))
#define ssh_add_sharing_x11_display(cl, auth, cs, ch)   \
    ((cl)->vt->add_sharing_x11_display(cl, auth, cs, ch))
#define ssh_remove_sharing_x11_display(cl, fa)   \
    ((cl)->vt->remove_sharing_x11_display(cl, fa))
#define ssh_send_packet_from_downstream(cl, id, type, pkt, len, log)    \
    ((cl)->vt->send_packet_from_downstream(cl, id, type, pkt, len, log))
#define ssh_alloc_sharing_channel(cl, cs) \
    ((cl)->vt->alloc_sharing_channel(cl, cs))
#define ssh_delete_sharing_channel(cl, ch) \
    ((cl)->vt->delete_sharing_channel(cl, ch))
#define ssh_sharing_queue_global_request(cl, cs) \
    ((cl)->vt->sharing_queue_global_request(cl, cs))
#define ssh_sharing_no_more_downstreams(cl) \
    ((cl)->vt->sharing_no_more_downstreams(cl))
#define ssh_agent_forwarding_permitted(cl) \
    ((cl)->vt->agent_forwarding_permitted(cl))
#define ssh_terminal_size(cl, w, h) ((cl)->vt->terminal_size(cl, w, h))
#define ssh_stdout_unthrottle(cl, bufsize) \
    ((cl)->vt->stdout_unthrottle(cl, bufsize))
#define ssh_stdin_backlog(cl) ((cl)->vt->stdin_backlog(cl))
#define ssh_throttle_all_channels(cl, throttled) \
    ((cl)->vt->throttle_all_channels(cl, throttled))
#define ssh_ldisc_option(cl, option) ((cl)->vt->ldisc_option(cl, option))

/* Exports from portfwd.c */
PortFwdManager *portfwdmgr_new(ConnectionLayer *cl);
void portfwdmgr_free(PortFwdManager *mgr);
void portfwdmgr_config(PortFwdManager *mgr, Conf *conf);
void portfwdmgr_close(PortFwdManager *mgr, PortFwdRecord *pfr);
void portfwdmgr_close_all(PortFwdManager *mgr);
char *portfwdmgr_connect(PortFwdManager *mgr, Channel **chan_ret,
                         char *hostname, int port, SshChannel *c,
                         int addressfamily);

Frontend *ssh_get_frontend(Ssh ssh);

/* Communications back to ssh.c from connection layers */
void ssh_throttle_conn(Ssh ssh, int adjust);
void ssh_got_exitcode(Ssh ssh, int status);
void ssh_ldisc_update(Ssh ssh);
void ssh_got_fallback_cmd(Ssh ssh);

/* Functions to abort the connection, for various reasons. */
void ssh_remote_error(Ssh ssh, const char *fmt, ...);
void ssh_remote_eof(Ssh ssh, const char *fmt, ...);
void ssh_proto_error(Ssh ssh, const char *fmt, ...);
void ssh_sw_abort(Ssh ssh, const char *fmt, ...);
void ssh_user_close(Ssh ssh, const char *fmt, ...);

#define SSH_CIPHER_IDEA		1
#define SSH_CIPHER_DES		2
#define SSH_CIPHER_3DES		3
#define SSH_CIPHER_BLOWFISH	6

#ifndef BIGNUM_INTERNAL
typedef void *Bignum;
#endif

typedef struct ssh_keyalg ssh_keyalg;
typedef const struct ssh_keyalg *ssh_key;

struct RSAKey {
    int bits;
    int bytes;
    Bignum modulus;
    Bignum exponent;
    Bignum private_exponent;
    Bignum p;
    Bignum q;
    Bignum iqmp;
    char *comment;
    ssh_key sshk;
};

struct dss_key {
    Bignum p, q, g, y, x;
    ssh_key sshk;
};

struct ec_curve;

struct ec_point {
    const struct ec_curve *curve;
    Bignum x, y;
    Bignum z;  /* Jacobian denominator */
    unsigned char infinity;
};

void ec_point_free(struct ec_point *point);

/* Weierstrass form curve */
struct ec_wcurve
{
    Bignum a, b, n;
    struct ec_point G;
};

/* Montgomery form curve */
struct ec_mcurve
{
    Bignum a, b;
    struct ec_point G;
};

/* Edwards form curve */
struct ec_ecurve
{
    Bignum l, d;
    struct ec_point B;
};

struct ec_curve {
    enum { EC_WEIERSTRASS, EC_MONTGOMERY, EC_EDWARDS } type;
    /* 'name' is the identifier of the curve when it has to appear in
     * wire protocol encodings, as it does in e.g. the public key and
     * signature formats for NIST curves. Curves which do not format
     * their keys or signatures in this way just have name==NULL.
     *
     * 'textname' is non-NULL for all curves, and is a human-readable
     * identification suitable for putting in log messages. */
    const char *name, *textname;
    unsigned int fieldBits;
    Bignum p;
    union {
        struct ec_wcurve w;
        struct ec_mcurve m;
        struct ec_ecurve e;
    };
};

const ssh_keyalg *ec_alg_by_oid(int len, const void *oid,
                                        const struct ec_curve **curve);
const unsigned char *ec_alg_oid(const ssh_keyalg *alg, int *oidlen);
extern const int ec_nist_curve_lengths[], n_ec_nist_curve_lengths;
int ec_nist_alg_and_curve_by_bits(int bits,
                                  const struct ec_curve **curve,
                                  const ssh_keyalg **alg);
int ec_ed_alg_and_curve_by_bits(int bits,
                                const struct ec_curve **curve,
                                const ssh_keyalg **alg);

struct ec_key {
    struct ec_point publicKey;
    Bignum privateKey;
    ssh_key sshk;
};

struct ec_point *ec_public(const Bignum privateKey, const struct ec_curve *curve);

/*
 * SSH-1 never quite decided which order to store the two components
 * of an RSA key. During connection setup, the server sends its host
 * and server keys with the exponent first; private key files store
 * the modulus first. The agent protocol is even more confusing,
 * because the client specifies a key to the server in one order and
 * the server lists the keys it knows about in the other order!
 */
typedef enum { RSA_SSH1_EXPONENT_FIRST, RSA_SSH1_MODULUS_FIRST } RsaSsh1Order;

void BinarySource_get_rsa_ssh1_pub(
    BinarySource *src, struct RSAKey *result, RsaSsh1Order order);
void BinarySource_get_rsa_ssh1_priv(
    BinarySource *src, struct RSAKey *rsa);
int rsa_ssh1_encrypt(unsigned char *data, int length, struct RSAKey *key);
Bignum rsa_ssh1_decrypt(Bignum input, struct RSAKey *key);
void rsasanitise(struct RSAKey *key);
int rsastr_len(struct RSAKey *key);
void rsastr_fmt(char *str, struct RSAKey *key);
char *rsa_ssh1_fingerprint(struct RSAKey *key);
int rsa_verify(struct RSAKey *key);
void rsa_ssh1_public_blob(BinarySink *bs, struct RSAKey *key,
                          RsaSsh1Order order);
int rsa_ssh1_public_blob_len(void *data, int maxlen);
void freersakey(struct RSAKey *key);

typedef uint32 word32;

unsigned long crc32_compute(const void *s, size_t len);
unsigned long crc32_update(unsigned long crc_input, const void *s, size_t len);

/* SSH CRC compensation attack detector */
struct crcda_ctx;
struct crcda_ctx *crcda_make_context(void);
void crcda_free_context(struct crcda_ctx *ctx);
int detect_attack(struct crcda_ctx *ctx, unsigned char *buf, uint32 len,
		  unsigned char *IV);

/*
 * SSH2 RSA key exchange functions
 */
struct ssh_hashalg;
struct RSAKey *ssh_rsakex_newkey(const void *data, int len);
void ssh_rsakex_freekey(struct RSAKey *key);
int ssh_rsakex_klen(struct RSAKey *key);
void ssh_rsakex_encrypt(const struct ssh_hashalg *h,
                        unsigned char *in, int inlen,
                        unsigned char *out, int outlen, struct RSAKey *key);

/*
 * SSH2 ECDH key exchange functions
 */
struct ssh_kex;
const char *ssh_ecdhkex_curve_textname(const struct ssh_kex *kex);
struct ec_key *ssh_ecdhkex_newkey(const struct ssh_kex *kex);
void ssh_ecdhkex_freekey(struct ec_key *key);
void ssh_ecdhkex_getpublic(struct ec_key *key, BinarySink *bs);
Bignum ssh_ecdhkex_getkey(struct ec_key *key,
                          const void *remoteKey, int remoteKeyLen);

/*
 * Helper function for k generation in DSA, reused in ECDSA
 */
Bignum *dss_gen_k(const char *id_string, Bignum modulus, Bignum private_key,
                  unsigned char *digest, int digest_len);

struct ssh2_cipheralg;
typedef const struct ssh2_cipheralg *ssh2_cipher;

typedef struct {
    uint32 h[4];
} MD5_Core_State;

struct MD5Context {
    MD5_Core_State core;
    unsigned char block[64];
    int blkused;
    uint32 lenhi, lenlo;
    BinarySink_IMPLEMENTATION;
};

void MD5Init(struct MD5Context *context);
void MD5Final(unsigned char digest[16], struct MD5Context *context);
void MD5Simple(void const *p, unsigned len, unsigned char output[16]);

struct hmacmd5_context;
struct hmacmd5_context *hmacmd5_make_context(void);
void hmacmd5_free_context(struct hmacmd5_context *ctx);
void hmacmd5_key(struct hmacmd5_context *ctx, void const *key, int len);
void hmacmd5_do_hmac(struct hmacmd5_context *ctx,
                     const void *blk, int len, unsigned char *hmac);

int supports_sha_ni(void);

typedef struct SHA_State {
    uint32 h[5];
    unsigned char block[64];
    int blkused;
    uint32 lenhi, lenlo;
    void (*sha1)(struct SHA_State * s, const unsigned char *p, int len);
    BinarySink_IMPLEMENTATION;
} SHA_State;
void SHA_Init(SHA_State * s);
void SHA_Final(SHA_State * s, unsigned char *output);
void SHA_Simple(const void *p, int len, unsigned char *output);

void hmac_sha1_simple(const void *key, int keylen,
                      const void *data, int datalen,
		      unsigned char *output);
typedef struct SHA256_State {
    uint32 h[8];
    unsigned char block[64];
    int blkused;
    uint32 lenhi, lenlo;
    void (*sha256)(struct SHA256_State * s, const unsigned char *p, int len);
    BinarySink_IMPLEMENTATION;
} SHA256_State;
void SHA256_Init(SHA256_State * s);
void SHA256_Final(SHA256_State * s, unsigned char *output);
void SHA256_Simple(const void *p, int len, unsigned char *output);

typedef struct {
    uint64 h[8];
    unsigned char block[128];
    int blkused;
    uint32 len[4];
    BinarySink_IMPLEMENTATION;
} SHA512_State;
#define SHA384_State SHA512_State
void SHA512_Init(SHA512_State * s);
void SHA512_Final(SHA512_State * s, unsigned char *output);
void SHA512_Simple(const void *p, int len, unsigned char *output);
void SHA384_Init(SHA384_State * s);
void SHA384_Final(SHA384_State * s, unsigned char *output);
void SHA384_Simple(const void *p, int len, unsigned char *output);

struct ssh2_macalg;

struct ssh1_cipheralg;
typedef const struct ssh1_cipheralg *ssh1_cipher;

struct ssh1_cipheralg {
    ssh1_cipher *(*new)(void);
    void (*free)(ssh1_cipher *);
    void (*sesskey)(ssh1_cipher *, const void *key);
    void (*encrypt)(ssh1_cipher *, void *blk, int len);
    void (*decrypt)(ssh1_cipher *, void *blk, int len);
    int blksize;
    const char *text_name;
};

#define ssh1_cipher_new(alg) ((alg)->new())
#define ssh1_cipher_free(ctx) ((*(ctx))->free(ctx))
#define ssh1_cipher_sesskey(ctx, key) ((*(ctx))->sesskey(ctx, key))
#define ssh1_cipher_encrypt(ctx, blk, len) ((*(ctx))->encrypt(ctx, blk, len))
#define ssh1_cipher_decrypt(ctx, blk, len) ((*(ctx))->decrypt(ctx, blk, len))

struct ssh2_cipheralg {
    ssh2_cipher *(*new)(const struct ssh2_cipheralg *alg);
    void (*free)(ssh2_cipher *);
    void (*setiv)(ssh2_cipher *, const void *iv);
    void (*setkey)(ssh2_cipher *, const void *key);
    void (*encrypt)(ssh2_cipher *, void *blk, int len);
    void (*decrypt)(ssh2_cipher *, void *blk, int len);
    /* Ignored unless SSH_CIPHER_SEPARATE_LENGTH flag set */
    void (*encrypt_length)(ssh2_cipher *, void *blk, int len,
                           unsigned long seq);
    void (*decrypt_length)(ssh2_cipher *, void *blk, int len,
                           unsigned long seq);
    const char *name;
    int blksize;
    /* real_keybits is the number of bits of entropy genuinely used by
     * the cipher scheme; it's used for deciding how big a
     * Diffie-Hellman group is needed to exchange a key for the
     * cipher. */
    int real_keybits;
    /* padded_keybytes is the number of bytes of key data expected as
     * input to the setkey function; it's used for deciding how much
     * data needs to be generated from the post-kex generation of key
     * material. In a sensible cipher which uses all its key bytes for
     * real work, this will just be real_keybits/8, but in DES-type
     * ciphers which ignore one bit in each byte, it'll be slightly
     * different. */
    int padded_keybytes;
    unsigned int flags;
#define SSH_CIPHER_IS_CBC	1
#define SSH_CIPHER_SEPARATE_LENGTH      2
    const char *text_name;
    /* If set, this takes priority over other MAC. */
    const struct ssh2_macalg *required_mac;
};

#define ssh2_cipher_new(alg) ((alg)->new(alg))
#define ssh2_cipher_free(ctx) ((*(ctx))->free(ctx))
#define ssh2_cipher_setiv(ctx, iv) ((*(ctx))->setiv(ctx, iv))
#define ssh2_cipher_setkey(ctx, key) ((*(ctx))->setkey(ctx, key))
#define ssh2_cipher_encrypt(ctx, blk, len) ((*(ctx))->encrypt(ctx, blk, len))
#define ssh2_cipher_decrypt(ctx, blk, len) ((*(ctx))->decrypt(ctx, blk, len))
#define ssh2_cipher_encrypt_length(ctx, blk, len, seq) \
    ((*(ctx))->encrypt_length(ctx, blk, len, seq))
#define ssh2_cipher_decrypt_length(ctx, blk, len, seq) \
    ((*(ctx))->decrypt_length(ctx, blk, len, seq))
#define ssh2_cipher_alg(ctx) (*(ctx))

struct ssh2_ciphers {
    int nciphers;
    const struct ssh2_cipheralg *const *list;
};

struct ssh2_macalg;
typedef struct ssh2_mac {
    const struct ssh2_macalg *vt;
    BinarySink_DELEGATE_IMPLEMENTATION;
} ssh2_mac;

struct ssh2_macalg {
    /* Passes in the cipher context */
    ssh2_mac *(*new)(const struct ssh2_macalg *alg, ssh2_cipher *cipher);
    void (*free)(ssh2_mac *);
    void (*setkey)(ssh2_mac *, const void *key);
    void (*start)(ssh2_mac *);
    void (*genresult)(ssh2_mac *, unsigned char *);
    const char *name, *etm_name;
    int len, keylen;
    const char *text_name;
};

#define ssh2_mac_new(alg, cipher) ((alg)->new(alg, cipher))
#define ssh2_mac_free(ctx) ((ctx)->vt->free(ctx))
#define ssh2_mac_setkey(ctx, key) ((ctx)->vt->free(ctx, key))
#define ssh2_mac_start(ctx) ((ctx)->vt->start(ctx))
#define ssh2_mac_genresult(ctx, out) ((ctx)->vt->genresult(ctx, out))
#define ssh2_mac_alg(ctx) ((ctx)->vt)

/* Centralised 'methods' for ssh2_mac, defined in sshmac.c */
int ssh2_mac_verresult(ssh2_mac *, const void *);
void ssh2_mac_generate(ssh2_mac *, void *, int, unsigned long seq);
int ssh2_mac_verify(ssh2_mac *, const void *, int, unsigned long seq);

typedef struct ssh_hash {
    const struct ssh_hashalg *vt;
    BinarySink_DELEGATE_IMPLEMENTATION;
} ssh_hash;

struct ssh_hashalg {
    ssh_hash *(*new)(const struct ssh_hashalg *alg);
    ssh_hash *(*copy)(ssh_hash *);
    void (*final)(ssh_hash *, unsigned char *); /* ALSO FREES THE ssh_hash! */
    void (*free)(ssh_hash *);
    int hlen; /* output length in bytes */
    const char *text_name;
};   

#define ssh_hash_new(alg) ((alg)->new(alg))
#define ssh_hash_copy(ctx) ((ctx)->vt->copy(ctx))
#define ssh_hash_final(ctx, out) ((ctx)->vt->final(ctx, out))
#define ssh_hash_free(ctx) ((ctx)->vt->free(ctx))
#define ssh_hash_alg(ctx) ((ctx)->vt)

struct ssh_kex {
    const char *name, *groupname;
    enum { KEXTYPE_DH, KEXTYPE_RSA, KEXTYPE_ECDH, KEXTYPE_GSS } main_type;
    const struct ssh_hashalg *hash;
    const void *extra;                 /* private to the kex methods */
};

struct ssh_kexes {
    int nkexes;
    const struct ssh_kex *const *list;
};

struct ssh_keyalg {
    /* Constructors that create an ssh_key */
    ssh_key *(*new_pub) (const ssh_keyalg *self, ptrlen pub);
    ssh_key *(*new_priv) (const ssh_keyalg *self, ptrlen pub, ptrlen priv);
    ssh_key *(*new_priv_openssh) (const ssh_keyalg *self, BinarySource *);

    /* Methods that operate on an existing ssh_key */
    void (*freekey) (ssh_key *key);
    void (*sign) (ssh_key *key, const void *data, int datalen, BinarySink *);
    int (*verify) (ssh_key *key, ptrlen sig, ptrlen data);
    void (*public_blob)(ssh_key *key, BinarySink *);
    void (*private_blob)(ssh_key *key, BinarySink *);
    void (*openssh_blob) (ssh_key *key, BinarySink *);
    char *(*cache_str) (ssh_key *key);

    /* 'Class methods' that don't deal with an ssh_key at all */
    int (*pubkey_bits) (const ssh_keyalg *self, ptrlen blob);

    /* Constant data fields giving information about the key type */
    const char *ssh_id;    /* string identifier in the SSH protocol */
    const char *cache_id;  /* identifier used in PuTTY's host key cache */
    const void *extra;     /* private to the public key methods */
};

#define ssh_key_new_pub(alg, data) ((alg)->new_pub(alg, data))
#define ssh_key_new_priv(alg, pub, priv) ((alg)->new_priv(alg, pub, priv))
#define ssh_key_new_priv_openssh(alg, bs) ((alg)->new_priv_openssh(alg, bs))

#define ssh_key_free(key) ((*(key))->freekey(key))
#define ssh_key_sign(key, data, len, bs) ((*(key))->sign(key, data, len, bs))
#define ssh_key_verify(key, sig, data) ((*(key))->verify(key, sig, data))
#define ssh_key_public_blob(key, bs) ((*(key))->public_blob(key, bs))
#define ssh_key_private_blob(key, bs) ((*(key))->private_blob(key, bs))
#define ssh_key_openssh_blob(key, bs) ((*(key))->openssh_blob(key, bs))
#define ssh_key_cache_str(key) ((*(key))->cache_str(key))

#define ssh_key_public_bits(alg, blob) ((alg)->pubkey_bits(alg, blob))

#define ssh_key_alg(key) (*(key))
#define ssh_key_ssh_id(key) ((*(key))->ssh_id)
#define ssh_key_cache_id(key) ((*(key))->cache_id)

typedef struct ssh_compressor {
    const struct ssh_compression_alg *vt;
} ssh_compressor;
typedef struct ssh_decompressor {
    const struct ssh_compression_alg *vt;
} ssh_decompressor;

struct ssh_compression_alg {
    const char *name;
    /* For zlib@openssh.com: if non-NULL, this name will be considered once
     * userauth has completed successfully. */
    const char *delayed_name;
    ssh_compressor *(*compress_new)(void);
    void (*compress_free)(ssh_compressor *);
    void (*compress)(ssh_compressor *, unsigned char *block, int len,
                     unsigned char **outblock, int *outlen,
                     int minlen);
    ssh_decompressor *(*decompress_new)(void);
    void (*decompress_free)(ssh_decompressor *);
    int (*decompress)(ssh_decompressor *, unsigned char *block, int len,
                      unsigned char **outblock, int *outlen);
    const char *text_name;
};

#define ssh_compressor_new(alg) ((alg)->compress_new())
#define ssh_compressor_free(comp) ((comp)->vt->compress_free(comp))
#define ssh_compressor_compress(comp, in, inlen, out, outlen, minlen) \
    ((comp)->vt->compress(comp, in, inlen, out, outlen, minlen))
#define ssh_decompressor_new(alg) ((alg)->decompress_new())
#define ssh_decompressor_free(comp) ((comp)->vt->decompress_free(comp))
#define ssh_decompressor_decompress(comp, in, inlen, out, outlen) \
    ((comp)->vt->decompress(comp, in, inlen, out, outlen))

struct ssh2_userkey {
    ssh_key *key;                      /* the key itself */
    char *comment;		       /* the key comment */
};

/* The maximum length of any hash algorithm used in kex. (bytes) */
#define SSH2_KEX_MAX_HASH_LEN (64) /* SHA-512 */

extern const struct ssh1_cipheralg ssh1_3des;
extern const struct ssh1_cipheralg ssh1_des;
extern const struct ssh1_cipheralg ssh1_blowfish;
extern const struct ssh2_ciphers ssh2_3des;
extern const struct ssh2_ciphers ssh2_des;
extern const struct ssh2_ciphers ssh2_aes;
extern const struct ssh2_ciphers ssh2_blowfish;
extern const struct ssh2_ciphers ssh2_arcfour;
extern const struct ssh2_ciphers ssh2_ccp;
extern const struct ssh_hashalg ssh_sha1;
extern const struct ssh_hashalg ssh_sha256;
extern const struct ssh_hashalg ssh_sha384;
extern const struct ssh_hashalg ssh_sha512;
extern const struct ssh_kexes ssh_diffiehellman_group1;
extern const struct ssh_kexes ssh_diffiehellman_group14;
extern const struct ssh_kexes ssh_diffiehellman_gex;
extern const struct ssh_kexes ssh_gssk5_sha1_kex;
extern const struct ssh_kexes ssh_rsa_kex;
extern const struct ssh_kexes ssh_ecdh_kex;
extern const ssh_keyalg ssh_dss;
extern const ssh_keyalg ssh_rsa;
extern const ssh_keyalg ssh_ecdsa_ed25519;
extern const ssh_keyalg ssh_ecdsa_nistp256;
extern const ssh_keyalg ssh_ecdsa_nistp384;
extern const ssh_keyalg ssh_ecdsa_nistp521;
extern const struct ssh2_macalg ssh_hmac_md5;
extern const struct ssh2_macalg ssh_hmac_sha1;
extern const struct ssh2_macalg ssh_hmac_sha1_buggy;
extern const struct ssh2_macalg ssh_hmac_sha1_96;
extern const struct ssh2_macalg ssh_hmac_sha1_96_buggy;
extern const struct ssh2_macalg ssh_hmac_sha256;
extern const struct ssh_compression_alg ssh_zlib;

typedef struct AESContext AESContext;
AESContext *aes_make_context(void);
void aes_free_context(AESContext *ctx);
void aes128_key(AESContext *ctx, const void *key);
void aes192_key(AESContext *ctx, const void *key);
void aes256_key(AESContext *ctx, const void *key);
void aes_iv(AESContext *ctx, const void *iv);
void aes_ssh2_encrypt_blk(AESContext *ctx, void *blk, int len);
void aes_ssh2_decrypt_blk(AESContext *ctx, void *blk, int len);
void aes_ssh2_sdctr(AESContext *ctx, void *blk, int len);

/*
 * PuTTY version number formatted as an SSH version string. 
 */
extern const char sshver[];

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
extern int ssh_fallback_cmd(Backend *backend);

void SHATransform(word32 * digest, word32 * data);

/*
 * Check of compiler version
 */
#ifdef _FORCE_SHA_NI
#   define COMPILER_SUPPORTS_SHA_NI
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<shaintrin.h>) && (defined(__x86_64__) || defined(__i386))
#       define COMPILER_SUPPORTS_SHA_NI
#   endif
#elif defined(__GNUC__)
#    if ((__GNUC__ >= 5) && (defined(__x86_64__) || defined(__i386)))
#       define COMPILER_SUPPORTS_SHA_NI
#    endif
#elif defined (_MSC_VER)
#   if (defined(_M_X64) || defined(_M_IX86)) && _MSC_VER >= 1900
#      define COMPILER_SUPPORTS_SHA_NI
#   endif
#endif

#ifdef _FORCE_SOFTWARE_SHA
#   undef COMPILER_SUPPORTS_SHA_NI
#endif

int random_byte(void);
void random_add_noise(void *noise, int length);
void random_add_heavynoise(void *noise, int length);

/* Exports from x11fwd.c */
enum {
    X11_TRANS_IPV4 = 0, X11_TRANS_IPV6 = 6, X11_TRANS_UNIX = 256
};
struct X11Display {
    /* Broken-down components of the display name itself */
    int unixdomain;
    char *hostname;
    int displaynum;
    int screennum;
    /* OSX sometimes replaces all the above with a full Unix-socket pathname */
    char *unixsocketpath;

    /* PuTTY networking SockAddr to connect to the display, and associated
     * gubbins */
    SockAddr addr;
    int port;
    char *realhost;

    /* Our local auth details for talking to the real X display. */
    int localauthproto;
    unsigned char *localauthdata;
    int localauthdatalen;
};
struct X11FakeAuth {
    /* Auth details we invented for a virtual display on the SSH server. */
    int proto;
    unsigned char *data;
    int datalen;
    char *protoname;
    char *datastring;

    /* The encrypted form of the first block, in XDM-AUTHORIZATION-1.
     * Used as part of the key when these structures are organised
     * into a tree. See x11_invent_fake_auth for explanation. */
    unsigned char *xa1_firstblock;

    /*
     * Used inside x11fwd.c to remember recently seen
     * XDM-AUTHORIZATION-1 strings, to avoid replay attacks.
     */
    tree234 *xdmseen;

    /*
     * What to do with an X connection matching this auth data.
     */
    struct X11Display *disp;
    ssh_sharing_connstate *share_cs;
    share_channel *share_chan;
};
void *x11_make_greeting(int endian, int protomajor, int protominor,
                        int auth_proto, const void *auth_data, int auth_len,
                        const char *peer_ip, int peer_port,
                        int *outlen);
int x11_authcmp(void *av, void *bv); /* for putting X11FakeAuth in a tree234 */
/*
 * x11_setup_display() parses the display variable and fills in an
 * X11Display structure. Some remote auth details are invented;
 * the supplied authtype parameter configures the preferred
 * authorisation protocol to use at the remote end. The local auth
 * details are looked up by calling platform_get_x11_auth.
 */
extern struct X11Display *x11_setup_display(const char *display, Conf *);
void x11_free_display(struct X11Display *disp);
struct X11FakeAuth *x11_invent_fake_auth(tree234 *t, int authtype);
void x11_free_fake_auth(struct X11FakeAuth *auth);
Channel *x11_new_channel(tree234 *authtree, SshChannel *c,
                         const char *peeraddr, int peerport,
                         int connection_sharing_possible);
char *x11_display(const char *display);
/* Platform-dependent X11 functions */
extern void platform_get_x11_auth(struct X11Display *display, Conf *);
    /* examine a mostly-filled-in X11Display and fill in localauth* */
extern const int platform_uses_x11_unix_by_default;
    /* choose default X transport in the absence of a specified one */
SockAddr platform_get_x11_unix_address(const char *path, int displaynum);
    /* make up a SockAddr naming the address for displaynum */
char *platform_get_x_display(void);
    /* allocated local X display string, if any */
/* Callbacks in x11.c usable _by_ platform X11 functions */
/*
 * This function does the job of platform_get_x11_auth, provided
 * it is told where to find a normally formatted .Xauthority file:
 * it opens that file, parses it to find an auth record which
 * matches the display details in "display", and fills in the
 * localauth fields.
 *
 * It is expected that most implementations of
 * platform_get_x11_auth() will work by finding their system's
 * .Xauthority file, adjusting the display details if necessary
 * for local oddities like Unix-domain socket transport, and
 * calling this function to do the rest of the work.
 */
void x11_get_auth_from_authfile(struct X11Display *display,
				const char *authfilename);
int x11_identify_auth_proto(ptrlen protoname);
void *x11_dehexify(ptrlen hex, int *outlen);

Channel *agentf_new(SshChannel *c);

Bignum copybn(Bignum b);
Bignum bn_power_2(int n);
void bn_restore_invariant(Bignum b);
Bignum bignum_from_long(unsigned long n);
void freebn(Bignum b);
Bignum modpow(Bignum base, Bignum exp, Bignum mod);
Bignum modmul(Bignum a, Bignum b, Bignum mod);
Bignum modsub(const Bignum a, const Bignum b, const Bignum n);
void decbn(Bignum n);
extern Bignum Zero, One;
Bignum bignum_from_bytes(const void *data, int nbytes);
Bignum bignum_from_bytes_le(const void *data, int nbytes);
Bignum bignum_random_in_range(const Bignum lower, const Bignum upper);
int bignum_bitcount(Bignum bn);
int bignum_byte(Bignum bn, int i);
int bignum_bit(Bignum bn, int i);
void bignum_set_bit(Bignum bn, int i, int value);
Bignum biggcd(Bignum a, Bignum b);
unsigned short bignum_mod_short(Bignum number, unsigned short modulus);
Bignum bignum_add_long(Bignum number, unsigned long addend);
Bignum bigadd(Bignum a, Bignum b);
Bignum bigsub(Bignum a, Bignum b);
Bignum bigmul(Bignum a, Bignum b);
Bignum bigmuladd(Bignum a, Bignum b, Bignum addend);
Bignum bigdiv(Bignum a, Bignum b);
Bignum bigmod(Bignum a, Bignum b);
Bignum modinv(Bignum number, Bignum modulus);
Bignum bignum_bitmask(Bignum number);
Bignum bignum_rshift(Bignum number, int shift);
Bignum bignum_lshift(Bignum number, int shift);
int bignum_cmp(Bignum a, Bignum b);
char *bignum_decimal(Bignum x);
Bignum bignum_from_decimal(const char *decimal);

void BinarySink_put_mp_ssh1(BinarySink *, Bignum);
void BinarySink_put_mp_ssh2(BinarySink *, Bignum);
Bignum BinarySource_get_mp_ssh1(BinarySource *);
Bignum BinarySource_get_mp_ssh2(BinarySource *);

#ifdef DEBUG
void diagbn(char *prefix, Bignum md);
#endif

int dh_is_gex(const struct ssh_kex *kex);
struct dh_ctx;
struct dh_ctx *dh_setup_group(const struct ssh_kex *kex);
struct dh_ctx *dh_setup_gex(Bignum pval, Bignum gval);
void dh_cleanup(struct dh_ctx *);
Bignum dh_create_e(struct dh_ctx *, int nbits);
const char *dh_validate_f(struct dh_ctx *, Bignum f);
Bignum dh_find_K(struct dh_ctx *, Bignum f);

int rsa_ssh1_encrypted(const Filename *filename, char **comment);
int rsa_ssh1_loadpub(const Filename *filename, BinarySink *bs,
                     char **commentptr, const char **errorstr);
int rsa_ssh1_loadkey(const Filename *filename, struct RSAKey *key,
                     const char *passphrase, const char **errorstr);
int rsa_ssh1_savekey(const Filename *filename, struct RSAKey *key,
                     char *passphrase);

extern int base64_decode_atom(const char *atom, unsigned char *out);
extern int base64_lines(int datalen);
extern void base64_encode_atom(const unsigned char *data, int n, char *out);
extern void base64_encode(FILE *fp, const unsigned char *data, int datalen,
                          int cpl);

/* ssh2_load_userkey can return this as an error */
extern struct ssh2_userkey ssh2_wrong_passphrase;
#define SSH2_WRONG_PASSPHRASE (&ssh2_wrong_passphrase)

int ssh2_userkey_encrypted(const Filename *filename, char **comment);
struct ssh2_userkey *ssh2_load_userkey(const Filename *filename,
				       const char *passphrase,
                                       const char **errorstr);
int ssh2_userkey_loadpub(const Filename *filename, char **algorithm,
                         BinarySink *bs,
                         char **commentptr, const char **errorstr);
int ssh2_save_userkey(const Filename *filename, struct ssh2_userkey *key,
		      char *passphrase);
const ssh_keyalg *find_pubkey_alg(const char *name);
const ssh_keyalg *find_pubkey_alg_len(ptrlen name);

enum {
    SSH_KEYTYPE_UNOPENABLE,
    SSH_KEYTYPE_UNKNOWN,
    SSH_KEYTYPE_SSH1, SSH_KEYTYPE_SSH2,
    /*
     * The OpenSSH key types deserve a little explanation. OpenSSH has
     * two physical formats for private key storage: an old PEM-based
     * one largely dictated by their use of OpenSSL and full of ASN.1,
     * and a new one using the same private key formats used over the
     * wire for talking to ssh-agent. The old format can only support
     * a subset of the key types, because it needs redesign for each
     * key type, and after a while they decided to move to the new
     * format so as not to have to do that.
     *
     * On input, key files are identified as either
     * SSH_KEYTYPE_OPENSSH_PEM or SSH_KEYTYPE_OPENSSH_NEW, describing
     * accurately which actual format the keys are stored in.
     *
     * On output, however, we default to following OpenSSH's own
     * policy of writing out PEM-style keys for maximum backwards
     * compatibility if the key type supports it, and otherwise
     * switching to the new format. So the formats you can select for
     * output are SSH_KEYTYPE_OPENSSH_NEW (forcing the new format for
     * any key type), and SSH_KEYTYPE_OPENSSH_AUTO to use the oldest
     * format supported by whatever key type you're writing out.
     *
     * So we have three type codes, but only two of them usable in any
     * given circumstance. An input key file will never be identified
     * as AUTO, only PEM or NEW; key export UIs should not be able to
     * select PEM, only AUTO or NEW.
     */
    SSH_KEYTYPE_OPENSSH_AUTO,
    SSH_KEYTYPE_OPENSSH_PEM,
    SSH_KEYTYPE_OPENSSH_NEW,
    SSH_KEYTYPE_SSHCOM,
    /*
     * Public-key-only formats, which we still want to be able to read
     * for various purposes.
     */
    SSH_KEYTYPE_SSH1_PUBLIC,
    SSH_KEYTYPE_SSH2_PUBLIC_RFC4716,
    SSH_KEYTYPE_SSH2_PUBLIC_OPENSSH
};
char *ssh1_pubkey_str(struct RSAKey *ssh1key);
void ssh1_write_pubkey(FILE *fp, struct RSAKey *ssh1key);
char *ssh2_pubkey_openssh_str(struct ssh2_userkey *key);
void ssh2_write_pubkey(FILE *fp, const char *comment,
                       const void *v_pub_blob, int pub_len,
                       int keytype);
char *ssh2_fingerprint_blob(const void *blob, int bloblen);
char *ssh2_fingerprint(ssh_key *key);
int key_type(const Filename *filename);
const char *key_type_to_str(int type);

int import_possible(int type);
int import_target_type(int type);
int import_encrypted(const Filename *filename, int type, char **comment);
int import_ssh1(const Filename *filename, int type,
		struct RSAKey *key, char *passphrase, const char **errmsg_p);
struct ssh2_userkey *import_ssh2(const Filename *filename, int type,
				 char *passphrase, const char **errmsg_p);
int export_ssh1(const Filename *filename, int type,
		struct RSAKey *key, char *passphrase);
int export_ssh2(const Filename *filename, int type,
                struct ssh2_userkey *key, char *passphrase);

void des3_decrypt_pubkey(const void *key, void *blk, int len);
void des3_encrypt_pubkey(const void *key, void *blk, int len);
void des3_decrypt_pubkey_ossh(const void *key, const void *iv,
			      void *blk, int len);
void des3_encrypt_pubkey_ossh(const void *key, const void *iv,
			      void *blk, int len);
void aes256_encrypt_pubkey(const void *key, void *blk, int len);
void aes256_decrypt_pubkey(const void *key, void *blk, int len);

void des_encrypt_xdmauth(const void *key, void *blk, int len);
void des_decrypt_xdmauth(const void *key, void *blk, int len);

void openssh_bcrypt(const char *passphrase,
                    const unsigned char *salt, int saltbytes,
                    int rounds, unsigned char *out, int outbytes);

/*
 * For progress updates in the key generation utility.
 */
#define PROGFN_INITIALISE 1
#define PROGFN_LIN_PHASE 2
#define PROGFN_EXP_PHASE 3
#define PROGFN_PHASE_EXTENT 4
#define PROGFN_READY 5
#define PROGFN_PROGRESS 6
typedef void (*progfn_t) (void *param, int action, int phase, int progress);

int rsa_generate(struct RSAKey *key, int bits, progfn_t pfn,
		 void *pfnparam);
int dsa_generate(struct dss_key *key, int bits, progfn_t pfn,
		 void *pfnparam);
int ec_generate(struct ec_key *key, int bits, progfn_t pfn,
                void *pfnparam);
int ec_edgenerate(struct ec_key *key, int bits, progfn_t pfn,
                  void *pfnparam);
Bignum primegen(int bits, int modulus, int residue, Bignum factor,
		int phase, progfn_t pfn, void *pfnparam, unsigned firstbits);
void invent_firstbits(unsigned *one, unsigned *two);

/*
 * Connection-sharing API provided by platforms. This function must
 * either:
 *  - return SHARE_NONE and do nothing
 *  - return SHARE_DOWNSTREAM and set *sock to a Socket connected to
 *    downplug
 *  - return SHARE_UPSTREAM and set *sock to a Socket connected to
 *    upplug.
 */
enum { SHARE_NONE, SHARE_DOWNSTREAM, SHARE_UPSTREAM };
int platform_ssh_share(const char *name, Conf *conf,
                       Plug downplug, Plug upplug, Socket *sock,
                       char **logtext, char **ds_err, char **us_err,
                       int can_upstream, int can_downstream);
void platform_ssh_share_cleanup(const char *name);

/*
 * List macro defining the SSH-1 message type codes.
 */
#define SSH1_MESSAGE_TYPES(X, y)                        \
    X(y, SSH1_MSG_DISCONNECT, 1)                        \
    X(y, SSH1_SMSG_PUBLIC_KEY, 2)                       \
    X(y, SSH1_CMSG_SESSION_KEY, 3)                      \
    X(y, SSH1_CMSG_USER, 4)                             \
    X(y, SSH1_CMSG_AUTH_RSA, 6)                         \
    X(y, SSH1_SMSG_AUTH_RSA_CHALLENGE, 7)               \
    X(y, SSH1_CMSG_AUTH_RSA_RESPONSE, 8)                \
    X(y, SSH1_CMSG_AUTH_PASSWORD, 9)                    \
    X(y, SSH1_CMSG_REQUEST_PTY, 10)                     \
    X(y, SSH1_CMSG_WINDOW_SIZE, 11)                     \
    X(y, SSH1_CMSG_EXEC_SHELL, 12)                      \
    X(y, SSH1_CMSG_EXEC_CMD, 13)                        \
    X(y, SSH1_SMSG_SUCCESS, 14)                         \
    X(y, SSH1_SMSG_FAILURE, 15)                         \
    X(y, SSH1_CMSG_STDIN_DATA, 16)                      \
    X(y, SSH1_SMSG_STDOUT_DATA, 17)                     \
    X(y, SSH1_SMSG_STDERR_DATA, 18)                     \
    X(y, SSH1_CMSG_EOF, 19)                             \
    X(y, SSH1_SMSG_EXIT_STATUS, 20)                     \
    X(y, SSH1_MSG_CHANNEL_OPEN_CONFIRMATION, 21)        \
    X(y, SSH1_MSG_CHANNEL_OPEN_FAILURE, 22)             \
    X(y, SSH1_MSG_CHANNEL_DATA, 23)                     \
    X(y, SSH1_MSG_CHANNEL_CLOSE, 24)                    \
    X(y, SSH1_MSG_CHANNEL_CLOSE_CONFIRMATION, 25)       \
    X(y, SSH1_SMSG_X11_OPEN, 27)                        \
    X(y, SSH1_CMSG_PORT_FORWARD_REQUEST, 28)            \
    X(y, SSH1_MSG_PORT_OPEN, 29)                        \
    X(y, SSH1_CMSG_AGENT_REQUEST_FORWARDING, 30)        \
    X(y, SSH1_SMSG_AGENT_OPEN, 31)                      \
    X(y, SSH1_MSG_IGNORE, 32)                           \
    X(y, SSH1_CMSG_EXIT_CONFIRMATION, 33)               \
    X(y, SSH1_CMSG_X11_REQUEST_FORWARDING, 34)          \
    X(y, SSH1_CMSG_AUTH_RHOSTS_RSA, 35)                 \
    X(y, SSH1_MSG_DEBUG, 36)                            \
    X(y, SSH1_CMSG_REQUEST_COMPRESSION, 37)             \
    X(y, SSH1_CMSG_AUTH_TIS, 39)                        \
    X(y, SSH1_SMSG_AUTH_TIS_CHALLENGE, 40)              \
    X(y, SSH1_CMSG_AUTH_TIS_RESPONSE, 41)               \
    X(y, SSH1_CMSG_AUTH_CCARD, 70)                      \
    X(y, SSH1_SMSG_AUTH_CCARD_CHALLENGE, 71)            \
    X(y, SSH1_CMSG_AUTH_CCARD_RESPONSE, 72)             \
    /* end of list */

#define SSH1_AUTH_RHOSTS                          1	/* 0x1 */
#define SSH1_AUTH_RSA                             2	/* 0x2 */
#define SSH1_AUTH_PASSWORD                        3	/* 0x3 */
#define SSH1_AUTH_RHOSTS_RSA                      4	/* 0x4 */
#define SSH1_AUTH_TIS                             5	/* 0x5 */
#define SSH1_AUTH_CCARD                           16	/* 0x10 */

#define SSH1_PROTOFLAG_SCREEN_NUMBER              1	/* 0x1 */
/* Mask for protoflags we will echo back to server if seen */
#define SSH1_PROTOFLAGS_SUPPORTED                 0	/* 0x1 */

/*
 * List macro defining SSH-2 message type codes. Some of these depend
 * on particular contexts (i.e. a previously negotiated kex or auth
 * method)
 */
#define SSH2_MESSAGE_TYPES(X, K, A, y)                                  \
    X(y, SSH2_MSG_DISCONNECT, 1)                                        \
    X(y, SSH2_MSG_IGNORE, 2)                                            \
    X(y, SSH2_MSG_UNIMPLEMENTED, 3)                                     \
    X(y, SSH2_MSG_DEBUG, 4)                                             \
    X(y, SSH2_MSG_SERVICE_REQUEST, 5)                                   \
    X(y, SSH2_MSG_SERVICE_ACCEPT, 6)                                    \
    X(y, SSH2_MSG_KEXINIT, 20)                                          \
    X(y, SSH2_MSG_NEWKEYS, 21)                                          \
    K(y, SSH2_MSG_KEXDH_INIT, 30, SSH2_PKTCTX_DHGROUP)                  \
    K(y, SSH2_MSG_KEXDH_REPLY, 31, SSH2_PKTCTX_DHGROUP)                 \
    K(y, SSH2_MSG_KEX_DH_GEX_REQUEST_OLD, 30, SSH2_PKTCTX_DHGEX)        \
    K(y, SSH2_MSG_KEX_DH_GEX_REQUEST, 34, SSH2_PKTCTX_DHGEX)            \
    K(y, SSH2_MSG_KEX_DH_GEX_GROUP, 31, SSH2_PKTCTX_DHGEX)              \
    K(y, SSH2_MSG_KEX_DH_GEX_INIT, 32, SSH2_PKTCTX_DHGEX)               \
    K(y, SSH2_MSG_KEX_DH_GEX_REPLY, 33, SSH2_PKTCTX_DHGEX)              \
    K(y, SSH2_MSG_KEXGSS_INIT, 30, SSH2_PKTCTX_GSSKEX)                  \
    K(y, SSH2_MSG_KEXGSS_CONTINUE, 31, SSH2_PKTCTX_GSSKEX)              \
    K(y, SSH2_MSG_KEXGSS_COMPLETE, 32, SSH2_PKTCTX_GSSKEX)              \
    K(y, SSH2_MSG_KEXGSS_HOSTKEY, 33, SSH2_PKTCTX_GSSKEX)               \
    K(y, SSH2_MSG_KEXGSS_ERROR, 34, SSH2_PKTCTX_GSSKEX)                 \
    K(y, SSH2_MSG_KEXGSS_GROUPREQ, 40, SSH2_PKTCTX_GSSKEX)              \
    K(y, SSH2_MSG_KEXGSS_GROUP, 41, SSH2_PKTCTX_GSSKEX)                 \
    K(y, SSH2_MSG_KEXRSA_PUBKEY, 30, SSH2_PKTCTX_RSAKEX)                \
    K(y, SSH2_MSG_KEXRSA_SECRET, 31, SSH2_PKTCTX_RSAKEX)                \
    K(y, SSH2_MSG_KEXRSA_DONE, 32, SSH2_PKTCTX_RSAKEX)                  \
    K(y, SSH2_MSG_KEX_ECDH_INIT, 30, SSH2_PKTCTX_ECDHKEX)               \
    K(y, SSH2_MSG_KEX_ECDH_REPLY, 31, SSH2_PKTCTX_ECDHKEX)              \
    X(y, SSH2_MSG_USERAUTH_REQUEST, 50)                                 \
    X(y, SSH2_MSG_USERAUTH_FAILURE, 51)                                 \
    X(y, SSH2_MSG_USERAUTH_SUCCESS, 52)                                 \
    X(y, SSH2_MSG_USERAUTH_BANNER, 53)                                  \
    A(y, SSH2_MSG_USERAUTH_PK_OK, 60, SSH2_PKTCTX_PUBLICKEY)            \
    A(y, SSH2_MSG_USERAUTH_PASSWD_CHANGEREQ, 60, SSH2_PKTCTX_PASSWORD)  \
    A(y, SSH2_MSG_USERAUTH_INFO_REQUEST, 60, SSH2_PKTCTX_KBDINTER)      \
    A(y, SSH2_MSG_USERAUTH_INFO_RESPONSE, 61, SSH2_PKTCTX_KBDINTER)     \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_RESPONSE, 60, SSH2_PKTCTX_GSSAPI)     \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_TOKEN, 61, SSH2_PKTCTX_GSSAPI)        \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_EXCHANGE_COMPLETE, 63, SSH2_PKTCTX_GSSAPI) \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_ERROR, 64, SSH2_PKTCTX_GSSAPI)        \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_ERRTOK, 65, SSH2_PKTCTX_GSSAPI)       \
    A(y, SSH2_MSG_USERAUTH_GSSAPI_MIC, 66, SSH2_PKTCTX_GSSAPI)          \
    X(y, SSH2_MSG_GLOBAL_REQUEST, 80)                                   \
    X(y, SSH2_MSG_REQUEST_SUCCESS, 81)                                  \
    X(y, SSH2_MSG_REQUEST_FAILURE, 82)                                  \
    X(y, SSH2_MSG_CHANNEL_OPEN, 90)                                     \
    X(y, SSH2_MSG_CHANNEL_OPEN_CONFIRMATION, 91)                        \
    X(y, SSH2_MSG_CHANNEL_OPEN_FAILURE, 92)                             \
    X(y, SSH2_MSG_CHANNEL_WINDOW_ADJUST, 93)                            \
    X(y, SSH2_MSG_CHANNEL_DATA, 94)                                     \
    X(y, SSH2_MSG_CHANNEL_EXTENDED_DATA, 95)                            \
    X(y, SSH2_MSG_CHANNEL_EOF, 96)                                      \
    X(y, SSH2_MSG_CHANNEL_CLOSE, 97)                                    \
    X(y, SSH2_MSG_CHANNEL_REQUEST, 98)                                  \
    X(y, SSH2_MSG_CHANNEL_SUCCESS, 99)                                  \
    X(y, SSH2_MSG_CHANNEL_FAILURE, 100)                                 \
    /* end of list */

#define DEF_ENUM_UNIVERSAL(y, name, value) name = value,
#define DEF_ENUM_CONTEXTUAL(y, name, value, context) name = value,
enum {
    SSH1_MESSAGE_TYPES(DEF_ENUM_UNIVERSAL, y)
    SSH2_MESSAGE_TYPES(DEF_ENUM_UNIVERSAL,
                       DEF_ENUM_CONTEXTUAL, DEF_ENUM_CONTEXTUAL, y)
    /* Virtual packet type, for packets too short to even have a type */
    SSH_MSG_NO_TYPE_CODE = 256
};
#undef DEF_ENUM_UNIVERSAL
#undef DEF_ENUM_CONTEXTUAL

/*
 * SSH-1 agent messages.
 */
#define SSH1_AGENTC_REQUEST_RSA_IDENTITIES    1
#define SSH1_AGENT_RSA_IDENTITIES_ANSWER      2
#define SSH1_AGENTC_RSA_CHALLENGE             3
#define SSH1_AGENT_RSA_RESPONSE               4
#define SSH1_AGENTC_ADD_RSA_IDENTITY          7
#define SSH1_AGENTC_REMOVE_RSA_IDENTITY       8
#define SSH1_AGENTC_REMOVE_ALL_RSA_IDENTITIES 9	/* openssh private? */

/*
 * Messages common to SSH-1 and OpenSSH's SSH-2.
 */
#define SSH_AGENT_FAILURE                    5
#define SSH_AGENT_SUCCESS                    6

/*
 * OpenSSH's SSH-2 agent messages.
 */
#define SSH2_AGENTC_REQUEST_IDENTITIES          11
#define SSH2_AGENT_IDENTITIES_ANSWER            12
#define SSH2_AGENTC_SIGN_REQUEST                13
#define SSH2_AGENT_SIGN_RESPONSE                14
#define SSH2_AGENTC_ADD_IDENTITY                17
#define SSH2_AGENTC_REMOVE_IDENTITY             18
#define SSH2_AGENTC_REMOVE_ALL_IDENTITIES       19

/*
 * Assorted other SSH-related enumerations.
 */
#define SSH2_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT 1	/* 0x1 */
#define SSH2_DISCONNECT_PROTOCOL_ERROR            2	/* 0x2 */
#define SSH2_DISCONNECT_KEY_EXCHANGE_FAILED       3	/* 0x3 */
#define SSH2_DISCONNECT_HOST_AUTHENTICATION_FAILED 4	/* 0x4 */
#define SSH2_DISCONNECT_MAC_ERROR                 5	/* 0x5 */
#define SSH2_DISCONNECT_COMPRESSION_ERROR         6	/* 0x6 */
#define SSH2_DISCONNECT_SERVICE_NOT_AVAILABLE     7	/* 0x7 */
#define SSH2_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED 8	/* 0x8 */
#define SSH2_DISCONNECT_HOST_KEY_NOT_VERIFIABLE   9	/* 0x9 */
#define SSH2_DISCONNECT_CONNECTION_LOST           10	/* 0xa */
#define SSH2_DISCONNECT_BY_APPLICATION            11	/* 0xb */
#define SSH2_DISCONNECT_TOO_MANY_CONNECTIONS      12	/* 0xc */
#define SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER    13	/* 0xd */
#define SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE 14	/* 0xe */
#define SSH2_DISCONNECT_ILLEGAL_USER_NAME         15	/* 0xf */

#define SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED     1	/* 0x1 */
#define SSH2_OPEN_CONNECT_FAILED                  2	/* 0x2 */
#define SSH2_OPEN_UNKNOWN_CHANNEL_TYPE            3	/* 0x3 */
#define SSH2_OPEN_RESOURCE_SHORTAGE               4	/* 0x4 */

#define SSH2_EXTENDED_DATA_STDERR                 1	/* 0x1 */

const char *ssh1_pkt_type(int type);
const char *ssh2_pkt_type(Pkt_KCtx pkt_kctx, Pkt_ACtx pkt_actx, int type);
int ssh2_pkt_type_code_valid(unsigned type);

/*
 * Need this to warn about support for the original SSH-2 keyfile
 * format.
 */
void old_keyfile_warning(void);

/*
 * Flags indicating implementation bugs that we know how to mitigate
 * if we think the other end has them.
 */
#define SSH_IMPL_BUG_LIST(X)                    \
    X(BUG_CHOKES_ON_SSH1_IGNORE)                \
    X(BUG_SSH2_HMAC)                            \
    X(BUG_NEEDS_SSH1_PLAIN_PASSWORD)            \
    X(BUG_CHOKES_ON_RSA)                        \
    X(BUG_SSH2_RSA_PADDING)                     \
    X(BUG_SSH2_DERIVEKEY)                       \
    X(BUG_SSH2_REKEY)                           \
    X(BUG_SSH2_PK_SESSIONID)                    \
    X(BUG_SSH2_MAXPKT)                          \
    X(BUG_CHOKES_ON_SSH2_IGNORE)                \
    X(BUG_CHOKES_ON_WINADJ)                     \
    X(BUG_SENDS_LATE_REQUEST_REPLY)             \
    X(BUG_SSH2_OLDGEX)                          \
    /* end of list */
#define TMP_DECLARE_LOG2_ENUM(thing) log2_##thing,
enum { SSH_IMPL_BUG_LIST(TMP_DECLARE_LOG2_ENUM) };
#undef TMP_DECLARE_LOG2_ENUM
#define TMP_DECLARE_REAL_ENUM(thing) thing = 1 << log2_##thing,
enum { SSH_IMPL_BUG_LIST(TMP_DECLARE_REAL_ENUM) };
#undef TMP_DECLARE_REAL_ENUM

/* Shared function that writes tty modes into a pty request */
void write_ttymodes_to_packet_from_conf(
    BinarySink *bs, Frontend *frontend, Conf *conf,
    int ssh_version, int ospeed, int ispeed);

/* Shared system for allocating local SSH channel ids. Expects to be
 * passed a tree full of structs that have a field called 'localid' of
 * type unsigned, and will check that! */
unsigned alloc_channel_id_general(tree234 *channels, size_t localid_offset);
#define alloc_channel_id(tree, type) \
    TYPECHECK(&((type *)0)->localid == (unsigned *)0, \
              alloc_channel_id_general(tree, offsetof(type, localid)))

int first_in_commasep_string(char const *needle, char const *haystack,
                             int haylen);
int in_commasep_string(char const *needle, char const *haystack, int haylen);
void add_to_commasep(strbuf *buf, const char *data);

int verify_ssh_manual_host_key(
    Conf *conf, const char *fingerprint, ssh_key *key);
