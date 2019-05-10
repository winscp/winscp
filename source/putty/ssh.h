#include <stdio.h>
#include <string.h>

#include "puttymem.h"
#include "tree234.h"
#include "network.h"
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
    bool on_free_queue;     /* is this packet scheduled for freeing? */
};

typedef struct PktIn {
    int type;
    unsigned long sequence; /* SSH-2 incoming sequence number */
    PacketQueueNode qnode;  /* for linking this packet on to a queue */
    BinarySource_IMPLEMENTATION;
} PktIn;

typedef struct PktOut {
    size_t prefix;          /* bytes up to and including type field */
    size_t length;          /* total bytes, including prefix */
    int type;
    size_t minlen;          /* SSH-2: ensure wire length is at least this */
    unsigned char *data;    /* allocated storage */
    size_t maxlen;          /* amount of storage allocated for `data' */

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
    PktIn *(*after)(PacketQueueBase *, PacketQueueNode *prev, bool pop);
} PktInQueue;

typedef struct PktOutQueue {
    PacketQueueBase pqb;
    PktOut *(*after)(PacketQueueBase *, PacketQueueNode *prev, bool pop);
} PktOutQueue;

void pq_base_push(PacketQueueBase *pqb, PacketQueueNode *node);
void pq_base_push_front(PacketQueueBase *pqb, PacketQueueNode *node);
void pq_base_concatenate(PacketQueueBase *dest,
                         PacketQueueBase *q1, PacketQueueBase *q2);

void pq_in_init(PktInQueue *pq);
void pq_out_init(PktOutQueue *pq);
void pq_in_clear(PktInQueue *pq);
void pq_out_clear(PktOutQueue *pq);

#define pq_push(pq, pkt)                                        \
    TYPECHECK((pq)->after(&(pq)->pqb, NULL, false) == pkt,      \
              pq_base_push(&(pq)->pqb, &(pkt)->qnode))
#define pq_push_front(pq, pkt)                                  \
    TYPECHECK((pq)->after(&(pq)->pqb, NULL, false) == pkt,      \
              pq_base_push_front(&(pq)->pqb, &(pkt)->qnode))
#define pq_peek(pq) ((pq)->after(&(pq)->pqb, &(pq)->pqb.end, false))
#define pq_pop(pq) ((pq)->after(&(pq)->pqb, &(pq)->pqb.end, true))
#define pq_concatenate(dst, q1, q2)                                     \
    TYPECHECK((q1)->after(&(q1)->pqb, NULL, false) ==                   \
              (dst)->after(&(dst)->pqb, NULL, false) &&                 \
              (q2)->after(&(q2)->pqb, NULL, false) ==                   \
              (dst)->after(&(dst)->pqb, NULL, false),                   \
              pq_base_concatenate(&(dst)->pqb, &(q1)->pqb, &(q2)->pqb))

#define pq_first(pq) pq_peek(pq)
#define pq_next(pq, pkt) ((pq)->after(&(pq)->pqb, &(pkt)->qnode, false))

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
    bool omit_passwords, omit_data;
    Pkt_KCtx kctx;
    Pkt_ACtx actx;
} PacketLogSettings;

#define MAX_BLANKS 4 /* no packet needs more censored sections than this */
int ssh1_censor_packet(
    const PacketLogSettings *pls, int type, bool sender_is_client,
    ptrlen pkt, logblank_t *blanks);
int ssh2_censor_packet(
    const PacketLogSettings *pls, int type, bool sender_is_client,
    ptrlen pkt, logblank_t *blanks);

PktOut *ssh_new_packet(void);
void ssh_free_pktout(PktOut *pkt);

Socket *ssh_connection_sharing_init(
    const char *host, int port, Conf *conf, LogContext *logctx,
    Plug *sshplug, ssh_sharing_state **state);
void ssh_connshare_provide_connlayer(ssh_sharing_state *sharestate,
                                     ConnectionLayer *cl);
bool ssh_share_test_for_upstream(const char *host, int port, Conf *conf);
void share_got_pkt_from_server(ssh_sharing_connstate *ctx, int type,
                               const void *pkt, int pktlen);
void share_activate(ssh_sharing_state *sharestate,
                    const char *server_verstring);
void sharestate_free(ssh_sharing_state *state);
int share_ndownstreams(ssh_sharing_state *state);

void ssh_connshare_log(Ssh *ssh, int event, const char *logtext,
                       const char *ds_err, const char *us_err);
void share_setup_x11_channel(ssh_sharing_connstate *cs, share_channel *chan,
                             unsigned upstream_id, unsigned server_id,
                             unsigned server_currwin, unsigned server_maxpkt,
                             unsigned client_adjusted_window,
                             const char *peer_addr, int peer_port, int endian,
                             int protomajor, int protominor,
                             const void *initial_data, int initial_len);

/* Per-application overrides for what roles we can take in connection
 * sharing, regardless of user configuration (e.g. pscp will never be
 * an upstream) */
extern const bool share_can_be_downstream;
extern const bool share_can_be_upstream;

struct X11Display;
struct X11FakeAuth;

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
        const char *description, const SocketPeerInfo *peerinfo,
        Channel *chan);

    /* Initiate opening of a 'session'-type channel */
    SshChannel *(*session_open)(ConnectionLayer *cl, Channel *chan);

    /* Open outgoing channels for X and agent forwarding. (Used in the
     * SSH server.) */
    SshChannel *(*serverside_x11_open)(ConnectionLayer *cl, Channel *chan,
                                       const SocketPeerInfo *pi);
    SshChannel *(*serverside_agent_open)(ConnectionLayer *cl, Channel *chan);

    /* Add an X11 display for ordinary X forwarding */
    struct X11FakeAuth *(*add_x11_display)(
        ConnectionLayer *cl, int authtype, struct X11Display *x11disp);

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
    bool (*agent_forwarding_permitted)(ConnectionLayer *cl);

    /* Set the size of the main terminal window (if any) */
    void (*terminal_size)(ConnectionLayer *cl, int width, int height);

    /* Indicate that the backlog on standard output has cleared */
    void (*stdout_unthrottle)(ConnectionLayer *cl, size_t bufsize);

    /* Query the size of the backlog on standard _input_ */
    size_t (*stdin_backlog)(ConnectionLayer *cl);

    /* Tell the connection layer that the SSH connection itself has
     * backed up, so it should tell all currently open channels to
     * cease reading from their local input sources if they can. (Or
     * tell it that that state of affairs has gone away again.) */
    void (*throttle_all_channels)(ConnectionLayer *cl, bool throttled);

    /* Ask the connection layer about its current preference for
     * line-discipline options. */
    bool (*ldisc_option)(ConnectionLayer *cl, int option);

    /* Communicate _to_ the connection layer (from the main session
     * channel) what its preference for line-discipline options is. */
    void (*set_ldisc_option)(ConnectionLayer *cl, int option, bool value);

    /* Communicate to the connection layer whether X and agent
     * forwarding were successfully enabled (for purposes of
     * knowing whether to accept subsequent channel-opens). */
    void (*enable_x_fwd)(ConnectionLayer *cl);
    void (*enable_agent_fwd)(ConnectionLayer *cl);

    /* Communicate to the connection layer whether the main session
     * channel currently wants user input. */
    void (*set_wants_user_input)(ConnectionLayer *cl, bool wanted);
};

struct ConnectionLayer {
    LogContext *logctx;
    const struct ConnectionLayerVtable *vt;
};

static inline struct ssh_rportfwd *ssh_rportfwd_alloc(
    ConnectionLayer *cl, const char *sh, int sp, const char *dh, int dp,
    int af, const char *log, PortFwdRecord *pfr, ssh_sharing_connstate *cs)
{ return cl->vt->rportfwd_alloc(cl, sh, sp, dh, dp, af, log, pfr, cs); }
static inline void ssh_rportfwd_remove(
    ConnectionLayer *cl, struct ssh_rportfwd *rpf)
{ cl->vt->rportfwd_remove(cl, rpf); }
static inline SshChannel *ssh_lportfwd_open(
    ConnectionLayer *cl, const char *host, int port,
    const char *desc, const SocketPeerInfo *pi, Channel *chan)
{ return cl->vt->lportfwd_open(cl, host, port, desc, pi, chan); }
static inline SshChannel *ssh_session_open(ConnectionLayer *cl, Channel *chan)
{ return cl->vt->session_open(cl, chan); }
static inline SshChannel *ssh_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketPeerInfo *pi)
{ return cl->vt->serverside_x11_open(cl, chan, pi); }
static inline SshChannel *ssh_serverside_agent_open(
    ConnectionLayer *cl, Channel *chan)
{ return cl->vt->serverside_agent_open(cl, chan); }
static inline struct X11FakeAuth *ssh_add_x11_display(
    ConnectionLayer *cl, int authtype, struct X11Display *x11disp)
{ return cl->vt->add_x11_display(cl, authtype, x11disp); }
static inline struct X11FakeAuth *ssh_add_sharing_x11_display(
    ConnectionLayer *cl, int authtype, ssh_sharing_connstate *share_cs,
    share_channel *share_chan)
{ return cl->vt->add_sharing_x11_display(cl, authtype, share_cs, share_chan); }
static inline void ssh_remove_sharing_x11_display(
    ConnectionLayer *cl, struct X11FakeAuth *auth)
{ cl->vt->remove_sharing_x11_display(cl, auth); }
static inline void ssh_send_packet_from_downstream(
    ConnectionLayer *cl, unsigned id, int type,
    const void *pkt, int len, const char *log)
{ cl->vt->send_packet_from_downstream(cl, id, type, pkt, len, log); }
static inline unsigned ssh_alloc_sharing_channel(
    ConnectionLayer *cl, ssh_sharing_connstate *connstate)
{ return cl->vt->alloc_sharing_channel(cl, connstate); }
static inline void ssh_delete_sharing_channel(
    ConnectionLayer *cl, unsigned localid)
{ cl->vt->delete_sharing_channel(cl, localid); }
static inline void ssh_sharing_queue_global_request(
    ConnectionLayer *cl, ssh_sharing_connstate *connstate)
{ cl->vt->sharing_queue_global_request(cl, connstate); }
static inline void ssh_sharing_no_more_downstreams(ConnectionLayer *cl)
{ cl->vt->sharing_no_more_downstreams(cl); }
static inline bool ssh_agent_forwarding_permitted(ConnectionLayer *cl)
{ return cl->vt->agent_forwarding_permitted(cl); }
static inline void ssh_terminal_size(ConnectionLayer *cl, int w, int h)
{ cl->vt->terminal_size(cl, w, h); }
static inline void ssh_stdout_unthrottle(ConnectionLayer *cl, size_t bufsize)
{ cl->vt->stdout_unthrottle(cl, bufsize); }
static inline size_t ssh_stdin_backlog(ConnectionLayer *cl)
{ return cl->vt->stdin_backlog(cl); }
static inline void ssh_throttle_all_channels(ConnectionLayer *cl, bool thr)
{ cl->vt->throttle_all_channels(cl, thr); }
static inline bool ssh_ldisc_option(ConnectionLayer *cl, int option)
{ return cl->vt->ldisc_option(cl, option); }
static inline void ssh_set_ldisc_option(ConnectionLayer *cl, int opt, bool val)
{ cl->vt->set_ldisc_option(cl, opt, val); }
static inline void ssh_enable_x_fwd(ConnectionLayer *cl)
{ cl->vt->enable_x_fwd(cl); }
static inline void ssh_enable_agent_fwd(ConnectionLayer *cl)
{ cl->vt->enable_agent_fwd(cl); }
static inline void ssh_set_wants_user_input(ConnectionLayer *cl, bool wanted)
{ cl->vt->set_wants_user_input(cl, wanted); }

/* Exports from portfwd.c */
PortFwdManager *portfwdmgr_new(ConnectionLayer *cl);
void portfwdmgr_free(PortFwdManager *mgr);
void portfwdmgr_config(PortFwdManager *mgr, Conf *conf);
void portfwdmgr_close(PortFwdManager *mgr, PortFwdRecord *pfr);
void portfwdmgr_close_all(PortFwdManager *mgr);
char *portfwdmgr_connect(PortFwdManager *mgr, Channel **chan_ret,
                         char *hostname, int port, SshChannel *c,
                         int addressfamily);
bool portfwdmgr_listen(PortFwdManager *mgr, const char *host, int port,
                       const char *keyhost, int keyport, Conf *conf);
bool portfwdmgr_unlisten(PortFwdManager *mgr, const char *host, int port);
Channel *portfwd_raw_new(ConnectionLayer *cl, Plug **plug);
void portfwd_raw_free(Channel *pfchan);
void portfwd_raw_setup(Channel *pfchan, Socket *s, SshChannel *sc);

Socket *platform_make_agent_socket(Plug *plug, const char *dirprefix,
                                   char **error, char **name);

LogContext *ssh_get_logctx(Ssh *ssh);

/* Communications back to ssh.c from connection layers */
void ssh_throttle_conn(Ssh *ssh, int adjust);
void ssh_got_exitcode(Ssh *ssh, int status);
void ssh_ldisc_update(Ssh *ssh);
void ssh_got_fallback_cmd(Ssh *ssh);

/* Communications back to ssh.c from the BPP */
void ssh_conn_processed_data(Ssh *ssh);
void ssh_check_frozen(Ssh *ssh);

/* Functions to abort the connection, for various reasons. */
void ssh_remote_error(Ssh *ssh, const char *fmt, ...);
void ssh_remote_eof(Ssh *ssh, const char *fmt, ...);
void ssh_proto_error(Ssh *ssh, const char *fmt, ...);
void ssh_sw_abort(Ssh *ssh, const char *fmt, ...);
void ssh_user_close(Ssh *ssh, const char *fmt, ...);

#define SSH_CIPHER_IDEA		1
#define SSH_CIPHER_DES		2
#define SSH_CIPHER_3DES		3
#define SSH_CIPHER_BLOWFISH	6

struct ssh_key {
    const ssh_keyalg *vt;
};

struct RSAKey {
    int bits;
    int bytes;
    mp_int *modulus;
    mp_int *exponent;
    mp_int *private_exponent;
    mp_int *p;
    mp_int *q;
    mp_int *iqmp;
    char *comment;
    ssh_key sshk;
};

struct dss_key {
    mp_int *p, *q, *g, *y, *x;
    ssh_key sshk;
};

struct ec_curve;

/* Weierstrass form curve */
struct ec_wcurve
{
    WeierstrassCurve *wc;
    WeierstrassPoint *G;
    mp_int *G_order;
};

/* Montgomery form curve */
struct ec_mcurve
{
    MontgomeryCurve *mc;
    MontgomeryPoint *G;
};

/* Edwards form curve */
struct ec_ecurve
{
    EdwardsCurve *ec;
    EdwardsPoint *G;
    mp_int *G_order;
};

typedef enum EllipticCurveType {
    EC_WEIERSTRASS, EC_MONTGOMERY, EC_EDWARDS
} EllipticCurveType;

struct ec_curve {
    EllipticCurveType type;
    /* 'name' is the identifier of the curve when it has to appear in
     * wire protocol encodings, as it does in e.g. the public key and
     * signature formats for NIST curves. Curves which do not format
     * their keys or signatures in this way just have name==NULL.
     *
     * 'textname' is non-NULL for all curves, and is a human-readable
     * identification suitable for putting in log messages. */
    const char *name, *textname;
    size_t fieldBits, fieldBytes;
    mp_int *p;
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
bool ec_nist_alg_and_curve_by_bits(int bits,
                                   const struct ec_curve **curve,
                                   const ssh_keyalg **alg);
bool ec_ed_alg_and_curve_by_bits(int bits,
                                 const struct ec_curve **curve,
                                 const ssh_keyalg **alg);

struct ecdsa_key {
    const struct ec_curve *curve;
    WeierstrassPoint *publicKey;
    mp_int *privateKey;
    ssh_key sshk;
};
struct eddsa_key {
    const struct ec_curve *curve;
    EdwardsPoint *publicKey;
    mp_int *privateKey;
    ssh_key sshk;
};

WeierstrassPoint *ecdsa_public(mp_int *private_key, const ssh_keyalg *alg);
EdwardsPoint *eddsa_public(mp_int *private_key, const ssh_keyalg *alg);

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
    BinarySource *src, RSAKey *result, RsaSsh1Order order);
void BinarySource_get_rsa_ssh1_priv(
    BinarySource *src, RSAKey *rsa);
bool rsa_ssh1_encrypt(unsigned char *data, int length, RSAKey *key);
mp_int *rsa_ssh1_decrypt(mp_int *input, RSAKey *key);
bool rsa_ssh1_decrypt_pkcs1(mp_int *input, RSAKey *key, strbuf *outbuf);
char *rsastr_fmt(RSAKey *key);
char *rsa_ssh1_fingerprint(RSAKey *key);
bool rsa_verify(RSAKey *key);
void rsa_ssh1_public_blob(BinarySink *bs, RSAKey *key, RsaSsh1Order order);
int rsa_ssh1_public_blob_len(ptrlen data);
void freersapriv(RSAKey *key);
void freersakey(RSAKey *key);

uint32_t crc32_rfc1662(ptrlen data);
uint32_t crc32_ssh1(ptrlen data);
uint32_t crc32_update(uint32_t crc_input, ptrlen data);

/* SSH CRC compensation attack detector */
struct crcda_ctx;
struct crcda_ctx *crcda_make_context(void);
void crcda_free_context(struct crcda_ctx *ctx);
bool detect_attack(struct crcda_ctx *ctx,
                   const unsigned char *buf, uint32_t len,
                   const unsigned char *IV);

/*
 * SSH2 RSA key exchange functions
 */
struct ssh_rsa_kex_extra {
    int minklen;
};
RSAKey *ssh_rsakex_newkey(ptrlen data);
void ssh_rsakex_freekey(RSAKey *key);
int ssh_rsakex_klen(RSAKey *key);
strbuf *ssh_rsakex_encrypt(
    RSAKey *key, const ssh_hashalg *h, ptrlen plaintext);
mp_int *ssh_rsakex_decrypt(
    RSAKey *key, const ssh_hashalg *h, ptrlen ciphertext);

/*
 * SSH2 ECDH key exchange functions
 */
const char *ssh_ecdhkex_curve_textname(const ssh_kex *kex);
ecdh_key *ssh_ecdhkex_newkey(const ssh_kex *kex);
void ssh_ecdhkex_freekey(ecdh_key *key);
void ssh_ecdhkex_getpublic(ecdh_key *key, BinarySink *bs);
mp_int *ssh_ecdhkex_getkey(ecdh_key *key, ptrlen remoteKey);

/*
 * Helper function for k generation in DSA, reused in ECDSA
 */
mp_int *dss_gen_k(const char *id_string,
                     mp_int *modulus, mp_int *private_key,
                     unsigned char *digest, int digest_len);

struct ssh_cipher {
    const ssh_cipheralg *vt;
};

struct ssh_cipheralg {
    ssh_cipher *(*new)(const ssh_cipheralg *alg);
    void (*free)(ssh_cipher *);
    void (*setiv)(ssh_cipher *, const void *iv);
    void (*setkey)(ssh_cipher *, const void *key);
    void (*encrypt)(ssh_cipher *, void *blk, int len);
    void (*decrypt)(ssh_cipher *, void *blk, int len);
    /* Ignored unless SSH_CIPHER_SEPARATE_LENGTH flag set */
    void (*encrypt_length)(ssh_cipher *, void *blk, int len,
                           unsigned long seq);
    void (*decrypt_length)(ssh_cipher *, void *blk, int len,
                           unsigned long seq);
    const char *ssh2_id;
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
    const ssh2_macalg *required_mac;

    /* Pointer to any extra data used by a particular implementation. */
    const void *extra;
};

static inline ssh_cipher *ssh_cipher_new(const ssh_cipheralg *alg)
{ return alg->new(alg); }
static inline void ssh_cipher_free(ssh_cipher *c)
{ c->vt->free(c); }
static inline void ssh_cipher_setiv(ssh_cipher *c, const void *iv)
{ c->vt->setiv(c, iv); }
static inline void ssh_cipher_setkey(ssh_cipher *c, const void *key)
{ c->vt->setkey(c, key); }
static inline void ssh_cipher_encrypt(ssh_cipher *c, void *blk, int len)
{ c->vt->encrypt(c, blk, len); }
static inline void ssh_cipher_decrypt(ssh_cipher *c, void *blk, int len)
{ c->vt->decrypt(c, blk, len); }
static inline void ssh_cipher_encrypt_length(
    ssh_cipher *c, void *blk, int len, unsigned long seq)
{ return c->vt->encrypt_length(c, blk, len, seq); }
static inline void ssh_cipher_decrypt_length(
    ssh_cipher *c, void *blk, int len, unsigned long seq)
{ return c->vt->decrypt_length(c, blk, len, seq); }
static inline const struct ssh_cipheralg *ssh_cipher_alg(ssh_cipher *c)
{ return c->vt; }

struct ssh2_ciphers {
    int nciphers;
    const ssh_cipheralg *const *list;
};

struct ssh2_mac {
    const ssh2_macalg *vt;
    BinarySink_DELEGATE_IMPLEMENTATION;
};

struct ssh2_macalg {
    /* Passes in the cipher context */
    ssh2_mac *(*new)(const ssh2_macalg *alg, ssh_cipher *cipher);
    void (*free)(ssh2_mac *);
    void (*setkey)(ssh2_mac *, ptrlen key);
    void (*start)(ssh2_mac *);
    void (*genresult)(ssh2_mac *, unsigned char *);
    const char *(*text_name)(ssh2_mac *);
    const char *name, *etm_name;
    int len, keylen;

    /* Pointer to any extra data used by a particular implementation. */
    const void *extra;
};

static inline ssh2_mac *ssh2_mac_new(
    const ssh2_macalg *alg, ssh_cipher *cipher)
{ return alg->new(alg, cipher); }
static inline void ssh2_mac_free(ssh2_mac *m)
{ m->vt->free(m); }
static inline void ssh2_mac_setkey(ssh2_mac *m, ptrlen key)
{ m->vt->setkey(m, key); }
static inline void ssh2_mac_start(ssh2_mac *m)
{ m->vt->start(m); }
static inline void ssh2_mac_genresult(ssh2_mac *m, unsigned char *out)
{ m->vt->genresult(m, out); }
static inline const char *ssh2_mac_text_name(ssh2_mac *m)
{ return m->vt->text_name(m); }
static inline const ssh2_macalg *ssh2_mac_alg(ssh2_mac *m)
{ return m->vt; }

/* Centralised 'methods' for ssh2_mac, defined in sshmac.c. These run
 * the MAC in a specifically SSH-2 style, i.e. taking account of a
 * packet sequence number as well as the data to be authenticated. */
bool ssh2_mac_verresult(ssh2_mac *, const void *);
void ssh2_mac_generate(ssh2_mac *, void *, int, unsigned long seq);
bool ssh2_mac_verify(ssh2_mac *, const void *, int, unsigned long seq);

/* Use a MAC in its raw form, outside SSH-2 context, to MAC a given
 * string with a given key in the most obvious way. */
void mac_simple(const ssh2_macalg *alg, ptrlen key, ptrlen data, void *output);

struct ssh_hash {
    const ssh_hashalg *vt;
    BinarySink_DELEGATE_IMPLEMENTATION;
};

struct ssh_hashalg {
    ssh_hash *(*new)(const ssh_hashalg *alg);
    ssh_hash *(*copy)(ssh_hash *);
    void (*final)(ssh_hash *, unsigned char *); /* ALSO FREES THE ssh_hash! */
    void (*free)(ssh_hash *);
    int hlen; /* output length in bytes */
    int blocklen; /* length of the hash's input block, or 0 for N/A */
    const char *text_basename;     /* the semantic name of the hash */
    const char *annotation;   /* extra info, e.g. which of multiple impls */
    const char *text_name;    /* both combined, e.g. "SHA-n (unaccelerated)" */
};

static inline ssh_hash *ssh_hash_new(const ssh_hashalg *alg)
{ return alg->new(alg); }
static inline ssh_hash *ssh_hash_copy(ssh_hash *h)
{ return h->vt->copy(h); }
static inline void ssh_hash_final(ssh_hash *h, unsigned char *out)
{ return h->vt->final(h, out); }
static inline void ssh_hash_free(ssh_hash *h)
{ return h->vt->free(h); }
static inline const ssh_hashalg *ssh_hash_alg(ssh_hash *h)
{ return h->vt; }

/* Handy macros for defining all those text-name fields at once */
#define HASHALG_NAMES_BARE(base) \
    base, NULL, base
#define HASHALG_NAMES_ANNOTATED(base, annotation) \
    base, annotation, base " (" annotation ")"

void hash_simple(const ssh_hashalg *alg, ptrlen data, void *output);

struct ssh_kex {
    const char *name, *groupname;
    enum { KEXTYPE_DH, KEXTYPE_RSA, KEXTYPE_ECDH, KEXTYPE_GSS } main_type;
    const ssh_hashalg *hash;
    const void *extra;                 /* private to the kex methods */
};

struct ssh_kexes {
    int nkexes;
    const ssh_kex *const *list;
};

struct ssh_keyalg {
    /* Constructors that create an ssh_key */
    ssh_key *(*new_pub) (const ssh_keyalg *self, ptrlen pub);
    ssh_key *(*new_priv) (const ssh_keyalg *self, ptrlen pub, ptrlen priv);
    ssh_key *(*new_priv_openssh) (const ssh_keyalg *self, BinarySource *);

    /* Methods that operate on an existing ssh_key */
    void (*freekey) (ssh_key *key);
    char *(*invalid) (ssh_key *key, unsigned flags);
    void (*sign) (ssh_key *key, ptrlen data, unsigned flags, BinarySink *);
    bool (*verify) (ssh_key *key, ptrlen sig, ptrlen data);
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
    const unsigned supported_flags;    /* signature-type flags we understand */
};

static inline ssh_key *ssh_key_new_pub(const ssh_keyalg *self, ptrlen pub)
{ return self->new_pub(self, pub); }
static inline ssh_key *ssh_key_new_priv(
    const ssh_keyalg *self, ptrlen pub, ptrlen priv)
{ return self->new_priv(self, pub, priv); }
static inline ssh_key *ssh_key_new_priv_openssh(
    const ssh_keyalg *self, BinarySource *src)
{ return self->new_priv_openssh(self, src); }
static inline void ssh_key_free(ssh_key *key)
{ key->vt->freekey(key); }
static inline char *ssh_key_invalid(ssh_key *key, unsigned flags)
{ return key->vt->invalid(key, flags); }
static inline void ssh_key_sign(
    ssh_key *key, ptrlen data, unsigned flags, BinarySink *bs)
{ key->vt->sign(key, data, flags, bs); }
static inline bool ssh_key_verify(ssh_key *key, ptrlen sig, ptrlen data)
{ return key->vt->verify(key, sig, data); }
static inline void ssh_key_public_blob(ssh_key *key, BinarySink *bs)
{ key->vt->public_blob(key, bs); }
static inline void ssh_key_private_blob(ssh_key *key, BinarySink *bs)
{ key->vt->private_blob(key, bs); }
static inline void ssh_key_openssh_blob(ssh_key *key, BinarySink *bs)
{ key->vt->openssh_blob(key, bs); }
static inline char *ssh_key_cache_str(ssh_key *key)
{ return key->vt->cache_str(key); }
static inline int ssh_key_public_bits(const ssh_keyalg *self, ptrlen blob)
{ return self->pubkey_bits(self, blob); }
static inline const ssh_keyalg *ssh_key_alg(ssh_key *key)
{ return key->vt; }
static inline const char *ssh_key_ssh_id(ssh_key *key)
{ return key->vt->ssh_id; }
static inline const char *ssh_key_cache_id(ssh_key *key)
{ return key->vt->cache_id; }

/*
 * Enumeration of signature flags from draft-miller-ssh-agent-02
 */
#define SSH_AGENT_RSA_SHA2_256 2
#define SSH_AGENT_RSA_SHA2_512 4

struct ssh_compressor {
    const ssh_compression_alg *vt;
};
struct ssh_decompressor {
    const ssh_compression_alg *vt;
};

struct ssh_compression_alg {
    const char *name;
    /* For zlib@openssh.com: if non-NULL, this name will be considered once
     * userauth has completed successfully. */
    const char *delayed_name;
    ssh_compressor *(*compress_new)(void);
    void (*compress_free)(ssh_compressor *);
    void (*compress)(ssh_compressor *, const unsigned char *block, int len,
                     unsigned char **outblock, int *outlen,
                     int minlen);
    ssh_decompressor *(*decompress_new)(void);
    void (*decompress_free)(ssh_decompressor *);
    bool (*decompress)(ssh_decompressor *, const unsigned char *block, int len,
                       unsigned char **outblock, int *outlen);
    const char *text_name;
};

static inline ssh_compressor *ssh_compressor_new(
    const ssh_compression_alg *alg)
{ return alg->compress_new(); }
static inline ssh_decompressor *ssh_decompressor_new(
    const ssh_compression_alg *alg)
{ return alg->decompress_new(); }
static inline void ssh_compressor_free(ssh_compressor *c)
{ c->vt->compress_free(c); }
static inline void ssh_decompressor_free(ssh_decompressor *d)
{ d->vt->decompress_free(d); }
static inline void ssh_compressor_compress(
    ssh_compressor *c, const unsigned char *block, int len,
    unsigned char **outblock, int *outlen, int minlen)
{ c->vt->compress(c, block, len, outblock, outlen, minlen); }
static inline bool ssh_decompressor_decompress(
    ssh_decompressor *d, const unsigned char *block, int len,
    unsigned char **outblock, int *outlen)
{ return d->vt->decompress(d, block, len, outblock, outlen); }
static inline const ssh_compression_alg *ssh_compressor_alg(
    ssh_compressor *c)
{ return c->vt; }
static inline const ssh_compression_alg *ssh_decompressor_alg(
    ssh_decompressor *d)
{ return d->vt; }

struct ssh2_userkey {
    ssh_key *key;                      /* the key itself */
    char *comment;		       /* the key comment */
};

/* The maximum length of any hash algorithm. (bytes) */
#define MAX_HASH_LEN (64)              /* longest is SHA-512 */

extern const ssh_cipheralg ssh_3des_ssh1;
extern const ssh_cipheralg ssh_blowfish_ssh1;
extern const ssh_cipheralg ssh_3des_ssh2_ctr;
extern const ssh_cipheralg ssh_3des_ssh2;
extern const ssh_cipheralg ssh_des;
extern const ssh_cipheralg ssh_des_sshcom_ssh2;
extern const ssh_cipheralg ssh_aes256_sdctr;
extern const ssh_cipheralg ssh_aes256_sdctr_hw;
extern const ssh_cipheralg ssh_aes256_sdctr_sw;
extern const ssh_cipheralg ssh_aes256_cbc;
extern const ssh_cipheralg ssh_aes256_cbc_hw;
extern const ssh_cipheralg ssh_aes256_cbc_sw;
extern const ssh_cipheralg ssh_aes192_sdctr;
extern const ssh_cipheralg ssh_aes192_sdctr_hw;
extern const ssh_cipheralg ssh_aes192_sdctr_sw;
extern const ssh_cipheralg ssh_aes192_cbc;
extern const ssh_cipheralg ssh_aes192_cbc_hw;
extern const ssh_cipheralg ssh_aes192_cbc_sw;
extern const ssh_cipheralg ssh_aes128_sdctr;
extern const ssh_cipheralg ssh_aes128_sdctr_hw;
extern const ssh_cipheralg ssh_aes128_sdctr_sw;
extern const ssh_cipheralg ssh_aes128_cbc;
extern const ssh_cipheralg ssh_aes128_cbc_hw;
extern const ssh_cipheralg ssh_aes128_cbc_sw;
extern const ssh_cipheralg ssh_blowfish_ssh2_ctr;
extern const ssh_cipheralg ssh_blowfish_ssh2;
extern const ssh_cipheralg ssh_arcfour256_ssh2;
extern const ssh_cipheralg ssh_arcfour128_ssh2;
extern const ssh_cipheralg ssh2_chacha20_poly1305;
extern const ssh2_ciphers ssh2_3des;
extern const ssh2_ciphers ssh2_des;
extern const ssh2_ciphers ssh2_aes;
extern const ssh2_ciphers ssh2_blowfish;
extern const ssh2_ciphers ssh2_arcfour;
extern const ssh2_ciphers ssh2_ccp;
extern const ssh_hashalg ssh_md5;
extern const ssh_hashalg ssh_sha1;
extern const ssh_hashalg ssh_sha1_hw;
extern const ssh_hashalg ssh_sha1_sw;
extern const ssh_hashalg ssh_sha256;
extern const ssh_hashalg ssh_sha256_hw;
extern const ssh_hashalg ssh_sha256_sw;
extern const ssh_hashalg ssh_sha384;
extern const ssh_hashalg ssh_sha512;
extern const ssh_kexes ssh_diffiehellman_group1;
extern const ssh_kexes ssh_diffiehellman_group14;
extern const ssh_kexes ssh_diffiehellman_gex;
extern const ssh_kexes ssh_gssk5_sha1_kex;
extern const ssh_kexes ssh_rsa_kex;
extern const ssh_kex ssh_ec_kex_curve25519;
extern const ssh_kex ssh_ec_kex_nistp256;
extern const ssh_kex ssh_ec_kex_nistp384;
extern const ssh_kex ssh_ec_kex_nistp521;
extern const ssh_kexes ssh_ecdh_kex;
extern const ssh_keyalg ssh_dss;
extern const ssh_keyalg ssh_rsa;
extern const ssh_keyalg ssh_ecdsa_ed25519;
extern const ssh_keyalg ssh_ecdsa_nistp256;
extern const ssh_keyalg ssh_ecdsa_nistp384;
extern const ssh_keyalg ssh_ecdsa_nistp521;
extern const ssh2_macalg ssh_hmac_md5;
extern const ssh2_macalg ssh_hmac_sha1;
extern const ssh2_macalg ssh_hmac_sha1_buggy;
extern const ssh2_macalg ssh_hmac_sha1_96;
extern const ssh2_macalg ssh_hmac_sha1_96_buggy;
extern const ssh2_macalg ssh_hmac_sha256;
extern const ssh2_macalg ssh2_poly1305;
extern const ssh_compression_alg ssh_zlib;

/*
 * On some systems, you have to detect hardware crypto acceleration by
 * asking the local OS API rather than OS-agnostically asking the CPU
 * itself. If so, then this function should be implemented in each
 * platform subdirectory.
 */
bool platform_aes_hw_available(void);
bool platform_sha256_hw_available(void);
bool platform_sha1_hw_available(void);

/*
 * PuTTY version number formatted as an SSH version string. 
 */
extern const char sshver[];

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which scp.c can reach
 * into the SSH code and find out which one it got.
 */
extern bool ssh_fallback_cmd(Backend *backend);

/*
 * The PRNG type, defined in sshprng.c. Visible data fields are
 * 'savesize', which suggests how many random bytes you should request
 * from a particular PRNG instance to write to putty.rnd, and a
 * BinarySink implementation which you can use to write seed data in
 * between calling prng_seed_{begin,finish}.
 */
struct prng {
    size_t savesize;
    BinarySink_IMPLEMENTATION;
    /* (also there's a surrounding implementation struct in sshprng.c) */
};
prng *prng_new(const ssh_hashalg *hashalg);
void prng_free(prng *p);
void prng_seed_begin(prng *p);
void prng_seed_finish(prng *p);
void prng_read(prng *p, void *vout, size_t size);
void prng_add_entropy(prng *p, unsigned source_id, ptrlen data);
size_t prng_seed_bits(prng *p);

/* This function must be implemented by the platform, and returns a
 * timer in milliseconds that the PRNG can use to know whether it's
 * been reseeded too recently to do it again.
 *
 * The PRNG system has its own special timing function not because its
 * timing needs are unusual in the real applications, but simply so
 * that testcrypt can mock it to keep the tests deterministic. */
uint64_t prng_reseed_time_ms(void);

void random_read(void *out, size_t size);

/* Exports from x11fwd.c */
enum {
    X11_TRANS_IPV4 = 0, X11_TRANS_IPV6 = 6, X11_TRANS_UNIX = 256
};
struct X11Display {
    /* Broken-down components of the display name itself */
    bool unixdomain;
    char *hostname;
    int displaynum;
    int screennum;
    /* OSX sometimes replaces all the above with a full Unix-socket pathname */
    char *unixsocketpath;

    /* PuTTY networking SockAddr to connect to the display, and associated
     * gubbins */
    SockAddr *addr;
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
 *
 * If the returned pointer is NULL, then *error_msg will contain a
 * dynamically allocated error message string.
 */
extern struct X11Display *x11_setup_display(const char *display, Conf *,
                                            char **error_msg);
void x11_free_display(struct X11Display *disp);
struct X11FakeAuth *x11_invent_fake_auth(tree234 *t, int authtype);
void x11_free_fake_auth(struct X11FakeAuth *auth);
Channel *x11_new_channel(tree234 *authtree, SshChannel *c,
                         const char *peeraddr, int peerport,
                         bool connection_sharing_possible);
char *x11_display(const char *display);
/* Platform-dependent X11 functions */
extern void platform_get_x11_auth(struct X11Display *display, Conf *);
    /* examine a mostly-filled-in X11Display and fill in localauth* */
extern const bool platform_uses_x11_unix_by_default;
    /* choose default X transport in the absence of a specified one */
SockAddr *platform_get_x11_unix_address(const char *path, int displaynum);
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
void x11_format_auth_for_authfile(
    BinarySink *bs, SockAddr *addr, int display_no,
    ptrlen authproto, ptrlen authdata);
int x11_identify_auth_proto(ptrlen protoname);
void *x11_dehexify(ptrlen hex, int *outlen);

Channel *agentf_new(SshChannel *c);

bool dh_is_gex(const ssh_kex *kex);
dh_ctx *dh_setup_group(const ssh_kex *kex);
dh_ctx *dh_setup_gex(mp_int *pval, mp_int *gval);
int dh_modulus_bit_size(const dh_ctx *ctx);
void dh_cleanup(dh_ctx *);
mp_int *dh_create_e(dh_ctx *, int nbits);
const char *dh_validate_f(dh_ctx *, mp_int *f);
mp_int *dh_find_K(dh_ctx *, mp_int *f);

bool rsa_ssh1_encrypted(const Filename *filename, char **comment);
int rsa_ssh1_loadpub(const Filename *filename, BinarySink *bs,
                     char **commentptr, const char **errorstr);
int rsa_ssh1_loadkey(const Filename *filename, RSAKey *key,
                     const char *passphrase, const char **errorstr);
bool rsa_ssh1_savekey(const Filename *filename, RSAKey *key, char *passphrase);

static inline bool is_base64_char(char c)
{
    return ((c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '+' || c == '/' || c == '=');
}

extern int base64_decode_atom(const char *atom, unsigned char *out);
extern int base64_lines(int datalen);
extern void base64_encode_atom(const unsigned char *data, int n, char *out);
extern void base64_encode(FILE *fp, const unsigned char *data, int datalen,
                          int cpl);

/* ssh2_load_userkey can return this as an error */
extern ssh2_userkey ssh2_wrong_passphrase;
#define SSH2_WRONG_PASSPHRASE (&ssh2_wrong_passphrase)

bool ssh2_userkey_encrypted(const Filename *filename, char **comment);
ssh2_userkey *ssh2_load_userkey(
    const Filename *filename, const char *passphrase, const char **errorstr);
bool ssh2_userkey_loadpub(
    const Filename *filename, char **algorithm, BinarySink *bs,
    char **commentptr, const char **errorstr);
bool ssh2_save_userkey(
    const Filename *filename, ssh2_userkey *key, char *passphrase);
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
char *ssh1_pubkey_str(RSAKey *ssh1key);
void ssh1_write_pubkey(FILE *fp, RSAKey *ssh1key);
char *ssh2_pubkey_openssh_str(ssh2_userkey *key);
void ssh2_write_pubkey(FILE *fp, const char *comment,
                       const void *v_pub_blob, int pub_len,
                       int keytype);
char *ssh2_fingerprint_blob(ptrlen);
char *ssh2_fingerprint(ssh_key *key);
int key_type(const Filename *filename);
const char *key_type_to_str(int type);

bool import_possible(int type);
int import_target_type(int type);
bool import_encrypted(const Filename *filename, int type, char **comment);
int import_ssh1(const Filename *filename, int type,
		RSAKey *key, char *passphrase, const char **errmsg_p);
ssh2_userkey *import_ssh2(const Filename *filename, int type,
                          char *passphrase, const char **errmsg_p);
bool export_ssh1(const Filename *filename, int type,
                 RSAKey *key, char *passphrase);
bool export_ssh2(const Filename *filename, int type,
                 ssh2_userkey *key, char *passphrase);

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

int rsa_generate(RSAKey *key, int bits, progfn_t pfn,
		 void *pfnparam);
int dsa_generate(struct dss_key *key, int bits, progfn_t pfn,
		 void *pfnparam);
int ecdsa_generate(struct ecdsa_key *key, int bits, progfn_t pfn,
                   void *pfnparam);
int eddsa_generate(struct eddsa_key *key, int bits, progfn_t pfn,
                   void *pfnparam);
mp_int *primegen(
    int bits, int modulus, int residue, mp_int *factor,
    int phase, progfn_t pfn, void *pfnparam, unsigned firstbits);
void invent_firstbits(unsigned *one, unsigned *two, unsigned min_separation);

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
                       Plug *downplug, Plug *upplug, Socket **sock,
                       char **logtext, char **ds_err, char **us_err,
                       bool can_upstream, bool can_downstream);
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

enum {
    /* TTY modes with opcodes defined consistently in the SSH specs. */
    #define TTYMODE_CHAR(name, val, index) SSH_TTYMODE_##name = val,
    #define TTYMODE_FLAG(name, val, field, mask) SSH_TTYMODE_##name = val,
    #include "sshttymodes.h"
    #undef TTYMODE_CHAR
    #undef TTYMODE_FLAG

    /* Modes encoded differently between SSH-1 and SSH-2, for which we
     * make up our own dummy opcodes to avoid confusion. */
    TTYMODE_dummy = 255,
    TTYMODE_ISPEED, TTYMODE_OSPEED,

    /* Limiting value that we can use as an array bound below */
    TTYMODE_LIMIT,

    /* The real opcodes for terminal speeds. */
    TTYMODE_ISPEED_SSH1 = 192,
    TTYMODE_OSPEED_SSH1 = 193,
    TTYMODE_ISPEED_SSH2 = 128,
    TTYMODE_OSPEED_SSH2 = 129,

    /* And the opcode that ends a list. */
    TTYMODE_END_OF_LIST = 0
};

struct ssh_ttymodes {
    /* A boolean per mode, indicating whether it's set. */
    bool have_mode[TTYMODE_LIMIT];

    /* The actual value for each mode. */
    unsigned mode_val[TTYMODE_LIMIT];
};

struct ssh_ttymodes get_ttymodes_from_conf(Seat *seat, Conf *conf);
struct ssh_ttymodes read_ttymodes_from_packet(
    BinarySource *bs, int ssh_version);
void write_ttymodes_to_packet(BinarySink *bs, int ssh_version,
                              struct ssh_ttymodes modes);

const char *ssh1_pkt_type(int type);
const char *ssh2_pkt_type(Pkt_KCtx pkt_kctx, Pkt_ACtx pkt_actx, int type);
bool ssh2_pkt_type_code_valid(unsigned type);

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

/* Shared system for allocating local SSH channel ids. Expects to be
 * passed a tree full of structs that have a field called 'localid' of
 * type unsigned, and will check that! */
unsigned alloc_channel_id_general(tree234 *channels, size_t localid_offset);
#define alloc_channel_id(tree, type) \
    TYPECHECK(&((type *)0)->localid == (unsigned *)0, \
              alloc_channel_id_general(tree, offsetof(type, localid)))

void add_to_commasep(strbuf *buf, const char *data);
bool get_commasep_word(ptrlen *list, ptrlen *word);

int verify_ssh_manual_host_key(
    Conf *conf, const char *fingerprint, ssh_key *key);

typedef struct ssh_transient_hostkey_cache ssh_transient_hostkey_cache;
ssh_transient_hostkey_cache *ssh_transient_hostkey_cache_new(void);
void ssh_transient_hostkey_cache_free(ssh_transient_hostkey_cache *thc);
void ssh_transient_hostkey_cache_add(
    ssh_transient_hostkey_cache *thc, ssh_key *key);
bool ssh_transient_hostkey_cache_verify(
    ssh_transient_hostkey_cache *thc, ssh_key *key);
bool ssh_transient_hostkey_cache_has(
    ssh_transient_hostkey_cache *thc, const ssh_keyalg *alg);
bool ssh_transient_hostkey_cache_non_empty(ssh_transient_hostkey_cache *thc);
