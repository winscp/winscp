#include <stdio.h>
#include <string.h>

#define WINSCP_SSH

#include "puttymem.h"
#include "tree234.h"
#ifndef WINSCP_VS
#include "network.h"
#endif
#include "misc.h"

#ifndef WINSCP_VS
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
    size_t formal_size;    /* contribution to PacketQueueBase's total_size */
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
    size_t total_size;    /* sum of all formal_size fields on the queue */
    struct IdempotentCallback *ic;
    Seat * seat; // WINSCP
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

void pq_in_init(PktInQueue *pq, Seat * seat); // WINSCP
void pq_out_init(PktOutQueue *pq, Seat * seat); // WINSCP
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
    SSH2_PKTCTX_HYBRIDKEX,
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

typedef struct ConnectionLayerVtable ConnectionLayerVtable;

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
        const char *description, const SocketEndpointInfo *peerinfo,
        Channel *chan);

    /* Initiate opening of a 'session'-type channel */
    SshChannel *(*session_open)(ConnectionLayer *cl, Channel *chan);

    /* Open outgoing channels for X and agent forwarding. (Used in the
     * SSH server.) */
    SshChannel *(*serverside_x11_open)(ConnectionLayer *cl, Channel *chan,
                                       const SocketEndpointInfo *pi);
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

    /* Communicate to the connection layer whether X forwarding was
     * successfully enabled (for purposes of knowing whether to accept
     * subsequent channel-opens). */
    void (*enable_x_fwd)(ConnectionLayer *cl);

    /* Communicate / query whether the main session channel currently
     * wants user input. The set function is called by mainchan; the
     * query function is called by the top-level ssh.c. */
    void (*set_wants_user_input)(ConnectionLayer *cl, bool wanted);
    bool (*get_wants_user_input)(ConnectionLayer *cl);

    /* Notify the connection layer that more data has been added to
     * the user input queue. */
    void (*got_user_input)(ConnectionLayer *cl);
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
    const char *desc, const SocketEndpointInfo *pi, Channel *chan)
{ return cl->vt->lportfwd_open(cl, host, port, desc, pi, chan); }
static inline SshChannel *ssh_session_open(ConnectionLayer *cl, Channel *chan)
{ return cl->vt->session_open(cl, chan); }
static inline SshChannel *ssh_serverside_x11_open(
    ConnectionLayer *cl, Channel *chan, const SocketEndpointInfo *pi)
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
static inline void ssh_set_wants_user_input(ConnectionLayer *cl, bool wanted)
{ cl->vt->set_wants_user_input(cl, wanted); }
static inline bool ssh_get_wants_user_input(ConnectionLayer *cl)
{ return cl->vt->get_wants_user_input(cl); }
static inline void ssh_got_user_input(ConnectionLayer *cl)
{ cl->vt->got_user_input(cl); }

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
Channel *portfwd_raw_new(ConnectionLayer *cl, Plug **plug, bool start_ready);
void portfwd_raw_free(Channel *pfchan);
void portfwd_raw_setup(Channel *pfchan, Socket *s, SshChannel *sc);

Socket *platform_make_agent_socket(Plug *plug, const char *dirprefix,
                                   char **error, char **name);

LogContext *ssh_get_logctx(Ssh *ssh);

/* Communications back to ssh.c from connection layers */
void ssh_throttle_conn(Ssh *ssh, int adjust);
void ssh_got_exitcode(Ssh *ssh, int status);
void ssh_ldisc_update(Ssh *ssh);
void ssh_check_sendok(Ssh *ssh);
void ssh_got_fallback_cmd(Ssh *ssh);
bool ssh_is_bare(Ssh *ssh);

/* Communications back to ssh.c from the BPP */
void ssh_conn_processed_data(Ssh *ssh);
void ssh_sendbuffer_changed(Ssh *ssh);
void ssh_check_frozen(Ssh *ssh);

/* Functions to abort the connection, for various reasons. */
void ssh_remote_error(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_remote_eof(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_proto_error(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_sw_abort(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_sw_abort_deferred(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_user_close(Ssh *ssh, const char *fmt, ...) PRINTF_LIKE(2, 3);
void ssh_spr_close(Ssh *ssh, SeatPromptResult spr, const char *context);

/* Bit positions in the SSH-1 cipher protocol word */
#define SSH1_CIPHER_IDEA        1
#define SSH1_CIPHER_DES         2
#define SSH1_CIPHER_3DES        3
#define SSH1_CIPHER_BLOWFISH    6

/* The subset of those that we support, with names for selecting them
 * on Uppity's command line */
#define SSH1_SUPPORTED_CIPHER_LIST(X)           \
    X(SSH1_CIPHER_3DES, "3des")                 \
    X(SSH1_CIPHER_BLOWFISH, "blowfish")         \
    X(SSH1_CIPHER_DES, "des")                   \
    /* end of list */
#define SSH1_CIPHER_LIST_MAKE_MASK(bitpos, name) | (1U << bitpos)
#define SSH1_SUPPORTED_CIPHER_MASK \
    (0 SSH1_SUPPORTED_CIPHER_LIST(SSH1_CIPHER_LIST_MAKE_MASK))

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

struct dsa_key {
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
    unsigned log2_cofactor;
};

/* Edwards form curve */
struct ec_ecurve
{
    EdwardsCurve *ec;
    EdwardsPoint *G;
    mp_int *G_order;
    unsigned log2_cofactor;
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
extern const int ec_ed_curve_lengths[], n_ec_ed_curve_lengths;
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

typedef enum KeyComponentType {
    KCT_TEXT, KCT_BINARY, KCT_MPINT
} KeyComponentType;
typedef struct key_component {
    char *name;
    KeyComponentType type;
    union {
        strbuf *str;                   /* used for KCT_TEXT and KCT_BINARY */
        mp_int *mp;                    /* used for KCT_MPINT */
    };
} key_component;
typedef struct key_components {
    size_t ncomponents, componentsize;
    key_component *components;
} key_components;
key_components *key_components_new(void);
void key_components_add_text(key_components *kc,
                             const char *name, const char *value);
void key_components_add_text_pl(key_components *kc,
                                const char *name, ptrlen value);
void key_components_add_binary(key_components *kc,
                               const char *name, ptrlen value);
void key_components_add_mp(key_components *kc,
                           const char *name, mp_int *value);
void key_components_add_uint(key_components *kc,
                             const char *name, uintmax_t value);
void key_components_add_copy(key_components *kc,
                             const char *name, const key_component *value);
void key_components_free(key_components *kc);

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
RSAKey *BinarySource_get_rsa_ssh1_priv_agent(BinarySource *src);
bool rsa_ssh1_encrypt(unsigned char *data, int length, RSAKey *key);
mp_int *rsa_ssh1_decrypt(mp_int *input, RSAKey *key);
bool rsa_ssh1_decrypt_pkcs1(mp_int *input, RSAKey *key, strbuf *outbuf);
char *rsastr_fmt(RSAKey *key);
char *rsa_ssh1_fingerprint(RSAKey *key);
char **rsa_ssh1_fake_all_fingerprints(RSAKey *key);
bool rsa_verify(RSAKey *key);
void rsa_ssh1_public_blob(BinarySink *bs, RSAKey *key, RsaSsh1Order order);
int rsa_ssh1_public_blob_len(ptrlen data);
void rsa_ssh1_private_blob_agent(BinarySink *bs, RSAKey *key);
void duprsakey(RSAKey *dst, const RSAKey *src);
void freersapriv(RSAKey *key);
void freersakey(RSAKey *key);
key_components *rsa_components(RSAKey *key);
#endif // WINSCP_VS

#ifndef WINSCP_VS
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
 * System for generating k in DSA and ECDSA.
 */
struct RFC6979Result {
    mp_int *k;
    unsigned ok;
};
RFC6979 *rfc6979_new(const ssh_hashalg *hashalg, mp_int *q, mp_int *x);
void rfc6979_setup(RFC6979 *s, ptrlen message);
RFC6979Result rfc6979_attempt(RFC6979 *s);
void rfc6979_free(RFC6979 *s);
mp_int *rfc6979(const ssh_hashalg *hashalg, mp_int *modulus,
                mp_int *private_key, ptrlen message);
#endif

struct ssh_cipher {
    const ssh_cipheralg *vt;
};

#ifndef WINSCP_VS
#ifdef MPEXT
// Resolve ambiguity with OpenSSL
#define SHA_Init putty_SHA_Init
#define SHA_Final putty_SHA_Final
#define SHA256_Init putty_SHA256_Init
#define SHA256_Final putty_SHA256_Final
#define SHA512_Init putty_SHA512_Init
#define SHA512_Final putty_SHA512_Final
#endif

#endif // WINSCP_VS

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
    /* For ciphers that update their state per logical message
     * (typically, per unit independently MACed) */
    void (*next_message)(ssh_cipher *);
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
#define SSH_CIPHER_IS_CBC       1
#define SSH_CIPHER_SEPARATE_LENGTH      2
    const char *text_name;
    /* If set, this takes priority over other MAC. */
    const ssh2_macalg *required_mac;

    /* Pointer to any extra data used by a particular implementation. */
    const void *extra;
};

#ifndef WINSCP_VS

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
{ c->vt->encrypt_length(c, blk, len, seq); }
static inline void ssh_cipher_decrypt_length(
    ssh_cipher *c, void *blk, int len, unsigned long seq)
{ c->vt->decrypt_length(c, blk, len, seq); }
static inline void ssh_cipher_next_message(ssh_cipher *c)
{ c->vt->next_message(c); }
static inline const struct ssh_cipheralg *ssh_cipher_alg(ssh_cipher *c)
{ return c->vt; }

#endif

void nullcipher_next_message(ssh_cipher *);

#ifndef WINSCP_VS

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
    void (*next_message)(ssh2_mac *);
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
static inline void ssh2_mac_next_message(ssh2_mac *m)
{ m->vt->next_message(m); }
static inline const char *ssh2_mac_text_name(ssh2_mac *m)
{ return m->vt->text_name(m); }
static inline const ssh2_macalg *ssh2_mac_alg(ssh2_mac *m)
{ return m->vt; }

/* Centralised 'methods' for ssh2_mac, defined in mac.c. These run
 * the MAC in a specifically SSH-2 style, i.e. taking account of a
 * packet sequence number as well as the data to be authenticated. */
bool ssh2_mac_verresult(ssh2_mac *, const void *);
void ssh2_mac_generate(ssh2_mac *, void *, int, unsigned long seq);
bool ssh2_mac_verify(ssh2_mac *, const void *, int, unsigned long seq);

void nullmac_next_message(ssh2_mac *m);

/* Use a MAC in its raw form, outside SSH-2 context, to MAC a given
 * string with a given key in the most obvious way. */
void mac_simple(const ssh2_macalg *alg, ptrlen key, ptrlen data, void *output);

/* Constructor that makes an HMAC object given just a MAC. This object
 * will have empty 'name' and 'etm_name' fields, so it's not suitable
 * for use in SSH. It's used as a subroutine in RFC 6979. */
ssh2_mac *hmac_new_from_hash(const ssh_hashalg *hash);

#endif // !WINSCP_VS

struct ssh_hash {
    const ssh_hashalg *vt;
    BinarySink_DELEGATE_IMPLEMENTATION;
};

struct ssh_hashalg {
    ssh_hash *(*new)(const ssh_hashalg *alg);
    void (*reset)(ssh_hash *);
    void (*copyfrom)(ssh_hash *dest, ssh_hash *src);
    void (*digest)(ssh_hash *, unsigned char *);
    void (*free)(ssh_hash *);
    size_t hlen; /* output length in bytes */
    size_t blocklen; /* length of the hash's input block, or 0 for N/A */
    const char *text_basename;     /* the semantic name of the hash */
    const char *annotation;   /* extra info, e.g. which of multiple impls */
    const char *text_name;    /* both combined, e.g. "SHA-n (unaccelerated)" */
    const void *extra;        /* private to the hash implementation */
};

static inline ssh_hash *ssh_hash_new(const ssh_hashalg *alg)
{ ssh_hash *h = alg->new(alg); if (h) h->vt->reset(h); return h; }
static inline ssh_hash *ssh_hash_copy(ssh_hash *orig)
{ ssh_hash *h = orig->vt->new(orig->vt); h->vt->copyfrom(h, orig); return h; }
static inline void ssh_hash_digest(ssh_hash *h, unsigned char *out)
{ h->vt->digest(h, out); }
static inline void ssh_hash_free(ssh_hash *h)
{ h->vt->free(h); }
static inline const ssh_hashalg *ssh_hash_alg(ssh_hash *h)
{ return h->vt; }

/* The reset and copyfrom vtable methods return void. But for call-site
 * convenience, these wrappers return their input pointer. */
static inline ssh_hash *ssh_hash_reset(ssh_hash *h)
{ h->vt->reset(h); return h; }
static inline ssh_hash *ssh_hash_copyfrom(ssh_hash *dest, ssh_hash *src)
{ dest->vt->copyfrom(dest, src); return dest; }

/* ssh_hash_final emits the digest _and_ frees the ssh_hash */
static inline void ssh_hash_final(ssh_hash *h, unsigned char *out)
{ h->vt->digest(h, out); h->vt->free(h); }

/* ssh_hash_digest_nondestructive generates a finalised hash from the
 * given object without changing its state, so you can continue
 * appending data to get a hash of an extended string. */
static inline void ssh_hash_digest_nondestructive(ssh_hash *h,
                                                  unsigned char *out)
{ ssh_hash_final(ssh_hash_copy(h), out); }

/* Handy macros for defining all those text-name fields at once */
#define HASHALG_NAMES_BARE(base) \
    /*.text_basename =*/ base, /*.annotation =*/ NULL, /*.text_name =*/ base
#define HASHALG_NAMES_ANNOTATED(base, ann) \
    /*.text_basename =*/ base, /*.annotation =*/ ann, /*.text_name =*/ base " (" ann ")"

#ifndef WINSCP_VS

void hash_simple(const ssh_hashalg *alg, ptrlen data, void *output);

struct ssh_kex {
    const char *name, *groupname;
    enum { KEXTYPE_DH, KEXTYPE_RSA, KEXTYPE_ECDH,
           KEXTYPE_GSS, KEXTYPE_GSS_ECDH } main_type;
    const ssh_hashalg *hash;
    union {                  /* publicly visible data for each type */
        const ecdh_keyalg *ecdh_vt;    /* for KEXTYPE_ECDH, KEXTYPE_GSS_ECDH */
    };
    const void *extra;                 /* private to the kex methods */
};

#ifndef __cplusplus // WINSCP not needed and won't compile, as KEYTYPE_* needs type
static inline bool kex_is_gss(const struct ssh_kex *kex)
{
    return kex->main_type == KEXTYPE_GSS || kex->main_type == KEXTYPE_GSS_ECDH;
}
#endif

struct ssh_kexes {
    int nkexes;
    const ssh_kex *const *list;
};

/* Indices of the negotiation strings in the KEXINIT packet */
enum kexlist {
    KEXLIST_KEX, KEXLIST_HOSTKEY, KEXLIST_CSCIPHER, KEXLIST_SCCIPHER,
    KEXLIST_CSMAC, KEXLIST_SCMAC, KEXLIST_CSCOMP, KEXLIST_SCCOMP,
    NKEXLIST
};

#pragma option push -w-mnc // WINSCP
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
    bool (*has_private) (ssh_key *key);
    char *(*cache_str) (ssh_key *key);
    key_components *(*components) (ssh_key *key);
    ssh_key *(*base_key) (ssh_key *key); /* does not confer ownership */
    /* The following methods can be NULL if !is_certificate */
    void (*ca_public_blob)(ssh_key *key, BinarySink *);
    bool (*check_cert)(ssh_key *key, bool host, ptrlen principal,
                       uint64_t time, const ca_options *opts,
                       BinarySink *error);
    void (*cert_id_string)(ssh_key *key, BinarySink *);
    SeatDialogText *(*cert_info)(ssh_key *key);

    /* 'Class methods' that don't deal with an ssh_key at all */
    int (*pubkey_bits) (const ssh_keyalg *self, ptrlen blob);
    unsigned (*supported_flags) (const ssh_keyalg *self);
    const char *(*alternate_ssh_id) (const ssh_keyalg *self, unsigned flags);
    char *(*alg_desc)(const ssh_keyalg *self);
    bool (*variable_size)(const ssh_keyalg *self);
    /* The following methods can be NULL if !is_certificate */
    const ssh_keyalg *(*related_alg)(const ssh_keyalg *self,
                                     const ssh_keyalg *base);

    /* Constant data fields giving information about the key type */
    const char *ssh_id;    /* string identifier in the SSH protocol */
    const char *cache_id;  /* identifier used in PuTTY's host key cache */
    const void *extra;     /* private to the public key methods */
    bool is_certificate;   /* is this a certified key type? */
    const ssh_keyalg *base_alg; /* if so, for what underlying key alg? */
};
#pragma option pop // WINSCP

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
static inline bool ssh_key_has_private(ssh_key *key)
{ return key->vt->has_private(key); }
static inline char *ssh_key_cache_str(ssh_key *key)
{ return key->vt->cache_str(key); }
static inline key_components *ssh_key_components(ssh_key *key)
{ return key->vt->components(key); }
static inline ssh_key *ssh_key_base_key(ssh_key *key)
{ return key->vt->base_key(key); }
static inline void ssh_key_ca_public_blob(ssh_key *key, BinarySink *bs)
{ key->vt->ca_public_blob(key, bs); }
static inline void ssh_key_cert_id_string(ssh_key *key, BinarySink *bs)
{ key->vt->cert_id_string(key, bs); }
static inline SeatDialogText *ssh_key_cert_info(ssh_key *key)
{ return key->vt->cert_info(key); }
static inline bool ssh_key_check_cert(
    ssh_key *key, bool host, ptrlen principal, uint64_t time,
    const ca_options *opts, BinarySink *error)
{ return key->vt->check_cert(key, host, principal, time, opts, error); }
static inline int ssh_key_public_bits(const ssh_keyalg *self, ptrlen blob)
{ return self->pubkey_bits(self, blob); }
static inline const ssh_keyalg *ssh_key_alg(ssh_key *key)
{ return key->vt; }
static inline const char *ssh_key_ssh_id(ssh_key *key)
{ return key->vt->ssh_id; }
static inline const char *ssh_key_cache_id(ssh_key *key)
{ return key->vt->cache_id; }
static inline unsigned ssh_key_supported_flags(ssh_key *key)
{ return key->vt->supported_flags(key->vt); }
static inline unsigned ssh_keyalg_supported_flags(const ssh_keyalg *self)
{ return self->supported_flags(self); }
static inline const char *ssh_keyalg_alternate_ssh_id(
    const ssh_keyalg *self, unsigned flags)
{ return self->alternate_ssh_id(self, flags); }
static inline char *ssh_keyalg_desc(const ssh_keyalg *self)
{ return self->alg_desc(self); }
static inline bool ssh_keyalg_variable_size(const ssh_keyalg *self)
{ return self->variable_size(self); }
static inline const ssh_keyalg *ssh_keyalg_related_alg(
    const ssh_keyalg *self, const ssh_keyalg *base)
{ return self->related_alg(self, base); }

/* Stub functions shared between multiple key types */
unsigned nullkey_supported_flags(const ssh_keyalg *self);
const char *nullkey_alternate_ssh_id(const ssh_keyalg *self, unsigned flags);
ssh_key *nullkey_base_key(ssh_key *key);
bool nullkey_variable_size_no(const ssh_keyalg *self);
bool nullkey_variable_size_yes(const ssh_keyalg *self);

/* Utility functions implemented centrally */
ssh_key *ssh_key_clone(ssh_key *key);

/*
 * SSH2 ECDH key exchange vtable
 */
struct ecdh_key {
    const ecdh_keyalg *vt;
};
struct ecdh_keyalg {
    /* Unusually, the 'new' method here doesn't directly take a vt
     * pointer, because it will also need the containing ssh_kex
     * structure for top-level parameters, and since that contains a
     * vt pointer anyway, we might as well _only_ pass that. */
    ecdh_key *(*new)(const ssh_kex *kex, bool is_server);
    void (*free)(ecdh_key *key);
    void (*getpublic)(ecdh_key *key, BinarySink *bs);
    bool (*getkey)(ecdh_key *key, ptrlen remoteKey, BinarySink *bs);
    char *(*description)(const ssh_kex *kex);

    /* Some things that use this vtable are genuinely elliptic-curve
     * Diffie-Hellman. Others are hybrid PQ + classical kex methods.
     * Provide a packet-naming context for use in the SSH log. (Purely
     * cosmetic.) */
    Pkt_KCtx packet_naming_ctx;
};
static inline ecdh_key *ecdh_key_new(const ssh_kex *kex, bool is_server)
{ return kex->ecdh_vt->new(kex, is_server); }
static inline void ecdh_key_free(ecdh_key *key)
{ key->vt->free(key); }
static inline void ecdh_key_getpublic(ecdh_key *key, BinarySink *bs)
{ key->vt->getpublic(key, bs); }
static inline bool ecdh_key_getkey(ecdh_key *key, ptrlen remoteKey,
                                   BinarySink *bs)
{ return key->vt->getkey(key, remoteKey, bs); }
static inline char *ecdh_keyalg_description(const ssh_kex *kex)
{ return kex->ecdh_vt->description(kex); }

/*
 * vtable for post-quantum key encapsulation methods (things like NTRU
 * and ML-KEM).
 *
 * These work in an asymmetric way that's conceptually more like the
 * old RSA kex (either SSH-1 or SSH-2) than like Diffie-Hellman. One
 * party generates a keypair and sends the public key; the other party
 * invents a secret and encrypts it with the public key; the first
 * party receives the ciphertext and decrypts it, and now both parties
 * have the secret.
 */
struct pq_kem_dk {
    const pq_kemalg *vt;
};
struct pq_kemalg {
    /* Generate a key pair, writing the public encryption key in wire
     * format to ek. Return the decryption key. */
    pq_kem_dk *(*keygen)(const pq_kemalg *alg, BinarySink *ek);
    /* Invent and encrypt a secret, writing the ciphertext in wire
     * format to c and the secret itself to k. Returns false on any
     * kind of really obvious validation failure of the encryption key. */
    bool (*encaps)(const pq_kemalg *alg, BinarySink *c, BinarySink *k,
                   ptrlen ek);
    /* Decrypt the secret and write it to k. Returns false on
     * validation failure. However, more competent cryptographic
     * attacks are rejected in a way that's not obvious, returning a
     * useless pseudorandom secret. */
    bool (*decaps)(pq_kem_dk *dk, BinarySink *k, ptrlen c);
    /* Free a decryption key. */
    void (*free_dk)(pq_kem_dk *dk);

    const void *extra;
    const char *description;
    size_t ek_len, c_len;
};

static inline pq_kem_dk *pq_kem_keygen(const pq_kemalg *alg, BinarySink *ek)
{ return alg->keygen(alg, ek); }
static inline bool pq_kem_encaps(const pq_kemalg *alg, BinarySink *c,
                                 BinarySink *k, ptrlen ek)
{ return alg->encaps(alg, c, k, ek); }
static inline bool pq_kem_decaps(pq_kem_dk *dk, BinarySink *k, ptrlen c)
{ return dk->vt->decaps(dk, k, c); }
static inline void pq_kem_free_dk(pq_kem_dk *dk)
{ dk->vt->free_dk(dk); }

/*
 * Suffix on GSSAPI SSH protocol identifiers that indicates Kerberos 5
 * as the mechanism.
 *
 * This suffix is the base64-encoded MD5 hash of the byte sequence
 * 06 09 2A 86 48 86 F7 12 01 02 02, which in turn is the ASN.1 DER
 * encoding of the object ID 1.2.840.113554.1.2.2 which designates
 * Kerberos v5.
 *
 * (The same encoded OID, minus the two-byte DER header, is defined in
 * ssh/pgssapi.c as GSS_MECH_KRB5.)
 */
#define GSS_KRB5_OID_HASH "toWM5Slw5Ew8Mqkay+al2g=="

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
    char *comment;                     /* the key comment */
};

/* Argon2 password hashing function */
typedef enum { Argon2d = 0, Argon2i = 1, Argon2id = 2 } Argon2Flavour;
void argon2(Argon2Flavour, uint32_t mem, uint32_t passes,
            uint32_t parallel, uint32_t taglen,
            ptrlen P, ptrlen S, ptrlen K, ptrlen X, strbuf *out);
void argon2_choose_passes(
    Argon2Flavour, uint32_t mem, uint32_t milliseconds, uint32_t *passes,
    uint32_t parallel, uint32_t taglen, ptrlen P, ptrlen S, ptrlen K, ptrlen X,
    strbuf *out);
/* The H' hash defined in Argon2, exposed just for testcrypt */
strbuf *argon2_long_hash(unsigned length, ptrlen data);

/* The maximum length of any hash algorithm. (bytes) */
#define MAX_HASH_LEN (114) /* longest is SHAKE256 with 114-byte output */

extern const ssh_cipheralg ssh_3des_ssh1;
extern const ssh_cipheralg ssh_blowfish_ssh1;
extern const ssh_cipheralg ssh_3des_ssh2_ctr;
extern const ssh_cipheralg ssh_3des_ssh2;
extern const ssh_cipheralg ssh_des;
extern const ssh_cipheralg ssh_des_sshcom_ssh2;
extern const ssh_cipheralg ssh_aes256_sdctr;
extern const ssh_cipheralg ssh_aes256_sdctr_ni;
extern const ssh_cipheralg ssh_aes256_sdctr_neon;
extern const ssh_cipheralg ssh_aes256_sdctr_sw;
extern const ssh_cipheralg ssh_aes256_gcm;
extern const ssh_cipheralg ssh_aes256_gcm_ni;
extern const ssh_cipheralg ssh_aes256_gcm_neon;
extern const ssh_cipheralg ssh_aes256_gcm_sw;
extern const ssh_cipheralg ssh_aes256_cbc;
extern const ssh_cipheralg ssh_aes256_cbc_ni;
extern const ssh_cipheralg ssh_aes256_cbc_neon;
extern const ssh_cipheralg ssh_aes256_cbc_sw;
extern const ssh_cipheralg ssh_aes192_sdctr;
extern const ssh_cipheralg ssh_aes192_sdctr_ni;
extern const ssh_cipheralg ssh_aes192_sdctr_neon;
extern const ssh_cipheralg ssh_aes192_sdctr_sw;
extern const ssh_cipheralg ssh_aes192_gcm;
extern const ssh_cipheralg ssh_aes192_gcm_ni;
extern const ssh_cipheralg ssh_aes192_gcm_neon;
extern const ssh_cipheralg ssh_aes192_gcm_sw;
extern const ssh_cipheralg ssh_aes192_cbc;
extern const ssh_cipheralg ssh_aes192_cbc_ni;
extern const ssh_cipheralg ssh_aes192_cbc_neon;
extern const ssh_cipheralg ssh_aes192_cbc_sw;
extern const ssh_cipheralg ssh_aes128_sdctr;
extern const ssh_cipheralg ssh_aes128_sdctr_ni;
extern const ssh_cipheralg ssh_aes128_sdctr_neon;
extern const ssh_cipheralg ssh_aes128_sdctr_sw;
extern const ssh_cipheralg ssh_aes128_gcm;
extern const ssh_cipheralg ssh_aes128_gcm_ni;
extern const ssh_cipheralg ssh_aes128_gcm_neon;
extern const ssh_cipheralg ssh_aes128_gcm_sw;
extern const ssh_cipheralg ssh_aes128_cbc;
extern const ssh_cipheralg ssh_aes128_cbc_ni;
extern const ssh_cipheralg ssh_aes128_cbc_neon;
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
extern const ssh2_ciphers ssh2_aesgcm;
extern const ssh_hashalg ssh_md5;
extern const ssh_hashalg ssh_sha1;
extern const ssh_hashalg ssh_sha1_ni;
extern const ssh_hashalg ssh_sha1_neon;
extern const ssh_hashalg ssh_sha1_sw;
extern const ssh_hashalg ssh_sha256;
extern const ssh_hashalg ssh_sha256_ni;
extern const ssh_hashalg ssh_sha256_neon;
extern const ssh_hashalg ssh_sha256_sw;
extern const ssh_hashalg ssh_sha384;
extern const ssh_hashalg ssh_sha384_neon;
extern const ssh_hashalg ssh_sha384_sw;
extern const ssh_hashalg ssh_sha512;
extern const ssh_hashalg ssh_sha512_neon;
extern const ssh_hashalg ssh_sha512_sw;
extern const ssh_hashalg ssh_sha3_224;
extern const ssh_hashalg ssh_sha3_256;
extern const ssh_hashalg ssh_sha3_384;
extern const ssh_hashalg ssh_sha3_512;
extern const ssh_hashalg ssh_shake256_32bytes;
extern const ssh_hashalg ssh_shake256_114bytes;
extern const ssh_hashalg ssh_blake2b;
extern const ssh_kexes ssh_diffiehellman_group1;
extern const ssh_kexes ssh_diffiehellman_group14;
extern const ssh_kexes ssh_diffiehellman_group15;
extern const ssh_kexes ssh_diffiehellman_group16;
extern const ssh_kexes ssh_diffiehellman_group17;
extern const ssh_kexes ssh_diffiehellman_group18;
extern const ssh_kexes ssh_diffiehellman_gex;
extern const ssh_kex ssh_diffiehellman_group1_sha1;
extern const ssh_kex ssh_diffiehellman_group14_sha256;
extern const ssh_kex ssh_diffiehellman_group14_sha1;
extern const ssh_kex ssh_diffiehellman_group15_sha512;
extern const ssh_kex ssh_diffiehellman_group16_sha512;
extern const ssh_kex ssh_diffiehellman_group17_sha512;
extern const ssh_kex ssh_diffiehellman_group18_sha512;
extern const ssh_kexes ssh_gssk5_sha1_kex;
extern const ssh_kexes ssh_gssk5_sha2_kex;
extern const ssh_kexes ssh_gssk5_ecdh_kex;
extern const ssh_kexes ssh_rsa_kex;
extern const ssh_kex ssh_ec_kex_curve25519;
extern const ssh_kex ssh_ec_kex_curve448;
extern const ssh_kex ssh_ec_kex_nistp256;
extern const ssh_kex ssh_ec_kex_nistp384;
extern const ssh_kex ssh_ec_kex_nistp521;
extern const ssh_kexes ssh_ecdh_kex;
extern const ssh_kexes ssh_ntru_hybrid_kex;
extern const pq_kemalg ssh_ntru;
extern const ssh_kexes ssh_mlkem_curve25519_hybrid_kex;
extern const ssh_kexes ssh_mlkem_nist_hybrid_kex;
extern const pq_kemalg ssh_mlkem512;
extern const pq_kemalg ssh_mlkem768;
extern const pq_kemalg ssh_mlkem1024;
extern const ssh_keyalg ssh_dsa;
extern const ssh_keyalg ssh_rsa;
extern const ssh_keyalg ssh_rsa_sha256;
extern const ssh_keyalg ssh_rsa_sha512;
extern const ssh_keyalg ssh_ecdsa_ed25519;
extern const ssh_keyalg ssh_ecdsa_ed448;
extern const ssh_keyalg ssh_ecdsa_nistp256;
extern const ssh_keyalg ssh_ecdsa_nistp384;
extern const ssh_keyalg ssh_ecdsa_nistp521;
extern const ssh_keyalg opensshcert_ssh_dsa;
extern const ssh_keyalg opensshcert_ssh_rsa;
extern const ssh_keyalg opensshcert_ssh_rsa_sha256;
extern const ssh_keyalg opensshcert_ssh_rsa_sha512;
extern const ssh_keyalg opensshcert_ssh_ecdsa_ed25519;
extern const ssh_keyalg opensshcert_ssh_ecdsa_nistp256;
extern const ssh_keyalg opensshcert_ssh_ecdsa_nistp384;
extern const ssh_keyalg opensshcert_ssh_ecdsa_nistp521;
extern const ssh2_macalg ssh_hmac_md5;
extern const ssh2_macalg ssh_hmac_sha1;
extern const ssh2_macalg ssh_hmac_sha1_buggy;
extern const ssh2_macalg ssh_hmac_sha1_96;
extern const ssh2_macalg ssh_hmac_sha1_96_buggy;
extern const ssh2_macalg ssh_hmac_sha256;
extern const ssh2_macalg ssh_hmac_sha384;
extern const ssh2_macalg ssh_hmac_sha512;
extern const ssh2_macalg ssh2_poly1305;
#endif
extern const ssh2_macalg ssh2_aesgcm_mac;
#ifndef WINSCP_VS
extern const ssh2_macalg ssh2_aesgcm_mac_sw;
extern const ssh2_macalg ssh2_aesgcm_mac_ref_poly;
extern const ssh2_macalg ssh2_aesgcm_mac_clmul;
extern const ssh2_macalg ssh2_aesgcm_mac_neon;
extern const ssh_compression_alg ssh_zlib;

/* Special constructor: BLAKE2b can be instantiated with any hash
 * length up to 128 bytes */
ssh_hash *blake2b_new_general(unsigned hashlen);

/* Special test function for AES-GCM */
void aesgcm_set_prefix_lengths(ssh2_mac *mac, size_t skip, size_t aad);

/* Shake128/256 extendable output functions (like a hash except you don't
 * commit up front to how much data you want to get out of it) */
ShakeXOF *shake128_xof_from_input(ptrlen data);
ShakeXOF *shake256_xof_from_input(ptrlen data);
void shake_xof_read(ShakeXOF *sx, void *output_v, size_t size);
void shake_xof_free(ShakeXOF *sx);

/*
 * On some systems, you have to detect hardware crypto acceleration by
 * asking the local OS API rather than OS-agnostically asking the CPU
 * itself. If so, then this function should be implemented in each
 * platform subdirectory.
 */
bool platform_aes_neon_available(void);
bool platform_pmull_neon_available(void);
bool platform_sha256_neon_available(void);
bool platform_sha1_neon_available(void);
bool platform_sha512_neon_available(void);
bool platform_dit_available(void);

/*
 * PuTTY version number formatted as an SSH version string.
 */
extern
#ifndef MPEXT
  const
#endif
  char sshver[];

/*
 * Gross hack: pscp will try to start SFTP but fall back to scp1 if
 * that fails. This variable is the means by which pscp.c can reach
 * into the SSH code and find out which one it got.
 */
extern bool ssh_fallback_cmd(Backend *backend);

/*
 * The PRNG type, defined in prng.c. Visible data fields are
 * 'savesize', which suggests how many random bytes you should request
 * from a particular PRNG instance to write to putty.rnd, and a
 * BinarySink implementation which you can use to write seed data in
 * between calling prng_seed_{begin,finish}.
 */
struct prng {
    size_t savesize;
    BinarySink_IMPLEMENTATION;
    /* (also there's a surrounding implementation struct in prng.c) */
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
/* X11-related helper functions in utils */
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
                                Filename *authfilename);
void x11_format_auth_for_authfile(
    BinarySink *bs, SockAddr *addr, int display_no,
    ptrlen authproto, ptrlen authdata);
void *x11_make_greeting(int endian, int protomajor, int protominor,
                        int auth_proto, const void *auth_data, int auth_len,
                        const char *peer_ip, int peer_port,
                        int *outlen);
int x11_identify_auth_proto(ptrlen protoname);
void *x11_dehexify(ptrlen hex, int *outlen);
bool x11_parse_ip(const char *addr_string, unsigned long *ip);

struct callback_set;
Channel *agentf_new(SshChannel *c, struct callback_set *callback_set); // WINSCP

bool dh_is_gex(const ssh_kex *kex);
dh_ctx *dh_setup_group(const ssh_kex *kex);
dh_ctx *dh_setup_gex(mp_int *pval, mp_int *gval);
int dh_modulus_bit_size(const dh_ctx *ctx);
void dh_cleanup(dh_ctx *);
mp_int *dh_create_e(dh_ctx *);
const char *dh_validate_f(dh_ctx *, mp_int *f);
mp_int *dh_find_K(dh_ctx *, mp_int *f);

static inline bool is_base64_char(char c)
{
    return ((c >= '0' && c <= '9') ||
            (c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            c == '+' || c == '/' || c == '=');
}

extern int base64_lines(int datalen);

/* ppk_load_* can return this as an error */
extern ssh2_userkey ssh2_wrong_passphrase;
#define SSH2_WRONG_PASSPHRASE (&ssh2_wrong_passphrase)

bool ppk_encrypted_s(BinarySource *src, char **comment);
bool ppk_encrypted_f(const Filename *filename, char **comment);
bool rsa1_encrypted_s(BinarySource *src, char **comment);
bool rsa1_encrypted_f(const Filename *filename, char **comment);

ssh2_userkey *ppk_load_s(BinarySource *src, const char *passphrase,
                         const char **errorstr);
ssh2_userkey *ppk_load_f(const Filename *filename, const char *passphrase,
                         const char **errorstr);
int rsa1_load_s(BinarySource *src, RSAKey *key,
                const char *passphrase, const char **errorstr);
int rsa1_load_f(const Filename *filename, RSAKey *key,
                const char *passphrase, const char **errorstr);

typedef struct ppk_save_parameters {
    unsigned fmt_version;              /* currently 2 or 3 */

    /*
     * Parameters for fmt_version == 3
     */
    Argon2Flavour argon2_flavour;
    uint32_t argon2_mem;               /* in Kbyte */
    bool argon2_passes_auto;
    union {
        uint32_t argon2_passes;        /* if auto == false */
        uint32_t argon2_milliseconds;  /* if auto == true */
    };
    uint32_t argon2_parallelism;

    /* The ability to choose a specific salt is only intended for the
     * use of the automated test of PuTTYgen. It's a (mild) security
     * risk to do it with any passphrase you actually care about,
     * because it invalidates the entire point of having a salt in the
     * first place. */
    const uint8_t *salt;
    size_t saltlen;
} ppk_save_parameters;
extern const ppk_save_parameters ppk_save_default_parameters;

strbuf *ppk_save_sb(ssh2_userkey *key, const char *passphrase,
                    const ppk_save_parameters *params);
bool ppk_save_f(const Filename *filename, ssh2_userkey *key,
                const char *passphrase, const ppk_save_parameters *params);
strbuf *rsa1_save_sb(RSAKey *key, const char *passphrase);
bool rsa1_save_f(const Filename *filename, RSAKey *key,
                 const char *passphrase);

bool ppk_loadpub_s(BinarySource *src, char **algorithm, BinarySink *bs,
                   char **commentptr, const char **errorstr);
bool ppk_loadpub_f(const Filename *filename, char **algorithm, BinarySink *bs,
                   char **commentptr, const char **errorstr);
int rsa1_loadpub_s(BinarySource *src, BinarySink *bs,
                   char **commentptr, const char **errorstr);
int rsa1_loadpub_f(const Filename *filename, BinarySink *bs,
                   char **commentptr, const char **errorstr);

extern const ssh_keyalg *const all_keyalgs[];
extern const size_t n_keyalgs;
const ssh_keyalg *find_pubkey_alg(const char *name);
const ssh_keyalg *find_pubkey_alg_len(ptrlen name);

ptrlen pubkey_blob_to_alg_name(ptrlen blob);
const ssh_keyalg *pubkey_blob_to_alg(ptrlen blob);

/* Convenient wrappers on the LoadedFile mechanism suitable for key files */
LoadedFile *lf_load_keyfile(const Filename *filename, const char **errptr);
LoadedFile *lf_load_keyfile_fp(FILE *fp, const char **errptr);

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

typedef enum {
    /* Default fingerprint types strip off a certificate to show you
     * the fingerprint of the underlying public key */
    SSH_FPTYPE_MD5,
    SSH_FPTYPE_SHA256,
    /* Non-default version of each fingerprint type which is 'raw',
     * giving you the true hash of the public key blob even if it
     * includes a certificate */
    SSH_FPTYPE_MD5_CERT,
    SSH_FPTYPE_SHA256_CERT,
} FingerprintType;

static inline bool ssh_fptype_is_cert(FingerprintType fptype)
{
    return fptype >= SSH_FPTYPE_MD5_CERT;
}
#ifndef __cplusplus // WINSCP not needed and won't compile
static inline FingerprintType ssh_fptype_from_cert(FingerprintType fptype)
{
    if (ssh_fptype_is_cert(fptype))
        fptype -= (SSH_FPTYPE_MD5_CERT - SSH_FPTYPE_MD5);
    return fptype;
}
static inline FingerprintType ssh_fptype_to_cert(FingerprintType fptype)
{
    if (!ssh_fptype_is_cert(fptype))
        fptype += (SSH_FPTYPE_MD5_CERT - SSH_FPTYPE_MD5);
    return fptype;
}
#endif

#define SSH_N_FPTYPES (SSH_FPTYPE_SHA256_CERT + 1)
#define SSH_FPTYPE_DEFAULT SSH_FPTYPE_SHA256

FingerprintType ssh2_pick_fingerprint(char **fingerprints,
                                      FingerprintType preferred_type);
FingerprintType ssh2_pick_default_fingerprint(char **fingerprints);

char *ssh1_pubkey_str(RSAKey *ssh1key);
void ssh1_write_pubkey(FILE *fp, RSAKey *ssh1key);
char *ssh2_pubkey_openssh_str(ssh2_userkey *key);
void ssh2_write_pubkey(FILE *fp, const char *comment,
                       const void *v_pub_blob, int pub_len,
                       int keytype);
char *ssh2_fingerprint_blob(ptrlen, FingerprintType);
char *ssh2_fingerprint(ssh_key *key, FingerprintType);
char *ssh2_double_fingerprint_blob(ptrlen, FingerprintType);
char *ssh2_double_fingerprint(ssh_key *key, FingerprintType);
char **ssh2_all_fingerprints_for_blob(ptrlen);
char **ssh2_all_fingerprints(ssh_key *key);
void ssh2_free_all_fingerprints(char **);
int key_type(const Filename *filename);
int key_type_s(BinarySource *src);
const char *key_type_to_str(int type);
bool openssh_loadpub(BinarySource *src, char **algorithm, // WINSCP
                     BinarySink *bs,
                     char **commentptr, const char **errorstr);

bool import_possible(int type);
int import_target_type(int type);
bool import_encrypted(const Filename *filename, int type, char **comment);
bool import_encrypted_s(const Filename *filename, BinarySource *src,
                        int type, char **comment);
int import_ssh1(const Filename *filename, int type,
                RSAKey *key, char *passphrase, const char **errmsg_p);
int import_ssh1_s(BinarySource *src, int type,
                  RSAKey *key, char *passphrase, const char **errmsg_p);
ssh2_userkey *import_ssh2(const Filename *filename, int type,
                          char *passphrase, const char **errmsg_p);
ssh2_userkey *import_ssh2_s(BinarySource *src, int type,
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
void aes256_encrypt_pubkey(const void *key, const void *iv,
                           void *blk, int len);
void aes256_decrypt_pubkey(const void *key, const void *iv,
                           void *blk, int len);

void des_encrypt_xdmauth(const void *key, void *blk, int len);
void des_decrypt_xdmauth(const void *key, void *blk, int len);

void openssh_bcrypt(ptrlen passphrase, ptrlen salt,
                    int rounds, unsigned char *out, int outbytes);

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

#define SSH1_AUTH_RHOSTS                          1     /* 0x1 */
#define SSH1_AUTH_RSA                             2     /* 0x2 */
#define SSH1_AUTH_PASSWORD                        3     /* 0x3 */
#define SSH1_AUTH_RHOSTS_RSA                      4     /* 0x4 */
#define SSH1_AUTH_TIS                             5     /* 0x5 */
#define SSH1_AUTH_CCARD                           16    /* 0x10 */

#define SSH1_PROTOFLAG_SCREEN_NUMBER              1     /* 0x1 */
/* Mask for protoflags we will echo back to server if seen */
#define SSH1_PROTOFLAGS_SUPPORTED                 0     /* 0x1 */

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
    X(y, SSH2_MSG_EXT_INFO, 7)                                          \
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
    K(y, SSH2_MSG_KEX_HYBRID_INIT, 30, SSH2_PKTCTX_HYBRIDKEX)           \
    K(y, SSH2_MSG_KEX_HYBRID_REPLY, 31, SSH2_PKTCTX_HYBRIDKEX)          \
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
#define SSH1_AGENTC_REMOVE_ALL_RSA_IDENTITIES 9 /* openssh private? */

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
#define SSH2_AGENTC_EXTENSION                   27
#define SSH_AGENT_EXTENSION_FAILURE             28

/*
 * Assorted other SSH-related enumerations.
 */
#define SSH2_DISCONNECT_HOST_NOT_ALLOWED_TO_CONNECT 1   /* 0x1 */
#define SSH2_DISCONNECT_PROTOCOL_ERROR            2     /* 0x2 */
#define SSH2_DISCONNECT_KEY_EXCHANGE_FAILED       3     /* 0x3 */
#define SSH2_DISCONNECT_HOST_AUTHENTICATION_FAILED 4    /* 0x4 */
#define SSH2_DISCONNECT_MAC_ERROR                 5     /* 0x5 */
#define SSH2_DISCONNECT_COMPRESSION_ERROR         6     /* 0x6 */
#define SSH2_DISCONNECT_SERVICE_NOT_AVAILABLE     7     /* 0x7 */
#define SSH2_DISCONNECT_PROTOCOL_VERSION_NOT_SUPPORTED 8        /* 0x8 */
#define SSH2_DISCONNECT_HOST_KEY_NOT_VERIFIABLE   9     /* 0x9 */
#define SSH2_DISCONNECT_CONNECTION_LOST           10    /* 0xa */
#define SSH2_DISCONNECT_BY_APPLICATION            11    /* 0xb */
#define SSH2_DISCONNECT_TOO_MANY_CONNECTIONS      12    /* 0xc */
#define SSH2_DISCONNECT_AUTH_CANCELLED_BY_USER    13    /* 0xd */
#define SSH2_DISCONNECT_NO_MORE_AUTH_METHODS_AVAILABLE 14       /* 0xe */
#define SSH2_DISCONNECT_ILLEGAL_USER_NAME         15    /* 0xf */

#define SSH2_OPEN_ADMINISTRATIVELY_PROHIBITED     1     /* 0x1 */
#define SSH2_OPEN_CONNECT_FAILED                  2     /* 0x2 */
#define SSH2_OPEN_UNKNOWN_CHANNEL_TYPE            3     /* 0x3 */
#define SSH2_OPEN_RESOURCE_SHORTAGE               4     /* 0x4 */

#define SSH2_EXTENDED_DATA_STDERR                 1     /* 0x1 */

enum {
    /* TTY modes with opcodes defined consistently in the SSH specs. */
    #define TTYMODE_CHAR(name, val, index) SSH_TTYMODE_##name = val,
    #define TTYMODE_FLAG(name, val, field, mask) SSH_TTYMODE_##name = val,
    #include "ssh/ttymode-list.h"
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
    X(BUG_REQUIRES_FILTERED_KEXINIT)            \
    X(BUG_RSA_SHA2_CERT_USERAUTH)               \
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
void add_to_commasep_pl(strbuf *buf, ptrlen data);
bool get_commasep_word(ptrlen *list, ptrlen *word);

/* Reasons why something warned by confirm_weak_crypto_primitive might
 * be considered weak */
typedef enum WeakCryptoReason {
    WCR_BELOW_THRESHOLD, /* user has told us to consider it weak */
    WCR_TERRAPIN,        /* known vulnerability CVE-2023-48795 */
    WCR_TERRAPIN_AVOIDABLE, /* same, but demoting ChaCha20 can avoid it */
} WeakCryptoReason;

SeatPromptResult verify_ssh_host_key(
    InteractionReadySeat iseat, Conf *conf, const char *host, int port,
    ssh_key *key, const char *keytype, char *keystr, const char *keydisp,
    char **fingerprints, int ca_count,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);
SeatPromptResult confirm_weak_crypto_primitive(
    InteractionReadySeat iseat, const char *algtype, const char *algname,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx,
    WeakCryptoReason wcr);
SeatPromptResult confirm_weak_cached_hostkey(
    InteractionReadySeat iseat, const char *algname, const char **betteralgs,
    void (*callback)(void *ctx, SeatPromptResult result), void *ctx);

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

#endif // WINSCP_VS

/*
 * Protocol definitions for authentication helper plugins
 */

#define AUTHPLUGIN_MSG_NAMES(X)                 \
    X(PLUGIN_INIT, 1)                           \
    X(PLUGIN_INIT_RESPONSE, 2)                  \
    X(PLUGIN_PROTOCOL, 3)                       \
    X(PLUGIN_PROTOCOL_ACCEPT, 4)                \
    X(PLUGIN_PROTOCOL_REJECT, 5)                \
    X(PLUGIN_AUTH_SUCCESS, 6)                   \
    X(PLUGIN_AUTH_FAILURE, 7)                   \
    X(PLUGIN_INIT_FAILURE, 8)                   \
    X(PLUGIN_KI_SERVER_REQUEST, 20)             \
    X(PLUGIN_KI_SERVER_RESPONSE, 21)            \
    X(PLUGIN_KI_USER_REQUEST, 22)               \
    X(PLUGIN_KI_USER_RESPONSE, 23)              \
    /* end of list */

#define PLUGIN_PROTOCOL_MAX_VERSION 2  /* the highest version we speak */

enum {
    #define ENUMDECL(name, value) name = value,
    AUTHPLUGIN_MSG_NAMES(ENUMDECL)
    #undef ENUMDECL

    /* Error codes internal to this implementation, indicating failure
     * to receive a meaningful packet at all */
    PLUGIN_NOTYPE = 256, /* packet too short to have a type */
    PLUGIN_EOF = 257 /* EOF from auth plugin */
};

/*
 * CPU features for security
 */

#if HAVE_ARM_DIT
void enable_dit(void);
#else
#define enable_dit() ((void)0)
#endif
