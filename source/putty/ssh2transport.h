/*
 * Header connecting the pieces of the SSH-2 transport layer.
 */

#ifndef PUTTY_SSH2TRANSPORT_H
#define PUTTY_SSH2TRANSPORT_H

#ifndef NO_GSSAPI
#include "sshgssc.h"
#include "sshgss.h"
#define MIN_CTXT_LIFETIME 5     /* Avoid rekey with short lifetime (seconds) */
#define GSS_KEX_CAPABLE (1<<0)  /* Can do GSS KEX */
#define GSS_CRED_UPDATED (1<<1) /* Cred updated since previous delegation */
#define GSS_CTXT_EXPIRES (1<<2) /* Context expires before next timer */
#define GSS_CTXT_MAYFAIL (1<<3) /* Context may expire during handshake */
#endif

#define DH_MIN_SIZE 1024
#define DH_MAX_SIZE 8192

enum kexlist {
    KEXLIST_KEX, KEXLIST_HOSTKEY, KEXLIST_CSCIPHER, KEXLIST_SCCIPHER,
    KEXLIST_CSMAC, KEXLIST_SCMAC, KEXLIST_CSCOMP, KEXLIST_SCCOMP,
    NKEXLIST
};
#define MAXKEXLIST 16
struct kexinit_algorithm {
    const char *name;
    union {
        struct {
            const ssh_kex *kex;
            bool warn;
        } kex;
        struct {
            const ssh_keyalg *hostkey;
            bool warn;
        } hk;
        struct {
            const ssh_cipheralg *cipher;
            bool warn;
        } cipher;
        struct {
            const ssh2_macalg *mac;
            bool etm;
        } mac;
        struct {
            const ssh_compression_alg *comp;
            bool delayed;
        } comp;
    } u;
};

#define HOSTKEY_ALGORITHMS(X)                   \
    X(HK_ED25519, ssh_ecdsa_ed25519)            \
    X(HK_ECDSA, ssh_ecdsa_nistp256)             \
    X(HK_ECDSA, ssh_ecdsa_nistp384)             \
    X(HK_ECDSA, ssh_ecdsa_nistp521)             \
    X(HK_DSA, ssh_dss)                          \
    X(HK_RSA, ssh_rsa)                          \
    /* end of list */
#define COUNT_HOSTKEY_ALGORITHM(type, alg) +1
#define N_HOSTKEY_ALGORITHMS (0 HOSTKEY_ALGORITHMS(COUNT_HOSTKEY_ALGORITHM))

struct ssh_signkey_with_user_pref_id {
    const ssh_keyalg *alg;
    int id;
};
extern const struct ssh_signkey_with_user_pref_id
    ssh2_hostkey_algs[N_HOSTKEY_ALGORITHMS];

/*
 * Enumeration of high-level classes of reason why we might need to do
 * a repeat key exchange. A full detailed reason in human-readable
 * string form for the Event Log is also provided, but this enum type
 * is used to discriminate between classes of reason that the code
 * needs to treat differently.
 *
 * RK_NONE == 0 is the value indicating that no rekey is currently
 * needed at all. RK_INITIAL indicates that we haven't even done the
 * _first_ key exchange yet. RK_SERVER indicates that we're rekeying
 * because the server asked for it, not because we decided it
 * ourselves. RK_NORMAL is the usual case. RK_GSS_UPDATE indicates
 * that we're rekeying because we've just got new GSSAPI credentials
 * (hence there's no point in doing a preliminary check for new GSS
 * creds, because we already know the answer); RK_POST_USERAUTH
 * indicates that _if_ we're going to need a post-userauth immediate
 * rekey for any reason, this is the moment to do it.
 *
 * So RK_POST_USERAUTH only tells the transport layer to _consider_
 * rekeying, not to definitely do it. Also, that one enum value is
 * special in that the user-readable reason text is passed in to the
 * transport layer as NULL, whereas fills in the reason text after it
 * decides whether it needs a rekey at all. In the other cases,
 * rekey_reason is passed in to the at the same time as rekey_class.
 */
typedef enum RekeyClass {
    RK_NONE = 0,
    RK_INITIAL,
    RK_SERVER,
    RK_NORMAL,
    RK_POST_USERAUTH,
    RK_GSS_UPDATE
} RekeyClass;

typedef struct transport_direction {
    const ssh_cipheralg *cipher;
    const ssh2_macalg *mac;
    bool etm_mode;
    const ssh_compression_alg *comp;
    bool comp_delayed;
    int mkkey_adjust;
} transport_direction;

struct ssh2_transport_state {
    int crState, crStateKex;

    PacketProtocolLayer *higher_layer;
    PktInQueue pq_in_higher;
    PktOutQueue pq_out_higher;
    IdempotentCallback ic_pq_out_higher;

    Conf *conf;
    char *savedhost;
    int savedport;
    const char *rekey_reason;
    enum RekeyClass rekey_class;

    unsigned long max_data_size;

    const ssh_kex *kex_alg;
    const ssh_keyalg *hostkey_alg;
    char *hostkey_str; /* string representation, for easy checking in rekeys */
    unsigned char session_id[MAX_HASH_LEN];
    int session_id_len;
    int dh_min_size, dh_max_size;
    bool dh_got_size_bounds;
    dh_ctx *dh_ctx;
    ssh_hash *exhash;

    struct DataTransferStats *stats;

    char *client_greeting, *server_greeting;

    bool kex_in_progress;
    unsigned long next_rekey, last_rekey;
    const char *deferred_rekey_reason;
    bool higher_layer_ok;

    /*
     * Fully qualified host name, which we need if doing GSSAPI.
     */
    char *fullhostname;

    /* shgss is outside the ifdef on purpose to keep APIs simple. If
     * NO_GSSAPI is not defined, then it's just an opaque structure
     * tag and the pointer will be NULL. */
    struct ssh_connection_shared_gss_state *shgss;
#ifndef NO_GSSAPI
    int gss_status;
    time_t gss_cred_expiry;             /* Re-delegate if newer */
    unsigned long gss_ctxt_lifetime;    /* Re-delegate when short */
#endif
    ssh_transient_hostkey_cache *thc;

    bool gss_kex_used;

    int nbits, pbits;
    bool warn_kex, warn_hk, warn_cscipher, warn_sccipher;
    mp_int *p, *g, *e, *f, *K;
    strbuf *outgoing_kexinit, *incoming_kexinit;
    strbuf *client_kexinit, *server_kexinit; /* aliases to the above */
    int kex_init_value, kex_reply_value;
    transport_direction in, out;
    ptrlen hostkeydata, sigdata;
    strbuf *hostkeyblob;
    char *keystr, *fingerprint;
    ssh_key *hkey;                     /* actual host key */
    RSAKey *rsa_kex_key;             /* for RSA kex */
    ecdh_key *ecdh_key;                     /* for ECDH kex */
    unsigned char exchange_hash[MAX_HASH_LEN];
    bool can_gssapi_keyex;
    bool need_gss_transient_hostkey;
    bool warned_about_no_gss_transient_hostkey;
    bool got_session_id;
    int dlgret;
    bool guessok;
    bool ignorepkt;
    struct kexinit_algorithm kexlists[NKEXLIST][MAXKEXLIST];
#ifndef NO_GSSAPI
    Ssh_gss_buf gss_buf;
    Ssh_gss_buf gss_rcvtok, gss_sndtok;
    Ssh_gss_stat gss_stat;
    Ssh_gss_buf mic;
    bool init_token_sent;
    bool complete_rcvd;
    bool gss_delegate;
#endif

    /* List of crypto primitives below the warning threshold that the
     * user has already clicked OK to, so that we don't keep asking
     * about them again during rekeys. This directly stores pointers
     * to the algorithm vtables, compared by pointer value (which is
     * not a determinism hazard, because we're only using it as a
     * set). */
    tree234 *weak_algorithms_consented_to;

    /*
     * List of host key algorithms for which we _don't_ have a stored
     * host key. These are indices into the main hostkey_algs[] array
     */
    int uncert_hostkeys[N_HOSTKEY_ALGORITHMS];
    int n_uncert_hostkeys;

    /*
     * Indicate that the current rekey is intended to finish with a
     * newly cross-certified host key. To double-check that we
     * certified the right one, we set this to point to the host key
     * algorithm we expect it to be.
     */
    const ssh_keyalg *cross_certifying;

    ssh_key *const *hostkeys;
    int nhostkeys;

    PacketProtocolLayer ppl;
};

/* Helpers shared between transport and kex */
PktIn *ssh2_transport_pop(struct ssh2_transport_state *s);
void ssh2_transport_dialog_callback(void *, int);

/* Provided by transport for use in kex */
void ssh2transport_finalise_exhash(struct ssh2_transport_state *s);

/* Provided by kex for use in transport. Must set the 'aborted' flag
 * if it throws a connection-terminating error, so that the caller
 * won't have to check that by looking inside its state parameter
 * which might already have been freed. */
void ssh2kex_coroutine(struct ssh2_transport_state *s, bool *aborted);

#endif /* PUTTY_SSH2TRANSPORT_H */
