/*
 * Packet protocol layer for the SSH-2 transport protocol (RFC 4253).
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "sshbpp.h"
#include "sshppl.h"
#include "sshcr.h"
#include "storage.h"

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
            const struct ssh_kex *kex;
            int warn;
        } kex;
        struct {
            const ssh_keyalg *hostkey;
            int warn;
        } hk;
        struct {
            const struct ssh2_cipheralg *cipher;
            int warn;
        } cipher;
        struct {
            const struct ssh2_macalg *mac;
            int etm;
        } mac;
        const struct ssh_compression_alg *comp;
    } u;
};

struct ssh_signkey_with_user_pref_id {
    const ssh_keyalg *alg;
    int id;
};
const static struct ssh_signkey_with_user_pref_id hostkey_algs[] = {
    { &ssh_ecdsa_ed25519, HK_ED25519 },
    { &ssh_ecdsa_nistp256, HK_ECDSA },
    { &ssh_ecdsa_nistp384, HK_ECDSA },
    { &ssh_ecdsa_nistp521, HK_ECDSA },
    { &ssh_dss, HK_DSA },
    { &ssh_rsa, HK_RSA },
};

const static struct ssh2_macalg *const macs[] = {
    &ssh_hmac_sha256, &ssh_hmac_sha1, &ssh_hmac_sha1_96, &ssh_hmac_md5
};
const static struct ssh2_macalg *const buggymacs[] = {
    &ssh_hmac_sha1_buggy, &ssh_hmac_sha1_96_buggy, &ssh_hmac_md5
};

static ssh_compressor *ssh_comp_none_init(void)
{
    return NULL;
}
static void ssh_comp_none_cleanup(ssh_compressor *handle)
{
}
static ssh_decompressor *ssh_decomp_none_init(void)
{
    return NULL;
}
static void ssh_decomp_none_cleanup(ssh_decompressor *handle)
{
}
static void ssh_comp_none_block(ssh_compressor *handle,
                                unsigned char *block, int len,
                                unsigned char **outblock, int *outlen,
                                int minlen)
{
}
static int ssh_decomp_none_block(ssh_decompressor *handle,
                                 unsigned char *block, int len,
                                 unsigned char **outblock, int *outlen)
{
    return 0;
}
const static struct ssh_compression_alg ssh_comp_none = {
    "none", NULL,
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_comp_none_block,
    ssh_decomp_none_init, ssh_decomp_none_cleanup, ssh_decomp_none_block,
    NULL
};
const static struct ssh_compression_alg *const compressions[] = {
    &ssh_zlib, &ssh_comp_none
};

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

struct ssh2_transport_state {
    int crState;

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

    const struct ssh_kex *kex_alg;
    const ssh_keyalg *hostkey_alg;
    char *hostkey_str; /* string representation, for easy checking in rekeys */
    unsigned char session_id[SSH2_KEX_MAX_HASH_LEN];
    int session_id_len;
    struct dh_ctx *dh_ctx;
    ssh_hash *exhash;

    struct DataTransferStats *stats;

    char *client_greeting, *server_greeting;

    int kex_in_progress;
    unsigned long next_rekey, last_rekey;
    const char *deferred_rekey_reason;
    int higher_layer_ok;

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
    tree234 *transient_hostkey_cache;
#endif

    int gss_kex_used;

    int nbits, pbits, warn_kex, warn_hk, warn_cscipher, warn_sccipher;
    Bignum p, g, e, f, K;
    void *our_kexinit;
    int our_kexinitlen;
    int kex_init_value, kex_reply_value;
    const struct ssh2_macalg *const *maclist;
    int nmacs;
    struct {
        const struct ssh2_cipheralg *cipher;
        const struct ssh2_macalg *mac;
        int etm_mode;
        const struct ssh_compression_alg *comp;
    } in, out;
    ptrlen hostkeydata, sigdata;
    char *keystr, *fingerprint;
    ssh_key *hkey;                     /* actual host key */
    struct RSAKey *rsa_kex_key;             /* for RSA kex */
    struct ec_key *ecdh_key;              /* for ECDH kex */
    unsigned char exchange_hash[SSH2_KEX_MAX_HASH_LEN];
    int n_preferred_kex;
    int can_gssapi_keyex;
    int need_gss_transient_hostkey;
    int warned_about_no_gss_transient_hostkey;
    const struct ssh_kexes *preferred_kex[KEX_MAX + 1]; /* +1 for GSSAPI */
    int n_preferred_hk;
    int preferred_hk[HK_MAX];
    int n_preferred_ciphers;
    const struct ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
    const struct ssh_compression_alg *preferred_comp;
    int userauth_succeeded;         /* for delayed compression */
    int pending_compression;
    int got_session_id;
    int dlgret;
    int guessok;
    int ignorepkt;
    struct kexinit_algorithm kexlists[NKEXLIST][MAXKEXLIST];
#ifndef NO_GSSAPI
    Ssh_gss_buf gss_buf;
    Ssh_gss_buf gss_rcvtok, gss_sndtok;
    Ssh_gss_stat gss_stat;
    Ssh_gss_buf mic;
    int init_token_sent;
    int complete_rcvd;
    int gss_delegate;
#endif

    /*
     * List of host key algorithms for which we _don't_ have a stored
     * host key. These are indices into the main hostkey_algs[] array
     */
    int uncert_hostkeys[lenof(hostkey_algs)];
    int n_uncert_hostkeys;

    /*
     * Flag indicating that the current rekey is intended to finish
     * with a newly cross-certified host key.
     */
    int cross_certifying;

    PacketProtocolLayer ppl;
};

static void ssh2_transport_free(PacketProtocolLayer *);
static void ssh2_transport_process_queue(PacketProtocolLayer *);
static int ssh2_transport_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx);
static void ssh2_transport_special_cmd(PacketProtocolLayer *ppl,
                                       SessionSpecialCode code, int arg);
static int ssh2_transport_want_user_input(PacketProtocolLayer *ppl);
static void ssh2_transport_got_user_input(PacketProtocolLayer *ppl);
static void ssh2_transport_reconfigure(PacketProtocolLayer *ppl, Conf *conf);

static void ssh2_transport_dialog_callback(void *, int);
static void ssh2_transport_set_max_data_size(struct ssh2_transport_state *s);
static unsigned long sanitise_rekey_time(int rekey_time, unsigned long def);
static void ssh2_transport_higher_layer_packet_callback(void *context);

static const struct PacketProtocolLayerVtable ssh2_transport_vtable = {
    ssh2_transport_free,
    ssh2_transport_process_queue,
    ssh2_transport_get_specials,
    ssh2_transport_special_cmd,
    ssh2_transport_want_user_input,
    ssh2_transport_got_user_input,
    ssh2_transport_reconfigure,
    NULL, /* no protocol name for this layer */
};

#ifndef NO_GSSAPI
static void ssh2_transport_gss_update(struct ssh2_transport_state *s,
                                      int definitely_rekeying);
static void ssh_init_transient_hostkey_store(struct ssh2_transport_state *);
static void ssh_cleanup_transient_hostkey_store(struct ssh2_transport_state *);
static void ssh_store_transient_hostkey(
    struct ssh2_transport_state *s, ssh_key *key);
static int ssh_verify_transient_hostkey(
    struct ssh2_transport_state *s, ssh_key *key);
static int ssh_have_transient_hostkey(
    struct ssh2_transport_state *s, const ssh_keyalg *alg);
static int ssh_have_any_transient_hostkey(
    struct ssh2_transport_state *s);
#endif

static int ssh2_transport_timer_update(struct ssh2_transport_state *s,
                                       unsigned long rekey_time);

static const char *const kexlist_descr[NKEXLIST] = {
    "key exchange algorithm",
    "host key algorithm",
    "client-to-server cipher",
    "server-to-client cipher",
    "client-to-server MAC",
    "server-to-client MAC",
    "client-to-server compression method",
    "server-to-client compression method"
};

PacketProtocolLayer *ssh2_transport_new(
    Conf *conf, const char *host, int port, const char *fullhostname,
    const char *client_greeting, const char *server_greeting,
    struct ssh_connection_shared_gss_state *shgss,
    struct DataTransferStats *stats,
    PacketProtocolLayer *higher_layer)
{
    struct ssh2_transport_state *s = snew(struct ssh2_transport_state);
    memset(s, 0, sizeof(*s));
    s->ppl.vt = &ssh2_transport_vtable;

    s->conf = conf_copy(conf);
    s->savedhost = dupstr(host);
    s->savedport = port;
    s->fullhostname = dupstr(fullhostname);
    s->shgss = shgss;
    s->client_greeting = dupstr(client_greeting);
    s->server_greeting = dupstr(server_greeting);
    s->stats = stats;

    pq_in_init(&s->pq_in_higher);
    pq_out_init(&s->pq_out_higher);
    s->pq_out_higher.pqb.ic = &s->ic_pq_out_higher;
    s->ic_pq_out_higher.fn = ssh2_transport_higher_layer_packet_callback;
    s->ic_pq_out_higher.ctx = &s->ppl;

    s->higher_layer = higher_layer;
    s->higher_layer->selfptr = &s->higher_layer;
    ssh_ppl_setup_queues(s->higher_layer, &s->pq_in_higher, &s->pq_out_higher);

#ifndef NO_GSSAPI
    s->gss_cred_expiry = GSS_NO_EXPIRATION;
    s->shgss->srv_name = GSS_C_NO_NAME;
    s->shgss->ctx = NULL;
    ssh_init_transient_hostkey_store(s);
#endif
    s->gss_kex_used = FALSE;

    ssh2_transport_set_max_data_size(s);

    return &s->ppl;
}

static void ssh2_transport_free(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);

    /*
     * As our last act before being freed, move any outgoing packets
     * off our higher layer's output queue on to our own output queue.
     * We might be being freed while the SSH connection is still alive
     * (because we're initiating shutdown from our end), in which case
     * we don't want those last few packets to get lost.
     *
     * (If our owner were to have already destroyed our output pq
     * before wanting to free us, then it would have to reset our
     * publicly visible out_pq field to NULL to inhibit this attempt.
     * But that's not how I expect the shutdown sequence to go in
     * practice.)
     */
    if (s->ppl.out_pq)
        pq_concatenate(s->ppl.out_pq, s->ppl.out_pq, &s->pq_out_higher);

    conf_free(s->conf);

    ssh_ppl_free(s->higher_layer);

    pq_in_clear(&s->pq_in_higher);
    pq_out_clear(&s->pq_out_higher);

    sfree(s->savedhost);
    sfree(s->fullhostname);
    sfree(s->client_greeting);
    sfree(s->server_greeting);
    sfree(s->keystr);
    sfree(s->hostkey_str);
    sfree(s->fingerprint);
    if (s->hkey) {
        ssh_key_free(s->hkey);
        s->hkey = NULL;
    }
    if (s->e) freebn(s->e);
    if (s->f) freebn(s->f);
    if (s->p) freebn(s->p);
    if (s->g) freebn(s->g);
    if (s->K) freebn(s->K);
    if (s->dh_ctx)
        dh_cleanup(s->dh_ctx);
    if (s->rsa_kex_key)
        ssh_rsakex_freekey(s->rsa_kex_key);
    if (s->ecdh_key)
        ssh_ecdhkex_freekey(s->ecdh_key);
    if (s->exhash)
        ssh_hash_free(s->exhash);
#ifndef NO_GSSAPI
    ssh_cleanup_transient_hostkey_store(s);
#endif
    sfree(s);
}

/*
 * SSH-2 key derivation (RFC 4253 section 7.2).
 */
static void ssh2_mkkey(
    struct ssh2_transport_state *s, strbuf *out,
    Bignum K, unsigned char *H, char chr, int keylen)
{
    int hlen = s->kex_alg->hash->hlen;
    int keylen_padded;
    unsigned char *key;
    ssh_hash *h;

    if (keylen == 0)
        return;

    /*
     * Round the requested amount of key material up to a multiple of
     * the length of the hash we're using to make it. This makes life
     * simpler because then we can just write each hash output block
     * straight into the output buffer without fiddling about
     * truncating the last one. Since it's going into a strbuf, and
     * strbufs are always smemclr()ed on free, there's no need to
     * worry about leaving extra potentially-sensitive data in memory
     * that the caller didn't ask for.
     */
    keylen_padded = ((keylen + hlen - 1) / hlen) * hlen;

    out->len = 0;
    key = strbuf_append(out, keylen_padded);

    /* First hlen bytes. */
    h = ssh_hash_new(s->kex_alg->hash);
    if (!(s->ppl.remote_bugs & BUG_SSH2_DERIVEKEY))
        put_mp_ssh2(h, K);
    put_data(h, H, hlen);
    put_byte(h, chr);
    put_data(h, s->session_id, s->session_id_len);
    ssh_hash_final(h, key);

    /* Subsequent blocks of hlen bytes. */
    if (keylen_padded > hlen) {
        int offset;

        h = ssh_hash_new(s->kex_alg->hash);
        if (!(s->ppl.remote_bugs & BUG_SSH2_DERIVEKEY))
            put_mp_ssh2(h, K);
        put_data(h, H, hlen);

        for (offset = hlen; offset < keylen_padded; offset += hlen) {
            put_data(h, key + offset - hlen, hlen);
            ssh_hash *h2 = ssh_hash_copy(h);
            ssh_hash_final(h2, key + offset);
        }

        ssh_hash_free(h);
    }
}

/*
 * Find a slot in a KEXINIT algorithm list to use for a new algorithm.
 * If the algorithm is already in the list, return a pointer to its
 * entry, otherwise return an entry from the end of the list.
 * This assumes that every time a particular name is passed in, it
 * comes from the same string constant.  If this isn't true, this
 * function may need to be rewritten to use strcmp() instead.
 */
static struct kexinit_algorithm *ssh2_kexinit_addalg(struct kexinit_algorithm
                                                     *list, const char *name)
{
    int i;

    for (i = 0; i < MAXKEXLIST; i++)
        if (list[i].name == NULL || list[i].name == name) {
            list[i].name = name;
            return &list[i];
        }
    assert(!"No space in KEXINIT list");
    return NULL;
}

int ssh2_common_filter_queue(PacketProtocolLayer *ppl)
{
    static const char *const ssh2_disconnect_reasons[] = {
        NULL,
        "host not allowed to connect",
        "protocol error",
        "key exchange failed",
        "host authentication failed",
        "MAC error",
        "compression error",
        "service not available",
        "protocol version not supported",
        "host key not verifiable",
        "connection lost",
        "by application",
        "too many connections",
        "auth cancelled by user",
        "no more auth methods available",
        "illegal user name",
    };

    PktIn *pktin;
    ptrlen msg;
    int reason;

    while ((pktin = pq_peek(ppl->in_pq)) != NULL) {
        switch (pktin->type) {
          case SSH2_MSG_DISCONNECT:
            reason = get_uint32(pktin);
            msg = get_string(pktin);

            ssh_remote_error(
                ppl->ssh, "Server sent disconnect message\n"
                "type %d (%s):\n\"%.*s\"", reason,
                ((reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
                 ssh2_disconnect_reasons[reason] : "unknown"),
                PTRLEN_PRINTF(msg));
            return TRUE;               /* indicate that we've been freed */

          case SSH2_MSG_DEBUG:
            /* XXX maybe we should actually take notice of the return value */
            get_bool(pktin);
            msg = get_string(pktin);
            ppl_logevent(("Remote debug message: %.*s", PTRLEN_PRINTF(msg)));
            pq_pop(ppl->in_pq);
            break;

          case SSH2_MSG_IGNORE:
            /* Do nothing, because we're ignoring it! Duhh. */
            pq_pop(ppl->in_pq);
            break;

          default:
            return FALSE;
        }
    }

    return FALSE;
}

static int ssh2_transport_filter_queue(struct ssh2_transport_state *s)
{
    PktIn *pktin;

    while (1) {
        if (ssh2_common_filter_queue(&s->ppl))
            return TRUE;
        if ((pktin = pq_peek(s->ppl.in_pq)) == NULL)
            return FALSE;

        /* Pass on packets to the next layer if they're outside
         * the range reserved for the transport protocol. */
        if (pktin->type >= 50) {
            /* ... except that we shouldn't tolerate higher-layer
             * packets coming from the server before we've seen
             * the first NEWKEYS. */
            if (!s->higher_layer_ok) {
                ssh_proto_error(s->ppl.ssh, "Received premature higher-"
                                "layer packet, type %d (%s)", pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return TRUE;
            }

            pq_pop(s->ppl.in_pq);
            pq_push(&s->pq_in_higher, pktin);
        } else {
            /* Anything else is a transport-layer packet that the main
             * process_queue coroutine should handle. */
            return FALSE;
        }
    }
}

static PktIn *ssh2_transport_pop(struct ssh2_transport_state *s)
{
    ssh2_transport_filter_queue(s);
    return pq_pop(s->ppl.in_pq);
}

static void ssh2_transport_process_queue(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);
    PktIn *pktin;
    PktOut *pktout;

    /* Filter centrally handled messages off the front of the queue on
     * every entry to this coroutine, no matter where we're resuming
     * from, even if we're _not_ looping on pq_pop. That way we can
     * still proactively handle those messages even if we're waiting
     * for a user response. */
    ssh2_transport_filter_queue(s);

    crBegin(s->crState);

    s->in.cipher = s->out.cipher = NULL;
    s->in.mac = s->out.mac = NULL;
    s->in.comp = s->out.comp = NULL;

    s->got_session_id = FALSE;
    s->userauth_succeeded = FALSE;
    s->pending_compression = FALSE;
    s->need_gss_transient_hostkey = FALSE;
    s->warned_about_no_gss_transient_hostkey = FALSE;

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (s->ppl.remote_bugs & BUG_SSH2_HMAC)
        s->maclist = buggymacs, s->nmacs = lenof(buggymacs);
    else
        s->maclist = macs, s->nmacs = lenof(macs);

  begin_key_exchange:

#ifndef NO_GSSAPI
    if (s->need_gss_transient_hostkey) {
        /*
         * This flag indicates a special case in which we must not do
         * GSS key exchange even if we could. (See comments below,
         * where the flag was set on the previous key exchange.)
         */
        s->can_gssapi_keyex = FALSE;
    } else if (conf_get_int(s->conf, CONF_try_gssapi_kex)) {
        /*
         * We always check if we have GSS creds before we come up with
         * the kex algorithm list, otherwise future rekeys will fail
         * when creds expire. To make this so, this code section must
         * follow the begin_key_exchange label above, otherwise this
         * section would execute just once per-connection.
         *
         * Update GSS state unless the reason we're here is that a
         * timer just checked the GSS state and decided that we should
         * rekey to update delegated credentials. In that case, the
         * state is "fresh".
         */
        if (s->rekey_class != RK_GSS_UPDATE)
            ssh2_transport_gss_update(s, TRUE);

        /* Do GSSAPI KEX when capable */
        s->can_gssapi_keyex = s->gss_status & GSS_KEX_CAPABLE;

        /*
         * But not when failure is likely. [ GSS implementations may
         * attempt (and fail) to use a ticket that is almost expired
         * when retrieved from the ccache that actually expires by the
         * time the server receives it. ]
         *
         * Note: The first time always try KEXGSS if we can, failures
         * will be very rare, and disabling the initial GSS KEX is
         * worse. Some day GSS libraries will ignore cached tickets
         * whose lifetime is critically short, and will instead use
         * fresh ones.
         */
        if (!s->got_session_id && (s->gss_status & GSS_CTXT_MAYFAIL) != 0)
            s->can_gssapi_keyex = 0;
        s->gss_delegate = conf_get_int(s->conf, CONF_gssapifwd);
    } else {
        s->can_gssapi_keyex = FALSE;
    }
#endif

    s->ppl.bpp->pls->kctx = SSH2_PKTCTX_NOKEX;
    {
        int i, j, k, warn;
        struct kexinit_algorithm *alg;

        /*
         * Set up the preferred key exchange. (NULL => warn below here)
         */
        s->n_preferred_kex = 0;
        if (s->can_gssapi_keyex)
            s->preferred_kex[s->n_preferred_kex++] = &ssh_gssk5_sha1_kex;
        for (i = 0; i < KEX_MAX; i++) {
	    switch (conf_get_int_int(s->conf, CONF_ssh_kexlist, i)) {
              case KEX_DHGEX:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_diffiehellman_gex;
                break;
              case KEX_DHGROUP14:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_diffiehellman_group14;
                break;
              case KEX_DHGROUP1:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_diffiehellman_group1;
                break;
              case KEX_RSA:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_rsa_kex;
                break;
              case KEX_ECDH:
                s->preferred_kex[s->n_preferred_kex++] =
                    &ssh_ecdh_kex;
                break;
              case KEX_WARN:
                /* Flag for later. Don't bother if it's the last in
                 * the list. */
                if (i < KEX_MAX - 1) {
                    s->preferred_kex[s->n_preferred_kex++] = NULL;
                }
                break;
            }
        }

        /*
         * Set up the preferred host key types. These are just the ids
         * in the enum in putty.h, so 'warn below here' is indicated
         * by HK_WARN.
         */
        s->n_preferred_hk = 0;
        for (i = 0; i < HK_MAX; i++) {
            int id = conf_get_int_int(s->conf, CONF_ssh_hklist, i);
            /* As above, don't bother with HK_WARN if it's last in the
             * list */
            if (id != HK_WARN || i < HK_MAX - 1)
                s->preferred_hk[s->n_preferred_hk++] = id;
        }

        /*
         * Set up the preferred ciphers. (NULL => warn below here)
         */
        s->n_preferred_ciphers = 0;
	for (i = 0; i < CIPHER_MAX; i++) {
	    switch (conf_get_int_int(s->conf, CONF_ssh_cipherlist, i)) {
              case CIPHER_BLOWFISH:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_blowfish;
                break;
              case CIPHER_DES:
		if (conf_get_int(s->conf, CONF_ssh2_des_cbc))
                    s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_des;
                break;
              case CIPHER_3DES:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_3des;
                break;
              case CIPHER_AES:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_aes;
                break;
              case CIPHER_ARCFOUR:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_arcfour;
                break;
              case CIPHER_CHACHA20:
                s->preferred_ciphers[s->n_preferred_ciphers++] = &ssh2_ccp;
                break;
              case CIPHER_WARN:
                /* Flag for later. Don't bother if it's the last in
                 * the list. */
                if (i < CIPHER_MAX - 1) {
                    s->preferred_ciphers[s->n_preferred_ciphers++] = NULL;
                }
                break;
            }
        }

        /*
         * Set up preferred compression.
         */
	if (conf_get_int(s->conf, CONF_compression))
            s->preferred_comp = &ssh_zlib;
        else
            s->preferred_comp = &ssh_comp_none;

        /*
         * Flag that KEX is in progress.
         */
        s->kex_in_progress = TRUE;

        for (i = 0; i < NKEXLIST; i++)
            for (j = 0; j < MAXKEXLIST; j++)
                s->kexlists[i][j].name = NULL;
        /* List key exchange algorithms. */
        warn = FALSE;
        for (i = 0; i < s->n_preferred_kex; i++) {
            const struct ssh_kexes *k = s->preferred_kex[i];
            if (!k) warn = TRUE;
            else for (j = 0; j < k->nkexes; j++) {
                    alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_KEX],
                                              k->list[j]->name);
                    alg->u.kex.kex = k->list[j];
                    alg->u.kex.warn = warn;
                }
        }
        /* List server host key algorithms. */
        if (!s->got_session_id) {
            /*
             * In the first key exchange, we list all the algorithms
             * we're prepared to cope with, but prefer those algorithms
             * for which we have a host key for this host.
             *
             * If the host key algorithm is below the warning
             * threshold, we warn even if we did already have a key
             * for it, on the basis that if the user has just
             * reconfigured that host key type to be warned about,
             * they surely _do_ want to be alerted that a server
             * they're actually connecting to is using it.
             */
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    if (have_ssh_host_key(s->savedhost, s->savedport,
                                          hostkey_algs[j].alg->cache_id)) {
                        alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                                  hostkey_algs[j].alg->ssh_id);
                        alg->u.hk.hostkey = hostkey_algs[j].alg;
                        alg->u.hk.warn = warn;
                    }
                }
            }
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                              hostkey_algs[j].alg->ssh_id);
                    alg->u.hk.hostkey = hostkey_algs[j].alg;
                    alg->u.hk.warn = warn;
                }
            }
#ifndef NO_GSSAPI
        } else if (s->gss_kex_used && !s->need_gss_transient_hostkey) {
            /*
             * If we've previously done a GSSAPI KEX, then we list
             * precisely the algorithms for which a previous GSS key
             * exchange has delivered us a host key, because we expect
             * one of exactly those keys to be used in any subsequent
             * non-GSS-based rekey.
             *
             * An exception is if this is the key exchange we
             * triggered for the purposes of populating that cache -
             * in which case the cache will currently be empty, which
             * isn't helpful!
             */
            warn = FALSE;
            for (i = 0; i < s->n_preferred_hk; i++) {
                if (s->preferred_hk[i] == HK_WARN)
                    warn = TRUE;
                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].id != s->preferred_hk[i])
                        continue;
                    if (ssh_have_transient_hostkey(s, hostkey_algs[j].alg)) {
                        alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                                  hostkey_algs[j].alg->ssh_id);
                        alg->u.hk.hostkey = hostkey_algs[j].alg;
                        alg->u.hk.warn = warn;
                    }
                }
            }
#endif
        } else {
            /*
             * In subsequent key exchanges, we list only the kex
             * algorithm that was selected in the first key exchange,
             * so that we keep getting the same host key and hence
             * don't have to interrupt the user's session to ask for
             * reverification.
             */
            assert(s->kex_alg);
            alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY],
                                      s->hostkey_alg->ssh_id);
            alg->u.hk.hostkey = s->hostkey_alg;
            alg->u.hk.warn = FALSE;
        }
        if (s->can_gssapi_keyex) {
            alg = ssh2_kexinit_addalg(s->kexlists[KEXLIST_HOSTKEY], "null");
            alg->u.hk.hostkey = NULL;
        }
        /* List encryption algorithms (client->server then server->client). */
        for (k = KEXLIST_CSCIPHER; k <= KEXLIST_SCCIPHER; k++) {
            warn = FALSE;
#ifdef FUZZING
            alg = ssh2_kexinit_addalg(s->kexlists[k], "none");
            alg->u.cipher.cipher = NULL;
            alg->u.cipher.warn = warn;
#endif /* FUZZING */
            for (i = 0; i < s->n_preferred_ciphers; i++) {
                const struct ssh2_ciphers *c = s->preferred_ciphers[i];
                if (!c) warn = TRUE;
                else for (j = 0; j < c->nciphers; j++) {
                        alg = ssh2_kexinit_addalg(s->kexlists[k],
                                                  c->list[j]->name);
                        alg->u.cipher.cipher = c->list[j];
                        alg->u.cipher.warn = warn;
                    }
            }
        }
        /* List MAC algorithms (client->server then server->client). */
        for (j = KEXLIST_CSMAC; j <= KEXLIST_SCMAC; j++) {
#ifdef FUZZING
            alg = ssh2_kexinit_addalg(s->kexlists[j], "none");
            alg->u.mac.mac = NULL;
            alg->u.mac.etm = FALSE;
#endif /* FUZZING */
            for (i = 0; i < s->nmacs; i++) {
                alg = ssh2_kexinit_addalg(s->kexlists[j], s->maclist[i]->name);
                alg->u.mac.mac = s->maclist[i];
                alg->u.mac.etm = FALSE;
            }
            for (i = 0; i < s->nmacs; i++)
                /* For each MAC, there may also be an ETM version,
                 * which we list second. */
                if (s->maclist[i]->etm_name) {
                    alg = ssh2_kexinit_addalg(s->kexlists[j],
                                              s->maclist[i]->etm_name);
                    alg->u.mac.mac = s->maclist[i];
                    alg->u.mac.etm = TRUE;
                }
        }
        /* List client->server compression algorithms,
         * then server->client compression algorithms. (We use the
         * same set twice.) */
        for (j = KEXLIST_CSCOMP; j <= KEXLIST_SCCOMP; j++) {
            assert(lenof(compressions) > 1);
            /* Prefer non-delayed versions */
            alg = ssh2_kexinit_addalg(s->kexlists[j], s->preferred_comp->name);
            alg->u.comp = s->preferred_comp;
            /* We don't even list delayed versions of algorithms until
             * they're allowed to be used, to avoid a race. See the end of
             * this function. */
            if (s->userauth_succeeded && s->preferred_comp->delayed_name) {
                alg = ssh2_kexinit_addalg(s->kexlists[j],
                                          s->preferred_comp->delayed_name);
                alg->u.comp = s->preferred_comp;
            }
            for (i = 0; i < lenof(compressions); i++) {
                const struct ssh_compression_alg *c = compressions[i];
                alg = ssh2_kexinit_addalg(s->kexlists[j], c->name);
                alg->u.comp = c;
                if (s->userauth_succeeded && c->delayed_name) {
                    alg = ssh2_kexinit_addalg(s->kexlists[j], c->delayed_name);
                    alg->u.comp = c;
                }
            }
        }
        /*
         * Construct and send our key exchange packet.
         */
        pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXINIT);
        for (i = 0; i < 16; i++)
            put_byte(pktout, (unsigned char) random_byte());
        for (i = 0; i < NKEXLIST; i++) {
            strbuf *list = strbuf_new();
            for (j = 0; j < MAXKEXLIST; j++) {
                if (s->kexlists[i][j].name == NULL) break;
                add_to_commasep(list, s->kexlists[i][j].name);
            }
            put_stringsb(pktout, list);
        }
        /* List client->server languages. Empty list. */
        put_stringz(pktout, "");
        /* List server->client languages. Empty list. */
        put_stringz(pktout, "");
        /* First KEX packet does _not_ follow, because we're not that brave. */
        put_bool(pktout, FALSE);
        /* Reserved. */
        put_uint32(pktout, 0);
    }

    s->our_kexinitlen = pktout->length - 5;
    s->our_kexinit = snewn(s->our_kexinitlen, unsigned char);
    memcpy(s->our_kexinit, pktout->data + 5, s->our_kexinitlen);

    pq_push(s->ppl.out_pq, pktout);

    crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);

    /*
     * Now examine the other side's KEXINIT to see what we're up
     * to.
     */
    {
        ptrlen str;
        int i, j;

        if (pktin->type != SSH2_MSG_KEXINIT) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting KEXINIT, type %d (%s)", pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx, pktin->type));
            return;
        }
        s->kex_alg = NULL;
        s->hostkey_alg = NULL;
        s->in.cipher = s->out.cipher = NULL;
        s->in.mac = s->out.mac = NULL;
        s->in.comp = s->out.comp = NULL;
        s->warn_kex = s->warn_hk = FALSE;
        s->warn_cscipher = s->warn_sccipher = FALSE;

        get_data(pktin, 16);           /* skip garbage cookie */

        s->guessok = FALSE;
        for (i = 0; i < NKEXLIST; i++) {
            str = get_string(pktin);
            if (get_err(pktin)) {
                ssh_proto_error(s->ppl.ssh, "KEXINIT packet was incomplete");
                return;
            }

            /* If we've already selected a cipher which requires a
             * particular MAC, then just select that, and don't even
             * bother looking through the server's KEXINIT string for
             * MACs. */
            if (i == KEXLIST_CSMAC && s->out.cipher &&
                s->out.cipher->required_mac) {
                s->out.mac = s->out.cipher->required_mac;
                s->out.etm_mode = !!(s->out.mac->etm_name);
                goto matched;
            }
            if (i == KEXLIST_SCMAC && s->in.cipher &&
                s->in.cipher->required_mac) {
                s->in.mac = s->in.cipher->required_mac;
                s->in.etm_mode = !!(s->in.mac->etm_name);
                goto matched;
            }

            for (j = 0; j < MAXKEXLIST; j++) {
                struct kexinit_algorithm *alg = &s->kexlists[i][j];
                if (alg->name == NULL) break;
                if (in_commasep_string(alg->name, str.ptr, str.len)) {
                    /* We've found a matching algorithm. */
                    if (i == KEXLIST_KEX || i == KEXLIST_HOSTKEY) {
                        /* Check if we might need to ignore first kex pkt */
                        if (j != 0 ||
                            !first_in_commasep_string(alg->name,
                                                      str.ptr, str.len))
                            s->guessok = FALSE;
                    }
                    if (i == KEXLIST_KEX) {
                        s->kex_alg = alg->u.kex.kex;
                        s->warn_kex = alg->u.kex.warn;
                    } else if (i == KEXLIST_HOSTKEY) {
                        /*
                         * Ignore an unexpected/inappropriate offer of "null",
                         * we offer "null" when we're willing to use GSS KEX,
                         * but it is only acceptable when GSSKEX is actually
                         * selected.
                         */
                        if (alg->u.hk.hostkey == NULL &&
                            s->kex_alg->main_type != KEXTYPE_GSS)
                            continue;
                        s->hostkey_alg = alg->u.hk.hostkey;
                        s->warn_hk = alg->u.hk.warn;
                    } else if (i == KEXLIST_CSCIPHER) {
                        s->out.cipher = alg->u.cipher.cipher;
                        s->warn_cscipher = alg->u.cipher.warn;
                    } else if (i == KEXLIST_SCCIPHER) {
                        s->in.cipher = alg->u.cipher.cipher;
                        s->warn_sccipher = alg->u.cipher.warn;
                    } else if (i == KEXLIST_CSMAC) {
                        s->out.mac = alg->u.mac.mac;
                        s->out.etm_mode = alg->u.mac.etm;
                    } else if (i == KEXLIST_SCMAC) {
                        s->in.mac = alg->u.mac.mac;
                        s->in.etm_mode = alg->u.mac.etm;
                    } else if (i == KEXLIST_CSCOMP) {
                        s->out.comp = alg->u.comp;
                    } else if (i == KEXLIST_SCCOMP) {
                        s->in.comp = alg->u.comp;
                    }
                    goto matched;
                }

                /* Set a flag if there's a delayed compression option
                 * available for a compression method that we just
                 * failed to select the immediate version of. */
                s->pending_compression = (
                    (i == KEXLIST_CSCOMP || i == KEXLIST_SCCOMP) &&
                    in_commasep_string(alg->u.comp->delayed_name,
                                       str.ptr, str.len) &&
                    !s->userauth_succeeded);
            }
            ssh_sw_abort(s->ppl.ssh, "Couldn't agree a %s (available: %.*s)",
                         kexlist_descr[i], PTRLEN_PRINTF(str));
            return;
          matched:;

            if (i == KEXLIST_HOSTKEY &&
                !s->gss_kex_used &&
                s->kex_alg->main_type != KEXTYPE_GSS) {
                int j;

                /*
                 * In addition to deciding which host key we're
                 * actually going to use, we should make a list of the
                 * host keys offered by the server which we _don't_
                 * have cached. These will be offered as cross-
                 * certification options by ssh_get_specials.
                 *
                 * We also count the key we're currently using for KEX
                 * as one we've already got, because by the time this
                 * menu becomes visible, it will be.
                 */
                s->n_uncert_hostkeys = 0;

                for (j = 0; j < lenof(hostkey_algs); j++) {
                    if (hostkey_algs[j].alg != s->hostkey_alg &&
                        in_commasep_string(hostkey_algs[j].alg->ssh_id,
                                           str.ptr, str.len) &&
                        !have_ssh_host_key(s->savedhost, s->savedport,
                                           hostkey_algs[j].alg->cache_id)) {
                        s->uncert_hostkeys[s->n_uncert_hostkeys++] = j;
                    }
                }
            }
        }

        if (s->pending_compression) {
            ppl_logevent(("Server supports delayed compression; "
                          "will try this later"));
        }
        get_string(pktin);  /* client->server language */
        get_string(pktin);  /* server->client language */
        s->ignorepkt = get_bool(pktin) && !s->guessok;

        s->exhash = ssh_hash_new(s->kex_alg->hash);
        put_stringz(s->exhash, s->client_greeting);
        put_stringz(s->exhash, s->server_greeting);
        put_string(s->exhash, s->our_kexinit, s->our_kexinitlen);
        sfree(s->our_kexinit);
        /* Include the type byte in the hash of server's KEXINIT */
        put_string(s->exhash,
                   (const char *)BinarySource_UPCAST(pktin)->data - 1,
                   BinarySource_UPCAST(pktin)->len + 1);

        if (s->warn_kex) {
            s->dlgret = askalg(s->ppl.frontend, "key-exchange algorithm",
                               s->kex_alg->name,
                               ssh2_transport_dialog_callback, s);
            crMaybeWaitUntilV(s->dlgret >= 0);
            if (s->dlgret == 0) {
                ssh_user_close(s->ppl.ssh, "User aborted at kex warning");
                return;
            }
        }

        if (s->warn_hk) {
            int j, k;
            char *betteralgs;

            /*
             * Change warning box wording depending on why we chose a
             * warning-level host key algorithm. If it's because
             * that's all we have *cached*, use the askhk mechanism,
             * and list the host keys we could usefully cross-certify.
             * Otherwise, use askalg for the standard wording.
             */
            betteralgs = NULL;
            for (j = 0; j < s->n_uncert_hostkeys; j++) {
                const struct ssh_signkey_with_user_pref_id *hktype =
                    &hostkey_algs[s->uncert_hostkeys[j]];
                int better = FALSE;
                for (k = 0; k < HK_MAX; k++) {
                    int id = conf_get_int_int(s->conf, CONF_ssh_hklist, k);
                    if (id == HK_WARN) {
                        break;
                    } else if (id == hktype->id) {
                        better = TRUE;
                        break;
                    }
                }
                if (better) {
                    if (betteralgs) {
                        char *old_ba = betteralgs;
                        betteralgs = dupcat(betteralgs, ",",
                                            hktype->alg->ssh_id,
                                            (const char *)NULL);
                        sfree(old_ba);
                    } else {
                        betteralgs = dupstr(hktype->alg->ssh_id);
                    }
                }
            }
            if (betteralgs) {
                s->dlgret = askhk(
                    s->ppl.frontend, s->hostkey_alg->ssh_id, betteralgs,
                    ssh2_transport_dialog_callback, s);
                sfree(betteralgs);
            } else {
                s->dlgret = askalg(s->ppl.frontend, "host key type",
                                   s->hostkey_alg->ssh_id,
                                   ssh2_transport_dialog_callback, s);
            }
            crMaybeWaitUntilV(s->dlgret >= 0);
            if (s->dlgret == 0) {
                ssh_user_close(s->ppl.ssh, "User aborted at host key warning");
                return;
            }
        }

        if (s->warn_cscipher) {
            s->dlgret = askalg(s->ppl.frontend,
                               "client-to-server cipher",
                               s->out.cipher->name,
                               ssh2_transport_dialog_callback, s);
            crMaybeWaitUntilV(s->dlgret >= 0);
            if (s->dlgret == 0) {
                ssh_user_close(s->ppl.ssh, "User aborted at cipher warning");
                return;
            }
        }

        if (s->warn_sccipher) {
            s->dlgret = askalg(s->ppl.frontend,
                               "server-to-client cipher",
                               s->in.cipher->name,
                               ssh2_transport_dialog_callback, s);
            crMaybeWaitUntilV(s->dlgret >= 0);
            if (s->dlgret == 0) {
                ssh_user_close(s->ppl.ssh, "User aborted at cipher warning");
                return;
            }
        }

        if (s->ignorepkt) /* first_kex_packet_follows */
            crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
    }

    if (s->kex_alg->main_type == KEXTYPE_DH) {
        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher ? s->out.cipher->real_keybits : 0;
            scbits = s->in.cipher ? s->in.cipher->real_keybits : 0;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > s->kex_alg->hash->hlen * 8)
            s->nbits = s->kex_alg->hash->hlen * 8;

        /*
         * If we're doing Diffie-Hellman group exchange, start by
         * requesting a group.
         */
        if (dh_is_gex(s->kex_alg)) {
            ppl_logevent(("Doing Diffie-Hellman group exchange"));
            s->ppl.bpp->pls->kctx = SSH2_PKTCTX_DHGEX;
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            if (s->pbits < DH_MIN_SIZE)
                s->pbits = DH_MIN_SIZE;
            if (s->pbits > DH_MAX_SIZE)
                s->pbits = DH_MAX_SIZE;
            if ((s->ppl.remote_bugs & BUG_SSH2_OLDGEX)) {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEX_DH_GEX_REQUEST_OLD);
                put_uint32(pktout, s->pbits);
            } else {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEX_DH_GEX_REQUEST);
                put_uint32(pktout, DH_MIN_SIZE);
                put_uint32(pktout, s->pbits);
                put_uint32(pktout, DH_MAX_SIZE);
            }
            pq_push(s->ppl.out_pq, pktout);

            crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_KEX_DH_GEX_GROUP) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                                "expecting Diffie-Hellman group, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                ssh_proto_error(s->ppl.ssh,
                                "Unable to parse Diffie-Hellman group packet");
                return;
            }
            s->dh_ctx = dh_setup_gex(s->p, s->g);
            s->kex_init_value = SSH2_MSG_KEX_DH_GEX_INIT;
            s->kex_reply_value = SSH2_MSG_KEX_DH_GEX_REPLY;
        } else {
            s->ppl.bpp->pls->kctx = SSH2_PKTCTX_DHGROUP;
            s->dh_ctx = dh_setup_group(s->kex_alg);
            s->kex_init_value = SSH2_MSG_KEXDH_INIT;
            s->kex_reply_value = SSH2_MSG_KEXDH_REPLY;
            ppl_logevent(("Using Diffie-Hellman with standard group \"%s\"",
                          s->kex_alg->groupname));
        }

        ppl_logevent(("Doing Diffie-Hellman key exchange with hash %s",
                      s->kex_alg->hash->text_name));
        /*
         * Now generate and send e for Diffie-Hellman.
         */
        set_busy_status(s->ppl.frontend, BUSY_CPU); /* this can take a while */
        s->e = dh_create_e(s->dh_ctx, s->nbits * 2);
        pktout = ssh_bpp_new_pktout(s->ppl.bpp, s->kex_init_value);
        put_mp_ssh2(pktout, s->e);
        pq_push(s->ppl.out_pq, pktout);

        set_busy_status(s->ppl.frontend, BUSY_WAITING); /* wait for server */
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != s->kex_reply_value) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting Diffie-Hellman reply, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            return;
        }
        set_busy_status(s->ppl.frontend, BUSY_CPU); /* cogitate */
        s->hostkeydata = get_string(pktin);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);
        s->f = get_mp_ssh2(pktin);
        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh,
                            "Unable to parse Diffie-Hellman reply packet");
            return;
        }

        {
            const char *err = dh_validate_f(s->dh_ctx, s->f);
            if (err) {
                ssh_proto_error(s->ppl.ssh, "Diffie-Hellman reply failed "
                                "validation: %s", err);
                return;
            }
        }
        s->K = dh_find_K(s->dh_ctx, s->f);

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        set_busy_status(s->ppl.frontend, BUSY_NOT);

        put_stringpl(s->exhash, s->hostkeydata);
        if (dh_is_gex(s->kex_alg)) {
            if (!(s->ppl.remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(s->exhash, DH_MIN_SIZE);
            put_uint32(s->exhash, s->pbits);
            if (!(s->ppl.remote_bugs & BUG_SSH2_OLDGEX))
                put_uint32(s->exhash, DH_MAX_SIZE);
            put_mp_ssh2(s->exhash, s->p);
            put_mp_ssh2(s->exhash, s->g);
        }
        put_mp_ssh2(s->exhash, s->e);
        put_mp_ssh2(s->exhash, s->f);

        dh_cleanup(s->dh_ctx);
        s->dh_ctx = NULL;
        freebn(s->f); s->f = NULL;
        freebn(s->e); s->e = NULL;
        if (dh_is_gex(s->kex_alg)) {
            freebn(s->g); s->g = NULL;
            freebn(s->p); s->p = NULL;
        }
    } else if (s->kex_alg->main_type == KEXTYPE_ECDH) {

        ppl_logevent(("Doing ECDH key exchange with curve %s and hash %s",
                      ssh_ecdhkex_curve_textname(s->kex_alg),
                      s->kex_alg->hash->text_name));
        s->ppl.bpp->pls->kctx = SSH2_PKTCTX_ECDHKEX;

        s->ecdh_key = ssh_ecdhkex_newkey(s->kex_alg);
        if (!s->ecdh_key) {
            ssh_sw_abort(s->ppl.ssh, "Unable to generate key for ECDH");
            return;
        }

        pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEX_ECDH_INIT);
        {
            strbuf *pubpoint = strbuf_new();
            ssh_ecdhkex_getpublic(s->ecdh_key, BinarySink_UPCAST(pubpoint));
            put_stringsb(pktout, pubpoint);
        }

        pq_push(s->ppl.out_pq, pktout);

        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEX_ECDH_REPLY) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting ECDH reply, type %d (%s)", pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            return;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(s->exhash, s->hostkeydata);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);

        {
            strbuf *pubpoint = strbuf_new();
            ssh_ecdhkex_getpublic(s->ecdh_key, BinarySink_UPCAST(pubpoint));
            put_string(s->exhash, pubpoint->u, pubpoint->len);
            strbuf_free(pubpoint);
        }

        {
            ptrlen keydata = get_string(pktin);
            put_stringpl(s->exhash, keydata);
            s->K = ssh_ecdhkex_getkey(s->ecdh_key, keydata.ptr, keydata.len);
            if (!get_err(pktin) && !s->K) {
                ssh_proto_error(s->ppl.ssh, "Received invalid elliptic curve "
                                "point in ECDH reply");
                return;
            }
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh, "Unable to parse ECDH reply packet");
            return;
        }

        ssh_ecdhkex_freekey(s->ecdh_key);
        s->ecdh_key = NULL;
#ifndef NO_GSSAPI
    } else if (s->kex_alg->main_type == KEXTYPE_GSS) {
        ptrlen data;

        s->ppl.bpp->pls->kctx = SSH2_PKTCTX_GSSKEX;
        s->init_token_sent = 0;
        s->complete_rcvd = 0;
        s->hkey = NULL;
        s->fingerprint = NULL;
        s->keystr = NULL;

        /*
         * Work out the number of bits of key we will need from the
         * key exchange. We start with the maximum key length of
         * either cipher...
         *
         * This is rote from the KEXTYPE_DH section above.
         */
        {
            int csbits, scbits;

            csbits = s->out.cipher->real_keybits;
            scbits = s->in.cipher->real_keybits;
            s->nbits = (csbits > scbits ? csbits : scbits);
        }
        /* The keys only have hlen-bit entropy, since they're based on
         * a hash. So cap the key size at hlen bits. */
        if (s->nbits > s->kex_alg->hash->hlen * 8)
            s->nbits = s->kex_alg->hash->hlen * 8;

        if (dh_is_gex(s->kex_alg)) {
            /*
             * Work out how big a DH group we will need to allow that
             * much data.
             */
            s->pbits = 512 << ((s->nbits - 1) / 64);
            ppl_logevent(("Doing GSSAPI (with Kerberos V5) Diffie-Hellman "
                          "group exchange, with minimum %d bits", s->pbits));
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXGSS_GROUPREQ);
            put_uint32(pktout, s->pbits); /* min */
            put_uint32(pktout, s->pbits); /* preferred */
            put_uint32(pktout, s->pbits * 2); /* max */
            pq_push(s->ppl.out_pq, pktout);

            crMaybeWaitUntilV(
                (pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_KEXGSS_GROUP) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                                "expecting Diffie-Hellman group, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }
            s->p = get_mp_ssh2(pktin);
            s->g = get_mp_ssh2(pktin);
            if (get_err(pktin)) {
                ssh_proto_error(s->ppl.ssh,
                                "Unable to parse Diffie-Hellman group packet");
                return;
            }
            s->dh_ctx = dh_setup_gex(s->p, s->g);
        } else {
            s->dh_ctx = dh_setup_group(s->kex_alg);
            ppl_logevent(("Using GSSAPI (with Kerberos V5) Diffie-Hellman with"
                          " standard group \"%s\"", s->kex_alg->groupname));
        }

        ppl_logevent(("Doing GSSAPI (with Kerberos V5) Diffie-Hellman key "
                      "exchange with hash %s", s->kex_alg->hash->text_name));
        /* Now generate e for Diffie-Hellman. */
        set_busy_status(s->ppl.frontend, BUSY_CPU); /* this can take a while */
        s->e = dh_create_e(s->dh_ctx, s->nbits * 2);

        if (s->shgss->lib->gsslogmsg)
            ppl_logevent(("%s", s->shgss->lib->gsslogmsg));

        /* initial tokens are empty */
        SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);
        SSH_GSS_CLEAR_BUF(&s->gss_sndtok);
        SSH_GSS_CLEAR_BUF(&s->mic);
        s->gss_stat = s->shgss->lib->acquire_cred(
            s->shgss->lib, &s->shgss->ctx, &s->gss_cred_expiry);
        if (s->gss_stat != SSH_GSS_OK) {
            ssh_sw_abort(s->ppl.ssh,
                         "GSSAPI key exchange failed to initialise");
            return;
        }

        /* now enter the loop */
        assert(s->shgss->srv_name);
        do {
            /*
             * When acquire_cred yields no useful expiration, go with the
             * service ticket expiration.
             */
            s->gss_stat = s->shgss->lib->init_sec_context(
                s->shgss->lib, &s->shgss->ctx, s->shgss->srv_name,
                s->gss_delegate, &s->gss_rcvtok, &s->gss_sndtok,
                (s->gss_cred_expiry == GSS_NO_EXPIRATION ?
                 &s->gss_cred_expiry : NULL), NULL);
            SSH_GSS_CLEAR_BUF(&s->gss_rcvtok);

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break; /* MIC is verified after the loop */

            if (s->gss_stat != SSH_GSS_S_COMPLETE &&
                s->gss_stat != SSH_GSS_S_CONTINUE_NEEDED) {
                if (s->shgss->lib->display_status(
                        s->shgss->lib, s->shgss->ctx,
                        &s->gss_buf) == SSH_GSS_OK) {
                    char *err = s->gss_buf.value;
                    ssh_sw_abort(s->ppl.ssh,
                                 "GSSAPI key exchange failed to initialise "
                                 "context: %s", err);
                    sfree(err);
                    return;
                }
            }
            assert(s->gss_stat == SSH_GSS_S_COMPLETE ||
                   s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED);

            if (!s->init_token_sent) {
                s->init_token_sent = 1;
                pktout = ssh_bpp_new_pktout(s->ppl.bpp,
                                            SSH2_MSG_KEXGSS_INIT);
                if (s->gss_sndtok.length == 0) {
                    ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange failed: "
                                 "no initial context token");
                    return;
                }
                put_string(pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                put_mp_ssh2(pktout, s->e);
                pq_push(s->ppl.out_pq, pktout);
                s->shgss->lib->free_tok(s->shgss->lib, &s->gss_sndtok);
                ppl_logevent(("GSSAPI key exchange initialised"));
            } else if (s->gss_sndtok.length != 0) {
                pktout = ssh_bpp_new_pktout(
                    s->ppl.bpp, SSH2_MSG_KEXGSS_CONTINUE);
                put_string(pktout,
                           s->gss_sndtok.value, s->gss_sndtok.length);
                pq_push(s->ppl.out_pq, pktout);
                s->shgss->lib->free_tok(s->shgss->lib, &s->gss_sndtok);
            }

            if (s->gss_stat == SSH_GSS_S_COMPLETE && s->complete_rcvd)
                break;

          wait_for_gss_token:
            crMaybeWaitUntilV(
                (pktin = ssh2_transport_pop(s)) != NULL);
            switch (pktin->type) {
              case SSH2_MSG_KEXGSS_CONTINUE:
                data = get_string(pktin);
                s->gss_rcvtok.value = (char *)data.ptr;
                s->gss_rcvtok.length = data.len;
                continue;
              case SSH2_MSG_KEXGSS_COMPLETE:
                s->complete_rcvd = 1;
                s->f = get_mp_ssh2(pktin);
                data = get_string(pktin);
                s->mic.value = (char *)data.ptr;
                s->mic.length = data.len;
                /* Save expiration time of cred when delegating */
                if (s->gss_delegate && s->gss_cred_expiry != GSS_NO_EXPIRATION)
                    s->gss_cred_expiry = s->gss_cred_expiry;
                /* If there's a final token we loop to consume it */
                if (get_bool(pktin)) {
                    data = get_string(pktin);
                    s->gss_rcvtok.value = (char *)data.ptr;
                    s->gss_rcvtok.length = data.len;
                    continue;
                }
                break;
              case SSH2_MSG_KEXGSS_HOSTKEY:
                s->hostkeydata = get_string(pktin);
                if (s->hostkey_alg) {
                    s->hkey = ssh_key_new_pub(s->hostkey_alg,
                                              s->hostkeydata);
                    put_string(s->exhash,
                               s->hostkeydata.ptr, s->hostkeydata.len);
                }
                /*
                 * Can't loop as we have no token to pass to
                 * init_sec_context.
                 */
                goto wait_for_gss_token;
              case SSH2_MSG_KEXGSS_ERROR:
                /*
                 * We have no use for the server's major and minor
                 * status.  The minor status is really only
                 * meaningful to the server, and with luck the major
                 * status means something to us (but not really all
                 * that much).  The string is more meaningful, and
                 * hopefully the server sends any error tokens, as
                 * that will produce the most useful information for
                 * us.
                 */
                get_uint32(pktin); /* server's major status */
                get_uint32(pktin); /* server's minor status */
                data = get_string(pktin);
                ppl_logevent(("GSSAPI key exchange failed; "
                              "server's message: %.*s", PTRLEN_PRINTF(data)));
                /* Language tag, but we have no use for it */
                get_string(pktin);
                /*
                 * Wait for an error token, if there is one, or the
                 * server's disconnect.  The error token, if there
                 * is one, must follow the SSH2_MSG_KEXGSS_ERROR
                 * message, per the RFC.
                 */
                goto wait_for_gss_token;
              default:
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet "
                                "during GSSAPI key exchange, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }
        } while (s->gss_rcvtok.length ||
                 s->gss_stat == SSH_GSS_S_CONTINUE_NEEDED ||
                 !s->complete_rcvd);

        s->K = dh_find_K(s->dh_ctx, s->f);

        /* We assume everything from now on will be quick, and it might
         * involve user interaction. */
        set_busy_status(s->ppl.frontend, BUSY_NOT);

        if (!s->hkey)
            put_stringz(s->exhash, "");
        if (dh_is_gex(s->kex_alg)) {
            /* min,  preferred, max */
            put_uint32(s->exhash, s->pbits);
            put_uint32(s->exhash, s->pbits);
            put_uint32(s->exhash, s->pbits * 2);

            put_mp_ssh2(s->exhash, s->p);
            put_mp_ssh2(s->exhash, s->g);
        }
        put_mp_ssh2(s->exhash, s->e);
        put_mp_ssh2(s->exhash, s->f);

        /*
         * MIC verification is done below, after we compute the hash
         * used as the MIC input.
         */

        dh_cleanup(s->dh_ctx);
        s->dh_ctx = NULL;
        freebn(s->f); s->f = NULL;
        freebn(s->e); s->e = NULL;
        if (dh_is_gex(s->kex_alg)) {
            freebn(s->g); s->g = NULL;
            freebn(s->p); s->p = NULL;
        }
#endif
    } else {
        ptrlen rsakeydata;

        assert(s->kex_alg->main_type == KEXTYPE_RSA);
        ppl_logevent(("Doing RSA key exchange with hash %s",
                      s->kex_alg->hash->text_name));
        s->ppl.bpp->pls->kctx = SSH2_PKTCTX_RSAKEX;
        /*
         * RSA key exchange. First expect a KEXRSA_PUBKEY packet
         * from the server.
         */
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_PUBKEY) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting RSA public key, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            return;
        }

        s->hostkeydata = get_string(pktin);
        put_stringpl(s->exhash, s->hostkeydata);
        s->hkey = ssh_key_new_pub(s->hostkey_alg, s->hostkeydata);

        rsakeydata = get_string(pktin);

        s->rsa_kex_key = ssh_rsakex_newkey(rsakeydata.ptr, rsakeydata.len);
        if (!s->rsa_kex_key) {
            ssh_proto_error(s->ppl.ssh,
                            "Unable to parse RSA public key packet");
            return;
        }

        put_stringpl(s->exhash, rsakeydata);

        /*
         * Next, set up a shared secret K, of precisely KLEN -
         * 2*HLEN - 49 bits, where KLEN is the bit length of the
         * RSA key modulus and HLEN is the bit length of the hash
         * we're using.
         */
        {
            int klen = ssh_rsakex_klen(s->rsa_kex_key);
            int nbits = klen - (2*s->kex_alg->hash->hlen*8 + 49);
            int i, byte = 0;
            strbuf *buf;
            unsigned char *outstr;
            int outstrlen;

            s->K = bn_power_2(nbits - 1);

            for (i = 0; i < nbits; i++) {
                if ((i & 7) == 0) {
                    byte = random_byte();
                }
                bignum_set_bit(s->K, i, (byte >> (i & 7)) & 1);
            }

            /*
             * Encode this as an mpint.
             */
            buf = strbuf_new();
            put_mp_ssh2(buf, s->K);

            /*
             * Encrypt it with the given RSA key.
             */
            outstrlen = (klen + 7) / 8;
            outstr = snewn(outstrlen, unsigned char);
            ssh_rsakex_encrypt(s->kex_alg->hash, buf->u, buf->len,
                               outstr, outstrlen, s->rsa_kex_key);

            /*
             * And send it off in a return packet.
             */
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXRSA_SECRET);
            put_string(pktout, outstr, outstrlen);
            pq_push(s->ppl.out_pq, pktout);

            put_string(s->exhash, outstr, outstrlen);

            strbuf_free(buf);
            sfree(outstr);
        }

        ssh_rsakex_freekey(s->rsa_kex_key);
        s->rsa_kex_key = NULL;

        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_KEXRSA_DONE) {
            ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                            "expecting RSA kex signature, type %d (%s)",
                            pktin->type,
                            ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                          s->ppl.bpp->pls->actx,
                                          pktin->type));
            return;
        }

        s->sigdata = get_string(pktin);
        if (get_err(pktin)) {
            ssh_proto_error(s->ppl.ssh, "Unable to parse RSA kex signature");
            return;
        }
    }

    put_mp_ssh2(s->exhash, s->K);
    assert(ssh_hash_alg(s->exhash)->hlen <= sizeof(s->exchange_hash));
    ssh_hash_final(s->exhash, s->exchange_hash);
    s->exhash = NULL;

#ifndef NO_GSSAPI
    if (s->kex_alg->main_type == KEXTYPE_GSS) {
        Ssh_gss_buf gss_buf;
        SSH_GSS_CLEAR_BUF(&s->gss_buf);

        gss_buf.value = s->exchange_hash;
        gss_buf.length = s->kex_alg->hash->hlen;
        s->gss_stat = s->shgss->lib->verify_mic(
            s->shgss->lib, s->shgss->ctx, &gss_buf, &s->mic);
        if (s->gss_stat != SSH_GSS_OK) {
            if (s->shgss->lib->display_status(
                    s->shgss->lib, s->shgss->ctx, &s->gss_buf) == SSH_GSS_OK) {
                char *err = s->gss_buf.value;
                ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange MIC was "
                             "not valid: %s", err);
                sfree(err);
            } else {
                ssh_sw_abort(s->ppl.ssh, "GSSAPI key exchange MIC was "
                             "not valid");
            }
            return;
        }

        s->gss_kex_used = TRUE;

        /*-
         * If this the first KEX, save the GSS context for "gssapi-keyex"
         * authentication.
         *
         * http://tools.ietf.org/html/rfc4462#section-4
         *
         * This method may be used only if the initial key exchange was
         * performed using a GSS-API-based key exchange method defined in
         * accordance with Section 2.  The GSS-API context used with this
         * method is always that established during an initial GSS-API-based
         * key exchange.  Any context established during key exchange for the
         * purpose of rekeying MUST NOT be used with this method.
         */
        if (s->got_session_id) {
            s->shgss->lib->release_cred(s->shgss->lib, &s->shgss->ctx);
        }
        ppl_logevent(("GSSAPI Key Exchange complete!"));
    }
#endif

    s->dh_ctx = NULL;

#if 0
    debug(("Exchange hash is:\n"));
    dmemdump(s->exchange_hash, s->kex_alg->hash->hlen);
#endif

    /* In GSS keyex there's no hostkey signature to verify */
    if (s->kex_alg->main_type != KEXTYPE_GSS) {
        if (!s->hkey) {
            ssh_proto_error(s->ppl.ssh, "Server's host key is invalid");
            return;
        }

        if (!ssh_key_verify(
                s->hkey, s->sigdata,
                make_ptrlen(s->exchange_hash, s->kex_alg->hash->hlen))) {
#ifndef FUZZING
            ssh_proto_error(s->ppl.ssh, "Signature from server's host key "
                            "is invalid");
            return;
#endif
        }
    }

    s->keystr = (s->hkey ? ssh_key_cache_str(s->hkey) : NULL);
#ifndef NO_GSSAPI
    if (s->gss_kex_used) {
        /*
         * In a GSS-based session, check the host key (if any) against
         * the transient host key cache. See comment above, at the
         * definition of ssh_transient_hostkey_cache_entry.
         */
        if (s->kex_alg->main_type == KEXTYPE_GSS) {

            /*
             * We've just done a GSS key exchange. If it gave us a
             * host key, store it.
             */
            if (s->hkey) {
                s->fingerprint = ssh2_fingerprint(s->hkey);
                ppl_logevent(("GSS kex provided fallback host key:"));
                ppl_logevent(("%s", s->fingerprint));
                sfree(s->fingerprint);
                s->fingerprint = NULL;
                ssh_store_transient_hostkey(s, s->hkey);
            } else if (!ssh_have_any_transient_hostkey(s)) {
                /*
                 * But if it didn't, then we currently have no
                 * fallback host key to use in subsequent non-GSS
                 * rekeys. So we should immediately trigger a non-GSS
                 * rekey of our own, to set one up, before the session
                 * keys have been used for anything else.
                 *
                 * This is similar to the cross-certification done at
                 * user request in the permanent host key cache, but
                 * here we do it automatically, once, at session
                 * startup, and only add the key to the transient
                 * cache.
                 */
                if (s->hostkey_alg) {
                    s->need_gss_transient_hostkey = TRUE;
                } else {
                    /*
                     * If we negotiated the "null" host key algorithm
                     * in the key exchange, that's an indication that
                     * no host key at all is available from the server
                     * (both because we listed "null" last, and
                     * because RFC 4462 section 5 says that a server
                     * MUST NOT offer "null" as a host key algorithm
                     * unless that is the only algorithm it provides
                     * at all).
                     *
                     * In that case we actually _can't_ perform a
                     * non-GSSAPI key exchange, so it's pointless to
                     * attempt one proactively. This is also likely to
                     * cause trouble later if a rekey is required at a
                     * moment whne GSS credentials are not available,
                     * but someone setting up a server in this
                     * configuration presumably accepts that as a
                     * consequence.
                     */
                    if (!s->warned_about_no_gss_transient_hostkey) {
                        ppl_logevent(("No fallback host key available"));
                        s->warned_about_no_gss_transient_hostkey = TRUE;
                    }
                }
            }
        } else {
            /*
             * We've just done a fallback key exchange, so make
             * sure the host key it used is in the cache of keys
             * we previously received in GSS kexes.
             *
             * An exception is if this was the non-GSS key exchange we
             * triggered on purpose to populate the transient cache.
             */
            assert(s->hkey);  /* only KEXTYPE_GSS lets this be null */
            s->fingerprint = ssh2_fingerprint(s->hkey);

            if (s->need_gss_transient_hostkey) {
                ppl_logevent(("Post-GSS rekey provided fallback host key:"));
                ppl_logevent(("%s", s->fingerprint));
                ssh_store_transient_hostkey(s, s->hkey);
                s->need_gss_transient_hostkey = FALSE;
            } else if (!ssh_verify_transient_hostkey(s, s->hkey)) {
                ppl_logevent(("Non-GSS rekey after initial GSS kex "
                              "used host key:"));
                ppl_logevent(("%s", s->fingerprint));
                ssh_sw_abort(s->ppl.ssh, "Server's host key did not match any "
                             "used in previous GSS kex");
                return;
            }

            sfree(s->fingerprint);
            s->fingerprint = NULL;
        }
    } else
#endif /* NO_GSSAPI */
        if (!s->got_session_id) {
            /*
             * Make a note of any other host key formats that are available.
             */
            {
                int i, j, nkeys = 0;
                char *list = NULL;
                for (i = 0; i < lenof(hostkey_algs); i++) {
                    if (hostkey_algs[i].alg == s->hostkey_alg)
                        continue;

                    for (j = 0; j < s->n_uncert_hostkeys; j++)
                        if (s->uncert_hostkeys[j] == i)
                            break;

                    if (j < s->n_uncert_hostkeys) {
                        char *newlist;
                        if (list)
                            newlist = dupprintf("%s/%s", list,
                                                hostkey_algs[i].alg->ssh_id);
                        else
                            newlist = dupprintf("%s", hostkey_algs[i].alg->ssh_id);
                        sfree(list);
                        list = newlist;
                        nkeys++;
                    }
                }
                if (list) {
                    ppl_logevent(("Server also has %s host key%s, but we "
                                  "don't know %s", list,
                                  nkeys > 1 ? "s" : "",
                                  nkeys > 1 ? "any of them" : "it"));
                    sfree(list);
                }
            }

            /*
             * Authenticate remote host: verify host key. (We've already
             * checked the signature of the exchange hash.)
             */
            s->fingerprint = ssh2_fingerprint(s->hkey);
            ppl_logevent(("Host key fingerprint is:"));
            ppl_logevent(("%s", s->fingerprint));
            /* First check against manually configured host keys. */
            s->dlgret = verify_ssh_manual_host_key(
                s->conf, s->fingerprint, s->hkey);
            if (s->dlgret == 0) {          /* did not match */
                ssh_sw_abort(s->ppl.ssh, "Host key did not appear in manually "
                             "configured list");
                return;
            } else if (s->dlgret < 0) { /* none configured; use standard handling */
                s->dlgret = verify_ssh_host_key(s->ppl.frontend,
                                                s->savedhost, s->savedport,
                                                ssh_key_cache_id(s->hkey),
                                                s->keystr, s->fingerprint,
                                                ssh2_transport_dialog_callback, s);
#ifdef FUZZING
                s->dlgret = 1;
#endif
                crMaybeWaitUntilV(s->dlgret >= 0);
                if (s->dlgret == 0) {
                    ssh_user_close(s->ppl.ssh,
                                   "User aborted at host key verification");
                    return;
                }
            }
            sfree(s->fingerprint);
            s->fingerprint = NULL;
            /*
             * Save this host key, to check against the one presented in
             * subsequent rekeys.
             */
            s->hostkey_str = s->keystr;
            s->keystr = NULL;
        } else if (s->cross_certifying) {
            s->fingerprint = ssh2_fingerprint(s->hkey);
            ppl_logevent(("Storing additional host key for this host:"));
            ppl_logevent(("%s", s->fingerprint));
            sfree(s->fingerprint);
            s->fingerprint = NULL;
            store_host_key(s->savedhost, s->savedport,
                           ssh_key_cache_id(s->hkey), s->keystr);
            s->cross_certifying = FALSE;
            /*
             * Don't forget to store the new key as the one we'll be
             * re-checking in future normal rekeys.
             */
            s->hostkey_str = s->keystr;
            s->keystr = NULL;
        } else {
            /*
             * In a rekey, we never present an interactive host key
             * verification request to the user. Instead, we simply
             * enforce that the key we're seeing this time is identical to
             * the one we saw before.
             */
            if (strcmp(s->hostkey_str, s->keystr)) {
#ifndef FUZZING
                ssh_sw_abort(s->ppl.ssh,
                             "Host key was different in repeat key exchange");
                return;
#endif
            }
        }
    sfree(s->keystr);
    s->keystr = NULL;
    if (s->hkey) {
        ssh_key_free(s->hkey);
        s->hkey = NULL;
    }

    /*
     * The exchange hash from the very first key exchange is also
     * the session id, used in session key construction and
     * authentication.
     */
    if (!s->got_session_id) {
        assert(sizeof(s->exchange_hash) <= sizeof(s->session_id));
        memcpy(s->session_id, s->exchange_hash, sizeof(s->exchange_hash));
        s->session_id_len = s->kex_alg->hash->hlen;
        assert(s->session_id_len <= sizeof(s->session_id));
        s->got_session_id = TRUE;
    }

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_NEWKEYS);
    pq_push(s->ppl.out_pq, pktout);
    /* Start counting down the outgoing-data limit for these cipher keys. */
    s->stats->out.running = TRUE;
    s->stats->out.remaining = s->max_data_size;

    /*
     * Force the BPP to synchronously marshal all packets up to and
     * including that NEWKEYS into wire format, before we switch over
     * to new crypto.
     */
    ssh_bpp_handle_output(s->ppl.bpp);

    /*
     * We've sent client NEWKEYS, so create and initialise
     * client-to-server session keys.
     */
    {
        strbuf *cipher_key = strbuf_new();
        strbuf *cipher_iv = strbuf_new();
        strbuf *mac_key = strbuf_new();

        if (s->out.cipher) {
            ssh2_mkkey(s, cipher_iv, s->K, s->exchange_hash, 'A',
                       s->out.cipher->blksize);
            ssh2_mkkey(s, cipher_key, s->K, s->exchange_hash, 'C',
                       s->out.cipher->padded_keybytes);
        }
        if (s->out.mac) {
            ssh2_mkkey(s, mac_key, s->K, s->exchange_hash, 'E',
                       s->out.mac->keylen);
        }

        ssh2_bpp_new_outgoing_crypto(
            s->ppl.bpp,
            s->out.cipher, cipher_key->u, cipher_iv->u,
            s->out.mac, s->out.etm_mode, mac_key->u,
            s->out.comp);

        strbuf_free(cipher_key);
        strbuf_free(cipher_iv);
        strbuf_free(mac_key);
    }

    /*
     * Now our end of the key exchange is complete, we can send all
     * our queued higher-layer packets. Transfer the whole of the next
     * layer's outgoing queue on to our own.
     */
    pq_concatenate(s->ppl.out_pq, s->ppl.out_pq, &s->pq_out_higher);

    /*
     * Expect SSH2_MSG_NEWKEYS from server.
     */
    crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
    if (pktin->type != SSH2_MSG_NEWKEYS) {
        ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                        "expecting SSH_MSG_NEWKEYS, type %d (%s)",
                        pktin->type,
                        ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                      s->ppl.bpp->pls->actx,
                                      pktin->type));
        return;
    }
    /* Start counting down the incoming-data limit for these cipher keys. */
    s->stats->in.running = TRUE;
    s->stats->in.remaining = s->max_data_size;

    /*
     * We've seen server NEWKEYS, so create and initialise
     * server-to-client session keys.
     */
    {
        strbuf *cipher_key = strbuf_new();
        strbuf *cipher_iv = strbuf_new();
        strbuf *mac_key = strbuf_new();

        if (s->in.cipher) {
            ssh2_mkkey(s, cipher_iv, s->K, s->exchange_hash, 'B',
                       s->in.cipher->blksize);
            ssh2_mkkey(s, cipher_key, s->K, s->exchange_hash, 'D',
                       s->in.cipher->padded_keybytes);
        }
        if (s->in.mac) {
            ssh2_mkkey(s, mac_key, s->K, s->exchange_hash, 'F',
                       s->in.mac->keylen);
        }

        ssh2_bpp_new_incoming_crypto(
            s->ppl.bpp,
            s->in.cipher, cipher_key->u, cipher_iv->u,
            s->in.mac, s->in.etm_mode, mac_key->u,
            s->in.comp);

        strbuf_free(cipher_key);
        strbuf_free(cipher_iv);
        strbuf_free(mac_key);
    }

    /*
     * Free shared secret.
     */
    freebn(s->K); s->K = NULL;

    /*
     * Update the specials menu to list the remaining uncertified host
     * keys.
     */
    update_specials_menu(s->ppl.frontend);

    /*
     * Key exchange is over. Loop straight back round if we have a
     * deferred rekey reason.
     */
    if (s->deferred_rekey_reason) {
        ppl_logevent(("%s", s->deferred_rekey_reason));
        pktin = NULL;
        s->deferred_rekey_reason = NULL;
        goto begin_key_exchange;
    }

    /*
     * Otherwise, schedule a timer for our next rekey.
     */
    s->kex_in_progress = FALSE;
    s->last_rekey = GETTICKCOUNT();
    (void) ssh2_transport_timer_update(s, 0);

    /*
     * Now we're encrypting. Get the next-layer protocol started if it
     * hasn't already, and then sit here waiting for reasons to go
     * back to the start and do a repeat key exchange. One of those
     * reasons is that we receive KEXINIT from the other end; the
     * other is if we find rekey_reason is non-NULL, i.e. we've
     * decided to initiate a rekey ourselves for some reason.
     */
    if (!s->higher_layer_ok) {
        pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_SERVICE_REQUEST);
        put_stringz(pktout, s->higher_layer->vt->name);
        pq_push(s->ppl.out_pq, pktout);
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
        if (pktin->type != SSH2_MSG_SERVICE_ACCEPT) {
            ssh_sw_abort(s->ppl.ssh, "Server refused request to start "
                         "'%s' protocol", s->higher_layer->vt->name);
            return;
        }

        s->higher_layer_ok = TRUE;
        queue_idempotent_callback(&s->higher_layer->ic_process_queue);
    }

    s->rekey_class = RK_NONE;
    do {
        crReturnV;

        /* Pass through outgoing packets from the higher layer. */
        pq_concatenate(s->ppl.out_pq, s->ppl.out_pq, &s->pq_out_higher);

        /* Wait for either a KEXINIT, or something setting
         * s->rekey_class. This call to ssh2_transport_pop also has
         * the side effect of transferring incoming packets _to_ the
         * higher layer (via filter_queue). */
        if ((pktin = ssh2_transport_pop(s)) != NULL) {
            if (pktin->type != SSH2_MSG_KEXINIT) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected transport-"
                                "layer packet outside a key exchange, "
                                "type %d (%s)", pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }
            pq_push_front(s->ppl.in_pq, pktin);
            ppl_logevent(("Server initiated key re-exchange"));
            s->rekey_class = RK_SERVER;
        }

        if (s->rekey_class == RK_POST_USERAUTH) {
            /*
             * userauth has seen a USERAUTH_SUCCEEDED. For a couple of
             * reasons, this may be the moment to do an immediate
             * rekey with different parameters. But it may not; so
             * here we turn that rekey class into either RK_NONE or
             * RK_NORMAL.
             *
             * One is to turn on delayed compression. We do this by a
             * rekey to work around a protocol design bug:
             * draft-miller-secsh-compression-delayed-00 says that you
             * negotiate delayed compression in the first key
             * exchange, and both sides start compressing when the
             * server has sent USERAUTH_SUCCESS. This has a race
             * condition -- the server can't know when the client has
             * seen it, and thus which incoming packets it should
             * treat as compressed.
             *
             * Instead, we do the initial key exchange without
             * offering the delayed methods, but note if the server
             * offers them; when we get here, if a delayed method was
             * available that was higher on our list than what we got,
             * we initiate a rekey in which we _do_ list the delayed
             * methods (and hopefully get it as a result). Subsequent
             * rekeys will do the same.
             *
             * Another reason for a rekey at this point is if we've
             * done a GSS key exchange and don't have anything in our
             * transient hostkey cache, in which case we should make
             * an attempt to populate the cache now.
             */
            assert(!s->userauth_succeeded); /* should only happen once */
            s->userauth_succeeded = TRUE;
            if (s->pending_compression) {
                s->rekey_reason = "enabling delayed compression";
                s->rekey_class = RK_NORMAL;
            } else if (s->need_gss_transient_hostkey) {
                s->rekey_reason = "populating transient host key cache";
                s->rekey_class = RK_NORMAL;
            } else {
                /* No need to rekey at this time. */
                s->rekey_class = RK_NONE;
            }
        }

        if (!s->rekey_class) {
            /* If we don't yet have any other reason to rekey, check
             * if we've hit our data limit in either direction. */
            if (!s->stats->in.running) {
                s->rekey_reason = "too much data received";
                s->rekey_class = RK_NORMAL;
            } else if (!s->stats->out.running) {
                s->rekey_reason = "too much data sent";
                s->rekey_class = RK_NORMAL;
            }
        }

        if (s->rekey_class != RK_NONE && s->rekey_class != RK_SERVER) {
            /*
             * Special case: if the server bug is set that doesn't
             * allow rekeying, we give a different log message and
             * continue waiting. (If such a server _initiates_ a
             * rekey, we process it anyway!)
             */
            if ((s->ppl.remote_bugs & BUG_SSH2_REKEY)) {
                ppl_logevent(("Server bug prevents key re-exchange (%s)",
                              s->rekey_reason));
                /* Reset the counters, so that at least this message doesn't
                 * hit the event log _too_ often. */
                s->stats->in.running = s->stats->out.running = TRUE;
                s->stats->in.remaining = s->stats->out.remaining =
                    s->max_data_size;
                (void) ssh2_transport_timer_update(s, 0);
                s->rekey_class = RK_NONE;
            } else {
                ppl_logevent(("Initiating key re-exchange (%s)",
                              s->rekey_reason));
            }
        }
    } while (s->rekey_class == RK_NONE);

    /* Once we exit the above loop, we really are rekeying. */
    goto begin_key_exchange;

    crFinishV;
}

static void ssh2_transport_higher_layer_packet_callback(void *context)
{
    PacketProtocolLayer *ppl = (PacketProtocolLayer *)context;
    ssh_ppl_process_queue(ppl);
}

static void ssh2_transport_timer(void *ctx, unsigned long now)
{
    struct ssh2_transport_state *s = (struct ssh2_transport_state *)ctx;
    unsigned long mins;
    unsigned long ticks;

    if (s->kex_in_progress || now != s->next_rekey)
        return;

    mins = sanitise_rekey_time(conf_get_int(s->conf, CONF_ssh_rekey_time), 60);
    if (mins == 0)
	return;

    /* Rekey if enough time has elapsed */
    ticks = mins * 60 * TICKSPERSEC;
    if (now - s->last_rekey > ticks - 30*TICKSPERSEC) {
        s->rekey_reason = "timeout";
        s->rekey_class = RK_NORMAL;
        queue_idempotent_callback(&s->ppl.ic_process_queue);
        return;
    }

#ifndef NO_GSSAPI
    /*
     * Rekey now if we have a new cred or context expires this cycle,
     * but not if this is unsafe.
     */
    if (conf_get_int(s->conf, CONF_gssapirekey)) {
        ssh2_transport_gss_update(s, FALSE);
        if ((s->gss_status & GSS_KEX_CAPABLE) != 0 &&
            (s->gss_status & GSS_CTXT_MAYFAIL) == 0 &&
            (s->gss_status & (GSS_CRED_UPDATED|GSS_CTXT_EXPIRES)) != 0) {
            s->rekey_reason = "GSS credentials updated";
            s->rekey_class = RK_GSS_UPDATE;
            queue_idempotent_callback(&s->ppl.ic_process_queue);
            return;
        }
    }
#endif

    /* Try again later. */
    (void) ssh2_transport_timer_update(s, 0);
}

/*
 * The rekey_time is zero except when re-configuring.
 *
 * We either schedule the next timer and return 0, or return 1 to run the
 * callback now, which will call us again to re-schedule on completion.
 */
static int ssh2_transport_timer_update(struct ssh2_transport_state *s,
                                       unsigned long rekey_time)
{
    unsigned long mins;
    unsigned long ticks;

    mins = sanitise_rekey_time(conf_get_int(s->conf, CONF_ssh_rekey_time), 60);
    ticks = mins * 60 * TICKSPERSEC;

    /* Handle change from previous setting */
    if (rekey_time != 0 && rekey_time != mins) {
        unsigned long next;
        unsigned long now = GETTICKCOUNT();

        mins = rekey_time;
        ticks = mins * 60 * TICKSPERSEC;
        next = s->last_rekey + ticks;

        /* If overdue, caller will rekey synchronously now */
        if (now - s->last_rekey > ticks)
            return 1;
        ticks = next - now;
    }

#ifndef NO_GSSAPI
    if (s->gss_kex_used) {
        /*
         * If we've used GSSAPI key exchange, then we should
         * periodically check whether we need to do another one to
         * pass new credentials to the server.
         */
        unsigned long gssmins;

        /* Check cascade conditions more frequently if configured */
        gssmins = sanitise_rekey_time(
            conf_get_int(s->conf, CONF_gssapirekey), GSS_DEF_REKEY_MINS);
        if (gssmins > 0) {
            if (gssmins < mins)
                ticks = (mins = gssmins) * 60 * TICKSPERSEC;

            if ((s->gss_status & GSS_KEX_CAPABLE) != 0) {
                /*
                 * Run next timer even sooner if it would otherwise be
                 * too close to the context expiration time
                 */
                if ((s->gss_status & GSS_CTXT_EXPIRES) == 0 &&
                    s->gss_ctxt_lifetime - mins * 60 < 2 * MIN_CTXT_LIFETIME)
                    ticks -= 2 * MIN_CTXT_LIFETIME * TICKSPERSEC;
            }
        }
    }
#endif

    /* Schedule the next timer */
    s->next_rekey = schedule_timer(ticks, ssh2_transport_timer, s);
    return 0;
}

static void ssh2_transport_dialog_callback(void *loginv, int ret)
{
    struct ssh2_transport_state *s = (struct ssh2_transport_state *)loginv;
    s->dlgret = ret;
    ssh_ppl_process_queue(&s->ppl);
}

#ifndef NO_GSSAPI
/*
 * This is called at the beginning of each SSH rekey to determine
 * whether we are GSS capable, and if we did GSS key exchange, and are
 * delegating credentials, it is also called periodically to determine
 * whether we should rekey in order to delegate (more) fresh
 * credentials. This is called "credential cascading".
 *
 * On Windows, with SSPI, we may not get the credential expiration, as
 * Windows automatically renews from cached passwords, so the
 * credential effectively never expires. Since we still want to
 * cascade when the local TGT is updated, we use the expiration of a
 * newly obtained context as a proxy for the expiration of the TGT.
 */
static void ssh2_transport_gss_update(struct ssh2_transport_state *s,
                                      int definitely_rekeying)
{
    PacketProtocolLayer *ppl = &s->ppl; /* for ppl_logevent */
    int gss_stat;
    time_t gss_cred_expiry;
    unsigned long mins;
    Ssh_gss_buf gss_sndtok;
    Ssh_gss_buf gss_rcvtok;
    Ssh_gss_ctx gss_ctx;

    s->gss_status = 0;

    /*
     * Nothing to do if no GSSAPI libraries are configured or GSSAPI
     * auth is not enabled.
     */
    if (s->shgss->libs->nlibraries == 0)
        return;
    if (!conf_get_int(s->conf, CONF_try_gssapi_auth) &&
        !conf_get_int(s->conf, CONF_try_gssapi_kex))
        return;

    /* Import server name and cache it */
    if (s->shgss->srv_name == GSS_C_NO_NAME) {
        gss_stat = s->shgss->lib->import_name(
            s->shgss->lib, s->fullhostname, &s->shgss->srv_name);
        if (gss_stat != SSH_GSS_OK) {
            if (gss_stat == SSH_GSS_BAD_HOST_NAME)
                ppl_logevent(("GSSAPI import name failed - Bad service name;"
                              " won't use GSS key exchange"));
            else
                ppl_logevent(("GSSAPI import name failed;"
                              " won't use GSS key exchange"));
            return;
        }
    }

    /*
     * Do we (still) have credentials? Capture the credential
     * expiration when available
     */
    gss_stat = s->shgss->lib->acquire_cred(
        s->shgss->lib, &gss_ctx, &gss_cred_expiry);
    if (gss_stat != SSH_GSS_OK)
        return;

    SSH_GSS_CLEAR_BUF(&gss_sndtok);
    SSH_GSS_CLEAR_BUF(&gss_rcvtok);

    /*
     * When acquire_cred yields no useful expiration, get a proxy for
     * the cred expiration from the context expiration.
     */
    gss_stat = s->shgss->lib->init_sec_context(
        s->shgss->lib, &gss_ctx, s->shgss->srv_name,
        0 /* don't delegate */, &gss_rcvtok, &gss_sndtok,
        (gss_cred_expiry == GSS_NO_EXPIRATION ? &gss_cred_expiry : NULL),
        &s->gss_ctxt_lifetime);

    /* This context was for testing only. */
    if (gss_ctx)
        s->shgss->lib->release_cred(s->shgss->lib, &gss_ctx);

    if (gss_stat != SSH_GSS_OK &&
        gss_stat != SSH_GSS_S_CONTINUE_NEEDED) {
        /*
         * No point in verbosely interrupting the user to tell them we
         * couldn't get GSS credentials, if this was only a check
         * between key exchanges to see if fresh ones were available.
         * When we do do a rekey, this message (if displayed) will
         * appear among the standard rekey blurb, but when we're not,
         * it shouldn't pop up all the time regardless.
         */
        if (definitely_rekeying)
            ppl_logevent(("No GSSAPI security context available"));

        return;
    }

    if (gss_sndtok.length)
        s->shgss->lib->free_tok(s->shgss->lib, &gss_sndtok);

    s->gss_status |= GSS_KEX_CAPABLE;

    /*
     * When rekeying to cascade, avoding doing this too close to the
     * context expiration time, since the key exchange might fail.
     */
    if (s->gss_ctxt_lifetime < MIN_CTXT_LIFETIME)
        s->gss_status |= GSS_CTXT_MAYFAIL;

    /*
     * If we're not delegating credentials, rekeying is not used to
     * refresh them. We must avoid setting GSS_CRED_UPDATED or
     * GSS_CTXT_EXPIRES when credential delegation is disabled.
     */
    if (conf_get_int(s->conf, CONF_gssapifwd) == 0)
        return;

    if (s->gss_cred_expiry != GSS_NO_EXPIRATION &&
        difftime(gss_cred_expiry, s->gss_cred_expiry) > 0)
        s->gss_status |= GSS_CRED_UPDATED;

    mins = sanitise_rekey_time(
        conf_get_int(s->conf, CONF_gssapirekey), GSS_DEF_REKEY_MINS);
    if (mins > 0 && s->gss_ctxt_lifetime <= mins * 60)
        s->gss_status |= GSS_CTXT_EXPIRES;
}

/*
 * Data structure managing host keys in sessions based on GSSAPI KEX.
 *
 * In a session we started with a GSSAPI key exchange, the concept of
 * 'host key' has completely different lifetime and security semantics
 * from the usual ones. Per RFC 4462 section 2.1, we assume that any
 * host key delivered to us in the course of a GSSAPI key exchange is
 * _solely_ there to use as a transient fallback within the same
 * session, if at the time of a subsequent rekey the GSS credentials
 * are temporarily invalid and so a non-GSS KEX method has to be used.
 *
 * In particular, in a GSS-based SSH deployment, host keys may not
 * even _be_ persistent identities for the server; it would be
 * legitimate for a server to generate a fresh one routinely if it
 * wanted to, like SSH-1 server keys.
 *
 * So, in this mode, we never touch the persistent host key cache at
 * all, either to check keys against it _or_ to store keys in it.
 * Instead, we maintain an in-memory cache of host keys that have been
 * mentioned in GSS key exchanges within this particular session, and
 * we permit precisely those host keys in non-GSS rekeys.
 */
struct ssh_transient_hostkey_cache_entry {
    const ssh_keyalg *alg;
    strbuf *pub_blob;
};

static int ssh_transient_hostkey_cache_cmp(void *av, void *bv)
{
    const struct ssh_transient_hostkey_cache_entry
        *a = (const struct ssh_transient_hostkey_cache_entry *)av,
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(a->alg->ssh_id, b->alg->ssh_id);
}

static int ssh_transient_hostkey_cache_find(void *av, void *bv)
{
    const ssh_keyalg *aalg = (const ssh_keyalg *)av;
    const struct ssh_transient_hostkey_cache_entry
        *b = (const struct ssh_transient_hostkey_cache_entry *)bv;
    return strcmp(aalg->ssh_id, b->alg->ssh_id);
}

static void ssh_init_transient_hostkey_store(
    struct ssh2_transport_state *s)
{
    s->transient_hostkey_cache =
        newtree234(ssh_transient_hostkey_cache_cmp);
}

static void ssh_cleanup_transient_hostkey_store(
    struct ssh2_transport_state *s)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    while ((ent = delpos234(s->transient_hostkey_cache, 0)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }
    freetree234(s->transient_hostkey_cache);
}

static void ssh_store_transient_hostkey(
    struct ssh2_transport_state *s, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent, *retd;

    if ((ent = find234(s->transient_hostkey_cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf_free(ent->pub_blob);
        sfree(ent);
    }

    ent = snew(struct ssh_transient_hostkey_cache_entry);
    ent->alg = ssh_key_alg(key);
    ent->pub_blob = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(ent->pub_blob));
    retd = add234(s->transient_hostkey_cache, ent);
    assert(retd == ent);
}

static int ssh_verify_transient_hostkey(
    struct ssh2_transport_state *s, ssh_key *key)
{
    struct ssh_transient_hostkey_cache_entry *ent;
    int toret = FALSE;

    if ((ent = find234(s->transient_hostkey_cache, (void *)ssh_key_alg(key),
                       ssh_transient_hostkey_cache_find)) != NULL) {
        strbuf *this_blob = strbuf_new();
        ssh_key_public_blob(key, BinarySink_UPCAST(this_blob));

        if (this_blob->len == ent->pub_blob->len &&
            !memcmp(this_blob->s, ent->pub_blob->s,
                    this_blob->len))
            toret = TRUE;

        strbuf_free(this_blob);
    }

    return toret;
}

static int ssh_have_transient_hostkey(
    struct ssh2_transport_state *s, const ssh_keyalg *alg)
{
    struct ssh_transient_hostkey_cache_entry *ent =
        find234(s->transient_hostkey_cache, (void *)alg,
                ssh_transient_hostkey_cache_find);
    return ent != NULL;
}

static int ssh_have_any_transient_hostkey(
    struct ssh2_transport_state *s)
{
    return count234(s->transient_hostkey_cache) > 0;
}

ptrlen ssh2_transport_get_session_id(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s;

    assert(ppl->vt == &ssh2_transport_vtable);
    s = container_of(ppl, struct ssh2_transport_state, ppl);

    assert(s->got_session_id);
    return make_ptrlen(s->session_id, s->session_id_len);
}

void ssh2_transport_notify_auth_done(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s;

    assert(ppl->vt == &ssh2_transport_vtable);
    s = container_of(ppl, struct ssh2_transport_state, ppl);

    s->rekey_reason = NULL;            /* will be filled in later */
    s->rekey_class = RK_POST_USERAUTH;
    queue_idempotent_callback(&s->ppl.ic_process_queue);
}

#endif /* NO_GSSAPI */

static int ssh2_transport_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);
    int need_separator = FALSE;
    int toret;

    if (ssh_ppl_get_specials(s->higher_layer, add_special, ctx)) {
        need_separator = TRUE;
        toret = TRUE;
    }

    /*
     * Don't bother offering rekey-based specials if we've decided the
     * remote won't cope with it, since we wouldn't bother sending it
     * if asked anyway.
     */
    if (!(s->ppl.remote_bugs & BUG_SSH2_REKEY)) {
        if (need_separator) {
            add_special(ctx, NULL, SS_SEP, 0);
            need_separator = FALSE;
        }

        add_special(ctx, "Repeat key exchange", SS_REKEY, 0);
        toret = TRUE;

        if (s->n_uncert_hostkeys) {
            int i;

            add_special(ctx, NULL, SS_SEP, 0);
            add_special(ctx, "Cache new host key type", SS_SUBMENU, 0);
            for (i = 0; i < s->n_uncert_hostkeys; i++) {
                const ssh_keyalg *alg =
                    hostkey_algs[s->uncert_hostkeys[i]].alg;

                add_special(ctx, alg->ssh_id, SS_XCERT, s->uncert_hostkeys[i]);
            }
            add_special(ctx, NULL, SS_EXITMENU, 0);
        }
    }

    return toret;
}

static void ssh2_transport_special_cmd(PacketProtocolLayer *ppl,
                                       SessionSpecialCode code, int arg)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);

    if (code == SS_REKEY) {
	if (!s->kex_in_progress) {
            s->rekey_reason = "at user request";
            s->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&s->ppl.ic_process_queue);
	}
    } else if (code == SS_XCERT) {
	if (!s->kex_in_progress) {
            s->hostkey_alg = hostkey_algs[arg].alg;
            s->cross_certifying = TRUE;
            s->rekey_reason = "cross-certifying new host key";
            s->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&s->ppl.ic_process_queue);
	}
    } else {
        /* Send everything else to the next layer up. This includes
         * SS_PING/SS_NOP, which we _could_ handle here - but it's
         * better to put them in the connection layer, so they'll
         * still work in bare connection mode. */
        ssh_ppl_special_cmd(s->higher_layer, code, arg);
    }
}

/* Safely convert rekey_time to unsigned long minutes */
static unsigned long sanitise_rekey_time(int rekey_time, unsigned long def)
{
    if (rekey_time < 0 || rekey_time > MAX_TICK_MINS)
        rekey_time = def;
    return (unsigned long)rekey_time;
}

static void ssh2_transport_set_max_data_size(struct ssh2_transport_state *s)
{
    s->max_data_size = parse_blocksize(
        conf_get_str(s->conf, CONF_ssh_rekey_data));
}

static void ssh2_transport_reconfigure(PacketProtocolLayer *ppl, Conf *conf)
{
    struct ssh2_transport_state *s;
    const char *rekey_reason = NULL;
    int rekey_mandatory = FALSE;
    unsigned long old_max_data_size, rekey_time;
    int i;

    assert(ppl->vt == &ssh2_transport_vtable);
    s = container_of(ppl, struct ssh2_transport_state, ppl);

    rekey_time = sanitise_rekey_time(
        conf_get_int(conf, CONF_ssh_rekey_time), 60);
    if (ssh2_transport_timer_update(s, rekey_time))
        rekey_reason = "timeout shortened";

    old_max_data_size = s->max_data_size;
    ssh2_transport_set_max_data_size(s);
    if (old_max_data_size != s->max_data_size &&
	s->max_data_size != 0) {
        if (s->max_data_size < old_max_data_size) {
            unsigned long diff = old_max_data_size - s->max_data_size;

            /* Intentionally use bitwise OR instead of logical, so
             * that we decrement both counters even if the first one
             * runs out */
            if ((DTS_CONSUME(s->stats, out, diff) != 0) |
                (DTS_CONSUME(s->stats, in, diff) != 0))
                rekey_reason = "data limit lowered";
        } else {
            unsigned long diff = s->max_data_size - old_max_data_size;
            if (s->stats->out.running)
                s->stats->out.remaining += diff;
            if (s->stats->in.running)
                s->stats->in.remaining += diff;
        }
    }

    if (conf_get_int(s->conf, CONF_compression) !=
	conf_get_int(conf, CONF_compression)) {
        rekey_reason = "compression setting changed";
        rekey_mandatory = TRUE;
    }

    for (i = 0; i < CIPHER_MAX; i++)
	if (conf_get_int_int(s->conf, CONF_ssh_cipherlist, i) !=
	    conf_get_int_int(conf, CONF_ssh_cipherlist, i)) {
        rekey_reason = "cipher settings changed";
        rekey_mandatory = TRUE;
    }
    if (conf_get_int(s->conf, CONF_ssh2_des_cbc) !=
	conf_get_int(conf, CONF_ssh2_des_cbc)) {
        rekey_reason = "cipher settings changed";
        rekey_mandatory = TRUE;
    }

    conf_free(s->conf);
    s->conf = conf_copy(conf);

    if (rekey_reason) {
        if (!s->kex_in_progress) {
            s->rekey_reason = rekey_reason;
            s->rekey_class = RK_NORMAL;
            queue_idempotent_callback(&s->ppl.ic_process_queue);
        } else if (rekey_mandatory) {
            s->deferred_rekey_reason = rekey_reason;
        }
    }

    /* Also pass the configuration along to our higher layer */
    ssh_ppl_reconfigure(s->higher_layer, conf);
}

static int ssh2_transport_want_user_input(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);

    /* Just delegate this to the higher layer */
    return ssh_ppl_want_user_input(s->higher_layer);
}

static void ssh2_transport_got_user_input(PacketProtocolLayer *ppl)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);

    /* Just delegate this to the higher layer */
    ssh_ppl_got_user_input(s->higher_layer);
}
