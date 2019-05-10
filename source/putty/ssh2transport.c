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
#include "ssh2transport.h"
#include "mpint.h"

const struct ssh_signkey_with_user_pref_id ssh2_hostkey_algs[] = {
    #define ARRAYENT_HOSTKEY_ALGORITHM(type, alg) { &alg, type },
    HOSTKEY_ALGORITHMS(ARRAYENT_HOSTKEY_ALGORITHM)
};

const static ssh2_macalg *const macs[] = {
    &ssh_hmac_sha256, &ssh_hmac_sha1, &ssh_hmac_sha1_96, &ssh_hmac_md5
};
const static ssh2_macalg *const buggymacs[] = {
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
                                const unsigned char *block, int len,
                                unsigned char **outblock, int *outlen,
                                int minlen)
{
}
static bool ssh_decomp_none_block(ssh_decompressor *handle,
                                  const unsigned char *block, int len,
                                  unsigned char **outblock, int *outlen)
{
    return false;
}
const static ssh_compression_alg ssh_comp_none = {
    "none", NULL,
    ssh_comp_none_init, ssh_comp_none_cleanup, ssh_comp_none_block,
    ssh_decomp_none_init, ssh_decomp_none_cleanup, ssh_decomp_none_block,
    NULL
};
const static ssh_compression_alg *const compressions[] = {
    &ssh_zlib, &ssh_comp_none
};

static void ssh2_transport_free(PacketProtocolLayer *);
static void ssh2_transport_process_queue(PacketProtocolLayer *);
static bool ssh2_transport_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx);
static void ssh2_transport_special_cmd(PacketProtocolLayer *ppl,
                                       SessionSpecialCode code, int arg);
static bool ssh2_transport_want_user_input(PacketProtocolLayer *ppl);
static void ssh2_transport_got_user_input(PacketProtocolLayer *ppl);
static void ssh2_transport_reconfigure(PacketProtocolLayer *ppl, Conf *conf);

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
                                      bool definitely_rekeying);
#endif

static bool ssh2_transport_timer_update(struct ssh2_transport_state *s,
                                        unsigned long rekey_time);
static int ssh2_transport_confirm_weak_crypto_primitive(
    struct ssh2_transport_state *s, const char *type, const char *name,
    const void *alg);

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

static int weak_algorithm_compare(void *av, void *bv);

PacketProtocolLayer *ssh2_transport_new(
    Conf *conf, const char *host, int port, const char *fullhostname,
    const char *client_greeting, const char *server_greeting,
    struct ssh_connection_shared_gss_state *shgss,
    struct DataTransferStats *stats, PacketProtocolLayer *higher_layer,
    bool is_server)
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
    s->hostkeyblob = strbuf_new();

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
#endif
    s->thc = ssh_transient_hostkey_cache_new();
    s->gss_kex_used = false;

    s->outgoing_kexinit = strbuf_new();
    s->incoming_kexinit = strbuf_new();
    if (is_server) {
        s->client_kexinit = s->incoming_kexinit;
        s->server_kexinit = s->outgoing_kexinit;
        s->out.mkkey_adjust = 1;
    } else {
        s->client_kexinit = s->outgoing_kexinit;
        s->server_kexinit = s->incoming_kexinit;
        s->in.mkkey_adjust = 1;
    }

    s->weak_algorithms_consented_to = newtree234(weak_algorithm_compare);

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
    strbuf_free(s->hostkeyblob);
    sfree(s->fingerprint);
    if (s->hkey && !s->hostkeys) {
        ssh_key_free(s->hkey);
        s->hkey = NULL;
    }
    if (s->f) mp_free(s->f);
    if (s->p) mp_free(s->p);
    if (s->g) mp_free(s->g);
    if (s->K) mp_free(s->K);
    if (s->dh_ctx)
        dh_cleanup(s->dh_ctx);
    if (s->rsa_kex_key)
        ssh_rsakex_freekey(s->rsa_kex_key);
    if (s->ecdh_key)
        ssh_ecdhkex_freekey(s->ecdh_key);
    if (s->exhash)
        ssh_hash_free(s->exhash);
    strbuf_free(s->outgoing_kexinit);
    strbuf_free(s->incoming_kexinit);
    ssh_transient_hostkey_cache_free(s->thc);

    freetree234(s->weak_algorithms_consented_to);

    expire_timer_context(s);
    sfree(s);
}

/*
 * SSH-2 key derivation (RFC 4253 section 7.2).
 */
static void ssh2_mkkey(
    struct ssh2_transport_state *s, strbuf *out,
    mp_int *K, unsigned char *H, char chr, int keylen)
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

bool ssh2_common_filter_queue(PacketProtocolLayer *ppl)
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
                ppl->ssh, "Remote side sent disconnect message\n"
                "type %d (%s):\n\"%.*s\"", reason,
                ((reason > 0 && reason < lenof(ssh2_disconnect_reasons)) ?
                 ssh2_disconnect_reasons[reason] : "unknown"),
                PTRLEN_PRINTF(msg));
            /* don't try to pop the queue, because we've been freed! */
            return true;               /* indicate that we've been freed */

          case SSH2_MSG_DEBUG:
            /* XXX maybe we should actually take notice of the return value */
            get_bool(pktin);
            msg = get_string(pktin);
            ppl_logevent("Remote debug message: %.*s", PTRLEN_PRINTF(msg));
            pq_pop(ppl->in_pq);
            break;

          case SSH2_MSG_IGNORE:
            /* Do nothing, because we're ignoring it! Duhh. */
            pq_pop(ppl->in_pq);
            break;

          default:
            return false;
        }
    }

    return false;
}

static bool ssh2_transport_filter_queue(struct ssh2_transport_state *s)
{
    PktIn *pktin;

    while (1) {
        if (ssh2_common_filter_queue(&s->ppl))
            return true;
        if ((pktin = pq_peek(s->ppl.in_pq)) == NULL)
            return false;

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
                return true;
            }

            pq_pop(s->ppl.in_pq);
            pq_push(&s->pq_in_higher, pktin);
        } else {
            /* Anything else is a transport-layer packet that the main
             * process_queue coroutine should handle. */
            return false;
        }
    }
}

PktIn *ssh2_transport_pop(struct ssh2_transport_state *s)
{
    if (ssh2_transport_filter_queue(s))
        return NULL;   /* we've been freed */
    return pq_pop(s->ppl.in_pq);
}

static void ssh2_write_kexinit_lists(
    BinarySink *pktout,
    struct kexinit_algorithm kexlists[NKEXLIST][MAXKEXLIST],
    Conf *conf, int remote_bugs,
    const char *hk_host, int hk_port, const ssh_keyalg *hk_prev,
    ssh_transient_hostkey_cache *thc,
    ssh_key *const *our_hostkeys, int our_nhostkeys,
    bool first_time, bool can_gssapi_keyex, bool transient_hostkey_mode)
{
    int i, j, k;
    bool warn;

    int n_preferred_kex;
    const ssh_kexes *preferred_kex[KEX_MAX + 1]; /* +1 for GSSAPI */
    int n_preferred_hk;
    int preferred_hk[HK_MAX];
    int n_preferred_ciphers;
    const ssh2_ciphers *preferred_ciphers[CIPHER_MAX];
    const ssh_compression_alg *preferred_comp;
    const ssh2_macalg *const *maclist;
    int nmacs;

    struct kexinit_algorithm *alg;

    /*
     * Set up the preferred key exchange. (NULL => warn below here)
     */
    n_preferred_kex = 0;
    if (can_gssapi_keyex)
        preferred_kex[n_preferred_kex++] = &ssh_gssk5_sha1_kex;
    for (i = 0; i < KEX_MAX; i++) {
        switch (conf_get_int_int(conf, CONF_ssh_kexlist, i)) {
          case KEX_DHGEX:
            preferred_kex[n_preferred_kex++] =
                &ssh_diffiehellman_gex;
            break;
          case KEX_DHGROUP14:
            preferred_kex[n_preferred_kex++] =
                &ssh_diffiehellman_group14;
            break;
          case KEX_DHGROUP1:
            preferred_kex[n_preferred_kex++] =
                &ssh_diffiehellman_group1;
            break;
          case KEX_RSA:
            preferred_kex[n_preferred_kex++] =
                &ssh_rsa_kex;
            break;
          case KEX_ECDH:
            preferred_kex[n_preferred_kex++] =
                &ssh_ecdh_kex;
            break;
          case KEX_WARN:
            /* Flag for later. Don't bother if it's the last in
             * the list. */
            if (i < KEX_MAX - 1) {
                preferred_kex[n_preferred_kex++] = NULL;
            }
            break;
        }
    }

    /*
     * Set up the preferred host key types. These are just the ids
     * in the enum in putty.h, so 'warn below here' is indicated
     * by HK_WARN.
     */
    n_preferred_hk = 0;
    for (i = 0; i < HK_MAX; i++) {
        int id = conf_get_int_int(conf, CONF_ssh_hklist, i);
        /* As above, don't bother with HK_WARN if it's last in the
         * list */
        if (id != HK_WARN || i < HK_MAX - 1)
            preferred_hk[n_preferred_hk++] = id;
    }

    /*
     * Set up the preferred ciphers. (NULL => warn below here)
     */
    n_preferred_ciphers = 0;
    for (i = 0; i < CIPHER_MAX; i++) {
        switch (conf_get_int_int(conf, CONF_ssh_cipherlist, i)) {
          case CIPHER_BLOWFISH:
            preferred_ciphers[n_preferred_ciphers++] = &ssh2_blowfish;
            break;
          case CIPHER_DES:
            if (conf_get_bool(conf, CONF_ssh2_des_cbc))
                preferred_ciphers[n_preferred_ciphers++] = &ssh2_des;
            break;
          case CIPHER_3DES:
            preferred_ciphers[n_preferred_ciphers++] = &ssh2_3des;
            break;
          case CIPHER_AES:
            preferred_ciphers[n_preferred_ciphers++] = &ssh2_aes;
            break;
          case CIPHER_ARCFOUR:
            preferred_ciphers[n_preferred_ciphers++] = &ssh2_arcfour;
            break;
          case CIPHER_CHACHA20:
            preferred_ciphers[n_preferred_ciphers++] = &ssh2_ccp;
            break;
          case CIPHER_WARN:
            /* Flag for later. Don't bother if it's the last in
             * the list. */
            if (i < CIPHER_MAX - 1) {
                preferred_ciphers[n_preferred_ciphers++] = NULL;
            }
            break;
        }
    }

    /*
     * Set up preferred compression.
     */
    if (conf_get_bool(conf, CONF_compression))
        preferred_comp = &ssh_zlib;
    else
        preferred_comp = &ssh_comp_none;

    for (i = 0; i < NKEXLIST; i++)
        for (j = 0; j < MAXKEXLIST; j++)
            kexlists[i][j].name = NULL;
    /* List key exchange algorithms. */
    warn = false;
    for (i = 0; i < n_preferred_kex; i++) {
        const ssh_kexes *k = preferred_kex[i];
        if (!k) warn = true;
        else for (j = 0; j < k->nkexes; j++) {
                alg = ssh2_kexinit_addalg(kexlists[KEXLIST_KEX],
                                          k->list[j]->name);
                alg->u.kex.kex = k->list[j];
                alg->u.kex.warn = warn;
            }
    }
    /* List server host key algorithms. */
    if (our_hostkeys) {
        /*
         * In server mode, we just list the algorithms that match the
         * host keys we actually have.
         */
        for (i = 0; i < our_nhostkeys; i++) {
            alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY],
                                      ssh_key_alg(our_hostkeys[i])->ssh_id);
            alg->u.hk.hostkey = ssh_key_alg(our_hostkeys[i]);
            alg->u.hk.warn = false;
        }
    } else if (first_time) {
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
        warn = false;
        for (i = 0; i < n_preferred_hk; i++) {
            if (preferred_hk[i] == HK_WARN)
                warn = true;
            for (j = 0; j < lenof(ssh2_hostkey_algs); j++) {
                if (ssh2_hostkey_algs[j].id != preferred_hk[i])
                    continue;
                if (have_ssh_host_key(hk_host, hk_port,
                                      ssh2_hostkey_algs[j].alg->cache_id)) {
                    alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY],
                                              ssh2_hostkey_algs[j].alg->ssh_id);
                    alg->u.hk.hostkey = ssh2_hostkey_algs[j].alg;
                    alg->u.hk.warn = warn;
                }
            }
        }
        warn = false;
        for (i = 0; i < n_preferred_hk; i++) {
            if (preferred_hk[i] == HK_WARN)
                warn = true;
            for (j = 0; j < lenof(ssh2_hostkey_algs); j++) {
                if (ssh2_hostkey_algs[j].id != preferred_hk[i])
                    continue;
                alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY],
                                          ssh2_hostkey_algs[j].alg->ssh_id);
                alg->u.hk.hostkey = ssh2_hostkey_algs[j].alg;
                alg->u.hk.warn = warn;
            }
        }
#ifndef NO_GSSAPI
    } else if (transient_hostkey_mode) {
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
        warn = false;
        for (i = 0; i < n_preferred_hk; i++) {
            if (preferred_hk[i] == HK_WARN)
                warn = true;
            for (j = 0; j < lenof(ssh2_hostkey_algs); j++) {
                if (ssh2_hostkey_algs[j].id != preferred_hk[i])
                    continue;
                if (ssh_transient_hostkey_cache_has(
                        thc, ssh2_hostkey_algs[j].alg)) {
                    alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY],
                                              ssh2_hostkey_algs[j].alg->ssh_id);
                    alg->u.hk.hostkey = ssh2_hostkey_algs[j].alg;
                    alg->u.hk.warn = warn;
                }
            }
        }
#endif
    } else {
        /*
         * In subsequent key exchanges, we list only the host key
         * algorithm that was selected in the first key exchange,
         * so that we keep getting the same host key and hence
         * don't have to interrupt the user's session to ask for
         * reverification.
         */
        assert(hk_prev);
        alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY], hk_prev->ssh_id);
        alg->u.hk.hostkey = hk_prev;
        alg->u.hk.warn = false;
    }
    if (can_gssapi_keyex) {
        alg = ssh2_kexinit_addalg(kexlists[KEXLIST_HOSTKEY], "null");
        alg->u.hk.hostkey = NULL;
    }
    /* List encryption algorithms (client->server then server->client). */
    for (k = KEXLIST_CSCIPHER; k <= KEXLIST_SCCIPHER; k++) {
        warn = false;
#ifdef FUZZING
        alg = ssh2_kexinit_addalg(kexlists[k], "none");
        alg->u.cipher.cipher = NULL;
        alg->u.cipher.warn = warn;
#endif /* FUZZING */
        for (i = 0; i < n_preferred_ciphers; i++) {
            const ssh2_ciphers *c = preferred_ciphers[i];
            if (!c) warn = true;
            else for (j = 0; j < c->nciphers; j++) {
                    alg = ssh2_kexinit_addalg(kexlists[k],
                                              c->list[j]->ssh2_id);
                    alg->u.cipher.cipher = c->list[j];
                    alg->u.cipher.warn = warn;
                }
        }
    }

    /*
     * Be prepared to work around the buggy MAC problem.
     */
    if (remote_bugs & BUG_SSH2_HMAC) {
        maclist = buggymacs;
        nmacs = lenof(buggymacs);
    } else {
        maclist = macs;
        nmacs = lenof(macs);
    }

    /* List MAC algorithms (client->server then server->client). */
    for (j = KEXLIST_CSMAC; j <= KEXLIST_SCMAC; j++) {
#ifdef FUZZING
        alg = ssh2_kexinit_addalg(kexlists[j], "none");
        alg->u.mac.mac = NULL;
        alg->u.mac.etm = false;
#endif /* FUZZING */
        for (i = 0; i < nmacs; i++) {
            alg = ssh2_kexinit_addalg(kexlists[j], maclist[i]->name);
            alg->u.mac.mac = maclist[i];
            alg->u.mac.etm = false;
        }
        for (i = 0; i < nmacs; i++) {
            /* For each MAC, there may also be an ETM version,
             * which we list second. */
            if (maclist[i]->etm_name) {
                alg = ssh2_kexinit_addalg(kexlists[j], maclist[i]->etm_name);
                alg->u.mac.mac = maclist[i];
                alg->u.mac.etm = true;
            }
        }
    }

    /* List client->server compression algorithms,
     * then server->client compression algorithms. (We use the
     * same set twice.) */
    for (j = KEXLIST_CSCOMP; j <= KEXLIST_SCCOMP; j++) {
        assert(lenof(compressions) > 1);
        /* Prefer non-delayed versions */
        alg = ssh2_kexinit_addalg(kexlists[j], preferred_comp->name);
        alg->u.comp.comp = preferred_comp;
        alg->u.comp.delayed = false;
        if (preferred_comp->delayed_name) {
            alg = ssh2_kexinit_addalg(kexlists[j],
                                      preferred_comp->delayed_name);
            alg->u.comp.comp = preferred_comp;
            alg->u.comp.delayed = true;
        }
        for (i = 0; i < lenof(compressions); i++) {
            const ssh_compression_alg *c = compressions[i];
            alg = ssh2_kexinit_addalg(kexlists[j], c->name);
            alg->u.comp.comp = c;
            alg->u.comp.delayed = false;
            if (c->delayed_name) {
                alg = ssh2_kexinit_addalg(kexlists[j], c->delayed_name);
                alg->u.comp.comp = c;
                alg->u.comp.delayed = true;
            }
        }
    }

    /*
     * Finally, format the lists into text and write them into the
     * outgoing KEXINIT packet.
     */
    for (i = 0; i < NKEXLIST; i++) {
        strbuf *list = strbuf_new();
        for (j = 0; j < MAXKEXLIST; j++) {
            if (kexlists[i][j].name == NULL) break;
            add_to_commasep(list, kexlists[i][j].name);
        }
        put_stringsb(pktout, list);
    }
    /* List client->server languages. Empty list. */
    put_stringz(pktout, "");
    /* List server->client languages. Empty list. */
    put_stringz(pktout, "");
}

static bool ssh2_scan_kexinits(
    ptrlen client_kexinit, ptrlen server_kexinit,
    struct kexinit_algorithm kexlists[NKEXLIST][MAXKEXLIST],
    const ssh_kex **kex_alg, const ssh_keyalg **hostkey_alg,
    transport_direction *cs, transport_direction *sc,
    bool *warn_kex, bool *warn_hk, bool *warn_cscipher, bool *warn_sccipher,
    Ssh *ssh, bool *ignore_guess_cs_packet, bool *ignore_guess_sc_packet,
    int *n_server_hostkeys, int server_hostkeys[MAXKEXLIST])
{
    BinarySource client[1], server[1];
    int i;
    bool guess_correct;
    ptrlen clists[NKEXLIST], slists[NKEXLIST];
    const struct kexinit_algorithm *selected[NKEXLIST];

    BinarySource_BARE_INIT_PL(client, client_kexinit);
    BinarySource_BARE_INIT_PL(server, server_kexinit);

    /* Skip packet type bytes and random cookies. */
    get_data(client, 1 + 16);
    get_data(server, 1 + 16);

    guess_correct = true;

    /* Find the matching string in each list, and map it to its
     * kexinit_algorithm structure. */
    for (i = 0; i < NKEXLIST; i++) {
        ptrlen clist, slist, cword, sword, found;
        bool cfirst, sfirst;
        int j;

        clists[i] = get_string(client);
        slists[i] = get_string(server);
        if (get_err(client) || get_err(server)) {
            /* Report a better error than the spurious "Couldn't
             * agree" that we'd generate if we pressed on regardless
             * and treated the empty get_string() result as genuine */
            ssh_proto_error(ssh, "KEXINIT packet was incomplete");
            return false;
        }

        for (cfirst = true, clist = clists[i];
             get_commasep_word(&clist, &cword); cfirst = false)
            for (sfirst = true, slist = slists[i];
                 get_commasep_word(&slist, &sword); sfirst = false)
                if (ptrlen_eq_ptrlen(cword, sword)) {
                    found = cword;
                    goto found_match;
                }

        /* No matching string found in the two lists. Delay reporting
         * a fatal error until below, because sometimes it turns out
         * not to be fatal. */
        selected[i] = NULL;

        /*
         * However, even if a failure to agree on any algorithm at all
         * is not completely fatal (e.g. because it's the MAC
         * negotiation for a cipher that comes with a built-in MAC),
         * it still invalidates the guessed key exchange packet. (RFC
         * 4253 section 7, not contradicted by OpenSSH's
         * PROTOCOL.chacha20poly1305 or as far as I can see by their
         * code.)
         */
        guess_correct = false;

        continue;

      found_match:

        selected[i] = NULL;
        for (j = 0; j < MAXKEXLIST; j++) {
            if (ptrlen_eq_string(found, kexlists[i][j].name)) {
                selected[i] = &kexlists[i][j];
                break;
            }
        }
        assert(selected[i]); /* kexlists[] must cover one of the inputs */

        /*
         * If the kex or host key algorithm is not the first one in
         * both sides' lists, that means the guessed key exchange
         * packet (if any) is officially wrong.
         */
        if ((i == KEXLIST_KEX || i == KEXLIST_HOSTKEY) && !(cfirst || sfirst))
            guess_correct = false;
    }

    /*
     * Skip language strings in both KEXINITs, and read the flags
     * saying whether a guessed KEX packet follows.
     */
    get_string(client);
    get_string(client);
    get_string(server);
    get_string(server);
    if (ignore_guess_cs_packet)
        *ignore_guess_cs_packet = get_bool(client) && !guess_correct;
    if (ignore_guess_sc_packet)
        *ignore_guess_sc_packet = get_bool(server) && !guess_correct;

    /*
     * Now transcribe the selected algorithm set into the output data.
     */
    for (i = 0; i < NKEXLIST; i++) {
        const struct kexinit_algorithm *alg;

        /*
         * If we've already selected a cipher which requires a
         * particular MAC, then just select that. This is the case in
         * which it's not a fatal error if the actual MAC string lists
         * didn't include any matching error.
         */
        if (i == KEXLIST_CSMAC && cs->cipher &&
            cs->cipher->required_mac) {
            cs->mac = cs->cipher->required_mac;
            cs->etm_mode = !!(cs->mac->etm_name);
            continue;
        }
        if (i == KEXLIST_SCMAC && sc->cipher &&
            sc->cipher->required_mac) {
            sc->mac = sc->cipher->required_mac;
            sc->etm_mode = !!(sc->mac->etm_name);
            continue;
        }

        alg = selected[i];
        if (!alg) {
            /*
             * Otherwise, any match failure _is_ a fatal error.
             */
            ssh_sw_abort(ssh, "Couldn't agree a %s (available: %.*s)",
                         kexlist_descr[i], PTRLEN_PRINTF(slists[i]));
            return false;
        }

        switch (i) {
          case KEXLIST_KEX:
            *kex_alg = alg->u.kex.kex;
            *warn_kex = alg->u.kex.warn;
            break;

          case KEXLIST_HOSTKEY:
            /*
             * Ignore an unexpected/inappropriate offer of "null",
             * we offer "null" when we're willing to use GSS KEX,
             * but it is only acceptable when GSSKEX is actually
             * selected.
             */
            if (alg->u.hk.hostkey == NULL &&
                (*kex_alg)->main_type != KEXTYPE_GSS)
                continue;

            *hostkey_alg = alg->u.hk.hostkey;
            *warn_hk = alg->u.hk.warn;
            break;

          case KEXLIST_CSCIPHER:
            cs->cipher = alg->u.cipher.cipher;
            *warn_cscipher = alg->u.cipher.warn;
            break;

          case KEXLIST_SCCIPHER:
            sc->cipher = alg->u.cipher.cipher;
            *warn_sccipher = alg->u.cipher.warn;
            break;

          case KEXLIST_CSMAC:
            cs->mac = alg->u.mac.mac;
            cs->etm_mode = alg->u.mac.etm;
            break;

          case KEXLIST_SCMAC:
            sc->mac = alg->u.mac.mac;
            sc->etm_mode = alg->u.mac.etm;
            break;

          case KEXLIST_CSCOMP:
            cs->comp = alg->u.comp.comp;
            cs->comp_delayed = alg->u.comp.delayed;
            break;

          case KEXLIST_SCCOMP:
            sc->comp = alg->u.comp.comp;
            sc->comp_delayed = alg->u.comp.delayed;
            break;

          default:
            unreachable("Bad list index in scan_kexinits");
        }
    }

    if (server_hostkeys) {
        /*
         * Finally, make an auxiliary pass over the server's host key
         * list to find all the host key algorithms offered by the
         * server which we know about at all, whether we selected each
         * one or not. We return these as a list of indices into the
         * constant ssh2_hostkey_algs[] array.
         */
        *n_server_hostkeys = 0;

        ptrlen list = slists[KEXLIST_HOSTKEY];
        for (ptrlen word; get_commasep_word(&list, &word) ;) {
            for (i = 0; i < lenof(ssh2_hostkey_algs); i++)
                if (ptrlen_eq_string(word, ssh2_hostkey_algs[i].alg->ssh_id)) {
                    server_hostkeys[(*n_server_hostkeys)++] = i;
                    break;
                }
        }
    }

    return true;
}

void ssh2transport_finalise_exhash(struct ssh2_transport_state *s)
{
    put_mp_ssh2(s->exhash, s->K);
    assert(ssh_hash_alg(s->exhash)->hlen <= sizeof(s->exchange_hash));
    ssh_hash_final(s->exhash, s->exchange_hash);
    s->exhash = NULL;

#if 0
    debug("Exchange hash is:\n");
    dmemdump(s->exchange_hash, s->kex_alg->hash->hlen);
#endif
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
    if (ssh2_transport_filter_queue(s))
        return;   /* we've been freed */

    crBegin(s->crState);

    s->in.cipher = s->out.cipher = NULL;
    s->in.mac = s->out.mac = NULL;
    s->in.comp = s->out.comp = NULL;

    s->got_session_id = false;
    s->need_gss_transient_hostkey = false;
    s->warned_about_no_gss_transient_hostkey = false;

  begin_key_exchange:

#ifndef NO_GSSAPI
    if (s->need_gss_transient_hostkey) {
        /*
         * This flag indicates a special case in which we must not do
         * GSS key exchange even if we could. (See comments below,
         * where the flag was set on the previous key exchange.)
         */
        s->can_gssapi_keyex = false;
    } else if (conf_get_bool(s->conf, CONF_try_gssapi_kex)) {
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
            ssh2_transport_gss_update(s, true);

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
            s->can_gssapi_keyex = false;
        s->gss_delegate = conf_get_bool(s->conf, CONF_gssapifwd);
    } else {
        s->can_gssapi_keyex = false;
    }
#endif

    s->ppl.bpp->pls->kctx = SSH2_PKTCTX_NOKEX;

    /*
     * Construct our KEXINIT packet, in a strbuf so we can refer to it
     * later.
     */
    s->client_kexinit->len = 0;
    put_byte(s->outgoing_kexinit, SSH2_MSG_KEXINIT);
    random_read(strbuf_append(s->outgoing_kexinit, 16), 16);
    ssh2_write_kexinit_lists(
        BinarySink_UPCAST(s->outgoing_kexinit), s->kexlists,
        s->conf, s->ppl.remote_bugs,
        s->savedhost, s->savedport, s->hostkey_alg, s->thc,
        s->hostkeys, s->nhostkeys,
        !s->got_session_id, s->can_gssapi_keyex,
        s->gss_kex_used && !s->need_gss_transient_hostkey);
    /* First KEX packet does _not_ follow, because we're not that brave. */
    put_bool(s->outgoing_kexinit, false);
    put_uint32(s->outgoing_kexinit, 0);             /* reserved */

    /*
     * Send our KEXINIT.
     */
    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_KEXINIT);
    put_data(pktout, s->outgoing_kexinit->u + 1,
             s->outgoing_kexinit->len - 1); /* omit initial packet type byte */
    pq_push(s->ppl.out_pq, pktout);

    /*
     * Flag that KEX is in progress.
     */
    s->kex_in_progress = true;

    /*
     * Wait for the other side's KEXINIT, and save it.
     */
    crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
    if (pktin->type != SSH2_MSG_KEXINIT) {
        ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                        "expecting KEXINIT, type %d (%s)", pktin->type,
                        ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                      s->ppl.bpp->pls->actx, pktin->type));
        return;
    }
    s->incoming_kexinit->len = 0;
    put_byte(s->incoming_kexinit, SSH2_MSG_KEXINIT);
    put_data(s->incoming_kexinit, get_ptr(pktin), get_avail(pktin));

    /*
     * Work through the two KEXINIT packets in parallel to find the
     * selected algorithm identifiers.
     */
    {
        int nhk, hks[MAXKEXLIST], i, j;

        if (!ssh2_scan_kexinits(
                ptrlen_from_strbuf(s->client_kexinit),
                ptrlen_from_strbuf(s->server_kexinit),
                s->kexlists, &s->kex_alg, &s->hostkey_alg, &s->out, &s->in,
                &s->warn_kex, &s->warn_hk, &s->warn_cscipher,
                &s->warn_sccipher, s->ppl.ssh, NULL, &s->ignorepkt, &nhk, hks))
            return; /* false means a fatal error function was called */

        /*
         * In addition to deciding which host key we're actually going
         * to use, we should make a list of the host keys offered by
         * the server which we _don't_ have cached. These will be
         * offered as cross-certification options by ssh_get_specials.
         *
         * We also count the key we're currently using for KEX as one
         * we've already got, because by the time this menu becomes
         * visible, it will be.
         */
        s->n_uncert_hostkeys = 0;

        for (i = 0; i < nhk; i++) {
            j = hks[i];
            if (ssh2_hostkey_algs[j].alg != s->hostkey_alg &&
                !have_ssh_host_key(s->savedhost, s->savedport,
                                   ssh2_hostkey_algs[j].alg->cache_id)) {
                s->uncert_hostkeys[s->n_uncert_hostkeys++] = j;
            }
        }
    }

    if (s->warn_kex) {
        s->dlgret = ssh2_transport_confirm_weak_crypto_primitive(
            s, "key-exchange algorithm", s->kex_alg->name, s->kex_alg);
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
         * that's all we have *cached*, list the host keys we
         * could usefully cross-certify. Otherwise, use the same
         * standard wording as any other weak crypto primitive.
         */
        betteralgs = NULL;
        for (j = 0; j < s->n_uncert_hostkeys; j++) {
            const struct ssh_signkey_with_user_pref_id *hktype =
                &ssh2_hostkey_algs[s->uncert_hostkeys[j]];
            bool better = false;
            for (k = 0; k < HK_MAX; k++) {
                int id = conf_get_int_int(s->conf, CONF_ssh_hklist, k);
                if (id == HK_WARN) {
                    break;
                } else if (id == hktype->id) {
                    better = true;
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
                /* Use the special warning prompt that lets us provide
                 * a list of better algorithms */
                s->dlgret = seat_confirm_weak_cached_hostkey(
                    s->ppl.seat, s->hostkey_alg->ssh_id, betteralgs,
                    ssh2_transport_dialog_callback, s);
            sfree(betteralgs);
        } else {
                /* If none exist, use the more general 'weak crypto'
                 * warning prompt */
                s->dlgret = ssh2_transport_confirm_weak_crypto_primitive(
                    s, "host key type", s->hostkey_alg->ssh_id,
                    s->hostkey_alg);
        }
        crMaybeWaitUntilV(s->dlgret >= 0);
        if (s->dlgret == 0) {
            ssh_user_close(s->ppl.ssh, "User aborted at host key warning");
            return;
        }
    }

    if (s->warn_cscipher) {
        s->dlgret = ssh2_transport_confirm_weak_crypto_primitive(
            s, "client-to-server cipher", s->out.cipher->ssh2_id,
            s->out.cipher);
        crMaybeWaitUntilV(s->dlgret >= 0);
        if (s->dlgret == 0) {
            ssh_user_close(s->ppl.ssh, "User aborted at cipher warning");
            return;
        }
    }

    if (s->warn_sccipher) {
        s->dlgret = ssh2_transport_confirm_weak_crypto_primitive(
            s, "server-to-client cipher", s->in.cipher->ssh2_id,
            s->in.cipher);
        crMaybeWaitUntilV(s->dlgret >= 0);
        if (s->dlgret == 0) {
            ssh_user_close(s->ppl.ssh, "User aborted at cipher warning");
            return;
        }
    }

    /*
     * If the other side has sent an initial key exchange packet that
     * we must treat as a wrong guess, wait for it, and discard it.
     */
    if (s->ignorepkt)
        crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);

    /*
     * Actually perform the key exchange.
     */
    s->exhash = ssh_hash_new(s->kex_alg->hash);
    put_stringz(s->exhash, s->client_greeting);
    put_stringz(s->exhash, s->server_greeting);
    put_string(s->exhash, s->client_kexinit->u, s->client_kexinit->len);
    put_string(s->exhash, s->server_kexinit->u, s->server_kexinit->len);
    s->crStateKex = 0;
    while (1) {
        bool aborted = false;
        ssh2kex_coroutine(s, &aborted);
        if (aborted)
            return;    /* disaster: our entire state has been freed */
        if (!s->crStateKex)
            break;     /* kex phase has terminated normally */
        crReturnV;
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
        s->got_session_id = true;
    }

    /*
     * Send SSH2_MSG_NEWKEYS.
     */
    pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_NEWKEYS);
    pq_push(s->ppl.out_pq, pktout);
    /* Start counting down the outgoing-data limit for these cipher keys. */
    dts_reset(&s->stats->out, s->max_data_size);

    /*
     * Force the BPP to synchronously marshal all packets up to and
     * including that NEWKEYS into wire format, before we switch over
     * to new crypto.
     */
    ssh_bpp_handle_output(s->ppl.bpp);

    /*
     * We've sent outgoing NEWKEYS, so create and initialise outgoing
     * session keys.
     */
    {
        strbuf *cipher_key = strbuf_new_nm();
        strbuf *cipher_iv = strbuf_new_nm();
        strbuf *mac_key = strbuf_new_nm();

        if (s->out.cipher) {
            ssh2_mkkey(s, cipher_iv, s->K, s->exchange_hash,
                       'A' + s->out.mkkey_adjust, s->out.cipher->blksize);
            ssh2_mkkey(s, cipher_key, s->K, s->exchange_hash,
                       'C' + s->out.mkkey_adjust,
                       s->out.cipher->padded_keybytes);
        }
        if (s->out.mac) {
            ssh2_mkkey(s, mac_key, s->K, s->exchange_hash,
                       'E' + s->out.mkkey_adjust, s->out.mac->keylen);
        }

        ssh2_bpp_new_outgoing_crypto(
            s->ppl.bpp,
            s->out.cipher, cipher_key->u, cipher_iv->u,
            s->out.mac, s->out.etm_mode, mac_key->u,
            s->out.comp, s->out.comp_delayed);

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
    dts_reset(&s->stats->in, s->max_data_size);

    /*
     * We've seen incoming NEWKEYS, so create and initialise
     * incoming session keys.
     */
    {
        strbuf *cipher_key = strbuf_new_nm();
        strbuf *cipher_iv = strbuf_new_nm();
        strbuf *mac_key = strbuf_new_nm();

        if (s->in.cipher) {
            ssh2_mkkey(s, cipher_iv, s->K, s->exchange_hash,
                       'A' + s->in.mkkey_adjust, s->in.cipher->blksize);
            ssh2_mkkey(s, cipher_key, s->K, s->exchange_hash,
                       'C' + s->in.mkkey_adjust,
                       s->in.cipher->padded_keybytes);
        }
        if (s->in.mac) {
            ssh2_mkkey(s, mac_key, s->K, s->exchange_hash,
                       'E' + s->in.mkkey_adjust, s->in.mac->keylen);
        }

        ssh2_bpp_new_incoming_crypto(
            s->ppl.bpp,
            s->in.cipher, cipher_key->u, cipher_iv->u,
            s->in.mac, s->in.etm_mode, mac_key->u,
            s->in.comp, s->in.comp_delayed);

        strbuf_free(cipher_key);
        strbuf_free(cipher_iv);
        strbuf_free(mac_key);
    }

    /*
     * Free shared secret.
     */
    mp_free(s->K); s->K = NULL;

    /*
     * Update the specials menu to list the remaining uncertified host
     * keys.
     */
    seat_update_specials_menu(s->ppl.seat);

    /*
     * Key exchange is over. Loop straight back round if we have a
     * deferred rekey reason.
     */
    if (s->deferred_rekey_reason) {
        ppl_logevent("%s", s->deferred_rekey_reason);
        pktin = NULL;
        s->deferred_rekey_reason = NULL;
        goto begin_key_exchange;
    }

    /*
     * Otherwise, schedule a timer for our next rekey.
     */
    s->kex_in_progress = false;
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
        if (!s->hostkeys) {
            /* We're the client, so send SERVICE_REQUEST. */
            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_SERVICE_REQUEST);
            put_stringz(pktout, s->higher_layer->vt->name);
            pq_push(s->ppl.out_pq, pktout);
            crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_SERVICE_ACCEPT) {
                ssh_sw_abort(s->ppl.ssh, "Server refused request to start "
                             "'%s' protocol", s->higher_layer->vt->name);
                return;
            }
        } else {
            ptrlen service_name;

            /* We're the server, so expect SERVICE_REQUEST. */
            crMaybeWaitUntilV((pktin = ssh2_transport_pop(s)) != NULL);
            if (pktin->type != SSH2_MSG_SERVICE_REQUEST) {
                ssh_proto_error(s->ppl.ssh, "Received unexpected packet when "
                                "expecting SERVICE_REQUEST, type %d (%s)",
                                pktin->type,
                                ssh2_pkt_type(s->ppl.bpp->pls->kctx,
                                              s->ppl.bpp->pls->actx,
                                              pktin->type));
                return;
            }
            service_name = get_string(pktin);
            if (!ptrlen_eq_string(service_name, s->higher_layer->vt->name)) {
                ssh_proto_error(s->ppl.ssh, "Client requested service "
                                "'%.*s' when we only support '%s'",
                                PTRLEN_PRINTF(service_name),
                                s->higher_layer->vt->name);
                return;
            }

            pktout = ssh_bpp_new_pktout(s->ppl.bpp, SSH2_MSG_SERVICE_ACCEPT);
            put_stringz(pktout, s->higher_layer->vt->name);
            pq_push(s->ppl.out_pq, pktout);
        }

        s->higher_layer_ok = true;
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
            ppl_logevent("Remote side initiated key re-exchange");
            s->rekey_class = RK_SERVER;
        }

        if (s->rekey_class == RK_POST_USERAUTH) {
            /*
             * userauth has seen a USERAUTH_SUCCESS. This may be the
             * moment to do an immediate rekey with different
             * parameters. But it may not; so here we turn that rekey
             * class into either RK_NONE or RK_NORMAL.
             *
             * Currently the only reason for this is if we've done a
             * GSS key exchange and don't have anything in our
             * transient hostkey cache, in which case we should make
             * an attempt to populate the cache now.
             */
            if (s->need_gss_transient_hostkey) {
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
            if (s->stats->in.expired) {
                s->rekey_reason = "too much data received";
                s->rekey_class = RK_NORMAL;
            } else if (s->stats->out.expired) {
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
                ppl_logevent("Remote bug prevents key re-exchange (%s)",
                             s->rekey_reason);
                /* Reset the counters, so that at least this message doesn't
                 * hit the event log _too_ often. */
                dts_reset(&s->stats->in, s->max_data_size);
                dts_reset(&s->stats->out, s->max_data_size);
                (void) ssh2_transport_timer_update(s, 0);
                s->rekey_class = RK_NONE;
            } else {
                ppl_logevent("Initiating key re-exchange (%s)",
                             s->rekey_reason);
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
        ssh2_transport_gss_update(s, false);
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
 * We either schedule the next timer and return false, or return true
 * to run the callback now, which will call us again to re-schedule on
 * completion.
 */
static bool ssh2_transport_timer_update(struct ssh2_transport_state *s,
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
            return true;
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
    return false;
}

void ssh2_transport_dialog_callback(void *loginv, int ret)
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
                                      bool definitely_rekeying)
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
    if (!conf_get_bool(s->conf, CONF_try_gssapi_auth) &&
        !conf_get_bool(s->conf, CONF_try_gssapi_kex))
        return;

    /* Import server name and cache it */
    if (s->shgss->srv_name == GSS_C_NO_NAME) {
        gss_stat = s->shgss->lib->import_name(
            s->shgss->lib, s->fullhostname, &s->shgss->srv_name);
        if (gss_stat != SSH_GSS_OK) {
            if (gss_stat == SSH_GSS_BAD_HOST_NAME)
                ppl_logevent("GSSAPI import name failed - Bad service name;"
                             " won't use GSS key exchange");
            else
                ppl_logevent("GSSAPI import name failed;"
                             " won't use GSS key exchange");
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
            ppl_logevent("No GSSAPI security context available");

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
    if (!conf_get_bool(s->conf, CONF_gssapifwd))
        return;

    if (s->gss_cred_expiry != GSS_NO_EXPIRATION &&
        difftime(gss_cred_expiry, s->gss_cred_expiry) > 0)
        s->gss_status |= GSS_CRED_UPDATED;

    mins = sanitise_rekey_time(
        conf_get_int(s->conf, CONF_gssapirekey), GSS_DEF_REKEY_MINS);
    if (mins > 0 && s->gss_ctxt_lifetime <= mins * 60)
        s->gss_status |= GSS_CTXT_EXPIRES;
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

static bool ssh2_transport_get_specials(
    PacketProtocolLayer *ppl, add_special_fn_t add_special, void *ctx)
{
    struct ssh2_transport_state *s =
        container_of(ppl, struct ssh2_transport_state, ppl);
    bool need_separator = false;
    bool toret = false;

    if (ssh_ppl_get_specials(s->higher_layer, add_special, ctx)) {
        need_separator = true;
        toret = true;
    }

    /*
     * Don't bother offering rekey-based specials if we've decided the
     * remote won't cope with it, since we wouldn't bother sending it
     * if asked anyway.
     */
    if (!(s->ppl.remote_bugs & BUG_SSH2_REKEY)) {
        if (need_separator) {
            add_special(ctx, NULL, SS_SEP, 0);
            need_separator = false;
        }

        add_special(ctx, "Repeat key exchange", SS_REKEY, 0);
        toret = true;

        if (s->n_uncert_hostkeys) {
            int i;

            add_special(ctx, NULL, SS_SEP, 0);
            add_special(ctx, "Cache new host key type", SS_SUBMENU, 0);
            for (i = 0; i < s->n_uncert_hostkeys; i++) {
                const ssh_keyalg *alg =
                    ssh2_hostkey_algs[s->uncert_hostkeys[i]].alg;

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
            s->cross_certifying = s->hostkey_alg = ssh2_hostkey_algs[arg].alg;
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
    bool rekey_mandatory = false;
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

            dts_consume(&s->stats->out, diff);
            dts_consume(&s->stats->in, diff);
            if (s->stats->out.expired || s->stats->in.expired)
                rekey_reason = "data limit lowered";
        } else {
            unsigned long diff = s->max_data_size - old_max_data_size;
            if (s->stats->out.running)
                s->stats->out.remaining += diff;
            if (s->stats->in.running)
                s->stats->in.remaining += diff;
        }
    }

    if (conf_get_bool(s->conf, CONF_compression) !=
	conf_get_bool(conf, CONF_compression)) {
        rekey_reason = "compression setting changed";
        rekey_mandatory = true;
    }

    for (i = 0; i < CIPHER_MAX; i++)
	if (conf_get_int_int(s->conf, CONF_ssh_cipherlist, i) !=
	    conf_get_int_int(conf, CONF_ssh_cipherlist, i)) {
        rekey_reason = "cipher settings changed";
        rekey_mandatory = true;
    }
    if (conf_get_bool(s->conf, CONF_ssh2_des_cbc) !=
	conf_get_bool(conf, CONF_ssh2_des_cbc)) {
        rekey_reason = "cipher settings changed";
        rekey_mandatory = true;
    }

    conf_free(s->conf);
    s->conf = conf_copy(conf);

    if (rekey_reason) {
        if (!s->kex_in_progress && !ssh2_bpp_rekey_inadvisable(s->ppl.bpp)) {
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

static bool ssh2_transport_want_user_input(PacketProtocolLayer *ppl)
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

static int weak_algorithm_compare(void *av, void *bv)
{
    uintptr_t a = (uintptr_t)av, b = (uintptr_t)bv;
    return a < b ? -1 : a > b ? +1 : 0;
}

/*
 * Wrapper on seat_confirm_weak_crypto_primitive(), which uses the
 * tree234 s->weak_algorithms_consented_to to ensure we ask at most
 * once about any given crypto primitive.
 */
static int ssh2_transport_confirm_weak_crypto_primitive(
    struct ssh2_transport_state *s, const char *type, const char *name,
    const void *alg)
{
    if (find234(s->weak_algorithms_consented_to, (void *)alg, NULL))
        return 1;
    add234(s->weak_algorithms_consented_to, (void *)alg);

    return seat_confirm_weak_crypto_primitive(
        s->ppl.seat, type, name, ssh2_transport_dialog_callback, s);
}
