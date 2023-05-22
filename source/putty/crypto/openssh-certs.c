/*
 * Public key type for OpenSSH certificates.
 */

#include "ssh.h"
#include "putty.h"

enum {
    SSH_CERT_TYPE_USER = 1,
    SSH_CERT_TYPE_HOST = 2,
};

typedef struct opensshcert_key {
    strbuf *nonce;
    uint64_t serial;
    uint32_t type;
    strbuf *key_id;
    strbuf *valid_principals;
    uint64_t valid_after, valid_before;
    strbuf *critical_options;
    strbuf *extensions;
    strbuf *reserved;
    strbuf *signature_key;
    strbuf *signature;

    ssh_key *basekey;

    ssh_key sshk;
} opensshcert_key;

typedef struct blob_fmt {
    const unsigned *fmt;
    size_t len;
} blob_fmt;

typedef struct opensshcert_extra {
    /*
     * OpenSSH certificate formats aren't completely consistent about
     * the relationship between the public+private blob uploaded to
     * the agent for the certified key type, and the one for the base
     * key type. Here we specify the mapping.
     *
     * Each of these foo_fmt strings indicates the layout of a
     * particular version of the key, in the form of an array of
     * integers together with a length, with each integer describing
     * one of the components of the key. The integers are defined by
     * enums, so that they're tightly packed; the general idea is that
     * if you're converting from one form to another, then you use the
     * format list for the source format to read out a succession of
     * SSH strings from the source data and put them in an array
     * indexed by the integer ids, and then use the list for the
     * destination format to write the strings out to the destination
     * in the right (maybe different) order.
     *
     * pub_fmt describes the format of the public-key blob for the
     * base key type, not counting the initial string giving the key
     * type identifier itself. As far as I know, this always matches
     * the format of the public-key data appearing in the middle of
     * the certificate.
     *
     * base_ossh_fmt describes the format of the full OpenSSH blob
     * appearing in the ssh-agent protocol for the base key,
     * containing the public and private key data.
     *
     * cert_ossh_fmt describes the format of the OpenSSH blob for the
     * certificate key format, beginning just after the certificate
     * string itself.
     */
    blob_fmt pub_fmt, base_ossh_fmt, cert_ossh_fmt;

    /*
     * The RSA-SHA2 algorithm names have their SSH id set to names
     * like "rsa-sha2-512-cert-...", which is what will be received in
     * the KEXINIT algorithm list if a host key in one of those
     * algorithms is presented. But the _key_ type id that will appear
     * in the public key blob is "ssh-rsa-cert-...". So we need a
     * separate field to indicate the key type id we expect to see in
     * certified public keys, and also the one we want to put back
     * into the artificial public blob we make to pass to the
     * constructor for the underlying key.
     *
     * (In rsa.c this is managed much more simply, because everything
     * sharing the same vtable wants the same key type id.)
     */
    const char *cert_key_ssh_id, *base_key_ssh_id;
} opensshcert_extra;

/*
 * The actual integer arrays defining the per-key blob formats.
 */

/* DSA is the most orthodox: only the obviously necessary public key
 * info appears at all, it's in the same order everywhere, and none of
 * it is repeated unnecessarily */
enum { DSA_p, DSA_q, DSA_g, DSA_y, DSA_x };
static const unsigned dsa_pub_fmt[] = { DSA_p, DSA_q, DSA_g, DSA_y };
static const unsigned dsa_base_ossh_fmt[] = {
    DSA_p, DSA_q, DSA_g, DSA_y, DSA_x };
static const unsigned dsa_cert_ossh_fmt[] = { DSA_x };

/* ECDSA is almost as nice, except that it pointlessly mentions the
 * curve name in the public data, which shouldn't be necessary given
 * that the SSH key id has already implied it. But at least that's
 * consistent everywhere. */
enum { ECDSA_curve, ECDSA_point, ECDSA_exp };
static const unsigned ecdsa_pub_fmt[] = { ECDSA_curve, ECDSA_point };
static const unsigned ecdsa_base_ossh_fmt[] = {
    ECDSA_curve, ECDSA_point, ECDSA_exp };
static const unsigned ecdsa_cert_ossh_fmt[] = { ECDSA_exp };

/* Ed25519 has the oddity that the private data following the
 * certificate in the OpenSSH blob is preceded by an extra copy of the
 * public data, for no obviously necessary reason since that doesn't
 * happen in any of the rest of these formats */
enum { EDDSA_point, EDDSA_exp };
static const unsigned eddsa_pub_fmt[] = { EDDSA_point };
static const unsigned eddsa_base_ossh_fmt[] = { EDDSA_point, EDDSA_exp };
static const unsigned eddsa_cert_ossh_fmt[] = { EDDSA_point, EDDSA_exp };

/* And RSA has the quirk that the modulus and exponent are reversed in
 * the base key type's OpenSSH blob! */
enum { RSA_e, RSA_n, RSA_d, RSA_p, RSA_q, RSA_iqmp };
static const unsigned rsa_pub_fmt[] = { RSA_e, RSA_n };
static const unsigned rsa_base_ossh_fmt[] = {
    RSA_n, RSA_e, RSA_d, RSA_p, RSA_q, RSA_iqmp };
static const unsigned rsa_cert_ossh_fmt[] = { RSA_d, RSA_p, RSA_q, RSA_iqmp };

/*
 * Routines to transform one kind of blob into another based on those
 * foo_fmt integer arrays.
 */
typedef struct BlobTransformer {
    ptrlen *parts;
    size_t nparts;
} BlobTransformer;

#define BLOBTRANS_DECLARE(bt) BlobTransformer bt[1] = { { NULL, 0 } }

static inline void blobtrans_clear(BlobTransformer *bt)
{
    sfree(bt->parts);
    bt->parts = NULL;
    bt->nparts = 0;
}

static inline bool blobtrans_read(BlobTransformer *bt, BinarySource *src,
                                  blob_fmt blob)
{
    size_t nparts = bt->nparts;
    { // WINSCP
    size_t i;
    for (i = 0; i < blob.len; i++)
        if (nparts < blob.fmt[i]+1)
            nparts = blob.fmt[i]+1;

    if (nparts > bt->nparts) {
        bt->parts = sresize(bt->parts, nparts, ptrlen);
        while (bt->nparts < nparts)
            bt->parts[bt->nparts++] = make_ptrlen(NULL, 0);
    }

    for (i = 0; i < blob.len; i++) {
        size_t j = blob.fmt[i];
        ptrlen part = get_string(src);
        if (bt->parts[j].ptr) {
            /*
             * If the same string appears in both the public blob and
             * the private data, check they match. (This happens in
             * Ed25519: an extra copy of the public point string
             * appears in the certified OpenSSH data after the
             * certificate and before the private key.)
             */
            if (!ptrlen_eq_ptrlen(bt->parts[j], part))
                return false;
        }
        bt->parts[j] = part;
    }

    return true;
    } // WINSCP
}

static inline void blobtrans_write(BlobTransformer *bt, BinarySink *bs,
                                   blob_fmt blob)
{
    size_t i; // WINSCP
    for (i = 0; i < blob.len; i++) {
        pinitassert(i < bt->nparts);
        ptrlen part = bt->parts[blob.fmt[i]];
        assert(part.ptr);
        put_stringpl(bs, part);
    }
}

/*
 * Forward declarations.
 */
static ssh_key *opensshcert_new_pub(const ssh_keyalg *self, ptrlen pub);
static ssh_key *opensshcert_new_priv(
    const ssh_keyalg *self, ptrlen pub, ptrlen priv);
static ssh_key *opensshcert_new_priv_openssh(
    const ssh_keyalg *self, BinarySource *src);
static void opensshcert_freekey(ssh_key *key);
static char *opensshcert_invalid(ssh_key *key, unsigned flags);
static void opensshcert_sign(ssh_key *key, ptrlen data, unsigned flags,
                             BinarySink *bs);
static bool opensshcert_verify(ssh_key *key, ptrlen sig, ptrlen data);
static void opensshcert_public_blob(ssh_key *key, BinarySink *bs);
static void opensshcert_private_blob(ssh_key *key, BinarySink *bs);
static void opensshcert_openssh_blob(ssh_key *key, BinarySink *bs);
static void opensshcert_ca_public_blob(ssh_key *key, BinarySink *bs);
static void opensshcert_cert_id_string(ssh_key *key, BinarySink *bs);
static SeatDialogText *opensshcert_cert_info(ssh_key *key);
static bool opensshcert_has_private(ssh_key *key);
static char *opensshcert_cache_str(ssh_key *key);
static key_components *opensshcert_components(ssh_key *key);
static ssh_key *opensshcert_base_key(ssh_key *key);
static bool opensshcert_check_cert(
    ssh_key *key, bool host, ptrlen principal, uint64_t time,
    const ca_options *opts, BinarySink *error);
static int opensshcert_pubkey_bits(const ssh_keyalg *self, ptrlen blob);
static unsigned opensshcert_supported_flags(const ssh_keyalg *self);
static const char *opensshcert_alternate_ssh_id(const ssh_keyalg *self,
                                                unsigned flags);
static char *opensshcert_alg_desc(const ssh_keyalg *self);
static bool opensshcert_variable_size(const ssh_keyalg *self);
static const ssh_keyalg *opensshcert_related_alg(const ssh_keyalg *self,
                                                 const ssh_keyalg *base);

/*
 * Top-level vtables for the certified key formats, defined via a list
 * macro so I can also make an array of them all.
 */

#define KEYALG_LIST(X)                                                  \
    X(ssh_dsa, "ssh-dss", "ssh-dss", dsa)                               \
    X(ssh_rsa, "ssh-rsa", "ssh-rsa", rsa)                               \
    X(ssh_rsa_sha256, "rsa-sha2-256", "ssh-rsa", rsa)                   \
    X(ssh_rsa_sha512, "rsa-sha2-512", "ssh-rsa", rsa)                   \
    X(ssh_ecdsa_ed25519, "ssh-ed25519", "ssh-ed25519", eddsa)           \
    X(ssh_ecdsa_nistp256, "ecdsa-sha2-nistp256","ecdsa-sha2-nistp256", ecdsa) \
    X(ssh_ecdsa_nistp384, "ecdsa-sha2-nistp384","ecdsa-sha2-nistp384", ecdsa) \
    X(ssh_ecdsa_nistp521, "ecdsa-sha2-nistp521","ecdsa-sha2-nistp521", ecdsa) \
    /* end of list */

#define KEYALG_DEF(name, ssh_alg_id_prefix, ssh_key_id_prefix, fmt_prefix) \
    static const struct opensshcert_extra opensshcert_##name##_extra = { \
        /*.pub_fmt =*/ { /*.fmt =*/ fmt_prefix ## _pub_fmt,                     \
                     /*.len =*/ lenof(fmt_prefix ## _pub_fmt) },            \
        /*.base_ossh_fmt =*/ { /*.fmt =*/ fmt_prefix ## _base_ossh_fmt,         \
                           /*.len =*/ lenof(fmt_prefix ## _base_ossh_fmt) }, \
        /*.cert_ossh_fmt =*/ { /*.fmt =*/ fmt_prefix ## _cert_ossh_fmt,         \
                           /*.len =*/ lenof(fmt_prefix ## _cert_ossh_fmt) }, \
        /*.cert_key_ssh_id =*/ ssh_key_id_prefix "-cert-v01@openssh.com",   \
        /*.base_key_ssh_id =*/ ssh_key_id_prefix,                           \
    };                                                                  \
                                                                        \
    const ssh_keyalg opensshcert_##name = {                             \
        /*.new_pub =*/ opensshcert_new_pub,                                 \
        /*.new_priv =*/ opensshcert_new_priv,                               \
        /*.new_priv_openssh =*/ opensshcert_new_priv_openssh,               \
        /*.freekey =*/ opensshcert_freekey,                                 \
        /*.invalid =*/ opensshcert_invalid,                                 \
        /*.sign =*/ opensshcert_sign,                                       \
        /*.verify =*/ opensshcert_verify,                                   \
        /*.public_blob =*/ opensshcert_public_blob,                         \
        /*.private_blob =*/ opensshcert_private_blob,                       \
        /*.openssh_blob =*/ opensshcert_openssh_blob,                       \
        /*.has_private =*/ opensshcert_has_private,                         \
        /*.cache_str =*/ opensshcert_cache_str,                             \
        /*.components =*/ opensshcert_components,                           \
        /*.base_key =*/ opensshcert_base_key,                               \
        /*.ca_public_blob =*/ opensshcert_ca_public_blob,                   \
        /*.check_cert =*/ opensshcert_check_cert,                           \
        /*.cert_id_string =*/ opensshcert_cert_id_string,                   \
        /*.cert_info =*/ opensshcert_cert_info,                             \
        /*.pubkey_bits =*/ opensshcert_pubkey_bits,                         \
        /*.supported_flags =*/ opensshcert_supported_flags,                 \
        /*.alternate_ssh_id =*/ opensshcert_alternate_ssh_id,               \
        /*.alg_desc =*/ opensshcert_alg_desc,                               \
        /*.variable_size =*/ opensshcert_variable_size,                     \
        /*.related_alg =*/ opensshcert_related_alg,                         \
        /*.ssh_id =*/ ssh_alg_id_prefix "-cert-v01@openssh.com",            \
        /*.cache_id =*/ "opensshcert-" ssh_key_id_prefix,                   \
        /*.extra =*/ &opensshcert_##name##_extra,                           \
        /*.is_certificate =*/ true,                                         \
        /*.base_alg =*/ &name,                                              \
    };
KEYALG_LIST(KEYALG_DEF)
#undef KEYALG_DEF

#define KEYALG_LIST_ENTRY(name, algid, keyid, fmt) &opensshcert_##name,
static const ssh_keyalg *const opensshcert_all_keyalgs[] = {
    KEYALG_LIST(KEYALG_LIST_ENTRY)
};
#undef KEYALG_LIST_ENTRY

static strbuf *get_base_public_blob(BinarySource *src,
                                    const opensshcert_extra *extra)
{
    strbuf *basepub = strbuf_new();
    put_stringz(basepub, extra->base_key_ssh_id);

    /* Make the base public key blob out of the public key
     * material in the certificate. This invocation of the
     * blobtrans system doesn't do any format translation, but it
     * does ensure that the right amount of data is copied so that
     * src ends up in the right position to read the remaining
     * certificate fields. */
    { // WINSCP
    BLOBTRANS_DECLARE(bt);
    blobtrans_read(bt, src, extra->pub_fmt);
    blobtrans_write(bt, BinarySink_UPCAST(basepub), extra->pub_fmt);
    blobtrans_clear(bt);

    return basepub;
    } // WINSCP
}

static opensshcert_key *opensshcert_new_shared(
    const ssh_keyalg *self, ptrlen blob, strbuf **basepub_out)
{
    const opensshcert_extra *extra = self->extra;

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, blob);

    /* Check the initial key-type string */
    if (!ptrlen_eq_string(get_string(src), extra->cert_key_ssh_id))
        return NULL;

    { // WINSCP
    opensshcert_key *ck = snew(opensshcert_key);
    memset(ck, 0, sizeof(*ck));
    ck->sshk.vt = self;

    ck->nonce = strbuf_dup(get_string(src));
    { // WINSCP
    strbuf *basepub = get_base_public_blob(src, extra);
    ck->serial = get_uint64(src);
    ck->type = get_uint32(src);
    ck->key_id = strbuf_dup(get_string(src));
    ck->valid_principals = strbuf_dup(get_string(src));
    ck->valid_after = get_uint64(src);
    ck->valid_before = get_uint64(src);
    ck->critical_options = strbuf_dup(get_string(src));
    ck->extensions = strbuf_dup(get_string(src));
    ck->reserved = strbuf_dup(get_string(src));
    ck->signature_key = strbuf_dup(get_string(src));
    ck->signature = strbuf_dup(get_string(src));


    if (get_err(src)) {
        ssh_key_free(&ck->sshk);
        strbuf_free(basepub);
        return NULL;
    }

    *basepub_out = basepub;
    return ck;
    } // WINSCP
    } // WINSCP
}

static ssh_key *opensshcert_new_pub(const ssh_keyalg *self, ptrlen pub)
{
    strbuf *basepub;
    opensshcert_key *ck = opensshcert_new_shared(self, pub, &basepub);
    if (!ck)
        return NULL;

    ck->basekey = ssh_key_new_pub(self->base_alg, ptrlen_from_strbuf(basepub));
    strbuf_free(basepub);

    if (!ck->basekey) {
        ssh_key_free(&ck->sshk);
        return NULL;
    }

    return &ck->sshk;
}

static ssh_key *opensshcert_new_priv(
    const ssh_keyalg *self, ptrlen pub, ptrlen priv)
{
    strbuf *basepub;
    opensshcert_key *ck = opensshcert_new_shared(self, pub, &basepub);
    if (!ck)
        return NULL;

    ck->basekey = ssh_key_new_priv(self->base_alg,
                                   ptrlen_from_strbuf(basepub), priv);
    strbuf_free(basepub);

    if (!ck->basekey) {
        ssh_key_free(&ck->sshk);
        return NULL;
    }

    return &ck->sshk;
}

static ssh_key *opensshcert_new_priv_openssh(
    const ssh_keyalg *self, BinarySource *src)
{
    const opensshcert_extra *extra = self->extra;

    ptrlen cert = get_string(src);

    strbuf *basepub;
    opensshcert_key *ck = opensshcert_new_shared(self, cert, &basepub);
    if (!ck)
        return NULL;

    { // WINSCP
    strbuf *baseossh = strbuf_new();

    /* Make the base OpenSSH key blob out of the public key blob
     * returned from opensshcert_new_shared, and the trailing
     * private data following the certificate */
    BLOBTRANS_DECLARE(bt);

    BinarySource pubsrc[1];
    BinarySource_BARE_INIT_PL(pubsrc, ptrlen_from_strbuf(basepub));
    get_string(pubsrc);            /* skip key type id */

    /* blobtrans_read might fail in this case, because we're reading
     * from two sources and they might fail to match */
    { // WINSCP
    bool success = blobtrans_read(bt, pubsrc, extra->pub_fmt) &&
        blobtrans_read(bt, src, extra->cert_ossh_fmt);

    blobtrans_write(bt, BinarySink_UPCAST(baseossh), extra->base_ossh_fmt);
    blobtrans_clear(bt);

    if (!success) {
        ssh_key_free(&ck->sshk);
        strbuf_free(basepub);
        strbuf_free(baseossh);
        return NULL;
    }

    strbuf_free(basepub);

    { // WINSCP
    BinarySource osshsrc[1];
    BinarySource_BARE_INIT_PL(osshsrc, ptrlen_from_strbuf(baseossh));
    ck->basekey = ssh_key_new_priv_openssh(self->base_alg, osshsrc);
    strbuf_free(baseossh);

    if (!ck->basekey) {
        ssh_key_free(&ck->sshk);
        return NULL;
    }

    return &ck->sshk;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static void opensshcert_freekey(ssh_key *key)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);

    /* If this function is called from one of the above constructors
     * because it failed part way through, we might not have managed
     * to construct ck->basekey, so it might be NULL. */
    if (ck->basekey)
        ssh_key_free(ck->basekey);

    strbuf_free(ck->nonce);
    strbuf_free(ck->key_id);
    strbuf_free(ck->valid_principals);
    strbuf_free(ck->critical_options);
    strbuf_free(ck->extensions);
    strbuf_free(ck->reserved);
    strbuf_free(ck->signature_key);
    strbuf_free(ck->signature);

    sfree(ck);
}

static ssh_key *opensshcert_base_key(ssh_key *key)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    return ck->basekey;
}

/*
 * Make a public key object from the CA public blob, potentially
 * taking into account that the signature might override the algorithm
 * name
 */
static ssh_key *opensshcert_ca_pub_key(
    opensshcert_key *ck, ptrlen sig, ptrlen *algname)
{
    ptrlen ca_keyblob = ptrlen_from_strbuf(ck->signature_key);

    ptrlen alg_source = sig.ptr ? sig : ca_keyblob;
    if (algname)
        *algname = pubkey_blob_to_alg_name(alg_source);

    { // WINSCP
    const ssh_keyalg *ca_alg = pubkey_blob_to_alg(alg_source);
    if (!ca_alg)
        return NULL;  /* don't even recognise the certifying key type */

    return ssh_key_new_pub(ca_alg, ca_keyblob);
    } // WINSCP
}

static void opensshcert_signature_preimage(opensshcert_key *ck, BinarySink *bs)
{
    const opensshcert_extra *extra = ck->sshk.vt->extra;
    put_stringz(bs, extra->cert_key_ssh_id);
    put_stringpl(bs, ptrlen_from_strbuf(ck->nonce));

    { // WINSCP
    strbuf *basepub = strbuf_new();
    ssh_key_public_blob(ck->basekey, BinarySink_UPCAST(basepub));
    { // WINSCP
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(basepub));
    get_string(src); /* skip initial key type string */
    put_data(bs, get_ptr(src), get_avail(src));
    strbuf_free(basepub);

    put_uint64(bs, ck->serial);
    put_uint32(bs, ck->type);
    put_stringpl(bs, ptrlen_from_strbuf(ck->key_id));
    put_stringpl(bs, ptrlen_from_strbuf(ck->valid_principals));
    put_uint64(bs, ck->valid_after);
    put_uint64(bs, ck->valid_before);
    put_stringpl(bs, ptrlen_from_strbuf(ck->critical_options));
    put_stringpl(bs, ptrlen_from_strbuf(ck->extensions));
    put_stringpl(bs, ptrlen_from_strbuf(ck->reserved));
    put_stringpl(bs, ptrlen_from_strbuf(ck->signature_key));
    } // WINSCP
    } // WINSCP
}

static void opensshcert_public_blob(ssh_key *key, BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);

    opensshcert_signature_preimage(ck, bs);
    put_stringpl(bs, ptrlen_from_strbuf(ck->signature));
}

static void opensshcert_private_blob(ssh_key *key, BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    ssh_key_private_blob(ck->basekey, bs);
}

static void opensshcert_openssh_blob(ssh_key *key, BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    const opensshcert_extra *extra = key->vt->extra;

    strbuf *cert = strbuf_new();
    ssh_key_public_blob(key, BinarySink_UPCAST(cert));
    put_stringsb(bs, cert);

    { // WINSCP
    strbuf *baseossh = strbuf_new_nm();
    ssh_key_openssh_blob(ck->basekey, BinarySink_UPCAST(baseossh));
    { // WINSCP
    BinarySource basesrc[1];
    BinarySource_BARE_INIT_PL(basesrc, ptrlen_from_strbuf(baseossh));

    { // WINSCP
    BLOBTRANS_DECLARE(bt);
    blobtrans_read(bt, basesrc, extra->base_ossh_fmt);
    blobtrans_write(bt, bs, extra->cert_ossh_fmt);
    blobtrans_clear(bt);

    strbuf_free(baseossh);
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static void opensshcert_ca_public_blob(ssh_key *key, BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    put_datapl(bs, ptrlen_from_strbuf(ck->signature_key));
}

static void opensshcert_cert_id_string(ssh_key *key, BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    put_datapl(bs, ptrlen_from_strbuf(ck->key_id));
}

static bool opensshcert_has_private(ssh_key *key)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    return ssh_key_has_private(ck->basekey);
}

static char *opensshcert_cache_str(ssh_key *key)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    return ssh_key_cache_str(ck->basekey);
}

static void opensshcert_time_to_iso8601(BinarySink *bs, uint64_t time)
{
    time_t t = time;
    char buf[256];
    put_data(bs, buf, strftime(buf, sizeof(buf),
                               "%Y-%m-%d %H:%M:%S UTC", gmtime(&t)));
}

static void opensshcert_string_list_key_components(
    key_components *kc, strbuf *input, const char *title, const char *title2)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(input));

    { // WINSCP
    const char *titles[2]; // WINSCP
    titles[0] = title;
    titles[1] = title2;
    { // WINSCP
    size_t ntitles = (title2 ? 2 : 1);

    unsigned index = 0;
    while (get_avail(src)) {
        size_t ti; // WINSCP
        for (ti = 0; ti < ntitles; ti++) {
            ptrlen value = get_string(src);
            if (get_err(src))
                break;
            { // WINSCP
            char *name = dupprintf("%s_%u", titles[ti], index);
            key_components_add_text_pl(kc, name, value);
            sfree(name);
            } // WINSCP
        }
        index++;
    }
    } // WINSCP
    } // WINSCP
}

static key_components *opensshcert_components(ssh_key *key)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    key_components *kc = ssh_key_components(ck->basekey);
    key_components_add_binary(kc, "cert_nonce", ptrlen_from_strbuf(ck->nonce));
    key_components_add_uint(kc, "cert_serial", ck->serial);
    switch (ck->type) {
      case SSH_CERT_TYPE_HOST:
        key_components_add_text(kc, "cert_type", "host");
        break;
      case SSH_CERT_TYPE_USER:
        key_components_add_text(kc, "cert_type", "user");
        break;
      default:
        key_components_add_uint(kc, "cert_type", ck->type);
        break;
    }
    key_components_add_text(kc, "cert_key_id", ck->key_id->s);
    opensshcert_string_list_key_components(kc, ck->valid_principals,
                                           "cert_valid_principal", NULL);
    key_components_add_uint(kc, "cert_valid_after", ck->valid_after);
    key_components_add_uint(kc, "cert_valid_before", ck->valid_before);
    /* Translate the validity period into human-legible dates, but
     * only if they're not the min/max integer. Rationale: if you see
     * "584554051223-11-09 07:00:15 UTC" as the expiry time you'll be
     * as likely to think it's a weird buffer overflow as half a
     * trillion years in the future! */
    if (ck->valid_after != 0) {
        strbuf *date = strbuf_new();
        opensshcert_time_to_iso8601(BinarySink_UPCAST(date), ck->valid_after);
        key_components_add_text_pl(kc, "cert_valid_after_date",
                                   ptrlen_from_strbuf(date));
        strbuf_free(date);
    }
    if (ck->valid_before != 0xFFFFFFFFFFFFFFFFLL ) { // WINSCP
        strbuf *date = strbuf_new();
        opensshcert_time_to_iso8601(BinarySink_UPCAST(date), ck->valid_before);
        key_components_add_text_pl(kc, "cert_valid_before_date",
                                   ptrlen_from_strbuf(date));
        strbuf_free(date);
    }
    opensshcert_string_list_key_components(kc, ck->critical_options,
                                           "cert_critical_option",
                                           "cert_critical_option_data");
    opensshcert_string_list_key_components(kc, ck->extensions,
                                           "cert_extension",
                                           "cert_extension_data");
    key_components_add_binary(kc, "cert_ca_key", ptrlen_from_strbuf(
                                  ck->signature_key));

    { // WINSCP
    ptrlen ca_algname;
    ssh_key *ca_key = opensshcert_ca_pub_key(ck, make_ptrlen(NULL, 0),
                                             &ca_algname);
    key_components_add_text_pl(kc, "cert_ca_key_algorithm_id", ca_algname);

    if (ca_key) {
        key_components *kc_ca_key = ssh_key_components(ca_key);
        size_t i; // WINSCP
        for (i = 0; i < kc_ca_key->ncomponents; i++) {
            key_component *comp = &kc_ca_key->components[i];
            char *subname = dupcat("cert_ca_key_", comp->name);
            key_components_add_copy(kc, subname, comp);
            sfree(subname);
        }
        key_components_free(kc_ca_key);
        ssh_key_free(ca_key);
    }

    key_components_add_binary(kc, "cert_ca_sig", ptrlen_from_strbuf(
                                  ck->signature));
    return kc;
    } // WINSCP
}

static SeatDialogText *opensshcert_cert_info(ssh_key *key)
{
#ifdef WINSCP
    assert(false);
    return NULL;
#else
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    SeatDialogText *text = seat_dialog_text_new();
    strbuf *tmp = strbuf_new();

    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Certificate type");
    switch (ck->type) {
      case SSH_CERT_TYPE_HOST:
        seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                "host key");
        seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                "Valid host names");
        break;
      case SSH_CERT_TYPE_USER:
        seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                "user authentication key");
        seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                "Valid user names");
        break;
      default:
        seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                "unknown type %" PRIu32, ck->type);
        seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                "Valid principals");
        break;
    }

    {
        BinarySource src[1];
        BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(
                                      ck->valid_principals));
        { // WINSCP
        const char *sep = "";
        strbuf_clear(tmp);
        while (get_avail(src)) {
            ptrlen principal = get_string(src);
            if (get_err(src))
                break;
            put_dataz(tmp, sep);
            sep = ",";
            put_datapl(tmp, principal);
        }
        seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                "%s", tmp->s);
        } // WINSCP
    }

    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Validity period");
    strbuf_clear(tmp);
    if (ck->valid_after == 0) {
        if (ck->valid_before == 0xFFFFFFFFFFFFFFFFLL) { // WINSCP
            put_dataz(tmp, "forever");
        } else {
            put_dataz(tmp, "until ");
            opensshcert_time_to_iso8601(BinarySink_UPCAST(tmp),
                                        ck->valid_before);
        }
    } else {
        if (ck->valid_before == 0xFFFFFFFFFFFFFFFFLL) { // WINSCP
            put_dataz(tmp, "after ");
            opensshcert_time_to_iso8601(BinarySink_UPCAST(tmp),
                                        ck->valid_after);
        } else {
            opensshcert_time_to_iso8601(BinarySink_UPCAST(tmp),
                                        ck->valid_after);
            put_dataz(tmp, " - ");
            opensshcert_time_to_iso8601(BinarySink_UPCAST(tmp),
                                        ck->valid_before);
        }
    }
    seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "%s", tmp->s);

    /*
     * List critical options we know about. (This is everything listed
     * in PROTOCOL.certkeys that isn't specific to U2F/FIDO key types
     * that PuTTY doesn't currently support.)
     */
    {
        BinarySource src[1];
        BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(
                                      ck->critical_options));
        strbuf_clear(tmp);
        while (get_avail(src)) {
            ptrlen key = get_string(src);
            ptrlen value = get_string(src);
            if (get_err(src))
                break;
            if (ck->type == SSH_CERT_TYPE_USER &&
                ptrlen_eq_string(key, "source-address")) {
                BinarySource src2[1];
                BinarySource_BARE_INIT_PL(src2, value);
                { // WINSCP
                ptrlen addresslist = get_string(src2);
                seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                        "Permitted client IP addresses");
                seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                        "%.*s", PTRLEN_PRINTF(addresslist));
                } // WINSCP
            } else if (ck->type == SSH_CERT_TYPE_USER &&
                       ptrlen_eq_string(key, "force-command")) {
                BinarySource src2[1];
                BinarySource_BARE_INIT_PL(src2, value);
                { // WINSCP
                ptrlen command = get_string(src2);
                seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                        "Forced remote command");
                seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                                        "%.*s", PTRLEN_PRINTF(command));
                } // WINSCP
            }
        }
    }

    /*
     * List certificate extensions. Again, we go through everything in
     * PROTOCOL.certkeys that isn't specific to U2F/FIDO key types.
     * But we also flip the sense round for user-readability: I think
     * it's more likely that the typical key will permit all these
     * things, so we emit no output in that case, and only mention the
     * things that _aren't_ enabled.
     */

    { // WINSCP
    bool x11_ok = false, agent_ok = false, portfwd_ok = false;
    bool pty_ok = false, user_rc_ok = false;

    {
        BinarySource src[1];
        BinarySource_BARE_INIT_PL(src, ptrlen_from_strbuf(
                                      ck->extensions));
        while (get_avail(src)) {
            ptrlen key = get_string(src);
            /* ptrlen value = */ get_string(src); // nothing needs this yet
            if (get_err(src))
                break;
            if (ptrlen_eq_string(key, "permit-X11-forwarding")) {
                x11_ok = true;
            } else if (ptrlen_eq_string(key, "permit-agent-forwarding")) {
                agent_ok = true;
            } else if (ptrlen_eq_string(key, "permit-port-forwarding")) {
                portfwd_ok = true;
            } else if (ptrlen_eq_string(key, "permit-pty")) {
                pty_ok = true;
            } else if (ptrlen_eq_string(key, "permit-user-rc")) {
                user_rc_ok = true;
            }
        }
    }

    if (ck->type == SSH_CERT_TYPE_USER) {
        if (!x11_ok) {
            seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                    "X11 forwarding permitted");
            seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "no");
        }
        if (!agent_ok) {
            seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                    "Agent forwarding permitted");
            seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "no");
        }
        if (!portfwd_ok) {
            seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                    "Port forwarding permitted");
            seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "no");
        }
        if (!pty_ok) {
            seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                    "PTY allocation permitted");
            seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "no");
        }
        if (!user_rc_ok) {
            seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                                    "Running user ~/.ssh.rc permitted");
            seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "no");
        }
    }

    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Certificate ID string");
    seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                            "%s", ck->key_id->s);
    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Certificate serial number");
    seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT,
                            "%" PRIu64, ck->serial);

    { // WINSCP
    char *fp = ssh2_fingerprint_blob(ptrlen_from_strbuf(ck->signature_key),
                                     SSH_FPTYPE_DEFAULT);
    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Fingerprint of signing CA key");
    seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "%s", fp);
    sfree(fp);

    fp = ssh2_fingerprint(key, ssh_fptype_to_cert(SSH_FPTYPE_DEFAULT));
    seat_dialog_text_append(text, SDT_MORE_INFO_KEY,
                            "Fingerprint including certificate");
    seat_dialog_text_append(text, SDT_MORE_INFO_VALUE_SHORT, "%s", fp);
    sfree(fp);

    strbuf_free(tmp);
    return text;
    } // WINSCP
    } // WINSCP
#endif
}

static int opensshcert_pubkey_bits(const ssh_keyalg *self, ptrlen blob)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, blob);

    get_string(src);                   /* key type */
    get_string(src);                   /* nonce */
    { // WINSCP
    strbuf *basepub = get_base_public_blob(src, self->extra);
    int bits = ssh_key_public_bits(
        self->base_alg, ptrlen_from_strbuf(basepub));
    strbuf_free(basepub);
    return bits;
    } // WINSCP
}

static unsigned opensshcert_supported_flags(const ssh_keyalg *self)
{
    return ssh_keyalg_supported_flags(self->base_alg);
}

static const char *opensshcert_alternate_ssh_id(const ssh_keyalg *self,
                                                unsigned flags)
{
    const char *base_id = ssh_keyalg_alternate_ssh_id(self->base_alg, flags);

    size_t i; // WINSCP
    for (i = 0; i < lenof(opensshcert_all_keyalgs); i++) {
        const ssh_keyalg *alg_i = opensshcert_all_keyalgs[i];
        if (!strcmp(base_id, alg_i->base_alg->ssh_id))
            return alg_i->ssh_id;
    }

    return self->ssh_id;
}

static char *opensshcert_alg_desc(const ssh_keyalg *self)
{
    char *base_desc = ssh_keyalg_desc(self->base_alg);
    char *our_desc = dupcat(base_desc, " cert");
    sfree(base_desc);
    return our_desc;
}

static bool opensshcert_variable_size(const ssh_keyalg *self)
{
    return ssh_keyalg_variable_size(self->base_alg);
}

static const ssh_keyalg *opensshcert_related_alg(const ssh_keyalg *self,
                                                 const ssh_keyalg *base)
{
    size_t i; // WINSCP
    for (i = 0; i < lenof(opensshcert_all_keyalgs); i++) {
        const ssh_keyalg *alg_i = opensshcert_all_keyalgs[i];
        if (base == alg_i->base_alg)
            return alg_i;
    }

    return self;
}

static char *opensshcert_invalid(ssh_key *key, unsigned flags)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    return ssh_key_invalid(ck->basekey, flags);
}

static bool opensshcert_check_cert(
    ssh_key *key, bool host, ptrlen principal, uint64_t time,
    const ca_options *opts, BinarySink *error)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    bool result = false;
    ssh_key *ca_key = NULL;
    strbuf *preimage = strbuf_new();
    BinarySource src[1];

    ptrlen signature = ptrlen_from_strbuf(ck->signature);

    /*
     * The OpenSSH certificate spec is one-layer only: it explicitly
     * forbids using a certified key in turn as the CA.
     *
     * If it did not, then we'd also have to recursively verify
     * everything up the CA chain until we reached the ultimate root,
     * and then make sure _that_ was something we trusted. (Not to
     * mention that there'd probably be an additional SSH_CERT_TYPE_CA
     * or some such, and certificate options saying what kinds of
     * certificate a CA was trusted to sign for, and ...)
     */
    ca_key = opensshcert_ca_pub_key(ck, make_ptrlen(NULL, 0), NULL);
    if (!ca_key) {
        put_fmt(error, "Certificate's signing key is invalid");
        goto out;
    }
    if (ssh_key_alg(ca_key)->is_certificate) {
        put_fmt(error, "Certificate is signed with a certified key "
                "(forbidden by OpenSSH certificate specification)");
        goto out;
    }

    /*
     * Now re-instantiate the key in a way that matches the signature
     * (i.e. so that if the key is an RSA one we get the right subtype
     * of RSA).
     */
    ssh_key_free(ca_key);
    ca_key = opensshcert_ca_pub_key(ck, signature, NULL);
    if (!ca_key) {
        put_fmt(error, "Certificate's signing key does not match "
                "signature type");
        goto out;
    }

    /* Check which signature algorithm is actually in use, because
     * that might be a reason to reject the certificate (e.g. ssh-rsa
     * when we wanted rsa-sha2-*). */
    { // WINSCP
    const ssh_keyalg *sig_alg = ssh_key_alg(ca_key);
    if ((sig_alg == &ssh_rsa && !opts->permit_rsa_sha1) ||
        (sig_alg == &ssh_rsa_sha256 && !opts->permit_rsa_sha256) ||
        (sig_alg == &ssh_rsa_sha512 && !opts->permit_rsa_sha512)) {
        put_fmt(error, "Certificate signature uses '%s' signature type "
                "(forbidden by user configuration)", sig_alg->ssh_id);
        goto out;
    }

    opensshcert_signature_preimage(ck, BinarySink_UPCAST(preimage));

    if (!ssh_key_verify(ca_key, signature, ptrlen_from_strbuf(preimage))) {
        put_fmt(error, "Certificate's signature is invalid");
        goto out;
    }

    { // WINSCP
    uint32_t expected_type = host ? SSH_CERT_TYPE_HOST : SSH_CERT_TYPE_USER;
    if (ck->type != expected_type) {
        put_fmt(error, "Certificate type is ");
        switch (ck->type) {
          case SSH_CERT_TYPE_HOST:
            put_fmt(error, "host");
            break;
          case SSH_CERT_TYPE_USER:
            put_fmt(error, "user");
            break;
          default:
            put_fmt(error, "unknown value %" PRIu32, ck->type);
            break;
        }
        put_fmt(error, "; expected %s", host ? "host" : "user");
        goto out;
    }

    /*
     * Check the time bounds on the certificate.
     */
    if (time < ck->valid_after) {
        put_fmt(error, "Certificate is not valid until ");
        opensshcert_time_to_iso8601(BinarySink_UPCAST(error),
                                    ck->valid_after);
        goto out;
    }
    if (time >= ck->valid_before) {
        put_fmt(error, "Certificate expired at ");
        opensshcert_time_to_iso8601(BinarySink_UPCAST(error),
                                    ck->valid_before);
        goto out;
    }

    /*
     * Check that this certificate is for the right thing.
     *
     * If valid_principals is a zero-length string then this is
     * specified to be a carte-blanche certificate valid for any
     * principal (at least, provided you trust the CA that issued it).
     */
    if (ck->valid_principals->len != 0) {
        BinarySource_BARE_INIT_PL(
            src, ptrlen_from_strbuf(ck->valid_principals));

        while (get_avail(src)) {
            ptrlen valid_principal = get_string(src);
            if (get_err(src)) {
                put_fmt(error, "Certificate's valid principals list is "
                        "incorrectly formatted");
                goto out;
            }
            if (ptrlen_eq_ptrlen(valid_principal, principal))
                goto principal_ok;
        }

        /*
         * No valid principal matched. Now go through the list a
         * second time writing the cert contents into the error
         * message, so that the user can see at a glance what went
         * wrong.
         *
         * (If you've typed the wrong spelling of the host name, you
         * really need to see "This cert is for 'foo.example.com' and
         * I was trying to match it against 'foo'", rather than just
         * "Computer says no".)
         */
        put_fmt(error, "Certificate's %s list [",
                host ? "hostname" : "username");
        BinarySource_BARE_INIT_PL(
            src, ptrlen_from_strbuf(ck->valid_principals));
        { // WINSCP
        const char *sep = "";
        while (get_avail(src)) {
            ptrlen valid_principal = get_string(src);
            put_fmt(error, "%s\"", sep);
            put_c_string_literal(error, valid_principal);
            put_fmt(error, "\"");
            sep = ", ";
        }
        put_fmt(error, "] does not contain expected %s \"",
                host ? "hostname" : "username");
        put_c_string_literal(error, principal);
        put_fmt(error, "\"");
        goto out;
      principal_ok:;
        } // WINSCP
    }

    /*
     * Check for critical options.
     */
    {
        BinarySource_BARE_INIT_PL(
            src, ptrlen_from_strbuf(ck->critical_options));

        while (get_avail(src)) {
            ptrlen option = get_string(src);
            ptrlen data = get_string(src);
            if (get_err(src)) {
                put_fmt(error, "Certificate's critical options list is "
                        "incorrectly formatted");
                goto out;
            }

            /*
             * If we ever do support any options, this will be where
             * we insert code to recognise and validate them.
             *
             * At present, we implement no critical options at all.
             * (For host certs, as of 2022-04-20, OpenSSH hasn't
             * defined any. For user certs, the only SSH server using
             * this is Uppity, which doesn't support key restrictions
             * in general.)
             */
            (void)data; /* no options supported => no use made of the data */

            /*
             * Report an unrecognised literal.
             */
            put_fmt(error, "Certificate specifies an unsupported critical "
                    "option \"");
            put_c_string_literal(error, option);
            put_fmt(error, "\"");
            goto out;
        }
    }

    /* If we get here without failing any check, accept the certificate! */
    result = true;

  out:
    if (ca_key)
        ssh_key_free(ca_key);
    strbuf_free(preimage);
    return result;
    } // WINSCP
    } // WINSCP
}

static bool opensshcert_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    /* This method is pure *signature* verification; checking the
     * certificate is done elsewhere. */
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    return ssh_key_verify(ck->basekey, sig, data);
}

static void opensshcert_sign(ssh_key *key, ptrlen data, unsigned flags,
                             BinarySink *bs)
{
    opensshcert_key *ck = container_of(key, opensshcert_key, sshk);
    ssh_key_sign(ck->basekey, data, flags, bs);
}
