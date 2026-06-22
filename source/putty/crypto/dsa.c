/*
 * Digital Signature Algorithm implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ssh.h"
#include "mpint.h"
#include "misc.h"

static void dsa_freekey(ssh_key *key);    /* forward reference */

static ssh_key *dsa_new_pub(const ssh_keyalg *self, ptrlen data)
{
    BinarySource src[1];
    struct dsa_key *dsa;

    BinarySource_BARE_INIT_PL(src, data);
    if (!ptrlen_eq_string(get_string(src), "ssh-dss"))
        return NULL;

    dsa = snew(struct dsa_key);
    dsa->sshk.vt = &ssh_dsa;
    dsa->p = get_mp_ssh2(src);
    dsa->q = get_mp_ssh2(src);
    dsa->g = get_mp_ssh2(src);
    dsa->y = get_mp_ssh2(src);
    dsa->x = NULL;

    if (get_err(src) ||
        mp_eq_integer(dsa->p, 0) || mp_eq_integer(dsa->q, 0)) {
        /* Invalid key. */
        dsa_freekey(&dsa->sshk);
        return NULL;
    }

    return &dsa->sshk;
}

static void dsa_freekey(ssh_key *key)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    if (dsa->p)
        mp_free(dsa->p);
    if (dsa->q)
        mp_free(dsa->q);
    if (dsa->g)
        mp_free(dsa->g);
    if (dsa->y)
        mp_free(dsa->y);
    if (dsa->x)
        mp_free(dsa->x);
    sfree(dsa);
}

static void append_hex_to_strbuf(strbuf *sb, mp_int *x)
{
    if (sb->len > 0)
        put_byte(sb, ',');
    put_data(sb, "0x", 2);
    char *hex = mp_get_hex(x);
    size_t hexlen = strlen(hex);
    put_data(sb, hex, hexlen);
    smemclr(hex, hexlen);
    sfree(hex);
}

static char *dsa_cache_str(ssh_key *key)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    strbuf *sb = strbuf_new();

    if (!dsa->p) {
        strbuf_free(sb);
        return NULL;
    }

    append_hex_to_strbuf(sb, dsa->p);
    append_hex_to_strbuf(sb, dsa->q);
    append_hex_to_strbuf(sb, dsa->g);
    append_hex_to_strbuf(sb, dsa->y);

    return strbuf_to_str(sb);
}

static key_components *dsa_components(ssh_key *key)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    key_components *kc = key_components_new();

    key_components_add_text(kc, "key_type", "DSA");
    assert(dsa->p);
    key_components_add_mp(kc, "p", dsa->p);
    key_components_add_mp(kc, "q", dsa->q);
    key_components_add_mp(kc, "g", dsa->g);
    key_components_add_mp(kc, "public_y", dsa->y);
    if (dsa->x)
        key_components_add_mp(kc, "private_x", dsa->x);

    return kc;
}

static char *dsa_invalid(ssh_key *key, unsigned flags)
{
    /* No validity criterion will stop us from using a DSA key at all */
    return NULL;
}

static bool dsa_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    BinarySource src[1];
    unsigned char hash[20];
    bool toret;

    if (!dsa->p)
        return false;

    BinarySource_BARE_INIT_PL(src, sig);

    /*
     * Commercial SSH (2.0.13) and OpenSSH disagree over the format
     * of a DSA signature. OpenSSH is in line with RFC 4253:
     * it uses a string "ssh-dss", followed by a 40-byte string
     * containing two 160-bit integers end-to-end. Commercial SSH
     * can't be bothered with the header bit, and considers a DSA
     * signature blob to be _just_ the 40-byte string containing
     * the two 160-bit integers. We tell them apart by measuring
     * the length: length 40 means the commercial-SSH bug, anything
     * else is assumed to be RFC-compliant.
     */
    if (sig.len != 40) {      /* bug not present; read admin fields */
        ptrlen type = get_string(src);
        sig = get_string(src);

        if (get_err(src) || !ptrlen_eq_string(type, "ssh-dss") ||
            sig.len != 40)
            return false;
    }

    /* Now we're sitting on a 40-byte string for sure. */
    mp_int *r = mp_from_bytes_be(make_ptrlen(sig.ptr, 20));
    mp_int *s = mp_from_bytes_be(make_ptrlen((const char *)sig.ptr + 20, 20));
    if (!r || !s) {
        if (r)
            mp_free(r);
        if (s)
            mp_free(s);
        return false;
    }

    /* Basic sanity checks: 0 < r,s < q */
    unsigned invalid = 0;
    invalid |= mp_eq_integer(r, 0);
    invalid |= mp_eq_integer(s, 0);
    invalid |= mp_cmp_hs(r, dsa->q);
    invalid |= mp_cmp_hs(s, dsa->q);
    if (invalid) {
        mp_free(r);
        mp_free(s);
        return false;
    }

    /*
     * Step 1. w <- s^-1 mod q.
     */
    mp_int *w = mp_invert(s, dsa->q);
    if (!w) {
        mp_free(r);
        mp_free(s);
        return false;
    }

    /*
     * Step 2. u1 <- SHA(message) * w mod q.
     */
    hash_simple(&ssh_sha1, data, hash);
    mp_int *sha = mp_from_bytes_be(make_ptrlen(hash, 20));
    mp_int *u1 = mp_modmul(sha, w, dsa->q);

    /*
     * Step 3. u2 <- r * w mod q.
     */
    mp_int *u2 = mp_modmul(r, w, dsa->q);

    /*
     * Step 4. v <- (g^u1 * y^u2 mod p) mod q.
     */
    mp_int *gu1p = mp_modpow(dsa->g, u1, dsa->p);
    mp_int *yu2p = mp_modpow(dsa->y, u2, dsa->p);
    mp_int *gu1yu2p = mp_modmul(gu1p, yu2p, dsa->p);
    mp_int *v = mp_mod(gu1yu2p, dsa->q);

    /*
     * Step 5. v should now be equal to r.
     */

    toret = mp_cmp_eq(v, r);

    mp_free(w);
    mp_free(sha);
    mp_free(u1);
    mp_free(u2);
    mp_free(gu1p);
    mp_free(yu2p);
    mp_free(gu1yu2p);
    mp_free(v);
    mp_free(r);
    mp_free(s);

    return toret;
}

static void dsa_public_blob(ssh_key *key, BinarySink *bs)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);

    put_stringz(bs, "ssh-dss");
    put_mp_ssh2(bs, dsa->p);
    put_mp_ssh2(bs, dsa->q);
    put_mp_ssh2(bs, dsa->g);
    put_mp_ssh2(bs, dsa->y);
}

static void dsa_private_blob(ssh_key *key, BinarySink *bs)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);

    put_mp_ssh2(bs, dsa->x);
}

static ssh_key *dsa_new_priv(const ssh_keyalg *self, ptrlen pub, ptrlen priv)
{
    BinarySource src[1];
    ssh_key *sshk;
    struct dsa_key *dsa;
    ptrlen hash;
    unsigned char digest[20];
    mp_int *ytest;

    sshk = dsa_new_pub(self, pub);
    if (!sshk)
        return NULL;

    dsa = container_of(sshk, struct dsa_key, sshk);
    BinarySource_BARE_INIT_PL(src, priv);
    dsa->x = get_mp_ssh2(src);
    if (get_err(src)) {
        dsa_freekey(&dsa->sshk);
        return NULL;
    }

    /*
     * Check the obsolete hash in the old DSA key format.
     */
    hash = get_string(src);
    if (hash.len == 20) {
        ssh_hash *h = ssh_hash_new(&ssh_sha1);
        put_mp_ssh2(h, dsa->p);
        put_mp_ssh2(h, dsa->q);
        put_mp_ssh2(h, dsa->g);
        ssh_hash_final(h, digest);
        if (!smemeq(hash.ptr, digest, 20)) {
            dsa_freekey(&dsa->sshk);
            return NULL;
        }
    }

    /*
     * Now ensure g^x mod p really is y.
     */
    ytest = mp_modpow(dsa->g, dsa->x, dsa->p);
    if (!mp_cmp_eq(ytest, dsa->y)) {
        mp_free(ytest);
        dsa_freekey(&dsa->sshk);
        return NULL;
    }
    mp_free(ytest);

    return &dsa->sshk;
}

static ssh_key *dsa_new_priv_openssh(const ssh_keyalg *self,
                                     BinarySource *src)
{
    struct dsa_key *dsa;

    dsa = snew(struct dsa_key);
    dsa->sshk.vt = &ssh_dsa;

    dsa->p = get_mp_ssh2(src);
    dsa->q = get_mp_ssh2(src);
    dsa->g = get_mp_ssh2(src);
    dsa->y = get_mp_ssh2(src);
    dsa->x = get_mp_ssh2(src);

    if (get_err(src) ||
        mp_eq_integer(dsa->q, 0) || mp_eq_integer(dsa->p, 0)) {
        /* Invalid key. */
        dsa_freekey(&dsa->sshk);
        return NULL;
    }

    return &dsa->sshk;
}

static void dsa_openssh_blob(ssh_key *key, BinarySink *bs)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);

    put_mp_ssh2(bs, dsa->p);
    put_mp_ssh2(bs, dsa->q);
    put_mp_ssh2(bs, dsa->g);
    put_mp_ssh2(bs, dsa->y);
    put_mp_ssh2(bs, dsa->x);
}

static bool dsa_has_private(ssh_key *key)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    return dsa->x != NULL;
}

static int dsa_pubkey_bits(const ssh_keyalg *self, ptrlen pub)
{
    ssh_key *sshk;
    struct dsa_key *dsa;
    int ret;

    sshk = dsa_new_pub(self, pub);
    if (!sshk)
        return -1;

    dsa = container_of(sshk, struct dsa_key, sshk);
    ret = mp_get_nbits(dsa->p);
    dsa_freekey(&dsa->sshk);

    return ret;
}

static void dsa_sign(ssh_key *key, ptrlen data, unsigned flags, BinarySink *bs)
{
    struct dsa_key *dsa = container_of(key, struct dsa_key, sshk);
    unsigned char digest[20];
    int i;

    hash_simple(&ssh_sha1, data, digest);

    /* Generate any valid exponent k, using the RFC 6979 deterministic
     * procedure. */
    mp_int *k = rfc6979(&ssh_sha1, dsa->q, dsa->x, data);
    mp_int *kinv = mp_invert(k, dsa->q);       /* k^-1 mod q */

    /*
     * Now we have k, so just go ahead and compute the signature.
     */
    mp_int *gkp = mp_modpow(dsa->g, k, dsa->p); /* g^k mod p */
    mp_int *r = mp_mod(gkp, dsa->q);        /* r = (g^k mod p) mod q */
    mp_free(gkp);

    mp_int *hash = mp_from_bytes_be(make_ptrlen(digest, 20));
    mp_int *xr = mp_mul(dsa->x, r);
    mp_int *hxr = mp_add(xr, hash);         /* hash + x*r */
    mp_int *s = mp_modmul(kinv, hxr, dsa->q); /* s = k^-1 * (hash+x*r) mod q */
    mp_free(hxr);
    mp_free(xr);
    mp_free(kinv);
    mp_free(k);
    mp_free(hash);

    put_stringz(bs, "ssh-dss");
    put_uint32(bs, 40);
    for (i = 0; i < 20; i++)
        put_byte(bs, mp_get_byte(r, 19 - i));
    for (i = 0; i < 20; i++)
        put_byte(bs, mp_get_byte(s, 19 - i));
    mp_free(r);
    mp_free(s);
}

static char *dsa_alg_desc(const ssh_keyalg *self) { return dupstr("DSA"); }

const ssh_keyalg ssh_dsa = {
    .new_pub = dsa_new_pub,
    .new_priv = dsa_new_priv,
    .new_priv_openssh = dsa_new_priv_openssh,
    .freekey = dsa_freekey,
    .invalid = dsa_invalid,
    .sign = dsa_sign,
    .verify = dsa_verify,
    .public_blob = dsa_public_blob,
    .private_blob = dsa_private_blob,
    .openssh_blob = dsa_openssh_blob,
    .has_private = dsa_has_private,
    .cache_str = dsa_cache_str,
    .components = dsa_components,
    .base_key = nullkey_base_key,
    .pubkey_bits = dsa_pubkey_bits,
    .supported_flags = nullkey_supported_flags,
    .alternate_ssh_id = nullkey_alternate_ssh_id,
    .alg_desc = dsa_alg_desc,
    .variable_size = nullkey_variable_size_yes,
    .ssh_id = "ssh-dss",
    .cache_id = "dss",
};
