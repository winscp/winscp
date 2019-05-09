/*
 * Digital Signature Standard implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ssh.h"
#include "mpint.h"
#include "misc.h"

static void dss_freekey(ssh_key *key);    /* forward reference */

static ssh_key *dss_new_pub(const ssh_keyalg *self, ptrlen data)
{
    BinarySource src[1];
    struct dss_key *dss;

    BinarySource_BARE_INIT_PL(src, data);
    if (!ptrlen_eq_string(get_string(src), "ssh-dss"))
	return NULL;

    dss = snew(struct dss_key);
    dss->sshk.vt = &ssh_dss;
    dss->p = get_mp_ssh2(src);
    dss->q = get_mp_ssh2(src);
    dss->g = get_mp_ssh2(src);
    dss->y = get_mp_ssh2(src);
    dss->x = NULL;

    if (get_err(src) ||
        mp_eq_integer(dss->p, 0) || mp_eq_integer(dss->q, 0)) {
        /* Invalid key. */
        dss_freekey(&dss->sshk);
        return NULL;
    }

    return &dss->sshk;
}

static void dss_freekey(ssh_key *key)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    if (dss->p)
        mp_free(dss->p);
    if (dss->q)
        mp_free(dss->q);
    if (dss->g)
        mp_free(dss->g);
    if (dss->y)
        mp_free(dss->y);
    if (dss->x)
        mp_free(dss->x);
    sfree(dss);
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

static char *dss_cache_str(ssh_key *key)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    strbuf *sb = strbuf_new();

    if (!dss->p)
	return NULL;

    append_hex_to_strbuf(sb, dss->p);
    append_hex_to_strbuf(sb, dss->q);
    append_hex_to_strbuf(sb, dss->g);
    append_hex_to_strbuf(sb, dss->y);

    return strbuf_to_str(sb);
}

static char *dss_invalid(ssh_key *key, unsigned flags)
{
    /* No validity criterion will stop us from using a DSA key at all */
    return NULL;
}

static bool dss_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    BinarySource src[1];
    unsigned char hash[20];
    bool toret;

    if (!dss->p)
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
    invalid |= mp_cmp_hs(r, dss->q);
    invalid |= mp_cmp_hs(s, dss->q);
    if (invalid) {
        mp_free(r);
        mp_free(s);
        return false;
    }

    /*
     * Step 1. w <- s^-1 mod q.
     */
    mp_int *w = mp_invert(s, dss->q);
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
    mp_int *u1 = mp_modmul(sha, w, dss->q);

    /*
     * Step 3. u2 <- r * w mod q.
     */
    mp_int *u2 = mp_modmul(r, w, dss->q);

    /*
     * Step 4. v <- (g^u1 * y^u2 mod p) mod q.
     */
    mp_int *gu1p = mp_modpow(dss->g, u1, dss->p);
    mp_int *yu2p = mp_modpow(dss->y, u2, dss->p);
    mp_int *gu1yu2p = mp_modmul(gu1p, yu2p, dss->p);
    mp_int *v = mp_mod(gu1yu2p, dss->q);

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

static void dss_public_blob(ssh_key *key, BinarySink *bs)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);

    put_stringz(bs, "ssh-dss");
    put_mp_ssh2(bs, dss->p);
    put_mp_ssh2(bs, dss->q);
    put_mp_ssh2(bs, dss->g);
    put_mp_ssh2(bs, dss->y);
}

static void dss_private_blob(ssh_key *key, BinarySink *bs)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);

    put_mp_ssh2(bs, dss->x);
}

static ssh_key *dss_new_priv(const ssh_keyalg *self, ptrlen pub, ptrlen priv)
{
    BinarySource src[1];
    ssh_key *sshk;
    struct dss_key *dss;
    ptrlen hash;
    unsigned char digest[20];
    mp_int *ytest;

    sshk = dss_new_pub(self, pub);
    if (!sshk)
        return NULL;

    dss = container_of(sshk, struct dss_key, sshk);
    BinarySource_BARE_INIT_PL(src, priv);
    dss->x = get_mp_ssh2(src);
    if (get_err(src)) {
        dss_freekey(&dss->sshk);
        return NULL;
    }

    /*
     * Check the obsolete hash in the old DSS key format.
     */
    hash = get_string(src);
    if (hash.len == 20) {
	ssh_hash *h = ssh_hash_new(&ssh_sha1);
	put_mp_ssh2(h, dss->p);
	put_mp_ssh2(h, dss->q);
	put_mp_ssh2(h, dss->g);
	ssh_hash_final(h, digest);
	if (!smemeq(hash.ptr, digest, 20)) {
	    dss_freekey(&dss->sshk);
	    return NULL;
	}
    }

    /*
     * Now ensure g^x mod p really is y.
     */
    ytest = mp_modpow(dss->g, dss->x, dss->p);
    if (!mp_cmp_eq(ytest, dss->y)) {
        mp_free(ytest);
	dss_freekey(&dss->sshk);
	return NULL;
    }
    mp_free(ytest);

    return &dss->sshk;
}

static ssh_key *dss_new_priv_openssh(const ssh_keyalg *self,
                                     BinarySource *src)
{
    struct dss_key *dss;

    dss = snew(struct dss_key);
    dss->sshk.vt = &ssh_dss;

    dss->p = get_mp_ssh2(src);
    dss->q = get_mp_ssh2(src);
    dss->g = get_mp_ssh2(src);
    dss->y = get_mp_ssh2(src);
    dss->x = get_mp_ssh2(src);

    if (get_err(src) ||
        mp_eq_integer(dss->q, 0) || mp_eq_integer(dss->p, 0)) {
        /* Invalid key. */
        dss_freekey(&dss->sshk);
        return NULL;
    }

    return &dss->sshk;
}

static void dss_openssh_blob(ssh_key *key, BinarySink *bs)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);

    put_mp_ssh2(bs, dss->p);
    put_mp_ssh2(bs, dss->q);
    put_mp_ssh2(bs, dss->g);
    put_mp_ssh2(bs, dss->y);
    put_mp_ssh2(bs, dss->x);
}

static int dss_pubkey_bits(const ssh_keyalg *self, ptrlen pub)
{
    ssh_key *sshk;
    struct dss_key *dss;
    int ret;

    sshk = dss_new_pub(self, pub);
    if (!sshk)
        return -1;

    dss = container_of(sshk, struct dss_key, sshk);
    ret = mp_get_nbits(dss->p);
    dss_freekey(&dss->sshk);

    return ret;
}

mp_int *dss_gen_k(const char *id_string, mp_int *modulus,
                     mp_int *private_key,
                     unsigned char *digest, int digest_len)
{
    /*
     * The basic DSS signing algorithm is:
     * 
     *  - invent a random k between 1 and q-1 (exclusive).
     *  - Compute r = (g^k mod p) mod q.
     *  - Compute s = k^-1 * (hash + x*r) mod q.
     * 
     * This has the dangerous properties that:
     * 
     *  - if an attacker in possession of the public key _and_ the
     *    signature (for example, the host you just authenticated
     *    to) can guess your k, he can reverse the computation of s
     *    and work out x = r^-1 * (s*k - hash) mod q. That is, he
     *    can deduce the private half of your key, and masquerade
     *    as you for as long as the key is still valid.
     * 
     *  - since r is a function purely of k and the public key, if
     *    the attacker only has a _range of possibilities_ for k
     *    it's easy for him to work through them all and check each
     *    one against r; he'll never be unsure of whether he's got
     *    the right one.
     * 
     *  - if you ever sign two different hashes with the same k, it
     *    will be immediately obvious because the two signatures
     *    will have the same r, and moreover an attacker in
     *    possession of both signatures (and the public key of
     *    course) can compute k = (hash1-hash2) * (s1-s2)^-1 mod q,
     *    and from there deduce x as before.
     * 
     *  - the Bleichenbacher attack on DSA makes use of methods of
     *    generating k which are significantly non-uniformly
     *    distributed; in particular, generating a 160-bit random
     *    number and reducing it mod q is right out.
     * 
     * For this reason we must be pretty careful about how we
     * generate our k. Since this code runs on Windows, with no
     * particularly good system entropy sources, we can't trust our
     * RNG itself to produce properly unpredictable data. Hence, we
     * use a totally different scheme instead.
     * 
     * What we do is to take a SHA-512 (_big_) hash of the private
     * key x, and then feed this into another SHA-512 hash that
     * also includes the message hash being signed. That is:
     * 
     *   proto_k = SHA512 ( SHA512(x) || SHA160(message) )
     * 
     * This number is 512 bits long, so reducing it mod q won't be
     * noticeably non-uniform. So
     * 
     *   k = proto_k mod q
     * 
     * This has the interesting property that it's _deterministic_:
     * signing the same hash twice with the same key yields the
     * same signature.
     * 
     * Despite this determinism, it's still not predictable to an
     * attacker, because in order to repeat the SHA-512
     * construction that created it, the attacker would have to
     * know the private key value x - and by assumption he doesn't,
     * because if he knew that he wouldn't be attacking k!
     *
     * (This trick doesn't, _per se_, protect against reuse of k.
     * Reuse of k is left to chance; all it does is prevent
     * _excessively high_ chances of reuse of k due to entropy
     * problems.)
     * 
     * Thanks to Colin Plumb for the general idea of using x to
     * ensure k is hard to guess, and to the Cambridge University
     * Computer Security Group for helping to argue out all the
     * fine details.
     */
    ssh_hash *h;
    unsigned char digest512[64];

    /*
     * Hash some identifying text plus x.
     */
    h = ssh_hash_new(&ssh_sha512);
    put_asciz(h, id_string);
    put_mp_ssh2(h, private_key);
    ssh_hash_final(h, digest512);

    /*
     * Now hash that digest plus the message hash.
     */
    h = ssh_hash_new(&ssh_sha512);
    put_data(h, digest512, sizeof(digest512));
    put_data(h, digest, digest_len);
    ssh_hash_final(h, digest512);

    /*
     * Now convert the result into a bignum, and coerce it to the
     * range [2,q), which we do by reducing it mod q-2 and adding 2.
     */
    mp_int *modminus2 = mp_copy(modulus);
    mp_sub_integer_into(modminus2, modminus2, 2);
    mp_int *proto_k = mp_from_bytes_be(make_ptrlen(digest512, 64));
    mp_int *k = mp_mod(proto_k, modminus2);
    mp_free(proto_k);
    mp_free(modminus2);
    mp_add_integer_into(k, k, 2);

    smemclr(digest512, sizeof(digest512));

    return k;
}

static void dss_sign(ssh_key *key, ptrlen data, unsigned flags, BinarySink *bs)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    unsigned char digest[20];
    int i;

    hash_simple(&ssh_sha1, data, digest);

    mp_int *k = dss_gen_k("DSA deterministic k generator", dss->q, dss->x,
                          digest, sizeof(digest));
    mp_int *kinv = mp_invert(k, dss->q);       /* k^-1 mod q */

    /*
     * Now we have k, so just go ahead and compute the signature.
     */
    mp_int *gkp = mp_modpow(dss->g, k, dss->p); /* g^k mod p */
    mp_int *r = mp_mod(gkp, dss->q);        /* r = (g^k mod p) mod q */
    mp_free(gkp);

    mp_int *hash = mp_from_bytes_be(make_ptrlen(digest, 20));
    mp_int *xr = mp_mul(dss->x, r);
    mp_int *hxr = mp_add(xr, hash);         /* hash + x*r */
    mp_int *s = mp_modmul(kinv, hxr, dss->q); /* s = k^-1 * (hash+x*r) mod q */
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

const ssh_keyalg ssh_dss = {
    dss_new_pub,
    dss_new_priv,
    dss_new_priv_openssh,

    dss_freekey,
    dss_invalid,
    dss_sign,
    dss_verify,
    dss_public_blob,
    dss_private_blob,
    dss_openssh_blob,
    dss_cache_str,

    dss_pubkey_bits,

    "ssh-dss",
    "dss",
    NULL,
    0, /* no supported flags */
};
