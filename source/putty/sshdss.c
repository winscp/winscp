/*
 * Digital Signature Standard implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ssh.h"
#include "misc.h"

static void dss_freekey(ssh_key *key);    /* forward reference */

static ssh_key *dss_new_pub(const ssh_keyalg *self, ptrlen data)
{
    BinarySource src[1];
    struct dss_key *dss;

    BinarySource_BARE_INIT(src, data.ptr, data.len);
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
        !bignum_cmp(dss->q, Zero) || !bignum_cmp(dss->p, Zero)) {
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
        freebn(dss->p);
    if (dss->q)
        freebn(dss->q);
    if (dss->g)
        freebn(dss->g);
    if (dss->y)
        freebn(dss->y);
    if (dss->x)
        freebn(dss->x);
    sfree(dss);
}

static void append_hex_to_strbuf(strbuf *sb, Bignum *x)
{
    if (sb->len > 0)
        put_byte(sb, ',');
    put_data(sb, "0x", 2);
    int nibbles = (3 + bignum_bitcount(x)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    static const char hex[] = "0123456789abcdef";
    for (int i = nibbles; i--;)
	put_byte(sb, hex[(bignum_byte(x, i / 2) >> (4 * (i % 2))) & 0xF]);
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

static bool dss_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    BinarySource src[1];
    unsigned char hash[20];
    Bignum r, s, w, gu1p, yu2p, gu1yu2p, u1, u2, sha, v;
    bool toret;

    if (!dss->p)
	return false;

    BinarySource_BARE_INIT(src, sig.ptr, sig.len);

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
    r = bignum_from_bytes(sig.ptr, 20);
    s = bignum_from_bytes((const char *)sig.ptr + 20, 20);
    if (!r || !s) {
        if (r)
            freebn(r);
        if (s)
            freebn(s);
	return false;
    }

    if (!bignum_cmp(s, Zero)) {
        freebn(r);
        freebn(s);
        return false;
    }

    /*
     * Step 1. w <- s^-1 mod q.
     */
    w = modinv(s, dss->q);
    if (!w) {
        freebn(r);
        freebn(s);
        return false;
    }

    /*
     * Step 2. u1 <- SHA(message) * w mod q.
     */
    SHA_Simple(data.ptr, data.len, hash);
    sha = bignum_from_bytes(hash, 20);
    u1 = modmul(sha, w, dss->q);

    /*
     * Step 3. u2 <- r * w mod q.
     */
    u2 = modmul(r, w, dss->q);

    /*
     * Step 4. v <- (g^u1 * y^u2 mod p) mod q.
     */
    gu1p = modpow(dss->g, u1, dss->p);
    yu2p = modpow(dss->y, u2, dss->p);
    gu1yu2p = modmul(gu1p, yu2p, dss->p);
    v = modmul(gu1yu2p, One, dss->q);

    /*
     * Step 5. v should now be equal to r.
     */

    toret = !bignum_cmp(v, r);

    freebn(w);
    freebn(sha);
    freebn(u1);
    freebn(u2);
    freebn(gu1p);
    freebn(yu2p);
    freebn(gu1yu2p);
    freebn(v);
    freebn(r);
    freebn(s);

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
    SHA_State s;
    unsigned char digest[20];
    Bignum ytest;

    sshk = dss_new_pub(self, pub);
    if (!sshk)
        return NULL;

    dss = container_of(sshk, struct dss_key, sshk);
    BinarySource_BARE_INIT(src, priv.ptr, priv.len);
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
	SHA_Init(&s);
	put_mp_ssh2(&s, dss->p);
	put_mp_ssh2(&s, dss->q);
	put_mp_ssh2(&s, dss->g);
	SHA_Final(&s, digest);
	if (0 != memcmp(hash.ptr, digest, 20)) {
	    dss_freekey(&dss->sshk);
	    return NULL;
	}
    }

    /*
     * Now ensure g^x mod p really is y.
     */
    ytest = modpow(dss->g, dss->x, dss->p);
    if (0 != bignum_cmp(ytest, dss->y)) {
	dss_freekey(&dss->sshk);
        freebn(ytest);
	return NULL;
    }
    freebn(ytest);

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
        !bignum_cmp(dss->q, Zero) || !bignum_cmp(dss->p, Zero)) {
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
    ret = bignum_bitcount(dss->p);
    dss_freekey(&dss->sshk);

    return ret;
}

Bignum *dss_gen_k(const char *id_string, Bignum modulus, Bignum private_key,
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
    SHA512_State ss;
    unsigned char digest512[64];
    Bignum proto_k, k;

    /*
     * Hash some identifying text plus x.
     */
    SHA512_Init(&ss);
    put_asciz(&ss, id_string);
    put_mp_ssh2(&ss, private_key);
    SHA512_Final(&ss, digest512);

    /*
     * Now hash that digest plus the message hash.
     */
    SHA512_Init(&ss);
    put_data(&ss, digest512, sizeof(digest512));
    put_data(&ss, digest, digest_len);

    while (1) {
        SHA512_State ss2 = ss;         /* structure copy */
        SHA512_Final(&ss2, digest512);

        smemclr(&ss2, sizeof(ss2));

        /*
         * Now convert the result into a bignum, and reduce it mod q.
         */
        proto_k = bignum_from_bytes(digest512, 64);
        k = bigmod(proto_k, modulus);
        freebn(proto_k);

        if (bignum_cmp(k, One) != 0 && bignum_cmp(k, Zero) != 0) {
            smemclr(&ss, sizeof(ss));
            smemclr(digest512, sizeof(digest512));
            return k;
        }

        /* Very unlikely we get here, but if so, k was unsuitable. */
        freebn(k);
        /* Perturb the hash to think of a different k. */
        put_byte(&ss, 'x');
        /* Go round and try again. */
    }
}

static void dss_sign(ssh_key *key, const void *data, int datalen,
                     unsigned flags, BinarySink *bs)
{
    struct dss_key *dss = container_of(key, struct dss_key, sshk);
    Bignum k, gkp, hash, kinv, hxr, r, s;
    unsigned char digest[20];
    int i;

    SHA_Simple(data, datalen, digest);

    k = dss_gen_k("DSA deterministic k generator", dss->q, dss->x,
                  digest, sizeof(digest));
    kinv = modinv(k, dss->q);	       /* k^-1 mod q */
    assert(kinv);

    /*
     * Now we have k, so just go ahead and compute the signature.
     */
    gkp = modpow(dss->g, k, dss->p);   /* g^k mod p */
    r = bigmod(gkp, dss->q);	       /* r = (g^k mod p) mod q */
    freebn(gkp);

    hash = bignum_from_bytes(digest, 20);
    hxr = bigmuladd(dss->x, r, hash);  /* hash + x*r */
    s = modmul(kinv, hxr, dss->q);     /* s = k^-1 * (hash + x*r) mod q */
    freebn(hxr);
    freebn(kinv);
    freebn(k);
    freebn(hash);

    put_stringz(bs, "ssh-dss");
    put_uint32(bs, 40);
    for (i = 0; i < 20; i++)
	put_byte(bs, bignum_byte(r, 19 - i));
    for (i = 0; i < 20; i++)
        put_byte(bs, bignum_byte(s, 19 - i));
    freebn(r);
    freebn(s);
}

const ssh_keyalg ssh_dss = {
    dss_new_pub,
    dss_new_priv,
    dss_new_priv_openssh,

    dss_freekey,
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
