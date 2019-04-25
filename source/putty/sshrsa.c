/*
 * RSA implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ssh.h"
#include "misc.h"

void BinarySource_get_rsa_ssh1_pub(
    BinarySource *src, struct RSAKey *rsa, RsaSsh1Order order)
{
    unsigned bits;
    Bignum e, m;

    bits = get_uint32(src);
    if (order == RSA_SSH1_EXPONENT_FIRST) {
        e = get_mp_ssh1(src);
        m = get_mp_ssh1(src);
    } else {
        m = get_mp_ssh1(src);
        e = get_mp_ssh1(src);
    }

    if (rsa) {
        rsa->bits = bits;
        rsa->exponent = e;
        rsa->modulus = m;
        rsa->bytes = (bignum_bitcount(m) + 7) / 8;
    } else {
        freebn(e);
        freebn(m);
    }
}

void BinarySource_get_rsa_ssh1_priv(
    BinarySource *src, struct RSAKey *rsa)
{
    rsa->private_exponent = get_mp_ssh1(src);
}

bool rsa_ssh1_encrypt(unsigned char *data, int length, struct RSAKey *key)
{
    Bignum b1, b2;
    int i;
    unsigned char *p;

    if (key->bytes < length + 4)
	return false;                  /* RSA key too short! */

    memmove(data + key->bytes - length, data, length);
    data[0] = 0;
    data[1] = 2;

    for (i = 2; i < key->bytes - length - 1; i++) {
	do {
	    data[i] = random_byte();
	} while (data[i] == 0);
    }
    data[key->bytes - length - 1] = 0;

    b1 = bignum_from_bytes(data, key->bytes);

    b2 = modpow(b1, key->exponent, key->modulus);

    p = data;
    for (i = key->bytes; i--;) {
	*p++ = bignum_byte(b2, i);
    }

    freebn(b1);
    freebn(b2);

    return true;
}

/*
 * Compute (base ^ exp) % mod, provided mod == p * q, with p,q
 * distinct primes, and iqmp is the multiplicative inverse of q mod p.
 * Uses Chinese Remainder Theorem to speed computation up over the
 * obvious implementation of a single big modpow.
 */
Bignum crt_modpow(Bignum base, Bignum exp, Bignum mod,
                  Bignum p, Bignum q, Bignum iqmp)
{
    Bignum pm1, qm1, pexp, qexp, presult, qresult, diff, multiplier, ret0, ret;

    /*
     * Reduce the exponent mod phi(p) and phi(q), to save time when
     * exponentiating mod p and mod q respectively. Of course, since p
     * and q are prime, phi(p) == p-1 and similarly for q.
     */
    pm1 = copybn(p);
    decbn(pm1);
    qm1 = copybn(q);
    decbn(qm1);
    pexp = bigmod(exp, pm1);
    qexp = bigmod(exp, qm1);

    /*
     * Do the two modpows.
     */
    presult = modpow(base, pexp, p);
    qresult = modpow(base, qexp, q);

    /*
     * Recombine the results. We want a value which is congruent to
     * qresult mod q, and to presult mod p.
     *
     * We know that iqmp * q is congruent to 1 * mod p (by definition
     * of iqmp) and to 0 mod q (obviously). So we start with qresult
     * (which is congruent to qresult mod both primes), and add on
     * (presult-qresult) * (iqmp * q) which adjusts it to be congruent
     * to presult mod p without affecting its value mod q.
     */
    if (bignum_cmp(presult, qresult) < 0) {
        /*
         * Can't subtract presult from qresult without first adding on
         * p.
         */
        Bignum tmp = presult;
        presult = bigadd(presult, p);
        freebn(tmp);
    }
    diff = bigsub(presult, qresult);
    multiplier = bigmul(iqmp, q);
    ret0 = bigmuladd(multiplier, diff, qresult);

    /*
     * Finally, reduce the result mod n.
     */
    ret = bigmod(ret0, mod);

    /*
     * Free all the intermediate results before returning.
     */
    freebn(pm1);
    freebn(qm1);
    freebn(pexp);
    freebn(qexp);
    freebn(presult);
    freebn(qresult);
    freebn(diff);
    freebn(multiplier);
    freebn(ret0);

    return ret;
}

/*
 * This function is a wrapper on modpow(). It has the same effect as
 * modpow(), but employs RSA blinding to protect against timing
 * attacks and also uses the Chinese Remainder Theorem (implemented
 * above, in crt_modpow()) to speed up the main operation.
 */
static Bignum rsa_privkey_op(Bignum input, struct RSAKey *key)
{
    Bignum random, random_encrypted, random_inverse;
    Bignum input_blinded, ret_blinded;
    Bignum ret;

    SHA512_State ss;
    unsigned char digest512[64];
    int digestused = lenof(digest512);
    int hashseq = 0;

    /*
     * Start by inventing a random number chosen uniformly from the
     * range 2..modulus-1. (We do this by preparing a random number
     * of the right length and retrying if it's greater than the
     * modulus, to prevent any potential Bleichenbacher-like
     * attacks making use of the uneven distribution within the
     * range that would arise from just reducing our number mod n.
     * There are timing implications to the potential retries, of
     * course, but all they tell you is the modulus, which you
     * already knew.)
     * 
     * To preserve determinism and avoid Pageant needing to share
     * the random number pool, we actually generate this `random'
     * number by hashing stuff with the private key.
     */
    while (1) {
	int bits, byte, bitsleft, v;
	random = copybn(key->modulus);
	/*
	 * Find the topmost set bit. (This function will return its
	 * index plus one.) Then we'll set all bits from that one
	 * downwards randomly.
	 */
	bits = bignum_bitcount(random);
	byte = 0;
	bitsleft = 0;
	while (bits--) {
	    if (bitsleft <= 0) {
		bitsleft = 8;
		/*
		 * Conceptually the following few lines are equivalent to
		 *    byte = random_byte();
		 */
		if (digestused >= lenof(digest512)) {
		    SHA512_Init(&ss);
		    put_data(&ss, "RSA deterministic blinding", 26);
		    put_uint32(&ss, hashseq);
		    put_mp_ssh2(&ss, key->private_exponent);
		    SHA512_Final(&ss, digest512);
		    hashseq++;

		    /*
		     * Now hash that digest plus the signature
		     * input.
		     */
		    SHA512_Init(&ss);
		    put_data(&ss, digest512, sizeof(digest512));
		    put_mp_ssh2(&ss, input);
		    SHA512_Final(&ss, digest512);

		    digestused = 0;
		}
		byte = digest512[digestused++];
	    }
	    v = byte & 1;
	    byte >>= 1;
	    bitsleft--;
	    bignum_set_bit(random, bits, v);
	}
        bn_restore_invariant(random);

	/*
	 * Now check that this number is strictly greater than
	 * zero, and strictly less than modulus.
	 */
	if (bignum_cmp(random, Zero) <= 0 ||
	    bignum_cmp(random, key->modulus) >= 0) {
	    freebn(random);
	    continue;
	}

        /*
         * Also, make sure it has an inverse mod modulus.
         */
        random_inverse = modinv(random, key->modulus);
        if (!random_inverse) {
	    freebn(random);
	    continue;
        }

        break;
    }

    /*
     * RSA blinding relies on the fact that (xy)^d mod n is equal
     * to (x^d mod n) * (y^d mod n) mod n. We invent a random pair
     * y and y^d; then we multiply x by y, raise to the power d mod
     * n as usual, and divide by y^d to recover x^d. Thus an
     * attacker can't correlate the timing of the modpow with the
     * input, because they don't know anything about the number
     * that was input to the actual modpow.
     * 
     * The clever bit is that we don't have to do a huge modpow to
     * get y and y^d; we will use the number we just invented as
     * _y^d_, and use the _public_ exponent to compute (y^d)^e = y
     * from it, which is much faster to do.
     */
    random_encrypted = crt_modpow(random, key->exponent,
                                  key->modulus, key->p, key->q, key->iqmp);
    input_blinded = modmul(input, random_encrypted, key->modulus);
    ret_blinded = crt_modpow(input_blinded, key->private_exponent,
                             key->modulus, key->p, key->q, key->iqmp);
    ret = modmul(ret_blinded, random_inverse, key->modulus);

    freebn(ret_blinded);
    freebn(input_blinded);
    freebn(random_inverse);
    freebn(random_encrypted);
    freebn(random);

    return ret;
}

Bignum rsa_ssh1_decrypt(Bignum input, struct RSAKey *key)
{
    return rsa_privkey_op(input, key);
}

bool rsa_ssh1_decrypt_pkcs1(Bignum input, struct RSAKey *key, strbuf *outbuf)
{
    strbuf *data = strbuf_new();
    bool success = false;
    BinarySource src[1];

    {
        Bignum *b = rsa_ssh1_decrypt(input, key);
        int i;
        for (i = (bignum_bitcount(key->modulus) + 7) / 8; i-- > 0 ;) {
            put_byte(data, bignum_byte(b, i));
        }
        freebn(b);
    }

    BinarySource_BARE_INIT(src, data->u, data->len);

    /* Check PKCS#1 formatting prefix */
    if (get_byte(src) != 0) goto out;
    if (get_byte(src) != 2) goto out;
    while (1) {
        unsigned char byte = get_byte(src);
        if (get_err(src)) goto out;
        if (byte == 0)
            break;
    }

    /* Everything else is the payload */
    success = true;
    put_data(outbuf, get_ptr(src), get_avail(src));

  out:
    strbuf_free(data);
    return success;
}

static void append_hex_to_strbuf(strbuf *sb, Bignum *x)
{
    if (sb->len > 0)
        put_byte(sb, ',');
    put_data(sb, "0x", 2);
    { // WINSCP
    int nibbles = (3 + bignum_bitcount(x)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    { // WINSCP
    static const char hex[] = "0123456789abcdef";
    int i; // WINSCP
    for (i = nibbles; i--;)
	put_byte(sb, hex[(bignum_byte(x, i / 2) >> (4 * (i % 2))) & 0xF]);
    } // WINSCP
    } // WINSCP
}

char *rsastr_fmt(struct RSAKey *key)
{
    strbuf *sb = strbuf_new();

    append_hex_to_strbuf(sb, key->exponent);
    append_hex_to_strbuf(sb, key->modulus);

    return strbuf_to_str(sb);
}

/*
 * Generate a fingerprint string for the key. Compatible with the
 * OpenSSH fingerprint code.
 */
char *rsa_ssh1_fingerprint(struct RSAKey *key)
{
    struct MD5Context md5c;
    unsigned char digest[16];
    strbuf *out;
    int i;

    MD5Init(&md5c);
    put_mp_ssh1(&md5c, key->modulus);
    put_mp_ssh1(&md5c, key->exponent);
    MD5Final(digest, &md5c);

    out = strbuf_new();
    strbuf_catf(out, "%d ", bignum_bitcount(key->modulus));
    for (i = 0; i < 16; i++)
	strbuf_catf(out, "%s%02x", i ? ":" : "", digest[i]);
    if (key->comment)
        strbuf_catf(out, " %s", key->comment);
    return strbuf_to_str(out);
}

/*
 * Verify that the public data in an RSA key matches the private
 * data. We also check the private data itself: we ensure that p >
 * q and that iqmp really is the inverse of q mod p.
 */
bool rsa_verify(struct RSAKey *key)
{
    Bignum n, ed, pm1, qm1;
    int cmp;

    /* n must equal pq. */
    n = bigmul(key->p, key->q);
    cmp = bignum_cmp(n, key->modulus);
    freebn(n);
    if (cmp != 0)
	return false;

    /* e * d must be congruent to 1, modulo (p-1) and modulo (q-1). */
    pm1 = copybn(key->p);
    decbn(pm1);
    ed = modmul(key->exponent, key->private_exponent, pm1);
    freebn(pm1);
    cmp = bignum_cmp(ed, One);
    freebn(ed);
    if (cmp != 0)
	return false;

    qm1 = copybn(key->q);
    decbn(qm1);
    ed = modmul(key->exponent, key->private_exponent, qm1);
    freebn(qm1);
    cmp = bignum_cmp(ed, One);
    freebn(ed);
    if (cmp != 0)
	return false;

    /*
     * Ensure p > q.
     *
     * I have seen key blobs in the wild which were generated with
     * p < q, so instead of rejecting the key in this case we
     * should instead flip them round into the canonical order of
     * p > q. This also involves regenerating iqmp.
     */
    if (bignum_cmp(key->p, key->q) <= 0) {
	Bignum tmp = key->p;
	key->p = key->q;
	key->q = tmp;

	freebn(key->iqmp);
	key->iqmp = modinv(key->q, key->p);
        if (!key->iqmp)
            return false;
    }

    /*
     * Ensure iqmp * q is congruent to 1, modulo p.
     */
    n = modmul(key->iqmp, key->q, key->p);
    cmp = bignum_cmp(n, One);
    freebn(n);
    if (cmp != 0)
	return false;

    return true;
}

void rsa_ssh1_public_blob(BinarySink *bs, struct RSAKey *key,
                          RsaSsh1Order order)
{
    put_uint32(bs, bignum_bitcount(key->modulus));
    if (order == RSA_SSH1_EXPONENT_FIRST) {
        put_mp_ssh1(bs, key->exponent);
        put_mp_ssh1(bs, key->modulus);
    } else {
        put_mp_ssh1(bs, key->modulus);
        put_mp_ssh1(bs, key->exponent);
    }
}

/* Given an SSH-1 public key blob, determine its length. */
int rsa_ssh1_public_blob_len(void *data, int maxlen)
{
    BinarySource src[1];

    BinarySource_BARE_INIT(src, data, maxlen);

    /* Expect a length word, then exponent and modulus. (It doesn't
     * even matter which order.) */
    get_uint32(src);
    freebn(get_mp_ssh1(src));
    freebn(get_mp_ssh1(src));

    if (get_err(src))
	return -1;

    /* Return the number of bytes consumed. */
    return src->pos;
}

void freersapriv(struct RSAKey *key)
{
    if (key->private_exponent) {
	freebn(key->private_exponent);
        key->private_exponent = NULL;
    }
    if (key->p) {
	freebn(key->p);
        key->p = NULL;
    }
    if (key->q) {
	freebn(key->q);
        key->q = NULL;
    }
    if (key->iqmp) {
	freebn(key->iqmp);
        key->iqmp = NULL;
    }
}

void freersakey(struct RSAKey *key)
{
    freersapriv(key);
    if (key->modulus) {
	freebn(key->modulus);
        key->modulus = NULL;
    }
    if (key->exponent) {
	freebn(key->exponent);
        key->exponent = NULL;
    }
    if (key->comment) {
	sfree(key->comment);
        key->comment = NULL;
    }
}

/* ----------------------------------------------------------------------
 * Implementation of the ssh-rsa signing key type. 
 */

static void rsa2_freekey(ssh_key *key);   /* forward reference */

static ssh_key *rsa2_new_pub(const ssh_keyalg *self, ptrlen data)
{
    BinarySource src[1];
    struct RSAKey *rsa;

    BinarySource_BARE_INIT(src, data.ptr, data.len);
    if (!ptrlen_eq_string(get_string(src), "ssh-rsa"))
	return NULL;

    rsa = snew(struct RSAKey);
    rsa->sshk.vt = &ssh_rsa;
    rsa->exponent = get_mp_ssh2(src);
    rsa->modulus = get_mp_ssh2(src);
    rsa->private_exponent = NULL;
    rsa->p = rsa->q = rsa->iqmp = NULL;
    rsa->comment = NULL;

    if (get_err(src)) {
	rsa2_freekey(&rsa->sshk);
	return NULL;
    }

    return &rsa->sshk;
}

static void rsa2_freekey(ssh_key *key)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);
    freersakey(rsa);
    sfree(rsa);
}

static char *rsa2_cache_str(ssh_key *key)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);
    return rsastr_fmt(rsa);
}

static void rsa2_public_blob(ssh_key *key, BinarySink *bs)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);

    put_stringz(bs, "ssh-rsa");
    put_mp_ssh2(bs, rsa->exponent);
    put_mp_ssh2(bs, rsa->modulus);
}

static void rsa2_private_blob(ssh_key *key, BinarySink *bs)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);

    put_mp_ssh2(bs, rsa->private_exponent);
    put_mp_ssh2(bs, rsa->p);
    put_mp_ssh2(bs, rsa->q);
    put_mp_ssh2(bs, rsa->iqmp);
}

static ssh_key *rsa2_new_priv(const ssh_keyalg *self,
                               ptrlen pub, ptrlen priv)
{
    BinarySource src[1];
    ssh_key *sshk;
    struct RSAKey *rsa;

    sshk = rsa2_new_pub(self, pub);
    if (!sshk)
        return NULL;

    rsa = container_of(sshk, struct RSAKey, sshk);
    BinarySource_BARE_INIT(src, priv.ptr, priv.len);
    rsa->private_exponent = get_mp_ssh2(src);
    rsa->p = get_mp_ssh2(src);
    rsa->q = get_mp_ssh2(src);
    rsa->iqmp = get_mp_ssh2(src);

    if (get_err(src) || !rsa_verify(rsa)) {
	rsa2_freekey(&rsa->sshk);
	return NULL;
    }

    return &rsa->sshk;
}

static ssh_key *rsa2_new_priv_openssh(const ssh_keyalg *self,
                                      BinarySource *src)
{
    struct RSAKey *rsa;

    rsa = snew(struct RSAKey);
    rsa->sshk.vt = &ssh_rsa;
    rsa->comment = NULL;

    rsa->modulus = get_mp_ssh2(src);
    rsa->exponent = get_mp_ssh2(src);
    rsa->private_exponent = get_mp_ssh2(src);
    rsa->iqmp = get_mp_ssh2(src);
    rsa->p = get_mp_ssh2(src);
    rsa->q = get_mp_ssh2(src);

    if (get_err(src) || !rsa_verify(rsa)) {
	rsa2_freekey(&rsa->sshk);
	return NULL;
    }

    return &rsa->sshk;
}

static void rsa2_openssh_blob(ssh_key *key, BinarySink *bs)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);

    put_mp_ssh2(bs, rsa->modulus);
    put_mp_ssh2(bs, rsa->exponent);
    put_mp_ssh2(bs, rsa->private_exponent);
    put_mp_ssh2(bs, rsa->iqmp);
    put_mp_ssh2(bs, rsa->p);
    put_mp_ssh2(bs, rsa->q);
}

static int rsa2_pubkey_bits(const ssh_keyalg *self, ptrlen pub)
{
    ssh_key *sshk;
    struct RSAKey *rsa;
    int ret;

    sshk = rsa2_new_pub(self, pub);
    if (!sshk)
        return -1;

    rsa = container_of(sshk, struct RSAKey, sshk);
    ret = bignum_bitcount(rsa->modulus);
    rsa2_freekey(&rsa->sshk);

    return ret;
}

/*
 * This is the magic ASN.1/DER prefix that goes in the decoded
 * signature, between the string of FFs and the actual SHA hash
 * value. The meaning of it is:
 * 
 * 00 -- this marks the end of the FFs; not part of the ASN.1 bit itself
 * 
 * 30 21 -- a constructed SEQUENCE of length 0x21
 *    30 09 -- a constructed sub-SEQUENCE of length 9
 *       06 05 -- an object identifier, length 5
 *          2B 0E 03 02 1A -- object id { 1 3 14 3 2 26 }
 *                            (the 1,3 comes from 0x2B = 43 = 40*1+3)
 *       05 00 -- NULL
 *    04 14 -- a primitive OCTET STRING of length 0x14
 *       [0x14 bytes of hash data follows]
 * 
 * The object id in the middle there is listed as `id-sha1' in
 * ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1d2.asn (the
 * ASN module for PKCS #1) and its expanded form is as follows:
 * 
 * id-sha1                OBJECT IDENTIFIER ::= {
 *    iso(1) identified-organization(3) oiw(14) secsig(3)
 *    algorithms(2) 26 }
 */
static const unsigned char sha1_asn1_prefix[] = {
    0x00, 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2B,
    0x0E, 0x03, 0x02, 0x1A, 0x05, 0x00, 0x04, 0x14,
};

/*
 * Two more similar pieces of ASN.1 used for signatures using SHA-256
 * and SHA-512, in the same format but differing only in various
 * length fields and OID.
 */
static const unsigned char sha256_asn1_prefix[] = {
    0x00, 0x30, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60,
    0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01,
    0x05, 0x00, 0x04, 0x20,
};
static const unsigned char sha512_asn1_prefix[] = {
    0x00, 0x30, 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60,
    0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03,
    0x05, 0x00, 0x04, 0x40,
};

#define SHA1_ASN1_PREFIX_LEN sizeof(sha1_asn1_prefix)

static unsigned char *rsa_pkcs1_signature_string(
    size_t nbytes, const struct ssh_hashalg *halg, ptrlen data)
{
    const unsigned char *asn1_prefix;
    unsigned asn1_prefix_size;

    if (halg == &ssh_sha256) {
        asn1_prefix = sha256_asn1_prefix;
        asn1_prefix_size = sizeof(sha256_asn1_prefix);
    } else if (halg == &ssh_sha512) {
        asn1_prefix = sha512_asn1_prefix;
        asn1_prefix_size = sizeof(sha512_asn1_prefix);
    } else {
        assert(halg == &ssh_sha1);
        asn1_prefix = sha1_asn1_prefix;
        asn1_prefix_size = sizeof(sha1_asn1_prefix);
    }

    { // WINSCP
    size_t fixed_parts = halg->hlen + asn1_prefix_size + 2;
    pinitassert(nbytes >= fixed_parts);
    size_t padding = nbytes - fixed_parts;

    unsigned char *bytes = snewn(nbytes, unsigned char);

    bytes[0] = 0;
    bytes[1] = 1;

    memset(bytes + 2, 0xFF, padding);

    memcpy(bytes + 2 + padding, asn1_prefix, asn1_prefix_size);

    { // WINSCP
    ssh_hash *h = ssh_hash_new(halg);
    put_data(h, data.ptr, data.len);
    ssh_hash_final(h, bytes + 2 + padding + asn1_prefix_size);
    } // WINSCP

    return bytes;
    } // WINSCP
}

static bool rsa2_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);
    BinarySource src[1];
    ptrlen type, in_pl;
    Bignum in, out;
    bool toret;

    BinarySource_BARE_INIT(src, sig.ptr, sig.len);
    type = get_string(src);
    /*
     * RFC 4253 section 6.6: the signature integer in an ssh-rsa
     * signature is 'without lengths or padding'. That is, we _don't_
     * expect the usual leading zero byte if the topmost bit of the
     * first byte is set. (However, because of the possibility of
     * BUG_SSH2_RSA_PADDING at the other end, we tolerate it if it's
     * there.) So we can't use get_mp_ssh2, which enforces that
     * leading-byte scheme; instead we use get_string and
     * bignum_from_bytes, which will tolerate anything.
     */
    in_pl = get_string(src);
    if (get_err(src) || !ptrlen_eq_string(type, "ssh-rsa"))
	return false;

    in = bignum_from_bytes(in_pl.ptr, in_pl.len);
    out = modpow(in, rsa->exponent, rsa->modulus);
    freebn(in);

    toret = true;

    { // WINSCP
    size_t nbytes = (bignum_bitcount(rsa->modulus) + 7) / 8;
    unsigned char *bytes = rsa_pkcs1_signature_string(nbytes, &ssh_sha1, data);
    size_t i; // WINSCP
    for (i = 0; i < nbytes; i++)
	if (bytes[nbytes-1 - i] != bignum_byte(out, i))
	    toret = false;
    smemclr(bytes, nbytes);
    sfree(bytes);
    } // WINSCP
    freebn(out);

    return toret;
}

static void rsa2_sign(ssh_key *key, const void *data, int datalen,
                      unsigned flags, BinarySink *bs)
{
    struct RSAKey *rsa = container_of(key, struct RSAKey, sshk);
    unsigned char *bytes;
    int nbytes;
    Bignum in, out;
    const struct ssh_hashalg *halg;
    const char *sign_alg_name;

    if (flags & SSH_AGENT_RSA_SHA2_256) {
        halg = &ssh_sha256;
        sign_alg_name = "rsa-sha2-256";
    } else if (flags & SSH_AGENT_RSA_SHA2_512) {
        halg = &ssh_sha512;
        sign_alg_name = "rsa-sha2-512";
    } else {
        halg = &ssh_sha1;
        sign_alg_name = "ssh-rsa";
    }

    nbytes = (bignum_bitcount(rsa->modulus) + 7) / 8;

    bytes = rsa_pkcs1_signature_string(
        nbytes, halg, make_ptrlen(data, datalen));
    in = bignum_from_bytes(bytes, nbytes);
    smemclr(bytes, nbytes);
    sfree(bytes);

    out = rsa_privkey_op(in, rsa);
    freebn(in);

    put_stringz(bs, sign_alg_name);
    nbytes = (bignum_bitcount(out) + 7) / 8;
    put_uint32(bs, nbytes);
    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < nbytes; i++)
	put_byte(bs, bignum_byte(out, nbytes - 1 - i));
    } // WINSCP

    freebn(out);
}

const ssh_keyalg ssh_rsa = {
    rsa2_new_pub,
    rsa2_new_priv,
    rsa2_new_priv_openssh,

    rsa2_freekey,
    rsa2_sign,
    rsa2_verify,
    rsa2_public_blob,
    rsa2_private_blob,
    rsa2_openssh_blob,
    rsa2_cache_str,

    rsa2_pubkey_bits,

    "ssh-rsa",
    "rsa2",
    NULL,
    SSH_AGENT_RSA_SHA2_256 | SSH_AGENT_RSA_SHA2_512,
};

struct RSAKey *ssh_rsakex_newkey(const void *data, int len)
{
    ssh_key *sshk = rsa2_new_pub(&ssh_rsa, make_ptrlen(data, len));
    if (!sshk)
        return NULL;
    return container_of(sshk, struct RSAKey, sshk);
}

void ssh_rsakex_freekey(struct RSAKey *key)
{
    rsa2_freekey(&key->sshk);
}

int ssh_rsakex_klen(struct RSAKey *rsa)
{
    return bignum_bitcount(rsa->modulus);
}

static void oaep_mask(const struct ssh_hashalg *h, void *seed, int seedlen,
		      void *vdata, int datalen)
{
    unsigned char *data = (unsigned char *)vdata;
    unsigned count = 0;

    while (datalen > 0) {
        int i, max = (datalen > h->hlen ? h->hlen : datalen);
        ssh_hash *s;
        unsigned char hash[SSH2_KEX_MAX_HASH_LEN];

	assert(h->hlen <= SSH2_KEX_MAX_HASH_LEN);
        s = ssh_hash_new(h);
        put_data(s, seed, seedlen);
        put_uint32(s, count);
        ssh_hash_final(s, hash);
        count++;

        for (i = 0; i < max; i++)
            data[i] ^= hash[i];

        data += max;
        datalen -= max;
    }
}

void ssh_rsakex_encrypt(const struct ssh_hashalg *h,
                        unsigned char *in, int inlen,
                        unsigned char *out, int outlen, struct RSAKey *rsa)
{
    Bignum b1, b2;
    int k, i;
    char *p;
    const int HLEN = h->hlen;

    /*
     * Here we encrypt using RSAES-OAEP. Essentially this means:
     * 
     *  - we have a SHA-based `mask generation function' which
     *    creates a pseudo-random stream of mask data
     *    deterministically from an input chunk of data.
     * 
     *  - we have a random chunk of data called a seed.
     * 
     *  - we use the seed to generate a mask which we XOR with our
     *    plaintext.
     * 
     *  - then we use _the masked plaintext_ to generate a mask
     *    which we XOR with the seed.
     * 
     *  - then we concatenate the masked seed and the masked
     *    plaintext, and RSA-encrypt that lot.
     * 
     * The result is that the data input to the encryption function
     * is random-looking and (hopefully) contains no exploitable
     * structure such as PKCS1-v1_5 does.
     * 
     * For a precise specification, see RFC 3447, section 7.1.1.
     * Some of the variable names below are derived from that, so
     * it'd probably help to read it anyway.
     */

    /* k denotes the length in octets of the RSA modulus. */
    k = (7 + bignum_bitcount(rsa->modulus)) / 8;

    /* The length of the input data must be at most k - 2hLen - 2. */
    assert(inlen > 0 && inlen <= k - 2*HLEN - 2);

    /* The length of the output data wants to be precisely k. */
    assert(outlen == k);

    /*
     * Now perform EME-OAEP encoding. First set up all the unmasked
     * output data.
     */
    /* Leading byte zero. */
    out[0] = 0;
    /* At position 1, the seed: HLEN bytes of random data. */
    for (i = 0; i < HLEN; i++)
        out[i + 1] = random_byte();
    /* At position 1+HLEN, the data block DB, consisting of: */
    /* The hash of the label (we only support an empty label here) */
    {
        ssh_hash *s = ssh_hash_new(h);
        ssh_hash_final(s, out + HLEN + 1);
    }
    /* A bunch of zero octets */
    memset(out + 2*HLEN + 1, 0, outlen - (2*HLEN + 1));
    /* A single 1 octet, followed by the input message data. */
    out[outlen - inlen - 1] = 1;
    memcpy(out + outlen - inlen, in, inlen);

    /*
     * Now use the seed data to mask the block DB.
     */
    oaep_mask(h, out+1, HLEN, out+HLEN+1, outlen-HLEN-1);

    /*
     * And now use the masked DB to mask the seed itself.
     */
    oaep_mask(h, out+HLEN+1, outlen-HLEN-1, out+1, HLEN);

    /*
     * Now `out' contains precisely the data we want to
     * RSA-encrypt.
     */
    b1 = bignum_from_bytes(out, outlen);
    b2 = modpow(b1, rsa->exponent, rsa->modulus);
    p = (char *)out;
    for (i = outlen; i--;) {
	*p++ = bignum_byte(b2, i);
    }
    freebn(b1);
    freebn(b2);

    /*
     * And we're done.
     */
}

Bignum ssh_rsakex_decrypt(const struct ssh_hashalg *h, ptrlen ciphertext,
                          struct RSAKey *rsa)
{
    Bignum b1, b2;
    int outlen, i;
    unsigned char *out;
    unsigned char labelhash[64];
    ssh_hash *hash;
    BinarySource src[1];
    const int HLEN = h->hlen;

    /*
     * Decryption side of the RSA key exchange operation.
     */

    /* The length of the encrypted data should be exactly the length
     * in octets of the RSA modulus.. */
    outlen = (7 + bignum_bitcount(rsa->modulus)) / 8;
    if (ciphertext.len != outlen)
        return NULL;

    /* Do the RSA decryption, and extract the result into a byte array. */
    b1 = bignum_from_bytes(ciphertext.ptr, ciphertext.len);
    b2 = rsa_privkey_op(b1, rsa);
    out = snewn(outlen, unsigned char);
    for (i = 0; i < outlen; i++)
        out[i] = bignum_byte(b2, outlen-1-i);
    freebn(b1);
    freebn(b2);

    /* Do the OAEP masking operations, in the reverse order from encryption */
    oaep_mask(h, out+HLEN+1, outlen-HLEN-1, out+1, HLEN);
    oaep_mask(h, out+1, HLEN, out+HLEN+1, outlen-HLEN-1);

    /* Check the leading byte is zero. */
    if (out[0] != 0) {
        sfree(out);
        return NULL;
    }
    /* Check the label hash at position 1+HLEN */
    assert(HLEN <= lenof(labelhash));
    hash = ssh_hash_new(h);
    ssh_hash_final(hash, labelhash);
    if (memcmp(out + HLEN + 1, labelhash, HLEN)) {
        sfree(out);
        return NULL;
    }
    /* Expect zero bytes followed by a 1 byte */
    for (i = 1 + 2 * HLEN; i < outlen; i++) {
        if (out[i] == 1) {
            i++;  /* skip over the 1 byte */
            break;
        } else if (out[i] != 1) {
            sfree(out);
            return NULL;
        }
    }
    /* And what's left is the input message data, which should be
     * encoded as an ordinary SSH-2 mpint. */
    BinarySource_BARE_INIT(src, out + i, outlen - i);
    b1 = get_mp_ssh2(src);
    sfree(out);
    if (get_err(src) || get_avail(src) != 0) {
        freebn(b1);
        return NULL;
    }

    /* Success! */
    return b1;
}

static const struct ssh_rsa_kex_extra ssh_rsa_kex_extra_sha1 = { 1024 };
static const struct ssh_rsa_kex_extra ssh_rsa_kex_extra_sha256 = { 2048 };

static const struct ssh_kex ssh_rsa_kex_sha1 = {
    "rsa1024-sha1", NULL, KEXTYPE_RSA,
    &ssh_sha1, &ssh_rsa_kex_extra_sha1,
};

static const struct ssh_kex ssh_rsa_kex_sha256 = {
    "rsa2048-sha256", NULL, KEXTYPE_RSA,
    &ssh_sha256, &ssh_rsa_kex_extra_sha256,
};

static const struct ssh_kex *const rsa_kex_list[] = {
    &ssh_rsa_kex_sha256,
    &ssh_rsa_kex_sha1
};

const struct ssh_kexes ssh_rsa_kex = {
    sizeof(rsa_kex_list) / sizeof(*rsa_kex_list),
    rsa_kex_list
};
