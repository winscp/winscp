/*
 * RSA implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ssh.h"
#include "misc.h"

int makekey(unsigned char *data, int len, struct RSAKey *result,
	    unsigned char **keystr, int order)
{
    unsigned char *p = data;
    int i, n;

    if (len < 4)
	return -1;

    if (result) {
	result->bits = 0;
	for (i = 0; i < 4; i++)
	    result->bits = (result->bits << 8) + *p++;
    } else
	p += 4;

    len -= 4;

    /*
     * order=0 means exponent then modulus (the keys sent by the
     * server). order=1 means modulus then exponent (the keys
     * stored in a keyfile).
     */

    if (order == 0) {
	n = ssh1_read_bignum(p, len, result ? &result->exponent : NULL);
	if (n < 0) return -1;
	p += n;
	len -= n;
    }

    n = ssh1_read_bignum(p, len, result ? &result->modulus : NULL);
    if (n < 0 || (result && bignum_bitcount(result->modulus) == 0)) return -1;
    if (result)
	result->bytes = n - 2;
    if (keystr)
	*keystr = p + 2;
    p += n;
    len -= n;

    if (order == 1) {
	n = ssh1_read_bignum(p, len, result ? &result->exponent : NULL);
	if (n < 0) return -1;
	p += n;
	len -= n;
    }
    return p - data;
}

int makeprivate(unsigned char *data, int len, struct RSAKey *result)
{
    return ssh1_read_bignum(data, len, &result->private_exponent);
}

int rsaencrypt(unsigned char *data, int length, struct RSAKey *key)
{
    Bignum b1, b2;
    int i;
    unsigned char *p;

    if (key->bytes < length + 4)
	return 0;		       /* RSA key too short! */

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

    return 1;
}

static void sha512_mpint(SHA512_State * s, Bignum b)
{
    unsigned char lenbuf[4];
    int len;
    len = (bignum_bitcount(b) + 8) / 8;
    PUT_32BIT(lenbuf, len);
    SHA512_Bytes(s, lenbuf, 4);
    while (len-- > 0) {
	lenbuf[0] = bignum_byte(b, len);
	SHA512_Bytes(s, lenbuf, 1);
    }
    memset(lenbuf, 0, sizeof(lenbuf));
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
		    unsigned char seqbuf[4];
		    PUT_32BIT(seqbuf, hashseq);
		    SHA512_Init(&ss);
		    SHA512_Bytes(&ss, "RSA deterministic blinding", 26);
		    SHA512_Bytes(&ss, seqbuf, sizeof(seqbuf));
		    sha512_mpint(&ss, key->private_exponent);
		    SHA512_Final(&ss, digest512);
		    hashseq++;

		    /*
		     * Now hash that digest plus the signature
		     * input.
		     */
		    SHA512_Init(&ss);
		    SHA512_Bytes(&ss, digest512, sizeof(digest512));
		    sha512_mpint(&ss, input);
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

	/*
	 * Now check that this number is strictly greater than
	 * zero, and strictly less than modulus.
	 */
	if (bignum_cmp(random, Zero) <= 0 ||
	    bignum_cmp(random, key->modulus) >= 0) {
	    freebn(random);
	    continue;
	} else {
	    break;
	}
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
    random_inverse = modinv(random, key->modulus);
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

Bignum rsadecrypt(Bignum input, struct RSAKey *key)
{
    return rsa_privkey_op(input, key);
}

int rsastr_len(struct RSAKey *key)
{
    Bignum md, ex;
    int mdlen, exlen;

    md = key->modulus;
    ex = key->exponent;
    mdlen = (bignum_bitcount(md) + 15) / 16;
    exlen = (bignum_bitcount(ex) + 15) / 16;
    return 4 * (mdlen + exlen) + 20;
}

void rsastr_fmt(char *str, struct RSAKey *key)
{
    Bignum md, ex;
    int len = 0, i, nibbles;
    static const char hex[] = "0123456789abcdef";

    md = key->modulus;
    ex = key->exponent;

    len += sprintf(str + len, "0x");

    nibbles = (3 + bignum_bitcount(ex)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	str[len++] = hex[(bignum_byte(ex, i / 2) >> (4 * (i % 2))) & 0xF];

    len += sprintf(str + len, ",0x");

    nibbles = (3 + bignum_bitcount(md)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	str[len++] = hex[(bignum_byte(md, i / 2) >> (4 * (i % 2))) & 0xF];

    str[len] = '\0';
}

/*
 * Generate a fingerprint string for the key. Compatible with the
 * OpenSSH fingerprint code.
 */
void rsa_fingerprint(char *str, int len, struct RSAKey *key)
{
    struct MD5Context md5c;
    unsigned char digest[16];
    char buffer[16 * 3 + 40];
    int numlen, slen, i;

    MD5Init(&md5c);
    numlen = ssh1_bignum_length(key->modulus) - 2;
    for (i = numlen; i--;) {
	unsigned char c = bignum_byte(key->modulus, i);
	MD5Update(&md5c, &c, 1);
    }
    numlen = ssh1_bignum_length(key->exponent) - 2;
    for (i = numlen; i--;) {
	unsigned char c = bignum_byte(key->exponent, i);
	MD5Update(&md5c, &c, 1);
    }
    MD5Final(digest, &md5c);

    sprintf(buffer, "%d ", bignum_bitcount(key->modulus));
    for (i = 0; i < 16; i++)
	sprintf(buffer + strlen(buffer), "%s%02x", i ? ":" : "",
		digest[i]);
    strncpy(str, buffer, len);
    str[len - 1] = '\0';
    slen = strlen(str);
    if (key->comment && slen < len - 1) {
	str[slen] = ' ';
	strncpy(str + slen + 1, key->comment, len - slen - 1);
	str[len - 1] = '\0';
    }
}

/*
 * Verify that the public data in an RSA key matches the private
 * data. We also check the private data itself: we ensure that p >
 * q and that iqmp really is the inverse of q mod p.
 */
int rsa_verify(struct RSAKey *key)
{
    Bignum n, ed, pm1, qm1;
    int cmp;

    /* n must equal pq. */
    n = bigmul(key->p, key->q);
    cmp = bignum_cmp(n, key->modulus);
    freebn(n);
    if (cmp != 0)
	return 0;

    /* e * d must be congruent to 1, modulo (p-1) and modulo (q-1). */
    pm1 = copybn(key->p);
    decbn(pm1);
    ed = modmul(key->exponent, key->private_exponent, pm1);
    cmp = bignum_cmp(ed, One);
    sfree(ed);
    if (cmp != 0)
	return 0;

    qm1 = copybn(key->q);
    decbn(qm1);
    ed = modmul(key->exponent, key->private_exponent, qm1);
    cmp = bignum_cmp(ed, One);
    sfree(ed);
    if (cmp != 0)
	return 0;

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
    }

    /*
     * Ensure iqmp * q is congruent to 1, modulo p.
     */
    n = modmul(key->iqmp, key->q, key->p);
    cmp = bignum_cmp(n, One);
    sfree(n);
    if (cmp != 0)
	return 0;

    return 1;
}

/* Public key blob as used by Pageant: exponent before modulus. */
unsigned char *rsa_public_blob(struct RSAKey *key, int *len)
{
    int length, pos;
    unsigned char *ret;

    length = (ssh1_bignum_length(key->modulus) +
	      ssh1_bignum_length(key->exponent) + 4);
    ret = snewn(length, unsigned char);

    PUT_32BIT(ret, bignum_bitcount(key->modulus));
    pos = 4;
    pos += ssh1_write_bignum(ret + pos, key->exponent);
    pos += ssh1_write_bignum(ret + pos, key->modulus);

    *len = length;
    return ret;
}

/* Given a public blob, determine its length. */
int rsa_public_blob_len(void *data, int maxlen)
{
    unsigned char *p = (unsigned char *)data;
    int n;

    if (maxlen < 4)
	return -1;
    p += 4;			       /* length word */
    maxlen -= 4;

    n = ssh1_read_bignum(p, maxlen, NULL);    /* exponent */
    if (n < 0)
	return -1;
    p += n;

    n = ssh1_read_bignum(p, maxlen, NULL);    /* modulus */
    if (n < 0)
	return -1;
    p += n;

    return p - (unsigned char *)data;
}

void freersakey(struct RSAKey *key)
{
    if (key->modulus)
	freebn(key->modulus);
    if (key->exponent)
	freebn(key->exponent);
    if (key->private_exponent)
	freebn(key->private_exponent);
    if (key->p)
	freebn(key->p);
    if (key->q)
	freebn(key->q);
    if (key->iqmp)
	freebn(key->iqmp);
    if (key->comment)
	sfree(key->comment);
}

/* ----------------------------------------------------------------------
 * Implementation of the ssh-rsa signing key type. 
 */

static void getstring(char **data, int *datalen, char **p, int *length)
{
    *p = NULL;
    if (*datalen < 4)
	return;
    *length = GET_32BIT(*data);
    *datalen -= 4;
    *data += 4;
    if (*datalen < *length)
	return;
    *p = *data;
    *data += *length;
    *datalen -= *length;
}
static Bignum getmp(char **data, int *datalen)
{
    char *p;
    int length;
    Bignum b;

    getstring(data, datalen, &p, &length);
    if (!p)
	return NULL;
    b = bignum_from_bytes((unsigned char *)p, length);
    return b;
}

static void *rsa2_newkey(char *data, int len)
{
    char *p;
    int slen;
    struct RSAKey *rsa;

    rsa = snew(struct RSAKey);
    if (!rsa)
	return NULL;
    getstring(&data, &len, &p, &slen);

    if (!p || slen != 7 || memcmp(p, "ssh-rsa", 7)) {
	sfree(rsa);
	return NULL;
    }
    rsa->exponent = getmp(&data, &len);
    rsa->modulus = getmp(&data, &len);
    rsa->private_exponent = NULL;
    rsa->p = rsa->q = rsa->iqmp = NULL;
    rsa->comment = NULL;

    return rsa;
}

static void rsa2_freekey(void *key)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    freersakey(rsa);
    sfree(rsa);
}

static char *rsa2_fmtkey(void *key)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    char *p;
    int len;

    len = rsastr_len(rsa);
    p = snewn(len, char);
    rsastr_fmt(p, rsa);
    return p;
}

static unsigned char *rsa2_public_blob(void *key, int *len)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    int elen, mlen, bloblen;
    int i;
    unsigned char *blob, *p;

    elen = (bignum_bitcount(rsa->exponent) + 8) / 8;
    mlen = (bignum_bitcount(rsa->modulus) + 8) / 8;

    /*
     * string "ssh-rsa", mpint exp, mpint mod. Total 19+elen+mlen.
     * (three length fields, 12+7=19).
     */
    bloblen = 19 + elen + mlen;
    blob = snewn(bloblen, unsigned char);
    p = blob;
    PUT_32BIT(p, 7);
    p += 4;
    memcpy(p, "ssh-rsa", 7);
    p += 7;
    PUT_32BIT(p, elen);
    p += 4;
    for (i = elen; i--;)
	*p++ = bignum_byte(rsa->exponent, i);
    PUT_32BIT(p, mlen);
    p += 4;
    for (i = mlen; i--;)
	*p++ = bignum_byte(rsa->modulus, i);
    assert(p == blob + bloblen);
    *len = bloblen;
    return blob;
}

static unsigned char *rsa2_private_blob(void *key, int *len)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    int dlen, plen, qlen, ulen, bloblen;
    int i;
    unsigned char *blob, *p;

    dlen = (bignum_bitcount(rsa->private_exponent) + 8) / 8;
    plen = (bignum_bitcount(rsa->p) + 8) / 8;
    qlen = (bignum_bitcount(rsa->q) + 8) / 8;
    ulen = (bignum_bitcount(rsa->iqmp) + 8) / 8;

    /*
     * mpint private_exp, mpint p, mpint q, mpint iqmp. Total 16 +
     * sum of lengths.
     */
    bloblen = 16 + dlen + plen + qlen + ulen;
    blob = snewn(bloblen, unsigned char);
    p = blob;
    PUT_32BIT(p, dlen);
    p += 4;
    for (i = dlen; i--;)
	*p++ = bignum_byte(rsa->private_exponent, i);
    PUT_32BIT(p, plen);
    p += 4;
    for (i = plen; i--;)
	*p++ = bignum_byte(rsa->p, i);
    PUT_32BIT(p, qlen);
    p += 4;
    for (i = qlen; i--;)
	*p++ = bignum_byte(rsa->q, i);
    PUT_32BIT(p, ulen);
    p += 4;
    for (i = ulen; i--;)
	*p++ = bignum_byte(rsa->iqmp, i);
    assert(p == blob + bloblen);
    *len = bloblen;
    return blob;
}

static void *rsa2_createkey(unsigned char *pub_blob, int pub_len,
			    unsigned char *priv_blob, int priv_len)
{
    struct RSAKey *rsa;
    char *pb = (char *) priv_blob;

    rsa = rsa2_newkey((char *) pub_blob, pub_len);
    rsa->private_exponent = getmp(&pb, &priv_len);
    rsa->p = getmp(&pb, &priv_len);
    rsa->q = getmp(&pb, &priv_len);
    rsa->iqmp = getmp(&pb, &priv_len);

    if (!rsa_verify(rsa)) {
	rsa2_freekey(rsa);
	return NULL;
    }

    return rsa;
}

static void *rsa2_openssh_createkey(unsigned char **blob, int *len)
{
    char **b = (char **) blob;
    struct RSAKey *rsa;

    rsa = snew(struct RSAKey);
    if (!rsa)
	return NULL;
    rsa->comment = NULL;

    rsa->modulus = getmp(b, len);
    rsa->exponent = getmp(b, len);
    rsa->private_exponent = getmp(b, len);
    rsa->iqmp = getmp(b, len);
    rsa->p = getmp(b, len);
    rsa->q = getmp(b, len);

    if (!rsa->modulus || !rsa->exponent || !rsa->private_exponent ||
	!rsa->iqmp || !rsa->p || !rsa->q) {
	sfree(rsa->modulus);
	sfree(rsa->exponent);
	sfree(rsa->private_exponent);
	sfree(rsa->iqmp);
	sfree(rsa->p);
	sfree(rsa->q);
	sfree(rsa);
	return NULL;
    }

    return rsa;
}

static int rsa2_openssh_fmtkey(void *key, unsigned char *blob, int len)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    int bloblen, i;

    bloblen =
	ssh2_bignum_length(rsa->modulus) +
	ssh2_bignum_length(rsa->exponent) +
	ssh2_bignum_length(rsa->private_exponent) +
	ssh2_bignum_length(rsa->iqmp) +
	ssh2_bignum_length(rsa->p) + ssh2_bignum_length(rsa->q);

    if (bloblen > len)
	return bloblen;

    bloblen = 0;
#define ENC(x) \
    PUT_32BIT(blob+bloblen, ssh2_bignum_length((x))-4); bloblen += 4; \
    for (i = ssh2_bignum_length((x))-4; i-- ;) blob[bloblen++]=bignum_byte((x),i);
    ENC(rsa->modulus);
    ENC(rsa->exponent);
    ENC(rsa->private_exponent);
    ENC(rsa->iqmp);
    ENC(rsa->p);
    ENC(rsa->q);

    return bloblen;
}

static int rsa2_pubkey_bits(void *blob, int len)
{
    struct RSAKey *rsa;
    int ret;

    rsa = rsa2_newkey((char *) blob, len);
    ret = bignum_bitcount(rsa->modulus);
    rsa2_freekey(rsa);

    return ret;
}

static char *rsa2_fingerprint(void *key)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    struct MD5Context md5c;
    unsigned char digest[16], lenbuf[4];
    char buffer[16 * 3 + 40];
    char *ret;
    int numlen, i;

    MD5Init(&md5c);
    MD5Update(&md5c, (unsigned char *)"\0\0\0\7ssh-rsa", 11);

#define ADD_BIGNUM(bignum) \
    numlen = (bignum_bitcount(bignum)+8)/8; \
    PUT_32BIT(lenbuf, numlen); MD5Update(&md5c, lenbuf, 4); \
    for (i = numlen; i-- ;) { \
        unsigned char c = bignum_byte(bignum, i); \
        MD5Update(&md5c, &c, 1); \
    }
    ADD_BIGNUM(rsa->exponent);
    ADD_BIGNUM(rsa->modulus);
#undef ADD_BIGNUM

    MD5Final(digest, &md5c);

    sprintf(buffer, "ssh-rsa %d ", bignum_bitcount(rsa->modulus));
    for (i = 0; i < 16; i++)
	sprintf(buffer + strlen(buffer), "%s%02x", i ? ":" : "",
		digest[i]);
    ret = snewn(strlen(buffer) + 1, char);
    if (ret)
	strcpy(ret, buffer);
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
static const unsigned char asn1_weird_stuff[] = {
    0x00, 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2B,
    0x0E, 0x03, 0x02, 0x1A, 0x05, 0x00, 0x04, 0x14,
};

#define ASN1_LEN ( (int) sizeof(asn1_weird_stuff) )

static int rsa2_verifysig(void *key, char *sig, int siglen,
			  char *data, int datalen)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    Bignum in, out;
    char *p;
    int slen;
    int bytes, i, j, ret;
    unsigned char hash[20];

    getstring(&sig, &siglen, &p, &slen);
    if (!p || slen != 7 || memcmp(p, "ssh-rsa", 7)) {
	return 0;
    }
    in = getmp(&sig, &siglen);
    out = modpow(in, rsa->exponent, rsa->modulus);
    freebn(in);

    ret = 1;

    bytes = (bignum_bitcount(rsa->modulus)+7) / 8;
    /* Top (partial) byte should be zero. */
    if (bignum_byte(out, bytes - 1) != 0)
	ret = 0;
    /* First whole byte should be 1. */
    if (bignum_byte(out, bytes - 2) != 1)
	ret = 0;
    /* Most of the rest should be FF. */
    for (i = bytes - 3; i >= 20 + ASN1_LEN; i--) {
	if (bignum_byte(out, i) != 0xFF)
	    ret = 0;
    }
    /* Then we expect to see the asn1_weird_stuff. */
    for (i = 20 + ASN1_LEN - 1, j = 0; i >= 20; i--, j++) {
	if (bignum_byte(out, i) != asn1_weird_stuff[j])
	    ret = 0;
    }
    /* Finally, we expect to see the SHA-1 hash of the signed data. */
    SHA_Simple(data, datalen, hash);
    for (i = 19, j = 0; i >= 0; i--, j++) {
	if (bignum_byte(out, i) != hash[j])
	    ret = 0;
    }
    freebn(out);

    return ret;
}

static unsigned char *rsa2_sign(void *key, char *data, int datalen,
				int *siglen)
{
    struct RSAKey *rsa = (struct RSAKey *) key;
    unsigned char *bytes;
    int nbytes;
    unsigned char hash[20];
    Bignum in, out;
    int i, j;

    SHA_Simple(data, datalen, hash);

    nbytes = (bignum_bitcount(rsa->modulus) - 1) / 8;
    assert(1 <= nbytes - 20 - ASN1_LEN);
    bytes = snewn(nbytes, unsigned char);

    bytes[0] = 1;
    for (i = 1; i < nbytes - 20 - ASN1_LEN; i++)
	bytes[i] = 0xFF;
    for (i = nbytes - 20 - ASN1_LEN, j = 0; i < nbytes - 20; i++, j++)
	bytes[i] = asn1_weird_stuff[j];
    for (i = nbytes - 20, j = 0; i < nbytes; i++, j++)
	bytes[i] = hash[j];

    in = bignum_from_bytes(bytes, nbytes);
    sfree(bytes);

    out = rsa_privkey_op(in, rsa);
    freebn(in);

    nbytes = (bignum_bitcount(out) + 7) / 8;
    bytes = snewn(4 + 7 + 4 + nbytes, unsigned char);
    PUT_32BIT(bytes, 7);
    memcpy(bytes + 4, "ssh-rsa", 7);
    PUT_32BIT(bytes + 4 + 7, nbytes);
    for (i = 0; i < nbytes; i++)
	bytes[4 + 7 + 4 + i] = bignum_byte(out, nbytes - 1 - i);
    freebn(out);

    *siglen = 4 + 7 + 4 + nbytes;
    return bytes;
}

const struct ssh_signkey ssh_rsa = {
    rsa2_newkey,
    rsa2_freekey,
    rsa2_fmtkey,
    rsa2_public_blob,
    rsa2_private_blob,
    rsa2_createkey,
    rsa2_openssh_createkey,
    rsa2_openssh_fmtkey,
    rsa2_pubkey_bits,
    rsa2_fingerprint,
    rsa2_verifysig,
    rsa2_sign,
    "ssh-rsa",
    "rsa2"
};

void *ssh_rsakex_newkey(char *data, int len)
{
    return rsa2_newkey(data, len);
}

void ssh_rsakex_freekey(void *key)
{
    rsa2_freekey(key);
}

int ssh_rsakex_klen(void *key)
{
    struct RSAKey *rsa = (struct RSAKey *) key;

    return bignum_bitcount(rsa->modulus);
}

static void oaep_mask(const struct ssh_hash *h, void *seed, int seedlen,
		      void *vdata, int datalen)
{
    unsigned char *data = (unsigned char *)vdata;
    unsigned count = 0;

    while (datalen > 0) {
        int i, max = (datalen > h->hlen ? h->hlen : datalen);
        void *s;
        unsigned char counter[4], hash[SSH2_KEX_MAX_HASH_LEN];

	assert(h->hlen <= SSH2_KEX_MAX_HASH_LEN);
        PUT_32BIT(counter, count);
        s = h->init();
        h->bytes(s, seed, seedlen);
        h->bytes(s, counter, 4);
        h->final(s, hash);
        count++;

        for (i = 0; i < max; i++)
            data[i] ^= hash[i];

        data += max;
        datalen -= max;
    }
}

void ssh_rsakex_encrypt(const struct ssh_hash *h, unsigned char *in, int inlen,
                        unsigned char *out, int outlen,
                        void *key)
{
    Bignum b1, b2;
    struct RSAKey *rsa = (struct RSAKey *) key;
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
    h->final(h->init(), out + HLEN + 1);
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

static const struct ssh_kex ssh_rsa_kex_sha1 = {
    "rsa1024-sha1", NULL, KEXTYPE_RSA, NULL, NULL, 0, 0, &ssh_sha1
};

static const struct ssh_kex ssh_rsa_kex_sha256 = {
    "rsa2048-sha256", NULL, KEXTYPE_RSA, NULL, NULL, 0, 0, &ssh_sha256
};

static const struct ssh_kex *const rsa_kex_list[] = {
    &ssh_rsa_kex_sha256,
    &ssh_rsa_kex_sha1
};

const struct ssh_kexes ssh_rsa_kex = {
    sizeof(rsa_kex_list) / sizeof(*rsa_kex_list),
    rsa_kex_list
};
