/*
 * Digital Signature Standard implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "ssh.h"
#include "misc.h"

static void sha_mpint(SHA_State * s, Bignum b)
{
    unsigned char lenbuf[4];
    int len;
    len = (bignum_bitcount(b) + 8) / 8;
    PUT_32BIT(lenbuf, len);
    SHA_Bytes(s, lenbuf, 4);
    while (len-- > 0) {
	lenbuf[0] = bignum_byte(b, len);
	SHA_Bytes(s, lenbuf, 1);
    }
    memset(lenbuf, 0, sizeof(lenbuf));
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
    if (p[0] & 0x80)
	return NULL;		       /* negative mp */
    b = bignum_from_bytes((unsigned char *)p, length);
    return b;
}

static Bignum get160(char **data, int *datalen)
{
    Bignum b;

    b = bignum_from_bytes((unsigned char *)*data, 20);
    *data += 20;
    *datalen -= 20;

    return b;
}

static void *dss_newkey(char *data, int len)
{
    char *p;
    int slen;
    struct dss_key *dss;

    dss = snew(struct dss_key);
    if (!dss)
	return NULL;
    getstring(&data, &len, &p, &slen);

#ifdef DEBUG_DSS
    {
	int i;
	printf("key:");
	for (i = 0; i < len; i++)
	    printf("  %02x", (unsigned char) (data[i]));
	printf("\n");
    }
#endif

    if (!p || memcmp(p, "ssh-dss", 7)) {
	sfree(dss);
	return NULL;
    }
    dss->p = getmp(&data, &len);
    dss->q = getmp(&data, &len);
    dss->g = getmp(&data, &len);
    dss->y = getmp(&data, &len);

    return dss;
}

static void dss_freekey(void *key)
{
    struct dss_key *dss = (struct dss_key *) key;
    freebn(dss->p);
    freebn(dss->q);
    freebn(dss->g);
    freebn(dss->y);
    sfree(dss);
}

static char *dss_fmtkey(void *key)
{
    struct dss_key *dss = (struct dss_key *) key;
    char *p;
    int len, i, pos, nibbles;
    static const char hex[] = "0123456789abcdef";
    if (!dss->p)
	return NULL;
    len = 8 + 4 + 1;		       /* 4 x "0x", punctuation, \0 */
    len += 4 * (bignum_bitcount(dss->p) + 15) / 16;
    len += 4 * (bignum_bitcount(dss->q) + 15) / 16;
    len += 4 * (bignum_bitcount(dss->g) + 15) / 16;
    len += 4 * (bignum_bitcount(dss->y) + 15) / 16;
    p = snewn(len, char);
    if (!p)
	return NULL;

    pos = 0;
    pos += sprintf(p + pos, "0x");
    nibbles = (3 + bignum_bitcount(dss->p)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	p[pos++] =
	    hex[(bignum_byte(dss->p, i / 2) >> (4 * (i % 2))) & 0xF];
    pos += sprintf(p + pos, ",0x");
    nibbles = (3 + bignum_bitcount(dss->q)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	p[pos++] =
	    hex[(bignum_byte(dss->q, i / 2) >> (4 * (i % 2))) & 0xF];
    pos += sprintf(p + pos, ",0x");
    nibbles = (3 + bignum_bitcount(dss->g)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	p[pos++] =
	    hex[(bignum_byte(dss->g, i / 2) >> (4 * (i % 2))) & 0xF];
    pos += sprintf(p + pos, ",0x");
    nibbles = (3 + bignum_bitcount(dss->y)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    for (i = nibbles; i--;)
	p[pos++] =
	    hex[(bignum_byte(dss->y, i / 2) >> (4 * (i % 2))) & 0xF];
    p[pos] = '\0';
    return p;
}

static char *dss_fingerprint(void *key)
{
    struct dss_key *dss = (struct dss_key *) key;
    struct MD5Context md5c;
    unsigned char digest[16], lenbuf[4];
    char buffer[16 * 3 + 40];
    char *ret;
    int numlen, i;

    MD5Init(&md5c);
    MD5Update(&md5c, (unsigned char *)"\0\0\0\7ssh-dss", 11);

#define ADD_BIGNUM(bignum) \
    numlen = (bignum_bitcount(bignum)+8)/8; \
    PUT_32BIT(lenbuf, numlen); MD5Update(&md5c, lenbuf, 4); \
    for (i = numlen; i-- ;) { \
        unsigned char c = bignum_byte(bignum, i); \
        MD5Update(&md5c, &c, 1); \
    }
    ADD_BIGNUM(dss->p);
    ADD_BIGNUM(dss->q);
    ADD_BIGNUM(dss->g);
    ADD_BIGNUM(dss->y);
#undef ADD_BIGNUM

    MD5Final(digest, &md5c);

    sprintf(buffer, "ssh-dss %d ", bignum_bitcount(dss->p));
    for (i = 0; i < 16; i++)
	sprintf(buffer + strlen(buffer), "%s%02x", i ? ":" : "",
		digest[i]);
    ret = snewn(strlen(buffer) + 1, char);
    if (ret)
	strcpy(ret, buffer);
    return ret;
}

static int dss_verifysig(void *key, char *sig, int siglen,
			 char *data, int datalen)
{
    struct dss_key *dss = (struct dss_key *) key;
    char *p;
    int slen;
    char hash[20];
    Bignum r, s, w, gu1p, yu2p, gu1yu2p, u1, u2, sha, v;
    int ret;

    if (!dss->p)
	return 0;

#ifdef DEBUG_DSS
    {
	int i;
	printf("sig:");
	for (i = 0; i < siglen; i++)
	    printf("  %02x", (unsigned char) (sig[i]));
	printf("\n");
    }
#endif
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
    if (siglen != 40) {		       /* bug not present; read admin fields */
	getstring(&sig, &siglen, &p, &slen);
	if (!p || slen != 7 || memcmp(p, "ssh-dss", 7)) {
	    return 0;
	}
	sig += 4, siglen -= 4;	       /* skip yet another length field */
    }
    r = get160(&sig, &siglen);
    s = get160(&sig, &siglen);
    if (!r || !s)
	return 0;

    /*
     * Step 1. w <- s^-1 mod q.
     */
    w = modinv(s, dss->q);

    /*
     * Step 2. u1 <- SHA(message) * w mod q.
     */
    SHA_Simple(data, datalen, (unsigned char *)hash);
    p = hash;
    slen = 20;
    sha = get160(&p, &slen);
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

    ret = !bignum_cmp(v, r);

    freebn(w);
    freebn(sha);
    freebn(gu1p);
    freebn(yu2p);
    freebn(gu1yu2p);
    freebn(v);
    freebn(r);
    freebn(s);

    return ret;
}

static unsigned char *dss_public_blob(void *key, int *len)
{
    struct dss_key *dss = (struct dss_key *) key;
    int plen, qlen, glen, ylen, bloblen;
    int i;
    unsigned char *blob, *p;

    plen = (bignum_bitcount(dss->p) + 8) / 8;
    qlen = (bignum_bitcount(dss->q) + 8) / 8;
    glen = (bignum_bitcount(dss->g) + 8) / 8;
    ylen = (bignum_bitcount(dss->y) + 8) / 8;

    /*
     * string "ssh-dss", mpint p, mpint q, mpint g, mpint y. Total
     * 27 + sum of lengths. (five length fields, 20+7=27).
     */
    bloblen = 27 + plen + qlen + glen + ylen;
    blob = snewn(bloblen, unsigned char);
    p = blob;
    PUT_32BIT(p, 7);
    p += 4;
    memcpy(p, "ssh-dss", 7);
    p += 7;
    PUT_32BIT(p, plen);
    p += 4;
    for (i = plen; i--;)
	*p++ = bignum_byte(dss->p, i);
    PUT_32BIT(p, qlen);
    p += 4;
    for (i = qlen; i--;)
	*p++ = bignum_byte(dss->q, i);
    PUT_32BIT(p, glen);
    p += 4;
    for (i = glen; i--;)
	*p++ = bignum_byte(dss->g, i);
    PUT_32BIT(p, ylen);
    p += 4;
    for (i = ylen; i--;)
	*p++ = bignum_byte(dss->y, i);
    assert(p == blob + bloblen);
    *len = bloblen;
    return blob;
}

static unsigned char *dss_private_blob(void *key, int *len)
{
    struct dss_key *dss = (struct dss_key *) key;
    int xlen, bloblen;
    int i;
    unsigned char *blob, *p;

    xlen = (bignum_bitcount(dss->x) + 8) / 8;

    /*
     * mpint x, string[20] the SHA of p||q||g. Total 4 + xlen.
     */
    bloblen = 4 + xlen;
    blob = snewn(bloblen, unsigned char);
    p = blob;
    PUT_32BIT(p, xlen);
    p += 4;
    for (i = xlen; i--;)
	*p++ = bignum_byte(dss->x, i);
    assert(p == blob + bloblen);
    *len = bloblen;
    return blob;
}

static void *dss_createkey(unsigned char *pub_blob, int pub_len,
			   unsigned char *priv_blob, int priv_len)
{
    struct dss_key *dss;
    char *pb = (char *) priv_blob;
    char *hash;
    int hashlen;
    SHA_State s;
    unsigned char digest[20];
    Bignum ytest;

    dss = dss_newkey((char *) pub_blob, pub_len);
    dss->x = getmp(&pb, &priv_len);

    /*
     * Check the obsolete hash in the old DSS key format.
     */
    hashlen = -1;
    getstring(&pb, &priv_len, &hash, &hashlen);
    if (hashlen == 20) {
	SHA_Init(&s);
	sha_mpint(&s, dss->p);
	sha_mpint(&s, dss->q);
	sha_mpint(&s, dss->g);
	SHA_Final(&s, digest);
	if (0 != memcmp(hash, digest, 20)) {
	    dss_freekey(dss);
	    return NULL;
	}
    }

    /*
     * Now ensure g^x mod p really is y.
     */
    ytest = modpow(dss->g, dss->x, dss->p);
    if (0 != bignum_cmp(ytest, dss->y)) {
	dss_freekey(dss);
	return NULL;
    }
    freebn(ytest);

    return dss;
}

static void *dss_openssh_createkey(unsigned char **blob, int *len)
{
    char **b = (char **) blob;
    struct dss_key *dss;

    dss = snew(struct dss_key);
    if (!dss)
	return NULL;

    dss->p = getmp(b, len);
    dss->q = getmp(b, len);
    dss->g = getmp(b, len);
    dss->y = getmp(b, len);
    dss->x = getmp(b, len);

    if (!dss->p || !dss->q || !dss->g || !dss->y || !dss->x) {
	sfree(dss->p);
	sfree(dss->q);
	sfree(dss->g);
	sfree(dss->y);
	sfree(dss->x);
	sfree(dss);
	return NULL;
    }

    return dss;
}

static int dss_openssh_fmtkey(void *key, unsigned char *blob, int len)
{
    struct dss_key *dss = (struct dss_key *) key;
    int bloblen, i;

    bloblen =
	ssh2_bignum_length(dss->p) +
	ssh2_bignum_length(dss->q) +
	ssh2_bignum_length(dss->g) +
	ssh2_bignum_length(dss->y) +
	ssh2_bignum_length(dss->x);

    if (bloblen > len)
	return bloblen;

    bloblen = 0;
#define ENC(x) \
    PUT_32BIT(blob+bloblen, ssh2_bignum_length((x))-4); bloblen += 4; \
    for (i = ssh2_bignum_length((x))-4; i-- ;) blob[bloblen++]=bignum_byte((x),i);
    ENC(dss->p);
    ENC(dss->q);
    ENC(dss->g);
    ENC(dss->y);
    ENC(dss->x);

    return bloblen;
}

static int dss_pubkey_bits(void *blob, int len)
{
    struct dss_key *dss;
    int ret;

    dss = dss_newkey((char *) blob, len);
    ret = bignum_bitcount(dss->p);
    dss_freekey(dss);

    return ret;
}

static unsigned char *dss_sign(void *key, char *data, int datalen, int *siglen)
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
    struct dss_key *dss = (struct dss_key *) key;
    SHA512_State ss;
    unsigned char digest[20], digest512[64];
    Bignum proto_k, k, gkp, hash, kinv, hxr, r, s;
    unsigned char *bytes;
    int nbytes, i;

    SHA_Simple(data, datalen, digest);

    /*
     * Hash some identifying text plus x.
     */
    SHA512_Init(&ss);
    SHA512_Bytes(&ss, "DSA deterministic k generator", 30);
    sha512_mpint(&ss, dss->x);
    SHA512_Final(&ss, digest512);

    /*
     * Now hash that digest plus the message hash.
     */
    SHA512_Init(&ss);
    SHA512_Bytes(&ss, digest512, sizeof(digest512));
    SHA512_Bytes(&ss, digest, sizeof(digest));
    SHA512_Final(&ss, digest512);

    memset(&ss, 0, sizeof(ss));

    /*
     * Now convert the result into a bignum, and reduce it mod q.
     */
    proto_k = bignum_from_bytes(digest512, 64);
    k = bigmod(proto_k, dss->q);
    freebn(proto_k);

    memset(digest512, 0, sizeof(digest512));

    /*
     * Now we have k, so just go ahead and compute the signature.
     */
    gkp = modpow(dss->g, k, dss->p);   /* g^k mod p */
    r = bigmod(gkp, dss->q);	       /* r = (g^k mod p) mod q */
    freebn(gkp);

    hash = bignum_from_bytes(digest, 20);
    kinv = modinv(k, dss->q);	       /* k^-1 mod q */
    hxr = bigmuladd(dss->x, r, hash);  /* hash + x*r */
    s = modmul(kinv, hxr, dss->q);     /* s = k^-1 * (hash + x*r) mod q */
    freebn(hxr);
    freebn(kinv);
    freebn(hash);

    /*
     * Signature blob is
     * 
     *   string  "ssh-dss"
     *   string  two 20-byte numbers r and s, end to end
     * 
     * i.e. 4+7 + 4+40 bytes.
     */
    nbytes = 4 + 7 + 4 + 40;
    bytes = snewn(nbytes, unsigned char);
    PUT_32BIT(bytes, 7);
    memcpy(bytes + 4, "ssh-dss", 7);
    PUT_32BIT(bytes + 4 + 7, 40);
    for (i = 0; i < 20; i++) {
	bytes[4 + 7 + 4 + i] = bignum_byte(r, 19 - i);
	bytes[4 + 7 + 4 + 20 + i] = bignum_byte(s, 19 - i);
    }
    freebn(r);
    freebn(s);

    *siglen = nbytes;
    return bytes;
}

const struct ssh_signkey ssh_dss = {
    dss_newkey,
    dss_freekey,
    dss_fmtkey,
    dss_public_blob,
    dss_private_blob,
    dss_createkey,
    dss_openssh_createkey,
    dss_openssh_fmtkey,
    dss_pubkey_bits,
    dss_fingerprint,
    dss_verifysig,
    dss_sign,
    "ssh-dss",
    "dss"
};
