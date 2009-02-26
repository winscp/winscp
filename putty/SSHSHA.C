/*
 * SHA1 hash algorithm. Used in SSH-2 as a MAC, and the transform is
 * also used as a `stirring' function for the PuTTY random number
 * pool. Implemented directly from the specification by Simon
 * Tatham.
 */

#include "ssh.h"

/* ----------------------------------------------------------------------
 * Core SHA algorithm: processes 16-word blocks into a message digest.
 */

#define rol(x,y) ( ((x) << (y)) | (((uint32)x) >> (32-y)) )

static void SHA_Core_Init(uint32 h[5])
{
    h[0] = 0x67452301;
    h[1] = 0xefcdab89;
    h[2] = 0x98badcfe;
    h[3] = 0x10325476;
    h[4] = 0xc3d2e1f0;
}

void SHATransform(word32 * digest, word32 * block)
{
    word32 w[80];
    word32 a, b, c, d, e;
    int t;

    for (t = 0; t < 16; t++)
	w[t] = block[t];

    for (t = 16; t < 80; t++) {
	word32 tmp = w[t - 3] ^ w[t - 8] ^ w[t - 14] ^ w[t - 16];
	w[t] = rol(tmp, 1);
    }

    a = digest[0];
    b = digest[1];
    c = digest[2];
    d = digest[3];
    e = digest[4];

    for (t = 0; t < 20; t++) {
	word32 tmp =
	    rol(a, 5) + ((b & c) | (d & ~b)) + e + w[t] + 0x5a827999;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 20; t < 40; t++) {
	word32 tmp = rol(a, 5) + (b ^ c ^ d) + e + w[t] + 0x6ed9eba1;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 40; t < 60; t++) {
	word32 tmp = rol(a,
			 5) + ((b & c) | (b & d) | (c & d)) + e + w[t] +
	    0x8f1bbcdc;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 60; t < 80; t++) {
	word32 tmp = rol(a, 5) + (b ^ c ^ d) + e + w[t] + 0xca62c1d6;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }

    digest[0] += a;
    digest[1] += b;
    digest[2] += c;
    digest[3] += d;
    digest[4] += e;
}

/* ----------------------------------------------------------------------
 * Outer SHA algorithm: take an arbitrary length byte string,
 * convert it into 16-word blocks with the prescribed padding at
 * the end, and pass those blocks to the core SHA algorithm.
 */

void SHA_Init(SHA_State * s)
{
    SHA_Core_Init(s->h);
    s->blkused = 0;
    s->lenhi = s->lenlo = 0;
}

void SHA_Bytes(SHA_State * s, void *p, int len)
{
    unsigned char *q = (unsigned char *) p;
    uint32 wordblock[16];
    uint32 lenw = len;
    int i;

    /*
     * Update the length field.
     */
    s->lenlo += lenw;
    s->lenhi += (s->lenlo < lenw);

    if (s->blkused && s->blkused + len < 64) {
	/*
	 * Trivial case: just add to the block.
	 */
	memcpy(s->block + s->blkused, q, len);
	s->blkused += len;
    } else {
	/*
	 * We must complete and process at least one block.
	 */
	while (s->blkused + len >= 64) {
	    memcpy(s->block + s->blkused, q, 64 - s->blkused);
	    q += 64 - s->blkused;
	    len -= 64 - s->blkused;
	    /* Now process the block. Gather bytes big-endian into words */
	    for (i = 0; i < 16; i++) {
		wordblock[i] =
		    (((uint32) s->block[i * 4 + 0]) << 24) |
		    (((uint32) s->block[i * 4 + 1]) << 16) |
		    (((uint32) s->block[i * 4 + 2]) << 8) |
		    (((uint32) s->block[i * 4 + 3]) << 0);
	    }
	    SHATransform(s->h, wordblock);
	    s->blkused = 0;
	}
	memcpy(s->block, q, len);
	s->blkused = len;
    }
}

void SHA_Final(SHA_State * s, unsigned char *output)
{
    int i;
    int pad;
    unsigned char c[64];
    uint32 lenhi, lenlo;

    if (s->blkused >= 56)
	pad = 56 + 64 - s->blkused;
    else
	pad = 56 - s->blkused;

    lenhi = (s->lenhi << 3) | (s->lenlo >> (32 - 3));
    lenlo = (s->lenlo << 3);

    memset(c, 0, pad);
    c[0] = 0x80;
    SHA_Bytes(s, &c, pad);

    c[0] = (lenhi >> 24) & 0xFF;
    c[1] = (lenhi >> 16) & 0xFF;
    c[2] = (lenhi >> 8) & 0xFF;
    c[3] = (lenhi >> 0) & 0xFF;
    c[4] = (lenlo >> 24) & 0xFF;
    c[5] = (lenlo >> 16) & 0xFF;
    c[6] = (lenlo >> 8) & 0xFF;
    c[7] = (lenlo >> 0) & 0xFF;

    SHA_Bytes(s, &c, 8);

    for (i = 0; i < 5; i++) {
	output[i * 4] = (s->h[i] >> 24) & 0xFF;
	output[i * 4 + 1] = (s->h[i] >> 16) & 0xFF;
	output[i * 4 + 2] = (s->h[i] >> 8) & 0xFF;
	output[i * 4 + 3] = (s->h[i]) & 0xFF;
    }
}

void SHA_Simple(void *p, int len, unsigned char *output)
{
    SHA_State s;

    SHA_Init(&s);
    SHA_Bytes(&s, p, len);
    SHA_Final(&s, output);
}

/*
 * Thin abstraction for things where hashes are pluggable.
 */

static void *sha1_init(void)
{
    SHA_State *s;

    s = snew(SHA_State);
    SHA_Init(s);
    return s;
}

static void sha1_bytes(void *handle, void *p, int len)
{
    SHA_State *s = handle;

    SHA_Bytes(s, p, len);
}

static void sha1_final(void *handle, unsigned char *output)
{
    SHA_State *s = handle;

    SHA_Final(s, output);
    sfree(s);
}

const struct ssh_hash ssh_sha1 = {
    sha1_init, sha1_bytes, sha1_final, 20, "SHA-1"
};

/* ----------------------------------------------------------------------
 * The above is the SHA-1 algorithm itself. Now we implement the
 * HMAC wrapper on it.
 */

static void *sha1_make_context(void)
{
    return snewn(3, SHA_State);
}

static void sha1_free_context(void *handle)
{
    sfree(handle);
}

static void sha1_key_internal(void *handle, unsigned char *key, int len)
{
    SHA_State *keys = (SHA_State *)handle;
    unsigned char foo[64];
    int i;

    memset(foo, 0x36, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    SHA_Init(&keys[0]);
    SHA_Bytes(&keys[0], foo, 64);

    memset(foo, 0x5C, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    SHA_Init(&keys[1]);
    SHA_Bytes(&keys[1], foo, 64);

    memset(foo, 0, 64);		       /* burn the evidence */
}

static void sha1_key(void *handle, unsigned char *key)
{
    sha1_key_internal(handle, key, 20);
}

static void sha1_key_buggy(void *handle, unsigned char *key)
{
    sha1_key_internal(handle, key, 16);
}

static void hmacsha1_start(void *handle)
{
    SHA_State *keys = (SHA_State *)handle;

    keys[2] = keys[0];		      /* structure copy */
}

static void hmacsha1_bytes(void *handle, unsigned char const *blk, int len)
{
    SHA_State *keys = (SHA_State *)handle;
    SHA_Bytes(&keys[2], (void *)blk, len);
}

static void hmacsha1_genresult(void *handle, unsigned char *hmac)
{
    SHA_State *keys = (SHA_State *)handle;
    SHA_State s;
    unsigned char intermediate[20];

    s = keys[2];		       /* structure copy */
    SHA_Final(&s, intermediate);
    s = keys[1];		       /* structure copy */
    SHA_Bytes(&s, intermediate, 20);
    SHA_Final(&s, hmac);
}

static void sha1_do_hmac(void *handle, unsigned char *blk, int len,
			 unsigned long seq, unsigned char *hmac)
{
    unsigned char seqbuf[4];

    seqbuf[0] = (unsigned char) ((seq >> 24) & 0xFF);
    seqbuf[1] = (unsigned char) ((seq >> 16) & 0xFF);
    seqbuf[2] = (unsigned char) ((seq >> 8) & 0xFF);
    seqbuf[3] = (unsigned char) ((seq) & 0xFF);

    hmacsha1_start(handle);
    hmacsha1_bytes(handle, seqbuf, 4);
    hmacsha1_bytes(handle, blk, len);
    hmacsha1_genresult(handle, hmac);
}

static void sha1_generate(void *handle, unsigned char *blk, int len,
			  unsigned long seq)
{
    sha1_do_hmac(handle, blk, len, seq, blk + len);
}

static int hmacsha1_verresult(void *handle, unsigned char const *hmac)
{
    unsigned char correct[20];
    hmacsha1_genresult(handle, correct);
    return !memcmp(correct, hmac, 20);
}

static int sha1_verify(void *handle, unsigned char *blk, int len,
		       unsigned long seq)
{
    unsigned char correct[20];
    sha1_do_hmac(handle, blk, len, seq, correct);
    return !memcmp(correct, blk + len, 20);
}

static void hmacsha1_96_genresult(void *handle, unsigned char *hmac)
{
    unsigned char full[20];
    hmacsha1_genresult(handle, full);
    memcpy(hmac, full, 12);
}

static void sha1_96_generate(void *handle, unsigned char *blk, int len,
			     unsigned long seq)
{
    unsigned char full[20];
    sha1_do_hmac(handle, blk, len, seq, full);
    memcpy(blk + len, full, 12);
}

static int hmacsha1_96_verresult(void *handle, unsigned char const *hmac)
{
    unsigned char correct[20];
    hmacsha1_genresult(handle, correct);
    return !memcmp(correct, hmac, 12);
}

static int sha1_96_verify(void *handle, unsigned char *blk, int len,
		       unsigned long seq)
{
    unsigned char correct[20];
    sha1_do_hmac(handle, blk, len, seq, correct);
    return !memcmp(correct, blk + len, 12);
}

void hmac_sha1_simple(void *key, int keylen, void *data, int datalen,
		      unsigned char *output) {
    SHA_State states[2];
    unsigned char intermediate[20];

    sha1_key_internal(states, key, keylen);
    SHA_Bytes(&states[0], data, datalen);
    SHA_Final(&states[0], intermediate);

    SHA_Bytes(&states[1], intermediate, 20);
    SHA_Final(&states[1], output);
}

const struct ssh_mac ssh_hmac_sha1 = {
    sha1_make_context, sha1_free_context, sha1_key,
    sha1_generate, sha1_verify,
    hmacsha1_start, hmacsha1_bytes, hmacsha1_genresult, hmacsha1_verresult,
    "hmac-sha1",
    20,
    "HMAC-SHA1"
};

const struct ssh_mac ssh_hmac_sha1_96 = {
    sha1_make_context, sha1_free_context, sha1_key,
    sha1_96_generate, sha1_96_verify,
    hmacsha1_start, hmacsha1_bytes,
    hmacsha1_96_genresult, hmacsha1_96_verresult,
    "hmac-sha1-96",
    12,
    "HMAC-SHA1-96"
};

const struct ssh_mac ssh_hmac_sha1_buggy = {
    sha1_make_context, sha1_free_context, sha1_key_buggy,
    sha1_generate, sha1_verify,
    hmacsha1_start, hmacsha1_bytes, hmacsha1_genresult, hmacsha1_verresult,
    "hmac-sha1",
    20,
    "bug-compatible HMAC-SHA1"
};

const struct ssh_mac ssh_hmac_sha1_96_buggy = {
    sha1_make_context, sha1_free_context, sha1_key_buggy,
    sha1_96_generate, sha1_96_verify,
    hmacsha1_start, hmacsha1_bytes,
    hmacsha1_96_genresult, hmacsha1_96_verresult,
    "hmac-sha1-96",
    12,
    "bug-compatible HMAC-SHA1-96"
};
