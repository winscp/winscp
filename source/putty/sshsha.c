/*
 * SHA1 hash algorithm. Used in SSH-2 as a MAC, and the transform is
 * also used as a `stirring' function for the PuTTY random number
 * pool. Implemented directly from the specification by Simon
 * Tatham.
 */

#include "ssh.h"

#include <assert.h>

/* ----------------------------------------------------------------------
 * Core SHA algorithm: processes 16-word blocks into a message digest.
 */

#define rol(x,y) ( ((x) << (y)) | (((uint32)x) >> (32-y)) )

static void sha1_sw(SHA_State * s, const unsigned char *q, int len);
static void sha1_ni(SHA_State * s, const unsigned char *q, int len);

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

#ifdef RANDOM_DIAGNOSTICS
    {
        extern int random_diagnostics;
        if (random_diagnostics) {
            int i;
            printf("SHATransform:");
            for (i = 0; i < 5; i++)
                printf(" %08x", digest[i]);
            printf(" +");
            for (i = 0; i < 16; i++)
                printf(" %08x", block[i]);
        }
    }
#endif

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

#ifdef RANDOM_DIAGNOSTICS
    {
        extern int random_diagnostics;
        if (random_diagnostics) {
            int i;
            printf(" =");
            for (i = 0; i < 5; i++)
                printf(" %08x", digest[i]);
            printf("\n");
        }
    }
#endif
}

/* ----------------------------------------------------------------------
 * Outer SHA algorithm: take an arbitrary length byte string,
 * convert it into 16-word blocks with the prescribed padding at
 * the end, and pass those blocks to the core SHA algorithm.
 */

static void SHA_BinarySink_write(BinarySink *bs, const void *p, size_t len);

void SHA_Init(SHA_State * s)
{
    SHA_Core_Init(s->h);
    s->blkused = 0;
    s->lenhi = s->lenlo = 0;
    if (supports_sha_ni())
        s->sha1 = &sha1_ni;
    else
        s->sha1 = &sha1_sw;
    BinarySink_INIT(s, SHA_BinarySink_write);
}

static void SHA_BinarySink_write(BinarySink *bs, const void *p, size_t len)
{
    struct SHA_State *s = BinarySink_DOWNCAST(bs, struct SHA_State);
    const unsigned char *q = (const unsigned char *) p;
    uint32 lenw = len;
    assert(lenw == len);

    /*
     * Update the length field.
     */
    s->lenlo += lenw;
    s->lenhi += (s->lenlo < lenw);
    (*(s->sha1))(s, q, len);
}

static void sha1_sw(SHA_State * s, const unsigned char *q, int len)
{
    uint32 wordblock[16];
    int i;

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
    put_data(s, &c, pad);

    put_uint32(s, lenhi);
    put_uint32(s, lenlo);

    for (i = 0; i < 5; i++) {
	output[i * 4] = (s->h[i] >> 24) & 0xFF;
	output[i * 4 + 1] = (s->h[i] >> 16) & 0xFF;
	output[i * 4 + 2] = (s->h[i] >> 8) & 0xFF;
	output[i * 4 + 3] = (s->h[i]) & 0xFF;
    }
}

void SHA_Simple(const void *p, int len, unsigned char *output)
{
    SHA_State s;

    SHA_Init(&s);
    put_data(&s, p, len);
    SHA_Final(&s, output);
    smemclr(&s, sizeof(s));
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

static void *sha1_copy(const void *vold)
{
    const SHA_State *old = (const SHA_State *)vold;
    SHA_State *s;

    s = snew(SHA_State);
    *s = *old;
    BinarySink_COPIED(s);
    return s;
}

static void sha1_free(void *handle)
{
    SHA_State *s = handle;

    smemclr(s, sizeof(*s));
    sfree(s);
}

static BinarySink *sha1_sink(void *handle)
{
    SHA_State *s = handle;
    return BinarySink_UPCAST(s);
}

static void sha1_final(void *handle, unsigned char *output)
{
    SHA_State *s = handle;

    SHA_Final(s, output);
    sha1_free(s);
}

const struct ssh_hash ssh_sha1 = {
    sha1_init, sha1_copy, sha1_sink, sha1_final, sha1_free, 20, "SHA-1"
};

/* ----------------------------------------------------------------------
 * The above is the SHA-1 algorithm itself. Now we implement the
 * HMAC wrapper on it.
 */

static void *sha1_make_context(void *cipher_ctx)
{
    return snewn(3, SHA_State);
}

static void sha1_free_context(void *handle)
{
    smemclr(handle, 3 * sizeof(SHA_State));
    sfree(handle);
}

static void sha1_key_internal(void *handle, const unsigned char *key, int len)
{
    SHA_State *keys = (SHA_State *)handle;
    unsigned char foo[64];
    int i;

    memset(foo, 0x36, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    SHA_Init(&keys[0]);
    put_data(&keys[0], foo, 64);

    memset(foo, 0x5C, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    SHA_Init(&keys[1]);
    put_data(&keys[1], foo, 64);

    smemclr(foo, 64);		       /* burn the evidence */
}

static void sha1_key(void *handle, const void *key)
{
    sha1_key_internal(handle, key, 20);
}

static void sha1_key_buggy(void *handle, const void *key)
{
    sha1_key_internal(handle, key, 16);
}

static void hmacsha1_start(void *handle)
{
    SHA_State *keys = (SHA_State *)handle;

    keys[2] = keys[0];		      /* structure copy */
    BinarySink_COPIED(&keys[2]);
}

static BinarySink *hmacsha1_sink(void *handle)
{
    SHA_State *keys = (SHA_State *)handle;
    return BinarySink_UPCAST(&keys[2]);
}

static void hmacsha1_genresult(void *handle, unsigned char *hmac)
{
    SHA_State *keys = (SHA_State *)handle;
    SHA_State s;
    unsigned char intermediate[20];

    s = keys[2];		       /* structure copy */
    BinarySink_COPIED(&s);
    SHA_Final(&s, intermediate);
    s = keys[1];		       /* structure copy */
    BinarySink_COPIED(&s);
    put_data(&s, intermediate, 20);
    SHA_Final(&s, hmac);
}

static void sha1_do_hmac(void *handle, const unsigned char *blk, int len,
			 unsigned long seq, unsigned char *hmac)
{
    BinarySink *bs = hmacsha1_sink(handle);
    hmacsha1_start(handle);
    put_uint32(bs, seq);
    put_data(bs, blk, len);
    hmacsha1_genresult(handle, hmac);
}

static void sha1_generate(void *handle, void *vblk, int len,
			  unsigned long seq)
{
    unsigned char *blk = (unsigned char *)vblk;
    sha1_do_hmac(handle, blk, len, seq, blk + len);
}

static int hmacsha1_verresult(void *handle, unsigned char const *hmac)
{
    unsigned char correct[20];
    hmacsha1_genresult(handle, correct);
    return smemeq(correct, hmac, 20);
}

static int sha1_verify(void *handle, const void *vblk, int len,
		       unsigned long seq)
{
    const unsigned char *blk = (const unsigned char *)vblk;
    unsigned char correct[20];
    sha1_do_hmac(handle, blk, len, seq, correct);
    return smemeq(correct, blk + len, 20);
}

static void hmacsha1_96_genresult(void *handle, unsigned char *hmac)
{
    unsigned char full[20];
    hmacsha1_genresult(handle, full);
    memcpy(hmac, full, 12);
}

static void sha1_96_generate(void *handle, void *vblk, int len,
			     unsigned long seq)
{
    unsigned char *blk = (unsigned char *)vblk;
    unsigned char full[20];
    sha1_do_hmac(handle, blk, len, seq, full);
    memcpy(blk + len, full, 12);
}

static int hmacsha1_96_verresult(void *handle, unsigned char const *hmac)
{
    unsigned char correct[20];
    hmacsha1_genresult(handle, correct);
    return smemeq(correct, hmac, 12);
}

static int sha1_96_verify(void *handle, const void *vblk, int len,
		       unsigned long seq)
{
    const unsigned char *blk = (const unsigned char *)vblk;
    unsigned char correct[20];
    sha1_do_hmac(handle, blk, len, seq, correct);
    return smemeq(correct, blk + len, 12);
}

void hmac_sha1_simple(void *key, int keylen, void *data, int datalen,
		      unsigned char *output) {
    SHA_State states[2];
    unsigned char intermediate[20];

    sha1_key_internal(states, key, keylen);
    put_data(&states[0], data, datalen);
    SHA_Final(&states[0], intermediate);

    put_data(&states[1], intermediate, 20);
    SHA_Final(&states[1], output);
}

const struct ssh_mac ssh_hmac_sha1 = {
    sha1_make_context, sha1_free_context, sha1_key,
    sha1_generate, sha1_verify,
    hmacsha1_start, hmacsha1_sink, hmacsha1_genresult, hmacsha1_verresult,
    "hmac-sha1", "hmac-sha1-etm@openssh.com",
    20, 20,
    "HMAC-SHA1"
};

const struct ssh_mac ssh_hmac_sha1_96 = {
    sha1_make_context, sha1_free_context, sha1_key,
    sha1_96_generate, sha1_96_verify,
    hmacsha1_start, hmacsha1_sink,
    hmacsha1_96_genresult, hmacsha1_96_verresult,
    "hmac-sha1-96", "hmac-sha1-96-etm@openssh.com",
    12, 20,
    "HMAC-SHA1-96"
};

const struct ssh_mac ssh_hmac_sha1_buggy = {
    sha1_make_context, sha1_free_context, sha1_key_buggy,
    sha1_generate, sha1_verify,
    hmacsha1_start, hmacsha1_sink, hmacsha1_genresult, hmacsha1_verresult,
    "hmac-sha1", NULL,
    20, 16,
    "bug-compatible HMAC-SHA1"
};

const struct ssh_mac ssh_hmac_sha1_96_buggy = {
    sha1_make_context, sha1_free_context, sha1_key_buggy,
    sha1_96_generate, sha1_96_verify,
    hmacsha1_start, hmacsha1_sink,
    hmacsha1_96_genresult, hmacsha1_96_verresult,
    "hmac-sha1-96", NULL,
    12, 16,
    "bug-compatible HMAC-SHA1-96"
};

#ifdef COMPILER_SUPPORTS_SHA_NI

#if defined _MSC_VER && defined _M_AMD64
# include <intrin.h>
#endif

/*
 * Set target architecture for Clang and GCC
 */
#if !defined(__clang__) && defined(__GNUC__)
#    pragma GCC target("sha")
#    pragma GCC target("sse4.1")
#endif

#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ >= 5))
#    define FUNC_ISA __attribute__ ((target("sse4.1,sha")))
#else
#    define FUNC_ISA
#endif

#include <wmmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>

#if defined(__clang__) || defined(__GNUC__)
#include <shaintrin.h>
#endif

/*
 * Determinators of CPU type
 */
#if defined(__clang__) || defined(__GNUC__)

#include <cpuid.h>
int supports_sha_ni(void)
{
    unsigned int CPUInfo[4];
    __cpuid(0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
    if (CPUInfo[0] < 7)
        return 0;

    __cpuid_count(7, 0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
    return CPUInfo[1] & (1 << 29); /* SHA */
}

#else /* defined(__clang__) || defined(__GNUC__) */

int supports_sha_ni(void)
{
    unsigned int CPUInfo[4];
    __cpuid(CPUInfo, 0);  
    if (CPUInfo[0] < 7)
        return 0;

    __cpuidex(CPUInfo, 7, 0);
    return CPUInfo[1] & (1 << 29); /* Check SHA */
}

#endif /* defined(__clang__) || defined(__GNUC__) */

/* SHA1 implementation using new instructions
   The code is based on Jeffrey Walton's SHA1 implementation:
   https://github.com/noloader/SHA-Intrinsics
*/
FUNC_ISA
static void sha1_ni_(SHA_State * s, const unsigned char *q, int len)
{
    if (s->blkused && s->blkused + len < 64) {
      /*
       * Trivial case: just add to the block.
       */
       memcpy(s->block + s->blkused, q, len);
       s->blkused += len;
    } else {
        __m128i ABCD, ABCD_SAVE, E0, E0_SAVE, E1;
        const __m128i MASK = _mm_set_epi64x(0x0001020304050607ULL, 0x08090a0b0c0d0e0fULL);

        ABCD = _mm_loadu_si128((const __m128i*) s->h);
        E0 = _mm_set_epi32(s->h[4], 0, 0, 0);
        ABCD = _mm_shuffle_epi32(ABCD, 0x1B);

        /*
         * We must complete and process at least one block.
         */
        while (s->blkused + len >= 64)
        {
            __m128i MSG0, MSG1, MSG2, MSG3;
            memcpy(s->block + s->blkused, q, 64 - s->blkused);
            q += 64 - s->blkused;
            len -= 64 - s->blkused;

            /* Save current state  */
            ABCD_SAVE = ABCD;
            E0_SAVE = E0;

            /* Rounds 0-3 */
            MSG0 = _mm_loadu_si128((const __m128i*)(s->block + 0));
            MSG0 = _mm_shuffle_epi8(MSG0, MASK);
            E0 = _mm_add_epi32(E0, MSG0);
            E1 = ABCD;
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 0);

            /* Rounds 4-7 */
            MSG1 = _mm_loadu_si128((const __m128i*)(s->block + 16));
            MSG1 = _mm_shuffle_epi8(MSG1, MASK);
            E1 = _mm_sha1nexte_epu32(E1, MSG1);
            E0 = ABCD;
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 0);
            MSG0 = _mm_sha1msg1_epu32(MSG0, MSG1);

            /* Rounds 8-11 */
            MSG2 = _mm_loadu_si128((const __m128i*)(s->block + 32));
            MSG2 = _mm_shuffle_epi8(MSG2, MASK);
            E0 = _mm_sha1nexte_epu32(E0, MSG2);
            E1 = ABCD;
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 0);
            MSG1 = _mm_sha1msg1_epu32(MSG1, MSG2);
            MSG0 = _mm_xor_si128(MSG0, MSG2);

            /* Rounds 12-15 */
            MSG3 = _mm_loadu_si128((const __m128i*)(s->block + 48));
            MSG3 = _mm_shuffle_epi8(MSG3, MASK);
            E1 = _mm_sha1nexte_epu32(E1, MSG3);
            E0 = ABCD;
            MSG0 = _mm_sha1msg2_epu32(MSG0, MSG3);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 0);
            MSG2 = _mm_sha1msg1_epu32(MSG2, MSG3);
            MSG1 = _mm_xor_si128(MSG1, MSG3);

            /* Rounds 16-19 */
            E0 = _mm_sha1nexte_epu32(E0, MSG0);
            E1 = ABCD;
            MSG1 = _mm_sha1msg2_epu32(MSG1, MSG0);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 0);
            MSG3 = _mm_sha1msg1_epu32(MSG3, MSG0);
            MSG2 = _mm_xor_si128(MSG2, MSG0);

            /* Rounds 20-23 */
            E1 = _mm_sha1nexte_epu32(E1, MSG1);
            E0 = ABCD;
            MSG2 = _mm_sha1msg2_epu32(MSG2, MSG1);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 1);
            MSG0 = _mm_sha1msg1_epu32(MSG0, MSG1);
            MSG3 = _mm_xor_si128(MSG3, MSG1);

            /* Rounds 24-27 */
            E0 = _mm_sha1nexte_epu32(E0, MSG2);
            E1 = ABCD;
            MSG3 = _mm_sha1msg2_epu32(MSG3, MSG2);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 1);
            MSG1 = _mm_sha1msg1_epu32(MSG1, MSG2);
            MSG0 = _mm_xor_si128(MSG0, MSG2);

            /* Rounds 28-31 */
            E1 = _mm_sha1nexte_epu32(E1, MSG3);
            E0 = ABCD;
            MSG0 = _mm_sha1msg2_epu32(MSG0, MSG3);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 1);
            MSG2 = _mm_sha1msg1_epu32(MSG2, MSG3);
            MSG1 = _mm_xor_si128(MSG1, MSG3);

            /* Rounds 32-35 */
            E0 = _mm_sha1nexte_epu32(E0, MSG0);
            E1 = ABCD;
            MSG1 = _mm_sha1msg2_epu32(MSG1, MSG0);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 1);
            MSG3 = _mm_sha1msg1_epu32(MSG3, MSG0);
            MSG2 = _mm_xor_si128(MSG2, MSG0);

            /* Rounds 36-39 */
            E1 = _mm_sha1nexte_epu32(E1, MSG1);
            E0 = ABCD;
            MSG2 = _mm_sha1msg2_epu32(MSG2, MSG1);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 1);
            MSG0 = _mm_sha1msg1_epu32(MSG0, MSG1);
            MSG3 = _mm_xor_si128(MSG3, MSG1);

            /* Rounds 40-43 */
            E0 = _mm_sha1nexte_epu32(E0, MSG2);
            E1 = ABCD;
            MSG3 = _mm_sha1msg2_epu32(MSG3, MSG2);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 2);
            MSG1 = _mm_sha1msg1_epu32(MSG1, MSG2);
            MSG0 = _mm_xor_si128(MSG0, MSG2);

            /* Rounds 44-47 */
            E1 = _mm_sha1nexte_epu32(E1, MSG3);
            E0 = ABCD;
            MSG0 = _mm_sha1msg2_epu32(MSG0, MSG3);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 2);
            MSG2 = _mm_sha1msg1_epu32(MSG2, MSG3);
            MSG1 = _mm_xor_si128(MSG1, MSG3);

            /* Rounds 48-51 */
            E0 = _mm_sha1nexte_epu32(E0, MSG0);
            E1 = ABCD;
            MSG1 = _mm_sha1msg2_epu32(MSG1, MSG0);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 2);
            MSG3 = _mm_sha1msg1_epu32(MSG3, MSG0);
            MSG2 = _mm_xor_si128(MSG2, MSG0);

            /* Rounds 52-55 */
            E1 = _mm_sha1nexte_epu32(E1, MSG1);
            E0 = ABCD;
            MSG2 = _mm_sha1msg2_epu32(MSG2, MSG1);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 2);
            MSG0 = _mm_sha1msg1_epu32(MSG0, MSG1);
            MSG3 = _mm_xor_si128(MSG3, MSG1);

            /* Rounds 56-59 */
            E0 = _mm_sha1nexte_epu32(E0, MSG2);
            E1 = ABCD;
            MSG3 = _mm_sha1msg2_epu32(MSG3, MSG2);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 2);
            MSG1 = _mm_sha1msg1_epu32(MSG1, MSG2);
            MSG0 = _mm_xor_si128(MSG0, MSG2);

            /* Rounds 60-63 */
            E1 = _mm_sha1nexte_epu32(E1, MSG3);
            E0 = ABCD;
            MSG0 = _mm_sha1msg2_epu32(MSG0, MSG3);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 3);
            MSG2 = _mm_sha1msg1_epu32(MSG2, MSG3);
            MSG1 = _mm_xor_si128(MSG1, MSG3);

            /* Rounds 64-67 */
            E0 = _mm_sha1nexte_epu32(E0, MSG0);
            E1 = ABCD;
            MSG1 = _mm_sha1msg2_epu32(MSG1, MSG0);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 3);
            MSG3 = _mm_sha1msg1_epu32(MSG3, MSG0);
            MSG2 = _mm_xor_si128(MSG2, MSG0);

            /* Rounds 68-71 */
            E1 = _mm_sha1nexte_epu32(E1, MSG1);
            E0 = ABCD;
            MSG2 = _mm_sha1msg2_epu32(MSG2, MSG1);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 3);
            MSG3 = _mm_xor_si128(MSG3, MSG1);

            /* Rounds 72-75 */
            E0 = _mm_sha1nexte_epu32(E0, MSG2);
            E1 = ABCD;
            MSG3 = _mm_sha1msg2_epu32(MSG3, MSG2);
            ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 3);

            /* Rounds 76-79 */
            E1 = _mm_sha1nexte_epu32(E1, MSG3);
            E0 = ABCD;
            ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 3);

            /* Combine state */
            E0 = _mm_sha1nexte_epu32(E0, E0_SAVE);
            ABCD = _mm_add_epi32(ABCD, ABCD_SAVE);

            s->blkused = 0;
        }

        ABCD = _mm_shuffle_epi32(ABCD, 0x1B);

        /* Save state */
        _mm_storeu_si128((__m128i*) s->h, ABCD);
        s->h[4] = _mm_extract_epi32(E0, 3);

        memcpy(s->block, q, len);
        s->blkused = len;
    }
}

/*
 * Workaround LLVM bug https://bugs.llvm.org/show_bug.cgi?id=34980
 */
static void sha1_ni(SHA_State * s, const unsigned char *q, int len)
{
    sha1_ni_(s, q, len);
}

#else /* COMPILER_SUPPORTS_AES_NI */

static void sha1_ni(SHA_State * s, const unsigned char *q, int len)
{
    assert(0);
}

int supports_sha_ni(void)
{
    return 0;
}

#endif  /* COMPILER_SUPPORTS_AES_NI */
