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

#define rol(x,y) ( ((x) << (y)) | (((uint32_t)x) >> (32-y)) )

static void sha1_sw(SHA_State * s, const unsigned char *q, int len);
static void sha1_ni(SHA_State * s, const unsigned char *q, int len);

static void SHA_Core_Init(uint32_t h[5])
{
    h[0] = 0x67452301;
    h[1] = 0xefcdab89;
    h[2] = 0x98badcfe;
    h[3] = 0x10325476;
    h[4] = 0xc3d2e1f0;
}

void SHATransform(uint32_t * digest, uint32_t * block)
{
    uint32_t w[80];
    uint32_t a, b, c, d, e;
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
	uint32_t tmp = w[t - 3] ^ w[t - 8] ^ w[t - 14] ^ w[t - 16];
	w[t] = rol(tmp, 1);
    }

    a = digest[0];
    b = digest[1];
    c = digest[2];
    d = digest[3];
    e = digest[4];

    for (t = 0; t < 20; t++) {
	uint32_t tmp =
	    rol(a, 5) + ((b & c) | (d & ~b)) + e + w[t] + 0x5a827999;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 20; t < 40; t++) {
	uint32_t tmp = rol(a, 5) + (b ^ c ^ d) + e + w[t] + 0x6ed9eba1;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 40; t < 60; t++) {
	uint32_t tmp = rol(a,
			 5) + ((b & c) | (b & d) | (c & d)) + e + w[t] +
	    0x8f1bbcdc;
	e = d;
	d = c;
	c = rol(b, 30);
	b = a;
	a = tmp;
    }
    for (t = 60; t < 80; t++) {
	uint32_t tmp = rol(a, 5) + (b ^ c ^ d) + e + w[t] + 0xca62c1d6;
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
    s->len = 0;
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

    /*
     * Update the length field.
     */
    s->len += len;

    (*(s->sha1))(s, q, len);
}

static void sha1_sw(SHA_State * s, const unsigned char *q, int len)
{
    uint32_t wordblock[16];
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
		    (((uint32_t) s->block[i * 4 + 0]) << 24) |
		    (((uint32_t) s->block[i * 4 + 1]) << 16) |
		    (((uint32_t) s->block[i * 4 + 2]) << 8) |
		    (((uint32_t) s->block[i * 4 + 3]) << 0);
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
    uint64_t len;

    if (s->blkused >= 56)
	pad = 56 + 64 - s->blkused;
    else
	pad = 56 - s->blkused;

    len = (s->len << 3);

    memset(c, 0, pad);
    c[0] = 0x80;
    put_data(s, &c, pad);

    put_uint64(s, len);

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

struct sha1_hash {
    SHA_State state;
    ssh_hash hash;
};

static ssh_hash *sha1_new(const ssh_hashalg *alg)
{
    struct sha1_hash *h = snew(struct sha1_hash);
    SHA_Init(&h->state);
    h->hash.vt = alg;
    BinarySink_DELEGATE_INIT(&h->hash, &h->state);
    return &h->hash;
}

static ssh_hash *sha1_copy(ssh_hash *hashold)
{
    struct sha1_hash *hold, *hnew;
    ssh_hash *hashnew = sha1_new(hashold->vt);

    hold = container_of(hashold, struct sha1_hash, hash);
    hnew = container_of(hashnew, struct sha1_hash, hash);

    hnew->state = hold->state;
    BinarySink_COPIED(&hnew->state);

    return hashnew;
}

static void sha1_free(ssh_hash *hash)
{
    struct sha1_hash *h = container_of(hash, struct sha1_hash, hash);

    smemclr(h, sizeof(*h));
    sfree(h);
}

static void sha1_final(ssh_hash *hash, unsigned char *output)
{
    struct sha1_hash *h = container_of(hash, struct sha1_hash, hash);
    SHA_Final(&h->state, output);
    sha1_free(hash);
}

const ssh_hashalg ssh_sha1 = {
    sha1_new, sha1_copy, sha1_final, sha1_free, 20, "SHA-1"
};

/* ----------------------------------------------------------------------
 * The above is the SHA-1 algorithm itself. Now we implement the
 * HMAC wrapper on it.
 */

struct hmacsha1 {
    SHA_State sha[3];
    ssh2_mac mac;
};

static ssh2_mac *hmacsha1_new(
    const ssh2_macalg *alg, ssh2_cipher *cipher)
{
    struct hmacsha1 *ctx = snew(struct hmacsha1);
    ctx->mac.vt = alg;
    BinarySink_DELEGATE_INIT(&ctx->mac, &ctx->sha[2]);
    return &ctx->mac;
}

static void hmacsha1_free(ssh2_mac *mac)
{
    struct hmacsha1 *ctx = container_of(mac, struct hmacsha1, mac);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void sha1_key_internal(SHA_State *keys,
                              const unsigned char *key, int len)
{
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

static void hmacsha1_key(ssh2_mac *mac, ptrlen key)
{
    struct hmacsha1 *ctx = container_of(mac, struct hmacsha1, mac);
    sha1_key_internal(ctx->sha, key.ptr, key.len);
}

static void hmacsha1_start(ssh2_mac *mac)
{
    struct hmacsha1 *ctx = container_of(mac, struct hmacsha1, mac);

    ctx->sha[2] = ctx->sha[0];         /* structure copy */
    BinarySink_COPIED(&ctx->sha[2]);
}

static void hmacsha1_genresult(ssh2_mac *mac, unsigned char *hmac)
{
    struct hmacsha1 *ctx = container_of(mac, struct hmacsha1, mac);
    SHA_State s;
    unsigned char intermediate[20];

    s = ctx->sha[2];                   /* structure copy */
    BinarySink_COPIED(&s);
    SHA_Final(&s, intermediate);
    s = ctx->sha[1];                   /* structure copy */
    BinarySink_COPIED(&s);
    put_data(&s, intermediate, 20);
    SHA_Final(&s, intermediate);
    memcpy(hmac, intermediate, ctx->mac.vt->len);
    smemclr(intermediate, sizeof(intermediate));
}

void hmac_sha1_simple(const void *key, int keylen,
                      const void *data, int datalen,
		      unsigned char *output) {
    SHA_State states[2];
    unsigned char intermediate[20];

    sha1_key_internal(states, key, keylen);
    put_data(&states[0], data, datalen);
    SHA_Final(&states[0], intermediate);

    put_data(&states[1], intermediate, 20);
    SHA_Final(&states[1], output);
}

const ssh2_macalg ssh_hmac_sha1 = {
    hmacsha1_new, hmacsha1_free, hmacsha1_key,
    hmacsha1_start, hmacsha1_genresult,
    "hmac-sha1", "hmac-sha1-etm@openssh.com",
    20, 20,
    "HMAC-SHA1"
};

const ssh2_macalg ssh_hmac_sha1_96 = {
    hmacsha1_new, hmacsha1_free, hmacsha1_key,
    hmacsha1_start, hmacsha1_genresult,
    "hmac-sha1-96", "hmac-sha1-96-etm@openssh.com",
    12, 20,
    "HMAC-SHA1-96"
};

const ssh2_macalg ssh_hmac_sha1_buggy = {
    hmacsha1_new, hmacsha1_free, hmacsha1_key,
    hmacsha1_start, hmacsha1_genresult,
    "hmac-sha1", NULL,
    20, 16,
    "bug-compatible HMAC-SHA1"
};

const ssh2_macalg ssh_hmac_sha1_96_buggy = {
    hmacsha1_new, hmacsha1_free, hmacsha1_key,
    hmacsha1_start, hmacsha1_genresult,
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
bool supports_sha_ni(void)
{
    unsigned int CPUInfo[4];
    __cpuid(0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
    if (CPUInfo[0] < 7)
        return false;

    __cpuid_count(7, 0, CPUInfo[0], CPUInfo[1], CPUInfo[2], CPUInfo[3]);
    return CPUInfo[1] & (1 << 29); /* SHA */
}

#else /* defined(__clang__) || defined(__GNUC__) */

bool supports_sha_ni(void)
{
    unsigned int CPUInfo[4];
    __cpuid(CPUInfo, 0);  
    if (CPUInfo[0] < 7)
        return false;

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
    unreachable("sha1_ni not compiled in");
}

bool supports_sha_ni(void)
{
    return false;
}

#endif  /* COMPILER_SUPPORTS_AES_NI */
