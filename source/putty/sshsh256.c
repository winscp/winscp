/*
 * SHA-256 algorithm as described at
 * 
 *   http://csrc.nist.gov/cryptval/shs.html
 */

#include "ssh.h"
#include <assert.h>

/*
 * Start by deciding whether we can support hardware SHA at all.
 */
#define HW_SHA256_NONE 0
#define HW_SHA256_NI 1
#define HW_SHA256_NEON 2

#ifdef _FORCE_SHA_NI
#   define HW_SHA256 HW_SHA256_NI
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<wmmintrin.h>) &&       \
    (defined(__x86_64__) || defined(__i386))
#       define HW_SHA256 HW_SHA256_NI
#   endif
#elif defined(__GNUC__)
#    if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)) && \
        (defined(__x86_64__) || defined(__i386))
#       define HW_SHA256 HW_SHA256_NI
#    endif
#elif defined (_MSC_VER)
#   if (defined(_M_X64) || defined(_M_IX86)) && _MSC_FULL_VER >= 150030729
#      define HW_SHA256 HW_SHA256_NI
#   endif
#endif

#ifdef _FORCE_SHA_NEON
#   define HW_SHA256 HW_SHA256_NEON
#elif defined __BYTE_ORDER__ && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    /* Arm can potentially support both endiannesses, but this code
     * hasn't been tested on anything but little. If anyone wants to
     * run big-endian, they'll need to fix it first. */
#elif defined __ARM_FEATURE_CRYPTO
    /* If the Arm crypto extension is available already, we can
     * support NEON SHA without having to enable anything by hand */
#   define HW_SHA256 HW_SHA256_NEON
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<arm_neon.h>) &&       \
    (defined(__aarch64__))
        /* clang can enable the crypto extension in AArch64 using
         * __attribute__((target)) */
#       define HW_SHA256 HW_SHA256_NEON
#       define USE_CLANG_ATTR_TARGET_AARCH64
#   endif
#elif defined _MSC_VER
    /* Visual Studio supports the crypto extension when targeting
     * AArch64, but as of VS2017, the AArch32 header doesn't quite
     * manage it (declaring the shae/shad intrinsics without a round
     * key operand). */
#   if defined _M_ARM64
#       define HW_SHA256 HW_SHA256_NEON
#       if defined _M_ARM64
#           define USE_ARM64_NEON_H /* unusual header name in this case */
#       endif
#   endif
#endif

#if defined _FORCE_SOFTWARE_SHA || !defined HW_SHA256
#   undef HW_SHA256
#   define HW_SHA256 HW_SHA256_NONE
#endif

/*
 * The actual query function that asks if hardware acceleration is
 * available.
 */
static bool sha256_hw_available(void);

/*
 * The top-level selection function, caching the results of
 * sha256_hw_available() so it only has to run once.
 */
static bool sha256_hw_available_cached(void)
{
    static bool initialised = false;
    static bool hw_available;
    if (!initialised) {
        hw_available = sha256_hw_available();
        initialised = true;
    }
    return hw_available;
}

static ssh_hash *sha256_select(const ssh_hashalg *alg)
{
    const ssh_hashalg *real_alg =
        sha256_hw_available_cached() ? &ssh_sha256_hw : &ssh_sha256_sw;

    return ssh_hash_new(real_alg);
}

const ssh_hashalg ssh_sha256 = {
    sha256_select, NULL, NULL, NULL,
    32, 64, HASHALG_NAMES_ANNOTATED("SHA-256", "dummy selector vtable"),
};

/* ----------------------------------------------------------------------
 * Definitions likely to be helpful to multiple implementations.
 */

static const uint32_t sha256_initial_state[] = {
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
};

static const uint32_t sha256_round_constants[] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
};

#define SHA256_ROUNDS 64

typedef struct sha256_block sha256_block;
struct sha256_block {
    uint8_t block[64];
    size_t used;
    uint64_t len;
};

static inline void sha256_block_setup(sha256_block *blk)
{
    blk->used = 0;
    blk->len = 0;
}

static inline bool sha256_block_write(
    sha256_block *blk, const void **vdata, size_t *len)
{
    size_t blkleft = sizeof(blk->block) - blk->used;
    size_t chunk = *len < blkleft ? *len : blkleft;

    const uint8_t *p = *vdata;
    memcpy(blk->block + blk->used, p, chunk);
    *vdata = p + chunk;
    *len -= chunk;
    blk->used += chunk;
    blk->len += chunk;

    if (blk->used == sizeof(blk->block)) {
        blk->used = 0;
        return true;
    }

    return false;
}

static inline void sha256_block_pad(sha256_block *blk, BinarySink *bs)
{
    uint64_t final_len = blk->len << 3;
    size_t pad = 1 + (63 & (55 - blk->used));

    put_byte(bs, 0x80);
    for (size_t i = 1; i < pad; i++)
        put_byte(bs, 0);
    put_uint64(bs, final_len);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}

/* ----------------------------------------------------------------------
 * Software implementation of SHA-256.
 */

static inline uint32_t ror(uint32_t x, unsigned y)
{
    return (x << (31 & -y)) | (x >> (31 & y));
}

static inline uint32_t Ch(uint32_t ctrl, uint32_t if1, uint32_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint32_t Maj(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint32_t Sigma_0(uint32_t x)
{
    return ror(x,2) ^ ror(x,13) ^ ror(x,22);
}

static inline uint32_t Sigma_1(uint32_t x)
{
    return ror(x,6) ^ ror(x,11) ^ ror(x,25);
}

static inline uint32_t sigma_0(uint32_t x)
{
    return ror(x,7) ^ ror(x,18) ^ (x >> 3);
}

static inline uint32_t sigma_1(uint32_t x)
{
    return ror(x,17) ^ ror(x,19) ^ (x >> 10);
}

static inline void sha256_sw_round(
    unsigned round_index, const uint32_t *schedule,
    uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d,
    uint32_t *e, uint32_t *f, uint32_t *g, uint32_t *h)
{
    uint32_t t1 = *h + Sigma_1(*e) + Ch(*e,*f,*g) +
        sha256_round_constants[round_index] + schedule[round_index];

    uint32_t t2 = Sigma_0(*a) + Maj(*a,*b,*c);

    *d += t1;
    *h = t1 + t2;
}

static void sha256_sw_block(uint32_t *core, const uint8_t *block)
{
    uint32_t w[SHA256_ROUNDS];
    uint32_t a,b,c,d,e,f,g,h;

    for (size_t t = 0; t < 16; t++)
        w[t] = GET_32BIT_MSB_FIRST(block + 4*t);

    for (size_t t = 16; t < SHA256_ROUNDS; t++)
	w[t] = sigma_1(w[t-2]) + w[t-7] + sigma_0(w[t-15]) + w[t-16];

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4]; f = core[5]; g = core[6]; h = core[7];

    for (size_t t = 0; t < SHA256_ROUNDS; t += 8) {
	sha256_sw_round(t+0, w, &a,&b,&c,&d,&e,&f,&g,&h);
	sha256_sw_round(t+1, w, &h,&a,&b,&c,&d,&e,&f,&g);
	sha256_sw_round(t+2, w, &g,&h,&a,&b,&c,&d,&e,&f);
	sha256_sw_round(t+3, w, &f,&g,&h,&a,&b,&c,&d,&e);
	sha256_sw_round(t+4, w, &e,&f,&g,&h,&a,&b,&c,&d);
	sha256_sw_round(t+5, w, &d,&e,&f,&g,&h,&a,&b,&c);
	sha256_sw_round(t+6, w, &c,&d,&e,&f,&g,&h,&a,&b);
	sha256_sw_round(t+7, w, &b,&c,&d,&e,&f,&g,&h,&a);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d;
    core[4] += e; core[5] += f; core[6] += g; core[7] += h;

    smemclr(w, sizeof(w));
}

typedef struct sha256_sw {
    uint32_t core[8];
    sha256_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha256_sw;

static void sha256_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha256_sw_new(const ssh_hashalg *alg)
{
    sha256_sw *s = snew(sha256_sw);

    memcpy(s->core, sha256_initial_state, sizeof(s->core));

    sha256_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha256_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha256_sw_copy(ssh_hash *hash)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);
    sha256_sw *copy = snew(sha256_sw);

    memcpy(copy, s, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha256_sw_free(ssh_hash *hash)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha256_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha256_sw *s = BinarySink_DOWNCAST(bs, sha256_sw);

    while (len > 0)
        if (sha256_block_write(&s->blk, &vp, &len))
            sha256_sw_block(s->core, s->blk.block);
}

static void sha256_sw_final(ssh_hash *hash, uint8_t *digest)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);

    sha256_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < 8; i++)
        PUT_32BIT_MSB_FIRST(digest + 4*i, s->core[i]);
    sha256_sw_free(hash);
}

const ssh_hashalg ssh_sha256_sw = {
    sha256_sw_new, sha256_sw_copy, sha256_sw_final, sha256_sw_free,
    32, 64, HASHALG_NAMES_ANNOTATED("SHA-256", "unaccelerated"),
};

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-256 using x86 SHA-NI.
 */

#if HW_SHA256 == HW_SHA256_NI

/*
 * Set target architecture for Clang and GCC
 */
#if defined(__clang__) || defined(__GNUC__)
#    define FUNC_ISA __attribute__ ((target("sse4.1,sha")))
#if !defined(__clang__)
#    pragma GCC target("sha")
#    pragma GCC target("sse4.1")
#endif
#else
#    define FUNC_ISA
#endif

#include <wmmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>
#if defined(__clang__) || defined(__GNUC__)
#include <shaintrin.h>
#endif

#if defined(__clang__) || defined(__GNUC__)
#include <cpuid.h>
#define GET_CPU_ID_0(out)                               \
    __cpuid(0, (out)[0], (out)[1], (out)[2], (out)[3])
#define GET_CPU_ID_7(out)                                       \
    __cpuid_count(7, 0, (out)[0], (out)[1], (out)[2], (out)[3])
#else
#define GET_CPU_ID_0(out) __cpuid(out, 0)
#define GET_CPU_ID_7(out) __cpuidex(out, 7, 0)
#endif

static bool sha256_hw_available(void)
{
    unsigned int CPUInfo[4];
    GET_CPU_ID_0(CPUInfo);  
    if (CPUInfo[0] < 7)
        return false;

    GET_CPU_ID_7(CPUInfo);
    return CPUInfo[1] & (1 << 29); /* Check SHA */
}

/* SHA256 implementation using new instructions
   The code is based on Jeffrey Walton's SHA256 implementation:
   https://github.com/noloader/SHA-Intrinsics
*/
FUNC_ISA
static inline void sha256_ni_block(__m128i *core, const uint8_t *p)
{
    __m128i STATE0, STATE1;
    __m128i MSG, TMP;
    __m128i MSG0, MSG1, MSG2, MSG3;
    const __m128i *block = (const __m128i *)p;
    const __m128i MASK = _mm_set_epi64x(
        0x0c0d0e0f08090a0bULL, 0x0405060700010203ULL);

    /* Load initial values */
    STATE0 = core[0];
    STATE1 = core[1];

    /* Rounds 0-3 */
    MSG = _mm_loadu_si128(block);
    MSG0 = _mm_shuffle_epi8(MSG, MASK);
    MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(
                            0xE9B5DBA5B5C0FBCFULL, 0x71374491428A2F98ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

    /* Rounds 4-7 */
    MSG1 = _mm_loadu_si128(block + 1);
    MSG1 = _mm_shuffle_epi8(MSG1, MASK);
    MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(
                            0xAB1C5ED5923F82A4ULL, 0x59F111F13956C25BULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

    /* Rounds 8-11 */
    MSG2 = _mm_loadu_si128(block + 2);
    MSG2 = _mm_shuffle_epi8(MSG2, MASK);
    MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(
                            0x550C7DC3243185BEULL, 0x12835B01D807AA98ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

    /* Rounds 12-15 */
    MSG3 = _mm_loadu_si128(block + 3);
    MSG3 = _mm_shuffle_epi8(MSG3, MASK);
    MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(
                            0xC19BF1749BDC06A7ULL, 0x80DEB1FE72BE5D74ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
    MSG0 = _mm_add_epi32(MSG0, TMP);
    MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

    /* Rounds 16-19 */
    MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(
                            0x240CA1CC0FC19DC6ULL, 0xEFBE4786E49B69C1ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
    MSG1 = _mm_add_epi32(MSG1, TMP);
    MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

    /* Rounds 20-23 */
    MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(
                            0x76F988DA5CB0A9DCULL, 0x4A7484AA2DE92C6FULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
    MSG2 = _mm_add_epi32(MSG2, TMP);
    MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

    /* Rounds 24-27 */
    MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(
                            0xBF597FC7B00327C8ULL, 0xA831C66D983E5152ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
    MSG3 = _mm_add_epi32(MSG3, TMP);
    MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

    /* Rounds 28-31 */
    MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(
                            0x1429296706CA6351ULL,  0xD5A79147C6E00BF3ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
    MSG0 = _mm_add_epi32(MSG0, TMP);
    MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

    /* Rounds 32-35 */
    MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(
                            0x53380D134D2C6DFCULL, 0x2E1B213827B70A85ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
    MSG1 = _mm_add_epi32(MSG1, TMP);
    MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

    /* Rounds 36-39 */
    MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(
                            0x92722C8581C2C92EULL, 0x766A0ABB650A7354ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
    MSG2 = _mm_add_epi32(MSG2, TMP);
    MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

    /* Rounds 40-43 */
    MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(
                            0xC76C51A3C24B8B70ULL, 0xA81A664BA2BFE8A1ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
    MSG3 = _mm_add_epi32(MSG3, TMP);
    MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

    /* Rounds 44-47 */
    MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(
                            0x106AA070F40E3585ULL, 0xD6990624D192E819ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
    MSG0 = _mm_add_epi32(MSG0, TMP);
    MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

    /* Rounds 48-51 */
    MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(
                            0x34B0BCB52748774CULL, 0x1E376C0819A4C116ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
    MSG1 = _mm_add_epi32(MSG1, TMP);
    MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
    MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

    /* Rounds 52-55 */
    MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(
                            0x682E6FF35B9CCA4FULL, 0x4ED8AA4A391C0CB3ULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
    MSG2 = _mm_add_epi32(MSG2, TMP);
    MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

    /* Rounds 56-59 */
    MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(
                            0x8CC7020884C87814ULL, 0x78A5636F748F82EEULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
    MSG3 = _mm_add_epi32(MSG3, TMP);
    MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

    /* Rounds 60-63 */
    MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(
                            0xC67178F2BEF9A3F7ULL, 0xA4506CEB90BEFFFAULL));
    STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
    MSG = _mm_shuffle_epi32(MSG, 0x0E);
    STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

    /* Combine state */
    core[0] = _mm_add_epi32(STATE0, core[0]);
    core[1] = _mm_add_epi32(STATE1, core[1]);
}

typedef struct sha256_ni {
    /*
     * These two vectors store the 8 words of the SHA-256 state, but
     * not in the same order they appear in the spec: the first word
     * holds A,B,E,F and the second word C,D,G,H.
     */
    __m128i core[2];
    sha256_block blk;
    void *pointer_to_free;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha256_ni;

static void sha256_ni_write(BinarySink *bs, const void *vp, size_t len);

static sha256_ni *sha256_ni_alloc(void)
{
    /*
     * The __m128i variables in the context structure need to be
     * 16-byte aligned, but not all malloc implementations that this
     * code has to work with will guarantee to return a 16-byte
     * aligned pointer. So we over-allocate, manually realign the
     * pointer ourselves, and store the original one inside the
     * context so we know how to free it later.
     */
    void *allocation = smalloc(sizeof(sha256_ni) + 15);
    uintptr_t alloc_address = (uintptr_t)allocation;
    uintptr_t aligned_address = (alloc_address + 15) & ~15;
    sha256_ni *s = (sha256_ni *)aligned_address;
    s->pointer_to_free = allocation;
    return s;
}

FUNC_ISA static ssh_hash *sha256_ni_new(const ssh_hashalg *alg)
{
    if (!sha256_hw_available_cached())
        return NULL;

    sha256_ni *s = sha256_ni_alloc();

    /* Initialise the core vectors in their storage order */
    s->core[0] = _mm_set_epi64x(
        0x6a09e667bb67ae85ULL, 0x510e527f9b05688cULL);
    s->core[1] = _mm_set_epi64x(
        0x3c6ef372a54ff53aULL, 0x1f83d9ab5be0cd19ULL);

    sha256_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha256_ni_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha256_ni_copy(ssh_hash *hash)
{
    sha256_ni *s = container_of(hash, sha256_ni, hash);
    sha256_ni *copy = sha256_ni_alloc();

    void *ptf_save = copy->pointer_to_free;
    *copy = *s; /* structure copy */
    copy->pointer_to_free = ptf_save;

    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha256_ni_free(ssh_hash *hash)
{
    sha256_ni *s = container_of(hash, sha256_ni, hash);

    void *ptf = s->pointer_to_free;
    smemclr(s, sizeof(*s));
    sfree(ptf);
}

static void sha256_ni_write(BinarySink *bs, const void *vp, size_t len)
{
    sha256_ni *s = BinarySink_DOWNCAST(bs, sha256_ni);

    while (len > 0)
        if (sha256_block_write(&s->blk, &vp, &len))
            sha256_ni_block(s->core, s->blk.block);
}

FUNC_ISA static void sha256_ni_final(ssh_hash *hash, uint8_t *digest)
{
    sha256_ni *s = container_of(hash, sha256_ni, hash);

    sha256_block_pad(&s->blk, BinarySink_UPCAST(s));

    /* Rearrange the words into the output order */
    __m128i feba = _mm_shuffle_epi32(s->core[0], 0x1B);
    __m128i dchg = _mm_shuffle_epi32(s->core[1], 0xB1);
    __m128i dcba = _mm_blend_epi16(feba, dchg, 0xF0);
    __m128i hgfe = _mm_alignr_epi8(dchg, feba, 8);

    /* Byte-swap them into the output endianness */
    const __m128i mask = _mm_setr_epi8(3,2,1,0,7,6,5,4,11,10,9,8,15,14,13,12);
    dcba = _mm_shuffle_epi8(dcba, mask);
    hgfe = _mm_shuffle_epi8(hgfe, mask);

    /* And store them */
    __m128i *output = (__m128i *)digest;
    _mm_storeu_si128(output, dcba);
    _mm_storeu_si128(output+1, hgfe);

    sha256_ni_free(hash);
}

const ssh_hashalg ssh_sha256_hw = {
    sha256_ni_new, sha256_ni_copy, sha256_ni_final, sha256_ni_free,
    32, 64, HASHALG_NAMES_ANNOTATED("SHA-256", "SHA-NI accelerated"),
};

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-256 using Arm NEON.
 */

#elif HW_SHA256 == HW_SHA256_NEON

/*
 * Manually set the target architecture, if we decided above that we
 * need to.
 */
#ifdef USE_CLANG_ATTR_TARGET_AARCH64
/*
 * A spot of cheating: redefine some ACLE feature macros before
 * including arm_neon.h. Otherwise we won't get the SHA intrinsics
 * defined by that header, because it will be looking at the settings
 * for the whole translation unit rather than the ones we're going to
 * put on some particular functions using __attribute__((target)).
 */
#define __ARM_NEON 1
#define __ARM_FEATURE_CRYPTO 1
#define FUNC_ISA __attribute__ ((target("neon,crypto")))
#endif /* USE_CLANG_ATTR_TARGET_AARCH64 */

#ifndef FUNC_ISA
#define FUNC_ISA
#endif

#ifdef USE_ARM64_NEON_H
#include <arm64_neon.h>
#else
#include <arm_neon.h>
#endif

static bool sha256_hw_available(void)
{
    /*
     * For Arm, we delegate to a per-platform detection function (see
     * explanation in sshaes.c).
     */
    return platform_sha256_hw_available();
}

typedef struct sha256_neon_core sha256_neon_core;
struct sha256_neon_core {
    uint32x4_t abcd, efgh;
};

FUNC_ISA
static inline uint32x4_t sha256_neon_load_input(const uint8_t *p)
{
    return vreinterpretq_u32_u8(vrev32q_u8(vld1q_u8(p)));
}

FUNC_ISA
static inline uint32x4_t sha256_neon_schedule_update(
    uint32x4_t m4, uint32x4_t m3, uint32x4_t m2, uint32x4_t m1)
{
    return vsha256su1q_u32(vsha256su0q_u32(m4, m3), m2, m1);
}

FUNC_ISA
static inline sha256_neon_core sha256_neon_round4(
    sha256_neon_core old, uint32x4_t sched, unsigned round)
{
    sha256_neon_core new;

    uint32x4_t round_input = vaddq_u32(
        sched, vld1q_u32(sha256_round_constants + round));
    new.abcd = vsha256hq_u32 (old.abcd, old.efgh, round_input);
    new.efgh = vsha256h2q_u32(old.efgh, old.abcd, round_input);
    return new;
}

FUNC_ISA
static inline void sha256_neon_block(sha256_neon_core *core, const uint8_t *p)
{
    uint32x4_t s0, s1, s2, s3;
    sha256_neon_core cr = *core;

    s0 = sha256_neon_load_input(p);
    cr = sha256_neon_round4(cr, s0, 0);
    s1 = sha256_neon_load_input(p+16);
    cr = sha256_neon_round4(cr, s1, 4);
    s2 = sha256_neon_load_input(p+32);
    cr = sha256_neon_round4(cr, s2, 8);
    s3 = sha256_neon_load_input(p+48);
    cr = sha256_neon_round4(cr, s3, 12);
    s0 = sha256_neon_schedule_update(s0, s1, s2, s3);
    cr = sha256_neon_round4(cr, s0, 16);
    s1 = sha256_neon_schedule_update(s1, s2, s3, s0);
    cr = sha256_neon_round4(cr, s1, 20);
    s2 = sha256_neon_schedule_update(s2, s3, s0, s1);
    cr = sha256_neon_round4(cr, s2, 24);
    s3 = sha256_neon_schedule_update(s3, s0, s1, s2);
    cr = sha256_neon_round4(cr, s3, 28);
    s0 = sha256_neon_schedule_update(s0, s1, s2, s3);
    cr = sha256_neon_round4(cr, s0, 32);
    s1 = sha256_neon_schedule_update(s1, s2, s3, s0);
    cr = sha256_neon_round4(cr, s1, 36);
    s2 = sha256_neon_schedule_update(s2, s3, s0, s1);
    cr = sha256_neon_round4(cr, s2, 40);
    s3 = sha256_neon_schedule_update(s3, s0, s1, s2);
    cr = sha256_neon_round4(cr, s3, 44);
    s0 = sha256_neon_schedule_update(s0, s1, s2, s3);
    cr = sha256_neon_round4(cr, s0, 48);
    s1 = sha256_neon_schedule_update(s1, s2, s3, s0);
    cr = sha256_neon_round4(cr, s1, 52);
    s2 = sha256_neon_schedule_update(s2, s3, s0, s1);
    cr = sha256_neon_round4(cr, s2, 56);
    s3 = sha256_neon_schedule_update(s3, s0, s1, s2);
    cr = sha256_neon_round4(cr, s3, 60);

    core->abcd = vaddq_u32(core->abcd, cr.abcd);
    core->efgh = vaddq_u32(core->efgh, cr.efgh);
}

typedef struct sha256_neon {
    sha256_neon_core core;
    sha256_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha256_neon;

static void sha256_neon_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha256_neon_new(const ssh_hashalg *alg)
{
    if (!sha256_hw_available_cached())
        return NULL;

    sha256_neon *s = snew(sha256_neon);

    s->core.abcd = vld1q_u32(sha256_initial_state);
    s->core.efgh = vld1q_u32(sha256_initial_state + 4);

    sha256_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha256_neon_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha256_neon_copy(ssh_hash *hash)
{
    sha256_neon *s = container_of(hash, sha256_neon, hash);
    sha256_neon *copy = snew(sha256_neon);

    *copy = *s; /* structure copy */

    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha256_neon_free(ssh_hash *hash)
{
    sha256_neon *s = container_of(hash, sha256_neon, hash);
    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha256_neon_write(BinarySink *bs, const void *vp, size_t len)
{
    sha256_neon *s = BinarySink_DOWNCAST(bs, sha256_neon);

    while (len > 0)
        if (sha256_block_write(&s->blk, &vp, &len))
            sha256_neon_block(&s->core, s->blk.block);
}

static void sha256_neon_final(ssh_hash *hash, uint8_t *digest)
{
    sha256_neon *s = container_of(hash, sha256_neon, hash);

    sha256_block_pad(&s->blk, BinarySink_UPCAST(s));
    vst1q_u8(digest,      vrev32q_u8(vreinterpretq_u8_u32(s->core.abcd)));
    vst1q_u8(digest + 16, vrev32q_u8(vreinterpretq_u8_u32(s->core.efgh)));
    sha256_neon_free(hash);
}

const ssh_hashalg ssh_sha256_hw = {
    sha256_neon_new, sha256_neon_copy, sha256_neon_final, sha256_neon_free,
    32, 64, HASHALG_NAMES_ANNOTATED("SHA-256", "NEON accelerated"),
};

/* ----------------------------------------------------------------------
 * Stub functions if we have no hardware-accelerated SHA-256. In this
 * case, sha256_hw_new returns NULL (though it should also never be
 * selected by sha256_select, so the only thing that should even be
 * _able_ to call it is testcrypt). As a result, the remaining vtable
 * functions should never be called at all.
 */

#elif HW_SHA256 == HW_SHA256_NONE

static bool sha256_hw_available(void)
{
    return false;
}

static ssh_hash *sha256_stub_new(const ssh_hashalg *alg)
{
    return NULL;
}

#define STUB_BODY { unreachable("Should never be called"); }

static ssh_hash *sha256_stub_copy(ssh_hash *hash) STUB_BODY
static void sha256_stub_free(ssh_hash *hash) STUB_BODY
static void sha256_stub_final(ssh_hash *hash, uint8_t *digest) STUB_BODY

const ssh_hashalg ssh_sha256_hw = {
    sha256_stub_new, sha256_stub_copy, sha256_stub_final, sha256_stub_free,
    32, 64, HASHALG_NAMES_ANNOTATED(
        "SHA-256", "!NONEXISTENT ACCELERATED VERSION!"),
};

#endif /* HW_SHA256 */
