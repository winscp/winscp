/*
 * SHA-1 algorithm as described at
 * 
 *   http://csrc.nist.gov/cryptval/shs.html
 */

#include "ssh.h"
#include <assert.h>

/*
 * Start by deciding whether we can support hardware SHA at all.
 */
#define HW_SHA1_NONE 0
#define HW_SHA1_NI 1
#define HW_SHA1_NEON 2

#ifdef _FORCE_SHA_NI
#   define HW_SHA1 HW_SHA1_NI
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<wmmintrin.h>) &&       \
    (defined(__x86_64__) || defined(__i386))
#       define HW_SHA1 HW_SHA1_NI
#   endif
#elif defined(__GNUC__)
#    if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 9)) && \
        (defined(__x86_64__) || defined(__i386))
#       define HW_SHA1 HW_SHA1_NI
#    endif
#elif defined (_MSC_VER)
#   if (defined(_M_X64) || defined(_M_IX86)) && _MSC_FULL_VER >= 150030729
#      define HW_SHA1 HW_SHA1_NI
#   endif
#endif

#ifdef _FORCE_SHA_NEON
#   define HW_SHA1 HW_SHA1_NEON
#elif defined __BYTE_ORDER__ && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    /* Arm can potentially support both endiannesses, but this code
     * hasn't been tested on anything but little. If anyone wants to
     * run big-endian, they'll need to fix it first. */
#elif defined __ARM_FEATURE_CRYPTO
    /* If the Arm crypto extension is available already, we can
     * support NEON SHA without having to enable anything by hand */
#   define HW_SHA1 HW_SHA1_NEON
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<arm_neon.h>) &&       \
    (defined(__aarch64__))
        /* clang can enable the crypto extension in AArch64 using
         * __attribute__((target)) */
#       define HW_SHA1 HW_SHA1_NEON
#       define USE_CLANG_ATTR_TARGET_AARCH64
#   endif
#elif defined _MSC_VER
    /* Visual Studio supports the crypto extension when targeting
     * AArch64, but as of VS2017, the AArch32 header doesn't quite
     * manage it (declaring the shae/shad intrinsics without a round
     * key operand). */
#   if defined _M_ARM64
#       define HW_SHA1 HW_SHA1_NEON
#       if defined _M_ARM64
#           define USE_ARM64_NEON_H /* unusual header name in this case */
#       endif
#   endif
#endif

#if defined _FORCE_SOFTWARE_SHA || !defined HW_SHA1
#   undef HW_SHA1
#   define HW_SHA1 HW_SHA1_NONE
#endif

/*
 * The actual query function that asks if hardware acceleration is
 * available.
 */
static bool sha1_hw_available(void);

/*
 * The top-level selection function, caching the results of
 * sha1_hw_available() so it only has to run once.
 */
static bool sha1_hw_available_cached(void)
{
    static bool initialised = false;
    static bool hw_available;
    if (!initialised) {
        hw_available = sha1_hw_available();
        initialised = true;
    }
    return hw_available;
}

static ssh_hash *sha1_select(const ssh_hashalg *alg)
{
    const ssh_hashalg *real_alg =
        sha1_hw_available_cached() ? &ssh_sha1_hw : &ssh_sha1_sw;

    return ssh_hash_new(real_alg);
}

const ssh_hashalg ssh_sha1 = {
    sha1_select, NULL, NULL, NULL,
    20, 64, HASHALG_NAMES_ANNOTATED("SHA-1", "dummy selector vtable"),
};

/* ----------------------------------------------------------------------
 * Definitions likely to be helpful to multiple implementations.
 */

static const uint32_t sha1_initial_state[] = {
    0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476, 0xc3d2e1f0,
};

#define SHA1_ROUNDS_PER_STAGE 20
#define SHA1_STAGE0_CONSTANT 0x5a827999
#define SHA1_STAGE1_CONSTANT 0x6ed9eba1
#define SHA1_STAGE2_CONSTANT 0x8f1bbcdc
#define SHA1_STAGE3_CONSTANT 0xca62c1d6
#define SHA1_ROUNDS (4 * SHA1_ROUNDS_PER_STAGE)

typedef struct sha1_block sha1_block;
struct sha1_block {
    uint8_t block[64];
    size_t used;
    uint64_t len;
};

static inline void sha1_block_setup(sha1_block *blk)
{
    blk->used = 0;
    blk->len = 0;
}

static inline bool sha1_block_write(
    sha1_block *blk, const void **vdata, size_t *len)
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

static inline void sha1_block_pad(sha1_block *blk, BinarySink *bs)
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
 * Software implementation of SHA-1.
 */

static inline uint32_t rol(uint32_t x, unsigned y)
{
    return (x << (31 & y)) | (x >> (31 & -y));
}

static inline uint32_t Ch(uint32_t ctrl, uint32_t if1, uint32_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint32_t Maj(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint32_t Par(uint32_t x, uint32_t y, uint32_t z)
{
    return (x ^ y ^ z);
}

static inline void sha1_sw_round(
    unsigned round_index, const uint32_t *schedule,
    uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d, uint32_t *e,
    uint32_t f, uint32_t constant)
{
    *e = rol(*a, 5) + f + *e + schedule[round_index] + constant;
    *b = rol(*b, 30);
}

static void sha1_sw_block(uint32_t *core, const uint8_t *block)
{
    uint32_t w[SHA1_ROUNDS];
    uint32_t a,b,c,d,e;

    for (size_t t = 0; t < 16; t++)
        w[t] = GET_32BIT_MSB_FIRST(block + 4*t);

    for (size_t t = 16; t < SHA1_ROUNDS; t++)
	w[t] = rol(w[t - 3] ^ w[t - 8] ^ w[t - 14] ^ w[t - 16], 1);

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4];

    size_t t = 0;
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Ch(b,c,d), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Ch(a,b,c), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Ch(e,a,b), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Ch(d,e,a), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Ch(c,d,e), SHA1_STAGE0_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Par(b,c,d), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Par(a,b,c), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Par(e,a,b), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Par(d,e,a), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Par(c,d,e), SHA1_STAGE1_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Maj(b,c,d), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Maj(a,b,c), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Maj(e,a,b), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Maj(d,e,a), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Maj(c,d,e), SHA1_STAGE2_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Par(b,c,d), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Par(a,b,c), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Par(e,a,b), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Par(d,e,a), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Par(c,d,e), SHA1_STAGE3_CONSTANT);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d; core[4] += e;

    smemclr(w, sizeof(w));
}

typedef struct sha1_sw {
    uint32_t core[5];
    sha1_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha1_sw;

static void sha1_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha1_sw_new(const ssh_hashalg *alg)
{
    sha1_sw *s = snew(sha1_sw);

    memcpy(s->core, sha1_initial_state, sizeof(s->core));

    sha1_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha1_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha1_sw_copy(ssh_hash *hash)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);
    sha1_sw *copy = snew(sha1_sw);

    memcpy(copy, s, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha1_sw_free(ssh_hash *hash)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha1_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha1_sw *s = BinarySink_DOWNCAST(bs, sha1_sw);

    while (len > 0)
        if (sha1_block_write(&s->blk, &vp, &len))
            sha1_sw_block(s->core, s->blk.block);
}

static void sha1_sw_final(ssh_hash *hash, uint8_t *digest)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);

    sha1_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < 5; i++)
        PUT_32BIT_MSB_FIRST(digest + 4*i, s->core[i]);
    sha1_sw_free(hash);
}

const ssh_hashalg ssh_sha1_sw = {
    sha1_sw_new, sha1_sw_copy, sha1_sw_final, sha1_sw_free,
    20, 64, HASHALG_NAMES_ANNOTATED("SHA-1", "unaccelerated"),
};

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-1 using x86 SHA-NI.
 */

#if HW_SHA1 == HW_SHA1_NI

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

static bool sha1_hw_available(void)
{
    unsigned int CPUInfo[4];
    GET_CPU_ID_0(CPUInfo);  
    if (CPUInfo[0] < 7)
        return false;

    GET_CPU_ID_7(CPUInfo);
    return CPUInfo[1] & (1 << 29); /* Check SHA */
}

/* SHA1 implementation using new instructions
   The code is based on Jeffrey Walton's SHA1 implementation:
   https://github.com/noloader/SHA-Intrinsics
*/
FUNC_ISA
static inline void sha1_ni_block(__m128i *core, const uint8_t *p)
{
    __m128i ABCD, E0, E1, MSG0, MSG1, MSG2, MSG3;
    const __m128i MASK = _mm_set_epi64x(
        0x0001020304050607ULL, 0x08090a0b0c0d0e0fULL);

    const __m128i *block = (const __m128i *)p;

    /* Load initial values */
    ABCD = core[0];
    E0 = core[1];

    /* Rounds 0-3 */
    MSG0 = _mm_loadu_si128(block);
    MSG0 = _mm_shuffle_epi8(MSG0, MASK);
    E0 = _mm_add_epi32(E0, MSG0);
    E1 = ABCD;
    ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 0);

    /* Rounds 4-7 */
    MSG1 = _mm_loadu_si128(block + 1);
    MSG1 = _mm_shuffle_epi8(MSG1, MASK);
    E1 = _mm_sha1nexte_epu32(E1, MSG1);
    E0 = ABCD;
    ABCD = _mm_sha1rnds4_epu32(ABCD, E1, 0);
    MSG0 = _mm_sha1msg1_epu32(MSG0, MSG1);

    /* Rounds 8-11 */
    MSG2 = _mm_loadu_si128(block + 2);
    MSG2 = _mm_shuffle_epi8(MSG2, MASK);
    E0 = _mm_sha1nexte_epu32(E0, MSG2);
    E1 = ABCD;
    ABCD = _mm_sha1rnds4_epu32(ABCD, E0, 0);
    MSG1 = _mm_sha1msg1_epu32(MSG1, MSG2);
    MSG0 = _mm_xor_si128(MSG0, MSG2);

    /* Rounds 12-15 */
    MSG3 = _mm_loadu_si128(block + 3);
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
    core[0] = _mm_add_epi32(ABCD, core[0]);
    core[1] = _mm_sha1nexte_epu32(E0, core[1]);
}

typedef struct sha1_ni {
    /*
     * core[0] stores the first four words of the SHA-1 state. core[1]
     * stores just the fifth word, in the vector lane at the highest
     * address.
     */
    __m128i core[2];
    sha1_block blk;
    void *pointer_to_free;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha1_ni;

static void sha1_ni_write(BinarySink *bs, const void *vp, size_t len);

static sha1_ni *sha1_ni_alloc(void)
{
    /*
     * The __m128i variables in the context structure need to be
     * 16-byte aligned, but not all malloc implementations that this
     * code has to work with will guarantee to return a 16-byte
     * aligned pointer. So we over-allocate, manually realign the
     * pointer ourselves, and store the original one inside the
     * context so we know how to free it later.
     */
    void *allocation = smalloc(sizeof(sha1_ni) + 15);
    uintptr_t alloc_address = (uintptr_t)allocation;
    uintptr_t aligned_address = (alloc_address + 15) & ~15;
    sha1_ni *s = (sha1_ni *)aligned_address;
    s->pointer_to_free = allocation;
    return s;
}

FUNC_ISA static ssh_hash *sha1_ni_new(const ssh_hashalg *alg)
{
    if (!sha1_hw_available_cached())
        return NULL;

    sha1_ni *s = sha1_ni_alloc();

    /* Initialise the core vectors in their storage order */
    s->core[0] = _mm_set_epi64x(
        0x67452301efcdab89ULL, 0x98badcfe10325476ULL);
    s->core[1] = _mm_set_epi32(0xc3d2e1f0, 0, 0, 0);

    sha1_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha1_ni_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha1_ni_copy(ssh_hash *hash)
{
    sha1_ni *s = container_of(hash, sha1_ni, hash);
    sha1_ni *copy = sha1_ni_alloc();

    void *ptf_save = copy->pointer_to_free;
    *copy = *s; /* structure copy */
    copy->pointer_to_free = ptf_save;

    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha1_ni_free(ssh_hash *hash)
{
    sha1_ni *s = container_of(hash, sha1_ni, hash);

    void *ptf = s->pointer_to_free;
    smemclr(s, sizeof(*s));
    sfree(ptf);
}

static void sha1_ni_write(BinarySink *bs, const void *vp, size_t len)
{
    sha1_ni *s = BinarySink_DOWNCAST(bs, sha1_ni);

    while (len > 0)
        if (sha1_block_write(&s->blk, &vp, &len))
            sha1_ni_block(s->core, s->blk.block);
}

FUNC_ISA static void sha1_ni_final(ssh_hash *hash, uint8_t *digest)
{
    sha1_ni *s = container_of(hash, sha1_ni, hash);

    sha1_block_pad(&s->blk, BinarySink_UPCAST(s));

    /* Rearrange the first vector into its output order */
    __m128i abcd = _mm_shuffle_epi32(s->core[0], 0x1B);

    /* Byte-swap it into the output endianness */
    const __m128i mask = _mm_setr_epi8(3,2,1,0,7,6,5,4,11,10,9,8,15,14,13,12);
    abcd = _mm_shuffle_epi8(abcd, mask);

    /* And store it */
    _mm_storeu_si128((__m128i *)digest, abcd);

    /* Finally, store the leftover word */
    uint32_t e = _mm_extract_epi32(s->core[1], 3);
    PUT_32BIT_MSB_FIRST(digest + 16, e);

    sha1_ni_free(hash);
}

const ssh_hashalg ssh_sha1_hw = {
    sha1_ni_new, sha1_ni_copy, sha1_ni_final, sha1_ni_free,
    20, 64, HASHALG_NAMES_ANNOTATED("SHA-1", "SHA-NI accelerated"),
};

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-1 using Arm NEON.
 */

#elif HW_SHA1 == HW_SHA1_NEON

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

static bool sha1_hw_available(void)
{
    /*
     * For Arm, we delegate to a per-platform detection function (see
     * explanation in sshaes.c).
     */
    return platform_sha1_hw_available();
}

typedef struct sha1_neon_core sha1_neon_core;
struct sha1_neon_core {
    uint32x4_t abcd;
    uint32_t e;
};

/* ------------- got up to here ----------------------------------------- */

FUNC_ISA
static inline uint32x4_t sha1_neon_load_input(const uint8_t *p)
{
    return vreinterpretq_u32_u8(vrev32q_u8(vld1q_u8(p)));
}

FUNC_ISA
static inline uint32x4_t sha1_neon_schedule_update(
    uint32x4_t m4, uint32x4_t m3, uint32x4_t m2, uint32x4_t m1)
{
    return vsha1su1q_u32(vsha1su0q_u32(m4, m3, m2), m1);
}

/*
 * SHA-1 has three different kinds of round, differing in whether they
 * use the Ch, Maj or Par functions defined above. Each one uses a
 * separate NEON instruction, so we define three inline functions for
 * the different round types using this macro.
 *
 * The two batches of Par-type rounds also use a different constant,
 * but that's passed in as an operand, so we don't need a fourth
 * inline function just for that.
 */
#define SHA1_NEON_ROUND_FN(type)                                        \
    FUNC_ISA static inline sha1_neon_core sha1_neon_round4_##type(      \
        sha1_neon_core old, uint32x4_t sched, uint32x4_t constant)      \
    {                                                                   \
        sha1_neon_core new;                                             \
        uint32x4_t round_input = vaddq_u32(sched, constant);            \
        new.abcd = vsha1##type##q_u32(old.abcd, old.e, round_input);    \
        new.e = vsha1h_u32(vget_lane_u32(vget_low_u32(old.abcd), 0));   \
        return new;                                                     \
    }
SHA1_NEON_ROUND_FN(c)
SHA1_NEON_ROUND_FN(p)
SHA1_NEON_ROUND_FN(m)

FUNC_ISA
static inline void sha1_neon_block(sha1_neon_core *core, const uint8_t *p)
{
    uint32x4_t constant, s0, s1, s2, s3;
    sha1_neon_core cr = *core;

    constant = vdupq_n_u32(SHA1_STAGE0_CONSTANT);
    s0 = sha1_neon_load_input(p);
    cr = sha1_neon_round4_c(cr, s0, constant);
    s1 = sha1_neon_load_input(p + 16);
    cr = sha1_neon_round4_c(cr, s1, constant);
    s2 = sha1_neon_load_input(p + 32);
    cr = sha1_neon_round4_c(cr, s2, constant);
    s3 = sha1_neon_load_input(p + 48);
    cr = sha1_neon_round4_c(cr, s3, constant);
    s0 = sha1_neon_schedule_update(s0, s1, s2, s3);
    cr = sha1_neon_round4_c(cr, s0, constant);

    constant = vdupq_n_u32(SHA1_STAGE1_CONSTANT);
    s1 = sha1_neon_schedule_update(s1, s2, s3, s0);
    cr = sha1_neon_round4_p(cr, s1, constant);
    s2 = sha1_neon_schedule_update(s2, s3, s0, s1);
    cr = sha1_neon_round4_p(cr, s2, constant);
    s3 = sha1_neon_schedule_update(s3, s0, s1, s2);
    cr = sha1_neon_round4_p(cr, s3, constant);
    s0 = sha1_neon_schedule_update(s0, s1, s2, s3);
    cr = sha1_neon_round4_p(cr, s0, constant);
    s1 = sha1_neon_schedule_update(s1, s2, s3, s0);
    cr = sha1_neon_round4_p(cr, s1, constant);

    constant = vdupq_n_u32(SHA1_STAGE2_CONSTANT);
    s2 = sha1_neon_schedule_update(s2, s3, s0, s1);
    cr = sha1_neon_round4_m(cr, s2, constant);
    s3 = sha1_neon_schedule_update(s3, s0, s1, s2);
    cr = sha1_neon_round4_m(cr, s3, constant);
    s0 = sha1_neon_schedule_update(s0, s1, s2, s3);
    cr = sha1_neon_round4_m(cr, s0, constant);
    s1 = sha1_neon_schedule_update(s1, s2, s3, s0);
    cr = sha1_neon_round4_m(cr, s1, constant);
    s2 = sha1_neon_schedule_update(s2, s3, s0, s1);
    cr = sha1_neon_round4_m(cr, s2, constant);

    constant = vdupq_n_u32(SHA1_STAGE3_CONSTANT);
    s3 = sha1_neon_schedule_update(s3, s0, s1, s2);
    cr = sha1_neon_round4_p(cr, s3, constant);
    s0 = sha1_neon_schedule_update(s0, s1, s2, s3);
    cr = sha1_neon_round4_p(cr, s0, constant);
    s1 = sha1_neon_schedule_update(s1, s2, s3, s0);
    cr = sha1_neon_round4_p(cr, s1, constant);
    s2 = sha1_neon_schedule_update(s2, s3, s0, s1);
    cr = sha1_neon_round4_p(cr, s2, constant);
    s3 = sha1_neon_schedule_update(s3, s0, s1, s2);
    cr = sha1_neon_round4_p(cr, s3, constant);

    core->abcd = vaddq_u32(core->abcd, cr.abcd);
    core->e += cr.e;
}

typedef struct sha1_neon {
    sha1_neon_core core;
    sha1_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha1_neon;

static void sha1_neon_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha1_neon_new(const ssh_hashalg *alg)
{
    if (!sha1_hw_available_cached())
        return NULL;

    sha1_neon *s = snew(sha1_neon);

    s->core.abcd = vld1q_u32(sha1_initial_state);
    s->core.e = sha1_initial_state[4];

    sha1_block_setup(&s->blk);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha1_neon_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *sha1_neon_copy(ssh_hash *hash)
{
    sha1_neon *s = container_of(hash, sha1_neon, hash);
    sha1_neon *copy = snew(sha1_neon);

    *copy = *s; /* structure copy */

    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);

    return &copy->hash;
}

static void sha1_neon_free(ssh_hash *hash)
{
    sha1_neon *s = container_of(hash, sha1_neon, hash);
    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha1_neon_write(BinarySink *bs, const void *vp, size_t len)
{
    sha1_neon *s = BinarySink_DOWNCAST(bs, sha1_neon);

    while (len > 0)
        if (sha1_block_write(&s->blk, &vp, &len))
            sha1_neon_block(&s->core, s->blk.block);
}

static void sha1_neon_final(ssh_hash *hash, uint8_t *digest)
{
    sha1_neon *s = container_of(hash, sha1_neon, hash);

    sha1_block_pad(&s->blk, BinarySink_UPCAST(s));
    vst1q_u8(digest, vrev32q_u8(vreinterpretq_u8_u32(s->core.abcd)));
    PUT_32BIT_MSB_FIRST(digest + 16, s->core.e);
    sha1_neon_free(hash);
}

const ssh_hashalg ssh_sha1_hw = {
    sha1_neon_new, sha1_neon_copy, sha1_neon_final, sha1_neon_free,
    20, 64, HASHALG_NAMES_ANNOTATED("SHA-1", "NEON accelerated"),
};

/* ----------------------------------------------------------------------
 * Stub functions if we have no hardware-accelerated SHA-1. In this
 * case, sha1_hw_new returns NULL (though it should also never be
 * selected by sha1_select, so the only thing that should even be
 * _able_ to call it is testcrypt). As a result, the remaining vtable
 * functions should never be called at all.
 */

#elif HW_SHA1 == HW_SHA1_NONE

static bool sha1_hw_available(void)
{
    return false;
}

static ssh_hash *sha1_stub_new(const ssh_hashalg *alg)
{
    return NULL;
}

#define STUB_BODY { unreachable("Should never be called"); }

static ssh_hash *sha1_stub_copy(ssh_hash *hash) STUB_BODY
static void sha1_stub_free(ssh_hash *hash) STUB_BODY
static void sha1_stub_final(ssh_hash *hash, uint8_t *digest) STUB_BODY

const ssh_hashalg ssh_sha1_hw = {
    sha1_stub_new, sha1_stub_copy, sha1_stub_final, sha1_stub_free,
    20, 64, HASHALG_NAMES_ANNOTATED(
        "SHA-1", "!NONEXISTENT ACCELERATED VERSION!"),
};

#endif /* HW_SHA1 */
