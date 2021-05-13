/*
 * SHA-512 algorithm as described at
 *
 *   http://csrc.nist.gov/cryptval/shs.html
 *
 * Modifications made for SHA-384 also
 */

#include <assert.h>
#include "ssh.h"

/*
 * Start by deciding whether we can support hardware SHA at all.
 */
#define HW_SHA512_NONE 0
#define HW_SHA512_NEON 1

#ifdef _FORCE_SHA512_NEON
#   define HW_SHA512 HW_SHA512_NEON
#elif defined __BYTE_ORDER__ && __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
    /* Arm can potentially support both endiannesses, but this code
     * hasn't been tested on anything but little. If anyone wants to
     * run big-endian, they'll need to fix it first. */
#elif defined __ARM_FEATURE_SHA512
    /* If the Arm SHA-512 extension is available already, we can
     * support NEON SHA without having to enable anything by hand */
#   define HW_SHA512 HW_SHA512_NEON
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<arm_neon.h>) &&       \
    (defined(__aarch64__))
        /* clang can enable the crypto extension in AArch64 using
         * __attribute__((target)) */
#       define HW_SHA512 HW_SHA512_NEON
#       define USE_CLANG_ATTR_TARGET_AARCH64
#   endif
#endif

#if defined _FORCE_SOFTWARE_SHA || !defined HW_SHA512
#   undef HW_SHA512
#   define HW_SHA512 HW_SHA512_NONE
#endif

/*
 * The actual query function that asks if hardware acceleration is
 * available.
 */
static bool sha512_hw_available(void);

/*
 * The top-level selection function, caching the results of
 * sha512_hw_available() so it only has to run once.
 */
static bool sha512_hw_available_cached(void)
{
    static bool initialised = false;
    static bool hw_available;
    if (!initialised) {
        hw_available = sha512_hw_available();
        initialised = true;
    }
    return hw_available;
}

struct sha512_select_options {
    const ssh_hashalg *hw, *sw;
};

static ssh_hash *sha512_select(const ssh_hashalg *alg)
{
    const struct sha512_select_options *options =
        (const struct sha512_select_options *)alg->extra;

    const ssh_hashalg *real_alg =
        sha512_hw_available_cached() ? options->hw : options->sw;

    return ssh_hash_new(real_alg);
}

const struct sha512_select_options ssh_sha512_select_options = {
    &ssh_sha512_hw, &ssh_sha512_sw,
};
const struct sha512_select_options ssh_sha384_select_options = {
    &ssh_sha384_hw, &ssh_sha384_sw,
};

const ssh_hashalg ssh_sha512 = {
    .new = sha512_select,
    .hlen = 64,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-512", "dummy selector vtable"),
    .extra = &ssh_sha512_select_options,
};

const ssh_hashalg ssh_sha384 = {
    .new = sha512_select,
    .hlen = 48,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-384", "dummy selector vtable"),
    .extra = &ssh_sha384_select_options,
};

/* ----------------------------------------------------------------------
 * Definitions likely to be helpful to multiple implementations.
 */

static const uint64_t sha512_initial_state[] = {
    0x6a09e667f3bcc908ULL,
    0xbb67ae8584caa73bULL,
    0x3c6ef372fe94f82bULL,
    0xa54ff53a5f1d36f1ULL,
    0x510e527fade682d1ULL,
    0x9b05688c2b3e6c1fULL,
    0x1f83d9abfb41bd6bULL,
    0x5be0cd19137e2179ULL,
};

static const uint64_t sha384_initial_state[] = {
    0xcbbb9d5dc1059ed8ULL,
    0x629a292a367cd507ULL,
    0x9159015a3070dd17ULL,
    0x152fecd8f70e5939ULL,
    0x67332667ffc00b31ULL,
    0x8eb44a8768581511ULL,
    0xdb0c2e0d64f98fa7ULL,
    0x47b5481dbefa4fa4ULL,
};

static const uint64_t sha512_round_constants[] = {
    0x428a2f98d728ae22ULL, 0x7137449123ef65cdULL,
    0xb5c0fbcfec4d3b2fULL, 0xe9b5dba58189dbbcULL,
    0x3956c25bf348b538ULL, 0x59f111f1b605d019ULL,
    0x923f82a4af194f9bULL, 0xab1c5ed5da6d8118ULL,
    0xd807aa98a3030242ULL, 0x12835b0145706fbeULL,
    0x243185be4ee4b28cULL, 0x550c7dc3d5ffb4e2ULL,
    0x72be5d74f27b896fULL, 0x80deb1fe3b1696b1ULL,
    0x9bdc06a725c71235ULL, 0xc19bf174cf692694ULL,
    0xe49b69c19ef14ad2ULL, 0xefbe4786384f25e3ULL,
    0x0fc19dc68b8cd5b5ULL, 0x240ca1cc77ac9c65ULL,
    0x2de92c6f592b0275ULL, 0x4a7484aa6ea6e483ULL,
    0x5cb0a9dcbd41fbd4ULL, 0x76f988da831153b5ULL,
    0x983e5152ee66dfabULL, 0xa831c66d2db43210ULL,
    0xb00327c898fb213fULL, 0xbf597fc7beef0ee4ULL,
    0xc6e00bf33da88fc2ULL, 0xd5a79147930aa725ULL,
    0x06ca6351e003826fULL, 0x142929670a0e6e70ULL,
    0x27b70a8546d22ffcULL, 0x2e1b21385c26c926ULL,
    0x4d2c6dfc5ac42aedULL, 0x53380d139d95b3dfULL,
    0x650a73548baf63deULL, 0x766a0abb3c77b2a8ULL,
    0x81c2c92e47edaee6ULL, 0x92722c851482353bULL,
    0xa2bfe8a14cf10364ULL, 0xa81a664bbc423001ULL,
    0xc24b8b70d0f89791ULL, 0xc76c51a30654be30ULL,
    0xd192e819d6ef5218ULL, 0xd69906245565a910ULL,
    0xf40e35855771202aULL, 0x106aa07032bbd1b8ULL,
    0x19a4c116b8d2d0c8ULL, 0x1e376c085141ab53ULL,
    0x2748774cdf8eeb99ULL, 0x34b0bcb5e19b48a8ULL,
    0x391c0cb3c5c95a63ULL, 0x4ed8aa4ae3418acbULL,
    0x5b9cca4f7763e373ULL, 0x682e6ff3d6b2b8a3ULL,
    0x748f82ee5defb2fcULL, 0x78a5636f43172f60ULL,
    0x84c87814a1f0ab72ULL, 0x8cc702081a6439ecULL,
    0x90befffa23631e28ULL, 0xa4506cebde82bde9ULL,
    0xbef9a3f7b2c67915ULL, 0xc67178f2e372532bULL,
    0xca273eceea26619cULL, 0xd186b8c721c0c207ULL,
    0xeada7dd6cde0eb1eULL, 0xf57d4f7fee6ed178ULL,
    0x06f067aa72176fbaULL, 0x0a637dc5a2c898a6ULL,
    0x113f9804bef90daeULL, 0x1b710b35131c471bULL,
    0x28db77f523047d84ULL, 0x32caab7b40c72493ULL,
    0x3c9ebe0a15c9bebcULL, 0x431d67c49c100d4cULL,
    0x4cc5d4becb3e42b6ULL, 0x597f299cfc657e2aULL,
    0x5fcb6fab3ad6faecULL, 0x6c44198c4a475817ULL,
};

#define SHA512_ROUNDS 80

typedef struct sha512_block sha512_block;
struct sha512_block {
    uint8_t block[128];
    size_t used;
    uint64_t lenhi, lenlo;
};

static inline void sha512_block_setup(sha512_block *blk)
{
    blk->used = 0;
    blk->lenhi = blk->lenlo = 0;
}

static inline bool sha512_block_write(
    sha512_block *blk, const void **vdata, size_t *len)
{
    size_t blkleft = sizeof(blk->block) - blk->used;
    size_t chunk = *len < blkleft ? *len : blkleft;

    const uint8_t *p = *vdata;
    memcpy(blk->block + blk->used, p, chunk);
    *vdata = p + chunk;
    *len -= chunk;
    blk->used += chunk;

    size_t chunkbits = chunk << 3;

    blk->lenlo += chunkbits;
    blk->lenhi += (blk->lenlo < chunkbits);

    if (blk->used == sizeof(blk->block)) {
        blk->used = 0;
        return true;
    }

    return false;
}

static inline void sha512_block_pad(sha512_block *blk, BinarySink *bs)
{
    uint64_t final_lenhi = blk->lenhi;
    uint64_t final_lenlo = blk->lenlo;
    size_t pad = 127 & (111 - blk->used);

    put_byte(bs, 0x80);
    put_padding(bs, pad, 0);
    put_uint64(bs, final_lenhi);
    put_uint64(bs, final_lenlo);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}

/* ----------------------------------------------------------------------
 * Software implementation of SHA-512.
 */

static inline uint64_t ror(uint64_t x, unsigned y)
{
    return (x << (63 & -y)) | (x >> (63 & y));
}

static inline uint64_t Ch(uint64_t ctrl, uint64_t if1, uint64_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint64_t Maj(uint64_t x, uint64_t y, uint64_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint64_t Sigma_0(uint64_t x)
{
    return ror(x,28) ^ ror(x,34) ^ ror(x,39);
}

static inline uint64_t Sigma_1(uint64_t x)
{
    return ror(x,14) ^ ror(x,18) ^ ror(x,41);
}

static inline uint64_t sigma_0(uint64_t x)
{
    return ror(x,1) ^ ror(x,8) ^ (x >> 7);
}

static inline uint64_t sigma_1(uint64_t x)
{
    return ror(x,19) ^ ror(x,61) ^ (x >> 6);
}

static inline void sha512_sw_round(
    unsigned round_index, const uint64_t *schedule,
    uint64_t *a, uint64_t *b, uint64_t *c, uint64_t *d,
    uint64_t *e, uint64_t *f, uint64_t *g, uint64_t *h)
{
    uint64_t t1 = *h + Sigma_1(*e) + Ch(*e,*f,*g) +
        sha512_round_constants[round_index] + schedule[round_index];

    uint64_t t2 = Sigma_0(*a) + Maj(*a,*b,*c);

    *d += t1;
    *h = t1 + t2;
}

static void sha512_sw_block(uint64_t *core, const uint8_t *block)
{
    uint64_t w[SHA512_ROUNDS];
    uint64_t a,b,c,d,e,f,g,h;

    int t;

    for (t = 0; t < 16; t++)
        w[t] = GET_64BIT_MSB_FIRST(block + 8*t);

    for (t = 16; t < SHA512_ROUNDS; t++)
        w[t] = w[t-16] + w[t-7] + sigma_0(w[t-15]) + sigma_1(w[t-2]);

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4]; f = core[5]; g = core[6]; h = core[7];

    for (t = 0; t < SHA512_ROUNDS; t+=8) {
        sha512_sw_round(t+0, w, &a,&b,&c,&d,&e,&f,&g,&h);
        sha512_sw_round(t+1, w, &h,&a,&b,&c,&d,&e,&f,&g);
        sha512_sw_round(t+2, w, &g,&h,&a,&b,&c,&d,&e,&f);
        sha512_sw_round(t+3, w, &f,&g,&h,&a,&b,&c,&d,&e);
        sha512_sw_round(t+4, w, &e,&f,&g,&h,&a,&b,&c,&d);
        sha512_sw_round(t+5, w, &d,&e,&f,&g,&h,&a,&b,&c);
        sha512_sw_round(t+6, w, &c,&d,&e,&f,&g,&h,&a,&b);
        sha512_sw_round(t+7, w, &b,&c,&d,&e,&f,&g,&h,&a);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d;
    core[4] += e; core[5] += f; core[6] += g; core[7] += h;

    smemclr(w, sizeof(w));
}

typedef struct sha512_sw {
    uint64_t core[8];
    sha512_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha512_sw;

static void sha512_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha512_sw_new(const ssh_hashalg *alg)
{
    sha512_sw *s = snew(sha512_sw);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha512_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void sha512_sw_reset(ssh_hash *hash)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);

    /* The 'extra' field in the ssh_hashalg indicates which
     * initialisation vector we're using */
    memcpy(s->core, hash->vt->extra, sizeof(s->core));
    sha512_block_setup(&s->blk);
}

static void sha512_sw_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    sha512_sw *copy = container_of(hcopy, sha512_sw, hash);
    sha512_sw *orig = container_of(horig, sha512_sw, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void sha512_sw_free(ssh_hash *hash)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha512_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha512_sw *s = BinarySink_DOWNCAST(bs, sha512_sw);

    while (len > 0)
        if (sha512_block_write(&s->blk, &vp, &len))
            sha512_sw_block(s->core, s->blk.block);
}

static void sha512_sw_digest(ssh_hash *hash, uint8_t *digest)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);

    sha512_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < hash->vt->hlen / 8; i++)
        PUT_64BIT_MSB_FIRST(digest + 8*i, s->core[i]);
}

const ssh_hashalg ssh_sha512_sw = {
    .new = sha512_sw_new,
    .reset = sha512_sw_reset,
    .copyfrom = sha512_sw_copyfrom,
    .digest = sha512_sw_digest,
    .free = sha512_sw_free,
    .hlen = 64,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-512", "unaccelerated"),
    .extra = sha512_initial_state,
};

const ssh_hashalg ssh_sha384_sw = {
    .new = sha512_sw_new,
    .reset = sha512_sw_reset,
    .copyfrom = sha512_sw_copyfrom,
    .digest = sha512_sw_digest,
    .free = sha512_sw_free,
    .hlen = 48,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-384", "unaccelerated"),
    .extra = sha384_initial_state,
};

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-512 using Arm NEON.
 */

#if HW_SHA512 == HW_SHA512_NEON

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
#define FUNC_ISA __attribute__ ((target("neon,sha3")))
#endif /* USE_CLANG_ATTR_TARGET_AARCH64 */

#ifndef FUNC_ISA
#define FUNC_ISA
#endif

#ifdef USE_ARM64_NEON_H
#include <arm64_neon.h>
#else
#include <arm_neon.h>
#endif

static bool sha512_hw_available(void)
{
    /*
     * For Arm, we delegate to a per-platform detection function (see
     * explanation in sshaes.c).
     */
    return platform_sha512_hw_available();
}

#if defined __clang__
/*
 * As of 2020-12-24, I've found that clang doesn't provide the SHA-512
 * NEON intrinsics. So I define my own set using inline assembler, and
 * use #define to effectively rename them over the top of the standard
 * names.
 *
 * The aim of that #define technique is that it should avoid a build
 * failure if these intrinsics _are_ defined in <arm_neon.h>.
 * Obviously it would be better in that situation to switch back to
 * using the real intrinsics, but until I see a version of clang that
 * supports them, I won't know what version number to test in the
 * ifdef.
 */
static inline FUNC_ISA
uint64x2_t vsha512su0q_u64_asm(uint64x2_t x, uint64x2_t y) {
    __asm__("sha512su0 %0.2D,%1.2D" : "+w" (x) : "w" (y));
    return x;
}
static inline FUNC_ISA
uint64x2_t vsha512su1q_u64_asm(uint64x2_t x, uint64x2_t y, uint64x2_t z) {
    __asm__("sha512su1 %0.2D,%1.2D,%2.2D" : "+w" (x) : "w" (y), "w" (z));
    return x;
}
static inline FUNC_ISA
uint64x2_t vsha512hq_u64_asm(uint64x2_t x, uint64x2_t y, uint64x2_t z) {
    __asm__("sha512h %0,%1,%2.2D" : "+w" (x) : "w" (y), "w" (z));
    return x;
}
static inline FUNC_ISA
uint64x2_t vsha512h2q_u64_asm(uint64x2_t x, uint64x2_t y, uint64x2_t z) {
    __asm__("sha512h2 %0,%1,%2.2D" : "+w" (x) : "w" (y), "w" (z));
    return x;
}
#undef vsha512su0q_u64
#define vsha512su0q_u64 vsha512su0q_u64_asm
#undef vsha512su1q_u64
#define vsha512su1q_u64 vsha512su1q_u64_asm
#undef vsha512hq_u64
#define vsha512hq_u64 vsha512hq_u64_asm
#undef vsha512h2q_u64
#define vsha512h2q_u64 vsha512h2q_u64_asm
#endif /* defined __clang__ */

typedef struct sha512_neon_core sha512_neon_core;
struct sha512_neon_core {
    uint64x2_t ab, cd, ef, gh;
};

FUNC_ISA
static inline uint64x2_t sha512_neon_load_input(const uint8_t *p)
{
    return vreinterpretq_u64_u8(vrev64q_u8(vld1q_u8(p)));
}

FUNC_ISA
static inline uint64x2_t sha512_neon_schedule_update(
    uint64x2_t m8, uint64x2_t m7, uint64x2_t m4, uint64x2_t m3, uint64x2_t m1)
{
    /*
     * vsha512su0q_u64() takes words from a long way back in the
     * schedule and performs the sigma_0 half of the computation of
     * the next two 64-bit message-schedule words.
     *
     * vsha512su1q_u64() combines the result of that with the sigma_1
     * steps, to output the finished version of those two words. The
     * total amount of input data it requires fits nicely into three
     * 128-bit vector registers, but one of those registers is
     * misaligned compared to the 128-bit chunks that the message
     * schedule is stored in. So we use vextq_u64 to make one of its
     * input words out of the second half of m4 and the first half of
     * m3.
     */
    return vsha512su1q_u64(vsha512su0q_u64(m8, m7), m1, vextq_u64(m4, m3, 1));
}

FUNC_ISA
static inline void sha512_neon_round2(
    unsigned round_index, uint64x2_t schedule_words,
    uint64x2_t *ab, uint64x2_t *cd, uint64x2_t *ef, uint64x2_t *gh)
{
    /*
     * vsha512hq_u64 performs the Sigma_1 and Ch half of the
     * computation of two rounds of SHA-512 (including feeding back
     * one of the outputs from the first of those half-rounds into the
     * second one).
     *
     * vsha512h2q_u64 combines the result of that with the Sigma_0 and
     * Maj steps, and outputs one 128-bit vector that replaces the gh
     * piece of the input hash state, and a second that updates cd by
     * addition.
     *
     * Similarly to vsha512su1q_u64 above, some of the input registers
     * expected by these instructions are misaligned by 64 bits
     * relative to the chunks we've divided the hash state into, so we
     * have to start by making 'de' and 'fg' words out of our input
     * cd,ef,gh, using vextq_u64.
     *
     * Also, one of the inputs to vsha512hq_u64 is expected to contain
     * the results of summing gh + two round constants + two words of
     * message schedule, but the two words of the message schedule
     * have to be the opposite way round in the vector register from
     * the way that vsha512su1q_u64 output them. Hence, there's
     * another vextq_u64 in here that swaps the two halves of the
     * initial_sum vector register.
     *
     * (This also means that I don't have to prepare a specially
     * reordered version of the sha512_round_constants[] array: as
     * long as I'm unavoidably doing a swap at run time _anyway_, I
     * can load from the normally ordered version of that array, and
     * just take care to fold in that data _before_ the swap rather
     * than after.)
     */

    /* Load two round constants, with the first one in the low half */
    uint64x2_t round_constants = vld1q_u64(
        sha512_round_constants + round_index);

    /* Add schedule words to round constants */
    uint64x2_t initial_sum = vaddq_u64(schedule_words, round_constants);

    /* Swap that sum around so the word used in the first of the two
     * rounds is in the _high_ half of the vector, matching where h
     * lives in the gh vector */
    uint64x2_t swapped_initial_sum = vextq_u64(initial_sum, initial_sum, 1);

    /* Add gh to that, now that they're matching ways round */
    uint64x2_t sum = vaddq_u64(swapped_initial_sum, *gh);

    /* Make the misaligned de and fg words */
    uint64x2_t de = vextq_u64(*cd, *ef, 1);
    uint64x2_t fg = vextq_u64(*ef, *gh, 1);

    /* Now we're ready to put all the pieces together. The output from
     * vsha512h2q_u64 can be used directly as the new gh, and the
     * output from vsha512hq_u64 is simultaneously the intermediate
     * value passed to h2 and the thing you have to add on to cd. */
    uint64x2_t intermed = vsha512hq_u64(sum, fg, de);
    *gh = vsha512h2q_u64(intermed, *cd, *ab);
    *cd = vaddq_u64(*cd, intermed);
}

FUNC_ISA
static inline void sha512_neon_block(sha512_neon_core *core, const uint8_t *p)
{
    uint64x2_t s0, s1, s2, s3, s4, s5, s6, s7;

    uint64x2_t ab = core->ab, cd = core->cd, ef = core->ef, gh = core->gh;

    s0 = sha512_neon_load_input(p + 16*0);
    sha512_neon_round2(0, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_load_input(p + 16*1);
    sha512_neon_round2(2, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_load_input(p + 16*2);
    sha512_neon_round2(4, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_load_input(p + 16*3);
    sha512_neon_round2(6, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_load_input(p + 16*4);
    sha512_neon_round2(8, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_load_input(p + 16*5);
    sha512_neon_round2(10, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_load_input(p + 16*6);
    sha512_neon_round2(12, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_load_input(p + 16*7);
    sha512_neon_round2(14, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(16, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(18, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(20, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(22, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(24, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(26, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(28, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(30, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(32, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(34, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(36, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(38, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(40, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(42, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(44, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(46, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(48, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(50, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(52, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(54, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(56, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(58, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(60, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(62, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(64, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(66, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(68, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(70, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(72, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(74, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(76, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(78, s7, &cd, &ef, &gh, &ab);

    core->ab = vaddq_u64(core->ab, ab);
    core->cd = vaddq_u64(core->cd, cd);
    core->ef = vaddq_u64(core->ef, ef);
    core->gh = vaddq_u64(core->gh, gh);
}

typedef struct sha512_neon {
    sha512_neon_core core;
    sha512_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha512_neon;

static void sha512_neon_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha512_neon_new(const ssh_hashalg *alg)
{
    if (!sha512_hw_available_cached())
        return NULL;

    sha512_neon *s = snew(sha512_neon);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha512_neon_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void sha512_neon_reset(ssh_hash *hash)
{
    sha512_neon *s = container_of(hash, sha512_neon, hash);
    const uint64_t *iv = (const uint64_t *)hash->vt->extra;

    s->core.ab = vld1q_u64(iv);
    s->core.cd = vld1q_u64(iv+2);
    s->core.ef = vld1q_u64(iv+4);
    s->core.gh = vld1q_u64(iv+6);

    sha512_block_setup(&s->blk);
}

static void sha512_neon_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    sha512_neon *copy = container_of(hcopy, sha512_neon, hash);
    sha512_neon *orig = container_of(horig, sha512_neon, hash);

    *copy = *orig; /* structure copy */

    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void sha512_neon_free(ssh_hash *hash)
{
    sha512_neon *s = container_of(hash, sha512_neon, hash);
    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha512_neon_write(BinarySink *bs, const void *vp, size_t len)
{
    sha512_neon *s = BinarySink_DOWNCAST(bs, sha512_neon);

    while (len > 0)
        if (sha512_block_write(&s->blk, &vp, &len))
            sha512_neon_block(&s->core, s->blk.block);
}

static void sha512_neon_digest(ssh_hash *hash, uint8_t *digest)
{
    sha512_neon *s = container_of(hash, sha512_neon, hash);

    sha512_block_pad(&s->blk, BinarySink_UPCAST(s));

    vst1q_u8(digest,    vrev64q_u8(vreinterpretq_u8_u64(s->core.ab)));
    vst1q_u8(digest+16, vrev64q_u8(vreinterpretq_u8_u64(s->core.cd)));
    vst1q_u8(digest+32, vrev64q_u8(vreinterpretq_u8_u64(s->core.ef)));
    vst1q_u8(digest+48, vrev64q_u8(vreinterpretq_u8_u64(s->core.gh)));
}

static void sha384_neon_digest(ssh_hash *hash, uint8_t *digest)
{
    sha512_neon *s = container_of(hash, sha512_neon, hash);

    sha512_block_pad(&s->blk, BinarySink_UPCAST(s));

    vst1q_u8(digest,    vrev64q_u8(vreinterpretq_u8_u64(s->core.ab)));
    vst1q_u8(digest+16, vrev64q_u8(vreinterpretq_u8_u64(s->core.cd)));
    vst1q_u8(digest+32, vrev64q_u8(vreinterpretq_u8_u64(s->core.ef)));
}

const ssh_hashalg ssh_sha512_hw = {
    .new = sha512_neon_new,
    .reset = sha512_neon_reset,
    .copyfrom = sha512_neon_copyfrom,
    .digest = sha512_neon_digest,
    .free = sha512_neon_free,
    .hlen = 64,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-512", "NEON accelerated"),
    .extra = sha512_initial_state,
};

const ssh_hashalg ssh_sha384_hw = {
    .new = sha512_neon_new,
    .reset = sha512_neon_reset,
    .copyfrom = sha512_neon_copyfrom,
    .digest = sha384_neon_digest,
    .free = sha512_neon_free,
    .hlen = 48,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-384", "NEON accelerated"),
    .extra = sha384_initial_state,
};

/* ----------------------------------------------------------------------
 * Stub functions if we have no hardware-accelerated SHA-512. In this
 * case, sha512_hw_new returns NULL (though it should also never be
 * selected by sha512_select, so the only thing that should even be
 * _able_ to call it is testcrypt). As a result, the remaining vtable
 * functions should never be called at all.
 */

#elif HW_SHA512 == HW_SHA512_NONE

static bool sha512_hw_available(void)
{
    return false;
}

static ssh_hash *sha512_stub_new(const ssh_hashalg *alg)
{
    return NULL;
}

#define STUB_BODY { unreachable("Should never be called"); }

static void sha512_stub_reset(ssh_hash *hash) STUB_BODY
static void sha512_stub_copyfrom(ssh_hash *hash, ssh_hash *orig) STUB_BODY
static void sha512_stub_free(ssh_hash *hash) STUB_BODY
static void sha512_stub_digest(ssh_hash *hash, uint8_t *digest) STUB_BODY

const ssh_hashalg ssh_sha512_hw = {
    .new = sha512_stub_new,
    .reset = sha512_stub_reset,
    .copyfrom = sha512_stub_copyfrom,
    .digest = sha512_stub_digest,
    .free = sha512_stub_free,
    .hlen = 64,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-512", "!NONEXISTENT ACCELERATED VERSION!"),
};

const ssh_hashalg ssh_sha384_hw = {
    .new = sha512_stub_new,
    .reset = sha512_stub_reset,
    .copyfrom = sha512_stub_copyfrom,
    .digest = sha512_stub_digest,
    .free = sha512_stub_free,
    .hlen = 48,
    .blocklen = 128,
    HASHALG_NAMES_ANNOTATED("SHA-384", "!NONEXISTENT ACCELERATED VERSION!"),
};

#endif /* HW_SHA512 */
