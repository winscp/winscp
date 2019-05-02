/*
 * sshaes.c - implementation of AES
 */

#include <assert.h>
#ifndef WINSCP_VS
#include <stdlib.h>
#endif

#include "ssh.h"
#include "mpint_i.h"               /* we reuse the BignumInt system */

/*
 * Start by deciding whether we can support hardware AES at all.
 */
#define HW_AES_NONE 0
#define HW_AES_NI 1

#ifdef _FORCE_AES_NI
#   define HW_AES HW_AES_NI
#elif defined(__clang__)
#   if __has_attribute(target) && __has_include(<wmmintrin.h>) &&       \
    (defined(__x86_64__) || defined(__i386))
#       define HW_AES HW_AES_NI
#   endif
#elif defined(__GNUC__)
#    if (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 4)) && \
    (defined(__x86_64__) || defined(__i386))
#       define HW_AES HW_AES_NI
#    endif
#elif defined (_MSC_VER)
#   if (defined(_M_X64) || defined(_M_IX86)) && _MSC_FULL_VER >= 150030729
#      define HW_AES HW_AES_NI
#   endif
#endif

#if defined _FORCE_SOFTWARE_AES || !defined HW_AES
#   undef HW_AES
#   define HW_AES HW_AES_NONE
#endif

#if HW_AES == HW_AES_NI
#define HW_NAME_SUFFIX " (AES-NI accelerated)"
#else
#define HW_NAME_SUFFIX " (!NONEXISTENT ACCELERATED VERSION!)"
#endif

/*
 * Vtable collection for AES. For each SSH-level cipher id (i.e.
 * combination of key length and cipher mode), we provide three
 * vtables: one for the pure software implementation, one using
 * hardware acceleration (if available), and a top-level one which is
 * never actually instantiated, and only contains a new() method whose
 * job is to decide whihc of the other two to return an actual
 * instance of.
 */

static ssh2_cipher *aes_select(const ssh2_cipheralg *alg);
static ssh2_cipher *aes_sw_new(const ssh2_cipheralg *alg);
static void aes_sw_free(ssh2_cipher *);
static void aes_sw_setiv_cbc(ssh2_cipher *, const void *iv);
static void aes_sw_setiv_sdctr(ssh2_cipher *, const void *iv);
static void aes_sw_setkey(ssh2_cipher *, const void *key);
static ssh2_cipher *aes_hw_new(const ssh2_cipheralg *alg);
static void aes_hw_free(ssh2_cipher *);
static void aes_hw_setiv_cbc(ssh2_cipher *, const void *iv);
static void aes_hw_setiv_sdctr(ssh2_cipher *, const void *iv);
static void aes_hw_setkey(ssh2_cipher *, const void *key);

struct aes_extra {
    const ssh2_cipheralg *sw, *hw;
};

#define VTABLES(cid, pid, bits, name, encsuffix, decsuffix, setiv)      \
    static void cid##_sw##encsuffix(ssh2_cipher *, void *blk, int len); \
    static void cid##_sw##decsuffix(ssh2_cipher *, void *blk, int len); \
    const ssh2_cipheralg ssh_##cid##_sw = {                             \
        aes_sw_new, aes_sw_free, aes_sw_##setiv, aes_sw_setkey,         \
        cid##_sw##encsuffix, cid##_sw##decsuffix, NULL, NULL,           \
        pid, 16, bits, bits/8, 0, name /*WINSCP " (unaccelerated)" */,  \
        NULL, NULL };                                                   \
                                                                        \
    static void cid##_hw##encsuffix(ssh2_cipher *, void *blk, int len); \
    static void cid##_hw##decsuffix(ssh2_cipher *, void *blk, int len); \
    const ssh2_cipheralg ssh_##cid##_hw = {                             \
        aes_hw_new, aes_hw_free, aes_hw_##setiv, aes_hw_setkey,         \
        cid##_hw##encsuffix, cid##_hw##decsuffix, NULL, NULL,           \
        pid, 16, bits, bits/8, 0, name HW_NAME_SUFFIX,                  \
        NULL, NULL };                                                   \
                                                                        \
    const struct aes_extra extra_##cid = {                              \
        &ssh_##cid##_sw, &ssh_##cid##_hw };                             \
                                                                        \
    const ssh2_cipheralg ssh_##cid = {                                  \
        aes_select, NULL, NULL, NULL, NULL, NULL, NULL, NULL,           \
        pid, 16, bits, bits/8, 0, name " (dummy selector vtable)",      \
        NULL, &extra_##cid };                                           \

VTABLES(aes128_cbc, "aes128", 128, "AES-128 CBC", _encrypt,_decrypt,setiv_cbc)
VTABLES(aes192_cbc, "aes192", 192, "AES-192 CBC", _encrypt,_decrypt,setiv_cbc)
VTABLES(aes256_cbc, "aes256", 256, "AES-256 CBC", _encrypt,_decrypt,setiv_cbc)
VTABLES(aes128_sdctr, "aes128-ctr", 128, "AES-128 SDCTR",,, setiv_sdctr)
VTABLES(aes192_sdctr, "aes192-ctr", 192, "AES-192 SDCTR",,, setiv_sdctr)
VTABLES(aes256_sdctr, "aes256-ctr", 256, "AES-256 SDCTR",,, setiv_sdctr)

static const ssh2_cipheralg ssh_rijndael_lysator = {
    /* Same as aes256_cbc, but with a different protocol ID */
    aes_select, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
    "rijndael-cbc@lysator.liu.se", 16, 256, 256/8, 0,
    "AES-256 CBC (dummy selector vtable)", NULL, &extra_aes256_cbc
};

static const ssh2_cipheralg *const aes_list[] = {
    &ssh_aes256_sdctr,
    &ssh_aes256_cbc,
    &ssh_rijndael_lysator,
    &ssh_aes192_sdctr,
    &ssh_aes192_cbc,
    &ssh_aes128_sdctr,
    &ssh_aes128_cbc,
};

const ssh2_ciphers ssh2_aes = { lenof(aes_list), aes_list };

/*
 * The actual query function that asks if hardware acceleration is
 * available.
 */
static bool aes_hw_available(void);

/*
 * The top-level selection function, caching the results of
 * aes_hw_available() so it only has to run once.
 */
static bool aes_hw_available_cached(void)
{
    static bool initialised = false;
    static bool hw_available;
    if (!initialised)
        hw_available = aes_hw_available();
    return hw_available;
}

static ssh2_cipher *aes_select(const ssh2_cipheralg *alg)
{
    const struct aes_extra *extra = (const struct aes_extra *)alg->extra;
    const ssh2_cipheralg *real_alg =
        aes_hw_available_cached() ? extra->hw : extra->sw;

    return ssh2_cipher_new(real_alg);
}

/* ----------------------------------------------------------------------
 * Definitions likely to be helpful to multiple implementations.
 */

#define REP2(x) x x
#define REP4(x) REP2(REP2(x))
#define REP8(x) REP2(REP4(x))
#define REP9(x) REP8(x) x
#define REP11(x) REP8(x) REP2(x) x
#define REP13(x) REP8(x) REP4(x) x

static const uint8_t key_setup_round_constants[] = {
    /* The first few powers of X in GF(2^8), used during key setup.
     * This can safely be a lookup table without side channel risks,
     * because key setup iterates through it once in a standard way
     * regardless of the key. */
    0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36,
};

#define MAXROUNDKEYS 15

/* ----------------------------------------------------------------------
 * Software implementation of AES.
 *
 * This implementation uses a bit-sliced representation. Instead of
 * the obvious approach of storing the cipher state so that each byte
 * (or field element, or entry in the cipher matrix) occupies 8
 * contiguous bits in a machine integer somewhere, we organise the
 * cipher state as an array of 8 integers, in such a way that each
 * logical byte of the cipher state occupies one bit in each integer,
 * all at the same position. This allows us to do parallel logic on
 * all bytes of the state by doing bitwise operations between the 8
 * integers; in particular, the S-box (SubBytes) lookup is done this
 * way, which takes about 110 operations - but for those 110 bitwise
 * ops you get 64 S-box lookups, not just one.
 */

#define SLICE_PARALLELISM (BIGNUM_INT_BYTES / 2)

#ifdef BITSLICED_DEBUG
/* Dump function that undoes the bitslicing transform, so you can see
 * the logical data represented by a set of slice words. */
static inline void dumpslices_uint16_t(
    const char *prefix, const uint16_t slices[8])
{
    printf("%-30s", prefix);
    for (unsigned byte = 0; byte < 16; byte++) {
        unsigned byteval = 0;
        for (unsigned bit = 0; bit < 8; bit++)
            byteval |= (1 & (slices[bit] >> byte)) << bit;
        printf("%02x", byteval);
    }
    printf("\n");
}

static inline void dumpslices_BignumInt(
    const char *prefix, const BignumInt slices[8])
{
    printf("%-30s", prefix);
    for (unsigned iter = 0; iter < SLICE_PARALLELISM; iter++) {
        for (unsigned byte = 0; byte < 16; byte++) {
            unsigned byteval = 0;
            for (unsigned bit = 0; bit < 8; bit++)
                byteval |= (1 & (slices[bit] >> (iter*16+byte))) << bit;
            printf("%02x", byteval);
        }
        if (iter+1 < SLICE_PARALLELISM)
            printf(" ");
    }
    printf("\n");
}
#else
#define dumpslices_uintN_t(prefix, slices) ((void)0)
#define dumpslices_BignumInt(prefix, slices) ((void)0)
#endif

/* -----
 * Bit-slicing transformation: convert between an array of 16 uint8_t
 * and an array of 8 uint16_t, so as to interchange the bit index
 * within each element and the element index within the array. (That
 * is, bit j of input[i] == bit i of output[j].
 */

#define SWAPWORDS(shift) do                                     \
    {                                                           \
        uint64_t mask = ~(uint64_t)0 / ((1ULL << shift) + 1);   \
        uint64_t diff = ((i0 >> shift) ^ i1) & mask;            \
        i0 ^= diff << shift;                                    \
        i1 ^= diff;                                             \
    } while (0)

#define SWAPINWORD(i, bigshift, smallshift) do                  \
    {                                                           \
        uint64_t mask = ~(uint64_t)0;                           \
        mask /= ((1ULL << bigshift) + 1);                       \
        mask /= ((1ULL << smallshift) + 1);                     \
        mask <<= smallshift;                                    \
        { /*WINSCP*/ \
        unsigned shift = bigshift - smallshift;                 \
        uint64_t diff = ((i >> shift) ^ i) & mask;              \
        i ^= diff ^ (diff << shift);                            \
        } /*WINSCP*/ \
    } while (0)

#define TO_BITSLICES(slices, bytes, uintN_t, assign_op, shift) do       \
    {                                                                   \
        uint64_t i0 = GET_64BIT_LSB_FIRST(bytes);                       \
        uint64_t i1 = GET_64BIT_LSB_FIRST(bytes + 8);                   \
        SWAPINWORD(i0, 8, 1);                                           \
        SWAPINWORD(i1, 8, 1);                                           \
        SWAPINWORD(i0, 16, 2);                                          \
        SWAPINWORD(i1, 16, 2);                                          \
        SWAPINWORD(i0, 32, 4);                                          \
        SWAPINWORD(i1, 32, 4);                                          \
        SWAPWORDS(8);                                                   \
        slices[0] assign_op (uintN_t)((i0 >>  0) & 0xFFFF) << (shift);  \
        slices[2] assign_op (uintN_t)((i0 >> 16) & 0xFFFF) << (shift);  \
        slices[4] assign_op (uintN_t)((i0 >> 32) & 0xFFFF) << (shift);  \
        slices[6] assign_op (uintN_t)((i0 >> 48) & 0xFFFF) << (shift);  \
        slices[1] assign_op (uintN_t)((i1 >>  0) & 0xFFFF) << (shift);  \
        slices[3] assign_op (uintN_t)((i1 >> 16) & 0xFFFF) << (shift);  \
        slices[5] assign_op (uintN_t)((i1 >> 32) & 0xFFFF) << (shift);  \
        slices[7] assign_op (uintN_t)((i1 >> 48) & 0xFFFF) << (shift);  \
    } while (0)

#define FROM_BITSLICES(bytes, slices, shift) do                 \
    {                                                           \
        uint64_t i1 = ((slices[7] >> (shift)) & 0xFFFF);        \
        i1 = (i1 << 16) | ((slices[5] >> (shift)) & 0xFFFF);    \
        i1 = (i1 << 16) | ((slices[3] >> (shift)) & 0xFFFF);    \
        i1 = (i1 << 16) | ((slices[1] >> (shift)) & 0xFFFF);    \
        { /*WINSCP*/ \
        uint64_t i0 = ((slices[6] >> (shift)) & 0xFFFF);        \
        i0 = (i0 << 16) | ((slices[4] >> (shift)) & 0xFFFF);    \
        i0 = (i0 << 16) | ((slices[2] >> (shift)) & 0xFFFF);    \
        i0 = (i0 << 16) | ((slices[0] >> (shift)) & 0xFFFF);    \
        SWAPWORDS(8);                                           \
        SWAPINWORD(i0, 32, 4);                                  \
        SWAPINWORD(i1, 32, 4);                                  \
        SWAPINWORD(i0, 16, 2);                                  \
        SWAPINWORD(i1, 16, 2);                                  \
        SWAPINWORD(i0, 8, 1);                                   \
        SWAPINWORD(i1, 8, 1);                                   \
        PUT_64BIT_LSB_FIRST(bytes, i0);                         \
        PUT_64BIT_LSB_FIRST((bytes) + 8, i1);                   \
        } /*WINSCP*/ \
    } while (0)

/* -----
 * Some macros that will be useful repeatedly.
 */

/* Iterate a unary transformation over all 8 slices. */
#define ITERATE(MACRO, output, input, uintN_t) do       \
    {                                                   \
        MACRO(output[0], input[0], uintN_t);            \
        MACRO(output[1], input[1], uintN_t);            \
        MACRO(output[2], input[2], uintN_t);            \
        MACRO(output[3], input[3], uintN_t);            \
        MACRO(output[4], input[4], uintN_t);            \
        MACRO(output[5], input[5], uintN_t);            \
        MACRO(output[6], input[6], uintN_t);            \
        MACRO(output[7], input[7], uintN_t);            \
    } while (0)

/* Simply add (i.e. XOR) two whole sets of slices together. */
#define BITSLICED_ADD(output, lhs, rhs) do      \
    {                                           \
        output[0] = lhs[0] ^ rhs[0];            \
        output[1] = lhs[1] ^ rhs[1];            \
        output[2] = lhs[2] ^ rhs[2];            \
        output[3] = lhs[3] ^ rhs[3];            \
        output[4] = lhs[4] ^ rhs[4];            \
        output[5] = lhs[5] ^ rhs[5];            \
        output[6] = lhs[6] ^ rhs[6];            \
        output[7] = lhs[7] ^ rhs[7];            \
    } while (0)

/* -----
 * The AES S-box, in pure bitwise logic so that it can be run in
 * parallel on whole words full of bit-sliced field elements.
 *
 * Source: 'A new combinational logic minimization technique with
 * applications to cryptology', https://eprint.iacr.org/2009/191
 */

/* Initial linear transformation for the forward S-box, from Fig 2 of
 * the paper. */
#define SBOX_FORWARD_TOP_TRANSFORM(input, uintN_t)      \
        uintN_t y14 = input[4] ^ input[2];              \
        uintN_t y13 = input[7] ^ input[1];              \
        uintN_t y9 = input[7] ^ input[4];               \
        uintN_t y8 = input[7] ^ input[2];               \
        uintN_t t0 = input[6] ^ input[5];               \
        uintN_t y1 = t0 ^ input[0];                     \
        uintN_t y4 = y1 ^ input[4];                     \
        uintN_t y12 = y13 ^ y14;                        \
        uintN_t y2 = y1 ^ input[7];                     \
        uintN_t y5 = y1 ^ input[1];                     \
        uintN_t y3 = y5 ^ y8;                           \
        uintN_t t1 = input[3] ^ y12;                    \
        uintN_t y15 = t1 ^ input[2];                    \
        uintN_t y20 = t1 ^ input[6];                    \
        uintN_t y6 = y15 ^ input[0];                    \
        uintN_t y10 = y15 ^ t0;                         \
        uintN_t y11 = y20 ^ y9;                         \
        uintN_t y7 = input[0] ^ y11;                    \
        uintN_t y17 = y10 ^ y11;                        \
        uintN_t y19 = y10 ^ y8;                         \
        uintN_t y16 = t0 ^ y11;                         \
        uintN_t y21 = y13 ^ y16;                        \
        uintN_t y18 = input[7] ^ y16;                   \
        /* Make a copy of input[0] under a new name, because the core
         * will refer to it, and in the inverse version of the S-box
         * the corresponding value will be one of the calculated ones
         * and not in input[0] itself. */               \
        uintN_t i0 = input[0];                          \
        /* end */

/* Core nonlinear component, from Fig 3 of the paper. */
#define SBOX_CORE(uintN_t)                              \
        uintN_t t2 = y12 & y15;                         \
        uintN_t t3 = y3 & y6;                           \
        uintN_t t4 = t3 ^ t2;                           \
        uintN_t t5 = y4 & i0;                           \
        uintN_t t6 = t5 ^ t2;                           \
        uintN_t t7 = y13 & y16;                         \
        uintN_t t8 = y5 & y1;                           \
        uintN_t t9 = t8 ^ t7;                           \
        uintN_t t10 = y2 & y7;                          \
        uintN_t t11 = t10 ^ t7;                         \
        uintN_t t12 = y9 & y11;                         \
        uintN_t t13 = y14 & y17;                        \
        uintN_t t14 = t13 ^ t12;                        \
        uintN_t t15 = y8 & y10;                         \
        uintN_t t16 = t15 ^ t12;                        \
        uintN_t t17 = t4 ^ t14;                         \
        uintN_t t18 = t6 ^ t16;                         \
        uintN_t t19 = t9 ^ t14;                         \
        uintN_t t20 = t11 ^ t16;                        \
        uintN_t t21 = t17 ^ y20;                        \
        uintN_t t22 = t18 ^ y19;                        \
        uintN_t t23 = t19 ^ y21;                        \
        uintN_t t24 = t20 ^ y18;                        \
        uintN_t t25 = t21 ^ t22;                        \
        uintN_t t26 = t21 & t23;                        \
        uintN_t t27 = t24 ^ t26;                        \
        uintN_t t28 = t25 & t27;                        \
        uintN_t t29 = t28 ^ t22;                        \
        uintN_t t30 = t23 ^ t24;                        \
        uintN_t t31 = t22 ^ t26;                        \
        uintN_t t32 = t31 & t30;                        \
        uintN_t t33 = t32 ^ t24;                        \
        uintN_t t34 = t23 ^ t33;                        \
        uintN_t t35 = t27 ^ t33;                        \
        uintN_t t36 = t24 & t35;                        \
        uintN_t t37 = t36 ^ t34;                        \
        uintN_t t38 = t27 ^ t36;                        \
        uintN_t t39 = t29 & t38;                        \
        uintN_t t40 = t25 ^ t39;                        \
        uintN_t t41 = t40 ^ t37;                        \
        uintN_t t42 = t29 ^ t33;                        \
        uintN_t t43 = t29 ^ t40;                        \
        uintN_t t44 = t33 ^ t37;                        \
        uintN_t t45 = t42 ^ t41;                        \
        uintN_t z0 = t44 & y15;                         \
        uintN_t z1 = t37 & y6;                          \
        uintN_t z2 = t33 & i0;                          \
        uintN_t z3 = t43 & y16;                         \
        uintN_t z4 = t40 & y1;                          \
        uintN_t z5 = t29 & y7;                          \
        uintN_t z6 = t42 & y11;                         \
        uintN_t z7 = t45 & y17;                         \
        uintN_t z8 = t41 & y10;                         \
        uintN_t z9 = t44 & y12;                         \
        uintN_t z10 = t37 & y3;                         \
        uintN_t z11 = t33 & y4;                         \
        uintN_t z12 = t43 & y13;                        \
        uintN_t z13 = t40 & y5;                         \
        uintN_t z14 = t29 & y2;                         \
        uintN_t z15 = t42 & y9;                         \
        uintN_t z16 = t45 & y14;                        \
        uintN_t z17 = t41 & y8;                         \
        /* end */

/* Final linear transformation for the forward S-box, from Fig 4 of
 * the paper. */
#define SBOX_FORWARD_BOTTOM_TRANSFORM(output, uintN_t)   \
        uintN_t t46 = z15 ^ z16;                        \
        uintN_t t47 = z10 ^ z11;                        \
        uintN_t t48 = z5 ^ z13;                         \
        uintN_t t49 = z9 ^ z10;                         \
        uintN_t t50 = z2 ^ z12;                         \
        uintN_t t51 = z2 ^ z5;                          \
        uintN_t t52 = z7 ^ z8;                          \
        uintN_t t53 = z0 ^ z3;                          \
        uintN_t t54 = z6 ^ z7;                          \
        uintN_t t55 = z16 ^ z17;                        \
        uintN_t t56 = z12 ^ t48;                        \
        uintN_t t57 = t50 ^ t53;                        \
        uintN_t t58 = z4 ^ t46;                         \
        uintN_t t59 = z3 ^ t54;                         \
        uintN_t t60 = t46 ^ t57;                        \
        uintN_t t61 = z14 ^ t57;                        \
        uintN_t t62 = t52 ^ t58;                        \
        uintN_t t63 = t49 ^ t58;                        \
        uintN_t t64 = z4 ^ t59;                         \
        uintN_t t65 = t61 ^ t62;                        \
        uintN_t t66 = z1 ^ t63;                         \
        output[7] = t59 ^ t63;                          \
        output[1] = ~(t56 ^ t62);                       \
        output[0] = ~(t48 ^ t60);                       \
        { /*WINSCP*/ \
        uintN_t t67 = t64 ^ t65;                        \
        output[4] = t53 ^ t66;                          \
        output[3] = t51 ^ t66;                          \
        output[2] = t47 ^ t65;                          \
        output[6] = ~(t64 ^ output[4]);                 \
        output[5] = ~(t55 ^ t67);                       \
        } /*WINSCP*/ \
        /* end */

#define BITSLICED_SUBBYTES(output, input, uintN_t) do { \
        SBOX_FORWARD_TOP_TRANSFORM(input, uintN_t);      \
        { /*WINSCP*/ \
        SBOX_CORE(uintN_t);                             \
        { /*WINSCP*/ \
        SBOX_FORWARD_BOTTOM_TRANSFORM(output, uintN_t);  \
        } /*WINSCP*/ \
        } /*WINSCP*/ \
    } while (0)

/*
 * Initial and final linear transformations for the backward S-box. I
 * generated these myself, by implementing the linear-transform
 * optimisation algorithm in the paper, and applying it to the
 * matrices calculated by _their_ top and bottom transformations, pre-
 * and post-multiplied as appropriate by the linear map in the inverse
 * S_box.
 */
#define SBOX_BACKWARD_TOP_TRANSFORM(input, uintN_t)     \
    /* Initial subtraction of the constant */           \
    uintN_t iv0 = ~input[0], iv1 = ~input[1];           \
    uintN_t iv5 = ~input[5], iv6 = ~input[6];           \
                                                        \
    uintN_t y5 = input[4] ^ iv6;                        \
    uintN_t y19 = input[3] ^ iv0;                       \
    uintN_t itmp8 = y5 ^ iv0;                           \
    uintN_t y4 = itmp8 ^ iv1;                           \
    uintN_t y9 = input[4] ^ input[3];                   \
    uintN_t y2 = y9 ^ y4;                               \
    uintN_t itmp9 = y2 ^ input[7];                      \
    uintN_t y1 = y9 ^ iv0;                              \
    uintN_t y6 = y5 ^ input[7];                         \
    uintN_t y18 = y9 ^ iv5;                             \
    uintN_t y7 = y18 ^ y2;                              \
    uintN_t y16 = y7 ^ y1;                              \
    uintN_t y21 = y7 ^ iv1;                             \
    uintN_t y3 = input[4] ^ input[7];                   \
    uintN_t y13 = y16 ^ y21;                            \
    uintN_t y8 = input[4] ^ y6;                         \
    uintN_t y10 = y8 ^ y19;                             \
    uintN_t y14 = y8 ^ y9;                              \
    uintN_t y20 = itmp9 ^ input[2];                     \
    uintN_t y11 = y9 ^ y20;                             \
    uintN_t i0 = y11 ^ y7;                              \
    uintN_t y15 = i0 ^ y6;                              \
    uintN_t y17 = y16 ^ y15;                            \
    uintN_t y12 = itmp9 ^ input[3];                     \
    /* end */
#define SBOX_BACKWARD_BOTTOM_TRANSFORM(output, uintN_t) \
    uintN_t otmp18 = z15 ^ z6;                          \
    uintN_t otmp19 = z13 ^ otmp18;                      \
    uintN_t otmp20 = z12 ^ otmp19;                      \
    uintN_t otmp21 = z16 ^ otmp20;                      \
    uintN_t otmp22 = z8 ^ otmp21;                       \
    uintN_t otmp23 = z0 ^ otmp22;                       \
    uintN_t otmp24 = otmp22 ^ z3;                       \
    uintN_t otmp25 = otmp24 ^ z4;                       \
    uintN_t otmp26 = otmp25 ^ z2;                       \
    uintN_t otmp27 = z1 ^ otmp26;                       \
    uintN_t otmp28 = z14 ^ otmp27;                      \
    uintN_t otmp29 = otmp28 ^ z10;                      \
    output[4] = z2 ^ otmp23;                            \
    output[7] = z5 ^ otmp24;                            \
    { /*WINSCP*/ \
    uintN_t otmp30 = z11 ^ otmp29;                      \
    output[5] = z13 ^ otmp30;                           \
    { /*WINSCP*/ \
    uintN_t otmp31 = otmp25 ^ z8;                       \
    output[1] = z7 ^ otmp31;                            \
    { /*WINSCP*/ \
    uintN_t otmp32 = z11 ^ z9;                          \
    uintN_t otmp33 = z17 ^ otmp32;                      \
    uintN_t otmp34 = otmp30 ^ otmp33;                   \
    output[0] = z15 ^ otmp33;                           \
    { /*WINSCP*/ \
    uintN_t otmp35 = z12 ^ otmp34;                      \
    output[6] = otmp35 ^ z16;                           \
    { /*WINSCP*/ \
    uintN_t otmp36 = z1 ^ otmp23;                       \
    uintN_t otmp37 = z5 ^ otmp36;                       \
    output[2] = z4 ^ otmp37;                            \
    { /*WINSCP*/ \
    uintN_t otmp38 = z11 ^ output[1];                   \
    uintN_t otmp39 = z2 ^ otmp38;                       \
    uintN_t otmp40 = z17 ^ otmp39;                      \
    uintN_t otmp41 = z0 ^ otmp40;                       \
    uintN_t otmp42 = z5 ^ otmp41;                       \
    uintN_t otmp43 = otmp42 ^ z10;                      \
    uintN_t otmp44 = otmp43 ^ z3;                       \
    output[3] = otmp44 ^ z16;                           \
    } /*WINSCP*/ \
    } /*WINSCP*/ \
    } /*WINSCP*/ \
    } /*WINSCP*/ \
    } /*WINSCP*/ \
    } /*WINSCP*/ \
    /* end */

#define BITSLICED_INVSUBBYTES(output, input, uintN_t) do {      \
        SBOX_BACKWARD_TOP_TRANSFORM(input, uintN_t);             \
        { /*WINSCP*/ \
        SBOX_CORE(uintN_t);                                     \
        { /*WINSCP*/ \
        SBOX_BACKWARD_BOTTOM_TRANSFORM(output, uintN_t);         \
        } /*WINSCP*/ \
        } /*WINSCP*/ \
    } while (0)


/* -----
 * The ShiftRows transformation. This operates independently on each
 * bit slice.
 */

#define SINGLE_BITSLICE_SHIFTROWS(output, input, uintN_t) do            \
    {                                                                   \
        uintN_t mask, mask2, mask3, diff, x = (input);                  \
        /* Rotate rows 2 and 3 by 16 bits */                            \
        mask = 0x00CC * (((uintN_t)~(uintN_t)0) / 0xFFFF);              \
        diff = ((x >> 8) ^ x) & mask;                                   \
        x ^= diff ^ (diff << 8);                                        \
        /* Rotate rows 1 and 3 by 8 bits */                             \
        mask  = 0x0AAA * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        mask2 = 0xA000 * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        mask3 = 0x5555 * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        x = ((x >> 4) & mask) | ((x << 12) & mask2) | (x & mask3);      \
        /* Write output */                                              \
        (output) = x;                                                   \
    } while (0)

#define SINGLE_BITSLICE_INVSHIFTROWS(output, input, uintN_t) do         \
    {                                                                   \
        uintN_t mask, mask2, mask3, diff, x = (input);                  \
        /* Rotate rows 2 and 3 by 16 bits */                            \
        mask = 0x00CC * (((uintN_t)~(uintN_t)0) / 0xFFFF);              \
        diff = ((x >> 8) ^ x) & mask;                                   \
        x ^= diff ^ (diff << 8);                                        \
        /* Rotate rows 1 and 3 by 8 bits, the opposite way to ShiftRows */ \
        mask  = 0x000A * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        mask2 = 0xAAA0 * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        mask3 = 0x5555 * (((uintN_t)~(uintN_t)0) / 0xFFFF);             \
        x = ((x >> 12) & mask) | ((x << 4) & mask2) | (x & mask3);      \
        /* Write output */                                              \
        (output) = x;                                                   \
    } while (0)

#define BITSLICED_SHIFTROWS(output, input, uintN_t) do                  \
    {                                                                   \
        ITERATE(SINGLE_BITSLICE_SHIFTROWS, output, input, uintN_t);     \
    } while (0)

#define BITSLICED_INVSHIFTROWS(output, input, uintN_t) do               \
    {                                                                   \
        ITERATE(SINGLE_BITSLICE_INVSHIFTROWS, output, input, uintN_t);  \
    } while (0)

/* -----
 * The MixColumns transformation. This has to operate on all eight bit
 * slices at once, and also passes data back and forth between the
 * bits in an adjacent group of 4 within each slice.
 *
 * Notation: let F = GF(2)[X]/<X^8+X^4+X^3+X+1> be the finite field
 * used in AES, and let R = F[Y]/<Y^4+1> be the ring whose elements
 * represent the possible contents of a column of the matrix. I use X
 * and Y below in those senses, i.e. X is the value in F that
 * represents the byte 0x02, and Y is the value in R that cycles the
 * four bytes around by one if you multiply by it.
 */

/* Multiply every column by Y^3, i.e. cycle it round one place to the
 * right. Operates on one bit slice at a time; you have to wrap it in
 * ITERATE to affect all the data at once. */
#define BITSLICED_MUL_BY_Y3(output, input, uintN_t) do          \
    {                                                           \
        uintN_t mask, mask2, x;                                 \
        mask  = 0x8 * (((uintN_t)~(uintN_t)0) / 0xF);           \
        mask2 = 0x7 * (((uintN_t)~(uintN_t)0) / 0xF);           \
        x = input;                                              \
        output = ((x << 3) & mask) ^ ((x >> 1) & mask2);        \
    } while (0)

/* Multiply every column by Y^2. */
#define BITSLICED_MUL_BY_Y2(output, input, uintN_t) do          \
    {                                                           \
        uintN_t mask, mask2, x;                                 \
        mask  = 0xC * (((uintN_t)~(uintN_t)0) / 0xF);           \
        mask2 = 0x3 * (((uintN_t)~(uintN_t)0) / 0xF);           \
        x = input;                                              \
        output = ((x << 2) & mask) ^ ((x >> 2) & mask2);        \
    } while (0)

#define BITSLICED_MUL_BY_1_Y3(output, input, uintN_t) do        \
    {                                                           \
        uintN_t tmp = input;                                    \
        BITSLICED_MUL_BY_Y3(tmp, input, uintN_t);               \
        output = input ^ tmp;                                   \
    } while (0)

/* Multiply every column by 1+Y^2. */
#define BITSLICED_MUL_BY_1_Y2(output, input, uintN_t) do        \
    {                                                           \
        uintN_t tmp = input;                                    \
        BITSLICED_MUL_BY_Y2(tmp, input, uintN_t);               \
        output = input ^ tmp;                                   \
    } while (0)

/* Multiply every field element by X. This has to feed data between
 * slices, so it does the whole job in one go without needing ITERATE. */
#define BITSLICED_MUL_BY_X(output, input, uintN_t) do   \
    {                                                   \
        uintN_t bit7 = input[7];                        \
        output[7] = input[6];                           \
        output[6] = input[5];                           \
        output[5] = input[4];                           \
        output[4] = input[3] ^ bit7;                    \
        output[3] = input[2] ^ bit7;                    \
        output[2] = input[1];                           \
        output[1] = input[0] ^ bit7;                    \
        output[0] =            bit7;                    \
    } while (0)

/*
 * The MixColumns constant is
 *   M = X + Y + Y^2 + (X+1)Y^3
 * which we construct by rearranging it into
 *   M = 1 + (1+Y^3) [ X + (1+Y^2) ]
 */
#define BITSLICED_MIXCOLUMNS(output, input, uintN_t) do         \
    {                                                           \
        uintN_t a[8], aX[8], b[8];                              \
        /* a = input * (1+Y^3) */                               \
        ITERATE(BITSLICED_MUL_BY_1_Y3, a, input, uintN_t);      \
        /* aX = a * X */                                        \
        BITSLICED_MUL_BY_X(aX, a, uintN_t);                     \
        /* b = a * (1+Y^2) = input * (1+Y+Y^2+Y^3) */           \
        ITERATE(BITSLICED_MUL_BY_1_Y2, b, a, uintN_t);          \
        /* output = input + aX + b (reusing a as a temp */      \
        BITSLICED_ADD(a, aX, b);                                \
        BITSLICED_ADD(output, input, a);                        \
    } while (0)

/*
 * The InvMixColumns constant, written out longhand, is
 *   I = (X^3+X^2+X) + (X^3+1)Y + (X^3+X^2+1)Y^2 + (X^3+X+1)Y^3
 * We represent this as
 *   I = (X^3+X^2+X+1)(Y^3+Y^2+Y+1) + 1 + X(Y+Y^2) + X^2(Y+Y^3)
 */
#define BITSLICED_INVMIXCOLUMNS(output, input, uintN_t) do      \
    {                                                           \
        /* We need input * X^i for i=1,...,3 */                 \
        uintN_t X[8], X2[8], X3[8];                             \
        BITSLICED_MUL_BY_X(X, input, uintN_t);                  \
        BITSLICED_MUL_BY_X(X2, X, uintN_t);                     \
        BITSLICED_MUL_BY_X(X3, X2, uintN_t);                    \
        /* Sum them all and multiply by 1+Y+Y^2+Y^3. */         \
        { /*WINSCP*/ \
        uintN_t S[8];                                           \
        BITSLICED_ADD(S, input, X);                             \
        BITSLICED_ADD(S, S, X2);                                \
        BITSLICED_ADD(S, S, X3);                                \
        ITERATE(BITSLICED_MUL_BY_1_Y3, S, S, uintN_t);          \
        ITERATE(BITSLICED_MUL_BY_1_Y2, S, S, uintN_t);          \
        /* Compute the X(Y+Y^2) term. */                        \
        { /*WINSCP*/ \
        uintN_t A[8];                                           \
        ITERATE(BITSLICED_MUL_BY_1_Y3, A, X, uintN_t);          \
        ITERATE(BITSLICED_MUL_BY_Y2, A, A, uintN_t);            \
        /* Compute the X^2(Y+Y^3) term. */                      \
        { /*WINSCP*/ \
        uintN_t B[8];                                           \
        ITERATE(BITSLICED_MUL_BY_1_Y2, B, X2, uintN_t);         \
        ITERATE(BITSLICED_MUL_BY_Y3, B, B, uintN_t);            \
        /* And add all the pieces together. */                  \
        BITSLICED_ADD(S, S, input);                             \
        BITSLICED_ADD(S, S, A);                                 \
        BITSLICED_ADD(output, S, B);                            \
        } /*WINSCP*/ \
        } /*WINSCP*/ \
        } /*WINSCP*/ \
    } while (0)

/* -----
 * Put it all together into a cipher round.
 */

/* Dummy macro to get rid of the MixColumns in the final round. */
#define NO_MIXCOLUMNS(out, in, uintN_t) do {} while (0)

#define ENCRYPT_ROUND_FN(suffix, uintN_t, mixcol_macro)                 \
    static void aes_sliced_round_e_##suffix(                            \
        uintN_t output[8], const uintN_t input[8], const uintN_t roundkey[8]) \
    {                                                                   \
        BITSLICED_SUBBYTES(output, input, uintN_t);                     \
        BITSLICED_SHIFTROWS(output, output, uintN_t);                   \
        mixcol_macro(output, output, uintN_t);                          \
        BITSLICED_ADD(output, output, roundkey);                        \
    }

ENCRYPT_ROUND_FN(serial, uint16_t, BITSLICED_MIXCOLUMNS)
ENCRYPT_ROUND_FN(serial_last, uint16_t, NO_MIXCOLUMNS)
ENCRYPT_ROUND_FN(parallel, BignumInt, BITSLICED_MIXCOLUMNS)
ENCRYPT_ROUND_FN(parallel_last, BignumInt, NO_MIXCOLUMNS)

#define DECRYPT_ROUND_FN(suffix, uintN_t, mixcol_macro)                 \
    static void aes_sliced_round_d_##suffix(                            \
        uintN_t output[8], const uintN_t input[8], const uintN_t roundkey[8]) \
    {                                                                   \
        BITSLICED_ADD(output, input, roundkey);                         \
        mixcol_macro(output, output, uintN_t);                          \
        BITSLICED_INVSUBBYTES(output, output, uintN_t);                 \
        BITSLICED_INVSHIFTROWS(output, output, uintN_t);                \
    }

#if 0 /* no cipher mode we support requires serial decryption */
DECRYPT_ROUND_FN(serial, uint16_t, BITSLICED_INVMIXCOLUMNS)
DECRYPT_ROUND_FN(serial_first, uint16_t, NO_MIXCOLUMNS)
#endif
DECRYPT_ROUND_FN(parallel, BignumInt, BITSLICED_INVMIXCOLUMNS)
DECRYPT_ROUND_FN(parallel_first, BignumInt, NO_MIXCOLUMNS)

/* -----
 * Key setup function.
 */

typedef struct aes_sliced_key aes_sliced_key;
struct aes_sliced_key {
    BignumInt roundkeys_parallel[MAXROUNDKEYS * 8];
    uint16_t roundkeys_serial[MAXROUNDKEYS * 8];
    unsigned rounds;
};

static void aes_sliced_key_setup(
    aes_sliced_key *sk, const void *vkey, size_t keybits)
{
    const unsigned char *key = (const unsigned char *)vkey;

    size_t key_words = keybits / 32;
    sk->rounds = key_words + 6;
    { // WINSCP
    size_t sched_words = (sk->rounds + 1) * 4;

    unsigned rconpos = 0;

    uint16_t *outslices = sk->roundkeys_serial;
    unsigned outshift = 0;

    memset(sk->roundkeys_serial, 0, sizeof(sk->roundkeys_serial));

    { // WINSCP
    uint8_t inblk[16];
    memset(inblk, 0, 16);
    { // WINSCP
    uint16_t slices[8];

    size_t i; // WINSCP
    for (i = 0; i < sched_words; i++) {
        /*
         * Prepare a word of round key in the low 4 bits of each
         * integer in slices[].
         */
	if (i < key_words) {
            memcpy(inblk, key + 4*i, 4);
            TO_BITSLICES(slices, inblk, uint16_t, =, 0);
        } else {
            unsigned wordindex, bitshift;
            uint16_t *prevslices;
            size_t i2; // WINSCP

            /* Fetch the (i-1)th key word */
            wordindex = i-1;
            bitshift = 4 * (wordindex & 3);
            prevslices = sk->roundkeys_serial + 8 * (wordindex >> 2);
            for (i2 = 0; i2 < 8; i2++)
                slices[i2] = prevslices[i2] >> bitshift;

            /* Decide what we're doing in this expansion stage */
            { // WINSCP
            bool rotate_and_round_constant = (i % key_words == 0);
            bool sub = rotate_and_round_constant ||
                (key_words == 8 && i % 8 == 4);

            if (rotate_and_round_constant) {
                for (i2 = 0; i2 < 8; i2++)
                    slices[i2] = ((slices[i2] << 3) | (slices[i2] >> 1)) & 0xF;
            }

            if (sub) {
                BITSLICED_SUBBYTES(slices, slices, uint16_t);
            }

            if (rotate_and_round_constant) {
                pinitassert(rconpos < lenof(key_setup_round_constants));
                uint8_t rcon = key_setup_round_constants[rconpos++];
                for (i2 = 0; i2 < 8; i2++)
                    slices[i2] ^= 1 & (rcon >> i2);
            }

            /* Combine with the (i-Nk)th key word */
            wordindex = i - key_words;
            bitshift = 4 * (wordindex & 3);
            prevslices = sk->roundkeys_serial + 8 * (wordindex >> 2);
            { // WINSCP
            for (i2 = 0; i2 < 8; i2++)
                slices[i2] ^= prevslices[i2] >> bitshift;
            } // WINSCP
            } // WINSCP
	}

        /*
         * Now copy it into sk.
         */
        { // WINSCP
        unsigned b;
        for (b = 0; b < 8; b++)
            outslices[b] |= (slices[b] & 0xF) << outshift;
        } // WINSCP
        outshift += 4;
        if (outshift == 16) {
            outshift = 0;
            outslices += 8;
        }
    }

    smemclr(inblk, sizeof(inblk));
    smemclr(slices, sizeof(slices));

    /*
     * Replicate that set of round keys into larger integers for the
     * parallel versions of the cipher.
     */
    for (i = 0; i < 8 * (sched_words / 4); i++) {
        sk->roundkeys_parallel[i] = sk->roundkeys_serial[i] *
            ((BignumInt)~(BignumInt)0 / 0xFFFF);
    }
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/* -----
 * The full cipher primitive, including transforming the input and
 * output to/from bit-sliced form.
 */

#define ENCRYPT_FN(suffix, uintN_t, nblocks)                            \
    static void aes_sliced_e_##suffix(                                  \
        uint8_t *output, const uint8_t *input, const aes_sliced_key *sk) \
    {                                                                   \
        uintN_t state[8];                                               \
        unsigned i; /*WINSCP*/ \
        TO_BITSLICES(state, input, uintN_t, =, 0);                      \
        for (i = 1; i < nblocks; i++) {                        \
            input += 16;                                                \
            TO_BITSLICES(state, input, uintN_t, |=, i*16);              \
        }                                                               \
        { /*WINSCP*/ \
        const uintN_t *keys = sk->roundkeys_##suffix;                   \
        BITSLICED_ADD(state, state, keys);                              \
        keys += 8;                                                      \
        for (i = 0; i < sk->rounds-1; i++) {                   \
            aes_sliced_round_e_##suffix(state, state, keys);            \
            keys += 8;                                                  \
        }                                                               \
        aes_sliced_round_e_##suffix##_last(state, state, keys);         \
        for (i = 0; i < nblocks; i++) {                        \
            FROM_BITSLICES(output, state, i*16);                        \
            output += 16;                                               \
        }                                                               \
        } /*WINSCP*/ \
    }

#define DECRYPT_FN(suffix, uintN_t, nblocks)                            \
    static void aes_sliced_d_##suffix(                                  \
        uint8_t *output, const uint8_t *input, const aes_sliced_key *sk) \
    {                                                                   \
        uintN_t state[8];                                               \
        unsigned i; /*WINSCP*/ \
        TO_BITSLICES(state, input, uintN_t, =, 0);                      \
        for (i = 1; i < nblocks; i++) {                        \
            input += 16;                                                \
            TO_BITSLICES(state, input, uintN_t, |=, i*16);              \
        }                                                               \
        { /*WINSCP*/ \
        const uintN_t *keys = sk->roundkeys_##suffix + 8*sk->rounds;    \
        aes_sliced_round_d_##suffix##_first(state, state, keys);        \
        keys -= 8;                                                      \
        for (i = 0; i < sk->rounds-1; i++) {                   \
            aes_sliced_round_d_##suffix(state, state, keys);            \
            keys -= 8;                                                  \
        }                                                               \
        BITSLICED_ADD(state, state, keys);                              \
        for (i = 0; i < nblocks; i++) {                        \
            FROM_BITSLICES(output, state, i*16);                        \
            output += 16;                                               \
        }                                                               \
        } /*WINSCP*/ \
    }

ENCRYPT_FN(serial, uint16_t, 1)
#if 0 /* no cipher mode we support requires serial decryption */
DECRYPT_FN(serial, uint16_t, 1)
#endif
ENCRYPT_FN(parallel, BignumInt, SLICE_PARALLELISM)
DECRYPT_FN(parallel, BignumInt, SLICE_PARALLELISM)

/* -----
 * The SSH interface and the cipher modes.
 */

#define SDCTR_WORDS (16 / BIGNUM_INT_BYTES)

typedef struct aes_sw_context aes_sw_context;
struct aes_sw_context {
    aes_sliced_key sk;
    union {
        struct {
            /* In CBC mode, the IV is just a copy of the last seen
             * cipher block. */
            uint8_t prevblk[16];
        } cbc;
        struct {
            /* In SDCTR mode, we keep the counter itself in a form
             * that's easy to increment. We also use the parallel
             * version of the core AES function, so we'll encrypt
             * multiple counter values in one go. That won't align
             * nicely with the sizes of data we're asked to encrypt,
             * so we must also store a cache of the last set of
             * keystream blocks we generated, and our current position
             * within that cache. */
            BignumInt counter[SDCTR_WORDS];
            uint8_t keystream[SLICE_PARALLELISM * 16];
            uint8_t *keystream_pos;
        } sdctr;
    } iv;
    ssh2_cipher ciph;
};

static ssh2_cipher *aes_sw_new(const ssh2_cipheralg *alg)
{
    aes_sw_context *ctx = snew(aes_sw_context);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void aes_sw_free(ssh2_cipher *ciph)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void aes_sw_setkey(ssh2_cipher *ciph, const void *vkey)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);
    aes_sliced_key_setup(&ctx->sk, vkey, ctx->ciph.vt->real_keybits);
}

static void aes_sw_setiv_cbc(ssh2_cipher *ciph, const void *iv)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);
    memcpy(ctx->iv.cbc.prevblk, iv, 16);
}

static void aes_sw_setiv_sdctr(ssh2_cipher *ciph, const void *viv)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);
    const uint8_t *iv = (const uint8_t *)viv;

    /* Import the initial counter value into the internal representation */
    unsigned i; // WINSCP
    for (i = 0; i < SDCTR_WORDS; i++)
        ctx->iv.sdctr.counter[i] =
            GET_BIGNUMINT_MSB_FIRST(
                iv + 16 - BIGNUM_INT_BYTES - i*BIGNUM_INT_BYTES);

    /* Set keystream_pos to indicate that the keystream cache is
     * currently empty */
    ctx->iv.sdctr.keystream_pos =
        ctx->iv.sdctr.keystream + sizeof(ctx->iv.sdctr.keystream);
}

typedef void (*aes_sw_fn)(uint32_t v[4], const uint32_t *keysched);

static inline void memxor16(void *vout, const void *vlhs, const void *vrhs)
{
    uint8_t *out = (uint8_t *)vout;
    const uint8_t *lhs = (const uint8_t *)vlhs, *rhs = (const uint8_t *)vrhs;
    uint64_t w;

    w = GET_64BIT_LSB_FIRST(lhs);
    w ^= GET_64BIT_LSB_FIRST(rhs);
    PUT_64BIT_LSB_FIRST(out, w);
    w = GET_64BIT_LSB_FIRST(lhs + 8);
    w ^= GET_64BIT_LSB_FIRST(rhs + 8);
    PUT_64BIT_LSB_FIRST(out + 8, w);
}

static inline void aes_cbc_sw_encrypt(
    ssh2_cipher *ciph, void *vblk, int blklen)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);

    /*
     * CBC encryption has to be done serially, because the input to
     * each run of the cipher includes the output from the previous
     * run.
     */

    uint8_t *blk, *finish; // WINSCP
    for (blk = (uint8_t *)vblk, finish = blk + blklen;
         blk < finish; blk += 16) {
        /*
         * We use the IV array itself as the location for the
         * encryption, because there's no reason not to.
         */

        /* XOR the new plaintext block into the previous cipher block */
        memxor16(ctx->iv.cbc.prevblk, ctx->iv.cbc.prevblk, blk);

        /* Run the cipher over the result, which leaves it
         * conveniently already stored in ctx->iv */
        aes_sliced_e_serial(
            ctx->iv.cbc.prevblk, ctx->iv.cbc.prevblk, &ctx->sk);

        /* Copy it to the output location */
        memcpy(blk, ctx->iv.cbc.prevblk, 16);
    }
}

static inline void aes_cbc_sw_decrypt(
    ssh2_cipher *ciph, void *vblk, int blklen)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);
    uint8_t *blk = (uint8_t *)vblk;

    /*
     * CBC decryption can run in parallel, because all the
     * _ciphertext_ blocks are already available.
     */

    size_t blocks_remaining = blklen / 16;

    uint8_t data[SLICE_PARALLELISM * 16];
    /* Zeroing the data array is probably overcautious, but it avoids
     * technically undefined behaviour from leaving it uninitialised
     * if our very first iteration doesn't include enough cipher
     * blocks to populate it fully */
    memset(data, 0, sizeof(data));

    while (blocks_remaining > 0) {
        /* Number of blocks we'll handle in this iteration. If we're
         * dealing with fewer than the maximum, it doesn't matter -
         * it's harmless to run the full parallel cipher function
         * anyway. */
        size_t blocks = (blocks_remaining < SLICE_PARALLELISM ?
                         blocks_remaining : SLICE_PARALLELISM);

        /* Parallel-decrypt the input, in a separate array so we still
         * have the cipher stream available for XORing. */
        memcpy(data, blk, 16 * blocks);
        aes_sliced_d_parallel(data, data, &ctx->sk);

        /* Write the output and update the IV */
        { // WINSCP
        size_t i; // WINSCP
        for (i = 0; i < blocks; i++) {
            uint8_t *decrypted = data + 16*i;
            uint8_t *output = blk + 16*i;

            memxor16(decrypted, decrypted, ctx->iv.cbc.prevblk);
            memcpy(ctx->iv.cbc.prevblk, output, 16);
            memcpy(output, decrypted, 16);
        }

        /* Advance the input pointer. */
        blk += 16 * blocks;
        blocks_remaining -= blocks;
        } // WINSCP
    }

    smemclr(data, sizeof(data));
}

static inline void aes_sdctr_sw(
    ssh2_cipher *ciph, void *vblk, int blklen)
{
    aes_sw_context *ctx = container_of(ciph, aes_sw_context, ciph);

    /*
     * SDCTR encrypt/decrypt loops round one block at a time XORing
     * the keystream into the user's data, and periodically has to run
     * a parallel encryption operation to get more keystream.
     */

    uint8_t *keystream_end =
        ctx->iv.sdctr.keystream + sizeof(ctx->iv.sdctr.keystream);

    uint8_t *blk, *finish; // WINSCP
    for (blk = (uint8_t *)vblk, finish = blk + blklen;
         blk < finish; blk += 16) {

        if (ctx->iv.sdctr.keystream_pos == keystream_end) {
            /*
             * Generate some keystream.
             */
            uint8_t *block; // WINSCP
            for (block = ctx->iv.sdctr.keystream;
                 block < keystream_end; block += 16) {
                /* Format the counter value into the buffer. */
                unsigned i; // WINSCP
                for (i = 0; i < SDCTR_WORDS; i++)
                    PUT_BIGNUMINT_MSB_FIRST(
                        block + 16 - BIGNUM_INT_BYTES - i*BIGNUM_INT_BYTES,
                        ctx->iv.sdctr.counter[i]);

                /* Increment the counter. */
                { // WINSCP
                BignumCarry carry = 1;
                for (i = 0; i < SDCTR_WORDS; i++)
                    BignumADC(ctx->iv.sdctr.counter[i], carry,
                              ctx->iv.sdctr.counter[i], 0, carry);
                } // WINSCP
            }

            /* Encrypt all those counter blocks. */
            aes_sliced_e_parallel(ctx->iv.sdctr.keystream,
                                  ctx->iv.sdctr.keystream, &ctx->sk);

            /* Reset keystream_pos to the start of the buffer. */
            ctx->iv.sdctr.keystream_pos = ctx->iv.sdctr.keystream;
        }

        memxor16(blk, blk, ctx->iv.sdctr.keystream_pos);
        ctx->iv.sdctr.keystream_pos += 16;
    }
}

#define SW_ENC_DEC(len)                                 \
    static void aes##len##_cbc_sw_encrypt(              \
        ssh2_cipher *ciph, void *vblk, int blklen)      \
    { aes_cbc_sw_encrypt(ciph, vblk, blklen); }         \
    static void aes##len##_cbc_sw_decrypt(              \
        ssh2_cipher *ciph, void *vblk, int blklen)      \
    { aes_cbc_sw_decrypt(ciph, vblk, blklen); }         \
    static void aes##len##_sdctr_sw(                    \
        ssh2_cipher *ciph, void *vblk, int blklen)      \
    { aes_sdctr_sw(ciph, vblk, blklen); }

SW_ENC_DEC(128)
SW_ENC_DEC(192)
SW_ENC_DEC(256)

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of AES using x86 AES-NI.
 */

#if HW_AES == HW_AES_NI

/*
 * Set target architecture for Clang and GCC
 */
#if !defined(__clang__) && defined(__GNUC__)
#    pragma GCC target("aes")
#    pragma GCC target("sse4.1")
#endif

#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 8)))
#    define FUNC_ISA __attribute__ ((target("sse4.1,aes")))
#else
#    define FUNC_ISA
#endif

#include <wmmintrin.h>
#include <smmintrin.h>

#if defined(__clang__) || defined(__GNUC__)
#include <cpuid.h>
#define GET_CPU_ID(out) __cpuid(1, (out)[0], (out)[1], (out)[2], (out)[3])
#else
#define GET_CPU_ID(out) __cpuid(out, 1)
#endif

bool aes_hw_available(void)
{
    /*
     * Determine if AES is available on this CPU, by checking that
     * both AES itself and SSE4.1 are supported.
     */
    unsigned int CPUInfo[4];
    GET_CPU_ID(CPUInfo);
    return (CPUInfo[2] & (1 << 25)) && (CPUInfo[2] & (1 << 19));
}

/*
 * Core AES-NI encrypt/decrypt functions, one per length and direction.
 */

#define NI_CIPHER(len, dir, dirlong, repmacro)                          \
    static FUNC_ISA inline __m128i aes_ni_##len##_##dir(                \
        __m128i v, const __m128i *keysched)                             \
    {                                                                   \
        v = _mm_xor_si128(v, *keysched++);                              \
        repmacro(v = _mm_aes##dirlong##_si128(v, *keysched++););        \
        return _mm_aes##dirlong##last_si128(v, *keysched);              \
    }

NI_CIPHER(128, e, enc, REP9)
NI_CIPHER(128, d, dec, REP9)
NI_CIPHER(192, e, enc, REP11)
NI_CIPHER(192, d, dec, REP11)
NI_CIPHER(256, e, enc, REP13)
NI_CIPHER(256, d, dec, REP13)

/*
 * The main key expansion.
 */
static FUNC_ISA void aes_ni_key_expand(
    const unsigned char *key, size_t key_words,
    __m128i *keysched_e, __m128i *keysched_d)
{
    size_t rounds = key_words + 6;
    size_t sched_words = (rounds + 1) * 4;

    /*
     * Store the key schedule as 32-bit integers during expansion, so
     * that it's easy to refer back to individual previous words. We
     * collect them into the final __m128i form at the end.
     */
    uint32_t sched[MAXROUNDKEYS * 4];

    unsigned rconpos = 0;

    for (size_t i = 0; i < sched_words; i++) {
	if (i < key_words) {
            sched[i] = GET_32BIT_LSB_FIRST(key + 4 * i);
        } else {
	    uint32_t temp = sched[i - 1];

            bool rotate_and_round_constant = (i % key_words == 0);
            bool only_sub = (key_words == 8 && i % 8 == 4);

            if (rotate_and_round_constant) {
                __m128i v = _mm_setr_epi32(0,temp,0,0);
                v = _mm_aeskeygenassist_si128(v, 0);
                temp = _mm_extract_epi32(v, 1);

                assert(rconpos < lenof(key_setup_round_constants));
                temp ^= key_setup_round_constants[rconpos++];
            } else if (only_sub) {
                __m128i v = _mm_setr_epi32(0,temp,0,0);
                v = _mm_aeskeygenassist_si128(v, 0);
                temp = _mm_extract_epi32(v, 0);
            }

            sched[i] = sched[i - key_words] ^ temp;
	}
    }

    /*
     * Combine the key schedule words into __m128i vectors and store
     * them in the output context.
     */
    for (size_t round = 0; round <= rounds; round++)
        keysched_e[round] = _mm_setr_epi32(
            sched[4*round  ], sched[4*round+1],
            sched[4*round+2], sched[4*round+3]);

    smemclr(sched, sizeof(sched));

    /*
     * Now prepare the modified keys for the inverse cipher.
     */
    for (size_t eround = 0; eround <= rounds; eround++) {
        size_t dround = rounds - eround;
        __m128i rkey = keysched_e[eround];
        if (eround && dround)      /* neither first nor last */
            rkey = _mm_aesimc_si128(rkey);
        keysched_d[dround] = rkey;
    }
}

/*
 * Auxiliary routine to increment the 128-bit counter used in SDCTR
 * mode.
 */
static FUNC_ISA inline __m128i aes_ni_sdctr_increment(__m128i v)
{
    const __m128i ONE  = _mm_setr_epi32(1,0,0,0);
    const __m128i ZERO = _mm_setzero_si128();

    /* Increment the low-order 64 bits of v */
    v  = _mm_add_epi64(v, ONE);
    /* Check if they've become zero */
    __m128i cmp = _mm_cmpeq_epi64(v, ZERO);
    /* If so, the low half of cmp is all 1s. Pack that into the high
     * half of addend with zero in the low half. */
    __m128i addend = _mm_unpacklo_epi64(ZERO, cmp);
    /* And subtract that from v, which increments the high 64 bits iff
     * the low 64 wrapped round. */
    v = _mm_sub_epi64(v, addend);

    return v;
}

/*
 * Auxiliary routine to reverse the byte order of a vector, so that
 * the SDCTR IV can be made big-endian for feeding to the cipher.
 */
static FUNC_ISA inline __m128i aes_ni_sdctr_reverse(__m128i v)
{
    v = _mm_shuffle_epi8(
        v, _mm_setr_epi8(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0));
    return v;
}

/*
 * The SSH interface and the cipher modes.
 */

typedef struct aes_ni_context aes_ni_context;
struct aes_ni_context {
    __m128i keysched_e[MAXROUNDKEYS], keysched_d[MAXROUNDKEYS], iv;

    ssh2_cipher ciph;
};

static ssh2_cipher *aes_hw_new(const ssh2_cipheralg *alg)
{
    if (!aes_hw_available_cached())
        return NULL;

    aes_ni_context *ctx = snew(aes_ni_context);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void aes_hw_free(ssh2_cipher *ciph)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void aes_hw_setkey(ssh2_cipher *ciph, const void *vkey)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    const unsigned char *key = (const unsigned char *)vkey;

    aes_ni_key_expand(key, ctx->ciph.vt->real_keybits / 32,
                      ctx->keysched_e, ctx->keysched_d);
}

static FUNC_ISA void aes_hw_setiv_cbc(ssh2_cipher *ciph, const void *iv)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    ctx->iv = _mm_loadu_si128(iv);
}

static FUNC_ISA void aes_hw_setiv_sdctr(ssh2_cipher *ciph, const void *iv)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    __m128i counter = _mm_loadu_si128(iv);
    ctx->iv = aes_ni_sdctr_reverse(counter);
}

typedef __m128i (*aes_ni_fn)(__m128i v, const __m128i *keysched);

static FUNC_ISA inline void aes_cbc_ni_encrypt(
    ssh2_cipher *ciph, void *vblk, int blklen, aes_ni_fn encrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i plaintext = _mm_loadu_si128((const __m128i *)blk);
        __m128i cipher_input = _mm_xor_si128(plaintext, ctx->iv);
        __m128i ciphertext = encrypt(cipher_input, ctx->keysched_e);
        _mm_storeu_si128((__m128i *)blk, ciphertext);
        ctx->iv = ciphertext;
    }
}

static FUNC_ISA inline void aes_cbc_ni_decrypt(
    ssh2_cipher *ciph, void *vblk, int blklen, aes_ni_fn decrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i ciphertext = _mm_loadu_si128((const __m128i *)blk);
        __m128i decrypted = decrypt(ciphertext, ctx->keysched_d);
        __m128i plaintext = _mm_xor_si128(decrypted, ctx->iv);
        _mm_storeu_si128((__m128i *)blk, plaintext);
        ctx->iv = ciphertext;
    }
}

static FUNC_ISA inline void aes_sdctr_ni(
    ssh2_cipher *ciph, void *vblk, int blklen, aes_ni_fn encrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i counter = aes_ni_sdctr_reverse(ctx->iv);
        __m128i keystream = encrypt(counter, ctx->keysched_e);
        __m128i input = _mm_loadu_si128((const __m128i *)blk);
        __m128i output = _mm_xor_si128(input, keystream);
        _mm_storeu_si128((__m128i *)blk, output);
        ctx->iv = aes_ni_sdctr_increment(ctx->iv);
    }
}

#define NI_ENC_DEC(len)                                                 \
    static FUNC_ISA void aes##len##_cbc_hw_encrypt(                     \
        ssh2_cipher *ciph, void *vblk, int blklen)                      \
    { aes_cbc_ni_encrypt(ciph, vblk, blklen, aes_ni_##len##_e); }       \
    static FUNC_ISA void aes##len##_cbc_hw_decrypt(                     \
        ssh2_cipher *ciph, void *vblk, int blklen)                      \
    { aes_cbc_ni_decrypt(ciph, vblk, blklen, aes_ni_##len##_d); }       \
    static FUNC_ISA void aes##len##_sdctr_hw(                           \
        ssh2_cipher *ciph, void *vblk, int blklen)                      \
    { aes_sdctr_ni(ciph, vblk, blklen, aes_ni_##len##_e); }             \

NI_ENC_DEC(128)
NI_ENC_DEC(192)
NI_ENC_DEC(256)

/* ----------------------------------------------------------------------
 * Stub functions if we have no hardware-accelerated AES. In this
 * case, aes_hw_new returns NULL (though it should also never be
 * selected by aes_select, so the only thing that should even be
 * _able_ to call it is testcrypt). As a result, the remaining vtable
 * functions should never be called at all.
 */

#elif HW_AES == HW_AES_NONE

bool aes_hw_available(void)
{
    return false;
}

static ssh2_cipher *aes_hw_new(const ssh2_cipheralg *alg)
{
    return NULL;
}

#define STUB_BODY { unreachable("Should never be called"); }

static void aes_hw_free(ssh2_cipher *ciph) STUB_BODY
static void aes_hw_setkey(ssh2_cipher *ciph, const void *key) STUB_BODY
static void aes_hw_setiv_cbc(ssh2_cipher *ciph, const void *iv) STUB_BODY
static void aes_hw_setiv_sdctr(ssh2_cipher *ciph, const void *iv) STUB_BODY
#define STUB_ENC_DEC(len)                                               \
    static void aes##len##_cbc_hw_encrypt(                              \
        ssh2_cipher *ciph, void *vblk, int blklen) STUB_BODY            \
    static void aes##len##_cbc_hw_decrypt(                              \
        ssh2_cipher *ciph, void *vblk, int blklen) STUB_BODY            \
    static void aes##len##_sdctr_hw(                                    \
        ssh2_cipher *ciph, void *vblk, int blklen) STUB_BODY

STUB_ENC_DEC(128)
STUB_ENC_DEC(192)
STUB_ENC_DEC(256)

#endif /* HW_AES */

/* ----------------------------------------------------------------------
 * Auxiliary routines for use of AES in other contexts than the main
 * SSH packet protocol.
 */

void aes256_encrypt_pubkey(const void *key, void *blk, int len)
{
    char iv[16];
    memset(iv, 0, 16);
    { // WINSCP
    ssh2_cipher *cipher = ssh2_cipher_new(&ssh_aes256_cbc);
    ssh2_cipher_setkey(cipher, key);
    ssh2_cipher_setiv(cipher, iv);
    ssh2_cipher_encrypt(cipher, blk, len);
    ssh2_cipher_free(cipher);
    } // WINSCP
}

void aes256_decrypt_pubkey(const void *key, void *blk, int len)
{
    char iv[16];
    memset(iv, 0, 16);
    { // WINSCP
    ssh2_cipher *cipher = ssh2_cipher_new(&ssh_aes256_cbc);
    ssh2_cipher_setkey(cipher, key);
    ssh2_cipher_setiv(cipher, iv);
    ssh2_cipher_decrypt(cipher, blk, len);
    ssh2_cipher_free(cipher);
    } // WINSCP
}


#ifdef MPEXT

#include "puttyexp.h"

AESContext * aes_make_context()
{
  ssh2_cipher * cipher = ssh2_cipher_new(&ssh_aes256_sdctr);
  return cipher;
}

void aes_free_context(AESContext * ctx)
{
  ssh2_cipher * cipher = (ssh2_cipher *)ctx;
  ssh2_cipher_free(cipher);
}

void aes_iv(AESContext * ctx, const void * iv)
{
  ssh2_cipher * cipher = (ssh2_cipher *)ctx;
  ssh2_cipher_setiv(cipher, iv);
}

void call_aes_setup(AESContext * ctx, unsigned char * key, int keylen)
{
  ssh2_cipher * cipher = (ssh2_cipher *)ctx;
  assert(keylen == 32);
  ssh2_cipher_setkey(cipher, key);
}

void call_aes_sdctr(unsigned char *blk, int len, void *ctx)
{
  ssh2_cipher * cipher = (ssh2_cipher *)ctx;
  ssh2_cipher_encrypt(cipher, blk, len);
}

#endif
