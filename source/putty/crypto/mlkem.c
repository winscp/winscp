/*
 * Implementation of ML-KEM, previously known as 'Crystals: Kyber'.
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "mlkem.h"
#include "smallmoduli.h"

/* ----------------------------------------------------------------------
 * General definitions.
 */

/*
 * Arithmetic in this system works mod 3329, which is prime, and
 * congruent to 1 mod 256 (in fact it's 13*256 + 1), meaning that
 * 256th roots of unity exist.
 */
#define Q 3329

/*
 * Parameter structure describing a particular instance of ML-KEM.
 */
struct mlkem_params {
    int k;            /* dimensions of the matrices used */
    int eta_1, eta_2; /* parameters for mlkem_matrix_poly_cbd calls */
    int d_u, d_v;     /* bit counts to use in lossy compressed encoding */
};

/*
 * Specific parameter sets.
 */
const mlkem_params mlkem_params_512 = {
    /*.k =*/ 2, /*.eta_1 =*/ 3, /*.eta_2 =*/ 2, /*.d_u =*/ 10, /*.d_v =*/ 4,
};
const mlkem_params mlkem_params_768 = {
    /*.k =*/ 3, /*.eta_1 =*/ 2, /*.eta_2 =*/ 2, /*.d_u =*/ 10, /*.d_v =*/ 4,
};
const mlkem_params mlkem_params_1024 = {
    /*.k =*/ 4, /*.eta_1 =*/ 2, /*.eta_2 =*/ 2, /*.d_u =*/ 11, /*.d_v =*/ 5,
};
#define KMAX 4

/* ----------------------------------------------------------------------
 * Number-theoretic transform on ring elements.
 *
 * The ring R used by ML-KEM is (Z/qZ)[X] / <X^256+1> (where q=3329 as
 * above). If the quotient polynomial were X^256-1 then it would split
 * into 256 linear factors, so that R could be expressed as the direct
 * sum of 256 rings (Z/qZ)[X] / <X-zeta^i> (where zeta is some fixed
 * primitive 256th root of unity mod q), each isomorphic to Z/qZ
 * itself. But X^256+1 only splits into 128 _quadratic_ factors, and
 * hence we can only decompose R as the direct sum of rings of the
 * form (Z/qZ)[X] / <X^2-zeta^j> for odd j, each a quadratic extension
 * of Z/qZ, and all mutually nonisomorphic. This means the NTT runs
 * one pass fewer than you'd "normally" expect, and also, multiplying
 * two elements of R in their NTT representation is not quite as
 * trivial as it would normally be - within each component ring of the
 * direct sum you have to do the multiplication slightly differently
 * depending on the power of zeta in its quotient polynomial.
 *
 * We take zeta=17 to be the canonical primitive 256th root of unity
 * for NTT purposes.
 */

/*
 * First 128 powers of zeta, reordered by bit-reversing the 7-bit
 * index. That is, the nth element of this array contains
 * zeta^(bitrev7(n)). Used by the NTT itself.
 */
static const uint16_t powers_reversed_order[128] = {
    1, 1729, 2580, 3289, 2642, 630, 1897, 848, 1062, 1919, 193, 797, 2786,
    3260, 569, 1746, 296, 2447, 1339, 1476, 3046, 56, 2240, 1333, 1426, 2094,
    535, 2882, 2393, 2879, 1974, 821, 289, 331, 3253, 1756, 1197, 2304, 2277,
    2055, 650, 1977, 2513, 632, 2865, 33, 1320, 1915, 2319, 1435, 807, 452,
    1438, 2868, 1534, 2402, 2647, 2617, 1481, 648, 2474, 3110, 1227, 910, 17,
    2761, 583, 2649, 1637, 723, 2288, 1100, 1409, 2662, 3281, 233, 756, 2156,
    3015, 3050, 1703, 1651, 2789, 1789, 1847, 952, 1461, 2687, 939, 2308, 2437,
    2388, 733, 2337, 268, 641, 1584, 2298, 2037, 3220, 375, 2549, 2090, 1645,
    1063, 319, 2773, 757, 2099, 561, 2466, 2594, 2804, 1092, 403, 1026, 1143,
    2150, 2775, 886, 1722, 1212, 1874, 1029, 2110, 2935, 885, 2154,
};

/*
 * First 128 _odd_ powers of zeta: the nth element is
 * zeta^(2*bitrev7(n)+1). Each of these is used for multiplication in
 * one of the 128 quadratic-extension rings in the NTT decomposition.
 */
static const uint16_t powers_odd_reversed_order[128] = {
    17, 3312, 2761, 568, 583, 2746, 2649, 680, 1637, 1692, 723, 2606, 2288,
    1041, 1100, 2229, 1409, 1920, 2662, 667, 3281, 48, 233, 3096, 756, 2573,
    2156, 1173, 3015, 314, 3050, 279, 1703, 1626, 1651, 1678, 2789, 540, 1789,
    1540, 1847, 1482, 952, 2377, 1461, 1868, 2687, 642, 939, 2390, 2308, 1021,
    2437, 892, 2388, 941, 733, 2596, 2337, 992, 268, 3061, 641, 2688, 1584,
    1745, 2298, 1031, 2037, 1292, 3220, 109, 375, 2954, 2549, 780, 2090, 1239,
    1645, 1684, 1063, 2266, 319, 3010, 2773, 556, 757, 2572, 2099, 1230, 561,
    2768, 2466, 863, 2594, 735, 2804, 525, 1092, 2237, 403, 2926, 1026, 2303,
    1143, 2186, 2150, 1179, 2775, 554, 886, 2443, 1722, 1607, 1212, 2117, 1874,
    1455, 1029, 2300, 2110, 1219, 2935, 394, 885, 2444, 2154, 1175,
};

/*
 * Convert a ring element into NTT representation.
 *
 * The input v is an array of 256 uint16_t, giving the coefficients of
 * a polynomial in X, with v[i] being the coefficient of X^i.
 *
 * v is modified in place. On output, adjacent pairs of elements of v
 * give the coefficients of a smaller polynomial in X, with the pair
 * v[2i],v[2i+1] being the coefficients of X^0 and X^1 respectively in
 * the ring (Z/qZ)[X] / <X^2 - k>, where k = powers_odd_reversed_order[i].
 */
static void mlkem_ntt(uint16_t *v)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);
    size_t next_power = 1;

    size_t len; // WINSCP
    for (len = 128; len >= 2; len /= 2) {
        size_t start; // WINSCP
        for (start = 0; start < 256; start += 2*len) {
            uint16_t mult = powers_reversed_order[next_power++];
            size_t j; // WINSCP
            for (j = start; j < start + len; j++) {
                uint16_t t = reduce(mult * v[j + len], Q, Qrecip);
                v[j + len] = reduce(v[j] + Q - t, Q, Qrecip);
                v[j] = reduce(v[j] + t, Q, Qrecip);
            }
        }
    }
}

/*
 * Convert back from NTT representation. Exactly inverts mlkem_ntt().
 */
static void mlkem_inverse_ntt(uint16_t *v)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);
    size_t next_power = 127;

    size_t len; // WINSCP
    for (len = 2; len <= 128; len *= 2) {
        size_t start; // WINSCP
        for (start = 0; start < 256; start += 2*len) {
            uint16_t mult = powers_reversed_order[next_power--];
            size_t j; // WINSCP
            for (j = start; j < start + len; j++) {
                uint16_t t = v[j];
                v[j] = reduce(t + v[j + len], Q, Qrecip);
                v[j + len] = reduce(mult * (v[j + len] + Q - t), Q, Qrecip);
            }
        }
    }

    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < 256; i++)
        v[i] = reduce(v[i] * 3303, Q, Qrecip);
    } // WINSCP
}

/*
 * Multiply two elements of R in NTT representation.
 *
 * The output can alias an input completely, but mustn't alias one
 * partially.
 */
static void mlkem_multiply_ntts(
    uint16_t *out, const uint16_t *a, const uint16_t *b)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);

    size_t i; // WINSCP
    for (i = 0; i < 128; i++) {
        uint16_t a0 = a[2*i], a1 = a[2*i+1];
        uint16_t b0 = b[2*i], b1 = b[2*i+1];
        uint16_t mult = powers_odd_reversed_order[i];
        uint16_t a1b1 = reduce(a1 * b1, Q, Qrecip);
        out[2*i] = reduce(a0 * b0 + a1b1 * mult, Q, Qrecip);
        out[2*i+1] = reduce(a0 * b1 + a1 * b0, Q, Qrecip);
    }
}

/* ----------------------------------------------------------------------
 * Operations on matrices over the ring R.
 *
 * Most of these don't mind whether the matrix contains ring elements
 * represented directly as polynomials, or in NTT form. The exception
 * is that mlkem_matrix_mul requires it to be in NTT form (because
 * multiplying is a huge pain in the ordinary representation).
 */

typedef struct mlkem_matrix mlkem_matrix;
struct mlkem_matrix {
    unsigned nrows, ncols;

    /*
     * (nrows * ncols * 256) 16-bit integers. Each 256-word block
     * contains an element of R; the blocks are in in row-major order,
     * so that (data + 256*(ncols*y + x)) points at the start of the
     * element in row y column x.
     */
    uint16_t *data;
};

/* Storage used for multiple matrices, to free all at once afterwards */
typedef struct mlkem_matrix_storage mlkem_matrix_storage;
struct mlkem_matrix_storage {
    uint16_t *data;
    size_t n;                          /* number of ring elements */
};

/*
 * Allocate space for multiple matrices. All the arrays of uint16_t
 * are allocated as a single big array. This makes it easy to free the
 * whole lot in one go afterwards.
 *
 * It also means that the arrays have a fixed memory relationship to
 * each other, which matters not at all during live use, but
 * eliminates spurious control-flow divergences in testsc based on
 * accidents of memory allocation when vectorised code checks two
 * memory regions to see if they alias. (The compiler-generated
 * aliasing check must do two comparisons, one for each direction, and
 * the order of those two regions in memory affects whether the first
 * comparison decides the second one is necessary.)
 *
 * The variadic arguments for this function consist of a sequence of
 * triples (mlkem_matrix *m, int nrows, int ncols), terminated by a
 * null matrix pointer.
 */
static void mlkem_matrix_alloc(mlkem_matrix_storage *storage, ...)
{
    va_list ap;
    mlkem_matrix *m;

    storage->n = 0;
    va_start(ap, storage);
    while ((m = va_arg(ap, mlkem_matrix *)) != NULL) {
        int nrows = va_arg(ap, int), ncols = va_arg(ap, int);
        storage->n += nrows * ncols;
    }
    va_end(ap);

    storage->data = snewn(256 * storage->n, uint16_t);
    { // WINSCP
    size_t pos = 0;
    va_start(ap, storage);
    while ((m = va_arg(ap, mlkem_matrix *)) != NULL) {
        int nrows = va_arg(ap, int), ncols = va_arg(ap, int);
        m->nrows = nrows;
        m->ncols = ncols;
        m->data = storage->data + 256 * pos;
        pos += nrows * ncols;
    }
    va_end(ap);
    } // WINSCP
}

/* Clear and free the storage allocated by mlkem_matrix_alloc. */
static void mlkem_matrix_storage_free(mlkem_matrix_storage *storage)
{
    smemclr(storage->data, 256 * storage->n * sizeof(uint16_t));
    sfree(storage->data);
}

/* Add two matrices. */
static void mlkem_matrix_add(mlkem_matrix *out, const mlkem_matrix *left,
                             const mlkem_matrix *right)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);

    assert(out->nrows == left->nrows);
    assert(out->ncols == left->ncols);
    assert(out->nrows == right->nrows);
    assert(out->ncols == right->ncols);

    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < out->nrows; i++) {
        size_t j; // WINSCP
        for (j = 0; j < out->ncols; j++) {
            const uint16_t *lv = left->data + 256*(i * left->ncols + j);
            const uint16_t *rv = right->data + 256*(i * right->ncols + j);
            uint16_t *ov = out->data + 256*(i * out->ncols + j);
            size_t p; // WINSCP
            for (p = 0; p < 256; p++)
                ov[p] = reduce(lv[p] + rv[p] , Q, Qrecip);
        }
    }
    } // WINSCP
}

/* Subtract matrices. */
static void mlkem_matrix_sub(mlkem_matrix *out, const mlkem_matrix *left,
                             const mlkem_matrix *right)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);

    assert(out->nrows == left->nrows);
    assert(out->ncols == left->ncols);
    assert(out->nrows == right->nrows);
    assert(out->ncols == right->ncols);

    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < out->nrows; i++) {
        size_t j; // WINSCP
        for (j = 0; j < out->ncols; j++) {
            const uint16_t *lv = left->data + 256*(i * left->ncols + j);
            const uint16_t *rv = right->data + 256*(i * right->ncols + j);
            uint16_t *ov = out->data + 256*(i * out->ncols + j);
            size_t p; // WINSCP
            for (p = 0; p < 256; p++)
                ov[p] = reduce(lv[p] + Q - rv[p] , Q, Qrecip);
        }
    }
    } // WINSCP
}

/* Convert every element of a matrix into NTT representation. */
static void mlkem_matrix_ntt(mlkem_matrix *m)
{
    size_t i; // WINSCP
    for (i = 0; i < m->nrows * m->ncols; i++)
        mlkem_ntt(m->data + i * 256);
}

/* Convert every element of a matrix out of NTT representation. */
static void mlkem_matrix_inverse_ntt(mlkem_matrix *m)
{
    size_t i; // WINSCP
    for (i = 0; i < m->nrows * m->ncols; i++)
        mlkem_inverse_ntt(m->data + i * 256);
}

/*
 * Multiply two matrices, assuming their elements to be currently in
 * NTT representation.
 *
 * The left input must have the same number of columns as the right
 * has rows, in the usual fashion. The output matrix is overwritten.
 *
 * If 'left_transposed' is true then the left matrix is used as if
 * transposed.
 */
static void mlkem_matrix_mul(mlkem_matrix *out, const mlkem_matrix *left,
                             const mlkem_matrix *right, bool left_transposed)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);
    size_t left_nrows = (left_transposed ? left->ncols : left->nrows);
    size_t left_ncols = (left_transposed ? left->nrows : left->ncols);

    assert(out->nrows == left_nrows);
    assert(left_ncols == right->nrows);
    assert(right->ncols == out->ncols);

    { // WINSCP
    uint16_t work[256];

    size_t i; // WINSCP
    for (i = 0; i < out->nrows; i++) {
        size_t j; // WINSCP
        for (j = 0; j < out->ncols; j++) {
            uint16_t *thisout = out->data + 256 * (i * out->ncols + j);
            memset(thisout, 0, 256 * sizeof(uint16_t));
            { // WINSCP
            size_t k; // WINSCP
            for (k = 0; k < right->nrows; k++) {
                size_t left_index = left_transposed ?
                    k * left->ncols + i : i * left->ncols + k;
                const uint16_t *lv = left->data + 256*left_index;
                const uint16_t *rv = right->data + 256*(k * right->ncols + j);
                mlkem_multiply_ntts(work, lv, rv);
                { // WINSCP
                size_t p; // WINSCP
                for (p = 0; p < 256; p++)
                    thisout[p] = reduce(thisout[p] + work[p], Q, Qrecip);
                } // WINSCP
            }
            } // WINSCP
        }
    }

    smemclr(work, sizeof(work));
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Random sampling functions to make up various kinds of randomised
 * matrix and vector.
 */

static void mlkem_sample_ntt(uint16_t *output, ptrlen seed); /* forward ref */

/*
 * Invent a matrix based on a 32-bit random seed rho.
 *
 * This matrix is logically part of the public (encryption) key: it's
 * not transmitted explicitly, but the seed is, so that the receiver
 * can reconstruct the same matrix. As a result, this function
 * _doesn't_ have to worry about side channel resistance, or even
 * leaving data lying around in arrays.
 */
static void mlkem_matrix_from_seed(mlkem_matrix *m, const void *rho)
{
    unsigned r; // WINSCP
    for (r = 0; r < m->nrows; r++) {
        unsigned c; // WINSCP
        for (c = 0; c < m->ncols; c++) {
            unsigned char seedbuf[34];
            memcpy(seedbuf, rho, 32);
            seedbuf[32] = c;
            seedbuf[33] = r;
            mlkem_sample_ntt(m->data + 256 * (r * m->nrows + c),
                             make_ptrlen(seedbuf, sizeof(seedbuf)));
        }
    }
}

/*
 * Invent a single element of the ring R, uniformly at random, derived
 * in a specified way from the input random seed.
 *
 * Used as a subroutine of mlkem_matrix_from_seed() above. So, for the
 * same reasons, this doesn't have to worry about side channels,
 * making the 'rejection sampling' generation technique easy.
 *
 * The name SampleNTT (in the official spec) reflects the fact that
 * the output elements are regarded as being in NTT representation.
 * But since the NTT is a bijection, and the sampling is from the
 * uniform probability distribution over R, nothing in this function
 * actually needs to worry about that.
 */
static void mlkem_sample_ntt(uint16_t *output, ptrlen seed)
{
    ShakeXOF *sx = shake128_xof_from_input(seed);
    unsigned char bytebuf[4];
    bytebuf[3] = '\0';

    { // WINSCP
    size_t pos; // WINSCP
    for (pos = 0; pos < 256 ;) {
        /* Read 3 bytes into the low-order end of bytebuf. The fourth
         * byte is always 0, so this gives us a random 24-bit integer. */
        shake_xof_read(sx, &bytebuf, 3);
        { // WINSCP
        uint32_t random24 = GET_32BIT_LSB_FIRST(bytebuf);

        /*
         * Split that integer up into two 12-bit ones, and use each
         * one if it's in range (taking care for the second one that
         * we didn't just reach the end of the buffer).
         *
         * This function is only used for generating matrices from an
         * element of the public key, so we can use data-dependent
         * control flow here without worrying about giving away
         * secrets.
         */
        uint16_t d1 = random24 & 0xFFF;
        uint16_t d2 = random24 >> 12;
        if (d1 < Q)
            output[pos++] = d1;
        if (d2 < Q && pos < 256)
            output[pos++] = d2;
        } // WINSCP
    }

    shake_xof_free(sx);
    } // WINSCP
}

/*
 * Invent a random vector, with its elements _not_ in NTT
 * representation, and all the coefficients very small integers (a lot
 * smaller than q) of one sign or the other.
 *
 * eta is a parameter of the probability distribution, sigma is an
 * input 32-byte random seed. Each element of the vector is made by a
 * separate hash operation based on sigma plus a distinguishing
 * integer suffix; 'offset' indicates the starting point for those
 * suffixes, so that the ith output value has suffix (offset+i).
 */
static void mlkem_matrix_poly_cbd(
    mlkem_matrix *v, int eta, const void *sigma, int offset)
{
    const uint64_t Qrecip = reciprocal_for_reduction(Q);

    unsigned char seedbuf[33];
    memcpy(seedbuf, sigma, 32);

    { // WINSCP
    unsigned char *randombuf = snewn(eta * 64, unsigned char);

    unsigned r; // WINSCP
    for (r = 0; r < v->nrows * v->ncols; r++) {
        seedbuf[32] = r + offset;
        { // WINSCP
        ShakeXOF *sx = shake256_xof_from_input(make_ptrlen(seedbuf, 33));
        shake_xof_read(sx, randombuf, eta * 64);
        shake_xof_free(sx);

        { // WINSCP
        size_t i; // WINSCP
        for (i = 0; i < 256; i++) {
            unsigned x = 0, y = 0;
            size_t j; // WINSCP
            for (j = 0; j < eta; j++) {
                size_t bitpos = 2 * i * eta + j;
                x += 1 & ((randombuf[bitpos >> 3]) >> (bitpos & 7));
            }
            for (j = 0; j < eta; j++) {
                size_t bitpos = 2 * i * eta + eta + j;
                y += 1 & ((randombuf[bitpos >> 3]) >> (bitpos & 7));
            }
            v->data[256 * r + i] = reduce(x + Q - y, Q, Qrecip);
        }
        } // WINSCP
        } // WINSCP
    }
    smemclr(seedbuf, sizeof(seedbuf));
    smemclr(randombuf, eta * 64);
    sfree(randombuf);
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Byte-encoding and decoding functions.
 */

/*
 * Losslessly encode one or more elements of the ring R.
 *
 * Each polynomial coefficient, in the range [0,q), is represented as
 * a 12-bit integer. So encoding an entire ring element requires
 * (256*12)/8 = 384 bytes, and if that 384-byte string were
 * interpreted as a little-endian 3072-bit integer D, then the
 * coefficient of X^i could be recovered as (D >> (12*i)) & 0xFFF.
 *
 * The input is expected to be an array of 256*n uint16_t (often the
 * 'data' pointer in an mlkem_matrix). The output is 384*n bytes.
 */
static void mlkem_byte_encode_lossless(
    void *outv, const uint16_t *in, size_t n)
{
    unsigned char *out = (unsigned char *)outv;
    uint32_t buffer = 0, bufbits = 0;
    size_t i; // WINSCP
    for (i = 0; i < 256*n; i++) {
        buffer |= (uint32_t) in[i] << bufbits;
        bufbits += 12;
        while (bufbits >= 8) {
            *out++ = buffer & 0xFF;
            buffer >>= 8;
            bufbits -= 8;
        }
    }
}

/*
 * Decode a string written by mlkem_byte_encode_lossless.
 *
 * Each 12-bit value extracted from the input data is checked to make
 * sure it's in the range [0,q); if it's out of range, the whole
 * function fails and returns false. (But it need not do so in
 * constant time, because that's an "abandon the whole connection"
 * error, not a "subtly make things not work for the attacker" error.)
 */
static bool mlkem_byte_decode_lossless(
    uint16_t *out, const void *inv, size_t n)
{
    const unsigned char *in = (const unsigned char *)inv;
    uint32_t buffer = 0, bufbits = 0;
    size_t i; // WINSCP
    for (i = 0; i < 384*n; i++) {
        buffer |= (uint32_t) in[i] << bufbits;
        bufbits += 8;
        while (bufbits >= 12) {
            uint16_t value = buffer & 0xFFF;
            if (value >= Q)
                return false;
            *out++ = value;
            buffer >>= 12;
            bufbits -= 12;
        }
    }

    return true;
}

/*
 * Lossily encode one or more elements of R, using d bits for each
 * polynomial coefficient, for some d < 12. Each output d-bit value is
 * obtained as if by regarding the input coefficient as an integer in
 * the range [0,q), multiplying by 2^d/q, and rounding to the nearest
 * integer. (Since q is odd, 'round to nearest' can't have a tie.)
 *
 * This means that a large enough input coefficient can round up to
 * 2^d itself. In that situation the output d-bit value is 0.
 */
static void mlkem_byte_encode_compressed(
    void *outv, const uint16_t *in, unsigned d, size_t n)
{
    const uint64_t Qrecip = reciprocal_for_reduction(2*Q);

    unsigned char *out = (unsigned char *)outv;
    uint32_t buffer = 0, bufbits = 0;
    size_t i; // WINSCP
    for (i = 0; i < 256*n; i++) {
        uint32_t dividend = ((uint32_t)in[i] << (d+1)) + Q;
        uint32_t quotient;
        reduce_with_quot(dividend, &quotient, 2*Q, Qrecip);
        buffer |= (uint32_t) (quotient & ((1 << d) - 1)) << bufbits;
        bufbits += d;
        while (bufbits >= 8) {
            *out++ = buffer & 0xFF;
            buffer >>= 8;
            bufbits -= 8;
        }
    }
}

/*
 * Decode the lossily encoded output of mlkem_byte_encode_compressed.
 *
 * Each d-bit chunk of the encoding is converted back into a
 * polynomial coefficient as if by multiplying by q/2^d and then
 * rounding to nearest. Unlike the rounding in the encode step, this
 * _can_ have a tie when an unrounded value is half way between two
 * integers. Ties are broken by rounding up (as if the whole rounding
 * were performed by the simple rounding method of adding 1/2 and then
 * truncating).
 *
 * Unlike the lossless decode function, this one can't fail input
 * validation, because any d-bit value generates some legal
 * coefficient.
 */
static void mlkem_byte_decode_compressed(
    uint16_t *out, const void *inv, unsigned d, size_t n)
{
    const unsigned char *in = (const unsigned char *)inv;
    uint32_t buffer = 0, bufbits = 0;
    size_t i; // WINSCP
    for (i = 0; i < 32*d*n; i++) {
        buffer |= (uint32_t) in[i] << bufbits;
        bufbits += 8;
        while (bufbits >= d) {
            uint32_t value = buffer & ((1 << d) - 1);
            *out++ = (value * (2*Q) + (1 << d)) >> (d + 1);;
            buffer >>= d;
            bufbits -= d;
        }
    }
}

/* ----------------------------------------------------------------------
 * The top-level ML-KEM functions.
 */

/*
 * Innermost keygen function, exposed for side-channel testing, with
 * separate random values rho (public) and sigma (private), so that
 * testsc can vary sigma while leaving rho the same.
 */
void mlkem_keygen_rho_sigma(
    BinarySink *ek_out, BinarySink *dk_out, const mlkem_params *params,
    const void *rho, const void *sigma, const void *z)
{
    mlkem_matrix_storage storage[1];
    mlkem_matrix a[1], s[1], e[1], t[1];
    mlkem_matrix_alloc(storage,
                       a, params->k, params->k,
                       s, params->k, 1,
                       e, params->k, 1,
                       t, params->k, 1,
                       (mlkem_matrix *)NULL);

    /*
     * Make a random k x k matrix A (regarded as in NTT form).
     */
    mlkem_matrix_from_seed(a, rho);

    /*
     * Make two column vectors s and e, with all components having
     * small polynomial coefficients, and then convert them _into_ NTT
     * form.
     */
    mlkem_matrix_poly_cbd(s, params->eta_1, sigma, 0);
    mlkem_matrix_poly_cbd(e, params->eta_1, sigma, params->k);
    mlkem_matrix_ntt(s);
    mlkem_matrix_ntt(e);

    /*
     * Compute the vector t = As + e.
     */
    mlkem_matrix_mul(t, a, s, false);
    mlkem_matrix_add(t, t, e);

    /*
     * The encryption key is the vector t, plus the random seed rho
     * from which anyone can reconstruct the matrix A.
     */
    { // WINSCP
    unsigned char ek[1568];
    mlkem_byte_encode_lossless(ek, t->data, params->k);
    memcpy(ek + 384 * params->k, rho, 32);
    { // WINSCP
    size_t eklen = 384 * params->k + 32;
    put_data(ek_out, ek, eklen);

    /*
     * The decryption key (for the internal "K-PKE" public-key system)
     * is the vector s.
     */
    { // WINSCP
    unsigned char dk[1536];
    mlkem_byte_encode_lossless(dk, s->data, params->k);
    { // WINSCP
    size_t dklen = 384 * params->k;

    /*
     * The decapsulation key, for the full ML-KEM, consists of
     *  - the decryption key as above
     *  - the encryption key
     *  - an extra hash of the encryption key
     *  - the random value z used for "implicit rejection", aka
     *    constructing a useless output value if tampering is
     *    detected. (I think so an attacker can't tell the difference
     *    between "I was rumbled" and "I was undetected but my attempt
     *    didn't generate the right key">)
     */
    put_data(dk_out, dk, dklen);
    put_data(dk_out, ek, eklen);
    { // WINSCP
    ssh_hash *h = ssh_hash_new(&ssh_sha3_256);
    put_data(h, ek, eklen);
    { // WINSCP
    unsigned char ekhash[32];
    ssh_hash_final(h, ekhash);
    put_data(dk_out, ekhash, 32);
    put_data(dk_out, z, 32);

    mlkem_matrix_storage_free(storage);
    smemclr(ek, sizeof(ek));
    smemclr(ekhash, sizeof(ekhash));
    smemclr(dk, sizeof(dk));
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/*
 * Internal keygen function as described in the official spec, taking
 * random values d and z and deterministically constructing a key from
 * them. The test vectors are expressed in terms of this.
 */
void mlkem_keygen_internal(
    BinarySink *ek, BinarySink *dk, const mlkem_params *params,
    const void *d, const void *z)
{
    /* Hash the input randomness d to make two 32-byte values rho and sigma */
    unsigned char rho_sigma[64];
    ssh_hash *h = ssh_hash_new(&ssh_sha3_512);
    put_data(h, d, 32);
    put_byte(h, params->k);
    ssh_hash_final(h, rho_sigma);
    mlkem_keygen_rho_sigma(ek, dk, params, rho_sigma, rho_sigma + 32, z);
    smemclr(rho_sigma, sizeof(rho_sigma));
}

/*
 * Keygen function for live use, making up the values at random.
 */
void mlkem_keygen(
    BinarySink *ek, BinarySink *dk, const mlkem_params *params)
{
    unsigned char dz[64];
    random_read(dz, 64);
    mlkem_keygen_internal(ek, dk, params, dz, dz + 32);
    smemclr(dz, sizeof(dz));
}

/*
 * Internal encapsulation function from the official spec, taking a
 * random value m as input and behaving deterministically. Again used
 * for test vectors.
 */
bool mlkem_encaps_internal(
    BinarySink *c_out, BinarySink *k_out,
    const mlkem_params *params, ptrlen ek, const void *m)
{
    mlkem_matrix_storage storage[1];
    mlkem_matrix t[1], a[1], y[1], e1[1], e2[1], mu[1], u[1], v[1];
    mlkem_matrix_alloc(storage,
                       t, params->k, 1,
                       a, params->k, params->k,
                       y, params->k, 1,
                       e1, params->k, 1,
                       e2, 1, 1,
                       mu, 1, 1,
                       u, params->k, 1,
                       v, 1, 1,
                       (mlkem_matrix *)NULL);

    /*
     * Validate input: ek must be the correct length, and its encoded
     * ring elements must not include any 16-bit integer intended to
     * represent a value mod q which is not in fact in the range [0,q).
     *
     * We test the latter property by decoding the matrix t, and
     * checking the success status returned by the decode.
     */
    if (ek.len != 384 * params->k + 32 ||
        !mlkem_byte_decode_lossless(t->data, ek.ptr, params->k)) {
        mlkem_matrix_storage_free(storage);
        return false;
    }

    /*
     * Regenerate the same matrix A used by key generation, from the
     * seed string rho at the end of ek.
     */
    mlkem_matrix_from_seed(a, (const unsigned char *)ek.ptr + 384 * params->k);

    /*
     * Hash the input randomness m, to get the value k we'll use as
     * the output shared secret, plus some randomness for making up
     * the vectors below.
     */
    { // WINSCP
    unsigned char kr[64];
    unsigned char ekhash[32];
    ssh_hash *h;
    /* Hash the encryption key */
    h = ssh_hash_new(&ssh_sha3_256);
    put_datapl(h, ek);
    ssh_hash_final(h, ekhash);
    /* Hash the input randomness m with that hash */
    h = ssh_hash_new(&ssh_sha3_512);
    put_data(h, m, 32);
    put_data(h, ekhash, 32);
    ssh_hash_final(h, kr);
    { // WINSCP
    const unsigned char *k = kr, *r = kr + 32;

    /*
     * Invent random k-element vectors y and e1, and a random scalar
     * e2 (here represented as a 1x1 matrix for the sake of not
     * proliferating internal helper functions). All are generated by
     * poly_cbd (i.e. their ring elements have polynomial coefficients
     * of small magnitude). y needs to be in NTT form.
     *
     * These generations all use r as their seed, which was the second
     * half of the 64-byte hash of the input m. We pass different
     * 'offset' values to mlkem_matrix_poly_cbd() to ensure the
     * generations are probabilistically independent.
     */
    mlkem_matrix_poly_cbd(y, params->eta_1, r, 0);
    mlkem_matrix_ntt(y);

    mlkem_matrix_poly_cbd(e1, params->eta_2, r, params->k);
    mlkem_matrix_poly_cbd(e2, params->eta_2, r, 2 * params->k);

    /*
     * Invent a random scalar mu (again imagined as a 1x1 matrix),
     * this time by doing lossy decompression of the random value m at
     * 1 bit per polynomial coefficient. That is, all the polynomial
     * coefficients of mu are either 0 or 1665 = (q+1)/2.
     *
     * This generation reuses the _input_ random value m, not either
     * half of the hash we made of it.
     */
    mlkem_byte_decode_compressed(mu->data, m, 1, 1);

    /*
     * Calculate a k-element vector u = A^T y + e1.
     *
     * A and y are in NTT representation, but e1 is not, and we don't
     * want the output to be in NTT form either. So we perform an
     * inverse NTT after the multiplication.
     */
    mlkem_matrix_mul(u, a, y, true);   /* regard a as transposed */
    mlkem_matrix_inverse_ntt(u);
    mlkem_matrix_add(u, u, e1);

    /*
     * Calculate a scalar v = t^T y + e2 + mu.
     *
     * (t and y are column vectors, so t^T y is just a scalar - you
     * could think of it as the dot product t.y if you preferred.)
     *
     * Similarly to above, we multiply t and y which are in NTT
     * representation, and then perform an inverse NTT before adding
     * e2 and mu, which aren't.
     */
    mlkem_matrix_mul(v, t, y, true);   /* regard t as transposed */
    mlkem_matrix_inverse_ntt(v);
    mlkem_matrix_add(v, v, e2);
    mlkem_matrix_add(v, v, mu);

    /*
     * The ciphertext consists of u and v, both encoded lossily, with
     * different numbers of bits retained per element.
     */
    { // WINSCP
    char c[1568];
    mlkem_byte_encode_compressed(c, u->data, params->d_u, params->k);
    mlkem_byte_encode_compressed(c + 32 * params->k * params->d_u,
                                 v->data, params->d_v, 1);
    put_data(c_out, c, 32 * (params->k * params->d_u + params->d_v));

    /*
     * The output shared secret is just half of the hash of m (the
     * first half, which we didn't use for generating vectors above).
     */
    put_data(k_out, k, 32);

    smemclr(kr, sizeof(kr));
    mlkem_matrix_storage_free(storage);

    return true;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/*
 * Encapsulation function for live use, using the real RNG..
 */
bool mlkem_encaps(BinarySink *ciphertext, BinarySink *kout,
                  const mlkem_params *params, ptrlen ek)
{
    unsigned char m[32];
    random_read(m, 32);
    { // WINSCP
    bool success = mlkem_encaps_internal(ciphertext, kout, params, ek, m);
    smemclr(m, sizeof(m));
    return success;
    } // WINSCP
}

/*
 * Decapsulation.
 */
bool mlkem_decaps(BinarySink *k_out, const mlkem_params *params,
                  ptrlen dk, ptrlen c)
{
    /*
     * Validation: check the input strings are the right lengths.
     */
    if (dk.len != 768 * params->k + 96)
        return false;
    if (c.len != 32 * (params->d_u * params->k + params->d_v))
        return false;

    /*
     * Further validation: extract the encryption key from the middle
     * of dk, hash it, and check the hash matches.
     */
    { // WINSCP
    const unsigned char *dkp = (const unsigned char *)dk.ptr;
    const unsigned char *cp = (const unsigned char *)c.ptr;
    ptrlen ek = make_ptrlen(dkp + 384*params->k, 384*params->k + 32);
    ssh_hash *h;
    unsigned char ekhash[32];
    h = ssh_hash_new(&ssh_sha3_256);
    put_datapl(h, ek);
    ssh_hash_final(h, ekhash);
    if (!smemeq(ekhash, dkp + 768*params->k + 32, 32))
        return false;

    { // WINSCP
    mlkem_matrix_storage storage[1];
    mlkem_matrix u[1], v[1], s[1], w[1];
    mlkem_matrix_alloc(storage,
                       u, params->k, 1,
                       v, 1, 1,
                       s, params->k, 1,
                       w, 1, 1,
                       (mlkem_matrix *)NULL);
    /*
     * Decode the vector u and the scalar v from the ciphertext. These
     * won't come out exactly the same as the originals, because of
     * the lossy compression.
     */
    mlkem_byte_decode_compressed(u->data, cp, params->d_u, params->k);
    mlkem_matrix_ntt(u);
    mlkem_byte_decode_compressed(v->data, cp + 32 * params->d_u * params->k,
                                 params->d_v, 1);

    /*
     * Decode the vector s from the private key.
     */
    mlkem_byte_decode_lossless(s->data, dkp, params->k);

    /*
     * Calculate the scalar w = v - s^T u.
     *
     * s and u are in NTT representation, but v isn't, so we
     * inverse-NTT the product before doing the subtraction. Therefore
     * w is not in NTT form either.
     */
    mlkem_matrix_mul(w, s, u, true);   /* regard s as transposed */
    mlkem_matrix_inverse_ntt(w);
    mlkem_matrix_sub(w, v, w);

    /*
     * The aim is that this reconstructs something close enough to the
     * random vector mu that was made from the input secret m to
     * encapsulation, on the grounds that mu's polynomial coefficients
     * were very widely separated (on opposite sides of the cyclic
     * additive group of Z/qZ) and the noise added during encryption
     * all had _small_ polynomial coefficients.
     *
     * So we now re-encode this lossily at 1 bit per polynomial
     * coefficient, and hope that it reconstructs the actual string m.
     *
     * However, this _is_ only a hope! The ML-KEM decryption is not a
     * true mathematical inverse to encryption. With extreme bad luck,
     * the noise can add up enough that it flips a bit of m, and
     * everything fails. The parameters are chosen to make this happen
     * with negligible probability (the same kind of low probability
     * that makes you not worry about spontaneous hash collisions),
     * but it's not actually impossible.
     */
    { // WINSCP
    unsigned char m[32];
    mlkem_byte_encode_compressed(m, w->data, 1, 1);

    /*
     * Now do the key _encapsulation_ again from scratch, using that
     * secret m as input, and check that it generates the identical
     * ciphertext. This should catch the above theoretical failure,
     * but also, it's a defence against malicious intervention in the
     * key exchange.
     *
     * This is also where we get the output secret k from: the
     * encapsulation function creates it as half of the hash of m.
     */
    { // WINSCP
    unsigned char c_regen[1568], k[32];
    buffer_sink c_sink[1], k_sink[1];
    buffer_sink_init(c_sink, c_regen, sizeof(c_regen));
    buffer_sink_init(k_sink, k, sizeof(k));
    { // WINSCP
    bool success = mlkem_encaps_internal(
        BinarySink_UPCAST(c_sink), BinarySink_UPCAST(k_sink), params, ek, m);
    /* If any application of ML-KEM uses a dk given to it by someone
     * else, then perhaps they have to worry about being given an
     * invalid one? But in our application we always expect this to
     * succeed, because dk is generated and used at the same end of
     * the SSH connection, within the same process, and nobody is
     * interfering with it. */
    assert(success && "We generated this dk ourselves, how can it be bad?");

    /*
     * If mlkem_encaps_internal returned success but delivered the
     * wrong ciphertext, that's a failure, but we must be careful not
     * to let the attacker know exactly what went wrong. So we
     * generate a plausible but wrong substitute output secret.
     *
     * k_reject is that secret; for constant-time reasons we generate
     * it unconditionally.
     */
    { // WINSCP
    unsigned char k_reject[32];
    h = ssh_hash_new(&ssh_shake256_32bytes);
    put_data(h, dkp + 768 * params->k + 64, 32);
    put_datapl(h, c);
    ssh_hash_final(h, k_reject);

    /*
     * Now replace k with k_reject if the ciphertexts didn't match.
     */
    assert((void *)c_sink->out == (void *)(c_regen + c.len));
    { // WINSCP
    unsigned match = smemeq(c.ptr, c_regen, c.len);
    unsigned mask = match - 1;
    size_t i; // WINSCP
    for (i = 0; i < 32; i++)
        k[i] ^= mask & (k[i] ^ k_reject[i]);

    /*
     * And we're done! Free everything and return whichever secret we
     * chose.
     */
    put_data(k_out, k, 32);
    mlkem_matrix_storage_free(storage);
    smemclr(m, sizeof(m));
    smemclr(c_regen, sizeof(c_regen));
    smemclr(k, sizeof(k));
    smemclr(k_reject, sizeof(k_reject));
    return true;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Implement the pq_kemalg vtable in terms of the above functions.
 */

struct mlkem_dk {
    strbuf *encoded;
    pq_kem_dk dk;
};

static pq_kem_dk *mlkem_vt_keygen(const pq_kemalg *alg, BinarySink *ek)
{
    struct mlkem_dk *mdk = snew(struct mlkem_dk);
    mdk->dk.vt = alg;
    mdk->encoded = strbuf_new_nm();
    mlkem_keygen(ek, BinarySink_UPCAST(mdk->encoded), alg->extra);
    return &mdk->dk;
}

static bool mlkem_vt_encaps(const pq_kemalg *alg, BinarySink *c, BinarySink *k,
                            ptrlen ek)
{
    return mlkem_encaps(c, k, alg->extra, ek);
}

static bool mlkem_vt_decaps(pq_kem_dk *dk, BinarySink *k, ptrlen c)
{
    struct mlkem_dk *mdk = container_of(dk, struct mlkem_dk, dk);
    return mlkem_decaps(k, mdk->dk.vt->extra,
                        ptrlen_from_strbuf(mdk->encoded), c);
}

static void mlkem_vt_free_dk(pq_kem_dk *dk)
{
    struct mlkem_dk *mdk = container_of(dk, struct mlkem_dk, dk);
    strbuf_free(mdk->encoded);
    sfree(mdk);
}

const pq_kemalg ssh_mlkem512 = {
    /*.keygen =*/ mlkem_vt_keygen,
    /*.encaps =*/ mlkem_vt_encaps,
    /*.decaps =*/ mlkem_vt_decaps,
    /*.free_dk =*/ mlkem_vt_free_dk,
    /*.extra =*/ &mlkem_params_512,
    /*.description =*/ "ML-KEM-512",
    /*.ek_len =*/ 384 * 2 + 32,
    /*.c_len =*/ 32 * (10 * 2 + 4),
};

const pq_kemalg ssh_mlkem768 = {
    /*.keygen =*/ mlkem_vt_keygen,
    /*.encaps =*/ mlkem_vt_encaps,
    /*.decaps =*/ mlkem_vt_decaps,
    /*.free_dk =*/ mlkem_vt_free_dk,
    /*.extra =*/ &mlkem_params_768,
    /*.description =*/ "ML-KEM-768",
    /*.ek_len =*/ 384 * 3 + 32,
    /*.c_len =*/ 32 * (10 * 3 + 4),
};

const pq_kemalg ssh_mlkem1024 = {
    /*.keygen =*/ mlkem_vt_keygen,
    /*.encaps =*/ mlkem_vt_encaps,
    /*.decaps =*/ mlkem_vt_decaps,
    /*.free_dk =*/ mlkem_vt_free_dk,
    /*.extra =*/ &mlkem_params_1024,
    /*.description =*/ "ML-KEM-1024",
    /*.ek_len =*/ 384 * 4 + 32,
    /*.c_len =*/ 32 * (11 * 4 + 5),
};
