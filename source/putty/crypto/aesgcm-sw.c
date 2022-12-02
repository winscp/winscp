/*
 * Implementation of the GCM polynomial hash in pure software.
 *
 * I don't know of a faster way to do this in a side-channel safe
 * manner than by precomputing a giant table and iterating over the
 * whole thing.
 *
 * The original GCM reference suggests that you precompute the effects
 * of multiplying a 128-bit value by the fixed key, in the form of a
 * table indexed by some number of bits of the input value, so that
 * you end up computing something of the form
 *
 *   table1[x & 0xFF] ^ table2[(x>>8) & 0xFF] ^ ... ^ table15[(x>>120) & 0xFF]
 *
 * But that was obviously written before cache and timing leaks were
 * known about. What's a time-safe approach?
 *
 * Well, the above technique isn't fixed to 8 bits of input per table.
 * You could trade off the number of tables against the size of each
 * table. At one extreme of this tradeoff, you have 128 tables each
 * indexed by a single input bit - which is to say, you have 128
 * values, each 128 bits wide, and you XOR together the subset of
 * those values corresponding to the input bits, which you can do by
 * making a bitmask out of each input bit using standard constant-
 * time-coding bit twiddling techniques.
 *
 * That's pretty unpleasant when GCM is supposed to be a fast
 * algorithm, but I don't know of a better approach that meets current
 * security standards! Suggestions welcome, if they can get through
 * testsc.
 */

#include "ssh.h"
#include "aesgcm.h"

/*
 * Store a 128-bit value in the most convenient form standard C will
 * let us, namely two uint64_t giving its most and least significant
 * halves.
 */
typedef struct {
    uint64_t hi, lo;
} value128_t;

typedef struct aesgcm_sw {
    AESGCM_COMMON_FIELDS;

    /* Accumulator for the current evaluation, and mask that will be
     * XORed in at the end. High  */
    value128_t acc, mask;

    /*
     * Table of values to XOR in for each bit, representing the effect
     * of multiplying by the fixed key. The key itself doesn't need to
     * be stored separately, because it's never used. (However, it is
     * also the first entry in the table, so if you _do_ need it,
     * there it is.)
     *
     * Table is indexed from the low bit of the input upwards.
     */
    value128_t table[128];
} aesgcm_sw;

static bool aesgcm_sw_available(void)
{
    return true;  /* pure software implementation, always available */
}

static void aesgcm_sw_setkey_impl(aesgcm_sw *gcm, const unsigned char *var)
{
    value128_t v;
    v.hi = GET_64BIT_MSB_FIRST(var);
    v.lo = GET_64BIT_MSB_FIRST(var + 8);

    /*
     * Prepare the table. This has to be done in reverse order, so
     * that the original value of the variable corresponds to
     * table[127], because AES-GCM works in the bit-reversal of its
     * logical specification so that's where the logical constant term
     * lives. (See more detailed comment in aesgcm-ref-poly.c.)
     */
    { // WINSCP
    size_t i;
    for (i = 0; i < 128; i++) {
        gcm->table[127 - i] = v;

        /* Multiply v by x, which means shifting right (bit reversal
         * again) and then adding 0xE1 at the top if we shifted a 1 out. */
        { // WINSCP
        uint64_t lobit = v.lo & 1;
        v.lo = (v.lo >> 1) ^ (v.hi << 63);
        v.hi = (v.hi >> 1) ^ (0xE100000000000000ULL & -lobit);
        } // WINSCP
    }
    } // WINSCP
}

static inline void aesgcm_sw_setup(aesgcm_sw *gcm, const unsigned char *mask)
{
    gcm->mask.hi = GET_64BIT_MSB_FIRST(mask);
    gcm->mask.lo = GET_64BIT_MSB_FIRST(mask + 8);
    gcm->acc.hi = gcm->acc.lo = 0;
}

static inline void aesgcm_sw_coeff(aesgcm_sw *gcm, const unsigned char *coeff)
{
    /* XOR in the new coefficient */
    gcm->acc.hi ^= GET_64BIT_MSB_FIRST(coeff);
    gcm->acc.lo ^= GET_64BIT_MSB_FIRST(coeff + 8);

    /* And now just loop over the bits of acc, making up a new value
     * by XORing together the entries of 'table' corresponding to set
     * bits. */

    { // WINSCP
    value128_t out;
    out.lo = out.hi = 0;

    { // WINSCP
    const value128_t *tableptr = gcm->table;

    size_t i;
    for (i = 0; i < 64; i++) {
        uint64_t bit = 1 & gcm->acc.lo;
        gcm->acc.lo >>= 1;
        { // WINSCP
        uint64_t mask = -bit;
        out.hi ^= mask & tableptr->hi;
        out.lo ^= mask & tableptr->lo;
        tableptr++;
        } // WINSCP
    }
    for (i = 0; i < 64; i++) {
        uint64_t bit = 1 & gcm->acc.hi;
        gcm->acc.hi >>= 1;
        { // WINSCP
        uint64_t mask = -bit;
        out.hi ^= mask & tableptr->hi;
        out.lo ^= mask & tableptr->lo;
        tableptr++;
        } // WINSCP
    }

    gcm->acc = out;
    } // WINSCP
    } // WINSCP
}

static inline void aesgcm_sw_output(aesgcm_sw *gcm, unsigned char *output)
{
    PUT_64BIT_MSB_FIRST(output, gcm->acc.hi ^ gcm->mask.hi);
    PUT_64BIT_MSB_FIRST(output + 8, gcm->acc.lo ^ gcm->mask.lo);
    smemclr(&gcm->acc, 16);
    smemclr(&gcm->mask, 16);
}

#define AESGCM_FLAVOUR sw
#define AESGCM_NAME "unaccelerated"
#include "aesgcm-footer.h"
