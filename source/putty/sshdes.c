/*
 * sshdes.c: implementation of DES.
 */

/*
 * Background
 * ----------
 *
 * The basic structure of DES is a Feistel network: the 64-bit cipher
 * block is divided into two 32-bit halves L and R, and in each round,
 * a mixing function is applied to one of them, the result is XORed
 * into the other, and then the halves are swapped so that the other
 * one will be the input to the mixing function next time. (This
 * structure guarantees reversibility no matter whether the mixing
 * function itself is bijective.)
 *
 * The mixing function for DES goes like this:
 *  + Extract eight contiguous 6-bit strings from the 32-bit word.
 *    They start at positions 4 bits apart, so each string overlaps
 *    the next one by one bit. At least one has to wrap cyclically
 *    round the end of the word.
 *  + XOR each of those strings with 6 bits of data from the key
 *    schedule (which consists of 8 x 6-bit strings per round).
 *  + Use the resulting 6-bit numbers as the indices into eight
 *    different lookup tables ('S-boxes'), each of which delivers a
 *    4-bit output.
 *  + Concatenate those eight 4-bit values into a 32-bit word.
 *  + Finally, apply a fixed permutation P to that word.
 *
 * DES adds one more wrinkle on top of this structure, which is to
 * conjugate it by a bitwise permutation of the cipher block. That is,
 * before starting the main cipher rounds, the input bits are permuted
 * according to a 64-bit permutation called IP, and after the rounds
 * are finished, the output bits are permuted back again by applying
 * the inverse of IP.
 *
 * This gives a lot of leeway to redefine the components of the cipher
 * without actually changing the input and output. You could permute
 * the bits in the output of any or all of the S-boxes, or reorder the
 * S-boxes among themselves, and adjust the following permutation P to
 * compensate. And you could adjust IP by post-composing a rotation of
 * each 32-bit half, and adjust the starting offsets of the 6-bit
 * S-box indices to compensate.
 *
 * test/desref.py demonstrates this by providing two equivalent forms
 * of the cipher, called DES and SGTDES, which give the same output.
 * DES is the form described in the original spec: if you make it
 * print diagnostic output during the cipher and check it against the
 * original, you should recognise the S-box outputs as matching the
 * ones you expect. But SGTDES, which I egotistically name after
 * myself, is much closer to the form implemented here: I've changed
 * the permutation P to suit my implementation strategy and
 * compensated by permuting the S-boxes, and also I've added a
 * rotation right by 1 bit to IP so that only one S-box index has to
 * wrap round the word and also so that the indices are nicely aligned
 * for the constant-time selection system I'm using.
 */

#include <stdio.h>

#include "ssh.h"
#include "mpint_i.h"               /* we reuse the BignumInt system */

/* If you compile with -DDES_DIAGNOSTICS, intermediate results will be
 * sent to debug() (so you also need to compile with -DDEBUG).
 * Otherwise this ifdef will condition away all the debug() calls. */
#ifndef DES_DIAGNOSTICS
#undef debug
#define debug(...) ((void)0)
#endif

/*
 * General utility functions.
 */
static inline uint32_t rol(uint32_t x, unsigned c)
{
    return (x << (31 & c)) | (x >> (31 & -c));
}
static inline uint32_t ror(uint32_t x, unsigned c)
{
    return rol(x, -c);
}

/*
 * The hard part of doing DES in constant time is the S-box lookup.
 *
 * My strategy is to iterate over the whole lookup table! That's slow,
 * but I don't see any way to avoid _something_ along those lines: in
 * every round, every entry in every S-box is potentially needed, and
 * if you can't change your memory access pattern based on the input
 * data, it follows that you have to read a quantity of information
 * equal to the size of all the S-boxes. (Unless they were to turn out
 * to be significantly compressible, but I for one couldn't show them
 * to be.)
 *
 * In more detail, I construct a sort of counter-based 'selection
 * gadget', which is 15 bits wide and starts off with the top bit
 * zero, the next eight bits all 1, and the bottom six set to the
 * input S-box index:
 *
 *     011111111xxxxxx
 *
 * Now if you add 1 in the lowest bit position, then either it carries
 * into the top section (resetting it to 100000000), or it doesn't do
 * that yet. If you do that 64 times, then it will _guarantee_ to have
 * ticked over into 100000000. In between those increments, the eight
 * bits that started off as 11111111 will have stayed that way for
 * some number of iterations and then become 00000000, and exactly how
 * many iterations depends on the input index.
 *
 * The purpose of the 0 bit at the top is to absorb the carry when the
 * switch happens, which means you can pack more than one gadget into
 * the same machine word and have them all work in parallel without
 * each one intefering with the next.
 *
 * The next step is to use each of those 8-bit segments as a bit mask:
 * each one is ANDed with a lookup table entry, and all the results
 * are XORed together. So you end up with the bitwise XOR of some
 * initial segment of the table entries. And the stored S-box tables
 * are transformed in such a way that the real S-box values are given
 * not by the individual entries, but by the cumulative XORs
 * constructed in this way.
 *
 * A refinement is that I increment each gadget by 2 rather than 1
 * each time, so I only iterate 32 times instead of 64. That's why
 * there are 8 selection bits instead of 4: each gadget selects enough
 * bits to reconstruct _two_ S-box entries, for a pair of indices
 * (2n,2n+1), and then finally I use the low bit of the index to do a
 * parallel selection between each of those pairs.
 *
 * The selection gadget is not quite 16 bits wide. So you can fit four
 * of them across a 64-bit word at 16-bit intervals, which is also
 * convenient because the place the S-box indices are coming from also
 * has pairs of them separated by 16-bit distances, so it's easy to
 * copy them into the gadgets in the first place.
 */

/*
 * The S-box data. Each pair of nonzero columns here describes one of
 * the S-boxes, corresponding to the SGTDES tables in test/desref.py,
 * under the following transformation.
 *
 * Take S-box #3 as an example. Its values in successive rows of this
 * table are eb,e8,54,3d, ... So the cumulative XORs of initial
 * sequences of those values are eb,(eb^e8),(eb^e8^54), ... which
 * comes to eb,03,57,... Of _those_ values, the top nibble (e,0,5,...)
 * gives the even-numbered entries in the S-box, in _reverse_ order
 * (because a lower input index selects the XOR of a longer
 * subsequence). The odd-numbered entries are given by XORing the two
 * digits together: (e^b),(0^3),(5^7),... = 5,3,2,... And indeed, if
 * you check SGTDES.sboxes[3] you find it ends ... 52 03 e5.
 */
#define SBOX_ITERATION(X)                       \
    /*  66  22  44  00      77  33  55  11 */   \
    X(0xf600970083008500, 0x0e00eb007b002e00)   \
    X(0xda00e4009000e000, 0xad00e800a700b400)   \
    X(0x1a009d003f003600, 0xf60054004300cd00)   \
    X(0xaf00c500e900a900, 0x63003d00f2005900)   \
    X(0xf300750079001400, 0x80005000a2008900)   \
    X(0xa100d400d6007b00, 0xd3009000d300e100)   \
    X(0x450087002600ac00, 0xae003c0031009c00)   \
    X(0xd000b100b6003600, 0x3e006f0092005900)   \
    X(0x4d008a0026001000, 0x89007a00b8004a00)   \
    X(0xca00f5003f00ac00, 0x6f00f0003c009400)   \
    X(0x92008d0090001000, 0x8c00c600ce004a00)   \
    X(0xe2005900e9006d00, 0x790078007800fa00)   \
    X(0x1300b10090008d00, 0xa300170027001800)   \
    X(0xc70058005f006a00, 0x9c00c100e0006300)   \
    X(0x9b002000f000f000, 0xf70057001600f900)   \
    X(0xeb00b0009000af00, 0xa9006300b0005800)   \
    X(0xa2001d00cf000000, 0x3800b00066000000)   \
    X(0xf100da007900d000, 0xbc00790094007900)   \
    X(0x570015001900ad00, 0x6f00ef005100cb00)   \
    X(0xc3006100e9006d00, 0xc000b700f800f200)   \
    X(0x1d005800b600d000, 0x67004d00cd002c00)   \
    X(0xf400b800d600e000, 0x5e00a900b000e700)   \
    X(0x5400d1003f009c00, 0xc90069002c005300)   \
    X(0xe200e50060005900, 0x6a00b800c500f200)   \
    X(0xdf0047007900d500, 0x7000ec004c00ea00)   \
    X(0x7100d10060009c00, 0x3f00b10095005e00)   \
    X(0x82008200f0002000, 0x87001d00cd008000)   \
    X(0xd0007000af00c000, 0xe200be006100f200)   \
    X(0x8000930060001000, 0x36006e0081001200)   \
    X(0x6500a300d600ac00, 0xcf003d007d00c000)   \
    X(0x9000700060009800, 0x62008100ad009200)   \
    X(0xe000e4003f00f400, 0x5a00ed009000f200)   \
    /* end of list */

/*
 * The S-box mapping function. Expects two 32-bit input words: si6420
 * contains the table indices for S-boxes 0,2,4,6 with their low bits
 * starting at position 2 (for S-box 0) and going up in steps of 8.
 * si7531 has indices 1,3,5,7 in the same bit positions.
 */
static inline uint32_t des_S(uint32_t si6420, uint32_t si7531)
{
    debug("sindices: %02x %02x %02x %02x %02x %02x %02x %02x\n",
          0x3F & (si6420 >>  2), 0x3F & (si7531 >>  2),
          0x3F & (si6420 >> 10), 0x3F & (si7531 >> 10),
          0x3F & (si6420 >> 18), 0x3F & (si7531 >> 18),
          0x3F & (si6420 >> 26), 0x3F & (si7531 >> 26));

#ifdef SIXTY_FOUR_BIT
    /*
     * On 64-bit machines, we store the table in exactly the form
     * shown above, and make two 64-bit words containing four
     * selection gadgets each.
     */

    /* Set up the gadgets. The 'cNNNN' variables will be gradually
     * incremented, and the bits in positions FF00FF00FF00FF00 will
     * act as selectors for the words in the table.
     *
     * A side effect of moving the input indices further apart is that
     * they change order, because it's easier to keep a pair that were
     * originally 16 bits apart still 16 bits apart, which now makes
     * them adjacent instead of separated by one. So the fact that
     * si6420 turns into c6240 (with the 2,4 reversed) is not a typo!
     * This will all be undone when we rebuild the output word later.
     */
    uint64_t c6240 = ((si6420 | ((uint64_t)si6420 << 24))
                      & 0x00FC00FC00FC00FC) | 0xFF00FF00FF00FF00;
    uint64_t c7351 = ((si7531 | ((uint64_t)si7531 << 24))
                      & 0x00FC00FC00FC00FC) | 0xFF00FF00FF00FF00;
    debug("S in:  c6240=%016"PRIx64" c7351=%016"PRIx64"\n", c6240, c7351);

    /* Iterate over the table. The 'sNNNN' variables accumulate the
     * XOR of all the table entries not masked out. */
    static const struct tbl { uint64_t t6240, t7351; } tbl[32] = {
#define TABLE64(a, b) { a, b },
        SBOX_ITERATION(TABLE64)
#undef TABLE64
    };
    uint64_t s6240 = 0, s7351 = 0;
    for (const struct tbl *t = tbl, *limit = tbl + 32; t < limit; t++) {
        s6240 ^= c6240 & t->t6240; c6240 += 0x0008000800080008;
        s7351 ^= c7351 & t->t7351; c7351 += 0x0008000800080008;
    }
    debug("S out: s6240=%016"PRIx64" s7351=%016"PRIx64"\n", s6240, s7351);

    /* Final selection between each even/odd pair: mask off the low
     * bits of all the input indices (which haven't changed throughout
     * the iteration), and multiply by a bit mask that will turn each
     * set bit into a mask covering the upper nibble of the selected
     * pair. Then use those masks to control which set of lower
     * nibbles is XORed into the upper nibbles. */
    s6240 ^= (s6240 << 4) & ((0xf000/0x004) * (c6240 & 0x0004000400040004));
    s7351 ^= (s7351 << 4) & ((0xf000/0x004) * (c7351 & 0x0004000400040004));

    /* Now the eight final S-box outputs are in the upper nibble of
     * each selection position. Mask away the rest of the clutter. */
    s6240 &= 0xf000f000f000f000;
    s7351 &= 0xf000f000f000f000;
    debug("s0=%x s1=%x s2=%x s3=%x s4=%x s5=%x s6=%x s7=%x\n",
          (unsigned)(0xF & (s6240 >> 12)),
          (unsigned)(0xF & (s7351 >> 12)),
          (unsigned)(0xF & (s6240 >> 44)),
          (unsigned)(0xF & (s7351 >> 44)),
          (unsigned)(0xF & (s6240 >> 28)),
          (unsigned)(0xF & (s7351 >> 28)),
          (unsigned)(0xF & (s6240 >> 60)),
          (unsigned)(0xF & (s7351 >> 60)));

    /* Combine them all into a single 32-bit output word, which will
     * come out in the order 76543210. */
    uint64_t combined = (s6240 >> 12) | (s7351 >> 8);
    return combined | (combined >> 24);

#else /* SIXTY_FOUR_BIT */
    /*
     * For 32-bit platforms, we do the same thing but in four 32-bit
     * words instead of two 64-bit ones, so the CPU doesn't have to
     * waste time propagating carries or shifted bits between the two
     * halves of a uint64 that weren't needed anyway.
     */

    /* Set up the gadgets */
    uint32_t c40 = ((si6420     ) & 0x00FC00FC) | 0xFF00FF00;
    uint32_t c62 = ((si6420 >> 8) & 0x00FC00FC) | 0xFF00FF00;
    uint32_t c51 = ((si7531     ) & 0x00FC00FC) | 0xFF00FF00;
    uint32_t c73 = ((si7531 >> 8) & 0x00FC00FC) | 0xFF00FF00;
    debug("S in:  c40=%08"PRIx32" c62=%08"PRIx32
          " c51=%08"PRIx32" c73=%08"PRIx32"\n", c40, c62, c51, c73);

    /* Iterate over the table */
    static const struct tbl { uint32_t t40, t62, t51, t73; } tbl[32] = {
#define TABLE32(a, b) { ((uint32_t)a), (a>>32), ((uint32_t)b), (b>>32) },
        SBOX_ITERATION(TABLE32)
#undef TABLE32
    };
    uint32_t s40 = 0, s62 = 0, s51 = 0, s73 = 0;
    for (const struct tbl *t = tbl, *limit = tbl + 32; t < limit; t++) {
        s40 ^= c40 & t->t40; c40 += 0x00080008;
        s62 ^= c62 & t->t62; c62 += 0x00080008;
        s51 ^= c51 & t->t51; c51 += 0x00080008;
        s73 ^= c73 & t->t73; c73 += 0x00080008;
    }
    debug("S out: s40=%08"PRIx32" s62=%08"PRIx32
           " s51=%08"PRIx32" s73=%08"PRIx32"\n", s40, s62, s51, s73);

    /* Final selection within each pair */
    s40 ^= (s40 << 4) & ((0xf000/0x004) * (c40 & 0x00040004));
    s62 ^= (s62 << 4) & ((0xf000/0x004) * (c62 & 0x00040004));
    s51 ^= (s51 << 4) & ((0xf000/0x004) * (c51 & 0x00040004));
    s73 ^= (s73 << 4) & ((0xf000/0x004) * (c73 & 0x00040004));

    /* Clean up the clutter */
    s40 &= 0xf000f000;
    s62 &= 0xf000f000;
    s51 &= 0xf000f000;
    s73 &= 0xf000f000;
    debug("s0=%x s1=%x s2=%x s3=%x s4=%x s5=%x s6=%x s7=%x\n",
          (unsigned)(0xF & (s40 >> 12)),
          (unsigned)(0xF & (s51 >> 12)),
          (unsigned)(0xF & (s62 >> 12)),
          (unsigned)(0xF & (s73 >> 12)),
          (unsigned)(0xF & (s40 >> 28)),
          (unsigned)(0xF & (s51 >> 28)),
          (unsigned)(0xF & (s62 >> 28)),
          (unsigned)(0xF & (s73 >> 28)));

    /* Recombine and return */
    return (s40 >> 12) | (s62 >> 4) | (s51 >> 8) | (s73);

#endif /* SIXTY_FOUR_BIT */

}

/*
 * Now for the permutation P. The basic strategy here is to use a
 * Benes network: in each stage, the bit at position i is allowed to
 * either stay where it is or swap with i ^ D, where D is a power of 2
 * that varies with each phase. (So when D=1, pairs of the form
 * {2n,2n+1} can swap; when D=2, the pairs are {4n+j,4n+j+2} for
 * j={0,1}, and so on.)
 *
 * You can recursively construct a Benes network for an arbitrary
 * permutation, in which the values of D iterate across all the powers
 * of 2 less than the permutation size and then go back again. For
 * example, the typical presentation for 32 bits would have D iterate
 * over 16,8,4,2,1,2,4,8,16, and there's an easy algorithm that can
 * express any permutation in that form by deciding which pairs of
 * bits to swap in the outer pair of stages and then recursing to do
 * all the stages in between.
 *
 * Actually implementing the swaps is easy when they're all between
 * bits at the same separation: make the value x ^ (x >> D), mask out
 * just the bits in the low position of a pair that needs to swap, and
 * then use the resulting value y to make x ^ y ^ (y << D) which is
 * the swapped version.
 *
 * In this particular case, I processed the bit indices in the other
 * order (going 1,2,4,8,16,8,4,2,1), which makes no significant
 * difference to the construction algorithm (it's just a relabelling),
 * but it now means that the first two steps only permute entries
 * within the output of each S-box - and therefore we can leave them
 * completely out, in favour of just defining the S-boxes so that
 * those permutation steps are already applied. Furthermore, by
 * exhaustive search over the rest of the possible bit-orders for each
 * S-box, I was able to find a version of P which could be represented
 * in such a way that two further phases had all their control bits
 * zero and could be skipped. So the number of swap stages is reduced
 * to 5 from the 9 that might have been needed.
 */

static inline uint32_t des_benes_step(uint32_t v, unsigned D, uint32_t mask)
{
    uint32_t diff = (v ^ (v >> D)) & mask;
    return v ^ diff ^ (diff << D);
}

static inline uint32_t des_P(uint32_t v_orig)
{
    uint32_t v = v_orig;

    /* initial stages with distance 1,2 are part of the S-box data table */
    v = des_benes_step(v,  4, 0x07030702);
    v = des_benes_step(v,  8, 0x004E009E);
    v = des_benes_step(v, 16, 0x0000D9D3);
/*  v = des_benes_step(v,  8, 0x00000000);  no-op, so we can skip it */
    v = des_benes_step(v,  4, 0x05040004);
/*  v = des_benes_step(v,  2, 0x00000000);  no-op, so we can skip it */
    v = des_benes_step(v,  1, 0x04045015);

    debug("P(%08"PRIx32") = %08"PRIx32"\n", v_orig, v);

    return v;
}

/*
 * Putting the S and P functions together, and adding in the round key
 * as well, gives us the full mixing function f.
 */

static inline uint32_t des_f(uint32_t R, uint32_t K7531, uint32_t K6420)
{
    uint32_t s7531 = R ^ K7531, s6420 = rol(R, 4) ^ K6420;
    return des_P(des_S(s6420, s7531));
}

/*
 * The key schedule, and the function to set it up.
 */

typedef struct des_keysched des_keysched;
struct des_keysched {
    uint32_t k7531[16], k6420[16];
};

/*
 * Simplistic function to select an arbitrary sequence of bits from
 * one value and glue them together into another value. bitnums[]
 * gives the sequence of bit indices of the input, from the highest
 * output bit downwards. An index of -1 means that output bit is left
 * at zero.
 *
 * This function is only used during key setup, so it doesn't need to
 * be highly optimised.
 */
static inline uint64_t bitsel(
    uint64_t input, const int8_t *bitnums, size_t size)
{
    uint64_t ret = 0;
    while (size-- > 0) {
        int bitpos = *bitnums++;
        ret <<= 1;
        if (bitpos >= 0)
            ret |= 1 & (input >> bitpos);
    }
    return ret;
}

void des_key_setup(uint64_t key, des_keysched *sched)
{
    static const int8_t PC1[] = {
         7, 15, 23, 31, 39, 47, 55, 63,  6, 14, 22, 30, 38, 46,
        54, 62,  5, 13, 21, 29, 37, 45, 53, 61,  4, 12, 20, 28,
        -1, -1, -1, -1,
         1,  9, 17, 25, 33, 41, 49, 57,  2, 10, 18, 26, 34, 42,
        50, 58,  3, 11, 19, 27, 35, 43, 51, 59, 36, 44, 52, 60,
    };
    static const int8_t PC2_7531[] = {
        46, 43, 49, 36, 59, 55, -1, -1, /* index into S-box 7 */
        37, 41, 48, 56, 34, 52, -1, -1, /* index into S-box 5 */
        15,  4, 25, 19,  9,  1, -1, -1, /* index into S-box 3 */
        12,  7, 17,  0, 22,  3, -1, -1, /* index into S-box 1 */
    };
    static const int8_t PC2_6420[] = {
        57, 32, 45, 54, 39, 50, -1, -1, /* index into S-box 6 */
        44, 53, 33, 40, 47, 58, -1, -1, /* index into S-box 4 */
        26, 16,  5, 11, 23,  8, -1, -1, /* index into S-box 2 */
        10, 14,  6, 20, 27, 24, -1, -1, /* index into S-box 0 */
    };
    static const int leftshifts[] = {1,1,2,2,2,2,2,2,1,2,2,2,2,2,2,1};

    /* Select 56 bits from the 64-bit input key integer (the low bit
     * of each input byte is unused), into a word consisting of two
     * 28-bit integers starting at bits 0 and 32. */
    uint64_t CD = bitsel(key, PC1, lenof(PC1));

    for (size_t i = 0; i < 16; i++) {
        /* Rotate each 28-bit half of CD left by 1 or 2 bits (varying
         * between rounds) */
        CD <<= leftshifts[i];
        CD = (CD & 0x0FFFFFFF0FFFFFFF) | ((CD & 0xF0000000F0000000) >> 28);

        /* Select key bits from the rotated word to use during the
         * actual cipher */
        sched->k7531[i] = bitsel(CD, PC2_7531, lenof(PC2_7531));
        sched->k6420[i] = bitsel(CD, PC2_6420, lenof(PC2_6420));
    }
}

/*
 * Helper routines for dealing with 64-bit blocks in the form of an L
 * and R word.
 */

typedef struct LR LR;
struct LR { uint32_t L, R; };

static inline LR des_load_lr(const void *vp)
{
    const uint8_t *p = (const uint8_t *)vp;
    LR out;
    out.L = GET_32BIT_MSB_FIRST(p);
    out.R = GET_32BIT_MSB_FIRST(p+4);
    return out;
}

static inline void des_store_lr(void *vp, LR lr)
{
    uint8_t *p = (uint8_t *)vp;
    PUT_32BIT_MSB_FIRST(p, lr.L);
    PUT_32BIT_MSB_FIRST(p+4, lr.R);
}

static inline LR des_xor_lr(LR a, LR b)
{
    a.L ^= b.L;
    a.R ^= b.R;
    return a;
}

static inline LR des_swap_lr(LR in)
{
    LR out;
    out.L = in.R;
    out.R = in.L;
    return out;
}

/*
 * The initial and final permutations of official DES are in a
 * restricted form, in which the 'before' and 'after' positions of a
 * given data bit are derived from each other by permuting the bits of
 * the _index_ and flipping some of them. This allows the permutation
 * to be performed effectively by a method that looks rather like
 * _half_ of a general Benes network, because the restricted form
 * means only half of it is actually needed.
 *
 * _Our_ initial and final permutations include a rotation by 1 bit,
 * but it's still easier to just suffix that to the standard IP/FP
 * than to regenerate everything using a more general method.
 *
 * Because we're permuting 64 bits in this case, between two 32-bit
 * words, there's a separate helper function for this code that
 * doesn't look quite like des_benes_step() above.
 */

static inline void des_bitswap_IP_FP(uint32_t *L, uint32_t *R,
                                     unsigned D, uint32_t mask)
{
    uint32_t diff = mask & ((*R >> D) ^ *L);
    *R ^= diff << D;
    *L ^= diff;
}

static inline LR des_IP(LR lr)
{
    des_bitswap_IP_FP(&lr.R, &lr.L,  4, 0x0F0F0F0F);
    des_bitswap_IP_FP(&lr.R, &lr.L, 16, 0x0000FFFF);
    des_bitswap_IP_FP(&lr.L, &lr.R,  2, 0x33333333);
    des_bitswap_IP_FP(&lr.L, &lr.R,  8, 0x00FF00FF);
    des_bitswap_IP_FP(&lr.R, &lr.L,  1, 0x55555555);

    lr.L = ror(lr.L, 1);
    lr.R = ror(lr.R, 1);

    return lr;
}

static inline LR des_FP(LR lr)
{
    lr.L = rol(lr.L, 1);
    lr.R = rol(lr.R, 1);

    des_bitswap_IP_FP(&lr.R, &lr.L,  1, 0x55555555);
    des_bitswap_IP_FP(&lr.L, &lr.R,  8, 0x00FF00FF);
    des_bitswap_IP_FP(&lr.L, &lr.R,  2, 0x33333333);
    des_bitswap_IP_FP(&lr.R, &lr.L, 16, 0x0000FFFF);
    des_bitswap_IP_FP(&lr.R, &lr.L,  4, 0x0F0F0F0F);

    return lr;
}

/*
 * The main cipher functions, which are identical except that they use
 * the key schedule in opposite orders.
 *
 * We provide a version without the initial and final permutations,
 * for use in triple-DES mode (no sense undoing and redoing it in
 * between the phases).
 */

static inline LR des_round(LR in, const des_keysched *sched, size_t round)
{
    LR out;
    out.L = in.R;
    out.R = in.L ^ des_f(in.R, sched->k7531[round], sched->k6420[round]);
    return out;
}

static inline LR des_inner_cipher(LR lr, const des_keysched *sched,
                                  size_t start, size_t step)
{ 
    lr = des_round(lr, sched, start+0x0*step);
    lr = des_round(lr, sched, start+0x1*step);
    lr = des_round(lr, sched, start+0x2*step);
    lr = des_round(lr, sched, start+0x3*step);
    lr = des_round(lr, sched, start+0x4*step);
    lr = des_round(lr, sched, start+0x5*step);
    lr = des_round(lr, sched, start+0x6*step);
    lr = des_round(lr, sched, start+0x7*step);
    lr = des_round(lr, sched, start+0x8*step);
    lr = des_round(lr, sched, start+0x9*step);
    lr = des_round(lr, sched, start+0xa*step);
    lr = des_round(lr, sched, start+0xb*step);
    lr = des_round(lr, sched, start+0xc*step);
    lr = des_round(lr, sched, start+0xd*step);
    lr = des_round(lr, sched, start+0xe*step);
    lr = des_round(lr, sched, start+0xf*step);
    return des_swap_lr(lr);
}

static inline LR des_full_cipher(LR lr, const des_keysched *sched,
                                 size_t start, size_t step)
{
    lr = des_IP(lr);
    lr = des_inner_cipher(lr, sched, start, step);
    lr = des_FP(lr);
    return lr;
}

/*
 * Parameter pairs for the start,step arguments to the cipher routines
 * above, causing them to use the same key schedule in opposite orders.
 */
#define ENCIPHER 0, 1                  /* for encryption */
#define DECIPHER 15, -1                /* for decryption */

/* ----------------------------------------------------------------------
 * Single-DES
 */

struct des_cbc_ctx {
    des_keysched sched;
    LR iv;
    ssh_cipher ciph;
};

static ssh_cipher *des_cbc_new(const ssh_cipheralg *alg)
{
    struct des_cbc_ctx *ctx = snew(struct des_cbc_ctx);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void des_cbc_free(ssh_cipher *ciph)
{
    struct des_cbc_ctx *ctx = container_of(ciph, struct des_cbc_ctx, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void des_cbc_setkey(ssh_cipher *ciph, const void *vkey)
{
    struct des_cbc_ctx *ctx = container_of(ciph, struct des_cbc_ctx, ciph);
    const uint8_t *key = (const uint8_t *)vkey;
    des_key_setup(GET_64BIT_MSB_FIRST(key), &ctx->sched);
}

static void des_cbc_setiv(ssh_cipher *ciph, const void *iv)
{
    struct des_cbc_ctx *ctx = container_of(ciph, struct des_cbc_ctx, ciph);
    ctx->iv = des_load_lr(iv);
}

static void des_cbc_encrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des_cbc_ctx *ctx = container_of(ciph, struct des_cbc_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        LR plaintext = des_load_lr(data);
        LR cipher_in = des_xor_lr(plaintext, ctx->iv);
        LR ciphertext = des_full_cipher(cipher_in, &ctx->sched, ENCIPHER);
        des_store_lr(data, ciphertext);
        ctx->iv = ciphertext;
    }
}

static void des_cbc_decrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des_cbc_ctx *ctx = container_of(ciph, struct des_cbc_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        LR ciphertext = des_load_lr(data);
        LR cipher_out = des_full_cipher(ciphertext, &ctx->sched, DECIPHER);
        LR plaintext = des_xor_lr(cipher_out, ctx->iv);
        des_store_lr(data, plaintext);
        ctx->iv = ciphertext;
    }
}

const ssh_cipheralg ssh_des = {
    des_cbc_new, des_cbc_free, des_cbc_setiv, des_cbc_setkey,
    des_cbc_encrypt, des_cbc_decrypt, NULL, NULL, "des-cbc",
    8, 56, 8, SSH_CIPHER_IS_CBC, "single-DES CBC", NULL
};

const ssh_cipheralg ssh_des_sshcom_ssh2 = {
    /* Same as ssh_des_cbc, but with a different SSH-2 ID */
    des_cbc_new, des_cbc_free, des_cbc_setiv, des_cbc_setkey,
    des_cbc_encrypt, des_cbc_decrypt, NULL, NULL, "des-cbc@ssh.com",
    8, 56, 8, SSH_CIPHER_IS_CBC, "single-DES CBC", NULL
};

static const ssh_cipheralg *const des_list[] = {
    &ssh_des,
    &ssh_des_sshcom_ssh2
};

const ssh2_ciphers ssh2_des = { lenof(des_list), des_list };

/* ----------------------------------------------------------------------
 * Triple-DES CBC, SSH-2 style. The CBC mode treats the three
 * invocations of DES as a single unified cipher, and surrounds it
 * with just one layer of CBC, so only one IV is needed.
 */

struct des3_cbc1_ctx {
    des_keysched sched[3];
    LR iv;
    ssh_cipher ciph;
};

static ssh_cipher *des3_cbc1_new(const ssh_cipheralg *alg)
{
    struct des3_cbc1_ctx *ctx = snew(struct des3_cbc1_ctx);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void des3_cbc1_free(ssh_cipher *ciph)
{
    struct des3_cbc1_ctx *ctx = container_of(ciph, struct des3_cbc1_ctx, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void des3_cbc1_setkey(ssh_cipher *ciph, const void *vkey)
{
    struct des3_cbc1_ctx *ctx = container_of(ciph, struct des3_cbc1_ctx, ciph);
    const uint8_t *key = (const uint8_t *)vkey;
    for (size_t i = 0; i < 3; i++)
        des_key_setup(GET_64BIT_MSB_FIRST(key + 8*i), &ctx->sched[i]);
}

static void des3_cbc1_setiv(ssh_cipher *ciph, const void *iv)
{
    struct des3_cbc1_ctx *ctx = container_of(ciph, struct des3_cbc1_ctx, ciph);
    ctx->iv = des_load_lr(iv);
}

static void des3_cbc1_cbc_encrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des3_cbc1_ctx *ctx = container_of(ciph, struct des3_cbc1_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        LR plaintext = des_load_lr(data);
        LR cipher_in = des_xor_lr(plaintext, ctx->iv);

        /* Run three copies of the cipher, without undoing and redoing
         * IP/FP in between. */
        LR lr = des_IP(cipher_in);
        lr = des_inner_cipher(lr, &ctx->sched[0], ENCIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[1], DECIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[2], ENCIPHER);
        LR ciphertext = des_FP(lr);

        des_store_lr(data, ciphertext);
        ctx->iv = ciphertext;
    }
}

static void des3_cbc1_cbc_decrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des3_cbc1_ctx *ctx = container_of(ciph, struct des3_cbc1_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        LR ciphertext = des_load_lr(data);

        /* Similarly to encryption, but with the order reversed. */
        LR lr = des_IP(ciphertext);
        lr = des_inner_cipher(lr, &ctx->sched[2], DECIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[1], ENCIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[0], DECIPHER);
        LR cipher_out = des_FP(lr);

        LR plaintext = des_xor_lr(cipher_out, ctx->iv);
        des_store_lr(data, plaintext);
        ctx->iv = ciphertext;
    }
}

const ssh_cipheralg ssh_3des_ssh2 = {
    des3_cbc1_new, des3_cbc1_free, des3_cbc1_setiv, des3_cbc1_setkey,
    des3_cbc1_cbc_encrypt, des3_cbc1_cbc_decrypt, NULL, NULL, "3des-cbc",
    8, 168, 24, SSH_CIPHER_IS_CBC, "triple-DES CBC", NULL
};

/* ----------------------------------------------------------------------
 * Triple-DES in SDCTR mode. Again, the three DES instances are
 * treated as one big cipher, with a single counter encrypted through
 * all three.
 */

#define SDCTR_WORDS (8 / BIGNUM_INT_BYTES)

struct des3_sdctr_ctx {
    des_keysched sched[3];
    BignumInt counter[SDCTR_WORDS];
    ssh_cipher ciph;
};

static ssh_cipher *des3_sdctr_new(const ssh_cipheralg *alg)
{
    struct des3_sdctr_ctx *ctx = snew(struct des3_sdctr_ctx);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void des3_sdctr_free(ssh_cipher *ciph)
{
    struct des3_sdctr_ctx *ctx = container_of(
        ciph, struct des3_sdctr_ctx, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void des3_sdctr_setkey(ssh_cipher *ciph, const void *vkey)
{
    struct des3_sdctr_ctx *ctx = container_of(
        ciph, struct des3_sdctr_ctx, ciph);
    const uint8_t *key = (const uint8_t *)vkey;
    for (size_t i = 0; i < 3; i++)
        des_key_setup(GET_64BIT_MSB_FIRST(key + 8*i), &ctx->sched[i]);
}

static void des3_sdctr_setiv(ssh_cipher *ciph, const void *viv)
{
    struct des3_sdctr_ctx *ctx = container_of(
        ciph, struct des3_sdctr_ctx, ciph);
    const uint8_t *iv = (const uint8_t *)viv;

    /* Import the initial counter value into the internal representation */
    for (unsigned i = 0; i < SDCTR_WORDS; i++)
        ctx->counter[i] = GET_BIGNUMINT_MSB_FIRST(
            iv + 8 - BIGNUM_INT_BYTES - i*BIGNUM_INT_BYTES);
}

static void des3_sdctr_encrypt_decrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des3_sdctr_ctx *ctx = container_of(
        ciph, struct des3_sdctr_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    uint8_t iv_buf[8];
    for (; len > 0; len -= 8, data += 8) {
        /* Format the counter value into the buffer. */
        for (unsigned i = 0; i < SDCTR_WORDS; i++)
            PUT_BIGNUMINT_MSB_FIRST(
                iv_buf + 8 - BIGNUM_INT_BYTES - i*BIGNUM_INT_BYTES,
                ctx->counter[i]);

        /* Increment the counter. */
        BignumCarry carry = 1;
        for (unsigned i = 0; i < SDCTR_WORDS; i++)
            BignumADC(ctx->counter[i], carry, ctx->counter[i], 0, carry);

        /* Triple-encrypt the counter value from the IV. */
        LR lr = des_IP(des_load_lr(iv_buf));
        lr = des_inner_cipher(lr, &ctx->sched[0], ENCIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[1], DECIPHER);
        lr = des_inner_cipher(lr, &ctx->sched[2], ENCIPHER);
        LR keystream = des_FP(lr);

        LR input = des_load_lr(data);
        LR output = des_xor_lr(input, keystream);
        des_store_lr(data, output);
    }
    smemclr(iv_buf, sizeof(iv_buf));
}

const ssh_cipheralg ssh_3des_ssh2_ctr = {
    des3_sdctr_new, des3_sdctr_free, des3_sdctr_setiv, des3_sdctr_setkey,
    des3_sdctr_encrypt_decrypt, des3_sdctr_encrypt_decrypt,
    NULL, NULL, "3des-ctr", 8, 168, 24, 0, "triple-DES SDCTR", NULL
};

static const ssh_cipheralg *const des3_list[] = {
    &ssh_3des_ssh2_ctr,
    &ssh_3des_ssh2
};

const ssh2_ciphers ssh2_3des = { lenof(des3_list), des3_list };

/* ----------------------------------------------------------------------
 * Triple-DES, SSH-1 style. SSH-1 replicated the whole CBC structure
 * three times, so there have to be three separate IVs, one in each
 * layer.
 */

struct des3_cbc3_ctx {
    des_keysched sched[3];
    LR iv[3];
    ssh_cipher ciph;
};

static ssh_cipher *des3_cbc3_new(const ssh_cipheralg *alg)
{
    struct des3_cbc3_ctx *ctx = snew(struct des3_cbc3_ctx);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void des3_cbc3_free(ssh_cipher *ciph)
{
    struct des3_cbc3_ctx *ctx = container_of(ciph, struct des3_cbc3_ctx, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void des3_cbc3_setkey(ssh_cipher *ciph, const void *vkey)
{
    struct des3_cbc3_ctx *ctx = container_of(ciph, struct des3_cbc3_ctx, ciph);
    const uint8_t *key = (const uint8_t *)vkey;
    for (size_t i = 0; i < 3; i++)
        des_key_setup(GET_64BIT_MSB_FIRST(key + 8*i), &ctx->sched[i]);
}

static void des3_cbc3_setiv(ssh_cipher *ciph, const void *viv)
{
    struct des3_cbc3_ctx *ctx = container_of(ciph, struct des3_cbc3_ctx, ciph);

    /*
     * In principle, we ought to provide an interface for the user to
     * input 24 instead of 8 bytes of IV. But that would make this an
     * ugly exception to the otherwise universal rule that IV size =
     * cipher block size, and there's really no need to violate that
     * rule given that this is a historical one-off oddity and SSH-1
     * always initialises all three IVs to zero anyway. So we fudge it
     * by just setting all the IVs to the same value.
     */

    LR iv = des_load_lr(viv);

    /* But we store the IVs in permuted form, so that we can handle
     * all three CBC layers without having to do IP/FP in between. */
    iv = des_IP(iv);
    for (size_t i = 0; i < 3; i++)
        ctx->iv[i] = iv;
}

static void des3_cbc3_cbc_encrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des3_cbc3_ctx *ctx = container_of(ciph, struct des3_cbc3_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        /* Load and IP the input. */
        LR plaintext = des_IP(des_load_lr(data));
        LR lr = plaintext;

        /* Do three passes of CBC, with the middle one inverted. */

        lr = des_xor_lr(lr, ctx->iv[0]);
        lr = des_inner_cipher(lr, &ctx->sched[0], ENCIPHER);
        ctx->iv[0] = lr;

        LR ciphertext = lr;
        lr = des_inner_cipher(ciphertext, &ctx->sched[1], DECIPHER);
        lr = des_xor_lr(lr, ctx->iv[1]);
        ctx->iv[1] = ciphertext;

        lr = des_xor_lr(lr, ctx->iv[2]);
        lr = des_inner_cipher(lr, &ctx->sched[2], ENCIPHER);
        ctx->iv[2] = lr;

        des_store_lr(data, des_FP(lr));
    }
}

static void des3_cbc3_cbc_decrypt(ssh_cipher *ciph, void *vdata, int len)
{
    struct des3_cbc3_ctx *ctx = container_of(ciph, struct des3_cbc3_ctx, ciph);
    uint8_t *data = (uint8_t *)vdata;
    for (; len > 0; len -= 8, data += 8) {
        /* Load and IP the input */
        LR lr = des_IP(des_load_lr(data));
        LR ciphertext;

        /* Do three passes of CBC, with the middle one inverted. */
        ciphertext = lr;
        lr = des_inner_cipher(ciphertext, &ctx->sched[2], DECIPHER);
        lr = des_xor_lr(lr, ctx->iv[2]);
        ctx->iv[2] = ciphertext;

        lr = des_xor_lr(lr, ctx->iv[1]);
        lr = des_inner_cipher(lr, &ctx->sched[1], ENCIPHER);
        ctx->iv[1] = lr;

        ciphertext = lr;
        lr = des_inner_cipher(ciphertext, &ctx->sched[0], DECIPHER);
        lr = des_xor_lr(lr, ctx->iv[0]);
        ctx->iv[0] = ciphertext;

        des_store_lr(data, des_FP(lr));
    }
}

const ssh_cipheralg ssh_3des_ssh1 = {
    des3_cbc3_new, des3_cbc3_free, des3_cbc3_setiv, des3_cbc3_setkey,
    des3_cbc3_cbc_encrypt, des3_cbc3_cbc_decrypt, NULL, NULL, NULL,
    8, 168, 24, SSH_CIPHER_IS_CBC, "triple-DES inner-CBC", NULL
};
