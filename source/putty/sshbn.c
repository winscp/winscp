/*
 * Bignum routines for RSA and DH and stuff.
 */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <ctype.h>

#include "misc.h"

#include "sshbn.h"

#define BIGNUM_INTERNAL
typedef BignumInt *Bignum;

#include "ssh.h"

BignumInt bnZero[1] = { 0 };
BignumInt bnOne[2] = { 1, 1 };
BignumInt bnTen[2] = { 1, 10 };

/*
 * The Bignum format is an array of `BignumInt'. The first
 * element of the array counts the remaining elements. The
 * remaining elements express the actual number, base 2^BIGNUM_INT_BITS, _least_
 * significant digit first. (So it's trivial to extract the bit
 * with value 2^n for any n.)
 *
 * All Bignums in this module are positive. Negative numbers must
 * be dealt with outside it.
 *
 * INVARIANT: the most significant word of any Bignum must be
 * nonzero.
 */

Bignum Zero = bnZero, One = bnOne, Ten = bnTen;

static Bignum newbn(int length)
{
    Bignum b;

    assert(length >= 0 && length < INT_MAX / BIGNUM_INT_BITS);

    b = snewn(length + 1, BignumInt);
    memset(b, 0, (length + 1) * sizeof(*b));
    b[0] = length;
    return b;
}

void bn_restore_invariant(Bignum b)
{
    while (b[0] > 1 && b[b[0]] == 0)
	b[0]--;
}

Bignum copybn(Bignum orig)
{
    Bignum b = snewn(orig[0] + 1, BignumInt);
    if (!b)
	abort();		       /* FIXME */
    memcpy(b, orig, (orig[0] + 1) * sizeof(*b));
    return b;
}

void freebn(Bignum b)
{
    /*
     * Burn the evidence, just in case.
     */
    smemclr(b, sizeof(b[0]) * (b[0] + 1));
    sfree(b);
}

Bignum bn_power_2(int n)
{
    Bignum ret;

    assert(n >= 0);

    ret = newbn(n / BIGNUM_INT_BITS + 1);
    bignum_set_bit(ret, n, 1);
    return ret;
}

/*
 * Internal addition. Sets c = a - b, where 'a', 'b' and 'c' are all
 * big-endian arrays of 'len' BignumInts. Returns the carry off the
 * top.
 */
static BignumCarry internal_add(const BignumInt *a, const BignumInt *b,
                                BignumInt *c, int len)
{
    int i;
    BignumCarry carry = 0;

    for (i = len-1; i >= 0; i--)
        BignumADC(c[i], carry, a[i], b[i], carry);

    return (BignumInt)carry;
}

/*
 * Internal subtraction. Sets c = a - b, where 'a', 'b' and 'c' are
 * all big-endian arrays of 'len' BignumInts. Any borrow from the top
 * is ignored.
 */
static void internal_sub(const BignumInt *a, const BignumInt *b,
                         BignumInt *c, int len)
{
    int i;
    BignumCarry carry = 1;

    for (i = len-1; i >= 0; i--)
        BignumADC(c[i], carry, a[i], ~b[i], carry);
}

/*
 * Compute c = a * b.
 * Input is in the first len words of a and b.
 * Result is returned in the first 2*len words of c.
 *
 * 'scratch' must point to an array of BignumInt of size at least
 * mul_compute_scratch(len). (This covers the needs of internal_mul
 * and all its recursive calls to itself.)
 */
#define KARATSUBA_THRESHOLD 50
static int mul_compute_scratch(int len)
{
    int ret = 0;
    while (len > KARATSUBA_THRESHOLD) {
        int toplen = len/2, botlen = len - toplen; /* botlen is the bigger */
        int midlen = botlen + 1;
        ret += 4*midlen;
        len = midlen;
    }
    return ret;
}
static void internal_mul(const BignumInt *a, const BignumInt *b,
			 BignumInt *c, int len, BignumInt *scratch)
{
    if (len > KARATSUBA_THRESHOLD) {
        int i;

        /*
         * Karatsuba divide-and-conquer algorithm. Cut each input in
         * half, so that it's expressed as two big 'digits' in a giant
         * base D:
         *
         *   a = a_1 D + a_0
         *   b = b_1 D + b_0
         *
         * Then the product is of course
         *
         *  ab = a_1 b_1 D^2 + (a_1 b_0 + a_0 b_1) D + a_0 b_0
         *
         * and we compute the three coefficients by recursively
         * calling ourself to do half-length multiplications.
         *
         * The clever bit that makes this worth doing is that we only
         * need _one_ half-length multiplication for the central
         * coefficient rather than the two that it obviouly looks
         * like, because we can use a single multiplication to compute
         *
         *   (a_1 + a_0) (b_1 + b_0) = a_1 b_1 + a_1 b_0 + a_0 b_1 + a_0 b_0
         *
         * and then we subtract the other two coefficients (a_1 b_1
         * and a_0 b_0) which we were computing anyway.
         *
         * Hence we get to multiply two numbers of length N in about
         * three times as much work as it takes to multiply numbers of
         * length N/2, which is obviously better than the four times
         * as much work it would take if we just did a long
         * conventional multiply.
         */

        int toplen = len/2, botlen = len - toplen; /* botlen is the bigger */
        int midlen = botlen + 1;
        BignumCarry carry;
#ifdef KARA_DEBUG
        int i;
#endif

        /*
         * The coefficients a_1 b_1 and a_0 b_0 just avoid overlapping
         * in the output array, so we can compute them immediately in
         * place.
         */

#ifdef KARA_DEBUG
        printf("a1,a0 = 0x");
        for (i = 0; i < len; i++) {
            if (i == toplen) printf(", 0x");
            printf("%0*x", BIGNUM_INT_BITS/4, a[i]);
        }
        printf("\n");
        printf("b1,b0 = 0x");
        for (i = 0; i < len; i++) {
            if (i == toplen) printf(", 0x");
            printf("%0*x", BIGNUM_INT_BITS/4, b[i]);
        }
        printf("\n");
#endif

        /* a_1 b_1 */
        internal_mul(a, b, c, toplen, scratch);
#ifdef KARA_DEBUG
        printf("a1b1 = 0x");
        for (i = 0; i < 2*toplen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, c[i]);
        }
        printf("\n");
#endif

        /* a_0 b_0 */
        internal_mul(a + toplen, b + toplen, c + 2*toplen, botlen, scratch);
#ifdef KARA_DEBUG
        printf("a0b0 = 0x");
        for (i = 0; i < 2*botlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, c[2*toplen+i]);
        }
        printf("\n");
#endif

        /* Zero padding. midlen exceeds toplen by at most 2, so just
         * zero the first two words of each input and the rest will be
         * copied over. */
        scratch[0] = scratch[1] = scratch[midlen] = scratch[midlen+1] = 0;

        for (i = 0; i < toplen; i++) {
            scratch[midlen - toplen + i] = a[i]; /* a_1 */
            scratch[2*midlen - toplen + i] = b[i]; /* b_1 */
        }

        /* compute a_1 + a_0 */
        scratch[0] = internal_add(scratch+1, a+toplen, scratch+1, botlen);
#ifdef KARA_DEBUG
        printf("a1plusa0 = 0x");
        for (i = 0; i < midlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, scratch[i]);
        }
        printf("\n");
#endif
        /* compute b_1 + b_0 */
        scratch[midlen] = internal_add(scratch+midlen+1, b+toplen,
                                       scratch+midlen+1, botlen);
#ifdef KARA_DEBUG
        printf("b1plusb0 = 0x");
        for (i = 0; i < midlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, scratch[midlen+i]);
        }
        printf("\n");
#endif

        /*
         * Now we can do the third multiplication.
         */
        internal_mul(scratch, scratch + midlen, scratch + 2*midlen, midlen,
                     scratch + 4*midlen);
#ifdef KARA_DEBUG
        printf("a1plusa0timesb1plusb0 = 0x");
        for (i = 0; i < 2*midlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, scratch[2*midlen+i]);
        }
        printf("\n");
#endif

        /*
         * Now we can reuse the first half of 'scratch' to compute the
         * sum of the outer two coefficients, to subtract from that
         * product to obtain the middle one.
         */
        scratch[0] = scratch[1] = scratch[2] = scratch[3] = 0;
        for (i = 0; i < 2*toplen; i++)
            scratch[2*midlen - 2*toplen + i] = c[i];
        scratch[1] = internal_add(scratch+2, c + 2*toplen,
                                  scratch+2, 2*botlen);
#ifdef KARA_DEBUG
        printf("a1b1plusa0b0 = 0x");
        for (i = 0; i < 2*midlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, scratch[i]);
        }
        printf("\n");
#endif

        internal_sub(scratch + 2*midlen, scratch,
                     scratch + 2*midlen, 2*midlen);
#ifdef KARA_DEBUG
        printf("a1b0plusa0b1 = 0x");
        for (i = 0; i < 2*midlen; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, scratch[2*midlen+i]);
        }
        printf("\n");
#endif

        /*
         * And now all we need to do is to add that middle coefficient
         * back into the output. We may have to propagate a carry
         * further up the output, but we can be sure it won't
         * propagate right the way off the top.
         */
        carry = internal_add(c + 2*len - botlen - 2*midlen,
                             scratch + 2*midlen,
                             c + 2*len - botlen - 2*midlen, 2*midlen);
        i = 2*len - botlen - 2*midlen - 1;
        while (carry) {
            assert(i >= 0);
            BignumADC(c[i], carry, c[i], 0, carry);
            i--;
        }
#ifdef KARA_DEBUG
        printf("ab = 0x");
        for (i = 0; i < 2*len; i++) {
            printf("%0*x", BIGNUM_INT_BITS/4, c[i]);
        }
        printf("\n");
#endif

    } else {
        int i;
        BignumInt carry;
        const BignumInt *ap, *bp;
        BignumInt *cp, *cps;

        /*
         * Multiply in the ordinary O(N^2) way.
         */

        for (i = 0; i < 2 * len; i++)
            c[i] = 0;

        for (cps = c + 2*len, ap = a + len; ap-- > a; cps--) {
            carry = 0;
            for (cp = cps, bp = b + len; cp--, bp-- > b ;)
                BignumMULADD2(carry, *cp, *ap, *bp, *cp, carry);
            *cp = carry;
        }
    }
}

/*
 * Variant form of internal_mul used for the initial step of
 * Montgomery reduction. Only bothers outputting 'len' words
 * (everything above that is thrown away).
 */
static void internal_mul_low(const BignumInt *a, const BignumInt *b,
                             BignumInt *c, int len, BignumInt *scratch)
{
    if (len > KARATSUBA_THRESHOLD) {
        int i;

        /*
         * Karatsuba-aware version of internal_mul_low. As before, we
         * express each input value as a shifted combination of two
         * halves:
         *
         *   a = a_1 D + a_0
         *   b = b_1 D + b_0
         *
         * Then the full product is, as before,
         *
         *  ab = a_1 b_1 D^2 + (a_1 b_0 + a_0 b_1) D + a_0 b_0
         *
         * Provided we choose D on the large side (so that a_0 and b_0
         * are _at least_ as long as a_1 and b_1), we don't need the
         * topmost term at all, and we only need half of the middle
         * term. So there's no point in doing the proper Karatsuba
         * optimisation which computes the middle term using the top
         * one, because we'd take as long computing the top one as
         * just computing the middle one directly.
         *
         * So instead, we do a much more obvious thing: we call the
         * fully optimised internal_mul to compute a_0 b_0, and we
         * recursively call ourself to compute the _bottom halves_ of
         * a_1 b_0 and a_0 b_1, each of which we add into the result
         * in the obvious way.
         *
         * In other words, there's no actual Karatsuba _optimisation_
         * in this function; the only benefit in doing it this way is
         * that we call internal_mul proper for a large part of the
         * work, and _that_ can optimise its operation.
         */

        int toplen = len/2, botlen = len - toplen; /* botlen is the bigger */

        /*
         * Scratch space for the various bits and pieces we're going
         * to be adding together: we need botlen*2 words for a_0 b_0
         * (though we may end up throwing away its topmost word), and
         * toplen words for each of a_1 b_0 and a_0 b_1. That adds up
         * to exactly 2*len.
         */

        /* a_0 b_0 */
        internal_mul(a + toplen, b + toplen, scratch + 2*toplen, botlen,
                     scratch + 2*len);

        /* a_1 b_0 */
        internal_mul_low(a, b + len - toplen, scratch + toplen, toplen,
                         scratch + 2*len);

        /* a_0 b_1 */
        internal_mul_low(a + len - toplen, b, scratch, toplen,
                         scratch + 2*len);

        /* Copy the bottom half of the big coefficient into place */
        for (i = 0; i < botlen; i++)
            c[toplen + i] = scratch[2*toplen + botlen + i];

        /* Add the two small coefficients, throwing away the returned carry */
        internal_add(scratch, scratch + toplen, scratch, toplen);

        /* And add that to the large coefficient, leaving the result in c. */
        internal_add(scratch, scratch + 2*toplen + botlen - toplen,
                     c, toplen);

    } else {
        int i;
        BignumInt carry;
        const BignumInt *ap, *bp;
        BignumInt *cp, *cps;

        /*
         * Multiply in the ordinary O(N^2) way.
         */

        for (i = 0; i < len; i++)
            c[i] = 0;

        for (cps = c + len, ap = a + len; ap-- > a; cps--) {
            carry = 0;
            for (cp = cps, bp = b + len; bp--, cp-- > c ;)
                BignumMULADD2(carry, *cp, *ap, *bp, *cp, carry);
        }
    }
}

/*
 * Montgomery reduction. Expects x to be a big-endian array of 2*len
 * BignumInts whose value satisfies 0 <= x < rn (where r = 2^(len *
 * BIGNUM_INT_BITS) is the Montgomery base). Returns in the same array
 * a value x' which is congruent to xr^{-1} mod n, and satisfies 0 <=
 * x' < n.
 *
 * 'n' and 'mninv' should be big-endian arrays of 'len' BignumInts
 * each, containing respectively n and the multiplicative inverse of
 * -n mod r.
 *
 * 'tmp' is an array of BignumInt used as scratch space, of length at
 * least 3*len + mul_compute_scratch(len).
 */
static void monty_reduce(BignumInt *x, const BignumInt *n,
                         const BignumInt *mninv, BignumInt *tmp, int len)
{
    int i;
    BignumInt carry;

    /*
     * Multiply x by (-n)^{-1} mod r. This gives us a value m such
     * that mn is congruent to -x mod r. Hence, mn+x is an exact
     * multiple of r, and is also (obviously) congruent to x mod n.
     */
    internal_mul_low(x + len, mninv, tmp, len, tmp + 3*len);

    /*
     * Compute t = (mn+x)/r in ordinary, non-modular, integer
     * arithmetic. By construction this is exact, and is congruent mod
     * n to x * r^{-1}, i.e. the answer we want.
     *
     * The following multiply leaves that answer in the _most_
     * significant half of the 'x' array, so then we must shift it
     * down.
     */
    internal_mul(tmp, n, tmp+len, len, tmp + 3*len);
    carry = internal_add(x, tmp+len, x, 2*len);
    for (i = 0; i < len; i++)
        x[len + i] = x[i], x[i] = 0;

    /*
     * Reduce t mod n. This doesn't require a full-on division by n,
     * but merely a test and single optional subtraction, since we can
     * show that 0 <= t < 2n.
     *
     * Proof:
     *  + we computed m mod r, so 0 <= m < r.
     *  + so 0 <= mn < rn, obviously
     *  + hence we only need 0 <= x < rn to guarantee that 0 <= mn+x < 2rn
     *  + yielding 0 <= (mn+x)/r < 2n as required.
     */
    if (!carry) {
        for (i = 0; i < len; i++)
            if (x[len + i] != n[i])
                break;
    }
    if (carry || i >= len || x[len + i] > n[i])
        internal_sub(x+len, n, x+len, len);
}

static void internal_add_shifted(BignumInt *number,
				 BignumInt n, int shift)
{
    int word = 1 + (shift / BIGNUM_INT_BITS);
    int bshift = shift % BIGNUM_INT_BITS;
    BignumInt addendh, addendl;
    BignumCarry carry;

    addendl = n << bshift;
    addendh = (bshift == 0 ? 0 : n >> (BIGNUM_INT_BITS - bshift));

    assert(word <= number[0]);
    BignumADC(number[word], carry, number[word], addendl, 0);
    word++;
    if (!addendh && !carry)
        return;
    assert(word <= number[0]);
    BignumADC(number[word], carry, number[word], addendh, carry);
    word++;
    while (carry) {
        assert(word <= number[0]);
        BignumADC(number[word], carry, number[word], 0, carry);
	word++;
    }
}

static int bn_clz(BignumInt x)
{
    /*
     * Count the leading zero bits in x. Equivalently, how far left
     * would we need to shift x to make its top bit set?
     *
     * Precondition: x != 0.
     */

    /* FIXME: would be nice to put in some compiler intrinsics under
     * ifdef here */
    int i, ret = 0;
    for (i = BIGNUM_INT_BITS / 2; i != 0; i >>= 1) {
        if ((x >> (BIGNUM_INT_BITS-i)) == 0) {
            x <<= i;
            ret += i;
        }
    }
    return ret;
}

static BignumInt reciprocal_word(BignumInt d)
{
    BignumInt dshort, recip, prodh, prodl;
    int corrections;

    /*
     * Input: a BignumInt value d, with its top bit set.
     */
    assert(d >> (BIGNUM_INT_BITS-1) == 1);

    /*
     * Output: a value, shifted to fill a BignumInt, which is strictly
     * less than 1/(d+1), i.e. is an *under*-estimate (but by as
     * little as possible within the constraints) of the reciprocal of
     * any number whose first BIGNUM_INT_BITS bits match d.
     *
     * Ideally we'd like to _totally_ fill BignumInt, i.e. always
     * return a value with the top bit set. Unfortunately we can't
     * quite guarantee that for all inputs and also return a fixed
     * exponent. So instead we take our reciprocal to be
     * 2^(BIGNUM_INT_BITS*2-1) / d, so that it has the top bit clear
     * only in the exceptional case where d takes exactly the maximum
     * value BIGNUM_INT_MASK; in that case, the top bit is clear and
     * the next bit down is set.
     */

    /*
     * Start by computing a half-length version of the answer, by
     * straightforward division within a BignumInt.
     */
    dshort = (d >> (BIGNUM_INT_BITS/2)) + 1;
    recip = (BIGNUM_TOP_BIT + dshort - 1) / dshort;
    recip <<= BIGNUM_INT_BITS - BIGNUM_INT_BITS/2;

    /*
     * Newton-Raphson iteration to improve that starting reciprocal
     * estimate: take f(x) = d - 1/x, and then the N-R formula gives
     * x_new = x - f(x)/f'(x) = x - (d-1/x)/(1/x^2) = x(2-d*x). Or,
     * taking our fixed-point representation into account, take f(x)
     * to be d - K/x (where K = 2^(BIGNUM_INT_BITS*2-1) as discussed
     * above) and then we get (2K - d*x) * x/K.
     *
     * Newton-Raphson doubles the number of correct bits at every
     * iteration, and the initial division above already gave us half
     * the output word, so it's only worth doing one iteration.
     */
    BignumMULADD(prodh, prodl, recip, d, recip);
    prodl = ~prodl;
    prodh = ~prodh;
    {
        BignumCarry c;
        BignumADC(prodl, c, prodl, 1, 0);
        prodh += c;
    }
    BignumMUL(prodh, prodl, prodh, recip);
    recip = (prodh << 1) | (prodl >> (BIGNUM_INT_BITS-1));

    /*
     * Now make sure we have the best possible reciprocal estimate,
     * before we return it. We might have been off by a handful either
     * way - not enough to bother with any better-thought-out kind of
     * correction loop.
     */
    BignumMULADD(prodh, prodl, recip, d, recip);
    corrections = 0;
    if (prodh >= BIGNUM_TOP_BIT) {
        do {
            BignumCarry c = 1;
            BignumADC(prodl, c, prodl, ~d, c); prodh += BIGNUM_INT_MASK + c;
            recip--;
            corrections++;
        } while (prodh >= ((BignumInt)1 << (BIGNUM_INT_BITS-1)));
    } else {
        while (1) {
            BignumInt newprodh, newprodl;
            BignumCarry c = 0;
            BignumADC(newprodl, c, prodl, d, c); newprodh = prodh + c;
            if (newprodh >= BIGNUM_TOP_BIT)
                break;
            prodh = newprodh;
            prodl = newprodl;
            recip++;
            corrections++;
        }
    }

    return recip;
}

/*
 * Compute a = a % m.
 * Input in first alen words of a and first mlen words of m.
 * Output in first alen words of a
 * (of which first alen-mlen words will be zero).
 * Quotient is accumulated in the `quotient' array, which is a Bignum
 * rather than the internal bigendian format.
 *
 * 'recip' must be the result of calling reciprocal_word() on the top
 * BIGNUM_INT_BITS of the modulus (denoted m0 in comments below), with
 * the topmost set bit normalised to the MSB of the input to
 * reciprocal_word. 'rshift' is how far left the top nonzero word of
 * the modulus had to be shifted to set that top bit.
 */
static void internal_mod(BignumInt *a, int alen,
			 BignumInt *m, int mlen,
			 BignumInt *quot, BignumInt recip, int rshift)
{
    int i, k;

#ifdef DIVISION_DEBUG
    {
        int d;
        printf("start division, m=0x");
        for (d = 0; d < mlen; d++)
            printf("%0*llx", BIGNUM_INT_BITS/4, (unsigned long long)m[d]);
        printf(", recip=%#0*llx, rshift=%d\n",
               BIGNUM_INT_BITS/4, (unsigned long long)recip, rshift);
    }
#endif

    /*
     * Repeatedly use that reciprocal estimate to get a decent number
     * of quotient bits, and subtract off the resulting multiple of m.
     *
     * Normally we expect to terminate this loop by means of finding
     * out q=0 part way through, but one way in which we might not get
     * that far in the first place is if the input a is actually zero,
     * in which case we'll discard zero words from the front of a
     * until we reach the termination condition in the for statement
     * here.
     */
    for (i = 0; i <= alen - mlen ;) {
	BignumInt product;
        BignumInt aword, q;
        int shift, full_bitoffset, bitoffset, wordoffset;

#ifdef DIVISION_DEBUG
        {
            int d;
            printf("main loop, a=0x");
            for (d = 0; d < alen; d++)
                printf("%0*llx", BIGNUM_INT_BITS/4, (unsigned long long)a[d]);
            printf("\n");
        }
#endif

        if (a[i] == 0) {
#ifdef DIVISION_DEBUG
            printf("zero word at i=%d\n", i);
#endif
            i++;
            continue;
        }

        aword = a[i];
        shift = bn_clz(aword);
        aword <<= shift;
        if (shift > 0 && i+1 < alen)
            aword |= a[i+1] >> (BIGNUM_INT_BITS - shift);

        {
            BignumInt unused;
            BignumMUL(q, unused, recip, aword);
            (void)unused;
        }

#ifdef DIVISION_DEBUG
        printf("i=%d, aword=%#0*llx, shift=%d, q=%#0*llx\n",
               i, BIGNUM_INT_BITS/4, (unsigned long long)aword,
               shift, BIGNUM_INT_BITS/4, (unsigned long long)q);
#endif

        /*
         * Work out the right bit and word offsets to use when
         * subtracting q*m from a.
         *
         * aword was taken from a[i], which means its LSB was at bit
         * position (alen-1-i) * BIGNUM_INT_BITS. But then we shifted
         * it left by 'shift', so now the low bit of aword corresponds
         * to bit position (alen-1-i) * BIGNUM_INT_BITS - shift, i.e.
         * aword is approximately equal to a / 2^(that).
         *
         * m0 comes from the top word of mod, so its LSB is at bit
         * position (mlen-1) * BIGNUM_INT_BITS - rshift, i.e. it can
         * be considered to be m / 2^(that power). 'recip' is the
         * reciprocal of m0, times 2^(BIGNUM_INT_BITS*2-1), i.e. it's
         * about 2^((mlen+1) * BIGNUM_INT_BITS - rshift - 1) / m.
         *
         * Hence, recip * aword is approximately equal to the product
         * of those, which simplifies to
         *
         * a/m * 2^((mlen+2+i-alen)*BIGNUM_INT_BITS + shift - rshift - 1)
         *
         * But we've also shifted recip*aword down by BIGNUM_INT_BITS
         * to form q, so we have
         *
         * q ~= a/m * 2^((mlen+1+i-alen)*BIGNUM_INT_BITS + shift - rshift - 1)
         *
         * and hence, when we now compute q*m, it will be about
         * a*2^(all that lot), i.e. the negation of that expression is
         * how far left we have to shift the product q*m to make it
         * approximately equal to a.
         */
        full_bitoffset = -((mlen+1+i-alen)*BIGNUM_INT_BITS + shift-rshift-1);
#ifdef DIVISION_DEBUG
        printf("full_bitoffset=%d\n", full_bitoffset);
#endif

        if (full_bitoffset < 0) {
            /*
             * If we find ourselves needing to shift q*m _right_, that
             * means we've reached the bottom of the quotient. Clip q
             * so that its right shift becomes zero, and if that means
             * q becomes _actually_ zero, this loop is done.
             */
            if (full_bitoffset <= -BIGNUM_INT_BITS)
                break;
            q >>= -full_bitoffset;
            full_bitoffset = 0;
            if (!q)
                break;
#ifdef DIVISION_DEBUG
            printf("now full_bitoffset=%d, q=%#0*llx\n",
                   full_bitoffset, BIGNUM_INT_BITS/4, (unsigned long long)q);
#endif
        }

        wordoffset = full_bitoffset / BIGNUM_INT_BITS;
        bitoffset = full_bitoffset % BIGNUM_INT_BITS;
#ifdef DIVISION_DEBUG
        printf("wordoffset=%d, bitoffset=%d\n", wordoffset, bitoffset);
#endif

        /* wordoffset as computed above is the offset between the LSWs
         * of m and a. But in fact m and a are stored MSW-first, so we
         * need to adjust it to be the offset between the actual array
         * indices, and flip the sign too. */
        wordoffset = alen - mlen - wordoffset;

        if (bitoffset == 0) {
            BignumCarry c = 1;
            BignumInt prev_hi_word = 0;
            for (k = mlen - 1; wordoffset+k >= i; k--) {
                BignumInt mword = k<0 ? 0 : m[k];
                BignumMULADD(prev_hi_word, product, q, mword, prev_hi_word);
#ifdef DIVISION_DEBUG
                printf("  aligned sub: product word for m[%d] = %#0*llx\n",
                       k, BIGNUM_INT_BITS/4,
                       (unsigned long long)product);
#endif
#ifdef DIVISION_DEBUG
                printf("  aligned sub: subtrahend for a[%d] = %#0*llx\n",
                       wordoffset+k, BIGNUM_INT_BITS/4,
                       (unsigned long long)product);
#endif
                BignumADC(a[wordoffset+k], c, a[wordoffset+k], ~product, c);
            }
        } else {
            BignumInt add_word = 0;
            BignumInt c = 1;
            BignumInt prev_hi_word = 0;
            for (k = mlen - 1; wordoffset+k >= i; k--) {
                BignumInt mword = k<0 ? 0 : m[k];
                BignumMULADD(prev_hi_word, product, q, mword, prev_hi_word);
#ifdef DIVISION_DEBUG
                printf("  unaligned sub: product word for m[%d] = %#0*llx\n",
                       k, BIGNUM_INT_BITS/4,
                       (unsigned long long)product);
#endif

                add_word |= product << bitoffset;

#ifdef DIVISION_DEBUG
                printf("  unaligned sub: subtrahend for a[%d] = %#0*llx\n",
                       wordoffset+k,
                       BIGNUM_INT_BITS/4, (unsigned long long)add_word);
#endif
                BignumADC(a[wordoffset+k], c, a[wordoffset+k], ~add_word, c);

                add_word = product >> (BIGNUM_INT_BITS - bitoffset);
            }
        }

	if (quot) {
#ifdef DIVISION_DEBUG
            printf("adding quotient word %#0*llx << %d\n",
                   BIGNUM_INT_BITS/4, (unsigned long long)q, full_bitoffset);
#endif
	    internal_add_shifted(quot, q, full_bitoffset);
#ifdef DIVISION_DEBUG
            {
                int d;
                printf("now quot=0x");
                for (d = quot[0]; d > 0; d--)
                    printf("%0*llx", BIGNUM_INT_BITS/4,
                           (unsigned long long)quot[d]);
                printf("\n");
            }
#endif
        }
    }

#ifdef DIVISION_DEBUG
    {
        int d;
        printf("end main loop, a=0x");
        for (d = 0; d < alen; d++)
            printf("%0*llx", BIGNUM_INT_BITS/4, (unsigned long long)a[d]);
        if (quot) {
            printf(", quot=0x");
            for (d = quot[0]; d > 0; d--)
                printf("%0*llx", BIGNUM_INT_BITS/4,
                       (unsigned long long)quot[d]);
        }
        printf("\n");
    }
#endif

    /*
     * The above loop should terminate with the remaining value in a
     * being strictly less than 2*m (if a >= 2*m then we should always
     * have managed to get a nonzero q word), but we can't guarantee
     * that it will be strictly less than m: consider a case where the
     * remainder is 1, and another where the remainder is m-1. By the
     * time a contains a value that's _about m_, you clearly can't
     * distinguish those cases by looking at only the top word of a -
     * you have to go all the way down to the bottom before you find
     * out whether it's just less or just more than m.
     *
     * Hence, we now do a final fixup in which we subtract one last
     * copy of m, or don't, accordingly. We should never have to
     * subtract more than one copy of m here.
     */
    for (i = 0; i < alen; i++) {
        /* Compare a with m, word by word, from the MSW down. As soon
         * as we encounter a difference, we know whether we need the
         * fixup. */
        int mindex = mlen-alen+i;
        BignumInt mword = mindex < 0 ? 0 : m[mindex];
        if (a[i] < mword) {
#ifdef DIVISION_DEBUG
            printf("final fixup not needed, a < m\n");
#endif
            return;
        } else if (a[i] > mword) {
#ifdef DIVISION_DEBUG
            printf("final fixup is needed, a > m\n");
#endif
            break;
        }
        /* If neither of those cases happened, the words are the same,
         * so keep going and look at the next one. */
    }
#ifdef DIVISION_DEBUG
    if (i == mlen) /* if we printed neither of the above diagnostics */
        printf("final fixup is needed, a == m\n");
#endif

    /*
     * If we got here without returning, then a >= m, so we must
     * subtract m, and increment the quotient.
     */
    {
        BignumCarry c = 1;
        for (i = alen - 1; i >= 0; i--) {
            int mindex = mlen-alen+i;
            BignumInt mword = mindex < 0 ? 0 : m[mindex];
            BignumADC(a[i], c, a[i], ~mword, c);
        }
    }
    if (quot)
        internal_add_shifted(quot, 1, 0);

#ifdef DIVISION_DEBUG
    {
        int d;
        printf("after final fixup, a=0x");
        for (d = 0; d < alen; d++)
            printf("%0*llx", BIGNUM_INT_BITS/4, (unsigned long long)a[d]);
        if (quot) {
            printf(", quot=0x");
            for (d = quot[0]; d > 0; d--)
                printf("%0*llx", BIGNUM_INT_BITS/4,
                       (unsigned long long)quot[d]);
        }
        printf("\n");
    }
#endif
}

/*
 * Compute (base ^ exp) % mod, the pedestrian way.
 */
Bignum modpow_simple(Bignum base_in, Bignum exp, Bignum mod)
{
    BignumInt *a, *b, *n, *m, *scratch;
    BignumInt recip;
    int rshift;
    int mlen, scratchlen, i, j;
    Bignum base, result;

    /*
     * The most significant word of mod needs to be non-zero. It
     * should already be, but let's make sure.
     */
    assert(mod[mod[0]] != 0);

    /*
     * Make sure the base is smaller than the modulus, by reducing
     * it modulo the modulus if not.
     */
    base = bigmod(base_in, mod);

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = snewn(mlen, BignumInt);
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Allocate n of size mlen, copy base to n */
    n = snewn(mlen, BignumInt);
    i = mlen - base[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < (int)base[0]; j++)
	n[i + j] = base[base[0] - j];

    /* Allocate a and b of size 2*mlen. Set a = 1 */
    a = snewn(2 * mlen, BignumInt);
    b = snewn(2 * mlen, BignumInt);
    for (i = 0; i < 2 * mlen; i++)
	a[i] = 0;
    a[2 * mlen - 1] = 1;

    /* Scratch space for multiplies */
    scratchlen = mul_compute_scratch(mlen);
    scratch = snewn(scratchlen, BignumInt);

    /* Skip leading zero bits of exp. */
    i = 0;
    j = BIGNUM_INT_BITS-1;
    while (i < (int)exp[0] && (exp[exp[0] - i] & ((BignumInt)1 << j)) == 0) {
	j--;
	if (j < 0) {
	    i++;
	    j = BIGNUM_INT_BITS-1;
	}
    }

    /* Compute reciprocal of the top full word of the modulus */
    {
        BignumInt m0 = m[0];
        rshift = bn_clz(m0);
        if (rshift) {
            m0 <<= rshift;
            if (mlen > 1)
                m0 |= m[1] >> (BIGNUM_INT_BITS - rshift);
        }
        recip = reciprocal_word(m0);
    }

    /* Main computation */
    while (i < (int)exp[0]) {
	while (j >= 0) {
	    internal_mul(a + mlen, a + mlen, b, mlen, scratch);
	    internal_mod(b, mlen * 2, m, mlen, NULL, recip, rshift);
	    if ((exp[exp[0] - i] & ((BignumInt)1 << j)) != 0) {
		internal_mul(b + mlen, n, a, mlen, scratch);
		internal_mod(a, mlen * 2, m, mlen, NULL, recip, rshift);
	    } else {
		BignumInt *t;
		t = a;
		a = b;
		b = t;
	    }
	    j--;
	}
	i++;
	j = BIGNUM_INT_BITS-1;
    }

    /* Copy result to buffer */
    result = newbn(mod[0]);
    for (i = 0; i < mlen; i++)
	result[result[0] - i] = a[i + mlen];
    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;

    /* Free temporary arrays */
    smemclr(a, 2 * mlen * sizeof(*a));
    sfree(a);
    smemclr(scratch, scratchlen * sizeof(*scratch));
    sfree(scratch);
    smemclr(b, 2 * mlen * sizeof(*b));
    sfree(b);
    smemclr(m, mlen * sizeof(*m));
    sfree(m);
    smemclr(n, mlen * sizeof(*n));
    sfree(n);

    freebn(base);

    return result;
}

/*
 * Compute (base ^ exp) % mod. Uses the Montgomery multiplication
 * technique where possible, falling back to modpow_simple otherwise.
 */
Bignum modpow(Bignum base_in, Bignum exp, Bignum mod)
{
    BignumInt *a, *b, *x, *n, *mninv, *scratch;
    int len, scratchlen, i, j;
    Bignum base, base2, r, rn, inv, result;

    /*
     * The most significant word of mod needs to be non-zero. It
     * should already be, but let's make sure.
     */
    assert(mod[mod[0]] != 0);

    /*
     * mod had better be odd, or we can't do Montgomery multiplication
     * using a power of two at all.
     */
    if (!(mod[1] & 1))
        return modpow_simple(base_in, exp, mod);

    /*
     * Make sure the base is smaller than the modulus, by reducing
     * it modulo the modulus if not.
     */
    base = bigmod(base_in, mod);

    /*
     * Compute the inverse of n mod r, for monty_reduce. (In fact we
     * want the inverse of _minus_ n mod r, but we'll sort that out
     * below.)
     */
    len = mod[0];
    r = bn_power_2(BIGNUM_INT_BITS * len);
    inv = modinv(mod, r);
    assert(inv); /* cannot fail, since mod is odd and r is a power of 2 */

    /*
     * Multiply the base by r mod n, to get it into Montgomery
     * representation.
     */
    base2 = modmul(base, r, mod);
    freebn(base);
    base = base2;

    rn = bigmod(r, mod);               /* r mod n, i.e. Montgomerified 1 */

    freebn(r);                         /* won't need this any more */

    /*
     * Set up internal arrays of the right lengths, in big-endian
     * format, containing the base, the modulus, and the modulus's
     * inverse.
     */
    n = snewn(len, BignumInt);
    for (j = 0; j < len; j++)
	n[len - 1 - j] = mod[j + 1];

    mninv = snewn(len, BignumInt);
    for (j = 0; j < len; j++)
	mninv[len - 1 - j] = (j < (int)inv[0] ? inv[j + 1] : 0);
    freebn(inv);         /* we don't need this copy of it any more */
    /* Now negate mninv mod r, so it's the inverse of -n rather than +n. */
    x = snewn(len, BignumInt);
    for (j = 0; j < len; j++)
        x[j] = 0;
    internal_sub(x, mninv, mninv, len);

    /* x = snewn(len, BignumInt); */ /* already done above */
    for (j = 0; j < len; j++)
	x[len - 1 - j] = (j < (int)base[0] ? base[j + 1] : 0);
    freebn(base);        /* we don't need this copy of it any more */

    a = snewn(2*len, BignumInt);
    b = snewn(2*len, BignumInt);
    for (j = 0; j < len; j++)
	a[2*len - 1 - j] = (j < (int)rn[0] ? rn[j + 1] : 0);
    freebn(rn);

    /* Scratch space for multiplies */
    scratchlen = 3*len + mul_compute_scratch(len);
    scratch = snewn(scratchlen, BignumInt);

    /* Skip leading zero bits of exp. */
    i = 0;
    j = BIGNUM_INT_BITS-1;
    while (i < (int)exp[0] && (exp[exp[0] - i] & ((BignumInt)1 << j)) == 0) {
	j--;
	if (j < 0) {
	    i++;
	    j = BIGNUM_INT_BITS-1;
	}
    }

    /* Main computation */
    while (i < (int)exp[0]) {
	while (j >= 0) {
	    internal_mul(a + len, a + len, b, len, scratch);
            monty_reduce(b, n, mninv, scratch, len);
	    if ((exp[exp[0] - i] & ((BignumInt)1 << j)) != 0) {
                internal_mul(b + len, x, a, len,  scratch);
                monty_reduce(a, n, mninv, scratch, len);
	    } else {
		BignumInt *t;
		t = a;
		a = b;
		b = t;
	    }
	    j--;
	}
	i++;
	j = BIGNUM_INT_BITS-1;
    }

    /*
     * Final monty_reduce to get back from the adjusted Montgomery
     * representation.
     */
    monty_reduce(a, n, mninv, scratch, len);

    /* Copy result to buffer */
    result = newbn(mod[0]);
    for (i = 0; i < len; i++)
	result[result[0] - i] = a[i + len];
    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;

    /* Free temporary arrays */
    smemclr(scratch, scratchlen * sizeof(*scratch));
    sfree(scratch);
    smemclr(a, 2 * len * sizeof(*a));
    sfree(a);
    smemclr(b, 2 * len * sizeof(*b));
    sfree(b);
    smemclr(mninv, len * sizeof(*mninv));
    sfree(mninv);
    smemclr(n, len * sizeof(*n));
    sfree(n);
    smemclr(x, len * sizeof(*x));
    sfree(x);

    return result;
}

/*
 * Compute (p * q) % mod.
 * The most significant word of mod MUST be non-zero.
 * We assume that the result array is the same size as the mod array.
 */
Bignum modmul(Bignum p, Bignum q, Bignum mod)
{
    BignumInt *a, *n, *m, *o, *scratch;
    BignumInt recip;
    int rshift, scratchlen;
    int pqlen, mlen, rlen, i, j;
    Bignum result;

    /*
     * The most significant word of mod needs to be non-zero. It
     * should already be, but let's make sure.
     */
    assert(mod[mod[0]] != 0);

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = snewn(mlen, BignumInt);
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    pqlen = (p[0] > q[0] ? p[0] : q[0]);

    /*
     * Make sure that we're allowing enough space. The shifting below
     * will underflow the vectors we allocate if pqlen is too small.
     */
    if (2*pqlen <= mlen)
        pqlen = mlen/2 + 1;

    /* Allocate n of size pqlen, copy p to n */
    n = snewn(pqlen, BignumInt);
    i = pqlen - p[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < (int)p[0]; j++)
	n[i + j] = p[p[0] - j];

    /* Allocate o of size pqlen, copy q to o */
    o = snewn(pqlen, BignumInt);
    i = pqlen - q[0];
    for (j = 0; j < i; j++)
	o[j] = 0;
    for (j = 0; j < (int)q[0]; j++)
	o[i + j] = q[q[0] - j];

    /* Allocate a of size 2*pqlen for result */
    a = snewn(2 * pqlen, BignumInt);

    /* Scratch space for multiplies */
    scratchlen = mul_compute_scratch(pqlen);
    scratch = snewn(scratchlen, BignumInt);

    /* Compute reciprocal of the top full word of the modulus */
    {
        BignumInt m0 = m[0];
        rshift = bn_clz(m0);
        if (rshift) {
            m0 <<= rshift;
            if (mlen > 1)
                m0 |= m[1] >> (BIGNUM_INT_BITS - rshift);
        }
        recip = reciprocal_word(m0);
    }

    /* Main computation */
    internal_mul(n, o, a, pqlen, scratch);
    internal_mod(a, pqlen * 2, m, mlen, NULL, recip, rshift);

    /* Copy result to buffer */
    rlen = (mlen < pqlen * 2 ? mlen : pqlen * 2);
    result = newbn(rlen);
    for (i = 0; i < rlen; i++)
	result[result[0] - i] = a[i + 2 * pqlen - rlen];
    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;

    /* Free temporary arrays */
    smemclr(scratch, scratchlen * sizeof(*scratch));
    sfree(scratch);
    smemclr(a, 2 * pqlen * sizeof(*a));
    sfree(a);
    smemclr(m, mlen * sizeof(*m));
    sfree(m);
    smemclr(n, pqlen * sizeof(*n));
    sfree(n);
    smemclr(o, pqlen * sizeof(*o));
    sfree(o);

    return result;
}

Bignum modsub(const Bignum a, const Bignum b, const Bignum n)
{
    Bignum a1, b1, ret;

    if (bignum_cmp(a, n) >= 0) a1 = bigmod(a, n);
    else a1 = a;
    if (bignum_cmp(b, n) >= 0) b1 = bigmod(b, n);
    else b1 = b;

    if (bignum_cmp(a1, b1) >= 0) /* a >= b */
    {
        ret = bigsub(a1, b1);
    }
    else
    {
        /* Handle going round the corner of the modulus without having
         * negative support in Bignum */
        Bignum tmp = bigsub(n, b1);
        assert(tmp);
        ret = bigadd(tmp, a1);
        freebn(tmp);
    }

    if (a != a1) freebn(a1);
    if (b != b1) freebn(b1);

    return ret;
}

/*
 * Compute p % mod.
 * The most significant word of mod MUST be non-zero.
 * We assume that the result array is the same size as the mod array.
 * We optionally write out a quotient if `quotient' is non-NULL.
 * We can avoid writing out the result if `result' is NULL.
 */
static void bigdivmod(Bignum p, Bignum mod, Bignum result, Bignum quotient)
{
    BignumInt *n, *m;
    BignumInt recip;
    int rshift;
    int plen, mlen, i, j;

    /*
     * The most significant word of mod needs to be non-zero. It
     * should already be, but let's make sure.
     */
    assert(mod[mod[0]] != 0);

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = snewn(mlen, BignumInt);
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    plen = p[0];
    /* Ensure plen > mlen */
    if (plen <= mlen)
	plen = mlen + 1;

    /* Allocate n of size plen, copy p to n */
    n = snewn(plen, BignumInt);
    for (j = 0; j < plen; j++)
	n[j] = 0;
    for (j = 1; j <= (int)p[0]; j++)
	n[plen - j] = p[j];

    /* Compute reciprocal of the top full word of the modulus */
    {
        BignumInt m0 = m[0];
        rshift = bn_clz(m0);
        if (rshift) {
            m0 <<= rshift;
            if (mlen > 1)
                m0 |= m[1] >> (BIGNUM_INT_BITS - rshift);
        }
        recip = reciprocal_word(m0);
    }

    /* Main computation */
    internal_mod(n, plen, m, mlen, quotient, recip, rshift);

    /* Copy result to buffer */
    if (result) {
	for (i = 1; i <= (int)result[0]; i++) {
	    int j = plen - i;
	    result[i] = j >= 0 ? n[j] : 0;
	}
    }

    /* Free temporary arrays */
    smemclr(m, mlen * sizeof(*m));
    sfree(m);
    smemclr(n, plen * sizeof(*n));
    sfree(n);
}

/*
 * Decrement a number.
 */
void decbn(Bignum bn)
{
    int i = 1;
    while (i < (int)bn[0] && bn[i] == 0)
	bn[i++] = BIGNUM_INT_MASK;
    bn[i]--;
}

Bignum bignum_from_bytes(const unsigned char *data, int nbytes)
{
    Bignum result;
    int w, i;

    assert(nbytes >= 0 && nbytes < INT_MAX/8);

    w = (nbytes + BIGNUM_INT_BYTES - 1) / BIGNUM_INT_BYTES; /* bytes->words */

    result = newbn(w);
    for (i = 1; i <= w; i++)
	result[i] = 0;
    for (i = nbytes; i--;) {
	unsigned char byte = *data++;
	result[1 + i / BIGNUM_INT_BYTES] |=
            (BignumInt)byte << (8*i % BIGNUM_INT_BITS);
    }

    bn_restore_invariant(result);
    return result;
}

Bignum bignum_from_bytes_le(const unsigned char *data, int nbytes)
{
    Bignum result;
    int w, i;

    assert(nbytes >= 0 && nbytes < INT_MAX/8);

    w = (nbytes + BIGNUM_INT_BYTES - 1) / BIGNUM_INT_BYTES; /* bytes->words */

    result = newbn(w);
    for (i = 1; i <= w; i++)
        result[i] = 0;
    for (i = 0; i < nbytes; ++i) {
        unsigned char byte = *data++;
        result[1 + i / BIGNUM_INT_BYTES] |=
            (BignumInt)byte << (8*i % BIGNUM_INT_BITS);
    }

    bn_restore_invariant(result);
    return result;
}

Bignum bignum_from_decimal(const char *decimal)
{
    Bignum result = copybn(Zero);

    while (*decimal) {
        Bignum tmp, tmp2;

        if (!isdigit((unsigned char)*decimal)) {
            freebn(result);
            return 0;
        }

        tmp = bigmul(result, Ten);
        tmp2 = bignum_from_long(*decimal - '0');
        freebn(result);
        result = bigadd(tmp, tmp2);
        freebn(tmp);
        freebn(tmp2);

        decimal++;
    }

    return result;
}

Bignum bignum_random_in_range(const Bignum lower, const Bignum upper)
{
    Bignum ret = NULL;
    unsigned char *bytes;
    int upper_len = bignum_bitcount(upper);
    int upper_bytes = upper_len / 8;
    int upper_bits = upper_len % 8;
    if (upper_bits) ++upper_bytes;

    bytes = snewn(upper_bytes, unsigned char);
    do {
        int i;

        if (ret) freebn(ret);

        for (i = 0; i < upper_bytes; ++i)
        {
            bytes[i] = (unsigned char)random_byte();
        }
        /* Mask the top to reduce failure rate to 50/50 */
        if (upper_bits)
        {
            bytes[i - 1] &= 0xFF >> (8 - upper_bits);
        }

        ret = bignum_from_bytes(bytes, upper_bytes);
    } while (bignum_cmp(ret, lower) < 0 || bignum_cmp(ret, upper) > 0);
    smemclr(bytes, upper_bytes);
    sfree(bytes);

    return ret;
}

/*
 * Read an SSH-1-format bignum from a data buffer. Return the number
 * of bytes consumed, or -1 if there wasn't enough data.
 */
int ssh1_read_bignum(const unsigned char *data, int len, Bignum * result)
{
    const unsigned char *p = data;
    int i;
    int w, b;

    if (len < 2)
	return -1;

    w = 0;
    for (i = 0; i < 2; i++)
	w = (w << 8) + *p++;
    b = (w + 7) / 8;		       /* bits -> bytes */

    if (len < b+2)
	return -1;

    if (!result)		       /* just return length */
	return b + 2;

    *result = bignum_from_bytes(p, b);

    return p + b - data;
}

/*
 * Return the bit count of a bignum, for SSH-1 encoding.
 */
int bignum_bitcount(Bignum bn)
{
    int bitcount = bn[0] * BIGNUM_INT_BITS - 1;
    while (bitcount >= 0
	   && (bn[bitcount / BIGNUM_INT_BITS + 1] >> (bitcount % BIGNUM_INT_BITS)) == 0) bitcount--;
    return bitcount + 1;
}

/*
 * Return the byte length of a bignum when SSH-1 encoded.
 */
int ssh1_bignum_length(Bignum bn)
{
    return 2 + (bignum_bitcount(bn) + 7) / 8;
}

/*
 * Return the byte length of a bignum when SSH-2 encoded.
 */
int ssh2_bignum_length(Bignum bn)
{
    return 4 + (bignum_bitcount(bn) + 8) / 8;
}

/*
 * Return a byte from a bignum; 0 is least significant, etc.
 */
int bignum_byte(Bignum bn, int i)
{
    if (i < 0 || i >= (int)(BIGNUM_INT_BYTES * bn[0]))
	return 0;		       /* beyond the end */
    else
	return (bn[i / BIGNUM_INT_BYTES + 1] >>
		((i % BIGNUM_INT_BYTES)*8)) & 0xFF;
}

/*
 * Return a bit from a bignum; 0 is least significant, etc.
 */
int bignum_bit(Bignum bn, int i)
{
    if (i < 0 || i >= (int)(BIGNUM_INT_BITS * bn[0]))
	return 0;		       /* beyond the end */
    else
	return (bn[i / BIGNUM_INT_BITS + 1] >> (i % BIGNUM_INT_BITS)) & 1;
}

/*
 * Set a bit in a bignum; 0 is least significant, etc.
 */
void bignum_set_bit(Bignum bn, int bitnum, int value)
{
    if (bitnum < 0 || bitnum >= (int)(BIGNUM_INT_BITS * bn[0])) {
        if (value) abort();		       /* beyond the end */
    } else {
	int v = bitnum / BIGNUM_INT_BITS + 1;
	BignumInt mask = (BignumInt)1 << (bitnum % BIGNUM_INT_BITS);
	if (value)
	    bn[v] |= mask;
	else
	    bn[v] &= ~mask;
    }
}

/*
 * Write a SSH-1-format bignum into a buffer. It is assumed the
 * buffer is big enough. Returns the number of bytes used.
 */
int ssh1_write_bignum(void *data, Bignum bn)
{
    unsigned char *p = data;
    int len = ssh1_bignum_length(bn);
    int i;
    int bitc = bignum_bitcount(bn);

    *p++ = (bitc >> 8) & 0xFF;
    *p++ = (bitc) & 0xFF;
    for (i = len - 2; i--;)
	*p++ = bignum_byte(bn, i);
    return len;
}

/*
 * Compare two bignums. Returns like strcmp.
 */
int bignum_cmp(Bignum a, Bignum b)
{
    int amax = a[0], bmax = b[0];
    int i;

    /* Annoyingly we have two representations of zero */
    if (amax == 1 && a[amax] == 0)
        amax = 0;
    if (bmax == 1 && b[bmax] == 0)
        bmax = 0;

    assert(amax == 0 || a[amax] != 0);
    assert(bmax == 0 || b[bmax] != 0);

    i = (amax > bmax ? amax : bmax);
    while (i) {
	BignumInt aval = (i > amax ? 0 : a[i]);
	BignumInt bval = (i > bmax ? 0 : b[i]);
	if (aval < bval)
	    return -1;
	if (aval > bval)
	    return +1;
	i--;
    }
    return 0;
}

/*
 * Right-shift one bignum to form another.
 */
Bignum bignum_rshift(Bignum a, int shift)
{
    Bignum ret;
    int i, shiftw, shiftb, shiftbb, bits;
    BignumInt ai, ai1;

    assert(shift >= 0);

    bits = bignum_bitcount(a) - shift;
    ret = newbn((bits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS);

    if (ret) {
	shiftw = shift / BIGNUM_INT_BITS;
	shiftb = shift % BIGNUM_INT_BITS;
	shiftbb = BIGNUM_INT_BITS - shiftb;

	ai1 = a[shiftw + 1];
	for (i = 1; i <= (int)ret[0]; i++) {
	    ai = ai1;
	    ai1 = (i + shiftw + 1 <= (int)a[0] ? a[i + shiftw + 1] : 0);
	    ret[i] = ((ai >> shiftb) | (ai1 << shiftbb)) & BIGNUM_INT_MASK;
	}
    }

    return ret;
}

/*
 * Left-shift one bignum to form another.
 */
Bignum bignum_lshift(Bignum a, int shift)
{
    Bignum ret;
    int bits, shiftWords, shiftBits;

    assert(shift >= 0);

    bits = bignum_bitcount(a) + shift;
    ret = newbn((bits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS);

    shiftWords = shift / BIGNUM_INT_BITS;
    shiftBits = shift % BIGNUM_INT_BITS;

    if (shiftBits == 0)
    {
        memcpy(&ret[1 + shiftWords], &a[1], sizeof(BignumInt) * a[0]);
    }
    else
    {
        int i;
        BignumInt carry = 0;

        /* Remember that Bignum[0] is length, so add 1 */
        for (i = shiftWords + 1; i < ((int)a[0]) + shiftWords + 1; ++i)
        {
            BignumInt from = a[i - shiftWords];
            ret[i] = (from << shiftBits) | carry;
            carry = from >> (BIGNUM_INT_BITS - shiftBits);
        }
        if (carry) ret[i] = carry;
    }

    return ret;
}

/*
 * Non-modular multiplication and addition.
 */
Bignum bigmuladd(Bignum a, Bignum b, Bignum addend)
{
    int alen = a[0], blen = b[0];
    int mlen = (alen > blen ? alen : blen);
    int rlen, i, maxspot;
    int wslen;
    BignumInt *workspace;
    Bignum ret;

    /* mlen space for a, mlen space for b, 2*mlen for result,
     * plus scratch space for multiplication */
    wslen = mlen * 4 + mul_compute_scratch(mlen);
    workspace = snewn(wslen, BignumInt);
    for (i = 0; i < mlen; i++) {
	workspace[0 * mlen + i] = (mlen - i <= (int)a[0] ? a[mlen - i] : 0);
	workspace[1 * mlen + i] = (mlen - i <= (int)b[0] ? b[mlen - i] : 0);
    }

    internal_mul(workspace + 0 * mlen, workspace + 1 * mlen,
		 workspace + 2 * mlen, mlen, workspace + 4 * mlen);

    /* now just copy the result back */
    rlen = alen + blen + 1;
    if (addend && rlen <= (int)addend[0])
	rlen = addend[0] + 1;
    ret = newbn(rlen);
    maxspot = 0;
    for (i = 1; i <= (int)ret[0]; i++) {
	ret[i] = (i <= 2 * mlen ? workspace[4 * mlen - i] : 0);
	if (ret[i] != 0)
	    maxspot = i;
    }
    ret[0] = maxspot;

    /* now add in the addend, if any */
    if (addend) {
	BignumCarry carry = 0;
	for (i = 1; i <= rlen; i++) {
            BignumInt retword = (i <= (int)ret[0] ? ret[i] : 0);
            BignumInt addword = (i <= (int)addend[0] ? addend[i] : 0);
            BignumADC(ret[i], carry, retword, addword, carry);
	    if (ret[i] != 0 && i > maxspot)
		maxspot = i;
	}
    }
    ret[0] = maxspot;

    smemclr(workspace, wslen * sizeof(*workspace));
    sfree(workspace);
    return ret;
}

/*
 * Non-modular multiplication.
 */
Bignum bigmul(Bignum a, Bignum b)
{
    return bigmuladd(a, b, NULL);
}

/*
 * Simple addition.
 */
Bignum bigadd(Bignum a, Bignum b)
{
    int alen = a[0], blen = b[0];
    int rlen = (alen > blen ? alen : blen) + 1;
    int i, maxspot;
    Bignum ret;
    BignumCarry carry;

    ret = newbn(rlen);

    carry = 0;
    maxspot = 0;
    for (i = 1; i <= rlen; i++) {
        BignumInt aword = (i <= (int)a[0] ? a[i] : 0);
        BignumInt bword = (i <= (int)b[0] ? b[i] : 0);
        BignumADC(ret[i], carry, aword, bword, carry);
        if (ret[i] != 0 && i > maxspot)
            maxspot = i;
    }
    ret[0] = maxspot;

    return ret;
}

/*
 * Subtraction. Returns a-b, or NULL if the result would come out
 * negative (recall that this entire bignum module only handles
 * positive numbers).
 */
Bignum bigsub(Bignum a, Bignum b)
{
    int alen = a[0], blen = b[0];
    int rlen = (alen > blen ? alen : blen);
    int i, maxspot;
    Bignum ret;
    BignumCarry carry;

    ret = newbn(rlen);

    carry = 1;
    maxspot = 0;
    for (i = 1; i <= rlen; i++) {
        BignumInt aword = (i <= (int)a[0] ? a[i] : 0);
        BignumInt bword = (i <= (int)b[0] ? b[i] : 0);
        BignumADC(ret[i], carry, aword, ~bword, carry);
        if (ret[i] != 0 && i > maxspot)
            maxspot = i;
    }
    ret[0] = maxspot;

    if (!carry) {
        freebn(ret);
        return NULL;
    }

    return ret;
}

/*
 * Create a bignum which is the bitmask covering another one. That
 * is, the smallest integer which is >= N and is also one less than
 * a power of two.
 */
Bignum bignum_bitmask(Bignum n)
{
    Bignum ret = copybn(n);
    int i;
    BignumInt j;

    i = ret[0];
    while (n[i] == 0 && i > 0)
	i--;
    if (i <= 0)
	return ret;		       /* input was zero */
    j = 1;
    while (j < n[i])
	j = 2 * j + 1;
    ret[i] = j;
    while (--i > 0)
	ret[i] = BIGNUM_INT_MASK;
    return ret;
}

/*
 * Convert an unsigned long into a bignum.
 */
Bignum bignum_from_long(unsigned long n)
{
    const int maxwords =
        (sizeof(unsigned long) + sizeof(BignumInt) - 1) / sizeof(BignumInt);
    Bignum ret;
    int i;

    ret = newbn(maxwords);
    ret[0] = 0;
    for (i = 0; i < maxwords; i++) {
        ret[i+1] = n >> (i * BIGNUM_INT_BITS);
        if (ret[i+1] != 0)
            ret[0] = i+1;
    }

    return ret;
}

/*
 * Add a long to a bignum.
 */
Bignum bignum_add_long(Bignum number, unsigned long n)
{
    const int maxwords =
        (sizeof(unsigned long) + sizeof(BignumInt) - 1) / sizeof(BignumInt);
    Bignum ret;
    int words, i;
    BignumCarry carry;

    words = number[0];
    if (words < maxwords)
        words = maxwords;
    words++;
    ret = newbn(words);

    carry = 0;
    ret[0] = 0;
    for (i = 0; i < words; i++) {
        BignumInt nword = (i < maxwords ? n >> (i * BIGNUM_INT_BITS) : 0);
        BignumInt numword = (i < number[0] ? number[i+1] : 0);
        BignumADC(ret[i+1], carry, numword, nword, carry);
	if (ret[i+1] != 0)
            ret[0] = i+1;
    }
    return ret;
}

/*
 * Compute the residue of a bignum, modulo a (max 16-bit) short.
 */
unsigned short bignum_mod_short(Bignum number, unsigned short modulus)
{
    unsigned long mod = modulus, r = 0;
    /* Precompute (BIGNUM_INT_MASK+1) % mod */
    unsigned long base_r = (BIGNUM_INT_MASK - modulus + 1) % mod;
    int i;

    for (i = number[0]; i > 0; i--) {
        /*
         * Conceptually, ((r << BIGNUM_INT_BITS) + number[i]) % mod
         */
        r = ((r * base_r) + (number[i] % mod)) % mod;
    }
    return (unsigned short) r;
}

#ifdef DEBUG
void diagbn(char *prefix, Bignum md)
{
    int i, nibbles, morenibbles;
    static const char hex[] = "0123456789ABCDEF";

    debug(("%s0x", prefix ? prefix : ""));

    nibbles = (3 + bignum_bitcount(md)) / 4;
    if (nibbles < 1)
	nibbles = 1;
    morenibbles = 4 * md[0] - nibbles;
    for (i = 0; i < morenibbles; i++)
	debug(("-"));
    for (i = nibbles; i--;)
	debug(("%c",
	       hex[(bignum_byte(md, i / 2) >> (4 * (i % 2))) & 0xF]));

    if (prefix)
	debug(("\n"));
}
#endif

/*
 * Simple division.
 */
Bignum bigdiv(Bignum a, Bignum b)
{
    Bignum q = newbn(a[0]);
    bigdivmod(a, b, NULL, q);
    while (q[0] > 1 && q[q[0]] == 0)
        q[0]--;
    return q;
}

/*
 * Simple remainder.
 */
Bignum bigmod(Bignum a, Bignum b)
{
    Bignum r = newbn(b[0]);
    bigdivmod(a, b, r, NULL);
    while (r[0] > 1 && r[r[0]] == 0)
        r[0]--;
    return r;
}

/*
 * Greatest common divisor.
 */
Bignum biggcd(Bignum av, Bignum bv)
{
    Bignum a = copybn(av);
    Bignum b = copybn(bv);

    while (bignum_cmp(b, Zero) != 0) {
	Bignum t = newbn(b[0]);
	bigdivmod(a, b, t, NULL);
	while (t[0] > 1 && t[t[0]] == 0)
	    t[0]--;
	freebn(a);
	a = b;
	b = t;
    }

    freebn(b);
    return a;
}

/*
 * Modular inverse, using Euclid's extended algorithm.
 */
Bignum modinv(Bignum number, Bignum modulus)
{
    Bignum a = copybn(modulus);
    Bignum b = copybn(number);
    Bignum xp = copybn(Zero);
    Bignum x = copybn(One);
    int sign = +1;

    assert(number[number[0]] != 0);
    assert(modulus[modulus[0]] != 0);

    while (bignum_cmp(b, One) != 0) {
	Bignum t, q;

        if (bignum_cmp(b, Zero) == 0) {
            /*
             * Found a common factor between the inputs, so we cannot
             * return a modular inverse at all.
             */
            freebn(b);
            freebn(a);
            freebn(xp);
            freebn(x);
            return NULL;
        }

        t = newbn(b[0]);
	q = newbn(a[0]);
	bigdivmod(a, b, t, q);
	while (t[0] > 1 && t[t[0]] == 0)
	    t[0]--;
	while (q[0] > 1 && q[q[0]] == 0)
	    q[0]--;
	freebn(a);
	a = b;
	b = t;
	t = xp;
	xp = x;
	x = bigmuladd(q, xp, t);
	sign = -sign;
	freebn(t);
	freebn(q);
    }

    freebn(b);
    freebn(a);
    freebn(xp);

    /* now we know that sign * x == 1, and that x < modulus */
    if (sign < 0) {
	/* set a new x to be modulus - x */
	Bignum newx = newbn(modulus[0]);
	BignumInt carry = 0;
	int maxspot = 1;
	int i;

	for (i = 1; i <= (int)newx[0]; i++) {
	    BignumInt aword = (i <= (int)modulus[0] ? modulus[i] : 0);
	    BignumInt bword = (i <= (int)x[0] ? x[i] : 0);
	    newx[i] = aword - bword - carry;
	    bword = ~bword;
	    carry = carry ? (newx[i] >= bword) : (newx[i] > bword);
	    if (newx[i] != 0)
		maxspot = i;
	}
	newx[0] = maxspot;
	freebn(x);
	x = newx;
    }

    /* and return. */
    return x;
}

/*
 * Render a bignum into decimal. Return a malloced string holding
 * the decimal representation.
 */
char *bignum_decimal(Bignum x)
{
    int ndigits, ndigit;
    int i, iszero;
    BignumInt carry;
    char *ret;
    BignumInt *workspace;

    /*
     * First, estimate the number of digits. Since log(10)/log(2)
     * is just greater than 93/28 (the joys of continued fraction
     * approximations...) we know that for every 93 bits, we need
     * at most 28 digits. This will tell us how much to malloc.
     *
     * Formally: if x has i bits, that means x is strictly less
     * than 2^i. Since 2 is less than 10^(28/93), this is less than
     * 10^(28i/93). We need an integer power of ten, so we must
     * round up (rounding down might make it less than x again).
     * Therefore if we multiply the bit count by 28/93, rounding
     * up, we will have enough digits.
     *
     * i=0 (i.e., x=0) is an irritating special case.
     */
    i = bignum_bitcount(x);
    if (!i)
	ndigits = 1;		       /* x = 0 */
    else
	ndigits = (28 * i + 92) / 93;  /* multiply by 28/93 and round up */
    ndigits++;			       /* allow for trailing \0 */
    ret = snewn(ndigits, char);

    /*
     * Now allocate some workspace to hold the binary form as we
     * repeatedly divide it by ten. Initialise this to the
     * big-endian form of the number.
     */
    workspace = snewn(x[0], BignumInt);
    for (i = 0; i < (int)x[0]; i++)
	workspace[i] = x[x[0] - i];

    /*
     * Next, write the decimal number starting with the last digit.
     * We use ordinary short division, dividing 10 into the
     * workspace.
     */
    ndigit = ndigits - 1;
    ret[ndigit] = '\0';
    do {
	iszero = 1;
	carry = 0;
	for (i = 0; i < (int)x[0]; i++) {
            /*
             * Conceptually, we want to compute
             *
             *   (carry << BIGNUM_INT_BITS) + workspace[i]
             *   -----------------------------------------
             *                      10
             *
             * but we don't have an integer type longer than BignumInt
             * to work with. So we have to do it in pieces.
             */

            BignumInt q, r;
            q = workspace[i] / 10;
            r = workspace[i] % 10;

            /* I want (BIGNUM_INT_MASK+1)/10 but can't say so directly! */
            q += carry * ((BIGNUM_INT_MASK-9) / 10 + 1);
            r += carry * ((BIGNUM_INT_MASK-9) % 10);

            q += r / 10;
            r %= 10;

	    workspace[i] = q;
	    carry = r;

	    if (workspace[i])
		iszero = 0;
	}
	ret[--ndigit] = (char) (carry + '0');
    } while (!iszero);

    /*
     * There's a chance we've fallen short of the start of the
     * string. Correct if so.
     */
    if (ndigit > 0)
	memmove(ret, ret + ndigit, ndigits - ndigit);

    /*
     * Done.
     */
    smemclr(workspace, x[0] * sizeof(*workspace));
    sfree(workspace);
    return ret;
}
