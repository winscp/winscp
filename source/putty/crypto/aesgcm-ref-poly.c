/*
 * Implementation of the GCM polynomial hash in pure software, but
 * based on a primitive that performs 64x64->128 bit polynomial
 * multiplication over GF(2).
 *
 * This implementation is technically correct (should even be
 * side-channel safe as far as I can see), but it's hopelessly slow,
 * so no live SSH connection should ever use it. Therefore, it's
 * deliberately not included in the lists in aesgcm-select.c. For pure
 * software GCM in live use, you want aesgcm-sw.c, and that's what the
 * selection system will choose.
 *
 * However, this implementation _is_ made available to testcrypt, so
 * all the GCM tests in cryptsuite.py are run over this as well as the
 * other implementations.
 *
 * The reason why this code exists at all is to act as a reference for
 * GCM implementations that use a CPU-specific polynomial multiply
 * intrinsic or asm statement. This version will run on whatever
 * platform you're trying to port to, and will generate all the same
 * intermediate results you expect the CPU-specific one to go through.
 * So you can insert parallel diagnostics in this version and in your
 * new version, to see where the two diverge.
 *
 * Also, this version is a good place to put long comments explaining
 * the entire strategy of this implementation and its rationale. That
 * avoids those comments having to be duplicated in the multiple
 * platform-specific implementations, which can focus on commenting
 * the way the local platform's idioms match up to this version, and
 * refer to this file for the explanation of the underlying technique.
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

typedef struct aesgcm_ref_poly {
    AESGCM_COMMON_FIELDS;

    /*
     * The state of our GCM implementation is represented entirely by
     * three 128-bit values:
     */

    /*
     * The value at which we're evaluating the polynomial. The GCM
     * spec calls this 'H'. It's defined once at GCM key setup time,
     * by encrypting the all-zeroes value with our block cipher.
     */
    value128_t var;

    /*
     * Accumulator containing the result of evaluating the polynomial
     * so far.
     */
    value128_t acc;

    /*
     * The mask value that is XORed into the final value of 'acc' to
     * produce the output MAC. This is different for every MAC
     * generated, because its purpose is to ensure that no information
     * gathered from a legal MAC can be used to help the forgery of
     * another one, and that comparing two legal MACs gives you no
     * useful information about the text they cover, because in each
     * case, the masks are different and pseudorandom.
     */
    value128_t mask;
} aesgcm_ref_poly;

static bool aesgcm_ref_poly_available(void)
{
    return true;  /* pure software implementation, always available */
}

/*
 * Primitive function that takes two uint64_t values representing
 * polynomials, multiplies them, and returns a value128_t struct
 * containing the full product.
 *
 * Because the input polynomials have maximum degree 63, the output
 * has max degree 63+63 = 127, not 128. As a result, the topmost bit
 * of the output is always zero.
 *
 * The inside of this function is implemented in the simplest way,
 * with no attention paid to performance. The important feature of
 * this implementation is not what's _inside_ this function, but
 * what's _outside_ it: aesgcm_ref_poly_coeff() tries to minimise the
 * number of these operations.
 */
static value128_t pmul(uint64_t x, uint64_t y)
{
    value128_t r;
    r.hi = r.lo = 0;

    { // WINSCP
    uint64_t bit = 1 & y;
    r.lo ^= x & -bit;

    { // WINSCP
    unsigned i;
    for (i = 1; i < 64; i++) {
        bit = 1 & (y >> i);
        { // WINSCP
        uint64_t z = x & -bit;
        r.lo ^= z << i;
        r.hi ^= z >> (64-i);
        } // WINSCP
    }
    } // WINSCP

    return r;
    } // WINSCP
}

/*
 * OK, I promised a long comment explaining what's going on in this
 * implementation, and now it's time.
 *
 * The way AES-GCM _itself_ is defined by its own spec, its finite
 * field consists of polynomials over GF(2), constrained to be 128
 * bits long by reducing them modulo P = x^128 + x^7 + x^2 + x + 1.
 * Using the usual binary representation in which bit i is the
 * coefficient of x^i, that's 0x100000000000000000000000000000087.
 *
 * That is, whenever you multiply two polynomials and find a term
 * x^128, you can replace it with x^7+x^2+x+1. More generally,
 * x^(128+n) can be replaced with x^(7+n)+x^(2+n)+x^(1+n)+x^n. In
 * binary terms, a 1 bit at the 128th position or above is replaced by
 * 0x87 exactly 128 bits further down.
 *
 * So you'd think that multiplying two 128-bit polynomials mod P would
 * be a matter of generating their full 256-bit product in the form of
 * four words HI:HU:LU:LO, and then reducing it mod P by a two-stage
 * process of computing HI * 0x87 and XORing it into HU:LU, then
 * computing HU * 0x87 and XORing it into LU:LO.
 *
 * But it's not!
 *
 * The reason why not is because when AES-GCM is applied to SSH,
 * somehow the _bit_ order got reversed. A 16-byte block of data in
 * memory is converted into a polynomial by regarding bit 7 of the
 * first byte as the constant term, bit 0 of the first byte as the x^7
 * coefficient, ..., bit 0 of the last byte as the x^127 coefficient.
 * So if we load that 16-byte block as a big-endian 128-bit integer,
 * we end up with it representing a polynomial back to front, with the
 * constant term at the top and the x^127 bit at the bottom.
 *
 * Well, that _shouldn't_ be a problem, right? The nice thing about
 * polynomial multiplication is that it's essentially reversible. If
 * you reverse the order of the coefficients of two polynomials, then
 * the product of the reversed polys is exactly the reversal of the
 * product of the original ones. So we bit-reverse our modulo
 * polynomial to get 0x1c2000000000000000000000000000001, and we just
 * pretend we're working mod that instead.
 *
 * And that is basically what we're about to do. But there's one
 * complication, that arises from the semantics of the polynomial
 * multiplication function we're using as our primitive operation.
 *
 * That function multiplies two polynomials of degree at most 63, to
 * give one with degree at most 127. So it returns a 128-bit integer
 * whose low bit is the constant term, and its very highest bit is 0,
 * and its _next_ highest bit is the product of the high bits of the
 * two inputs.
 *
 * That operation is _not_ symmetric in bit-reversal. If you give it
 * the 64-bit-wise reversals of two polynomials P,Q, then its output
 * is not the 128-bit-wise reversal of their product PQ, because that
 * would require the constant term of PQ to appear in bit 127 of the
 * output, and in fact it appears in bit 126. So in fact, what we get
 * is offset by one bit from where we'd like it: it's the bit-reversal
 * of PQx, not of PQ.
 *
 * There's more than one way we could fix this. One approach would be
 * to work around this off-by-one error by taking the 128-bit output
 * of pmul() and shifting it left by a bit. Then it _is_ the bitwise
 * reversal of the 128-bit value we'd have wanted to get, and we could
 * implement the exact algorithm described above, in fully
 * bit-reversed form.
 *
 * But a 128-bit left shift is not a trivial operation in the vector
 * architectures that this code is acting as a reference for. So we'd
 * prefer to find a fix that doesn't need compensation during the
 * actual per-block multiplication step.
 *
 * If we did the obvious thing anyway - compute the unshifted 128-bit
 * product representing the bit-reversal of PQx, and reduce it mod
 * 0x1c2000000000000000000000000000001 - then we'd get a result which
 * is exactly what we want, except that it's got a factor of x in it
 * that we need to get rid of. The obvious answer is to divide by x
 * (which is legal and safe, since mod P, x is invertible).
 *
 * Dividing a 128-bit polynomial by x is easy in principle. Shift left
 * (because we're still bit-reversed), and if that shifted a 1 bit off
 * the top, XOR 0xc2000000000000000000000000000001 into the remaining
 * 128 bits.
 *
 * But we're back to having that expensive left shift. What can we do
 * about that?
 *
 * Happily, one of the two input values to our per-block multiply
 * operation is fixed! It's given to us at key setup stage, and never
 * changed until the next rekey. So if at key setup time we do this
 * left shift business _once_, replacing the logical value Q with Q/x,
 * then that exactly cancels out the unwanted factor of x that shows
 * up in our multiply operation. And now it doesn't matter that it's
 * expensive (in the sense of 'a few more instructions than you'd
 * like'), because it only happens once per SSH key exchange, not once
 * per 16 bytes of data transferred.
 */

static void aesgcm_ref_poly_setkey_impl(aesgcm_ref_poly *ctx,
                                        const unsigned char *var)
{
    /*
     * Key setup function. We copy the provided 16-byte 'var'
     * value into our polynomial. But, as discussed above, we also
     * need to divide it by x.
     */

    ctx->var.hi = GET_64BIT_MSB_FIRST(var);
    ctx->var.lo = GET_64BIT_MSB_FIRST(var + 8);

    { // WINSCP
    uint64_t bit = 1 & (ctx->var.hi >> 63);
    ctx->var.hi = (ctx->var.hi << 1) ^ (ctx->var.lo >> 63);
    ctx->var.lo = (ctx->var.lo << 1) ^ bit;
    ctx->var.hi ^= 0xC200000000000000LL & -bit; // WINSCP
    } // WINSCP
}

static inline void aesgcm_ref_poly_setup(aesgcm_ref_poly *ctx,
                                         const unsigned char *mask)
{
    /*
     * Set up to start evaluating a particular MAC. Copy in the mask
     * value for this packet, and initialise acc to zero.
     */

    ctx->mask.hi = GET_64BIT_MSB_FIRST(mask);
    ctx->mask.lo = GET_64BIT_MSB_FIRST(mask + 8);
    ctx->acc.hi = ctx->acc.lo = 0;
}

static inline void aesgcm_ref_poly_coeff(aesgcm_ref_poly *ctx,
                                         const unsigned char *coeff)
{
    /*
     * One step of Horner's-rule polynomial evaluation (with each
     * coefficient of the polynomial being an element of GF(2^128),
     * itself composed of polynomials over GF(2) mod P).
     *
     * We take our accumulator value, add the incoming coefficient
     * (which means XOR, by GF(2) rules), and multiply by x (that is,
     * 'var').
     */

    /*
     * The addition first, which is easy.
     */
    ctx->acc.hi ^= GET_64BIT_MSB_FIRST(coeff);
    ctx->acc.lo ^= GET_64BIT_MSB_FIRST(coeff + 8);

    /*
     * First, create the 256-bit product of the two 128-bit
     * polynomials over GF(2) stored in ctx->acc and ctx->var.
     *
     * The obvious way to do this is by four smaller multiplications
     * of 64x64 -> 128 bits. But we can do better using a single
     * iteration of the Karatsuba technique, which is actually more
     * convenient in polynomials over GF(2) than it is in integers,
     * because there aren't all those awkward carries complicating
     * things.
     *
     * Letting B denote x^64, and imagining our two inputs are split
     * up into 64-bit chunks ah,al,bh,bl, the product we want is
     *
     *         (ah B + al) (bh B + bl)
     *       = (ah bh) B^2 + (al bh + ah bl) B + (al bl)
     *
     * which looks like four smaller multiplications of each of ah,al
     * with each of bh,bl. But Karatsuba's trick is to first compute
     *
     *         (ah + al) (bh + bl)
     *       = ah bh + al bh + ah bl + al bl
     *
     * and then subtract the terms (ah bh) and (al bl), which we had
     * to compute anyway, to get the middle two terms (al bh + ah bl)
     * which are our coefficient of B.
     *
     * This involves more bookkeeping instructions like XORs, but with
     * any luck those are faster than the main multiplication.
     */
    { // WINSCP
    uint64_t ah = ctx->acc.hi, al = ctx->acc.lo;
    uint64_t bh = ctx->var.hi, bl = ctx->var.lo;
    /* Compute the outer two terms */
    value128_t lo = pmul(al, bl);
    value128_t hi = pmul(ah, bh);
    /* Compute the trick product (ah+al)(bh+bl) */
    value128_t md = pmul(ah ^ al, bh ^ bl);
    /* Subtract off the outer two terms to get md = al bh + ah bl */
    md.hi ^= lo.hi ^ hi.hi;
    md.lo ^= lo.lo ^ hi.lo;
    /* And add that into the 256-bit value given by hi * x^128 + lo */
    lo.hi ^= md.lo;
    hi.lo ^= md.hi;

    /*
     * OK. Now hi and lo together make up the 256-bit full product.
     * Now reduce it mod the reversal of the GCM modulus polynomial.
     * As discussed above, that's 0x1c2000000000000000000000000000001.
     *
     * We want the _topmost_ 128 bits of this, because we're working
     * in a bit-reversed world. So what we fundamentally want to do is
     * to take our 256-bit product, and add to it the product of its
     * low 128 bits with 0x1c2000000000000000000000000000001. Then the
     * top 128 bits will be the output we want.
     *
     * Since there's no carrying in this arithmetic, it's enough to
     * discard the 1 bit at the bottom of that, because it won't
     * affect anything in the half we're keeping. So it's enough to
     * add 0x1c2000000000000000000000000000000 * lo to (hi:lo).
     *
     * We can only work with 64 bits at a time, so the first thing we
     * do is to break that up:
     *
     *  - add 0x1c200000000000000 * lo.lo to (hi.lo : lo.hi)
     *  - add 0x1c200000000000000 * lo.hi to (hi.hi : hi.lo)
     *
     * But there's still a problem: 0x1c200000000000000 is just too
     * _big_ to fit in 64 bits. So we have to break it up into the low
     * 64 bits 0xc200000000000000, and its leading 1. So each of those
     * steps of the form 'add 0x1c200000000000000 * x to y:z' becomes
     * 'add 0xc200000000000000 * x to y:z' followed by 'add x to y',
     * the latter step dealing with the leading 1.
     */

    /* First step, adding to the middle two words of our number. After
     * this the lowest word (in lo.lo) is ignored. */
    { // WINSCP
    value128_t r1 = pmul(0xC200000000000000LL, lo.lo); // WINSCP
    hi.lo ^= r1.hi ^ lo.lo;
    lo.hi ^= r1.lo;

    /* Second of those steps, adding to the top two words, and
     * discarding lo.hi. */
    { // WINSCP
    value128_t r2 = pmul(0xC200000000000000LL, lo.hi);
    hi.hi ^= r2.hi ^ lo.hi;
    hi.lo ^= r2.lo;

    /* Now 'hi' is precisely what we have left. */
    ctx->acc = hi;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static inline void aesgcm_ref_poly_output(aesgcm_ref_poly *ctx,
                                          unsigned char *output)
{
    PUT_64BIT_MSB_FIRST(output, ctx->acc.hi ^ ctx->mask.hi);
    PUT_64BIT_MSB_FIRST(output + 8, ctx->acc.lo ^ ctx->mask.lo);
    smemclr(&ctx->acc, 16);
    smemclr(&ctx->mask, 16);
}

#define AESGCM_FLAVOUR ref_poly
#define AESGCM_NAME "reference polynomial-based implementation"
#include "aesgcm-footer.h"
