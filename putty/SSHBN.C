/*
 * Bignum routines for RSA and DH and stuff.
 */

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "misc.h"

#if defined __GNUC__ && defined __i386__
typedef unsigned long BignumInt;
typedef unsigned long long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFFFFFUL
#define BIGNUM_TOP_BIT   0x80000000UL
#define BIGNUM_INT_BITS  32
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) \
    __asm__("div %2" : \
	    "=d" (r), "=a" (q) : \
	    "r" (w), "d" (hi), "a" (lo))
#else
typedef unsigned short BignumInt;
typedef unsigned long BignumDblInt;
#define BIGNUM_INT_MASK  0xFFFFU
#define BIGNUM_TOP_BIT   0x8000U
#define BIGNUM_INT_BITS  16
#define MUL_WORD(w1, w2) ((BignumDblInt)w1 * w2)
#define DIVMOD_WORD(q, r, hi, lo, w) do { \
    BignumDblInt n = (((BignumDblInt)hi) << BIGNUM_INT_BITS) | lo; \
    q = n / w; \
    r = n % w; \
} while (0)
#endif

#define BIGNUM_INT_BYTES (BIGNUM_INT_BITS / 8)

#define BIGNUM_INTERNAL
typedef BignumInt *Bignum;

#include "ssh.h"

BignumInt bnZero[1] = { 0 };
BignumInt bnOne[2] = { 1, 1 };

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

Bignum Zero = bnZero, One = bnOne;

static Bignum newbn(int length)
{
    Bignum b = snewn(length + 1, BignumInt);
    if (!b)
	abort();		       /* FIXME */
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
    memset(b, 0, sizeof(b[0]) * (b[0] + 1));
    sfree(b);
}

Bignum bn_power_2(int n)
{
    Bignum ret = newbn(n / BIGNUM_INT_BITS + 1);
    bignum_set_bit(ret, n, 1);
    return ret;
}

/*
 * Compute c = a * b.
 * Input is in the first len words of a and b.
 * Result is returned in the first 2*len words of c.
 */
static void internal_mul(BignumInt *a, BignumInt *b,
			 BignumInt *c, int len)
{
    int i, j;
    BignumDblInt t;

    for (j = 0; j < 2 * len; j++)
	c[j] = 0;

    for (i = len - 1; i >= 0; i--) {
	t = 0;
	for (j = len - 1; j >= 0; j--) {
	    t += MUL_WORD(a[i], (BignumDblInt) b[j]);
	    t += (BignumDblInt) c[i + j + 1];
	    c[i + j + 1] = (BignumInt) t;
	    t = t >> BIGNUM_INT_BITS;
	}
	c[i] = (BignumInt) t;
    }
}

static void internal_add_shifted(BignumInt *number,
				 unsigned n, int shift)
{
    int word = 1 + (shift / BIGNUM_INT_BITS);
    int bshift = shift % BIGNUM_INT_BITS;
    BignumDblInt addend;

    addend = (BignumDblInt)n << bshift;

    while (addend) {
	addend += number[word];
	number[word] = (BignumInt) addend & BIGNUM_INT_MASK;
	addend >>= BIGNUM_INT_BITS;
	word++;
    }
}

/*
 * Compute a = a % m.
 * Input in first alen words of a and first mlen words of m.
 * Output in first alen words of a
 * (of which first alen-mlen words will be zero).
 * The MSW of m MUST have its high bit set.
 * Quotient is accumulated in the `quotient' array, which is a Bignum
 * rather than the internal bigendian format. Quotient parts are shifted
 * left by `qshift' before adding into quot.
 */
static void internal_mod(BignumInt *a, int alen,
			 BignumInt *m, int mlen,
			 BignumInt *quot, int qshift)
{
    BignumInt m0, m1;
    unsigned int h;
    int i, k;

    m0 = m[0];
    if (mlen > 1)
	m1 = m[1];
    else
	m1 = 0;

    for (i = 0; i <= alen - mlen; i++) {
	BignumDblInt t;
	unsigned int q, r, c, ai1;

	if (i == 0) {
	    h = 0;
	} else {
	    h = a[i - 1];
	    a[i - 1] = 0;
	}

	if (i == alen - 1)
	    ai1 = 0;
	else
	    ai1 = a[i + 1];

	/* Find q = h:a[i] / m0 */
	DIVMOD_WORD(q, r, h, a[i], m0);

	/* Refine our estimate of q by looking at
	   h:a[i]:a[i+1] / m0:m1 */
	t = MUL_WORD(m1, q);
	if (t > ((BignumDblInt) r << BIGNUM_INT_BITS) + ai1) {
	    q--;
	    t -= m1;
	    r = (r + m0) & BIGNUM_INT_MASK;     /* overflow? */
	    if (r >= (BignumDblInt) m0 &&
		t > ((BignumDblInt) r << BIGNUM_INT_BITS) + ai1) q--;
	}

	/* Subtract q * m from a[i...] */
	c = 0;
	for (k = mlen - 1; k >= 0; k--) {
	    t = MUL_WORD(q, m[k]);
	    t += c;
	    c = t >> BIGNUM_INT_BITS;
	    if ((BignumInt) t > a[i + k])
		c++;
	    a[i + k] -= (BignumInt) t;
	}

	/* Add back m in case of borrow */
	if (c != h) {
	    t = 0;
	    for (k = mlen - 1; k >= 0; k--) {
		t += m[k];
		t += a[i + k];
		a[i + k] = (BignumInt) t;
		t = t >> BIGNUM_INT_BITS;
	    }
	    q--;
	}
	if (quot)
	    internal_add_shifted(quot, q, qshift + BIGNUM_INT_BITS * (alen - mlen - i));
    }
}

/*
 * Compute (base ^ exp) % mod.
 */
Bignum modpow(Bignum base_in, Bignum exp, Bignum mod)
{
    BignumInt *a, *b, *n, *m;
    int mshift;
    int mlen, i, j;
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

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < BIGNUM_INT_BITS-1; mshift++)
	if ((m[0] << mshift) & BIGNUM_TOP_BIT)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (BIGNUM_INT_BITS - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    /* Allocate n of size mlen, copy base to n */
    n = snewn(mlen, BignumInt);
    i = mlen - base[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < base[0]; j++)
	n[i + j] = base[base[0] - j];

    /* Allocate a and b of size 2*mlen. Set a = 1 */
    a = snewn(2 * mlen, BignumInt);
    b = snewn(2 * mlen, BignumInt);
    for (i = 0; i < 2 * mlen; i++)
	a[i] = 0;
    a[2 * mlen - 1] = 1;

    /* Skip leading zero bits of exp. */
    i = 0;
    j = BIGNUM_INT_BITS-1;
    while (i < exp[0] && (exp[exp[0] - i] & (1 << j)) == 0) {
	j--;
	if (j < 0) {
	    i++;
	    j = BIGNUM_INT_BITS-1;
	}
    }

    /* Main computation */
    while (i < exp[0]) {
	while (j >= 0) {
	    internal_mul(a + mlen, a + mlen, b, mlen);
	    internal_mod(b, mlen * 2, m, mlen, NULL, 0);
	    if ((exp[exp[0] - i] & (1 << j)) != 0) {
		internal_mul(b + mlen, n, a, mlen);
		internal_mod(a, mlen * 2, m, mlen, NULL, 0);
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

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = mlen - 1; i < 2 * mlen - 1; i++)
	    a[i] = (a[i] << mshift) | (a[i + 1] >> (BIGNUM_INT_BITS - mshift));
	a[2 * mlen - 1] = a[2 * mlen - 1] << mshift;
	internal_mod(a, mlen * 2, m, mlen, NULL, 0);
	for (i = 2 * mlen - 1; i >= mlen; i--)
	    a[i] = (a[i] >> mshift) | (a[i - 1] << (BIGNUM_INT_BITS - mshift));
    }

    /* Copy result to buffer */
    result = newbn(mod[0]);
    for (i = 0; i < mlen; i++)
	result[result[0] - i] = a[i + mlen];
    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;

    /* Free temporary arrays */
    for (i = 0; i < 2 * mlen; i++)
	a[i] = 0;
    sfree(a);
    for (i = 0; i < 2 * mlen; i++)
	b[i] = 0;
    sfree(b);
    for (i = 0; i < mlen; i++)
	m[i] = 0;
    sfree(m);
    for (i = 0; i < mlen; i++)
	n[i] = 0;
    sfree(n);

    freebn(base);

    return result;
}

/*
 * Compute (p * q) % mod.
 * The most significant word of mod MUST be non-zero.
 * We assume that the result array is the same size as the mod array.
 */
Bignum modmul(Bignum p, Bignum q, Bignum mod)
{
    BignumInt *a, *n, *m, *o;
    int mshift;
    int pqlen, mlen, rlen, i, j;
    Bignum result;

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = snewn(mlen, BignumInt);
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < BIGNUM_INT_BITS-1; mshift++)
	if ((m[0] << mshift) & BIGNUM_TOP_BIT)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (BIGNUM_INT_BITS - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    pqlen = (p[0] > q[0] ? p[0] : q[0]);

    /* Allocate n of size pqlen, copy p to n */
    n = snewn(pqlen, BignumInt);
    i = pqlen - p[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < p[0]; j++)
	n[i + j] = p[p[0] - j];

    /* Allocate o of size pqlen, copy q to o */
    o = snewn(pqlen, BignumInt);
    i = pqlen - q[0];
    for (j = 0; j < i; j++)
	o[j] = 0;
    for (j = 0; j < q[0]; j++)
	o[i + j] = q[q[0] - j];

    /* Allocate a of size 2*pqlen for result */
    a = snewn(2 * pqlen, BignumInt);

    /* Main computation */
    internal_mul(n, o, a, pqlen);
    internal_mod(a, pqlen * 2, m, mlen, NULL, 0);

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = 2 * pqlen - mlen - 1; i < 2 * pqlen - 1; i++)
	    a[i] = (a[i] << mshift) | (a[i + 1] >> (BIGNUM_INT_BITS - mshift));
	a[2 * pqlen - 1] = a[2 * pqlen - 1] << mshift;
	internal_mod(a, pqlen * 2, m, mlen, NULL, 0);
	for (i = 2 * pqlen - 1; i >= 2 * pqlen - mlen; i--)
	    a[i] = (a[i] >> mshift) | (a[i - 1] << (BIGNUM_INT_BITS - mshift));
    }

    /* Copy result to buffer */
    rlen = (mlen < pqlen * 2 ? mlen : pqlen * 2);
    result = newbn(rlen);
    for (i = 0; i < rlen; i++)
	result[result[0] - i] = a[i + 2 * pqlen - rlen];
    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;

    /* Free temporary arrays */
    for (i = 0; i < 2 * pqlen; i++)
	a[i] = 0;
    sfree(a);
    for (i = 0; i < mlen; i++)
	m[i] = 0;
    sfree(m);
    for (i = 0; i < pqlen; i++)
	n[i] = 0;
    sfree(n);
    for (i = 0; i < pqlen; i++)
	o[i] = 0;
    sfree(o);

    return result;
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
    int mshift;
    int plen, mlen, i, j;

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = snewn(mlen, BignumInt);
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < BIGNUM_INT_BITS-1; mshift++)
	if ((m[0] << mshift) & BIGNUM_TOP_BIT)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (BIGNUM_INT_BITS - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    plen = p[0];
    /* Ensure plen > mlen */
    if (plen <= mlen)
	plen = mlen + 1;

    /* Allocate n of size plen, copy p to n */
    n = snewn(plen, BignumInt);
    for (j = 0; j < plen; j++)
	n[j] = 0;
    for (j = 1; j <= p[0]; j++)
	n[plen - j] = p[j];

    /* Main computation */
    internal_mod(n, plen, m, mlen, quotient, mshift);

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = plen - mlen - 1; i < plen - 1; i++)
	    n[i] = (n[i] << mshift) | (n[i + 1] >> (BIGNUM_INT_BITS - mshift));
	n[plen - 1] = n[plen - 1] << mshift;
	internal_mod(n, plen, m, mlen, quotient, 0);
	for (i = plen - 1; i >= plen - mlen; i--)
	    n[i] = (n[i] >> mshift) | (n[i - 1] << (BIGNUM_INT_BITS - mshift));
    }

    /* Copy result to buffer */
    if (result) {
	for (i = 1; i <= result[0]; i++) {
	    int j = plen - i;
	    result[i] = j >= 0 ? n[j] : 0;
	}
    }

    /* Free temporary arrays */
    for (i = 0; i < mlen; i++)
	m[i] = 0;
    sfree(m);
    for (i = 0; i < plen; i++)
	n[i] = 0;
    sfree(n);
}

/*
 * Decrement a number.
 */
void decbn(Bignum bn)
{
    int i = 1;
    while (i < bn[0] && bn[i] == 0)
	bn[i++] = BIGNUM_INT_MASK;
    bn[i]--;
}

Bignum bignum_from_bytes(const unsigned char *data, int nbytes)
{
    Bignum result;
    int w, i;

    w = (nbytes + BIGNUM_INT_BYTES - 1) / BIGNUM_INT_BYTES; /* bytes->words */

    result = newbn(w);
    for (i = 1; i <= w; i++)
	result[i] = 0;
    for (i = nbytes; i--;) {
	unsigned char byte = *data++;
	result[1 + i / BIGNUM_INT_BYTES] |= byte << (8*i % BIGNUM_INT_BITS);
    }

    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;
    return result;
}

/*
 * Read an ssh1-format bignum from a data buffer. Return the number
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
 * Return the bit count of a bignum, for ssh1 encoding.
 */
int bignum_bitcount(Bignum bn)
{
    int bitcount = bn[0] * BIGNUM_INT_BITS - 1;
    while (bitcount >= 0
	   && (bn[bitcount / BIGNUM_INT_BITS + 1] >> (bitcount % BIGNUM_INT_BITS)) == 0) bitcount--;
    return bitcount + 1;
}

/*
 * Return the byte length of a bignum when ssh1 encoded.
 */
int ssh1_bignum_length(Bignum bn)
{
    return 2 + (bignum_bitcount(bn) + 7) / 8;
}

/*
 * Return the byte length of a bignum when ssh2 encoded.
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
    if (i >= BIGNUM_INT_BYTES * bn[0])
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
    if (i >= BIGNUM_INT_BITS * bn[0])
	return 0;		       /* beyond the end */
    else
	return (bn[i / BIGNUM_INT_BITS + 1] >> (i % BIGNUM_INT_BITS)) & 1;
}

/*
 * Set a bit in a bignum; 0 is least significant, etc.
 */
void bignum_set_bit(Bignum bn, int bitnum, int value)
{
    if (bitnum >= BIGNUM_INT_BITS * bn[0])
	abort();		       /* beyond the end */
    else {
	int v = bitnum / BIGNUM_INT_BITS + 1;
	int mask = 1 << (bitnum % BIGNUM_INT_BITS);
	if (value)
	    bn[v] |= mask;
	else
	    bn[v] &= ~mask;
    }
}

/*
 * Write a ssh1-format bignum into a buffer. It is assumed the
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
    int i = (amax > bmax ? amax : bmax);
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

    bits = bignum_bitcount(a) - shift;
    ret = newbn((bits + BIGNUM_INT_BITS - 1) / BIGNUM_INT_BITS);

    if (ret) {
	shiftw = shift / BIGNUM_INT_BITS;
	shiftb = shift % BIGNUM_INT_BITS;
	shiftbb = BIGNUM_INT_BITS - shiftb;

	ai1 = a[shiftw + 1];
	for (i = 1; i <= ret[0]; i++) {
	    ai = ai1;
	    ai1 = (i + shiftw + 1 <= a[0] ? a[i + shiftw + 1] : 0);
	    ret[i] = ((ai >> shiftb) | (ai1 << shiftbb)) & BIGNUM_INT_MASK;
	}
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
    BignumInt *workspace;
    Bignum ret;

    /* mlen space for a, mlen space for b, 2*mlen for result */
    workspace = snewn(mlen * 4, BignumInt);
    for (i = 0; i < mlen; i++) {
	workspace[0 * mlen + i] = (mlen - i <= a[0] ? a[mlen - i] : 0);
	workspace[1 * mlen + i] = (mlen - i <= b[0] ? b[mlen - i] : 0);
    }

    internal_mul(workspace + 0 * mlen, workspace + 1 * mlen,
		 workspace + 2 * mlen, mlen);

    /* now just copy the result back */
    rlen = alen + blen + 1;
    if (addend && rlen <= addend[0])
	rlen = addend[0] + 1;
    ret = newbn(rlen);
    maxspot = 0;
    for (i = 1; i <= ret[0]; i++) {
	ret[i] = (i <= 2 * mlen ? workspace[4 * mlen - i] : 0);
	if (ret[i] != 0)
	    maxspot = i;
    }
    ret[0] = maxspot;

    /* now add in the addend, if any */
    if (addend) {
	BignumDblInt carry = 0;
	for (i = 1; i <= rlen; i++) {
	    carry += (i <= ret[0] ? ret[i] : 0);
	    carry += (i <= addend[0] ? addend[i] : 0);
	    ret[i] = (BignumInt) carry & BIGNUM_INT_MASK;
	    carry >>= BIGNUM_INT_BITS;
	    if (ret[i] != 0 && i > maxspot)
		maxspot = i;
	}
    }
    ret[0] = maxspot;

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
 * Convert a (max 32-bit) long into a bignum.
 */
Bignum bignum_from_long(unsigned long nn)
{
    Bignum ret;
    BignumDblInt n = nn;

    ret = newbn(3);
    ret[1] = (BignumInt)(n & BIGNUM_INT_MASK);
    ret[2] = (BignumInt)((n >> BIGNUM_INT_BITS) & BIGNUM_INT_MASK);
    ret[3] = 0;
    ret[0] = (ret[2]  ? 2 : 1);
    return ret;
}

/*
 * Add a long to a bignum.
 */
Bignum bignum_add_long(Bignum number, unsigned long addendx)
{
    Bignum ret = newbn(number[0] + 1);
    int i, maxspot = 0;
    BignumDblInt carry = 0, addend = addendx;

    for (i = 1; i <= ret[0]; i++) {
	carry += addend & BIGNUM_INT_MASK;
	carry += (i <= number[0] ? number[i] : 0);
	addend >>= BIGNUM_INT_BITS;
	ret[i] = (BignumInt) carry & BIGNUM_INT_MASK;
	carry >>= BIGNUM_INT_BITS;
	if (ret[i] != 0)
	    maxspot = i;
    }
    ret[0] = maxspot;
    return ret;
}

/*
 * Compute the residue of a bignum, modulo a (max 16-bit) short.
 */
unsigned short bignum_mod_short(Bignum number, unsigned short modulus)
{
    BignumDblInt mod, r;
    int i;

    r = 0;
    mod = modulus;
    for (i = number[0]; i > 0; i--)
	r = (r * (BIGNUM_TOP_BIT % mod) * 2 + number[i] % mod) % mod;
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
    return q;
}

/*
 * Simple remainder.
 */
Bignum bigmod(Bignum a, Bignum b)
{
    Bignum r = newbn(b[0]);
    bigdivmod(a, b, r, NULL);
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

    while (bignum_cmp(b, One) != 0) {
	Bignum t = newbn(b[0]);
	Bignum q = newbn(a[0]);
	bigdivmod(a, b, t, q);
	while (t[0] > 1 && t[t[0]] == 0)
	    t[0]--;
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

	for (i = 1; i <= newx[0]; i++) {
	    BignumInt aword = (i <= modulus[0] ? modulus[i] : 0);
	    BignumInt bword = (i <= x[0] ? x[i] : 0);
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
    BignumDblInt carry;
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
     */
    i = bignum_bitcount(x);
    ndigits = (28 * i + 92) / 93;      /* multiply by 28/93 and round up */
    ndigits++;			       /* allow for trailing \0 */
    ret = snewn(ndigits, char);

    /*
     * Now allocate some workspace to hold the binary form as we
     * repeatedly divide it by ten. Initialise this to the
     * big-endian form of the number.
     */
    workspace = snewn(x[0], BignumInt);
    for (i = 0; i < x[0]; i++)
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
	for (i = 0; i < x[0]; i++) {
	    carry = (carry << BIGNUM_INT_BITS) + workspace[i];
	    workspace[i] = (BignumInt) (carry / 10);
	    if (workspace[i])
		iszero = 0;
	    carry %= 10;
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
    sfree(workspace);
    return ret;
}
