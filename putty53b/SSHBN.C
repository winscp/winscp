/*
 * Bignum routines for RSA and DH and stuff.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "misc.h"

#define BIGNUM_INTERNAL
typedef unsigned short *Bignum;

#include "ssh.h"

unsigned short bnZero[1] = { 0 };
unsigned short bnOne[2] = { 1, 1 };

/*
 * The Bignum format is an array of `unsigned short'. The first
 * element of the array counts the remaining elements. The
 * remaining elements express the actual number, base 2^16, _least_
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
    Bignum b = smalloc((length + 1) * sizeof(unsigned short));
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
    Bignum b = smalloc((orig[0] + 1) * sizeof(unsigned short));
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
    Bignum ret = newbn(n / 16 + 1);
    bignum_set_bit(ret, n, 1);
    return ret;
}

/*
 * Compute c = a * b.
 * Input is in the first len words of a and b.
 * Result is returned in the first 2*len words of c.
 */
static void internal_mul(unsigned short *a, unsigned short *b,
			 unsigned short *c, int len)
{
    int i, j;
    unsigned long ai, t;

    for (j = 0; j < 2 * len; j++)
	c[j] = 0;

    for (i = len - 1; i >= 0; i--) {
	ai = a[i];
	t = 0;
	for (j = len - 1; j >= 0; j--) {
	    t += ai * (unsigned long) b[j];
	    t += (unsigned long) c[i + j + 1];
	    c[i + j + 1] = (unsigned short) t;
	    t = t >> 16;
	}
	c[i] = (unsigned short) t;
    }
}

static void internal_add_shifted(unsigned short *number,
				 unsigned n, int shift)
{
    int word = 1 + (shift / 16);
    int bshift = shift % 16;
    unsigned long addend;

    addend = n << bshift;

    while (addend) {
	addend += number[word];
	number[word] = (unsigned short) addend & 0xFFFF;
	addend >>= 16;
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
static void internal_mod(unsigned short *a, int alen,
			 unsigned short *m, int mlen,
			 unsigned short *quot, int qshift)
{
    unsigned short m0, m1;
    unsigned int h;
    int i, k;

    m0 = m[0];
    if (mlen > 1)
	m1 = m[1];
    else
	m1 = 0;

    for (i = 0; i <= alen - mlen; i++) {
	unsigned long t;
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
	t = ((unsigned long) h << 16) + a[i];
	q = t / m0;
	r = t % m0;

	/* Refine our estimate of q by looking at
	   h:a[i]:a[i+1] / m0:m1 */
	t = (long) m1 *(long) q;
	if (t > ((unsigned long) r << 16) + ai1) {
	    q--;
	    t -= m1;
	    r = (r + m0) & 0xffff;     /* overflow? */
	    if (r >= (unsigned long) m0 &&
		t > ((unsigned long) r << 16) + ai1) q--;
	}

	/* Subtract q * m from a[i...] */
	c = 0;
	for (k = mlen - 1; k >= 0; k--) {
	    t = (long) q *(long) m[k];
	    t += c;
	    c = t >> 16;
	    if ((unsigned short) t > a[i + k])
		c++;
	    a[i + k] -= (unsigned short) t;
	}

	/* Add back m in case of borrow */
	if (c != h) {
	    t = 0;
	    for (k = mlen - 1; k >= 0; k--) {
		t += m[k];
		t += a[i + k];
		a[i + k] = (unsigned short) t;
		t = t >> 16;
	    }
	    q--;
	}
	if (quot)
	    internal_add_shifted(quot, q, qshift + 16 * (alen - mlen - i));
    }
}

/*
 * Compute (base ^ exp) % mod.
 * The base MUST be smaller than the modulus.
 * The most significant word of mod MUST be non-zero.
 * We assume that the result array is the same size as the mod array.
 */
Bignum modpow(Bignum base, Bignum exp, Bignum mod)
{
    unsigned short *a, *b, *n, *m;
    int mshift;
    int mlen, i, j;
    Bignum result;

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = smalloc(mlen * sizeof(unsigned short));
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < 15; mshift++)
	if ((m[0] << mshift) & 0x8000)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (16 - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    /* Allocate n of size mlen, copy base to n */
    n = smalloc(mlen * sizeof(unsigned short));
    i = mlen - base[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < base[0]; j++)
	n[i + j] = base[base[0] - j];

    /* Allocate a and b of size 2*mlen. Set a = 1 */
    a = smalloc(2 * mlen * sizeof(unsigned short));
    b = smalloc(2 * mlen * sizeof(unsigned short));
    for (i = 0; i < 2 * mlen; i++)
	a[i] = 0;
    a[2 * mlen - 1] = 1;

    /* Skip leading zero bits of exp. */
    i = 0;
    j = 15;
    while (i < exp[0] && (exp[exp[0] - i] & (1 << j)) == 0) {
	j--;
	if (j < 0) {
	    i++;
	    j = 15;
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
		unsigned short *t;
		t = a;
		a = b;
		b = t;
	    }
	    j--;
	}
	i++;
	j = 15;
    }

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = mlen - 1; i < 2 * mlen - 1; i++)
	    a[i] = (a[i] << mshift) | (a[i + 1] >> (16 - mshift));
	a[2 * mlen - 1] = a[2 * mlen - 1] << mshift;
	internal_mod(a, mlen * 2, m, mlen, NULL, 0);
	for (i = 2 * mlen - 1; i >= mlen; i--)
	    a[i] = (a[i] >> mshift) | (a[i - 1] << (16 - mshift));
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

    return result;
}

/*
 * Compute (p * q) % mod.
 * The most significant word of mod MUST be non-zero.
 * We assume that the result array is the same size as the mod array.
 */
Bignum modmul(Bignum p, Bignum q, Bignum mod)
{
    unsigned short *a, *n, *m, *o;
    int mshift;
    int pqlen, mlen, rlen, i, j;
    Bignum result;

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = smalloc(mlen * sizeof(unsigned short));
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < 15; mshift++)
	if ((m[0] << mshift) & 0x8000)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (16 - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    pqlen = (p[0] > q[0] ? p[0] : q[0]);

    /* Allocate n of size pqlen, copy p to n */
    n = smalloc(pqlen * sizeof(unsigned short));
    i = pqlen - p[0];
    for (j = 0; j < i; j++)
	n[j] = 0;
    for (j = 0; j < p[0]; j++)
	n[i + j] = p[p[0] - j];

    /* Allocate o of size pqlen, copy q to o */
    o = smalloc(pqlen * sizeof(unsigned short));
    i = pqlen - q[0];
    for (j = 0; j < i; j++)
	o[j] = 0;
    for (j = 0; j < q[0]; j++)
	o[i + j] = q[q[0] - j];

    /* Allocate a of size 2*pqlen for result */
    a = smalloc(2 * pqlen * sizeof(unsigned short));

    /* Main computation */
    internal_mul(n, o, a, pqlen);
    internal_mod(a, pqlen * 2, m, mlen, NULL, 0);

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = 2 * pqlen - mlen - 1; i < 2 * pqlen - 1; i++)
	    a[i] = (a[i] << mshift) | (a[i + 1] >> (16 - mshift));
	a[2 * pqlen - 1] = a[2 * pqlen - 1] << mshift;
	internal_mod(a, pqlen * 2, m, mlen, NULL, 0);
	for (i = 2 * pqlen - 1; i >= 2 * pqlen - mlen; i--)
	    a[i] = (a[i] >> mshift) | (a[i - 1] << (16 - mshift));
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
void bigdivmod(Bignum p, Bignum mod, Bignum result, Bignum quotient)
{
    unsigned short *n, *m;
    int mshift;
    int plen, mlen, i, j;

    /* Allocate m of size mlen, copy mod to m */
    /* We use big endian internally */
    mlen = mod[0];
    m = smalloc(mlen * sizeof(unsigned short));
    for (j = 0; j < mlen; j++)
	m[j] = mod[mod[0] - j];

    /* Shift m left to make msb bit set */
    for (mshift = 0; mshift < 15; mshift++)
	if ((m[0] << mshift) & 0x8000)
	    break;
    if (mshift) {
	for (i = 0; i < mlen - 1; i++)
	    m[i] = (m[i] << mshift) | (m[i + 1] >> (16 - mshift));
	m[mlen - 1] = m[mlen - 1] << mshift;
    }

    plen = p[0];
    /* Ensure plen > mlen */
    if (plen <= mlen)
	plen = mlen + 1;

    /* Allocate n of size plen, copy p to n */
    n = smalloc(plen * sizeof(unsigned short));
    for (j = 0; j < plen; j++)
	n[j] = 0;
    for (j = 1; j <= p[0]; j++)
	n[plen - j] = p[j];

    /* Main computation */
    internal_mod(n, plen, m, mlen, quotient, mshift);

    /* Fixup result in case the modulus was shifted */
    if (mshift) {
	for (i = plen - mlen - 1; i < plen - 1; i++)
	    n[i] = (n[i] << mshift) | (n[i + 1] >> (16 - mshift));
	n[plen - 1] = n[plen - 1] << mshift;
	internal_mod(n, plen, m, mlen, quotient, 0);
	for (i = plen - 1; i >= plen - mlen; i--)
	    n[i] = (n[i] >> mshift) | (n[i - 1] << (16 - mshift));
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
	bn[i++] = 0xFFFF;
    bn[i]--;
}

Bignum bignum_from_bytes(unsigned char *data, int nbytes)
{
    Bignum result;
    int w, i;

    w = (nbytes + 1) / 2;	       /* bytes -> words */

    result = newbn(w);
    for (i = 1; i <= w; i++)
	result[i] = 0;
    for (i = nbytes; i--;) {
	unsigned char byte = *data++;
	if (i & 1)
	    result[1 + i / 2] |= byte << 8;
	else
	    result[1 + i / 2] |= byte;
    }

    while (result[0] > 1 && result[result[0]] == 0)
	result[0]--;
    return result;
}

/*
 * Read an ssh1-format bignum from a data buffer. Return the number
 * of bytes consumed.
 */
int ssh1_read_bignum(unsigned char *data, Bignum * result)
{
    unsigned char *p = data;
    int i;
    int w, b;

    w = 0;
    for (i = 0; i < 2; i++)
	w = (w << 8) + *p++;
    b = (w + 7) / 8;		       /* bits -> bytes */

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
    int bitcount = bn[0] * 16 - 1;
    while (bitcount >= 0
	   && (bn[bitcount / 16 + 1] >> (bitcount % 16)) == 0) bitcount--;
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
    if (i >= 2 * bn[0])
	return 0;		       /* beyond the end */
    else if (i & 1)
	return (bn[i / 2 + 1] >> 8) & 0xFF;
    else
	return (bn[i / 2 + 1]) & 0xFF;
}

/*
 * Return a bit from a bignum; 0 is least significant, etc.
 */
int bignum_bit(Bignum bn, int i)
{
    if (i >= 16 * bn[0])
	return 0;		       /* beyond the end */
    else
	return (bn[i / 16 + 1] >> (i % 16)) & 1;
}

/*
 * Set a bit in a bignum; 0 is least significant, etc.
 */
void bignum_set_bit(Bignum bn, int bitnum, int value)
{
    if (bitnum >= 16 * bn[0])
	abort();		       /* beyond the end */
    else {
	int v = bitnum / 16 + 1;
	int mask = 1 << (bitnum % 16);
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
	unsigned short aval = (i > amax ? 0 : a[i]);
	unsigned short bval = (i > bmax ? 0 : b[i]);
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
    unsigned short ai, ai1;

    bits = bignum_bitcount(a) - shift;
    ret = newbn((bits + 15) / 16);

    if (ret) {
	shiftw = shift / 16;
	shiftb = shift % 16;
	shiftbb = 16 - shiftb;

	ai1 = a[shiftw + 1];
	for (i = 1; i <= ret[0]; i++) {
	    ai = ai1;
	    ai1 = (i + shiftw + 1 <= a[0] ? a[i + shiftw + 1] : 0);
	    ret[i] = ((ai >> shiftb) | (ai1 << shiftbb)) & 0xFFFF;
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
    unsigned short *workspace;
    Bignum ret;

    /* mlen space for a, mlen space for b, 2*mlen for result */
    workspace = smalloc(mlen * 4 * sizeof(unsigned short));
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
	unsigned long carry = 0;
	for (i = 1; i <= rlen; i++) {
	    carry += (i <= ret[0] ? ret[i] : 0);
	    carry += (i <= addend[0] ? addend[i] : 0);
	    ret[i] = (unsigned short) carry & 0xFFFF;
	    carry >>= 16;
	    if (ret[i] != 0 && i > maxspot)
		maxspot = i;
	}
    }
    ret[0] = maxspot;

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
    unsigned short j;

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
	ret[i] = 0xFFFF;
    return ret;
}

/*
 * Convert a (max 32-bit) long into a bignum.
 */
Bignum bignum_from_long(unsigned long n)
{
    Bignum ret;

    ret = newbn(3);
    ret[1] = (unsigned short)(n & 0xFFFF);
    ret[2] = (unsigned short)((n >> 16) & 0xFFFF);
    ret[3] = 0;
    ret[0] = (ret[2]  ? 2 : 1);
    return ret;
}

/*
 * Add a long to a bignum.
 */
Bignum bignum_add_long(Bignum number, unsigned long addend)
{
    Bignum ret = newbn(number[0] + 1);
    int i, maxspot = 0;
    unsigned long carry = 0;

    for (i = 1; i <= ret[0]; i++) {
	carry += addend & 0xFFFF;
	carry += (i <= number[0] ? number[i] : 0);
	addend >>= 16;
	ret[i] = (unsigned short) carry & 0xFFFF;
	carry >>= 16;
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
    unsigned long mod, r;
    int i;

    r = 0;
    mod = modulus;
    for (i = number[0]; i > 0; i--)
	r = (r * 65536 + number[i]) % mod;
    return (unsigned short) r;
}

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
    }

    freebn(b);
    freebn(a);
    freebn(xp);

    /* now we know that sign * x == 1, and that x < modulus */
    if (sign < 0) {
	/* set a new x to be modulus - x */
	Bignum newx = newbn(modulus[0]);
	unsigned short carry = 0;
	int maxspot = 1;
	int i;

	for (i = 1; i <= newx[0]; i++) {
	    unsigned short aword = (i <= modulus[0] ? modulus[i] : 0);
	    unsigned short bword = (i <= x[0] ? x[i] : 0);
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
    unsigned long carry;
    char *ret;
    unsigned short *workspace;

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
    ret = smalloc(ndigits);

    /*
     * Now allocate some workspace to hold the binary form as we
     * repeatedly divide it by ten. Initialise this to the
     * big-endian form of the number.
     */
    workspace = smalloc(sizeof(unsigned short) * x[0]);
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
	    carry = (carry << 16) + workspace[i];
	    workspace[i] = (unsigned short) (carry / 10);
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
    return ret;
}
