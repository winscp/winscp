/*
 * DSS key generation.
 */

#include "misc.h"
#include "ssh.h"

int dsa_generate(struct dss_key *key, int bits, progfn_t pfn,
		 void *pfnparam)
{
    Bignum qm1, power, g, h, tmp;
    int progress;

    /*
     * Set up the phase limits for the progress report. We do this
     * by passing minus the phase number.
     *
     * For prime generation: our initial filter finds things
     * coprime to everything below 2^16. Computing the product of
     * (p-1)/p for all prime p below 2^16 gives about 20.33; so
     * among B-bit integers, one in every 20.33 will get through
     * the initial filter to be a candidate prime.
     *
     * Meanwhile, we are searching for primes in the region of 2^B;
     * since pi(x) ~ x/log(x), when x is in the region of 2^B, the
     * prime density will be d/dx pi(x) ~ 1/log(B), i.e. about
     * 1/0.6931B. So the chance of any given candidate being prime
     * is 20.33/0.6931B, which is roughly 29.34 divided by B.
     *
     * So now we have this probability P, we're looking at an
     * exponential distribution with parameter P: we will manage in
     * one attempt with probability P, in two with probability
     * P(1-P), in three with probability P(1-P)^2, etc. The
     * probability that we have still not managed to find a prime
     * after N attempts is (1-P)^N.
     * 
     * We therefore inform the progress indicator of the number B
     * (29.34/B), so that it knows how much to increment by each
     * time. We do this in 16-bit fixed point, so 29.34 becomes
     * 0x1D.57C4.
     */
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 1, 0x2800);
    pfn(pfnparam, PROGFN_EXP_PHASE, 1, -0x1D57C4 / 160);
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 2, 0x40 * bits);
    pfn(pfnparam, PROGFN_EXP_PHASE, 2, -0x1D57C4 / bits);

    /*
     * In phase three we are finding an order-q element of the
     * multiplicative group of p, by finding an element whose order
     * is _divisible_ by q and raising it to the power of (p-1)/q.
     * _Most_ elements will have order divisible by q, since for a
     * start phi(p) of them will be primitive roots. So
     * realistically we don't need to set this much below 1 (64K).
     * Still, we'll set it to 1/2 (32K) to be on the safe side.
     */
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 3, 0x2000);
    pfn(pfnparam, PROGFN_EXP_PHASE, 3, -32768);

    /*
     * In phase four we are finding an element x between 1 and q-1
     * (exclusive), by inventing 160 random bits and hoping they
     * come out to a plausible number; so assuming q is uniformly
     * distributed between 2^159 and 2^160, the chance of any given
     * attempt succeeding is somewhere between 0.5 and 1. Lacking
     * the energy to arrange to be able to specify this probability
     * _after_ generating q, we'll just set it to 0.75.
     */
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 4, 0x2000);
    pfn(pfnparam, PROGFN_EXP_PHASE, 4, -49152);

    pfn(pfnparam, PROGFN_READY, 0, 0);

    /*
     * Generate q: a prime of length 160.
     */
    key->q = primegen(160, 2, 2, NULL, 1, pfn, pfnparam);
    /*
     * Now generate p: a prime of length `bits', such that p-1 is
     * divisible by q.
     */
    key->p = primegen(bits-160, 2, 2, key->q, 2, pfn, pfnparam);

    /*
     * Next we need g. Raise 2 to the power (p-1)/q modulo p, and
     * if that comes out to one then try 3, then 4 and so on. As
     * soon as we hit a non-unit (and non-zero!) one, that'll do
     * for g.
     */
    power = bigdiv(key->p, key->q);    /* this is floor(p/q) == (p-1)/q */
    h = bignum_from_long(1);
    progress = 0;
    while (1) {
	pfn(pfnparam, PROGFN_PROGRESS, 3, ++progress);
	g = modpow(h, power, key->p);
	if (bignum_cmp(g, One) > 0)
	    break;		       /* got one */
	tmp = h;
	h = bignum_add_long(h, 1);
	freebn(tmp);
    }
    key->g = g;
    freebn(h);

    /*
     * Now we're nearly done. All we need now is our private key x,
     * which should be a number between 1 and q-1 exclusive, and
     * our public key y = g^x mod p.
     */
    qm1 = copybn(key->q);
    decbn(qm1);
    progress = 0;
    while (1) {
	int i, v, byte, bitsleft;
	Bignum x;

	pfn(pfnparam, PROGFN_PROGRESS, 4, ++progress);
	x = bn_power_2(159);
	byte = 0;
	bitsleft = 0;

	for (i = 0; i < 160; i++) {
	    if (bitsleft <= 0)
		bitsleft = 8, byte = random_byte();
	    v = byte & 1;
	    byte >>= 1;
	    bitsleft--;
	    bignum_set_bit(x, i, v);
	}

	if (bignum_cmp(x, One) <= 0 || bignum_cmp(x, qm1) >= 0) {
	    freebn(x);
	    continue;
	} else {
	    key->x = x;
	    break;
	}
    }
    freebn(qm1);

    key->y = modpow(key->g, key->x, key->p);

    return 1;
}
