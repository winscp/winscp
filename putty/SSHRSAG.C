/*
 * RSA key generation.
 */

#include "ssh.h"

#define RSA_EXPONENT 37		       /* we like this prime */

int rsa_generate(struct RSAKey *key, int bits, progfn_t pfn,
		 void *pfnparam)
{
    Bignum pm1, qm1, phi_n;

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
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 1, 0x10000);
    pfn(pfnparam, PROGFN_EXP_PHASE, 1, -0x1D57C4 / (bits / 2));
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 2, 0x10000);
    pfn(pfnparam, PROGFN_EXP_PHASE, 2, -0x1D57C4 / (bits - bits / 2));
    pfn(pfnparam, PROGFN_PHASE_EXTENT, 3, 0x4000);
    pfn(pfnparam, PROGFN_LIN_PHASE, 3, 5);
    pfn(pfnparam, PROGFN_READY, 0, 0);

    /*
     * We don't generate e; we just use a standard one always.
     */
    key->exponent = bignum_from_long(RSA_EXPONENT);

    /*
     * Generate p and q: primes with combined length `bits', not
     * congruent to 1 modulo e. (Strictly speaking, we wanted (p-1)
     * and e to be coprime, and (q-1) and e to be coprime, but in
     * general that's slightly more fiddly to arrange. By choosing
     * a prime e, we can simplify the criterion.)
     */
    key->p = primegen(bits / 2, RSA_EXPONENT, 1, NULL,
		      1, pfn, pfnparam);
    key->q = primegen(bits - bits / 2, RSA_EXPONENT, 1, NULL,
		      2, pfn, pfnparam);

    /*
     * Ensure p > q, by swapping them if not.
     */
    if (bignum_cmp(key->p, key->q) < 0) {
	Bignum t = key->p;
	key->p = key->q;
	key->q = t;
    }

    /*
     * Now we have p, q and e. All we need to do now is work out
     * the other helpful quantities: n=pq, d=e^-1 mod (p-1)(q-1),
     * and (q^-1 mod p).
     */
    pfn(pfnparam, PROGFN_PROGRESS, 3, 1);
    key->modulus = bigmul(key->p, key->q);
    pfn(pfnparam, PROGFN_PROGRESS, 3, 2);
    pm1 = copybn(key->p);
    decbn(pm1);
    qm1 = copybn(key->q);
    decbn(qm1);
    phi_n = bigmul(pm1, qm1);
    pfn(pfnparam, PROGFN_PROGRESS, 3, 3);
    freebn(pm1);
    freebn(qm1);
    key->private_exponent = modinv(key->exponent, phi_n);
    pfn(pfnparam, PROGFN_PROGRESS, 3, 4);
    key->iqmp = modinv(key->q, key->p);
    pfn(pfnparam, PROGFN_PROGRESS, 3, 5);

    /*
     * Clean up temporary numbers.
     */
    freebn(phi_n);

    return 1;
}
