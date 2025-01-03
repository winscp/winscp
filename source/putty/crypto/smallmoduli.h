/*
 * Shared code between algorithms whose state consists of a large
 * collection of residues mod a small prime.
 */

/*
 * We need to do modular arithmetic on small values (considerably
 * smaller than 2^16), and we need to do it without using integer
 * division which might not be time-safe. Input values might not fit
 * in a 16-bit int, because we'll also be multiplying mod q.
 *
 * The strategy for this is the same as I used in
 * mp_mod_known_integer: see there for the proofs. The basic idea is
 * that we precompute the reciprocal of our modulus as a fixed-point
 * number, and use that to get an approximate quotient which we
 * subtract off. For these integer sizes, precomputing a fixed-point
 * reciprocal of the form (2^48 / modulus) leaves us at most off by 1
 * in the quotient, so there's a single (time-safe) trial subtraction
 * at the end.
 *
 * (It's possible that some speed could be gained by not reducing
 * fully at every step. But then you'd have to carefully identify all
 * the places in the algorithm where things are compared to zero. This
 * was the easiest way to get it all working in the first place.)
 */

/* Precompute the reciprocal */
static inline uint64_t reciprocal_for_reduction(uint16_t q)
{
    return ((uint64_t)1 << 48) / q;
}

/* Reduce x mod q, assuming qrecip == reciprocal_for_reduction(q) */
static inline uint16_t reduce(uint32_t x, uint16_t q, uint64_t qrecip)
{
    uint64_t unshifted_quot = x * qrecip;
    uint64_t quot = unshifted_quot >> 48;
    uint16_t reduced = x - quot * q;
    reduced -= q * (1 & ((q-1 - reduced) >> 15));
    return reduced;
}

/* Reduce x mod q as above, but also return the quotient */
static inline uint16_t reduce_with_quot(uint32_t x, uint32_t *quot_out,
                                        uint16_t q, uint64_t qrecip)
{
    uint64_t unshifted_quot = x * qrecip;
    uint64_t quot = unshifted_quot >> 48;
    uint16_t reduced = x - quot * q;
    uint64_t extraquot = (1 & ((q-1 - reduced) >> 15));
    reduced -= extraquot * q;
    *quot_out = quot + extraquot;
    return reduced;
}
