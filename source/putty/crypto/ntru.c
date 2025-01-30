/*
 * Implementation of OpenSSH 9.x's hybrid key exchange protocol
 * sntrup761x25519-sha512@openssh.com .
 *
 * This consists of the 'Streamlined NTRU Prime' quantum-resistant
 * cryptosystem, run in parallel with ordinary Curve25519 to generate
 * a shared secret combining the output of both systems.
 *
 * (Hence, even if you don't trust this newfangled NTRU Prime thing at
 * all, it's at least no _less_ secure than the kex you were using
 * already.)
 *
 * References for the NTRU Prime cryptosystem, up to and including
 * binary encodings of public and private keys and the exact preimages
 * of the hashes used in key exchange:
 *
 * https://ntruprime.cr.yp.to/
 * https://ntruprime.cr.yp.to/nist/ntruprime-20201007.pdf
 *
 * The SSH protocol layer is not documented anywhere I could find (as
 * of 2022-04-15, not even in OpenSSH's PROTOCOL.* files). I had to
 * read OpenSSH's source code to find out how it worked, and the
 * answer is as follows:
 *
 * This hybrid kex method is treated for SSH purposes as a form of
 * elliptic-curve Diffie-Hellman, and shares the same SSH message
 * sequence: client sends SSH2_MSG_KEX_ECDH_INIT containing its public
 * half, server responds with SSH2_MSG_KEX_ECDH_REPLY containing _its_
 * public half plus the host key and signature on the shared secret.
 *
 * (This is a bit of a fudge, because unlike actual ECDH, this kex
 * method is asymmetric: one side sends a public key, and the other
 * side encrypts something with it and sends the ciphertext back. So
 * while the normal ECDH implementations can compute the two sides
 * independently in parallel, this system reusing the same messages
 * has to be serial. But the order of the messages _is_ firmly
 * specified in SSH ECDH, so it works anyway.)
 *
 * For this kex method, SSH2_MSG_KEX_ECDH_INIT still contains a single
 * SSH 'string', which consists of the concatenation of a Streamlined
 * NTRU Prime public key with the Curve25519 public value. (Both of
 * these have fixed length in bytes, so there's no ambiguity in the
 * concatenation.)
 *
 * SSH2_MSG_KEX_ECDH_REPLY is mostly the same as usual. The only
 * string in the packet that varies is the second one, which would
 * normally contain the server's public elliptic curve point. Instead,
 * it now contains the concatenation of
 *
 *  - a Streamlined NTRU Prime ciphertext
 *  - the 'confirmation hash' specified in ntruprime-20201007.pdf,
 *    hashing the plaintext of that ciphertext together with the
 *    public key
 *  - the Curve25519 public point as usual.
 *
 * Again, all three of those elements have fixed lengths.
 *
 * The client decrypts the ciphertext, checks the confirmation hash,
 * and if successful, generates the 'session hash' specified in
 * ntruprime-20201007.pdf, which is 32 bytes long and is the ultimate
 * output of the Streamlined NTRU Prime key exchange.
 *
 * The output of the hybrid kex method as a whole is an SSH 'string'
 * of length 64 containing the SHA-512 hash of the concatenatio of
 *
 *  - the Streamlined NTRU Prime session hash (32 bytes)
 *  - the Curve25519 shared secret (32 bytes).
 *
 * That string is included directly into the SSH exchange hash and key
 * derivation hashes, in place of the mpint that comes out of most
 * other kex methods.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "mpint.h"
#include "ntru.h"
#include "smallmoduli.h"

/* Invert x mod q, assuming it's nonzero. (For time-safety, no check
 * is made for zero; it just returns 0.)
 *
 * Expects qrecip == reciprocal_for_reduction(q). (But it's passed in
 * as a parameter to save recomputing it, on the theory that the
 * caller will have had it lying around already in most cases.) */
static uint16_t invert(uint16_t x, uint16_t q, uint64_t qrecip)
{
    /* Fermat inversion: compute x^(q-2), since x^(q-1) == 1. */
    uint32_t sq = x, bit = 1, acc = 1, exp = q-2;
    while (1) {
        if (exp & bit) {
            acc = reduce(acc * sq, q, qrecip);
            exp &= ~bit;
            if (!exp)
                return acc;
        }
        sq = reduce(sq * sq, q, qrecip);
        bit <<= 1;
    }
}

/* Check whether x == 0, time-safely, and return 1 if it is or 0 otherwise. */
static unsigned iszero(uint16_t x)
{
    return 1 & ~((x + 0xFFFF) >> 16);
}

/*
 * Handy macros to cut down on all those extra function parameters. In
 * the common case where a function is working mod the same modulus
 * throughout (and has called it q), you can just write 'SETUP;' at
 * the top and then call REDUCE(...) and INVERT(...) without having to
 * write out q and qrecip every time.
 */
#define SETUP uint64_t qrecip = reciprocal_for_reduction(q)
#define REDUCE(x) reduce(x, q, qrecip)
#define INVERT(x) invert(x, q, qrecip)

/* ----------------------------------------------------------------------
 * Quotient-ring functions.
 *
 * NTRU Prime works with two similar but different quotient rings:
 *
 *   Z_q[x] / <x^p-x-1>      where p,q are the prime parameters of the system
 *   Z_3[x] / <x^p-x-1>      with the same p, but coefficients mod 3.
 *
 * The former is a field (every nonzero element is invertible),
 * because the system parameters are chosen such that x^p-x-1 is
 * invertible over Z_q. The latter is not a field (or not necessarily,
 * and in particular, not for the value of p we use here).
 *
 * In these core functions, you pass in the modulus you want as the
 * parameter q, which is either the 'real' q specified in the system
 * parameters, or 3 if you're doing one of the mod-3 parts of the
 * algorithm.
 */

/*
 * Multiply two elements of a quotient ring.
 *
 * 'a' and 'b' are arrays of exactly p coefficients, with constant
 * term first. 'out' is an array the same size to write the inverse
 * into.
 */
void ntru_ring_multiply(uint16_t *out, const uint16_t *a, const uint16_t *b,
                        unsigned p, unsigned q)
{
    SETUP;

    /*
     * Strategy: just compute the full product with 2p coefficients,
     * and then reduce it mod x^p-x-1 by working downwards from the
     * top coefficient replacing x^{p+k} with (x+1)x^k for k = ...,1,0.
     *
     * Possibly some speed could be gained here by doing the recursive
     * Karatsuba optimisation for the initial multiplication? But I
     * haven't tried it.
     */
    uint32_t *unreduced = snewn(2*p, uint32_t);
    { // WINSCP
    unsigned i;
    for (i = 0; i < 2*p; i++)
        unreduced[i] = 0;
    } // WINSCP
    { // WINSCP
    unsigned i, j;
    for (i = 0; i < p; i++)
        for (j = 0; j < p; j++)
            unreduced[i+j] = REDUCE(unreduced[i+j] + a[i] * b[j]);
    } // WINSCP

    { // WINSCP
    unsigned i;
    for (i = 2*p - 1; i >= p; i--) {
        unreduced[i-p] += unreduced[i];
        unreduced[i-p+1] += unreduced[i];
        unreduced[i] = 0;
    }
    } // WINSCP

    { // WINSCP
    unsigned i;
    for (i = 0; i < p; i++)
        out[i] = REDUCE(unreduced[i]);
    } // WINSCP

    smemclr(unreduced, 2*p * sizeof(*unreduced));
    sfree(unreduced);
}

/*
 * Invert an element of the quotient ring.
 *
 * 'in' is an array of exactly p coefficients, with constant term
 * first. 'out' is an array the same size to write the inverse into.
 *
 * Method: essentially Stein's gcd algorithm, taking the gcd of the
 * input (regarded as an element of Z_q[x] proper) and x^p-x-1. Given
 * two polynomials over a field which are not both divisible by x, you
 * can find their gcd by iterating the following procedure:
 *
 *  - if one is divisible by x, divide off x
 *  - otherwise, subtract from the higher-degree one whatever scalar
 *    multiple of the lower-degree one will make it divisible by x,
 *    and _then_ divide off x
 *
 * Neither of these types of step changes the gcd of the two
 * polynomials.
 *
 * Each step reduces the sum of the two polynomials' degree by at
 * least one, as long as at least one of the degrees is positive.
 * (Maybe more than one if all the stars align in the second case, if
 * the subtraction cancels the leading term as well as the constant
 * term.) So in at most deg A + deg B steps, we must have reached the
 * situation where both polys are constants; in one more step after
 * that, one of them will be zero; and in one step after _that_, the
 * zero one will reliably be the one we're dividing by x. Or rather,
 * that's what happens in the case where A,B are coprime; if not, then
 * one hits zero while the other is still nonzero.
 *
 * In a normal gcd algorithm, you'd track a linear combination of the
 * two original polynomials that yields each working value, and end up
 * with a linear combination of the inputs that yields the gcd. In
 * this algorithm, the 'divide off x' step makes that awkward - but we
 * can solve that by instead multiplying by the inverse of x in the
 * ring that we want our answer to be valid in! And since the modulus
 * polynomial of the ring is x^p-x-1, the inverse of x is easy to
 * calculate, because it's always just x^{p-1} - 1, which is also very
 * easy to multiply by.
 */
unsigned ntru_ring_invert(uint16_t *out, const uint16_t *in,
                          unsigned p, unsigned q)
{
    SETUP;

    /* Size of the polynomial arrays we'll work with */
    const size_t SIZE = p+1;

    /* Number of steps of the algorithm is the max possible value of
     * deg A + deg B + 2, where deg A <= p-1 and deg B = p */
    const size_t STEPS = 2*p + 1;

    /* Our two working polynomials */
    uint16_t *A = snewn(SIZE, uint16_t);
    uint16_t *B = snewn(SIZE, uint16_t);

    /* Coefficient of the input value in each one */
    uint16_t *Ac = snewn(SIZE, uint16_t);
    uint16_t *Bc = snewn(SIZE, uint16_t);

    /* Initialise A to the input, and Ac correspondingly to 1 */
    memcpy(A, in, p*sizeof(uint16_t));
    A[p] = 0;
    Ac[0] = 1;
    { // WINSCP
    size_t i;
    for (i = 1; i < SIZE; i++)
        Ac[i] = 0;
    } // WINSCP

    /* Initialise B to the quotient polynomial of the ring, x^p-x-1
     * And Bc = 0 */
    B[0] = B[1] = q-1;
    { // WINSCP
    size_t i;
    for (i = 2; i < p; i++)
        B[i] = 0;
    } // WINSCP
    B[p] = 1;
    { // WINSCP
    size_t i;
    for (i = 0; i < SIZE; i++)
        Bc[i] = 0;
    } // WINSCP

    /* Run the gcd-finding algorithm. */
    { // WINSCP
    size_t i;
    for (i = 0; i < STEPS; i++) {
        /*
         * First swap round so that A is the one we'll be dividing by x.
         *
         * In the case where one of the two polys has a zero constant
         * term, it's that one. In the other case, it's the one of
         * smaller degree. We must compute both, and choose between
         * them in a side-channel-safe way.
         */
        unsigned x_divides_A = iszero(A[0]);
        unsigned x_divides_B = iszero(B[0]);
        unsigned B_is_bigger = 0;
        {
            unsigned not_seen_top_term_of_A = 1, not_seen_top_term_of_B = 1;
            { // WINSCP
            size_t j;
            for (j = SIZE; j-- > 0 ;) {
                not_seen_top_term_of_A &= iszero(A[j]);
                not_seen_top_term_of_B &= iszero(B[j]);
                B_is_bigger |= (~not_seen_top_term_of_B &
                                not_seen_top_term_of_A);
            }
            } // WINSCP
        }
        { // WINSCP
        unsigned need_swap = x_divides_B | (~x_divides_A & B_is_bigger);
        uint16_t swap_mask = (uint64_t)-(int64_t)need_swap; // WINSCP
        { // WINSCP
        size_t j;
        for (j = 0; j < SIZE; j++) {
            uint16_t diff = (A[j] ^ B[j]) & swap_mask;
            A[j] ^= diff;
            B[j] ^= diff;
        }
        } // WINSCP
        { // WINSCP
        size_t j;
        for (j = 0; j < SIZE; j++) {
            uint16_t diff = (Ac[j] ^ Bc[j]) & swap_mask;
            Ac[j] ^= diff;
            Bc[j] ^= diff;
        }
        } // WINSCP

        /*
         * Replace A with a linear combination of both A and B that
         * has constant term zero, which we do by calculating
         *
         *   (constant term of B) * A - (constant term of A) * B
         *
         * In one of the two cases, A's constant term is already zero,
         * so the coefficient of B will be zero too; hence, this will
         * do nothing useful (it will merely scale A by some scalar
         * value), but it will take the same length of time as doing
         * something, which is just what we want.
         */
        { // WINSCP
        uint16_t Amult = B[0], Bmult = q - A[0];
        { // WINSCP
        size_t j; // WINSCP
        for (j = 0; j < SIZE; j++)
            A[j] = REDUCE(Amult * A[j] + Bmult * B[j]);
        } // WINSCP
        /* And do the same transformation to Ac */
        { // WINSCP
        size_t j;
        for (j = 0; j < SIZE; j++)
            Ac[j] = REDUCE(Amult * Ac[j] + Bmult * Bc[j]);
        } // WINSCP

        /*
         * Now divide A by x, and compensate by multiplying Ac by
         * x^{p-1}-1 mod x^p-x-1.
         *
         * That multiplication is particularly easy, precisely because
         * x^{p-1}-1 is the multiplicative inverse of x! Each x^n term
         * for n>0 just moves down to the x^{n-1} term, and only the
         * constant term has to be dealt with in an interesting way.
         */
        { // WINSCP
        size_t j;
        for (j = 1; j < SIZE; j++)
            A[j-1] = A[j];
        } // WINSCP
        A[SIZE-1] = 0;
        { // WINSCP
        uint16_t Ac0 = Ac[0];
        { // WINSCP
        size_t j;
        for (j = 1; j < p; j++)
            Ac[j-1] = Ac[j];
        } // WINSCP
        Ac[p-1] = Ac0;
        Ac[0] = REDUCE(Ac[0] + q - Ac0);
        } // WINSCP
        } // WINSCP
    }
    } // WINSCP

    /*
     * Now we expect that A is 0, and B is a constant. If so, then
     * they are coprime, and we're going to return success. If not,
     * they have a common factor.
     */
    { // WINSCP
    unsigned success = iszero(A[0]) & (1 ^ iszero(B[0]));
    { // WINSCP
    size_t j;
    for (j = 1; j < SIZE; j++)
        success &= iszero(A[j]) & iszero(B[j]);
    } // WINSCP

    /*
     * So we're going to return Bc, but first, scale it by the
     * multiplicative inverse of the constant we ended up with in
     * B[0].
     */
    { // WINSCP
    uint16_t scale = INVERT(B[0]);
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++)
        out[i] = REDUCE(scale * Bc[i]);
    } // WINSCP

    smemclr(A, SIZE * sizeof(*A));
    sfree(A);
    smemclr(B, SIZE * sizeof(*B));
    sfree(B);
    smemclr(Ac, SIZE * sizeof(*Ac));
    sfree(Ac);
    smemclr(Bc, SIZE * sizeof(*Bc));
    sfree(Bc);

    return success;
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/*
 * Given an array of values mod q, convert each one to its
 * minimum-absolute-value representative, and then reduce mod 3.
 *
 * Output values are 0, 1 and 0xFFFF, representing -1.
 *
 * (Normally our arrays of uint16_t are in 'minimal non-negative
 * residue' form, so the output of this function is unusual. But it's
 * useful to have it in this form so that it can be reused by
 * ntru_round3. You can put it back to the usual representation using
 * ntru_normalise, below.)
 */
void ntru_mod3(uint16_t *out, const uint16_t *in, unsigned p, unsigned q)
{
    uint64_t qrecip = reciprocal_for_reduction(q);
    uint64_t recip3 = reciprocal_for_reduction(3);

    unsigned bias = q/2;
    uint16_t adjust = 3 - reduce(bias-1, 3, recip3);

    { // WINSCP
    unsigned i;
    for (i = 0; i < p; i++) {
        uint16_t val = reduce(in[i] + bias, q, qrecip);
        uint16_t residue = reduce(val + adjust, 3, recip3);
        out[i] = residue - 1;
    }
    } // WINSCP
}

/*
 * Given an array of values mod q, round each one to the nearest
 * multiple of 3 to its minimum-absolute-value representative.
 *
 * Output values are signed integers coerced to uint16_t, so again,
 * use ntru_normalise afterwards to put them back to normal.
 */
void ntru_round3(uint16_t *out, const uint16_t *in, unsigned p, unsigned q)
{
    SETUP;
    unsigned bias = q/2;
    ntru_mod3(out, in, p, q);
    { // WINSCP
    unsigned i;
    for (i = 0; i < p; i++)
        out[i] = REDUCE(in[i] + bias) - bias - out[i];
    } // WINSCP
}

/*
 * Given an array of signed integers coerced to uint16_t in the range
 * [-q/2,+q/2], normalise them back to mod q values.
 */
static void ntru_normalise(uint16_t *out, const uint16_t *in,
                           unsigned p, unsigned q)
{
    unsigned i; // WINSCP
    for (i = 0; i < p; i++)
        out[i] = in[i] + q * (in[i] >> 15);
}

/*
 * Given an array of values mod q, add a constant to each one.
 */
void ntru_bias(uint16_t *out, const uint16_t *in, unsigned bias,
               unsigned p, unsigned q)
{
    SETUP;
    unsigned i; // WINSCP
    for (i = 0; i < p; i++)
        out[i] = REDUCE(in[i] + bias);
}

/*
 * Given an array of values mod q, multiply each one by a constant.
 */
void ntru_scale(uint16_t *out, const uint16_t *in, uint16_t scale,
                unsigned p, unsigned q)
{
    SETUP;
    unsigned i; // WINSCP
    for (i = 0; i < p; i++)
        out[i] = REDUCE(in[i] * scale);
}

/*
 * Given an array of values mod 3, convert them to values mod q in a
 * way that maps -1,0,+1 to -1,0,+1.
 */
static void ntru_expand(
    uint16_t *out, const uint16_t *in, unsigned p, unsigned q)
{
    size_t i; // WINSCP
    for (i = 0; i < p; i++) {
        uint16_t v = in[i];
        /* Map 2 to q-1, and leave 0 and 1 unchanged */
        v += (v >> 1) * (q-3);
        out[i] = v;
    }
}

/* ----------------------------------------------------------------------
 * Implement the binary encoding from ntruprime-20201007.pdf, which is
 * used to encode public keys and ciphertexts (though not plaintexts,
 * which are done in a much simpler way).
 *
 * The general idea is that your encoder takes as input a list of
 * small non-negative integers (r_i), and a sequence of limits (m_i)
 * such that 0 <= r_i < m_i, and emits a sequence of bytes that encode
 * all of these as tightly as reasonably possible.
 *
 * That's more general than is really needed, because in both the
 * actual uses of this encoding, the input m_i are all the same! But
 * the array of (r_i,m_i) pairs evolves during encoding, so they don't
 * _stay_ all the same, so you still have to have all the generality.
 *
 * The encoding process makes a number of passes along the list of
 * inputs. In each step, pairs of adjacent numbers are combined into
 * one larger one by turning (r_i,m_i) and (r_{i+1},m_{i+1}) into the
 * pair (r_i + m_i r_{i+1}, m_i m_{i+1}), i.e. so that the original
 * numbers could be recovered by taking the quotient and remaiinder of
 * the new r value by m_i. Then, if the new m_i is at least 2^14, we
 * emit the low 8 bits of r_i to the output stream and reduce r_i and
 * its limit correspondingly. So at the end of the pass, we've got
 * half as many numbers still to encode, they're all still not too
 * big, and we've emitted some amount of data into the output. Then do
 * another pass, keep going until there's only one number left, and
 * emit it little-endian.
 *
 * That's all very well, but how do you decode it again? DJB exhibits
 * a pair of recursive functions that are supposed to be mutually
 * inverse, but I didn't have any confidence that I'd be able to debug
 * them sensibly if they turned out not to be (or rather, if I
 * implemented one of them wrong). So I came up with my own strategy
 * instead.
 *
 * In my strategy, we start by processing just the (m_i) into an
 * 'encoding schedule' consisting of a sequence of simple
 * instructions. The instructions operate on a FIFO queue of numbers,
 * initialised to the original (r_i). The three instruction types are:
 *
 *  - 'COMBINE': consume two numbers a,b from the head of the queue,
 *    combine them by calculating a + m*b for some specified m, and
 *    push the result on the tail of the queue.
 *
 *  - 'BYTE': divide the tail element of the queue by 2^8 and emit the
 *    low bits into the output stream.
 *
 *  - 'COPY': pop a number from the head of the queue and push it
 *    straight back on the tail. (Used for handling the leftover
 *    element at the end of a pass if the input to the pass was a list
 *    of odd length.)
 *
 * So we effectively implement DJB's encoding process in simulation,
 * and instead of actually processing a set of (r_i), we 'compile' the
 * process into a sequence of instructions that can be handed just the
 * (r_i) later and encode them in the right way. At the end of the
 * instructions, the queue is expected to have been reduced to length
 * 1 and contain the single integer 0.
 *
 * The nice thing about this system is that each of those three
 * instructions is easy to reverse. So you can also use the same
 * instructions for decoding: start with a queue containing 0, and
 * process the instructions in reverse order and reverse sense. So
 * BYTE means to _consume_ a byte from the encoded data (starting from
 * the rightmost end) and use it to make a queue element bigger; and
 * COMBINE run in reverse pops a single element from one end of the
 * queue, divides it by m, and pushes the quotient and remainder on
 * the other end.
 *
 * (So it's easy to debug, because the queue passes through the exact
 * same sequence of states during decoding that it did during
 * encoding, just in reverse order.)
 *
 * Also, the encoding schedule comes with information about the
 * expected size of the encoded data, because you can find that out
 * easily by just counting the BYTE commands.
 */

enum {
    /*
     * Command values appearing in the 'ops' array. ENC_COPY and
     * ENC_BYTE are single values; values of the form
     * (ENC_COMBINE_BASE + m) represent a COMBINE command with
     * parameter m.
     */
    ENC_COPY, ENC_BYTE, ENC_COMBINE_BASE
};
struct NTRUEncodeSchedule {
    /*
     * Object representing a compiled set of encoding instructions.
     *
     * 'nvals' is the number of r_i we expect to encode. 'nops' is the
     * number of encoding commands in the 'ops' list; 'opsize' is the
     * physical size of the array, used during construction.
     *
     * 'endpos' is used to avoid a last-minute faff during decoding.
     * We implement our FIFO of integers as a ring buffer of size
     * 'nvals'. Encoding cycles round it some number of times, and the
     * final 0 element ends up at some random location in the array.
     * If we know _where_ the 0 ends up during encoding, we can put
     * the initial 0 there at the start of decoding, and then when we
     * finish reversing all the instructions, we'll end up with the
     * output numbers already arranged at their correct positions, so
     * that there's no need to rotate the array at the last minute.
     */
    size_t nvals, endpos, nops, opsize;
    uint32_t *ops;
};
static inline void sched_append(NTRUEncodeSchedule *sched, uint16_t op)
{
    /* Helper function to append an operation to the schedule, and
     * update endpos. */
    sgrowarray(sched->ops, sched->opsize, sched->nops);
    sched->ops[sched->nops++] = op;
    if (op != ENC_BYTE)
        sched->endpos = (sched->endpos + 1) % sched->nvals;
}

/*
 * Take in the list of limit values (m_i) and compute the encoding
 * schedule.
 */
NTRUEncodeSchedule *ntru_encode_schedule(const uint16_t *ms_in, size_t n)
{
    NTRUEncodeSchedule *sched = snew(NTRUEncodeSchedule);
    sched->nvals = n;
    sched->endpos = n-1;
    sched->nops = sched->opsize = 0;
    sched->ops = NULL;

    assert(n != 0);

    /*
     * 'ms' is the list of (m_i) on input to the current pass.
     * 'ms_new' is the list output from the current pass. After each
     * pass we swap the arrays round.
     */
    { // WINSCP
    uint32_t *ms = snewn(n, uint32_t);
    uint32_t *msnew = snewn(n, uint32_t);
    { // WINSCP
    size_t i;
    for (i = 0; i < n; i++)
        ms[i] = ms_in[i];
    } // WINSCP

    while (n > 1) {
        size_t nnew = 0;
        { // WINSCP
        size_t i;
        for (i = 0; i < n; i += 2) {
            if (i+1 == n) {
                /*
                 * Odd element at the end of the input list: just copy
                 * it unchanged to the output.
                 */
                sched_append(sched, ENC_COPY);
                msnew[nnew++] = ms[i];
                break;
            }

            /*
             * Normal case: consume two elements from the input list
             * and combine them.
             */
            { // WINSCP
            uint32_t m1 = ms[i], m2 = ms[i+1], m = m1*m2;
            sched_append(sched, ENC_COMBINE_BASE + m1);

            /*
             * And then, as long as the combined limit is big enough,
             * emit an output byte from the bottom of it.
             */
            while (m >= (1<<14)) {
                sched_append(sched, ENC_BYTE);
                m = (m + 0xFF) >> 8;
            }

            /*
             * Whatever is left after that, we emit into the output
             * list and append to the fifo.
             */
            msnew[nnew++] = m;
            } // WINSCP
        }
        } // WINSCP

        /*
         * End of pass. The output list of (m_i) now becomes the input
         * list.
         */
        { // WINSCP
        uint32_t *tmp = ms;
        ms = msnew;
        n = nnew;
        msnew = tmp;
        } // WINSCP
    }

    /*
     * When that loop terminates, it's because there's exactly one
     * number left to encode. (Or, technically, _at most_ one - but we
     * don't support encoding a completely empty list in this
     * implementation, because what would be the point?) That number
     * is just emitted little-endian until its limit is 1 (meaning its
     * only possible actual value is 0).
     */
    assert(n == 1);
    { // WINSCP
    uint32_t m = ms[0];
    while (m > 1) {
        sched_append(sched, ENC_BYTE);
        m = (m + 0xFF) >> 8;
    }

    sfree(ms);
    sfree(msnew);

    return sched;
    } // WINSCP
    } // WINSCP
}

void ntru_encode_schedule_free(NTRUEncodeSchedule *sched)
{
    sfree(sched->ops);
    sfree(sched);
}

/*
 * Calculate the output length of the encoded data in bytes.
 */
size_t ntru_encode_schedule_length(NTRUEncodeSchedule *sched)
{
    size_t len = 0;
    { // WINSCP
    size_t i;
    for (i = 0; i < sched->nops; i++)
        if (sched->ops[i] == ENC_BYTE)
            len++;
    return len;
    } // WINSCP
}

/*
 * Retrieve the number of items encoded. (Used by testcrypt.)
 */
size_t ntru_encode_schedule_nvals(NTRUEncodeSchedule *sched)
{
    return sched->nvals;
}

/*
 * Actually encode a sequence of (r_i), emitting the output bytes to
 * an arbitrary BinarySink.
 */
void ntru_encode(NTRUEncodeSchedule *sched, const uint16_t *rs_in,
                 BinarySink *bs)
{
    size_t n = sched->nvals;
    uint32_t *rs = snewn(n, uint32_t);
    { // WINSCP
    size_t i;
    for (i = 0; i < n; i++)
        rs[i] = rs_in[i];
    } // WINSCP

    /*
     * The head and tail pointers of the queue are both 'full'. That
     * is, rs[head] is the first element actually in the queue, and
     * rs[tail] is the last element.
     *
     * So you append to the queue by first advancing 'tail' and then
     * writing to rs[tail], whereas you consume from the queue by
     * first reading rs[head] and _then_ advancing 'head'.
     *
     * The more normal thing would be to make 'tail' point to the
     * first empty slot instead of the last full one. But then you'd
     * have to faff about with modular arithmetic to find the last
     * full slot for the BYTE command, so in this case, it's easier to
     * do it the less usual way.
     */
    { // WINSCP
    size_t head = 0, tail = n-1;

    { // WINSCP
    size_t i;
    for (i = 0; i < sched->nops; i++) {
        uint16_t op = sched->ops[i];
        switch (op) {
          case ENC_BYTE:
            put_byte(bs, rs[tail] & 0xFF);
            rs[tail] >>= 8;
            break;
          case ENC_COPY: {
            uint32_t r = rs[head];
            head = (head + 1) % n;
            tail = (tail + 1) % n;
            rs[tail] = r;
            break;
          }
          default: {
            uint32_t r1 = rs[head];
            head = (head + 1) % n;
            { // WINSCP
            uint32_t r2 = rs[head];
            head = (head + 1) % n;
            tail = (tail + 1) % n;
            rs[tail] = r1 + (op - ENC_COMBINE_BASE) * r2;
            break;
            } // WINSCP
          }
        }
    }
    } // WINSCP

    /*
     * Expect that we've ended up with a single zero in the queue, at
     * exactly the position that the setup-time analysis predicted it.
     */
    assert(head == sched->endpos);
    assert(tail == sched->endpos);
    assert(rs[head] == 0);

    smemclr(rs, n * sizeof(*rs));
    sfree(rs);
    } // WINSCP
}

/*
 * Decode a ptrlen of binary data into a sequence of (r_i). The data
 * is expected to be of exactly the right length (on pain of assertion
 * failure).
 */
void ntru_decode(NTRUEncodeSchedule *sched, uint16_t *rs_out, ptrlen data)
{
    size_t n = sched->nvals;
    const uint8_t *base = (const uint8_t *)data.ptr;
    const uint8_t *pos = base + data.len;

    /*
     * Initialise the queue to a single zero, at the 'endpos' position
     * that will mean the final output is correctly aligned.
     *
     * 'head' and 'tail' have the same meanings as in encoding. So
     * 'tail' is the location that BYTE modifies and COPY and COMBINE
     * consume from, and 'head' is the location that COPY and COMBINE
     * push on to. As in encoding, they both point at the extremal
     * full slots in the array.
     */
    uint32_t *rs = snewn(n, uint32_t);
    size_t head = sched->endpos, tail = head;
    rs[tail] = 0;

    { // WINSCP
    size_t i;
    for (i = sched->nops; i-- > 0 ;) {
        uint16_t op = sched->ops[i];
        switch (op) {
          case ENC_BYTE: {
            assert(pos > base);
            { // WINSCP
            uint8_t byte = *--pos;
            rs[tail] = (rs[tail] << 8) | byte;
            break;
            } // WINSCP
          }
          case ENC_COPY: {
            uint32_t r = rs[tail];
            tail = (tail + n - 1) % n;
            head = (head + n - 1) % n;
            rs[head] = r;
            break;
          }
          default: {
            uint32_t r = rs[tail];
            tail = (tail + n - 1) % n;

            { // WINSCP
            uint32_t m = op - ENC_COMBINE_BASE;
            uint64_t mrecip = reciprocal_for_reduction(m);

            uint32_t r1, r2;
            r1 = reduce_with_quot(r, &r2, m, mrecip);

            head = (head + n - 1) % n;
            rs[head] = r2;
            head = (head + n - 1) % n;
            rs[head] = r1;
            break;
            } // WINSCP
          }
        }
    }
    } // WINSCP

    assert(pos == base);
    assert(head == 0);
    assert(tail == n-1);

    { // WINSCP
    size_t i;
    for (i = 0; i < n; i++)
        rs_out[i] = rs[i];
    } // WINSCP
    smemclr(rs, n * sizeof(*rs));
    sfree(rs);
}

/* ----------------------------------------------------------------------
 * The actual public-key cryptosystem.
 */

struct NTRUKeyPair {
    unsigned p, q, w;
    uint16_t *h;                       /* public key */
    uint16_t *f3, *ginv;               /* private key */
    uint16_t *rho;                     /* for implicit rejection */
};

/* Helper function to free an array of uint16_t containing a ring
 * element, clearing it on the way since some of them are sensitive. */
static void ring_free(uint16_t *val, unsigned p)
{
    smemclr(val, p*sizeof(*val));
    sfree(val);
}

void ntru_keypair_free(NTRUKeyPair *keypair)
{
    ring_free(keypair->h, keypair->p);
    ring_free(keypair->f3, keypair->p);
    ring_free(keypair->ginv, keypair->p);
    ring_free(keypair->rho, keypair->p);
    sfree(keypair);
}

/* Trivial accessors used by test programs. */
unsigned ntru_keypair_p(NTRUKeyPair *keypair) { return keypair->p; }
const uint16_t *ntru_pubkey(NTRUKeyPair *keypair) { return keypair->h; }

/*
 * Generate a value of the class DJB describes as 'Short': it consists
 * of p terms that are all either 0 or +1 or -1, and exactly w of them
 * are not zero.
 *
 * Values of this kind are used for several purposes: part of the
 * private key, a plaintext, and the 'rho' fake-plaintext value used
 * for deliberately returning a duff but non-revealing session hash if
 * things go wrong.
 *
 * -1 is represented as 2 in the output array. So if you want these
 * numbers mod 3, then they come out already in the right form.
 * Otherwise, use ntru_expand.
 */
void ntru_gen_short(uint16_t *v, unsigned p, unsigned w)
{
    /*
     * Get enough random data to generate a polynomial all of whose p
     * terms are in {0,+1,-1}, and exactly w of them are nonzero.
     * We'll do this by making up a completely random sequence of
     * {+1,-1} and then setting a random subset of them to 0.
     *
     * So we'll need p random bits to choose the nonzero values, and
     * then (doing it the simplest way) log2(p!) bits to shuffle them,
     * plus say 128 bits to ensure any fluctuations in uniformity are
     * negligible.
     *
     * log2(p!) is a pain to calculate, so we'll bound it above by
     * p*log2(p), which we bound in turn by p*16.
     */
    size_t randbitpos = 17 * p + 128;
    mp_int *randdata = mp_resize(mp_random_bits(randbitpos), randbitpos + 32);

    /*
     * Initial value before zeroing out some terms: p randomly chosen
     * values in {1,2}.
     */
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++)
        v[i] = 1 + mp_get_bit(randdata, --randbitpos);
    } // WINSCP

    /*
     * Hereafter we're going to extract random bits by multiplication,
     * treating randdata as a large fixed-point number.
     */
    mp_reduce_mod_2to(randdata, randbitpos);

    /*
     * Zero out some terms, leaving a randomly selected w of them
     * nonzero.
     */
    { // WINSCP
    uint32_t nonzeros_left = w;
    mp_int *x = mp_new(64);
    { // WINSCP
    size_t i;
    for (i = p; i-- > 0 ;) {
        /*
         * Pick a random number out of the number of terms remaning.
         */
        mp_mul_integer_into(randdata, randdata, i+1);
        mp_rshift_fixed_into(x, randdata, randbitpos);
        mp_reduce_mod_2to(randdata, randbitpos);
        { // WINSCP
        size_t j = mp_get_integer(x);

        /*
         * If that's less than nonzeros_left, then we're leaving this
         * number nonzero. Otherwise we're zeroing it out.
         */
        uint32_t keep = (uint32_t)(j - nonzeros_left) >> 31;
        v[i] &= (uint32_t)-(int32_t)keep;            /* clear this field if keep == 0 */ // WINSCP
        nonzeros_left -= keep;    /* decrement counter if keep == 1 */
        } // WINSCP
    }
    } // WINSCP

    mp_free(x);
    mp_free(randdata);
    } // WINSCP
}

/*
 * Make a single attempt at generating a key pair. This involves
 * inventing random elements of both our quotient rings and hoping
 * they're both invertible.
 *
 * They may not be, if you're unlucky. The element of Z_q/<x^p-x-1>
 * will _almost_ certainly be invertible, because that is a field, so
 * invertibility can only fail if you were so unlucky as to choose the
 * all-0s element. But the element of Z_3/<x^p-x-1> may fail to be
 * invertible because it has a common factor with x^p-x-1 (which, over
 * Z_3, is not irreducible).
 *
 * So we can't guarantee to generate a key pair in constant time,
 * because there's no predicting how many retries we'll need. However,
 * this isn't a failure of side-channel safety, because we completely
 * discard all the random numbers and state from each failed attempt.
 * So if there were a side-channel leakage from a failure, the only
 * thing it would give away would be a bunch of random numbers that
 * turned out not to be used anyway.
 *
 * But a _successful_ call to this function should execute in a
 * secret-independent manner, and this 'make a single attempt'
 * function is exposed in the API so that 'testsc' can check that.
 */
NTRUKeyPair *ntru_keygen_attempt(unsigned p, unsigned q, unsigned w)
{
    /*
     * First invent g, which is the one more likely to fail to invert.
     * This is simply a uniformly random polynomial with p terms over
     * Z_3. So we need p*log2(3) random bits for it, plus 128 for
     * uniformity. It's easiest to bound log2(3) above by 2.
     */
    size_t randbitpos = 2 * p + 128;
    mp_int *randdata = mp_resize(mp_random_bits(randbitpos), randbitpos + 32);

    /*
     * Select p random values from {0,1,2}.
     */
    uint16_t *g = snewn(p, uint16_t);
    mp_int *x = mp_new(64);
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++) {
        mp_mul_integer_into(randdata, randdata, 3);
        mp_rshift_fixed_into(x, randdata, randbitpos);
        mp_reduce_mod_2to(randdata, randbitpos);
        g[i] = mp_get_integer(x);
    }
    } // WINSCP
    mp_free(x);
    mp_free(randdata);

    /*
     * Try to invert g over Z_3, and fail if it isn't invertible.
     */
    { // WINSCP
    uint16_t *ginv = snewn(p, uint16_t);
    if (!ntru_ring_invert(ginv, g, p, 3)) {
        ring_free(g, p);
        ring_free(ginv, p);
        return NULL;
    }

    /*
     * Fine; we have g. Now make up an f, and convert it to a
     * polynomial over q.
     */
    { // WINSCP
    uint16_t *f = snewn(p, uint16_t);
    ntru_gen_short(f, p, w);
    ntru_expand(f, f, p, q);

    /*
     * Multiply f by 3.
     */
    { // WINSCP
    uint16_t *f3 = snewn(p, uint16_t);
    ntru_scale(f3, f, 3, p, q);

    /*
     * Invert 3*f over Z_q. This is guaranteed to succeed, since
     * Z_q/<x^p-x-1> is a field, so the only non-invertible value is
     * 0. And f is nonzero because it came from ntru_gen_short (hence,
     * w of its components are nonzero), hence so is 3*f.
     */
    { // WINSCP
    uint16_t *f3inv = snewn(p, uint16_t);
    bool expect_always_success = ntru_ring_invert(f3inv, f3, p, q);
    assert(expect_always_success);

    /*
     * Make the public key, by converting g to a polynomial over q and
     * then multiplying by f3inv.
     */
    { // WINSCP
    uint16_t *g_q = snewn(p, uint16_t);
    ntru_expand(g_q, g, p, q);
    { // WINSCP
    uint16_t *h = snewn(p, uint16_t);
    ntru_ring_multiply(h, g_q, f3inv, p, q);

    /*
     * Make up rho, used to substitute for the plaintext in the
     * session hash in case of confirmation failure.
     */
    { // WINSCP
    uint16_t *rho = snewn(p, uint16_t);
    ntru_gen_short(rho, p, w);

    /*
     * And we're done! Free everything except the pieces we're
     * returning.
     */
    { // WINSCP
    NTRUKeyPair *keypair = snew(NTRUKeyPair);
    keypair->p = p;
    keypair->q = q;
    keypair->w = w;
    keypair->h = h;
    keypair->f3 = f3;
    keypair->ginv = ginv;
    keypair->rho = rho;
    ring_free(f, p);
    ring_free(f3inv, p);
    ring_free(g, p);
    ring_free(g_q, p);
    return keypair;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/*
 * The top-level key generation function for real use (as opposed to
 * testsc): keep trying to make a key until you succeed.
 */
NTRUKeyPair *ntru_keygen(unsigned p, unsigned q, unsigned w)
{
    while (1) {
        NTRUKeyPair *keypair = ntru_keygen_attempt(p, q, w);
        if (keypair)
            return keypair;
    }
}

/*
 * Public-key encryption.
 */
void ntru_encrypt(uint16_t *ciphertext, const uint16_t *plaintext,
                  uint16_t *pubkey, unsigned p, unsigned q)
{
    uint16_t *r_q = snewn(p, uint16_t);
    ntru_expand(r_q, plaintext, p, q);

    { // WINSCP
    uint16_t *unrounded = snewn(p, uint16_t);
    ntru_ring_multiply(unrounded, r_q, pubkey, p, q);

    ntru_round3(ciphertext, unrounded, p, q);
    ntru_normalise(ciphertext, ciphertext, p, q);

    ring_free(r_q, p);
    ring_free(unrounded, p);
    } // WINSCP
}

/*
 * Public-key decryption.
 */
void ntru_decrypt(uint16_t *plaintext, const uint16_t *ciphertext,
                  NTRUKeyPair *keypair)
{
    unsigned p = keypair->p, q = keypair->q, w = keypair->w;
    uint16_t *tmp = snewn(p, uint16_t);

    ntru_ring_multiply(tmp, ciphertext, keypair->f3, p, q);

    ntru_mod3(tmp, tmp, p, q);
    ntru_normalise(tmp, tmp, p, 3);

    ntru_ring_multiply(plaintext, tmp, keypair->ginv, p, 3);
    ring_free(tmp, p);

    /*
     * With luck, this should have recovered exactly the original
     * plaintext. But, as per the spec, we check whether it has
     * exactly w nonzero coefficients, and if not, then something has
     * gone wrong - and in that situation we time-safely substitute a
     * different output.
     *
     * (I don't know exactly why we do this, but I assume it's because
     * otherwise the mis-decoded output could be made to disgorge a
     * secret about the private key in some way.)
     */

    { // WINSCP
    unsigned weight = p;
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++)
        weight -= iszero(plaintext[i]);
    } // WINSCP
    { // WINSCP
    unsigned ok = iszero(weight ^ w);

    /*
     * The default failure return value consists of w 1s followed by
     * 0s.
     */
    unsigned mask = ok - 1;
    { // WINSCP
    size_t i;
    for (i = 0; i < w; i++) {
        uint16_t diff = (1 ^ plaintext[i]) & mask;
        plaintext[i] ^= diff;
    }
    } // WINSCP
    { // WINSCP
    size_t i;
    for (i = w; i < p; i++) {
        uint16_t diff = (0 ^ plaintext[i]) & mask;
        plaintext[i] ^= diff;
    }
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Encode and decode public keys, ciphertexts and plaintexts.
 *
 * Public keys and ciphertexts use the complicated binary encoding
 * system implemented above. In both cases, the inputs are regarded as
 * symmetric about zero, and are first biased to map their most
 * negative permitted value to 0, so that they become non-negative and
 * hence suitable as inputs to the encoding system. In the case of a
 * ciphertext, where the input coefficients have also been coerced to
 * be multiples of 3, we divide by 3 as well, saving space by reducing
 * the upper bounds (m_i) on all the encoded numbers.
 */

/*
 * Compute the encoding schedule for a public key.
 */
static NTRUEncodeSchedule *ntru_encode_pubkey_schedule(unsigned p, unsigned q)
{
    uint16_t *ms = snewn(p, uint16_t);
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++)
        ms[i] = q;
    } // WINSCP
    { // WINSCP
    NTRUEncodeSchedule *sched = ntru_encode_schedule(ms, p);
    sfree(ms);
    return sched;
    } // WINSCP
}

/*
 * Encode a public key.
 */
void ntru_encode_pubkey(const uint16_t *pubkey, unsigned p, unsigned q,
                        BinarySink *bs)
{
    /* Compute the biased version for encoding */
    uint16_t *biased_pubkey = snewn(p, uint16_t);
    ntru_bias(biased_pubkey, pubkey, q / 2, p, q);

    /* Encode it */
    { // WINSCP
    NTRUEncodeSchedule *sched = ntru_encode_pubkey_schedule(p, q);
    ntru_encode(sched, biased_pubkey, bs);
    ntru_encode_schedule_free(sched);

    ring_free(biased_pubkey, p);
    } // WINSCP
}

/*
 * Decode a public key and write it into 'pubkey'. We also return a
 * ptrlen pointing at the chunk of data we removed from the
 * BinarySource.
 */
ptrlen ntru_decode_pubkey(uint16_t *pubkey, unsigned p, unsigned q,
                          BinarySource *src)
{
    NTRUEncodeSchedule *sched = ntru_encode_pubkey_schedule(p, q);

    /* Retrieve the right number of bytes from the source */
    size_t len = ntru_encode_schedule_length(sched);
    ptrlen encoded = get_data(src, len);
    if (get_err(src)) {
        /* If there wasn't enough data, give up and return all-zeroes
         * purely for determinism. But that value should never be
         * used, because the caller will also check get_err(src). */
        memset(pubkey, 0, p*sizeof(*pubkey));
    } else {
        /* Do the decoding */
        ntru_decode(sched, pubkey, encoded);

        /* Unbias the coefficients */
        ntru_bias(pubkey, pubkey, q-q/2, p, q);
    }

    ntru_encode_schedule_free(sched);
    return encoded;
}

/*
 * For ciphertext biasing: work out the largest absolute value a
 * ciphertext element can take, which is given by taking q/2 and
 * rounding it to the nearest multiple of 3.
 */
static inline unsigned ciphertext_bias(unsigned q)
{
    return (q/2+1) / 3;
}

/*
 * The number of possible values of a ciphertext coefficient (for use
 * as the m_i in encoding) ranges from +ciphertext_bias(q) to
 * -ciphertext_bias(q) inclusive.
 */
static inline unsigned ciphertext_m(unsigned q)
{
    return 1 + 2 * ciphertext_bias(q);
}

/*
 * Compute the encoding schedule for a ciphertext.
 */
static NTRUEncodeSchedule *ntru_encode_ciphertext_schedule(
    unsigned p, unsigned q)
{
    unsigned m = ciphertext_m(q);
    uint16_t *ms = snewn(p, uint16_t);
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++)
        ms[i] = m;
    } // WINSCP
    { // WINSCP
    NTRUEncodeSchedule *sched = ntru_encode_schedule(ms, p);
    sfree(ms);
    return sched;
    } // WINSCP
}

/*
 * Encode a ciphertext.
 */
void ntru_encode_ciphertext(const uint16_t *ciphertext, unsigned p, unsigned q,
                            BinarySink *bs)
{
    SETUP;

    /*
     * Bias the ciphertext, and scale down by 1/3, which we do by
     * modular multiplication by the inverse of 3 mod q. (That only
     * works if we know the inputs are all _exact_ multiples of 3
     * - but we do!)
     */
    uint16_t *biased_ciphertext = snewn(p, uint16_t);
    ntru_bias(biased_ciphertext, ciphertext, 3 * ciphertext_bias(q), p, q);
    ntru_scale(biased_ciphertext, biased_ciphertext, INVERT(3), p, q);

    /* Encode. */
    { // WINSCP
    NTRUEncodeSchedule *sched = ntru_encode_ciphertext_schedule(p, q);
    ntru_encode(sched, biased_ciphertext, bs);
    ntru_encode_schedule_free(sched);

    ring_free(biased_ciphertext, p);
    } // WINSCP
}

ptrlen ntru_decode_ciphertext(uint16_t *ct, NTRUKeyPair *keypair,
                              BinarySource *src)
{
    unsigned p = keypair->p, q = keypair->q;

    NTRUEncodeSchedule *sched = ntru_encode_ciphertext_schedule(p, q);

    /* Retrieve the right number of bytes from the source */
    size_t len = ntru_encode_schedule_length(sched);
    ptrlen encoded = get_data(src, len);
    if (get_err(src)) {
        /* As above, return deterministic nonsense on failure */
        memset(ct, 0, p*sizeof(*ct));
    } else {
        /* Do the decoding */
        ntru_decode(sched, ct, encoded);

        /* Undo the scaling and bias */
        ntru_scale(ct, ct, 3, p, q);
        ntru_bias(ct, ct, q - 3 * ciphertext_bias(q), p, q);
    }

    ntru_encode_schedule_free(sched);
    return encoded;        /* also useful to the caller, optionally */
}

/*
 * Encode a plaintext.
 *
 * This is a much simpler encoding than the NTRUEncodeSchedule system:
 * since elements of a plaintext are mod 3, we just encode each one in
 * 2 bits, applying the usual bias so that {-1,0,+1} map to {0,1,2}
 * respectively.
 *
 * There's no corresponding decode function, because plaintexts are
 * never transmitted on the wire (the whole point is that they're too
 * secret!). Plaintexts are only encoded in order to put them into
 * hash preimages.
 */
void ntru_encode_plaintext(const uint16_t *plaintext, unsigned p,
                           BinarySink *bs)
{
    unsigned byte = 0, bitpos = 0;
    { // WINSCP
    size_t i;
    for (i = 0; i < p; i++) {
        unsigned encoding = (plaintext[i] + 1) * iszero(plaintext[i] >> 1);
        byte |= encoding << bitpos;
        bitpos += 2;
        if (bitpos == 8 || i+1 == p) {
            put_byte(bs, byte);
            byte = 0;
            bitpos = 0;
        }
    }
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Compute the hashes required by the key exchange layer of NTRU Prime.
 *
 * There are two of these. The 'confirmation hash' is sent by the
 * server along with the ciphertext, and the client can recalculate it
 * to check whether the ciphertext was decrypted correctly. Then, the
 * 'session hash' is the actual output of key exchange, and if the
 * confirmation hash doesn't match, it gets deliberately corrupted.
 */

/*
 * Make the confirmation hash, whose inputs are the plaintext and the
 * public key.
 *
 * This is defined as H(2 || H(3 || r) || H(4 || K)), where r is the
 * plaintext and K is the public key (as encoded by the above
 * functions), and the constants 2,3,4 are single bytes. The choice of
 * hash function (H itself) is SHA-512 truncated to 256 bits.
 *
 * (To be clear: that is _not_ the thing that FIPS 180-4 6.7 defines
 * as "SHA-512/256", which varies the initialisation vector of the
 * SHA-512 algorithm as well as truncating the output. _This_
 * algorithm uses the standard SHA-512 IV, and _just_ truncates the
 * output, in the manner suggested by FIPS 180-4 section 7.)
 *
 * 'out' should therefore expect to receive 32 bytes of data.
 */
static void ntru_confirmation_hash(
    uint8_t *out, const uint16_t *plaintext,
    const uint16_t *pubkey, unsigned p, unsigned q)
{
    /* The outer hash object */
    ssh_hash *hconfirm = ssh_hash_new(&ssh_sha512);
    put_byte(hconfirm, 2);             /* initial byte 2 */

    { // WINSCP
    uint8_t hashdata[64];

    /* Compute H(3 || r) and add it to the main hash */
    ssh_hash *h3r = ssh_hash_new(&ssh_sha512);
    put_byte(h3r, 3);
    ntru_encode_plaintext(plaintext, p, BinarySink_UPCAST(h3r));
    ssh_hash_final(h3r, hashdata);
    put_data(hconfirm, hashdata, 32);

    /* Compute H(4 || K) and add it to the main hash */
    { // WINSCP
    ssh_hash *h4K = ssh_hash_new(&ssh_sha512);
    put_byte(h4K, 4);
    ntru_encode_pubkey(pubkey, p, q, BinarySink_UPCAST(h4K));
    ssh_hash_final(h4K, hashdata);
    put_data(hconfirm, hashdata, 32);

    /* Compute the full output of the main SHA-512 hash */
    ssh_hash_final(hconfirm, hashdata);

    /* And copy the first 32 bytes into the caller's output array */
    memcpy(out, hashdata, 32);
    smemclr(hashdata, sizeof(hashdata));
    } // WINSCP
    } // WINSCP
}

/*
 * Make the session hash, whose inputs are the plaintext, the
 * ciphertext, and the confirmation hash (hence, transitively, a
 * dependence on the public key as well).
 *
 * As computed by the server, and by the client if the confirmation
 * hash matched, this is defined as
 *
 *   H(1 || H(3 || r) || ciphertext || confirmation hash)
 *
 * but if the confirmation hash _didn't_ match, then the plaintext r
 * is replaced with the dummy plaintext-shaped value 'rho' we invented
 * during key generation (presumably to avoid leaking any information
 * about our secrets), and the initial byte 1 is replaced with 0 (to
 * ensure that the resulting hash preimage can't match any legitimate
 * preimage). So in that case, you instead get
 *
 *   H(0 || H(3 || rho) || ciphertext || confirmation hash)
 *
 * The inputs to this function include 'ok', which is the value to use
 * as the initial byte (1 on success, 0 on failure), and 'plaintext'
 * which should already have been substituted with rho in case of
 * failure.
 *
 * The ciphertext is provided in already-encoded form.
 */
static void ntru_session_hash(
    uint8_t *out, unsigned ok, const uint16_t *plaintext,
    unsigned p, ptrlen ciphertext, ptrlen confirmation_hash)
{
    /* The outer hash object */
    ssh_hash *hsession = ssh_hash_new(&ssh_sha512);
    put_byte(hsession, ok);            /* initial byte 1 or 0 */

    { // WINSCP
    uint8_t hashdata[64];

    /* Compute H(3 || r), or maybe H(3 || rho), and add it to the main hash */
    ssh_hash *h3r = ssh_hash_new(&ssh_sha512);
    put_byte(h3r, 3);
    ntru_encode_plaintext(plaintext, p, BinarySink_UPCAST(h3r));
    ssh_hash_final(h3r, hashdata);
    put_data(hsession, hashdata, 32);

    /* Put the ciphertext and confirmation hash in */
    put_datapl(hsession, ciphertext);
    put_datapl(hsession, confirmation_hash);

    /* Compute the full output of the main SHA-512 hash */
    ssh_hash_final(hsession, hashdata);

    /* And copy the first 32 bytes into the caller's output array */
    memcpy(out, hashdata, 32);
    smemclr(hashdata, sizeof(hashdata));
    } // WINSCP
}

/* ----------------------------------------------------------------------
 * Top-level KEM functions.
 */

/*
 * The parameters p,q,w for the system. There are other choices of
 * these, but OpenSSH only specifies this set. (If that ever changes,
 * we'll need to turn these into elements of the state structures.)
 */
#define p_LIVE 761
#define q_LIVE 4591
#define w_LIVE 286

struct ntru_dk {
    NTRUKeyPair *keypair;
    strbuf *encoded;
    pq_kem_dk dk;
};

static pq_kem_dk *ntru_vt_keygen(const pq_kemalg *alg, BinarySink *ek)
{
    struct ntru_dk *ndk = snew(struct ntru_dk);
    ndk->dk.vt = alg;
    ndk->encoded = strbuf_new_nm();
    ndk->keypair = ntru_keygen(p_LIVE, q_LIVE, w_LIVE);
    ntru_encode_pubkey(ndk->keypair->h, p_LIVE, q_LIVE, ek);
    return &ndk->dk;
}

static bool ntru_vt_encaps(const pq_kemalg *alg, BinarySink *c, BinarySink *k,
                           ptrlen ek)
{
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, ek);

    { // WINSCP
    uint16_t *pubkey = snewn(p_LIVE, uint16_t);
    ntru_decode_pubkey(pubkey, p_LIVE, q_LIVE, src);

    if (get_err(src) || get_avail(src)) {
        /* Hard-fail if the input wasn't exactly the right length */ 
        ring_free(pubkey, p_LIVE);
        return false;
    }

    /* Invent a valid NTRU plaintext. */
    { // WINSCP
    uint16_t *plaintext = snewn(p_LIVE, uint16_t);
    ntru_gen_short(plaintext, p_LIVE, w_LIVE);

    /* Encrypt the plaintext, and encode the ciphertext into a strbuf,
     * so we can reuse it for both the session hash and sending to the
     * client. */
    { // WINSCP
    uint16_t *ciphertext = snewn(p_LIVE, uint16_t);
    ntru_encrypt(ciphertext, plaintext, pubkey, p_LIVE, q_LIVE);
    { // WINSCP
    strbuf *ciphertext_encoded = strbuf_new_nm();
    ntru_encode_ciphertext(ciphertext, p_LIVE, q_LIVE,
                           BinarySink_UPCAST(ciphertext_encoded));
    put_datapl(c, ptrlen_from_strbuf(ciphertext_encoded));

    /* Compute the confirmation hash, and append that to the data sent
     * to the other side. */
    { // WINSCP
    uint8_t confhash[32];
    ntru_confirmation_hash(confhash, plaintext, pubkey, p_LIVE, q_LIVE);
    put_data(c, confhash, 32);

    /* Compute the session hash, i.e. the output shared secret. */
    { // WINSCP
    uint8_t sesshash[32];
    ntru_session_hash(sesshash, 1, plaintext, p_LIVE,
                      ptrlen_from_strbuf(ciphertext_encoded),
                      make_ptrlen(confhash, 32));
    put_data(k, sesshash, 32);

    ring_free(pubkey, p_LIVE);
    ring_free(plaintext, p_LIVE);
    ring_free(ciphertext, p_LIVE);
    strbuf_free(ciphertext_encoded);
    smemclr(confhash, sizeof(confhash));
    smemclr(sesshash, sizeof(sesshash));

    return true;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static bool ntru_vt_decaps(pq_kem_dk *dk, BinarySink *k, ptrlen c)
{
    struct ntru_dk *ndk = container_of(dk, struct ntru_dk, dk);

    /* Expect a string containing a ciphertext and a confirmation hash. */
    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, c);

    { // WINSCP
    uint16_t *ciphertext = snewn(p_LIVE, uint16_t);
    ptrlen ciphertext_encoded = ntru_decode_ciphertext(
        ciphertext, ndk->keypair, src);
    ptrlen confirmation_hash = get_data(src, 32);

    if (get_err(src) || get_avail(src)) {
        /* Hard-fail if the input wasn't exactly the right length */
        ring_free(ciphertext, p_LIVE);
        return false;
    }

    /* Decrypt the ciphertext to recover the sender's plaintext */
    { // WINSCP
    uint16_t *plaintext = snewn(p_LIVE, uint16_t);
    ntru_decrypt(plaintext, ciphertext, ndk->keypair);

    /* Make the confirmation hash */
    { // WINSCP
    uint8_t confhash[32];
    ntru_confirmation_hash(confhash, plaintext, ndk->keypair->h,
                           p_LIVE, q_LIVE);

    /* Check it matches the one the server sent */
    { // WINSCP
    unsigned ok = smemeq(confhash, confirmation_hash.ptr, 32);

    /* If not, substitute in rho for the plaintext in the session hash */
    unsigned mask = ok-1;
    size_t i; // WINSCP
    for (i = 0; i < p_LIVE; i++)
        plaintext[i] ^= mask & (plaintext[i] ^ ndk->keypair->rho[i]);

    /* Compute the session hash, whether or not we did that */
    { // WINSCP
    uint8_t sesshash[32];
    ntru_session_hash(sesshash, ok, plaintext, p_LIVE, ciphertext_encoded,
                      confirmation_hash);
    put_data(k, sesshash, 32);

    ring_free(plaintext, p_LIVE);
    ring_free(ciphertext, p_LIVE);
    smemclr(confhash, sizeof(confhash));
    smemclr(sesshash, sizeof(sesshash));

    return true;
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
    } // WINSCP
}

static void ntru_vt_free_dk(pq_kem_dk *dk)
{
    struct ntru_dk *ndk = container_of(dk, struct ntru_dk, dk);
    strbuf_free(ndk->encoded);
    ntru_keypair_free(ndk->keypair);
    sfree(ndk);
}

const pq_kemalg ssh_ntru = {
    /*.keygen =*/ ntru_vt_keygen,
    /*.encaps =*/ ntru_vt_encaps,
    /*.decaps =*/ ntru_vt_decaps,
    /*.free_dk =*/ ntru_vt_free_dk,
    NULL, // WINSCP
    /*.description =*/ "NTRU Prime",
    /*.ek_len =*/ 1158,
    /*.c_len =*/ 1039,
};
