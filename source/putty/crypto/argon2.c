/*
 * Implementation of the Argon2 password hash function.
 *
 * My sources for the algorithm description and test vectors (the latter in
 * test/cryptsuite.py) were the reference implementation on Github, and also
 * the Internet-Draft description:
 *
 *   https://github.com/P-H-C/phc-winner-argon2
 *   https://datatracker.ietf.org/doc/html/draft-irtf-cfrg-argon2-13
 */

#include <assert.h>

#include "putty.h"
#include "ssh.h"
#include "marshal.h"

/* ----------------------------------------------------------------------
 * Argon2 uses data marshalling rules similar to SSH but with 32-bit integers
 * stored little-endian. Start with some local BinarySink routines for storing
 * a uint32 and a string in that fashion.
 */

static void BinarySink_put_uint32_le(BinarySink *bs, unsigned long val)
{
    unsigned char data[4];
    PUT_32BIT_LSB_FIRST(data, val);
    bs->write(bs, data, sizeof(data));
}

static void BinarySink_put_stringpl_le(BinarySink *bs, ptrlen pl)
{
    /* Check that the string length fits in a uint32, without doing a
     * potentially implementation-defined shift of more than 31 bits */
    assert((pl.len >> 31) < 2);

    BinarySink_put_uint32_le(bs, pl.len);
    bs->write(bs, pl.ptr, pl.len);
}

#define put_uint32_le(bs, val) \
    BinarySink_put_uint32_le(BinarySink_UPCAST(bs), val)
#define put_stringpl_le(bs, val) \
    BinarySink_put_stringpl_le(BinarySink_UPCAST(bs), val)

/* ----------------------------------------------------------------------
 * Argon2 defines a hash-function family that's an extension of BLAKE2b to
 * generate longer output digests, by repeatedly outputting half of a BLAKE2
 * hash output and then re-hashing the whole thing until there are 64 or fewer
 * bytes left to output. The spec calls this H' (a variant of the original
 * hash it calls H, which is the unmodified BLAKE2b).
 */

static ssh_hash *hprime_new(unsigned length)
{
    ssh_hash *h = blake2b_new_general(length > 64 ? 64 : length);
    put_uint32_le(h, length);
    return h;
}

static void hprime_final(ssh_hash *h, unsigned length, void *vout)
{
    uint8_t *out = (uint8_t *)vout;

    while (length > 64) {
        uint8_t hashbuf[64];
        ssh_hash_final(h, hashbuf);

        memcpy(out, hashbuf, 32);
        out += 32;
        length -= 32;

        h = blake2b_new_general(length > 64 ? 64 : length);
        put_data(h, hashbuf, 64);

        smemclr(hashbuf, sizeof(hashbuf));
    }

    ssh_hash_final(h, out);
}

/* Externally visible entry point for the long hash function. This is only
 * used by testcrypt, so it would be overkill to set it up like a proper
 * ssh_hash. */
strbuf *argon2_long_hash(unsigned length, ptrlen data)
{
    ssh_hash *h = hprime_new(length);
    put_datapl(h, data);
    strbuf *out = strbuf_new();
    hprime_final(h, length, strbuf_append(out, length));
    return out;
}

/* ----------------------------------------------------------------------
 * Argon2's own mixing function G, which operates on 1Kb blocks of data.
 *
 * The definition of G in the spec takes two 1Kb blocks as input and produces
 * a 1Kb output block. The first thing that happens to the input blocks is
 * that they get XORed together, and then only the XOR output is used, so you
 * could perfectly well regard G as a 1Kb->1Kb function.
 */

static inline uint64_t ror(uint64_t x, unsigned rotation)
{
    unsigned lshift = 63 & -rotation, rshift = 63 & rotation;
    return (x << lshift) | (x >> rshift);
}

static inline uint64_t trunc32(uint64_t x)
{
    return x & 0xFFFFFFFF;
}

/* Internal function similar to the BLAKE2b round, which mixes up four 64-bit
 * words */
static inline void GB(uint64_t *a, uint64_t *b, uint64_t *c, uint64_t *d)
{
    *a += *b + 2 * trunc32(*a) * trunc32(*b);
    *d = ror(*d ^ *a, 32);
    *c += *d + 2 * trunc32(*c) * trunc32(*d);
    *b = ror(*b ^ *c, 24);
    *a += *b + 2 * trunc32(*a) * trunc32(*b);
    *d = ror(*d ^ *a, 16);
    *c += *d + 2 * trunc32(*c) * trunc32(*d);
    *b = ror(*b ^ *c, 63);
}

/* Higher-level internal function which mixes up sixteen 64-bit words. This is
 * applied to different subsets of the 128 words in a kilobyte block, and the
 * API here is designed to make it easy to apply in the circumstances the spec
 * requires. In every call, the sixteen words form eight pairs adjacent in
 * memory, whose addresses are in arithmetic progression. So the 16 input
 * words are in[0], in[1], in[instep], in[instep+1], ..., in[7*instep],
 * in[7*instep+1], and the 16 output words similarly. */
static inline void P(uint64_t *out, unsigned outstep,
                     uint64_t *in, unsigned instep)
{
    for (unsigned i = 0; i < 8; i++) {
        out[i*outstep] = in[i*instep];
        out[i*outstep+1] = in[i*instep+1];
    }

    GB(out+0*outstep+0, out+2*outstep+0, out+4*outstep+0, out+6*outstep+0);
    GB(out+0*outstep+1, out+2*outstep+1, out+4*outstep+1, out+6*outstep+1);
    GB(out+1*outstep+0, out+3*outstep+0, out+5*outstep+0, out+7*outstep+0);
    GB(out+1*outstep+1, out+3*outstep+1, out+5*outstep+1, out+7*outstep+1);

    GB(out+0*outstep+0, out+2*outstep+1, out+5*outstep+0, out+7*outstep+1);
    GB(out+0*outstep+1, out+3*outstep+0, out+5*outstep+1, out+6*outstep+0);
    GB(out+1*outstep+0, out+3*outstep+1, out+4*outstep+0, out+6*outstep+1);
    GB(out+1*outstep+1, out+2*outstep+0, out+4*outstep+1, out+7*outstep+0);
}

/* The full G function, taking input blocks X and Y. The result of G is most
 * often XORed into an existing output block, so this API is designed with
 * that in mind: the mixing function's output is always XORed into whatever
 * 1Kb of data is already at 'out'. */
static void G_xor(uint8_t *out, const uint8_t *X, const uint8_t *Y)
{
    uint64_t R[128], Q[128], Z[128];

    for (unsigned i = 0; i < 128; i++)
        R[i] = GET_64BIT_LSB_FIRST(X + 8*i) ^ GET_64BIT_LSB_FIRST(Y + 8*i);

    for (unsigned i = 0; i < 8; i++)
        P(Q+16*i, 2, R+16*i, 2);

    for (unsigned i = 0; i < 8; i++)
        P(Z+2*i, 16, Q+2*i, 16);

    for (unsigned i = 0; i < 128; i++)
        PUT_64BIT_LSB_FIRST(out + 8*i,
                            GET_64BIT_LSB_FIRST(out + 8*i) ^ R[i] ^ Z[i]);

    smemclr(R, sizeof(R));
    smemclr(Q, sizeof(Q));
    smemclr(Z, sizeof(Z));
}

/* ----------------------------------------------------------------------
 * The main Argon2 function.
 */

static void argon2_internal(uint32_t p, uint32_t T, uint32_t m, uint32_t t,
                            uint32_t y, ptrlen P, ptrlen S, ptrlen K, ptrlen X,
                            uint8_t *out)
{
    /*
     * Start by hashing all the input data together: the four string arguments
     * (password P, salt S, optional secret key K, optional associated data
     * X), plus all the parameters for the function's memory and time usage.
     *
     * The output of this hash is the sole input to the subsequent mixing
     * step: Argon2 does not preserve any more entropy from the inputs, it
     * just makes it extra painful to get the final answer.
     */
    uint8_t h0[64];
    {
        ssh_hash *h = blake2b_new_general(64);
        put_uint32_le(h, p);
        put_uint32_le(h, T);
        put_uint32_le(h, m);
        put_uint32_le(h, t);
        put_uint32_le(h, 0x13);        /* hash function version number */
        put_uint32_le(h, y);
        put_stringpl_le(h, P);
        put_stringpl_le(h, S);
        put_stringpl_le(h, K);
        put_stringpl_le(h, X);
        ssh_hash_final(h, h0);
    }

    struct blk { uint8_t data[1024]; };

    /*
     * Array of 1Kb blocks. The total size is (approximately) m, the
     * caller-specified parameter for how much memory to use; the blocks are
     * regarded as a rectangular array of p rows ('lanes') by q columns, where
     * p is the 'parallelism' input parameter (the lanes can be processed
     * concurrently up to a point) and q is whatever makes the product pq come
     * to m.
     *
     * Additionally, each row is divided into four equal 'segments', which are
     * important to the way the algorithm decides which blocks to use as input
     * to each step of the function.
     *
     * The term 'slice' refers to a whole set of vertically aligned segments,
     * i.e. slice 0 is the whole left quarter of the array, and slice 3 the
     * whole right quarter.
     */
    size_t SL = m / (4*p); /* segment length: # of 1Kb blocks in a segment */
    size_t q = 4 * SL;     /* width of the array: 4 segments times SL */
    size_t mprime = q * p; /* total size of the array, approximately m */

    /* Allocate the memory. */
    struct blk *B = snewn(mprime, struct blk);
    memset(B, 0, mprime * sizeof(struct blk));

    /*
     * Initial setup: fill the first two full columns of the array with data
     * expanded from the starting hash h0. Each block is the result of using
     * the long-output hash function H' to hash h0 itself plus the block's
     * coordinates in the array.
     */
    for (size_t i = 0; i < p; i++) {
        ssh_hash *h = hprime_new(1024);
        put_data(h, h0, 64);
        put_uint32_le(h, 0);
        put_uint32_le(h, i);
        hprime_final(h, 1024, B[i].data);
    }
    for (size_t i = 0; i < p; i++) {
        ssh_hash *h = hprime_new(1024);
        put_data(h, h0, 64);
        put_uint32_le(h, 1);
        put_uint32_le(h, i);
        hprime_final(h, 1024, B[i+p].data);
    }

    /*
     * Declarations for the main loop.
     *
     * The basic structure of the main loop is going to involve processing the
     * array one whole slice (vertically divided quarter) at a time. Usually
     * we'll write a new value into every single block in the slice, except
     * that in the initial slice on the first pass, we've already written
     * values into the first two columns during the initial setup above. So
     * 'jstart' indicates the starting index in each segment we process; it
     * starts off as 2 so that we don't overwrite the initial setup, and then
     * after the first slice is done, we set it to 0, and it stays there.
     *
     * d_mode indicates whether we're being data-dependent (true) or
     * data-independent (false). In the hybrid Argon2id mode, we start off
     * independent, and then once we've mixed things up enough, switch over to
     * dependent mode to force long serial chains of computation.
     */
    size_t jstart = 2;
    bool d_mode = (y == 0);
    struct blk out2i, tmp2i, in2i;

    /* Outermost loop: t whole passes from left to right over the array */
    for (size_t pass = 0; pass < t; pass++) {

        /* Within that, we process the array in its four main slices */
        for (unsigned slice = 0; slice < 4; slice++) {

            /* In Argon2id mode, if we're half way through the first pass,
             * this is the moment to switch d_mode from false to true */
            if (pass == 0 && slice == 2 && y == 2)
                d_mode = true;

            /* Loop over every segment in the slice (i.e. every row). So i is
             * the y-coordinate of each block we process. */
            for (size_t i = 0; i < p; i++) {

                /* And within that segment, process the blocks from left to
                 * right, starting at 'jstart' (usually 0, but 2 in the first
                 * slice). */
                for (size_t jpre = jstart; jpre < SL; jpre++) {

                    /* j is the x-coordinate of each block we process, made up
                     * of the slice number and the index 'jpre' within the
                     * segment. */
                    size_t j = slice * SL + jpre;

                    /* jm1 is j-1 (mod q) */
                    uint32_t jm1 = (j == 0 ? q-1 : j-1);

                    /*
                     * Construct two 32-bit pseudorandom integers J1 and J2.
                     * This is the part of the algorithm that varies between
                     * the data-dependent and independent modes.
                     */
                    uint32_t J1, J2;
                    if (d_mode) {
                        /*
                         * Data-dependent: grab the first 64 bits of the block
                         * to the left of this one.
                         */
                        J1 = GET_32BIT_LSB_FIRST(B[i + p * jm1].data);
                        J2 = GET_32BIT_LSB_FIRST(B[i + p * jm1].data + 4);
                    } else {
                        /*
                         * Data-independent: generate pseudorandom data by
                         * hashing a sequence of preimage blocks that include
                         * all our input parameters, plus the coordinates of
                         * this point in the algorithm (array position and
                         * pass number) to make all the hash outputs distinct.
                         *
                         * The hash we use is G itself, applied twice. So we
                         * generate 1Kb of data at a time, which is enough for
                         * 128 (J1,J2) pairs. Hence we only need to do the
                         * hashing if our index within the segment is a
                         * multiple of 128, or if we're at the very start of
                         * the algorithm (in which case we started at 2 rather
                         * than 0). After that we can just keep picking data
                         * out of our most recent hash output.
                         */
                        if (jpre == jstart || jpre % 128 == 0) {
                            /*
                             * Hash preimage is mostly zeroes, with a
                             * collection of assorted integer values we had
                             * anyway.
                             */
                            memset(in2i.data, 0, sizeof(in2i.data));
                            PUT_64BIT_LSB_FIRST(in2i.data +  0, pass);
                            PUT_64BIT_LSB_FIRST(in2i.data +  8, i);
                            PUT_64BIT_LSB_FIRST(in2i.data + 16, slice);
                            PUT_64BIT_LSB_FIRST(in2i.data + 24, mprime);
                            PUT_64BIT_LSB_FIRST(in2i.data + 32, t);
                            PUT_64BIT_LSB_FIRST(in2i.data + 40, y);
                            PUT_64BIT_LSB_FIRST(in2i.data + 48, jpre / 128 + 1);

                            /*
                             * Now apply G twice to generate the hash output
                             * in out2i.
                             */
                            memset(tmp2i.data, 0, sizeof(tmp2i.data));
                            G_xor(tmp2i.data, tmp2i.data, in2i.data);
                            memset(out2i.data, 0, sizeof(out2i.data));
                            G_xor(out2i.data, out2i.data, tmp2i.data);
                        }

                        /*
                         * Extract J1 and J2 from the most recent hash output
                         * (whether we've just computed it or not).
                         */
                        J1 = GET_32BIT_LSB_FIRST(
                            out2i.data + 8 * (jpre % 128));
                        J2 = GET_32BIT_LSB_FIRST(
                            out2i.data + 8 * (jpre % 128) + 4);
                    }

                    /*
                     * Now convert J1 and J2 into the index of an existing
                     * block of the array to use as input to this step. This
                     * is fairly fiddly.
                     *
                     * The easy part: the y-coordinate of the input block is
                     * obtained by reducing J2 mod p, except that at the very
                     * start of the algorithm (processing the first slice on
                     * the first pass) we simply use the same y-coordinate as
                     * our output block.
                     *
                     * Note that it's safe to use the ordinary % operator
                     * here, without any concern for timing side channels: in
                     * data-independent mode J2 is not correlated to any
                     * secrets, and in data-dependent mode we're going to be
                     * giving away side-channel data _anyway_ when we use it
                     * as an array index (and by assumption we don't care,
                     * because it's already massively randomised from the real
                     * inputs).
                     */
                    uint32_t index_l = (pass == 0 && slice == 0) ? i : J2 % p;

                    /*
                     * The hard part: which block in this array row do we use?
                     *
                     * First, we decide what the possible candidates are. This
                     * requires some case analysis, and depends on whether the
                     * array row is the same one we're writing into or not.
                     *
                     * If it's not the same row: we can't use any block from
                     * the current slice (because the segments within a slice
                     * have to be processable in parallel, so in a concurrent
                     * implementation those blocks are potentially in the
                     * process of being overwritten by other threads). But the
                     * other three slices are fair game, except that in the
                     * first pass, slices to the right of us won't have had
                     * any values written into them yet at all.
                     *
                     * If it is the same row, we _are_ allowed to use blocks
                     * from the current slice, but only the ones before our
                     * current position.
                     *
                     * In both cases, we also exclude the individual _column_
                     * just to the left of the current one. (The block
                     * immediately to our left is going to be the _other_
                     * input to G, but the spec also says that we avoid that
                     * column even in a different row.)
                     *
                     * All of this means that we end up choosing from a
                     * cyclically contiguous interval of blocks within this
                     * lane, but the start and end points require some thought
                     * to get them right.
                     */

                    /* Start position is the beginning of the _next_ slice
                     * (containing data from the previous pass), unless we're
                     * on pass 0, where the start position has to be 0. */
                    uint32_t Wstart = (pass == 0 ? 0 : (slice + 1) % 4 * SL);

                    /* End position splits up by cases. */
                    uint32_t Wend;
                    if (index_l == i) {
                        /* Same lane as output: we can use anything up to (but
                         * not including) the block immediately left of us. */
                        Wend = jm1;
                    } else {
                        /* Different lane from output: we can use anything up
                         * to the previous slice boundary, or one less than
                         * that if we're at the very left edge of our slice
                         * right now. */
                        Wend = SL * slice;
                        if (jpre == 0)
                            Wend = (Wend + q-1) % q;
                    }

                    /* Total number of blocks available to choose from */
                    uint32_t Wsize = (Wend + q - Wstart) % q;

                    /* Fiddly computation from the spec that chooses from the
                     * available blocks, in a deliberately non-uniform
                     * fashion, using J1 as pseudorandom input data. Output is
                     * zz which is the index within our contiguous interval. */
                    uint32_t x = ((uint64_t)J1 * J1) >> 32;
                    uint32_t y = ((uint64_t)Wsize * x) >> 32;
                    uint32_t zz = Wsize - 1 - y;

                    /* And index_z is the actual x coordinate of the block we
                     * want. */
                    uint32_t index_z = (Wstart + zz) % q;

                    /* Phew! Combine that block with the one immediately to
                     * our left, and XOR over the top of whatever is already
                     * in our current output block. */
                    G_xor(B[i + p * j].data, B[i + p * jm1].data,
                          B[index_l + p * index_z].data);
                }
            }

            /* We've finished processing a slice. Reset jstart to 0. It will
             * onily _not_ have been 0 if this was pass 0 slice 0, in which
             * case it still had its initial value of 2 to avoid the starting
             * data. */
            jstart = 0;
        }
    }

    /*
     * The main output is all done. Final output works by taking the XOR of
     * all the blocks in the rightmost column of the array, and then using
     * that as input to our long hash H'. The output of _that_ is what we
     * deliver to the caller.
     */

    struct blk C = B[p * (q-1)];
    for (size_t i = 1; i < p; i++)
        memxor(C.data, C.data, B[i + p * (q-1)].data, 1024);

    {
        ssh_hash *h = hprime_new(T);
        put_data(h, C.data, 1024);
        hprime_final(h, T, out);
    }

    /*
     * Clean up.
     */
    smemclr(out2i.data, sizeof(out2i.data));
    smemclr(tmp2i.data, sizeof(tmp2i.data));
    smemclr(in2i.data, sizeof(in2i.data));
    smemclr(C.data, sizeof(C.data));
    smemclr(B, mprime * sizeof(struct blk));
    sfree(B);
}

/*
 * Wrapper function that appends to a strbuf (which sshpubk.c will want).
 */
void argon2(Argon2Flavour flavour, uint32_t mem, uint32_t passes,
            uint32_t parallel, uint32_t taglen,
            ptrlen P, ptrlen S, ptrlen K, ptrlen X, strbuf *out)
{
    argon2_internal(parallel, taglen, mem, passes, flavour,
                    P, S, K, X, strbuf_append(out, taglen));
}

/*
 * Wrapper function which dynamically chooses the number of passes to run in
 * order to hit an approximate total amount of CPU time. Writes the result
 * into 'passes'.
 */
void argon2_choose_passes(
    Argon2Flavour flavour, uint32_t mem,
    uint32_t milliseconds, uint32_t *passes,
    uint32_t parallel, uint32_t taglen,
    ptrlen P, ptrlen S, ptrlen K, ptrlen X,
    strbuf *out)
{
    unsigned long desired_time = (TICKSPERSEC * milliseconds) / 1000;

    /*
     * We only need the time taken to be approximately right, so we
     * scale up the number of passes geometrically, which avoids
     * taking O(t^2) time to find a pass count taking time t.
     *
     * Using the Fibonacci numbers is slightly nicer than the obvious
     * approach of powers of 2, because it's still very easy to
     * compute, and grows less fast (powers of 1.6 instead of 2), so
     * you get just a touch more precision.
     */
    uint32_t a = 1, b = 1;

    while (true) {
        unsigned long start_time = GETTICKCOUNT();
        argon2(flavour, mem, b, parallel, taglen, P, S, K, X, out);
        unsigned long ticks = GETTICKCOUNT() - start_time;

        /* But just in case computers get _too_ fast, we have to cap
         * the growth before it gets past the uint32_t upper bound! So
         * if computing a+b would overflow, stop here. */

        if (ticks >= desired_time || a > (uint32_t)~b) {
            *passes = b;
            return;
        } else {
            strbuf_clear(out);

            /* Next Fibonacci number: replace (a, b) with (b, a+b) */
            b += a;
            a = b - a;
        }
    }
}
