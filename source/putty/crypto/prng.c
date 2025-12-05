/*
 * PuTTY's cryptographic pseudorandom number generator.
 *
 * This module just defines the PRNG object type and its methods. The
 * usual global instance of it is managed by sshrand.c.
 */

#include "putty.h"
#include "ssh.h"
#include "mpint_i.h"

#ifdef PRNG_DIAGNOSTICS
#define prngdebug debug
#else
#define prngdebug(...) ((void)0)
#endif

/*
 * This random number generator is based on the 'Fortuna' design by
 * Niels Ferguson and Bruce Schneier. The biggest difference is that I
 * use SHA-256 in place of a block cipher: the generator side of the
 * system works by computing HASH(key || counter) instead of
 * ENCRYPT(counter, key).
 *
 * Rationale: the Fortuna description itself suggests that using
 * SHA-256 would be nice but people wouldn't accept it because it's
 * too slow - but PuTTY isn't a heavy enough user of random numbers to
 * make that a serious worry. In fact even with SHA-256 this generator
 * is faster than the one we previously used. Also the Fortuna
 * description worries about periodic rekeying to avoid the barely
 * detectable pattern of never repeating a cipher block - but with
 * SHA-256, even that shouldn't be a worry, because the output
 * 'blocks' are twice the size, and also SHA-256 has no guarantee of
 * bijectivity, so it surely _could_ be possible to generate the same
 * block from two counter values. Thirdly, Fortuna has to have a hash
 * function anyway, for reseeding and entropy collection, so reusing
 * the same one means it only depends on one underlying primitive and
 * can be easily reinstantiated with a larger hash function if you
 * decide you'd like to do that on a particular occasion.
 */

#define NCOLLECTORS 32
#define RESEED_DATA_SIZE 64

typedef struct prng_impl prng_impl;
struct prng_impl {
    prng Prng;

    const ssh_hashalg *hashalg;

    /*
     * Generation side:
     *
     * 'generator' is a hash object with the current key preloaded
     * into it. The counter-mode generation is achieved by copying
     * that hash object, appending the counter value to the copy, and
     * calling ssh_hash_final.
     */
    ssh_hash *generator;
    BignumInt counter[128 / BIGNUM_INT_BITS];

    /*
     * When re-seeding the generator, you call prng_seed_begin(),
     * which sets up a hash object in 'keymaker'. You write your new
     * seed data into it (which you can do by calling put_data on the
     * PRNG object itself) and then call prng_seed_finish(), which
     * finalises this hash and uses the output to set up the new
     * generator.
     *
     * The keymaker hash preimage includes the previous key, so if you
     * just want to change keys for the sake of not keeping the same
     * one for too long, you don't have to put any extra seed data in
     * at all.
     */
    ssh_hash *keymaker;

    /*
     * Collection side:
     *
     * There are NCOLLECTORS hash objects collecting entropy. Each
     * separately numbered entropy source puts its output into those
     * hash objects in the order 0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,...,
     * that is to say, each entropy source has a separate counter
     * which is incremented every time that source generates an event,
     * and the event data is added to the collector corresponding to
     * the index of the lowest set bit in the current counter value.
     *
     * Whenever collector #0 has at least RESEED_DATA_SIZE bytes (and
     * it's not at least 100ms since the last reseed), the PRNG is
     * reseeded, with seed data on reseed #n taken from the first j
     * collectors, where j is one more than the number of factors of 2
     * in n. That is, collector #0 is used in every reseed; #1 in
     * every other one, #2 in every fourth, etc.
     *
     * 'until_reseed' counts the amount of data that still needs to be
     * added to collector #0 before a reseed will be triggered.
     */
    uint32_t source_counters[NOISE_MAX_SOURCES];
    ssh_hash *collectors[NCOLLECTORS];
    size_t until_reseed;
    uint32_t reseeds;
    uint64_t last_reseed_time;
};

static void prng_seed_BinarySink_write(
    BinarySink *bs, const void *data, size_t len);

prng *prng_new(const ssh_hashalg *hashalg)
{
    prng_impl *pi = snew(prng_impl);

    memset(pi, 0, sizeof(prng_impl));
    pi->hashalg = hashalg;
    pi->keymaker = NULL;
    pi->generator = NULL;
    memset(pi->counter, 0, sizeof(pi->counter));
    for (size_t i = 0; i < NCOLLECTORS; i++)
        pi->collectors[i] = ssh_hash_new(pi->hashalg);
    pi->until_reseed = 0;
    BinarySink_INIT(&pi->Prng, prng_seed_BinarySink_write);

    pi->Prng.savesize = pi->hashalg->hlen * 4;

    return &pi->Prng;
}

void prng_free(prng *pr)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);

    smemclr(pi->counter, sizeof(pi->counter));
    for (size_t i = 0; i < NCOLLECTORS; i++)
        ssh_hash_free(pi->collectors[i]);
    if (pi->generator)
        ssh_hash_free(pi->generator);
    if (pi->keymaker)
        ssh_hash_free(pi->keymaker);
    smemclr(pi, sizeof(*pi));
    sfree(pi);
}

void prng_seed_begin(prng *pr)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);

    assert(!pi->keymaker);

    prngdebug("prng: reseed begin\n");

    /*
     * Make a hash instance that will generate the key for the new one.
     */
    if (pi->generator) {
        pi->keymaker = pi->generator;
        pi->generator = NULL;
    } else {
        pi->keymaker = ssh_hash_new(pi->hashalg);
    }

    put_byte(pi->keymaker, 'R');
}

static void prng_seed_BinarySink_write(
    BinarySink *bs, const void *data, size_t len)
{
    prng *pr = BinarySink_DOWNCAST(bs, prng);
    prng_impl *pi = container_of(pr, prng_impl, Prng);
    assert(pi->keymaker);
    prngdebug("prng: got %"SIZEu" bytes of seed\n", len);
    put_data(pi->keymaker, data, len);
}

void prng_seed_finish(prng *pr)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);
    unsigned char buf[MAX_HASH_LEN];

    assert(pi->keymaker);

    prngdebug("prng: reseed finish\n");

    /*
     * Actually generate the key.
     */
    ssh_hash_final(pi->keymaker, buf);
    pi->keymaker = NULL;

    /*
     * Load that key into a fresh hash instance, which will become the
     * new generator.
     */
    assert(!pi->generator);
    pi->generator = ssh_hash_new(pi->hashalg);
    put_data(pi->generator, buf, pi->hashalg->hlen);

    pi->until_reseed = RESEED_DATA_SIZE;
    pi->last_reseed_time = prng_reseed_time_ms();

    smemclr(buf, sizeof(buf));
}

static inline void prng_generate(prng_impl *pi, void *outbuf)
{
    ssh_hash *h = ssh_hash_copy(pi->generator);

    prngdebug("prng_generate\n");
    put_byte(h, 'G');
    for (unsigned i = 0; i < 128; i += 8)
        put_byte(h, pi->counter[i/BIGNUM_INT_BITS] >> (i%BIGNUM_INT_BITS));
    BignumCarry c = 1;
    for (unsigned i = 0; i < lenof(pi->counter); i++)
        BignumADC(pi->counter[i], c, pi->counter[i], 0, c);
    ssh_hash_final(h, outbuf);
}

void prng_read(prng *pr, void *vout, size_t size)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);
    unsigned char buf[MAX_HASH_LEN];

    assert(!pi->keymaker);

    prngdebug("prng_read %"SIZEu"\n", size);

    uint8_t *out = (uint8_t *)vout;
    while (size > 0) {
        prng_generate(pi, buf);
        size_t to_use = size > pi->hashalg->hlen ? pi->hashalg->hlen : size;
        memcpy(out, buf, to_use);
        out += to_use;
        size -= to_use;
    }

    smemclr(buf, sizeof(buf));

    prng_seed_begin(&pi->Prng);
    prng_seed_finish(&pi->Prng);
}

void prng_add_entropy(prng *pr, unsigned source_id, ptrlen data)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);

    assert(source_id < NOISE_MAX_SOURCES);
    uint32_t counter = ++pi->source_counters[source_id];

    size_t index = 0;
    while (index+1 < NCOLLECTORS && !(counter & 1)) {
        counter >>= 1;
        index++;
    }

    prngdebug("prng_add_entropy source=%u size=%"SIZEu" -> collector %zi\n",
              source_id, data.len, index);

    put_datapl(pi->collectors[index], data);

    if (index == 0)
        pi->until_reseed = (pi->until_reseed < data.len ? 0 :
                            pi->until_reseed - data.len);

    if (pi->until_reseed == 0 &&
        prng_reseed_time_ms() - pi->last_reseed_time >= 100) {
        prng_seed_begin(&pi->Prng);

        unsigned char buf[MAX_HASH_LEN];
        uint32_t reseed_index = ++pi->reseeds;
        prngdebug("prng entropy reseed #%"PRIu32"\n", reseed_index);
        for (size_t i = 0; i < NCOLLECTORS; i++) {
            prngdebug("emptying collector %"SIZEu"\n", i);
            ssh_hash_digest(pi->collectors[i], buf);
            put_data(&pi->Prng, buf, pi->hashalg->hlen);
            ssh_hash_reset(pi->collectors[i]);
            if (reseed_index & 1)
                break;
            reseed_index >>= 1;
        }
        smemclr(buf, sizeof(buf));
        prng_seed_finish(&pi->Prng);
    }
}

size_t prng_seed_bits(prng *pr)
{
    prng_impl *pi = container_of(pr, prng_impl, Prng);
    return pi->hashalg->hlen * 8;
}
