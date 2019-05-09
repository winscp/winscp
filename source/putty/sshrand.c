/*
 * sshrand.c: manage the global live PRNG instance.
 */

#include "putty.h"
#include "ssh.h"
#include <assert.h>

/* Collect environmental noise every 5 minutes */
#define NOISE_REGULAR_INTERVAL (5*60*TICKSPERSEC)

int random_active = 0;

#ifdef FUZZING

/*
 * Special dummy version of the RNG for use when fuzzing.
 */
void random_add_noise(NoiseSourceId source, const void *noise, int length) { }
void random_ref(void) { }
void random_setup_special(void) { }
void random_unref(void) { }
void random_read(void *out, size_t size)
{
    memset(out, 0x45, size); /* Chosen by eight fair coin tosses */
}
void random_get_savedata(void **data, int *len) { }

#else /* !FUZZING */

/* Dummy structure for the sake of having something to expire_timer_context */
static struct random_timer_context { int dummy; } random_timer_ctx;

static prng *global_prng;
static unsigned long next_noise_collection;

void random_add_noise(NoiseSourceId source, const void *noise, int length)
{
    if (!random_active)
	return;

    prng_add_entropy(global_prng, source, make_ptrlen(noise, length));
}

static void random_timer(void *ctx, unsigned long now)
{
    if (random_active > 0 && now == next_noise_collection) {
	noise_regular();
	next_noise_collection =
	    schedule_timer(NOISE_REGULAR_INTERVAL, random_timer,
                           &random_timer_ctx);
    }
}

static void random_seed_callback(void *noise, int length)
{
    put_data(global_prng, noise, length);
}

static void random_create(const ssh_hashalg *hashalg)
{
    assert(!global_prng);
    global_prng = prng_new(hashalg);

    prng_seed_begin(global_prng);
    noise_get_heavy(random_seed_callback);
    prng_seed_finish(global_prng);

    next_noise_collection =
        schedule_timer(NOISE_REGULAR_INTERVAL, random_timer,
                       &random_timer_ctx);

    /* noise_get_heavy probably read our random seed file.
     * Therefore (in fact, even if it didn't), we should write a
     * fresh one, in case another instance of ourself starts up
     * before we finish, and also in case an attacker gets hold of
     * the seed data we used. */
    random_save_seed();
}

void random_ref(void)
{
    if (!random_active++)
        random_create(&ssh_sha256);
}

void random_setup_special()
{
    random_active++;
    random_create(&ssh_sha512);
}

void random_reseed(ptrlen seed)
{
    prng_seed_begin(global_prng);
    put_datapl(global_prng, seed);
    prng_seed_finish(global_prng);
}

void random_unref(void)
{
    assert(random_active > 0);
    if (random_active == 1) {
        random_save_seed();
        expire_timer_context(&random_timer_ctx);
        prng_free(global_prng);
        global_prng = NULL;
    }
    random_active--;
}

void random_read(void *buf, size_t size)
{
    assert(random_active > 0);
    prng_read(global_prng, buf, size);
}

void random_get_savedata(void **data, int *len)
{
    void *buf = snewn(global_prng->savesize, char);
    random_read(buf, global_prng->savesize);
    *len = global_prng->savesize;
    *data = buf;
}

size_t random_seed_bits(void)
{
    assert(random_active > 0);
    return prng_seed_bits(global_prng);
}

#endif /* FUZZING */
