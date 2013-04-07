/*
 * cryptographic random number generator for PuTTY's ssh client
 */

#include "putty.h"
#include "ssh.h"
#include <assert.h>

/* Collect environmental noise every 5 minutes */
#define NOISE_REGULAR_INTERVAL (5*60*TICKSPERSEC)

void noise_get_heavy(void (*func) (void *, int));
void noise_get_light(void (*func) (void *, int));

/*
 * `pool' itself is a pool of random data which we actually use: we
 * return bytes from `pool', at position `poolpos', until `poolpos'
 * reaches the end of the pool. At this point we generate more
 * random data, by adding noise, stirring well, and resetting
 * `poolpos' to point to just past the beginning of the pool (not
 * _the_ beginning, since otherwise we'd give away the whole
 * contents of our pool, and attackers would just have to guess the
 * next lot of noise).
 *
 * `incomingb' buffers acquired noise data, until it gets full, at
 * which point the acquired noise is SHA'ed into `incoming' and
 * `incomingb' is cleared. The noise in `incoming' is used as part
 * of the noise for each stirring of the pool, in addition to local
 * time, process listings, and other such stuff.
 */

#define HASHINPUT 64		       /* 64 bytes SHA input */
#define HASHSIZE 20		       /* 160 bits SHA output */
#define POOLSIZE 1200		       /* size of random pool */

#ifdef MPEXT
extern CRITICAL_SECTION noise_section;
#endif MPEXT

struct RandPool {
    unsigned char pool[POOLSIZE];
    int poolpos;

    unsigned char incoming[HASHSIZE];

    unsigned char incomingb[HASHINPUT];
    int incomingpos;

    int stir_pending;
};

static struct RandPool pool;
int random_active = 0;
long next_noise_collection;

static void random_stir(void)
{
    word32 block[HASHINPUT / sizeof(word32)];
    word32 digest[HASHSIZE / sizeof(word32)];
    int i, j, k;

    /*
     * noise_get_light will call random_add_noise, which may call
     * back to here. Prevent recursive stirs.
     */
    if (pool.stir_pending)
	return;
    pool.stir_pending = TRUE;

    noise_get_light(random_add_noise);

    SHATransform((word32 *) pool.incoming, (word32 *) pool.incomingb);
    pool.incomingpos = 0;

    /*
     * Chunks of this code are blatantly endianness-dependent, but
     * as it's all random bits anyway, WHO CARES?
     */
    memcpy(digest, pool.incoming, sizeof(digest));

    /*
     * Make two passes over the pool.
     */
    for (i = 0; i < 2; i++) {

	/*
	 * We operate SHA in CFB mode, repeatedly adding the same
	 * block of data to the digest. But we're also fiddling
	 * with the digest-so-far, so this shouldn't be Bad or
	 * anything.
	 */
	memcpy(block, pool.pool, sizeof(block));

	/*
	 * Each pass processes the pool backwards in blocks of
	 * HASHSIZE, just so that in general we get the output of
	 * SHA before the corresponding input, in the hope that
	 * things will be that much less predictable that way
	 * round, when we subsequently return bytes ...
	 */
	for (j = POOLSIZE; (j -= HASHSIZE) >= 0;) {
	    /*
	     * XOR the bit of the pool we're processing into the
	     * digest.
	     */

	    for (k = 0; k < sizeof(digest) / sizeof(*digest); k++)
		digest[k] ^= ((word32 *) (pool.pool + j))[k];

	    /*
	     * Munge our unrevealed first block of the pool into
	     * it.
	     */
	    SHATransform(digest, block);

	    /*
	     * Stick the result back into the pool.
	     */

	    for (k = 0; k < sizeof(digest) / sizeof(*digest); k++)
		((word32 *) (pool.pool + j))[k] = digest[k];
	}
    }

    /*
     * Might as well save this value back into `incoming', just so
     * there'll be some extra bizarreness there.
     */
    SHATransform(digest, block);
    memcpy(pool.incoming, digest, sizeof(digest));

    pool.poolpos = sizeof(pool.incoming);

    pool.stir_pending = FALSE;
}

void random_add_noise(void *noise, int length)
{
    unsigned char *p = noise;
    int i;

    if (!random_active)
	return;

    /*
     * This function processes HASHINPUT bytes into only HASHSIZE
     * bytes, so _if_ we were getting incredibly high entropy
     * sources then we would be throwing away valuable stuff.
     */
    while (length >= (HASHINPUT - pool.incomingpos)) {
	memcpy(pool.incomingb + pool.incomingpos, p,
	       HASHINPUT - pool.incomingpos);
	p += HASHINPUT - pool.incomingpos;
	length -= HASHINPUT - pool.incomingpos;
	SHATransform((word32 *) pool.incoming, (word32 *) pool.incomingb);
	for (i = 0; i < HASHSIZE; i++) {
	    pool.pool[pool.poolpos++] ^= pool.incomingb[i];
	    if (pool.poolpos >= POOLSIZE)
		pool.poolpos = 0;
	}
	if (pool.poolpos < HASHSIZE)
	    random_stir();

	pool.incomingpos = 0;
    }

#ifdef MPEXT
    if (length > 0)
#endif
    memcpy(pool.incomingb + pool.incomingpos, p, length);
    pool.incomingpos += length;
}

void random_add_heavynoise(void *noise, int length)
{
    unsigned char *p = noise;
    int i;

    while (length >= POOLSIZE) {
	for (i = 0; i < POOLSIZE; i++)
	    pool.pool[i] ^= *p++;
	random_stir();
	length -= POOLSIZE;
    }

    for (i = 0; i < length; i++)
	pool.pool[i] ^= *p++;
    random_stir();
}

static void random_add_heavynoise_bitbybit(void *noise, int length)
{
    unsigned char *p = noise;
    int i;

    while (length >= POOLSIZE - pool.poolpos) {
	for (i = 0; i < POOLSIZE - pool.poolpos; i++)
	    pool.pool[pool.poolpos + i] ^= *p++;
	random_stir();
	length -= POOLSIZE - pool.poolpos;
	pool.poolpos = 0;
    }

    for (i = 0; i < length; i++)
	pool.pool[i] ^= *p++;
    pool.poolpos = i;
}

static void random_timer(void *ctx, long now)
{
    if (random_active > 0 && now - next_noise_collection >= 0) {
	noise_regular();
	next_noise_collection =
	    schedule_timer(NOISE_REGULAR_INTERVAL, random_timer, &pool);
    }
}

void random_ref(void)
{
    if (!random_active) {
#ifdef MPEXT
        InitializeCriticalSection(&noise_section);
#endif
	memset(&pool, 0, sizeof(pool));    /* just to start with */

	noise_get_heavy(random_add_heavynoise_bitbybit);
	random_stir();

	next_noise_collection =
	    schedule_timer(NOISE_REGULAR_INTERVAL, random_timer, &pool);
    }

#ifdef MPEXT
    EnterCriticalSection(&noise_section);
#endif
    random_active++;
#ifdef MPEXT
    LeaveCriticalSection(&noise_section);
#endif
}

void random_unref(void)
{
#ifdef MPEXT
    EnterCriticalSection(&noise_section);
#endif
    random_active--;
    assert(random_active >= 0);
#ifdef MPEXT
    if (random_active)
    {
        LeaveCriticalSection(&noise_section);
        return;
    }
#else
    if (random_active) return;
#endif

    expire_timer_context(&pool);
#ifdef MPEXT
    LeaveCriticalSection(&noise_section);
#endif
}

int random_byte(void)
{
#ifdef MPEXT
    int pos = pool.poolpos;

    if (pos < sizeof(pool.incoming) || pos >= POOLSIZE)
    {
      EnterCriticalSection(&noise_section);
      if (pool.poolpos >= POOLSIZE)
      {
        random_stir();
      }
      pos = pool.poolpos;
      pool.poolpos++;
      LeaveCriticalSection(&noise_section);
    }
    else
    {
      pool.poolpos++;
    }

    return pool.pool[pos];
#else
    if (pool.poolpos >= POOLSIZE)
	random_stir();

    return pool.pool[pool.poolpos++];
#endif
}

void random_get_savedata(void **data, int *len)
{
    void *buf = snewn(POOLSIZE / 2, char);
#ifdef MPEXT
    EnterCriticalSection(&noise_section);
#endif
    random_stir();
    memcpy(buf, pool.pool + pool.poolpos, POOLSIZE / 2);
    *len = POOLSIZE / 2;
    *data = buf;
    random_stir();
#ifdef MPEXT
    LeaveCriticalSection(&noise_section);
#endif
}
