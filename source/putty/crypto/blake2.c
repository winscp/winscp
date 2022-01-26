/*
 * BLAKE2 (RFC 7693) implementation for PuTTY.
 *
 * The BLAKE2 hash family includes BLAKE2s, in which the hash state is
 * operated on as a collection of 32-bit integers, and BLAKE2b, based
 * on 64-bit integers. At present this code implements BLAKE2b only.
 */

#include <assert.h>
#include "ssh.h"

static inline uint64_t ror(uint64_t x, unsigned rotation)
{
    unsigned lshift = 63 & -rotation, rshift = 63 & rotation;
    return (x << lshift) | (x >> rshift);
}

/* RFC 7963 section 2.1 */
enum { R1 = 32, R2 = 24, R3 = 16, R4 = 63 };

/* RFC 7693 section 2.6 */
static const uint64_t iv[] = {
    0x6a09e667f3bcc908,                /* floor(2^64 * frac(sqrt(2)))  */
    0xbb67ae8584caa73b,                /* floor(2^64 * frac(sqrt(3)))  */
    0x3c6ef372fe94f82b,                /* floor(2^64 * frac(sqrt(5)))  */
    0xa54ff53a5f1d36f1,                /* floor(2^64 * frac(sqrt(7)))  */
    0x510e527fade682d1,                /* floor(2^64 * frac(sqrt(11))) */
    0x9b05688c2b3e6c1f,                /* floor(2^64 * frac(sqrt(13))) */
    0x1f83d9abfb41bd6b,                /* floor(2^64 * frac(sqrt(17))) */
    0x5be0cd19137e2179,                /* floor(2^64 * frac(sqrt(19))) */
};

/* RFC 7693 section 2.7 */
static const unsigned char sigma[][16] = {
    { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15},
    {14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3},
    {11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4},
    { 7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8},
    { 9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13},
    { 2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9},
    {12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11},
    {13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10},
    { 6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5},
    {10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0},
    /* This array recycles if you have more than 10 rounds. BLAKE2b
     * has 12, so we repeat the first two rows again. */
    { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15},
    {14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3},
};

static inline void g_half(uint64_t v[16], unsigned a, unsigned b, unsigned c,
                          unsigned d, uint64_t x, unsigned r1, unsigned r2)
{
    v[a] += v[b] + x;
    v[d] ^= v[a];
    v[d] = ror(v[d], r1);
    v[c] += v[d];
    v[b] ^= v[c];
    v[b] = ror(v[b], r2);
}

static inline void g(uint64_t v[16], unsigned a, unsigned b, unsigned c,
                     unsigned d, uint64_t x, uint64_t y)
{
    g_half(v, a, b, c, d, x, R1, R2);
    g_half(v, a, b, c, d, y, R3, R4);
}

static inline void f(uint64_t h[8], uint64_t m[16], uint64_t offset_hi,
                     uint64_t offset_lo, unsigned final)
{
    uint64_t v[16];
    memcpy(v, h, 8 * sizeof(*v));
    memcpy(v + 8, iv, 8 * sizeof(*v));
    v[12] ^= offset_lo;
    v[13] ^= offset_hi;
    v[14] ^= -(uint64_t)final;
    for (unsigned round = 0; round < 12; round++) {
        const unsigned char *s = sigma[round];
        g(v,  0,  4,  8, 12, m[s[ 0]], m[s[ 1]]);
        g(v,  1,  5,  9, 13, m[s[ 2]], m[s[ 3]]);
        g(v,  2,  6, 10, 14, m[s[ 4]], m[s[ 5]]);
        g(v,  3,  7, 11, 15, m[s[ 6]], m[s[ 7]]);
        g(v,  0,  5, 10, 15, m[s[ 8]], m[s[ 9]]);
        g(v,  1,  6, 11, 12, m[s[10]], m[s[11]]);
        g(v,  2,  7,  8, 13, m[s[12]], m[s[13]]);
        g(v,  3,  4,  9, 14, m[s[14]], m[s[15]]);
    }
    for (unsigned i = 0; i < 8; i++)
        h[i] ^= v[i] ^ v[i+8];
    smemclr(v, sizeof(v));
}

static inline void f_outer(uint64_t h[8], uint8_t blk[128], uint64_t offset_hi,
                           uint64_t offset_lo, unsigned final)
{
    uint64_t m[16];
    for (unsigned i = 0; i < 16; i++)
        m[i] = GET_64BIT_LSB_FIRST(blk + 8*i);
    f(h, m, offset_hi, offset_lo, final);
    smemclr(m, sizeof(m));
}

typedef struct blake2b {
    uint64_t h[8];
    unsigned hashlen;

    uint8_t block[128];
    size_t used;
    uint64_t lenhi, lenlo;

    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} blake2b;

static void blake2b_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *blake2b_new_inner(unsigned hashlen)
{
    assert(hashlen <= ssh_blake2b.hlen);

    blake2b *s = snew(blake2b);
    s->hash.vt = &ssh_blake2b;
    s->hashlen = hashlen;
    BinarySink_INIT(s, blake2b_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static ssh_hash *blake2b_new(const ssh_hashalg *alg)
{
    return blake2b_new_inner(alg->hlen);
}

ssh_hash *blake2b_new_general(unsigned hashlen)
{
    ssh_hash *h = blake2b_new_inner(hashlen);
    ssh_hash_reset(h);
    return h;
}

static void blake2b_reset(ssh_hash *hash)
{
    blake2b *s = container_of(hash, blake2b, hash);

    /* Initialise the hash to the standard IV */
    memcpy(s->h, iv, sizeof(s->h));

    /* XOR in the parameters: secret key length (here always 0) in
     * byte 1, and hash length in byte 0. */
    s->h[0] ^= 0x01010000 ^ s->hashlen;

    s->used = 0;
    s->lenhi = s->lenlo = 0;
}

static void blake2b_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    blake2b *copy = container_of(hcopy, blake2b, hash);
    blake2b *orig = container_of(horig, blake2b, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void blake2b_free(ssh_hash *hash)
{
    blake2b *s = container_of(hash, blake2b, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void blake2b_write(BinarySink *bs, const void *vp, size_t len)
{
    blake2b *s = BinarySink_DOWNCAST(bs, blake2b);
    const uint8_t *p = vp;

    while (len > 0) {
        if (s->used == sizeof(s->block)) {
            f_outer(s->h, s->block, s->lenhi, s->lenlo, 0);
            s->used = 0;
        }

        size_t chunk = sizeof(s->block) - s->used;
        if (chunk > len)
            chunk = len;

        memcpy(s->block + s->used, p, chunk);
        s->used += chunk;
        p += chunk;
        len -= chunk;

        s->lenlo += chunk;
        s->lenhi += (s->lenlo < chunk);
    }
}

static void blake2b_digest(ssh_hash *hash, uint8_t *digest)
{
    blake2b *s = container_of(hash, blake2b, hash);

    memset(s->block + s->used, 0, sizeof(s->block) - s->used);
    f_outer(s->h, s->block, s->lenhi, s->lenlo, 1);

    uint8_t hash_pre[128];
    for (unsigned i = 0; i < 8; i++)
        PUT_64BIT_LSB_FIRST(hash_pre + 8*i, s->h[i]);
    memcpy(digest, hash_pre, s->hashlen);
    smemclr(hash_pre, sizeof(hash_pre));
}

const ssh_hashalg ssh_blake2b = {
    .new = blake2b_new,
    .reset = blake2b_reset,
    .copyfrom = blake2b_copyfrom,
    .digest = blake2b_digest,
    .free = blake2b_free,
    .hlen = 64,
    .blocklen = 128,
    HASHALG_NAMES_BARE("BLAKE2b-64"),
};
