/*
 * Software implementation of SHA-512.
 */

#include "ssh.h"
#include "sha512.h"

static bool sha512_sw_available(void)
{
    /* Software SHA-512 is always available */
    return true;
}

static inline uint64_t ror(uint64_t x, unsigned y)
{
    return (x << (63 & -y)) | (x >> (63 & y));
}

static inline uint64_t Ch(uint64_t ctrl, uint64_t if1, uint64_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint64_t Maj(uint64_t x, uint64_t y, uint64_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint64_t Sigma_0(uint64_t x)
{
    return ror(x,28) ^ ror(x,34) ^ ror(x,39);
}

static inline uint64_t Sigma_1(uint64_t x)
{
    return ror(x,14) ^ ror(x,18) ^ ror(x,41);
}

static inline uint64_t sigma_0(uint64_t x)
{
    return ror(x,1) ^ ror(x,8) ^ (x >> 7);
}

static inline uint64_t sigma_1(uint64_t x)
{
    return ror(x,19) ^ ror(x,61) ^ (x >> 6);
}

static inline void sha512_sw_round(
    unsigned round_index, const uint64_t *schedule,
    uint64_t *a, uint64_t *b, uint64_t *c, uint64_t *d,
    uint64_t *e, uint64_t *f, uint64_t *g, uint64_t *h)
{
    uint64_t t1 = *h + Sigma_1(*e) + Ch(*e,*f,*g) +
        sha512_round_constants[round_index] + schedule[round_index];

    uint64_t t2 = Sigma_0(*a) + Maj(*a,*b,*c);

    *d += t1;
    *h = t1 + t2;
}

static void sha512_sw_block(uint64_t *core, const uint8_t *block)
{
    uint64_t w[SHA512_ROUNDS];
    uint64_t a,b,c,d,e,f,g,h;

    int t;

    for (t = 0; t < 16; t++)
        w[t] = GET_64BIT_MSB_FIRST(block + 8*t);

    for (t = 16; t < SHA512_ROUNDS; t++)
        w[t] = w[t-16] + w[t-7] + sigma_0(w[t-15]) + sigma_1(w[t-2]);

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4]; f = core[5]; g = core[6]; h = core[7];

    for (t = 0; t < SHA512_ROUNDS; t+=8) {
        sha512_sw_round(t+0, w, &a,&b,&c,&d,&e,&f,&g,&h);
        sha512_sw_round(t+1, w, &h,&a,&b,&c,&d,&e,&f,&g);
        sha512_sw_round(t+2, w, &g,&h,&a,&b,&c,&d,&e,&f);
        sha512_sw_round(t+3, w, &f,&g,&h,&a,&b,&c,&d,&e);
        sha512_sw_round(t+4, w, &e,&f,&g,&h,&a,&b,&c,&d);
        sha512_sw_round(t+5, w, &d,&e,&f,&g,&h,&a,&b,&c);
        sha512_sw_round(t+6, w, &c,&d,&e,&f,&g,&h,&a,&b);
        sha512_sw_round(t+7, w, &b,&c,&d,&e,&f,&g,&h,&a);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d;
    core[4] += e; core[5] += f; core[6] += g; core[7] += h;

    smemclr(w, sizeof(w));
}

typedef struct sha512_sw {
    uint64_t core[8];
    sha512_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha512_sw;

static void sha512_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha512_sw_new(const ssh_hashalg *alg)
{
    sha512_sw *s = snew(sha512_sw);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha512_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void sha512_sw_reset(ssh_hash *hash)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);
    const struct sha512_extra *extra =
        (const struct sha512_extra *)hash->vt->extra;

    memcpy(s->core, extra->initial_state, sizeof(s->core));
    sha512_block_setup(&s->blk);
}

static void sha512_sw_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    sha512_sw *copy = container_of(hcopy, sha512_sw, hash);
    sha512_sw *orig = container_of(horig, sha512_sw, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void sha512_sw_free(ssh_hash *hash)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha512_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha512_sw *s = BinarySink_DOWNCAST(bs, sha512_sw);

    while (len > 0)
        if (sha512_block_write(&s->blk, &vp, &len))
            sha512_sw_block(s->core, s->blk.block);
}

static void sha512_sw_digest(ssh_hash *hash, uint8_t *digest)
{
    sha512_sw *s = container_of(hash, sha512_sw, hash);

    sha512_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < hash->vt->hlen / 8; i++)
        PUT_64BIT_MSB_FIRST(digest + 8*i, s->core[i]);
}

/*
 * This implementation doesn't need separate digest methods for
 * SHA-384 and SHA-512, because the above implementation reads the
 * hash length out of the vtable.
 */
#define sha384_sw_digest sha512_sw_digest

SHA512_VTABLES(sw, "unaccelerated");
