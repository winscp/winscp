/*
 * Software implementation of SHA-256.
 */

#include "ssh.h"
#include "sha256.h"

static bool sha256_sw_available(void)
{
    /* Software SHA-256 is always available */
    return true;
}

static inline uint32_t ror(uint32_t x, unsigned y)
{
    return (x << (31 & -y)) | (x >> (31 & y));
}

static inline uint32_t Ch(uint32_t ctrl, uint32_t if1, uint32_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint32_t Maj(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint32_t Sigma_0(uint32_t x)
{
    return ror(x,2) ^ ror(x,13) ^ ror(x,22);
}

static inline uint32_t Sigma_1(uint32_t x)
{
    return ror(x,6) ^ ror(x,11) ^ ror(x,25);
}

static inline uint32_t sigma_0(uint32_t x)
{
    return ror(x,7) ^ ror(x,18) ^ (x >> 3);
}

static inline uint32_t sigma_1(uint32_t x)
{
    return ror(x,17) ^ ror(x,19) ^ (x >> 10);
}

static inline void sha256_sw_round(
    unsigned round_index, const uint32_t *schedule,
    uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d,
    uint32_t *e, uint32_t *f, uint32_t *g, uint32_t *h)
{
    uint32_t t1 = *h + Sigma_1(*e) + Ch(*e,*f,*g) +
        sha256_round_constants[round_index] + schedule[round_index];

    uint32_t t2 = Sigma_0(*a) + Maj(*a,*b,*c);

    *d += t1;
    *h = t1 + t2;
}

static void sha256_sw_block(uint32_t *core, const uint8_t *block)
{
    uint32_t w[SHA256_ROUNDS];
    uint32_t a,b,c,d,e,f,g,h;

    for (size_t t = 0; t < 16; t++)
        w[t] = GET_32BIT_MSB_FIRST(block + 4*t);

    for (size_t t = 16; t < SHA256_ROUNDS; t++)
        w[t] = sigma_1(w[t-2]) + w[t-7] + sigma_0(w[t-15]) + w[t-16];

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4]; f = core[5]; g = core[6]; h = core[7];

    for (size_t t = 0; t < SHA256_ROUNDS; t += 8) {
        sha256_sw_round(t+0, w, &a,&b,&c,&d,&e,&f,&g,&h);
        sha256_sw_round(t+1, w, &h,&a,&b,&c,&d,&e,&f,&g);
        sha256_sw_round(t+2, w, &g,&h,&a,&b,&c,&d,&e,&f);
        sha256_sw_round(t+3, w, &f,&g,&h,&a,&b,&c,&d,&e);
        sha256_sw_round(t+4, w, &e,&f,&g,&h,&a,&b,&c,&d);
        sha256_sw_round(t+5, w, &d,&e,&f,&g,&h,&a,&b,&c);
        sha256_sw_round(t+6, w, &c,&d,&e,&f,&g,&h,&a,&b);
        sha256_sw_round(t+7, w, &b,&c,&d,&e,&f,&g,&h,&a);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d;
    core[4] += e; core[5] += f; core[6] += g; core[7] += h;

    smemclr(w, sizeof(w));
}

typedef struct sha256_sw {
    uint32_t core[8];
    sha256_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha256_sw;

static void sha256_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha256_sw_new(const ssh_hashalg *alg)
{
    sha256_sw *s = snew(sha256_sw);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha256_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void sha256_sw_reset(ssh_hash *hash)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);

    memcpy(s->core, sha256_initial_state, sizeof(s->core));
    sha256_block_setup(&s->blk);
}

static void sha256_sw_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    sha256_sw *copy = container_of(hcopy, sha256_sw, hash);
    sha256_sw *orig = container_of(horig, sha256_sw, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void sha256_sw_free(ssh_hash *hash)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha256_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha256_sw *s = BinarySink_DOWNCAST(bs, sha256_sw);

    while (len > 0)
        if (sha256_block_write(&s->blk, &vp, &len))
            sha256_sw_block(s->core, s->blk.block);
}

static void sha256_sw_digest(ssh_hash *hash, uint8_t *digest)
{
    sha256_sw *s = container_of(hash, sha256_sw, hash);

    sha256_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < 8; i++)
        PUT_32BIT_MSB_FIRST(digest + 4*i, s->core[i]);
}

SHA256_VTABLE(sw, "unaccelerated");
