/*
 * Software implementation of SHA-1.
 */

#include "ssh.h"
#include "sha1.h"

static bool sha1_sw_available(void)
{
    /* Software SHA-1 is always available */
    return true;
}

static inline uint32_t rol(uint32_t x, unsigned y)
{
    return (x << (31 & y)) | (x >> (31 & -y));
}

static inline uint32_t Ch(uint32_t ctrl, uint32_t if1, uint32_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

static inline uint32_t Maj(uint32_t x, uint32_t y, uint32_t z)
{
    return (x & y) | (z & (x | y));
}

static inline uint32_t Par(uint32_t x, uint32_t y, uint32_t z)
{
    return (x ^ y ^ z);
}

static inline void sha1_sw_round(
    unsigned round_index, const uint32_t *schedule,
    uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d, uint32_t *e,
    uint32_t f, uint32_t constant)
{
    *e = rol(*a, 5) + f + *e + schedule[round_index] + constant;
    *b = rol(*b, 30);
}

static void sha1_sw_block(uint32_t *core, const uint8_t *block)
{
    uint32_t w[SHA1_ROUNDS];
    uint32_t a,b,c,d,e;

    for (size_t t = 0; t < 16; t++)
        w[t] = GET_32BIT_MSB_FIRST(block + 4*t);

    for (size_t t = 16; t < SHA1_ROUNDS; t++)
        w[t] = rol(w[t - 3] ^ w[t - 8] ^ w[t - 14] ^ w[t - 16], 1);

    a = core[0]; b = core[1]; c = core[2]; d = core[3];
    e = core[4];

    size_t t = 0;
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Ch(b,c,d), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Ch(a,b,c), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Ch(e,a,b), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Ch(d,e,a), SHA1_STAGE0_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Ch(c,d,e), SHA1_STAGE0_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Par(b,c,d), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Par(a,b,c), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Par(e,a,b), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Par(d,e,a), SHA1_STAGE1_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Par(c,d,e), SHA1_STAGE1_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Maj(b,c,d), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Maj(a,b,c), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Maj(e,a,b), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Maj(d,e,a), SHA1_STAGE2_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Maj(c,d,e), SHA1_STAGE2_CONSTANT);
    }
    for (size_t u = 0; u < SHA1_ROUNDS_PER_STAGE/5; u++) {
        sha1_sw_round(t++,w, &a,&b,&c,&d,&e, Par(b,c,d), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &e,&a,&b,&c,&d, Par(a,b,c), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &d,&e,&a,&b,&c, Par(e,a,b), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &c,&d,&e,&a,&b, Par(d,e,a), SHA1_STAGE3_CONSTANT);
        sha1_sw_round(t++,w, &b,&c,&d,&e,&a, Par(c,d,e), SHA1_STAGE3_CONSTANT);
    }

    core[0] += a; core[1] += b; core[2] += c; core[3] += d; core[4] += e;

    smemclr(w, sizeof(w));
}

typedef struct sha1_sw {
    uint32_t core[5];
    sha1_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} sha1_sw;

static void sha1_sw_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *sha1_sw_new(const ssh_hashalg *alg)
{
    sha1_sw *s = snew(sha1_sw);

    s->hash.vt = alg;
    BinarySink_INIT(s, sha1_sw_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void sha1_sw_reset(ssh_hash *hash)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);

    memcpy(s->core, sha1_initial_state, sizeof(s->core));
    sha1_block_setup(&s->blk);
}

static void sha1_sw_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    sha1_sw *copy = container_of(hcopy, sha1_sw, hash);
    sha1_sw *orig = container_of(horig, sha1_sw, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void sha1_sw_free(ssh_hash *hash)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void sha1_sw_write(BinarySink *bs, const void *vp, size_t len)
{
    sha1_sw *s = BinarySink_DOWNCAST(bs, sha1_sw);

    while (len > 0)
        if (sha1_block_write(&s->blk, &vp, &len))
            sha1_sw_block(s->core, s->blk.block);
}

static void sha1_sw_digest(ssh_hash *hash, uint8_t *digest)
{
    sha1_sw *s = container_of(hash, sha1_sw, hash);

    sha1_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < 5; i++)
        PUT_32BIT_MSB_FIRST(digest + 4*i, s->core[i]);
}

SHA1_VTABLE(sw, "unaccelerated");
