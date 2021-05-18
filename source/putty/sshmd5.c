/*
 * MD5 implementation for PuTTY. Written directly from the spec by
 * Simon Tatham.
 */

#include <assert.h>
#include "ssh.h"

static const uint32_t md5_initial_state[] = {
    0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476,
};

static const struct md5_round_constant {
    uint32_t addition, rotation, msg_index;
} md5_round_constants[] = {
    { 0xd76aa478,  7,  0 }, { 0xe8c7b756, 12,  1 },
    { 0x242070db, 17,  2 }, { 0xc1bdceee, 22,  3 },
    { 0xf57c0faf,  7,  4 }, { 0x4787c62a, 12,  5 },
    { 0xa8304613, 17,  6 }, { 0xfd469501, 22,  7 },
    { 0x698098d8,  7,  8 }, { 0x8b44f7af, 12,  9 },
    { 0xffff5bb1, 17, 10 }, { 0x895cd7be, 22, 11 },
    { 0x6b901122,  7, 12 }, { 0xfd987193, 12, 13 },
    { 0xa679438e, 17, 14 }, { 0x49b40821, 22, 15 },
    { 0xf61e2562,  5,  1 }, { 0xc040b340,  9,  6 },
    { 0x265e5a51, 14, 11 }, { 0xe9b6c7aa, 20,  0 },
    { 0xd62f105d,  5,  5 }, { 0x02441453,  9, 10 },
    { 0xd8a1e681, 14, 15 }, { 0xe7d3fbc8, 20,  4 },
    { 0x21e1cde6,  5,  9 }, { 0xc33707d6,  9, 14 },
    { 0xf4d50d87, 14,  3 }, { 0x455a14ed, 20,  8 },
    { 0xa9e3e905,  5, 13 }, { 0xfcefa3f8,  9,  2 },
    { 0x676f02d9, 14,  7 }, { 0x8d2a4c8a, 20, 12 },
    { 0xfffa3942,  4,  5 }, { 0x8771f681, 11,  8 },
    { 0x6d9d6122, 16, 11 }, { 0xfde5380c, 23, 14 },
    { 0xa4beea44,  4,  1 }, { 0x4bdecfa9, 11,  4 },
    { 0xf6bb4b60, 16,  7 }, { 0xbebfbc70, 23, 10 },
    { 0x289b7ec6,  4, 13 }, { 0xeaa127fa, 11,  0 },
    { 0xd4ef3085, 16,  3 }, { 0x04881d05, 23,  6 },
    { 0xd9d4d039,  4,  9 }, { 0xe6db99e5, 11, 12 },
    { 0x1fa27cf8, 16, 15 }, { 0xc4ac5665, 23,  2 },
    { 0xf4292244,  6,  0 }, { 0x432aff97, 10,  7 },
    { 0xab9423a7, 15, 14 }, { 0xfc93a039, 21,  5 },
    { 0x655b59c3,  6, 12 }, { 0x8f0ccc92, 10,  3 },
    { 0xffeff47d, 15, 10 }, { 0x85845dd1, 21,  1 },
    { 0x6fa87e4f,  6,  8 }, { 0xfe2ce6e0, 10, 15 },
    { 0xa3014314, 15,  6 }, { 0x4e0811a1, 21, 13 },
    { 0xf7537e82,  6,  4 }, { 0xbd3af235, 10, 11 },
    { 0x2ad7d2bb, 15,  2 }, { 0xeb86d391, 21,  9 },
};

typedef struct md5_block md5_block;
struct md5_block {
    uint8_t block[64];
    size_t used;
    uint64_t len;
};

static inline void md5_block_setup(md5_block *blk)
{
    blk->used = 0;
    blk->len = 0;
}

static inline bool md5_block_write(
    md5_block *blk, const void **vdata, size_t *len)
{
    size_t blkleft = sizeof(blk->block) - blk->used;
    size_t chunk = *len < blkleft ? *len : blkleft;

    const uint8_t *p = *vdata;
    memcpy(blk->block + blk->used, p, chunk);
    *vdata = p + chunk;
    *len -= chunk;
    blk->used += chunk;
    blk->len += chunk;

    if (blk->used == sizeof(blk->block)) {
        blk->used = 0;
        return true;
    }

    return false;
}

static inline void md5_block_pad(md5_block *blk, BinarySink *bs)
{
    uint64_t final_len = blk->len << 3;
    size_t pad = 63 & (55 - blk->used);

    put_byte(bs, 0x80);
    put_padding(bs, pad, 0);

    unsigned char buf[8];
    PUT_64BIT_LSB_FIRST(buf, final_len);
    put_data(bs, buf, 8);
    smemclr(buf, 8);

    assert(blk->used == 0 && "Should have exactly hit a block boundary");
}

static inline uint32_t rol(uint32_t x, unsigned y)
{
    return (x << (31 & y)) | (x >> (31 & -y));
}

static inline uint32_t Ch(uint32_t ctrl, uint32_t if1, uint32_t if0)
{
    return if0 ^ (ctrl & (if1 ^ if0));
}

/* Parameter functions for the four MD5 round types */
static inline uint32_t F(uint32_t x, uint32_t y, uint32_t z)
{ return Ch(x, y, z); }
static inline uint32_t G(uint32_t x, uint32_t y, uint32_t z)
{ return Ch(z, x, y); }
static inline uint32_t H(uint32_t x, uint32_t y, uint32_t z)
{ return x ^ y ^ z; }
static inline uint32_t I(uint32_t x, uint32_t y, uint32_t z)
{ return y ^ (x | ~z); }

static inline void md5_round(
    unsigned round_index, const uint32_t *message,
    uint32_t *a, uint32_t *b, uint32_t *c, uint32_t *d,
    uint32_t (*f)(uint32_t, uint32_t, uint32_t))
{
    struct md5_round_constant rc = md5_round_constants[round_index];

    *a = *b + rol(*a + f(*b, *c, *d) + message[rc.msg_index] + rc.addition,
                  rc.rotation);
}

static void md5_do_block(uint32_t *core, const uint8_t *block)
{
    uint32_t message_words[16];
    for (size_t i = 0; i < 16; i++)
        message_words[i] = GET_32BIT_LSB_FIRST(block + 4*i);

    uint32_t a = core[0], b = core[1], c = core[2], d = core[3];

    size_t t = 0;
    for (size_t u = 0; u < 4; u++) {
        md5_round(t++, message_words, &a, &b, &c, &d, F);
        md5_round(t++, message_words, &d, &a, &b, &c, F);
        md5_round(t++, message_words, &c, &d, &a, &b, F);
        md5_round(t++, message_words, &b, &c, &d, &a, F);
    }
    for (size_t u = 0; u < 4; u++) {
        md5_round(t++, message_words, &a, &b, &c, &d, G);
        md5_round(t++, message_words, &d, &a, &b, &c, G);
        md5_round(t++, message_words, &c, &d, &a, &b, G);
        md5_round(t++, message_words, &b, &c, &d, &a, G);
    }
    for (size_t u = 0; u < 4; u++) {
        md5_round(t++, message_words, &a, &b, &c, &d, H);
        md5_round(t++, message_words, &d, &a, &b, &c, H);
        md5_round(t++, message_words, &c, &d, &a, &b, H);
        md5_round(t++, message_words, &b, &c, &d, &a, H);
    }
    for (size_t u = 0; u < 4; u++) {
        md5_round(t++, message_words, &a, &b, &c, &d, I);
        md5_round(t++, message_words, &d, &a, &b, &c, I);
        md5_round(t++, message_words, &c, &d, &a, &b, I);
        md5_round(t++, message_words, &b, &c, &d, &a, I);
    }

    core[0] += a;
    core[1] += b;
    core[2] += c;
    core[3] += d;

    smemclr(message_words, sizeof(message_words));
}

typedef struct md5 {
    uint32_t core[4];
    md5_block blk;
    BinarySink_IMPLEMENTATION;
    ssh_hash hash;
} md5;

static void md5_write(BinarySink *bs, const void *vp, size_t len);

static ssh_hash *md5_new(const ssh_hashalg *alg)
{
    md5 *s = snew(md5);

    s->hash.vt = alg;
    BinarySink_INIT(s, md5_write);
    BinarySink_DELEGATE_INIT(&s->hash, s);
    return &s->hash;
}

static void md5_reset(ssh_hash *hash)
{
    md5 *s = container_of(hash, md5, hash);

    memcpy(s->core, md5_initial_state, sizeof(s->core));
    md5_block_setup(&s->blk);
}

static void md5_copyfrom(ssh_hash *hcopy, ssh_hash *horig)
{
    md5 *copy = container_of(hcopy, md5, hash);
    md5 *orig = container_of(horig, md5, hash);

    memcpy(copy, orig, sizeof(*copy));
    BinarySink_COPIED(copy);
    BinarySink_DELEGATE_INIT(&copy->hash, copy);
}

static void md5_free(ssh_hash *hash)
{
    md5 *s = container_of(hash, md5, hash);

    smemclr(s, sizeof(*s));
    sfree(s);
}

static void md5_write(BinarySink *bs, const void *vp, size_t len)
{
    md5 *s = BinarySink_DOWNCAST(bs, md5);

    while (len > 0)
        if (md5_block_write(&s->blk, &vp, &len))
            md5_do_block(s->core, s->blk.block);
}

static void md5_digest(ssh_hash *hash, uint8_t *digest)
{
    md5 *s = container_of(hash, md5, hash);

    md5_block_pad(&s->blk, BinarySink_UPCAST(s));
    for (size_t i = 0; i < 4; i++)
        PUT_32BIT_LSB_FIRST(digest + 4*i, s->core[i]);
}

const ssh_hashalg ssh_md5 = {
    .new = md5_new,
    .reset = md5_reset,
    .copyfrom = md5_copyfrom,
    .digest = md5_digest,
    .free = md5_free,
    .hlen = 16,
    .blocklen = 64,
    HASHALG_NAMES_BARE("MD5"),
};
