/*
 * SHA-3, as defined in FIPS PUB 202.
 */

#include <assert.h>
#include <string.h>
#include "ssh.h"

static inline uint64_t rol(uint64_t x, unsigned shift)
{
    unsigned L = (+shift) & 63;
    unsigned R = (-shift) & 63;
    return (x << L) | (x >> R);
}

/*
 * General Keccak is defined such that its state is a 5x5 array of
 * words which can be any power-of-2 size from 1 up to 64. SHA-3 fixes
 * on 64, and so do we.
 *
 * The number of rounds is defined as 12 + 2k if the word size is 2^k.
 * Here we have 64-bit words only, so k=6, so 24 rounds always.
 */
typedef uint64_t keccak_core_state[5][5];
#define NROUNDS 24               /* would differ for other word sizes */
static const uint64_t round_constants[NROUNDS];
static const unsigned rotation_counts[5][5];

/*
 * Core Keccak transform: just squodge the state around internally,
 * without adding or extracting any data from it.
 */
static void keccak_transform(keccak_core_state A)
{
    union {
        uint64_t C[5];
        uint64_t B[5][5];
    } u;

    for (unsigned round = 0; round < NROUNDS; round++) {
        /* theta step */
        for (unsigned x = 0; x < 5; x++)
            u.C[x] = A[x][0] ^ A[x][1] ^ A[x][2] ^ A[x][3] ^ A[x][4];
        for (unsigned x = 0; x < 5; x++) {
            uint64_t D = rol(u.C[(x+1) % 5], 1) ^ u.C[(x+4) % 5];
            for (unsigned y = 0; y < 5; y++)
                A[x][y] ^= D;
        }

        /* rho and pi steps */
        for (unsigned x = 0; x < 5; x++)
            for (unsigned y = 0; y < 5; y++)
                u.B[y][(2*x+3*y) % 5] = rol(A[x][y], rotation_counts[x][y]);

        /* chi step */
        for (unsigned x = 0; x < 5; x++)
            for (unsigned y = 0; y < 5; y++)
                A[x][y] = u.B[x][y] ^ (u.B[(x+2)%5][y] & ~u.B[(x+1)%5][y]);

        /* iota step */
        A[0][0] ^= round_constants[round];
    }

    smemclr(&u, sizeof(u));
}

typedef struct {
    keccak_core_state A;
    unsigned char bytes[25*8];
    unsigned char first_pad_byte;
    size_t bytes_got, bytes_wanted, hash_bytes;
} keccak_state;

/*
 * Keccak accumulation function: given a piece of message, add it to
 * the hash.
 */
static void keccak_accumulate(keccak_state *s, const void *vdata, size_t len)
{
    const unsigned char *data = (const unsigned char *)vdata;

    while (len >= s->bytes_wanted - s->bytes_got) {
        size_t b = s->bytes_wanted - s->bytes_got;
        memcpy(s->bytes + s->bytes_got, data, b);
        len -= b;
        data += b;

        size_t n = 0;
        for (unsigned y = 0; y < 5; y++) {
            for (unsigned x = 0; x < 5; x++) {
                if (n >= s->bytes_wanted)
                    break;

                s->A[x][y] ^= GET_64BIT_LSB_FIRST(s->bytes + n);
                n += 8;
            }
        }
        keccak_transform(s->A);

        s->bytes_got = 0;
    }

    memcpy(s->bytes + s->bytes_got, data, len);
    s->bytes_got += len;
}

/*
 * Keccak output function.
 */
static void keccak_output(keccak_state *s, void *voutput)
{
    unsigned char *output = (unsigned char *)voutput;

    /*
     * Add message padding.
     */
    {
        unsigned char padding[25*8];
        size_t len = s->bytes_wanted - s->bytes_got;
        if (len == 0)
            len = s->bytes_wanted;
        memset(padding, 0, len);
        padding[0] |= s->first_pad_byte;
        padding[len-1] |= 0x80;
        keccak_accumulate(s, padding, len);
    }

    size_t n = 0;
    for (unsigned y = 0; y < 5; y++) {
        for (unsigned x = 0; x < 5; x++) {
            size_t to_copy = s->hash_bytes - n;
            if (to_copy == 0)
                break;
            if (to_copy > 8)
                to_copy = 8;
            unsigned char outbytes[8];
            PUT_64BIT_LSB_FIRST(outbytes, s->A[x][y]);
            memcpy(output + n, outbytes, to_copy);
            n += to_copy;
        }
    }
}

static void keccak_init(keccak_state *s, unsigned hashbits, unsigned ratebits,
                        unsigned char first_pad_byte)
{
    int x, y;

    assert(hashbits % 8 == 0);
    assert(ratebits % 8 == 0);

    s->hash_bytes = hashbits / 8;
    s->bytes_wanted = (25 * 64 - ratebits) / 8;
    s->bytes_got = 0;
    s->first_pad_byte = first_pad_byte;

    assert(s->bytes_wanted % 8 == 0);

    for (y = 0; y < 5; y++)
        for (x = 0; x < 5; x++)
            s->A[x][y] = 0;
}

static void keccak_sha3_init(keccak_state *s, int hashbits)
{
    keccak_init(s, hashbits, hashbits * 2, 0x06);
}

static void keccak_shake_init(keccak_state *s, int parambits, int hashbits)
{
    keccak_init(s, hashbits, parambits * 2, 0x1f);
}

/*
 * Keccak round constants, generated via the LFSR specified in the
 * Keccak reference by the following piece of Python:

import textwrap
from functools import reduce

rbytes = [1]
while len(rbytes) < 7*24:
    k = rbytes[-1] * 2
    rbytes.append(k ^ (0x171 * (k >> 8)))

rbits = [byte & 1 for byte in rbytes]

rwords = [sum(rbits[i+j] << ((1 << j) - 1) for j in range(7))
          for i in range(0, len(rbits), 7)]

print(textwrap.indent("\n".join(textwrap.wrap(", ".join(
    map("0x{:016x}".format, rwords)))), " "*4))

*/

static const uint64_t round_constants[24] = {
    0x0000000000000001, 0x0000000000008082, 0x800000000000808a,
    0x8000000080008000, 0x000000000000808b, 0x0000000080000001,
    0x8000000080008081, 0x8000000000008009, 0x000000000000008a,
    0x0000000000000088, 0x0000000080008009, 0x000000008000000a,
    0x000000008000808b, 0x800000000000008b, 0x8000000000008089,
    0x8000000000008003, 0x8000000000008002, 0x8000000000000080,
    0x000000000000800a, 0x800000008000000a, 0x8000000080008081,
    0x8000000000008080, 0x0000000080000001, 0x8000000080008008
};

/*
 * Keccak per-element rotation counts, generated from the matrix
 * formula in the Keccak reference by the following piece of Python:

coords = [1, 0]
while len(coords) < 26:
    coords.append((2*coords[-2] + 3*coords[-1]) % 5)

matrix = { (coords[i], coords[i+1]) : i for i in range(24) }
matrix[0,0] = -1

f = lambda t: (t+1) * (t+2) // 2 % 64

for y in range(5):
    print("    {{{}}},".format(", ".join("{:2d}".format(f(matrix[y,x]))
                                         for x in range(5))))

*/
static const unsigned rotation_counts[5][5] = {
    { 0, 36,  3, 41, 18},
    { 1, 44, 10, 45,  2},
    {62,  6, 43, 15, 61},
    {28, 55, 25, 21, 56},
    {27, 20, 39,  8, 14},
};

/*
 * The PuTTY ssh_hashalg abstraction.
 */
struct keccak_hash {
    keccak_state state;
    ssh_hash hash;
    BinarySink_IMPLEMENTATION;
};

static void keccak_BinarySink_write(BinarySink *bs, const void *p, size_t len)
{
    struct keccak_hash *kh = BinarySink_DOWNCAST(bs, struct keccak_hash);
    keccak_accumulate(&kh->state, p, len);
}

static ssh_hash *keccak_new(const ssh_hashalg *alg)
{
    struct keccak_hash *kh = snew(struct keccak_hash);
    kh->hash.vt = alg;
    BinarySink_INIT(kh, keccak_BinarySink_write);
    BinarySink_DELEGATE_INIT(&kh->hash, kh);
    return ssh_hash_reset(&kh->hash);
}

static void keccak_free(ssh_hash *hash)
{
    struct keccak_hash *kh = container_of(hash, struct keccak_hash, hash);
    smemclr(kh, sizeof(*kh));
    sfree(kh);
}

static void keccak_copyfrom(ssh_hash *hnew, ssh_hash *hold)
{
    struct keccak_hash *khold = container_of(hold, struct keccak_hash, hash);
    struct keccak_hash *khnew = container_of(hnew, struct keccak_hash, hash);
    khnew->state = khold->state;
}

static void keccak_digest(ssh_hash *hash, unsigned char *output)
{
    struct keccak_hash *kh = container_of(hash, struct keccak_hash, hash);
    keccak_output(&kh->state, output);
}

static void sha3_reset(ssh_hash *hash)
{
    struct keccak_hash *kh = container_of(hash, struct keccak_hash, hash);
    keccak_sha3_init(&kh->state, hash->vt->hlen * 8);
}

#define DEFINE_SHA3(bits)                       \
    const ssh_hashalg ssh_sha3_##bits = {       \
        .new = keccak_new,                      \
        .reset = sha3_reset,                    \
        .copyfrom = keccak_copyfrom,            \
        .digest = keccak_digest,                \
        .free = keccak_free,                    \
        .hlen = bits/8,                         \
        .blocklen = 200 - 2*(bits/8),           \
        HASHALG_NAMES_BARE("SHA3-" #bits),      \
    }

DEFINE_SHA3(224);
DEFINE_SHA3(256);
DEFINE_SHA3(384);
DEFINE_SHA3(512);

static void shake256_reset(ssh_hash *hash)
{
    struct keccak_hash *kh = container_of(hash, struct keccak_hash, hash);
    keccak_shake_init(&kh->state, 256, hash->vt->hlen * 8);
}

/*
 * There is some confusion over the output length parameter for the
 * SHAKE functions. By my reading, FIPS PUB 202 defines SHAKE256(M,d)
 * to generate d _bits_ of output. But RFC 8032 (defining Ed448) talks
 * about "SHAKE256(x,114)" in a context where it definitely means
 * generating 114 _bytes_ of output.
 *
 * Our internal ID therefore suffixes the output length with "bytes",
 * to be clear which we're talking about
 */

#define DEFINE_SHAKE(param, hashbytes)                          \
    const ssh_hashalg ssh_shake##param##_##hashbytes##bytes = { \
        .new = keccak_new,                                      \
        .reset = shake##param##_reset,                          \
        .copyfrom = keccak_copyfrom,                            \
        .digest = keccak_digest,                                \
        .free = keccak_free,                                    \
        .hlen = hashbytes,                                      \
        .blocklen = 0,                                          \
        HASHALG_NAMES_BARE("SHAKE" #param),                     \
    }

DEFINE_SHAKE(256, 114);
