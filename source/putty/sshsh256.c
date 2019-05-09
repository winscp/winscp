/*
 * SHA-256 algorithm as described at
 * 
 *   http://csrc.nist.gov/cryptval/shs.html
 */

#include "ssh.h"
#include <assert.h>

/* ----------------------------------------------------------------------
 * Core SHA256 algorithm: processes 16-word blocks into a message digest.
 */

#define ror(x,y) ( ((x) << (32-y)) | (((uint32_t)(x)) >> (y)) )
#define shr(x,y) ( (((uint32_t)(x)) >> (y)) )
#define Ch(x,y,z) ( ((x) & (y)) ^ (~(x) & (z)) )
#define Maj(x,y,z) ( ((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)) )
#define bigsigma0(x) ( ror((x),2) ^ ror((x),13) ^ ror((x),22) )
#define bigsigma1(x) ( ror((x),6) ^ ror((x),11) ^ ror((x),25) )
#define smallsigma0(x) ( ror((x),7) ^ ror((x),18) ^ shr((x),3) )
#define smallsigma1(x) ( ror((x),17) ^ ror((x),19) ^ shr((x),10) )

typedef struct SHA256_State {
    uint32_t h[8];
    unsigned char block[64];
    int blkused;
    uint64_t len;
    void (*sha256)(struct SHA256_State * s, const unsigned char *p, int len);
    BinarySink_IMPLEMENTATION;
} SHA256_State;

static void SHA256_sw(SHA256_State *s, const unsigned char *q, int len);
static void SHA256_ni(SHA256_State *s, const unsigned char *q, int len);

#ifndef WINSCP_VS

void SHA256_Core_Init(SHA256_State *s) {
    s->h[0] = 0x6a09e667;
    s->h[1] = 0xbb67ae85;
    s->h[2] = 0x3c6ef372;
    s->h[3] = 0xa54ff53a;
    s->h[4] = 0x510e527f;
    s->h[5] = 0x9b05688c;
    s->h[6] = 0x1f83d9ab;
    s->h[7] = 0x5be0cd19;
}
#endif // !WINSCP_VS

#ifndef WINSCP_VS
void SHA256_Block(SHA256_State *s, uint32_t *block);
#else
void SHA256_Block(SHA256_State *s, uint32_t *block) {
    uint32_t w[80];
    uint32_t a,b,c,d,e,f,g,h;
    static const int k[] = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
        0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
        0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
        0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
        0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
        0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
        0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
        0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
    };

    int t;

    for (t = 0; t < 16; t++)
        w[t] = block[t];

    for (t = 16; t < 64; t++)
	w[t] = smallsigma1(w[t-2]) + w[t-7] + smallsigma0(w[t-15]) + w[t-16];

    a = s->h[0]; b = s->h[1]; c = s->h[2]; d = s->h[3];
    e = s->h[4]; f = s->h[5]; g = s->h[6]; h = s->h[7];

    for (t = 0; t < 64; t+=8) {
        uint32_t t1, t2;

#define ROUND(j,a,b,c,d,e,f,g,h) \
	t1 = h + bigsigma1(e) + Ch(e,f,g) + k[j] + w[j]; \
	t2 = bigsigma0(a) + Maj(a,b,c); \
        d = d + t1; h = t1 + t2;

	ROUND(t+0, a,b,c,d,e,f,g,h);
	ROUND(t+1, h,a,b,c,d,e,f,g);
	ROUND(t+2, g,h,a,b,c,d,e,f);
	ROUND(t+3, f,g,h,a,b,c,d,e);
	ROUND(t+4, e,f,g,h,a,b,c,d);
	ROUND(t+5, d,e,f,g,h,a,b,c);
	ROUND(t+6, c,d,e,f,g,h,a,b);
	ROUND(t+7, b,c,d,e,f,g,h,a);
    }

    s->h[0] += a; s->h[1] += b; s->h[2] += c; s->h[3] += d;
    s->h[4] += e; s->h[5] += f; s->h[6] += g; s->h[7] += h;
}
#endif // !WINSCP_VS

#ifndef WINSCP_VS
/* ----------------------------------------------------------------------
 * Outer SHA256 algorithm: take an arbitrary length byte string,
 * convert it into 16-word blocks with the prescribed padding at
 * the end, and pass those blocks to the core SHA256 algorithm.
 */

#define BLKSIZE 64

static void SHA256_BinarySink_write(BinarySink *bs,
                                    const void *p, size_t len);

void SHA256_Init(SHA256_State *s) {
    SHA256_Core_Init(s);
    s->blkused = 0;
    s->len = 0;
    if (supports_sha_ni())
        s->sha256 = &SHA256_ni;
    else
        s->sha256 = &SHA256_sw;
    BinarySink_INIT(s, SHA256_BinarySink_write);
}

static void SHA256_BinarySink_write(BinarySink *bs,
                                    const void *p, size_t len)
{
    struct SHA256_State *s = BinarySink_DOWNCAST(bs, struct SHA256_State);
    unsigned char *q = (unsigned char *)p;

    /*
     * Update the length field.
     */
    s->len += len;

    (*(s->sha256))(s, q, len);
}

static void SHA256_sw(SHA256_State *s, const unsigned char *q, int len) {
    uint32_t wordblock[16];
    int i;

    if (s->blkused && s->blkused+len < BLKSIZE) {
        /*
         * Trivial case: just add to the block.
         */
        memcpy(s->block + s->blkused, q, len);
        s->blkused += len;
    } else {
        /*
         * We must complete and process at least one block.
         */
        while (s->blkused + len >= BLKSIZE) {
            memcpy(s->block + s->blkused, q, BLKSIZE - s->blkused);
            q += BLKSIZE - s->blkused;
            len -= BLKSIZE - s->blkused;
            /* Now process the block. Gather bytes big-endian into words */
            for (i = 0; i < 16; i++) {
                wordblock[i] =
                    ( ((uint32_t)s->block[i*4+0]) << 24 ) |
                    ( ((uint32_t)s->block[i*4+1]) << 16 ) |
                    ( ((uint32_t)s->block[i*4+2]) <<  8 ) |
                    ( ((uint32_t)s->block[i*4+3]) <<  0 );
            }
            SHA256_Block(s, wordblock);
            s->blkused = 0;
        }
        memcpy(s->block, q, len);
        s->blkused = len;
    }
}

void SHA256_Final(SHA256_State *s, unsigned char *digest) {
    int i;
    int pad;
    unsigned char c[64];
    uint64_t len;

    if (s->blkused >= 56)
        pad = 56 + 64 - s->blkused;
    else
        pad = 56 - s->blkused;

    len = (s->len << 3);

    memset(c, 0, pad);
    c[0] = 0x80;
    put_data(s, &c, pad);

    put_uint64(s, len);

    for (i = 0; i < 8; i++) {
	digest[i*4+0] = (s->h[i] >> 24) & 0xFF;
	digest[i*4+1] = (s->h[i] >> 16) & 0xFF;
	digest[i*4+2] = (s->h[i] >>  8) & 0xFF;
	digest[i*4+3] = (s->h[i] >>  0) & 0xFF;
    }
}

void SHA256_Simple(const void *p, int len, unsigned char *output) {
    SHA256_State s;

    SHA256_Init(&s);
    put_data(&s, p, len);
    SHA256_Final(&s, output);
    smemclr(&s, sizeof(s));
}

/*
 * Thin abstraction for things where hashes are pluggable.
 */

struct sha256_hash {
    SHA256_State state;
    ssh_hash hash;
};

static ssh_hash *sha256_new(const ssh_hashalg *alg)
{
    struct sha256_hash *h = snew(struct sha256_hash);
    SHA256_Init(&h->state);
    h->hash.vt = alg;
    BinarySink_DELEGATE_INIT(&h->hash, &h->state);
    return &h->hash;
}

static ssh_hash *sha256_copy(ssh_hash *hashold)
{
    struct sha256_hash *hold, *hnew;
    ssh_hash *hashnew = sha256_new(hashold->vt);

    hold = container_of(hashold, struct sha256_hash, hash);
    hnew = container_of(hashnew, struct sha256_hash, hash);

    hnew->state = hold->state;
    BinarySink_COPIED(&hnew->state);

    return hashnew;
}

static void sha256_free(ssh_hash *hash)
{
    struct sha256_hash *h = container_of(hash, struct sha256_hash, hash);

    smemclr(h, sizeof(*h));
    sfree(h);
}

static void sha256_final(ssh_hash *hash, unsigned char *output)
{
    struct sha256_hash *h = container_of(hash, struct sha256_hash, hash);
    SHA256_Final(&h->state, output);
    sha256_free(hash);
}

const ssh_hashalg ssh_sha256 = {
    sha256_new, sha256_copy, sha256_final, sha256_free, 32, 64, "SHA-256"
};
#endif // !WINSCP_VS

#ifdef COMPILER_SUPPORTS_SHA_NI

#if defined _MSC_VER && defined _M_AMD64
# include <intrin.h>
#endif

/*
 * Set target architecture for Clang and GCC
 */
#if !defined(__clang__) && defined(__GNUC__)
#    pragma GCC target("sha")
#    pragma GCC target("sse4.1")
#endif

#if defined(__clang__) || (defined(__GNUC__) && (__GNUC__ >= 5))
#    define FUNC_ISA __attribute__ ((target("sse4.1,sha")))
#else
#    define FUNC_ISA
#endif

#include <wmmintrin.h>
#include <smmintrin.h>
#include <immintrin.h>

#if defined(__clang__) || defined(__GNUC__)
#include <shaintrin.h>
#endif

/* SHA256 implementation using new instructions
   The code is based on Jeffrey Walton's SHA256 implementation:
   https://github.com/noloader/SHA-Intrinsics
*/
FUNC_ISA
static void SHA256_ni_(SHA256_State * s, const unsigned char *q, int len) {
    if (s->blkused && s->blkused+len < BLKSIZE) {
        /*
         * Trivial case: just add to the block.
         */
        memcpy(s->block + s->blkused, q, len);
        s->blkused += len;
    } else {
        __m128i STATE0, STATE1;
        __m128i MSG, TMP;
        __m128i MSG0, MSG1, MSG2, MSG3;
        __m128i ABEF_SAVE, CDGH_SAVE;
        const __m128i MASK = _mm_set_epi64x(0x0c0d0e0f08090a0bULL, 0x0405060700010203ULL);

        /* Load initial values */
        TMP = _mm_loadu_si128((const __m128i*) &s->h[0]);
        STATE1 = _mm_loadu_si128((const __m128i*) &s->h[4]);

        TMP = _mm_shuffle_epi32(TMP, 0xB1);          /* CDAB */
        STATE1 = _mm_shuffle_epi32(STATE1, 0x1B);    /* EFGH */
        STATE0 = _mm_alignr_epi8(TMP, STATE1, 8);    /* ABEF */
        STATE1 = _mm_blend_epi16(STATE1, TMP, 0xF0); /* CDGH */
        /*
         * We must complete and process at least one block.
         */
        while (s->blkused + len >= BLKSIZE) {
            memcpy(s->block + s->blkused, q, BLKSIZE - s->blkused);
            q += BLKSIZE - s->blkused;
            len -= BLKSIZE - s->blkused;

                /* Save current state */
            ABEF_SAVE = STATE0;
            CDGH_SAVE = STATE1;

            /* Rounds 0-3 */
            MSG = _mm_loadu_si128((const __m128i*) (s->block + 0));
            MSG0 = _mm_shuffle_epi8(MSG, MASK);
            MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(0xE9B5DBA5B5C0FBCFULL, 0x71374491428A2F98ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

            /* Rounds 4-7 */
            MSG1 = _mm_loadu_si128((const __m128i*) (s->block + 16));
            MSG1 = _mm_shuffle_epi8(MSG1, MASK);
            MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(0xAB1C5ED5923F82A4ULL, 0x59F111F13956C25BULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

            /* Rounds 8-11 */
            MSG2 = _mm_loadu_si128((const __m128i*) (s->block + 32));
            MSG2 = _mm_shuffle_epi8(MSG2, MASK);
            MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(0x550C7DC3243185BEULL, 0x12835B01D807AA98ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

            /* Rounds 12-15 */
            MSG3 = _mm_loadu_si128((const __m128i*) (s->block + 48));
            MSG3 = _mm_shuffle_epi8(MSG3, MASK);
            MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(0xC19BF1749BDC06A7ULL, 0x80DEB1FE72BE5D74ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
            MSG0 = _mm_add_epi32(MSG0, TMP);
            MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

            /* Rounds 16-19 */
            MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(0x240CA1CC0FC19DC6ULL, 0xEFBE4786E49B69C1ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
            MSG1 = _mm_add_epi32(MSG1, TMP);
            MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

            /* Rounds 20-23 */
            MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(0x76F988DA5CB0A9DCULL, 0x4A7484AA2DE92C6FULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
            MSG2 = _mm_add_epi32(MSG2, TMP);
            MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

            /* Rounds 24-27 */
            MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(0xBF597FC7B00327C8ULL, 0xA831C66D983E5152ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
            MSG3 = _mm_add_epi32(MSG3, TMP);
            MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

            /* Rounds 28-31 */
            MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(0x1429296706CA6351ULL,  0xD5A79147C6E00BF3ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
            MSG0 = _mm_add_epi32(MSG0, TMP);
            MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

            /* Rounds 32-35 */
            MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(0x53380D134D2C6DFCULL, 0x2E1B213827B70A85ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
            MSG1 = _mm_add_epi32(MSG1, TMP);
            MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

            /* Rounds 36-39 */
            MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(0x92722C8581C2C92EULL, 0x766A0ABB650A7354ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
            MSG2 = _mm_add_epi32(MSG2, TMP);
            MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG0 = _mm_sha256msg1_epu32(MSG0, MSG1);

            /* Rounds 40-43 */
            MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(0xC76C51A3C24B8B70ULL, 0xA81A664BA2BFE8A1ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
            MSG3 = _mm_add_epi32(MSG3, TMP);
            MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG1 = _mm_sha256msg1_epu32(MSG1, MSG2);

            /* Rounds 44-47 */
            MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(0x106AA070F40E3585ULL, 0xD6990624D192E819ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG3, MSG2, 4);
            MSG0 = _mm_add_epi32(MSG0, TMP);
            MSG0 = _mm_sha256msg2_epu32(MSG0, MSG3);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG2 = _mm_sha256msg1_epu32(MSG2, MSG3);

            /* Rounds 48-51 */
            MSG = _mm_add_epi32(MSG0, _mm_set_epi64x(0x34B0BCB52748774CULL, 0x1E376C0819A4C116ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG0, MSG3, 4);
            MSG1 = _mm_add_epi32(MSG1, TMP);
            MSG1 = _mm_sha256msg2_epu32(MSG1, MSG0);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);
            MSG3 = _mm_sha256msg1_epu32(MSG3, MSG0);

            /* Rounds 52-55 */
            MSG = _mm_add_epi32(MSG1, _mm_set_epi64x(0x682E6FF35B9CCA4FULL, 0x4ED8AA4A391C0CB3ULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG1, MSG0, 4);
            MSG2 = _mm_add_epi32(MSG2, TMP);
            MSG2 = _mm_sha256msg2_epu32(MSG2, MSG1);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

            /* Rounds 56-59 */
            MSG = _mm_add_epi32(MSG2, _mm_set_epi64x(0x8CC7020884C87814ULL, 0x78A5636F748F82EEULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            TMP = _mm_alignr_epi8(MSG2, MSG1, 4);
            MSG3 = _mm_add_epi32(MSG3, TMP);
            MSG3 = _mm_sha256msg2_epu32(MSG3, MSG2);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

            /* Rounds 60-63 */
            MSG = _mm_add_epi32(MSG3, _mm_set_epi64x(0xC67178F2BEF9A3F7ULL, 0xA4506CEB90BEFFFAULL));
            STATE1 = _mm_sha256rnds2_epu32(STATE1, STATE0, MSG);
            MSG = _mm_shuffle_epi32(MSG, 0x0E);
            STATE0 = _mm_sha256rnds2_epu32(STATE0, STATE1, MSG);

            /* Combine state  */
            STATE0 = _mm_add_epi32(STATE0, ABEF_SAVE);
            STATE1 = _mm_add_epi32(STATE1, CDGH_SAVE);

            s->blkused = 0;
        }

        TMP = _mm_shuffle_epi32(STATE0, 0x1B);       /* FEBA */
        STATE1 = _mm_shuffle_epi32(STATE1, 0xB1);    /* DCHG */
        STATE0 = _mm_blend_epi16(TMP, STATE1, 0xF0); /* DCBA */
        STATE1 = _mm_alignr_epi8(STATE1, TMP, 8);    /* ABEF */

        /* Save state */
        _mm_storeu_si128((__m128i*) &s->h[0], STATE0);
        _mm_storeu_si128((__m128i*) &s->h[4], STATE1);

        memcpy(s->block, q, len);
        s->blkused = len;
    }
}

/*
 * Workaround LLVM bug https://bugs.llvm.org/show_bug.cgi?id=34980
 */
static void SHA256_ni(SHA256_State * s, const unsigned char *q, int len)
{
    SHA256_ni_(s, q, len);
}

#else /* COMPILER_SUPPORTS_AES_NI */

static void SHA256_ni(SHA256_State * s, const unsigned char *q, int len)
{
    unreachable("SHA256_ni not compiled in");
}

#endif  /* COMPILER_SUPPORTS_AES_NI */
