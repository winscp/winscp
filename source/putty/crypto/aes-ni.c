/*
 * Hardware-accelerated implementation of AES using x86 AES-NI.
 */

#include "ssh.h"
#include "aes.h"

#include <wmmintrin.h>
#include <smmintrin.h>

#if defined(__clang__) || defined(__GNUC__)
#include <cpuid.h>
#define GET_CPU_ID(out) __cpuid(1, (out)[0], (out)[1], (out)[2], (out)[3])
#else
#define GET_CPU_ID(out) __cpuid(out, 1)
#endif

static bool aes_ni_available(void)
{
    /*
     * Determine if AES is available on this CPU, by checking that
     * both AES itself and SSE4.1 are supported.
     */
    unsigned int CPUInfo[4];
    GET_CPU_ID(CPUInfo);
    return (CPUInfo[2] & (1 << 25)) && (CPUInfo[2] & (1 << 19));
}

/*
 * Core AES-NI encrypt/decrypt functions, one per length and direction.
 */

#define NI_CIPHER(len, dir, dirlong, repmacro)                          \
    static inline __m128i aes_ni_##len##_##dir(                         \
        __m128i v, const __m128i *keysched)                             \
    {                                                                   \
        v = _mm_xor_si128(v, *keysched++);                              \
        repmacro(v = _mm_aes##dirlong##_si128(v, *keysched++););        \
        return _mm_aes##dirlong##last_si128(v, *keysched);              \
    }

NI_CIPHER(128, e, enc, REP9)
NI_CIPHER(128, d, dec, REP9)
NI_CIPHER(192, e, enc, REP11)
NI_CIPHER(192, d, dec, REP11)
NI_CIPHER(256, e, enc, REP13)
NI_CIPHER(256, d, dec, REP13)

/*
 * The main key expansion.
 */
static void aes_ni_key_expand(
    const unsigned char *key, size_t key_words,
    __m128i *keysched_e, __m128i *keysched_d)
{
    size_t rounds = key_words + 6;
    size_t sched_words = (rounds + 1) * 4;

    /*
     * Store the key schedule as 32-bit integers during expansion, so
     * that it's easy to refer back to individual previous words. We
     * collect them into the final __m128i form at the end.
     */
    uint32_t sched[MAXROUNDKEYS * 4];

    unsigned rconpos = 0;

    for (size_t i = 0; i < sched_words; i++) {
        if (i < key_words) {
            sched[i] = GET_32BIT_LSB_FIRST(key + 4 * i);
        } else {
            uint32_t temp = sched[i - 1];

            bool rotate_and_round_constant = (i % key_words == 0);
            bool only_sub = (key_words == 8 && i % 8 == 4);

            if (rotate_and_round_constant) {
                __m128i v = _mm_setr_epi32(0,temp,0,0);
                v = _mm_aeskeygenassist_si128(v, 0);
                temp = _mm_extract_epi32(v, 1);

                assert(rconpos < lenof(aes_key_setup_round_constants));
                temp ^= aes_key_setup_round_constants[rconpos++];
            } else if (only_sub) {
                __m128i v = _mm_setr_epi32(0,temp,0,0);
                v = _mm_aeskeygenassist_si128(v, 0);
                temp = _mm_extract_epi32(v, 0);
            }

            sched[i] = sched[i - key_words] ^ temp;
        }
    }

    /*
     * Combine the key schedule words into __m128i vectors and store
     * them in the output context.
     */
    for (size_t round = 0; round <= rounds; round++)
        keysched_e[round] = _mm_setr_epi32(
            sched[4*round  ], sched[4*round+1],
            sched[4*round+2], sched[4*round+3]);

    smemclr(sched, sizeof(sched));

    /*
     * Now prepare the modified keys for the inverse cipher.
     */
    for (size_t eround = 0; eround <= rounds; eround++) {
        size_t dround = rounds - eround;
        __m128i rkey = keysched_e[eround];
        if (eround && dround)      /* neither first nor last */
            rkey = _mm_aesimc_si128(rkey);
        keysched_d[dround] = rkey;
    }
}

/*
 * Auxiliary routine to increment the 128-bit counter used in SDCTR
 * mode.
 */
static inline __m128i aes_ni_sdctr_increment(__m128i v)
{
    const __m128i ONE  = _mm_setr_epi32(1,0,0,0);
    const __m128i ZERO = _mm_setzero_si128();

    /* Increment the low-order 64 bits of v */
    v  = _mm_add_epi64(v, ONE);
    /* Check if they've become zero */
    __m128i cmp = _mm_cmpeq_epi64(v, ZERO);
    /* If so, the low half of cmp is all 1s. Pack that into the high
     * half of addend with zero in the low half. */
    __m128i addend = _mm_unpacklo_epi64(ZERO, cmp);
    /* And subtract that from v, which increments the high 64 bits iff
     * the low 64 wrapped round. */
    v = _mm_sub_epi64(v, addend);

    return v;
}

/*
 * Auxiliary routine to reverse the byte order of a vector, so that
 * the SDCTR IV can be made big-endian for feeding to the cipher.
 */
static inline __m128i aes_ni_sdctr_reverse(__m128i v)
{
    v = _mm_shuffle_epi8(
        v, _mm_setr_epi8(15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0));
    return v;
}

/*
 * The SSH interface and the cipher modes.
 */

typedef struct aes_ni_context aes_ni_context;
struct aes_ni_context {
    __m128i keysched_e[MAXROUNDKEYS], keysched_d[MAXROUNDKEYS], iv;

    void *pointer_to_free;
    ssh_cipher ciph;
};

static ssh_cipher *aes_ni_new(const ssh_cipheralg *alg)
{
    const struct aes_extra *extra = (const struct aes_extra *)alg->extra;
    if (!check_availability(extra))
        return NULL;

    /*
     * The __m128i variables in the context structure need to be
     * 16-byte aligned, but not all malloc implementations that this
     * code has to work with will guarantee to return a 16-byte
     * aligned pointer. So we over-allocate, manually realign the
     * pointer ourselves, and store the original one inside the
     * context so we know how to free it later.
     */
    void *allocation = smalloc(sizeof(aes_ni_context) + 15);
    uintptr_t alloc_address = (uintptr_t)allocation;
    uintptr_t aligned_address = (alloc_address + 15) & ~15;
    aes_ni_context *ctx = (aes_ni_context *)aligned_address;

    ctx->ciph.vt = alg;
    ctx->pointer_to_free = allocation;
    return &ctx->ciph;
}

static void aes_ni_free(ssh_cipher *ciph)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    void *allocation = ctx->pointer_to_free;
    smemclr(ctx, sizeof(*ctx));
    sfree(allocation);
}

static void aes_ni_setkey(ssh_cipher *ciph, const void *vkey)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    const unsigned char *key = (const unsigned char *)vkey;

    aes_ni_key_expand(key, ctx->ciph.vt->real_keybits / 32,
                      ctx->keysched_e, ctx->keysched_d);
}

static void aes_ni_setiv_cbc(ssh_cipher *ciph, const void *iv)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    ctx->iv = _mm_loadu_si128(iv);
}

static void aes_ni_setiv_sdctr(ssh_cipher *ciph, const void *iv)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);
    __m128i counter = _mm_loadu_si128(iv);
    ctx->iv = aes_ni_sdctr_reverse(counter);
}

typedef __m128i (*aes_ni_fn)(__m128i v, const __m128i *keysched);

static inline void aes_cbc_ni_encrypt(
    ssh_cipher *ciph, void *vblk, int blklen, aes_ni_fn encrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i plaintext = _mm_loadu_si128((const __m128i *)blk);
        __m128i cipher_input = _mm_xor_si128(plaintext, ctx->iv);
        __m128i ciphertext = encrypt(cipher_input, ctx->keysched_e);
        _mm_storeu_si128((__m128i *)blk, ciphertext);
        ctx->iv = ciphertext;
    }
}

static inline void aes_cbc_ni_decrypt(
    ssh_cipher *ciph, void *vblk, int blklen, aes_ni_fn decrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i ciphertext = _mm_loadu_si128((const __m128i *)blk);
        __m128i decrypted = decrypt(ciphertext, ctx->keysched_d);
        __m128i plaintext = _mm_xor_si128(decrypted, ctx->iv);
        _mm_storeu_si128((__m128i *)blk, plaintext);
        ctx->iv = ciphertext;
    }
}

static inline void aes_sdctr_ni(
    ssh_cipher *ciph, void *vblk, int blklen, aes_ni_fn encrypt)
{
    aes_ni_context *ctx = container_of(ciph, aes_ni_context, ciph);

    for (uint8_t *blk = (uint8_t *)vblk, *finish = blk + blklen;
         blk < finish; blk += 16) {
        __m128i counter = aes_ni_sdctr_reverse(ctx->iv);
        __m128i keystream = encrypt(counter, ctx->keysched_e);
        __m128i input = _mm_loadu_si128((const __m128i *)blk);
        __m128i output = _mm_xor_si128(input, keystream);
        _mm_storeu_si128((__m128i *)blk, output);
        ctx->iv = aes_ni_sdctr_increment(ctx->iv);
    }
}

#define NI_ENC_DEC(len)                                                 \
    static void aes##len##_ni_cbc_encrypt(                              \
        ssh_cipher *ciph, void *vblk, int blklen)                       \
    { aes_cbc_ni_encrypt(ciph, vblk, blklen, aes_ni_##len##_e); }       \
    static void aes##len##_ni_cbc_decrypt(                              \
        ssh_cipher *ciph, void *vblk, int blklen)                       \
    { aes_cbc_ni_decrypt(ciph, vblk, blklen, aes_ni_##len##_d); }       \
    static void aes##len##_ni_sdctr(                                    \
        ssh_cipher *ciph, void *vblk, int blklen)                       \
    { aes_sdctr_ni(ciph, vblk, blklen, aes_ni_##len##_e); }             \

NI_ENC_DEC(128)
NI_ENC_DEC(192)
NI_ENC_DEC(256)

AES_EXTRA(_ni);
AES_ALL_VTABLES(_ni, "AES-NI accelerated");
