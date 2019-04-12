#include <assert.h>
#include "ssh.h"

/*
 * MD5 implementation for PuTTY. Written directly from the spec by
 * Simon Tatham.
 */

/* ----------------------------------------------------------------------
 * Core MD5 algorithm: processes 16-word blocks into a message digest.
 */

#define F(x,y,z) ( ((x) & (y)) | ((~(x)) & (z)) )
#define G(x,y,z) ( ((x) & (z)) | ((~(z)) & (y)) )
#define H(x,y,z) ( (x) ^ (y) ^ (z) )
#define I(x,y,z) ( (y) ^ ( (x) | ~(z) ) )

#define rol(x,y) ( ((x) << (y)) | (((uint32)x) >> (32-y)) )

#define subround(f,w,x,y,z,k,s,ti) \
       w = x + rol(w + f(x,y,z) + block[k] + ti, s)

static void MD5_Core_Init(MD5_Core_State * s)
{
    s->h[0] = 0x67452301;
    s->h[1] = 0xefcdab89;
    s->h[2] = 0x98badcfe;
    s->h[3] = 0x10325476;
}

static void MD5_Block(MD5_Core_State * s, uint32 * block)
{
    uint32 a, b, c, d;

    a = s->h[0];
    b = s->h[1];
    c = s->h[2];
    d = s->h[3];

    subround(F, a, b, c, d, 0, 7, 0xd76aa478);
    subround(F, d, a, b, c, 1, 12, 0xe8c7b756);
    subround(F, c, d, a, b, 2, 17, 0x242070db);
    subround(F, b, c, d, a, 3, 22, 0xc1bdceee);
    subround(F, a, b, c, d, 4, 7, 0xf57c0faf);
    subround(F, d, a, b, c, 5, 12, 0x4787c62a);
    subround(F, c, d, a, b, 6, 17, 0xa8304613);
    subround(F, b, c, d, a, 7, 22, 0xfd469501);
    subround(F, a, b, c, d, 8, 7, 0x698098d8);
    subround(F, d, a, b, c, 9, 12, 0x8b44f7af);
    subround(F, c, d, a, b, 10, 17, 0xffff5bb1);
    subround(F, b, c, d, a, 11, 22, 0x895cd7be);
    subround(F, a, b, c, d, 12, 7, 0x6b901122);
    subround(F, d, a, b, c, 13, 12, 0xfd987193);
    subround(F, c, d, a, b, 14, 17, 0xa679438e);
    subround(F, b, c, d, a, 15, 22, 0x49b40821);
    subround(G, a, b, c, d, 1, 5, 0xf61e2562);
    subround(G, d, a, b, c, 6, 9, 0xc040b340);
    subround(G, c, d, a, b, 11, 14, 0x265e5a51);
    subround(G, b, c, d, a, 0, 20, 0xe9b6c7aa);
    subround(G, a, b, c, d, 5, 5, 0xd62f105d);
    subround(G, d, a, b, c, 10, 9, 0x02441453);
    subround(G, c, d, a, b, 15, 14, 0xd8a1e681);
    subround(G, b, c, d, a, 4, 20, 0xe7d3fbc8);
    subround(G, a, b, c, d, 9, 5, 0x21e1cde6);
    subround(G, d, a, b, c, 14, 9, 0xc33707d6);
    subround(G, c, d, a, b, 3, 14, 0xf4d50d87);
    subround(G, b, c, d, a, 8, 20, 0x455a14ed);
    subround(G, a, b, c, d, 13, 5, 0xa9e3e905);
    subround(G, d, a, b, c, 2, 9, 0xfcefa3f8);
    subround(G, c, d, a, b, 7, 14, 0x676f02d9);
    subround(G, b, c, d, a, 12, 20, 0x8d2a4c8a);
    subround(H, a, b, c, d, 5, 4, 0xfffa3942);
    subround(H, d, a, b, c, 8, 11, 0x8771f681);
    subround(H, c, d, a, b, 11, 16, 0x6d9d6122);
    subround(H, b, c, d, a, 14, 23, 0xfde5380c);
    subround(H, a, b, c, d, 1, 4, 0xa4beea44);
    subround(H, d, a, b, c, 4, 11, 0x4bdecfa9);
    subround(H, c, d, a, b, 7, 16, 0xf6bb4b60);
    subround(H, b, c, d, a, 10, 23, 0xbebfbc70);
    subround(H, a, b, c, d, 13, 4, 0x289b7ec6);
    subround(H, d, a, b, c, 0, 11, 0xeaa127fa);
    subround(H, c, d, a, b, 3, 16, 0xd4ef3085);
    subround(H, b, c, d, a, 6, 23, 0x04881d05);
    subround(H, a, b, c, d, 9, 4, 0xd9d4d039);
    subround(H, d, a, b, c, 12, 11, 0xe6db99e5);
    subround(H, c, d, a, b, 15, 16, 0x1fa27cf8);
    subround(H, b, c, d, a, 2, 23, 0xc4ac5665);
    subround(I, a, b, c, d, 0, 6, 0xf4292244);
    subround(I, d, a, b, c, 7, 10, 0x432aff97);
    subround(I, c, d, a, b, 14, 15, 0xab9423a7);
    subround(I, b, c, d, a, 5, 21, 0xfc93a039);
    subround(I, a, b, c, d, 12, 6, 0x655b59c3);
    subround(I, d, a, b, c, 3, 10, 0x8f0ccc92);
    subround(I, c, d, a, b, 10, 15, 0xffeff47d);
    subround(I, b, c, d, a, 1, 21, 0x85845dd1);
    subround(I, a, b, c, d, 8, 6, 0x6fa87e4f);
    subround(I, d, a, b, c, 15, 10, 0xfe2ce6e0);
    subround(I, c, d, a, b, 6, 15, 0xa3014314);
    subround(I, b, c, d, a, 13, 21, 0x4e0811a1);
    subround(I, a, b, c, d, 4, 6, 0xf7537e82);
    subround(I, d, a, b, c, 11, 10, 0xbd3af235);
    subround(I, c, d, a, b, 2, 15, 0x2ad7d2bb);
    subround(I, b, c, d, a, 9, 21, 0xeb86d391);

    s->h[0] += a;
    s->h[1] += b;
    s->h[2] += c;
    s->h[3] += d;
}

/* ----------------------------------------------------------------------
 * Outer MD5 algorithm: take an arbitrary length byte string,
 * convert it into 16-word blocks with the prescribed padding at
 * the end, and pass those blocks to the core MD5 algorithm.
 */

#define BLKSIZE 64

static void MD5_BinarySink_write(BinarySink *bs, const void *data, size_t len);

void MD5Init(struct MD5Context *s)
{
    MD5_Core_Init(&s->core);
    s->blkused = 0;
    s->lenhi = s->lenlo = 0;
    BinarySink_INIT(s, MD5_BinarySink_write);
}

static void MD5_BinarySink_write(BinarySink *bs, const void *data, size_t len)
{
    struct MD5Context *s = BinarySink_DOWNCAST(bs, struct MD5Context);
    const unsigned char *q = (const unsigned char *)data;
    uint32 wordblock[16];
    uint32 lenw = len;
    int i;

    assert(lenw == len);

    /*
     * Update the length field.
     */
    s->lenlo += lenw;
    s->lenhi += (s->lenlo < lenw);

    if (s->blkused + len < BLKSIZE) {
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
	    /* Now process the block. Gather bytes little-endian into words */
	    for (i = 0; i < 16; i++) {
		wordblock[i] =
		    (((uint32) s->block[i * 4 + 3]) << 24) |
		    (((uint32) s->block[i * 4 + 2]) << 16) |
		    (((uint32) s->block[i * 4 + 1]) << 8) |
		    (((uint32) s->block[i * 4 + 0]) << 0);
	    }
	    MD5_Block(&s->core, wordblock);
	    s->blkused = 0;
	}
#ifdef MPEXT
	if (len > 0)
#endif
	memcpy(s->block, q, len);
	s->blkused = len;
    }
}

void MD5Final(unsigned char output[16], struct MD5Context *s)
{
    int i;
    unsigned pad;
    unsigned char c[64];
    uint32 lenhi, lenlo;

    if (s->blkused >= 56)
	pad = 56 + 64 - s->blkused;
    else
	pad = 56 - s->blkused;

    lenhi = (s->lenhi << 3) | (s->lenlo >> (32 - 3));
    lenlo = (s->lenlo << 3);

    memset(c, 0, pad);
    c[0] = 0x80;
    put_data(s, c, pad);

    c[7] = (lenhi >> 24) & 0xFF;
    c[6] = (lenhi >> 16) & 0xFF;
    c[5] = (lenhi >> 8) & 0xFF;
    c[4] = (lenhi >> 0) & 0xFF;
    c[3] = (lenlo >> 24) & 0xFF;
    c[2] = (lenlo >> 16) & 0xFF;
    c[1] = (lenlo >> 8) & 0xFF;
    c[0] = (lenlo >> 0) & 0xFF;

    put_data(s, c, 8);

    for (i = 0; i < 4; i++) {
	output[4 * i + 3] = (s->core.h[i] >> 24) & 0xFF;
	output[4 * i + 2] = (s->core.h[i] >> 16) & 0xFF;
	output[4 * i + 1] = (s->core.h[i] >> 8) & 0xFF;
	output[4 * i + 0] = (s->core.h[i] >> 0) & 0xFF;
    }
}

void MD5Simple(void const *p, unsigned len, unsigned char output[16])
{
    struct MD5Context s;

    MD5Init(&s);
    put_data(&s, (unsigned char const *)p, len);
    MD5Final(output, &s);
    smemclr(&s, sizeof(s));
}

/* ----------------------------------------------------------------------
 * The above is the MD5 algorithm itself. Now we implement the
 * HMAC wrapper on it.
 * 
 * Some of these functions are exported directly, because they are
 * useful elsewhere (SOCKS5 CHAP authentication uses HMAC-MD5).
 */

struct hmacmd5_context {
    struct MD5Context md5[3];
    ssh2_mac mac;
};

struct hmacmd5_context *hmacmd5_make_context(void)
{
    struct hmacmd5_context *ctx = snew(struct hmacmd5_context);
    BinarySink_DELEGATE_INIT(&ctx->mac, &ctx->md5[2]);
    return ctx;
}

static ssh2_mac *hmacmd5_ssh2_new(const struct ssh2_macalg *alg,
                                  ssh2_cipher *cipher)
{
    struct hmacmd5_context *ctx = hmacmd5_make_context();
    ctx->mac.vt = alg;
    return &ctx->mac;
}

void hmacmd5_free_context(struct hmacmd5_context *ctx)
{
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void hmacmd5_ssh2_free(ssh2_mac *mac)
{
    struct hmacmd5_context *ctx =
        container_of(mac, struct hmacmd5_context, mac);
    hmacmd5_free_context(ctx);
}

void hmacmd5_key(struct hmacmd5_context *ctx, void const *keyv, int len)
{
    unsigned char foo[64];
    unsigned char const *key = (unsigned char const *)keyv;
    int i;

    memset(foo, 0x36, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    MD5Init(&ctx->md5[0]);
    put_data(&ctx->md5[0], foo, 64);

    memset(foo, 0x5C, 64);
    for (i = 0; i < len && i < 64; i++)
	foo[i] ^= key[i];
    MD5Init(&ctx->md5[1]);
    put_data(&ctx->md5[1], foo, 64);

    smemclr(foo, 64);		       /* burn the evidence */
}

static void hmacmd5_ssh2_setkey(ssh2_mac *mac, const void *key)
{
    struct hmacmd5_context *ctx =
        container_of(mac, struct hmacmd5_context, mac);
    hmacmd5_key(ctx, key, ctx->mac.vt->keylen);
}

static void hmacmd5_start(ssh2_mac *mac)
{
    struct hmacmd5_context *ctx =
        container_of(mac, struct hmacmd5_context, mac);

    ctx->md5[2] = ctx->md5[0]; /* structure copy */
    BinarySink_COPIED(&ctx->md5[2]);
}

static void hmacmd5_genresult(ssh2_mac *mac, unsigned char *hmac)
{
    struct hmacmd5_context *ctx =
        container_of(mac, struct hmacmd5_context, mac);
    struct MD5Context s;
    unsigned char intermediate[16];

    s = ctx->md5[2];              /* structure copy */
    BinarySink_COPIED(&s);
    MD5Final(intermediate, &s);
    s = ctx->md5[1];              /* structure copy */
    BinarySink_COPIED(&s);
    put_data(&s, intermediate, 16);
    MD5Final(hmac, &s);
    smemclr(intermediate, sizeof(intermediate));
}

void hmacmd5_do_hmac(struct hmacmd5_context *ctx,
                     const void *blk, int len, unsigned char *hmac)
{
    ssh2_mac_start(&ctx->mac);
    put_data(&ctx->mac, blk, len);
    /*WINSCP return*/ ssh2_mac_genresult(&ctx->mac, hmac);
}

const struct ssh2_macalg ssh_hmac_md5 = {
    hmacmd5_ssh2_new, hmacmd5_ssh2_free, hmacmd5_ssh2_setkey,
    hmacmd5_start, hmacmd5_genresult,
    "hmac-md5", "hmac-md5-etm@openssh.com",
    16, 16,
    "HMAC-MD5"
};
