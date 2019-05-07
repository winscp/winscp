/*
 * Arcfour (RC4) implementation for PuTTY.
 *
 * Coded from Schneier.
 */

#include <assert.h>
#include "ssh.h"

typedef struct {
    unsigned char i, j, s[256];
    ssh_cipher ciph;
} ArcfourContext;

static void arcfour_block(void *handle, void *vblk, int len)
{
    unsigned char *blk = (unsigned char *)vblk;
    ArcfourContext *ctx = (ArcfourContext *)handle;
    unsigned k;
    unsigned char tmp, i, j, *s;

    s = ctx->s;
    i = ctx->i; j = ctx->j;
    for (k = 0; (int)k < len; k++) {
	i  = (i + 1) & 0xff;
	j  = (j + s[i]) & 0xff;
	tmp = s[i]; s[i] = s[j]; s[j] = tmp;
	blk[k] ^= s[(s[i]+s[j]) & 0xff];
    }
    ctx->i = i; ctx->j = j;
}

static void arcfour_setkey(ArcfourContext *ctx, unsigned char const *key,
			   unsigned keybytes)
{
    unsigned char tmp, k[256], *s;
    unsigned i, j;

    s = ctx->s;
    assert(keybytes <= 256);
    ctx->i = ctx->j = 0;
    for (i = 0; i < 256; i++) {
	s[i] = i;
	k[i] = key[i % keybytes];
    }
    j = 0;
    for (i = 0; i < 256; i++) {
	j = (j + s[i] + k[i]) & 0xff;
	tmp = s[i]; s[i] = s[j]; s[j] = tmp;
    }
}

/* -- Interface with PuTTY -- */

/*
 * We don't implement Arcfour in SSH-1 because it's utterly insecure in
 * several ways.  See CERT Vulnerability Notes VU#25309, VU#665372,
 * and VU#565052.
 * 
 * We don't implement the "arcfour" algorithm in SSH-2 because it doesn't
 * stir the cipher state before emitting keystream, and hence is likely
 * to leak data about the key.
 */

static ssh_cipher *arcfour_new(const ssh_cipheralg *alg)
{
    ArcfourContext *ctx = snew(ArcfourContext);
    ctx->ciph.vt = alg;
    return &ctx->ciph;
}

static void arcfour_free(ssh_cipher *cipher)
{
    ArcfourContext *ctx = container_of(cipher, ArcfourContext, ciph);
    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

static void arcfour_stir(ArcfourContext *ctx)
{
    unsigned char *junk = snewn(1536, unsigned char);
    memset(junk, 0, 1536);
    arcfour_block(ctx, junk, 1536);
    smemclr(junk, 1536);
    sfree(junk);
}

static void arcfour_ssh2_setiv(ssh_cipher *cipher, const void *key)
{
    /* As a pure stream cipher, Arcfour has no IV separate from the key */
}

static void arcfour_ssh2_setkey(ssh_cipher *cipher, const void *key)
{
    ArcfourContext *ctx = container_of(cipher, ArcfourContext, ciph);
    arcfour_setkey(ctx, key, ctx->ciph.vt->padded_keybytes);
    arcfour_stir(ctx);
}

static void arcfour_ssh2_block(ssh_cipher *cipher, void *blk, int len)
{
    ArcfourContext *ctx = container_of(cipher, ArcfourContext, ciph);
    arcfour_block(ctx, blk, len);
}

const ssh_cipheralg ssh_arcfour128_ssh2 = {
    arcfour_new, arcfour_free, arcfour_ssh2_setiv, arcfour_ssh2_setkey,
    arcfour_ssh2_block, arcfour_ssh2_block, NULL, NULL,
    "arcfour128",
    1, 128, 16, 0, "Arcfour-128",
    NULL
};

const ssh_cipheralg ssh_arcfour256_ssh2 = {
    arcfour_new, arcfour_free, arcfour_ssh2_setiv, arcfour_ssh2_setkey,
    arcfour_ssh2_block, arcfour_ssh2_block, NULL, NULL,
    "arcfour256",
    1, 256, 32, 0, "Arcfour-256",
    NULL
};

static const ssh_cipheralg *const arcfour_list[] = {
    &ssh_arcfour256_ssh2,
    &ssh_arcfour128_ssh2,
};

const ssh2_ciphers ssh2_arcfour = { lenof(arcfour_list), arcfour_list };
