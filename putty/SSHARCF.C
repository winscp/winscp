/*
 * Arcfour (RC4) implementation for PuTTY.
 *
 * Coded from Schneier.
 */

#include <assert.h>
#include "ssh.h"

typedef struct {
    unsigned char i, j, s[256];
} ArcfourContext;

static void arcfour_block(void *handle, unsigned char *blk, int len)
{
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

static void *arcfour_make_context(void)
{
    return snew(ArcfourContext);
}

static void arcfour_free_context(void *handle)
{
    sfree(handle);
}

static void arcfour_stir(ArcfourContext *ctx)
{
    unsigned char *junk = snewn(1536, unsigned char);
    memset(junk, 0, 1536);
    arcfour_block(ctx, junk, 1536);
    memset(junk, 0, 1536);
    sfree(junk);
}

static void arcfour128_key(void *handle, unsigned char *key)
{
    ArcfourContext *ctx = (ArcfourContext *)handle;
    arcfour_setkey(ctx, key, 16);
    arcfour_stir(ctx);
}

static void arcfour256_key(void *handle, unsigned char *key)
{
    ArcfourContext *ctx = (ArcfourContext *)handle;
    arcfour_setkey(ctx, key, 32);
    arcfour_stir(ctx);
}

static void arcfour_iv(void *handle, unsigned char *key)
{

}

const struct ssh2_cipher ssh_arcfour128_ssh2 = {
    arcfour_make_context, arcfour_free_context, arcfour_iv, arcfour128_key,
    arcfour_block, arcfour_block,
    "arcfour128",
    1, 128, 0, "Arcfour-128"
};

const struct ssh2_cipher ssh_arcfour256_ssh2 = {
    arcfour_make_context, arcfour_free_context, arcfour_iv, arcfour256_key,
    arcfour_block, arcfour_block,
    "arcfour256",
    1, 256, 0, "Arcfour-256"
};

static const struct ssh2_cipher *const arcfour_list[] = {
    &ssh_arcfour256_ssh2,
    &ssh_arcfour128_ssh2,
};

const struct ssh2_ciphers ssh2_arcfour = {
    sizeof(arcfour_list) / sizeof(*arcfour_list),
    arcfour_list
};
