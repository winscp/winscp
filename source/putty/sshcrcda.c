/*	$OpenBSD: deattack.c,v 1.14 2001/06/23 15:12:18 itojun Exp $	*/

/*
 * Cryptographic attack detector for ssh - source code
 *
 * Copyright (c) 1998 CORE SDI S.A., Buenos Aires, Argentina.
 *
 * All rights reserved. Redistribution and use in source and binary
 * forms, with or without modification, are permitted provided that
 * this copyright notice is retained.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES ARE DISCLAIMED. IN NO EVENT SHALL CORE SDI S.A. BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY OR
 * CONSEQUENTIAL DAMAGES RESULTING FROM THE USE OR MISUSE OF THIS
 * SOFTWARE.
 *
 * Ariel Futoransky <futo@core-sdi.com>
 * <http://www.core-sdi.com>
 * 
 * Modified for use in PuTTY by Simon Tatham
 */

#include <assert.h>
#include "misc.h"
#include "ssh.h"

/* SSH Constants */
#define SSH_MAXBLOCKS	(32 * 1024)
#define SSH_BLOCKSIZE	(8)

/* Hashing constants */
#define HASH_MINSIZE	(8 * 1024)
#define HASH_ENTRYSIZE	(sizeof(uint16_t))
#define HASH_FACTOR(x)	((x)*3/2)
#define HASH_UNUSEDCHAR	(0xff)
#define HASH_UNUSED	(0xffff)
#define HASH_IV     	(0xfffe)

#define HASH_MINBLOCKS	(7*SSH_BLOCKSIZE)

/* Hash function (Input keys are cipher results) */
#define HASH(x)		GET_32BIT_MSB_FIRST(x)

#define CMP(a, b)	(memcmp(a, b, SSH_BLOCKSIZE))

uint8_t ONE[4] = { 1, 0, 0, 0 };
uint8_t ZERO[4] = { 0, 0, 0, 0 };

struct crcda_ctx {
    uint16_t *h;
    uint32_t n;
};

struct crcda_ctx *crcda_make_context(void)
{
    struct crcda_ctx *ret = snew(struct crcda_ctx);
    ret->h = NULL;
    ret->n = HASH_MINSIZE / HASH_ENTRYSIZE;
    return ret;
}

void crcda_free_context(struct crcda_ctx *ctx)
{
    if (ctx) {
	sfree(ctx->h);
	ctx->h = NULL;
	sfree(ctx);
    }
}

static void crc_update(uint32_t *a, const void *b)
{
    *a = crc32_update(*a, make_ptrlen(b, 4));
}

/* detect if a block is used in a particular pattern */
static bool check_crc(const uint8_t *S, const uint8_t *buf,
                      uint32_t len, const uint8_t *IV)
{
    uint32_t crc;
    const uint8_t *c;

    crc = 0;
    if (IV && !CMP(S, IV)) {
        crc_update(&crc, ONE);
        crc_update(&crc, ZERO);
    }
    for (c = buf; c < buf + len; c += SSH_BLOCKSIZE) {
        if (!CMP(S, c)) {
            crc_update(&crc, ONE);
            crc_update(&crc, ZERO);
        } else {
            crc_update(&crc, ZERO);
            crc_update(&crc, ZERO);
        }
    }
    return (crc == 0);
}

/* Detect a crc32 compensation attack on a packet */
bool detect_attack(struct crcda_ctx *ctx,
                   const unsigned char *buf, uint32_t len,
                   const unsigned char *IV)
{
    register uint32_t i, j;
    uint32_t l;
    register const uint8_t *c;
    const uint8_t *d;

    assert(!(len > (SSH_MAXBLOCKS * SSH_BLOCKSIZE) ||
             len % SSH_BLOCKSIZE != 0));
    for (l = ctx->n; l < HASH_FACTOR(len / SSH_BLOCKSIZE); l = l << 2)
        ;

    if (ctx->h == NULL) {
        ctx->n = l;
        ctx->h = snewn(ctx->n, uint16_t);
    } else {
        if (l > ctx->n) {
            ctx->n = l;
            ctx->h = sresize(ctx->h, ctx->n, uint16_t);
        }
    }

    if (len <= HASH_MINBLOCKS) {
        for (c = buf; c < buf + len; c += SSH_BLOCKSIZE) {
            if (IV && (!CMP(c, IV))) {
                if ((check_crc(c, buf, len, IV)))
                    return true;          /* attack detected */
                else
                    break;
            }
            for (d = buf; d < c; d += SSH_BLOCKSIZE) {
                if (!CMP(c, d)) {
                    if ((check_crc(c, buf, len, IV)))
                        return true;      /* attack detected */
                    else
                        break;
                }
            }
        }
        return false;                  /* ok */
    }
    memset(ctx->h, HASH_UNUSEDCHAR, ctx->n * HASH_ENTRYSIZE);

    if (IV)
        ctx->h[HASH(IV) & (ctx->n - 1)] = HASH_IV;

    for (c = buf, j = 0; c < (buf + len); c += SSH_BLOCKSIZE, j++) {
        for (i = HASH(c) & (ctx->n - 1); ctx->h[i] != HASH_UNUSED;
             i = (i + 1) & (ctx->n - 1)) {
            if (ctx->h[i] == HASH_IV) {
                assert(IV); /* or we wouldn't have stored HASH_IV above */
                if (!CMP(c, IV)) {
                    if (check_crc(c, buf, len, IV))
                        return true;      /* attack detected */
                    else
                        break;
                }
            } else if (!CMP(c, buf + ctx->h[i] * SSH_BLOCKSIZE)) {
                if (check_crc(c, buf, len, IV))
                    return true;          /* attack detected */
                else
                    break;
            }
        }
        ctx->h[i] = j;
    }
    return false;                          /* ok */
}
