/*
 * Implementation of HMAC (RFC 2104) for PuTTY, in a general form that
 * can wrap any underlying hash function.
 */

#include "ssh.h"

struct hmac {
    const ssh_hashalg *hashalg;
    ssh_hash *h_outer, *h_inner, *h_live;
    bool keyed;
    uint8_t *digest;
    strbuf *text_name;
    ssh2_mac mac;
};

struct hmac_extra {
    const ssh_hashalg *hashalg_base;
    const char *suffix, *annotation;
};

static ssh2_mac *hmac_new(const ssh2_macalg *alg, ssh_cipher *cipher)
{
    struct hmac *ctx = snew(struct hmac);
    const struct hmac_extra *extra = (const struct hmac_extra *)alg->extra;

    ctx->h_outer = ssh_hash_new(extra->hashalg_base);
    /* In case that hashalg was a selector vtable, we'll now switch to
     * using whatever real one it selected, for all future purposes. */
    ctx->hashalg = ssh_hash_alg(ctx->h_outer);
    ctx->h_inner = ssh_hash_new(ctx->hashalg);
    ctx->h_live = ssh_hash_new(ctx->hashalg);
    ctx->keyed = false;

    /*
     * HMAC is not well defined as a wrapper on an absolutely general
     * hash function; it expects that the function it's wrapping will
     * consume data in fixed-size blocks, and it's partially defined
     * in terms of that block size. So we insist that the hash we're
     * given must have defined a meaningful block size.
     */
    assert(ctx->hashalg->blocklen);

    ctx->digest = snewn(ctx->hashalg->hlen, uint8_t);

    ctx->text_name = strbuf_new();
    strbuf_catf(ctx->text_name, "HMAC-%s",
                ctx->hashalg->text_basename, extra->suffix);
    if (extra->annotation || ctx->hashalg->annotation) {
        strbuf_catf(ctx->text_name, " (");
        const char *sep = "";
        if (extra->annotation) {
            strbuf_catf(ctx->text_name, "%s%s", sep, extra->annotation);
            sep = ", ";
        }
        if (ctx->hashalg->annotation) {
            strbuf_catf(ctx->text_name, "%s%s", sep, ctx->hashalg->annotation);
            sep = ", ";
        }
        strbuf_catf(ctx->text_name, ")");
    }

    ctx->mac.vt = alg;
    BinarySink_DELEGATE_INIT(&ctx->mac, ctx->h_live);

    return &ctx->mac;
}

static void hmac_free(ssh2_mac *mac)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);

    ssh_hash_free(ctx->h_outer);
    ssh_hash_free(ctx->h_inner);
    ssh_hash_free(ctx->h_live);
    smemclr(ctx->digest, ctx->hashalg->hlen);
    sfree(ctx->digest);
    strbuf_free(ctx->text_name);

    smemclr(ctx, sizeof(*ctx));
    sfree(ctx);
}

#define PAD_OUTER 0x5C
#define PAD_INNER 0x36

static void hmac_key(ssh2_mac *mac, ptrlen key)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);

    const uint8_t *kp;
    size_t klen;
    strbuf *sb = NULL;

    if (ctx->keyed) {
        /*
         * If we've already been keyed, throw away the existing hash
         * objects and make a fresh pair to put the new key in.
         */
        ssh_hash_free(ctx->h_outer);
        ssh_hash_free(ctx->h_inner);
        ctx->h_outer = ssh_hash_new(ctx->hashalg);
        ctx->h_inner = ssh_hash_new(ctx->hashalg);
    }
    ctx->keyed = true;

    if (key.len > ctx->hashalg->blocklen) {
        /*
         * RFC 2104 section 2: if the key exceeds the block length of
         * the underlying hash, then we start by hashing the key, and
         * use that hash as the 'true' key for the HMAC construction.
         */
        sb = strbuf_new_nm();
        strbuf_append(sb, ctx->hashalg->hlen);

        ssh_hash *htmp = ssh_hash_new(ctx->hashalg);
        put_datapl(htmp, key);
        ssh_hash_final(htmp, sb->u);

        kp = sb->u;
        klen = sb->len;
    } else {
        /*
         * A short enough key is used as is.
         */
        kp = (const uint8_t *)key.ptr;
        klen = key.len;
    }

    if (ctx->h_outer)
        ssh_hash_free(ctx->h_outer);
    if (ctx->h_inner)
        ssh_hash_free(ctx->h_inner);

    ctx->h_outer = ssh_hash_new(ctx->hashalg);
    for (size_t i = 0; i < klen; i++)
        put_byte(ctx->h_outer, PAD_OUTER ^ kp[i]);
    for (size_t i = klen; i < ctx->hashalg->blocklen; i++)
        put_byte(ctx->h_outer, PAD_OUTER);

    ctx->h_inner = ssh_hash_new(ctx->hashalg);
    for (size_t i = 0; i < klen; i++)
        put_byte(ctx->h_inner, PAD_INNER ^ kp[i]);
    for (size_t i = klen; i < ctx->hashalg->blocklen; i++)
        put_byte(ctx->h_inner, PAD_INNER);

    if (sb)
        strbuf_free(sb);
}

static void hmac_start(ssh2_mac *mac)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);

    ssh_hash_free(ctx->h_live);
    ctx->h_live = ssh_hash_copy(ctx->h_inner);
    BinarySink_DELEGATE_INIT(&ctx->mac, ctx->h_live);
}

static void hmac_genresult(ssh2_mac *mac, unsigned char *output)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);
    ssh_hash *htmp;

    /* Leave h_live in place, so that the SSH-2 BPP can continue
     * regenerating test results from different-length prefixes of the
     * packet */
    htmp = ssh_hash_copy(ctx->h_live);
    ssh_hash_final(htmp, ctx->digest);

    htmp = ssh_hash_copy(ctx->h_outer);
    put_data(htmp, ctx->digest, ctx->hashalg->hlen);
    ssh_hash_final(htmp, ctx->digest);

    /*
     * Some instances of HMAC truncate the output hash, so instead of
     * writing it directly to 'output' we wrote it to our own
     * full-length buffer, and now we copy the required amount.
     */
    memcpy(output, ctx->digest, mac->vt->len);
    smemclr(ctx->digest, ctx->hashalg->hlen);
}

static const char *hmac_text_name(ssh2_mac *mac)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);
    return ctx->text_name->s;
}

const struct hmac_extra ssh_hmac_sha256_extra = { &ssh_sha256, "" };
const ssh2_macalg ssh_hmac_sha256 = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-sha2-256", "hmac-sha2-256-etm@openssh.com",
    32, 32, &ssh_hmac_sha256_extra,
};

const struct hmac_extra ssh_hmac_md5_extra = { &ssh_md5, "" };
const ssh2_macalg ssh_hmac_md5 = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-md5", "hmac-md5-etm@openssh.com",
    16, 16, &ssh_hmac_md5_extra,
};

const struct hmac_extra ssh_hmac_sha1_extra = { &ssh_sha1, "" };

const ssh2_macalg ssh_hmac_sha1 = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-sha1", "hmac-sha1-etm@openssh.com",
    20, 20, &ssh_hmac_sha1_extra,
};

const struct hmac_extra ssh_hmac_sha1_96_extra = { &ssh_sha1, "-96" };

const ssh2_macalg ssh_hmac_sha1_96 = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-sha1-96", "hmac-sha1-96-etm@openssh.com",
    12, 20, &ssh_hmac_sha1_96_extra,
};

const struct hmac_extra ssh_hmac_sha1_buggy_extra = {
    &ssh_sha1, " (bug-compatible)"
};

const ssh2_macalg ssh_hmac_sha1_buggy = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-sha1", NULL,
    20, 16, &ssh_hmac_sha1_buggy_extra,
};

const struct hmac_extra ssh_hmac_sha1_96_buggy_extra = {
    &ssh_sha1, "-96 (bug-compatible)"
};

const ssh2_macalg ssh_hmac_sha1_96_buggy = {
    hmac_new, hmac_free, hmac_key,
    hmac_start, hmac_genresult, hmac_text_name,
    "hmac-sha1-96", NULL,
    12, 16, &ssh_hmac_sha1_96_buggy_extra,
};
