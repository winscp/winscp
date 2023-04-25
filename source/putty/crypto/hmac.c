/*
 * Implementation of HMAC (RFC 2104) for PuTTY, in a general form that
 * can wrap any underlying hash function.
 */

#include "ssh.h"

struct hmac {
    const ssh_hashalg *hashalg;
    ssh_hash *h_outer, *h_inner, *h_live;
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
    put_fmt(ctx->text_name, "HMAC-%s%s",
            ctx->hashalg->text_basename, extra->suffix);
    if (extra->annotation || ctx->hashalg->annotation) {
        put_fmt(ctx->text_name, " (");
        const char *sep = "";
        if (extra->annotation) {
            put_fmt(ctx->text_name, "%s%s", sep, extra->annotation);
            sep = ", ";
        }
        if (ctx->hashalg->annotation) {
            put_fmt(ctx->text_name, "%s%s", sep, ctx->hashalg->annotation);
            sep = ", ";
        }
        put_fmt(ctx->text_name, ")");
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

    if (key.len > ctx->hashalg->blocklen) {
        /*
         * RFC 2104 section 2: if the key exceeds the block length of
         * the underlying hash, then we start by hashing the key, and
         * use that hash as the 'true' key for the HMAC construction.
         */
        sb = strbuf_new_nm();
        strbuf_append(sb, ctx->hashalg->hlen);
        hash_simple(ctx->hashalg, key, sb->u);
        kp = sb->u;
        klen = sb->len;
    } else {
        /*
         * A short enough key is used as is.
         */
        kp = (const uint8_t *)key.ptr;
        klen = key.len;
    }

    ssh_hash_reset(ctx->h_outer);
    for (size_t i = 0; i < klen; i++)
        put_byte(ctx->h_outer, PAD_OUTER ^ kp[i]);
    for (size_t i = klen; i < ctx->hashalg->blocklen; i++)
        put_byte(ctx->h_outer, PAD_OUTER);

    ssh_hash_reset(ctx->h_inner);
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
    ssh_hash_copyfrom(ctx->h_live, ctx->h_inner);
}

static void hmac_genresult(ssh2_mac *mac, unsigned char *output)
{
    struct hmac *ctx = container_of(mac, struct hmac, mac);
    ssh_hash *htmp;

    /* Leave h_live and h_outer in place, so that the SSH-2 BPP can
     * continue regenerating test results from different-length
     * prefixes of the packet */
    ssh_hash_digest_nondestructive(ctx->h_live, ctx->digest);

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

static const struct hmac_extra ssh_hmac_sha256_extra = { &ssh_sha256, "" };
const ssh2_macalg ssh_hmac_sha256 = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-sha2-256",
    .etm_name = "hmac-sha2-256-etm@openssh.com",
    .len = 32,
    .keylen = 32,
    .extra = &ssh_hmac_sha256_extra,
};

static const struct hmac_extra ssh_hmac_md5_extra = { &ssh_md5, "" };
const ssh2_macalg ssh_hmac_md5 = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-md5",
    .etm_name = "hmac-md5-etm@openssh.com",
    .len = 16,
    .keylen = 16,
    .extra = &ssh_hmac_md5_extra,
};

static const struct hmac_extra ssh_hmac_sha1_extra = { &ssh_sha1, "" };

const ssh2_macalg ssh_hmac_sha1 = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-sha1",
    .etm_name = "hmac-sha1-etm@openssh.com",
    .len = 20,
    .keylen = 20,
    .extra = &ssh_hmac_sha1_extra,
};

static const struct hmac_extra ssh_hmac_sha1_96_extra = { &ssh_sha1, "-96" };

const ssh2_macalg ssh_hmac_sha1_96 = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-sha1-96",
    .etm_name = "hmac-sha1-96-etm@openssh.com",
    .len = 12,
    .keylen = 20,
    .extra = &ssh_hmac_sha1_96_extra,
};

static const struct hmac_extra ssh_hmac_sha1_buggy_extra = {
    &ssh_sha1, "", "bug-compatible"
};

const ssh2_macalg ssh_hmac_sha1_buggy = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-sha1",
    .len = 20,
    .keylen = 16,
    .extra = &ssh_hmac_sha1_buggy_extra,
};

static const struct hmac_extra ssh_hmac_sha1_96_buggy_extra = {
    &ssh_sha1, "-96", "bug-compatible"
};

const ssh2_macalg ssh_hmac_sha1_96_buggy = {
    .new = hmac_new,
    .free = hmac_free,
    .setkey = hmac_key,
    .start = hmac_start,
    .genresult = hmac_genresult,
    .text_name = hmac_text_name,
    .name = "hmac-sha1-96",
    .len = 12,
    .keylen = 16,
    .extra = &ssh_hmac_sha1_96_buggy_extra,
};
