/*
 * Diffie-Hellman implementation for PuTTY.
 */

#include <assert.h>

#include "ssh.h"
#include "misc.h"
#include "mpint.h"

struct dh_ctx {
    mp_int *x, *e, *p, *q, *g;
};

struct dh_extra {
    bool gex;
    void (*construct)(dh_ctx *ctx);
};

static void dh_group1_construct(dh_ctx *ctx)
{
    ctx->p = MP_LITERAL(0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF);
    ctx->g = mp_from_integer(2);
}

static void dh_group14_construct(dh_ctx *ctx)
{
    ctx->p = MP_LITERAL(0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF);
    ctx->g = mp_from_integer(2);
}

static const struct dh_extra extra_group1 = {
    false, dh_group1_construct,
};

static const ssh_kex ssh_diffiehellman_group1_sha1 = {
    "diffie-hellman-group1-sha1", "group1",
    KEXTYPE_DH, &ssh_sha1, &extra_group1,
};

static const ssh_kex *const group1_list[] = {
    &ssh_diffiehellman_group1_sha1
};

const ssh_kexes ssh_diffiehellman_group1 = { lenof(group1_list), group1_list };

static const struct dh_extra extra_group14 = {
    false, dh_group14_construct,
};

static const ssh_kex ssh_diffiehellman_group14_sha256 = {
    "diffie-hellman-group14-sha256", "group14",
    KEXTYPE_DH, &ssh_sha256, &extra_group14,
};

static const ssh_kex ssh_diffiehellman_group14_sha1 = {
    "diffie-hellman-group14-sha1", "group14",
    KEXTYPE_DH, &ssh_sha1, &extra_group14,
};

static const ssh_kex *const group14_list[] = {
    &ssh_diffiehellman_group14_sha256,
    &ssh_diffiehellman_group14_sha1
};

const ssh_kexes ssh_diffiehellman_group14 = {
    lenof(group14_list), group14_list
};

static const struct dh_extra extra_gex = { true };

static const ssh_kex ssh_diffiehellman_gex_sha256 = {
    "diffie-hellman-group-exchange-sha256", NULL,
    KEXTYPE_DH, &ssh_sha256, &extra_gex,
};

static const ssh_kex ssh_diffiehellman_gex_sha1 = {
    "diffie-hellman-group-exchange-sha1", NULL,
    KEXTYPE_DH, &ssh_sha1, &extra_gex,
};

static const ssh_kex *const gex_list[] = {
    &ssh_diffiehellman_gex_sha256,
    &ssh_diffiehellman_gex_sha1
};

const ssh_kexes ssh_diffiehellman_gex = { lenof(gex_list), gex_list };

/*
 * Suffix on GSSAPI SSH protocol identifiers that indicates Kerberos 5
 * as the mechanism.
 *
 * This suffix is the base64-encoded MD5 hash of the byte sequence
 * 06 09 2A 86 48 86 F7 12 01 02 02, which in turn is the ASN.1 DER
 * encoding of the object ID 1.2.840.113554.1.2.2 which designates
 * Kerberos v5.
 *
 * (The same encoded OID, minus the two-byte DER header, is defined in
 * pgssapi.c as GSS_MECH_KRB5.)
 */
#define GSS_KRB5_OID_HASH "toWM5Slw5Ew8Mqkay+al2g=="

static const ssh_kex ssh_gssk5_diffiehellman_gex_sha1 = {
    "gss-gex-sha1-" GSS_KRB5_OID_HASH, NULL,
    KEXTYPE_GSS, &ssh_sha1, &extra_gex,
};

static const ssh_kex ssh_gssk5_diffiehellman_group14_sha1 = {
    "gss-group14-sha1-" GSS_KRB5_OID_HASH, "group14",
    KEXTYPE_GSS, &ssh_sha1, &extra_group14,
};

static const ssh_kex ssh_gssk5_diffiehellman_group1_sha1 = {
    "gss-group1-sha1-" GSS_KRB5_OID_HASH, "group1",
    KEXTYPE_GSS, &ssh_sha1, &extra_group1,
};

static const ssh_kex *const gssk5_sha1_kex_list[] = {
    &ssh_gssk5_diffiehellman_gex_sha1,
    &ssh_gssk5_diffiehellman_group14_sha1,
    &ssh_gssk5_diffiehellman_group1_sha1
};

const ssh_kexes ssh_gssk5_sha1_kex = {
    lenof(gssk5_sha1_kex_list), gssk5_sha1_kex_list
};

/*
 * Common DH initialisation.
 */
static void dh_init(dh_ctx *ctx)
{
    ctx->q = mp_rshift_fixed(ctx->p, 1);
    ctx->x = ctx->e = NULL;
}

bool dh_is_gex(const ssh_kex *kex)
{
    const struct dh_extra *extra = (const struct dh_extra *)kex->extra;
    return extra->gex;
}

/*
 * Initialise DH for a standard group.
 */
dh_ctx *dh_setup_group(const ssh_kex *kex)
{
    const struct dh_extra *extra = (const struct dh_extra *)kex->extra;
    assert(!extra->gex);
    dh_ctx *ctx = snew(dh_ctx);
    extra->construct(ctx);
    dh_init(ctx);
    return ctx;
}

/*
 * Initialise DH for a server-supplied group.
 */
dh_ctx *dh_setup_gex(mp_int *pval, mp_int *gval)
{
    dh_ctx *ctx = snew(dh_ctx);
    ctx->p = mp_copy(pval);
    ctx->g = mp_copy(gval);
    dh_init(ctx);
    return ctx;
}

/*
 * Return size of DH modulus p.
 */
int dh_modulus_bit_size(const dh_ctx *ctx)
{
    return mp_get_nbits(ctx->p);
}

/*
 * Clean up and free a context.
 */
void dh_cleanup(dh_ctx *ctx)
{
    if (ctx->x)
        mp_free(ctx->x);
    if (ctx->e)
        mp_free(ctx->e);
    if (ctx->p)
        mp_free(ctx->p);
    if (ctx->g)
        mp_free(ctx->g);
    if (ctx->q)
        mp_free(ctx->q);
    sfree(ctx);
}

/*
 * DH stage 1: invent a number x between 1 and q, and compute e =
 * g^x mod p. Return e.
 * 
 * If `nbits' is greater than zero, it is used as an upper limit
 * for the number of bits in x. This is safe provided that (a) you
 * use twice as many bits in x as the number of bits you expect to
 * use in your session key, and (b) the DH group is a safe prime
 * (which SSH demands that it must be).
 * 
 * P. C. van Oorschot, M. J. Wiener
 * "On Diffie-Hellman Key Agreement with Short Exponents".
 * Advances in Cryptology: Proceedings of Eurocrypt '96
 * Springer-Verlag, May 1996.
 */
mp_int *dh_create_e(dh_ctx *ctx, int nbits)
{
    /*
     * Lower limit is just 2.
     */
    mp_int *lo = mp_from_integer(2);

    /*
     * Upper limit.
     */
    mp_int *hi = mp_copy(ctx->q);
    mp_sub_integer_into(hi, hi, 1);
    if (nbits) {
        mp_int *pow2 = mp_power_2(nbits+1);
        mp_min_into(pow2, pow2, hi);
        mp_free(hi);
        hi = pow2;
    }

    /*
     * Make a random number in that range.
     */
    ctx->x = mp_random_in_range(lo, hi);
    mp_free(lo);
    mp_free(hi);

    /*
     * Now compute e = g^x mod p.
     */
    ctx->e = mp_modpow(ctx->g, ctx->x, ctx->p);

    return ctx->e;
}

/*
 * DH stage 2-epsilon: given a number f, validate it to ensure it's in
 * range. (RFC 4253 section 8: "Values of 'e' or 'f' that are not in
 * the range [1, p-1] MUST NOT be sent or accepted by either side."
 * Also, we rule out 1 and p-1 too, since that's easy to do and since
 * they lead to obviously weak keys that even a passive eavesdropper
 * can figure out.)
 */
const char *dh_validate_f(dh_ctx *ctx, mp_int *f)
{
    if (!mp_hs_integer(f, 2)) {
        return "f value received is too small";
    } else {
        mp_int *pm1 = mp_copy(ctx->p);
        mp_sub_integer_into(pm1, pm1, 1);
        unsigned cmp = mp_cmp_hs(f, pm1);
        mp_free(pm1);
        if (cmp)
            return "f value received is too large";
    }
    return NULL;
}

/*
 * DH stage 2: given a number f, compute K = f^x mod p.
 */
mp_int *dh_find_K(dh_ctx *ctx, mp_int *f)
{
    return mp_modpow(f, ctx->x, ctx->p);
}
