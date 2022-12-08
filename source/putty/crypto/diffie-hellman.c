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
    /* Command to recompute, from the expression in RFC 2412 section E.2:
spigot -B16 '2^1024 - 2^960 - 1 + 2^64 * ( floor(2^894 pi) + 129093 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static void dh_group14_construct(dh_ctx *ctx)
{
    /* Command to recompute, from the expression in RFC 3526 section 3:
spigot -B16 '2^2048 - 2^1984 - 1 + 2^64 * ( floor(2^1918 pi) + 124476 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AACAA68FFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static void dh_group15_construct(dh_ctx *ctx)
{
    /* Command to recompute, from the expression in RFC 3526 section 4:
spigot -B16 '2^3072 - 2^3008 - 1 + 2^64 * ( floor(2^2942 pi) + 1690314 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static void dh_group16_construct(dh_ctx *ctx)
{
    /* Command to recompute, from the expression in RFC 3526 section 5:
spigot -B16 '2^4096 - 2^4032 - 1 + 2^64 * ( floor(2^3966 pi) + 240904 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199FFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static void dh_group17_construct(dh_ctx *ctx)
{
    /* Command to recompute, from the expression in RFC 3526 section 6:
spigot -B16 '2^6144 - 2^6080 - 1 + 2^64 * ( floor(2^6014 pi) + 929484 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C93402849236C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AACC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E6DCC4024FFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static void dh_group18_construct(dh_ctx *ctx)
{
    /* Command to recompute, from the expression in RFC 3526 section 7:
spigot -B16 '2^8192 - 2^8128 - 1 + 2^64 * ( floor(2^8062 pi) + 4743158 )'
     */
    ctx->p = MP_LITERAL_WINSCP_STR("0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF6955817183995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200CBBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C93402849236C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AACC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E6DBE115974A3926F12FEE5E438777CB6A932DF8CD8BEC4D073B931BA3BC832B68D9DD300741FA7BF8AFC47ED2576F6936BA424663AAB639C5AE4F5683423B4742BF1C978238F16CBE39D652DE3FDB8BEFC848AD922222E04A4037C0713EB57A81A23F0C73473FC646CEA306B4BCBC8862F8385DDFA9D4B7FA2C087E879683303ED5BDD3A062B3CF5B3A278A66D2A13F83F44F82DDF310EE074AB6A364597E899A0255DC164F31CC50846851DF9AB48195DED7EA1B1D510BD7EE74D73FAF36BC31ECFA268359046F4EB879F924009438B481C6CD7889A002ED5EE382BC9190DA6FC026E479558E4475677E9AA9E3050E2765694DFC81F56E880B96E7160C980DD98EDD3DFFFFFFFFFFFFFFFFF");
    ctx->g = mp_from_integer(2);
}

static const struct dh_extra extra_group1 = {
    false, dh_group1_construct,
};

const ssh_kex ssh_diffiehellman_group1_sha1 = {
    /*.name =*/ "diffie-hellman-group1-sha1",
    /*.groupname =*/ "group1",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_group1,
};

static const ssh_kex *const group1_list[] = {
    &ssh_diffiehellman_group1_sha1
};

const ssh_kexes ssh_diffiehellman_group1 = { lenof(group1_list), group1_list };

static const struct dh_extra extra_group18 = {
    false, dh_group18_construct,
};

const ssh_kex ssh_diffiehellman_group18_sha512 = {
    /*.name =*/ "diffie-hellman-group18-sha512",
    /*.groupname =*/ "group18",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group18,
};

static const ssh_kex *const group18_list[] = {
    &ssh_diffiehellman_group18_sha512,
};

const ssh_kexes ssh_diffiehellman_group18 = {
    lenof(group18_list), group18_list
};

static const struct dh_extra extra_group17 = {
    false, dh_group17_construct,
};

const ssh_kex ssh_diffiehellman_group17_sha512 = {
    /*.name =*/ "diffie-hellman-group17-sha512",
    /*.groupname =*/ "group17",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group17,
};

static const ssh_kex *const group17_list[] = {
    &ssh_diffiehellman_group17_sha512,
};

const ssh_kexes ssh_diffiehellman_group17 = {
    lenof(group17_list), group17_list
};

static const struct dh_extra extra_group16 = {
    false, dh_group16_construct,
};

const ssh_kex ssh_diffiehellman_group16_sha512 = {
    /*.name =*/ "diffie-hellman-group16-sha512",
    /*.groupname =*/ "group16",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group16,
};

static const ssh_kex *const group16_list[] = {
    &ssh_diffiehellman_group16_sha512,
};

const ssh_kexes ssh_diffiehellman_group16 = {
    lenof(group16_list), group16_list
};

static const struct dh_extra extra_group15 = {
    false, dh_group15_construct,
};

const ssh_kex ssh_diffiehellman_group15_sha512 = {
    /*.name =*/ "diffie-hellman-group15-sha512",
    /*.groupname =*/ "group15",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group15,
};

static const ssh_kex *const group15_list[] = {
    &ssh_diffiehellman_group15_sha512,
};

const ssh_kexes ssh_diffiehellman_group15 = {
    lenof(group15_list), group15_list
};

static const struct dh_extra extra_group14 = {
    false, dh_group14_construct,
};

const ssh_kex ssh_diffiehellman_group14_sha256 = {
    /*.name =*/ "diffie-hellman-group14-sha256",
    /*.groupname =*/ "group14",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha256,
    NULL, // WINSCP
    /*.extra =*/ &extra_group14,
};

const ssh_kex ssh_diffiehellman_group14_sha1 = {
    /*.name =*/ "diffie-hellman-group14-sha1",
    /*.groupname =*/ "group14",
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_group14,
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
    /*.name =*/ "diffie-hellman-group-exchange-sha256",
    /*.groupname =*/ NULL,
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha256,
    NULL, // WINSCP
    /*.extra =*/ &extra_gex,
};

static const ssh_kex ssh_diffiehellman_gex_sha1 = {
    /*.name =*/ "diffie-hellman-group-exchange-sha1",
    /*.groupname =*/ NULL,
    /*.main_type =*/ KEXTYPE_DH,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_gex,
};

static const ssh_kex *const gex_list[] = {
    &ssh_diffiehellman_gex_sha256,
    &ssh_diffiehellman_gex_sha1
};

const ssh_kexes ssh_diffiehellman_gex = { lenof(gex_list), gex_list };

static const ssh_kex ssh_gssk5_diffiehellman_gex_sha1 = {
    /*.name =*/ "gss-gex-sha1-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ NULL,
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_gex,
};

static const ssh_kex ssh_gssk5_diffiehellman_group18_sha512 = {
    /*.name =*/ "gss-group18-sha512-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group18",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group18,
};

static const ssh_kex ssh_gssk5_diffiehellman_group17_sha512 = {
    /*.name =*/ "gss-group17-sha512-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group17",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group17,
};

static const ssh_kex ssh_gssk5_diffiehellman_group16_sha512 = {
    /*.name =*/ "gss-group16-sha512-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group16",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group16,
};

static const ssh_kex ssh_gssk5_diffiehellman_group15_sha512 = {
    /*.name =*/ "gss-group15-sha512-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group15",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha512,
    NULL, // WINSCP
    /*.extra =*/ &extra_group15,
};

static const ssh_kex ssh_gssk5_diffiehellman_group14_sha256 = {
    /*.name =*/ "gss-group14-sha256-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group14",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha256,
    NULL, // WINSCP
    /*.extra =*/ &extra_group14,
};

static const ssh_kex *const gssk5_sha2_kex_list[] = {
    &ssh_gssk5_diffiehellman_group16_sha512,
    &ssh_gssk5_diffiehellman_group17_sha512,
    &ssh_gssk5_diffiehellman_group18_sha512,
    &ssh_gssk5_diffiehellman_group15_sha512,
    &ssh_gssk5_diffiehellman_group14_sha256,
};

const ssh_kexes ssh_gssk5_sha2_kex = {
    lenof(gssk5_sha2_kex_list), gssk5_sha2_kex_list
};

static const ssh_kex ssh_gssk5_diffiehellman_group14_sha1 = {
    /*.name =*/ "gss-group14-sha1-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group14",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_group14,
};

static const ssh_kex ssh_gssk5_diffiehellman_group1_sha1 = {
    /*.name =*/ "gss-group1-sha1-" GSS_KRB5_OID_HASH,
    /*.groupname =*/ "group1",
    /*.main_type =*/ KEXTYPE_GSS,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &extra_group1,
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
    pinitassert(!extra->gex);
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
 */
mp_int *dh_create_e(dh_ctx *ctx)
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
        { // WINSCP
        unsigned cmp = mp_cmp_hs(f, pm1);
        mp_free(pm1);
        if (cmp)
            return "f value received is too large";
        } // WINSCP
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
