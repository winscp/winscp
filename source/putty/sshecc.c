/*
 * Elliptic-curve crypto module for PuTTY
 * Implements the three required curves, no optional curves
 *
 * NOTE: Only curves on prime field are handled by the maths functions
 *       in Weierstrass form using Jacobian co-ordinates.
 *
 *       Montgomery form curves are supported for DH. (Curve25519)
 *
 *       Edwards form curves are supported for DSA. (Ed25519)
 */

/*
 * References:
 *
 * Elliptic curves in SSH are specified in RFC 5656:
 *   http://tools.ietf.org/html/rfc5656
 *
 * That specification delegates details of public key formatting and a
 * lot of underlying mechanism to SEC 1:
 *   http://www.secg.org/sec1-v2.pdf
 *
 * Montgomery maths from:
 * Handbook of elliptic and hyperelliptic curve cryptography, Chapter 13
 *   http://cs.ucsb.edu/~koc/ccs130h/2013/EllipticHyperelliptic-CohenFrey.pdf
 *
 * Curve25519 spec from libssh (with reference to other things in the
 * libssh code):
 *   https://git.libssh.org/users/aris/libssh.git/tree/doc/curve25519-sha256@libssh.org.txt
 *
 * Edwards DSA:
 *   http://ed25519.cr.yp.to/ed25519-20110926.pdf
 */

#include <stdlib.h>
#include <assert.h>

#include "ssh.h"
#include "mpint.h"
#include "ecc.h"

/* ----------------------------------------------------------------------
 * Elliptic curve definitions
 */

static void initialise_common(
    struct ec_curve *curve, EllipticCurveType type, mp_int *p)
{
    curve->type = type;
    curve->p = mp_copy(p);
    curve->fieldBits = mp_get_nbits(p);
    curve->fieldBytes = (curve->fieldBits + 7) / 8;
}

static void initialise_wcurve(
    struct ec_curve *curve, mp_int *p, mp_int *a, mp_int *b,
    mp_int *nonsquare, mp_int *G_x, mp_int *G_y, mp_int *G_order)
{
    initialise_common(curve, EC_WEIERSTRASS, p);

    curve->w.wc = ecc_weierstrass_curve(p, a, b, nonsquare);

    curve->w.G = ecc_weierstrass_point_new(curve->w.wc, G_x, G_y);
    curve->w.G_order = mp_copy(G_order);
}

static void initialise_mcurve(
    struct ec_curve *curve, mp_int *p, mp_int *a, mp_int *b,
    mp_int *G_x)
{
    initialise_common(curve, EC_MONTGOMERY, p);

    curve->m.mc = ecc_montgomery_curve(p, a, b);

    curve->m.G = ecc_montgomery_point_new(curve->m.mc, G_x);
}

static void initialise_ecurve(
    struct ec_curve *curve, mp_int *p, mp_int *d, mp_int *a,
    mp_int *nonsquare, mp_int *G_x, mp_int *G_y, mp_int *G_order)
{
    initialise_common(curve, EC_EDWARDS, p);

    curve->e.ec = ecc_edwards_curve(p, d, a, nonsquare);

    curve->e.G = ecc_edwards_point_new(curve->e.ec, G_x, G_y);
    curve->e.G_order = mp_copy(G_order);
}

static struct ec_curve *ec_p256(void)
{
    static struct ec_curve curve = { 0 };
    static bool initialised = false;

    if (!initialised)
    {
        mp_int *p = MP_LITERAL(0xffffffff00000001000000000000000000000000ffffffffffffffffffffffff);
        mp_int *a = MP_LITERAL(0xffffffff00000001000000000000000000000000fffffffffffffffffffffffc);
        mp_int *b = MP_LITERAL(0x5ac635d8aa3a93e7b3ebbd55769886bc651d06b0cc53b0f63bce3c3e27d2604b);
        mp_int *G_x = MP_LITERAL(0x6b17d1f2e12c4247f8bce6e563a440f277037d812deb33a0f4a13945d898c296);
        mp_int *G_y = MP_LITERAL(0x4fe342e2fe1a7f9b8ee7eb4a7c0f9e162bce33576b315ececbb6406837bf51f5);
        mp_int *G_order = MP_LITERAL(0xffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551);
        mp_int *nonsquare_mod_p = mp_from_integer(3);
        initialise_wcurve(&curve, p, a, b, nonsquare_mod_p, G_x, G_y, G_order);
        mp_free(p);
        mp_free(a);
        mp_free(b);
        mp_free(G_x);
        mp_free(G_y);
        mp_free(G_order);
        mp_free(nonsquare_mod_p);

        curve.textname = curve.name = "nistp256";

        /* Now initialised, no need to do it again */
        initialised = true;
    }

    return &curve;
}

static struct ec_curve *ec_p384(void)
{
    static struct ec_curve curve = { 0 };
    static bool initialised = false;

    if (!initialised)
    {
        mp_int *p = MP_LITERAL(0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000ffffffff);
        mp_int *a = MP_LITERAL(0xfffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffeffffffff0000000000000000fffffffc);
        mp_int *b = MP_LITERAL(0xb3312fa7e23ee7e4988e056be3f82d19181d9c6efe8141120314088f5013875ac656398d8a2ed19d2a85c8edd3ec2aef);
        mp_int *G_x = MP_LITERAL(0xaa87ca22be8b05378eb1c71ef320ad746e1d3b628ba79b9859f741e082542a385502f25dbf55296c3a545e3872760ab7);
        mp_int *G_y = MP_LITERAL(0x3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f);
        mp_int *G_order = MP_LITERAL(0xffffffffffffffffffffffffffffffffffffffffffffffffc7634d81f4372ddf581a0db248b0a77aecec196accc52973);
        mp_int *nonsquare_mod_p = mp_from_integer(19);
        initialise_wcurve(&curve, p, a, b, nonsquare_mod_p, G_x, G_y, G_order);
        mp_free(p);
        mp_free(a);
        mp_free(b);
        mp_free(G_x);
        mp_free(G_y);
        mp_free(G_order);
        mp_free(nonsquare_mod_p);

        curve.textname = curve.name = "nistp384";

        /* Now initialised, no need to do it again */
        initialised = true;
    }

    return &curve;
}

static struct ec_curve *ec_p521(void)
{
    static struct ec_curve curve = { 0 };
    static bool initialised = false;

    if (!initialised)
    {
        mp_int *p = MP_LITERAL(0x01ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff);
        mp_int *a = MP_LITERAL(0x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffc);
        mp_int *b = MP_LITERAL(0x0051953eb9618e1c9a1f929a21a0b68540eea2da725b99b315f3b8b489918ef109e156193951ec7e937b1652c0bd3bb1bf073573df883d2c34f1ef451fd46b503f00);
        mp_int *G_x = MP_LITERAL(0x00c6858e06b70404e9cd9e3ecb662395b4429c648139053fb521f828af606b4d3dbaa14b5e77efe75928fe1dc127a2ffa8de3348b3c1856a429bf97e7e31c2e5bd66);
        mp_int *G_y = MP_LITERAL(0x011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650);
        mp_int *G_order = MP_LITERAL(0x01fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffa51868783bf2f966b7fcc0148f709a5d03bb5c9b8899c47aebb6fb71e91386409);
        mp_int *nonsquare_mod_p = mp_from_integer(3);
        initialise_wcurve(&curve, p, a, b, nonsquare_mod_p, G_x, G_y, G_order);
        mp_free(p);
        mp_free(a);
        mp_free(b);
        mp_free(G_x);
        mp_free(G_y);
        mp_free(G_order);
        mp_free(nonsquare_mod_p);

        curve.textname = curve.name = "nistp521";

        /* Now initialised, no need to do it again */
        initialised = true;
    }

    return &curve;
}

static struct ec_curve *ec_curve25519(void)
{
    static struct ec_curve curve = { 0 };
    static bool initialised = false;

    if (!initialised)
    {
        mp_int *p = MP_LITERAL(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed);
        mp_int *a = MP_LITERAL(0x0000000000000000000000000000000000000000000000000000000000076d06);
        mp_int *b = MP_LITERAL(0x0000000000000000000000000000000000000000000000000000000000000001);
        mp_int *G_x = MP_LITERAL(0x0000000000000000000000000000000000000000000000000000000000000009);
        initialise_mcurve(&curve, p, a, b, G_x);
        mp_free(p);
        mp_free(a);
        mp_free(b);
        mp_free(G_x);

        /* This curve doesn't need a name, because it's never used in
         * any format that embeds the curve name */
        curve.name = NULL;
        curve.textname = "Curve25519";

        /* Now initialised, no need to do it again */
        initialised = true;
    }

    return &curve;
}

static struct ec_curve *ec_ed25519(void)
{
    static struct ec_curve curve = { 0 };
    static bool initialised = false;

    if (!initialised)
    {
        mp_int *p = MP_LITERAL(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffed);
        mp_int *d = MP_LITERAL(0x52036cee2b6ffe738cc740797779e89800700a4d4141d8ab75eb4dca135978a3);
        mp_int *a = MP_LITERAL(0x7fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffec); /* == p-1 */
        mp_int *G_x = MP_LITERAL(0x216936d3cd6e53fec0a4e231fdd6dc5c692cc7609525a7b2c9562d608f25d51a);
        mp_int *G_y = MP_LITERAL(0x6666666666666666666666666666666666666666666666666666666666666658);
        mp_int *G_order = MP_LITERAL(0x1000000000000000000000000000000014def9dea2f79cd65812631a5cf5d3ed);
        mp_int *nonsquare_mod_p = mp_from_integer(2);
        initialise_ecurve(&curve, p, d, a, nonsquare_mod_p, G_x, G_y, G_order);
        mp_free(p);
        mp_free(d);
        mp_free(a);
        mp_free(G_x);
        mp_free(G_y);
        mp_free(G_order);
        mp_free(nonsquare_mod_p);

        /* This curve doesn't need a name, because it's never used in
         * any format that embeds the curve name */
        curve.name = NULL;

        curve.textname = "Ed25519";

        /* Now initialised, no need to do it again */
        initialised = true;
    }

    return &curve;
}

/* ----------------------------------------------------------------------
 * Public point from private
 */

struct ecsign_extra {
    struct ec_curve *(*curve)(void);
    const ssh_hashalg *hash;

    /* These fields are used by the OpenSSH PEM format importer/exporter */
    const unsigned char *oid;
    int oidlen;
};

WeierstrassPoint *ecdsa_public(mp_int *private_key, const ssh_keyalg *alg)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_WEIERSTRASS);

    mp_int *priv_reduced = mp_mod(private_key, curve->p);
    WeierstrassPoint *toret = ecc_weierstrass_multiply(
        curve->w.G, priv_reduced);
    mp_free(priv_reduced);
    return toret;
}

static mp_int *eddsa_exponent_from_hash(
    ptrlen hash, const struct ec_curve *curve)
{
    /*
     * Make an integer out of the hash data, little-endian.
     */
    assert(hash.len >= curve->fieldBytes);
    mp_int *e = mp_from_bytes_le(make_ptrlen(hash.ptr, curve->fieldBytes));

    /*
     * Set the highest bit that fits in the modulus, and clear any
     * above that.
     */
    mp_set_bit(e, curve->fieldBits - 1, 1);
    mp_reduce_mod_2to(e, curve->fieldBits);

    /*
     * Clear exactly three low bits.
     */
    for (size_t bit = 0; bit < 3; bit++)
        mp_set_bit(e, bit, 0);

    return e;
}

EdwardsPoint *eddsa_public(mp_int *private_key, const ssh_keyalg *alg)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_EDWARDS);

    ssh_hash *h = ssh_hash_new(extra->hash);
    for (size_t i = 0; i < curve->fieldBytes; ++i)
        put_byte(h, mp_get_byte(private_key, i));

    unsigned char hash[MAX_HASH_LEN];
    ssh_hash_final(h, hash);

    mp_int *exponent = eddsa_exponent_from_hash(
        make_ptrlen(hash, extra->hash->hlen), curve);

    EdwardsPoint *toret = ecc_edwards_multiply(curve->e.G, exponent);
    mp_free(exponent);

    return toret;
}

/* ----------------------------------------------------------------------
 * Marshalling and unmarshalling functions
 */

static mp_int *BinarySource_get_mp_le(BinarySource *src)
{
    return mp_from_bytes_le(get_string(src));
}
#define get_mp_le(src) BinarySource_get_mp_le(BinarySource_UPCAST(src))

static void BinarySink_put_mp_le_unsigned(BinarySink *bs, mp_int *x)
{
    size_t bytes = (mp_get_nbits(x) + 7) / 8;

    put_uint32(bs, bytes);
    for (size_t i = 0; i < bytes; ++i)
        put_byte(bs, mp_get_byte(x, i));
}
#define put_mp_le_unsigned(bs, x) \
    BinarySink_put_mp_le_unsigned(BinarySink_UPCAST(bs), x)

static WeierstrassPoint *ecdsa_decode(
    ptrlen encoded, const struct ec_curve *curve)
{
    assert(curve->type == EC_WEIERSTRASS);
    BinarySource src[1];

    BinarySource_BARE_INIT_PL(src, encoded);
    unsigned char format_type = get_byte(src);

    WeierstrassPoint *P;

    size_t len = get_avail(src);
    mp_int *x;
    mp_int *y;

    switch (format_type) {
      case 0:
        /* The identity. */
        P = ecc_weierstrass_point_new_identity(curve->w.wc);
        break;
      case 2:
      case 3:
        /* A compressed point, in which the x-coordinate is stored in
         * full, and y is deduced from that and a single bit
         * indicating its parity (stored in the format type byte). */
        x = mp_from_bytes_be(get_data(src, len));
        P = ecc_weierstrass_point_new_from_x(curve->w.wc, x, format_type & 1);
        mp_free(x);
        if (!P)            /* this can fail if the input is invalid */
            return NULL;
        break;
      case 4:
        /* An uncompressed point: the x,y coordinates are stored in
         * full. We expect the rest of the string to have even length,
         * and be divided half and half between the two values. */
        if (len % 2 != 0)
            return NULL;
        len /= 2;
        x = mp_from_bytes_be(get_data(src, len));
        y = mp_from_bytes_be(get_data(src, len));
        P = ecc_weierstrass_point_new(curve->w.wc, x, y);
        mp_free(x);
        mp_free(y);
        break;
      default:
        /* An unrecognised type byte. */
        return NULL;
    }

    /* Verify the point is on the curve */
    if (!ecc_weierstrass_point_valid(P)) {
        ecc_weierstrass_point_free(P);
        return NULL;
    }

    return P;
}

static WeierstrassPoint *BinarySource_get_wpoint(
    BinarySource *src, const struct ec_curve *curve)
{
    ptrlen str = get_string(src);
    if (get_err(src))
        return NULL;
    return ecdsa_decode(str, curve);
}
#define get_wpoint(src, curve) \
    BinarySource_get_wpoint(BinarySource_UPCAST(src), curve)

static void BinarySink_put_wpoint(
    BinarySink *bs, WeierstrassPoint *point, const struct ec_curve *curve,
    bool bare)
{
    strbuf *sb;
    BinarySink *bs_inner;

    if (!bare) {
        /*
         * Encapsulate the raw data inside an outermost string layer.
         */
        sb = strbuf_new();
        bs_inner = BinarySink_UPCAST(sb);
    } else {
        /*
         * Just write the data directly to the output.
         */
        bs_inner = bs;
    }

    if (ecc_weierstrass_is_identity(point)) {
        put_byte(bs_inner, 0);
    } else {
        mp_int *x, *y;
        ecc_weierstrass_get_affine(point, &x, &y);

        /*
         * For ECDSA, we only ever output uncompressed points.
         */
        put_byte(bs_inner, 0x04);
        for (size_t i = curve->fieldBytes; i--;)
            put_byte(bs_inner, mp_get_byte(x, i));
        for (size_t i = curve->fieldBytes; i--;)
            put_byte(bs_inner, mp_get_byte(y, i));

        mp_free(x);
        mp_free(y);
    }

    if (!bare)
        put_stringsb(bs, sb);
}
#define put_wpoint(bs, point, curve, bare)                              \
    BinarySink_put_wpoint(BinarySink_UPCAST(bs), point, curve, bare)

static EdwardsPoint *eddsa_decode(ptrlen encoded, const struct ec_curve *curve)
{
    assert(curve->type == EC_EDWARDS);
    assert(curve->fieldBits % 8 == 7);

    mp_int *y = mp_from_bytes_le(encoded);

    if (mp_get_nbits(y) > curve->fieldBits+1) {
        mp_free(y);
        return NULL;
    }

    /* The topmost bit of the encoding isn't part of y, so it stores
     * the bottom bit of x. Extract it, and zero that bit in y. */
    unsigned desired_x_parity = mp_get_bit(y, curve->fieldBits);
    mp_set_bit(y, curve->fieldBits, 0);

    EdwardsPoint *P = ecc_edwards_point_new_from_y(
        curve->e.ec, y, desired_x_parity);
    mp_free(y);

    /* A point constructed in this way will always satisfy the curve
     * equation, unless ecc.c wasn't able to construct one at all, in
     * which case P is now NULL. Either way, return it. */
    return P;
}

static EdwardsPoint *BinarySource_get_epoint(
    BinarySource *src, const struct ec_curve *curve)
{
    ptrlen str = get_string(src);
    if (get_err(src))
        return NULL;
    return eddsa_decode(str, curve);
}
#define get_epoint(src, curve) \
    BinarySource_get_epoint(BinarySource_UPCAST(src), curve)

static void BinarySink_put_epoint(
    BinarySink *bs, EdwardsPoint *point, const struct ec_curve *curve,
    bool bare)
{
    mp_int *x, *y;
    ecc_edwards_get_affine(point, &x, &y);

    assert(curve->fieldBytes >= 2);

    /*
     * EdDSA requires point compression. We store a single integer,
     * with bytes in little-endian order, which mostly contains y but
     * in which the topmost bit is the low bit of x.
     */
    if (!bare)
        put_uint32(bs, curve->fieldBytes);   /* string length field */
    for (size_t i = 0; i < curve->fieldBytes - 1; i++)
        put_byte(bs, mp_get_byte(y, i));
    put_byte(bs, (mp_get_byte(y, curve->fieldBytes - 1) & 0x7F) |
             (mp_get_bit(x, 0) << 7));

    mp_free(x);
    mp_free(y);
}
#define put_epoint(bs, point, curve, bare)                      \
    BinarySink_put_epoint(BinarySink_UPCAST(bs), point, curve, bare)

/* ----------------------------------------------------------------------
 * Exposed ECDSA interface
 */

static void ecdsa_freekey(ssh_key *key)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);

    if (ek->publicKey)
        ecc_weierstrass_point_free(ek->publicKey);
    if (ek->privateKey)
        mp_free(ek->privateKey);
    sfree(ek);
}

static void eddsa_freekey(ssh_key *key)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);

    if (ek->publicKey)
        ecc_edwards_point_free(ek->publicKey);
    if (ek->privateKey)
        mp_free(ek->privateKey);
    sfree(ek);
}

static char *ec_signkey_invalid(ssh_key *key, unsigned flags)
{
    /* All validity criteria for both ECDSA and EdDSA were checked
     * when we loaded the key in the first place */
    return NULL;
}

static ssh_key *ecdsa_new_pub(const ssh_keyalg *alg, ptrlen data)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_WEIERSTRASS);

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, data);
    get_string(src);

    /* Curve name is duplicated for Weierstrass form */
    if (!ptrlen_eq_string(get_string(src), curve->name))
        return NULL;

    struct ecdsa_key *ek = snew(struct ecdsa_key);
    ek->sshk.vt = alg;
    ek->curve = curve;
    ek->privateKey = NULL;

    ek->publicKey = get_wpoint(src, curve);
    if (!ek->publicKey) {
        ecdsa_freekey(&ek->sshk);
        return NULL;
    }

    return &ek->sshk;
}

static ssh_key *eddsa_new_pub(const ssh_keyalg *alg, ptrlen data)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_EDWARDS);

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, data);
    get_string(src);

    struct eddsa_key *ek = snew(struct eddsa_key);
    ek->sshk.vt = alg;
    ek->curve = curve;
    ek->privateKey = NULL;

    ek->publicKey = get_epoint(src, curve);
    if (!ek->publicKey) {
        eddsa_freekey(&ek->sshk);
        return NULL;
    }

    return &ek->sshk;
}

static char *ecc_cache_str_shared(
    const char *curve_name, mp_int *x, mp_int *y)
{
    strbuf *sb = strbuf_new();

    if (curve_name)
        strbuf_catf(sb, "%s,", curve_name);

    char *hx = mp_get_hex(x);
    char *hy = mp_get_hex(y);
    strbuf_catf(sb, "0x%s,0x%s", hx, hy);
    sfree(hx);
    sfree(hy);

    return strbuf_to_str(sb);
}

static char *ecdsa_cache_str(ssh_key *key)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);
    mp_int *x, *y;

    ecc_weierstrass_get_affine(ek->publicKey, &x, &y);
    char *toret = ecc_cache_str_shared(ek->curve->name, x, y);
    mp_free(x);
    mp_free(y);
    return toret;
}

static char *eddsa_cache_str(ssh_key *key)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);
    mp_int *x, *y;

    ecc_edwards_get_affine(ek->publicKey, &x, &y);
    char *toret = ecc_cache_str_shared(ek->curve->name, x, y);
    mp_free(x);
    mp_free(y);
    return toret;
}

static void ecdsa_public_blob(ssh_key *key, BinarySink *bs)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);

    put_stringz(bs, ek->sshk.vt->ssh_id);
    put_stringz(bs, ek->curve->name);
    put_wpoint(bs, ek->publicKey, ek->curve, false);
}

static void eddsa_public_blob(ssh_key *key, BinarySink *bs)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);

    put_stringz(bs, ek->sshk.vt->ssh_id);
    put_epoint(bs, ek->publicKey, ek->curve, false);
}

static void ecdsa_private_blob(ssh_key *key, BinarySink *bs)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);

    /* ECDSA uses ordinary SSH-2 mpint format to store the private key */
    assert(ek->privateKey);
    put_mp_ssh2(bs, ek->privateKey);
}

static void eddsa_private_blob(ssh_key *key, BinarySink *bs)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);

    /* EdDSA stores the private key integer little-endian and unsigned */
    assert(ek->privateKey);
    put_mp_le_unsigned(bs, ek->privateKey);
}

static ssh_key *ecdsa_new_priv(const ssh_keyalg *alg, ptrlen pub, ptrlen priv)
{
    ssh_key *sshk = ecdsa_new_pub(alg, pub);
    if (!sshk)
        return NULL;
    struct ecdsa_key *ek = container_of(sshk, struct ecdsa_key, sshk);

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, priv);
    ek->privateKey = get_mp_ssh2(src);

    return &ek->sshk;
}

static ssh_key *eddsa_new_priv(const ssh_keyalg *alg, ptrlen pub, ptrlen priv)
{
    ssh_key *sshk = eddsa_new_pub(alg, pub);
    if (!sshk)
        return NULL;
    struct eddsa_key *ek = container_of(sshk, struct eddsa_key, sshk);

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, priv);
    ek->privateKey = get_mp_le(src);

    return &ek->sshk;
}

static ssh_key *eddsa_new_priv_openssh(
    const ssh_keyalg *alg, BinarySource *src)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_EDWARDS);

    ptrlen pubkey_pl = get_string(src);
    ptrlen privkey_extended_pl = get_string(src);
    if (get_err(src) || pubkey_pl.len != curve->fieldBytes)
        return NULL;

    /*
     * The OpenSSH format for ed25519 private keys also for some
     * reason encodes an extra copy of the public key in the second
     * half of the secret-key string. Check that that's present and
     * correct as well, otherwise the key we think we've imported
     * won't behave identically to the way OpenSSH would have treated
     * it.
     */
    BinarySource subsrc[1];
    BinarySource_BARE_INIT_PL(subsrc, privkey_extended_pl);
    ptrlen privkey_pl = get_data(subsrc, curve->fieldBytes);
    ptrlen pubkey_copy_pl = get_data(subsrc, curve->fieldBytes);
    if (get_err(subsrc) || get_avail(subsrc))
        return NULL;
    if (!ptrlen_eq_ptrlen(pubkey_pl, pubkey_copy_pl))
        return NULL;

    struct eddsa_key *ek = snew(struct eddsa_key);
    ek->sshk.vt = alg;
    ek->curve = curve;
    ek->privateKey = NULL;

    ek->publicKey = eddsa_decode(pubkey_pl, curve);
    if (!ek->publicKey) {
        eddsa_freekey(&ek->sshk);
        return NULL;
    }

    ek->privateKey = mp_from_bytes_le(privkey_pl);

    return &ek->sshk;
}

static void eddsa_openssh_blob(ssh_key *key, BinarySink *bs)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);
    assert(ek->curve->type == EC_EDWARDS);

    /* Encode the public and private points as strings */
    strbuf *pub_sb = strbuf_new();
    put_epoint(pub_sb, ek->publicKey, ek->curve, false);
    ptrlen pub = make_ptrlen(pub_sb->s + 4, pub_sb->len - 4);

    strbuf *priv_sb = strbuf_new_nm();
    put_mp_le_unsigned(priv_sb, ek->privateKey);
    ptrlen priv = make_ptrlen(priv_sb->s + 4, priv_sb->len - 4);

    put_stringpl(bs, pub);

    /* Encode the private key as the concatenation of the
     * little-endian key integer and the public key again */
    put_uint32(bs, priv.len + pub.len);
    put_datapl(bs, priv);
    put_datapl(bs, pub);

    strbuf_free(pub_sb);
    strbuf_free(priv_sb);
}

static ssh_key *ecdsa_new_priv_openssh(
    const ssh_keyalg *alg, BinarySource *src)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    assert(curve->type == EC_WEIERSTRASS);

    get_string(src);

    struct eddsa_key *ek = snew(struct eddsa_key);
    ek->sshk.vt = alg;
    ek->curve = curve;
    ek->privateKey = NULL;

    ek->publicKey = get_epoint(src, curve);
    if (!ek->publicKey) {
        eddsa_freekey(&ek->sshk);
        return NULL;
    }

    ek->privateKey = get_mp_ssh2(src);

    return &ek->sshk;
}

static void ecdsa_openssh_blob(ssh_key *key, BinarySink *bs)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);
    put_stringz(bs, ek->curve->name);
    put_wpoint(bs, ek->publicKey, ek->curve, false);
    put_mp_ssh2(bs, ek->privateKey);
}

static int ec_shared_pubkey_bits(const ssh_keyalg *alg, ptrlen blob)
{
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)alg->extra;
    struct ec_curve *curve = extra->curve();
    return curve->fieldBits;
}

static mp_int *ecdsa_signing_exponent_from_data(
    const struct ec_curve *curve, const struct ecsign_extra *extra,
    ptrlen data)
{
    /* Hash the data being signed. */
    unsigned char hash[MAX_HASH_LEN];
    ssh_hash *h = ssh_hash_new(extra->hash);
    put_datapl(h, data);
    ssh_hash_final(h, hash);

    /*
     * Take the leftmost b bits of the hash of the signed data (where
     * b is the number of bits in order(G)), interpreted big-endian.
     */
    mp_int *z = mp_from_bytes_be(make_ptrlen(hash, extra->hash->hlen));
    size_t zbits = mp_get_nbits(z);
    size_t nbits = mp_get_nbits(curve->w.G_order);
    size_t shift = zbits - nbits;
    /* Bound the shift count below at 0, using bit twiddling to avoid
     * a conditional branch */
    shift &= ~-(shift >> (CHAR_BIT * sizeof(size_t) - 1));
    mp_int *toret = mp_rshift_safe(z, shift);
    mp_free(z);

    return toret;
}

static bool ecdsa_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)ek->sshk.vt->extra;

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, sig);

    /* Check the signature starts with the algorithm name */
    if (!ptrlen_eq_string(get_string(src), ek->sshk.vt->ssh_id))
        return false;

    /* Everything else is nested inside a sub-string. Descend into that. */
    ptrlen sigstr = get_string(src);
    if (get_err(src))
        return false;
    BinarySource_BARE_INIT_PL(src, sigstr);

    /* Extract the signature integers r,s */
    mp_int *r = get_mp_ssh2(src);
    mp_int *s = get_mp_ssh2(src);
    if (get_err(src)) {
        mp_free(r);
        mp_free(s);
        return false;
    }

    /* Basic sanity checks: 0 < r,s < order(G) */
    unsigned invalid = 0;
    invalid |= mp_eq_integer(r, 0);
    invalid |= mp_eq_integer(s, 0);
    invalid |= mp_cmp_hs(r, ek->curve->w.G_order);
    invalid |= mp_cmp_hs(s, ek->curve->w.G_order);

    /* Get the hash of the signed data, converted to an integer */
    mp_int *z = ecdsa_signing_exponent_from_data(ek->curve, extra, data);

    /* Verify the signature integers against the hash */
    mp_int *w = mp_invert(s, ek->curve->w.G_order);
    mp_int *u1 = mp_modmul(z, w, ek->curve->w.G_order);
    mp_free(z);
    mp_int *u2 = mp_modmul(r, w, ek->curve->w.G_order);
    mp_free(w);
    WeierstrassPoint *u1G = ecc_weierstrass_multiply(ek->curve->w.G, u1);
    mp_free(u1);
    WeierstrassPoint *u2P = ecc_weierstrass_multiply(ek->publicKey, u2);
    mp_free(u2);
    WeierstrassPoint *sum = ecc_weierstrass_add_general(u1G, u2P);
    ecc_weierstrass_point_free(u1G);
    ecc_weierstrass_point_free(u2P);

    mp_int *x;
    ecc_weierstrass_get_affine(sum, &x, NULL);
    ecc_weierstrass_point_free(sum);

    mp_divmod_into(x, ek->curve->w.G_order, NULL, x);
    invalid |= (1 ^ mp_cmp_eq(r, x));
    mp_free(x);

    mp_free(r);
    mp_free(s);

    return !invalid;
}

static mp_int *eddsa_signing_exponent_from_data(
    struct eddsa_key *ek, const struct ecsign_extra *extra,
    ptrlen r_encoded, ptrlen data)
{
    /* Hash (r || public key || message) */
    unsigned char hash[MAX_HASH_LEN];
    ssh_hash *h = ssh_hash_new(extra->hash);
    put_datapl(h, r_encoded);
    put_epoint(h, ek->publicKey, ek->curve, true); /* omit string header */
    put_datapl(h, data);
    ssh_hash_final(h, hash);

    /* Convert to an integer */
    mp_int *toret = mp_from_bytes_le(make_ptrlen(hash, extra->hash->hlen));

    smemclr(hash, extra->hash->hlen);
    return toret;
}

static bool eddsa_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)ek->sshk.vt->extra;

    BinarySource src[1];
    BinarySource_BARE_INIT_PL(src, sig);

    /* Check the signature starts with the algorithm name */
    if (!ptrlen_eq_string(get_string(src), ek->sshk.vt->ssh_id))
        return false;

    /* Now expect a single string which is the concatenation of an
     * encoded curve point r and an integer s. */
    ptrlen sigstr = get_string(src);
    if (get_err(src))
        return false;
    BinarySource_BARE_INIT_PL(src, sigstr);
    ptrlen rstr = get_data(src, ek->curve->fieldBytes);
    ptrlen sstr = get_data(src, ek->curve->fieldBytes);
    if (get_err(src) || get_avail(src))
        return false;

    EdwardsPoint *r = eddsa_decode(rstr, ek->curve);
    if (!r)
        return false;
    mp_int *s = mp_from_bytes_le(sstr);

    mp_int *H = eddsa_signing_exponent_from_data(ek, extra, rstr, data);

    /* Verify that s*G == r + H*publicKey */
    EdwardsPoint *lhs = ecc_edwards_multiply(ek->curve->e.G, s);
    mp_free(s);
    EdwardsPoint *hpk = ecc_edwards_multiply(ek->publicKey, H);
    mp_free(H);
    EdwardsPoint *rhs = ecc_edwards_add(r, hpk);
    ecc_edwards_point_free(hpk);
    unsigned valid = ecc_edwards_eq(lhs, rhs);
    ecc_edwards_point_free(lhs);
    ecc_edwards_point_free(rhs);
    ecc_edwards_point_free(r);

    return valid;
}

static void ecdsa_sign(ssh_key *key, ptrlen data,
                       unsigned flags, BinarySink *bs)
{
    struct ecdsa_key *ek = container_of(key, struct ecdsa_key, sshk);
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)ek->sshk.vt->extra;
    assert(ek->privateKey);

    mp_int *z = ecdsa_signing_exponent_from_data(ek->curve, extra, data);

    /* Generate k between 1 and curve->n, using the same deterministic
     * k generation system we use for conventional DSA. */
    mp_int *k;
    {
        unsigned char digest[20];
        hash_simple(&ssh_sha1, data, digest);
        k = dss_gen_k(
            "ECDSA deterministic k generator", ek->curve->w.G_order,
            ek->privateKey, digest, sizeof(digest));
    }

    WeierstrassPoint *kG = ecc_weierstrass_multiply(ek->curve->w.G, k);
    mp_int *x;
    ecc_weierstrass_get_affine(kG, &x, NULL);
    ecc_weierstrass_point_free(kG);

    /* r = kG.x mod order(G) */
    mp_int *r = mp_mod(x, ek->curve->w.G_order);
    mp_free(x);

    /* s = (z + r * priv)/k mod n */
    mp_int *rPriv = mp_modmul(r, ek->privateKey, ek->curve->w.G_order);
    mp_int *numerator = mp_modadd(z, rPriv, ek->curve->w.G_order);
    mp_free(z);
    mp_free(rPriv);
    mp_int *kInv = mp_invert(k, ek->curve->w.G_order);
    mp_free(k);
    mp_int *s = mp_modmul(numerator, kInv, ek->curve->w.G_order);
    mp_free(numerator);
    mp_free(kInv);

    /* Format the output */
    put_stringz(bs, ek->sshk.vt->ssh_id);

    strbuf *substr = strbuf_new();
    put_mp_ssh2(substr, r);
    put_mp_ssh2(substr, s);
    put_stringsb(bs, substr);

    mp_free(r);
    mp_free(s);
}

static void eddsa_sign(ssh_key *key, ptrlen data,
                       unsigned flags, BinarySink *bs)
{
    struct eddsa_key *ek = container_of(key, struct eddsa_key, sshk);
    const struct ecsign_extra *extra =
        (const struct ecsign_extra *)ek->sshk.vt->extra;
    assert(ek->privateKey);

    /*
     * EdDSA prescribes a specific method of generating the random
     * nonce integer for the signature. (A verifier can't tell
     * whether you followed that method, but it's important to
     * follow it anyway, because test vectors will want a specific
     * signature for a given message, and because this preserves
     * determinism of signatures even if the same signature were
     * made twice by different software.)
     */

    /*
     * First, we hash the private key integer (bare, little-endian)
     * into a hash generating 2*fieldBytes of output.
     */
    unsigned char hash[MAX_HASH_LEN];
    ssh_hash *h = ssh_hash_new(extra->hash);
    for (size_t i = 0; i < ek->curve->fieldBytes; ++i)
        put_byte(h, mp_get_byte(ek->privateKey, i));
    ssh_hash_final(h, hash);

    /*
     * The first half of the output hash is converted into an
     * integer a, by the standard EdDSA transformation.
     */
    mp_int *a = eddsa_exponent_from_hash(
        make_ptrlen(hash, ek->curve->fieldBytes), ek->curve);

    /*
     * The second half of the hash of the private key is hashed again
     * with the message to be signed, and used as an exponent to
     * generate the signature point r.
     */
    h = ssh_hash_new(extra->hash);
    put_data(h, hash + ek->curve->fieldBytes,
             extra->hash->hlen - ek->curve->fieldBytes);
    put_datapl(h, data);
    ssh_hash_final(h, hash);
    mp_int *log_r_unreduced = mp_from_bytes_le(
        make_ptrlen(hash, extra->hash->hlen));
    mp_int *log_r = mp_mod(log_r_unreduced, ek->curve->e.G_order);
    mp_free(log_r_unreduced);
    EdwardsPoint *r = ecc_edwards_multiply(ek->curve->e.G, log_r);

    /*
     * Encode r now, because we'll need its encoding for the next
     * hashing step as well as to write into the actual signature.
     */
    strbuf *r_enc = strbuf_new();
    put_epoint(r_enc, r, ek->curve, true); /* omit string header */
    ecc_edwards_point_free(r);

    /*
     * Compute the hash of (r || public key || message) just as
     * eddsa_verify does.
     */
    mp_int *H = eddsa_signing_exponent_from_data(
        ek, extra, ptrlen_from_strbuf(r_enc), data);

    /* And then s = (log(r) + H*a) mod order(G). */
    mp_int *Ha = mp_modmul(H, a, ek->curve->e.G_order);
    mp_int *s = mp_modadd(log_r, Ha, ek->curve->e.G_order);
    mp_free(H);
    mp_free(a);
    mp_free(Ha);
    mp_free(log_r);

    /* Format the output */
    put_stringz(bs, ek->sshk.vt->ssh_id);
    put_uint32(bs, r_enc->len + ek->curve->fieldBytes);
    put_data(bs, r_enc->u, r_enc->len);
    strbuf_free(r_enc);
    for (size_t i = 0; i < ek->curve->fieldBytes; ++i)
        put_byte(bs, mp_get_byte(s, i));
    mp_free(s);
}

const struct ecsign_extra sign_extra_ed25519 = {
    ec_ed25519, &ssh_sha512,
    NULL, 0,
};
const ssh_keyalg ssh_ecdsa_ed25519 = {
    eddsa_new_pub,
    eddsa_new_priv,
    eddsa_new_priv_openssh,

    eddsa_freekey,
    ec_signkey_invalid,
    eddsa_sign,
    eddsa_verify,
    eddsa_public_blob,
    eddsa_private_blob,
    eddsa_openssh_blob,
    eddsa_cache_str,

    ec_shared_pubkey_bits,

    "ssh-ed25519",
    "ssh-ed25519",
    &sign_extra_ed25519,
    0, /* no supported flags */
};

/* OID: 1.2.840.10045.3.1.7 (ansiX9p256r1) */
static const unsigned char nistp256_oid[] = {
    0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07
};
const struct ecsign_extra sign_extra_nistp256 = {
    ec_p256, &ssh_sha256,
    nistp256_oid, lenof(nistp256_oid),
};
const ssh_keyalg ssh_ecdsa_nistp256 = {
    ecdsa_new_pub,
    ecdsa_new_priv,
    ecdsa_new_priv_openssh,

    ecdsa_freekey,
    ec_signkey_invalid,
    ecdsa_sign,
    ecdsa_verify,
    ecdsa_public_blob,
    ecdsa_private_blob,
    ecdsa_openssh_blob,
    ecdsa_cache_str,

    ec_shared_pubkey_bits,

    "ecdsa-sha2-nistp256",
    "ecdsa-sha2-nistp256",
    &sign_extra_nistp256,
    0, /* no supported flags */
};

/* OID: 1.3.132.0.34 (secp384r1) */
static const unsigned char nistp384_oid[] = {
    0x2b, 0x81, 0x04, 0x00, 0x22
};
const struct ecsign_extra sign_extra_nistp384 = {
    ec_p384, &ssh_sha384,
    nistp384_oid, lenof(nistp384_oid),
};
const ssh_keyalg ssh_ecdsa_nistp384 = {
    ecdsa_new_pub,
    ecdsa_new_priv,
    ecdsa_new_priv_openssh,

    ecdsa_freekey,
    ec_signkey_invalid,
    ecdsa_sign,
    ecdsa_verify,
    ecdsa_public_blob,
    ecdsa_private_blob,
    ecdsa_openssh_blob,
    ecdsa_cache_str,

    ec_shared_pubkey_bits,

    "ecdsa-sha2-nistp384",
    "ecdsa-sha2-nistp384",
    &sign_extra_nistp384,
    0, /* no supported flags */
};

/* OID: 1.3.132.0.35 (secp521r1) */
static const unsigned char nistp521_oid[] = {
    0x2b, 0x81, 0x04, 0x00, 0x23
};
const struct ecsign_extra sign_extra_nistp521 = {
    ec_p521, &ssh_sha512,
    nistp521_oid, lenof(nistp521_oid),
};
const ssh_keyalg ssh_ecdsa_nistp521 = {
    ecdsa_new_pub,
    ecdsa_new_priv,
    ecdsa_new_priv_openssh,

    ecdsa_freekey,
    ec_signkey_invalid,
    ecdsa_sign,
    ecdsa_verify,
    ecdsa_public_blob,
    ecdsa_private_blob,
    ecdsa_openssh_blob,
    ecdsa_cache_str,

    ec_shared_pubkey_bits,

    "ecdsa-sha2-nistp521",
    "ecdsa-sha2-nistp521",
    &sign_extra_nistp521,
    0, /* no supported flags */
};

/* ----------------------------------------------------------------------
 * Exposed ECDH interface
 */

struct eckex_extra {
    struct ec_curve *(*curve)(void);
    void (*setup)(ecdh_key *dh);
    void (*cleanup)(ecdh_key *dh);
    void (*getpublic)(ecdh_key *dh, BinarySink *bs);
    mp_int *(*getkey)(ecdh_key *dh, ptrlen remoteKey);
};

struct ecdh_key {
    const struct eckex_extra *extra;
    const struct ec_curve *curve;
    mp_int *private;
    union {
        WeierstrassPoint *w_public;
        MontgomeryPoint *m_public;
    };
};

const char *ssh_ecdhkex_curve_textname(const ssh_kex *kex)
{
    const struct eckex_extra *extra = (const struct eckex_extra *)kex->extra;
    struct ec_curve *curve = extra->curve();
    return curve->textname;
}

static void ssh_ecdhkex_w_setup(ecdh_key *dh)
{
    mp_int *one = mp_from_integer(1);
    dh->private = mp_random_in_range(one, dh->curve->w.G_order);
    mp_free(one);

    dh->w_public = ecc_weierstrass_multiply(dh->curve->w.G, dh->private);
}

static void ssh_ecdhkex_m_setup(ecdh_key *dh)
{
    strbuf *bytes = strbuf_new_nm();
    random_read(strbuf_append(bytes, dh->curve->fieldBytes),
                dh->curve->fieldBytes);

    bytes->u[0] &= 0xF8;
    bytes->u[bytes->len-1] &= 0x7F;
    bytes->u[bytes->len-1] |= 0x40;
    dh->private = mp_from_bytes_le(ptrlen_from_strbuf(bytes));
    strbuf_free(bytes);

    dh->m_public = ecc_montgomery_multiply(dh->curve->m.G, dh->private);
}

ecdh_key *ssh_ecdhkex_newkey(const ssh_kex *kex)
{
    const struct eckex_extra *extra = (const struct eckex_extra *)kex->extra;
    const struct ec_curve *curve = extra->curve();

    ecdh_key *dh = snew(ecdh_key);
    dh->extra = extra;
    dh->curve = curve;
    dh->extra->setup(dh);
    return dh;
}

static void ssh_ecdhkex_w_getpublic(ecdh_key *dh, BinarySink *bs)
{
    put_wpoint(bs, dh->w_public, dh->curve, true);
}

static void ssh_ecdhkex_m_getpublic(ecdh_key *dh, BinarySink *bs)
{
    mp_int *x;
    ecc_montgomery_get_affine(dh->m_public, &x);
    for (size_t i = 0; i < dh->curve->fieldBytes; ++i)
        put_byte(bs, mp_get_byte(x, i));
    mp_free(x);
}

void ssh_ecdhkex_getpublic(ecdh_key *dh, BinarySink *bs)
{
    dh->extra->getpublic(dh, bs);
}

static mp_int *ssh_ecdhkex_w_getkey(ecdh_key *dh, ptrlen remoteKey)
{
    WeierstrassPoint *remote_p = ecdsa_decode(remoteKey, dh->curve);
    if (!remote_p)
        return NULL;

    if (ecc_weierstrass_is_identity(remote_p)) {
        /* Not a sensible Diffie-Hellman input value */
        ecc_weierstrass_point_free(remote_p);
        return NULL;
    }

    WeierstrassPoint *p = ecc_weierstrass_multiply(remote_p, dh->private);

    mp_int *x;
    ecc_weierstrass_get_affine(p, &x, NULL);

    ecc_weierstrass_point_free(remote_p);
    ecc_weierstrass_point_free(p);

    return x;
}

static mp_int *ssh_ecdhkex_m_getkey(ecdh_key *dh, ptrlen remoteKey)
{
    mp_int *remote_x = mp_from_bytes_le(remoteKey);
    if (mp_eq_integer(remote_x, 0)) {
        /*
         * The libssh spec for Curve25519 key exchange says that
         * 'every possible public key maps to a valid ECC Point' and
         * therefore no validation needs to be done on the server's
         * provided x-coordinate. However, I don't believe it: an
         * x-coordinate of zero doesn't work sensibly, because you end
         * up dividing by zero in the doubling formula
         * (x+1)^2(x-1)^2/(4(x^3+ax^2+x)). (Put another way, although
         * that point P is not the _identity_ of the curve, it is a
         * torsion point such that 2P is the identity.)
         */
        mp_free(remote_x);
        return NULL;
    }
    MontgomeryPoint *remote_p = ecc_montgomery_point_new(
        dh->curve->m.mc, remote_x);
    mp_free(remote_x);

    MontgomeryPoint *p = ecc_montgomery_multiply(remote_p, dh->private);
    mp_int *x;
    ecc_montgomery_get_affine(p, &x);

    ecc_montgomery_point_free(remote_p);
    ecc_montgomery_point_free(p);

    /*
     * Endianness-swap. The Curve25519 algorithm definition assumes
     * you were doing your computation in arrays of 32 little-endian
     * bytes, and now specifies that you take your final one of those
     * and convert it into a bignum in _network_ byte order, i.e.
     * big-endian.
     *
     * In particular, the spec says, you convert the _whole_ 32 bytes
     * into a bignum. That is, on the rare occasions that x has come
     * out with the most significant 8 bits zero, we have to imagine
     * that being represented by a 32-byte string with the last byte
     * being zero, so that has to be converted into an SSH-2 bignum
     * with the _low_ byte zero, i.e. a multiple of 256.
     */
    strbuf *sb = strbuf_new();
    for (size_t i = 0; i < dh->curve->fieldBytes; ++i)
        put_byte(sb, mp_get_byte(x, i));
    mp_free(x);
    x = mp_from_bytes_be(ptrlen_from_strbuf(sb));
    strbuf_free(sb);

    return x;
}

mp_int *ssh_ecdhkex_getkey(ecdh_key *dh, ptrlen remoteKey)
{
    return dh->extra->getkey(dh, remoteKey);
}

static void ssh_ecdhkex_w_cleanup(ecdh_key *dh)
{
    ecc_weierstrass_point_free(dh->w_public);
}

static void ssh_ecdhkex_m_cleanup(ecdh_key *dh)
{
    ecc_montgomery_point_free(dh->m_public);
}

void ssh_ecdhkex_freekey(ecdh_key *dh)
{
    mp_free(dh->private);
    dh->extra->cleanup(dh);
    sfree(dh);
}

static const struct eckex_extra kex_extra_curve25519 = {
    ec_curve25519,
    ssh_ecdhkex_m_setup,
    ssh_ecdhkex_m_cleanup,
    ssh_ecdhkex_m_getpublic,
    ssh_ecdhkex_m_getkey,
};
const ssh_kex ssh_ec_kex_curve25519 = {
    "curve25519-sha256@libssh.org", NULL, KEXTYPE_ECDH,
    &ssh_sha256, &kex_extra_curve25519,
};

const struct eckex_extra kex_extra_nistp256 = {
    ec_p256,
    ssh_ecdhkex_w_setup,
    ssh_ecdhkex_w_cleanup,
    ssh_ecdhkex_w_getpublic,
    ssh_ecdhkex_w_getkey,
};
const ssh_kex ssh_ec_kex_nistp256 = {
    "ecdh-sha2-nistp256", NULL, KEXTYPE_ECDH,
    &ssh_sha256, &kex_extra_nistp256,
};

const struct eckex_extra kex_extra_nistp384 = {
    ec_p384,
    ssh_ecdhkex_w_setup,
    ssh_ecdhkex_w_cleanup,
    ssh_ecdhkex_w_getpublic,
    ssh_ecdhkex_w_getkey,
};
const ssh_kex ssh_ec_kex_nistp384 = {
    "ecdh-sha2-nistp384", NULL, KEXTYPE_ECDH,
    &ssh_sha384, &kex_extra_nistp384,
};

const struct eckex_extra kex_extra_nistp521 = {
    ec_p521,
    ssh_ecdhkex_w_setup,
    ssh_ecdhkex_w_cleanup,
    ssh_ecdhkex_w_getpublic,
    ssh_ecdhkex_w_getkey,
};
const ssh_kex ssh_ec_kex_nistp521 = {
    "ecdh-sha2-nistp521", NULL, KEXTYPE_ECDH,
    &ssh_sha512, &kex_extra_nistp521,
};

static const ssh_kex *const ec_kex_list[] = {
    &ssh_ec_kex_curve25519,
    &ssh_ec_kex_nistp256,
    &ssh_ec_kex_nistp384,
    &ssh_ec_kex_nistp521,
};

const ssh_kexes ssh_ecdh_kex = { lenof(ec_kex_list), ec_kex_list };

/* ----------------------------------------------------------------------
 * Helper functions for finding key algorithms and returning auxiliary
 * data.
 */

const ssh_keyalg *ec_alg_by_oid(int len, const void *oid,
                                        const struct ec_curve **curve)
{
    static const ssh_keyalg *algs_with_oid[] = {
        &ssh_ecdsa_nistp256,
        &ssh_ecdsa_nistp384,
        &ssh_ecdsa_nistp521,
    };
    int i;

    for (i = 0; i < lenof(algs_with_oid); i++) {
        const ssh_keyalg *alg = algs_with_oid[i];
        const struct ecsign_extra *extra =
            (const struct ecsign_extra *)alg->extra;
        if (len == extra->oidlen && !memcmp(oid, extra->oid, len)) {
            *curve = extra->curve();
            return alg;
        }
    }
    return NULL;
}

const unsigned char *ec_alg_oid(const ssh_keyalg *alg,
                                int *oidlen)
{
    const struct ecsign_extra *extra = (const struct ecsign_extra *)alg->extra;
    *oidlen = extra->oidlen;
    return extra->oid;
}

const int ec_nist_curve_lengths[] = { 256, 384, 521 };
const int n_ec_nist_curve_lengths = lenof(ec_nist_curve_lengths);

bool ec_nist_alg_and_curve_by_bits(
    int bits, const struct ec_curve **curve, const ssh_keyalg **alg)
{
    switch (bits) {
      case 256: *alg = &ssh_ecdsa_nistp256; break;
      case 384: *alg = &ssh_ecdsa_nistp384; break;
      case 521: *alg = &ssh_ecdsa_nistp521; break;
      default: return false;
    }
    *curve = ((struct ecsign_extra *)(*alg)->extra)->curve();
    return true;
}

bool ec_ed_alg_and_curve_by_bits(
    int bits, const struct ec_curve **curve, const ssh_keyalg **alg)
{
    switch (bits) {
      case 256: *alg = &ssh_ecdsa_ed25519; break;
      default: return false;
    }
    *curve = ((struct ecsign_extra *)(*alg)->extra)->curve();
    return true;
}
