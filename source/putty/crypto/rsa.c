/*
 * RSA implementation for PuTTY.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "ssh.h"
#include "mpint.h"
#include "misc.h"

void BinarySource_get_rsa_ssh1_pub(
    BinarySource *src, RSAKey *rsa, RsaSsh1Order order)
{
    unsigned bits;
    mp_int *e, *m;

    bits = get_uint32(src);
    if (order == RSA_SSH1_EXPONENT_FIRST) {
        e = get_mp_ssh1(src);
        m = get_mp_ssh1(src);
    } else {
        m = get_mp_ssh1(src);
        e = get_mp_ssh1(src);
    }

    if (rsa) {
        rsa->bits = bits;
        rsa->exponent = e;
        rsa->modulus = m;
        rsa->bytes = (mp_get_nbits(m) + 7) / 8;
    } else {
        mp_free(e);
        mp_free(m);
    }
}

void BinarySource_get_rsa_ssh1_priv(
    BinarySource *src, RSAKey *rsa)
{
    rsa->private_exponent = get_mp_ssh1(src);
}

key_components *rsa_components(RSAKey *rsa)
{
    key_components *kc = key_components_new();
    key_components_add_text(kc, "key_type", "RSA");
    key_components_add_mp(kc, "public_modulus", rsa->modulus);
    key_components_add_mp(kc, "public_exponent", rsa->exponent);
    if (rsa->private_exponent) {
        key_components_add_mp(kc, "private_exponent", rsa->private_exponent);
        key_components_add_mp(kc, "private_p", rsa->p);
        key_components_add_mp(kc, "private_q", rsa->q);
        key_components_add_mp(kc, "private_inverse_q_mod_p", rsa->iqmp);
    }
    return kc;
}

RSAKey *BinarySource_get_rsa_ssh1_priv_agent(BinarySource *src)
{
    RSAKey *rsa = snew(RSAKey);
    memset(rsa, 0, sizeof(RSAKey));

    get_rsa_ssh1_pub(src, rsa, RSA_SSH1_MODULUS_FIRST);
    get_rsa_ssh1_priv(src, rsa);

    /* SSH-1 names p and q the other way round, i.e. we have the
     * inverse of p mod q and not of q mod p. We swap the names,
     * because our internal RSA wants iqmp. */
    rsa->iqmp = get_mp_ssh1(src);
    rsa->q = get_mp_ssh1(src);
    rsa->p = get_mp_ssh1(src);

    return rsa;
}

void duprsakey(RSAKey *dst, const RSAKey *src)
{
    dst->bits = src->bits;
    dst->bytes = src->bytes;
    dst->modulus = mp_copy(src->modulus);
    dst->exponent = mp_copy(src->exponent);
    dst->private_exponent = src->private_exponent ?
        mp_copy(src->private_exponent) : NULL;
    dst->p = mp_copy(src->p);
    dst->q = mp_copy(src->q);
    dst->iqmp = mp_copy(src->iqmp);
    dst->comment = src->comment ? dupstr(src->comment) : NULL;
    dst->sshk.vt = src->sshk.vt;
}

bool rsa_ssh1_encrypt(unsigned char *data, int length, RSAKey *key)
{
    mp_int *b1, *b2;
    int i;
    unsigned char *p;

    if (key->bytes < length + 4)
        return false;                  /* RSA key too short! */

    memmove(data + key->bytes - length, data, length);
    data[0] = 0;
    data[1] = 2;

    { // WINSCP
    size_t npad = key->bytes - length - 3;
    /*
     * Generate a sequence of nonzero padding bytes. We do this in a
     * reasonably uniform way and without having to loop round
     * retrying the random number generation, by first generating an
     * integer in [0,2^n) for an appropriately large n; then we
     * repeatedly multiply by 255 to give an integer in [0,255*2^n),
     * extract the top 8 bits to give an integer in [0,255), and mask
     * those bits off before multiplying up again for the next digit.
     * This gives us a sequence of numbers in [0,255), and of course
     * adding 1 to each of them gives numbers in [1,256) as we wanted.
     *
     * (You could imagine this being a sort of fixed-point operation:
     * given a uniformly random binary _fraction_, multiplying it by k
     * and subtracting off the integer part will yield you a sequence
     * of integers each in [0,k). I'm just doing that scaled up by a
     * power of 2 to avoid the fractions.)
     */
    size_t random_bits = (npad + 16) * 8;
    mp_int *randval = mp_new(random_bits + 8);
    mp_int *tmp = mp_random_bits(random_bits);
    mp_copy_into(randval, tmp);
    mp_free(tmp);
    for (i = 2; i < key->bytes - length - 1; i++) {
        mp_mul_integer_into(randval, randval, 255);
        { // WINSCP
        uint8_t byte = mp_get_byte(randval, random_bits / 8);
        assert(byte != 255);
        data[i] = byte + 1;
        mp_reduce_mod_2to(randval, random_bits);
        } // WINSCP
    }
    mp_free(randval);
    data[key->bytes - length - 1] = 0;

    b1 = mp_from_bytes_be(make_ptrlen(data, key->bytes));

    b2 = mp_modpow(b1, key->exponent, key->modulus);

    p = data;
    for (i = key->bytes; i--;) {
        *p++ = mp_get_byte(b2, i);
    }

    mp_free(b1);
    mp_free(b2);

    return true;
    } // WINSCP
}

/*
 * Compute (base ^ exp) % mod, provided mod == p * q, with p,q
 * distinct primes, and iqmp is the multiplicative inverse of q mod p.
 * Uses Chinese Remainder Theorem to speed computation up over the
 * obvious implementation of a single big modpow.
 */
static mp_int *crt_modpow(mp_int *base, mp_int *exp, mp_int *mod,
                          mp_int *p, mp_int *q, mp_int *iqmp)
{
    mp_int *pm1, *qm1, *pexp, *qexp, *presult, *qresult;
    mp_int *diff, *multiplier, *ret0, *ret;

    /*
     * Reduce the exponent mod phi(p) and phi(q), to save time when
     * exponentiating mod p and mod q respectively. Of course, since p
     * and q are prime, phi(p) == p-1 and similarly for q.
     */
    pm1 = mp_copy(p);
    mp_sub_integer_into(pm1, pm1, 1);
    qm1 = mp_copy(q);
    mp_sub_integer_into(qm1, qm1, 1);
    pexp = mp_mod(exp, pm1);
    qexp = mp_mod(exp, qm1);

    /*
     * Do the two modpows.
     */
    { // WINSCP
    mp_int *base_mod_p = mp_mod(base, p);
    presult = mp_modpow(base_mod_p, pexp, p);
    mp_free(base_mod_p);
    } // WINSCP
    { // WINSCP
    mp_int *base_mod_q = mp_mod(base, q);
    qresult = mp_modpow(base_mod_q, qexp, q);
    mp_free(base_mod_q);
    } // WINSCP

    /*
     * Recombine the results. We want a value which is congruent to
     * qresult mod q, and to presult mod p.
     *
     * We know that iqmp * q is congruent to 1 * mod p (by definition
     * of iqmp) and to 0 mod q (obviously). So we start with qresult
     * (which is congruent to qresult mod both primes), and add on
     * (presult-qresult) * (iqmp * q) which adjusts it to be congruent
     * to presult mod p without affecting its value mod q.
     *
     * (If presult-qresult < 0, we add p to it to keep it positive.)
     */
    { // WINSCP
    unsigned presult_too_small = mp_cmp_hs(qresult, presult);
    mp_cond_add_into(presult, presult, p, presult_too_small);
    } // WINSCP

    diff = mp_sub(presult, qresult);
    multiplier = mp_mul(iqmp, q);
    ret0 = mp_mul(multiplier, diff);
    mp_add_into(ret0, ret0, qresult);

    /*
     * Finally, reduce the result mod n.
     */
    ret = mp_mod(ret0, mod);

    /*
     * Free all the intermediate results before returning.
     */
    mp_free(pm1);
    mp_free(qm1);
    mp_free(pexp);
    mp_free(qexp);
    mp_free(presult);
    mp_free(qresult);
    mp_free(diff);
    mp_free(multiplier);
    mp_free(ret0);

    return ret;
}

/*
 * Wrapper on crt_modpow that looks up all the right values from an
 * RSAKey.
 */
static mp_int *rsa_privkey_op(mp_int *input, RSAKey *key)
{
    return crt_modpow(input, key->private_exponent,
                      key->modulus, key->p, key->q, key->iqmp);
}

mp_int *rsa_ssh1_decrypt(mp_int *input, RSAKey *key)
{
    return rsa_privkey_op(input, key);
}

bool rsa_ssh1_decrypt_pkcs1(mp_int *input, RSAKey *key,
                            strbuf *outbuf)
{
    strbuf *data = strbuf_new_nm();
    bool success = false;
    BinarySource src[1];

    {
        mp_int *b = rsa_ssh1_decrypt(input, key);
        size_t i; // WINSCP
        for (i = (mp_get_nbits(key->modulus) + 7) / 8; i-- > 0 ;) {
            put_byte(data, mp_get_byte(b, i));
        }
        mp_free(b);
    }

    BinarySource_BARE_INIT(src, data->u, data->len);

    /* Check PKCS#1 formatting prefix */
    if (get_byte(src) != 0) goto out;
    if (get_byte(src) != 2) goto out;
    while (1) {
        unsigned char byte = get_byte(src);
        if (get_err(src)) goto out;
        if (byte == 0)
            break;
    }

    /* Everything else is the payload */
    success = true;
    put_data(outbuf, get_ptr(src), get_avail(src));

  out:
    strbuf_free(data);
    return success;
}

static void append_hex_to_strbuf(strbuf *sb, mp_int *x)
{
    if (sb->len > 0)
        put_byte(sb, ',');
    put_data(sb, "0x", 2);
    { // WINSCP
    char *hex = mp_get_hex(x);
    size_t hexlen = strlen(hex);
    put_data(sb, hex, hexlen);
    smemclr(hex, hexlen);
    sfree(hex);
    } // WINSCP
}

char *rsastr_fmt(RSAKey *key)
{
    strbuf *sb = strbuf_new();

    append_hex_to_strbuf(sb, key->exponent);
    append_hex_to_strbuf(sb, key->modulus);

    return strbuf_to_str(sb);
}

/*
 * Generate a fingerprint string for the key. Compatible with the
 * OpenSSH fingerprint code.
 */
char *rsa_ssh1_fingerprint(RSAKey *key)
{
    unsigned char digest[16];
    strbuf *out;
    int i;

    /*
     * The hash preimage for SSH-1 key fingerprinting consists of the
     * modulus and exponent _without_ any preceding length field -
     * just the minimum number of bytes to represent each integer,
     * stored big-endian, concatenated with no marker at the division
     * between them.
     */

    ssh_hash *hash = ssh_hash_new(&ssh_md5);
    { // WINSCP
    size_t i; // WINSCP
    for (i = (mp_get_nbits(key->modulus) + 7) / 8; i-- > 0 ;)
        put_byte(hash, mp_get_byte(key->modulus, i));
    for (i = (mp_get_nbits(key->exponent) + 7) / 8; i-- > 0 ;)
        put_byte(hash, mp_get_byte(key->exponent, i));
    } // WINSCP
    ssh_hash_final(hash, digest);

    out = strbuf_new();
    put_fmt(out, "%"SIZEu" ", mp_get_nbits(key->modulus));
    for (i = 0; i < 16; i++)
        put_fmt(out, "%s%02x", i ? ":" : "", digest[i]);
    if (key->comment)
        put_fmt(out, " %s", key->comment);
    return strbuf_to_str(out);
}

/*
 * Wrap the output of rsa_ssh1_fingerprint up into the same kind of
 * structure that comes from ssh2_all_fingerprints.
 */
char **rsa_ssh1_fake_all_fingerprints(RSAKey *key)
{
    char **fingerprints = snewn(SSH_N_FPTYPES, char *);
    unsigned i; // WINSCP
    for (i = 0; i < SSH_N_FPTYPES; i++)
        fingerprints[i] = NULL;
    fingerprints[SSH_FPTYPE_MD5] = rsa_ssh1_fingerprint(key);
    return fingerprints;
}

/*
 * Verify that the public data in an RSA key matches the private
 * data. We also check the private data itself: we ensure that p >
 * q and that iqmp really is the inverse of q mod p.
 */
bool rsa_verify(RSAKey *key)
{
    mp_int *n, *ed, *pm1, *qm1;
    unsigned ok = 1;

    /* Preliminary checks: p,q can't be 0 or 1. (Of course no other
     * very small value is any good either, but these are the values
     * we _must_ check for to avoid assertion failures further down
     * this function.) */
    if (!(mp_hs_integer(key->p, 2) & mp_hs_integer(key->q, 2)))
        return false;

    /* n must equal pq. */
    n = mp_mul(key->p, key->q);
    ok &= mp_cmp_eq(n, key->modulus);
    mp_free(n);

    /* e * d must be congruent to 1, modulo (p-1) and modulo (q-1). */
    pm1 = mp_copy(key->p);
    mp_sub_integer_into(pm1, pm1, 1);
    ed = mp_modmul(key->exponent, key->private_exponent, pm1);
    mp_free(pm1);
    ok &= mp_eq_integer(ed, 1);
    mp_free(ed);

    qm1 = mp_copy(key->q);
    mp_sub_integer_into(qm1, qm1, 1);
    ed = mp_modmul(key->exponent, key->private_exponent, qm1);
    mp_free(qm1);
    ok &= mp_eq_integer(ed, 1);
    mp_free(ed);

    /*
     * Ensure p > q.
     *
     * I have seen key blobs in the wild which were generated with
     * p < q, so instead of rejecting the key in this case we
     * should instead flip them round into the canonical order of
     * p > q. This also involves regenerating iqmp.
     */
    { // WINSCP
    mp_int *p_new = mp_max(key->p, key->q);
    mp_int *q_new = mp_min(key->p, key->q);
    mp_free(key->p);
    mp_free(key->q);
    mp_free(key->iqmp);
    key->p = p_new;
    key->q = q_new;
    key->iqmp = mp_invert(key->q, key->p);

    return ok;
    } // WINSCP
}

void rsa_ssh1_public_blob(BinarySink *bs, RSAKey *key,
                          RsaSsh1Order order)
{
    put_uint32(bs, mp_get_nbits(key->modulus));
    if (order == RSA_SSH1_EXPONENT_FIRST) {
        put_mp_ssh1(bs, key->exponent);
        put_mp_ssh1(bs, key->modulus);
    } else {
        put_mp_ssh1(bs, key->modulus);
        put_mp_ssh1(bs, key->exponent);
    }
}

void rsa_ssh1_private_blob_agent(BinarySink *bs, RSAKey *key)
{
    rsa_ssh1_public_blob(bs, key, RSA_SSH1_MODULUS_FIRST);
    put_mp_ssh1(bs, key->private_exponent);
    put_mp_ssh1(bs, key->iqmp);
    put_mp_ssh1(bs, key->q);
    put_mp_ssh1(bs, key->p);
}

/* Given an SSH-1 public key blob, determine its length. */
int rsa_ssh1_public_blob_len(ptrlen data)
{
    BinarySource src[1];

    BinarySource_BARE_INIT_PL(src, data);

    /* Expect a length word, then exponent and modulus. (It doesn't
     * even matter which order.) */
    get_uint32(src);
    mp_free(get_mp_ssh1(src));
    mp_free(get_mp_ssh1(src));

    if (get_err(src))
        return -1;

    /* Return the number of bytes consumed. */
    return src->pos;
}

void freersapriv(RSAKey *key)
{
    if (key->private_exponent) {
        mp_free(key->private_exponent);
        key->private_exponent = NULL;
    }
    if (key->p) {
        mp_free(key->p);
        key->p = NULL;
    }
    if (key->q) {
        mp_free(key->q);
        key->q = NULL;
    }
    if (key->iqmp) {
        mp_free(key->iqmp);
        key->iqmp = NULL;
    }
}

void freersakey(RSAKey *key)
{
    freersapriv(key);
    if (key->modulus) {
        mp_free(key->modulus);
        key->modulus = NULL;
    }
    if (key->exponent) {
        mp_free(key->exponent);
        key->exponent = NULL;
    }
    if (key->comment) {
        sfree(key->comment);
        key->comment = NULL;
    }
}

/* ----------------------------------------------------------------------
 * Implementation of the ssh-rsa signing key type family.
 */

struct ssh2_rsa_extra {
    unsigned signflags;
};

static void rsa2_freekey(ssh_key *key);   /* forward reference */

static ssh_key *rsa2_new_pub(const ssh_keyalg *self, ptrlen data)
{
    BinarySource src[1];
    RSAKey *rsa;

    BinarySource_BARE_INIT_PL(src, data);
    if (!ptrlen_eq_string(get_string(src), "ssh-rsa"))
        return NULL;

    rsa = snew(RSAKey);
    rsa->sshk.vt = self;
    rsa->exponent = get_mp_ssh2(src);
    rsa->modulus = get_mp_ssh2(src);
    rsa->private_exponent = NULL;
    rsa->p = rsa->q = rsa->iqmp = NULL;
    rsa->comment = NULL;

    if (get_err(src)) {
        rsa2_freekey(&rsa->sshk);
        return NULL;
    }

    return &rsa->sshk;
}

static void rsa2_freekey(ssh_key *key)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    freersakey(rsa);
    sfree(rsa);
}

static char *rsa2_cache_str(ssh_key *key)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    return rsastr_fmt(rsa);
}

static key_components *rsa2_components(ssh_key *key)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    return rsa_components(rsa);
}

static bool rsa2_has_private(ssh_key *key)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    return rsa->private_exponent != NULL;
}

static void rsa2_public_blob(ssh_key *key, BinarySink *bs)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);

    put_stringz(bs, "ssh-rsa");
    put_mp_ssh2(bs, rsa->exponent);
    put_mp_ssh2(bs, rsa->modulus);
}

static void rsa2_private_blob(ssh_key *key, BinarySink *bs)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);

    put_mp_ssh2(bs, rsa->private_exponent);
    put_mp_ssh2(bs, rsa->p);
    put_mp_ssh2(bs, rsa->q);
    put_mp_ssh2(bs, rsa->iqmp);
}

static ssh_key *rsa2_new_priv(const ssh_keyalg *self,
                              ptrlen pub, ptrlen priv)
{
    BinarySource src[1];
    ssh_key *sshk;
    RSAKey *rsa;

    sshk = rsa2_new_pub(self, pub);
    if (!sshk)
        return NULL;

    rsa = container_of(sshk, RSAKey, sshk);
    BinarySource_BARE_INIT_PL(src, priv);
    rsa->private_exponent = get_mp_ssh2(src);
    rsa->p = get_mp_ssh2(src);
    rsa->q = get_mp_ssh2(src);
    rsa->iqmp = get_mp_ssh2(src);

    if (get_err(src) || !rsa_verify(rsa)) {
        rsa2_freekey(&rsa->sshk);
        return NULL;
    }

    return &rsa->sshk;
}

static ssh_key *rsa2_new_priv_openssh(const ssh_keyalg *self,
                                      BinarySource *src)
{
    RSAKey *rsa;

    rsa = snew(RSAKey);
    rsa->sshk.vt = &ssh_rsa;
    rsa->comment = NULL;

    rsa->modulus = get_mp_ssh2(src);
    rsa->exponent = get_mp_ssh2(src);
    rsa->private_exponent = get_mp_ssh2(src);
    rsa->iqmp = get_mp_ssh2(src);
    rsa->p = get_mp_ssh2(src);
    rsa->q = get_mp_ssh2(src);

    if (get_err(src) || !rsa_verify(rsa)) {
        rsa2_freekey(&rsa->sshk);
        return NULL;
    }

    return &rsa->sshk;
}

static void rsa2_openssh_blob(ssh_key *key, BinarySink *bs)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);

    put_mp_ssh2(bs, rsa->modulus);
    put_mp_ssh2(bs, rsa->exponent);
    put_mp_ssh2(bs, rsa->private_exponent);
    put_mp_ssh2(bs, rsa->iqmp);
    put_mp_ssh2(bs, rsa->p);
    put_mp_ssh2(bs, rsa->q);
}

static int rsa2_pubkey_bits(const ssh_keyalg *self, ptrlen pub)
{
    ssh_key *sshk;
    RSAKey *rsa;
    int ret;

    sshk = rsa2_new_pub(self, pub);
    if (!sshk)
        return -1;

    rsa = container_of(sshk, RSAKey, sshk);
    ret = mp_get_nbits(rsa->modulus);
    rsa2_freekey(&rsa->sshk);

    return ret;
}

static inline const ssh_hashalg *rsa2_hash_alg_for_flags(
    unsigned flags, const char **protocol_id_out)
{
    const ssh_hashalg *halg;
    const char *protocol_id;

    if (flags & SSH_AGENT_RSA_SHA2_256) {
        halg = &ssh_sha256;
        protocol_id = "rsa-sha2-256";
    } else if (flags & SSH_AGENT_RSA_SHA2_512) {
        halg = &ssh_sha512;
        protocol_id = "rsa-sha2-512";
    } else {
        halg = &ssh_sha1;
        protocol_id = "ssh-rsa";
    }

    if (protocol_id_out)
        *protocol_id_out = protocol_id;

    return halg;
}

static inline ptrlen rsa_pkcs1_prefix_for_hash(const ssh_hashalg *halg)
{
    if (halg == &ssh_sha1) {
        /*
         * This is the magic ASN.1/DER prefix that goes in the decoded
         * signature, between the string of FFs and the actual SHA-1
         * hash value. The meaning of it is:
         *
         * 00 -- this marks the end of the FFs; not part of the ASN.1
         * bit itself
         *
         * 30 21 -- a constructed SEQUENCE of length 0x21
         *    30 09 -- a constructed sub-SEQUENCE of length 9
         *       06 05 -- an object identifier, length 5
         *          2B 0E 03 02 1A -- object id { 1 3 14 3 2 26 }
         *                            (the 1,3 comes from 0x2B = 43 = 40*1+3)
         *       05 00 -- NULL
         *    04 14 -- a primitive OCTET STRING of length 0x14
         *       [0x14 bytes of hash data follows]
         *
         * The object id in the middle there is listed as `id-sha1' in
         * ftp://ftp.rsasecurity.com/pub/pkcs/pkcs-1/pkcs-1v2-1d2.asn
         * (the ASN module for PKCS #1) and its expanded form is as
         * follows:
         *
         * id-sha1                OBJECT IDENTIFIER ::= {
         *    iso(1) identified-organization(3) oiw(14) secsig(3)
         *    algorithms(2) 26 }
         */
        static const unsigned char sha1_asn1_prefix[] = {
            0x00, 0x30, 0x21, 0x30, 0x09, 0x06, 0x05, 0x2B,
            0x0E, 0x03, 0x02, 0x1A, 0x05, 0x00, 0x04, 0x14,
        };
        return PTRLEN_FROM_CONST_BYTES(sha1_asn1_prefix);
    }

    if (halg == &ssh_sha256) {
        /*
         * A similar piece of ASN.1 used for signatures using SHA-256,
         * in the same format but differing only in various length
         * fields and OID.
         */
        static const unsigned char sha256_asn1_prefix[] = {
            0x00, 0x30, 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60,
            0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x01,
            0x05, 0x00, 0x04, 0x20,
        };
        return PTRLEN_FROM_CONST_BYTES(sha256_asn1_prefix);
    }

    if (halg == &ssh_sha512) {
        /*
         * And one more for SHA-512.
         */
        static const unsigned char sha512_asn1_prefix[] = {
            0x00, 0x30, 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60,
            0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x02, 0x03,
            0x05, 0x00, 0x04, 0x40,
        };
        return PTRLEN_FROM_CONST_BYTES(sha512_asn1_prefix);
    }

    unreachable("bad hash algorithm for RSA PKCS#1");
}

static inline size_t rsa_pkcs1_length_of_fixed_parts(const ssh_hashalg *halg)
{
    ptrlen asn1_prefix = rsa_pkcs1_prefix_for_hash(halg);
    return halg->hlen + asn1_prefix.len + 2;
}

static unsigned char *rsa_pkcs1_signature_string(
    size_t nbytes, const ssh_hashalg *halg, ptrlen data)
{
    size_t fixed_parts = rsa_pkcs1_length_of_fixed_parts(halg);
    pinitassert(nbytes >= fixed_parts);
    size_t padding = nbytes - fixed_parts;

    ptrlen asn1_prefix = rsa_pkcs1_prefix_for_hash(halg);

    unsigned char *bytes = snewn(nbytes, unsigned char);

    bytes[0] = 0;
    bytes[1] = 1;

    memset(bytes + 2, 0xFF, padding);

    memcpy(bytes + 2 + padding, asn1_prefix.ptr, asn1_prefix.len);

    { // WINSCP
    ssh_hash *h = ssh_hash_new(halg);
    put_datapl(h, data);
    ssh_hash_final(h, bytes + 2 + padding + asn1_prefix.len);
    } // WINSCP

    return bytes;
}

static bool rsa2_verify(ssh_key *key, ptrlen sig, ptrlen data)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    BinarySource src[1];
    ptrlen type, in_pl;
    mp_int *in, *out;

    const struct ssh2_rsa_extra *extra =
        (const struct ssh2_rsa_extra *)key->vt->extra;

    const ssh_hashalg *halg = rsa2_hash_alg_for_flags(extra->signflags, NULL);

    /* Start by making sure the key is even long enough to encode a
     * signature. If not, everything fails to verify. */
    size_t nbytes = (mp_get_nbits(rsa->modulus) + 7) / 8;
    if (nbytes < rsa_pkcs1_length_of_fixed_parts(halg))
        return false;

    BinarySource_BARE_INIT_PL(src, sig);
    type = get_string(src);
    /*
     * RFC 4253 section 6.6: the signature integer in an ssh-rsa
     * signature is 'without lengths or padding'. That is, we _don't_
     * expect the usual leading zero byte if the topmost bit of the
     * first byte is set. (However, because of the possibility of
     * BUG_SSH2_RSA_PADDING at the other end, we tolerate it if it's
     * there.) So we can't use get_mp_ssh2, which enforces that
     * leading-byte scheme; instead we use get_string and
     * mp_from_bytes_be, which will tolerate anything.
     */
    in_pl = get_string(src);
    if (get_err(src) || !ptrlen_eq_string(type, key->vt->ssh_id))
        return false;

    in = mp_from_bytes_be(in_pl);
    out = mp_modpow(in, rsa->exponent, rsa->modulus);
    mp_free(in);


    { // WINSCP
    unsigned diff = 0;

    unsigned char *bytes = rsa_pkcs1_signature_string(nbytes, halg, data);
    size_t i; // WINSCP
    for (i = 0; i < nbytes; i++)
        diff |= bytes[nbytes-1 - i] ^ mp_get_byte(out, i);
    smemclr(bytes, nbytes);
    sfree(bytes);
    mp_free(out);

    return diff == 0;
    } // WINSCP
}

static void rsa2_sign(ssh_key *key, ptrlen data,
                      unsigned flags, BinarySink *bs)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    unsigned char *bytes;
    size_t nbytes;
    mp_int *in, *out;
    const ssh_hashalg *halg;
    const char *sign_alg_name;

    const struct ssh2_rsa_extra *extra =
        (const struct ssh2_rsa_extra *)key->vt->extra;
    flags |= extra->signflags;

    halg = rsa2_hash_alg_for_flags(flags, &sign_alg_name);

    nbytes = (mp_get_nbits(rsa->modulus) + 7) / 8;

    bytes = rsa_pkcs1_signature_string(nbytes, halg, data);
    in = mp_from_bytes_be(make_ptrlen(bytes, nbytes));
    smemclr(bytes, nbytes);
    sfree(bytes);

    out = rsa_privkey_op(in, rsa);
    mp_free(in);

    put_stringz(bs, sign_alg_name);
    if (flags == 0) {
        /*
         * Original "ssh-rsa", per RFC 4253 section 6.6, stores the
         * signature integer in a string without padding - not even
         * the leading zero byte that an ordinary SSH-2 mpint would
         * require to avoid looking like two's complement.
         *
         * "The value for 'rsa_signature_blob' is encoded as a string
         * containing s (which is an integer, without lengths or
         * padding, unsigned, and in network byte order)."
         */
        nbytes = (mp_get_nbits(out) + 7) / 8;
    } else {
        /*
         * The SHA-256 and SHA-512 signature systems, per RFC 8332
         * section 3, should be padded to the length of the key
         * modulus.
         *
         * "The value for 'rsa_signature_blob' is encoded as a string
         * that contains an octet string S (which is the output of
         * RSASSA-PKCS1-v1_5) and that has the same length (in octets)
         * as the RSA modulus."
         *
         * Awkwardly, RFC 8332 doesn't say whether that means the
         * 'raw' length of the RSA modulus (that is, ceil(n/8) for an
         * n-bit key) or the length it would occupy as an SSH-2 mpint.
         * My interpretation is the former.
         */
        nbytes = (mp_get_nbits(rsa->modulus) + 7) / 8;
    }
    put_uint32(bs, nbytes);
    { // WINSCP
    size_t i; // WINSCP
    for (i = 0; i < nbytes; i++)
        put_byte(bs, mp_get_byte(out, nbytes - 1 - i));
    } // WINSCP

    mp_free(out);
}

static char *rsa2_invalid(ssh_key *key, unsigned flags)
{
    RSAKey *rsa = container_of(key, RSAKey, sshk);
    size_t bits = mp_get_nbits(rsa->modulus), nbytes = (bits + 7) / 8;
    const char *sign_alg_name;
    const ssh_hashalg *halg = rsa2_hash_alg_for_flags(flags, &sign_alg_name);
    if (nbytes < rsa_pkcs1_length_of_fixed_parts(halg)) {
        return dupprintf(
            "%"SIZEu"-bit RSA key is too short to generate %s signatures",
            bits, sign_alg_name);
    }

    return NULL;
}

static unsigned ssh_rsa_supported_flags(const ssh_keyalg *self)
{
    return SSH_AGENT_RSA_SHA2_256 | SSH_AGENT_RSA_SHA2_512;
}

static const char *ssh_rsa_alternate_ssh_id(
    const ssh_keyalg *self, unsigned flags)
{
    if (flags & SSH_AGENT_RSA_SHA2_512)
        return ssh_rsa_sha512.ssh_id;
    if (flags & SSH_AGENT_RSA_SHA2_256)
        return ssh_rsa_sha256.ssh_id;
    return self->ssh_id;
}

static char *rsa2_alg_desc(const ssh_keyalg *self) { return dupstr("RSA"); }

static const struct ssh2_rsa_extra
    rsa_extra = { 0 },
    rsa_sha256_extra = { SSH_AGENT_RSA_SHA2_256 },
    rsa_sha512_extra = { SSH_AGENT_RSA_SHA2_512 };

// WINSCP
#define COMMON_KEYALG_FIELDS                    \
    /*.new_pub =*/ rsa2_new_pub,                    \
    /*.new_priv =*/ rsa2_new_priv,                  \
    /*.new_priv_openssh =*/ rsa2_new_priv_openssh,  \
    /*.freekey =*/ rsa2_freekey,                    \
    /*.invalid =*/ rsa2_invalid,                    \
    /*.sign =*/ rsa2_sign,                          \
    /*.verify =*/ rsa2_verify,                      \
    /*.public_blob =*/ rsa2_public_blob,            \
    /*.private_blob =*/ rsa2_private_blob,          \
    /*.openssh_blob =*/ rsa2_openssh_blob,          \
    /*.has_private =*/ rsa2_has_private,            \
    /*.cache_str =*/ rsa2_cache_str,                \
    /*.components =*/ rsa2_components,              \
    /*.base_key =*/ nullkey_base_key,               \
    NULL, NULL, NULL, NULL, \
    /*.pubkey_bits =*/ rsa2_pubkey_bits
#define COMMON_KEYALG_FIELDS1a \
    /*.alg_desc =*/ rsa2_alg_desc,                  \
    /*.variable_size =*/ nullkey_variable_size_yes, \
    NULL
#define COMMON_KEYALG_FIELDS2 \
    /*.cache_id =*/ "rsa2"
#define COMMON_KEYALG_FIELDS3 \
    false, NULL

const ssh_keyalg ssh_rsa = {
    // WINSCP
    COMMON_KEYALG_FIELDS,
    /*.supported_flags =*/ ssh_rsa_supported_flags,
    /*.alternate_ssh_id =*/ ssh_rsa_alternate_ssh_id,
    COMMON_KEYALG_FIELDS1a,
    /*.ssh_id =*/ "ssh-rsa",
    COMMON_KEYALG_FIELDS2,
    /*.extra =*/ &rsa_extra,
    COMMON_KEYALG_FIELDS3,
};

const ssh_keyalg ssh_rsa_sha256 = {
    // WINSCP
    COMMON_KEYALG_FIELDS,
    /*.supported_flags =*/ nullkey_supported_flags,
    /*.alternate_ssh_id =*/ nullkey_alternate_ssh_id,
    COMMON_KEYALG_FIELDS1a,
    /*.ssh_id =*/ "rsa-sha2-256",
    COMMON_KEYALG_FIELDS2,
    /*.extra =*/ &rsa_sha256_extra,
    COMMON_KEYALG_FIELDS3,
};

const ssh_keyalg ssh_rsa_sha512 = {
    // WINSCP
    COMMON_KEYALG_FIELDS,
    /*.supported_flags =*/ nullkey_supported_flags,
    /*.alternate_ssh_id =*/ nullkey_alternate_ssh_id,
    COMMON_KEYALG_FIELDS1a,
    /*.ssh_id =*/ "rsa-sha2-512",
    COMMON_KEYALG_FIELDS2,
    /*.extra =*/ &rsa_sha512_extra,
    COMMON_KEYALG_FIELDS3,
};

RSAKey *ssh_rsakex_newkey(ptrlen data)
{
    ssh_key *sshk = rsa2_new_pub(&ssh_rsa, data);
    if (!sshk)
        return NULL;
    return container_of(sshk, RSAKey, sshk);
}

void ssh_rsakex_freekey(RSAKey *key)
{
    rsa2_freekey(&key->sshk);
}

int ssh_rsakex_klen(RSAKey *rsa)
{
    return mp_get_nbits(rsa->modulus);
}

static void oaep_mask(const ssh_hashalg *h, void *seed, int seedlen,
                      void *vdata, int datalen)
{
    unsigned char *data = (unsigned char *)vdata;
    unsigned count = 0;

    ssh_hash *s = ssh_hash_new(h);

    while (datalen > 0) {
        int i, max = (datalen > h->hlen ? h->hlen : datalen);
        unsigned char hash[MAX_HASH_LEN];

        ssh_hash_reset(s);
        assert(h->hlen <= MAX_HASH_LEN);
        put_data(s, seed, seedlen);
        put_uint32(s, count);
        ssh_hash_digest(s, hash);
        count++;

        for (i = 0; i < max; i++)
            data[i] ^= hash[i];

        data += max;
        datalen -= max;
    }

    ssh_hash_free(s);
}

strbuf *ssh_rsakex_encrypt(RSAKey *rsa, const ssh_hashalg *h, ptrlen in)
{
    mp_int *b1, *b2;
    int k, i;
    char *p;
    const int HLEN = h->hlen;

    /*
     * Here we encrypt using RSAES-OAEP. Essentially this means:
     *
     *  - we have a SHA-based `mask generation function' which
     *    creates a pseudo-random stream of mask data
     *    deterministically from an input chunk of data.
     *
     *  - we have a random chunk of data called a seed.
     *
     *  - we use the seed to generate a mask which we XOR with our
     *    plaintext.
     *
     *  - then we use _the masked plaintext_ to generate a mask
     *    which we XOR with the seed.
     *
     *  - then we concatenate the masked seed and the masked
     *    plaintext, and RSA-encrypt that lot.
     *
     * The result is that the data input to the encryption function
     * is random-looking and (hopefully) contains no exploitable
     * structure such as PKCS1-v1_5 does.
     *
     * For a precise specification, see RFC 3447, section 7.1.1.
     * Some of the variable names below are derived from that, so
     * it'd probably help to read it anyway.
     */

    /* k denotes the length in octets of the RSA modulus. */
    k = (7 + mp_get_nbits(rsa->modulus)) / 8;

    /* The length of the input data must be at most k - 2hLen - 2. */
    assert(in.len > 0 && in.len <= k - 2*HLEN - 2);

    /* The length of the output data wants to be precisely k. */
    { // WINSCP
    strbuf *toret = strbuf_new_nm();
    int outlen = k;
    unsigned char *out = strbuf_append(toret, outlen);

    /*
     * Now perform EME-OAEP encoding. First set up all the unmasked
     * output data.
     */
    /* Leading byte zero. */
    out[0] = 0;
    /* At position 1, the seed: HLEN bytes of random data. */
    random_read(out + 1, HLEN);
    /* At position 1+HLEN, the data block DB, consisting of: */
    /* The hash of the label (we only support an empty label here) */
    hash_simple(h, PTRLEN_LITERAL(""), out + HLEN + 1);
    /* A bunch of zero octets */
    memset(out + 2*HLEN + 1, 0, outlen - (2*HLEN + 1));
    /* A single 1 octet, followed by the input message data. */
    out[outlen - in.len - 1] = 1;
    memcpy(out + outlen - in.len, in.ptr, in.len);

    /*
     * Now use the seed data to mask the block DB.
     */
    oaep_mask(h, out+1, HLEN, out+HLEN+1, outlen-HLEN-1);

    /*
     * And now use the masked DB to mask the seed itself.
     */
    oaep_mask(h, out+HLEN+1, outlen-HLEN-1, out+1, HLEN);

    /*
     * Now `out' contains precisely the data we want to
     * RSA-encrypt.
     */
    b1 = mp_from_bytes_be(make_ptrlen(out, outlen));
    b2 = mp_modpow(b1, rsa->exponent, rsa->modulus);
    p = (char *)out;
    for (i = outlen; i--;) {
        *p++ = mp_get_byte(b2, i);
    }
    mp_free(b1);
    mp_free(b2);

    /*
     * And we're done.
     */
    return toret;
    } // WINSCP
}

mp_int *ssh_rsakex_decrypt(
    RSAKey *rsa, const ssh_hashalg *h, ptrlen ciphertext)
{
    mp_int *b1, *b2;
    int outlen, i;
    unsigned char *out;
    unsigned char labelhash[64];
    BinarySource src[1];
    const int HLEN = h->hlen;

    /*
     * Decryption side of the RSA key exchange operation.
     */

    /* The length of the encrypted data should be exactly the length
     * in octets of the RSA modulus.. */
    outlen = (7 + mp_get_nbits(rsa->modulus)) / 8;
    if (ciphertext.len != outlen)
        return NULL;

    /* Do the RSA decryption, and extract the result into a byte array. */
    b1 = mp_from_bytes_be(ciphertext);
    b2 = rsa_privkey_op(b1, rsa);
    out = snewn(outlen, unsigned char);
    for (i = 0; i < outlen; i++)
        out[i] = mp_get_byte(b2, outlen-1-i);
    mp_free(b1);
    mp_free(b2);

    /* Do the OAEP masking operations, in the reverse order from encryption */
    oaep_mask(h, out+HLEN+1, outlen-HLEN-1, out+1, HLEN);
    oaep_mask(h, out+1, HLEN, out+HLEN+1, outlen-HLEN-1);

    /* Check the leading byte is zero. */
    if (out[0] != 0) {
        sfree(out);
        return NULL;
    }
    /* Check the label hash at position 1+HLEN */
    assert(HLEN <= lenof(labelhash));
    hash_simple(h, PTRLEN_LITERAL(""), labelhash);
    if (memcmp(out + HLEN + 1, labelhash, HLEN)) {
        sfree(out);
        return NULL;
    }
    /* Expect zero bytes followed by a 1 byte */
    for (i = 1 + 2 * HLEN; i < outlen; i++) {
        if (out[i] == 1) {
            i++;  /* skip over the 1 byte */
            break;
        } else if (out[i] != 0) {
            sfree(out);
            return NULL;
        }
    }
    /* And what's left is the input message data, which should be
     * encoded as an ordinary SSH-2 mpint. */
    BinarySource_BARE_INIT(src, out + i, outlen - i);
    b1 = get_mp_ssh2(src);
    sfree(out);
    if (get_err(src) || get_avail(src) != 0) {
        mp_free(b1);
        return NULL;
    }

    /* Success! */
    return b1;
}

static const struct ssh_rsa_kex_extra ssh_rsa_kex_extra_sha1 = { 1024 };
static const struct ssh_rsa_kex_extra ssh_rsa_kex_extra_sha256 = { 2048 };

static const ssh_kex ssh_rsa_kex_sha1 = {
    /*.name =*/ "rsa1024-sha1",
    NULL,
    /*.main_type =*/ KEXTYPE_RSA,
    /*.hash =*/ &ssh_sha1,
    NULL, // WINSCP
    /*.extra =*/ &ssh_rsa_kex_extra_sha1,
};

static const ssh_kex ssh_rsa_kex_sha256 = {
    /*.name =*/ "rsa2048-sha256",
    NULL,
    /*.main_type =*/ KEXTYPE_RSA,
    /*.hash =*/ &ssh_sha256,
    NULL, // WINSCP
    /*.extra =*/ &ssh_rsa_kex_extra_sha256,
};

static const ssh_kex *const rsa_kex_list[] = {
    &ssh_rsa_kex_sha256,
    &ssh_rsa_kex_sha1
};

const ssh_kexes ssh_rsa_kex = { lenof(rsa_kex_list), rsa_kex_list };
