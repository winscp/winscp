/*
 * EC key generation.
 */

#include "ssh.h"

/* Forward reference from sshecc.c */
struct ec_point *ecp_mul(const struct ec_point *a, const Bignum b);

int ec_generate(struct ec_key *key, int bits, progfn_t pfn,
                void *pfnparam)
{
    struct ec_point *publicKey;

    if (!ec_nist_alg_and_curve_by_bits(bits, &key->publicKey.curve,
                                       &key->signalg))
        return 0;

    key->privateKey = bignum_random_in_range(One, key->publicKey.curve->w.n);
    if (!key->privateKey) return 0;

    publicKey = ec_public(key->privateKey, key->publicKey.curve);
    if (!publicKey) {
        freebn(key->privateKey);
        key->privateKey = NULL;
        return 0;
    }

    key->publicKey.x = publicKey->x;
    key->publicKey.y = publicKey->y;
    key->publicKey.z = NULL;
    sfree(publicKey);

    return 1;
}

int ec_edgenerate(struct ec_key *key, int bits, progfn_t pfn,
                  void *pfnparam)
{
    struct ec_point *publicKey;

    if (!ec_ed_alg_and_curve_by_bits(bits, &key->publicKey.curve,
                                     &key->signalg))
        return 0;

    {
        /* EdDSA secret keys are just 32 bytes of hash preimage; the
         * 64-byte SHA-512 hash of that key will be used when signing,
         * but the form of the key stored on disk is the preimage
         * only. */
        Bignum privMax = bn_power_2(bits);
        if (!privMax) return 0;
        key->privateKey = bignum_random_in_range(Zero, privMax);
        freebn(privMax);
        if (!key->privateKey) return 0;
    }

    publicKey = ec_public(key->privateKey, key->publicKey.curve);
    if (!publicKey) {
        freebn(key->privateKey);
        key->privateKey = NULL;
        return 0;
    }

    key->publicKey.x = publicKey->x;
    key->publicKey.y = publicKey->y;
    key->publicKey.z = NULL;
    sfree(publicKey);

    return 1;
}
