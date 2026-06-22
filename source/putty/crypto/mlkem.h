/*
 * Internal functions for the ML-KEM cryptosystem, exposed in a header
 * that is expected to be included only by mlkem.c and test programs.
 */

#ifndef PUTTY_CRYPTO_MLKEM_H
#define PUTTY_CRYPTO_MLKEM_H

typedef struct mlkem_params mlkem_params;

extern const mlkem_params mlkem_params_512;
extern const mlkem_params mlkem_params_768;
extern const mlkem_params mlkem_params_1024;

/*
 * ML-KEM key generation.
 *
 * The official spec gives two APIs for this function: an outer one
 * that invents random data from an implicit PRNG parameter, and an
 * inner one that takes the randomness as explicit input for running
 * test vectors.
 *
 * To make side-channel testing easier, I introduce a third API inside
 * the latter. The spec's "inner" function takes a parameter 'd'
 * containing 32 bytes of randomness, which it immediately expands
 * into a 64-byte hash and then uses the two halves of that hash for
 * different purposes. My even-more-inner function expects the caller
 * to have done that hashing already, and to present the two 32-byte
 * half-hashes rho and sigma separately.
 *
 * Rationale: it would be difficult to make the keygen running time
 * independent of rho, becase the required technique for constructing
 * a matrix from rho uses rejection sampling, so timing will depend on
 * how many samples were rejected. Happily, it's also not _necessary_
 * to make the timing independent of rho, because rho is part of the
 * _public_ key, so it's sent in clear over the wire anyway. So for
 * testsc purposes, it's convenient to regard rho as fixed and vary
 * sigma, so that the timing variations due to rho don't show up as
 * failures in the test.
 *
 * Inputs: 'd', 'z', 'rho' and 'sigma' are all 32-byte random strings.
 *
 * Return: the encryption and decryption keys are written to the two
 * provided BinarySinks.
 */
void mlkem_keygen(
    BinarySink *ek, BinarySink *dk, const mlkem_params *params);
void mlkem_keygen_internal(
    BinarySink *ek, BinarySink *dk, const mlkem_params *params,
    const void *d, const void *z);
void mlkem_keygen_rho_sigma(
    BinarySink *ek, BinarySink *dk, const mlkem_params *params,
    const void *rho, const void *sigma, const void *z);

/*
 * ML-KEM key encapsulation, with only two forms, the outer (random)
 * and inner (for test vectors) versions from the spec.
 *
 * Inputs: the encryption key from keygen. 'm' should be a 32-byte
 * random string if provided.
 *
 * Returns: if successful, returns true, and writes to the two
 * BinarySinks a ciphertext to send to the other side, and our copy of
 * the output shared secret k. If failure, returns false, and the
 * strbuf pointers aren't filled in at all.
 */
bool mlkem_encaps(BinarySink *ciphertext, BinarySink *kout,
                  const mlkem_params *params, ptrlen ek);
bool mlkem_encaps_internal(BinarySink *ciphertext, BinarySink *kout,
                           const mlkem_params *params, ptrlen ek,
                           const void *m);

/*
 * ML-KEM key decapsulation. This doesn't use any randomness, so even
 * the official spec only presents one version of it. (Actually it
 * defines two functions, but the outer one adds nothing over the
 * inner one.)
 *
 * Inputs: the decryption key from keygen, and the ciphertext output
 * from encapsulation.
 *
 * Returns: false on validation failure, and true otherwise
 * (regardless of whether the ciphertext was implicitly rejected). The
 * shared secret k is written to the provided BinarySink.
 */
bool mlkem_decaps(BinarySink *k, const mlkem_params *params,
                  ptrlen dk, ptrlen c);

#endif /* PUTTY_CRYPTO_MLKEM_H */
