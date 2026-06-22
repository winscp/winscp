/*
 * Internal functions for the NTRU cryptosystem, exposed in a header
 * that is expected to be included only by ntru.c and test programs.
 */

#ifndef PUTTY_CRYPTO_NTRU_H
#define PUTTY_CRYPTO_NTRU_H

unsigned ntru_ring_invert(uint16_t *out, const uint16_t *in,
                          unsigned p, unsigned q);
void ntru_ring_multiply(uint16_t *out, const uint16_t *a, const uint16_t *b,
                        unsigned p, unsigned q);
void ntru_mod3(uint16_t *out, const uint16_t *in, unsigned p, unsigned q);
void ntru_round3(uint16_t *out, const uint16_t *in, unsigned p, unsigned q);
void ntru_bias(uint16_t *out, const uint16_t *in, unsigned bias,
               unsigned p, unsigned q);
void ntru_scale(uint16_t *out, const uint16_t *in, uint16_t scale,
                unsigned p, unsigned q);

NTRUEncodeSchedule *ntru_encode_schedule(const uint16_t *ms_in, size_t n);
void ntru_encode_schedule_free(NTRUEncodeSchedule *sched);
size_t ntru_encode_schedule_length(NTRUEncodeSchedule *sched);
size_t ntru_encode_schedule_nvals(NTRUEncodeSchedule *sched);
void ntru_encode(NTRUEncodeSchedule *sched, const uint16_t *rs_in,
                 BinarySink *bs);
void ntru_decode(NTRUEncodeSchedule *sched, uint16_t *rs_out, ptrlen data);

void ntru_gen_short(uint16_t *v, unsigned p, unsigned w);

NTRUKeyPair *ntru_keygen_attempt(unsigned p, unsigned q, unsigned w);
NTRUKeyPair *ntru_keygen(unsigned p, unsigned q, unsigned w);
void ntru_keypair_free(NTRUKeyPair *keypair);

void ntru_encrypt(uint16_t *ciphertext, const uint16_t *plaintext,
                  uint16_t *pubkey, unsigned p, unsigned q);
void ntru_decrypt(uint16_t *plaintext, const uint16_t *ciphertext,
                  NTRUKeyPair *keypair);

void ntru_encode_pubkey(const uint16_t *pubkey, unsigned p, unsigned q,
                        BinarySink *bs);
ptrlen ntru_decode_pubkey(uint16_t *pubkey, unsigned p, unsigned q,
                          BinarySource *src);
void ntru_encode_ciphertext(const uint16_t *ciphertext, unsigned p, unsigned q,
                            BinarySink *bs);
ptrlen ntru_decode_ciphertext(uint16_t *ct, NTRUKeyPair *keypair,
                              BinarySource *src);
void ntru_encode_plaintext(const uint16_t *plaintext, unsigned p,
                           BinarySink *bs);

unsigned ntru_keypair_p(NTRUKeyPair *keypair);
const uint16_t *ntru_pubkey(NTRUKeyPair *keypair);

#endif /* PUTTY_CRYPTO_NTRU_H */
