/*
 * Convenience functions to encrypt and decrypt PuTTY's own .PPK
 * format for SSH-2 private key files, which uses 256-bit AES in CBC
 * mode.
 */

#include "ssh.h"

static ssh_cipher *aes256_pubkey_cipher(const void *key, const void *iv)
{
    ssh_cipher *cipher = ssh_cipher_new(&ssh_aes256_cbc);
    ssh_cipher_setkey(cipher, key);
    ssh_cipher_setiv(cipher, iv);
    return cipher;
}

void aes256_encrypt_pubkey(const void *key, const void *iv, void *blk, int len)
{
    ssh_cipher *c = aes256_pubkey_cipher(key, iv);
    ssh_cipher_encrypt(c, blk, len);
    ssh_cipher_free(c);
}

void aes256_decrypt_pubkey(const void *key, const void *iv, void *blk, int len)
{
    ssh_cipher *c = aes256_pubkey_cipher(key, iv);
    ssh_cipher_decrypt(c, blk, len);
    ssh_cipher_free(c);
}
