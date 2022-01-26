/*
 * Convenience functions to encrypt and decrypt the cookies used in
 * XDM-AUTHORIZATION-1. 
 */

#include "ssh.h"

static ssh_cipher *des_xdmauth_cipher(const void *vkeydata)
{
    /*
     * XDM-AUTHORIZATION-1 uses single-DES, but packs the key into 7
     * bytes, so here we have to repack it manually into the canonical
     * form where it occupies 8 bytes each with the low bit unused.
     */
    const unsigned char *keydata = (const unsigned char *)vkeydata;
    unsigned char key[8];
    int i, nbits, j;
    unsigned int bits;

    bits = 0;
    nbits = 0;
    j = 0;
    for (i = 0; i < 8; i++) {
        if (nbits < 7) {
            bits = (bits << 8) | keydata[j];
            nbits += 8;
            j++;
        }
        key[i] = (bits >> (nbits - 7)) << 1;
        bits &= ~(0x7F << (nbits - 7));
        nbits -= 7;
    }

    ssh_cipher *c = ssh_cipher_new(&ssh_des);
    ssh_cipher_setkey(c, key);
    smemclr(key, sizeof(key));
    ssh_cipher_setiv(c, key);
    return c;
}

void des_encrypt_xdmauth(const void *keydata, void *blk, int len)
{
    ssh_cipher *c = des_xdmauth_cipher(keydata);
    ssh_cipher_encrypt(c, blk, len);
    ssh_cipher_free(c);
}

void des_decrypt_xdmauth(const void *keydata, void *blk, int len)
{
    ssh_cipher *c = des_xdmauth_cipher(keydata);
    ssh_cipher_decrypt(c, blk, len);
    ssh_cipher_free(c);
}
