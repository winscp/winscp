/*
 * sshauxcrypt.c: wrapper functions on crypto primitives for use in
 * other contexts than the main SSH packet protocol, such as
 * encrypting private key files and performing XDM-AUTHORIZATION-1.
 *
 * These all work through the standard cipher/hash/MAC APIs, so they
 * don't need to live in the same actual source files as the ciphers
 * they wrap, and I think it keeps things tidier to have them out of
 * the way here instead.
 */

#include "ssh.h"

static ssh_cipher *aes256_pubkey_cipher(const void *key)
{
    /*
     * PuTTY's own .PPK format for SSH-2 private key files is
     * encrypted with 256-bit AES in CBC mode.
     */
    char iv[16];
    memset(iv, 0, 16);
    ssh_cipher *cipher = ssh_cipher_new(&ssh_aes256_cbc);
    ssh_cipher_setkey(cipher, key);
    ssh_cipher_setiv(cipher, iv);
    return cipher;
}

void aes256_encrypt_pubkey(const void *key, void *blk, int len)
{
    ssh_cipher *c = aes256_pubkey_cipher(key);
    ssh_cipher_encrypt(c, blk, len);
    ssh_cipher_free(c);
}

void aes256_decrypt_pubkey(const void *key, void *blk, int len)
{
    ssh_cipher *c = aes256_pubkey_cipher(key);
    ssh_cipher_decrypt(c, blk, len);
    ssh_cipher_free(c);
}

static ssh_cipher *des3_pubkey_cipher(const void *vkey)
{
    /*
     * SSH-1 private key files are encrypted with triple-DES in SSH-1
     * style (three separate CBC layers), but the same key is used for
     * the first and third layers.
     */
    ssh_cipher *c = ssh_cipher_new(&ssh_3des_ssh1);
    uint8_t keys3[24], iv[8];

    memcpy(keys3, vkey, 16);
    memcpy(keys3 + 16, vkey, 8);
    ssh_cipher_setkey(c, keys3);
    smemclr(keys3, sizeof(keys3));

    memset(iv, 0, 8);
    ssh_cipher_setiv(c, iv);

    return c;
}

void des3_decrypt_pubkey(const void *vkey, void *vblk, int len)
{
    ssh_cipher *c = des3_pubkey_cipher(vkey);
    ssh_cipher_decrypt(c, vblk, len);
    ssh_cipher_free(c);
}

void des3_encrypt_pubkey(const void *vkey, void *vblk, int len)
{
    ssh_cipher *c = des3_pubkey_cipher(vkey);
    ssh_cipher_encrypt(c, vblk, len);
    ssh_cipher_free(c);
}

static ssh_cipher *des3_pubkey_ossh_cipher(const void *vkey, const void *viv)
{
    /*
     * OpenSSH PEM private key files are encrypted with triple-DES in
     * SSH-2 style (one CBC layer), with three distinct keys, and an
     * IV also generated from the passphrase.
     */
    ssh_cipher *c = ssh_cipher_new(&ssh_3des_ssh2);
    ssh_cipher_setkey(c, vkey);
    ssh_cipher_setiv(c, viv);
    return c;
}

void des3_decrypt_pubkey_ossh(const void *vkey, const void *viv,
			      void *vblk, int len)
{
    ssh_cipher *c = des3_pubkey_ossh_cipher(vkey, viv);
    ssh_cipher_decrypt(c, vblk, len);
    ssh_cipher_free(c);
}

void des3_encrypt_pubkey_ossh(const void *vkey, const void *viv,
			      void *vblk, int len)
{
    ssh_cipher *c = des3_pubkey_ossh_cipher(vkey, viv);
    ssh_cipher_encrypt(c, vblk, len);
    ssh_cipher_free(c);
}

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

void hash_simple(const ssh_hashalg *alg, ptrlen data, void *output)
{
    ssh_hash *hash = ssh_hash_new(alg);
    put_datapl(hash, data);
    ssh_hash_final(hash, output);
}

void mac_simple(const ssh2_macalg *alg, ptrlen key, ptrlen data, void *output)
{
    ssh2_mac *mac = ssh2_mac_new(alg, NULL);
    ssh2_mac_setkey(mac, key);
    ssh2_mac_start(mac);
    put_datapl(mac, data);
    ssh2_mac_genresult(mac, output);
    ssh2_mac_free(mac);
}
