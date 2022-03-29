/*
 * Convenience functions to encrypt and decrypt OpenSSH PEM format for
 * SSH-2 private key files. This uses triple-DES in SSH-2 style (one
 * CBC layer), with three distinct keys, and an IV also generated from
 * the passphrase.
 */

#include "ssh.h"

static ssh_cipher *des3_pubkey_ossh_cipher(const void *vkey, const void *viv)
{
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
