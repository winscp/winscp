/*
 * 'bcrypt' password hash function, for PuTTY's import/export of
 * OpenSSH encrypted private key files.
 *
 * This is not really the same as the original bcrypt; OpenSSH has
 * modified it in various ways, and of course we have to do the same.
 */

#include <stddef.h>
#include <string.h>
#include "ssh.h"
#include "sshblowf.h"

BlowfishContext *bcrypt_setup(const unsigned char *key, int keybytes,
                              const unsigned char *salt, int saltbytes)
{
    int i;
    BlowfishContext *ctx;

    ctx = blowfish_make_context();
    blowfish_initkey(ctx);
    blowfish_expandkey(ctx, key, keybytes, salt, saltbytes);

    /* Original bcrypt replaces this fixed loop count with the
     * variable cost. OpenSSH instead iterates the whole thing more
     * than once if it wants extra rounds. */
    for (i = 0; i < 64; i++) {
        blowfish_expandkey(ctx, salt, saltbytes, NULL, 0);
        blowfish_expandkey(ctx, key, keybytes, NULL, 0);
    }

    return ctx;
}

void bcrypt_hash(const unsigned char *key, int keybytes,
                 const unsigned char *salt, int saltbytes,
                 unsigned char output[32])
{
    BlowfishContext *ctx;
    int i;

    ctx = bcrypt_setup(key, keybytes, salt, saltbytes);
    /* This was quite a nice starting string until it ran into
     * little-endian Blowfish :-/ */
    memcpy(output, "cyxOmorhcitawolBhsiftawSanyDetim", 32);
    for (i = 0; i < 64; i++) {
        blowfish_lsb_encrypt_ecb(output, 32, ctx);
    }
    blowfish_free_context(ctx);
}

void bcrypt_genblock(int counter,
                     const unsigned char hashed_passphrase[64],
                     const unsigned char *salt, int saltbytes,
                     unsigned char output[32])
{
    unsigned char hashed_salt[64];

    /* Hash the input salt with the counter value optionally suffixed
     * to get our real 32-byte salt */
    ssh_hash *h = ssh_hash_new(&ssh_sha512);
    put_data(h, salt, saltbytes);
    if (counter)
        put_uint32(h, counter);
    ssh_hash_final(h, hashed_salt);

    bcrypt_hash(hashed_passphrase, 64, hashed_salt, 64, output);

    smemclr(&hashed_salt, sizeof(hashed_salt));
}

void openssh_bcrypt(const char *passphrase,
                    const unsigned char *salt, int saltbytes,
                    int rounds, unsigned char *out, int outbytes)
{
    unsigned char hashed_passphrase[64];
    unsigned char block[32], outblock[32];
    const unsigned char *thissalt;
    int thissaltbytes;
    int modulus, residue, i, j, round;

    /* Hash the passphrase to get the bcrypt key material */
    hash_simple(&ssh_sha512, ptrlen_from_asciz(passphrase), hashed_passphrase);

    /* We output key bytes in a scattered fashion to meld all output
     * key blocks into all parts of the output. To do this, we pick a
     * modulus, and we output the key bytes to indices of out[] in the
     * following order: first the indices that are multiples of the
     * modulus, then the ones congruent to 1 mod modulus, etc. Each of
     * those passes consumes exactly one block output from
     * bcrypt_genblock, so we must pick a modulus large enough that at
     * most 32 bytes are used in the pass. */
    modulus = (outbytes + 31) / 32;

    for (residue = 0; residue < modulus; residue++) {
        /* Our output block of data is the XOR of all blocks generated
         * by bcrypt in the following loop */
        memset(outblock, 0, sizeof(outblock));

        thissalt = salt;
        thissaltbytes = saltbytes;
        for (round = 0; round < rounds; round++) {
            bcrypt_genblock(round == 0 ? residue+1 : 0,
                            hashed_passphrase,
                            thissalt, thissaltbytes, block);
            /* Each subsequent bcrypt call reuses the previous one's
             * output as its salt */
            thissalt = block;
            thissaltbytes = 32;

            for (i = 0; i < 32; i++)
                outblock[i] ^= block[i];
        }

        for (i = residue, j = 0; i < outbytes; i += modulus, j++)
            out[i] = outblock[j];
    }
    smemclr(&hashed_passphrase, sizeof(hashed_passphrase));
}
