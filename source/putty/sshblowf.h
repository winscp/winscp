/*
 * Header file shared between sshblowf.c and sshbcrypt.c. Exposes the
 * internal Blowfish routines needed by bcrypt.
 */

typedef struct BlowfishContext BlowfishContext;

BlowfishContext *blowfish_make_context(void);
void blowfish_free_context(BlowfishContext *ctx);
void blowfish_initkey(BlowfishContext *ctx);
void blowfish_expandkey(BlowfishContext *ctx,
                        const void *key, short keybytes,
                        const void *salt, short saltbytes);
void blowfish_lsb_encrypt_ecb(void *blk, int len, BlowfishContext *ctx);
