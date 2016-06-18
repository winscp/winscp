/*
 * Header file shared between sshblowf.c and sshbcrypt.c. Exposes the
 * internal Blowfish routines needed by bcrypt.
 */

typedef struct BlowfishContext BlowfishContext;

void *blowfish_make_context(void);
void blowfish_free_context(void *handle);
void blowfish_initkey(BlowfishContext *ctx);
void blowfish_expandkey(BlowfishContext *ctx,
                        const unsigned char *key, short keybytes,
                        const unsigned char *salt, short saltbytes);
void blowfish_lsb_encrypt_ecb(unsigned char *blk, int len,
                              BlowfishContext *ctx);
