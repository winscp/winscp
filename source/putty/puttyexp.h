#ifndef PUTTY_PUTTYEXP_H
#define PUTTY_PUTTYEXP_H

#include "defs.h"

// from ssh.c

struct ssh1_cipheralg;
typedef const struct ssh1_cipheralg *ssh1_cipher;
struct ssh2_cipheralg;
typedef const struct ssh2_cipheralg *ssh2_cipher;

int is_ssh(Plug plug);
int get_ssh_version(Backend * be);
void * get_ssh_frontend(Plug plug);
const ssh1_cipher * get_cipher(Backend * be);
const ssh2_cipher * get_cscipher(Backend * be);
const ssh2_cipher * get_sccipher(Backend * be);
const struct ssh_compressor * get_cscomp(Backend * be);
const struct ssh_decompressor * get_sccomp(Backend * be);
#define WINSCP_QUERY_REMMAXPKT 1
#define WINSCP_QUERY_MAIN_CHANNEL 2
#define WINSCP_QUERY_TIMER 3
unsigned int winscp_query(Backend * be, int query);
void md5checksum(const char * buffer, int len, unsigned char output[16]);
typedef const struct ssh_keyalg * cp_ssh_keyalg;
void get_hostkey_algs(int * count, cp_ssh_keyalg * SignKeys);
void get_macs(int * count, const struct ssh2_macalg *** amacs);

// from wingss.c

#ifndef SSH2_GSS_OIDTYPE
#include "sshgss.h"
#endif

// from portfwd.c

int is_pfwd(Plug plug);
Frontend * get_pfwd_frontend(Plug plug);

// for winstore.c

#include "winstuff.h"

long reg_open_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);
long reg_create_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);
long reg_query_winscp_value_ex(HKEY Key, const char * ValueName,
  unsigned long * Reserved, unsigned long * Type, unsigned char * Data,
  unsigned long * DataSize);
long reg_set_winscp_value_ex(HKEY Key, const char * ValueName, unsigned long Reserved,
  unsigned long Type, const unsigned char * Data, unsigned long DataSize);
long reg_close_winscp_key(HKEY Key);

// from winstore.c

void putty_mungestr(const char *in, char *out);
void putty_unmungestr(const char *in, char *out, int outlen);

// from winnet.c

void select_result(WPARAM wParam, LPARAM lParam);

// from sshaes.c

void call_aes_setup(void * ctx, unsigned char * key, int keylen);
void call_aes_encrypt(void * ctx, unsigned int * block);
void call_aes_decrypt(void * ctx, unsigned int * block);
void call_aes_sdctr(unsigned char *blk, int len, void *ctx);

// from winmisc.c

void win_misc_cleanup();

// from misc.c

const char * get_putty_version();

// from winsecur.c

void win_secur_cleanup(void);

// from sshecc.c

void ec_cleanup(void);

#endif
