#ifndef PUTTY_PUTTYEXP_H
#define PUTTY_PUTTYEXP_H

// from ssh.c

void ssh_close(void * handle);
int is_ssh(void * handle);
void call_ssh_timer(void * handle);
int get_ssh_version(void * handle);
void * get_ssh_frontend(void * handle);
int get_ssh1_compressing(void * handle);
const struct ssh_cipher * get_cipher(void * handle);
const struct ssh2_cipher * get_cscipher(void * handle);
const struct ssh2_cipher * get_sccipher(void * handle);
const struct ssh_compress * get_cscomp(void * handle);
const struct ssh_compress * get_sccomp(void * handle);
int get_ssh_state(void * handle);
int get_ssh_state_closed(void * handle);
int get_ssh_state_session(void * handle);
int get_ssh_exitcode(void * handle);
const unsigned int * ssh2_remmaxpkt(void * handle);
const unsigned int * ssh2_remwindow(void * handle);
void md5checksum(const char * buffer, int len, unsigned char output[16]);
typedef const struct ssh_signkey * cp_ssh_signkey;
void get_hostkey_algs(int * count, cp_ssh_signkey * SignKeys);
void get_macs(int * count, const struct ssh_mac *** amacs);

// from wingss.c

#ifndef SSH2_GSS_OIDTYPE
#include "sshgss.h"
#endif

// from portfwd.c

int is_pfwd(void * handle);
void * get_pfwd_backend(void * handle);

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

// from sshzlib.c

extern const struct ssh_compress ssh_zlib;

// from sshaes.c

void * call_aes_make_context();
void call_aes_free_context(void * handle);
void call_aes_setup(void * ctx, int blocklen, unsigned char * key, int keylen);
void call_aes_encrypt(void * ctx, unsigned int * block);
void call_aes_decrypt(void * ctx, unsigned int * block);
void call_aes_sdctr(unsigned char *blk, int len, void *ctx);
void aes_iv(void *handle, unsigned char *iv);

// from sshsha.c

void call_sha1_key_internal(void * handle, unsigned char * key, int len);

// from winmisc.c

void win_misc_cleanup();

// from misc.c

const char * get_putty_version();

// from winsecur.c

void win_secur_cleanup(void);

// from sshecc.c

void ec_cleanup(void);

#endif
