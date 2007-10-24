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
int has_gssapi_ssh();

// from portfwd.c

int is_pfwd(void * handle);
int is_pfwd_listener(void * handle);
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
void putty_get_seedpath(void);
char * seedpath_ptr();
int seedpath_size();

// from winnet.c

int select_result(WPARAM wParam, LPARAM lParam);

// from utf8.c

#include "charset/charset.h"

typedef struct charset_spec charset_spec;
void read_utf8(charset_spec const *, long int, charset_state *,
           void (*)(void *, long int), void *);
void write_utf8(charset_spec const *, long int,
        charset_state *, void (*)(void *, long int), void *);

// from sshzlib.c

extern const struct ssh_compress ssh_zlib;

#endif
