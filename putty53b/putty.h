#ifndef PUTTY_PUTTY_H

//#ifndef AUTO_WINSOCK
//#include <winsock2.h>
//#endif
//#include <stdio.h>

#include <putty.org.h>

#ifndef PUTTY_ORIGINAL

#if defined(PUTTY_LIB)

// safemalloc() and saferealloc() in misc.c call MessageBox() directly
// we want to raise exception instead.
#pragma option push -w-dup
#define MessageBox(A, Message, B, C) fatalbox(Message)
#pragma option pop

long reg_open_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);
long reg_create_winscp_key(HKEY Key, const char * SubKey, HKEY * Result);

#pragma option push -w-dup
#define RegOpenKey reg_open_winscp_key
#define RegCreateKey reg_create_winscp_key
#pragma option pop

#endif

#if defined(PUTTY_LIB) && defined(SSH_FILE)
// To force our code to ssh.c we have to find symbol, that is only
// once in whole file, we choosed 'ssh_backend'
#define ssh_backend ssh_backend_not_used; \
  void ssh_close() { if (s) { sk_close(s); s = NULL; } }; \
  int get_ssh_version() { return ssh_version; } \
  int get_ssh1_compressing() { return ssh1_compressing; } \
  struct ssh_cipher *get_cipher() { return cipher; } \
  struct ssh2_cipher *get_cscipher() { return cscipher; } \
  struct ssh2_cipher *get_sccipher() { return sccipher; } \
  struct ssh_compress *get_cscomp() { return cscomp; } \
  struct ssh_compress *get_sccomp() { return sccomp; } \
  int get_ssh_state() { return ssh_state; } \
  int get_ssh_state_closed() { return ssh_state == SSH_STATE_CLOSED; } \
  int get_ssh_exitcode() { return ssh_exitcode; } \
  Backend ssh_backend
#endif

#if defined(PUTTY_LIB) && defined(WINSTORE_FILE)
// this needs to be only in one module, we choose winstore.c
//const char *PUTTY_REG_POS_ORIG = PUTTY_REG_POS;
// This is 'forced' into winstore.c, because otherwise we can't call
// mungestr() and unmungestr(), 'cause they are static
static void mungestr(char *in, char *out);
void putty_mungestr(char *in, char *out)
{
  mungestr(in, out);
}
static void unmungestr(char *in, char *out, int outlen);
void putty_unmungestr(char *in, char *out, int outlen)
{
  unmungestr(in, out, outlen);
}
static void get_seedpath(void);
void putty_get_seedpath(void)
{
  get_seedpath();
}
// Needs to be included sooner than "read_setting_s" macro is defined
#include "storage.h"
// To access seedpath (static)
#define read_setting_s \
  seedpath_ptr() { return seedpath; } \
  int seedpath_size() { return sizeof(seedpath); } \
  char *read_setting_s
#endif

#endif

#endif
