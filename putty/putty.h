#ifndef PUTTY_PUTTY_H

// workaround for some error in Borland's preprocessor when Local Options are set
/*#ifndef GLOBAL
#ifdef PUTTY_DO_GLOBALS
#define GLOBAL
#else
#define GLOBAL extern
#endif
#endif

#include <windows.h>
#include "winstuff.h"
#define PUTTY_PUTTYPS_H*/

#include "putty.org.h"

#ifndef PUTTY_ORIGINAL

#if defined(PUTTY_LIB)

// safemalloc() and saferealloc() in misc.c call MessageBox() directly
// we want to raise exception instead.
//#pragma option push -w-dup
//#define MessageBox(A, Message, B, C) fatalbox(Message)
//#pragma option pop

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
  void ssh_close(void * handle) { ssh_do_close((Ssh)handle); } \
  int get_ssh_version(void * handle) { return ((Ssh)handle)->version; } \
  int get_ssh1_compressing(void * handle) { return ((Ssh)handle)->v1_compressing; } \
  const struct ssh_cipher * get_cipher(void * handle) { return ((Ssh)handle)->cipher; } \
  const struct ssh2_cipher * get_cscipher(void * handle) { return ((Ssh)handle)->cscipher; } \
  const struct ssh2_cipher * get_sccipher(void * handle) { return ((Ssh)handle)->sccipher; } \
  const struct ssh_compress * get_cscomp(void * handle) { return ((Ssh)handle)->cscomp; } \
  const struct ssh_compress * get_sccomp(void * handle) { return ((Ssh)handle)->sccomp; } \
  int get_ssh_state(void * handle) { return ((Ssh)handle)->state; } \
  int get_ssh_state_closed(void * handle) { return ((Ssh)handle)->state == SSH_STATE_CLOSED; } \
  int get_ssh_exitcode(void * handle) { return ssh_return_exitcode(handle); } \
  Backend ssh_backend
#endif

#if defined(PUTTY_LIB) && defined(WINSTORE_FILE)
// this needs to be only in one module, we choose winstore.c
//const char *PUTTY_REG_POS_ORIG = PUTTY_REG_POS;
// This is 'forced' into winstore.c, because otherwise we can't call
// mungestr() and unmungestr(), 'cause they are static
static void mungestr(const char *in, char *out);
void putty_mungestr(const char *in, char *out)
{
  mungestr(in, out);
}
static void unmungestr(const char *in, char *out, int outlen);
void putty_unmungestr(const char *in, char *out, int outlen)
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
#define enum_settings_next \
  seedpath_ptr() { return seedpath; } \
  int seedpath_size() { return sizeof(seedpath); } \
  char * enum_settings_next
#endif

#endif

#endif
