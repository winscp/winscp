//---------------------------------------------------------------------------
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

// Suppress Parameter 'xxx' is never used warning.
// It cannot be suppressed by not using param name, beacuse it is not possible in C
#pragma option push -w-par

#pragma option push -w-pch
#include <windows.h>
#pragma option pop /* -w-pch */

#define C_ONLY
#include "Net.h"
#include "Common.h"
#undef C_ONLY
#include <misc.h>
#include <storage.h>
//---------------------------------------------------------------------------
char sshver[50];
//---------------------------------------------------------------------------
void SSHLogEvent(void * frontend, char * string);
void SSHFatalError(char * string);
void SSHConnectionFatal(void * frontend, char * string);
void SSHVerifyHostKey(void * frontend, char *host, int port, char * keytype,
  char * keystr, char * fingerprint);
void SSHOldKeyfileWarning(void);
void SSHAskCipher(void * frontend, char * CipherName, int CipherType);
long RegOpenWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
long RegCreateWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
//---------------------------------------------------------------------------
#define FATAL_MSG(FMT) \
  va_list ap; \
  char stuff[200]; \
  va_start(ap, FMT); \
  vsnprintf(stuff, sizeof(stuff), FMT, ap); \
  stuff[sizeof(stuff) - 1] = '\0'; \
  va_end(ap);

#define FATAL_BOX(FMT) \
  FATAL_MSG(FMT); \
  SSHFatalError(stuff);
//---------------------------------------------------------------------------
void fatalbox(char *fmt, ...)
{
  FATAL_BOX(fmt);
}
//---------------------------------------------------------------------------
void modalfatalbox(char *fmt, ...)
{
  FATAL_BOX(fmt);
}
//---------------------------------------------------------------------------
/*void begin_session(void)
{
  // Maybe this is good point for showing main window 
} */
//---------------------------------------------------------------------------
void logevent(void * frontend, char * string)
{
  SSHLogEvent(frontend, string);
}
//---------------------------------------------------------------------------
void connection_fatal(void * frontend, char * fmt, ...)
{
  FATAL_MSG(fmt);
  SSHConnectionFatal(frontend, stuff);
}
//---------------------------------------------------------------------------
void verify_ssh_host_key(void * frontend, char * host, int port, char * keytype,
  char * keystr, char * fingerprint)
{
  SSHVerifyHostKey(frontend, host, port, keytype, keystr, fingerprint);
}
//---------------------------------------------------------------------------
int askappend(void * frontend, char * filename)
{
  // this is called from loggin.c of putty, which is never used with WinSCP
  assert(0);
  return 0;
}
//---------------------------------------------------------------------------
void old_keyfile_warning(void)
{
  SSHOldKeyfileWarning();
}
//---------------------------------------------------------------------------
void askcipher(void * frontend, char * ciphername, int cs)
{
  SSHAskCipher(frontend, ciphername, cs);
}
//---------------------------------------------------------------------------
void cleanup_exit(int code)
{
  SSHFatalError("");
}
//---------------------------------------------------------------------------
void ldisc_send(void * handle, char * buf, int len, int interactive)
{
    /*
     * This is only here because of the calls to ldisc_send(NULL,
     * 0) in ssh.c. Nothing in PSCP actually needs to use the ldisc
     * as an ldisc. So if we get called with any real data, I want
     * to know about it.
     */
  assert(len == 0);
}
//---------------------------------------------------------------------------
long reg_open_winscp_key(HKEY Key, const char * SubKey, HKEY * Result)
{
  return RegOpenWinSCPKey(Key, SubKey, Result);
}
//---------------------------------------------------------------------------
long reg_create_winscp_key(HKEY Key, const char * SubKey, HKEY * Result)
{
  return RegCreateWinSCPKey(Key, SubKey, Result);
}
//---------------------------------------------------------------------------
void agent_schedule_callback(void (* callback)(void *, void *, int),
  void * callback_ctx, void * data, int len)
{
  assert(0);
}
//---------------------------------------------------------------------------
void update_specials_menu(void * frontend)
{
  // nothing
}
//---------------------------------------------------------------------------
#pragma option pop // -w-par
