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
#include <storage.h>
//---------------------------------------------------------------------------
void SSHLogEvent(char *string);
void SSHFatalError(char *string);
//void SSHGotHostKey(void);
void SSHVerifyHostKey(char *host, int port, char *keytype, char *keystr, char *fingerprint);
void SSHOldKeyfileWarning(void);
void SSHAskCipher(char * CipherName, int CipherType);
long RegOpenWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
long RegCreateWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
//---------------------------------------------------------------------------
void fatalbox(char *fmt, ...)
{
  va_list ap;
  char stuff[200];

  va_start(ap, fmt);
  vsprintf(stuff, fmt, ap);
  va_end(ap);
  SSHFatalError(stuff);
}
//---------------------------------------------------------------------------
void begin_session(void)
{
  /* Maybe this is good point for showing main window */
}
//---------------------------------------------------------------------------
void logevent(char *string)
{
  SSHLogEvent(string);
}
//---------------------------------------------------------------------------
void connection_fatal(char *fmt, ...)
{
  char str[0x200]; /* Make the size big enough */
  va_list ap;
  va_start(ap, fmt);
  // 2003-01-04 "Fatal" prefix removed
  //strcpy(str, "Fatal: ");
  vsprintf(str/*+strlen(str)*/, fmt, ap);
  va_end(ap);
  strcat(str, "\n");

  fatalbox(str);
}
//---------------------------------------------------------------------------
void verify_ssh_host_key(char *host, int port, char *keytype,
  char *keystr, char *fingerprint)
{
  SSHVerifyHostKey(host, port, keytype, keystr, fingerprint);
}
//---------------------------------------------------------------------------
int askappend(char * filename)
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
void askcipher(char *ciphername, int cs)
{
  SSHAskCipher(ciphername, cs);
}
//---------------------------------------------------------------------------
void cleanup_exit(int code)
{
  SSHFatalError("");
}
//---------------------------------------------------------------------------
void ldisc_send(char * buf, int len, int interactive)
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
#pragma option pop // -w-par
