//---------------------------------------------------------------------------
#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>

#include <putty.h>

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
void SSHLogEvent(void * frontend, const char * string);
void SSHFatalError(char * string);
void SSHConnectionFatal(void * frontend, char * string);
void SSHVerifyHostKey(void * frontend, char *host, int port, char * keytype,
  char * keystr, char * fingerprint);
void SSHOldKeyfileWarning(void);
void SSHAskAlg(void * frontend, const char * AlgType, const char * AlgName);
long RegOpenWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
long RegCreateWinSCPKey(HKEY hKey, const char * lpSubKey, HKEY * phkResult);
long RegQueryWinSCPValueEx(HKEY Key, const char * ValueName, unsigned long * Reserved,
  unsigned long * Type, unsigned char * Data, unsigned long * DataSize);
long RegSetWinSCPValueEx(HKEY Key, const char * ValueName, unsigned long Reserved,
  unsigned long Type, const unsigned char * Data, unsigned long DataSize);
long RegCloseWinSCPKey(HKEY Key);
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
void logevent(void * frontend, const char * string)
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
int verify_ssh_host_key(void * frontend, char * host, int port, char * keytype,
  char * keystr, char * fingerprint, void (*callback)(void * ctx, int result),
  void * ctx)
{
  SSHVerifyHostKey(frontend, host, port, keytype, keystr, fingerprint);
  // We should return 0 when key was not confirmed, we throw exception instead.
  return 1;
}
//---------------------------------------------------------------------------
int askappend(void * frontend, Filename filename,
  void (*callback)(void * ctx, int result), void * ctx)
{
  // this is called from logging.c of putty, which is never used with WinSCP
  assert(0);
  return 0;
}
//---------------------------------------------------------------------------
void old_keyfile_warning(void)
{
  SSHOldKeyfileWarning();
}
//---------------------------------------------------------------------------
int askalg(void * frontend, const char * algtype, const char * algname,
  void (*callback)(void * ctx, int result), void * ctx)
{
  SSHAskAlg(frontend, algtype, algname);
  // We should return 0 when key was not confirmed, we throw exception instead.
  return 1;
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
long reg_query_winscp_value_ex(HKEY Key, const char * ValueName, unsigned long * Reserved,
  unsigned long * Type, unsigned char * Data, unsigned long * DataSize)
{
  return RegQueryWinSCPValueEx(Key, ValueName, Reserved, Type, Data, DataSize);
} 
//---------------------------------------------------------------------------
long reg_set_winscp_value_ex(HKEY Key, const char * ValueName, unsigned long Reserved,
  unsigned long Type, const unsigned char * Data, unsigned long DataSize)
{
  return RegSetWinSCPValueEx(Key, ValueName, Reserved, Type, Data, DataSize);
}
//---------------------------------------------------------------------------
long reg_close_winscp_key(HKEY Key)
{
  return RegCloseWinSCPKey(Key);
}
//---------------------------------------------------------------------------
void agent_schedule_callback(void (* callback)(void *, void *, int),
  void * callback_ctx, void * data, int len)
{
  assert(0);
}
//---------------------------------------------------------------------------
void notify_remote_exit(void *frontend)
{
  // nothing
}
//---------------------------------------------------------------------------
void update_specials_menu(void * frontend)
{
  // nothing
}
//---------------------------------------------------------------------------
typedef void (*timer_fn_t)(void *ctx, long now);
long schedule_timer(int ticks, timer_fn_t fn, void * ctx)
{
  return ticks + GetTickCount();
}
//---------------------------------------------------------------------------
void expire_timer_context(void * ctx)
{
  // nothing
}
//---------------------------------------------------------------------------
Pinger pinger_new(Config * cfg, Backend * back, void * backhandle)
{
  return NULL;
}
//---------------------------------------------------------------------------
void pinger_reconfig(Pinger pinger, Config * oldcfg, Config * newcfg)
{
  // nothing
}
//---------------------------------------------------------------------------
void pinger_free(Pinger pinger)
{
  // nothing
}
//---------------------------------------------------------------------------
void set_busy_status(void * frontend, int status)
{
  // nothing
}
//---------------------------------------------------------------------------
#pragma option pop // -w-par
