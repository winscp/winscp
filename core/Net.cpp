//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Net.h"

#include "PuttyIntf.h"
#include "Interface.h"
#include "SecureShell.h"
#include "TextsCore.h"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
int SessionsCount = 0;
//---------------------------------------------------------------------------
void __fastcall InitWinsock();
static int get_line(void * frontend, const char * prompt, char * str,
  int maxlen, int is_pw);
//---------------------------------------------------------------------------
void __fastcall NetInitialize()
{
  ssh_get_line = get_line;
  ssh_getline_pw_only = TRUE;

  InitWinsock();
  sk_init();
  AnsiString VersionString = SshVersionString();
  assert(!VersionString.IsEmpty() && VersionString.Length() < 40);
  strcpy(sshver, VersionString.c_str());
}
//---------------------------------------------------------------------------
void __fastcall NetFinalize()
{
  WSACleanup();
}
//---------------------------------------------------------------------------
void __fastcall InitWinsock(void)
{
  // see scp.c init_winsock()
  WORD winsock_ver;
  WSADATA wsadata;

  #pragma option push -w-prc
  winsock_ver = MAKEWORD(1, 1);
  #pragma option pop

  if (WSAStartup(winsock_ver, &wsadata))
  {
    SSH_FATAL_ERROR("Unable to initialise WinSock");
  }

  if (LOBYTE(wsadata.wVersion) != 1 ||
      HIBYTE(wsadata.wVersion) != 1)
  {
    SSH_FATAL_ERROR("WinSock version is incompatible with 1.1");
  }
}
//---------------------------------------------------------------------------
extern "C" char * do_select(Plug plug, SOCKET skt, int startup)
{
  assert(plug != NULL);
  void * frontend;
  if (!is_ssh(plug))
  {
    // If it is not SSH plug, them it must be Proxy plug.
    // Get SSH plug which it wraps.
    Proxy_Socket ProxySocket = ((Proxy_Plug)plug)->proxy_socket;
    plug = ProxySocket->plug;
  }

  frontend = get_ssh_frontend(plug);
  assert(frontend);

  if (startup == 0)
  {
    skt = INVALID_SOCKET;
  }
  reinterpret_cast<TSecureShell*>(frontend)->SetSocket(&skt);

  return NULL;
}
//---------------------------------------------------------------------------
int from_backend(void * frontend, int is_stderr, char * data, int datalen)
{
  assert(frontend);
  ((TSecureShell *)frontend)->FromBackend((is_stderr == 1), data, datalen);
  return 0;
}
//---------------------------------------------------------------------------
static int get_line(void * frontend, const char * prompt, char * str,
  int maxlen, int is_pw)
{
  assert(frontend != NULL);

  TSecureShell * SecureShell = reinterpret_cast<TSecureShell*>(frontend);
  AnsiString Response;
  bool Result = SecureShell->PromptUser(prompt, Response, is_pw);
  if (Result)
  {
    strcpy(str, Response.SubString(1, maxlen).c_str());
  }

  return Result ? 1 : 0;
}
//---------------------------------------------------------------------------
void SSHLogEvent(void * frontend, char * string)
{
  // Frontend maybe NULL here
  if (frontend != NULL)
  {
    ((TSecureShell *)frontend)->PuttyLogEvent(string);
  }
}
//---------------------------------------------------------------------------
void SSHFatalError(char * string)
{
  // Only few calls from putty\winnet.c might be connected with specific
  // TSecureShell. Otherwise called only for really fatal errors
  // like 'out of memory' from putty\ssh.c.
  SSH_FATAL_ERROR_EXT(NULL, string);
}
//---------------------------------------------------------------------------
void SSHConnectionFatal(void * frontend, char * string)
{
  assert(frontend);
  ((TSecureShell *)frontend)->FatalError(string);
}
//---------------------------------------------------------------------------
void SSHVerifyHostKey(void * frontend, char * Host, int Port, char * KeyType,
  char * KeyStr, char * Fingerprint)
{
  assert(frontend);
  ((TSecureShell *)frontend)->VerifyHostKey(Host, Port, KeyType, KeyStr, Fingerprint);
}
//---------------------------------------------------------------------------
void SSHAskCipher(void * frontend, char * CipherName, int CipherType)
{
  assert(frontend);
  ((TSecureShell *)frontend)->AskCipher(CipherName, CipherType);
}
//---------------------------------------------------------------------------
void SSHOldKeyfileWarning(void)
{
  // no reference to TSecureShell instace available
}
