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
TSecureShell * CurrentSSH = NULL;
//---------------------------------------------------------------------------
void __fastcall InitWinsock();
static int get_line(const char * prompt, char * str, int maxlen, int is_pw);
//---------------------------------------------------------------------------
void __fastcall NetInitialize()
{
  ssh_get_line = get_line;

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
char * do_select(SOCKET skt, int startup)
{
  assert(CurrentSSH);

  if (CurrentSSH)
  {
    CurrentSSH->Socket = startup ? skt : INVALID_SOCKET;
  }
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
static int get_line(const char * prompt, char * str, int maxlen, int is_pw)
{
  assert(is_pw);

  AnsiString Password, Prompt(prompt);
  int Result;
  if (Prompt.Pos("Passphrase") == 1)
  {
    AnsiString Key(Prompt);
    int P;
    if ((P = Prompt.Pos("\"")) > 0) Key.Delete(1, P);
    if ((P = Prompt.Pos("\"")) > 0) Key.Delete(P, Prompt.Length() - P);

    Result = GetSessionPassword(
      FmtLoadStr(PROMPT_KEY_PASSPHRASE, ARRAYOFCONST((Key))),
      Password);
  }
  else if (Prompt.Pos("'s password"))
  {
    assert(CurrentSSH);
    Result = CurrentSSH->GetPassword(Password);
  }
  else
  {
    // in other cases we assume TIS/Cryptocard authentification prompt
    Result = GetSessionPassword(AnsiString(prompt), Password);
  };

  if (Result)
  {
    strcpy(str, Password.SubString(1, maxlen).c_str());
  }
  
  return Result;
}
//---------------------------------------------------------------------------
void SSHLogEvent(void * frontend, char * string)
{
  assert(frontend);
  ((TSecureShell *)frontend)->LogEvent(string);
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
  assert(CurrentSSH);
  CurrentSSH->OldKeyfileWarning();
}
