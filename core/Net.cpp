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
void init_winsock(void);
//---------------------------------------------------------------------------
Integer SessionsCount = 0;
//---------------------------------------------------------------------------
void NetInitialize()
{
  default_protocol = PROT_SSH;
  ssh_get_line = get_line;
  init_winsock();
  sk_init();
}
//---------------------------------------------------------------------------
void NetFinalize()
{
  WSACleanup();
}
//---------------------------------------------------------------------------
void init_winsock(void)
{
  // see scp.c init_winsock()
  WORD winsock_ver;
  WSADATA wsadata;

  #pragma option push -w-prc
  winsock_ver = MAKEWORD(1, 1);
  #pragma option pop
  
  if (WSAStartup(winsock_ver, &wsadata))
      SSH_FATAL_ERROR("Unable to initialise WinSock");
  if (LOBYTE(wsadata.wVersion) != 1 ||
      HIBYTE(wsadata.wVersion) != 1)
      SSH_FATAL_ERROR("WinSock version is incompatible with 1.1");
}
//---------------------------------------------------------------------------
TSecureShell *CurrentSSH = NULL;
#define REQUIRE_SSH() assert(CurrentSSH)
char *do_select(SOCKET skt, int startup)
{
  REQUIRE_SSH();

  if (startup) CurrentSSH->Socket = skt;
    else CurrentSSH->Socket = INVALID_SOCKET;
  return NULL;
}
//---------------------------------------------------------------------------
int from_backend(int is_stderr, char *data, int datalen) {
    REQUIRE_SSH();
    CurrentSSH->FromBackend((Boolean)(is_stderr == 1), data, datalen);
    return 0;
}
//---------------------------------------------------------------------------
static int get_line(const char *prompt, char *str, int maxlen, int is_pw)
{
  if (!is_pw) throw Exception("");

  AnsiString Password, Prompt(prompt);
  Integer Result;
  if (Prompt.Pos("Passphrase") == 1)
  {
    AnsiString Key(Prompt);
    Integer P;
    if ((P = Prompt.Pos("\"")) > 0) Key.Delete(1, P);
    if ((P = Prompt.Pos("\"")) > 0) Key.Delete(P, Prompt.Length() - P);

    Result = GetSessionPassword(
      FmtLoadStr(PROMPT_KEY_PASSPHRASE, ARRAYOFCONST((Key))),
      Password);
  }
    else
  if (Prompt.Pos("'s password"))
  {
    REQUIRE_SSH();
    Result = CurrentSSH->GetPassword(Password);
  }
    else
  {
    // in other cases we assume TIS/Cryptocard authentification prompt
    Result = GetSessionPassword(AnsiString(prompt), Password);
  };

  if (Result)
    strcpy(str, Password.SubString(1, maxlen).c_str());
  return Result;
}
//---------------------------------------------------------------------------
void SSHLogEvent(char *string)
{
  REQUIRE_SSH();
  CurrentSSH->LogEvent(string);
}
//---------------------------------------------------------------------------
void SSHFatalError(char *string)
{
  REQUIRE_SSH();
  CurrentSSH->FatalError(string);
}
//---------------------------------------------------------------------------
/*void SSHGotHostKey(void)
{
  REQUIRE_SSH();
  CurrentSSH->GotHostKey();
} */
//---------------------------------------------------------------------------
void SSHVerifyHostKey(char * Host, int Port, char * KeyType,
  char * KeyStr, char * Fingerprint)
{
  REQUIRE_SSH();
  CurrentSSH->VerifyHostKey(Host, Port, KeyType, KeyStr, Fingerprint);
}
//---------------------------------------------------------------------------
void SSHAskCipher(char * CipherName, int CipherType)
{
  REQUIRE_SSH();
  CurrentSSH->AskCipher(CipherName, CipherType);
}
//---------------------------------------------------------------------------
void SSHOldKeyfileWarning(void)
{
  REQUIRE_SSH();
  CurrentSSH->OldKeyfileWarning();
}

