//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "PuttyIntf.h"
#include "Exceptions.h"
#include "Interface.h"
#include "SecureShell.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "Common.h"
#include "CoreMain.h"

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
//---------------------------------------------------------------------------
#define SSH_ERROR(x) throw ESsh(NULL, x)
#define SSH_FATAL_ERROR_EXT(E, x) throw ESshFatal(E, x)
#define SSH_FATAL_ERROR(x) SSH_FATAL_ERROR_EXT(NULL, x)
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define MAX_BUFSIZE 32768
//---------------------------------------------------------------------------
struct TPuttyTranslation
{
  const char * Original;
  int Translation;
};
//---------------------------------------------------------------------------
__fastcall TSecureShell::TSecureShell(TSessionUI* UI,
  TSessionData * SessionData, TSessionLog * Log, TConfiguration * Configuration)
{
  FUI = UI;
  FSessionData = SessionData;
  FLog = Log;
  FConfiguration = Configuration;
  FActive = false;
  OutPtr = NULL;
  Pending = NULL;
  FBackendHandle = NULL;
  ResetConnection();
  FOnCaptureOutput = NULL;
  FOnReceive = NULL;
  FConfig = new Config();
  FSocket = INVALID_SOCKET;
  FSocketEvent = CreateEvent(NULL, false, false, NULL);
  FFrozen = false;
}
//---------------------------------------------------------------------------
__fastcall TSecureShell::~TSecureShell()
{
  Active = false;
  ResetConnection();
  CloseHandle(FSocketEvent);
  delete FConfig;
  FConfig = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::ResetConnection()
{
  FreeBackend();
  ClearStdError();
  PendLen = 0;
  PendSize = 0;
  sfree(Pending);
  Pending = NULL;
  FCWriteTemp = "";
  ResetSessionInfo();
  FAuthenticating = false;
  FAuthenticated = false;
  FStoredPasswordTried = false;
  FStoredPasswordTriedForKI = false;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::ResetSessionInfo()
{
  FSessionInfoValid = false;
  FMaxPacketSize = NULL;
}
//---------------------------------------------------------------------------
inline void __fastcall TSecureShell::UpdateSessionInfo()
{
  if (!FSessionInfoValid)
  {
    FSshVersion = get_ssh_version(FBackendHandle);
    FSessionInfo.ProtocolBaseName = "SSH";
    FSessionInfo.ProtocolName =
      FORMAT("%s-%d", (FSessionInfo.ProtocolBaseName, get_ssh_version(FBackendHandle)));
    FSessionInfo.SecurityProtocolName = FSessionInfo.ProtocolName;

    FSessionInfo.CSCompression =
      FuncToCompression(FSshVersion, get_cscomp(FBackendHandle));
    FSessionInfo.SCCompression =
      FuncToCompression(FSshVersion, get_sccomp(FBackendHandle));

    if (FSshVersion == 1)
    {
      FSessionInfo.CSCipher = CipherNames[FuncToSsh1Cipher(get_cipher(FBackendHandle))];
      FSessionInfo.SCCipher = CipherNames[FuncToSsh1Cipher(get_cipher(FBackendHandle))];
    }
    else
    {
      FSessionInfo.CSCipher = CipherNames[FuncToSsh2Cipher(get_cscipher(FBackendHandle))];
      FSessionInfo.SCCipher = CipherNames[FuncToSsh2Cipher(get_sccipher(FBackendHandle))];
    }

    FSessionInfoValid = true;
  }
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TSecureShell::GetSessionInfo()
{
  if (!FSessionInfoValid)
  {
    UpdateSessionInfo();
  }
  return FSessionInfo;
}
//---------------------------------------------------------------------
void __fastcall TSecureShell::StoreToConfig(TSessionData * Data, Config * cfg)
{
  // clear all (parameters not set below)
  memset(cfg, 0, sizeof(*cfg));

  // user-configurable settings
  ASCOPY(cfg->host, Data->HostName);
  ASCOPY(cfg->username, Data->UserName);
  cfg->port = Data->PortNumber;
  cfg->protocol = PROT_SSH;
  // always set 0, as we will handle keepalives ourselves to avoid
  // multi-threaded issues in putty timer list
  cfg->ping_interval = 0;
  cfg->compression = Data->Compression;
  cfg->agentfwd = Data->AgentFwd;
  cfg->addressfamily = Data->AddressFamily;
  ASCOPY(cfg->ssh_rekey_data, Data->RekeyData);
  cfg->ssh_rekey_time = Data->RekeyTime;

  for (int c = 0; c < CIPHER_COUNT; c++)
  {
    int pcipher;
    switch (Data->Cipher[c]) {
      case cipWarn: pcipher = CIPHER_WARN; break;
      case cip3DES: pcipher = CIPHER_3DES; break;
      case cipBlowfish: pcipher = CIPHER_BLOWFISH; break;
      case cipAES: pcipher = CIPHER_AES; break;
      case cipDES: pcipher = CIPHER_DES; break;
      default: assert(false);
    }
    cfg->ssh_cipherlist[c] = pcipher;
  }

  for (int k = 0; k < KEX_COUNT; k++)
  {
    int pkex;
    switch (Data->Kex[k]) {
      case kexWarn: pkex = KEX_WARN; break;
      case kexDHGroup1: pkex = KEX_DHGROUP1; break;
      case kexDHGroup14: pkex = KEX_DHGROUP14; break;
      case kexDHGEx: pkex = KEX_DHGEX; break;
      default: assert(false);
    }
    cfg->ssh_kexlist[k] = pkex;
  }

  AnsiString SPublicKeyFile = Data->PublicKeyFile;
  if (SPublicKeyFile.IsEmpty()) SPublicKeyFile = Configuration->DefaultKeyFile;
  SPublicKeyFile = StripPathQuotes(ExpandEnvironmentVariables(SPublicKeyFile));
  ASCOPY(cfg->keyfile.path, SPublicKeyFile);
  cfg->sshprot = Data->SshProt;
  cfg->ssh2_des_cbc = Data->Ssh2DES;
  cfg->try_tis_auth = Data->AuthTIS;
  cfg->try_ki_auth = Data->AuthKI;
  cfg->try_gssapi_auth = Data->AuthGSSAPI;
  cfg->gssapi_fwd_tgt = Data->GSSAPIFwdTGT;
  ASCOPY(cfg->gssapi_server_realm, Data->GSSAPIServerRealm);
  cfg->change_username = Data->ChangeUsername;

  cfg->proxy_type = Data->ProxyMethod;
  ASCOPY(cfg->proxy_host, Data->ProxyHost);
  cfg->proxy_port = Data->ProxyPort;
  ASCOPY(cfg->proxy_username, Data->ProxyUsername);
  ASCOPY(cfg->proxy_password, Data->ProxyPassword);
  ASCOPY(cfg->proxy_telnet_command, Data->ProxyTelnetCommand);
  cfg->proxy_dns = Data->ProxyDNS;
  cfg->even_proxy_localhost = Data->ProxyLocalhost;

  #pragma option push -w-eas
  // after 0.53b values were reversed, however putty still stores
  // settings to registry in save way as before
  cfg->sshbug_ignore1 = Data->Bug[sbIgnore1];
  cfg->sshbug_plainpw1 = Data->Bug[sbPlainPW1];
  cfg->sshbug_rsa1 = Data->Bug[sbRSA1];
  cfg->sshbug_hmac2 = Data->Bug[sbHMAC2];
  cfg->sshbug_derivekey2 = Data->Bug[sbDeriveKey2];
  cfg->sshbug_rsapad2 = Data->Bug[sbRSAPad2];
  cfg->sshbug_rekey2 = Data->Bug[sbRekey2];
  // new after 0.53b
  cfg->sshbug_pksessid2 = Data->Bug[sbPKSessID2];
  #pragma option pop

  if (!Data->TunnelPortFwd.IsEmpty())
  {
    ASCOPY(cfg->portfwd, Data->TunnelPortFwd);
    // when setting up a tunnel, do not open shell/sftp
    cfg->ssh_no_shell = TRUE;
  }
  else
  {
    if (Data->FSProtocol == fsSCPonly)
    {
      cfg->ssh_subsys = FALSE;
      if (Data->Shell.IsEmpty())
      {
        // Following forces Putty to open default shell
        // see ssh.c: do_ssh2_authconn() and ssh1_protocol()
        cfg->remote_cmd[0] = '\0';
      }
      else
      {
        ASCOPY(cfg->remote_cmd, Data->Shell);
      }
    }
    else
    {
      cfg->ssh_subsys = TRUE;
      strcpy(cfg->remote_cmd, "sftp");

      if (Data->FSProtocol != fsSFTPonly)
      {
        cfg->ssh_subsys2 = FALSE;
        if (Data->Shell.IsEmpty())
        {
          // Following forces Putty to open default shell
          // see ssh.c: do_ssh2_authconn() and ssh1_protocol()
          cfg->remote_cmd2[0] = '\0';
        }
        else
        {
          ASCOPY(cfg->remote_cmd2, Data->Shell);
        }
        // Putty reads only "ptr" member for fallback
        cfg->remote_cmd_ptr2 = cfg->remote_cmd2;
      }
      else
      {
        // see psftp_connect() from psftp.c
        cfg->ssh_subsys2 = FALSE;
        cfg->remote_cmd_ptr2 =
          "test -x /usr/lib/sftp-server && exec /usr/lib/sftp-server\n"
          "test -x /usr/local/lib/sftp-server && exec /usr/local/lib/sftp-server\n"
          "exec sftp-server";
      }
    }
  }

  // permanent settings
  cfg->nopty = TRUE;
  cfg->tcp_keepalives = 0;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Open()
{
  ResetConnection();

  FAuthenticating = false;
  FAuthenticated = false;

  Active = false;
  FBackend = &ssh_backend;

  FAuthenticationLog = "";
  FUI->Information(LoadStr(STATUS_LOOKUPHOST), true);
  StoreToConfig(FSessionData, FConfig);

  char * RealHost;
  FreeBackend(); // in case we are reconnecting
  const char * InitError = FBackend->init(this, &FBackendHandle, FConfig,
    FSessionData->HostName.c_str(), FSessionData->PortNumber, &RealHost, 0,
    FConfig->tcp_keepalives);
  sfree(RealHost);
  if (InitError)
  {
    PuttyFatalError(InitError);
  }
  FUI->Information(LoadStr(STATUS_CONNECT), true);
  /*FLoggingContext = log_init(this, (void *)FConfig);
  FBackend->provide_logctx(FBackendHandle, FLoggingContext);*/
  Init();

  CheckConnection(CONNECTION_FAILED);
  FLastDataSent = Now();

  FSessionInfo.LoginTime = Now();

  FAuthenticating = false;
  FAuthenticated = true;
  FUI->Information(LoadStr(STATUS_AUTHENTICATED), true);

  ResetSessionInfo();

  assert(!FSessionInfo.SshImplementation.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Init()
{
  try
  {
    try
    {
      while (!get_ssh_state_session(FBackendHandle))
      {
        if (Configuration->LogProtocol >= 1)
        {
          LogEvent("Waiting for the server to continue with the initialisation");
        }
        WaitForData();
      }

      // unless this is tunnel session, it must be safe to send now
      assert(FBackend->sendok(FBackendHandle) || !FSessionData->TunnelPortFwd.IsEmpty());
    }
    catch(Exception & E)
    {
      if (FAuthenticating && !FAuthenticationLog.IsEmpty())
      {
        FatalError(&E, FMTLOAD(AUTHENTICATION_LOG, (FAuthenticationLog)));
      }
      else
      {
        throw;
      }
    }
  }
  catch(Exception & E)
  {
    if (FAuthenticating)
    {
      FatalError(&E, LoadStr(AUTHENTICATION_FAILED));
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::PuttyLogEvent(const AnsiString & Str)
{
  #define SERVER_VERSION_MSG "Server version: "
  // Gross hack
  if (Str.Pos(SERVER_VERSION_MSG) == 1)
  {
    FSessionInfo.SshVersionString = Str.SubString(strlen(SERVER_VERSION_MSG) + 1,
      Str.Length() - strlen(SERVER_VERSION_MSG));

    const char * Ptr = strchr(FSessionInfo.SshVersionString.c_str(), '-');
    if (Ptr != NULL)
    {
      Ptr = strchr(Ptr + 1, '-');
    }
    FSessionInfo.SshImplementation = (Ptr != NULL) ? Ptr + 1 : "";
  }
  #define FORWARDING_FAILURE_MSG "Forwarded connection refused by server: "
  else if (Str.Pos(FORWARDING_FAILURE_MSG) == 1)
  {
    FLastTunnelError = Str.SubString(strlen(FORWARDING_FAILURE_MSG) + 1,
      Str.Length() - strlen(FORWARDING_FAILURE_MSG));

    static const TPuttyTranslation Translation[] = {
      { "Administratively prohibited [%]", PFWD_TRANSL_ADMIN },
      { "Connect failed [%]", PFWD_TRANSL_CONNECT },
    };
    TranslatePuttyMessage(Translation, LENOF(Translation), FLastTunnelError);
  }
  LogEvent(Str);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::PromptUser(const AnsiString Prompt,
  AnsiString & Response, bool IsPassword)
{
  bool Result;
  if (!IsPassword)
  {
    assert(Prompt == "login as: ");
    LogEvent(FORMAT("Username prompt (%s)", (Prompt)));

    if (FSessionData->AuthGSSAPI)
    {
      // use empty username if no username was filled on login dialog
      // and GSSAPI auth is enabled, hence there's chance that the server can
      //  deduce the username otherwise
      Response = "";
      Result = true;
    }
    else
    {
      Result = FUI->PromptUser(FSessionData,
        FMTLOAD(USERNAME_PROMPT, (FSessionData->SessionName)),
        pkPrompt, Response);
      if (Result)
      {
        FUserName = Response;
      }
    }
  }
  else
  {
    if (Prompt.Pos("Passphrase for key ") == 1)
    {
      AnsiString Key(Prompt);
      int P = Prompt.Pos("\"");
      if (P > 0)
      {
        Key.Delete(1, P);
        P = Key.LastDelimiter("\"");
        if (P > 0)
        {
          Key.SetLength(P - 1);
        }
      }

      LogEvent(FORMAT("Passphrase prompt (%s)", (Prompt)));

      Result = FUI->PromptUser(FSessionData, FMTLOAD(PROMPT_KEY_PASSPHRASE, (Key)),
        pkPassphrase, Response);
    }
    else if (Prompt.Pos("'s password: "))
    {
      LogEvent(FORMAT("Session password prompt (%s)", (Prompt)));

      if (!FSessionData->Password.IsEmpty() && !FStoredPasswordTried)
      {
        LogEvent("Using stored password.");
        FUI->Information(LoadStr(AUTH_PASSWORD), false);
        Result = true;
        Response = FSessionData->Password;
        FStoredPasswordTried = true;
      }
      else
      {
        Result = FUI->PromptUser(FSessionData,
          FMTLOAD(PROMPT_SESSION_PASSWORD, (FSessionData->SessionName)),
          pkPassword, Response);
      }
    }
    else
    {
      // in other cases we assume TIS/Cryptocard/keyboard-interactive authentification prompt
      LogEvent(FORMAT("%s prompt from server", (Prompt)));

      if (FSessionData->AuthKIPassword && !FSessionData->Password.IsEmpty() &&
          !FStoredPasswordTriedForKI)
      {
        LogEvent("Using stored password.");
        FUI->Information(LoadStr(AUTH_PASSWORD), false);
        Result = true;
        Response = FSessionData->Password;
        FStoredPasswordTriedForKI = true;
      }
      else
      {
        static const AnsiString ResponseSuffix("\r\nResponse: ");

        // Strip Cryptocard/TIS "Response" suffix
        AnsiString UserPrompt = Prompt;
        if (UserPrompt.SubString(UserPrompt.Length() - ResponseSuffix.Length() + 1,
              ResponseSuffix.Length()) == ResponseSuffix)
        {
          UserPrompt.SetLength(UserPrompt.Length() - ResponseSuffix.Length());
        }

        // some servers add leading blank line to make the prompt look prettier
        // on terminal console
        UserPrompt = UserPrompt.Trim();

        Result = FUI->PromptUser(FSessionData, UserPrompt, pkServerPrompt, Response);
      }
    };
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::GotHostKey()
{
  // due to re-key GotHostKey() may be called again later during session
  if (!FAuthenticating && !FAuthenticated)
  {
    FAuthenticating = true;
    FUI->Information(LoadStr(STATUS_AUTHENTICATE), true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::CWrite(const char * Data, int Length, bool Untrusted)
{
  // some messages to stderr may indicate that something has changed with the
  // session, so reset the session info
  ResetSessionInfo();

  // We send only whole line at once, so we have to cache incoming data
  FCWriteTemp += DeleteChar(AnsiString(Data, Length), '\r');

  AnsiString Line;
  // Do we have at least one complete line in std error cache?
  while (FCWriteTemp.Pos("\n") > 0)
  {
    AnsiString Line = CutToChar(FCWriteTemp, '\n', false);

    FLog->Add(llStdError, Line);

    if (FAuthenticating)
    {
      // No point trying to translate message coming from server
      if (!Untrusted)
      {
        TranslateAuthenticationMessage(Line);
      }
      FAuthenticationLog += (FAuthenticationLog.IsEmpty() ? "" : "\n") + Line;
    }

    // Untrusted means generally that it comes from the server,
    // do not use such a output as "information" for end user
    if (!Untrusted)
    {
      FUI->Information(Line, false);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::RegisterReceiveHandler(TNotifyEvent Handler)
{
  assert(FOnReceive == NULL);
  FOnReceive = Handler;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::UnregisterReceiveHandler(TNotifyEvent Handler)
{
  assert(FOnReceive == Handler);
  USEDPARAM(Handler);
  FOnReceive = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FromBackend(bool IsStdErr, const char * Data, int Length)
{
  CheckConnection();

  if (Configuration->LogProtocol >= 1)
  {
    LogEvent(FORMAT("Received %u bytes (%d)", (Length, int(IsStdErr))));
  }

  // Following is taken from scp.c from_backend() and modified

  if (IsStdErr)
  {
    AddStdError(AnsiString(Data, Length));
  }
  else
  {
    unsigned char *p = (unsigned char *)Data;
    unsigned Len = (unsigned)Length;

    // with event-select mechanism we can now receive data even before we
    // actually expect them (OutPtr can be NULL)

    if ((OutPtr != NULL) && (OutLen > 0) && (Len > 0))
    {
      unsigned Used = OutLen;
      if (Used > Len) Used = Len;
      memcpy(OutPtr, p, Used);
      OutPtr += Used; OutLen -= Used;
      p += Used; Len -= Used;
    }

    if (Len > 0)
    {
      if (PendSize < PendLen + Len)
      {
        PendSize = PendLen + Len + 4096;
        Pending = (char *)
          (Pending ? srealloc(Pending, PendSize) : smalloc(PendSize));
        if (!Pending) FatalError("Out of memory");
      }
      memcpy(Pending + PendLen, p, Len);
      PendLen += Len;
    }

    if (FOnReceive != NULL)
    {
      if (!FFrozen)
      {
        FFrozen = true;
        try
        {
          do
          {
            FDataWhileFrozen = false;
            FOnReceive(NULL);
          }
          while (FDataWhileFrozen);
        }
        __finally
        {
          FFrozen = false;
        }
      }
      else
      {
        FDataWhileFrozen = true;
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::Peek(char *& Buf, int Len)
{
  bool Result = (int(PendLen) >= Len);

  if (Result)
  {
    Buf = Pending;
  }

  return Result;
}
//---------------------------------------------------------------------------
Integer __fastcall TSecureShell::Receive(char * Buf, Integer Len)
{
  CheckConnection();

  if (Len > 0)
  {
    // Following is taken from scp.c ssh_scp_recv() and modified

    OutPtr = Buf;
    OutLen = Len;

    try
    {
      /*
       * See if the pending-input block contains some of what we
       * need.
       */
      if (PendLen > 0)
      {
        unsigned PendUsed = PendLen;
        if (PendUsed > OutLen)
        {
          PendUsed = OutLen;
        }
        memcpy(OutPtr, Pending, PendUsed);
        memmove(Pending, Pending + PendUsed, PendLen - PendUsed);
        OutPtr += PendUsed;
        OutLen -= PendUsed;
        PendLen -= PendUsed;
        if (PendLen == 0)
        {
          PendSize = 0;
          sfree(Pending);
          Pending = NULL;
        }
      }

      while (OutLen > 0)
      {
        if (Configuration->LogProtocol >= 1)
        {
          LogEvent(FORMAT("Waiting for another %u bytes", (static_cast<int>(OutLen))));
        }
        WaitForData();
      }

      // This seems ambiguous
      if (Len <= 0) FatalError(LoadStr(LOST_CONNECTION));
    }
    __finally
    {
      OutPtr = NULL;
    }
  };
  if (Configuration->LogProtocol >= 1)
  {
    LogEvent(FORMAT("Read %u bytes (%d pending)",
      (static_cast<int>(Len), static_cast<int>(PendLen))));
  }
  return Len;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSecureShell::ReceiveLine()
{
  unsigned Index;
  Char Ch;
  AnsiString Line;
  Boolean EOL = False;

  do
  {
    // If there is any buffer of received chars
    if (PendLen > 0)
    {
      Index = 0;
      // Repeat until we walk thru whole buffer or reach end-of-line
      while ((Index < PendLen) && (!Index || (Pending[Index-1] != '\n')))
      {
        Index++;
      }
      EOL = (Boolean)(Index && (Pending[Index-1] == '\n'));
      Integer PrevLen = Line.Length();
      Line.SetLength(PrevLen + Index);
      Receive(Line.c_str() + PrevLen, Index);
    }

    // If buffer don't contain end-of-line character
    // we read one more which causes receiving new buffer of chars
    if (!EOL)
    {
      Receive(&Ch, 1);
      Line += Ch;
      EOL = (Ch == '\n');
    }
  }
  while (!EOL);

  // We don't want end-of-line character
  Line.SetLength(Line.Length()-1);
  CaptureOutput(llOutput, Line);
  return Line;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendSpecial(int Code)
{
  LogEvent(FORMAT("Sending special code: %d", (Code)));
  CheckConnection();
  FBackend->special(FBackendHandle, (Telnet_Special)Code);
  CheckConnection();
  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendEOF()
{
  SendSpecial(TS_EOF);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Send(const char * Buf, Integer Len)
{
  CheckConnection();
  int BufSize = FBackend->send(FBackendHandle, (char *)Buf, Len);
  if (Configuration->LogProtocol >= 1)
  {
    LogEvent(FORMAT("Sent %u bytes", (static_cast<int>(Len))));
    LogEvent(FORMAT("There are %u bytes remaining in the send buffer", (BufSize)));
  }
  FLastDataSent = Now();
  // among other forces receive of pending data to free the servers's send buffer
  EventSelectLoop(0, false);

  while (BufSize > MAX_BUFSIZE)
  {
    if (Configuration->LogProtocol >= 1)
    {
      LogEvent(FORMAT("There are %u bytes remaining in the send buffer, "
        "need to send at least another %u bytes",
        (BufSize, BufSize - MAX_BUFSIZE)));
    }
    EventSelectLoop(100, false);
    BufSize = FBackend->sendbuffer(FBackendHandle);
    if (Configuration->LogProtocol >= 1)
    {
      LogEvent(FORMAT("There are %u bytes remaining in the send buffer", (BufSize)));
    }
  }
  CheckConnection();
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendNull()
{
  LogEvent("Sending NULL.");
  Send("", 1);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendStr(AnsiString Str)
{
  CheckConnection();
  Send(Str.c_str(), Str.Length());
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendLine(AnsiString Line)
{
  SendStr(Line);
  Send("\n", 1);
  FLog->Add(llInput, Line);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::TranslatePuttyMessage(
  const TPuttyTranslation * Translation, size_t Count, AnsiString & Message)
{
  bool Result = false;
  for (unsigned int Index = 0; Index < Count; Index++)
  {
    const char * Original = Translation[Index].Original;
    const char * Div = strchr(Original, '%');
    if (Div == NULL)
    {
      if (strcmp(Message.c_str(), Original) == 0)
      {
        Message = LoadStr(Translation[Index].Translation);
        Result = true;
        break;
      }
    }
    else
    {
      size_t OriginalLen = strlen(Original);
      size_t PrefixLen = Div - Original;
      size_t SuffixLen = OriginalLen - PrefixLen - 1;
      if (((size_t)Message.Length() >= OriginalLen - 1) &&
          (strncmp(Message.c_str(), Original, PrefixLen) == 0) &&
          (strncmp(Message.c_str() + Message.Length() - SuffixLen, Div + 1, SuffixLen) == 0))
      {
        Message = FMTLOAD(Translation[Index].Translation,
          (Message.SubString(PrefixLen + 1, Message.Length() - PrefixLen - SuffixLen).TrimRight()));
        Result = true;
        break;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::TranslateAuthenticationMessage(AnsiString & Message)
{
  static const TPuttyTranslation Translation[] = {
    { "Using username \"%\".", AUTH_TRANSL_USERNAME },
    { "Using keyboard-interactive authentication.", AUTH_TRANSL_KEYB_INTER },
    { "Authenticating with public key \"%\" from agent", AUTH_TRANSL_PUBLIC_KEY_AGENT },
    { "Authenticating with public key \"%\"", AUTH_TRANSL_PUBLIC_KEY },
    { "Authenticated using RSA key \"%\" from agent", AUTH_TRANSL_PUBLIC_KEY_AGENT },
    { "Wrong passphrase", AUTH_TRANSL_WRONG_PASSPHRASE },
    { "Wrong passphrase.", AUTH_TRANSL_WRONG_PASSPHRASE },
    { "Access denied", AUTH_TRANSL_ACCESS_DENIED },
    { "Trying public key authentication.", AUTH_TRANSL_TRY_PUBLIC_KEY },
    { "Server refused our public key.", AUTH_TRANSL_KEY_REFUSED },
    { "Server refused our key", AUTH_TRANSL_KEY_REFUSED }
  };

  return TranslatePuttyMessage(Translation, LENOF(Translation), Message);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::AddStdError(AnsiString Str)
{
  FStdError += Str;

  Integer P;
  Str = DeleteChar(Str, '\r');
  // We send only whole line at once to log, so we have to cache
  // incoming std error data
  FStdErrorTemp += Str;
  AnsiString Line;
  // Do we have at least one complete line in std error cache?
  while ((P = FStdErrorTemp.Pos("\n")) > 0)
  {
    Line = FStdErrorTemp.SubString(1, P-1);
    FStdErrorTemp.Delete(1, P);
    AddStdErrorLine(Line);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::AddStdErrorLine(const AnsiString & Str)
{
  if (FAuthenticating)
  {
    FAuthenticationLog += (FAuthenticationLog.IsEmpty() ? "" : "\n") + Str;
  }
  CaptureOutput(llStdError, Str);
}
//---------------------------------------------------------------------------
const AnsiString & __fastcall TSecureShell::GetStdError()
{
  return FStdError;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::ClearStdError()
{
  // Flush std error cache
  if (!FStdErrorTemp.IsEmpty())
  {
    if (FAuthenticating)
    {
      FAuthenticationLog +=
        (FAuthenticationLog.IsEmpty() ? "" : "\n") + FStdErrorTemp;
    }
    CaptureOutput(llStdError, FStdErrorTemp);
    FStdErrorTemp = "";
  }
  FStdError = "";
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::CaptureOutput(TLogLineType Type,
  const AnsiString & Line)
{
  if (FOnCaptureOutput != NULL)
  {
    FOnCaptureOutput(Line, (Type == llStdError));
  }
  FLog->Add(Type, Line);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FatalError(Exception * E, AnsiString Msg)
{
  // the same code is in TTerminal
  if (FActive)
  {
    // We log this instead of exception handler, because Close() would
    // probably cause exception handler to loose pointer to TShellLog()
    LogEvent("Attempt to close connection due to fatal exception:");
    FLog->Add(llException, Msg);
    FLog->AddException(E);
    Close();
  }
  SSH_FATAL_ERROR_EXT(E, Msg);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::TranslateErrorMessage(AnsiString & Message)
{
  static const TPuttyTranslation Translation[] = {
    { "Server unexpectedly closed network connection", UNEXPECTED_CLOSE_ERROR },
    { "Network error: Connection refused", NET_TRANSL_REFUSED },
    { "Network error: Connection reset by peer", NET_TRANSL_RESET },
    { "Network error: Connection timed out", NET_TRANSL_TIMEOUT },
  };

  return TranslatePuttyMessage(Translation, LENOF(Translation), Message);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::PuttyFatalError(AnsiString Error)
{
  TranslateErrorMessage(Error);

  FatalError(Error);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FatalError(AnsiString Error)
{
  FatalError(NULL, Error);
}
//---------------------------------------------------------------------------
void __fastcall inline TSecureShell::LogEvent(const AnsiString & Str)
{
  if (FLog->Logging)
  {
    FLog->Add(llMessage, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SocketEventSelect(SOCKET Socket, HANDLE Event, bool Startup)
{
  int Events;

  if (Startup)
  {
    Events = (FD_CONNECT | FD_READ | FD_WRITE | FD_OOB | FD_CLOSE | FD_ACCEPT);
  }
  else
  {
    Events = 0;
  }

  if (Configuration->LogProtocol >= 2)
  {
    LogEvent(FORMAT("Selecting events %d for socket %d", (int(Events), int(Socket))));
  }

  if (WSAEventSelect(Socket, (WSAEVENT)Event, Events) == SOCKET_ERROR)
  {
    if (Configuration->LogProtocol >= 2)
    {
      LogEvent(FORMAT("Error selecting events %d for socket %d", (int(Events), int(Socket))));
    }

    if (Startup)
    {
      FatalError(FMTLOAD(EVENT_SELECT_ERROR, (WSAGetLastError())));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::UpdateSocket(SOCKET value, bool Startup)
{
  assert(value);
  assert((FActive && (FSocket == value)) || (!FActive && Startup));

  SocketEventSelect(value, FSocketEvent, Startup);

  if (Startup)
  {
    FSocket = value;
    FActive = true;
  }
  else
  {
    FSocket = INVALID_SOCKET;
    Discard();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::UpdatePortFwdSocket(SOCKET value, bool Startup)
{
  if (Configuration->LogProtocol >= 2)
  {
    LogEvent(FORMAT("Updating forwarding socket %d (%d)", (int(value), int(Startup))));
  }

  SocketEventSelect(value, FSocketEvent, Startup);

  if (Startup)
  {
    FPortFwdSockets.insert(value);
  }
  else
  {
    FPortFwdSockets.erase(value);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetActive(bool value)
{
  if (FActive != value)
  {
    if (value)
    {
      Open();
    }
    else
    {
      Close();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FreeBackend()
{
  if (FBackendHandle != NULL)
  {
    FBackend->free(FBackendHandle);
    FBackendHandle = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Discard()
{
  bool WasActive = FActive;
  FActive = false;

  if (WasActive)
  {
    FUI->Closed();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Close()
{
  LogEvent("Closing connection.");
  assert(FActive);

  FreeBackend();

  Discard();
}
//---------------------------------------------------------------------------
void inline __fastcall TSecureShell::CheckConnection(int Message)
{
  if (!FActive || get_ssh_state_closed(FBackendHandle))
  {
    AnsiString Str = LoadStr(Message >= 0 ? Message : NOT_CONNECTED);
    int ExitCode = get_ssh_exitcode(FBackendHandle);
    if (ExitCode >= 0)
    {
      Str += " " + FMTLOAD(SSH_EXITCODE, (ExitCode));
    }
    FatalError(Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::PoolForData(unsigned int & Result)
{
  try
  {
    if (Configuration->LogProtocol >= 2)
    {
      LogEvent("Pooling for data in case they finally arrives");
    }

    if (EventSelectLoop(0, false))
    {
      LogEvent("Data has arrived, closing query to user.");
      Result = qaOK;
    }
  }
  catch(...)
  {
    // if we let the exception out, it may popup another message dialog
    // in whole event loop, another call to PoolForData from original dialog
    // would be invoked, leading to an infinite loop.
    // by retrying we hope (that probably fatal) error would repeat in WaitForData
    Result = qaRetry;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::WaitForData()
{
  // see winsftp.c
  bool IncomingData;

  do
  {
    if (Configuration->LogProtocol >= 2)
    {
      LogEvent("Looking for incoming data");
    }

    IncomingData = EventSelectLoop(FSessionData->Timeout * 1000, true);
    if (!IncomingData)
    {
      LogEvent("Waiting for data timed out, asking user what to do.");
      TQueryParams Params(qpFatalAbort | qpAllowContinueOnError);
      Params.Timer = 500;
      Params.TimerEvent = PoolForData;
      Params.TimerMessage = FMTLOAD(TIMEOUT_STILL_WAITING, (FSessionData->Timeout));
      Params.TimerAnswers = qaAbort;
      int Answer = FUI->QueryUser(FMTLOAD(CONFIRM_PROLONG_TIMEOUT2, (FSessionData->Timeout)),
        NULL, qaRetry | qaAbort, &Params);
      switch (Answer)
      {
        case qaRetry:
          // noop
          break;

        case qaOK:
          // read event was already captured in PoolForData(),
          // make sure we do not try to select it again as it would timeout
          // unless another read event occurs
          IncomingData = true;
          break;

        default:
          assert(false);
          // fallthru

        case qaAbort:
          FatalError(LoadStr(USER_TERMINATED));
          break;
      }
    }
  }
  while (!IncomingData);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::SshFallbackCmd() const
{
  return ssh_fallback_cmd(FBackendHandle);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::ProcessNetworkEvents(SOCKET Socket)
{
  bool Result = false;

  if (Configuration->LogProtocol >= 2)
  {
    LogEvent(FORMAT("Enumerating network events for socket %d", (int(Socket))));
  }

  // see winplink.c
  WSANETWORKEVENTS Events;
  if (WSAEnumNetworkEvents(Socket, NULL, &Events) == 0)
  {
    static const struct { int Bit, Mask; const char * Desc; } EventTypes[] =
    {
      { FD_READ_BIT, FD_READ, "read" },
      { FD_WRITE_BIT, FD_WRITE, "write" },
      { FD_OOB_BIT, FD_OOB, "oob" },
      { FD_ACCEPT_BIT, FD_ACCEPT, "accept" },
      { FD_CONNECT_BIT, FD_CONNECT, "connect" },
      { FD_CLOSE_BIT, FD_CLOSE, "close" },
    };

    noise_ultralight(Socket);
    noise_ultralight(Events.lNetworkEvents);

    if (Configuration->LogProtocol >= 2)
    {
      LogEvent(FORMAT("Enumerated %d network events for socket %d",
        (int(Events.lNetworkEvents), int(Socket))));
    }

    for (int Event = 0; Event < LENOF(EventTypes); Event++)
    {
      if (Events.lNetworkEvents & EventTypes[Event].Mask)
      {
        if (EventTypes[Event].Mask & FD_READ)
        {
          Result = true;
        }

        int Err = Events.iErrorCode[EventTypes[Event].Bit];
        if (Configuration->LogProtocol >= 2)
        {
          LogEvent(FORMAT("Detected network %s event on socket %d with error %d",
            (EventTypes[Event].Desc, int(Socket), Err)));
        }
        #pragma option push -w-prc
        LPARAM SelectEvent = WSAMAKESELECTREPLY(EventTypes[Event].Mask, Err);
        #pragma option pop
        if (!select_result((WPARAM)Socket, SelectEvent))
        {
          // note that connection was closed definitely,
          // so "check" is actually not required
          CheckConnection();
        }
      }
    }
  }
  else
  {
    if (Configuration->LogProtocol >= 2)
    {
      LogEvent(FORMAT("Error enumerating network events for socket %d", (int(Socket))));
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::EventSelectLoop(unsigned int MSec, bool ReadEventRequired)
{
  CheckConnection();

  bool Result = false;

  do
  {
    if (Configuration->LogProtocol >= 2)
    {
      LogEvent("Looking for network events");
    }

    unsigned int TicksBefore = GetTickCount();
    int WaitResult = WaitForSingleObject(FSocketEvent, MSec);
    switch (WaitResult)
    {
      case WAIT_OBJECT_0:
        if (Configuration->LogProtocol >= 1)
        {
          LogEvent("Detected network event");
        }

        if (ProcessNetworkEvents(FSocket))
        {
          Result = true;
        }

        {
          TSockets::iterator i = FPortFwdSockets.begin();
          while (i != FPortFwdSockets.end())
          {
            ProcessNetworkEvents(*i);
            i++;
          }
        }
        break;

      case WAIT_TIMEOUT:
        if (Configuration->LogProtocol >= 2)
        {
          LogEvent("Timeout waiting for network events");
        }

        MSec = 0;
        break;

      default:
        if (Configuration->LogProtocol >= 2)
        {
          LogEvent(FORMAT("Unknown waiting result %d", (WaitResult)));
        }

        MSec = 0;
        break;
    }

    unsigned int TicksAfter = GetTickCount();
    // ticks wraps once in 49.7 days
    if (TicksBefore < TicksAfter)
    {
      unsigned int Ticks = TicksAfter - TicksBefore;
      if (Ticks > MSec)
      {
        MSec = 0;
      }
      else
      {
        MSec -= Ticks;
      }
    }
  }
  while (ReadEventRequired && (MSec > 0) && !Result);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Idle(unsigned int MSec)
{
  noise_regular();

  call_ssh_timer(FBackendHandle);

  EventSelectLoop(MSec, false);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::KeepAlive()
{
  LogEvent("Sending null packet to keep session alive.");
  SendSpecial(TS_PING);
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSecureShell::MaxPacketSize()
{
  if (!FSessionInfoValid)
  {
    UpdateSessionInfo();
  }

  if (FSshVersion == 1)
  {
    return 0;
  }
  else
  {
    if (FMaxPacketSize == NULL)
    {
      FMaxPacketSize = ssh2_remmaxpkt(FBackendHandle);
    }
    return *FMaxPacketSize;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSecureShell::FuncToCompression(
  int SshVersion, const void * Compress) const
{
  enum TCompressionType { ctNone, ctZLib };
  if (SshVersion == 1)
  {
    return get_ssh1_compressing(FBackendHandle) ? "ZLib" : "";
  }
  else
  {
    return (ssh_compress *)Compress == &ssh_zlib ? "ZLib" : "";
  }
}
//---------------------------------------------------------------------------
TCipher __fastcall TSecureShell::FuncToSsh1Cipher(const void * Cipher)
{
  const ssh_cipher *CipherFuncs[] =
    {&ssh_3des, &ssh_des, &ssh_blowfish_ssh1};
  const TCipher TCiphers[] = {cip3DES, cipDES, cipBlowfish};
  assert(LENOF(CipherFuncs) == LENOF(TCiphers));
  TCipher Result = cipWarn;

  for (int Index = 0; Index < LENOF(TCiphers); Index++)
  {
    if ((ssh_cipher *)Cipher == CipherFuncs[Index])
    {
      Result = TCiphers[Index];
    }
  }

  assert(Result != cipWarn);
  return Result;
}
//---------------------------------------------------------------------------
TCipher __fastcall TSecureShell::FuncToSsh2Cipher(const void * Cipher)
{
  const ssh2_ciphers *CipherFuncs[] =
    {&ssh2_3des, &ssh2_des, &ssh2_aes, &ssh2_blowfish};
  const TCipher TCiphers[] = {cip3DES, cipDES, cipAES, cipBlowfish};
  assert(LENOF(CipherFuncs) == LENOF(TCiphers));
  TCipher Result = cipWarn;

  for (int C = 0; C < LENOF(TCiphers); C++)
  {
    for (int F = 0; F < CipherFuncs[C]->nciphers; F++)
    {
      if ((ssh2_cipher *)Cipher == CipherFuncs[C]->list[F])
      {
        Result = TCiphers[C];
      }
    }
  }

  assert(Result != cipWarn);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::VerifyHostKey(const AnsiString Host, int Port,
  const AnsiString KeyType, const AnsiString KeyStr, const AnsiString Fingerprint)
{
  GotHostKey();

  int Result;

  FSessionInfo.HostKeyFingerprint = Fingerprint;

  // Verify the key against the registry.
  Result = verify_host_key(Host.c_str(), Port, KeyType.c_str(), KeyStr.c_str());

  if (Result != 0)
  {
    if (Configuration->DisableAcceptingHostKeys)
    {
      FatalError(LoadStr(KEY_NOT_VERIFIED));
    }
    else
    {
      TQueryParams Params;
      Params.HelpKeyword = (Result == 1 ? HELP_UNKNOWN_KEY : HELP_DIFFERENT_KEY);
      int R = FUI->QueryUser(
        FMTLOAD((Result == 1 ? UNKNOWN_KEY2 : DIFFERENT_KEY2), (KeyType, Fingerprint)),
        NULL, qaYes | qaNo | qaCancel, &Params, qtWarning);

      switch (R) {
        case qaYes:
          store_host_key(Host.c_str(), Port, KeyType.c_str(), KeyStr.c_str());
          break;

        case qaCancel:
          FatalError(LoadStr(KEY_NOT_VERIFIED));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::AskAlg(const AnsiString AlgType,
  const AnsiString AlgName)
{
  AnsiString Msg;
  if (AlgType == "key-exchange algorithm")
  {
    Msg = FMTLOAD(KEX_BELOW_TRESHOLD, (AlgName));
  }
  else
  {
    int CipherType;
    if (AlgType == "cipher")
    {
      CipherType = CIPHER_TYPE_BOTH;
    }
    else if (AlgType == "client-to-server cipher")
    {
      CipherType = CIPHER_TYPE_CS;
    }
    else if (AlgType == "server-to-client cipher")
    {
      CipherType = CIPHER_TYPE_SC;
    }
    else
    {
      assert(false);
    }

    Msg = FMTLOAD(CIPHER_BELOW_TRESHOLD, (LoadStr(CipherType), AlgName));
  }

  if (FUI->QueryUser(Msg, NULL, qaYes | qaNo, NULL, qtWarning) == qaNo)
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::DisplayBanner(const AnsiString & Banner)
{
  FUI->DisplayBanner(Banner);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::OldKeyfileWarning()
{
  // actually never called, see Net.cpp
  FUI->QueryUser(LoadStr(OLD_KEY), NULL, qaOK, NULL, qtWarning);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::GetStoredCredentialsTried()
{
  return FStoredPasswordTried || FStoredPasswordTriedForKI;
}
