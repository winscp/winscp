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
  FWaiting = 0;
  FOpened = false;
  OutPtr = NULL;
  Pending = NULL;
  FBackendHandle = NULL;
  ResetConnection();
  FOnCaptureOutput = NULL;
  FOnReceive = NULL;
  FConfig = new Config();
  memset(FConfig, 0, sizeof(*FConfig));
  FSocket = INVALID_SOCKET;
  FSocketEvent = CreateEvent(NULL, false, false, NULL);
  FFrozen = false;
  FSimple = false;
}
//---------------------------------------------------------------------------
__fastcall TSecureShell::~TSecureShell()
{
  assert(FWaiting == 0);
  Active = false;
  ResetConnection();
  CloseHandle(FSocketEvent);
  ClearConfig(FConfig);
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
void __fastcall TSecureShell::ClearConfig(Config * cfg)
{
  StrDispose(cfg->remote_cmd_ptr);
  StrDispose(cfg->remote_cmd_ptr2);
  // clear all
  memset(cfg, 0, sizeof(*cfg));
}
//---------------------------------------------------------------------
void __fastcall TSecureShell::StoreToConfig(TSessionData * Data, Config * cfg, bool Simple)
{
  ClearConfig(cfg);

  // user-configurable settings
  ASCOPY(cfg->host, Data->HostName);
  ASCOPY(cfg->username, Data->UserName);
  cfg->port = Data->PortNumber;
  cfg->protocol = PROT_SSH;
  // always set 0, as we will handle keepalives ourselves to avoid
  // multi-threaded issues in putty timer list
  cfg->ping_interval = 0;
  cfg->compression = Data->Compression;
  cfg->tryagent = Data->TryAgent;
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
      case cipArcfour: pcipher = CIPHER_ARCFOUR; break;
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
      case kexRSA: pkex = KEX_RSA; break;
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
  cfg->ssh_no_userauth = Data->SshNoUserAuth;
  cfg->try_tis_auth = Data->AuthTIS;
  cfg->try_ki_auth = Data->AuthKI;
  cfg->try_gssapi_auth = Data->AuthGSSAPI;
  cfg->gssapifwd = Data->GSSAPIFwdTGT;
  cfg->change_username = Data->ChangeUsername;

  cfg->proxy_type = Data->ProxyMethod;
  ASCOPY(cfg->proxy_host, Data->ProxyHost);
  cfg->proxy_port = Data->ProxyPort;
  ASCOPY(cfg->proxy_username, Data->ProxyUsername);
  ASCOPY(cfg->proxy_password, Data->ProxyPassword);
  if (Data->ProxyMethod == pmCmd)
  {
    ASCOPY(cfg->proxy_telnet_command, Data->ProxyLocalCommand);
  }
  else
  {
    ASCOPY(cfg->proxy_telnet_command, Data->ProxyTelnetCommand);
  }
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
  cfg->sshbug_maxpkt2 = Data->Bug[sbMaxPkt2];
  #pragma option pop

  if (!Data->TunnelPortFwd.IsEmpty())
  {
    assert(!Simple);
    ASCOPY(cfg->portfwd, Data->TunnelPortFwd);
    // when setting up a tunnel, do not open shell/sftp
    cfg->ssh_no_shell = TRUE;
  }
  else
  {
    assert(Simple);
    cfg->ssh_simple = Simple;

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
        cfg->remote_cmd_ptr = StrNew(Data->Shell.c_str());
      }
    }
    else
    {
      if (Data->SftpServer.IsEmpty())
      {
        cfg->ssh_subsys = TRUE;
        strcpy(cfg->remote_cmd, "sftp");
      }
      else
      {
        cfg->ssh_subsys = FALSE;
        cfg->remote_cmd_ptr = StrNew(Data->SftpServer.c_str());
      }

      if (Data->FSProtocol != fsSFTPonly)
      {
        cfg->ssh_subsys2 = FALSE;
        if (Data->Shell.IsEmpty())
        {
          // Following forces Putty to open default shell
          // see ssh.c: do_ssh2_authconn() and ssh1_protocol()
          cfg->remote_cmd_ptr2 = StrNew("\0");
        }
        else
        {
          cfg->remote_cmd_ptr2 = StrNew(Data->Shell.c_str());
        }
      }

      if ((Data->FSProtocol == fsSFTPonly) && Data->SftpServer.IsEmpty())
      {
        // see psftp_connect() from psftp.c
        cfg->ssh_subsys2 = FALSE;
        cfg->remote_cmd_ptr2 = StrNew(
          "test -x /usr/lib/sftp-server && exec /usr/lib/sftp-server\n"
          "test -x /usr/local/lib/sftp-server && exec /usr/local/lib/sftp-server\n"
          "exec sftp-server");
      }
    }
  }

  // permanent settings
  cfg->nopty = TRUE;
  cfg->tcp_keepalives = 0;
  cfg->ssh_show_banner = TRUE;
  for (int Index = 0; Index < ngsslibs; Index++)
  {
    cfg->ssh_gsslist[Index] = gsslibkeywords[Index].v;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Open()
{
  FBackend = &ssh_backend;
  ResetConnection();

  FAuthenticating = false;
  FAuthenticated = false;

  Active = false;

  FAuthenticationLog = "";
  FUI->Information(LoadStr(STATUS_LOOKUPHOST), true);
  StoreToConfig(FSessionData, FConfig, Simple);

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
  Init();

  CheckConnection(CONNECTION_FAILED);
  FLastDataSent = Now();

  FSessionInfo.LoginTime = Now();

  FAuthenticating = false;
  FAuthenticated = true;
  FUI->Information(LoadStr(STATUS_AUTHENTICATED), true);

  ResetSessionInfo();

  assert(!FSessionInfo.SshImplementation.IsEmpty());
  FOpened = true;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Init()
{
  try
  {
    try
    {
      // Recent pscp checks FBackend->exitcode(FBackendHandle) in the loop
      // (see comment in putty revision 8110)
      // It seems that we do not need to do it.

      while (!get_ssh_state_session(FBackendHandle))
      {
        if (Configuration->ActualLogProtocol >= 1)
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
        FUI->FatalError(&E, FMTLOAD(AUTHENTICATION_LOG, (FAuthenticationLog)));
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
      FUI->FatalError(&E, LoadStr(AUTHENTICATION_FAILED));
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
bool __fastcall TSecureShell::PromptUser(bool /*ToServer*/,
  AnsiString AName, bool /*NameRequired*/,
  AnsiString Instructions, bool InstructionsRequired,
  TStrings * Prompts, TStrings * Results)
{
  // there can be zero prompts!

  assert(Results->Count == Prompts->Count);

  TPromptKind PromptKind;
  // beware of changing order
  static const TPuttyTranslation NameTranslation[] = {
    { "SSH login name", USERNAME_TITLE },
    { "SSH key passphrase", PASSPHRASE_TITLE },
    { "SSH TIS authentication", SERVER_PROMPT_TITLE },
    { "SSH CryptoCard authentication", SERVER_PROMPT_TITLE },
    { "SSH server: %", SERVER_PROMPT_TITLE2 },
    { "SSH server authentication", SERVER_PROMPT_TITLE },
    { "SSH password", PASSWORD_TITLE },
    { "New SSH password", NEW_PASSWORD_TITLE },
  };

  AnsiString Name = AName;
  int Index = TranslatePuttyMessage(NameTranslation, LENOF(NameTranslation), Name);

  const TPuttyTranslation * InstructionTranslation = NULL;
  const TPuttyTranslation * PromptTranslation = NULL;
  size_t PromptTranslationCount = 1;

  if (Index == 0) // username
  {
    static const TPuttyTranslation UsernamePromptTranslation[] = {
      { "login as: ", USERNAME_PROMPT2 },
    };

    PromptTranslation = UsernamePromptTranslation;
    PromptKind = pkUserName;
  }
  else if (Index == 1) // passhrase
  {
    static const TPuttyTranslation PassphrasePromptTranslation[] = {
      { "Passphrase for key \"%\": ", PROMPT_KEY_PASSPHRASE },
    };

    PromptTranslation = PassphrasePromptTranslation;
    PromptKind = pkPassphrase;
  }
  else if (Index == 2) // TIS
  {
    static const TPuttyTranslation TISInstructionTranslation[] = {
      { "Using TIS authentication.%", TIS_INSTRUCTION },
    };
    static const TPuttyTranslation TISPromptTranslation[] = {
      { "Response: ", PROMPT_PROMPT },
    };

    InstructionTranslation = TISInstructionTranslation;
    PromptTranslation = TISPromptTranslation;
    PromptKind = pkTIS;
  }
  else if (Index == 3) // CryptoCard
  {
    static const TPuttyTranslation CryptoCardInstructionTranslation[] = {
      { "Using CryptoCard authentication.%", CRYPTOCARD_INSTRUCTION },
    };
    static const TPuttyTranslation CryptoCardPromptTranslation[] = {
      { "Response: ", PROMPT_PROMPT },
    };

    InstructionTranslation = CryptoCardInstructionTranslation;
    PromptTranslation = CryptoCardPromptTranslation;
    PromptKind = pkCryptoCard;
  }
  else if ((Index == 4) || (Index == 5))
  {
    static const TPuttyTranslation KeybInteractiveInstructionTranslation[] = {
      { "Using keyboard-interactive authentication.%", KEYBINTER_INSTRUCTION },
    };

    InstructionTranslation = KeybInteractiveInstructionTranslation;
    PromptKind = pkKeybInteractive;
  }
  else if (Index == 6)
  {
    assert(Prompts->Count == 1);
    Prompts->Strings[0] = LoadStr(PASSWORD_PROMPT);
    PromptKind = pkPassword;
  }
  else if (Index == 7)
  {
    static const TPuttyTranslation NewPasswordPromptTranslation[] = {
      { "Current password (blank for previously entered password): ", NEW_PASSWORD_CURRENT_PROMPT },
      { "Enter new password: ", NEW_PASSWORD_NEW_PROMPT },
      { "Confirm new password: ", NEW_PASSWORD_CONFIRM_PROMPT },
    };
    PromptTranslation = NewPasswordPromptTranslation;
    PromptTranslationCount = LENOF(NewPasswordPromptTranslation);
    PromptKind = pkNewPassword;
  }
  else
  {
    PromptKind = pkPrompt;
    assert(false);
  }

  LogEvent(FORMAT("Prompt (%d, %s, %s, %s)", (PromptKind, AName, Instructions, (Prompts->Count > 0 ? Prompts->Strings[0] : AnsiString("<no prompt>")))));

  Name = Name.Trim();

  if (InstructionTranslation != NULL)
  {
    TranslatePuttyMessage(InstructionTranslation, 1, Instructions);
  }

  // some servers add leading blank line to make the prompt look prettier
  // on terminal console
  Instructions = Instructions.Trim();

  for (int Index = 0; Index < Prompts->Count; Index++)
  {
    AnsiString Prompt = Prompts->Strings[Index];
    if (PromptTranslation != NULL)
    {
      TranslatePuttyMessage(PromptTranslation, PromptTranslationCount, Prompt);
    }
    // some servers add leading blank line to make the prompt look prettier
    // on terminal console
    Prompts->Strings[Index] = Prompt.Trim();
  }

  bool Result = false;
  if (PromptKind == pkUserName)
  {
    if (FSessionData->AuthGSSAPI)
    {
      // use empty username if no username was filled on login dialog
      // and GSSAPI auth is enabled, hence there's chance that the server can
      // deduce the username otherwise
      Results->Strings[0] = "";
      Result = true;
    }
  }
  else if ((PromptKind == pkTIS) || (PromptKind == pkCryptoCard) ||
      (PromptKind == pkKeybInteractive))
  {
    if (FSessionData->AuthKIPassword && !FSessionData->Password.IsEmpty() &&
        !FStoredPasswordTriedForKI && (Prompts->Count == 1) &&
        !bool(Prompts->Objects[0]))
    {
      LogEvent("Using stored password.");
      FUI->Information(LoadStr(AUTH_PASSWORD), false);
      Result = true;
      Results->Strings[0] = FSessionData->Password;
      FStoredPasswordTriedForKI = true;
    }
    else if (Instructions.IsEmpty() && !InstructionsRequired && (Prompts->Count == 0))
    {
      LogEvent("Ignoring empty SSH server authentication request");
      Result = true;
    }
  }
  else if (PromptKind == pkPassword)
  {
    if (!FSessionData->Password.IsEmpty() && !FStoredPasswordTried)
    {
      LogEvent("Using stored password.");
      FUI->Information(LoadStr(AUTH_PASSWORD), false);
      Result = true;
      Results->Strings[0] = FSessionData->Password;
      FStoredPasswordTried = true;
    }
  }

  if (!Result)
  {
    Result = FUI->PromptUser(FSessionData,
      PromptKind, Name, Instructions, Prompts, Results);

    if (Result)
    {
      if ((PromptKind == pkUserName) && (Prompts->Count == 1))
      {
        FUserName = Results->Strings[0];
      }
    }
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
void __fastcall TSecureShell::CWrite(const char * Data, int Length)
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
      TranslateAuthenticationMessage(Line);
      FAuthenticationLog += (FAuthenticationLog.IsEmpty() ? "" : "\n") + Line;
    }

    FUI->Information(Line, false);
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

  if (Configuration->ActualLogProtocol >= 1)
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
        if (Configuration->ActualLogProtocol >= 1)
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
  if (Configuration->ActualLogProtocol >= 1)
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
int __fastcall TSecureShell::TimeoutPrompt(TQueryParamsTimerEvent PoolEvent)
{
  FWaiting++;

  int Answer;
  try
  {
    TQueryParams Params(qpFatalAbort | qpAllowContinueOnError);
    Params.Timer = 500;
    Params.TimerEvent = PoolEvent;
    Params.TimerMessage = FMTLOAD(TIMEOUT_STILL_WAITING2, (FSessionData->Timeout));
    Params.TimerAnswers = qaAbort;
    Answer = FUI->QueryUser(FMTLOAD(CONFIRM_PROLONG_TIMEOUT3, (FSessionData->Timeout)),
      NULL, qaRetry | qaAbort, &Params);
  }
  __finally
  {
    FWaiting--;
  }
  return Answer;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendBuffer(unsigned int & Result)
{
  // for comments see PoolForData
  if (!Active)
  {
    Result = qaRetry;
  }
  else
  {
    try
    {
      if (FBackend->sendbuffer(FBackendHandle) <= MAX_BUFSIZE)
      {
        Result = qaOK;
      }
    }
    catch(...)
    {
      Result = qaRetry;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::DispatchSendBuffer(int BufSize)
{
  TDateTime Start = Now();
  do
  {
    CheckConnection();
    if (Configuration->ActualLogProtocol >= 1)
    {
      LogEvent(FORMAT("There are %u bytes remaining in the send buffer, "
        "need to send at least another %u bytes",
        (BufSize, BufSize - MAX_BUFSIZE)));
    }
    EventSelectLoop(100, false, NULL);
    BufSize = FBackend->sendbuffer(FBackendHandle);
    if (Configuration->ActualLogProtocol >= 1)
    {
      LogEvent(FORMAT("There are %u bytes remaining in the send buffer", (BufSize)));
    }

    if (Now() - Start > FSessionData->TimeoutDT)
    {
      LogEvent("Waiting for dispatching send buffer timed out, asking user what to do.");
      int Answer = TimeoutPrompt(SendBuffer);
      switch (Answer)
      {
        case qaRetry:
          Start = Now();
          break;

        case qaOK:
          BufSize = 0;
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
  while (BufSize > MAX_BUFSIZE);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Send(const char * Buf, Integer Len)
{
  CheckConnection();
  int BufSize = FBackend->send(FBackendHandle, (char *)Buf, Len);
  if (Configuration->ActualLogProtocol >= 1)
  {
    LogEvent(FORMAT("Sent %u bytes", (static_cast<int>(Len))));
    LogEvent(FORMAT("There are %u bytes remaining in the send buffer", (BufSize)));
  }
  FLastDataSent = Now();
  // among other forces receive of pending data to free the servers's send buffer
  EventSelectLoop(0, false, NULL);

  if (BufSize > MAX_BUFSIZE)
  {
    DispatchSendBuffer(BufSize);
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
int __fastcall TSecureShell::TranslatePuttyMessage(
  const TPuttyTranslation * Translation, size_t Count, AnsiString & Message)
{
  int Result = -1;
  for (unsigned int Index = 0; Index < Count; Index++)
  {
    const char * Original = Translation[Index].Original;
    const char * Div = strchr(Original, '%');
    if (Div == NULL)
    {
      if (strcmp(Message.c_str(), Original) == 0)
      {
        Message = LoadStr(Translation[Index].Translation);
        Result = int(Index);
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
        Result = int(Index);
        break;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::TranslateAuthenticationMessage(AnsiString & Message)
{
  static const TPuttyTranslation Translation[] = {
    { "Using username \"%\".", AUTH_TRANSL_USERNAME },
    { "Using keyboard-interactive authentication.", AUTH_TRANSL_KEYB_INTER }, // not used anymore
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
int __fastcall TSecureShell::TranslateErrorMessage(AnsiString & Message)
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
  FUI->FatalError(NULL, Error);
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

  if (Configuration->ActualLogProtocol >= 2)
  {
    LogEvent(FORMAT("Selecting events %d for socket %d", (int(Events), int(Socket))));
  }

  if (WSAEventSelect(Socket, (WSAEVENT)Event, Events) == SOCKET_ERROR)
  {
    if (Configuration->ActualLogProtocol >= 2)
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
  if (!FActive && !Startup)
  {
    // no-op
    // Remove the branch eventualy:
    // When TCP connection fails, PuTTY does not release the memory allocate for
    // socket. As a simple hack we call sk_tcp_close() in ssh.c to release the memory,
    // until they fix it better. Unfortunately sk_tcp_close calls do_select,
    // so we must filter that out.
  }
  else
  {
    assert(value);
    assert((FActive && (FSocket == value)) || (!FActive && Startup));

    // filter our "local proxy" connection, which have no socket
    if (value != INVALID_SOCKET)
    {
      SocketEventSelect(value, FSocketEvent, Startup);
    }
    else
    {
      assert(FSessionData->ProxyMethod == pmCmd);
    }

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
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::UpdatePortFwdSocket(SOCKET value, bool Startup)
{
  if (Configuration->ActualLogProtocol >= 2)
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
  FOpened = false;

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

  // this is particularly necessary when using local proxy command
  // (e.g. plink), otherwise it hangs in sk_localproxy_close
  SendEOF();

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
void __fastcall TSecureShell::PoolForData(WSANETWORKEVENTS & Events, unsigned int & Result)
{
  if (!Active)
  {
    // see comment below
    Result = qaRetry;
  }
  else
  {
    try
    {
      if (Configuration->ActualLogProtocol >= 2)
      {
        LogEvent("Pooling for data in case they finally arrives");
      }

      // in extreme condition it may happen that send buffer is full, but there
      // will be no data comming and we may not empty the send buffer because we
      // do not process FD_WRITE until we receive any FD_READ
      if (EventSelectLoop(0, false, &Events))
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
      // by retrying we hope (that probably fatal) error would repeat in WaitForData.
      // anyway now once no actual work is done in EventSelectLoop,
      // hardly any exception can occur actually
      Result = qaRetry;
    }
  }
}
//---------------------------------------------------------------------------
class TPoolForDataEvent
{
public:
  __fastcall TPoolForDataEvent(TSecureShell * SecureShell, WSANETWORKEVENTS & Events) :
    FSecureShell(SecureShell),
    FEvents(Events)
  {
  }

  void __fastcall PoolForData(unsigned int & Result)
  {
    FSecureShell->PoolForData(FEvents, Result);
  }

private:
  TSecureShell * FSecureShell;
  WSANETWORKEVENTS & FEvents;
};
//---------------------------------------------------------------------------
void __fastcall TSecureShell::WaitForData()
{
  // see winsftp.c
  bool IncomingData;

  do
  {
    if (Configuration->ActualLogProtocol >= 2)
    {
      LogEvent("Looking for incoming data");
    }

    IncomingData = EventSelectLoop(FSessionData->Timeout * 1000, true, NULL);
    if (!IncomingData)
    {
      WSANETWORKEVENTS Events;
      memset(&Events, 0, sizeof(Events));
      TPoolForDataEvent Event(this, Events);

      LogEvent("Waiting for data timed out, asking user what to do.");
      int Answer = TimeoutPrompt(Event.PoolForData);
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
          HandleNetworkEvents(FSocket, Events);
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
bool __fastcall TSecureShell::EnumNetworkEvents(SOCKET Socket, WSANETWORKEVENTS & Events)
{
  if (Configuration->ActualLogProtocol >= 2)
  {
    LogEvent(FORMAT("Enumerating network events for socket %d", (int(Socket))));
  }

  // see winplink.c
  WSANETWORKEVENTS AEvents;
  if (WSAEnumNetworkEvents(Socket, NULL, &AEvents) == 0)
  {
    noise_ultralight(Socket);
    noise_ultralight(AEvents.lNetworkEvents);

    Events.lNetworkEvents |= AEvents.lNetworkEvents;
    for (int Index = 0; Index < FD_MAX_EVENTS; Index++)
    {
      if (AEvents.iErrorCode[Index] != 0)
      {
        Events.iErrorCode[Index] = AEvents.iErrorCode[Index];
      }
    }

    if (Configuration->ActualLogProtocol >= 2)
    {
      LogEvent(FORMAT("Enumerated %d network events making %d cumulative events for socket %d",
        (int(AEvents.lNetworkEvents), int(Events.lNetworkEvents), int(Socket))));
    }
  }
  else
  {
    if (Configuration->ActualLogProtocol >= 2)
    {
      LogEvent(FORMAT("Error enumerating network events for socket %d", (int(Socket))));
    }
  }

  return
    FLAGSET(Events.lNetworkEvents, FD_READ) ||
    FLAGSET(Events.lNetworkEvents, FD_CLOSE);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::HandleNetworkEvents(SOCKET Socket, WSANETWORKEVENTS & Events)
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

  for (int Event = 0; Event < LENOF(EventTypes); Event++)
  {
    if (FLAGSET(Events.lNetworkEvents, EventTypes[Event].Mask))
    {
      int Err = Events.iErrorCode[EventTypes[Event].Bit];
      if (Configuration->ActualLogProtocol >= 2)
      {
        LogEvent(FORMAT("Handling network %s event on socket %d with error %d",
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
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::ProcessNetworkEvents(SOCKET Socket)
{
  WSANETWORKEVENTS Events;
  memset(&Events, 0, sizeof(Events));
  bool Result = EnumNetworkEvents(Socket, Events);
  HandleNetworkEvents(Socket, Events);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::EventSelectLoop(unsigned int MSec, bool ReadEventRequired,
  WSANETWORKEVENTS * Events)
{
  CheckConnection();

  bool Result = false;

  do
  {
    if (Configuration->ActualLogProtocol >= 2)
    {
      LogEvent("Looking for network events");
    }
    unsigned int TicksBefore = GetTickCount();
    int HandleCount;
    // note that this returns all handles, not only the session-related handles
    HANDLE * Handles = handle_get_events(&HandleCount);
    try
    {
      Handles = sresize(Handles, HandleCount + 1, HANDLE);
      Handles[HandleCount] = FSocketEvent;
      unsigned int WaitResult = WaitForMultipleObjects(HandleCount + 1, Handles, FALSE, MSec);
      if (WaitResult < WAIT_OBJECT_0 + HandleCount)
      {
        if (handle_got_event(Handles[WaitResult - WAIT_OBJECT_0]))
        {
          Result = true;
        }
      }
      else if (WaitResult == WAIT_OBJECT_0 + HandleCount)
      {
        if (Configuration->ActualLogProtocol >= 1)
        {
          LogEvent("Detected network event");
        }

        if (Events == NULL)
        {
          if (ProcessNetworkEvents(FSocket))
          {
            Result = true;
          }
        }
        else
        {
          if (EnumNetworkEvents(FSocket, *Events))
          {
            Result = true;
          }
        }

        {
          TSockets::iterator i = FPortFwdSockets.begin();
          while (i != FPortFwdSockets.end())
          {
            ProcessNetworkEvents(*i);
            i++;
          }
        }
      }
      else if (WaitResult == WAIT_TIMEOUT)
      {
        if (Configuration->ActualLogProtocol >= 2)
        {
          LogEvent("Timeout waiting for network events");
        }

        MSec = 0;
      }
      else
      {
        if (Configuration->ActualLogProtocol >= 2)
        {
          LogEvent(FORMAT("Unknown waiting result %d", (int(WaitResult))));
        }

        MSec = 0;
      }
    }
    __finally
    {
      sfree(Handles);
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

  EventSelectLoop(MSec, false, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::KeepAlive()
{
  if (FActive && (FWaiting == 0))
  {
    LogEvent("Sending null packet to keep session alive.");
    SendSpecial(TS_PING);
  }
  else
  {
    // defer next keepalive attempt
    FLastDataSent = Now();
  }
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
    {&ssh2_3des, &ssh2_des, &ssh2_aes, &ssh2_blowfish, &ssh2_arcfour};
  const TCipher TCiphers[] = {cip3DES, cipDES, cipAES, cipBlowfish, cipArcfour};
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
struct TClipboardHandler
{
  AnsiString Text;

  void __fastcall Copy(TObject * /*Sender*/)
  {
    CopyToClipboard(Text);
  }
};
//---------------------------------------------------------------------------
void __fastcall TSecureShell::VerifyHostKey(AnsiString Host, int Port,
  const AnsiString KeyType, AnsiString KeyStr, const AnsiString Fingerprint)
{
  GotHostKey();

  char Delimiter = ';';
  assert(KeyStr.Pos(Delimiter) == 0);

  if (FSessionData->Tunnel)
  {
    Host = FSessionData->OrigHostName;
    Port = FSessionData->OrigPortNumber;
  }

  FSessionInfo.HostKeyFingerprint = Fingerprint;

  bool Result = false;

  AnsiString Buf = FSessionData->HostKey;
  while (!Result && !Buf.IsEmpty())
  {
    AnsiString ExpectedKey = CutToChar(Buf, Delimiter, false);
    if (ExpectedKey == Fingerprint)
    {
      Result = true;
    }
  }

  AnsiString StoredKeys;
  if (!Result)
  {
    StoredKeys.SetLength(10240);
    if (retrieve_host_key(Host.c_str(), Port, KeyType.c_str(),
          StoredKeys.c_str(), StoredKeys.Length()) == 0)
    {
      PackStr(StoredKeys);
      AnsiString Buf = StoredKeys;
      while (!Result && !Buf.IsEmpty())
      {
        AnsiString StoredKey = CutToChar(Buf, Delimiter, false);
        if (StoredKey == KeyStr)
        {
          Result = true;
        }
      }
    }
    else
    {
      StoredKeys = "";
    }
  }

  if (!Result)
  {
    if (Configuration->DisableAcceptingHostKeys)
    {
      FatalError(LoadStr(KEY_NOT_VERIFIED));
    }
    else
    {
      TClipboardHandler ClipboardHandler;
      ClipboardHandler.Text = Fingerprint;

      bool Unknown = StoredKeys.IsEmpty();

      int Answers;
      int AliasesCount;
      TQueryButtonAlias Aliases[3];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
      Aliases[0].OnClick = &ClipboardHandler.Copy;
      Answers = qaYes | qaCancel | qaRetry;
      AliasesCount = 1;
      if (!Unknown)
      {
        Aliases[1].Button = qaYes;
        Aliases[1].Alias = LoadStr(UPDATE_KEY_BUTTON);
        Aliases[2].Button = qaOK;
        Aliases[2].Alias = LoadStr(ADD_KEY_BUTTON);
        AliasesCount += 2;
        Answers |= qaSkip | qaOK;
      }
      else
      {
        Answers |= qaNo;
      }

      TQueryParams Params;
      Params.NoBatchAnswers = qaYes | qaRetry | qaSkip | qaOK;
      Params.HelpKeyword = (Unknown ? HELP_UNKNOWN_KEY : HELP_DIFFERENT_KEY);
      Params.Aliases = Aliases;
      Params.AliasesCount = AliasesCount;
      int R = FUI->QueryUser(
        FMTLOAD((Unknown ? UNKNOWN_KEY2 : DIFFERENT_KEY3), (KeyType, Fingerprint)),
        NULL, Answers, &Params, qtWarning);

      switch (R) {
        case qaOK:
          assert(!Unknown);
          KeyStr = (StoredKeys + Delimiter + KeyStr);
          // fall thru
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
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::GetReady()
{
  return FOpened && (FWaiting == 0);
}
