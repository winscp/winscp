//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <stdio.h>

#include "PuttyIntf.h"
#include "Net.h"
#include "Interface.h"
#include "SecureShell.h"
#include "TextsCore.h"
#include "Common.h"
#include "ScpMain.h"

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define MAX_BUFSIZE 16384
const TColor LogLineColors[] =
  {clGreen, clRed, clMaroon, clBlue, clGray};
//---------------------------------------------------------------------------
__fastcall TSecureShell::TSecureShell()
{
  FSessionData = new TSessionData("");
  FActive = False;
  PendLen = 0;
  PendSize = 0;
  FStdErrorTemp = "";
  FBytesSent = 0;
  FBytesReceived = 0;
  FLog = new TSessionLog(this);
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnShowExtendedException = NULL;
  FOnUpdateStatus = NULL;
  FOnClose = NULL;
  FCSCipher = cipWarn; // = not detected yet
  FSCCipher = cipWarn;
  FReachedStatus = 0;
  UpdateStatus(sshClosed);
  FConfig = new Config();
  FSocket = new SOCKET;
  FMaxPacketSize = 0;
}
//---------------------------------------------------------------------------
__fastcall TSecureShell::~TSecureShell()
{
  if (FReachedStatus)
  {
    TCoreGuard Guard;

    SessionsCount--;
    if (SessionsCount == 0)
    {
      NetFinalize();
    }
  }

  ClearStdError();
  Active = false;
  SAFE_DESTROY(FSessionData);
  SAFE_DESTROY(FLog);
  delete FConfig;
  FConfig = NULL;
  UserObject = NULL;
  delete FSocket;
  FSocket = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Open()
{
  const char * InitError;
  char * RealHost;

  FPasswordTried = false;
  FPasswordTriedForKI = false;
  FReachedStatus = 0;

  {
    TCoreGuard Guard;

    SessionsCount++;
    if (SessionsCount == 1)
    {
      UpdateStatus(sshInitWinSock);
      NetInitialize();
    }
  }

  Log->AddStartupInfo();

  Active = false;
  FBackend = &ssh_backend;

  FAuthenticationLog = "";
  UpdateStatus(sshLookupHost);
  SessionData->StoreToConfig(FConfig);

  InitError = FBackend->init(this, &FBackendHandle, FConfig,
    SessionData->HostName.c_str(), SessionData->PortNumber, &RealHost, 0);
  if (InitError)
  {
    FatalError(InitError);
  }
  FRealHost = RealHost;
  UpdateStatus(sshConnect);
  /*FLoggingContext = log_init(this, (void *)FConfig);
  FBackend->provide_logctx(FBackendHandle, FLoggingContext);*/
  Init();

  CheckConnection(CONNECTION_FAILED);
  FLastDataSent = Now();
  FLoginTime = Now();

  Log->AddSeparator();
  UpdateStatus(sshAuthenticated);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Init()
{
  try
  {
    try
    {
      while (!FBackend->sendok(FBackendHandle))
      {
        WaitForData();
      }
    }
    catch(Exception & E)
    {
      if ((FReachedStatus == sshAuthenticate) && !FAuthenticationLog.IsEmpty())
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
    if (FReachedStatus == sshAuthenticate)
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
bool __fastcall TSecureShell::PromptUser(const AnsiString Prompt,
  AnsiString & Response, bool IsPassword)
{
  assert(IsPassword);

  bool Result;
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

    Result = DoPromptUser(FMTLOAD(PROMPT_KEY_PASSPHRASE, (Key)),
      pkPassphrase, Response);
  }
  else if (Prompt.Pos("'s password: "))
  {
    LogEvent(FORMAT("Session password prompt (%s)", (Prompt)));

    if (!SessionData->Password.IsEmpty() && !FPasswordTried)
    {
      LogEvent("Using stored password.");
      Result = true;
      Response = SessionData->Password;
    }
    else
    {
      LogEvent("Asking user for password.");
      Result = DoPromptUser(
        FMTLOAD(PROMPT_SESSION_PASSWORD, (SessionData->SessionName)),
        pkPassword, Response);
    }
    FPasswordTried = true;
  }
  else
  {
    // in other cases we assume TIS/Cryptocard/keyboard-interactive authentification prompt
    LogEvent(FORMAT("%s prompt from server (%s)",
      (IsPassword ? "Password" : "Normal", Prompt)));

    if (!SessionData->Password.IsEmpty() && IsPassword &&
        SessionData->AuthKIPassword && !FPasswordTriedForKI)
    {
      LogEvent("Responding with stored password.");
      Result = true;
      Response = SessionData->Password;
    }
    else
    {
      LogEvent("Asking user for response.");

      static const AnsiString ResponseSuffix("\r\nResponse: ");

      // Strip Cryptocard/TIS "Response" suffix
      AnsiString UserPrompt = Prompt;
      if (UserPrompt.SubString(UserPrompt.Length() - ResponseSuffix.Length() + 1,
            ResponseSuffix.Length()) == ResponseSuffix)
      {
        UserPrompt.SetLength(UserPrompt.Length() - ResponseSuffix.Length());
      }

      Result = DoPromptUser(UserPrompt, pkServerPrompt, Response);
    }
    FPasswordTriedForKI = true;
  };
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::DoPromptUser(AnsiString Prompt, TPromptKind Kind,
  AnsiString & Response)
{
  bool Result = false;
  if (OnPromptUser != NULL)
  {
    OnPromptUser(this, Prompt, Kind, Response, Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::GotHostKey()
{
  UpdateStatus(sshAuthenticate);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FromBackend(Boolean IsStdErr, char * Data, Integer Length)
{
  CheckConnection();

  // Following is taken from scp.c from_backend() and modified

  if (IsStdErr)
  {
    AddStdError(AnsiString(Data, Length));
  }
  else
  {
    unsigned char *p = (unsigned char *)Data;
    unsigned Len = (unsigned)Length;

    // If this is before the real session begins, raise exception.
    if (!OutPtr)
    {
      FatalError("Internal error: Session not yet begun.");
    }

    if ((OutLen > 0) && (Len > 0))
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
  }
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

    // I don't undestand this yet, but it works :-)
    while (OutLen > 0) WaitForData();

    // This seems ambiguous
    if (Len <= 0) FatalError(LoadStr(LOST_CONNECTION));
  };
  FBytesReceived += Len;
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
  Log->Add(llOutput, Line);
  return Line;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SendSpecial(int Code)
{
  LogEvent(FORMAT("Sending special code: %d", (Code)));
  CheckConnection();
  FBackend->special(FBackendHandle, (Telnet_Special)Code);
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
  FLastDataSent = Now();
  FBytesSent += Len;
  while (BufSize > MAX_BUFSIZE)
  {
    WaitForData();
    BufSize = FBackend->sendbuffer(FBackendHandle);
  }
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
  Log->Add(llInput, Line);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::AddStdError(AnsiString Str)
{
  StdError += Str;

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
    if (Status == sshAuthenticate)
    {
      FAuthenticationLog += (FAuthenticationLog.IsEmpty() ? "" : "\n") + Line;
    }
    Log->Add(llStdError, Line);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::ClearStdError()
{
  // Flush std error cache
  if (!FStdErrorTemp.IsEmpty())
  {
    if (Status == sshAuthenticate)
    {
      FAuthenticationLog +=
        (FAuthenticationLog.IsEmpty() ? "" : "\n") + FStdErrorTemp;
    }
    Log->Add(llStdError, FStdErrorTemp);
    FStdErrorTemp = "";
  }
  StdError = "";
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FatalError(Exception * E, AnsiString Msg)
{
  if (FActive)
  {
    // We log this instead of exception handler, because Close() would
    // probably cause exception handler to loose pointer to TShellLog()
    LogEvent("Attempt to close connection due to fatal exception:");
    Log->Add(llException, Msg);
    Log->AddException(E);
    Close();
  }
  SSH_FATAL_ERROR_EXT(E, Msg);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::FatalError(AnsiString Error)
{
  FatalError(NULL, Error);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetSocket(void * value)
{
  assert(value);
  if (FActive && (*static_cast<SOCKET*>(value) != INVALID_SOCKET))
    FatalError("Cannot set socket during connection");
  assert(FSocket);
  *static_cast<SOCKET*>(FSocket) = *static_cast<SOCKET*>(value);
  if (*static_cast<SOCKET*>(FSocket) != INVALID_SOCKET)
  {
    FActive = true;
  }
  else
  {
    Discard();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetSessionData(TSessionData * value)
{
  assert(!FActive);
  FSessionData->Assign(value);
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
bool __fastcall TSecureShell::GetActive() const
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Discard()
{
  bool WasActive = FActive;
  FActive = false;

  if (WasActive && OnClose)
  {
    OnClose(this);
  }
  
  FStatus = sshClosed;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Close()
{
  LogEvent("Closing connection.");
  CheckConnection();

  ssh_close(FBackendHandle);
  // This should be called insted, but there seem tu be error in freeing
  // FBackend->free(FBackendHandle);

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
extern int select_result(WPARAM, LPARAM);
void __fastcall TSecureShell::WaitForData()
{
  int R;
  do
  {
    CheckConnection();

    struct timeval time;
    fd_set readfds;
    FD_ZERO(&readfds);
    FD_SET(*static_cast<SOCKET*>(FSocket), &readfds);

    time.tv_sec = FSessionData->Timeout;
    time.tv_usec = 0;
    R = select(1, &readfds, NULL, NULL, &time);
    if (R < 0)
    {
      SSH_FATAL_ERROR(
        Format("Cannot determine status of socket (%d).", ARRAYOFCONST((R))));
    }
    else if (R == 0)
    {
      LogEvent("Waiting for data timed out, asking user what to do.");
      if (DoQueryUser(FMTLOAD(CONFIRM_PROLONG_TIMEOUT, (FSessionData->Timeout)),
            qaRetry | qaAbort, qpFatalAbort | qpAllowContinueOnError) != qaRetry)
      {
        FatalError(LoadStr(USER_TERMINATED));
      }
    }
  }
  while (R <= 0);

  select_result((WPARAM)(*static_cast<SOCKET*>(FSocket)), (LPARAM)FD_READ);
}
//---------------------------------------------------------------------------
bool __fastcall TSecureShell::SshFallbackCmd() const
{
  return ssh_fallback_cmd(FBackendHandle);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Error(const AnsiString Error) const
{
  SSH_ERROR(Error);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::Idle()
{
  // In Putty this is called each 100 ms when in foreground and
  // each 10 min when in background
  noise_regular();
  // Keep session alive
  if ((FSessionData->PingType != ptOff) &&
      (Now() - FLastDataSent > FSessionData->PingIntervalDT))
  {
    KeepAlive();
    // in case keep alive could not be processed, postpone next attempt
    FLastDataSent = Now();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::KeepAlive()
{
  LogEvent("Sending null packet to keep session alive.");
  SendSpecial(TS_PING);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetLog(TSessionLog * value)
{
  FLog->Assign(value);
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetConfiguration(TConfiguration *value)
{
  FConfiguration = value;
  Log->Configuration = value;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TSecureShell::GetDuration() const
{
  return Now() - FLoginTime;
}
//---------------------------------------------------------------------------
unsigned long __fastcall TSecureShell::MaxPacketSize()
{
  if (SshVersion == 1)
  {
    return 0;
  }
  else
  {
    if (FMaxPacketSize == 0)
    {
      unsigned long RemWindow = ssh2_remwindow(FBackendHandle);
      FMaxPacketSize = ssh2_remmaxpkt(FBackendHandle);
      if (RemWindow < FMaxPacketSize)
      {
        FMaxPacketSize = RemWindow;
      }
    }
    return FMaxPacketSize;
  }
}
//---------------------------------------------------------------------------
TCompressionType __fastcall TSecureShell::FuncToCompression(
  const void * Compress) const
{
  if (SshVersion == 1)
  {
    return get_ssh1_compressing(FBackendHandle) ? ctZLib : ctNone;
  }
  else
  {
    return (ssh_compress *)Compress == &ssh_zlib ? ctZLib : ctNone;
  }
}
//---------------------------------------------------------------------------
TCompressionType __fastcall TSecureShell::GetCSCompression() const
{
  assert(Active);
  return FuncToCompression(get_cscomp(FBackendHandle));
}
//---------------------------------------------------------------------------
TCompressionType __fastcall TSecureShell::GetSCCompression() const
{
  assert(Active);
  return FuncToCompression(get_sccomp(FBackendHandle));
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::GetSshVersion() const
{
  assert(Active);
  return get_ssh_version(FBackendHandle);
}
//---------------------------------------------------------------------------
TCipher __fastcall TSecureShell::FuncToSsh1Cipher(const void * Cipher) const
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
TCipher __fastcall TSecureShell::FuncToSsh2Cipher(const void * Cipher) const
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
TCipher __fastcall TSecureShell::GetCSCipher() 
{
  assert(Active);

  if (FCSCipher == cipWarn)
  {
    if (SshVersion == 1)
    {
      FCSCipher = FuncToSsh1Cipher(get_cipher(FBackendHandle));
    }
    else
    {
      FCSCipher = FuncToSsh2Cipher(get_cscipher(FBackendHandle));
    }
  }
  return FCSCipher;
}
//---------------------------------------------------------------------------
TCipher __fastcall TSecureShell::GetSCCipher()
{
  CheckConnection();

  if (FSCCipher == cipWarn)
  {
    if (SshVersion == 1)
    {
      FSCCipher = FuncToSsh1Cipher(get_cipher(FBackendHandle));
    }
    else
    {
      FSCCipher = FuncToSsh2Cipher(get_sccipher(FBackendHandle));
    }
  }
  return FSCCipher;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::DoShowExtendedException(Exception * E)
{
  DoHandleExtendedException(E);
  if (OnShowExtendedException != NULL)
  {
    OnShowExtendedException(this, E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::DoHandleExtendedException(Exception * E)
{
  Log->AddException(E);
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::DoQueryUser(const AnsiString Query,
  TStrings * MoreMessages, int Answers, int Params, TQueryType Type)
{
  LogEvent(FORMAT("Asking user:\n%s (%s)", (Query, (MoreMessages ? MoreMessages->CommaText : AnsiString() ))));
  int Answer = qaCancel;
  if (FOnQueryUser)
  {
    FOnQueryUser(this, Query, MoreMessages, Answers, Params, Answer, Type);
  }
  return Answer;
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::DoQueryUser(const AnsiString Query,
  const AnsiString OtherMessage, int Answers, int Params)
{
  TStrings * MoreMessages = new TStringList();
  Integer Result;
  try {
    if (!OtherMessage.IsEmpty()) MoreMessages->Add(OtherMessage);
    Result = DoQueryUser(Query, MoreMessages, Answers, Params);
  } __finally {
    delete MoreMessages;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::DoQueryUser(const AnsiString Query,
  int Answers, int Params)
{
  return DoQueryUser(Query, "", Answers, Params);
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::DoQueryUser(const AnsiString Query,
  Exception * E, int Answers, int Params)
{
  int Result;
  TStrings * MoreMessages = new TStringList();
  try
  {
    if (E->InheritsFrom(__classid(ExtException)) &&
        ((ExtException*)E)->MoreMessages)
    {
      MoreMessages->Assign(((ExtException*)E)->MoreMessages);
    }
    if (!E->Message.IsEmpty() && !Query.IsEmpty()) MoreMessages->Insert(0, E->Message);
    Result = DoQueryUser(!Query.IsEmpty() ? Query : E->Message,
      MoreMessages->Count ? MoreMessages : NULL,
      Answers, Params);
  }
  __finally
  {
    delete MoreMessages;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::VerifyHostKey(const AnsiString Host, int Port,
  const AnsiString KeyType, const AnsiString KeyStr, const AnsiString Fingerprint)
{
  GotHostKey();

  int Result;

  // Verify the key against the registry.
  Result = verify_host_key(Host.c_str(), Port, KeyType.c_str(), KeyStr.c_str());
  
  if (Result != 0)
  {
    int R = DoQueryUser(
      FMTLOAD((Result == 1 ? UNKNOWN_KEY : DIFFERENT_KEY), (Fingerprint)),
      NULL, qaYes | qaNo | qaCancel, 0, qtWarning);

    switch (R) {
      case qaYes:
        store_host_key(Host.c_str(), Port, KeyType.c_str(), KeyStr.c_str());
        break;

      case qaCancel:
        SSH_FATAL_ERROR(LoadStr(KEY_NOT_VERIFIED));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::AskCipher(const AnsiString CipherName, int CipherType)
{
  static int CipherTypeRes[] = {CIPHER_TYPE_BOTH, CIPHER_TYPE_CS, CIPHER_TYPE_SC};
  assert(CipherType >= 0 && CipherType < LENOF(CipherTypeRes));
  AnsiString Msg = FMTLOAD(CIPHER_BELOW_TRESHOLD,
    (LoadStr(CipherTypeRes[CipherType]), CipherName));

  if (DoQueryUser(Msg, NULL, qaYes | qaNo, 0, qtWarning) == qaNo)
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::OldKeyfileWarning()
{
  DoQueryUser(LoadStr(OLD_KEY), NULL, qaOK, 0, qtWarning);
}
//---------------------------------------------------------------------------
int __fastcall TSecureShell::GetStatus() const
{
  return FStatus;
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::UpdateStatus(int Value)
{
  if (FStatus != Value)
  {
    FStatus = Value;
    if (FStatus > FReachedStatus) FReachedStatus = FStatus;
    if (FOnUpdateStatus) FOnUpdateStatus(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecureShell::SetUserObject(TObject * value)
{
  if (UserObject != value)
  {
    if (UserObject)
    {
      delete UserObject;
    }
    FUserObject = value;
  }
}
//=== TSessionLog -----------------------------------------------------------
const char *LogLineMarks = "<>!.*";
__fastcall TSessionLog::TSessionLog(TSecureShell * AOwner): TStringList()
{
  FEnabled = true;
  FOwner = AOwner;
  FFile = NULL;
  FLoggedLines = 0;
  FTopIndex = -1;
  FFileName = "";
}
//---------------------------------------------------------------------------
__fastcall TSessionLog::~TSessionLog()
{
  CloseLogFile();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionLog::GetSessionName()
{
  assert(FOwner != NULL);
  assert(FOwner->SessionData != NULL);
  return FOwner->SessionData->SessionName;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetLine(Integer Index, AnsiString value)
{
  Strings[Index] = AnsiString(Strings[Index - FTopIndex][1]) + value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionLog::GetLine(Integer Index)
{
  return Strings[Index - FTopIndex].SubString(2, Strings[Index - FTopIndex].Length() - 1);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetType(Integer Index, TLogLineType value)
{
  Strings[Index - FTopIndex][1] = LogLineMarks[value];
}
//---------------------------------------------------------------------------
TLogLineType __fastcall TSessionLog::GetType(Integer Index)
{
  int P = AnsiString(LogLineMarks).Pos(Strings[Index - FTopIndex][1]);
  if (P)
  {
    return TLogLineType(P - 1);
  }
  else
  {
    assert(false);
    return (TLogLineType)0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Add(TLogLineType aType, AnsiString aLine)
{
  assert(Configuration);
  if (IsLogging())
  {
    try
    {
      if (Configuration->Logging)
      {
        BeginUpdate();
      }

      try
      {
        while (!aLine.IsEmpty())
        {
          AnsiString NewStr;
          NewStr = AnsiString(LogLineMarks[aType]) + CutToChar(aLine, '\n', False);
          if (Configuration->Logging)
          {
            TStringList::Add(NewStr);
            FLoggedLines++;
          }
          NewStr.Insert(' ', 2);
          DoAddLine(NewStr);
          if (Configuration->Logging && Configuration->LogToFile)
          {
            if (!FFile) OpenLogFile();
            if (FFile) fprintf((FILE *)FFile, "%s\n", NewStr.c_str());
          }
        }
        if (Configuration->Logging)
        {
          if (FTopIndex < 0) FTopIndex = 0;
          DeleteUnnecessary();
        }
      }
      __finally
      {
        if (Configuration->Logging)
        {
          EndUpdate();
        }
      }
    }
    catch (Exception &E)
    {
      // We failed logging, turn it of and notify user.
      Configuration->Logging = False;
      try
      {
        throw ExtException(&E, LOG_ERROR);
      }
      catch (Exception &E)
      {
        FOwner->DoShowExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddException(Exception * E)
{
  if (E)
  {
    Add(llException, ExceptionLogString(E));
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetConfiguration(TConfiguration * value)
{
  if (FConfiguration != value)
  {
    FConfiguration = value;
    ReflectSettings();
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::ReflectSettings()
{
  // if logging to file was turned off or log file was change -> close current log file
  if (FFile && (!LogToFile || (FFileName != LogFileName))) CloseLogFile();
  // if loggin to file is enabled and we don't log -> open log file
  if (!FFile && LogToFile) OpenLogFile();
  DeleteUnnecessary();
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::SetEnabled(bool value)
{
  if (FEnabled != value)
  {
    FEnabled = value;
    ReflectSettings();
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TSessionLog::GetLogToFile()
{
  return Enabled && (Configuration != NULL) && Configuration->Logging &&
    Configuration->LogToFile;
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::CloseLogFile()
{
  if (FFile)
  {
    fclose((FILE *)FFile);
    FFile = NULL;
  }
  FFileName = "";
}
//---------------------------------------------------------------------------
void TSessionLog::OpenLogFile()
{
  if (LogToFile)
  {
    try
    {
      assert(!FFile);
      assert(Configuration);
      FFile = fopen(LogFileName.c_str(), (Configuration->LogFileAppend ? "a" : "w"));
      if (FFile)
      {
        setvbuf((FILE *)FFile, NULL, _IONBF, BUFSIZ);
        FFileName = LogFileName;
      }
      else
      {
        throw Exception(FMTLOAD(LOG_OPENERROR, (LogFileName)));
      }
    }
    catch (Exception &E)
    {
      // We failed logging to file, turn it of and notify user.
      FFileName = "";
      Configuration->LogToFile = false;
      try
      {
        throw ExtException(&E, LOG_ERROR);
      }
      catch (Exception &E)
      {
        FOwner->DoShowExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
void TSessionLog::DeleteUnnecessary()
{
  BeginUpdate();
  try
  {
    if (!Configuration || !Configuration->Logging)
    {
      Clear();
    }
    else
    {
      while (!Configuration->LogWindowComplete && (Count > Configuration->LogWindowLines))
      {
        Delete(0);
        FTopIndex++;
      }
    }
  }
  __finally
  {
    EndUpdate();
  }
}
//---------------------------------------------------------------------------
TColor __fastcall TSessionLog::GetColor(int Index)
{
  return LogLineColors[Type[Index]];
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::DoAddLine(const AnsiString AddedLine)
{
  if (FOnAddLine)
  {
    FOnAddLine(this, AddedLine);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddStartupInfo()
{
  assert(FOwner != NULL);
  TSessionData * Data = FOwner->SessionData;
  assert(Data != NULL);
  assert(Configuration);
  if (Configuration->Logging || FOnAddLine)
  {
    BeginUpdate();
    try
    {
      #define ADF(S, F) Add(llMessage, FORMAT(S, F));
      AddSeparator();
      ADF("WinSCP %s (OS %s)", (Configuration->VersionStr, Configuration->OSVersionStr));
      ADF("Login time: %s", (FormatDateTime("dddddd tt", Now())));
      AddSeparator();
      ADF("Session name: %s", (Data->SessionName));
      ADF("Host name: %s (Port: %d)", (Data->HostName, Data->PortNumber));
      ADF("User name: %s (Password: %s, Key file: %s)",
        (Data->UserName, BooleanToEngStr(!Data->Password.IsEmpty()),
         BooleanToEngStr(!Data->PublicKeyFile.IsEmpty())))
      ADF("Transfer Protocol: %s", (Data->FSProtocolStr));
      ADF("SSH protocol version: %s; Compression: %s",
        (Data->SshProtStr, BooleanToEngStr(Data->Compression)));
      ADF("Agent forwarding: %s; TIS/CryptoCard: %s; KI: %s",
        (BooleanToEngStr(Data->AgentFwd), BooleanToEngStr(Data->AuthTIS),
         BooleanToEngStr(Data->AuthKI)));
      ADF("Ciphers: %s; Ssh2DES: %s",
        (Data->CipherList, BooleanToEngStr(Data->Ssh2DES)));
      char * PingTypes = "-NC";
      ADF("Ping type: %s, Ping interval: %d sec; Timeout: %d sec",
        (AnsiString(PingTypes[Data->PingType]), Data->PingInterval, Data->Timeout));
      AnsiString Bugs;
      char const * BugFlags = "A+-";
      for (int Index = 0; Index < BUG_COUNT; Index++)
      {
        Bugs += AnsiString(BugFlags[Data->Bug[(TSshBug)Index]])+(Index<BUG_COUNT?",":"");
      }
      ADF("SSH Bugs: %s", (Bugs));
      ADF("Proxy: %s", (ProxyMethodList[Data->ProxyMethod]));
      if (Data->ProxyMethod != pmNone)
      {
        ADF("HostName: %s (Port: %d); Username: %s; Passwd: %s",
          (Data->ProxyHost, Data->ProxyPort,
           Data->ProxyUsername, BooleanToEngStr(!Data->ProxyPassword.IsEmpty())));
        if (Data->ProxyMethod == pmTelnet)
        {
          ADF("Telnet command: %s", (Data->ProxyTelnetCommand));
        }
      }
      ADF("Return code variable: %s; Lookup user groups: %s",
        ((Data->DetectReturnVar ? AnsiString("Autodetect") : Data->ReturnVar),
         BooleanToEngStr(Data->LookupUserGroups)));
      ADF("Shell: %s, EOL: %d", ((Data->Shell.IsEmpty()? AnsiString("default") : Data->Shell), Data->EOLType));
      ADF("Local directory: %s, Remote directory: %s, Update: %s, Cache: %s",
        ((Data->LocalDirectory.IsEmpty() ? AnsiString("default") : Data->LocalDirectory),
         (Data->RemoteDirectory.IsEmpty() ? AnsiString("home") : Data->RemoteDirectory),
         BooleanToEngStr(Data->UpdateDirectories),
         BooleanToEngStr(Data->CacheDirectories)));
      ADF("Cache directory changes: %s, Permanent: %s",
        (BooleanToEngStr(Data->CacheDirectoryChanges),
         BooleanToEngStr(Data->PreserveDirectoryChanges)));
      ADF("Clear aliases: %s, Unset nat.vars: %s, Resolve symlinks: %s",
        (BooleanToEngStr(Data->ClearAliases), BooleanToEngStr(Data->UnsetNationalVars),
         BooleanToEngStr(Data->ResolveSymlinks)));
      ADF("Alias LS: %s, Ign LS warn: %s, Scp1 Comp: %s",
        (BooleanToEngStr(Data->AliasGroupList),
         BooleanToEngStr(Data->IgnoreLsWarnings), 
         BooleanToEngStr(Data->Scp1Compatibility)));

      AddSeparator();

      #undef ADF
    }
    __finally
    {
      EndUpdate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::AddSeparator()
{
  Add(llMessage, "--------------------------------------------------------------------------");
}
//---------------------------------------------------------------------------
int __fastcall TSessionLog::GetIndexes(int Index)
{
  assert((Index >= 0) && (Index < Count));
  int Result = TopIndex + Index;
  assert((Result >= 0) && (Result < LoggedLines));
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TSessionLog::GetBottomIndex()
{
  return (Count ? Indexes[Count-1] : -1);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TSessionLog::GetLogFileName()
{
  assert(Configuration);
  AnsiString Result = StripPathQuotes(Configuration->LogFileName);
  return Result;
}
//---------------------------------------------------------------------------
Boolean __fastcall TSessionLog::GetLoggingToFile()
{
  return
    (Configuration && Configuration->Logging && Configuration->LogToFile && FFile);
}
//---------------------------------------------------------------------------
void __fastcall TSessionLog::Clear()
{
  FTopIndex += Count;
  TStringList::Clear();
}





