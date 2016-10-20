//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Terminal.h"

#include <SysUtils.hpp>
#include <FileCtrl.hpp>

#include "Common.h"
#include "PuttyTools.h"
#include "FileBuffer.h"
#include "Interface.h"
#include "RemoteFiles.h"
#include "SecureShell.h"
#include "ScpFileSystem.h"
#include "SftpFileSystem.h"
#ifndef NO_FILEZILLA
#include "FtpFileSystem.h"
#endif
#include "WebDAVFileSystem.h"
#include "TextsCore.h"
#include "HelpCore.h"
#include "CoreMain.h"
#include "Queue.h"
#include <openssl/pkcs12.h>
#include <openssl/err.h>

#ifndef AUTO_WINSOCK
#include <winsock2.h>
#endif
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_TERMINAL this
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TLoopDetector
{
public:
  __fastcall TLoopDetector();
  void __fastcall RecordVisitedDirectory(const UnicodeString & Directory);
  bool __fastcall IsUnvisitedDirectory(const UnicodeString & Directory);

private:
  std::unique_ptr<TStringList> FVisitedDirectories;
};
//---------------------------------------------------------------------------
__fastcall TLoopDetector::TLoopDetector()
{
  FVisitedDirectories.reset(CreateSortedStringList());
}
//---------------------------------------------------------------------------
void __fastcall TLoopDetector::RecordVisitedDirectory(const UnicodeString & Directory)
{
  UnicodeString VisitedDirectory = ExcludeTrailingBackslash(Directory);
  FVisitedDirectories->Add(VisitedDirectory);
}
//---------------------------------------------------------------------------
bool __fastcall TLoopDetector::IsUnvisitedDirectory(const UnicodeString & Directory)
{
  bool Result = (FVisitedDirectories->IndexOf(Directory) < 0);

  if (Result)
  {
    RecordVisitedDirectory(Directory);
  }

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
struct TMoveFileParams
{
  UnicodeString Target;
  UnicodeString FileMask;
};
//---------------------------------------------------------------------------
struct TFilesFindParams
{
  TFileMasks FileMask;
  TFileFoundEvent OnFileFound;
  TFindingFileEvent OnFindingFile;
  bool Cancel;
  TLoopDetector LoopDetector;
  UnicodeString RealDirectory;
};
//---------------------------------------------------------------------------
TCalculateSizeStats::TCalculateSizeStats()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TSynchronizeOptions::TSynchronizeOptions()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TSynchronizeOptions::~TSynchronizeOptions()
{
  delete Filter;
}
//---------------------------------------------------------------------------
bool __fastcall TSynchronizeOptions::MatchesFilter(const UnicodeString & FileName)
{
  int FoundIndex;
  bool Result;
  if (Filter == NULL)
  {
    Result = true;
  }
  else
  {
    Result = Filter->Find(FileName, FoundIndex);
  }
  return Result;
}
//---------------------------------------------------------------------------
TSpaceAvailable::TSpaceAvailable()
{
  memset(this, 0, sizeof(*this));
}
//---------------------------------------------------------------------------
TOverwriteFileParams::TOverwriteFileParams()
{
  SourceSize = 0;
  DestSize = 0;
  SourcePrecision = mfFull;
  DestPrecision = mfFull;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSynchronizeChecklist::TItem::TItem() :
  Action(saNone), IsDirectory(false), RemoteFile(NULL), Checked(true), ImageIndex(-1)
{
  Local.ModificationFmt = mfFull;
  Local.Modification = 0;
  Local.Size = 0;
  Remote.ModificationFmt = mfFull;
  Remote.Modification = 0;
  Remote.Size = 0;
}
//---------------------------------------------------------------------------
TSynchronizeChecklist::TItem::~TItem()
{
  delete RemoteFile;
}
//---------------------------------------------------------------------------
const UnicodeString& TSynchronizeChecklist::TItem::GetFileName() const
{
  if (!Remote.FileName.IsEmpty())
  {
    return Remote.FileName;
  }
  else
  {
    DebugAssert(!Local.FileName.IsEmpty());
    return Local.FileName;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSynchronizeChecklist::TSynchronizeChecklist() :
  FList(new TList())
{
}
//---------------------------------------------------------------------------
TSynchronizeChecklist::~TSynchronizeChecklist()
{
  for (int Index = 0; Index < FList->Count; Index++)
  {
    delete static_cast<TItem *>(FList->Items[Index]);
  }
  delete FList;
}
//---------------------------------------------------------------------------
void TSynchronizeChecklist::Add(TItem * Item)
{
  FList->Add(Item);
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeChecklist::Compare(void * AItem1, void * AItem2)
{
  TItem * Item1 = static_cast<TItem *>(AItem1);
  TItem * Item2 = static_cast<TItem *>(AItem2);

  int Result;
  if (!Item1->Local.Directory.IsEmpty())
  {
    Result = AnsiCompareText(Item1->Local.Directory, Item2->Local.Directory);
  }
  else
  {
    DebugAssert(!Item1->Remote.Directory.IsEmpty());
    Result = AnsiCompareText(Item1->Remote.Directory, Item2->Remote.Directory);
  }

  if (Result == 0)
  {
    Result = AnsiCompareText(Item1->GetFileName(), Item2->GetFileName());
  }

  return Result;
}
//---------------------------------------------------------------------------
void TSynchronizeChecklist::Sort()
{
  FList->Sort(Compare);
}
//---------------------------------------------------------------------------
int TSynchronizeChecklist::GetCount() const
{
  return FList->Count;
}
//---------------------------------------------------------------------------
const TSynchronizeChecklist::TItem * TSynchronizeChecklist::GetItem(int Index) const
{
  return static_cast<TItem *>(FList->Items[Index]);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeChecklist::Update(const TItem * Item, bool Check, TAction Action)
{
  // TSynchronizeChecklist owns non-const items so it can manipulate them freely,
  // const_cast here is just an optimization
  TItem * MutableItem = const_cast<TItem *>(Item);
  DebugAssert(FList->IndexOf(MutableItem) >= 0);
  MutableItem->Checked = Check;
  MutableItem->Action = Action;
}
//---------------------------------------------------------------------------
TSynchronizeChecklist::TAction __fastcall TSynchronizeChecklist::Reverse(TSynchronizeChecklist::TAction Action)
{
  switch (Action)
  {
    case saUploadNew:
      return saDeleteLocal;

    case saDownloadNew:
      return saDeleteRemote;

    case saUploadUpdate:
      return saDownloadUpdate;

    case saDownloadUpdate:
      return saUploadUpdate;

    case saDeleteRemote:
      return saDownloadNew;

    case saDeleteLocal:
      return saUploadNew;

    default:
    case saNone:
      DebugFail();
      return saNone;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTunnelThread : public TSimpleThread
{
public:
  __fastcall TTunnelThread(TSecureShell * SecureShell);
  virtual __fastcall ~TTunnelThread();

  virtual void __fastcall Terminate();

protected:
  virtual void __fastcall Execute();

private:
  TSecureShell * FSecureShell;
  bool FTerminated;
};
//---------------------------------------------------------------------------
__fastcall TTunnelThread::TTunnelThread(TSecureShell * SecureShell) :
  FSecureShell(SecureShell),
  FTerminated(false)
{
  Start();
}
//---------------------------------------------------------------------------
__fastcall TTunnelThread::~TTunnelThread()
{
  // close before the class's virtual functions (Terminate particularly) are lost
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TTunnelThread::Terminate()
{
  FTerminated = true;
}
//---------------------------------------------------------------------------
void __fastcall TTunnelThread::Execute()
{
  try
  {
    while (!FTerminated)
    {
      FSecureShell->Idle(250);
    }
  }
  catch(...)
  {
    if (FSecureShell->Active)
    {
      FSecureShell->Close();
    }
    // do not pass exception out of thread's proc
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTunnelUI : public TSessionUI
{
public:
  __fastcall TTunnelUI(TTerminal * Terminal);
  virtual void __fastcall Information(const UnicodeString & Str, bool Status);
  virtual unsigned int __fastcall QueryUser(const UnicodeString Query,
    TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual unsigned int __fastcall QueryUserException(const UnicodeString Query,
    Exception * E, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
    TStrings * Results);
  virtual void __fastcall DisplayBanner(const UnicodeString & Banner);
  virtual void __fastcall FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpContext);
  virtual void __fastcall HandleExtendedException(Exception * E);
  virtual void __fastcall Closed();
  virtual void __fastcall ProcessGUI();

private:
  TTerminal * FTerminal;
  unsigned int FTerminalThread;
};
//---------------------------------------------------------------------------
__fastcall TTunnelUI::TTunnelUI(TTerminal * Terminal)
{
  FTerminal = Terminal;
  FTerminalThread = GetCurrentThreadId();
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::Information(const UnicodeString & Str, bool Status)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->Information(Str, Status);
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTunnelUI::QueryUser(const UnicodeString Query,
  TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    Result = FTerminal->QueryUser(Query, MoreMessages, Answers, Params, QueryType);
  }
  else
  {
    Result = AbortAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTunnelUI::QueryUserException(const UnicodeString Query,
  Exception * E, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    Result = FTerminal->QueryUserException(Query, E, Answers, Params, QueryType);
  }
  else
  {
    Result = AbortAnswer(Answers);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTunnelUI::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  bool Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    if (IsAuthenticationPrompt(Kind))
    {
      Instructions = LoadStr(TUNNEL_INSTRUCTION) +
        (Instructions.IsEmpty() ? L"" : L"\n") +
        Instructions;
    }

    Result = FTerminal->PromptUser(Data, Kind, Name, Instructions, Prompts, Results);
  }
  else
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::DisplayBanner(const UnicodeString & Banner)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->DisplayBanner(Banner);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  throw ESshFatal(E, Msg, HelpKeyword);
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::HandleExtendedException(Exception * E)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->HandleExtendedException(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::Closed()
{
  // noop
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::ProcessGUI()
{
  // noop
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TCallbackGuard
{
public:
  inline __fastcall TCallbackGuard(TTerminal * FTerminal);
  inline __fastcall ~TCallbackGuard();

  void __fastcall FatalError(Exception * E, const UnicodeString & Msg, const UnicodeString & HelpKeyword);
  inline void __fastcall Verify();
  void __fastcall Dismiss();

private:
  ExtException * FFatalError;
  TTerminal * FTerminal;
  bool FGuarding;
};
//---------------------------------------------------------------------------
__fastcall TCallbackGuard::TCallbackGuard(TTerminal * Terminal) :
  FTerminal(Terminal),
  FFatalError(NULL),
  FGuarding(FTerminal->FCallbackGuard == NULL)
{
  if (FGuarding)
  {
    FTerminal->FCallbackGuard = this;
  }
}
//---------------------------------------------------------------------------
__fastcall TCallbackGuard::~TCallbackGuard()
{
  if (FGuarding)
  {
    DebugAssert((FTerminal->FCallbackGuard == this) || (FTerminal->FCallbackGuard == NULL));
    FTerminal->FCallbackGuard = NULL;
  }

  delete FFatalError;
}
//---------------------------------------------------------------------------
void __fastcall TCallbackGuard::FatalError(Exception * E, const UnicodeString & Msg, const UnicodeString & HelpKeyword)
{
  DebugAssert(FGuarding);

  // make sure we do not bother about getting back the silent abort exception
  // we issued ourselves. this may happen when there is an exception handler
  // that converts any exception to fatal one (such as in TTerminal::Open).
  if (dynamic_cast<ECallbackGuardAbort *>(E) == NULL)
  {
    delete FFatalError;
    FFatalError = new ExtException(E, Msg, HelpKeyword);
  }

  // silently abort what we are doing.
  // non-silent exception would be caught probably by default application
  // exception handler, which may not do an appropriate action
  // (particularly it will not resume broken transfer).
  throw ECallbackGuardAbort();
}
//---------------------------------------------------------------------------
void __fastcall TCallbackGuard::Dismiss()
{
  DebugAssert(FFatalError == NULL);
  FGuarding = false;
}
//---------------------------------------------------------------------------
void __fastcall TCallbackGuard::Verify()
{
  if (FGuarding)
  {
    FGuarding = false;
    DebugAssert(FTerminal->FCallbackGuard == this);
    FTerminal->FCallbackGuard = NULL;

    if (FFatalError != NULL)
    {
      throw ESshFatal(FFatalError, L"");
    }
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TRobustOperationLoop::TRobustOperationLoop(TTerminal * Terminal, TFileOperationProgressType * OperationProgress) :
  FTerminal(Terminal),
  FOperationProgress(OperationProgress),
  FRetry(false)
{
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::TryReopen(Exception & E)
{
  FRetry =
    !FTerminal->Active &&
    FTerminal->QueryReopen(&E, ropNoReadDirectory, FOperationProgress);
  return FRetry;
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::ShouldRetry()
{
  return FRetry;
}
//---------------------------------------------------------------------------
bool TRobustOperationLoop::Retry()
{
  bool Result = FRetry;
  FRetry = false;
  return Result;
}
//---------------------------------------------------------------------------
class TRetryOperationLoop
{
public:
  TRetryOperationLoop(TTerminal * Terminal);

  void Error(Exception & E);
  void Error(Exception & E, TSessionAction & Action);
  void Error(Exception & E, const UnicodeString & Message);
  void Error(Exception & E, TSessionAction & Action, const UnicodeString & Message);
  bool Retry();

private:
  TTerminal * FTerminal;
  bool FRetry;

  void DoError(Exception & E, TSessionAction * Action, const UnicodeString & Message);
};
//---------------------------------------------------------------------------
TRetryOperationLoop::TRetryOperationLoop(TTerminal * Terminal)
{
  FTerminal = Terminal;
  FRetry = false;
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::DoError(Exception & E, TSessionAction * Action, const UnicodeString & Message)
{
  // Note that the action may already be canceled when RollbackAction is called
  unsigned int Result;
  try
  {
    Result = FTerminal->CommandError(&E, Message, qaRetry | qaSkip | qaAbort);
  }
  catch (Exception & E2)
  {
    if (Action != NULL)
    {
      FTerminal->RollbackAction(*Action, NULL, &E2);
    }
    throw;
  }

  switch (Result)
  {
    case qaRetry:
      FRetry = true;
      if (Action != NULL)
      {
        Action->Cancel();
      }
      break;

    case qaAbort:
      if (Action != NULL)
      {
        FTerminal->RollbackAction(*Action, NULL, &E);
      }
      Abort();
      break;

    case qaSkip:
      if (Action != NULL)
      {
        Action->Cancel();
      }
      break;

    default:
      DebugFail();
      break;
  }
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E)
{
  DoError(E, NULL, UnicodeString());
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, TSessionAction & Action)
{
  DoError(E, &Action, UnicodeString());
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, const UnicodeString & Message)
{
  DoError(E, NULL, Message);
}
//---------------------------------------------------------------------------
void TRetryOperationLoop::Error(Exception & E, TSessionAction & Action, const UnicodeString & Message)
{
  DoError(E, &Action, Message);
}
//---------------------------------------------------------------------------
bool TRetryOperationLoop::Retry()
{
  bool Result = FRetry;
  FRetry = false;
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TTerminal::TTerminal(TSessionData * SessionData,
  TConfiguration * Configuration)
{
  FConfiguration = Configuration;
  FSessionData = new TSessionData(L"");
  FSessionData->Assign(SessionData);
  TDateTime Started = Now(); // use the same time for session and XML log
  FLog = new TSessionLog(this, Started, FSessionData, Configuration);
  FActionLog = new TActionLog(this, Started, FSessionData, Configuration);
  FFiles = new TRemoteDirectory(this);
  FExceptionOnFail = 0;
  FInTransaction = 0;
  FReadCurrentDirectoryPending = false;
  FReadDirectoryPending = false;
  FUsersGroupsLookedup = False;
  FTunnelLocalPortNumber = 0;
  FFileSystem = NULL;
  FSecureShell = NULL;
  FOnProgress = NULL;
  FOnFinished = NULL;
  FOnDeleteLocalFile = NULL;
  FOnReadDirectoryProgress = NULL;
  FOnQueryUser = NULL;
  FOnPromptUser = NULL;
  FOnDisplayBanner = NULL;
  FOnShowExtendedException = NULL;
  FOnInformation = NULL;
  FOnCustomCommand = NULL;
  FOnClose = NULL;
  FOnFindingFile = NULL;

  FUseBusyCursor = True;
  FLockDirectory = L"";
  FDirectoryCache = new TRemoteDirectoryCache();
  FDirectoryChangesCache = NULL;
  FFSProtocol = cfsUnknown;
  FCommandSession = NULL;
  FAutoReadDirectory = true;
  FReadingCurrentDirectory = false;
  FStatus = ssClosed;
  FOpening = 0;
  FTunnelThread = NULL;
  FTunnel = NULL;
  FTunnelData = NULL;
  FTunnelLog = NULL;
  FTunnelUI = NULL;
  FTunnelOpening = false;
  FCallbackGuard = NULL;
  FNesting = 0;
}
//---------------------------------------------------------------------------
__fastcall TTerminal::~TTerminal()
{
  if (Active)
  {
    Close();
  }

  if (FCallbackGuard != NULL)
  {
    // see TTerminal::HandleExtendedException
    FCallbackGuard->Dismiss();
  }
  DebugAssert(FTunnel == NULL);

  SAFE_DESTROY(FCommandSession);

  if (SessionData->CacheDirectoryChanges && SessionData->PreserveDirectoryChanges &&
      (FDirectoryChangesCache != NULL))
  {
    Configuration->SaveDirectoryChangesCache(SessionData->SessionKey,
      FDirectoryChangesCache);
  }

  SAFE_DESTROY_EX(TCustomFileSystem, FFileSystem);
  SAFE_DESTROY_EX(TSessionLog, FLog);
  SAFE_DESTROY_EX(TActionLog, FActionLog);
  delete FFiles;
  delete FDirectoryCache;
  delete FDirectoryChangesCache;
  SAFE_DESTROY(FSessionData);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Idle()
{
  // Once we disconnect, do nothing, until reconnect handler
  // "receives the information".
  // Never go idle when called from within ::ProcessGUI() call
  // as we may recurse for good, timeouting eventually.
  if (Active && (FNesting == 0))
  {
    TAutoNestingCounter NestingCounter(FNesting);

    if (Configuration->ActualLogProtocol >= 1)
    {
      LogEvent(L"Session upkeep");
    }

    DebugAssert(FFileSystem != NULL);
    FFileSystem->Idle();

    if (CommandSessionOpened)
    {
      try
      {
        FCommandSession->Idle();
      }
      catch(Exception & E)
      {
        // If the secondary session is dropped, ignore the error and let
        // it be reconnected when needed.
        // BTW, non-fatal error can hardly happen here, that's why
        // it is displayed, because it can be useful to know.
        if (FCommandSession->Active)
        {
          FCommandSession->HandleExtendedException(&E);
        }
      }
    }
  }
}
//---------------------------------------------------------------------
RawByteString __fastcall TTerminal::EncryptPassword(const UnicodeString & Password)
{
  return Configuration->EncryptPassword(Password, SessionData->SessionName);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::DecryptPassword(const RawByteString & Password)
{
  UnicodeString Result;
  try
  {
    Result = Configuration->DecryptPassword(Password, SessionData->SessionName);
  }
  catch(EAbort &)
  {
    // silently ignore aborted prompts for master password and return empty password
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RecryptPasswords()
{
  FSessionData->RecryptPasswords();
  FRememberedPassword = EncryptPassword(DecryptPassword(FRememberedPassword));
  FRememberedTunnelPassword = EncryptPassword(DecryptPassword(FRememberedTunnelPassword));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::ExpandFileName(UnicodeString Path,
  const UnicodeString BasePath)
{
  // replace this by AbsolutePath()
  Path = UnixExcludeTrailingBackslash(Path);
  if (!UnixIsAbsolutePath(Path) && !BasePath.IsEmpty())
  {
    // TODO: Handle more complicated cases like "../../xxx"
    if (Path == L"..")
    {
      Path = UnixExcludeTrailingBackslash(UnixExtractFilePath(
        UnixExcludeTrailingBackslash(BasePath)));
    }
    else
    {
      Path = UnixIncludeTrailingBackslash(BasePath) + Path;
    }
  }
  return Path;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetActive()
{
  return (FFileSystem != NULL) && FFileSystem->GetActive();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Close()
{
  FFileSystem->Close();

  // Cannot rely on CommandSessionOpened here as Status is set to ssClosed
  // only after the OnClose is called
  if ((FCommandSession != NULL) && FCommandSession->Active)
  {
    // prevent recursion
    FCommandSession->OnClose = NULL;
    FCommandSession->Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ResetConnection()
{
  // used to be called from Reopen(), why?
  FTunnelError = L"";

  FRememberedPasswordTried = false;
  // Particularly to prevent reusing a wrong client certificate passphrase
  // from a previous login attempt
  FRememberedPassword = UnicodeString();
  FRememberedTunnelPasswordTried = false;
  FRememberedTunnelPassword = UnicodeString();

  if (FDirectoryChangesCache != NULL)
  {
    delete FDirectoryChangesCache;
    FDirectoryChangesCache = NULL;
  }

  FFiles->Directory = L"";
  // note that we cannot clear contained files
  // as they can still be referenced in the GUI atm
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::FingerprintScan()
{
  SessionData->FingerprintScan = true;
  try
  {
    Open();
    // we should never get here
    Abort();
  }
  catch (...)
  {
    if (!FFingerprintScanned.IsEmpty())
    {
      return FFingerprintScanned;
    }
    throw;
  }
  DebugFail();
  return UnicodeString();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Open()
{
  TAutoNestingCounter OpeningCounter(FOpening);
  ReflectSettings();
  try
  {
    DoInformation(L"", true, 1);
    try
    {
      try
      {
        ResetConnection();
        FStatus = ssOpening;

        try
        {
          if (FFileSystem == NULL)
          {
            Log->AddSystemInfo();
            DoInitializeLog();
            Log->AddStartupInfo();
          }

          DebugAssert(FTunnel == NULL);
          if (FSessionData->Tunnel)
          {
            DoInformation(LoadStr(OPEN_TUNNEL), true);
            LogEvent(L"Opening tunnel.");
            OpenTunnel();
            Log->AddSeparator();

            FSessionData->ConfigureTunnel(FTunnelLocalPortNumber);

            DoInformation(LoadStr(USING_TUNNEL), false);
            LogEvent(FORMAT(L"Connecting via tunnel interface %s:%d.",
              (FSessionData->HostNameExpanded, FSessionData->PortNumber)));
          }
          else
          {
            DebugAssert(FTunnelLocalPortNumber == 0);
          }

          if (FFileSystem == NULL)
          {
            if (SessionData->FSProtocol == fsFTP)
            {
              #ifdef NO_FILEZILLA
              LogEvent(L"FTP protocol is not supported by this build.");
              FatalError(NULL, LoadStr(FTP_UNSUPPORTED));
              #else
              FFSProtocol = cfsFTP;
              FFileSystem = new TFTPFileSystem(this);
              FFileSystem->Open();
              Log->AddSeparator();
              LogEvent(L"Using FTP protocol.");
              #endif
            }
            else if (SessionData->FSProtocol == fsWebDAV)
            {
              FFSProtocol = cfsWebDAV;
              FFileSystem = new TWebDAVFileSystem(this);
              FFileSystem->Open();
              Log->AddSeparator();
              LogEvent(L"Using WebDAV protocol.");
            }
            else
            {
              DebugAssert(FSecureShell == NULL);
              try
              {
                FSecureShell = new TSecureShell(this, FSessionData, Log, Configuration);
                try
                {
                  // there will be only one channel in this session
                  FSecureShell->Simple = true;
                  FSecureShell->Open();
                }
                catch(Exception & E)
                {
                  DebugAssert(!FSecureShell->Active);
                  if (SessionData->FingerprintScan)
                  {
                    FFingerprintScanned = FSecureShell->GetHostKeyFingerprint();
                  }
                  if (!FSecureShell->Active && !FTunnelError.IsEmpty())
                  {
                    // the only case where we expect this to happen
                    DebugAssert(E.Message == LoadStr(UNEXPECTED_CLOSE_ERROR));
                    FatalError(&E, FMTLOAD(TUNNEL_ERROR, (FTunnelError)));
                  }
                  else
                  {
                    throw;
                  }
                }

                Log->AddSeparator();

                if ((SessionData->FSProtocol == fsSCPonly) ||
                    (SessionData->FSProtocol == fsSFTP && FSecureShell->SshFallbackCmd()))
                {
                  FFSProtocol = cfsSCP;
                  FFileSystem = new TSCPFileSystem(this, FSecureShell);
                  FSecureShell = NULL; // ownership passed
                  LogEvent(L"Using SCP protocol.");
                }
                else
                {
                  FFSProtocol = cfsSFTP;
                  FFileSystem = new TSFTPFileSystem(this, FSecureShell);
                  FSecureShell = NULL; // ownership passed
                  LogEvent(L"Using SFTP protocol.");
                }
              }
              __finally
              {
                delete FSecureShell;
                FSecureShell = NULL;
              }
            }
          }
          else
          {
            FFileSystem->Open();
          }
        }
        __finally
        {
          if (FSessionData->Tunnel)
          {
            FSessionData->RollbackTunnel();
          }
        }

        if (SessionData->CacheDirectoryChanges)
        {
          DebugAssert(FDirectoryChangesCache == NULL);
          FDirectoryChangesCache = new TRemoteDirectoryChangesCache(
            Configuration->CacheDirectoryChangesMaxSize);
          if (SessionData->PreserveDirectoryChanges)
          {
            Configuration->LoadDirectoryChangesCache(SessionData->SessionKey,
              FDirectoryChangesCache);
          }
        }

        DoStartup();

        if (FCollectFileSystemUsage)
        {
          FFileSystem->CollectUsage();
          FCollectFileSystemUsage = false;
        }

        DoInformation(LoadStr(STATUS_READY), true);
        FStatus = ssOpened;
      }
      catch(...)
      {
        // rollback
        if (FDirectoryChangesCache != NULL)
        {
          delete FDirectoryChangesCache;
          FDirectoryChangesCache = NULL;
        }
        if (SessionData->FingerprintScan && (FFileSystem != NULL) &&
            DebugAlwaysTrue(SessionData->Ftps != ftpsNone))
        {
          FFingerprintScanned = FFileSystem->GetSessionInfo().CertificateFingerprint;
        }
        throw;
      }
    }
    __finally
    {
      DoInformation(L"", true, 0);
    }
  }
  catch(EFatal &)
  {
    throw;
  }
  catch(Exception & E)
  {
    // any exception while opening session is fatal
    FatalError(&E, L"");
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsListenerFree(unsigned int PortNumber)
{
  SOCKET Socket = socket(AF_INET, SOCK_STREAM, 0);
  bool Result = (Socket != INVALID_SOCKET);
  if (Result)
  {
    SOCKADDR_IN Address;

    memset(&Address, 0, sizeof(Address));
    Address.sin_family = AF_INET;
    Address.sin_port = htons(static_cast<short>(PortNumber));
    Address.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    Result = (bind(Socket, reinterpret_cast<sockaddr *>(&Address), sizeof(Address)) == 0);
    closesocket(Socket);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenTunnel()
{
  DebugAssert(FTunnelData == NULL);

  FTunnelLocalPortNumber = FSessionData->TunnelLocalPortNumber;
  if (FTunnelLocalPortNumber == 0)
  {
    FTunnelLocalPortNumber = Configuration->TunnelLocalPortNumberLow;
    while (!IsListenerFree(FTunnelLocalPortNumber))
    {
      FTunnelLocalPortNumber++;
      if (FTunnelLocalPortNumber > Configuration->TunnelLocalPortNumberHigh)
      {
        FTunnelLocalPortNumber = 0;
        FatalError(NULL, FMTLOAD(TUNNEL_NO_FREE_PORT,
          (Configuration->TunnelLocalPortNumberLow, Configuration->TunnelLocalPortNumberHigh)));
      }
    }
    LogEvent(FORMAT(L"Autoselected tunnel local port number %d", (FTunnelLocalPortNumber)));
  }

  try
  {
    FTunnelData = new TSessionData(L"");
    FTunnelData->Assign(StoredSessions->DefaultSettings);
    FTunnelData->Name = FMTLOAD(TUNNEL_SESSION_NAME, (FSessionData->SessionName));
    FTunnelData->Tunnel = false;
    FTunnelData->HostName = FSessionData->TunnelHostName;
    FTunnelData->PortNumber = FSessionData->TunnelPortNumber;
    FTunnelData->UserName = FSessionData->TunnelUserName;
    FTunnelData->Password = FSessionData->TunnelPassword;
    FTunnelData->PublicKeyFile = FSessionData->TunnelPublicKeyFile;
    FTunnelData->TunnelPortFwd = FORMAT(L"L%d\t%s:%d",
      (FTunnelLocalPortNumber, FSessionData->HostNameExpanded, FSessionData->PortNumber));
    FTunnelData->HostKey = FSessionData->TunnelHostKey;

    // inherit proxy options on the main session
    FTunnelData->ProxyMethod = FSessionData->ProxyMethod;
    FTunnelData->ProxyHost = FSessionData->ProxyHost;
    FTunnelData->ProxyPort = FSessionData->ProxyPort;
    FTunnelData->ProxyUsername = FSessionData->ProxyUsername;
    FTunnelData->ProxyPassword = FSessionData->ProxyPassword;
    FTunnelData->ProxyTelnetCommand = FSessionData->ProxyTelnetCommand;
    FTunnelData->ProxyLocalCommand = FSessionData->ProxyLocalCommand;
    FTunnelData->ProxyDNS = FSessionData->ProxyDNS;
    FTunnelData->ProxyLocalhost = FSessionData->ProxyLocalhost;

    // inherit most SSH options of the main session (except for private key and bugs)
    FTunnelData->Compression = FSessionData->Compression;
    FTunnelData->SshProt = FSessionData->SshProt;
    FTunnelData->CipherList = FSessionData->CipherList;
    FTunnelData->Ssh2DES = FSessionData->Ssh2DES;

    FTunnelData->KexList = FSessionData->KexList;
    FTunnelData->RekeyData = FSessionData->RekeyData;
    FTunnelData->RekeyTime = FSessionData->RekeyTime;

    FTunnelData->SshNoUserAuth = FSessionData->SshNoUserAuth;
    FTunnelData->AuthGSSAPI = FSessionData->AuthGSSAPI;
    FTunnelData->GSSAPIFwdTGT = FSessionData->GSSAPIFwdTGT;
    FTunnelData->TryAgent = FSessionData->TryAgent;
    FTunnelData->AgentFwd = FSessionData->AgentFwd;
    FTunnelData->AuthTIS = FSessionData->AuthTIS;
    FTunnelData->AuthKI = FSessionData->AuthKI;
    FTunnelData->AuthKIPassword = FSessionData->AuthKIPassword;

    // The Started argument is not used with Parent being set
    FTunnelLog = new TSessionLog(this, TDateTime(), FTunnelData, Configuration);
    FTunnelLog->Parent = FLog;
    FTunnelLog->Name = L"Tunnel";
    FTunnelLog->ReflectSettings();
    FTunnelUI = new TTunnelUI(this);
    FTunnel = new TSecureShell(FTunnelUI, FTunnelData, FTunnelLog, Configuration);

    FTunnelOpening = true;
    try
    {
      FTunnel->Open();
    }
    __finally
    {
      FTunnelOpening = false;
    }

    FTunnelThread = new TTunnelThread(FTunnel);
  }
  catch(...)
  {
    CloseTunnel();
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CloseTunnel()
{
  SAFE_DESTROY_EX(TTunnelThread, FTunnelThread);
  FTunnelError = FTunnel->LastTunnelError;
  SAFE_DESTROY_EX(TSecureShell, FTunnel);
  SAFE_DESTROY_EX(TTunnelUI, FTunnelUI);
  SAFE_DESTROY_EX(TSessionLog, FTunnelLog);
  SAFE_DESTROY(FTunnelData);

  FTunnelLocalPortNumber = 0;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Closed()
{
  if (FTunnel != NULL)
  {
    CloseTunnel();
  }

  if (OnClose)
  {
    TCallbackGuard Guard(this);
    OnClose(this);
    Guard.Verify();
  }

  FStatus = ssClosed;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessGUI()
{
  // Do not process GUI here, as we are called directly from a GUI loop and may
  // recurse for good.
  // Alternatively we may check for (FOperationProgress == NULL)
  if (FNesting == 0)
  {
    TAutoNestingCounter NestingCounter(FNesting);
    ::ProcessGUI();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Progress(TFileOperationProgressType * OperationProgress)
{
  if (FNesting == 0)
  {
    TAutoNestingCounter NestingCounter(FNesting);
    OperationProgress->Progress();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Reopen(int Params)
{
  TFSProtocol OrigFSProtocol = SessionData->FSProtocol;
  UnicodeString PrevRemoteDirectory = SessionData->RemoteDirectory;
  bool PrevReadCurrentDirectoryPending = FReadCurrentDirectoryPending;
  bool PrevReadDirectoryPending = FReadDirectoryPending;
  DebugAssert(!FSuspendTransaction);
  bool PrevAutoReadDirectory = FAutoReadDirectory;
  // here used to be a check for FExceptionOnFail being 0
  // but it can happen, e.g. when we are downloading file to execute it.
  // however I'm not sure why we mind having excaption-on-fail enabled here
  int PrevExceptionOnFail = FExceptionOnFail;
  try
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
    FSuspendTransaction = true;
    FExceptionOnFail = 0;
    // typically, we avoid reading directory, when there is operation ongoing,
    // for file list which may reference files from current directory
    if (FLAGSET(Params, ropNoReadDirectory))
    {
      AutoReadDirectory = false;
    }

    // only peek, we may not be connected at all atm,
    // so make sure we do not try retrieving current directory from the server
    // (particularly with FTP)
    UnicodeString ACurrentDirectory = PeekCurrentDirectory();
    if (!ACurrentDirectory.IsEmpty())
    {
      SessionData->RemoteDirectory = ACurrentDirectory;
    }
    if (SessionData->FSProtocol == fsSFTP)
    {
      SessionData->FSProtocol = (FFSProtocol == cfsSCP ? fsSCPonly : fsSFTPonly);
    }

    // Could be active before, if fatal error occured in the secondary terminal.
    // But now, since we handle the secondary terminal's OnClose,
    // by closing outselves, it should not happen anymore.
    if (DebugAlwaysFalse(Active))
    {
      Close();
    }

    Open();
  }
  __finally
  {
    SessionData->RemoteDirectory = PrevRemoteDirectory;
    SessionData->FSProtocol = OrigFSProtocol;
    FAutoReadDirectory = PrevAutoReadDirectory;
    FReadCurrentDirectoryPending = PrevReadCurrentDirectoryPending;
    FReadDirectoryPending = PrevReadDirectoryPending;
    FSuspendTransaction = false;
    FExceptionOnFail = PrevExceptionOnFail;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, UnicodeString Prompt, bool Echo, int MaxLen, UnicodeString & Result)
{
  bool AResult;
  TStrings * Prompts = new TStringList;
  TStrings * Results = new TStringList;
  try
  {
    Prompts->AddObject(Prompt, (TObject *)(FLAGMASK(Echo, pupEcho)));
    Results->AddObject(Result, (TObject *)MaxLen);

    AResult = PromptUser(Data, Kind, Name, Instructions, Prompts, Results);

    Result = Results->Strings[0];
  }
  __finally
  {
    delete Prompts;
    delete Results;
  }

  return AResult;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::PromptUser(TSessionData * Data, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  // If PromptUser is overridden in descendant class, the overridden version
  // is not called when accessed via TSessionIU interface.
  // So this is workaround.
  // Actually no longer needed as we do not uverride DoPromptUser
  // anymore in TSecondaryTerminal.
  return DoPromptUser(Data, Kind, Name, Instructions, Prompts, Results);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminal::GetPasswordSource()
{
  return this;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoPromptUser(TSessionData * /*Data*/, TPromptKind Kind,
  UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results)
{
  bool AResult = false;


  bool PasswordOrPassphrasePrompt = IsPasswordOrPassphrasePrompt(Kind, Prompts);
  if (PasswordOrPassphrasePrompt)
  {
    bool & PasswordTried =
      FTunnelOpening ? FRememberedTunnelPasswordTried : FRememberedPasswordTried;
    if (!PasswordTried)
    {
      // let's expect that the main session is already authenticated and its password
      // is not written after, so no locking is necessary
      // (no longer true, once the main session can be reconnected)
      UnicodeString APassword;
      if (FTunnelOpening)
      {
        APassword = GetPasswordSource()->RememberedTunnelPassword;
      }
      else
      {
        APassword = GetPasswordSource()->RememberedPassword;
      }
      Results->Strings[0] = APassword;
      if (!Results->Strings[0].IsEmpty())
      {
        LogEvent(L"Using remembered password.");
        AResult = true;
      }
      PasswordTried = true;
    }
  }

  if (!AResult)
  {
    if (PasswordOrPassphrasePrompt && !Configuration->RememberPassword)
    {
      Prompts->Objects[0] = (TObject*)(int(Prompts->Objects[0]) | pupRemember);
    }

    if (OnPromptUser != NULL)
    {
      TCallbackGuard Guard(this);
      OnPromptUser(this, Kind, Name, Instructions, Prompts, Results, AResult, NULL);
      Guard.Verify();
    }

    if (AResult && PasswordOrPassphrasePrompt &&
        (Configuration->RememberPassword || FLAGSET(int(Prompts->Objects[0]), pupRemember)))
    {
      RawByteString EncryptedPassword = EncryptPassword(Results->Strings[0]);
      if (FTunnelOpening)
      {
        GetPasswordSource()->FRememberedTunnelPassword = EncryptedPassword;
      }
      else
      {
        GetPasswordSource()->FRememberedPassword = EncryptedPassword;
      }
    }
  }

  return AResult;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::QueryUser(const UnicodeString Query,
  TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  LogEvent(FORMAT(L"Asking user:\n%s (%s)", (Query, (MoreMessages ? MoreMessages->CommaText : UnicodeString() ))));
  unsigned int Answer = AbortAnswer(Answers);
  if (FOnQueryUser)
  {
    TCallbackGuard Guard(this);
    FOnQueryUser(this, Query, MoreMessages, Answers, Params, Answer, QueryType, NULL);
    Guard.Verify();
  }
  return Answer;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::QueryUserException(const UnicodeString Query,
  Exception * E, unsigned int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  unsigned int Result;
  UnicodeString ExMessage;
  if (DebugAlwaysTrue(ExceptionMessage(E, ExMessage) || !Query.IsEmpty()))
  {
    TStrings * MoreMessages = new TStringList();
    try
    {
      if (!ExMessage.IsEmpty() && !Query.IsEmpty())
      {
        MoreMessages->Add(UnformatMessage(ExMessage));
      }

      ExtException * EE = dynamic_cast<ExtException*>(E);
      if ((EE != NULL) && (EE->MoreMessages != NULL))
      {
        MoreMessages->AddStrings(EE->MoreMessages);
      }

      // We know MoreMessages not to be NULL here,
      // AppendExceptionStackTraceAndForget should never return true
      // (indicating it had to create the string list)
      DebugAlwaysFalse(AppendExceptionStackTraceAndForget(MoreMessages));

      TQueryParams HelpKeywordOverrideParams;
      if (Params != NULL)
      {
        HelpKeywordOverrideParams.Assign(*Params);
      }
      HelpKeywordOverrideParams.HelpKeyword =
        MergeHelpKeyword(HelpKeywordOverrideParams.HelpKeyword, GetExceptionHelpKeyword(E));

      Result = QueryUser(!Query.IsEmpty() ? Query : ExMessage,
        MoreMessages->Count ? MoreMessages : NULL,
        Answers, &HelpKeywordOverrideParams, QueryType);
    }
    __finally
    {
      delete MoreMessages;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DisplayBanner(const UnicodeString & Banner)
{
  if (OnDisplayBanner != NULL)
  {
    if (Configuration->ForceBanners ||
        Configuration->ShowBanner(SessionData->SessionKey, Banner))
    {
      bool NeverShowAgain = false;
      int Options =
        FLAGMASK(Configuration->ForceBanners, boDisableNeverShowAgain);
      TCallbackGuard Guard(this);
      OnDisplayBanner(this, SessionData->SessionName, Banner,
        NeverShowAgain, Options);
      Guard.Verify();
      if (!Configuration->ForceBanners && NeverShowAgain)
      {
        Configuration->NeverShowBanner(SessionData->SessionKey, Banner);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::HandleExtendedException(Exception * E)
{
  Log->AddException(E);
  if (OnShowExtendedException != NULL)
  {
    TCallbackGuard Guard(this);
    // the event handler may destroy 'this' ...
    OnShowExtendedException(this, E, NULL);
    // .. hence guard is dismissed from destructor, to make following call no-op
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ShowExtendedException(Exception * E)
{
  Log->AddException(E);
  if (OnShowExtendedException != NULL)
  {
    OnShowExtendedException(this, E, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoInformation(const UnicodeString & Str, bool Status,
  int Phase)
{
  if (OnInformation)
  {
    TCallbackGuard Guard(this);
    OnInformation(this, Str, Status, Phase);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Information(const UnicodeString & Str, bool Status)
{
  DoInformation(Str, Status);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoProgress(TFileOperationProgressType & ProgressData)
{
  if (OnProgress != NULL)
  {
    TCallbackGuard Guard(this);
    OnProgress(ProgressData);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoFinished(TFileOperation Operation, TOperationSide Side, bool Temp,
  const UnicodeString & FileName, bool Success, TOnceDoneOperation & OnceDoneOperation)
{
  if (OnFinished != NULL)
  {
    TCallbackGuard Guard(this);
    OnFinished(Operation, Side, Temp, FileName, Success, OnceDoneOperation);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SaveCapabilities(TFileSystemInfo & FileSystemInfo)
{
  for (int Index = 0; Index < fcCount; Index++)
  {
    FileSystemInfo.IsCapable[Index] = IsCapable[(TFSCapability)Index];
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetIsCapable(TFSCapability Capability) const
{
  DebugAssert(FFileSystem);
  return FFileSystem->IsCapable(Capability);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::AbsolutePath(UnicodeString Path, bool Local)
{
  return FFileSystem->AbsolutePath(Path, Local);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReactOnCommand(int /*TFSCommand*/ Cmd)
{
  bool ChangesDirectory = false;
  bool ModifiesFiles = false;

  switch ((TFSCommand)Cmd) {
    case fsChangeDirectory:
    case fsHomeDirectory:
      ChangesDirectory = true;
      break;

    case fsCopyToRemote:
    case fsDeleteFile:
    case fsRenameFile:
    case fsMoveFile:
    case fsCopyFile:
    case fsCreateDirectory:
    case fsChangeMode:
    case fsChangeGroup:
    case fsChangeOwner:
    case fsChangeProperties:
    case fsLock:
      ModifiesFiles = true;
      break;

    case fsAnyCommand:
      ChangesDirectory = true;
      ModifiesFiles = true;
      break;
  }

  if (ChangesDirectory)
  {
    if (!InTransaction())
    {
      ReadCurrentDirectory();
      if (AutoReadDirectory)
      {
        ReadDirectory(false);
      }
    }
    else
    {
      FReadCurrentDirectoryPending = true;
      if (AutoReadDirectory)
      {
        FReadDirectoryPending = true;
      }
    }
  }
  else if (ModifiesFiles && AutoReadDirectory && Configuration->AutoReadDirectoryAfterOp)
  {
    if (!InTransaction())
    {
      ReadDirectory(true);
    }
    else
    {
      FReadDirectoryPending = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::TerminalError(UnicodeString Msg)
{
  TerminalError(NULL, Msg);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::TerminalError(
  Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  throw ETerminal(E, Msg, HelpKeyword);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoQueryReopen(Exception * E)
{
  EFatal * Fatal = dynamic_cast<EFatal *>(E);
  DebugAssert(Fatal != NULL);
  bool Result;
  if ((Fatal != NULL) && Fatal->ReopenQueried)
  {
    Result = false;
  }
  else
  {
    LogEvent(L"Connection was lost, asking what to do.");

    TQueryParams Params(qpAllowContinueOnError);
    Params.Timeout = Configuration->SessionReopenAuto;
    Params.TimeoutAnswer = qaRetry;
    TQueryButtonAlias Aliases[1];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RECONNECT_BUTTON);
    Params.Aliases = Aliases;
    Params.AliasesCount = LENOF(Aliases);
    Result = (QueryUserException(L"", E, qaRetry | qaAbort, &Params, qtError) == qaRetry);

    if (Fatal != NULL)
    {
      Fatal->ReopenQueried = true;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::QueryReopen(Exception * E, int Params,
  TFileOperationProgressType * OperationProgress)
{
  TSuspendFileOperationProgress Suspend(OperationProgress);

  bool Result = DoQueryReopen(E);

  if (Result)
  {
    TDateTime Start = Now();
    do
    {
      try
      {
        Reopen(Params);
      }
      catch(Exception & E)
      {
        if (!Active)
        {
          Result =
            ((Configuration->SessionReopenTimeout == 0) ||
             (int(double(Now() - Start) * MSecsPerDay) < Configuration->SessionReopenTimeout)) &&
            DoQueryReopen(&E);
        }
        else
        {
          throw;
        }
      }
    }
    while (!Active && Result);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::FileOperationLoopQuery(Exception & E,
  TFileOperationProgressType * OperationProgress, const UnicodeString Message,
  bool AllowSkip, UnicodeString SpecialRetry, UnicodeString HelpKeyword)
{
  bool Result = false;
  Log->AddException(&E);
  unsigned int Answer;
  bool SkipToAllPossible = AllowSkip && (OperationProgress != NULL);

  if (SkipToAllPossible && OperationProgress->SkipToAll)
  {
    Answer = qaSkip;
  }
  else
  {
    int Answers = qaRetry | qaAbort |
      FLAGMASK(AllowSkip, qaSkip) |
      FLAGMASK(SkipToAllPossible, qaAll) |
      FLAGMASK(!SpecialRetry.IsEmpty(), qaYes);
    TQueryParams Params(qpAllowContinueOnError | FLAGMASK(!AllowSkip, qpFatalAbort));
    Params.HelpKeyword = HelpKeyword;
    TQueryButtonAlias Aliases[2];
    int AliasCount = 0;

    if (FLAGSET(Answers, qaAll))
    {
      Aliases[AliasCount].Button = qaAll;
      Aliases[AliasCount].Alias = LoadStr(SKIP_ALL_BUTTON);
      AliasCount++;
    }
    if (FLAGSET(Answers, qaYes))
    {
      Aliases[AliasCount].Button = qaYes;
      Aliases[AliasCount].Alias = SpecialRetry;
      AliasCount++;
    }

    if (AliasCount > 0)
    {
      Params.Aliases = Aliases;
      Params.AliasesCount = AliasCount;
    }

    {
      TSuspendFileOperationProgress Suspend(OperationProgress);
      Answer = QueryUserException(Message, &E, Answers, &Params, qtError);
    }

    if (Answer == qaAll)
    {
      DebugAssert(OperationProgress != NULL);
      OperationProgress->SkipToAll = true;
      Answer = qaSkip;
    }
    if (Answer == qaYes)
    {
      Result = true;
      Answer = qaRetry;
    }
  }

  if (Answer != qaRetry)
  {
    if ((Answer == qaAbort) && (OperationProgress != NULL))
    {
      OperationProgress->Cancel = csCancel;
    }

    if (AllowSkip)
    {
      THROW_SKIP_FILE(&E, Message);
    }
    else
    {
      // this can happen only during file transfer with SCP
      throw ExtException(&E, Message);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::FileOperationLoop(TFileOperationEvent CallBackFunc,
  TFileOperationProgressType * OperationProgress, bool AllowSkip,
  const UnicodeString Message, void * Param1, void * Param2)
{
  DebugAssert(CallBackFunc);
  int Result;
  FILE_OPERATION_LOOP_BEGIN
  {
    Result = CallBackFunc(Param1, Param2);
  }
  FILE_OPERATION_LOOP_END_EX(Message, AllowSkip);

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::TranslateLockedPath(UnicodeString Path, bool Lock)
{
  if (SessionData->LockInHome && !Path.IsEmpty() && (Path[1] == L'/'))
  {
    if (Lock)
    {
      if (Path.SubString(1, FLockDirectory.Length()) == FLockDirectory)
      {
        Path.Delete(1, FLockDirectory.Length());
        if (Path.IsEmpty()) Path = L"/";
      }
    }
    else
    {
      Path = UnixExcludeTrailingBackslash(FLockDirectory + Path);
    }
  }
  return Path;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ClearCaches()
{
  FDirectoryCache->Clear();
  if (FDirectoryChangesCache != NULL)
  {
    FDirectoryChangesCache->Clear();
  }
  if (FCommandSession != NULL)
  {
    FCommandSession->ClearCaches();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ClearCachedFileList(const UnicodeString Path,
  bool SubDirs)
{
  FDirectoryCache->ClearFileList(Path, SubDirs);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AddCachedFileList(TRemoteFileList * FileList)
{
  FDirectoryCache->AddFileList(FileList);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DirectoryFileList(const UnicodeString Path,
  TRemoteFileList *& FileList, bool CanLoad)
{
  bool Result = false;
  if (UnixSamePath(FFiles->Directory, Path))
  {
    Result = (FileList == NULL) || (FileList->Timestamp < FFiles->Timestamp);
    if (Result)
    {
      if (FileList == NULL)
      {
        FileList = new TRemoteFileList();
      }
      FFiles->DuplicateTo(FileList);
    }
  }
  else
  {
    if (((FileList == NULL) && FDirectoryCache->HasFileList(Path)) ||
        ((FileList != NULL) && FDirectoryCache->HasNewerFileList(Path, FileList->Timestamp)))
    {
      bool Created = (FileList == NULL);
      if (Created)
      {
        FileList = new TRemoteFileList();
      }

      Result = FDirectoryCache->GetFileList(Path, FileList);
      if (!Result && Created)
      {
        SAFE_DESTROY(FileList);
      }
    }
    // do not attempt to load file list if there is cached version,
    // only absence of cached version indicates that we consider
    // the directory content obsolete
    else if (CanLoad && !FDirectoryCache->HasFileList(Path))
    {
      bool Created = (FileList == NULL);
      if (Created)
      {
        FileList = new TRemoteFileList();
      }
      FileList->Directory = Path;

      try
      {
        ReadDirectory(FileList);
        Result = true;
      }
      catch(...)
      {
        if (Created)
        {
          SAFE_DESTROY(FileList);
        }
        throw;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SetCurrentDirectory(UnicodeString value)
{
  DebugAssert(FFileSystem);
  value = TranslateLockedPath(value, false);
  if (value != FFileSystem->CurrentDirectory)
  {
    ChangeDirectory(value);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetCurrentDirectory()
{
  if (FFileSystem != NULL)
  {
    // there's occasional crash when assigning FFileSystem->CurrentDirectory
    // to FCurrentDirectory, splitting the assignment to two statemets
    // to locate the crash more closely
    UnicodeString CurrentDirectory = FFileSystem->CurrentDirectory;
    FCurrentDirectory = CurrentDirectory;
    if (FCurrentDirectory.IsEmpty())
    {
      ReadCurrentDirectory();
    }
  }

  UnicodeString Result = TranslateLockedPath(FCurrentDirectory, true);
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::PeekCurrentDirectory()
{
  if (FFileSystem)
  {
    FCurrentDirectory = FFileSystem->CurrentDirectory;
  }

  UnicodeString Result = TranslateLockedPath(FCurrentDirectory, true);
  return Result;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetGroups()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FGroups;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetUsers()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FUsers;
}
//---------------------------------------------------------------------------
const TRemoteTokenList * __fastcall TTerminal::GetMembership()
{
  DebugAssert(FFileSystem);
  LookupUsersGroups();
  return &FMembership;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetUserName() const
{
  // in future might also be implemented to detect username similar to GetUserGroups
  DebugAssert(FFileSystem != NULL);
  UnicodeString Result = FFileSystem->GetUserName();
  // Is empty also when stored username was used
  if (Result.IsEmpty())
  {
    Result = SessionData->UserNameExpanded;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetAreCachesEmpty() const
{
  return FDirectoryCache->IsEmpty &&
    ((FDirectoryChangesCache == NULL) || FDirectoryChangesCache->IsEmpty);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoInitializeLog()
{
  if (FOnInitializeLog)
  {
    TCallbackGuard Guard(this);
    FOnInitializeLog(this);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoChangeDirectory()
{
  if (FOnChangeDirectory)
  {
    TCallbackGuard Guard(this);
    FOnChangeDirectory(this);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectory(bool ReloadOnly)
{
  if (FOnReadDirectory)
  {
    TCallbackGuard Guard(this);
    FOnReadDirectory(this, ReloadOnly);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartReadDirectory()
{
  if (FOnStartReadDirectory)
  {
    TCallbackGuard Guard(this);
    FOnStartReadDirectory(this);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectoryProgress(int Progress, int ResolvedLinks, bool & Cancel)
{
  if (FReadingCurrentDirectory && (FOnReadDirectoryProgress != NULL))
  {
    TCallbackGuard Guard(this);
    FOnReadDirectoryProgress(this, Progress, ResolvedLinks, Cancel);
    Guard.Verify();
  }
  if (FOnFindingFile != NULL)
  {
    TCallbackGuard Guard(this);
    FOnFindingFile(this, L"", Cancel);
    Guard.Verify();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::InTransaction()
{
  return (FInTransaction > 0) && !FSuspendTransaction;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::BeginTransaction()
{
  if (FInTransaction == 0)
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
  }
  FInTransaction++;

  if (FCommandSession != NULL)
  {
    FCommandSession->BeginTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EndTransaction()
{
  DoEndTransaction(false);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoEndTransaction(bool Inform)
{
  if (FInTransaction == 0)
    TerminalError(L"Can't end transaction, not in transaction");
  DebugAssert(FInTransaction > 0);
  FInTransaction--;

  // it connection was closed due to fatal error during transaction, do nothing
  if (Active)
  {
    if (FInTransaction == 0)
    {
      try
      {
        if (FReadCurrentDirectoryPending)
        {
          ReadCurrentDirectory();
        }

        if (FReadDirectoryPending)
        {
          if (Inform)
          {
            DoInformation(LoadStr(STATUS_OPEN_DIRECTORY), true);
          }
          ReadDirectory(!FReadCurrentDirectoryPending);
        }
      }
      __finally
      {
        FReadCurrentDirectoryPending = false;
        FReadDirectoryPending = false;
      }
    }
  }

  if (FCommandSession != NULL)
  {
    FCommandSession->EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SetExceptionOnFail(bool value)
{
  if (value)
  {
    FExceptionOnFail++;
  }
  else
  {
    if (FExceptionOnFail == 0)
      throw Exception(L"ExceptionOnFail is already zero.");
    FExceptionOnFail--;
  }

  if (FCommandSession != NULL)
  {
    FCommandSession->FExceptionOnFail = FExceptionOnFail;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetExceptionOnFail() const
{
  return (bool)(FExceptionOnFail > 0);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FatalAbort()
{
  FatalError(NULL, "");
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword)
{
  bool SecureShellActive = (FSecureShell != NULL) && FSecureShell->Active;
  if (Active || SecureShellActive)
  {
    // We log this instead of exception handler, because Close() would
    // probably cause exception handler to loose pointer to TShellLog()
    LogEvent(L"Attempt to close connection due to fatal exception:");
    Log->Add(llException, Msg);
    Log->AddException(E);

    if (Active)
    {
      Close();
    }

    // this may happen if failure of authentication of SSH, owned by terminal yet
    // (because the protocol was not decided yet), is detected by us (not by putty).
    // e.g. not verified host key
    if (SecureShellActive)
    {
      FSecureShell->Close();
    }
  }

  if (FCallbackGuard != NULL)
  {
    FCallbackGuard->FatalError(E, Msg, HelpKeyword);
  }
  else
  {
    throw ESshFatal(E, Msg, HelpKeyword);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CommandError(Exception * E, const UnicodeString Msg)
{
  CommandError(E, Msg, 0);
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::CommandError(Exception * E, const UnicodeString Msg,
  unsigned int Answers, const UnicodeString HelpKeyword)
{
  // may not be, particularly when TTerminal::Reopen is being called
  // from within OnShowExtendedException handler
  DebugAssert(FCallbackGuard == NULL);
  unsigned int Result = 0;
  if (E && E->InheritsFrom(__classid(EFatal)))
  {
    FatalError(E, Msg, HelpKeyword);
  }
  else if (E && E->InheritsFrom(__classid(EAbort)))
  {
    // resent EAbort exception
    Abort();
  }
  else if (ExceptionOnFail)
  {
    throw ECommand(E, Msg, HelpKeyword);
  }
  else if (!Answers)
  {
    ECommand * ECmd = new ECommand(E, Msg, HelpKeyword);
    try
    {
      HandleExtendedException(ECmd);
    }
    __finally
    {
      delete ECmd;
    }
  }
  else
  {
    // small hack to enable "skip to all" for TRetryOperationLoop
    bool CanSkip = FLAGSET(Answers, qaSkip) && (OperationProgress != NULL);
    if (CanSkip && OperationProgress->SkipToAll)
    {
      Result = qaSkip;
    }
    else
    {
      TQueryParams Params(qpAllowContinueOnError, HelpKeyword);
      TQueryButtonAlias Aliases[1];
      if (CanSkip)
      {
        Aliases[0].Button = qaAll;
        Aliases[0].Alias = LoadStr(SKIP_ALL_BUTTON);
        Params.Aliases = Aliases;
        Params.AliasesCount = LENOF(Aliases);
        Answers |= qaAll;
      }
      Result = QueryUserException(Msg, E, Answers, &Params, qtError);
      if (Result == qaAll)
      {
        DebugAssert(OperationProgress != NULL);
        OperationProgress->SkipToAll = true;
        Result = qaSkip;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::HandleException(Exception * E)
{
  if (ExceptionOnFail)
  {
    return false;
  }
  else
  {
    Log->AddException(E);
    return true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CloseOnCompletion(TOnceDoneOperation Operation, const UnicodeString Message)
{
  Configuration->Usage->Inc(L"ClosesOnCompletion");
  LogEvent(L"Closing session after completed operation (as requested by user)");
  Close();
  throw ESshTerminate(NULL,
    Message.IsEmpty() ? UnicodeString(LoadStr(CLOSED_ON_COMPLETION)) : Message,
    Operation);
}
//---------------------------------------------------------------------------
TBatchOverwrite __fastcall TTerminal::EffectiveBatchOverwrite(
  const UnicodeString & SourceFullFileName, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, bool Special)
{
  TBatchOverwrite Result;
  if (Special &&
      (FLAGSET(Params, cpResume) || CopyParam->ResumeTransfer(SourceFullFileName)))
  {
    Result = boResume;
  }
  else if (FLAGSET(Params, cpAppend))
  {
    Result = boAppend;
  }
  else if (CopyParam->NewerOnly &&
           (((OperationProgress->Side == osLocal) && IsCapable[fcNewerOnlyUpload]) ||
            (OperationProgress->Side != osLocal)))
  {
    // no way to change batch overwrite mode when CopyParam->NewerOnly is on
    Result = boOlder;
  }
  else if (FLAGSET(Params, cpNoConfirmation) || !Configuration->ConfirmOverwriting)
  {
    // no way to change batch overwrite mode when overwrite confirmations are off
    DebugAssert(OperationProgress->BatchOverwrite == boNo);
    Result = boAll;
  }
  else
  {
    Result = OperationProgress->BatchOverwrite;
    if (!Special &&
        ((Result == boOlder) || (Result == boAlternateResume) || (Result == boResume)))
    {
      Result = boNo;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CheckRemoteFile(
  const UnicodeString & FileName, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress)
{
  return (EffectiveBatchOverwrite(FileName, CopyParam, Params, OperationProgress, true) != boAll);
}
//---------------------------------------------------------------------------
unsigned int __fastcall TTerminal::ConfirmFileOverwrite(
  const UnicodeString & SourceFullFileName, const UnicodeString & TargetFileName,
  const TOverwriteFileParams * FileParams, unsigned int Answers, TQueryParams * QueryParams,
  TOperationSide Side, const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
  UnicodeString Message)
{
  unsigned int Result;
  // duplicated in TSFTPFileSystem::SFTPConfirmOverwrite
  bool CanAlternateResume =
    (FileParams != NULL) &&
    (FileParams->DestSize < FileParams->SourceSize) &&
    !OperationProgress->AsciiTransfer;
  TBatchOverwrite BatchOverwrite = EffectiveBatchOverwrite(SourceFullFileName, CopyParam, Params, OperationProgress, true);
  bool Applicable = true;
  switch (BatchOverwrite)
  {
    case boOlder:
      Applicable = (FileParams != NULL);
      break;

    case boAlternateResume:
      Applicable = CanAlternateResume;
      break;

    case boResume:
      Applicable = CanAlternateResume;
      break;
  }

  if (!Applicable)
  {
    TBatchOverwrite ABatchOverwrite = EffectiveBatchOverwrite(SourceFullFileName, CopyParam, Params, OperationProgress, false);
    DebugAssert(BatchOverwrite != ABatchOverwrite);
    BatchOverwrite = ABatchOverwrite;
  }

  if (BatchOverwrite == boNo)
  {
    if (Message.IsEmpty())
    {
      // Side refers to destination side here
      Message = FMTLOAD((Side == osLocal ? LOCAL_FILE_OVERWRITE2 :
        REMOTE_FILE_OVERWRITE2), (TargetFileName, TargetFileName));
    }
    if (FileParams != NULL)
    {
      Message = FMTLOAD(FILE_OVERWRITE_DETAILS, (Message,
        FormatSize(FileParams->SourceSize),
        UserModificationStr(FileParams->SourceTimestamp, FileParams->SourcePrecision),
        FormatSize(FileParams->DestSize),
        UserModificationStr(FileParams->DestTimestamp, FileParams->DestPrecision)));
    }
    if (DebugAlwaysTrue(QueryParams->HelpKeyword.IsEmpty()))
    {
      QueryParams->HelpKeyword = HELP_OVERWRITE;
    }
    Result = QueryUser(Message, NULL, Answers, QueryParams);
    switch (Result)
    {
      case qaNeverAskAgain:
        Configuration->ConfirmOverwriting = false;
        Result = qaYes;
        break;

      case qaYesToAll:
        BatchOverwrite = boAll;
        break;

      case qaAll:
        BatchOverwrite = boOlder;
        break;

      case qaNoToAll:
        BatchOverwrite = boNone;
        break;
    }

    // we user has not selected another batch overwrite mode,
    // keep the current one. note that we may get here even
    // when batch overwrite was selected already, but it could not be applied
    // to current transfer (see condition above)
    if (BatchOverwrite != boNo)
    {
      OperationProgress->BatchOverwrite = BatchOverwrite;
    }
  }

  if (BatchOverwrite != boNo)
  {
    switch (BatchOverwrite)
    {
      case boAll:
        Result = qaYes;
        break;

      case boNone:
        Result = qaNo;
        break;

      case boOlder:
        if (FileParams == NULL)
        {
          Result = qaNo;
        }
        else
        {
          TModificationFmt Precision = LessDateTimePrecision(FileParams->SourcePrecision, FileParams->DestPrecision);
          TDateTime ReducedSourceTimestamp =
            ReduceDateTimePrecision(FileParams->SourceTimestamp, Precision);
          TDateTime ReducedDestTimestamp =
            ReduceDateTimePrecision(FileParams->DestTimestamp, Precision);

          Result =
            (CompareFileTime(ReducedSourceTimestamp, ReducedDestTimestamp) > 0) ?
            qaYes : qaNo;

          LogEvent(FORMAT(L"Source file timestamp is [%s], destination timestamp is [%s], will%s overwrite",
            (StandardTimestamp(ReducedSourceTimestamp),
             StandardTimestamp(ReducedDestTimestamp),
             ((Result == qaYes) ? L"" : L" not"))));
        }
        break;

      case boAlternateResume:
        DebugAssert(CanAlternateResume);
        Result = qaSkip; // ugh
        break;

      case boAppend:
        Result = qaRetry;
        break;

      case boResume:
        Result = qaRetry;
        break;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileModified(const TRemoteFile * File,
  const UnicodeString FileName, bool ClearDirectoryChange)
{
  UnicodeString ParentDirectory;
  UnicodeString Directory;

  if (SessionData->CacheDirectories || SessionData->CacheDirectoryChanges)
  {
    if ((File != NULL) && (File->Directory != NULL))
    {
      if (File->IsDirectory)
      {
        Directory = File->Directory->FullDirectory + File->FileName;
      }
      ParentDirectory = File->Directory->Directory;
    }
    else if (!FileName.IsEmpty())
    {
      ParentDirectory = UnixExtractFilePath(FileName);
      if (ParentDirectory.IsEmpty())
      {
        ParentDirectory = CurrentDirectory;
      }

      // this case for scripting
      if ((File != NULL) && File->IsDirectory)
      {
        Directory = UnixIncludeTrailingBackslash(ParentDirectory) +
          UnixExtractFileName(File->FileName);
      }
    }
  }

  if (SessionData->CacheDirectories)
  {
    if (!Directory.IsEmpty())
    {
      DirectoryModified(Directory, true);
    }
    if (!ParentDirectory.IsEmpty())
    {
      DirectoryModified(ParentDirectory, false);
    }
  }

  if (SessionData->CacheDirectoryChanges && ClearDirectoryChange)
  {
    if (!Directory.IsEmpty())
    {
      FDirectoryChangesCache->ClearDirectoryChange(Directory);
      FDirectoryChangesCache->ClearDirectoryChangeTarget(Directory);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryModified(const UnicodeString Path, bool SubDirs)
{
  if (Path.IsEmpty())
  {
    ClearCachedFileList(CurrentDirectory, SubDirs);
  }
  else
  {
    ClearCachedFileList(Path, SubDirs);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  AddCachedFileList(FileList);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReloadDirectory()
{
  if (SessionData->CacheDirectories)
  {
    DirectoryModified(CurrentDirectory, false);
  }
  if (SessionData->CacheDirectoryChanges)
  {
    DebugAssert(FDirectoryChangesCache != NULL);
    FDirectoryChangesCache->ClearDirectoryChange(CurrentDirectory);
  }

  ReadCurrentDirectory();
  FReadCurrentDirectoryPending = false;
  ReadDirectory(true);
  FReadDirectoryPending = false;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RefreshDirectory()
{
  if (!SessionData->CacheDirectories)
  {
    LogEvent(L"Not refreshing directory, caching is off.");
  }
  else if (FDirectoryCache->HasNewerFileList(CurrentDirectory, FFiles->Timestamp))
  {
    // Second parameter was added to allow (rather force) using the cache.
    // Before, the directory was reloaded always, it seems useless,
    // has it any reason?
    ReadDirectory(true, true);
    FReadDirectoryPending = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EnsureNonExistence(const UnicodeString FileName)
{
  // if filename doesn't contain path, we check for existence of file
  if ((UnixExtractFileDir(FileName).IsEmpty()) &&
      UnixSamePath(CurrentDirectory, FFiles->Directory))
  {
    TRemoteFile *File = FFiles->FindFile(FileName);
    if (File)
    {
      if (File->IsDirectory) throw ECommand(NULL, FMTLOAD(RENAME_CREATE_DIR_EXISTS, (FileName)));
        else throw ECommand(NULL, FMTLOAD(RENAME_CREATE_FILE_EXISTS, (FileName)));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall inline TTerminal::LogEvent(const UnicodeString & Str)
{
  if (Log->Logging)
  {
    Log->Add(llMessage, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RollbackAction(TSessionAction & Action,
  TFileOperationProgressType * OperationProgress, Exception * E)
{
  // EScpSkipFile without "cancel" is file skip,
  // and we do not want to record skipped actions.
  // But EScpSkipFile with "cancel" is abort and we want to record that.
  // Note that TSCPFileSystem modifies the logic of RollbackAction little bit.
  if ((dynamic_cast<EScpSkipFile *>(E) != NULL) &&
      ((OperationProgress == NULL) ||
       (OperationProgress->Cancel == csContinue)))
  {
    Action.Cancel();
  }
  else
  {
    Action.Rollback(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartup()
{
  LogEvent(L"Doing startup conversation with host.");
  BeginTransaction();
  try
  {
    DoInformation(LoadStr(STATUS_STARTUP), true);

    // Make sure that directory would be loaded at last
    FReadCurrentDirectoryPending = true;
    FReadDirectoryPending = AutoReadDirectory;

    FFileSystem->DoStartup();

    LookupUsersGroups();

    if (!SessionData->RemoteDirectory.IsEmpty())
    {
      ChangeDirectory(SessionData->RemoteDirectory);
    }
  }
  __finally
  {
    DoEndTransaction(true);
  }
  LogEvent(L"Startup conversation with host finished.");
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadCurrentDirectory()
{
  DebugAssert(FFileSystem);
  try
  {
    // reset flag is case we are called externally (like from console dialog)
    FReadCurrentDirectoryPending = false;

    LogEvent(L"Getting current directory name.");
    UnicodeString OldDirectory = FFileSystem->CurrentDirectory;

    FFileSystem->ReadCurrentDirectory();
    ReactOnCommand(fsCurrentDirectory);

    if (SessionData->CacheDirectoryChanges)
    {
      DebugAssert(FDirectoryChangesCache != NULL);
      if (!CurrentDirectory.IsEmpty() && !FLastDirectoryChange.IsEmpty() && (CurrentDirectory != OldDirectory))
      {
        FDirectoryChangesCache->AddDirectoryChange(OldDirectory,
          FLastDirectoryChange, CurrentDirectory);
      }
      // not to broke the cache, if the next directory change would not
      // be initialized by ChangeDirectory(), which sets it
      // (HomeDirectory() particularly)
      FLastDirectoryChange = L"";
    }

    if (OldDirectory.IsEmpty())
    {
      FLockDirectory = (SessionData->LockInHome ?
        FFileSystem->CurrentDirectory : UnicodeString(L""));
    }
    if (OldDirectory != FFileSystem->CurrentDirectory) DoChangeDirectory();
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(READ_CURRENT_DIR_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadDirectory(bool ReloadOnly, bool ForceCache)
{
  bool LoadedFromCache = false;

  if (SessionData->CacheDirectories && FDirectoryCache->HasFileList(CurrentDirectory))
  {
    if (ReloadOnly && !ForceCache)
    {
      LogEvent(L"Cached directory not reloaded.");
    }
    else
    {
      DoStartReadDirectory();
      try
      {
        LoadedFromCache = FDirectoryCache->GetFileList(CurrentDirectory, FFiles);
      }
      __finally
      {
        DoReadDirectory(ReloadOnly);
      }

      if (LoadedFromCache)
      {
        LogEvent(L"Directory content loaded from cache.");
      }
      else
      {
        LogEvent(L"Cached Directory content has been removed.");
      }
    }
  }

  if (!LoadedFromCache)
  {
    DoStartReadDirectory();
    FReadingCurrentDirectory = true;
    bool Cancel = false; // dummy
    DoReadDirectoryProgress(0, 0, Cancel);

    try
    {
      TRemoteDirectory * Files = new TRemoteDirectory(this, FFiles);
      try
      {
        Files->Directory = CurrentDirectory;
        CustomReadDirectory(Files);
      }
      __finally
      {
        DoReadDirectoryProgress(-1, 0, Cancel);
        FReadingCurrentDirectory = false;
        TRemoteDirectory * OldFiles = FFiles;
        FFiles = Files;
        try
        {
          DoReadDirectory(ReloadOnly);
        }
        __finally
        {
          // delete only after loading new files to dir view,
          // not to destroy the file objects that the view holds
          // (can be issue in multi threaded environment, such as when the
          // terminal is reconnecting in the terminal thread)
          delete OldFiles;
        }
        if (Active)
        {
          if (SessionData->CacheDirectories)
          {
            DirectoryLoaded(FFiles);
          }
        }
      }
    }
    catch (Exception &E)
    {
      CommandError(&E, FmtLoadStr(LIST_DIR_ERROR, ARRAYOFCONST((FFiles->Directory))));
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRemoteFileInfo(TRemoteFile * File)
{
  return
    FORMAT(L"%s;%s;%d;%s;%d;%s;%s;%s;%d",
      (File->FileName, File->Type, File->Size, StandardTimestamp(File->Modification), int(File->ModificationFmt),
       File->Owner.LogText, File->Group.LogText, File->Rights->Text,
       File->Attr));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogRemoteFile(TRemoteFile * File)
{
  // optimization
  if (Log->Logging)
  {
    LogEvent(GetRemoteFileInfo(File));
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::FormatFileDetailsForLog(const UnicodeString & FileName, TDateTime Modification, __int64 Size)
{
  UnicodeString Result;
    // optimization
  if (Log->Logging)
  {
    Result = FORMAT(L"'%s' [%s] [%s]", (FileName, (Modification != TDateTime() ? StandardTimestamp(Modification) : UnicodeString(L"n/a")), IntToStr(Size)));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogFileDetails(const UnicodeString & FileName, TDateTime Modification, __int64 Size)
{
  // optimization
  if (Log->Logging)
  {
    LogEvent(FORMAT("File: %s", (FormatFileDetailsForLog(FileName, Modification, Size))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LogFileDone(TFileOperationProgressType * OperationProgress)
{
  // optimization
  if (Log->Logging)
  {
    LogEvent(FORMAT("Transfer done: '%s' [%s]", (OperationProgress->FullFileName, IntToStr(OperationProgress->TransferedSize))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomReadDirectory(TRemoteFileList * FileList)
{
  DebugAssert(FileList);
  DebugAssert(FFileSystem);

  TRobustOperationLoop RobustLoop(this, OperationProgress);

  do
  {
    try
    {
      FFileSystem->ReadDirectory(FileList);
    }
    catch (Exception & E)
    {
      // Do not retry for initial listing of directory,
      // we instead retry whole connection attempt,
      // what would be done anyway (but Open is not ready for recursion).
      if ((FOpening > 0) ||
          !RobustLoop.TryReopen(E))
      {
        throw;
      }
    }
   }
  while (RobustLoop.Retry());


  if (Log->Logging)
  {
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      LogRemoteFile(FileList->Files[Index]);
    }
  }

  ReactOnCommand(fsListDirectory);
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::ReadDirectoryListing(UnicodeString Directory, const TFileMasks & Mask)
{
  TRemoteFileList * FileList;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    FileList = NULL;
    TLsSessionAction Action(ActionLog, AbsolutePath(Directory, true));

    try
    {
      FileList = DoReadDirectoryListing(Directory, false);
      if (FileList != NULL)
      {
        int Index = 0;
        while (Index < FileList->Count)
        {
          TRemoteFile * File = FileList->Files[Index];
          TFileMasks::TParams Params;
          Params.Size = File->Size;
          Params.Modification = File->Modification;
          // Have to use UnicodeString(), instead of L"", as with that
          // overload with (UnicodeString, bool, bool, TParams*) wins
          if (!Mask.Matches(File->FileName, false, UnicodeString(), &Params))
          {
            FileList->Delete(Index);
          }
          else
          {
            Index++;
          }
        }

        Action.FileList(FileList);
      }
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action);
    }
  }
  while (RetryLoop.Retry());
  return FileList;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TTerminal::ReadFileListing(UnicodeString Path)
{
  TRemoteFile * File;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    File = NULL;
    TStatSessionAction Action(ActionLog, AbsolutePath(Path, true));
    try
    {
      // reset caches
      AnnounceFileListOperation();
      ReadFile(Path, File);
      Action.File(File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action);
    }
  }
  while (RetryLoop.Retry());
  return File;
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::CustomReadDirectoryListing(UnicodeString Directory, bool UseCache)
{
  TRemoteFileList * FileList = NULL;
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FileList = DoReadDirectoryListing(Directory, UseCache);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E);
    }
  }
  while (RetryLoop.Retry());
  return FileList;
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TTerminal::DoReadDirectoryListing(UnicodeString Directory, bool UseCache)
{
  TRemoteFileList * FileList = new TRemoteFileList();
  try
  {
    bool Cache = UseCache && SessionData->CacheDirectories;
    bool LoadedFromCache = Cache && FDirectoryCache->HasFileList(Directory);
    if (LoadedFromCache)
    {
      LoadedFromCache = FDirectoryCache->GetFileList(Directory, FileList);
    }

    if (!LoadedFromCache)
    {
      FileList->Directory = Directory;

      ExceptionOnFail = true;
      try
      {
        ReadDirectory(FileList);
      }
      __finally
      {
        ExceptionOnFail = false;
      }

      if (Cache)
      {
        AddCachedFileList(FileList);
      }
    }
  }
  catch(...)
  {
    delete FileList;
    throw;
  }
  return FileList;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessDirectory(const UnicodeString DirName,
  TProcessFileEvent CallBackFunc, void * Param, bool UseCache, bool IgnoreErrors)
{
  TRemoteFileList * FileList = NULL;
  if (IgnoreErrors)
  {
    ExceptionOnFail = true;
    try
    {
      try
      {
        FileList = CustomReadDirectoryListing(DirName, UseCache);
      }
      catch(...)
      {
        if (!Active)
        {
          throw;
        }
      }
    }
    __finally
    {
      ExceptionOnFail = false;
    }
  }
  else
  {
    FileList = CustomReadDirectoryListing(DirName, UseCache);
  }

  // skip if directory listing fails and user selects "skip"
  if (FileList)
  {
    try
    {
      UnicodeString Directory = UnixIncludeTrailingBackslash(DirName);

      TRemoteFile * File;
      for (int Index = 0; Index < FileList->Count; Index++)
      {
        File = FileList->Files[Index];
        if (!File->IsParentDirectory && !File->IsThisDirectory)
        {
          CallBackFunc(Directory + File->FileName, File, Param);
          // We should catch EScpSkipFile here as we do in ProcessFiles.
          // Now we have to handle EScpSkipFile in every callback implementation.
        }
      }
    }
    __finally
    {
      delete FileList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadDirectory(TRemoteFileList * FileList)
{
  try
  {
    CustomReadDirectory(FileList);
  }
  catch (Exception &E)
  {
    CommandError(&E, FmtLoadStr(LIST_DIR_ERROR, ARRAYOFCONST((FileList->Directory))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  DebugAssert(FFileSystem);
  try
  {
    LogEvent(FORMAT(L"Reading symlink \"%s\".", (SymlinkFile->FileName)));
    FFileSystem->ReadSymlink(SymlinkFile, File);
    ReactOnCommand(fsReadSymlink);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(READ_SYMLINK_ERROR, (SymlinkFile->FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  DebugAssert(FFileSystem);
  File = NULL;
  try
  {
    LogEvent(FORMAT(L"Listing file \"%s\".", (FileName)));
    FFileSystem->ReadFile(FileName, File);
    ReactOnCommand(fsListFile);
    LogRemoteFile(File);
  }
  catch (Exception &E)
  {
    if (File) delete File;
    File = NULL;
    CommandError(&E, FMTLOAD(CANT_GET_ATTRS, (FileName)));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::FileExists(const UnicodeString FileName, TRemoteFile ** AFile)
{
  bool Result;
  TRemoteFile * File = NULL;
  try
  {
    ExceptionOnFail = true;
    try
    {
      ReadFile(UnixExcludeTrailingBackslash(FileName), File);
    }
    __finally
    {
      ExceptionOnFail = false;
    }

    if (AFile != NULL)
    {
      *AFile = File;
    }
    else
    {
      delete File;
    }
    Result = true;
  }
  catch(...)
  {
    if (Active)
    {
      Result = false;
    }
    else
    {
      throw;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AnnounceFileListOperation()
{
  FFileSystem->AnnounceFileListOperation();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::ProcessFiles(TStrings * FileList,
  TFileOperation Operation, TProcessFileEvent ProcessFile, void * Param,
  TOperationSide Side, bool Ex)
{
  DebugAssert(FFileSystem);
  DebugAssert(FileList);

  bool Result = false;
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  try
  {
    TFileOperationProgressType Progress(&DoProgress, &DoFinished);
    Progress.Start(Operation, Side, FileList->Count);

    FOperationProgress = &Progress;
    try
    {
      if (Side == osRemote)
      {
        BeginTransaction();
      }

      try
      {
        int Index = 0;
        UnicodeString FileName;
        bool Success;
        while ((Index < FileList->Count) && (Progress.Cancel == csContinue))
        {
          FileName = FileList->Strings[Index];
          try
          {
            try
            {
              Success = false;
              if (!Ex)
              {
                ProcessFile(FileName, (TRemoteFile *)FileList->Objects[Index], Param);
              }
              else
              {
                // not used anymore
                TProcessFileEventEx ProcessFileEx = (TProcessFileEventEx)ProcessFile;
                ProcessFileEx(FileName, (TRemoteFile *)FileList->Objects[Index], Param, Index);
              }
              Success = true;
            }
            __finally
            {
              Progress.Finish(FileName, Success, OnceDoneOperation);
            }
          }
          catch(EScpSkipFile & E)
          {
            TSuspendFileOperationProgress Suspend(OperationProgress);
            if (!HandleException(&E))
            {
              throw;
            }
          }
          Index++;
        }
      }
      __finally
      {
        if (Side == osRemote)
        {
          EndTransaction();
        }
      }

      if (Progress.Cancel == csContinue)
      {
        Result = true;
      }
    }
    __finally
    {
      FOperationProgress = NULL;
      Progress.Stop();
    }
  }
  catch (...)
  {
    OnceDoneOperation = odoIdle;
    // this was missing here. was it by purpose?
    // without it any error message is lost
    throw;
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }

  return Result;
}
//---------------------------------------------------------------------------
// not used anymore
bool __fastcall TTerminal::ProcessFilesEx(TStrings * FileList, TFileOperation Operation,
  TProcessFileEventEx ProcessFile, void * Param, TOperationSide Side)
{
  return ProcessFiles(FileList, Operation, TProcessFileEvent(ProcessFile),
    Param, Side, true);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TTerminal::GetFixedPaths()
{
  DebugAssert(FFileSystem != NULL);
  return FFileSystem->GetFixedPaths();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetResolvingSymlinks()
{
  return SessionData->ResolveSymlinks && IsCapable[fcResolveSymlink];
}
//---------------------------------------------------------------------------
TUsableCopyParamAttrs __fastcall TTerminal::UsableCopyParamAttrs(int Params)
{
  TUsableCopyParamAttrs Result;
  Result.General =
    FLAGMASK(!IsCapable[fcTextMode], cpaNoTransferMode) |
    FLAGMASK(!IsCapable[fcModeChanging], cpaNoRights) |
    FLAGMASK(!IsCapable[fcModeChanging], cpaNoPreserveReadOnly) |
    FLAGMASK(FLAGSET(Params, cpDelete), cpaNoClearArchive) |
    FLAGMASK(!IsCapable[fcIgnorePermErrors], cpaNoIgnorePermErrors) |
    // the following three are never supported for download,
    // so when they are not suppored for upload too,
    // set them in General flags, so that they do not get enabled on
    // Synchronize dialog.
    FLAGMASK(!IsCapable[fcModeChangingUpload], cpaNoRights) |
    FLAGMASK(!IsCapable[fcRemoveCtrlZUpload], cpaNoRemoveCtrlZ) |
    FLAGMASK(!IsCapable[fcRemoveBOMUpload], cpaNoRemoveBOM) |
    FLAGMASK(!IsCapable[fcPreservingTimestampDirs], cpaNoPreserveTimeDirs) |
    FLAGMASK(!IsCapable[fcResumeSupport], cpaNoResumeSupport);
  Result.Download = Result.General | cpaNoClearArchive |
    cpaNoIgnorePermErrors |
    // May be already set in General flags, but it's unconditional here
    cpaNoRights | cpaNoRemoveCtrlZ | cpaNoRemoveBOM;
  Result.Upload = Result.General | cpaNoPreserveReadOnly |
    FLAGMASK(!IsCapable[fcPreservingTimestampUpload], cpaNoPreserveTime);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsRecycledFile(UnicodeString FileName)
{
  bool Result = !SessionData->RecycleBinPath.IsEmpty();
  if (Result)
  {
    UnicodeString Path = UnixExtractFilePath(FileName);
    if (Path.IsEmpty())
    {
      Path = CurrentDirectory;
    }
    Result = UnixSamePath(Path, SessionData->RecycleBinPath);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RecycleFile(UnicodeString FileName,
  const TRemoteFile * File)
{
  if (FileName.IsEmpty())
  {
    DebugAssert(File != NULL);
    FileName = File->FileName;
  }

  if (!IsRecycledFile(FileName))
  {
    LogEvent(FORMAT(L"Moving file \"%s\" to remote recycle bin '%s'.",
      (FileName, SessionData->RecycleBinPath)));

    TMoveFileParams Params;
    Params.Target = SessionData->RecycleBinPath;
    Params.FileMask = FORMAT(L"*-%s.*", (FormatDateTime(L"yyyymmdd-hhnnss", Now())));

    MoveFile(FileName, File, &Params);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::TryStartOperationWithFile(
  const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2)
{
  bool Result = true;
  if ((OperationProgress != NULL) &&
      ((OperationProgress->Operation == Operation1) ||
       ((Operation2 != foNone) && (OperationProgress->Operation == Operation2))))
  {
    if (OperationProgress->Cancel != csContinue)
    {
      Result = false;
    }
    else
    {
      OperationProgress->SetFile(FileName);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::StartOperationWithFile(
  const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2)
{
  if (!TryStartOperationWithFile(FileName, Operation1, Operation2))
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteFile(UnicodeString FileName,
  const TRemoteFile * File, void * AParams)
{
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foDelete);
  int Params = (AParams != NULL) ? *((int*)AParams) : 0;
  bool Recycle =
    FLAGCLEAR(Params, dfForceDelete) &&
    (SessionData->DeleteToRecycleBin != FLAGSET(Params, dfAlternative)) &&
    !SessionData->RecycleBinPath.IsEmpty();
  if (Recycle && !IsRecycledFile(FileName))
  {
    RecycleFile(FileName, File);
  }
  else
  {
    LogEvent(FORMAT(L"Deleting file \"%s\".", (FileName)));
    FileModified(File, FileName, true);
    DoDeleteFile(FileName, File, Params);
    ReactOnCommand(fsDeleteFile);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoDeleteFile(const UnicodeString FileName,
  const TRemoteFile * File, int Params)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TRmSessionAction Action(ActionLog, AbsolutePath(FileName, true));
    try
    {
      DebugAssert(FFileSystem);
      // 'File' parameter: SFTPFileSystem needs to know if file is file or directory
      FFileSystem->DeleteFile(FileName, File, Params, Action);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(DELETE_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteFiles(TStrings * FilesToDelete, int Params)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor);
  FUseBusyCursor = false;

  // TODO: avoid resolving symlinks while reading subdirectories.
  // Resolving does not work anyway for relative symlinks in subdirectories
  // (at least for SFTP).
  return ProcessFiles(FilesToDelete, foDelete, DeleteFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteLocalFile(UnicodeString FileName,
  const TRemoteFile * /*File*/, void * Params)
{
  StartOperationWithFile(FileName, foDelete);
  if (OnDeleteLocalFile == NULL)
  {
    RecursiveDeleteFileChecked(FileName, false);
  }
  else
  {
    OnDeleteLocalFile(FileName, FLAGSET(*((int*)Params), dfAlternative));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteLocalFiles(TStrings * FileList, int Params)
{
  return ProcessFiles(FileList, foDelete, DeleteLocalFile, &Params, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFile(UnicodeString FileName,
  const TRemoteFile * File, void * AParams)
{
  TCustomCommandParams * Params = ((TCustomCommandParams *)AParams);
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foCustomCommand);
  LogEvent(FORMAT(L"Executing custom command \"%s\" (%d) on file \"%s\".",
    (Params->Command, Params->Params, FileName)));
  FileModified(File, FileName);
  DoCustomCommandOnFile(FileName, File, Params->Command, Params->Params,
    Params->OutputEvent);
  ReactOnCommand(fsAnyCommand);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCustomCommandOnFile(UnicodeString FileName,
  const TRemoteFile * File, UnicodeString Command, int Params,
  TCaptureOutputEvent OutputEvent)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      if (IsCapable[fcAnyCommand])
      {
        DebugAssert(FFileSystem);
        DebugAssert(fcShellAnyCommand);
        FFileSystem->CustomCommandOnFile(FileName, File, Command, Params, OutputEvent);
      }
      else
      {
        DebugAssert(CommandSessionOpened);
        DebugAssert(FCommandSession->FSProtocol == cfsSCP);
        LogEvent(L"Executing custom command on command session.");

        if (FCommandSession->CurrentDirectory != CurrentDirectory)
        {
          FCommandSession->CurrentDirectory = CurrentDirectory;
          // We are likely in transaction, so ReadCurrentDirectory won't get called
          // until transaction ends. But we need to know CurrentDirectory to
          // expand !/ pattern.
          // Doing this only, when current directory of the main and secondary shell differs,
          // what would be the case before the first file in transaction.
          // Otherwise we would be reading pwd before every time as the
          // CustomCommandOnFile on its own sets FReadCurrentDirectoryPending
          if (FCommandSession->FReadCurrentDirectoryPending)
          {
            FCommandSession->ReadCurrentDirectory();
          }
        }
        FCommandSession->FFileSystem->CustomCommandOnFile(FileName, File, Command,
          Params, OutputEvent);
      }
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(CUSTOM_COMMAND_ERROR, (Command, FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFiles(UnicodeString Command,
  int Params, TStrings * Files, TCaptureOutputEvent OutputEvent)
{
  if (!TRemoteCustomCommand().IsFileListCommand(Command))
  {
    TCustomCommandParams AParams;
    AParams.Command = Command;
    AParams.Params = Params;
    AParams.OutputEvent = OutputEvent;
    ProcessFiles(Files, foCustomCommand, CustomCommandOnFile, &AParams);
  }
  else
  {
    UnicodeString FileList;
    for (int i = 0; i < Files->Count; i++)
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(Files->Objects[i]);
      bool Dir = File->IsDirectory && CanRecurseToDirectory(File);

      if (!Dir || FLAGSET(Params, ccApplyToDirectories))
      {
        if (!FileList.IsEmpty())
        {
          FileList += L" ";
        }

        FileList += L"\"" + ShellDelimitStr(Files->Strings[i], L'"') + L"\"";
      }
    }

    TCustomCommandData Data(this);
    UnicodeString Cmd =
      TRemoteCustomCommand(Data, CurrentDirectory, L"", FileList).
        Complete(Command, true);
    if (!DoOnCustomCommand(Cmd))
    {
      DoAnyCommand(Cmd, OutputEvent, NULL);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoOnCustomCommand(const UnicodeString & Command)
{
  bool Result = false;
  if (FOnCustomCommand != NULL)
  {
    FOnCustomCommand(this, Command, Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFileProperties(UnicodeString FileName,
  const TRemoteFile * File, /*const TRemoteProperties*/ void * Properties)
{
  TRemoteProperties * RProperties = (TRemoteProperties *)Properties;
  DebugAssert(RProperties && !RProperties->Valid.Empty());

  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  StartOperationWithFile(FileName, foSetProperties);
  if (Log->Logging)
  {
    LogEvent(FORMAT(L"Changing properties of \"%s\" (%s)",
      (FileName, BooleanToEngStr(RProperties->Recursive))));
    if (RProperties->Valid.Contains(vpRights))
    {
      LogEvent(FORMAT(L" - mode: \"%s\"", (RProperties->Rights.ModeStr)));
    }
    if (RProperties->Valid.Contains(vpGroup))
    {
      LogEvent(FORMAT(L" - group: %s", (RProperties->Group.LogText)));
    }
    if (RProperties->Valid.Contains(vpOwner))
    {
      LogEvent(FORMAT(L" - owner: %s", (RProperties->Owner.LogText)));
    }
    if (RProperties->Valid.Contains(vpModification))
    {
      LogEvent(FORMAT(L" - modification: \"%s\"",
        (FormatDateTime(L"dddddd tt",
           UnixToDateTime(RProperties->Modification, SessionData->DSTMode)))));
    }
    if (RProperties->Valid.Contains(vpLastAccess))
    {
      LogEvent(FORMAT(L" - last access: \"%s\"",
        (FormatDateTime(L"dddddd tt",
           UnixToDateTime(RProperties->LastAccess, SessionData->DSTMode)))));
    }
  }
  FileModified(File, FileName);
  DoChangeFileProperties(FileName, File, RProperties);
  ReactOnCommand(fsChangeProperties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoChangeFileProperties(const UnicodeString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TChmodSessionAction Action(ActionLog, AbsolutePath(FileName, true));
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->ChangeFileProperties(FileName, File, Properties, Action);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(CHANGE_PROPERTIES_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFilesProperties(TStrings * FileList,
  const TRemoteProperties * Properties)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor);
  FUseBusyCursor = false;

  AnnounceFileListOperation();
  ProcessFiles(FileList, foSetProperties, ChangeFileProperties, (void *)Properties);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LoadFilesProperties(TStrings * FileList)
{
  // see comment in TSFTPFileSystem::IsCapable
  bool Result =
    IsCapable[fcLoadingAdditionalProperties] &&
    FFileSystem->LoadFilesProperties(FileList);
  if (Result && SessionData->CacheDirectories &&
      (FileList->Count > 0) &&
      (dynamic_cast<TRemoteFile *>(FileList->Objects[0])->Directory == FFiles))
  {
    AddCachedFileList(FFiles);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFileSize(UnicodeString FileName,
  const TRemoteFile * File, /*TCalculateSizeParams*/ void * Param)
{
  DebugAssert(Param);
  DebugAssert(File);
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Param);

  if (FileName.IsEmpty())
  {
    FileName = File->FileName;
  }

  if (!TryStartOperationWithFile(FileName, foCalculateSize))
  {
    AParams->Result = false;
    // Combined with csIgnoreErrors, this will not abort completelly, but we get called again
    // with next sibling of our parent directory (and we get again to this branch, throwing again)
    Abort();
  }

  bool AllowTransfer = (AParams->CopyParam == NULL);
  if (!AllowTransfer)
  {
    TFileMasks::TParams MaskParams;
    MaskParams.Size = File->Size;
    MaskParams.Modification = File->Modification;

    UnicodeString BaseFileName =
      GetBaseFileName(UnixExcludeTrailingBackslash(File->FullFileName));
    AllowTransfer = AParams->CopyParam->AllowTransfer(
      BaseFileName, osRemote, File->IsDirectory, MaskParams);
  }

  if (AllowTransfer)
  {
    if (File->IsDirectory)
    {
      if (CanRecurseToDirectory(File))
      {
        if (!AParams->AllowDirs)
        {
          AParams->Result = false;
        }
        else
        {
          LogEvent(FORMAT(L"Getting size of directory \"%s\"", (FileName)));
          // pass in full path so we get it back in file list for AllowTransfer() exclusion
          DoCalculateDirectorySize(File->FullFileName, File, AParams);
        }
      }

      if (AParams->Stats != NULL)
      {
        AParams->Stats->Directories++;
      }
    }
    else
    {
      AParams->Size += File->Size;

      if (AParams->Stats != NULL)
      {
        AParams->Stats->Files++;
      }
    }

    if ((AParams->Stats != NULL) && File->IsSymLink)
    {
      AParams->Stats->SymLinks++;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCalculateDirectorySize(const UnicodeString FileName,
  const TRemoteFile * /*File*/, TCalculateSizeParams * Params)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      ProcessDirectory(FileName, CalculateFileSize, Params);
    }
    catch(Exception & E)
    {
      // We can probably replace the csIgnoreErrors with IgnoreErrors argument of the ProcessDirectory
      if (!Active || ((Params->Params & csIgnoreErrors) == 0))
      {
        RetryLoop.Error(E, FMTLOAD(CALCULATE_SIZE_ERROR, (FileName)));
      }
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CalculateFilesSize(TStrings * FileList,
  __int64 & Size, int Params, const TCopyParamType * CopyParam,
  bool AllowDirs, TCalculateSizeStats * Stats)
{
  // With FTP protocol, we may use DSIZ command from
  // draft-peterson-streamlined-ftp-command-extensions-10
  // Implemented by Serv-U FTP.

  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor);
  FUseBusyCursor = false;

  TCalculateSizeParams Param;
  Param.Size = 0;
  Param.Params = Params;
  Param.CopyParam = CopyParam;
  Param.Stats = Stats;
  Param.AllowDirs = AllowDirs;
  Param.Result = true;
  ProcessFiles(FileList, foCalculateSize, CalculateFileSize, &Param);
  Size = Param.Size;
  return Param.Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFilesChecksum(const UnicodeString & Alg,
  TStrings * FileList, TStrings * Checksums,
  TCalculatedChecksumEvent OnCalculatedChecksum)
{
  FFileSystem->CalculateFilesChecksum(Alg, FileList, Checksums, OnCalculatedChecksum);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RenameFile(const UnicodeString FileName,
  const UnicodeString NewName)
{
  LogEvent(FORMAT(L"Renaming file \"%s\" to \"%s\".", (FileName, NewName)));
  DoRenameFile(FileName, NewName, false);
  ReactOnCommand(fsRenameFile);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RenameFile(const TRemoteFile * File,
  const UnicodeString NewName, bool CheckExistence)
{
  DebugAssert(File && File->Directory == FFiles);
  bool Proceed = true;
  // if filename doesn't contain path, we check for existence of file
  if ((File->FileName != NewName) && CheckExistence &&
      Configuration->ConfirmOverwriting &&
      UnixSamePath(CurrentDirectory, FFiles->Directory))
  {
    TRemoteFile * DuplicateFile = FFiles->FindFile(NewName);
    if (DuplicateFile)
    {
      UnicodeString QuestionFmt;
      if (DuplicateFile->IsDirectory)
      {
        QuestionFmt = LoadStr(DIRECTORY_OVERWRITE);
      }
      else
      {
        QuestionFmt = LoadStr(FILE_OVERWRITE);
      }
      TQueryParams Params(qpNeverAskAgainCheck);
      UnicodeString Question = MainInstructions(FORMAT(QuestionFmt, (NewName)));
      unsigned int Result = QueryUser(Question, NULL,
        qaYes | qaNo, &Params);
      if (Result == qaNeverAskAgain)
      {
        Proceed = true;
        Configuration->ConfirmOverwriting = false;
      }
      else
      {
        Proceed = (Result == qaYes);
      }
    }
  }

  if (Proceed)
  {
    FileModified(File, File->FileName);
    RenameFile(File->FileName, NewName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoRenameFile(const UnicodeString FileName,
  const UnicodeString NewName, bool Move)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TMvSessionAction Action(ActionLog, AbsolutePath(FileName, true), AbsolutePath(NewName, true));
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->RenameFile(FileName, NewName);
    }
    catch(Exception & E)
    {
      UnicodeString Message = FMTLOAD(Move ? MOVE_FILE_ERROR : RENAME_FILE_ERROR, (FileName, NewName));
      RetryLoop.Error(E, Action, Message);
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MoveFile(const UnicodeString FileName,
  const TRemoteFile * File, /*const TMoveFileParams*/ void * Param)
{
  StartOperationWithFile(FileName, foRemoteMove, foDelete);
  DebugAssert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  UnicodeString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT(L"Moving file \"%s\" to \"%s\".", (FileName, NewName)));
  FileModified(File, FileName);
  DoRenameFile(FileName, NewName, true);
  ReactOnCommand(fsMoveFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::MoveFiles(TStrings * FileList, const UnicodeString Target,
  const UnicodeString FileMask)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  DirectoryModified(Target, true);
  bool Result;
  BeginTransaction();
  try
  {
    Result = ProcessFiles(FileList, foRemoteMove, MoveFile, &Params);
  }
  __finally
  {
    if (Active)
    {
      UnicodeString WithTrailing = UnixIncludeTrailingBackslash(CurrentDirectory);
      bool PossiblyMoved = false;
      // check if we was moving current directory.
      // this is just optimization to avoid checking existence of current
      // directory after each move operation.
      for (int Index = 0; !PossiblyMoved && (Index < FileList->Count); Index++)
      {
        const TRemoteFile * File =
          dynamic_cast<const TRemoteFile *>(FileList->Objects[Index]);
        // File can be NULL, and filename may not be full path,
        // but currently this is the only way we can move (at least in GUI)
        // current directory
        if ((File != NULL) &&
            File->IsDirectory &&
            ((CurrentDirectory.SubString(1, FileList->Strings[Index].Length()) == FileList->Strings[Index]) &&
             ((FileList->Strings[Index].Length() == CurrentDirectory.Length()) ||
              (CurrentDirectory[FileList->Strings[Index].Length() + 1] == L'/'))))
        {
          PossiblyMoved = true;
        }
      }

      if (PossiblyMoved && !FileExists(CurrentDirectory))
      {
        UnicodeString NearestExisting = CurrentDirectory;
        do
        {
          NearestExisting = UnixExtractFileDir(NearestExisting);
        }
        while (!IsUnixRootPath(NearestExisting) && !FileExists(NearestExisting));

        ChangeDirectory(NearestExisting);
      }
    }
    EndTransaction();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCopyFile(const UnicodeString FileName,
  const UnicodeString NewName)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      DebugAssert(FFileSystem);
      if (IsCapable[fcRemoteCopy])
      {
        FFileSystem->CopyFile(FileName, NewName);
      }
      else
      {
        DebugAssert(CommandSessionOpened);
        DebugAssert(FCommandSession->FSProtocol == cfsSCP);
        LogEvent(L"Copying file on command session.");
        FCommandSession->CurrentDirectory = CurrentDirectory;
        FCommandSession->FFileSystem->CopyFile(FileName, NewName);
      }
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(COPY_FILE_ERROR, (FileName, NewName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CopyFile(const UnicodeString FileName,
  const TRemoteFile * /*File*/, /*const TMoveFileParams*/ void * Param)
{
  StartOperationWithFile(FileName, foRemoteCopy);
  DebugAssert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  UnicodeString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT(L"Copying file \"%s\" to \"%s\".", (FileName, NewName)));
  DoCopyFile(FileName, NewName);
  ReactOnCommand(fsCopyFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyFiles(TStrings * FileList, const UnicodeString Target,
  const UnicodeString FileMask)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  DirectoryModified(Target, true);
  return ProcessFiles(FileList, foRemoteCopy, CopyFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateDirectory(const UnicodeString DirName,
  const TRemoteProperties * Properties)
{
  DebugAssert(FFileSystem);
  EnsureNonExistence(DirName);
  FileModified(NULL, DirName);

  LogEvent(FORMAT(L"Creating directory \"%s\".", (DirName)));
  DoCreateDirectory(DirName);

  if ((Properties != NULL) && !Properties->Valid.Empty())
  {
    DoChangeFileProperties(DirName, NULL, Properties);
  }

  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateDirectory(const UnicodeString DirName)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    TMkdirSessionAction Action(ActionLog, AbsolutePath(DirName, true));
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->CreateDirectory(DirName);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, Action, FMTLOAD(CREATE_DIR_ERROR, (DirName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  DebugAssert(FFileSystem);
  EnsureNonExistence(FileName);
  if (SessionData->CacheDirectories)
  {
    DirectoryModified(CurrentDirectory, false);
  }

  LogEvent(FORMAT(L"Creating link \"%s\" to \"%s\" (symbolic: %s).",
    (FileName, PointTo, BooleanToEngStr(Symbolic))));
  DoCreateLink(FileName, PointTo, Symbolic);
  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateLink(const UnicodeString FileName,
  const UnicodeString PointTo, bool Symbolic)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      DebugAssert(FFileSystem);
      FFileSystem->CreateLink(FileName, PointTo, Symbolic);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(CREATE_LINK_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::HomeDirectory()
{
  DebugAssert(FFileSystem);
  try
  {
    LogEvent(L"Changing directory to home directory.");
    FFileSystem->HomeDirectory();
    ReactOnCommand(fsHomeDirectory);
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(CHANGE_HOMEDIR_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeDirectory(const UnicodeString Directory)
{
  DebugAssert(FFileSystem);
  try
  {
    UnicodeString CachedDirectory;
    DebugAssert(!SessionData->CacheDirectoryChanges || (FDirectoryChangesCache != NULL));
    // never use directory change cache during startup, this ensures, we never
    // end-up initially in non-existing directory
    if ((Status == ssOpened) &&
        SessionData->CacheDirectoryChanges &&
        FDirectoryChangesCache->GetDirectoryChange(PeekCurrentDirectory(),
          Directory, CachedDirectory))
    {
      LogEvent(FORMAT(L"Cached directory change via \"%s\" to \"%s\".",
        (Directory, CachedDirectory)));
      FFileSystem->CachedChangeDirectory(CachedDirectory);
    }
    else
    {
      LogEvent(FORMAT(L"Changing directory to \"%s\".", (Directory)));
      FFileSystem->ChangeDirectory(Directory);
    }
    FLastDirectoryChange = Directory;
    ReactOnCommand(fsChangeDirectory);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(CHANGE_DIR_ERROR, (Directory)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LookupUsersGroups()
{
  if (!FUsersGroupsLookedup && (SessionData->LookupUserGroups != asOff) &&
      IsCapable[fcUserGroupListing])
  {
    DebugAssert(FFileSystem);

    try
    {
      FUsersGroupsLookedup = true;
      LogEvent(L"Looking up groups and users.");
      FFileSystem->LookupUsersGroups();
      ReactOnCommand(fsLookupUsersGroups);

      if (Log->Logging)
      {
        FGroups.Log(this, L"groups");
        FMembership.Log(this, L"membership");
        FUsers.Log(this, L"users");
      }
    }
    catch (Exception &E)
    {
      if (!Active || (SessionData->LookupUserGroups == asOn))
      {
        CommandError(&E, LoadStr(LOOKUP_GROUPS_ERROR));
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::AllowedAnyCommand(const UnicodeString Command)
{
  return !Command.Trim().IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetCommandSessionOpened()
{
  // consider secondary terminal open in "ready" state only
  // so we never do keepalives on it until it is completely initialized
  return (FCommandSession != NULL) &&
    (FCommandSession->Status == ssOpened);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminal::GetCommandSession()
{
  if ((FCommandSession != NULL) && !FCommandSession->Active)
  {
    SAFE_DESTROY(FCommandSession);
  }

  if (FCommandSession == NULL)
  {
    // transaction cannot be started yet to allow proper matching transaction
    // levels between main and command session
    DebugAssert(FInTransaction == 0);

    try
    {
      FCommandSession = new TSecondaryTerminal(this, SessionData,
        Configuration, L"Shell");

      FCommandSession->AutoReadDirectory = false;

      TSessionData * CommandSessionData = FCommandSession->FSessionData;
      CommandSessionData->RemoteDirectory = CurrentDirectory;
      CommandSessionData->FSProtocol = fsSCPonly;
      CommandSessionData->ClearAliases = false;
      CommandSessionData->UnsetNationalVars = false;
      CommandSessionData->LookupUserGroups = asOff;

      FCommandSession->FExceptionOnFail = FExceptionOnFail;

      FCommandSession->OnQueryUser = OnQueryUser;
      FCommandSession->OnPromptUser = OnPromptUser;
      FCommandSession->OnShowExtendedException = OnShowExtendedException;
      FCommandSession->OnProgress = OnProgress;
      FCommandSession->OnFinished = OnFinished;
      FCommandSession->OnInformation = OnInformation;
      FCommandSession->OnCustomCommand = OnCustomCommand;
      FCommandSession->OnClose = CommandSessionClose;
      // do not copy OnDisplayBanner to avoid it being displayed
    }
    catch(...)
    {
      SAFE_DESTROY(FCommandSession);
      throw;
    }
  }

  return FCommandSession;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CommandSessionClose(TObject * /*Sender*/)
{
  // Keep the states in sync.
  // This is particularly to invoke ours OnClose,
  // So that it is triggered before Reopen is called
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{

  #pragma warn -inl
  class TOutputProxy
  {
  public:
    __fastcall TOutputProxy(TCallSessionAction & Action, TCaptureOutputEvent OutputEvent) :
      FAction(Action),
      FOutputEvent(OutputEvent)
    {
    }

    void __fastcall Output(const UnicodeString & Str, TCaptureOutputType OutputType)
    {
      switch (OutputType)
      {
        case cotOutput:
          FAction.AddOutput(Str, false);
          break;
        case cotError:
          FAction.AddOutput(Str, true);
          break;
        case cotExitCode:
          FAction.ExitCode(StrToInt(Str));
          break;
      }

      if (FOutputEvent != NULL)
      {
        FOutputEvent(Str, OutputType);
      }
    }

  private:
    TCallSessionAction & FAction;
    TCaptureOutputEvent FOutputEvent;
  };
  #pragma warn .inl

  TCallSessionAction Action(ActionLog, Command, CurrentDirectory);
  TOutputProxy ProxyOutputEvent(Action, OutputEvent);
  DoAnyCommand(Command, ProxyOutputEvent.Output, &Action);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoAnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent, TCallSessionAction * Action)
{
  DebugAssert(FFileSystem);
  try
  {
    DirectoryModified(CurrentDirectory, false);
    if (IsCapable[fcAnyCommand])
    {
      LogEvent(L"Executing user defined command.");
      FFileSystem->AnyCommand(Command, OutputEvent);
    }
    else
    {
      DebugAssert(CommandSessionOpened);
      DebugAssert(FCommandSession->FSProtocol == cfsSCP);
      LogEvent(L"Executing user defined command on command session.");

      FCommandSession->CurrentDirectory = CurrentDirectory;
      FCommandSession->FFileSystem->AnyCommand(Command, OutputEvent);

      FCommandSession->FFileSystem->ReadCurrentDirectory();

      // synchronize pwd (by purpose we lose transaction optimization here)
      ChangeDirectory(FCommandSession->CurrentDirectory);
    }
    ReactOnCommand(fsAnyCommand);
  }
  catch (Exception &E)
  {
    if (Action != NULL)
    {
      RollbackAction(*Action, NULL, &E);
    }
    if (ExceptionOnFail || (E.InheritsFrom(__classid(EFatal)))) throw;
      else HandleExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoCreateLocalFile(const UnicodeString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
  bool NoConfirmation)
{
  bool Result = true;
  bool Done;
  unsigned int CreateAttr = FILE_ATTRIBUTE_NORMAL;
  do
  {
    *AHandle = CreateFile(ApiPath(FileName).c_str(), GENERIC_WRITE, FILE_SHARE_READ,
      NULL, CREATE_ALWAYS, CreateAttr, 0);
    Done = (*AHandle != INVALID_HANDLE_VALUE);
    if (!Done)
    {
      // save the error, otherwise it gets overwritten by call to FileExists
      int LastError = GetLastError();
      int FileAttr;
      if (::FileExists(ApiPath(FileName)) &&
        (((FileAttr = FileGetAttrFix(ApiPath(FileName))) & (faReadOnly | faHidden)) != 0))
      {
        if (FLAGSET(FileAttr, faReadOnly))
        {
          if (OperationProgress->BatchOverwrite == boNone)
          {
            Result = false;
          }
          else if ((OperationProgress->BatchOverwrite != boAll) && !NoConfirmation)
          {
            unsigned int Answer;

            {
              TSuspendFileOperationProgress Suspend(OperationProgress);
              Answer = QueryUser(
                MainInstructions(FMTLOAD(READ_ONLY_OVERWRITE, (FileName))), NULL,
                qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll, 0);
            }

            switch (Answer) {
              case qaYesToAll: OperationProgress->BatchOverwrite = boAll; break;
              case qaCancel: OperationProgress->Cancel = csCancel; // continue on next case
              case qaNoToAll: OperationProgress->BatchOverwrite = boNone;
              case qaNo: Result = false; break;
            }
          }
        }
        else
        {
          DebugAssert(FLAGSET(FileAttr, faHidden));
          Result = true;
        }

        if (Result)
        {
          CreateAttr |=
            FLAGMASK(FLAGSET(FileAttr, faHidden), FILE_ATTRIBUTE_HIDDEN) |
            FLAGMASK(FLAGSET(FileAttr, faReadOnly), FILE_ATTRIBUTE_READONLY);

          FILE_OPERATION_LOOP_BEGIN
          {
            if (FileSetAttr(ApiPath(FileName), FileAttr & ~(faReadOnly | faHidden)) != 0)
            {
              RaiseLastOSError();
            }
          }
          FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (FileName)));
        }
        else
        {
          Done = true;
        }
      }
      else
      {
        RaiseLastOSError(LastError);
      }
    }
  }
  while (!Done);

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CreateLocalFile(const UnicodeString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
  bool NoConfirmation)
{
  DebugAssert(AHandle);
  bool Result = true;

  FILE_OPERATION_LOOP_BEGIN
  {
    Result = DoCreateLocalFile(FileName, OperationProgress, AHandle, NoConfirmation);
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CREATE_FILE_ERROR, (FileName)));

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenLocalFile(const UnicodeString FileName,
  unsigned int Access, int * AAttrs, HANDLE * AHandle, __int64 * ACTime,
  __int64 * AMTime, __int64 * AATime, __int64 * ASize,
  bool TryWriteReadOnly)
{
  int Attrs = 0;
  HANDLE Handle = 0;

  FILE_OPERATION_LOOP_BEGIN
  {
    UnicodeString FileNameApi = ApiPath(FileName);
    Attrs = FileGetAttrFix(FileNameApi);
    if (Attrs < 0)
    {
      RaiseLastOSError();
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(FILE_NOT_EXISTS, (FileName)));

  if (FLAGCLEAR(Attrs, faDirectory) || (AHandle == NULL))
  {
    bool NoHandle = false;
    if (!TryWriteReadOnly && (Access == GENERIC_WRITE) &&
        ((Attrs & faReadOnly) != 0))
    {
      Access = GENERIC_READ;
      NoHandle = true;
    }

    FILE_OPERATION_LOOP_BEGIN
    {
      DWORD Flags = FLAGMASK(FLAGSET(Attrs, faDirectory), FILE_FLAG_BACKUP_SEMANTICS);
      Handle = CreateFile(ApiPath(FileName).c_str(), Access,
        Access == GENERIC_READ ? FILE_SHARE_READ | FILE_SHARE_WRITE : FILE_SHARE_READ,
        NULL, OPEN_EXISTING, Flags, 0);
      if (Handle == INVALID_HANDLE_VALUE)
      {
        Handle = 0;
        RaiseLastOSError();
      }
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(OPENFILE_ERROR, (FileName)));

    try
    {
      if (AATime || AMTime || ACTime)
      {
        FILETIME ATime;
        FILETIME MTime;
        FILETIME CTime;

        // Get last file access and modification time
        FILE_OPERATION_LOOP_BEGIN
        {
          THROWOSIFFALSE(GetFileTime(Handle, &CTime, &ATime, &MTime));
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(CANT_GET_ATTRS, (FileName)));

        if (ACTime)
        {
          *ACTime = ConvertTimestampToUnixSafe(CTime, SessionData->DSTMode);
        }
        if (AATime)
        {
          *AATime = ConvertTimestampToUnixSafe(ATime, SessionData->DSTMode);
        }
        if (AMTime)
        {
          *AMTime = ConvertTimestampToUnix(MTime, SessionData->DSTMode);
        }
      }

      if (ASize)
      {
        // Get file size
        FILE_OPERATION_LOOP_BEGIN
        {
          unsigned long LSize;
          unsigned long HSize;
          LSize = GetFileSize(Handle, &HSize);
          if ((LSize == 0xFFFFFFFF) && (GetLastError() != NO_ERROR)) RaiseLastOSError();
          *ASize = (__int64(HSize) << 32) + LSize;
        }
        FILE_OPERATION_LOOP_END(FMTLOAD(CANT_GET_ATTRS, (FileName)));
      }

      if ((AHandle == NULL) || NoHandle)
      {
        CloseHandle(Handle);
        Handle = NULL;
      }
    }
    catch(...)
    {
      CloseHandle(Handle);
      throw;
    }
  }

  if (AAttrs) *AAttrs = Attrs;
  if (AHandle) *AHandle = Handle;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::AllowLocalFileTransfer(UnicodeString FileName,
  const TCopyParamType * CopyParam, TFileOperationProgressType * OperationProgress)
{
  bool Result = true;
  // optimization
  if (Log->Logging || !CopyParam->AllowAnyTransfer())
  {
    WIN32_FIND_DATA FindData;
    HANDLE Handle;
    FILE_OPERATION_LOOP_BEGIN
    {
      Handle = FindFirstFile(ApiPath(ExcludeTrailingBackslash(FileName)).c_str(), &FindData);
      if (Handle == INVALID_HANDLE_VALUE)
      {
        RaiseLastOSError();
      }
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
    ::FindClose(Handle);
    bool Directory = FLAGSET(FindData.dwFileAttributes, FILE_ATTRIBUTE_DIRECTORY);
    TFileMasks::TParams Params;
    // SearchRec.Size in C++B2010 is __int64,
    // so we should be able to use it instead of FindData.nFileSize*
    Params.Size =
      (static_cast<__int64>(FindData.nFileSizeHigh) << 32) +
      FindData.nFileSizeLow;
    Params.Modification = FileTimeToDateTime(FindData.ftLastWriteTime);
    UnicodeString BaseFileName = GetBaseFileName(FileName);
    if (!CopyParam->AllowTransfer(BaseFileName, osLocal, Directory, Params))
    {
      LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
      Result = false;
    }
    else if (CopyParam->SkipTransfer(FileName, Directory))
    {
      OperationProgress->AddSkippedFileSize(Params.Size);
      Result = false;
    }

    if (Result)
    {
      LogFileDetails(FileName, Params.Modification, Params.Size);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MakeLocalFileList(const UnicodeString FileName,
  const TSearchRec Rec, void * Param)
{
  TMakeLocalFileListParams & Params = *static_cast<TMakeLocalFileListParams*>(Param);

  bool Directory = FLAGSET(Rec.Attr, faDirectory);
  if (Directory && Params.Recursive)
  {
    ProcessLocalDirectory(FileName, MakeLocalFileList, &Params);
  }

  if (!Directory || Params.IncludeDirs)
  {
    Params.FileList->Add(FileName);
    if (Params.FileTimes != NULL)
    {
      Params.FileTimes->push_back(const_cast<TSearchRec &>(Rec).TimeStamp);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateLocalFileSize(const UnicodeString FileName,
  const TSearchRec Rec, /*TCalculateSizeParams*/ void * Params)
{
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Params);

  if (!TryStartOperationWithFile(FileName, foCalculateSize))
  {
    AParams->Result = false;
  }
  else
  {
    try
    {
      bool Dir = FLAGSET(Rec.Attr, faDirectory);

      bool AllowTransfer = (AParams->CopyParam == NULL);
      // SearchRec.Size in C++B2010 is __int64,
      // so we should be able to use it instead of FindData.nFileSize*
      __int64 Size =
        (static_cast<__int64>(Rec.FindData.nFileSizeHigh) << 32) +
        Rec.FindData.nFileSizeLow;
      if (!AllowTransfer)
      {
        TFileMasks::TParams MaskParams;
        MaskParams.Size = Size;
        MaskParams.Modification = FileTimeToDateTime(Rec.FindData.ftLastWriteTime);

        UnicodeString BaseFileName = GetBaseFileName(FileName);
        AllowTransfer = AParams->CopyParam->AllowTransfer(BaseFileName, osLocal, Dir, MaskParams);
      }

      if (AllowTransfer)
      {
        if (!Dir)
        {
          AParams->Size += Size;
        }
        else
        {
          ProcessLocalDirectory(FileName, CalculateLocalFileSize, Params);
        }
      }
    }
    catch (...)
    {
      // ignore
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CalculateLocalFilesSize(TStrings * FileList,
  __int64 & Size, const TCopyParamType * CopyParam, bool AllowDirs)
{
  bool Result = false;
  TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);
  TOnceDoneOperation OnceDoneOperation = odoIdle;
  OperationProgress.Start(foCalculateSize, osLocal, FileList->Count);
  try
  {
    TCalculateSizeParams Params;
    Params.Size = 0;
    Params.Params = 0;
    Params.CopyParam = CopyParam;
    Params.Result = true;

    DebugAssert(!FOperationProgress);
    FOperationProgress = &OperationProgress;
    for (int Index = 0; Params.Result && (Index < FileList->Count); Index++)
    {
      UnicodeString FileName = FileList->Strings[Index];
      TSearchRec Rec;
      if (FileSearchRec(FileName, Rec))
      {
        if (FLAGSET(Rec.Attr, faDirectory) && !AllowDirs)
        {
          Params.Result = false;
        }
        else
        {
          CalculateLocalFileSize(FileName, Rec, &Params);
          OperationProgress.Finish(FileName, true, OnceDoneOperation);
        }
      }
    }

    Size = Params.Size;
    Result = Params.Result;
  }
  __finally
  {
    FOperationProgress = NULL;
    OperationProgress.Stop();
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }
  return Result;
}
//---------------------------------------------------------------------------
struct TSynchronizeFileData
{
  bool Modified;
  bool New;
  bool IsDirectory;
  TSynchronizeChecklist::TItem::TFileInfo Info;
  TSynchronizeChecklist::TItem::TFileInfo MatchingRemoteFile;
  TRemoteFile * MatchingRemoteFileFile;
  int MatchingRemoteFileImageIndex;
  FILETIME LocalLastWriteTime;
};
//---------------------------------------------------------------------------
const int sfFirstLevel = 0x01;
struct TSynchronizeData
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;
  TTerminal::TSynchronizeMode Mode;
  int Params;
  TSynchronizeDirectory OnSynchronizeDirectory;
  TSynchronizeOptions * Options;
  int Flags;
  TStringList * LocalFileList;
  const TCopyParamType * CopyParam;
  TSynchronizeChecklist * Checklist;
};
//---------------------------------------------------------------------------
TSynchronizeChecklist * __fastcall TTerminal::SynchronizeCollect(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory,
  TSynchronizeOptions * Options)
{
  TValueRestorer<bool> UseBusyCursorRestorer(FUseBusyCursor);
  FUseBusyCursor = false;

  TSynchronizeChecklist * Checklist = new TSynchronizeChecklist();
  try
  {
    DoSynchronizeCollectDirectory(LocalDirectory, RemoteDirectory, Mode,
      CopyParam, Params, OnSynchronizeDirectory, Options, sfFirstLevel,
      Checklist);
    Checklist->Sort();
  }
  catch(...)
  {
    delete Checklist;
    throw;
  }
  return Checklist;
}
//---------------------------------------------------------------------------
static void __fastcall AddFlagName(UnicodeString & ParamsStr, int & Params, int Param, const UnicodeString & Name)
{
  if (FLAGSET(Params, Param))
  {
    AddToList(ParamsStr, Name, ", ");
  }
  Params &= ~Param;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::SynchronizeModeStr(TSynchronizeMode Mode)
{
  UnicodeString ModeStr;
  switch (Mode)
  {
    case smRemote:
      ModeStr = L"Remote";
      break;
    case smLocal:
      ModeStr = L"Local";
      break;
    case smBoth:
      ModeStr = L"Both";
      break;
    default:
      ModeStr = L"Unknown";
      break;
  }
  return ModeStr;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::SynchronizeParamsStr(int Params)
{
  UnicodeString ParamsStr;
  AddFlagName(ParamsStr, Params, spDelete, L"Delete");
  AddFlagName(ParamsStr, Params, spNoConfirmation, L"NoConfirmation");
  AddFlagName(ParamsStr, Params, spExistingOnly, L"ExistingOnly");
  AddFlagName(ParamsStr, Params, spNoRecurse, L"NoRecurse");
  AddFlagName(ParamsStr, Params, spUseCache, L"UseCache");
  AddFlagName(ParamsStr, Params, spDelayProgress, L"DelayProgress");
  AddFlagName(ParamsStr, Params, spPreviewChanges, L"*PreviewChanges"); // GUI only
  AddFlagName(ParamsStr, Params, spSubDirs, L"SubDirs");
  AddFlagName(ParamsStr, Params, spTimestamp, L"Timestamp");
  AddFlagName(ParamsStr, Params, spNotByTime, L"NotByTime");
  AddFlagName(ParamsStr, Params, spBySize, L"BySize");
  AddFlagName(ParamsStr, Params, spSelectedOnly, L"*SelectedOnly"); // GUI only
  AddFlagName(ParamsStr, Params, spMirror, L"Mirror");
  if (Params > 0)
  {
    AddToList(ParamsStr, FORMAT(L"0x%x", (int(Params))), L", ");
  }
  return ParamsStr;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeCollectDirectory(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory, TSynchronizeOptions * Options,
  int Flags, TSynchronizeChecklist * Checklist)
{
  TSynchronizeData Data;

  Data.LocalDirectory = IncludeTrailingBackslash(LocalDirectory);
  Data.RemoteDirectory = UnixIncludeTrailingBackslash(RemoteDirectory);
  Data.Mode = Mode;
  Data.Params = Params;
  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;
  Data.LocalFileList = NULL;
  Data.CopyParam = CopyParam;
  Data.Options = Options;
  Data.Flags = Flags;
  Data.Checklist = Checklist;

  LogEvent(FORMAT(L"Collecting synchronization list for local directory '%s' and remote directory '%s', "
    "mode = %s, params = 0x%x (%s)", (LocalDirectory, RemoteDirectory,
    SynchronizeModeStr(Mode), int(Params), SynchronizeParamsStr(Params))));

  if (FLAGCLEAR(Params, spDelayProgress))
  {
    DoSynchronizeProgress(Data, true);
  }

  try
  {
    bool Found;
    TSearchRecChecked SearchRec;
    Data.LocalFileList = CreateSortedStringList();

    FILE_OPERATION_LOOP_BEGIN
    {
      int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
      Found = (FindFirstChecked(Data.LocalDirectory + L"*.*", FindAttrs, SearchRec) == 0);
    }
    FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (LocalDirectory)));

    if (Found)
    {
      try
      {
        UnicodeString FileName;
        while (Found)
        {
          FileName = SearchRec.Name;
          // add dirs for recursive mode or when we are interested in newly
          // added subdirs
          // SearchRec.Size in C++B2010 is __int64,
          // so we should be able to use it instead of FindData.nFileSize*
          __int64 Size =
            (static_cast<__int64>(SearchRec.FindData.nFileSizeHigh) << 32) +
            SearchRec.FindData.nFileSizeLow;
          TDateTime Modification = FileTimeToDateTime(SearchRec.FindData.ftLastWriteTime);
          TFileMasks::TParams MaskParams;
          MaskParams.Size = Size;
          MaskParams.Modification = Modification;
          UnicodeString RemoteFileName =
            ChangeFileName(CopyParam, FileName, osLocal, false);
          UnicodeString FullLocalFileName = Data.LocalDirectory + FileName;
          UnicodeString BaseFileName = GetBaseFileName(FullLocalFileName);
          if ((FileName != L".") && (FileName != L"..") &&
              CopyParam->AllowTransfer(BaseFileName, osLocal,
                FLAGSET(SearchRec.Attr, faDirectory), MaskParams) &&
              !FFileSystem->TemporaryTransferFile(FileName) &&
              (FLAGCLEAR(Flags, sfFirstLevel) ||
               (Options == NULL) ||
               Options->MatchesFilter(FileName) ||
               Options->MatchesFilter(RemoteFileName)))
          {
            TSynchronizeFileData * FileData = new TSynchronizeFileData;

            FileData->IsDirectory = FLAGSET(SearchRec.Attr, faDirectory);
            FileData->Info.FileName = FileName;
            FileData->Info.Directory = Data.LocalDirectory;
            FileData->Info.Modification = Modification;
            FileData->Info.ModificationFmt = mfFull;
            FileData->Info.Size = Size;
            FileData->LocalLastWriteTime = SearchRec.FindData.ftLastWriteTime;
            FileData->New = true;
            FileData->Modified = false;
            Data.LocalFileList->AddObject(FileName,
              reinterpret_cast<TObject*>(FileData));
            LogEvent(FORMAT(L"Local file %s included to synchronization",
              (FormatFileDetailsForLog(FullLocalFileName, Modification, Size))));
          }
          else
          {
            LogEvent(FORMAT(L"Local file %s excluded from synchronization",
              (FormatFileDetailsForLog(FullLocalFileName, Modification, Size))));
          }

          FILE_OPERATION_LOOP_BEGIN
          {
            Found = (FindNextChecked(SearchRec) == 0);
          }
          FILE_OPERATION_LOOP_END(FMTLOAD(LIST_DIR_ERROR, (LocalDirectory)));
        }
      }
      __finally
      {
        FindClose(SearchRec);
      }

      // can we expect that ProcessDirectory would take so little time
      // that we can postpone showing progress window until anything actually happens?
      bool Cached = FLAGSET(Params, spUseCache) && SessionData->CacheDirectories &&
        FDirectoryCache->HasFileList(RemoteDirectory);

      if (!Cached && FLAGSET(Params, spDelayProgress))
      {
        DoSynchronizeProgress(Data, true);
      }

      ProcessDirectory(RemoteDirectory, SynchronizeCollectFile, &Data,
        FLAGSET(Params, spUseCache));

      TSynchronizeFileData * FileData;
      for (int Index = 0; Index < Data.LocalFileList->Count; Index++)
      {
        FileData = reinterpret_cast<TSynchronizeFileData *>
          (Data.LocalFileList->Objects[Index]);
        // add local file either if we are going to upload it
        // (i.e. if it is updated or we want to upload even new files)
        // or if we are going to delete it (i.e. all "new"=obsolete files)
        bool Modified = (FileData->Modified && ((Mode == smBoth) || (Mode == smRemote)));
        bool New = (FileData->New &&
          ((Mode == smLocal) ||
           (((Mode == smBoth) || (Mode == smRemote)) && FLAGCLEAR(Params, spTimestamp))));

        if (New)
        {
          LogEvent(FORMAT(L"Local file %s is new",
            (FormatFileDetailsForLog(FileData->Info.Directory + FileData->Info.FileName,
             FileData->Info.Modification, FileData->Info.Size))));
        }

        if (Modified || New)
        {
          TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
          try
          {
            ChecklistItem->IsDirectory = FileData->IsDirectory;

            ChecklistItem->Local = FileData->Info;
            ChecklistItem->FLocalLastWriteTime = FileData->LocalLastWriteTime;

            if (Modified)
            {
              DebugAssert(!FileData->MatchingRemoteFile.Directory.IsEmpty());
              ChecklistItem->Remote = FileData->MatchingRemoteFile;
              ChecklistItem->ImageIndex = FileData->MatchingRemoteFileImageIndex;
              ChecklistItem->RemoteFile = FileData->MatchingRemoteFileFile;
            }
            else
            {
              ChecklistItem->Remote.Directory = Data.RemoteDirectory;
            }

            if ((Mode == smBoth) || (Mode == smRemote))
            {
              ChecklistItem->Action =
                (Modified ? TSynchronizeChecklist::saUploadUpdate : TSynchronizeChecklist::saUploadNew);
              ChecklistItem->Checked =
                (Modified || FLAGCLEAR(Params, spExistingOnly)) &&
                (!ChecklistItem->IsDirectory || FLAGCLEAR(Params, spNoRecurse) ||
                 FLAGSET(Params, spSubDirs));
            }
            else if ((Mode == smLocal) && FLAGCLEAR(Params, spTimestamp))
            {
              ChecklistItem->Action = TSynchronizeChecklist::saDeleteLocal;
              ChecklistItem->Checked =
                FLAGSET(Params, spDelete) &&
                (!ChecklistItem->IsDirectory || FLAGCLEAR(Params, spNoRecurse) ||
                 FLAGSET(Params, spSubDirs));
            }

            if (ChecklistItem->Action != TSynchronizeChecklist::saNone)
            {
              Data.Checklist->Add(ChecklistItem);
              ChecklistItem = NULL;
            }
          }
          __finally
          {
            delete ChecklistItem;
          }
        }
        else
        {
          if (FileData->Modified)
          {
            delete FileData->MatchingRemoteFileFile;
          }
        }
      }
    }
  }
  __finally
  {
    if (Data.LocalFileList != NULL)
    {
      for (int Index = 0; Index < Data.LocalFileList->Count; Index++)
      {
        TSynchronizeFileData * FileData = reinterpret_cast<TSynchronizeFileData*>
          (Data.LocalFileList->Objects[Index]);
        delete FileData;
      }
      delete Data.LocalFileList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeCollectFile(const UnicodeString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  try
  {
    DoSynchronizeCollectFile(FileName, File, Param);
  }
  catch(EScpSkipFile & E)
  {
    TSuspendFileOperationProgress Suspend(OperationProgress);
    if (!HandleException(&E))
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeCollectFile(const UnicodeString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  TSynchronizeData * Data = static_cast<TSynchronizeData *>(Param);

  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Size;
  MaskParams.Modification = File->Modification;
  UnicodeString LocalFileName =
    ChangeFileName(Data->CopyParam, File->FileName, osRemote, false);
  UnicodeString FullRemoteFileName =
    UnixExcludeTrailingBackslash(File->FullFileName);
  UnicodeString BaseFileName = GetBaseFileName(FullRemoteFileName);
  if (Data->CopyParam->AllowTransfer(
        BaseFileName, osRemote,
        File->IsDirectory, MaskParams) &&
      !FFileSystem->TemporaryTransferFile(File->FileName) &&
      (FLAGCLEAR(Data->Flags, sfFirstLevel) ||
       (Data->Options == NULL) ||
        Data->Options->MatchesFilter(File->FileName) ||
        Data->Options->MatchesFilter(LocalFileName)))
  {
    TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
    try
    {
      ChecklistItem->IsDirectory = File->IsDirectory;
      ChecklistItem->ImageIndex = File->IconIndex;

      ChecklistItem->Remote.FileName = File->FileName;
      ChecklistItem->Remote.Directory = Data->RemoteDirectory;
      ChecklistItem->Remote.Modification = File->Modification;
      ChecklistItem->Remote.ModificationFmt = File->ModificationFmt;
      ChecklistItem->Remote.Size = File->Size;

      bool Modified = false;
      bool New = false;
      if (File->IsDirectory && !CanRecurseToDirectory(File))
      {
        LogEvent(FORMAT(L"Skipping symlink to directory \"%s\".", (File->FileName)));
      }
      else
      {
        int LocalIndex = Data->LocalFileList->IndexOf(LocalFileName);
        New = (LocalIndex < 0);
        if (!New)
        {
          TSynchronizeFileData * LocalData =
            reinterpret_cast<TSynchronizeFileData *>(Data->LocalFileList->Objects[LocalIndex]);

          LocalData->New = false;

          if (File->IsDirectory != LocalData->IsDirectory)
          {
            LogEvent(FORMAT(L"%s is directory on one side, but file on the another",
              (File->FileName)));
          }
          else if (!File->IsDirectory)
          {
            ChecklistItem->Local = LocalData->Info;

            ChecklistItem->Local.Modification =
              ReduceDateTimePrecision(ChecklistItem->Local.Modification, File->ModificationFmt);

            bool LocalModified = false;
            // for spTimestamp+spBySize require that the file sizes are the same
            // before comparing file time
            int TimeCompare;
            if (FLAGCLEAR(Data->Params, spNotByTime) &&
                (FLAGCLEAR(Data->Params, spTimestamp) ||
                 FLAGCLEAR(Data->Params, spBySize) ||
                 (ChecklistItem->Local.Size == ChecklistItem->Remote.Size)))
            {
              TimeCompare = CompareFileTime(ChecklistItem->Local.Modification,
                   ChecklistItem->Remote.Modification);
            }
            else
            {
              TimeCompare = 0;
            }
            if (TimeCompare < 0)
            {
              if ((FLAGCLEAR(Data->Params, spTimestamp) && FLAGCLEAR(Data->Params, spMirror)) ||
                  (Data->Mode == smBoth) || (Data->Mode == smLocal))
              {
                Modified = true;
              }
              else
              {
                LocalModified = true;
              }
            }
            else if (TimeCompare > 0)
            {
              if ((FLAGCLEAR(Data->Params, spTimestamp) && FLAGCLEAR(Data->Params, spMirror)) ||
                  (Data->Mode == smBoth) || (Data->Mode == smRemote))
              {
                LocalModified = true;
              }
              else
              {
                Modified = true;
              }
            }
            else if (FLAGSET(Data->Params, spBySize) &&
                     (ChecklistItem->Local.Size != ChecklistItem->Remote.Size) &&
                     FLAGCLEAR(Data->Params, spTimestamp))
            {
              Modified = true;
              LocalModified = true;
            }

            if (LocalModified)
            {
              LocalData->Modified = true;
              LocalData->MatchingRemoteFile = ChecklistItem->Remote;
              LocalData->MatchingRemoteFileImageIndex = ChecklistItem->ImageIndex;
              // we need this for custom commands over checklist only,
              // not for sync itself
              LocalData->MatchingRemoteFileFile = File->Duplicate();
              LogEvent(FORMAT(L"Local file %s is modified comparing to remote file %s",
                (FormatFileDetailsForLog(LocalData->Info.Directory + LocalData->Info.FileName,
                   LocalData->Info.Modification, LocalData->Info.Size),
                 FormatFileDetailsForLog(FullRemoteFileName,
                   File->Modification, File->Size))));
            }

            if (Modified)
            {
              LogEvent(FORMAT(L"Remote file %s is modified comparing to local file %s",
                (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size),
                 FormatFileDetailsForLog(LocalData->Info.Directory + LocalData->Info.FileName,
                   LocalData->Info.Modification, LocalData->Info.Size))));
            }
          }
          else if (FLAGCLEAR(Data->Params, spNoRecurse))
          {
            DoSynchronizeCollectDirectory(
              Data->LocalDirectory + LocalData->Info.FileName,
              Data->RemoteDirectory + File->FileName,
              Data->Mode, Data->CopyParam, Data->Params, Data->OnSynchronizeDirectory,
              Data->Options, (Data->Flags & ~sfFirstLevel),
              Data->Checklist);
          }
        }
        else
        {
          ChecklistItem->Local.Directory = Data->LocalDirectory;
          LogEvent(FORMAT(L"Remote file %s is new",
            (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size))));
        }
      }

      if (New || Modified)
      {
        DebugAssert(!New || !Modified);

        // download the file if it changed or is new and we want to have it locally
        if ((Data->Mode == smBoth) || (Data->Mode == smLocal))
        {
          if (FLAGCLEAR(Data->Params, spTimestamp) || Modified)
          {
            ChecklistItem->Action =
              (Modified ? TSynchronizeChecklist::saDownloadUpdate : TSynchronizeChecklist::saDownloadNew);
            ChecklistItem->Checked =
              (Modified || FLAGCLEAR(Data->Params, spExistingOnly)) &&
              (!ChecklistItem->IsDirectory || FLAGCLEAR(Data->Params, spNoRecurse) ||
               FLAGSET(Data->Params, spSubDirs));
          }
        }
        else if ((Data->Mode == smRemote) && New)
        {
          if (FLAGCLEAR(Data->Params, spTimestamp))
          {
            ChecklistItem->Action = TSynchronizeChecklist::saDeleteRemote;
            ChecklistItem->Checked =
              FLAGSET(Data->Params, spDelete) &&
              (!ChecklistItem->IsDirectory || FLAGCLEAR(Data->Params, spNoRecurse) ||
               FLAGSET(Data->Params, spSubDirs));
          }
        }

        if (ChecklistItem->Action != TSynchronizeChecklist::saNone)
        {
          ChecklistItem->RemoteFile = File->Duplicate();
          Data->Checklist->Add(ChecklistItem);
          ChecklistItem = NULL;
        }
      }
    }
    __finally
    {
      delete ChecklistItem;
    }
  }
  else
  {
    LogEvent(FORMAT(L"Remote file %s excluded from synchronization",
      (FormatFileDetailsForLog(FullRemoteFileName, File->Modification, File->Size))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeApply(TSynchronizeChecklist * Checklist,
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory)
{
  TSynchronizeData Data;

  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;

  int CopyParams =
    FLAGMASK(FLAGSET(Params, spNoConfirmation), cpNoConfirmation);

  TCopyParamType SyncCopyParam = *CopyParam;
  // when synchronizing by time, we force preserving time,
  // otherwise it does not make any sense
  if (FLAGCLEAR(Params, spNotByTime))
  {
    SyncCopyParam.PreserveTime = true;
  }

  TStringList * DownloadList = new TStringList();
  TStringList * DeleteRemoteList = new TStringList();
  TStringList * UploadList = new TStringList();
  TStringList * DeleteLocalList = new TStringList();

  BeginTransaction();

  try
  {
    int IIndex = 0;
    while (IIndex < Checklist->Count)
    {
      const TSynchronizeChecklist::TItem * ChecklistItem;

      DownloadList->Clear();
      DeleteRemoteList->Clear();
      UploadList->Clear();
      DeleteLocalList->Clear();

      ChecklistItem = Checklist->Item[IIndex];

      UnicodeString CurrentLocalDirectory = ChecklistItem->Local.Directory;
      UnicodeString CurrentRemoteDirectory = ChecklistItem->Remote.Directory;

      LogEvent(FORMAT(L"Synchronizing local directory '%s' with remote directory '%s', "
        "params = 0x%x (%s)", (CurrentLocalDirectory, CurrentRemoteDirectory,
        int(Params), SynchronizeParamsStr(Params))));

      int Count = 0;

      while ((IIndex < Checklist->Count) &&
             (Checklist->Item[IIndex]->Local.Directory == CurrentLocalDirectory) &&
             (Checklist->Item[IIndex]->Remote.Directory == CurrentRemoteDirectory))
      {
        ChecklistItem = Checklist->Item[IIndex];
        if (ChecklistItem->Checked)
        {
          Count++;

          if (FLAGSET(Params, spTimestamp))
          {
            switch (ChecklistItem->Action)
            {
              case TSynchronizeChecklist::saDownloadUpdate:
                DownloadList->AddObject(
                  UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) +
                    ChecklistItem->Remote.FileName,
                  (TObject *)(ChecklistItem));
                break;

              case TSynchronizeChecklist::saUploadUpdate:
                UploadList->AddObject(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->Local.FileName,
                  (TObject *)(ChecklistItem));
                break;

              default:
                DebugFail();
                break;
            }
          }
          else
          {
            switch (ChecklistItem->Action)
            {
              case TSynchronizeChecklist::saDownloadNew:
              case TSynchronizeChecklist::saDownloadUpdate:
                DownloadList->AddObject(
                  UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) +
                    ChecklistItem->Remote.FileName,
                  ChecklistItem->RemoteFile);
                break;

              case TSynchronizeChecklist::saDeleteRemote:
                DeleteRemoteList->AddObject(
                  UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) +
                    ChecklistItem->Remote.FileName,
                  ChecklistItem->RemoteFile);
                break;

              case TSynchronizeChecklist::saUploadNew:
              case TSynchronizeChecklist::saUploadUpdate:
                UploadList->Add(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->Local.FileName);
                break;

              case TSynchronizeChecklist::saDeleteLocal:
                DeleteLocalList->Add(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->Local.FileName);
                break;

              default:
                DebugFail();
                break;
            }
          }
        }
        IIndex++;
      }

      // prevent showing/updating of progress dialog if there's nothing to do
      if (Count > 0)
      {
        Data.LocalDirectory = IncludeTrailingBackslash(CurrentLocalDirectory);
        Data.RemoteDirectory = UnixIncludeTrailingBackslash(CurrentRemoteDirectory);
        DoSynchronizeProgress(Data, false);

        if (FLAGSET(Params, spTimestamp))
        {
          if (DownloadList->Count > 0)
          {
            ProcessFiles(DownloadList, foSetProperties,
              SynchronizeLocalTimestamp, NULL, osLocal);
          }

          if (UploadList->Count > 0)
          {
            ProcessFiles(UploadList, foSetProperties,
              SynchronizeRemoteTimestamp);
          }
        }
        else
        {
          if ((DownloadList->Count > 0) &&
              !CopyToLocal(DownloadList, Data.LocalDirectory, &SyncCopyParam, CopyParams))
          {
            Abort();
          }

          if ((DeleteRemoteList->Count > 0) &&
              !DeleteFiles(DeleteRemoteList))
          {
            Abort();
          }

          if ((UploadList->Count > 0) &&
              !CopyToRemote(UploadList, Data.RemoteDirectory, &SyncCopyParam, CopyParams))
          {
            Abort();
          }

          if ((DeleteLocalList->Count > 0) &&
              !DeleteLocalFiles(DeleteLocalList))
          {
            Abort();
          }
        }
      }
    }
  }
  __finally
  {
    delete DownloadList;
    delete DeleteRemoteList;
    delete UploadList;
    delete DeleteLocalList;

    EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeProgress(const TSynchronizeData & Data,
  bool Collect)
{
  if (Data.OnSynchronizeDirectory != NULL)
  {
    bool Continue = true;
    Data.OnSynchronizeDirectory(Data.LocalDirectory, Data.RemoteDirectory,
      Continue, Collect);

    if (!Continue)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeLocalTimestamp(const UnicodeString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  UnicodeString LocalFile =
    IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
      ChecklistItem->Local.FileName;

  FILE_OPERATION_LOOP_BEGIN
  {
    HANDLE Handle;
    OpenLocalFile(LocalFile, GENERIC_WRITE, NULL, &Handle,
      NULL, NULL, NULL, NULL);
    FILETIME WrTime = DateTimeToFileTime(ChecklistItem->Remote.Modification,
      SessionData->DSTMode);
    bool Result = SetFileTime(Handle, NULL, NULL, &WrTime);
    int Error = GetLastError();
    CloseHandle(Handle);
    if (!Result)
    {
      RaiseLastOSError(Error);
    }
  }
  FILE_OPERATION_LOOP_END(FMTLOAD(CANT_SET_ATTRS, (LocalFile)));
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeRemoteTimestamp(const UnicodeString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  TRemoteProperties Properties;
  Properties.Valid << vpModification;
  Properties.Modification = ConvertTimestampToUnix(ChecklistItem->FLocalLastWriteTime,
    SessionData->DSTMode);

  ChangeFileProperties(
    UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) + ChecklistItem->Remote.FileName,
    NULL, &Properties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileFind(UnicodeString FileName,
  const TRemoteFile * File, /*TFilesFindParams*/ void * Param)
{
  // see DoFilesFind
  FOnFindingFile = NULL;

  DebugAssert(Param);
  DebugAssert(File);
  TFilesFindParams * AParams = static_cast<TFilesFindParams*>(Param);

  if (!AParams->Cancel)
  {
    if (FileName.IsEmpty())
    {
      FileName = File->FileName;
    }

    TFileMasks::TParams MaskParams;
    MaskParams.Size = File->Size;
    MaskParams.Modification = File->Modification;

    UnicodeString FullFileName = UnixExcludeTrailingBackslash(File->FullFileName);
    bool ImplicitMatch;
    // Do not use recursive include match
    if (AParams->FileMask.Matches(FullFileName, false,
         File->IsDirectory, &MaskParams, false, ImplicitMatch))
    {
      if (!ImplicitMatch)
      {
        AParams->OnFileFound(this, FileName, File, AParams->Cancel);
      }

      if (File->IsDirectory)
      {
        UnicodeString RealDirectory;
        if (!File->IsSymLink || File->LinkTo.IsEmpty())
        {
          RealDirectory = UnixIncludeTrailingBackslash(AParams->RealDirectory) + File->FileName;
        }
        else
        {
          RealDirectory = ::AbsolutePath(AParams->RealDirectory, File->LinkTo);
        }

        if (!AParams->LoopDetector.IsUnvisitedDirectory(RealDirectory))
        {
          LogEvent(FORMAT(L"Already searched \"%s\" directory (real path \"%s\"), link loop detected", (FullFileName, RealDirectory)));
        }
        else
        {
          DoFilesFind(FullFileName, *AParams, RealDirectory);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoFilesFind(UnicodeString Directory, TFilesFindParams & Params, UnicodeString RealDirectory)
{
  LogEvent(FORMAT(L"Searching directory \"%s\" (real path \"%s\")", (Directory, RealDirectory)));
  Params.OnFindingFile(this, Directory, Params.Cancel);
  if (!Params.Cancel)
  {
    DebugAssert(FOnFindingFile == NULL);
    // ideally we should set the handler only around actually reading
    // of the directory listing, so we at least reset the handler in
    // FileFind
    FOnFindingFile = Params.OnFindingFile;
    UnicodeString PrevRealDirectory = Params.RealDirectory;
    try
    {
      Params.RealDirectory = RealDirectory;
      ProcessDirectory(Directory, FileFind, &Params, false, true);
    }
    __finally
    {
      Params.RealDirectory = PrevRealDirectory;
      FOnFindingFile = NULL;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FilesFind(UnicodeString Directory, const TFileMasks & FileMask,
  TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile)
{
  TFilesFindParams Params;
  Params.FileMask = FileMask;
  Params.OnFileFound = OnFileFound;
  Params.OnFindingFile = OnFindingFile;
  Params.Cancel = false;

  Params.LoopDetector.RecordVisitedDirectory(Directory);

  DoFilesFind(Directory, Params, Directory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  DebugAssert(IsCapable[fcCheckingSpaceAvailable]);

  try
  {
    FFileSystem->SpaceAvailable(Path, ASpaceAvailable);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(SPACE_AVAILABLE_ERROR, (Path)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LockFile(const UnicodeString FileName,
  const TRemoteFile * File, void * /*Param*/)
{
  StartOperationWithFile(FileName, foLock);

  LogEvent(FORMAT(L"Locking file \"%s\".", (FileName)));
  FileModified(File, FileName, true);

  DoLockFile(FileName, File);
  ReactOnCommand(fsLock);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoLockFile(const UnicodeString & FileName, const TRemoteFile * File)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FFileSystem->LockFile(FileName, File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(LOCK_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UnlockFile(const UnicodeString FileName,
  const TRemoteFile * File, void * /*Param*/)
{
  StartOperationWithFile(FileName, foUnlock);

  LogEvent(FORMAT(L"Unlocking file \"%s\".", (FileName)));
  FileModified(File, FileName, true);

  DoUnlockFile(FileName, File);
  ReactOnCommand(fsLock);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoUnlockFile(const UnicodeString & FileName, const TRemoteFile * File)
{
  TRetryOperationLoop RetryLoop(this);
  do
  {
    try
    {
      FFileSystem->UnlockFile(FileName, File);
    }
    catch(Exception & E)
    {
      RetryLoop.Error(E, FMTLOAD(UNLOCK_FILE_ERROR, (FileName)));
    }
  }
  while (RetryLoop.Retry());
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::LockFiles(TStrings * FileList)
{
  BeginTransaction();
  try
  {
    ProcessFiles(FileList, foLock, LockFile, NULL);
  }
  __finally
  {
    EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::UnlockFiles(TStrings * FileList)
{
  BeginTransaction();
  try
  {
    ProcessFiles(FileList, foUnlock, UnlockFile, NULL);
  }
  __finally
  {
    EndTransaction();
  }
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TTerminal::GetSessionInfo()
{
  return FFileSystem->GetSessionInfo();
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TTerminal::GetFileSystemInfo(bool Retrieve)
{
  return FFileSystem->GetFileSystemInfo(Retrieve);
}
//---------------------------------------------------------------------
void __fastcall TTerminal::GetSupportedChecksumAlgs(TStrings * Algs)
{
  FFileSystem->GetSupportedChecksumAlgs(Algs);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetPassword()
{
  UnicodeString Result;
  // FRememberedPassword is empty also when stored password was used
  if (FRememberedPassword.IsEmpty())
  {
    Result = SessionData->Password;
  }
  else
  {
    Result = RememberedPassword;
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRememberedPassword()
{
  return DecryptPassword(FRememberedPassword);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetRememberedTunnelPassword()
{
  return DecryptPassword(FRememberedTunnelPassword);
}
//---------------------------------------------------------------------
bool __fastcall TTerminal::GetStoredCredentialsTried()
{
  bool Result;
  if (FFileSystem != NULL)
  {
    Result = FFileSystem->GetStoredCredentialsTried();
  }
  else if (FSecureShell != NULL)
  {
    Result = FSecureShell->GetStoredCredentialsTried();
  }
  else
  {
    DebugAssert(FTunnelOpening);
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  DebugAssert(FFileSystem);
  DebugAssert(FilesToCopy);


  bool Result = false;
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  try
  {
    __int64 Size;
    // dirty trick: when moving, do not pass copy param to avoid exclude mask
    bool CalculatedSize =
      CalculateLocalFilesSize(
        FilesToCopy, Size,
        (FLAGCLEAR(Params, cpDelete) ? CopyParam : NULL),
        CopyParam->CalculateSize);

    TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);
    OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osLocal,
      FilesToCopy->Count, Params & cpTemporary, TargetDir, CopyParam->CPSLimit);

    FOperationProgress = &OperationProgress;
    bool CollectingUsage = false;
    try
    {
      if (CalculatedSize)
      {
        if (Configuration->Usage->Collect)
        {
          int CounterSize = TUsage::CalculateCounterSize(Size);
          Configuration->Usage->Inc(L"Uploads");
          Configuration->Usage->Inc(L"UploadedBytes", CounterSize);
          Configuration->Usage->SetMax(L"MaxUploadSize", CounterSize);
          CollectingUsage = true;
        }

        OperationProgress.SetTotalSize(Size);
      }

      UnicodeString UnlockedTargetDir = TranslateLockedPath(TargetDir, false);
      BeginTransaction();
      try
      {
        if (Log->Logging)
        {
          LogEvent(FORMAT(L"Copying %d files/directories to remote directory "
            "\"%s\"", (FilesToCopy->Count, TargetDir)));
          LogEvent(CopyParam->LogStr);
        }

        FFileSystem->CopyToRemote(FilesToCopy, UnlockedTargetDir,
          CopyParam, Params, &OperationProgress, OnceDoneOperation);
      }
      __finally
      {
        if (Active)
        {
          ReactOnCommand(fsCopyToRemote);
        }
        EndTransaction();
      }

      if (OperationProgress.Cancel == csContinue)
      {
        Result = true;
      }
    }
    __finally
    {
      if (CollectingUsage)
      {
        int CounterTime = TimeToSeconds(OperationProgress.TimeElapsed());
        Configuration->Usage->Inc(L"UploadTime", CounterTime);
        Configuration->Usage->SetMax(L"MaxUploadTime", CounterTime);
      }
      OperationProgress.Stop();
      FOperationProgress = NULL;
    }
  }
  catch (Exception &E)
  {
    CommandError(&E, MainInstructions(LoadStr(TOREMOTE_COPY_ERROR)));
    OnceDoneOperation = odoIdle;
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  DebugAssert(FFileSystem);

  // see scp.c: sink(), tolocal()

  bool Result = false;
  bool OwnsFileList = (FilesToCopy == NULL);
  TOnceDoneOperation OnceDoneOperation = odoIdle;

  try
  {
    if (OwnsFileList)
    {
      FilesToCopy = new TStringList();
      FilesToCopy->Assign(Files->SelectedFiles);
    }

    BeginTransaction();
    try
    {
      __int64 TotalSize;
      bool TotalSizeKnown = false;
      TFileOperationProgressType OperationProgress(&DoProgress, &DoFinished);

      ExceptionOnFail = true;
      try
      {
        // dirty trick: when moving, do not pass copy param to avoid exclude mask
        if (CalculateFilesSize(
             FilesToCopy, TotalSize, csIgnoreErrors,
             (FLAGCLEAR(Params, cpDelete) ? CopyParam : NULL),
             CopyParam->CalculateSize, NULL))
        {
          TotalSizeKnown = true;
        }
      }
      __finally
      {
        ExceptionOnFail = false;
      }

      OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osRemote,
        FilesToCopy->Count, Params & cpTemporary, TargetDir, CopyParam->CPSLimit);

      FOperationProgress = &OperationProgress;
      bool CollectingUsage = false;
      try
      {
        if (TotalSizeKnown)
        {
          if (Configuration->Usage->Collect)
          {
            int CounterTotalSize = TUsage::CalculateCounterSize(TotalSize);
            Configuration->Usage->Inc(L"Downloads");
            Configuration->Usage->Inc(L"DownloadedBytes", CounterTotalSize);
            Configuration->Usage->SetMax(L"MaxDownloadSize", CounterTotalSize);
            CollectingUsage = true;
          }

          OperationProgress.SetTotalSize(TotalSize);
        }

        try
        {
          try
          {
            if (Log->Logging)
            {
              LogEvent(FORMAT(L"Copying %d files/directories to local directory "
                "\"%s\"", (FilesToCopy->Count, TargetDir)));
              LogEvent(CopyParam->LogStr);
            }

            FFileSystem->CopyToLocal(FilesToCopy, TargetDir, CopyParam, Params,
              &OperationProgress, OnceDoneOperation);
          }
          __finally
          {
            if (Active)
            {
              ReactOnCommand(fsCopyToLocal);
            }
          }
        }
        catch (Exception &E)
        {
          CommandError(&E, MainInstructions(LoadStr(TOLOCAL_COPY_ERROR)));
          OnceDoneOperation = odoIdle;
        }

        if (OperationProgress.Cancel == csContinue)
        {
          Result = true;
        }
      }
      __finally
      {
        if (CollectingUsage)
        {
          int CounterTime = TimeToSeconds(OperationProgress.TimeElapsed());
          Configuration->Usage->Inc(L"DownloadTime", CounterTime);
          Configuration->Usage->SetMax(L"MaxDownloadTime", CounterTime);
        }
        FOperationProgress = NULL;
        OperationProgress.Stop();
      }
    }
    __finally
    {
      // If session is still active (no fatal error) we reload directory
      // by calling EndTransaction
      EndTransaction();
    }

  }
  __finally
  {
    if (OwnsFileList) delete FilesToCopy;
  }

  if (OnceDoneOperation != odoIdle)
  {
    CloseOnCompletion(OnceDoneOperation);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReflectSettings()
{
  DebugAssert(FLog != NULL);
  FLog->ReflectSettings();
  DebugAssert(FActionLog != NULL);
  FActionLog->ReflectSettings();
  // also FTunnelLog ?
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CollectUsage()
{
  switch (SessionData->FSProtocol)
  {
    case fsSCPonly:
      Configuration->Usage->Inc(L"OpenedSessionsSCP");
      break;

    case fsSFTP:
    case fsSFTPonly:
      Configuration->Usage->Inc(L"OpenedSessionsSFTP");
      break;

    case fsFTP:
      if (SessionData->Ftps == ftpsNone)
      {
        Configuration->Usage->Inc(L"OpenedSessionsFTP");
      }
      else
      {
        Configuration->Usage->Inc(L"OpenedSessionsFTPS");
      }
      break;

    case fsWebDAV:
      if (SessionData->Ftps == ftpsNone)
      {
        Configuration->Usage->Inc(L"OpenedSessionsWebDAV");
      }
      else
      {
        Configuration->Usage->Inc(L"OpenedSessionsWebDAVS");
      }
      break;
  }

  if (Configuration->Logging && Configuration->LogToFile)
  {
    Configuration->Usage->Inc(L"OpenedSessionsLogToFile2");
  }

  if (Configuration->LogActions)
  {
    Configuration->Usage->Inc(L"OpenedSessionsXmlLog");
  }

  std::unique_ptr<TSessionData> FactoryDefaults(new TSessionData(L""));
  if (!SessionData->IsSame(FactoryDefaults.get(), true))
  {
    Configuration->Usage->Inc(L"OpenedSessionsAdvanced");
  }

  if (SessionData->ProxyMethod != ::pmNone)
  {
    Configuration->Usage->Inc(L"OpenedSessionsProxy");
  }
  if (SessionData->FtpProxyLogonType > 0)
  {
    Configuration->Usage->Inc(L"OpenedSessionsFtpProxy");
  }

  FCollectFileSystemUsage = true;
}
//---------------------------------------------------------------------------
static UnicodeString __fastcall FormatCertificateData(const UnicodeString & Fingerprint, int Failures)
{
  return FORMAT(L"%s;%2.2X", (Fingerprint, Failures));
}
//---------------------------------------------------------------------------
bool  __fastcall TTerminal::VerifyCertificate(
  const UnicodeString & CertificateStorageKey, const UnicodeString & SiteKey,
  const UnicodeString & Fingerprint,
  const UnicodeString & CertificateSubject, int Failures)
{
  bool Result = false;

  UnicodeString CertificateData = FormatCertificateData(Fingerprint, Failures);

  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateConfigStorage());
  Storage->AccessMode = smRead;

  if (Storage->OpenSubKey(CertificateStorageKey, false))
  {
    if (Storage->ValueExists(SiteKey))
    {
      UnicodeString CachedCertificateData = Storage->ReadString(SiteKey, L"");
      if (CertificateData == CachedCertificateData)
      {
        LogEvent(FORMAT(L"Certificate for \"%s\" matches cached fingerprint and failures", (CertificateSubject)));
        Result = true;
      }
    }
    else if (Storage->ValueExists(Fingerprint))
    {
      LogEvent(FORMAT(L"Certificate for \"%s\" matches legacy cached fingerprint", (CertificateSubject)));
      Result = true;
    }
  }

  if (!Result)
  {
    UnicodeString Buf = SessionData->HostKey;
    while (!Result && !Buf.IsEmpty())
    {
      UnicodeString ExpectedKey = CutToChar(Buf, L';', false);
      if (ExpectedKey == L"*")
      {
        UnicodeString Message = LoadStr(ANY_CERTIFICATE);
        Information(Message, true);
        Log->Add(llException, Message);
        Result = true;
      }
      else if (ExpectedKey == Fingerprint)
      {
        LogEvent(FORMAT(L"Certificate for \"%s\" matches configured fingerprint", (CertificateSubject)));
        Result = true;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CacheCertificate(const UnicodeString & CertificateStorageKey,
  const UnicodeString & SiteKey, const UnicodeString & Fingerprint, int Failures)
{
  UnicodeString CertificateData = FormatCertificateData(Fingerprint, Failures);

  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateConfigStorage());
  Storage->AccessMode = smReadWrite;

  if (Storage->OpenSubKey(CertificateStorageKey, true))
  {
    Storage->WriteString(SiteKey, CertificateData);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CollectTlsUsage(const UnicodeString & TlsVersionStr)
{
  // see SSL_get_version() in OpenSSL ssl_lib.c
  if (TlsVersionStr == L"TLSv1.2")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS12");
  }
  else if (TlsVersionStr == L"TLSv1.1")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS11");
  }
  else if (TlsVersionStr == L"TLSv1")
  {
    Configuration->Usage->Inc(L"OpenedSessionsTLS10");
  }
  else if (TlsVersionStr == L"SSLv3")
  {
    Configuration->Usage->Inc(L"OpenedSessionsSSL30");
  }
  else if (TlsVersionStr == L"SSLv2")
  {
    Configuration->Usage->Inc(L"OpenedSessionsSSL20");
  }
  else
  {
    DebugFail();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LoadTlsCertificate(X509 *& Certificate, EVP_PKEY *& PrivateKey)
{
  bool Result = !SessionData->TlsCertificateFile.IsEmpty();
  if (Result)
  {
    UnicodeString Passphrase = SessionData->Passphrase;

    // Inspired by neon's ne_ssl_clicert_read
    bool Retry;

    do
    {
      Retry = false;

      bool WrongPassphrase = false;
      ParseCertificate(SessionData->TlsCertificateFile, Passphrase, Certificate, PrivateKey, WrongPassphrase);
      if (WrongPassphrase)
      {
        if (Passphrase.IsEmpty())
        {
          LogEvent(L"Certificate is encrypted, need passphrase");
          Information(LoadStr(CLIENT_CERTIFICATE_LOADING), false);
        }
        else
        {
          Information(LoadStr(CERTIFICATE_DECODE_ERROR_INFO), false);
        }

        Passphrase = L"";
        if (PromptUser(
              SessionData, pkPassphrase,
              LoadStr(CERTIFICATE_PASSPHRASE_TITLE), L"",
              LoadStr(CERTIFICATE_PASSPHRASE_PROMPT), false, 0, Passphrase))
        {
          Retry = true;
        }
        else
        {
          Result = false;
        }
      }
    }
    while (Retry);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::GetBaseFileName(UnicodeString FileName)
{
  if (FSessionData->TrimVMSVersions)
  {
    int P = FileName.LastDelimiter(L";");
    if (P > 0)
    {
      FileName.SetLength(P - 1);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TTerminal::ChangeFileName(const TCopyParamType * CopyParam,
  UnicodeString FileName, TOperationSide Side, bool FirstLevel)
{
  FileName = GetBaseFileName(FileName);
  FileName = CopyParam->ChangeFileName(FileName, Side, FirstLevel);
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CanRecurseToDirectory(const TRemoteFile * File)
{
  return !File->IsSymLink || FSessionData->FollowDirectorySymlinks;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsThisOrChild(TTerminal * Terminal)
{
  return
    (this == Terminal) ||
    ((FCommandSession != NULL) && (FCommandSession == Terminal));
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TSecondaryTerminal::TSecondaryTerminal(TTerminal * MainTerminal,
  TSessionData * ASessionData, TConfiguration * Configuration, const UnicodeString & Name) :
  TTerminal(ASessionData, Configuration),
  FMainTerminal(MainTerminal)
{
  Log->Parent = FMainTerminal->Log;
  Log->Name = Name;
  ActionLog->Enabled = false;
  SessionData->NonPersistant();
  DebugAssert(FMainTerminal != NULL);
  if (!FMainTerminal->UserName.IsEmpty())
  {
    SessionData->UserName = FMainTerminal->UserName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::UpdateFromMain()
{
  if ((FFileSystem != NULL) && (FMainTerminal->FFileSystem != NULL))
  {
    FFileSystem->UpdateFromMain(FMainTerminal->FFileSystem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  FMainTerminal->DirectoryLoaded(FileList);
  DebugAssert(FileList != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryModified(const UnicodeString Path,
  bool SubDirs)
{
  // clear cache of main terminal
  FMainTerminal->DirectoryModified(Path, SubDirs);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TSecondaryTerminal::GetPasswordSource()
{
  return FMainTerminal;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::TTerminalList(TConfiguration * AConfiguration) :
  TObjectList()
{
  DebugAssert(AConfiguration);
  FConfiguration = AConfiguration;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::~TTerminalList()
{
  DebugAssert(Count == 0);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::CreateTerminal(TSessionData * Data)
{
  return new TTerminal(Data, FConfiguration);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::NewTerminal(TSessionData * Data)
{
  TTerminal * Terminal = CreateTerminal(Data);
  Add(Terminal);
  return Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::FreeTerminal(TTerminal * Terminal)
{
  DebugAssert(IndexOf(Terminal) >= 0);
  Remove(Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::FreeAndNullTerminal(TTerminal * & Terminal)
{
  TTerminal * T = Terminal;
  Terminal = NULL;
  FreeTerminal(T);
}
//---------------------------------------------------------------------------
TTerminal * __fastcall TTerminalList::GetTerminal(int Index)
{
  return dynamic_cast<TTerminal *>(Items[Index]);
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::Idle()
{
  TTerminal * Terminal;
  for (int i = 0; i < Count; i++)
  {
    Terminal = Terminals[i];
    if (Terminal->Status == ssOpened)
    {
      Terminal->Idle();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminalList::RecryptPasswords()
{
  for (int Index = 0; Index < Count; Index++)
  {
    Terminals[Index]->RecryptPasswords();
  }
}
