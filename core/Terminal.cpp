//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Terminal.h"

#include <SysUtils.hpp>
#include <FileCtrl.hpp>

#include "Common.h"
#include "PuttyTools.h"
#include "Exceptions.h"
#include "FileBuffer.h"
#include "Interface.h"
#include "RemoteFiles.h"
#include "SecureShell.h"
#include "ScpFileSystem.h"
#include "SftpFileSystem.h"
#ifndef NO_FILEZILLA
#include "FtpFileSystem.h"
#endif
#include "TextsCore.h"
#include "HelpCore.h"
#include "Security.h"
#include "CoreMain.h"
#include "Queue.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define SSH_ERROR(x) throw ESsh(NULL, x)
#define SSH_FATAL_ERROR_EXT(E, x) throw ESshFatal(E, x)
#define SSH_FATAL_ERROR(x) SSH_FATAL_ERROR_EXT(NULL, x)
//---------------------------------------------------------------------------
#define COMMAND_ERROR_ARI(MESSAGE, REPEAT) \
  { \
    int Result = CommandError(&E, MESSAGE, qaRetry | qaSkip | qaAbort); \
    switch (Result) { \
      case qaRetry: { REPEAT; } break; \
      case qaAbort: Abort(); \
    } \
  }

#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(this, ALLOW_SKIP, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
struct TMoveFileParams
{
  AnsiString Target;
  AnsiString FileMask;
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
  Action(saNone), IsDirectory(false), FRemoteFile(NULL), Checked(true), ImageIndex(-1)
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
  delete FRemoteFile;
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
    Result = CompareText(Item1->Local.Directory, Item2->Local.Directory);
  }
  else
  {
    assert(!Item1->Remote.Directory.IsEmpty());
    Result = CompareText(Item1->Remote.Directory, Item2->Remote.Directory);
  }

  if (Result == 0)
  {
    Result = CompareText(Item1->FileName, Item2->FileName);
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
    throw;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TTunnelUI : public TSessionUI
{
public:
  __fastcall TTunnelUI(TTerminal * Terminal);
  virtual void __fastcall Information(const AnsiString & Str, bool Status);
  virtual int __fastcall QueryUser(const AnsiString Query,
    TStrings * MoreMessages, int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual int __fastcall QueryUserException(const AnsiString Query,
    Exception * E, int Answers, const TQueryParams * Params,
    TQueryType QueryType);
  virtual bool __fastcall PromptUser(TSessionData * Data, AnsiString Prompt,
    TPromptKind Kind, AnsiString & Response);
  virtual void __fastcall DisplayBanner(const AnsiString & Banner);
  virtual void __fastcall ShowExtendedException(Exception * E);
  virtual void __fastcall Closed();

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
void __fastcall TTunnelUI::Information(const AnsiString & Str, bool Status)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->Information(Str, Status);
  }
}
//---------------------------------------------------------------------------
int __fastcall TTunnelUI::QueryUser(const AnsiString Query,
  TStrings * MoreMessages, int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  int Result;
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
int __fastcall TTunnelUI::QueryUserException(const AnsiString Query,
  Exception * E, int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  int Result;
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
bool __fastcall TTunnelUI::PromptUser(TSessionData * Data, AnsiString Prompt,
  TPromptKind Kind, AnsiString & Response)
{
  bool Result;
  if (GetCurrentThreadId() == FTerminalThread)
  {
    Result = FTerminal->PromptUser(Data, Prompt, Kind, Response);
  }
  else
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::DisplayBanner(const AnsiString & Banner)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->DisplayBanner(Banner);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::ShowExtendedException(Exception * E)
{
  if (GetCurrentThreadId() == FTerminalThread)
  {
    FTerminal->ShowExtendedException(E);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTunnelUI::Closed()
{
  // noop
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
std::set<int> TTerminal::FTunnelLocalPorts;
TCriticalSection TTerminal::FSection;
//---------------------------------------------------------------------------
__fastcall TTerminal::TTerminal(TSessionData * SessionData,
  TConfiguration * Configuration)
{
  FConfiguration = Configuration;
  FSessionData = new TSessionData("");
  FSessionData->Assign(SessionData);
  FLog = new TSessionLog(this, FSessionData, Configuration);
  FFiles = new TRemoteDirectory(this);
  FExceptionOnFail = 0;
  FInTransaction = 0;
  FReadCurrentDirectoryPending = false;
  FReadDirectoryPending = false;
  FUsersGroupsLookedup = False;
  FTunnelLocalPortNumber = 0;
  FGroups = new TUsersGroupsList();
  FUsers = new TUsersGroupsList();
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
  FOnClose = NULL;

  FUseBusyCursor = True;
  FLockDirectory = "";
  FDirectoryCache = new TRemoteDirectoryCache();
  FDirectoryChangesCache = NULL;
  FFSProtocol = cfsUnknown;
  FCommandSession = NULL;
  FAutoReadDirectory = true;
  FReadingCurrentDirectory = false;
  FStatus = ssClosed;
  FTunnelThread = NULL;
  FTunnel = NULL;
  FTunnelData = NULL;
  FTunnelLog = NULL;
  FTunnelUI = NULL;
}
//---------------------------------------------------------------------------
__fastcall TTerminal::~TTerminal()
{
  if (Active)
  {
    Close();
  }

  assert(FTunnel == NULL);

  SAFE_DESTROY(FCommandSession);

  if (SessionData->CacheDirectoryChanges && SessionData->PreserveDirectoryChanges &&
      (FDirectoryChangesCache != NULL))
  {
    Configuration->SaveDirectoryChangesCache(SessionData->SessionKey,
      FDirectoryChangesCache);
  }

  SAFE_DESTROY_EX(TCustomFileSystem, FFileSystem);
  SAFE_DESTROY_EX(TSessionLog, FLog);
  delete FFiles;
  delete FGroups;
  delete FUsers;
  delete FDirectoryCache;
  delete FDirectoryChangesCache;
  SAFE_DESTROY(FSessionData);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Idle()
{
  if (Configuration->LogProtocol >= 1)
  {
    LogEvent("Session upkeep");
  }

  assert(FFileSystem != NULL);
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
        FCommandSession->ShowExtendedException(&E);
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsAbsolutePath(const AnsiString Path)
{
  return !Path.IsEmpty() && Path[1] == '/';
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::ExpandFileName(AnsiString Path,
  const AnsiString BasePath)
{
  Path = UnixExcludeTrailingBackslash(Path);
  if (!IsAbsolutePath(Path) && !BasePath.IsEmpty())
  {
    // TODO: Handle more complicated cases like "../../xxx"
    if (Path == "..")
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

  if (CommandSessionOpened)
  {
    FCommandSession->Close();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ResetConnection()
{
  FAnyInformation = false;
  // used to be called from Reopen(), why?
  FTunnelError = "";

  if (FDirectoryChangesCache != NULL)
  {
    delete FDirectoryChangesCache;
    FDirectoryChangesCache = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Open()
{
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
          Log->AddStartupInfo();
        }

        assert(FTunnel == NULL);
        if (FSessionData->Tunnel)
        {
          DoInformation(LoadStr(OPEN_TUNNEL), true);
          LogEvent("Opening tunnel.");
          OpenTunnel();
          Log->AddSeparator();

          FSessionData->ConfigureTunnel(FTunnelLocalPortNumber);

          DoInformation(LoadStr(USING_TUNNEL), false);
          LogEvent(FORMAT("Connecting via tunnel interface %s:%d.",
            (FSessionData->HostName, FSessionData->PortNumber)));
        }
        else
        {
          assert(FTunnelLocalPortNumber == 0);
        }

        if (FFileSystem == NULL)
        {
          if (SessionData->FSProtocol == fsFTP)
          {
            #ifdef NO_FILEZILLA
            LogEvent("FTP protocol is not supported by this build.");
            FatalError(NULL, LoadStr(FTP_UNSUPPORTED));
            #else
            FFSProtocol = cfsFTP;
            FFileSystem = new TFTPFileSystem(this);
            FFileSystem->Open();
            Log->AddSeparator();
            LogEvent("Using FTP protocol.");
            #endif
          }
          else
          {
            assert(FSecureShell == NULL);
            try
            {
              FSecureShell = new TSecureShell(this, FSessionData, Log, Configuration);
              try
              {
                FSecureShell->Open();
              }
              catch(Exception & E)
              {
                assert(!FSecureShell->Active);
                if (!FSecureShell->Active && !FTunnelError.IsEmpty())
                {
                  // the only case where we expect this to happen
                  assert(E.Message == LoadStr(UNEXPECTED_CLOSE_ERROR));
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
                LogEvent("Using SCP protocol.");
              }
              else
              {
                FFSProtocol = cfsSFTP;
                FFileSystem = new TSFTPFileSystem(this, FSecureShell);
                FSecureShell = NULL; // ownership passed
                LogEvent("Using SFTP protocol.");
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
        assert(FDirectoryChangesCache == NULL);
        FDirectoryChangesCache = new TRemoteDirectoryChangesCache();
        if (SessionData->PreserveDirectoryChanges)
        {
          Configuration->LoadDirectoryChangesCache(SessionData->SessionKey,
            FDirectoryChangesCache);
        }
      }

      DoStartup();

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
      throw;
    }
  }
  __finally
  {
    // Prevent calling Information with active=false unless there was at least
    // one call with active=true
    if (FAnyInformation)
    {
      DoInformation("", true, false);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenTunnel()
{
  assert(FTunnelData == NULL);

  FTunnelLocalPortNumber = FSessionData->TunnelLocalPortNumber;
  if (FTunnelLocalPortNumber == 0)
  {
    TGuard Guard(&FSection);

    FTunnelLocalPortNumber = Configuration->TunnelLocalPortNumberLow;
    // override strange bug in STL
    #pragma warn -8092
    while ((FTunnelLocalPorts.find(FTunnelLocalPortNumber) != FTunnelLocalPorts.end()) ||
           !IsListenerFree(FTunnelLocalPortNumber))
    #pragma warn .8092
    {
      FTunnelLocalPortNumber++;
      if (FTunnelLocalPortNumber > Configuration->TunnelLocalPortNumberHigh)
      {
        FTunnelLocalPortNumber = 0;
        FatalError(NULL, FMTLOAD(TUNNEL_NO_FREE_PORT,
          (Configuration->TunnelLocalPortNumberLow, Configuration->TunnelLocalPortNumberHigh)));
      }
    }
    FTunnelLocalPorts.insert(FTunnelLocalPortNumber);
    LogEvent(FORMAT("Autoselected tunnel local port number %d", (FTunnelLocalPortNumber)));
  }

  try
  {
    FTunnelData = new TSessionData("");
    FTunnelData->Assign(StoredSessions->DefaultSettings);
    FTunnelData->Name = FMTLOAD(TUNNEL_SESSION_NAME, (FSessionData->SessionName));
    FTunnelData->Tunnel = false;
    FTunnelData->HostName = FSessionData->TunnelHostName;
    FTunnelData->PortNumber = FSessionData->TunnelPortNumber;
    FTunnelData->UserName = FSessionData->TunnelUserName;
    FTunnelData->Password = FSessionData->TunnelPassword;
    FTunnelData->PublicKeyFile = FSessionData->TunnelPublicKeyFile;
    FTunnelData->TunnelPortFwd = FORMAT("L%d\t%s:%d",
      (FTunnelLocalPortNumber, FSessionData->HostName, FSessionData->PortNumber));
    FTunnelData->ProxyMethod = FSessionData->ProxyMethod;
    FTunnelData->ProxyHost = FSessionData->ProxyHost;
    FTunnelData->ProxyPort = FSessionData->ProxyPort;
    FTunnelData->ProxyUsername = FSessionData->ProxyUsername;
    FTunnelData->ProxyPassword = FSessionData->ProxyPassword;
    FTunnelData->ProxyTelnetCommand = FSessionData->ProxyTelnetCommand;
    FTunnelData->ProxyDNS = FSessionData->ProxyDNS;
    FTunnelData->ProxyLocalhost = FSessionData->ProxyLocalhost;

    FTunnelLog = new TSessionLog(this, FTunnelData, Configuration);
    FTunnelLog->Parent = FLog;
    FTunnelLog->Name = "Tunnel";
    FTunnelUI = new TTunnelUI(this);
    FTunnel = new TSecureShell(FTunnelUI, FTunnelData, FTunnelLog, Configuration);

    FTunnel->Open();

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

  TGuard Guard(&FSection);
  FTunnelLocalPorts.erase(FTunnelLocalPortNumber);
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
    OnClose(this);
  }

  FStatus = ssClosed;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Reopen(int Params)
{
  assert(FExceptionOnFail == 0);

  TFSProtocol OrigFSProtocol = SessionData->FSProtocol;
  AnsiString PrevRemoteDirectory = SessionData->RemoteDirectory;
  bool PrevReadCurrentDirectoryPending = FReadCurrentDirectoryPending;
  bool PrevReadDirectoryPending = FReadDirectoryPending;
  int PrevInTransaction = FInTransaction;
  bool PrevAutoReadDirectory = FAutoReadDirectory;
  try
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
    FInTransaction = 0;
    // typically, we avoid reading directory, when there is operation ongoing,
    // for file list which may reference files from current directory
    if (FLAGSET(Params, ropNoReadDirectory))
    {
      AutoReadDirectory = false;
    }

    SessionData->RemoteDirectory = CurrentDirectory;
    if (SessionData->FSProtocol == fsSFTP)
    {
      SessionData->FSProtocol = (FFSProtocol == cfsSCP ? fsSCPonly : fsSFTPonly);
    }

    if (Active)
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
    FInTransaction = PrevInTransaction;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::PromptUser(TSessionData * Data, AnsiString Prompt,
  TPromptKind Kind, AnsiString & Response)
{
  // If PromptUser is overriden in descendant class, the overriden version
  // is not called when accessed via TSessionIU interface.
  // So this is workaround.
  return DoPromptUser(Data, Prompt, Kind, Response);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoPromptUser(TSessionData * /*Data*/, AnsiString Prompt,
  TPromptKind Kind, AnsiString & Response)
{
  bool Result = false;

  if (OnPromptUser != NULL)
  {
    OnPromptUser(this, Prompt, Kind, Response, Result, NULL);
  }

  if ((Configuration->RememberPassword) &&
      ((Kind == pkPassword) || (Kind == pkPassphrase) || (Kind == pkServerPrompt)))
  {
    FPassword = EncryptPassword(Response, SessionData->SessionName);
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::QueryUser(const AnsiString Query,
  TStrings * MoreMessages, int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  LogEvent(FORMAT("Asking user:\n%s (%s)", (Query, (MoreMessages ? MoreMessages->CommaText : AnsiString() ))));
  int Answer = AbortAnswer(Answers);
  if (FOnQueryUser)
  {
    FOnQueryUser(this, Query, MoreMessages, Answers, Params, Answer, QueryType, NULL);
  }
  return Answer;
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::QueryUserException(const AnsiString Query,
  Exception * E, int Answers, const TQueryParams * Params,
  TQueryType QueryType)
{
  int Result;
  TStrings * MoreMessages = new TStringList();
  try
  {
    if (!E->Message.IsEmpty() && !Query.IsEmpty())
    {
      MoreMessages->Add(E->Message);
    }

    ExtException * EE = dynamic_cast<ExtException*>(E);
    if ((EE != NULL) && (EE->MoreMessages != NULL))
    {
      MoreMessages->AddStrings(EE->MoreMessages);
    }
    Result = QueryUser(!Query.IsEmpty() ? Query : E->Message,
      MoreMessages->Count ? MoreMessages : NULL,
      Answers, Params, QueryType);
  }
  __finally
  {
    delete MoreMessages;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DisplayBanner(const AnsiString & Banner)
{
  if (OnDisplayBanner != NULL)
  {
    if (Configuration->ForceBanners ||
        Configuration->ShowBanner(SessionData->SessionKey, Banner))
    {
      bool NeverShowAgain = false;
      int Options =
        FLAGMASK(Configuration->ForceBanners, boDisableNeverShowAgain);
      OnDisplayBanner(this, SessionData->SessionName, Banner,
        NeverShowAgain, Options);
      if (!Configuration->ForceBanners && NeverShowAgain)
      {
        Configuration->NeverShowBanner(SessionData->SessionKey, Banner);
      }
    }
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
void __fastcall TTerminal::DoInformation(const AnsiString & Str, bool Status,
  bool Active)
{
  if (Active)
  {
    FAnyInformation = true;
  }

  if (OnInformation)
  {
    OnInformation(this, Str, Status, Active);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Information(const AnsiString & Str, bool Status)
{
  DoInformation(Str, Status);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetIsCapable(TFSCapability Capability) const
{
  assert(FFileSystem);
  return FFileSystem->IsCapable(Capability);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::AbsolutePath(AnsiString Path)
{
  return FFileSystem->AbsolutePath(Path);
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
      ModifiesFiles = true;
      break;

    case fsAnyCommand:
      ChangesDirectory = true;
      ModifiesFiles = true;
      break;
  }

  if (ChangesDirectory)
  {
    if (!FInTransaction)
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
    else
  if (ModifiesFiles && AutoReadDirectory && Configuration->AutoReadDirectoryAfterOp)
  {
    if (!FInTransaction) ReadDirectory(true);
      else FReadDirectoryPending = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::TerminalError(AnsiString Msg)
{
  TerminalError(NULL, Msg);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::TerminalError(Exception * E, AnsiString Msg)
{
  throw ETerminal(E, Msg);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DoQueryReopen(Exception * E)
{

  LogEvent("Connection was lost, asking what to do.");

  TQueryParams Params(qpAllowContinueOnError);
  Params.Timeout = Configuration->SessionReopenAuto;
  Params.TimeoutAnswer = qaRetry;
  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(RECONNECT_BUTTON);
  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);
  return (QueryUserException("", E, qaRetry | qaAbort, &Params, qtError) == qaRetry);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::QueryReopen(Exception * E, int Params,
  TFileOperationProgressType * OperationProgress)
{
  TSuspendFileOperationProgress Suspend(OperationProgress);

  bool Result = DoQueryReopen(E);

  if (Result)
  {
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
          Result = DoQueryReopen(&E);
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
  TFileOperationProgressType * OperationProgress, const AnsiString Message,
  bool AllowSkip, AnsiString SpecialRetry)
{
  bool Result = false;
  Log->AddException(&E);
  int Answer;

  if (AllowSkip && OperationProgress->SkipToAll)
  {
    Answer = qaSkip;
  }
  else
  {
    int Answers = qaRetry | qaAbort |
      FLAGMASK(AllowSkip, (qaSkip | qaAll)) |
      FLAGMASK(!SpecialRetry.IsEmpty(), qaYes);
    TQueryParams Params(qpAllowContinueOnError | FLAGMASK(!AllowSkip, qpFatalAbort));
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

    SUSPEND_OPERATION (
      Answer = QueryUserException(Message, &E, Answers, &Params, qtError);
    );

    if (Answer == qaAll)
    {
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
    if (Answer == qaAbort)
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
  const AnsiString Message, void * Param1, void * Param2)
{
  assert(CallBackFunc);
  int Result;
  FILE_OPERATION_LOOP_EX
  (
    AllowSkip, Message,
    Result = CallBackFunc(Param1, Param2);
  );

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::TranslateLockedPath(AnsiString Path, bool Lock)
{
  if (!SessionData->LockInHome || Path.IsEmpty() || (Path[1] != '/'))
    return Path;

  if (Lock)
  {
    if (Path.SubString(1, FLockDirectory.Length()) == FLockDirectory)
    {
      Path.Delete(1, FLockDirectory.Length());
      if (Path.IsEmpty()) Path = "/";
    }
  }
  else
  {
    Path = UnixExcludeTrailingBackslash(FLockDirectory + Path);
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
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ClearCachedFileList(const AnsiString Path,
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
bool __fastcall TTerminal::DirectoryFileList(const AnsiString Path,
  TRemoteFileList *& FileList, bool CanLoad)
{
  bool Result = false;
  if (UnixComparePaths(FFiles->Directory, Path))
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
void __fastcall TTerminal::SetCurrentDirectory(AnsiString value)
{
  assert(FFileSystem);
  value = TranslateLockedPath(value, false);
  if (value != FFileSystem->CurrentDirectory)
  {
    ChangeDirectory(value);
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::GetCurrentDirectory()
{
  if (FFileSystem)
  {
    FCurrentDirectory = FFileSystem->CurrentDirectory;
    if (FCurrentDirectory.IsEmpty())
    {
      ReadCurrentDirectory();
    }
  }

  return TranslateLockedPath(FCurrentDirectory, true);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::PeekCurrentDirectory()
{
  if (FFileSystem)
  {
    FCurrentDirectory = FFileSystem->CurrentDirectory;
  }

  return TranslateLockedPath(FCurrentDirectory, true);
}
//---------------------------------------------------------------------------
TUsersGroupsList * __fastcall TTerminal::GetGroups()
{
  assert(FFileSystem);
  if (!FUsersGroupsLookedup && SessionData->LookupUserGroups &&
      IsCapable[fcUserGroupListing])
  {
    LookupUsersGroups();
  }
  return FGroups;
}
//---------------------------------------------------------------------------
TUsersGroupsList * __fastcall TTerminal::GetUsers()
{
  assert(FFileSystem);
  if (!FUsersGroupsLookedup && SessionData->LookupUserGroups &&
      IsCapable[fcUserGroupListing])
  {
    LookupUsersGroups();
  }
  return FUsers;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::GetUserName() const
{
  // in future might also be implemented to detect username similar to GetUserGroups
  assert(FFileSystem != NULL);
  AnsiString Result = FFileSystem->GetUserName();
  // Is empty also when stored password was used
  if (Result.IsEmpty())
  {
    Result = SessionData->UserName;
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
void __fastcall TTerminal::DoChangeDirectory()
{
  if (FOnChangeDirectory)
  {
    FOnChangeDirectory(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectory(bool ReloadOnly)
{
  if (FOnReadDirectory)
  {
    FOnReadDirectory(this, ReloadOnly);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartReadDirectory()
{
  if (FOnStartReadDirectory)
  {
    FOnStartReadDirectory(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoReadDirectoryProgress(int Progress, bool & Cancel)
{
  if (FReadingCurrentDirectory && (FOnReadDirectoryProgress != NULL))
  {
    FOnReadDirectoryProgress(this, Progress, Cancel);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::BeginTransaction()
{
  if (!FInTransaction)
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
  if (!FInTransaction)
    TerminalError("Can't end transaction, not in transaction");
  assert(FInTransaction > 0);
  FInTransaction--;

  // it connection was closed due to fatal error during transaction, do nothing
  if (Active)
  {
    if (FInTransaction == 0)
    {
      try
      {
        if (FReadCurrentDirectoryPending) ReadCurrentDirectory();
        if (FReadDirectoryPending) ReadDirectory(!FReadCurrentDirectoryPending);
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
  if (value) FExceptionOnFail++;
    else
  {
    if (FExceptionOnFail == 0)
      throw Exception("ExceptionOnFail is already zero.");
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
void __fastcall TTerminal::FatalError(Exception * E, AnsiString Msg)
{
  // the same code is in TSecureShell
  if (Active)
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
void __fastcall TTerminal::CommandError(Exception * E, const AnsiString Msg)
{
  CommandError(E, Msg, 0);
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::CommandError(Exception * E, const AnsiString Msg,
  int Answers)
{
  int Result = 0;
  if (E && E->InheritsFrom(__classid(EFatal)))
  {
    FatalError(E, Msg);
  }
  else if (E && E->InheritsFrom(__classid(EAbort)))
  {
    // resent EAbort exception
    Abort();
  }
  else if (ExceptionOnFail)
  {
    throw ECommand(E, Msg);
  }
  else if (!Answers)
  {
    ECommand * ECmd = new ECommand(E, Msg);
    try
    {
      ShowExtendedException(ECmd);
    }
    __finally
    {
      delete ECmd;
    }
  }
  else
  {
    // small hack to anable "skip to all" for COMMAND_ERROR_ARI
    bool CanSkip = FLAGSET(Answers, qaSkip) && (OperationProgress != NULL);
    if (CanSkip && OperationProgress->SkipToAll)
    {
      Result = qaSkip;
    }
    else
    {
      TQueryParams Params(qpAllowContinueOnError);
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
        assert(OperationProgress != NULL);
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
void __fastcall TTerminal::CloseOnCompletion(const AnsiString Message)
{
  LogEvent("Closing session after completed operation (as requested by user)");
  Close();
  throw ESshTerminate(NULL,
    Message.IsEmpty() ? LoadStr(CLOSED_ON_COMPLETION) : Message);
}
//---------------------------------------------------------------------------
int __fastcall TTerminal::ConfirmFileOverwrite(const AnsiString FileName,
  const TOverwriteFileParams * FileParams, int Answers, const TQueryParams * Params,
  TOperationSide Side, TFileOperationProgressType * OperationProgress)
{
  int Answer;
  int AnswerForNewer =
    (CompareFileTime(FileParams->SourceTimestamp, FileParams->DestTimestamp) > 0) ?
    qaYes : qaNo;
  if (OperationProgress->YesToNewer)
  {
    Answer = AnswerForNewer;
  }
  else
  {
    AnsiString Message = FMTLOAD((Side == osLocal ? LOCAL_FILE_OVERWRITE :
      REMOTE_FILE_OVERWRITE), (FileName));
    if (FileParams)
    {
      Message = FMTLOAD(FILE_OVERWRITE_DETAILS, (Message,
        IntToStr(FileParams->SourceSize),
        UserModificationStr(FileParams->SourceTimestamp, FileParams->SourcePrecision),
        IntToStr(FileParams->DestSize),
        UserModificationStr(FileParams->DestTimestamp, FileParams->DestPrecision)));
    }
    Answer = QueryUser(Message, NULL, Answers, Params);
    switch (Answer)
    {
      case qaNeverAskAgain:
        Configuration->ConfirmOverwriting = false;
        Answer = qaYes;
        break;

      case qaYesToAll:
        OperationProgress->YesToAll = true;
        Answer = qaYes;
        break;

      case qaAll:
        OperationProgress->YesToNewer = true;
        Answer = AnswerForNewer;
        break;

      case qaNoToAll:
        OperationProgress->NoToAll = true;
        Answer = qaNo;
    }
  }
  return Answer;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileModified(const TRemoteFile * File,
  const AnsiString FileName, bool ClearDirectoryChange)
{
  AnsiString ParentDirectory;
  AnsiString Directory;

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
void __fastcall TTerminal::DoDirectoryModified(const AnsiString Path, bool SubDirs)
{
  if (OnDirectoryModified != NULL)
  {
    OnDirectoryModified(this, Path, SubDirs);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryModified(const AnsiString Path, bool SubDirs)
{
  if (Path.IsEmpty())
  {
    ClearCachedFileList(CurrentDirectory, SubDirs);
  }
  else
  {
    ClearCachedFileList(Path, SubDirs);
  }
  DoDirectoryModified(Path, SubDirs);
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
    assert(FDirectoryChangesCache != NULL);
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
  if (SessionData->CacheDirectories &&
      FDirectoryCache->HasNewerFileList(CurrentDirectory, FFiles->Timestamp))
  {
    // Second parameter was added to allow (rather force) using the cache.
    // Before, the directory was reloaded always, it seems useless,
    // has it any reason?
    ReadDirectory(true, true);
    FReadDirectoryPending = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EnsureNonExistence(const AnsiString FileName)
{
  // if filename doesn't contain path, we check for existence of file
  if ((UnixExtractFileDir(FileName).IsEmpty()) &&
      UnixComparePaths(CurrentDirectory, FFiles->Directory))
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
void __fastcall inline TTerminal::LogEvent(const AnsiString & Str)
{
  if (Log->Logging)
  {
    Log->Add(llMessage, Str);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoStartup()
{
  LogEvent("Doing startup conversation with host.");
  BeginTransaction();
  try
  {
    DoInformation(LoadStr(STATUS_STARTUP), true);

    // Make sure that directory would be loaded at last
    FReadCurrentDirectoryPending = true;
    FReadDirectoryPending = AutoReadDirectory;

    FFileSystem->DoStartup();

    if (SessionData->LookupUserGroups && IsCapable[fcUserGroupListing])
    {
      LookupUsersGroups();
    }

    DoInformation(LoadStr(STATUS_OPEN_DIRECTORY), true);
    if (!SessionData->RemoteDirectory.IsEmpty())
    {
      ChangeDirectory(SessionData->RemoteDirectory);
    }

  }
  __finally
  {
    EndTransaction();
  }
  LogEvent("Startup conversation with host finished.");
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadCurrentDirectory()
{
  assert(FFileSystem);
  try
  {
    // reset flag is case we are called externally (like from console dialog)
    FReadCurrentDirectoryPending = false;

    LogEvent("Getting current directory name.");
    AnsiString OldDirectory = FFileSystem->CurrentDirectory;

    FFileSystem->ReadCurrentDirectory();
    ReactOnCommand(fsCurrentDirectory);

    if (SessionData->CacheDirectoryChanges)
    {
      assert(FDirectoryChangesCache != NULL);
      FDirectoryChangesCache->AddDirectoryChange(OldDirectory,
        FLastDirectoryChange, CurrentDirectory);
      // not to broke the cache, if the next directory change would not
      // be initialited by ChangeDirectory(), which sets it
      // (HomeDirectory() particularly)
      FLastDirectoryChange = "";
    }

    if (OldDirectory.IsEmpty())
    {
      FLockDirectory = (SessionData->LockInHome ?
        FFileSystem->CurrentDirectory : AnsiString(""));
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
      LogEvent("Cached directory not reloaded.");
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
        LogEvent("Directory content loaded from cache.");
      }
      else
      {
        LogEvent("Cached Directory content has been removed.");
      }
    }
  }

  if (!LoadedFromCache)
  {
    DoStartReadDirectory();
    FReadingCurrentDirectory = true;
    bool Cancel = false; // dummy
    DoReadDirectoryProgress(0, Cancel);

    try
    {
      TRemoteDirectory * Files = new TRemoteDirectory(this);
      try
      {
        Files->Directory = CurrentDirectory;
        CustomReadDirectory(Files);
      }
      __finally
      {
        DoReadDirectoryProgress(-1, Cancel);
        FReadingCurrentDirectory = false;
        delete FFiles;
        FFiles = Files;
        DoReadDirectory(ReloadOnly);
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
void __fastcall TTerminal::CustomReadDirectory(TRemoteFileList * FileList)
{
  assert(FileList);
  assert(FFileSystem);
  FFileSystem->ReadDirectory(FileList);
  ReactOnCommand(fsListDirectory);
}
//---------------------------------------------------------------------------
TRemoteFileList * TTerminal::ReadDirectoryListing(AnsiString Directory, bool UseCache)
{
  TRemoteFileList * FileList;
  try
  {
    FileList = new TRemoteFileList();
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
      FileList = NULL;
      throw;
    }
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      "",
      FileList = ReadDirectoryListing(Directory, UseCache);
    );
  }
  return FileList;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessDirectory(const AnsiString DirName,
  TProcessFileEvent CallBackFunc, void * Param, bool UseCache)
{
  TRemoteFileList * FileList = ReadDirectoryListing(DirName, UseCache);

  // skip if directory listing fails and user selects "skip"
  if (FileList)
  {
    try
    {
      AnsiString Directory = UnixIncludeTrailingBackslash(DirName);

      TRemoteFile * File;
      for (int Index = 0; Index < FileList->Count; Index++)
      {
        File = FileList->Files[Index];
        if (!File->IsParentDirectory && !File->IsThisDirectory)
        {
          CallBackFunc(Directory + File->FileName, File, Param);
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
  assert(FFileSystem);
  try
  {
    LogEvent(FORMAT("Reading symlink \"%s\".", (SymlinkFile->FileName)));
    FFileSystem->ReadSymlink(SymlinkFile, File);
    ReactOnCommand(fsReadSymlink);
  }
  catch (Exception &E)
  {
    CommandError(&E, FMTLOAD(READ_SYMLINK_ERROR, (SymlinkFile->FileName)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadFile(const AnsiString FileName,
  TRemoteFile *& File)
{
  assert(FFileSystem);
  File = NULL;
  try
  {
    LogEvent(FORMAT("Listing file \"%s\".", (FileName)));
    FFileSystem->ReadFile(FileName, File);
    ReactOnCommand(fsListFile);
  }
  catch (Exception &E)
  {
    if (File) delete File;
    File = NULL;
    CommandError(&E, FMTLOAD(CANT_GET_ATTRS, (FileName)));
  }
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
  assert(FFileSystem);
  assert(FileList);

  bool Result = false;
  bool DisconnectWhenComplete = false;

  try
  {
    TFileOperationProgressType Progress(FOnProgress, FOnFinished);
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
        AnsiString FileName;
        bool Success;
        while ((Index < FileList->Count) && (Progress.Cancel == csContinue))
        {
          FileName = FileList->Strings[Index];
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
            Progress.Finish(FileName, Success, DisconnectWhenComplete);
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
    DisconnectWhenComplete = false;
    // this was missing here. was it by purpose?
    // without it any error message is lost
    throw;
  }

  if (DisconnectWhenComplete)
  {
    CloseOnCompletion();
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
  assert(FFileSystem != NULL);
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
    FLAGMASK(FLAGSET(Params, cpDelete), cpaNoExcludeMask) |
    FLAGMASK(FLAGSET(Params, cpDelete), cpaNoClearArchive) |
    FLAGMASK(!IsCapable[fcIgnorePermErrors], cpaNoIgnorePermErrors);
  Result.Download = Result.General | cpaNoClearArchive | cpaNoRights |
    cpaNoIgnorePermErrors;
  Result.Upload = Result.General | cpaNoPreserveReadOnly |
    FLAGMASK(!IsCapable[fcModeChangingUpload], cpaNoRights) |
    FLAGMASK(!IsCapable[fcPreservingTimestampUpload], cpaNoPreserveTime);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::IsRecycledFile(AnsiString FileName)
{
  AnsiString Path = UnixExtractFilePath(FileName);
  if (Path.IsEmpty())
  {
    Path = CurrentDirectory;
  }
  return UnixComparePaths(Path, SessionData->RecycleBinPath);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RecycleFile(AnsiString FileName,
  const TRemoteFile * File)
{
  if (FileName.IsEmpty())
  {
    assert(File != NULL);
    FileName = File->FileName;
  }

  if (!IsRecycledFile(FileName))
  {
    LogEvent(FORMAT("Moving file \"%s\" to remote recycle bin '%s'.",
      (FileName, SessionData->RecycleBinPath)));

    TMoveFileParams Params;
    Params.Target = SessionData->RecycleBinPath;
    Params.FileMask = FORMAT("*-%s.*", (FormatDateTime("yyyymmdd-hhnnss", Now())));

    MoveFile(FileName, File, &Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteFile(AnsiString FileName,
  const TRemoteFile * File, void * Recursive)
{
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  if (OperationProgress && OperationProgress->Operation == foDelete)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
  if (SessionData->DeleteToRecycleBin && !IsRecycledFile(FileName))
  {
    RecycleFile(FileName, File);
  }
  else
  {
    LogEvent(FORMAT("Deleting file \"%s\".", (FileName)));
    if (File) FileModified(File, FileName, true);
    DoDeleteFile(FileName, File, Recursive);
    ReactOnCommand(fsDeleteFile);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoDeleteFile(const AnsiString FileName,
  const TRemoteFile * File, void * Recursive)
{
  try
  {
    assert(FFileSystem);
    // 'File' parameter: SFTPFileSystem needs to know if file is file or directory
    FFileSystem->DeleteFile(FileName, File,
      Recursive ? *((bool*)Recursive) : true);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(DELETE_FILE_ERROR, (FileName)),
      DoDeleteFile(FileName, File, Recursive)
    );
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteFiles(TStrings * FilesToDelete, bool * Recursive)
{
  // TODO: avoid resolving symlinks while reading subdirectories.
  // Resolving does not work anyway for relative symlinks in subdirectories
  // (at least for SFTP).
  return ProcessFiles(FilesToDelete, foDelete, DeleteFile, Recursive);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DeleteLocalFile(AnsiString FileName,
  const TRemoteFile * /*File*/, void * /*Param*/)
{
  if (OnDeleteLocalFile == NULL)
  {
    if (!RecursiveDeleteFile(FileName, false))
    {
      throw Exception(FMTLOAD(DELETE_FILE_ERROR, (FileName)));
    }
  }
  else
  {
    OnDeleteLocalFile(FileName);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::DeleteLocalFiles(TStrings * FileList)
{
  return ProcessFiles(FileList, foDelete, DeleteLocalFile, NULL, osLocal);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFile(AnsiString FileName,
  const TRemoteFile * File, void * AParams)
{
  TCustomCommandParams * Params = ((TCustomCommandParams *)AParams);
  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  if (OperationProgress && OperationProgress->Operation == foCustomCommand)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
  LogEvent(FORMAT("Executing custom command \"%s\" (%d) on file \"%s\".",
    (Params->Command, Params->Params, FileName)));
  if (File) FileModified(File, FileName);
  DoCustomCommandOnFile(FileName, File, Params->Command, Params->Params,
    Params->OutputEvent);
  ReactOnCommand(fsAnyCommand);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCustomCommandOnFile(AnsiString FileName,
  const TRemoteFile * File, AnsiString Command, int Params,
  TCaptureOutputEvent OutputEvent)
{
  try
  {
    if (IsCapable[fcAnyCommand])
    {
      assert(FFileSystem);
      assert(fcShellAnyCommand);
      FFileSystem->CustomCommandOnFile(FileName, File, Command, Params, OutputEvent);
    }
    else
    {
      assert(CommandSessionOpened);
      assert(FCommandSession->FSProtocol == cfsSCP);
      LogEvent("Executing custom command on command session.");

      FCommandSession->CurrentDirectory = CurrentDirectory;
      FCommandSession->FFileSystem->CustomCommandOnFile(FileName, File, Command,
        Params, OutputEvent);
    }
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(CUSTOM_COMMAND_ERROR, (Command, FileName)),
      DoCustomCommandOnFile(FileName, File, Command, Params, OutputEvent)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFiles(AnsiString Command,
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
    AnsiString FileList;
    for (int i = 0; i < Files->Count; i++)
    {
      TRemoteFile * File = static_cast<TRemoteFile *>(Files->Objects[i]);
      bool Dir = File->IsDirectory && !File->IsSymLink;

      if (!Dir || FLAGSET(Params, ccApplyToDirectories))
      {
        if (!FileList.IsEmpty())
        {
          FileList += " ";
        }

        FileList += "\"" + ShellDelimitStr(Files->Strings[i], '"') + "\"";
      }
    }

    AnsiString Cmd = TRemoteCustomCommand("", FileList).Complete(Command, true);
    AnyCommand(Cmd, OutputEvent);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFileProperties(AnsiString FileName,
  const TRemoteFile * File, /*const TRemoteProperties*/ void * Properties)
{
  TRemoteProperties * RProperties = (TRemoteProperties *)Properties;
  assert(RProperties && !RProperties->Valid.Empty());

  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  if (OperationProgress && OperationProgress->Operation == foSetProperties)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
  if (Log->Logging)
  {
    LogEvent(FORMAT("Changing properties of \"%s\" (%s)",
      (FileName, BooleanToEngStr(RProperties->Recursive))));
    if (RProperties->Valid.Contains(vpRights))
    {
      LogEvent(FORMAT(" - mode: \"%s\"", (RProperties->Rights.ModeStr)));
    }
    if (RProperties->Valid.Contains(vpGroup))
    {
      LogEvent(FORMAT(" - group: \"%s\"", (RProperties->Group)));
    }
    if (RProperties->Valid.Contains(vpOwner))
    {
      LogEvent(FORMAT(" - owner: \"%s\"", (RProperties->Owner)));
    }
    if (RProperties->Valid.Contains(vpModification))
    {
      LogEvent(FORMAT(" - modification: \"%s\"",
        (FormatDateTime("dddddd tt",
           UnixToDateTime(RProperties->Modification, SessionData->DSTMode)))));
    }
    if (RProperties->Valid.Contains(vpLastAccess))
    {
      LogEvent(FORMAT(" - last access: \"%s\"",
        (FormatDateTime("dddddd tt",
           UnixToDateTime(RProperties->LastAccess, SessionData->DSTMode)))));
    }
  }
  FileModified(File, FileName);
  DoChangeFileProperties(FileName, File, RProperties);
  ReactOnCommand(fsChangeProperties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoChangeFileProperties(const AnsiString FileName,
  const TRemoteFile * File, const TRemoteProperties * Properties)
{
  try
  {
    assert(FFileSystem);
    FFileSystem->ChangeFileProperties(FileName, File, Properties);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(CHANGE_PROPERTIES_ERROR, (FileName)),
      DoChangeFileProperties(FileName, File, Properties)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeFilesProperties(TStrings * FileList,
  const TRemoteProperties * Properties)
{
  AnnounceFileListOperation();
  ProcessFiles(FileList, foSetProperties, ChangeFileProperties, (void *)Properties);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::LoadFilesProperties(TStrings * FileList)
{
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
void __fastcall TTerminal::CalculateFileSize(AnsiString FileName,
  const TRemoteFile * File, /*TCalculateSizeParams*/ void * Param)
{
  assert(Param);
  assert(File);
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Param);

  if (FileName.IsEmpty())
  {
    FileName = File->FileName;
  }

  bool AllowTransfer = (AParams->CopyParam == NULL);
  if (!AllowTransfer)
  {
    TFileMasks::TParams MaskParams;
    MaskParams.Size = File->Size;

    AllowTransfer = AParams->CopyParam->AllowTransfer(
      UnixExcludeTrailingBackslash(File->FullFileName), osRemote, File->IsDirectory,
      MaskParams);
  }

  if (AllowTransfer)
  {
    if (File->IsDirectory)
    {
      if (!File->IsSymLink)
      {
        LogEvent(FORMAT("Getting size of directory \"%s\"", (FileName)));
        // pass in full path so we get it back in file list for AllowTransfer() exclusion
        DoCalculateDirectorySize(File->FullFileName, File, AParams);
      }
      else
      {
        AParams->Size += File->Size;
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

  if (OperationProgress && OperationProgress->Operation == foCalculateSize)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCalculateDirectorySize(const AnsiString FileName,
  const TRemoteFile * File, TCalculateSizeParams * Params)
{
  try
  {
    ProcessDirectory(FileName, CalculateFileSize, Params);
  }
  catch(Exception & E)
  {
    if (!Active || ((Params->Params & csIgnoreErrors) == 0))
    {
      COMMAND_ERROR_ARI
      (
        FMTLOAD(CALCULATE_SIZE_ERROR, (FileName)),
        DoCalculateDirectorySize(FileName, File, Params)
      );
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFilesSize(TStrings * FileList,
  __int64 & Size, int Params, const TCopyParamType * CopyParam,
  TCalculateSizeStats * Stats)
{
  TCalculateSizeParams Param;
  Param.Size = 0;
  Param.Params = Params;
  Param.CopyParam = CopyParam;
  Param.Stats = Stats;
  ProcessFiles(FileList, foCalculateSize, CalculateFileSize, &Param);
  Size = Param.Size;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFilesChecksum(const AnsiString & Alg,
  TStrings * FileList, TStrings * Checksums,
  TCalculatedChecksumEvent OnCalculatedChecksum)
{
  FFileSystem->CalculateFilesChecksum(Alg, FileList, Checksums, OnCalculatedChecksum);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RenameFile(const AnsiString FileName,
  const AnsiString NewName)
{
  LogEvent(FORMAT("Renaming file \"%s\" to \"%s\".", (FileName, NewName)));
  DoRenameFile(FileName, NewName, false);
  ReactOnCommand(fsRenameFile);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::RenameFile(const TRemoteFile * File,
  const AnsiString NewName, bool CheckExistence)
{
  assert(File && File->Directory == FFiles);
  bool Proceed = true;
  // if filename doesn't contain path, we check for existence of file
  if ((File->FileName != NewName) && CheckExistence &&
      Configuration->ConfirmOverwriting &&
      UnixComparePaths(CurrentDirectory, FFiles->Directory))
  {
    TRemoteFile * DuplicateFile = FFiles->FindFile(NewName);
    if (DuplicateFile)
    {
      AnsiString QuestionFmt;
      if (DuplicateFile->IsDirectory) QuestionFmt = LoadStr(DIRECTORY_OVERWRITE);
        else QuestionFmt = LoadStr(FILE_OVERWRITE);
      int Result;
      TQueryParams Params(qpNeverAskAgainCheck);
      Result = QueryUser(FORMAT(QuestionFmt, (NewName)), NULL,
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
void __fastcall TTerminal::DoRenameFile(const AnsiString FileName,
  const AnsiString NewName, bool Move)
{
  try
  {
    assert(FFileSystem);
    FFileSystem->RenameFile(FileName, NewName);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(Move ? MOVE_FILE_ERROR : RENAME_FILE_ERROR, (FileName, NewName)),
      DoRenameFile(FileName, NewName, Move)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MoveFile(const AnsiString FileName,
  const TRemoteFile * File, /*const TMoveFileParams*/ void * Param)
{
  if (OperationProgress &&
      ((OperationProgress->Operation == foRemoteMove) ||
       (OperationProgress->Operation == foDelete)))
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }

  assert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  AnsiString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT("Moving file \"%s\" to \"%s\".", (FileName, NewName)));
  FileModified(File, FileName);
  DoRenameFile(FileName, NewName, true);
  ReactOnCommand(fsMoveFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::MoveFiles(TStrings * FileList, const AnsiString Target,
  const AnsiString FileMask)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  DirectoryModified(Target, true);
  return ProcessFiles(FileList, foRemoteMove, MoveFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCopyFile(const AnsiString FileName,
  const AnsiString NewName)
{
  try
  {
    assert(FFileSystem);
    if (IsCapable[fcRemoteCopy])
    {
      FFileSystem->CopyFile(FileName, NewName);
    }
    else
    {
      assert(CommandSessionOpened);
      assert(FCommandSession->FSProtocol == cfsSCP);
      LogEvent("Copying file on command session.");
      FCommandSession->CurrentDirectory = CurrentDirectory;
      FCommandSession->FFileSystem->CopyFile(FileName, NewName);
    }
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(COPY_FILE_ERROR, (FileName, NewName)),
      DoCopyFile(FileName, NewName)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CopyFile(const AnsiString FileName,
  const TRemoteFile * /*File*/, /*const TMoveFileParams*/ void * Param)
{
  if (OperationProgress && (OperationProgress->Operation == foRemoteCopy))
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }

  assert(Param != NULL);
  const TMoveFileParams & Params = *static_cast<const TMoveFileParams*>(Param);
  AnsiString NewName = UnixIncludeTrailingBackslash(Params.Target) +
    MaskFileName(UnixExtractFileName(FileName), Params.FileMask);
  LogEvent(FORMAT("Copying file \"%s\" to \"%s\".", (FileName, NewName)));
  DoCopyFile(FileName, NewName);
  ReactOnCommand(fsCopyFile);
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyFiles(TStrings * FileList, const AnsiString Target,
  const AnsiString FileMask)
{
  TMoveFileParams Params;
  Params.Target = Target;
  Params.FileMask = FileMask;
  DirectoryModified(Target, true);
  return ProcessFiles(FileList, foRemoteCopy, CopyFile, &Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateDirectory(const AnsiString DirName,
  const TRemoteProperties * Properties)
{
  assert(FFileSystem);
  EnsureNonExistence(DirName);
  FileModified(NULL, DirName);

  LogEvent(FORMAT("Creating directory \"%s\".", (DirName)));
  DoCreateDirectory(DirName, Properties);
  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateDirectory(const AnsiString DirName,
  const TRemoteProperties * Properties)
{
  try
  {
    assert(FFileSystem);
    FFileSystem->CreateDirectory(DirName, Properties);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(CREATE_DIR_ERROR, (DirName)),
      DoCreateDirectory(DirName, Properties)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CreateLink(const AnsiString FileName,
  const AnsiString PointTo, bool Symbolic)
{
  assert(FFileSystem);
  EnsureNonExistence(FileName);
  if (SessionData->CacheDirectories)
  {
    DirectoryModified(CurrentDirectory, false);
  }

  LogEvent(FORMAT("Creating link \"%s\" to \"%s\" (symbolic: %s).",
    (FileName, PointTo, BooleanToEngStr(Symbolic))));
  DoCreateLink(FileName, PointTo, Symbolic);
  ReactOnCommand(fsCreateDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCreateLink(const AnsiString FileName,
  const AnsiString PointTo, bool Symbolic)
{
  try
  {
    assert(FFileSystem);
    FFileSystem->CreateLink(FileName, PointTo, Symbolic);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(CREATE_LINK_ERROR, (FileName)),
      DoCreateLink(FileName, PointTo, Symbolic);
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::HomeDirectory()
{
  assert(FFileSystem);
  try
  {
    LogEvent("Changing directory to home directory.");
    FFileSystem->HomeDirectory();
    ReactOnCommand(fsHomeDirectory);
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(CHANGE_HOMEDIR_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ChangeDirectory(const AnsiString Directory)
{
  assert(FFileSystem);
  try
  {
    AnsiString CachedDirectory;
    assert(!SessionData->CacheDirectoryChanges || (FDirectoryChangesCache != NULL));
    // never use directory change cache during startup, this ensures, we never
    // end-up initially in non-existing directory
    if ((Status == ssOpened) &&
        SessionData->CacheDirectoryChanges &&
        FDirectoryChangesCache->GetDirectoryChange(PeekCurrentDirectory(),
          Directory, CachedDirectory))
    {
      LogEvent(FORMAT("Cached directory change via \"%s\" to \"%s\".",
        (Directory, CachedDirectory)));
      FFileSystem->CachedChangeDirectory(CachedDirectory);
    }
    else
    {
      LogEvent(FORMAT("Changing directory to \"%s\".", (Directory)));
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
  assert(FFileSystem);
  assert(IsCapable[fcUserGroupListing]);

  try
  {
    FUsersGroupsLookedup = true;
    LogEvent("Looking up groups and users.");
    FFileSystem->LookupUsersGroups();
    ReactOnCommand(fsLookupUsersGroups);

    if (Log->Logging)
    {
      if (FGroups->Count > 0)
      {
        LogEvent("Following groups found:");
        for (int Index = 0; Index < FGroups->Count; Index++)
        {
          LogEvent(AnsiString("  ") + FGroups->Strings[Index]);
        }
      }
      else
      {
        LogEvent("No groups found.");
      }

      if (FUsers->Count > 0)
      {
        LogEvent("Following users found:");
        for (int Index = 0; Index < FUsers->Count; Index++)
        {
          LogEvent(AnsiString("  ") + FUsers->Strings[Index]);
        }
      }
      else
      {
        LogEvent("No users found.");
      }
    }
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(LOOKUP_GROUPS_ERROR));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::AllowedAnyCommand(const AnsiString Command)
{
  return !Command.Trim().IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetCommandSessionOpened()
{
  // consider secodary terminal open in "ready" state only
  // so we never do keepalives on it until it is completelly initialised
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
    // transaction cannot be started yet to allow proper matching transation
    // levels between main and command session
    assert(!FInTransaction);

    try
    {
      FCommandSession = new TSecondaryTerminal(this, SessionData,
        Configuration, "Shell");

      FCommandSession->AutoReadDirectory = false;

      FCommandSession->FSessionData->RemoteDirectory = CurrentDirectory;
      FCommandSession->FSessionData->FSProtocol = fsSCPonly;

      FCommandSession->FExceptionOnFail = FExceptionOnFail;

      FCommandSession->OnQueryUser = OnQueryUser;
      FCommandSession->OnPromptUser = OnPromptUser;
      FCommandSession->OnShowExtendedException = OnShowExtendedException;
      FCommandSession->OnProgress = OnProgress;
      FCommandSession->OnFinished = OnFinished;
      FCommandSession->OnInformation = OnInformation;
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
void __fastcall TTerminal::AnyCommand(const AnsiString Command,
  TCaptureOutputEvent OutputEvent)
{
  assert(FFileSystem);
  try
  {
    DirectoryModified(CurrentDirectory, false);
    if (IsCapable[fcAnyCommand])
    {
      LogEvent("Executing user defined command.");
      FFileSystem->AnyCommand(Command, OutputEvent);
    }
    else
    {
      assert(CommandSessionOpened);
      assert(FCommandSession->FSProtocol == cfsSCP);
      LogEvent("Executing user defined command on command session.");

      FCommandSession->CurrentDirectory = CurrentDirectory;
      FCommandSession->FFileSystem->AnyCommand(Command, OutputEvent);

      FCommandSession->FFileSystem->ReadCurrentDirectory();

      // synchronize pwd (by purpose we lose transaction optimalisation here)
      ChangeDirectory(FCommandSession->CurrentDirectory);
    }
    ReactOnCommand(fsAnyCommand);
  }
  catch (Exception &E)
  {
    if (ExceptionOnFail || (E.InheritsFrom(__classid(EFatal)))) throw;
      else ShowExtendedException(&E);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CreateLocalFile(const AnsiString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
  bool NoConfirmation)
{
  assert(AHandle);
  bool Result = true;

  FILE_OPERATION_LOOP (FMTLOAD(CREATE_FILE_ERROR, (FileName)),
    bool Done;
    unsigned int CreateAttr = FILE_ATTRIBUTE_NORMAL;
    do
    {
      *AHandle = CreateFile(FileName.c_str(), GENERIC_WRITE, FILE_SHARE_READ,
        NULL, CREATE_ALWAYS, CreateAttr, 0);
      Done = (*AHandle != INVALID_HANDLE_VALUE);
      if (!Done)
      {
        int FileAttr;
        if (FileExists(FileName) &&
          (((FileAttr = FileGetAttr(FileName)) & (faReadOnly | faHidden)) != 0))
        {
          if (FLAGSET(FileAttr, faReadOnly))
          {
            if (OperationProgress->NoToAll)
            {
              Result = false;
            }
            else if (!OperationProgress->YesToAll && !NoConfirmation)
            {
              int Answer;
              SUSPEND_OPERATION
              (
                Answer = QueryUser(
                  FMTLOAD(READ_ONLY_OVERWRITE, (FileName)), NULL,
                  qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll, 0);
              );
              switch (Answer) {
                case qaYesToAll: OperationProgress->YesToAll = true; break;
                case qaCancel: OperationProgress->Cancel = csCancel; // continue on next case
                case qaNoToAll: OperationProgress->NoToAll = true;
                case qaNo: Result = false; break;
              }
            }
          }
          else
          {
            assert(FLAGSET(FileAttr, faHidden));
            Result = true;
          }

          if (Result)
          {
            CreateAttr |=
              FLAGMASK(FLAGSET(FileAttr, faHidden), FILE_ATTRIBUTE_HIDDEN) |
              FLAGMASK(FLAGSET(FileAttr, faReadOnly), FILE_ATTRIBUTE_READONLY);

            FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
              if (FileSetAttr(FileName, FileAttr & ~(faReadOnly | faHidden)) != 0)
              {
                EXCEPTION;
              }
            );
          }
          else
          {
            Done = true;
          }
        }
        else
        {
          EXCEPTION;
        }
      }
    }
    while (!Done);
  );

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::OpenLocalFile(const AnsiString FileName,
  int Access, int * AAttrs, HANDLE * AHandle, __int64 * ACTime,
  __int64 * AMTime, __int64 * AATime, __int64 * ASize,
  bool TryWriteReadOnly)
{
  int Attrs = 0;
  HANDLE Handle = 0;

  FILE_OPERATION_LOOP (FMTLOAD(FILE_NOT_EXISTS, (FileName)),
    Attrs = FileGetAttr(FileName);
    if (Attrs == -1) EXCEPTION;
  )

  if ((Attrs & faDirectory) == 0)
  {
    bool NoHandle = false;
    if (!TryWriteReadOnly && (Access == GENERIC_WRITE) &&
        ((Attrs & faReadOnly) != 0))
    {
      Access = GENERIC_READ;
      NoHandle = true;
    }

    FILE_OPERATION_LOOP (FMTLOAD(OPENFILE_ERROR, (FileName)),
      Handle = CreateFile(FileName.c_str(), Access,
        Access == GENERIC_READ ? FILE_SHARE_READ | FILE_SHARE_WRITE : FILE_SHARE_READ,
        NULL, OPEN_EXISTING, 0, 0);
      if (Handle == INVALID_HANDLE_VALUE)
      {
        Handle = 0;
        EXCEPTION;
      }
    );

    try
    {
      if (AATime || AMTime || ACTime)
      {
        // Get last file access and modification time
        FILE_OPERATION_LOOP (FMTLOAD(CANT_GET_ATTRS, (FileName)),
          FILETIME ATime;
          FILETIME MTime;
          FILETIME CTime;
          if (!GetFileTime(Handle, &CTime, &ATime, &MTime)) EXCEPTION;
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
        );
      }

      if (ASize)
      {
        // Get file size
        FILE_OPERATION_LOOP (FMTLOAD(CANT_GET_ATTRS, (FileName)),
          unsigned long LSize;
          unsigned long HSize;
          LSize = GetFileSize(Handle, &HSize);
          if ((LSize == 0xFFFFFFFF) && (GetLastError() != NO_ERROR)) EXCEPTION;
          *ASize = (__int64(HSize) << 32) + LSize;
        );
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
AnsiString __fastcall TTerminal::FileUrl(const AnsiString Protocol,
  const AnsiString FileName)
{
  assert(FileName.Length() > 0);
  return Protocol + "://" + EncodeUrlChars(SessionData->SessionName) +
    (FileName[1] == '/' ? "" : "/") + EncodeUrlChars(FileName, "/");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::FileUrl(const AnsiString FileName)
{
  return FFileSystem->FileUrl(FileName);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::MakeLocalFileList(const AnsiString FileName,
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
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateLocalFileSize(const AnsiString FileName,
  const TSearchRec Rec, /*TCalculateSizeParams*/ void * Params)
{
  TCalculateSizeParams * AParams = static_cast<TCalculateSizeParams*>(Params);

  bool Dir = FLAGSET(Rec.Attr, faDirectory);

  bool AllowTransfer = (AParams->CopyParam == NULL);
  __int64 Size =
    (static_cast<__int64>(Rec.FindData.nFileSizeHigh) << 32) +
    Rec.FindData.nFileSizeLow;
  if (!AllowTransfer)
  {
    TFileMasks::TParams MaskParams;
    MaskParams.Size = Size;

    AllowTransfer = AParams->CopyParam->AllowTransfer(FileName, osLocal, Dir, MaskParams);
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

  if (OperationProgress && OperationProgress->Operation == foCalculateSize)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateLocalFilesSize(TStrings * FileList,
  __int64 & Size, const TCopyParamType * CopyParam)
{
  TFileOperationProgressType OperationProgress(FOnProgress, FOnFinished);
  bool DisconnectWhenComplete = false;
  OperationProgress.Start(foCalculateSize, osLocal, FileList->Count);
  try
  {
    TCalculateSizeParams Params;
    Params.Size = 0;
    Params.Params = 0;
    Params.CopyParam = CopyParam;

    assert(!FOperationProgress);
    FOperationProgress = &OperationProgress;
    TSearchRec Rec;
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      AnsiString FileName = FileList->Strings[Index];
      if (FileSearchRec(FileName, Rec))
      {
        CalculateLocalFileSize(FileName, Rec, &Params);
        OperationProgress.Finish(FileName, true, DisconnectWhenComplete);
      }
    }

    Size = Params.Size;
  }
  __finally
  {
    FOperationProgress = NULL;
    OperationProgress.Stop();
  }

  if (DisconnectWhenComplete)
  {
    CloseOnCompletion();
  }
}
//---------------------------------------------------------------------------
struct TSynchronizeFileData
{
  bool Modified;
  bool New;
  bool IsDirectory;
  TSynchronizeChecklist::TItem::TFileInfo Info;
  TSynchronizeChecklist::TItem::TFileInfo MatchingRemoteFile;
  int MatchingRemoteFileImageIndex;
  FILETIME LocalLastWriteTime;
};
//---------------------------------------------------------------------------
const int sfFirstLevel = 0x01;
struct TSynchronizeData
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;
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
TSynchronizeChecklist * __fastcall TTerminal::SynchronizeCollect(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory,
  TSynchronizeOptions * Options)
{
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
void __fastcall TTerminal::DoSynchronizeCollectDirectory(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode,
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

  LogEvent(FORMAT("Collecting synchronization list for local directory '%s' and remote directory '%s', "
    "mode = %d, params = %d", (LocalDirectory, RemoteDirectory,
    int(Mode), int(Params))));

  if (FLAGCLEAR(Params, spDelayProgress))
  {
    DoSynchronizeProgress(Data, true);
  }

  try
  {
    bool Found;
    TSearchRec SearchRec;
    Data.LocalFileList = new TStringList();
    Data.LocalFileList->Sorted = true;
    Data.LocalFileList->CaseSensitive = false;

    FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (LocalDirectory)),
      int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
      Found = (FindFirst(Data.LocalDirectory + "*.*", FindAttrs, SearchRec) == 0);
    );

    if (Found)
    {
      try
      {
        AnsiString FileName;
        while (Found)
        {
          FileName = SearchRec.Name;
          // add dirs for recursive mode or when we are interested in newly
          // added subdirs
          int FoundIndex;
          __int64 Size =
            (static_cast<__int64>(SearchRec.FindData.nFileSizeHigh) << 32) +
            SearchRec.FindData.nFileSizeLow;
          TFileMasks::TParams MaskParams;
          MaskParams.Size = Size;
          if ((FileName != ".") && (FileName != "..") &&
              CopyParam->AllowTransfer(Data.LocalDirectory + FileName, osLocal,
                FLAGSET(SearchRec.Attr, faDirectory), MaskParams) &&
              !FFileSystem->TemporaryTransferFile(FileName) &&
              (FLAGCLEAR(Flags, sfFirstLevel) ||
               (Options == NULL) || (Options->Filter == NULL) ||
               Options->Filter->Find(FileName, FoundIndex)))
          {
            TSynchronizeFileData * FileData = new TSynchronizeFileData;

            FILETIME LocalLastWriteTime;
            SYSTEMTIME SystemLastWriteTime;
            FileTimeToLocalFileTime(&SearchRec.FindData.ftLastWriteTime, &LocalLastWriteTime);
            FileTimeToSystemTime(&LocalLastWriteTime, &SystemLastWriteTime);

            FileData->IsDirectory = FLAGSET(SearchRec.Attr, faDirectory);
            FileData->Info.Directory = Data.LocalDirectory;
            FileData->Info.Modification = SystemTimeToDateTime(SystemLastWriteTime);
            FileData->Info.ModificationFmt = mfFull;
            FileData->Info.Size = Size;
            FileData->LocalLastWriteTime = SearchRec.FindData.ftLastWriteTime;
            FileData->New = true;
            FileData->Modified = false;
            Data.LocalFileList->AddObject(FileName,
              reinterpret_cast<TObject*>(FileData));
          }

          FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (LocalDirectory)),
            Found = (FindNext(SearchRec) == 0);
          );
        }
      }
      __finally
      {
        FindClose(SearchRec);
      }

      // can we expect that ProcessDirectory would take so little time
      // that we can pospone showing progress window until anything actually happens?
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
        if (Modified || New)
        {
          TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
          try
          {
            ChecklistItem->FileName = Data.LocalFileList->Strings[Index];
            ChecklistItem->IsDirectory = FileData->IsDirectory;

            ChecklistItem->Local = FileData->Info;
            ChecklistItem->FLocalLastWriteTime = FileData->LocalLastWriteTime;

            if (Modified)
            {
              assert(!FileData->MatchingRemoteFile.Directory.IsEmpty());
              ChecklistItem->Remote = FileData->MatchingRemoteFile;
              ChecklistItem->ImageIndex = FileData->MatchingRemoteFileImageIndex;
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
void __fastcall TTerminal::SynchronizeCollectFile(const AnsiString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  TSynchronizeData * Data = static_cast<TSynchronizeData *>(Param);

  int FoundIndex;
  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Size;
  if (Data->CopyParam->AllowTransfer(
        UnixExcludeTrailingBackslash(File->FullFileName), osRemote,
        File->IsDirectory, MaskParams) &&
      !FFileSystem->TemporaryTransferFile(File->FileName) &&
      (FLAGCLEAR(Data->Flags, sfFirstLevel) ||
       (Data->Options == NULL) || (Data->Options->Filter == NULL) ||
        Data->Options->Filter->Find(File->FileName, FoundIndex)))
  {
    TSynchronizeChecklist::TItem * ChecklistItem = new TSynchronizeChecklist::TItem();
    try
    {
      ChecklistItem->FileName = File->FileName;
      ChecklistItem->IsDirectory = File->IsDirectory;
      ChecklistItem->ImageIndex = File->IconIndex;

      ChecklistItem->Remote.Directory = Data->RemoteDirectory;
      ChecklistItem->Remote.Modification = File->Modification;
      ChecklistItem->Remote.ModificationFmt = File->ModificationFmt;
      ChecklistItem->Remote.Size = File->Size;

      bool Modified = false;
      int LocalIndex = Data->LocalFileList->IndexOf(ChecklistItem->FileName);
      bool New = (LocalIndex < 0);
      if (!New)
      {
        TSynchronizeFileData * LocalData =
          reinterpret_cast<TSynchronizeFileData *>(Data->LocalFileList->Objects[LocalIndex]);

        LocalData->New = false;

        if (File->IsDirectory != LocalData->IsDirectory)
        {
          LogEvent(FORMAT("%s is directory on one side, but file on the another",
            (File->FileName)));
        }
        else if (!File->IsDirectory)
        {
          ChecklistItem->Local = LocalData->Info;

          ReduceDateTimePrecision(ChecklistItem->Local.Modification, File->ModificationFmt);

          bool LocalModified = false;
          // for spTimestamp+spBySize require that the file sizes are the same
          // before comparing file time
          bool TimeCompare =
            FLAGCLEAR(Data->Params, spNotByTime) &&
            (FLAGCLEAR(Data->Params, spTimestamp) ||
             FLAGCLEAR(Data->Params, spBySize) ||
             (ChecklistItem->Local.Size == ChecklistItem->Remote.Size));
          if (TimeCompare &&
              (CompareFileTime(ChecklistItem->Local.Modification,
                 ChecklistItem->Remote.Modification) < 0))
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
          else if (TimeCompare &&
                   (CompareFileTime(ChecklistItem->Local.Modification,
                      ChecklistItem->Remote.Modification) > 0))
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
          }
        }
        else if (FLAGCLEAR(Data->Params, spNoRecurse))
        {
          DoSynchronizeCollectDirectory(
            Data->LocalDirectory + File->FileName,
            Data->RemoteDirectory + File->FileName,
            Data->Mode, Data->CopyParam, Data->Params, Data->OnSynchronizeDirectory,
            Data->Options, (Data->Flags & ~sfFirstLevel),
            Data->Checklist);
        }
      }
      else
      {
        ChecklistItem->Local.Directory = Data->LocalDirectory;
      }

      if (New || Modified)
      {
        assert(!New || !Modified);

        // download the file if it changed or is new and we want to have it locally
        if ((Data->Mode == smBoth) || (Data->Mode == smLocal))
        {
          ChecklistItem->Action =
            (Modified ? TSynchronizeChecklist::saDownloadUpdate : TSynchronizeChecklist::saDownloadNew);
          ChecklistItem->Checked =
            (Modified || FLAGCLEAR(Data->Params, spExistingOnly)) &&
            (!ChecklistItem->IsDirectory || FLAGCLEAR(Data->Params, spNoRecurse) ||
             FLAGSET(Data->Params, spSubDirs));
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
          ChecklistItem->FRemoteFile = File->Duplicate();
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
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeApply(TSynchronizeChecklist * Checklist,
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
  const TCopyParamType * CopyParam, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory)
{
  TSynchronizeData Data;

  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;

  int CopyParams =
    FLAGMASK(FLAGSET(Params, spNoConfirmation), cpNoConfirmation);

  TCopyParamType SyncCopyParam = *CopyParam;
  SyncCopyParam.PreserveTime = true;

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

      AnsiString CurrentLocalDirectory = ChecklistItem->Local.Directory;
      AnsiString CurrentRemoteDirectory = ChecklistItem->Remote.Directory;

      LogEvent(FORMAT("Synchronizing local directory '%s' with remote directory '%s', "
        "params = %d", (CurrentLocalDirectory, CurrentRemoteDirectory, int(Params))));

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
                    ChecklistItem->FileName,
                  (TObject *)(ChecklistItem));
                break;

              case TSynchronizeChecklist::saUploadUpdate:
                UploadList->AddObject(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->FileName,
                  (TObject *)(ChecklistItem));
                break;

              default:
                assert(false);
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
                    ChecklistItem->FileName,
                  ChecklistItem->FRemoteFile);
                break;

              case TSynchronizeChecklist::saDeleteRemote:
                DeleteRemoteList->AddObject(
                  UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) +
                    ChecklistItem->FileName,
                  ChecklistItem->FRemoteFile);
                break;

              case TSynchronizeChecklist::saUploadNew:
              case TSynchronizeChecklist::saUploadUpdate:
                UploadList->Add(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->FileName);
                break;

              case TSynchronizeChecklist::saDeleteLocal:
                DeleteLocalList->Add(
                  IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
                    ChecklistItem->FileName);
                break;

              default:
                assert(false);
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
void __fastcall TTerminal::SynchronizeLocalTimestamp(const AnsiString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  AnsiString LocalFile =
    IncludeTrailingBackslash(ChecklistItem->Local.Directory) +
      ChecklistItem->FileName;

  FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (LocalFile)),
    HANDLE Handle;
    OpenLocalFile(LocalFile, GENERIC_WRITE, NULL, &Handle,
      NULL, NULL, NULL, NULL);
    FILETIME WrTime = DateTimeToFileTime(ChecklistItem->Remote.Modification,
      SessionData->DSTMode);
    bool Result = SetFileTime(Handle, NULL, NULL, &WrTime);
    CloseHandle(Handle);
    if (!Result)
    {
      Abort();
    }
  );
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeRemoteTimestamp(const AnsiString /*FileName*/,
  const TRemoteFile * File, void * /*Param*/)
{
  const TSynchronizeChecklist::TItem * ChecklistItem =
    reinterpret_cast<const TSynchronizeChecklist::TItem *>(File);

  TRemoteProperties Properties;
  Properties.Valid << vpModification;
  Properties.Modification = ConvertTimestampToUnix(ChecklistItem->FLocalLastWriteTime,
    SessionData->DSTMode);

  // unfortunatelly we never have a remote file here
  assert(ChecklistItem->FRemoteFile == NULL);

  ChangeFileProperties(
    UnixIncludeTrailingBackslash(ChecklistItem->Remote.Directory) + ChecklistItem->FileName,
    NULL, &Properties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SpaceAvailable(const AnsiString Path,
  TSpaceAvailable & ASpaceAvailable)
{
  assert(IsCapable[fcCheckingSpaceAvailable]);

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
AnsiString __fastcall TTerminal::GetPassword()
{
  AnsiString Result;
  // FPassword is empty also when stored password was used
  if (FPassword.IsEmpty())
  {
    Result = SessionData->Password;
  }
  else
  {
    Result = DecryptPassword(FPassword, SessionData->SessionName);
  }
  return Result;
}
//---------------------------------------------------------------------
bool __fastcall TTerminal::GetStoredCredentialsTried()
{
  bool Result;
  if (FFileSystem != NULL)
  {
    Result = FFileSystem->GetStoredCredentialsTried();
  }
  else
  {
    assert(FSecureShell != NULL);
    Result = FSecureShell->GetStoredCredentialsTried();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToRemote(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  assert(FFileSystem);
  assert(FilesToCopy);

  assert(IsCapable[fcNewerOnlyUpload] || FLAGCLEAR(Params, cpNewerOnly));

  bool Result = false;
  bool DisconnectWhenComplete = false;

  try
  {

    __int64 Size;
    if (CopyParam->CalculateSize)
    {
      // dirty trick: when moving, do not pass copy param to avoid exclude mask
      CalculateLocalFilesSize(FilesToCopy, Size,
        (FLAGCLEAR(Params, cpDelete) ? CopyParam : NULL));
    }

    TFileOperationProgressType OperationProgress(FOnProgress, FOnFinished);
    OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osLocal,
      FilesToCopy->Count, Params & cpTemporary, TargetDir);

    OperationProgress.YesToNewer = FLAGSET(Params, cpNewerOnly);

    FOperationProgress = &OperationProgress;
    try
    {
      if (CopyParam->CalculateSize)
      {
        OperationProgress.SetTotalSize(Size);
      }

      AnsiString UnlockedTargetDir = TranslateLockedPath(TargetDir, false);
      BeginTransaction();
      try
      {
        if (Log->Logging)
        {
          LogEvent(FORMAT("Copying %d files/directories to remote directory "
            "\"%s\"", (FilesToCopy->Count, TargetDir)));
          LogEvent(CopyParam->LogStr);
        }

        FFileSystem->CopyToRemote(FilesToCopy, UnlockedTargetDir,
          CopyParam, Params, &OperationProgress, DisconnectWhenComplete);
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
      OperationProgress.Stop();
      FOperationProgress = NULL;
    }
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(TOREMOTE_COPY_ERROR));
    DisconnectWhenComplete = false;
  }

  if (DisconnectWhenComplete) CloseOnCompletion();

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToLocal(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  assert(FFileSystem);

  // see scp.c: sink(), tolocal()

  bool Result = false;
  bool OwnsFileList = (FilesToCopy == NULL);
  bool DisconnectWhenComplete = false;

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
      TFileOperationProgressType OperationProgress(FOnProgress, FOnFinished);

      if (CopyParam->CalculateSize)
      {
        ExceptionOnFail = true;
        try
        {
          // dirty trick: when moving, do not pass copy param to avoid exclude mask
          CalculateFilesSize(FilesToCopy, TotalSize, csIgnoreErrors,
            (FLAGCLEAR(Params, cpDelete) ? CopyParam : NULL));
          TotalSizeKnown = true;
        }
        __finally
        {
          ExceptionOnFail = false;
        }
      }

      OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osRemote,
        FilesToCopy->Count, Params & cpTemporary, TargetDir);

      OperationProgress.YesToNewer = FLAGSET(Params, cpNewerOnly);

      FOperationProgress = &OperationProgress;
      try
      {
        if (TotalSizeKnown)
        {
          OperationProgress.SetTotalSize(TotalSize);
        }

        try
        {
          try
          {
            FFileSystem->CopyToLocal(FilesToCopy, TargetDir, CopyParam, Params,
              &OperationProgress, DisconnectWhenComplete);
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
          CommandError(&E, LoadStr(TOLOCAL_COPY_ERROR));
          DisconnectWhenComplete = false;
        }

        if (OperationProgress.Cancel == csContinue)
        {
          Result = true;
        }
      }
      __finally
      {
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

  if (DisconnectWhenComplete) CloseOnCompletion();

  return Result;
}
//---------------------------------------------------------------------------
__fastcall TSecondaryTerminal::TSecondaryTerminal(TTerminal * MainTerminal,
  TSessionData * ASessionData, TConfiguration * Configuration, const AnsiString & Name) :
  TTerminal(ASessionData, Configuration),
  FMainTerminal(MainTerminal), FMasterPasswordTried(false)
{
  Log->Parent = FMainTerminal->Log;
  Log->Name = Name;

  SessionData->NonPersistant();
  assert(FMainTerminal != NULL);
  if (!FMainTerminal->UserName.IsEmpty())
  {
    SessionData->UserName = FMainTerminal->UserName;
  }
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryLoaded(TRemoteFileList * FileList)
{
  FMainTerminal->DirectoryLoaded(FileList);
  assert(FileList != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TSecondaryTerminal::DirectoryModified(const AnsiString Path,
  bool SubDirs)
{
  // clear cache of main terminal
  FMainTerminal->DirectoryModified(Path, SubDirs);
}
//---------------------------------------------------------------------------
bool __fastcall TSecondaryTerminal::DoPromptUser(TSessionData * Data,
  AnsiString Prompt, TPromptKind Kind, AnsiString & Response)
{
  bool Result = false;

  if (!FMasterPasswordTried &&
      ((Kind == pkPassword) || (Kind == pkPassphrase) || (Kind == pkServerPrompt)))
  {
    // let's expect that the main session is already authenticated and its password
    // is not written after, so no locking is necessary
    // (no longer true, once the main session can be reconnected)
    Response = FMainTerminal->Password;
    if (!Response.IsEmpty())
    {
      Result = true;
    }
    FMasterPasswordTried = true;
  }

  if (!Result)
  {
    Result = TTerminal::DoPromptUser(Data, Prompt, Kind, Response);
  }

  return Result;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::TTerminalList(TConfiguration * AConfiguration) :
  TObjectList()
{
  assert(AConfiguration);
  FConfiguration = AConfiguration;
}
//---------------------------------------------------------------------------
__fastcall TTerminalList::~TTerminalList()
{
  assert(Count == 0);
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
  assert(IndexOf(Terminal) >= 0);
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
int __fastcall TTerminalList::GetActiveCount()
{
  int Result = 0;
  TTerminal * Terminal;
  for (int i = 0; i < Count; i++)
  {
    Terminal = Terminals[i];
    if (Terminal->Active)
    {
      Result++;
    }
  }
  return Result;
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
