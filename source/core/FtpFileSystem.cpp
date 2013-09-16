//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#ifndef NO_FILEZILLA
//---------------------------------------------------------------------------
#include <list>
#define MPEXT
#include "FtpFileSystem.h"
#include "FileZillaIntf.h"

#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "TextsFileZilla.h"
#include "HelpCore.h"
#define OPENSSL_NO_EC
#define OPENSSL_NO_ECDSA
#define OPENSSL_NO_ECDH
#include <openssl/x509_vfy.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION, L"")
//---------------------------------------------------------------------------
const int DummyCodeClass = 8;
const int DummyTimeoutCode = 801;
const int DummyCancelCode = 802;
const int DummyDisconnectCode = 803;
//---------------------------------------------------------------------------
class TFileZillaImpl : public TFileZillaIntf
{
public:
  __fastcall TFileZillaImpl(TFTPFileSystem * FileSystem);

  virtual const wchar_t * __fastcall Option(int OptionID) const;
  virtual int __fastcall OptionVal(int OptionID) const;

protected:
  virtual bool __fastcall DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam);

  virtual bool __fastcall HandleStatus(const wchar_t * Status, int Type);
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
    const wchar_t * Path1, const wchar_t * Path2,
    __int64 Size1, __int64 Size2, time_t LocalTime,
    bool HasLocalTime, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult);
  virtual bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult);
  virtual bool __fastcall HandleAsynchRequestNeedPass(
    struct TNeedPassRequestData & Data, int & RequestResult);
  virtual bool __fastcall HandleListData(const wchar_t * Path, const TListDataEntry * Entries,
    unsigned int Count);
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer);
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply);
  virtual bool __fastcall HandleCapabilities(TFTPServerCapabilities * ServerCapabilities);
  virtual bool __fastcall CheckError(int ReturnCode, const wchar_t * Context);

  virtual void PreserveDownloadFileTime(HANDLE Handle, void * UserData);
  virtual bool GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time);

private:
  TFTPFileSystem * FFileSystem;
};
//---------------------------------------------------------------------------
__fastcall TFileZillaImpl::TFileZillaImpl(TFTPFileSystem * FileSystem) :
  TFileZillaIntf(),
  FFileSystem(FileSystem)
{
}
//---------------------------------------------------------------------------
const wchar_t * __fastcall TFileZillaImpl::Option(int OptionID) const
{
  return FFileSystem->GetOption(OptionID);
}
//---------------------------------------------------------------------------
int __fastcall TFileZillaImpl::OptionVal(int OptionID) const
{
  return FFileSystem->GetOptionVal(OptionID);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam)
{
  return FFileSystem->PostMessage(Type, wParam, lParam);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleStatus(const wchar_t * Status, int Type)
{
  return FFileSystem->HandleStatus(Status, Type);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestOverwrite(
  wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
  const wchar_t * Path1, const wchar_t * Path2,
  __int64 Size1, __int64 Size2, time_t LocalTime,
  bool HasLocalTime, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestOverwrite(
    FileName1, FileName1Len, FileName2, Path1, Path2, Size1, Size2, LocalTime,
    HasLocalTime, RemoteTime, UserData, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestVerifyCertificate(Data, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestNeedPass(
  struct TNeedPassRequestData & Data, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestNeedPass(Data, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleListData(const wchar_t * Path,
  const TListDataEntry * Entries, unsigned int Count)
{
  return FFileSystem->HandleListData(Path, Entries, Count);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
  bool FileTransfer)
{
  return FFileSystem->HandleTransferStatus(Valid, TransferSize, Bytes, Percent,
    TimeElapsed, TimeLeft, TransferRate, FileTransfer);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleReply(int Command, unsigned int Reply)
{
  return FFileSystem->HandleReply(Command, Reply);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleCapabilities(TFTPServerCapabilities * ServerCapabilities)
{
  return FFileSystem->HandleCapabilities(ServerCapabilities);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::CheckError(int ReturnCode, const wchar_t * Context)
{
  return FFileSystem->CheckError(ReturnCode, Context);
}
//---------------------------------------------------------------------------
void TFileZillaImpl::PreserveDownloadFileTime(HANDLE Handle, void * UserData)
{
  return FFileSystem->PreserveDownloadFileTime(Handle, UserData);
}
//---------------------------------------------------------------------------
bool TFileZillaImpl::GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time)
{
  return FFileSystem->GetFileModificationTimeInUtc(FileName, Time);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
class TMessageQueue : public std::list<std::pair<WPARAM, LPARAM> >
{
};
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
struct TFileTransferData
{
  TFileTransferData()
  {
    Params = 0;
    AutoResume = false;
    OverwriteResult = -1;
    CopyParam = NULL;
  }

  UnicodeString FileName;
  int Params;
  bool AutoResume;
  int OverwriteResult;
  const TCopyParamType * CopyParam;
  TDateTime Modification;
};
//---------------------------------------------------------------------------
const int tfFirstLevel = 0x01;
const int tfAutoResume = 0x02;
const wchar_t CertificateStorageKey[] = L"FtpsCertificates";
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  UnicodeString TargetDir;
  const TCopyParamType * CopyParam;
  int Params;
  TFileOperationProgressType * OperationProgress;
  bool Skipped;
  unsigned int Flags;
};
//---------------------------------------------------------------------------
class TFileListHelper
{
public:
  TFileListHelper(TFTPFileSystem * FileSystem, TRemoteFileList * FileList,
      bool IgnoreFileList) :
    FFileSystem(FileSystem),
    FFileList(FFileSystem->FFileList),
    FIgnoreFileList(FFileSystem->FIgnoreFileList)
  {
    FFileSystem->FFileList = FileList;
    FFileSystem->FIgnoreFileList = IgnoreFileList;
  }

  ~TFileListHelper()
  {
    FFileSystem->FFileList = FFileList;
    FFileSystem->FIgnoreFileList = FIgnoreFileList;
  }

private:
  TFTPFileSystem * FFileSystem;
  TRemoteFileList * FFileList;
  bool FIgnoreFileList;
};
//---------------------------------------------------------------------------
__fastcall TFTPFileSystem::TFTPFileSystem(TTerminal * ATerminal):
  TCustomFileSystem(ATerminal),
  FFileZillaIntf(NULL),
  FQueueCriticalSection(new TCriticalSection),
  FTransferStatusCriticalSection(new TCriticalSection),
  FQueueEvent(CreateEvent(NULL, true, false, NULL)),
  FQueue(new TMessageQueue),
  FReply(0),
  FCommandReply(0),
  FLastCommand(CMD_UNKNOWN),
  FLastResponse(new TStringList()),
  FLastErrorResponse(new TStringList()),
  FLastError(new TStringList()),
  FFeatures(new TStringList()),
  FFileList(NULL),
  FFileListCache(NULL),
  FActive(false),
  FOpening(false),
  FWaitingForReply(false),
  FIgnoreFileList(false),
  FOnCaptureOutput(NULL),
  FFileSystemInfoValid(false),
  FDoListAll(false),
  FServerCapabilities(NULL)
{
  ResetReply();

  FListAll = FTerminal->SessionData->FtpListAll;
  FFileSystemInfo.ProtocolBaseName = L"FTP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  FTimeoutStatus = LoadStr(IDS_ERRORMSG_TIMEOUT);
  FDisconnectStatus = LoadStr(IDS_STATUSMSG_DISCONNECTED);
  FServerCapabilities = new TFTPServerCapabilities();
}
//---------------------------------------------------------------------------
__fastcall TFTPFileSystem::~TFTPFileSystem()
{
  assert(FFileList == NULL);

  FFileZillaIntf->Destroying();

  // to release memory associated with the messages
  DiscardMessages();

  delete FFileZillaIntf;
  FFileZillaIntf = NULL;

  delete FQueue;
  FQueue = NULL;

  CloseHandle(FQueueEvent);

  delete FQueueCriticalSection;
  FQueueCriticalSection = NULL;
  delete FTransferStatusCriticalSection;
  FTransferStatusCriticalSection = NULL;

  delete FLastResponse;
  FLastResponse = NULL;
  delete FLastErrorResponse;
  FLastErrorResponse = NULL;
  delete FLastError;
  FLastError = NULL;
  delete FFeatures;
  FFeatures = NULL;
  delete FServerCapabilities;
  FServerCapabilities = NULL;

  ResetCaches();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Open()
{
  // on reconnect, typically there may be pending status messages from previous session
  DiscardMessages();

  ResetCaches();
  FCurrentDirectory = L"";
  FHomeDirectory = L"";

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();

  switch (Data->Ftps)
  {
    case ftpsNone:
      // noop;
      break;

    case ftpsImplicit:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_IMPLICIT);
      break;

    case ftpsExplicitSsl:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT_SSL);
      break;

    case ftpsExplicitTls:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT_TLS);
      break;

    default:
      FAIL;
      break;
  }

  FLastDataSent = Now();

  FMultineResponse = false;

  // initialize FZAPI on the first connect only
  if (FFileZillaIntf == NULL)
  {
    FFileZillaIntf = new TFileZillaImpl(this);

    try
    {
      TFileZillaIntf::TLogLevel LogLevel;
      switch (FTerminal->Configuration->ActualLogProtocol)
      {
        default:
        case 0:
        case 1:
          LogLevel = TFileZillaIntf::LOG_WARNING;
          break;

        case 2:
          LogLevel = TFileZillaIntf::LOG_INFO;
          break;
      }
      FFileZillaIntf->SetDebugLevel(LogLevel);

      FFileZillaIntf->Init();
    }
    catch(...)
    {
      delete FFileZillaIntf;
      FFileZillaIntf = NULL;
      throw;
    }
  }

  UnicodeString HostName = Data->HostNameExpanded;
  UnicodeString UserName = Data->UserNameExpanded;
  UnicodeString Password = Data->Password;
  UnicodeString Account = Data->FtpAccount;
  UnicodeString Path = Data->RemoteDirectory;
  int ServerType;
  switch (Data->Ftps)
  {
    case ftpsImplicit:
      ServerType = TFileZillaIntf::SERVER_FTP_SSL_IMPLICIT;
      break;

    case ftpsExplicitSsl:
      ServerType = TFileZillaIntf::SERVER_FTP_SSL_EXPLICIT;
      break;

    case ftpsExplicitTls:
      ServerType = TFileZillaIntf::SERVER_FTP_TLS_EXPLICIT;
      break;

    default:
      assert(Data->Ftps == ftpsNone);
      ServerType = TFileZillaIntf::SERVER_FTP;
      break;
  }
  int Pasv = (Data->FtpPasvMode ? 1 : 2);
  int TimeZoneOffset = TimeToMinutes(Data->TimeDifference);
  int UTF8 = 0;
  switch (Data->NotUtf)
  {
    case asOn:
      UTF8 = 2;
      break;

    case asOff:
      UTF8 = 1;
      break;

    case asAuto:
      UTF8 = 0;
      break;
  }

  FPasswordFailed = false;
  bool PromptedForCredentials = false;

  do
  {
    FSystem = L"";
    FFeatures->Clear();
    FFileSystemInfoValid = false;

    // TODO: the same for account? it ever used?

    // ask for username if it was not specified in advance, even on retry,
    // but keep previous one as default,
    if (Data->UserNameExpanded.IsEmpty())
    {
      FTerminal->LogEvent(L"Username prompt (no username provided)");

      if (!FPasswordFailed && !PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT), false);
        PromptedForCredentials = true;
      }

      if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(USERNAME_TITLE), L"",
            LoadStr(USERNAME_PROMPT2), true, 0, UserName))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
      else
      {
        FUserName = UserName;
      }
    }

    // on retry ask for password
    if (FPasswordFailed)
    {
      FTerminal->LogEvent(L"Password prompt (last login attempt failed)");

      // on retry ask for new password
      Password = L"";
      if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(PASSWORD_TITLE), L"",
            LoadStr(PASSWORD_PROMPT), false, 0, Password))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
    }

    FPasswordFailed = false;
    TValueRestorer<bool> OpeningRestorer(FOpening);
    FOpening = true;

    FActive = FFileZillaIntf->Connect(
      HostName.c_str(), Data->PortNumber, UserName.c_str(),
      Password.c_str(), Account.c_str(), false, Path.c_str(),
      ServerType, Pasv, TimeZoneOffset, UTF8, Data->FtpForcePasvIp,
      Data->FtpUseMlsd);

    assert(FActive);

    try
    {
      // do not wait for FTP response code as Connect is complex operation
      GotReply(WaitForCommandReply(false), REPLY_CONNECT, LoadStr(CONNECTION_FAILED));

      Shred(Password);

      // we have passed, even if we got 530 on the way (if it is possible at all),
      // ignore it
      assert(!FPasswordFailed);
      FPasswordFailed = false;
    }
    catch(...)
    {
      if (FPasswordFailed)
      {
        FTerminal->Information(
          LoadStr(Password.IsEmpty() ? FTP_ACCESS_DENIED_EMPTY_PASSWORD : FTP_ACCESS_DENIED),
          false);
      }
      else
      {
        // see handling of REPLY_CONNECT in GotReply
        FTerminal->Closed();
        throw;
      }
    }
  }
  while (FPasswordFailed);

  FSessionInfo.CSCipher = FFileZillaIntf->GetCipherName().c_str();
  FSessionInfo.SCCipher = FSessionInfo.CSCipher;
  UnicodeString TlsVersionStr = FFileZillaIntf->GetTlsVersionStr().c_str();
  AddToList(FSessionInfo.SecurityProtocolName, TlsVersionStr, L", ");
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Close()
{
  assert(FActive);
  bool Result;
  if (FFileZillaIntf->Close(FOpening))
  {
    CHECK(FLAGSET(WaitForCommandReply(false), TFileZillaIntf::REPLY_DISCONNECTED));
    Result = true;
  }
  else
  {
    // See TFileZillaIntf::Close
    Result = FOpening;
  }

  if (ALWAYS_TRUE(Result))
  {
    assert(FActive);
    Discard();
    FTerminal->Closed();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetActive()
{
  return FActive;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Idle()
{
  if (FActive && !FWaitingForReply)
  {
    PoolForFatalNonCommandReply();

    // Keep session alive
    if ((FTerminal->SessionData->FtpPingType != ptOff) &&
        (double(Now() - FLastDataSent) > double(FTerminal->SessionData->FtpPingIntervalDT) * 4))
    {
      FLastDataSent = Now();

      TRemoteDirectory * Files = new TRemoteDirectory(FTerminal);
      try
      {
        try
        {
          Files->Directory = CurrentDirectory;
          DoReadDirectory(Files);
        }
        catch(...)
        {
          // ignore non-fatal errors
          // (i.e. current directory may not exist anymore)
          if (!FTerminal->Active)
          {
            throw;
          }
        }
      }
      __finally
      {
        delete Files;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Discard()
{
  // remove all pending messages, to get complete log
  // note that we need to retry discard on reconnect, as there still may be another
  // "disconnect/timeout/..." status messages coming
  DiscardMessages();
  assert(FActive);
  FActive = false;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::AbsolutePath(UnicodeString Path, bool /*Local*/)
{
  // TODO: improve (handle .. etc.)
  if (TTerminal::IsAbsolutePath(Path))
  {
    return Path;
  }
  else
  {
    return ::AbsolutePath(FCurrentDirectory, Path);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::ActualCurrentDirectory()
{
  wchar_t CurrentPath[1024];
  FFileZillaIntf->GetCurrentPath(CurrentPath, LENOF(CurrentPath));
  return UnixExcludeTrailingBackslash(CurrentPath);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::EnsureLocation()
{
  // if we do not know what's the current directory, do nothing
  if (!FCurrentDirectory.IsEmpty())
  {
    // Make sure that the FZAPI current working directory,
    // is actually our working directory.
    // It may not be because:
    // 1) We did cached directory change
    // 2) Listing was requested for non-current directory, which
    // makes FZAPI change its current directory (and not restoring it back afterwards)
    if (!UnixComparePaths(ActualCurrentDirectory(), FCurrentDirectory))
    {
      FTerminal->LogEvent(FORMAT(L"Synchronizing current directory \"%s\".",
        (FCurrentDirectory)));
      DoChangeDirectory(FCurrentDirectory);
      // make sure FZAPI is aware that we changed current working directory
      FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnyCommand(const UnicodeString Command,
  TCaptureOutputEvent OutputEvent)
{
  // end-user has right to expect that client current directory is really
  // current directory for the server
  EnsureLocation();

  assert(FOnCaptureOutput == NULL);
  FOnCaptureOutput = OutputEvent;
  try
  {
    FFileZillaIntf->CustomCommand(Command.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_3XX_CODE);
  }
  __finally
  {
    FOnCaptureOutput = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetCaches()
{
  delete FFileListCache;
  FFileListCache = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnnounceFileListOperation()
{
  ResetCaches();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoChangeDirectory(const UnicodeString & Directory)
{
  UnicodeString Command = FORMAT(L"CWD %s", (Directory));
  FFileZillaIntf->CustomCommand(Command.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeDirectory(const UnicodeString ADirectory)
{
  UnicodeString Directory = ADirectory;
  try
  {
    // For changing directory, we do not make paths absolute, instead we
    // delegate this to the server, hence we synchronize current working
    // directory with the server and only then we ask for the change with
    // relative path.
    // But if synchronization fails, typically because current working directory
    // no longer exists, we fall back to out own resolution, to give
    // user chance to leave the non-existing directory.
    EnsureLocation();
  }
  catch(...)
  {
    if (FTerminal->Active)
    {
      Directory = AbsolutePath(Directory, false);
    }
    else
    {
      throw;
    }
  }

  DoChangeDirectory(Directory);

  // make next ReadCurrentDirectory retrieve actual server-side current directory
  FCurrentDirectory = L"";
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CachedChangeDirectory(const UnicodeString Directory)
{
  FCurrentDirectory = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeFileProperties(const UnicodeString AFileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & Action)
{
  assert(Properties);
  assert(!Properties->Valid.Contains(vpGroup));
  assert(!Properties->Valid.Contains(vpOwner));
  assert(!Properties->Valid.Contains(vpLastAccess));
  assert(!Properties->Valid.Contains(vpModification));

  if (Properties->Valid.Contains(vpRights))
  {
    TRemoteFile * OwnedFile = NULL;

    try
    {
      UnicodeString FileName = AbsolutePath(AFileName, false);

      if (File == NULL)
      {
        ReadFile(FileName, OwnedFile);
        File = OwnedFile;
      }

      if ((File != NULL) && File->IsDirectory && !File->IsSymLink && Properties->Recursive)
      {
        try
        {
          FTerminal->ProcessDirectory(AFileName, FTerminal->ChangeFileProperties,
            (void*)Properties);
        }
        catch(...)
        {
          Action.Cancel();
          throw;
        }
      }

      TRights Rights;
      if (File != NULL)
      {
        Rights = *File->Rights;
      }
      Rights |= Properties->Rights.NumberSet;
      Rights &= (unsigned short)~Properties->Rights.NumberUnset;
      if ((File != NULL) && File->IsDirectory && Properties->AddXToDirectories)
      {
        Rights.AddExecute();
      }

      Action.Rights(Rights);

      UnicodeString FileNameOnly = UnixExtractFileName(FileName);
      UnicodeString FilePath = UnixExtractFilePath(FileName);
      // FZAPI wants octal number represented as decadic
      FFileZillaIntf->Chmod(Rights.NumberDecadic, FileNameOnly.c_str(), FilePath.c_str());

      GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
    }
    __finally
    {
      delete OwnedFile;
    }
  }
  else
  {
    Action.Cancel();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::LoadFilesProperties(TStrings * /*FileList*/)
{
  assert(false);
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CalculateFilesChecksum(const UnicodeString & /*Alg*/,
  TStrings * /*FileList*/, TStrings * /*Checksums*/,
  TCalculatedChecksumEvent /*OnCalculatedChecksum*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ConfirmOverwrite(UnicodeString & FileName,
  TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
  const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
  int Params, bool AutoResume)
{
  bool Result;
  bool CanAutoResume = FLAGSET(Params, cpNoConfirmation) && AutoResume;
  bool DestIsSmaller = (FileParams != NULL) && (FileParams->DestSize < FileParams->SourceSize);
  bool DestIsSame = (FileParams != NULL) && (FileParams->DestSize == FileParams->SourceSize);
  bool CanResume =
    !OperationProgress->AsciiTransfer &&
    // when resuming transfer after interrupted connection,
    // do nothing (dummy resume) when the files has the same size.
    // this is workaround for servers that strangely fails just after successful
    // upload.
    (DestIsSmaller || (DestIsSame && CanAutoResume));

  unsigned int Answer;
  if (CanAutoResume && CanResume)
  {
    if (DestIsSame)
    {
      assert(CanAutoResume);
      Answer = qaSkip;
    }
    else
    {
      Answer = qaRetry;
    }
  }
  else
  {
    // retry = "resume"
    // all = "yes to newer"
    // ignore = "rename"
    int Answers = qaYes | qaNo | qaCancel | qaYesToAll | qaNoToAll | qaAll | qaIgnore;
    if (CanResume)
    {
      Answers |= qaRetry;
    }
    TQueryButtonAlias Aliases[5];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RESUME_BUTTON);
    Aliases[0].GroupWith = qaNo;
    Aliases[0].GrouppedShiftState = TShiftState() << ssAlt;
    Aliases[1].Button = qaAll;
    Aliases[1].Alias = LoadStr(YES_TO_NEWER_BUTTON);
    Aliases[1].GroupWith = qaYes;
    Aliases[1].GrouppedShiftState = TShiftState() << ssCtrl;
    Aliases[2].Button = qaIgnore;
    Aliases[2].Alias = LoadStr(RENAME_BUTTON);
    Aliases[2].GroupWith = qaNo;
    Aliases[2].GrouppedShiftState = TShiftState() << ssCtrl;
    Aliases[3].Button = qaYesToAll;
    Aliases[3].GroupWith = qaYes;
    Aliases[3].GrouppedShiftState = TShiftState() << ssShift;
    Aliases[4].Button = qaNoToAll;
    Aliases[4].GroupWith = qaNo;
    Aliases[4].GrouppedShiftState = TShiftState() << ssShift;
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);
    SUSPEND_OPERATION
    (
      Answer = FTerminal->ConfirmFileOverwrite(FileName, FileParams,
        Answers, &QueryParams,
        OperationProgress->Side == osLocal ? osRemote : osLocal,
        CopyParam, Params, OperationProgress);
    )
  }

  Result = true;

  switch (Answer)
  {
    // resume
    case qaRetry:
      OverwriteMode = omResume;
      assert(FileParams != NULL);
      assert(CanResume);
      FFileTransferResumed = FileParams->DestSize;
      break;

    // rename
    case qaIgnore:
      if (FTerminal->PromptUser(FTerminal->SessionData, pkFileName,
            LoadStr(RENAME_TITLE), L"", LoadStr(RENAME_PROMPT2), true, 0, FileName))
      {
        OverwriteMode = omOverwrite;
      }
      else
      {
        if (!OperationProgress->Cancel)
        {
          OperationProgress->Cancel = csCancel;
        }
        FFileTransferAbort = ftaCancel;
        Result = false;
      }
      break;

    case qaYes:
      OverwriteMode = omOverwrite;
      break;

    case qaCancel:
      if (!OperationProgress->Cancel)
      {
        OperationProgress->Cancel = csCancel;
      }
      FFileTransferAbort = ftaCancel;
      Result = false;
      break;

    case qaNo:
      FFileTransferAbort = ftaSkip;
      Result = false;
      break;

    case qaSkip:
      OverwriteMode = omComplete;
      break;

    default:
      assert(false);
      Result = false;
      break;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetFileTransfer()
{
  FFileTransferAbort = ftaNone;
  FFileTransferCancelled = false;
  FFileTransferResumed = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadDirectoryProgress(__int64 Bytes)
{
  // with FTP we do not know exactly how many entries we have received,
  // instead we know number of bytes received only.
  // so we report approximation based on average size of entry.
  int Progress = int(Bytes / 80);
  if (Progress - FLastReadDirectoryProgress >= 10)
  {
    bool Cancel = false;
    FLastReadDirectoryProgress = Progress;
    FTerminal->DoReadDirectoryProgress(Progress, Cancel);
    if (Cancel)
    {
      FTerminal->DoReadDirectoryProgress(-2, Cancel);
      FFileZillaIntf->Cancel();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoFileTransferProgress(__int64 TransferSize,
  __int64 Bytes)
{
  TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;

  OperationProgress->SetTransferSize(TransferSize);

  if (FFileTransferResumed > 0)
  {
    OperationProgress->AddResumed(FFileTransferResumed);
    FFileTransferResumed = 0;
  }

  __int64 Diff = Bytes - OperationProgress->TransferedSize;
  if (ALWAYS_TRUE(Diff >= 0))
  {
    OperationProgress->AddTransfered(Diff);
  }

  if (OperationProgress->Cancel == csCancel)
  {
    FFileTransferCancelled = true;
    FFileTransferAbort = ftaCancel;
    FFileZillaIntf->Cancel();
  }

  if (FFileTransferCPSLimit != OperationProgress->CPSLimit)
  {
    FFileTransferCPSLimit = OperationProgress->CPSLimit;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::FileTransferProgress(__int64 TransferSize,
  __int64 Bytes)
{
  TGuard Guard(FTransferStatusCriticalSection);

  DoFileTransferProgress(TransferSize, Bytes);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::FileTransfer(const UnicodeString & FileName,
  const UnicodeString & LocalFile, const UnicodeString & RemoteFile,
  const UnicodeString & RemotePath, bool Get, __int64 Size, int Type,
  TFileTransferData & UserData, TFileOperationProgressType * OperationProgress)
{
  FILE_OPERATION_LOOP(FMTLOAD(TRANSFER_ERROR, (FileName)),
    FFileZillaIntf->FileTransfer(LocalFile.c_str(), RemoteFile.c_str(),
      RemotePath.c_str(), Get, Size, Type, &UserData);
    // we may actually catch response code of the listing
    // command (when checking for existence of the remote file)
    unsigned int Reply = WaitForCommandReply();
    GotReply(Reply, FLAGMASK(FFileTransferCancelled, REPLY_ALLOW_CANCEL));
  );

  switch (FFileTransferAbort)
  {
    case ftaSkip:
      THROW_SKIP_FILE_NULL;

    case ftaCancel:
      Abort();
      break;
  }

  if (!FFileTransferCancelled)
  {
    // show completion of transfer
    // call non-guarded variant to avoid deadlock with keepalives
    // (we are not waiting for reply anymore so keepalives are free to proceed)
    DoFileTransferProgress(OperationProgress->TransferSize, OperationProgress->TransferSize);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;
  UnicodeString FullTargetDir = IncludeTrailingBackslash(TargetDir);

  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    UnicodeString FileName = FilesToCopy->Strings[Index];
    const TRemoteFile * File = dynamic_cast<const TRemoteFile *>(FilesToCopy->Objects[Index]);
    bool Success = false;

    try
    {
      try
      {
        SinkRobust(AbsolutePath(FileName, false), File, FullTargetDir, CopyParam, Params,
          OperationProgress, tfFirstLevel);
        Success = true;
        FLastDataSent = Now();
      }
      catch(EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SinkRobust(const UnicodeString FileName,
  const TRemoteFile * File, const UnicodeString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem
  bool Retry;

  TDownloadSessionAction Action(FTerminal->ActionLog);

  do
  {
    Retry = false;
    try
    {
      Sink(FileName, File, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch(Exception & E)
    {
      Retry = true;
      if (FTerminal->Active ||
          !FTerminal->QueryReopen(&E, ropNoReadDirectory, OperationProgress))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      assert(File != NULL);
      if (!File->IsDirectory)
      {
        // prevent overwrite confirmations
        Params |= cpNoConfirmation;
        Flags |= tfAutoResume;
      }
    }
  }
  while (Retry);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Sink(const UnicodeString FileName,
  const TRemoteFile * File, const UnicodeString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TDownloadSessionAction & Action)
{
  UnicodeString OnlyFileName = UnixExtractFileName(FileName);

  Action.FileName(FileName);

  TFileMasks::TParams MaskParams;
  assert(File);
  MaskParams.Size = File->Size;
  MaskParams.Modification = File->Modification;

  if (!CopyParam->AllowTransfer(FileName, osRemote, File->IsDirectory, MaskParams))
  {
    FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  FTerminal->LogEvent(FORMAT(L"File: \"%s\"", (FileName)));

  OperationProgress->SetFile(OnlyFileName);

  UnicodeString DestFileName = CopyParam->ChangeFileName(OnlyFileName,
    osRemote, FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = TargetDir + DestFileName;

  if (File->IsDirectory)
  {
    Action.Cancel();
    if (!File->IsSymLink)
    {
      FILE_OPERATION_LOOP (FMTLOAD(NOT_DIRECTORY_ERROR, (DestFullName)),
        int Attrs = FileGetAttr(DestFullName);
        if (FLAGCLEAR(Attrs, faDirectory))
        {
          EXCEPTION;
        }
      );

      FILE_OPERATION_LOOP (FMTLOAD(CREATE_DIR_ERROR, (DestFullName)),
        THROWOSIFFALSE(ForceDirectories(DestFullName));
      );

      TSinkFileParams SinkFileParams;
      SinkFileParams.TargetDir = IncludeTrailingBackslash(DestFullName);
      SinkFileParams.CopyParam = CopyParam;
      SinkFileParams.Params = Params;
      SinkFileParams.OperationProgress = OperationProgress;
      SinkFileParams.Skipped = false;
      SinkFileParams.Flags = Flags & ~(tfFirstLevel | tfAutoResume);

      FTerminal->ProcessDirectory(FileName, SinkFile, &SinkFileParams);

      // Do not delete directory if some of its files were skip.
      // Throw "skip file" for the directory to avoid attempt to deletion
      // of any parent directory
      if (FLAGSET(Params, cpDelete) && SinkFileParams.Skipped)
      {
        THROW_SKIP_FILE_NULL;
      }
    }
    else
    {
      // file is symlink to directory, currently do nothing, but it should be
      // reported to user
    }
  }
  else
  {
    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to local directory started.", (FileName)));

    // Will we use ASCII of BINARY file transfer?
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osRemote, MaskParams));
    FTerminal->LogEvent(UnicodeString((OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary")) +
      L" transfer mode selected.");

    // Suppose same data size to transfer as to write
    OperationProgress->SetTransferSize(File->Size);
    OperationProgress->SetLocalSize(OperationProgress->TransferSize);

    int Attrs;
    FILE_OPERATION_LOOP (FMTLOAD(NOT_FILE_ERROR, (DestFullName)),
      Attrs = FileGetAttr(DestFullName);
      if ((Attrs >= 0) && FLAGSET(Attrs, faDirectory))
      {
        EXCEPTION;
      }
    );

    OperationProgress->TransferingFile = false; // not set with FTP protocol

    ResetFileTransfer();

    TFileTransferData UserData;

    UnicodeString FilePath = UnixExtractFilePath(FileName);
    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

    {
      // ignore file list
      TFileListHelper Helper(this, NULL, true);

      FFileTransferCPSLimit = OperationProgress->CPSLimit;
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      UserData.FileName = DestFileName;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      UserData.CopyParam = CopyParam;
      UserData.Modification = File->Modification;
      FileTransfer(FileName, DestFullName, OnlyFileName,
        FilePath, true, File->Size, TransferType, UserData, OperationProgress);
    }

    // in case dest filename is changed from overwrite dialog
    if (DestFileName != UserData.FileName)
    {
      DestFullName = TargetDir + UserData.FileName;
      Attrs = FileGetAttr(DestFullName);
    }

    Action.Destination(ExpandUNCFileName(DestFullName));

    if (Attrs == -1)
    {
      Attrs = faArchive;
    }
    int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
    if ((NewAttrs & Attrs) != NewAttrs)
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFullName)),
        THROWOSIFFALSE(FileSetAttr(DestFullName, Attrs | NewAttrs) == 0);
      );
    }
  }

  if (FLAGSET(Params, cpDelete))
  {
    // If file is directory, do not delete it recursively, because it should be
    // empty already. If not, it should not be deleted (some files were
    // skipped or some new files were copied to it, while we were downloading)
    int Params = dfNoRecursive;
    FTerminal->DeleteFile(FileName, File, &Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SinkFile(UnicodeString FileName,
  const TRemoteFile * File, void * Param)
{
  TSinkFileParams * Params = (TSinkFileParams *)Param;
  assert(Params->OperationProgress);
  try
  {
    SinkRobust(FileName, File, Params->TargetDir, Params->CopyParam,
      Params->Params, Params->OperationProgress, Params->Flags);
  }
  catch(EScpSkipFile & E)
  {
    TFileOperationProgressType * OperationProgress = Params->OperationProgress;

    Params->Skipped = true;

    SUSPEND_OPERATION (
      if (!FTerminal->HandleException(&E))
      {
        throw;
      }
    );

    if (OperationProgress->Cancel)
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToRemote(TStrings * FilesToCopy,
  const UnicodeString ATargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  assert((FilesToCopy != NULL) && (OperationProgress != NULL));

  Params &= ~cpAppend;
  UnicodeString FileName, FileNameOnly;
  UnicodeString TargetDir = AbsolutePath(ATargetDir, false);
  UnicodeString FullTargetDir = UnixIncludeTrailingBackslash(TargetDir);
  int Index = 0;
  while ((Index < FilesToCopy->Count) && !OperationProgress->Cancel)
  {
    bool Success = false;
    FileName = FilesToCopy->Strings[Index];
    FileNameOnly = ExtractFileName(FileName);

    try
    {
      try
      {
        if (FTerminal->SessionData->CacheDirectories)
        {
          FTerminal->DirectoryModified(TargetDir, false);

          if (DirectoryExists(FileName))
          {
            FTerminal->DirectoryModified(FullTargetDir + FileNameOnly, true);
          }
        }
        SourceRobust(FileName, FullTargetDir, CopyParam, Params, OperationProgress,
          tfFirstLevel);
        Success = true;
        FLastDataSent = Now();
      }
      catch(EScpSkipFile & E)
      {
        SUSPEND_OPERATION (
          if (!FTerminal->HandleException(&E)) throw;
        );
      }
    }
    __finally
    {
      OperationProgress->Finish(FileName, Success, OnceDoneOperation);
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SourceRobust(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem
  bool Retry;

  TUploadSessionAction Action(FTerminal->ActionLog);

  do
  {
    Retry = false;
    try
    {
      Source(FileName, TargetDir, CopyParam, Params, OperationProgress,
        Flags, Action);
    }
    catch(Exception & E)
    {
      Retry = true;
      if (FTerminal->Active ||
          !FTerminal->QueryReopen(&E, ropNoReadDirectory, OperationProgress))
      {
        FTerminal->RollbackAction(Action, OperationProgress, &E);
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
      Action.Restart();
      // prevent overwrite confirmations
      // (should not be set for directories!)
      Params |= cpNoConfirmation;
      Flags |= tfAutoResume;
    }
  }
  while (Retry);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Source(const UnicodeString FileName,
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action)
{
  FTerminal->LogEvent(FORMAT(L"File: \"%s\"", (FileName)));

  Action.FileName(ExpandUNCFileName(FileName));

  OperationProgress->SetFile(FileName, false);

  if (!FTerminal->AllowLocalFileTransfer(FileName, CopyParam))
  {
    FTerminal->LogEvent(FORMAT(L"File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  __int64 Size;
  int Attrs;

  FTerminal->OpenLocalFile(FileName, GENERIC_READ, &Attrs,
    NULL, NULL, NULL, NULL, &Size);

  OperationProgress->SetFileInProgress();

  bool Dir = FLAGSET(Attrs, faDirectory);
  if (Dir)
  {
    Action.Cancel();
    DirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
      Attrs, CopyParam, Params, OperationProgress, Flags);
  }
  else
  {
    UnicodeString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName),
      osLocal, FLAGSET(Flags, tfFirstLevel));

    FTerminal->LogEvent(FORMAT(L"Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->TransferingFile = false;

    TDateTime Modification;
    // Inspired by SysUtils::FileAge
    WIN32_FIND_DATA FindData;
    HANDLE Handle = FindFirstFile(FileName.c_str(), &FindData);
    if (Handle != INVALID_HANDLE_VALUE)
    {
      Modification =
        UnixToDateTime(
          ConvertTimestampToUnixSafe(FindData.ftLastWriteTime, dstmUnix),
          dstmUnix);
      FindClose(Handle);
    }

    // Will we use ASCII of BINARY file transfer?
    TFileMasks::TParams MaskParams;
    MaskParams.Size = Size;
    MaskParams.Modification = Modification;
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osLocal, MaskParams));
    FTerminal->LogEvent(
      UnicodeString(OperationProgress->AsciiTransfer ? L"Ascii" : L"Binary") +
        L" transfer mode selected.");

    ResetFileTransfer();

    TFileTransferData UserData;

    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);

    {
      // ignore file list
      TFileListHelper Helper(this, NULL, true);

      FFileTransferCPSLimit = OperationProgress->CPSLimit;
      // not used for uploads anyway
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      // not used for uploads, but we get new name (if any) back in this field
      UserData.FileName = DestFileName;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      UserData.CopyParam = CopyParam;
      UserData.Modification = Modification;
      FileTransfer(FileName, FileName, DestFileName,
        TargetDir, false, Size, TransferType, UserData, OperationProgress);
    }

    UnicodeString DestFullName = TargetDir + UserData.FileName;
    // only now, we know the final destination
    Action.Destination(DestFullName);

    // we are not able to tell if setting timestamp succeeded,
    // so we log it always (if supported)
    if (FFileTransferPreserveTime &&
        (FServerCapabilities->GetCapability(mfmt_command) == yes))
    {
      TTouchSessionAction TouchAction(FTerminal->ActionLog, DestFullName, Modification);
    }
  }

  /* TODO : Delete also read-only files. */
  if (FLAGSET(Params, cpDelete))
  {
    if (!Dir)
    {
      FILE_OPERATION_LOOP (FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)),
        THROWOSIFFALSE(Sysutils::DeleteFile(FileName));
      )
    }
  }
  else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
  {
    FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
      THROWOSIFFALSE(FileSetAttr(FileName, Attrs & ~faArchive) == 0);
    )
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DirectorySource(const UnicodeString DirectoryName,
  const UnicodeString TargetDir, int Attrs, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  UnicodeString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName)), osLocal,
    FLAGSET(Flags, tfFirstLevel));
  UnicodeString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);

  OperationProgress->SetFile(DirectoryName);

  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  TSearchRec SearchRec;
  bool FindOK;

  FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
    FindOK = (bool)(FindFirstChecked(DirectoryName + L"*.*",
      FindAttrs, SearchRec) == 0);
  );

  bool CreateDir = true;

  try
  {
    while (FindOK && !OperationProgress->Cancel)
    {
      UnicodeString FileName = DirectoryName + SearchRec.Name;
      try
      {
        if ((SearchRec.Name != L".") && (SearchRec.Name != L".."))
        {
          SourceRobust(FileName, DestFullName, CopyParam, Params, OperationProgress,
            Flags & ~(tfFirstLevel | tfAutoResume));
          // if any file got uploaded (i.e. there were any file in the
          // directory and at least one was not skipped),
          // do not try to create the directory,
          // as it should be already created by FZAPI during upload
          CreateDir = false;
        }
      }
      catch (EScpSkipFile &E)
      {
        // If ESkipFile occurs, just log it and continue with next file
        SUSPEND_OPERATION (
          // here a message to user was displayed, which was not appropriate
          // when user refused to overwrite the file in subdirectory.
          // hopefuly it won't be missing in other situations.
          if (!FTerminal->HandleException(&E)) throw;
        );
      }

      FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
        FindOK = (FindNextChecked(SearchRec) == 0);
      );
    }
  }
  __finally
  {
    FindClose(SearchRec);
  }

  if (CreateDir)
  {
    TRemoteProperties Properties;
    if (CopyParam->PreserveRights)
    {
      Properties.Valid = TValidProperties() << vpRights;
      Properties.Rights = CopyParam->RemoteFileRights(Attrs);
    }

    try
    {
      FTerminal->ExceptionOnFail = true;
      try
      {
        FTerminal->CreateDirectory(DestFullName, &Properties);
      }
      __finally
      {
        FTerminal->ExceptionOnFail = false;
      }
    }
    catch(...)
    {
      TRemoteFile * File = NULL;
      // ignore non-fatal error when the directory already exists
      bool Rethrow =
        !FTerminal->Active ||
        !FTerminal->FileExists(UnixExcludeTrailingBackslash(DestFullName), &File) ||
        !File->IsDirectory;
      delete File;
      if (Rethrow)
      {
        throw;
      }
    }
  }

  /* TODO : Delete also read-only directories. */
  /* TODO : Show error message on failure. */
  if (!OperationProgress->Cancel)
  {
    if (FLAGSET(Params, cpDelete))
    {
      RemoveDir(DirectoryName);
    }
    else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DirectoryName)),
        THROWOSIFFALSE(FileSetAttr(DirectoryName, Attrs & ~faArchive) == 0);
      )
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateDirectory(const UnicodeString ADirName)
{
  UnicodeString DirName = AbsolutePath(ADirName, false);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->MakeDir(DirName.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateLink(const UnicodeString /*FileName*/,
  const UnicodeString /*PointTo*/, bool /*Symbolic*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DeleteFile(const UnicodeString AFileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePath = UnixExtractFilePath(FileName);

  bool Dir = (File != NULL) && File->IsDirectory && !File->IsSymLink;

  if (Dir && FLAGCLEAR(Params, dfNoRecursive))
  {
    try
    {
      FTerminal->ProcessDirectory(FileName, FTerminal->DeleteFile, &Params);
    }
    catch(...)
    {
      Action.Cancel();
      throw;
    }
  }

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    if (Dir)
    {
      // If current remote directory is in the directory being removed,
      // some servers may refuse to delete it
      // This is common as ProcessDirectory above would CWD to
      // the directory to LIST it.
      // EnsureLocation should reset actual current directory to user's working directory.
      // If user's working directory is still below deleted directory, it is
      // perfectly correct to report an error.
      if (UnixIsChildPath(ActualCurrentDirectory(), FileName))
      {
        EnsureLocation();
      }
      FFileZillaIntf->RemoveDir(FileNameOnly.c_str(), FilePath.c_str());
    }
    else
    {
      FFileZillaIntf->Delete(FileNameOnly.c_str(), FilePath.c_str());
    }
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CustomCommandOnFile(const UnicodeString /*FileName*/,
  const TRemoteFile * /*File*/, UnicodeString /*Command*/, int /*Params*/,
  TCaptureOutputEvent /*OutputEvent*/)
{
  // if ever implemented, do not forget to add EnsureLocation,
  // see AnyCommand for a reason why
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoStartup()
{
  TStrings * PostLoginCommands = new TStringList();
  try
  {
    PostLoginCommands->Text = FTerminal->SessionData->PostLoginCommands;
    for (int Index = 0; Index < PostLoginCommands->Count; Index++)
    {
      UnicodeString Command = PostLoginCommands->Strings[Index];
      if (!Command.IsEmpty())
      {
        FFileZillaIntf->CustomCommand(Command.c_str());

        GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_3XX_CODE);
      }
    }
  }
  __finally
  {
    delete PostLoginCommands;
  }

  // retrieve initialize working directory to save it as home directory
  ReadCurrentDirectory();
  FHomeDirectory = FCurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::HomeDirectory()
{
  // FHomeDirectory is an absolute path, so avoid unnecessary overhead
  // of ChangeDirectory, such as EnsureLocation
  DoChangeDirectory(FHomeDirectory);
  FCurrentDirectory = FHomeDirectory;
  // make sure FZAPI is aware that we changed current working directory
  FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::IsCapable(int Capability) const
{
  assert(FTerminal);
  switch (Capability)
  {
    case fcResolveSymlink: // sic
    case fcTextMode:
    case fcModeChanging: // but not fcModeChangingUpload
    case fcNewerOnlyUpload:
    case fcAnyCommand: // but not fcShellAnyCommand
    case fcRename:
    case fcRemoteMove:
      return true;

    case fcPreservingTimestampUpload:
      return (FServerCapabilities->GetCapability(mfmt_command) == yes);

    case fcModeChangingUpload:
    case fcLoadingAdditionalProperties:
    case fcShellAnyCommand:
    case fcCalculatingChecksum:
    case fcHardLink:
    case fcSymbolicLink:
    case fcCheckingSpaceAvailable:
    case fcUserGroupListing:
    case fcGroupChanging:
    case fcOwnerChanging:
    case fcGroupOwnerChangingByID:
    case fcSecondaryShell:
    case fcRemoteCopy:
    case fcNativeTextMode:
    case fcTimestampChanging:
    case fcIgnorePermErrors:
      return false;

    default:
      assert(false);
      return false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::LookupUsersGroups()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadCurrentDirectory()
{
  // ask the server for current directory on startup only
  // and immediately after call to CWD,
  // later our current directory may be not synchronized with FZAPI current
  // directory anyway, see comments in EnsureLocation
  if (FCurrentDirectory.IsEmpty())
  {
    FFileZillaIntf->CustomCommand(L"PWD");

    unsigned int Code;
    TStrings * Response = NULL;
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE, L"", &Code, &Response);

    try
    {
      assert(Response != NULL);
      bool Result = false;

      // the only allowed 2XX code to "PWD"
      if ((Code == 257) &&
          (Response->Count == 1))
      {
        UnicodeString Path = Response->Text;

        int P = Path.Pos(L"\"");
        if (P == 0)
        {
          // some systems use single quotes, be tolerant
          P = Path.Pos(L"'");
        }
        if (P != 0)
        {
          Path.Delete(1, P - 1);

          if (Unquote(Path))
          {
            FCurrentDirectory = UnixExcludeTrailingBackslash(Path);
            Result = true;
          }
        }
      }

      if (Result)
      {
        FFileZillaIntf->SetCurrentPath(FCurrentDirectory.c_str());
      }
      else
      {
        throw Exception(FMTLOAD(FTP_PWD_RESPONSE_ERROR, (Response->Text)));
      }
    }
    __finally
    {
      delete Response;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoReadDirectory(TRemoteFileList * FileList)
{
  FileList->Reset();
  // FZAPI does not list parent directory, add it
  FileList->AddFile(new TRemoteParentDirectory(FTerminal));

  FLastReadDirectoryProgress = 0;

  TFileListHelper Helper(this, FileList, false);

  // always specify path to list, do not attempt to list "current" dir as:
  // 1) List() lists again the last listed directory, not the current working directory
  // 2) we handle this way the cached directory change
  UnicodeString Directory = AbsolutePath(FileList->Directory, false);
  FFileZillaIntf->List(Directory.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);

  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  // whole below "-a" logic is for LIST,
  // if we know we are going to use MLSD, skip it
  if (FFileZillaIntf->UsingMlsd())
  {
    DoReadDirectory(FileList);
  }
  else
  {
    bool GotNoFilesForAll = false;
    bool Repeat;

    do
    {
      Repeat = false;
      try
      {
        FDoListAll = (FListAll == asAuto) || (FListAll == asOn);
        DoReadDirectory(FileList);

        // We got no files with "-a", but again no files w/o "-a",
        // so it was not "-a"'s problem, revert to auto and let it decide the next time
        if (GotNoFilesForAll && (FileList->Count == 0))
        {
          assert(FListAll == asOff);
          FListAll = asAuto;
        }
        else if (FListAll == asAuto)
        {
          // some servers take "-a" as a mask and return empty directory listing
          // (note that it's actually never empty here, there's always at least parent directory,
          // added explicitly by DoReadDirectory)
          if ((FileList->Count == 0) ||
              ((FileList->Count == 1) && FileList->Files[0]->IsParentDirectory))
          {
            Repeat = true;
            FListAll = asOff;
            GotNoFilesForAll = true;
          }
          else
          {
            // reading first directory has succeeded, always use "-a"
            FListAll = asOn;
          }
        }

        // use "-a" even for implicit directory reading by FZAPI?
        // (e.g. before file transfer)
        FDoListAll = (FListAll == asOn);
      }
      catch(Exception & E)
      {
        FDoListAll = false;
        // reading the first directory has failed,
        // further try without "-a" only as the server may not support it
        if (FListAll == asAuto)
        {
          if (!FTerminal->Active)
          {
            FTerminal->Reopen(ropNoReadDirectory);
          }

          FListAll = asOff;
          Repeat = true;
        }
        else
        {
          throw;
        }
      }
    }
    while (Repeat);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoReadFile(const UnicodeString & FileName,
  TRemoteFile *& AFile)
{
  // end-user has right to expect that client current directory is really
  // current directory for the server
  EnsureLocation();

  TRemoteFileList * FileList = new TRemoteFileList();
  try
  {
    TFileListHelper Helper(this, FileList, false);
    FFileZillaIntf->ListFile(FileName.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);
    TRemoteFile * File = FileList->FindFile(UnixExtractFileName(FileName));
    if (File != NULL)
    {
      AFile = File->Duplicate();
    }

    FLastDataSent = Now();
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadFile(const UnicodeString FileName,
  TRemoteFile *& File)
{
  UnicodeString Path = UnixExtractFilePath(FileName);
  UnicodeString NameOnly = UnixExtractFileName(FileName);
  TRemoteFile *AFile = NULL;
  bool Own;
  if (FServerCapabilities->GetCapability(mlsd_command) == yes)
  {
    DoReadFile(FileName, AFile);
    Own = true;
  }
  else
  {
    // FZAPI does not have efficient way to read properties of one file.
    // In case we need properties of set of files from the same directory,
    // cache the file list for future
    if ((FFileListCache != NULL) &&
        UnixComparePaths(Path, FFileListCache->Directory) &&
        (TTerminal::IsAbsolutePath(FFileListCache->Directory) ||
        (FFileListCachePath == CurrentDirectory)))
    {
      AFile = FFileListCache->FindFile(NameOnly);
    }
    // if cache is invalid or file is not in cache, (re)read the directory
    if (AFile == NULL)
    {
      TRemoteFileList * FileListCache = new TRemoteFileList();
      FileListCache->Directory = Path;
      try
      {
        ReadDirectory(FileListCache);
      }
      catch(...)
      {
        delete FileListCache;
        throw;
      }
      // set only after we successfully read the directory,
      // otherwise, when we reconnect from ReadDirectory,
      // the FFileListCache is reset from ResetCache.
      delete FFileListCache;
      FFileListCache = FileListCache;
      FFileListCachePath = GetCurrentDirectory();

      AFile = FFileListCache->FindFile(NameOnly);
    }

    Own = false;
  }

  if (AFile == NULL)
  {
    File = NULL;
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
  }

  assert(AFile != NULL);
  File = Own ? AFile : AFile->Duplicate();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  // Resolving symlinks over FTP is big overhead
  // (involves opening TCPIP connection for retrieving "directory listing").
  // Moreover FZAPI does not support that anyway.
  // Note that while we could use MLST to read the symlink,
  // it's hardly of any use as, if MLST is supported, we use MLSD to
  // retrieve directory listing and from MLSD we cannot atm detect that
  // the file is symlink anyway.
  File = new TRemoteFile(SymlinkFile);
  try
  {
    File->Terminal = FTerminal;
    File->FileName = UnixExtractFileName(SymlinkFile->LinkTo);
    // FZAPI treats all symlink target as directories
    File->Type = FILETYPE_DIRECTORY;
  }
  catch(...)
  {
    delete File;
    File = NULL;
    throw;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RenameFile(const UnicodeString AFileName,
  const UnicodeString ANewName)
{
  UnicodeString FileName = AbsolutePath(AFileName, false);
  UnicodeString NewName = AbsolutePath(ANewName, false);

  UnicodeString FileNameOnly = UnixExtractFileName(FileName);
  UnicodeString FilePathOnly = UnixExtractFilePath(FileName);
  UnicodeString NewNameOnly = UnixExtractFileName(NewName);
  UnicodeString NewPathOnly = UnixExtractFilePath(NewName);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->Rename(FileNameOnly.c_str(), NewNameOnly.c_str(),
      FilePathOnly.c_str(), NewPathOnly.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyFile(const UnicodeString FileName,
  const UnicodeString NewName)
{
  assert(false);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::FileUrl(const UnicodeString FileName)
{
  return FTerminal->FileUrl(L"ftp", FileName);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TFTPFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SpaceAvailable(const UnicodeString /*Path*/,
  TSpaceAvailable & /*ASpaceAvailable*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
const TSessionInfo & __fastcall TFTPFileSystem::GetSessionInfo()
{
  return FSessionInfo;
}
//---------------------------------------------------------------------------
const TFileSystemInfo & __fastcall TFTPFileSystem::GetFileSystemInfo(bool /*Retrieve*/)
{
  if (!FFileSystemInfoValid)
  {
    FFileSystemInfo.RemoteSystem = FSystem;
    FFileSystemInfo.RemoteSystem.Unique();

    if (FFeatures->Count == 0)
    {
      FFileSystemInfo.AdditionalInfo = LoadStr(FTP_NO_FEATURE_INFO);
    }
    else
    {
      FFileSystemInfo.AdditionalInfo =
        FORMAT(L"%s\r\n", (LoadStr(FTP_FEATURE_INFO)));
      for (int Index = 0; Index < FFeatures->Count; Index++)
      {
        FFileSystemInfo.AdditionalInfo += FORMAT(L"  %s\r\n", (FFeatures->Strings[Index]));
      }
    }

    for (int Index = 0; Index < fcCount; Index++)
    {
      FFileSystemInfo.IsCapable[Index] = IsCapable((TFSCapability)Index);
    }

    FFileSystemInfoValid = true;
  }
  return FFileSystemInfo;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::TemporaryTransferFile(const UnicodeString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetStoredCredentialsTried()
{
  return !FTerminal->SessionData->Password.IsEmpty();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::GetUserName()
{
  return FUserName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
const wchar_t * __fastcall TFTPFileSystem::GetOption(int OptionID) const
{
  TSessionData * Data = FTerminal->SessionData;

  switch (OptionID)
  {
    case OPTION_PROXYHOST:
    case OPTION_FWHOST:
      FOptionScratch = Data->ProxyHost;
      break;

    case OPTION_PROXYUSER:
    case OPTION_FWUSER:
      FOptionScratch = Data->ProxyUsername;
      break;

    case OPTION_PROXYPASS:
    case OPTION_FWPASS:
      FOptionScratch = Data->ProxyPassword;
      break;

    case OPTION_TRANSFERIP:
      FOptionScratch = FTerminal->Configuration->ExternalIpAddress;
      break;

    case OPTION_ANONPWD:
    case OPTION_TRANSFERIP6:
      FOptionScratch = L"";
      break;

    default:
      assert(false);
      FOptionScratch = L"";
  }

  return FOptionScratch.c_str();
}
//---------------------------------------------------------------------------
int __fastcall TFTPFileSystem::GetOptionVal(int OptionID) const
{
  TSessionData * Data = FTerminal->SessionData;
  int Result;

  switch (OptionID)
  {
    case OPTION_PROXYTYPE:
      switch (Data->ProxyMethod)
      {
        case ::pmNone:
          Result = 0; // PROXYTYPE_NOPROXY;
          break;

        case pmSocks4:
          Result = 2; // PROXYTYPE_SOCKS4A
          break;

        case pmSocks5:
          Result = 3; // PROXYTYPE_SOCKS5
          break;

        case pmHTTP:
          Result = 4; // PROXYTYPE_HTTP11
          break;

        case pmTelnet:
        case pmCmd:
        default:
          assert(false);
          Result = 0; // PROXYTYPE_NOPROXY;
          break;
      }
      break;

    case OPTION_PROXYPORT:
    case OPTION_FWPORT:
      Result = Data->ProxyPort;
      break;

    case OPTION_PROXYUSELOGON:
      Result = !Data->ProxyUsername.IsEmpty();
      break;

    case OPTION_LOGONTYPE:
      Result = Data->FtpProxyLogonType;
      break;

    case OPTION_TIMEOUTLENGTH:
      Result = Data->Timeout;
      break;

    case OPTION_DEBUGSHOWLISTING:
      Result = true;
      break;

    case OPTION_PASV:
      // should never get here t_server.nPasv being nonzero
      assert(false);
      Result = FALSE;
      break;

    case OPTION_PRESERVEDOWNLOADFILETIME:
    case OPTION_MPEXT_PRESERVEUPLOADFILETIME:
      Result = FFileTransferPreserveTime ? TRUE : FALSE;
      break;

    case OPTION_LIMITPORTRANGE:
      Result = FALSE;
      break;

    case OPTION_PORTRANGELOW:
    case OPTION_PORTRANGEHIGH:
      // should never get here OPTION_LIMITPORTRANGE being zero
      assert(false);
      Result = 0;
      break;

    case OPTION_ENABLE_IPV6:
      Result = ((Data->AddressFamily == afIPv6) ? TRUE : FALSE);
      break;

    case OPTION_KEEPALIVE:
      Result = ((Data->FtpPingType != ptOff) ? TRUE : FALSE);
      break;

    case OPTION_INTERVALLOW:
    case OPTION_INTERVALHIGH:
      Result = Data->FtpPingInterval;
      break;

    case OPTION_VMSALLREVISIONS:
      Result = FALSE;
      break;

    case OPTION_SPEEDLIMIT_DOWNLOAD_TYPE:
    case OPTION_SPEEDLIMIT_UPLOAD_TYPE:
      Result = (FFileTransferCPSLimit == 0 ? 0 : 1);
      break;

    case OPTION_SPEEDLIMIT_DOWNLOAD_VALUE:
    case OPTION_SPEEDLIMIT_UPLOAD_VALUE:
      Result = (FFileTransferCPSLimit / 1024); // FZAPI expects KiB/s
      break;

    case OPTION_MPEXT_SHOWHIDDEN:
      Result = (FDoListAll ? TRUE : FALSE);
      break;

    case OPTION_MPEXT_SSLSESSIONREUSE:
      Result = (Data->SslSessionReuse ? TRUE : FALSE);
      break;

    case OPTION_MPEXT_MIN_TLS_VERSION:
      Result = Data->MinTlsVersion;
      break;

    case OPTION_MPEXT_MAX_TLS_VERSION:
      Result = Data->MaxTlsVersion;
      break;

    case OPTION_MPEXT_SNDBUF:
      Result = Data->SendBuf;
      break;

    default:
      assert(false);
      Result = FALSE;
      break;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::PostMessage(unsigned int Type, WPARAM wParam, LPARAM lParam)
{
  if (Type == TFileZillaIntf::MSG_TRANSFERSTATUS)
  {
    // Stop here if FileTransferProgress is proceeding,
    // it makes "pause" in queue work.
    // Paused queue item stops in some of the TFileOperationProgressType
    // methods called from FileTransferProgress
    TGuard Guard(FTransferStatusCriticalSection);
  }

  TGuard Guard(FQueueCriticalSection);

  FQueue->push_back(TMessageQueue::value_type(wParam, lParam));
  SetEvent(FQueueEvent);

  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ProcessMessage()
{
  bool Result;
  TMessageQueue::value_type Message;

  {
    TGuard Guard(FQueueCriticalSection);

    Result = !FQueue->empty();
    if (Result)
    {
      Message = FQueue->front();
      FQueue->pop_front();
    }
    else
    {
      // now we are perfectly sure that the queue is empty as it is locked,
      // so reset the event
      ResetEvent(FQueueEvent);
    }
  }

  if (Result)
  {
    FFileZillaIntf->HandleMessage(Message.first, Message.second);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DiscardMessages()
{
  while (ProcessMessage());
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::WaitForMessages()
{
  unsigned int Result = WaitForSingleObject(FQueueEvent, INFINITE);
  if (Result != WAIT_OBJECT_0)
  {
    FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, (L"ftp#1", IntToStr(int(Result)))));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::PoolForFatalNonCommandReply()
{
  assert(FReply == 0);
  assert(FCommandReply == 0);
  assert(!FWaitingForReply);

  FWaitingForReply = true;

  unsigned int Reply;

  try
  {
    // discard up to one reply
    // (it should not happen here that two replies are posted anyway)
    while (ProcessMessage() && (FReply == 0));
    Reply = FReply;
  }
  __finally
  {
    FReply = 0;
    assert(FCommandReply == 0);
    FCommandReply = 0;
    assert(FWaitingForReply);
    FWaitingForReply = false;
  }

  if (Reply != 0)
  {
    // throws
    GotNonCommandReply(Reply);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::NoFinalLastCode()
{
  return (FLastCodeClass == 0) || (FLastCodeClass == 1);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::KeepWaitingForReply(unsigned int & ReplyToAwait, bool WantLastCode)
{
  // to keep waiting,
  // non-command reply must be unset,
  // the reply we wait for must be unset or
  // last code must be unset (if we wait for it)
  return
     (FReply == 0) &&
     ((ReplyToAwait == 0) ||
      (WantLastCode && NoFinalLastCode()));
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoWaitForReply(unsigned int & ReplyToAwait, bool WantLastCode)
{
  try
  {
    while (KeepWaitingForReply(ReplyToAwait, WantLastCode))
    {
      WaitForMessages();
      // wait for the first reply only,
      // i.e. in case two replies are posted get the first only.
      // e.g. when server closes the connection, but posts error message before,
      // sometime it happens that command (like download) fails because of the error
      // and does not catch the disconnection. then asynchronous "disconnect reply"
      // is posted immediately afterwards. leave detection of that to Idle()
      while (ProcessMessage() && KeepWaitingForReply(ReplyToAwait, WantLastCode));
    }

    if (FReply != 0)
    {
      // throws
      GotNonCommandReply(FReply);
    }
  }
  catch(...)
  {
    // even if non-fatal error happens, we must process pending message,
    // so that we "eat" the reply message, so that it gets not mistakenly
    // associated with future connect
    if (FTerminal->Active)
    {
      DoWaitForReply(ReplyToAwait, WantLastCode);
    }
    throw;
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFTPFileSystem::WaitForReply(bool Command, bool WantLastCode)
{
  assert(FReply == 0);
  assert(FCommandReply == 0);
  assert(!FWaitingForReply);

  ResetReply();
  FWaitingForReply = true;

  unsigned int Reply;

  try
  {
    unsigned int & ReplyToAwait = (Command ? FCommandReply : FReply);
    DoWaitForReply(ReplyToAwait, WantLastCode);

    Reply = ReplyToAwait;
  }
  __finally
  {
    FReply = 0;
    FCommandReply = 0;
    assert(FWaitingForReply);
    FWaitingForReply = false;
  }

  return Reply;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFTPFileSystem::WaitForCommandReply(bool WantLastCode)
{
  return WaitForReply(true, WantLastCode);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::WaitForFatalNonCommandReply()
{
  WaitForReply(false, false);
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ResetReply()
{
  FLastCode = 0;
  FLastCodeClass = 0;
  assert(FLastResponse != NULL);
  FLastResponse->Clear();
  assert(FLastErrorResponse != NULL);
  FLastErrorResponse->Clear();
  assert(FLastError != NULL);
  FLastError->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::GotNonCommandReply(unsigned int Reply)
{
  assert(FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));
  GotReply(Reply);
  // should never get here as GotReply should raise fatal exception
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::GotReply(unsigned int Reply, unsigned int Flags,
  UnicodeString Error, unsigned int * Code, TStrings ** Response)
{
  try
  {
    if (FLAGSET(Reply, TFileZillaIntf::REPLY_OK))
    {
      assert(Reply == TFileZillaIntf::REPLY_OK);

      // With REPLY_2XX_CODE treat "OK" non-2xx code like an error.
      // REPLY_3XX_CODE has to be always used along with REPLY_2XX_CODE.
      if ((FLAGSET(Flags, REPLY_2XX_CODE) && (FLastCodeClass != 2)) &&
          ((FLAGCLEAR(Flags, REPLY_3XX_CODE) || (FLastCodeClass != 3))))
      {
        GotReply(TFileZillaIntf::REPLY_ERROR, Flags, Error);
      }
    }
    else if (FLAGSET(Reply, TFileZillaIntf::REPLY_CANCEL) &&
        FLAGSET(Flags, REPLY_ALLOW_CANCEL))
    {
      assert(
        (Reply == (TFileZillaIntf::REPLY_CANCEL | TFileZillaIntf::REPLY_ERROR)) ||
        (Reply == (TFileZillaIntf::REPLY_ABORTED | TFileZillaIntf::REPLY_CANCEL | TFileZillaIntf::REPLY_ERROR)));
      // noop
    }
    // we do not expect these with our usage of FZ
    else if (Reply &
          (TFileZillaIntf::REPLY_WOULDBLOCK | TFileZillaIntf::REPLY_OWNERNOTSET |
           TFileZillaIntf::REPLY_INVALIDPARAM | TFileZillaIntf::REPLY_ALREADYCONNECTED |
           TFileZillaIntf::REPLY_IDLE | TFileZillaIntf::REPLY_NOTINITIALIZED |
           TFileZillaIntf::REPLY_ALREADYINIZIALIZED))
    {
      FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, (L"ftp#2", FORMAT(L"0x%x", (int(Reply))))));
    }
    else
    {
      // everything else must be an error or disconnect notification
      assert(
        FLAGSET(Reply, TFileZillaIntf::REPLY_ERROR) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));

      // TODO: REPLY_CRITICALERROR ignored

      // REPLY_NOTCONNECTED happens if connection is closed between moment
      // when FZAPI interface method dispatches the command to FZAPI thread
      // and moment when FZAPI thread receives the command
      bool Disconnected =
        FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED) ||
        FLAGSET(Reply, TFileZillaIntf::REPLY_NOTCONNECTED);

      AnsiString HelpKeyword;
      TStrings * MoreMessages = new TStringList();
      try
      {
        if (Disconnected)
        {
          if (FLAGCLEAR(Flags, REPLY_CONNECT))
          {
            MoreMessages->Add(LoadStr(LOST_CONNECTION));
            Discard();
            FTerminal->Closed();
          }
          else
          {
            // For connection failure, do not report that connection was lost,
            // its obvious.
            // Also do not report to terminal that we are closed as
            // that turns terminal into closed mode, but we want to
            // pretend (at least with failed authentication) to retry
            // with the same connection (as with SSH), so we explicitly
            // close terminal in Open() only after we give up
            Discard();
          }
        }

        if (FLAGSET(Reply, TFileZillaIntf::REPLY_ABORTED))
        {
          MoreMessages->Add(LoadStr(USER_TERMINATED));
        }

        if (FLAGSET(Reply, TFileZillaIntf::REPLY_NOTSUPPORTED))
        {
          MoreMessages->Add(LoadStr(FZ_NOTSUPPORTED));
        }

        if (FLastCode == 530)
        {
          MoreMessages->Add(LoadStr(AUTHENTICATION_FAILED));
        }

        if (FLastCode == 425)
        {
          if (!FTerminal->SessionData->FtpPasvMode)
          {
            MoreMessages->Add(LoadStr(FTP_CANNOT_OPEN_ACTIVE_CONNECTION2));
            HelpKeyword = HELP_FTP_CANNOT_OPEN_ACTIVE_CONNECTION;
          }
        }

        if (FLastCode == DummyTimeoutCode)
        {
          HelpKeyword = HELP_ERRORMSG_TIMEOUT;
        }

        MoreMessages->AddStrings(FLastError);
        // already cleared from WaitForReply, but GotReply can be also called
        // from Closed. then make sure that error from previous command not
        // associated with session closure is not reused
        FLastError->Clear();

        MoreMessages->AddStrings(FLastErrorResponse);
        // see comment for FLastError
        FLastResponse->Clear();
        FLastErrorResponse->Clear();

        if (MoreMessages->Count == 0)
        {
          delete MoreMessages;
          MoreMessages = NULL;
        }
      }
      catch(...)
      {
        delete MoreMessages;
        throw;
      }

      if (Error.IsEmpty() && (MoreMessages != NULL))
      {
        assert(MoreMessages->Count > 0);
        Error = MoreMessages->Strings[0];
        MoreMessages->Delete(0);
      }

      if (Disconnected)
      {
        // for fatal error, it is essential that there is some message
        assert(!Error.IsEmpty());
        ExtException * E = new ExtException(Error, MoreMessages, true, HelpKeyword);
        try
        {
          FTerminal->FatalError(E, L"");
        }
        __finally
        {
          delete E;
        }
      }
      else
      {
        throw ExtException(Error, MoreMessages, true, HelpKeyword);
      }
    }

    if ((Code != NULL) && (FLastCodeClass != DummyCodeClass))
    {
      *Code = FLastCode;
    }

    if (Response != NULL)
    {
      *Response = FLastResponse;
      FLastResponse = new TStringList();
      // just to be consistent
      delete FLastErrorResponse;
      FLastErrorResponse = new TStringList();
    }
  }
  __finally
  {
    ResetReply();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SetLastCode(int Code)
{
  FLastCode = Code;
  FLastCodeClass = (Code / 100);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::StoreLastResponse(const UnicodeString & Text)
{
  FLastResponse->Add(Text);
  if (FLastCodeClass >= 4)
  {
    FLastErrorResponse->Add(Text);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::HandleReplyStatus(UnicodeString Response)
{
  int Code;

  if (FOnCaptureOutput != NULL)
  {
    FOnCaptureOutput(Response, false);
  }

  // Two forms of multiline responses were observed
  // (the first is according to the RFC 959):

  // 211-Features:
  //  MDTM
  //  REST STREAM
  //  SIZE
  // 211 End

  // 211-Features:
  // 211-MDTM
  // 211-REST STREAM
  // 211-SIZE
  // 211-AUTH TLS
  // 211-PBSZ
  // 211-PROT
  // 211 End

  bool HasCodePrefix =
    (Response.Length() >= 3) &&
    TryStrToInt(Response.SubString(1, 3), Code) &&
    (Code >= 100) && (Code <= 599) &&
    ((Response.Length() == 3) || (Response[4] == L' ') || (Response[4] == L'-'));

  if (HasCodePrefix && !FMultineResponse)
  {
    FMultineResponse = (Response.Length() >= 4) && (Response[4] == L'-');
    FLastResponse->Clear();
    FLastErrorResponse->Clear();
    SetLastCode(Code);
    if (Response.Length() >= 5)
    {
      StoreLastResponse(Response.SubString(5, Response.Length() - 4));
    }
  }
  else
  {
    int Start;
    // response with code prefix
    if (HasCodePrefix && (FLastCode == Code))
    {
      // End of multiline response?
      if ((Response.Length() <= 3) || (Response[4] == L' '))
      {
        FMultineResponse = false;
      }
      Start = 5;
    }
    else
    {
      Start = (((Response.Length() >= 1) && (Response[1] == L' ')) ? 2 : 1);
    }

    // Intermediate empty lines are being added
    if (FMultineResponse || (Response.Length() >= Start))
    {
      StoreLastResponse(Response.SubString(Start, Response.Length() - Start + 1));
    }
  }

  if (!FMultineResponse)
  {
    if (FLastCode == 220)
    {
      if (FTerminal->Configuration->ShowFtpWelcomeMessage)
      {
        FTerminal->DisplayBanner(FLastResponse->Text);
      }
    }
    else if (FLastCommand == PASS)
    {
      // 530 = "Not logged in."
      if (FLastCode == 530)
      {
        FPasswordFailed = true;
      }
    }
    else if (FLastCommand == SYST)
    {
      assert(FSystem.IsEmpty());
      // Positive reply to "SYST" must be 215, see RFC 959
      if (FLastCode == 215)
      {
        FSystem = FLastResponse->Text.TrimRight();
        // full name is "Personal FTP Server PRO K6.0"
        if ((FListAll == asAuto) &&
            (FSystem.Pos(L"Personal FTP Server") > 0))
        {
          FTerminal->LogEvent(L"Server is known not to support LIST -a");
          FListAll = asOff;
        }
      }
      else
      {
        FSystem = L"";
      }
    }
    else if (FLastCommand == FEAT)
    {
      // Response to FEAT must be multiline, where leading and trailing line
      // is "meaningless". See RFC 2389.
      if ((FLastCode == 211) && (FLastResponse->Count > 2))
      {
        FLastResponse->Delete(0);
        FLastResponse->Delete(FLastResponse->Count - 1);
        FFeatures->Assign(FLastResponse);
      }
      else
      {
        FFeatures->Clear();
      }
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFTPFileSystem::ExtractStatusMessage(UnicodeString Status)
{
  // CApiLog::LogMessage
  // (note that the formatting may not be present when LogMessageRaw is used)
  int P1 = Status.Pos(L"): ");
  if (P1 > 0)
  {
    int P2 = Status.Pos(L".cpp(");
    if ((P2 > 0) && (P2 < P1))
    {
      int P3 = Status.Pos(L"   caller=0x");
      if ((P3 > 0) && (P3 > P1))
      {
        Status = Status.SubString(P1 + 3, P3 - P1 - 3);
      }
    }
  }
  return Status;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleStatus(const wchar_t * AStatus, int Type)
{
  TLogLineType LogType = (TLogLineType)-1;
  UnicodeString Status(AStatus);

  switch (Type)
  {
    case TFileZillaIntf::LOG_STATUS:
      FTerminal->Information(Status, true);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_COMMAND:
      if (Status == L"SYST")
      {
        FLastCommand = SYST;
      }
      else if (Status == L"FEAT")
      {
        FLastCommand = FEAT;
      }
      else if (Status.SubString(1, 5) == L"PASS ")
      {
        FLastCommand = PASS;
      }
      else
      {
        FLastCommand = CMD_UNKNOWN;
      }
      LogType = llInput;
      break;

    case TFileZillaIntf::LOG_ERROR:
    case TFileZillaIntf::LOG_APIERROR:
    case TFileZillaIntf::LOG_WARNING:
      // when timeout message occurs, break loop waiting for response code
      // by setting dummy one
      if (Type == TFileZillaIntf::LOG_ERROR)
      {
        if (Status == FTimeoutStatus)
        {
          if (NoFinalLastCode())
          {
            SetLastCode(DummyTimeoutCode);
          }
        }
        else if (Status == FDisconnectStatus)
        {
          if (NoFinalLastCode())
          {
            SetLastCode(DummyDisconnectCode);
          }
        }
      }
      // there can be multiple error messages associated with single failure
      // (such as "cannot open local file..." followed by "download failed")
      Status = ExtractStatusMessage(Status);
      FLastError->Add(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_REPLY:
      HandleReplyStatus(AStatus);
      LogType = llOutput;
      break;

    case TFileZillaIntf::LOG_INFO:
      Status = ExtractStatusMessage(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_DEBUG:
      // used for directory listing only
      LogType = llMessage;
      break;

    default:
      assert(false);
      break;
  }

  if (FTerminal->Log->Logging && (LogType != (TLogLineType)-1))
  {
    FTerminal->Log->Add(LogType, Status);
  }

  return true;
}
//---------------------------------------------------------------------------
TDateTime __fastcall TFTPFileSystem::ConvertLocalTimestamp(time_t Time)
{
  // This reverses how FZAPI converts FILETIME to time_t,
  // before passing it to FZ_ASYNCREQUEST_OVERWRITE.
  __int64 Timestamp;
  tm * Tm = localtime(&Time);
  if (Tm != NULL)
  {
    SYSTEMTIME SystemTime;
    SystemTime.wYear = static_cast<WORD>(Tm->tm_year + 1900);
    SystemTime.wMonth = static_cast<WORD>(Tm->tm_mon + 1);
    SystemTime.wDayOfWeek = 0;
    SystemTime.wDay = static_cast<WORD>(Tm->tm_mday);
    SystemTime.wHour = static_cast<WORD>(Tm->tm_hour);
    SystemTime.wMinute = static_cast<WORD>(Tm->tm_min);
    SystemTime.wSecond = static_cast<WORD>(Tm->tm_sec);
    SystemTime.wMilliseconds = 0;

    FILETIME LocalTime;
    SystemTimeToFileTime(&SystemTime, &LocalTime);
    FILETIME FileTime;
    LocalFileTimeToFileTime(&LocalTime, &FileTime);
    Timestamp = ConvertTimestampToUnixSafe(FileTime, dstmUnix);
  }
  else
  {
    // incorrect, but at least something
    Timestamp = Time;
  }

  return UnixToDateTime(Timestamp, dstmUnix);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestOverwrite(
  wchar_t * FileName1, size_t FileName1Len, const wchar_t * /*FileName2*/,
  const wchar_t * /*Path1*/, const wchar_t * /*Path2*/,
  __int64 Size1, __int64 Size2, time_t LocalTime,
  bool /*HasLocalTime*/, const TRemoteFileTime & RemoteTime, void * AUserData, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    TFileTransferData & UserData = *((TFileTransferData *)AUserData);
    if (UserData.OverwriteResult >= 0)
    {
      // on retry, use the same answer as on the first attempt
      RequestResult = UserData.OverwriteResult;
    }
    else
    {
      TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;
      UnicodeString FileName = FileName1;
      assert(UserData.FileName == FileName);
      TOverwriteMode OverwriteMode = omOverwrite;
      TOverwriteFileParams FileParams;
      bool NoFileParams =
        (Size1 < 0) || (LocalTime == 0) ||
        (Size2 < 0) || !RemoteTime.HasDate;
      if (!NoFileParams)
      {
        FileParams.SourceSize = Size2;
        FileParams.DestSize = Size1;

        if (OperationProgress->Side == osLocal)
        {
          FileParams.SourceTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.DestTimestamp, FileParams.DestPrecision);
        }
        else
        {
          FileParams.DestTimestamp = ConvertLocalTimestamp(LocalTime);
          RemoteFileTimeToDateTimeAndPrecision(RemoteTime, FileParams.SourceTimestamp, FileParams.SourcePrecision);
        }
      }

      if (ConfirmOverwrite(FileName, OverwriteMode, OperationProgress,
            (NoFileParams ? NULL : &FileParams), UserData.CopyParam, UserData.Params,
            UserData.AutoResume && UserData.CopyParam->AllowResume(FileParams.SourceSize)))
      {
        switch (OverwriteMode)
        {
          case omOverwrite:
            if (FileName != FileName1)
            {
              wcsncpy(FileName1, FileName.c_str(), FileName1Len);
              FileName1[FileName1Len - 1] = L'\0';
              UserData.FileName = FileName1;
              RequestResult = TFileZillaIntf::FILEEXISTS_RENAME;
            }
            else
            {
              RequestResult = TFileZillaIntf::FILEEXISTS_OVERWRITE;
            }
            break;

          case omResume:
            RequestResult = TFileZillaIntf::FILEEXISTS_RESUME;
            break;

          case omComplete:
            FTerminal->LogEvent(L"File transfer was completed before disconnect");
            RequestResult = TFileZillaIntf::FILEEXISTS_COMPLETE;
            break;

          default:
            assert(false);
            RequestResult = TFileZillaIntf::FILEEXISTS_OVERWRITE;
            break;
        }
      }
      else
      {
        RequestResult = TFileZillaIntf::FILEEXISTS_SKIP;
      }
    }

    // remember the answer for the retries
    UserData.OverwriteResult = RequestResult;

    if (RequestResult == TFileZillaIntf::FILEEXISTS_SKIP)
    {
      // when user chooses not to overwrite, break loop waiting for response code
      // by setting dummy one, as FZAPI won't do anything then
      SetLastCode(DummyTimeoutCode);
    }

    return true;
  }
}
//---------------------------------------------------------------------------
struct TClipboardHandler
{
  UnicodeString Text;

  void __fastcall Copy(TObject * /*Sender*/)
  {
    CopyToClipboard(Text);
  }
};
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatContactList(UnicodeString Entry1, UnicodeString Entry2)
{
  if (!Entry1.IsEmpty() && !Entry2.IsEmpty())
  {
    return FORMAT(L"%s, %s", (Entry1, Entry2));
  }
  else
  {
    return Entry1 + Entry2;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatContact(const TFtpsCertificateData::TContact & Contact)
{
  UnicodeString Result =
    FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 1),
      (FormatContactList(FormatContactList(FormatContactList(
        Contact.Organization, Contact.Unit), Contact.CommonName), Contact.Mail)));

  if ((wcslen(Contact.Country) > 0) ||
      (wcslen(Contact.StateProvince) > 0) ||
      (wcslen(Contact.Town) > 0))
  {
    Result +=
      FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 2),
        (FormatContactList(FormatContactList(
          Contact.Country, Contact.StateProvince), Contact.Town)));
  }

  if (wcslen(Contact.Other) > 0)
  {
    Result += FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 3), (Contact.Other));
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FormatValidityTime(const TFtpsCertificateData::TValidityTime & ValidityTime)
{
  return FormatDateTime(L"ddddd tt",
    EncodeDateVerbose(
      (unsigned short)ValidityTime.Year, (unsigned short)ValidityTime.Month,
      (unsigned short)ValidityTime.Day) +
    EncodeTimeVerbose(
      (unsigned short)ValidityTime.Hour, (unsigned short)ValidityTime.Min,
      (unsigned short)ValidityTime.Sec, 0));
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    FSessionInfo.CertificateFingerprint =
      BytesToHex(RawByteString((const char*)Data.Hash, Data.HashLen), false, L':');

    int VerificationResultStr;
    switch (Data.VerificationResult)
    {
      case X509_V_OK:
        VerificationResultStr = CERT_OK;
        break;
      case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT:
        VerificationResultStr = CERT_ERR_UNABLE_TO_GET_ISSUER_CERT;
        break;
      case X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE:
        VerificationResultStr = CERT_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE;
        break;
      case X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY:
        VerificationResultStr = CERT_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY;
        break;
      case X509_V_ERR_CERT_SIGNATURE_FAILURE:
        VerificationResultStr = CERT_ERR_CERT_SIGNATURE_FAILURE;
        break;
      case X509_V_ERR_CERT_NOT_YET_VALID:
        VerificationResultStr = CERT_ERR_CERT_NOT_YET_VALID;
        break;
      case X509_V_ERR_CERT_HAS_EXPIRED:
        VerificationResultStr = CERT_ERR_CERT_HAS_EXPIRED;
        break;
      case X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD:
        VerificationResultStr = CERT_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD;
        break;
      case X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD:
        VerificationResultStr = CERT_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD;
        break;
      case X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT:
        VerificationResultStr = CERT_ERR_DEPTH_ZERO_SELF_SIGNED_CERT;
        break;
      case X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN:
        VerificationResultStr = CERT_ERR_SELF_SIGNED_CERT_IN_CHAIN;
        break;
      case X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY:
        VerificationResultStr = CERT_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY;
        break;
      case X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE:
        VerificationResultStr = CERT_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE;
        break;
      case X509_V_ERR_INVALID_CA:
        VerificationResultStr = CERT_ERR_INVALID_CA;
        break;
      case X509_V_ERR_PATH_LENGTH_EXCEEDED:
        VerificationResultStr = CERT_ERR_PATH_LENGTH_EXCEEDED;
        break;
      case X509_V_ERR_INVALID_PURPOSE:
        VerificationResultStr = CERT_ERR_INVALID_PURPOSE;
        break;
      case X509_V_ERR_CERT_UNTRUSTED:
        VerificationResultStr = CERT_ERR_CERT_UNTRUSTED;
        break;
      case X509_V_ERR_CERT_REJECTED:
        VerificationResultStr = CERT_ERR_CERT_REJECTED;
        break;
      case X509_V_ERR_KEYUSAGE_NO_CERTSIGN:
        VerificationResultStr = CERT_ERR_KEYUSAGE_NO_CERTSIGN;
        break;
      case X509_V_ERR_CERT_CHAIN_TOO_LONG:
        VerificationResultStr = CERT_ERR_CERT_CHAIN_TOO_LONG;
        break;
      default:
        VerificationResultStr = CERT_ERR_UNKNOWN;
        break;
    }

    UnicodeString Summary = LoadStr(VerificationResultStr);
    if (Data.VerificationResult != X509_V_OK)
    {
      Summary += L" " + FMTLOAD(CERT_ERRDEPTH, (Data.VerificationDepth + 1));
    }

    FSessionInfo.Certificate =
      FMTLOAD(CERT_TEXT, (
        FormatContact(Data.Issuer),
        FormatContact(Data.Subject),
        FormatValidityTime(Data.ValidFrom),
        FormatValidityTime(Data.ValidUntil),
        FSessionInfo.CertificateFingerprint,
        Summary));

    RequestResult = 0;

    THierarchicalStorage * Storage =
      FTerminal->Configuration->CreateScpStorage(false);
    try
    {
      Storage->AccessMode = smRead;

      if (Storage->OpenSubKey(CertificateStorageKey, false) &&
          Storage->ValueExists(FSessionInfo.CertificateFingerprint))
      {
        RequestResult = 1;
      }
    }
    __finally
    {
      delete Storage;
    }

    if (RequestResult == 0)
    {
      UnicodeString Buf = FTerminal->SessionData->HostKey;
      while ((RequestResult == 0) && !Buf.IsEmpty())
      {
        UnicodeString ExpectedKey = CutToChar(Buf, L';', false);
        if (ExpectedKey == L"*")
        {
          UnicodeString Message = LoadStr(ANY_CERTIFICATE);
          FTerminal->Information(Message, true);
          FTerminal->Log->Add(llException, Message);
          RequestResult = 1;
        }
        else if (ExpectedKey == FSessionInfo.CertificateFingerprint)
        {
          RequestResult = 1;
        }
      }
    }

    if (RequestResult == 0)
    {
      TClipboardHandler ClipboardHandler;
      ClipboardHandler.Text = FSessionInfo.CertificateFingerprint;

      TQueryButtonAlias Aliases[1];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(COPY_KEY_BUTTON);
      Aliases[0].OnClick = &ClipboardHandler.Copy;

      TQueryParams Params;
      Params.HelpKeyword = HELP_VERIFY_CERTIFICATE;
      Params.NoBatchAnswers = qaYes | qaRetry;
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);
      unsigned int Answer = FTerminal->QueryUser(
        FMTLOAD(VERIFY_CERT_PROMPT2, (FSessionInfo.Certificate)),
        NULL, qaYes | qaNo | qaCancel | qaRetry, &Params, qtWarning);

      switch (Answer)
      {
        case qaYes:
          // 2 = always, as used by FZ's VerifyCertDlg.cpp,
          // however FZAPI takes all non-zero values equally
          RequestResult = 2;
          break;

        case qaNo:
          RequestResult = 1;
          break;

        case qaCancel:
          RequestResult = 0;
          break;

        default:
          assert(false);
          RequestResult = 0;
          break;
      }

      if (RequestResult == 2)
      {
        THierarchicalStorage * Storage =
          FTerminal->Configuration->CreateScpStorage(false);
        try
        {
          Storage->AccessMode = smReadWrite;

          if (Storage->OpenSubKey(CertificateStorageKey, true))
          {
            Storage->WriteString(FSessionInfo.CertificateFingerprint, L"");
          }
        }
        __finally
        {
          delete Storage;
        }
      }
    }

    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestNeedPass(
  struct TNeedPassRequestData & Data, int & RequestResult)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    UnicodeString Password = L"";
    if (FTerminal->PromptUser(FTerminal->SessionData, pkPassword, LoadStr(PASSWORD_TITLE), L"",
      LoadStr(PASSWORD_PROMPT), false, 0, Password))
    {
      Data.Password = _wcsdup(Password.c_str());
      RequestResult = TFileZillaIntf::REPLY_OK;
    }
    else
    {
      RequestResult = TFileZillaIntf::REPLY_ABORTED;
    }
    return true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::RemoteFileTimeToDateTimeAndPrecision(const TRemoteFileTime & Source, TDateTime & DateTime, TModificationFmt & ModificationFmt)
{
  // ModificationFmt must be set after Modification
  if (Source.HasDate)
  {
    DateTime =
      EncodeDateVerbose((unsigned short)Source.Year, (unsigned short)Source.Month,
        (unsigned short)Source.Day);
    if (Source.HasTime)
    {
      DateTime = DateTime +
        EncodeTimeVerbose((unsigned short)Source.Hour, (unsigned short)Source.Minute,
          (unsigned short)Source.Second, 0);
      // not exact as we got year as well, but it is most probably
      // guessed by FZAPI anyway
      ModificationFmt = Source.HasSeconds ? mfFull : mfMDHM;
    }
    else
    {
      ModificationFmt = mfMDY;
    }

    if (Source.Utc)
    {
      DateTime = ConvertTimestampFromUTC(DateTime);
    }

  }
  else
  {
    // With SCP we estimate date to be today, if we have at least time

    DateTime = double(0);
    ModificationFmt = mfNone;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleListData(const wchar_t * Path,
  const TListDataEntry * Entries, unsigned int Count)
{
  if (!FActive)
  {
    return false;
  }
  else if (FIgnoreFileList)
  {
    // directory listing provided implicitly by FZAPI during certain operations is ignored
    assert(FFileList == NULL);
    return false;
  }
  else
  {
    assert(FFileList != NULL);
    // this can actually fail in real life,
    // when connected to server with case insensitive paths
    assert(UnixComparePaths(AbsolutePath(FFileList->Directory, false), Path));
    USEDPARAM(Path);

    for (unsigned int Index = 0; Index < Count; Index++)
    {
      const TListDataEntry * Entry = &Entries[Index];
      TRemoteFile * File = new TRemoteFile();
      try
      {
        File->Terminal = FTerminal;

        File->FileName = Entry->Name;
        try
        {
          int PermissionsLen = wcslen(Entry->Permissions);
          if (PermissionsLen >= 10)
          {
            File->Rights->Text = Entry->Permissions + 1;
          }
          else if ((PermissionsLen == 3) || (PermissionsLen == 4))
          {
            File->Rights->Octal = Entry->Permissions;
          }
        }
        catch(...)
        {
          // ignore permissions errors with FTP
        }

        const wchar_t * Space = wcschr(Entry->OwnerGroup, L' ');
        if (Space != NULL)
        {
          File->Owner.Name = UnicodeString(Entry->OwnerGroup, Space - Entry->OwnerGroup);
          File->Group.Name = Space + 1;
        }
        else
        {
          File->Owner.Name = Entry->OwnerGroup;
        }

        File->Size = Entry->Size;

        if (Entry->Link)
        {
          File->Type = FILETYPE_SYMLINK;
        }
        else if (Entry->Dir)
        {
          File->Type = FILETYPE_DIRECTORY;
        }
        else
        {
          File->Type = L'-';
        }

        TDateTime Modification;
        TModificationFmt ModificationFmt;
        RemoteFileTimeToDateTimeAndPrecision(Entry->Time, Modification, ModificationFmt);
        File->Modification = Modification;
        File->ModificationFmt = ModificationFmt;
        File->LastAccess = File->Modification;

        File->LinkTo = Entry->LinkTarget;

        File->Complete();
      }
      catch (Exception & E)
      {
        delete File;
        UnicodeString EntryData =
          FORMAT(L"%s/%s/%s/%s/%d/%d/%d/%d/%d/%d/%d/%d/%d/%d",
            (Entry->Name, Entry->Permissions, Entry->OwnerGroup, IntToStr(Entry->Size),
             int(Entry->Dir), int(Entry->Link), Entry->Time.Year, Entry->Time.Month, Entry->Time.Day,
             Entry->Time.Hour, Entry->Time.Minute, int(Entry->Time.HasTime),
             int(Entry->Time.HasSeconds), int(Entry->Time.HasDate)));
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (EntryData)), HELP_LIST_LINE_ERROR);
      }

      FFileList->AddFile(File);
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, int /*Percent*/, int /*TimeElapsed*/, int /*TimeLeft*/, int /*TransferRate*/,
  bool FileTransfer)
{
  if (!FActive)
  {
    return false;
  }
  else if (!Valid)
  {
  }
  else if (FileTransfer)
  {
    FileTransferProgress(TransferSize, Bytes);
  }
  else
  {
    ReadDirectoryProgress(Bytes);
  }
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleReply(int Command, unsigned int Reply)
{
  if (!FActive)
  {
    return false;
  }
  else
  {
    if (FTerminal->Configuration->ActualLogProtocol >= 1)
    {
      FTerminal->LogEvent(FORMAT(L"Got reply %x to the command %d", (int(Reply), Command)));
    }

    // reply with Command 0 is not associated with current operation
    // so do not treat is as a reply
    // (it is typically used asynchronously to notify about disconnects)
    if (Command != 0)
    {
      assert(FCommandReply == 0);
      FCommandReply = Reply;
    }
    else
    {
      assert(FReply == 0);
      FReply = Reply;
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleCapabilities(
  TFTPServerCapabilities * ServerCapabilities)
{
  FServerCapabilities->Assign(ServerCapabilities);
  FFileSystemInfoValid = false;
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::CheckError(int ReturnCode, const wchar_t * Context)
{
  // we do not expect any FZAPI call to fail as it generally can fail only due to:
  // - invalid parameters
  // - busy FZAPI core
  // the only exception is REPLY_NOTCONNECTED that can happen if
  // connection is closed just between the last call to Idle()
  // and call to any FZAPI command
  // in such case reply without associated command is posted,
  // which we are going to wait for unless we are already waiting
  // on higher level (this typically happens if connection is lost while
  // waiting for user interaction and is detected within call to
  // SetAsyncRequestResult)
  if (FLAGSET(ReturnCode, TFileZillaIntf::REPLY_NOTCONNECTED))
  {
    if (!FWaitingForReply)
    {
      // throws
      WaitForFatalNonCommandReply();
    }
  }
  else
  {
    FTerminal->FatalError(NULL,
      FMTLOAD(INTERNAL_ERROR, (FORMAT(L"fz#%s", (Context)), IntToHex(ReturnCode, 4))));
    assert(false);
  }

  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::Unquote(UnicodeString & Str)
{
  enum
  {
    INIT,
    QUOTE,
    QUOTED,
    DONE
  } State;

  State = INIT;
  assert((Str.Length() > 0) && ((Str[1] == L'"') || (Str[1] == L'\'')));

  int Index = 1;
  wchar_t Quote;
  while (Index <= Str.Length())
  {
    switch (State)
    {
      case INIT:
        if ((Str[Index] == L'"') || (Str[Index] == L'\''))
        {
          Quote = Str[Index];
          State = QUOTED;
          Str.Delete(Index, 1);
        }
        else
        {
          assert(false);
          // no quoted string
          Str.SetLength(0);
        }
        break;

      case QUOTED:
        if (Str[Index] == Quote)
        {
          State = QUOTE;
          Str.Delete(Index, 1);
        }
        else
        {
          Index++;
        }
        break;

      case QUOTE:
        if (Str[Index] == Quote)
        {
          Index++;
        }
        else
        {
          // end of quoted string, trim the rest
          Str.SetLength(Index - 1);
          State = DONE;
        }
        break;
    }
  }

  return (State == DONE);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::PreserveDownloadFileTime(HANDLE Handle, void * UserData)
{
  TFileTransferData * Data = static_cast<TFileTransferData *>(UserData);
  FILETIME WrTime = DateTimeToFileTime(Data->Modification, dstmUnix);
  SetFileTime(Handle, NULL, NULL, &WrTime);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time)
{
  bool Result;
  try
  {
    // error-handling-free and DST-mode-inaware copy of TTerminal::OpenLocalFile
    HANDLE Handle = CreateFile(FileName, GENERIC_READ,
      FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);
    if (Handle == INVALID_HANDLE_VALUE)
    {
      Result = false;
    }
    else
    {
      FILETIME MTime;
      if (!GetFileTime(Handle, NULL, NULL, &MTime))
      {
        Result = false;
      }
      else
      {
        TDateTime Modification = ConvertTimestampToUTC(FileTimeToDateTime(MTime));

        unsigned short Year;
        unsigned short Month;
        unsigned short Day;
        Modification.DecodeDate(&Year, &Month, &Day);
        Time.tm_year = Year - 1900;
        Time.tm_mon = Month - 1;
        Time.tm_mday = Day;

        unsigned short Hour;
        unsigned short Min;
        unsigned short Sec;
        unsigned short MSec;
        Modification.DecodeTime(&Hour, &Min, &Sec, &MSec);
        Time.tm_hour = Hour;
        Time.tm_min = Min;
        Time.tm_sec = Sec;

        Result = true;
      }

      CloseHandle(Handle);
    }
  }
  catch (...)
  {
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
#endif NO_FILEZILLA
