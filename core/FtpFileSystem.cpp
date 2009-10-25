//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#ifndef NO_FILEZILLA
//---------------------------------------------------------------------------
#include <list>
#include "FtpFileSystem.h"
#include "FileZillaIntf.h"

#include "Common.h"
#include "Exceptions.h"
#include "Terminal.h"
#include "TextsCore.h"
#include "TextsFileZilla.h"
#include "HelpCore.h"
#include <openssl/x509_vfy.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION)
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

  virtual const char * __fastcall Option(int OptionID) const;
  virtual int __fastcall OptionVal(int OptionID) const;

protected:
  virtual bool __fastcall DoPostMessage(TMessageType Type, WPARAM wParam, LPARAM lParam);

  virtual bool __fastcall HandleStatus(const char * Status, int Type);
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    char * FileName1, size_t FileName1Len, const char * FileName2,
    const char * Path1, const char * Path2,
    __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
    bool HasTime1, bool HasTime2, void * UserData, int & RequestResult);
  virtual bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult);
  virtual bool __fastcall HandleListData(const char * Path, const TListDataEntry * Entries,
    unsigned int Count);
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer);
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply);
  virtual bool __fastcall HandleCapabilities(bool Mfmt);
  virtual bool __fastcall CheckError(int ReturnCode, const char * Context);

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
const char * __fastcall TFileZillaImpl::Option(int OptionID) const
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
bool __fastcall TFileZillaImpl::HandleStatus(const char * Status, int Type)
{
  return FFileSystem->HandleStatus(Status, Type);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestOverwrite(
  char * FileName1, size_t FileName1Len, const char * FileName2,
  const char * Path1, const char * Path2,
  __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
  bool HasTime1, bool HasTime2, void * UserData, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestOverwrite(
    FileName1, FileName1Len, FileName2, Path1, Path2, Size1, Size2, Time1, Time2,
    HasTime1, HasTime2, UserData, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleAsynchRequestVerifyCertificate(
  const TFtpsCertificateData & Data, int & RequestResult)
{
  return FFileSystem->HandleAsynchRequestVerifyCertificate(Data, RequestResult);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::HandleListData(const char * Path,
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
bool __fastcall TFileZillaImpl::HandleCapabilities(bool Mfmt)
{
  return FFileSystem->HandleCapabilities(Mfmt);
}
//---------------------------------------------------------------------------
bool __fastcall TFileZillaImpl::CheckError(int ReturnCode, const char * Context)
{
  return FFileSystem->CheckError(ReturnCode, Context);
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

  AnsiString FileName;
  int Params;
  bool AutoResume;
  int OverwriteResult;
  const TCopyParamType * CopyParam;
};
//---------------------------------------------------------------------------
const int tfFirstLevel = 0x01;
const int tfAutoResume = 0x02;
const char CertificateStorageKey[] = "FtpsCertificates";
//---------------------------------------------------------------------------
struct TSinkFileParams
{
  AnsiString TargetDir;
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
  FLastError(new TStringList()),
  FFeatures(new TStringList()),
  FFileList(NULL),
  FFileListCache(NULL),
  FActive(false),
  FWaitingForReply(false),
  FIgnoreFileList(false),
  FOnCaptureOutput(NULL),
  FFileSystemInfoValid(false),
  FDoListAll(false),
  FMfmt(false)
{
  ResetReply();

  FListAll = FTerminal->SessionData->FtpListAll;
  FFileSystemInfo.ProtocolBaseName = "FTP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  FTimeoutStatus = LoadStr(IDS_ERRORMSG_TIMEOUT);
  FDisconnectStatus = LoadStr(IDS_STATUSMSG_DISCONNECTED);
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
  delete FLastError;
  FLastError = NULL;
  delete FFeatures;
  FFeatures = NULL;

  ResetCaches();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Open()
{
  // on reconnect, typically there may be pending status messages from previous session
  DiscardMessages();

  TSessionData * Data = FTerminal->SessionData;

  FSessionInfo.LoginTime = Now();
  FSessionInfo.ProtocolBaseName = "FTP";
  FSessionInfo.ProtocolName = FSessionInfo.ProtocolBaseName;

  switch (Data->Ftps)
  {
    case ftpsImplicit:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_IMPLICIT);
      break;

    case ftpsExplicitSsl:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT_SSL);
      break;

    case ftpsExplicitTls:
      FSessionInfo.SecurityProtocolName = LoadStr(FTPS_EXPLICIT_TLS);
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
      switch (FTerminal->Configuration->LogProtocol)
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

  AnsiString HostName = Data->HostName;
  AnsiString UserName = Data->UserName;
  AnsiString Password = Data->Password;
  AnsiString Account = Data->FtpAccount;
  AnsiString Path = Data->RemoteDirectory;
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
  int TimeZoneOffset = int(double(Data->TimeDifference) * 24 * 60);
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
  };

  FPasswordFailed = false;
  bool PromptedForCredentials = false;

  do
  {
    FSystem = "";
    FFeatures->Clear();
    FFileSystemInfoValid = false;

    // TODO: the same for account? it ever used?

    // ask for username if it was not specified in advance, even on retry,
    // but keep previous one as default,
    if (Data->UserName.IsEmpty())
    {
      FTerminal->LogEvent("Username prompt (no username provided)");

      if (!FPasswordFailed && !PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT), false);
        PromptedForCredentials = true;
      }

      if (!FTerminal->PromptUser(Data, pkUserName, LoadStr(USERNAME_TITLE), "",
            LoadStr(USERNAME_PROMPT2), true, 0, UserName))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
      else
      {
        FUserName = UserName;
      }
    }

    // ask for password if it was not specified in advance,
    // on retry ask always
    if ((Data->Password.IsEmpty() && !Data->Passwordless) || FPasswordFailed)
    {
      FTerminal->LogEvent("Password prompt (no password provided or last login attempt failed)");

      if (!FPasswordFailed && !PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT), false);
        PromptedForCredentials = true;
      }

      // on retry ask for new password
      Password = "";
      if (!FTerminal->PromptUser(Data, pkPassword, LoadStr(PASSWORD_TITLE), "",
            LoadStr(PASSWORD_PROMPT), false, 0, Password))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
    }

    FActive = FFileZillaIntf->Connect(
      HostName.c_str(), Data->PortNumber, UserName.c_str(),
      Password.c_str(), Account.c_str(), false, Path.c_str(),
      ServerType, Pasv, TimeZoneOffset, UTF8, Data->FtpForcePasvIp);

    assert(FActive);

    FPasswordFailed = false;

    try
    {
      // do not wait for FTP response code as Connect is complex operation
      GotReply(WaitForCommandReply(false), REPLY_CONNECT, LoadStr(CONNECTION_FAILED));

      // we have passed, even if we got 530 on the way (if it is possible at all),
      // ignore it
      assert(!FPasswordFailed);
      FPasswordFailed = false;
    }
    catch(...)
    {
      if (FPasswordFailed)
      {
        FTerminal->Information(LoadStr(FTP_ACCESS_DENIED), false);
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
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::Close()
{
  assert(FActive);
  if (FFileZillaIntf->Close())
  {
    CHECK(FLAGSET(WaitForCommandReply(false), TFileZillaIntf::REPLY_DISCONNECTED));
    assert(FActive);
    Discard();
    FTerminal->Closed();
  }
  else
  {
    assert(false);
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
AnsiString __fastcall TFTPFileSystem::AbsolutePath(AnsiString Path, bool /*Local*/)
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
AnsiString __fastcall TFTPFileSystem::ActualCurrentDirectory()
{
  char CurrentPath[1024];
  FFileZillaIntf->GetCurrentPath(CurrentPath, sizeof(CurrentPath));
  return UnixExcludeTrailingBackslash(AnsiString(CurrentPath));
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
    // makes FAPI change its current directory (and not restoring it back afterwards)
    if (!UnixComparePaths(ActualCurrentDirectory(), FCurrentDirectory))
    {
      FTerminal->LogEvent(FORMAT("Synchronizing current directory \"%s\".",
        (FCurrentDirectory)));
      DoChangeDirectory(FCurrentDirectory);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::AnyCommand(const AnsiString Command,
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

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
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
void __fastcall TFTPFileSystem::DoChangeDirectory(const AnsiString & Directory)
{
  AnsiString Command = FORMAT("CWD %s", (Directory));
  FFileZillaIntf->CustomCommand(Command.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeDirectory(const AnsiString ADirectory)
{
  AnsiString Directory = ADirectory;
  try
  {
    // For changing directory, we do not make paths absolute, instead we
    // delegate this to the server, hence we sychronize current working
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
  FCurrentDirectory = "";
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CachedChangeDirectory(const AnsiString Directory)
{
  FCurrentDirectory = UnixExcludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeFileProperties(const AnsiString AFileName,
  const TRemoteFile * File, const TRemoteProperties * Properties,
  TChmodSessionAction & Action)
{
  assert(!Properties->Valid.Contains(vpGroup));
  assert(!Properties->Valid.Contains(vpOwner));
  assert(!Properties->Valid.Contains(vpLastAccess));
  assert(!Properties->Valid.Contains(vpModification));

  if (Properties->Valid.Contains(vpRights))
  {
    assert(Properties);

    TRemoteFile * OwnedFile = NULL;

    try
    {
      AnsiString FileName = AbsolutePath(AFileName, false);

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

      AnsiString FileNameOnly = UnixExtractFileName(FileName);
      AnsiString FilePath = UnixExtractFilePath(FileName);
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
void __fastcall TFTPFileSystem::CalculateFilesChecksum(const AnsiString & /*Alg*/,
  TStrings * /*FileList*/, TStrings * /*Checksums*/,
  TCalculatedChecksumEvent /*OnCalculatedChecksum*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::ConfirmOverwrite(AnsiString & FileName,
  TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
  const TOverwriteFileParams * FileParams, int Params, bool AutoResume)
{
  bool Result;
  bool CanAutoResume = FLAGSET(Params, cpNoConfirmation) && AutoResume;
  // when resuming transfer after interrupted connection,
  // do nothing (dummy resume) when the files has the same size.
  // this is workaround for servers that strangely fails just after successful
  // upload.
  bool CanResume =
    (FileParams != NULL) &&
    (((FileParams->DestSize < FileParams->SourceSize)) ||
     ((FileParams->DestSize == FileParams->SourceSize) && CanAutoResume));

  int Answer;
  if (CanAutoResume && CanResume)
  {
    Answer = qaRetry;
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
    TQueryButtonAlias Aliases[3];
    Aliases[0].Button = qaRetry;
    Aliases[0].Alias = LoadStr(RESUME_BUTTON);
    Aliases[1].Button = qaAll;
    Aliases[1].Alias = LoadStr(YES_TO_NEWER_BUTTON);
    Aliases[2].Button = qaIgnore;
    Aliases[2].Alias = LoadStr(RENAME_BUTTON);
    TQueryParams QueryParams(qpNeverAskAgainCheck);
    QueryParams.Aliases = Aliases;
    QueryParams.AliasesCount = LENOF(Aliases);
    SUSPEND_OPERATION
    (
      Answer = FTerminal->ConfirmFileOverwrite(FileName, FileParams,
        Answers, &QueryParams,
        OperationProgress->Side == osLocal ? osRemote : osLocal,
        Params, OperationProgress);
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
      if (FTerminal->PromptUser(FTerminal->SessionData, pkPrompt,
            LoadStr(RENAME_TITLE), "", LoadStr(RENAME_PROMPT2), true, 0, FileName))
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
  assert(Diff >= 0);
  if (Diff >= 0)
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
void __fastcall TFTPFileSystem::FileTransfer(const AnsiString & FileName,
  const AnsiString & LocalFile, const AnsiString & RemoteFile,
  const AnsiString & RemotePath, bool Get, __int64 Size, int Type,
  TFileTransferData & UserData, TFileOperationProgressType * OperationProgress)
{
  FILE_OPERATION_LOOP(FMTLOAD(TRANSFER_ERROR, (FileName)),
    FFileZillaIntf->FileTransfer(LocalFile.c_str(), RemoteFile.c_str(),
      RemotePath.c_str(), Get, Size, Type, &UserData);
    // we may actually catch reponse code of the listing
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
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  Params &= ~cpAppend;
  AnsiString FullTargetDir = IncludeTrailingBackslash(TargetDir);

  int Index = 0;
  while (Index < FilesToCopy->Count && !OperationProgress->Cancel)
  {
    AnsiString FileName = FilesToCopy->Strings[Index];
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
void __fastcall TFTPFileSystem::SinkRobust(const AnsiString FileName,
  const TRemoteFile * File, const AnsiString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem
  bool Retry;

  TDownloadSessionAction Action(FTerminal->Log);

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
void __fastcall TFTPFileSystem::Sink(const AnsiString FileName,
  const TRemoteFile * File, const AnsiString TargetDir,
  const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TDownloadSessionAction & Action)
{
  AnsiString OnlyFileName = UnixExtractFileName(FileName);

  Action.FileName(FileName);

  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Size;

  if (FLAGCLEAR(Params, cpDelete) &&
      !CopyParam->AllowTransfer(FileName, osRemote, File->IsDirectory, MaskParams))
  {
    FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  assert(File);
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  OperationProgress->SetFile(OnlyFileName);

  AnsiString DestFileName = CopyParam->ChangeFileName(OnlyFileName,
    osRemote, FLAGSET(Flags, tfFirstLevel));
  AnsiString DestFullName = TargetDir + DestFileName;

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
        if (!ForceDirectories(DestFullName))
        {
          RaiseLastOSError();
        }
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
    FTerminal->LogEvent(FORMAT("Copying \"%s\" to local directory started.", (FileName)));

    // Will we use ASCII of BINARY file tranfer?
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osRemote, MaskParams));
    FTerminal->LogEvent(AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
      " transfer mode selected.");

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

    AnsiString FilePath = UnixExtractFilePath(FileName);
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
void __fastcall TFTPFileSystem::SinkFile(AnsiString FileName,
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
  const AnsiString ATargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  TOnceDoneOperation & OnceDoneOperation)
{
  assert((FilesToCopy != NULL) && (OperationProgress != NULL));

  Params &= ~cpAppend;
  AnsiString FileName, FileNameOnly;
  AnsiString TargetDir = AbsolutePath(ATargetDir, false);
  AnsiString FullTargetDir = UnixIncludeTrailingBackslash(TargetDir);
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
void __fastcall TFTPFileSystem::SourceRobust(const AnsiString FileName,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  // the same in TSFTPFileSystem
  bool Retry;

  TUploadSessionAction Action(FTerminal->Log);

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
void __fastcall TFTPFileSystem::Source(const AnsiString FileName,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
  TFileOperationProgressType * OperationProgress, unsigned int Flags,
  TUploadSessionAction & Action)
{
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  Action.FileName(ExpandUNCFileName(FileName));

  OperationProgress->SetFile(FileName, false);

  __int64 Size;
  int Attrs;

  FTerminal->OpenLocalFile(FileName, GENERIC_READ, &Attrs,
    NULL, NULL, NULL, NULL, &Size);

  bool Dir = FLAGSET(Attrs, faDirectory);

  TFileMasks::TParams MaskParams;
  MaskParams.Size = Size;

  if (FLAGCLEAR(Params, cpDelete) &&
      !CopyParam->AllowTransfer(FileName, osLocal, Dir, MaskParams))
  {
    FTerminal->LogEvent(FORMAT("File \"%s\" excluded from transfer", (FileName)));
    THROW_SKIP_FILE_NULL;
  }

  OperationProgress->SetFileInProgress();

  if (Dir)
  {
    DirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
      Attrs, CopyParam, Params, OperationProgress, Flags);
    Action.Cancel();
  }
  else
  {
    AnsiString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName),
      osLocal, FLAGSET(Flags, tfFirstLevel));

    FTerminal->LogEvent(FORMAT("Copying \"%s\" to remote directory started.", (FileName)));

    OperationProgress->SetLocalSize(Size);

    // Suppose same data size to transfer as to read
    // (not true with ASCII transfer)
    OperationProgress->SetTransferSize(OperationProgress->LocalSize);
    OperationProgress->TransferingFile = false;

    // Will we use ASCII of BINARY file tranfer?
    OperationProgress->SetAsciiTransfer(
      CopyParam->UseAsciiTransfer(FileName, osLocal, MaskParams));
    FTerminal->LogEvent(
      AnsiString((OperationProgress->AsciiTransfer ? "Ascii" : "Binary")) +
        " transfer mode selected.");

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
      FileTransfer(FileName, FileName, DestFileName,
        TargetDir, false, Size, TransferType, UserData, OperationProgress);
    }

    AnsiString DestFullName = TargetDir + UserData.FileName;
    // only now, we know the final destination
    Action.Destination(DestFullName);

    // we are not able to tell if setting timestamp succeeded,
    // so we log it always (if supported)
    if (FFileTransferPreserveTime && FMfmt)
    {
      // Inspired by SysUtils::FileAge
      WIN32_FIND_DATA FindData;
      HANDLE Handle = FindFirstFile(DestFullName.c_str(), &FindData);
      if (Handle != INVALID_HANDLE_VALUE)
      {
        TTouchSessionAction TouchAction(FTerminal->Log, DestFullName,
          UnixToDateTime(
            ConvertTimestampToUnixSafe(FindData.ftLastWriteTime, dstmUnix),
            dstmUnix));
        FindClose(Handle);
      }
    }
  }

  /* TODO : Delete also read-only files. */
  if (FLAGSET(Params, cpDelete))
  {
    FILE_OPERATION_LOOP (FMTLOAD(DELETE_LOCAL_FILE_ERROR, (FileName)),
      THROWOSIFFALSE(Sysutils::DeleteFile(FileName));
    )
  }
  else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
  {
    FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
      THROWOSIFFALSE(FileSetAttr(FileName, Attrs & ~faArchive) == 0);
    )
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DirectorySource(const AnsiString DirectoryName,
  const AnsiString TargetDir, int Attrs, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  AnsiString DestDirectoryName = CopyParam->ChangeFileName(
    ExtractFileName(ExcludeTrailingBackslash(DirectoryName)), osLocal,
    FLAGSET(Flags, tfFirstLevel));
  AnsiString DestFullName = UnixIncludeTrailingBackslash(TargetDir + DestDirectoryName);

  OperationProgress->SetFile(DirectoryName);

  int FindAttrs = faReadOnly | faHidden | faSysFile | faDirectory | faArchive;
  TSearchRec SearchRec;
  bool FindOK;

  FILE_OPERATION_LOOP (FMTLOAD(LIST_DIR_ERROR, (DirectoryName)),
    FindOK = (bool)(FindFirst(DirectoryName + "*.*",
      FindAttrs, SearchRec) == 0);
  );

  bool CreateDir = true;

  try
  {
    while (FindOK && !OperationProgress->Cancel)
    {
      AnsiString FileName = DirectoryName + SearchRec.Name;
      try
      {
        if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
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
        FindOK = (FindNext(SearchRec) == 0);
      );
    };
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
void __fastcall TFTPFileSystem::CreateDirectory(const AnsiString ADirName)
{
  AnsiString DirName = AbsolutePath(ADirName, false);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->MakeDir(DirName.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateLink(const AnsiString /*FileName*/,
  const AnsiString /*PointTo*/, bool /*Symbolic*/)
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DeleteFile(const AnsiString AFileName,
  const TRemoteFile * File, int Params, TRmSessionAction & Action)
{
  AnsiString FileName = AbsolutePath(AFileName, false);
  AnsiString FileNameOnly = UnixExtractFileName(FileName);
  AnsiString FilePath = UnixExtractFilePath(FileName);

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
      // Is current remote directory is in the directory being removed,
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
void __fastcall TFTPFileSystem::CustomCommandOnFile(const AnsiString /*FileName*/,
  const TRemoteFile * /*File*/, AnsiString /*Command*/, int /*Params*/,
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
      AnsiString Command = PostLoginCommands->Strings[Index];
      if (!Command.IsEmpty())
      {
        FFileZillaIntf->CustomCommand(Command.c_str());

        GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
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
      return FMfmt;

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
  // and immediatelly after call to CWD,
  // later our current directory may be not synchronized with FZAPI current
  // directory anyway, see comments in EnsureLocation
  if (FCurrentDirectory.IsEmpty())
  {
    FFileZillaIntf->CustomCommand("PWD");

    unsigned int Code;
    TStrings * Response = NULL;
    GotReply(WaitForCommandReply(), REPLY_2XX_CODE, "", &Code, &Response);

    try
    {
      assert(Response != NULL);
      bool Result = false;

      // the only allowed 2XX code to "PWD"
      if ((Code == 257) &&
          (Response->Count == 1))
      {
        AnsiString Path = Response->Text;

        int P = Path.Pos("\"");
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
  FileList->Clear();
  // FZAPI does not list parent directory, add it
  FileList->AddFile(new TRemoteParentDirectory(FTerminal));

  FLastReadDirectoryProgress = 0;

  TFileListHelper Helper(this, FileList, false);

  // always specify path to list, do not attempt to list "current" dir as:
  // 1) List() lists again the last listed directory, not the current working directory
  // 2) we handle this way the cached directory change
  AnsiString Directory = AbsolutePath(FileList->Directory, false);
  FFileZillaIntf->List(Directory.c_str());

  GotReply(WaitForCommandReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);

  FLastDataSent = Now();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  bool Repeat;

  do
  {
    Repeat = false;
    try
    {
      FDoListAll = (FListAll == asAuto) || (FListAll == asOn);
      DoReadDirectory(FileList);

      if (FListAll == asAuto)
      {
        // reading first directory has succeeded, always use "-a"
        FListAll = asOn;
      }
      // use "-a" even for implicit directory reading by FZAPI?
      // (e.g. before file transfer)
      FDoListAll = (FListAll == asOn);
    }
    catch(...)
    {
      FDoListAll = false;
      // reading the first directory has failed,
      // further try without "-a" only as the server may not support it
      if ((FListAll == asAuto) && FTerminal->Active)
      {
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
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadFile(const AnsiString FileName,
  TRemoteFile *& File)
{
  AnsiString Path = UnixExtractFilePath(FileName);
  AnsiString NameOnly = UnixExtractFileName(FileName);

  // FZAPI does not have efficient way to read properties of one file.
  // If case we need properties of set of files from the same directory,
  // cache the file list for future
  TRemoteFile * AFile = NULL;
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
    delete FFileListCache;
    FFileListCache = NULL;
    FFileListCache = new TRemoteFileList();
    FFileListCache->Directory = Path;
    ReadDirectory(FFileListCache);
    FFileListCachePath = CurrentDirectory;

    AFile = FFileListCache->FindFile(NameOnly);
    if (AFile == NULL)
    {
      File = NULL;
      throw Exception(FMTLOAD(FILE_NOT_EXISTS, (FileName)));
    }
  }

  assert(AFile != NULL);
  File = AFile->Duplicate();
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ReadSymlink(TRemoteFile * SymlinkFile,
  TRemoteFile *& File)
{
  // Resolving symlinks over FTP is big overhead
  // (involves opening TCPIP connection for retrieving "directory listing").
  // Moreover FZAPI does not support that anyway.
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
void __fastcall TFTPFileSystem::RenameFile(const AnsiString AFileName,
  const AnsiString ANewName)
{
  AnsiString FileName = AbsolutePath(AFileName, false);
  AnsiString NewName = AbsolutePath(ANewName, false);

  AnsiString FileNameOnly = UnixExtractFileName(FileName);
  AnsiString FilePathOnly = UnixExtractFilePath(FileName);
  AnsiString NewNameOnly = UnixExtractFileName(NewName);
  AnsiString NewPathOnly = UnixExtractFilePath(NewName);

  {
    // ignore file list
    TFileListHelper Helper(this, NULL, true);

    FFileZillaIntf->Rename(FileNameOnly.c_str(), NewNameOnly.c_str(),
      FilePathOnly.c_str(), NewPathOnly.c_str());

    GotReply(WaitForCommandReply(), REPLY_2XX_CODE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyFile(const AnsiString FileName,
  const AnsiString NewName)
{
  assert(false);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFTPFileSystem::FileUrl(const AnsiString FileName)
{
  return FTerminal->FileUrl("ftp", FileName);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TFTPFileSystem::GetFixedPaths()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::SpaceAvailable(const AnsiString /*Path*/,
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
        FORMAT("%s\r\n", (LoadStr(FTP_FEATURE_INFO)));
      for (int Index = 0; Index < FFeatures->Count; Index++)
      {
        FFileSystemInfo.AdditionalInfo += FORMAT("  %s\r\n", (FFeatures->Strings[Index]));
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
bool __fastcall TFTPFileSystem::TemporaryTransferFile(const AnsiString & /*FileName*/)
{
  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::GetStoredCredentialsTried()
{
  return !FTerminal->SessionData->Password.IsEmpty();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFTPFileSystem::GetUserName()
{
  return FUserName;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFTPFileSystem::GetCurrentDirectory()
{
  return FCurrentDirectory;
}
//---------------------------------------------------------------------------
const char * __fastcall TFTPFileSystem::GetOption(int OptionID) const
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

    case OPTION_ANONPWD:
    case OPTION_TRANSFERIP:
    case OPTION_TRANSFERIP6:
      FOptionScratch = "";
      break;

    default:
      assert(false);
      FOptionScratch = "";
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
        case pmNone:
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
      // Listing is logged on FZAPI level 5 (what is strangely LOG_APIERROR)
      Result = (FTerminal->Configuration->LogProtocol >= 1);
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
      // now we are perfecly sure that the queue is empty as it is locked,
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
    FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, ("ftp#1", IntToStr(Result))));
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
  assert(!FTransferStatusCriticalSection->Acquired);

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
  AnsiString Error, unsigned int * Code, TStrings ** Response)
{
  try
  {
    if (FLAGSET(Reply, TFileZillaIntf::REPLY_OK))
    {
      assert(Reply == TFileZillaIntf::REPLY_OK);

      // With REPLY_2XX_CODE treat "OK" non-2xx code like an error
      if (FLAGSET(Flags, REPLY_2XX_CODE) && (FLastCodeClass != 2))
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
      FTerminal->FatalError(NULL, FMTLOAD(INTERNAL_ERROR, ("ftp#2", FORMAT("0x%x", (int(Reply))))));
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
            MoreMessages->Add(LoadStr(FTP_CANNOT_OPEN_ACTIVE_CONNECTION));
          }
        }

        MoreMessages->AddStrings(FLastError);
        // already cleared from WaitForReply, but GotReply can be also called
        // from Closed. then make sure that error from previous command not
        // associated with session closure is not reused
        FLastError->Clear();

        MoreMessages->AddStrings(FLastResponse);
        // see comment for FLastError
        FLastResponse->Clear();

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
        ExtException * E = new ExtException(Error, MoreMessages, true);
        try
        {
          FTerminal->FatalError(E, "");
        }
        __finally
        {
          delete E;
        }
      }
      else
      {
        throw ExtException(Error, MoreMessages, true);
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
void __fastcall TFTPFileSystem::HandleReplyStatus(AnsiString Response)
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
    ((Response.Length() == 3) || (Response[4] == ' ') || (Response[4] == '-'));

  if (HasCodePrefix && !FMultineResponse)
  {
    FMultineResponse = (Response.Length() >= 4) && (Response[4] == '-');
    FLastResponse->Clear();
    if (Response.Length() >= 5)
    {
      FLastResponse->Add(Response.SubString(5, Response.Length() - 4));
    }
    SetLastCode(Code);
  }
  else
  {
    int Start;
    // response with code prefix
    if (HasCodePrefix && (FLastCode == Code))
    {
      // End of multiline response?
      if ((Response.Length() <= 3) || (Response[4] == ' '))
      {
        FMultineResponse = false;
      }
      Start = 5;
    }
    else
    {
      Start = (((Response.Length() >= 1) && (Response[1] == ' ')) ? 2 : 1);
    }

    // Intermediate empty lines are being added
    if (FMultineResponse || (Response.Length() >= Start))
    {
      FLastResponse->Add(Response.SubString(Start, Response.Length() - Start + 1));
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
      };
    }
    else if (FLastCommand == SYST)
    {
      assert(FSystem.IsEmpty());
      // Possitive reply to "SYST" must be 215, see RFC 959
      if (FLastCode == 215)
      {
        FSystem = FLastResponse->Text.TrimRight();
        // full name is "Personal FTP Server PRO K6.0"
        if ((FListAll == asAuto) &&
            (FSystem.Pos("Personal FTP Server") > 0))
        {
          FTerminal->LogEvent("Server is known not to support LIST -a");
          FListAll = asOff;
        }
      }
      else
      {
        FSystem = "";
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
AnsiString __fastcall TFTPFileSystem::ExtractStatusMessage(AnsiString Status)
{
  // CApiLog::LogMessage
  // (note that the formatting may not be present when LogMessageRaw is used)
  int P1 = Status.Pos("): ");
  if (P1 > 0)
  {
    int P2 = Status.Pos(".cpp(");
    if ((P2 > 0) && (P2 < P1))
    {
      int P3 = Status.Pos("   caller=0x");
      if ((P3 > 0) && (P3 > P1))
      {
        Status = Status.SubString(P1 + 3, P3 - P1 - 3);
      }
    }
  }
  return Status;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleStatus(const char * AStatus, int Type)
{
  TLogLineType LogType = (TLogLineType)-1;
  AnsiString Status(AStatus);

  switch (Type)
  {
    case TFileZillaIntf::LOG_STATUS:
      FTerminal->Information(Status, true);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_COMMAND:
      if (Status == "SYST")
      {
        FLastCommand = SYST;
      }
      else if (Status == "FEAT")
      {
        FLastCommand = FEAT;
      }
      else if (Status.SubString(1, 5) == "PASS ")
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
      // (such as "cannot open local file..." followed by "download failed"
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
TDateTime __fastcall TFTPFileSystem::ConvertRemoteTimestamp(time_t Time, bool HasTime)
{
  TDateTime Result;
  tm * Tm = localtime(&Time);
  if (Tm != NULL)
  {
    // should be the same as HandleListData
    Result = EncodeDate(
      static_cast<unsigned short>(Tm->tm_year + 1900),
      static_cast<unsigned short>(Tm->tm_mon + 1),
      static_cast<unsigned short>(Tm->tm_mday));
    if (HasTime)
    {
      Result += EncodeTime(
        static_cast<unsigned short>(Tm->tm_hour),
        static_cast<unsigned short>(Tm->tm_min),
        static_cast<unsigned short>(Tm->tm_sec), 0);
    }
  }
  else
  {
    // incorrect, but at least something
    Result = UnixToDateTime(Time, dstmUnix);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleAsynchRequestOverwrite(
  char * FileName1, size_t FileName1Len, const char * /*FileName2*/,
  const char * /*Path1*/, const char * /*Path2*/,
  __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
  bool HasTime1, bool HasTime2, void * AUserData, int & RequestResult)
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
      AnsiString FileName = FileName1;
      assert(UserData.FileName == FileName);
      TOverwriteMode OverwriteMode = omOverwrite;
      TOverwriteFileParams FileParams;
      bool NoFileParams =
        (Size1 < 0) || (Time1 == 0) ||
        (Size2 < 0) || (Time2 == 0);
      if (!NoFileParams)
      {
        FileParams.SourceSize = Size2;
        FileParams.DestSize = Size1;

        if (OperationProgress->Side == osLocal)
        {
          FileParams.SourceTimestamp = ConvertLocalTimestamp(Time2);
          FileParams.DestTimestamp = ConvertRemoteTimestamp(Time1, HasTime1);
          FileParams.DestPrecision = (HasTime1 ? mfMDHM : mfMDY);
        }
        else
        {
          FileParams.SourceTimestamp = ConvertRemoteTimestamp(Time2, HasTime2);
          FileParams.SourcePrecision = (HasTime2 ? mfMDHM : mfMDY);
          FileParams.DestTimestamp = ConvertLocalTimestamp(Time1);
        }
      }

      if (ConfirmOverwrite(FileName, OverwriteMode, OperationProgress,
            (NoFileParams ? NULL : &FileParams), UserData.Params,
            UserData.AutoResume && UserData.CopyParam->AllowResume(FileParams.SourceSize)))
      {
        switch (OverwriteMode)
        {
          case omOverwrite:
            if (FileName != FileName1)
            {
              strncpy(FileName1, FileName.c_str(), FileName1Len);
              FileName1[FileName1Len - 1] = '\0';
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
      // when user chosses not to overwrite, break loop waiting for response code
      // by setting dummy one, az FZAPI won't do anything then
      SetLastCode(DummyTimeoutCode);
    }

    return true;
  }
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
AnsiString __fastcall FormatContactList(AnsiString Entry1, AnsiString Entry2)
{
  if (!Entry1.IsEmpty() && !Entry2.IsEmpty())
  {
    return FORMAT("%s, %s", (Entry1, Entry2));
  }
  else
  {
    return Entry1 + Entry2;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall FormatContact(const TFtpsCertificateData::TContact & Contact)
{
  AnsiString Result =
    FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 1),
      (FormatContactList(FormatContactList(FormatContactList(
        Contact.Organization, Contact.Unit), Contact.CommonName), Contact.Mail)));

  if ((strlen(Contact.Country) > 0) ||
      (strlen(Contact.StateProvince) > 0) ||
      (strlen(Contact.Town) > 0))
  {
    Result +=
      FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 2),
        (FormatContactList(FormatContactList(
          Contact.Country, Contact.StateProvince), Contact.Town)));
  }

  if (strlen(Contact.Other) > 0)
  {
    Result += FORMAT(LoadStrPart(VERIFY_CERT_CONTACT, 3), (Contact.Other));
  }

  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall FormatValidityTime(const TFtpsCertificateData::TValidityTime & ValidityTime)
{
  return FormatDateTime("ddddd tt",
    EncodeDate(
      (unsigned short)ValidityTime.Year, (unsigned short)ValidityTime.Month,
      (unsigned short)ValidityTime.Day) +
    EncodeTime(
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
      StrToHex(AnsiString((const char*)Data.Hash, Data.HashLen), false, ':');

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

    AnsiString Summary = LoadStr(VerificationResultStr);
    if (Data.VerificationResult != X509_V_OK)
    {
      Summary += " " + FMTLOAD(CERT_ERRDEPTH, (Data.VerificationDepth + 1));
    }

    FSessionInfo.Certificate =
      FMTLOAD(CERT_TEXT, (
        FormatContact(Data.Subject),
        FormatContact(Data.Issuer),
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
      AnsiString Buf = FTerminal->SessionData->HostKey;
      while ((RequestResult == 0) && !Buf.IsEmpty())
      {
        AnsiString ExpectedKey = CutToChar(Buf, ';', false);
        if (ExpectedKey == FSessionInfo.CertificateFingerprint)
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
      int Answer = FTerminal->QueryUser(
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
            Storage->WriteString(FSessionInfo.CertificateFingerprint, "");
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
bool __fastcall TFTPFileSystem::HandleListData(const char * Path,
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
        if (strlen(Entry->Permissions) >= 10)
        {
          try
          {
            File->Rights->Text = Entry->Permissions + 1;
          }
          catch(...)
          {
            // ignore permissions errors with FTP
          }
        }

        const char * Space = strchr(Entry->OwnerGroup, ' ');
        if (Space != NULL)
        {
          File->Owner.Name = AnsiString(Entry->OwnerGroup, Space - Entry->OwnerGroup);
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
          File->Type = '-';
        }

        // ModificationFmt must be set after Modification
        if (Entry->HasDate)
        {
          // should be the same as ConvertRemoteTimestamp
          TDateTime Modification =
            EncodeDate((unsigned short)Entry->Year, (unsigned short)Entry->Month,
              (unsigned short)Entry->Day);
          if (Entry->HasTime)
          {
            File->Modification = Modification +
              EncodeTime((unsigned short)Entry->Hour, (unsigned short)Entry->Minute, 0, 0);
            // not exact as we got year as well, but it is most probably
            // guessed by FZAPI anyway
            File->ModificationFmt = mfMDHM;
          }
          else
          {
            File->Modification = Modification;
            File->ModificationFmt = mfMDY;
          }
        }
        else
        {
          // With SCP we estimate date to be today, if we have at least time

          File->Modification = double(0);
          File->ModificationFmt = mfNone;
        }
        File->LastAccess = File->Modification;

        File->LinkTo = Entry->LinkTarget;

        File->Complete();
      }
      catch (Exception & E)
      {
        delete File;
        AnsiString EntryData =
          FORMAT("%s/%s/%s/%s/%d/%d/%d/%d/%d/%d/%d/%d/%d",
            (Entry->Name, Entry->Permissions, Entry->OwnerGroup, IntToStr(Entry->Size),
             int(Entry->Dir), int(Entry->Link), Entry->Year, Entry->Month, Entry->Day,
             Entry->Hour, Entry->Minute, int(Entry->HasTime), int(Entry->HasDate)));
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (EntryData)));
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
    if (FTerminal->Configuration->LogProtocol >= 1)
    {
      FTerminal->LogEvent(FORMAT("Got reply %x to the command %d", (int(Reply), Command)));
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
bool __fastcall TFTPFileSystem::HandleCapabilities(bool Mfmt)
{
  FMfmt = Mfmt;
  FFileSystemInfoValid = false;
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::CheckError(int ReturnCode, const char * Context)
{
  // we do not expect any FZAPI call to fail as it generally can fail only due to:
  // - invalid paramerers
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
      FMTLOAD(INTERNAL_ERROR, (FORMAT("fz#%s", (Context)), IntToHex(ReturnCode, 4))));
    assert(false);
  }

  return false;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::Unquote(AnsiString & Str)
{
  enum
  {
    INIT,
    QUOTE,
    QUOTED,
    DONE
  } State;

  State = INIT;
  assert((Str.Length() > 0) && (Str[1] == '"'));

  int Index = 1;
  while (Index <= Str.Length())
  {
    switch (State)
    {
      case INIT:
        switch (Str[Index])
        {
          case '"':
            State = QUOTED;
            Str.Delete(Index, 1);
            break;

          default:
            assert(false);
            // no quoted string
            Str.SetLength(0);
            break;
        }
        break;

      case QUOTED:
        switch (Str[Index])
        {
          case '"':
            State = QUOTE;
            Str.Delete(Index, 1);
            break;

          default:
            Index++;
            break;
        }
        break;

      case QUOTE:
        switch (Str[Index])
        {
          case '"':
            Index++;
            break;

          default:
            // end of quoted string, trim the rest
            Str.SetLength(Index - 1);
            State = DONE;
            break;
        }
        break;
    }
  }

  return (State == DONE);
}
//---------------------------------------------------------------------------
#endif NO_FILEZILLA
