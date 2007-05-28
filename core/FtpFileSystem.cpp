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
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define FILE_OPERATION_LOOP_EX(ALLOW_SKIP, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_CUSTOM(FTerminal, ALLOW_SKIP, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
class TFileZillaImpl : public TFileZillaIntf
{
public:
  __fastcall TFileZillaImpl(TFTPFileSystem * FileSystem);

  virtual const char * __fastcall Option(int OptionID) const;
  virtual int __fastcall OptionVal(int OptionID) const;

protected:
  virtual bool __fastcall DoPostMessage(WPARAM wParam, LPARAM lParam);

  virtual bool __fastcall HandleStatus(const char * Status, int Type);
  virtual bool __fastcall HandleAsynchRequestOverwrite(
    char * FileName1, size_t FileName1Len, const char * FileName2,
    const char * Path1, const char * Path2,
    __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
    bool HasTime1, bool HasTime2, void * UserData, int & RequestResult);
  virtual bool __fastcall HandleListData(const char * Path, const TListDataEntry * Entries,
    unsigned int Count);
  virtual bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer);
  virtual bool __fastcall HandleReply(int Command, unsigned int Reply);
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
bool __fastcall TFileZillaImpl::DoPostMessage(WPARAM wParam, LPARAM lParam)
{
  return FFileSystem->PostMessage(wParam, lParam);
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
  int Params;
  bool AutoResume;
};
//---------------------------------------------------------------------------
const int tfFirstLevel = 0x01;
const int tfAutoResume = 0x02;
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
__fastcall TFTPFileSystem::TFTPFileSystem(TTerminal * ATerminal):
  TCustomFileSystem(ATerminal),
  FFileZillaIntf(NULL),
  FQueueCriticalSection(new TCriticalSection),
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
  FFileSystemInfoValid(false)
{
  ResetReply();

  FFileSystemInfo.ProtocolBaseName = "FTP";
  FFileSystemInfo.ProtocolName = FFileSystemInfo.ProtocolBaseName;
  // capabilities of FTP protocol are fixed
  for (int Index = 0; Index < fcCount; Index++)
  {
    FFileSystemInfo.IsCapable[Index] = IsCapable((TFSCapability)Index);
  }
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

  FSessionInfo.LoginTime = Now();
  FSessionInfo.ProtocolBaseName = "FTP";
  FSessionInfo.ProtocolName = FSessionInfo.ProtocolBaseName;

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
          LogLevel = TFileZillaIntf::LOG_LIST;
          break;

        case 1:
          LogLevel = TFileZillaIntf::LOG_WARNING;
          break;

        case 2:
          LogLevel = TFileZillaIntf::LOG_DEBUG;
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

  TSessionData * Data = FTerminal->SessionData;
  AnsiString HostName = Data->HostName;
  AnsiString UserName = Data->UserName;
  AnsiString Password = Data->Password;
  AnsiString Account = Data->FtpAccount;
  AnsiString Path = Data->RemoteDirectory;
  int ServerType = 0x1000; // FZ_SERVERTYPE_FTP
  int Pasv = (Data->FtpPasvMode ? 1 : 2);
  int TimeZoneOffset = int(double(Data->TimeDifference) * 24 * 60);
  int UTF8 = 0;
  switch (Data->SFTPBug[sbUtf])
  {
    case asOn:
      UTF8 = 1;
      break;

    case asOff:
      UTF8 = 2;
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

    // !!! the same for account? it ever used?

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

      if (!FTerminal->PromptUser(Data,
            FMTLOAD(USERNAME_PROMPT, (Data->SessionName)),
            pkPrompt, UserName))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
      else
      {
        FUserName = UserName;
      }
    }

    // ask for username if it was not specified in advance,
    // on retry ask always
    if (Data->Password.IsEmpty() || FPasswordFailed)
    {
      FTerminal->LogEvent("Password prompt (no password provided or last login attempt failed)");

      if (!FPasswordFailed && !PromptedForCredentials)
      {
        FTerminal->Information(LoadStr(FTP_CREDENTIAL_PROMPT), false);
        PromptedForCredentials = true;
      }

      // on retry ask for new password
      Password = "";
      if (!FTerminal->PromptUser(Data,
            FMTLOAD(PROMPT_SESSION_PASSWORD, (Data->SessionName)),
            pkPassword, Password))
      {
        FTerminal->FatalError(NULL, LoadStr(AUTHENTICATION_FAILED));
      }
    }

    FActive = FFileZillaIntf->Connect(
      HostName.c_str(), Data->PortNumber, UserName.c_str(),
      Password.c_str(), Account.c_str(), false, Path.c_str(),
      ServerType, Pasv, TimeZoneOffset, UTF8);

    assert(FActive);

    FPasswordFailed = false;

    try
    {
      GotReply(WaitForReply(), REPLY_CONNECT, LoadStr(CONNECTION_FAILED));

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
    CHECK(FLAGSET(WaitForReply(), TFileZillaIntf::REPLY_DISCONNECTED));
    assert(FActive);
    Discard();
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
    unsigned int Reply = PoolForReply();
    if (Reply != 0)
    {
      assert(FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));
      GotReply(Reply);
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
  FTerminal->Closed();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFTPFileSystem::AbsolutePath(AnsiString Path)
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
    char CurrentPath[1024];
    FFileZillaIntf->GetCurrentPath(CurrentPath, sizeof(CurrentPath));
    if (!UnixComparePaths(CurrentPath, FCurrentDirectory))
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

    GotReply(WaitForReply(), REPLY_2XX_CODE);
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

  GotReply(WaitForReply(), REPLY_2XX_CODE);
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
      Directory = AbsolutePath(Directory);
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
  FCurrentDirectory = Directory;
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::ChangeFileProperties(const AnsiString AFileName,
  const TRemoteFile * File, const TRemoteProperties * Properties)
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
      AnsiString FileName = AbsolutePath(AFileName);

      if (File == NULL)
      {
        ReadFile(FileName, OwnedFile);
        File = OwnedFile;
      }

      if ((File != NULL) && File->IsDirectory && !File->IsSymLink && Properties->Recursive)
      {
        FTerminal->ProcessDirectory(AFileName, FTerminal->ChangeFileProperties,
          (void*)Properties);
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

      AnsiString FileNameOnly = UnixExtractFileName(FileName);
      AnsiString FilePath = UnixExtractFilePath(FileName);
      // FZAPI wants octal number represented as decadic
      FFileZillaIntf->Chmod(Rights.NumberDecadic, FileNameOnly.c_str(), FilePath.c_str());

      GotReply(WaitForReply(), REPLY_2XX_CODE);
    }
    __finally
    {
      delete OwnedFile;
    }
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
  if (OperationProgress->YesToAll)
  {
    OverwriteMode = omOverwrite;
    Result = true;
  }
  else if (OperationProgress->NoToAll)
  {
    FFileTransferAbort = ftaSkip;
    Result = false;
  }
  else
  {
    bool CanResume = (FileParams->DestSize < FileParams->SourceSize);

    int Answer;
    if (FLAGSET(Params, cpNoConfirmation))
    {
      Answer = (CanResume && AutoResume ? qaRetry : qaYes);
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
      TQueryParams Params(qpNeverAskAgainCheck);
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);
      SUSPEND_OPERATION
      (
        Answer = FTerminal->ConfirmFileOverwrite(FileName, FileParams,
          Answers, &Params,
          OperationProgress->Side == osLocal ? osRemote : osLocal,
          OperationProgress);
      )
    }

    Result = true;

    switch (Answer)
    {
      // resume
      case qaRetry:
        OverwriteMode = omResume;
        FFileTransferResumed = FileParams->DestSize;
        break;

      // rename
      case qaIgnore:
        if (FTerminal->PromptUser(FTerminal->SessionData, LoadStr(RENAME_PROMPT),
              pkPrompt, FileName))
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
void __fastcall TFTPFileSystem::CheckFileTransferAbort()
{
  switch (FFileTransferAbort)
  {
    case ftaSkip:
      THROW_SKIP_FILE_NULL;

    case ftaCancel:
      Abort();
      break;
  }
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
void __fastcall TFTPFileSystem::FileTransferProgress(__int64 TransferSize,
  __int64 Bytes, int /*Percent*/)
{
  TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;

  OperationProgress->SetTransferSize(TransferSize);

  if (FFileTransferResumed > 0)
  {
    OperationProgress->AddResumed(FFileTransferResumed);
    FFileTransferResumed = 0;
  }

  assert(OperationProgress->TransferedSize <= Bytes);
  OperationProgress->AddTransfered(Bytes - OperationProgress->TransferedSize);

  if (OperationProgress->Cancel == csCancel)
  {
    FFileTransferCancelled = true;
    FFileZillaIntf->Cancel();
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CopyToLocal(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam,
  int Params, TFileOperationProgressType * OperationProgress,
  bool & DisconnectWhenComplete)
{
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
        SinkRobust(AbsolutePath(FileName), File, FullTargetDir, CopyParam, Params,
          OperationProgress, tfFirstLevel);
        Success = true;
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
      OperationProgress->Finish(FileName, Success, DisconnectWhenComplete);
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
  // set in advance as particularly cpNoConfirmation may change
  int ReopenParams =
    ropNoReadDirectory |
    FLAGMASK(FLAGSET(Params, cpNoConfirmation), ropNoConfirmation);

  do
  {
    Retry = false;
    try
    {
      Sink(FileName, File, TargetDir, CopyParam, Params, OperationProgress, Flags);
    }
    catch(Exception & E)
    {
      Retry = true;
      if (FTerminal->Active ||
          !FTerminal->QueryReopen(&E, ReopenParams, OperationProgress))
      {
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
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
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  AnsiString OnlyFileName = UnixExtractFileName(FileName);

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
          EXCEPTION;
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

    AnsiString FilePath = UnixExtractFilePath(FileName);
    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);
    assert(!FIgnoreFileList);
    FIgnoreFileList = true;
    try
    {
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      FFileTransferFileName = DestFileName;
      TFileTransferData UserData;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      FFileZillaIntf->FileTransfer(DestFullName.c_str(), OnlyFileName.c_str(),
        FilePath.c_str(), true, File->Size, TransferType, &UserData);
      unsigned int Reply = WaitForReply();
      GotReply(Reply, FLAGMASK(FFileTransferCancelled, REPLY_ALLOW_CANCEL));
    }
    __finally
    {
      FIgnoreFileList = false;
    }

    CheckFileTransferAbort();

    // in case dest filename is changed from overwrite dialog
    if (DestFileName != FFileTransferFileName)
    {
      DestFullName = TargetDir + FFileTransferFileName;
      Attrs = FileGetAttr(DestFullName);
    }

    if (Attrs == -1)
    {
      Attrs = faArchive;
    }
    int NewAttrs = CopyParam->LocalFileAttrs(*File->Rights);
    if ((NewAttrs & Attrs) != NewAttrs)
    {
      FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (DestFullName)),
        THROWIFFALSE(FileSetAttr(DestFullName, Attrs | NewAttrs) == 0);
      );
    }
  }

  if (FLAGSET(Params, cpDelete))
  {
    // If file is directory, do not delete it recursively, because it should be
    // empty already. If not, it should not be deleted (some files were
    // skipped or some new files were copied to it, while we were downloading)
    bool Recursive = false;
    FTerminal->DeleteFile(FileName, File, &Recursive);
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
  bool & DisconnectWhenComplete)
{
  assert((FilesToCopy != NULL) && (OperationProgress != NULL));

  AnsiString FileName, FileNameOnly;
  AnsiString TargetDir = AbsolutePath(ATargetDir);
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
      OperationProgress->Finish(FileName, Success, DisconnectWhenComplete);
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
  // set in advance as particularly cpNoConfirmation may change
  int ReopenParams =
    ropNoReadDirectory |
    FLAGMASK(FLAGSET(Params, cpNoConfirmation), ropNoConfirmation);

  do
  {
    Retry = false;
    try
    {
      Source(FileName, TargetDir, CopyParam, Params, OperationProgress, Flags);
    }
    catch(Exception & E)
    {
      Retry = true;
      if (FTerminal->Active ||
          !FTerminal->QueryReopen(&E, ReopenParams, OperationProgress))
      {
        throw;
      }
    }

    if (Retry)
    {
      OperationProgress->RollbackTransfer();
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
  TFileOperationProgressType * OperationProgress, unsigned int Flags)
{
  FTerminal->LogEvent(FORMAT("File: \"%s\"", (FileName)));

  OperationProgress->SetFile(FileName);

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

  if (Dir)
  {
    DirectorySource(IncludeTrailingBackslash(FileName), TargetDir,
      Attrs, CopyParam, Params, OperationProgress, Flags);
  }
  else
  {
    AnsiString DestFileName = CopyParam->ChangeFileName(ExtractFileName(FileName),
      osLocal, FLAGSET(Flags, tfFirstLevel));
    AnsiString DestFullName = TargetDir + DestFileName;

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

    unsigned int TransferType = (OperationProgress->AsciiTransfer ? 1 : 2);
    assert(!FIgnoreFileList);
    FIgnoreFileList = true;
    try
    {
      // not supports for uploads anyway
      FFileTransferPreserveTime = CopyParam->PreserveTime;
      // not used for uploads
      FFileTransferFileName = DestFileName;
      TFileTransferData UserData;
      UserData.Params = Params;
      UserData.AutoResume = FLAGSET(Flags, tfAutoResume);
      FFileZillaIntf->FileTransfer(FileName.c_str(), DestFileName.c_str(),
        TargetDir.c_str(), false, Size, TransferType, &UserData);
      unsigned int Reply = WaitForReply();
      GotReply(Reply, FLAGMASK(FFileTransferCancelled, REPLY_ALLOW_CANCEL));
    }
    __finally
    {
      FIgnoreFileList = false;
    }
  }

  /* TODO : Delete also read-only files. */
  /* TODO : Show error message on failure. */
  if (FLAGSET(Params, cpDelete))
  {
    Sysutils::DeleteFile(FileName);
  }
  else if (CopyParam->ClearArchive && FLAGSET(Attrs, faArchive))
  {
    FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
      THROWIFFALSE(FileSetAttr(FileName, Attrs & ~faArchive) == 0);
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

  while (FindOK && !OperationProgress->Cancel)
  {
    AnsiString FileName = DirectoryName + SearchRec.Name;
    try
    {
      if ((SearchRec.Name != ".") && (SearchRec.Name != ".."))
      {
        SourceRobust(FileName, DestFullName, CopyParam, Params, OperationProgress,
          Flags & ~(tfFirstLevel | tfAutoResume));
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

  FindClose(SearchRec);

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
        THROWIFFALSE(FileSetAttr(DirectoryName, Attrs & ~faArchive) == 0);
      )
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CreateDirectory(const AnsiString ADirName,
  const TRemoteProperties * Properties)
{
  AnsiString DirName = AbsolutePath(ADirName);

  FIgnoreFileList = true;
  try
  {
    FFileZillaIntf->MakeDir(DirName.c_str());

    GotReply(WaitForReply(), REPLY_2XX_CODE);
  }
  __finally
  {
    FIgnoreFileList = false;
  }

  if (Properties != NULL)
  {
    ChangeFileProperties(DirName, NULL, Properties);
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
  const TRemoteFile * File, bool Recursive)
{
  AnsiString FileName = AbsolutePath(AFileName);
  AnsiString FileNameOnly = UnixExtractFileName(FileName);
  AnsiString FilePath = UnixExtractFilePath(FileName);

  bool Dir = (File != NULL) && File->IsDirectory && !File->IsSymLink;

  if (Dir && Recursive)
  {
    FTerminal->ProcessDirectory(FileName, FTerminal->DeleteFile, &Recursive);
  }

  assert(!FIgnoreFileList);
  FIgnoreFileList = true;
  try
  {
    if (Dir)
    {
      FFileZillaIntf->RemoveDir(FileNameOnly.c_str(), FilePath.c_str());
    }
    else
    {
      FFileZillaIntf->Delete(FileNameOnly.c_str(), FilePath.c_str());
    }
    GotReply(WaitForReply(), REPLY_2XX_CODE);
  }
  __finally
  {
    FIgnoreFileList = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::CustomCommandOnFile(const AnsiString /*FileName*/,
  const TRemoteFile * /*File*/, AnsiString /*Command*/, int /*Params*/,
  TCaptureOutputEvent /*OutputEvent*/)
{
  // if ever implemeneted, do not forget to add EnsureLocation,
  // see AnyCommand for a reason why
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TFTPFileSystem::DoStartup()
{
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

    case fcModeChangingUpload:
    case fcPreservingTimestampUpload:
    case fcLoadingAdditionalProperties:
    case fcShellAnyCommand:
    case fcCalculatingChecksum:
    case fcHardLink:
    case fcSymbolicLink:
    case fcCheckingSpaceAvailable:
    case fcUserGroupListing:
    case fcGroupChanging:
    case fcOwnerChanging:
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
    GotReply(WaitForReply(), REPLY_2XX_CODE, "", &Code, &Response);

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
            FCurrentDirectory = Path;
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
void __fastcall TFTPFileSystem::ReadDirectory(TRemoteFileList * FileList)
{
  FileList->Clear();
  // FZAPI does not list parent directory, add it
  FileList->Add(new TRemoteParentDirectory());

  FLastReadDirectoryProgress = 0;

  assert(FFileList == NULL);
  FFileList = FileList;
  try
  {
    // always specify path to list, do not attempt to list "current" dir as:
    // 1) List() lists again the last listed directory, not the current working directory
    // 2) we handle this way the cached directory change
    AnsiString Directory = AbsolutePath(FileList->Directory);
    FFileZillaIntf->List(Directory.c_str());

    GotReply(WaitForReply(), REPLY_2XX_CODE | REPLY_ALLOW_CANCEL);
  }
  __finally
  {
    FFileList = NULL;
  }
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
  if ((FFileListCache == NULL) ||
      !UnixComparePaths(Path, FFileListCache->Directory))
  {
    delete FFileListCache;
    FFileListCache = NULL;
    FFileListCache = new TRemoteFileList();
    FFileListCache->Directory = Path;
    ReadDirectory(FFileListCache);
  }

  TRemoteFile * AFile = FFileListCache->FindFile(NameOnly);
  if (AFile != NULL)
  {
    File = AFile->Duplicate();
  }
  else
  {
    File = NULL;
  }
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
  AnsiString FileName = AbsolutePath(AFileName);
  AnsiString NewName = AbsolutePath(ANewName);

  AnsiString FileNameOnly = UnixExtractFileName(FileName);
  AnsiString FilePathOnly = UnixExtractFilePath(FileName);
  AnsiString NewNameOnly = UnixExtractFileName(NewName);
  AnsiString NewPathOnly = UnixExtractFilePath(NewName);

  FIgnoreFileList = true;
  try
  {
    FFileZillaIntf->Rename(FileNameOnly.c_str(), NewNameOnly.c_str(),
      FilePathOnly.c_str(), NewPathOnly.c_str());

    GotReply(WaitForReply(), REPLY_2XX_CODE);
  }
  __finally
  {
    FIgnoreFileList = false;
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
      FOptionScratch = Data->ProxyHost;
      break;

    case OPTION_PROXYUSER:
      FOptionScratch = Data->ProxyUsername;
      break;

    case OPTION_PROXYPASS:
      FOptionScratch = Data->ProxyPassword;
      break;

    case OPTION_ANONPWD:
    case OPTION_TRANSFERIP:
    case OPTION_TRANSFERIP6:
      FOptionScratch = "";
      break;

    case OPTION_FWHOST:
    case OPTION_FWUSER:
    case OPTION_FWPASS:
      // should never get here OPTION_LOGONTYPE being 0
      assert(false);
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
        default:
          assert(false);
          Result = 0; // PROXYTYPE_NOPROXY;
          break;
      }
      break;

    case OPTION_PROXYPORT:
      Result = Data->ProxyPort;
      break;

    case OPTION_PROXYUSELOGON:
      Result = !Data->ProxyUsername.IsEmpty();
      break;

    case OPTION_LOGONTYPE:
      Result = 0; // no FTP proxy
      break;

    case OPTION_FWPORT:
      // should never get here OPTION_LOGONTYPE being 0
      assert(false);
      Result = 0;
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

    default:
      assert(false);
      Result = FALSE;
      break;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::PostMessage(WPARAM wParam, LPARAM lParam)
{
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
unsigned int __fastcall TFTPFileSystem::PoolForReply()
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

  return Reply;
}
//---------------------------------------------------------------------------
unsigned int __fastcall TFTPFileSystem::WaitForReply(bool Command)
{
  assert(FReply == 0);
  assert(FCommandReply == 0);
  assert(!FWaitingForReply);

  ResetReply();
  FWaitingForReply = true;

  unsigned int Reply;

  try
  {
    unsigned int& ReplyToAwait = (Command ? FCommandReply : FReply);
    while (ReplyToAwait == 0)
    {
      WaitForMessages();
      // wait for the first reply only,
      // i.e. in case two replies are posted get the first only.
      // e.g. when server closes the connection, but posts error message before,
      // sometime it happens that command (like download) fails because of the error
      // and does not catch the disconnection. then asynchronous "disconnect reply"
      // is posted immediately afterwards. leave detection of that to Idle()
      while (ProcessMessage() && (ReplyToAwait == 0));
    }

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
          // for connection failure, do not report that connection was lost, its obvious
          if (FLAGCLEAR(Flags, REPLY_CONNECT))
          {
            MoreMessages->Add(LoadStr(LOST_CONNECTION));
          }
          Discard();
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

    if (Code != NULL)
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
void __fastcall TFTPFileSystem::HandleReplyStatus(const char * AStatus)
{
  int Code;
  AnsiString Response(AStatus);

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
    FLastCode = Code;
    FLastCodeClass = (Code / 100);
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
bool __fastcall TFTPFileSystem::HandleStatus(const char * Status, int Type)
{
  assert(
    (Type <= TFileZillaIntf::LOG_LIST) ||
    ((Type <= TFileZillaIntf::LOG_WARNING) && (FTerminal->Configuration->LogProtocol >= 1)) ||
    (FTerminal->Configuration->LogProtocol >= 2));

  TLogLineType LogType;

  switch (Type)
  {
    case TFileZillaIntf::LOG_STATUS:
      FTerminal->Information(Status, true);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_COMMAND:
      if (strcmp(Status, "SYST") == 0)
      {
        FLastCommand = SYST;
      }
      else if (strcmp(Status, "FEAT") == 0)
      {
        FLastCommand = FEAT;
      }
      else if (strncmp(Status, "PASS ", 5) == 0)
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
      // there can be multiple error messages associated with single failure
      // (such as "cannot open local file..." followed by "download failed"
      FLastError->Add(Status);
      LogType = llMessage;
      break;

    case TFileZillaIntf::LOG_REPLY:
      HandleReplyStatus(Status);
      LogType = llOutput;
      break;

    default:
      LogType = llMessage;
      break;
  }

  if (FTerminal->Log->Logging)
  {
    FTerminal->Log->Add(LogType, Status);
  }

  return true;
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
    TFileOperationProgressType * OperationProgress = FTerminal->OperationProgress;
    AnsiString FileName = FileName1;
    assert(FFileTransferFileName == FileName);
    TOverwriteMode OverwriteMode = omOverwrite;
    TOverwriteFileParams FileParams;
    FileParams.SourceSize = Size2;
    FileParams.DestSize = Size1;
    // !!! TODO DST
    FileParams.SourceTimestamp = UnixToDateTime(Time2, dstmUnix);
    FileParams.DestTimestamp = UnixToDateTime(Time1, dstmUnix);
    if ((OperationProgress->Side == osLocal) && !HasTime1)
    {
      FileParams.DestPrecision = mfMDY;
    }
    if ((OperationProgress->Side == osRemote) && !HasTime2)
    {
      FileParams.SourcePrecision = mfMDY;
    }

    if (ConfirmOverwrite(FileName, OverwriteMode, OperationProgress,
          &FileParams, UserData.Params, UserData.AutoResume))
    {
      switch (OverwriteMode)
      {
        case omOverwrite:
          if (FileName != FileName1)
          {
            strncpy(FileName1, FileName.c_str(), FileName1Len);
            FileName1[FileName1Len - 1] = '\0';
            FFileTransferFileName = FileName1;
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
    assert(UnixComparePaths(AbsolutePath(FFileList->Directory), Path));
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
          File->Rights->Text = Entry->Permissions + 1;
        }

        const char * Space = strchr(Entry->OwnerGroup, ' ');
        if (Space != NULL)
        {
          File->Owner = AnsiString(Entry->OwnerGroup, Space - Entry->OwnerGroup);
          File->Group = Space + 1;
        }
        else
        {
          File->Owner = Entry->OwnerGroup;
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
        throw ETerminal(&E, FMTLOAD(LIST_LINE_ERROR, (
          FORMAT("%s/%s/%s/%s/%d/%d/%d/%d/%d/%d/%d/%d/%d",
            (Entry->Name, Entry->Permissions, Entry->OwnerGroup, IntToStr(Entry->Size),
             Entry->Dir, Entry->Link, Entry->Year, Entry->Month, Entry->Day,
             Entry->Hour, Entry->Minute, Entry->HasTime, Entry->HasDate)))));
      }

      FFileList->AddFile(File);
    }
    return true;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFTPFileSystem::HandleTransferStatus(bool Valid, __int64 TransferSize,
  __int64 Bytes, int Percent, int /*TimeElapsed*/, int /*TimeLeft*/, int /*TransferRate*/,
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
    FileTransferProgress(TransferSize, Bytes, Percent);
  }
  else
  {
    assert(Percent == -1);
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
    // (it is typically used asynchronously to notify about disconnects,
    // this is handled in TFileZillaIntf::PostMessage already)
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
  // waiting for user interaction and is detected with call to
  // SetAsyncRequestResult)
  if (FLAGSET(ReturnCode, TFileZillaIntf::REPLY_NOTCONNECTED))
  {
    if (!FWaitingForReply)
    {
      unsigned int Reply = WaitForReply(false);

      assert(FLAGSET(Reply, TFileZillaIntf::REPLY_DISCONNECTED));
      GotReply(Reply);

      // should never get here as GotReply should raise fatal exception
      assert(false);
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
