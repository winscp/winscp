//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Terminal.h"

#include <SysUtils.hpp>
#include <FileCtrl.hpp>

#include "Common.h"
#include "FileBuffer.h"
#include "FileSystems.h"
#include "Interface.h"
#include "RemoteFiles.h"
#include "ScpFileSystem.h"
#include "SftpFileSystem.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
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
__fastcall TTerminal::TTerminal(): TSecureShell()
{
  FFiles = new TRemoteDirectory(this);
  FExceptionOnFail = 0;
  FInTransaction = 0;
  FReadCurrentDirectoryPending = false;
  FReadDirectoryPending = false;
  FUserGroupsLookedup = False;
  FUserGroups = new TUserGroupsList();
  FOnProgress = NULL;
  FOnFinished = NULL;
  FOnDeleteLocalFile = NULL;
  FAdditionalInfo = NULL;
  FUseBusyCursor = True;
  FLockDirectory = "";
  FDirectoryCache = new TRemoteDirectoryCache();
  FDirectoryChangesCache = NULL;
  FFSProtocol = cfsUnknown;
}
//---------------------------------------------------------------------------
__fastcall TTerminal::~TTerminal()
{
  if (SessionData->CacheDirectoryChanges && SessionData->PreserveDirectoryChanges &&
      (FDirectoryChangesCache != NULL))
  {
    Configuration->SaveDirectoryChangesCache(SessionData->SessionKey,
      FDirectoryChangesCache);
  }

  SAFE_DESTROY(FFileSystem);
  delete FFiles;
  delete FUserGroups;
  delete FDirectoryCache;
  delete FDirectoryChangesCache;
  delete FAdditionalInfo;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::KeepAlive()
{
  if (SessionData->PingType == ptDummyCommand)
  {
    LogEvent("Executing dummy command to keep session alive.");
    assert(Active);
    assert(FFileSystem != NULL);
    try
    {
      FFileSystem->KeepAlive();
    }
    catch(Exception & E)
    {
      if (Active)
      {
        HandleExtendedException(&E, this);
      }
      else
      {
        throw;
      }
    }
  }
  else
  {
    TSecureShell::KeepAlive();
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
AnsiString __fastcall TTerminal::GetProtocolName()
{
  assert(FFileSystem);
  return FFileSystem->ProtocolName;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Close()
{
  // file system cannot be destoryed here, moved to destructor
  TSecureShell::Close();
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::Open()
{
  TSecureShell::Open();
  assert(!FFileSystem);
  if ((SessionData->FSProtocol == fsSCPonly) ||
      (SessionData->FSProtocol == fsSFTP && SshFallbackCmd()))
  {
    FFSProtocol = cfsSCP;
    FFileSystem = new TSCPFileSystem(this);
    LogEvent("Using SCP protocol.");
  }
  else
  {
    FFSProtocol = cfsSFTP;
    FFileSystem = new TSFTPFileSystem(this);
    LogEvent("Using SFTP protocol.");
  }
  if (SessionData->CacheDirectoryChanges)
  {
    FDirectoryChangesCache = new TRemoteDirectoryChangesCache();
    if (SessionData->PreserveDirectoryChanges)
    {
      Configuration->LoadDirectoryChangesCache(SessionData->SessionKey,
        FDirectoryChangesCache);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetIsCapable(TFSCapability Capability) const
{
  assert(FFileSystem);
  return FFileSystem->IsCapable(Capability);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TTerminal::GetAdditionalInfo()
{
  bool Initial = (FAdditionalInfo == NULL);
  if (Initial)
  {
    FAdditionalInfo = new TStringList();
  }
  assert(FFileSystem);
  FFileSystem->AdditionalInfo(FAdditionalInfo, Initial);
  return FAdditionalInfo;
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
    case fsCreateDirectory:
    case fsChangeMode:
    case fsChangeGroup:
    case fsChangeOwner:
    case fsChangeProperties:
    case fsAnyCommand:
      ModifiesFiles = true;
      break;
  }

  if (ChangesDirectory)
  {
    if (!FInTransaction)
    {
      ReadCurrentDirectory();
      ReadDirectory(false);
    }
      else
    {
      FReadCurrentDirectoryPending = true;
      FReadDirectoryPending = true;
    }
  }
    else
  if (ModifiesFiles)
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
TUserGroupsList * __fastcall TTerminal::GetUserGroups()
{
  assert(FFileSystem);
  if (!FUserGroupsLookedup && SessionData->LookupUserGroups &&
      IsCapable[fcUserGroupListing])
  {
    LookupUserGroups();
  }
  return FUserGroups;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TTerminal::GetUserName() const
{
  // in future might be implemented to detect username similar to GetUserGroups
  return SessionData->UserName;
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
void __fastcall TTerminal::BeginTransaction()
{
  if (!FInTransaction)
  {
    FReadCurrentDirectoryPending = false;
    FReadDirectoryPending = false;
  }
  FInTransaction++;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::EndTransaction()
{
  // it connection was closed due to fatal error during transaction, do nothing
  if (Active)
  {
    if (!FInTransaction)
      TerminalError("Can't end transaction, not in transaction");
    assert(FInTransaction > 0);
    FInTransaction--;

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
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::GetExceptionOnFail() const
{
  return (bool)(FExceptionOnFail > 0);
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
      ShowExtendedException(ECmd, this);
    }
    __finally
    {
      delete ECmd;
    }
  }
  else
  {
    Result = DoQueryUser(Msg, E, Answers, qpAllowContinueOnError);
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
    HandleExtendedException(E, this);
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
  const TOverwriteFileParams * FileParams, int Answers, int Params)
{
  AnsiString Message = FMTLOAD(FILE_OVERWRITE, (FileName));
  if (FileParams)
  {
    Message = FMTLOAD(FILE_OVERWRITE_DETAILS, (Message,
      IntToStr(FileParams->SourceSize),
      FormatDateTime("ddddd tt", FileParams->SourceTimestamp),
      IntToStr(FileParams->DestSize),
      FormatDateTime("ddddd tt", FileParams->DestTimestamp)));
  }
  return DoQueryUser(Message, Answers, Params);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::FileModified(const TRemoteFile * File,
  const AnsiString FileName)
{
  if (SessionData->CacheDirectories)
  {
    if ((File != NULL) && (File->Directory != NULL))
    {
      if (File->IsDirectory)
      {
        // do not use UnixIncludeTrailingBackslash(CurrentDirectory)
        FDirectoryCache->ClearFileList(
          File->Directory->FullDirectory + File->FileName, true);
      }
      FDirectoryCache->ClearFileList(File->Directory->Directory, false);
    }
    else if (!FileName.IsEmpty())
    {
      AnsiString Directory = UnixExtractFilePath(FileName);
      FDirectoryCache->ClearFileList(
        !Directory.IsEmpty() ? Directory : CurrentDirectory, false);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DirectoryModified(const AnsiString UnlockedPath, bool SubDirs)
{
  FDirectoryCache->ClearFileList(TranslateLockedPath(UnlockedPath, true),
    SubDirs);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReloadDirectory()
{
  if (SessionData->CacheDirectories)
  {
    FDirectoryCache->ClearFileList(CurrentDirectory, false);
  }
  if (SessionData->CacheDirectoryChanges)
  {
    assert(FDirectoryChangesCache != NULL);
    FDirectoryChangesCache->ClearDirectoryChange(CurrentDirectory);
  }

  ReadCurrentDirectory();
  FReadCurrentDirectoryPending = false;
  ReadDirectory(True);
  FReadDirectoryPending = false;
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
void __fastcall TTerminal::DoStartup()
{
  LogEvent("Doing startup conversation with host.");
  BeginTransaction();
  try
  {
    UpdateStatus(sshStartup);

    // Make sure that directory would be loaded at last
    FReadCurrentDirectoryPending = true;
    FReadDirectoryPending = true;

    FFileSystem->DoStartup();

    if (SessionData->LookupUserGroups && IsCapable[fcUserGroupListing])
    {
      LookupUserGroups();
    }

    UpdateStatus(sshOpenDirectory);
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
  UpdateStatus(sshReady);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ReadCurrentDirectory()
{
  assert(FFileSystem);
  try
  {
    LogEvent("Getting current directory name.");
    AnsiString OldDirectory = FFileSystem->CurrentDirectory;

    FFileSystem->ReadCurrentDirectory();
    ReactOnCommand(fsCurrentDirectory);

    if (SessionData->CacheDirectoryChanges)
    {
      assert(FDirectoryChangesCache != NULL);
      FDirectoryChangesCache->AddDirectoryChange(OldDirectory,
        FLastDirectoryChange, CurrentDirectory);
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
void __fastcall TTerminal::ReadDirectory(bool ReloadOnly)
{
  TRemoteFileList * CachedFileList = NULL;
  if (SessionData->CacheDirectories)
  {
    CachedFileList = FDirectoryCache->GetFileList(CurrentDirectory);
  }

  if (CachedFileList)
  {
    if (ReloadOnly)
    {
      LogEvent("Cached directory not reloaded.");
    }
    else
    {
      DoStartReadDirectory();
      try
      {
        CachedFileList->DuplicateTo(FFiles);
      }
      __finally
      {
        DoReadDirectory(ReloadOnly);
      }
      LogEvent("Directory content loaded from cache.");
    }
  }
  else
  {
    DoStartReadDirectory();
    FFiles->Directory = CurrentDirectory;

    try
    {
      try
      {
        CustomReadDirectory(FFiles);
      }
      __finally
      {
        // this must be called before error is displayed, otherwise
        // TUnixDirView would be drawn with invalid data (it keeps reference
        // to already destoroyed old listing)
        DoReadDirectory(ReloadOnly);
        if (Active)
        {
          if (SessionData->CacheDirectories)
          {
            FDirectoryCache->AddFileList(FFiles);
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
TRemoteFileList * TTerminal::ReadDirectoryListing(AnsiString Directory)
{
  TRemoteFileList * FileList;
  try
  {
    FileList = new TRemoteFileList();
    try
    {
      FileList->Directory = Directory;
      AnsiString Directory = UnixIncludeTrailingBackslash(Directory);
      ExceptionOnFail = true;
      try
      {
        ReadDirectory(FileList);
      }
      __finally
      {
        ExceptionOnFail = false;
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
      FileList = ReadDirectoryListing(Directory);
    );
  }
  return FileList;
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::ProcessDirectory(const AnsiString DirName,
  TProcessFileEvent CallBackFunc, void * Param)
{
  TRemoteFileList * FileList = ReadDirectoryListing(DirName);

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
    CommandError(&E, FMTLOAD(LIST_DIR_ERROR, (FileName)));
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::ProcessFiles(TStrings * FileList,
  TFileOperation Operation, TProcessFileEvent ProcessFile, void * Param,
  TOperationSide Side)
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
      BeginTransaction();
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
            ProcessFile(FileName, (TRemoteFile *)FileList->Objects[Index], Param);
            Success = true;
          }
          __finally
          {
            AnsiString FileNameOnly = (Side == osRemote) ?
              UnixExtractFileName(FileName) : ExtractFileName(FileName);
            Progress.Finish(FileNameOnly, Success, DisconnectWhenComplete);
          }
          Index++;
        }
      }
      __finally
      {
        EndTransaction();
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
  LogEvent(FORMAT("Deleting file \"%s\".", (FileName)));
  if (File) FileModified(File, FileName);
  DoDeleteFile(FileName, File, Recursive);
  ReactOnCommand(fsDeleteFile);
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
  DoCustomCommandOnFile(FileName, File, Params->Command, Params->Params);
  ReactOnCommand(fsAnyCommand);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoCustomCommandOnFile(AnsiString FileName,
  const TRemoteFile * File, AnsiString Command, int Params)
{
  try
  {
    assert(FFileSystem);
    FFileSystem->CustomCommandOnFile(FileName, File, Command, Params);
  }
  catch(Exception & E)
  {
    COMMAND_ERROR_ARI
    (
      FMTLOAD(CUSTOM_COMMAND_ERROR, (Command, FileName)),
      DoCustomCommandOnFile(FileName, File, Command, Params)
    );
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CustomCommandOnFiles(AnsiString Command,
  int Params, TStrings * Files)
{
  TCustomCommandParams AParams;
  AParams.Command = Command;
  AParams.Params = Params;
  ProcessFiles(Files, foCustomCommand, CustomCommandOnFile, &AParams);
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
  if (IsLogging())
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
  }
  if (File) FileModified(File, FileName);
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
  ProcessFiles(FileList, foSetProperties, ChangeFileProperties, (void *)Properties);
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateFileSize(AnsiString FileName,
  const TRemoteFile * File, /*TCalculateSizeParams*/ void * Param)
{
  assert(Param);
  assert(File);

  if (FileName.IsEmpty() && File)
  {
    FileName = File->FileName;
  }
  if (File->IsDirectory && !File->IsSymLink)
  {
    LogEvent(FORMAT("Getting size of directory \"%s\"", (FileName)));
    DoCalculateDirectorySize(FileName, File,
      static_cast<TCalculateSizeParams*>(Param));
  }
  else
  {
    static_cast<TCalculateSizeParams*>(Param)->Size += File->Size;
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
  __int64 & Size, int Params)
{
  TCalculateSizeParams Param;
  Param.Size = 0;
  Param.Params = Params;
  ProcessFiles(FileList, foCalculateSize, CalculateFileSize, &Param);
  Size = Param.Size;
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
      Result = DoQueryUser(FORMAT(QuestionFmt, (NewName)),
        qaYes | qaNo, qpNeverAskAgainCheck);
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
    FDirectoryCache->ClearFileList(CurrentDirectory, false);
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
    if (SessionData->CacheDirectoryChanges &&
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
void __fastcall TTerminal::LookupUserGroups()
{
  assert(FFileSystem);
  assert(IsCapable[fcUserGroupListing]);

  try
  {
    FUserGroupsLookedup = true;
    LogEvent("Looking up current user groups.");
    FFileSystem->LookupUserGroups();
    ReactOnCommand(fsLookupUserGroups);

    if (IsLogging())
    {
      LogEvent("Following groups found:");
      for (int Index = 0; Index < FUserGroups->Count; Index++)
      {
        LogEvent(AnsiString("  ") + FUserGroups->Strings[Index]);
      }
    }
  }
  catch (Exception &E)
  {
    CommandError(&E, LoadStr(LOOKUP_GROUPS_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::AnyCommand(const AnsiString Command)
{
  assert(FFileSystem);
  try
  {
    DirectoryModified(CurrentDirectory, false);
    LogEvent("Executing used defined command.");
    FFileSystem->AnyCommand(Command);
    ReactOnCommand(fsAnyCommand);
  }
  catch (Exception &E)
  {
    if (ExceptionOnFail || (E.InheritsFrom(__classid(EFatal)))) throw;
      else ShowExtendedException(&E, this);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CreateLocalFile(const AnsiString FileName,
  TFileOperationProgressType * OperationProgress, HANDLE * AHandle)
{
  assert(AHandle);
  bool Result = true;

  FILE_OPERATION_LOOP (FMTLOAD(CREATE_FILE_ERROR, (FileName)),
    bool Done;
    do
    {
      *AHandle = CreateFile(FileName.c_str(), GENERIC_WRITE, 0, NULL,
        CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
      Done = (*AHandle != INVALID_HANDLE_VALUE);
      if (!Done)
      {
        int FileAttr;
        if (FileExists(FileName) &&
          (((FileAttr = FileGetAttr(FileName)) & faReadOnly) != 0))
        {
          if (OperationProgress->NoToAll)
          {
            Result = false;
          }
          else if (!OperationProgress->YesToAll)
          {
            int Answer;
            SUSPEND_OPERATION
            (
              Answer = DoQueryUser(
                FMTLOAD(READ_ONLY_OVERWRITE, (FileName)),
                qaYes | qaNo | qaAbort | qaYesToAll | qaNoToAll, 0);
            );
            switch (Answer) {
              case qaYesToAll: OperationProgress->YesToAll = true; break;
              case qaAbort: OperationProgress->Cancel = csCancel; // continue on next case
              case qaNoToAll: OperationProgress->NoToAll = true;
              case qaNo: Result = false; break;
            }
          }

          if (Result)
          {
            FILE_OPERATION_LOOP (FMTLOAD(CANT_SET_ATTRS, (FileName)),
              if (FileSetAttr(FileName, FileAttr & ~faReadOnly) != 0)
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
  int Access, int * AAttrs, HANDLE * AHandle, unsigned long * ACTime,
  unsigned long * AMTime, unsigned long * AATime, __int64 * ASize,
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
        Access == GENERIC_READ ? FILE_SHARE_READ : 0,
        NULL, OPEN_EXISTING, 0, 0);
      if (Handle == INVALID_HANDLE_VALUE)
      {
        Handle = 0;
        EXCEPTION;
      }
    );

    try
    {
      if (AATime || AMTime)
      {
        // Get last file access and modification time
        FILE_OPERATION_LOOP (FMTLOAD(CANT_GET_ATTRS, (FileName)),
          FILETIME ATime;
          FILETIME MTime;
          FILETIME CTime;
          if (!GetFileTime(Handle, &CTime, &ATime, &MTime)) EXCEPTION;
          if (ACTime) TIME_WIN_TO_POSIX(CTime, *ACTime);
          if (AATime) TIME_WIN_TO_POSIX(ATime, *AATime);
          if (AMTime) TIME_WIN_TO_POSIX(MTime, *AMTime);
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
void __fastcall TTerminal::CalculateLocalFileSize(const AnsiString FileName,
  const TSearchRec Rec, /*__int64*/ void * Size)
{
  if ((Rec.Attr & faDirectory) == 0)
  {
    (*static_cast<__int64*>(Size)) +=
      (static_cast<__int64>(Rec.FindData.nFileSizeHigh) << 32) +
      Rec.FindData.nFileSizeLow;
  }
  
  if (OperationProgress && OperationProgress->Operation == foCalculateSize)
  {
    if (OperationProgress->Cancel != csContinue) Abort();
    OperationProgress->SetFile(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::CalculateLocalFilesSize(TStrings * FileList, __int64 & Size)
{
  Size = 0;
  TFileOperationProgressType OperationProgress(FOnProgress, FOnFinished);
  OperationProgress.Start(foCalculateSize, osLocal, FileList->Count);
  try
  {
    assert(!FOperationProgress);
    FOperationProgress = &OperationProgress;
    TSearchRec Rec;
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      if (FileSearchRec(FileList->Strings[Index], Rec))
      {
        if (Rec.Attr & faDirectory)
        {
          ProcessLocalDirectory(FileList->Strings[Index],
            CalculateLocalFileSize, &Size);
        }
        CalculateLocalFileSize(FileList->Strings[Index], Rec, &Size);
      }
    }
  }
  __finally
  {
    FOperationProgress = NULL;
    OperationProgress.Stop();
  }
}
//---------------------------------------------------------------------------
struct TSynchronizeFileData
{
  int Time;
  int Attr;
  unsigned long SizeHigh;
  unsigned long SizeLow;
  FILETIME LastWriteTime;
  bool Modified;
  bool New;
};
//---------------------------------------------------------------------------
struct TSynchronizeData
{
  AnsiString LocalDirectory;
  AnsiString RemoteDirectory;
  TTerminal::TSynchronizeMode Mode;
  int Params;
  TSynchronizeDirectory OnSynchronizeDirectory;
  TStringList * LocalFileList;
  TStringList * ModifiedRemoteFileList;
  TStringList * NewRemoteFileList;
};
//---------------------------------------------------------------------------
void __fastcall TTerminal::Synchronize(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory)
{
  BeginTransaction();
  try
  {
    DoSynchronizeDirectory(LocalDirectory, RemoteDirectory, Mode, Params,
      OnSynchronizeDirectory);
  }
  __finally
  {
    EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::DoSynchronizeDirectory(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode, int Params,
  TSynchronizeDirectory OnSynchronizeDirectory)
{
  TSearchRec SearchRec;
  bool Found;
  bool Delete = (Params & spDelete) != 0;
  TSynchronizeData Data;

  Data.LocalDirectory = IncludeTrailingBackslash(LocalDirectory);
  Data.RemoteDirectory = UnixIncludeTrailingBackslash(RemoteDirectory);
  Data.Mode = Mode;
  Data.Params = Params;
  Data.OnSynchronizeDirectory = OnSynchronizeDirectory;
  Data.LocalFileList = NULL;
  Data.NewRemoteFileList = NULL;
  Data.ModifiedRemoteFileList = NULL;
  TStrings * LocalFileList = NULL;

  LogEvent(FORMAT("Synchronizing local directory '%s' with remote directory '%s', "
    "mode = %d, params = %d", (LocalDirectory, RemoteDirectory,
    int(Mode), int(Params))));

  bool Continue = true;
  OnSynchronizeDirectory(LocalDirectory, RemoteDirectory, Continue);

  if (!Continue)
  {
    Abort();
  }

  try
  {
    Data.LocalFileList = new TStringList();
    Data.LocalFileList->Sorted = true;
    Data.LocalFileList->CaseSensitive = false;
    Data.NewRemoteFileList = new TStringList();
    Data.ModifiedRemoteFileList = new TStringList();
    LocalFileList = new TStringList();

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
          if ((FileName != ".") && (FileName != ".."))
          {
            TSynchronizeFileData * FileData = new TSynchronizeFileData;
            FileData->Time = SearchRec.Time;
            FileData->SizeHigh = SearchRec.FindData.nFileSizeHigh;
            FileData->SizeLow = SearchRec.FindData.nFileSizeLow;
            FileData->Attr = SearchRec.Attr;
            FileData->LastWriteTime = SearchRec.FindData.ftLastWriteTime;
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

      ProcessDirectory(RemoteDirectory, SynchronizeFile, &Data);

      TSynchronizeFileData * FileData;
      for (int Index = 0; Index < Data.LocalFileList->Count; Index++)
      {
        FileData = reinterpret_cast<TSynchronizeFileData *>
          (Data.LocalFileList->Objects[Index]);
        if ((FileData->Modified && ((Mode == smBoth) || (Mode == smRemote))) ||
            (FileData->New))
        {
          LocalFileList->Add(Data.LocalDirectory + Data.LocalFileList->Strings[Index]);
        }
      }

      TCopyParamType CopyParam = Configuration->CopyParam;
      CopyParam.PreserveTime = true;
      int CopyParams = (Params & spNoConfirmation) != 0 ? cpNoConfirmation : 0;

      if (LocalFileList->Count > 0)
      {
        bool Result;
        if ((Mode == smBoth) || (Mode == smRemote))
        {
          Result = CopyToRemote(LocalFileList, RemoteDirectory, &CopyParam, CopyParams);
        }
        else if ((Mode == smLocal) && Delete)
        {
          Result = DeleteLocalFiles(LocalFileList);
        }
        if (!Result)
        {
          Abort();
        }
      }

      if ((Mode == smBoth) || (Mode == smLocal))
      {
        Data.ModifiedRemoteFileList->AddStrings(Data.NewRemoteFileList);
        if (Data.ModifiedRemoteFileList->Count > 0)
        {
          if (!CopyToLocal(Data.ModifiedRemoteFileList, LocalDirectory,
            &CopyParam, CopyParams))
          {
            Abort();
          }
        }
      }
      if ((Mode == smRemote) && Delete && (Data.NewRemoteFileList->Count > 0))
      {
        if (!DeleteFiles(Data.NewRemoteFileList))
        {
          Abort();
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
        delete reinterpret_cast<TSynchronizeFileData*>
          (Data.LocalFileList->Objects[Index]);
      }
      delete Data.LocalFileList;
    }
    
    TStringList * FileList = Data.NewRemoteFileList;
    while (FileList != Data.ModifiedRemoteFileList)
    {
      if (FileList != NULL)
      {
        for (int Index = 0; Index < FileList->Count; Index++)
        {
          delete static_cast<TRemoteFile*>(FileList->Objects[Index]);
        }
        delete FileList;
      }
      FileList = Data.ModifiedRemoteFileList;
    }

    delete LocalFileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTerminal::SynchronizeFile(const AnsiString FileName,
  const TRemoteFile * File, /*TSynchronizeData*/ void * Param)
{
  TSynchronizeData * Data = static_cast<TSynchronizeData *>(Param);

  bool Modified = false;
  int Index = Data->LocalFileList->IndexOf(File->FileName);
  bool New = (Index < 0);
  if (!New)
  {
    TSynchronizeFileData * LocalData =
      reinterpret_cast<TSynchronizeFileData *>(Data->LocalFileList->Objects[Index]);

    LocalData->New = false;

    bool LocalDirectory = (LocalData->Attr & faDirectory) != 0;
    if (File->IsDirectory != LocalDirectory)
    {
      LogEvent(FORMAT("%s is directory on one side, but file on the another",
        (File->FileName)));
    }
    else if (!File->IsDirectory)
    {
      FILETIME LocalLastWriteTime;
      SYSTEMTIME SystemLastWriteTime;
      FileTimeToLocalFileTime(&LocalData->LastWriteTime, &LocalLastWriteTime);
      FileTimeToSystemTime(&LocalLastWriteTime, &SystemLastWriteTime);
      TDateTime LocalTime = SystemTimeToDateTime(SystemLastWriteTime);
      TDateTime RemoteTime = File->Modification;

      UnifyDateTimePrecision(LocalTime, RemoteTime);

      if (LocalTime < RemoteTime)
      {
        Modified = true;
      }
      else if (LocalTime > RemoteTime)
      {
        LocalData->Modified = true;
      }
    }
    else
    {
      DoSynchronizeDirectory(
        Data->LocalDirectory + File->FileName,
        Data->RemoteDirectory + File->FileName,
        Data->Mode, Data->Params, Data->OnSynchronizeDirectory);
    }
  }

  if (New || Modified)
  {
    assert(!New || !Modified);
    
    TStringList * FileList = New ? Data->NewRemoteFileList :
      Data->ModifiedRemoteFileList;
    FileList->AddObject(FileName,
      const_cast<TRemoteFile *>(File)->Duplicate());
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTerminal::CopyToRemote(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  assert(FFileSystem);
  assert(FilesToCopy);

  bool Result = false;
  bool DisconnectWhenComplete = false;

  try
  {

    __int64 Size;
    if (CopyParam->CalculateSize)
    {
      CalculateLocalFilesSize(FilesToCopy, Size);
    }

    TFileOperationProgressType OperationProgress(FOnProgress, FOnFinished);
    OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osLocal,
      FilesToCopy->Count, Params & cpDragDrop, TargetDir);

    if (CopyParam->CalculateSize)
    {
      OperationProgress.SetTotalSize(Size);
    }

    FOperationProgress = &OperationProgress;
    try
    {
      AnsiString UnlockedTargetDir = TranslateLockedPath(TargetDir, false);
      BeginTransaction();
      try
      {
        if (IsLogging())
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
          EndTransaction();
        }
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
          CalculateFilesSize(FilesToCopy, TotalSize, csIgnoreErrors);
          TotalSizeKnown = true;
        }
        __finally
        {
          ExceptionOnFail = false;
        }
      }

      OperationProgress.Start((Params & cpDelete ? foMove : foCopy), osRemote,
        FilesToCopy->Count, Params & cpDragDrop, TargetDir);

      if (TotalSizeKnown)
      {
        OperationProgress.SetTotalSize(TotalSize);
      }

      FOperationProgress = &OperationProgress;
      try
      {

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
TTerminal * __fastcall TTerminalList::NewTerminal(TSessionData * Data)
{
  TTerminal * Terminal = new TTerminal();
  try
  {
    Terminal->Configuration = FConfiguration;
    Terminal->SessionData = Data;
    Add(Terminal);
  }
  catch(...)
  {
    delete Terminal;
    throw;
  }
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
void __fastcall TTerminalList::Idle()
{
  TTerminal * Terminal;
  for (int i = 0; i < Count; i++)
  {
    Terminal = Terminals[i];
    if (Terminal->Status == sshReady)
    {
      Terminal->Idle();
    }
  }
}

