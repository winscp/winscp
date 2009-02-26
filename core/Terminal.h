//---------------------------------------------------------------------------
#ifndef TerminalH
#define TerminalH

#include <Classes.hpp>

#include "SessionInfo.h"
#include "Interface.h"
#include "FileOperationProgress.h"
#include "FileMasks.h"
//---------------------------------------------------------------------------
class TCopyParamType;
class TFileOperationProgressType;
class TRemoteDirectory;
class TRemoteFile;
class TCustomFileSystem;
class TTunnelThread;
class TSecureShell;
struct TCalculateSizeParams;
struct TOverwriteFileParams;
struct TSynchronizeData;
struct TSynchronizeOptions;
class TSynchronizeChecklist;
struct TCalculateSizeStats;
struct TFileSystemInfo;
struct TSpaceAvailable;
class TTunnelUI;
class TCallbackGuard;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TQueryUserEvent)
  (TObject * Sender, const AnsiString Query, TStrings * MoreMessages, int Answers,
   const TQueryParams * Params, int & Answer, TQueryType QueryType, void * Arg);
typedef void __fastcall (__closure *TPromptUserEvent)
  (TTerminal * Terminal, TPromptKind Kind, AnsiString Name, AnsiString Instructions,
   TStrings * Prompts, TStrings * Results, bool & Result, void * Arg);
typedef void __fastcall (__closure *TDisplayBannerEvent)
  (TTerminal * Terminal, AnsiString SessionName, const AnsiString & Banner,
   bool & NeverShowAgain, int Options);
typedef void __fastcall (__closure *TExtendedExceptionEvent)
  (TTerminal * Terminal, Exception * E, void * Arg);
typedef void __fastcall (__closure *TReadDirectoryEvent)(System::TObject * Sender, Boolean ReloadOnly);
typedef void __fastcall (__closure *TReadDirectoryProgressEvent)(
  System::TObject* Sender, int Progress, bool & Cancel);
typedef void __fastcall (__closure *TProcessFileEvent)
  (const AnsiString FileName, const TRemoteFile * File, void * Param);
typedef void __fastcall (__closure *TProcessFileEventEx)
  (const AnsiString FileName, const TRemoteFile * File, void * Param, int Index);
typedef int __fastcall (__closure *TFileOperationEvent)
  (void * Param1, void * Param2);
typedef void __fastcall (__closure *TSynchronizeDirectory)
  (const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
   bool & Continue, bool Collect);
typedef void __fastcall (__closure *TDeleteLocalFileEvent)(
  const AnsiString FileName, bool Alternative);
typedef int __fastcall (__closure *TDirectoryModifiedEvent)
  (TTerminal * Terminal, const AnsiString Directory, bool SubDirs);
typedef void __fastcall (__closure *TInformationEvent)
  (TTerminal * Terminal, const AnsiString & Str, bool Status, bool Active = true);
//---------------------------------------------------------------------------
#define SUSPEND_OPERATION(Command)                            \
  {                                                           \
    TSuspendFileOperationProgress Suspend(OperationProgress); \
    Command                                                   \
  }

#define THROW_SKIP_FILE(EXCEPTION, MESSAGE) \
  throw EScpSkipFile(EXCEPTION, MESSAGE)
#define THROW_SKIP_FILE_NULL THROW_SKIP_FILE(NULL, "")

/* TODO : Better user interface (query to user) */
#define FILE_OPERATION_LOOP_CUSTOM(TERMINAL, ALLOW_SKIP, MESSAGE, OPERATION) { \
  bool DoRepeat; \
  do { \
    DoRepeat = false; \
    try { \
      OPERATION;                                                            \
    }                                                                       \
    catch (EAbort & E)                                                      \
    {                                                                       \
      throw;                                                                \
    }                                                                       \
    catch (EScpSkipFile & E)                                                \
    {                                                                       \
      throw;                                                                \
    }                                                                       \
    catch (EFatal & E)                                                      \
    {                                                                       \
      throw;                                                                \
    }                                                                       \
    catch (Exception & E)                                                   \
    {                                                                       \
      TERMINAL->FileOperationLoopQuery(E, OperationProgress, MESSAGE, ALLOW_SKIP); \
      DoRepeat = true;                                                      \
    } \
  } while (DoRepeat); }

#define FILE_OPERATION_LOOP(MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_EX(True, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
enum TCurrentFSProtocol { cfsUnknown, cfsSCP, cfsSFTP, cfsFTP };
//---------------------------------------------------------------------------
const cpDelete = 0x01;
const cpTemporary = 0x04;
const cpNoConfirmation = 0x08;
const cpNewerOnly = 0x10;
//---------------------------------------------------------------------------
const ccApplyToDirectories = 0x01;
const ccRecursive = 0x02;
const ccUser = 0x100;
//---------------------------------------------------------------------------
const csIgnoreErrors = 0x01;
//---------------------------------------------------------------------------
const ropNoReadDirectory = 0x02;
//---------------------------------------------------------------------------
const boDisableNeverShowAgain = 0x01;
//---------------------------------------------------------------------------
class TTerminal : public TObject, public TSessionUI
{
public:
  // TScript::SynchronizeProc relies on the order
  enum TSynchronizeMode { smRemote, smLocal, smBoth };
  static const spDelete = 0x01; // cannot be combined with spTimestamp
  static const spNoConfirmation = 0x02; // has no effect for spTimestamp
  static const spExistingOnly = 0x04; // is implicit for spTimestamp
  static const spNoRecurse = 0x08;
  static const spUseCache = 0x10; // cannot be combined with spTimestamp
  static const spDelayProgress = 0x20; // cannot be combined with spTimestamp
  static const spPreviewChanges = 0x40; // not used by core
  static const spSubDirs = 0x80; // cannot be combined with spTimestamp
  static const spTimestamp = 0x100;
  static const spNotByTime = 0x200; // cannot be combined with spTimestamp and smBoth
  static const spBySize = 0x400; // cannot be combined with smBoth, has opposite meaning for spTimestamp
  // 0x800 is reserved for GUI (spSelectedOnly)
  static const spMirror = 0x1000;

// for TranslateLockedPath()
friend class TRemoteFile;
// for ReactOnCommand()
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
friend class TFTPFileSystem;
friend class TTunnelUI;
friend class TCallbackGuard;

private:
  TSessionData * FSessionData;
  TSessionLog * FLog;
  TConfiguration * FConfiguration;
  AnsiString FCurrentDirectory;
  AnsiString FLockDirectory;
  Integer FExceptionOnFail;
  TRemoteDirectory * FFiles;
  int FInTransaction;
  TNotifyEvent FOnChangeDirectory;
  TReadDirectoryEvent FOnReadDirectory;
  TNotifyEvent FOnStartReadDirectory;
  TReadDirectoryProgressEvent FOnReadDirectoryProgress;
  TDeleteLocalFileEvent FOnDeleteLocalFile;
  TRemoteTokenList FMembership;
  TRemoteTokenList FGroups;
  TRemoteTokenList FUsers;
  bool FUsersGroupsLookedup;
  TFileOperationProgressEvent FOnProgress;
  TFileOperationFinished FOnFinished;
  TFileOperationProgressType * FOperationProgress;
  bool FUseBusyCursor;
  TRemoteDirectoryCache * FDirectoryCache;
  TRemoteDirectoryChangesCache * FDirectoryChangesCache;
  TCustomFileSystem * FFileSystem;
  TSecureShell * FSecureShell;
  AnsiString FLastDirectoryChange;
  TCurrentFSProtocol FFSProtocol;
  TTerminal * FCommandSession;
  bool FAutoReadDirectory;
  bool FReadingCurrentDirectory;
  bool * FClosedOnCompletion;
  TSessionStatus FStatus;
  AnsiString FPassword;
  TTunnelThread * FTunnelThread;
  TSecureShell * FTunnel;
  TSessionData * FTunnelData;
  TSessionLog * FTunnelLog;
  TTunnelUI * FTunnelUI;
  int FTunnelLocalPortNumber;
  AnsiString FTunnelError;
  bool FTunnelOpening;
  TQueryUserEvent FOnQueryUser;
  TPromptUserEvent FOnPromptUser;
  TDisplayBannerEvent FOnDisplayBanner;
  TExtendedExceptionEvent FOnShowExtendedException;
  TInformationEvent FOnInformation;
  TNotifyEvent FOnClose;
  bool FAnyInformation;
  TCallbackGuard * FCallbackGuard;

  void __fastcall CommandError(Exception * E, const AnsiString Msg);
  int __fastcall CommandError(Exception * E, const AnsiString Msg, int Answers);
  AnsiString __fastcall PeekCurrentDirectory();
  AnsiString __fastcall GetCurrentDirectory();
  bool __fastcall GetExceptionOnFail() const;
  const TRemoteTokenList * __fastcall GetGroups();
  const TRemoteTokenList * __fastcall GetUsers();
  const TRemoteTokenList * __fastcall GetMembership();
  void __fastcall SetCurrentDirectory(AnsiString value);
  void __fastcall SetExceptionOnFail(bool value);
  void __fastcall ReactOnCommand(int /*TFSCommand*/ Cmd);
  AnsiString __fastcall GetUserName() const;
  bool __fastcall GetAreCachesEmpty() const;
  void __fastcall ClearCachedFileList(const AnsiString Path, bool SubDirs);
  void __fastcall AddCachedFileList(TRemoteFileList * FileList);
  bool __fastcall GetCommandSessionOpened();
  TTerminal * __fastcall GetCommandSession();
  bool __fastcall GetResolvingSymlinks();
  bool __fastcall GetActive();
  AnsiString __fastcall GetPassword();
  bool __fastcall GetStoredCredentialsTried();

protected:
  bool FReadCurrentDirectoryPending;
  bool FReadDirectoryPending;

  void __fastcall DoStartReadDirectory();
  void __fastcall DoReadDirectoryProgress(int Progress, bool & Cancel);
  void __fastcall DoReadDirectory(bool ReloadOnly);
  void __fastcall DoCreateDirectory(const AnsiString DirName);
  void __fastcall DoDeleteFile(const AnsiString FileName, const TRemoteFile * File,
    int Params);
  void __fastcall DoCustomCommandOnFile(AnsiString FileName,
    const TRemoteFile * File, AnsiString Command, int Params, TCaptureOutputEvent OutputEvent);
  void __fastcall DoRenameFile(const AnsiString FileName,
    const AnsiString NewName, bool Move);
  void __fastcall DoCopyFile(const AnsiString FileName, const AnsiString NewName);
  void __fastcall DoChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties);
  void __fastcall DoChangeDirectory();
  void __fastcall EnsureNonExistence(const AnsiString FileName);
  void __fastcall LookupUsersGroups();
  void __fastcall FileModified(const TRemoteFile * File,
    const AnsiString FileName, bool ClearDirectoryChange = false);
  int __fastcall FileOperationLoop(TFileOperationEvent CallBackFunc,
    TFileOperationProgressType * OperationProgress, bool AllowSkip,
    const AnsiString Message, void * Param1 = NULL, void * Param2 = NULL);
  bool __fastcall GetIsCapable(TFSCapability Capability) const;
  bool __fastcall ProcessFiles(TStrings * FileList, TFileOperation Operation,
    TProcessFileEvent ProcessFile, void * Param = NULL, TOperationSide Side = osRemote,
    bool Ex = false);
  bool __fastcall ProcessFilesEx(TStrings * FileList, TFileOperation Operation,
    TProcessFileEventEx ProcessFile, void * Param = NULL, TOperationSide Side = osRemote);
  void __fastcall ProcessDirectory(const AnsiString DirName,
    TProcessFileEvent CallBackFunc, void * Param = NULL, bool UseCache = false);
  void __fastcall AnnounceFileListOperation();
  AnsiString __fastcall TranslateLockedPath(AnsiString Path, bool Lock);
  void __fastcall ReadDirectory(TRemoteFileList * FileList);
  void __fastcall CustomReadDirectory(TRemoteFileList * FileList);
  void __fastcall DoCreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  bool __fastcall CreateLocalFile(const AnsiString FileName,
    TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
    bool NoConfirmation);
  void __fastcall OpenLocalFile(const AnsiString FileName, int Access,
    int * Attrs, HANDLE * Handle, __int64 * ACTime, __int64 * MTime,
    __int64 * ATime, __int64 * Size, bool TryWriteReadOnly = true);
  bool __fastcall HandleException(Exception * E);
  void __fastcall CalculateFileSize(AnsiString FileName,
    const TRemoteFile * File, /*TCalculateSizeParams*/ void * Size);
  void __fastcall DoCalculateDirectorySize(const AnsiString FileName,
    const TRemoteFile * File, TCalculateSizeParams * Params);
  void __fastcall CalculateLocalFileSize(const AnsiString FileName,
    const TSearchRec Rec, /*__int64*/ void * Size);
  void __fastcall CalculateLocalFilesSize(TStrings * FileList, __int64 & Size,
    const TCopyParamType * CopyParam = NULL);
  int __fastcall ConfirmFileOverwrite(const AnsiString FileName,
    const TOverwriteFileParams * FileParams, int Answers, const TQueryParams * Params,
    TOperationSide Side, TFileOperationProgressType * OperationProgress);
  void __fastcall DoSynchronizeCollectDirectory(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory,
    TSynchronizeOptions * Options, int Level, TSynchronizeChecklist * Checklist);
  void __fastcall SynchronizeCollectFile(const AnsiString FileName,
    const TRemoteFile * File, /*TSynchronizeData*/ void * Param);
  void __fastcall SynchronizeRemoteTimestamp(const AnsiString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall SynchronizeLocalTimestamp(const AnsiString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall DoSynchronizeProgress(const TSynchronizeData & Data, bool Collect);
  void __fastcall DeleteLocalFile(AnsiString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall RecycleFile(AnsiString FileName, const TRemoteFile * File);
  TStrings * __fastcall GetFixedPaths();
  void __fastcall DoStartup();
  virtual bool __fastcall DoQueryReopen(Exception * E);
  virtual void __fastcall FatalError(Exception * E, AnsiString Msg);
  void __fastcall ResetConnection();
  virtual bool __fastcall DoPromptUser(TSessionData * Data, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions, TStrings * Prompts,
    TStrings * Response);
  void __fastcall OpenTunnel();
  void __fastcall CloseTunnel();
  void __fastcall DoInformation(const AnsiString & Str, bool Status, bool Active = true);
  AnsiString __fastcall FileUrl(const AnsiString Protocol, const AnsiString FileName);
  bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions, AnsiString Prompt, bool Echo,
    int MaxLen, AnsiString & Result);

  virtual void __fastcall Information(const AnsiString & Str, bool Status);
  virtual int __fastcall QueryUser(const AnsiString Query,
    TStrings * MoreMessages, int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation);
  virtual int __fastcall QueryUserException(const AnsiString Query,
    Exception * E, int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation);
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions, TStrings * Prompts, TStrings * Results);
  virtual void __fastcall DisplayBanner(const AnsiString & Banner);
  virtual void __fastcall Closed();
  bool __fastcall IsListenerFree(unsigned int PortNumber);
  void __fastcall DoProgress(TFileOperationProgressType & ProgressData, TCancelStatus & Cancel);
  void __fastcall DoFinished(TFileOperation Operation, TOperationSide Side, bool Temp,
    const AnsiString & FileName, bool Success, bool & DisconnectWhenComplete);
  void __fastcall RollbackAction(TSessionAction & Action,
    TFileOperationProgressType * OperationProgress, Exception * E = NULL);
  void __fastcall DoAnyCommand(const AnsiString Command, TCaptureOutputEvent OutputEvent,
    TCallSessionAction * Action);
  TRemoteFileList * DoReadDirectoryListing(AnsiString Directory, bool UseCache);

  __property TFileOperationProgressType * OperationProgress = { read=FOperationProgress };

public:
  __fastcall TTerminal(TSessionData * SessionData, TConfiguration * Configuration);
  __fastcall ~TTerminal();
  void __fastcall Open();
  void __fastcall Close();
  void __fastcall Reopen(int Params);
  virtual void __fastcall DirectoryModified(const AnsiString Path, bool SubDirs);
  virtual void __fastcall DirectoryLoaded(TRemoteFileList * FileList);
  virtual void __fastcall ShowExtendedException(Exception * E);
  void __fastcall Idle();
  bool __fastcall AllowedAnyCommand(const AnsiString Command);
  void __fastcall AnyCommand(const AnsiString Command, TCaptureOutputEvent OutputEvent);
  void __fastcall CloseOnCompletion(const AnsiString Message = "");
  AnsiString __fastcall AbsolutePath(AnsiString Path, bool Local);
  void __fastcall BeginTransaction();
  void __fastcall ReadCurrentDirectory();
  void __fastcall ReadDirectory(bool ReloadOnly, bool ForceCache = false);
  TRemoteFileList * ReadDirectoryListing(AnsiString Directory, const TFileMasks & Mask);
  TRemoteFileList * CustomReadDirectoryListing(AnsiString Directory, bool UseCache);
  void __fastcall ReadFile(const AnsiString FileName, TRemoteFile *& File);
  bool __fastcall FileExists(const AnsiString FileName);
  void __fastcall ReadSymlink(TRemoteFile * SymlinkFile, TRemoteFile *& File);
  bool __fastcall CopyToLocal(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params);
  bool __fastcall CopyToRemote(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params);
  void __fastcall CreateDirectory(const AnsiString DirName,
    const TRemoteProperties * Properties = NULL);
  void __fastcall CreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  void __fastcall DeleteFile(AnsiString FileName,
    const TRemoteFile * File = NULL, void * Params = NULL);
  bool __fastcall DeleteFiles(TStrings * FilesToDelete, int Params = 0);
  bool __fastcall DeleteLocalFiles(TStrings * FileList, int Params = 0);
  bool __fastcall IsRecycledFile(AnsiString FileName);
  void __fastcall CustomCommandOnFile(AnsiString FileName,
    const TRemoteFile * File, void * AParams);
  void __fastcall CustomCommandOnFiles(AnsiString Command, int Params,
    TStrings * Files, TCaptureOutputEvent OutputEvent);
  void __fastcall ChangeDirectory(const AnsiString Directory);
  void __fastcall EndTransaction();
  void __fastcall HomeDirectory();
  void __fastcall ChangeFileProperties(AnsiString FileName,
    const TRemoteFile * File, /*const TRemoteProperties */ void * Properties);
  void __fastcall ChangeFilesProperties(TStrings * FileList,
    const TRemoteProperties * Properties);
  bool __fastcall LoadFilesProperties(TStrings * FileList);
  void __fastcall TerminalError(AnsiString Msg);
  void __fastcall TerminalError(Exception * E, AnsiString Msg);
  void __fastcall ReloadDirectory();
  void __fastcall RefreshDirectory();
  void __fastcall RenameFile(const AnsiString FileName, const AnsiString NewName);
  void __fastcall RenameFile(const TRemoteFile * File, const AnsiString NewName, bool CheckExistence);
  void __fastcall MoveFile(const AnsiString FileName, const TRemoteFile * File,
    /*const TMoveFileParams*/ void * Param);
  bool __fastcall MoveFiles(TStrings * FileList, const AnsiString Target,
    const AnsiString FileMask);
  void __fastcall CopyFile(const AnsiString FileName, const TRemoteFile * File,
    /*const TMoveFileParams*/ void * Param);
  bool __fastcall CopyFiles(TStrings * FileList, const AnsiString Target,
    const AnsiString FileMask);
  void __fastcall CalculateFilesSize(TStrings * FileList, __int64 & Size,
    int Params, const TCopyParamType * CopyParam = NULL, TCalculateSizeStats * Stats = NULL);
  void __fastcall CalculateFilesChecksum(const AnsiString & Alg, TStrings * FileList,
    TStrings * Checksums, TCalculatedChecksumEvent OnCalculatedChecksum);
  void __fastcall ClearCaches();
  TSynchronizeChecklist * __fastcall SynchronizeCollect(const AnsiString LocalDirectory,
    const AnsiString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory, TSynchronizeOptions * Options);
  void __fastcall SynchronizeApply(TSynchronizeChecklist * Checklist,
    const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory);
  void __fastcall SpaceAvailable(const AnsiString Path, TSpaceAvailable & ASpaceAvailable);
  bool __fastcall DirectoryFileList(const AnsiString Path,
    TRemoteFileList *& FileList, bool CanLoad);
  void __fastcall MakeLocalFileList(const AnsiString FileName,
    const TSearchRec Rec, void * Param);
  AnsiString __fastcall FileUrl(const AnsiString FileName);
  bool __fastcall FileOperationLoopQuery(Exception & E,
    TFileOperationProgressType * OperationProgress, const AnsiString Message,
    bool AllowSkip, AnsiString SpecialRetry = "");
  TUsableCopyParamAttrs __fastcall UsableCopyParamAttrs(int Params);
  bool __fastcall QueryReopen(Exception * E, int Params,
    TFileOperationProgressType * OperationProgress);

  const TSessionInfo & __fastcall GetSessionInfo();
  const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve = false);
  void __fastcall inline LogEvent(const AnsiString & Str);

  static bool __fastcall IsAbsolutePath(const AnsiString Path);
  static AnsiString __fastcall ExpandFileName(AnsiString Path,
    const AnsiString BasePath);

  __property TSessionData * SessionData = { read = FSessionData };
  __property TSessionLog * Log = { read = FLog };
  __property TConfiguration * Configuration = { read = FConfiguration };
  __property bool Active = { read = GetActive };
  __property TSessionStatus Status = { read = FStatus };
  __property AnsiString CurrentDirectory = { read = GetCurrentDirectory, write = SetCurrentDirectory };
  __property bool ExceptionOnFail = { read = GetExceptionOnFail, write = SetExceptionOnFail };
  __property TRemoteDirectory * Files = { read = FFiles };
  __property TNotifyEvent OnChangeDirectory = { read = FOnChangeDirectory, write = FOnChangeDirectory };
  __property TReadDirectoryEvent OnReadDirectory = { read = FOnReadDirectory, write = FOnReadDirectory };
  __property TNotifyEvent OnStartReadDirectory = { read = FOnStartReadDirectory, write = FOnStartReadDirectory };
  __property TReadDirectoryProgressEvent OnReadDirectoryProgress = { read = FOnReadDirectoryProgress, write = FOnReadDirectoryProgress };
  __property TDeleteLocalFileEvent OnDeleteLocalFile = { read = FOnDeleteLocalFile, write = FOnDeleteLocalFile };
  __property const TRemoteTokenList * Groups = { read = GetGroups };
  __property const TRemoteTokenList * Users = { read = GetUsers };
  __property const TRemoteTokenList * Membership = { read = GetMembership };
  __property TFileOperationProgressEvent OnProgress  = { read=FOnProgress, write=FOnProgress };
  __property TFileOperationFinished OnFinished  = { read=FOnFinished, write=FOnFinished };
  __property TCurrentFSProtocol FSProtocol = { read = FFSProtocol };
  __property bool UseBusyCursor = { read = FUseBusyCursor, write = FUseBusyCursor };
  __property AnsiString UserName = { read=GetUserName };
  __property bool IsCapable[TFSCapability Capability] = { read = GetIsCapable };
  __property bool AreCachesEmpty = { read = GetAreCachesEmpty };
  __property bool CommandSessionOpened = { read = GetCommandSessionOpened };
  __property TTerminal * CommandSession = { read = GetCommandSession };
  __property bool AutoReadDirectory = { read = FAutoReadDirectory, write = FAutoReadDirectory };
  __property TStrings * FixedPaths = { read = GetFixedPaths };
  __property bool ResolvingSymlinks = { read = GetResolvingSymlinks };
  __property AnsiString Password = { read = GetPassword };
  __property bool StoredCredentialsTried = { read = GetStoredCredentialsTried };
  __property TQueryUserEvent OnQueryUser = { read = FOnQueryUser, write = FOnQueryUser };
  __property TPromptUserEvent OnPromptUser = { read = FOnPromptUser, write = FOnPromptUser };
  __property TDisplayBannerEvent OnDisplayBanner = { read = FOnDisplayBanner, write = FOnDisplayBanner };
  __property TExtendedExceptionEvent OnShowExtendedException = { read = FOnShowExtendedException, write = FOnShowExtendedException };
  __property TInformationEvent OnInformation = { read = FOnInformation, write = FOnInformation };
  __property TNotifyEvent OnClose = { read = FOnClose, write = FOnClose };
  __property int TunnelLocalPortNumber = { read = FTunnelLocalPortNumber };
};
//---------------------------------------------------------------------------
class TSecondaryTerminal : public TTerminal
{
public:
  __fastcall TSecondaryTerminal(TTerminal * MainTerminal,
    TSessionData * SessionData, TConfiguration * Configuration,
    const AnsiString & Name);

protected:
  virtual void __fastcall DirectoryLoaded(TRemoteFileList * FileList);
  virtual void __fastcall DirectoryModified(const AnsiString Path,
    bool SubDirs);
  virtual bool __fastcall DoPromptUser(TSessionData * Data, TPromptKind Kind,
    AnsiString Name, AnsiString Instructions, TStrings * Prompts, TStrings * Results);

private:
  bool FMasterPasswordTried;
  TTerminal * FMainTerminal;
};
//---------------------------------------------------------------------------
class TTerminalList : public TObjectList
{
public:
  __fastcall TTerminalList(TConfiguration * AConfiguration);
  __fastcall ~TTerminalList();

  virtual TTerminal * __fastcall NewTerminal(TSessionData * Data);
  virtual void __fastcall FreeTerminal(TTerminal * Terminal);
  void __fastcall FreeAndNullTerminal(TTerminal * & Terminal);
  virtual void __fastcall Idle();

  __property TTerminal * Terminals[int Index]  = { read=GetTerminal };
  __property int ActiveCount = { read = GetActiveCount };

protected:
  virtual TTerminal * __fastcall CreateTerminal(TSessionData * Data);

private:
  TConfiguration * FConfiguration;

  TTerminal * __fastcall GetTerminal(int Index);
  int __fastcall GetActiveCount();
};
//---------------------------------------------------------------------------
struct TCustomCommandParams
{
  AnsiString Command;
  int Params;
  TCaptureOutputEvent OutputEvent;
};
//---------------------------------------------------------------------------
struct TCalculateSizeStats
{
  TCalculateSizeStats();

  int Files;
  int Directories;
  int SymLinks;
};
//---------------------------------------------------------------------------
struct TCalculateSizeParams
{
  __int64 Size;
  int Params;
  const TCopyParamType * CopyParam;
  TCalculateSizeStats * Stats;
};
//---------------------------------------------------------------------------
struct TOverwriteFileParams
{
  TOverwriteFileParams();

  __int64 SourceSize;
  __int64 DestSize;
  TDateTime SourceTimestamp;
  TDateTime DestTimestamp;
  TModificationFmt SourcePrecision;
  TModificationFmt DestPrecision;
};
//---------------------------------------------------------------------------
struct TMakeLocalFileListParams
{
  TStrings * FileList;
  bool IncludeDirs;
  bool Recursive;
};
//---------------------------------------------------------------------------
struct TSynchronizeOptions
{
  TSynchronizeOptions();
  ~TSynchronizeOptions();

  TStringList * Filter;
};
//---------------------------------------------------------------------------
class TSynchronizeChecklist
{
friend class TTerminal;

public:
  enum TAction { saNone, saUploadNew, saDownloadNew, saUploadUpdate,
    saDownloadUpdate, saDeleteRemote, saDeleteLocal };
  static const int ActionCount = saDeleteLocal;

  class TItem
  {
  friend class TTerminal;

  public:
    struct TFileInfo
    {
      AnsiString FileName;
      AnsiString Directory;
      TDateTime Modification;
      TModificationFmt ModificationFmt;
      __int64 Size;
    };

    TAction Action;
    bool IsDirectory;
    TFileInfo Local;
    TFileInfo Remote;
    int ImageIndex;
    bool Checked;

    const AnsiString& GetFileName() const;

    ~TItem();

  private:
    TRemoteFile * FRemoteFile;
    FILETIME FLocalLastWriteTime;

    TItem();
  };

  ~TSynchronizeChecklist();

  __property int Count = { read = GetCount };
  __property const TItem * Item[int Index] = { read = GetItem };

protected:
  TSynchronizeChecklist();

  void Sort();
  void Add(TItem * Item);

  int GetCount() const;
  const TItem * GetItem(int Index) const;

private:
  TList * FList;

  static int __fastcall Compare(void * Item1, void * Item2);
};
//---------------------------------------------------------------------------
struct TSpaceAvailable
{
  TSpaceAvailable();

  __int64 BytesOnDevice;
  __int64 UnusedBytesOnDevice;
  __int64 BytesAvailableToUser;
  __int64 UnusedBytesAvailableToUser;
  unsigned long BytesPerAllocationUnit;
};
//---------------------------------------------------------------------------
#endif
