//---------------------------------------------------------------------------
#ifndef TerminalH
#define TerminalH

#include <Classes.hpp>

#include "SessionInfo.h"
#include "Interface.h"
#include "FileOperationProgress.h"
#include "FileMasks.h"
#include "RemoteFiles.h"
#include "Exceptions.h"
//---------------------------------------------------------------------------
class TCopyParamType;
class TFileOperationProgressType;
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
struct TFilesFindParams;
class TTunnelUI;
class TCallbackGuard;
class TParallelOperation;
class TCollectedFileList;
struct TLocalFileHandle;
typedef std::vector<__int64> TCalculatedSizes;
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TQueryUserEvent)
  (TObject * Sender, const UnicodeString Query, TStrings * MoreMessages, unsigned int Answers,
   const TQueryParams * Params, unsigned int & Answer, TQueryType QueryType, void * Arg);
typedef void __fastcall (__closure *TPromptUserEvent)
  (TTerminal * Terminal, TPromptKind Kind, UnicodeString Name, UnicodeString Instructions,
   TStrings * Prompts, TStrings * Results, bool & Result, void * Arg);
typedef void __fastcall (__closure *TDisplayBannerEvent)
  (TTerminal * Terminal, UnicodeString SessionName, const UnicodeString & Banner,
   bool & NeverShowAgain, int Options, unsigned int & Params);
typedef void __fastcall (__closure *TExtendedExceptionEvent)
  (TTerminal * Terminal, Exception * E, void * Arg);
typedef void __fastcall (__closure *TReadDirectoryEvent)(System::TObject * Sender, Boolean ReloadOnly);
typedef void __fastcall (__closure *TReadDirectoryProgressEvent)(
  System::TObject* Sender, int Progress, int ResolvedLinks, bool & Cancel);
typedef void __fastcall (__closure *TProcessFileEvent)
  (const UnicodeString FileName, const TRemoteFile * File, void * Param);
typedef void __fastcall (__closure *TProcessFileEventEx)
  (const UnicodeString FileName, const TRemoteFile * File, void * Param, int Index);
typedef int __fastcall (__closure *TFileOperationEvent)
  (void * Param1, void * Param2);
typedef void __fastcall (__closure *TSynchronizeDirectory)
  (const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
   bool & Continue, bool Collect);
typedef void __fastcall (__closure *TUpdatedSynchronizationChecklistItems)(
  const TSynchronizeChecklist::TItemList & Items);
typedef void __fastcall (__closure *TProcessedSynchronizationChecklistItem)(
  void * Token, const TSynchronizeChecklist::TItem * Item);
typedef void __fastcall (__closure *TDeleteLocalFileEvent)(
  const UnicodeString FileName, bool Alternative, int & Deleted);
typedef int __fastcall (__closure *TDirectoryModifiedEvent)
  (TTerminal * Terminal, const UnicodeString Directory, bool SubDirs);
typedef void __fastcall (__closure *TInformationEvent)
  (TTerminal * Terminal, const UnicodeString & Str, bool Status, int Phase);
typedef void __fastcall (__closure *TCustomCommandEvent)
  (TTerminal * Terminal, const UnicodeString & Command, bool & Handled);
//---------------------------------------------------------------------------
const unsigned int folNone = 0x00;
const unsigned int folAllowSkip = 0x01;
const unsigned int folRetryOnFatal = 0x02;

/* TODO : Better user interface (query to user) */
#define FILE_OPERATION_LOOP_BEGIN \
  { \
    bool DoRepeat; \
    do \
    { \
      DoRepeat = false; \
      try \

#define FILE_OPERATION_LOOP_END_CUSTOM(MESSAGE, FLAGS, HELPKEYWORD) \
      catch (Exception & E) \
      { \
        FILE_OPERATION_LOOP_TERMINAL->FileOperationLoopEnd(E, OperationProgress, MESSAGE, FLAGS, L"", HELPKEYWORD); \
        DoRepeat = true; \
      } \
    } while (DoRepeat); \
  }

#define FILE_OPERATION_LOOP_END_EX(MESSAGE, FLAGS) \
  FILE_OPERATION_LOOP_END_CUSTOM(MESSAGE, FLAGS, L"")
#define FILE_OPERATION_LOOP_END(MESSAGE) \
  FILE_OPERATION_LOOP_END_EX(MESSAGE, folAllowSkip)
//---------------------------------------------------------------------------
enum TCurrentFSProtocol { cfsUnknown, cfsSCP, cfsSFTP, cfsFTP, cfsWebDAV, cfsS3 };
//---------------------------------------------------------------------------
const int cpDelete = 0x01;
const int cpTemporary = 0x04;
const int cpNoConfirmation = 0x08;
const int cpAppend = 0x20;
const int cpResume = 0x40;
const int cpNoRecurse = 0x80;
//---------------------------------------------------------------------------
const int ccApplyToDirectories = 0x01;
const int ccRecursive = 0x02;
const int ccUser = 0x100;
//---------------------------------------------------------------------------
const int csIgnoreErrors = 0x01;
const int csStopOnFirstFile = 0x02;
const int csDisallowTemporaryTransferFiles = 0x04;
//---------------------------------------------------------------------------
const int ropNoReadDirectory = 0x02;
//---------------------------------------------------------------------------
const int boDisableNeverShowAgain = 0x01;
const int bpMonospacedFont = 0x01;
//---------------------------------------------------------------------------
const int tfNone = 0x00;
const int tfFirstLevel = 0x01;
const int tfNewDirectory = 0x02;
const int tfAutoResume = 0x04;
const int tfPreCreateDir = 0x08;
const int tfUseFileTransferAny = 0x10;
//---------------------------------------------------------------------------
class TTerminal : public TObject, public TSessionUI
{
public:
  // TScript::SynchronizeProc relies on the order
  enum TSynchronizeMode { smRemote, smLocal, smBoth };
  static const int spDelete = 0x01; // cannot be combined with spTimestamp
  static const int spNoConfirmation = 0x02; // has no effect for spTimestamp
  static const int spExistingOnly = 0x04; // is implicit for spTimestamp
  static const int spNoRecurse = 0x08;
  static const int spUseCache = 0x10; // cannot be combined with spTimestamp
  static const int spDelayProgress = 0x20; // cannot be combined with spTimestamp
  static const int spPreviewChanges = 0x40; // not used by core
  static const int spSubDirs = 0x80; // cannot be combined with spTimestamp
  static const int spTimestamp = 0x100;
  static const int spNotByTime = 0x200; // cannot be combined with spTimestamp and smBoth
  static const int spBySize = 0x400; // cannot be combined with smBoth, has opposite meaning for spTimestamp
  static const int spSelectedOnly = 0x800; // not used by core
  static const int spMirror = 0x1000;
  static const int spDefault = TTerminal::spNoConfirmation | TTerminal::spPreviewChanges;

// for TranslateLockedPath()
friend class TRemoteFile;
// for ReactOnCommand()
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
friend class TFTPFileSystem;
friend class TWebDAVFileSystem;
friend class TS3FileSystem;
friend class TTunnelUI;
friend class TCallbackGuard;
friend class TSecondaryTerminal;
friend class TRetryOperationLoop;

private:
  TSessionData * FSessionData;
  TSessionLog * FLog;
  TActionLog * FActionLog;
  TConfiguration * FConfiguration;
  UnicodeString FCurrentDirectory;
  UnicodeString FLockDirectory;
  Integer FExceptionOnFail;
  TRemoteDirectory * FFiles;
  int FInTransaction;
  bool FSuspendTransaction;
  TNotifyEvent FOnChangeDirectory;
  TReadDirectoryEvent FOnReadDirectory;
  TNotifyEvent FOnStartReadDirectory;
  TReadDirectoryProgressEvent FOnReadDirectoryProgress;
  TDeleteLocalFileEvent FOnDeleteLocalFile;
  TNotifyEvent FOnInitializeLog;
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
  TSecureShell * FSecureShell;
  UnicodeString FLastDirectoryChange;
  TCurrentFSProtocol FFSProtocol;
  TTerminal * FCommandSession;
  bool FAutoReadDirectory;
  bool FReadingCurrentDirectory;
  bool * FClosedOnCompletion;
  TSessionStatus FStatus;
  int FOpening;
  RawByteString FRememberedPassword;
  TPromptKind FRememberedPasswordKind;
  RawByteString FRememberedTunnelPassword;
  TTunnelThread * FTunnelThread;
  TSecureShell * FTunnel;
  TSessionData * FTunnelData;
  TSessionLog * FTunnelLog;
  TTunnelUI * FTunnelUI;
  int FTunnelLocalPortNumber;
  UnicodeString FTunnelError;
  TQueryUserEvent FOnQueryUser;
  TPromptUserEvent FOnPromptUser;
  TDisplayBannerEvent FOnDisplayBanner;
  TExtendedExceptionEvent FOnShowExtendedException;
  TInformationEvent FOnInformation;
  TCustomCommandEvent FOnCustomCommand;
  TNotifyEvent FOnClose;
  TCallbackGuard * FCallbackGuard;
  TFindingFileEvent FOnFindingFile;
  bool FEnableSecureShellUsage;
  bool FCollectFileSystemUsage;
  bool FRememberedPasswordTried;
  bool FRememberedTunnelPasswordTried;
  int FNesting;
  UnicodeString FFingerprintScannedSHA256;
  UnicodeString FFingerprintScannedMD5;
  DWORD FLastProgressLogged;
  UnicodeString FDestFileName;
  bool FMultipleDestinationFiles;
  bool FFileTransferAny;
  typedef std::map<UnicodeString, UnicodeString> TEncryptedFileNames;
  TEncryptedFileNames FEncryptedFileNames;
  std::set<UnicodeString> FFoldersScannedForEncryptedFiles;
  RawByteString FEncryptKey;
  TFileOperationProgressType::TPersistence * FOperationProgressPersistence;
  TOnceDoneOperation FOperationProgressOnceDoneOperation;

  void __fastcall CommandError(Exception * E, const UnicodeString Msg);
  unsigned int __fastcall CommandError(Exception * E, const UnicodeString Msg,
    unsigned int Answers, const UnicodeString HelpKeyword = L"");
  UnicodeString __fastcall GetCurrentDirectory();
  bool __fastcall GetExceptionOnFail() const;
  const TRemoteTokenList * __fastcall GetGroups();
  const TRemoteTokenList * __fastcall GetUsers();
  const TRemoteTokenList * __fastcall GetMembership();
  void __fastcall SetCurrentDirectory(UnicodeString value);
  void __fastcall SetExceptionOnFail(bool value);
  void __fastcall ReactOnCommand(int /*TFSCommand*/ Cmd);
  UnicodeString __fastcall GetUserName() const;
  bool __fastcall GetAreCachesEmpty() const;
  void __fastcall ClearCachedFileList(const UnicodeString Path, bool SubDirs);
  void __fastcall AddCachedFileList(TRemoteFileList * FileList);
  bool __fastcall GetCommandSessionOpened();
  TTerminal * __fastcall GetCommandSession();
  bool __fastcall GetResolvingSymlinks();
  bool __fastcall GetActive();
  UnicodeString __fastcall GetPassword();
  UnicodeString __fastcall GetRememberedPassword();
  UnicodeString __fastcall GetRememberedTunnelPassword();
  bool __fastcall GetStoredCredentialsTried();
  inline bool __fastcall InTransaction();
  void __fastcall SaveCapabilities(TFileSystemInfo & FileSystemInfo);
  void __fastcall CreateTargetDirectory(const UnicodeString & DirectoryPath, int Attrs, const TCopyParamType * CopyParam);
  static UnicodeString __fastcall SynchronizeModeStr(TSynchronizeMode Mode);
  static UnicodeString __fastcall SynchronizeParamsStr(int Params);

protected:
  bool FReadCurrentDirectoryPending;
  bool FReadDirectoryPending;
  bool FTunnelOpening;
  TCustomFileSystem * FFileSystem;

  void __fastcall DoStartReadDirectory();
  void __fastcall DoReadDirectoryProgress(int Progress, int ResolvedLinks, bool & Cancel);
  void __fastcall DoReadDirectory(bool ReloadOnly);
  void __fastcall DoCreateDirectory(const UnicodeString & DirName, bool Encrypt);
  void __fastcall DoDeleteFile(const UnicodeString FileName, const TRemoteFile * File,
    int Params);
  void __fastcall DoCustomCommandOnFile(UnicodeString FileName,
    const TRemoteFile * File, UnicodeString Command, int Params, TCaptureOutputEvent OutputEvent);
  void __fastcall DoRenameFile(const UnicodeString FileName, const TRemoteFile * File,
    const UnicodeString NewName, bool Move);
  void __fastcall DoCopyFile(const UnicodeString FileName, const TRemoteFile * File, const UnicodeString NewName);
  void __fastcall DoChangeFileProperties(const UnicodeString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties);
  void __fastcall DoChangeDirectory();
  void __fastcall DoInitializeLog();
  void __fastcall EnsureNonExistence(const UnicodeString FileName);
  void __fastcall LookupUsersGroups();
  void __fastcall FileModified(const TRemoteFile * File,
    const UnicodeString FileName, bool ClearDirectoryChange = false);
  int __fastcall FileOperationLoop(TFileOperationEvent CallBackFunc,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    const UnicodeString Message, void * Param1 = NULL, void * Param2 = NULL);
  bool __fastcall GetIsCapable(TFSCapability Capability) const;
  bool __fastcall ProcessFiles(TStrings * FileList, TFileOperation Operation,
    TProcessFileEvent ProcessFile, void * Param = NULL, TOperationSide Side = osRemote,
    bool Ex = false);
  bool __fastcall ProcessFilesEx(TStrings * FileList, TFileOperation Operation,
    TProcessFileEventEx ProcessFile, void * Param = NULL, TOperationSide Side = osRemote);
  void __fastcall ProcessDirectory(const UnicodeString DirName,
    TProcessFileEvent CallBackFunc, void * Param = NULL, bool UseCache = false,
    bool IgnoreErrors = false);
  bool __fastcall DeleteContentsIfDirectory(
    const UnicodeString & FileName, const TRemoteFile * File, int Params, TRmSessionAction & Action);
  void __fastcall AnnounceFileListOperation();
  UnicodeString __fastcall TranslateLockedPath(UnicodeString Path, bool Lock);
  void __fastcall ReadDirectory(TRemoteFileList * FileList);
  void __fastcall CustomReadDirectory(TRemoteFileList * FileList);
  void __fastcall DoCreateLink(const UnicodeString FileName, const UnicodeString PointTo, bool Symbolic);
  bool __fastcall CreateLocalFile(const UnicodeString FileName,
    TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
    bool NoConfirmation);
  void __fastcall OpenLocalFile(const UnicodeString FileName, unsigned int Access,
    int * Attrs, HANDLE * Handle, __int64 * ACTime, __int64 * MTime,
    __int64 * ATime, __int64 * Size, bool TryWriteReadOnly = true);
  void __fastcall OpenLocalFile(
    const UnicodeString & FileName, unsigned int Access, TLocalFileHandle & Handle, bool TryWriteReadOnly = true);
  bool __fastcall AllowLocalFileTransfer(
    const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
    const TCopyParamType * CopyParam, TFileOperationProgressType * OperationProgress);
  bool __fastcall HandleException(Exception * E);
  void __fastcall CalculateFileSize(UnicodeString FileName,
    const TRemoteFile * File, /*TCalculateSizeParams*/ void * Size);
  void __fastcall DoCalculateFileSize(UnicodeString FileName,
    const TRemoteFile * File, void * Param);
  bool __fastcall DoCalculateDirectorySize(const UnicodeString & FileName, TCalculateSizeParams * Params);
  void __fastcall CalculateLocalFileSize(
    const UnicodeString & FileName, const TSearchRecSmart & Rec, /*__int64*/ void * Size);
  TBatchOverwrite __fastcall EffectiveBatchOverwrite(
    const UnicodeString & SourceFullFileName, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, bool Special);
  bool __fastcall CheckRemoteFile(
    const UnicodeString & FileName, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress);
  unsigned int __fastcall ConfirmFileOverwrite(
    const UnicodeString & SourceFullFileName, const UnicodeString & TargetFileName,
    const TOverwriteFileParams * FileParams, unsigned int Answers, TQueryParams * QueryParams,
    TOperationSide Side, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, UnicodeString Message = L"");
  void __fastcall DoSynchronizeCollectDirectory(const UnicodeString LocalDirectory,
    const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory,
    TSynchronizeOptions * Options, int Level, TSynchronizeChecklist * Checklist);
  bool __fastcall LocalFindFirstLoop(const UnicodeString & Directory, TSearchRecChecked & SearchRec);
  bool __fastcall LocalFindNextLoop(TSearchRecChecked & SearchRec);
  bool __fastcall DoAllowLocalFileTransfer(
    const UnicodeString & FileName, const TSearchRecSmart & SearchRec, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles);
  bool __fastcall DoAllowRemoteFileTransfer(
    const TRemoteFile * File, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles);
  bool __fastcall IsEmptyLocalDirectory(
    const UnicodeString & LocalDirectory, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles);
  bool __fastcall IsEmptyRemoteDirectory(
    const TRemoteFile * File, const TCopyParamType * CopyParam, bool DisallowTemporaryTransferFiles);
  void __fastcall DoSynchronizeCollectFile(const UnicodeString FileName,
    const TRemoteFile * File, /*TSynchronizeData*/ void * Param);
  void __fastcall SynchronizeCollectFile(const UnicodeString FileName,
    const TRemoteFile * File, /*TSynchronizeData*/ void * Param);
  void __fastcall SynchronizeRemoteTimestamp(const UnicodeString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall SynchronizeLocalTimestamp(const UnicodeString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall DoSynchronizeProgress(const TSynchronizeData & Data, bool Collect);
  void __fastcall DeleteLocalFile(UnicodeString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall RecycleFile(UnicodeString FileName, const TRemoteFile * File);
  TStrings * __fastcall GetFixedPaths();
  void __fastcall DoStartup();
  virtual bool __fastcall DoQueryReopen(Exception * E);
  virtual void __fastcall FatalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword = L"");
  void __fastcall ResetConnection();
  virtual bool __fastcall DoPromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, TStrings * Prompts,
    TStrings * Response);
  void __fastcall OpenTunnel();
  void __fastcall CloseTunnel();
  void __fastcall DoInformation(const UnicodeString & Str, bool Status, int Phase = -1);
  bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, UnicodeString Prompt, bool Echo,
    int MaxLen, UnicodeString & Result);
  void __fastcall FileFind(UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall DoFilesFind(UnicodeString Directory, TFilesFindParams & Params, UnicodeString RealDirectory);
  bool __fastcall DoCreateLocalFile(const UnicodeString FileName,
    TFileOperationProgressType * OperationProgress, HANDLE * AHandle,
    bool NoConfirmation);
  void __fastcall LockFile(const UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall UnlockFile(const UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall DoLockFile(const UnicodeString & FileName, const TRemoteFile * File);
  void __fastcall DoUnlockFile(const UnicodeString & FileName, const TRemoteFile * File);
  void __fastcall OperationFinish(
    TFileOperationProgressType * Progress, const void * Item, const UnicodeString & FileName,
    bool Success, TOnceDoneOperation & OnceDoneOperation);
  void __fastcall OperationStart(
    TFileOperationProgressType & Progress, TFileOperation Operation, TOperationSide Side, int Count);
  void __fastcall OperationStart(
    TFileOperationProgressType & Progress, TFileOperation Operation, TOperationSide Side, int Count,
    bool Temp, const UnicodeString & Directory, unsigned long CPSLimit);
  void __fastcall OperationStop(TFileOperationProgressType & Progress);
  virtual void __fastcall Information(const UnicodeString & Str, bool Status);
  virtual unsigned int __fastcall QueryUser(const UnicodeString Query,
    TStrings * MoreMessages, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation);
  virtual unsigned int __fastcall QueryUserException(const UnicodeString Query,
    Exception * E, unsigned int Answers, const TQueryParams * Params,
    TQueryType QueryType = qtConfirmation);
  virtual bool __fastcall PromptUser(TSessionData * Data, TPromptKind Kind,
    UnicodeString Name, UnicodeString Instructions, TStrings * Prompts, TStrings * Results);
  virtual void __fastcall DisplayBanner(const UnicodeString & Banner);
  virtual void __fastcall Closed();
  virtual void __fastcall ProcessGUI();
  void __fastcall Progress(TFileOperationProgressType * OperationProgress);
  virtual void __fastcall HandleExtendedException(Exception * E);
  bool __fastcall IsListenerFree(unsigned int PortNumber);
  void __fastcall DoProgress(TFileOperationProgressType & ProgressData);
  void __fastcall DoFinished(TFileOperation Operation, TOperationSide Side, bool Temp,
    const UnicodeString & FileName, bool Success, TOnceDoneOperation & OnceDoneOperation);
  void __fastcall RollbackAction(TSessionAction & Action,
    TFileOperationProgressType * OperationProgress, Exception * E = NULL);
  void __fastcall DoAnyCommand(const UnicodeString Command, TCaptureOutputEvent OutputEvent,
    TCallSessionAction * Action);
  TRemoteFileList * __fastcall DoReadDirectoryListing(UnicodeString Directory, bool UseCache);
  RawByteString __fastcall EncryptPassword(const UnicodeString & Password);
  UnicodeString __fastcall DecryptPassword(const RawByteString & Password);
  UnicodeString __fastcall GetRemoteFileInfo(TRemoteFile * File);
  void __fastcall LogRemoteFile(TRemoteFile * File);
  UnicodeString __fastcall FormatFileDetailsForLog(const UnicodeString & FileName, TDateTime Modification, __int64 Size);
  void __fastcall LogFileDetails(const UnicodeString & FileName, TDateTime Modification, __int64 Size);
  void __fastcall LogFileDone(TFileOperationProgressType * OperationProgress, const UnicodeString & DestFileName);
  void __fastcall LogTotalTransferDetails(
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    TFileOperationProgressType * OperationProgress, bool Parallel, TStrings * Files);
  void __fastcall LogTotalTransferDone(TFileOperationProgressType * OperationProgress);
  virtual TTerminal * __fastcall GetPasswordSource();
  void __fastcall DoEndTransaction(bool Inform);
  bool  __fastcall VerifyCertificate(
    const UnicodeString & CertificateStorageKey, const UnicodeString & SiteKey,
    const UnicodeString & Fingerprint,
    const UnicodeString & CertificateSubject, int Failures);
  void __fastcall CacheCertificate(const UnicodeString & CertificateStorageKey,
    const UnicodeString & SiteKey, const UnicodeString & Fingerprint, int Failures);
  bool __fastcall ConfirmCertificate(
    TSessionInfo & SessionInfo, int Failures, const UnicodeString & CertificateStorageKey, bool CanRemember);
  void __fastcall CollectTlsUsage(const UnicodeString & TlsVersionStr);
  bool __fastcall LoadTlsCertificate(X509 *& Certificate, EVP_PKEY *& PrivateKey);
  bool __fastcall TryStartOperationWithFile(
    const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2 = foNone);
  void __fastcall StartOperationWithFile(
    const UnicodeString & FileName, TFileOperation Operation1, TFileOperation Operation2 = foNone);
  bool __fastcall CanRecurseToDirectory(const TRemoteFile * File);
  bool __fastcall DoOnCustomCommand(const UnicodeString & Command);
  bool __fastcall CanParallel(const TCopyParamType * CopyParam, int Params, TParallelOperation * ParallelOperation);
  void __fastcall CopyParallel(TParallelOperation * ParallelOperation, TFileOperationProgressType * OperationProgress);
  void __fastcall DoCopyToRemote(
    TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags, TOnceDoneOperation & OnceDoneOperation);
  void __fastcall SourceRobust(
    const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
    const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall Source(
    const UnicodeString & FileName, const TSearchRecSmart * SearchRec,
    const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags, TUploadSessionAction & Action, bool & ChildError);
  void __fastcall DirectorySource(
    const UnicodeString & DirectoryName, const UnicodeString & TargetDir, const UnicodeString & DestDirectoryName,
    int Attrs, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SelectTransferMode(
    const UnicodeString & BaseFileName, TOperationSide Side, const TCopyParamType * CopyParam,
    const TFileMasks::TParams & MaskParams);
  void __fastcall SelectSourceTransferMode(const TLocalFileHandle & Handle, const TCopyParamType * CopyParam);
  void __fastcall UpdateSource(const TLocalFileHandle & Handle, const TCopyParamType * CopyParam, int Params);
  void __fastcall DoCopyToLocal(
    TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags, TOnceDoneOperation & OnceDoneOperation);
  void __fastcall SinkRobust(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall Sink(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & TargetDir,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TDownloadSessionAction & Action);
  void __fastcall SinkFile(UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall UpdateTargetAttrs(
    const UnicodeString & DestFullName, const TRemoteFile * File, const TCopyParamType * CopyParam, int Attrs);
  void __fastcall UpdateTargetTime(HANDLE Handle, TDateTime Modification, TDSTMode DSTMode);

  UnicodeString __fastcall EncryptFileName(const UnicodeString & Path, bool EncryptNewFiles);
  UnicodeString __fastcall DecryptFileName(const UnicodeString & Path);
  TEncryptedFileNames::const_iterator __fastcall GetEncryptedFileName(const UnicodeString & Path);
  bool __fastcall IsFileEncrypted(const UnicodeString & Path, bool EncryptNewFiles = false);

  __property TFileOperationProgressType * OperationProgress = { read=FOperationProgress };

public:
  __fastcall TTerminal(TSessionData * SessionData, TConfiguration * Configuration);
  __fastcall ~TTerminal();
  void __fastcall Open();
  void __fastcall Close();
  void __fastcall FingerprintScan(UnicodeString & SHA256, UnicodeString & MD5);
  void __fastcall Reopen(int Params);
  virtual void __fastcall DirectoryModified(const UnicodeString Path, bool SubDirs);
  virtual void __fastcall DirectoryLoaded(TRemoteFileList * FileList);
  void __fastcall ShowExtendedException(Exception * E);
  void __fastcall Idle();
  void __fastcall RecryptPasswords();
  bool __fastcall AllowedAnyCommand(const UnicodeString Command);
  void __fastcall AnyCommand(const UnicodeString Command, TCaptureOutputEvent OutputEvent);
  void __fastcall CloseOnCompletion(
    TOnceDoneOperation Operation = odoDisconnect, const UnicodeString & Message = L"",
    const UnicodeString & TargetLocalPath = L"", const UnicodeString & DestLocalFileName = L"");
  UnicodeString __fastcall AbsolutePath(UnicodeString Path, bool Local);
  void __fastcall BeginTransaction();
  void __fastcall ReadCurrentDirectory();
  void __fastcall ReadDirectory(bool ReloadOnly, bool ForceCache = false);
  TRemoteFileList * __fastcall ReadDirectoryListing(UnicodeString Directory, const TFileMasks & Mask);
  TRemoteFileList * __fastcall CustomReadDirectoryListing(UnicodeString Directory, bool UseCache);
  TRemoteFile * __fastcall ReadFileListing(UnicodeString Path);
  void __fastcall ReadFile(const UnicodeString FileName, TRemoteFile *& File);
  bool __fastcall FileExists(const UnicodeString FileName, TRemoteFile ** File = NULL);
  void __fastcall ReadSymlink(TRemoteFile * SymlinkFile, TRemoteFile *& File);
  bool __fastcall CopyToLocal(
    TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TParallelOperation * ParallelOperation);
  bool __fastcall CopyToRemote(
    TStrings * FilesToCopy, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TParallelOperation * ParallelOperation);
  int __fastcall CopyToParallel(TParallelOperation * ParallelOperation, TFileOperationProgressType * OperationProgress);
  void __fastcall LogParallelTransfer(TParallelOperation * ParallelOperation);
  void __fastcall CreateDirectory(const UnicodeString & DirName, const TRemoteProperties * Properties);
  void __fastcall CreateLink(const UnicodeString FileName, const UnicodeString PointTo, bool Symbolic);
  void __fastcall DeleteFile(UnicodeString FileName,
    const TRemoteFile * File = NULL, void * Params = NULL);
  bool __fastcall DeleteFiles(TStrings * FilesToDelete, int Params = 0);
  bool __fastcall DeleteLocalFiles(TStrings * FileList, int Params = 0);
  bool __fastcall IsRecycledFile(UnicodeString FileName);
  void __fastcall CustomCommandOnFile(UnicodeString FileName,
    const TRemoteFile * File, void * AParams);
  void __fastcall CustomCommandOnFiles(UnicodeString Command, int Params,
    TStrings * Files, TCaptureOutputEvent OutputEvent);
  void __fastcall ChangeDirectory(const UnicodeString Directory);
  void __fastcall EndTransaction();
  void __fastcall HomeDirectory();
  UnicodeString __fastcall GetHomeDirectory();
  void __fastcall ChangeFileProperties(UnicodeString FileName,
    const TRemoteFile * File, /*const TRemoteProperties */ void * Properties);
  void __fastcall ChangeFilesProperties(TStrings * FileList,
    const TRemoteProperties * Properties);
  bool __fastcall LoadFilesProperties(TStrings * FileList);
  void __fastcall TerminalError(UnicodeString Msg);
  void __fastcall TerminalError(Exception * E, UnicodeString Msg, UnicodeString HelpKeyword = L"");
  void __fastcall ReloadDirectory();
  void __fastcall RefreshDirectory();
  void __fastcall RenameFile(const TRemoteFile * File, const UnicodeString NewName, bool CheckExistence);
  void __fastcall MoveFile(const UnicodeString FileName, const TRemoteFile * File,
    /*const TMoveFileParams*/ void * Param);
  bool __fastcall MoveFiles(TStrings * FileList, const UnicodeString Target,
    const UnicodeString FileMask);
  void __fastcall CopyFile(const UnicodeString FileName, const TRemoteFile * File,
    /*const TMoveFileParams*/ void * Param);
  bool __fastcall CopyFiles(TStrings * FileList, const UnicodeString Target,
    const UnicodeString FileMask);
  bool __fastcall CalculateFilesSize(TStrings * FileList, __int64 & Size,
    int Params, const TCopyParamType * CopyParam, bool AllowDirs,
    TCalculateSizeStats & Stats);
  bool __fastcall CalculateLocalFilesSize(TStrings * FileList, __int64 & Size,
    const TCopyParamType * CopyParam, bool AllowDirs, TStrings * Files, TCalculatedSizes * CalculatedSizes);
  void __fastcall CalculateFilesChecksum(const UnicodeString & Alg, TStrings * FileList,
    TStrings * Checksums, TCalculatedChecksumEvent OnCalculatedChecksum);
  void __fastcall ClearCaches();
  TSynchronizeChecklist * __fastcall SynchronizeCollect(const UnicodeString LocalDirectory,
    const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory, TSynchronizeOptions * Options);
  void __fastcall SynchronizeApply(
    TSynchronizeChecklist * Checklist,
    const TCopyParamType * CopyParam, int Params,
    TSynchronizeDirectory OnSynchronizeDirectory, TProcessedSynchronizationChecklistItem OnProcessedItem,
    TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems, void * Token,
    TFileOperationStatistics * Statistics);
  void __fastcall SynchronizeChecklistCalculateSize(
    TSynchronizeChecklist * Checklist, const TSynchronizeChecklist::TItemList & Items,
    const TCopyParamType * CopyParam);
  void __fastcall FilesFind(UnicodeString Directory, const TFileMasks & FileMask,
    TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile);
  void __fastcall SpaceAvailable(const UnicodeString Path, TSpaceAvailable & ASpaceAvailable);
  void __fastcall LockFiles(TStrings * FileList);
  void __fastcall UnlockFiles(TStrings * FileList);
  TRemoteFileList * __fastcall DirectoryFileList(const UnicodeString Path, TDateTime Timestamp, bool CanLoad);
  void __fastcall MakeLocalFileList(
    const UnicodeString & FileName, const TSearchRecSmart & Rec, void * Param);
  bool __fastcall FileOperationLoopQuery(Exception & E,
    TFileOperationProgressType * OperationProgress, const UnicodeString Message,
    unsigned int Flags, UnicodeString SpecialRetry = L"", UnicodeString HelpKeyword = L"");
  void __fastcall FileOperationLoopEnd(Exception & E,
    TFileOperationProgressType * OperationProgress, const UnicodeString & Message,
    unsigned int Flags, const UnicodeString & SpecialRetry, const UnicodeString & HelpKeyword);
  TUsableCopyParamAttrs __fastcall UsableCopyParamAttrs(int Params);
  bool __fastcall ContinueReopen(TDateTime Start);
  bool __fastcall QueryReopen(Exception * E, int Params,
    TFileOperationProgressType * OperationProgress);
  UnicodeString __fastcall PeekCurrentDirectory();
  void __fastcall FatalAbort();
  void __fastcall ReflectSettings();
  void __fastcall CollectUsage();
  TTerminal * __fastcall CreateSecondarySession(const UnicodeString & Name, TSessionData * SessionData);
  void __fastcall FillSessionDataForCode(TSessionData * Data);
  void __fastcall UpdateSessionCredentials(TSessionData * Data);

  const TSessionInfo & __fastcall GetSessionInfo();
  const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve = false);
  void __fastcall inline LogEvent(const UnicodeString & Str);
  void __fastcall LogEvent(int Level, const UnicodeString & Str);
  void __fastcall GetSupportedChecksumAlgs(TStrings * Algs);
  UnicodeString __fastcall ChangeFileName(const TCopyParamType * CopyParam,
    UnicodeString FileName, TOperationSide Side, bool FirstLevel);
  UnicodeString __fastcall GetBaseFileName(UnicodeString FileName);
  bool __fastcall IsEncryptingFiles() const { return !FEncryptKey.IsEmpty(); }
  RawByteString __fastcall GetEncryptKey() { return FEncryptKey; }

  static UnicodeString __fastcall ExpandFileName(UnicodeString Path,
    const UnicodeString BasePath);

  __property TSessionData * SessionData = { read = FSessionData };
  __property TSessionLog * Log = { read = FLog };
  __property TActionLog * ActionLog = { read = FActionLog };
  __property TConfiguration * Configuration = { read = FConfiguration };
  __property bool Active = { read = GetActive };
  __property TSessionStatus Status = { read = FStatus };
  __property UnicodeString CurrentDirectory = { read = GetCurrentDirectory, write = SetCurrentDirectory };
  __property bool ExceptionOnFail = { read = GetExceptionOnFail, write = SetExceptionOnFail };
  __property TRemoteDirectory * Files = { read = FFiles };
  __property TNotifyEvent OnChangeDirectory = { read = FOnChangeDirectory, write = FOnChangeDirectory };
  __property TReadDirectoryEvent OnReadDirectory = { read = FOnReadDirectory, write = FOnReadDirectory };
  __property TNotifyEvent OnStartReadDirectory = { read = FOnStartReadDirectory, write = FOnStartReadDirectory };
  __property TReadDirectoryProgressEvent OnReadDirectoryProgress = { read = FOnReadDirectoryProgress, write = FOnReadDirectoryProgress };
  __property TDeleteLocalFileEvent OnDeleteLocalFile = { read = FOnDeleteLocalFile, write = FOnDeleteLocalFile };
  __property TNotifyEvent OnInitializeLog = { read = FOnInitializeLog, write = FOnInitializeLog };
  __property const TRemoteTokenList * Groups = { read = GetGroups };
  __property const TRemoteTokenList * Users = { read = GetUsers };
  __property const TRemoteTokenList * Membership = { read = GetMembership };
  __property TFileOperationProgressEvent OnProgress  = { read=FOnProgress, write=FOnProgress };
  __property TFileOperationFinished OnFinished  = { read=FOnFinished, write=FOnFinished };
  __property TCurrentFSProtocol FSProtocol = { read = FFSProtocol };
  __property bool UseBusyCursor = { read = FUseBusyCursor, write = FUseBusyCursor };
  __property UnicodeString UserName = { read=GetUserName };
  __property bool IsCapable[TFSCapability Capability] = { read = GetIsCapable };
  __property bool AreCachesEmpty = { read = GetAreCachesEmpty };
  __property bool CommandSessionOpened = { read = GetCommandSessionOpened };
  __property TTerminal * CommandSession = { read = GetCommandSession };
  __property bool AutoReadDirectory = { read = FAutoReadDirectory, write = FAutoReadDirectory };
  __property TStrings * FixedPaths = { read = GetFixedPaths };
  __property bool ResolvingSymlinks = { read = GetResolvingSymlinks };
  __property UnicodeString Password = { read = GetPassword };
  __property bool StoredCredentialsTried = { read = GetStoredCredentialsTried };
  __property TQueryUserEvent OnQueryUser = { read = FOnQueryUser, write = FOnQueryUser };
  __property TPromptUserEvent OnPromptUser = { read = FOnPromptUser, write = FOnPromptUser };
  __property TDisplayBannerEvent OnDisplayBanner = { read = FOnDisplayBanner, write = FOnDisplayBanner };
  __property TExtendedExceptionEvent OnShowExtendedException = { read = FOnShowExtendedException, write = FOnShowExtendedException };
  __property TInformationEvent OnInformation = { read = FOnInformation, write = FOnInformation };
  __property TCustomCommandEvent OnCustomCommand = { read = FOnCustomCommand, write = FOnCustomCommand };
  __property TNotifyEvent OnClose = { read = FOnClose, write = FOnClose };
  __property int TunnelLocalPortNumber = { read = FTunnelLocalPortNumber };
};
//---------------------------------------------------------------------------
class TSecondaryTerminal : public TTerminal
{
public:
  __fastcall TSecondaryTerminal(TTerminal * MainTerminal,
    TSessionData * SessionData, TConfiguration * Configuration,
    const UnicodeString & Name);

  void __fastcall UpdateFromMain();

  __property TTerminal * MainTerminal = { read = FMainTerminal };

protected:
  virtual void __fastcall DirectoryLoaded(TRemoteFileList * FileList);
  virtual void __fastcall DirectoryModified(const UnicodeString Path,
    bool SubDirs);
  virtual TTerminal * __fastcall GetPasswordSource();

private:
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
  void __fastcall RecryptPasswords();

  __property TTerminal * Terminals[int Index]  = { read=GetTerminal };

protected:
  virtual TTerminal * __fastcall CreateTerminal(TSessionData * Data);

private:
  TConfiguration * FConfiguration;

  TTerminal * __fastcall GetTerminal(int Index);
};
//---------------------------------------------------------------------------
struct TCustomCommandParams
{
  UnicodeString Command;
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
  TStrings * FoundFiles;
  TCalculatedSizes * CalculatedSizes;
};
//---------------------------------------------------------------------------
struct TCalculateSizeParams
{
  TCalculateSizeParams();

  __int64 Size;
  int Params;
  const TCopyParamType * CopyParam;
  TCalculateSizeStats * Stats;
  bool AllowDirs;
  TCollectedFileList * Files;
  UnicodeString LastDirPath;
  bool Result;
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
typedef std::vector<TDateTime> TDateTimes;
//---------------------------------------------------------------------------
struct TMakeLocalFileListParams
{
  TStrings * FileList;
  TDateTimes * FileTimes;
  bool IncludeDirs;
  bool Recursive;
};
//---------------------------------------------------------------------------
struct TSynchronizeOptions
{
  TSynchronizeOptions();
  ~TSynchronizeOptions();

  TStringList * Filter;

  bool __fastcall FilterFind(const UnicodeString & FileName);
  bool __fastcall MatchesFilter(const UnicodeString & FileName);
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
class TRobustOperationLoop
{
public:
  TRobustOperationLoop(TTerminal * Terminal, TFileOperationProgressType * OperationProgress, bool * AnyTransfer = NULL);
  ~TRobustOperationLoop();
  bool TryReopen(Exception & E);
  bool ShouldRetry();
  bool Retry();

private:
  TTerminal * FTerminal;
  TFileOperationProgressType * FOperationProgress;
  bool FRetry;
  bool * FAnyTransfer;
  bool FPrevAnyTransfer;
  TDateTime FStart;
};
//---------------------------------------------------------------------------
class TCollectedFileList : public TObject
{
public:
  TCollectedFileList();
  int Add(const UnicodeString & FileName, TObject * Object, bool Dir);
  void DidNotRecurse(int Index);
  void Delete(int Index);

  int Count() const;
  UnicodeString GetFileName(int Index) const;
  TObject * GetObject(int Index) const;
  bool IsDir(int Index) const;
  bool IsRecursed(int Index) const;

private:
  struct TFileData
  {
    UnicodeString FileName;
    TObject * Object;
    bool Dir;
    bool Recursed;
  };
  typedef std::vector<TFileData> TFileDataList;
  TFileDataList FList;
};
//---------------------------------------------------------------------------
class TParallelOperation
{
public:
  TParallelOperation(TOperationSide Side);
  ~TParallelOperation();

  void Init(
    TStrings * AFiles, const UnicodeString & TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * MainOperationProgress, const UnicodeString & MainName);

  bool IsInitialized();
  void WaitFor();
  bool ShouldAddClient();
  void AddClient();
  void RemoveClient();
  int GetNext(
    TTerminal * Terminal, UnicodeString & FileName, TObject *& Object, UnicodeString & TargetDir,
    bool & Dir, bool & Recursed);
  void Done(const UnicodeString & FileName, bool Dir, bool Success);

  __property TOperationSide Side = { read = FSide };
  __property const TCopyParamType * CopyParam = { read = FCopyParam };
  __property int Params = { read = FParams };
  __property UnicodeString TargetDir = { read = FTargetDir };
  __property TFileOperationProgressType * MainOperationProgress = { read = FMainOperationProgress };
  __property UnicodeString MainName = { read = FMainName };

private:
  struct TDirectoryData
  {
    UnicodeString OppositePath;
    bool Exists;
  };

  std::unique_ptr<TStrings> FFileList;
  int FIndex;
  typedef std::map<UnicodeString, TDirectoryData> TDirectories;
  TDirectories FDirectories;
  UnicodeString FTargetDir;
  const TCopyParamType * FCopyParam;
  int FParams;
  bool FProbablyEmpty;
  int FClients;
  std::unique_ptr<TCriticalSection> FSection;
  TFileOperationProgressType * FMainOperationProgress;
  TOperationSide FSide;
  UnicodeString FMainName;

  bool CheckEnd(TCollectedFileList * Files);
};
//---------------------------------------------------------------------------
struct TLocalFileHandle
{
  TLocalFileHandle();
  ~TLocalFileHandle();

  void Dismiss();
  void Close();
  void Release();

  UnicodeString FileName;
  HANDLE Handle;
  int Attrs;
  bool Directory;
  TDateTime Modification;
  __int64 MTime;
  __int64 ATime;
  __int64 Size;
};
//---------------------------------------------------------------------------
class TLocalFile : public TObject
{
public:
  TSearchRecSmart SearchRec;
};
//---------------------------------------------------------------------------
#endif
