//---------------------------------------------------------------------------
#ifndef TerminalH
#define TerminalH

#include <Classes.hpp>

#include "FileOperationProgress.h"
#include "FileMasks.h"
#include "SecureShell.h"
//---------------------------------------------------------------------------
class TCopyParamType;
class TFileOperationProgressType;
class TRemoteDirectory;
class TRemoteFile;
class TCustomFileSystem;
typedef TStringList TUserGroupsList;
typedef void __fastcall (__closure *TReadDirectoryEvent)(System::TObject* Sender, Boolean ReloadOnly);
typedef void __fastcall (__closure *TProcessFileEvent)
  (const AnsiString FileName, const TRemoteFile * File, void * Param);
typedef int __fastcall (__closure *TFileOperationEvent)
  (void * Param1, void * Param2);
//---------------------------------------------------------------------------
#define SUSPEND_OPERATION(Command)    \
  {                                   \
    OperationProgress->Suspend();     \
    try {                             \
      Command                         \
    } __finally {                     \
      OperationProgress->Resume();    \
    }                                 \
  }

#define THROW_SKIP_FILE(EXCEPTION, MESSAGE) \
  throw EScpSkipFile(EXCEPTION, MESSAGE)

/* TODO : Better user interface (query to user) */
// FILENAME is no longer used
#define FILE_OPERATION_LOOP_EX(FILENAME, ALLOW_SKIP, MESSAGE, OPERATION) { \
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
    catch (EFatal & E)                                                \
    {                                                                       \
      throw;                                                                \
    }                                                                       \
    catch (Exception & E)                                                   \
    { \
      HandleExtendedException(&E, this); \
      int Answers = qaRetry | qaAbort | ((ALLOW_SKIP) ? qaSkip : 0); \
      int Answer; \
      int Params = !(ALLOW_SKIP) ? qpFatalAbort : 0; \
      SUSPEND_OPERATION ( \
        Answer = FTerminal->DoQueryUser(MESSAGE, &E, Answers, Params); \
      ); \
      DoRepeat = (Answer == qaRetry); \
      if (Answer == qaAbort) OperationProgress->Cancel = csCancel; \
      if (!DoRepeat && ALLOW_SKIP) THROW_SKIP_FILE(&E, MESSAGE); \
        else \
      if (!DoRepeat && !ALLOW_SKIP) throw; \
    } \
  } while (DoRepeat); }

#define FILE_OPERATION_LOOP(FILENAME, MESSAGE, OPERATION) \
  FILE_OPERATION_LOOP_EX(FILENAME, True, MESSAGE, OPERATION)
//---------------------------------------------------------------------------
enum TFSCapability { fcUserGroupListing, fcModeChanging, fcGroupChanging,
  fcOwnerChanging, fcAnyCommand, fcHardLink };
//---------------------------------------------------------------------------
const cpDelete = 1;
//const cpCheckExistence = 2; !!! don't use
const cpDragDrop = 4;
const cpTemporary = 4; // alis to cpDragDrop
//---------------------------------------------------------------------------
class TTerminal : public TSecureShell
{
// for TranslateLockedPath()
friend class TRemoteFile;
// for ReactOnCommand()
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
private:
  AnsiString FCurrentDirectory;
  AnsiString FLockDirectory;
  Integer FExceptionOnFail;
  TRemoteDirectory * FFiles;
  Integer FInTransaction;
  TNotifyEvent FOnChangeDirectory;
  TReadDirectoryEvent FOnReadDirectory;
  bool FReadCurrentDirectoryPending;
  bool FReadDirectoryPending;
  TUserGroupsList * FUserGroups;
  Boolean FUserGroupsLookedup;
  TFileOperationProgressEvent FOnProgress;
  TFileOperationFinished FOnFinished;
  TFileOperationProgressType * FOperationProgress;
  Boolean FUseBusyCursor;
  TRemoteDirectoryCache * FDirectoryCache;
  TCustomFileSystem * FFileSystem;
  void __fastcall CommandError(Exception * E, const AnsiString Msg);
  int __fastcall CommandError(Exception * E, const AnsiString Msg, int Answers);
  AnsiString __fastcall GetCurrentDirectory();
  bool __fastcall GetExceptionOnFail() const;
  AnsiString __fastcall GetProtocolName();
  TUserGroupsList * __fastcall GetUserGroups();
  void __fastcall SetCurrentDirectory(AnsiString value);
  void __fastcall SetExceptionOnFail(bool value);
  void __fastcall ReactOnCommand(int /*TFSCommand*/ Cmd);
  AnsiString __fastcall GetUserName() const;
protected:
  void __fastcall DoReadDirectory(bool ReloadOnly);
  void __fastcall DoCreateDirectory(const AnsiString DirName,
    const TRemoteProperties * Properties);
  void __fastcall DoDeleteFile(const AnsiString FileName,
    const TRemoteFile * File, void * Param);
  void __fastcall DoRenameFile(const AnsiString FileName, const AnsiString NewName);
  void __fastcall DoChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties);
  void __fastcall DoChangeDirectory();
  void __fastcall EnsureNonExistence(const AnsiString FileName);
  void __fastcall LookupUserGroups();
  void __fastcall FileModified(const TRemoteFile * File);
  int __fastcall FileOperationLoop(TFileOperationEvent CallBackFunc,
    TFileOperationProgressType * OperationProgress, bool AllowSkip,
    const AnsiString Message, void * Param1 = NULL, void * Param2 = NULL);
  bool __fastcall GetIsCapable(TFSCapability Capability) const;
  void __fastcall DirectoryModified(const AnsiString Path, bool SubDirs);
  void __fastcall ProcessFiles(TStrings * FileList,
    TFileOperation Operation, TProcessFileEvent ProcessFile, void * Param = NULL);
  void __fastcall ProcessDirectory(const AnsiString DirName,
    TProcessFileEvent CallBackFunc, void * Param = NULL);
  AnsiString __fastcall TranslateLockedPath(AnsiString Path, bool Lock);
  void __fastcall ReadDirectory(TRemoteFileList * FileList);
  void __fastcall CustomReadDirectory(TRemoteFileList * FileList);
  void __fastcall DoCreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  void __fastcall OpenLocalFile(const AnsiString FileName, int Access,
    int * Attrs, HANDLE * Handle, unsigned long * ACTime, unsigned long * MTime,
    unsigned long * ATime, __int64 * Size);
  TRemoteFileList * ReadDirectoryListing(AnsiString Directory);
  bool __fastcall HandleException(Exception * E);

  __property TFileOperationProgressType * OperationProgress = { read=FOperationProgress };

public:
  __fastcall TTerminal();
  __fastcall ~TTerminal();
  virtual void __fastcall Open();
  virtual void __fastcall Close();
  void __fastcall AnyCommand(const AnsiString Command);
  void __fastcall CloseOnCompletion();
  void __fastcall BeginTransaction();
  void __fastcall ReadCurrentDirectory();
  void __fastcall ReadDirectory(bool ReloadOnly);
  void __fastcall ReadFile(const AnsiString FileName, TRemoteFile *& File);
  void __fastcall ReadSymlink(TRemoteFile * SymlinkFile, TRemoteFile *& File);
  void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params);
  void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params);
  void __fastcall CreateDirectory(const AnsiString DirName,
    const TRemoteProperties * Properties = NULL);
  void __fastcall CreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  void __fastcall DeleteFile(AnsiString FileName,
    const TRemoteFile * File = NULL, void * Recursive = NULL);
  //void __fastcall DeleteFile(const TRemoteFile * File);
  void __fastcall DeleteFiles(TStrings * FilesToDelete, bool * Recursive = NULL);
  void __fastcall ChangeDirectory(const AnsiString Directory);
  void __fastcall DoStartup();
  void __fastcall EndTransaction();
  void __fastcall HomeDirectory();
  void __fastcall ChangeFileProperties(AnsiString FileName,
    const TRemoteFile * File, /*const TRemoteProperties */ void * Properties);
  void __fastcall ChangeFilesProperties(TStrings * FileList,
    const TRemoteProperties * Properties);
  void __fastcall TerminalError(AnsiString Msg);
  void __fastcall TerminalError(Exception * E, AnsiString Msg);
  void __fastcall ReloadDirectory();
  void __fastcall RenameFile(const AnsiString FileName, const AnsiString NewName);
  void __fastcall RenameFile(const TRemoteFile * File, const AnsiString NewName, bool CheckExistence);
  __property AnsiString CurrentDirectory = { read = GetCurrentDirectory, write = SetCurrentDirectory };
  __property Boolean ExceptionOnFail = { read = GetExceptionOnFail, write = SetExceptionOnFail };
  __property TRemoteDirectory * Files = { read = FFiles };
  __property TNotifyEvent OnChangeDirectory = { read = FOnChangeDirectory, write = FOnChangeDirectory };
  __property TReadDirectoryEvent OnReadDirectory = { read = FOnReadDirectory, write = FOnReadDirectory };
  __property TUserGroupsList * UserGroups = { read = GetUserGroups };
  __property TFileOperationProgressEvent OnProgress  = { read=FOnProgress, write=FOnProgress };
  __property TFileOperationFinished OnFinished  = { read=FOnFinished, write=FOnFinished };
  __property AnsiString ProtocolName = { read = GetProtocolName };
  __property Boolean UseBusyCursor = { read = FUseBusyCursor, write = FUseBusyCursor };
  __property AnsiString UserName  = { read=GetUserName };
  __property bool IsCapable[TFSCapability Capability] = { read = GetIsCapable };
};
//---------------------------------------------------------------------------
#endif
