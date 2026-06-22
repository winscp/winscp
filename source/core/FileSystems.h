//---------------------------------------------------------------------------
#ifndef FileSystemsH
#define FileSystemsH

#include <SessionInfo.h>
#include "Exceptions.h"
//---------------------------------------------------------------------------
class TTerminal;
class TRights;
class TRemoteFile;
class TRemoteFileList;
class TCopyParamType;
struct TSpaceAvailable;
class TFileOperationProgressType;
class TRemoteProperties;
struct TLocalFileHandle;
//---------------------------------------------------------------------------
enum TFSCommand { fsNull = 0, fsVarValue, fsLastLine, fsFirstLine,
  fsCurrentDirectory, fsChangeDirectory, fsListDirectory, fsListCurrentDirectory,
  fsListFile, fsLookupUsersGroups, fsCopyToRemote, fsCopyToLocal, fsDeleteFile,
  fsRenameFile, fsCreateDirectory, fsChangeMode, fsChangeGroup, fsChangeOwner,
  fsHomeDirectory, fsUnset, fsUnalias, fsCreateLink, fsCopyFile,
  fsAnyCommand, fsLang, fsReadSymlink, fsChangeProperties, fsMoveFile,
  fsLock };
//---------------------------------------------------------------------------
const int dfNoRecursive = 0x01;
const int dfAlternative = 0x02;
const int dfForceDelete = 0x04;
//---------------------------------------------------------------------------
class TCustomFileSystem
{
public:
  virtual __fastcall ~TCustomFileSystem();

  virtual void __fastcall Open() = 0;
  virtual void __fastcall Close() = 0;
  virtual bool __fastcall GetActive() = 0;
  virtual void __fastcall CollectUsage() = 0;
  virtual void __fastcall Idle() = 0;
  virtual UnicodeString __fastcall AbsolutePath(UnicodeString Path, bool Local) = 0;
  virtual void __fastcall AnyCommand(const UnicodeString Command,
    TCaptureOutputEvent OutputEvent) = 0;
  virtual void __fastcall ChangeDirectory(const UnicodeString Directory) = 0;
  virtual void __fastcall CachedChangeDirectory(const UnicodeString Directory) = 0;
  virtual void __fastcall AnnounceFileListOperation() = 0;
  virtual void __fastcall ChangeFileProperties(const UnicodeString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties,
    TChmodSessionAction & Action) = 0;
  virtual bool __fastcall LoadFilesProperties(TStrings * FileList) = 0;
  virtual UnicodeString CalculateFilesChecksumInitialize(const UnicodeString & Alg);
  virtual void __fastcall CalculateFilesChecksum(
    const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
    TFileOperationProgressType * OperationProgress, bool FirstLevel) = 0;
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation) = 0;
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation) = 0;
  virtual void __fastcall TransferOnDirectory(
    const UnicodeString & Directory, const TCopyParamType * CopyParam, int Params);
  virtual void __fastcall Source(
    TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TUploadSessionAction & Action, bool & ChildError) = 0;
  virtual void __fastcall DirectorySunk(
    const UnicodeString & DestFullName, const TRemoteFile * File, const TCopyParamType * CopyParam);
  virtual void __fastcall Sink(
    const UnicodeString & FileName, const TRemoteFile * File,
    const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
    unsigned int Flags, TDownloadSessionAction & Action) = 0;
  virtual void __fastcall CreateDirectory(const UnicodeString & DirName, bool Encrypt) = 0;
  virtual void __fastcall CreateLink(const UnicodeString FileName, const UnicodeString PointTo, bool Symbolic) = 0;
  virtual void __fastcall DeleteFile(const UnicodeString FileName,
    const TRemoteFile * File, int Params,
    TRmSessionAction & Action) = 0;
  virtual void __fastcall CustomCommandOnFile(const UnicodeString FileName,
    const TRemoteFile * File, UnicodeString Command, int Params, TCaptureOutputEvent OutputEvent) = 0;
  virtual void __fastcall DoStartup() = 0;
  virtual void __fastcall HomeDirectory() = 0;
  virtual UnicodeString __fastcall GetHomeDirectory();
  virtual bool __fastcall IsCapable(int Capability) const = 0;
  virtual void __fastcall LookupUsersGroups() = 0;
  virtual void __fastcall ReadCurrentDirectory() = 0;
  virtual void __fastcall ReadDirectory(TRemoteFileList * FileList) = 0;
  virtual void __fastcall ReadFile(const UnicodeString FileName,
    TRemoteFile *& File) = 0;
  virtual void __fastcall ReadSymlink(TRemoteFile * SymLinkFile,
    TRemoteFile *& File) = 0;
  virtual void __fastcall RenameFile(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Overwrite) = 0;
  virtual void __fastcall CopyFile(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Overwrite) = 0;
  virtual TStrings * __fastcall GetFixedPaths() = 0;
  virtual void __fastcall SpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & ASpaceAvailable) = 0;
  virtual const TSessionInfo & __fastcall GetSessionInfo() = 0;
  virtual const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve) = 0;
  virtual bool __fastcall TemporaryTransferFile(const UnicodeString & FileName) = 0;
  virtual bool __fastcall GetStoredCredentialsTried() = 0;
  virtual UnicodeString __fastcall GetUserName() = 0;
  virtual void __fastcall GetSupportedChecksumAlgs(TStrings * Algs) = 0;
  virtual void __fastcall LockFile(const UnicodeString & FileName, const TRemoteFile * File) = 0;
  virtual void __fastcall UnlockFile(const UnicodeString & FileName, const TRemoteFile * File) = 0;
  virtual void __fastcall UpdateFromMain(TCustomFileSystem * MainFileSystem) = 0;
  virtual void __fastcall ClearCaches() = 0;

  __property UnicodeString CurrentDirectory = { read = GetCurrentDirectory };

protected:
  TTerminal * FTerminal;

  __fastcall TCustomFileSystem(TTerminal * ATerminal);
  virtual UnicodeString __fastcall GetCurrentDirectory() = 0;
};
//---------------------------------------------------------------------------
#endif
