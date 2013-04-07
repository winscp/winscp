#ifndef WebDavFileSystemH
#define WebDavFileSystemH

#include <apr_pools.h>

#include <FileSystems.h>
#include "Terminal.h"

//------------------------------------------------------------------------------
struct TListDataEntry;
struct TFileTransferData;
//------------------------------------------------------------------------------
namespace webdav {
  struct session_t;
  typedef int error_t;
} // namespace webdav
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
class TWebDAVFileSystem : public TCustomFileSystem
{
  friend class TWebDAVFileListHelper;

public:
  explicit TWebDAVFileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TWebDAVFileSystem();

  virtual void __fastcall Open();
  virtual void __fastcall Close();
  virtual bool __fastcall GetActive();
  virtual void __fastcall Idle();
  virtual UnicodeString __fastcall AbsolutePath(UnicodeString Path, bool Local);
  virtual void __fastcall AnyCommand(const UnicodeString Command,
    TCaptureOutputEvent OutputEvent);
  virtual void __fastcall ChangeDirectory(const UnicodeString Directory);
  virtual void __fastcall CachedChangeDirectory(const UnicodeString Directory);
  virtual void __fastcall AnnounceFileListOperation();
  virtual void __fastcall ChangeFileProperties(const UnicodeString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties,
    TChmodSessionAction & Action);
  virtual bool __fastcall LoadFilesProperties(TStrings * FileList);
  virtual void __fastcall CalculateFilesChecksum(const UnicodeString & Alg,
    TStrings * FileList, TStrings * Checksums,
    TCalculatedChecksumEvent OnCalculatedChecksum);
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall CreateDirectory(const UnicodeString DirName);
  virtual void __fastcall CreateLink(const UnicodeString FileName, const UnicodeString PointTo, bool Symbolic);
  virtual void __fastcall DeleteFile(const UnicodeString FileName,
    const TRemoteFile * File, int Params, TRmSessionAction & Action);
  virtual void __fastcall CustomCommandOnFile(const UnicodeString FileName,
    const TRemoteFile * File, UnicodeString Command, int Params, TCaptureOutputEvent OutputEvent);
  virtual void __fastcall DoStartup();
  virtual void __fastcall HomeDirectory();
  virtual bool __fastcall IsCapable(int Capability) const;
  virtual void __fastcall LookupUsersGroups();
  virtual void __fastcall ReadCurrentDirectory();
  virtual void __fastcall ReadDirectory(TRemoteFileList * FileList);
  virtual void __fastcall ReadFile(const UnicodeString FileName,
    TRemoteFile *& File);
  virtual void __fastcall ReadSymlink(TRemoteFile * SymlinkFile,
    TRemoteFile *& File);
  virtual void __fastcall RenameFile(const UnicodeString FileName,
    const UnicodeString NewName);
  virtual void __fastcall CopyFile(const UnicodeString FileName,
    const UnicodeString NewName);
  virtual UnicodeString __fastcall FileUrl(const UnicodeString FileName);
  virtual TStrings * __fastcall GetFixedPaths();
  virtual void __fastcall SpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & ASpaceAvailable);
  virtual const TSessionInfo & __fastcall GetSessionInfo();
  virtual const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve);
  virtual bool __fastcall TemporaryTransferFile(const UnicodeString & FileName);
  virtual bool __fastcall GetStoredCredentialsTried();
  virtual UnicodeString __fastcall GetUserName();

public:
  virtual void __fastcall ReadDirectoryProgress(__int64 Bytes);
  virtual void __fastcall FileTransferProgress(__int64 TransferSize, __int64 Bytes);

protected:
  enum TOverwriteMode { omOverwrite, omResume };

  virtual UnicodeString __fastcall GetCurrentDirectory();

  bool __fastcall HandleListData(const wchar_t * Path, const TListDataEntry * Entries,
    unsigned int Count);
  void __fastcall EnsureLocation();
  void __fastcall DoChangeDirectory(const UnicodeString Directory);

  void __fastcall Sink(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TDownloadSessionAction & Action);
  void __fastcall SinkRobust(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SinkFile(const UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall WebDAVSourceRobust(const UnicodeString FileName,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall WebDAVSource(const UnicodeString FileName,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TUploadSessionAction & Action);
  void __fastcall WebDAVDirectorySource(const UnicodeString DirectoryName,
    const UnicodeString TargetDir, int Attrs, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags);
  bool __fastcall ConfirmOverwrite(UnicodeString & FileName,
    TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
    int Params, bool AutoResume, unsigned int &Answer);
  void __fastcall ResetFileTransfer();
  void __fastcall DoFileTransferProgress(__int64 TransferSize, __int64 Bytes);
  void __fastcall DoReadDirectory(TRemoteFileList * FileList);
  void __fastcall FileTransfer(const UnicodeString FileName, const UnicodeString LocalFile,
    const UnicodeString RemoteFile, const UnicodeString RemotePath, bool Get,
    __int64 Size, int Type, TFileTransferData & UserData,
    TFileOperationProgressType * OperationProgress);

private:
  TFileSystemInfo FFileSystemInfo;
  UnicodeString FCurrentDirectory;
  TRemoteFileList * FFileList;
  UnicodeString FCachedDirectoryChange;
  TCaptureOutputEvent FOnCaptureOutput;
  TSessionInfo FSessionInfo;
  UnicodeString FUserName;
  bool FPasswordFailed;
  bool FActive;
  enum { ftaNone, ftaSkip, ftaCancel } FFileTransferAbort;
  bool FIgnoreFileList;
  bool FFileTransferCancelled;
  __int64 FFileTransferResumed;
  bool FFileTransferPreserveTime;
  bool FHasTrailingSlash;
  size_t FFileTransferCPSLimit;
  size_t FLastReadDirectoryProgress;
  TFileOperationProgressType * FCurrentOperationProgress;
  TCriticalSection * FTransferStatusCriticalSection;

private:
  void __fastcall CustomReadFile(const UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  bool SendPropFindRequest(const wchar_t * dir, int & responseCode);

private:
  bool WebDAVCheckExisting(const wchar_t * path, int & is_dir);
  bool WebDAVMakeDirectory(const wchar_t * path);
  bool WebDAVGetList(const UnicodeString Directory);
  bool WebDAVGetFile(const wchar_t * remotePath, HANDLE * LocalFileHandle);
  bool WebDAVPutFile(const wchar_t * remotePath, const wchar_t * localPath, const unsigned __int64 fileSize);
  bool WebDAVRenameFile(const wchar_t * srcPath, const wchar_t * dstPath);
  bool WebDAVDeleteFile(const wchar_t * path);

public:
  webdav::error_t GetServerSettings(
    int * proxy_method,
    const char **proxy_host,
    unsigned int *proxy_port,
    const char **proxy_username,
    const char **proxy_password,
    int *timeout_seconds,
    int *neon_debug,
    const char **neon_debug_file_name,
    bool *compression,
    const char **pk11_provider,
    const char **ssl_authority_file,
    apr_pool_t *pool);
  webdav::error_t VerifyCertificate(
    const char * Prompt, const char *fingerprint,
    unsigned int & RequestResult);
  webdav::error_t AskForClientCertificateFilename(
    const char **cert_file, unsigned int & RequestResult,
    apr_pool_t *pool);
  webdav::error_t AskForUsername(
    const char **user_name,
    unsigned int & RequestResult,
    apr_pool_t *pool);
  webdav::error_t AskForUserPassword(
    const char **password,
    unsigned int & RequestResult,
    apr_pool_t *pool);
  webdav::error_t AskForPassphrase(
    const char **passphrase,
    const char *realm,
    unsigned int & RequestResult,
    apr_pool_t *pool);
  webdav::error_t SimplePrompt(
    const char *prompt_text,
    const char *prompt_string,
    unsigned int & RequestResult);
  webdav::error_t CreateStorage(THierarchicalStorage *& Storage);
  unsigned long AdjustToCPSLimit(unsigned long len);
  bool GetIsCancelled();
private:
  webdav::error_t OpenURL(const UnicodeString & repos_URL,
    apr_pool_t *pool);
private:
  apr_pool_t *webdav_pool;
  webdav::session_t *FSession;
private:
  TWebDAVFileSystem(const TWebDAVFileSystem &);
  TWebDAVFileSystem & operator=(const TWebDAVFileSystem &);
};

#endif
