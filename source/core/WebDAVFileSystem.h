//------------------------------------------------------------------------------
#ifndef WebDavFileSystemH
#define WebDavFileSystemH

//------------------------------------------------------------------------------
#include <ne_uri.h>
#include <ne_utils.h>
#include <ne_string.h>
#include <ne_request.h>
#include <FileSystems.h>
//------------------------------------------------------------------------------
struct TWebDAVCertificateData;
struct ne_ssl_certificate_s;
struct ne_session_s;
struct ne_prop_result_set_s;
struct TOverwriteFileParams;
struct ssl_st;
//------------------------------------------------------------------------------
class TWebDAVFileSystem : public TCustomFileSystem
{
public:
  explicit TWebDAVFileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TWebDAVFileSystem();

  virtual void __fastcall Open();
  virtual void __fastcall Close();
  virtual bool __fastcall GetActive();
  virtual void __fastcall CollectUsage();
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
  virtual TStrings * __fastcall GetFixedPaths();
  virtual void __fastcall SpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & ASpaceAvailable);
  virtual const TSessionInfo & __fastcall GetSessionInfo();
  virtual const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve);
  virtual bool __fastcall TemporaryTransferFile(const UnicodeString & FileName);
  virtual bool __fastcall GetStoredCredentialsTried();
  virtual UnicodeString __fastcall GetUserName();
  virtual void __fastcall GetSupportedChecksumAlgs(TStrings * Algs);

  void __fastcall NeonDebug(const UnicodeString & Message);
  void __fastcall InitSslSession(ssl_st * Ssl);

protected:
  virtual UnicodeString __fastcall GetCurrentDirectory();

  void __fastcall Sink(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TDownloadSessionAction & Action, bool & ChildError);
  void __fastcall SinkRobust(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SinkFile(const UnicodeString FileName, const TRemoteFile * File, void * Param);
  void __fastcall SourceRobust(const UnicodeString FileName,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall Source(const UnicodeString FileName,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TUploadSessionAction & Action, bool & ChildError);
  void __fastcall DirectorySource(const UnicodeString DirectoryName,
    const UnicodeString TargetDir, int Attrs, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall ConfirmOverwrite(const UnicodeString & FullFileName, UnicodeString & FileName,
    TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
    int Params);
  void __fastcall CheckStatus(int NeonStatus);
  void __fastcall ClearNeonError();
  static void NeonPropsResult(
    void * UserData, const ne_uri * Uri, const ne_prop_result_set_s * Results);
  void __fastcall ParsePropResultSet(TRemoteFile * File,
    const UnicodeString & Path, const ne_prop_result_set_s * Results);
  void __fastcall TryOpenDirectory(UnicodeString Directory);
  static int NeonBodyReader(void * UserData, const char * Buf, size_t Len);
  static void NeonPreSend(ne_request * Request, void * UserData, ne_buffer * Header);
  static int NeonBodyAccepter(void * UserData, ne_request * Request, const ne_status * Status);
  static void NeonCreateRequest(ne_request * Request, void * UserData, const char * Method, const char * Uri);
  static int NeonRequestAuth(void * UserData, const char * Realm, int Attempt, char * UserName, char * Password);
  void NeonOpen(UnicodeString & CorrectedUrl, const UnicodeString & Url);
  void NeonClientOpenSessionInternal(UnicodeString & CorrectedUrl, UnicodeString Url);
  static int NeonProxyAuth(
    void * UserData, const char * Realm, int Attempt, char * UserName, char * Password);
  static void NeonNotifier(void * UserData, ne_session_status Status, const ne_session_status_info * StatusInfo);
  static ssize_t NeonUploadBodyProvider(void * UserData, char * Buffer, size_t BufLen);
  static int NeonPostSend(ne_request * Req, void * UserData, const ne_status * Status);
  void ExchangeCapabilities(const char * Path, UnicodeString & CorrectedUrl);
  static int NeonServerSSLCallback(void * UserData, int Failures, const struct ne_ssl_certificate_s * Certificate);
  void __fastcall CloseNeonSession();
  bool __fastcall CancelTransfer();
  UnicodeString __fastcall GetNeonError();
  static void NeonQuotaResult(void * UserData, const ne_uri * Uri, const ne_prop_result_set_s * Results);
  static const char * __fastcall GetProp(const ne_prop_result_set_s * Results, const char * Name);

private:
  TFileSystemInfo FFileSystemInfo;
  UnicodeString FCurrentDirectory;
  UnicodeString FCachedDirectoryChange;
  TSessionInfo FSessionInfo;
  UnicodeString FUserName;
  bool FActive;
  bool FHasTrailingSlash;
  bool FCancelled;
  bool FStoredPasswordTried;
  bool FUploading;
  bool FDownloading;
  ne_session_s * FNeonSession;
  bool FInitialHandshake;
  bool FAuthenticationRequested;
  UnicodeString FResponse;
  RawByteString FPassword;
  UnicodeString FTlsVersionStr;
  enum TIgnoreAuthenticationFailure { iafNo, iafWaiting, iafPasswordFailed } FIgnoreAuthenticationFailure;

  void __fastcall CustomReadFile(UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  int __fastcall CustomReadFileInternal(const UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  void __fastcall RegisterForDebug();
  void __fastcall UnregisterFromDebug();
  bool VerifyCertificate(
    const TWebDAVCertificateData & Data);
  void OpenUrl(const UnicodeString & Url);
  void __fastcall CollectTLSSessionInfo();
  UnicodeString __fastcall GetRedirectUrl();
  UnicodeString __fastcall ParsePathFromUrl(const UnicodeString & Url);
  int __fastcall ReadDirectoryInternal(const UnicodeString & Path, TRemoteFileList * FileList);
  int __fastcall RenameFileInternal(const UnicodeString & FileName, const UnicodeString & NewName);
  bool __fastcall IsValidRedirect(int NeonStatus, UnicodeString & Path);
  UnicodeString __fastcall DirectoryPath(UnicodeString Path);
};
//------------------------------------------------------------------------------
void __fastcall NeonInitialize();
void __fastcall NeonFinalize();
//------------------------------------------------------------------------------
#endif
