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
struct TNeonCertificateData;
struct ne_ssl_certificate_s;
struct ne_session_s;
struct ne_prop_result_set_s;
struct ne_lock_store_s;
struct TOverwriteFileParams;
struct ssl_st;
struct ne_lock;
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
  virtual void __fastcall CalculateFilesChecksum(
    const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
    TFileOperationProgressType * OperationProgress, bool FirstLevel);
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall Source(
    TLocalFileHandle & Handle, const UnicodeString & TargetDir, UnicodeString & DestFileName,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TUploadSessionAction & Action, bool & ChildError);
  virtual void __fastcall CreateDirectory(const UnicodeString & DirName, bool Encrypt);
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
  virtual void __fastcall RenameFile(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Overwrite);
  virtual void __fastcall CopyFile(
    const UnicodeString & FileName, const TRemoteFile * File, const UnicodeString & NewName, bool Overwrite);
  virtual TStrings * __fastcall GetFixedPaths();
  virtual void __fastcall SpaceAvailable(const UnicodeString Path,
    TSpaceAvailable & ASpaceAvailable);
  virtual const TSessionInfo & __fastcall GetSessionInfo();
  virtual const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve);
  virtual bool __fastcall TemporaryTransferFile(const UnicodeString & FileName);
  virtual bool __fastcall GetStoredCredentialsTried();
  virtual UnicodeString __fastcall GetUserName();
  virtual void __fastcall GetSupportedChecksumAlgs(TStrings * Algs);
  virtual void __fastcall LockFile(const UnicodeString & FileName, const TRemoteFile * File);
  virtual void __fastcall UnlockFile(const UnicodeString & FileName, const TRemoteFile * File);
  virtual void __fastcall UpdateFromMain(TCustomFileSystem * MainFileSystem);
  virtual void __fastcall ClearCaches();

protected:
  virtual UnicodeString __fastcall GetCurrentDirectory();

  virtual void __fastcall Sink(
    const UnicodeString & FileName, const TRemoteFile * File,
    const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
    unsigned int Flags, TDownloadSessionAction & Action);
  void __fastcall ConfirmOverwrite(
    const UnicodeString & SourceFullFileName, UnicodeString & DestFileName,
    TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
    int Params);
  void CheckStatus(int NeonStatus);
  struct TSessionContext
  {
    TSessionContext();
    ~TSessionContext();
    TWebDAVFileSystem * FileSystem;
    ne_session_s * NeonSession; // The main one (there might be aux session for the same context)
    UnicodeString HostName;
    int PortNumber;
    bool NtlmAuthenticationFailed;
    UnicodeString AuthorizationProtocol;
  };
  void CheckStatus(TSessionContext * SessionContext, int NeonStatus);
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
  TSessionContext * NeonOpen(const UnicodeString & Url, UTF8String & Path, UTF8String & Query);
  void NeonClientOpenSessionInternal(UnicodeString & CorrectedUrl, UnicodeString Url);
  static void NeonNotifier(void * UserData, ne_session_status Status, const ne_session_status_info * StatusInfo);
  static ssize_t NeonUploadBodyProvider(void * UserData, char * Buffer, size_t BufLen);
  static int NeonPostSend(ne_request * Req, void * UserData, const ne_status * Status);
  static void NeonPostHeaders(ne_request * Req, void * UserData, const ne_status * Status);
  void ExchangeCapabilities(const char * Path, UnicodeString & CorrectedUrl);
  static int DoNeonServerSSLCallback(void * UserData, int Failures, const struct ne_ssl_certificate_s * Certificate, bool Aux);
  static int NeonServerSSLCallbackMain(void * UserData, int Failures, const struct ne_ssl_certificate_s * Certificate);
  static int NeonServerSSLCallbackAux(void * UserData, int Failures, const struct ne_ssl_certificate_s * Certificate);
  static void NeonProvideClientCert(void * UserData, ne_session * Sess, const ne_ssl_dname * const * DNames, int DNCount);
  void __fastcall CloseNeonSession();
  bool __fastcall CancelTransfer();
  UnicodeString __fastcall GetNeonError();
  static void NeonQuotaResult(void * UserData, const ne_uri * Uri, const ne_prop_result_set_s * Results);
  static const char * __fastcall GetProp(const ne_prop_result_set_s * Results,
    const char * Name, const char * NameSpace = NULL);
  static void LockResult(void * UserData, const struct ne_lock * Lock,
   const ne_uri * Uri, const ne_status * Status);
  void __fastcall RequireLockStore();
  void InitSslSession(ssl_st * Ssl, ne_session * Session);
  void __fastcall NeonAddAuthentication(TSessionContext * SessionContext, bool UseNegotiate);
  void __fastcall HttpAuthenticationFailed(TSessionContext * SessionContext);

private:
  TFileSystemInfo FFileSystemInfo;
  UnicodeString FCurrentDirectory;
  UnicodeString FCachedDirectoryChange;
  TSessionInfo FSessionInfo;
  UnicodeString FUserName;
  bool FActive;
  bool FHasTrailingSlash;
  bool FSkipped;
  bool FCancelled;
  bool FStoredPasswordTried;
  bool FUploading;
  bool FDownloading;
  UnicodeString FUploadMimeType;
  ne_lock_store_s * FNeonLockStore;
  TCriticalSection * FNeonLockStoreSection;
  bool FInitialHandshake;
  bool FAuthenticationRequested;
  UnicodeString FResponse;
  RawByteString FPassword;
  UnicodeString FTlsVersionStr;
  unsigned int FCapabilities;
  TSessionContext * FSessionContext;
  enum TIgnoreAuthenticationFailure { iafNo, iafWaiting, iafPasswordFailed } FIgnoreAuthenticationFailure;
  UnicodeString FAuthorizationProtocol;
  UnicodeString FLastAuthorizationProtocol;
  bool FAuthenticationRetry;
  bool FOneDrive;
  enum { odiUnknown, odiUpperCase, odiLowerCase } FOneDriveInterface;

  void __fastcall CustomReadFile(UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  int __fastcall CustomReadFileInternal(const UnicodeString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  bool VerifyCertificate(TSessionContext * SessionContext, TNeonCertificateData Data, bool Aux);
  void OpenUrl(const UnicodeString & Url);
  void __fastcall CollectTLSSessionInfo();
  UnicodeString __fastcall GetRedirectUrl();
  UnicodeString __fastcall ParsePathFromUrl(const UnicodeString & Url);
  int __fastcall ReadDirectoryInternal(const UnicodeString & Path, TRemoteFileList * FileList);
  int __fastcall RenameFileInternal(const UnicodeString & FileName, const UnicodeString & NewName, bool Overwrite);
  int __fastcall CopyFileInternal(const UnicodeString & FileName, const UnicodeString & NewName, bool Overwrite);
  bool IsRedirect(int NeonStatus);
  bool __fastcall IsValidRedirect(int NeonStatus, UnicodeString & Path);
  UnicodeString __fastcall DirectoryPath(UnicodeString Path);
  UnicodeString __fastcall FilePath(const TRemoteFile * File);
  struct ne_lock * __fastcall FindLock(const RawByteString & Path);
  void __fastcall DiscardLock(const RawByteString & Path);
  bool __fastcall IsNtlmAuthentication(TSessionContext * SessionContext);
  static void NeonAuxRequestInit(ne_session_s * Session, ne_request * Request, void * UserData);
  void __fastcall SetSessionTls(TSessionContext * SessionContext, ne_session_s * Session, bool Aux);
  void __fastcall InitSession(TSessionContext * SessionContext, ne_session_s * Session);
  bool IsTlsSession(ne_session * Session);
};
//------------------------------------------------------------------------------
#endif
