//------------------------------------------------------------------------------
#ifndef S3FileSystemH
#define S3FileSystemH
//------------------------------------------------------------------------------
#include <FileSystems.h>
//------------------------------------------------------------------------------
struct TNeonCertificateData;
struct TOverwriteFileParams;
struct TLibS3CallbackData;
struct TLibS3BucketContext;
struct TLibS3ListBucketCallbackData;
struct TLibS3TransferObjectDataCallbackData;
struct TLibS3PutObjectDataCallbackData;
struct TLibS3GetObjectDataCallbackData;
struct ssl_st;
struct TS3FileProperties;
#ifdef NEED_LIBS3
// resolve clash
#define S3Protocol _S3Protocol
#include "libs3.h"
#undef S3Protocol
#else
struct ne_session_s;
struct ne_ssl_certificate_s;
struct S3ResponseProperties;
struct S3RequestContext;
struct S3ErrorDetails;
struct S3ListBucketContent;
struct S3ResponseHandler;
struct S3AclGrant;
enum S3Status { };
enum _S3Protocol { };
enum S3Permission { };
#endif
//------------------------------------------------------------------------------
class TS3FileSystem : public TCustomFileSystem
{
public:
  explicit TS3FileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TS3FileSystem();

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
  virtual void __fastcall Sink(
    const UnicodeString & FileName, const TRemoteFile * File,
    const UnicodeString & TargetDir, UnicodeString & DestFileName, int Attrs,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
    unsigned int Flags, TDownloadSessionAction & Action);
  virtual void __fastcall CreateDirectory(const UnicodeString & DirName, bool Encrypt);
  virtual void __fastcall CreateLink(const UnicodeString FileName, const UnicodeString PointTo, bool Symbolic);
  virtual void __fastcall DeleteFile(const UnicodeString FileName,
    const TRemoteFile * File, int Params,
    TRmSessionAction & Action);
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
  virtual void __fastcall ReadSymlink(TRemoteFile * SymLinkFile,
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
  bool FActive;
  TFileSystemInfo FFileSystemInfo;
  UnicodeString FCurrentDirectory;
  UnicodeString FCachedDirectoryChange;
  TSessionInfo FSessionInfo;
  UTF8String FAccessKeyId;
  UTF8String FSecretAccessKey;
  UTF8String FSecurityTokenBuf;
  const char * FSecurityToken;
  UTF8String FHostName;
  UTF8String FPortSuffix;
  int FTimeout;
  S3RequestContext * FRequestContext;
  _S3Protocol FLibS3Protocol;
  ne_session_s * FNeonSession;
  UnicodeString FTlsVersionStr;
  UnicodeString FResponse;
  bool FResponseIgnore;
  typedef std::map<UnicodeString, UnicodeString> TRegions;
  TRegions FRegions;
  TRegions FHostNames;
  UnicodeString FAuthRegion;

  virtual UnicodeString __fastcall GetCurrentDirectory();

  void LibS3Deinitialize();
  bool VerifyCertificate(TNeonCertificateData Data);
  void CollectTLSSessionInfo();
  void CheckLibS3Error(const TLibS3CallbackData & Data, bool FatalOnConnectError = false);
  void InitSslSession(ssl_st * Ssl, ne_session_s * Session);
  void RequestInit(TLibS3CallbackData & Data);
  void TryOpenDirectory(const UnicodeString & Directory);
  void ReadDirectoryInternal(const UnicodeString & Path, TRemoteFileList * FileList, int MaxKeys, const UnicodeString & FileName);
  void ParsePath(UnicodeString Path, UnicodeString & BucketName, UnicodeString & Key);
  TRemoteToken MakeRemoteToken(const char * OwnerId, const char * OwnerDisplayName);
  TLibS3BucketContext GetBucketContext(const UnicodeString & BucketName, const UnicodeString & Prefix);
  void DoListBucket(
    const UnicodeString & Prefix, TRemoteFileList * FileList, int MaxKeys, const TLibS3BucketContext & BucketContext,
    TLibS3ListBucketCallbackData & Data);
  UnicodeString GetFolderKey(const UnicodeString & Key);
  void HandleNonBucketStatus(TLibS3CallbackData & Data, bool & Retry);
  void DoReadFile(const UnicodeString & FileName, TRemoteFile *& File);
  void ConfirmOverwrite(
    const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
    TFileOperationProgressType * OperationProgress, const TOverwriteFileParams * FileParams,
    const TCopyParamType * CopyParam, int Params);
  int PutObjectData(int BufferSize, char * Buffer, TLibS3PutObjectDataCallbackData & Data);
  S3Status GetObjectData(int BufferSize, const char * Buffer, TLibS3GetObjectDataCallbackData & Data);
  bool ShouldCancelTransfer(TLibS3TransferObjectDataCallbackData & Data);
  bool IsGoogleCloud();
  void __fastcall LoadFileProperties(const UnicodeString AFileName, const TRemoteFile * File, void * Param);
  bool DoLoadFileProperties(
    const UnicodeString & AFileName, const TRemoteFile * File, TS3FileProperties & Properties, bool LoadTags);
  unsigned short AclGrantToPermissions(S3AclGrant & AclGrant, const TS3FileProperties & Properties);
  bool ParsePathForPropertiesRequests(
    const UnicodeString & Path, const TRemoteFile * File, UnicodeString & BucketName, UnicodeString & Key);
  void AssumeRole(const UnicodeString & RoleArn);
  void SetCredentials(const UnicodeString & AccessKeyId, const UnicodeString & SecretAccessKey, const UnicodeString & SessionToken);

  static TS3FileSystem * GetFileSystem(void * CallbackData);
  static void LibS3SessionCallback(ne_session_s * Session, void * CallbackData);
  static S3Status LibS3ResponsePropertiesCallback(const S3ResponseProperties * Properties, void * CallbackData);
  static void LibS3ResponseCompleteCallback(S3Status Status, const S3ErrorDetails * Error, void * CallbackData);
  static int LibS3SslCallback(int Failures, const ne_ssl_certificate_s * Certificate, void * CallbackData);
  static void LibS3ResponseDataCallback(const char * Data, size_t Size, void * CallbackData);
  static S3Status LibS3ListServiceCallback(
    const char * OwnerId, const char * OwnerDisplayName, const char * BucketName,
    int64_t CreationDate, void * CallbackData);
  static S3Status LibS3ListBucketCallback(
    int IsTruncated, const char * NextMarker, int ContentsCount, const S3ListBucketContent * Contents,
    int CommonPrefixesCount, const char ** CommonPrefixes, void * CallbackData);
  static int LibS3PutObjectDataCallback(int BufferSize, char * Buffer, void * CallbackData);
  static S3Status LibS3MultipartInitialCallback(const char * UploadId, void * CallbackData);
  static int LibS3MultipartCommitPutObjectDataCallback(int BufferSize, char * Buffer, void * CallbackData);
  static S3Status LibS3MultipartResponsePropertiesCallback(const S3ResponseProperties * Properties, void * CallbackData);
  static S3Status LibS3GetObjectDataCallback(int BufferSize, const char * Buffer, void * CallbackData);
  static S3Status LibS3XmlDataCallback(int BufferSize, const char * Buffer, void * CallbackData);
  static int LibS3XmlDataToCallback(int BufferSize, char * Buffer, void * CallbackData);

  static const int S3MinMultiPartChunkSize;
  static const int S3MaxMultiPartChunks;
};
//------------------------------------------------------------------------------
UnicodeString __fastcall S3LibVersion();
UnicodeString __fastcall S3LibDefaultHostName();
UnicodeString __fastcall S3LibDefaultRegion();
bool IsAmazonS3SessionData(TSessionData * Data);
TStrings * GetS3Profiles();
UnicodeString S3EnvUserName(const UnicodeString & Profile, UnicodeString * Source = NULL, bool OnlyCached = false);
UnicodeString S3EnvPassword(const UnicodeString & Profile, UnicodeString * Source = NULL, bool OnlyCached = false);
UnicodeString S3EnvSessionToken(const UnicodeString & Profile, UnicodeString * Source = NULL, bool OnlyCached = false);
UnicodeString S3EnvRoleArn(const UnicodeString & Profile, UnicodeString * Source = NULL, bool OnlyCached = false);
//------------------------------------------------------------------------------
#endif
