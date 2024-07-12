//---------------------------------------------------------------------------
#ifndef FtpFileSystemH
#define FtpFileSystemH

//---------------------------------------------------------------------------
#include <time.h>
#include <FileSystems.h>
//---------------------------------------------------------------------------
class TFileZillaIntf;
class TFileZillaImpl;
class TMessageQueue;
class TFTPServerCapabilities;
struct TOverwriteFileParams;
struct TListDataEntry;
struct TFileTransferData;
struct TFtpsCertificateData;
struct TRemoteFileTime;
//---------------------------------------------------------------------------
class TFTPFileSystem : public TCustomFileSystem
{
friend class TFileZillaImpl;
friend class TFileListHelper;

public:
  __fastcall TFTPFileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TFTPFileSystem();

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
  virtual UnicodeString CalculateFilesChecksumInitialize(const UnicodeString & Alg);
  void __fastcall CalculateFilesChecksum(
    const UnicodeString & Alg, TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
    TFileOperationProgressType * OperationProgress, bool FirstLevel);
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall TransferOnDirectory(
    const UnicodeString & Directory, const TCopyParamType * CopyParam, int Params);
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
  enum TOverwriteMode { omOverwrite, omResume, omComplete };

  virtual UnicodeString __fastcall GetCurrentDirectory();

  const wchar_t * __fastcall GetOption(int OptionID) const;
  int __fastcall GetOptionVal(int OptionID) const;

  enum
  {
    REPLY_CONNECT =      0x01,
    REPLY_2XX_CODE =     0x02,
    REPLY_ALLOW_CANCEL = 0x04,
    REPLY_3XX_CODE =     0x08,
    REPLY_SINGLE_LINE =  0x10
  };

  bool __fastcall PostMessage(unsigned int Type, WPARAM wParam, LPARAM lParam);
  bool __fastcall ProcessMessage();
  void __fastcall DiscardMessages();
  void __fastcall WaitForMessages();
  unsigned int __fastcall WaitForReply(bool Command, bool WantLastCode);
  unsigned int __fastcall WaitForCommandReply(bool WantLastCode = true);
  void __fastcall WaitForFatalNonCommandReply();
  void __fastcall PoolForFatalNonCommandReply();
  void __fastcall GotNonCommandReply(unsigned int Reply);
  UnicodeString __fastcall GotReply(unsigned int Reply, unsigned int Flags = 0,
    UnicodeString Error = L"", unsigned int * Code = NULL,
    TStrings ** Response = NULL);
  void __fastcall ResetReply();
  void __fastcall HandleReplyStatus(UnicodeString Response);
  void __fastcall DoWaitForReply(unsigned int& ReplyToAwait, bool WantLastCode);
  bool __fastcall KeepWaitingForReply(unsigned int& ReplyToAwait, bool WantLastCode);
  inline bool __fastcall NoFinalLastCode();

  bool __fastcall HandleStatus(const wchar_t * Status, int Type);
  bool __fastcall HandleAsynchRequestOverwrite(
    wchar_t * FileName1, size_t FileName1Len, const wchar_t * FileName2,
    const wchar_t * Path1, const wchar_t * Path2,
    __int64 Size1, __int64 Size2, time_t LocalTime,
    bool HasLocalTime, const TRemoteFileTime & RemoteTime, void * UserData, int & RequestResult);
  bool __fastcall HandleAsynchRequestVerifyCertificate(
    const TFtpsCertificateData & Data, int & RequestResult);
  bool __fastcall HandleAsynchRequestNeedPass(
    struct TNeedPassRequestData & Data, int & RequestResult);
  bool __fastcall HandleListData(const wchar_t * Path, const TListDataEntry * Entries,
    unsigned int Count);
  bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, bool FileTransfer);
  bool __fastcall HandleReply(int Command, unsigned int Reply);
  bool __fastcall HandleCapabilities(TFTPServerCapabilities * ServerCapabilities);
  bool __fastcall CheckError(int ReturnCode, const wchar_t * Context);
  void __fastcall PreserveDownloadFileTime(HANDLE Handle, void * UserData);
  bool __fastcall GetFileModificationTimeInUtc(const wchar_t * FileName, struct tm & Time);
  void __fastcall EnsureLocation(const UnicodeString & Directory, bool Log);
  void __fastcall EnsureLocation();
  bool EnsureLocationWhenWorkFromCwd(const UnicodeString & Directory);
  UnicodeString __fastcall ActualCurrentDirectory();
  void __fastcall Discard();
  void __fastcall DoChangeDirectory(const UnicodeString & Directory);
  void SendCwd(const UnicodeString & Directory);

  void __fastcall Sink(const UnicodeString FileName,
    const TRemoteFile * File, const UnicodeString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TDownloadSessionAction & Action);
  bool __fastcall ConfirmOverwrite(const UnicodeString & SourceFullFileName, UnicodeString & TargetFileName,
    TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, const TCopyParamType * CopyParam,
    int Params, bool AutoResume);
  void __fastcall ReadDirectoryProgress(__int64 Bytes);
  void __fastcall ResetFileTransfer();
  void __fastcall DoFileTransferProgress(__int64 TransferSize, __int64 Bytes);
  void __fastcall FileTransferProgress(__int64 TransferSize, __int64 Bytes);
  void __fastcall ResetCaches();
  void __fastcall CaptureOutput(const UnicodeString & Str);
  void __fastcall DoReadDirectory(TRemoteFileList * FileList);
  void __fastcall DoReadFile(const UnicodeString & FileName, TRemoteFile *& AFile);
  void __fastcall FileTransfer(const UnicodeString & FileName, const UnicodeString & LocalFile,
    const UnicodeString & RemoteFile, const UnicodeString & RemotePath, bool Get,
    __int64 Size, int Type, TFileTransferData & UserData,
    TFileOperationProgressType * OperationProgress);
  TDateTime __fastcall ConvertLocalTimestamp(time_t Time);
  void __fastcall RemoteFileTimeToDateTimeAndPrecision(const TRemoteFileTime & Source,
    TDateTime & DateTime, TModificationFmt & ModificationFmt);
  void __fastcall SetLastCode(int Code);
  void __fastcall StoreLastResponse(const UnicodeString & Text);
  void __fastcall SetCPSLimit(TFileOperationProgressType * OperationProgress);
  bool __fastcall VerifyCertificateHostName(const TFtpsCertificateData & Data);
  bool __fastcall SupportsReadingFile();
  void __fastcall AutoDetectTimeDifference(TRemoteFileList * FileList);
  void __fastcall AutoDetectTimeDifference(const UnicodeString & Directory, const TCopyParamType * CopyParam, int Params);
  void __fastcall ApplyTimeDifference(TRemoteFile * File);
  void __fastcall ApplyTimeDifference(
    const UnicodeString & FileName, TDateTime & Modification, TModificationFmt & ModificationFmt);
  void __fastcall DummyReadDirectory(const UnicodeString & Directory);
  bool __fastcall IsEmptyFileList(TRemoteFileList * FileList);
  void __fastcall CheckTimeDifference();
  inline bool __fastcall NeedAutoDetectTimeDifference();
  bool __fastcall LookupUploadModificationTime(
    const UnicodeString & FileName, TDateTime & Modification, TModificationFmt ModificationFmt);
  UnicodeString __fastcall DoCalculateFileChecksum(const UnicodeString & Alg, TRemoteFile * File);
  bool UsingHashCommandChecksum(const UnicodeString & Alg);
  void __fastcall HandleFeatReply();
  void __fastcall ResetFeatures();
  void ProcessFeatures();
  bool __fastcall SupportsSiteCommand(const UnicodeString & Command) const;
  bool __fastcall SupportsCommand(const UnicodeString & Command) const;
  void __fastcall RegisterChecksumAlgCommand(const UnicodeString & Alg, const UnicodeString & Command);
  void __fastcall SendCommand(const UnicodeString & Command);
  bool __fastcall CanTransferSkipList(int Params, unsigned int Flags, const TCopyParamType * CopyParam);
  void __fastcall Disconnect();
  UnicodeString RemoteExtractFilePath(const UnicodeString & Path);

  static bool __fastcall Unquote(UnicodeString & Str);

private:
  enum TCommand
  {
    CMD_UNKNOWN,
    PASS,
    SYST,
    FEAT
  };

  TFileZillaIntf * FFileZillaIntf;
  TCriticalSection * FQueueCriticalSection;
  TCriticalSection * FTransferStatusCriticalSection;
  TMessageQueue * FQueue;
  HANDLE FQueueEvent;
  TSessionInfo FSessionInfo;
  TFileSystemInfo FFileSystemInfo;
  bool FFileSystemInfoValid;
  unsigned int FReply;
  unsigned int FCommandReply;
  TCommand FLastCommand;
  bool FAnyPassword;
  bool FPasswordFailed;
  bool FStoredPasswordTried;
  bool FMultiLineResponse;
  int FLastCode;
  int FLastCodeClass;
  DWORD FLastReadDirectoryProgress;
  UnicodeString FTimeoutStatus;
  UnicodeString FDisconnectStatus;
  TStrings * FLastResponse;
  TStrings * FLastErrorResponse;
  TStrings * FLastError;
  UnicodeString FSystem;
  UnicodeString FServerID;
  TStrings * FFeatures;
  UnicodeString FCurrentDirectory;
  bool FReadCurrentDirectory;
  UnicodeString FHomeDirectory;
  TRemoteFileList * FFileList;
  TRemoteFileList * FFileListCache;
  UnicodeString FFileListCachePath;
  UnicodeString FWelcomeMessage;
  bool FActive;
  bool FWaitingForReply;
  enum { ftaNone, ftaSkip, ftaCancel } FFileTransferAbort;
  bool FIgnoreFileList;
  bool FFileTransferCancelled;
  __int64 FFileTransferResumed;
  bool FFileTransferPreserveTime;
  bool FFileTransferRemoveBOM;
  bool FFileTransferNoList;
  unsigned long FFileTransferCPSLimit;
  bool FAwaitingProgress;
  TCaptureOutputEvent FOnCaptureOutput;
  UnicodeString FUserName;
  TAutoSwitch FListAll;
  bool FDoListAll;
  TAutoSwitch FWorkFromCwd;
  TFTPServerCapabilities * FServerCapabilities;
  TDateTime FLastDataSent;
  bool FAnyTransferSucceeded;
  bool FDetectTimeDifference;
  __int64 FTimeDifference;
  std::unique_ptr<TStrings> FChecksumAlgs;
  std::unique_ptr<TStrings> FChecksumCommands;
  std::unique_ptr<TStrings> FSupportedCommands;
  std::unique_ptr<TStrings> FSupportedSiteCommands;
  std::unique_ptr<TStrings> FHashAlgs;
  typedef std::map<UnicodeString, TDateTime> TUploadedTimes;
  TUploadedTimes FUploadedTimes;
  bool FSupportsAnyChecksumFeature;
  UnicodeString FLastCommandSent;
  X509 * FCertificate;
  EVP_PKEY * FPrivateKey;
  bool FTransferActiveImmediately;
  bool FWindowsServer;
  __int64 FBytesAvailable;
  bool FBytesAvailableSupported;
  bool FMVS;
  bool FVMS;
  bool FFileZilla;
  bool FIIS;
  bool FFileTransferAny;
  bool FLoggedIn;
  bool FVMSAllRevisions;
  bool FForceReadSymlink;
  mutable UnicodeString FOptionScratch;
};
//---------------------------------------------------------------------------
UnicodeString __fastcall GetOpenSSLVersionText();
//---------------------------------------------------------------------------
#endif // FtpFileSystemH
