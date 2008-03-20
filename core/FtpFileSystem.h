//---------------------------------------------------------------------------
#ifndef FtpFileSystemH
#define FtpFileSystemH

#ifndef NO_FILEZILLA
//---------------------------------------------------------------------------
#include <time.h>
#include <FileSystems.h>
//---------------------------------------------------------------------------
class TFileZillaIntf;
class TFileZillaImpl;
class TCriticalSection;
class TMessageQueue;
class TOverwriteFileParams;
struct TListDataEntry;
struct TFileTransferData;
//---------------------------------------------------------------------------
class TFTPFileSystem : public TCustomFileSystem
{
friend class TFileZillaImpl;

public:
  __fastcall TFTPFileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TFTPFileSystem();

  virtual void __fastcall Open();
  virtual void __fastcall Close();
  virtual bool __fastcall GetActive();
  virtual void __fastcall Idle();
  virtual AnsiString __fastcall AbsolutePath(AnsiString Path);
  virtual void __fastcall AnyCommand(const AnsiString Command,
    TCaptureOutputEvent OutputEvent);
  virtual void __fastcall ChangeDirectory(const AnsiString Directory);
  virtual void __fastcall CachedChangeDirectory(const AnsiString Directory);
  virtual void __fastcall AnnounceFileListOperation();
  virtual void __fastcall ChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties);
  virtual bool __fastcall LoadFilesProperties(TStrings * FileList);
  virtual void __fastcall CalculateFilesChecksum(const AnsiString & Alg,
    TStrings * FileList, TStrings * Checksums,
    TCalculatedChecksumEvent OnCalculatedChecksum);
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    bool & DisconnectWhenComplete);
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    bool & DisconnectWhenComplete);
  virtual void __fastcall CreateDirectory(const AnsiString DirName,
    const TRemoteProperties * Properties);
  virtual void __fastcall CreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  virtual void __fastcall DeleteFile(const AnsiString FileName,
    const TRemoteFile * File = NULL, int Params = dfNoRecursive);
  virtual void __fastcall CustomCommandOnFile(const AnsiString FileName,
    const TRemoteFile * File, AnsiString Command, int Params, TCaptureOutputEvent OutputEvent);
  virtual void __fastcall DoStartup();
  virtual void __fastcall HomeDirectory();
  virtual bool __fastcall IsCapable(int Capability) const;
  virtual void __fastcall LookupUsersGroups();
  virtual void __fastcall ReadCurrentDirectory();
  virtual void __fastcall ReadDirectory(TRemoteFileList * FileList);
  virtual void __fastcall ReadFile(const AnsiString FileName,
    TRemoteFile *& File);
  virtual void __fastcall ReadSymlink(TRemoteFile * SymlinkFile,
    TRemoteFile *& File);
  virtual void __fastcall RenameFile(const AnsiString FileName,
    const AnsiString NewName);
  virtual void __fastcall CopyFile(const AnsiString FileName,
    const AnsiString NewName);
  virtual AnsiString __fastcall FileUrl(const AnsiString FileName);
  virtual TStrings * __fastcall GetFixedPaths();
  virtual void __fastcall SpaceAvailable(const AnsiString Path,
    TSpaceAvailable & ASpaceAvailable);
  virtual const TSessionInfo & __fastcall GetSessionInfo();
  virtual const TFileSystemInfo & __fastcall GetFileSystemInfo(bool Retrieve);
  virtual bool __fastcall TemporaryTransferFile(const AnsiString & FileName);
  virtual bool __fastcall GetStoredCredentialsTried();
  virtual AnsiString __fastcall GetUserName();

protected:
  enum TOverwriteMode { omOverwrite, omResume };

  virtual AnsiString __fastcall GetCurrentDirectory();

  const char * __fastcall GetOption(int OptionID) const;
  int __fastcall GetOptionVal(int OptionID) const;

  enum
  {
    REPLY_CONNECT =      0x01,
    REPLY_2XX_CODE =     0x02,
    REPLY_ALLOW_CANCEL = 0x04
  };

  bool __fastcall PostMessage(WPARAM wParam, LPARAM lParam);
  bool __fastcall ProcessMessage();
  void __fastcall DiscardMessages();
  void __fastcall WaitForMessages();
  unsigned int __fastcall WaitForReply(bool Command = true);
  unsigned int __fastcall PoolForReply();
  void __fastcall GotReply(unsigned int Reply, unsigned int Flags = 0,
    AnsiString Error = "", unsigned int * Code = NULL,
    TStrings ** Response = NULL);
  void __fastcall ResetReply();
  void __fastcall HandleReplyStatus(const char * AStatus);
  void __fastcall DoWaitForReply(unsigned int& ReplyToAwait);

  bool __fastcall HandleStatus(const char * Status, int Type);
  bool __fastcall HandleAsynchRequestOverwrite(
    char * FileName1, size_t FileName1Len, const char * FileName2,
    const char * Path1, const char * Path2,
    __int64 Size1, __int64 Size2, time_t Time1, time_t Time2,
    bool HasTime1, bool HasTime2, void * UserData, int & RequestResult);
  bool __fastcall HandleListData(const char * Path, const TListDataEntry * Entries,
    unsigned int Count);
  bool __fastcall HandleTransferStatus(bool Valid, __int64 TransferSize,
    __int64 Bytes, int Percent, int TimeElapsed, int TimeLeft, int TransferRate,
    bool FileTransfer);
  bool __fastcall HandleReply(int Command, unsigned int Reply);
  bool __fastcall CheckError(int ReturnCode, const char * Context);
  void __fastcall EnsureLocation();
  AnsiString __fastcall ActualCurrentDirectory();
  void __fastcall Discard();
  void __fastcall DoChangeDirectory(const AnsiString & Directory);

  void __fastcall Sink(const AnsiString FileName,
    const TRemoteFile * File, const AnsiString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SinkRobust(const AnsiString FileName,
    const TRemoteFile * File, const AnsiString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SinkFile(AnsiString FileName, const TRemoteFile * File, void * Param);
  void __fastcall SourceRobust(const AnsiString FileName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall Source(const AnsiString FileName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall DirectorySource(const AnsiString DirectoryName,
    const AnsiString TargetDir, int Attrs, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags);
  bool __fastcall ConfirmOverwrite(AnsiString & FileName,
    TOverwriteMode & OverwriteMode, TFileOperationProgressType * OperationProgress,
    const TOverwriteFileParams * FileParams, int Params, bool AutoResume);
  void __fastcall CheckFileTransferAbort();
  void __fastcall ReadDirectoryProgress(__int64 Bytes);
  void __fastcall ResetFileTransfer();
  void __fastcall FileTransferProgress(__int64 TransferSize, __int64 Bytes, int Percent);
  void __fastcall ResetCaches();
  void __fastcall CaptureOutput(const AnsiString & Str);
  void __fastcall DoReadDirectory(TRemoteFileList * FileList);
  void __fastcall FileTransfer(const AnsiString & FileName, const AnsiString & LocalFile,
    const AnsiString & RemoteFile, const AnsiString & RemotePath, bool Get,
    __int64 Size, int Type, TFileTransferData & UserData,
    TFileOperationProgressType * OperationProgress);

  static bool __fastcall Unquote(AnsiString & Str);

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
  TMessageQueue * FQueue;
  HANDLE FQueueEvent;
  TSessionInfo FSessionInfo;
  TFileSystemInfo FFileSystemInfo;
  bool FFileSystemInfoValid;
  unsigned int FReply;
  unsigned int FCommandReply;
  TCommand FLastCommand;
  bool FPasswordFailed;
  bool FMultineResponse;
  int FLastCode;
  int FLastCodeClass;
  int FLastReadDirectoryProgress;
  TStrings * FLastResponse;
  TStrings * FLastError;
  AnsiString FSystem;
  TStrings * FFeatures;
  AnsiString FCurrentDirectory;
  AnsiString FHomeDirectory;
  TRemoteFileList * FFileList;
  TRemoteFileList * FFileListCache;
  bool FActive;
  bool FWaitingForReply;
  enum { ftaNone, ftaSkip, ftaCancel } FFileTransferAbort;
  bool FIgnoreFileList;
  bool FFileTransferCancelled;
  __int64 FFileTransferResumed;
  bool FFileTransferPreserveTime;
  unsigned long FFileTransferCPSLimit;
  bool FAwaitingProgress;
  TCaptureOutputEvent FOnCaptureOutput;
  AnsiString FUserName;
  int FListAll;
  TDateTime FLastDataSent;
  mutable AnsiString FOptionScratch;
};
//---------------------------------------------------------------------------
#endif NO_FILEZILLA
//---------------------------------------------------------------------------
#endif // FtpFileSystemH
