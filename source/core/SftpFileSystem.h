//---------------------------------------------------------------------------
#ifndef SftpFileSystemH
#define SftpFileSystemH

#include <FileSystems.h>
//---------------------------------------------------------------------------
class TSFTPPacket;
struct TOverwriteFileParams;
struct TSFTPSupport;
class TSecureShell;
class TEncryption;
//---------------------------------------------------------------------------
enum TSFTPOverwriteMode { omOverwrite, omAppend, omResume };
//---------------------------------------------------------------------------
class TSFTPFileSystem : public TCustomFileSystem
{
friend class TSFTPPacket;
friend class TSFTPQueue;
friend class TSFTPAsynchronousQueue;
friend class TSFTPUploadQueue;
friend class TSFTPDownloadQueue;
friend class TSFTPLoadFilesPropertiesQueue;
friend class TSFTPCalculateFilesChecksumQueue;
friend class TSFTPBusy;
public:
  __fastcall TSFTPFileSystem(TTerminal * ATerminal, TSecureShell * SecureShell);
  virtual __fastcall ~TSFTPFileSystem();

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
  virtual void __fastcall DirectorySunk(
    const UnicodeString & DestFullName, const TRemoteFile * File, const TCopyParamType * CopyParam);
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
  virtual UnicodeString __fastcall GetHomeDirectory();
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
  TSecureShell * FSecureShell;
  TFileSystemInfo FFileSystemInfo;
  bool FFileSystemInfoValid;
  int FVersion;
  UnicodeString FCurrentDirectory;
  UnicodeString FDirectoryToChangeTo;
  UnicodeString FHomeDirectory;
  AnsiString FEOL;
  TList * FPacketReservations;
  Variant FPacketNumbers;
  char FPreviousLoggedPacket;
  int FNotLoggedWritePackets, FNotLoggedReadPackets, FNotLoggedStatusPackets, FNotLoggedDataPackets;
  std::set<unsigned int> FNotLoggedRequests;
  int FBusy;
  void * FBusyToken;
  bool FAvoidBusy;
  UnicodeString FExtensions;
  std::unique_ptr<TStrings> FSupportedExtensions;
  TSFTPSupport * FSupport;
  TAutoSwitch FUtfStrings;
  bool FUtfDisablingAnnounced;
  bool FSignedTS;
  TStrings * FFixedPaths;
  unsigned long FMaxPacketSize;
  bool FSupportsStatVfsV2;
  bool FSupportsHardlink;
  std::unique_ptr<TStringList> FChecksumAlgs;
  std::unique_ptr<TStringList> FChecksumSftpAlgs;

  void __fastcall SendCustomReadFile(TSFTPPacket * Packet, TSFTPPacket * Response,
    unsigned long Flags);
  void __fastcall CustomReadFile(const UnicodeString FileName,
    TRemoteFile *& File, unsigned char Type, TRemoteFile * ALinkedByFile = NULL,
    int AllowStatus = -1);
  virtual UnicodeString __fastcall GetCurrentDirectory();
  unsigned long __fastcall GotStatusPacket(TSFTPPacket * Packet, int AllowStatus, bool DoNotForceLog);
  bool __fastcall RemoteFileExists(const UnicodeString FullPath, TRemoteFile ** File = NULL);
  TRemoteFile * __fastcall LoadFile(TSFTPPacket * Packet,
    TRemoteFile * ALinkedByFile, const UnicodeString FileName,
    TRemoteFileList * TempFileList = NULL, bool Complete = true);
  void __fastcall LoadFile(TRemoteFile * File, TSFTPPacket * Packet,
    bool Complete = true);
  UnicodeString __fastcall LocalCanonify(const UnicodeString & Path);
  UnicodeString __fastcall Canonify(const UnicodeString & Path);
  UnicodeString __fastcall RealPath(const UnicodeString & Path);
  UnicodeString __fastcall RealPath(const UnicodeString & Path, const UnicodeString & BaseDir);
  void __fastcall ReserveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response);
  int __fastcall ReceivePacket(TSFTPPacket * Packet, int ExpectedType = -1,
    int AllowStatus = -1, bool TryOnly = false);
  bool __fastcall PeekPacket();
  void __fastcall RemoveReservation(int Reservation);
  void __fastcall SendPacket(const TSFTPPacket * Packet);
  int __fastcall ReceiveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response, int ExpectedType = -1, int AllowStatus = -1, bool TryOnly = false);
  int __fastcall SendPacketAndReceiveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response, int ExpectedType = -1, int AllowStatus = -1);
  void __fastcall UnreserveResponse(TSFTPPacket * Response);
  void LogPacket(const TSFTPPacket * Packet, TLogLineType Type);
  void __fastcall TryOpenDirectory(const UnicodeString Directory);
  bool __fastcall SupportsExtension(const UnicodeString & Extension) const;
  void __fastcall ResetConnection();
  void __fastcall RegisterChecksumAlg(const UnicodeString & Alg, const UnicodeString & SftpAlg);
  void __fastcall DoDeleteFile(const UnicodeString FileName, unsigned char Type);

  void __fastcall SFTPSource(const UnicodeString FileName,
    const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags,
    TUploadSessionAction & Action, bool & ChildError);
  RawByteString __fastcall SFTPOpenRemoteFile(const UnicodeString & FileName,
    unsigned int OpenType, bool EncryptNewFiles = false, __int64 Size = -1);
  int __fastcall SFTPOpenRemote(void * AOpenParams, void * Param2);
  void __fastcall SFTPCloseRemote(const RawByteString Handle,
    const UnicodeString FileName, TFileOperationProgressType * OperationProgress,
    bool TransferFinished, bool Request, TSFTPPacket * Packet);
  void __fastcall SFTPConfirmOverwrite(const UnicodeString & FullFileName, UnicodeString & FileName,
    const TCopyParamType * CopyParam, int Params, TFileOperationProgressType * OperationProgress,
    TSFTPOverwriteMode & Mode, const TOverwriteFileParams * FileParams);
  bool SFTPConfirmResume(const UnicodeString DestFileName, bool PartialBiggerThanSource,
    TFileOperationProgressType * OperationProgress);
  const char * __fastcall GetEOL() const;
  inline void __fastcall BusyStart();
  inline void __fastcall BusyEnd();
  inline unsigned long __fastcall TransferBlockSize(
    unsigned long Overhead, TFileOperationProgressType * OperationProgress);
  inline unsigned long __fastcall UploadBlockSize(const RawByteString & Handle,
    TFileOperationProgressType * OperationProgress);
  inline unsigned long __fastcall DownloadBlockSize(
    TFileOperationProgressType * OperationProgress);
  inline int __fastcall PacketLength(unsigned char * LenBuf, int ExpectedType);
  void __fastcall Progress(TFileOperationProgressType * OperationProgress);
  void AddPathString(TSFTPPacket & Packet, const UnicodeString & Value, bool EncryptNewFiles = false);
  void __fastcall WriteLocalFile(
    const TCopyParamType * CopyParam, TStream * FileStream, TFileBuffer & BlockBuf, const UnicodeString & LocalFileName,
    TFileOperationProgressType * OperationProgress);
  bool __fastcall DoesFileLookLikeSymLink(TRemoteFile * File);
  void DoCloseRemoteIfOpened(const RawByteString & Handle);
};
//---------------------------------------------------------------------------
#endif // SftpFileSystemH
