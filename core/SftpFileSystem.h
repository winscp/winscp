//---------------------------------------------------------------------------
#ifndef SftpFileSystemH
#define SftpFileSystemH

#include <FileSystems.h>
//---------------------------------------------------------------------------
class TSFTPPacket;
class TOverwriteFileParams;
struct TSFTPSupport;
class TSecureShell;
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
    const TRemoteFile * File = NULL, bool Recursive = false);
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
  TSecureShell * FSecureShell;
  TFileSystemInfo FFileSystemInfo;
  bool FFileSystemInfoValid;
  int FVersion;
  AnsiString FCurrentDirectory;
  AnsiString FDirectoryToChangeTo;
  AnsiString FHomeDirectory;
  AnsiString FEOL;
  TList * FPacketReservations;
  Variant FPacketNumbers;
  char FPreviousLoggedPacket;
  int FNotLoggedPackets;
  int FBusy;
  bool FAvoidBusy;
  TStrings * FExtensions;
  TSFTPSupport * FSupport;
  bool FUtfStrings;
  bool FUtfNever;
  bool FSignedTS;
  TStrings * FFixedPaths;
  unsigned long FMaxPacketSize;

  void __fastcall SendCustomReadFile(TSFTPPacket * Packet, TSFTPPacket * Response,
    const AnsiString FileName, unsigned long Flags);
  void __fastcall CustomReadFile(const AnsiString FileName,
    TRemoteFile *& File, char Type, TRemoteFile * ALinkedByFile = NULL,
    int AllowStatus = -1);
  virtual AnsiString __fastcall GetCurrentDirectory();
  AnsiString __fastcall GetHomeDirectory();
  unsigned long __fastcall GotStatusPacket(TSFTPPacket * Packet, int AllowStatus);
  bool __fastcall inline IsAbsolutePath(const AnsiString Path);
  bool __fastcall RemoteFileExists(const AnsiString FullPath, TRemoteFile ** File = NULL);
  TRemoteFile * __fastcall LoadFile(TSFTPPacket * Packet,
    TRemoteFile * ALinkedByFile, const AnsiString FileName,
    TRemoteFileList * TempFileList = NULL, bool Complete = true);
  void __fastcall LoadFile(TRemoteFile * File, TSFTPPacket * Packet,
    bool Complete = true);
  AnsiString __fastcall LocalCanonify(const AnsiString & Path);
  AnsiString __fastcall Canonify(AnsiString Path);
  AnsiString __fastcall RealPath(const AnsiString Path);
  AnsiString __fastcall RealPath(const AnsiString Path, const AnsiString BaseDir);
  void __fastcall ReserveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response);
  int __fastcall ReceivePacket(TSFTPPacket * Packet, int ExpectedType = -1,
    int AllowStatus = -1);
  bool __fastcall PeekPacket();
  void __fastcall RemoveReservation(int Reservation);
  void __fastcall SendPacket(const TSFTPPacket * Packet);
  int __fastcall ReceiveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response, int ExpectedType = -1, int AllowStatus = -1);
  int __fastcall SendPacketAndReceiveResponse(const TSFTPPacket * Packet,
    TSFTPPacket * Response, int ExpectedType = -1, int AllowStatus = -1);
  void __fastcall UnreserveResponse(TSFTPPacket * Response);
  void __fastcall TryOpenDirectory(const AnsiString Directory);
  bool __fastcall SupportsExtension(const AnsiString & Extension) const;
  void __fastcall ResetConnection();
  void __fastcall DoCalculateFilesChecksum(const AnsiString & Alg,
    TStrings * FileList, TStrings * Checksums,
    TCalculatedChecksumEvent OnCalculatedChecksum,
    TFileOperationProgressType * OperationProgress, bool FirstLevel);

  void __fastcall SFTPSourceRobust(const AnsiString FileName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SFTPSource(const AnsiString FileName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  AnsiString __fastcall SFTPOpenRemoteFile(const AnsiString & FileName,
    unsigned int OpenType, __int64 Size = -1);
  int __fastcall SFTPOpenRemote(void * AOpenParams, void * Param2);
  void __fastcall SFTPCloseRemote(const AnsiString Handle,
    const AnsiString FileName, TFileOperationProgressType * OperationProgress,
    bool TransferFinished, bool Request, TSFTPPacket * Packet);
  void __fastcall SFTPDirectorySource(const AnsiString DirectoryName,
    const AnsiString TargetDir, int Attrs, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SFTPConfirmOverwrite(AnsiString & FileName,
    TFileOperationProgressType * OperationProgress,
    TSFTPOverwriteMode & Mode, const TOverwriteFileParams * FileParams);
  bool SFTPConfirmResume(const AnsiString DestFileName, bool PartialBiggerThanSource,
    TFileOperationProgressType * OperationProgress);
  void __fastcall SFTPSinkRobust(const AnsiString FileName,
    const TRemoteFile * File, const AnsiString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SFTPSink(const AnsiString FileName,
    const TRemoteFile * File, const AnsiString TargetDir,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, unsigned int Flags);
  void __fastcall SFTPSinkFile(AnsiString FileName,
    const TRemoteFile * File, void * Param);
  char * __fastcall GetEOL() const;
  inline void __fastcall BusyStart();
  inline void __fastcall BusyEnd();
  inline unsigned long __fastcall TransferBlockSize(unsigned long Overhead,
    TFileOperationProgressType * OperationProgress, unsigned long MaxPacketSize = 0);
  inline unsigned long __fastcall UploadBlockSize(const AnsiString & Handle,
    TFileOperationProgressType * OperationProgress);
  inline unsigned long __fastcall DownloadBlockSize(
    TFileOperationProgressType * OperationProgress);
  inline int __fastcall PacketLength(char * LenBuf, int ExpectedType);

  static AnsiString __fastcall DecodeUTF(const AnsiString UTF);
  static AnsiString __fastcall EncodeUTF(const WideString Source);
};
//---------------------------------------------------------------------------
#endif // SftpFileSystemH
