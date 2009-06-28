//---------------------------------------------------------------------------
#ifndef ScpFileSystemH
#define ScpFileSystemH

#include <FileSystems.h>
//---------------------------------------------------------------------------
class TCommandSet;
class TSecureShell;
//---------------------------------------------------------------------------
class TSCPFileSystem : public TCustomFileSystem
{
public:
  __fastcall TSCPFileSystem(TTerminal * ATerminal, TSecureShell * SecureShell);
  virtual __fastcall ~TSCPFileSystem();

  virtual void __fastcall Open();
  virtual void __fastcall Close();
  virtual bool __fastcall GetActive();
  virtual void __fastcall Idle();
  virtual AnsiString __fastcall AbsolutePath(AnsiString Path, bool Local);
  virtual void __fastcall AnyCommand(const AnsiString Command,
    TCaptureOutputEvent OutputEvent);
  virtual void __fastcall ChangeDirectory(const AnsiString Directory);
  virtual void __fastcall CachedChangeDirectory(const AnsiString Directory);
  virtual void __fastcall AnnounceFileListOperation();
  virtual void __fastcall ChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties,
    TChmodSessionAction & Action);
  virtual bool __fastcall LoadFilesProperties(TStrings * FileList);
  virtual void __fastcall CalculateFilesChecksum(const AnsiString & Alg,
    TStrings * FileList, TStrings * Checksums,
    TCalculatedChecksumEvent OnCalculatedChecksum);
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    TOnceDoneOperation & OnceDoneOperation);
  virtual void __fastcall CreateDirectory(const AnsiString DirName);
  virtual void __fastcall CreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic);
  virtual void __fastcall DeleteFile(const AnsiString FileName,
    const TRemoteFile * File, int Params, TRmSessionAction & Action);
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
  __property TStrings * Output = { read = FOutput };
  __property int ReturnCode = { read = FReturnCode };

  virtual AnsiString __fastcall GetCurrentDirectory();

private:
  TSecureShell * FSecureShell;
  TCommandSet * FCommandSet;
  TFileSystemInfo FFileSystemInfo;
  AnsiString FCurrentDirectory;
  TStrings * FOutput;
  int FReturnCode;
  AnsiString FCachedDirectoryChange;
  bool FProcessingCommand;
  int FLsFullTime;
  TCaptureOutputEvent FOnCaptureOutput;

  void __fastcall ClearAliases();
  void __fastcall ClearAlias(AnsiString Alias);
  void __fastcall CustomReadFile(const AnsiString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  static AnsiString __fastcall DelimitStr(AnsiString Str);
  void __fastcall DetectReturnVar();
  bool __fastcall IsLastLine(AnsiString & Line);
  static bool __fastcall IsTotalListingLine(const AnsiString Line);
  void __fastcall EnsureLocation();
  void __fastcall ExecCommand(const AnsiString & Cmd, int Params,
    const AnsiString & CmdString);
  void __fastcall ExecCommand(TFSCommand Cmd, const TVarRec * args = NULL,
    int size = 0, int Params = -1);
  void __fastcall ReadCommandOutput(int Params, const AnsiString * Cmd = NULL);
  void __fastcall SCPResponse(bool * GotLastLine = NULL);
  void __fastcall SCPDirectorySource(const AnsiString DirectoryName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, int Level);
  void __fastcall SCPError(const AnsiString Message, bool Fatal);
  void __fastcall SCPSendError(const AnsiString Message, bool Fatal);
  void __fastcall SCPSink(const AnsiString TargetDir,
    const AnsiString FileName, const AnsiString SourceDir,
    const TCopyParamType * CopyParam, bool & Success,
    TFileOperationProgressType * OperationProgress, int Params, int Level);
  void __fastcall SCPSource(const AnsiString FileName,
    const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress, int Level);
  void __fastcall SendCommand(const AnsiString Cmd);
  void __fastcall SkipFirstLine();
  void __fastcall SkipStartupMessage();
  void __fastcall UnsetNationalVars();
  TRemoteFile * __fastcall CreateRemoteFile(const AnsiString & ListingStr,
    TRemoteFile * LinkedByFile = NULL);
  void __fastcall CaptureOutput(const AnsiString & AddedLine, bool StdError);
  void __fastcall ChangeFileToken(const AnsiString & DelimitedName,
    const TRemoteToken & Token, TFSCommand Cmd, const AnsiString & RecursiveStr);

  static bool __fastcall RemoveLastLine(AnsiString & Line,
    int & ReturnCode, AnsiString LastLine = "");
};
//---------------------------------------------------------------------------
#endif // ScpFileSystemH
