//---------------------------------------------------------------------------
#ifndef ScpFileSystemH
#define ScpFileSystemH

#include <FileSystems.h>
//---------------------------------------------------------------------------
class TCommandSet;
//---------------------------------------------------------------------------
class TSCPFileSystem : public TCustomFileSystem
{
public:
  __fastcall TSCPFileSystem(TTerminal * ATerminal);
  virtual __fastcall ~TSCPFileSystem();

  virtual void __fastcall AnyCommand(const AnsiString Command);
  virtual void __fastcall ChangeDirectory(const AnsiString Directory);
  virtual void __fastcall ChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties);
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
    const TRemoteFile * File, bool Recursive);
  virtual void __fastcall CustomCommandOnFile(const AnsiString FileName,
    const TRemoteFile * File, AnsiString Command);
  virtual void __fastcall DoStartup();
  virtual void __fastcall HomeDirectory();
  virtual bool __fastcall IsCapable(int Capability) const;
  virtual void __fastcall LookupUserGroups();
  virtual void __fastcall ReadCurrentDirectory();
  virtual void __fastcall ReadDirectory(TRemoteFileList * FileList);
  virtual void __fastcall ReadFile(const AnsiString FileName,
    TRemoteFile *& File);
  virtual void __fastcall ReadSymlink(TRemoteFile * SymlinkFile,
    TRemoteFile *& File);
  virtual void __fastcall RenameFile(const AnsiString FileName,
    const AnsiString NewName);

  static bool __fastcall RemoveLastLine(AnsiString & Line,
    int & ReturnCode, AnsiString LastLine = "");

protected:
  __property TStrings * Output = { read = FOutput };
  __property int ReturnCode = { read = FReturnCode };

  virtual AnsiString __fastcall GetCurrentDirectory();
  virtual void __fastcall SetCurrentDirectory(AnsiString value);
  virtual AnsiString __fastcall GetProtocolName() const;

private:
  TCommandSet * FCommandSet;
  AnsiString FCurrentDirectory;
  TStrings * FOutput;
  int FReturnCode;

  void __fastcall AliasGroupList();
  void __fastcall ClearAliases();
  void __fastcall CustomReadFile(const AnsiString FileName,
    TRemoteFile *& File, TRemoteFile * ALinkedByFile);
  static AnsiString __fastcall DelimitStr(AnsiString Str);
  void __fastcall DetectReturnVar();
  bool __fastcall IsLastLine(AnsiString & Line);
  static bool __fastcall IsTotalListingLine(const AnsiString Line);
  void __fastcall ExecCommand(const AnsiString Cmd, int Params = -1);
  void __fastcall ExecCommand(TFSCommand Cmd, const TVarRec * args = NULL,
    int size = 0, int Params = -1);
  void __fastcall ReadCommandOutput(int Params);
  void __fastcall SCPResponse();
  void __fastcall SCPDirectorySource(const AnsiString DirectoryName,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress);
  void __fastcall SCPError(const AnsiString Message, bool Fatal);
  void __fastcall SCPSendError(const AnsiString Message, bool Fatal);
  void __fastcall SCPSink(const AnsiString TargetDir,
    const AnsiString FileName, const TCopyParamType * CopyParam, bool & Success,
    TFileOperationProgressType * OperationProgress, int Params);
  void __fastcall SCPSource(const AnsiString FileName,
    const TCopyParamType * CopyParam, int Params,
    TFileOperationProgressType * OperationProgress);
  void __fastcall SendCommand(const AnsiString Cmd);
  void __fastcall SkipFirstLine();
  void __fastcall SkipStartupMessage();
  void __fastcall UnsetNationalVars();
};
//---------------------------------------------------------------------------
#endif // ScpFileSystemH
