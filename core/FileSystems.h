//---------------------------------------------------------------------------
#ifndef FileSystemsH
#define FileSystemsH

#include <Classes.hpp>
//---------------------------------------------------------------------------
class TTerminal;
class TRights;
class TRemoteFile;
class TRemoteFileList;
struct TCopyParamType;
class TFileOperationProgressType;
class TRemoteProperties;
//---------------------------------------------------------------------------
enum TFSCommand { fsNull = 0, fsVarValue, fsLastLine, fsFirstLine,
  fsCurrentDirectory, fsChangeDirectory, fsListDirectory, fsListCurrentDirectory,
  fsListFile, fsLookupUserGroups, fsCopyToRemote, fsCopyToLocal, fsDeleteFile,
  fsRenameFile, fsCreateDirectory, fsChangeMode, fsChangeGroup, fsChangeOwner,
  fsHomeDirectory, fsUnset, fsUnalias, fsAliasGroupList, fsCreateLink,
  fsAnyCommand, fsReadSymlink, fsChangeProperties };
//---------------------------------------------------------------------------
typedef void __fastcall (__closure * TGetParamValueEvent)
  (const AnsiString Name, AnsiString & Value);
//---------------------------------------------------------------------------
class TCustomFileSystem : public TObject
{
public:
  static AnsiString __fastcall CompleteCustomCommand(AnsiString Command,
    const AnsiString FileName, TGetParamValueEvent OnGetParamValue);

  virtual void __fastcall AnyCommand(const AnsiString Command) = 0;
  virtual void __fastcall ChangeDirectory(const AnsiString Directory) = 0;
  virtual void __fastcall ChangeFileProperties(const AnsiString FileName,
    const TRemoteFile * File, const TRemoteProperties * Properties) = 0;
  virtual void __fastcall CopyToLocal(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    bool & DisconnectWhenComplete) = 0;
  virtual void __fastcall CopyToRemote(TStrings * FilesToCopy,
    const AnsiString TargetDir, const TCopyParamType * CopyParam,
    int Params, TFileOperationProgressType * OperationProgress,
    bool & DisconnectWhenComplete) = 0;
  virtual void __fastcall CreateDirectory(const AnsiString DirName,
    const TRemoteProperties * Properties) = 0;
  virtual void __fastcall CreateLink(const AnsiString FileName, const AnsiString PointTo, bool Symbolic) = 0;
  virtual void __fastcall DeleteFile(const AnsiString FileName,
    const TRemoteFile * File, bool Recursive) = 0;
  virtual void __fastcall CustomCommandOnFile(const AnsiString FileName,
    const TRemoteFile * File, AnsiString Command, int Params) = 0;
  virtual void __fastcall DoStartup() = 0;
  virtual void __fastcall HomeDirectory() = 0;
  virtual bool __fastcall IsCapable(int Capability) const = 0;
  virtual void __fastcall LookupUserGroups() = 0;
  virtual void __fastcall ReadCurrentDirectory() = 0;
  virtual void __fastcall ReadDirectory(TRemoteFileList * FileList) = 0;
  virtual void __fastcall ReadFile(const AnsiString FileName,
    TRemoteFile *& File) = 0;
  virtual void __fastcall ReadSymlink(TRemoteFile * SymLinkFile,
    TRemoteFile *& File) = 0;
  virtual void __fastcall RenameFile(const AnsiString FileName,
    const AnsiString NewName) = 0;

  __property AnsiString CurrentDirectory = { read = GetCurrentDirectory, write = SetCurrentDirectory };
  __property AnsiString ProtocolName = { read = GetProtocolName };

protected:
  TTerminal * FTerminal;

  __fastcall TCustomFileSystem(TTerminal * ATerminal);
  virtual AnsiString __fastcall GetCurrentDirectory() = 0;
  virtual AnsiString __fastcall GetProtocolName() const = 0;
  virtual void __fastcall SetCurrentDirectory(AnsiString value) = 0;
};
//---------------------------------------------------------------------------
#endif
