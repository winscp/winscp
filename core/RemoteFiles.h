//---------------------------------------------------------------------------
#ifndef RemoteFilesH
#define RemoteFilesH

//---------------------------------------------------------------------------
enum TModificationFmt { mfMDHM, mfMDY, mfFull };
enum TRightsFlag {rfUserRead, rfUserWrite, rfUserExec, rfGroupRead,
  rfGroupWrite, rfGroupExec, rfOtherRead, rfOtherWrite, rfOtherExec};
#define RightsFlagCount 9
#define raUserRead 0400
#define raUserWrite 0200
#define raUserExec 0100
#define raGroupRead 0040
#define raGroupWrite 0020
#define raGroupExec 0010
#define raOtherRead 0004
#define raOtherWrite 0002
#define raOtherExec 0001
#define raUser 0700
#define raGroup 0070
#define raOther 0007
#define raRead 0444
#define raWrite 0222
#define raExecute 0111
#define raAll 0777
#define raNo 0000
#define raDefault 0644

#define raDirectory 0040000
 
enum TRightState {rsNo, rsYes, rsUndef};
//---------------------------------------------------------------------------
#define SYMLINKSTR " -> "
#define UNDEFRIGHT '$'
#define NORIGHT '-'
#define PARENTDIRECTORY ".."
#define THISDIRECTORY "."
#define FULLRIGHTS "rwxrwxrwx"
#define ROOTDIRECTORY "/"
#define MODEGROUPS "ugo"
#define FILETYPE_SYMLINK 'L'
#define FILETYPE_DIRECTORY 'D'
//---------------------------------------------------------------------------
#define TIME_POSIX_TO_WIN(t, ft) (*(LONGLONG*)&(ft) = \
    ((LONGLONG) (t) + (LONGLONG) 11644473600) * (LONGLONG) 10000000)
#define TIME_WIN_TO_POSIX(ft, t) ((t) = (unsigned long) \
    ((*(LONGLONG*)&(ft)) / (LONGLONG) 10000000 - (LONGLONG) 11644473600))
//---------------------------------------------------------------------------
class TRemoteDirectory;
class TTerminal;
class TRights;
class TRemoteProperties;
class TRemoteFileList;
//---------------------------------------------------------------------------
class TRemoteFile : public TPersistent
{
private:
  TRemoteFileList * FDirectory;
  AnsiString FOwner;
  TModificationFmt FModificationFmt;
  __int64 FSize;
  AnsiString FFileName;
  Integer FINodeBlocks;
  TDateTime FModification;
  TDateTime FLastAccess;
  AnsiString FGroup;
  Integer FIconIndex;
  Boolean FIsSymLink;
  TRemoteFile * FLinkedFile;
  TRemoteFile * FLinkedByFile;
  AnsiString FLinkTo;
  TRights *FRights;
  TTerminal *FTerminal;
  Char FType;
  bool FSelected;
  bool FCyclicLink;
  int __fastcall GetAttr();
  bool __fastcall GetBrokenLink();
  bool __fastcall GetIsDirectory() const;
  TRemoteFile * __fastcall GetLinkedFile();
  void __fastcall SetLinkedFile(TRemoteFile * value);
  AnsiString __fastcall GetModificationStr();
  void __fastcall SetModification(const TDateTime & value);
  void __fastcall SetListingStr(AnsiString value);
  AnsiString __fastcall GetListingStr();
  AnsiString __fastcall GetRightsStr();
  char __fastcall GetType() const;
  void __fastcall SetType(char AType);
  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall SetRights(TRights * value);
  AnsiString __fastcall GetFullFileName();
  int __fastcall GetIconIndex();
  bool __fastcall GetIsHidden();
  bool __fastcall GetIsParentDirectory();
  bool __fastcall GetIsThisDirectory();
  bool __fastcall GetIsInaccesibleDirectory();
  AnsiString __fastcall GetExtension();
  AnsiString __fastcall GetUserModificationStr();

protected:
  void __fastcall FindLinkedFile();

public:
  __fastcall TRemoteFile(TRemoteFile * ALinkedByFile = NULL);
  virtual __fastcall ~TRemoteFile();
  TRemoteFile * __fastcall Duplicate();

  void __fastcall ShiftTime(const TDateTime & Difference);

  __property int Attr = { read = GetAttr };
  __property bool BrokenLink = { read = GetBrokenLink };
  __property TRemoteFileList * Directory = { read = FDirectory, write = FDirectory };
  __property AnsiString RightsStr = { read = GetRightsStr };
  __property __int64 Size = { read = FSize, write = FSize };
  __property AnsiString Owner = { read = FOwner, write = FOwner };
  __property AnsiString Group = { read = FGroup, write = FGroup };
  __property AnsiString FileName = { read = FFileName, write = FFileName };
  __property int INodeBlocks = { read = FINodeBlocks };
  __property TDateTime Modification = { read = FModification, write = SetModification };
  __property AnsiString ModificationStr = { read = GetModificationStr };
  __property AnsiString UserModificationStr = { read = GetUserModificationStr };
  __property TDateTime LastAccess = { read = FLastAccess, write = FLastAccess };
  __property bool IsSymLink = { read = FIsSymLink };
  __property bool IsDirectory = { read = GetIsDirectory };
  __property TRemoteFile * LinkedFile = { read = GetLinkedFile, write = SetLinkedFile };
  __property AnsiString LinkTo = { read = FLinkTo, write = FLinkTo };
  __property AnsiString ListingStr = { read = GetListingStr, write = SetListingStr };
  __property TRights * Rights = { read = FRights, write = SetRights };
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
  __property Char Type = { read = GetType, write = SetType };
  __property bool Selected  = { read=FSelected, write=FSelected };
  __property AnsiString FullFileName  = { read=GetFullFileName };
  __property int IconIndex = { read = GetIconIndex };
  __property bool IsHidden = { read = GetIsHidden };
  __property bool IsParentDirectory = { read = GetIsParentDirectory };
  __property bool IsThisDirectory = { read = GetIsThisDirectory };
  __property bool IsInaccesibleDirectory  = { read=GetIsInaccesibleDirectory };
  __property AnsiString Extension  = { read=GetExtension };
};
//---------------------------------------------------------------------------
class TRemoteFileList : public TObjectList
{
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
protected:
  AnsiString FDirectory;
  TRemoteFile * __fastcall GetFiles(Integer Index);
  virtual void __fastcall SetDirectory(AnsiString value);
  AnsiString __fastcall GetFullDirectory();
  Boolean __fastcall GetIsRoot();
  TRemoteFile * __fastcall GetParentDirectory();
  AnsiString __fastcall GetParentPath();
  __int64 __fastcall GetTotalSize();

  virtual void __fastcall Clear();
public:
  __fastcall TRemoteFileList();
  TRemoteFile * __fastcall FindFile(const AnsiString &FileName);
  virtual void __fastcall DuplicateTo(TRemoteFileList * Copy);
  virtual void __fastcall AddFile(TRemoteFile * File);
  __property AnsiString Directory = { read = FDirectory, write = SetDirectory };
  __property TRemoteFile * Files[Integer Index] = { read = GetFiles };
  __property AnsiString FullDirectory  = { read=GetFullDirectory };
  __property Boolean IsRoot = { read = GetIsRoot };
  __property AnsiString ParentPath = { read = GetParentPath };
  __property __int64 TotalSize = { read = GetTotalSize };
};
//---------------------------------------------------------------------------
class TRemoteDirectory : public TRemoteFileList
{
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
private:
  Boolean FIncludeParentDirectory;
  Boolean FIncludeThisDirectory;
  TTerminal * FTerminal;
  TStrings * FSelectedFiles;
  TRemoteFile * FParentDirectory;
  TRemoteFile * FThisDirectory;
  virtual void __fastcall SetDirectory(AnsiString value);
  TStrings * __fastcall GetSelectedFiles();
  Boolean __fastcall GetLoaded();
  void __fastcall SetIncludeParentDirectory(Boolean value);
  void __fastcall SetIncludeThisDirectory(Boolean value);
protected:
  virtual void __fastcall Clear();
  //virtual TRemoteFile * __fastcall NewFile(TRemoteFile * ALinkedByFile = NULL);
public:
  __fastcall TRemoteDirectory(TTerminal * aTerminal);
  virtual void __fastcall AddFile(TRemoteFile * File);
  virtual void __fastcall DuplicateTo(TRemoteFileList * Copy);
  __property TTerminal * Terminal = { read = FTerminal, write = FTerminal };
  __property TStrings * SelectedFiles  = { read=GetSelectedFiles };
  __property Boolean IncludeParentDirectory = { read = FIncludeParentDirectory, write = SetIncludeParentDirectory };
  __property Boolean IncludeThisDirectory = { read = FIncludeThisDirectory, write = SetIncludeThisDirectory };
  __property Boolean Loaded = { read = GetLoaded };
  __property TRemoteFile * ParentDirectory = { read = FParentDirectory };
  __property TRemoteFile * ThisDirectory = { read = FThisDirectory };
};
//---------------------------------------------------------------------------
class TRemoteDirectoryCache : private TStringList
{
public:
  __fastcall TRemoteDirectoryCache();
  virtual __fastcall ~TRemoteDirectoryCache();
  TRemoteFileList * __fastcall GetFileList(const AnsiString Directory);
  void __fastcall AddFileList(TRemoteFileList * FileList);
  void __fastcall ClearFileList(AnsiString Directory, bool SubDirs);
protected:
  virtual void __fastcall Delete(int Index);
};
//---------------------------------------------------------------------------
class TRights {
private:
  Boolean FAllowUndef;
  AnsiString FText;
  AnsiString __fastcall GetFullRights() const;
  Boolean __fastcall GetIsUndef() const;
  AnsiString __fastcall GetModeStr() const;
  AnsiString __fastcall GetSimplestStr() const;
  void __fastcall SetNumber(Word value);
  AnsiString __fastcall GetText() const;
  void __fastcall SetText(AnsiString value);
  void __fastcall SetOctal(AnsiString value);
  Word __fastcall GetNumber() const;
  Word __fastcall GetNumberSet() const;
  Word __fastcall GetNumberUnset() const;
  AnsiString __fastcall GetOctal() const;
  Boolean __fastcall GetReadOnly();
  Boolean __fastcall GetRight(TRightsFlag Flag) const;
  TRightState __fastcall GetRightUndef(TRightsFlag Flag) const;
  void __fastcall SetAllowUndef(Boolean value);
  void __fastcall SetReadOnly(Boolean value);
  void __fastcall SetRight(TRightsFlag Flag, Boolean value);
  void __fastcall SetRightUndef(TRightsFlag Flag, TRightState value);
protected:
  Word __fastcall CalcNumber(TRightState State, Boolean AllowUndef) const;
public:
  TRights __fastcall operator &(Integer rhr) const;
  TRights __fastcall operator &(const TRights & rhr) const;
  TRights & __fastcall operator &=(Integer rhr);
  TRights & __fastcall operator &=(const TRights & rhr);
  TRights __fastcall operator |(Integer rhr) const;
  TRights __fastcall operator |(const TRights & rhr) const;
  TRights & __fastcall operator |=(Integer rhr);
  TRights & __fastcall operator |=(const TRights & rhr);
  TRights __fastcall operator ~() const;
  TRights & __fastcall operator =(Integer rhr);
  TRights & __fastcall operator =(const TRights & rhr);
  bool __fastcall operator ==(Integer rhr) const;
  bool __fastcall operator ==(const TRights & rhr) const;
  bool __fastcall operator !=(const TRights & rhr) const;
  __fastcall operator unsigned short() const;
  __fastcall operator unsigned long() const;
  void __fastcall AddExecute();
  void __fastcall AllUndef();
  virtual void __fastcall Assign(const TRights * Source);
  __fastcall TRights(const TRights & Source);
  __fastcall TRights(Word aNumber);
  __fastcall TRights();
  __property Boolean AllowUndef = { read = FAllowUndef, write = SetAllowUndef };
  __property AnsiString FullRights = { read = GetFullRights };
  __property Boolean IsUndef = { read = GetIsUndef };
  __property AnsiString ModeStr = { read = GetModeStr };
  __property AnsiString SimplestStr = { read = GetSimplestStr };
  __property AnsiString Octal = { read = GetOctal, write = SetOctal };
  __property Word Number = { read = GetNumber, write = SetNumber };
  __property Word NumberSet = { read = GetNumberSet };
  __property Word NumberUnset = { read = GetNumberUnset };
  __property Boolean ReadOnly = { read = GetReadOnly, write = SetReadOnly };
  __property Boolean Right[TRightsFlag Flag] = { read = GetRight, write = SetRight };
  __property TRightState RightUndef[TRightsFlag Flag] = { read = GetRightUndef, write = SetRightUndef };
  __property AnsiString Text = { read = GetText, write = SetText };
};
//---------------------------------------------------------------------------
enum TValidProperty { vpRights, vpGroup, vpOwner };
typedef Set<TValidProperty, vpRights, vpOwner> TValidProperties;
class TRemoteProperties
{
public:
  TValidProperties Valid;
  bool Recursive;
  TRights Rights;
  bool AddXToDirectories;
  AnsiString Group;
  AnsiString Owner;

  __fastcall TRemoteProperties();
  bool __fastcall operator ==(const TRemoteProperties & rhp) const;
  bool __fastcall operator !=(const TRemoteProperties & rhp) const;

  static TRemoteProperties __fastcall CommonProperties(TStrings * FileList);
  static TRemoteProperties __fastcall ChangedProperties(
    const TRemoteProperties & OriginalProperties, TRemoteProperties NewProperties);
};
//---------------------------------------------------------------------------
AnsiString __fastcall UnixIncludeTrailingBackslash(const AnsiString Path);
AnsiString __fastcall UnixExcludeTrailingBackslash(const AnsiString Path);
AnsiString __fastcall UnixExtractFileDir(const AnsiString Path);
AnsiString __fastcall UnixExtractFilePath(const AnsiString Path);
AnsiString __fastcall UnixExtractFileName(const AnsiString Path);
AnsiString __fastcall UnixExtractFileExt(const AnsiString Path);
Boolean __fastcall UnixComparePaths(const AnsiString Path1, const AnsiString Path2);
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(unsigned long TimeStamp);
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime);
//---------------------------------------------------------------------------
#endif

