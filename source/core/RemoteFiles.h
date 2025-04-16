//---------------------------------------------------------------------------
#ifndef RemoteFilesH
#define RemoteFilesH
//---------------------------------------------------------------------------
#include <vector>
#include <map>
//---------------------------------------------------------------------------
enum TModificationFmt { mfNone, mfMDHM, mfYMDHM, mfMDY, mfFull };
//---------------------------------------------------------------------------
#define SYMLINKSTR L" -> "
#define ROOTDIRECTORY L"/"
#define FILETYPE_DEFAULT L'-'
#define FILETYPE_SYMLINK L'L'
#define FILETYPE_DIRECTORY L'D'
extern const UnicodeString PartialExt;
//---------------------------------------------------------------------------
class TTerminal;
class TRights;
class TRemoteFileList;
class THierarchicalStorage;
//---------------------------------------------------------------------------
class TRemoteToken
{
public:
  __fastcall TRemoteToken();
  explicit __fastcall TRemoteToken(const UnicodeString & Name);

  void __fastcall Clear();

  bool __fastcall operator ==(const TRemoteToken & rht) const;
  bool __fastcall operator !=(const TRemoteToken & rht) const;
  TRemoteToken & __fastcall operator =(const TRemoteToken & rht);

  int __fastcall Compare(const TRemoteToken & rht) const;

  __property UnicodeString Name = { read = FName, write = FName };
  __property bool NameValid = { read = GetNameValid };
  __property unsigned int ID = { read = FID, write = SetID };
  __property bool IDValid = { read = FIDValid };
  __property bool IsSet  = { read = GetIsSet };
  __property UnicodeString LogText = { read = GetLogText };
  __property UnicodeString DisplayText = { read = GetDisplayText };

private:
  UnicodeString FName;
  unsigned int FID;
  bool FIDValid;

  void __fastcall SetID(unsigned int value);
  bool __fastcall GetNameValid() const;
  bool __fastcall GetIsSet() const;
  UnicodeString __fastcall GetDisplayText() const;
  UnicodeString __fastcall GetLogText() const;
};
//---------------------------------------------------------------------------
class TRemoteTokenList
{
public:
  TRemoteTokenList * __fastcall Duplicate() const;
  void __fastcall Clear();
  void __fastcall Add(const TRemoteToken & Token);
  void __fastcall AddUnique(const TRemoteToken & Token);
  bool __fastcall Exists(const UnicodeString & Name) const;
  const TRemoteToken * Find(unsigned int ID) const;
  const TRemoteToken * Find(const UnicodeString & Name) const;
  void __fastcall Log(TTerminal * Terminal, const wchar_t * Title);

  int __fastcall Count() const;
  const TRemoteToken * __fastcall Token(int Index) const;

private:
  typedef std::vector<TRemoteToken> TTokens;
  typedef std::map<UnicodeString, size_t> TNameMap;
  typedef std::map<unsigned int, size_t> TIDMap;
  TTokens FTokens;
  TNameMap FNameMap;
  TIDMap FIDMap;
};
//---------------------------------------------------------------------------
class TRemoteFile : public TPersistent
{
private:
  TRemoteFileList * FDirectory;
  TRemoteToken FOwner;
  TModificationFmt FModificationFmt;
  __int64 FSize;
  __int64 FCalculatedSize;
  UnicodeString FFileName;
  UnicodeString FDisplayName;
  Integer FINodeBlocks;
  TDateTime FModification;
  TDateTime FLastAccess;
  TRemoteToken FGroup;
  Integer FIconIndex;
  Boolean FIsSymLink;
  TRemoteFile * FLinkedFile;
  TRemoteFile * FLinkedByFile;
  UnicodeString FLinkTo;
  TRights *FRights;
  UnicodeString FHumanRights;
  TTerminal *FTerminal;
  wchar_t FType;
  UnicodeString FTags;
  bool FCyclicLink;
  UnicodeString FFullFileName;
  int FIsHidden;
  UnicodeString FTypeName;
  bool FIsEncrypted;
  int __fastcall GetAttr();
  bool __fastcall GetBrokenLink();
  bool __fastcall GetIsDirectory() const;
  const TRemoteFile * __fastcall GetLinkedFile() const;
  UnicodeString __fastcall GetModificationStr();
  void __fastcall SetModification(const TDateTime & value);
  void __fastcall SetListingStr(UnicodeString value);
  UnicodeString __fastcall GetListingStr();
  UnicodeString __fastcall GetRightsStr();
  wchar_t __fastcall GetType() const;
  void __fastcall SetType(wchar_t AType);
  void __fastcall SetTerminal(TTerminal * value);
  void __fastcall SetRights(TRights * value);
  UnicodeString __fastcall GetFullFileName() const;
  bool __fastcall GetHaveFullFileName() const;
  int __fastcall GetIconIndex() const;
  UnicodeString __fastcall GetTypeName();
  bool __fastcall GetIsHidden() const;
  void __fastcall SetIsHidden(bool value);
  bool __fastcall GetIsParentDirectory() const;
  bool __fastcall GetIsThisDirectory() const;
  bool __fastcall GetIsInaccessibleDirectory() const;
  UnicodeString __fastcall GetExtension();
  UnicodeString __fastcall GetUserModificationStr();
  void __fastcall LoadTypeInfo();
  __int64 __fastcall GetSize() const;

protected:
  void __fastcall FindLinkedFile();

public:
  __fastcall TRemoteFile(TRemoteFile * ALinkedByFile = NULL);
  virtual __fastcall ~TRemoteFile();
  TRemoteFile * __fastcall Duplicate(bool Standalone = true) const;

  void __fastcall ShiftTimeInSeconds(__int64 Seconds);
  bool __fastcall IsTimeShiftingApplicable();
  void __fastcall Complete();
  void __fastcall SetEncrypted();
  const TRemoteFile * __fastcall Resolve() const;

  static bool __fastcall IsTimeShiftingApplicable(TModificationFmt ModificationFmt);
  static void __fastcall ShiftTimeInSeconds(TDateTime & DateTime, TModificationFmt ModificationFmt, __int64 Seconds);

  __property int Attr = { read = GetAttr };
  __property bool BrokenLink = { read = GetBrokenLink };
  __property TRemoteFileList * Directory = { read = FDirectory, write = FDirectory };
  __property UnicodeString RightsStr = { read = GetRightsStr };
  __property __int64 Size = { read = GetSize, write = FSize };
  __property __int64 CalculatedSize = { read = FCalculatedSize, write = FCalculatedSize };
  __property TRemoteToken Owner = { read = FOwner, write = FOwner };
  __property TRemoteToken Group = { read = FGroup, write = FGroup };
  __property UnicodeString FileName = { read = FFileName, write = FFileName };
  __property UnicodeString DisplayName = { read = FDisplayName, write = FDisplayName };
  __property int INodeBlocks = { read = FINodeBlocks };
  __property TDateTime Modification = { read = FModification, write = SetModification };
  __property UnicodeString ModificationStr = { read = GetModificationStr };
  __property UnicodeString UserModificationStr = { read = GetUserModificationStr };
  __property TModificationFmt ModificationFmt = { read = FModificationFmt, write = FModificationFmt };
  __property TDateTime LastAccess = { read = FLastAccess, write = FLastAccess };
  __property bool IsSymLink = { read = FIsSymLink };
  __property bool IsDirectory = { read = GetIsDirectory };
  __property const TRemoteFile * LinkedFile = { read = GetLinkedFile };
  __property UnicodeString LinkTo = { read = FLinkTo, write = FLinkTo };
  __property UnicodeString ListingStr = { read = GetListingStr, write = SetListingStr };
  __property TRights * Rights = { read = FRights, write = SetRights };
  __property UnicodeString HumanRights = { read = FHumanRights, write = FHumanRights };
  __property TTerminal * Terminal = { read = FTerminal, write = SetTerminal };
  __property wchar_t Type = { read = GetType, write = SetType };
  __property UnicodeString FullFileName  = { read = GetFullFileName, write = FFullFileName };
  __property bool HaveFullFileName  = { read = GetHaveFullFileName };
  __property int IconIndex = { read = GetIconIndex };
  __property UnicodeString TypeName = { read = GetTypeName };
  __property UnicodeString Tags = { read = FTags, write = FTags };
  __property bool IsHidden = { read = GetIsHidden, write = SetIsHidden };
  __property bool IsParentDirectory = { read = GetIsParentDirectory };
  __property bool IsThisDirectory = { read = GetIsThisDirectory };
  __property bool IsInaccesibleDirectory  = { read=GetIsInaccessibleDirectory };
  __property UnicodeString Extension  = { read=GetExtension };
  __property bool IsEncrypted  = { read = FIsEncrypted };
};
//---------------------------------------------------------------------------
class TRemoteDirectoryFile : public TRemoteFile
{
public:
  __fastcall TRemoteDirectoryFile();
};
//---------------------------------------------------------------------------
class TRemoteParentDirectory : public TRemoteDirectoryFile
{
public:
  __fastcall TRemoteParentDirectory(TTerminal * Terminal);
};
//---------------------------------------------------------------------------
class TRemoteFileList : public TObjectList
{
friend class TSCPFileSystem;
friend class TSFTPFileSystem;
friend class TFTPFileSystem;
friend class TWebDAVFileSystem;
friend class TS3FileSystem;
protected:
  UnicodeString FDirectory;
  TDateTime FTimestamp;
  TRemoteFile * __fastcall GetFiles(Integer Index);
  virtual void __fastcall SetDirectory(UnicodeString value);
  UnicodeString __fastcall GetFullDirectory();
  Boolean __fastcall GetIsRoot();
  TRemoteFile * __fastcall GetParentDirectory();
  UnicodeString __fastcall GetParentPath();
  __int64 __fastcall GetTotalSize();
public:
  __fastcall TRemoteFileList();
  virtual void __fastcall Reset();
  TRemoteFile * __fastcall FindFile(const UnicodeString &FileName);
  virtual void __fastcall DuplicateTo(TRemoteFileList * Copy);
  virtual void __fastcall AddFile(TRemoteFile * File);
  virtual void ExtractFile(TRemoteFile * File);

  static TStrings * __fastcall CloneStrings(TStrings * List);
  static bool AnyDirectory(TStrings * List);

  __property UnicodeString Directory = { read = FDirectory, write = SetDirectory };
  __property TRemoteFile * Files[Integer Index] = { read = GetFiles };
  __property UnicodeString FullDirectory  = { read=GetFullDirectory };
  __property Boolean IsRoot = { read = GetIsRoot };
  __property UnicodeString ParentPath = { read = GetParentPath };
  __property __int64 TotalSize = { read = GetTotalSize };
  __property TDateTime Timestamp = { read = FTimestamp };
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
  TRemoteFile * FParentDirectory;
  TRemoteFile * FThisDirectory;
  virtual void __fastcall SetDirectory(UnicodeString value);
  Boolean __fastcall GetLoaded();
  void __fastcall SetIncludeParentDirectory(Boolean value);
  void __fastcall SetIncludeThisDirectory(Boolean value);
  void __fastcall ReleaseRelativeDirectories();
public:
  __fastcall TRemoteDirectory(TTerminal * aTerminal, TRemoteDirectory * Template = NULL);
  virtual __fastcall ~TRemoteDirectory();
  virtual void __fastcall AddFile(TRemoteFile * File);
  virtual void __fastcall DuplicateTo(TRemoteFileList * Copy);
  virtual void __fastcall Reset();
  __property TTerminal * Terminal = { read = FTerminal, write = FTerminal };
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
  bool __fastcall HasFileList(const UnicodeString Directory);
  bool __fastcall HasNewerFileList(const UnicodeString Directory, TDateTime Timestamp);
  bool __fastcall GetFileList(const UnicodeString Directory,
    TRemoteFileList * FileList);
  void __fastcall AddFileList(TRemoteFileList * FileList);
  void __fastcall ClearFileList(UnicodeString Directory, bool SubDirs);
  void __fastcall Clear();

  __property bool IsEmpty = { read = GetIsEmpty };
protected:
  virtual void __fastcall Delete(int Index);
private:
  TCriticalSection * FSection;
  bool __fastcall GetIsEmpty() const;
  void __fastcall DoClearFileList(UnicodeString Directory, bool SubDirs);
};
//---------------------------------------------------------------------------
class TRemoteDirectoryChangesCache : private TStringList
{
public:
  __fastcall TRemoteDirectoryChangesCache(int MaxSize);

  void __fastcall AddDirectoryChange(const UnicodeString SourceDir,
    const UnicodeString Change, const UnicodeString TargetDir);
  void __fastcall ClearDirectoryChange(UnicodeString SourceDir);
  void __fastcall ClearDirectoryChangeTarget(UnicodeString TargetDir);
  bool __fastcall GetDirectoryChange(const UnicodeString SourceDir,
    const UnicodeString Change, UnicodeString & TargetDir);
  void __fastcall Clear();

  void __fastcall Serialize(UnicodeString & Data);
  void __fastcall Deserialize(const UnicodeString Data);

  __property bool IsEmpty = { read = GetIsEmpty };

private:
  static bool __fastcall DirectoryChangeKey(const UnicodeString SourceDir,
    const UnicodeString Change, UnicodeString & Key);
  bool __fastcall GetIsEmpty() const;
  void __fastcall SetValue(const UnicodeString & Name, const UnicodeString & Value);
  UnicodeString __fastcall GetValue(const UnicodeString & Name);

  int FMaxSize;
};
//---------------------------------------------------------------------------
class TRights
{
public:
  static const int TextLen = 9;
  static const wchar_t UndefSymbol = L'$';
  static const wchar_t UnsetSymbol = L'-';
  // Used by Win32-OpenSSH for permissions that are not applicable on Windows.
  // See strmode() in contrib\win32\win32compat\misc.c
  static const wchar_t UnsetSymbolWin = L'*';
  static const wchar_t BasicSymbols[];
  static const wchar_t CombinedSymbols[];
  static const wchar_t ExtendedSymbols[];
  static const wchar_t ModeGroups[];
  enum TRightLevel {
    rlNone = -1,
    rlRead, rlWrite, rlExec, rlSpecial,
    rlFirst = rlRead, rlLastNormal = rlExec, rlLastWithSpecial = rlSpecial,
    rlS3Read = rlRead, rlS3Write = rlWrite, rlS3ReadACP = rlExec, rlS3WriteACP = rlSpecial, rlLastAcl = rlLastWithSpecial,
  };
  enum TRightGroup {
    rgUser, rgGroup, rgOther,
    rgFirst = rgUser, rgLast = rgOther,
    rgS3AllAwsUsers = rgGroup, rgS3AllUsers = rgOther,
  };
  enum TRight {
    rrUserIDExec, rrGroupIDExec, rrStickyBit,
    rrUserRead, rrUserWrite, rrUserExec,
    rrGroupRead, rrGroupWrite, rrGroupExec,
    rrOtherRead, rrOtherWrite, rrOtherExec,
    rrFirst = rrUserIDExec, rrLast = rrOtherExec };
  enum TFlag {
    rfSetUID =    04000, rfSetGID =      02000, rfStickyBit = 01000,
    rfUserRead =  00400, rfUserWrite =   00200, rfUserExec =  00100,
    rfGroupRead = 00040, rfGroupWrite =  00020, rfGroupExec = 00010,
    rfOtherRead = 00004, rfOtherWrite =  00002, rfOtherExec = 00001,
    rfRead =      00444, rfWrite =       00222, rfExec =      00111,
    rfNo =        00000, rfDefault =     00644, rfAll =       00777,
    rfSpecials =  07000, rfAllSpecials = 07777,
    rfS3Read = rfOtherRead, rfS3Write = rfOtherWrite, rfS3ReadACP = rfOtherExec, rfS3WriteACP = rfStickyBit,
     };
  enum TUnsupportedFlag {
    rfDirectory  = 040000 };
  enum TState { rsNo, rsYes, rsUndef };

public:
  static TFlag __fastcall RightToFlag(TRight Right);
  static TRight CalculateRight(TRightGroup Group, TRightLevel Level);
  static TFlag CalculateFlag(TRightGroup Group, TRightLevel Level);
  static unsigned short CalculatePermissions(TRightGroup Group, TRightLevel Level, TRightLevel Level2 = rlNone, TRightLevel Level3 = rlNone);

  __fastcall TRights();
  __fastcall TRights(const TRights & Source);
  __fastcall TRights(unsigned short Number);
  void __fastcall Assign(const TRights * Source);
  void __fastcall AddExecute();
  void __fastcall AllUndef();
  TRights Combine(const TRights & Other) const;
  void SetTextOverride(const UnicodeString & value);

  bool __fastcall operator ==(const TRights & rhr) const;
  bool __fastcall operator ==(unsigned short rhr) const;
  bool __fastcall operator !=(const TRights & rhr) const;
  TRights & __fastcall operator =(const TRights & rhr);
  TRights & __fastcall operator =(unsigned short rhr);
  TRights __fastcall operator ~() const;
  TRights __fastcall operator &(unsigned short rhr) const;
  TRights __fastcall operator &(const TRights & rhr) const;
  TRights & __fastcall operator &=(unsigned short rhr);
  TRights & __fastcall operator &=(const TRights & rhr);
  TRights __fastcall operator |(unsigned short rhr) const;
  TRights __fastcall operator |(const TRights & rhr) const;
  TRights & __fastcall operator |=(unsigned short rhr);
  TRights & __fastcall operator |=(const TRights & rhr);
  __fastcall operator unsigned short() const;
  __fastcall operator unsigned long() const;

  UnicodeString __fastcall GetChmodStr(int Directory) const;

  __property bool AllowUndef = { read = FAllowUndef, write = SetAllowUndef };
  __property bool IsUndef = { read = GetIsUndef };
  __property UnicodeString ModeStr = { read = GetModeStr };
  __property UnicodeString Octal = { read = GetOctal, write = SetOctal };
  __property unsigned short Number = { read = GetNumber, write = SetNumber };
  __property unsigned short NumberSet = { read = FSet };
  __property unsigned short NumberUnset = { read = FUnset };
  __property unsigned long NumberDecadic = { read = GetNumberDecadic };
  __property bool ReadOnly = { read = GetReadOnly, write = SetReadOnly };
  __property bool Right[TRight Right] = { read = GetRight, write = SetRight };
  __property TState RightUndef[TRight Right] = { read = GetRightUndef, write = SetRightUndef };
  __property UnicodeString Text = { read = GetText, write = SetText };
  __property bool Unknown = { read = FUnknown };

private:
  bool FAllowUndef;
  unsigned short FSet;
  unsigned short FUnset;
  UnicodeString FText;
  bool FUnknown;

  bool __fastcall GetIsUndef() const;
  UnicodeString __fastcall GetModeStr() const;
  void __fastcall SetNumber(unsigned short value);
  UnicodeString __fastcall GetText() const;
  void __fastcall SetText(const UnicodeString & value);
  void __fastcall SetOctal(UnicodeString value);
  unsigned short __fastcall GetNumber() const;
  unsigned short __fastcall GetNumberSet() const;
  unsigned short __fastcall GetNumberUnset() const;
  unsigned long __fastcall GetNumberDecadic() const;
  UnicodeString __fastcall GetOctal() const;
  bool __fastcall GetReadOnly();
  bool __fastcall GetRight(TRight Right) const;
  TState __fastcall GetRightUndef(TRight Right) const;
  void __fastcall SetAllowUndef(bool value);
  void __fastcall SetReadOnly(bool value);
  void __fastcall SetRight(TRight Right, bool value);
  void __fastcall SetRightUndef(TRight Right, TState value);
};
//---------------------------------------------------------------------------
enum TValidProperty { vpRights, vpGroup, vpOwner, vpModification, vpLastAccess, vpEncrypt, vpTags };
typedef Set<TValidProperty, vpRights, vpTags> TValidProperties;
class TRemoteProperties
{
public:
  TValidProperties Valid;
  bool Recursive;
  TRights Rights;
  bool AddXToDirectories;
  TRemoteToken Group;
  TRemoteToken Owner;
  __int64 Modification; // unix time
  __int64 LastAccess; // unix time
  bool Encrypt;
  UnicodeString Tags;

  __fastcall TRemoteProperties();
  __fastcall TRemoteProperties(const TRemoteProperties & rhp);
  bool __fastcall operator ==(const TRemoteProperties & rhp) const;
  bool __fastcall operator !=(const TRemoteProperties & rhp) const;
  void __fastcall Default();
  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;

  static TRemoteProperties __fastcall CommonProperties(TStrings * FileList);
  static TRemoteProperties __fastcall ChangedProperties(
    const TRemoteProperties & OriginalProperties, TRemoteProperties NewProperties);
};
//---------------------------------------------------------------------------
class TSynchronizeChecklist
{
friend class TTerminal;

public:
  enum TAction {
    saNone, saUploadNew, saDownloadNew, saUploadUpdate, saDownloadUpdate, saDeleteRemote, saDeleteLocal };
  static const int ActionCount = saDeleteLocal;

  class TItem
  {
  friend class TTerminal;
  friend class TSynchronizeChecklist;

  public:
    struct TFileInfo
    {
      UnicodeString FileName;
      UnicodeString Directory;
      TDateTime Modification;
      TModificationFmt ModificationFmt;
      __int64 Size;
    };

    TAction Action;
    bool IsDirectory;
    TFileInfo Local;
    TFileInfo Remote;
    int ImageIndex;
    bool Checked;
    TRemoteFile * RemoteFile;

    const UnicodeString& GetFileName() const;
    bool IsRemoteOnly() const { return (Action == saDownloadNew) || (Action == saDeleteRemote); }
    bool IsLocalOnly() const { return (Action == saUploadNew) || (Action == saDeleteLocal); }
    bool HasSize() const { return !IsDirectory || FDirectoryHasSize; }
    __int64 __fastcall GetBaseSize() const;
    __int64 __fastcall GetSize() const;
    __int64 __fastcall GetSize(TAction AAction) const;
    UnicodeString GetLocalPath() const;
    // Contrary to RemoteFile->FullFileName, this does not include trailing slash for directories
    UnicodeString GetRemotePath() const;
    UnicodeString GetLocalTarget() const;
    UnicodeString GetRemoteTarget() const;
    TStrings * GetFileList() const;

    ~TItem();

  private:
    FILETIME FLocalLastWriteTime;
    bool FDirectoryHasSize;

    TItem();
    __int64 __fastcall GetBaseSize(TAction AAction) const;
  };

  typedef std::vector<const TSynchronizeChecklist::TItem *> TItemList;

  ~TSynchronizeChecklist();

  void __fastcall Update(const TItem * Item, bool Check, TAction Action);
  void __fastcall UpdateDirectorySize(const TItem * Item, __int64 Size);
  void Delete(const TItem * Item);

  static TAction __fastcall Reverse(TAction Action);
  static bool __fastcall IsItemSizeIrrelevant(TAction Action);
  bool GetNextChecked(int & Index, const TItem *& Item) const;

  __property int Count = { read = GetCount };
  __property int CheckedCount = { read = GetCheckedCount };
  __property const TItem * Item[int Index] = { read = GetItem };

  static int Compare(const TItem * Item1, const TItem * Item2);

protected:
  TSynchronizeChecklist();

  void Sort();
  void Add(TItem * Item);

  int GetCount() const;
  int GetCheckedCount() const;
  const TItem * GetItem(int Index) const;

private:
  TList * FList;

  static int __fastcall Compare(void * Item1, void * Item2);
};
//---------------------------------------------------------------------------
class TFileOperationProgressType;
//---------------------------------------------------------------------------
class TSynchronizeProgress
{
public:
  TSynchronizeProgress(const TSynchronizeChecklist * Checklist);

  void ItemProcessed(const TSynchronizeChecklist::TItem * ChecklistItem);
  int Progress(const TFileOperationProgressType * CurrentItemOperationProgress) const;
  TDateTime TimeLeft(const TFileOperationProgressType * CurrentItemOperationProgress) const;

private:
  const TSynchronizeChecklist * FChecklist;
  mutable __int64 FTotalSize;
  __int64 FProcessedSize;

  __int64 ItemSize(const TSynchronizeChecklist::TItem * ChecklistItem) const;
  __int64 GetProcessed(const TFileOperationProgressType * CurrentItemOperationProgress) const;
};
//---------------------------------------------------------------------------
bool __fastcall IsUnixStyleWindowsPath(const UnicodeString & Path);
bool __fastcall UnixIsAbsolutePath(const UnicodeString & Path);
UnicodeString __fastcall UnixIncludeTrailingBackslash(const UnicodeString & Path);
UnicodeString __fastcall UnixExcludeTrailingBackslash(const UnicodeString & Path, bool Simple = false);
UnicodeString __fastcall SimpleUnixExcludeTrailingBackslash(const UnicodeString & Path);
UnicodeString __fastcall UnixCombinePaths(const UnicodeString & Path1, const UnicodeString & Path2);
UnicodeString __fastcall UnixExtractFileDir(const UnicodeString & Path);
UnicodeString __fastcall UnixExtractFilePath(const UnicodeString & Path);
UnicodeString __fastcall UnixExtractFileName(const UnicodeString & Path);
UnicodeString ExtractShortName(const UnicodeString & Path, bool Unix);
UnicodeString __fastcall UnixExtractFileExt(const UnicodeString & Path);
Boolean __fastcall UnixSamePath(const UnicodeString & Path1, const UnicodeString & Path2);
bool __fastcall UnixIsChildPath(const UnicodeString & Parent, const UnicodeString & Child);
bool __fastcall ExtractCommonPath(TStrings * Files, UnicodeString & Path);
bool __fastcall UnixExtractCommonPath(TStrings * Files, UnicodeString & Path);
UnicodeString __fastcall ExtractFileName(const UnicodeString & Path, bool Unix);
bool __fastcall IsUnixRootPath(const UnicodeString & Path);
bool __fastcall IsUnixHiddenFile(const UnicodeString & Path);
UnicodeString __fastcall AbsolutePath(const UnicodeString & Base, const UnicodeString & Path);
UnicodeString __fastcall FromUnixPath(const UnicodeString & Path);
UnicodeString __fastcall ToUnixPath(const UnicodeString & Path);
UnicodeString __fastcall MinimizeName(const UnicodeString & FileName, int MaxLen, bool Unix);
UnicodeString __fastcall MakeFileList(TStrings * FileList);
TDateTime __fastcall ReduceDateTimePrecision(TDateTime DateTime,
  TModificationFmt Precision);
TModificationFmt __fastcall LessDateTimePrecision(
  TModificationFmt Precision1, TModificationFmt Precision2);
UnicodeString __fastcall UserModificationStr(TDateTime DateTime,
  TModificationFmt Precision);
UnicodeString __fastcall ModificationStr(TDateTime DateTime,
  TModificationFmt Precision);
int GetPartialFileExtLen(const UnicodeString & FileName);
int __fastcall FakeFileImageIndex(UnicodeString FileName, unsigned long Attrs = 0,
  UnicodeString * TypeName = NULL);
bool __fastcall SameUserName(const UnicodeString & UserName1, const UnicodeString & UserName2);
UnicodeString __fastcall FormatMultiFilesToOneConfirmation(const UnicodeString & Target, bool Unix);
//---------------------------------------------------------------------------
#endif
