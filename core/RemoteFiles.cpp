//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "RemoteFiles.h"

#include <SysUtils.hpp>

#include "Common.h"
#include "Exceptions.h"
#include "Interface.h"
#include "Terminal.h"
#include "TextsCore.h"
/* TODO 1 : Path class instead of AnsiString (handle relativity...) */
//---------------------------------------------------------------------------
AnsiString __fastcall UnixIncludeTrailingBackslash(const AnsiString Path)
{
  if (!Path.IsDelimiter("/", Path.Length())) return Path + "/";
    else return Path;
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnixExcludeTrailingBackslash(const AnsiString Path)
{
  if ((Path.Length() > 1) && Path.IsDelimiter("/", Path.Length()))
      return Path.SubString(1, Path.Length() - 1);
    else return Path;
}
//---------------------------------------------------------------------------
Boolean __fastcall UnixComparePaths(const AnsiString Path1, const AnsiString Path2)
{
  return (UnixIncludeTrailingBackslash(Path1) == UnixIncludeTrailingBackslash(Path2));
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnixExtractFileDir(const AnsiString Path)
{
  Integer Pos = Path.LastDelimiter('/');
  if (Pos > 1) return Path.SubString(1, Pos - 1);
    else
  if (Pos == 1) return "/";
    else return Path;
}
//---------------------------------------------------------------------------
// must return trailing backslash
AnsiString __fastcall UnixExtractFilePath(const AnsiString Path)
{
  Integer Pos = Path.LastDelimiter('/');
  if (Pos) return Path.SubString(1, Pos);
    else return Path;
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnixExtractFileName(const AnsiString Path)
{
  Integer Pos = Path.LastDelimiter('/');
  if (Pos) return Path.SubString(Pos + 1, Path.Length() - Pos);
    else return Path;
}
//---------------------------------------------------------------------------
AnsiString __fastcall UnixExtractFileExt(const AnsiString Path)
{
  AnsiString FileName = UnixExtractFileName(Path);
  Integer Pos = FileName.LastDelimiter(".");
  if (Pos) return Path.SubString(Pos, Path.Length() - Pos + 1);
    else return "";
}
//---------------------------------------------------------------------------
void __fastcall DateTimeParams(TDateTime * AUnixEpoch, double * ADifference)
{
  static double Difference;
  static TDateTime UnixEpoch = 0;

  if (double(UnixEpoch) == 0)
  {
    TIME_ZONE_INFORMATION TZI;
    unsigned long GTZI;

    GTZI = GetTimeZoneInformation(&TZI);
    switch (GTZI) {
      case TIME_ZONE_ID_UNKNOWN:
        Difference = 0;
        break;

      case TIME_ZONE_ID_STANDARD:
        Difference = double(TZI.Bias + TZI.StandardBias) / 1440;
        break;

      case TIME_ZONE_ID_DAYLIGHT:
        Difference = double(TZI.Bias + TZI.DaylightBias) / 1440;
        break;

      case TIME_ZONE_ID_INVALID:
      default:
        throw Exception(TIMEZONE_ERROR);
    }
    // Is it same as SysUtils::UnixDateDelta = 25569 ?? 
    UnixEpoch = EncodeDate(1970, 1, 1);
  }
  if (AUnixEpoch) *AUnixEpoch = UnixEpoch;
  if (ADifference) *ADifference = Difference;
}
//---------------------------------------------------------------------------
TDateTime __fastcall UnixToDateTime(unsigned long TimeStamp)
{
  TDateTime UnixEpoch;
  double Difference;
  DateTimeParams(&UnixEpoch, &Difference);

  TDateTime Result;
  Result = UnixEpoch + (double(TimeStamp) / 86400) - Difference;
  return Result;
}
//---------------------------------------------------------------------------
FILETIME __fastcall DateTimeToFileTime(const TDateTime DateTime)
{
  unsigned long UnixTimeStamp;
  FILETIME Result;
  TDateTime UnixEpoch;
  double Difference;

  DateTimeParams(&UnixEpoch, &Difference);
  UnixTimeStamp = (unsigned long)((double(DateTime - UnixEpoch + Difference) * 86400));
  TIME_POSIX_TO_WIN(UnixTimeStamp, Result);
  return Result;
}
//---------------------------------------------------------------------------
TDateTime AdjustDateTimeFromUnix(const TDateTime DateTime)
{
  // to be implemented
  return DateTime;
}
//- TRemoteFiles ------------------------------------------------------------
__fastcall TRemoteFile::TRemoteFile(TRemoteFile * ALinkedByFile):
  TPersistent()
{
  FLinkedFile = NULL;
  FRights = new TRights();
  FIconIndex = -1;
  FCyclicLink = false;
  FModificationFmt = mfFull;
  FLinkedByFile = ALinkedByFile;
  FTerminal = NULL;
  FDirectory = NULL;
}
//---------------------------------------------------------------------------
__fastcall TRemoteFile::~TRemoteFile()
{
  delete FRights;
  delete FLinkedFile;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TRemoteFile::Duplicate()
{
  TRemoteFile * Result;
  Result = new TRemoteFile();
  try
  {
    if (FLinkedFile)
    {
      Result->FLinkedFile = FLinkedFile->Duplicate();
      Result->FLinkedFile->FLinkedByFile = Result;
    }
    *Result->Rights = *FRights;
    #define COPY_FP(PROP) Result->F ## PROP = F ## PROP;
    COPY_FP(Terminal);
    COPY_FP(Owner);
    COPY_FP(ModificationFmt);
    COPY_FP(Size);
    COPY_FP(FileName);
    COPY_FP(INodeBlocks);
    COPY_FP(Modification);
    COPY_FP(Group);
    COPY_FP(IconIndex);
    COPY_FP(IsSymLink);
    COPY_FP(LinkTo);
    COPY_FP(Type);
    COPY_FP(Selected);
    COPY_FP(CyclicLink);
    #undef COPY_FP
  }
  catch(...)
  {
    delete Result;
    throw;
  }
  return Result;
}
//---------------------------------------------------------------------------
Integer __fastcall TRemoteFile::GetIconIndex()
{
  assert(FIconIndex >= -1);
  if (FIconIndex < 0)
  {
    /* TODO : If file is link: Should be attributes taken from linked file? */
    unsigned long Attrs = FILE_ATTRIBUTE_NORMAL;
    if (IsDirectory) Attrs |= FILE_ATTRIBUTE_DIRECTORY;
    if (IsHidden) Attrs |= FILE_ATTRIBUTE_HIDDEN;

    TSHFileInfo SHFileInfo;
    AnsiString DumbFileName = (IsSymLink ? LinkTo : FileName);
    // On Win2k we get icon of "ZIP drive" for ".." (parent directory)
    if (DumbFileName == "..") DumbFileName = "dumb";

    SHGetFileInfo(DumbFileName.c_str(),
      Attrs, &SHFileInfo, sizeof(SHFileInfo),
      SHGFI_SYSICONINDEX | SHGFI_USEFILEATTRIBUTES);
    FIconIndex = SHFileInfo.iIcon;
  }
  return FIconIndex;
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetIsHidden()
{
  return (!IsParentDirectory && !IsThisDirectory &&
    !FileName.IsEmpty() && (FileName[1] == '.'));
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetIsDirectory() const
{
  return (toupper(Type) == FILETYPE_DIRECTORY);
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetIsParentDirectory()
{
  return (FileName == PARENTDIRECTORY);
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetIsThisDirectory()
{
  return (FileName == THISDIRECTORY);
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetIsInaccesibleDirectory()
{
  Boolean Result;
  if (IsDirectory)
  {
    assert(Terminal);
    Result = !
       (((Rights->RightUndef[rfOtherExec] != rsNo)) ||
        ((Rights->Right[rfGroupExec] != rsNo) &&
         (Terminal->UserGroups->IndexOf(Group) >= 0)) ||
        ((Rights->Right[rfUserExec] != rsNo) &&
         (AnsiCompareText(Terminal->UserName, Owner) == 0)));
  }
    else Result = False;
  return Result;
}
//---------------------------------------------------------------------------
char __fastcall TRemoteFile::GetType() const
{
  if (IsSymLink && FLinkedFile) return FLinkedFile->Type;
    else return FType;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetType(char AType)
{
  FType = AType;
  // Allow even non-standard file types (e.g. 'S')
  // if (!AnsiString("-DL").Pos((Char)toupper(FType))) Abort();
  FIsSymLink = ((Char)toupper(FType) == FILETYPE_SYMLINK);
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TRemoteFile::GetLinkedFile()
{
  // it would be called releatedly for broken symlinks
  //if (!FLinkedFile) FindLinkedFile();
  return FLinkedFile;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetLinkedFile(TRemoteFile * value)
{
  if (FLinkedFile != value)
  {
    if (FLinkedFile) delete FLinkedFile;
    FLinkedFile = value;
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFile::GetBrokenLink()
{
  // If file is symlink but we couldn't find linked file we assume broken link
  return (IsSymLink && (FCyclicLink || !FLinkedFile));
  // "!FLinkTo.IsEmpty()" removed because it does not work with SFTP 
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetModification(const TDateTime & value)
{
  if (FModification != value)
  {
    FModificationFmt = mfFull;
    FModification = value;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetUserModificationStr()
{
  if (FModificationFmt == mfFull)
  {
    return FormatDateTime("ddddd tt", Modification);
  }
  else
  {
    return FormatDateTime("ddddd t", Modification);
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetModificationStr()
{
  Word Year, Month, Day, Hour, Min, Sec, MSec;
  Modification.DecodeDate(&Year, &Month, &Day);
  Modification.DecodeTime(&Hour, &Min, &Sec, &MSec);
  if (FModificationFmt != mfMDY)
    return FORMAT("%3s %2d %2d:%2.2d",
      (EngShortMonthNames[Month-1], Day, Hour, Min));
  else
    return FORMAT("%3s %2d %2d",
      (EngShortMonthNames[Month-1], Day, Year));
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetExtension()
{
  return UnixExtractFileExt(FFileName);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetRights(TRights * value)
{
  FRights->Assign(value);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetRightsStr()
{
  return FRights->Text;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetListingStr(AnsiString value)
{
  // Value stored in 'value' can be used for error message
  AnsiString Line = value;
  FIconIndex = -1;
  try {
    AnsiString Col;

    // Do we need to do this (is ever TAB is LS output)?
    Line = ReplaceChar(Line, '\t', ' ');

    Type = Line[1];
    Line.Delete(1, 1);

    #define GETNCOL  \
      { if (Line.IsEmpty()) throw Exception(""); \
        Integer P = Line.Pos(' '); \
        if (P) { Col = Line.SubString(1, P-1); Line.Delete(1, P); } \
          else { Col = Line; Line = ""; } \
      }
    #define GETCOL { GETNCOL; Line = TrimLeft(Line); }

    // Rights string may contain special permission attributes (S,t, ...)
    Rights->AllowUndef = True;
    // On some system there is no space between permissions and node blocks count columns
    // so we get only first 9 characters and trim all following spaces (if any)
    Rights->Text = Line.SubString(1, 9);
    Line.Delete(1, 9);
    // Rights column maybe followed by '+' sign, we ignore it
    if (!Line.IsEmpty() && (Line[1] == '+')) Line.Delete(1, 1);
    Line = Line.TrimLeft();

    GETCOL;
    FINodeBlocks = StrToInt(Col);

    GETCOL;
    FOwner = Col;

    // #60 17.10.01: group name can contain space
    FGroup = "";
    GETCOL;
    __int64 ASize;
    do
    {
      FGroup += Col;
      GETCOL;
      assert(!Col.IsEmpty());
      // for devices etc.. there is additional column ending by comma, we ignore it
      if (Col[Col.Length()] == ',') GETCOL;
      ASize = StrToInt64Def(Col, -1);
      // if it's not a number (file size) we take it as part of group name
      // (at least on CygWin, there can be group with space in its name)
      if (ASize < 0) Col = " " + Col;
    }
    while (ASize < 0);

    // do not read modification time and filename if it is already set
    if (double(FModification) == 0 && FileName.IsEmpty())
    {
      FSize = ASize;

      Word Day, Month, Year, Hour, Min, P;

      GETCOL;
      Month = 0;
      for (Word IMonth = 0; IMonth < 12; IMonth++)
        if (!Col.AnsiCompareIC(EngShortMonthNames[IMonth])) { Month = IMonth; Month++; break; }
      if (!Month) Abort();

      // don't trim possible leading space of year column
      // we need to know is space is before (most systems) or after year
      GETNCOL;
      Day = (Word)StrToInt(Col);
      if ((Day < 1) || (Day > 31)) Abort();

      // Time/Year indicator is always 5 charactes long (???), on most
      // systems year is aligned to right (_YYYY), but on some to left (YYYY_),
      // we must ensure that trailing space is also deleted, so real
      // separator space is not treated as part of file name
      Col = Line.SubString(1, 6).Trim();
      Line.Delete(1, 6);
      // GETNCOL; // We don't want to trim input strings (name with space at beginning???)
      // Check if we got time (contains :) or year
      if ((P = (Word)Col.Pos(':')) > 0)
      {
        Word CurrMonth, CurrDay;
        Hour = (Word)StrToInt(Col.SubString(1, P-1));
        Min = (Word)StrToInt(Col.SubString(P+1, Col.Length() - P));
        if (Hour > 23 || Hour > 59) Abort();
        // When we don't got year, we assume current year
        // with exception that the date would be in future
        // in this case we assume last year.
        DecodeDate(Date(), Year, CurrMonth, CurrDay);
        if ((Month > CurrMonth) ||
            (Month == CurrMonth && Day > CurrDay)) Year--;
        FModificationFmt = mfMDHM;
      }
        else
      {
        Year = (Word)StrToInt(Col);
        if (Year > 10000) Abort();
        // When we don't got time we assume midnight
        Hour = 0; Min = 0;
        FModificationFmt = mfMDY;
      }

      FModification = AdjustDateTimeFromUnix(
        EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, 0, 0));

        
      // separating space is already deleted, other spaces are treated as part of name

      {
        Integer P;

        FLinkTo = "";
        if (IsSymLink)
        {
          P = Line.Pos(SYMLINKSTR);
          if (P)
          {
            FLinkTo = Line.SubString(
              P + strlen(SYMLINKSTR), Line.Length() - P + strlen(SYMLINKSTR) + 1);
            Line.SetLength(P - 1);
          }
            else Abort();
        }
        FFileName = UnixExtractFileName(Line);
      }
    }

    #undef GETNCOL
    #undef GETCOL
  }
  catch (Exception &E)
  {
    throw ETerminal(&E, FmtLoadStr(LIST_LINE_ERROR, ARRAYOFCONST((value))));
  }

  if (IsSymLink) FindLinkedFile();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::FindLinkedFile()
{
  assert(Terminal && IsSymLink);

  if (FLinkedFile) delete FLinkedFile;
  FLinkedFile = NULL;

  FCyclicLink = false;
  if (!LinkTo.IsEmpty())
  {
    // check for cyclic link
    TRemoteFile * LinkedBy = FLinkedByFile;
    while (LinkedBy)
    {
      if (LinkedBy->LinkTo == LinkTo)
      {
        // this is currenly redundant information, because it is used only to
        // detect broken symlink, which would be otherwise detected
        // by FLinkedFile == NULL
        FCyclicLink = true;
        break;
      }
      LinkedBy = LinkedBy->FLinkedByFile;
    }
  }

  if (FCyclicLink)
  {
    TRemoteFile * LinkedBy = FLinkedByFile;
    while (LinkedBy)
    {
      LinkedBy->FCyclicLink = true;
      LinkedBy = LinkedBy->FLinkedByFile;
    }
  }
  else
  {
    Terminal->ExceptionOnFail = true;
    try
    {
      try
      {
        Terminal->ReadSymlink(this, FLinkedFile);
      }
      __finally
      {
        Terminal->ExceptionOnFail = false;
      }
    }
    catch (Exception &E)
    {
      if (E.InheritsFrom(__classid(EFatal))) throw;
        else HandleExtendedException(&E, this);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetListingStr()
{
  return Format("%s%s %3s %s-8s %-8s %9d %s %s%s", ARRAYOFCONST((
    Type, Rights->Text, IntToStr(INodeBlocks), Owner,
    Group, IntToStr(Size), ModificationStr, FileName,
    (FLinkedFile ? AnsiString(SYMLINKSTR) + FLinkedFile->FileName : AnsiString()))));
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFile::GetFullFileName()
{
  assert(Terminal);
  AnsiString Path;
  if (IsParentDirectory) Path = Directory->ParentPath;
    else
  if (IsDirectory) Path = UnixIncludeTrailingBackslash(Directory->FullDirectory + FileName);
    else Path = Directory->FullDirectory + FileName;
  return Terminal->TranslateLockedPath(Path, true);
}
//---------------------------------------------------------------------------
Integer __fastcall TRemoteFile::GetAttr()
{
  Integer Result = 0;
  if (Rights->ReadOnly) Result |= faReadOnly;
  if (IsHidden) Result |= faHidden;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFile::SetTerminal(TTerminal * value)
{
  FTerminal = value;
  if (FLinkedFile) FLinkedFile->Terminal = value;
}
//=== TRemoteFileList ------------------------------------------------------
__fastcall TRemoteFileList::TRemoteFileList():
  TObjectList()
{
}
//---------------------------------------------------------------------------
/*TRemoteFile * __fastcall TRemoteFileList::NewFile(TRemoteFile * ALinkedByFile)
{
  return new TRemoteFile(this, NULL, ALinkedByFile);
} */
//---------------------------------------------------------------------------
void __fastcall TRemoteFileList::AddFile(TRemoteFile * File)
{
  Add(File);
  File->Directory = this;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFileList::DuplicateTo(TRemoteFileList * Copy)
{
  Copy->Clear();
  for (int Index = 0; Index < Count; Index++)
  {
    TRemoteFile * File = Files[Index];
    Copy->AddFile(File->Duplicate());
  }
  Copy->FDirectory = Directory;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFileList::Clear()
{
  TObjectList::Clear();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteFileList::SetDirectory(AnsiString value)
{
  FDirectory = UnixExcludeTrailingBackslash(value);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFileList::GetFullDirectory()
{
  return UnixIncludeTrailingBackslash(Directory);
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TRemoteFileList::GetFiles(Integer Index)
{
  return (TRemoteFile *)Items[Index];
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteFileList::GetIsRoot()
{
  return (Directory == ROOTDIRECTORY);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRemoteFileList::GetParentPath()
{
  return UnixExtractFilePath(Directory);
}
//---------------------------------------------------------------------------
__int64 __fastcall TRemoteFileList::GetTotalSize()
{
  __int64 Result = 0;
  for (Integer Index = 0; Index < Count; Index++)
    if (!Files[Index]->IsDirectory) Result += Files[Index]->Size;
  return Result;
}
//---------------------------------------------------------------------------
TRemoteFile * __fastcall TRemoteFileList::FindFile(const AnsiString &FileName)
{
  for (Integer Index = 0; Index < Count; Index++)
    if (Files[Index]->FileName == FileName) return Files[Index];
  return NULL;
}
//=== TRemoteDirectory ------------------------------------------------------
__fastcall TRemoteDirectory::TRemoteDirectory(TTerminal * aTerminal):
  TRemoteFileList(), FTerminal(aTerminal)
{
  FSelectedFiles = NULL;
  FThisDirectory = NULL;
  FParentDirectory = NULL;
  FIncludeThisDirectory = false;
  FIncludeParentDirectory = true;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::Clear()
{
  if (ThisDirectory && !IncludeThisDirectory)
  {
    delete FThisDirectory;
    FThisDirectory = NULL;
  }
  if (ParentDirectory && !IncludeParentDirectory)
  {
    delete FParentDirectory;
    FParentDirectory = NULL;
  }

  TRemoteFileList::Clear();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::SetDirectory(AnsiString value)
{
  TRemoteFileList::SetDirectory(value);
  //Load();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::AddFile(TRemoteFile * File)
{
  if (File->IsThisDirectory) FThisDirectory = File;
  if (File->IsParentDirectory) FParentDirectory = File;

  if ((!File->IsThisDirectory || IncludeThisDirectory) &&
      (!File->IsParentDirectory || IncludeParentDirectory))
  {
    TRemoteFileList::AddFile(File);
  }
  File->Terminal = Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::DuplicateTo(TRemoteFileList * Copy)
{
  TRemoteFileList::DuplicateTo(Copy);
  if (ThisDirectory && !IncludeThisDirectory)
  {
    Copy->AddFile(ThisDirectory->Duplicate());
  }
  if (ParentDirectory && !IncludeParentDirectory)
  {
    Copy->AddFile(ParentDirectory->Duplicate());
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TRemoteDirectory::GetLoaded()
{
  return ((Terminal != NULL) && Terminal->Active && !Directory.IsEmpty());
}
//---------------------------------------------------------------------------
/*TRemoteFile * __fastcall TRemoteDirectory::NewFile(TRemoteFile * ALinkedByFile)
{
  return new TRemoteFile(this, Terminal, ALinkedByFile);
} */
//---------------------------------------------------------------------------
TStrings * __fastcall TRemoteDirectory::GetSelectedFiles()
{
  if (!FSelectedFiles) FSelectedFiles = new TStringList();
    else FSelectedFiles->Clear();
  for (Integer Index = 0; Index < Count; Index ++)
    if (Files[Index]->Selected) FSelectedFiles->Add(Files[Index]->FullFileName);
  return FSelectedFiles;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::SetIncludeParentDirectory(Boolean value)
{
  if (IncludeParentDirectory != value)
  {
    FIncludeParentDirectory = value;
    if (value && ParentDirectory)
    {
      assert(IndexOf(ParentDirectory) < 0);
      Add(ParentDirectory);
    }
      else
    if (!value && ParentDirectory)
    {
      assert(IndexOf(ParentDirectory) >= 0);
      Extract(ParentDirectory);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectory::SetIncludeThisDirectory(Boolean value)
{
  if (IncludeThisDirectory != value)
  {
    FIncludeThisDirectory = value;
    if (value && ThisDirectory)
    {
      assert(IndexOf(ThisDirectory) < 0);
      Add(ThisDirectory);
    }
      else
    if (!value && ThisDirectory)
    {
      assert(IndexOf(ThisDirectory) >= 0);
      Extract(ThisDirectory);
    }
  }
}
//===========================================================================
__fastcall TRemoteDirectoryCache::TRemoteDirectoryCache(): TStringList()
{
  Sorted = true;
  Duplicates = dupError;
  CaseSensitive = true;
}
//---------------------------------------------------------------------------
__fastcall TRemoteDirectoryCache::~TRemoteDirectoryCache()
{
  for (int Index = 0; Index < Count; Index++)
  {
    delete (TRemoteFileList *)Objects[Index];
    Objects[Index] = NULL;
  }
}
//---------------------------------------------------------------------------
TRemoteFileList * __fastcall TRemoteDirectoryCache::GetFileList(const AnsiString Directory)
{
  int Index = IndexOf(UnixExcludeTrailingBackslash(Directory));
  return (Index >= 0 ? (TRemoteFileList *)Objects[Index] : NULL);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectoryCache::AddFileList(TRemoteFileList * FileList)
{
  assert(FileList);
  TRemoteFileList * Copy = new TRemoteFileList();
  FileList->DuplicateTo(Copy);
  AddObject(Copy->Directory, Copy);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectoryCache::ClearFileList(AnsiString Directory, bool SubDirs)
{
  Directory = UnixExcludeTrailingBackslash(Directory);
  int Index = IndexOf(Directory);
  if (Index >= 0)
  {
    Delete(Index);
  }
  if (SubDirs)
  {
    Directory = UnixIncludeTrailingBackslash(Directory);
    Index = Count-1;
    while (Index >= 0)
    {
      if (Strings[Index].SubString(1, Directory.Length()) == Directory)
      {
        Delete(Index);
      }
      Index--;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRemoteDirectoryCache::Delete(int Index)
{
  delete (TRemoteFileList *)Objects[Index];
  TStringList::Delete(Index);
}
//=== TRights ---------------------------------------------------------------
__fastcall TRights::TRights()
{
  FAllowUndef = False;
  FText.SetLength(RightsFlagCount);
  Number = 0;
}
//---------------------------------------------------------------------------
__fastcall TRights::TRights(Word aNumber)
{
  FAllowUndef = False;
  FText.SetLength(RightsFlagCount);
  Number = aNumber;
}
//---------------------------------------------------------------------------
__fastcall TRights::TRights(const TRights & Source)
{
  Assign(&Source);
}
//---------------------------------------------------------------------------
void __fastcall TRights::Assign(const TRights * Source)
{
  AllowUndef = Source->AllowUndef;
  Text = Source->Text;
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetText(AnsiString value)
{
  if (value != FText)
  {
    if ((value.Length() != RightsFlagCount) ||
        (!AllowUndef && value.Pos(UNDEFRIGHT)) ||
        value.Pos(" "))
          throw Exception(FmtLoadStr(RIGHTS_ERROR, ARRAYOFCONST((value))));
    FText = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetOctal(AnsiString value)
{
  assert(value.Length() == 3);
  #pragma option push -w-sig
  Number =
    ((value[1] - '0') << 6) +
    ((value[2] - '0') << 3) +
    ((value[3] - '0') << 0);
  #pragma option pop
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetNumber(Word value)
{
  for (int Index = 0; Index < RightsFlagCount; Index++)
    Right[(TRightsFlag)Index] = (Boolean)((value & (1 << (RightsFlagCount - 1 - Index))) != 0);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRights::GetOctal() const
{
  AnsiString Result;
  Result += (char)('0' + ((Number & 0700) >> 6));
  Result += (char)('0' + ((Number & 0070) >> 3));
  Result += (char)('0' + ((Number & 0007) >> 0));

  return Result;
}
//---------------------------------------------------------------------------
Word __fastcall TRights::CalcNumber(TRightState State, Boolean
#ifdef _DEBUG
  AllowUndef
#endif
) const
{
  Word Result = 0;
  for (int Index = 0; Index < RightsFlagCount; Index++)
  {
    TRightState AState = RightUndef[(TRightsFlag)Index];
    assert(AllowUndef || (AState != rsUndef));
    if (AState == State) Result |= (Word)(1 << (RightsFlagCount - 1 - Index));
  }
  return Result;
}
//---------------------------------------------------------------------------
Word __fastcall TRights::GetNumber() const
{
  return CalcNumber(rsYes, False);
}
//---------------------------------------------------------------------------
Word __fastcall TRights::GetNumberSet() const
{
  return CalcNumber(rsYes, True);
}
//---------------------------------------------------------------------------
Word __fastcall TRights::GetNumberUnset() const
{
  return CalcNumber(rsNo, True);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRights::GetText() const
{
  return FText;
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetAllowUndef(Boolean value)
{
  if (FAllowUndef != value)
  {
    assert(!value || !FText.Pos(UNDEFRIGHT));
    FAllowUndef = value;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRights::GetFullRights() const
{
  return FULLRIGHTS;
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetRight(TRightsFlag Flag, Boolean value)
{
  RightUndef[Flag] = (value ? rsYes : rsNo);
}
//---------------------------------------------------------------------------
Boolean __fastcall TRights::GetRight(TRightsFlag Flag) const
{
  TRightState State = RightUndef[Flag];
  assert(State != rsUndef);
  return (State == rsYes);
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetRightUndef(TRightsFlag Flag, TRightState value)
{
  if (value != RightUndef[Flag])
  {
    assert((value != rsUndef) || AllowUndef);
    switch (value) {
      case rsUndef: FText[Flag+1] = UNDEFRIGHT; break;
      case rsNo: FText[Flag+1] = NORIGHT; break;
      default: FText[Flag+1] = FullRights[Flag+1];
    }
  }
}
//---------------------------------------------------------------------------
TRightState __fastcall TRights::GetRightUndef(TRightsFlag Flag) const
{
  Char FlagChar = Text[Flag+1];
  if (FlagChar == NORIGHT) return rsNo;
    else
  if (FlagChar == FULLRIGHTS[Flag]) return rsYes;
    else return rsUndef;
}
//---------------------------------------------------------------------------
TRights __fastcall TRights::operator |(const TRights & rhr) const
{
  TRights Result = *this;
  Result |= rhr;
  return Result;
}
//---------------------------------------------------------------------------
TRights __fastcall TRights::operator |(Integer rhr) const
{
  TRights Result = *this;
  Result |= rhr;
  return Result;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator =(const TRights & rhr)
{
  Assign(&rhr);
  return *this;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator =(Integer rhr)
{
  Number = (Word)rhr;
  return *this;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator |=(const TRights & rhr)
{
  Number |= rhr.Number;
  return *this;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator |=(Integer rhr)
{
  Number |= (Word)rhr;
  return *this;
}
//---------------------------------------------------------------------------
TRights __fastcall TRights::operator &(Integer rhr) const
{
  TRights Result = *this;
  Result &= rhr;
  return Result;
}
//---------------------------------------------------------------------------
TRights __fastcall TRights::operator &(const TRights & rhr) const
{
  TRights Result = *this;
  Result &= rhr;
  return Result;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator &=(Integer rhr)
{
  Number &= (Word)rhr;
  return *this;
}
//---------------------------------------------------------------------------
TRights & __fastcall TRights::operator &=(const TRights & rhr)
{
  if (AllowUndef)
  {
    for (int Index = 0; Index < RightsFlagCount; Index++)
      if (RightUndef[(TRightsFlag)Index] != rhr.RightUndef[(TRightsFlag)Index])
        RightUndef[(TRightsFlag)Index] = rsUndef;
  }
    else Number &= rhr.Number;
  return *this;
}
//---------------------------------------------------------------------------
TRights __fastcall TRights::operator ~() const
{
  TRights Result;
  Result.Number = (Word)~Number;
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRights::operator ==(const TRights & rhr) const
{
  if (AllowUndef || rhr.AllowUndef)
  {
    for (int Index = 0; Index < RightsFlagCount; Index++)
      if (RightUndef[(TRightsFlag)Index] != rhr.RightUndef[(TRightsFlag)Index])
        return False;
    return True;
  }
    else return (bool)(Number == rhr.Number);
}
//---------------------------------------------------------------------------
bool __fastcall TRights::operator ==(Integer rhr) const
{
  return (bool)(Number == rhr);
}
//---------------------------------------------------------------------------
bool __fastcall TRights::operator !=(const TRights & rhr) const
{
  return !(*this == rhr);
}
//---------------------------------------------------------------------------
void __fastcall TRights::SetReadOnly(Boolean value)
{
  Right[rfUserWrite] = !value;
  Right[rfGroupWrite] = !value;
  Right[rfOtherWrite] = !value;
}
//---------------------------------------------------------------------------
Boolean __fastcall TRights::GetReadOnly()
{
  return (Boolean)((NumberUnset & raWrite) == raWrite);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRights::GetSimplestStr() const
{
  return IsUndef ? ModeStr : Octal;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TRights::GetModeStr() const
{
  AnsiString Result = "";
  AnsiString SetModeStr, UnsetModeStr;
  TRightsFlag Flag;
  for (Integer Group = 0; Group < 3; Group++)
  {
    SetModeStr = "";
    UnsetModeStr = "";
    for (Integer Mode = 0; Mode < 3; Mode++)
    {
      Flag = (TRightsFlag)((Group * 3) + Mode);
      switch (RightUndef[Flag]) {
        case rsYes: SetModeStr += FULLRIGHTS[(int)Flag]; break;
        case rsNo: UnsetModeStr += FULLRIGHTS[(int)Flag]; break;
      }
    }
    if (!SetModeStr.IsEmpty() || !UnsetModeStr.IsEmpty())
    {
      if (!Result.IsEmpty()) Result += ',';
      Result += MODEGROUPS[Group];
      if (!SetModeStr.IsEmpty()) Result += "+" + SetModeStr;
      if (!UnsetModeStr.IsEmpty()) Result += "-" + UnsetModeStr;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRights::AddExecute()
{
  for (int Index = 0; Index < 3; Index++)
  {
    if (NumberSet & (0700 >> (Index * 3)))
    {
      Right[(TRightsFlag)(rfUserExec + (Index * 3))] = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRights::AllUndef()
{
  for (int Index = 0; Index < RightsFlagCount; Index++)
  {
    RightUndef[(TRightsFlag)Index] = rsUndef;
  }
}
//---------------------------------------------------------------------------
Boolean __fastcall TRights::GetIsUndef() const
{
  for (int Index = 0; Index < RightsFlagCount; Index++)
    if (RightUndef[(TRightsFlag)Index] == rsUndef)
      return True;
  return False;
}
//---------------------------------------------------------------------------
__fastcall TRights::operator unsigned short() const
{
  return Number;
}
//---------------------------------------------------------------------------
__fastcall TRights::operator unsigned long() const
{
  return Number;
}
//=== TRemoteProperties -------------------------------------------------------
__fastcall TRemoteProperties::TRemoteProperties()
{
  Valid.Clear();
  AddXToDirectories = false;
  Rights.AllowUndef = false;
  Rights.Number = 0;
  Group = "";
  Owner = "";
  Recursive = false;
};
/*//---------------------------------------------------------------------------
__fastcall TRemoteProperties::TRemoteProperties(const TRemoteProperties & Source)
{
  *this = Source;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteProperties::Clear()
{
};
//---------------------------------------------------------------------------
void __fastcall TRemoteProperties::operator =(const TRemoteProperties &rhp)
{
  Valid = rhp.Valid;
  Recursive = rhp.Recursive;
  Rights = rhp.Rights;
  AddXToDirectories = rhp.AddXToDirectories;
  Group = rhp.Group;
  Owner = rhp.Owner;
}  */
//---------------------------------------------------------------------------
bool __fastcall TRemoteProperties::operator ==(const TRemoteProperties & rhp) const
{
  bool Result = (Valid == rhp.Valid && Recursive == rhp.Recursive);

  if (Result)
  {
    if ((Valid.Contains(vpRights) &&
          (Rights != rhp.Rights || AddXToDirectories != rhp.AddXToDirectories)) ||
        (Valid.Contains(vpOwner) && Owner != rhp.Owner) ||
        (Valid.Contains(vpGroup) && Group != rhp.Group))
    {
      Result = false;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TRemoteProperties::operator !=(const TRemoteProperties & rhp) const
{
  return !(*this == rhp);
}


