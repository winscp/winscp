//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileMasks.h"

#include "Common.h"
#include "TextsCore.h"
#include "RemoteFiles.h"
#include "PuttyTools.h"
#include "Terminal.h"
#include <StrUtils.hpp>
//---------------------------------------------------------------------------
extern const wchar_t IncludeExcludeFileMasksDelimiter = L'|';
UnicodeString FileMasksDelimiters = L";,";
static UnicodeString AllFileMasksDelimiters = FileMasksDelimiters + IncludeExcludeFileMasksDelimiter;
static UnicodeString DirectoryMaskDelimiters = L"/\\";
static UnicodeString FileMasksDelimiterStr = UnicodeString(FileMasksDelimiters[1]) + L' ';
UnicodeString AnyMask = L"*.*";
//---------------------------------------------------------------------------
__fastcall EFileMasksException::EFileMasksException(
    UnicodeString Message, int AErrorStart, int AErrorLen) :
  Exception(Message)
{
  ErrorStart = AErrorStart;
  ErrorLen = AErrorLen;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MaskFilePart(const UnicodeString Part, const UnicodeString Mask, bool& Masked)
{
  UnicodeString Result;
  int RestStart = 1;
  bool Delim = false;
  for (int Index = 1; Index <= Mask.Length(); Index++)
  {
    switch (Mask[Index])
    {
      case L'\\':
        if (!Delim)
        {
          Delim = true;
          Masked = true;
          break;
        }

      case L'*':
        if (!Delim)
        {
          Result += Part.SubString(RestStart, Part.Length() - RestStart + 1);
          RestStart = Part.Length() + 1;
          Masked = true;
          break;
        }

      case L'?':
        if (!Delim)
        {
          if (RestStart <= Part.Length())
          {
            Result += Part[RestStart];
            RestStart++;
          }
          Masked = true;
          break;
        }

      default:
        Result += Mask[Index];
        RestStart++;
        Delim = false;
        break;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MaskFileName(UnicodeString FileName, const UnicodeString Mask)
{
  if (IsEffectiveFileNameMask(Mask))
  {
    bool Masked;
    int P = Mask.LastDelimiter(L".");
    if (P > 0)
    {
      int P2 = FileName.LastDelimiter(".");
      // only dot at beginning of file name is not considered as
      // name/ext separator
      UnicodeString FileExt = P2 > 1 ?
        FileName.SubString(P2 + 1, FileName.Length() - P2) : UnicodeString();
      FileExt = MaskFilePart(FileExt, Mask.SubString(P + 1, Mask.Length() - P), Masked);
      if (P2 > 1)
      {
        FileName.SetLength(P2 - 1);
      }
      FileName = MaskFilePart(FileName, Mask.SubString(1, P - 1), Masked);
      if (!FileExt.IsEmpty())
      {
        FileName += L"." + FileExt;
      }
    }
    else
    {
      FileName = MaskFilePart(FileName, Mask, Masked);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall IsFileNameMask(const UnicodeString & Mask)
{
  bool Result = Mask.IsEmpty(); // empty mask is the same as *
  if (!Result)
  {
    MaskFilePart(UnicodeString(), Mask, Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall IsEffectiveFileNameMask(const UnicodeString & Mask)
{
  return !Mask.IsEmpty() && (Mask != L"*") && (Mask != AnyMask);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall DelimitFileNameMask(UnicodeString Mask)
{
  for (int i = 1; i <= Mask.Length(); i++)
  {
    if (wcschr(L"\\*?", Mask[i]) != NULL)
    {
      Mask.Insert(L"\\", i);
      i++;
    }
  }
  return Mask;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TFileMasks::TParams::TParams() :
  Size(0)
{
}
//---------------------------------------------------------------------------
UnicodeString TFileMasks::TParams::ToString() const
{
  return UnicodeString(L"[") + IntToStr(Size) + L"/" + DateTimeToStr(Modification) + L"]";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::IsMask(const UnicodeString Mask)
{
  return (Mask.LastDelimiter(L"?*[") > 0);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFileMasks::NormalizeMask(const UnicodeString & Mask, const UnicodeString & AnyMask)
{
  if (!IsEffectiveFileNameMask(Mask))
  {
    return AnyMask;
  }
  else
  {
    return Mask;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFileMasks::ComposeMaskStr(
  TStrings * MasksStr, bool Directory)
{
  UnicodeString Result;
  UnicodeString ResultNoDirMask;
  for (int I = 0; I < MasksStr->Count; I++)
  {
    UnicodeString Str = MasksStr->Strings[I].Trim();
    if (!Str.IsEmpty())
    {
      for (int P = 1; P <= Str.Length(); P++)
      {
        if (Str.IsDelimiter(AllFileMasksDelimiters, P))
        {
          Str.Insert(Str[P], P);
          P++;
        }
      }

      UnicodeString StrNoDirMask;
      if (Directory)
      {
        StrNoDirMask = Str;
        Str = MakeDirectoryMask(Str);
      }
      else
      {
        while (Str.IsDelimiter(DirectoryMaskDelimiters, Str.Length()))
        {
          Str.SetLength(Str.Length() - 1);
        }
        StrNoDirMask = Str;
      }

      AddToList(Result, Str, FileMasksDelimiterStr);
      AddToList(ResultNoDirMask, StrNoDirMask, FileMasksDelimiterStr);
    }
  }

  // For directories, the above will add slash ay the end of masks,
  // breaking size and time masks and thus circumverting their validation.
  // This performes as hoc validation to cover the scenario.
  // For files this makes no difference, but no harm either
  TFileMasks Temp(Directory ? 1 : 0);
  Temp = ResultNoDirMask;

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFileMasks::ComposeMaskStr(
  TStrings * IncludeFileMasksStr, TStrings * ExcludeFileMasksStr,
  TStrings * IncludeDirectoryMasksStr, TStrings * ExcludeDirectoryMasksStr)
{
  UnicodeString IncludeMasks = ComposeMaskStr(IncludeFileMasksStr, false);
  AddToList(IncludeMasks, ComposeMaskStr(IncludeDirectoryMasksStr, true), FileMasksDelimiterStr);
  UnicodeString ExcludeMasks = ComposeMaskStr(ExcludeFileMasksStr, false);
  AddToList(ExcludeMasks, ComposeMaskStr(ExcludeDirectoryMasksStr, true), FileMasksDelimiterStr);

  UnicodeString Result = IncludeMasks;
  if (!ExcludeMasks.IsEmpty())
  {
    if (!Result.IsEmpty())
    {
      Result += L' ';
    }
    Result += UnicodeString(IncludeExcludeFileMasksDelimiter) + L' ' + ExcludeMasks;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks()
{
  Init();
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(int ForceDirectoryMasks)
{
  Init();
  FForceDirectoryMasks = ForceDirectoryMasks;
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const TFileMasks & Source)
{
  Init();
  FForceDirectoryMasks = Source.FForceDirectoryMasks;
  SetStr(Source.Masks, false);
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::TFileMasks(const UnicodeString & AMasks)
{
  Init();
  SetStr(AMasks, false);
}
//---------------------------------------------------------------------------
__fastcall TFileMasks::~TFileMasks()
{
  Clear();
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Init()
{
  FForceDirectoryMasks = -1;

  DoInit(false);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::DoInit(bool Delete)
{
  for (int Index = 0; Index < 4; Index++)
  {
    if (Delete)
    {
      delete FMasksStr[Index];
    }
    FMasksStr[Index] = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Clear()
{
  DoInit(true);

  for (int Index = 0; Index < 4; Index++)
  {
    Clear(FMasks[Index]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::Clear(TMasks & Masks)
{
  TMasks::iterator I = Masks.begin();
  while (I != Masks.end())
  {
    ReleaseMaskMask((*I).FileNameMask);
    ReleaseMaskMask((*I).DirectoryMask);
    I++;
  }
  Masks.clear();
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::MatchesMasks(const UnicodeString FileName, bool Directory,
  const UnicodeString Path, const TParams * Params, const TMasks & Masks, bool Recurse)
{
  bool Result = false;

  TMasks::const_iterator I = Masks.begin();
  while (!Result && (I != Masks.end()))
  {
    const TMask & Mask = *I;
    Result =
      MatchesMaskMask(Mask.DirectoryMask, Path) &&
      MatchesMaskMask(Mask.FileNameMask, FileName);

    if (Result)
    {
      bool HasSize = (Params != NULL);

      switch (Mask.HighSizeMask)
      {
        case TMask::None:
          Result = true;
          break;

        case TMask::Open:
          Result = HasSize && (Params->Size < Mask.HighSize);
          break;

        case TMask::Close:
          Result = HasSize && (Params->Size <= Mask.HighSize);
          break;
      }

      if (Result)
      {
        switch (Mask.LowSizeMask)
        {
          case TMask::None:
            Result = true;
            break;

          case TMask::Open:
            Result = HasSize && (Params->Size > Mask.LowSize);
            break;

          case TMask::Close:
            Result = HasSize && (Params->Size >= Mask.LowSize);
            break;
        }
      }

      bool HasModification = (Params != NULL);

      if (Result)
      {
        switch (Mask.HighModificationMask)
        {
          case TMask::None:
            Result = true;
            break;

          case TMask::Open:
            Result = HasModification && (Params->Modification < Mask.HighModification);
            break;

          case TMask::Close:
            Result = HasModification && (Params->Modification <= Mask.HighModification);
            break;
        }
      }

      if (Result)
      {
        switch (Mask.LowModificationMask)
        {
          case TMask::None:
            Result = true;
            break;

          case TMask::Open:
            Result = HasModification && (Params->Modification > Mask.LowModification);
            break;

          case TMask::Close:
            Result = HasModification && (Params->Modification >= Mask.LowModification);
            break;
        }
      }
    }

    I++;
  }

  if (!Result && Directory && !IsUnixRootPath(Path) && Recurse)
  {
    UnicodeString ParentFileName = UnixExtractFileName(Path);
    UnicodeString ParentPath = SimpleUnixExcludeTrailingBackslash(UnixExtractFilePath(Path));
    // Pass Params down or not?
    // Currently it includes Size/Time only, what is not used for directories.
    // So it depends on future use. Possibly we should make a copy
    // and pass on only relevant fields.
    Result = MatchesMasks(ParentFileName, true, ParentPath, Params, Masks, Recurse);
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const UnicodeString FileName, bool Directory,
  const UnicodeString Path, const TParams * Params) const
{
  bool ImplicitMatch;
  return Matches(FileName, Directory, Path, Params, true, ImplicitMatch);
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const UnicodeString FileName, bool Directory,
  const UnicodeString Path, const TParams * Params,
  bool RecurseInclude, bool & ImplicitMatch) const
{
  bool ImplicitIncludeMatch = FMasks[MASK_INDEX(Directory, true)].empty();
  bool ExplicitIncludeMatch = MatchesMasks(FileName, Directory, Path, Params, FMasks[MASK_INDEX(Directory, true)], RecurseInclude);
  bool Result =
    (ImplicitIncludeMatch || ExplicitIncludeMatch) &&
    !MatchesMasks(FileName, Directory, Path, Params, FMasks[MASK_INDEX(Directory, false)], false);
  ImplicitMatch =
    Result && ImplicitIncludeMatch && !ExplicitIncludeMatch &&
    FMasks[MASK_INDEX(Directory, false)].empty();
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const UnicodeString FileName, bool Local,
  bool Directory, const TParams * Params) const
{
  bool ImplicitMatch;
  return Matches(FileName, Local, Directory, Params, true, ImplicitMatch);
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::Matches(const UnicodeString FileName, bool Local,
  bool Directory, const TParams * Params, bool RecurseInclude, bool & ImplicitMatch) const
{
  bool Result;
  if (Local)
  {
    UnicodeString Path = ExtractFilePath(FileName);
    if (!Path.IsEmpty())
    {
      Path = ToUnixPath(ExcludeTrailingBackslash(Path));
    }
    Result = Matches(ExtractFileName(FileName), Directory, Path, Params,
      RecurseInclude, ImplicitMatch);
  }
  else
  {
    Result = Matches(UnixExtractFileName(FileName), Directory,
      SimpleUnixExcludeTrailingBackslash(UnixExtractFilePath(FileName)), Params,
      RecurseInclude, ImplicitMatch);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const TFileMasks & rhm) const
{
  return (Masks == rhm.Masks);
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const UnicodeString & rhs)
{
  Masks = rhs;
  return *this;
}
//---------------------------------------------------------------------------
TFileMasks & __fastcall TFileMasks::operator =(const TFileMasks & rhm)
{
  FForceDirectoryMasks = rhm.FForceDirectoryMasks;
  Masks = rhm.Masks;
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::operator ==(const UnicodeString & rhs) const
{
  return (Masks == rhs);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::ThrowError(int Start, int End)
{
  throw EFileMasksException(
    FMTLOAD(MASK_ERROR, (Masks.SubString(Start, End - Start + 1))),
    Start, End - Start + 1);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::CreateMaskMask(const UnicodeString & Mask, int Start, int End,
  bool Ex, TMaskMask & MaskMask)
{
  try
  {
    DebugAssert(MaskMask.Mask == NULL);
    if (Ex && !IsEffectiveFileNameMask(Mask))
    {
      MaskMask.Kind = TMaskMask::Any;
      MaskMask.Mask = NULL;
    }
    else
    {
      MaskMask.Kind = (Ex && (Mask == L"*.")) ? TMaskMask::NoExt : TMaskMask::Regular;
      MaskMask.Mask = new Masks::TMask(Mask);
    }
  }
  catch(...)
  {
    ThrowError(Start, End);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TFileMasks::MakeDirectoryMask(UnicodeString Str)
{
  DebugAssert(!Str.IsEmpty());
  if (Str.IsEmpty() || !Str.IsDelimiter(DirectoryMaskDelimiters, Str.Length()))
  {
    int D = Str.LastDelimiter(DirectoryMaskDelimiters);
    // if there's any [back]slash anywhere in str,
    // add the same [back]slash at the end, otherwise add slash
    wchar_t Delimiter = (D > 0) ? Str[D] : DirectoryMaskDelimiters[1];
    Str += Delimiter;
  }
  return Str;
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::CreateMask(
  const UnicodeString & MaskStr, int MaskStart, int /*MaskEnd*/, bool Include)
{
  bool Directory = false; // shut up
  TMask Mask;

  Mask.MaskStr = MaskStr;
  Mask.UserStr = MaskStr;
  Mask.FileNameMask.Kind = TMaskMask::Any;
  Mask.FileNameMask.Mask = NULL;
  Mask.DirectoryMask.Kind = TMaskMask::Any;
  Mask.DirectoryMask.Mask = NULL;
  Mask.HighSizeMask = TMask::None;
  Mask.LowSizeMask = TMask::None;
  Mask.HighModificationMask = TMask::None;
  Mask.LowModificationMask = TMask::None;

  wchar_t NextPartDelimiter = L'\0';
  int NextPartFrom = 1;
  while (NextPartFrom <= MaskStr.Length())
  {
    wchar_t PartDelimiter = NextPartDelimiter;
    int PartFrom = NextPartFrom;
    UnicodeString PartStr = CopyToChars(MaskStr, NextPartFrom, L"<>", false, &NextPartDelimiter, true);

    int PartStart = MaskStart + PartFrom - 1;
    int PartEnd = MaskStart + NextPartFrom - 1 - 2;

    TrimEx(PartStr, PartStart, PartEnd);

    if (PartDelimiter != L'\0')
    {
      bool Low = (PartDelimiter == L'>');

      TMask::TMaskBoundary Boundary;
      if ((PartStr.Length() >= 1) && (PartStr[1] == L'='))
      {
        Boundary = TMask::Close;
        PartStr.Delete(1, 1);
      }
      else
      {
        Boundary = TMask::Open;
      }

      TDateTime Modification;
      __int64 DummySize;
      if ((!TryStrToInt64(PartStr, DummySize) && TryStrToDateTimeStandard(PartStr, Modification)) ||
          TryRelativeStrToDateTime(PartStr, Modification, false))
      {
        TMask::TMaskBoundary & ModificationMask =
          (Low ? Mask.LowModificationMask : Mask.HighModificationMask);

        if ((ModificationMask != TMask::None) || Directory)
        {
          // include delimiter into size part
          ThrowError(PartStart - 1, PartEnd);
        }

        ModificationMask = Boundary;
        (Low ? Mask.LowModification : Mask.HighModification) = Modification;
      }
      else
      {
        TMask::TMaskBoundary & SizeMask = (Low ? Mask.LowSizeMask : Mask.HighSizeMask);
        __int64 & Size = (Low ? Mask.LowSize : Mask.HighSize);

        if ((SizeMask != TMask::None) || Directory)
        {
          // include delimiter into size part
          ThrowError(PartStart - 1, PartEnd);
        }

        SizeMask = Boundary;
        if (!TryStrToSize(PartStr, Size))
        {
          ThrowError(PartStart, PartEnd);
        }
      }
    }
    else if (!PartStr.IsEmpty())
    {
      int D = PartStr.LastDelimiter(DirectoryMaskDelimiters);

      Directory = (D > 0) && (D == PartStr.Length());

      if (Directory)
      {
        do
        {
          PartStr.SetLength(PartStr.Length() - 1);
          Mask.UserStr.Delete(PartStart - MaskStart + D, 1);
          D--;
        }
        while (PartStr.IsDelimiter(DirectoryMaskDelimiters, PartStr.Length()));

        D = PartStr.LastDelimiter(DirectoryMaskDelimiters);

        if (FForceDirectoryMasks == 0)
        {
          Directory = false;
          Mask.MaskStr = Mask.UserStr;
        }
      }
      else if (FForceDirectoryMasks > 0)
      {
        Directory = true;
        Mask.MaskStr.Insert(DirectoryMaskDelimiters[1], PartStart - MaskStart + PartStr.Length());
      }

      if (D > 0)
      {
        // make sure sole "/" (root dir) is preserved as is
        CreateMaskMask(
          SimpleUnixExcludeTrailingBackslash(ToUnixPath(PartStr.SubString(1, D))),
          PartStart, PartStart + D - 1, false,
          Mask.DirectoryMask);
        CreateMaskMask(
          PartStr.SubString(D + 1, PartStr.Length() - D),
          PartStart + D, PartEnd, true,
          Mask.FileNameMask);
      }
      else
      {
        CreateMaskMask(PartStr, PartStart, PartEnd, true, Mask.FileNameMask);
      }
    }
  }

  FMasks[MASK_INDEX(Directory, Include)].push_back(Mask);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TFileMasks::GetMasksStr(int Index) const
{
  if (FMasksStr[Index] == NULL)
  {
    FMasksStr[Index] = new TStringList();
    TMasks::const_iterator I = FMasks[Index].begin();
    while (I != FMasks[Index].end())
    {
      FMasksStr[Index]->Add((*I).UserStr);
      I++;
    }
  }

  return FMasksStr[Index];
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::ReleaseMaskMask(TMaskMask & MaskMask)
{
  delete MaskMask.Mask;
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::TrimEx(UnicodeString & Str, int & Start, int & End)
{
  UnicodeString Buf = TrimLeft(Str);
  Start += Str.Length() - Buf.Length();
  Str = TrimRight(Buf);
  End -= Buf.Length() - Str.Length();
}
//---------------------------------------------------------------------------
bool __fastcall TFileMasks::MatchesMaskMask(const TMaskMask & MaskMask, const UnicodeString & Str)
{
  bool Result;
  if (MaskMask.Kind == TMaskMask::Any)
  {
    Result = true;
  }
  else if ((MaskMask.Kind == TMaskMask::NoExt) && (Str.Pos(L".") == 0))
  {
    Result = true;
  }
  else
  {
    Result = MaskMask.Mask->Matches(Str);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetMasks(const UnicodeString value)
{
  if (FStr != value)
  {
    SetStr(value, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetMask(const UnicodeString & Mask)
{
  SetStr(Mask, true);
}
//---------------------------------------------------------------------------
void __fastcall TFileMasks::SetStr(const UnicodeString Str, bool SingleMask)
{
  UnicodeString Backup = FStr;
  try
  {
    FStr = Str;
    Clear();

    int NextMaskFrom = 1;
    bool Include = true;
    while (NextMaskFrom <= Str.Length())
    {
      int MaskStart = NextMaskFrom;
      wchar_t NextMaskDelimiter;
      UnicodeString MaskStr;
      if (SingleMask)
      {
        MaskStr = Str;
        NextMaskFrom = Str.Length() + 1;
        NextMaskDelimiter = L'\0';
      }
      else
      {
        MaskStr = CopyToChars(Str, NextMaskFrom, AllFileMasksDelimiters, false, &NextMaskDelimiter, true);
      }
      int MaskEnd = NextMaskFrom - 2;

      TrimEx(MaskStr, MaskStart, MaskEnd);

      if (!MaskStr.IsEmpty())
      {
        CreateMask(MaskStr, MaskStart, MaskEnd, Include);
      }

      if (NextMaskDelimiter == IncludeExcludeFileMasksDelimiter)
      {
        if (Include)
        {
          Include = false;
        }
        else
        {
          ThrowError(NextMaskFrom - 1, Str.Length());
        }
      }
    }
  }
  catch(...)
  {
    // this does not work correctly if previous mask was set using SetMask.
    // this should not fail (the mask was validated before),
    // otherwise we end in an infinite loop
    SetStr(Backup, false);
    throw;
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
#define TEXT_TOKEN L'\255'
//---------------------------------------------------------------------------
const wchar_t TCustomCommand::NoQuote = L'\0';
const UnicodeString TCustomCommand::Quotes = L"\"'";
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommand::Escape(const UnicodeString & S)
{
  return ReplaceStr(S, L"!", L"!!");
}
//---------------------------------------------------------------------------
TCustomCommand::TCustomCommand()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::GetToken(
  const UnicodeString & Command, int Index, int & Len, wchar_t & PatternCmd)
{
  DebugAssert(Index <= Command.Length());
  const wchar_t * Ptr = Command.c_str() + Index - 1;

  if (Ptr[0] == L'!')
  {
    PatternCmd = Ptr[1];
    if (PatternCmd == L'\0')
    {
      Len = 1;
    }
    else if (PatternCmd == L'!')
    {
      Len = 2;
    }
    else
    {
      Len = PatternLen(Command, Index);
    }

    if (Len <= 0)
    {
      throw Exception(FMTLOAD(CUSTOM_COMMAND_UNKNOWN, (PatternCmd, Index)));
    }
    else
    {
      if ((Command.Length() - Index + 1) < Len)
      {
        throw Exception(FMTLOAD(CUSTOM_COMMAND_UNTERMINATED, (PatternCmd, Index)));
      }
    }
  }
  else
  {
    PatternCmd = TEXT_TOKEN;
    const wchar_t * NextPattern = wcschr(Ptr, L'!');
    if (NextPattern == NULL)
    {
      Len = Command.Length() - Index + 1;
    }
    else
    {
      Len = NextPattern - Ptr;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::PatternHint(int /*Index*/, const UnicodeString & /*Pattern*/)
{
  // noop
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomCommand::Complete(const UnicodeString & Command,
  bool LastPass)
{
  int Index = 1;
  int PatternIndex = 0;
  while (Index <= Command.Length())
  {
    int Len;
    wchar_t PatternCmd;
    GetToken(Command, Index, Len, PatternCmd);

    if (PatternCmd == TEXT_TOKEN)
    {
    }
    else if (PatternCmd == L'!')
    {
    }
    else
    {
      UnicodeString Pattern = Command.SubString(Index, Len);
      PatternHint(PatternIndex, Pattern);
      PatternIndex++;
    }

    Index += Len;
  }

  UnicodeString Result;
  Index = 1;
  PatternIndex = 0;
  while (Index <= Command.Length())
  {
    int Len;
    wchar_t PatternCmd;
    GetToken(Command, Index, Len, PatternCmd);

    if (PatternCmd == TEXT_TOKEN)
    {
      Result += Command.SubString(Index, Len);
    }
    else if (PatternCmd == L'!')
    {
      if (LastPass)
      {
        Result += L'!';
      }
      else
      {
        Result += Command.SubString(Index, Len);
      }
    }
    else
    {
      wchar_t Quote = NoQuote;
      if ((Index > 1) && (Index + Len - 1 < Command.Length()) &&
          Command.IsDelimiter(Quotes, Index - 1) &&
          Command.IsDelimiter(Quotes, Index + Len) &&
          (Command[Index - 1] == Command[Index + Len]))
      {
        Quote = Command[Index - 1];
      }
      UnicodeString Pattern = Command.SubString(Index, Len);
      UnicodeString Replacement;
      bool Delimit = true;
      if (PatternReplacement(PatternIndex, Pattern, Replacement, Delimit))
      {
        if (!LastPass)
        {
          Replacement = Escape(Replacement);
        }
        if (Delimit)
        {
          DelimitReplacement(Replacement, Quote);
        }
        Result += Replacement;
      }
      else
      {
        Result += Pattern;
      }

      PatternIndex++;
    }

    Index += Len;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::DelimitReplacement(UnicodeString & Replacement, wchar_t Quote)
{
  Replacement = ShellDelimitStr(Replacement, Quote);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::Validate(const UnicodeString & Command)
{
  CustomValidate(Command, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::CustomValidate(const UnicodeString & Command,
  void * Arg)
{
  int Index = 1;

  while (Index <= Command.Length())
  {
    int Len;
    wchar_t PatternCmd;
    GetToken(Command, Index, Len, PatternCmd);
    ValidatePattern(Command, Index, Len, PatternCmd, Arg);

    Index += Len;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommand::FindPattern(const UnicodeString & Command,
  wchar_t PatternCmd)
{
  bool Result = false;
  int Index = 1;

  while (!Result && (Index <= Command.Length()))
  {
    int Len;
    wchar_t APatternCmd;
    GetToken(Command, Index, Len, APatternCmd);
    if (((PatternCmd != L'!') && (tolower(PatternCmd) == tolower(APatternCmd))) ||
        ((PatternCmd == L'!') && (Len == 1) && (APatternCmd != TEXT_TOKEN)) ||
        ((PatternCmd == L'\0') && (APatternCmd != TEXT_TOKEN)))
    {
      Result = true;
    }

    Index += Len;
  }

  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommand::HasAnyPatterns(const UnicodeString & Command)
{
  return FindPattern(Command, L'\0');
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommand::ValidatePattern(const UnicodeString & /*Command*/,
  int /*Index*/, int /*Len*/, wchar_t /*PatternCmd*/, void * /*Arg*/)
{
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TInteractiveCustomCommand::TInteractiveCustomCommand(
  TCustomCommand * ChildCustomCommand)
{
  FChildCustomCommand = ChildCustomCommand;
}
//---------------------------------------------------------------------------
void __fastcall TInteractiveCustomCommand::Prompt(
  int /*Index*/, const UnicodeString & /*Prompt*/, UnicodeString & Value)
{
  Value = L"";
}
//---------------------------------------------------------------------------
void __fastcall TInteractiveCustomCommand::Execute(
  const UnicodeString & /*Command*/, UnicodeString & Value)
{
  Value = L"";
}
//---------------------------------------------------------------------------
int __fastcall TInteractiveCustomCommand::PatternLen(const UnicodeString & Command, int Index)
{
  int Len;
  wchar_t PatternCmd = (Index < Command.Length()) ? Command[Index + 1] : L'\0';
  switch (PatternCmd)
  {
    case L'?':
      {
        const wchar_t * Ptr = Command.c_str() + Index - 1;
        const wchar_t * PatternEnd = wcschr(Ptr + 1, L'!');
        if (PatternEnd == NULL)
        {
          throw Exception(FMTLOAD(CUSTOM_COMMAND_UNTERMINATED, (Command[Index + 1], Index)));
        }
        Len = PatternEnd - Ptr + 1;
      }
      break;

    case L'`':
      {
        const wchar_t * Ptr = Command.c_str() + Index - 1;
        const wchar_t * PatternEnd = wcschr(Ptr + 2, L'`');
        if (PatternEnd == NULL)
        {
          throw Exception(FMTLOAD(CUSTOM_COMMAND_UNTERMINATED, (Command[Index + 1], Index)));
        }
        Len = PatternEnd - Ptr + 1;
      }
      break;

    default:
      Len = FChildCustomCommand->PatternLen(Command, Index);
      break;
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TInteractiveCustomCommand::IsPromptPattern(const UnicodeString & Pattern)
{
  return (Pattern.Length() >= 3) && (Pattern[2] == L'?');
}
//---------------------------------------------------------------------------
void __fastcall TInteractiveCustomCommand::ParsePromptPattern(
  const UnicodeString & Pattern, UnicodeString & Prompt, UnicodeString & Default, bool & Delimit)
{
  int Pos = Pattern.SubString(3, Pattern.Length() - 2).Pos(L"?");
  if (Pos > 0)
  {
    Default = Pattern.SubString(3 + Pos, Pattern.Length() - 3 - Pos);
    if ((Pos > 1) && (Pattern[3 + Pos - 2] == L'\\'))
    {
      Delimit = false;
      Pos--;
    }
    Prompt = Pattern.SubString(3, Pos - 1);
  }
  else
  {
    Prompt = Pattern.SubString(3, Pattern.Length() - 3);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TInteractiveCustomCommand::PatternReplacement(int Index, const UnicodeString & Pattern,
  UnicodeString & Replacement, bool & Delimit)
{
  bool Result;
  if (IsPromptPattern(Pattern))
  {
    UnicodeString PromptStr;
    // The PromptStr and Replacement are actually never used
    // as the only implementation (TWinInteractiveCustomCommand) uses
    // prompts and defaults from PatternHint.
    ParsePromptPattern(Pattern, PromptStr, Replacement, Delimit);

    Prompt(Index, PromptStr, Replacement);

    Result = true;
  }
  else if ((Pattern.Length() >= 3) && (Pattern[2] == L'`'))
  {
    UnicodeString Command = Pattern.SubString(3, Pattern.Length() - 3);
    Command = FChildCustomCommand->Complete(Command, true);
    Execute(Command, Replacement);
    Delimit = false;
    Result = true;
  }
  else
  {
    Result = false;
  }

  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCustomCommandData::TCustomCommandData()
{
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandData::TCustomCommandData(TTerminal * Terminal)
{
  Init(Terminal->SessionData, Terminal->UserName, Terminal->Password,
    Terminal->GetSessionInfo().HostKeyFingerprintSHA256);
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandData::TCustomCommandData(
  TSessionData * SessionData, const UnicodeString & UserName, const UnicodeString & Password)
{
  Init(SessionData, UserName, Password, UnicodeString());
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandData::Init(
  TSessionData * ASessionData, const UnicodeString & AUserName,
  const UnicodeString & APassword, const UnicodeString & AHostKey)
{
  FSessionData.reset(new TSessionData(L""));
  FSessionData->Assign(ASessionData);
  FSessionData->UserName = AUserName;
  FSessionData->Password = APassword;
  FSessionData->HostKey = AHostKey;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandData::operator=(const TCustomCommandData & Data)
{
  DebugAssert(Data.SessionData != NULL);
  FSessionData.reset(new TSessionData(L""));
  FSessionData->Assign(Data.SessionData);
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TCustomCommandData::GetSesssionData() const
{
  return FSessionData.get();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand()
{
}
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand(const TCustomCommandData & Data,
  const UnicodeString & Path)
{
  FData = Data;
  FPath = Path;
}
//---------------------------------------------------------------------------
TFileCustomCommand::TFileCustomCommand(const TCustomCommandData & Data,
    const UnicodeString & Path, const UnicodeString & FileName,
    const UnicodeString & FileList) :
  TCustomCommand()
{
  FData = Data;
  FPath = Path;
  FFileName = FileName;
  FFileList = FileList;
}
//---------------------------------------------------------------------------
int __fastcall TFileCustomCommand::PatternLen(const UnicodeString & Command, int Index)
{
  int Len;
  wchar_t PatternCmd = (Index < Command.Length()) ? tolower(Command[Index + 1]) : L'\0';
  switch (PatternCmd)
  {
    case L's':
    case L'e':
    case L'@':
    case L'u':
    case L'p':
    case L'#':
    case L'/':
    case L'&':
    case L'n':
      Len = 2;
      break;

    default:
      Len = 1;
      break;
  }
  return Len;
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::PatternReplacement(
  int /*Index*/, const UnicodeString & Pattern, UnicodeString & Replacement, bool & Delimit)
{
  // keep consistent with TSessionLog::OpenLogFile

  if (SameText(Pattern, L"!s"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = FData.SessionData->GenerateSessionUrl(sufSession);
    }
  }
  else if (SameText(Pattern, L"!e"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = FData.SessionData->GenerateSessionUrl(sufComplete);
    }
  }
  else if (Pattern == L"!@")
  {
    if (FData.SessionData != NULL)
    {
      Replacement = FData.SessionData->HostNameExpanded;
    }
  }
  else if (SameText(Pattern, L"!u"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = FData.SessionData->UserName;
    }
  }
  else if (SameText(Pattern, L"!p"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = NormalizeString(FData.SessionData->Password);
    }
  }
  else if (SameText(Pattern, L"!#"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = IntToStr(FData.SessionData->PortNumber);
    }
  }
  else if (Pattern == L"!/")
  {
    Replacement = UnixIncludeTrailingBackslash(FPath);
  }
  else if (Pattern == L"!&")
  {
    Replacement = FFileList;
    // already delimited
    Delimit = false;
  }
  else if (SameText(Pattern, L"!n"))
  {
    if (FData.SessionData != NULL)
    {
      Replacement = FData.SessionData->SessionName;
    }
  }
  else
  {
    DebugAssert(Pattern.Length() == 1);
    Replacement = FFileName;
  }

  return true;
}
//---------------------------------------------------------------------------
void __fastcall TFileCustomCommand::Validate(const UnicodeString & Command)
{
  int Found[2] = { 0, 0 };
  CustomValidate(Command, &Found);
  if ((Found[0] > 0) && (Found[1] > 0))
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_FILELIST_ERROR,
      (Found[1], Found[0])));
  }
}
//---------------------------------------------------------------------------
void __fastcall TFileCustomCommand::ValidatePattern(const UnicodeString & Command,
  int Index, int /*Len*/, wchar_t PatternCmd, void * Arg)
{
  int * Found = static_cast<int *>(Arg);

  DebugAssert(Index > 0);

  if (PatternCmd == L'&')
  {
    Found[0] = Index;
  }
  else if ((PatternCmd != TEXT_TOKEN) && (PatternLen(Command, Index) == 1))
  {
    Found[1] = Index;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsFileListCommand(const UnicodeString & Command)
{
  return FindPattern(Command, L'&');
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsRemoteFileCommand(const UnicodeString & Command)
{
  return FindPattern(Command, L'!') || FindPattern(Command, L'&');
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsFileCommand(const UnicodeString & Command)
{
  return IsRemoteFileCommand(Command);
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsSiteCommand(const UnicodeString & Command)
{
  return FindPattern(Command, L'@');
}
//---------------------------------------------------------------------------
bool __fastcall TFileCustomCommand::IsPasswordCommand(const UnicodeString & Command)
{
  return FindPattern(Command, L'p');
}
//---------------------------------------------------------------------------
