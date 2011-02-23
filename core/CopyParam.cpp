//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "CopyParam.h"
#include "HierarchicalStorage.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
__fastcall TCopyParamType::TCopyParamType()
{
  Default();
}
//---------------------------------------------------------------------------
__fastcall TCopyParamType::TCopyParamType(const TCopyParamType & Source)
{
  Assign(&Source);
}
//---------------------------------------------------------------------------
__fastcall TCopyParamType::~TCopyParamType()
{
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Default()
{
  // when changing defaults, make sure GetInfoStr() can handle it
  FileNameCase = ncNoChange;
  PreserveReadOnly = false;
  PreserveTime = true;
  Rights.Number = TRights::rfDefault;
  PreserveRights = false; // Was True until #106
  IgnorePermErrors = false;
  AsciiFileMask.Masks = "*.*html; *.htm; *.txt; *.php; *.php3; *.cgi; *.c; *.cpp; *.h; *.pas; "
    "*.bas; *.tex; *.pl; *.js; .htaccess; *.xtml; *.css; *.cfg; *.ini; *.sh; *.xml";
  TransferMode = tmAutomatic;
  AddXToDirectories = true;
  ResumeSupport = rsSmart;
  ResumeThreshold = 100 * 1024; // (100 KiB)
  InvalidCharsReplacement = TokenReplacement;
  LocalInvalidChars = "/\\:*?\"<>|";
  CalculateSize = true;
  FileMask = "*.*";
  ExcludeFileMask.Masks = "";
  NegativeExclude = false;
  ClearArchive = false;
  CPSLimit = 0;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::GetInfoStr(AnsiString Separator, int Options) const
{
  TCopyParamType Defaults;
  AnsiString Result;

  bool SomeAttrExcluded = false;
  #define ADD(STR, EXCEPT) \
    if (FLAGCLEAR(Options, EXCEPT)) \
    { \
      Result += (Result.IsEmpty() ? AnsiString() : Separator) + (STR); \
    } \
    else \
    { \
      SomeAttrExcluded = true; \
    }

  if ((TransferMode != Defaults.TransferMode) ||
      ((TransferMode == tmAutomatic) && !(AsciiFileMask == Defaults.AsciiFileMask)))
  {
    AnsiString S = FORMAT(LoadStrPart(COPY_INFO_TRANSFER_TYPE, 1),
      (LoadStrPart(COPY_INFO_TRANSFER_TYPE, TransferMode + 2)));
    if (TransferMode == tmAutomatic)
    {
      S = FORMAT(S, (AsciiFileMask.Masks));
    }
    ADD(S, cpaExcludeMaskOnly | cpaNoTransferMode);
  }

  if (FileNameCase != Defaults.FileNameCase)
  {
    ADD(FORMAT(LoadStrPart(COPY_INFO_FILENAME, 1),
      (LoadStrPart(COPY_INFO_FILENAME, FileNameCase + 2))),
      cpaExcludeMaskOnly);
  }

  if ((InvalidCharsReplacement == NoReplacement) !=
        (Defaults.InvalidCharsReplacement == NoReplacement))
  {
    assert(InvalidCharsReplacement == NoReplacement);
    if (InvalidCharsReplacement == NoReplacement)
    {
      ADD(LoadStr(COPY_INFO_DONT_REPLACE_INV_CHARS), cpaExcludeMaskOnly);
    }
  }

  if ((PreserveRights != Defaults.PreserveRights) ||
      (PreserveRights &&
       ((Rights != Defaults.Rights) || (AddXToDirectories != Defaults.AddXToDirectories))))
  {
    assert(PreserveRights);

    if (PreserveRights)
    {
      AnsiString RightsStr = Rights.Text;
      if (AddXToDirectories)
      {
        RightsStr += ", " + LoadStr(COPY_INFO_ADD_X_TO_DIRS);
      }
      ADD(FORMAT(LoadStr(COPY_INFO_PERMISSIONS), (RightsStr)),
        cpaExcludeMaskOnly | cpaNoRights);
    }
  }

  if (PreserveTime != Defaults.PreserveTime)
  {
    ADD(LoadStr(PreserveTime ? COPY_INFO_TIMESTAMP : COPY_INFO_DONT_PRESERVE_TIME),
      cpaExcludeMaskOnly | cpaNoPreserveTime);
  }

  if ((PreserveRights || PreserveTime) &&
      (IgnorePermErrors != Defaults.IgnorePermErrors))
  {
    assert(IgnorePermErrors);

    if (IgnorePermErrors)
    {
      ADD(LoadStr(COPY_INFO_IGNORE_PERM_ERRORS),
        cpaExcludeMaskOnly | cpaNoIgnorePermErrors);
    }
  }

  if (PreserveReadOnly != Defaults.PreserveReadOnly)
  {
    assert(PreserveReadOnly);
    if (PreserveReadOnly)
    {
      ADD(LoadStr(COPY_INFO_PRESERVE_READONLY),
        cpaExcludeMaskOnly | cpaNoPreserveReadOnly);
    }
  }

  if (CalculateSize != Defaults.CalculateSize)
  {
    assert(!CalculateSize);
    if (!CalculateSize)
    {
      ADD(LoadStr(COPY_INFO_DONT_CALCULATE_SIZE), cpaExcludeMaskOnly);
    }
  }

  if (ClearArchive != Defaults.ClearArchive)
  {
    assert(ClearArchive);
    if (ClearArchive)
    {
      ADD(LoadStr(COPY_INFO_CLEAR_ARCHIVE),
        cpaExcludeMaskOnly | cpaNoClearArchive);
    }
  }

  if (((NegativeExclude != Defaults.NegativeExclude) && !(ExcludeFileMask == "")) ||
      !(ExcludeFileMask == Defaults.ExcludeFileMask))
  {
    ADD(FORMAT(LoadStr(NegativeExclude ? COPY_INFO_INCLUDE_MASK : COPY_INFO_EXCLUDE_MASK),
      (ExcludeFileMask.Masks)),
      cpaNoExcludeMask);
  }

  if (CPSLimit > 0)
  {
    ADD(FMTLOAD(COPY_INFO_CPS_LIMIT, (int(CPSLimit / 1024))), cpaExcludeMaskOnly);
  }

  if (SomeAttrExcluded)
  {
    Result += (Result.IsEmpty() ? AnsiString() : Separator) +
      FORMAT(LoadStrPart(COPY_INFO_NOT_USABLE, 1),
        (LoadStrPart(COPY_INFO_NOT_USABLE, (Result.IsEmpty() ? 3 : 2))));
  }
  else if (Result.IsEmpty())
  {
    Result = LoadStr(COPY_INFO_DEFAULT);
  }
  #undef ADD

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Assign(const TCopyParamType * Source)
{
  assert(Source != NULL);
  #define COPY(Prop) Prop = Source->Prop
  COPY(FileNameCase);
  COPY(PreserveReadOnly);
  COPY(PreserveTime);
  COPY(Rights);
  COPY(AsciiFileMask);
  COPY(TransferMode);
  COPY(AddXToDirectories);
  COPY(PreserveRights);
  COPY(IgnorePermErrors);
  COPY(ResumeSupport);
  COPY(ResumeThreshold);
  COPY(InvalidCharsReplacement);
  COPY(LocalInvalidChars);
  COPY(CalculateSize);
  COPY(FileMask);
  COPY(ExcludeFileMask);
  COPY(NegativeExclude);
  COPY(ClearArchive);
  COPY(CPSLimit);
  #undef COPY
}
//---------------------------------------------------------------------------
TCopyParamType & __fastcall TCopyParamType::operator =(const TCopyParamType & rhp)
{
  Assign(&rhp);
  return *this;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::SetLocalInvalidChars(AnsiString value)
{
  if (value != LocalInvalidChars)
  {
    FLocalInvalidChars = value;
    FTokenizibleChars = FLocalInvalidChars + TokenPrefix;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::GetReplaceInvalidChars() const
{
  return (InvalidCharsReplacement != NoReplacement);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::SetReplaceInvalidChars(bool value)
{
  if (ReplaceInvalidChars != value)
  {
    InvalidCharsReplacement = (value ? TokenReplacement : NoReplacement);
  }
}
//---------------------------------------------------------------------------
char * __fastcall TCopyParamType::ReplaceChar(AnsiString & FileName, char * InvalidChar) const
{
  int Index = InvalidChar - FileName.c_str() + 1;
  if (FileName.ByteType(Index) == mbSingleByte)
  {
    if (InvalidCharsReplacement == TokenReplacement)
    {
      FileName.Insert(CharToHex(FileName[Index]), Index + 1);
      FileName[Index] = TokenPrefix;
      InvalidChar = FileName.c_str() + Index + 2;
    }
    else
    {
      FileName[Index] = InvalidCharsReplacement;
      InvalidChar++;
    }
  }
  else
  {
    InvalidChar++;
  }
  return InvalidChar;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::ValidLocalFileName(AnsiString FileName) const
{
  if (InvalidCharsReplacement != NoReplacement)
  {
    bool ATokenReplacement = (InvalidCharsReplacement == TokenReplacement);
    const char * Chars =
      (ATokenReplacement ? FTokenizibleChars : LocalInvalidChars).c_str();
    char * InvalidChar = FileName.c_str();
    while ((InvalidChar = strpbrk(InvalidChar, Chars)) != NULL)
    {
      int Pos = (InvalidChar - FileName.c_str() + 1);
      char Char;
      if ((InvalidCharsReplacement == TokenReplacement) &&
          (*InvalidChar == TokenPrefix) &&
          (((FileName.Length() - Pos) <= 1) ||
           (((Char = HexToChar(FileName.SubString(Pos + 1, 2))) == '\0') ||
            (FTokenizibleChars.Pos(Char) == 0))))
      {
        InvalidChar++;
      }
      else
      {
        InvalidChar = ReplaceChar(FileName, InvalidChar);
      }
    }

    // Windows trim trailing space or dot, hence we must encode it to preserve it
    if (!FileName.IsEmpty() &&
        ((FileName[FileName.Length()] == ' ') ||
         (FileName[FileName.Length()] == '.')))
    {
      ReplaceChar(FileName, FileName.c_str() + FileName.Length() - 1);
    }

    if (IsReservedName(FileName))
    {
      int P = FileName.Pos(".");
      if (P == 0)
      {
        P = FileName.Length() + 1;
      }
      FileName.Insert("%00", P);
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::RestoreChars(AnsiString FileName) const
{
  if (InvalidCharsReplacement == TokenReplacement)
  {
    char * InvalidChar = FileName.c_str();
    while ((InvalidChar = strchr(InvalidChar, TokenPrefix)) != NULL)
    {
      int Index = InvalidChar - FileName.c_str() + 1;
      if ((FileName.Length() >= Index + 2) &&
          (FileName.ByteType(Index) == mbSingleByte) &&
          (FileName.ByteType(Index + 1) == mbSingleByte) &&
          (FileName.ByteType(Index + 2) == mbSingleByte))
      {
        AnsiString Hex = FileName.SubString(Index + 1, 2);
        char Char = HexToChar(Hex);
        if ((Char != '\0') &&
            ((FTokenizibleChars.Pos(Char) > 0) ||
             (((Char == ' ') || (Char == '.')) && (Index == FileName.Length() - 2))))
        {
          FileName[Index] = Char;
          FileName.Delete(Index + 1, 2);
          InvalidChar = FileName.c_str() + Index;
        }
        else if ((Hex == "00") &&
                 ((Index == FileName.Length() - 2) || (FileName[Index + 3] == '.')) &&
                 IsReservedName(FileName.SubString(1, Index - 1) + FileName.SubString(Index + 3, FileName.Length() - Index - 3 + 1)))
        {
          FileName.Delete(Index, 3);
          InvalidChar = FileName.c_str() + Index - 1;
        }
        else
        {
          InvalidChar++;
        }
      }
      else
      {
        InvalidChar++;
      }
    }
  }
  return FileName;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::ValidLocalPath(AnsiString Path) const
{
  AnsiString Result;
  while (!Path.IsEmpty())
  {
    if (!Result.IsEmpty())
    {
      Result += "\\";
    }
    Result += ValidLocalFileName(CutToChar(Path, '\\', false));
  }
  return Result;
}
//---------------------------------------------------------------------------
// not used yet
AnsiString __fastcall TCopyParamType::Untokenize(AnsiString FileName)
{
  char * Token;
  AnsiString Result = FileName;
  while ((Token = AnsiStrScan(Result.c_str(), TokenPrefix)) != NULL)
  {
    int Index = Token - Result.c_str() + 1;
    if (Index > Result.Length() - 2)
    {
      Result = FileName;
      break;
    }
    else
    {
      char Ch = (char)HexToInt(Result.SubString(Index + 1, 2), -1);
      if (Ch == '\0')
      {
        Result = FileName;
        break;
      }
      else
      {
        Result[Index] = Ch;
        Result.Delete(Index + 1, 2);
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::ChangeFileName(AnsiString FileName,
  TOperationSide Side, bool FirstLevel) const
{
  if (FirstLevel)
  {
    FileName = MaskFileName(FileName, FileMask);
  }
  switch (FileNameCase) {
    case ncUpperCase: FileName = FileName.UpperCase(); break;
    case ncLowerCase: FileName = FileName.LowerCase(); break;
    case ncFirstUpperCase: FileName = FileName.SubString(1, 1).UpperCase() +
      FileName.SubString(2, FileName.Length()-1).LowerCase(); break;
    case ncLowerCaseShort:
      if ((FileName.Length() <= 12) && (FileName.Pos(".") <= 9) &&
          (FileName == FileName.UpperCase()))
      {
        FileName = FileName.LowerCase();
      }
      break;
    case ncNoChange:
    default:
      /*nothing*/
      break;
  }
  if (Side == osRemote)
  {
    FileName = ValidLocalFileName(FileName);
  }
  else
  {
    FileName = RestoreChars(FileName);
  }
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::UseAsciiTransfer(AnsiString FileName,
  TOperationSide Side, const TFileMasks::TParams & Params) const
{
  switch (TransferMode) {
    case tmBinary: return false;
    case tmAscii: return true;
    case tmAutomatic: return AsciiFileMask.Matches(FileName, (Side == osLocal),
      false, &Params);
    default: assert(false); return false;
  }
}
//---------------------------------------------------------------------------
TRights __fastcall TCopyParamType::RemoteFileRights(Integer Attrs) const
{
  TRights R = Rights;
  if ((Attrs & faDirectory) && AddXToDirectories)
    R.AddExecute();
  return R;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::GetLogStr() const
{
  char CaseC[] = "NULFS";
  char ModeC[] = "BAM";
  char ResumeC[] = "YSN";
  return FORMAT(
    "  PrTime: %s; PrRO: %s; Rght: %s; PrR: %s (%s); FnCs: %s; RIC: %s; "
      "Resume: %s (%d); CalcS: %s; Mask: %s\n"
    "  TM: %s; ClAr: %s; CPS: %u; ExclM(%s): %s\n"
    "  AscM: %s\n",
    (BooleanToEngStr(PreserveTime),
     BooleanToEngStr(PreserveReadOnly),
     Rights.Text,
     BooleanToEngStr(PreserveRights),
     BooleanToEngStr(IgnorePermErrors),
     CaseC[FileNameCase],
     CharToHex(InvalidCharsReplacement),
     ResumeC[ResumeSupport],
     (int)ResumeThreshold,
     BooleanToEngStr(CalculateSize),
     FileMask,
     ModeC[TransferMode],
     BooleanToEngStr(ClearArchive),
     int(CPSLimit),
     BooleanToEngStr(NegativeExclude),
     ExcludeFileMask.Masks,
     AsciiFileMask.Masks));
}
//---------------------------------------------------------------------------
int __fastcall TCopyParamType::LocalFileAttrs(const TRights & Rights) const
{
  int Result = 0;
  if (PreserveReadOnly && !Rights.Right[TRights::rrUserWrite])
  {
    Result |= faReadOnly;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowResume(__int64 Size) const
{
  switch (ResumeSupport) {
    case rsOn: return true;
    case rsOff: return false;
    case rsSmart: return (Size >= ResumeThreshold);
    default: assert(false); return false;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowAnyTransfer() const
{
  return ExcludeFileMask.Masks.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowTransfer(AnsiString FileName,
  TOperationSide Side, bool Directory, const TFileMasks::TParams & Params) const
{
  bool Result = true;
  if (!ExcludeFileMask.Masks.IsEmpty())
  {
    Result = (ExcludeFileMask.Matches(FileName, (Side == osLocal),
      Directory, &Params) == NegativeExclude);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Load(THierarchicalStorage * Storage)
{
  AddXToDirectories = Storage->ReadBool("AddXToDirectories", AddXToDirectories);
  AsciiFileMask.Masks = Storage->ReadString("Masks", AsciiFileMask.Masks);
  FileNameCase = (TFileNameCase)Storage->ReadInteger("FileNameCase", FileNameCase);
  PreserveReadOnly = Storage->ReadBool("PreserveReadOnly", PreserveReadOnly);
  PreserveTime = Storage->ReadBool("PreserveTime", PreserveTime);
  PreserveRights = Storage->ReadBool("PreserveRights", PreserveRights);
  IgnorePermErrors = Storage->ReadBool("IgnorePermErrors", IgnorePermErrors);
  Rights.Text = Storage->ReadString("Text", Rights.Text);
  TransferMode = (TTransferMode)Storage->ReadInteger("TransferMode", TransferMode);
  ResumeSupport = (TResumeSupport)Storage->ReadInteger("ResumeSupport", ResumeSupport);
  ResumeThreshold = Storage->ReadInt64("ResumeThreshold", ResumeThreshold);
  InvalidCharsReplacement = (char)Storage->ReadInteger("ReplaceInvalidChars",
    (unsigned char)InvalidCharsReplacement);
  LocalInvalidChars = Storage->ReadString("LocalInvalidChars", LocalInvalidChars);
  CalculateSize = Storage->ReadBool("CalculateSize", CalculateSize);
  ExcludeFileMask.Masks = Storage->ReadString("ExcludeFileMask", ExcludeFileMask.Masks);
  NegativeExclude = Storage->ReadBool("NegativeExclude", NegativeExclude);
  ClearArchive = Storage->ReadBool("ClearArchive", ClearArchive);
  CPSLimit = Storage->ReadInteger("CPSLimit", CPSLimit);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteBool("AddXToDirectories", AddXToDirectories);
  Storage->WriteString("Masks", AsciiFileMask.Masks);
  Storage->WriteInteger("FileNameCase", FileNameCase);
  Storage->WriteBool("PreserveReadOnly", PreserveReadOnly);
  Storage->WriteBool("PreserveTime", PreserveTime);
  Storage->WriteBool("PreserveRights", PreserveRights);
  Storage->WriteBool("IgnorePermErrors", IgnorePermErrors);
  Storage->WriteString("Text", Rights.Text);
  Storage->WriteInteger("TransferMode", TransferMode);
  Storage->WriteInteger("ResumeSupport", ResumeSupport);
  Storage->WriteInt64("ResumeThreshold", ResumeThreshold);
  Storage->WriteInteger("ReplaceInvalidChars", (unsigned char)InvalidCharsReplacement);
  Storage->WriteString("LocalInvalidChars", LocalInvalidChars);
  Storage->WriteBool("CalculateSize", CalculateSize);
  Storage->WriteString("ExcludeFileMask", ExcludeFileMask.Masks);
  Storage->WriteBool("NegativeExclude", NegativeExclude);
  Storage->WriteBool("ClearArchive", ClearArchive);
  Storage->WriteInteger("CPSLimit", CPSLimit);
}
//---------------------------------------------------------------------------
#define C(Property) (Property == rhp.Property)
bool __fastcall TCopyParamType::operator==(const TCopyParamType & rhp) const
{
  return
    C(AddXToDirectories) &&
    C(AsciiFileMask) &&
    C(FileNameCase) &&
    C(PreserveReadOnly) &&
    C(PreserveTime) &&
    C(PreserveRights) &&
    C(IgnorePermErrors) &&
    C(Rights) &&
    C(TransferMode) &&
    C(ResumeSupport) &&
    C(ResumeThreshold) &&
    C(InvalidCharsReplacement) &&
    C(LocalInvalidChars) &&
    C(CalculateSize) &&
    C(ExcludeFileMask) &&
    C(NegativeExclude) &&
    C(ClearArchive) &&
    C(CPSLimit) &&
    true;
}
#undef C
//---------------------------------------------------------------------------
unsigned long __fastcall GetSpeedLimit(const AnsiString & Text)
{
  unsigned long Speed;
  if (AnsiSameText(Text, LoadStr(SPEED_UNLIMITED)))
  {
    Speed = 0;
  }
  else
  {
    int SSpeed;
    if (!TryStrToInt(Text, SSpeed) ||
        (SSpeed < 0))
    {
      throw Exception(FMTLOAD(SPEED_INVALID, (Text)));
    }
    Speed = SSpeed;
  }
  return Speed * 1024;
}
//---------------------------------------------------------------------------
AnsiString __fastcall SetSpeedLimit(unsigned long Limit)
{
  AnsiString Text;
  if (Limit == 0)
  {
    Text = LoadStr(SPEED_UNLIMITED);
  }
  else
  {
    Text = IntToStr(Limit / 1024);
  }
  return Text;
}
