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
  PreserveReadOnly = true;
  PreserveTime = true;
  Rights.Number = TRights::rfDefault;
  PreserveRights = false; // Was True until #106
  AsciiFileMask.Masks = "*.*html; *.htm; *.txt; *.php*; *.cgi; *.c; *.cpp; *.h; *.pas; "
    "*.bas; *.tex; *.pl; .htaccess; *.xtml; *.css; *.cfg; *.ini; *.sh; *.xml";
  TransferMode = tmAutomatic;
  AddXToDirectories = true;
  ResumeSupport = rsSmart;
  ResumeThreshold = 100 * 1024; // (100 kB)
  InvalidCharsReplacement = TokenReplacement;
  LocalInvalidChars = "/\\:*?\"<>|";
  CalculateSize = true;
  FileMask = "*.*";
  ExcludeFileMask.Masks = "";
  NegativeExclude = false;
  ClearArchive = false;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCopyParamType::GetInfoStr(AnsiString Separator, int Options) const
{
  TCopyParamType Defaults;
  AnsiString Result;

  #define ADD(STR) Result += (Result.IsEmpty() ? AnsiString() : Separator) + (STR)
  if (FLAGCLEAR(Options, cpiExcludeMaskOnly))
  {
    if ((TransferMode != Defaults.TransferMode) ||
        ((TransferMode == tmAutomatic) && !(AsciiFileMask == Defaults.AsciiFileMask)))
    {
      AnsiString S = FORMAT(LoadStrPart(COPY_INFO_TRANSFER_TYPE, 1),
        (LoadStrPart(COPY_INFO_TRANSFER_TYPE, TransferMode + 2)));
      if (TransferMode == tmAutomatic)
      {
        S = FORMAT(S, (AsciiFileMask.Masks));
      }
      ADD(S);
    }

    if (FileNameCase != Defaults.FileNameCase)
    {
      ADD(FORMAT(LoadStrPart(COPY_INFO_FILENAME, 1),
        (LoadStrPart(COPY_INFO_FILENAME, FileNameCase + 2))));
    }

    if ((InvalidCharsReplacement == NoReplacement) !=
          (Defaults.InvalidCharsReplacement == NoReplacement))
    {
      assert(InvalidCharsReplacement == NoReplacement);
      if (InvalidCharsReplacement == NoReplacement)
      {
        ADD(LoadStr(COPY_INFO_DONT_REPLACE_INV_CHARS));
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
        ADD(FORMAT(LoadStr(COPY_INFO_PERMISSIONS), (RightsStr)));
      }
    }

    if (PreserveReadOnly != Defaults.PreserveReadOnly)
    {
      assert(!PreserveReadOnly);
      if (!PreserveReadOnly)
      {
        ADD(LoadStr(COPY_INFO_DONT_PRESERVE_READONLY));
      }
    }

    if (PreserveTime != Defaults.PreserveTime)
    {
      ADD(LoadStr(PreserveTime ? COPY_INFO_TIMESTAMP : COPY_INFO_DONT_PRESERVE_TIME));
    }

    if (CalculateSize != Defaults.CalculateSize)
    {
      assert(!CalculateSize);
      if (!CalculateSize)
      {
        ADD(LoadStr(COPY_INFO_DONT_CALCULATE_SIZE));
      }
    }

    if (ClearArchive != Defaults.ClearArchive)
    {
      assert(ClearArchive);
      if (ClearArchive)
      {
        ADD(LoadStr(COPY_INFO_CLEAR_ARCHIVE));
      }
    }
  }

  if ((NegativeExclude != Defaults.NegativeExclude) ||
      !(ExcludeFileMask == Defaults.ExcludeFileMask))
  {
    ADD(FORMAT(LoadStr(NegativeExclude ? COPY_INFO_INCLUDE_MASK : COPY_INFO_EXCLUDE_MASK),
      (ExcludeFileMask.Masks)));
  }
  #undef ADD

  if (Result.IsEmpty())
  {
    Result = LoadStr(COPY_INFO_DEFAULT);
  }

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
  COPY(ResumeSupport);
  COPY(ResumeThreshold);
  COPY(InvalidCharsReplacement);
  COPY(LocalInvalidChars);
  COPY(CalculateSize);
  COPY(FileMask);
  COPY(ExcludeFileMask);
  COPY(NegativeExclude);
  COPY(ClearArchive);
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
      int Index = InvalidChar - FileName.c_str() + 1;
      if (FileName.ByteType(Index) == mbSingleByte)
      {
        if (ATokenReplacement)
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
    }
  }
  return FileName;
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
  return FileName;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::UseAsciiTransfer(AnsiString FileName,
  TOperationSide Side) const
{
  switch (TransferMode) {
    case tmBinary: return false;
    case tmAscii: return true;
    case tmAutomatic: return AsciiFileMask.Matches(FileName, (Side == osLocal), false);
    default: assert(false); return false;
  }
}
//---------------------------------------------------------------------------
TRights __fastcall TCopyParamType::RemoteFileRights(Integer Attrs) const
{
  TRights R = Rights;
/*  if ((Attrs & faReadOnly) && PreserveReadOnly)
    R.ReadOnly = True;*/
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
    "  PrTime: %s; PrRO: %s; Rght: %s; PrR: %s; FnCs: %s; RIC: %s; "
      "Resume: %s (%d); CalcS: %s; Mask: %s\n"
    "  TM: %s; ClAr: %s; ExclM(%s): %s\n"
    "  AscM: %s\n",
    (BooleanToEngStr(PreserveTime),
     BooleanToEngStr(PreserveReadOnly),
     Rights.Text,
     BooleanToEngStr(PreserveRights),
     CaseC[FileNameCase],
     CharToHex(InvalidCharsReplacement),
     ResumeC[ResumeSupport],
     (int)ResumeThreshold,
     BooleanToEngStr(CalculateSize),
     FileMask,
     ModeC[TransferMode],
     BooleanToEngStr(ClearArchive),
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
bool __fastcall TCopyParamType::AllowTransfer(AnsiString FileName,
  TOperationSide Side, bool Directory) const
{
  bool Result = true;
  if (!ExcludeFileMask.Masks.IsEmpty())
  {
    Result = (ExcludeFileMask.Matches(FileName, (Side == osLocal), Directory) ==
      NegativeExclude);
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
    true;
}
#undef C
//---------------------------------------------------------------------------
