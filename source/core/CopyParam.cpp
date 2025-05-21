//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "CopyParam.h"
#include "HierarchicalStorage.h"
#include "TextsCore.h"
#include "Interface.h"
//---------------------------------------------------------------------------
const wchar_t * TransferModeNames[] = { L"binary", L"ascii", L"automatic" };
const int TransferModeNamesCount = LENOF(TransferModeNames);
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
  PreserveTimeDirs = false;
  Rights.Number = TRights::rfDefault;
  PreserveRights = false; // Was True until #106
  IgnorePermErrors = false;
  AsciiFileMask.Masks = L"*.*html; *.htm; *.txt; *.php; *.php3; *.cgi; *.c; *.cpp; *.h; *.pas; "
    "*.bas; *.tex; *.pl; *.js; .htaccess; *.xtml; *.css; *.cfg; *.ini; *.sh; *.xml";
  TransferMode = tmBinary;
  AddXToDirectories = true;
  ResumeSupport = rsSmart;
  ResumeThreshold = 100 * 1024; // (100 KB)
  InvalidCharsReplacement = TokenReplacement;
  LocalInvalidChars = ::LocalInvalidChars;
  CalculateSize = true;
  FileMask = AnyMask;
  IncludeFileMask.Masks = L"";
  TransferSkipList = NULL;
  TransferResumeFile = L"";
  ClearArchive = false;
  RemoveCtrlZ = false;
  RemoveBOM = false;
  CPSLimit = 0;
  NewerOnly = false;
  EncryptNewFiles = true;
  ExcludeHiddenFiles = false;
  ExcludeEmptyDirectories = false;
  Size = -1;
  PartOffset = -1;
  PartSize = -1;
  OnceDoneOperation = odoIdle;
  OnTransferOut = NULL;
  OnTransferIn = NULL;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamType::GetInfoStr(
  UnicodeString Separator, int Attrs) const
{
  UnicodeString Result;
  bool SomeAttrIncluded;
  UnicodeString ScriptArgs;
  UnicodeString AssemblyCode;
  DoGetInfoStr(
    Separator, Attrs, Result, SomeAttrIncluded,
    UnicodeString(), ScriptArgs, TAssemblyLanguage(0), AssemblyCode);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AnyUsableCopyParam(int Attrs) const
{
  UnicodeString Result;
  bool SomeAttrIncluded;
  UnicodeString ScriptArgs;
  UnicodeString AssemblyCode;
  DoGetInfoStr(
    L";", Attrs, Result, SomeAttrIncluded,
    UnicodeString(), ScriptArgs, TAssemblyLanguage(0), AssemblyCode);
  return SomeAttrIncluded;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamType::GenerateTransferCommandArgs(int Attrs, const UnicodeString & Link) const
{
  UnicodeString Result;
  bool SomeAttrIncluded;
  UnicodeString ScriptArgs;
  UnicodeString AssemblyCode;
  DoGetInfoStr(
    L";", Attrs, Result, SomeAttrIncluded,
    Link, ScriptArgs, TAssemblyLanguage(0), AssemblyCode);
  return ScriptArgs;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamType::GenerateAssemblyCode(TAssemblyLanguage Language, int Attrs) const
{
  UnicodeString Result;
  bool SomeAttrIncluded;
  UnicodeString ScriptArgs;
  UnicodeString AssemblyCode;
  DoGetInfoStr(L";", Attrs, Result, SomeAttrIncluded, UnicodeString(), ScriptArgs, Language, AssemblyCode);
  return AssemblyCode;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::DoGetInfoStr(
  UnicodeString Separator, int Options,
  UnicodeString & Result, bool & SomeAttrIncluded,
  const UnicodeString & Link, UnicodeString & ScriptArgs, TAssemblyLanguage Language, UnicodeString & AssemblyCode) const
{
  TCopyParamType Defaults;
  TCopyParamType ScriptNonDefaults;
  TCopyParamType CodeNonDefaults;

  bool SomeAttrExcluded = false;
  SomeAttrIncluded = false;
  #define ADD(STR, EXCEPT) \
    FLAGCLEAR(Options, EXCEPT) ? (AddToList(Result, (STR), Separator), SomeAttrIncluded = true, true) : (SomeAttrExcluded = true, false)

  bool AsciiFileMaskDiffers = (TransferMode == tmAutomatic) && !(AsciiFileMask == Defaults.AsciiFileMask);
  bool TransferModeDiffers = ((TransferMode != Defaults.TransferMode) || AsciiFileMaskDiffers);

  if (FLAGCLEAR(Options, cpaIncludeMaskOnly | cpaNoTransferMode))
  {
    // Adding Transfer type unconditionally
    bool FormatMask;
    int Ident;
    switch (TransferMode)
    {
      case tmBinary:
        FormatMask = false;
        Ident = 2;
        break;
      case tmAscii:
        FormatMask = false;
        Ident = 3;
        break;
      case tmAutomatic:
      default:
        FormatMask = !(AsciiFileMask == Defaults.AsciiFileMask);
        Ident = FormatMask ? 4 : 5;
        break;
    }
    UnicodeString S = FORMAT(LoadStrPart(COPY_INFO_TRANSFER_TYPE2, 1),
      (LoadStrPart(COPY_INFO_TRANSFER_TYPE2, Ident)));
    if (FormatMask)
    {
      S = FORMAT(S, (AsciiFileMask.Masks));
    }
    AddToList(Result, S, Separator);

    if (TransferModeDiffers)
    {
      ADD("", cpaIncludeMaskOnly | cpaNoTransferMode);

      ScriptArgs += RtfSwitchValue(TRANSFER_SWITCH, Link, TransferModeNames[TransferMode]);
      const wchar_t * TransferModeMembers[] = { L"Binary", L"Ascii", L"Automatic" };
      AssemblyCode += AssemblyProperty(
        Language, TransferOptionsClassName, L"TransferMode", L"TransferMode", TransferModeMembers[TransferMode], false);
      if (AsciiFileMaskDiffers)
      {
        ScriptNonDefaults.AsciiFileMask = AsciiFileMask;
        CodeNonDefaults.AsciiFileMask = AsciiFileMask;
      }
    }
  }
  else
  {
    if (TransferModeDiffers)
    {
      SomeAttrExcluded = true;
    }
  }

  if (FileNameCase != Defaults.FileNameCase)
  {
    if (ADD(FORMAT(LoadStrPart(COPY_INFO_FILENAME, 1),
         (LoadStrPart(COPY_INFO_FILENAME, FileNameCase + 2))),
         cpaIncludeMaskOnly))
    {
      ScriptNonDefaults.FileNameCase = FileNameCase;
      CodeNonDefaults.FileNameCase = FileNameCase;
    }
  }

  if (InvalidCharsReplacement != Defaults.InvalidCharsReplacement)
  {
    int Except = cpaIncludeMaskOnly;
    if (InvalidCharsReplacement == NoReplacement)
    {
      ADD(LoadStr(COPY_INFO_DONT_REPLACE_INV_CHARS), Except);
    }

    if (FLAGCLEAR(Options, Except))
    {
      ScriptNonDefaults.InvalidCharsReplacement = InvalidCharsReplacement;
      CodeNonDefaults.InvalidCharsReplacement = InvalidCharsReplacement;
    }
  }

  if ((PreserveRights != Defaults.PreserveRights) ||
      (PreserveRights &&
       ((Rights != Defaults.Rights) || (AddXToDirectories != Defaults.AddXToDirectories))))
  {
    const int Except = cpaIncludeMaskOnly | cpaNoRights;
    if (DebugAlwaysTrue(PreserveRights))
    {
      UnicodeString RightsStr = Rights.Text;
      if (AddXToDirectories)
      {
        RightsStr += L", " + LoadStr(COPY_INFO_ADD_X_TO_DIRS);
      }
      ADD(FORMAT(LoadStr(COPY_INFO_PERMISSIONS), (RightsStr)),
        Except);

      if (FLAGCLEAR(Options, Except))
      {
        ScriptArgs += RtfSwitchValue(PERMISSIONS_SWITCH, Link, Rights.Octal);

        const UnicodeString FilePermissionsClassName = L"FilePermissions";
        const bool Inline = true;
        UnicodeString FilePermissions =
          AssemblyNewClassInstanceStart(Language, FilePermissionsClassName, Inline) +
          AssemblyProperty(Language, FilePermissionsClassName, L"Octal", Rights.Octal, Inline) +
          AssemblyNewClassInstanceEnd(Language, Inline);

        AssemblyCode += AssemblyPropertyRaw(Language, TransferOptionsClassName, L"FilePermissions", FilePermissions, false);
      }
    }

    if ((AddXToDirectories != Defaults.AddXToDirectories) && FLAGCLEAR(Options, Except))
    {
      ScriptNonDefaults.AddXToDirectories = AddXToDirectories;
      CodeNonDefaults.AddXToDirectories = AddXToDirectories;
    }
  }

  bool APreserveTimeDirs = PreserveTime && PreserveTimeDirs;
  if ((PreserveTime != Defaults.PreserveTime) || (APreserveTimeDirs != Defaults.PreserveTimeDirs))
  {
    bool AddPreserveTime = false;
    UnicodeString Str = LoadStr(PreserveTime ? COPY_INFO_TIMESTAMP : COPY_INFO_DONT_PRESERVE_TIME);

    const int ExceptDirs = cpaNoPreserveTimeDirs;
    if (APreserveTimeDirs != Defaults.PreserveTimeDirs)
    {
      if (DebugAlwaysTrue(PreserveTimeDirs))
      {
        if (FLAGCLEAR(Options, ExceptDirs))
        {
          Str = FMTLOAD(COPY_INFO_PRESERVE_TIME_DIRS, (Str));
          AddPreserveTime = true;
        }
      }
      ADD("", ExceptDirs);
    }

    const int Except = cpaIncludeMaskOnly | cpaNoPreserveTime;
    if (PreserveTime != Defaults.PreserveTime)
    {
      if (FLAGCLEAR(Options, Except))
      {
        AddPreserveTime = true;
      }
      ADD(L"", Except);
    }

    if (AddPreserveTime)
    {
      AddToList(Result, Str, Separator);
    }

    if (FLAGCLEAR(Options, Except))
    {
      if (PreserveTime)
      {
        if (PreserveTimeDirs && FLAGCLEAR(Options, ExceptDirs))
        {
          ScriptArgs += RtfSwitchValue(PRESERVETIME_SWITCH, Link, PRESERVETIMEDIRS_SWITCH_VALUE);
          CodeNonDefaults.PreserveTimeDirs = PreserveTimeDirs;
        }
        else
        {
          ScriptArgs += RtfSwitch(PRESERVETIME_SWITCH, Link);
        }
      }
      else
      {
        ScriptArgs += RtfSwitch(NOPRESERVETIME_SWITCH, Link);
        AssemblyCode += AssemblyProperty(Language, TransferOptionsClassName, L"PreserveTimestamp", false, false);
      }
    }
  }

  if ((PreserveRights || PreserveTime) &&
      (IgnorePermErrors != Defaults.IgnorePermErrors))
  {
    if (DebugAlwaysTrue(IgnorePermErrors))
    {
      if (ADD(LoadStr(COPY_INFO_IGNORE_PERM_ERRORS), cpaIncludeMaskOnly | cpaNoIgnorePermErrors))
      {
        ScriptNonDefaults.IgnorePermErrors = IgnorePermErrors;
        CodeNonDefaults.IgnorePermErrors = IgnorePermErrors;
      }
    }
  }

  if (PreserveReadOnly != Defaults.PreserveReadOnly)
  {
    if (DebugAlwaysTrue(PreserveReadOnly))
    {
      if (ADD(LoadStr(COPY_INFO_PRESERVE_READONLY), cpaIncludeMaskOnly | cpaNoPreserveReadOnly))
      {
        ScriptNonDefaults.PreserveReadOnly = PreserveReadOnly;
        CodeNonDefaults.PreserveReadOnly = PreserveReadOnly;
      }
    }
  }

  if (CalculateSize != Defaults.CalculateSize)
  {
    if (DebugAlwaysTrue(!CalculateSize))
    {
      ADD(LoadStr(COPY_INFO_DONT_CALCULATE_SIZE), cpaIncludeMaskOnly | cpaNoCalculateSize);
      // Always false in scripting, in assembly controlled by use of FileTransferProgress
    }
  }

  if (ClearArchive != Defaults.ClearArchive)
  {
    if (DebugAlwaysTrue(ClearArchive))
    {
      if (ADD(LoadStr(COPY_INFO_CLEAR_ARCHIVE), cpaIncludeMaskOnly | cpaNoClearArchive))
      {
        ScriptNonDefaults.ClearArchive = ClearArchive;
        CodeNonDefaults.ClearArchive = ClearArchive;
      }
    }
  }

  if ((TransferMode == tmAscii) || (TransferMode == tmAutomatic))
  {
    if (RemoveBOM != Defaults.RemoveBOM)
    {
      if (DebugAlwaysTrue(RemoveBOM))
      {
        if (ADD(LoadStr(COPY_INFO_REMOVE_BOM), cpaIncludeMaskOnly | cpaNoRemoveBOM | cpaNoTransferMode))
        {
          ScriptNonDefaults.RemoveBOM = RemoveBOM;
          CodeNonDefaults.RemoveBOM = RemoveBOM;
        }
      }
    }

    if (RemoveCtrlZ != Defaults.RemoveCtrlZ)
    {
      if (DebugAlwaysTrue(RemoveCtrlZ))
      {
        if (ADD(LoadStr(COPY_INFO_REMOVE_CTRLZ), cpaIncludeMaskOnly | cpaNoRemoveCtrlZ | cpaNoTransferMode))
        {
          ScriptNonDefaults.RemoveCtrlZ = RemoveCtrlZ;
          CodeNonDefaults.RemoveCtrlZ = RemoveCtrlZ;
        }
      }
    }
  }

  if (!(IncludeFileMask == Defaults.IncludeFileMask))
  {
    if (ADD(FORMAT(LoadStr(COPY_INFO_FILE_MASK), (IncludeFileMask.Masks)), cpaNoIncludeMask))
    {
      ScriptArgs += RtfSwitch(FILEMASK_SWITCH, Link, IncludeFileMask.Masks);
      AssemblyCode += AssemblyProperty(Language, TransferOptionsClassName, L"FileMask", IncludeFileMask.Masks, false);
    }
  }

  DebugAssert(FTransferSkipList.get() == NULL);
  DebugAssert(FTransferResumeFile.IsEmpty());

  if (CPSLimit > 0)
  {
    int LimitKB = int(CPSLimit / 1024);
    if (ADD(FMTLOAD(COPY_INFO_CPS_LIMIT2, (LimitKB)), cpaIncludeMaskOnly))
    {
      ScriptArgs += RtfSwitch(SPEED_SWITCH, Link, LimitKB);
      AssemblyCode += AssemblyProperty(Language, TransferOptionsClassName, L"SpeedLimit", LimitKB, false);
    }
  }

  if (NewerOnly != Defaults.NewerOnly)
  {
    if (DebugAlwaysTrue(NewerOnly))
    {
      if (ADD(StripHotkey(LoadStr(COPY_PARAM_NEWER_ONLY)), cpaIncludeMaskOnly | cpaNoNewerOnly))
      {
        ScriptArgs += RtfSwitch(NEWERONLY_SWICH, Link);
        CodeNonDefaults.NewerOnly = NewerOnly;
      }
    }
  }

  if (EncryptNewFiles != Defaults.EncryptNewFiles)
  {
    if (!DebugAlwaysFalse(EncryptNewFiles))
    {
      if (ADD(StripHotkey(LoadStr(COPY_INFO_DONT_ENCRYPT_NEW_FILES)), cpaIncludeMaskOnly | cpaNoEncryptNewFiles))
      {
        ScriptNonDefaults.EncryptNewFiles = EncryptNewFiles;
        CodeNonDefaults.EncryptNewFiles = EncryptNewFiles;
      }
    }
  }

  if (ExcludeHiddenFiles != Defaults.ExcludeHiddenFiles)
  {
    if (DebugAlwaysTrue(ExcludeHiddenFiles))
    {
      if (ADD(StripHotkey(LoadStr(COPY_INFO_EXCLUDE_HIDDEN_FILES)), cpaNoIncludeMask))
      {
        ScriptNonDefaults.ExcludeHiddenFiles = ExcludeHiddenFiles;
        CodeNonDefaults.ExcludeHiddenFiles = ExcludeHiddenFiles;
      }
    }
  }

  if (ExcludeEmptyDirectories != Defaults.ExcludeEmptyDirectories)
  {
    if (DebugAlwaysTrue(ExcludeEmptyDirectories))
    {
      if (ADD(StripHotkey(LoadStr(COPY_INFO_EXCLUDE_EMPTY_DIRS)), 0))
      {
        ScriptNonDefaults.ExcludeEmptyDirectories = ExcludeEmptyDirectories;
        CodeNonDefaults.ExcludeEmptyDirectories = ExcludeEmptyDirectories;
      }
    }
  }

  bool ResumeThresholdDiffers = ((ResumeSupport == rsSmart) && (ResumeThreshold != Defaults.ResumeThreshold));
  if (((ResumeSupport != Defaults.ResumeSupport) || ResumeThresholdDiffers) &&
      (TransferMode != tmAscii) && FLAGCLEAR(Options, cpaNoResumeSupport))
  {
    UnicodeString Value;
    UnicodeString CodeState;
    int ResumeThresholdKB = static_cast<int>(ResumeThreshold / 1024);
    switch (ResumeSupport)
    {
      case rsOff:
        Value = ToggleNames[ToggleOff];
        CodeState = L"Off";
        break;

      case rsOn:
        Value = ToggleNames[ToggleOn];
        CodeState = L"On";
        break;

      case rsSmart:
        Value = IntToStr(ResumeThresholdKB);
        break;
    }
    ScriptArgs += RtfSwitchValue(RESUMESUPPORT_SWITCH, Link, Value);

    const UnicodeString ResumeSupportClassName = L"TransferResumeSupport";
    const bool Inline = true;
    UnicodeString ResumeSupportCode =
      AssemblyNewClassInstanceStart(Language, ResumeSupportClassName, Inline);
    if (ResumeSupport == rsSmart)
    {
      ResumeSupportCode += AssemblyProperty(Language, ResumeSupportClassName, L"Threshold", ResumeThresholdKB, Inline);
    }
    else
    {
      ResumeSupportCode += AssemblyProperty(Language, ResumeSupportClassName, L"State", L"TransferResumeSupportState", CodeState, Inline);
    }
    ResumeSupportCode += AssemblyNewClassInstanceEnd(Language, Inline);

    AssemblyCode += AssemblyPropertyRaw(Language, TransferOptionsClassName, L"ResumeSupport", ResumeSupportCode, false);
  }

  std::unique_ptr<TStringList> RawOptions;
  std::unique_ptr<TOptionsStorage> OptionsStorage;

  RawOptions.reset(new TStringList());
  OptionsStorage.reset(new TOptionsStorage(RawOptions.get(), true));
  ScriptNonDefaults.Save(OptionsStorage.get(), &Defaults);

  if (RawOptions->Count > 0)
  {
    ScriptArgs +=
      RtfSwitch(RAWTRANSFERSETTINGS_SWITCH, Link) +
      FORMAT(L"[%d]", (RawOptions->Count)) +
      StringsToParams(RawOptions.get());
  }

  RawOptions.reset(new TStringList());
  OptionsStorage.reset(new TOptionsStorage(RawOptions.get(), true));
  CodeNonDefaults.Save(OptionsStorage.get(), &Defaults);

  if (!AssemblyCode.IsEmpty())
  {
    AssemblyCode =
      AssemblyNewClassInstanceStart(Language, TransferOptionsClassName, false) +
      AssemblyCode +
      AssemblyNewClassInstanceEnd(Language, false);
  }

  if (RawOptions->Count > 0)
  {
    if (AssemblyCode.IsEmpty())
    {
      AssemblyCode = AssemblyVariableDeclaration(Language) + AssemblyNewClassInstance(Language, TransferOptionsClassName, false);
      if (Language == alCSharp)
      {
        AssemblyCode += RtfText(L"()");
      }
      AssemblyCode += AssemblyStatementSeparator(Language) + RtfPara;
    }
    else
    {
      AssemblyCode += RtfPara;
    }

    AssemblyCode +=
      AssemblyAddRawSettings(Language, RawOptions.get(), TransferOptionsClassName, L"AddRawSettings");
  }

  if (SomeAttrExcluded)
  {
    Result += (Result.IsEmpty() ? UnicodeString() : Separator) +
      FORMAT(LoadStrPart(COPY_INFO_NOT_USABLE, 1),
        (LoadStrPart(COPY_INFO_NOT_USABLE, (SomeAttrIncluded ? 2 : 3))));
  }
  else if (Result.IsEmpty())
  {
    Result = LoadStr(COPY_INFO_DEFAULT);
  }
  #undef ADD
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Assign(const TCopyParamType * Source)
{
  DebugAssert(Source != NULL);
  #define COPY(Prop) Prop = Source->Prop
  COPY(FileNameCase);
  COPY(PreserveReadOnly);
  COPY(PreserveTime);
  COPY(PreserveTimeDirs);
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
  COPY(IncludeFileMask);
  COPY(TransferSkipList);
  COPY(TransferResumeFile);
  COPY(ClearArchive);
  COPY(RemoveCtrlZ);
  COPY(RemoveBOM);
  COPY(CPSLimit);
  COPY(NewerOnly);
  COPY(EncryptNewFiles);
  COPY(ExcludeHiddenFiles);
  COPY(ExcludeEmptyDirectories);
  COPY(Size);
  COPY(PartOffset);
  COPY(PartSize);
  COPY(OnceDoneOperation);
  COPY(OnTransferOut);
  COPY(OnTransferIn);
  #undef COPY
}
//---------------------------------------------------------------------------
TCopyParamType & __fastcall TCopyParamType::operator =(const TCopyParamType & rhp)
{
  Assign(&rhp);
  return *this;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::SetLocalInvalidChars(UnicodeString value)
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
UnicodeString __fastcall TCopyParamType::ValidLocalFileName(UnicodeString FileName) const
{
  return ::ValidLocalFileName(FileName, InvalidCharsReplacement, FTokenizibleChars, LocalInvalidChars);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamType::RestoreChars(UnicodeString FileName) const
{
  if (InvalidCharsReplacement == TokenReplacement)
  {
    wchar_t * InvalidChar = FileName.c_str();
    while ((InvalidChar = wcschr(InvalidChar, TokenPrefix)) != NULL)
    {
      int Index = InvalidChar - FileName.c_str() + 1;
      if (FileName.Length() >= Index + 2)
      {
        UnicodeString Hex = FileName.SubString(Index + 1, 2);
        wchar_t Char = static_cast<wchar_t>(HexToByte(Hex));
        if ((Char != L'\0') && (Char != L'/') &&
            ((FTokenizibleChars.Pos(Char) > 0) ||
             // not decoding lone dot
             (((Char == L' ') || ((Char == L'.') && (Index > 1))) && (Index == FileName.Length() - 2))))
        {
          FileName[Index] = Char;
          FileName.Delete(Index + 1, 2);
          InvalidChar = FileName.c_str() + Index;
        }
        else if ((Hex == L"00") &&
                 ((Index == FileName.Length() - 2) || (FileName[Index + 3] == L'.')) &&
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
UnicodeString __fastcall TCopyParamType::ValidLocalPath(UnicodeString Path) const
{
  UnicodeString Result;
  while (!Path.IsEmpty())
  {
    if (!Result.IsEmpty())
    {
      Result += L"\\";
    }
    Result += ValidLocalFileName(CutToChar(Path, L'\\', false));
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamType::ChangeFileName(UnicodeString FileName,
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
      if ((FileName.Length() <= 12) && (FileName.Pos(L".") <= 9) &&
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
bool __fastcall TCopyParamType::UseAsciiTransfer(UnicodeString FileName,
  TOperationSide Side, const TFileMasks::TParams & Params) const
{
  switch (TransferMode)
  {
    case tmBinary: return false;
    case tmAscii: return true;
    case tmAutomatic: return AsciiFileMask.Matches(FileName, (Side == osLocal),
      false, &Params);
    default: DebugFail(); return false;
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
UnicodeString __fastcall TCopyParamType::GetLogStr() const
{
  wchar_t CaseC[] = L"NULFS";
  wchar_t ModeC[] = L"BAM";
  wchar_t ResumeC[] = L"YSN";
  // OpenArray (ARRAYOFCONST) supports only up to 19 arguments, so we had to split it
  return
    FORMAT(
      L"  PrTime: %s%s; PrRO: %s; Rght: %s; PrR: %s (%s); FnCs: %s; RIC: %s; "
         "Resume: %s (%d); CalcS: %s; Mask: %s\n",
      (BooleanToEngStr(PreserveTime),
       UnicodeString(PreserveTime && PreserveTimeDirs ? L"+Dirs" : L""),
       BooleanToEngStr(PreserveReadOnly),
       Rights.Text,
       BooleanToEngStr(PreserveRights),
       BooleanToEngStr(IgnorePermErrors),
       CaseC[FileNameCase],
       CharToHex(InvalidCharsReplacement),
       ResumeC[ResumeSupport],
       (int)ResumeThreshold,
       BooleanToEngStr(CalculateSize),
       FileMask)) +
    FORMAT(
      L"  TM: %s; ClAr: %s; RemEOF: %s; RemBOM: %s; CPS: %u; NewerOnly: %s; EncryptNewFiles: %s; ExcludeHiddenFiles: %s; ExcludeEmptyDirectories: %s; InclM: %s; ResumeL: %d\n"
       "  AscM: %s\n",
      (ModeC[TransferMode],
       BooleanToEngStr(ClearArchive),
       BooleanToEngStr(RemoveCtrlZ),
       BooleanToEngStr(RemoveBOM),
       int(CPSLimit),
       BooleanToEngStr(NewerOnly),
       BooleanToEngStr(EncryptNewFiles),
       BooleanToEngStr(ExcludeHiddenFiles),
       BooleanToEngStr(ExcludeEmptyDirectories),
       IncludeFileMask.Masks,
       ((FTransferSkipList.get() != NULL) ? FTransferSkipList->Count : 0) + (!FTransferResumeFile.IsEmpty() ? 1 : 0),
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
bool __fastcall TCopyParamType::AllowResume(__int64 Size, const UnicodeString & FileName) const
{
  bool Result;
  if (FileName.Length() + PartialExt.Length() > 255) // it's a different limit than MAX_PATH
  {
    Result = false;
  }
  else
  {
    switch (ResumeSupport)
    {
      case rsOn: Result = true; break;
      case rsOff: Result = false; break;
      case rsSmart: Result = (Size >= ResumeThreshold); break;
      default: DebugFail(); Result = false; break;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowAnyTransfer() const
{
  return
    IncludeFileMask.Masks.IsEmpty() &&
    !ExcludeHiddenFiles &&
    !ExcludeEmptyDirectories &&
    ((FTransferSkipList.get() == NULL) || (FTransferSkipList->Count == 0)) &&
    FTransferResumeFile.IsEmpty();
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::AllowTransfer(UnicodeString FileName,
  TOperationSide Side, bool Directory, const TFileMasks::TParams & Params, bool Hidden) const
{
  bool Result = true;
  if (Hidden && ExcludeHiddenFiles)
  {
    Result = false;
  }
  else if (!IncludeFileMask.Masks.IsEmpty())
  {
    Result = IncludeFileMask.Matches(FileName, (Side == osLocal),
      Directory, &Params);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::SkipTransfer(
  UnicodeString FileName, bool Directory) const
{
  bool Result = false;
  // we deliberately do not filter directories, as path is added to resume list
  // when a transfer of file or directory is started,
  // so for directories we need to recurse and check every single file
  if (!Directory && (FTransferSkipList.get() != NULL))
  {
    Result = (FTransferSkipList->IndexOf(FileName) >= 0);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamType::ResumeTransfer(UnicodeString FileName) const
{
  // Returning true has the same effect as cpResume
  return
    (FileName == FTransferResumeFile) &&
    DebugAlwaysTrue(!FTransferResumeFile.IsEmpty());
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCopyParamType::GetTransferSkipList() const
{
  return FTransferSkipList.get();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::SetTransferSkipList(TStrings * value)
{
  if ((value == NULL) || (value->Count == 0))
  {
    FTransferSkipList.reset(NULL);
  }
  else
  {
    FTransferSkipList.reset(new TStringList());
    FTransferSkipList->AddStrings(value);
    FTransferSkipList->Sorted = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Load(THierarchicalStorage * Storage)
{
  AddXToDirectories = Storage->ReadBool(L"AddXToDirectories", AddXToDirectories);
  AsciiFileMask.Masks = Storage->ReadString(L"Masks", AsciiFileMask.Masks);
  FileNameCase = (TFileNameCase)Storage->ReadInteger(L"FileNameCase", FileNameCase);
  PreserveReadOnly = Storage->ReadBool(L"PreserveReadOnly", PreserveReadOnly);
  PreserveTime = Storage->ReadBool(L"PreserveTime", PreserveTime);
  PreserveTimeDirs = Storage->ReadBool(L"PreserveTimeDirs", PreserveTimeDirs);
  PreserveRights = Storage->ReadBool(L"PreserveRights", PreserveRights);
  IgnorePermErrors = Storage->ReadBool(L"IgnorePermErrors", IgnorePermErrors);
  Rights.Text = Storage->ReadString(L"Text", Rights.Text);
  TransferMode = (TTransferMode)Storage->ReadInteger(L"TransferMode", TransferMode);
  ResumeSupport = (TResumeSupport)Storage->ReadInteger(L"ResumeSupport", ResumeSupport);
  ResumeThreshold = Storage->ReadInt64(L"ResumeThreshold", ResumeThreshold);
  InvalidCharsReplacement = (wchar_t)Storage->ReadInteger(L"ReplaceInvalidChars",
    (unsigned int)InvalidCharsReplacement);
  LocalInvalidChars = Storage->ReadString(L"LocalInvalidChars", LocalInvalidChars);
  CalculateSize = Storage->ReadBool(L"CalculateSize", CalculateSize);
  if (Storage->ValueExists(L"IncludeFileMask"))
  {
    IncludeFileMask.Masks = Storage->ReadString(L"IncludeFileMask", IncludeFileMask.Masks);
  }
  else if (Storage->ValueExists(L"ExcludeFileMask"))
  {
    UnicodeString ExcludeFileMask = Storage->ReadString(L"ExcludeFileMask", L"");
    if (!ExcludeFileMask.IsEmpty())
    {
      bool NegativeExclude = Storage->ReadBool(L"NegativeExclude", false);
      if (NegativeExclude)
      {
        IncludeFileMask.Masks = ExcludeFileMask;
      }
      // convert at least simple cases to new format
      else if (ExcludeFileMask.Pos(IncludeExcludeFileMasksDelimiter) == 0)
      {
        IncludeFileMask.Masks = UnicodeString(IncludeExcludeFileMasksDelimiter) + ExcludeFileMask;
      }
    }
  }
  TransferSkipList = NULL;
  TransferResumeFile = L"";
  ClearArchive = Storage->ReadBool(L"ClearArchive", ClearArchive);
  RemoveCtrlZ = Storage->ReadBool(L"RemoveCtrlZ", RemoveCtrlZ);
  RemoveBOM = Storage->ReadBool(L"RemoveBOM", RemoveBOM);
  CPSLimit = Storage->ReadInteger(L"CPSLimit", CPSLimit);
  NewerOnly = Storage->ReadBool(L"NewerOnly", NewerOnly);
  EncryptNewFiles = Storage->ReadBool(L"EncryptNewFiles", EncryptNewFiles);
  ExcludeHiddenFiles = Storage->ReadBool(L"ExcludeHiddenFiles", ExcludeHiddenFiles);
  ExcludeEmptyDirectories = Storage->ReadBool(L"ExcludeEmptyDirectories", ExcludeEmptyDirectories);
  Size = -1;
  PartOffset = -1;
  PartSize = -1;
  OnceDoneOperation = odoIdle;
  OnTransferOut = NULL;
  OnTransferIn = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamType::Save(THierarchicalStorage * Storage, const TCopyParamType * Defaults) const
{
  // Same as in TSessionData::DoSave
  #define WRITE_DATA_EX(TYPE, NAME, PROPERTY, CONV) \
    if ((Defaults != NULL) && (CONV(Defaults->PROPERTY) == CONV(PROPERTY))) \
    { \
      Storage->DeleteValue(NAME); \
    } \
    else \
    { \
      Storage->Write ## TYPE(NAME, CONV(PROPERTY)); \
    }
  #define WRITE_DATA_CONV(TYPE, NAME, PROPERTY) WRITE_DATA_EX(TYPE, NAME, PROPERTY, WRITE_DATA_CONV_FUNC)
  #define WRITE_DATA(TYPE, PROPERTY) WRITE_DATA_EX(TYPE, TEXT(#PROPERTY), PROPERTY, )

  WRITE_DATA(Bool, AddXToDirectories);
  WRITE_DATA_EX(String, L"Masks", AsciiFileMask.Masks, );
  WRITE_DATA(Integer, FileNameCase);
  WRITE_DATA(Bool, PreserveReadOnly);
  WRITE_DATA(Bool, PreserveTime);
  WRITE_DATA(Bool, PreserveTimeDirs);
  WRITE_DATA(Bool, PreserveRights);
  WRITE_DATA(Bool, IgnorePermErrors);
  WRITE_DATA_EX(String, L"Text", Rights.Text, );
  WRITE_DATA(Integer, TransferMode);
  WRITE_DATA(Integer, ResumeSupport);
  WRITE_DATA(Int64, ResumeThreshold);
  #define WRITE_DATA_CONV_FUNC(X) (unsigned int)(X)
  WRITE_DATA_CONV(Integer, L"ReplaceInvalidChars", InvalidCharsReplacement);
  WRITE_DATA(String, LocalInvalidChars);
  WRITE_DATA(Bool, CalculateSize);
  WRITE_DATA_EX(String, L"IncludeFileMask", IncludeFileMask.Masks, );
  Storage->DeleteValue(L"ExcludeFileMask"); // obsolete
  Storage->DeleteValue(L"NegativeExclude"); // obsolete
  DebugAssert(FTransferSkipList.get() == NULL);
  DebugAssert(FTransferResumeFile.IsEmpty());
  WRITE_DATA(Bool, ClearArchive);
  WRITE_DATA(Bool, RemoveCtrlZ);
  WRITE_DATA(Bool, RemoveBOM);
  WRITE_DATA(Integer, CPSLimit);
  WRITE_DATA(Bool, NewerOnly);
  WRITE_DATA(Bool, EncryptNewFiles);
  WRITE_DATA(Bool, ExcludeHiddenFiles);
  WRITE_DATA(Bool, ExcludeEmptyDirectories);
  DebugAssert(Size < 0);
  DebugAssert(PartOffset < 0);
  DebugAssert(PartSize < 0);
  DebugAssert(OnceDoneOperation == odoIdle);
  DebugAssert(OnTransferOut == NULL);
  DebugAssert(OnTransferIn == NULL);
}
//---------------------------------------------------------------------------
#define C(Property) (Property == rhp.Property)
bool __fastcall TCopyParamType::operator==(const TCopyParamType & rhp) const
{
  DebugAssert(FTransferSkipList.get() == NULL);
  DebugAssert(FTransferResumeFile.IsEmpty());
  DebugAssert(OnTransferOut == NULL);
  DebugAssert(OnTransferIn == NULL);
  DebugAssert(rhp.FTransferSkipList.get() == NULL);
  DebugAssert(rhp.FTransferResumeFile.IsEmpty());
  DebugAssert(rhp.OnTransferOut == NULL);
  DebugAssert(rhp.OnTransferIn == NULL);
  return
    C(AddXToDirectories) &&
    C(AsciiFileMask) &&
    C(FileNameCase) &&
    C(PreserveReadOnly) &&
    C(PreserveTime) &&
    C(PreserveTimeDirs) &&
    C(PreserveRights) &&
    C(IgnorePermErrors) &&
    C(Rights) &&
    C(TransferMode) &&
    C(ResumeSupport) &&
    C(ResumeThreshold) &&
    C(InvalidCharsReplacement) &&
    C(LocalInvalidChars) &&
    C(CalculateSize) &&
    C(IncludeFileMask) &&
    C(ClearArchive) &&
    C(RemoveCtrlZ) &&
    C(RemoveBOM) &&
    C(CPSLimit) &&
    C(NewerOnly) &&
    C(EncryptNewFiles) &&
    C(ExcludeHiddenFiles) &&
    C(ExcludeEmptyDirectories) &&
    C(Size) &&
    C(PartOffset) &&
    C(PartSize) &&
    C(OnceDoneOperation) &&
    true;
}
#undef C
//---------------------------------------------------------------------------
const unsigned long MinSpeed = 8 * 1024;
const unsigned long MaxSpeed = 8 * 1024 * 1024;
//---------------------------------------------------------------------------
static bool __fastcall TryGetSpeedLimit(const UnicodeString & Text, unsigned long & Speed)
{
  bool Result;
  if (AnsiSameText(Text, LoadStr(SPEED_UNLIMITED)))
  {
    Speed = 0;
    Result = true;
  }
  else
  {
    int SSpeed;
    Result = TryStrToInt(Text, SSpeed) && (SSpeed >= 0);
    if (Result)
    {
      Speed = SSpeed * 1024;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned long __fastcall GetSpeedLimit(const UnicodeString & Text)
{
  unsigned long Speed;
  if (!TryGetSpeedLimit(Text, Speed))
  {
    throw Exception(FMTLOAD(SPEED_INVALID, (Text)));
  }
  return Speed;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall SetSpeedLimit(unsigned long Limit)
{
  UnicodeString Text;
  if (Limit == 0)
  {
    Text = LoadStr(SPEED_UNLIMITED);
  }
  else
  {
    Text = IntToStr(int(Limit / 1024));
  }
  return Text;
}
//---------------------------------------------------------------------------
void __fastcall CopySpeedLimits(TStrings * Source, TStrings * Dest)
{
  std::unique_ptr<TStringList> Temp(new TStringList());

  bool Unlimited = false;
  for (int Index = 0; Index < Source->Count; Index++)
  {
    UnicodeString Text = Source->Strings[Index];
    unsigned long Speed;
    bool Valid = TryGetSpeedLimit(Text, Speed);
    if ((!Valid || (Speed == 0)) && !Unlimited)
    {
      Temp->Add(LoadStr(SPEED_UNLIMITED));
      Unlimited = true;
    }
    else if (Valid && (Speed > 0))
    {
      Temp->Add(Text);
    }
  }

  if (!Unlimited)
  {
    Temp->Insert(0, LoadStr(SPEED_UNLIMITED));
  }

  Dest->Assign(Temp.get());
}
//---------------------------------------------------------------------------
TOperationSide ReverseOperationSide(TOperationSide Side)
{
  TOperationSide Result;
  switch (Side)
  {
    case osLocal:
      Result = osRemote;
      break;

    case osRemote:
      Result = osLocal;
      break;

    default:
    case osCurrent:
      DebugFail();
      Result = Side;
      break;
  }
  return Result;
}
