//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <Common.h>
#include "GUIConfiguration.h"
#include "GUITools.h"
#include <FileInfo.h>
#include <TextsCore.h>
#include <TextsWin.h>
#include <Terminal.h>
#include <CoreMain.h>
#include <shlobj.h>
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const int ccLocal = ccUser;
const int ccShowResults = ccUser << 1;
const int ccCopyResults = ccUser << 2;
const int ccRemoteFiles = ccUser << 3;
const int ccShowResultsInMsgBox = ccUser << 4;
const int ccSet = 0x80000000;
//---------------------------------------------------------------------------
static const unsigned int AdditionalLanguageMask = 0xFFFFFF00;
static const UnicodeString AdditionalLanguagePrefix(L"XX");
static const UnicodeString TranslationsSubFolder(L"Translations");
//---------------------------------------------------------------------------
TGUIConfiguration * GUIConfiguration = NULL;
//---------------------------------------------------------------------------
__fastcall TGUICopyParamType::TGUICopyParamType()
  : TCopyParamType()
{
  GUIDefault();
}
//---------------------------------------------------------------------------
__fastcall TGUICopyParamType::TGUICopyParamType(const TCopyParamType & Source)
  : TCopyParamType(Source)
{
  GUIDefault();
}
//---------------------------------------------------------------------------
__fastcall TGUICopyParamType::TGUICopyParamType(const TGUICopyParamType & Source)
  : TCopyParamType(Source)
{
  GUIAssign(&Source);
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Assign(const TCopyParamType * Source)
{
  TCopyParamType::Assign(Source);

  const TGUICopyParamType * GUISource;
  GUISource = dynamic_cast<const TGUICopyParamType *>(Source);
  if (GUISource != NULL)
  {
    GUIAssign(GUISource);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::GUIAssign(const TGUICopyParamType * Source)
{
  Queue = Source->Queue;
  QueueNoConfirmation = Source->QueueNoConfirmation;
  QueueParallel = Source->QueueParallel;
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Default()
{
  TCopyParamType::Default();

  GUIDefault();
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::GUIDefault()
{
  Queue = false;
  QueueNoConfirmation = true;
  QueueParallel = true;
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Load(THierarchicalStorage * Storage)
{
  TCopyParamType::Load(Storage);

  Queue = Storage->ReadBool(L"Queue", Queue);
  QueueNoConfirmation = Storage->ReadBool(L"QueueNoConfirmation", QueueNoConfirmation);
  QueueParallel = Storage->ReadBool(L"QueueParallel", QueueParallel);
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Save(THierarchicalStorage * Storage, const TCopyParamType * Defaults) const
{
  DebugAssert(Defaults == NULL);
  TCopyParamType::Save(Storage, Defaults);

  Storage->WriteBool(L"Queue", Queue);
  Storage->WriteBool(L"QueueNoConfirmation", QueueNoConfirmation);
  Storage->WriteBool(L"QueueParallel", QueueParallel);
}
//---------------------------------------------------------------------------
TGUICopyParamType & __fastcall TGUICopyParamType::operator =(const TCopyParamType & rhp)
{
  Assign(&rhp);
  return *this;
}
//---------------------------------------------------------------------------
TGUICopyParamType & __fastcall TGUICopyParamType::operator =(const TGUICopyParamType & rhp)
{
  Assign(&rhp);
  return *this;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TCopyParamRuleData::Default()
{
  HostName = L"";
  UserName = L"";
  RemoteDirectory = L"";
  LocalDirectory = L"";
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TCopyParamRule::TCopyParamRule()
{
}
//---------------------------------------------------------------------------
__fastcall TCopyParamRule::TCopyParamRule(const TCopyParamRuleData & Data)
{
  FData = Data;
}
//---------------------------------------------------------------------------
__fastcall TCopyParamRule::TCopyParamRule(const TCopyParamRule & Source)
{
  FData.HostName = Source.FData.HostName;
  FData.UserName = Source.FData.UserName;
  FData.RemoteDirectory = Source.FData.RemoteDirectory;
  FData.LocalDirectory = Source.FData.LocalDirectory;
}
//---------------------------------------------------------------------------
#define C(Property) (Property == rhp.Property)
bool __fastcall TCopyParamRule::operator==(const TCopyParamRule & rhp) const
{
  return
    C(FData.HostName) &&
    C(FData.UserName) &&
    C(FData.RemoteDirectory) &&
    C(FData.LocalDirectory) &&
    true;
}
#undef C
//---------------------------------------------------------------------------
bool __fastcall TCopyParamRule::Match(const UnicodeString & Mask,
  const UnicodeString & Value, bool Path, bool Local, int ForceDirectoryMasks) const
{
  bool Result;
  if (Mask.IsEmpty())
  {
    Result = true;
  }
  else
  {
    TFileMasks M(ForceDirectoryMasks);
    M.Masks = Mask;
    if (Path)
    {
      Result = M.Matches(Value, Local, true);
    }
    else
    {
      Result = M.MatchesFileName(Value);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamRule::Matches(const TCopyParamRuleData & Value) const
{
  return
    Match(FData.HostName, Value.HostName, false, true, 0) &&
    Match(FData.UserName, Value.UserName, false, true, 0) &&
    Match(FData.RemoteDirectory, Value.RemoteDirectory, true, false, 1) &&
    Match(FData.LocalDirectory, Value.LocalDirectory, true, true, 1);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamRule::Load(THierarchicalStorage * Storage)
{
  FData.HostName = Storage->ReadString(L"HostName", FData.HostName);
  FData.UserName = Storage->ReadString(L"UserName", FData.UserName);
  FData.RemoteDirectory = Storage->ReadString(L"RemoteDirectory", FData.RemoteDirectory);
  FData.LocalDirectory = Storage->ReadString(L"LocalDirectory", FData.LocalDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamRule::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteString(L"HostName", FData.HostName);
  Storage->WriteString(L"UserName", FData.UserName);
  Storage->WriteString(L"RemoteDirectory", FData.RemoteDirectory);
  Storage->WriteString(L"LocalDirectory", FData.LocalDirectory);
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamRule::GetEmpty() const
{
  return
    FData.HostName.IsEmpty() &&
    FData.UserName.IsEmpty() &&
    FData.RemoteDirectory.IsEmpty() &&
    FData.LocalDirectory.IsEmpty();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamRule::GetInfoStr(UnicodeString Separator) const
{
  UnicodeString Result;
  #define ADD(FMT, ELEM) \
    if (!FData.ELEM.IsEmpty()) \
      Result += (Result.IsEmpty() ? UnicodeString() : Separator) + FMTLOAD(FMT, (FData.ELEM));
  ADD(COPY_RULE_HOSTNAME, HostName);
  ADD(COPY_RULE_USERNAME, UserName);
  ADD(COPY_RULE_REMOTE_DIR, RemoteDirectory);
  ADD(COPY_RULE_LOCAL_DIR, LocalDirectory);
  #undef ADD
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
UnicodeString TCopyParamList::FInvalidChars(L"/\\[]");
//---------------------------------------------------------------------------
__fastcall TCopyParamList::TCopyParamList()
{
  Init();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Init()
{
  FCopyParams = new TList();
  FRules = new TList();
  FNames = new TStringList();
  FNameList = NULL;
  FModified = false;
}
//---------------------------------------------------------------------------
__fastcall TCopyParamList::~TCopyParamList()
{
  Clear();
  delete FCopyParams;
  delete FRules;
  delete FNames;
  delete FNameList;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Reset()
{
  SAFE_DESTROY(FNameList);
  FModified = false;
}
//---------------------------------------------------------------------
void __fastcall TCopyParamList::Modify()
{
  SAFE_DESTROY(FNameList);
  FModified = true;
}
//---------------------------------------------------------------------
void __fastcall TCopyParamList::ValidateName(const UnicodeString Name)
{
  if (Name.LastDelimiter(FInvalidChars) > 0)
  {
    throw Exception(FMTLOAD(ITEM_NAME_INVALID, (Name, FInvalidChars)));
  }
}
//---------------------------------------------------------------------------
TCopyParamList & __fastcall TCopyParamList::operator=(const TCopyParamList & rhl)
{
  Clear();

  for (int Index = 0; Index < rhl.Count; Index++)
  {
    TCopyParamType * CopyParam = new TCopyParamType(*rhl.CopyParams[Index]);
    TCopyParamRule * Rule = NULL;
    if (rhl.Rules[Index] != NULL)
    {
      Rule = new TCopyParamRule(*rhl.Rules[Index]);
    }
    Add(rhl.Names[Index], CopyParam, Rule);
  }
  // there should be comparison of with the assigned list, but we rely on caller
  // to do it instead (TGUIConfiguration::SetCopyParamList)
  Modify();
  return *this;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamList::operator==(const TCopyParamList & rhl) const
{
  bool Result = (Count == rhl.Count);
  if (Result)
  {
    int i = 0;
    while ((i < Count) && Result)
    {
      Result =
        (Names[i] == rhl.Names[i]) &&
        CompareItem(i, rhl.CopyParams[i], rhl.Rules[i]);
      i++;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TCopyParamList::IndexOfName(const UnicodeString Name) const
{
  return FNames->IndexOf(Name);
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamList::CompareItem(int Index,
  const TCopyParamType * CopyParam, const TCopyParamRule * Rule) const
{
  return
    ((*CopyParams[Index]) == *CopyParam) &&
    ((Rules[Index] == NULL) ?
      (Rule == NULL) :
      ((Rule != NULL) && (*Rules[Index]) == (*Rule)));
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Clear()
{
  for (int i = 0; i < Count; i++)
  {
    delete CopyParams[i];
    delete Rules[i];
  }
  FCopyParams->Clear();
  FRules->Clear();
  FNames->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Add(const UnicodeString Name,
  TCopyParamType * CopyParam, TCopyParamRule * Rule)
{
  Insert(Count, Name, CopyParam, Rule);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Insert(int Index, const UnicodeString Name,
  TCopyParamType * CopyParam, TCopyParamRule * Rule)
{
  DebugAssert(FNames->IndexOf(Name) < 0);
  FNames->Insert(Index, Name);
  DebugAssert(CopyParam != NULL);
  FCopyParams->Insert(Index, reinterpret_cast<TObject *>(CopyParam));
  FRules->Insert(Index, reinterpret_cast<TObject *>(Rule));
  Modify();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Change(int Index, const UnicodeString Name,
  TCopyParamType * CopyParam, TCopyParamRule * Rule)
{
  if ((Name != Names[Index]) || !CompareItem(Index, CopyParam, Rule))
  {
    FNames->Strings[Index] = Name;
    delete CopyParams[Index];
    FCopyParams->Items[Index] = (reinterpret_cast<TObject *>(CopyParam));
    delete Rules[Index];
    FRules->Items[Index] = (reinterpret_cast<TObject *>(Rule));
    Modify();
  }
  else
  {
    delete CopyParam;
    delete Rule;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Move(int CurIndex, int NewIndex)
{
  if (CurIndex != NewIndex)
  {
    FNames->Move(CurIndex, NewIndex);
    FCopyParams->Move(CurIndex, NewIndex);
    FRules->Move(CurIndex, NewIndex);
    Modify();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Delete(int Index)
{
  DebugAssert((Index >= 0) && (Index < Count));
  FNames->Delete(Index);
  delete CopyParams[Index];
  FCopyParams->Delete(Index);
  delete Rules[Index];
  FRules->Delete(Index);
  Modify();
}
//---------------------------------------------------------------------------
int __fastcall TCopyParamList::Find(const TCopyParamRuleData & Value) const
{
  int Result = -1;
  int i = 0;
  while ((i < FRules->Count) && (Result < 0))
  {
    if (FRules->Items[i] != NULL)
    {
      if (Rules[i]->Matches(Value))
      {
        Result = i;
      }
    }
    i++;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Load(THierarchicalStorage * Storage, int ACount)
{
  for (int Index = 0; Index < ACount; Index++)
  {
    UnicodeString Name = IntToStr(Index);
    TCopyParamRule * Rule = NULL;
    TCopyParamType * CopyParam = new TCopyParamType();
    try
    {
      if (Storage->OpenSubKey(Name, false))
      {
        try
        {
          Name = Storage->ReadString(L"Name", Name);
          CopyParam->Load(Storage);

          if (Storage->ReadBool(L"HasRule", false))
          {
            Rule = new TCopyParamRule();
            Rule->Load(Storage);
          }
        }
        __finally
        {
          Storage->CloseSubKey();
        }
      }
    }
    catch(...)
    {
      delete CopyParam;
      delete Rule;
      throw;
    }

    FCopyParams->Add(reinterpret_cast<TObject *>(CopyParam));
    FRules->Add(reinterpret_cast<TObject *>(Rule));
    FNames->Add(Name);
  }
  Reset();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Save(THierarchicalStorage * Storage) const
{
  Storage->ClearSubKeys();
  for (int Index = 0; Index < Count; Index++)
  {
    if (Storage->OpenSubKey(IntToStr(Index), true))
    {
      try
      {
        const TCopyParamType * CopyParam = CopyParams[Index];
        const TCopyParamRule * Rule = Rules[Index];

        Storage->WriteString(L"Name", Names[Index]);
        CopyParam->Save(Storage);
        Storage->WriteBool(L"HasRule", (Rule != NULL));
        if (Rule != NULL)
        {
          Rule->Save(Storage);
        }
      }
      __finally
      {
        Storage->CloseSubKey();
      }
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TCopyParamList::GetCount() const
{
  return FCopyParams->Count;
}
//---------------------------------------------------------------------------
const TCopyParamRule * __fastcall TCopyParamList::GetRule(int Index) const
{
  return reinterpret_cast<TCopyParamRule *>(FRules->Items[Index]);
}
//---------------------------------------------------------------------------
const TCopyParamType * __fastcall TCopyParamList::GetCopyParam(int Index) const
{
  return reinterpret_cast<TCopyParamType *>(FCopyParams->Items[Index]);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCopyParamList::GetName(int Index) const
{
  return FNames->Strings[Index];
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCopyParamList::GetNameList() const
{
  if (FNameList == NULL)
  {
    FNameList = new TStringList();

    for (int i = 0; i < Count; i++)
    {
      FNameList->Add(FNames->Strings[i]);
    }
  }
  return FNameList;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamList::GetAnyRule() const
{
  bool Result = false;
  int i = 0;
  while ((i < Count) && !Result)
  {
    Result = (Rules[i] != NULL);
    i++;
  }
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TGUIConfiguration::TGUIConfiguration(): TConfiguration()
{
  FLocale = 0;
  SetAppliedLocale(InternalLocale(), UnicodeString());
  FLocales = new TObjectList();
  FLastLocalesExts = L"*";
  FCopyParamList = new TCopyParamList();
  CoreSetResourceModule(GetResourceModule());
}
//---------------------------------------------------------------------------
__fastcall TGUIConfiguration::~TGUIConfiguration()
{
  delete FLocales;
  delete FCopyParamList;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::Default()
{
  TConfiguration::Default();

  // reset before call to DefaultLocalized()
  FDefaultCopyParam.Default();

  FCopyParamListDefaults = true;
  DefaultLocalized();

  FIgnoreCancelBeforeFinish = TDateTime(0, 0, 3, 0);
  FContinueOnError = false;
  FConfirmCommandSession = true;
  FSynchronizeParams = TTerminal::spDefault;
  FSynchronizeModeAuto = -1;
  FSynchronizeMode = TTerminal::smRemote;
  FMaxWatchDirectories = 500;
  FSynchronizeOptions = soRecurse | soSynchronizeAsk;
  FQueueBootstrap = false;
  FQueueKeepDoneItems = true;
  FQueueKeepDoneItemsFor = 15;
  FQueueAutoPopup = true;
  FSessionRememberPassword = true;
  UnicodeString ProgramsFolder;
  SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  FDefaultPuttyPathOnly = IncludeTrailingBackslash(ProgramsFolder) + L"PuTTY\\" + OriginalPuttyExecutable;
  FDefaultPuttyPath = L"%ProgramFiles%\\PuTTY\\" + OriginalPuttyExecutable;
  FPuttyPath = FormatCommand(FDefaultPuttyPath, L"");
  FUsePuttyPwFile = asAuto;
  FPuttyPassword = false;
  FTelnetForFtpInPutty = true;
  FPuttySession = L"WinSCP temporary session";
  FBeepOnFinish = false;
  FBeepOnFinishAfter = TDateTime(0, 0, 30, 0);
  FBeepSound = L"SystemDefault";
  FCopyParamCurrent = L"";
  FKeepUpToDateChangeDelay = 500;
  FChecksumAlg = L"sha1";
  FSessionReopenAutoIdle = 9000;

  FNewDirectoryProperties.Default();
  FNewDirectoryProperties.Rights = TRights::rfDefault | TRights::rfExec;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::DefaultLocalized()
{
  if (FCopyParamListDefaults)
  {
    FCopyParamList->Clear();

    // guard against "empty resource string" from obsolete traslations
    // (DefaultLocalized is called for the first time before detection of
    // obsolete translations)
    if (!LoadStr(COPY_PARAM_PRESET_ASCII).IsEmpty())
    {
      TCopyParamType * CopyParam;

      CopyParam = new TCopyParamType(FDefaultCopyParam);
      CopyParam->TransferMode = tmAscii;
      FCopyParamList->Add(LoadStr(COPY_PARAM_PRESET_ASCII), CopyParam, NULL);

      CopyParam = new TCopyParamType(FDefaultCopyParam);
      CopyParam->TransferMode = tmBinary;
      FCopyParamList->Add(LoadStr(COPY_PARAM_PRESET_BINARY), CopyParam, NULL);

      CopyParam = new TCopyParamType(FDefaultCopyParam);
      CopyParam->NewerOnly = true;
      FCopyParamList->Add(LoadStr(COPY_PARAM_NEWER_ONLY), CopyParam, NULL);

      CopyParam = new TCopyParamType(FDefaultCopyParam);
      CopyParam->IncludeFileMask = TFileMasks(FORMAT(L"%s */", (IncludeExcludeFileMasksDelimiter)));
      FCopyParamList->Add(LoadStr(COPY_PARAM_PRESET_EXCLUDE_ALL_DIR), CopyParam, NULL);
    }

    FCopyParamList->Reset();
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::UpdateStaticUsage()
{
  TConfiguration::UpdateStaticUsage();
  Usage->Set(L"CopyParamsCount", (FCopyParamListDefaults ? 0 : FCopyParamList->Count));
  Usage->Set(L"Putty", ExtractProgramName(PuttyPath));
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKeyPath(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKeyPath(); }
#define KEY(TYPE, VAR) KEYEX(TYPE, VAR, PropertyToKey(TEXT(#VAR)))
#define REGCONFIG(CANCREATE) \
  BLOCK(L"Interface", CANCREATE, \
    KEY(Bool,     ContinueOnError); \
    KEY(Bool,     ConfirmCommandSession); \
    KEY(Integer,  SynchronizeParams); \
    KEY(Integer,  SynchronizeOptions); \
    KEY(Integer,  SynchronizeModeAuto); \
    KEY(Integer,  SynchronizeMode); \
    KEY(Integer,  MaxWatchDirectories); \
    KEY(Bool,     QueueBootstrap); \
    KEY(Integer,  QueueKeepDoneItems); \
    KEY(Integer,  QueueKeepDoneItemsFor); \
    KEY(Bool,     QueueAutoPopup); \
    KEYEX(Bool,   SessionRememberPassword, L"QueueRememberPassword"); \
    KEY(String,   PuttySession); \
    KEY(String,   PuttyPath); \
    KEY(Integer,  UsePuttyPwFile); \
    KEY(Bool,     PuttyPassword); \
    KEY(Bool,     TelnetForFtpInPutty); \
    KEY(DateTime, IgnoreCancelBeforeFinish); \
    KEY(Bool,     BeepOnFinish); \
    KEY(DateTime, BeepOnFinishAfter); \
    KEY(String,   BeepSound); \
    KEY(Integer,  KeepUpToDateChangeDelay); \
    KEY(String,   ChecksumAlg); \
    KEY(Integer,  SessionReopenAutoIdle); \
  ); \
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::DoSaveCopyParam(THierarchicalStorage * Storage, const TCopyParamType * CopyParam, const TCopyParamType * Defaults)
{
  bool Result = Storage->OpenSubKeyPath(L"Interface\\CopyParam", true);
  if (Result)
  {
    CopyParam->Save(Storage, Defaults);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SaveData(THierarchicalStorage * Storage, bool All)
{
  TConfiguration::SaveData(Storage, All);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) Storage->Write ## TYPE(NAME, VAR)
  REGCONFIG(true);
  #undef KEYEX

  if (DoSaveCopyParam(Storage, &FDefaultCopyParam, NULL))
  try
  {
    FDefaultCopyParam.Save(Storage);

    if (FCopyParamListDefaults)
    {
      DebugAssert(!FCopyParamList->Modified);
      Storage->WriteInteger(L"CopyParamList", -1);
    }
    else if (All || FCopyParamList->Modified)
    {
      Storage->WriteInteger(L"CopyParamList", FCopyParamList->Count);
      FCopyParamList->Save(Storage);
    }
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }

  if (Storage->OpenSubKeyPath(L"Interface\\NewDirectory2", true))
  try
  {
    FNewDirectoryProperties.Save(Storage);
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::LoadCopyParam(THierarchicalStorage * Storage, TCopyParamType * CopyParam)
{
  bool Result =
    Storage->OpenSubKeyPath(L"Interface\\CopyParam", false);
  if (Result)
  {
    try
    {
      CopyParam->Load(Storage);
    }
    catch (...)
    {
      Storage->CloseSubKeyPath();
      throw;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::LoadDefaultCopyParam(THierarchicalStorage * Storage)
{
  FDefaultCopyParam.Load(Storage);
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::LoadData(THierarchicalStorage * Storage)
{
  TConfiguration::LoadData(Storage);

  // duplicated from core\configuration.cpp
  #define KEYEX(TYPE, VAR, NAME) VAR = Storage->Read ## TYPE(NAME, VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEYEX

  // FDefaultCopyParam must be loaded before eventual setting defaults for CopyParamList
  if (LoadCopyParam(Storage, &FDefaultCopyParam))
  try
  {
    int CopyParamListCount = Storage->ReadInteger(L"CopyParamList", -1);
    FCopyParamListDefaults = (CopyParamListCount < 0);
    if (!FCopyParamListDefaults)
    {
      FCopyParamList->Clear();
      FCopyParamList->Load(Storage, CopyParamListCount);
    }
    else if (FCopyParamList->Modified)
    {
      FCopyParamList->Clear();
      FCopyParamListDefaults = false;
    }
    FCopyParamList->Reset();
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }

  // Make it compatible with versions prior to 3.7.1 that have not saved PuttyPath
  // with quotes. First check for absence of quotes.
  // Add quotes either if the path is set to default putty path (even if it does
  // not exists) or when the path points to existing file (so there are no parameters
  // yet in the string). Note that FileExists may display error dialog, but as
  // it should be called only for custom users path, let's expect that the user
  // can take care of it.
  if ((FPuttyPath.SubString(1, 1) != L"\"") &&
      (IsPathToSameFile(ExpandEnvironmentVariables(FPuttyPath), FDefaultPuttyPathOnly) ||
       FileExists(ApiPath(ExpandEnvironmentVariables(FPuttyPath)))))
  {
    FPuttyPath = FormatCommand(FPuttyPath, L"");
  }

  if (Storage->OpenSubKeyPath(L"Interface\\NewDirectory2", false))
  try
  {
    FNewDirectoryProperties.Load(Storage);
  }
  __finally
  {
    Storage->CloseSubKeyPath();
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::Saved()
{
  TConfiguration::Saved();

  FCopyParamList->Reset();
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
UnicodeString __fastcall TGUIConfiguration::GetTranslationModule(const UnicodeString & Path)
{
  UnicodeString SubPath =
    TPath::Combine(TPath::Combine(ExtractFilePath(Path), TranslationsSubFolder), ExtractFileName(Path));
  UnicodeString Result;
  // Prefer the SubPath. Default to SubPath.
  if (FileExists(Path) && !FileExists(SubPath))
  {
    Result = Path;
  }
  else
  {
    Result = SubPath;
  }
  return Result;
}
//---------------------------------------------------------------------------
HINSTANCE __fastcall TGUIConfiguration::LoadNewResourceModule(LCID ALocale,
  UnicodeString & FileName)
{
  UnicodeString LibraryFileName;
  HINSTANCE NewInstance = 0;
  LCID AInternalLocale = InternalLocale();
  bool Internal = (ALocale == AInternalLocale);
  DWORD PrimaryLang = PRIMARYLANGID(ALocale);
  if (!Internal)
  {
    UnicodeString Module;
    UnicodeString LocaleName;

    Module = ModuleFileName();
    if ((ALocale & AdditionalLanguageMask) != AdditionalLanguageMask)
    {
      wchar_t LocaleStr[4];
      GetLocaleInfo(ALocale, LOCALE_SABBREVLANGNAME, LocaleStr, LENOF(LocaleStr));
      LocaleName = LocaleStr;
      DebugAssert(!LocaleName.IsEmpty());
    }
    else
    {
      LocaleName = AdditionalLanguagePrefix +
        char(ALocale & ~AdditionalLanguageMask);
    }

    Module = ChangeFileExt(Module, UnicodeString(L".") + LocaleName);
    // Look for a potential language/country translation
    UnicodeString ModulePath = GetTranslationModule(Module);
    NewInstance = LoadLibraryEx(ModulePath.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
    if (NewInstance)
    {
      LibraryFileName = ModulePath;
    }
    else
    {
      DWORD SubLang = SUBLANGID(ALocale);
      DebugAssert(SUBLANG_DEFAULT == SUBLANG_CHINESE_TRADITIONAL);
      // Finally look for a language-only translation.
      // But for Chinese, never use "traditional" (what is the "default" Chinese), if we want "Simplified"
      // (the same what Inno Setup does)
      if ((PrimaryLang != LANG_CHINESE) ||
          (SubLang == SUBLANG_CHINESE_TRADITIONAL))
      {
        Module.SetLength(Module.Length() - 1);
        ModulePath = GetTranslationModule(Module);
        NewInstance = LoadLibraryEx(ModulePath.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
        if (NewInstance)
        {
          LibraryFileName = ModulePath;
        }
      }
    }
  }

  // If the locale is non-US English and we do not have that translation (and it's unlikely we ever have),
  // treat it as if it were US English.
  if (!NewInstance && !Internal && (PrimaryLang == static_cast<DWORD>(PRIMARYLANGID(AInternalLocale))))
  {
    Internal = true;
  }

  if (!NewInstance && !Internal)
  {
    throw Exception(FMTLOAD(LOCALE_LOAD_ERROR, (int(ALocale))));
  }
  else
  {
    if (Internal)
    {
      NewInstance = HInstance;
    }
  }

  FileName = LibraryFileName;

  return NewInstance;
}
//---------------------------------------------------------------------------
LCID __fastcall TGUIConfiguration::InternalLocale()
{
  LCID Result;
  if (GetTranslationCount(ApplicationInfo) > 0)
  {
    TTranslation Translation;
    Translation = GetTranslation(ApplicationInfo, 0);
    Result = MAKELANGID(PRIMARYLANGID(Translation.Language), SUBLANG_DEFAULT);
  }
  else
  {
    DebugFail();
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
LCID __fastcall TGUIConfiguration::GetLocale()
{
  return FLocale;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetLocale(LCID value)
{
  if (Locale != value)
  {
    SetLocaleInternal(value, false, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetLocaleSafe(LCID value)
{
  if (Locale != value)
  {
    SetLocaleInternal(value, true, false);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGUIConfiguration::GetAppliedLocaleHex()
{
  return IntToHex(__int64(AppliedLocale), 4);
}
//---------------------------------------------------------------------------
int __fastcall TGUIConfiguration::GetResourceModuleCompleteness(HINSTANCE /*Module*/)
{
  return 100;
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::IsTranslationComplete(HINSTANCE /*Module*/)
{
  return true;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetLocaleInternal(LCID value, bool Safe, bool CompleteOnly)
{
  LCID L = value;
  if (L == NULL)
  {
    L = GetUserDefaultUILanguage();
  }

  HINSTANCE Module = NULL;
  UnicodeString FileName;

  try
  {
    Module = LoadNewResourceModule(L, FileName);
    DebugAssert(Module != NULL);
    if (CompleteOnly && !IsTranslationComplete(Module))
    {
      Abort();
    }
  }
  catch (...)
  {
    if (Module != NULL)
    {
      FreeResourceModule(Module);
      Module = NULL;
    }

    if (Safe)
    {
      // ignore any exception while loading locale
    }
    else
    {
      throw;
    }
  }

  if (Module != NULL)
  {
    FLocale = value;
    if (CanApplyLocaleImmediately)
    {
      SetAppliedLocale(L, FileName);
      SetResourceModule(Module);
      ResStringCleanupCache();
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::GetCanApplyLocaleImmediately()
{
  return
    (Screen->FormCount == 0) &&
    (Screen->DataModuleCount == 0);
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::UsingInternalTranslation()
{
  return FLocaleModuleName.IsEmpty();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGUIConfiguration::AppliedLocaleCopyright()
{
  UnicodeString Result;
  if (UsingInternalTranslation())
  {
    DebugFail(); // we do not expect to get called with internal locale
    Result = UnicodeString();
  }
  else
  {
    Result = GetFileFileInfoString(L"LegalCopyright", FLocaleModuleName);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TGUIConfiguration::AppliedLocaleVersion()
{
  UnicodeString Result;
  if (UsingInternalTranslation())
  {
    // noop
  }
  else
  {
    Result = GetFileVersion(FLocaleModuleName);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetAppliedLocale(LCID AppliedLocale, const UnicodeString & LocaleModuleName)
{
  FAppliedLocale = AppliedLocale;
  FLocaleModuleName = LocaleModuleName;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::FreeResourceModule(HANDLE Instance)
{
  TLibModule * MainModule = FindModule(HInstance);
  if ((unsigned)Instance != MainModule->Instance)
  {
    FreeLibrary(static_cast<HMODULE>(Instance));
  }
}
//---------------------------------------------------------------------------
HANDLE __fastcall TGUIConfiguration::ChangeToDefaultResourceModule()
{
  return ChangeResourceModule(NULL);
}
//---------------------------------------------------------------------------
HANDLE __fastcall TGUIConfiguration::ChangeResourceModule(HANDLE Instance)
{
  if (Instance == NULL)
  {
    Instance = HInstance;
  }
  TLibModule * MainModule = FindModule(HInstance);
  HANDLE Result = (HANDLE)MainModule->ResInstance;
  MainModule->ResInstance = (unsigned)Instance;
  CoreSetResourceModule(Instance);
  return Result;
}
//---------------------------------------------------------------------------
HANDLE __fastcall TGUIConfiguration::GetResourceModule()
{
  return (HANDLE)FindModule(HInstance)->ResInstance;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetResourceModule(HINSTANCE Instance)
{
  HANDLE PrevHandle = ChangeResourceModule(Instance);
  FreeResourceModule(PrevHandle);

  DefaultLocalized();
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::FindLocales(const UnicodeString & Path, TStrings * Exts, UnicodeString & LocalesExts)
{
  int FindAttrs = faReadOnly | faArchive;

  TSearchRecOwned SearchRec;
  UnicodeString BaseName = ChangeFileExt(ExtractFileName(ModuleFileName()), L".");
  UnicodeString LocalesMask = TPath::Combine(Path, BaseName + L"*");
  bool Found = (FindFirstUnchecked(LocalesMask, FindAttrs, SearchRec) == 0);
  while (Found)
  {
    if (DebugAlwaysTrue(SameText(BaseName, LeftStr(SearchRec.Name, BaseName.Length()))))
    {
      UnicodeString Ext = MidStr(SearchRec.Name, BaseName.Length() + 1).UpperCase();
      // DLL is a remnant from times the .NET assembly was winscp.dll, not winscpnet.dll.
      if ((Ext.Length() >= 2) && (Ext.Length() <= 3) &&
          (Ext != L"EXE") && (Ext != L"COM") && (Ext != L"DLL") && (Ext != L"INI") && (Ext != L"MAP"))
      {
        LocalesExts += Ext + L",";
        Exts->Add(Ext);
      }
    }
    Found = (FindNextChecked(SearchRec) == 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::AddLocale(LCID Locale, const UnicodeString & AName)
{
  UnicodeString Name = AName;

  if (Name.IsEmpty())
  {
    wchar_t LocaleStr[255];
    GetLocaleInfo(Locale, LOCALE_SENGLANGUAGE, LocaleStr, LENOF(LocaleStr));
    Name = UnicodeString(LocaleStr) + TitleSeparator;
    // LOCALE_SNATIVELANGNAME
    GetLocaleInfo(Locale, LOCALE_SLANGUAGE, LocaleStr, LENOF(LocaleStr));
    Name += LocaleStr;
  }

  std::unique_ptr<TLocaleInfo> LocaleInfo(new TLocaleInfo());
  LocaleInfo->Locale = Locale;
  LocaleInfo->Name = Name;

  try
  {
    UnicodeString FileName;
    HINSTANCE Module = LoadNewResourceModule(Locale, FileName);
    try
    {
      LocaleInfo->Completeness = GetResourceModuleCompleteness(Module);
    }
    __finally
    {
      FreeResourceModule(Module);
    }
  }
  catch (...)
  {
    LocaleInfo->Completeness = -1;
  }

  FLocales->Add(LocaleInfo.release());
}
//---------------------------------------------------------------------------
int __fastcall TGUIConfiguration::LocalesCompare(void * Item1, void * Item2)
{
  TLocaleInfo * LocaleInfo1 = static_cast<TLocaleInfo *>(Item1);
  TLocaleInfo * LocaleInfo2 = static_cast<TLocaleInfo *>(Item2);
  return CompareText(LocaleInfo1->Name, LocaleInfo2->Name);
}
//---------------------------------------------------------------------------
TObjectList * __fastcall TGUIConfiguration::GetLocales()
{
  UnicodeString LocalesPath = ExtractFilePath(ModuleFileName());
  UnicodeString SubLocalesPath = TPath::Combine(LocalesPath, TranslationsSubFolder);

  UnicodeString LocalesExts;
  std::unique_ptr<TStringList> Exts(CreateSortedStringList());
  FindLocales(LocalesPath, Exts.get(), LocalesExts);
  FindLocales(SubLocalesPath, Exts.get(), LocalesExts);

  if (FLastLocalesExts != LocalesExts)
  {
    FLastLocalesExts = LocalesExts;
    FLocales->Clear();

    AddLocale(InternalLocale(), EmptyStr);

    TLanguages * Langs = Languages();
    int Count = Langs->Count;

    typedef std::map<UnicodeString, std::pair<int, DWORD> > TConflicts;
    TConflicts DefaultLangConflicts;
    LCID InvalidLocale = static_cast<LCID>(-1);

    // The two-leter Windows code is not actually unique among languages.
    // So find any duplicities and resolve them to the language, where ISO code also match.
    // Notably:
    // Georgian KAT ka-GE - Kalaallisut KAL kl-GL
    // Tamil TAI ta-IN - Tajik TAJ tj-TJ
    for (int Index = 0; Index < Count; Index++)
    {
      LCID Locale = Langs->LocaleID[Index];
      DWORD SubLang = SUBLANGID(Locale);
      if (SubLang == SUBLANG_DEFAULT)
      {
        UnicodeString LangExt2 = LeftStr(Langs->Ext[Index].UpperCase(), 2);
        TConflicts::iterator Conflict = DefaultLangConflicts.find(LangExt2);
        if (Conflict == DefaultLangConflicts.end())
        {
          Conflict = DefaultLangConflicts.insert(std::make_pair(LangExt2, std::make_pair(1, InvalidLocale))).first;
        }
        else
        {
          Conflict->second.first++;
        }

        UnicodeString LangName = CopyToChar(Langs->LocaleName[Index], L'-', false);
        bool Matches = SameText(LangName, LangExt2);
        if (Matches)
        {
          Conflict->second.second = Locale;
        }
      }
    }

    for (int Index = 0; Index < Count; Index++)
    {
      LCID Locale = Langs->LocaleID[Index];
      DWORD SubLang = SUBLANGID(Locale);
      UnicodeString LangExt3 = Langs->Ext[Index].UpperCase();
      int Ext = Exts->IndexOf(LangExt3);
      if ((Ext < 0) && (SubLang == SUBLANG_DEFAULT))
      {
        UnicodeString LangExt2 = LeftStr(LangExt3, 2);
        TConflicts::const_iterator DefaultLangConflict = DefaultLangConflicts.find(LangExt2);
        // Unless it is a conflicting extension with no resolution or resolved to another locale
        if ((DefaultLangConflict == DefaultLangConflicts.end()) ||
            (DefaultLangConflict->second.first == 1) ||
            ((DefaultLangConflict->second.second != InvalidLocale) &&
             (DefaultLangConflict->second.second == Locale)))
        {
          Ext = Exts->IndexOf(LangExt2);
        }
      }

      if ((Ext >= 0) && DebugAlwaysTrue(Exts->Objects[Ext] == NULL))
      {
        Exts->Objects[Ext] = reinterpret_cast<TObject*>(Locale);
        AddLocale(Locale, EmptyStr);
      }
    }

    for (int Index = 0; Index < Exts->Count; Index++)
    {
      if ((Exts->Objects[Index] == NULL) &&
          (Exts->Strings[Index].Length() == 3) &&
          SameText(Exts->Strings[Index].SubString(1, 2), AdditionalLanguagePrefix))
      {
        UnicodeString ModulePath = ChangeFileExt(ModuleFileName(), UnicodeString(L".") + Exts->Strings[Index]);
        ModulePath = GetTranslationModule(ModulePath);
        UnicodeString LangName = GetFileFileInfoString(L"LangName", ModulePath);
        if (!LangName.IsEmpty())
        {
          AddLocale(AdditionalLanguageMask + Exts->Strings[Index][3], LangName);
        }
      }
    }

    FLocales->Sort(LocalesCompare);
  }

  return FLocales;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetDefaultCopyParam(const TGUICopyParamType & value)
{
  FDefaultCopyParam.Assign(&value);
  Changed();
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::GetRememberPassword()
{
  bool Result = SessionRememberPassword || PuttyPassword;

  if (!Result)
  {
    try
    {
      TRemoteCustomCommand RemoteCustomCommand;
      TInteractiveCustomCommand InteractiveCustomCommand(&RemoteCustomCommand);
      UnicodeString APuttyPath = InteractiveCustomCommand.Complete(PuttyPath, false);
      Result = RemoteCustomCommand.IsPasswordCommand(PuttyPath);
    }
    catch (...)
    {
      // noop
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
const TCopyParamList * __fastcall TGUIConfiguration::GetCopyParamList()
{
  return FCopyParamList;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetCopyParamList(const TCopyParamList * value)
{
  if (!(*FCopyParamList == *value))
  {
    *FCopyParamList = *value;
    FCopyParamListDefaults = false;
    Changed();
  }
}
//---------------------------------------------------------------------------
int __fastcall TGUIConfiguration::GetCopyParamIndex()
{
  int Result;
  if (FCopyParamCurrent.IsEmpty())
  {
    Result = -1;
  }
  else
  {
    Result = FCopyParamList->IndexOfName(FCopyParamCurrent);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetCopyParamIndex(int value)
{
  UnicodeString Name;
  if (value < 0)
  {
    Name = L"";
  }
  else
  {
    Name = FCopyParamList->Names[value];
  }
  CopyParamCurrent = Name;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetCopyParamCurrent(UnicodeString value)
{
  SET_CONFIG_PROPERTY(CopyParamCurrent);
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TGUIConfiguration::GetCurrentCopyParam()
{
  return CopyParamPreset[CopyParamCurrent];
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TGUIConfiguration::GetCopyParamPreset(UnicodeString Name)
{
  TGUICopyParamType Result = FDefaultCopyParam;
  if (!Name.IsEmpty())
  {
    int Index = FCopyParamList->IndexOfName(Name);
    DebugAssert(Index >= 0);
    if (Index >= 0)
    {
      const TCopyParamType * Preset = FCopyParamList->CopyParams[Index];
      DebugAssert(Preset != NULL);
      Result.Assign(Preset); // overwrite all but GUI options
      // reset all options known not to be configurable per-preset
      // kind of hack
      Result.ResumeSupport = FDefaultCopyParam.ResumeSupport;
      Result.ResumeThreshold = FDefaultCopyParam.ResumeThreshold;
      Result.LocalInvalidChars = FDefaultCopyParam.LocalInvalidChars;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::GetHasCopyParamPreset(UnicodeString Name)
{
  return Name.IsEmpty() || (FCopyParamList->IndexOfName(Name) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetNewDirectoryProperties(
  const TRemoteProperties & value)
{
  SET_CONFIG_PROPERTY(NewDirectoryProperties);
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetQueueBootstrap(bool value)
{
  SET_CONFIG_PROPERTY(QueueBootstrap);
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetQueueKeepDoneItems(bool value)
{
  SET_CONFIG_PROPERTY(QueueKeepDoneItems);
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetQueueKeepDoneItemsFor(int value)
{
  SET_CONFIG_PROPERTY(QueueKeepDoneItemsFor);
}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TGUIConfiguration::SelectPuttySessionsForImport(
  const UnicodeString & RootKey, const UnicodeString & Source, TStoredSessionList * Sessions, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(new TStoredSessionList(true));
  ImportSessionList->DefaultSettings = Sessions->DefaultSettings;

  UnicodeString SessionsKey = GetPuttySessionsKey(RootKey);
  std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(SessionsKey));
  Storage->ConfigureForPutty();
  if (Storage->OpenRootKey(false))
  {
    ImportSessionList->Load(Storage.get(), false, true, true);
  }

  TSessionData * PuttySessionData =
    (TSessionData *)ImportSessionList->FindByName(PuttySession);
  if (PuttySessionData != NULL)
  {
    ImportSessionList->Remove(PuttySessionData);
  }
  UnicodeString NoSessionsError = FMTLOAD(PUTTY_NO_SITES2, (Source, SessionsKey));
  SelectSessionsToImportIfAny(ImportSessionList.get(), Sessions, Error, NoSessionsError);

  return ImportSessionList.release();
}
//---------------------------------------------------------------------
bool __fastcall TGUIConfiguration::AnyPuttySessionForImport(TStoredSessionList * Sessions)
{
  try
  {
    UnicodeString Error;
    std::unique_ptr<TStoredSessionList> SessionsForImport(
      SelectPuttySessionsForImport(OriginalPuttyRegistryStorageKey, L"PuTTY", Sessions, Error));
    return (SessionsForImport->Count > 0);
  }
  catch (...)
  {
    return false;
  }
}
//---------------------------------------------------------------------------
