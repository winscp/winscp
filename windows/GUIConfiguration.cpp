//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include "GUIConfiguration.h"
#include "GUITools.h"
#include <Common.h>
#include <FileInfo.h>
#include <TextsCore.h>
#include <Terminal.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const ccLocal = ccUser;
const ccShowResults = ccUser << 1;
const ccCopyResults = ccUser << 2;
//---------------------------------------------------------------------------
static const unsigned int AdditionaLanguageMask = 0xFFFFFF00;
static const AnsiString AdditionaLanguagePrefix("XX");
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
  NewerOnly = Source->NewerOnly;
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
  NewerOnly = false;
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Load(THierarchicalStorage * Storage)
{
  TCopyParamType::Load(Storage);

  Queue = Storage->ReadBool("Queue", Queue);
  QueueNoConfirmation = Storage->ReadBool("QueueNoConfirmation", QueueNoConfirmation);
  NewerOnly = Storage->ReadBool("NewerOnly", NewerOnly);
}
//---------------------------------------------------------------------------
void __fastcall TGUICopyParamType::Save(THierarchicalStorage * Storage)
{
  TCopyParamType::Save(Storage);

  Storage->WriteBool("Queue", Queue);
  Storage->WriteBool("QueueNoConfirmation", QueueNoConfirmation);
  Storage->WriteBool("NewerOnly", NewerOnly);
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
  HostName = "";
  UserName = "";
  RemoteDirectory = "";
  LocalDirectory = "";
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
bool __fastcall TCopyParamRule::Match(const AnsiString & Mask,
  const AnsiString & Value, bool Path, bool Local) const
{
  bool Result;
  if (Mask.IsEmpty())
  {
    Result = true;
  }
  else
  {
    TFileMasks M(Mask);
    if (Path)
    {
      Result = M.Matches(Value, Local, true);
    }
    else
    {
      Result = M.Matches(Value, false);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamRule::Matches(const TCopyParamRuleData & Value) const
{
  return
    Match(FData.HostName, Value.HostName, false) &&
    Match(FData.UserName, Value.UserName, false) &&
    Match(FData.RemoteDirectory, Value.RemoteDirectory, true, false) &&
    Match(FData.LocalDirectory, Value.LocalDirectory, true, true);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamRule::Load(THierarchicalStorage * Storage)
{
  FData.HostName = Storage->ReadString("HostName", FData.HostName);
  FData.UserName = Storage->ReadString("UserName", FData.UserName);
  FData.RemoteDirectory = Storage->ReadString("RemoteDirectory", FData.RemoteDirectory);
  FData.LocalDirectory = Storage->ReadString("LocalDirectory", FData.LocalDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamRule::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteString("HostName", FData.HostName);
  Storage->WriteString("UserName", FData.UserName);
  Storage->WriteString("RemoteDirectory", FData.RemoteDirectory);
  Storage->WriteString("LocalDirectory", FData.LocalDirectory);
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
AnsiString __fastcall TCopyParamRule::GetInfoStr(AnsiString Separator) const
{
  AnsiString Result;
  #define ADD(FMT, ELEM) \
    if (!FData.ELEM.IsEmpty()) \
      Result += (Result.IsEmpty() ? AnsiString() : Separator) + FMTLOAD(FMT, (FData.ELEM));
  ADD(COPY_RULE_HOSTNAME, HostName);
  ADD(COPY_RULE_USERNAME, UserName);
  ADD(COPY_RULE_REMOTE_DIR, RemoteDirectory);
  ADD(COPY_RULE_LOCAL_DIR, LocalDirectory);
  #undef ADD
  return Result;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
AnsiString TCopyParamList::FInvalidChars("/\\[]");
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
void __fastcall TCopyParamList::ValidateName(const AnsiString Name)
{
  if (Name.LastDelimiter(FInvalidChars) > 0)
  {
    throw Exception(FMTLOAD(ITEM_NAME_INVALID, (Name, FInvalidChars)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::operator=(const TCopyParamList & rhl)
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
  // there should be comparison of with the assigned list, be we rely on caller
  // to do it instead (TGUIConfiguration::SetCopyParamList)
  Modify();
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
int __fastcall TCopyParamList::IndexOfName(const AnsiString Name) const
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
void __fastcall TCopyParamList::Add(const AnsiString Name,
  TCopyParamType * CopyParam, TCopyParamRule * Rule)
{
  Insert(Count, Name, CopyParam, Rule);
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Insert(int Index, const AnsiString Name,
  TCopyParamType * CopyParam, TCopyParamRule * Rule)
{
  assert(FNames->IndexOf(Name) < 0);
  FNames->Insert(Index, Name);
  assert(CopyParam != NULL);
  FCopyParams->Insert(Index, reinterpret_cast<TObject *>(CopyParam));
  FRules->Insert(Index, reinterpret_cast<TObject *>(Rule));
  Modify();
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamList::Change(int Index, const AnsiString Name,
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
  assert((Index >= 0) && (Index < Count));
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
    AnsiString Name = IntToStr(Index);
    TCopyParamRule * Rule = NULL;
    TCopyParamType * CopyParam = new TCopyParamType();
    try
    {
      if (Storage->OpenSubKey(Name, false))
      {
        try
        {
          Name = Storage->ReadString("Name", Name);
          CopyParam->Load(Storage);

          if (Storage->ReadBool("HasRule", false))
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

        Storage->WriteString("Name", Names[Index]);
        CopyParam->Save(Storage);
        Storage->WriteBool("HasRule", (Rule != NULL));
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
AnsiString __fastcall TCopyParamList::GetName(int Index) const
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
      FNameList->Add(StripHotkey(FNames->Strings[i]));
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
  FLocales = new TStringList();
  FLastLocalesExts = "*";
  dynamic_cast<TStringList*>(FLocales)->Sorted = true;
  dynamic_cast<TStringList*>(FLocales)->CaseSensitive = false;
  FCopyParamList = new TCopyParamList();
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
  FCopyParamDialogExpanded = false;
  FErrorDialogExpanded = false;
  FContinueOnError = false;
  FConfirmCommandSession = true;
  FSynchronizeParams = TTerminal::spNoConfirmation;
  FSynchronizeModeAuto = -1;
  FSynchronizeMode = TTerminal::smRemote;
  FMaxWatchDirectories = 500;
  FSynchronizeOptions = soRecurse | soSynchronizeAsk;
  FQueueTransfersLimit = 2;
  FQueueAutoPopup = true;
  FQueueRememberPassword = false;
  AnsiString ProgramsFolder;
  SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  FDefaultPuttyPathOnly = IncludeTrailingBackslash(ProgramsFolder) + "PuTTY\\putty.exe";
  FDefaultPuttyPath = FormatCommand(FDefaultPuttyPathOnly, "");
  FPuttyPath = FDefaultPuttyPath;
  PSftpPath = FormatCommand(IncludeTrailingBackslash(ProgramsFolder) + "PuTTY\\psftp.exe", "");
  FPuttyPassword = false;
  FPuttySession = "WinSCP temporary session";
  FBeepOnFinish = false;
  FBeepOnFinishAfter = TDateTime(0, 0, 30, 0);
  FSynchronizeBrowsing = false;
  FCopyParamCurrent = "";
  FKeepUpToDateChangeDelay = 500;

  FNewDirectoryProperties.Default();
  FNewDirectoryProperties.Rights = TRights::rfDefault;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::DefaultLocalized()
{
  if (FCopyParamListDefaults)
  {
    FCopyParamList->Clear();

    // guard against "empty resourse string" from obsolete traslations
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
      CopyParam->ExcludeFileMask.Masks = "*.bak; *.tmp; ~$*; *.wbk; *~; #*; .#*";
      CopyParam->NegativeExclude = false; // just for sure
      FCopyParamList->Add(LoadStr(COPY_PARAM_PRESET_EXCLUDE), CopyParam, NULL);
    }

    FCopyParamList->Reset();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TGUIConfiguration::PropertyToKey(const AnsiString Property)
{
  // no longer useful
  int P = Property.LastDelimiter(".>");
  return Property.SubString(P + 1, Property.Length() - P);
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(CANCREATE) \
  BLOCK("Interface", CANCREATE, \
    KEY(Bool,     CopyParamDialogExpanded); \
    KEY(Bool,     ErrorDialogExpanded); \
    KEY(Bool,     ContinueOnError); \
    KEY(Bool,     ConfirmCommandSession); \
    KEY(Integer,  SynchronizeParams); \
    KEY(Integer,  SynchronizeOptions); \
    KEY(Integer,  SynchronizeModeAuto); \
    KEY(Integer,  SynchronizeMode); \
    KEY(Integer,  MaxWatchDirectories); \
    KEY(Integer,  QueueTransfersLimit); \
    KEY(Bool,     QueueAutoPopup); \
    KEY(Bool,     QueueRememberPassword); \
    KEY(String,   PuttySession); \
    KEY(String,   PuttyPath); \
    KEY(Bool,     PuttyPassword); \
    KEY(DateTime, IgnoreCancelBeforeFinish); \
    KEY(Bool,     BeepOnFinish); \
    KEY(DateTime, BeepOnFinishAfter); \
    KEY(Bool,     SynchronizeBrowsing); \
    KEY(Integer,  KeepUpToDateChangeDelay); \
  ); \
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  TConfiguration::SaveSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) Storage->Write ## TYPE(PropertyToKey(#VAR), VAR)
  REGCONFIG(true);
  #undef KEY

  if (Storage->OpenSubKey("Interface\\CopyParam", true))
  try
  {
    FDefaultCopyParam.Save(Storage);

    if (FCopyParamListDefaults)
    {
      assert(!FCopyParamList->Modified);
      Storage->WriteInteger("CopyParamList", -1);
    }
    else if (FCopyParamList->Modified)
    {
      Storage->WriteInteger("CopyParamList", FCopyParamList->Count);
      FCopyParamList->Save(Storage);
    }
  }
  __finally
  {
    Storage->CloseSubKey();
  }

  if (Storage->OpenSubKey("Interface\\NewDirectory", true))
  try
  {
    FNewDirectoryProperties.Save(Storage);
  }
  __finally
  {
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::LoadSpecial(THierarchicalStorage * Storage)
{
  TConfiguration::LoadSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(PropertyToKey(#VAR), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEY

  if (Storage->OpenSubKey("Interface\\CopyParam", false))
  try
  {
    // must be loaded before eventual setting defaults for CopyParamList
    FDefaultCopyParam.Load(Storage);

    int CopyParamListCount = Storage->ReadInteger("CopyParamList", -1);
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
    Storage->CloseSubKey();
  }

  // Make it compatible with versions prior to 3.7.1 that have not saved PuttyPath 
  // with quotes. First check for absence of quotes. 
  // Add quotes either if the path is set to default putty path (even if it does 
  // not exists) or when the path points to existing file (so there are no parameters 
  // yet in the string). Note that FileExists may display error dialog, but as
  // it should be called only for custom users path, let's expect that the user
  // can take care of it.
  if ((FPuttyPath.SubString(1, 1) != "\"") &&
      ((FPuttyPath == FDefaultPuttyPathOnly) || FileExists(FPuttyPath)))
  {
    FPuttyPath = FormatCommand(FPuttyPath, "");
  }

  if (Storage->OpenSubKey("Interface\\NewDirectory", false))
  try
  {
    FNewDirectoryProperties.Load(Storage);
  }
  __finally
  {
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::ModifyAll()
{
  TConfiguration::ModifyAll();

  if (!FCopyParamListDefaults)
  {
    FCopyParamList->Modify();
  }
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
HANDLE __fastcall TGUIConfiguration::LoadNewResourceModule(LCID ALocale,
  AnsiString * FileName)
{
  AnsiString LibraryFileName;
  HANDLE NewInstance = 0;
  bool Internal = (ALocale == InternalLocale());
  if (!Internal)
  {
    AnsiString Module;
    AnsiString LocaleName;

    Module = ModuleFileName();
    if ((ALocale & AdditionaLanguageMask) != AdditionaLanguageMask)
    {
      char LocaleStr[4];
      GetLocaleInfo(ALocale, LOCALE_SABBREVLANGNAME, LocaleStr, sizeof(LocaleStr));
      LocaleName = LocaleStr;
      assert(!LocaleName.IsEmpty());
    }
    else
    {
      LocaleName = AdditionaLanguagePrefix +
        char(ALocale & ~AdditionaLanguageMask);
    }

    Module = ChangeFileExt(Module, AnsiString(".") + LocaleName);
    // Look for a potential language/country translation
    NewInstance = LoadLibraryEx(Module.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
    if (!NewInstance)
    {
      // Finally look for a language only translation
      Module.SetLength(Module.Length() - 1);
      NewInstance = LoadLibraryEx(Module.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
      if (NewInstance)
      {
        LibraryFileName = Module;
      }
    }
    else
    {
      LibraryFileName = Module;
    }
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

  if (FileName != NULL)
  {
    *FileName = LibraryFileName;
  }

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
    assert(false);
    Result = 0;
  }
  return Result;
}
//---------------------------------------------------------------------------
LCID __fastcall TGUIConfiguration::GetLocale()
{
  if (!FLocale)
  {
    FLocale = InternalLocale();
  }
  return FLocale;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetLocale(LCID value)
{
  if (Locale != value)
  {
    HANDLE Module = LoadNewResourceModule(value);
    if (Module != NULL)
    {
      FLocale = value;
      SetResourceModule(Module);
    }
    else
    {
      assert(false);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetLocaleSafe(LCID value)
{
  if (Locale != value)
  {
    HANDLE Module;

    try
    {
      Module = LoadNewResourceModule(value);
    }
    catch(...)
    {
      // ignore any exception while loading locale
      Module = NULL;
    }

    if (Module != NULL)
    {
      FLocale = value;
      SetResourceModule(Module);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::FreeResourceModule(HANDLE Instance)
{
  TPasLibModule * MainModule = FindModule(HInstance);
  if (Instance != MainModule->Instance)
  {
    FreeLibrary(static_cast<HMODULE>(Instance));
  }
}
//---------------------------------------------------------------------------
HANDLE __fastcall TGUIConfiguration::ChangeResourceModule(HANDLE Instance)
{
  if (Instance == NULL)
  {
    Instance = HInstance;
  }
  TPasLibModule * MainModule = FindModule(HInstance);
  HANDLE Result = MainModule->ResInstance;
  MainModule->ResInstance = Instance;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetResourceModule(HANDLE Instance)
{
  HANDLE PrevHandle = ChangeResourceModule(Instance);
  FreeResourceModule(PrevHandle);

  DefaultLocalized();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TGUIConfiguration::GetLocales()
{
  AnsiString LocalesExts;
  TStringList * Exts = new TStringList();
  try
  {
    Exts->Sorted = true;
    Exts->CaseSensitive = false;

    int FindAttrs = faReadOnly | faArchive;
    TSearchRec SearchRec;
    bool Found;

    Found = (bool)(FindFirst(ChangeFileExt(ModuleFileName(), ".*"),
      FindAttrs, SearchRec) == 0);
    try
    {
      AnsiString Ext;
      while (Found)
      {
        Ext = ExtractFileExt(SearchRec.Name).UpperCase();
        if ((Ext.Length() >= 3) && (Ext != ".EXE") &&
            (Ext != ".DLL") && (Ext != ".INI"))
        {
          Ext = Ext.SubString(2, Ext.Length() - 1);
          LocalesExts += Ext;
          Exts->Add(Ext);
        }
        Found = (FindNext(SearchRec) == 0);
      }
    }
    __finally
    {
      FindClose(SearchRec);
    }

    if (FLastLocalesExts != LocalesExts)
    {
      FLastLocalesExts = LocalesExts;
      FLocales->Clear();

      TLanguages * Langs = Languages();
      int Ext, Index, Count;
      char LocaleStr[255];
      LCID Locale;

      Count = Langs->Count;
      Index = -1;
      while (Index < Count)
      {
        if (Index >= 0)
        {
          Locale = Langs->LocaleID[Index];
          Ext = Exts->IndexOf(Langs->Ext[Index]);
          if (Ext < 0)
          {
            Ext = Exts->IndexOf(Langs->Ext[Index].SubString(1, 2));
            if (Ext >= 0)
            {
              Locale = MAKELANGID(PRIMARYLANGID(Locale), SUBLANG_DEFAULT);
            }
          }

          if (Ext >= 0)
          {
            Exts->Objects[Ext] = reinterpret_cast<TObject*>(Locale);
          }
          else
          {
            Locale = 0;
          }
        }
        else
        {
          Locale = InternalLocale();
        }

        if (Locale)
        {
          AnsiString Name;
          GetLocaleInfo(Locale, LOCALE_SENGLANGUAGE,
            LocaleStr, sizeof(LocaleStr));
          Name = LocaleStr;
          Name += " - ";
          // LOCALE_SNATIVELANGNAME
          GetLocaleInfo(Locale, LOCALE_SLANGUAGE,
            LocaleStr, sizeof(LocaleStr));
          Name += LocaleStr;
          FLocales->AddObject(Name, reinterpret_cast<TObject*>(Locale));
        }
        Index++;
      }

      for (int Index = 0; Index < Exts->Count; Index++)
      {
        if ((Exts->Objects[Index] == NULL) &&
            (Exts->Strings[Index].Length() == 3) && 
            SameText(Exts->Strings[Index].SubString(1, 2), AdditionaLanguagePrefix))
        {
          AnsiString LangName = GetFileFileInfoString("LangName",
            ChangeFileExt(ModuleFileName(), AnsiString(".") + Exts->Strings[Index]));
          if (!LangName.IsEmpty())
          {
            FLocales->AddObject(LangName, reinterpret_cast<TObject*>(
              AdditionaLanguageMask + Exts->Strings[Index][3]));
          }
        }
      }
    }
  }
  __finally
  {
    delete Exts;
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
  return QueueRememberPassword || PuttyPassword;
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
  AnsiString Name;
  if (value < 0)
  {
    Name = "";
  }
  else
  {
    Name = FCopyParamList->Names[value];
  }
  CopyParamCurrent = Name;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SetCopyParamCurrent(AnsiString value)
{
  SET_CONFIG_PROPERTY(CopyParamCurrent);
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TGUIConfiguration::GetCurrentCopyParam()
{
  return CopyParamPreset[CopyParamCurrent];
}
//---------------------------------------------------------------------------
TGUICopyParamType __fastcall TGUIConfiguration::GetCopyParamPreset(AnsiString Name)
{
  TGUICopyParamType Result = FDefaultCopyParam;
  if (!Name.IsEmpty())
  {
    int Index = FCopyParamList->IndexOfName(Name);
    assert(Index >= 0);
    if (Index >= 0)
    {
      const TCopyParamType * Preset = FCopyParamList->CopyParams[Index];
      assert(Preset != NULL);
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
void __fastcall TGUIConfiguration::SetNewDirectoryProperties(
  const TRemoteProperties & value)
{
  SET_CONFIG_PROPERTY(NewDirectoryProperties);
}
//---------------------------------------------------------------------------

