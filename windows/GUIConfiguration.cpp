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
__fastcall TGUIConfiguration::TGUIConfiguration(): TConfiguration()
{
  FLocale = 0;
  FLocales = new TStringList();
  FLastLocalesExts = "*";
  dynamic_cast<TStringList*>(FLocales)->Sorted = true;
  dynamic_cast<TStringList*>(FLocales)->CaseSensitive = false;
}
//---------------------------------------------------------------------------
__fastcall TGUIConfiguration::~TGUIConfiguration()
{
  delete FLocales;
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::Default()
{
  TConfiguration::Default();

  FCopyParam.Default();
  FIgnoreCancelBeforeFinish = TDateTime(0, 0, 3, 0);
  FCopyParamDialogExpanded = false;
  FErrorDialogExpanded = false;
  FContinueOnError = false;
  FConfirmCommandSession = true;
  FSynchronizeParams = TTerminal::spDelete | TTerminal::spNoConfirmation;
  FSynchronizeRecurse = true; 
  FQueueTransfersLimit = 2;
  FQueueAutoPopup = true;
  FQueueRememberPassword = false;
  AnsiString ProgramsFolder;
  SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  FDefaultPuttyPath = IncludeTrailingBackslash(ProgramsFolder) + "PuTTY\\putty.exe";
  FPuttyPath = FormatCommand(FDefaultPuttyPath, "");
  FPuttyPassword = false;
  FPuttySession = "WinSCP temporary session";
  FBeepOnFinish = false;
  FBeepOnFinishAfter = TDateTime(0, 0, 30, 0);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TGUIConfiguration::PropertyToKey(const AnsiString Property)
{
  if (Property == "CopyParam.ExcludeFileMask.Masks")
  {
    return "ExcludeFileMask";
  }
  else
  {
    int P = Property.LastDelimiter(".>");
    return Property.SubString(P + 1, Property.Length() - P);
  }
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
    KEY(Bool,     SynchronizeRecurse); \
    KEY(Integer,  QueueTransfersLimit); \
    KEY(Bool,     QueueAutoPopup); \
    KEY(Bool,     QueueRememberPassword); \
    KEY(String,   PuttySession); \
    KEY(String,   PuttyPath); \
    KEY(Bool,     PuttyPassword); \
    KEY(DateTime, IgnoreCancelBeforeFinish); \
    KEY(Bool,     BeepOnFinish); \
    KEY(DateTime, BeepOnFinishAfter); \
  ); \
  BLOCK("Interface\\CopyParam", CANCREATE, \
    KEY(Bool,    CopyParam.AddXToDirectories); \
    KEY(String,  CopyParam.AsciiFileMask.Masks); \
    KEY(Integer, CopyParam.FileNameCase); \
    KEY(Bool,    CopyParam.PreserveReadOnly); \
    KEY(Bool,    CopyParam.PreserveTime); \
    KEY(Bool,    CopyParam.PreserveRights); \
    KEY(String,  CopyParam.Rights.Text); \
    KEY(Integer, CopyParam.TransferMode); \
    KEY(Integer, CopyParam.ResumeSupport); \
    KEY(Int64,   CopyParam.ResumeThreshold); \
    KEY(Bool,    CopyParam.ReplaceInvalidChars); \
    KEY(String,  CopyParam.LocalInvalidChars); \
    KEY(Bool,    CopyParam.CalculateSize); \
    KEY(Bool,    CopyParam.Queue); \
    KEY(Bool,    CopyParam.QueueNoConfirmation); \
    KEY(String,  CopyParam.ExcludeFileMask.Masks); \
    KEY(Bool,    CopyParam.ClearArchive); \
  ); \
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  TConfiguration::SaveSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) Storage->Write ## TYPE(PropertyToKey(#VAR), VAR)
  REGCONFIG(true);
  #undef KEY
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

  // Make it compatible with versions prior to 3.7.1 that have not saved PuttyPath 
  // with quotes. First check for absence of quotes. 
  // Add quotes either if the path is set to default putty path (even if it does 
  // not exists) or when the path points to existing file (so there are no parameters 
  // yet in the string). Note that FileExists may display error dialog, but as
  // it should be called only for custom users path, let's expect that the user
  // can take care of it.
  if ((FPuttyPath.SubString(1, 1) != "\"") &&
      ((FPuttyPath == FDefaultPuttyPath) || FileExists(FPuttyPath)))
  {
    FPuttyPath = FormatCommand(FPuttyPath, "");
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
    FreeLibrary(Instance);
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
void __fastcall TGUIConfiguration::SetCopyParam(TGUICopyParamType value)
{
  FCopyParam.Assign(&value);
  Changed();
}
//---------------------------------------------------------------------------
bool __fastcall TGUIConfiguration::GetRememberPassword()
{
  return QueueRememberPassword || PuttyPassword;
}

