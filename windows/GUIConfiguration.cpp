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
// C++B TLibModule is invalid (differs from PAS definition)
struct TPasLibModule {
  TPasLibModule * Next;
  void * Instance;
  void * CodeInstance;
  void * DataInstance;
  void * ResInstance;
};
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

  FCopyParamDialogExpanded = false;
  FErrorDialogExpanded = false;
  FContinueOnError = false;
  FSynchronizeParams = TTerminal::spDelete | TTerminal::spNoConfirmation; 
  AnsiString ProgramsFolder;
  SpecialFolderLocation(CSIDL_PROGRAM_FILES, ProgramsFolder);
  FPuttyPath = IncludeTrailingBackslash(ProgramsFolder) + "PuTTY\\putty.exe";
  FPuttySession = "WinSCP temporary session";
}
//---------------------------------------------------------------------------
// duplicated from core\configuration.cpp
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(".>")+1, ELEM.Length() - ELEM.LastDelimiter(".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(CANCREATE) \
  BLOCK("Interface", CANCREATE, \
    KEY(Bool,     CopyParamDialogExpanded); \
    KEY(Bool,     ErrorDialogExpanded); \
    KEY(Bool,     ContinueOnError); \
    KEY(Integer,  SynchronizeParams); \
    KEY(String,   PuttySession); \
    KEY(String,   PuttyPath); \
  );
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::SaveSpecial(THierarchicalStorage * Storage)
{
  TConfiguration::SaveSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) Storage->Write ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  REGCONFIG(true);
  #undef KEY
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::LoadSpecial(THierarchicalStorage * Storage)
{
  TConfiguration::LoadSpecial(Storage);

  // duplicated from core\configuration.cpp
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEY
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TPasLibModule * TGUIConfiguration::FindModule(void * Instance)
{
  TPasLibModule * CurModule;
  CurModule = reinterpret_cast<TPasLibModule*>(LibModuleList);

  while (CurModule)
  {
    if (CurModule->Instance == Instance)
    {
      break;
    }
    else
    {
      CurModule = CurModule->Next;
    }
  }
  return CurModule;
}
//---------------------------------------------------------------------------
HANDLE TGUIConfiguration::LoadNewResourceModule(LCID ALocale)
{
  HANDLE NewInstance = 0;
  bool Internal = (ALocale == InternalLocale());
  if (!Internal)
  {
    AnsiString Module;
    AnsiString LocaleName;
    char LocaleStr[4];

    Module = ModuleFileName();
    GetLocaleInfo(ALocale, LOCALE_SABBREVLANGNAME, LocaleStr, sizeof(LocaleStr));
    LocaleName = LocaleStr;
    assert(!LocaleName.IsEmpty());

    Module = ChangeFileExt(Module, AnsiString(".") + LocaleName);
    // Look for a potential language/country translation
    NewInstance = LoadLibraryEx(Module.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
    if (!NewInstance)
    {
      // Finally look for a language only translation
      Module.SetLength(Module.Length() - 1);
      NewInstance = LoadLibraryEx(Module.c_str(), 0, LOAD_LIBRARY_AS_DATAFILE);
    }
  }

  if (!NewInstance && !Internal)
  {
    throw Exception(FMTLOAD(LOCALE_LOAD_ERROR, (int(ALocale))));
  }
  else
  {
    TPasLibModule * MainModule = FindModule(HInstance);
    if (MainModule->ResInstance != MainModule->Instance)
    {
      FreeLibrary(static_cast<HMODULE>(MainModule->ResInstance));
    }
    MainModule->ResInstance = Internal ? MainModule->Instance : NewInstance;
    if (Internal)
    {
      NewInstance = MainModule->Instance;
    }
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
    if (LoadNewResourceModule(value))
    {
      FLocale = value;
      ReinitLocale();
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
    bool Result;

    try
    {
      Result = LoadNewResourceModule(value);
    }
    catch(...)
    {
      // ignore any exception while loading locale
    }

    if (Result)
    {
      FLocale = value;
      ReinitLocale();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TGUIConfiguration::ReinitLocale()
{
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
    }
  }
  __finally
  {
    delete Exts;
  }

  return FLocales;
}
