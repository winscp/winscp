//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <shlobj.hpp>
#include <FileInfo.h>

#include "Exceptions.h"
#include "Common.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#include "TextsCore.h"
#include "Interface.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const char CustomCommandFileNamePattern[] = "!";
//---------------------------------------------------------------------------
bool SpecialFolderLocation(int PathID, AnsiString & Path)
{
  LPITEMIDLIST Pidl;
  char Buf[256];
  if (SHGetSpecialFolderLocation(NULL, PathID, &Pidl) == NO_ERROR &&
      SHGetPathFromIDList(Pidl, Buf))
  {
    Path = AnsiString(Buf);
    return true;
  }
  return false;
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::TConfiguration()
{
  FUpdating = 0;
  FStorage = stDetect;
  DontSave = false;
  RandomSeedSave = true;
  FApplicationInfo = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Default()
{
  AnsiString ARandomSeedFile = RandomSeedFile;
  // This works correct only when Default() is called before first
  // change to RandomSeedFile property
  RandomSeedFile = StringReplace(ExtractFilePath(ARandomSeedFile) +
    "winscp" + ExtractFileExt(ARandomSeedFile), "\\\\", "\\",
    TReplaceFlags() << rfReplaceAll);
  FIgnoreCancelBeforeFinish = TDateTime(0, 0, 3, 0);
  FConfirmOverwriting = true;
  FDefaultDirIsHome = true;
  FCopyParam.Default();

  FLogging = false;
  FLogFileName = "";
  FLogFileAppend = true;
  FLogWindowLines = 100;

  FDisablePasswordStoring = false;

  Changed();
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::~TConfiguration()
{
  assert(!FUpdating);
  if (RandomSeedSave) random_save_seed();
  if (FApplicationInfo) FreeFileInfo(FApplicationInfo);
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::CreateScpStorage(bool /*SessionList*/)
{
  if (Storage == stRegistry)
  {
    return new TRegistryStorage(RegistryStorageKey);
  }
  else
  {
    return new TIniFileStorage(IniFileStorageName);
  }
}
//---------------------------------------------------------------------------
#define LASTELEM(ELEM) \
  ELEM.SubString(ELEM.LastDelimiter(".>")+1, ELEM.Length() - ELEM.LastDelimiter(".>"))
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKey(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKey(); }
#define REGCONFIG(ACCESS, CANCREATE, ADDON) \
  THierarchicalStorage * Storage = CreateScpStorage(false); \
  try { \
    Storage->AccessMode = ACCESS; \
    if (Storage->OpenSubKey(ConfigurationSubKey, CANCREATE)) { \
      BLOCK("Interface", CANCREATE, \
        KEY(Bool,     DefaultDirIsHome); \
        KEY(String,   RandomSeedFile); \
        KEY(DateTime, IgnoreCancelBeforeFinish); \
        KEY(Bool,     ConfirmOverwriting); \
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
      ); \
      BLOCK("Logging", CANCREATE, \
        KEY(Bool,    Logging); \
        KEY(String,  LogFileName); \
        KEY(Bool,    LogFileAppend); \
        KEY(Integer, LogWindowLines); \
      ); \
      ADDON(Storage); \
    }; \
  } __finally { \
    delete Storage; \
  }
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveSpecial(THierarchicalStorage * /*Storage*/)
{
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Save()
{
  if (DontSave) return;

  if (Storage == stRegistry) CleanupIniFile();

  #define KEY(TYPE, VAR) Storage->Write ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  REGCONFIG(smReadWrite, true, SaveSpecial);
  #undef KEY
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadSpecial(THierarchicalStorage * /*Storage*/)
{
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  FDisablePasswordStoring = Storage->ReadBool("DisablePasswordStoring", FDisablePasswordStoring);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Load()
{
  #define KEY(TYPE, VAR) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#VAR)), VAR)
  #pragma warn -eas
  REGCONFIG(smRead, false, LoadSpecial);
  #pragma warn +eas
  #undef KEY

  TRegistryStorage * AdminStorage;
  AdminStorage = new TRegistryStorage(RegistryStorageKey, HKEY_LOCAL_MACHINE);
  try
  {
    if (AdminStorage->OpenRootKey(false))
    {
      LoadAdmin(AdminStorage);
      AdminStorage->CloseSubKey();
    }
  }
  __finally
  {
    delete AdminStorage;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Changed()
{
  if (FUpdating == 0)
  {
    if (OnChange)
    {
      OnChange(this);
    }
  }
  else
  {
    FChanged = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::BeginUpdate()
{
  if (FUpdating == 0)
  {
    FChanged = false;
  }
  FUpdating++;
  // Greater value would probably indicate some nesting problem in code
  assert(FUpdating < 6);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::EndUpdate()
{
  assert(FUpdating > 0);
  FUpdating--;
  if ((FUpdating == 0) && FChanged)
  {
    FChanged = false;
    Changed();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupConfiguration()
{
  try
  {
    CleanupRegistry(ConfigurationSubKey);
    if (Storage == stRegistry)
    {
      DontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_CONFIG_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRegistry(AnsiString CleanupSubKey)
{
  TRegistryStorage *Registry = new TRegistryStorage(RegistryStorageKey);
  try
  {
    Registry->RecursiveDeleteSubKey(CleanupSubKey);
  }
  __finally
  {
    delete Registry;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupHostKeys()
{
  try
  {
    CleanupRegistry(SshHostKeysSubKey);
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_HOSTKEYS_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRandomSeedFile()
{
  try
  {
    RandomSeedSave = false;
    if (FileExists(RandomSeedFile))
    {
      if (!DeleteFile(RandomSeedFile)) Abort();
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_SEEDFILE_ERROR);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupIniFile()
{
  try
  {
    if (FileExists(IniFileStorageName))
    {
      if (!DeleteFile(IniFileStorageName)) Abort();
    }
    if (Storage == stIniFile)
    {
      DontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_INIFILE_ERROR);
  }
}
//---------------------------------------------------------------------------
TVSFixedFileInfo *__fastcall TConfiguration::GetFixedApplicationInfo()
{
  return GetFixedFileInfo(ApplicationInfo);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::ModuleFileName()
{
  return ParamStr(0);
}
//---------------------------------------------------------------------------
void * __fastcall TConfiguration::GetApplicationInfo()
{
  if (!FApplicationInfo)
  {
    FApplicationInfo = CreateFileInfo(ModuleFileName());
  }
  return FApplicationInfo;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetProductVersion()
{
  return TrimVersion(FileInfoString["ProductVersion"]);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::TrimVersion(AnsiString Version)
{
  while (Version.SubString(Version.Length() - 1, 2) == ".0")
  {
    Version.SetLength(Version.Length() - 2);
  }
  return Version;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetVersionStr()
{
  try
  {
    return FmtLoadStr(VERSION, ARRAYOFCONST((
      HIWORD(FixedApplicationInfo->dwFileVersionMS),
      LOWORD(FixedApplicationInfo->dwFileVersionMS),
      HIWORD(FixedApplicationInfo->dwFileVersionLS),
      LOWORD(FixedApplicationInfo->dwFileVersionLS))));
  }
  catch (Exception &E)
  {
    throw ExtException(&E, "Can't get application version");
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetVersion()
{
  try
  {
    AnsiString Result;
    Result = TrimVersion(FORMAT("%d.%d.%d.%d", (HIWORD(FixedApplicationInfo->dwFileVersionMS),
      LOWORD(FixedApplicationInfo->dwFileVersionMS),
      HIWORD(FixedApplicationInfo->dwFileVersionLS),
      LOWORD(FixedApplicationInfo->dwFileVersionLS))));
    return Result;
  }
  catch (Exception &E)
  {
    throw ExtException(&E, "Can't get application version");
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileInfoString(const AnsiString Key)
{
  AnsiString Result;
  if (GetTranslationCount(ApplicationInfo) > 0)
  {
    TTranslation Translation;
    Translation = GetTranslation(ApplicationInfo, 0);
    Result = ::GetFileInfoString(ApplicationInfo,
      Translation, Key);
    PackStr(Result);
  }
  else
  {
    assert(false);
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRegistryStorageKey()
{
  return GetRegistryKey();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetIniFileStorageName(AnsiString value)
{
  if (!value.IsEmpty() && !FileExists(value))
  {
    throw Exception(FMTLOAD(FILE_NOT_EXISTS, (value)));
  }
  FIniFileStorageName = value;
  FStorage = stIniFile;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetIniFileStorageName()
{
  if (FIniFileStorageName.IsEmpty())
  {
    return ChangeFileExt(ParamStr(0), ".ini");
  }
  else
  {
    return FIniFileStorageName;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPuttyRegistryStorageKey()
{
  return PUTTY_REG_POS;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPuttySessionsKey()
{
  return PuttyRegistryStorageKey + "\\Sessions";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetStoredSessionsSubKey()
{
  return "Sessions";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetSshHostKeysSubKey()
{
  return "SshHostKeys";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetConfigurationSubKey()
{
  return "Configuration";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRootKeyStr()
{
  return RootKeyToStr(HKEY_CURRENT_USER);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetStorage(TStorage value)
{
  if (FStorage != value)
  {
    FStorage = value;
    ModifyAll();
    Save();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::ModifyAll()
{
  // nothing
}
//---------------------------------------------------------------------------
TStorage __fastcall TConfiguration::GetStorage()
{
  if (FStorage == stDetect)
  {
    FStorage = FileExists(IniFileStorageName) ? stIniFile : stRegistry;
  }
  return FStorage;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetRandomSeedFile(AnsiString value)
{
  char *seedpath = seedpath_ptr();
  if (value.Length() > seedpath_size())
  {
    value.SetLength(seedpath_size());
  }
  strcpy(seedpath, StripPathQuotes(value).c_str());
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCopyParam(TCopyParamType value)
{
  FCopyParam.Assign(value);
  Changed();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRandomSeedFile()
{
  return AnsiString(seedpath_ptr());
}
//---------------------------------------------------------------------------
TEOLType __fastcall TConfiguration::GetLocalEOLType()
{
  return eolCRLF;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogging(bool value)
{
  SET_CONFIG_PROPERTY(Logging);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileName(AnsiString value)
{
  SET_CONFIG_PROPERTY(LogFileName);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogToFile(bool value)
{
  if (value != LogToFile)
  {
    LogFileName = value ? DefaultLogFileName : AnsiString("");
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogToFile()
{
  return !LogFileName.IsEmpty();
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileAppend(bool value)
{
  SET_CONFIG_PROPERTY(LogFileAppend);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowLines(int value)
{
  SET_CONFIG_PROPERTY(LogWindowLines);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogWindowComplete(bool value)
{
  if (value != LogWindowComplete)
  {
    LogWindowLines = value ? 0 : 50;
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogWindowComplete()
{
  return (bool)(LogWindowLines == 0);
}
//---------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetDefaultLogFileName()
{
  return GetTemporaryPath() + "winscp.log";
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmOverwriting(bool value)
{
  SET_CONFIG_PROPERTY(ConfirmOverwriting);
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetConfirmOverwriting()
{
  return FConfirmOverwriting;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetDefaultDirIsHome(bool value)
{
  SET_CONFIG_PROPERTY(DefaultDirIsHome);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetIgnoreCancelBeforeFinish(TDateTime value)
{
  SET_CONFIG_PROPERTY(IgnoreCancelBeforeFinish);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetTimeFormat()
{
  return "h:nn:ss";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPartialExt() const
{
  return ".filepart";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetDefaultKeyFile()
{
  return "";
}
