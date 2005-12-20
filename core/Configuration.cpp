//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <FileInfo.h>

#include "Exceptions.h"
#include "Common.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#include "TextsCore.h"
#include "Interface.h"
#define GSSAPIDLL "gssapi32"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TConfiguration::TConfiguration()
{
  FCriticalSection = new TCriticalSection();
  FUpdating = 0;
  FStorage = stDetect;
  FDontSave = false;
  FRandomSeedSave = true;
  FApplicationInfo = NULL;
  FGSSAPIInstalled = -1;
  // make sure random generator is initialised, so random_save_seed()
  // in destructor can proceed
  random_ref();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Default()
{
  TGuard Guard(FCriticalSection);

  AnsiString ARandomSeedFile = RandomSeedFile;
  // This works correct only when Default() is called before first
  // change to RandomSeedFile property
  RandomSeedFile = StringReplace(ExtractFilePath(ARandomSeedFile) +
    "winscp" + ExtractFileExt(ARandomSeedFile), "\\\\", "\\",
    TReplaceFlags() << rfReplaceAll);
  FConfirmOverwriting = true;
  FConfirmResume = true;
  FSessionReopenAuto = 5000;
  FSessionReopenNoConfirmation = 2000;

  FLogging = false;
  FPermanentLogging = false;
  FLogFileName = "";
  FPermanentLogFileName = "";
  FLogFileAppend = true;
  FLogWindowLines = 100;
  FLogProtocol = 0;

  FDisablePasswordStoring = false;
  FForceBanners = false;
  FDisableAcceptingHostKeys = false;

  Changed();
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::~TConfiguration()
{
  assert(!FUpdating);
  if (FRandomSeedSave) random_save_seed();
  random_unref();
  if (FApplicationInfo) FreeFileInfo(FApplicationInfo);
  delete FCriticalSection;
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
#define KEY(TYPE, VAR) KEYEX(TYPE, VAR, VAR)
#define REGCONFIG(ACCESS, CANCREATE, ADDON) \
  THierarchicalStorage * Storage = CreateScpStorage(false); \
  try { \
    Storage->AccessMode = ACCESS; \
    if (Storage->OpenSubKey(ConfigurationSubKey, CANCREATE)) { \
      BLOCK("Interface", CANCREATE, \
        KEY(String,   RandomSeedFile); \
        KEY(Bool,     ConfirmOverwriting); \
        KEY(Bool,     ConfirmResume); \
        KEY(Integer,  SessionReopenAuto); \
        KEY(Integer,  SessionReopenNoConfirmation); \
      ); \
      BLOCK("Logging", CANCREATE, \
        KEYEX(Bool,  PermanentLogging, Logging); \
        KEYEX(String,PermanentLogFileName, LogFileName); \
        KEY(Bool,    LogFileAppend); \
        KEY(Integer, LogWindowLines); \
        KEY(Integer, LogProtocol); \
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
  if (FDontSave) return;

  if (Storage == stRegistry) CleanupIniFile();

  #define KEYEX(TYPE, VAR, NAME) Storage->Write ## TYPE(LASTELEM(AnsiString(#NAME)), VAR)
  REGCONFIG(smReadWrite, true, SaveSpecial);
  #undef KEYEX
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadSpecial(THierarchicalStorage * /*Storage*/)
{
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  FDisablePasswordStoring = Storage->ReadBool("DisablePasswordStoring", FDisablePasswordStoring);
  FForceBanners = Storage->ReadBool("ForceBanners", FForceBanners);
  FDisableAcceptingHostKeys = Storage->ReadBool("DisableAcceptingHostKeys", FDisableAcceptingHostKeys);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Load()
{
  TGuard Guard(FCriticalSection);

  #define KEYEX(TYPE, VAR, NAME) VAR = Storage->Read ## TYPE(LASTELEM(AnsiString(#NAME)), VAR)
  #pragma warn -eas
  REGCONFIG(smRead, false, LoadSpecial);
  #pragma warn +eas
  #undef KEYEX

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
void __fastcall TConfiguration::LoadDirectoryChangesCache(const AnsiString SessionKey,
  TRemoteDirectoryChangesCache * DirectoryChangesCache)
{
  THierarchicalStorage * Storage = CreateScpStorage(false);
  try
  {
    Storage->AccessMode = smRead;
    if (Storage->OpenSubKey(ConfigurationSubKey, false) &&
        Storage->OpenSubKey("CDCache", false) &&
        Storage->ValueExists(SessionKey))
    {
      DirectoryChangesCache->Deserialize(Storage->ReadBinaryData(SessionKey));
    }
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveDirectoryChangesCache(const AnsiString SessionKey,
  TRemoteDirectoryChangesCache * DirectoryChangesCache)
{
  THierarchicalStorage * Storage = CreateScpStorage(false);
  try
  {
    Storage->AccessMode = smReadWrite;
    if (Storage->OpenSubKey(ConfigurationSubKey, true) &&
        Storage->OpenSubKey("CDCache", true))
    {
      AnsiString Data;
      DirectoryChangesCache->Serialize(Data);
      Storage->WriteBinaryData(SessionKey, Data);
    }
  }
  __finally
  {
    delete Storage;
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::BannerHash(const AnsiString & Banner)
{
  AnsiString Result;
  Result.SetLength(16);
  md5checksum(Banner.c_str(), Banner.Length(), (unsigned char*)Result.c_str());
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::ShowBanner(const AnsiString SessionKey,
  const AnsiString & Banner)
{
  bool Result;
  THierarchicalStorage * Storage = CreateScpStorage(false);
  try
  {
    Storage->AccessMode = smRead;
    Result =
      !Storage->OpenSubKey(ConfigurationSubKey, false) ||
      !Storage->OpenSubKey("Banners", false) ||
      !Storage->ValueExists(SessionKey) ||
      (Storage->ReadString(SessionKey, "") != StrToHex(BannerHash(Banner)));
  }
  __finally
  {
    delete Storage;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::NeverShowBanner(const AnsiString SessionKey,
  const AnsiString & Banner)
{
  THierarchicalStorage * Storage = CreateScpStorage(false);
  try
  {
    Storage->AccessMode = smReadWrite;

    if (Storage->OpenSubKey(ConfigurationSubKey, true) &&
        Storage->OpenSubKey("Banners", true))
    {
      Storage->WriteString(SessionKey, StrToHex(BannerHash(Banner)));
    }
  }
  __finally
  {
    delete Storage;
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
      FDontSave = true;
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
    FRandomSeedSave = false;
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
      FDontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, CLEANUP_INIFILE_ERROR);
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetOSVersionStr()
{
  AnsiString Result;
  OSVERSIONINFO OSVersionInfo;
  OSVersionInfo.dwOSVersionInfoSize = sizeof(OSVersionInfo);
  if (GetVersionEx(&OSVersionInfo) != 0)
  {
    Result = FORMAT("%d.%d.%d %s", (int(OSVersionInfo.dwMajorVersion),
      int(OSVersionInfo.dwMinorVersion), int(OSVersionInfo.dwBuildNumber),
      OSVersionInfo.szCSDVersion)).Trim();
  }
  return Result;
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
void * __fastcall TConfiguration::GetFileApplicationInfo(const AnsiString FileName)
{
  void * Result;
  if (FileName.IsEmpty())
  {
    if (!FApplicationInfo)
    {
      FApplicationInfo = CreateFileInfo(ModuleFileName());
    }
    Result = FApplicationInfo;
  }
  else
  {
    Result = CreateFileInfo(FileName);
  }
  return Result;
}
//---------------------------------------------------------------------------
void * __fastcall TConfiguration::GetApplicationInfo()
{
  return GetFileApplicationInfo("");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileProductName(const AnsiString FileName)
{
  return GetFileFileInfoString("ProductName", FileName);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileCompanyName(const AnsiString FileName)
{
  return GetFileFileInfoString("CompanyName", FileName);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetProductName()
{
  return GetFileProductName("");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetCompanyName()
{
  return GetFileCompanyName("");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileProductVersion(const AnsiString FileName)
{
  return TrimVersion(GetFileFileInfoString("ProductVersion", FileName));
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetProductVersion()
{
  return GetFileProductVersion("");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::TrimVersion(AnsiString Version)
{
  while ((Version.Pos(".") != Version.LastDelimiter(".")) &&
    (Version.SubString(Version.Length() - 1, 2) == ".0"))
  {
    Version.SetLength(Version.Length() - 2);
  }
  return Version;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetVersionStr()
{
  TGuard Guard(FCriticalSection);
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
  TGuard Guard(FCriticalSection);
  try
  {
    AnsiString Result;
    Result = TrimVersion(FORMAT("%d.%d.%d", (
      HIWORD(FixedApplicationInfo->dwFileVersionMS),
      LOWORD(FixedApplicationInfo->dwFileVersionMS),
      HIWORD(FixedApplicationInfo->dwFileVersionLS))));
    return Result;
  }
  catch (Exception &E)
  {
    throw ExtException(&E, "Can't get application version");
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileFileInfoString(const AnsiString Key,
  const AnsiString FileName)
{
  TGuard Guard(FCriticalSection);

  AnsiString Result;
  void * Info = GetFileApplicationInfo(FileName);
  try
  {
    if ((Info != NULL) && (GetTranslationCount(Info) > 0))
    {
      TTranslation Translation;
      Translation = GetTranslation(Info, 0);
      Result = ::GetFileInfoString(Info, Translation, Key);
    }
    else
    {
      assert(!FileName.IsEmpty());
    }
  }
  __finally
  {
    if (!FileName.IsEmpty())
    {
      FreeFileInfo(Info);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetFileInfoString(const AnsiString Key)
{
  return GetFileFileInfoString(Key, "");
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetRegistryStorageKey()
{
  return GetRegistryKey();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetIniFileStorageName(AnsiString value)
{
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
bool __fastcall TConfiguration::GetGSSAPIInstalled()
{
  if (FGSSAPIInstalled < 0)
  {
    HINSTANCE Library = LoadLibrary(GSSAPIDLL);
    FGSSAPIInstalled = (Library != NULL ? 1 : 0);
    FreeLibrary(Library);
  }
  return (FGSSAPIInstalled > 0);
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
  if (value.Length() >= seedpath_size())
  {
    value.SetLength(seedpath_size() - 1);
  }
  strcpy(seedpath, StripPathQuotes(value).c_str());
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
void __fastcall TConfiguration::TemporaryLogging(const AnsiString ALogFileName)
{
  FLogging = true;
  FLogFileName = ALogFileName;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogging(bool value)
{
  if (Logging != value)
  {
    FPermanentLogging = value;
    FLogging = value;
    Changed();
  }
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileName(AnsiString value)
{
  if (LogFileName != value)
  {
    FPermanentLogFileName = value;
    FLogFileName = value;
    Changed();
  }
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
void __fastcall TConfiguration::SetLogProtocol(int value)
{
  SET_CONFIG_PROPERTY(LogProtocol);
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
  return IncludeTrailingBackslash(SystemTemporaryDirectory()) + "winscp.log";
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmOverwriting(bool value)
{
  TGuard Guard(FCriticalSection);
  SET_CONFIG_PROPERTY(ConfirmOverwriting);
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetConfirmOverwriting()
{
  TGuard Guard(FCriticalSection);
  return FConfirmOverwriting;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetConfirmResume(bool value)
{
  TGuard Guard(FCriticalSection);
  SET_CONFIG_PROPERTY(ConfirmResume);
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetConfirmResume()
{
  TGuard Guard(FCriticalSection);
  return FConfirmResume;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetTimeFormat()
{
  return "h:nn:ss";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetPartialExt() const
{
  return PARTIAL_EXT;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetDefaultKeyFile()
{
  return "";
}
//---------------------------------------------------------------------------
AnsiString __fastcall TConfiguration::GetLocalInvalidChars()
{
  return "/\\:*?\"<>|";
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetRememberPassword()
{
  return false;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetSessionReopenAuto(int value)
{
  SET_CONFIG_PROPERTY(SessionReopenAuto);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetSessionReopenNoConfirmation(int value)
{
  SET_CONFIG_PROPERTY(SessionReopenNoConfirmation);
}
