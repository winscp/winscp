//---------------------------------------------------------------------------
#ifndef ConfigurationH
#define ConfigurationH

#include "RemoteFiles.h"
#include "FileBuffer.h"
#include "HierarchicalStorage.h"
//---------------------------------------------------------------------------
#define SET_CONFIG_PROPERTY_EX(PROPERTY, APPLY) \
  if (PROPERTY != value) { F ## PROPERTY = value; Changed(); APPLY; }
#define SET_CONFIG_PROPERTY(PROPERTY) \
  SET_CONFIG_PROPERTY_EX(PROPERTY, )
//---------------------------------------------------------------------------
class TCriticalSection;
enum TAutoSwitch { asOn, asOff, asAuto };
//---------------------------------------------------------------------------
class TConfiguration : public TObject
{
private:
  bool FDontSave;
  bool FChanged;
  int FUpdating;
  TNotifyEvent FOnChange;

  void * FApplicationInfo;
  bool FLogging;
  bool FPermanentLogging;
  AnsiString FLogFileName;
  AnsiString FPermanentLogFileName;
  int FLogWindowLines;
  bool FLogFileAppend;
  int FLogProtocol;
  bool FLogActions;
  bool FPermanentLogActions;
  bool FConfirmOverwriting;
  bool FConfirmResume;
  bool FAutoReadDirectoryAfterOp;
  int FSessionReopenAuto;
  int FSessionReopenBackground;
  AnsiString FIniFileStorageName;
  int FTunnelLocalPortNumberLow;
  int FTunnelLocalPortNumberHigh;
  int FCacheDirectoryChangesMaxSize;
  bool FShowFtpWelcomeMessage;
  AnsiString FDefaultRandomSeedFile;
  AnsiString FRandomSeedFile;
  AnsiString FPuttyRegistryStorageKey;

  bool FDisablePasswordStoring;
  bool FForceBanners;
  bool FDisableAcceptingHostKeys;

  AnsiString __fastcall GetOSVersionStr();
  TVSFixedFileInfo *__fastcall GetFixedApplicationInfo();
  void * __fastcall GetApplicationInfo();
  virtual AnsiString __fastcall GetVersionStr();
  virtual AnsiString __fastcall GetVersion();
  AnsiString __fastcall GetProductVersion();
  AnsiString __fastcall GetProductName();
  AnsiString __fastcall GetCompanyName();
  AnsiString __fastcall TrimVersion(AnsiString Version);
  AnsiString __fastcall GetStoredSessionsSubKey();
  AnsiString __fastcall GetPuttySessionsKey();
  void __fastcall SetRandomSeedFile(AnsiString value);
  AnsiString __fastcall GetRandomSeedFileName();
  void __fastcall SetPuttyRegistryStorageKey(AnsiString value);
  AnsiString __fastcall GetSshHostKeysSubKey();
  AnsiString __fastcall GetRootKeyStr();
  AnsiString __fastcall GetConfigurationSubKey();
  TEOLType __fastcall GetLocalEOLType();
  void __fastcall SetLogging(bool value);
  void __fastcall SetLogFileName(AnsiString value);
  void __fastcall SetLogToFile(bool value);
  bool __fastcall GetLogToFile();
  void __fastcall SetLogWindowLines(int value);
  void __fastcall SetLogWindowComplete(bool value);
  bool __fastcall GetLogWindowComplete();
  void __fastcall SetLogFileAppend(bool value);
  void __fastcall SetLogProtocol(int value);
  void __fastcall SetLogActions(bool value);
  AnsiString __fastcall GetDefaultLogFileName();
  AnsiString __fastcall GetTimeFormat();
  void __fastcall SetStorage(TStorage value);
  AnsiString __fastcall GetRegistryStorageKey();
  AnsiString __fastcall GetIniFileStorageName();
  void __fastcall SetIniFileStorageName(AnsiString value);
  AnsiString __fastcall GetPartialExt() const;
  AnsiString __fastcall GetFileInfoString(const AnsiString Key);
  AnsiString __fastcall GetLocalInvalidChars();
  bool __fastcall GetGSSAPIInstalled();
  void __fastcall SetSessionReopenAuto(int value);
  void __fastcall SetSessionReopenBackground(int value);
  void __fastcall SetTunnelLocalPortNumberLow(int value);
  void __fastcall SetTunnelLocalPortNumberHigh(int value);
  void __fastcall SetCacheDirectoryChangesMaxSize(int value);
  void __fastcall SetShowFtpWelcomeMessage(bool value);
  int __fastcall GetCompoundVersion();

protected:
  TStorage FStorage;
  TCriticalSection * FCriticalSection;

  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Changed();
  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual void __fastcall CopyData(THierarchicalStorage * Source, THierarchicalStorage * Target);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall Saved();
  void __fastcall CleanupRegistry(AnsiString CleanupSubKey);
  AnsiString __fastcall BannerHash(const AnsiString & Banner);

  virtual bool __fastcall GetConfirmOverwriting();
  virtual void __fastcall SetConfirmOverwriting(bool value);
  bool __fastcall GetConfirmResume();
  void __fastcall SetConfirmResume(bool value);
  bool __fastcall GetAutoReadDirectoryAfterOp();
  void __fastcall SetAutoReadDirectoryAfterOp(bool value);
  virtual bool __fastcall GetRememberPassword();

  virtual AnsiString __fastcall ModuleFileName();

  AnsiString __fastcall GetFileFileInfoString(const AnsiString Key,
    const AnsiString FileName);
  void * __fastcall GetFileApplicationInfo(const AnsiString FileName);
  AnsiString __fastcall GetFileProductVersion(const AnsiString FileName);
  AnsiString __fastcall GetFileProductName(const AnsiString FileName);
  AnsiString __fastcall GetFileCompanyName(const AnsiString FileName);

  __property bool PermanentLogging  = { read=FPermanentLogging, write=SetLogging };
  __property AnsiString PermanentLogFileName  = { read=FPermanentLogFileName, write=SetLogFileName };
  __property bool PermanentLogActions  = { read=FPermanentLogActions, write=SetLogActions };

public:
  __fastcall TConfiguration();
  virtual __fastcall ~TConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall Load();
  virtual void __fastcall Save(bool All, bool Explicit);
  void __fastcall Export(const AnsiString FileName);
  void __fastcall CleanupConfiguration();
  void __fastcall CleanupIniFile();
  void __fastcall CleanupHostKeys();
  void __fastcall CleanupRandomSeedFile();
  void __fastcall BeginUpdate();
  void __fastcall EndUpdate();
  void __fastcall LoadDirectoryChangesCache(const AnsiString SessionKey,
    TRemoteDirectoryChangesCache * DirectoryChangesCache);
  void __fastcall SaveDirectoryChangesCache(const AnsiString SessionKey,
    TRemoteDirectoryChangesCache * DirectoryChangesCache);
  bool __fastcall ShowBanner(const AnsiString SessionKey, const AnsiString & Banner);
  void __fastcall NeverShowBanner(const AnsiString SessionKey, const AnsiString & Banner);
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);
  void __fastcall TemporaryLogging(const AnsiString ALogFileName);

  __property TVSFixedFileInfo *FixedApplicationInfo  = { read=GetFixedApplicationInfo };
  __property void * ApplicationInfo  = { read=GetApplicationInfo };
  __property AnsiString StoredSessionsSubKey = {read=GetStoredSessionsSubKey};
  __property AnsiString PuttyRegistryStorageKey  = { read=FPuttyRegistryStorageKey, write=SetPuttyRegistryStorageKey };
  __property AnsiString PuttySessionsKey  = { read=GetPuttySessionsKey };
  __property AnsiString RandomSeedFile  = { read=FRandomSeedFile, write=SetRandomSeedFile };
  __property AnsiString RandomSeedFileName  = { read=GetRandomSeedFileName };
  __property AnsiString SshHostKeysSubKey  = { read=GetSshHostKeysSubKey };
  __property AnsiString RootKeyStr  = { read=GetRootKeyStr };
  __property AnsiString ConfigurationSubKey  = { read=GetConfigurationSubKey };
  __property TEOLType LocalEOLType = { read = GetLocalEOLType };
  __property AnsiString VersionStr = { read=GetVersionStr };
  __property AnsiString Version = { read=GetVersion };
  __property int CompoundVersion = { read=GetCompoundVersion };
  __property AnsiString ProductVersion = { read=GetProductVersion };
  __property AnsiString ProductName = { read=GetProductName };
  __property AnsiString CompanyName = { read=GetCompanyName };
  __property AnsiString FileInfoString[AnsiString Key] = { read = GetFileInfoString };
  __property AnsiString OSVersionStr = { read = GetOSVersionStr };
  __property bool Logging  = { read=FLogging, write=SetLogging };
  __property AnsiString LogFileName  = { read=FLogFileName, write=SetLogFileName };
  __property bool LogToFile  = { read=GetLogToFile, write=SetLogToFile };
  __property bool LogFileAppend  = { read=FLogFileAppend, write=SetLogFileAppend };
  __property int LogProtocol  = { read=FLogProtocol, write=SetLogProtocol };
  __property bool LogActions  = { read=FLogActions, write=SetLogActions };
  __property int LogWindowLines  = { read=FLogWindowLines, write=SetLogWindowLines };
  __property bool LogWindowComplete  = { read=GetLogWindowComplete, write=SetLogWindowComplete };
  __property AnsiString DefaultLogFileName  = { read=GetDefaultLogFileName };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property bool ConfirmOverwriting = { read = GetConfirmOverwriting, write = SetConfirmOverwriting};
  __property bool ConfirmResume = { read = GetConfirmResume, write = SetConfirmResume};
  __property bool AutoReadDirectoryAfterOp = { read = GetAutoReadDirectoryAfterOp, write = SetAutoReadDirectoryAfterOp};
  __property bool RememberPassword = { read = GetRememberPassword };
  __property AnsiString PartialExt = {read=GetPartialExt};
  __property int SessionReopenAuto = { read = FSessionReopenAuto, write = SetSessionReopenAuto };
  __property int SessionReopenBackground = { read = FSessionReopenBackground, write = SetSessionReopenBackground };
  __property int TunnelLocalPortNumberLow = { read = FTunnelLocalPortNumberLow, write = SetTunnelLocalPortNumberLow };
  __property int TunnelLocalPortNumberHigh = { read = FTunnelLocalPortNumberHigh, write = SetTunnelLocalPortNumberHigh };
  __property int CacheDirectoryChangesMaxSize = { read = FCacheDirectoryChangesMaxSize, write = SetCacheDirectoryChangesMaxSize };
  __property bool ShowFtpWelcomeMessage = { read = FShowFtpWelcomeMessage, write = SetShowFtpWelcomeMessage };

  __property AnsiString TimeFormat = { read = GetTimeFormat };
  __property TStorage Storage  = { read=GetStorage, write=SetStorage };
  __property AnsiString RegistryStorageKey  = { read=GetRegistryStorageKey };
  __property AnsiString IniFileStorageName  = { read=GetIniFileStorageName, write=SetIniFileStorageName };
  __property AnsiString DefaultKeyFile = { read = GetDefaultKeyFile };
  __property AnsiString LocalInvalidChars = { read = GetLocalInvalidChars };

  __property bool DisablePasswordStoring = { read = FDisablePasswordStoring };
  __property bool ForceBanners = { read = FForceBanners };
  __property bool DisableAcceptingHostKeys = { read = FDisableAcceptingHostKeys };
  __property bool GSSAPIInstalled = { read = GetGSSAPIInstalled };
};
//---------------------------------------------------------------------------
#endif
