//---------------------------------------------------------------------------
#ifndef ConfigurationH
#define ConfigurationH

#include "RemoteFiles.h"
#include "FileBuffer.h"
#include "HierarchicalStorage.h"
//---------------------------------------------------------------------------
#define SET_CONFIG_PROPERTY(PROPERTY) \
  if (PROPERTY != value) { F ## PROPERTY = value; Changed(); }
//---------------------------------------------------------------------------
class TCriticalSection;
//---------------------------------------------------------------------------
class TConfiguration : public TObject
{
private:
  bool FDontSave;
  bool FChanged;
  int FUpdating;
  TNotifyEvent FOnChange;
  bool FRandomSeedSave;
  
  void * FApplicationInfo;
  bool FLogging;
  AnsiString FLogFileName;
  int FLogWindowLines;
  bool FLogFileAppend;
  int FLogProtocol;
  bool FConfirmOverwriting;
  bool FConfirmResume;
  AnsiString FIniFileStorageName;

  bool FDisablePasswordStoring;
  int FGSSAPIInstalled; 

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
  AnsiString __fastcall GetPuttyRegistryStorageKey();
  void __fastcall SetRandomSeedFile(AnsiString value);
  AnsiString __fastcall GetRandomSeedFile();
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

protected:
  TStorage FStorage;
  TCriticalSection * FCriticalSection;

  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Changed();
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall ModifyAll();
  void __fastcall CleanupRegistry(AnsiString CleanupSubKey);

  virtual bool __fastcall GetConfirmOverwriting();
  virtual void __fastcall SetConfirmOverwriting(bool value);
  bool __fastcall GetConfirmResume();
  void __fastcall SetConfirmResume(bool value);
  virtual bool __fastcall GetRememberPassword();

  virtual AnsiString __fastcall ModuleFileName();

  AnsiString __fastcall GetFileFileInfoString(const AnsiString Key,
    const AnsiString FileName);
  void * __fastcall GetFileApplicationInfo(const AnsiString FileName);
  AnsiString __fastcall GetFileProductVersion(const AnsiString FileName);
  AnsiString __fastcall GetFileProductName(const AnsiString FileName);
  AnsiString __fastcall GetFileCompanyName(const AnsiString FileName);

public:
  __fastcall TConfiguration();
  virtual __fastcall ~TConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall Load();
  virtual void __fastcall Save();
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
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);

  __property TVSFixedFileInfo *FixedApplicationInfo  = { read=GetFixedApplicationInfo };
  __property void * ApplicationInfo  = { read=GetApplicationInfo };
  __property AnsiString StoredSessionsSubKey = {read=GetStoredSessionsSubKey};
  __property AnsiString PuttyRegistryStorageKey  = { read=GetPuttyRegistryStorageKey };
  __property AnsiString PuttySessionsKey  = { read=GetPuttySessionsKey };
  __property AnsiString RandomSeedFile  = { read=GetRandomSeedFile, write=SetRandomSeedFile };
  __property AnsiString SshHostKeysSubKey  = { read=GetSshHostKeysSubKey };
  __property AnsiString RootKeyStr  = { read=GetRootKeyStr };
  __property AnsiString ConfigurationSubKey  = { read=GetConfigurationSubKey };
  __property TEOLType LocalEOLType = { read = GetLocalEOLType };
  __property AnsiString VersionStr = { read=GetVersionStr };
  __property AnsiString Version = { read=GetVersion };
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
  __property int LogWindowLines  = { read=FLogWindowLines, write=SetLogWindowLines };
  __property bool LogWindowComplete  = { read=GetLogWindowComplete, write=SetLogWindowComplete };
  __property AnsiString DefaultLogFileName  = { read=GetDefaultLogFileName };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property bool ConfirmOverwriting = { read = GetConfirmOverwriting, write = SetConfirmOverwriting};
  __property bool ConfirmResume = { read = GetConfirmResume, write = SetConfirmResume};
  __property bool RememberPassword = { read = GetRememberPassword };
  __property AnsiString PartialExt = {read=GetPartialExt};

  __property AnsiString TimeFormat = { read = GetTimeFormat };
  __property TStorage Storage  = { read=GetStorage, write=SetStorage };
  __property AnsiString RegistryStorageKey  = { read=GetRegistryStorageKey };
  __property AnsiString IniFileStorageName  = { read=GetIniFileStorageName, write=SetIniFileStorageName };
  __property AnsiString DefaultKeyFile = { read = GetDefaultKeyFile };
  __property AnsiString LocalInvalidChars = { read = GetLocalInvalidChars };

  __property bool DisablePasswordStoring = { read = FDisablePasswordStoring };
  __property bool GSSAPIInstalled = { read = GetGSSAPIInstalled };
};
//---------------------------------------------------------------------------
#endif
