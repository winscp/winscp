//---------------------------------------------------------------------------
#ifndef ConfigurationH
#define ConfigurationH

#include "CopyParam.h"
#include "FileBuffer.h"
#include "HierarchicalStorage.h"
//---------------------------------------------------------------------------
#define SET_CONFIG_PROPERTY(PROPERTY) \
  if (PROPERTY != value) { F ## PROPERTY = value; Changed(); }
//---------------------------------------------------------------------------
class TConfiguration : public TObject
{
private:
  bool FDontSave;
  bool FChanged;
  int FUpdating;
  TNotifyEvent FOnChange;
  bool FRandomSeedSave;
  
  TCopyParamType FCopyParam;
  void * FApplicationInfo;
  bool FDefaultDirIsHome;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FLogging;
  AnsiString FLogFileName;
  int FLogWindowLines;
  bool FLogFileAppend;
  bool FConfirmOverwriting;
  AnsiString FIniFileStorageName;

  TVSFixedFileInfo *__fastcall GetFixedApplicationInfo();
  void * __fastcall GetApplicationInfo();
  virtual AnsiString __fastcall GetVersionStr();
  virtual AnsiString __fastcall GetVersion();
  AnsiString __fastcall GetStoredSessionsSubKey();
  AnsiString __fastcall GetPuttySessionsKey();
  AnsiString __fastcall GetPuttyRegistryStorageKey();
  void __fastcall SetRandomSeedFile(AnsiString value);
  AnsiString __fastcall GetRandomSeedFile();
  AnsiString __fastcall GetSshHostKeysSubKey();
  AnsiString __fastcall GetRootKeyStr();
  AnsiString __fastcall GetConfigurationSubKey();
  void __fastcall CleanupRegistry(AnsiString CleanupSubKey);
  TEOLType __fastcall GetLocalEOLType();
  void __fastcall SetDefaultDirIsHome(bool value);
  void __fastcall SetCopyParam(TCopyParamType value);
  void __fastcall SetLogging(bool value);
  void __fastcall SetLogFileName(AnsiString value);
  void __fastcall SetLogToFile(bool value);
  bool __fastcall GetLogToFile();
  void __fastcall SetLogWindowLines(int value);
  void __fastcall SetLogWindowComplete(bool value);
  bool __fastcall GetLogWindowComplete();
  void __fastcall SetLogFileAppend(bool value);
  AnsiString __fastcall GetDefaultLogFileName();
  AnsiString __fastcall GetTimeFormat();
  void __fastcall SetIgnoreCancelBeforeFinish(TDateTime value);
  void __fastcall SetConfirmOverwriting(bool value);
  void __fastcall SetStorage(TStorage value);
  AnsiString __fastcall GetRegistryStorageKey();
  AnsiString __fastcall GetIniFileStorageName();
  void __fastcall SetIniFileStorageName(AnsiString value);
  AnsiString __fastcall GetPartialExt() const;
protected:
  TStorage FStorage;

  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Changed();
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall ModifyAll();

public:
  __fastcall TConfiguration();
  __fastcall ~TConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall Load();
  virtual void __fastcall Save();
  void __fastcall CleanupConfiguration();
  void __fastcall CleanupIniFile();
  void __fastcall CleanupHostKeys();
  void __fastcall CleanupRandomSeedFile();
  void __fastcall BeginUpdate();
  void __fastcall EndUpdate();
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);

  __property TVSFixedFileInfo *FixedApplicationInfo  = { read=GetFixedApplicationInfo };
  __property void * ApplicationInfo  = { read=GetApplicationInfo };
  __property bool DefaultDirIsHome = { read = FDefaultDirIsHome, write = SetDefaultDirIsHome };
  __property TCopyParamType CopyParam = { read = FCopyParam, write = SetCopyParam };
  __property AnsiString StoredSessionsSubKey = {read=GetStoredSessionsSubKey};
  __property AnsiString PuttyRegistryStorageKey  = { read=GetPuttyRegistryStorageKey };
  __property AnsiString PuttySessionsKey  = { read=GetPuttySessionsKey };
  __property AnsiString RandomSeedFile  = { read=GetRandomSeedFile, write=SetRandomSeedFile };
  __property AnsiString SshHostKeysSubKey  = { read=GetSshHostKeysSubKey };
  __property AnsiString RootKeyStr  = { read=GetRootKeyStr };
  __property AnsiString ConfigurationSubKey  = { read=GetConfigurationSubKey };
  __property bool DontSave  = { read=FDontSave, write=FDontSave };
  __property bool RandomSeedSave  = { read=FRandomSeedSave, write=FRandomSeedSave };
  __property TEOLType LocalEOLType = { read = GetLocalEOLType };
  __property AnsiString VersionStr  = { read=GetVersionStr };
  __property AnsiString Version  = { read=GetVersion };
  __property bool Logging  = { read=FLogging, write=SetLogging };
  __property AnsiString LogFileName  = { read=FLogFileName, write=SetLogFileName };
  __property bool LogToFile  = { read=GetLogToFile, write=SetLogToFile };
  __property bool LogFileAppend  = { read=FLogFileAppend, write=SetLogFileAppend };
  __property int LogWindowLines  = { read=FLogWindowLines, write=SetLogWindowLines };
  __property bool LogWindowComplete  = { read=GetLogWindowComplete, write=SetLogWindowComplete };
  __property AnsiString DefaultLogFileName  = { read=GetDefaultLogFileName };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = SetIgnoreCancelBeforeFinish };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property bool ConfirmOverwriting = { read = FConfirmOverwriting, write = SetConfirmOverwriting};
  __property AnsiString PartialExt = {read=GetPartialExt};

  __property AnsiString TimeFormat = { read = GetTimeFormat };
  __property TStorage Storage  = { read=GetStorage, write=SetStorage };
  __property AnsiString RegistryStorageKey  = { read=GetRegistryStorageKey };
  __property AnsiString IniFileStorageName  = { read=GetIniFileStorageName, write=SetIniFileStorageName };
  __property AnsiString DefaultKeyFile = { read = GetDefaultKeyFile };
};
//---------------------------------------------------------------------------
bool SpecialFolderLocation(int PathID, AnsiString & Path);
//---------------------------------------------------------------------------
#endif
