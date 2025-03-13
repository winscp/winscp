//---------------------------------------------------------------------------
#ifndef ConfigurationH
#define ConfigurationH

#include <set>
#include <list>
#include "RemoteFiles.h"
#include "FileBuffer.h"
#include "HierarchicalStorage.h"
#include "Usage.h"
//---------------------------------------------------------------------------
#define SET_CONFIG_PROPERTY_EX(PROPERTY, APPLY) \
  if (PROPERTY != value) { F ## PROPERTY = value; Changed(); APPLY; }
#define SET_CONFIG_PROPERTY(PROPERTY) \
  SET_CONFIG_PROPERTY_EX(PROPERTY, )
//---------------------------------------------------------------------------
extern const wchar_t * AutoSwitchNames;
extern const wchar_t * NotAutoSwitchNames;
enum TAutoSwitch { asOn, asOff, asAuto }; // Has to match PuTTY FORCE_ON, FORCE_OFF, AUTO
//---------------------------------------------------------------------------
class TStoredSessionList;
class TCopyParamType;
//---------------------------------------------------------------------------
class TSshHostCA
{
public:
  TSshHostCA();
  void Save(THierarchicalStorage * Storage) const;
  bool Load(THierarchicalStorage * Storage);

  UnicodeString Name;
  RawByteString PublicKey;
  UnicodeString ValidityExpression;
  bool PermitRsaSha1;
  bool PermitRsaSha256;
  bool PermitRsaSha512;

  typedef std::vector<TSshHostCA> TList;
};
//---------------------------------------------------------------------------
class TSshHostCAList
{
public:
  TSshHostCAList();
  TSshHostCAList(const TSshHostCA::TList & List);
  TSshHostCAList & operator =(const TSshHostCAList & other);
  void Default();
  const TSshHostCA::TList & GetList() const;
  int GetCount() const;
  const TSshHostCA * Get(int Index) const;
  const TSshHostCA * Find(const UnicodeString & Name) const;

  void Save(THierarchicalStorage * Storage);
  void Load(THierarchicalStorage * Storage);

private:
  TSshHostCA::TList FList;
};
//---------------------------------------------------------------------------
class TConfiguration : public TObject
{
private:
  bool FDontSave;
  bool FForceSave;
  bool FChanged;
  int FUpdating;
  TNotifyEvent FOnChange;

  void * FApplicationInfo;
  TUsage * FUsage;
  bool FLogging;
  bool FPermanentLogging;
  UnicodeString FLogFileName;
  UnicodeString FPermanentLogFileName;
  bool FLogFileAppend;
  bool FLogSensitive;
  bool FPermanentLogSensitive;
  __int64 FLogMaxSize;
  __int64 FPermanentLogMaxSize;
  int FLogMaxCount;
  int FPermanentLogMaxCount;
  int FLogProtocol;
  int FPermanentLogProtocol;
  int FActualLogProtocol;
  bool FLogActions;
  bool FPermanentLogActions;
  bool FLogActionsRequired;
  UnicodeString FActionsLogFileName;
  UnicodeString FPermanentActionsLogFileName;
  bool FConfirmOverwriting;
  bool FConfirmResume;
  bool FAutoReadDirectoryAfterOp;
  int FSessionReopenAuto;
  int FSessionReopenBackground;
  int FSessionReopenTimeout;
  int FSessionReopenAutoStall;
  UnicodeString FCustomIniFileStorageName;
  UnicodeString FIniFileStorageName;
  UnicodeString FVirtualIniFileStorageName;
  std::unique_ptr<TStrings> FOptionsStorage;
  int FProgramIniPathWritable;
  int FTunnelLocalPortNumberLow;
  int FTunnelLocalPortNumberHigh;
  int FCacheDirectoryChangesMaxSize;
  bool FShowFtpWelcomeMessage;
  UnicodeString FDefaultRandomSeedFile;
  UnicodeString FRandomSeedFile;
  UnicodeString FPuttyRegistryStorageKey;
  UnicodeString FExternalIpAddress;
  int FLocalPortNumberMin;
  int FLocalPortNumberMax;
  bool FTryFtpWhenSshFails;
  int FParallelDurationThreshold;
  bool FScripting;
  UnicodeString FMimeTypes;
  int FDontReloadMoreThanSessions;
  int FScriptProgressFileNameLimit;
  int FKeyVersion;
  int FQueueTransfersLimit;
  int FParallelTransferThreshold;
  UnicodeString FCertificateStorage;
  UnicodeString FAWSAPI;
  UnicodeString FChecksumCommands;
  std::unique_ptr<TSshHostCAList> FSshHostCAList;
  std::unique_ptr<TSshHostCAList> FPuttySshHostCAList;
  bool FSshHostCAsFromPuTTY;
  int FHttpsCertificateValidation;
  UnicodeString FSynchronizationChecksumAlgs;

  bool FDisablePasswordStoring;
  bool FForceBanners;
  bool FDisableAcceptingHostKeys;
  bool FDefaultCollectUsage;

  TVSFixedFileInfo *__fastcall GetFixedApplicationInfo();
  void * __fastcall GetApplicationInfo();
  virtual UnicodeString __fastcall GetVersionStr();
  virtual UnicodeString __fastcall GetVersion();
  UnicodeString __fastcall GetProductVersion();
  UnicodeString __fastcall GetProductName();
  UnicodeString __fastcall GetCompanyName();
  UnicodeString __fastcall GetFileVersion(TVSFixedFileInfo * Info);
  UnicodeString __fastcall GetStoredSessionsSubKey();
  UnicodeString __fastcall DoGetPuttySessionsKey();
  UnicodeString __fastcall GetPuttySessionsSubKey();
  void __fastcall SetRandomSeedFile(UnicodeString value);
  UnicodeString __fastcall GetRandomSeedFileName();
  void __fastcall SetPuttyRegistryStorageKey(UnicodeString value);
  UnicodeString __fastcall GetSshHostKeysSubKey();
  UnicodeString __fastcall GetRootKeyStr();
  UnicodeString __fastcall GetConfigurationSubKey();
  TEOLType __fastcall GetLocalEOLType();
  void __fastcall SetLogging(bool value);
  bool __fastcall GetLogging();
  void __fastcall SetLogFileName(UnicodeString value);
  UnicodeString __fastcall GetLogFileName();
  bool __fastcall GetLogToFile();
  void __fastcall SetLogFileAppend(bool value);
  void __fastcall SetLogSensitive(bool value);
  void __fastcall SetLogMaxSize(__int64 value);
  __int64 __fastcall GetLogMaxSize();
  void __fastcall SetLogMaxCount(int value);
  int __fastcall GetLogMaxCount();
  void __fastcall SetLogProtocol(int value);
  void __fastcall SetLogActions(bool value);
  bool __fastcall GetLogActions();
  void __fastcall SetActionsLogFileName(UnicodeString value);
  UnicodeString __fastcall GetPermanentActionsLogFileName();
  UnicodeString __fastcall GetActionsLogFileName();
  UnicodeString __fastcall GetDefaultLogFileName();
  UnicodeString __fastcall GetTimeFormat();
  UnicodeString __fastcall GetRegistryStorageKey();
  UnicodeString __fastcall GetIniFileStorageNameForReadingWriting();
  UnicodeString __fastcall GetIniFileStorageNameForReading();
  UnicodeString __fastcall GetIniFileStorageName(bool ReadingOnly);
  void __fastcall SetOptionsStorage(TStrings * value);
  TStrings * __fastcall GetOptionsStorage();
  UnicodeString __fastcall GetFileInfoString(const UnicodeString Key);
  void __fastcall SetSessionReopenAuto(int value);
  void __fastcall SetSessionReopenBackground(int value);
  void __fastcall SetSessionReopenTimeout(int value);
  void __fastcall SetSessionReopenAutoStall(int value);
  void __fastcall SetTunnelLocalPortNumberLow(int value);
  void __fastcall SetTunnelLocalPortNumberHigh(int value);
  void __fastcall SetCacheDirectoryChangesMaxSize(int value);
  void __fastcall SetShowFtpWelcomeMessage(bool value);
  int __fastcall GetCompoundVersion();
  void __fastcall UpdateActualLogProtocol();
  void __fastcall SetExternalIpAddress(UnicodeString value);
  void __fastcall SetTryFtpWhenSshFails(bool value);
  void __fastcall SetParallelDurationThreshold(int value);
  void __fastcall SetMimeTypes(UnicodeString value);
  void SetCertificateStorage(const UnicodeString & value);
  UnicodeString GetCertificateStorageExpanded();
  void SetAWSAPI(const UnicodeString & value);
  bool __fastcall GetCollectUsage();
  void __fastcall SetCollectUsage(bool value);
  bool __fastcall GetIsUnofficial();
  bool __fastcall GetPersistent();
  void SetLocalPortNumberMin(int value);
  void SetLocalPortNumberMax(int value);
  void SetQueueTransfersLimit(int value);
  const TSshHostCAList * GetSshHostCAList();
  void SetSshHostCAList(const TSshHostCAList * value);
  const TSshHostCAList * GetPuttySshHostCAList();
  const TSshHostCAList * GetActiveSshHostCAList();

protected:
  TStorage FStorage;
  TCriticalSection * FCriticalSection;

  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Changed();
  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual void __fastcall LoadFrom(THierarchicalStorage * Storage);
  virtual void __fastcall CopyData(THierarchicalStorage * Source, THierarchicalStorage * Target);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  void LoadSshHostCAList(TSshHostCAList * SshHostCAList, THierarchicalStorage * Storage);
  virtual UnicodeString __fastcall GetDefaultKeyFile();
  virtual void __fastcall Saved();
  void __fastcall CleanupRegistry(const UnicodeString & RegistryPath);
  void __fastcall CopyAllStringsInSubKey(
    THierarchicalStorage * Source, THierarchicalStorage * Target, const UnicodeString & Name);
  bool __fastcall CopySubKey(THierarchicalStorage * Source, THierarchicalStorage * Target, const UnicodeString & Name);
  UnicodeString __fastcall BannerHash(const UnicodeString & Banner);
  void __fastcall SetBannerData(const UnicodeString & SessionKey, const UnicodeString & BannerHash, unsigned int Params);
  void __fastcall GetBannerData(const UnicodeString & SessionKey, UnicodeString & BannerHash, unsigned int & Params);
  static UnicodeString __fastcall PropertyToKey(const UnicodeString & Property);
  void DoSave(THierarchicalStorage * AStorage, bool All);
  virtual void __fastcall DoSave(bool All, bool Explicit);
  UnicodeString __fastcall FormatFingerprintKey(const UnicodeString & SiteKey, const UnicodeString & FingerprintType);
  THierarchicalStorage * OpenDirectoryStatisticsCache(bool CanCreate);
  UnicodeString __fastcall GetDirectoryStatisticsCacheKey(
    const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam);
  void SelectSessionsToImportIfAny(
    TStoredSessionList * ImportSessionList, TStoredSessionList * Sessions,
    UnicodeString & Error, const UnicodeString & NoSessionsError);

  virtual bool __fastcall GetConfirmOverwriting();
  virtual void __fastcall SetConfirmOverwriting(bool value);
  bool __fastcall GetConfirmResume();
  void __fastcall SetConfirmResume(bool value);
  bool __fastcall GetAutoReadDirectoryAfterOp();
  void __fastcall SetAutoReadDirectoryAfterOp(bool value);
  virtual bool __fastcall GetRememberPassword();
  UnicodeString __fastcall GetReleaseType();
  UnicodeString __fastcall LoadCustomIniFileStorageName();
  void __fastcall SaveCustomIniFileStorageName();
  UnicodeString __fastcall GetRegistryStorageOverrideKey();
  TStrings * GetCaches();
  UnicodeString GetFullVersion();

  virtual UnicodeString __fastcall ModuleFileName();

  UnicodeString __fastcall GetFileFileInfoString(const UnicodeString Key,
    const UnicodeString FileName, bool AllowEmpty = false);
  void * __fastcall GetFileApplicationInfo(const UnicodeString FileName);
  UnicodeString __fastcall GetFileProductVersion(const UnicodeString FileName);
  UnicodeString __fastcall GetFileProductName(const UnicodeString FileName);
  UnicodeString __fastcall GetFileCompanyName(const UnicodeString FileName);

  __property bool PermanentLogging  = { read=GetLogging, write=SetLogging };
  __property UnicodeString PermanentLogFileName  = { read=GetLogFileName, write=SetLogFileName };
  __property bool PermanentLogActions  = { read=GetLogActions, write=SetLogActions };
  __property UnicodeString PermanentActionsLogFileName  = { read=GetPermanentActionsLogFileName, write=SetActionsLogFileName };
  __property int PermanentLogProtocol  = { read=FPermanentLogProtocol, write=SetLogProtocol };
  __property bool PermanentLogSensitive  = { read=FPermanentLogSensitive, write=SetLogSensitive };
  __property __int64 PermanentLogMaxSize  = { read=GetLogMaxSize, write=SetLogMaxSize };
  __property int PermanentLogMaxCount  = { read=GetLogMaxCount, write=SetLogMaxCount };

public:
  __fastcall TConfiguration();
  virtual __fastcall ~TConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall UpdateStaticUsage();
  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save();
  void __fastcall SaveExplicit();
  void __fastcall MoveStorage(TStorage AStorage, const UnicodeString & ACustomIniFileStorageName);
  void __fastcall ScheduleCustomIniFileStorageUse(const UnicodeString & ACustomIniFileStorageName);
  void __fastcall SetExplicitIniFileStorageName(const UnicodeString & FileName);
  void __fastcall SetNulStorage();
  UnicodeString __fastcall GetAutomaticIniFileStorageName(bool ReadingOnly);
  UnicodeString __fastcall GetDefaultIniFileExportPath();
  UnicodeString __fastcall GetIniFileParamValue();
  void __fastcall Export(const UnicodeString & FileName);
  void __fastcall Import(const UnicodeString & FileName);
  void __fastcall CleanupConfiguration();
  void __fastcall CleanupIniFile();
  void __fastcall CleanupCaches();
  bool __fastcall HasAnyCache();
  void __fastcall CleanupRandomSeedFile();
  void __fastcall BeginUpdate();
  void __fastcall EndUpdate();
  void __fastcall DontSave();
  void __fastcall LoadDirectoryChangesCache(const UnicodeString SessionKey,
    TRemoteDirectoryChangesCache * DirectoryChangesCache);
  void __fastcall SaveDirectoryChangesCache(const UnicodeString SessionKey,
    TRemoteDirectoryChangesCache * DirectoryChangesCache);
  TStrings * __fastcall LoadDirectoryStatisticsCache(
    const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam);
  void __fastcall SaveDirectoryStatisticsCache(
    const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam, TStrings * DataList);
  bool __fastcall ShowBanner(const UnicodeString & SessionKey, const UnicodeString & Banner, unsigned int & Params);
  void __fastcall NeverShowBanner(const UnicodeString & SessionKey, const UnicodeString & Banner);
  void __fastcall SetBannerParams(const UnicodeString & SessionKey, unsigned int Params);
  void __fastcall RememberLastFingerprint(const UnicodeString & SiteKey, const UnicodeString & FingerprintType, const UnicodeString & Fingerprint);
  UnicodeString __fastcall LastFingerprint(const UnicodeString & SiteKey, const UnicodeString & FingerprintType);
  THierarchicalStorage * CreateConfigStorage();
  THierarchicalStorage * CreateConfigRegistryStorage();
  virtual THierarchicalStorage * CreateScpStorage(bool & SessionList);
  void __fastcall TemporaryLogging(const UnicodeString ALogFileName);
  void __fastcall TemporaryActionsLogging(const UnicodeString ALogFileName);
  void __fastcall TemporaryLogProtocol(int ALogProtocol);
  void __fastcall TemporaryLogSensitive(bool ALogSensitive);
  void __fastcall TemporaryLogMaxSize(__int64 ALogMaxSize);
  void __fastcall TemporaryLogMaxCount(int ALogMaxCount);
  virtual RawByteString __fastcall EncryptPassword(UnicodeString Password, UnicodeString Key);
  virtual UnicodeString __fastcall DecryptPassword(RawByteString Password, UnicodeString Key);
  virtual RawByteString __fastcall StronglyRecryptPassword(RawByteString Password, UnicodeString Key);
  UnicodeString __fastcall GetFileDescription(const UnicodeString & FileName);
  UnicodeString __fastcall GetFileVersion(const UnicodeString & FileName);
  UnicodeString __fastcall GetFileMimeType(const UnicodeString & FileName);
  bool RegistryPathExists(const UnicodeString & RegistryPath);
  bool HasLocalPortNumberLimits();
  virtual UnicodeString TemporaryDir(bool Mask = false) = 0;
  UnicodeString GetVersionStrHuman();

  TStoredSessionList * __fastcall SelectFilezillaSessionsForImport(
    TStoredSessionList * Sessions, UnicodeString & Error);
  bool __fastcall AnyFilezillaSessionForImport(TStoredSessionList * Sessions);
  TStoredSessionList * __fastcall SelectKnownHostsSessionsForImport(
    TStoredSessionList * Sessions, UnicodeString & Error);
  TStoredSessionList * __fastcall SelectKnownHostsSessionsForImport(
    TStrings * Lines, TStoredSessionList * Sessions, UnicodeString & Error);
  TStoredSessionList * SelectOpensshSessionsForImport(TStoredSessionList * Sessions, UnicodeString & Error);
  TStoredSessionList * SelectSessionsForImport(TStoredSessionList * Sessions, const UnicodeString & FileName, UnicodeString & Error);
  UnicodeString GetPuttySessionsKey(const UnicodeString & RootKey);
  void RefreshPuttySshHostCAList();

  __property TVSFixedFileInfo *FixedApplicationInfo  = { read=GetFixedApplicationInfo };
  __property void * ApplicationInfo  = { read=GetApplicationInfo };
  __property TUsage * Usage = { read = FUsage };
  __property bool CollectUsage = { read = GetCollectUsage, write = SetCollectUsage };
  __property UnicodeString StoredSessionsSubKey = {read=GetStoredSessionsSubKey};
  __property UnicodeString PuttyRegistryStorageKey  = { read=FPuttyRegistryStorageKey, write=SetPuttyRegistryStorageKey };
  __property UnicodeString PuttySessionsKey  = { read=DoGetPuttySessionsKey };
  __property UnicodeString PuttySessionsSubKey  = { read=GetPuttySessionsSubKey };
  __property UnicodeString RandomSeedFile  = { read=FRandomSeedFile, write=SetRandomSeedFile };
  __property UnicodeString RandomSeedFileName  = { read=GetRandomSeedFileName };
  __property UnicodeString SshHostKeysSubKey  = { read=GetSshHostKeysSubKey };
  __property UnicodeString RootKeyStr  = { read=GetRootKeyStr };
  __property UnicodeString ConfigurationSubKey  = { read=GetConfigurationSubKey };
  __property TEOLType LocalEOLType = { read = GetLocalEOLType };
  __property UnicodeString VersionStr = { read=GetVersionStr };
  __property UnicodeString Version = { read=GetVersion };
  __property int CompoundVersion = { read=GetCompoundVersion };
  __property UnicodeString ProductVersion = { read=GetProductVersion };
  __property UnicodeString ProductName = { read=GetProductName };
  __property UnicodeString CompanyName = { read=GetCompanyName };
  __property bool IsUnofficial = { read = GetIsUnofficial };
  __property bool Logging  = { read=FLogging, write=SetLogging };
  __property UnicodeString LogFileName  = { read=FLogFileName, write=SetLogFileName };
  __property bool LogToFile  = { read=GetLogToFile };
  __property bool LogFileAppend  = { read=FLogFileAppend, write=SetLogFileAppend };
  __property bool LogSensitive  = { read=FLogSensitive, write=SetLogSensitive };
  __property __int64 LogMaxSize  = { read=FLogMaxSize, write=SetLogMaxSize };
  __property int LogMaxCount  = { read=FLogMaxCount, write=SetLogMaxCount };
  __property int LogProtocol  = { read=FLogProtocol, write=SetLogProtocol };
  __property int ActualLogProtocol  = { read=FActualLogProtocol };
  __property bool LogActions  = { read=FLogActions, write=SetLogActions };
  __property bool LogActionsRequired  = { read=FLogActionsRequired, write=FLogActionsRequired };
  __property UnicodeString ActionsLogFileName  = { read=GetActionsLogFileName, write=SetActionsLogFileName };
  __property UnicodeString DefaultLogFileName  = { read=GetDefaultLogFileName };
  __property TNotifyEvent OnChange = { read = FOnChange, write = FOnChange };
  __property bool ConfirmOverwriting = { read = GetConfirmOverwriting, write = SetConfirmOverwriting};
  __property bool ConfirmResume = { read = GetConfirmResume, write = SetConfirmResume};
  __property bool AutoReadDirectoryAfterOp = { read = GetAutoReadDirectoryAfterOp, write = SetAutoReadDirectoryAfterOp};
  __property bool RememberPassword = { read = GetRememberPassword };
  __property int SessionReopenAuto = { read = FSessionReopenAuto, write = SetSessionReopenAuto };
  __property int SessionReopenBackground = { read = FSessionReopenBackground, write = SetSessionReopenBackground };
  __property int SessionReopenTimeout = { read = FSessionReopenTimeout, write = SetSessionReopenTimeout };
  __property int SessionReopenAutoStall = { read = FSessionReopenAutoStall, write = SetSessionReopenAutoStall };
  __property int TunnelLocalPortNumberLow = { read = FTunnelLocalPortNumberLow, write = SetTunnelLocalPortNumberLow };
  __property int TunnelLocalPortNumberHigh = { read = FTunnelLocalPortNumberHigh, write = SetTunnelLocalPortNumberHigh };
  __property int CacheDirectoryChangesMaxSize = { read = FCacheDirectoryChangesMaxSize, write = SetCacheDirectoryChangesMaxSize };
  __property bool ShowFtpWelcomeMessage = { read = FShowFtpWelcomeMessage, write = SetShowFtpWelcomeMessage };
  __property UnicodeString ExternalIpAddress = { read = FExternalIpAddress, write = SetExternalIpAddress };
  __property UnicodeString CertificateStorage = { read = FCertificateStorage, write = SetCertificateStorage };
  __property UnicodeString CertificateStorageExpanded = { read = GetCertificateStorageExpanded };
  __property UnicodeString AWSAPI = { read = FAWSAPI, write = SetAWSAPI };
  __property UnicodeString ChecksumCommands = { read = FChecksumCommands };
  __property int LocalPortNumberMin = { read = FLocalPortNumberMin, write = SetLocalPortNumberMin };
  __property int LocalPortNumberMax = { read = FLocalPortNumberMax, write = SetLocalPortNumberMax };
  __property bool TryFtpWhenSshFails = { read = FTryFtpWhenSshFails, write = SetTryFtpWhenSshFails };
  __property int ParallelDurationThreshold = { read = FParallelDurationThreshold, write = SetParallelDurationThreshold };
  __property UnicodeString MimeTypes = { read = FMimeTypes, write = SetMimeTypes };
  __property int DontReloadMoreThanSessions = { read = FDontReloadMoreThanSessions, write = FDontReloadMoreThanSessions };
  __property int ScriptProgressFileNameLimit = { read = FScriptProgressFileNameLimit, write = FScriptProgressFileNameLimit };
  __property int QueueTransfersLimit = { read = FQueueTransfersLimit, write = SetQueueTransfersLimit };
  __property int ParallelTransferThreshold = { read = FParallelTransferThreshold, write = FParallelTransferThreshold };
  __property int KeyVersion = { read = FKeyVersion, write = FKeyVersion };
  __property TSshHostCAList * SshHostCAList = { read = GetSshHostCAList, write = SetSshHostCAList };
  __property TSshHostCAList * PuttySshHostCAList = { read = GetPuttySshHostCAList };
  __property TSshHostCAList * ActiveSshHostCAList = { read = GetActiveSshHostCAList };
  __property bool SshHostCAsFromPuTTY = { read = FSshHostCAsFromPuTTY, write = FSshHostCAsFromPuTTY };
  __property int HttpsCertificateValidation = { read = FHttpsCertificateValidation, write = FHttpsCertificateValidation };
  __property UnicodeString SynchronizationChecksumAlgs = { read = FSynchronizationChecksumAlgs, write = FSynchronizationChecksumAlgs };

  __property UnicodeString TimeFormat = { read = GetTimeFormat };
  __property TStorage Storage  = { read=GetStorage };
  __property UnicodeString RegistryStorageKey  = { read=GetRegistryStorageKey };
  __property UnicodeString CustomIniFileStorageName  = { read=FCustomIniFileStorageName };
  __property UnicodeString ExplicitIniFileStorageName  = { read=FIniFileStorageName };
  __property UnicodeString IniFileStorageName  = { read=GetIniFileStorageNameForReadingWriting };
  __property UnicodeString IniFileStorageNameForReading  = { read=GetIniFileStorageNameForReading };
  __property TStrings * OptionsStorage = { read = GetOptionsStorage, write = SetOptionsStorage };
  __property bool Persistent = { read = GetPersistent };
  __property bool ForceSave = { read = FForceSave, write = FForceSave };
  __property bool Scripting = { read = FScripting, write = FScripting };

  __property UnicodeString DefaultKeyFile = { read = GetDefaultKeyFile };

  __property bool DisablePasswordStoring = { read = FDisablePasswordStoring };
  __property bool ForceBanners = { read = FForceBanners };
  __property bool DisableAcceptingHostKeys = { read = FDisableAcceptingHostKeys };
};
//---------------------------------------------------------------------------
class TShortCuts
{
public:
  void __fastcall Add(TShortCut ShortCut);
  bool __fastcall Has(TShortCut ShortCut) const;

private:
  std::set<TShortCut> FShortCuts;
};
//---------------------------------------------------------------------------
extern const UnicodeString OriginalPuttyRegistryStorageKey;
extern const UnicodeString KittyRegistryStorageKey;
extern const UnicodeString OriginalPuttyExecutable;
extern const UnicodeString KittyExecutable;
//---------------------------------------------------------------------------
extern const UnicodeString Sha1ChecksumAlg;
extern const UnicodeString Sha224ChecksumAlg;
extern const UnicodeString Sha256ChecksumAlg;
extern const UnicodeString Sha384ChecksumAlg;
extern const UnicodeString Sha512ChecksumAlg;
extern const UnicodeString Md5ChecksumAlg;
extern const UnicodeString Crc32ChecksumAlg;
//---------------------------------------------------------------------------
extern const UnicodeString SshFingerprintType;
extern const UnicodeString TlsFingerprintType;
//---------------------------------------------------------------------------
extern const UnicodeString FtpsCertificateStorageKey;
extern const UnicodeString HttpsCertificateStorageKey;
//---------------------------------------------------------------------------
extern const int BelowNormalLogLevels;
//---------------------------------------------------------------------------
extern const UnicodeString OpensshFolderName;
extern const UnicodeString OpensshAuthorizedKeysFileName;
//---------------------------------------------------------------------------
#endif
