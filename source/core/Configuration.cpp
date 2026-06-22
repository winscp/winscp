//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <FileInfo.h>

#include "Common.h"
#include "Exceptions.h"
#include "Configuration.h"
#include "PuttyIntf.h"
#include "TextsCore.h"
#include "Interface.h"
#include "CoreMain.h"
#include "Security.h"
#include "FileMasks.h"
#include "CopyParam.h"
#include <shlobj.h>
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.DateUtils.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const wchar_t * AutoSwitchNames = L"On;Off;Auto";
const wchar_t * NotAutoSwitchNames = L"Off;On;Auto";
//---------------------------------------------------------------------------
// See https://www.iana.org/assignments/hash-function-text-names/hash-function-text-names.xhtml
const UnicodeString Sha1ChecksumAlg(L"sha-1");
const UnicodeString Sha224ChecksumAlg(L"sha-224");
const UnicodeString Sha256ChecksumAlg(L"sha-256");
const UnicodeString Sha384ChecksumAlg(L"sha-384");
const UnicodeString Sha512ChecksumAlg(L"sha-512");
const UnicodeString Md5ChecksumAlg(L"md5");
// Not defined by IANA
const UnicodeString Crc32ChecksumAlg(L"crc32");
//---------------------------------------------------------------------------
const UnicodeString SshFingerprintType(L"ssh");
const UnicodeString TlsFingerprintType(L"tls");
//---------------------------------------------------------------------------
const UnicodeString FtpsCertificateStorageKey(L"FtpsCertificates");
const UnicodeString HttpsCertificateStorageKey(L"HttpsCertificates");
const UnicodeString LastFingerprintsStorageKey(L"LastFingerprints");
const UnicodeString DirectoryStatisticsCacheKey(L"DirectoryStatisticsCache");
const UnicodeString SshHostCAsKey(L"SshHostCAs");
const UnicodeString CDCacheKey(L"CDCache");
const UnicodeString BannersKey(L"Banners");
//---------------------------------------------------------------------------
const UnicodeString OpensshFolderName(L".ssh");
const UnicodeString OpensshAuthorizedKeysFileName(L"authorized_keys");
//---------------------------------------------------------------------------
const int BelowNormalLogLevels = 1;
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSshHostCA::TSshHostCA()
{
  PermitRsaSha1 = false;
  PermitRsaSha256 = true;
  PermitRsaSha512 = true;
}
//---------------------------------------------------------------------------
bool TSshHostCA::Load(THierarchicalStorage * Storage)
{
  PublicKey = DecodeBase64ToStr(Storage->ReadString(L"PublicKey", PublicKey));
  ValidityExpression = Storage->ReadString(L"Validity", ValidityExpression);
  PermitRsaSha1 = Storage->ReadBool(L"PermitRSASHA1", PermitRsaSha1);
  PermitRsaSha256 = Storage->ReadBool(L"PermitRSASHA256", PermitRsaSha256);
  PermitRsaSha512 = Storage->ReadBool(L"PermitRSASHA512", PermitRsaSha512);
  return !PublicKey.IsEmpty() && !ValidityExpression.IsEmpty();
}
//---------------------------------------------------------------------------
void TSshHostCA::Save(THierarchicalStorage * Storage) const
{
  Storage->WriteString(L"PublicKey", EncodeStrToBase64(PublicKey));
  Storage->WriteString(L"Validity", ValidityExpression);
  Storage->WriteBool(L"PermitRSASHA1", PermitRsaSha1);
  Storage->WriteBool(L"PermitRSASHA256", PermitRsaSha256);
  Storage->WriteBool(L"PermitRSASHA512", PermitRsaSha512);
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
TSshHostCAList::TSshHostCAList()
{
}
//---------------------------------------------------------------------------
TSshHostCAList::TSshHostCAList(const TSshHostCA::TList & List)
{
  FList = List;
}
//---------------------------------------------------------------------------
void TSshHostCAList::Default()
{
  FList.clear();
}
//---------------------------------------------------------------------------
void TSshHostCAList::Save(THierarchicalStorage * Storage)
{
  Storage->ClearSubKeys();
  TSshHostCA::TList::const_iterator I = FList.begin();
  while (I != FList.end())
  {
    const TSshHostCA & SshHostCA = *I;
    if (Storage->OpenSubKey(SshHostCA.Name, true))
    {
      SshHostCA.Save(Storage);
      Storage->CloseSubKey();
    }
    ++I;
  }
}
//---------------------------------------------------------------------------
void TSshHostCAList::Load(THierarchicalStorage * Storage)
{
  FList.clear();
  std::unique_ptr<TStrings> SubKeys(new TStringList());
  Storage->GetSubKeyNames(SubKeys.get());

  for (int Index = 0; Index < SubKeys->Count; Index++)
  {
    TSshHostCA SshHostCA;
    SshHostCA.Name = SubKeys->Strings[Index];
    if (Storage->OpenSubKey(SshHostCA.Name, false))
    {
      if (SshHostCA.Load(Storage))
      {
        FList.push_back(SshHostCA);
      }

      Storage->CloseSubKey();
    }
  }
}
//---------------------------------------------------------------------------
int TSshHostCAList::GetCount() const
{
  return FList.size();
}
//---------------------------------------------------------------------------
const TSshHostCA * TSshHostCAList::Get(int Index) const
{
  return &FList[Index];
}
//---------------------------------------------------------------------------
const TSshHostCA * TSshHostCAList::Find(const UnicodeString & Name) const
{
  TSshHostCA::TList::const_iterator I = FList.begin();
  while (I != FList.end())
  {
    const TSshHostCA & SshHostCA = *I;
    if (SameStr(SshHostCA.Name, Name))
    {
      return &SshHostCA;
    }
    ++I;
  }
  return NULL;
}
//---------------------------------------------------------------------------
const TSshHostCA::TList & TSshHostCAList::GetList() const
{
  return FList;
}
//---------------------------------------------------------------------------
TSshHostCAList & TSshHostCAList::operator =(const TSshHostCAList & other)
{
  FList = other.FList;
  return *this;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
__fastcall TConfiguration::TConfiguration()
{
  FCriticalSection = new TCriticalSection();
  FUpdating = 0;
  FStorage = stDetect;
  FDontSave = false;
  FForceSave = false;
  FApplicationInfo = NULL;
  FUsage = new TUsage(this);
  FDefaultCollectUsage = IsUWP();
  FScripting = false;
  FSshHostCAList.reset(new TSshHostCAList());

  UnicodeString RandomSeedPath;
  if (!GetEnvironmentVariable(L"APPDATA").IsEmpty())
  {
    RandomSeedPath = L"%APPDATA%";
  }
  else
  {
    RandomSeedPath = GetShellFolderPath(CSIDL_LOCAL_APPDATA);
    if (RandomSeedPath.IsEmpty())
    {
      RandomSeedPath = GetShellFolderPath(CSIDL_APPDATA);
    }
  }

  FDefaultRandomSeedFile = IncludeTrailingBackslash(RandomSeedPath) + L"winscp.rnd";
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Default()
{
  TGuard Guard(FCriticalSection);

  FDisablePasswordStoring = false;
  FForceBanners = false;
  FDisableAcceptingHostKeys = false;

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

  RandomSeedFile = FDefaultRandomSeedFile;
  PuttyRegistryStorageKey = OriginalPuttyRegistryStorageKey;
  FConfirmOverwriting = true;
  FConfirmResume = true;
  FAutoReadDirectoryAfterOp = true;
  FSessionReopenAuto = 5000;
  FSessionReopenBackground = 2000;
  FSessionReopenTimeout = 0;
  FSessionReopenAutoStall = 60000;
  FTunnelLocalPortNumberLow = 50000;
  FTunnelLocalPortNumberHigh = 50099;
  FCacheDirectoryChangesMaxSize = 100;
  FShowFtpWelcomeMessage = false;
  FExternalIpAddress = L"";
  FLocalPortNumberMin = 0;
  FLocalPortNumberMax = 0;
  FTryFtpWhenSshFails = true;
  FParallelDurationThreshold = 10;
  FMimeTypes = UnicodeString();
  FCertificateStorage = EmptyStr;
  FAWSAPI = EmptyStr;
  FChecksumCommands = EmptyStr;
  FDontReloadMoreThanSessions = 1000;
  FScriptProgressFileNameLimit = 25;
  FQueueTransfersLimit = 2;
  FParallelTransferThreshold = -1; // default (currently off), 0 = explicitly off
  FKeyVersion = 0;
  FSshHostCAList->Default();
  RefreshPuttySshHostCAList();
  FSshHostCAsFromPuTTY = false;
  FHttpsCertificateValidation = 0;
  FSynchronizationChecksumAlgs = EmptyStr;
  CollectUsage = FDefaultCollectUsage;

  FLogging = false;
  FPermanentLogging = false;
  FLogFileName = DefaultLogFileName;
  FPermanentLogFileName = FLogFileName;
  FLogFileAppend = true;
  FLogSensitive = false;
  FPermanentLogSensitive = FLogSensitive;
  FLogMaxSize = 0;
  FPermanentLogMaxSize = FLogMaxSize;
  FLogMaxCount = 0;
  FPermanentLogMaxCount = FLogMaxCount;
  FLogProtocol = 0;
  FPermanentLogProtocol = FLogProtocol;
  UpdateActualLogProtocol();
  FLogActions = false;
  FPermanentLogActions = false;
  FLogActionsRequired = false;
  FActionsLogFileName = L"%TEMP%\\!S.xml";
  FPermanentActionsLogFileName = FActionsLogFileName;
  FProgramIniPathWritable = -1;
  FCustomIniFileStorageName = LoadCustomIniFileStorageName();

  Changed();
}
//---------------------------------------------------------------------------
__fastcall TConfiguration::~TConfiguration()
{
  DebugAssert(!FUpdating);
  if (FApplicationInfo) FreeFileInfo(FApplicationInfo);
  delete FCriticalSection;
  delete FUsage;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::UpdateStaticUsage()
{
  Usage->Set(L"ConfigurationIniFile", (Storage == stIniFile));
  Usage->Set(L"ConfigurationIniFileCustom", !CustomIniFileStorageName.IsEmpty());
  Usage->Set("Unofficial", IsUnofficial);

  // this is called from here, because we are guarded from calling into
  // master password handler here, see TWinConfiguration::UpdateStaticUsage
  StoredSessions->UpdateStaticUsage();
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::CreateConfigStorage()
{
  bool SessionList = false;
  return CreateScpStorage(SessionList);
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::CreateConfigRegistryStorage()
{
  return new TRegistryStorage(RegistryStorageKey);
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::CreateScpStorage(bool & SessionList)
{
  TGuard Guard(FCriticalSection);
  THierarchicalStorage * Result;
  if (Storage == stRegistry)
  {
    Result = CreateConfigRegistryStorage();
  }
  else if (Storage == stNul)
  {
    Result = TIniFileStorage::CreateNul();
  }
  else
  {
    UnicodeString StorageName = IniFileStorageName;
    Result = TIniFileStorage::CreateFromPath(StorageName);
  }

  if ((FOptionsStorage.get() != NULL) && (FOptionsStorage->Count > 0))
  {
    if (!SessionList)
    {
      Result = new TOptionsStorage(FOptionsStorage.get(), ConfigurationSubKey, Result);
    }
    else
    {
      // cannot reuse session list storage for configuration as for it we need
      // the option-override storage above
    }
  }
  else
  {
    // All the above stores can be reused for configuration,
    // if no options-overrides are set
    SessionList = false;
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::PropertyToKey(const UnicodeString & Property)
{
  // no longer useful
  int P = Property.LastDelimiter(L".>");
  UnicodeString Result = Property.SubString(P + 1, Property.Length() - P);
  if ((Result[1] == L'F') && (towupper(Result[2]) == Result[2]))
  {
    Result.Delete(1, 1);
  }
  return Result;
}
//---------------------------------------------------------------------------
#define BLOCK(KEY, CANCREATE, BLOCK) \
  if (Storage->OpenSubKeyPath(KEY, CANCREATE)) try { BLOCK } __finally { Storage->CloseSubKeyPath(); }
#define KEY(TYPE, VAR) KEYEX(TYPE, VAR, PropertyToKey(TEXT(#VAR)))
#define REGCONFIG(CANCREATE) \
  BLOCK(L"Interface", CANCREATE, \
    KEY(String,   RandomSeedFile); \
    KEY(String,   PuttyRegistryStorageKey); \
    KEY(Bool,     ConfirmOverwriting); \
    KEY(Bool,     ConfirmResume); \
    KEY(Bool,     AutoReadDirectoryAfterOp); \
    KEY(Integer,  SessionReopenAuto); \
    KEY(Integer,  SessionReopenBackground); \
    KEY(Integer,  SessionReopenTimeout); \
    KEY(Integer,  SessionReopenAutoStall); \
    KEY(Integer,  TunnelLocalPortNumberLow); \
    KEY(Integer,  TunnelLocalPortNumberHigh); \
    KEY(Integer,  CacheDirectoryChangesMaxSize); \
    KEY(Bool,     ShowFtpWelcomeMessage); \
    KEY(String,   ExternalIpAddress); \
    KEY(Integer,  LocalPortNumberMin); \
    KEY(Integer,  LocalPortNumberMax); \
    KEY(Bool,     TryFtpWhenSshFails); \
    KEY(Integer,  ParallelDurationThreshold); \
    KEY(String,   MimeTypes); \
    KEY(Integer,  DontReloadMoreThanSessions); \
    KEY(Integer,  ScriptProgressFileNameLimit); \
    KEY(Integer,  QueueTransfersLimit); \
    KEY(Integer,  ParallelTransferThreshold); \
    KEY(Integer,  KeyVersion); \
    KEY(Bool,     SshHostCAsFromPuTTY); \
    KEY(Integer,  HttpsCertificateValidation); \
    KEY(String,   SynchronizationChecksumAlgs); \
    KEY(Bool,     CollectUsage); \
    KEY(String,   CertificateStorage); \
    KEY(String,   AWSAPI); \
  ); \
  BLOCK(L"Logging", CANCREATE, \
    KEYEX(Bool,  PermanentLogging, L"Logging"); \
    KEYEX(String,PermanentLogFileName, L"LogFileName"); \
    KEY(Bool,    LogFileAppend); \
    KEYEX(Bool,  PermanentLogSensitive, L"LogSensitive"); \
    KEYEX(Int64, PermanentLogMaxSize, L"LogMaxSize"); \
    KEYEX(Integer, PermanentLogMaxCount, L"LogMaxCount"); \
    KEYEX(Integer,PermanentLogProtocol, L"LogProtocol"); \
    KEYEX(Bool,  PermanentLogActions, L"LogActions"); \
    KEYEX(String,PermanentActionsLogFileName, L"ActionsLogFileName"); \
  );
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveData(THierarchicalStorage * Storage, bool /*All*/)
{
  #define KEYEX(TYPE, VAR, NAME) Storage->Write ## TYPE(NAME, VAR)
  REGCONFIG(true);
  #undef KEYEX

  if (Storage->OpenSubKey(L"Usage", true))
  {
    FUsage->Save(Storage);
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Save()
{
  // only modified, implicit
  DoSave(false, false);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveExplicit()
{
  // only modified, explicit
  DoSave(false, true);
}
//---------------------------------------------------------------------------
void TConfiguration::DoSave(THierarchicalStorage * AStorage, bool All)
{
  if (AStorage->OpenSubKey(ConfigurationSubKey, true))
  {
    SaveData(AStorage, All);
    AStorage->CloseSubKey();
  }

  if (AStorage->OpenSubKey(SshHostCAsKey, true))
  {
    FSshHostCAList->Save(AStorage);
    AStorage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::DoSave(bool All, bool Explicit)
{
  if (FDontSave) return;

  THierarchicalStorage * AStorage = CreateConfigStorage();
  try
  {
    AStorage->AccessMode = smReadWrite;
    AStorage->Explicit = Explicit;
    AStorage->ForceSave = FForceSave;
    // if saving to TOptionsStorage, make sure we save everything so that
    // all configuration is properly transferred to the master storage
    bool ConfigAll = All || AStorage->Temporary;
    DoSave(AStorage, ConfigAll);
  }
  __finally
  {
    delete AStorage;
  }

  Saved();

  if (All)
  {
    StoredSessions->Save(true, Explicit);
  }

  // clean up as last, so that if it fails (read only INI), the saving can proceed
  if (Storage == stRegistry)
  {
    CleanupIniFile();
  }

  SaveCustomIniFileStorageName();

}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveCustomIniFileStorageName()
{
  // Particularly, not to create an empty "Override" key, unless the custom INI file is ever set
  if (CustomIniFileStorageName != LoadCustomIniFileStorageName())
  {
    std::unique_ptr<TRegistryStorage> RegistryStorage(new TRegistryStorage(GetRegistryStorageOverrideKey()));
    RegistryStorage->AccessMode = smReadWrite;
    RegistryStorage->Explicit = true;
    if (RegistryStorage->OpenRootKey(true))
    {
      RegistryStorage->WriteString(L"IniFile", CustomIniFileStorageName);
      RegistryStorage->CloseSubKey();
      AppLogFmt(L"Saved custom INI file path %s", (CustomIniFileStorageName));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Export(const UnicodeString & FileName)
{
  // not to "append" the export to an existing file
  if (FileExists(FileName))
  {
    DeleteFileChecked(FileName);
  }

  THierarchicalStorage * Storage = NULL;
  THierarchicalStorage * ExportStorage = NULL;
  try
  {
    ExportStorage = TIniFileStorage::CreateFromPath(FileName);
    ExportStorage->AccessMode = smReadWrite;
    ExportStorage->Explicit = true;

    Storage = CreateConfigStorage();
    Storage->AccessMode = smRead;

    CopyData(Storage, ExportStorage);

    DoSave(ExportStorage, true);
  }
  __finally
  {
    delete ExportStorage;
    delete Storage;
  }

  StoredSessions->Export(FileName);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Import(const UnicodeString & FileName)
{
  THierarchicalStorage * Storage = NULL;
  THierarchicalStorage * ImportStorage = NULL;
  try
  {
    ImportStorage = TIniFileStorage::CreateFromPath(FileName);
    ImportStorage->AccessMode = smRead;

    Storage = CreateConfigStorage();
    Storage->AccessMode = smReadWrite;
    Storage->Explicit = true;

    CopyData(ImportStorage, Storage);

    Default();
    LoadFrom(ImportStorage);

    Storage->RecursiveDeleteSubKey(Configuration->StoredSessionsSubKey);

    if (ImportStorage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
    {
      StoredSessions->Clear();
      StoredSessions->DefaultSettings->Default();
      StoredSessions->Load(ImportStorage);
    }
  }
  __finally
  {
    delete ImportStorage;
    delete Storage;
  }

  // save all and explicit
  DoSave(true, true);

  FDontSave = true;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadData(THierarchicalStorage * Storage)
{
  #define KEYEX(TYPE, VAR, NAME) VAR = Storage->Read ## TYPE(NAME, VAR)
  #pragma warn -eas
  REGCONFIG(false);
  #pragma warn +eas
  #undef KEYEX

  if (Storage->OpenSubKey(L"Usage", false))
  {
    FUsage->Load(Storage);
    Storage->CloseSubKey();
  }

  if (FPermanentLogActions && FPermanentActionsLogFileName.IsEmpty() &&
      FPermanentLogging && !FPermanentLogFileName.IsEmpty())
  {
     FPermanentActionsLogFileName = FPermanentLogFileName;
     FPermanentLogging = false;
     FPermanentLogFileName = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadAdmin(THierarchicalStorage * Storage)
{
  FDisablePasswordStoring = Storage->ReadBool(L"DisablePasswordStoring", FDisablePasswordStoring);
  FForceBanners = Storage->ReadBool(L"ForceBanners", FForceBanners);
  FDisableAcceptingHostKeys = Storage->ReadBool(L"DisableAcceptingHostKeys", FDisableAcceptingHostKeys);
  FDefaultCollectUsage = Storage->ReadBool(L"DefaultCollectUsage", FDefaultCollectUsage);
}
//---------------------------------------------------------------------------
void TConfiguration::LoadSshHostCAList(TSshHostCAList * SshHostCAList, THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(SshHostCAsKey, false))
  {
    SshHostCAList->Load(Storage);
    Storage->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadFrom(THierarchicalStorage * Storage)
{
  if (Storage->OpenSubKey(ConfigurationSubKey, false))
  {
    LoadData(Storage);
    Storage->CloseSubKey();
  }
  LoadSshHostCAList(FSshHostCAList.get(), Storage);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetRegistryStorageOverrideKey()
{
  return GetRegistryStorageKey() + L" Override";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::LoadCustomIniFileStorageName()
{
  UnicodeString Result;
  std::unique_ptr<TRegistryStorage> RegistryStorage(new TRegistryStorage(GetRegistryStorageOverrideKey()));
  if (RegistryStorage->OpenRootKey(false))
  {
    Result = RegistryStorage->ReadString(L"IniFile", L"");
    AppLogFmt(L"Loaded custom INI file path %s", (Result));
    RegistryStorage->CloseSubKey();
  }
  RegistryStorage.reset(NULL);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Load(THierarchicalStorage * Storage)
{
  TGuard Guard(FCriticalSection);
  TStorageAccessMode StorageAccessMode = Storage->AccessMode;
  try
  {
    Storage->AccessMode = smRead;
    LoadFrom(Storage);
  }
  __finally
  {
    Storage->AccessMode = StorageAccessMode;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::CopySubKey(THierarchicalStorage * Source, THierarchicalStorage * Target, const UnicodeString & Name)
{
  bool Result = Source->OpenSubKey(Name, false);
  if (Result)
  {
    Result = Target->OpenSubKey(Name, true);
    if (!Result)
    {
      Source->CloseSubKey();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CopyAllStringsInSubKey(
  THierarchicalStorage * Source, THierarchicalStorage * Target, const UnicodeString & Name)
{
  if (CopySubKey(Source, Target, Name))
  {
    std::unique_ptr<TStrings> Names(new TStringList());
    Source->GetValueNames(Names.get());

    for (int Index = 0; Index < Names->Count; Index++)
    {
      UnicodeString Buf = Source->ReadStringRaw(Names->Strings[Index], UnicodeString());
      Target->WriteStringRaw(Names->Strings[Index], Buf);
    }

    Target->CloseSubKey();
    Source->CloseSubKey();
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CopyData(THierarchicalStorage * Source,
  THierarchicalStorage * Target)
{
  if (CopySubKey(Source, Target, ConfigurationSubKey))
  {
    if (CopySubKey(Source, Target, CDCacheKey))
    {
      std::unique_ptr<TStrings> Names(new TStringList());
      Source->GetValueNames(Names.get());

      for (int Index = 0; Index < Names->Count; Index++)
      {
        Target->WriteBinaryData(Names->Strings[Index], Source->ReadBinaryData(Names->Strings[Index]));
      }

      Target->CloseSubKey();
      Source->CloseSubKey();
    }

    CopyAllStringsInSubKey(Source, Target, BannersKey);
    CopyAllStringsInSubKey(Source, Target, LastFingerprintsStorageKey);

    Target->CloseSubKey();
    Source->CloseSubKey();
  }

  CopyAllStringsInSubKey(Source, Target, SshHostKeysSubKey);
  CopyAllStringsInSubKey(Source, Target, SshHostCAsKey);
  CopyAllStringsInSubKey(Source, Target, FtpsCertificateStorageKey);
  CopyAllStringsInSubKey(Source, Target, HttpsCertificateStorageKey);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::LoadDirectoryChangesCache(const UnicodeString SessionKey,
  TRemoteDirectoryChangesCache * DirectoryChangesCache)
{
  THierarchicalStorage * Storage = CreateConfigStorage();
  try
  {
    Storage->AccessMode = smRead;
    if (Storage->OpenSubKey(ConfigurationSubKey, false) &&
        Storage->OpenSubKey(CDCacheKey, false) &&
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
void __fastcall TConfiguration::SaveDirectoryChangesCache(const UnicodeString SessionKey,
  TRemoteDirectoryChangesCache * DirectoryChangesCache)
{
  THierarchicalStorage * Storage = CreateConfigStorage();
  try
  {
    Storage->AccessMode = smReadWrite;
    if (Storage->OpenSubKey(ConfigurationSubKey, true) &&
        Storage->OpenSubKey(CDCacheKey, true))
    {
      UnicodeString Data;
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
UnicodeString __fastcall TConfiguration::BannerHash(const UnicodeString & Banner)
{
  RawByteString Result;
  Result.SetLength(16);
  md5checksum(
    reinterpret_cast<const char*>(Banner.c_str()), Banner.Length() * sizeof(wchar_t),
    (unsigned char*)Result.c_str());
  return BytesToHex(Result);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::GetBannerData(
  const UnicodeString & SessionKey, UnicodeString & BannerHash, unsigned int & Params)
{
  BannerHash = UnicodeString();
  Params = 0;

  std::unique_ptr<THierarchicalStorage> Storage(CreateConfigStorage());
  Storage->AccessMode = smRead;
  if (Storage->OpenSubKey(ConfigurationSubKey, false) &&
      Storage->OpenSubKey(BannersKey, false))
  {
    UnicodeString S = Storage->ReadString(SessionKey, L"");
    BannerHash = CutToChar(S, L',', true);
    Params = StrToIntDef(L"$" + CutToChar(S, L',', true), 0);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::ShowBanner(
  const UnicodeString & SessionKey, const UnicodeString & Banner, unsigned int & Params)
{
  UnicodeString StoredBannerHash;
  GetBannerData(SessionKey, StoredBannerHash, Params);
  bool Result = (StoredBannerHash != BannerHash(Banner));
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetBannerData(
  const UnicodeString & SessionKey, const UnicodeString & BannerHash, unsigned int Params)
{
  std::unique_ptr<THierarchicalStorage> Storage(CreateConfigStorage());
  Storage->AccessMode = smReadWrite;

  if (Storage->OpenSubKey(ConfigurationSubKey, true) &&
      Storage->OpenSubKey(BannersKey, true))
  {
    Storage->WriteString(SessionKey, BannerHash + L"," + UIntToStr(Params));
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::NeverShowBanner(const UnicodeString & SessionKey, const UnicodeString & Banner)
{
  UnicodeString DummyBannerHash;
  unsigned int Params;
  GetBannerData(SessionKey, DummyBannerHash, Params);
  SetBannerData(SessionKey, BannerHash(Banner), Params);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetBannerParams(const UnicodeString & SessionKey, unsigned int Params)
{
  UnicodeString BannerHash;
  unsigned int DummyParams;
  GetBannerData(SessionKey, BannerHash, DummyParams);
  SetBannerData(SessionKey, BannerHash, Params);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::FormatFingerprintKey(const UnicodeString & SiteKey, const UnicodeString & FingerprintType)
{
  return FORMAT(L"%s:%s", (SiteKey, FingerprintType));
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::RememberLastFingerprint(const UnicodeString & SiteKey, const UnicodeString & FingerprintType, const UnicodeString & Fingerprint)
{
  std::unique_ptr<THierarchicalStorage> Storage(CreateConfigStorage());
  Storage->AccessMode = smReadWrite;

  if (Storage->OpenSubKey(ConfigurationSubKey, true) &&
      Storage->OpenSubKey(LastFingerprintsStorageKey, true))
  {
    UnicodeString FingerprintKey = FormatFingerprintKey(SiteKey, FingerprintType);
    Storage->WriteString(FingerprintKey, Fingerprint);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::LastFingerprint(const UnicodeString & SiteKey, const UnicodeString & FingerprintType)
{
  UnicodeString Result;

  std::unique_ptr<THierarchicalStorage> Storage(CreateConfigStorage());
  Storage->AccessMode = smRead;

  if (Storage->OpenSubKey(ConfigurationSubKey, false) &&
      Storage->OpenSubKey(LastFingerprintsStorageKey, false))
  {
    UnicodeString FingerprintKey = FormatFingerprintKey(SiteKey, FingerprintType);
    Result = Storage->ReadString(FingerprintKey, L"");
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Changed()
{
  TNotifyEvent AOnChange = NULL;

  {
    TGuard Guard(FCriticalSection);
    if (FUpdating == 0)
    {
      AOnChange = OnChange;
    }
    else
    {
      FChanged = true;
    }
  }

  // No specific reason to call this outside of a guard, just that it is less of a change to a previous unguarded code
  if (AOnChange != NULL)
  {
    AOnChange(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::BeginUpdate()
{
  FCriticalSection->Enter();
  if (FUpdating == 0)
  {
    FChanged = false;
  }
  FUpdating++;
  // Greater value would probably indicate some nesting problem in code
  DebugAssert(FUpdating < 6);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::EndUpdate()
{
  DebugAssert(FUpdating > 0);
  FUpdating--;
  if ((FUpdating == 0) && FChanged)
  {
    FChanged = false;
    Changed();
  }
  FCriticalSection->Leave();
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
    throw ExtException(&E, LoadStr(CLEANUP_CONFIG_ERROR));
  }
}
//---------------------------------------------------------------------------
bool TConfiguration::RegistryPathExists(const UnicodeString & RegistryPath)
{
  std::unique_ptr<TRegistryStorage> Registry(new TRegistryStorage(RegistryStorageKey));

  UnicodeString ParentKey = ExtractFileDir(RegistryPath);
  UnicodeString SubKey = ExtractFileName(RegistryPath);
  return
    Registry->OpenRootKey(false) &&
    (ParentKey.IsEmpty() ||
     Registry->OpenSubKeyPath(ParentKey, false)) &&
    Registry->KeyExists(SubKey);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRegistry(const UnicodeString & RegistryPath)
{
  UnicodeString CompanyKey = GetCompanyRegistryKey();
  UnicodeString Prefix = IncludeTrailingBackslash(CompanyKey);
  if (DebugAlwaysTrue(SameStr(LeftStr(RegistryStorageKey, Prefix.Length()), Prefix)))
  {
    UnicodeString CompanyParentKey = ExtractFileDir(CompanyKey);
    std::unique_ptr<TRegistryStorage> Registry(new TRegistryStorage(CompanyParentKey));
    UnicodeString RegistryStorageSubKey = MidStr(RegistryStorageKey, CompanyParentKey.Length() + 2);
    Registry->UnmungedRoot = RegistryStorageSubKey;

    AppLogFmt(L"Cleaning up registry key %s", (RegistryPath));
    UnicodeString ARegistryPath = CombinePaths(RegistryStorageSubKey, RegistryPath);
    UnicodeString Buf = ARegistryPath;
    while (!Buf.IsEmpty())
    {
      UnicodeString ParentKey = ExtractFileDir(Buf);
      // Actually can be simplified to Registry->OpenSubKeyPath(ParentKey, false)
      if (ParentKey.IsEmpty() ? Registry->OpenRootKey(false) : Registry->OpenSubKeyPath(ParentKey, false))
      {
        UnicodeString SubKey = ExtractFileName(Buf);
        if (Registry->KeyExists(SubKey))
        {
          bool Recursive = (Buf == ARegistryPath);
          if (Registry->DeleteSubKey(SubKey, Recursive))
          {
            AppLogFmt(L"Deleted registry key %s in %s", (SubKey, ParentKey));
          }
          else
          {
            break;
          }
        }
        Registry->CloseSubKeyPath();
        Buf = ParentKey;
      }
      else
      {
        break;
      }
    }
  }
}
//---------------------------------------------------------------------------
TStrings * TConfiguration::GetCaches()
{
  std::unique_ptr<TStrings> Result(new TStringList());
  Result->Add(SshHostKeysSubKey);
  Result->Add(SshHostCAsKey);
  Result->Add(FtpsCertificateStorageKey);
  Result->Add(HttpsCertificateStorageKey);
  Result->Add(DirectoryStatisticsCacheKey);
  Result->Add(CombinePaths(ConfigurationSubKey, CDCacheKey));
  Result->Add(CombinePaths(ConfigurationSubKey, BannersKey));
  Result->Add(CombinePaths(ConfigurationSubKey, LastFingerprintsStorageKey));
  return Result.release();
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::HasAnyCache()
{
  bool Result = false;
  std::unique_ptr<TStrings> Caches(GetCaches());
  for (int Index = 0; Index < Caches->Count; Index++)
  {
    if (RegistryPathExists(Caches->Strings[Index]))
    {
      Result = true;
      break;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupCaches()
{
  try
  {
    std::unique_ptr<TStrings> Caches(GetCaches());
    for (int Index = 0; Index < Caches->Count; Index++)
    {
      CleanupRegistry(Caches->Strings[Index]);
    }
  }
  catch (Exception & E)
  {
    throw ExtException(&E, LoadStr(CLEANUP_CACHES_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupRandomSeedFile()
{
  try
  {
    DontSaveRandomSeed();
    if (FileExists(ApiPath(RandomSeedFileName)))
    {
      DeleteFileChecked(RandomSeedFileName);
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, LoadStr(CLEANUP_SEEDFILE_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::CleanupIniFile()
{
  try
  {
    if (FileExists(ApiPath(IniFileStorageNameForReading)))
    {
      DeleteFileChecked(IniFileStorageNameForReading);
    }
    if (Storage == stIniFile)
    {
      FDontSave = true;
    }
  }
  catch (Exception &E)
  {
    throw ExtException(&E, LoadStr(CLEANUP_INIFILE_ERROR));
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::DontSave()
{
  FDontSave = true;
}
//---------------------------------------------------------------------------
RawByteString __fastcall TConfiguration::EncryptPassword(UnicodeString Password, UnicodeString Key)
{
  if (Password.IsEmpty())
  {
    return RawByteString();
  }
  else
  {
    return ::EncryptPassword(Password, Key);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::DecryptPassword(RawByteString Password, UnicodeString Key)
{
  if (Password.IsEmpty())
  {
    return UnicodeString();
  }
  else
  {
    return ::DecryptPassword(Password, Key);
  }
}
//---------------------------------------------------------------------------
RawByteString __fastcall TConfiguration::StronglyRecryptPassword(RawByteString Password, UnicodeString /*Key*/)
{
  return Password;
}
//---------------------------------------------------------------------------
TVSFixedFileInfo *__fastcall TConfiguration::GetFixedApplicationInfo()
{
  return GetFixedFileInfo(ApplicationInfo);
}
//---------------------------------------------------------------------------
int __fastcall TConfiguration::GetCompoundVersion()
{
  TVSFixedFileInfo * FileInfo = FixedApplicationInfo;
  return CalculateCompoundVersion(
    HIWORD(FileInfo->dwFileVersionMS), LOWORD(FileInfo->dwFileVersionMS),
    HIWORD(FileInfo->dwFileVersionLS));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::ModuleFileName()
{
  return ParamStr(0);
}
//---------------------------------------------------------------------------
void * __fastcall TConfiguration::GetFileApplicationInfo(const UnicodeString FileName)
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
UnicodeString __fastcall TConfiguration::GetFileProductName(const UnicodeString FileName)
{
  return GetFileFileInfoString(L"ProductName", FileName);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileCompanyName(const UnicodeString FileName)
{
  // particularly in IDE build, company name is empty
  return GetFileFileInfoString(L"CompanyName", FileName, true);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetProductName()
{
  return GetFileProductName(L"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetCompanyName()
{
  return GetFileCompanyName(L"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileProductVersion(const UnicodeString FileName)
{
  return TrimVersion(GetFileFileInfoString(L"ProductVersion", FileName));
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileDescription(const UnicodeString & FileName)
{
  return GetFileFileInfoString(L"FileDescription", FileName);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetProductVersion()
{
  return GetFileProductVersion(L"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetReleaseType()
{
  return GetFileInfoString(L"ReleaseType");
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetIsUnofficial()
{
  #ifdef BUILD_OFFICIAL
  return false;
  #else
  return true;
  #endif
}
//---------------------------------------------------------------------------
static TDateTime GetBuildDate()
{
  UnicodeString BuildDateStr = __DATE__;
  UnicodeString MonthStr = CutToChar(BuildDateStr, L' ', true);
  int Month = ParseShortEngMonthName(MonthStr);
  int Day = StrToInt(CutToChar(BuildDateStr, L' ', true));
  int Year = StrToInt(Trim(BuildDateStr));
  TDateTime Result = EncodeDateVerbose(static_cast<Word>(Year), static_cast<Word>(Month), static_cast<Word>(Day));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TConfiguration::GetFullVersion()
{
  UnicodeString Result = Version;

  UnicodeString AReleaseType = GetReleaseType();
  if (DebugAlwaysTrue(!AReleaseType.IsEmpty()) &&
      !SameText(AReleaseType, L"stable") &&
      !SameText(AReleaseType, L"development"))
  {
    Result += L" " + AReleaseType;
  }
  return Result;
}
//---------------------------------------------------------------------------
static UnicodeString GetUnofficialBuildTag()
{
  DebugAssert(Configuration->IsUnofficial);
  UnicodeString Result;
  #ifdef _DEBUG
  Result = LoadStr(VERSION_DEBUG_BUILD);
  #else
  Result = LoadStr(VERSION_DEV_BUILD);
  #endif
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TConfiguration::GetVersionStrHuman()
{
  TGuard Guard(FCriticalSection);
  try
  {
    TDateTime BuildDate = GetBuildDate();

    UnicodeString FullVersion = GetFullVersion();

    if (IsUnofficial)
    {
      UnicodeString BuildStr = GetUnofficialBuildTag();
      FullVersion += L" " + BuildStr;
    }

    UnicodeString DateStr;
    TDateTime ANow = Now();
    if (BuildDate < ANow)
    {
      DateStr = FormatRelativeTime(ANow, BuildDate, true);
    }
    else
    {
      DateStr = FormatDateTime(L"ddddd", BuildDate);
    }

    UnicodeString Result = FORMAT(L"%s (%s)", (FullVersion, DateStr));

    return Result;
  }
  catch (Exception &E)
  {
    throw ExtException(&E, L"Can't get application version");
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetVersionStr()
{
  TGuard Guard(FCriticalSection);
  try
  {
    UnicodeString BuildStr;
    if (!IsUnofficial)
    {
      BuildStr = LoadStr(VERSION_BUILD);
    }
    else
    {
      BuildStr = GetUnofficialBuildTag();
    }

    UnicodeString FullVersion = GetFullVersion();

    int Build = LOWORD(FixedApplicationInfo->dwFileVersionLS);
    if (Build > 0)
    {
      BuildStr += L" " + IntToStr(Build);
    }

    UnicodeString DateStr = FormatDateTime(L"yyyy-mm-dd", GetBuildDate());
    AddToList(BuildStr, DateStr, L" ");

    UnicodeString Result = FMTLOAD(VERSION2, (FullVersion, BuildStr));

    if (IsUnofficial)
    {
      Result += L" " + LoadStr(VERSION_DONT_DISTRIBUTE);
    }

    return Result;
  }
  catch (Exception &E)
  {
    throw ExtException(&E, L"Can't get application version");
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileVersion(const UnicodeString & FileName)
{
  UnicodeString Result;
  void * FileInfo = CreateFileInfo(FileName);
  try
  {
    Result = GetFileVersion(GetFixedFileInfo(FileInfo));
  }
  __finally
  {
    FreeFileInfo(FileInfo);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileVersion(TVSFixedFileInfo * Info)
{
  TGuard Guard(FCriticalSection);
  try
  {
    UnicodeString Result =
      FormatVersion(
        HIWORD(Info->dwFileVersionMS),
        LOWORD(Info->dwFileVersionMS),
        HIWORD(Info->dwFileVersionLS));
    return Result;
  }
  catch (Exception &E)
  {
    throw ExtException(&E, L"Can't get file version");
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetVersion()
{
  return GetFileVersion(FixedApplicationInfo);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileFileInfoString(const UnicodeString Key,
  const UnicodeString FileName, bool AllowEmpty)
{
  TGuard Guard(FCriticalSection);

  UnicodeString Result;
  void * Info = GetFileApplicationInfo(FileName);
  try
  {
    if ((Info != NULL) && (GetTranslationCount(Info) > 0))
    {
      TTranslation Translation;
      Translation = GetTranslation(Info, 0);
      Result = ::GetFileInfoString(Info, Translation, Key, AllowEmpty);
    }
    else
    {
      DebugAssert(!FileName.IsEmpty());
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
UnicodeString __fastcall TConfiguration::GetFileInfoString(const UnicodeString Key)
{
  return GetFileFileInfoString(Key, L"");
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetFileMimeType(const UnicodeString & FileName)
{
  UnicodeString Result;
  bool Found = false;

  if (!MimeTypes.IsEmpty())
  {
    UnicodeString FileNameOnly = ExtractFileName(FileName);
    UnicodeString AMimeTypes = MimeTypes;
    while (!Found && !AMimeTypes.IsEmpty())
    {
      UnicodeString Token = CutToChar(AMimeTypes, L',', true);
      UnicodeString MaskStr = CutToChar(Token, L'=', true);
      TFileMasks Mask(MaskStr);
      if (Mask.MatchesFileName(FileNameOnly))
      {
        Result = Token.Trim();
        Found = true;
      }
    }
  }

  if (!Found) // allow an override to "no" Content-Type
  {
    Result = ::GetFileMimeType(FileName);
  }

  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetRegistryStorageKey()
{
  return GetRegistryKey();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetNulStorage()
{
  FStorage = stNul;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetExplicitIniFileStorageName(const UnicodeString & FileName)
{
  FIniFileStorageName = FileName;
  FStorage = stIniFile;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetDefaultIniFileExportPath()
{
  UnicodeString PersonalDirectory = GetPersonalFolder();
  UnicodeString FileName = IncludeTrailingBackslash(PersonalDirectory) +
    ExtractFileName(ExpandEnvironmentVariables(IniFileStorageName));
  return FileName;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetIniFileStorageNameForReading()
{
  return GetIniFileStorageName(true);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetIniFileStorageNameForReadingWriting()
{
  return GetIniFileStorageName(false);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetAutomaticIniFileStorageName(bool ReadingOnly)
{
  UnicodeString ProgramPath = ParamStr(0);

  UnicodeString ProgramIniPath = ChangeFileExt(ProgramPath, L".ini");

  UnicodeString IniPath;
  if (FileExists(ApiPath(ProgramIniPath)))
  {
    IniPath = ProgramIniPath;
  }
  else
  {
    UnicodeString AppDataIniPath =
      IncludeTrailingBackslash(GetShellFolderPath(CSIDL_APPDATA)) +
      ExtractFileName(ProgramIniPath);
    if (FileExists(ApiPath(AppDataIniPath)))
    {
      IniPath = AppDataIniPath;
    }
    else
    {
      // avoid expensive test if we are interested in existing files only
      if (!ReadingOnly && (FProgramIniPathWritable < 0))
      {
        UnicodeString ProgramDir = ExtractFilePath(ProgramPath);
        FProgramIniPathWritable = IsDirectoryWriteable(ProgramDir) ? 1 : 0;
      }

      // does not really matter what we return when < 0
      IniPath = (FProgramIniPathWritable == 0) ? AppDataIniPath : ProgramIniPath;
    }
  }

  // BACKWARD COMPATIBILITY with 4.x
  if (FVirtualIniFileStorageName.IsEmpty() &&
      TPath::IsDriveRooted(IniPath))
  {
    UnicodeString LocalAppDataPath = GetShellFolderPath(CSIDL_LOCAL_APPDATA);
    // virtual store for non-system drives have a different virtual store,
    // do not bother about them
    if (TPath::IsDriveRooted(LocalAppDataPath) &&
        SameText(ExtractFileDrive(IniPath), ExtractFileDrive(LocalAppDataPath)))
    {
      FVirtualIniFileStorageName =
        IncludeTrailingBackslash(LocalAppDataPath) +
        L"VirtualStore\\" +
        IniPath.SubString(4, IniPath.Length() - 3);
    }
  }

  if (!FVirtualIniFileStorageName.IsEmpty() &&
      FileExists(ApiPath(FVirtualIniFileStorageName)))
  {
    return FVirtualIniFileStorageName;
  }
  else
  {
    return IniPath;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetIniFileParamValue()
{
  UnicodeString Result;
  if (Storage == stNul)
  {
    Result = INI_NUL;
  }
  else if ((Storage == stIniFile) && !FIniFileStorageName.IsEmpty())
  {
    Result = FIniFileStorageName;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetIniFileStorageName(bool ReadingOnly)
{
  UnicodeString Result;
  if (!FIniFileStorageName.IsEmpty())
  {
    Result = FIniFileStorageName;
  }
  else if (!FCustomIniFileStorageName.IsEmpty())
  {
    Result = ExpandEnvironmentVariables(FCustomIniFileStorageName);
  }
  else
  {
    Result = GetAutomaticIniFileStorageName(ReadingOnly);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetOptionsStorage(TStrings * value)
{
  TGuard Guard(FCriticalSection);
  if (FOptionsStorage.get() == NULL)
  {
    FOptionsStorage.reset(new TStringList());
  }
  FOptionsStorage->AddStrings(value);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TConfiguration::GetOptionsStorage()
{
  return FOptionsStorage.get();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetPuttySessionsSubKey()
{
  return StoredSessionsSubKey;
}
//---------------------------------------------------------------------------
UnicodeString TConfiguration::GetPuttySessionsKey(const UnicodeString & RootKey)
{
  return RootKey + L"\\" + PuttySessionsSubKey;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::DoGetPuttySessionsKey()
{
  return GetPuttySessionsKey(PuttyRegistryStorageKey);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetStoredSessionsSubKey()
{
  return L"Sessions";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetSshHostKeysSubKey()
{
  return L"SshHostKeys";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetConfigurationSubKey()
{
  return L"Configuration";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetRootKeyStr()
{
  return RootKeyToStr(HKEY_CURRENT_USER);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::MoveStorage(TStorage AStorage, const UnicodeString & ACustomIniFileStorageName)
{
  if ((FStorage != AStorage) ||
      ((FStorage == stIniFile) && !FIniFileStorageName.IsEmpty()) ||
      // Not expanding, as we want to allow change from explicit path to path with variables and vice versa
      !IsPathToSameFile(FCustomIniFileStorageName, ACustomIniFileStorageName))
  {
    TStorage StorageBak = FStorage;
    UnicodeString CustomIniFileStorageNameBak = FCustomIniFileStorageName;
    UnicodeString IniFileStorageNameBak = FIniFileStorageName;
    try
    {
      THierarchicalStorage * SourceStorage = NULL;
      THierarchicalStorage * TargetStorage = NULL;

      try
      {
        SourceStorage = CreateConfigStorage();
        SourceStorage->AccessMode = smRead;

        FStorage = AStorage;
        FCustomIniFileStorageName = ACustomIniFileStorageName;
        FIniFileStorageName = UnicodeString();

        TargetStorage = CreateConfigStorage();
        TargetStorage->AccessMode = smReadWrite;
        TargetStorage->Explicit = true;

        // copy before save as it removes the ini file,
        // when switching from ini to registry
        CopyData(SourceStorage, TargetStorage);
      }
      __finally
      {
        delete SourceStorage;
        delete TargetStorage;
      }

      // save all and explicit,
      // this also removes an INI file, when switching to registry storage
      DoSave(true, true);
    }
    catch (...)
    {
      // If this fails, do not pretend that storage was switched.
      // For instance:
      // - When writing to an INI file fails (unlikely, as we fallback to user profile)
      // - When removing INI file fails, when switching to registry
      //   (possible, when the INI file is in Program Files folder)
      FStorage = StorageBak;
      FCustomIniFileStorageName = CustomIniFileStorageNameBak;
      FIniFileStorageName = IniFileStorageNameBak;
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::ScheduleCustomIniFileStorageUse(const UnicodeString & ACustomIniFileStorageName)
{
  FStorage = stIniFile;
  FCustomIniFileStorageName = ACustomIniFileStorageName;
  SaveCustomIniFileStorageName();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::Saved()
{
  // nothing
}
//---------------------------------------------------------------------------
TStorage __fastcall TConfiguration::GetStorage()
{
  TGuard Guard(FCriticalSection);
  if (FStorage == stDetect)
  {
    DebugFail(); // This is never called, as the detection is completely overriden by TWinConfiguration
    if (FileExists(ApiPath(IniFileStorageNameForReading)))
    {
      FStorage = stIniFile;
    }
    else
    {
      FStorage = stRegistry;
    }
  }
  return FStorage;
}
//---------------------------------------------------------------------
static TStoredSessionList * CreateSessionsForImport(TStoredSessionList * Sessions)
{
  std::unique_ptr<TStoredSessionList> Result(new TStoredSessionList(true));
  Result->DefaultSettings = Sessions->DefaultSettings;
  return Result.release();
}
//---------------------------------------------------------------------
void TConfiguration::SelectSessionsToImportIfAny(
  TStoredSessionList * ImportSessionList, TStoredSessionList * Sessions,
  UnicodeString & Error, const UnicodeString & NoSessionsError)
{
  if (ImportSessionList->Count > 0)
  {
    ImportSessionList->SelectSessionsToImport(Sessions, true);
  }
  else
  {
    Error = NoSessionsError;
  }
}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TConfiguration::SelectFilezillaSessionsForImport(
  TStoredSessionList * Sessions, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(CreateSessionsForImport(Sessions));

  UnicodeString AppDataPath = GetShellFolderPath(CSIDL_APPDATA);
  UnicodeString FilezillaSiteManagerFile = CombinePaths(AppDataPath, L"FileZilla\\sitemanager.xml");
  UnicodeString FilezillaConfigurationFile = CombinePaths(AppDataPath, L"FileZilla\\filezilla.xml");

  if (FileExists(ApiPath(FilezillaSiteManagerFile)))
  {
    ImportSessionList->ImportFromFilezilla(FilezillaSiteManagerFile, FilezillaConfigurationFile);

    UnicodeString NoSessionsError = FMTLOAD(FILEZILLA_NO_SITES, (FilezillaSiteManagerFile));
    SelectSessionsToImportIfAny(ImportSessionList.get(), Sessions, Error, NoSessionsError);
  }
  else
  {
    Error = FMTLOAD(FILEZILLA_SITE_MANAGER_NOT_FOUND, (FilezillaSiteManagerFile));
  }

  return ImportSessionList.release();
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::AnyFilezillaSessionForImport(TStoredSessionList * Sessions)
{
  try
  {
    UnicodeString Error;
    std::unique_ptr<TStoredSessionList> SessionsForImport(SelectFilezillaSessionsForImport(Sessions, Error));
    return (SessionsForImport->Count > 0);
  }
  catch (...)
  {
    return false;
  }
}
//---------------------------------------------------------------------
UnicodeString GetOpensshFolder()
{
  UnicodeString ProfilePath = GetShellFolderPath(CSIDL_PROFILE);
  UnicodeString Result = CombinePaths(ProfilePath, OpensshFolderName);
  return Result;
}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TConfiguration::SelectKnownHostsSessionsForImport(
  TStoredSessionList * Sessions, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(CreateSessionsForImport(Sessions));
  UnicodeString KnownHostsFile = CombinePaths(GetOpensshFolder(), L"known_hosts");

  try
  {
    if (FileExists(ApiPath(KnownHostsFile)))
    {
      std::unique_ptr<TStrings> Lines(new TStringList());
      LoadScriptFromFile(KnownHostsFile, Lines.get(), true);
      ImportSessionList->ImportFromKnownHosts(Lines.get());
    }
    else
    {
      throw Exception(LoadStr(KNOWN_HOSTS_NOT_FOUND));
    }
  }
  catch (Exception & E)
  {
    Error = FORMAT(L"%s\n(%s)", (E.Message, KnownHostsFile));
  }

  return ImportSessionList.release();
}
//---------------------------------------------------------------------
TStoredSessionList * __fastcall TConfiguration::SelectKnownHostsSessionsForImport(
  TStrings * Lines, TStoredSessionList * Sessions, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(CreateSessionsForImport(Sessions));

  try
  {
    ImportSessionList->ImportFromKnownHosts(Lines);
  }
  catch (Exception & E)
  {
    Error = E.Message;
  }

  return ImportSessionList.release();
}
//---------------------------------------------------------------------------
TStoredSessionList * TConfiguration::SelectOpensshSessionsForImport(
  TStoredSessionList * Sessions, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(CreateSessionsForImport(Sessions));
  UnicodeString ConfigFile = CombinePaths(GetOpensshFolder(), L"config");

  try
  {
    if (FileExists(ApiPath(ConfigFile)))
    {
      std::unique_ptr<TStrings> Lines(new TStringList());
      LoadScriptFromFile(ConfigFile, Lines.get(), true);

      const UnicodeString OpensshIncludeDirective(L"Include");
      for (int Index = 0; Index < Lines->Count; Index++)
      {
        UnicodeString Line = Lines->Strings[Index];
        UnicodeString Directive, Args;
        if (ParseOpensshDirective(Line, Directive, Args))
        {
          if (SameText(Directive, OpensshIncludeDirective))
          {
            while (!Args.IsEmpty())
            {
              UnicodeString IncludePath = ConvertPathFromOpenssh(CutOpensshToken(Args));

              // If path does not exist, try if it works relatively to .ssh/
              if (!FileExists(ApiPath(IncludePath)))
              {
                IncludePath = CombinePaths(GetOpensshFolder(), IncludePath);
              }

              if (FileExists(ApiPath(IncludePath)))
              {
                std::unique_ptr <TStrings> LinesToInclude(new TStringList());
                LoadScriptFromFile(IncludePath, LinesToInclude.get(), true);
                Lines->Delete(Index); // Not really needed
                for (int Index2 = 0; Index2 < LinesToInclude->Count; Index2++)
                {
                  Lines->Insert(Index + Index2, LinesToInclude->Strings[Index2]);
                }
                Index--;
              }
            }
          }
        }
      }

      ImportSessionList->ImportFromOpenssh(Lines.get());

      UnicodeString NoSessionsError = FORMAT(L"%s\n(%s)", (LoadStr(OPENSSH_CONFIG_NO_SITES), ConfigFile));
      SelectSessionsToImportIfAny(ImportSessionList.get(), Sessions, Error, NoSessionsError);
    }
    else
    {
      throw Exception(LoadStr(OPENSSH_CONFIG_NOT_FOUND));
    }
  }
  catch (Exception & E)
  {
    Error = FORMAT(L"%s\n(%s)", (E.Message, ConfigFile));
  }

  return ImportSessionList.release();
}
//---------------------------------------------------------------------------
TStoredSessionList * TConfiguration::SelectSessionsForImport(
  TStoredSessionList * Sessions, const UnicodeString & FileName, UnicodeString & Error)
{
  std::unique_ptr<TStoredSessionList> ImportSessionList(CreateSessionsForImport(Sessions));

  try
  {
    if (FileName.IsEmpty())
    {
      throw Exception(LoadStr(INI_SELECT));
    }
    else
    {
      std::unique_ptr<THierarchicalStorage> ImportStorage(TIniFileStorage::CreateFromPath(FileName));
      ImportStorage->AccessMode = smRead;

      if (ImportStorage->OpenSubKey(Configuration->StoredSessionsSubKey, false))
      {
        ImportSessionList->Load(ImportStorage.get());
      }

      UnicodeString NoSessionsError = FMTLOAD(INI_NO_SITES, (FileName));
      SelectSessionsToImportIfAny(ImportSessionList.get(), Sessions, Error, NoSessionsError);
    }
  }
  catch (Exception & E)
  {
    Error = E.Message;
  }

  return ImportSessionList.release();
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetRandomSeedFile(UnicodeString value)
{
  if (RandomSeedFile != value)
  {
    UnicodeString PrevRandomSeedFileName = RandomSeedFileName;

    FRandomSeedFile = value;

    // never allow empty seed file to avoid Putty trying to reinitialize the path
    if (RandomSeedFileName.IsEmpty())
    {
      FRandomSeedFile = FDefaultRandomSeedFile;
    }

    if (!PrevRandomSeedFileName.IsEmpty() &&
        (PrevRandomSeedFileName != RandomSeedFileName) &&
        FileExists(ApiPath(PrevRandomSeedFileName)))
    {
      // ignore any error
      DeleteFile(ApiPath(PrevRandomSeedFileName));
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetDirectoryStatisticsCacheKey(
  const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam)
{
  std::unique_ptr<TStringList> RawOptions(new TStringList());
  RawOptions->Add(SessionKey);
  RawOptions->Add(UnixExcludeTrailingBackslash(Path));

  TCopyParamType Defaults;
  TCopyParamType FilterCopyParam;
  FilterCopyParam.IncludeFileMask = CopyParam.IncludeFileMask;
  FilterCopyParam.ExcludeHiddenFiles = CopyParam.ExcludeHiddenFiles;
  FilterCopyParam.ExcludeEmptyDirectories = CopyParam.ExcludeEmptyDirectories;

  std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(RawOptions.get(), true));
  FilterCopyParam.Save(OptionsStorage.get(), &Defaults);

  UTF8String RawOptionsBuf(RawOptions->CommaText.LowerCase());
  UnicodeString Result = Sha256(RawOptionsBuf.c_str(), RawOptionsBuf.Length());
  return Result;
}
//---------------------------------------------------------------------------
THierarchicalStorage * TConfiguration::OpenDirectoryStatisticsCache(bool CanCreate)
{
  std::unique_ptr<THierarchicalStorage> Storage(Configuration->CreateConfigStorage());
  Storage->AccessMode = CanCreate ? smReadWrite : smRead;
  if (!Storage->OpenSubKey(DirectoryStatisticsCacheKey, CanCreate))
  {
    Storage.reset(NULL);
  }
  return Storage.release();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TConfiguration::LoadDirectoryStatisticsCache(
  const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam)
{
  std::unique_ptr<THierarchicalStorage> Storage(OpenDirectoryStatisticsCache(false));
  TStrings * Result;
  if (Storage.get() != NULL)
  {
    UnicodeString Key = GetDirectoryStatisticsCacheKey(SessionKey, Path, CopyParam);
    UnicodeString Buf = Storage->ReadString(Key, UnicodeString());
    Result = CommaTextToStringList(Buf);
  }
  else
  {
    Result = new TStringList();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SaveDirectoryStatisticsCache(
  const UnicodeString & SessionKey, const UnicodeString & Path, const TCopyParamType & CopyParam, TStrings * DataList)
{
  std::unique_ptr<THierarchicalStorage> Storage(OpenDirectoryStatisticsCache(true));
  if (Storage.get() != NULL)
  {
    UnicodeString Key = GetDirectoryStatisticsCacheKey(SessionKey, Path, CopyParam);
    UnicodeString Buf = DataList->CommaText;
    Storage->WriteString(Key, Buf);
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetRandomSeedFileName()
{
  // StripPathQuotes should not be needed as we do not feed quotes anymore
  return StripPathQuotes(ExpandEnvironmentVariables(FRandomSeedFile)).Trim();
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetExternalIpAddress(UnicodeString value)
{
  SET_CONFIG_PROPERTY(ExternalIpAddress);
}
//---------------------------------------------------------------------
bool TConfiguration::HasLocalPortNumberLimits()
{
  return (LocalPortNumberMin > 0) && (LocalPortNumberMax >= LocalPortNumberMin);
}
//---------------------------------------------------------------------
void TConfiguration::SetLocalPortNumberMin(int value)
{
  SET_CONFIG_PROPERTY(LocalPortNumberMin);
}
//---------------------------------------------------------------------
void TConfiguration::SetLocalPortNumberMax(int value)
{
  SET_CONFIG_PROPERTY(LocalPortNumberMax);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetMimeTypes(UnicodeString value)
{
  SET_CONFIG_PROPERTY(MimeTypes);
}
//---------------------------------------------------------------------
void TConfiguration::SetCertificateStorage(const UnicodeString & value)
{
  SET_CONFIG_PROPERTY(CertificateStorage);
}
//---------------------------------------------------------------------
UnicodeString TConfiguration::GetCertificateStorageExpanded()
{
  UnicodeString Result = FCertificateStorage;
  if (Result.IsEmpty())
  {
    UnicodeString DefaultCertificateStorage = CombinePaths(ExtractFilePath(ModuleFileName()), L"cacert.pem");
    if (FileExists(DefaultCertificateStorage))
    {
      Result = DefaultCertificateStorage;
    }
  }
  return Result;
}
//---------------------------------------------------------------------
void TConfiguration::SetAWSAPI(const UnicodeString & value)
{
  SET_CONFIG_PROPERTY(AWSAPI);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetTryFtpWhenSshFails(bool value)
{
  SET_CONFIG_PROPERTY(TryFtpWhenSshFails);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetParallelDurationThreshold(int value)
{
  SET_CONFIG_PROPERTY(ParallelDurationThreshold);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetPuttyRegistryStorageKey(UnicodeString value)
{
  SET_CONFIG_PROPERTY_EX(PuttyRegistryStorageKey,
    RefreshPuttySshHostCAList());
}
//---------------------------------------------------------------------------
TEOLType __fastcall TConfiguration::GetLocalEOLType()
{
  return eolCRLF;
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetCollectUsage()
{
  return FUsage->Collect;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetCollectUsage(bool value)
{
  FUsage->Collect = value;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryLogging(const UnicodeString ALogFileName)
{
  if (SameText(ExtractFileExt(ALogFileName), L".xml"))
  {
    TemporaryActionsLogging(ALogFileName);
  }
  else
  {
    FLogging = true;
    FLogFileName = ALogFileName;
    UpdateActualLogProtocol();
  }
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryActionsLogging(const UnicodeString ALogFileName)
{
  FLogActions = true;
  FActionsLogFileName = ALogFileName;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryLogProtocol(int ALogProtocol)
{
  FLogProtocol = ALogProtocol;
  UpdateActualLogProtocol();
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryLogSensitive(bool ALogSensitive)
{
  FLogSensitive = ALogSensitive;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryLogMaxSize(__int64 ALogMaxSize)
{
  FLogMaxSize = ALogMaxSize;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::TemporaryLogMaxCount(int ALogMaxCount)
{
  FLogMaxCount = ALogMaxCount;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogging(bool value)
{
  TGuard Guard(FCriticalSection);
  if (Logging != value)
  {
    FPermanentLogging = value;
    FLogging = value;
    UpdateActualLogProtocol();
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogging()
{
  TGuard Guard(FCriticalSection);
  return FPermanentLogging;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileName(UnicodeString value)
{
  TGuard Guard(FCriticalSection);
  if (LogFileName != value)
  {
    FPermanentLogFileName = value;
    FLogFileName = value;
    Changed();
  }
}
//---------------------------------------------------------------------
UnicodeString  __fastcall TConfiguration::GetLogFileName()
{
  TGuard Guard(FCriticalSection);
  return FPermanentLogFileName;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetActionsLogFileName(UnicodeString value)
{
  TGuard Guard(FCriticalSection);
  if (ActionsLogFileName != value)
  {
    FPermanentActionsLogFileName = value;
    FActionsLogFileName = value;
    Changed();
  }
}
//---------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetPermanentActionsLogFileName()
{
  TGuard Guard(FCriticalSection);
  return FPermanentActionsLogFileName;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetActionsLogFileName()
{
  TGuard Guard(FCriticalSection);
  return FActionsLogFileName;
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogToFile()
{
  // guarded within GetLogFileName
  return !LogFileName.IsEmpty();
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::UpdateActualLogProtocol()
{
  FActualLogProtocol = FLogging ? FLogProtocol : (-BelowNormalLogLevels - 1);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogProtocol(int value)
{
  TGuard Guard(FCriticalSection);
  if (LogProtocol != value)
  {
    FPermanentLogProtocol = value;
    FLogProtocol = value;
    Changed();
    UpdateActualLogProtocol();
  }
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogActions(bool value)
{
  TGuard Guard(FCriticalSection);
  if (LogActions != value)
  {
    FPermanentLogActions = value;
    FLogActions = value;
    Changed();
  }
}
//---------------------------------------------------------------------
bool __fastcall TConfiguration::GetLogActions()
{
  TGuard Guard(FCriticalSection);
  return FPermanentLogActions;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogFileAppend(bool value)
{
  SET_CONFIG_PROPERTY(LogFileAppend);
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogSensitive(bool value)
{
  if (LogSensitive != value)
  {
    FPermanentLogSensitive = value;
    FLogSensitive = value;
    Changed();
  }
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogMaxSize(__int64 value)
{
  TGuard Guard(FCriticalSection);
  if (LogMaxSize != value)
  {
    FPermanentLogMaxSize = value;
    FLogMaxSize = value;
    Changed();
  }
}
//---------------------------------------------------------------------
__int64 __fastcall TConfiguration::GetLogMaxSize()
{
  TGuard Guard(FCriticalSection);
  return FPermanentLogMaxSize;
}
//---------------------------------------------------------------------
void __fastcall TConfiguration::SetLogMaxCount(int value)
{
  if (LogMaxCount != value)
  {
    FPermanentLogMaxCount = value;
    FLogMaxCount = value;
    Changed();
  }
}
//---------------------------------------------------------------------
int __fastcall TConfiguration::GetLogMaxCount()
{
  TGuard Guard(FCriticalSection);
  return FPermanentLogMaxCount;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetDefaultLogFileName()
{
  return L"%TEMP%\\!S.log";
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
void __fastcall TConfiguration::SetAutoReadDirectoryAfterOp(bool value)
{
  TGuard Guard(FCriticalSection);
  SET_CONFIG_PROPERTY(AutoReadDirectoryAfterOp);
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetAutoReadDirectoryAfterOp()
{
  TGuard Guard(FCriticalSection);
  return FAutoReadDirectoryAfterOp;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetTimeFormat()
{
  return L"h:nn:ss";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TConfiguration::GetDefaultKeyFile()
{
  return L"";
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
void __fastcall TConfiguration::SetSessionReopenBackground(int value)
{
  SET_CONFIG_PROPERTY(SessionReopenBackground);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetSessionReopenTimeout(int value)
{
  SET_CONFIG_PROPERTY(SessionReopenTimeout);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetSessionReopenAutoStall(int value)
{
  SET_CONFIG_PROPERTY(SessionReopenAutoStall);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetTunnelLocalPortNumberLow(int value)
{
  SET_CONFIG_PROPERTY(TunnelLocalPortNumberLow);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetTunnelLocalPortNumberHigh(int value)
{
  SET_CONFIG_PROPERTY(TunnelLocalPortNumberHigh);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetCacheDirectoryChangesMaxSize(int value)
{
  SET_CONFIG_PROPERTY(CacheDirectoryChangesMaxSize);
}
//---------------------------------------------------------------------------
void __fastcall TConfiguration::SetShowFtpWelcomeMessage(bool value)
{
  SET_CONFIG_PROPERTY(ShowFtpWelcomeMessage);
}
//---------------------------------------------------------------------------
void TConfiguration::SetQueueTransfersLimit(int value)
{
  SET_CONFIG_PROPERTY(QueueTransfersLimit);
}
//---------------------------------------------------------------------------
const TSshHostCAList * TConfiguration::GetSshHostCAList()
{
  return FSshHostCAList.get();
}
//---------------------------------------------------------------------------
void TConfiguration::SetSshHostCAList(const TSshHostCAList * value)
{
  *FSshHostCAList = *value;
}
//---------------------------------------------------------------------------
const TSshHostCAList * TConfiguration::GetPuttySshHostCAList()
{
  if (FPuttySshHostCAList.get() == NULL)
  {
    std::unique_ptr<TRegistryStorage> Storage(new TRegistryStorage(PuttyRegistryStorageKey));
    Storage->ConfigureForPutty();
    FPuttySshHostCAList.reset(new TSshHostCAList());
    LoadSshHostCAList(FPuttySshHostCAList.get(), Storage.get());
  }
  return FPuttySshHostCAList.get();
}
//---------------------------------------------------------------------------
void TConfiguration::RefreshPuttySshHostCAList()
{
  FPuttySshHostCAList.reset(NULL);
}
//---------------------------------------------------------------------------
const TSshHostCAList * TConfiguration::GetActiveSshHostCAList()
{
  return FSshHostCAsFromPuTTY ? PuttySshHostCAList : SshHostCAList;
}
//---------------------------------------------------------------------------
bool __fastcall TConfiguration::GetPersistent()
{
  return (Storage != stNul) && !FDontSave;
}
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
void __fastcall TShortCuts::Add(TShortCut ShortCut)
{
  FShortCuts.insert(ShortCut);
}
//---------------------------------------------------------------------------
bool __fastcall TShortCuts::Has(TShortCut ShortCut) const
{
  return (FShortCuts.count(ShortCut) != 0);
}
