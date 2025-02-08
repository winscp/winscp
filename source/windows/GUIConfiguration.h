//---------------------------------------------------------------------------
#ifndef GUIConfigurationH
#define GUIConfigurationH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "CopyParam.h"
//---------------------------------------------------------------------------
class TGUIConfiguration;
class TStoredSessionList;
enum TInterface { ifCommander, ifExplorer };
//---------------------------------------------------------------------------
extern const int ccLocal;
extern const int ccShowResults;
extern const int ccCopyResults;
extern const int ccSet;
extern const int ccRemoteFiles;
extern const int ccShowResultsInMsgBox;
//---------------------------------------------------------------------------
const int soRecurse =         0x01;
const int soSynchronize =     0x02;
const int soSynchronizeAsk =  0x04;
const int soContinueOnError = 0x08;
//---------------------------------------------------------------------------
class TGUICopyParamType : public TCopyParamType
{
public:
  __fastcall TGUICopyParamType();
  __fastcall TGUICopyParamType(const TCopyParamType & Source);
  __fastcall TGUICopyParamType(const TGUICopyParamType & Source);

  virtual void __fastcall Load(THierarchicalStorage * Storage);
  virtual void __fastcall Save(THierarchicalStorage * Storage, const TCopyParamType * Defaults = NULL) const;

  virtual void __fastcall Default();
  virtual void __fastcall Assign(const TCopyParamType * Source);
  TGUICopyParamType & __fastcall operator =(const TGUICopyParamType & rhp);
  TGUICopyParamType & __fastcall operator =(const TCopyParamType & rhp);

  __property bool Queue = { read = FQueue, write = FQueue };
  __property bool QueueNoConfirmation = { read = FQueueNoConfirmation, write = FQueueNoConfirmation };
  __property bool QueueParallel = { read = FQueueParallel, write = FQueueParallel };

protected:
  void __fastcall GUIDefault();
  void __fastcall GUIAssign(const TGUICopyParamType * Source);

private:
  bool FQueue;
  bool FQueueNoConfirmation;
  bool FQueueParallel;
};
//---------------------------------------------------------------------------
struct TCopyParamRuleData
{
  UnicodeString HostName;
  UnicodeString UserName;
  UnicodeString RemoteDirectory;
  UnicodeString LocalDirectory;

  void __fastcall Default();
};
//---------------------------------------------------------------------------
class TCopyParamRule
{
public:
  __fastcall TCopyParamRule();
  __fastcall TCopyParamRule(const TCopyParamRuleData & Data);
  __fastcall TCopyParamRule(const TCopyParamRule & Source);

  bool __fastcall Matches(const TCopyParamRuleData & Value) const;
  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;

  UnicodeString __fastcall GetInfoStr(UnicodeString Separator) const;

  bool __fastcall operator ==(const TCopyParamRule & rhp) const;

  __property TCopyParamRuleData Data = { read = FData, write = FData };
  __property bool IsEmpty = { read = GetEmpty };

private:
  TCopyParamRuleData FData;

  inline bool __fastcall Match(const UnicodeString & Mask,
    const UnicodeString & Value, bool Path, bool Local, int ForceDirectoryMasks) const;
  bool __fastcall GetEmpty() const;
};
//---------------------------------------------------------------------------
class TLocaleInfo : public TObject
{
public:
  LCID Locale;
  UnicodeString Name;
  int Completeness;
};
//---------------------------------------------------------------------------
class TCopyParamList
{
friend class TGUIConfiguration;
public:
  __fastcall TCopyParamList();
  virtual __fastcall ~TCopyParamList();
  int __fastcall Find(const TCopyParamRuleData & Value) const;

  void __fastcall Load(THierarchicalStorage * Storage, int Count);
  void __fastcall Save(THierarchicalStorage * Storage) const;

  static void __fastcall ValidateName(const UnicodeString Name);

  TCopyParamList & __fastcall operator=(const TCopyParamList & rhl);
  bool __fastcall operator==(const TCopyParamList & rhl) const;

  void __fastcall Clear();
  void __fastcall Add(const UnicodeString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Insert(int Index, const UnicodeString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Change(int Index, const UnicodeString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Move(int CurIndex, int NewIndex);
  void __fastcall Delete(int Index);
  int __fastcall IndexOfName(const UnicodeString Name) const;

  __property int Count = { read = GetCount };
  __property UnicodeString Names[int Index] = { read = GetName };
  __property const TCopyParamRule * Rules[int Index] = { read = GetRule };
  __property const TCopyParamType * CopyParams[int Index] = { read = GetCopyParam };
  __property bool Modified = { read = FModified };
  __property TStrings * NameList = { read = GetNameList };
  __property bool AnyRule = { read = GetAnyRule };

private:
  static UnicodeString FInvalidChars;
  TList * FRules;
  TList * FCopyParams;
  TStrings * FNames;
  mutable TStrings * FNameList;
  bool FModified;

  int __fastcall GetCount() const;
  const TCopyParamRule * __fastcall GetRule(int Index) const;
  const TCopyParamType * __fastcall GetCopyParam(int Index) const;
  UnicodeString __fastcall GetName(int Index) const;
  TStrings * __fastcall GetNameList() const;
  bool __fastcall GetAnyRule() const;

  void __fastcall Init();
  void __fastcall Reset();
  void __fastcall Modify();
  bool __fastcall CompareItem(int Index, const TCopyParamType * CopyParam,
    const TCopyParamRule * Rule) const;
};
//---------------------------------------------------------------------------
class TGUIConfiguration : public TConfiguration
{
private:
  TObjectList * FLocales;
  UnicodeString FLastLocalesExts;
  bool FContinueOnError;
  bool FConfirmCommandSession;
  UnicodeString FPuttyPath;
  TAutoSwitch FUsePuttyPwFile;
  bool FPuttyPassword;
  bool FTelnetForFtpInPutty;
  UnicodeString FPuttySession;
  int FSynchronizeParams;
  int FSynchronizeOptions;
  int FSynchronizeModeAuto;
  int FSynchronizeMode;
  int FMaxWatchDirectories;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FQueueAutoPopup;
  bool FSessionRememberPassword;
  bool FQueueBootstrap;
  bool FQueueKeepDoneItems;
  int FQueueKeepDoneItemsFor;
  TGUICopyParamType FDefaultCopyParam;
  bool FBeepOnFinish;
  TDateTime FBeepOnFinishAfter;
  UnicodeString FBeepSound;
  UnicodeString FDefaultPuttyPathOnly;
  UnicodeString FDefaultPuttyPath;
  TCopyParamList * FCopyParamList;
  bool FCopyParamListDefaults;
  UnicodeString FCopyParamCurrent;
  TRemoteProperties FNewDirectoryProperties;
  int FKeepUpToDateChangeDelay;
  UnicodeString FChecksumAlg;
  int FSessionReopenAutoIdle;
  LCID FAppliedLocale;
  // Corresponds to FAppliedLocale
  UnicodeString FLocaleModuleName;

protected:
  LCID FLocale;

  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  LCID __fastcall GetLocale();
  void __fastcall SetLocale(LCID value);
  void __fastcall SetLocaleSafe(LCID value);
  UnicodeString __fastcall GetAppliedLocaleHex();
  virtual HINSTANCE __fastcall LoadNewResourceModule(LCID Locale,
    UnicodeString & FileName);
  HANDLE __fastcall GetResourceModule();
  void __fastcall SetResourceModule(HINSTANCE Instance);
  TObjectList * __fastcall GetLocales();
  void __fastcall AddLocale(LCID Locale, const UnicodeString & Name);
  void __fastcall FreeResourceModule(HANDLE Instance);
  void __fastcall SetDefaultCopyParam(const TGUICopyParamType & value);
  virtual bool __fastcall GetRememberPassword();
  const TCopyParamList * __fastcall GetCopyParamList();
  void __fastcall SetCopyParamList(const TCopyParamList * value);
  virtual void __fastcall DefaultLocalized();
  int __fastcall GetCopyParamIndex();
  TGUICopyParamType __fastcall GetCurrentCopyParam();
  TGUICopyParamType __fastcall GetCopyParamPreset(UnicodeString Name);
  bool __fastcall GetHasCopyParamPreset(UnicodeString Name);
  void __fastcall SetCopyParamIndex(int value);
  void __fastcall SetCopyParamCurrent(UnicodeString value);
  void __fastcall SetNewDirectoryProperties(const TRemoteProperties & value);
  virtual void __fastcall Saved();
  void __fastcall SetQueueBootstrap(bool value);
  void __fastcall SetQueueKeepDoneItems(bool value);
  void __fastcall SetQueueKeepDoneItemsFor(int value);
  void __fastcall SetLocaleInternal(LCID value, bool Safe, bool CompleteOnly);
  void __fastcall SetAppliedLocale(LCID AppliedLocale, const UnicodeString & LocaleModuleName);
  bool __fastcall GetCanApplyLocaleImmediately();
  UnicodeString __fastcall GetTranslationModule(const UnicodeString & Path);
  void __fastcall FindLocales(const UnicodeString & Path, TStrings * Exts, UnicodeString & LocalesExts);
  virtual int __fastcall GetResourceModuleCompleteness(HINSTANCE Module);
  virtual bool __fastcall IsTranslationComplete(HINSTANCE Module);
  static int __fastcall LocalesCompare(void * Item1, void * Item2);
  LCID __fastcall InternalLocale();
  bool __fastcall DoSaveCopyParam(THierarchicalStorage * Storage, const TCopyParamType * CopyParam, const TCopyParamType * Defaults);

public:
  __fastcall TGUIConfiguration();
  virtual __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall UpdateStaticUsage();
  bool __fastcall LoadCopyParam(THierarchicalStorage * Storage, TCopyParamType * CopyParam);
  void __fastcall LoadDefaultCopyParam(THierarchicalStorage * Storage);

  HANDLE __fastcall ChangeToDefaultResourceModule();
  HANDLE __fastcall ChangeResourceModule(HANDLE Instance);
  bool __fastcall UsingInternalTranslation();
  UnicodeString __fastcall AppliedLocaleCopyright();
  UnicodeString __fastcall AppliedLocaleVersion();
  TStoredSessionList * __fastcall SelectPuttySessionsForImport(
    const UnicodeString & RootKey, const UnicodeString & Source, TStoredSessionList * Sessions, UnicodeString & Error);
  bool __fastcall AnyPuttySessionForImport(TStoredSessionList * Sessions);

  __property bool ContinueOnError = { read = FContinueOnError, write = FContinueOnError };
  __property bool ConfirmCommandSession = { read = FConfirmCommandSession, write = FConfirmCommandSession };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = FSynchronizeParams };
  __property int SynchronizeOptions = { read = FSynchronizeOptions, write = FSynchronizeOptions };
  __property int SynchronizeModeAuto = { read = FSynchronizeModeAuto, write = FSynchronizeModeAuto };
  __property int SynchronizeMode = { read = FSynchronizeMode, write = FSynchronizeMode };
  __property int MaxWatchDirectories = { read = FMaxWatchDirectories, write = FMaxWatchDirectories };
  __property bool QueueBootstrap = { read = FQueueBootstrap, write = SetQueueBootstrap };
  __property bool QueueKeepDoneItems = { read = FQueueKeepDoneItems, write = SetQueueKeepDoneItems };
  __property int QueueKeepDoneItemsFor = { read = FQueueKeepDoneItemsFor, write = SetQueueKeepDoneItemsFor };
  __property bool QueueAutoPopup = { read = FQueueAutoPopup, write = FQueueAutoPopup };
  __property bool SessionRememberPassword = { read = FSessionRememberPassword, write = FSessionRememberPassword };
  __property LCID Locale = { read = GetLocale, write = SetLocale };
  __property LCID LocaleSafe = { read = GetLocale, write = SetLocaleSafe };
  __property UnicodeString AppliedLocaleHex = { read = GetAppliedLocaleHex };
  __property TObjectList * Locales = { read = GetLocales };
  __property UnicodeString PuttyPath = { read = FPuttyPath, write = FPuttyPath };
  __property TAutoSwitch UsePuttyPwFile = { read = FUsePuttyPwFile, write = FUsePuttyPwFile };
  __property UnicodeString DefaultPuttyPath = { read = FDefaultPuttyPath };
  __property bool PuttyPassword = { read = FPuttyPassword, write = FPuttyPassword };
  __property bool TelnetForFtpInPutty = { read = FTelnetForFtpInPutty, write = FTelnetForFtpInPutty };
  __property UnicodeString PuttySession = { read = FPuttySession, write = FPuttySession };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = FIgnoreCancelBeforeFinish };
  __property TGUICopyParamType DefaultCopyParam = { read = FDefaultCopyParam, write = SetDefaultCopyParam };
  __property bool BeepOnFinish = { read = FBeepOnFinish, write = FBeepOnFinish };
  __property TDateTime BeepOnFinishAfter = { read = FBeepOnFinishAfter, write = FBeepOnFinishAfter };
  __property UnicodeString BeepSound = { read = FBeepSound, write = FBeepSound };
  __property const TCopyParamList * CopyParamList = { read = GetCopyParamList, write = SetCopyParamList };
  __property UnicodeString CopyParamCurrent = { read = FCopyParamCurrent, write = SetCopyParamCurrent };
  __property int CopyParamIndex = { read = GetCopyParamIndex, write = SetCopyParamIndex };
  __property TGUICopyParamType CurrentCopyParam = { read = GetCurrentCopyParam };
  __property TGUICopyParamType CopyParamPreset[UnicodeString Name] = { read = GetCopyParamPreset };
  __property bool HasCopyParamPreset[UnicodeString Name] = { read = GetHasCopyParamPreset };
  __property TRemoteProperties NewDirectoryProperties = { read = FNewDirectoryProperties, write = SetNewDirectoryProperties };
  __property int KeepUpToDateChangeDelay = { read = FKeepUpToDateChangeDelay, write = FKeepUpToDateChangeDelay };
  __property UnicodeString ChecksumAlg = { read = FChecksumAlg, write = FChecksumAlg };
  __property int SessionReopenAutoIdle = { read = FSessionReopenAutoIdle, write = FSessionReopenAutoIdle };
  __property bool CanApplyLocaleImmediately = { read = GetCanApplyLocaleImmediately };
  __property LCID AppliedLocale = { read = FAppliedLocale };
};
//---------------------------------------------------------------------------
extern TGUIConfiguration * GUIConfiguration;
//---------------------------------------------------------------------------
#endif
