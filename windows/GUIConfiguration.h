//---------------------------------------------------------------------------
#ifndef GUIConfigurationH
#define GUIConfigurationH
//---------------------------------------------------------------------------
#include "Configuration.h"
#include "CopyParam.h"
//---------------------------------------------------------------------------
struct TPasLibModule;
class TGUIConfiguration;
enum TLogView { lvNone, lvWindow, pvPanel };
enum TInterface { ifCommander, ifExplorer };
//---------------------------------------------------------------------------
extern const ccLocal;
extern const ccShowResults;
extern const ccCopyResults;
extern const ccSet;
//---------------------------------------------------------------------------
const soRecurse =        0x01;
const soSynchronize =    0x02;
const soSynchronizeAsk = 0x04;
//---------------------------------------------------------------------------
class TGUICopyParamType : public TCopyParamType
{
public:
  __fastcall TGUICopyParamType();
  __fastcall TGUICopyParamType(const TCopyParamType & Source);
  __fastcall TGUICopyParamType(const TGUICopyParamType & Source);

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage);

  virtual void __fastcall Default();
  virtual void __fastcall Assign(const TCopyParamType * Source);
  TGUICopyParamType & __fastcall operator =(const TGUICopyParamType & rhp);
  TGUICopyParamType & __fastcall operator =(const TCopyParamType & rhp);

  __property bool Queue = { read = FQueue, write = FQueue };
  __property bool QueueNoConfirmation = { read = FQueueNoConfirmation, write = FQueueNoConfirmation };
  __property bool NewerOnly = { read = FNewerOnly, write = FNewerOnly };

protected:
  void __fastcall GUIDefault();
  void __fastcall GUIAssign(const TGUICopyParamType * Source);

private:
  bool FQueue;
  bool FQueueNoConfirmation;
  bool FNewerOnly;
};
//---------------------------------------------------------------------------
struct TCopyParamRuleData
{
  AnsiString HostName;
  AnsiString UserName;
  AnsiString RemoteDirectory;
  AnsiString LocalDirectory;

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

  AnsiString __fastcall GetInfoStr(AnsiString Separator) const;

  bool __fastcall operator ==(const TCopyParamRule & rhp) const;

  __property TCopyParamRuleData Data = { read = FData, write = FData };
  __property bool IsEmpty = { read = GetEmpty };

private:
  TCopyParamRuleData FData;

  inline bool __fastcall Match(const AnsiString & Mask,
    const AnsiString & Value, bool Path, bool Local = true) const;
  bool __fastcall GetEmpty() const;
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

  static void __fastcall ValidateName(const AnsiString Name);

  void __fastcall operator=(const TCopyParamList & rhl);
  bool __fastcall operator==(const TCopyParamList & rhl) const;

  void __fastcall Clear();
  void __fastcall Add(const AnsiString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Insert(int Index, const AnsiString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Change(int Index, const AnsiString Name,
    TCopyParamType * CopyParam, TCopyParamRule * Rule);
  void __fastcall Move(int CurIndex, int NewIndex);
  void __fastcall Delete(int Index);
  int __fastcall IndexOfName(const AnsiString Name) const;

  __property int Count = { read = GetCount };
  __property AnsiString Names[int Index] = { read = GetName };
  __property const TCopyParamRule * Rules[int Index] = { read = GetRule };
  __property const TCopyParamType * CopyParams[int Index] = { read = GetCopyParam };
  __property bool Modified = { read = FModified };
  __property TStrings * NameList = { read = GetNameList };
  __property bool AnyRule = { read = GetAnyRule };

private:
  static AnsiString FInvalidChars;
  TList * FRules;
  TList * FCopyParams;
  TStrings * FNames;
  mutable TStrings * FNameList;
  bool FModified;

  int __fastcall GetCount() const;
  const TCopyParamRule * __fastcall GetRule(int Index) const;
  const TCopyParamType * __fastcall GetCopyParam(int Index) const;
  AnsiString __fastcall GetName(int Index) const;
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
  bool FCopyParamDialogExpanded;
  bool FErrorDialogExpanded;
  TStrings * FLocales;
  AnsiString FLastLocalesExts;
  bool FContinueOnError;
  bool FConfirmCommandSession;
  AnsiString FPuttyPath;
  AnsiString FPSftpPath;
  bool FPuttyPassword;
  AnsiString FPuttySession;
  int FSynchronizeParams;
  int FSynchronizeOptions;
  int FSynchronizeModeAuto;
  int FSynchronizeMode;
  int FMaxWatchDirectories;
  TDateTime FIgnoreCancelBeforeFinish;
  bool FQueueAutoPopup;
  bool FQueueRememberPassword;
  int FQueueTransfersLimit;
  TGUICopyParamType FDefaultCopyParam;
  bool FBeepOnFinish;
  TDateTime FBeepOnFinishAfter;
  AnsiString FDefaultPuttyPathOnly;
  AnsiString FDefaultPuttyPath;
  bool FSynchronizeBrowsing;
  TCopyParamList * FCopyParamList;
  bool FCopyParamListDefaults;
  AnsiString FCopyParamCurrent;
  TRemoteProperties FNewDirectoryProperties;
  int FKeepUpToDateChangeDelay;
  AnsiString FChecksumAlg;

protected:
  LCID FLocale;

  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual LCID __fastcall GetLocale();
  void __fastcall SetLocale(LCID value);
  void __fastcall SetLocaleSafe(LCID value);
  virtual HINSTANCE __fastcall LoadNewResourceModule(LCID Locale,
    AnsiString * FileName = NULL);
  HINSTANCE __fastcall GetResourceModule();
  virtual void __fastcall SetResourceModule(HINSTANCE Instance);
  TStrings * __fastcall GetLocales();
  LCID __fastcall InternalLocale();
  void __fastcall FreeResourceModule(HINSTANCE Instance);
  void __fastcall SetDefaultCopyParam(const TGUICopyParamType & value);
  virtual bool __fastcall GetRememberPassword();
  const TCopyParamList * __fastcall GetCopyParamList();
  void __fastcall SetCopyParamList(const TCopyParamList * value);
  static AnsiString __fastcall PropertyToKey(const AnsiString Property);
  virtual void __fastcall DefaultLocalized();
  int __fastcall GetCopyParamIndex();
  TGUICopyParamType __fastcall GetCurrentCopyParam();
  TGUICopyParamType __fastcall GetCopyParamPreset(AnsiString Name);
  void __fastcall SetCopyParamIndex(int value);
  void __fastcall SetCopyParamCurrent(AnsiString value);
  void __fastcall SetNewDirectoryProperties(const TRemoteProperties & value);
  virtual void __fastcall Saved();

public:
  __fastcall TGUIConfiguration();
  virtual __fastcall ~TGUIConfiguration();
  virtual void __fastcall Default();

  HINSTANCE __fastcall ChangeResourceModule(HINSTANCE Instance);

  __property bool CopyParamDialogExpanded = { read = FCopyParamDialogExpanded, write = FCopyParamDialogExpanded };
  __property bool ErrorDialogExpanded = { read = FErrorDialogExpanded, write = FErrorDialogExpanded };
  __property bool ContinueOnError = { read = FContinueOnError, write = FContinueOnError };
  __property bool ConfirmCommandSession = { read = FConfirmCommandSession, write = FConfirmCommandSession };
  __property int SynchronizeParams = { read = FSynchronizeParams, write = FSynchronizeParams };
  __property int SynchronizeOptions = { read = FSynchronizeOptions, write = FSynchronizeOptions };
  __property int SynchronizeModeAuto = { read = FSynchronizeModeAuto, write = FSynchronizeModeAuto };
  __property int SynchronizeMode = { read = FSynchronizeMode, write = FSynchronizeMode };
  __property int MaxWatchDirectories = { read = FMaxWatchDirectories, write = FMaxWatchDirectories };
  __property int QueueTransfersLimit = { read = FQueueTransfersLimit, write = FQueueTransfersLimit };
  __property bool QueueAutoPopup = { read = FQueueAutoPopup, write = FQueueAutoPopup };
  __property bool QueueRememberPassword = { read = FQueueRememberPassword, write = FQueueRememberPassword };
  __property LCID Locale = { read = GetLocale, write = SetLocale };
  __property LCID LocaleSafe = { read = GetLocale, write = SetLocaleSafe };
  __property TStrings * Locales = { read = GetLocales };
  __property AnsiString PuttyPath = { read = FPuttyPath, write = FPuttyPath };
  __property AnsiString DefaultPuttyPath = { read = FDefaultPuttyPath };
  __property AnsiString PSftpPath = { read = FPSftpPath, write = FPSftpPath };
  __property bool PuttyPassword = { read = FPuttyPassword, write = FPuttyPassword };
  __property AnsiString PuttySession = { read = FPuttySession, write = FPuttySession };
  __property TDateTime IgnoreCancelBeforeFinish = { read = FIgnoreCancelBeforeFinish, write = FIgnoreCancelBeforeFinish };
  __property TGUICopyParamType DefaultCopyParam = { read = FDefaultCopyParam, write = SetDefaultCopyParam };
  __property bool BeepOnFinish = { read = FBeepOnFinish, write = FBeepOnFinish };
  __property bool SynchronizeBrowsing = { read = FSynchronizeBrowsing, write = FSynchronizeBrowsing };
  __property TDateTime BeepOnFinishAfter = { read = FBeepOnFinishAfter, write = FBeepOnFinishAfter };
  __property const TCopyParamList * CopyParamList = { read = GetCopyParamList, write = SetCopyParamList };
  __property AnsiString CopyParamCurrent = { read = FCopyParamCurrent, write = SetCopyParamCurrent };
  __property int CopyParamIndex = { read = GetCopyParamIndex, write = SetCopyParamIndex };
  __property TGUICopyParamType CurrentCopyParam = { read = GetCurrentCopyParam };
  __property TGUICopyParamType CopyParamPreset[AnsiString Name] = { read = GetCopyParamPreset };
  __property TRemoteProperties NewDirectoryProperties = { read = FNewDirectoryProperties, write = SetNewDirectoryProperties };
  __property int KeepUpToDateChangeDelay = { read = FKeepUpToDateChangeDelay, write = FKeepUpToDateChangeDelay };
  __property AnsiString ChecksumAlg = { read = FChecksumAlg, write = FChecksumAlg };
};
//---------------------------------------------------------------------------
#define GUIConfiguration (dynamic_cast<TGUIConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
