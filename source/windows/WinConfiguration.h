//---------------------------------------------------------------------------
#ifndef WinConfigurationH
#define WinConfigurationH
//---------------------------------------------------------------------------
#include "CustomWinConfiguration.h"
#include "CustomDirView.hpp"
#include "FileInfo.h"
//---------------------------------------------------------------------------
enum TEditor { edInternal, edExternal, edOpen };
enum TGenerateUrlCodeTarget { guctUrl, guctScript, guctAssembly };
enum TScriptFormat { sfScriptFile, sfBatchFile, sfCommandLine, sfPowerShell };
enum TLocaleFlagOverride { lfoLanguageIfRecommended, lfoLanguage, lfoAlways, lfoNever };
//---------------------------------------------------------------------------
#define C(Property) (Property != rhc.Property) ||
struct TScpExplorerConfiguration {
  UnicodeString WindowParams;
  UnicodeString DirViewParams;
  UnicodeString ToolbarsLayout;
  UnicodeString ToolbarsButtons;
  bool SessionsTabs;
  bool StatusBar;
  UnicodeString LastLocalTargetDirectory;
  int ViewStyle;
  bool ShowFullAddress;
  bool DriveView;
  int DriveViewWidth;
  int DriveViewWidthPixelsPerInch;
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(ToolbarsLayout) C(ToolbarsButtons)
        C(SessionsTabs) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress)
        C(DriveView) C(DriveViewWidth) C(DriveViewWidthPixelsPerInch) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderPanelConfiguration {
  UnicodeString DirViewParams;
  int ViewStyle;
  bool StatusBar;
  bool DriveView;
  int DriveViewHeight;
  int DriveViewHeightPixelsPerInch;
  int DriveViewWidth;
  int DriveViewWidthPixelsPerInch;
  UnicodeString LastPath;
  bool __fastcall operator !=(TScpCommanderPanelConfiguration & rhc)
    { return C(DirViewParams) C(ViewStyle) C(StatusBar)
        C(DriveView) C(DriveViewHeight) C(DriveViewHeightPixelsPerInch)
        C(DriveViewWidth) C(DriveViewWidthPixelsPerInch) C(LastPath) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderConfiguration {
  UnicodeString WindowParams;
  double LocalPanelWidth;
  UnicodeString ToolbarsLayout;
  UnicodeString ToolbarsButtons;
  bool SessionsTabs;
  bool StatusBar;
  TOperationSide CurrentPanel;
  TNortonLikeMode NortonLikeMode;
  bool PreserveLocalDirectory;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool CompareByTime;
  bool CompareBySize;
  bool SwappedPanels;
  bool TreeOnLeft;
  bool ExplorerKeyboardShortcuts;
  bool SystemContextMenu;
  UnicodeString OtherLocalPanelDirViewParams;
  int OtherLocalPanelViewStyle;
  UnicodeString OtherLocalPanelLastPath;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(ToolbarsLayout) C(ToolbarsButtons)
      C(SessionsTabs) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel)
      C(NortonLikeMode) C(PreserveLocalDirectory)
      C(CompareBySize) C(CompareByTime) C(SwappedPanels)
      C(TreeOnLeft) C(ExplorerKeyboardShortcuts) C(SystemContextMenu)
      C(OtherLocalPanelDirViewParams) C(OtherLocalPanelViewStyle) C(OtherLocalPanelLastPath) 0; };

  TCompareCriterias __fastcall CompareCriterias()
  {
    TCompareCriterias Criterias;
    if (CompareByTime)
    {
      Criterias << ccTime;
    }
    if (CompareBySize)
    {
      Criterias << ccSize;
    }
    return Criterias;
  }
};
//---------------------------------------------------------------------------
struct TFontConfiguration
{
  UnicodeString FontName;
  int FontSize;
  int FontCharset;
  int FontStyle;

  __fastcall TFontConfiguration()
  {
    FontSize = 0;
    FontCharset = DEFAULT_CHARSET;
    FontStyle = 0;
  }

  // keep in sync with SameFont
  bool __fastcall operator !=(const TFontConfiguration & rhc)
    { return !SameText(FontName, rhc.FontName) || C(FontSize)
      C(FontCharset) C(FontStyle) 0; };
};
//---------------------------------------------------------------------------
struct TEditorConfiguration {
  TFontConfiguration Font;
  TColor FontColor;
  TColor BackgroundColor;
  bool WordWrap;
  UnicodeString FindText;
  UnicodeString ReplaceText;
  bool FindMatchCase;
  bool FindWholeWord;
  bool FindDown;
  unsigned int TabSize;
  unsigned int MaxEditors;
  unsigned int EarlyClose;
  bool SDIShellEditor;
  UnicodeString WindowParams;
  int Encoding;
  bool WarnOnEncodingFallback;
  bool WarnOrLargeFileSize;
  bool AutoFont;
  bool DisableSmoothScroll;
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(Font) C(FontColor) C(BackgroundColor) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) C(FindDown) C(TabSize)
      C(MaxEditors) C(EarlyClose) C(SDIShellEditor) C(WindowParams)
      C(Encoding) C(WarnOnEncodingFallback) C(WarnOrLargeFileSize) C(AutoFont) C(DisableSmoothScroll) 0; };
};
//---------------------------------------------------------------------------
enum TQueueViewShow { qvShow, qvHideWhenEmpty, qvHide };
struct TQueueViewConfiguration {
  int Height;
  int HeightPixelsPerInch;
  UnicodeString Layout;
  TQueueViewShow Show;
  TQueueViewShow LastHideShow;
  bool ToolBar;
  bool Label;
  bool FileList;
  int FileListHeight;
  int FileListHeightPixelsPerInch;
  bool __fastcall operator !=(TQueueViewConfiguration & rhc)
    { return C(Height) C(HeightPixelsPerInch) C(Layout) C(Show) C(LastHideShow) C(ToolBar) C(Label)
        C(FileList) C(FileListHeight) C(FileListHeightPixelsPerInch) 0; };
};
//---------------------------------------------------------------------------
struct TUpdatesData
{
  int ForVersion;
  int Version;
  UnicodeString Message;
  bool Critical;
  UnicodeString Release;
  bool Disabled;
  UnicodeString Url;
  UnicodeString UrlButton;
  UnicodeString NewsUrl;
  TSize NewsSize;
  UnicodeString DownloadUrl;
  __int64 DownloadSize;
  UnicodeString DownloadSha256;
  UnicodeString AuthenticationError;
  bool OpenGettingStarted;
  UnicodeString DownloadingUrl;
  TSize TipsSize;
  UnicodeString TipsUrl;
  UnicodeString Tips;
  int TipsIntervalDays;
  int TipsIntervalRuns;
  bool __fastcall operator !=(TUpdatesData & rhc)
    { return C(ForVersion) C(Version) C(Message) C(Critical) C(Release)
             C(Disabled) C(Url) C(UrlButton) C(NewsUrl) C(NewsSize)
             C(DownloadUrl) C(DownloadSize) C(DownloadSha256) C(AuthenticationError)
             C(OpenGettingStarted) C(DownloadingUrl)
             C(TipsSize) C(TipsUrl) C(Tips) C(TipsIntervalDays) C(TipsIntervalRuns) 0; };
  void Reset()
  {
    ForVersion = 0;
    Version = 0;
    Message = L"";
    Critical = false;
    Release = L"";
    Disabled = false;
    Url = L"";
    UrlButton = L"";
    NewsUrl = L"";
    NewsSize = TSize();
    DownloadUrl = L"";
    DownloadSize = 0;
    DownloadSha256 = L"";
    AuthenticationError = L"";
    OpenGettingStarted = false;
    DownloadingUrl = L"";
    TipsSize = TSize();
    TipsUrl = L"";
    Tips = L"";
    TipsIntervalDays = 7;
    TipsIntervalRuns = 5;
  }
};
//---------------------------------------------------------------------------
enum TConnectionType { ctDirect, ctAuto, ctProxy };
extern TDateTime DefaultUpdatesPeriod;
extern const UnicodeString ScpExplorerDirViewParamsDefault;
extern const UnicodeString ScpCommanderRemotePanelDirViewParamsDefault;
extern const UnicodeString ScpCommanderLocalPanelDirViewParamsDefault;
extern UnicodeString QueueViewLayoutDefault;
extern UnicodeString ScpCommanderWindowParamsDefault;
extern UnicodeString ScpExplorerWindowParamsDefault;
//---------------------------------------------------------------------------
struct TUpdatesConfiguration
{
  TDateTime Period;
  TDateTime LastCheck;
  TConnectionType ConnectionType;
  UnicodeString ProxyHost;
  int ProxyPort;
  TAutoSwitch BetaVersions;
  bool ShowOnStartup;
  UnicodeString AuthenticationEmail;
  UnicodeString Mode;
  bool HaveResults;
  bool ShownResults;
  UnicodeString DotNetVersion;
  UnicodeString ConsoleVersion;
  TUpdatesData Results;

  bool __fastcall operator !=(TUpdatesConfiguration & rhc)
    { return C(Period) C(LastCheck) C(ConnectionType) C(ProxyHost) C(ProxyPort)
        C(BetaVersions) C(ShowOnStartup) C(AuthenticationEmail) C(Mode)
        C(HaveResults) C(ShownResults) C(DotNetVersion)
        C(ConsoleVersion) C(Results)  0; };

  bool __fastcall HaveValidResultsForVersion(int CompoundVersion)
  {
    return
      HaveResults &&
      (double(Period) > 0) &&
      (ZeroBuildNumber(Results.ForVersion) == CompoundVersion);
  }
};
//---------------------------------------------------------------------------
struct TEditorData
{
  __fastcall TEditorData();
  __fastcall TEditorData(const TEditorData & Source);

  TFileMasks FileMask;
  TEditor Editor;
  UnicodeString ExternalEditor;
  bool ExternalEditorText;
  bool SDIExternalEditor;
  bool DetectMDIExternalEditor;

  bool __fastcall operator ==(const TEditorData & rhd) const;
  void __fastcall ExternalEditorOptionsAutodetect();
};
//---------------------------------------------------------------------------
struct TFileColorData
{
  TFileColorData();

  TFileMasks FileMask;
  TColor Color;

  void Load(const UnicodeString & S);
  UnicodeString Save() const;
  typedef std::vector<TFileColorData> TList;
  static void LoadList(const UnicodeString & S, TList & List);
  static UnicodeString SaveList(const TList & List);
};
//---------------------------------------------------------------------------
#undef C
//---------------------------------------------------------------------------
class TEditorPreferences
{
public:
  __fastcall TEditorPreferences();
  __fastcall TEditorPreferences(const TEditorData & Data);
  bool __fastcall Matches(const UnicodeString FileName, bool Local,
    const TFileMasks::TParams & Params) const;
  void __fastcall Load(THierarchicalStorage * Storage, bool Legacy);
  void __fastcall Save(THierarchicalStorage * Storage) const;
  void __fastcall LegacyDefaults();
  UnicodeString __fastcall ExtractExternalEditorName() const;

  static UnicodeString __fastcall GetDefaultExternalEditor();

  bool __fastcall operator ==(const TEditorPreferences & rhp) const;

  __property const TEditorData * Data = { read = GetConstData };
  __property UnicodeString Name = { read = GetName };

  TEditorData * __fastcall GetData();

private:
  TEditorData FData;
  mutable UnicodeString FName;

  UnicodeString __fastcall GetName() const;
  const TEditorData * __fastcall GetConstData() const { return &FData; };
};
//---------------------------------------------------------------------------
class TEditorList
{
public:
  __fastcall TEditorList();
  virtual __fastcall ~TEditorList();

  const TEditorPreferences * __fastcall Find(const UnicodeString FileName,
    bool Local, const TFileMasks::TParams & Params) const;

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;

  TEditorList & __fastcall operator=(const TEditorList & rhl);
  bool __fastcall operator==(const TEditorList & rhl) const;

  void __fastcall Clear();
  void __fastcall Add(TEditorPreferences * Editor);
  void __fastcall Insert(int Index, TEditorPreferences * Editor);
  void __fastcall Change(int Index, TEditorPreferences * Editor);
  void __fastcall Move(int CurIndex, int NewIndex);
  void __fastcall Delete(int Index);
  void __fastcall Saved();

  bool __fastcall IsDefaultList() const;

  __property int Count = { read = GetCount };
  __property const TEditorPreferences * Editors[int Index] = { read = GetEditor };
  __property bool Modified = { read = FModified };

private:
  TList * FEditors;
  bool FModified;

  int __fastcall GetCount() const;

  void __fastcall Init();
  void __fastcall Modify();
  const TEditorPreferences * __fastcall GetEditor(int Index) const;
};
//---------------------------------------------------------------------------
class TBookmarks;
class TBookmarkList;
class TCustomCommandList;
enum TPathInCaption { picShort, picFull, picNone };
enum TSessionTabNameFormat { stnfNone, stnfShortPath, stnfShortPathTrunc };
// constants must be compatible with legacy CopyOnDoubleClick
enum TDoubleClickAction { dcaOpen = 0, dcaCopy = 1, dcaEdit = 2 };
enum TResolvedDoubleClickAction { rdcaNone, rdcaChangeDir, rdcaOpen, rdcaCopy, rdcaEdit };
enum TStoreTransition { stInit, stStandard, stStoreFresh, stStoreMigrated, stStoreAcknowledged };
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TMasterPasswordPromptEvent)();
//---------------------------------------------------------------------------
class TWinConfiguration : public TCustomWinConfiguration
{
private:
  UnicodeString FAutoStartSession;
  TDoubleClickAction FDoubleClickAction;
  bool FCopyOnDoubleClickConfirmation;
  bool FAlwaysRespectDoubleClickAction;
  bool FDDDisableMove;
  TAutoSwitch FDDTransferConfirmation;
  bool FDeleteToRecycleBin;
  bool FDimmHiddenFiles;
  bool FRenameWholeName;
  TScpCommanderConfiguration FScpCommander;
  TScpExplorerConfiguration FScpExplorer;
  bool FSelectDirectories;
  UnicodeString FSelectMask;
  bool FShowHiddenFiles;
  TFormatBytesStyle FFormatSizeBytes;
  TIncrementalSearch FPanelSearch;
  bool FShowInaccesibleDirectories;
  bool FConfirmTransferring;
  bool FConfirmDeleting;
  bool FConfirmRecycling;
  bool FUseLocationProfiles;
  bool FUseSharedBookmarks;
  UnicodeString FDDTemporaryDirectory;
  UnicodeString FDDDrives;
  bool FDDWarnLackOfTempSpace;
  bool FDDFakeFile;
  int FDDExtInstalled;
  int FDDExtTimeout;
  bool FConfirmClosingSession;
  double FDDWarnLackOfTempSpaceRatio;
  UnicodeString FTemporarySessionFile;
  UnicodeString FTemporaryKeyFile;
  TBookmarks * FBookmarks;
  TCustomCommandList * FCustomCommandList;
  TCustomCommandList * FExtensionList;
  UnicodeString FExtensionsDeleted;
  UnicodeString FExtensionsOrder;
  UnicodeString FExtensionsShortCuts;
  bool FCustomCommandsDefaults;
  TEditorConfiguration FEditor;
  TQueueViewConfiguration FQueueView;
  bool FEnableQueueByDefault;
  bool FEmbeddedSessions;
  bool FExpertMode;
  bool FDisableOpenEdit;
  bool FDefaultDirIsHome;
  int FDDDeleteDelay;
  bool FTemporaryDirectoryAppendSession;
  bool FTemporaryDirectoryAppendPath;
  bool FTemporaryDirectoryDeterministic;
  bool FTemporaryDirectoryCleanup;
  bool FConfirmTemporaryDirectoryCleanup;
  UnicodeString FDefaultTranslationFile;
  UnicodeString FInvalidDefaultTranslationMessage;
  bool FPreservePanelState;
  TAutoSwitch FDarkTheme;
  int FSysDarkTheme;
  UnicodeString FLastStoredSession;
  UnicodeString FLastWorkspace;
  bool FAutoSaveWorkspace;
  bool FAutoSaveWorkspacePasswords;
  UnicodeString FAutoWorkspace;
  TPathInCaption FPathInCaption;
  TSessionTabNameFormat FSessionTabNameFormat;
  bool FMinimizeToTray;
  bool FBalloonNotifications;
  unsigned int FNotificationsTimeout;
  unsigned int FNotificationsStickTime;
  TUpdatesConfiguration FUpdates;
  UnicodeString FVersionHistory;
  bool FCopyParamAutoSelectNotice;
  bool FLockToolbars;
  bool FSelectiveToolbarText;
  int FLargerToolbar;
  TEditorList * FEditorList;
  TEditorPreferences * FLegacyEditor;
  UnicodeString FDefaultKeyFile;
  bool FAutoOpenInPutty;
  TDateTime FDefaultUpdatesPeriod;
  bool FRefreshRemotePanel;
  TDateTime FRefreshRemotePanelInterval;
  TFontConfiguration FPanelFont;
  bool FNaturalOrderNumericalSorting;
  bool FAlwaysSortDirectoriesByName;
  bool FFullRowSelect;
  bool FOfferedEditorAutoConfig;
  bool FUseMasterPassword;
  UnicodeString FPlainMasterPasswordEncrypt;
  UnicodeString FPlainMasterPasswordDecrypt;
  UnicodeString FMasterPasswordVerifier;
  TMasterPasswordPromptEvent FOnMasterPasswordPrompt;
  UnicodeString FOpenedStoredSessionFolders;
  bool FAutoImportedFromPuttyOrFilezilla;
  int FGenerateUrlComponents;
  TGenerateUrlCodeTarget FGenerateUrlCodeTarget;
  TScriptFormat FGenerateUrlScriptFormat;
  TAssemblyLanguage FGenerateUrlAssemblyLanguage;
  bool FExternalSessionInExistingInstance;
  bool FShowLoginWhenNoSession;
  bool FKeepOpenWhenNoSession;
  bool FDefaultToNewRemoteTab;
  bool FLocalIconsByExt;
  bool FFlashTaskbar;
  int FMaxSessions;
  TLocaleFlagOverride FBidiModeOverride;
  TLocaleFlagOverride FFlipChildrenOverride;
  bool FShowTips;
  UnicodeString FTipsSeen;
  TDateTime FTipsShown;
  UnicodeString FFileColors;
  int FRunsSinceLastTip;
  bool FLockedInterface;
  bool FTimeoutShellIconRetrieval;
  bool FUseIconUpdateThread;
  bool FAllowWindowPrint;
  TStoreTransition FStoreTransition;
  int FQueueTransferLimitMax;
  bool FHiContrast;
  bool FEditorCheckNotModified;
  bool FSessionTabCaptionTruncation;
  UnicodeString FRemoteThumbnailMask;
  int FRemoteThumbnailSizeLimit;
  UnicodeString FFirstRun;
  int FDontDecryptPasswords;
  int FMasterPasswordSession;
  bool FMasterPasswordSessionAsked;
  std::unique_ptr<TStringList> FCustomCommandOptions;
  bool FCustomCommandOptionsModified;
  int FLastMachineInstallations;
  __property int LastMachineInstallations = { read = FLastMachineInstallations, write = FLastMachineInstallations };
  int FMachineInstallations;
  LCID FDefaultLocale;
  std::unique_ptr<TStrings> FExtensionTranslations;

  void __fastcall SetDoubleClickAction(TDoubleClickAction value);
  void __fastcall SetCopyOnDoubleClickConfirmation(bool value);
  void __fastcall SetAlwaysRespectDoubleClickAction(bool value);
  void __fastcall SetDDDisableMove(bool value);
  void __fastcall SetDDTransferConfirmation(TAutoSwitch value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetDimmHiddenFiles(bool value);
  void __fastcall SetRenameWholeName(bool value);
  void __fastcall SetScpCommander(TScpCommanderConfiguration value);
  void __fastcall SetScpExplorer(TScpExplorerConfiguration value);
  void __fastcall SetSelectDirectories(bool value);
  void __fastcall SetShowHiddenFiles(bool value);
  void __fastcall SetFormatSizeBytes(TFormatBytesStyle value);
  void __fastcall SetPanelSearch(TIncrementalSearch value);
  void __fastcall SetShowInaccesibleDirectories(bool value);
  void __fastcall SetConfirmTransferring(bool value);
  void __fastcall SetConfirmDeleting(bool value);
  void __fastcall SetConfirmRecycling(bool value);
  void __fastcall SetUseLocationProfiles(bool value);
  void __fastcall SetUseSharedBookmarks(bool value);
  void __fastcall SetDDTemporaryDirectory(UnicodeString value);
  void __fastcall SetDDDrives(UnicodeString value);
  void __fastcall SetDDWarnLackOfTempSpace(bool value);
  void __fastcall SetDDFakeFile(bool value);
  void __fastcall SetDDExtTimeout(int value);
  void __fastcall SetConfirmClosingSession(bool value);
  void __fastcall SetDDWarnLackOfTempSpaceRatio(double value);
  void __fastcall SetBookmarks(UnicodeString Key, TBookmarkList * value);
  TBookmarkList * __fastcall GetBookmarks(UnicodeString Key);
  void __fastcall SetSharedBookmarks(TBookmarkList * value);
  TBookmarkList * __fastcall GetSharedBookmarks();
  void __fastcall SetAutoStartSession(UnicodeString value);
  void __fastcall SetExpertMode(bool value);
  void __fastcall SetDefaultDirIsHome(bool value);
  void __fastcall SetEditor(TEditorConfiguration value);
  void __fastcall SetQueueView(TQueueViewConfiguration value);
  void __fastcall SetEnableQueueByDefault(bool value);
  void __fastcall SetCustomCommandList(TCustomCommandList * value);
  void __fastcall SetExtensionList(TCustomCommandList * value);
  void __fastcall SetTemporaryDirectoryAppendSession(bool value);
  void __fastcall SetTemporaryDirectoryAppendPath(bool value);
  void __fastcall SetTemporaryDirectoryDeterministic(bool value);
  void __fastcall SetTemporaryDirectoryCleanup(bool value);
  void __fastcall SetConfirmTemporaryDirectoryCleanup(bool value);
  void __fastcall SetPreservePanelState(bool value);
  void __fastcall SetDarkTheme(TAutoSwitch value);
  void __fastcall SetLastStoredSession(UnicodeString value);
  void __fastcall SetAutoSaveWorkspace(bool value);
  void __fastcall SetAutoSaveWorkspacePasswords(bool value);
  void __fastcall SetAutoWorkspace(UnicodeString value);
  void __fastcall SetPathInCaption(TPathInCaption value);
  void __fastcall SetSessionTabNameFormat(TSessionTabNameFormat value);
  void __fastcall SetMinimizeToTray(bool value);
  void __fastcall SetBalloonNotifications(bool value);
  void __fastcall SetNotificationsTimeout(unsigned int value);
  void __fastcall SetNotificationsStickTime(unsigned int value);
  void __fastcall SetCopyParamAutoSelectNotice(bool value);
  TUpdatesConfiguration __fastcall GetUpdates();
  void __fastcall SetUpdates(TUpdatesConfiguration value);
  void __fastcall SetVersionHistory(UnicodeString value);
  void __fastcall SetLockToolbars(bool value);
  void __fastcall SetSelectiveToolbarText(bool value);
  void SetLargerToolbar(int value);
  const TEditorList * __fastcall GetEditorList();
  void __fastcall SetEditorList(const TEditorList * value);
  void __fastcall SetAutoOpenInPutty(bool value);
  void __fastcall SetRefreshRemotePanel(bool value);
  void __fastcall SetRefreshRemotePanelInterval(TDateTime value);
  void __fastcall SetPanelFont(const TFontConfiguration & value);
  void __fastcall SetNaturalOrderNumericalSorting(bool value);
  void __fastcall SetAlwaysSortDirectoriesByName(bool value);
  void __fastcall SetFullRowSelect(bool value);
  void __fastcall SetOfferedEditorAutoConfig(bool value);
  void __fastcall SetLastMonitor(int value);
  int __fastcall GetLastMonitor();
  void __fastcall SetOpenedStoredSessionFolders(UnicodeString value);
  void __fastcall SetAutoImportedFromPuttyOrFilezilla(bool value);
  void __fastcall SetGenerateUrlComponents(int value);
  void __fastcall SetGenerateUrlCodeTarget(TGenerateUrlCodeTarget value);
  void __fastcall SetGenerateUrlScriptFormat(TScriptFormat value);
  void __fastcall SetGenerateUrlAssemblyLanguage(TAssemblyLanguage value);
  void __fastcall SetExternalSessionInExistingInstance(bool value);
  void __fastcall SetShowLoginWhenNoSession(bool value);
  void __fastcall SetKeepOpenWhenNoSession(bool value);
  void __fastcall SetDefaultToNewRemoteTab(bool value);
  void __fastcall SetLocalIconsByExt(bool value);
  void __fastcall SetFlashTaskbar(bool value);
  void __fastcall SetBidiModeOverride(TLocaleFlagOverride value);
  void __fastcall SetFlipChildrenOverride(TLocaleFlagOverride value);
  void __fastcall SetShowTips(bool value);
  void __fastcall SetTipsSeen(UnicodeString value);
  void __fastcall SetTipsShown(TDateTime value);
  void __fastcall SetFileColors(UnicodeString value);
  void __fastcall SetRunsSinceLastTip(int value);
  int __fastcall GetHonorDrivePolicy();
  void __fastcall SetHonorDrivePolicy(int value);
  bool __fastcall GetUseABDrives();
  void __fastcall SetUseABDrives(bool value);
  bool __fastcall GetIsBeta();
  TStrings * __fastcall GetCustomCommandOptions();
  void __fastcall SetCustomCommandOptions(TStrings * value);
  void __fastcall SetLockedInterface(bool value);
  bool __fastcall GetTimeoutShellOperations();
  void __fastcall SetTimeoutShellOperations(bool value);
  void __fastcall SetTimeoutShellIconRetrieval(bool value);
  void __fastcall SetUseIconUpdateThread(bool value);
  void __fastcall SetAllowWindowPrint(bool value);
  void SetStoreTransition(TStoreTransition value);
  void SetQueueTransferLimitMax(int value);
  void SetHiContrast(bool value);
  void SetEditorCheckNotModified(bool value);
  void SetSessionTabCaptionTruncation(bool value);
  void SetLoadingTooLongLimit(int value);
  int GetLoadingTooLongLimit();
  void SetFirstRun(const UnicodeString & value);
  int __fastcall GetLocaleCompletenessTreshold();

  bool __fastcall GetDDExtInstalled();
  void __fastcall AddVersionToHistory();
  bool __fastcall GetAnyBetaInVersionHistory();
  void __fastcall PurgePassword(UnicodeString & Password);
  void __fastcall UpdateEntryInJumpList(
    bool Session, const UnicodeString & Name, bool Add);
  TStringList * __fastcall LoadJumpList(THierarchicalStorage * Storage,
    UnicodeString Name);
  void __fastcall SaveJumpList(THierarchicalStorage * Storage,
    UnicodeString Name, TStringList * List);
  void __fastcall TrimJumpList(TStringList * List);
  void __fastcall UpdateIconFont();

protected:
  virtual TStorage __fastcall GetStorage();
  bool DetectStorage(bool SafeOnly);
  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual void __fastcall LoadFrom(THierarchicalStorage * Storage);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual void __fastcall CopyData(THierarchicalStorage * Source, THierarchicalStorage * Target);
  virtual UnicodeString __fastcall GetDefaultKeyFile();
  virtual void __fastcall Saved();
  void __fastcall RecryptPasswords(TStrings * RecryptPasswordErrors);
  virtual bool __fastcall GetUseMasterPassword();
  bool __fastcall SameStringLists(TStrings * Strings1, TStrings * Strings2);
  virtual HINSTANCE __fastcall LoadNewResourceModule(LCID Locale,
    UnicodeString & FileName);
  void __fastcall CheckTranslationVersion(const UnicodeString FileName,
    bool InternalLocaleOnError);
  virtual void __fastcall DefaultLocalized();
  bool __fastcall DetectRegistryStorage(HKEY RootKey);
  bool __fastcall CanWriteToStorage();
  bool __fastcall DoIsBeta(const UnicodeString & ReleaseType);
  void __fastcall AskForMasterPassword();
  void __fastcall DoLoadExtensionList(const UnicodeString & Path, const UnicodeString & PathId, TStringList * DeletedExtensions);
  TStrings * __fastcall GetExtensionsPaths();
  virtual int __fastcall GetResourceModuleCompleteness(HINSTANCE Module);
  virtual bool __fastcall IsTranslationComplete(HINSTANCE Module);
  void __fastcall LoadExtensionList();
  void __fastcall ReleaseExtensionTranslations();
  void __fastcall LoadExtensionTranslations();
  TStrings * __fastcall DoFindTemporaryFolders(bool OnlyFirst);

public:
  __fastcall TWinConfiguration();
  virtual __fastcall ~TWinConfiguration();
  virtual void __fastcall Default();
  void __fastcall ClearTemporaryLoginData();
  virtual THierarchicalStorage * CreateScpStorage(bool & SessionList);
  virtual UnicodeString TemporaryDir(bool Mask = false);
  TStrings * __fastcall FindTemporaryFolders();
  bool __fastcall AnyTemporaryFolders();
  void __fastcall CleanupTemporaryFolders();
  void __fastcall CleanupTemporaryFolders(TStrings * Folders = NULL);
  UnicodeString __fastcall ExpandedTemporaryDirectory();
  void __fastcall CheckDefaultTranslation();
  const TEditorPreferences * __fastcall DefaultEditorForFile(
    const UnicodeString FileName, bool Local, const TFileMasks::TParams & MaskParams);
  virtual UnicodeString __fastcall DecryptPassword(RawByteString Password, UnicodeString Key);
  virtual RawByteString __fastcall StronglyRecryptPassword(RawByteString Password, UnicodeString Key);
  void __fastcall SetMasterPassword(UnicodeString value);
  void __fastcall ChangeMasterPassword(UnicodeString value, TStrings * RecryptPasswordErrors);
  bool __fastcall ValidateMasterPassword(UnicodeString value);
  void __fastcall ClearMasterPassword(TStrings * RecryptPasswordErrors);
  void __fastcall BeginMasterPasswordSession();
  void __fastcall EndMasterPasswordSession();
  virtual void __fastcall AskForMasterPasswordIfNotSet();
  void __fastcall AddSessionToJumpList(UnicodeString SessionName);
  void __fastcall DeleteSessionFromJumpList(UnicodeString SessionName);
  void __fastcall AddWorkspaceToJumpList(UnicodeString Workspace);
  void __fastcall DeleteWorkspaceFromJumpList(UnicodeString Workspace);
  void __fastcall UpdateJumpList();
  virtual void __fastcall UpdateStaticUsage();
  void __fastcall CustomCommandShortCuts(TShortCuts & ShortCuts) const;
  UnicodeString __fastcall GetUserExtensionsPath();
  UnicodeString __fastcall GetExtensionId(const UnicodeString & ExtensionPath);
  UnicodeString __fastcall ExtensionStringTranslation(const UnicodeString & ExtensionId, const UnicodeString & S);
  UnicodeString __fastcall UniqueExtensionName(const UnicodeString & ExtensionName, int Counter);
  UnicodeString __fastcall GetProvisionaryExtensionId(const UnicodeString & FileName);
  bool __fastcall IsDDExtRunning();
  bool __fastcall IsDDExtBroken();
  bool __fastcall UseDarkTheme();
  TResolvedDoubleClickAction ResolveDoubleClickAction(bool IsDirectory, TTerminal * Terminal);
  bool TrySetSafeStorage();

  static void __fastcall RestoreFont(const TFontConfiguration & Configuration, TFont * Font);
  static void __fastcall StoreFont(TFont * Font, TFontConfiguration & Configuration);

  __property TScpCommanderConfiguration ScpCommander = { read = FScpCommander, write = SetScpCommander };
  __property TScpExplorerConfiguration ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool SelectDirectories = { read = FSelectDirectories, write = SetSelectDirectories };
  __property UnicodeString SelectMask = { read = FSelectMask, write = FSelectMask };
  __property bool ShowHiddenFiles = { read = FShowHiddenFiles, write = SetShowHiddenFiles };
  __property TFormatBytesStyle FormatSizeBytes = { read = FFormatSizeBytes, write = SetFormatSizeBytes };
  __property TIncrementalSearch PanelSearch = { read = FPanelSearch, write = SetPanelSearch };
  __property bool ShowInaccesibleDirectories = { read = FShowInaccesibleDirectories, write = SetShowInaccesibleDirectories };
  __property TEditorConfiguration Editor = { read = FEditor, write = SetEditor };
  __property TQueueViewConfiguration QueueView = { read = FQueueView, write = SetQueueView };
  __property bool EnableQueueByDefault = { read = FEnableQueueByDefault, write = SetEnableQueueByDefault };
  __property TUpdatesConfiguration Updates = { read = GetUpdates, write = SetUpdates };
  __property UnicodeString VersionHistory = { read = FVersionHistory, write = SetVersionHistory };
  __property bool AnyBetaInVersionHistory = { read = GetAnyBetaInVersionHistory };
  __property bool IsBeta = { read = GetIsBeta };
  __property UnicodeString AutoStartSession = { read = FAutoStartSession, write = SetAutoStartSession };
  __property TDoubleClickAction DoubleClickAction = { read = FDoubleClickAction, write = SetDoubleClickAction };
  __property bool CopyOnDoubleClickConfirmation = { read = FCopyOnDoubleClickConfirmation, write = SetCopyOnDoubleClickConfirmation };
  __property bool AlwaysRespectDoubleClickAction = { read = FAlwaysRespectDoubleClickAction, write = SetAlwaysRespectDoubleClickAction };
  __property bool DDDisableMove = { read = FDDDisableMove, write = SetDDDisableMove };
  __property TAutoSwitch DDTransferConfirmation = { read = FDDTransferConfirmation, write = SetDDTransferConfirmation };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool DimmHiddenFiles = { read = FDimmHiddenFiles, write = SetDimmHiddenFiles };
  __property bool RenameWholeName = { read = FRenameWholeName, write = SetRenameWholeName };
  __property bool ConfirmTransferring = { read = FConfirmTransferring, write = SetConfirmTransferring};
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
  __property bool ConfirmRecycling = { read = FConfirmRecycling, write = SetConfirmRecycling};
  __property bool UseLocationProfiles = { read = FUseLocationProfiles, write = SetUseLocationProfiles};
  __property bool UseSharedBookmarks = { read = FUseSharedBookmarks, write = SetUseSharedBookmarks};
  __property UnicodeString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=SetDDTemporaryDirectory };
  __property UnicodeString DDDrives  = { read=FDDDrives, write=SetDDDrives };
  __property bool DDWarnLackOfTempSpace  = { read=FDDWarnLackOfTempSpace, write=SetDDWarnLackOfTempSpace };
  __property bool DDFakeFile = { read=FDDFakeFile, write=SetDDFakeFile };
  __property bool DDExtInstalled = { read=GetDDExtInstalled };
  __property int DDExtTimeout = { read=FDDExtTimeout, write=SetDDExtTimeout };
  __property bool ConfirmClosingSession  = { read=FConfirmClosingSession, write=SetConfirmClosingSession };
  __property double DDWarnLackOfTempSpaceRatio  = { read=FDDWarnLackOfTempSpaceRatio, write=SetDDWarnLackOfTempSpaceRatio };
  __property TBookmarkList * Bookmarks[UnicodeString Key] = { read = GetBookmarks, write = SetBookmarks };
  __property TBookmarkList * SharedBookmarks = { read = GetSharedBookmarks, write = SetSharedBookmarks };
  __property bool EmbeddedSessions = { read = FEmbeddedSessions };
  __property bool ExpertMode = { read = FExpertMode, write = SetExpertMode };
  __property bool DefaultDirIsHome = { read = FDefaultDirIsHome, write = SetDefaultDirIsHome };
  __property bool DisableOpenEdit = { read = FDisableOpenEdit };
  __property TCustomCommandList * CustomCommandList = { read = FCustomCommandList, write = SetCustomCommandList };
  __property TCustomCommandList * ExtensionList = { read = FExtensionList, write = SetExtensionList };
  __property int DDDeleteDelay = { read = FDDDeleteDelay };
  __property bool TemporaryDirectoryAppendSession = { read = FTemporaryDirectoryAppendSession, write = SetTemporaryDirectoryAppendSession };
  __property bool TemporaryDirectoryAppendPath = { read = FTemporaryDirectoryAppendPath, write = SetTemporaryDirectoryAppendPath };
  __property bool TemporaryDirectoryDeterministic = { read = FTemporaryDirectoryDeterministic, write = SetTemporaryDirectoryDeterministic };
  __property bool TemporaryDirectoryCleanup = { read = FTemporaryDirectoryCleanup, write = SetTemporaryDirectoryCleanup };
  __property bool ConfirmTemporaryDirectoryCleanup = { read = FConfirmTemporaryDirectoryCleanup, write = SetConfirmTemporaryDirectoryCleanup };
  __property bool PreservePanelState = { read = FPreservePanelState, write = SetPreservePanelState };
  __property TAutoSwitch DarkTheme = { read = FDarkTheme, write = SetDarkTheme };
  __property UnicodeString LastStoredSession = { read = FLastStoredSession, write = SetLastStoredSession };
  __property UnicodeString LastWorkspace = { read = FLastWorkspace, write = FLastWorkspace };
  __property bool AutoSaveWorkspace = { read = FAutoSaveWorkspace, write = SetAutoSaveWorkspace };
  __property bool AutoSaveWorkspacePasswords = { read = FAutoSaveWorkspacePasswords, write = SetAutoSaveWorkspacePasswords };
  __property UnicodeString AutoWorkspace = { read = FAutoWorkspace, write = SetAutoWorkspace };
  __property TPathInCaption PathInCaption = { read = FPathInCaption, write = SetPathInCaption };
  __property TSessionTabNameFormat SessionTabNameFormat = { read = FSessionTabNameFormat, write = FSessionTabNameFormat };
  __property bool MinimizeToTray = { read = FMinimizeToTray, write = SetMinimizeToTray };
  __property bool BalloonNotifications = { read = FBalloonNotifications, write = SetBalloonNotifications };
  __property unsigned int NotificationsTimeout = { read = FNotificationsTimeout, write = SetNotificationsTimeout };
  __property unsigned int NotificationsStickTime = { read = FNotificationsStickTime, write = SetNotificationsStickTime };
  __property UnicodeString DefaultTranslationFile = { read = FDefaultTranslationFile };
  __property bool CopyParamAutoSelectNotice = { read = FCopyParamAutoSelectNotice, write = SetCopyParamAutoSelectNotice };
  __property bool LockToolbars = { read = FLockToolbars, write = SetLockToolbars };
  __property bool SelectiveToolbarText = { read = FSelectiveToolbarText, write = SetSelectiveToolbarText };
  __property int LargerToolbar = { read = FLargerToolbar, write = SetLargerToolbar };
  __property bool AutoOpenInPutty = { read = FAutoOpenInPutty, write = SetAutoOpenInPutty };
  __property bool RefreshRemotePanel = { read = FRefreshRemotePanel, write = SetRefreshRemotePanel };
  __property TDateTime RefreshRemotePanelInterval = { read = FRefreshRemotePanelInterval, write = SetRefreshRemotePanelInterval };
  __property TFontConfiguration PanelFont = { read = FPanelFont, write = SetPanelFont };
  __property bool NaturalOrderNumericalSorting = { read = FNaturalOrderNumericalSorting, write = SetNaturalOrderNumericalSorting };
  __property bool AlwaysSortDirectoriesByName = { read = FAlwaysSortDirectoriesByName, write = SetAlwaysSortDirectoriesByName };
  __property bool FullRowSelect = { read = FFullRowSelect, write = SetFullRowSelect };
  __property bool OfferedEditorAutoConfig = { read = FOfferedEditorAutoConfig, write = SetOfferedEditorAutoConfig };
  __property int LastMonitor = { read = GetLastMonitor, write = SetLastMonitor };
  __property const TEditorList * EditorList = { read = GetEditorList, write = SetEditorList };
  __property UnicodeString DefaultKeyFile = { read = GetDefaultKeyFile, write = FDefaultKeyFile };
  __property UnicodeString OpenedStoredSessionFolders = { read = FOpenedStoredSessionFolders, write = SetOpenedStoredSessionFolders };
  __property bool AutoImportedFromPuttyOrFilezilla = { read = FAutoImportedFromPuttyOrFilezilla, write = SetAutoImportedFromPuttyOrFilezilla };
  __property int GenerateUrlComponents = { read = FGenerateUrlComponents, write = SetGenerateUrlComponents };
  __property TGenerateUrlCodeTarget GenerateUrlCodeTarget = { read = FGenerateUrlCodeTarget, write = SetGenerateUrlCodeTarget };
  __property TScriptFormat GenerateUrlScriptFormat = { read = FGenerateUrlScriptFormat, write = SetGenerateUrlScriptFormat };
  __property TAssemblyLanguage GenerateUrlAssemblyLanguage = { read = FGenerateUrlAssemblyLanguage, write = SetGenerateUrlAssemblyLanguage };
  __property bool ExternalSessionInExistingInstance = { read = FExternalSessionInExistingInstance, write = SetExternalSessionInExistingInstance };
  __property bool ShowLoginWhenNoSession = { read = FShowLoginWhenNoSession, write = SetShowLoginWhenNoSession };
  __property bool KeepOpenWhenNoSession = { read = FKeepOpenWhenNoSession, write = SetKeepOpenWhenNoSession };
  __property bool DefaultToNewRemoteTab = { read = FDefaultToNewRemoteTab, write = SetDefaultToNewRemoteTab };
  __property bool LocalIconsByExt = { read = FLocalIconsByExt, write = SetLocalIconsByExt };
  __property bool FlashTaskbar = { read = FFlashTaskbar, write = SetFlashTaskbar };
  __property int MaxSessions = { read = FMaxSessions, write = FMaxSessions };
  __property TLocaleFlagOverride BidiModeOverride = { read = FBidiModeOverride, write = SetBidiModeOverride };
  __property TLocaleFlagOverride FlipChildrenOverride = { read = FFlipChildrenOverride, write = SetFlipChildrenOverride };
  __property bool ShowTips = { read = FShowTips, write = SetShowTips };
  __property UnicodeString TipsSeen = { read = FTipsSeen, write = SetTipsSeen };
  __property TDateTime TipsShown = { read = FTipsShown, write = SetTipsShown };
  __property UnicodeString FileColors = { read = FFileColors, write = SetFileColors };
  __property int RunsSinceLastTip = { read = FRunsSinceLastTip, write = SetRunsSinceLastTip };
  __property int HonorDrivePolicy = { read = GetHonorDrivePolicy, write = SetHonorDrivePolicy };
  __property bool UseABDrives = { read = GetUseABDrives, write = SetUseABDrives };
  __property TMasterPasswordPromptEvent OnMasterPasswordPrompt = { read = FOnMasterPasswordPrompt, write = FOnMasterPasswordPrompt };
  __property TStrings * CustomCommandOptions = { read = GetCustomCommandOptions, write = SetCustomCommandOptions };
  __property bool LockedInterface = { read = FLockedInterface, write = SetLockedInterface };
  __property bool TimeoutShellOperations = { read = GetTimeoutShellOperations, write = SetTimeoutShellOperations };
  __property bool TimeoutShellIconRetrieval = { read = FTimeoutShellIconRetrieval, write = SetTimeoutShellIconRetrieval };
  __property bool UseIconUpdateThread = { read = FUseIconUpdateThread, write = SetUseIconUpdateThread };
  __property bool AllowWindowPrint = { read = FAllowWindowPrint, write = SetAllowWindowPrint };
  __property TStoreTransition StoreTransition = { read = FStoreTransition, write = SetStoreTransition };
  __property int QueueTransferLimitMax = { read = FQueueTransferLimitMax, write = SetQueueTransferLimitMax };
  __property bool HiContrast = { read = FHiContrast, write = SetHiContrast };
  __property bool EditorCheckNotModified = { read = FEditorCheckNotModified, write = SetEditorCheckNotModified };
  __property bool SessionTabCaptionTruncation = { read = FSessionTabCaptionTruncation, write = SetSessionTabCaptionTruncation };
  __property int LoadingTooLongLimit = { read = GetLoadingTooLongLimit, write = SetLoadingTooLongLimit };
  __property UnicodeString RemoteThumbnailMask = { read = FRemoteThumbnailMask, write = FRemoteThumbnailMask };
  __property int RemoteThumbnailSizeLimit = { read = FRemoteThumbnailSizeLimit, write = FRemoteThumbnailSizeLimit };
  __property UnicodeString FirstRun = { read = FFirstRun, write = SetFirstRun };
  __property LCID DefaultLocale = { read = FDefaultLocale };
  __property int LocaleCompletenessTreshold = { read = GetLocaleCompletenessTreshold };
};
//---------------------------------------------------------------------------
class TCustomCommandType
{
public:
  __fastcall TCustomCommandType();
  __fastcall TCustomCommandType(const TCustomCommandType & Other);

  enum TOptionKind { okUnknown, okLabel, okLink, okSeparator, okGroup, okTextBox, okFile, okDropDownList, okComboBox, okCheckBox };
  enum TOptionFlag { ofRun = 0x01, ofConfig = 0x02, ofSite = 0x04 };

  class TOption
  {
  public:
    __fastcall TOption() {}

    UnicodeString Id;
    unsigned int Flags;
    TOptionKind Kind;
    UnicodeString Caption;
    UnicodeString Default;
    typedef std::vector<UnicodeString> TParams;
    TParams Params;
    UnicodeString FileCaption;
    UnicodeString FileFilter;
    UnicodeString FileInitial;
    UnicodeString FileExt;

    bool operator==(const TOption & Other) const;
    __property bool IsControl = { read = GetIsControl };
    bool CanHavePatterns() const;
    bool HasPatterns(TCustomCommand * CustomCommandForOptions) const;

  private:
    bool __fastcall GetIsControl() const;
  };

  TCustomCommandType & operator=(const TCustomCommandType & Other);
  bool __fastcall Equals(const TCustomCommandType * Other) const;

  void __fastcall LoadExtension(const UnicodeString & Path);
  void __fastcall LoadExtension(TStrings * Lines, const UnicodeString & PathForBaseName);
  static UnicodeString __fastcall GetExtensionId(const UnicodeString & Name);

  __property UnicodeString Name = { read = FName, write = FName };
  __property UnicodeString Command = { read = FCommand, write = FCommand };
  __property int Params = { read = FParams, write = FParams };
  __property TShortCut ShortCut = { read = FShortCut, write = FShortCut };
  __property UnicodeString Id = { read = FId, write = FId };
  __property UnicodeString FileName = { read = FFileName, write = FFileName };
  __property UnicodeString Description = { read = FDescription, write = FDescription };
  __property UnicodeString HomePage = { read = FHomePage, write = FHomePage };
  __property UnicodeString OptionsPage = { read = FOptionsPage, write = FOptionsPage };

  __property int OptionsCount = { read = GetOptionsCount };
  const TOption & __fastcall GetOption(int Index) const;
  bool __fastcall AnyOptionWithFlag(unsigned int Flag) const;
  UnicodeString __fastcall GetOptionKey(const TOption & Option, const UnicodeString & Site) const;
  UnicodeString __fastcall GetCommandWithExpandedOptions(
    TStrings * CustomCommandOptions, const UnicodeString & Site) const;
  bool __fastcall HasCustomShortCut() const;

protected:
  bool __fastcall ParseOption(const UnicodeString & Value, TOption & Option, const UnicodeString & ExtensionBaseName);
  int __fastcall GetOptionsCount() const;
  UnicodeString __fastcall GetOptionCommand(const TOption & Option, const UnicodeString & Value) const;

private:
  UnicodeString FName;
  UnicodeString FCommand;
  int FParams;
  TShortCut FShortCut;
  TShortCut FShortCutOriginal;
  UnicodeString FId;
  UnicodeString FFileName;
  UnicodeString FDescription;
  UnicodeString FHomePage;
  UnicodeString FOptionsPage;
  std::vector<TOption> FOptions;
};
//---------------------------------------------------------------------------
class TCustomCommandList
{
public:
  __fastcall TCustomCommandList();
  __fastcall ~TCustomCommandList();

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage);
  void __fastcall Reset();
  void __fastcall Modify();

  void __fastcall Clear();
  void __fastcall Add(const UnicodeString Name, const UnicodeString Command, int Params);
  void __fastcall Add(TCustomCommandType * Command);
  void __fastcall Insert(int Index, TCustomCommandType * Command);
  void __fastcall Change(int Index, TCustomCommandType * Command);
  void __fastcall Move(int CurIndex, int NewIndex);
  void __fastcall Delete(int Index);
  void __fastcall SortBy(TStrings * Ids);

  const TCustomCommandType * Find(const UnicodeString Name) const;
  const TCustomCommandType * Find(TShortCut ShortCut) const;
  int FindIndexByFileName(const UnicodeString & FileName) const;

  bool __fastcall Equals(const TCustomCommandList * Other) const;
  void __fastcall Assign(const TCustomCommandList * Other);

  void __fastcall ShortCuts(TShortCuts & ShortCuts) const;

  __property bool Modified = { read = FModified };
  __property int Count = { read = GetCount };
  __property const TCustomCommandType * Commands[int Index] = { read = GetConstCommand };

private:
  bool FModified;
  TList * FCommands;
  int __fastcall GetCount() const;
  const TCustomCommandType * __fastcall GetConstCommand(int Index) const;
  TCustomCommandType * __fastcall GetCommand(int Index);
};
//---------------------------------------------------------------------------
extern TWinConfiguration * WinConfiguration;
extern const UnicodeString WinSCPExtensionExt;
//---------------------------------------------------------------------------
#endif
