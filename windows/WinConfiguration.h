//---------------------------------------------------------------------------
#ifndef WinConfigurationH
#define WinConfigurationH
//---------------------------------------------------------------------------
#include "CustomWinConfiguration.h"
#include "CustomDirView.hpp"
//---------------------------------------------------------------------------
enum TEditor { edInternal, edExternal, edOpen };
extern const char ShellCommandFileNamePattern[];
//---------------------------------------------------------------------------
#define C(Property) (Property != rhc.Property) ||
struct TScpExplorerConfiguration {
  AnsiString WindowParams;
  AnsiString DirViewParams;
  AnsiString ToolbarsLayout;
  bool StatusBar;
  AnsiString LastLocalTargetDirectory;
  int ViewStyle;
  bool ShowFullAddress;
  bool DriveView;
  int DriveViewWidth;
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(ToolbarsLayout) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress)
        C(DriveView) C(DriveViewWidth) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderPanelConfiguration {
  AnsiString DirViewParams;
  bool StatusBar;
  bool DriveView;
  int DriveViewHeight;
  bool __fastcall operator !=(TScpCommanderPanelConfiguration & rhc)
    { return C(DirViewParams) C(StatusBar)
        C(DriveView) C(DriveViewHeight) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderConfiguration {
  AnsiString WindowParams;
  float LocalPanelWidth;
  AnsiString ToolbarsLayout;
  bool StatusBar;
  TOperationSide CurrentPanel;
  TNortonLikeMode NortonLikeMode;
  bool PreserveLocalDirectory;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool CompareByTime;
  bool CompareBySize;
  bool SwappedPanels;
  bool FullRowSelect;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(ToolbarsLayout) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel)
      C(NortonLikeMode) C(PreserveLocalDirectory)
      C(CompareBySize) C(CompareByTime) C(SwappedPanels)
      C(FullRowSelect) 0; };

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
struct TEditorConfiguration {
  AnsiString FontName;
  int FontHeight;
  int FontCharset;
  int FontStyle;
  bool WordWrap;
  AnsiString FindText;
  AnsiString ReplaceText;
  bool FindMatchCase;
  bool FindWholeWord;
  bool FindDown;
  unsigned int TabSize;
  unsigned int MaxEditors;
  unsigned int EarlyClose;
  bool SDIShellEditor;
  AnsiString WindowParams;
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(FontName) C(FontHeight)
      C(FontCharset) C(FontStyle) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) C(FindDown) C(TabSize)
      C(MaxEditors) C(EarlyClose) C(SDIShellEditor) C(WindowParams) 0; };
};
//---------------------------------------------------------------------------
enum TQueueViewShow { qvShow, qvHideWhenEmpty, qvHide };
struct TQueueViewConfiguration {
  int Height;
  AnsiString Layout;
  TQueueViewShow Show;
  TQueueViewShow LastHideShow;
  bool ToolBar;
  bool __fastcall operator !=(TQueueViewConfiguration & rhc)
    { return C(Height) C(Layout) C(Show) C(LastHideShow) C(ToolBar) 0; };
};
//---------------------------------------------------------------------------
struct TUpdatesData
{
  int ForVersion;
  int Version;
  AnsiString Message;
  bool Critical;
  AnsiString Release;
  bool Disabled;
  AnsiString Url;
  AnsiString UrlButton;
  bool __fastcall operator !=(TUpdatesData & rhc)
    { return C(ForVersion) C(Version) C(Message) C(Critical) C(Release)
             C(Disabled) C(Url) C(UrlButton) 0; };
  void Reset()
  {
    ForVersion = 0;
    Version = 0;
    Message = "";
    Critical = false;
    Release = "";
    Disabled = false;
    Url = "";
    UrlButton = "";
  }
};
//---------------------------------------------------------------------------
enum TConnectionType { ctDirect, ctAuto, ctProxy };
//---------------------------------------------------------------------------
struct TUpdatesConfiguration
{
  TDateTime Period;
  TDateTime LastCheck;
  TConnectionType ConnectionType;
  AnsiString ProxyHost;
  int ProxyPort;
  TAutoSwitch BetaVersions;
  bool HaveResults;
  bool ShownResults;
  TUpdatesData Results;
  bool __fastcall operator !=(TUpdatesConfiguration & rhc)
    { return C(Period) C(LastCheck) C(ConnectionType) C(ProxyHost) C(ProxyPort)
        C(BetaVersions) C(HaveResults) C(ShownResults) C(Results)  0; };
};
//---------------------------------------------------------------------------
struct TEditorData
{
  __fastcall TEditorData();
  __fastcall TEditorData(const TEditorData & Source);

  TFileMasks FileMask;
  TEditor Editor;
  AnsiString ExternalEditor;
  bool ExternalEditorText;
  bool SDIExternalEditor;
  bool DetectMDIExternalEditor;

  bool __fastcall operator ==(const TEditorData & rhd) const;
};
//---------------------------------------------------------------------------
#undef C
//---------------------------------------------------------------------------
class TEditorPreferences
{
public:
  __fastcall TEditorPreferences();
  __fastcall TEditorPreferences(const TEditorData & Data);
  bool __fastcall Matches(const AnsiString FileName, bool Local,
    const TFileMasks::TParams & Params) const;
  void __fastcall Load(THierarchicalStorage * Storage, bool Legacy);
  void __fastcall Save(THierarchicalStorage * Storage) const;
  void __fastcall LegacyDefaults();

  bool __fastcall operator ==(const TEditorPreferences & rhp) const;

  __property const TEditorData * Data = { read = GetConstData };
  __property AnsiString Name = { read = GetName };

  TEditorData * __fastcall GetData();

private:
  TEditorData FData;
  mutable AnsiString FName;

  AnsiString __fastcall GetName() const;
  const TEditorData * __fastcall GetConstData() const { return &FData; };
};
//---------------------------------------------------------------------------
class TEditorList
{
public:
  __fastcall TEditorList();
  virtual __fastcall ~TEditorList();

  const TEditorPreferences * __fastcall Find(const AnsiString FileName,
    bool Local, const TFileMasks::TParams & Params) const;

  void __fastcall Load(THierarchicalStorage * Storage);
  void __fastcall Save(THierarchicalStorage * Storage) const;

  void __fastcall operator=(const TEditorList & rhl);
  bool __fastcall operator==(const TEditorList & rhl) const;

  void __fastcall Clear();
  void __fastcall Add(TEditorPreferences * Editor);
  void __fastcall Insert(int Index, TEditorPreferences * Editor);
  void __fastcall Change(int Index, TEditorPreferences * Editor);
  void __fastcall Move(int CurIndex, int NewIndex);
  void __fastcall Delete(int Index);
  void __fastcall Saved();

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
class TCustomCommands;
enum TPathInCaption { picShort, picFull, picNone };
// constants must be compatible with legacy CopyOnDoubleClick
enum TDoubleClickAction { dcaOpen = 0, dcaCopy = 1, dcaEdit = 2 };
//---------------------------------------------------------------------------
class TWinConfiguration : public TCustomWinConfiguration
{
private:
  AnsiString FAutoStartSession;
  TDoubleClickAction FDoubleClickAction;
  bool FCopyOnDoubleClickConfirmation;
  bool FDDAllowMove;
  bool FDDAllowMoveInit;
  bool FDDTransferConfirmation;
  bool FDeleteToRecycleBin;
  bool FDimmHiddenFiles;
  bool FLogWindowOnStartup;
  AnsiString FLogWindowParams;
  TScpCommanderConfiguration FScpCommander;
  TScpExplorerConfiguration FScpExplorer;
  bool FSelectDirectories;
  AnsiString FSelectMask;
  bool FShowHiddenFiles;
  bool FShowInaccesibleDirectories;
  bool FConfirmTransferring;
  bool FConfirmDeleting;
  bool FConfirmRecycling;
  bool FUseLocationProfiles;
  bool FUseSharedBookmarks;
  AnsiString FDDTemporaryDirectory;
  bool FDDWarnLackOfTempSpace;
  bool FDDExtEnabled;
  int FDDExtInstalled;
  int FDDExtTimeout;
  bool FConfirmClosingSession;
  bool FConfirmExitOnCompletion;
  double FDDWarnLackOfTempSpaceRatio;
  AnsiString FTemporarySessionFile;
  AnsiString FTemporaryKeyFile;
  TBookmarks * FBookmarks;
  TCustomCommands * FCustomCommands;
  bool FCustomCommandsModified;
  bool FCustomCommandsDefaults;
  TEditorConfiguration FEditor;
  TQueueViewConfiguration FQueueView;
  bool FEmbeddedSessions;
  bool FExpertMode;
  bool FDisableOpenEdit;
  bool FDefaultDirIsHome;
  int FDDDeleteDelay;
  bool FTemporaryDirectoryCleanup;
  bool FConfirmTemporaryDirectoryCleanup;
  AnsiString FDefaultTranslationFile;
  AnsiString FInvalidDefaultTranslationMessage;
  bool FPreservePanelState;
  AnsiString FTheme;
  TPathInCaption FPathInCaption;
  bool FMinimizeToTray;
  bool FBalloonNotifications;
  unsigned int FNotificationsTimeout;
  unsigned int FNotificationsStickTime;
  TUpdatesConfiguration FUpdates;
  AnsiString FVersionHistory;
  bool FCopyParamAutoSelectNotice;
  bool FSessionToolbarAutoShown;
  bool FLockToolbars;
  TEditorList * FEditorList;
  TEditorPreferences * FLegacyEditor;
  AnsiString FDefaultKeyFile;
  bool FAutoOpenInPutty;
  bool FTelnetForFtpInPutty;
  TDateTime FDefaultUpdatesPeriod;

  void __fastcall SetDoubleClickAction(TDoubleClickAction value);
  void __fastcall SetCopyOnDoubleClickConfirmation(bool value);
  void __fastcall SetDDAllowMove(bool value);
  void __fastcall SetDDAllowMoveInit(bool value);
  void __fastcall SetDDTransferConfirmation(bool value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetDimmHiddenFiles(bool value);
  void __fastcall SetLogWindowOnStartup(bool value);
  void __fastcall SetLogWindowParams(AnsiString value);
  void __fastcall SetScpCommander(TScpCommanderConfiguration value);
  void __fastcall SetScpExplorer(TScpExplorerConfiguration value);
  void __fastcall SetSelectDirectories(bool value);
  void __fastcall SetShowHiddenFiles(bool value);
  void __fastcall SetShowInaccesibleDirectories(bool value);
  void __fastcall SetConfirmTransferring(bool value);
  void __fastcall SetConfirmDeleting(bool value);
  void __fastcall SetConfirmRecycling(bool value);
  void __fastcall SetUseLocationProfiles(bool value);
  void __fastcall SetUseSharedBookmarks(bool value);
  void __fastcall SetDDTemporaryDirectory(AnsiString value);
  void __fastcall SetDDWarnLackOfTempSpace(bool value);
  void __fastcall SetDDExtEnabled(bool value);
  void __fastcall SetDDExtTimeout(int value);
  void __fastcall SetConfirmClosingSession(bool value);
  void __fastcall SetConfirmExitOnCompletion(bool value);
  void __fastcall SetDDWarnLackOfTempSpaceRatio(double value);
  void __fastcall SetBookmarks(AnsiString Key, TBookmarkList * value);
  TBookmarkList * __fastcall GetBookmarks(AnsiString Key);
  void __fastcall SetSharedBookmarks(TBookmarkList * value);
  TBookmarkList * __fastcall GetSharedBookmarks();
  void __fastcall SetAutoStartSession(AnsiString value);
  void __fastcall SetExpertMode(bool value);
  void __fastcall SetDefaultDirIsHome(bool value);
  void __fastcall SetEditor(TEditorConfiguration value);
  void __fastcall SetQueueView(TQueueViewConfiguration value);
  void __fastcall SetCustomCommands(TCustomCommands * value);
  void __fastcall SetTemporaryDirectoryCleanup(bool value);
  void __fastcall SetConfirmTemporaryDirectoryCleanup(bool value);
  void __fastcall SetPreservePanelState(bool value);
  void __fastcall SetTheme(AnsiString value);
  void __fastcall SetPathInCaption(TPathInCaption value);
  void __fastcall SetMinimizeToTray(bool value);
  void __fastcall SetBalloonNotifications(bool value);
  void __fastcall SetNotificationsTimeout(unsigned int value);
  void __fastcall SetNotificationsStickTime(unsigned int value);
  void __fastcall SetCopyParamAutoSelectNotice(bool value);
  void __fastcall SetSessionToolbarAutoShown(bool value);
  TUpdatesConfiguration __fastcall GetUpdates();
  void __fastcall SetUpdates(TUpdatesConfiguration value);
  void __fastcall SetVersionHistory(AnsiString value);
  void __fastcall SetLockToolbars(bool value);
  const TEditorList * __fastcall GetEditorList();
  void __fastcall SetEditorList(const TEditorList * value);
  void __fastcall SetAutoOpenInPutty(bool value);
  void __fastcall SetTelnetForFtpInPutty(bool value);
  void __fastcall SetLastMonitor(int value);
  int __fastcall GetLastMonitor();

  bool __fastcall GetDDExtInstalled();
  void __fastcall AddVersionToHistory(AnsiString & VersionHistory);
  bool __fastcall GetAnyBetaInVersionHistory();

protected:
  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Load();
  virtual void __fastcall SaveData(THierarchicalStorage * Storage, bool All);
  virtual void __fastcall LoadData(THierarchicalStorage * Storage);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall Saved();
  bool __fastcall SameStringLists(TStrings * Strings1, TStrings * Strings2);
  bool __fastcall InternalReloadComponentRes(const AnsiString ResName,
    HINSTANCE HInst, TComponent * Instance);
  bool __fastcall InitComponent(TComponent * Instance,
    TClass RootAncestor, TClass ClassType);
  virtual HINSTANCE __fastcall LoadNewResourceModule(LCID Locale,
    AnsiString * FileName = NULL);
  virtual void __fastcall SetResourceModule(HINSTANCE Instance);
  virtual LCID __fastcall GetLocale();
  void __fastcall CheckTranslationVersion(const AnsiString FileName,
    bool InternalLocaleOnError);
  virtual void __fastcall DefaultLocalized();
  bool __fastcall DetectRegistryStorage(HKEY RootKey);
  bool __fastcall CanWriteToStorage();

public:
  __fastcall TWinConfiguration();
  virtual __fastcall ~TWinConfiguration();
  virtual void __fastcall Default();
  void __fastcall ClearTemporaryLoginData();
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);
  AnsiString __fastcall TemporaryDir(bool Mask = false);
  TStrings * __fastcall FindTemporaryFolders();
  void __fastcall CleanupTemporaryFolders(TStrings * Folders = NULL);
  void __fastcall CheckDefaultTranslation();
  const TEditorPreferences * __fastcall DefaultEditorForFile(
    const AnsiString FileName, bool Local, const TFileMasks::TParams & MaskParams);

  __property TScpCommanderConfiguration ScpCommander = { read = FScpCommander, write = SetScpCommander };
  __property TScpExplorerConfiguration ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool SelectDirectories = { read = FSelectDirectories, write = SetSelectDirectories };
  __property AnsiString SelectMask = { read = FSelectMask, write = FSelectMask };
  __property bool ShowHiddenFiles = { read = FShowHiddenFiles, write = SetShowHiddenFiles };
  __property bool ShowInaccesibleDirectories = { read = FShowInaccesibleDirectories, write = SetShowInaccesibleDirectories };
  __property TEditorConfiguration Editor = { read = FEditor, write = SetEditor };
  __property TQueueViewConfiguration QueueView = { read = FQueueView, write = SetQueueView };
  __property TUpdatesConfiguration Updates = { read = GetUpdates, write = SetUpdates };
  __property AnsiString VersionHistory = { read = FVersionHistory, write = SetVersionHistory };
  __property bool AnyBetaInVersionHistory = { read = GetAnyBetaInVersionHistory };
  __property AnsiString AutoStartSession = { read = FAutoStartSession, write = SetAutoStartSession };
  __property TDoubleClickAction DoubleClickAction = { read = FDoubleClickAction, write = SetDoubleClickAction };
  __property bool CopyOnDoubleClickConfirmation = { read = FCopyOnDoubleClickConfirmation, write = SetCopyOnDoubleClickConfirmation };
  __property bool DDAllowMove = { read = FDDAllowMove, write = SetDDAllowMove };
  __property bool DDAllowMoveInit = { read = FDDAllowMoveInit, write = SetDDAllowMoveInit };
  __property bool DDTransferConfirmation = { read = FDDTransferConfirmation, write = SetDDTransferConfirmation };
  __property bool LogWindowOnStartup = { read = FLogWindowOnStartup, write = SetLogWindowOnStartup };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool DimmHiddenFiles = { read = FDimmHiddenFiles, write = SetDimmHiddenFiles };
  __property AnsiString LogWindowParams = { read = FLogWindowParams, write = SetLogWindowParams };
  __property bool ConfirmTransferring = { read = FConfirmTransferring, write = SetConfirmTransferring};
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
  __property bool ConfirmRecycling = { read = FConfirmRecycling, write = SetConfirmRecycling};
  __property bool UseLocationProfiles = { read = FUseLocationProfiles, write = SetUseLocationProfiles};
  __property bool UseSharedBookmarks = { read = FUseSharedBookmarks, write = SetUseSharedBookmarks};
  __property AnsiString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=SetDDTemporaryDirectory };
  __property bool DDWarnLackOfTempSpace  = { read=FDDWarnLackOfTempSpace, write=SetDDWarnLackOfTempSpace };
  __property bool DDExtEnabled = { read=FDDExtEnabled, write=SetDDExtEnabled };
  __property bool DDExtInstalled = { read=GetDDExtInstalled };
  __property int DDExtTimeout = { read=FDDExtTimeout, write=SetDDExtTimeout };
  __property bool ConfirmClosingSession  = { read=FConfirmClosingSession, write=SetConfirmClosingSession };
  __property bool ConfirmExitOnCompletion  = { read=FConfirmExitOnCompletion, write=SetConfirmExitOnCompletion };
  __property double DDWarnLackOfTempSpaceRatio  = { read=FDDWarnLackOfTempSpaceRatio, write=SetDDWarnLackOfTempSpaceRatio };
  __property TBookmarkList * Bookmarks[AnsiString Key] = { read = GetBookmarks, write = SetBookmarks };
  __property TBookmarkList * SharedBookmarks = { read = GetSharedBookmarks, write = SetSharedBookmarks };
  __property bool EmbeddedSessions = { read = FEmbeddedSessions };
  __property bool ExpertMode = { read = FExpertMode, write = SetExpertMode };
  __property bool DefaultDirIsHome = { read = FDefaultDirIsHome, write = SetDefaultDirIsHome };
  __property bool DisableOpenEdit = { read = FDisableOpenEdit };
  __property TCustomCommands * CustomCommands = { read = FCustomCommands, write = SetCustomCommands };
  __property int DDDeleteDelay = { read = FDDDeleteDelay };
  __property bool TemporaryDirectoryCleanup = { read = FTemporaryDirectoryCleanup, write = SetTemporaryDirectoryCleanup };
  __property bool ConfirmTemporaryDirectoryCleanup = { read = FConfirmTemporaryDirectoryCleanup, write = SetConfirmTemporaryDirectoryCleanup };
  __property bool PreservePanelState = { read = FPreservePanelState, write = SetPreservePanelState };
  __property AnsiString Theme = { read = FTheme, write = SetTheme };
  __property TPathInCaption PathInCaption = { read = FPathInCaption, write = SetPathInCaption };
  __property bool MinimizeToTray = { read = FMinimizeToTray, write = SetMinimizeToTray };
  __property bool BalloonNotifications = { read = FBalloonNotifications, write = SetBalloonNotifications };
  __property unsigned int NotificationsTimeout = { read = FNotificationsTimeout, write = SetNotificationsTimeout };
  __property unsigned int NotificationsStickTime = { read = FNotificationsStickTime, write = SetNotificationsStickTime };
  __property AnsiString DefaultTranslationFile = { read = FDefaultTranslationFile };
  __property bool CopyParamAutoSelectNotice = { read = FCopyParamAutoSelectNotice, write = SetCopyParamAutoSelectNotice };
  __property bool SessionToolbarAutoShown = { read = FSessionToolbarAutoShown, write = SetSessionToolbarAutoShown };
  __property bool LockToolbars = { read = FLockToolbars, write = SetLockToolbars };
  __property bool AutoOpenInPutty = { read = FAutoOpenInPutty, write = SetAutoOpenInPutty };
  __property int LastMonitor = { read = GetLastMonitor, write = SetLastMonitor };
  __property const TEditorList * EditorList = { read = GetEditorList, write = SetEditorList };
  __property AnsiString DefaultKeyFile = { read = GetDefaultKeyFile, write = FDefaultKeyFile };
};
//---------------------------------------------------------------------------
class TCustomCommands : public TStringList
{
public:
  __property int Params[AnsiString Name] = {read=GetParam, write=SetParam};

  bool __fastcall Equals(TCustomCommands * Commands);

private:
  int __fastcall GetParam(const AnsiString & Name);
  void __fastcall SetParam(const AnsiString & Name, int value);
};
//---------------------------------------------------------------------------
#define WinConfiguration (dynamic_cast<TWinConfiguration *>(Configuration))
//---------------------------------------------------------------------------
#endif
