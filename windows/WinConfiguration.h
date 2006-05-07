//---------------------------------------------------------------------------
#ifndef WinConfigurationH
#define WinConfigurationH
//---------------------------------------------------------------------------
#include "CustomWinConfiguration.h"
#include "CustomDirView.hpp"
//---------------------------------------------------------------------------
enum TEditor { edInternal, edExternal };
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
  int SessionComboWidth;
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(ToolbarsLayout) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress)
        C(DriveView) C(DriveViewWidth) C(SessionComboWidth) 0; };
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
  bool CommandLine;
  TOperationSide CurrentPanel;
  TNortonLikeMode NortonLikeMode;
  bool PreserveLocalDirectory;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool CompareByTime;
  bool CompareBySize;
  bool SwappedPanels;
  int SessionComboWidth;
  bool FullRowSelect;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(ToolbarsLayout) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel) C(CommandLine)
      C(NortonLikeMode) C(PreserveLocalDirectory)
      C(CompareBySize) C(CompareByTime) C(SwappedPanels)
      C(SessionComboWidth) C(FullRowSelect) 0; };

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
  bool SingleEditor;
  unsigned int MaxEditors;
  unsigned int EarlyClose;
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(FontName) C(FontHeight)
      C(FontCharset) C(FontStyle) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) C(SingleEditor)
      C(MaxEditors) C(EarlyClose) 0; };
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
  bool __fastcall operator !=(TUpdatesData & rhc)
    { return C(ForVersion) C(Version) C(Message) C(Critical) C(Release) 0; };
  void Reset()
  {
    ForVersion = 0;
    Version = 0;
    Message = "";
    Critical = false;
    Release = "";
  }
};
//---------------------------------------------------------------------------
struct TUpdatesConfiguration
{
  TDateTime Period;
  TDateTime LastCheck;
  AnsiString ProxyHost;
  int ProxyPort;
  bool HaveResults;
  bool ShownResults;
  TUpdatesData Results;
  bool __fastcall operator !=(TUpdatesConfiguration & rhc)
    { return C(Period) C(LastCheck) C(ProxyHost) C(ProxyPort) C(HaveResults)
        C(ShownResults) C(Results)  0; };
};
//---------------------------------------------------------------------------
struct TEditorData
{
  TFileMasks FileMask;
  TEditor Editor;
  AnsiString ExternalEditor;
  bool ExternalEditorText;
  bool MDIExternalEditor;
  bool DetectMDIExternalEditor;
};
//---------------------------------------------------------------------------
#undef C
//---------------------------------------------------------------------------
class TEditorPreferences
{
public:
  __fastcall TEditorPreferences();
  __fastcall TEditorPreferences(const TEditorPreferences & Source);

  bool __fastcall Matches(const AnsiString FileName, bool Local,
    const TFileMasks::TParams & Params) const;
  void __fastcall Load(THierarchicalStorage * Storage, bool Legacy);
  void __fastcall Save(THierarchicalStorage * Storage) const;
  void __fastcall LegacyDefaults();

  bool __fastcall operator ==(const TEditorPreferences & rhp) const;

  __property TEditorData Data = { read = FData, write = FData };
  __property AnsiString Name = { read = GetName };

private:
  TEditorData FData;
  mutable AnsiString FName;

  AnsiString __fastcall GetName() const;
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
  void __fastcall Modify();

  __property int Count = { read = GetCount };
  __property const TEditorPreferences * Editors[int Index] = { read = GetEditor };
  __property bool Modified = { read = FModified };

private:
  TList * FEditors;
  bool FModified;

  int __fastcall GetCount() const;

  void __fastcall Init();
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
  bool FConfirmDeleting;
  bool FConfirmRecycling;
  bool FUseLocationProfiles;
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
  bool FForceDeleteTempFolder;
  bool FDefaultDirIsHome;
  int FDDDeleteDelay;
  bool FTemporaryDirectoryCleanup;
  bool FConfirmTemporaryDirectoryCleanup;
  AnsiString FDefaultTranslationFile;
  bool FInvalidDefaultTranslation;
  AnsiString FInvalidDefaultTranslationMessage;
  bool FPreservePanelState;
  AnsiString FTheme;
  TPathInCaption FPathInCaption;
  TUpdatesConfiguration FUpdates;
  bool FCopyParamAutoSelectNotice;
  bool FSessionToolbarAutoShown;
  bool FLockToolbars;
  TEditorList * FEditorList;
  TEditorPreferences * FLegacyEditor;
  AnsiString FDefaultKeyFile;
  bool FAutoOpenInPutty;

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
  void __fastcall SetConfirmDeleting(bool value);
  void __fastcall SetConfirmRecycling(bool value);
  void __fastcall SetUseLocationProfiles(bool value);
  void __fastcall SetDDTemporaryDirectory(AnsiString value);
  void __fastcall SetDDWarnLackOfTempSpace(bool value);
  void __fastcall SetDDExtEnabled(bool value);
  void __fastcall SetDDExtTimeout(int value);
  void __fastcall SetConfirmClosingSession(bool value);
  void __fastcall SetConfirmExitOnCompletion(bool value);
  void __fastcall SetForceDeleteTempFolder(bool value);
  void __fastcall SetDDWarnLackOfTempSpaceRatio(double value);
  void __fastcall SetBookmarks(AnsiString Key, TBookmarkList * value);
  TBookmarkList * __fastcall GetBookmarks(AnsiString Key);
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
  void __fastcall SetCopyParamAutoSelectNotice(bool value);
  void __fastcall SetSessionToolbarAutoShown(bool value);
  TUpdatesConfiguration __fastcall GetUpdates();
  void __fastcall SetUpdates(TUpdatesConfiguration value);
  void __fastcall SetLockToolbars(bool value);
  const TEditorList * __fastcall GetEditorList();
  void __fastcall SetEditorList(const TEditorList * value);
  void __fastcall SetAutoOpenInPutty(bool value);

  bool __fastcall GetDDExtInstalled();

protected:
  virtual TStorage __fastcall GetStorage();
  virtual void __fastcall Load();
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadAdmin(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall ModifyAll();
  bool __fastcall SameStringLists(TStrings * Strings1, TStrings * Strings2);
  bool __fastcall InternalReloadComponentRes(const AnsiString ResName,
    HANDLE HInst, TComponent * Instance);
  bool __fastcall InitComponent(TComponent * Instance,
    TClass RootAncestor, TClass ClassType);
  virtual HANDLE __fastcall LoadNewResourceModule(LCID Locale,
    AnsiString * FileName = NULL);
  virtual void __fastcall SetResourceModule(HANDLE Instance);
  virtual LCID __fastcall GetLocale();
  void __fastcall CheckTranslationVersion(const AnsiString FileName,
    bool InternalLocaleOnError);
  virtual void __fastcall DefaultLocalized();

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
  bool __fastcall ConfirmRemoveDefaultTranslation();
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
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
  __property bool ConfirmRecycling = { read = FConfirmRecycling, write = SetConfirmRecycling};
  __property bool UseLocationProfiles = { read = FUseLocationProfiles, write = SetUseLocationProfiles};
  __property AnsiString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=SetDDTemporaryDirectory };
  __property bool DDWarnLackOfTempSpace  = { read=FDDWarnLackOfTempSpace, write=SetDDWarnLackOfTempSpace };
  __property bool DDExtEnabled = { read=FDDExtEnabled, write=SetDDExtEnabled };
  __property bool DDExtInstalled = { read=GetDDExtInstalled };
  __property int DDExtTimeout = { read=FDDExtTimeout, write=SetDDExtTimeout };
  __property bool ConfirmClosingSession  = { read=FConfirmClosingSession, write=SetConfirmClosingSession };
  __property bool ConfirmExitOnCompletion  = { read=FConfirmExitOnCompletion, write=SetConfirmExitOnCompletion };
  __property bool ForceDeleteTempFolder  = { read=FForceDeleteTempFolder, write=SetForceDeleteTempFolder };
  __property double DDWarnLackOfTempSpaceRatio  = { read=FDDWarnLackOfTempSpaceRatio, write=SetDDWarnLackOfTempSpaceRatio };
  __property TBookmarkList * Bookmarks[AnsiString Key] = { read = GetBookmarks, write = SetBookmarks };
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
  __property bool InvalidDefaultTranslation = { read = FInvalidDefaultTranslation };
  __property AnsiString DefaultTranslationFile = { read = FDefaultTranslationFile };
  __property bool CopyParamAutoSelectNotice = { read = FCopyParamAutoSelectNotice, write = SetCopyParamAutoSelectNotice };
  __property bool SessionToolbarAutoShown = { read = FSessionToolbarAutoShown, write = SetSessionToolbarAutoShown };
  __property bool LockToolbars = { read = FLockToolbars, write = SetLockToolbars };
  __property bool AutoOpenInPutty = { read = FAutoOpenInPutty, write = SetAutoOpenInPutty };
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
