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
  AnsiString CoolBarLayout;
  bool StatusBar;
  AnsiString LastLocalTargetDirectory;
  int ViewStyle;
  bool ShowFullAddress;
  bool DriveView;
  int DriveViewWidth;
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(CoolBarLayout) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress)
        C(DriveView) C(DriveViewWidth) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderPanelConfiguration {
  AnsiString DirViewParams;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool DriveView;
  int DriveViewHeight;
  bool __fastcall operator !=(TScpCommanderPanelConfiguration & rhc)
    { return C(DirViewParams) C(CoolBarLayout) C(StatusBar)
        C(DriveView) C(DriveViewHeight) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderConfiguration {
  AnsiString WindowParams;
  float LocalPanelWidth;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool CommandLine;
  bool ToolBar;
  TOperationSide CurrentPanel;
  bool ExplorerStyleSelection;
  bool PreserveLocalDirectory;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool CompareByTime;
  bool CompareBySize;
  bool SynchronizeBrowsing;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(CoolBarLayout) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel) C(ToolBar) C(CommandLine)
      C(ExplorerStyleSelection) C(PreserveLocalDirectory)
      C(CompareBySize) C(CompareByTime) C(SynchronizeBrowsing) 0; };

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
  TEditor Editor;
  AnsiString ExternalEditor;
  bool ExternalEditorText;
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
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(Editor) C(ExternalEditor) C(ExternalEditorText) C(FontName) C(FontHeight)
      C(FontCharset) C(FontStyle) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) C(SingleEditor) 0; };
};
//---------------------------------------------------------------------------
enum TQueueViewShow { qvShow, qvHideWhenEmpty, qvHide };
struct TQueueViewConfiguration {
  int Height;
  AnsiString Layout;
  TQueueViewShow Show;
  bool ToolBar;
  bool __fastcall operator !=(TQueueViewConfiguration & rhc)
    { return C(Height) C(Layout) C(Show) C(ToolBar) 0; };
};
#undef C
//---------------------------------------------------------------------------
class TBookmarks;
class TBookmarkList;
class TCustomCommands;
//---------------------------------------------------------------------------
class TWinConfiguration : public TCustomWinConfiguration
{
private:
  AnsiString FAutoStartSession;
  bool FCopyOnDoubleClick;
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

  void __fastcall SetCopyOnDoubleClick(bool value);
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

  bool __fastcall GetDDExtInstalled();

protected:
  virtual TStorage __fastcall GetStorage();
  bool __fastcall DumpResourceToFile(
    const AnsiString ResName, const AnsiString FileName);
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
  void __fastcall CheckTranslationVersion(const AnsiString FileName);
  void __fastcall DefaultLocalized();

public:
  __fastcall TWinConfiguration();
  virtual __fastcall ~TWinConfiguration();
  virtual void __fastcall Default();
  void __fastcall RestoreForm(AnsiString Data, TCustomForm * Form);
  AnsiString __fastcall StoreForm(TCustomForm * Form);
  void __fastcall ClearTemporaryLoginData();
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);
  static void ReformatFileNameCommand(AnsiString & Command);
  AnsiString __fastcall TemporaryTranferDir();

  __property TScpCommanderConfiguration ScpCommander = { read = FScpCommander, write = SetScpCommander };
  __property TScpExplorerConfiguration ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool SelectDirectories = { read = FSelectDirectories, write = SetSelectDirectories };
  __property AnsiString SelectMask = { read = FSelectMask, write = FSelectMask };
  __property bool ShowHiddenFiles = { read = FShowHiddenFiles, write = SetShowHiddenFiles };
  __property bool ShowInaccesibleDirectories = { read = FShowInaccesibleDirectories, write = SetShowInaccesibleDirectories };
  __property TEditorConfiguration Editor = { read = FEditor, write = SetEditor };
  __property TQueueViewConfiguration QueueView = { read = FQueueView, write = SetQueueView };
  __property AnsiString AutoStartSession = { read = FAutoStartSession, write = SetAutoStartSession };
  __property bool CopyOnDoubleClick = { read = FCopyOnDoubleClick, write = SetCopyOnDoubleClick };
  __property bool CopyOnDoubleClickConfirmation = { read = FCopyOnDoubleClickConfirmation, write = SetCopyOnDoubleClickConfirmation };
  __property bool DDAllowMove = { read = FDDAllowMove, write = SetDDAllowMove };
  __property bool DDAllowMoveInit = { read = FDDAllowMoveInit, write = SetDDAllowMoveInit };
  __property bool DDTransferConfirmation = { read = FDDTransferConfirmation, write = SetDDTransferConfirmation };
  __property bool LogWindowOnStartup = { read = FLogWindowOnStartup, write = SetLogWindowOnStartup };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool DimmHiddenFiles = { read = FDimmHiddenFiles, write = SetDimmHiddenFiles };
  __property AnsiString LogWindowParams = { read = FLogWindowParams, write = SetLogWindowParams };
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
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
