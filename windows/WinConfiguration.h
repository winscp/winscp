//---------------------------------------------------------------------------
#ifndef WinConfigurationH
#define WinConfigurationH
//---------------------------------------------------------------------------
#include "GUIConfiguration.h"
//---------------------------------------------------------------------------
enum TInterface { ifCommander, ifExplorer };
enum TEditor { edInternal, edExternal };
extern const char ShellCommandFileNamePattern[];
enum TLogView { lvNone, lvWindow, pvPanel };
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
  bool __fastcall operator !=(TScpExplorerConfiguration & rhc)
    { return C(WindowParams) C(DirViewParams) C(CoolBarLayout) C(StatusBar)
        C(LastLocalTargetDirectory) C(ViewStyle) C(ShowFullAddress) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderPanelConfiguration {
  AnsiString DirViewParams;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool __fastcall operator !=(TScpCommanderPanelConfiguration & rhc)
    { return C(DirViewParams) C(CoolBarLayout) C(StatusBar) 0; };
};
//---------------------------------------------------------------------------
struct TScpCommanderConfiguration {
  AnsiString WindowParams;
  float LocalPanelWidth;
  AnsiString CoolBarLayout;
  bool StatusBar;
  bool ToolBar;
  TOperationSide CurrentPanel;
  bool ExplorerStyleSelection;
  TScpCommanderPanelConfiguration LocalPanel;
  TScpCommanderPanelConfiguration RemotePanel;
  bool __fastcall operator !=(TScpCommanderConfiguration & rhc)
    { return C(WindowParams) C(LocalPanelWidth) C(CoolBarLayout) C(StatusBar)
      C(LocalPanel) C(RemotePanel) C(CurrentPanel) C(ToolBar)
      C(ExplorerStyleSelection) 0; };
};
//---------------------------------------------------------------------------
struct TEditorConfiguration {
  TEditor Editor;
  AnsiString ExternalEditor;
  AnsiString FontName;
  int FontHeight;
  int FontCharset;
  int FontStyle;
  bool WordWrap;
  AnsiString FindText;
  AnsiString ReplaceText;
  bool FindMatchCase;
  bool FindWholeWord;
  bool __fastcall operator !=(TEditorConfiguration & rhc)
    { return C(Editor) C(ExternalEditor) C(FontName) C(FontHeight)
      C(FontCharset) C(FontStyle) C(WordWrap) C(FindText) C(ReplaceText)
      C(FindMatchCase) C(FindWholeWord) 0; };
};
#undef C
//---------------------------------------------------------------------------
class TBookmarks;
class TBookmarkList;
//---------------------------------------------------------------------------
class TWinConfiguration : public TGUIConfiguration
{
private:
  TInterface FInterface;
  AnsiString FAutoStartSession;
  bool FCopyOnDoubleClick;
  bool FCopyOnDoubleClickConfirmation;
  bool FDDAllowMove;
  bool FDDTransferConfirmation;
  bool FDeleteToRecycleBin;
  bool FDimmHiddenFiles;
  TLogView FLogView;
  bool FLogWindowOnStartup;
  AnsiString FLogWindowParams;
  AnsiString FMaskHistory;
  TScpCommanderConfiguration FScpCommander;
  TScpExplorerConfiguration FScpExplorer;
  bool FSelectDirectories;
  AnsiString FSelectMask;
  bool FShowHiddenFiles;
  bool FShowInaccesibleDirectories;
  bool FShowAdvancedLoginOptions;
  bool FConfirmDeleting;
  bool FUseLocationProfiles;
  AnsiString FDDTemporaryDirectory;
  bool FDDWarnLackOfTempSpace;
  bool FConfirmClosingSession;
  double FDDWarnLackOfTempSpaceRatio;
  AnsiString FTemporarySessionFile;
  AnsiString FTemporaryKeyFile;
  TBookmarks * FBookmarks;
  TStrings * FCommandsHistory;
  TEditorConfiguration FEditor;
  bool FEmbeddedSessions;
  bool FExpertMode;
  bool FDisableOpenEdit;

  void __fastcall SetCopyOnDoubleClick(bool value);
  void __fastcall SetCopyOnDoubleClickConfirmation(bool value);
  void __fastcall SetDDAllowMove(bool value);
  void __fastcall SetDDTransferConfirmation(bool value);
  void __fastcall SetDeleteToRecycleBin(bool value);
  void __fastcall SetDimmHiddenFiles(bool value);
  void __fastcall SetInterface(TInterface value);
  void __fastcall SetLogView(TLogView value);
  void __fastcall SetLogWindowOnStartup(bool value);
  void __fastcall SetLogWindowParams(AnsiString value);
  void __fastcall SetMaskHistory(AnsiString value);
  void __fastcall SetScpCommander(TScpCommanderConfiguration value);
  void __fastcall SetScpExplorer(TScpExplorerConfiguration value);
  void __fastcall SetSelectDirectories(bool value);
  void __fastcall SetShowHiddenFiles(bool value);
  void __fastcall SetShowInaccesibleDirectories(bool value);
  void __fastcall SetShowAdvancedLoginOptions(bool value);
  void __fastcall SetConfirmDeleting(bool value);
  void __fastcall SetUseLocationProfiles(bool value);
  void __fastcall SetDDTemporaryDirectory(AnsiString value);
  void __fastcall SetDDWarnLackOfTempSpace(bool value);
  void __fastcall SetConfirmClosingSession(bool value);
  void __fastcall SetDDWarnLackOfTempSpaceRatio(double value);
  void __fastcall SetBookmarks(AnsiString Key, TBookmarkList * value);
  TBookmarkList * __fastcall GetBookmarks(AnsiString Key);
  void __fastcall SetAutoStartSession(AnsiString value);
  void __fastcall SetCommandsHistory(TStrings * value);
  void __fastcall SetExpertMode(bool value);
  void __fastcall SetEditor(TEditorConfiguration value);

protected:
  virtual TStorage __fastcall GetStorage();
  bool __fastcall DumpResourceToFile(
    const AnsiString ResName, const AnsiString FileName);
  virtual void __fastcall SaveSpecial(THierarchicalStorage * Storage);
  virtual void __fastcall LoadSpecial(THierarchicalStorage * Storage);
  virtual AnsiString __fastcall GetDefaultKeyFile();
  virtual void __fastcall ModifyAll();

public:
  __fastcall TWinConfiguration();
  __fastcall ~TWinConfiguration();
  virtual void __fastcall Default();
  virtual void __fastcall Load();
  void __fastcall RestoreForm(AnsiString Data, TCustomForm * Form);
  AnsiString __fastcall StoreForm(TCustomForm * Form);
  void __fastcall ClearTemporaryLoginData();
  virtual THierarchicalStorage * CreateScpStorage(bool SessionList);
  static void ReformatFileNameCommand(AnsiString & Command);

  __property TLogView LogView = { read = FLogView, write = SetLogView };
  __property TInterface Interface = { read = FInterface, write = SetInterface };
  __property TScpCommanderConfiguration ScpCommander = { read = FScpCommander, write = SetScpCommander };
  __property TScpExplorerConfiguration ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool SelectDirectories = { read = FSelectDirectories, write = SetSelectDirectories };
  __property AnsiString SelectMask = { read = FSelectMask, write = FSelectMask };
  __property bool ShowHiddenFiles = { read = FShowHiddenFiles, write = SetShowHiddenFiles };
  __property bool ShowInaccesibleDirectories = { read = FShowInaccesibleDirectories, write = SetShowInaccesibleDirectories };
  __property bool ShowAdvancedLoginOptions = { read = FShowAdvancedLoginOptions, write = SetShowAdvancedLoginOptions};
  __property TEditorConfiguration Editor = { read = FEditor, write = SetEditor };
  __property AnsiString AutoStartSession = { read = FAutoStartSession, write = SetAutoStartSession };
  __property bool CopyOnDoubleClick = { read = FCopyOnDoubleClick, write = SetCopyOnDoubleClick };
  __property bool CopyOnDoubleClickConfirmation = { read = FCopyOnDoubleClickConfirmation, write = SetCopyOnDoubleClickConfirmation };
  __property bool DDAllowMove = { read = FDDAllowMove, write = SetDDAllowMove };
  __property bool DDTransferConfirmation = { read = FDDTransferConfirmation, write = SetDDTransferConfirmation };
  __property bool LogWindowOnStartup = { read = FLogWindowOnStartup, write = SetLogWindowOnStartup };
  __property bool DeleteToRecycleBin = { read = FDeleteToRecycleBin, write = SetDeleteToRecycleBin };
  __property bool DimmHiddenFiles = { read = FDimmHiddenFiles, write = SetDimmHiddenFiles };
  __property AnsiString LogWindowParams = { read = FLogWindowParams, write = SetLogWindowParams };
  __property AnsiString MaskHistory = { read = FMaskHistory, write = SetMaskHistory };
  __property bool ConfirmDeleting = { read = FConfirmDeleting, write = SetConfirmDeleting};
  __property bool UseLocationProfiles = { read = FUseLocationProfiles, write = SetUseLocationProfiles};
  __property AnsiString DDTemporaryDirectory  = { read=FDDTemporaryDirectory, write=SetDDTemporaryDirectory };
  __property bool DDWarnLackOfTempSpace  = { read=FDDWarnLackOfTempSpace, write=SetDDWarnLackOfTempSpace };
  __property bool ConfirmClosingSession  = { read=FConfirmClosingSession, write=SetConfirmClosingSession };
  __property double DDWarnLackOfTempSpaceRatio  = { read=FDDWarnLackOfTempSpaceRatio, write=SetDDWarnLackOfTempSpaceRatio };
  __property TBookmarkList * Bookmarks[AnsiString Key] = { read = GetBookmarks, write = SetBookmarks };
  __property TStrings * CommandsHistory = { read = FCommandsHistory, write = SetCommandsHistory };
  __property bool EmbeddedSessions = { read = FEmbeddedSessions };
  __property bool ExpertMode = { read = FExpertMode, write = SetExpertMode };
  __property bool DisableOpenEdit = { read = FDisableOpenEdit };
};
//---------------------------------------------------------------------------
#define WinConfiguration ((TWinConfiguration *) Configuration)
//---------------------------------------------------------------------------
#endif
