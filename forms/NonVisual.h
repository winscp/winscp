//---------------------------------------------------------------------------
#ifndef NonVisualH
#define NonVisualH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <Menus.hpp>
#include <ImgList.hpp>
#include <ExtCtrls.hpp>
#include <Rights.h>

#include "CustomScpExplorer.h"
//---------------------------------------------------------------------------
#define fcStatusBar        0x01
#define fcToolBar          0x02
#define fcCoolBar          0x03
#define fcLocalCoolBar     0x11
#define fcLocalStatusBar   0x12
#define fcRemoteCoolBar    0x13
#define fcRemoteStatusBar  0x14

#define fcExplorerMenuBand        0x0003
#define fcExplorerAddressBand     0x0103
#define fcExplorerToolbarBand     0x0203
#define fcExplorerSelectionBand   0x0303
#define fcExplorerSessionBand     0x0403
#define fcExplorerPreferencesBand 0x0503
#define fcExplorerSortBand        0x0603

#define fcCommanderMenuBand             0x0003
#define fcCommanderSessionBand          0x0103
#define fcCommanderPreferencesBand      0x0203
#define fcCommanderSelectionBand        0x0303
#define fcCommanderToolbarBand          0x0403
#define fcCommanderSortBand             0x0503
#define fcCommanderCommandsBand         0x0603

#define fcCommanderLocalHistoryBand     0x0111
#define fcCommanderLocalNavigationBand  0x0211
#define fcCommanderRemoteHistoryBand    0x0113
#define fcCommanderRemoteNavigationBand 0x0213
//---------------------------------------------------------------------------
class TNonVisualDataModule : public TDataModule
{
__published:	// IDE-managed Components
  TActionList *LogActions;
  TAction *LogClearAction;
  TAction *LogSelectAllAction;
  TAction *LogCopyAction;
  TAction *LogCloseAction;
  TPopupMenu *LogMemoPopup;
  TMenuItem *Clear1;
  TMenuItem *Close1;
  TMenuItem *Selectall1;
  TImageList *LogImages;
  TImageList *LogDisabledImages;
  TActionList *RightsActions;
  TImageList *RightsImages;
  TPopupMenu *RightsPopup;
  TAction *NoRightsAction;
  TAction *DefaultRightsAction;
  TAction *AllRightsAction;
  TMenuItem *Norights1;
  TMenuItem *Defaultrights1;
  TMenuItem *Allrights1;
  TImageList *ExplorerImages;
  TPopupMenu *RemoteDirViewPopup;
  TMenuItem *CurrentCopyMenuItem;
  TMenuItem *Delete1;
  TMenuItem *Properties1;
  TMenuItem *Rename1;
  TMenuItem *N1;
  TMenuItem *Moveto1;
  TAction *LocalSortByNameAction;
  TAction *LocalSortAscendingAction;
  TAction *LocalSortBySizeAction;
  TActionList *ExplorerActions;
  TAction *CurrentRenameAction;
  TAction *CurrentDeleteAction;
  TAction *CurrentCreateDirAction;
  TImageList *ExplorerDisabledImages;
  TAction *CurrentCycleStyleAction;
  TAction *CurrentIconAction;
  TAction *CurrentSmallIconAction;
  TAction *CurrentReportAction;
  TAction *CurrentListAction;
  TAction *CurrentCopyFocusedAction;
  TAction *CurrentMoveFocusedAction;
  TAction *CurrentDeleteFocusedAction;
  TAction *CurrentPropertiesFocusedAction;
  TAction *CurrentPropertiesAction;
  TAction *CurrentCopyAction;
  TAction *CurrentMoveAction;
  TMainMenu *ExplorerMenu;
  TMenuItem *ExplorerFileMenu;
  TMenuItem *Delete2;
  TMenuItem *Rename3;
  TMenuItem *Properties2;
  TMenuItem *N2;
  TMenuItem *Copyto2;
  TMenuItem *Moveto2;
  TMenuItem *ExplorerViewMenu;
  TMenuItem *LargeIcons2;
  TMenuItem *SmallIcons2;
  TMenuItem *List2;
  TMenuItem *Details2;
  TMenuItem *Createdirectory2;
  TMenuItem *Refresh1;
  TAction *RemoteBackAction;
  TAction *RemoteForwardAction;
  TAction *RemoteParentDirAction;
  TAction *RemoteRootDirAction;
  TAction *RemoteHomeDirAction;
  TAction *RemoteRefreshAction;
  TAction *AboutAction;
  TMenuItem *N4;
  TMenuItem *Toolbars1;
  TAction *StatusBarAction;
  TAction *ExplorerAddressBandAction;
  TAction *ExplorerMenuBandAction;
  TAction *ExplorerToolbarBandAction;
  TMenuItem *Address1;
  TMenuItem *Buttons1;
  TMenuItem *StatusBar1;
  TPopupMenu *ExplorerBarPopup;
  TMenuItem *Address2;
  TMenuItem *StandardButtons1;
  TMenuItem *N5;
  TMenuItem *StatusBar2;
  TPopupMenu *ExplorerStylePopup;
  TMenuItem *CurrentIconAction1;
  TMenuItem *CurrentSmallIconAction1;
  TMenuItem *CurrentListAction1;
  TMenuItem *CurrentReportAction1;
  TMenuItem *N6;
  TMenuItem *Goto1;
  TMenuItem *Back1;
  TMenuItem *Forward1;
  TMenuItem *Parentdirectory1;
  TMenuItem *Rootdirectory1;
  TMenuItem *Homedirectory1;
  TMenuItem *N7;
  TAction *RemoteOpenDirAction;
  TMenuItem *OpenDirectory1;
  TMenuItem *N8;
  TAction *SelectAction;
  TAction *UnselectAction;
  TAction *SelectAllAction;
  TAction *InvertSelectionAction;
  TAction *ExplorerSelectionBandAction;
  TAction *ClearSelectionAction;
  TMenuItem *SelectionButtons1;
  TMenuItem *SelectionButtons2;
  TTimer *SessionIdleTimer;
  TImageList *SessionImages;
  TAction *ViewLogAction;
  TMenuItem *LogWindow1;
  TAction *NewSessionAction;
  TAction *CloseSessionAction;
  TAction *SavedSessionsAction;
  TMainMenu *CommonScpMenu;
  TMenuItem *CommonSessionMenu;
  TMenuItem *Disconnect1;
  TMenuItem *Newsession1;
  TAction *ExplorerSessionBandAction;
  TMenuItem *SessionButtons1;
  TMenuItem *SessionButtons2;
  TMenuItem *SavedSessionsMenu;
  TMenuItem *N9;
  TMenuItem *N10;
  TMenuItem *Disconnect2;
  TAction *PreferencesAction;
  TAction *ExplorerPreferencesBandAction;
  TMenuItem *PreferencesButtons1;
  TMenuItem *PreferencesButtons2;
  TMenuItem *Preferences1;
  TAction *RemoteChangePathAction;
  TAction *LocalChangePathAction;
  TMainMenu *CommanderMenu;
  TMenuItem *CommanderLocalMenu;
  TMenuItem *Changedrive1;
  TMenuItem *CommanderRemoteMenu;
  TMenuItem *Changedirectory1;
  TMenuItem *CommonMarkMenu;
  TMenuItem *ClearSelection1;
  TMenuItem *InvertSelection1;
  TMenuItem *SelectAll2;
  TMenuItem *UnselectFiles1;
  TMenuItem *SelectFiles1;
  TMenuItem *CommanderCommandsMenu;
  TMenuItem *Copyto3;
  TMenuItem *Moveto3;
  TMenuItem *Delete3;
  TMenuItem *Rename2;
  TMenuItem *Properties3;
  TMenuItem *N12;
  TMenuItem *Createdirectory1;
  TMenuItem *N13;
  TAction *LocalOpenDirAction;
  TAction *LocalBackAction;
  TAction *LocalForwardAction;
  TAction *LocalParentDirAction;
  TAction *LocalRootDirAction;
  TAction *LocalHomeDirAction;
  TAction *LocalRefreshAction;
  TMenuItem *GoTo2;
  TMenuItem *Forward2;
  TMenuItem *Back2;
  TMenuItem *N14;
  TMenuItem *HomeDirectory2;
  TMenuItem *RootDirectory2;
  TMenuItem *ParentDirectory2;
  TMenuItem *N15;
  TMenuItem *OpenDirectory2;
  TMenuItem *N16;
  TMenuItem *N17;
  TMenuItem *GoTo3;
  TMenuItem *Forward3;
  TMenuItem *Back3;
  TMenuItem *N18;
  TMenuItem *HomeDirectory3;
  TMenuItem *RootDirectory3;
  TMenuItem *ParentDirectory3;
  TMenuItem *N19;
  TMenuItem *OpenDirectory3;
  TMenuItem *Refresh2;
  TMenuItem *Refresh3;
  TMenuItem *CommanderOptionsMenu;
  TMenuItem *Preferences2;
  TMenuItem *N24;
  TMenuItem *LogWindow2;
  TMenuItem *StatusBar3;
  TMenuItem *Toolbars2;
  TAction *ToolBarAction;
  TMenuItem *CommandToolbar1;
  TAction *CommanderMenuBandAction;
  TAction *CommanderSessionBandAction;
  TAction *CommanderPreferencesBandAction;
  TAction *CommanderSelectionBandAction;
  TAction *CommanderToolbarBandAction;
  TAction *CommanderLocalHistoryBandAction;
  TAction *CommanderLocalNavigationBandAction;
  TAction *CommanderRemoteHistoryBandAction;
  TAction *CommanderRemoteNavigationBandAction;
  TMenuItem *SessionButtons3;
  TMenuItem *SessionButtons4;
  TMenuItem *PreferencesButtons3;
  TMenuItem *StandardButtons2;
  TMenuItem *Leftpanel1;
  TMenuItem *RemotePanel1;
  TMenuItem *N20;
  TMenuItem *HistoryButtons1;
  TMenuItem *NavigationButtons1;
  TAction *LocalStatusBarAction;
  TAction *RemoteStatusBarAction;
  TMenuItem *StatusBar4;
  TMenuItem *N21;
  TMenuItem *HistoryButtons2;
  TMenuItem *NavigationButtons2;
  TMenuItem *N22;
  TMenuItem *StatusBar5;
  TPopupMenu *CommanderBarPopup;
  TMenuItem *PreferencesButtons4;
  TMenuItem *SelectionButtons3;
  TMenuItem *SessionButtons5;
  TMenuItem *StandardButtons3;
  TMenuItem *LocalPanel1;
  TMenuItem *StatusBar6;
  TMenuItem *N23;
  TMenuItem *NavigationButtons3;
  TMenuItem *HistoryButtons3;
  TMenuItem *RemotePanel2;
  TMenuItem *StatusBar7;
  TMenuItem *N25;
  TMenuItem *NavigationButtons4;
  TMenuItem *HistoryButtons4;
  TMenuItem *N26;
  TMenuItem *StatusBar8;
  TMenuItem *CommandsToolbar1;
  TMenuItem *N27;
  TPopupMenu *RemotePanelPopup;
  TMenuItem *HistoryButtons5;
  TMenuItem *NavigationButtons5;
  TMenuItem *N28;
  TMenuItem *StatusBar9;
  TPopupMenu *LocalPanelPopup;
  TMenuItem *HistoryButtons6;
  TMenuItem *NavigationButtons6;
  TMenuItem *N29;
  TMenuItem *StatusBar10;
  TMenuItem *N30;
  TMenuItem *Sort1;
  TMenuItem *Vzestupn1;
  TMenuItem *N31;
  TMenuItem *N32;
  TAction *LocalSortByAttrAction;
  TAction *LocalSortByTypeAction;
  TAction *LocalSortByChangedAction;
  TAction *CommanderSortBandAction;
  TMenuItem *Sortbuttons1;
  TMenuItem *BySize1;
  TMenuItem *ByType1;
  TMenuItem *ByModification1;
  TMenuItem *ByAttributes1;
  TMenuItem *SortButtons2;
  TAction *RemoteSortAscendingAction;
  TAction *RemoteSortByNameAction;
  TAction *RemoteSortBySizeAction;
  TAction *RemoteSortByRightsAction;
  TAction *RemoteSortByChangedAction;
  TAction *RemoteSortByOwnerAction;
  TAction *RemoteSortByGroupAction;
  TMenuItem *Sort2;
  TMenuItem *ByAttributes2;
  TMenuItem *ByModification2;
  TMenuItem *BySize2;
  TMenuItem *ByName1;
  TMenuItem *N34;
  TMenuItem *Ascending1;
  TMenuItem *ByOwner1;
  TMenuItem *ByGroup1;
  TAction *CurrentSortByTypeAction;
  TAction *CurrentSortAscendingAction;
  TAction *CurrentSortByNameAction;
  TAction *CurrentSortBySizeAction;
  TAction *CurrentSortByRightsAction;
  TAction *CurrentSortByChangedAction;
  TAction *CurrentSortByOwnerAction;
  TAction *CurrentSortByGroupAction;
  TAction *ExplorerSortBandAction;
  TMenuItem *SortButtons3;
  TMenuItem *SortButtons4;
  TMenuItem *Sort3;
  TMenuItem *ByGroup2;
  TMenuItem *ByOwner2;
  TMenuItem *ByPermissions1;
  TMenuItem *BySize3;
  TMenuItem *ByModification3;
  TMenuItem *ByName2;
  TMenuItem *N35;
  TMenuItem *Ascending2;
  TMenuItem *N36;
  TMenuItem *CommonHelpMenu;
  TMenuItem *About1;
  TPopupMenu *LocalDirViewColumnPopup;
  TAction *SortColumnAscendingAction;
  TAction *SortColumnDescendingAction;
  TMenuItem *SortAscending1;
  TMenuItem *SortDescending1;
  TAction *HomepageAction;
  TAction *HistoryPageAction;
  TAction *RequirementsPageAction;
  TMenuItem *N11;
  TMenuItem *WinSCPhomepage1;
  TMenuItem *Versionhistory1;
  TMenuItem *Programrequirements1;
  TAction *SaveCurrentSessionAction;
  TMenuItem *Savesession1;
  TAction *LocalSortByExtAction;
  TAction *RemoteSortByExtAction;
  TAction *CurrentSortByExtAction;
  TMenuItem *ByExtension1;
  TMenuItem *ByExtension2;
  TMenuItem *ByExtension3;
  TAction *ShowHideRemoteNameColumnAction;
  TAction *ShowHideRemoteExtColumnAction;
  TAction *ShowHideRemoteSizeColumnAction;
  TAction *ShowHideRemoteChangedColumnAction;
  TAction *ShowHideRemoteRightsColumnAction;
  TAction *ShowHideRemoteOwnerColumnAction;
  TAction *ShowHideRemoteGroupColumnAction;
  TAction *ShowHideLocalNameColumnAction;
  TAction *ShowHideLocalExtColumnAction;
  TAction *ShowHideLocalTypeColumnAction;
  TAction *ShowHideLocalSizeColumnAction;
  TAction *ShowHideLocalChangedColumnAction;
  TAction *ShowHideLocalAttrColumnAction;
  TMenuItem *Showcolumns1;
  TMenuItem *Name1;
  TMenuItem *Size1;
  TMenuItem *Type1;
  TMenuItem *Modification1;
  TMenuItem *Attributes1;
  TMenuItem *Showcolumns2;
  TMenuItem *Attributes2;
  TMenuItem *Modification2;
  TMenuItem *Size2;
  TMenuItem *Name2;
  TMenuItem *Owner1;
  TMenuItem *Group1;
  TPopupMenu *RemoteDirViewColumnPopup;
  TMenuItem *MenuItem1;
  TMenuItem *MenuItem2;
  TMenuItem *Showcolumns3;
  TMenuItem *Attributes3;
  TMenuItem *Modification3;
  TMenuItem *Type2;
  TMenuItem *Size3;
  TMenuItem *Name3;
  TMenuItem *N37;
  TMenuItem *Showcolumns4;
  TMenuItem *Group2;
  TMenuItem *Owner2;
  TMenuItem *Permissions1;
  TMenuItem *Modification4;
  TMenuItem *Size4;
  TMenuItem *Name4;
  TMenuItem *N38;
  TMenuItem *Showcolumns5;
  TMenuItem *Group3;
  TMenuItem *Owner3;
  TMenuItem *Permissions2;
  TMenuItem *Modification5;
  TMenuItem *Size5;
  TMenuItem *Name5;
  TAction *HideColumnAction;
  TMenuItem *Hidecolumn1;
  TMenuItem *Hidecolumn2;
  TMenuItem *N39;
  TAction *CompareDirectoriesAction;
  TMenuItem *Comparedirectories1;
  TAction *CommanderCommandsBandAction;
  TMenuItem *CommandsButtons1;
  TMenuItem *CommandsButtons2;
  TAction *SynchronizeAction;
  TMenuItem *Keepremotedirectoryuptodate1;
  TImageList *ArrowImages;
  TAction *ForumPageAction;
  TMenuItem *Supportforum1;
  TAction *LocalAddBookmarkAction;
  TAction *RemoteAddBookmarkAction;
  TMenuItem *Addtobookmarks1;
  TMenuItem *N33;
  TMenuItem *Addtobookmarks2;
  TMenuItem *Addtobookmarks3;
  TAction *ConsoleAction;
  TMenuItem *OpenTerminal1;
  TMenuItem *ExporerCommandsMenu;
  TMenuItem *N3;
  TMenuItem *N40;
  TMenuItem *Openterminal2;
  TAction *LocalExploreDirectory;
  TMenuItem *Exploredirectory1;
  TAction *LeaveRightsAsIsAction;
  TMenuItem *Leaveasis1;
  TAction *CurrentEditAction;
  TMenuItem *CurentEditMenuItem;
  TMenuItem *Edit2;
  TMenuItem *Edit3;
  TAction *CurrentEditAlternativeAction;
  TMenuItem *Edit4;
  TMenuItem *Editalternative1;
  TAction *CurrentOpenAction;
  TMenuItem *CurrentOpenMenuItem;
  TMenuItem *Open2;
  TMenuItem *N41;
  TMenuItem *Open3;
  TMenuItem *N42;
  TAction *SynchorizeBrowsingAction;
  TMenuItem *Synchronizebrowsing1;
  TAction *AddEditLinkAction;
  TMenuItem *Addeditlink1;
  TMenuItem *Addeditlink2;
  void __fastcall LogActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall LogActionsExecute(TBasicAction *Action, bool &Handled);
  void __fastcall RightsActionsExecute(TBasicAction *Action, bool &Handled);
  void __fastcall RightsActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall ExplorerActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall ExplorerActionsExecute(TBasicAction *Action, bool &Handled);
  void __fastcall SessionIdleTimerTimer(TObject *Sender);
private:
  TRightsFrame * FRightsFrame;
  TListColumn * FListColumn;
  TCustomScpExplorerForm * FScpExplorer;
protected:
  void __fastcall CreateSessionListMenu();
  TCustomDirView * __fastcall DirView(TOperationSide Side) { return ScpExplorer->DirView(Side); }
  void __fastcall OpenBrowser(AnsiString URL);
  void __fastcall SessionItemClick(TObject * Sender);
public:
  void __fastcall CommanderShortcuts();
  void __fastcall ExplorerShortcuts();		// User declarations
  __fastcall TNonVisualDataModule(TComponent* Owner);
  __property TListColumn * ListColumn = { read = FListColumn, write = FListColumn };
  __property TRightsFrame * RightsFrame = { read = FRightsFrame, write = FRightsFrame };
  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = FScpExplorer };
};
//---------------------------------------------------------------------------
extern PACKAGE TNonVisualDataModule *NonVisualDataModule;
//---------------------------------------------------------------------------
#endif
