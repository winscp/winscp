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
#include "TB2Item.hpp"
#include "TBX.hpp"
#include "TB2ExtItems.hpp"
#include "TBXExtItems.hpp"
#include "TBXToolPals.hpp"
#include <System.Actions.hpp>
//---------------------------------------------------------------------------
#define fcStatusBar        0x01
#define fcToolBar2         0x02
#define fcLocalStatusBar   0x12
#define fcRemoteStatusBar  0x14
#define fcRemotePopup      0x17
#define fcCommandLinePanel 0x18
#define fcQueueView        0x19
#define fcQueueToolbar     0x1A
#define fcLocalTree        0x1B
#define fcRemoteTree       0x1C
#define fcSessionToolbar   0x1E
#define fcCustomCommandsBand 0x1F
#define fcColorMenu        0x20
#define fcTransferDropDown 0x22
#define fcTransferList     0x23
#define fcTransferLabel    0x24
#define fcSessionsTabs     0x25
#define fcLocalPopup       0x26
#define fcRemotePathComboBox 0x27
#define fcQueueFileList    0x28
#define fcMenu             0x29

#define fcExplorerMenuBand        0x31
#define fcExplorerAddressBand     0x32
#define fcExplorerToolbarBand     0x33
#define fcExplorerSelectionBand   0x34
#define fcExplorerSessionBand     0x35
#define fcExplorerPreferencesBand 0x36
#define fcExplorerSortBand        0x37
#define fcExplorerUpdatesBand     0x38
#define fcExplorerTransferBand    0x39
#define fcExplorerCustomCommandsBand 0x40

#define fcCommanderMenuBand             0x51
#define fcCommanderSessionBand          0x52
#define fcCommanderPreferencesBand      0x53
#define fcCommanderSortBand             0x56
#define fcCommanderCommandsBand         0x57
#define fcCommanderUpdatesBand          0x58
#define fcCommanderTransferBand         0x59
#define fcCommanderCustomCommandsBand   0x61

#define fcCommanderLocalHistoryBand     0x71
#define fcCommanderLocalNavigationBand  0x72
#define fcCommanderLocalFileBand        0x73
#define fcCommanderLocalSelectionBand   0x74
#define fcCommanderRemoteHistoryBand    0x75
#define fcCommanderRemoteNavigationBand 0x76
#define fcCommanderRemoteFileBand       0x77
#define fcCommanderRemoteSelectionBand  0x78
//---------------------------------------------------------------------------
class TNonVisualDataModule : public TDataModule
{
__published:    // IDE-managed Components
  TAction *LocalSortByNameAction2;
  TAction *LocalSortAscendingAction2;
  TAction *LocalSortBySizeAction2;
  TActionList *ExplorerActions;
  TAction *CurrentRenameAction;
  TAction *CurrentDeleteAction;
  TAction *CurrentCreateDirAction;
  TAction *RemoteCycleStyleAction;
  TAction *RemoteIconAction;
  TAction *RemoteSmallIconAction;
  TAction *RemoteReportAction;
  TAction *RemoteListAction;
  TAction *CurrentDeleteFocusedAction;
  TAction *CurrentPropertiesFocusedAction;
  TAction *CurrentPropertiesAction;
  TAction *RemoteBackAction;
  TAction *RemoteForwardAction;
  TAction *RemoteParentDirAction;
  TAction *RemoteRootDirAction;
  TAction *RemoteHomeDirAction;
  TAction *RemoteRefreshAction;
  TAction *AboutAction;
  TAction *StatusBarAction;
  TAction *SessionsTabsAction2;
  TAction *ExplorerAddressBandAction;
  TAction *ExplorerMenuBandAction;
  TAction *ExplorerToolbarBandAction;
  TAction *RemoteOpenDirAction;
  TAction *SelectAction;
  TAction *UnselectAction;
  TAction *SelectAllAction;
  TAction *InvertSelectionAction;
  TAction *ExplorerSelectionBandAction;
  TAction *ClearSelectionAction;
  TTimer *SessionIdleTimer;
  TAction *SiteManagerAction;
  TAction *CloseTabAction;
  TAction *SavedSessionsAction2;
  TAction *WorkspacesAction;
  TAction *ExplorerSessionBandAction2;
  TAction *PreferencesAction;
  TAction *ExplorerPreferencesBandAction;
  TAction *RemoteChangePathAction2;
  TAction *LocalChangePathAction2;
  TAction *LocalOpenDirAction;
  TAction *LocalBackAction;
  TAction *LocalForwardAction;
  TAction *LocalParentDirAction;
  TAction *LocalRootDirAction;
  TAction *LocalHomeDirAction;
  TAction *LocalRefreshAction;
  TAction *ToolBar2Action;
  TAction *CommanderMenuBandAction;
  TAction *CommanderSessionBandAction2;
  TAction *CommanderPreferencesBandAction;
  TAction *CommanderLocalHistoryBandAction2;
  TAction *CommanderLocalNavigationBandAction2;
  TAction *CommanderRemoteHistoryBandAction2;
  TAction *CommanderRemoteNavigationBandAction2;
  TAction *LocalStatusBarAction2;
  TAction *RemoteStatusBarAction2;
  TAction *LocalSortByAttrAction2;
  TAction *LocalSortByTypeAction2;
  TAction *LocalSortByChangedAction2;
  TAction *CommanderSortBandAction;
  TAction *RemoteSortAscendingAction2;
  TAction *RemoteSortByNameAction2;
  TAction *RemoteSortBySizeAction2;
  TAction *RemoteSortByRightsAction2;
  TAction *RemoteSortByChangedAction2;
  TAction *RemoteSortByOwnerAction2;
  TAction *RemoteSortByGroupAction2;
  TAction *CurrentSortByTypeAction2;
  TAction *CurrentSortAscendingAction;
  TAction *CurrentSortByNameAction;
  TAction *CurrentSortBySizeAction;
  TAction *CurrentSortByRightsAction;
  TAction *CurrentSortByChangedAction;
  TAction *CurrentSortByOwnerAction;
  TAction *CurrentSortByGroupAction;
  TAction *ExplorerSortBandAction;
  TAction *SortColumnAscendingAction;
  TAction *SortColumnDescendingAction;
  TAction *HomepageAction;
  TAction *HistoryPageAction;
  TAction *SaveCurrentSessionAction2;
  TAction *LocalSortByExtAction2;
  TAction *RemoteSortByExtAction2;
  TAction *CurrentSortByExtAction;
  TAction *ShowHideRemoteNameColumnAction2;
  TAction *ShowHideRemoteExtColumnAction2;
  TAction *ShowHideRemoteSizeColumnAction2;
  TAction *ShowHideRemoteChangedColumnAction2;
  TAction *ShowHideRemoteRightsColumnAction2;
  TAction *ShowHideRemoteOwnerColumnAction2;
  TAction *ShowHideRemoteGroupColumnAction2;
  TAction *ShowHideLocalNameColumnAction2;
  TAction *ShowHideLocalExtColumnAction2;
  TAction *ShowHideLocalTypeColumnAction2;
  TAction *ShowHideLocalSizeColumnAction2;
  TAction *ShowHideLocalChangedColumnAction2;
  TAction *ShowHideLocalAttrColumnAction2;
  TAction *HideColumnAction;
  TAction *CompareDirectoriesAction2;
  TAction *CommanderCommandsBandAction;
  TAction *SynchronizeAction;
  TAction *ForumPageAction;
  TAction *LocalAddBookmarkAction2;
  TAction *RemoteAddBookmarkAction2;
  TAction *ConsoleAction;
  TAction *LocalExploreDirectoryAction;
  TAction *CurrentEditAction;
  TAction *CurrentOpenAction;
  TAction *SynchronizeBrowsingAction2;
  TAction *CurrentAddEditLinkAction;
  TAction *CloseApplicationAction2;
  TAction *OpenedTabsAction;
  TAction *CustomCommandsFileAction;
  TAction *CustomCommandsCustomizeAction;
  TAction *CheckForUpdatesAction;
  TAction *PuttyAction;
  TAction *DonatePageAction;
  TAction *FileSystemInfoAction;
  TAction *ClearCachesAction;
  TAction *FullSynchronizeAction;
  TAction *RemoteMoveToAction;
  TAction *RemoteMoveToFocusedAction;
  TAction *SelectOneAction;
  TAction *ShowHiddenFilesAction;
  TAction *FormatSizeBytesNoneAction;
  TAction *CommandLinePanelAction;
  TAction *LocalPathToClipboardAction2;
  TAction *RemotePathToClipboardAction2;
  TAction *GoToCommandLineAction;
  TAction *FileListToCommandLineAction;
  TAction *FileListToClipboardAction;
  TAction *FullFileListToClipboardAction;
  TAction *QueueItemQueryAction;
  TAction *QueueItemPromptAction;
  TAction *QueueItemErrorAction;
  TAction *QueueItemDeleteAction;
  TAction *QueueItemExecuteAction;
  TAction *QueueGoToAction;
  TAction *QueueItemUpAction;
  TAction *QueueItemDownAction;
  TAction *QueueToggleShowAction;
  TAction *QueueShowAction;
  TAction *QueueHideWhenEmptyAction;
  TAction *QueueHideAction;
  TAction *QueueToolbarAction;
  TAction *QueuePreferencesAction;
  TAction *PasteAction3;
  TAction *RemoteTreeAction;
  TAction *LocalTreeAction;
  TAction *GoToTreeAction;
  TAction *NewFileAction;
  TAction *RemoteCopyToFocusedAction;
  TAction *RemoteCopyToAction;
  TAction *FileGenerateUrlAction2;
  TAction *TableOfContentsAction;
  TTBXPopupMenu *CommanderBarPopup;
  TTBXItem *SessionButtons5;
  TTBXItem *PreferencesButtons4;
  TTBXItem *SortButtons2;
  TTBXItem *CommandsButtons2;
  TTBXSeparatorItem *N26;
  TTBXItem *CommandLine2;
  TTBXItem *CommandsToolbar1;
  TTBXItem *StatusBar8;
  TTBXSeparatorItem *N27;
  TTBXSubmenuItem *LocalPanel1;
  TTBXItem *HistoryButtons3;
  TTBXItem *NavigationButtons3;
  TTBXSeparatorItem *N23;
  TTBXItem *Tree7;
  TTBXSeparatorItem *N77;
  TTBXItem *StatusBar6;
  TTBXSubmenuItem *RemotePanel2;
  TTBXItem *HistoryButtons4;
  TTBXItem *NavigationButtons4;
  TTBXSeparatorItem *N25;
  TTBXItem *Tree8;
  TTBXSeparatorItem *N78;
  TTBXItem *StatusBar7;
  TTBXSubmenuItem *Options1;
  TTBXItem *Show5;
  TTBXItem *HidewhenEmpty5;
  TTBXItem *Hide4;
  TTBXSeparatorItem *N69;
  TTBXItem *Toolbar4;
  TTBXSeparatorItem *N68;
  TTBXItem *Customize4;
  TTBXPopupMenu *RemotePanelPopup;
  TTBXItem *CopyPathtoClipboard1;
  TTBXSeparatorItem *N51;
  TTBXItem *HistoryButtons5;
  TTBXItem *NavigationButtons5;
  TTBXSeparatorItem *N28;
  TTBXItem *Tree5;
  TTBXSeparatorItem *N75;
  TTBXItem *StatusBar9;
  TTBXPopupMenu *LocalPanelPopup;
  TTBXItem *CopyPathtoClipboard2;
  TTBXSeparatorItem *N52;
  TTBXItem *HistoryButtons6;
  TTBXItem *NavigationButtons6;
  TTBXSeparatorItem *N29;
  TTBXItem *Tree6;
  TTBXSeparatorItem *N76;
  TTBXItem *StatusBar10;
  TTBXPopupMenu *RemoteFilePopup;
  TTBXItem *RemoteOpenMenuItem;
  TTBXSubmenuItem *RemoteEditMenuItem;
  TTBXSubmenuItem *RemoteCopyMenuItem;
  TTBXItem *Duplicate3;
  TTBXItem *Moveto1;
  TTBXItem *Moveto6;
  TTBXItem *Delete1;
  TTBXItem *Rename1;
  TTBXSeparatorItem *N45;
  TTBXSubmenuItem *RemoteFilePopupCustomCommandsMenu;
  TTBXSubmenuItem *FileNames3;
  TTBXItem *InserttoCommandLine2;
  TTBXItem *CopytoClipboard3;
  TTBXItem *CopytoClipboardIncludePaths3;
  TTBXItem *CopyURLtoClipboard3;
  TTBXSeparatorItem *N1;
  TTBXItem *Properties1;
  TTBXPopupMenu *RemoteDirViewPopup;
  TTBXSubmenuItem *GoTo4;
  TTBXItem *OpenDirectoryBookmark3;
  TTBXSeparatorItem *N81;
  TTBXItem *ParentDirectory4;
  TTBXItem *RootDirectory4;
  TTBXItem *HomeDirectory4;
  TTBXSeparatorItem *N80;
  TTBXItem *Back4;
  TTBXItem *Forward4;
  TTBXItem *Refresh4;
  TTBXItem *AddToBookmarks4;
  TTBXItem *CopyPathtoClipboard6;
  TTBXSeparatorItem *N79;
  TTBXPopupMenu *LocalDirViewPopup;
  TTBXSubmenuItem *GoTo5;
  TTBXItem *OpenDirectoryBookmark4;
  TTBXItem *ExploreDirectory2;
  TTBXSeparatorItem *N84;
  TTBXItem *ParentDirectory5;
  TTBXItem *RootDirectory5;
  TTBXItem *HomeDirectory5;
  TTBXSeparatorItem *N83;
  TTBXItem *Back5;
  TTBXItem *Forward5;
  TTBXItem *Refresh5;
  TTBXItem *AddToBookmarks5;
  TTBXItem *CopyPathtoClipboard7;
  TTBXSeparatorItem *N82;
  TTBXPopupMenu *LocalDirViewColumnPopup;
  TTBXItem *SortAscending1;
  TTBXItem *SortDescending1;
  TTBXItem *Hidecolumn1;
  TTBXSeparatorItem *N37;
  TTBXSubmenuItem *LocalColumnsSubmenuItem;
  TTBXItem *Name3;
  TTBXItem *Size3;
  TTBXItem *Type2;
  TTBXItem *Modification3;
  TTBXItem *Attributes3;
  TTBXPopupMenu *RemoteDirViewColumnPopup;
  TTBXItem *MenuItem1;
  TTBXItem *MenuItem2;
  TTBXItem *Hidecolumn2;
  TTBXSeparatorItem *N38;
  TTBXSubmenuItem *RemoteColumnsSubmenuItem;
  TTBXItem *Name4;
  TTBXItem *Size4;
  TTBXItem *Modification4;
  TTBXItem *Permissions1;
  TTBXItem *Owner2;
  TTBXItem *Group2;
  TTBXPopupMenu *SessionsPopup;
  TTBXPopupMenu *QueuePopup;
  TTBXItem *ShowQuery1;
  TTBXItem *ShowError1;
  TTBXItem *ShowPrompt1;
  TTBXSeparatorItem *N53;
  TTBXItem *ExecuteNow1;
  TTBXItem *Delete4;
  TTBXSeparatorItem *N54;
  TTBXItem *MoveUp1;
  TTBXItem *MoveDown1;
  TTBXItem *QueueEnableItem;
  TTBXSeparatorItem *N67;
  TTBXSubmenuItem *Queue2;
  TTBXItem *Show4;
  TTBXItem *HidewhenEmpty4;
  TTBXItem *Hide3;
  TTBXSeparatorItem *N66;
  TTBXItem *Toolbar3;
  TTBXSeparatorItem *N65;
  TTBXItem *Customize3;
  TTBXPopupMenu *ExplorerBarPopup;
  TTBXItem *Address2;
  TTBXItem *StandardButtons1;
  TTBXItem *SelectionButtons1;
  TTBXItem *SessionButtons2;
  TTBXItem *PreferencesButtons1;
  TTBXItem *SortButtons3;
  TTBXSeparatorItem *N5;
  TTBXItem *StatusBar2;
  TTBXSeparatorItem *N72;
  TTBXSubmenuItem *Queue7;
  TTBXItem *Show6;
  TTBXItem *HidewhenEmpty6;
  TTBXItem *Hide5;
  TTBXSeparatorItem *N71;
  TTBXItem *Toolbar5;
  TTBXSeparatorItem *N70;
  TTBXItem *Customize5;
  TTBXItem *Tree4;
  TTBXItem *RemoteSortByExtColumnPopupItem;
  TTBXItem *TBXItem67;
  TTBXItem *LocalSortByExtColumnPopupItem;
  TAction *FileListFromClipboardAction;
  TAction *ShowHideRemoteLinkTargetColumnAction2;
  TTBXItem *TBXItem1;
  TAction *DownloadPageAction;
  TAction *CommanderUpdatesBandAction;
  TTBXItem *TBXItem2;
  TAction *UpdatesPreferencesAction;
  TAction *ExplorerUpdatesBandAction;
  TTBXItem *TBXItem3;
  TAction *ExplorerTransferBandAction;
  TAction *CommanderTransferBandAction;
  TTBXItem *TBXItem4;
  TTBXItem *TBXItem5;
  TAction *PresetsPreferencesAction;
  TAction *AutoSizeLocalColumnsAction;
  TAction *CustomCommandsEnterAction;
  TAction *LockToolbarsAction;
  TTBXItem *TBXItem6;
  TTBXItem *TBXItem7;
  TAction *ShowHideRemoteTypeColumnAction2;
  TTBXItem *TBXItem8;
  TAction *RemoteSortByTypeAction2;
  TAction *QueueItemPauseAction;
  TTBXItem *TBXItem9;
  TAction *QueueItemResumeAction;
  TTBXItem *TBXItem10;
  TAction *QueuePauseAllAction;
  TAction *QueueResumeAllAction;
  TAction *QueueDeleteAllDoneAction;
  TAction *QueueEnableAction;
  TTBXSubmenuItem *TBXSubmenuItem1;
  TTBXItem *TBXItem11;
  TTBXItem *TBXItem12;
  TAction *EditorListCustomizeAction;
  TAction *RestoreSelectionAction;
  TAction *CurrentEditFocusedAction;
  TAction *NewLinkAction;
  TAction *NewDirAction;
  TTBXSubmenuItem *TBXSubmenuItem26;
  TTBXItem *TBXItem135;
  TTBXItem *TBXItem136;
  TTBXItem *TBXItem209;
  TAction *QueueDisconnectOnceEmptyAction2;
  TTBXItem *TBXItem13;
  TAction *LocalCopyAction;
  TAction *RemoteCopyAction;
  TAction *ExplorerCustomCommandsBandAction;
  TAction *CommanderCustomCommandsBandAction;
  TTBXItem *TBXItem15;
  TTBXItem *TBXItem16;
  TAction *CustomCommandsBandAction;
  TAction *ColorMenuAction2;
  TAction *AutoReadDirectoryAfterOpAction;
  TTBXPopupMenu *RemoteAddressPopup;
  TTBXSubmenuItem *TBXSubmenuItem2;
  TTBXItem *TBXItem17;
  TTBXSeparatorItem *TBXSeparatorItem1;
  TTBXItem *TBXItem18;
  TTBXItem *TBXItem19;
  TTBXItem *TBXItem20;
  TTBXSeparatorItem *TBXSeparatorItem2;
  TTBXItem *TBXItem21;
  TTBXItem *TBXItem22;
  TTBXItem *TBXItem24;
  TTBXItem *TBXItem25;
  TAction *DuplicateTabAction;
  TAction *CustomCommandsLastAction;
  TAction *CustomCommandsLastFocusedAction;
  TAction *CustomCommandsEnterFocusedAction;
  TAction *CurrentAddEditLinkContextAction;
  TTBXItem *TBXItem23;
  TTBEditAction *QueueItemSpeedAction;
  TTBXComboBoxItem *QueuePopupSpeedComboBoxItem;
  TAction *CurrentDeleteAlternativeAction;
  TAction *CurrentEditWithAction;
  TAction *LocalFilterAction;
  TAction *RemoteFilterAction;
  TTBXItem *TBXItem26;
  TTBXItem *TBXItem27;
  TAction *QueueShutDownOnceEmptyAction2;
  TAction *QueueIdleOnceEmptyAction;
  TTBXSubmenuItem *TBXSubmenuItem3;
  TTBXItem *TBXItem28;
  TTBXItem *TBXItem29;
  TAction *QueueCycleOnceEmptyAction;
  TAction *RemoteFindFilesAction2;
  TTBXItem *TBXItem30;
  TTBXItem *TBXItem31;
  TTBXItem *TBXItem32;
  TTBXItem *TBXItem33;
  TTBXItem *TBXItem34;
  TTBXItem *TBXItem35;
  TTBXItem *TBXItem36;
  TTBXItem *TBXItem37;
  TTBXItem *TBXItem38;
  TTBXColorItem *ColorMenuItem;
  TAction *CurrentEditInternalAction;
  TAction *SaveWorkspaceAction;
  TAction *LocalRenameAction2;
  TAction *LocalEditAction2;
  TAction *LocalMoveAction;
  TAction *LocalCreateDirAction3;
  TAction *LocalDeleteAction2;
  TAction *LocalPropertiesAction2;
  TAction *RemoteRenameAction2;
  TAction *RemoteEditAction2;
  TAction *RemoteMoveAction;
  TAction *RemoteCreateDirAction3;
  TAction *RemoteDeleteAction2;
  TAction *RemotePropertiesAction2;
  TAction *LocalAddEditLinkAction3;
  TAction *RemoteAddEditLinkAction3;
  TAction *CommanderLocalFileBandAction2;
  TAction *CommanderRemoteFileBandAction2;
  TTBXItem *TBXItem14;
  TTBXItem *TBXItem39;
  TTBXItem *TBXItem40;
  TTBXItem *TBXItem41;
  TAction *RemoteCopyFocusedAction;
  TAction *RemoteMoveFocusedAction;
  TAction *LocalSelectAction2;
  TAction *LocalUnselectAction2;
  TAction *LocalSelectAllAction2;
  TAction *RemoteSelectAction2;
  TAction *RemoteUnselectAction2;
  TAction *RemoteSelectAllAction2;
  TAction *CommanderLocalSelectionBandAction2;
  TAction *CommanderRemoteSelectionBandAction2;
  TTBXItem *TBXItem42;
  TTBXItem *TBXItem43;
  TTBXItem *TBXItem44;
  TTBXItem *TBXItem45;
  TAction *SelectiveToolbarTextAction;
  TTBXItem *TBXItem46;
  TTBXItem *TBXItem47;
  TTBXItem *TBXItem48;
  TTBXItem *TBXItem49;
  TTBXPopupMenu *LocalFilePopup;
  TTBXItem *LocalOpenMenuItem;
  TTBXSubmenuItem *LocalEditMenuItem;
  TTBXSubmenuItem *LocalCopyMenuItem;
  TTBXItem *TBXItem54;
  TTBXItem *TBXItem57;
  TTBXItem *TBXItem58;
  TTBXSeparatorItem *TBXSeparatorItem3;
  TTBXSubmenuItem *LocalFilePopupCustomCommandsMenu;
  TTBXSubmenuItem *TBXSubmenuItem5;
  TTBXItem *TBXItem59;
  TTBXItem *TBXItem60;
  TTBXItem *TBXItem61;
  TTBXSeparatorItem *TBXSeparatorItem4;
  TTBXItem *TBXItem63;
  TAction *LocalCopyFocusedAction;
  TAction *LocalMoveFocusedAction;
  TTBXItem *TBXItem50;
  TAction *CurrentSystemMenuFocusedAction;
  TTBXItem *TBXItem51;
  TTBXSeparatorItem *TBXSeparatorItem5;
  TTBXSeparatorItem *TBXSeparatorItem6;
  TTBXItem *TBXItem56;
  TAction *SessionGenerateUrlAction2;
  TTBXItem *TBXItem52;
  TAction *FormatSizeBytesKilobytesAction;
  TAction *FormatSizeBytesShortAction;
  TTBXSubmenuItem *RemoteFormatSizeBytesPopupItem;
  TTBXItem *TBXItem53;
  TTBXItem *TBXItem55;
  TTBXSeparatorItem *TBXSeparatorItem7;
  TTBXSeparatorItem *TBXSeparatorItem8;
  TTBXSubmenuItem *LocalFormatSizeBytesPopupItem;
  TTBXItem *TBXItem64;
  TTBXItem *TBXItem65;
  TTBXItem *TBXItem66;
  TAction *QueueSuspendOnceEmptyAction2;
  TTBXItem *TBXItem68;
  TAction *CurrentEditWithFocusedAction;
  TAction *CurrentEditInternalFocusedAction;
  TTBXSubmenuItem *TBXSubmenuItem7;
  TTBXItem *TBXItem70;
  TTBXItem *TBXItem71;
  TAction *QueueDeleteAllAction;
  TTBXItem *TBXItem142;
  TTBXItem *TBXItem72;
  TTBXItem *TBXItem73;
  TAction *LocalCopyFocusedQueueAction;
  TAction *LocalCopyQueueAction;
  TAction *RemoteCopyFocusedQueueAction;
  TAction *RemoteCopyQueueAction;
  TAction *LocalCopyFocusedNonQueueAction;
  TAction *LocalCopyNonQueueAction;
  TAction *RemoteCopyFocusedNonQueueAction;
  TAction *RemoteCopyNonQueueAction;
  TTBXItem *TBXItem69;
  TTBXSeparatorItem *TBXSeparatorItem9;
  TTBXItem *TBXItem74;
  TTBXSeparatorItem *TBXSeparatorItem10;
  TAction *SelectSameExtAction;
  TAction *UnselectSameExtAction;
  TAction *GoToAddressAction;
  TAction *LockAction;
  TAction *UnlockAction;
  TAction *TipsAction;
  TAction *CustomCommandsNonFileAction;
  TTBXSubmenuItem *RemoteDirViewPopupCustomCommandsMenu;
  TTBXItem *TBXItem75;
  TTBXItem *TBXItem76;
  TAction *ChangePasswordAction;
  TAction *RemoteNewFileAction;
  TAction *LocalNewFileAction;
  TTBXSubmenuItem *TBXItem77;
  TAction *CustomizeToolbarAction;
  TTBXSubmenuItem *TBXSubmenuItem4;
  TTBXSubmenuItem *TBXSubmenuItem6;
  TTBXSubmenuItem *TBXSubmenuItem9;
  TAction *PrivateKeyUploadAction;
  TAction *RenameTabAction;
  TTBXItem *TBXItem78;
  TAction *CurrentCopyToClipboardAction2;
  TAction *FileColorsPreferencesAction;
  TAction *DisconnectSessionAction;
  TTBXItem *TBXItem79;
  TAction *ReconnectSessionAction;
  TTBXItem *TBXItem80;
  TAction *CurrentCopyToClipboardFocusedAction2;
  TTBXSeparatorItem *TBXSeparatorItem11;
  TTBXItem *TBXItem81;
  TTBXSeparatorItem *TBXSeparatorItem12;
  TTBXItem *TBXItem82;
  TAction *QueueFileListAction;
  TTBXItem *TBXItem83;
  TTBXItem *TBXItem84;
  TTBXItem *TBXItem85;
  TAction *CommanderLocalPanelAction;
  TAction *CommanderRemotePanelAction;
  TTBXSubmenuItem *TBXSubmenuItem8;
  TTBXItem *TBXItem86;
  TTBXSeparatorItem *TBXSeparatorItem13;
  TTBXItem *TBXItem87;
  TTBXItem *TBXItem88;
  TTBXItem *TBXItem89;
  TTBXSeparatorItem *TBXSeparatorItem14;
  TTBXItem *TBXItem90;
  TTBXItem *TBXItem91;
  TTBXSubmenuItem *TBXSubmenuItem10;
  TTBXItem *TBXItem92;
  TTBXItem *TBXItem93;
  TTBXSeparatorItem *TBXSeparatorItem15;
  TTBXItem *TBXItem94;
  TTBXItem *TBXItem95;
  TTBXItem *TBXItem96;
  TTBXSeparatorItem *TBXSeparatorItem16;
  TTBXItem *TBXItem97;
  TTBXItem *TBXItem98;
  TAction *RemoteExploreDirectoryAction;
  TTBXItem *TBXItem99;
  TTBXItem *TBXItem100;
  TAction *LocalLocalCopyAction;
  TAction *LocalLocalMoveAction;
  TAction *LocalOtherCopyAction;
  TAction *LocalOtherMoveAction;
  TAction *LocalLocalCopyFocusedAction;
  TAction *LocalLocalMoveFocusedAction;
  TTBXItem *TBXItem101;
  TTBXItem *LocalLocalCopyMenuItem;
  TTBXSeparatorItem *TBXSeparatorItem17;
  TTBXPopupMenu *LocalBrowserPopup;
  TTBXItem *TBXItem62;
  TTBXSeparatorItem *TBXSeparatorItem21;
  TTBXSubmenuItem *TBXSubmenuItem11;
  TTBXSeparatorItem *TBXSeparatorItem22;
  TTBXItem *TBXItem110;
  TTBXItem *TBXItem102;
  TTBXItem *TBXItem103;
  TAction *NewTabAction;
  TAction *NewLocalTabAction;
  TAction *NewRemoteTabAction;
  TAction *DefaultToNewRemoteTabAction;
  TTBXPopupMenu *NewTabPopup;
  TTBXItem *NewRemoteTabItem;
  TTBXItem *NewLocalTabItem;
  TTBXSeparatorItem *TBXSeparatorItem67;
  TTBXItem *TBXItem232;
  TTBXSubmenuItem *SessionsNewTabItem;
  TTBXItem *TBXItem104;
  TTBXItem *TBXItem105;
  TTBXSeparatorItem *TBXSeparatorItem18;
  TTBXItem *TBXItem106;
  TTBXSubmenuItem *TBXSubmenuItem12;
  TTBXItem *TBXItem107;
  TTBXItem *TBXItem108;
  TTBXSeparatorItem *TBXSeparatorItem19;
  TTBXItem *TBXItem111;
  TAction *CalculateDirectorySizesAction;
  TAction *LocalCalculateDirectorySizesAction;
  TAction *RemoteCalculateDirectorySizesAction;
  TTBXItem *RemoteCalculateDirectorySizesPopupItem;
  TTBXItem *LocalCalculateDirectorySizesPopupItem;
  TAction *LocalOtherDirAction;
  TAction *RemoteOtherDirAction;
  TTBXItem *TBXItem109;
  TTBXItem *TBXItem113;
  TAction *AutoSizeRemoteColumnsAction;
  TTBXSeparatorItem *TBXSeparatorItem73;
  TTBXItem *TBXItem264;
  TTBXSeparatorItem *TBXSeparatorItem20;
  TTBXItem *TBXItem114;
  TAction *ResetLayoutRemoteColumnsAction;
  TAction *ResetLayoutLocalColumnsAction;
  TTBXItem *TBXItem112;
  TTBXItem *TBXItem115;
  TAction *QueueResetLayoutColumnsAction;
  TTBXSeparatorItem *TBXSeparatorItem23;
  TTBXItem *TBXItem116;
  TAction *IncrementalSearchStartAction;
  TAction *RemoteThumbnailAction;
  TAction *LocalReportAction;
  TAction *LocalThumbnailAction;
  TTBXSeparatorItem *TBXSeparatorItem24;
  TAction *ToolbarIconSizeLargeAction;
  TTBXItem *TBXItem117;
  TTBXSeparatorItem *TBXSeparatorItem25;
  TTBXSeparatorItem *TBXSeparatorItem26;
  TTBXItem *TBXItem118;
  TTBXItem *TBXItem119;
  TTBXItem *TBXItem120;
  TTBXSeparatorItem *TBXSeparatorItem27;
  TAction *ToolbarIconSizeAction;
  TTBXSubmenuItem *TBXSubmenuItem13;
  TTBXSubmenuItem *TBXSubmenuItem14;
  TTBXSubmenuItem *TBXSubmenuItem15;
  TTBXSubmenuItem *TBXSubmenuItem16;
  TAction *ToolbarIconSizeNormalAction;
  TAction *ToolbarIconSizeVeryLargeAction;
  TTBXItem *TBXItem121;
  TTBXItem *TBXItem122;
  TTBXItem *TBXItem123;
  TTBXItem *TBXItem126;
  TTBXItem *TBXItem127;
  TTBXItem *TBXItem128;
  TTBXItem *TBXItem129;
  TTBXItem *TBXItem130;
  void __fastcall ExplorerActionsUpdate(TBasicAction *Action, bool &Handled);
  void __fastcall ExplorerActionsExecute(TBasicAction *Action, bool &Handled);
  void __fastcall SessionIdleTimerTimer(TObject *Sender);
  void __fastcall QueuePopupPopup(TObject *Sender);
  void __fastcall QueuePopupSpeedComboBoxItemItemClick(TObject *Sender);
  void __fastcall QueueSpeedComboBoxItemAcceptText(TObject *Sender,
          UnicodeString &NewText, bool &Accept);
  void __fastcall FocusedEditMenuItemPopup(TTBCustomItem *Sender, bool FromLink);
  void __fastcall EditMenuItemPopup(TTBCustomItem *Sender, bool FromLink);
  void __fastcall QueuePopupSpeedComboBoxItemAdjustImageIndex(TTBXComboBoxItem *Sender,
          const UnicodeString AText, int AIndex, int &ImageIndex);
  UnicodeString __fastcall CustomCommandCaption(const TCustomCommandType * Command, bool Toolbar);
  UnicodeString __fastcall CustomCommandHint(const TCustomCommandType * Command);

private:
  TListColumn * FListColumn;
  TCustomScpExplorerForm * FScpExplorer;
  bool FSessionIdleTimerExecuting;
  int FBusy;
  TTBCustomToolbar * FCustomizedToolbar;
  int FRemoteRootDirImageIndex;

  void __fastcall SetScpExplorer(TCustomScpExplorerForm * value);
  bool __fastcall GetBusy();

protected:
  void __fastcall CreateSessionListMenu(TAction * Action);
  void __fastcall CreateSessionListMenuLevel(TTBCustomItem * Menu, int Index, int Level);
  void __fastcall CreateToolbarButtonsList();
  bool __fastcall IsToolbarCustomizable();
  UnicodeString __fastcall GetSessionFolderRoot(TSessionData * Data, int Level);
  void __fastcall CreateWorkspacesMenu(TAction * Action);
  void __fastcall WorkspaceItemClick(TObject * Sender);
  int __fastcall CreateCustomCommandsListMenu(TCustomCommandList * List, TTBCustomItem * Menu, bool OnFocused,
    bool Toolbar, TCustomCommandListType ListType, int Tag, TStrings * HiddenCommands);
  void __fastcall CreateCustomCommandsMenu(TAction * Action, TCustomCommandListType ListType);
  bool __fastcall CheckCustomCommandsToolbarList(TTBXToolbar * Toolbar, TCustomCommandList * List, int & Index);
  void __fastcall UpdateCustomCommandsToolbarList(TTBXToolbar * Toolbar, TCustomCommandList * List, int & Index);
  void __fastcall CreateSessionColorMenu(TAction * Action);
  void __fastcall SessionColorChange(TColor Color);
  void __fastcall CreateOpenedSessionListMenu(TAction * Action);
  TCustomDirView * __fastcall DirView(TOperationSide Side) { return ScpExplorer->DirView(Side); }
  bool __fastcall DirViewEnabled(TOperationSide Side) { return ScpExplorer->DirViewEnabled(Side); }
  void __fastcall SessionItemClick(TObject * Sender);
  void __fastcall SessionFolderItemClick(TObject * Sender);
  void __fastcall SessionFolderThisItemClick(TObject * Sender);
  void __fastcall OpenedSessionItemClick(TObject * Sender);
  void __fastcall CustomCommandClick(TObject * Sender);
  void __fastcall CreateEditorListMenu(TTBCustomItem * Menu, bool OnFocused);
  void __fastcall EditorItemClick(TObject * Sender);
  void __fastcall EditorItemClickFocused(TObject * Sender);
  void __fastcall DoEditorItemClick(TObject * Sender, bool OnFocused);
  void __fastcall DoIdle();
  inline void __fastcall ShowUpdatesUpdate();
  void __fastcall PreferencesDialog(TPreferencesMode APreferencesMode);
  void __fastcall CustomCommandsLastUpdate(TAction * Action);
  UnicodeString __fastcall QueueItemSpeed(const UnicodeString & Text,
    TTBXComboBoxItem * Item);
  void __fastcall CycleQueueOnceEmptyAction();
  void __fastcall SetQueueOnceEmptyAction(TAction * Action);
  TAction * __fastcall CurrentQueueOnceEmptyAction();
  void __fastcall CloneShortcuts();
  void __fastcall ToolbarButtonItemClick(TObject * Sender);
  void __fastcall CustomCommandsCustomize(TObject * Sender);

public:
  __fastcall TNonVisualDataModule(TComponent * Owner);
  virtual __fastcall ~TNonVisualDataModule();

  void __fastcall CommanderShortcuts();
  void __fastcall ExplorerShortcuts();
  TShortCut __fastcall OpenSessionShortCut(int Index);
  void __fastcall UpdateNonVisibleActions();
  void __fastcall UpdateCustomCommandsToolbar(TTBXToolbar * Toolbar);
  void __fastcall QueueSpeedComboBoxItem(TTBXComboBoxItem * Item);
  void __fastcall QueueSpeedComboBoxItemUpdate(TTBXComboBoxItem * Item);
  void __fastcall CreateCustomCommandsMenu(TTBCustomItem * Menu, bool OnFocused,
    bool Toolbar, TCustomCommandListType ListType, TStrings * HiddenCommands);
  void __fastcall CreateCustomCommandsMenu(TAction * Action, bool OnFocused, TCustomCommandListType ListType);
  TOnceDoneOperation __fastcall CurrentQueueOnceEmptyOperation();
  void __fastcall ResetQueueOnceEmptyOperation();
  void __fastcall StartBusy();
  void __fastcall EndBusy();
  void __fastcall ControlContextPopup(TObject * Sender, const TPoint & MousePos);
  bool __fastcall IsCustomizableToolbarItem(TTBCustomItem * Item);

  __property TListColumn * ListColumn = { read = FListColumn, write = FListColumn };
  __property TCustomScpExplorerForm * ScpExplorer = { read = FScpExplorer, write = SetScpExplorer };
  __property bool Busy = { read = GetBusy };
};
//---------------------------------------------------------------------------
extern PACKAGE TNonVisualDataModule *NonVisualDataModule;
//---------------------------------------------------------------------------
#endif
