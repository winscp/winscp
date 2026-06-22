//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "NonVisual.h"

#include <CoreMain.h>
#include <TextsWin.h>
#include <Tools.h>
#include <Setup.h>

#include <Interface.h>
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "TBX.hpp"
#include "VCLCommon.h"
#include <HistoryComboBox.hpp>
#include "Glyphs.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "TB2Item"
#pragma link "TBX"
#pragma link "TB2ExtItems"
#pragma link "TBXExtItems"
#pragma link "TBXToolPals"
#pragma resource "*.dfm"
TNonVisualDataModule *NonVisualDataModule;
//---------------------------------------------------------------------------
#define SCPCOMMANDER ((TScpCommanderForm *)ScpExplorer)
#define UPDEX(HandleAction, Condition, OtherEnabled, OtherDisabled) if (Action == HandleAction) { \
  ((TCustomAction *)Action)->Enabled = (Condition); \
  if (((TCustomAction *)Action)->Enabled) { OtherEnabled; } else { OtherDisabled; }; \
  Handled = true; } else
#define UPDEX1(HandleAction, Condition, Other) UPDEX(HandleAction, Condition, Other, Other)
#define UPD(HandleAction, Condition) if (Action == HandleAction) { \
  ((TCustomAction *)Action)->Enabled = (Condition); Handled = true; } else
#define UPDFUNC(HandleAction, Function) if (Action == HandleAction) { Function; Handled = true; } else
#define EXE(HandleAction, Command) if (Action == HandleAction) { \
  Command; Handled = true; } else
#define UPDACT(HandleAction, Command) \
  EXE(HandleAction, ((TCustomAction *)Action)->Enabled = true; Command)
#define UPDCOMP2(COMP, NUM) if (Action == COMP ## Action ## NUM) { COMP ## Action ## NUM->Enabled = true; \
  COMP ## Action ## NUM->Visible = ScpExplorer->IsComponentPossible(fc ## COMP); \
  COMP ## Action ## NUM->Checked = ScpExplorer->ComponentVisible[fc ## COMP]; Handled = true; } else
#define UPDCOMP(COMP) UPDCOMP2(COMP, )
#define EXECOMP2(COMP, NUM) EXE(COMP ## Action ## NUM, \
  ScpExplorer->ComponentVisible[fc ## COMP] = !ScpExplorer->ComponentVisible[fc ## COMP] )
#define EXECOMP(COMP) EXECOMP2(COMP, )
#define COLPROPS(SIDE) \
  ((TCustomDirViewColProperties*)ScpExplorer->DirView(os ## SIDE)->ColProperties)
#define UPDSORT(SIDE, NAME, LCOL, RCOL, NUM) \
  UPDEX(SIDE ## SortBy ## NAME ## Action ## NUM, (AuxInt = (ScpExplorer->IsSideLocalBrowser(os ## SIDE) ? LCOL : RCOL)) >= 0, \
    Action->Checked = (COLPROPS(SIDE)->SortColumn == AuxInt), \
    Action->Checked = false \
  )
#define EXESORT(SIDE, NAME, LCOL, RCOL, NUM) \
  EXE(SIDE ## SortBy ## NAME ## Action ## NUM, ScpExplorer->DirView(os ## SIDE)->SortBy(ScpExplorer->IsSideLocalBrowser(os ## SIDE) ? LCOL : RCOL))
#define UPDSORTA(SIDE, NUM) if (Action == SIDE ## SortAscendingAction ## NUM) { \
  SIDE ## SortAscendingAction ## NUM->Enabled = true; Handled = true; \
  SIDE ## SortAscendingAction ## NUM->Checked = COLPROPS(SIDE)->SortAscending; } else
#define EXESORTA(SIDE, NUM) EXE(SIDE ## SortAscendingAction ## NUM, \
  COLPROPS(SIDE)->SortAscending = !COLPROPS(SIDE)->SortAscending; )
#define UPDSHCOL(SIDE, NAME, LCOL, RCOL) \
  UPDFUNC(ShowHide ## SIDE ## NAME ## ColumnAction2, \
    int Col = (ScpExplorer->IsSideLocalBrowser(os ## SIDE) ? LCOL : RCOL); \
    Action->Enabled = (Col >= 0); \
    Action->Checked = Action->Enabled && COLPROPS(SIDE)->Visible[Col]; \
  )
#define EXESHCOL(SIDE, NAME, LCOL, RCOL) \
  EXE(ShowHide ## SIDE ## NAME ## ColumnAction2, \
    int Col = (ScpExplorer->IsSideLocalBrowser(os ## SIDE) ? LCOL : RCOL); \
    COLPROPS(SIDE)->Visible[Col] = !COLPROPS(SIDE)->Visible[Col])

#define BAND_COMPONENTS \
  EMIT_BAND_COMPONENT(ExplorerMenuBand) \
  EMIT_BAND_COMPONENT(ExplorerAddressBand) \
  EMIT_BAND_COMPONENT(ExplorerToolbarBand) \
  EMIT_BAND_COMPONENT(ExplorerSelectionBand) \
  EMIT_BAND_COMPONENT2(ExplorerSessionBand, 2) \
  EMIT_BAND_COMPONENT(ExplorerPreferencesBand) \
  EMIT_BAND_COMPONENT(ExplorerSortBand) \
  EMIT_BAND_COMPONENT(ExplorerUpdatesBand) \
  EMIT_BAND_COMPONENT(ExplorerTransferBand) \
  EMIT_BAND_COMPONENT(ExplorerCustomCommandsBand) \
  EMIT_BAND_COMPONENT(CommanderMenuBand) \
  EMIT_BAND_COMPONENT2(CommanderSessionBand, 2) \
  EMIT_BAND_COMPONENT(CommanderPreferencesBand) \
  EMIT_BAND_COMPONENT(CommanderSortBand) \
  EMIT_BAND_COMPONENT(CommanderCommandsBand) \
  EMIT_BAND_COMPONENT(CommanderUpdatesBand) \
  EMIT_BAND_COMPONENT(CommanderTransferBand) \
  EMIT_BAND_COMPONENT(CommanderCustomCommandsBand) \
  EMIT_BAND_COMPONENT2(CommanderLocalHistoryBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderLocalNavigationBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderLocalFileBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderLocalSelectionBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderRemoteHistoryBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderRemoteNavigationBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderRemoteFileBand, 2) \
  EMIT_BAND_COMPONENT2(CommanderRemoteSelectionBand, 2)
#define EMIT_BAND_COMPONENT(COMP) EMIT_BAND_COMPONENT2(COMP, )
//---------------------------------------------------------------------------
__fastcall TNonVisualDataModule::TNonVisualDataModule(TComponent* Owner)
        : TDataModule(Owner)
{
  FListColumn = NULL;
  FSessionIdleTimerExecuting = false;
  FCustomizedToolbar = NULL;
  FBusy = 0;

  QueueSpeedComboBoxItem(QueuePopupSpeedComboBoxItem);
  FRemoteRootDirImageIndex = RemoteRootDirAction->ImageIndex;
}
//---------------------------------------------------------------------------
__fastcall TNonVisualDataModule::~TNonVisualDataModule()
{
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ExplorerActionsUpdate(
  TBasicAction * BasicAction, bool & Handled)
{
  TAction * Action = DebugNotNull(dynamic_cast<TAction *>(BasicAction));
  if (!ScpExplorer || !ScpExplorer->AllowedAction(Action, aaUpdate))
  {
    Action->Enabled = false;
    Handled = true;
    return;
  }
  void * AuxVoidPtr;
  int AuxInt;
  #define HasManagedSession ScpExplorer->HasManagedSession()
  #define HasTerminal ScpExplorer->HasActiveTerminal()
  // CURRENT DIRVIEW
  #define EnabledSelectedOperation (ScpExplorer->EnableSelectedOperation[osCurrent])
  #define EnabledFocusedOperation (ScpExplorer->EnableFocusedOperation[osCurrent])
  #define EnabledSelectedFileOperation (ScpExplorer->EnableSelectedFileOperation[osCurrent])
  #define EnabledFocusedFileOperation (ScpExplorer->EnableFocusedFileOperation[osCurrent])
  #define EnabledLocalSelectedOperation (ScpExplorer->HasDirView[osLocal] && ScpExplorer->EnableSelectedOperation[osLocal])
  #define EnabledLocalFocusedOperation (ScpExplorer->HasDirView[osLocal] && ScpExplorer->EnableFocusedOperation[osLocal])
  #define EnabledLocalSelectedFileOperation (ScpExplorer->HasDirView[osLocal] && ScpExplorer->EnableSelectedFileOperation[osLocal])
  #define EnabledRemoteSelectedOperation (ScpExplorer->EnableSelectedOperation[osRemote] && HasTerminal)
  #define EnabledRemoteFocusedOperation (ScpExplorer->EnableFocusedOperation[osRemote] && HasTerminal)
  #define EnabledRemoteSelectedFileOperation (ScpExplorer->EnableSelectedFileOperation[osRemote] && HasTerminal)
  #define EnabledOtherSelectedOperation ScpExplorer->IsLocalBrowserMode() && ScpExplorer->EnableSelectedOperation[osOther]
  #define EnabledOtherSelectedFileOperation ScpExplorer->IsLocalBrowserMode() && ScpExplorer->EnableSelectedFileOperation[osOther]
  // focused operation
  UPD(CurrentDeleteFocusedAction, EnabledFocusedOperation)
  UPD(CurrentPropertiesFocusedAction, EnabledFocusedOperation)
  UPD(CurrentEditFocusedAction, EnabledFocusedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentSystemMenuFocusedAction, EnabledFocusedOperation)
  UPD(CurrentEditWithFocusedAction, EnabledFocusedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentEditInternalFocusedAction, EnabledFocusedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentCopyToClipboardFocusedAction2, EnabledFocusedOperation)
  // file operation
  UPD(CurrentRenameAction, EnabledFocusedOperation &&
    (ScpExplorer->IsSideLocalBrowser(osCurrent) || ScpExplorer->Terminal->IsCapable[fcRename]))
  UPD(CurrentEditAction, EnabledSelectedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentEditInternalAction, EnabledSelectedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentEditWithAction, EnabledSelectedFileOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPD(CurrentOpenAction, EnabledFocusedOperation &&
    !WinConfiguration->DisableOpenEdit)
  UPDEX1(CurrentAddEditLinkContextAction, ScpExplorer->CanAddEditLink(osCurrent),
    Action->Visible = ScpExplorer->LinkFocused())
  UPD(NewLinkAction, ScpExplorer->CanAddEditLink(osCurrent))
  // selected operation
  UPD(CurrentDeleteAction, EnabledSelectedOperation)
  UPD(CurrentDeleteAlternativeAction, EnabledSelectedOperation)
  UPD(CurrentPropertiesAction, EnabledSelectedOperation)
  UPD(CurrentCopyToClipboardAction2, EnabledSelectedOperation)
  UPD(RemoteMoveToAction, EnabledSelectedOperation &&
    !ScpExplorer->IsSideLocalBrowser(osCurrent) &&
    ScpExplorer->Terminal->IsCapable[fcRemoteMove])
  UPD(RemoteCopyToAction, EnabledSelectedOperation &&
    !ScpExplorer->IsSideLocalBrowser(osCurrent))
  UPD(FileListToCommandLineAction, EnabledSelectedOperation)
  UPD(FileListToClipboardAction, EnabledSelectedOperation)
  UPD(FullFileListToClipboardAction, EnabledSelectedOperation)
  UPD(FileGenerateUrlAction2, EnabledSelectedOperation && !ScpExplorer->IsSideLocalBrowser(osCurrent))
  UPD(FileListFromClipboardAction, IsFormatInClipboard(CF_TEXT))
  UPD(CurrentAddEditLinkAction, ScpExplorer->CanAddEditLink(osCurrent))
  UPD(LockAction,
    EnabledSelectedOperation && !ScpExplorer->IsSideLocalBrowser(osCurrent) &&
    ScpExplorer->Terminal->IsCapable[fcLocking])
  UPD(UnlockAction,
    EnabledSelectedOperation && !ScpExplorer->IsSideLocalBrowser(osCurrent) &&
    ScpExplorer->Terminal->IsCapable[fcLocking])
  UPD(CalculateDirectorySizesAction, EnabledSelectedOperation)
  // local selected operation
  UPD(LocalCopyAction, HasTerminal && EnabledLocalSelectedOperation)
  UPDEX1(LocalCopyQueueAction,
    HasTerminal && EnabledLocalSelectedOperation && ScpExplorer->Terminal->IsCapable[fcBackgroundTransfers],
    Action->Visible = !ScpExplorer->IsLocalBrowserMode())
  UPDEX1(LocalCopyNonQueueAction, HasTerminal && EnabledLocalSelectedOperation, Action->Visible = !ScpExplorer->IsLocalBrowserMode())
  UPD(LocalRenameAction2, EnabledLocalSelectedOperation)
  UPD(LocalEditAction2, EnabledLocalSelectedFileOperation && !WinConfiguration->DisableOpenEdit)
  UPD(LocalMoveAction, HasTerminal && EnabledLocalSelectedOperation)
  UPD(LocalCreateDirAction3, true)
  UPD(LocalDeleteAction2, EnabledLocalSelectedOperation)
  UPD(LocalPropertiesAction2, EnabledLocalSelectedOperation)
  UPD(LocalAddEditLinkAction3, ScpExplorer->CanAddEditLink(osLocal))
  UPD(LocalNewFileAction, !WinConfiguration->DisableOpenEdit)
  UPD(LocalLocalCopyAction, ScpExplorer->IsLocalBrowserMode() && EnabledLocalSelectedOperation)
  UPD(LocalLocalMoveAction, ScpExplorer->IsLocalBrowserMode() && EnabledLocalSelectedOperation)
  UPD(LocalOtherCopyAction, EnabledOtherSelectedOperation)
  UPD(LocalOtherMoveAction, EnabledOtherSelectedOperation)
  UPD(LocalCalculateDirectorySizesAction, EnabledLocalSelectedOperation)
  // local focused operation
  UPDEX1(LocalCopyFocusedAction, HasTerminal && EnabledLocalFocusedOperation, Action->Visible = !ScpExplorer->IsLocalBrowserMode())
  UPD(LocalCopyFocusedQueueAction, HasTerminal && EnabledLocalFocusedOperation && ScpExplorer->Terminal->IsCapable[fcBackgroundTransfers])
  UPD(LocalCopyFocusedNonQueueAction, HasTerminal && EnabledLocalFocusedOperation)
  UPD(LocalMoveFocusedAction, HasTerminal && EnabledLocalFocusedOperation)
  UPDEX1(LocalLocalCopyFocusedAction, ScpExplorer->IsLocalBrowserMode() && EnabledFocusedOperation, Action->Visible = ScpExplorer->IsLocalBrowserMode())
  UPDEX1(LocalLocalMoveFocusedAction, ScpExplorer->IsLocalBrowserMode() && EnabledFocusedOperation, Action->Visible = ScpExplorer->IsLocalBrowserMode())
  // remote selected operation
  UPD(RemoteCopyAction, EnabledRemoteSelectedOperation)
  UPDEX1(RemoteCopyQueueAction,
    EnabledRemoteSelectedOperation && ScpExplorer->Terminal->IsCapable[fcBackgroundTransfers],
    Action->Visible = !ScpExplorer->IsLocalBrowserMode())
  UPDEX1(RemoteCopyNonQueueAction, EnabledRemoteSelectedOperation, Action->Visible = !ScpExplorer->IsLocalBrowserMode())
  UPD(RemoteRenameAction2, (EnabledRemoteSelectedOperation && ScpExplorer->Terminal->IsCapable[fcRename]) || EnabledOtherSelectedOperation)
  UPD(RemoteEditAction2, (EnabledRemoteSelectedFileOperation || EnabledOtherSelectedFileOperation) && !WinConfiguration->DisableOpenEdit)
  UPD(RemoteMoveAction, EnabledRemoteSelectedOperation)
  UPD(RemoteCreateDirAction3, DirViewEnabled(osRemote))
  UPD(RemoteNewFileAction, DirViewEnabled(osRemote) && !WinConfiguration->DisableOpenEdit)
  UPD(RemoteDeleteAction2, EnabledRemoteSelectedOperation || EnabledOtherSelectedOperation)
  UPD(RemotePropertiesAction2, EnabledRemoteSelectedOperation || EnabledOtherSelectedOperation)
  UPD(RemoteAddEditLinkAction3, ScpExplorer->CanAddEditLink(osRemote))
  UPD(RemoteCalculateDirectorySizesAction, EnabledRemoteSelectedOperation || EnabledOtherSelectedOperation)
  // remote focused operation
  UPD(RemoteCopyFocusedAction, EnabledRemoteFocusedOperation)
  UPD(RemoteCopyFocusedQueueAction, EnabledRemoteFocusedOperation && ScpExplorer->Terminal->IsCapable[fcBackgroundTransfers])
  UPD(RemoteCopyFocusedNonQueueAction, EnabledRemoteFocusedOperation)
  UPD(RemoteMoveFocusedAction, EnabledRemoteFocusedOperation)
  UPD(RemoteMoveToFocusedAction, EnabledFocusedOperation &&
    !ScpExplorer->IsSideLocalBrowser(osCurrent) &&
    ScpExplorer->Terminal->IsCapable[fcRemoteMove])
  UPD(RemoteCopyToFocusedAction, EnabledFocusedOperation &&
    !ScpExplorer->IsSideLocalBrowser(osCurrent))
  // directory
  UPD(CurrentCreateDirAction, DirViewEnabled(osCurrent))
  UPD(NewDirAction, DirViewEnabled(osCurrent))
  UPD(RemoteFindFilesAction2, HasTerminal)
  // selection
  UPD(SelectOneAction, DirView(osCurrent)->FilesCount)
  UPD(SelectAction, DirView(osCurrent)->FilesCount)
  UPD(UnselectAction, DirView(osCurrent)->SelCount)
  UPD(SelectAllAction, DirView(osCurrent)->FilesCount)
  UPD(InvertSelectionAction, DirView(osCurrent)->FilesCount)
  UPD(ClearSelectionAction, DirView(osCurrent)->SelCount)
  UPD(RestoreSelectionAction, DirView(osCurrent)->SelectedNamesSaved)
  UPD(SelectSameExtAction, EnabledFocusedFileOperation)
  UPD(UnselectSameExtAction, EnabledFocusedFileOperation)
  UPD(PasteAction3, ScpExplorer->CanPasteFromClipBoard())
  UPD(LocalSelectAction2, ScpExplorer->HasDirView[osLocal] && DirView(osLocal)->FilesCount)
  UPD(LocalUnselectAction2, ScpExplorer->HasDirView[osLocal] && DirView(osLocal)->SelCount)
  UPD(LocalSelectAllAction2, ScpExplorer->HasDirView[osLocal] && DirView(osLocal)->FilesCount)
  UPD(RemoteSelectAction2, DirView(osRemote)->FilesCount)
  UPD(RemoteUnselectAction2, DirView(osRemote)->SelCount)
  UPD(RemoteSelectAllAction2, DirView(osRemote)->FilesCount)

  //style
  #define STYLEIMAGE(STYLE) Style == dvs ## STYLE ? Remote ## STYLE ## Action->ImageIndex :
  UPDACT(RemoteCycleStyleAction, int Style = DirView(osRemote)->DirViewStyle;
    RemoteCycleStyleAction->ImageIndex = (STYLEIMAGE(Icon) STYLEIMAGE(SmallIcon) STYLEIMAGE(List) STYLEIMAGE(Report) RemoteIconAction->ImageIndex))
  #undef STYLEIMAGE
  #define STYLEACTION(SIDE, STYLE) UPDACT(SIDE ## STYLE ## Action, \
    SIDE ## STYLE ## Action->Checked = (DirView(os ## SIDE)->DirViewStyle == dvs ## STYLE))
  STYLEACTION(Remote, Icon)
  STYLEACTION(Remote, SmallIcon)
  STYLEACTION(Remote, List)
  STYLEACTION(Remote, Report)
  STYLEACTION(Remote, Thumbnail)
  STYLEACTION(Local, Report)
  STYLEACTION(Local, Thumbnail)
  #undef STYLEACTION

  // REMOTE+LOCAL
  // back/forward
  #define HISTORYACTION(SIDE, DIRECTION, HINTFMT, DELTA) \
    UPDEX(SIDE ## DIRECTION ## Action, DirViewEnabled(os ## SIDE) && (DirView(os ## SIDE)->DIRECTION ## Count > 0), \
    SIDE ## DIRECTION ## Action->Hint = FMTLOAD(HINTFMT, (DirView(os ## SIDE)->HistoryPath[DELTA])), \
    SIDE ## DIRECTION ## Action->Hint = L"")
  HISTORYACTION(Local, Back, EXPLORER_BACK_HINT, -1)
  HISTORYACTION(Local, Forward, EXPLORER_FORWARD_HINT, 1)
  HISTORYACTION(Remote, Back, EXPLORER_BACK_HINT, -1)
  HISTORYACTION(Remote, Forward, EXPLORER_FORWARD_HINT, 1)
  #undef HISTORYACTION
  #define PANEL_ACTIONS(SIDE) \
    UPD(SIDE ## ParentDirAction, DirViewEnabled(os ## SIDE) && !DirView(os ## SIDE)->IsRoot) \
    UPDEX1(SIDE ## RootDirAction, DirViewEnabled(os ## SIDE) && !DirView(os ## SIDE)->IsRoot, Action->ImageIndex = (ScpExplorer->IsSideLocalBrowser(os ## SIDE) ? LocalRootDirAction->ImageIndex : FRemoteRootDirImageIndex)) \
    UPD(SIDE ## HomeDirAction, DirViewEnabled(os ## SIDE)) \
    UPD(SIDE ## RefreshAction, DirViewEnabled(os ## SIDE) && DirView(os ## SIDE)->DirOK) \
    UPD(SIDE ## OpenDirAction, DirViewEnabled(os ## SIDE)) \
    UPD(SIDE ## OtherDirAction, ScpExplorer->IsLocalBrowserMode() && !SamePaths(DirView(osLocal)->Path, DirView(osOther)->Path)) \
    UPD(SIDE ## ChangePathAction2, DirViewEnabled(os ## SIDE)) \
    UPD(SIDE ## AddBookmarkAction2, DirViewEnabled(os ## SIDE)) \
    UPD(SIDE ## PathToClipboardAction2, DirViewEnabled(os ## SIDE)) \
    UPDEX1(SIDE ## FilterAction, DirViewEnabled(os ## SIDE), Action->Checked = !DirView(os ## SIDE)->Mask.IsEmpty()) \
    UPD(SIDE ## ExploreDirectoryAction, ScpExplorer->IsSideLocalBrowser(os ## SIDE))
  PANEL_ACTIONS(Local)
  PANEL_ACTIONS(Remote)
  #undef PANEL_ACTIONS

  // HELP
  UPD(AboutAction, true)
  UPD(HomepageAction, true)
  UPD(HistoryPageAction, true)
  UPD(TableOfContentsAction, true)
  UPD(ForumPageAction, true)
  UPDACT(CheckForUpdatesAction, ShowUpdatesUpdate())
  UPD(UpdatesPreferencesAction, true)
  UPDEX1(DonatePageAction, true, DonatePageAction->Visible = !IsUWP())
  UPD(DownloadPageAction, true)
  UPDEX1(TipsAction, true, TipsAction->Visible = AnyTips())

  // VIEW
  UPDCOMP2(SessionsTabs, 2)
  UPDCOMP(StatusBar)
  UPDCOMP(ToolBar2)
  UPDCOMP2(LocalStatusBar, 2)
  UPDCOMP2(RemoteStatusBar, 2)
  UPDCOMP(CommandLinePanel)
  UPDCOMP(RemoteTree)
  UPDCOMP(LocalTree)
  #define EMIT_BAND_COMPONENT2(COMP, NUM) UPDCOMP2(COMP, NUM)
  BAND_COMPONENTS
  #undef EMIT_BAND_COMPONENT2
  UPD(CommanderLocalPanelAction, true)
  UPD(CommanderRemotePanelAction, true)

  UPD(GoToCommandLineAction, true)
  UPD(GoToTreeAction, true)
  UPDEX(ShowHiddenFilesAction, true,
    ShowHiddenFilesAction->Checked = WinConfiguration->ShowHiddenFiles, )
  UPDEX(FormatSizeBytesNoneAction, true,
    FormatSizeBytesNoneAction->Checked = (WinConfiguration->FormatSizeBytes == fbNone), )
  UPDEX(FormatSizeBytesKilobytesAction, true,
    FormatSizeBytesKilobytesAction->Checked = (WinConfiguration->FormatSizeBytes == fbKilobytes), )
  UPDEX(FormatSizeBytesShortAction, true,
    FormatSizeBytesShortAction->Checked = (WinConfiguration->FormatSizeBytes == fbShort), )
  UPDEX(AutoReadDirectoryAfterOpAction, true,
    AutoReadDirectoryAfterOpAction->Checked = Configuration->AutoReadDirectoryAfterOp, )
  UPD(PreferencesAction, true)
  UPD(PresetsPreferencesAction, true)
  UPD(FileColorsPreferencesAction, true)
  UPDEX(LockToolbarsAction, true,
    LockToolbarsAction->Checked = WinConfiguration->LockToolbars, )
  UPDEX(SelectiveToolbarTextAction, true,
    SelectiveToolbarTextAction->Checked = WinConfiguration->SelectiveToolbarText, )
  UPD(ToolbarIconSizeAction, true)
  UPDEX1(ToolbarIconSizeNormalAction, true,
    ToolbarIconSizeNormalAction->Checked = (WinConfiguration->LargerToolbar <= 0))
  UPDEX1(ToolbarIconSizeLargeAction, GlyphsModule->IsLargerToolbarPossible(1),
    ToolbarIconSizeLargeAction->Checked = (WinConfiguration->LargerToolbar == 1))
  UPDEX1(ToolbarIconSizeVeryLargeAction, GlyphsModule->IsLargerToolbarPossible(2),
    ToolbarIconSizeVeryLargeAction->Checked = (WinConfiguration->LargerToolbar >= 2))
  UPDCOMP(CustomCommandsBand)
  UPD(ColorMenuAction2, HasTerminal)
  UPD(GoToAddressAction, true)
  UPD(CustomizeToolbarAction, IsToolbarCustomizable())

  // SORT
  UPDSORTA(Local, 2)
  #define UPDSORTL(NAME, COL) UPDSORT(Local, NAME, COL, -1, 2)
  UPDSORTL(Name, dvName)
  UPDSORTL(Ext, dvExt)
  UPDSORTL(Size, dvSize)
  UPDSORTL(Type, dvType)
  UPDSORTL(Changed, dvChanged)
  UPDSORTL(Attr, dvAttr)
  #undef UPDSORTL
  UPDSORTA(Remote, 2)
  UPDSORT(Remote, Name, dvName, uvName, 2)
  UPDSORT(Remote, Ext, dvExt, uvExt, 2)
  UPDSORT(Remote, Size, dvSize, uvSize, 2)
  UPDSORT(Remote, Changed, dvChanged, uvChanged, 2)
  UPDSORT(Remote, Rights, dvAttr, uvRights, 2)
  UPDSORT(Remote, Owner, -1, uvOwner, 2)
  UPDSORT(Remote, Group, -1, uvGroup, 2)
  UPDSORT(Remote, Type, dvType, uvType, 2)
  UPDSORTA(Current, )
  UPDSORT(Current, Name, dvName, uvName, )
  UPDSORT(Current, Ext, dvExt, uvExt, )
  UPDSORT(Current, Size, dvSize, uvSize, )
  UPDSORT(Current, Type, dvType, uvType, 2)
  UPDSORT(Current, Changed, dvChanged, uvChanged, )
  UPDSORT(Current, Rights, dvAttr, uvRights, )
  UPDSORT(Current, Owner, -1, uvOwner, )
  UPDSORT(Current, Group, -1, uvGroup, )
  #define COLVIEWPROPS ((TCustomDirViewColProperties*)(((TCustomDirView*)(((TListColumns*)(ListColumn->Collection))->Owner()))->ColProperties))
  UPDEX(SortColumnAscendingAction, (ListColumn != NULL), SortColumnAscendingAction->Checked =
    (COLVIEWPROPS->SortColumn == ListColumn->Index) && COLVIEWPROPS->SortAscending, )
  UPDEX(SortColumnDescendingAction, (ListColumn != NULL), SortColumnDescendingAction->Checked =
    (COLVIEWPROPS->SortColumn == ListColumn->Index) && !COLVIEWPROPS->SortAscending, )
  #undef COLVIEWPROPS
  UPD(AutoSizeLocalColumnsAction, DirViewEnabled(osLocal));
  UPD(AutoSizeRemoteColumnsAction, DirViewEnabled(osRemote));
  UPD(ResetLayoutLocalColumnsAction, true);
  UPD(ResetLayoutRemoteColumnsAction, true);

  // SHOW/HIDE COLUMN
  #define UPDSHCOLL(NAME) UPDSHCOL(Local, NAME, dv ## NAME, -1)
  UPDSHCOLL(Name)
  UPDSHCOLL(Ext)
  UPDSHCOLL(Size)
  UPDSHCOLL(Type)
  UPDSHCOLL(Changed)
  UPDSHCOLL(Attr)
  #undef UPDSHCOLL
  UPDSHCOL(Remote, Name, dvName, uvName)
  UPDSHCOL(Remote, Ext, dvExt, uvExt)
  UPDSHCOL(Remote, Size, dvSize, uvSize)
  UPDSHCOL(Remote, Changed, dvChanged, uvChanged)
  UPDSHCOL(Remote, Rights, dvAttr, uvRights)
  UPDSHCOL(Remote, Owner, -1, uvOwner)
  UPDSHCOL(Remote, Group, -1, uvGroup)
  UPDSHCOL(Remote, LinkTarget, -1, uvLinkTarget)
  UPDSHCOL(Remote, Type, dvType, uvType)
  UPD(HideColumnAction, (ListColumn != NULL))

  // SESSION
  UPDACT(NewTabAction, Action->ImageIndex = ScpExplorer->GetNewTabActionImageIndex())
  UPD(NewRemoteTabAction, true)
  UPD(NewLocalTabAction, true)
  UPDACT(DefaultToNewRemoteTabAction, Action->Checked = WinConfiguration->DefaultToNewRemoteTab)
  UPD(SiteManagerAction, true)
  UPD(DuplicateTabAction, HasManagedSession)
  UPD(RenameTabAction, HasManagedSession)
  UPD(CloseTabAction, HasManagedSession && ScpExplorer->CanCloseSession(ScpExplorer->ManagedSession))
  UPDEX1(DisconnectSessionAction, HasTerminal, DisconnectSessionAction->Visible = (ScpExplorer->Terminal == NULL) || !ScpExplorer->Terminal->Disconnected)
  UPDEX1(ReconnectSessionAction, (ScpExplorer->Terminal != NULL) && ScpExplorer->Terminal->Disconnected, ReconnectSessionAction->Visible = ReconnectSessionAction->Enabled)
  UPD(SavedSessionsAction2, true)
  UPD(WorkspacesAction, StoredSessions->HasAnyWorkspace())
  UPD(OpenedTabsAction, HasManagedSession)
  UPD(SaveCurrentSessionAction2, HasTerminal)
  UPD(SaveWorkspaceAction, HasManagedSession)

  // COMMAND
  UPD(CompareDirectoriesAction2, HasManagedSession) // Or simply true, as the command is in Commander only and it always has a managed session
  UPD(SynchronizeAction, HasTerminal)
  UPD(FullSynchronizeAction, HasTerminal)
  UPD(ConsoleAction, ScpExplorer->CanConsole())
  UPD(PuttyAction, HasTerminal && TTerminalManager::Instance()->CanOpenInPutty())
  UPD(SynchronizeBrowsingAction2, HasTerminal)
  UPD(CloseApplicationAction2, true)
  UPD(FileSystemInfoAction, HasTerminal)
  UPD(SessionGenerateUrlAction2, HasTerminal)
  UPD(ClearCachesAction, HasTerminal && !ScpExplorer->Terminal->AreCachesEmpty)
  UPD(NewFileAction, DirViewEnabled(osCurrent) && !WinConfiguration->DisableOpenEdit)
  UPD(EditorListCustomizeAction, true)
  UPD(ChangePasswordAction, ScpExplorer->CanChangePassword())
  UPD(PrivateKeyUploadAction, ScpExplorer->CanPrivateKeyUpload())
  UPD(IncrementalSearchStartAction, true);

  // CUSTOM COMMANDS
  UPD(CustomCommandsFileAction, true)
  UPD(CustomCommandsNonFileAction, true)
  UPD(CustomCommandsEnterAction, true)
  UPD(CustomCommandsEnterFocusedAction, true)
  UPDFUNC(CustomCommandsLastAction, CustomCommandsLastUpdate(CustomCommandsLastAction))
  UPDFUNC(CustomCommandsLastFocusedAction, CustomCommandsLastUpdate(CustomCommandsLastFocusedAction))
  UPD(CustomCommandsCustomizeAction, true)

  // QUEUE
  UPDEX(QueueEnableAction, HasTerminal, Action->Checked = ScpExplorer->GetQueueEnabled(), )
  #define UPDQUEUE(OPERATION) UPD(Queue ## OPERATION ## Action, \
    ScpExplorer->AllowQueueOperation(qo ## OPERATION))
  UPDQUEUE(GoTo)
  UPDQUEUE(Preferences)
  UPDEX(QueueItemQueryAction, ScpExplorer->AllowQueueOperation(qoItemQuery),
    Action->Visible = true, Action->Visible = false)
  UPDEX(QueueItemErrorAction, ScpExplorer->AllowQueueOperation(qoItemError),
    Action->Visible = true, Action->Visible = false)
  UPDEX(QueueItemPromptAction, ScpExplorer->AllowQueueOperation(qoItemPrompt),
    Action->Visible = true, Action->Visible = false)
  UPDQUEUE(ItemDelete)
  UPDEX(QueueItemExecuteAction, ScpExplorer->AllowQueueOperation(qoItemExecute),
    Action->Visible = true, Action->Visible =
      !ScpExplorer->AllowQueueOperation(qoItemPause) &&
      !ScpExplorer->AllowQueueOperation(qoItemResume))
  UPDEX(QueueItemPauseAction, ScpExplorer->AllowQueueOperation(qoItemPause),
    Action->Visible = true, Action->Visible = false)
  UPDEX(QueueItemResumeAction, ScpExplorer->AllowQueueOperation(qoItemResume),
    Action->Visible = true, Action->Visible = false)
  UPDQUEUE(ItemUp)
  UPDQUEUE(ItemDown)
  UPDQUEUE(PauseAll)
  UPDQUEUE(ResumeAll)
  UPDQUEUE(DeleteAll)
  UPDQUEUE(DeleteAllDone)
  #undef UPDQUEUE
  UPDEX(QueueItemSpeedAction, ScpExplorer->AllowQueueOperation(qoItemSpeed, &AuxVoidPtr),
    QueueItemSpeedAction->Text = SetSpeedLimit(reinterpret_cast<unsigned long>(AuxVoidPtr)),
    QueueItemSpeedAction->Text = L"")
  UPDEX1(QueueToggleShowAction, !ScpExplorer->IsLocalBrowserMode(), Action->Checked = ScpExplorer->ComponentVisible[fcQueueView])
  #define QUEUEACTION(SHOW) \
    UPDEX1(Queue ## SHOW ## Action, !ScpExplorer->IsLocalBrowserMode(), Action->Checked = WinConfiguration->QueueView.Show == qv ## SHOW)
  QUEUEACTION(Show)
  QUEUEACTION(HideWhenEmpty)
  QUEUEACTION(Hide)
  #undef QUEUEACTION
  UPDEX1(QueueCycleOnceEmptyAction, ScpExplorer->AllowQueueOperation(qoOnceEmpty),
    QueueCycleOnceEmptyAction->ImageIndex = CurrentQueueOnceEmptyAction()->ImageIndex;
    QueueCycleOnceEmptyAction->Checked = !QueueIdleOnceEmptyAction->Checked)
  UPD(QueueIdleOnceEmptyAction, ScpExplorer->AllowQueueOperation(qoOnceEmpty))
  UPD(QueueDisconnectOnceEmptyAction2, ScpExplorer->AllowQueueOperation(qoOnceEmpty))
  UPD(QueueSuspendOnceEmptyAction2, ScpExplorer->AllowQueueOperation(qoOnceEmpty))
  UPD(QueueShutDownOnceEmptyAction2, ScpExplorer->AllowQueueOperation(qoOnceEmpty))
  UPDCOMP(CommanderPreferencesBand)
  UPDACT(QueueToolbarAction,
    Action->Enabled = ScpExplorer->ComponentVisible[fcQueueView];
    Action->Checked = ScpExplorer->ComponentVisible[fcQueueToolbar])
  UPDACT(QueueFileListAction,
    Action->Enabled = ScpExplorer->ComponentVisible[fcQueueView];
    Action->Checked = ScpExplorer->ComponentVisible[fcQueueFileList])
  UPD(QueueResetLayoutColumnsAction, ScpExplorer->ComponentVisible[fcQueueView]);
  ;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ExplorerActionsExecute(
  TBasicAction * BasicAction, bool & Handled)
{
  DebugAssert(ScpExplorer);
  TAction * Action = DebugNotNull(dynamic_cast<TAction *>(BasicAction));
  if (!ScpExplorer->AllowedAction(Action, aaExecute))
  {
    Handled = true;
    return;
  }
  ScpExplorer->BeforeAction();

  bool ExecutedByShortcut = (Action->ActionComponent == NULL);
  unsigned int ShortCutFlag = FLAGMASK(ExecutedByShortcut, cocShortCutHint);

  {
    TAutoNestingCounter Counter(FBusy);
    // focused operation
    EXE(CurrentDeleteFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foDelete, osCurrent, true))
    EXE(CurrentPropertiesFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foSetProperties, osCurrent, true))
    EXE(CurrentEditFocusedAction, ScpExplorer->ExecuteFile(osCurrent, efDefaultEditor, NULL, true, true))
    EXE(CurrentSystemMenuFocusedAction, ScpExplorer->DisplaySystemContextMenu())
    EXE(CurrentEditWithFocusedAction, ScpExplorer->ExecuteCurrentFileWith(true))
    EXE(CurrentEditInternalFocusedAction, ScpExplorer->ExecuteFile(osCurrent, efInternalEditor, NULL, true, true))
    EXE(CurrentCopyToClipboardFocusedAction2, ScpExplorer->CopyFilesToClipboard(osCurrent, true))
    // operation
    EXE(CurrentEditAction, ScpExplorer->ExecuteFile(osCurrent, efDefaultEditor, NULL, true, false))
    EXE(CurrentEditInternalAction, ScpExplorer->ExecuteFile(osCurrent, efInternalEditor, NULL, true, false))
    EXE(CurrentEditWithAction, ScpExplorer->ExecuteCurrentFileWith(false))
    EXE(CurrentOpenAction, ScpExplorer->ExecuteCurrentFile())
    EXE(CurrentAddEditLinkAction, ScpExplorer->AddEditLink(osCurrent, false))
    EXE(CurrentAddEditLinkContextAction, ScpExplorer->AddEditLink(osCurrent, false))
    EXE(NewLinkAction, ScpExplorer->AddEditLink(osCurrent, true))
    EXE(CurrentRenameAction, ScpExplorer->ExecuteFileOperationCommand(foRename, osCurrent, false))
    EXE(CurrentDeleteAction, ScpExplorer->ExecuteFileOperationCommand(foDelete, osCurrent, false))
    EXE(CurrentDeleteAlternativeAction, ScpExplorer->ExecuteFileOperationCommand(foDelete, osCurrent, false, false, (void*)true))
    EXE(CurrentPropertiesAction, ScpExplorer->ExecuteFileOperationCommand(foSetProperties, osCurrent, false))
    EXE(CurrentCopyToClipboardAction2, ScpExplorer->CopyFilesToClipboard(osCurrent, false))
    EXE(FileListToCommandLineAction, ScpExplorer->PanelExport(osCurrent, peFileList, pedCommandLine))
    EXE(FileListToClipboardAction, ScpExplorer->PanelExport(osCurrent, peFileList, pedClipboard))
    EXE(FullFileListToClipboardAction, ScpExplorer->PanelExport(osCurrent, peFullFileList, pedClipboard))
    EXE(FileGenerateUrlAction2, ScpExplorer->FileGenerateUrl())
    EXE(FileListFromClipboardAction, ScpExplorer->FileListFromClipboard())
    EXE(LockAction, ScpExplorer->ExecuteFileOperationCommand(foLock, osCurrent, false))
    EXE(UnlockAction, ScpExplorer->ExecuteFileOperationCommand(foUnlock, osCurrent, false))
    EXE(CalculateDirectorySizesAction, ScpExplorer->CalculateDirectorySizes(osCurrent))
    // local selected operation
    EXE(LocalCopyAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, false, ShortCutFlag))
    EXE(LocalCopyQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, false, cocQueue))
    EXE(LocalCopyNonQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, false, cocNonQueue))
    EXE(LocalRenameAction2, ScpExplorer->ExecuteFileOperationCommand(foRename, osLocal, false))
    EXE(LocalEditAction2, ScpExplorer->ExecuteFile(osLocal, efDefaultEditor, NULL, true, false))
    EXE(LocalMoveAction, ScpExplorer->ExecuteFileOperationCommand(foMove, osLocal, false))
    EXE(LocalCreateDirAction3, ScpExplorer->CreateDirectory(osLocal))
    EXE(LocalDeleteAction2, ScpExplorer->ExecuteFileOperationCommand(foDelete, osLocal, false))
    EXE(LocalPropertiesAction2, ScpExplorer->ExecuteFileOperationCommand(foSetProperties, osLocal, false))
    EXE(LocalAddEditLinkAction3, ScpExplorer->AddEditLink(osLocal, false))
    EXE(LocalNewFileAction, ScpExplorer->EditNew(osLocal))
    EXE(LocalLocalCopyAction, ScpExplorer->LocalLocalCopyCommand(foCopy, osLocal, false, ShortCutFlag))
    EXE(LocalLocalMoveAction, ScpExplorer->LocalLocalCopyCommand(foMove, osLocal, false, ShortCutFlag))
    EXE(LocalOtherCopyAction, ScpExplorer->LocalLocalCopyCommand(foCopy, osOther, false, ShortCutFlag))
    EXE(LocalOtherMoveAction, ScpExplorer->LocalLocalCopyCommand(foMove, osOther, false, ShortCutFlag))
    EXE(LocalCalculateDirectorySizesAction, ScpExplorer->CalculateDirectorySizes(osLocal))
    // local focused operation
    EXE(LocalCopyFocusedAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, true, ShortCutFlag))
    EXE(LocalCopyFocusedQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, true, cocQueue))
    EXE(LocalCopyFocusedNonQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osLocal, true, cocNonQueue))
    EXE(LocalMoveFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foMove, osLocal, true))
    EXE(LocalLocalCopyFocusedAction, ScpExplorer->LocalLocalCopyCommand(foCopy, osCurrent, true, ShortCutFlag))
    EXE(LocalLocalMoveFocusedAction, ScpExplorer->LocalLocalCopyCommand(foMove, osCurrent, true, ShortCutFlag))
    // remote selected operation
    EXE(RemoteCopyAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, false, ShortCutFlag))
    EXE(RemoteCopyQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, false, cocQueue))
    EXE(RemoteCopyNonQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, false, cocNonQueue))
    EXE(RemoteRenameAction2, ScpExplorer->ExecuteFileOperationCommand(foRename, osRemote, false))
    EXE(RemoteEditAction2, ScpExplorer->ExecuteFile(osRemote, efDefaultEditor, NULL, true, false))
    EXE(RemoteMoveAction, ScpExplorer->ExecuteFileOperationCommand(foMove, osRemote, false))
    EXE(RemoteCreateDirAction3, ScpExplorer->CreateDirectory(osRemote))
    EXE(RemoteNewFileAction, ScpExplorer->EditNew(osRemote))
    EXE(RemoteDeleteAction2, ScpExplorer->ExecuteFileOperationCommand(foDelete, osRemote, false))
    EXE(RemotePropertiesAction2, ScpExplorer->ExecuteFileOperationCommand(foSetProperties, osRemote, false))
    EXE(RemoteMoveToAction, ScpExplorer->ExecuteFileOperationCommand(foRemoteMove, osCurrent, false))
    EXE(RemoteCopyToAction, ScpExplorer->ExecuteFileOperationCommand(foRemoteCopy, osCurrent, false))
    EXE(RemoteAddEditLinkAction3, ScpExplorer->AddEditLink(osRemote, false))
    EXE(RemoteCalculateDirectorySizesAction, ScpExplorer->CalculateDirectorySizes(osRemote))
    // remote focused operation
    EXE(RemoteCopyFocusedAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, true, ShortCutFlag))
    EXE(RemoteCopyFocusedQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, true, cocQueue))
    EXE(RemoteCopyFocusedNonQueueAction, ScpExplorer->ExecuteCopyOperationCommand(osRemote, true, cocNonQueue))
    EXE(RemoteMoveFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foMove, osRemote, true))
    EXE(RemoteMoveToFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foRemoteMove, osCurrent, true))
    EXE(RemoteCopyToFocusedAction, ScpExplorer->ExecuteFileOperationCommand(foRemoteCopy, osCurrent, true))
    // directory
    EXE(CurrentCreateDirAction, ScpExplorer->CreateDirectory(osCurrent))
    EXE(NewDirAction, ScpExplorer->CreateDirectory(osCurrent))
    EXE(RemoteFindFilesAction2, ScpExplorer->RemoteFindFiles())
    //selection
    EXE(SelectOneAction, DirView(osCurrent)->SelectCurrentItem(DirView(osCurrent)->NortonLike != nlOff))
    EXE(SelectAction, ScpExplorer->SelectByMask(osCurrent, true))
    EXE(UnselectAction, ScpExplorer->SelectByMask(osCurrent, false))
    EXE(SelectAllAction, ScpExplorer->SelectAll(osCurrent, smAll))
    EXE(InvertSelectionAction, ScpExplorer->SelectAll(osCurrent, smInvert))
    EXE(ClearSelectionAction, ScpExplorer->SelectAll(osCurrent, smNone))
    EXE(RestoreSelectionAction, ScpExplorer->RestoreSelectedNames(osCurrent))
    EXE(SelectSameExtAction, ScpExplorer->SelectSameExt(true))
    EXE(UnselectSameExtAction, ScpExplorer->SelectSameExt(false))
    EXE(LocalSelectAction2, ScpExplorer->SelectByMask(osLocal, true))
    EXE(LocalUnselectAction2, ScpExplorer->SelectByMask(osLocal, false))
    EXE(LocalSelectAllAction2, ScpExplorer->SelectAll(osLocal, smAll))
    EXE(RemoteSelectAction2, ScpExplorer->SelectByMask(osRemote, true))
    EXE(RemoteUnselectAction2, ScpExplorer->SelectByMask(osRemote, false))
    EXE(RemoteSelectAllAction2, ScpExplorer->SelectAll(osRemote, smAll))
    EXE(PasteAction3, ScpExplorer->PasteFromClipBoard())

    // style
    EXE(RemoteCycleStyleAction,
      if (DirView(osRemote)->DirViewStyle == dvsThumbnail) DirView(osRemote)->DirViewStyle = dvsIcon;
        else DirView(osRemote)->DirViewStyle = (TDirViewStyle)(DirView(osRemote)->DirViewStyle + 1);
      ScpExplorer->UpdateControls();
    )
    #define STYLEACTION(SIDE, STYLE) EXE(SIDE ## STYLE ## Action, \
      ScpExplorer->ChangeDirViewStyle(os ## SIDE, dvs ## STYLE))
    STYLEACTION(Remote, Icon)
    STYLEACTION(Remote, SmallIcon)
    STYLEACTION(Remote, List)
    STYLEACTION(Remote, Report)
    STYLEACTION(Remote, Thumbnail)
    STYLEACTION(Local, Report)
    STYLEACTION(Local, Thumbnail)
    #undef STYLEACTION

    #define PANEL_ACTIONS(SIDE) \
      EXE(SIDE ## BackAction, ScpExplorer->HistoryGo(os ## SIDE, -1)) \
      EXE(SIDE ## ForwardAction, ScpExplorer->HistoryGo(os ## SIDE, 1)) \
      EXE(SIDE ## ParentDirAction, DirView(os ## SIDE)->ExecuteParentDirectory()) \
      EXE(SIDE ## RootDirAction, DirView(os ## SIDE)->ExecuteRootDirectory()) \
      EXE(SIDE ## HomeDirAction, ScpExplorer->HomeDirectory(os ## SIDE)) \
      EXE(SIDE ## RefreshAction, ScpExplorer->ReloadDirectory(os ## SIDE)) \
      EXE(SIDE ## OpenDirAction, ScpExplorer->OpenDirectory(os ## SIDE)) \
      EXE(SIDE ## OtherDirAction, DirView(os ## SIDE)->Path = DirView(ScpExplorer->GetOtherSide(os ## SIDE))->Path) \
      EXE(SIDE ## ChangePathAction2, ScpExplorer->ChangePath(os ## SIDE)) \
      EXE(SIDE ## AddBookmarkAction2, ScpExplorer->AddBookmark(os ## SIDE)) \
      EXE(SIDE ## PathToClipboardAction2, ScpExplorer->PanelExport(os ## SIDE, pePath, pedClipboard)) \
      EXE(SIDE ## FilterAction, ScpExplorer->Filter(os ## SIDE)) \
      EXE(SIDE ## ExploreDirectoryAction, ScpExplorer->ExploreLocalDirectory(os ## SIDE))
    PANEL_ACTIONS(Local)
    PANEL_ACTIONS(Remote)
    #undef PANEL_ACTIONS

    //HELP
    EXE(AboutAction, DoAboutDialog(Configuration))
    EXE(HomepageAction, OpenBrowser(LoadStr(HOMEPAGE_URL)))
    EXE(HistoryPageAction, OpenBrowser(LoadStr(HISTORY_URL)))
    EXE(TableOfContentsAction, Application->HelpSystem->ShowTableOfContents())
    EXE(ForumPageAction, OpenBrowser(LoadStr(FORUM_URL)))
    EXE(CheckForUpdatesAction, CheckForUpdates(false))
    EXE(UpdatesPreferencesAction, PreferencesDialog(pmUpdates))
    EXE(DonatePageAction, OpenBrowser(LoadStr(DONATE_URL)))
    EXE(DownloadPageAction, OpenBrowser(LoadStr(DOWNLOAD_URL)))
    EXE(TipsAction, ShowTips())

    // VIEW
    EXECOMP2(SessionsTabs, 2)
    EXECOMP(StatusBar)
    EXECOMP(ToolBar2)
    EXECOMP2(LocalStatusBar, 2)
    EXECOMP2(RemoteStatusBar, 2)
    #define EMIT_BAND_COMPONENT2(COMP, NUM) EXECOMP2(COMP, NUM)
    BAND_COMPONENTS
    #undef EMIT_BAND_COMPONENT2
    EXECOMP(CommandLinePanel)
    EXECOMP(RemoteTree)
    EXECOMP(LocalTree)
    EXE(CommanderLocalPanelAction, )
    EXE(CommanderRemotePanelAction, )
    EXE(GoToCommandLineAction, ScpExplorer->GoToCommandLine())
    EXE(GoToTreeAction, ScpExplorer->GoToTree())

    EXE(ShowHiddenFilesAction, ScpExplorer->ToggleShowHiddenFiles())
    EXE(FormatSizeBytesNoneAction, ScpExplorer->SetFormatSizeBytes(fbNone))
    EXE(FormatSizeBytesKilobytesAction, ScpExplorer->SetFormatSizeBytes(fbKilobytes))
    EXE(FormatSizeBytesShortAction, ScpExplorer->SetFormatSizeBytes(fbShort))
    EXE(AutoReadDirectoryAfterOpAction, ScpExplorer->ToggleAutoReadDirectoryAfterOp())
    EXE(PreferencesAction, PreferencesDialog(::pmDefault) )
    EXE(PresetsPreferencesAction, PreferencesDialog(pmPresets) )
    EXE(FileColorsPreferencesAction, PreferencesDialog(pmFileColors) )
    EXE(LockToolbarsAction, WinConfiguration->LockToolbars = !WinConfiguration->LockToolbars)
    EXE(SelectiveToolbarTextAction, WinConfiguration->SelectiveToolbarText = !WinConfiguration->SelectiveToolbarText)
    EXE(ToolbarIconSizeAction, )
    EXE(ToolbarIconSizeNormalAction, WinConfiguration->LargerToolbar = 0)
    EXE(ToolbarIconSizeLargeAction, WinConfiguration->LargerToolbar = 1)
    EXE(ToolbarIconSizeVeryLargeAction, WinConfiguration->LargerToolbar = 2)
    EXECOMP(CustomCommandsBand)
    EXE(ColorMenuAction2, CreateSessionColorMenu(ColorMenuAction2))
    EXE(GoToAddressAction, ScpExplorer->GoToAddress())
    EXE(CustomizeToolbarAction, CreateToolbarButtonsList())

    #define COLVIEWPROPS ((TCustomDirViewColProperties*)(((TCustomDirView*)(((TListColumns*)(ListColumn->Collection))->Owner()))->ColProperties))
    // SORT
    EXESORTA(Local, 2)
    #define EXESORTL(NAME, COL) EXESORT(Local, NAME, COL, COL, 2)
    EXESORTL(Name, dvName)
    EXESORTL(Ext, dvExt)
    EXESORTL(Size, dvSize)
    EXESORTL(Type, dvType)
    EXESORTL(Changed, dvChanged)
    EXESORTL(Attr, dvAttr)
    #undef EXESORTL
    EXESORTA(Remote, 2)
    EXESORT(Remote, Name, dvName, uvName, 2)
    EXESORT(Remote, Ext, dvExt, uvExt, 2)
    EXESORT(Remote, Size, dvSize, uvSize, 2)
    EXESORT(Remote, Changed, dvChanged, uvChanged, 2)
    EXESORT(Remote, Rights, dvAttr, uvRights, 2)
    EXESORT(Remote, Owner, -1, uvOwner, 2)
    EXESORT(Remote, Group, -1, uvGroup, 2)
    EXESORT(Remote, Type, dvType, uvType, 2)
    EXESORTA(Current, )
    EXESORT(Current, Name, dvName, uvName, )
    EXESORT(Current, Ext, dvExt, uvExt, )
    EXESORT(Current, Size, dvSize, uvSize, )
    EXESORT(Current, Type, dvType, uvType, 2)
    EXESORT(Current, Changed, dvChanged, uvChanged, )
    EXESORT(Current, Rights, dvAttr, uvRights, )
    EXESORT(Current, Owner, -1, uvOwner, )
    EXESORT(Current, Group, -1, uvGroup, )
    EXE(SortColumnAscendingAction, DebugAssert(ListColumn);
      COLVIEWPROPS->SortColumn = ListColumn->Index; COLVIEWPROPS->SortAscending = true; ListColumn = NULL )
    EXE(SortColumnDescendingAction, DebugAssert(ListColumn);
      COLVIEWPROPS->SortColumn = ListColumn->Index; COLVIEWPROPS->SortAscending = false; ListColumn = NULL )
    EXE(AutoSizeLocalColumnsAction, ScpExplorer->AutoSizeColumns(osLocal))
    EXE(AutoSizeRemoteColumnsAction, ScpExplorer->AutoSizeColumns(osRemote))
    EXE(ResetLayoutLocalColumnsAction, ScpExplorer->ResetLayoutColumns(osLocal))
    EXE(ResetLayoutRemoteColumnsAction, ScpExplorer->ResetLayoutColumns(osRemote))

    // SHOW/HIDE COLUMN
    #define EXESHCOLL(NAME) EXESHCOL(Local, NAME, dv ## NAME, -1)
    EXESHCOLL(Name)
    EXESHCOLL(Ext)
    EXESHCOLL(Size)
    EXESHCOLL(Type)
    EXESHCOLL(Changed)
    EXESHCOLL(Attr)
    #undef EXESHCOLL
    EXESHCOL(Remote, Name, dvName, uvName)
    EXESHCOL(Remote, Ext, dvExt, uvExt)
    EXESHCOL(Remote, Size, dvSize, uvSize)
    EXESHCOL(Remote, Changed, dvChanged, uvChanged)
    EXESHCOL(Remote, Rights, dvAttr, uvRights)
    EXESHCOL(Remote, Owner, -1, uvOwner)
    EXESHCOL(Remote, Group, -1, uvGroup)
    EXESHCOL(Remote, LinkTarget, -1, uvLinkTarget)
    EXESHCOL(Remote, Type, dvType, uvType)
    EXE(HideColumnAction, DebugAssert(ListColumn);
      COLVIEWPROPS->Visible[ListColumn->Index] = false; ListColumn = NULL )
    #undef COLVIEWPROPS

    // SESSION
    EXE(NewTabAction, ScpExplorer->NewTab(osCurrent, !ExecutedByShortcut))
    EXE(NewRemoteTabAction, ScpExplorer->NewTab(osRemote))
    EXE(NewLocalTabAction, ScpExplorer->NewTab(osLocal))
    EXE(DefaultToNewRemoteTabAction, WinConfiguration->DefaultToNewRemoteTab = !WinConfiguration->DefaultToNewRemoteTab)
    EXE(SiteManagerAction, ScpExplorer->NewSession())
    EXE(DuplicateTabAction, ScpExplorer->DuplicateTab())
    EXE(RenameTabAction, ScpExplorer->RenameTab())
    EXE(CloseTabAction, ScpExplorer->CloseTab())
    EXE(DisconnectSessionAction, ScpExplorer->DisconnectSession())
    EXE(ReconnectSessionAction, ScpExplorer->ReconnectSession())
    EXE(SavedSessionsAction2, CreateSessionListMenu(SavedSessionsAction2))
    EXE(WorkspacesAction, CreateWorkspacesMenu(WorkspacesAction))
    EXE(OpenedTabsAction, CreateOpenedSessionListMenu(OpenedTabsAction))
    EXE(SaveCurrentSessionAction2, ScpExplorer->SaveCurrentSession())
    EXE(SaveWorkspaceAction, ScpExplorer->SaveWorkspace(false))

    // COMMAND
    EXE(CompareDirectoriesAction2, ScpExplorer->CompareDirectories())
    EXE(SynchronizeAction, ScpExplorer->SynchronizeDirectories())
    EXE(FullSynchronizeAction, ScpExplorer->FullSynchronizeDirectories())
    EXE(ConsoleAction, ScpExplorer->OpenConsole())
    EXE(PuttyAction, TTerminalManager::Instance()->OpenInPutty())
    EXE(SynchronizeBrowsingAction2, ScpExplorer->SynchronizeBrowsingChanged())
    EXE(CloseApplicationAction2, ScpExplorer->CloseApp())
    EXE(FileSystemInfoAction, ScpExplorer->FileSystemInfo())
    EXE(SessionGenerateUrlAction2, ScpExplorer->SessionGenerateUrl())
    EXE(ClearCachesAction, ScpExplorer->Terminal->ClearCaches())
    EXE(NewFileAction, ScpExplorer->EditNew(osCurrent))
    EXE(EditorListCustomizeAction, PreferencesDialog(pmEditor))
    EXE(ChangePasswordAction, ScpExplorer->ChangePassword())
    EXE(PrivateKeyUploadAction, ScpExplorer->PrivateKeyUpload())
    EXE(IncrementalSearchStartAction, ScpExplorer->IncrementalSearchStart())

    // CUSTOM COMMANDS
    EXE(CustomCommandsFileAction, CreateCustomCommandsMenu(CustomCommandsFileAction, ccltFile))
    EXE(CustomCommandsNonFileAction, CreateCustomCommandsMenu(CustomCommandsNonFileAction, ccltNonFile))
    EXE(CustomCommandsEnterAction, ScpExplorer->AdHocCustomCommand(false))
    EXE(CustomCommandsEnterFocusedAction, ScpExplorer->AdHocCustomCommand(true))
    EXE(CustomCommandsLastAction, ScpExplorer->LastCustomCommand(false))
    EXE(CustomCommandsLastFocusedAction, ScpExplorer->LastCustomCommand(true))
    EXE(CustomCommandsCustomizeAction, CustomCommandsCustomize(NULL))

    // QUEUE
    EXE(QueueEnableAction, ScpExplorer->ToggleQueueEnabled())
    #define EXEQUEUE(OPERATION) EXE(Queue ## OPERATION ## Action, \
      ScpExplorer->ExecuteQueueOperation(qo ## OPERATION))
    EXEQUEUE(GoTo)
    EXEQUEUE(Preferences)
    EXEQUEUE(ItemQuery)
    EXEQUEUE(ItemError)
    EXEQUEUE(ItemPrompt)
    EXEQUEUE(ItemDelete)
    EXEQUEUE(ItemExecute)
    EXEQUEUE(ItemPause)
    EXEQUEUE(ItemResume)
    EXEQUEUE(ItemUp)
    EXEQUEUE(ItemDown)
    EXEQUEUE(PauseAll)
    EXEQUEUE(ResumeAll)
    EXEQUEUE(DeleteAll)
    EXEQUEUE(DeleteAllDone)
    #undef EXEQUEUE
    EXE(QueueToggleShowAction, ScpExplorer->ToggleQueueVisibility())
    #define QUEUEACTION(SHOW) EXE(Queue ## SHOW ## Action, \
      TQueueViewConfiguration Config = WinConfiguration->QueueView; \
      if (Config.Show != qvShow) Config.LastHideShow = Config.Show; \
      Config.Show = qv ## SHOW; \
      WinConfiguration->QueueView = Config)
    QUEUEACTION(Show)
    QUEUEACTION(HideWhenEmpty)
    QUEUEACTION(Hide)
    #undef QUEUEACTION
    EXE(QueueCycleOnceEmptyAction, CycleQueueOnceEmptyAction())
    EXE(QueueIdleOnceEmptyAction, SetQueueOnceEmptyAction(QueueIdleOnceEmptyAction))
    EXE(QueueDisconnectOnceEmptyAction2, SetQueueOnceEmptyAction(QueueDisconnectOnceEmptyAction2))
    EXE(QueueSuspendOnceEmptyAction2, SetQueueOnceEmptyAction(QueueSuspendOnceEmptyAction2))
    EXE(QueueShutDownOnceEmptyAction2, SetQueueOnceEmptyAction(QueueShutDownOnceEmptyAction2))
    EXECOMP(QueueToolbar)
    EXECOMP(QueueFileList)
    EXE(QueueResetLayoutColumnsAction, ScpExplorer->QueueResetLayoutColumns());
    EXE(QueueItemSpeedAction, )
    ;
  }

  DoIdle();
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::UpdateNonVisibleActions()
{
  // following actions needs to be updated even when all clients
  // are invisible, so the queue list toolbar button can be shown
  NonVisualDataModule->QueueItemQueryAction->Update();
  NonVisualDataModule->QueueItemErrorAction->Update();
  NonVisualDataModule->QueueItemPromptAction->Update();
}
//---------------------------------------------------------------------------
#define CTRL TShiftState() << ssCtrl
#define ALT TShiftState() << ssAlt
#define SHIFT TShiftState() << ssShift
#define CTRLSHIFT TShiftState() << ssCtrl << ssShift
#define CTRLALT TShiftState() << ssCtrl << ssAlt
#define NONE TShiftState()
void __fastcall TNonVisualDataModule::ExplorerShortcuts()
{
  // Directory
  CurrentCreateDirAction->ShortCut = ShortCut(L'D', CTRL);
  // File operation
  CurrentRenameAction->ShortCut = ShortCut(VK_F2, NONE);
  CurrentEditAction->ShortCut = ShortCut(L'E', CTRL);
  CurrentAddEditLinkAction->ShortCut = ShortCut(L'L', CTRLALT);
  CurrentEditInternalAction->ShortCut = 0;
  CurrentEditInternalFocusedAction->ShortCut = 0;
  // Focused operation
  RemoteCopyAction->ShortCut = ShortCut(L'T', CTRL);
  RemoteMoveAction->ShortCut = ShortCut(L'M', CTRL);
  CurrentDeleteFocusedAction->ShortCut = ShortCut(VK_DELETE, NONE);
  CurrentPropertiesFocusedAction->ShortCut = ShortCut(VK_RETURN, ALT);
  RemoteMoveToFocusedAction->ShortCut = ShortCut(L'M', CTRLALT);
  // remote directory
  RemoteOpenDirAction->ShortCut = ShortCut(L'O', CTRL);
  RemoteRefreshAction->ShortCut = ShortCut(VK_F5, NONE);
  RemoteHomeDirAction->ShortCut = ShortCut(L'H', CTRL);
  RemotePathToClipboardAction2->ShortCut = ShortCut(L'P', CTRLSHIFT);
  // selected operation
  CurrentDeleteAlternativeAction->ShortCut = ShortCut(VK_DELETE, SHIFT);
  RemoteMoveToAction->ShortCut = ShortCut(L'M', CTRLALT);
  // commands
  NewFileAction->ShortCut = ShortCut(L'E', CTRLSHIFT);
  RemoteFindFilesAction2->ShortCut = ShortCut(VK_F3, NONE);
  NewTabAction->ShortCut = ShortCut(L'N', CTRL);

  CloseApplicationAction2->ShortCut = ShortCut(VK_F4, ALT);

  CloneShortcuts();

  SessionsNewTabItem->DropdownCombo = false;
  SetSubmenu(SessionsNewTabItem, false);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CommanderShortcuts()
{
  bool ExplorerKeyboardShortcuts = WinConfiguration->ScpCommander.ExplorerKeyboardShortcuts;
  // Directory
  CurrentCreateDirAction->ShortCut = ShortCut(VK_F7, NONE);
  // File operation
  CurrentRenameAction->ShortCut = ShortCut(VK_F2, NONE);
  CurrentEditAction->ShortCut = ShortCut(VK_F4, NONE);
  CurrentAddEditLinkAction->ShortCut = ShortCut(VK_F6, ALT);
  CurrentEditInternalAction->ShortCut = ShortCut(VK_F4, CTRLALT);
  CurrentEditInternalFocusedAction->ShortCut = ShortCut(VK_F4, CTRLALT);
  // Focused operation
  RemoteCopyAction->ShortCut =
    ExplorerKeyboardShortcuts ? ShortCut(L'K', CTRL) : ShortCut(VK_F5, NONE);
  RemoteMoveAction->ShortCut = ShortCut(VK_F6, NONE);
  CurrentDeleteFocusedAction->ShortCut = ShortCut(VK_F8, NONE);
  CurrentPropertiesFocusedAction->ShortCut = ShortCut(VK_F9, NONE);
  RemoteMoveToFocusedAction->ShortCut = ShortCut(VK_F6, SHIFT);
  RemoteCopyToFocusedAction->ShortCut = ShortCut(VK_F5, SHIFT);
  // remote directory
  RemoteOpenDirAction->ShortCut = ShortCut(L'O', CTRL);
  RemoteRefreshAction->ShortCut =
    ExplorerKeyboardShortcuts ? ShortCut(VK_F5, NONE) : ShortCut(L'R', CTRL);
  RemoteHomeDirAction->ShortCut = ShortCut(L'H', CTRL);
  RemotePathToClipboardAction2->ShortCut = ShortCut(VK_OEM_6 /* ] */, CTRL);
  // local directory
  LocalPathToClipboardAction2->ShortCut = ShortCut(VK_OEM_4 /* [ */, CTRL);
  // selected operation
  CurrentDeleteAction->SecondaryShortCuts->Clear();
  CurrentDeleteAction->SecondaryShortCuts->Add(ShortCutToText(ShortCut(VK_DELETE, NONE)));
  CurrentDeleteAlternativeAction->ShortCut = ShortCut(VK_F8, SHIFT);
  CurrentDeleteAlternativeAction->SecondaryShortCuts->Clear();
  CurrentDeleteAlternativeAction->SecondaryShortCuts->Add(ShortCutToText(ShortCut(VK_DELETE, SHIFT)));
  RemoteMoveToAction->ShortCut = ShortCut(VK_F6, SHIFT);
  RemoteCopyToAction->ShortCut = ShortCut(VK_F5, SHIFT);
  // selection
  SelectOneAction->ShortCut = VK_INSERT;
  // commands
  NewFileAction->ShortCut = ShortCut(VK_F4, SHIFT);
  RemoteFindFilesAction2->ShortCut =
    ExplorerKeyboardShortcuts ? ShortCut(VK_F3, NONE) : ShortCut(VK_F7, ALT);
  NewTabAction->ShortCut = ShortCut(L'T', CTRL);
  // legacy shortcut (can be removed when necessary)
  NewFileAction->SecondaryShortCuts->Clear();
  NewFileAction->SecondaryShortCuts->Add(ShortCutToText(ShortCut(VK_F4, CTRLSHIFT)));

  CloseApplicationAction2->ShortCut = ShortCut(VK_F10, NONE);

  TShortCut CtrlF4 = ShortCut(VK_F4, CTRL);
  LocalSortByExtAction2->ShortCut = ExplorerKeyboardShortcuts ? TShortCut(0) : CtrlF4;
  RemoteSortByExtAction2->ShortCut = LocalSortByExtAction2->ShortCut;
  CurrentSortByExtAction->ShortCut = LocalSortByExtAction2->ShortCut;
  int Index = CloseTabAction->SecondaryShortCuts->IndexOfShortCut(CtrlF4);
  if (ExplorerKeyboardShortcuts && (Index < 0))
  {
    CloseTabAction->SecondaryShortCuts->Add(ShortCutToText(CtrlF4));
  }
  else if (!ExplorerKeyboardShortcuts && (Index >= 0))
  {
    CloseTabAction->SecondaryShortCuts->Delete(Index);
  }

  CloneShortcuts();
}
#undef CTRL
#undef ALT
#undef NONE
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CloneShortcuts()
{
  // Commands
  NewDirAction->ShortCut = CurrentCreateDirAction->ShortCut;
  // File operation
  CurrentAddEditLinkContextAction->ShortCut = CurrentAddEditLinkAction->ShortCut;
  LocalAddEditLinkAction3->ShortCut = CurrentAddEditLinkAction->ShortCut;
  RemoteAddEditLinkAction3->ShortCut = CurrentAddEditLinkAction->ShortCut;
  RemoteNewFileAction->ShortCut = NewFileAction->ShortCut;
  LocalNewFileAction->ShortCut = NewFileAction->ShortCut;
  // local directory
  LocalOpenDirAction->ShortCut = RemoteOpenDirAction->ShortCut;
  LocalRefreshAction->ShortCut = RemoteRefreshAction->ShortCut;
  LocalHomeDirAction->ShortCut = RemoteHomeDirAction->ShortCut;
  // selected operation
  CurrentDeleteAction->ShortCut = CurrentDeleteFocusedAction->ShortCut;
  CurrentPropertiesAction->ShortCut = CurrentPropertiesFocusedAction->ShortCut;
  // local selected operation
  LocalCopyAction->ShortCut = RemoteCopyAction->ShortCut;
  LocalLocalCopyAction->ShortCut = RemoteCopyAction->ShortCut;
  LocalOtherCopyAction->ShortCut = RemoteCopyAction->ShortCut;
  LocalRenameAction2->ShortCut = CurrentRenameAction->ShortCut;
  LocalEditAction2->ShortCut = CurrentEditAction->ShortCut;
  LocalMoveAction->ShortCut = RemoteMoveAction->ShortCut;
  LocalLocalMoveAction->ShortCut = LocalMoveAction->ShortCut;
  LocalOtherMoveAction->ShortCut = LocalMoveAction->ShortCut;
  LocalCreateDirAction3->ShortCut = CurrentCreateDirAction->ShortCut;
  LocalDeleteAction2->ShortCut = CurrentDeleteAction->ShortCut;
  LocalPropertiesAction2->ShortCut = CurrentPropertiesAction->ShortCut;
  // local focused operation
  LocalCopyFocusedAction->ShortCut = LocalCopyAction->ShortCut;
  LocalMoveFocusedAction->ShortCut = LocalMoveAction->ShortCut;
  LocalLocalCopyFocusedAction->ShortCut = LocalCopyAction->ShortCut;
  LocalLocalMoveFocusedAction->ShortCut = LocalMoveAction->ShortCut;
  // remote selected operation
  RemoteRenameAction2->ShortCut = CurrentRenameAction->ShortCut;
  RemoteEditAction2->ShortCut = CurrentEditAction->ShortCut;
  RemoteCreateDirAction3->ShortCut = CurrentCreateDirAction->ShortCut;
  RemoteDeleteAction2->ShortCut = CurrentDeleteAction->ShortCut;
  RemotePropertiesAction2->ShortCut = CurrentPropertiesAction->ShortCut;
  // remote focused operation
  RemoteCopyFocusedAction->ShortCut = RemoteCopyAction->ShortCut;
  RemoteMoveFocusedAction->ShortCut = RemoteMoveAction->ShortCut;
  // selection
  LocalSelectAction2->ShortCut = SelectAction->ShortCut;
  LocalUnselectAction2->ShortCut = UnselectAction->ShortCut;
  LocalSelectAllAction2->ShortCut = SelectAllAction->ShortCut;
  RemoteSelectAction2->ShortCut = SelectAction->ShortCut;
  RemoteUnselectAction2->ShortCut = UnselectAction->ShortCut;
  RemoteSelectAllAction2->ShortCut = SelectAllAction->ShortCut;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SetScpExplorer(TCustomScpExplorerForm * value)
{
  FScpExplorer = value;
  SessionIdleTimer->Enabled = (FScpExplorer != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionIdleTimerTimer(
      TObject */*Sender*/)
{
  DoIdle();
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::DoIdle()
{
  if (!FSessionIdleTimerExecuting)
  {
    FSessionIdleTimerExecuting = true;
    try
    {
      DebugAssert(ScpExplorer);
      ScpExplorer->Idle();
    }
    __finally
    {
      FSessionIdleTimerExecuting = false;
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TNonVisualDataModule::CustomCommandCaption(const TCustomCommandType * Command, bool Toolbar)
{
  UnicodeString Result = Command->Name;
  if (Toolbar)
  {
    Result = StripHotkey(Result);
    Result = EscapeHotkey(Result);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TNonVisualDataModule::CustomCommandHint(const TCustomCommandType * Command)
{
  UnicodeString Name = StripHotkey(Command->Name);
  UnicodeString LongHint =
    !Command->Description.IsEmpty() ? Command->Description : FMTLOAD(CUSTOM_COMMAND_HINT_LONG, (Name, Command->Command));
  UnicodeString Result = FORMAT(L"%s|%s", (Name, LongHint));
  return Result;
}
//---------------------------------------------------------------------------
const int CustomCommandOnFocused = 0x0100;
const int CustomCommandBoth = 0x0200;
const int CustomCommandExtension = 0x0400;
const int CustomCommandIndexMask = 0x00FF;
//---------------------------------------------------------------------------
// See IsValidIdent
static UnicodeString MakeIdent(const UnicodeString & S)
{
  UnicodeString Result;
  for (int Index = 1; Index <= S.Length(); Index++)
  {
    wchar_t C = S[Index];
    if (IsLetter(C) ||
        (!Result.IsEmpty() && IsDigit(C)))
    {
      Result += C;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TNonVisualDataModule::CreateCustomCommandsListMenu(
  TCustomCommandList * List, TTBCustomItem * Menu, bool OnFocused, bool Toolbar, TCustomCommandListType ListType,
  int Tag, TStrings * HiddenCommands)
{
  int Result = 0;

  for (int Index = 0; Index < List->Count; Index++)
  {
    const TCustomCommandType * Command = List->Commands[Index];
    int State = ScpExplorer->CustomCommandState(*Command, OnFocused, ListType);

    if (State >= 0)
    {
      TTBCustomItem * Item = new TTBXItem(Owner);
      Item->Caption = CustomCommandCaption(Command, Toolbar);
      Item->Tag = Index | Tag;
      Item->Enabled = (State > 0);
      if (Toolbar)
      {
        UnicodeString Name = ExtractFileName(Command->Id);
        if (Name.IsEmpty())
        {
          Name = Command->Name;
        }
        Name = MakeIdent(Name);
        Name += L"CustomCommand";
        // This is only the last resort to avoid run-time errors.
        // If there are duplicates, button hidding won't be deterministic.
        int X = 0;
        UnicodeString UniqueName = Name;
        while (Owner->FindComponent(UniqueName) != NULL)
        {
          X++;
          UniqueName = FORMAT(L"%s%d", (Name, X));
        }
        Item->Name = UniqueName;
        Item->Visible = (HiddenCommands->IndexOf(Item->Name) < 0);
      }
      if (OnFocused)
      {
        Item->Tag = Item->Tag | CustomCommandOnFocused;
      }
      if (ListType == ccltBoth)
      {
        Item->Tag = Item->Tag | CustomCommandBoth;
      }
      Item->Hint = CustomCommandHint(Command);
      Item->ShortCut = Command->ShortCut;
      Item->OnClick = CustomCommandClick;

      Menu->Add(Item);

      Result++;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CustomCommandsCustomize(TObject *)
{
  PreferencesDialog(pmCustomCommands);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateCustomCommandsMenu(
  TTBCustomItem * Menu, bool OnFocused, bool Toolbar, TCustomCommandListType ListType, TStrings * HiddenCommands)
{
  CreateCustomCommandsListMenu(WinConfiguration->CustomCommandList, Menu, OnFocused, Toolbar, ListType, 0, HiddenCommands);

  TTBCustomItem * Item;

  if ((ListType == ccltAll) || (ListType == ccltFile) || (ListType == ccltNonFile))
  {
    Item = new TTBXItem(Menu);
    Item->Action = OnFocused ? CustomCommandsEnterFocusedAction : CustomCommandsEnterAction;
    Menu->Add(Item);
    GiveTBItemPriority(Item);

    Item = new TTBXItem(Menu);
    Item->Action = OnFocused ? CustomCommandsLastFocusedAction : CustomCommandsLastAction;
    if (Toolbar)
    {
      Item->Caption = EscapeHotkey(StripHotkey(LoadStr(CUSTOM_COMMAND_LAST_SHORT)));
    }
    Menu->Add(Item);
    GiveTBItemPriority(Item);
  }

  TTBXSeparatorItem * Separator = AddMenuSeparator(Menu);
  int ExtensionItems =
    CreateCustomCommandsListMenu(WinConfiguration->ExtensionList, Menu, OnFocused, Toolbar, ListType, CustomCommandExtension, HiddenCommands);
  Separator->Visible = (ExtensionItems > 0);

  AddMenuSeparator(Menu);

  if (((ListType == ccltFile) || ((ListType == ccltNonFile) && !OnFocused)) && DebugAlwaysTrue(!Toolbar))
  {
    Item = new TTBXSubmenuItem(Menu);
    // copy the texts from the action, but do not use is as a handler, because it will duplicate the auxiliary commands in the submenu
    Item->Action = (ListType == ccltFile) ? CustomCommandsNonFileAction : CustomCommandsFileAction;
    Item->Action = NULL;
    TCustomCommandListType SubListType = (ListType == ccltFile) ? ccltNonFile : ccltFile;
    int CustomCommandItems = CreateCustomCommandsListMenu(WinConfiguration->CustomCommandList, Item, OnFocused, Toolbar, SubListType, 0, HiddenCommands);
    TTBXSeparatorItem * Separator = AddMenuSeparator(Item);
    int ExtensionItems = CreateCustomCommandsListMenu(WinConfiguration->ExtensionList, Item, OnFocused, Toolbar, SubListType, CustomCommandExtension, HiddenCommands);
    Separator->Visible = (ExtensionItems > 0);
    Item->Enabled = (CustomCommandItems + ExtensionItems > 0);

    Menu->Add(Item);
  }

  if (!Toolbar && (ListType != ccltBoth))
  {
    Item = new TTBXItem(Menu);
    Item->Action = CustomCommandsBandAction;
    Menu->Add(Item);
    GiveTBItemPriority(Item);
  }

  Item = new TTBXItem(Menu);
  Item->Action = CustomCommandsCustomizeAction;
  if (ListType == ccltBoth)
  {
    // Hack. Bypass the Busy test in ExplorerActionsExecute.
    Item->OnClick = CustomCommandsCustomize;
  }
  Menu->Add(Item);
  GiveTBItemPriority(Item);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateCustomCommandsMenu(
  TAction * Action, bool OnFocused, TCustomCommandListType ListType)
{
  DebugAssert(Action);
  TTBCustomItem * Menu = dynamic_cast<TTBCustomItem *>(Action->ActionComponent);
  if (Menu)
  {
    int PrevCount = Menu->Count;

    CreateCustomCommandsMenu(Menu, OnFocused, false, ListType, NULL);

    for (int Index = 0; Index < PrevCount; Index++)
    {
      Menu->Delete(0);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateCustomCommandsMenu(TAction * Action, TCustomCommandListType ListType)
{
  TTBCustomItem * Menu = dynamic_cast<TTBCustomItem *>(Action->ActionComponent);
  if (DebugAlwaysTrue(Menu != NULL))
  {
    bool OnFocused =
      (Menu == RemoteDirViewPopupCustomCommandsMenu) || (Menu == LocalFilePopupCustomCommandsMenu) || (Menu == RemoteFilePopupCustomCommandsMenu);
    CreateCustomCommandsMenu(Action, OnFocused, ListType);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TNonVisualDataModule::CheckCustomCommandsToolbarList(TTBXToolbar * Toolbar, TCustomCommandList * List, int & Index)
{
  bool Changed = false;
  int CommandIndex = 0;
  while (!Changed && (CommandIndex < List->Count))
  {
    TTBCustomItem * Item = Toolbar->Items->Items[Index];
    const TCustomCommandType * Command = List->Commands[CommandIndex];

    UnicodeString Caption = CustomCommandCaption(Command, true);
    UnicodeString Hint = CustomCommandHint(Command);
    Changed =
      (Item->Caption != Caption) ||
      (Item->Hint != Hint);

    Index++;
    CommandIndex++;
  }
  return Changed;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::UpdateCustomCommandsToolbarList(TTBXToolbar * Toolbar, TCustomCommandList * List, int & Index)
{
  for (int CommandIndex = 0; CommandIndex < List->Count; CommandIndex++, Index++)
  {
    TTBCustomItem * Item = Toolbar->Items->Items[Index];
    DebugAssert((Item->Tag & CustomCommandIndexMask) == CommandIndex);
    int State = ScpExplorer->CustomCommandState(*List->Commands[CommandIndex], false, ccltAll);
    DebugAssert(State >= 0);
    Item->Enabled = (State > 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::UpdateCustomCommandsToolbar(TTBXToolbar * Toolbar)
{
  TCustomCommandList * CustomCommandList = WinConfiguration->CustomCommandList;
  TCustomCommandList * ExtensionList = WinConfiguration->ExtensionList;
  int AfterCustomCommandsCommandCount = 2; // ad hoc, last
  int AdditionalCommands = AfterCustomCommandsCommandCount + 3; // custom/ext separator + separator, customize
  int CommandCount = CustomCommandList->Count + ExtensionList->Count;
  bool Changed = (CommandCount + AdditionalCommands != Toolbar->Items->Count);
  if (!Changed)
  {
    int Index = 0;
    Changed = CheckCustomCommandsToolbarList(Toolbar, CustomCommandList, Index);

    if (!Changed)
    {
      Index += AfterCustomCommandsCommandCount;
      Changed = (dynamic_cast<TTBXSeparatorItem *>(Toolbar->Items->Items[Index]) == NULL);
      Index++;

      if (!Changed)
      {
        Changed = CheckCustomCommandsToolbarList(Toolbar, ExtensionList, Index);
      }
    }
  }

  if (Changed)
  {
    Toolbar->BeginUpdate();
    try
    {
      std::unique_ptr<TStrings> HiddenCommands(CreateSortedStringList());
      for (int Index = 0; Index < Toolbar->Items->Count; Index++)
      {
        TTBCustomItem * Item = Toolbar->Items->Items[Index];
        if (IsCustomizableToolbarItem(Item) && !Item->Visible)
        {
          HiddenCommands->Add(Item->Name);
        }
      }
      Toolbar->Items->Clear();
      CreateCustomCommandsMenu(Toolbar->Items, false, true, ccltAll, HiddenCommands.get());
      DebugAssert(CommandCount + AdditionalCommands == Toolbar->Items->Count);
    }
    __finally
    {
      Toolbar->EndUpdate();
    }
  }
  else
  {
    int Index = 0;
    UpdateCustomCommandsToolbarList(Toolbar, CustomCommandList, Index);
    Index += AfterCustomCommandsCommandCount + 1;
    UpdateCustomCommandsToolbarList(Toolbar, ExtensionList, Index);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CustomCommandClick(TObject * Sender)
{
  TTBCustomItem * Item = dynamic_cast<TTBCustomItem *>(Sender);
  DebugAssert(Item);
  const TCustomCommandList * List = FLAGSET(Item->Tag, CustomCommandExtension) ? WinConfiguration->ExtensionList : WinConfiguration->CustomCommandList;
  const TCustomCommandType * Command = List->Commands[Item->Tag & CustomCommandIndexMask];
  if (FLAGCLEAR(Item->Tag, CustomCommandBoth))
  {
    ScpExplorer->ExecuteFileOperationCommand(foCustomCommand, osRemote,
      FLAGSET(Item->Tag, CustomCommandOnFocused), false, const_cast<TCustomCommandType *>(Command));
  }
  else
  {
    ScpExplorer->BothCustomCommand(*Command);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateSessionColorMenu(TAction * Action)
{
  if (DebugAlwaysTrue(Action->ActionComponent != NULL))
  {
    ::CreateSessionColorMenu(Action->ActionComponent, ScpExplorer->SessionColor,
      SessionColorChange);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionColorChange(TColor Color)
{
  ScpExplorer->SessionColor = Color;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateSessionListMenu(TAction * Action)
{
  StoredSessions->Reload();
  CreateSessionListMenuLevel(
    dynamic_cast<TTBCustomItem *>(Action->ActionComponent), 0, 0);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TNonVisualDataModule::GetSessionFolderRoot(
  TSessionData * Data, int Level)
{
  UnicodeString Path = Data->Name;
  UnicodeString Root;
  for (int ALevel = 0; ALevel < Level; ALevel++)
  {
    Root.Insert(CutToChar(Path, L'/', false) + L'/', Root.Length() + 1);
  }
  return Root;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateSessionListMenuLevel(
  TTBCustomItem * Menu, int Index, int Level)
{
  Menu->Clear();

  TTBCustomItem * Item = new TTBXItem(Menu);

  UnicodeString Root;
  if (Level == 0)
  {
    Item->Action = SiteManagerAction;
    Root = L"";
  }
  else
  {
    DebugAssert(Index < StoredSessions->Count);
    TSessionData * Data = StoredSessions->Sessions[Index];

    Root = GetSessionFolderRoot(Data, Level);

    Item->Caption = LoadStr(SAVEDSESSIONFOLDER_THIS_OPEN);
    Item->Tag = MAKELONG(Index, Level);
    UnicodeString FolderName = Root;
    if (DebugAlwaysTrue(!FolderName.IsEmpty() && FolderName[FolderName.Length()] == L'/'))
    {
      FolderName.Delete(FolderName.Length(), 1);
    }
    Item->Hint = FMTLOAD(SAVEDSESSIONFOLDER_THIS_HINT, (FolderName));
    Item->OnClick = SessionFolderThisItemClick;
  }

  Menu->Insert(Menu->Count, Item);

  AddMenuSeparator(Menu);

  int FirstSession = Menu->Count;
  UnicodeString PrevName;
  while (Index < StoredSessions->Count)
  {
    TSessionData * Data = StoredSessions->Sessions[Index];
    if (!AnsiSameText(Data->Name.SubString(1, Root.Length()), Root))
    {
      // Sessions are sorted, so no chance further sessions may match
      break;
    }
    else if (!Data->IsWorkspace)
    {
      UnicodeString Name = Data->Name.SubString(Root.Length() + 1, Data->Name.Length() - Root.Length());
      int P = Name.Pos(L'/');
      if (P > 0)
      {
        Name.SetLength(P - 1);
      }

      if (Name != PrevName)
      {
        if (P > 0)
        {
          TTBCustomItem * Item = new TTBXSubmenuItem(Menu);
          Item->Caption = Name;
          Item->Tag = ((Level + 1) << 16) | Index; // MAKELONG
          Item->ImageIndex = SavedSessionsAction2->ImageIndex;
          Item->OnClick = SessionFolderItemClick;

          Menu->Insert(FirstSession, Item);
          FirstSession++;
        }
        else
        {
          TTBCustomItem * Item = new TTBXItem(Menu);
          Item->Caption = Name;
          Item->Tag = Index;
          Item->Hint = FMTLOAD(SAVEDSESSION_HINT, (Data->Name));
          Item->OnClick = SessionItemClick;
          Menu->Insert(Menu->Count, Item);
        }

        PrevName = Name;
      }
    }
    Index++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionFolderItemClick(TObject * Sender)
{
  TTBCustomItem * Item = dynamic_cast<TTBCustomItem *>(Sender);
  DebugAssert(Item != NULL);
  CreateSessionListMenuLevel(Item, LOWORD(Item->Tag), HIWORD(Item->Tag));
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionFolderThisItemClick(TObject * Sender)
{
  TTBCustomItem * Item = DebugNotNull(dynamic_cast<TTBCustomItem *>(Sender));
  int Index = LOWORD(Item->Tag);
  int Level = HIWORD(Item->Tag);
  UnicodeString Folder = GetSessionFolderRoot(StoredSessions->Sessions[Index], Level);
  ScpExplorer->OpenFolderOrWorkspace(Folder);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionItemClick(TObject * Sender)
{
  DebugAssert(StoredSessions && (((TTBCustomItem *)Sender)->Tag < StoredSessions->Count));
  ScpExplorer->OpenStoredSession(StoredSessions->Sessions[((TTBCustomItem *)Sender)->Tag]);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateWorkspacesMenu(TAction * Action)
{
  StoredSessions->Reload();
  if (!StoredSessions->HasAnyWorkspace())
  {
    Abort();
  }
  else
  {
    TTBCustomItem * Menu =
      DebugNotNull(dynamic_cast<TTBCustomItem *>(Action->ActionComponent));

    Menu->Clear();

    std::unique_ptr<TStrings> Workspaces(StoredSessions->GetWorkspaces());
    for (int Index = 0; Index < Workspaces->Count; Index++)
    {
      TTBCustomItem * Item = new TTBXItem(Menu);
      Item->Caption = Workspaces->Strings[Index];
      Item->Tag = Index;
      Item->Hint = FMTLOAD(WORKSPACE_HINT, (Workspaces->Strings[Index]));
      Item->OnClick = WorkspaceItemClick;
      Menu->Insert(Menu->Count, Item);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::WorkspaceItemClick(TObject * Sender)
{
  std::unique_ptr<TStrings> Workspaces(StoredSessions->GetWorkspaces());
  ScpExplorer->OpenFolderOrWorkspace(
    Workspaces->Strings[DebugNotNull(dynamic_cast<TTBCustomItem *>(Sender))->Tag]);
}
//---------------------------------------------------------------------------
TShortCut __fastcall TNonVisualDataModule::OpenSessionShortCut(int Index)
{
  if (Index >= 0 && Index < 10)
  {
    return ShortCut((Word)(Index < 9 ? L'0' + 1 + Index : L'0'),
      TShiftState() << ssAlt);
  }
  else
  {
    return scNone;
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateOpenedSessionListMenu(TAction * Action)
{
  TTBCustomItem * OpenedSessionsMenu = dynamic_cast<TTBCustomItem *>(Action->ActionComponent);
  DebugAssert(OpenedSessionsMenu != NULL);
  TTerminalManager * Manager = TTerminalManager::Instance();
  TStrings * SessionList = Manager->SessionList;
  int PrevCount = OpenedSessionsMenu->Count;
  for (int Index = 0; Index < SessionList->Count; Index++)
  {
    TManagedTerminal * Session = dynamic_cast<TManagedTerminal *>(SessionList->Objects[Index]);
    DebugAssert(Session != NULL);
    TTBCustomItem * Item = new TTBXItem(OpenedSessionsMenu);
    Item->Caption = SessionList->Strings[Index];
    Item->Tag = int(Session);
    Item->Hint = FMTLOAD(OPENED_TAB_HINT, (Item->Caption));
    Item->Checked = (Manager->ActiveSession == Session);
    Item->ShortCut = OpenSessionShortCut(Index);
    Item->OnClick = OpenedSessionItemClick;
    Item->RadioItem = true;
    OpenedSessionsMenu->Add(Item);
  }
  for (int Index = 0; Index < PrevCount; Index++)
  {
    OpenedSessionsMenu->Delete(0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::OpenedSessionItemClick(TObject * Sender)
{
  TTerminalManager::Instance()->ActiveSession = (TManagedTerminal*)(((TMenuItem *)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateEditorListMenu(TTBCustomItem * Menu, bool OnFocused)
{
  DebugAssert(Menu != NULL);
  int PrevCount = Menu->Count;

  TTBCustomItem * Item;

  Item = new TTBXItem(Menu);
  Item->Action = OnFocused ? CurrentEditFocusedAction : CurrentEditAction;
  Menu->Add(Item);

  AddMenuSeparator(Menu);

  Item = new TTBXItem(Menu);
  Item->Action = OnFocused ? CurrentEditInternalFocusedAction : CurrentEditInternalAction;
  Menu->Add(Item);

  AddMenuSeparator(Menu);

  TStringList * UsedEditors = CreateSortedStringList();
  try
  {
    const TEditorList * EditorList = WinConfiguration->EditorList;
    for (int Index = 0; Index < EditorList->Count; Index++)
    {
      const TEditorPreferences * Editor = EditorList->Editors[Index];

      if ((Editor->Data->Editor == edExternal) &&
          (UsedEditors->IndexOf(Editor->Data->ExternalEditor) < 0))
      {
        UsedEditors->Add(Editor->Data->ExternalEditor);

        TTBCustomItem * Item = new TTBXItem(Menu);
        Item->Caption = Editor->Name;
        Item->Tag = Index;
        Item->Hint = FMTLOAD(EXTERNAL_EDITOR_HINT, (Editor->Name));
        if (OnFocused)
        {
          Item->OnClick = EditorItemClickFocused;
        }
        else
        {
          Item->OnClick = EditorItemClick;
        }
        Menu->Add(Item);
      }
    }

    Item = new TTBXItem(Menu);
    Item->Action = OnFocused ? CurrentEditWithFocusedAction : CurrentEditWithAction;
    Menu->Add(Item);

    AddMenuSeparator(Menu);

    Item = new TTBXItem(Menu);
    Item->Action = EditorListCustomizeAction;
    Menu->Add(Item);

    for (int Index = 0; Index < PrevCount; Index++)
    {
      Menu->Delete(0);
    }
  }
  __finally
  {
    delete UsedEditors;
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::DoEditorItemClick(TObject * Sender, bool OnFocused)
{
  int Tag = dynamic_cast<TTBXItem*>(Sender)->Tag;
  const TEditorList * EditorList = WinConfiguration->EditorList;
  // sanity check
  if (Tag < EditorList->Count)
  {
    ScpExplorer->ExecuteFile(osCurrent, efExternalEditor, EditorList->Editors[Tag]->Data,
      true, OnFocused);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::EditorItemClick(TObject * Sender)
{
  DoEditorItemClick(Sender, false);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::EditorItemClickFocused(TObject * Sender)
{
  DoEditorItemClick(Sender, true);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueuePopupPopup(TObject * /*Sender*/)
{
  TAction * Action = NULL;

  switch (ScpExplorer->DefaultQueueOperation())
  {
    case qoItemQuery:
      Action = QueueItemQueryAction;
      break;

    case qoItemError:
      Action = QueueItemErrorAction;
      break;

    case qoItemPrompt:
      Action = QueueItemPromptAction;
      break;

    case qoItemExecute:
      Action = QueueItemExecuteAction;
      break;

    case qoItemPause:
      Action = QueueItemPauseAction;
      break;

    case qoItemResume:
      Action = QueueItemResumeAction;
      break;
  }

  TTBCustomItem * Item;
  for (int Index = 0; Index < QueuePopup->Items->Count; Index++)
  {
    Item = QueuePopup->Items->Items[Index];
    TTBItemOptions O = Item->Options;
    if ((Action != NULL) && (Item->Action == Action))
    {
      O << tboDefault;
    }
    else
    {
      O >> tboDefault;
    }
    Item->Options = O;
  }

  QueueSpeedComboBoxItemUpdate(QueuePopupSpeedComboBoxItem);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ShowUpdatesUpdate()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  unsigned short H, M, S, MS;
  DecodeTime(Now(), H, M, S, MS);
  int CurrentCompoundVer = Configuration->CompoundVersion;
  CheckForUpdatesAction->ImageIndex =
    ((Updates.HaveResults && (Updates.Results.ForVersion == CurrentCompoundVer) &&
      !Updates.Results.Disabled &&
      ((Updates.Results.Critical && !Updates.ShownResults && (MS >= 500)) ||
       ((!Updates.Results.Critical || Updates.ShownResults) &&
        ((Updates.Results.Version > CurrentCompoundVer) ||
         !Updates.Results.Message.IsEmpty())))) ? 80 :
     ((int(Updates.Period) <= 0) ? 81 : 63));
  CheckForUpdatesAction->Visible = !IsUWP();
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::PreferencesDialog(TPreferencesMode APreferencesMode)
{
  if (ScpExplorer != NULL)
  {
    ScpExplorer->PreferencesDialog(APreferencesMode);
  }
  else
  {
    DoPreferencesDialog(APreferencesMode);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CustomCommandsLastUpdate(TAction * Action)
{
  TCustomCommandType Command;
  int State;
  bool Defined = ScpExplorer->GetLastCustomCommand(
    (Action == CustomCommandsLastFocusedAction), Command, State);
  Action->Visible = Defined;
  if (Defined)
  {
    UnicodeString TitleCommand = Command.Command;
    int MaxTitleCommandLen = 20;
    if (TitleCommand.Length() > MaxTitleCommandLen)
    {
      TitleCommand = TitleCommand.SubString(1, MaxTitleCommandLen - 3) + Ellipsis;
    }
    Action->Caption = FMTLOAD(CUSTOM_COMMAND_LAST, (EscapeHotkey(TitleCommand)));
    Action->Hint = Command.Command;
    Action->Enabled = (State > 0);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TNonVisualDataModule::QueueItemSpeed(const UnicodeString & Text,
  TTBXComboBoxItem * Item)
{
  // Keep in sync with TProgressForm::ItemSpeed
  unsigned long Speed = GetSpeedLimit(Text);
  ScpExplorer->ExecuteQueueOperation(qoItemSpeed, reinterpret_cast<void*>(Speed));

  UnicodeString Result = SetSpeedLimit(Speed);
  SaveToHistory(Item->Strings, Result);
  CustomWinConfiguration->History[L"SpeedLimit"] = Item->Strings;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueuePopupSpeedComboBoxItemItemClick(
  TObject * Sender)
{
  TTBXComboBoxItem * Item = dynamic_cast<TTBXComboBoxItem *>(Sender);
  QueueItemSpeedAction->Text = QueueItemSpeed(Item->Text, Item);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueueSpeedComboBoxItemAcceptText(
  TObject * Sender, UnicodeString & NewText, bool & /*Accept*/)
{
  TTBXComboBoxItem * Item = dynamic_cast<TTBXComboBoxItem *>(Sender);
  NewText = QueueItemSpeed(NewText, Item);
  QueueItemSpeedAction->Text = NewText;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueueSpeedComboBoxItem(TTBXComboBoxItem * Item)
{
  // IDE often looses this link
  Item->OnAcceptText = QueueSpeedComboBoxItemAcceptText;
  Item->OnItemClick = QueuePopupSpeedComboBoxItemItemClick;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueueSpeedComboBoxItemUpdate(TTBXComboBoxItem * Item)
{
  CopySpeedLimits(CustomWinConfiguration->History[L"SpeedLimit"], Item->Strings);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SetQueueOnceEmptyAction(TAction * Action)
{
  TAction * Current = CurrentQueueOnceEmptyAction();
  if (Current != Action)
  {
    Current->Checked = false;
    Action->Checked = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CycleQueueOnceEmptyAction()
{
  TAction * Current = CurrentQueueOnceEmptyAction();
  Current->Checked = false;
  if (Current == QueueIdleOnceEmptyAction)
  {
    QueueDisconnectOnceEmptyAction2->Checked = true;
  }
  else if (Current == QueueDisconnectOnceEmptyAction2)
  {
    QueueSuspendOnceEmptyAction2->Checked = true;
  }
  else if (Current == QueueSuspendOnceEmptyAction2)
  {
    QueueShutDownOnceEmptyAction2->Checked = true;
  }
  else if (Current == QueueShutDownOnceEmptyAction2)
  {
    QueueIdleOnceEmptyAction->Checked = true;
  }
  else
  {
    DebugFail();
  }
}
//---------------------------------------------------------------------------
TAction * __fastcall TNonVisualDataModule::CurrentQueueOnceEmptyAction()
{
  TAction * Result;
  if (QueueIdleOnceEmptyAction->Checked)
  {
    Result = QueueIdleOnceEmptyAction;
  }
  else if (QueueDisconnectOnceEmptyAction2->Checked)
  {
    Result = QueueDisconnectOnceEmptyAction2;
  }
  else if (QueueSuspendOnceEmptyAction2->Checked)
  {
    Result = QueueSuspendOnceEmptyAction2;
  }
  else if (QueueShutDownOnceEmptyAction2->Checked)
  {
    Result = QueueShutDownOnceEmptyAction2;
  }
  else
  {
    DebugFail();
  }
  return Result;
}
//---------------------------------------------------------------------------
TOnceDoneOperation __fastcall TNonVisualDataModule::CurrentQueueOnceEmptyOperation()
{
  TOnceDoneOperation Result;
  TBasicAction * Current = CurrentQueueOnceEmptyAction();
  if (Current == QueueIdleOnceEmptyAction)
  {
    Result = odoIdle;
  }
  else if (Current == QueueDisconnectOnceEmptyAction2)
  {
    Result = odoDisconnect;
  }
  else if (Current == QueueSuspendOnceEmptyAction2)
  {
    Result = odoSuspend;
  }
  else if (Current == QueueShutDownOnceEmptyAction2)
  {
    Result = odoShutDown;
  }
  else
  {
    DebugFail();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ResetQueueOnceEmptyOperation()
{
  SetQueueOnceEmptyAction(QueueIdleOnceEmptyAction);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::StartBusy()
{
  FBusy++;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::EndBusy()
{
  FBusy--;
}
//---------------------------------------------------------------------------
bool __fastcall TNonVisualDataModule::GetBusy()
{
  return
    (FBusy > 0) ||
    ((ScpExplorer != NULL) && ScpExplorer->IsBusy());
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::FocusedEditMenuItemPopup(TTBCustomItem * Sender,
  bool /*FromLink*/)
{
  CreateEditorListMenu(Sender, true);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::EditMenuItemPopup(TTBCustomItem * Sender,
  bool /*FromLink*/)
{
  CreateEditorListMenu(Sender, false);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueuePopupSpeedComboBoxItemAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString /*AText*/, int /*AIndex*/, int & ImageIndex)
{
  // Use fixed image (do not change image by item index)
  ImageIndex = Sender->ImageIndex;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ToolbarButtonItemClick(TObject * Sender)
{
  TTBCustomItem * Item = DebugNotNull(dynamic_cast<TTBCustomItem *>(Sender));
  TTBCustomItem * ButtonItem = reinterpret_cast<TTBCustomItem *>(Item->Tag);
  ButtonItem->Visible = !ButtonItem->Visible;
}
//---------------------------------------------------------------------------
bool __fastcall TNonVisualDataModule::IsCustomizableToolbarItem(TTBCustomItem * Item)
{
  return
    ((dynamic_cast<TTBXItem *>(Item) != NULL) ||
     (dynamic_cast<TTBXSubmenuItem *>(Item) != NULL)) &&
    (Item->Action != CustomCommandsLastAction) &&
    DebugAlwaysTrue(Item->Action != CustomCommandsLastFocusedAction);
}
//---------------------------------------------------------------------------
bool __fastcall TNonVisualDataModule::IsToolbarCustomizable()
{
  bool Result = false;
  if (FCustomizedToolbar != NULL)
  {
    for (int Index = 0; Index < FCustomizedToolbar->Items->Count; Index++)
    {
      if (IsCustomizableToolbarItem(FCustomizedToolbar->Items->Items[Index]))
      {
        Result = true;
        break;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateToolbarButtonsList()
{
  if (FCustomizedToolbar != NULL)
  {
    TTBCustomItem * CustomizeItem = DebugNotNull(dynamic_cast<TTBCustomItem *>(CustomizeToolbarAction->ActionComponent));
    CustomizeItem->Clear();

    for (int Index = 0; Index < FCustomizedToolbar->Items->Count; Index++)
    {
      TTBCustomItem * ButtonItem = FCustomizedToolbar->Items->Items[Index];

      TTBCustomItem * Item = NULL;
      if (dynamic_cast<TTBSeparatorItem *>(ButtonItem) != NULL)
      {
        Item = new TTBSeparatorItem(CustomizeItem);
      }
      else if (IsCustomizableToolbarItem(ButtonItem))
      {
        Item = new TTBXItem(CustomizeItem);
        Item->Caption = StripEllipsis(ButtonItem->Caption);
        Item->ImageIndex = ButtonItem->ImageIndex;
        Item->Tag = reinterpret_cast<int>(ButtonItem);
        Item->OnClick = ToolbarButtonItemClick;
        Item->Checked = ButtonItem->Visible;
      }

      if (Item != NULL)
      {
        CustomizeItem->Insert(CustomizeItem->Count, Item);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ControlContextPopup(TObject * Sender, const TPoint & MousePos)
{
  TTBDock * Dock = dynamic_cast<TTBDock *>(Sender);
  if (Dock != NULL)
  {
    // While we can identify toolbar for which context menu is popping up in OnExecute,
    // we cannot in OnUpdate, so we have to remember it here.
    FCustomizedToolbar = dynamic_cast<TTBCustomToolbar *>(Dock->ControlAtPos(MousePos, true, true, false));
  }
  else
  {
    FCustomizedToolbar = NULL;
  }
}
