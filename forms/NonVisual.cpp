//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "NonVisual.h"

#include <Common.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <Tools.h>
#include <Setup.h>

#include <Log.h>
#include <Interface.h>
#include <Glyphs.h>
#include "WinConfiguration.h"
#include "TerminalManager.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TNonVisualDataModule *NonVisualDataModule;
//---------------------------------------------------------------------------
#define SCPCOMMANDER ((TScpCommanderForm *)ScpExplorer)
#define UPDEX(HandleAction, Condition, OtherEnabled, OtherDisabled) if (Action == HandleAction) { \
  ((TCustomAction *)Action)->Enabled = (Condition); \
  if (((TCustomAction *)Action)->Enabled) { OtherEnabled; } else { OtherDisabled; }; \
  Handled = true; } else
#define UPD(HandleAction, Condition) if (Action == HandleAction) { \
  ((TCustomAction *)Action)->Enabled = (Condition); Handled = true; } else
#define EXE(HandleAction, Command) if (Action == HandleAction) { \
  Command; Handled = true; } else
#define UPDACT(HandleAction, Command) \
  EXE(HandleAction, ((TCustomAction *)Action)->Enabled = true; Command)
#define UPDCOMP(COMP) if (Action == COMP ## Action) { COMP ## Action->Enabled = true; \
  COMP ## Action->Checked = ScpExplorer->ComponentVisible[fc ## COMP]; Handled = true; } else
#define EXECOMP(COMP) EXE(COMP ## Action, \
  ScpExplorer->ComponentVisible[fc ## COMP] = !ScpExplorer->ComponentVisible[fc ## COMP] )
#define COLPROPS(SIDE) \
  ((TCustomDirViewColProperties*)ScpExplorer->DirView(os ## SIDE)->ColProperties)
#define UPDSORT(SIDE, PREFIX, COL) if (Action == SIDE ## SortBy ## COL ## Action) { \
  SIDE ## SortBy ## COL ## Action->Enabled = true; Handled = true; \
  SIDE ## SortBy ## COL ## Action->Checked = (COLPROPS(SIDE)->SortColumn == PREFIX ## COL); } else
#define EXESORT(SIDE, PREFIX, COL) EXE(SIDE ## SortBy ## COL ## Action, \
    if (COLPROPS(SIDE)->SortColumn == PREFIX ## COL) \
      COLPROPS(SIDE)->SortAscending = !COLPROPS(SIDE)->SortAscending; \
    else COLPROPS(SIDE)->SortColumn = PREFIX ## COL )
#define UPDSORTA(SIDE) if (Action == SIDE ## SortAscendingAction) { \
  SIDE ## SortAscendingAction->Enabled = true; Handled = true; \
  SIDE ## SortAscendingAction->Checked = COLPROPS(SIDE)->SortAscending; } else
#define EXESORTA(SIDE) EXE(SIDE ## SortAscendingAction, \
  COLPROPS(SIDE)->SortAscending = !COLPROPS(SIDE)->SortAscending; )
#define UPDSORTC(LPREFIX, LCOL, RPREFIX, RCOL) if (Action == CurrentSortBy ## RCOL ## Action) { \
  CurrentSortBy ## RCOL ## Action->Enabled = ScpExplorer->AllowedAction((TAction *)Action, aaShortCut); \
  if (CurrentSortBy ## RCOL ## Action->Enabled) { \
    if (ScpExplorer->DirView(osCurrent) == ScpExplorer->DirView(osRemote)) \
         CurrentSortBy ## RCOL ## Action->Checked = (COLPROPS(Current)->SortColumn == RPREFIX ## RCOL); \
    else CurrentSortBy ## RCOL ## Action->Checked = (COLPROPS(Current)->SortColumn == LPREFIX ## LCOL); \
  } else CurrentSortBy ## RCOL ## Action->Checked =  false; Handled = true; } else
#define EXESORTC(COL, LCOL, RCOL) \
  EXE(CurrentSortBy ## COL ## Action, \
    Integer NewSortCol = \
      ((ScpExplorer->DirView(osCurrent) == ScpExplorer->DirView(osRemote)) ? RCOL : LCOL); \
    if (COLPROPS(Current)->SortColumn == NewSortCol) \
      COLPROPS(Current)->SortAscending = !COLPROPS(Current)->SortAscending; \
    else COLPROPS(Current)->SortColumn = NewSortCol \
  )
#define UPDSHCOL(SIDE, PREFIX, COL) \
  EXE(ShowHide ## SIDE ## COL ## ColumnAction, \
    ShowHide ## SIDE ## COL ## ColumnAction->Checked = COLPROPS(SIDE)->Visible[PREFIX ## COL])
#define EXESHCOL(SIDE, PREFIX, COL) \
  EXE(ShowHide ## SIDE ## COL ## ColumnAction, \
    COLPROPS(SIDE)->Visible[PREFIX ## COL] = !COLPROPS(SIDE)->Visible[PREFIX ## COL])
//---------------------------------------------------------------------------
__fastcall TNonVisualDataModule::TNonVisualDataModule(TComponent* Owner)
        : TDataModule(Owner)
{
  FListColumn = NULL;
  FSessionIdleTimerExecuting = false;
  FIdle = true;

  FGlyphsModule = NULL;
  HANDLE ResourceModule = GUIConfiguration->ChangeResourceModule(NULL);
  try
  {
    FGlyphsModule = new TGlyphsModule(Owner);
  }
  __finally
  {
    GUIConfiguration->ChangeResourceModule(ResourceModule);
  }

  for (int i = 0; i < FGlyphsModule->ComponentCount; i++)
  {
    TImageList * Source = dynamic_cast<TImageList *>(FGlyphsModule->Components[i]);
    if (Source != NULL)
    {
      TImageList * Dest = dynamic_cast<TImageList *>(FindComponent(Source->Name));
      assert(Dest != NULL);
      assert(Dest->ShareImages);
      Dest->Handle = Source->Handle;
    }
  }
}
//---------------------------------------------------------------------------
__fastcall TNonVisualDataModule::~TNonVisualDataModule()
{
  delete FGlyphsModule;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::LogActionsUpdate(
      TBasicAction *Action, bool &Handled)
{
  TLogMemo * LogMemo = TTerminalManager::Instance()->LogMemo;
  bool ValidLogMemo = LogMemo && LogMemo->Parent;
  UPD(LogClearAction, ValidLogMemo && LogMemo->Lines->Count)
  UPD(LogSelectAllAction, ValidLogMemo && LogMemo->Lines->Count &&
    LogMemo->SelLength != LogMemo->Lines->Text.Length())
  UPD(LogCopyAction, ValidLogMemo && LogMemo->SelLength)

  UPD(LogCloseAction, Configuration->Logging && (WinConfiguration->LogView == lvWindow))
  ;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::LogActionsExecute(
      TBasicAction *Action, bool &Handled)
{
  assert(FIdle);
  FIdle = false;

  try
  {
    TLogMemo * LogMemo = TTerminalManager::Instance()->LogMemo;
    assert(LogMemo && LogMemo->Parent);
    EXE(LogClearAction, LogMemo->SessionLog->Clear())
    EXE(LogSelectAllAction, LogMemo->SelectAll())
    EXE(LogCopyAction, LogMemo->CopyToClipboard())

    EXE(LogCloseAction, WinConfiguration->LogView = lvNone)
    ;
  }
  __finally
  {
    FIdle = true;
  }
  
  DoIdle();
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ExplorerActionsUpdate(
      TBasicAction *Action, bool &Handled)
{
  if (!ScpExplorer || !ScpExplorer->AllowedAction((TAction *)Action, aaUpdate))
  {
    ((TAction *)Action)->Enabled = false;
    Handled = true;
    return;
  }
  // CURRENT DIRVIEW
  bool EnableSelectedOperation = ScpExplorer->EnableSelectedOperation[osCurrent];
  bool EnableFocusedOperation = ScpExplorer->EnableFocusedOperation[osCurrent];
  // focused operation
  UPD(CurrentCopyFocusedAction, EnableFocusedOperation)
  UPD(CurrentMoveFocusedAction, EnableFocusedOperation)
  UPD(CurrentDeleteFocusedAction, EnableFocusedOperation)
  UPD(CurrentPropertiesFocusedAction, EnableFocusedOperation)
  UPD(RemoteMoveToFocusedAction, EnableFocusedOperation && (DirView(osRemote) == DirView(osCurrent)))
  UPD(RemoteCopyToFocusedAction, EnableFocusedOperation && (DirView(osRemote) == DirView(osCurrent)))
  // file operation
  UPD(CurrentRenameAction, EnableFocusedOperation &&
    ((ScpExplorer->HasDirView[osLocal] && DirView(osLocal) == DirView(osCurrent)) ||
      ScpExplorer->Terminal->IsCapable[fcRename]))
  UPD(CurrentEditAction, EnableFocusedOperation &&
    !WinConfiguration->DisableOpenEdit &&
    !DirView(osCurrent)->ItemIsDirectory(DirView(osCurrent)->ItemFocused))
  UPD(CurrentEditAlternativeAction, EnableFocusedOperation &&
    !WinConfiguration->DisableOpenEdit &&
    !DirView(osCurrent)->ItemIsDirectory(DirView(osCurrent)->ItemFocused) &&
    (WinConfiguration->Editor.Editor == edExternal || !WinConfiguration->Editor.ExternalEditor.IsEmpty()))
  UPD(CurrentOpenAction, EnableFocusedOperation &&
    !WinConfiguration->DisableOpenEdit &&
    !DirView(osCurrent)->ItemIsDirectory(DirView(osCurrent)->ItemFocused))
  UPD(AddEditLinkAction, ScpExplorer->Terminal &&
    (DirView(osCurrent) != DirView(osRemote) ||
     (ScpExplorer->Terminal->IsCapable[fcResolveSymlink] &&
      ScpExplorer->Terminal->IsCapable[fcSymbolicLink])))
  // selected operaton
  UPD(CurrentCopyAction, EnableSelectedOperation)
  UPD(CurrentMoveAction, EnableSelectedOperation)
  UPD(CurrentDeleteAction, EnableSelectedOperation)
  UPD(CurrentPropertiesAction, EnableSelectedOperation)
  UPD(RemoteMoveToAction, EnableSelectedOperation && (DirView(osRemote) == DirView(osCurrent)))
  UPD(RemoteCopyToAction, EnableSelectedOperation && (DirView(osRemote) == DirView(osCurrent)))
  UPD(FileListToCommandLineAction, EnableSelectedOperation)
  UPD(FileListToClipboardAction, EnableSelectedOperation)
  UPD(FullFileListToClipboardAction, EnableSelectedOperation)
  UPD(UrlToClipboardAction, EnableSelectedOperation && (DirView(osRemote) == DirView(osCurrent)))
  // directory
  UPD(CurrentCreateDirAction, true)
  // selection
  UPD(SelectOneAction, DirView(osCurrent)->FilesCount)
  UPD(SelectAction, DirView(osCurrent)->FilesCount)
  UPD(UnselectAction, DirView(osCurrent)->SelCount)
  UPD(SelectAllAction, DirView(osCurrent)->FilesCount)
  UPD(InvertSelectionAction, DirView(osCurrent)->FilesCount)
  UPD(ClearSelectionAction, DirView(osCurrent)->SelCount)
  UPD(PasteAction, DirView(osCurrent)->CanPasteFromClipBoard())

  //style
  UPDACT(CurrentCycleStyleAction,
    CurrentCycleStyleAction->ImageIndex = 8 + (DirView(osCurrent)->ViewStyle + 1) % 4)
  #define STYLEACTION(Style) UPDACT(Current ## Style ## Action, \
    Current ## Style ## Action->Checked = (DirView(osCurrent)->ViewStyle == vs ## Style))
  STYLEACTION(Icon)
  STYLEACTION(SmallIcon)
  STYLEACTION(List)
  STYLEACTION(Report)
  #undef STYLEACTION

  // REMOTE+LOCAL
  // back/forward
  #define HISTORYACTION(SIDE, DIRECTION, HINTFMT, DELTA) \
    UPDEX(SIDE ## DIRECTION ## Action, (DirView(os ## SIDE)->DIRECTION ## Count > 0), \
    SIDE ## DIRECTION ## Action->Hint = FMTLOAD(HINTFMT, (DirView(os ## SIDE)->HistoryPath[DELTA])), \
    SIDE ## DIRECTION ## Action->Hint = "")
  HISTORYACTION(Local, Back, EXPLORER_BACK_HINT, -1)
  HISTORYACTION(Local, Forward, EXPLORER_FORWARD_HINT, 1)
  HISTORYACTION(Remote, Back, EXPLORER_BACK_HINT, -1)
  HISTORYACTION(Remote, Forward, EXPLORER_FORWARD_HINT, 1)
  #undef HISTORYACTION
  #define PANEL_ACTIONS(SIDE) \
    UPD(SIDE ## ParentDirAction, !DirView(os ## SIDE)->IsRoot) \
    UPD(SIDE ## RootDirAction, !DirView(os ## SIDE)->IsRoot) \
    UPD(SIDE ## HomeDirAction, true) \
    UPD(SIDE ## RefreshAction, DirView(os ## SIDE)->DirOK) \
    UPD(SIDE ## OpenDirAction, true) \
    UPD(SIDE ## ChangePathAction, true) \
    UPD(SIDE ## AddBookmarkAction, true) \
    UPD(SIDE ## PathToClipboardAction, true)
  PANEL_ACTIONS(Local)
  PANEL_ACTIONS(Remote)
  #undef PANEL_ACTIONS
  UPD(LocalExploreDirectoryAction, true)

  // HELP
  UPD(AboutAction, true)
  UPD(HomepageAction, true)
  UPD(HistoryPageAction, true)
  UPD(TableOfContentsAction, true)
  UPD(ForumPageAction, true)
  UPD(CheckForUpdatesAction, true)
  UPD(DonatePageAction, true)

  // VIEW
  UPDCOMP(StatusBar)
  UPDCOMP(ToolBar)
  UPDCOMP(LocalStatusBar)
  UPDCOMP(RemoteStatusBar)
  UPDCOMP(CommandLinePanel)
  UPDCOMP(RemoteTree)
  UPDCOMP(LocalTree)
  UPDCOMP(ExplorerMenuBand)
  UPDCOMP(ExplorerAddressBand)
  UPDCOMP(ExplorerToolbarBand)
  UPDCOMP(ExplorerSelectionBand)
  UPDCOMP(ExplorerSessionBand)
  UPDCOMP(ExplorerPreferencesBand)
  UPDCOMP(ExplorerSortBand)
  UPDCOMP(CommanderMenuBand)
  UPDCOMP(CommanderSessionBand)
  UPDCOMP(CommanderPreferencesBand)
  UPDCOMP(CommanderSelectionBand)
  UPDCOMP(CommanderToolbarBand)
  UPDCOMP(CommanderSortBand)
  UPDCOMP(CommanderCommandsBand)
  UPDCOMP(CommanderLocalHistoryBand)
  UPDCOMP(CommanderLocalNavigationBand)
  UPDCOMP(CommanderRemoteHistoryBand)
  UPDCOMP(CommanderRemoteNavigationBand)

  UPD(GoToCommandLineAction, true)
  UPD(GoToTreeAction, true)
  UPDEX(ViewLogAction, Configuration->Logging,
    ViewLogAction->Checked = (WinConfiguration->LogView == lvWindow),
    ViewLogAction->Checked = false )
  UPDEX(ShowHiddenFilesAction, true,
    ShowHiddenFilesAction->Checked = WinConfiguration->ShowHiddenFiles, )
  UPD(PreferencesAction, true)

  // SORT
  UPDSORTA(Local)
  UPDSORT(Local, dv, Name)
  UPDSORT(Local, dv, Ext)
  UPDSORT(Local, dv, Size)
  UPDSORT(Local, dv, Type)
  UPDSORT(Local, dv, Changed)
  UPDSORT(Local, dv, Attr)
  UPDSORTA(Remote)
  UPDSORT(Remote, uv, Name)
  UPDSORT(Remote, uv, Ext)
  UPDSORT(Remote, uv, Size)
  UPDSORT(Remote, uv, Changed)
  UPDSORT(Remote, uv, Rights)
  UPDSORT(Remote, uv, Owner)
  UPDSORT(Remote, uv, Group)
  UPDSORTA(Current)
  UPDSORTC(dv, Name, uv, Name)
  UPDSORTC(dv, Ext, uv, Ext)
  UPDSORTC(dv, Size, uv, Size)
  #define uvType uvName /* no type columns on remote panel */
  UPDSORTC(dv, Type, uv, Type)
  #undef uvType
  UPDSORTC(dv, Changed, uv, Changed)
  UPDSORTC(dv, Attr, uv, Rights)
  UPDSORTC(dv, Name, uv, Owner)
  UPDSORTC(dv, Name, uv, Group)
  #define COLVIEWPROPS ((TCustomDirViewColProperties*)(((TCustomDirView*)(((TListColumns*)(ListColumn->Collection))->Owner()))->ColProperties))
  UPDEX(SortColumnAscendingAction, (ListColumn != NULL), SortColumnAscendingAction->Checked =
    (COLVIEWPROPS->SortColumn == ListColumn->Index) && COLVIEWPROPS->SortAscending, /*assert(false)*/  )
  UPDEX(SortColumnDescendingAction, (ListColumn != NULL), SortColumnDescendingAction->Checked =
    (COLVIEWPROPS->SortColumn == ListColumn->Index) && !COLVIEWPROPS->SortAscending, /*assert(false)*/ )
  #undef COLVIEWPROPS

  // SHOW/HIDE COLUMN
  UPDSHCOL(Local, dv, Name)
  UPDSHCOL(Local, dv, Ext)
  UPDSHCOL(Local, dv, Size)
  UPDSHCOL(Local, dv, Type)
  UPDSHCOL(Local, dv, Changed)
  UPDSHCOL(Local, dv, Attr)
  UPDSHCOL(Remote, uv, Name)
  UPDSHCOL(Remote, uv, Ext)
  UPDSHCOL(Remote, uv, Size)
  UPDSHCOL(Remote, uv, Changed)
  UPDSHCOL(Remote, uv, Rights)
  UPDSHCOL(Remote, uv, Owner)
  UPDSHCOL(Remote, uv, Group)
  UPD(HideColumnAction, (ListColumn != NULL))

  // SESSION
  UPD(NewSessionAction, true)
  UPD(CloseSessionAction, true)
  UPD(SavedSessionsAction, (StoredSessions->Count > 0))
  UPD(OpenedSessionsAction, true)
  UPD(SaveCurrentSessionAction, true)

  // COMMAND
  UPD(CompareDirectoriesAction, true)
  UPD(SynchronizeAction, true)
  UPD(FullSynchronizeAction, true)
  UPD(ConsoleAction, true)
  UPD(PuttyAction, true)
  UPD(SynchronizeBrowsingAction, true)
  UPD(CloseApplicationAction, true)
  UPD(FileSystemInfoAction, true)
  UPD(ClearCachesAction, (ScpExplorer->Terminal != NULL) && !ScpExplorer->Terminal->AreCachesEmpty)
  UPD(EditNewAction, !WinConfiguration->DisableOpenEdit)

  // CUSTOM COMMANDS
  UPD(CustomCommandsAction,
    (ScpExplorer->DirView(osCurrent) == ScpExplorer->DirView(osRemote)))
  UPD(CustomCommandsCustomizeAction, true)

  // QUEUE
  #define UPDQUEUE(OPERATION) UPD(Queue ## OPERATION ## Action, \
    ScpExplorer->AllowQueueOperation(qo ## OPERATION))
  UPDQUEUE(GoTo)
  UPDQUEUE(Preferences)
  UPDEX(QueueItemQueryAction, ScpExplorer->AllowQueueOperation(qoItemQuery),
    ((TAction *)Action)->Visible = true, ((TAction *)Action)->Visible = false)
  UPDEX(QueueItemErrorAction, ScpExplorer->AllowQueueOperation(qoItemError),
    ((TAction *)Action)->Visible = true, ((TAction *)Action)->Visible = false)
  UPDEX(QueueItemPromptAction, ScpExplorer->AllowQueueOperation(qoItemPrompt),
    ((TAction *)Action)->Visible = true, ((TAction *)Action)->Visible = false)
  UPDQUEUE(ItemDelete)
  UPDQUEUE(ItemExecute)
  UPDQUEUE(ItemUp)
  UPDQUEUE(ItemDown)
  #undef UPDQUEUE
  UPDACT(QueueToggleShowAction,
    ((TAction *)Action)->Checked = ScpExplorer->ComponentVisible[fcQueueView])
  #define QUEUEACTION(SHOW) UPDACT(Queue ## SHOW ## Action, \
    ((TAction *)Action)->Checked = WinConfiguration->QueueView.Show == qv ## SHOW)
  QUEUEACTION(Show)
  QUEUEACTION(HideWhenEmpty)
  QUEUEACTION(Hide)
  #undef QUEUEACTION
  UPDCOMP(CommanderPreferencesBand)
  UPDACT(QueueToolbarAction,
    ((TAction *)Action)->Enabled = ScpExplorer->ComponentVisible[fcQueueView];
    ((TAction *)Action)->Checked = ScpExplorer->ComponentVisible[fcQueueToolbar]);
  ;
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::ExplorerActionsExecute(
      TBasicAction *Action, bool &Handled)
{
  assert(ScpExplorer);
  if (!ScpExplorer->AllowedAction((TAction *)Action, aaExecute))
  {
    Handled = true;
    return;
  }
  
  assert(FIdle);
  FIdle = false;
  try
  {
    // focused operation
    EXE(CurrentCopyFocusedAction, ScpExplorer->ExecuteFileOperation(foCopy, osCurrent, true))
    EXE(CurrentMoveFocusedAction, ScpExplorer->ExecuteFileOperation(foMove, osCurrent, true))
    EXE(CurrentDeleteFocusedAction, ScpExplorer->ExecuteFileOperation(foDelete, osCurrent, true))
    EXE(CurrentPropertiesFocusedAction, ScpExplorer->ExecuteFileOperation(foSetProperties, osCurrent, true))
    EXE(RemoteMoveToFocusedAction, ScpExplorer->ExecuteFileOperation(foRemoteMove, osCurrent, true))
    EXE(RemoteCopyToFocusedAction, ScpExplorer->ExecuteFileOperation(foRemoteCopy, osCurrent, true))
    // operation
    EXE(CurrentCopyAction, ScpExplorer->ExecuteFileOperation(foCopy, osCurrent, false))
    EXE(CurrentMoveAction, ScpExplorer->ExecuteFileOperation(foMove, osCurrent, false))
    EXE(CurrentEditAction, ScpExplorer->ExecuteFile(osCurrent, efEditor))
    EXE(CurrentEditAlternativeAction, ScpExplorer->ExecuteFile(osCurrent, efAlternativeEditor))
    EXE(CurrentOpenAction, ScpExplorer->ExecuteCurrentFile())
    EXE(AddEditLinkAction, ScpExplorer->AddEditLink())
    EXE(CurrentRenameAction, ScpExplorer->ExecuteFileOperation(foRename, osCurrent, false))
    EXE(CurrentDeleteAction, ScpExplorer->ExecuteFileOperation(foDelete, osCurrent, false))
    EXE(CurrentPropertiesAction, ScpExplorer->ExecuteFileOperation(foSetProperties, osCurrent, false))
    EXE(RemoteMoveToAction, ScpExplorer->ExecuteFileOperation(foRemoteMove, osCurrent, false))
    EXE(RemoteCopyToAction, ScpExplorer->ExecuteFileOperation(foRemoteCopy, osCurrent, false))
    EXE(FileListToCommandLineAction, ScpExplorer->PanelExport(osCurrent, peFileList, pedCommandLine))
    EXE(FileListToClipboardAction, ScpExplorer->PanelExport(osCurrent, peFileList, pedClipboard))
    EXE(FullFileListToClipboardAction, ScpExplorer->PanelExport(osCurrent, peFullFileList, pedClipboard))
    EXE(UrlToClipboardAction, ScpExplorer->PanelExport(osCurrent, peUrl, pedClipboard))
    // directory
    EXE(CurrentCreateDirAction, ScpExplorer->CreateDirectory(osCurrent))
    //selection
    EXE(SelectOneAction, DirView(osCurrent)->SelectCurrentItem(DirView(osCurrent)->NortonLike))
    EXE(SelectAction, DirView(osCurrent)->DoSelectByMask(true))
    EXE(UnselectAction, DirView(osCurrent)->DoSelectByMask(false))
    EXE(SelectAllAction, DirView(osCurrent)->SelectAll(smAll))
    EXE(InvertSelectionAction, DirView(osCurrent)->SelectAll(smInvert))
    EXE(ClearSelectionAction, DirView(osCurrent)->SelectAll(smNone))
    EXE(PasteAction, DirView(osCurrent)->PasteFromClipBoard())

    // style
    EXE(CurrentCycleStyleAction,
      if (DirView(osCurrent)->ViewStyle == vsReport) DirView(osCurrent)->ViewStyle = vsIcon;
        else DirView(osCurrent)->ViewStyle = (TViewStyle)(DirView(osCurrent)->ViewStyle + 1);
    )
    #define STYLEACTION(Style) EXE(Current ## Style ## Action, \
      DirView(osCurrent)->ViewStyle = vs ## Style)
    STYLEACTION(Icon)
    STYLEACTION(SmallIcon)
    STYLEACTION(List)
    STYLEACTION(Report)
    #undef STYLEACTION

    #define PANEL_ACTIONS(SIDE) \
      EXE(SIDE ## BackAction, DirView(os ## SIDE)->HistoryGo(-1)) \
      EXE(SIDE ## ForwardAction, DirView(os ## SIDE)->HistoryGo(1)) \
      EXE(SIDE ## ParentDirAction, DirView(os ## SIDE)->ExecuteParentDirectory()) \
      EXE(SIDE ## RootDirAction, DirView(os ## SIDE)->ExecuteRootDirectory()) \
      EXE(SIDE ## HomeDirAction, DirView(os ## SIDE)->ExecuteHomeDirectory()) \
      EXE(SIDE ## RefreshAction, DirView(os ## SIDE)->ReloadDirectory()) \
      EXE(SIDE ## OpenDirAction, ScpExplorer->OpenDirectory(os ## SIDE)) \
      EXE(SIDE ## ChangePathAction, ScpExplorer->ChangePath(os ## SIDE)) \
      EXE(SIDE ## AddBookmarkAction, ScpExplorer->AddBookmark(os ## SIDE)) \
      EXE(SIDE ## PathToClipboardAction, ScpExplorer->PanelExport(os ## SIDE, pePath, pedClipboard))
    PANEL_ACTIONS(Local)
    PANEL_ACTIONS(Remote)
    #undef PANEL_ACTIONS
    EXE(LocalExploreDirectoryAction, ScpExplorer->ExploreLocalDirectory())

    //HELP
    EXE(AboutAction, DoAboutDialog(Configuration))
    EXE(HomepageAction, OpenBrowser(LoadStr(HOMEPAGE_URL)))
    EXE(HistoryPageAction, OpenBrowser(LoadStr(HISTORY_URL)))
    EXE(TableOfContentsAction, Application->HelpSystem->ShowTableOfContents())
    EXE(ForumPageAction, OpenBrowser(LoadStr(FORUM_URL)))
    EXE(CheckForUpdatesAction, CheckForUpdates())
    EXE(DonatePageAction, OpenBrowser(LoadStr(DONATE_URL)))

    // VIEW
    EXECOMP(StatusBar)
    EXECOMP(ToolBar)
    EXECOMP(LocalStatusBar)
    EXECOMP(RemoteStatusBar)
    EXECOMP(ExplorerMenuBand)
    EXECOMP(ExplorerAddressBand)
    EXECOMP(ExplorerToolbarBand)
    EXECOMP(ExplorerSelectionBand)
    EXECOMP(ExplorerSessionBand)
    EXECOMP(ExplorerPreferencesBand)
    EXECOMP(ExplorerSortBand)
    EXECOMP(CommanderMenuBand)
    EXECOMP(CommanderSessionBand)
    EXECOMP(CommanderPreferencesBand)
    EXECOMP(CommanderSelectionBand)
    EXECOMP(CommanderToolbarBand)
    EXECOMP(CommanderSortBand)
    EXECOMP(CommanderCommandsBand)
    EXECOMP(CommanderLocalHistoryBand)
    EXECOMP(CommanderLocalNavigationBand)
    EXECOMP(CommanderRemoteHistoryBand)
    EXECOMP(CommanderRemoteNavigationBand)
    EXECOMP(CommandLinePanel)
    EXECOMP(RemoteTree)
    EXECOMP(LocalTree)
    EXE(GoToCommandLineAction, ScpExplorer->GoToCommandLine())
    EXE(GoToTreeAction, ScpExplorer->GoToTree())

    EXE(ViewLogAction, WinConfiguration->LogView =
      (WinConfiguration->LogView == lvNone ? lvWindow : lvNone) )
    EXE(ShowHiddenFilesAction, WinConfiguration->ShowHiddenFiles = !WinConfiguration->ShowHiddenFiles)
    EXE(PreferencesAction, DoPreferencesDialog(pmDefault) )

    #define COLVIEWPROPS ((TCustomDirViewColProperties*)(((TCustomDirView*)(((TListColumns*)(ListColumn->Collection))->Owner()))->ColProperties))
    // SORT
    EXESORTA(Local)
    EXESORT(Local, dv, Name)
    EXESORT(Local, dv, Ext)
    EXESORT(Local, dv, Size)
    EXESORT(Local, dv, Type)
    EXESORT(Local, dv, Changed)
    EXESORT(Local, dv, Attr)
    EXESORTA(Remote)
    EXESORT(Remote, uv, Name)
    EXESORT(Remote, uv, Ext)
    EXESORT(Remote, uv, Size)
    EXESORT(Remote, uv, Changed)
    EXESORT(Remote, uv, Rights)
    EXESORT(Remote, uv, Owner)
    EXESORT(Remote, uv, Group)
    EXESORTA(Current)
    EXESORTC(Name, dvName, uvName)
    EXESORTC(Ext, dvExt, uvExt)
    EXESORTC(Size, dvSize, uvSize)
    EXESORTC(Type, dvType, uvName)
    EXESORTC(Changed, dvChanged, uvChanged)
    EXESORTC(Rights, dvAttr, uvRights)
    EXESORTC(Owner, dvName, uvOwner)
    EXESORTC(Group, dvName, uvGroup)
    EXE(SortColumnAscendingAction, assert(ListColumn);
      COLVIEWPROPS->SortColumn = ListColumn->Index; COLVIEWPROPS->SortAscending = true; ListColumn = NULL )
    EXE(SortColumnDescendingAction, assert(ListColumn);
      COLVIEWPROPS->SortColumn = ListColumn->Index; COLVIEWPROPS->SortAscending = false; ListColumn = NULL )

    // SHOW/HIDE COLUMN
    EXESHCOL(Local, dv, Name)
    EXESHCOL(Local, dv, Ext)
    EXESHCOL(Local, dv, Size)
    EXESHCOL(Local, dv, Type)
    EXESHCOL(Local, dv, Changed)
    EXESHCOL(Local, dv, Attr)
    EXESHCOL(Remote, uv, Name)
    EXESHCOL(Remote, uv, Ext)
    EXESHCOL(Remote, uv, Size)
    EXESHCOL(Remote, uv, Changed)
    EXESHCOL(Remote, uv, Rights)
    EXESHCOL(Remote, uv, Owner)
    EXESHCOL(Remote, uv, Group)
    EXE(HideColumnAction, assert(ListColumn);
      COLVIEWPROPS->Visible[ListColumn->Index] = false; ListColumn = NULL )
    #undef COLVIEWPROPS

    // SESSION
    EXE(NewSessionAction, ScpExplorer->NewSession())
    EXE(CloseSessionAction, ScpExplorer->CloseSession())
    EXE(SavedSessionsAction, CreateSessionListMenu())
    EXE(OpenedSessionsAction, )
    EXE(SaveCurrentSessionAction, ScpExplorer->SaveCurrentSession())

    // COMMAND
    EXE(CompareDirectoriesAction, ScpExplorer->CompareDirectories())
    EXE(SynchronizeAction, ScpExplorer->SynchronizeDirectories())
    EXE(FullSynchronizeAction, ScpExplorer->FullSynchronizeDirectories())
    EXE(ConsoleAction, ScpExplorer->OpenConsole())
    EXE(PuttyAction, ScpExplorer->OpenInPutty())
    EXE(SynchronizeBrowsingAction, )
    EXE(CloseApplicationAction, ScpExplorer->Close())
    EXE(FileSystemInfoAction, DoFileSystemInfoDialog(ScpExplorer->Terminal))
    EXE(ClearCachesAction, ScpExplorer->Terminal->ClearCaches())
    EXE(EditNewAction, ScpExplorer->EditNew(osCurrent));

    // CUSTOM COMMANDS
    EXE(CustomCommandsAction, CreateCustomCommandsMenu(CustomCommandsAction))
    EXE(CustomCommandsCustomizeAction, DoPreferencesDialog(pmCustomCommands))

    // QUEUE
    #define EXEQUEUE(OPERATION) EXE(Queue ## OPERATION ## Action, \
      ScpExplorer->ExecuteQueueOperation(qo ## OPERATION))
    EXEQUEUE(GoTo)
    EXEQUEUE(Preferences)
    EXEQUEUE(ItemQuery)
    EXEQUEUE(ItemError)
    EXEQUEUE(ItemPrompt)
    EXEQUEUE(ItemDelete)
    EXEQUEUE(ItemExecute)
    EXEQUEUE(ItemUp)
    EXEQUEUE(ItemDown)
    #undef EXEQUEUE
    EXE(QueueToggleShowAction,
      TQueueViewConfiguration Config = WinConfiguration->QueueView;
      Config.Show = ScpExplorer->ComponentVisible[fcQueueView] ? qvHide : qvShow;
      WinConfiguration->QueueView = Config)
    #define QUEUEACTION(SHOW) EXE(Queue ## SHOW ## Action, \
      TQueueViewConfiguration Config = WinConfiguration->QueueView; \
      Config.Show = qv ## SHOW; WinConfiguration->QueueView = Config)
    QUEUEACTION(Show)
    QUEUEACTION(HideWhenEmpty)
    QUEUEACTION(Hide)
    #undef QUEUEACTION
    EXECOMP(QueueToolbar);
    ;
  }
  __finally
  {
    FIdle = true;
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
  CurrentCreateDirAction->ShortCut = ShortCut('D', CTRL);
  // File operation
  CurrentRenameAction->ShortCut = ShortCut(VK_F2, NONE);
  CurrentEditAction->ShortCut = ShortCut('E', CTRL);
  CurrentEditAlternativeAction->ShortCut = ShortCut('E', CTRLSHIFT);
  AddEditLinkAction->ShortCut = ShortCut('L', CTRLALT);
  // Focused operation
  CurrentCopyFocusedAction->ShortCut = ShortCut('C', CTRL);
  CurrentMoveFocusedAction->ShortCut = ShortCut('M', CTRL);
  CurrentDeleteFocusedAction->ShortCut = ShortCut(VK_DELETE, NONE);
  CurrentPropertiesFocusedAction->ShortCut = ShortCut(VK_RETURN, ALT);
  RemoteMoveToFocusedAction->ShortCut = ShortCut('M', CTRLALT);
  // remote directory
  RemoteOpenDirAction->ShortCut = ShortCut('O', CTRL);
  RemoteRefreshAction->ShortCut = ShortCut(VK_F5, NONE);
  RemoteHomeDirAction->ShortCut = ShortCut('H', CTRL);
  // selected operation
  CurrentCopyAction->ShortCut = CurrentCopyFocusedAction->ShortCut;
  CurrentMoveAction->ShortCut = CurrentMoveFocusedAction->ShortCut;
  CurrentDeleteAction->ShortCut = CurrentDeleteFocusedAction->ShortCut;
  CurrentPropertiesAction->ShortCut = CurrentPropertiesFocusedAction->ShortCut;
  RemoteMoveToAction->ShortCut = ShortCut('M', CTRLALT);
  // selection
  SelectAction->ShortCut = ShortCut(VK_ADD, NONE);
  UnselectAction->ShortCut = ShortCut(VK_SUBTRACT, NONE);
  SelectAllAction->ShortCut = ShortCut('A', CTRL);
  InvertSelectionAction->ShortCut = ShortCut(VK_MULTIPLY, NONE);
  ClearSelectionAction->ShortCut = ShortCut('L', CTRL);

  CloseApplicationAction->ShortCut = ShortCut(VK_F4, ALT);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CommanderShortcuts()
{
  // Directory
  CurrentCreateDirAction->ShortCut = ShortCut(VK_F7, NONE);
  // File operation
  CurrentRenameAction->ShortCut = ShortCut(VK_F2, NONE);
  CurrentEditAction->ShortCut = ShortCut(VK_F4, NONE);
  CurrentEditAlternativeAction->ShortCut = ShortCut(VK_F4, SHIFT);
  AddEditLinkAction->ShortCut = ShortCut(VK_F6, ALT);
  // Focused operation
  CurrentCopyFocusedAction->ShortCut = ShortCut(VK_F5, NONE);
  CurrentMoveFocusedAction->ShortCut = ShortCut(VK_F6, NONE);
  CurrentDeleteFocusedAction->ShortCut = ShortCut(VK_F8, NONE);
  CurrentPropertiesFocusedAction->ShortCut = ShortCut(VK_F9, NONE);
  RemoteMoveToFocusedAction->ShortCut = ShortCut(VK_F6, SHIFT);
  RemoteCopyToFocusedAction->ShortCut = ShortCut(VK_F5, SHIFT);
  // remote directory
  RemoteOpenDirAction->ShortCut = ShortCut('O', CTRL);
  RemoteRefreshAction->ShortCut = ShortCut('R', CTRL);
  RemoteHomeDirAction->ShortCut = ShortCut('H', CTRL);
  RemotePathToClipboardAction->ShortCut = ShortCut(VK_OEM_6 /* ] */, CTRL);
  // local directory
  LocalOpenDirAction->ShortCut = RemoteOpenDirAction->ShortCut;
  LocalRefreshAction->ShortCut = RemoteRefreshAction->ShortCut;
  LocalHomeDirAction->ShortCut = RemoteHomeDirAction->ShortCut;
  LocalPathToClipboardAction->ShortCut = ShortCut(VK_OEM_4 /* [ */, CTRL);
  // selected operation
  CurrentCopyAction->ShortCut = CurrentCopyFocusedAction->ShortCut;
  CurrentMoveAction->ShortCut = CurrentMoveFocusedAction->ShortCut;
  CurrentDeleteAction->ShortCut = CurrentDeleteFocusedAction->ShortCut;
  CurrentDeleteAction->SecondaryShortCuts->Clear();
  CurrentDeleteAction->SecondaryShortCuts->Add(ShortCutToText(ShortCut(VK_DELETE, NONE)));
  CurrentPropertiesAction->ShortCut = CurrentPropertiesFocusedAction->ShortCut;
  RemoteMoveToAction->ShortCut = ShortCut(VK_F6, SHIFT);
  RemoteCopyToAction->ShortCut = ShortCut(VK_F5, SHIFT);
  // selection
  SelectOneAction->ShortCut = VK_INSERT;
  SelectAction->ShortCut = ShortCut(VK_ADD, NONE);
  UnselectAction->ShortCut = ShortCut(VK_SUBTRACT, NONE);
  SelectAllAction->ShortCut = ShortCut('A', CTRL);
  InvertSelectionAction->ShortCut = ShortCut(VK_MULTIPLY, NONE);
  ClearSelectionAction->ShortCut = ShortCut('L', CTRL);

  CloseApplicationAction->ShortCut = ShortCut(VK_F10, NONE);
}
#undef CTRL
#undef ALT
#undef NONE
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
      assert(ScpExplorer);
      ScpExplorer->Idle(FIdle);
    }
    __finally
    {
      FSessionIdleTimerExecuting = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateCustomCommandsMenu(TAction * Action)
{
  assert(Action);
  TMenuItem * Menu = dynamic_cast<TMenuItem *>(Action->ActionComponent);
  if (Menu)
  {
    int PrevCount = Menu->Count;
    for (int Index = 0; Index < WinConfiguration->CustomCommands->Count; Index++)
    {
      AnsiString Description = WinConfiguration->CustomCommands->Names[Index];
      AnsiString Command = WinConfiguration->CustomCommands->Values[Description];

      TMenuItem * Item = new TMenuItem(Menu);
      Item->Caption = Description;
      Item->Tag = Index;
      Item->Enabled = ScpExplorer->EnableCustomCommand(Description);
      if (Menu == RemoteDirViewCustomCommandsMenu)
      {
        Item->Tag = Item->Tag | 0x0100;
      }
      Item->Hint = FMTLOAD(CUSTOM_COMMAND_HINT,
        (StringReplace(Description, "&", "", TReplaceFlags() << rfReplaceAll)));
      Item->OnClick = CustomCommandClick;
      Menu->Add(Item);
    }

    TMenuItem * Item;
    if (WinConfiguration->CustomCommands->Count)
    {
      Item = new TMenuItem(Menu);
      Item->Caption = "-";
      Item->Hint = "E";
      Menu->Add(Item);
    }

    Item = new TMenuItem(Menu);
    Item->Action = CustomCommandsCustomizeAction;
    Menu->Add(Item);

    for (int Index = 0; Index < PrevCount; Index++)
    {
      Menu->Delete(0);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CustomCommandClick(TObject * Sender)
{
  TMenuItem * Item = dynamic_cast<TMenuItem *>(Sender);
  assert(Item);
  AnsiString CommandName;
  CommandName = WinConfiguration->CustomCommands->Names[Item->Tag & 0x00FF];
  ScpExplorer->ExecuteFileOperation(foCustomCommand, osRemote,
    (Item->Tag & 0xFF00) != 0, false, &CommandName);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateSessionListMenu()
{
  int PrevCount = SavedSessionsMenu->Count;
  StoredSessions->Load();
  for (int Index = 0; Index < StoredSessions->Count; Index++)
  {
    TSessionData * Data = StoredSessions->Sessions[Index];
    TMenuItem * Item = new TMenuItem(SavedSessionsMenu);
    Item->Caption = Data->Name;
    Item->Tag = Index;
    Item->Hint = FMTLOAD(SAVEDSESSION_HINT, (Data->Name));
    Item->OnClick = SessionItemClick;
    SavedSessionsMenu->Add(Item);
  }
  for (int Index = 0; Index < PrevCount; Index++)
  {
    SavedSessionsMenu->Delete(0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::SessionItemClick(TObject * Sender)
{
  assert(StoredSessions && (((TMenuItem *)Sender)->Tag < StoredSessions->Count));
  ScpExplorer->OpenStoredSession(StoredSessions->Sessions[((TMenuItem *)Sender)->Tag]);
}
//---------------------------------------------------------------------------
TShortCut __fastcall TNonVisualDataModule::OpenSessionShortCut(int Index)
{
  if (Index >= 0 && Index < 10)
  {
    return ShortCut((Word)(Index < 9 ? '0' + 1 + Index : '0'),
      TShiftState() << ssAlt);
  }
  else
  {
    return scNone;
  }
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::CreateOpenedSessionListMenu()
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  TStrings * TerminalList = Manager->TerminalList;
  int PrevCount = OpenedSessionsMenu->Count;
  for (int Index = 0; Index < TerminalList->Count; Index++)
  {
    TTerminal * Terminal = dynamic_cast<TTerminal *>(TerminalList->Objects[Index]);
    assert(Terminal);
    TMenuItem * Item = new TMenuItem(this);
    Item->Caption = TerminalList->Strings[Index];
    Item->Tag = int(Terminal);
    Item->Hint = FMTLOAD(OPENEDSESSION_HINT, (Item->Caption));
    Item->Checked = (Manager->ActiveTerminal == Terminal);
    Item->ShortCut = OpenSessionShortCut(Index);
    Item->OnClick = OpenedSessionItemClick;
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
  TTerminalManager::Instance()->ActiveTerminal = (TTerminal*)(((TMenuItem *)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TNonVisualDataModule::QueuePopupPopup(TObject * /*Sender*/)
{
  TAction * Action = NULL;
  bool HideFirstLine = true;

  switch (ScpExplorer->DefaultQueueOperation())
  {
    case qoItemQuery:
      Action = QueueItemQueryAction;
      HideFirstLine = false;
      break;

    case qoItemError:
      Action = QueueItemErrorAction;
      HideFirstLine = false;
      break;

    case qoItemPrompt:
      Action = QueueItemPromptAction;
      HideFirstLine = false;
      break;

    case qoItemExecute:
      Action = QueueItemExecuteAction;
      break;
  }

  TMenuItem * Item;
  for (int Index = 0; Index < QueuePopup->Items->Count; Index++)
  {
    Item = QueuePopup->Items->Items[Index];
    Item->Default = (Action != NULL) && (Item->Action == Action);
    if (Item->Caption == "-")
    {
      Item->Visible = !HideFirstLine;
      HideFirstLine = false;
    }
  }
}
//---------------------------------------------------------------------------



