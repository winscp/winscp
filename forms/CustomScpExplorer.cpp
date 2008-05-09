//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "CustomScpExplorer.h"

#include <Interface.h>
#include <Exceptions.h>
#include <CoreMain.h>
#include <FileSystems.h>
#include <TextsCore.h>
#include <TextsWin.h>
#include <HelpWin.h>

#include <VCLCommon.h>
#include <Log.h>
#include <Progress.h>
#include <SynchronizeProgress.h>

#include <DragExt.h>

#include "GUITools.h"
#include "NonVisual.h"
#include "Glyphs.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "EditorManager.h"
#include "Setup.h"
#include <Consts.hpp>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CustomDirView"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "CustomDriveView"
#pragma link "UnixDriveView"
#pragma link "CustomDriveView"
#pragma link "TB2Dock"
#pragma link "TBXStatusBars"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
#define SAVE_SELECTION(DIRVIEW) \
  AnsiString FocusFile = ""; \
  AnsiString LastFocusedFile = ""; \
  if (DIRVIEW->ItemFocused) LastFocusedFile = DIRVIEW->ItemFocused->Caption; \
  { TListItem * ClosestUnselected = DIRVIEW->ClosestUnselected(DIRVIEW->ItemFocused); \
  if (ClosestUnselected) FocusFile = ClosestUnselected->Caption; }
#define RESTORE_SELECTION(DIRVIEW) \
  if (!LastFocusedFile.IsEmpty() && \
      (!DIRVIEW->ItemFocused || (DIRVIEW->ItemFocused->Caption != LastFocusedFile))) \
  { \
    TListItem *ItemToSelect = DIRVIEW->FindFileItem(FocusFile); \
    if (ItemToSelect) \
    { \
      DIRVIEW->ItemFocused = ItemToSelect; \
      DIRVIEW->ItemFocused->MakeVisible(False); \
    } \
  }
//---------------------------------------------------------------------------
#define WM_COMPONENT_HIDE (WM_WINSCP_USER + 4)
//---------------------------------------------------------------------------
class TMutexGuard
{
public:
  TMutexGuard(HANDLE AMutex, int Message = MUTEX_RELEASE_TIMEOUT,
    int Timeout = 5000)
  {
    FMutex = NULL;
    unsigned long WaitResult = WaitForSingleObject(AMutex, Timeout);
    if (WaitResult == WAIT_TIMEOUT)
    {
      throw Exception(LoadStr(MUTEX_RELEASE_TIMEOUT));
    }
    else
    {
      FMutex = AMutex;
    }
  }

  ~TMutexGuard()
  {
    if (FMutex != NULL)
    {
      ReleaseMutex(FMutex);
    }
  }

private:
  HANDLE FMutex;
};
//---------------------------------------------------------------------------
struct TTransferOperationParam
{
  TTransferOperationParam();

  AnsiString TargetDirectory;
  bool Temp;
  bool DragDrop;
};
//---------------------------------------------------------------------------
TTransferOperationParam::TTransferOperationParam()
{
  Temp = false;
  DragDrop = false;
}
//---------------------------------------------------------------------------
TCustomCommandParam::TCustomCommandParam()
{
  Params = 0;
}
//---------------------------------------------------------------------------
class TTransferPresetNoteData : public TObject
{
public:
  AnsiString Message;
  TStrings * More;

  virtual __fastcall ~TTransferPresetNoteData()
  {
    delete More;
  }
};
//---------------------------------------------------------------------------
static const int OutPos = -32000;
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::TCustomScpExplorerForm(TComponent* Owner):
    FFormRestored(False), TForm(Owner)
{
  FCurrentSide = osRemote;
  FDocks = new TList();
  RestoreParams();
  ConfigurationChanged();
  RemoteDirView->Invalidate();
  assert(NonVisualDataModule && !NonVisualDataModule->ScpExplorer);
  NonVisualDataModule->ScpExplorer = this;
  Application->OnMinimize = ApplicationMinimize;
  Application->OnRestore = ApplicationRestore;
  FAutoOperation = false;
  FForceExecution = false;
  FShowStatusBarHint = false;
  FIgnoreNextSysCommand = false;
  FErrorList = NULL;
  FSynchronizeProgressForm = NULL;
  FProgressForm = NULL;
  FRefreshLocalDirectory = false;
  FRefreshRemoteDirectory = false;
  FDDMoveSlipped = false;
  FDDExtMapFile = NULL;
  FDDExtMutex = CreateMutex(NULL, false, DRAG_EXT_MUTEX);
  assert(FDDExtMutex != NULL);
  FDDTargetControl = NULL;
  FDelayedDeletionTimer = NULL;
  FDelayedDeletionList = new TStringList();
  FDDFileList = NULL;
  FPendingTempSpaceWarn = false;
  FCapturedLog = NULL;
  FDragDropOperation = false;
  memset(&FHistoryMenu, 0, sizeof(FHistoryMenu));
  FNoTransferPresetAutoSelect = false;
  FCopyParamDefault = "";
  FSynchronizeController = NULL;
  FPendingQueueActionItem = NULL;
  FLockLevel = 0;
  FAlternativeDelete = false;
  FTrayIcon = new TTrayIcon(0);
  FTrayIcon->OnClick = TrayIconClick;

  FEditorManager = new TEditorManager();
  FEditorManager->OnFileChange = ExecutedFileChanged;
  FEditorManager->OnFileReload = ExecutedFileReload;
  FEditorManager->OnFileEarlyClosed = ExecutedFileEarlyClosed;

  FQueueStatus = NULL;
  FQueueStatusSection = new TCriticalSection();
  FQueueStatusInvalidated = false;
  FQueueItemInvalidated = false;
  FQueueActedItem = NULL;
  FQueueController = new TQueueController(QueueView2);

  FUserActionTimer = new TTimer(this);
  FUserActionTimer->Enabled = false;
  FUserActionTimer->Interval = 10;
  FUserActionTimer->OnTimer = UserActionTimer;

  FNotes = new TStringList();
  FNoteTimer = new TTimer(this);
  FNoteTimer->Enabled = false;
  FNoteTimer->OnTimer = NoteTimer;
  FOnNoteClick = NULL;

  FOle32Library = LoadLibrary("Ole32.dll");
  FDragMoveCursor = FOle32Library != NULL ?
    LoadCursor(FOle32Library, MAKEINTRESOURCE(2)) : NULL;

  UseSystemSettings(this);

  TTBXComboBoxItem * SessionCombo = dynamic_cast<TTBXComboBoxItem*>(
    static_cast<TObject*>(GetComponent(fcSessionCombo)));
  if (SessionCombo != NULL)
  {
    SessionCombo->OnDrawItem = SessionComboDrawItem;
    SessionCombo->OnPopup = SessionComboPopup;
    SessionCombo->OnChange = SessionComboChange;
    SessionCombo->Hint = NonVisualDataModule->OpenedSessionsAction->Hint;
  }

  TTBXStringList * TransferList = dynamic_cast<TTBXStringList*>(
    static_cast<TObject*>(GetComponent(fcTransferList)));
  assert(TransferList != NULL);
  FTransferListHoverIndex = -1;
  TransferList->OnChange = TransferListChange;
  TransferList->OnDrawItem = TransferListDrawItem;

  class TTBXPublicItem : public TTBXCustomItem
  {
  public:
    __property ItemStyle;
  };
  TTBXPublicItem * ColorMenu = reinterpret_cast<TTBXPublicItem *>(GetComponent(fcColorMenu));
  ColorMenu->ItemStyle = ColorMenu->ItemStyle << tbisSubmenu;

  RemoteDirView->Font = Screen->IconFont;

  reinterpret_cast<TLabel*>(QueueSplitter)->OnDblClick = QueueSplitterDblClick;

  TSHFileInfo FileInfo;
  FSystemImageList = new TImageList(this);
  int ImageListHandle = SHGetFileInfo("", 0, &FileInfo, sizeof(FileInfo),
    SHGFI_SYSICONINDEX | SHGFI_SMALLICON);
  if (ImageListHandle != 0)
  {
    FSystemImageList->ShareImages = true;
    FSystemImageList->Handle = ImageListHandle;
    FSystemImageList->DrawingStyle = dsTransparent;
  }

  Application->HookMainWindow(MainWindowHook);

  StartUpdates();
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
  Application->UnhookMainWindow(MainWindowHook);

  delete FSystemImageList;
  FSystemImageList = NULL;

  Application->OnHint = NULL;
  Application->OnMinimize = NULL;
  Application->OnRestore = NULL;
  delete FTrayIcon;
  FTrayIcon = NULL;

  FEditorManager->CloseInternalEditors(ForceCloseInternalEditor);
  delete FEditorManager;

  if (FDelayedDeletionTimer)
  {
    DoDelayedDeletion(NULL);
    SAFE_DESTROY(FDelayedDeletionTimer);
  }
  SAFE_DESTROY(FDelayedDeletionList);

  assert(FSynchronizeController == NULL);

  CloseHandle(FDDExtMutex);
  FDDExtMutex = NULL;

  FreeLibrary(FOle32Library);
  FOle32Library = NULL;
  FDragMoveCursor = NULL;

  assert(!FErrorList);
  StoreParams();
  Terminal = NULL;
  Queue = NULL;
  assert(NonVisualDataModule && (NonVisualDataModule->ScpExplorer == this));
  NonVisualDataModule->ScpExplorer = NULL;

  delete FQueueController;
  FQueueController = NULL;
  delete FQueueStatusSection;
  FQueueStatusSection = NULL;
  delete FQueueStatus;
  FQueueStatus = NULL;

  delete FUserActionTimer;
  FUserActionTimer = NULL;

  delete FNoteTimer;
  delete FNotes;
  delete FNoteData;

  SAFE_DESTROY(FDocks);

  SAFE_DESTROY(FHistoryMenu[0][0]);
  SAFE_DESTROY(FHistoryMenu[0][1]);
  SAFE_DESTROY(FHistoryMenu[1][0]);
  SAFE_DESTROY(FHistoryMenu[1][1]);

  StopUpdateThread();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTerminal(TTerminal * value)
{
  if (FTerminal != value)
  {
    TerminalChanging();
    FTerminal = value;
    bool PrevNoTransferPresetAutoSelect = FNoTransferPresetAutoSelect;
    FNoTransferPresetAutoSelect = true;
    try
    {
      TerminalChanged();
    }
    __finally
    {
      FNoTransferPresetAutoSelect = PrevNoTransferPresetAutoSelect;
    }

    if (Terminal != NULL)
    {
      TransferPresetAutoSelect();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalChanging()
{
  if (FTerminal != NULL)
  {
    UpdateTerminal(Terminal);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalChanged()
{
  RemoteDirView->Terminal = Terminal;
  Caption = Application->Title;
  NonVisualDataModule->QueueDisconnectOnceEmptyAction->Checked = false;
  if (Terminal)
  {
    if (Terminal->Active)
    {
      Terminal->RefreshDirectory();
    }

    TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
    assert(ManagedTerminal != NULL);

    if (WinConfiguration->PreservePanelState &&
        (ManagedTerminal->RemoteExplorerState != NULL))
    {
      RemoteDirView->RestoreState(ManagedTerminal->RemoteExplorerState);
    }

    SessionColor = ManagedTerminal->Color;

    InitStatusBar();
  }
  TerminalListChanged(NULL);
  UpdateTransferList();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetQueue(TTerminalQueue * value)
{
  if (Queue != value)
  {
    if (FQueue != NULL)
    {
      FQueue->OnListUpdate = NULL;
      FQueue->OnQueueItemUpdate = NULL;
    }
    FQueue = value;
    if (FQueue != NULL)
    {
      assert(FQueue->OnListUpdate == NULL);
      FQueue->OnListUpdate = QueueListUpdate;
      assert(FQueue->OnQueueItemUpdate == NULL);
      FQueue->OnQueueItemUpdate = QueueItemUpdate;
    }
    QueueChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2Deletion(TObject * /*Sender*/,
  TListItem * Item)
{
  if (FQueueActedItem == Item)
  {
    FQueueActedItem = NULL;
    if ((QueueView2->PopupMenu != NULL) &&
        (QueueView2->PopupMenu->PopupComponent == QueueView2))
    {
      // rather "trick", suggested by Jordan on jrsoftware.toolbar2000
      ReleaseCapture();
    }
  }

  if (Item->Data == FPendingQueueActionItem)
  {
    FPendingQueueActionItem = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueStatus(bool AppIdle)
{
  {
    TGuard Guard(FQueueStatusSection);

    FQueueStatusInvalidated = false;

    if (FQueue != NULL)
    {
      FQueueStatus = FQueue->CreateStatus(FQueueStatus);
    }
  }

  FQueueController->UpdateQueueStatus(FQueueStatus);

  UpdateQueueView();

  bool IsEmpty = (FQueueStatus == NULL) || (FQueueStatus->Count == 0);

  if (NonVisualDataModule->QueueDisconnectOnceEmptyAction->Checked &&
      IsEmpty && (Terminal != NULL))
  {
    NonVisualDataModule->QueueDisconnectOnceEmptyAction->Checked = false;
    if (AppIdle)
    {
      Terminal->CloseOnCompletion(LoadStr(CLOSED_ON_QUEUE_EMPTY));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueView()
{
  ComponentVisible[fcQueueView] =
    (WinConfiguration->QueueView.Show == qvShow) ||
    ((WinConfiguration->QueueView.Show == qvHideWhenEmpty) &&
     (FQueueStatus != NULL) && (FQueueStatus->Count > 0));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueChanged()
{
  if (FQueueStatus != NULL)
  {
    delete FQueueStatus;
    FQueueStatus = NULL;
  }
  UpdateQueueStatus(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueListUpdate(TTerminalQueue * Queue)
{
  if (FQueue == Queue)
  {
    FQueueStatusInvalidated = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueItemUpdate(TTerminalQueue * Queue,
  TQueueItem * Item)
{
  if (FQueue == Queue)
  {
    TGuard Guard(FQueueStatusSection);

    assert(FQueueStatus != NULL);

    TQueueItemProxy * QueueItem = FQueueStatus->FindByQueueItem(Item);

    if ((Item->Status == TQueueItem::qsDone) && (Terminal != NULL))
    {
      FRefreshLocalDirectory = (QueueItem == NULL) ||
        (!QueueItem->Info->ModifiedLocal.IsEmpty());
      FRefreshRemoteDirectory = (QueueItem == NULL) ||
        (!QueueItem->Info->ModifiedRemote.IsEmpty());
    }

    if (QueueItem != NULL)
    {
      QueueItem->UserData = (void*)true;
      FQueueItemInvalidated = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RefreshQueueItems(bool AppIdle)
{
  if (FQueueStatus != NULL)
  {
    bool QueueAutoPopup = GUIConfiguration->QueueAutoPopup;
    bool Refresh = FQueueItemInvalidated;
    FQueueItemInvalidated = false;

    int Limit = Refresh ? FQueueStatus->Count : FQueueStatus->ActiveCount;

    bool Updated = false;
    bool Update;
    TQueueItemProxy * QueueItem;
    bool WasUserAction;
    for (int Index = 0; Index < Limit; Index++)
    {
      Update = false;
      QueueItem = FQueueStatus->Items[Index];
      WasUserAction = TQueueItem::IsUserActionStatus(QueueItem->Status);
      if (AppIdle && QueueAutoPopup && WasUserAction &&
          (FPendingQueueActionItem == NULL))
      {
        FPendingQueueActionItem = QueueItem;
        FUserActionTimer->Enabled = true;
      }

      if ((bool)QueueItem->UserData)
      {
        QueueItem->UserData = (void*)false;
        QueueItem->Update();
        Updated = true;
        Update = true;
      }
      else if (FQueueController->QueueItemNeedsFrequentRefresh(QueueItem))
      {
        Update = true;
      }

      if (Update)
      {
        FQueueController->RefreshQueueItem(QueueItem);
      }
    }

    if (Updated)
    {
      NonVisualDataModule->UpdateNonVisibleActions();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTransferList()
{
  TTBXStringList * TransferList = dynamic_cast<TTBXStringList*>(
    static_cast<TComponent*>(GetComponent(fcTransferList)));
  TTBXDropDownItem * TransferDropDown = dynamic_cast<TTBXDropDownItem*>(
    static_cast<TComponent*>(GetComponent(fcTransferDropDown)));
  TransferList->Strings->BeginUpdate();
  try
  {
    TransferList->Strings->Assign(GUIConfiguration->CopyParamList->NameList);
    TransferList->Strings->Insert(0, StripHotkey(LoadStr(COPY_PARAM_DEFAULT)));
    TransferList->ItemIndex = GUIConfiguration->CopyParamIndex + 1;
    if (FTransferDropDownHint.IsEmpty())
    {
      FTransferDropDownHint = TransferDropDown->Hint;
    }
    // this way we get name for "default" settings (COPY_PARAM_DEFAULT)
    AnsiString Name = TransferList->Strings->Strings[TransferList->ItemIndex];
    TransferDropDown->Text = Name;
    TransferDropDown->Hint = FORMAT("%s\n \n%s:\n%s|%s",
      (FTransferDropDownHint, Name,
       GUIConfiguration->CurrentCopyParam.GetInfoStr("; ",
         FLAGMASK(Terminal != NULL, Terminal->UsableCopyParamAttrs(0).General)),
       FTransferDropDownHint));
    // update the label, otherwise when it is updated only on the first draw
    // of the list, it is drawn "bold" for some reason
    FTransferListHoverIndex = TransferList->ItemIndex;
    UpdateTransferLabel();
  }
  __finally
  {
    TransferList->Strings->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateCustomCommandsToolbar()
{
  TTBXToolbar * Toolbar = dynamic_cast<TTBXToolbar *>(GetComponent(fcCustomCommandsBand));
  assert(Toolbar != NULL);

  NonVisualDataModule->UpdateCustomCommandsToolbar(Toolbar);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateActions()
{
  TForm::UpdateActions();

  if (ComponentVisible[fcCustomCommandsBand])
  {
    UpdateCustomCommandsToolbar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ConfigurationChanged()
{
  assert(Configuration && RemoteDirView);
  RemoteDirView->DDAllowMove = WinConfiguration->DDAllowMoveInit;
  RemoteDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  RemoteDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  RemoteDirView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;

  RemoteDriveView->DDAllowMove = WinConfiguration->DDAllowMoveInit;
  RemoteDriveView->DimmHiddenDirs = WinConfiguration->DimmHiddenFiles;
  RemoteDriveView->ShowHiddenDirs = WinConfiguration->ShowHiddenFiles;
  RemoteDriveView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;

  SetDockAllowDrag(!WinConfiguration->LockToolbars);

  UpdateTransferList();

  if (Terminal != NULL)
  {
    TransferPresetAutoSelect();
  }

  UpdateQueueView();
  UpdateControls();

  // this can be called even before constuctor finishes.
  // can we be sure that the FEditorManager is NULL then?
  if (FEditorManager != NULL)
  {
    FEditorManager->ProcessFiles(FileConfigurationChanged, NULL);
  }

  if (ComponentVisible[fcCustomCommandsBand])
  {
    UpdateCustomCommandsToolbar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileConfigurationChanged(
  const AnsiString FileName, TEditedFileData & /*Data*/, TObject * Token,
  void * /*Arg*/)
{
  if (Token != NULL)
  {
    TForm * Editor = dynamic_cast<TForm*>(Token);
    assert(Editor != NULL);
    ReconfigureEditorForm(Editor);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CopyParamDialog(
  TTransferDirection Direction, TTransferType Type, bool Temp,
  TStrings * FileList, AnsiString & TargetDirectory, TGUICopyParamType & CopyParam,
  bool Confirm, bool DragDrop)
{
  bool Result = true;
  assert(Terminal && Terminal->Active);
  // Temp means d&d here so far, may change in future!
  if (Temp && (Direction == tdToLocal) && (Type == ttMove) &&
      !WinConfiguration->DDAllowMove)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    int Answer = MessageDialog(LoadStr(DND_DOWNLOAD_MOVE_WARNING), qtWarning,
      qaOK | qaCancel, HELP_DND_DOWNLOAD_MOVE_WARNING, &Params);
    if (Answer == qaNeverAskAgain)
    {
      WinConfiguration->DDAllowMove = true;
    }
    else if (Answer == qaCancel)
    {
      Result = false;
    }
  }

  // these parameters are known in advance
  int Params =
    FLAGMASK(Type == ttMove, cpDelete);
  bool ToTemp = (Temp && (Direction == tdToLocal));
  if (Result && Confirm && WinConfiguration->ConfirmTransferring)
  {
    bool DisableNewerOnly =
      (!Terminal->IsCapable[fcNewerOnlyUpload] && (Direction == tdToRemote)) ||
      ToTemp;
    int Options =
      FLAGMASK(DisableNewerOnly, coDisableNewerOnly) |
      FLAGMASK(ToTemp, coTemp) |
      coDoNotShowAgain;
    TUsableCopyParamAttrs UsableCopyParamAttrs = Terminal->UsableCopyParamAttrs(Params);
    int CopyParamAttrs = (Direction == tdToRemote ?
      UsableCopyParamAttrs.Upload : UsableCopyParamAttrs.Download);
    int OutputOptions = 0;
    Result = DoCopyDialog(Direction == tdToRemote, Type == ttMove,
      FileList, TargetDirectory, &CopyParam, Options, CopyParamAttrs, &OutputOptions);

    if (Result && FLAGSET(OutputOptions, cooDoNotShowAgain))
    {
      if (DragDrop)
      {
        WinConfiguration->DDTransferConfirmation = false;
      }
      else
      {
        WinConfiguration->ConfirmTransferring = false;
      }
    }
  }

  if (Result && CopyParam.Queue && !ToTemp)
  {
    assert(Queue != NULL);

    // these parameter are known only after transfer dialog
    Params |=
      (CopyParam.QueueNoConfirmation ? cpNoConfirmation : 0) |
      (CopyParam.NewerOnly ? cpNewerOnly : 0);
    TQueueItem * QueueItem;
    if (Direction == tdToRemote)
    {
      QueueItem = new TUploadQueueItem(Terminal, FileList, TargetDirectory,
        &CopyParam, Params);
    }
    else
    {
      QueueItem = new TDownloadQueueItem(Terminal, FileList, TargetDirectory,
        &CopyParam, Params);
    }
    Queue->AddItem(QueueItem);
    Result = false;

    TOperationSide Side = ((Direction == tdToRemote) ? osLocal : osRemote);
    if (HasDirView[Side])
    {
      DirView(Side)->SelectAll(smNone);
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreFormParams()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreParams()
{
  assert(FDocks != NULL);
  for (int Index = 0; Index < ComponentCount; Index++)
  {
    TTBDock * Dock = dynamic_cast<TTBDock *>(Components[Index]);
    if ((Dock != NULL) && (Dock->Tag == 0))
    {
      FDocks->Add(Dock);
    }
  }

  // IDE often looses this link
  RemoteDirView->HeaderImages = GlyphsModule->ArrowImages;

  ConfigurationChanged();

  QueuePanel->Height = WinConfiguration->QueueView.Height;
  LoadListViewStr(QueueView2, WinConfiguration->QueueView.Layout);
  QueueDock->Visible = WinConfiguration->QueueView.ToolBar;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreParams()
{
  WinConfiguration->QueueView.Height = QueuePanel->Height;
  WinConfiguration->QueueView.Layout = GetListViewStr(QueueView2);
  WinConfiguration->QueueView.ToolBar = QueueDock->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = true;
    RestoreFormParams();
  }
  TForm::CreateParams(Params);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetDockAllowDrag(bool value)
{
  assert(FDocks != NULL);
  for (int Index = 0; Index < FDocks->Count; Index++)
  {
    static_cast<TTBDock*>(FDocks->Items[Index])->AllowDrag = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LoadToolbarsLayoutStr(AnsiString LayoutStr)
{
  SetDockAllowDrag(true);
  ::LoadToolbarsLayoutStr(this, LayoutStr);
  SetDockAllowDrag(!WinConfiguration->LockToolbars);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomScpExplorerForm::GetToolbarsLayoutStr()
{
  AnsiString Result;
  SetDockAllowDrag(true);
  Result = ::GetToolbarsLayoutStr(this);
  SetDockAllowDrag(!WinConfiguration->LockToolbars);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & /*Cancel*/)
{
  // Download to temporary local directory
  if (FPendingTempSpaceWarn && ProgressData.InProgress && ProgressData.TotalSizeSet)
  {
    bool Continue = true;
    FPendingTempSpaceWarn = false;
    DoWarnLackOfTempSpace(ProgressData.Directory, ProgressData.TotalSize, Continue);
    if (!Continue)
    {
      Abort();
    }
  }

  // operation is being executed and we still didn't show up progress form
  if (ProgressData.InProgress && !FProgressForm)
  {
    FProgressForm = new TProgressForm(Application);
    FProgressForm->DeleteToRecycleBin = ((ProgressData.Side == osLocal ?
      WinConfiguration->DeleteToRecycleBin :
      Terminal->SessionData->DeleteToRecycleBin) != FAlternativeDelete);
    // When main window is hidden, synchronisation form does not exist,
    // we suppose "/upload" or URL download mode
    if (!Visible && (ProgressData.Operation != foCalculateSize) &&
        (ProgressData.Operation != foCalculateChecksum) &&
        (FSynchronizeProgressForm == NULL))
    {
      FProgressForm->DisconnectWhenComplete = true;
    }
  }
  // operation is finished (or terminated), so we hide progress form
  else if (!ProgressData.InProgress && FProgressForm)
  {
    SAFE_DESTROY(FProgressForm);

    if ((ProgressData.Operation != foCalculateSize) &&
        (ProgressData.Cancel == csContinue) &&
        !FAutoOperation)
    {
      OperationComplete(ProgressData.StartTime);
    }
  }

  if (FProgressForm)
  {
    FProgressForm->SetProgressData(ProgressData);
    if (FProgressForm->Cancel > ProgressData.Cancel)
    {
      ProgressData.Cancel = FProgressForm->Cancel;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationComplete(
  const TDateTime & StartTime)
{
  if (GUIConfiguration->BeepOnFinish &&
      (Now() - StartTime > GUIConfiguration->BeepOnFinishAfter))
  {
    MessageBeep(MB_OK);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & Cancel)
{
  FileOperationProgress(ProgressData, Cancel);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::PanelOperation(TOperationSide /*Side*/,
  bool DragDrop)
{
  return (!DragDrop && (DropSourceControl == NULL)) ||
    (DropSourceControl == RemoteDirView);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool /*Temp*/, const AnsiString FileName, bool Success,
  bool & DisconnectWhenComplete)
{
  if (!FAutoOperation)
  {
    // no selection on "/upload", form servers only as event handler
    // (it is not displayed)
    if (PanelOperation(Side, FDragDropOperation) &&
        Visible && (Operation != foCalculateSize) &&
        (Operation != foGetProperties) && (Operation != foCalculateChecksum))
    {
      TCustomDirView * DView = DirView(Side);
      AnsiString FileNameOnly = ExtractFileName(FileName, (Side == osRemote));
      TListItem *Item = DView->FindFileItem(FileNameOnly);
      // this can happen when local drive is unplugged in the middle of the operation
      if (Item != NULL)
      {
        if (Success) Item->Selected = false;
        if (DView->ViewStyle == vsReport)
        {
          TRect DisplayRect = Item->DisplayRect(drBounds);
          if (DisplayRect.Bottom > DView->ClientHeight)
          {
            DView->Scroll(0, Item->Top - DView->TopItem->Top);
          }
        }
        Item->MakeVisible(false);
      }
    }
  }

  if (Success && (FSynchronizeController != NULL))
  {
    if (Operation == foCopy)
    {
      assert(Side == osLocal);
      FSynchronizeController->LogOperation(soUpload, FileName);
    }
    else if (Operation == foDelete)
    {
      assert(Side == osRemote);
      FSynchronizeController->LogOperation(soDelete, FileName);
    }
  }

  if (FProgressForm)
  {
    DisconnectWhenComplete = FProgressForm->DisconnectWhenComplete;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool Temp, const AnsiString FileName, Boolean Success,
  bool & DisconnectWhenComplete)
{
  DoOperationFinished(Operation, Side, Temp, FileName, Success,
    DisconnectWhenComplete);
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TCustomScpExplorerForm::DirView(TOperationSide Side)
{
  assert(GetSide(Side) == osRemote);
  USEDPARAM(Side);
  return RemoteDirView;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableFocusedOperation(
  TOperationSide Side, int FilesOnly)
{
  return DirView(Side)->AnyFileSelected(true, (FilesOnly != 0));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableSelectedOperation(
  TOperationSide Side, int FilesOnly)
{
  return DirView(Side)->AnyFileSelected(false, (FilesOnly != 0));
}
//---------------------------------------------------------------------------
struct THistoryItemData
{
  // TOperationSide has 1 byte, but whole structure seems padded to 4 bytes,
  // just like "int"
  TOperationSide Side;
  short int Index;
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HistoryItemClick(System::TObject* Sender)
{
  TTBCustomItem * Item = dynamic_cast<TTBCustomItem *>(Sender);
  THistoryItemData Data = *reinterpret_cast<THistoryItemData*>(&(Item->Tag));
  DirView(Data.Side)->HistoryGo(Data.Index);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateHistoryMenu(TOperationSide Side,
  bool Back)
{
  if (FHistoryMenu[Side == osLocal][Back] != NULL)
  {
    TCustomDirView * DView = DirView(Side);
    TTBXPopupMenu * Menu = FHistoryMenu[Side == osLocal][Back];

    int ICount = Back ? DView->BackCount : DView->ForwardCount;
    if (ICount > 10)
    {
      ICount = 10;
    }
    Menu->Items->Clear();
    THistoryItemData Data;
    Data.Side = Side;
    for (short int i = 1; i <= ICount; i++)
    {
      TTBCustomItem * Item = new TTBXItem(Menu);
      Data.Index = static_cast<short int>(i * (Back ? -1 : 1));
      Item->Caption = MinimizeName(DView->HistoryPath[Data.Index], 50, (Side == osRemote));
      Item->Hint = DView->HistoryPath[Data.Index];
      assert(sizeof(int) == sizeof(THistoryItemData));
      Item->Tag = *reinterpret_cast<int*>(&Data);
      Item->OnClick = HistoryItemClick;
      Menu->Items->Add(Item);
    }
  }
}
//---------------------------------------------------------------------------
TTBXPopupMenu * __fastcall TCustomScpExplorerForm::HistoryMenu(
  TOperationSide Side, bool Back)
{
  if (FHistoryMenu[Side == osLocal][Back] == NULL)
  {
    // In Pascal the size of TTBXPopupMenu is 132, in C++ 136,
    // operator new allocates memory in Pascal code, but calls inline
    // contructor in C++, leading in problems, the funtion does
    // both in Pascal code
    FHistoryMenu[Side == osLocal][Back] = CreateTBXPopupMenu(this);
    UpdateHistoryMenu(Side, Back);
  }
  return FHistoryMenu[Side == osLocal][Back];
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewHistoryChange(
      TCustomDirView *Sender)
{
  TOperationSide Side = (Sender == DirView(osRemote) ? osRemote : osLocal);
  UpdateHistoryMenu(Side, true);
  UpdateHistoryMenu(Side, false);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CustomCommandRemoteAllowed()
{
  // remote custom commands can be executed only if the server supports shell commands
  // or have secondary shell
  return (FTerminal->IsCapable[fcSecondaryShell] || FTerminal->IsCapable[fcShellAnyCommand]);
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::CustomCommandState(
  const AnsiString & Command, int Params, bool OnFocused)
{
  int Result;

  TFileCustomCommand RemoteCustomCommand;
  TLocalCustomCommand LocalCustomCommand;
  TFileCustomCommand * NonInteractiveCustomCommand =
    FLAGCLEAR(Params, ccLocal) ? &RemoteCustomCommand : &LocalCustomCommand;
  TInteractiveCustomCommand InteractiveCustomCommand(NonInteractiveCustomCommand);
  AnsiString Cmd = InteractiveCustomCommand.Complete(Command, false);

  if (FLAGCLEAR(Params, ccLocal))
  {
    Result = CustomCommandRemoteAllowed();
    if (Result)
    {
      // custom command that does not operate with files can be executed anytime ...
      if (!NonInteractiveCustomCommand->IsFileCommand(Cmd))
      {
        // ... but do not show such command in remote file menu
        // (TODO, currently custom commands are in file menu only, so we cannot hide
        // such command, because it won't be accessible from elsewhere)
        Result = OnFocused ? /*-1*/ true : true;
      }
      else
      {
        Result = (FCurrentSide == osRemote) && EnableSelectedOperation[osRemote];
      }
    }
  }
  else
  {
    // custom command that does not operate with files can be executed anytime
    if (!NonInteractiveCustomCommand->IsFileCommand(Cmd))
    {
      Result = true;
    }
    else if (LocalCustomCommand.HasLocalFileName(Cmd))
    {
      // special case is "diff"-style command that can be executed over any side,
      // if we have both sides
      Result = (HasDirView[osLocal] && EnableSelectedOperation[osLocal]) &&
         EnableSelectedOperation[osRemote];
    }
    else
    {
      // other local custom commands can be executed only on remote side
      Result = (FCurrentSide == osRemote) && EnableSelectedOperation[osRemote];
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::CustomCommandState(
  const AnsiString & Description, bool OnFocused)
{
  AnsiString Command = WinConfiguration->CustomCommands->Values[Description];
  int Params = WinConfiguration->CustomCommands->Params[Description];

  return CustomCommandState(Command, Params, OnFocused);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomCommand(TStrings * FileList,
  AnsiString Name, AnsiString Command, int Params)
{
  if (FLAGCLEAR(Params, ccLocal))
  {
    if (EnsureCommandSessionFallback(fcShellAnyCommand))
    {
      TCustomCommandData Data(Terminal);
      TRemoteCustomCommand RemoteCustomCommand(Data, Terminal->CurrentDirectory);
      TWinInteractiveCustomCommand InteractiveCustomCommand(
        &RemoteCustomCommand, Name);

      Command = InteractiveCustomCommand.Complete(Command, false);
      bool Capture = FLAGSET(Params, ccShowResults) || FLAGSET(Params, ccCopyResults);
      TCaptureOutputEvent OutputEvent = NULL;

      assert(FCapturedLog == NULL);
      if (Capture)
      {
        FCapturedLog = new TStringList();
        OutputEvent = TerminalCaptureLog;
      }

      try
      {
        if (!RemoteCustomCommand.IsFileCommand(Command))
        {
          Terminal->AnyCommand(RemoteCustomCommand.Complete(Command, true),
            OutputEvent);
        }
        else
        {
          Terminal->CustomCommandOnFiles(Command, Params, FileList, OutputEvent);
        }

        if ((FCapturedLog != NULL) && (FCapturedLog->Count > 0))
        {
          if (FLAGSET(Params, ccCopyResults))
          {
            CopyToClipboard(FCapturedLog);
          }

          if (FLAGSET(Params, ccShowResults))
          {
            DoConsoleDialog(Terminal, "", FCapturedLog);
          }
        }
      }
      __finally
      {
        SAFE_DESTROY(FCapturedLog);
      }
    }
  }
  else
  {
    TCustomCommandData Data(Terminal);
    TLocalCustomCommand LocalCustomCommand(Data, Terminal->CurrentDirectory);
    TWinInteractiveCustomCommand InteractiveCustomCommand(
      &LocalCustomCommand, Name);

    Command = InteractiveCustomCommand.Complete(Command, false);

    if (!LocalCustomCommand.IsFileCommand(Command))
    {
      ExecuteShellAndWait(LocalCustomCommand.Complete(Command, true));
    }
    else
    {
      TStrings * LocalFileList = NULL;
      TStrings * RemoteFileList = NULL;
      try
      {
        bool FileListCommand = LocalCustomCommand.IsFileListCommand(Command);
        bool LocalFileCommand = LocalCustomCommand.HasLocalFileName(Command);

        if (LocalFileCommand)
        {
          assert(HasDirView[osLocal]);
          assert(EnableSelectedOperation[osLocal]);

          LocalFileList = DirView(osLocal)->CreateFileList(false, true, NULL);

          if (FileListCommand)
          {
            if (LocalFileList->Count != 1)
            {
              throw Exception(LoadStr(CUSTOM_COMMAND_SELECTED_UNMATCH1));
            }
          }
          else
          {
            if ((LocalFileList->Count != 1) &&
                (FileList->Count != 1) &&
                (LocalFileList->Count != FileList->Count))
            {
              throw Exception(LoadStr(CUSTOM_COMMAND_SELECTED_UNMATCH));
            }
          }
        }

        AnsiString RootTempDir;
        AnsiString TempDir;

        TemporarilyDownloadFiles(FileList, false, RootTempDir, TempDir, false, false, true);

        try
        {
          RemoteFileList = new TStringList();

          TMakeLocalFileListParams MakeFileListParam;
          MakeFileListParam.FileList = RemoteFileList;
          MakeFileListParam.IncludeDirs = FLAGSET(Params, ccApplyToDirectories);
          MakeFileListParam.Recursive =
            FLAGSET(Params, ccRecursive) && !FileListCommand;

          ProcessLocalDirectory(TempDir, Terminal->MakeLocalFileList, &MakeFileListParam);

          TFileOperationProgressType Progress(&OperationProgress, &OperationFinished);

          Progress.Start(foCustomCommand, osRemote, FileListCommand ? 1 : FileList->Count);
          assert(FProgressForm != NULL);
          FProgressForm->ReadOnly = true;

          try
          {
            if (FileListCommand)
            {
              AnsiString LocalFile;
              // MakeFileList does not delimit filenames
              AnsiString FileList = MakeFileList(RemoteFileList);

              if (LocalFileCommand)
              {
                assert(LocalFileList->Count == 1);
                LocalFile = LocalFileList->Strings[0];
              }

              TCustomCommandData Data(FTerminal);
              TLocalCustomCommand CustomCommand(Data,
                Terminal->CurrentDirectory, "", LocalFile, FileList);
              ExecuteShellAndWait(CustomCommand.Complete(Command, true));
            }
            else if (LocalFileCommand)
            {
              if (LocalFileList->Count == 1)
              {
                AnsiString LocalFile = LocalFileList->Strings[0];

                for (int Index = 0; Index < RemoteFileList->Count; Index++)
                {
                  AnsiString FileName = RemoteFileList->Strings[Index];
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(Data,
                    Terminal->CurrentDirectory, FileName, LocalFile, "");
                  ExecuteShellAndWait(CustomCommand.Complete(Command, true));
                }
              }
              else if (RemoteFileList->Count == 1)
              {
                AnsiString FileName = RemoteFileList->Strings[0];

                for (int Index = 0; Index < LocalFileList->Count; Index++)
                {
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(
                    Data, Terminal->CurrentDirectory,
                    FileName, LocalFileList->Strings[Index], "");
                  ExecuteShellAndWait(CustomCommand.Complete(Command, true));
                }
              }
              else
              {
                if (LocalFileList->Count != RemoteFileList->Count)
                {
                  throw Exception(LoadStr(CUSTOM_COMMAND_PAIRS_DOWNLOAD_FAILED));
                }

                for (int Index = 0; Index < LocalFileList->Count; Index++)
                {
                  AnsiString FileName = RemoteFileList->Strings[Index];
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(
                    Data, Terminal->CurrentDirectory,
                    FileName, LocalFileList->Strings[Index], "");
                  ExecuteShellAndWait(CustomCommand.Complete(Command, true));
                }
              }
            }
            else
            {
              for (int Index = 0; Index < RemoteFileList->Count; Index++)
              {
                TCustomCommandData Data(FTerminal);
                TLocalCustomCommand CustomCommand(Data,
                  Terminal->CurrentDirectory, RemoteFileList->Strings[Index], "", "");
                ExecuteShellAndWait(CustomCommand.Complete(Command, true));
              }
            }
          }
          __finally
          {
            Progress.Stop();
          }
        }
        __finally
        {
          RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir), false);
        }
      }
      __finally
      {
        delete RemoteFileList;
        delete LocalFileList;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalCaptureLog(
  const AnsiString & AddedLine, bool /*StdError*/)
{
  assert(FCapturedLog != NULL);
  FCapturedLog->Add(AddedLine);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::IsFileControl(TObject * Control,
  TOperationSide Side)
{
  return (Side == osRemote) &&
    ((Control == RemoteDirView) || (Control == RemoteDriveView));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewContextPopup(
      TObject * /*Sender*/, const TPoint &MousePos, bool &Handled)
{
  TListItem * Item = RemoteDirView->ItemFocused;
  if ((RemoteDirView->GetItemAt(MousePos.x, MousePos.y) == Item) &&
      RemoteDirView->AnyFileSelected(true, false))
  {
    TPoint ScreenPoint, ClientPoint;
    ClientPoint = ((MousePos.x < 0) && (MousePos.y < 0)) ?
      TPoint(0, 0) : MousePos;
    ScreenPoint = RemoteDirView->ClientToScreen(ClientPoint);

    #define DEFAULTITEM(ITEM, DEFAULT) \
      { \
        TTBItemOptions O; \
        O = NonVisualDataModule->ITEM->Options; \
        if ((DEFAULT) && Terminal->ResolvingSymlinks && \
            !RemoteDirView->ItemIsDirectory(Item)) \
          NonVisualDataModule->ITEM->Options = O << tboDefault; \
        else \
          NonVisualDataModule->ITEM->Options = O >> tboDefault; \
      }
    DEFAULTITEM(CurrentOpenMenuItem, WinConfiguration->DoubleClickAction == dcaOpen);
    DEFAULTITEM(CurrentEditMenuItem, WinConfiguration->DoubleClickAction == dcaEdit);
    DEFAULTITEM(CurrentCopyMenuItem, WinConfiguration->DoubleClickAction == dcaCopy);
    #undef DEFAULTITEM

    NonVisualDataModule->CurrentOpenMenuItem->Visible = WinConfiguration->ExpertMode;
    NonVisualDataModule->CurrentEditMenuItem->Visible = WinConfiguration->ExpertMode;

    reinterpret_cast<TPopupMenu*>(GetComponent(fcRemotePopup))->Popup(
      ScreenPoint.x, ScreenPoint.y);
  }
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReloadLocalDirectory(const AnsiString Directory)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchStart(void *& /*Storage*/)
{
  assert(FErrorList == NULL);
  if (WinConfiguration->ContinueOnError)
  {
    FErrorList = new TStringList();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchEnd(void * /*Storage*/)
{
  if (FErrorList)
  {
    HandleErrorList(FErrorList);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * Param)
{
  void * BatchStorage;
  BatchStart(BatchStorage);
  try
  {
    if ((Operation == foCopy) || (Operation == foMove))
    {
      TTransferDirection Direction = (Side == osLocal ? tdToRemote : tdToLocal);
      TTransferType Type = (Operation == foCopy ? ttCopy : ttMove);
      AnsiString TargetDirectory;
      bool Temp = false;
      bool DragDrop = false;
      if (Param != NULL)
      {
        TTransferOperationParam& TParam =
          *static_cast<TTransferOperationParam*>(Param);
        TargetDirectory = TParam.TargetDirectory;
        Temp = TParam.Temp;
        DragDrop = TParam.DragDrop;
      }
      TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
      if (CopyParamDialog(Direction, Type, Temp, FileList, TargetDirectory,
          CopyParam, !NoConfirmation, DragDrop))
      {
        assert(Terminal);
        bool SelectionRestored = false;
        TCustomDirView * DView = NULL;
        if (HasDirView[Side])
        {
          DView = DirView(Side);
          DView->SaveSelection();
          DView->SaveSelectedNames();
        }

        try
        {
          if (Side == osLocal)
          {
            int Params =
              FLAGMASK(Operation == foMove, cpDelete) |
              FLAGMASK(CopyParam.NewerOnly, cpNewerOnly) |
              FLAGMASK(Temp, cpTemporary);
            Terminal->CopyToRemote(FileList, TargetDirectory, &CopyParam, Params);
            if (Operation == foMove)
            {
              ReloadLocalDirectory();
              if (DView != NULL)
              {
                DView->RestoreSelection();
              }
              SelectionRestored = true;
            }
          }
          else
          {
            try
            {
              int Params =
                (Operation == foMove ? cpDelete : 0) |
                (CopyParam.NewerOnly ? cpNewerOnly : 0);
              Terminal->CopyToLocal(FileList, TargetDirectory, &CopyParam,
                Params);
            }
            __finally
            {
              if (Operation == foMove)
              {
                if (DView != NULL)
                {
                  DView->RestoreSelection();
                }
                SelectionRestored = true;
              }
              ReloadLocalDirectory(TargetDirectory);
            }
          }
        }
        __finally
        {
          if (!SelectionRestored && (DView != NULL))
          {
            DView->DiscardSavedSelection();
          }
        }
      }
    }
    else if (Operation == foRename)
    {
      assert(DirView(Side)->ItemFocused);
      DirView(Side)->ItemFocused->EditCaption();
    }
    else if (Operation == foDelete)
    {
      assert(FileList->Count);
      bool Alternative = bool(Param);
      bool Recycle;
      if (Side == osLocal)
      {
        Recycle = (WinConfiguration->DeleteToRecycleBin != Alternative);
      }
      else
      {
        Recycle = (Terminal->SessionData->DeleteToRecycleBin != Alternative) &&
          !Terminal->IsRecycledFile(FileList->Strings[0]);
      }

      bool Confirmed =
        !(Recycle ? WinConfiguration->ConfirmRecycling : WinConfiguration->ConfirmDeleting);
      if (!Confirmed)
      {
        AnsiString Query;
        if (FileList->Count == 1)
        {
          if (Side == osLocal)
          {
            Query = ExtractFileName(FileList->Strings[0]);
          }
          else
          {
            Query = UnixExtractFileName(FileList->Strings[0]);
          }
          Query = FMTLOAD(
            (Recycle ? CONFIRM_RECYCLE_FILE : CONFIRM_DELETE_FILE), (Query));
        }
        else
        {
          Query = FMTLOAD(
            (Recycle ? CONFIRM_RECYCLE_FILES : CONFIRM_DELETE_FILES), (FileList->Count));
        }

        TMessageParams Params(mpNeverAskAgainCheck);
        int Answer = MessageDialog(Query, qtConfirmation,
          qaOK | qaCancel, HELP_NONE, &Params);
        if (Answer == qaNeverAskAgain)
        {
          Confirmed = true;
          if (Recycle)
          {
            WinConfiguration->ConfirmRecycling = false;
          }
          else
          {
            WinConfiguration->ConfirmDeleting = false;
          }
        }
        else
        {
          Confirmed = (Answer == qaOK);
        }
      }

      if (Confirmed) DeleteFiles(Side, FileList, FLAGMASK(Alternative, dfAlternative));
    }
    else if (Operation == foSetProperties)
    {
      RemoteDirView->SaveSelectedNames();
      SetProperties(Side, FileList);
    }
    else if (Operation == foCustomCommand)
    {
      assert(Param);
      assert(Side == osRemote);

      RemoteDirView->SaveSelectedNames();
      TCustomCommandParam * AParam = static_cast<TCustomCommandParam*>(Param);
      CustomCommand(FileList, AParam->Name, AParam->Command, AParam->Params);
    }
    else if ((Operation == foRemoteMove) || (Operation == foRemoteCopy))
    {
      assert(Side == osRemote);
      RemoteTransferFiles(FileList, NoConfirmation, (Operation == foRemoteMove));
    }
    else
    {
      assert(false);
    }
  }
  __finally
  {
    BatchEnd(BatchStorage);
  }
}
//---------------------------------------------------------------------------
TOperationSide __fastcall TCustomScpExplorerForm::GetSide(TOperationSide Side)
{
  if (Side == osCurrent)
  {
    Side = FCurrentSide;
  }

  return Side;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, bool OnFocused, bool NoConfirmation, void * Param)
{
  Side = GetSide(Side);

  TStrings * FileList = DirView(Side)->CreateFileList(OnFocused, (Side == osLocal), NULL);
  try
  {
    ExecuteFileOperation(Operation, Side, FileList, NoConfirmation, Param);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HandleErrorList(TStringList *& ErrorList)
{
  try
  {
    if (ErrorList->Count)
    {
      if (MessageDialog(FMTLOAD(ERROR_LIST_COUNT, (ErrorList->Count)), qtError,
          qaOK | qaCancel, HELP_NONE) == qaOK)
      {
        int Answer;
        int Index = 0;
        do
        {
          assert(Index >= 0 && Index < ErrorList->Count);
          TQueryButtonAlias Aliases[2];
          Aliases[0].Button = qaYes;
          Aliases[0].Alias = LoadStr(PREV_BUTTON);
          Aliases[1].Button = qaNo;
          Aliases[1].Alias = LoadStr(NEXT_BUTTON);
          TMessageParams Params;
          Params.Aliases = Aliases;
          Params.AliasesCount = LENOF(Aliases);

          Answer = MoreMessageDialog(
            FMTLOAD(ERROR_LIST_NUMBER, (Index+1, ErrorList->Count, ErrorList->Strings[Index])),
            dynamic_cast<TStrings *>(ErrorList->Objects[Index]), qtError,
            (Index ? qaYes : 0) | (Index < ErrorList->Count - 1 ? qaNo : 0) |
            qaOK, HELP_NONE, &Params);

          if (Answer == qaNo)
          {
            Index++;
          }
          if (Answer == qaYes)
          {
            Index--;
          }
        }
        while (Answer != qaOK);
      }
    }
  }
  __finally
  {
    TStrings * List = ErrorList;
    ErrorList = NULL;
    for (int i = 0; i < List->Count; i++)
    {
      delete List->Objects[i];
    }
    delete List;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::EditNew(TOperationSide Side)
{
  assert(!WinConfiguration->DisableOpenEdit);

  Side = GetSide(Side);

  AnsiString Name = LoadStr(NEW_FILE);
  if (InputDialog(LoadStr(NEW_FILE_CAPTION), LoadStr(NEW_FILE_PROMPT), Name,
        HELP_EDIT_NEW))
  {
    AnsiString TargetFileName;
    AnsiString LocalFileName;
    AnsiString RootTempDir;
    AnsiString TempDir;
    if (Side == osRemote)
    {
      TempDir = TemporaryDirectoryForRemoteFiles(
        GUIConfiguration->CurrentCopyParam, RootTempDir);

      TargetFileName = UnixExtractFileName(Name);
      LocalFileName = TempDir +
        GUIConfiguration->CurrentCopyParam.ChangeFileName(TargetFileName, osRemote, false);
    }
    else
    {
      if (ExtractFilePath(Name).IsEmpty())
      {
        LocalFileName = IncludeTrailingBackslash(DirView(Side)->PathName) + Name;
      }
      else
      {
        LocalFileName = ExpandFileName(Name);
      }

      TargetFileName = ExtractFileName(Name);
    }

    if (!FileExists(LocalFileName))
    {
      int File = FileCreate(LocalFileName);
      if (File < 0)
      {
        if (!RootTempDir.IsEmpty())
        {
          RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir), false);
        }
        throw Exception(FMTLOAD(CREATE_FILE_ERROR, (LocalFileName)));
      }
      else
      {
        FileClose(File);
      }
    }

    TExecuteFileBy ExecuteFileBy = efDefaultEditor;
    const TEditorPreferences * ExternalEditor = NULL;
    TFileMasks::TParams MaskParams; // size not known
    ExecuteFileNormalize(ExecuteFileBy, ExternalEditor, TargetFileName,
      false, MaskParams);

    CustomExecuteFile(Side, ExecuteFileBy, LocalFileName, TargetFileName,
      ExternalEditor, RootTempDir);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteExecuteForceText(
  TExecuteFileBy ExecuteFileBy, const TEditorPreferences * ExternalEditor)
{
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Data.Editor == edExternal)));
  assert(ExecuteFileBy != efDefaultEditor);

  return
    ((ExecuteFileBy == efInternalEditor)) ||
    ((ExecuteFileBy == efExternalEditor) && ExternalEditor->Data.ExternalEditorText);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, AnsiString FileName, AnsiString OriginalFileName,
  const TEditorPreferences * ExternalEditor, AnsiString LocalRootDirectory)
{
  assert(!WinConfiguration->DisableOpenEdit);
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Data.Editor == edExternal)));
  assert(ExecuteFileBy != efDefaultEditor);

  Side = GetSide(Side);

  TEditedFileData Data;
  if (Side == osRemote)
  {
    Data.Terminal = Terminal;
    Data.Queue = Queue;
    Data.ForceText = RemoteExecuteForceText(ExecuteFileBy, ExternalEditor);
    Data.RemoteDirectory = RemoteDirView->PathName;
    Data.SessionName = Terminal->SessionData->SessionName;
    Data.LocalRootDirectory = LocalRootDirectory;
    Data.OriginalFileName = OriginalFileName;
    Data.Command = ""; // will be changed later for external editor
  }

  if (ExecuteFileBy == efInternalEditor)
  {
    if (Side == osRemote)
    {
      AnsiString Caption = RemoteDirView->Path + OriginalFileName +
        " - " + Terminal->SessionData->SessionName;
      TForm * Editor = ShowEditorForm(FileName, this, FEditorManager->FileChanged,
        FEditorManager->FileReload, FEditorManager->FileClosed, Caption);

      FEditorManager->AddFileInternal(FileName, Data, Editor);
    }
    else
    {
      ShowEditorForm(FileName, this, NULL, NULL, NULL, "");
    }
  }
  else
  {
    HANDLE Process;

    if (ExecuteFileBy == efExternalEditor)
    {
      AnsiString Program, Params, Dir;
      Data.Command = ExternalEditor->Data.ExternalEditor;
      ReformatFileNameCommand(Data.Command);
      SplitCommand(Data.Command, Program, Params, Dir);
      Params = ExpandFileNameCommand(Params, FileName);
      if (!ExecuteShell(Program, Params, Process))
      {
        throw Exception(FMTLOAD(EDITOR_ERROR, (Program)));
      }
    }
    else
    {
      assert(Side == osRemote);
      if (!ExecuteShell(FileName, "", Process))
      {
        throw Exception(FMTLOAD(EXECUTE_FILE_ERROR, (FileName)));
      }
    }

    if ((Side == osLocal) ||
        ((ExecuteFileBy == efExternalEditor) &&
         ExternalEditor->Data.MDIExternalEditor))
    {
      // no need for handle
      if (Process != NULL)
      {
        CHECK(CloseHandle(Process));
      }
      Process = INVALID_HANDLE_VALUE;
    }
    else
    {
      if (Process == NULL)
      {
        throw Exception(LoadStr(OPEN_FILE_NO_PROCESS2));
      }
    }

    if (Side == osRemote)
    {
      FEditorManager->AddFileExternal(FileName, Data, Process);
    }
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomScpExplorerForm::TemporaryDirectoryForRemoteFiles(
  TCopyParamType CopyParam, AnsiString & RootDirectory)
{
  RootDirectory = IncludeTrailingBackslash(WinConfiguration->TemporaryDir());
  AnsiString Result = FTerminal->CurrentDirectory;
  if (!Result.IsEmpty() && (Result[1] == '/'))
  {
    Result.Delete(1, 1);
  }
  Result = IncludeTrailingBackslash(RootDirectory + CopyParam.ValidLocalPath(FromUnixPath(Result)));
  if (!ForceDirectories(Result))
  {
    throw Exception(FMTLOAD(CREATE_TEMP_DIR_ERROR, (Result)));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporarilyDownloadFiles(
  TStrings * FileList, bool ForceText, AnsiString & RootTempDir, AnsiString & TempDir,
  bool AllFiles, bool GetTargetNames, bool AutoOperation)
{
  TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  if (ForceText)
  {
    CopyParam.TransferMode = tmAscii;
  }
  // do not forget to add additional options to ExecutedFileChanged, FAR and SS
  CopyParam.FileNameCase = ncNoChange;
  CopyParam.PreserveReadOnly = false;
  CopyParam.ResumeSupport = rsOff;
  CopyParam.ReplaceInvalidChars = true;
  CopyParam.FileMask = "";
  if (AllFiles)
  {
    CopyParam.ExcludeFileMask = TFileMasks();
  }

  if (RootTempDir.IsEmpty())
  {
    TempDir = TemporaryDirectoryForRemoteFiles(CopyParam, RootTempDir);
  }

  assert(!FAutoOperation);
  FAutoOperation = AutoOperation;
  Terminal->ExceptionOnFail = true;
  try
  {
    try
    {
      // turn off confirmations, as for MDI editors we may possibly download
      // the same file over
      Terminal->CopyToLocal(FileList, TempDir, &CopyParam,
        cpNoConfirmation | cpTemporary);

      if (GetTargetNames)
      {
        for (int i = 0; i < FileList->Count; i++)
        {
          FileList->Strings[i] =
            CopyParam.ChangeFileName(UnixExtractFileName(FileList->Strings[i]), osRemote, false);
        }
      }
    }
    catch(...)
    {
      try
      {
        RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir), false);
      }
      catch(...)
      {
      }
      throw;
    }
  }
  __finally
  {
    FAutoOperation = false;
    Terminal->ExceptionOnFail = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileNormalize(
  TExecuteFileBy & ExecuteFileBy, const TEditorPreferences *& ExternalEditor,
  const AnsiString & FileName, bool Local, const TFileMasks::TParams & MaskParams)
{
  if (ExecuteFileBy == efDefaultEditor)
  {
    ExternalEditor = WinConfiguration->DefaultEditorForFile(FileName, Local, MaskParams);
    if ((ExternalEditor == NULL) || (ExternalEditor->Data.Editor == edInternal))
    {
      ExecuteFileBy = efInternalEditor;
      ExternalEditor = NULL;
    }
    else
    {
      ExecuteFileBy = efExternalEditor;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, const TEditorPreferences * ExternalEditor,
  bool AllSelected, bool OnFocused)
{
  assert(!WinConfiguration->DisableOpenEdit);
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Data.Editor == edExternal)));

  Side = GetSide(Side);

  TCustomDirView * DView = DirView(Side);
  TStrings * FileList = AllSelected ?
    DView->CreateFileList(OnFocused, Side == osLocal) :
    DView->CreateFocusedFileList(Side == osLocal);
  try
  {
    assert(AllSelected || (FileList->Count == 1));
    for (int i = 0; i < FileList->Count; i++)
    {
      AnsiString ListFileName = FileList->Strings[i];
      AnsiString FileNameOnly = (Side == osRemote) ?
        UnixExtractFileName(ListFileName) : ExtractFileName(ListFileName);
      TListItem * Item = DView->FindFileItem(FileNameOnly);
      if (!DView->ItemIsDirectory(Item))
      {
        AnsiString OriginalFileName;
        AnsiString FullFileName;
        AnsiString FileName;

        if (Side == osRemote)
        {
          OriginalFileName = ListFileName;
          FullFileName = RemoteDirView->Path + ListFileName;
        }
        else
        {
          OriginalFileName = ExtractFileName(ListFileName);
          FullFileName = ListFileName;
        }

        AnsiString LocalDirectory;
        AnsiString LocalRootDirectory;
        TFileMasks::TParams MaskParams;
        MaskParams.Size = DView->ItemFileSize(Item);
        ExecuteFileNormalize(ExecuteFileBy, ExternalEditor, FullFileName,
          (Side == osLocal), MaskParams);

        if (Side == osRemote)
        {
          TObject * Token = NULL;
          if (!FEditorManager->CanAddFile(RemoteDirView->PathName, OriginalFileName,
                 Terminal->SessionData->SessionName, Token, LocalRootDirectory,
                 LocalDirectory))
          {
            if (Token != NULL)
            {
              TForm * Form = dynamic_cast<TForm *>(Token);
              Form->SetFocus();
              Abort();
            }
            else
            {
              throw Exception(FMTLOAD(ALREADY_EDITED_EXTERNALLY, (OriginalFileName)));
            }
          }

          TStringList * FileList1 = new TStringList();
          try
          {
            FileList1->AddObject(ListFileName, FileList->Objects[i]);
            TemporarilyDownloadFiles(FileList1,
              RemoteExecuteForceText(ExecuteFileBy, ExternalEditor),
              LocalRootDirectory, LocalDirectory, true, true, true);
            FileName = LocalDirectory + FileList1->Strings[0];
          }
          __finally
          {
            delete FileList1;
          }
        }
        else
        {
          FileName = FileList->Strings[i];
        }

        CustomExecuteFile(Side, ExecuteFileBy, FileName, OriginalFileName,
          ExternalEditor, LocalRootDirectory);
      }
    }
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChanged(const AnsiString FileName,
  const TEditedFileData & Data, HANDLE UploadCompleteEvent)
{
  if ((Data.Terminal == NULL) || !Data.Terminal->Active)
  {
    throw Exception(FMTLOAD(EDIT_SESSION_CLOSED,
      (ExtractFileName(FileName), Data.SessionName)));
  }

  TStrings * FileList = new TStringList();
  try
  {
    FileList->Add(FileName);

    // consider using the same settings (preset) as when the file was downloaded
    TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
    if (Data.ForceText)
    {
      CopyParam.TransferMode = tmAscii;
    }
    // do not forget to add additional options to TemporarilyDownloadFiles, FAR and SS
    CopyParam.FileNameCase = ncNoChange;
    CopyParam.PreserveRights = false;
    CopyParam.ResumeSupport = rsOff;
    // so i do not need to worry if masking algorithm works in all cases
    // ("" means "copy file name", no masking is actually done)
    if (ExtractFileName(FileName) == Data.OriginalFileName)
    {
      CopyParam.FileMask = "";
    }
    else
    {
      CopyParam.FileMask = DelimitFileNameMask(Data.OriginalFileName);
    }
    CopyParam.ReplaceInvalidChars = true;
    CopyParam.ExcludeFileMask = TFileMasks();

    assert(Queue != NULL);

    int Params = cpNoConfirmation | cpTemporary;
    TQueueItem * QueueItem = new TUploadQueueItem(Data.Terminal, FileList,
      Data.RemoteDirectory, &CopyParam, Params);
    QueueItem->CompleteEvent = UploadCompleteEvent;
    Data.Queue->AddItem(QueueItem);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileReload(
  const AnsiString FileName, const TEditedFileData & Data)
{
  if ((Data.Terminal == NULL) || !Data.Terminal->Active)
  {
    throw Exception(FMTLOAD(EDIT_SESSION_CLOSED_RELOAD,
      (ExtractFileName(FileName), Data.SessionName)));
  }

  TStrings * FileList = new TStringList();
  try
  {
    AnsiString RemoteFileName =
      UnixIncludeTrailingBackslash(Data.RemoteDirectory) + Data.OriginalFileName;
    TRemoteFile * File = NULL;
    FTerminal->ExceptionOnFail = true;
    try
    {
      FTerminal->ReadFile(RemoteFileName, File);
      if (!File->HaveFullFileName)
      {
        File->FullFileName = RemoteFileName;
      }
    }
    __finally
    {
      FTerminal->ExceptionOnFail = false;
    }
    FileList->AddObject(RemoteFileName, File);

    AnsiString RootTempDir = Data.LocalRootDirectory;
    AnsiString TempDir = ExtractFilePath(FileName);
    TemporarilyDownloadFiles(FileList, Data.ForceText, RootTempDir,
      TempDir, true, true, true);

    // sanity check, the target file name should be still the same
    assert(ExtractFileName(FileName) == FileList->Strings[0]);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileEarlyClosed(
  const TEditedFileData & Data, bool & KeepOpen)
{
  // Command is set for external editors only (not for "shell" open).
  if (!Data.Command.IsEmpty())
  {
    bool AnyFound = false;
    bool AnyMDI = false;
    bool AnyNonMDI = false;
    bool AnyDetect = false;
    TEditorList * EditorList = new TEditorList();
    try
    {
      *EditorList = *WinConfiguration->EditorList;
      for (int i = 0; i < EditorList->Count; i++)
      {
        const TEditorPreferences * Editor = EditorList->Editors[i];
        if ((Editor->Data.Editor == edExternal) &&
            (Editor->Data.ExternalEditor == Data.Command))
        {
          AnyFound = true;
          if (!Editor->Data.MDIExternalEditor)
          {
            AnyNonMDI = true;
            if (Editor->Data.DetectMDIExternalEditor)
            {
              AnyDetect = true;
            }
          }
          else
          {
            AnyMDI = true;
          }
        }
      }

      bool EnableMDI = false;
      bool DisableDetect = false;

      if (AnyMDI)
      {
        KeepOpen = true;
        if (AnyNonMDI)
        {
          // there is at least one instance of the editor with MDI support enabled,
          // and one with disabled, enable it for all instances
          EnableMDI = true;
        }
      }
      else if (AnyFound && !AnyDetect)
      {
        // at least once instance found but all have MDI autodetection disabled
        // => close the file (default action)
      }
      else
      {
        // no instance of the editor has MDI support enabled

        TMessageParams Params;
        if (AnyFound)
        {
          // there is at least one instance of the editor with enabled
          // MDI autodetection
          Params.Params |= mpNeverAskAgainCheck;
        }
        int Answer = MessageDialog(FMTLOAD(EDITOR_EARLY_CLOSED, (Data.OriginalFileName)), qtWarning,
          qaYes | qaNo, HELP_EDITOR_EARLY_CLOSED, &Params);
        switch (Answer)
        {
          case qaNeverAskAgain:
            DisableDetect = true;
            break;

          case qaNo:
            EnableMDI = true;
            KeepOpen = true;
            break;
        }
      }

      if (AnyFound && (EnableMDI || DisableDetect))
      {
        bool Changed = false;
        for (int i = 0; i < EditorList->Count; i++)
        {
          const TEditorPreferences * Editor = EditorList->Editors[i];
          if ((Editor->Data.Editor == edExternal) &&
              (Editor->Data.ExternalEditor == Data.Command) &&
              ((EnableMDI && !Editor->Data.MDIExternalEditor) ||
               (DisableDetect && Editor->Data.DetectMDIExternalEditor)))
          {
            Changed = true;
            TEditorPreferences * UpdatedEditor = new TEditorPreferences(*Editor);
            if (EnableMDI)
            {
              UpdatedEditor->Data.MDIExternalEditor = true;
            }
            if (DisableDetect)
            {
              UpdatedEditor->Data.DetectMDIExternalEditor = false;
            }
            EditorList->Change(i, UpdatedEditor);
          }
        }

        if (Changed)
        {
          WinConfiguration->EditorList = EditorList;
        }
      }
    }
    __finally
    {
      delete EditorList;
    }
  }
  else
  {
    // "open" case

    MessageDialog(FMTLOAD(APP_EARLY_CLOSED, (Data.OriginalFileName)), qtWarning,
      qaOK, HELP_APP_EARLY_CLOSED);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewEnter(TObject * /*Sender*/)
{
  SideEnter(osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDriveViewEnter(TObject * /*Sender*/)
{
  MakeNextInTabOrder(RemoteDirView, RemoteDriveView);
  SideEnter(osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SideEnter(TOperationSide Side)
{
  FCurrentSide = Side;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DeleteFiles(TOperationSide Side,
  TStrings * FileList, bool Alternative)
{
  assert(Terminal);
  TCustomDirView * DView = DirView(Side);
  DView->SaveSelection();
  DView->SaveSelectedNames();
  assert(!FAlternativeDelete);
  FAlternativeDelete = Alternative;

  try
  {
    if (Side == osRemote)
    {
      Terminal->DeleteFiles(FileList, FLAGMASK(Alternative, dfAlternative));
    }
    else
    {
      try
      {
        Terminal->DeleteLocalFiles(FileList, FLAGMASK(Alternative, dfAlternative));
      }
      __finally
      {
        ReloadLocalDirectory();
      }
    }
    FAlternativeDelete = false;
  }
  catch(...)
  {
    FAlternativeDelete = false;
    DView->DiscardSavedSelection();
    throw;
  }
  DView->RestoreSelection();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteTransferDialog(TTerminal *& Session,
  AnsiString & Target, AnsiString & FileMask, bool & DirectCopy,
  bool NoConfirmation, bool Move)
{
  if (RemoteDriveView->DropTarget != NULL)
  {
    Target = RemoteDriveView->NodePathName(RemoteDriveView->DropTarget);
  }
  else if (RemoteDirView->DropTarget != NULL)
  {
    assert(RemoteDirView->ItemIsDirectory(RemoteDirView->DropTarget));
    Target = RemoteDirView->ItemFullFileName(RemoteDirView->DropTarget);
  }
  else
  {
    Target = RemoteDirView->Path;
  }

  Session = TTerminalManager::Instance()->ActiveTerminal;
  Target = UnixIncludeTrailingBackslash(Target);
  FileMask = "*.*";
  DirectCopy = FTerminal->IsCapable[fcRemoteCopy] || FTerminal->IsCapable[fcSecondaryShell];
  bool Result = true;
  if (!NoConfirmation)
  {
    if (Move)
    {
      Result = DoRemoteMoveDialog(Target, FileMask);
    }
    else
    {
      assert(Terminal != NULL);
      // update Terminal->RemoteDirectory
      UpdateTerminal(Terminal);
      TStrings * Sessions = TTerminalManager::Instance()->TerminalList;
      TStrings * Directories = new TStringList;
      try
      {
        for (int Index = 0; Index < Sessions->Count; Index++)
        {
          TManagedTerminal * Terminal =
            dynamic_cast<TManagedTerminal *>(Sessions->Objects[Index]);
          Directories->Add(Terminal->RemoteDirectory);
        }

        TDirectRemoteCopy AllowDirectCopy;
        if (FTerminal->IsCapable[fcRemoteCopy] || FTerminal->CommandSessionOpened)
        {
          assert(DirectCopy);
          AllowDirectCopy = drcAllow;
        }
        else if (FTerminal->IsCapable[fcSecondaryShell])
        {
          assert(DirectCopy);
          AllowDirectCopy = drcConfirmCommandSession;
        }
        else
        {
          assert(!DirectCopy);
          AllowDirectCopy = drcDisallow;
        }
        void * ASession = Session;
        Result = DoRemoteCopyDialog(Sessions, Directories, AllowDirectCopy,
          ASession, Target, FileMask, DirectCopy);
        Session = static_cast<TTerminal *>(ASession);
      }
      __finally
      {
        delete Directories;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteTransferFiles(
  TStrings * FileList, bool NoConfirmation, bool Move)
{
  TTerminal * Session;
  bool DirectCopy;
  AnsiString Target, FileMask;
  if (RemoteTransferDialog(Session, Target, FileMask, DirectCopy, NoConfirmation, Move))
  {
    if (!Move && !DirectCopy)
    {
      AnsiString RootTempDir;
      AnsiString TempDir;

      TemporarilyDownloadFiles(FileList, false, RootTempDir, TempDir, false, false, false);

      TStrings * TemporaryFilesList = new TStringList();

      try
      {
        TMakeLocalFileListParams MakeFileListParam;
        MakeFileListParam.FileList = TemporaryFilesList;
        MakeFileListParam.IncludeDirs = true;
        MakeFileListParam.Recursive = false;

        ProcessLocalDirectory(TempDir, FTerminal->MakeLocalFileList, &MakeFileListParam);

        TTerminalManager::Instance()->ActiveTerminal = Session;

        TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
        CopyParam.FileMask = FileMask;

        assert(!FAutoOperation);
        FAutoOperation = true;
        FTerminal->CopyToRemote(TemporaryFilesList, Target, &CopyParam, cpTemporary);
      }
      __finally
      {
        delete TemporaryFilesList;
        FAutoOperation = false;
        RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir), false);
      }
    }
    else
    {
      RemoteDirView->SaveSelection();
      RemoteDirView->SaveSelectedNames();

      try
      {
        if (Move)
        {
          Terminal->MoveFiles(FileList, Target, FileMask);
        }
        else
        {
          assert(DirectCopy);
          assert(Session == FTerminal);

          if (FTerminal->IsCapable[fcRemoteCopy] ||
              FTerminal->CommandSessionOpened ||
              CommandSessionFallback())
          {
            Terminal->CopyFiles(FileList, Target, FileMask);
          }
        }
      }
      catch(...)
      {
        RemoteDirView->DiscardSavedSelection();
        throw;
      }
      RemoteDirView->RestoreSelection();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateDirectory(TOperationSide Side)
{
  Side = GetSide(Side);
  TRemoteProperties Properties = GUIConfiguration->NewDirectoryProperties;
  TRemoteProperties * AProperties = (Side == osRemote ? &Properties : NULL);
  AnsiString Name = LoadStr(NEW_FOLDER);
  bool SaveSettings = false;

  if (DoCreateDirectoryDialog(Name, AProperties, SaveSettings))
  {
    if (Side == osRemote)
    {
      if (SaveSettings)
      {
        GUIConfiguration->NewDirectoryProperties = Properties;
      }
      RemoteDirView->CreateDirectoryEx(Name, &Properties);
    }
    else
    {
      DirView(Side)->CreateDirectory(Name);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HomeDirectory(TOperationSide Side)
{
  DirView(Side)->ExecuteHomeDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenDirectory(TOperationSide Side)
{
  DoOpenDirectoryDialog(odBrowse, Side);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetSelectFilter(
      TCustomDirView *Sender, bool Select, TFileFilter &Filter)
{
  assert(Sender);
  if (!DoSelectMaskDialog(Sender, Select, &Filter, Configuration)) Abort();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CalculateSize(
  TStrings * FileList, __int64 & Size, TCalculateSizeStats & Stats,
  bool & Close)
{
  // terminal can be already closed (e.g. dropped connection)
  if (Terminal != NULL)
  {
    try
    {
      Terminal->CalculateFilesSize(FileList, Size, 0, NULL, &Stats);
    }
    catch(...)
    {
      if (!Terminal->Active)
      {
        Close = true;
      }
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CalculateChecksum(const AnsiString & Alg,
  TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  bool & Close)
{
  // terminal can be already closed (e.g. dropped connection)
  if (Terminal != NULL)
  {
    try
    {
      Terminal->CalculateFilesChecksum(Alg, FileList, NULL, OnCalculatedChecksum);
    }
    catch(...)
    {
      if (!Terminal->Active)
      {
        Close = true;
      }
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetProperties(TOperationSide Side, TStrings * FileList)
{
  if (Side == osRemote)
  {
    TRemoteProperties CurrentProperties;

    if (Terminal->LoadFilesProperties(FileList))
    {
      RemoteDirView->Invalidate();
    }
    CurrentProperties = TRemoteProperties::CommonProperties(FileList);

    int Flags = 0;
    if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
    if (Terminal->IsCapable[fcOwnerChanging]) Flags |= cpOwner;
    if (Terminal->IsCapable[fcGroupChanging]) Flags |= cpGroup;

    TCalculateChecksumEvent CalculateChecksumEvent = NULL;
    if (Terminal->IsCapable[fcCalculatingChecksum])
    {
      CalculateChecksumEvent = CalculateChecksum;
    }

    TRemoteProperties NewProperties = CurrentProperties;
    if (DoPropertiesDialog(FileList, RemoteDirView->PathName,
        Terminal->Groups, Terminal->Users, &NewProperties, Flags,
        CalculateSize, CalculateChecksumEvent))
    {
      NewProperties = TRemoteProperties::ChangedProperties(CurrentProperties, NewProperties);
      Terminal->ChangeFilesProperties(FileList, &NewProperties);
    }
  }
  else
  {
    DirView(Side)->DisplayPropertiesMenu();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::KeyDown(Word & Key, Classes::TShiftState Shift)
{
  if (QueueView2->Focused() && (QueueView2->OnKeyDown != NULL))
  {
    QueueView2->OnKeyDown(QueueView2, Key, Shift);
  }

  if (!DirView(osCurrent)->IsEditing())
  {
    TShortCut KeyShortCut = ShortCut(Key, Shift);
    for (int Index = 0; Index < NonVisualDataModule->ExplorerActions->ActionCount; Index++)
    {
      TAction * Action = (TAction *)NonVisualDataModule->ExplorerActions->Actions[Index];
      if (((Action->ShortCut == KeyShortCut) ||
           (Action->SecondaryShortCuts->IndexOfShortCut(KeyShortCut) >= 0)) &&
          AllowedAction(Action, aaShortCut))
      {
        Key = 0;
        Action->Execute();
        return;
      }
    }
    for (int i = 0; i < TTerminalManager::Instance()->Count; i++)
    {
      if (NonVisualDataModule->OpenSessionShortCut(i) == KeyShortCut)
      {
        FIgnoreNextSysCommand = true;
        TTerminalManager::Instance()->ActiveTerminalIndex = i;
        return;
      }
    }
    if (Key == VK_TAB && Shift.Contains(ssCtrl))
    {
      TTerminalManager::Instance()->CycleTerminals(!Shift.Contains(ssShift));
      Key = 0;
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InitStatusBar()
{
  const TSessionInfo & SessionInfo = Terminal->GetSessionInfo();
  const TFileSystemInfo & FileSystemInfo = Terminal->GetFileSystemInfo();
  TTBXStatusBar * SessionStatusBar = (TTBXStatusBar *)GetComponent(fcStatusBar);
  assert(Terminal);

  bool SecurityEnabled = !SessionInfo.SecurityProtocolName.IsEmpty();
  SessionStatusBar->Panels->Items[1]->Enabled = SecurityEnabled;
  // expanded from ?: to avoid memory leaks
  if (SecurityEnabled)
  {
    SessionStatusBar->Panels->Items[1]->Hint =
      FMTLOAD(STATUS_SECURE, (SessionInfo.SecurityProtocolName));
  }
  else
  {
    SessionStatusBar->Panels->Items[1]->Hint = LoadStr(STATUS_INSECURE);
  }

  if (FileSystemInfo.ProtocolName.IsEmpty())
  {
    SessionStatusBar->Panels->Items[2]->Caption = SessionInfo.ProtocolName;
  }
  else
  {
    SessionStatusBar->Panels->Items[2]->Caption = FileSystemInfo.ProtocolName;
  }
  SessionStatusBar->Panels->Items[2]->Hint = LoadStr(STATUS_PROTOCOL_HINT);

  SessionStatusBar->Panels->Items[3]->Enabled =
    (!SessionInfo.CSCompression.IsEmpty() || !SessionInfo.SCCompression.IsEmpty());
  if (SessionInfo.CSCompression == SessionInfo.SCCompression)
  {
    SessionStatusBar->Panels->Items[3]->Hint =
      FMTLOAD(STATUS_COMPRESSION_HINT, (DefaultStr(SessionInfo.CSCompression, LoadStr(NO_STR))));
  }
  else
  {
    SessionStatusBar->Panels->Items[3]->Hint = FMTLOAD(STATUS_COMPRESSION2_HINT,
      (DefaultStr(SessionInfo.CSCompression, LoadStr(NO_STR)),
       DefaultStr(SessionInfo.SCCompression, LoadStr(NO_STR))));
  }

  SessionStatusBar->Panels->Items[4]->Hint = LoadStr(STATUS_DURATION_HINT);

  UpdateStatusBar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusBar()
{
  TTBXStatusBar * SessionStatusBar = (TTBXStatusBar *)GetComponent(fcStatusBar);
  assert(SessionStatusBar != NULL);
  if (FShowStatusBarHint)
  {
    SessionStatusBar->SimplePanel = true;
    SessionStatusBar->SimpleText = FStatusBarHint;
  }
  else if (!Terminal || !Terminal->Active || Terminal->Status < ssOpened)
  {
    // note: (Terminal->Status < sshReady) currently never happens here,
    // so STATUS_CONNECTING is never used
    SessionStatusBar->SimplePanel = true;
    SessionStatusBar->SimpleText = LoadStr(
      !Terminal || !Terminal->Active ? STATUS_DISCONNECTED : STATUS_CONNECTING);
  }
  else
  {
    assert(Terminal);
    SessionStatusBar->SimplePanel = false;
    const TSessionInfo & SessionInfo = Terminal->GetSessionInfo();

    if (FNote.IsEmpty())
    {
      UpdateStatusPanelText(SessionStatusBar->Panels->Items[0]);
    }
    else
    {
      SessionStatusBar->Panels->Items[0]->Caption = FNote;
    }
    SessionStatusBar->Panels->Items[0]->Hint = FNoteHints;

    SessionStatusBar->Panels->Items[4]->Caption =
      FormatDateTimeSpan(Configuration->TimeFormat, Now() - SessionInfo.LoginTime);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusPanelText(TTBXStatusPanel * Panel)
{
  Panel->Caption = "";
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Idle(bool AppIdle)
{
  FEditorManager->Check();

  // make sure that Idle is called before update queue, as it may invoke QueueEvent
  // that needs to know is queue view is visible (and it may be closed after queue update)
  TTerminalManager::Instance()->Idle();

  if (AppIdle)
  {
    if (FRefreshRemoteDirectory)
    {
      if ((Terminal != NULL) && Terminal->Active)
      {
        Terminal->RefreshDirectory();
      }
      FRefreshRemoteDirectory = false;
    }
    if (FRefreshLocalDirectory)
    {
      ReloadLocalDirectory();
      FRefreshLocalDirectory = false;
    }
  }

  if (FQueueStatusInvalidated)
  {
    UpdateQueueStatus(AppIdle);
  }

  RefreshQueueItems(AppIdle);

  UpdateStatusBar();
  FIgnoreNextSysCommand = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UserActionTimer(TObject * /*Sender*/)
{
  try
  {
    FUserActionTimer->Enabled = false;
    if (GUIConfiguration->QueueAutoPopup && (FPendingQueueActionItem != NULL))
    {
      if (TQueueItem::IsUserActionStatus(FPendingQueueActionItem->Status))
      {
        FPendingQueueActionItem->ProcessUserAction();
      }
    }
  }
  __finally
  {
    FPendingQueueActionItem = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationHint(TObject * /*Sender*/)
{
  assert(Application);
  AnsiString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty();
  if (FShowStatusBarHint)
  {
    FStatusBarHint = AHint != "E" ? AHint : AnsiString("");
  }
  else
  {
    FStatusBarHint = "";
  }
  UpdateStatusBar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationMinimize(TObject * /*Sender*/)
{
  if (WinConfiguration->MinimizeToTray)
  {
    UpdateTrayIcon();
    FTrayIcon->Visible = true;
    ShowWindow(Application->Handle, SW_HIDE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationRestore(TObject * /*Sender*/)
{
  if (FTrayIcon->Visible)
  {
    FTrayIcon->Visible = false;
    ShowWindow(Application->Handle, SW_SHOW);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTrayIcon()
{
  FTrayIcon->Hint = Application->Title;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationTitleChanged()
{
  UpdateTrayIcon();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TrayIconClick(TObject * /*Sender*/)
{
  Application->Restore();
  Application->BringToFront();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::OpenInNewWindow()
{
  return FLAGSET(GetAsyncKeyState(VK_SHIFT), 0x8000);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NewSession()
{
  if (OpenInNewWindow())
  {
    if (!ExecuteShell(Application->ExeName, ""))
    {
      throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Application->ExeName)));
    }
  }
  else
  {
    TTerminalManager::Instance()->NewSession();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DuplicateSession()
{
  TSessionData * SessionData = new TSessionData("");
  try
  {
    SessionData->Assign(Terminal->SessionData);
    // current working directories become defaults here, what is not right
    UpdateSessionData(SessionData);

    if (OpenInNewWindow())
    {
      AnsiString SessionName = StoredSessions->HiddenPrefix + "duplicate";
      StoredSessions->NewSession(SessionName, SessionData);
      // modified only, explicit
      StoredSessions->Save(false, true);
      if (!ExecuteShell(Application->ExeName, FORMAT("\"%s\"", (SessionName))))
      {
        throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Application->ExeName)));
      }
    }
    else
    {
      TTerminalManager * Manager = TTerminalManager::Instance();
      TTerminal * Terminal = Manager->NewTerminal(SessionData);
      Manager->ActiveTerminal = Terminal;
      Manager->ConnectActiveTerminal();
    }
  }
  __finally
  {
    delete SessionData;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanCloseQueue()
{
  assert(FQueue != NULL);
  return (FQueue->IsEmpty ||
    (MessageDialog(LoadStr(PENDING_QUEUE_ITEMS), qtWarning, qaOK | qaCancel, HELP_NONE) == qaOK));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseSession()
{
  if (CanCloseQueue())
  {
    TTerminalManager::Instance()->FreeActiveTerminal();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenStoredSession(TSessionData * Data)
{
  if (OpenInNewWindow())
  {
    if (!ExecuteShell(Application->ExeName, FORMAT("\"%s\"", (Data->Name))))
    {
      throw Exception(FMTLOAD(EXECUTE_APP_ERROR, (Application->ExeName)));
    }
  }
  else
  {
    TTerminalManager * Manager = TTerminalManager::Instance();
    TTerminal * Terminal = Manager->NewTerminal(Data);
    Manager->ActiveTerminal = Terminal;
    Manager->ConnectActiveTerminal();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  if (Terminal->Active && WinConfiguration->ConfirmClosingSession)
  {
    int Result;
    TMessageParams Params(mpNeverAskAgainCheck);
    AnsiString Message;
    if (TTerminalManager::Instance()->Count > 1)
    {
      Message = LoadStr(CLOSE_SESSIONS);
    }
    else
    {
      Message = FMTLOAD(CLOSE_SESSION, (Terminal->SessionData->SessionName));
    }
    Result = MessageDialog(Message, qtConfirmation,
      qaOK | qaCancel, HELP_NONE, &Params);

    if (Result == qaNeverAskAgain)
    {
      WinConfiguration->ConfirmClosingSession = false;
    }
    CanClose = (Result == qaOK || Result == qaNeverAskAgain);
  }

  if (CanClose)
  {
    CanClose = CanCloseQueue();
  }

  if (CanClose)
  {
    CanClose = FEditorManager->CloseInternalEditors(CloseInternalEditor) &&
      FEditorManager->CloseExternalFilesWithoutProcess() &&
      (FEditorManager->Empty(true) ||
       (MessageDialog(LoadStr(PENDING_EDITORS), qtWarning, qaIgnore | qaCancel,
          HELP_NONE) == qaIgnore));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseInternalEditor(TObject * Sender)
{
  TForm * Form = dynamic_cast<TForm *>(Sender);
  assert(Form != NULL);
  Form->Close();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ForceCloseInternalEditor(TObject * Sender)
{
  TForm * Form = dynamic_cast<TForm *>(Sender);
  delete Form;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDisplayProperties(
      TObject *Sender)
{
  TStrings *FileList = ((TUnixDirView*)Sender)->CreateFileList(True, False, NULL);
  try
  {
    SetProperties(osRemote, FileList);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ComponentShowing(Word Component, bool value)
{
  if (value)
  {
    if (Component == fcCustomCommandsBand)
    {
      UpdateCustomCommandsToolbar();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetComponentVisible(Word Component, Boolean value)
{
  TControl * Control = GetComponent(Component);
  assert(Control);
  bool Changed = (Control->Visible != value);
  if (Changed)
  {
    ComponentShowing(Component, value);

    TWinControl * WinControl = dynamic_cast<TWinControl*>(Control);
    bool WasFocused = (WinControl != NULL) && (ActiveControl != NULL) &&
      ((ActiveControl == WinControl) || (ActiveControl->Parent == WinControl));
    if (value)
    {
      int RemainingHeight = Control->Parent->ClientHeight;
      int RemainingWidth = Control->Parent->ClientWidth;
      for (int i = 0; i < Control->Parent->ControlCount; i++)
      {
        TControl * ChildControl = Control->Parent->Controls[i];
        if (ChildControl->Visible)
        {
          switch (ChildControl->Align)
          {
            case alTop:
            case alBottom:
              RemainingHeight -= ChildControl->Height;
              break;

            case alLeft:
            case alRight:
              RemainingWidth -= ChildControl->Width;
              break;
          }
        }
      }

      static int Reserve = 32;
      // queue in explorer, trees in commander
      if (Control->Height > RemainingHeight - Reserve)
      {
        Control->Height = RemainingHeight / 2;
      }

      if (Control->Width > RemainingWidth - Reserve)
      {
        Control->Width = RemainingWidth / 2;
      }
    }
    Control->Visible = value;
    if (WasFocused && Visible)
    {
      DirView(osCurrent)->SetFocus();
    }

    FixControlsPlacement();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetComponentVisible(Word Component)
{
  TControl * Control = GetComponent(Component);
  if (Control == NULL)
  {
    return false;
  }
  else
  {
    return Control->Visible;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FixControlsPlacement()
{
  if (RemoteDirView->ItemFocused != NULL)
  {
    RemoteDirView->ItemFocused->MakeVisible(false);
  }
  QueueSplitter->Visible = QueuePanel->Visible;
  RemotePanelSplitter->Visible = RemoteDriveView->Visible;
}
//---------------------------------------------------------------------------
TControl * __fastcall TCustomScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcStatusBar: return RemoteStatusBar;
    case fcRemotePopup: return reinterpret_cast<TControl *>(NonVisualDataModule->RemoteFilePopup);
    case fcQueueView: return QueuePanel;
    case fcQueueToolbar: return QueueDock;
    case fcRemoteTree: return RemoteDriveView;
    default: return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewColumnRightClick(
      TObject *Sender, TListColumn *Column, TPoint &Point)
{
  assert(NonVisualDataModule && Column && Sender);
  NonVisualDataModule->ListColumn = Column;
  TPoint ScreenPoint = ((TControl*)Sender)->ClientToScreen(Point);
  TPopupMenu * DirViewColumnMenu;
  if (Sender == RemoteDirView)
  {
    DirViewColumnMenu = NonVisualDataModule->RemoteDirViewColumnPopup;
    NonVisualDataModule->RemoteSortByExtColumnPopupItem->Visible =
      (Column->Index == uvName);
  }
  else
  {
    DirViewColumnMenu = NonVisualDataModule->LocalDirViewColumnPopup;
    NonVisualDataModule->LocalSortByExtColumnPopupItem->Visible =
      (Column->Index == dvName);
  }
  DirViewColumnMenu->Popup(ScreenPoint.x, ScreenPoint.y);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewExecFile(
      TObject *Sender, TListItem *Item, bool &AllowExec)
{
  DoDirViewExecFile(Sender, Item, AllowExec);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDirViewExecFile(TObject * Sender,
  TListItem * Item, bool & AllowExec)
{
  assert(Sender && Item && Configuration);
  assert(AllowExec);
  TCustomDirView * ADirView = (TCustomDirView *)Sender;
  bool Remote = (ADirView == DirView(osRemote));
  bool ResolvedSymlinks = !Remote || Terminal->ResolvingSymlinks;

  // Anything special is done on double click only (not on "open" indicated by FForceExecution),
  // on files only (not directories)
  // and only when symlinks are resolved (apply to remote panel only)
  if (!ADirView->ItemIsDirectory(Item) &&
      (ResolvedSymlinks || FForceExecution))
  {
    if ((WinConfiguration->DoubleClickAction != dcaOpen) &&
        !FForceExecution &&
        ResolvedSymlinks)
    {
      if (WinConfiguration->DoubleClickAction == dcaCopy)
      {
        ExecuteFileOperation(foCopy,
          (ADirView == DirView(osRemote) ? osRemote : osLocal),
          true, !WinConfiguration->CopyOnDoubleClickConfirmation);
        AllowExec = false;
      }
      else if (WinConfiguration->DoubleClickAction == dcaEdit)
      {
        if (!Remote || !WinConfiguration->DisableOpenEdit)
        {
          ExecuteFile(osCurrent, efDefaultEditor);
          AllowExec = false;
        }
      }
      else
      {
        assert(false);
      }
    }

    // if we have not done anything special, fall back to default behaviour
    if (AllowExec)
    {
      if (Remote && !WinConfiguration->DisableOpenEdit)
      {
        ExecuteFile(osRemote, efShell);
        AllowExec = false;
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetHasDirView(TOperationSide Side)
{
  return ((Side == osRemote) || (Side == osCurrent));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CompareDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoSynchronizeDirectories(
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, bool UseDefaults)
{
  TSynchronizeParamType Params;
  Params.LocalDirectory = LocalDirectory;
  Params.RemoteDirectory = RemoteDirectory;
  int UnusedParams =
    (GUIConfiguration->SynchronizeParams &
      (spPreviewChanges | spTimestamp | spNotByTime | spBySize));
  Params.Params = GUIConfiguration->SynchronizeParams & ~UnusedParams;
  Params.Options = GUIConfiguration->SynchronizeOptions;
  bool SaveSettings = false;
  TSynchronizeController Controller(&DoSynchronize, &DoSynchronizeInvalid,
    &DoSynchronizeTooManyDirectories);
  assert(FSynchronizeController == NULL);
  FSynchronizeController = &Controller;
  bool Result;
  try
  {
    TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
    int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Upload;
    int Options =
      FLAGMASK(SynchronizeAllowSelectedOnly(), soAllowSelectedOnly);
    Result = DoSynchronizeDialog(Params, &CopyParam, Controller.StartStop,
      SaveSettings, Options, CopyParamAttrs, GetSynchronizeOptions, UseDefaults);
    if (Result)
    {
      if (SaveSettings)
      {
        GUIConfiguration->SynchronizeParams = Params.Params | UnusedParams;
        GUIConfiguration->SynchronizeOptions = Params.Options;
      }
      LocalDirectory = Params.LocalDirectory;
      RemoteDirectory = Params.RemoteDirectory;
    }
  }
  __finally
  {
    FSynchronizeController = NULL;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronize(
  TSynchronizeController * /*Sender*/, const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, const TCopyParamType & CopyParam,
  const TSynchronizeParamType & Params, TSynchronizeChecklist ** Checklist,
  TSynchronizeOptions * Options, bool Full)
{
  if (Terminal->Status == ssOpened)
  {
    try
    {
      int PParams = Params.Params;
      if (!Full)
      {
        PParams |= TTerminal::spNoRecurse | TTerminal::spUseCache |
          TTerminal::spDelayProgress | TTerminal::spSubDirs;
      }
      Synchronize(LocalDirectory, RemoteDirectory, smRemote, CopyParam,
        PParams, Checklist, Options);
    }
    catch(Exception & E)
    {
      ShowExtendedExceptionEx(Terminal, &E);
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronizeInvalid(
  TSynchronizeController * /*Sender*/, const AnsiString Directory,
  const AnsiString ErrorStr)
{
  if (!Directory.IsEmpty())
  {
    SimpleErrorDialog(FMTLOAD(WATCH_ERROR_DIRECTORY, (Directory)), ErrorStr);
  }
  else
  {
    SimpleErrorDialog(LoadStr(WATCH_ERROR_GENERAL), ErrorStr);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronizeTooManyDirectories(
  TSynchronizeController * /*Sender*/, int & MaxDirectories)
{
  if (MaxDirectories < GUIConfiguration->MaxWatchDirectories)
  {
    MaxDirectories = GUIConfiguration->MaxWatchDirectories;
  }
  else
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    int Result = MessageDialog(
      FMTLOAD(TOO_MANY_WATCH_DIRECTORIES, (MaxDirectories, MaxDirectories)),
      qtConfirmation, qaYes | qaNo, HELP_TOO_MANY_WATCH_DIRECTORIES, &Params);

    if ((Result == qaYes) || (Result == qaNeverAskAgain))
    {
      MaxDirectories *= 2;
      if (Result == qaNeverAskAgain)
      {
        GUIConfiguration->MaxWatchDirectories = MaxDirectories;
      }
    }
    else
    {
      Abort();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Synchronize(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType & CopyParam, int Params, TSynchronizeChecklist ** Checklist,
  TSynchronizeOptions * Options)
{
  assert(!FAutoOperation);
  void * BatchStorage;
  BatchStart(BatchStorage);
  FAutoOperation = true;

  TSynchronizeChecklist * AChecklist = NULL;
  try
  {
    FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, true);
    if (FLAGCLEAR(Params, TTerminal::spDelayProgress))
    {
      FSynchronizeProgressForm->Start();
    }

    AChecklist = Terminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
      static_cast<TTerminal::TSynchronizeMode>(Mode),
      &CopyParam, Params | spNoConfirmation, TerminalSynchronizeDirectory,
      Options);

    SAFE_DESTROY(FSynchronizeProgressForm);

    FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, false);
    if (FLAGCLEAR(Params, TTerminal::spDelayProgress))
    {
      FSynchronizeProgressForm->Start();
    }

    Terminal->SynchronizeApply(AChecklist, LocalDirectory, RemoteDirectory,
      &CopyParam, Params | spNoConfirmation, TerminalSynchronizeDirectory);
  }
  __finally
  {
    if (Checklist == NULL)
    {
      delete AChecklist;
    }
    else
    {
      *Checklist = AChecklist;
    }

    FAutoOperation = false;
    SAFE_DESTROY(FSynchronizeProgressForm);
    BatchEnd(BatchStorage);
    ReloadLocalDirectory();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::SynchronizeAllowSelectedOnly()
{
  // can be called from command line
  return Visible &&
    ((DirView(osRemote)->SelCount > 0) ||
     (HasDirView[osLocal] && (DirView(osLocal)->SelCount > 0)));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetSynchronizeOptions(
  int Params, TSynchronizeOptions & Options)
{
  if (FLAGSET(Params, spSelectedOnly) && SynchronizeAllowSelectedOnly())
  {
    Options.Filter = new TStringList();
    Options.Filter->CaseSensitive = false;
    Options.Filter->Duplicates = dupAccept;

    if (DirView(osRemote)->SelCount > 0)
    {
      DirView(osRemote)->CreateFileList(false, false, Options.Filter);
    }
    if (HasDirView[osLocal] && (DirView(osLocal)->SelCount > 0))
    {
      DirView(osLocal)->CreateFileList(false, false, Options.Filter);
    }
    Options.Filter->Sort();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoFullSynchronizeDirectories(
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory,
  TSynchronizeMode & Mode, bool & SaveMode, bool UseDefaults)
{
  bool Result;
  int Params = GUIConfiguration->SynchronizeParams;

  bool SaveSettings = false;
  int Options =
    FLAGMASK(!Terminal->IsCapable[fcTimestampChanging], fsoDisableTimestamp) |
    FLAGMASK(SynchronizeAllowSelectedOnly(), fsoAllowSelectedOnly);
  TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  TUsableCopyParamAttrs CopyParamAttrs = Terminal->UsableCopyParamAttrs(0);
  Result = UseDefaults ||
    DoFullSynchronizeDialog(Mode, Params, LocalDirectory, RemoteDirectory,
      &CopyParam, SaveSettings, SaveMode, Options, CopyParamAttrs);
  if (Result)
  {
    TSynchronizeOptions SynchronizeOptions;
    GetSynchronizeOptions(Params, SynchronizeOptions);

    if (SaveSettings)
    {
      GUIConfiguration->SynchronizeParams = Params;
    }
    else
    {
      SaveMode = false;
    }

    TDateTime StartTime = Now();

    TSynchronizeChecklist * Checklist = NULL;
    try
    {
      assert(!FAutoOperation);
      FAutoOperation = true;

      try
      {
        FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, true);
        FSynchronizeProgressForm->Start();

        Checklist = Terminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
          static_cast<TTerminal::TSynchronizeMode>(Mode),
          &CopyParam, Params | spNoConfirmation, TerminalSynchronizeDirectory,
          &SynchronizeOptions);
      }
      __finally
      {
        FAutoOperation = false;
        SAFE_DESTROY(FSynchronizeProgressForm);
      }

      if (Checklist->Count == 0)
      {
        MessageDialog(LoadStr(COMPARE_NO_DIFFERENCES), qtInformation, qaOK,
          HELP_SYNCHRONIZE_NO_DIFFERENCES);
      }
      else if (FLAGCLEAR(Params, spPreviewChanges) ||
               DoSynchronizeChecklistDialog(Checklist, Mode, Params,
                 LocalDirectory, RemoteDirectory))
      {
        assert(!FAutoOperation);
        void * BatchStorage;
        BatchStart(BatchStorage);
        FAutoOperation = true;

        if (FLAGSET(Params, spPreviewChanges))
        {
          StartTime = Now();
        }

        try
        {
          FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, false);
          FSynchronizeProgressForm->Start();

          Terminal->SynchronizeApply(Checklist, LocalDirectory, RemoteDirectory,
            &CopyParam, Params | spNoConfirmation, TerminalSynchronizeDirectory);
        }
        __finally
        {
          FAutoOperation = false;
          SAFE_DESTROY(FSynchronizeProgressForm);
          BatchEnd(BatchStorage);
          ReloadLocalDirectory();
        }
      }
    }
    __finally
    {
      delete Checklist;
    }

    OperationComplete(StartTime);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalSynchronizeDirectory(
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory,
  bool & Continue, bool /*Collect*/)
{
  assert(FSynchronizeProgressForm != NULL);
  if (!FSynchronizeProgressForm->Started)
  {
    FSynchronizeProgressForm->Start();
  }
  FSynchronizeProgressForm->SetData(LocalDirectory, RemoteDirectory, Continue);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExploreLocalDirectory()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveCurrentSession()
{
  TSessionData * SessionData = new TSessionData("");
  try
  {
    SessionData->Assign(Terminal->SessionData);
    UpdateSessionData(SessionData);

    bool SavePassword;
    bool * PSavePassword;

    if (Configuration->DisablePasswordStoring ||
        SessionData->Password.IsEmpty())
    {
      PSavePassword = NULL;
    }
    else
    {
      PSavePassword = &SavePassword;
      SavePassword = false;
    }

    AnsiString SessionName = Terminal->SessionData->SessionName;
    if (DoSaveSessionDialog(SessionName, PSavePassword, NULL))
    {
      if ((PSavePassword != NULL) && !*PSavePassword)
      {
        SessionData->Password = "";
      }

      StoredSessions->NewSession(SessionName, SessionData);
      // modified only, explicit
      StoredSessions->Save(false, true);
    }
  }
  __finally
  {
    delete SessionData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTerminal(TTerminal * Terminal)
{
  TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(FTerminal);
  assert(ManagedTerminal != NULL);

  SAFE_DESTROY(ManagedTerminal->RemoteExplorerState);

  if (WinConfiguration->PreservePanelState)
  {
    ManagedTerminal->RemoteExplorerState = RemoteDirView->SaveState();
  }

  // cannot use RemoteDirView->Path, because it is empty if connection
  // was already closed
  ManagedTerminal->RemoteDirectory = Terminal->CurrentDirectory;
  ManagedTerminal->Color = SessionColor;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  assert(Data != NULL);

  // cannot use RemoteDirView->Path, because it is empty if connection
  // was already closed
  Data->RemoteDirectory = Terminal->CurrentDirectory;
  Data->Color = SessionColor;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolBarResize(TObject *Sender)
{
  TTBXToolbar * Toolbar = dynamic_cast<TTBXToolbar*>(Sender);
  assert(Toolbar != NULL);

  for (int i = 0; i < Toolbar->Items->Count; i++)
  {
    TTBXCustomDropDownItem * DropDownItem;
    DropDownItem = dynamic_cast<TTBXCustomDropDownItem *>(Toolbar->Items->Items[i]);
    if (DropDownItem != NULL)
    {
      ToolbarItemResize(DropDownItem,
        Toolbar->Width - (Toolbar->View->BaseSize.x - DropDownItem->EditWidth) -
        Toolbar->NonClientWidth);
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolbarItemResize(TTBXCustomDropDownItem * Item, int Width)
{
  Item->EditWidth = Width;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolbarGetBaseSize(
  TTBCustomToolbar * Toolbar, TPoint & ASize)
{
  for (int i = 0; i < Toolbar->Items->Count; i++)
  {
    TTBXCustomDropDownItem * DropDownItem;
    DropDownItem = dynamic_cast<TTBXCustomDropDownItem *>(Toolbar->Items->Items[i]);
    if (DropDownItem != NULL)
    {
      ASize.x -= DropDownItem->EditWidth;
      ASize.x += 50 /* minimal combo width */;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoWarnLackOfTempSpace(
  const AnsiString Path, __int64 RequiredSpace, bool & Continue)
{
  if (WinConfiguration->DDWarnLackOfTempSpace)
  {
    AnsiString ADrive = ExtractFileDrive(ExpandFileName(Path));
    if (!ADrive.IsEmpty())
    {
      __int64 FreeSpace = DiskFree((Byte)(ADrive[1]-'A'+1));
      assert(RequiredSpace >= 0);
      __int64 RequiredWithReserve;
      RequiredWithReserve = (__int64)(RequiredSpace * WinConfiguration->DDWarnLackOfTempSpaceRatio);
      if (FreeSpace < RequiredWithReserve)
      {
        int Result;
        TMessageParams Params(mpNeverAskAgainCheck);
        Result = MessageDialog(FMTLOAD(DD_WARN_LACK_OF_TEMP_SPACE, (Path,
          FormatBytes(FreeSpace), FormatBytes(RequiredSpace))),
          qtWarning, qaYes | qaNo, HELP_DD_WARN_LACK_OF_TEMP_SPACE, &Params);

        if (Result == qaNeverAskAgain)
        {
          WinConfiguration->DDWarnLackOfTempSpace = false;
        }

        Continue = (Result == qaYes || Result == qaNeverAskAgain);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddBookmark(TOperationSide Side)
{
  DoOpenDirectoryDialog(odAddBookmark, Side);
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCustomScpExplorerForm::CreateVisitedDirectories(TOperationSide Side)
{
  TStringList * VisitedDirectories = new TStringList();
  try
  {
    TCustomDirView * DView = DirView(Side);

    VisitedDirectories->Duplicates = dupIgnore;
    // we should better use TCustomDirView::FCaseSensitive, but it is private
    VisitedDirectories->CaseSensitive = (Side == osRemote);
    VisitedDirectories->Sorted = true;

    for (int Index = -DView->BackCount; Index <= DView->ForwardCount; Index++)
    {
      VisitedDirectories->Add(DView->HistoryPath[Index]);
    }
  }
  catch (...)
  {
    delete VisitedDirectories;
    throw;
  }
  return VisitedDirectories;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOpenDirectoryDialog(
  TOpenDirectoryMode Mode, TOperationSide Side)
{
  if (Mode != odAddBookmark ||
      (MessageDialog(FMTLOAD(ADD_BOOKMARK_CONFIRM, (DirView(Side)->PathName)),
        qtConfirmation, qaYes | qaNo, HELP_ADD_BOOKMARK_CONFIRM) == qaYes))
  {
    TStrings * VisitedDirectories = CreateVisitedDirectories(Side);
    try
    {
      AnsiString Name = DirView(Side)->PathName;
      if (::DoOpenDirectoryDialog(Mode, Side, Name, VisitedDirectories, Terminal,
            HasDirView[osLocal]))
      {
        DirView(Side)->Path = Name;
      }
    }
    __finally
    {
      delete VisitedDirectories;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CommandSessionFallback()
{
  bool Result = true;

  assert(!FTerminal->CommandSessionOpened);

  try
  {
    TTerminalManager::ConnectTerminal(FTerminal->CommandSession, false);
  }
  catch(Exception & E)
  {
    ShowExtendedExceptionEx(FTerminal->CommandSession, &E);
    Result = false;
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::EnsureCommandSessionFallback(TFSCapability Capability)
{
  bool Result = FTerminal->IsCapable[Capability] ||
    FTerminal->CommandSessionOpened;

  if (!Result)
  {
    assert(FTerminal->IsCapable[fcSecondaryShell]);
    if (!GUIConfiguration->ConfirmCommandSession)
    {
      Result = true;
    }
    else
    {
      TMessageParams Params(mpNeverAskAgainCheck);
      const TFileSystemInfo & FileSystemInfo = Terminal->GetFileSystemInfo();
      int Answer = MessageDialog(FMTLOAD(PERFORM_ON_COMMAND_SESSION,
        (FileSystemInfo.ProtocolName, FileSystemInfo.ProtocolName)), qtConfirmation,
        qaOK | qaCancel, HELP_PERFORM_ON_COMMAND_SESSION, &Params);
      if (Answer == qaNeverAskAgain)
      {
        GUIConfiguration->ConfirmCommandSession = false;
        Result = true;
      }
      else if (Answer == qaOK)
      {
        Result = true;
      }
    }

    if (Result)
    {
      Result = CommandSessionFallback();
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenConsole(AnsiString Command)
{
  if (EnsureCommandSessionFallback(fcAnyCommand))
  {
    DoConsoleDialog(Terminal, Command);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileControlDDDragEnter(
      TObject *Sender, _di_IDataObject /*DataObj*/, int /*grfKeyState*/,
      const TPoint & /*Point*/, int & /*dwEffect*/, bool & Accept)
{
  if (IsFileControl(DropSourceControl, osRemote) &&
      (FDDExtMapFile != NULL))
  {
    Accept = true;
  }

  FDDTargetControl = dynamic_cast<TControl*>(Sender);
  assert(FDDTargetControl != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileControlDDDragLeave(
      TObject *Sender)
{
  USEDPARAM(Sender);
  assert(FDDTargetControl == Sender);
  FDDTargetControl = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddEditLink(bool Add)
{
  assert(FCurrentSide == osRemote);

  bool Edit = false;
  TRemoteFile * File = NULL;
  AnsiString FileName;
  AnsiString PointTo;
  bool SymbolicLink = true;

  if (RemoteDirView->ItemFocused)
  {
    assert(RemoteDirView->ItemFocused->Data);
    File = (TRemoteFile *)RemoteDirView->ItemFocused->Data;

    Edit = !Add && File->IsSymLink && Terminal->SessionData->ResolveSymlinks;
    if (Edit)
    {
      FileName = File->FileName;
      PointTo = File->LinkTo;
    }
    else
    {
      PointTo = File->FileName;
    }
  }

  if (DoSymlinkDialog(FileName, PointTo, osRemote, SymbolicLink, Edit,
        Terminal->IsCapable[fcHardLink]))
  {
    if (Edit)
    {
      assert(File->FileName == FileName);
      int Params = dfNoRecursive;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->DeleteFile("", File, &Params);
      }
      __finally
      {
        Terminal->ExceptionOnFail = false;
      }
    }
    Terminal->CreateLink(FileName, PointTo, SymbolicLink);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanAddEditLink()
{
  return
    (Terminal != NULL) &&
    ((DirView(osCurrent) != DirView(osRemote)) ||
     (Terminal->ResolvingSymlinks &&
      Terminal->IsCapable[fcSymbolicLink]));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::LinkFocused()
{
  return
    (FCurrentSide == osRemote) &&
    (RemoteDirView->ItemFocused != NULL) &&
    ((TRemoteFile *)RemoteDirView->ItemFocused->Data)->IsSymLink &&
    Terminal->SessionData->ResolveSymlinks;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteCurrentFile()
{
  assert(!WinConfiguration->DisableOpenEdit);
  FForceExecution = true;
  try
  {
    DirView(osCurrent)->ExecuteCurrentFile();
  }
  __finally
  {
    FForceExecution = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalClosed(TObject * Sender)
{
  FEditorManager->ProcessFiles(FileTerminalClosed, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileTerminalClosed(const AnsiString FileName,
  TEditedFileData & Data, TObject * /*Token*/, void * Arg)
{
  TTerminal * Terminal = static_cast<TTerminal *>(Arg);
  assert(Terminal != NULL);

  if (Data.Terminal == Terminal)
  {
    Data.Terminal = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LastTerminalClosed(TObject * /*Sender*/)
{
  try
  {
    TTerminalManager::Instance()->NewSession();
  }
  __finally
  {
    if (!Terminal || !Terminal->Active)
    {
      Application->Terminate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalListChanged(TObject * /*Sender*/)
{
  TTBXComboBoxItem * SessionCombo = dynamic_cast<TTBXComboBoxItem*>(
    static_cast<TComponent*>(GetComponent(fcSessionCombo)));
  if (SessionCombo != NULL)
  {
    SessionCombo->Strings = TTerminalManager::Instance()->TerminalList;
    SessionCombo->ItemIndex = TTerminalManager::Instance()->ActiveTerminalIndex;
  }
  if ((TTerminalManager::Instance()->ActiveCount >= 2) &&
      !WinConfiguration->SessionToolbarAutoShown &&
      !ComponentVisible[fcSessionToolbar])
  {
    ComponentVisible[fcSessionToolbar] = true;
    WinConfiguration->SessionToolbarAutoShown = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboPopup(TTBCustomItem * Sender,
  bool /*FromLink*/)
{
  TTBXComboBoxItem * SessionCombo = dynamic_cast<TTBXComboBoxItem *>(Sender);
  assert(SessionCombo);

  HDC DC = GetDC(0);
  TCanvas * Canvas = new TCanvas();
  try
  {
    Canvas->Handle = DC;
    Canvas->Font = ToolbarFont;

    int MaxWidth = 0, Width;
    for (int i = 0; i < SessionCombo->Strings->Count; i++)
    {
      Width = Canvas->TextExtent(EscapeHotkey(SessionCombo->Strings->Strings[i])).cx;
      TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(i);
      if (ShortCut != scNone)
      {
        Width += Canvas->TextExtent(ShortCutToText(ShortCut) + " ").cx;
      }
      if (Width > MaxWidth)
      {
        MaxWidth = Width;
      }
    }
    if (SessionCombo->Strings->Count > SessionCombo->MaxVisibleItems)
    {
      MaxWidth += GetSystemMetrics(SM_CXVSCROLL);
    }
    SessionCombo->MinListWidth = MaxWidth + 8 + 8 + 1;
  }
  __finally
  {
    Canvas->Handle = NULL;
    ReleaseDC(0, DC);
    delete Canvas;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboDrawItem(
  TTBXCustomList * Sender, TCanvas * Canvas, const TRect & ARect, int Index,
  int AHoverIndex, bool & DrawDefault)
{
  TTBXStringList * SessionCombo = dynamic_cast<TTBXStringList *>(Sender);
  assert(SessionCombo);

  if (Index >= 0)
  {
    int ShortCutWidth = 0;
    AnsiString ShortCutStr;
    TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(Index);
    if (ShortCut != scNone)
    {
      ShortCutStr = " " + ShortCutToText(ShortCut);
      ShortCutWidth = Canvas->TextExtent(ShortCutStr).cx;
    }

    if (Index != AHoverIndex)
    {
      TManagedTerminal * Terminal =
        dynamic_cast<TManagedTerminal *>(SessionCombo->Strings->Objects[Index]);
      assert(Terminal != NULL);

      TColor Color =
        (Terminal == this->Terminal ? SessionColor : Terminal->Color);
      if (Color != 0)
      {
        Canvas->Brush->Color = Color;
        Canvas->Brush->Style = bsSolid;
        Canvas->FillRect(ARect);
      }
    }

    TRect R = ARect;
    InflateRect(&R, -4, 1);
    R.Right -= ShortCutWidth + 2;
    AnsiString S = EscapeHotkey(SessionCombo->Strings->Strings[Index]);
    DrawText(Canvas->Handle, S.c_str(), S.Length(), &R,
      DT_SINGLELINE | DT_VCENTER | DT_NOPREFIX);
    R = ARect;
    InflateRect(&R, -4, 1);
    R.Left = R.Right - ShortCutWidth;
    DrawText(Canvas->Handle, ShortCutStr.c_str(), ShortCutStr.Length(), &R,
      DT_SINGLELINE | DT_VCENTER | DT_NOPREFIX);
  }
  DrawDefault = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboChange(TObject * Sender,
  const AnsiString Text)
{
  TTBXComboBoxItem * SessionCombo = dynamic_cast<TTBXComboBoxItem *>(Sender);
  assert(SessionCombo);
  TTerminal * Terminal;
  Terminal = dynamic_cast<TTerminal *>(SessionCombo->Strings->Objects[SessionCombo->ItemIndex]);
  assert(Terminal);
  TTerminalManager::Instance()->ActiveTerminal = Terminal;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferListChange(TObject * Sender)
{
  TTBXStringList * TransferList = dynamic_cast<TTBXStringList *>(Sender);
  assert(TransferList != NULL);
  AnsiString Name;
  if (TransferList->ItemIndex <= 0)
  {
    Name = "";
  }
  else
  {
    Name = GUIConfiguration->CopyParamList->Names[TransferList->ItemIndex - 1];
  }
  if (FCopyParamAutoSelected.IsEmpty())
  {
    // if previous preset was not autoselected, make new preset the "default"
    FCopyParamDefault = Name;
  }
  GUIConfiguration->CopyParamCurrent = Name;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTransferLabel()
{
  // sanity check
  bool ExistingPreset =
    (FTransferListHoverIndex >= 0) &&
    (FTransferListHoverIndex < 1 + GUIConfiguration->CopyParamList->Count);
  assert(ExistingPreset);
  if (ExistingPreset)
  {
    HDC DC = GetDC(0);
    TCanvas * Canvas = new TCanvas();
    try
    {
      Canvas->Handle = DC;
      Canvas->Font = ToolbarFont;

      AnsiString Name;
      if (FTransferListHoverIndex == 0)
      {
        Name = "";
      }
      else
      {
        Name = GUIConfiguration->CopyParamList->Names[FTransferListHoverIndex - 1];
      }

      TTBXLabelItem * TransferLabel = dynamic_cast<TTBXLabelItem*>(
        static_cast<TComponent*>(GetComponent(fcTransferLabel)));
      TTBXStringList * TransferList = dynamic_cast<TTBXStringList*>(
        static_cast<TObject*>(GetComponent(fcTransferList)));

      AnsiString InfoStr =
        GUIConfiguration->CopyParamPreset[Name].
          GetInfoStr("; ",
            FLAGMASK(Terminal != NULL, Terminal->UsableCopyParamAttrs(0).General));
      int MaxWidth = TransferList->MinWidth - (2 * TransferLabel->Margin) - 10;
      if (Canvas->TextExtent(InfoStr).cx > MaxWidth)
      {
        AnsiString Ellipsis = "...";
        while (Canvas->TextExtent(InfoStr + Ellipsis).cx > MaxWidth)
        {
          InfoStr.SetLength(InfoStr.Length() - 1);
        }
        InfoStr += Ellipsis;
      }

      // UpdateCaption does not cause invalidation of whole submenu, while
      // setting Caption property does.
      // also it probably does not resize the label, even if necessary
      // (we do not want that anyway)
      TransferLabel->UpdateCaption(InfoStr);
    }
    __finally
    {
      Canvas->Handle = NULL;
      ReleaseDC(0, DC);
      delete Canvas;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferListDrawItem(
  TTBXCustomList * /*Sender*/, TCanvas * /*ACanvas*/, const TRect & /*ARect*/,
  int /*AIndex*/, int AHoverIndex, bool & /*DrawDefault*/)
{
  if (FTransferListHoverIndex != AHoverIndex)
  {
    FTransferListHoverIndex = AHoverIndex;
    UpdateTransferLabel();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMAppCommand(TMessage & Message)
{
  int Command  = GET_APPCOMMAND_LPARAM(Message.LParam);
  TShiftState Shift = KeyDataToShiftState(GET_KEYSTATE_LPARAM(Message.LParam));
  if ((Shift * (TShiftState() << ssShift << ssAlt << ssCtrl)).Empty())
  {
    if (Command == _APPCOMMAND_BROWSER_FAVORITES)
    {
      OpenDirectory(GetSide(osCurrent));
      Message.Result = 1;
    }
    else
    {
      TForm::Dispatch(&Message);
    }
  }
  else
  {
    TForm::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CMAppSysCommand(TMessage & Message)
{
  TWMSysCommand * SysCommand = (TWMSysCommand *)Message.LParam;
  if (SysCommand->CmdType != SC_KEYMENU || !FIgnoreNextSysCommand)
  {
    FIgnoreNextSysCommand = false;
    TForm::Dispatch(&Message);
  }
  else
  {
    Message.Result = 1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMSysCommand(TMessage & Message)
{
  // The four low-order bits are used internally by Windows
  unsigned int Cmd = (Message.WParam & 0xFFF0);
  // SC_RESTORE, SC_MAXIMIZE, SC_MINIMIZE - buttons on windows title
  // SC_DEFAULT - double click on windows title (does not work, at least on WinXP)
  // 61730 - restore thru double click - undocumented
  // 61490 - maximize thru double click - undocumented
  if ((Cmd == SC_RESTORE) || (Cmd == SC_MAXIMIZE) ||
      (Cmd == SC_MINIMIZE) || (Cmd == SC_DEFAULT))
  {
    SysResizing(Cmd);
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMWindowPosChanging(TWMWindowPosMsg & Message)
{
  // MSVDM (not sure if it is generic feature) sets size of all windows
  // to iconic and moves top-level windows (with destop as parent) out
  // of the screen (-32000:-32000). However other windows (such as
  // VCL main window with hidden parent) are moved just above
  // the task bar. Override this.
  if (FLAGCLEAR(Message.WindowPos->flags, SWP_NOMOVE) &&
      (Message.WindowPos->cx == GetSystemMetrics(SM_CXMINIMIZED)) &&
      (Message.WindowPos->cy == GetSystemMetrics(SM_CYMINIMIZED)))
  {
    Message.WindowPos->x = OutPos;
    Message.WindowPos->y = OutPos;
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SysResizing(unsigned int /*Cmd*/)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoShow()
{
  // only now are the controls resized finally, so the size constraints
  // will not conflict with possibly very small window size
  RestoreFormParams();

  FixControlsPlacement();

  if (Position == poDefaultPosOnly)
  {
    CutFormToDesktop(this);
  }

  TForm::DoShow();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PopupTrayBalloon(TTerminal * Terminal,
  const AnsiString & Str, TQueryType Type, Exception * E, unsigned int Seconds)
{
  if (WinConfiguration->BalloonNotifications &&
      (dynamic_cast<EAbort*>(E) == NULL))
  {
    AnsiString Message;
    if (E == NULL)
    {
      Message = Str;
    }
    else
    {
      assert(Str.IsEmpty());
      Message = E->Message;
    }

    if (!Message.IsEmpty())
    {
      const ResourceString * Captions[] = { &_SMsgDlgConfirm, &_SMsgDlgWarning,
        &_SMsgDlgError, &_SMsgDlgInformation, NULL };
      AnsiString Title = LoadResourceString(Captions[Type]);
      if (Terminal != NULL)
      {
        Title = FORMAT("%s - %s",
          (TTerminalManager::Instance()->TerminalTitle(Terminal), Title));
      }

      if (Seconds == 0)
      {
        Seconds = WinConfiguration->NotificationsTimeout;
      }
      FTrayIcon->PopupBalloon(Title, Message, Type, Seconds * 1000);
    }
  }
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::MoreMessageDialog(const AnsiString Message,
    TStrings * MoreMessages, TQueryType Type, int Answers,
    AnsiString HelpKeyword, const TMessageParams * Params,
    TTerminal * Terminal)
{
  if (WinConfiguration->ContinueOnError &&
      (Params != NULL) && (Params->Params & mpAllowContinueOnError) &&
      FErrorList)
  {
    TStringList * MoreMessagesCopy = NULL;
    if (MoreMessages)
    {
      MoreMessagesCopy = new TStringList();
      MoreMessagesCopy->Assign(MoreMessages);
    }
    FErrorList->AddObject(Message, MoreMessagesCopy);
    if (Answers & qaSkip) return qaSkip;
      else
    if (Answers & qaIgnore) return qaIgnore;
      else
    if (Answers & qaOK) return qaOK;
      else
    if (Answers & qaYes) return qaYes;
      else
    if (Answers & qaRetry) return qaRetry;
      else
    {
      assert(false);
      return qaYes;
    }
  }
  else
  {
    if (FTrayIcon->Visible)
    {
      PopupTrayBalloon(Terminal, Message, Type);
    }
    int Result;
    try
    {
      Result = ::MoreMessageDialog(Message, MoreMessages, Type, Answers, HelpKeyword, Params);
    }
    __finally
    {
      FTrayIcon->CancelBalloon();
    }
    return Result;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ShowExtendedException(
  TTerminal * Terminal, Exception * E)
{
  if (FTrayIcon->Visible)
  {
    PopupTrayBalloon(Terminal, "", qtError, E);
  }

  try
  {
    ShowExtendedExceptionEx(Terminal, E);
  }
  __finally
  {
    FTrayIcon->CancelBalloon();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InactiveTerminalException(
  TTerminal * Terminal, Exception * E)
{
  Notify(Terminal, "", qtError, false, NULL, E);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Notify(TTerminal * Terminal,
  AnsiString Message, TQueryType Type,
  bool Important, TNotifyEvent OnClick, Exception * E)
{
  if (dynamic_cast<EAbort*>(E) == NULL)
  {
    unsigned int Seconds = WinConfiguration->NotificationsTimeout;
    if (Important)
    {
      Seconds *= 5;
    }

    if (E != NULL)
    {
      assert(Message.IsEmpty());
      Message = E->Message;
    }

    AnsiString NoteMessage(Message);
    if (Terminal != NULL)
    {
      NoteMessage = FORMAT("%s: %s",
        (TTerminalManager::Instance()->TerminalTitle(Terminal), NoteMessage));
    }

    if (WinConfiguration->BalloonNotifications &&
        TTrayIcon::SupportsBalloons())
    {
      AddNote(NoteMessage);
      PopupTrayBalloon(Terminal, Message, Type, NULL, Seconds);
    }
    else
    {
      FlashOnBackground();
      PostNote(NoteMessage, Seconds, OnClick, NULL);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueEvent(TTerminal * ATerminal,
  TTerminalQueue * /*Queue*/, TQueueEvent Event)
{
  AnsiString Message;
  switch (Event)
  {
    case qeEmpty:
      if ((ATerminal != Terminal) || !ComponentVisible[fcQueueView])
      {
        Message = LoadStr(BALLOON_QUEUE_EMPTY);
      }
      break;

    case qePendingUserAction:
      if ((ATerminal != Terminal) ||
          (!ComponentVisible[fcQueueView] && !GUIConfiguration->QueueAutoPopup))
      {
        Message = LoadStr(BALLOON_QUEUE_USER_ACTION);
      }
      break;

    default:
      assert(false);
  }

  if (!Message.IsEmpty())
  {
    Notify(ATerminal, Message, qtInformation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDCreateDragFileList(
  TObject * /*Sender*/, TFileList * FileList, bool & Created)
{
  if (FDDExtMapFile != NULL)
  {
    CloseHandle(FDDExtMapFile);
    FDDExtMapFile = NULL;
  }

  if (WinConfiguration->DDExtEnabled)
  {
    if (!WinConfiguration->DDExtInstalled)
    {
      throw Exception(LoadStr(DRAGEXT_TARGET_NOT_INSTALLED));
    }
    DDExtInitDrag(FileList, Created);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDExtInitDrag(TFileList * FileList,
  bool & Created)
{
  FDragExtFakeDirectory =
    ExcludeTrailingBackslash(WinConfiguration->TemporaryDir());
  if (!ForceDirectories(FDragExtFakeDirectory))
  {
    throw Exception(FMTLOAD(CREATE_TEMP_DIR_ERROR, (FDragExtFakeDirectory)));
  }
  FileList->AddItem(NULL, FDragExtFakeDirectory);

  Created = true;

  FDDExtMapFile = CreateFileMapping((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TDragExtCommStruct), DRAG_EXT_MAPPING);

  {
    TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
    TDragExtCommStruct* CommStruct;
    CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
      FILE_MAP_ALL_ACCESS, 0, 0, 0));
    assert(CommStruct != NULL);
    CommStruct->Version = TDragExtCommStruct::CurrentVersion;
    CommStruct->Dragging = true;
    strncpy(CommStruct->DropDest, FDragExtFakeDirectory.c_str(),
      sizeof(CommStruct->DropDest));
    CommStruct->DropDest[sizeof(CommStruct->DropDest) - 1] = '\0';
    UnmapViewOfFile(CommStruct);
  }

  FDDMoveSlipped = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlFileOperation(
  TObject * Sender, TFileOperation Operation, bool NoConfirmation, void * Param)
{
  if (Sender == RemoteDirView)
  {
    ExecuteFileOperation(Operation, osRemote, true, NoConfirmation, Param);
  }
  else
  {
    assert(Sender == RemoteDriveView);
    TStrings * FileList = RemoteDriveView->DragFileList();
    try
    {
      ExecuteFileOperation(Operation, osRemote, FileList, NoConfirmation, Param);
    }
    __finally
    {
      delete FileList;
    }
  }

  if (FDDTargetControl == RemoteDriveView)
  {
    RemoteDriveView->UpdateDropTarget();
  }
  // foRemoteMove happens when file/dir is dragged within the remote tree view
  // or from tree view to dir view.
  // foMove happens when file/dir is dragged from remote tree view outside of
  // application via dragex
  if (((Operation == foRemoteMove) || (Operation == foMove)) &&
      (DropSourceControl == RemoteDriveView))
  {
    RemoteDriveView->UpdateDropSource();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDEnd(TObject * Sender)
{
  SAFE_DESTROY(FDDFileList);

  if (FDDExtMapFile != NULL)
  {
    try
    {
      TDragResult DDResult = (Sender == RemoteDirView) ?
        RemoteDirView->LastDDResult : RemoteDriveView->LastDDResult;

      if ((DDResult == drCopy) || (DDResult == drMove))
      {
        AnsiString TargetDirectory;
        TFileOperation Operation;

        Operation = (DDResult == drMove) ? foMove : foCopy;

        if (FDDMoveSlipped)
        {
          Operation = foMove;
        }

        TTransferOperationParam Param;
        DDGetTarget(Param.TargetDirectory);
        // download using ddext
        Param.Temp = false;
        Param.DragDrop = true;

        RemoteFileControlFileOperation(Sender, Operation,
          !WinConfiguration->DDTransferConfirmation, &Param);
      }
    }
    __finally
    {
      CloseHandle(FDDExtMapFile);
      FDDExtMapFile = NULL;
      RemoveDir(FDragExtFakeDirectory);
      FDragExtFakeDirectory = "";
    }
  }

  if (!FDragDropSshTerminate.IsEmpty())
  {
    throw ESshTerminate(NULL, FDragDropSshTerminate);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDGiveFeedback(
  TObject * Sender, int dwEffect, HRESULT & /*Result*/)
{
  HCURSOR SlippedCopyCursor;

  FDDMoveSlipped =
    (FDragMoveCursor != NULL) &&
    (!WinConfiguration->DDAllowMoveInit) && (dwEffect == DROPEFFECT_Copy) &&
    ((IsFileControl(FDDTargetControl, osRemote) && (GetKeyState(VK_CONTROL) >= 0) &&
      FTerminal->IsCapable[fcRemoteMove]) ||
     (IsFileControl(FDDTargetControl, osLocal) && (GetKeyState(VK_SHIFT) < 0)));

  SlippedCopyCursor = FDDMoveSlipped ? FDragMoveCursor : Dragdrop::DefaultCursor;

  DragDropFiles(Sender)->CHCopy = SlippedCopyCursor;
  DragDropFiles(Sender)->CHScrollCopy = SlippedCopyCursor;

  // Remember drop effect so we know (when user dropes files), if we copy or move
  FLastDropEffect = dwEffect;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDGetTarget(AnsiString & Directory)
{
  bool Result = false;

  Enabled = false;
  try
  {
    int Timer = 0;
    while (!Result && (Timer < WinConfiguration->DDExtTimeout))
    {
      {
        TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
        TDragExtCommStruct* CommStruct;
        CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
          FILE_MAP_ALL_ACCESS, 0, 0, 0));
        assert(CommStruct != NULL);
        Result = !CommStruct->Dragging;
        if (Result)
        {
          Directory = ExtractFilePath(CommStruct->DropDest);
        }
        UnmapViewOfFile(CommStruct);
      }
      if (!Result)
      {
        Sleep(50);
        Timer += 50;
        Application->ProcessMessages();
      }
    }
  }
  __finally
  {
    Enabled = true;
  }

  if (!Result)
  {
    throw Exception(LoadStr(DRAGEXT_TARGET_UNKNOWN));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddDelayedDirectoryDeletion(
  const AnsiString TempDir, int SecDelay)
{
  TDateTime Alarm = Now() + (double)SecDelay*(double(1)/24/60/60);
  FDelayedDeletionList->AddObject(TempDir, reinterpret_cast<TObject*>(Alarm.FileDate()));
  if (FDelayedDeletionTimer == NULL)
  {
    assert(HandleAllocated());
    FDelayedDeletionTimer = new TTimer(this);
    FDelayedDeletionTimer->Interval = 10000;
    FDelayedDeletionTimer->OnTimer = DoDelayedDeletion;
  }
  else
  {
    FDelayedDeletionTimer->Enabled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDelayedDeletion(TObject * Sender)
{
  assert(FDelayedDeletionList != NULL);

  TDateTime N = Now();
  TDateTime Alert;
  AnsiString Directory;

  for (int Index = FDelayedDeletionList->Count-1; Index >= 0; Index--)
  {
    Alert = FileDateToDateTime(reinterpret_cast<int>(FDelayedDeletionList->Objects[Index]));
    if ((N >= Alert) || (Sender == NULL))
    {
      Directory = FDelayedDeletionList->Strings[Index];
      if (DeleteDirectory(ExcludeTrailingBackslash(Directory)))
      {
        FDelayedDeletionList->Delete(Index);
      }
    }
  }

  if (FDelayedDeletionList->Count == 0)
  {
    FDelayedDeletionTimer->Enabled = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDTargetDrop()
{
  if (IsFileControl(FDDTargetControl, osRemote))
  {
    TTransferOperationParam Param;
    Param.DragDrop = true;
    // when move from remote side is disabled, we allow coying inside the remote
    // panel, but we interpret is as moving (we also slip in the move cursor)
    if ((FLastDropEffect == DROPEFFECT_MOVE) ||
        (!WinConfiguration->DDAllowMoveInit && (FLastDropEffect == DROPEFFECT_COPY) &&
         FDDMoveSlipped))
    {
      RemoteFileControlFileOperation(DropSourceControl,
        foRemoteMove, !WinConfiguration->DDTransferConfirmation, &Param);
    }
    else if (FLastDropEffect == DROPEFFECT_COPY)
    {
      RemoteFileControlFileOperation(DropSourceControl,
        foRemoteCopy, !WinConfiguration->DDTransferConfirmation, &Param);
    }
    // abort drag&drop
    Abort();
  }
  else if ((FDDExtMapFile == NULL) && (FLastDropEffect != DROPEFFECT_NONE))
  {
    assert(!FDragTempDir.IsEmpty());
    TTransferType Type;
    AnsiString TempDir = FDragTempDir;
    // We clear FDragTempDir before calling
    // just in case it fail (raises exception)
    FDragTempDir = "";
    Type = (FLastDropEffect & DROPEFFECT_MOVE ? ttMove : Type = ttCopy);

    TGUICopyParamType CopyParams = GUIConfiguration->CurrentCopyParam;
    // empty directory parameter means temp directory -> don't display it!
    AnsiString TargetDir = "";

    if (!CopyParamDialog(tdToLocal, Type, true, FDDFileList,
          TargetDir, CopyParams, WinConfiguration->DDTransferConfirmation, true))
    {
      Abort();
    }

    // TargetDir is set when dropped on local file control
    // (this was workaround for legacy dirview event handling, now it should be
    // made prettier)
    if (TargetDir.IsEmpty())
    {
      TargetDir = TempDir;

      if (ForceDirectories(TargetDir))
      {
        assert(Terminal && !TargetDir.IsEmpty());
        // do not attempt to warn unless we know total transfer size
        FPendingTempSpaceWarn = CopyParams.CalculateSize;
        try
        {
          FDragDropOperation = true;
          // cpNewerOnly has no efect here,
          // as we download to empty temp directory
          int Params = cpTemporary |
            (Type == ttMove ? cpDelete : 0);
          DDDownload(FDDFileList, TargetDir, &CopyParams, Params);
        }
        __finally
        {
          FDragDropOperation = false;
          FPendingTempSpaceWarn = false;
          AddDelayedDirectoryDeletion(TargetDir, WinConfiguration->DDDeleteDelay);
        }
      }
      else
      {
        throw Exception(FMTLOAD(CREATE_TEMP_DIR_ERROR, (TargetDir)));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDDownload(TStrings * FilesToCopy,
  const AnsiString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  Terminal->CopyToLocal(FilesToCopy, TargetDir, CopyParam, Params);
  if (FLAGSET(Params, cpDelete) && (DropSourceControl == RemoteDriveView))
  {
    RemoteDriveView->UpdateDropSource();
  }
}
//---------------------------------------------------------------------------
class TFakeDataObjectFilesEx : public TDataObjectFilesEx
{
public:
        __fastcall TFakeDataObjectFilesEx(TFileList * AFileList, bool RenderPIDL,
    bool RenderFilename) : TDataObjectFilesEx(AFileList, RenderPIDL, RenderFilename)
  {
  }

  virtual bool __fastcall AllowData(const tagFORMATETC & FormatEtc)
  {
    return (FormatEtc.cfFormat == CF_HDROP) ? false :
      TDataObjectFilesEx::AllowData(FormatEtc);
  }
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDCreateDataObject(
  TObject * Sender, TDataObject *& DataObject)
{
  if (FDDExtMapFile != NULL)
  {
    TFileList * FileList = DragDropFiles(Sender)->FileList;
    if (!FileList->RenderPIDLs() || !FileList->RenderNames())
    {
      Abort();
    }

    if (FileList->Count > 0)
    {
      TDataObjectFilesEx * FilesObject = new TFakeDataObjectFilesEx(FileList, true, true);
      if (!FilesObject->IsValid(true, true))
      {
        FilesObject->_Release();
      }
      else
      {
        DataObject = FilesObject;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GoToCommandLine()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GoToTree()
{
  ComponentVisible[fcRemoteTree] = true;
  RemoteDriveView->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExport(TOperationSide Side,
  TPanelExport Export, TPanelExportDestination Destination, bool OnFocused)
{
  Side = GetSide(Side);

  TCustomDirView * DirView = this->DirView(Side);
  TStringList * ExportData = new TStringList();
  try
  {
    switch (Export)
    {
      case pePath:
        ExportData->Add(DirView->PathName);
        break;

      case peFileList:
      case peFullFileList:
        {
          bool FullPath = (Export == peFullFileList);
          DirView->CreateFileList(OnFocused, FullPath, ExportData);
          AnsiString FileName;
          for (int Index = 0; Index < ExportData->Count; Index++)
          {
            if (ExportData->Strings[Index].Pos(" ") > 0)
            {
              ExportData->Strings[Index] = FORMAT("\"%s\"", (ExportData->Strings[Index]));
            }
          }
        }
        break;

      case peUrl:
        {
          assert(DirView == RemoteDirView);
          DirView->CreateFileList(OnFocused, true, ExportData);
          for (int Index = 0; Index < ExportData->Count; Index++)
          {
            ExportData->Strings[Index] =
              FTerminal->FileUrl(ExportData->Strings[Index]);
          }
        }
        break;

      default:
        assert(false);
    }

    PanelExportStore(Side, Export, Destination, ExportData);
  }
  __finally
  {
    delete ExportData;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExportStore(TOperationSide /*Side*/,
  TPanelExport /*Export*/, TPanelExportDestination Destination,
  TStringList * ExportData)
{
  if (Destination == pedClipboard)
  {
    CopyToClipboard(ExportData);
  }
  else
  {
    assert(false);
  }
}
//---------------------------------------------------------------------------
TQueueOperation __fastcall TCustomScpExplorerForm::DefaultQueueOperation()
{
  return FQueueController->DefaultOperation();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::AllowQueueOperation(
  TQueueOperation Operation, void ** Param)
{
  switch (Operation)
  {
    case qoPreferences:
      return true;

    case qoGoTo:
      return ComponentVisible[fcQueueView];

    case qoDisconnectOnceEmpty:
      return !FQueueController->Empty;

    default:
      return FQueueController->AllowOperation(Operation, Param);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteQueueOperation(
  TQueueOperation Operation, void * Param)
{
  if (Operation == qoGoTo)
  {
    assert(QueueView2->Visible);
    QueueView2->SetFocus();
  }
  else if (Operation == qoPreferences)
  {
    PreferencesDialog(pmQueue);
  }
  else
  {
    FQueueController->ExecuteOperation(Operation, Param);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2ContextPopup(
  TObject * /*Sender*/, TPoint & /*MousePos*/, bool & /*Handled*/)
{
  FQueueActedItem = QueueView2->ItemFocused;
}
//---------------------------------------------------------------------------
/*virtual*/ int __fastcall TCustomScpExplorerForm::GetStaticComponentsHeight()
{
  return TopDock->Height + QueueSplitter->Height;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueSplitterCanResize(
  TObject * /*Sender*/, int & NewSize, bool & Accept)
{
  // when queue is hidden by double-clicking splitter, stray attempt to
  // resize the panel with strange value arrives, make sure it is ignored
  if (ComponentVisible[fcQueueView])
  {
    int HeightLimit = ClientHeight - GetStaticComponentsHeight() -
      RemotePanel->Constraints->MinHeight;

    if (NewSize > HeightLimit)
    {
      NewSize = HeightLimit;
    }
  }
  else
  {
    Accept = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2StartDrag(TObject * /*Sender*/,
  TDragObject *& /*DragObject*/)
{
  FQueueActedItem = QueueView2->ItemFocused;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2DragOver(TObject * /*Sender*/,
  TObject * Source, int X, int Y, TDragState /*State*/, bool & Accept)
{
  Accept = true;
  if (Source == QueueView2)
  {
    TListItem * DropTarget = QueueView2->GetItemAt(X, Y);
    Accept = (DropTarget != NULL) && (FQueueActedItem != NULL);
    if (Accept)
    {
      TQueueItemProxy * QueueItem;
      TQueueItemProxy * DestQueueItem;

      QueueItem = static_cast<TQueueItemProxy *>(FQueueActedItem->Data);
      DestQueueItem = static_cast<TQueueItemProxy *>(DropTarget->Data);
      Accept = (QueueItem != DestQueueItem) &&
        (QueueItem->Status == TQueueItem::qsPending) &&
        (DestQueueItem->Status == TQueueItem::qsPending);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2DragDrop(TObject * /*Sender*/,
  TObject * /*Source*/, int /*X*/, int /*Y*/)
{
  if ((FQueueActedItem != NULL) && (QueueView2->DropTarget != NULL))
  {
    TQueueItemProxy * QueueItem;
    TQueueItemProxy * DestQueueItem;

    QueueItem = static_cast<TQueueItemProxy *>(FQueueActedItem->Data);
    DestQueueItem = static_cast<TQueueItemProxy *>(QueueView2->DropTarget->Data);
    QueueItem->Move(DestQueueItem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2Enter(TObject * /*Sender*/)
{
  if ((QueueView2->ItemFocused == NULL) &&
      (QueueView2->Items->Count > 0))
  {
    QueueView2->ItemFocused = QueueView2->Items->Item[0];
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView2SelectItem(
  TObject * /*Sender*/, TListItem * /*Item*/, bool Selected)
{
  if (Selected)
  {
    NonVisualDataModule->UpdateNonVisibleActions();
  }
}
//---------------------------------------------------------------------------
TDragDropFilesEx * __fastcall TCustomScpExplorerForm::DragDropFiles(TObject * Sender)
{
  TDragDropFilesEx * Result = NULL;
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  if (DirView != NULL)
  {
    Result = DirView->DragDropFilesEx;
  }
  else
  {
    TCustomDriveView * DriveView = dynamic_cast<TCustomDriveView *>(Sender);
    if (DriveView != NULL)
    {
      Result = DriveView->DragDropFilesEx;
    }
  }
  assert(Result != NULL);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDFileOperation(
  TObject * Sender, int Effect, AnsiString /*SourcePath*/,
  AnsiString TargetPath, bool & /*DoOperation*/)
{
  TFileOperation Operation;

  switch (Effect)
  {
    case DROPEFFECT_MOVE:
      Operation = foMove;
      break;

    case DROPEFFECT_COPY:
    // occures on WinXP (reported by user)
    default:
      Operation = foCopy;
      break;
  };

  TStrings * FileList = new TStringList();
  try
  {
    TDragDropFilesEx * DragDropFilesEx = DragDropFiles(Sender);

    for (int Index = 0; Index < DragDropFilesEx->FileList->Count; Index++)
    {
      FileList->Add(DragDropFilesEx->FileList->Items[Index]->Name);
    }

    FDragDropOperation = true;
    TTransferOperationParam Param;
    Param.TargetDirectory = TargetPath;
    // upload, no temp dirs
    Param.Temp = false;
    Param.DragDrop = true;
    ExecuteFileOperation(Operation, osLocal, FileList,
      !WinConfiguration->DDTransferConfirmation, &Param);
  }
  __finally
  {
    FDragDropOperation = false;
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileContolDDChooseEffect(
  TObject * Sender, int grfKeyState, int & dwEffect)
{
  // if any drop effect is allowed at all (e.g. no drop to self and drop to parent)
  if ((dwEffect != DROPEFFECT_None) &&
      IsFileControl(DropSourceControl, osRemote))
  {
    // do not allow drop on remote panel (on free space, still allow drop on directories)
    if ((Sender == RemoteDirView) && (DropSourceControl == RemoteDirView) &&
        (RemoteDirView->DropTarget == NULL))
    {
      dwEffect = DROPEFFECT_None;
    }
    else
    {
      if (dwEffect == DROPEFFECT_Copy)
      {
        bool MoveCapable = FTerminal->IsCapable[fcRemoteMove];
        bool CopyCapable = FTerminal->IsCapable[fcRemoteCopy] || FTerminal->IsCapable[fcSecondaryShell];
        // if we do not support neither of operations, there's no discussion
        if (!MoveCapable && !CopyCapable)
        {
          dwEffect = DROPEFFECT_None;
        }
        // when moving is disabled, we need to keep effect to "copy",
        // which will be later interpretted as move (with slipped-in cursor)
        else if (!WinConfiguration->DDAllowMoveInit && FLAGCLEAR(grfKeyState, MK_CONTROL))
        {
          // no-op, keep copy
        }
        else
        {
          // The default effect inside remote panel is move,
          // unless we do not support it, but support copy
          if (FLAGCLEAR(grfKeyState, MK_CONTROL))
          {
            dwEffect = MoveCapable ? DROPEFFECT_Move : DROPEFFECT_Copy;
          }
          else
          {
            // with ctrl-down, we want copy unless it is not supported
            dwEffect = CopyCapable ? DROPEFFECT_Copy : DROPEFFECT_None;
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDDragFileName(
  TObject * Sender, TRemoteFile * File, AnsiString & FileName)
{
  if (FDDTotalSize >= 0)
  {
    if (File->IsDirectory)
    {
      FDDTotalSize = -1;
    }
    else
    {
      FDDTotalSize += File->Size;
    }
  }
  assert(!FDragTempDir.IsEmpty());
  // TODO: this is quite ineffective
  // TODO: what if invalid character replacement is disabled?
  FileName = FDragTempDir + GUIConfiguration->CurrentCopyParam.ValidLocalFileName(File->FileName);

  AnsiString TransferFileName;
  if (Sender == RemoteDriveView)
  {
    TransferFileName = UnixExcludeTrailingBackslash(File->FullFileName);
  }
  else
  {
    TransferFileName = File->FileName;
  }
  FDDFileList->AddObject(TransferFileName, File);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDDragDetect(
  TObject * /*Sender*/, int /*grfKeyState*/, const TPoint & /*DetectStart*/,
  const TPoint & /*Point*/, TDragDetectStatus /*DragStatus*/)
{
  FDDFileList = new TStringList();
  FDragTempDir = WinConfiguration->TemporaryDir();
  FDDTotalSize = 0;
  FDragDropSshTerminate = "";
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDQueryContinueDrag(
  TObject * /*Sender*/, BOOL /*FEscapePressed*/, int /*grfKeyState*/,
  HRESULT & Result)
{
  if (Result == DRAGDROP_S_DROP)
  {
    try
    {
      GlobalDragImageList->HideDragImage();
      try
      {
        RemoteFileControlDDTargetDrop();
      }
      catch(ESshTerminate & E)
      {
        assert(E.MoreMessages == NULL); // not supported
        assert(!E.Message.IsEmpty());
        FDragDropSshTerminate = E.Message;
      }
    }
    catch (Exception &E)
    {
      // If downloading fails we need to cancel drag&drop, othwerwise
      // Explorer shows error
      // But by the way exception probably never reach this point as
      // it's catched on way
      Result = DRAGDROP_S_CANCEL;
      assert(Terminal != NULL);
      Terminal->ShowExtendedException(&E);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewMatchMask(
  TObject * /*Sender*/, AnsiString FileName, bool Directory, __int64 Size,
  AnsiString Masks,bool & Matches)
{
  TFileMasks::TParams MaskParams;
  MaskParams.Size = Size;
  TFileMasks M(Masks);
  Matches = M.Matches(FileName, Directory, AnsiString(""), &MaskParams);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetOverlay(
  TObject * Sender, TListItem * Item, WORD & Indexes)
{
  TCustomDirView * DirView = reinterpret_cast<TCustomDirView *>(Sender);
  AnsiString Ext;
  if (DirView == RemoteDirView)
  {
    Ext = UnixExtractFileExt(DirView->ItemFileName(Item));
  }
  else
  {
    Ext = ExtractFileExt(DirView->ItemFileName(Item));
  }

  if (SameText(Ext, Configuration->PartialExt))
  {
    Indexes |= oiPartial;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanPasteFromClipBoard()
{
  return
    IsFormatInClipboard(CF_TEXT) ||
    DirView(osCurrent)->CanPasteFromClipBoard();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PasteFromClipBoard()
{
  if (DirView(osCurrent)->CanPasteFromClipBoard())
  {
    DirView(osCurrent)->PasteFromClipBoard();
  }
  else
  {
    AnsiString Path;
    if (TextFromClipboard(Path) && !Path.IsEmpty())
    {
      DirView(osCurrent)->Path = Path;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileListFromClipboard()
{
  // TBD
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomScpExplorerForm::FileStatusBarText(
  const TStatusFileInfo & FileInfo)
{
  return
    FMTLOAD(FILE_INFO_FORMAT,
      (FormatBytes(FileInfo.SelectedSize),
       FormatBytes(FileInfo.FilesSize),
       FormatFloat("#,##0", FileInfo.SelectedCount),
       FormatFloat("#,##0", FileInfo.FilesCount)));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateFileStatusBar(
  TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo, int Panel)
{
  AnsiString Text = FileStatusBarText(FileInfo);
  if (StatusBar->SimplePanel)
  {
    assert(Panel == 0);
    StatusBar->SimpleText = Text;
  }
  else
  {
    StatusBar->Panels->Items[Panel]->Caption = Text;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteStatusBarClick(
  TObject * /*Sender*/)
{
  RemoteDirView->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToggleQueueVisibility()
{
  TQueueViewConfiguration Config = WinConfiguration->QueueView;
  switch (Config.Show)
  {
    case qvShow:
      if ((FQueueStatus != NULL) && (FQueueStatus->Count > 0))
      {
        Config.Show = qvHide;
      }
      else
      {
        Config.Show = Config.LastHideShow;
      }
      break;

    case qvHideWhenEmpty:
      if (ComponentVisible[fcQueueView])
      {
        Config.Show = qvHide;
      }
      else
      {
        Config.LastHideShow = Config.Show;
        Config.Show = qvShow;
      }
      break;

    case qvHide:
      Config.LastHideShow = Config.Show;
      Config.Show = qvShow;
      break;
  }
  WinConfiguration->QueueView = Config;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomScpExplorerForm::PathForCaption()
{
  AnsiString Result;
  if (FTerminal != NULL)
  {
    switch (WinConfiguration->PathInCaption)
    {
      case picShort:
        {
          Result = UnixExtractFileName(FTerminal->CurrentDirectory);
          if (Result.IsEmpty())
          {
            Result = FTerminal->CurrentDirectory;
          }
        }
        break;

      case picFull:
        Result = FTerminal->CurrentDirectory;
        break;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateControls()
{
  // beware that app title can contain progress indicator that we do not want to
  // see in window title. we rely here on fact that this should be never called
  // during operation
  Caption = TTerminalManager::Instance()->UpdateAppTitle();
  RemoteDirView->Color = (FSessionColor != 0 ? FSessionColor : clWindow);
  RemoteDriveView->Color = RemoteDirView->Color;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDirViewLoaded(TCustomDirView * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewLoaded(
  TObject * Sender)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  assert(DirView != NULL);
  DoDirViewLoaded(DirView);
  TransferPresetAutoSelect();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StartUpdates()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  // first run after installation
  if (double(Updates.LastCheck) == 0)
  {
    // make sure next time there will be an update (if enabled)
    Updates.LastCheck = TDateTime(1);
    WinConfiguration->Updates = Updates;
  }
  else if ((double(Updates.Period) > 0) &&
           (Now() - Updates.LastCheck >= Updates.Period))
  {
    StartUpdateThread(UpdatesChecked);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdatesChecked()
{
  AnsiString Message;
  bool New;
  TQueryType Type;
  GetUpdatesMessage(Message, New, Type, false);
  if (!Message.IsEmpty())
  {
    if (!New && (Type != qtWarning))
    {
      PostNote(Message, 0, UpdatesNoteClicked, NULL);
    }
    else
    {
      Notify(NULL, Message, Type, true, UpdatesNoteClicked);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdatesNoteClicked(TObject * /*Sender*/)
{
  CheckForUpdates(true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetTransferPresetAutoSelectData(
  TCopyParamRuleData & Data)
{
  assert(Terminal != NULL);
  Data.HostName = Terminal->SessionData->HostName;
  Data.UserName = Terminal->SessionData->UserName;
  Data.RemoteDirectory = RemoteDirView->PathName;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetAutoSelect()
{
  if (!FNoTransferPresetAutoSelect)
  {
    assert(Terminal != NULL);
    TCopyParamRuleData Data;
    GetTransferPresetAutoSelectData(Data);

    int CopyParamIndex = GUIConfiguration->CopyParamList->Find(Data);
    AnsiString CopyParamCurrent = GUIConfiguration->CopyParamCurrent;

    if (CopyParamIndex < 0)
    {
      // there is no preset that matches autoselection
      // set preset that we consider "default"
      FCopyParamAutoSelected = ""; // forget last autoselected preset
      GUIConfiguration->CopyParamCurrent = FCopyParamDefault;
    }
    else
    {
      // there is preset matching autoselection
      AnsiString CopyParamName = GUIConfiguration->CopyParamList->Names[CopyParamIndex];
      if (CopyParamName == FCopyParamAutoSelected)
      {
        // autoselected the same preset as the last time
        // make no change (i.e. preserve custom user preset, if any)
      }
      else
      {
        // autoselected the different preset then the last time (or there
        // was default preset set)
        FCopyParamAutoSelected = CopyParamName; // remember autoselection
        GUIConfiguration->CopyParamCurrent = CopyParamName;
      }
    }

    if (GUIConfiguration->CopyParamCurrent != CopyParamCurrent)
    {
      TTransferPresetNoteData * Data = new TTransferPresetNoteData;
      try
      {
        int Fmt =
          (CopyParamIndex < 0) ?
            (GUIConfiguration->CopyParamIndex < 0 ? COPY_PARAM_DEFAULT_NORM : COPY_PARAM_DEFAULT_CUSTOM) :
            COPY_PARAM_AUTOSELECTED;
        Data->Message = FMTLOAD(Fmt, (StripHotkey(GUIConfiguration->CopyParamCurrent)));
        Data->More = new TStringList();

        int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).General;
        AnsiString Info = GUIConfiguration->CurrentCopyParam.GetInfoStr("\n",
          CopyParamAttrs);
        if (CopyParamIndex >= 0)
        {
          assert(GUIConfiguration->CopyParamList->Rules[CopyParamIndex] != NULL);
          Info = FORMAT("%s\n \n%s", (Info,
            FMTLOAD(COPY_PARAM_RULE,
              (GUIConfiguration->CopyParamList->Rules[CopyParamIndex]->GetInfoStr("\n")))));
        }
        Data->More->Text = Info;

        if (WinConfiguration->CopyParamAutoSelectNotice)
        {
          TransferPresetNoteMessage(Data, true);
        }
        else
        {
          PostNote(Data->Message, 0, TransferPresetNoteClicked, Data);
          Data = NULL; // ownership passed
        }
      }
      __finally
      {
        delete Data;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetNoteMessage(
  TTransferPresetNoteData * NoteData, bool AllowNeverAskAgain)
{
  assert(NoteData != NULL);

  TMessageParams Params(AllowNeverAskAgain ? mpNeverAskAgainCheck : 0);

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaIgnore; // ignore" is after "ok"
  Aliases[0].Alias = LoadStr(CONFIGURE_BUTTON);

  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);

  int Result =
    MoreMessageDialog(NoteData->Message, NoteData->More, qtInformation,
      qaOK | qaIgnore, HELP_COPY_PARAM_AUTOSELECTED, &Params);

  switch (Result)
  {
    case qaNeverAskAgain:
      assert(AllowNeverAskAgain);
      WinConfiguration->CopyParamAutoSelectNotice = false;
      break;

    case qaIgnore:
      PreferencesDialog(pmPresets);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetNoteClicked(TObject * /*Sender*/)
{
  TransferPresetNoteMessage(dynamic_cast<TTransferPresetNoteData *>(FNoteData), false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PreferencesDialog(
  TPreferencesMode APreferencesMode)
{
  TCopyParamRuleData Data;
  GetTransferPresetAutoSelectData(Data);
  TPreferencesDialogData PreferencesData;
  PreferencesData.CopyParamRuleData = &Data;
  DoPreferencesDialog(APreferencesMode, &PreferencesData);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AdHocCustomCommandValidate(
  const AnsiString & Command, int Params)
{
  if (CustomCommandState(Command, Params, FEditingFocusedAdHocCommand) <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE, (Command)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AdHocCustomCommand(bool OnFocused)
{
  bool RemoteAllowed = CustomCommandRemoteAllowed();
  TCustomCommandParam Param;
  // make sure we use local custom command when remote are not supported
  if (RemoteAllowed || FLAGSET(FLastCustomCommand.Params, ccLocal))
  {
    Param = FLastCustomCommand;
  }
  else
  {
    Param.Params |= ccLocal;
  }
  Param.Name = LoadStr(CUSTOM_COMMAND_AD_HOC_NAME);
  FEditingFocusedAdHocCommand = OnFocused;
  int Options = FLAGMASK(!RemoteAllowed, ccoDisableRemote);
  if (DoCustomCommandDialog(Param.Name, Param.Command, Param.Params,
       WinConfiguration->CustomCommands, ccmAdHoc, Options,
       AdHocCustomCommandValidate))
  {
    FLastCustomCommand = Param;
    UpdateCustomCommandsToolbar();
    ExecuteFileOperation(foCustomCommand, osRemote, OnFocused, false, &Param);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LastCustomCommand(bool OnFocused)
{
  assert(!FLastCustomCommand.Command.IsEmpty());

  int State = CustomCommandState(FLastCustomCommand.Command,
    FLastCustomCommand.Params, OnFocused);
  assert(State > 0);
  if (State <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE, (FLastCustomCommand.Command)));
  }

  ExecuteFileOperation(foCustomCommand, osRemote, OnFocused, false, &FLastCustomCommand);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetLastCustomCommand(bool OnFocused,
  TCustomCommandParam & CustomCommand, int & State)
{
  bool Result = !FLastCustomCommand.Command.IsEmpty();

  if (Result)
  {
    CustomCommand = FLastCustomCommand;

    State = CustomCommandState(FLastCustomCommand.Command,
      FLastCustomCommand.Params, OnFocused);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WhatsThis()
{
  SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BeforeAction()
{
  if (RemoteDirView->ItemFocused != NULL)
  {
    RemoteDirView->ItemFocused->CancelEdit();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PostComponentHide(unsigned short Component)
{
  assert(ComponentVisible[Component]);
  PostMessage(Handle, WM_COMPONENT_HIDE, Component, 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueSplitterDblClick(TObject * /*Sender*/)
{
  // when queue panel is resized here directly, the status bar is stretched
  // over whole space the panel occupied
  PostComponentHide(fcQueueView);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Dispatch(void * Message)
{
  TMessage * M = static_cast<TMessage*>(Message);
  switch (M->Msg)
  {
    case CM_APPSYSCOMMAND:
      CMAppSysCommand(*M);
      break;

    case _WM_APPCOMMAND:
      WMAppCommand(*M);
      break;

    case WM_SYSCOMMAND:
      WMSysCommand(*M);
      break;

    case WM_WINDOWPOSCHANGING:
      WMWindowPosChanging(*reinterpret_cast<TWMWindowPosMsg *>(M));
      break;

    case WM_COMPONENT_HIDE:
      {
        unsigned short Component = static_cast<unsigned short>(M->WParam);
        // sanity check
        if (ComponentVisible[Component])
        {
          // special treatment
          if (Component == fcQueueView)
          {
            ToggleQueueVisibility();
            assert(!ComponentVisible[fcQueueView]);
          }
          else
          {
            ComponentVisible[Component] = false;
          }
        }
      }
      break;

    default:
      TForm::Dispatch(Message);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormConstrainedResize(
  TObject * /*Sender*/, int & MinWidth, int & MinHeight, int & MaxWidth,
  int & MaxHeight)
{
  // workaround for bug in TWinControl.CalcConstraints
  // Check for empty rect (restore from iconinc state) is done there only after
  // call to AdjustClientRect, which enlarges the rect (for forms).
  TRect R = GetClientRect();
  // when restoring from iconic state, place no restrictions
  if (IsRectEmpty(R))
  {
    MinWidth = 0;
    MinHeight = 0;
    MaxWidth = 0;
    MaxHeight = 0;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetSpaceAvailable(const AnsiString Path,
  TSpaceAvailable & ASpaceAvailable, bool & Close)
{
  // terminal can be already closed (e.g. dropped connection)
  if ((Terminal != NULL) && Terminal->IsCapable[fcCheckingSpaceAvailable])
  {
    try
    {
      Terminal->SpaceAvailable(Path, ASpaceAvailable);
    }
    catch(...)
    {
      if (!Terminal->Active)
      {
        Close = true;
      }
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileSystemInfo()
{
  const TSessionInfo & SessionInfo = Terminal->GetSessionInfo();
  const TFileSystemInfo & FileSystemInfo = Terminal->GetFileSystemInfo(true);
  TGetSpaceAvailable OnGetSpaceAvailable = NULL;
  if (Terminal->IsCapable[fcCheckingSpaceAvailable])
  {
    OnGetSpaceAvailable = GetSpaceAvailable;
  }
  DoFileSystemInfoDialog(SessionInfo, FileSystemInfo, Terminal->CurrentDirectory,
    OnGetSpaceAvailable);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetSessionColor(TColor value)
{
  if (value != FSessionColor)
  {
    FSessionColor = value;

    TColor C = (value != 0 ? value : clNone);
    TTBXColorPalette * ColorPalette = dynamic_cast<TTBXColorPalette *>(
      static_cast<TObject *>(GetComponent(fcColorPalette)));
    assert(ColorPalette != NULL);
    ColorPalette->Color = C;

    TTBXColorItem * ColorItem = dynamic_cast<TTBXColorItem *>(
      static_cast<TObject *>(GetComponent(fcColorMenu)));
    assert(ColorItem != NULL);
    ColorItem->Color = C;

    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionColorPick()
{
  TColorDialog * Dialog = new TColorDialog(this);
  try
  {
    Dialog->Options = Dialog->Options << cdFullOpen;
    Dialog->Color = (FSessionColor != 0 ? FSessionColor : clSkyBlue);
    if (Dialog->Execute())
    {
      SessionColor = Dialog->Color;
    }
  }
  __finally
  {
    delete Dialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionColorPaletteChange(
  TObject * Sender)
{
  TTBXColorPalette * ColorPalette = dynamic_cast<TTBXColorPalette *>(Sender);
  assert(ColorPalette != NULL);
  SessionColor = (ColorPalette->Color != clNone ? ColorPalette->Color : (TColor)0);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CancelNote()
{
  bool Result = FNoteTimer->Enabled;
  if (Result)
  {
    // cannot cancel note too early
    if (Now() - FNoteShown >
          EncodeTime(0, 0, (unsigned short)(WinConfiguration->NotificationsStickTime), 0))
    {
      FNoteTimer->Enabled = false;
      FNote = "";
      // beware that OnNoteClick may being executed
      SAFE_DESTROY(FNoteData);
      FOnNoteClick = NULL;
      FNoteHints = FNotes->Text;
      FNoteHints.Delete(FNoteHints.Length() - 1, 2);
      UpdateStatusBar();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NoteTimer(TObject * /*Sender*/)
{
  assert(FNoteTimer->Enabled);
  CancelNote();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddNote(AnsiString Note, bool UpdateNow)
{
  int P = Note.Pos("\n");
  if (P > 0)
  {
    Note.SetLength(P - 1);
  }

  FNotes->Add(FORMAT("[%s] %s",
    (FormatDateTime(Configuration->TimeFormat, Now()), Note)));
  while (FNotes->Count > 10)
  {
    FNotes->Delete(0);
  }

  if (UpdateNow)
  {
    FNoteHints = FNotes->Text;
    FNoteHints.Delete(FNoteHints.Length() - 1, 2);
    UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PostNote(AnsiString Note,
  unsigned int Seconds, TNotifyEvent OnNoteClick, TObject * NoteData)
{
  int P = Note.Pos("\n");
  if (P > 0)
  {
    Note.SetLength(P - 1);
  }

  FNoteHints = FNotes->Text;
  FNoteHints.Delete(FNoteHints.Length() - 1, 2);
  FNote = Note;
  // beware that OnNoteClick may being executed
  SAFE_DESTROY(FNoteData);
  FNoteData = NoteData;
  FOnNoteClick = OnNoteClick;
  AddNote(Note, false);
  UpdateStatusBar();
  FNoteShown = Now();
  FNoteTimer->Enabled = false;
  if (Seconds == 0)
  {
    Seconds = WinConfiguration->NotificationsTimeout;
  }
  FNoteTimer->Interval = Seconds * 1000;
  FNoteTimer->Enabled = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReadDirectoryCancelled()
{
  PostNote(LoadStr(DIRECTORY_READING_CANCELLED), 0, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeBrowsingChanged()
{
  PostNote(FORMAT(LoadStrPart(SYNC_DIR_BROWSE_TOGGLE, 1),
    (LoadStrPart(SYNC_DIR_BROWSE_TOGGLE,
      (NonVisualDataModule->SynchronizeBrowsingAction->Checked ? 2 : 3)))),
    0, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToggleShowHiddenFiles()
{
  WinConfiguration->ShowHiddenFiles = !WinConfiguration->ShowHiddenFiles;
  PostNote(FORMAT(LoadStrPart(SHOW_HIDDEN_FILES_TOGGLE, 1),
    (LoadStrPart(SHOW_HIDDEN_FILES_TOGGLE,
      (WinConfiguration->ShowHiddenFiles ? 2 : 3)))), 0, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToggleAutoReadDirectoryAfterOp()
{
  Configuration->AutoReadDirectoryAfterOp = !Configuration->AutoReadDirectoryAfterOp;
  PostNote(FORMAT(LoadStrPart(AUTO_READ_DIRECTORY_TOGGLE, 1),
    (LoadStrPart(AUTO_READ_DIRECTORY_TOGGLE,
      (Configuration->AutoReadDirectoryAfterOp ? 2 : 3)))), 0, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StatusBarPanelDblClick(
  TTBXCustomStatusBar * /*Sender*/, TTBXStatusPanel * Panel)
{
  if (Panel->Index == 0)
  {
    if (FOnNoteClick != NULL)
    {
      FOnNoteClick(NULL);
    }
  }
  else
  {
    FileSystemInfo();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LockWindow()
{
  // workaround:
  // 1) for unknown reason, disabling window, while minimized,
  // prevents it from restoring, even if it was enabled again meanwhile
  // 2) when disabling the main window, while another has focus
  // minimize is no longer possible ("keep up to date" dialog)
  if (!IsIconic(Application->Handle) && (Screen->ActiveForm == this))
  {
    Enabled = false;
  }

  FLockLevel++;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UnlockWindow()
{
  assert(FLockLevel > 0);
  FLockLevel--;

  if (FLockLevel == 0)
  {
    Enabled = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateRemotePathComboBox(
  TTBXComboBoxItem * RemotePathComboBox, bool TextOnly)
{
  if (!TextOnly)
  {
    TStrings * Items = RemotePathComboBox->Strings;
    Items->BeginUpdate();
    try
    {
      Items->Clear();
      AnsiString APath = UnixExcludeTrailingBackslash(RemoteDirView->Path);
      while (!IsUnixRootPath(APath))
      {
        int P = APath.LastDelimiter('/');
        assert(P >= 0);
        Items->Insert(0, APath.SubString(P + 1, APath.Length() - P));
        APath.SetLength(P - 1);
      }
      Items->Insert(0, Customunixdirview_SUnixDefaultRootName);
    }
    __finally
    {
      RemotePathComboBox->ItemIndex = Items->Count - 1;
      Items->EndUpdate();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const AnsiString /*AText*/, int AIndex,
  int & ImageIndex)
{
  if (AIndex < 0)
  {
    AIndex = Sender->ItemIndex;
  }
  ImageIndex = (AIndex < Sender->Strings->Count - 1 ? StdDirIcon : StdDirSelIcon);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxDrawItem(
  TTBXCustomList * /*Sender*/, TCanvas * /*ACanvas*/, TRect & ARect, int AIndex,
  int /*AHoverIndex*/, bool & /*DrawDefault*/)
{
  ARect.Left += (10 * AIndex);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxMeasureWidth(
  TTBXCustomList * /*Sender*/, TCanvas * /*ACanvas*/, int AIndex, int &AWidth)
{
  AWidth += (10 * AIndex);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxItemClick(
  TObject * Sender)
{
  TTBXComboBoxItem * RemotePathComboBox = dynamic_cast<TTBXComboBoxItem*>(Sender);

  AnsiString APath = UnixExcludeTrailingBackslash(RemoteDirView->Path);
  int Index = RemotePathComboBox->ItemIndex;
  while (Index < RemotePathComboBox->Strings->Count - 1)
  {
    APath = UnixExtractFileDir(APath);
    Index++;
  }
  // VanDyke style paths
  if (APath.IsEmpty())
  {
    assert(RemotePathComboBox->ItemIndex == 0);
    APath = ROOTDIRECTORY;
  }
  if (RemoteDirView->Path != APath)
  {
    RemoteDirView->Path = APath;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxCancel(TObject * Sender)
{
  UpdateRemotePathComboBox(dynamic_cast<TTBXComboBoxItem*>(Sender), true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClickToolbarItem(TTBCustomItem * Item,
  bool PositionCursor)
{
  TTBCustomItem * TopItem = Item;
  while (TopItem->Parent != NULL)
  {
    TopItem = TopItem->Parent;
  }
  TTBCustomToolbar * Toolbar = dynamic_cast<TTBCustomToolbar *>(TopItem->ParentComponent);
  assert(Toolbar != NULL);
  TTBItemViewer * Viewer = Toolbar->View->Find(Item);
  assert(Viewer != NULL);

  POINT P = Point(Viewer->BoundsRect.Left + (Viewer->BoundsRect.Width() / 2),
    Viewer->BoundsRect.Top + (Viewer->BoundsRect.Height() / 2));

  if (PositionCursor)
  {
    Mouse->CursorPos = Toolbar->ClientToScreen(P);
  }

  PostMessage(Toolbar->Handle, WM_LBUTTONDOWN, MK_LBUTTON,
    *reinterpret_cast<LPARAM*>(&P));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::MainWindowHook(TMessage & AMessage)
{
  // workaround for problem with hidden application window.
  // "groupped" window arrangement commands send WM_WINDOWPOSCHANGING to
  // windows associated with taskbar buttons (i.e. the hidden one for VCL).
  // we forward here the message to the main window.
  // this does not solve "all windows" arrangement commands,
  // which calculates size according to number of all visible windows.
  // hence they count VCL application twice.

  // code is said to be from Issue 4/95 of The Delphi Magazine
  if (AMessage.Msg == WM_WINDOWPOSCHANGING)
  {
    TWMWindowPosMsg & Message = reinterpret_cast<TWMWindowPosMsg &>(AMessage);

    assert(Message.WindowPos->hwnd == Application->Handle);
    // With WindowBlinds, we get [cx = Width, cy = 0]
    // on restore from taskbar into maximization
    if ((Message.WindowPos->hwnd == Application->Handle) &&
        !IsIconic(Message.WindowPos->hwnd) &&
        ((Message.WindowPos->cx > 0) ||
         (Message.WindowPos->cy > 0)))
    {
      assert(BorderStyle == bsSizeable);
      unsigned int LocalFlags =
        (Message.WindowPos->flags | SWP_NOZORDER) & ~SWP_NOSIZE;
      int AWidth = (Message.WindowPos->cx > 0) ? Message.WindowPos->cx : Width;
      int AHeight = (Message.WindowPos->cy > 0) ? Message.WindowPos->cy : Height;

      SetWindowPos(Handle, 0, Message.WindowPos->x, Message.WindowPos->y,
        AWidth, AHeight, LocalFlags);
    }
  }

  return false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewEditing(
  TObject * Sender, TListItem * /*Item*/, bool & /*AllowEdit*/)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  assert(DirView != NULL);
  HWND Edit = ListView_GetEditControl(DirView->Handle);
  // OnEditing is called also from TCustomListView::CanEdit
  if (Edit != NULL)
  {
    AnsiString Text;
    Text.SetLength(GetWindowTextLength(Edit) + 1);
    GetWindowText(Edit, Text.c_str(), Text.Length());

    int P = Text.LastDelimiter(".");
    if (P > 0)
    {
      // SendMessage does not work, edit control is probably not fully
      // initialized yet atm
      PostMessage(Edit, EM_SETSEL, 0, P - 1);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormActivate(TObject * /*Sender*/)
{
  Application->OnHint = ApplicationHint;
}
//---------------------------------------------------------------------------
