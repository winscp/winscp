//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <DiscMon.hpp>

#include <Common.h>

#include "CustomScpExplorer.h"

#include <Interface.h>
#include <Net.h>
#include <ScpMain.h>
#include <FileSystems.h>
#include <TextsWin.h>
#include <DiscMon.hpp>

#include <VCLCommon.h>
#include <Log.h>
#include <Progress.h>
#include <SynchronizeProgress.h>
#include <OperationStatus.h>
#include <Queue.h>

#include <DragExt.h>

#include "GUITools.h"
#include "NonVisual.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "CustomDirView"
#pragma link "CustomUnixDirView"
#pragma link "IEListView"
#pragma link "NortonLikeListView"
#pragma link "UnixDirView"
#pragma link "AssociatedStatusBar"
#pragma link "CustomDriveView"
#pragma link "UnixDriveView"
#pragma link "CustomDriveView"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
#ifdef DEBUGMODE
#define DEBUG(MSG) CurrentSSH->LogEvent(MSG)
#else
#define DEBUG(MSG)
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
  AnsiString TargetDirectory;
  bool DragDrop;
};
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::TCustomScpExplorerForm(TComponent* Owner):
    FFormRestored(False), TForm(Owner)
{
  FCurrentSide = osRemote;
  RestoreParams();
  RemoteDirView->Invalidate();
  assert(NonVisualDataModule && !NonVisualDataModule->ScpExplorer);
  NonVisualDataModule->ScpExplorer = this;
  Application->OnHint = ApplicationHint;
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

  FSynchronizeMonitor = NULL;

  FQueueStatus = NULL;
  FQueueStatusSection = new TCriticalSection();
  FQueueStatusInvalidated = false;
  FQueueItemInvalidated = false;
  FQueueActedItem = NULL;
  FQueueController = new TQueueController(QueueView);

  FUserActionTimer = new TTimer(this);
  FUserActionTimer->Enabled = false;
  FUserActionTimer->Interval = 10;
  FUserActionTimer->OnTimer = UserActionTimer;

  FOle32Library = LoadLibrary("Ole32.dll");
  FDragMoveCursor = FOle32Library != NULL ?
    LoadCursor(FOle32Library, MAKEINTRESOURCE(2)) : NULL;

  UseSystemSettings(this);

  TComboBox * SessionCombo = dynamic_cast<TComboBox*>(GetComponent(fcSessionCombo));
  if (SessionCombo != NULL)
  {
    SessionCombo->OnDrawItem = SessionComboDrawItem;
    SessionCombo->OnDropDown = SessionComboDropDown;
    SessionCombo->OnChange = SessionComboChange;
    SessionCombo->Hint = NonVisualDataModule->OpenedSessionsAction->Hint;
  }

  TToolBar * MenuToolBar = dynamic_cast<TToolBar*>(GetComponent(fcMenuToolBar));
  assert(MenuToolBar);
  MenuToolBar->Font = Screen->MenuFont;
  assert(MenuToolBar->ControlCount);
  MenuToolBar->Height = MenuToolBar->Controls[0]->Height;

  RemoteDirView->Font = Screen->IconFont;
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
  if (FDelayedDeletionTimer)
  {
    DoDelayedDeletion(NULL);
    SAFE_DESTROY(FDelayedDeletionTimer);
  }
  SAFE_DESTROY(FDelayedDeletionList);

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
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTerminal(TTerminal * value)
{
  if (FTerminal != value)
  {
    if (FTerminal)
    {
      UpdateSessionData(Terminal->SessionData);
    }
    FTerminal = value;
    TerminalChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalChanged()
{
  RemoteDirView->Terminal = Terminal;
  Caption = Application->Title;
  if (Terminal)
  {
    if (Terminal->Active)
    {
      Terminal->RefreshDirectory();
    }

    UpdateStatusBar();
  }
  TerminalListChanged(NULL);
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
void __fastcall TCustomScpExplorerForm::QueueViewDeletion(TObject * /*Sender*/,
  TListItem * Item)
{
  if (FQueueActedItem == Item)
  {
    FQueueActedItem = NULL;
    if ((QueueView->PopupMenu != NULL) &&
        (QueueView->PopupMenu->PopupComponent == QueueView))
    {
      SendMessage(PopupList->Window, WM_CANCELMODE, 0, 0);
    }
  }

  if (Item->Data == FPendingQueueActionItem)
  {
    FPendingQueueActionItem = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueStatus()
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
  UpdateQueueStatus();
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
      FRefreshLocalDirectory = (QueueItem == NULL) || (QueueItem->Info->ModifiesLocal);
      FRefreshRemoteDirectory = (QueueItem == NULL) || (QueueItem->Info->ModifiesRemote);
    }

    if (QueueItem != NULL)
    {
      QueueItem->Flag = true;
      FQueueItemInvalidated = true;
    }
  }
}
//---------------------------------------------------------------------------
TQueueItemProxy * __fastcall TCustomScpExplorerForm::RefreshQueueItems()
{
  TQueueItemProxy * Result = NULL;
  if (FQueueStatus != NULL)
  {
    bool Refresh = FQueueItemInvalidated;
    FQueueItemInvalidated = false;

    int Limit = Refresh ? FQueueStatus->Count : FQueueStatus->ActiveCount;

    bool Updated = false;
    bool Update;
    TQueueItemProxy * QueueItem;
    bool UserAction;
    for (int Index = 0; Index < Limit; Index++)
    {
      Update = false;
      QueueItem = FQueueStatus->Items[Index];
      UserAction = TQueueItem::IsUserActionStatus(QueueItem->Status);
      if (UserAction && (Result == NULL))
      {
        Result = QueueItem;
      }

      if (QueueItem->Flag)
      {
        QueueItem->Flag = false;
        QueueItem->Update();
        Updated = true;
        Update = true;
      }
      else if (UserAction)
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

  return Result;
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
  UpdateQueueView();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CopyParamDialog(
  TTransferDirection Direction, TTransferType Type, bool DragDrop,
  TStrings * FileList, AnsiString & TargetDirectory, TGUICopyParamType & CopyParam,
  bool Confirm)
{
  bool Result = true;
  assert(Terminal && Terminal->Active);
  if (DragDrop && (Direction == tdToLocal) && (Type == ttMove) &&
      !WinConfiguration->DDAllowMove)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    int Answer = MessageDialog(LoadStr(DND_DOWNLOAD_MOVE_WARNING), qtWarning,
      qaOK | qaCancel, 0, &Params);
    if (Answer == qaNeverAskAgain)
    {
      WinConfiguration->DDAllowMove = true;
    }
    else if (Answer == qaCancel)
    {
      Result = false;
    }
  }

  bool DragDropTemp = (DragDrop && (Direction == tdToLocal));
  if (Result && Confirm)
  {
    bool DisableNewerOnly =
      (!Terminal->IsCapable[fcNewerOnlyUpload] && (Direction == tdToRemote)) ||
      (DragDrop && (Direction == tdToLocal));
    int Options =
      (!Terminal->IsCapable[fcTextMode] ? coDisableTransferMode : 0) |
      (DisableNewerOnly ? coDisableNewerOnly : 0) |
      (DragDropTemp ? coDragDropTemp : 0);
    Result = DoCopyDialog(Direction == tdToRemote, Type == ttMove,
      FileList, TargetDirectory, &CopyParam, Options);
  }

  if (Result && CopyParam.Queue && !DragDropTemp)
  {
    assert(Queue != NULL);

    int Params =
      ((Type == ttMove) ? cpDelete : 0) |
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
  // IDE often looses this link
  RemoteDirView->HeaderImages = NonVisualDataModule->ArrowImages;

  if (Position == poDesigned)
  {
    RestoreFormParams();
  }
  ConfigurationChanged();

  QueuePanel->Height = WinConfiguration->QueueView.Height;
  LoadListViewStr(QueueView, WinConfiguration->QueueView.Layout);
  QueueCoolBar->Visible = WinConfiguration->QueueView.ToolBar;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreParams()
{
  WinConfiguration->QueueView.Height = QueuePanel->Height;
  WinConfiguration->QueueView.Layout = GetListViewStr(QueueView);
  WinConfiguration->QueueView.ToolBar = QueueCoolBar->Visible;
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
void __fastcall TCustomScpExplorerForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData, TCancelStatus & /*Cancel*/)
{
  // operation is being executed and we still didn't show up progress form
  if (ProgressData.InProgress && !FProgressForm)
  {
    //assert(Screen && Screen->ActiveCustomForm);
    FProgressForm = new TProgressForm(Application);
    FProgressForm->DeleteToRecycleBin = WinConfiguration->DeleteToRecycleBin;
    // When main window is hidden, we suppose "/upload" or URL download mode
    if (!Visible && (ProgressData.Operation != foCalculateSize))
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
  bool DragDrop, const AnsiString FileName, bool Success,
  bool & DisconnectWhenComplete)
{
  if ((FSynchronizeMonitor == NULL) && !FAutoOperation)
  {
    // no selection on "/upload", form servers only as event handler
    // (it is not displayed)
    if (PanelOperation(Side, DragDrop) &&
        Visible && (Operation != foCalculateSize))
    {
      AnsiString FileNameOnly = (Side == osRemote) ?
        UnixExtractFileName(FileName) : ExtractFileName(FileName);
      TListItem *Item = DirView(Side)->FindFileItem(FileNameOnly);
      assert(Item);
      if (Success) Item->Selected = false;
      Item->MakeVisible(false);
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
  bool DragDrop, const AnsiString FileName, Boolean Success,
  bool & DisconnectWhenComplete)
{
  DoOperationFinished(Operation, Side, DragDrop, FileName, Success,
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
bool __fastcall TCustomScpExplorerForm::GetEnableFocusedOperation(TOperationSide Side)
{
  return DirView(Side)->AnyFileSelected(true);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableSelectedOperation(TOperationSide Side)
{
  return DirView(Side)->AnyFileSelected(false);
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
      RemoteDirView->AnyFileSelected(true))
  {
    TPoint ScreenPoint, ClientPoint;
    ClientPoint = ((MousePos.x < 0) && (MousePos.y < 0)) ?
      TPoint(0, 0) : MousePos;
    ScreenPoint = RemoteDirView->ClientToScreen(ClientPoint);

    NonVisualDataModule->CurrentOpenMenuItem->Default = !WinConfiguration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentCopyMenuItem->Default = WinConfiguration->CopyOnDoubleClick;
    NonVisualDataModule->CurrentOpenMenuItem->Visible = WinConfiguration->ExpertMode;
    NonVisualDataModule->CurentEditMenuItem->Visible = WinConfiguration->ExpertMode;

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
      bool DragDrop = false;
      if (Param != NULL)
      {
        TTransferOperationParam& TParam =
          *static_cast<TTransferOperationParam*>(Param);
        TargetDirectory = TParam.TargetDirectory;
        DragDrop = TParam.DragDrop;
      }
      TGUICopyParamType CopyParam = GUIConfiguration->CopyParam;
      if (CopyParamDialog(Direction, Type, DragDrop, FileList, TargetDirectory,
          CopyParam, !NoConfirmation))
      {
        assert(Terminal);
        bool SelectionRestored = false;
        TCustomDirView * DView = NULL;
        if (HasDirView[Side])
        {
          DView = DirView(Side);
          DView->SaveSelection();
        }

        try
        {
          if (Side == osLocal)
          {
            int Params =
              FLAGMASK(Operation == foMove, cpDelete) |
              FLAGMASK(CopyParam.NewerOnly, cpNewerOnly) |
              FLAGMASK(DragDrop, cpDragDrop);
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
      bool Confirmed = !WinConfiguration->ConfirmDeleting;
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
          Query = FMTLOAD(CONFIRM_DELETE_FILE, (Query));
        }
        else
        {
          Query = FMTLOAD(CONFIRM_DELETE_FILES, (FileList->Count));
        }

        TMessageParams Params(mpNeverAskAgainCheck);
        int Answer = MessageDialog(Query, qtConfirmation,
          qaOK | qaCancel, 0, &Params);
        if (Answer == qaNeverAskAgain)
        {
          Confirmed = true;
          WinConfiguration->ConfirmDeleting = false;
        }
        else
        {
          Confirmed = (Answer == qaOK);
        }
      }

      if (Confirmed) DeleteFiles(Side, FileList);
    }
    else if (Operation == foSetProperties)
    {
      SetProperties(Side, FileList);
    }
    else if (Operation == foCustomCommand)
    {
      assert(Param);
      assert(Side == osRemote);

      FCustomCommandName = *((AnsiString *)Param);
      try
      {
        AnsiString Command = TCustomFileSystem::CompleteCustomCommand(
          WinConfiguration->CustomCommands->Values[FCustomCommandName],
          "", CustomCommandGetParamValue);
        Terminal->CustomCommandOnFiles(Command,
          WinConfiguration->CustomCommands->Params[FCustomCommandName],
          FileList);
      }
      __finally
      {
        FCustomCommandName = "";
      }
    }
    else if (Operation == foRemoteMove)
    {
      assert(Side == osRemote);
      RemoteMoveFiles(FileList, NoConfirmation);
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
void __fastcall TCustomScpExplorerForm::CustomCommandGetParamValue(
  const AnsiString AName, AnsiString & Value)
{
  assert(!FCustomCommandName.IsEmpty());
  AnsiString Name = AName;
  if (Name.IsEmpty())
  {
    Name = FMTLOAD(CUSTOM_COMMANDS_PARAM_PROMPT, (StripHotkey(FCustomCommandName)));
  }
  if (!InputDialog(FMTLOAD(CUSTOM_COMMANDS_PARAM_TITLE,
        (StripHotkey(FCustomCommandName))), Name, Value))
  {
    Abort();
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
          qaOK | qaCancel, 0) == qaOK)
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
            qaOK, 0, &Params);

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
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy)
{
  assert(!WinConfiguration->DisableOpenEdit);

  Side = GetSide(Side);

  bool Edit = (ExecuteFileBy == efEditor || ExecuteFileBy == efAlternativeEditor);
  bool InternalEdit = Edit &&
    ((WinConfiguration->Editor.Editor == edInternal) !=
     (ExecuteFileBy == efAlternativeEditor));
  FExecutedFileForceText =
    (InternalEdit || (Edit && WinConfiguration->Editor.ExternalEditorText));

  TStrings * FileList = DirView(Side)->CreateFocusedFileList(Side == osLocal);
  try
  {
    assert(FileList->Count == 1);
    if (Side == osRemote)
    {
      TCopyParamType CopyParam = GUIConfiguration->CopyParam;
      if (FExecutedFileForceText)
      {
        CopyParam.TransferMode = tmAscii;
      }
      CopyParam.FileNameCase = ncNoChange;
      CopyParam.PreserveReadOnly = false;
      CopyParam.ResumeSupport = rsOff;

      AnsiString TempDir = WinConfiguration->TemporaryTranferDir();
      ForceDirectories(TempDir);

      FAutoOperation = true;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->CopyToLocal(FileList, TempDir, &CopyParam, cpTemporary);
      }
      __finally
      {
        FAutoOperation = false;
        Terminal->ExceptionOnFail = false;
      }

      FExecutedFile = TempDir + FileList->Strings[0];
    }
    else
    {
      FExecutedFile = FileList->Strings[0];
    }
  }
  __finally
  {
    delete FileList;
  }

  Terminal->BeginTransaction();
  try
  {
    try
    {
      FExecutedFileTimestamp = FileAge(FExecutedFile);
      FFileExecutedBy = ExecuteFileBy;

      if (InternalEdit)
      {
        TNotifyEvent OnFileChanged = NULL;
        AnsiString Caption = FExecutedFile;
        if (Side == osRemote)
        {
          OnFileChanged = ExecutedFileChanged;
          Caption = RemoteDirView->Path + ExtractFileName(FExecutedFile);
        }
        DoEditorForm(FExecutedFile, this, OnFileChanged, Caption);
      }
      else
      {
        TDiscMonitor * DiscMonitor = NULL;
        try
        {
          if (Side == osRemote)
          {
            DiscMonitor = new TDiscMonitor(this);
            DiscMonitor->SubTree = false;
            DiscMonitor->Filters = TMonitorFilters() << moLastWrite;
            DiscMonitor->AddDirectory(ExtractFilePath(FExecutedFile), false);
            DiscMonitor->OnChange = ExecutedFileChangedNotification;
            DiscMonitor->Active = true;
          }

          TOperationStatusForm * StatusForm = new TOperationStatusForm(Application);
          try
          {
            StatusForm->Status = LoadStr(DOCUMENT_WAIT);
            StatusForm->ShowAsModal();

            if (Edit)
            {
              AnsiString ExternalEditor, Program, Params, Dir;
              ExternalEditor = WinConfiguration->Editor.ExternalEditor;
              TWinConfiguration::ReformatFileNameCommand(ExternalEditor);
              SplitCommand(ExternalEditor, Program, Params, Dir);
              assert(Params.Pos(ShellCommandFileNamePattern) > 0);
              Params = StringReplace(Params, ShellCommandFileNamePattern,
                AddPathQuotes(FExecutedFile), TReplaceFlags() << rfReplaceAll);
              if (ExecuteShellAndWait(Program, Params) < 0)
              {
                throw Exception(FMTLOAD(EDITOR_ERROR, (Program)));
              }
            }
            else
            {
              assert(Side == osRemote);
              if (ExecuteShellAndWait(FExecutedFile, "") < 0)
              {
                throw Exception(FMTLOAD(EXECUTE_FILE_ERROR, (FExecutedFile)));
              }
            }
          }
          __finally
          {
            delete StatusForm;
          }

          if (Side == osRemote)
          {
            if (DiscMonitor)
            {
              DiscMonitor->Active = false;
            }
            // upload file if it was saved while [editor] was closed
            ExecutedFileChanged(NULL);
          }
        }
        __finally
        {
          delete DiscMonitor;
        }
      }
    }
    __finally
    {
      AnsiString FileName = FExecutedFile;
      FExecutedFile = "";
      if (Side == osRemote)
      {
        bool Deleted;
        AnsiString DirName = ExtractFilePath(FileName);

        if (WinConfiguration->ForceDeleteTempFolder)
        {
          Deleted = RecursiveDeleteFile(ExcludeTrailingBackslash(DirName), false);
        }
        else
        {
          Deleted = DeleteFile(FileName) && RemoveDir(DirName);
        }

        if (!Deleted)
        {
          throw Exception(FMTLOAD(DELETE_TEMP_EXECUTE_FILE_ERROR, (DirName)));
        }
      }
      else
      {
        ReloadLocalDirectory();
      }
    }
  }
  __finally
  {
    Terminal->EndTransaction();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChanged(TObject * Sender)
{
  static Uploading = 0;
  try
  {
    assert(!FExecutedFile.IsEmpty());

    if (Uploading == 1)
    {
      Uploading = 2;
    }
    else
    {
      Uploading = 1;
      try
      {
        int FileTimestamp = FileAge(FExecutedFile);
        if (FileTimestamp != FExecutedFileTimestamp)
        {
          FExecutedFileTimestamp = FileTimestamp;

          TStrings * FileList = new TStringList();
          try
          {
            FileList->Add(FExecutedFile);

            TCopyParamType CopyParam = GUIConfiguration->CopyParam;
            if (FExecutedFileForceText)
            {
              CopyParam.TransferMode = tmAscii;
            }
            CopyParam.FileNameCase = ncNoChange;
            CopyParam.PreserveRights = false;
            CopyParam.ResumeSupport = rsOff;

            FAutoOperation = true;
            Terminal->ExceptionOnFail = true;
            try
            {
              Terminal->CopyToRemote(FileList, RemoteDirView->PathName,
                &CopyParam, cpNoConfirmation);
            }
            __finally
            {
              FAutoOperation = false;
              Terminal->ExceptionOnFail = false;
            }
          }
          __finally
          {
            delete FileList;
          }
        }
      }
      __finally
      {
        if (Uploading == 2)
        {
          Uploading = 0;
          ExecutedFileChanged(this);
        }
        else
        {
          Uploading = 0;
        }
      }
    }
  }
  catch (Exception & E)
  {
    if (dynamic_cast<TDiscMonitor *> (Sender))
    {
      Terminal->DoHandleExtendedException(&E);
    }
    else
    {
      throw;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChangedNotification(
  TObject * Sender, const AnsiString /*Directory*/)
{
  ExecutedFileChanged(Sender);
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
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DeleteFiles(TOperationSide Side,
  TStrings * FileList)
{
  assert(Terminal);
  TCustomDirView * DView = DirView(Side);
  DView->SaveSelection();

  try
  {
    if (Side == osRemote)
    {
      Terminal->DeleteFiles(FileList);
    }
    else
    {
      try
      {
        Terminal->DeleteLocalFiles(FileList);
      }
      __finally
      {
        ReloadLocalDirectory();
      }
    }
  }
  catch(...)
  {
    DView->DiscardSavedSelection();
    throw;
  }
  DView->RestoreSelection();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteMoveDialog(TStrings * FileList,
  AnsiString & Target, AnsiString & FileMask, bool NoConfirmation)
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
  Target = UnixIncludeTrailingBackslash(Target);
  FileMask = "*.*";
  bool Result = true;
  if (!NoConfirmation)
  {
    Result = DoRemoteMoveDialog(FileList, Target, FileMask);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteMoveFiles(
  TStrings * FileList, bool NoConfirmation)
{
  AnsiString Target, FileMask;
  if (RemoteMoveDialog(FileList, Target, FileMask, NoConfirmation))
  {
    RemoteDirView->SaveSelection();

    try
    {
      Terminal->MoveFiles(FileList, Target, FileMask);
    }
    catch(...)
    {
      RemoteDirView->DiscardSavedSelection();
      throw;
    }
    RemoteDirView->RestoreSelection();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateDirectory(TOperationSide Side)
{
  AnsiString Name = LoadStr(NEW_FOLDER);
  if (InputDialog(LoadStr(CREATE_FOLDER_CAPTION), LoadStr(CREATE_FOLDER_PROMPT), Name))
  {
    DirView(Side)->CreateDirectory(Name);
  }
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
void __fastcall TCustomScpExplorerForm::SetProperties(TOperationSide Side, TStrings * FileList)
{
  if (Side == osRemote)
  {
    TRemoteProperties CurrentProperties;

    CurrentProperties = TRemoteProperties::CommonProperties(FileList);

    int Flags = 0;
    if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
    if (Terminal->IsCapable[fcOwnerChanging]) Flags |= cpOwner;
    if (Terminal->IsCapable[fcGroupChanging]) Flags |= cpGroup;

    TRemoteProperties NewProperties = CurrentProperties;
    if (DoPropertiesDialog(FileList, RemoteDirView->PathName,
        Terminal->Groups, Terminal->Users, &NewProperties, Flags, Terminal))
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
  if (QueueView->Focused() && (QueueView->OnKeyDown != NULL))
  {
    QueueView->OnKeyDown(QueueView, Key, Shift); 
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
    for (int i = 0; i < NonVisualDataModule->OpenedSessionsMenu->Count; i++)
    {
      TMenuItem * Item = NonVisualDataModule->OpenedSessionsMenu->Items[i];
      if (Item->ShortCut == KeyShortCut)
      {
        Key = 0;
        Item->Click();
        FIgnoreNextSysCommand = true;
        return;
      }
    }
    if (Key == VK_TAB && Shift.Contains(ssCtrl))
    {
      TTerminalManager::Instance()->CycleTerminals(!Shift.Contains(ssShift));
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusBar()
{
  TStatusBar * SessionStatusBar = (TStatusBar *)GetComponent(fcStatusBar);
  assert(SessionStatusBar && (SessionStatusBar->Panels->Count >= 3));
  if (FShowStatusBarHint)
  {
    SessionStatusBar->SimplePanel = true;
    SessionStatusBar->SimpleText = FStatusBarHint;
  }
  else if (!Terminal || !Terminal->Active || Terminal->Status < sshReady)
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
    int Index = SessionStatusBar->Tag;
    SessionStatusBar->Panels->Items[Index]->Text = FormatBytes(Terminal->BytesReceived);
    SessionStatusBar->Panels->Items[Index + 1]->Text = FormatBytes(Terminal->BytesSent);
    SessionStatusBar->Panels->Items[Index + 5]->Text = Terminal->ProtocolName;
    SessionStatusBar->Panels->Items[Index + 6]->Text =
      FormatDateTime(Configuration->TimeFormat, Terminal->Duration);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionStatusBarDrawPanel(
      TStatusBar *StatusBar, TStatusPanel *Panel, const TRect &Rect)
{
  if (Terminal && Terminal->Active && Terminal->Status >= sshReady)
  {
    TFontStyles Style;
    StatusBar->Font->Style = Style;
    int ImageIndex;
    AnsiString PanelText;
    switch (Panel->Index - StatusBar->Tag) {
      case 2: ImageIndex = Terminal->SshVersion - 1; break;
      case 3: ImageIndex = 2 + (Terminal->CSCompression || Terminal->SCCompression); break;
      case 4: ImageIndex = 4; PanelText = CipherNames[Terminal->CSCipher]; break;
      default: assert(false); break;
    }
    TCanvas *SCanvas = StatusBar->Canvas;
    if ((Panel->Alignment == taCenter) && PanelText.IsEmpty())
    {
      NonVisualDataModule->SessionImages->Draw(SCanvas,
        Rect.Left + Rect.Width() / 2 - NonVisualDataModule->SessionImages->Width / 2,
        Rect.Top, ImageIndex, true);
    }
    else
    {
      NonVisualDataModule->SessionImages->Draw(SCanvas,
        Rect.Left + 1, Rect.Top, ImageIndex, true);
      if (!PanelText.IsEmpty())
      {
        SCanvas->TextOut(Rect.left + 18, Rect.top + 1, PanelText);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Idle(bool AppIdle)
{
  // terminal may not be active here, when connection is closed by remote side
  // and coresponding error message is being displayed
  if (Terminal && Terminal->Active)
  {
    Terminal->Idle();
  }

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
    UpdateQueueStatus();
  }

  TQueueItemProxy * PendingQueueActionItem = RefreshQueueItems();
  if (AppIdle &&
      GUIConfiguration->QueueAutoPopup &&
      (PendingQueueActionItem != NULL) &&
      (FPendingQueueActionItem == NULL))
  {
    FPendingQueueActionItem = PendingQueueActionItem;
    FUserActionTimer->Enabled = true;
  }

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
void __fastcall TCustomScpExplorerForm::SessionStatusBarMouseMove(
      TObject * Sender, TShiftState /*Shift*/, int X, int Y)
{
  if (Terminal && Terminal->Active)
  {
    const int Hints[3] = { STATUS_RECEIVED_HINT, STATUS_SENT_HINT, STATUS_DURATION_HINT };
    TStatusBar * StatusBar = ((TStatusBar *)Sender);
    assert(StatusBar);
    TPoint Local(X, Y)/* = StatusBar->ScreenToClient(Mouse->CursorPos)*/;
    int IPanel = 0;
    while ((Local.x > StatusBar->Panels->Items[IPanel]->Width) &&
      (IPanel < StatusBar->Panels->Count - 1))
    {
      Local.x -= StatusBar->Panels->Items[IPanel]->Width;
      IPanel ++;
    }
    AnsiString AHint;
    if (StatusBar->Tag && (IPanel == 0))
    {
      AHint = LoadStr(STATUS_FILEINFO_HINT);
    }
    else
    {
      if (StatusBar->Tag) IPanel--;
      switch (IPanel) {
        case 0:
        case 1: AHint = LoadStr(Hints[IPanel]); break;
        case 6: AHint = LoadStr(Hints[2]); break;
        case 2: AHint = FMTLOAD(STATUS_VERSION_HINT, (Terminal->SshVersion)); break;

        case 3:
          if (Terminal->CSCompression == Terminal->SCCompression)
          {
            AHint = FMTLOAD(STATUS_COMPRESSION_HINT, (BooleanToStr(Terminal->CSCompression)));
          }
          else
          {
            AHint = FMTLOAD(STATUS_COMPRESSION2_HINT,
              (BooleanToStr(Terminal->CSCompression), BooleanToStr(Terminal->SCCompression)));
          }
          break;

        case 4:
          if (Terminal->CSCipher == Terminal->SCCipher)
          {
            AHint = FMTLOAD(STATUS_ENCRYPTION_HINT, (CipherNames[Terminal->CSCipher]));
          }
          else
          {
            AHint = FMTLOAD(STATUS_ENCRYPTION2_HINT,
              (CipherNames[Terminal->CSCipher], CipherNames[Terminal->SCCipher]));
          }
          break;

        case 5:
          AHint = FMTLOAD(STATUS_FS_PROTOCOL, (Terminal->ProtocolName));
          break;

        default: AHint = ""; break;
      }
    }

    if (AHint.IsEmpty())
    {
      StatusBar->Hint = AHint;
    }
    else
    {
      AHint = FORMAT("%s|X", (AHint));
      if (AHint != StatusBar->Hint)
      {
        Application->CancelHint();
        StatusBar->Hint = AHint;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationHint(TObject * /*Sender*/)
{
  assert(GetComponent(fcStatusBar) && Application);
  AnsiString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty() && (AHint != "X");
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
void __fastcall TCustomScpExplorerForm::NewSession()
{
  TSessionData * Data = new TSessionData("");
  try
  {
    Data->Assign(StoredSessions->DefaultSettings);
    if (DoLoginDialog(StoredSessions, Data, loAddSession))
    {
      assert(Data->CanLogin);
      TTerminalManager * Manager = TTerminalManager::Instance();
      TTerminal * Terminal = Manager->NewTerminal(Data);
      Manager->ActiveTerminal = Terminal;
      Manager->ConnectActiveTerminal();
    }
  }
  __finally
  {
    delete Data;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanCloseQueue()
{
  assert(FQueue != NULL);
  return (FQueue->IsEmpty ||
    (MessageDialog(LoadStr(PENDING_QUEUE_ITEMS), qtWarning, qaOK | qaCancel) == qaOK));
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
  TTerminalManager * Manager = TTerminalManager::Instance();
  TTerminal * Terminal = Manager->NewTerminal(Data);
  Manager->ActiveTerminal = Terminal;
  Manager->ConnectActiveTerminal();
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
      qaOK | qaCancel, 0, &Params);

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
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DropDownButtonMenu(TObject *Sender)
{
  ((TToolButton*)Sender)->CheckMenuDropdown();
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
void __fastcall TCustomScpExplorerForm::SetComponentVisible(Word Component, Boolean value)
{
  TControl * Control = GetComponent((Word)(Component & 0x00FF));
  assert(Control);
  bool Changed;
  if ((dynamic_cast<TCoolBar*>(Control) != NULL) && (Component & 0xFF00))
  {
    TCoolBand * Band = GetCoolBand(dynamic_cast<TCoolBar*>(Control),
      (Component & 0xFF00) >> 8);
    assert(Band);
    Changed = (Band->Visible != value);
    if (Changed)
    {
      Band->Visible = value;
    }
  }
  else
  {
    Changed = (Control->Visible != value);
    if (Changed)
    {
      Control->Visible = value;
    }
  }

  if (Changed)
  {
    FixControlsPlacement();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetComponentVisible(Word Component)
{
  TControl * Control = GetComponent((Word)(Component & 0x00FF));
  if (Control == NULL)
  {
    return false;
  }
  else if ((dynamic_cast<TCoolBar*>(Control) != NULL) && (Component & 0xFF00))
  {
    TCoolBand * Band = GetCoolBand(dynamic_cast<TCoolBar*>(Control),
      (Component & 0xFF00) >> 8);
    return Band != NULL ? Band->Visible : false;
  }
  else
  {
    return Control->Visible;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FixControlsPlacement()
{
  TToolBar * MenuToolBar = dynamic_cast<TToolBar*>(GetComponent(fcMenuToolBar));
  MenuToolBar->Height = MenuToolBar->Controls[0]->Height;

  if (RemoteDirView->ItemFocused != NULL)
  {
    RemoteDirView->ItemFocused->MakeVisible(false);
  }
  QueueSplitter->Visible = QueuePanel->Visible;
  RemotePanelSplitter->Visible = RemoteDriveView->Visible;
}
//---------------------------------------------------------------------------
TCoolBand * __fastcall TCustomScpExplorerForm::GetCoolBand(TCoolBar * Coolbar, int ID)
{
  return dynamic_cast<TCoolBand *>(Coolbar->Bands->FindItemID(ID));
}
//---------------------------------------------------------------------------
TControl * __fastcall TCustomScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcStatusBar: return RemoteStatusBar;
    case fcCoolBar: return TopCoolBar;
    case fcRemotePopup: return reinterpret_cast<TControl *>(NonVisualDataModule->RemoteDirViewPopup);
    case fcQueueView: return QueuePanel;
    case fcQueueToolbar: return QueueCoolBar;
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
  }
  else
  {
    DirViewColumnMenu = NonVisualDataModule->LocalDirViewColumnPopup;
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
  TCustomDirView * ADirView = (TCustomDirView *)Sender;
  if (ADirView->ItemIsDirectory(Item))
  {
    AllowExec = true;
  }
  else if (WinConfiguration->CopyOnDoubleClick && !FForceExecution)
  {
    ExecuteFileOperation(foCopy,
      (ADirView == DirView(osRemote) ? osRemote : osLocal),
      true, !WinConfiguration->CopyOnDoubleClickConfirmation);
    AllowExec = false;
  }
  else if (ADirView == DirView(osRemote) && !WinConfiguration->DisableOpenEdit)
  {
    ExecuteFile(osRemote, efDefault);
    AllowExec = false;
  }
  else
  {
    AllowExec = true;
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
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory)
{
  TSynchronizeParamType Params;
  Params.LocalDirectory = LocalDirectory;
  Params.RemoteDirectory = RemoteDirectory;
  Params.Params = GUIConfiguration->SynchronizeParams;
  Params.Recurse = GUIConfiguration->SynchronizeRecurse;
  bool SaveSettings = false;
  bool Result = DoSynchronizeDialog(Params, SynchronizeStartStop, SaveSettings);
  if (Result)
  {
    if (SaveSettings)
    {
      GUIConfiguration->SynchronizeParams = Params.Params;
      GUIConfiguration->SynchronizeRecurse = Params.Recurse;
    }
    LocalDirectory = Params.LocalDirectory;
    RemoteDirectory = Params.RemoteDirectory;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeStartStop(TObject * /*Sender*/,
  bool Start, const TSynchronizeParamType & Params, TSynchronizeAbortEvent OnAbort)
{
  if (Start)
  {
    try
    {
      FSynchronizeParams = Params;
      assert(OnAbort);
      FSynchronizeAbort = OnAbort;
      FSynchronizeMonitor = new TDiscMonitor(this);
      FSynchronizeMonitor->SubTree = false;
      TMonitorFilters Filters;
      Filters << moFilename << moLastWrite;
      if (FSynchronizeParams.Recurse)
      {
        Filters << moDirName;
      }
      FSynchronizeMonitor->Filters = Filters;
      FSynchronizeMonitor->AddDirectory(FSynchronizeParams.LocalDirectory,
        FSynchronizeParams.Recurse);
      FSynchronizeMonitor->OnChange = SynchronizeChange;
      FSynchronizeMonitor->OnInvalid = SynchronizeInvalid;
      FSynchronizeMonitor->Open();
    }
    catch(...)
    {
      //SAFE_DESTROY(FSynchronizeMonitor);
      throw;
    }
  }
  else
  {
    assert(FSynchronizeMonitor != NULL);
    SAFE_DESTROY(FSynchronizeMonitor);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeChange(
  TObject * /*Sender*/, const AnsiString Directory)
{
  try
  {
    AnsiString RemoteDirectory;
    AnsiString RootLocalDirectory;
    RootLocalDirectory = IncludeTrailingBackslash(FSynchronizeParams.LocalDirectory);
    RemoteDirectory = UnixIncludeTrailingBackslash(FSynchronizeParams.RemoteDirectory);

    AnsiString LocalDirectory = IncludeTrailingBackslash(Directory);

    assert(LocalDirectory.SubString(1, RootLocalDirectory.Length()) ==
      RootLocalDirectory);
    RemoteDirectory = RemoteDirectory +
      ToUnixPath(LocalDirectory.SubString(RootLocalDirectory.Length() + 1,
        LocalDirectory.Length() - RootLocalDirectory.Length()));

    TCopyParamType CopyParam = GUIConfiguration->CopyParam;
    CopyParam.PreserveTime = true;
    Synchronize(LocalDirectory, RemoteDirectory, smRemote, CopyParam,
      FSynchronizeParams.Params | TTerminal::spNoRecurse | TTerminal::spUseCache |
        TTerminal::spDelayProgress);
  }
  catch(Exception & E)
  {
    ShowExtendedExceptionEx(Terminal, &E);
    SynchronizeAbort(dynamic_cast<EFatal*>(&E) != NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeAbort(bool Close)
{
  FSynchronizeMonitor->Close();
  assert(FSynchronizeAbort);
  FSynchronizeAbort(this, Close);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeInvalid(
  TObject * /*Sender*/, const AnsiString Directory)
{
  if (!Directory.IsEmpty())
  {
    SimpleErrorDialog(FMTLOAD(WATCH_ERROR_DIRECTORY, (Directory)));
  }
  else
  {
    SimpleErrorDialog(LoadStr(WATCH_ERROR_GENERAL));
  }

  SynchronizeAbort(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FullSynchronizeDirectories()
{
  assert(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Synchronize(const AnsiString LocalDirectory,
  const AnsiString RemoteDirectory, TSynchronizeMode Mode,
  const TCopyParamType & CopyParam, int Params)
{
  assert(!FAutoOperation);
  void * BatchStorage;
  BatchStart(BatchStorage);
  FAutoOperation = true;

  try
  {
    FSynchronizeProgressForm = new TSynchronizeProgressForm(Application);
    if (FLAGCLEAR(Params, TTerminal::spDelayProgress))
    {
      FSynchronizeProgressForm->Start();
    }

    Terminal->Synchronize(LocalDirectory, RemoteDirectory, 
      static_cast<TTerminal::TSynchronizeMode>(Mode),
      &CopyParam, Params, TerminalSynchronizeDirectory);
  }
  __finally
  {
    FAutoOperation = false;
    SAFE_DESTROY(FSynchronizeProgressForm);
    BatchEnd(BatchStorage);
    ReloadLocalDirectory();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoFullSynchronizeDirectories(
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, TSynchronizeMode & Mode)
{
  bool Result;
  int Params = GUIConfiguration->SynchronizeParams;

  bool SaveSettings;
  Result = DoFullSynchronizeDialog(Mode, Params, LocalDirectory, RemoteDirectory,
    SaveSettings);
  if (Result)
  {
    if (SaveSettings)
    {
      GUIConfiguration->SynchronizeParams = Params;
    }

    TDateTime StartTime = Now();
    Synchronize(LocalDirectory, RemoteDirectory, Mode,
      GUIConfiguration->CopyParam, Params);

    OperationComplete(StartTime);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalSynchronizeDirectory(
  const AnsiString LocalDirectory, const AnsiString RemoteDirectory, bool & Continue)
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
  AnsiString SessionName;
  SessionName = Terminal->SessionData->SessionName;
  SessionName = DoSaveSessionDialog(StoredSessions, SessionName);
  if (!SessionName.IsEmpty())
  {
    TSessionData * SessionData = new TSessionData("");
    try
    {
      SessionData->Assign(Terminal->SessionData);
      UpdateSessionData(SessionData);
      StoredSessions->NewSession(SessionName, SessionData);
      StoredSessions->Save();
    }
    __finally
    {
      delete SessionData;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  assert(Terminal && Terminal->SessionData);
  if (!Data)
  {
    Data = Terminal->SessionData;
  }

  // cannot use RemoteDirView->Path, because it is empty if connection
  // was already closed
  Data->RemoteDirectory = Terminal->CurrentDirectory;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolBarResize(TObject *Sender)
{
  TToolBar * ToolBar = dynamic_cast<TToolBar*>(Sender);
  assert(ToolBar != NULL);
  if (ToolBar->ControlCount == 1)
  {
    ToolBar->Controls[0]->Width = ToolBar->Width;
  }
  else
  {
    TControl * ResizeControl = NULL;
    int Width = ToolBar->Width;
    for (int i = 0; i < ToolBar->ControlCount; i++)
    {
      TControl * Control = ToolBar->Controls[i];
      if (Control->Tag != 0)
      {
        ResizeControl = Control;
      }
      else
      {
        Width -= Control->Width;
      }
    }

    assert(ResizeControl != NULL);
    ResizeControl->Width = Width;
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
      Integer MessageRes = 0;
      if (RequiredSpace >= 0)
      {
        __int64 RequiredWithReserve;
        RequiredWithReserve = (__int64)(RequiredSpace * WinConfiguration->DDWarnLackOfTempSpaceRatio);
        if (FreeSpace < RequiredWithReserve) MessageRes = DD_WARN_LACK_OF_TEMP_SPACE;
      }
      else
      {
        MessageRes = DD_WARN_UNKNOWN_TEMP_SPACE;
      }

      if (MessageRes)
      {
        int Result;
        TMessageParams Params(mpNeverAskAgainCheck);
        Result = MessageDialog(FMTLOAD(MessageRes, (Path,
          FormatBytes(FreeSpace), FormatBytes(RequiredSpace))),
          qtWarning, qaYes | qaNo, 0, &Params);

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
        qtConfirmation, qaYes | qaNo, 0) == qaYes))
  {
    TStrings * VisitedDirectories = CreateVisitedDirectories(Side);
    try
    {
      AnsiString Name = DirView(Side)->PathName;
      if (::DoOpenDirectoryDialog(Mode, Side, Name, VisitedDirectories, Terminal))
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
void __fastcall TCustomScpExplorerForm::OpenInPutty()
{
  OpenSessionInPutty(Terminal->SessionData);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenConsole(AnsiString Command)
{
  DoConsoleDialog(Terminal, Command);
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
void __fastcall TCustomScpExplorerForm::AddEditLink()
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

    Edit = File->IsSymLink && Terminal->SessionData->ResolveSymlinks;
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
      bool Recursive = false;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->DeleteFile("", File, &Recursive);
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
void __fastcall TCustomScpExplorerForm::LastTerminalClosed(TObject * /*Sender*/)
{
  try
  {
    NewSession();
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
  TCustomCombo * SessionCombo = dynamic_cast<TCustomCombo *>(GetComponent(fcSessionCombo));
  if (SessionCombo != NULL)
  {
    SessionCombo->Items = TTerminalManager::Instance()->TerminalList;
    SessionCombo->ItemIndex = TTerminalManager::Instance()->ActiveTerminalIndex;
  }
  NonVisualDataModule->CreateOpenedSessionListMenu();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboDropDown(TObject * Sender)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Sender);
  assert(SessionCombo);
  TCanvas * Canvas = SessionCombo->Canvas;

  int MaxWidth = 0, Width;
  for (int i = 0; i < SessionCombo->Items->Count; i++)
  {
    Width = Canvas->TextExtent(SessionCombo->Items->Strings[i]).cx;
    TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(i);
    if (ShortCut != scNone)
    {
      Width += Canvas->TextExtent(ShortCutToText(ShortCut)).cx;
    }
    if (Width > MaxWidth)
    {
      MaxWidth = Width;
    }
  }
  MaxWidth += 8;
  if (SessionCombo->Items->Count > ((TComboBox *)SessionCombo)->DropDownCount)
  {
    MaxWidth += GetSystemMetrics(SM_CXVSCROLL);
  }
  SessionCombo->Perform(CB_SETDROPPEDWIDTH, MaxWidth + 8 + 1, 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboDrawItem(
  TWinControl * Control, int Index, const TRect & Rect, TOwnerDrawState /*State*/)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Control);
  assert(SessionCombo);
  TCanvas * Canvas = SessionCombo->Canvas;
  Canvas->FillRect(Rect);

  if (Index >= 0)
  {
    int ShortCutWidth = 0;
    AnsiString ShortCutStr;
    if (Rect.Top != 3)
    {
      TShortCut ShortCut = NonVisualDataModule->OpenSessionShortCut(Index);
      if (ShortCut != scNone)
      {
        ShortCutStr = ShortCutToText(ShortCut);
        ShortCutWidth = Canvas->TextExtent(ShortCutStr).cx;
      }
    }

    TRect R = Rect;
    R.Right -= ShortCutWidth + 2;
    Canvas->TextRect(R, R.Left + 2, R.Top, SessionCombo->Items->Strings[Index]);
    R = Rect;
    R.Left = R.Right - ShortCutWidth - 2;
    Canvas->TextRect(R, R.Left, R.Top, ShortCutStr);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionComboChange(TObject * Sender)
{
  TCustomComboBox * SessionCombo = dynamic_cast<TCustomComboBox *>(Sender);
  assert(SessionCombo);
  TTerminal * Terminal;
  Terminal = dynamic_cast<TTerminal *>(SessionCombo->Items->Objects[SessionCombo->ItemIndex]);
  assert(Terminal);
  TTerminalManager::Instance()->ActiveTerminal = Terminal;
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
void __fastcall TCustomScpExplorerForm::DoShow()
{
  FixControlsPlacement();

  TForm::DoShow();
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::MoreMessageDialog(const AnsiString Message,
    TStrings * MoreMessages, TQueryType Type, int Answers,
    int HelpCtx, const TMessageParams * Params)
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
    return ::MoreMessageDialog(Message, MoreMessages, Type, Answers, HelpCtx, Params);
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
    ExcludeTrailingBackslash(WinConfiguration->TemporaryTranferDir());
  ForceDirectories(FDragExtFakeDirectory);
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
  if (DropSourceControl == RemoteDriveView)
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
        Param.DragDrop = false;

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
    (IsFileControl(FDDTargetControl, osRemote) ||
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
    // when move from remote side is disabled, we allow coying inside the remote
    // panel, but we interpret is as moving (we also slip in the move cursor)
    if ((FLastDropEffect == DROPEFFECT_MOVE) ||
        (!WinConfiguration->DDAllowMoveInit && (FLastDropEffect == DROPEFFECT_COPY)))
    {
      RemoteFileControlFileOperation(DropSourceControl,
        foRemoteMove, !WinConfiguration->DDTransferConfirmation, NULL);
    }
    // abort drag&drop
    Abort();
  }
  else if ((FDDExtMapFile == NULL) && (FLastDropEffect != DROPEFFECT_NONE))
  {
    assert(!FDragTempDir.IsEmpty());
    TTransferType Type;
    AnsiString TempDir = FDragTempDir;
    // We clear FUniqTempDir before calling
    // just in case it fail (raises exception)
    FDragTempDir = "";
    Type = (FLastDropEffect & DROPEFFECT_MOVE ? ttMove : Type = ttCopy);

    TGUICopyParamType CopyParams = GUIConfiguration->CopyParam;
    // empty directory parameter means temp directory -> don't display it!
    AnsiString TargetDir = "";

    if (!CopyParamDialog(tdToLocal, Type, true, FDDFileList,
          TargetDir, CopyParams, WinConfiguration->DDTransferConfirmation))
    {
      Abort();
    }

    // TargetDir is set when dropped on local file control
    bool TemporaryDownload = TargetDir.IsEmpty();
    bool Continue = true;

    if (TemporaryDownload)
    {
      DoWarnLackOfTempSpace(TempDir, FDDTotalSize, Continue);
      TargetDir = TempDir;
    }

    if (Continue)
    {
      if (TemporaryDownload)
      {
        if (ForceDirectories(TargetDir))
        {
          assert(Terminal && !TargetDir.IsEmpty());
          try
          {
            // cpNewerOnly has no efect here,
            // as we download to empty temp directory
            int Params = cpDragDrop |
              (Type == ttMove ? cpDelete : 0);
            Terminal->CopyToLocal(FDDFileList, TargetDir, &CopyParams,
              Params);
          }
          __finally
          {
            AddDelayedDirectoryDeletion(TargetDir, WinConfiguration->DDDeleteDelay);
          }
        }
        else
        {
          throw Exception(FMTLOAD(DD_DIR_EXCEPTION, (TargetDir)));
        }
      }
    }
    else
    {
      Abort();
    }
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
    if (Export == pePath)
    {
      ExportData->Add(DirView->PathName);
    }
    else
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
    if (ExportData->Count == 1)
    {
      CopyToClipboard(ExportData->Strings[0]);
    }
    else
    {
      CopyToClipboard(ExportData->Text);
    }
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
  TQueueOperation Operation)
{
  switch (Operation)
  {
    case qoPreferences:
      return true;

    case qoGoTo:
      return ComponentVisible[fcQueueView];

    default:
      return FQueueController->AllowOperation(Operation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteQueueOperation(
  TQueueOperation Operation)
{
  if (Operation == qoGoTo)
  {
    assert(QueueView->Visible);
    QueueView->SetFocus();
  }
  else if (Operation == qoPreferences)
  {
    DoPreferencesDialog(pmQueue);
  }
  else
  {
    FQueueController->ExecuteOperation(Operation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueViewContextPopup(
  TObject * /*Sender*/, TPoint & /*MousePos*/, bool & /*Handled*/)
{
  FQueueActedItem = QueueView->ItemFocused;
}
//---------------------------------------------------------------------------
/*virtual*/ int __fastcall TCustomScpExplorerForm::GetStaticComponentsHeight()
{
  return TopCoolBar->Height + QueueSplitter->Height;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueSplitterCanResize(
  TObject * /*Sender*/, int & NewSize, bool & /*Accept*/)
{
  int HeightLimit = ClientHeight - GetStaticComponentsHeight() -
    RemotePanel->Constraints->MinHeight;

  if (NewSize > HeightLimit)
  {
    NewSize = HeightLimit;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StatusBarResize(
  TObject * Sender)
{
  RepaintStatusBar(dynamic_cast<TCustomStatusBar *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueViewStartDrag(TObject * /*Sender*/,
  TDragObject *& /*DragObject*/)
{
  FQueueActedItem = QueueView->ItemFocused;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueViewDragOver(TObject * /*Sender*/,
  TObject * Source, int X, int Y, TDragState /*State*/, bool & Accept)
{
  Accept = true;
  if (Source == QueueView)
  {
    TListItem * DropTarget = QueueView->GetItemAt(X, Y);
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
void __fastcall TCustomScpExplorerForm::QueueViewDragDrop(TObject * /*Sender*/,
  TObject * /*Source*/, int /*X*/, int /*Y*/)
{
  if ((FQueueActedItem != NULL) && (QueueView->DropTarget != NULL))
  {
    TQueueItemProxy * QueueItem;
    TQueueItemProxy * DestQueueItem;

    QueueItem = static_cast<TQueueItemProxy *>(FQueueActedItem->Data);
    DestQueueItem = static_cast<TQueueItemProxy *>(QueueView->DropTarget->Data);
    QueueItem->Move(DestQueueItem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueViewEnter(TObject * /*Sender*/)
{
  if ((QueueView->ItemFocused == NULL) &&
      (QueueView->Items->Count > 0))
  {
    QueueView->ItemFocused = QueueView->Items->Item[0];
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueViewSelectItem(
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

    TTransferOperationParam Param;
    Param.TargetDirectory = TargetPath;
    Param.DragDrop = true;
    ExecuteFileOperation(Operation, osLocal, FileList, false, &Param);
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileContolDDChooseEffect(
  TObject * Sender, int /*grfKeyState*/, int & dwEffect)
{
  if ((DropSourceControl == RemoteDriveView) &&
      (RemoteDriveView->DragNode == RemoteDriveView->DropTarget))
  {
    dwEffect = DROPEFFECT_None;
  }
  else if (IsFileControl(DropSourceControl, osRemote))
  {
    if (((Sender == RemoteDriveView) && (RemoteDriveView->DropTarget != NULL)) ||
        ((Sender == RemoteDirView) &&
         ((RemoteDirView->DropTarget != NULL) || (DropSourceControl != RemoteDirView))))
    {
      // Hack: when moving is disabled, we at least allow to accept copy
      // but it should be interpreted as move
      dwEffect = WinConfiguration->DDAllowMoveInit ? DROPEFFECT_Move : DROPEFFECT_Copy;
    }
    else
    {
      dwEffect = DROPEFFECT_None;
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
  FileName = FDragTempDir + GUIConfiguration->CopyParam.ValidLocalFileName(File->FileName);

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
  FDragTempDir = WinConfiguration->TemporaryTranferDir();
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
      Terminal->DoShowExtendedException(&E);
    }
  }
}
//---------------------------------------------------------------------------

