//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "CustomScpExplorer.h"

#include <Bookmarks.h>
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
#include <WinApi.h>

#include "GUITools.h"
#include "NonVisual.h"
#include "Glyphs.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "EditorManager.h"
#include "Setup.h"
#include <Consts.hpp>
#include <DateUtils.hpp>
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
  UnicodeString FocusFile = L""; \
  UnicodeString LastFocusedFile = L""; \
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

  UnicodeString TargetDirectory;
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
class TTransferPresetNoteData : public TObject
{
public:
  UnicodeString Message;
  TStrings * More;

  virtual __fastcall ~TTransferPresetNoteData()
  {
    delete More;
  }
};
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::TCustomScpExplorerForm(TComponent* Owner):
    FFormRestored(false),
    TForm(Owner),
    FDirViewMatchMask(-1, true)
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
  // CreateMutexW keeps failing with ERROR_NOACCESS
  FDDExtMutex = CreateMutexA(NULL, false, AnsiString(DRAG_EXT_MUTEX).c_str());
  assert(FDDExtMutex != NULL);
  FDDTargetControl = NULL;
  FDelayedDeletionTimer = NULL;
  FDelayedDeletionList = new TStringList();
  FDDFileList = NULL;
  FPendingTempSpaceWarn = false;
  FCapturedLog = NULL;
  FDragDropOperation = false;
  memset(&FHistoryMenu, 0, sizeof(FHistoryMenu));
  FAllowTransferPresetAutoSelect = true;
  FCopyParamDefault = L"";
  FSynchronizeController = NULL;
  FPendingQueueActionItem = NULL;
  FLockLevel = 0;
  FAlternativeDelete = false;
  FTrayIcon = new ::TTrayIcon(0);
  FTrayIcon->OnClick = TrayIconClick;
  FMaxQueueLength = 0;

  FEditorManager = new TEditorManager();
  FEditorManager->OnFileChange = ExecutedFileChanged;
  FEditorManager->OnFileReload = ExecutedFileReload;
  FEditorManager->OnFileEarlyClosed = ExecutedFileEarlyClosed;

  FLocalEditors = new TList();

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

  FOle32Library = LoadLibrary(L"Ole32.dll");
  FDragMoveCursor = FOle32Library != NULL ?
    LoadCursor(FOle32Library, MAKEINTRESOURCE(2)) : NULL;

  UseSystemSettings(this);

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
  TTBXPublicItem * ColorMenu;
  ColorMenu =
    reinterpret_cast<TTBXPublicItem *>(
      dynamic_cast<TTBXCustomItem *>(
        static_cast<TObject *>(GetComponent(fcColorMenu))));
  assert(ColorMenu != NULL);
  ColorMenu->ItemStyle = ColorMenu->ItemStyle << tbisSubmenu;
  ColorMenu =
    reinterpret_cast<TTBXPublicItem *>(
      dynamic_cast<TTBXCustomItem *>(
        static_cast<TObject *>(NonVisualDataModule->ColorMenuItem)));
  assert(ColorMenu != NULL);
  ColorMenu->ItemStyle = ColorMenu->ItemStyle << tbisSubmenu;
  TTBXColorPalette * ColorPalette =
    dynamic_cast<TTBXColorPalette *>(
      static_cast<TObject *>(GetComponent(fcColorPalette)));
  assert(ColorPalette != NULL);
  ColorPalette->OnChange = NonVisualDataModule->SessionColorPaletteChange;

  RemoteDirView->Font = Screen->IconFont;

  reinterpret_cast<TLabel*>(QueueSplitter)->OnDblClick = QueueSplitterDblClick;

  FSystemImageList = SharedSystemImageList(false);
  FSystemImageList->DrawingStyle = dsTransparent;

  FCustomCommandMenu = CreateTBXPopupMenu(this);
  FCustomCommandLocalFileList = NULL;
  FCustomCommandRemoteFileList = NULL;

  FSessionColors = new TImageList(this);
  FSessionColors->SetSize(10, 10);
  SessionsPageControl->Images = FSessionColors;

  StartUpdates();
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
  SessionsPageControl->Images = NULL;
  SAFE_DESTROY(FSessionColors);
  SAFE_DESTROY(FSessionsDragDropFilesEx);

  delete FCustomCommandLocalFileList;
  delete FCustomCommandRemoteFileList;
  delete FCustomCommandMenu;

  delete FSystemImageList;
  FSystemImageList = NULL;

  if (Application->OnHint == ApplicationHint)
  {
    Application->OnHint = NULL;
  }
  assert(Application->OnMinimize == ApplicationMinimize);
  Application->OnMinimize = NULL;
  assert(Application->OnRestore == ApplicationRestore);
  Application->OnRestore = NULL;
  delete FTrayIcon;
  FTrayIcon = NULL;

  FEditorManager->CloseInternalEditors(ForceCloseInternalEditor);
  delete FEditorManager;

  ForceCloseLocalEditors();
  delete FLocalEditors;

  if (FDelayedDeletionTimer)
  {
    DoDelayedDeletion(NULL);
    SAFE_DESTROY(FDelayedDeletionTimer);
  }
  SAFE_DESTROY(FDelayedDeletionList);
  // sometimes we do not get DDEnd so the list is not released
  SAFE_DESTROY(FDDFileList);

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
    bool PrevAllowTransferPresetAutoSelect = FAllowTransferPresetAutoSelect;
    FAllowTransferPresetAutoSelect = false;
    try
    {
      TerminalChanged();
    }
    __finally
    {
      FAllowTransferPresetAutoSelect = PrevAllowTransferPresetAutoSelect;
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
  NonVisualDataModule->ResetQueueOnceEmptyOperation();
  if (Terminal)
  {
    if (Terminal->Active)
    {
      Terminal->RefreshDirectory();
    }

    TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
    assert(ManagedTerminal != NULL);

    if (WinConfiguration->PreservePanelState)
    {
      if (ManagedTerminal->RemoteExplorerState != NULL)
      {
        RemoteDirView->RestoreState(ManagedTerminal->RemoteExplorerState);
      }
      else
      {
        RemoteDirView->ClearState();
      }
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
void __fastcall TCustomScpExplorerForm::UpdateQueueStatus(bool AppIdle, bool QueueChanging)
{
  {
    TGuard Guard(FQueueStatusSection);

    FQueueStatusInvalidated = false;

    if (FQueue != NULL)
    {
      FQueueStatus = FQueue->CreateStatus(FQueueStatus);
    }
  }

  if ((FQueueStatus != NULL) && (FQueueStatus->Count > FMaxQueueLength))
  {
    FMaxQueueLength = FQueueStatus->Count;
    Configuration->Usage->SetMax(L"MaxQueueLength", FMaxQueueLength);
  }

  FQueueController->UpdateQueueStatus(FQueueStatus);

  UpdateQueueView();

  bool IsEmpty = (FQueueStatus == NULL) || (FQueueStatus->Count == 0);

  if (IsEmpty && (Terminal != NULL))
  {
    TOnceDoneOperation OnceDoneOperation =
      NonVisualDataModule->CurrentQueueOnceEmptyOperation();
    NonVisualDataModule->ResetQueueOnceEmptyOperation();

    if ((FQueue != NULL) && !WinConfiguration->EnableQueueByDefault && !QueueChanging)
    {
      FQueue->Enabled = false;
    }

    if ((OnceDoneOperation != odoIdle) && AppIdle)
    {
      Terminal->CloseOnCompletion(OnceDoneOperation, LoadStr(CLOSED_ON_QUEUE_EMPTY));
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
  UpdateQueueStatus(false, true);
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

    // this may be running in parallel with QueueChanged
    if (FQueueStatus != NULL)
    {
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
    UnicodeString Name = TransferList->Strings->Strings[TransferList->ItemIndex];
    TransferDropDown->Text = StripHotkey(Name);
    TransferDropDown->Hint = FORMAT(L"%s\n \n%s:\n%s|%s",
      (FTransferDropDownHint, StripHotkey(Name),
       GUIConfiguration->CurrentCopyParam.GetInfoStr(L"; ",
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
  RemoteDirView->FormatSizeBytes = WinConfiguration->FormatSizeBytes;
  RemoteDirView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;

  if (RemoteDirView->RowSelect != WinConfiguration->FullRowSelect)
  {
    RemoteDirView->RowSelect = WinConfiguration->FullRowSelect;
    // selection is not redrawn automatically when RowSelect changes
    RemoteDirView->Invalidate();
  }

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
  if (FEditorManager != NULL)
  {
    FEditorManager->ProcessFiles(FileConfigurationChanged, NULL);
  }

  // this can be called even before constuctor finishes.
  if (FLocalEditors != NULL)
  {
    for (int Index = 0; Index < FLocalEditors->Count; Index++)
    {
      ReconfigureEditorForm(static_cast<TForm *>(FLocalEditors->Items[Index]));
    }
  }

  if (ComponentVisible[fcCustomCommandsBand])
  {
    UpdateCustomCommandsToolbar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileConfigurationChanged(
  const UnicodeString FileName, TEditedFileData & /*Data*/, TObject * Token,
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
  TStrings * FileList, UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam,
  bool Confirm, bool DragDrop)
{
  bool Result = true;
  assert(Terminal && Terminal->Active);
  // Temp means d&d here so far, may change in future!
  if (Temp && (Direction == tdToLocal) && (Type == ttMove) &&
      !WinConfiguration->DDAllowMove)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    unsigned int Answer = MessageDialog(LoadStr(DND_DOWNLOAD_MOVE_WARNING), qtWarning,
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

    Configuration->Usage->Inc(L"TransfersOnBackground");

    // these parameter are known only after transfer dialog
    Params |=
      (CopyParam.QueueNoConfirmation ? cpNoConfirmation : 0) |
      (CopyParam.NewerOnly ? cpNewerOnly : 0);

    if (CopyParam.QueueIndividually)
    {
      for (int Index = 0; Index < FileList->Count; Index++)
      {
        TStrings * FileList1 = new TStringList();
        try
        {
          FileList1->AddObject(FileList->Strings[Index], FileList->Objects[Index]);
          AddQueueItem(Direction, FileList1, TargetDirectory, CopyParam, Params);
        }
        __finally
        {
          delete FileList1;
        }
      }
    }
    else
    {
      AddQueueItem(Direction, FileList, TargetDirectory, CopyParam, Params);
    }
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
void __fastcall TCustomScpExplorerForm::AddQueueItem(
  TTransferDirection Direction, TStrings * FileList,
  const UnicodeString TargetDirectory, const TCopyParamType & CopyParam,
  int Params)
{
  assert(Queue != NULL);

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
void __fastcall TCustomScpExplorerForm::LoadToolbarsLayoutStr(UnicodeString LayoutStr)
{
  SetDockAllowDrag(true);
  ::LoadToolbarsLayoutStr(this, LayoutStr);
  SetDockAllowDrag(!WinConfiguration->LockToolbars);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::GetToolbarsLayoutStr()
{
  UnicodeString Result;
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
    FProgressForm->DeleteToRecycleBin = (ProgressData.Side == osLocal ?
      (WinConfiguration->DeleteToRecycleBin != FAlternativeDelete) :
      ((Terminal->SessionData->DeleteToRecycleBin != FAlternativeDelete) &&
       !Terminal->SessionData->RecycleBinPath.IsEmpty()));
    // When main window is hidden, synchronisation form does not exist,
    // we suppose "/upload" or URL download mode
    if (!Visible && (ProgressData.Operation != foCalculateSize) &&
        (ProgressData.Operation != foCalculateChecksum) &&
        (FSynchronizeProgressForm == NULL))
    {
      FProgressForm->OnceDoneOperation = odoDisconnect;
    }

    if (FTaskbarList != NULL)
    {
      FTaskbarList->SetProgressState(Handle, TBPF_NORMAL);
    }
  }
  // operation is finished (or terminated), so we hide progress form
  else if (!ProgressData.InProgress && FProgressForm)
  {
    if (FTaskbarList != NULL)
    {
      FTaskbarList->SetProgressState(Handle, TBPF_NOPROGRESS);
    }

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

    if (FTaskbarList != NULL)
    {
      assert(ProgressData.InProgress);
      FTaskbarList->SetProgressValue(Handle, ProgressData.OverallProgress(), 100);
    }

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
  bool /*Temp*/, const UnicodeString & FileName, bool Success,
  TOnceDoneOperation & OnceDoneOperation)
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
      UnicodeString FileNameOnly = ExtractFileName(FileName, (Side == osRemote));
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

    if ((Operation == foCopy) || (Operation == foMove))
    {
      if (Side == osLocal)
      {
        Configuration->Usage->Inc(L"UploadedFiles");
      }
      else
      {
        Configuration->Usage->Inc(L"DownloadedFiles");
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
    OnceDoneOperation = FProgressForm->OnceDoneOperation;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool Temp, const UnicodeString & FileName, Boolean Success,
  TOnceDoneOperation & OnceDoneOperation)
{
  DoOperationFinished(Operation, Side, Temp, FileName, Success,
    OnceDoneOperation);
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
  short int Side;
  short int Index;
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HistoryGo(TOperationSide Side, int Index)
{
  DirView(Side)->HistoryGo(Index);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HistoryItemClick(System::TObject* Sender)
{
  TTBCustomItem * Item = dynamic_cast<TTBCustomItem *>(Sender);
  THistoryItemData Data = *reinterpret_cast<THistoryItemData*>(&(Item->Tag));
  HistoryGo((TOperationSide)Data.Side, Data.Index);
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
    // contructor in C++, leading in problems, the function does
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
  return (FTerminal != NULL) && (FTerminal->IsCapable[fcSecondaryShell] || FTerminal->IsCapable[fcShellAnyCommand]);
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::BothCustomCommandState(const TCustomCommandType & Command)
{
  bool Result = FLAGSET(Command.Params, ccLocal);
  if (Result)
  {
    TLocalCustomCommand LocalCustomCommand;
    TInteractiveCustomCommand InteractiveCustomCommand(&LocalCustomCommand);
    UnicodeString Cmd = InteractiveCustomCommand.Complete(Command.Command, false);

    Result =
      LocalCustomCommand.IsFileCommand(Cmd) &&
      LocalCustomCommand.HasLocalFileName(Cmd);
  }
  return Result ? 1 : -1;
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::CustomCommandState(
  const TCustomCommandType & Command, bool OnFocused)
{
  int Result;

  TFileCustomCommand RemoteCustomCommand;
  TLocalCustomCommand LocalCustomCommand;
  TFileCustomCommand * NonInteractiveCustomCommand =
    FLAGCLEAR(Command.Params, ccLocal) ? &RemoteCustomCommand : &LocalCustomCommand;
  TInteractiveCustomCommand InteractiveCustomCommand(NonInteractiveCustomCommand);
  UnicodeString Cmd = InteractiveCustomCommand.Complete(Command.Command, false);

  if (FLAGCLEAR(Command.Params, ccLocal))
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
void __fastcall TCustomScpExplorerForm::CustomCommand(TStrings * FileList,
  const TCustomCommandType & ACommand, TStrings * ALocalFileList)
{
  if (FLAGCLEAR(ACommand.Params, ccLocal))
  {
    if (EnsureCommandSessionFallback(fcShellAnyCommand))
    {
      TCustomCommandData Data(Terminal);
      TRemoteCustomCommand RemoteCustomCommand(Data, Terminal->CurrentDirectory);
      TWinInteractiveCustomCommand InteractiveCustomCommand(
        &RemoteCustomCommand, ACommand.Name);

      UnicodeString Command = InteractiveCustomCommand.Complete(ACommand.Command, false);

      Configuration->Usage->Inc(L"LocalCustomCommandRuns");

      bool Capture = FLAGSET(ACommand.Params, ccShowResults) || FLAGSET(ACommand.Params, ccCopyResults);
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
          Terminal->CustomCommandOnFiles(Command, ACommand.Params, FileList, OutputEvent);
        }

        if ((FCapturedLog != NULL) && (FCapturedLog->Count > 0))
        {
          if (FLAGSET(ACommand.Params, ccCopyResults))
          {
            CopyToClipboard(FCapturedLog);
          }

          if (FLAGSET(ACommand.Params, ccShowResults))
          {
            DoConsoleDialog(Terminal, L"", FCapturedLog);
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
      &LocalCustomCommand, ACommand.Name);

    UnicodeString Command = InteractiveCustomCommand.Complete(ACommand.Command, false);

    Configuration->Usage->Inc(L"RemoteCustomCommandRuns");

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
          if (ALocalFileList == NULL)
          {
            assert(HasDirView[osLocal]);
            assert(EnableSelectedOperation[osLocal]);
            LocalFileList = DirView(osLocal)->CreateFileList(false, true, NULL);
          }
          else
          {
            LocalFileList = ALocalFileList;
          }

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

        UnicodeString RootTempDir;
        UnicodeString TempDir;

        TemporarilyDownloadFiles(FileList, false, RootTempDir, TempDir, false, false, true);

        try
        {
          RemoteFileList = new TStringList();

          TMakeLocalFileListParams MakeFileListParam;
          MakeFileListParam.FileList = RemoteFileList;
          MakeFileListParam.IncludeDirs = FLAGSET(ACommand.Params, ccApplyToDirectories);
          MakeFileListParam.Recursive =
            FLAGSET(ACommand.Params, ccRecursive) && !FileListCommand;

          ProcessLocalDirectory(TempDir, Terminal->MakeLocalFileList, &MakeFileListParam);

          TFileOperationProgressType Progress(&OperationProgress, &OperationFinished);

          Progress.Start(foCustomCommand, osRemote, FileListCommand ? 1 : FileList->Count);
          assert(FProgressForm != NULL);
          FProgressForm->ReadOnly = true;

          try
          {
            if (FileListCommand)
            {
              UnicodeString LocalFile;
              // MakeFileList does not delimit filenames
              UnicodeString FileList = MakeFileList(RemoteFileList);

              if (LocalFileCommand)
              {
                assert(LocalFileList->Count == 1);
                LocalFile = LocalFileList->Strings[0];
              }

              TCustomCommandData Data(FTerminal);
              TLocalCustomCommand CustomCommand(Data,
                Terminal->CurrentDirectory, L"", LocalFile, FileList);
              ExecuteShellAndWait(CustomCommand.Complete(Command, true));
            }
            else if (LocalFileCommand)
            {
              if (LocalFileList->Count == 1)
              {
                UnicodeString LocalFile = LocalFileList->Strings[0];

                for (int Index = 0; Index < RemoteFileList->Count; Index++)
                {
                  UnicodeString FileName = RemoteFileList->Strings[Index];
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(Data,
                    Terminal->CurrentDirectory, FileName, LocalFile, L"");
                  ExecuteShellAndWait(CustomCommand.Complete(Command, true));
                }
              }
              else if (RemoteFileList->Count == 1)
              {
                UnicodeString FileName = RemoteFileList->Strings[0];

                for (int Index = 0; Index < LocalFileList->Count; Index++)
                {
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(
                    Data, Terminal->CurrentDirectory,
                    FileName, LocalFileList->Strings[Index], L"");
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
                  UnicodeString FileName = RemoteFileList->Strings[Index];
                  TCustomCommandData Data(FTerminal);
                  TLocalCustomCommand CustomCommand(
                    Data, Terminal->CurrentDirectory,
                    FileName, LocalFileList->Strings[Index], L"");
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
                  Terminal->CurrentDirectory, RemoteFileList->Strings[Index], L"", L"");
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
        if (LocalFileList != ALocalFileList)
        {
          delete LocalFileList;
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BothCustomCommand(
  const TCustomCommandType & Command)
{
  assert(FCustomCommandLocalFileList != NULL);
  assert(FCustomCommandRemoteFileList != NULL);
  assert(FCustomCommandLocalFileList->Count == FCustomCommandRemoteFileList->Count);

  TStrings * LocalFileList = new TStringList();
  TStrings * RemoteFileList = new TStringList();
  try
  {
    for (int Index = 0; Index < FCustomCommandLocalFileList->Count; Index++)
    {
      LocalFileList->Clear();
      LocalFileList->AddObject(
        FCustomCommandLocalFileList->Strings[Index],
        FCustomCommandLocalFileList->Objects[Index]);
      RemoteFileList->Clear();
      RemoteFileList->AddObject(
        FCustomCommandRemoteFileList->Strings[Index],
        FCustomCommandRemoteFileList->Objects[Index]);

      CustomCommand(RemoteFileList, Command, LocalFileList);
    }
  }
  __finally
  {
    delete LocalFileList;
    delete RemoteFileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomCommandMenu(
  TObject * Sender, const TPoint & MousePos,
  TStrings * LocalFileList, TStrings * RemoteFileList)
{
  FCustomCommandMenu->Items->Clear();
  delete FCustomCommandLocalFileList;
  delete FCustomCommandRemoteFileList;
  // takeover ownership,
  // the lists must survive the MenuPopup as OnClick occurs only after it exits
  FCustomCommandLocalFileList = LocalFileList;
  FCustomCommandRemoteFileList = RemoteFileList;

  NonVisualDataModule->CreateCustomCommandsMenu(FCustomCommandMenu->Items, false, false, true);

  MenuPopup(FCustomCommandMenu, MousePos, dynamic_cast<TComponent *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalCaptureLog(
  const UnicodeString & AddedLine, bool /*StdError*/)
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
void __fastcall TCustomScpExplorerForm::ReloadLocalDirectory(const UnicodeString Directory)
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
void __fastcall TCustomScpExplorerForm::UpdateCopyParamCounters(
  const TCopyParamType & CopyParam)
{
  if (!CopyParam.IncludeFileMask.Masks.IsEmpty())
  {
    Configuration->Usage->Inc(L"FileMaskUses");
  }
  if (IsEffectiveFileNameMask(CopyParam.FileMask))
  {
    Configuration->Usage->Inc(L"OperationMaskUses");
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * Param)
{
  void * BatchStorage;
  BatchStart(BatchStorage);
  bool Result;
  try
  {
    if ((Operation == foCopy) || (Operation == foMove))
    {
      TTransferDirection Direction = (Side == osLocal ? tdToRemote : tdToLocal);
      TTransferType Type = (Operation == foCopy ? ttCopy : ttMove);
      UnicodeString TargetDirectory;
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
      Result =
        CopyParamDialog(Direction, Type, Temp, FileList, TargetDirectory,
          CopyParam, !NoConfirmation, DragDrop);
      if (Result)
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

        UpdateCopyParamCounters(CopyParam);

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
      Result = true;
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
          !Terminal->SessionData->RecycleBinPath.IsEmpty() &&
          !Terminal->IsRecycledFile(FileList->Strings[0]);
      }

      Result =
        !(Recycle ? WinConfiguration->ConfirmRecycling : WinConfiguration->ConfirmDeleting);
      if (!Result)
      {
        UnicodeString Query;
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
        unsigned int Answer = MessageDialog(Query, qtConfirmation,
          qaOK | qaCancel, HELP_NONE, &Params);
        if (Answer == qaNeverAskAgain)
        {
          Result = true;
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
          Result = (Answer == qaOK);
        }
      }

      if (Result)
      {
        DeleteFiles(Side, FileList, FLAGMASK(Alternative, dfAlternative));
      }
    }
    else if (Operation == foSetProperties)
    {
      RemoteDirView->SaveSelectedNames();
      Result = SetProperties(Side, FileList);
    }
    else if (Operation == foCustomCommand)
    {
      assert(Param);
      assert(Side == osRemote);

      RemoteDirView->SaveSelectedNames();
      const TCustomCommandType * Command = static_cast<const TCustomCommandType*>(Param);
      CustomCommand(FileList, *Command, NULL);
      Result = true;
    }
    else if ((Operation == foRemoteMove) || (Operation == foRemoteCopy))
    {
      assert(Side == osRemote);
      Result = RemoteTransferFiles(FileList, NoConfirmation,
        (Operation == foRemoteMove), reinterpret_cast<TTerminal *>(Param));
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
  return Result;
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
bool __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, bool OnFocused, bool NoConfirmation, void * Param)
{
  Side = GetSide(Side);

  bool Result;
  TStrings * FileList = DirView(Side)->CreateFileList(OnFocused, (Side == osLocal), NULL);
  try
  {
    Result = ExecuteFileOperation(Operation, Side, FileList, NoConfirmation, Param);
  }
  __finally
  {
    delete FileList;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileOperationCommand(
  TFileOperation Operation, TOperationSide Side, bool OnFocused,
  bool NoConfirmation, void * Param)
{
  if (ExecuteFileOperation(Operation, Side, OnFocused, NoConfirmation, Param))
  {
    if ((Operation == foCopy) || (Operation == foMove))
    {
      if (GetSide(Side) == osLocal)
      {
        Configuration->Usage->Inc(L"UploadsCommand");
      }
      else
      {
        Configuration->Usage->Inc(L"DownloadsCommand");
      }
    }
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
        unsigned int Answer;
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

  TCustomDirView * CurrentDirView = DirView(osCurrent);
  TListItem * FocusedItem = CurrentDirView->ItemFocused;
  UnicodeString Name;
  if ((FocusedItem != NULL) && !CurrentDirView->ItemIsParentDirectory(FocusedItem))
  {
    Name = CurrentDirView->ItemFileName(FocusedItem);
  }
  else
  {
    Name = LoadStr(NEW_FILE);
  }
  TStrings * History = CustomWinConfiguration->History[L"EditFile"];
  if (InputDialog(LoadStr(EDIT_FILE_CAPTION), LoadStr(EDIT_FILE_PROMPT), Name,
        HELP_EDIT_NEW, History, true))
  {
    CustomWinConfiguration->History[L"EditFile"] = History;
    UnicodeString TargetFileName;
    UnicodeString LocalFileName;
    UnicodeString RootTempDir;
    UnicodeString TempDir;
    UnicodeString RemoteDirectory;
    if (Side == osRemote)
    {
      Name = AbsolutePath(FTerminal->CurrentDirectory, Name);

      TRemoteFile * File = NULL;
      if (FTerminal->FileExists(Name, &File))
      {
        try
        {
          // needed for checking filemasks, as there's no directory object
          // associated with the file object
          File->FullFileName = Name;

          TFileMasks::TParams MaskParams;
          MaskParams.Size = File->Size;
          MaskParams.Modification = File->Modification;

          ExecuteFile(Side, efDefaultEditor, NULL, Name, File, MaskParams);

          return;
        }
        __finally
        {
          delete File;
        }
      }

      RemoteDirectory = UnixExtractFilePath(Name);
      TemporaryDirectoryForRemoteFiles(
        RemoteDirectory, GUIConfiguration->CurrentCopyParam, TempDir, RootTempDir);

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
    const TEditorData * ExternalEditor = NULL;
    TFileMasks::TParams MaskParams; // size not known
    ExecuteFileNormalize(ExecuteFileBy, ExternalEditor, TargetFileName,
      false, MaskParams);

    CustomExecuteFile(Side, ExecuteFileBy, LocalFileName, TargetFileName,
      ExternalEditor, RootTempDir, RemoteDirectory);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteExecuteForceText(
  TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor)
{
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));
  assert(ExecuteFileBy != efDefaultEditor);

  return
    ((ExecuteFileBy == efInternalEditor)) ||
    ((ExecuteFileBy == efExternalEditor) && ExternalEditor->ExternalEditorText);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, UnicodeString FileName, UnicodeString OriginalFileName,
  const TEditorData * ExternalEditor, UnicodeString LocalRootDirectory,
  UnicodeString RemoteDirectory)
{
  assert(!WinConfiguration->DisableOpenEdit);
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));
  assert(ExecuteFileBy != efDefaultEditor);

  Side = GetSide(Side);

  TEditedFileData Data;
  if (Side == osRemote)
  {
    Data.Terminal = Terminal;
    Data.Queue = Queue;
    Data.ForceText = RemoteExecuteForceText(ExecuteFileBy, ExternalEditor);
    Data.RemoteDirectory = RemoteDirectory;
    Data.SessionName = Terminal->SessionData->SessionName;
    Data.LocalRootDirectory = LocalRootDirectory;
    Data.OriginalFileName = OriginalFileName;
    Data.Command = L""; // will be changed later for external editor
  }

  if (ExecuteFileBy == efInternalEditor)
  {
    if (Side == osRemote)
    {
      UnicodeString Caption = UnixIncludeTrailingBackslash(RemoteDirectory) + OriginalFileName +
        L" - " + Terminal->SessionData->SessionName;
      TForm * Editor;
      try
      {
        Editor = ShowEditorForm(FileName, this, FEditorManager->FileChanged,
          FEditorManager->FileReload, FEditorManager->FileClosed, Caption);
      }
      catch(...)
      {
        try
        {
          RecursiveDeleteFile(ExcludeTrailingBackslash(LocalRootDirectory), false);
        }
        catch(...)
        {
        }
        throw;
      }

      FEditorManager->AddFileInternal(FileName, Data, Editor);
    }
    else
    {
      FLocalEditors->Add(ShowEditorForm(FileName, this, NULL, NULL, LocalEditorClosed, L""));
    }
  }
  else
  {
    HANDLE Process;

    if (ExecuteFileBy == efExternalEditor)
    {
      UnicodeString Program, Params, Dir;
      Data.Command = ExternalEditor->ExternalEditor;
      ReformatFileNameCommand(Data.Command);
      SplitCommand(Data.Command, Program, Params, Dir);
      Params = ExpandFileNameCommand(Params, FileName);
      Program = ExpandEnvironmentVariables(Program);
      if (!ExecuteShell(Program, Params, Process))
      {
        throw Exception(FMTLOAD(EDITOR_ERROR, (Program)));
      }
    }
    else
    {
      assert(Side == osRemote);
      if (!ExecuteShell(FileName, L"", Process))
      {
        throw Exception(FMTLOAD(EXECUTE_FILE_ERROR, (FileName)));
      }
    }

    if ((Side == osLocal) ||
        ((ExecuteFileBy == efShell) &&
         !WinConfiguration->Editor.SDIShellEditor) ||
        ((ExecuteFileBy == efExternalEditor) &&
         !ExternalEditor->SDIExternalEditor))
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
        throw ExtException(LoadStr(OPEN_FILE_NO_PROCESS2), L"", HELP_OPEN_FILE_NO_PROCESS);
      }
    }

    if (Side == osRemote)
    {
      FEditorManager->AddFileExternal(FileName, Data, Process);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalEditorClosed(TObject * Sender, bool /*Forced*/)
{
  CHECK(FLocalEditors->Extract(Sender) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporaryDirectoryForRemoteFiles(
  UnicodeString RemoteDirectory, TCopyParamType CopyParam,
  UnicodeString & Result, UnicodeString & RootDirectory)
{
  RootDirectory = IncludeTrailingBackslash(WinConfiguration->TemporaryDir());
  Result = RootDirectory;

  if (WinConfiguration->TemporaryDirectoryAppendSession)
  {
    Result = IncludeTrailingBackslash(Result + CopyParam.ValidLocalPath(Terminal->SessionData->SessionName));
  }

  if (WinConfiguration->TemporaryDirectoryAppendPath)
  {
    if (!RemoteDirectory.IsEmpty() && (RemoteDirectory[1] == L'/'))
    {
      RemoteDirectory.Delete(1, 1);
    }
    Result = IncludeTrailingBackslash(Result + CopyParam.ValidLocalPath(FromUnixPath(RemoteDirectory)));
  }

  if (!ForceDirectories(Result))
  {
    throw EOSExtException(FMTLOAD(CREATE_TEMP_DIR_ERROR, (Result)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporarilyDownloadFiles(
  TStrings * FileList, bool ForceText, UnicodeString & RootTempDir, UnicodeString & TempDir,
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
  CopyParam.ReplaceInvalidChars = true;
  CopyParam.FileMask = L"";
  if (AllFiles)
  {
    CopyParam.IncludeFileMask = TFileMasks();
  }

  if (RootTempDir.IsEmpty())
  {
    TemporaryDirectoryForRemoteFiles(FTerminal->CurrentDirectory, CopyParam, TempDir, RootTempDir);
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
  TExecuteFileBy & ExecuteFileBy, const TEditorData *& ExternalEditor,
  const UnicodeString & FileName, bool Local, const TFileMasks::TParams & MaskParams)
{
  if (ExecuteFileBy == efDefaultEditor)
  {
    const TEditorPreferences * Editor =
      WinConfiguration->DefaultEditorForFile(FileName, Local, MaskParams);
    if ((Editor == NULL) || (Editor->Data->Editor == edInternal))
    {
      ExecuteFileBy = efInternalEditor;
      ExternalEditor = NULL;
    }
    else if (Editor->Data->Editor == edOpen)
    {
      ExecuteFileBy = efShell;
      ExternalEditor = NULL;
    }
    else
    {
      ExecuteFileBy = efExternalEditor;
      ExternalEditor = Editor->Data;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor,
  UnicodeString FullFileName, TObject * Object, const TFileMasks::TParams & MaskParams)
{

  UnicodeString OriginalFileName;
  UnicodeString LocalRootDirectory;
  UnicodeString RemoteDirectory;
  ExecuteFileNormalize(ExecuteFileBy, ExternalEditor, FullFileName,
    (Side == osLocal), MaskParams);

  UnicodeString Counter;
  UnicodeString LocalFileName;
  if (Side == osRemote)
  {
    OriginalFileName = UnixExtractFileName(FullFileName);
    RemoteDirectory = UnixExtractFilePath(FullFileName);
    TObject * Token = NULL;
    UnicodeString LocalDirectory;
    if (!FEditorManager->CanAddFile(RemoteDirectory, OriginalFileName,
           Terminal->SessionData->SessionName, Token, LocalRootDirectory,
           LocalDirectory))
    {
      if (Token != NULL)
      {
        TForm * Form = dynamic_cast<TForm *>(Token);
        if (Form->WindowState == wsMinimized)
        {
          ShowWindow(Form->Handle, SW_RESTORE);
        }
        else
        {
          Form->SetFocus();
        }
        Abort();
      }
      else
      {
        throw Exception(FMTLOAD(ALREADY_EDITED_EXTERNALLY_OR_UPLOADED, (OriginalFileName)));
      }
    }

    TStringList * FileList1 = new TStringList();
    try
    {
      FileList1->AddObject(FullFileName, Object);
      TemporarilyDownloadFiles(FileList1,
        RemoteExecuteForceText(ExecuteFileBy, ExternalEditor),
        LocalRootDirectory, LocalDirectory, true, true, true);
      LocalFileName = LocalDirectory + FileList1->Strings[0];
    }
    __finally
    {
      delete FileList1;
    }

    switch (ExecuteFileBy)
    {
      case efShell:
        Counter = "RemoteFilesExecuted";
        break;

      case efInternalEditor:
        Counter = "RemoteFilesOpenedInInternalEditor";
        break;

      case efExternalEditor:
        Counter = "RemoteFilesOpenedInExternalEditor";
        break;

      default:
        assert(false);
    }
  }
  else
  {
    LocalFileName = FullFileName;
    OriginalFileName = ExtractFileName(FullFileName);

    switch (ExecuteFileBy)
    {
      case efShell:
        Counter = "LocalFilesExecuted";
        break;

      case efInternalEditor:
        Counter = "LocalFilesOpenedInInternalEditor";
        break;

      case efExternalEditor:
        Counter = "LocalFilesOpenedInExternalEditor";
        break;

      default:
        assert(false);
    }
  }

  Configuration->Usage->Inc(Counter);

  CustomExecuteFile(Side, ExecuteFileBy, LocalFileName, OriginalFileName,
    ExternalEditor, LocalRootDirectory, RemoteDirectory);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor,
  bool AllSelected, bool OnFocused)
{
  assert(!WinConfiguration->DisableOpenEdit);
  assert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));

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
      UnicodeString ListFileName = FileList->Strings[i];
      UnicodeString FileNameOnly = (Side == osRemote) ?
        UnixExtractFileName(ListFileName) : ExtractFileName(ListFileName);
      TListItem * Item = DView->FindFileItem(FileNameOnly);
      if (!DView->ItemIsDirectory(Item))
      {
        UnicodeString FullFileName;
        if (Side == osRemote)
        {
          FullFileName = RemoteDirView->Path + ListFileName;
        }
        else
        {
          FullFileName = ListFileName;
        }

        TFileMasks::TParams MaskParams;
        MaskParams.Size = DView->ItemFileSize(Item);
        TDateTimePrecision Precision;
        MaskParams.Modification = DView->ItemFileTime(Item, Precision);

        ExecuteFile(Side, ExecuteFileBy, ExternalEditor, FullFileName,
          FileList->Objects[i], MaskParams);
      }
    }
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChanged(const UnicodeString FileName,
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
    // so i do not need to worry if masking algorithm works in all cases
    // ("" means "copy file name", no masking is actually done)
    if (ExtractFileName(FileName) == Data.OriginalFileName)
    {
      CopyParam.FileMask = L"";
    }
    else
    {
      CopyParam.FileMask = DelimitFileNameMask(Data.OriginalFileName);
    }
    CopyParam.ReplaceInvalidChars = true;
    CopyParam.IncludeFileMask = TFileMasks();

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
  const UnicodeString FileName, const TEditedFileData & Data)
{
  if ((Data.Terminal == NULL) || !Data.Terminal->Active)
  {
    throw Exception(FMTLOAD(EDIT_SESSION_CLOSED_RELOAD,
      (ExtractFileName(FileName), Data.SessionName)));
  }

  TRemoteFile * File = NULL;
  TStrings * FileList = new TStringList();
  try
  {
    UnicodeString RemoteFileName =
      UnixIncludeTrailingBackslash(Data.RemoteDirectory) + Data.OriginalFileName;
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

    UnicodeString RootTempDir = Data.LocalRootDirectory;
    UnicodeString TempDir = ExtractFilePath(FileName);

    TTerminal * PrevTerminal = TTerminalManager::Instance()->ActiveTerminal;
    TTerminalManager::Instance()->ActiveTerminal = Data.Terminal;
    try
    {
      TemporarilyDownloadFiles(FileList, Data.ForceText, RootTempDir,
        TempDir, true, true, true);
    }
    __finally
    {
      // it actually may not exist anymore...
      TTerminalManager::Instance()->ActiveTerminal = PrevTerminal;
    }

    // sanity check, the target file name should be still the same
    assert(ExtractFileName(FileName) == FileList->Strings[0]);
  }
  __finally
  {
    delete File;
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
        if ((Editor->Data->Editor == edExternal) &&
            (Editor->Data->ExternalEditor == Data.Command))
        {
          AnyFound = true;
          if (Editor->Data->SDIExternalEditor)
          {
            AnyNonMDI = true;
            if (Editor->Data->DetectMDIExternalEditor)
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
          // there is at least one instance of the editor
          Params.Params |= mpNeverAskAgainCheck;
        }
        unsigned int Answer = MessageDialog(FMTLOAD(EDITOR_EARLY_CLOSED2, (Data.OriginalFileName)), qtWarning,
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
          if ((Editor->Data->Editor == edExternal) &&
              (Editor->Data->ExternalEditor == Data.Command) &&
              ((EnableMDI && Editor->Data->SDIExternalEditor) ||
               (DisableDetect && Editor->Data->DetectMDIExternalEditor)))
          {
            Changed = true;
            TEditorPreferences * UpdatedEditor = new TEditorPreferences(*Editor);
            if (EnableMDI)
            {
              UpdatedEditor->GetData()->SDIExternalEditor = false;
            }
            if (DisableDetect)
            {
              UpdatedEditor->GetData()->DetectMDIExternalEditor = false;
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
  if (Visible)
  {
    UpdateControls();
  }
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
  UnicodeString & Target, UnicodeString & FileMask, bool & DirectCopy,
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

  if (Session == NULL)
  {
    Session = TTerminalManager::Instance()->ActiveTerminal;
  }
  Target = UnixIncludeTrailingBackslash(Target);
  FileMask = L"*.*";
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
bool __fastcall TCustomScpExplorerForm::RemoteTransferFiles(
  TStrings * FileList, bool NoConfirmation, bool Move, TTerminal * Session)
{
  bool DirectCopy;
  UnicodeString Target, FileMask;
  bool Result = RemoteTransferDialog(Session, Target, FileMask, DirectCopy, NoConfirmation, Move);
  if (Result)
  {
    if (!Move && !DirectCopy)
    {
      UnicodeString RootTempDir;
      UnicodeString TempDir;

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
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateDirectory(TOperationSide Side)
{
  Side = GetSide(Side);
  TRemoteProperties Properties = GUIConfiguration->NewDirectoryProperties;
  TRemoteProperties * AProperties = (Side == osRemote ? &Properties : NULL);
  UnicodeString Name = LoadStr(NEW_FOLDER);
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
bool __fastcall TCustomScpExplorerForm::OpenBookmark(UnicodeString Local, UnicodeString Remote)
{
  UnicodeString Path;
  if (FCurrentSide == osRemote)
  {
    Path = Remote;
  }
  else
  {
    Path = Local;
  }

  bool Result = !Path.IsEmpty();
  if (Result)
  {
    DirView(FCurrentSide)->Path = Path;
  }
  return Result;
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
void __fastcall TCustomScpExplorerForm::CalculateChecksum(const UnicodeString & Alg,
  TStrings * FileList, TCalculatedChecksumEvent OnCalculatedChecksum,
  bool & Close)
{
  // terminal can be already closed (e.g. dropped connection)
  if (Terminal != NULL)
  {
    Configuration->Usage->Inc(L"ChecksumCalculated");

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
bool __fastcall TCustomScpExplorerForm::SetProperties(TOperationSide Side, TStrings * FileList)
{
  bool Result;
  if (Side == osRemote)
  {
    TRemoteTokenList * GroupList = NULL;
    TRemoteTokenList * UserList = NULL;

    try
    {
      TRemoteProperties CurrentProperties;

      if (Terminal->LoadFilesProperties(FileList))
      {
        RemoteDirView->Invalidate();
      }

      bool CapableGroupChanging = Terminal->IsCapable[fcGroupChanging];
      bool CapableOwnerChanging = Terminal->IsCapable[fcOwnerChanging];
      if (CapableGroupChanging || CapableOwnerChanging)
      {
        if (CapableGroupChanging)
        {
          GroupList = Terminal->Groups->Duplicate();
        }
        if (CapableOwnerChanging)
        {
          UserList = Terminal->Users->Duplicate();
        }
        TRemoteDirectory * Files = Terminal->Files;
        int Count = Files->Count;
        if (Count > 100)
        {
          Count = 100;
        }
        for (int Index = 0; Index < Count; Index++)
        {
          TRemoteFile * File = Files->Files[Index];
          if (CapableGroupChanging)
          {
            GroupList->AddUnique(File->Group);
          }
          if (CapableOwnerChanging)
          {
            UserList->AddUnique(File->Owner);
          }
        }

        // if we haven't collected tokens for all files in current directory,
        // make sure we collect them at least for all selected files.
        // (note that so far the files in FileList has to be from current direcotry)
        if (Count < Files->Count)
        {
          for (int Index = 0; Index < FileList->Count; Index++)
          {
            TRemoteFile * File = (TRemoteFile *)(FileList->Objects[Index]);
            if (CapableGroupChanging)
            {
              GroupList->AddUnique(File->Group);
            }
            if (CapableOwnerChanging)
            {
              UserList->AddUnique(File->Owner);
            }
          }
        }
      }

      CurrentProperties = TRemoteProperties::CommonProperties(FileList);

      int Flags = 0;
      if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
      if (CapableOwnerChanging) Flags |= cpOwner;
      if (CapableGroupChanging) Flags |= cpGroup;

      TCalculateChecksumEvent CalculateChecksumEvent = NULL;
      if (Terminal->IsCapable[fcCalculatingChecksum])
      {
        CalculateChecksumEvent = CalculateChecksum;
      }

      TRemoteProperties NewProperties = CurrentProperties;
      Result =
        DoPropertiesDialog(FileList, RemoteDirView->PathName,
          GroupList, UserList, &NewProperties, Flags,
          Terminal->IsCapable[fcGroupOwnerChangingByID],
          CalculateSize, CalculateChecksumEvent);
      if (Result)
      {
        NewProperties = TRemoteProperties::ChangedProperties(CurrentProperties, NewProperties);
        Terminal->ChangeFilesProperties(FileList, &NewProperties);
      }
    }
    __finally
    {
      delete GroupList;
      delete UserList;
    }
  }
  else
  {
    DirView(Side)->DisplayPropertiesMenu();
    Result = true;
  }
  return Result;
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

    if (IsCustomShortCut(KeyShortCut))
    {
      const TCustomCommandType * Command = WinConfiguration->CustomCommandList->Find(KeyShortCut);
      if (Command != NULL)
      {
        if (CustomCommandState(*Command, false) > 0)
        {
          ExecuteFileOperationCommand(foCustomCommand, osRemote,
            false, false, const_cast<TCustomCommandType *>(Command));
        }
        Key = 0;
      }

      if (WinConfiguration->SharedBookmarks != NULL)
      {
        TBookmark * Bookmark = WinConfiguration->SharedBookmarks->FindByShortCut(KeyShortCut);
        if ((Bookmark != NULL) &&
            OpenBookmark(Bookmark->Local, Bookmark->Remote))
        {
          Key = 0;
        }
      }
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
    // escape hotkeys particularly because of the custom commands names
    SessionStatusBar->SimpleText = EscapeHotkey(FStatusBarHint);
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
  Panel->Caption = L"";
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Idle(bool AppIdle)
{
  if (FShowing)
  {
    FEditorManager->Check();

    // make sure that Idle is called before update queue, as it may invoke QueueEvent
    // that needs to know if queue view is visible (and it may be closed after queue update)
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

      if (WinConfiguration->RefreshRemotePanel)
      {
        TManagedTerminal * ManagedTerminal =
          dynamic_cast<TManagedTerminal *>(Terminal);
        if ((ManagedTerminal != NULL) && Terminal->Active &&
            (Now() - ManagedTerminal->DirectoryLoaded >
               WinConfiguration->RefreshRemotePanelInterval))
        {
          RemoteDirView->ReloadDirectory();
        }
      }
    }

    if (FQueueStatusInvalidated)
    {
      UpdateQueueStatus(AppIdle, false);
    }

    RefreshQueueItems(AppIdle);

    UpdateStatusBar();
  }

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
  UnicodeString AHint = GetLongHint(Application->Hint);
  FShowStatusBarHint = Active && !AHint.IsEmpty();
  if (FShowStatusBarHint)
  {
    FStatusBarHint = AHint != L"E" ? AHint : UnicodeString(L"");
  }
  else
  {
    FStatusBarHint = L"";
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
    ShowWindow(Handle, SW_HIDE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationRestore(TObject * /*Sender*/)
{
  if (FTrayIcon->Visible)
  {
    FTrayIcon->Visible = false;
    ShowWindow(Handle, SW_SHOW);
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
  // workaround
  // TApplication.WndProc sets TApplication.FAppIconic to false,
  // on WM_ACTIVATEAPP, what renders TApplication.Restore no-op function.
  // But WM_ACTIVATEAPP message can be received even when
  // the main window is minimized to the tray and internal editor window is focused
  // (after another application was previously active)
  if (::IsIconic(Handle))
  {
    bool * AppIconic = reinterpret_cast<bool *>((reinterpret_cast<char *>(Application)) + 56);
    if (!*AppIconic)
    {
      *AppIconic = true;
    }
  }
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
    if (!ExecuteShell(Application->ExeName, L""))
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
  TSessionData * SessionData = new TSessionData(L"");
  try
  {
    SessionData->Assign(Terminal->SessionData);
    // current working directories become defaults here, what is not right
    UpdateSessionData(SessionData);

    if (OpenInNewWindow())
    {
      UnicodeString SessionName = StoredSessions->HiddenPrefix + Terminal->SessionData->Name;
      StoredSessions->NewSession(SessionName, SessionData);
      // modified only, explicit
      StoredSessions->Save(false, true);
      if (!ExecuteShell(Application->ExeName, FORMAT(L"\"%s\"", (SessionName))))
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
    // encode session name because of slashes in hierarchical sessions
    if (!ExecuteShell(Application->ExeName, FORMAT(L"\"%s\"", (EncodeUrlChars(Data->Name)))))
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
    unsigned int Result;
    TMessageParams Params(mpNeverAskAgainCheck);
    UnicodeString Message;
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
    CanClose =
      FEditorManager->CloseInternalEditors(CloseInternalEditor) &&
      FEditorManager->CloseExternalFilesWithoutProcess();

    if (CanClose)
    {
      while (CanClose && (FLocalEditors->Count > 0))
      {
        int PrevCount = FLocalEditors->Count;
        static_cast<TForm *>(FLocalEditors->Items[0])->Close();
        CanClose = (FLocalEditors->Count < PrevCount);
      }

      if (CanClose)
      {
        CanClose =
          FEditorManager->Empty(true) ||
          (MessageDialog(LoadStr(PENDING_EDITORS), qtWarning, qaIgnore | qaCancel,
            HELP_NONE) == qaIgnore);
      }
    }
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
void __fastcall TCustomScpExplorerForm::ForceCloseLocalEditors()
{

  while (FLocalEditors->Count > 0)
  {
    delete static_cast<TForm *>(FLocalEditors->Items[0]);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewDisplayProperties(
      TObject *Sender)
{
  APPLICATION_EXCEPTION_HACK_BEGIN
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
  APPLICATION_EXCEPTION_HACK_END;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ComponentShowing(Byte Component, bool value)
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
void __fastcall TCustomScpExplorerForm::SetComponentVisible(Byte Component, Boolean value)
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
bool __fastcall TCustomScpExplorerForm::GetComponentVisible(Byte Component)
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
    case fcSessionsTabs: return SessionsPageControl;
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
    NonVisualDataModule->RemoteFormatSizeBytesPopupItem->Visible =
      (Column->Index == uvSize);
  }
  else
  {
    DirViewColumnMenu = NonVisualDataModule->LocalDirViewColumnPopup;
    NonVisualDataModule->LocalSortByExtColumnPopupItem->Visible =
      (Column->Index == dvName);
    NonVisualDataModule->LocalFormatSizeBytesPopupItem->Visible =
      (Column->Index == dvSize);
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
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory, bool UseDefaults)
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
      else
      {
        if (FLAGSET(GUIConfiguration->SynchronizeOptions, soSynchronizeAsk) &&
            FLAGCLEAR(Params.Options, soSynchronizeAsk) &&
            FLAGSET(Params.Options, soSynchronize))
        {
          GUIConfiguration->SynchronizeOptions =
            (GUIConfiguration->SynchronizeOptions & ~soSynchronizeAsk) |
            soSynchronize;
        }
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
  TSynchronizeController * /*Sender*/, const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, const TCopyParamType & CopyParam,
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
      else
      {
        // if keepuptodate is non-recursive,
        // full sync before has to be non-recursive as well
        if (FLAGCLEAR(Params.Options, soRecurse))
        {
          PParams |= TTerminal::spNoRecurse;
        }
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
  TSynchronizeController * /*Sender*/, const UnicodeString Directory,
  const UnicodeString ErrorStr)
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
    unsigned int Result = MessageDialog(
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
void __fastcall TCustomScpExplorerForm::Synchronize(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, TSynchronizeMode Mode,
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
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
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
    Configuration->Usage->Inc(L"Synchronizations");
    UpdateCopyParamCounters(CopyParam);

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
                 LocalDirectory, RemoteDirectory, CustomCommandMenu))
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
  const UnicodeString LocalDirectory, const UnicodeString RemoteDirectory,
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
  TSessionData * SessionData = new TSessionData(L"");
  try
  {
    SessionData->Assign(Terminal->SessionData);
    UpdateSessionData(SessionData);

    TSessionData * EditingSessionData = StoredSessions->FindSame(SessionData);

    bool SavePassword;
    bool * PSavePassword;
    bool NotRecommendedSavingPassword =
      !CustomWinConfiguration->UseMasterPassword &&
      !SameText(SessionData->UserName, AnonymousUserName);

    if (Configuration->DisablePasswordStoring ||
        !SessionData->HasAnyPassword())
    {
      PSavePassword = NULL;
    }
    else
    {
      PSavePassword = &SavePassword;
      SavePassword =
        ((EditingSessionData != NULL) &&
         (EditingSessionData->Password == SessionData->Password)) ||
        !NotRecommendedSavingPassword;
    }

    UnicodeString SessionName = Terminal->SessionData->SessionName;
    if (DoSaveSessionDialog(SessionName, PSavePassword, EditingSessionData, NotRecommendedSavingPassword))
    {
      if ((PSavePassword != NULL) && !*PSavePassword)
      {
        SessionData->Password = L"";
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
  // also only peek, we may not be connected at all atm,
  // so make sure we do not try retrieving current directory from the server
  // (particularly with FTP)
  UnicodeString ACurrentDirectory = Terminal->PeekCurrentDirectory();
  if (!ACurrentDirectory.IsEmpty())
  {
    ManagedTerminal->RemoteDirectory = ACurrentDirectory;
  }
  ManagedTerminal->Color = SessionColor;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  assert(Data != NULL);

  // cannot use RemoteDirView->Path, because it is empty if connection
  // was already closed
  // also only peek, we may not be connected at all atm
  // (well this can hardly be true here, as opposite to UpdateTerminal above),
  // so make sure we do not try retrieving current directory from the server
  // (particularly with FTP)
  UnicodeString ACurrentDirectory = Terminal->PeekCurrentDirectory();
  if (!ACurrentDirectory.IsEmpty())
  {
    Data->RemoteDirectory = ACurrentDirectory;
  }
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
  const UnicodeString Path, __int64 RequiredSpace, bool & Continue)
{
  if (WinConfiguration->DDWarnLackOfTempSpace)
  {
    UnicodeString ADrive = ExtractFileDrive(ExpandFileName(Path));
    if (!ADrive.IsEmpty())
    {
      __int64 FreeSpace = DiskFree((Byte)(ADrive[1]-'A'+1));
      assert(RequiredSpace >= 0);
      __int64 RequiredWithReserve;
      RequiredWithReserve = (__int64)(RequiredSpace * WinConfiguration->DDWarnLackOfTempSpaceRatio);
      if (FreeSpace < RequiredWithReserve)
      {
        unsigned int Result;
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
  bool Continue = true;
  if (Mode == odAddBookmark)
  {
    TMessageParams Params(mpNeverAskAgainCheck);
    Params.NewerAskAgainTitle = LoadStr(ADD_BOOKMARK_SHARED);
    Params.NewerAskAgainCheckedInitially = WinConfiguration->UseSharedBookmarks;

    unsigned int Answer =
      MessageDialog(FMTLOAD(ADD_BOOKMARK_CONFIRM, (DirView(Side)->PathName)),
        qtConfirmation, qaYes | qaNo, HELP_ADD_BOOKMARK_CONFIRM, &Params);
    if (Answer == qaNeverAskAgain)
    {
      Continue = true;
      WinConfiguration->UseSharedBookmarks = true;
    }
    else if (Answer == qaYes)
    {
      Continue = true;
      WinConfiguration->UseSharedBookmarks = false;
    }
    else
    {
      Continue = false;
    }
  }

  if (Continue)
  {
    TStrings * VisitedDirectories = CreateVisitedDirectories(Side);
    try
    {
      UnicodeString Name = DirView(Side)->PathName;
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
      unsigned int Answer = MessageDialog(FMTLOAD(PERFORM_ON_COMMAND_SESSION,
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
void __fastcall TCustomScpExplorerForm::OpenConsole(UnicodeString Command)
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
void __fastcall TCustomScpExplorerForm::SessionsDDDragEnter(
  _di_IDataObject DataObj, int KeyState,
  const TPoint & Point, int & Effect, bool & Accept)
{
  FileControlDDDragEnter(SessionsPageControl, DataObj, KeyState, Point, Effect, Accept);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileControlDDDragLeave(
  TObject * Sender)
{
  USEDPARAM(Sender);
  assert(FDDTargetControl == Sender);
  FDDTargetControl = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDDragLeave()
{
  FileControlDDDragLeave(SessionsPageControl);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddEditLink(bool Add)
{
  assert(FCurrentSide == osRemote);

  bool Edit = false;
  TRemoteFile * File = NULL;
  UnicodeString FileName;
  UnicodeString PointTo;
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
    Configuration->Usage->Inc(L"RemoteLinksCreated");

    if (Edit)
    {
      assert(File->FileName == FileName);
      int Params = dfNoRecursive;
      Terminal->ExceptionOnFail = true;
      try
      {
        Terminal->DeleteFile(L"", File, &Params);
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
void __fastcall TCustomScpExplorerForm::ExecuteCurrentFileWith()
{
  TEditorData ExternalEditor;
  ExternalEditor.Editor = edExternal;
  bool Remember = false;

  if (DoEditorPreferencesDialog(&ExternalEditor, Remember, epmAdHoc,
        (GetSide(osCurrent) == osRemote)))
  {
    if (Remember)
    {
      TEditorList * EditorList = new TEditorList;
      try
      {
        *EditorList = *WinConfiguration->EditorList;

        bool Found = false;
        int i = 0;
        while (!Found && (i < EditorList->Count))
        {
          const TEditorPreferences * Editor = EditorList->Editors[i];
          if ((Editor->Data->Editor == edExternal) &&
              (Editor->Data->ExternalEditor == ExternalEditor.ExternalEditor))
          {
            Found = true;
          }
          i++;
        }

        if (!Found)
        {
          EditorList->Add(new TEditorPreferences(ExternalEditor));
          WinConfiguration->EditorList = EditorList;
        }
      }
      __finally
      {
        delete EditorList;
      }
    }

    ExecuteFile(osCurrent, efExternalEditor, &ExternalEditor, true, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalRemoved(TObject * Sender)
{
  FEditorManager->ProcessFiles(FileTerminalRemoved, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileTerminalRemoved(const UnicodeString FileName,
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
  TStrings * TerminalList = TTerminalManager::Instance()->TerminalList;
  int ActiveTerminalIndex = TTerminalManager::Instance()->ActiveTerminalIndex;

  Configuration->Usage->SetMax(L"MaxOpenedSessions", TerminalList->Count);

  bool ListChanged = (TerminalList->Count + 1 != SessionsPageControl->PageCount);
  if (!ListChanged)
  {
    int Index = 0;
    while (!ListChanged && (Index < TerminalList->Count))
    {
      ListChanged =
        (SessionsPageControl->Pages[Index]->Tag != reinterpret_cast<int>(TerminalList->Objects[Index])) ||
        (SessionsPageControl->Pages[Index]->Caption != TerminalList->Strings[Index]);
      Index++;
    }
  }

  if (ListChanged)
  {
    SendMessage(SessionsPageControl->Handle, WM_SETREDRAW, 0, 0);
    try
    {
      FSessionColors->Clear();

      while (SessionsPageControl->PageCount > 0)
      {
        delete SessionsPageControl->Pages[0];
      }

      for (int Index = 0; Index < TerminalList->Count; Index++)
      {
        TTabSheet * TabSheet = new TTabSheet(SessionsPageControl);
        TabSheet->Caption = TerminalList->Strings[Index];
        TTerminal * Terminal = dynamic_cast<TTerminal *>(TerminalList->Objects[Index]);
        TabSheet->Tag = reinterpret_cast<int>(Terminal);
        TabSheet->PageControl = SessionsPageControl;

        TManagedTerminal * ManagedTerminal = dynamic_cast<TManagedTerminal *>(Terminal);
        TColor Color =
          (ManagedTerminal == NULL) ? (TColor)0 :
            ((ManagedTerminal == FTerminal) ? FSessionColor : ManagedTerminal->Color);
        TabSheet->ImageIndex = AddSessionColor(Color);
      }

      TTabSheet * TabSheet = new TTabSheet(SessionsPageControl);
      TabSheet->Caption = L"   +";
      TabSheet->PageControl = SessionsPageControl;
      TabSheet->ImageIndex = -1;
    }
    __finally
    {
      SendMessage(SessionsPageControl->Handle, WM_SETREDRAW, 1, 0);
    }
  }

  SessionsPageControl->ActivePageIndex = ActiveTerminalIndex;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlChange(TObject * /*Sender*/)
{
  assert(SessionsPageControl->ActivePage != NULL);
  TTerminal * Terminal = reinterpret_cast<TTerminal *>(SessionsPageControl->ActivePage->Tag);
  if (Terminal != NULL)
  {
    TTerminalManager::Instance()->ActiveTerminal = Terminal;
  }
  else
  {
    try
    {
      NewSession();
    }
    __finally
    {
      TerminalListChanged(NULL);
    }

    FSessionsPageControlNewSessionTime = Now();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferListChange(TObject * Sender)
{
  TTBXStringList * TransferList = dynamic_cast<TTBXStringList *>(Sender);
  assert(TransferList != NULL);
  UnicodeString Name;
  if (TransferList->ItemIndex <= 0)
  {
    Name = L"";
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

      UnicodeString Name;
      if (FTransferListHoverIndex == 0)
      {
        Name = L"";
      }
      else
      {
        Name = GUIConfiguration->CopyParamList->Names[FTransferListHoverIndex - 1];
      }

      TTBXLabelItem * TransferLabel = dynamic_cast<TTBXLabelItem*>(
        static_cast<TComponent*>(GetComponent(fcTransferLabel)));
      TTBXStringList * TransferList = dynamic_cast<TTBXStringList*>(
        static_cast<TObject*>(GetComponent(fcTransferList)));

      UnicodeString InfoStr =
        GUIConfiguration->CopyParamPreset[Name].
          GetInfoStr(L"; ",
            FLAGMASK(Terminal != NULL, Terminal->UsableCopyParamAttrs(0).General));
      int MaxWidth = TransferList->MinWidth - (2 * TransferLabel->Margin) - 10;
      if (Canvas->TextExtent(InfoStr).cx > MaxWidth)
      {
        UnicodeString Ellipsis = L"...";
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

  FSessionsDragDropFilesEx->DragDropControl = SessionsPageControl;

  FShowing = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PopupTrayBalloon(TTerminal * Terminal,
  const UnicodeString & Str, TQueryType Type, Exception * E, unsigned int Seconds)
{
  bool Do;
  UnicodeString Message;
  if (E == NULL)
  {
    Message = Str;
    Do = true;
  }
  else
  {
    Do = ExceptionMessage(E, Message);
  }

  if (Do && WinConfiguration->BalloonNotifications)
  {
    const ResourceString * Captions[] = { &_SMsgDlgConfirm, &_SMsgDlgWarning,
      &_SMsgDlgError, &_SMsgDlgInformation, NULL };
    UnicodeString Title = LoadResourceString(Captions[Type]);
    if (Terminal != NULL)
    {
      Title = FORMAT(L"%s - %s",
        (TTerminalManager::Instance()->TerminalTitle(Terminal), Title));
    }

    if (Seconds == 0)
    {
      Seconds = WinConfiguration->NotificationsTimeout;
    }
    FTrayIcon->PopupBalloon(Title, Message, Type, Seconds * MSecsPerSec);
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TCustomScpExplorerForm::MoreMessageDialog(const UnicodeString Message,
    TStrings * MoreMessages, TQueryType Type, unsigned int Answers,
    UnicodeString HelpKeyword, const TMessageParams * Params,
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
    unsigned int Result;
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
    PopupTrayBalloon(Terminal, L"", qtError, E);
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
  Notify(Terminal, L"", qtError, false, NULL, E);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Notify(TTerminal * Terminal,
  UnicodeString Message, TQueryType Type,
  bool Important, TNotifyEvent OnClick, Exception * E)
{
  if ((E == NULL) ||
      ExceptionMessage(E, Message))
  {
    unsigned int Seconds = WinConfiguration->NotificationsTimeout;
    if (Important)
    {
      Seconds *= 5;
    }

    UnicodeString NoteMessage(Message);
    if (Terminal != NULL)
    {
      NoteMessage = FORMAT(L"%s: %s",
        (TTerminalManager::Instance()->TerminalTitle(Terminal), NoteMessage));
    }

    if (WinConfiguration->BalloonNotifications &&
        ::TTrayIcon::SupportsBalloons())
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
  UnicodeString Message;
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

  FDDExtMapFile = CreateFileMappingA((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TDragExtCommStruct), AnsiString(DRAG_EXT_MAPPING).c_str());

  {
    TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
    TDragExtCommStruct* CommStruct;
    CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
      FILE_MAP_ALL_ACCESS, 0, 0, 0));
    assert(CommStruct != NULL);
    CommStruct->Version = TDragExtCommStruct::CurrentVersion;
    CommStruct->Dragging = true;
    wcsncpy(CommStruct->DropDest, FDragExtFakeDirectory.c_str(),
      LENOF(CommStruct->DropDest));
    CommStruct->DropDest[LENOF(CommStruct->DropDest) - 1] = L'\0';
    UnmapViewOfFile(CommStruct);
  }

  FDDMoveSlipped = false;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteFileControlFileOperation(
  TObject * Sender, TFileOperation Operation, bool NoConfirmation, void * Param)
{
  bool Result;
  if (Sender == RemoteDirView)
  {
    Result = ExecuteFileOperation(Operation, osRemote, true, NoConfirmation, Param);
  }
  else
  {
    assert(Sender == RemoteDriveView);
    TStrings * FileList = RemoteDriveView->DragFileList();
    try
    {
      Result = ExecuteFileOperation(Operation, osRemote, FileList, NoConfirmation, Param);
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
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDEnd(TObject * Sender)
{
  APPLICATION_EXCEPTION_HACK_BEGIN
  {
    SAFE_DESTROY(FDDFileList);

    if (FDDExtMapFile != NULL)
    {
      try
      {
        TDragResult DDResult = (Sender == RemoteDirView) ?
          RemoteDirView->LastDDResult : RemoteDriveView->LastDDResult;

        // note that we seem to never get drMove here, see also comment below
        if ((DDResult == drCopy) || (DDResult == drMove) || (DDResult == drInvalid))
        {
          UnicodeString TargetDirectory;
          TFileOperation Operation;

          // drInvalid may mean drMove, see comment below
          Operation = (DDResult == drCopy) ? foCopy : foMove;

          if (FDDMoveSlipped)
          {
            Operation = foMove;
          }

          TTransferOperationParam Param;
          bool Internal;
          if (!DDGetTarget(Param.TargetDirectory, Internal))
          {
            // we get drInvalid both if move-d&d was intercepted by ddext,
            // and when users drops on no-drop location.
            // we tell the difference by existence of response from ddext,
            // so we ignore absence of response in this case
            if (DDResult != drInvalid)
            {
              throw Exception(LoadStr(DRAGEXT_TARGET_UNKNOWN));
            }
          }
          else
          {
            // download using ddext
            Param.Temp = false;
            Param.DragDrop = true;

            if (RemoteFileControlFileOperation(Sender, Operation,
                  !WinConfiguration->DDTransferConfirmation, &Param))
            {
              Configuration->Usage->Inc(
                Internal ? L"DownloadsDragDropInternal" : L"DownloadsDragDropExternalExt");
            }
          }
        }
      }
      __finally
      {
        CloseHandle(FDDExtMapFile);
        FDDExtMapFile = NULL;
        RemoveDir(FDragExtFakeDirectory);
        FDragExtFakeDirectory = L"";
      }
    }

    if (!FDragDropSshTerminate.IsEmpty())
    {
      throw ESshTerminate(NULL, FDragDropSshTerminate, FDragDropOnceDoneOperation);
    }
  }
  APPLICATION_EXCEPTION_HACK_END;
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
bool __fastcall TCustomScpExplorerForm::DDGetTarget(UnicodeString & Directory, bool & Internal)
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
          Internal = false;
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

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddDelayedDirectoryDeletion(
  const UnicodeString TempDir, int SecDelay)
{
  TDateTime Alarm = Now() + (double)((double)SecDelay*OneMillisecond);
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
  UnicodeString Directory;

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
  if (IsFileControl(FDDTargetControl, osRemote) ||
      (FDDTargetControl == SessionsPageControl))
  {
    TTerminal * TargetTerminal = NULL;
    TFileOperation Operation = foNone;
    if (FDDTargetControl == SessionsPageControl)
    {
      TPoint Point = SessionsPageControl->ScreenToClient(Mouse->CursorPos);
      int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
      // do not allow dropping on the "+" tab
      TargetTerminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
      if (TargetTerminal != NULL)
      {
        if ((FLastDropEffect == DROPEFFECT_MOVE) &&
            (TargetTerminal == TTerminalManager::Instance()->ActiveTerminal))
        {
          Operation = foRemoteMove;
        }
        else
        {
          Operation = foRemoteCopy;
        }
      }
    }
    else
    {
      // when move from remote side is disabled, we allow copying inside the remote
      // panel, but we interpret is as moving (we also slip in the move cursor)
      if ((FLastDropEffect == DROPEFFECT_MOVE) ||
          (!WinConfiguration->DDAllowMoveInit && (FLastDropEffect == DROPEFFECT_COPY) &&
           FDDMoveSlipped))
      {
        Operation = foRemoteMove;
      }
      else if (FLastDropEffect == DROPEFFECT_COPY)
      {
        Operation = foRemoteCopy;
      }
    }

    if (Operation != foNone)
    {
      RemoteFileControlFileOperation(DropSourceControl,
        Operation, !WinConfiguration->DDTransferConfirmation, TargetTerminal);
    }
    // abort drag&drop
    Abort();
  }
  else if ((FDDExtMapFile == NULL) && (FLastDropEffect != DROPEFFECT_NONE))
  {
    assert(!FDragTempDir.IsEmpty());
    TTransferType Type;
    UnicodeString TempDir = FDragTempDir;
    // We clear FDragTempDir before calling
    // just in case it fail (raises exception)
    FDragTempDir = L"";
    Type = (FLastDropEffect & DROPEFFECT_MOVE ? ttMove : Type = ttCopy);

    TGUICopyParamType CopyParams = GUIConfiguration->CurrentCopyParam;
    // empty directory parameter means temp directory -> don't display it!
    UnicodeString TargetDir = L"";

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
          Configuration->Usage->Inc(L"DownloadsDragDropExternalTemp");
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
  const UnicodeString TargetDir, const TCopyParamType * CopyParam, int Params)
{
  UpdateCopyParamCounters(*CopyParam);
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
          UnicodeString FileName;
          for (int Index = 0; Index < ExportData->Count; Index++)
          {
            if (ExportData->Strings[Index].Pos(L" ") > 0)
            {
              ExportData->Strings[Index] = FORMAT(L"\"%s\"", (ExportData->Strings[Index]));
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
void __fastcall TCustomScpExplorerForm::Filter(TOperationSide Side)
{
  TCustomDirView * DirView = this->DirView(Side);
  TFileFilter Filter;
  DefaultFileFilter(Filter);
  Filter.Masks = DirView->Mask;
  if (DoFilterMaskDialog(DirView, &Filter))
  {
    DirView->Mask = TFileMasks::NormalizeMask(Filter.Masks);
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

    case qoOnceEmpty:
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
bool __fastcall TCustomScpExplorerForm::GetQueueEnabled()
{
  return (Queue != NULL) && Queue->Enabled;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToggleQueueEnabled()
{
  assert(Queue != NULL);
  if (Queue != NULL)
  {
    Queue->Enabled = !Queue->Enabled;
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
  if (Sender == SessionsPageControl)
  {
    Result = FSessionsDragDropFilesEx;
  }
  else
  {
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
  }
  assert(Result != NULL);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDragDropFileOperation(
  TObject * Sender, int Effect, UnicodeString TargetPath)
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
    if (ExecuteFileOperation(Operation, osLocal, FileList,
          !WinConfiguration->DDTransferConfirmation, &Param))
    {
      if (IsFileControl(DropSourceControl, osLocal))
      {
        Configuration->Usage->Inc(L"UploadsDragDropInternal");
      }
      else
      {
        Configuration->Usage->Inc(L"UploadsDragDropExternal");
      }
    }
  }
  __finally
  {
    FDragDropOperation = false;
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDFileOperation(
  TObject * Sender, int Effect, UnicodeString /*SourcePath*/,
  UnicodeString TargetPath, bool & /*DoOperation*/)
{
  APPLICATION_EXCEPTION_HACK_BEGIN
  {
    RemoteFileControlDragDropFileOperation(Sender, Effect, TargetPath);
  }
  APPLICATION_EXCEPTION_HACK_END;
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
        // currently we support copying always (at least via temporary directory);
        // remove associated checks once this all proves stable andworking
        bool CopyCapable = true;
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
  TObject * Sender, TRemoteFile * File, UnicodeString & FileName)
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

  UnicodeString TransferFileName;
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
  // sometimes we do not get DDEnd so the list is not released
  SAFE_DESTROY(FDDFileList);
  FDDFileList = new TStringList();
  FDragTempDir = WinConfiguration->TemporaryDir();
  FDDTotalSize = 0;
  FDragDropSshTerminate = L"";
  FDragDropOnceDoneOperation = odoIdle;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDQueryContinueDrag(
  TObject * /*Sender*/, BOOL /*FEscapePressed*/, int /*grfKeyState*/,
  HRESULT & Result)
{
  APPLICATION_EXCEPTION_HACK_BEGIN
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
          FDragDropOnceDoneOperation = E.Operation;
        }
      }
      catch (Exception &E)
      {
        // If downloading fails we need to cancel drag&drop, otherwise
        // Explorer shows error
        // But by the way exception probably never reach this point as
        // it's catched on way
        Result = DRAGDROP_S_CANCEL;
        assert(Terminal != NULL);
        Terminal->ShowExtendedException(&E);
      }
    }
  }
  APPLICATION_EXCEPTION_HACK_END
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewMatchMask(
  TObject * /*Sender*/, UnicodeString FileName, bool Directory, __int64 Size,
  TDateTime Modification, UnicodeString Masks, bool & Matches)
{
  TFileMasks::TParams MaskParams;
  MaskParams.Size = Size;
  MaskParams.Modification = Modification;
  // this does not re-parse the mask if it is the same as the last time
  FDirViewMatchMask = Masks;
  Matches = FDirViewMatchMask.Matches(FileName, Directory, UnicodeString(L""), &MaskParams);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewGetOverlay(
  TObject * Sender, TListItem * Item, WORD & Indexes)
{
  TCustomDirView * DirView = reinterpret_cast<TCustomDirView *>(Sender);
  UnicodeString Ext;
  if (DirView == RemoteDirView)
  {
    Ext = UnixExtractFileExt(DirView->ItemFileName(Item));
  }
  else
  {
    Ext = ExtractFileExt(DirView->ItemFileName(Item));
  }

  if (AnsiSameText(Ext, Configuration->PartialExt))
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
    UnicodeString Path;
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
UnicodeString __fastcall TCustomScpExplorerForm::FileStatusBarText(
  const TStatusFileInfo & FileInfo)
{
  UnicodeString Result =
    FMTLOAD(FILE_INFO_FORMAT,
      (FormatBytes(FileInfo.SelectedSize),
       FormatBytes(FileInfo.FilesSize),
       FormatFloat(L"#,##0", FileInfo.SelectedCount),
       FormatFloat(L"#,##0", FileInfo.FilesCount)));

  if ((FileInfo.HiddenCount > 0) || (FileInfo.FilteredCount > 0))
  {
    UnicodeString Ex;
    if (FileInfo.HiddenCount > 0)
    {
      Ex = FMTLOAD(FILE_INFO_HIDDEN, (FileInfo.HiddenCount));
    }
    if (FileInfo.FilteredCount > 0)
    {
      if (!Ex.IsEmpty())
      {
        Ex += L" + ";
      }
      Ex += FMTLOAD(FILE_INFO_FILTERED, (FileInfo.FilteredCount));
    }
    Result = FMTLOAD(FILE_INFO_FORMAT_EX, (Result, Ex));
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateFileStatusBar(
  TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo, int Panel)
{
  UnicodeString Text = FileStatusBarText(FileInfo);
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
UnicodeString __fastcall TCustomScpExplorerForm::PathForCaption()
{
  UnicodeString Result;
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
  TTerminalManager::Instance()->UpdateAppTitle();
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
  UnicodeString Message;
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
  Data.HostName = Terminal->SessionData->HostNameExpanded;
  Data.UserName = Terminal->SessionData->UserNameExpanded;
  Data.RemoteDirectory = RemoteDirView->PathName;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetAutoSelect()
{
  if (FAllowTransferPresetAutoSelect)
  {
    assert(Terminal != NULL);
    TCopyParamRuleData Data;
    GetTransferPresetAutoSelectData(Data);

    int CopyParamIndex = GUIConfiguration->CopyParamList->Find(Data);
    UnicodeString CopyParamCurrent = GUIConfiguration->CopyParamCurrent;

    if (CopyParamIndex < 0)
    {
      // there is no preset that matches autoselection
      // set preset that we consider "default"
      FCopyParamAutoSelected = L""; // forget last autoselected preset
      GUIConfiguration->CopyParamCurrent = FCopyParamDefault;
    }
    else
    {
      // there is preset matching autoselection
      UnicodeString CopyParamName = GUIConfiguration->CopyParamList->Names[CopyParamIndex];
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
      if (CopyParamIndex >= 0)
      {
        Configuration->Usage->Inc(L"CopyParamAutoSelects");
      }

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
        UnicodeString Info = GUIConfiguration->CurrentCopyParam.GetInfoStr(L"\n",
          CopyParamAttrs);
        if (CopyParamIndex >= 0)
        {
          assert(GUIConfiguration->CopyParamList->Rules[CopyParamIndex] != NULL);
          Info = FORMAT(L"%s\n \n%s", (Info,
            FMTLOAD(COPY_PARAM_RULE,
              (GUIConfiguration->CopyParamList->Rules[CopyParamIndex]->GetInfoStr(L"\n")))));
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
  Aliases[0].Button = qaIgnore; // "ignore" is after "ok"
  Aliases[0].Alias = LoadStr(CONFIGURE_BUTTON);

  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);

  unsigned int Result =
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
  const TCustomCommandType & Command)
{
  if (CustomCommandState(Command, FEditingFocusedAdHocCommand) <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE, (Command.Command)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AdHocCustomCommand(bool OnFocused)
{
  bool RemoteAllowed = CustomCommandRemoteAllowed();
  TCustomCommandType Command;
  // make sure we use local custom command when remote are not supported
  if (RemoteAllowed || FLAGSET(FLastCustomCommand.Params, ccLocal))
  {
    Command = FLastCustomCommand;
  }
  else
  {
    Command.Params = Command.Params | ccLocal;
  }
  Command.Name = LoadStr(CUSTOM_COMMAND_AD_HOC_NAME);
  FEditingFocusedAdHocCommand = OnFocused;
  int Options = FLAGMASK(!RemoteAllowed, ccoDisableRemote);
  if (DoCustomCommandDialog(Command, WinConfiguration->CustomCommandList,
       ccmAdHoc, Options, AdHocCustomCommandValidate, NULL))
  {
    FLastCustomCommand = Command;
    UpdateCustomCommandsToolbar();
    ExecuteFileOperation(foCustomCommand, osRemote, OnFocused, false, &Command);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LastCustomCommand(bool OnFocused)
{
  assert(!FLastCustomCommand.Command.IsEmpty());

  int State = CustomCommandState(FLastCustomCommand, OnFocused);
  assert(State > 0);
  if (State <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE, (FLastCustomCommand.Command)));
  }

  ExecuteFileOperation(foCustomCommand, osRemote, OnFocused, false, &FLastCustomCommand);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetLastCustomCommand(bool OnFocused,
  TCustomCommandType & Command, int & State)
{
  bool Result = !FLastCustomCommand.Command.IsEmpty();

  if (Result)
  {
    Command = FLastCustomCommand;

    State = CustomCommandState(FLastCustomCommand, OnFocused);
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
void __fastcall TCustomScpExplorerForm::PostComponentHide(Byte Component)
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

    case WM_COMPONENT_HIDE:
      {
        Byte Component = static_cast<Byte>(M->WParam);
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
    MaxWidth = 32000;
    MaxHeight = 32000;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetSpaceAvailable(const UnicodeString Path,
  TSpaceAvailable & ASpaceAvailable, bool & Close)
{
  // terminal can be already closed (e.g. dropped connection)
  if ((Terminal != NULL) && Terminal->IsCapable[fcCheckingSpaceAvailable])
  {
    Configuration->Usage->Inc(L"SpaceAvailableChecks");

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

    NonVisualDataModule->SessionColorPalette->Color = C;
    NonVisualDataModule->ColorMenuItem->Color = C;

    SessionsPageControl->ActivePage->ImageIndex = AddSessionColor(value);

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
bool __fastcall TCustomScpExplorerForm::CancelNote()
{
  bool Result = FNoteTimer->Enabled;
  if (Result)
  {
    // cannot cancel note too early
    if (Now() - FNoteShown >
          EncodeTimeVerbose(0, 0, (unsigned short)(WinConfiguration->NotificationsStickTime), 0))
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
void __fastcall TCustomScpExplorerForm::AddNote(UnicodeString Note, bool UpdateNow)
{
  int P = Note.Pos(L"\n");
  if (P > 0)
  {
    Note.SetLength(P - 1);
  }

  FNotes->Add(FORMAT(L"[%s] %s",
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
void __fastcall TCustomScpExplorerForm::PostNote(UnicodeString Note,
  unsigned int Seconds, TNotifyEvent OnNoteClick, TObject * NoteData)
{
  int P = Note.Pos(L"\n");
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
  FNoteTimer->Interval = Seconds * MSecsPerSec;
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
  if (NonVisualDataModule->SynchronizeBrowsingAction->Checked)
  {
    Configuration->Usage->Inc(L"SynchronizeBrowsingEnabled");
  }

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
void __fastcall TCustomScpExplorerForm::ToggleFormatSizeBytes()
{
  WinConfiguration->FormatSizeBytes = !WinConfiguration->FormatSizeBytes;
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
      UnicodeString APath = UnixExcludeTrailingBackslash(RemoteDirView->Path);
      while (!IsUnixRootPath(APath))
      {
        int P = APath.LastDelimiter(L'/');
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
  TTBXComboBoxItem * Sender, const UnicodeString /*AText*/, int AIndex,
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

  UnicodeString APath = UnixExcludeTrailingBackslash(RemoteDirView->Path);
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
void __fastcall TCustomScpExplorerForm::DirViewEditing(
  TObject * Sender, TListItem * Item, bool & /*AllowEdit*/)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  assert(DirView != NULL);
  if (!WinConfiguration->RenameWholeName && !DirView->ItemIsDirectory(Item))
  {
    HWND Edit = ListView_GetEditControl(DirView->Handle);
    // OnEditing is called also from TCustomListView::CanEdit
    if (Edit != NULL)
    {
      EditSelectBaseName(Edit);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormActivate(TObject * /*Sender*/)
{
  Application->OnHint = ApplicationHint;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateWnd()
{
  TForm::CreateWnd();
  if (FSessionsDragDropFilesEx == NULL)
  {
    FSessionsDragDropFilesEx = new TDragDropFilesEx(this);
    FSessionsDragDropFilesEx->AutoDetectDnD = false;
    FSessionsDragDropFilesEx->NeedValid = TFileExMustDnDSet() << nvFilename;
    FSessionsDragDropFilesEx->RenderDataOn = rdoEnterAndDropSync;
    FSessionsDragDropFilesEx->TargetEffects = TDropEffectSet() << deCopy << deMove;
    FSessionsDragDropFilesEx->OnDragOver = SessionsDDDragOver;
    FSessionsDragDropFilesEx->OnProcessDropped = SessionsDDProcessDropped;
    FSessionsDragDropFilesEx->OnDragEnter = SessionsDDDragEnter;
    FSessionsDragDropFilesEx->OnDragLeave = SessionsDDDragLeave;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DestroyWnd()
{
  TForm::DestroyWnd();
  FSessionsDragDropFilesEx->DragDropControl = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormShow(TObject * /*Sender*/)
{
  SideEnter(FCurrentSide);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFindFiles(
  UnicodeString Directory, const TFileMasks & FileMask,
  TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile)
{
  Configuration->Usage->Inc(L"FileFinds");
  FTerminal->FilesFind(Directory, FileMask, OnFileFound, OnFindingFile);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFocusRemotePath(UnicodeString Path)
{
  RemoteDirView->Path = UnixExtractFilePath(Path);
  TListItem * Item = RemoteDirView->FindFileItem(UnixExtractFileName(Path));
  if (Item != NULL)
  {
    RemoteDirView->ItemFocused = Item;
    RemoteDirView->ItemFocused->MakeVisible(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FindFiles()
{
  UnicodeString Path;
  if (DoFileFindDialog(RemoteDirView->Path, DoFindFiles, Path))
  {
    DoFocusRemotePath(Path);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTaskbarList(ITaskbarList3 * TaskbarList)
{
  FTaskbarList = TaskbarList;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlMouseDown(
  TObject * /*Sender*/, TMouseButton Button, TShiftState /*Shift*/, int X, int Y)
{
  int Index = SessionsPageControl->IndexOfTabAt(X, Y);
  if (Index >= 0)
  {
    if (Button == mbRight)
    {
      SessionsPageControl->ActivePageIndex = Index;
      SessionsPageControlChange(NULL);
    }
    else if (Button == mbLeft)
    {
      // "Mouse down" is raised only after tab is switched.
      // If switching tab (switching session) takes long enough for user
      // to actually release the button, "mouse down" is still raised,
      // but we do not get "mouse up" event, so dragging is not cancelled,
      // prevent that by not beginning dragging in the first place.
      if (FLAGSET(GetAsyncKeyState(VK_LBUTTON), 0x8000))
      {
        // when user clicks the "+", we get mouse down only after the session
        // is closed, when new session tab is already on X:Y, so dragging
        // starts, prevent that
        if (MilliSecondsBetween(Now(), FSessionsPageControlNewSessionTime) > 500)
        {
          TTerminal * Terminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
          if (Terminal != NULL)
          {
            SessionsPageControl->BeginDrag(false);
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlDragDrop(
  TObject * /*Sender*/, TObject * /*Source*/, int X, int Y)
{
  int Index = SessionsPageControl->IndexOfTabAt(X, Y);
  // do not allow dropping on the "+" tab
  TTerminal * TargetTerminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
  if ((TargetTerminal != NULL) &&
      (SessionsPageControl->ActivePage->PageIndex != Index))
  {
    Configuration->Usage->Inc(L"SessionTabMoves");
    // this is almost redundant as we would recreate tabs in TerminalListChanged,
    // but we want to actually prevent that to avoid flicker
    SessionsPageControl->ActivePage->PageIndex = Index;
    TTerminal * Terminal = reinterpret_cast<TTerminal *>(SessionsPageControl->ActivePage->Tag);
    TTerminalManager::Instance()->Move(Terminal, TargetTerminal);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlDragOver(
  TObject * Sender, TObject * Source, int X, int Y,
  TDragState /*State*/, bool & Accept)
{
  Accept = (Sender == Source);
  if (Accept)
  {
    int Index = SessionsPageControl->IndexOfTabAt(X, Y);
    TTerminal * Terminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
    // do not allow dragging to the "+" tab
    Accept = (Terminal != NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDDragOver(int /*KeyState*/,
  const TPoint & Point, int & Effect)
{
  int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
  if (Index < 0)
  {
    Effect = DROPEFFECT_None;
  }
  else
  {
    TTerminal * TargetTerminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
    // do not allow dropping on the "+" tab
    if (TargetTerminal == NULL)
    {
      Effect = DROPEFFECT_None;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDProcessDropped(
  TObject * /*Sender*/, int /*KeyState*/, const TPoint & Point, int Effect)
{
  APPLICATION_EXCEPTION_HACK_BEGIN
  {
    int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
    // do not allow dropping on the "+" tab
    TTerminal * TargetTerminal = reinterpret_cast<TTerminal *>(SessionsPageControl->Pages[Index]->Tag);
    if (TargetTerminal != NULL)
    {
      assert(!IsFileControl(DropSourceControl, osRemote));
      if (!IsFileControl(DropSourceControl, osRemote))
      {
        TTerminalManager::Instance()->ActiveTerminal = TargetTerminal;
        RemoteFileControlDragDropFileOperation(SessionsPageControl, Effect,
          TTerminalManager::Instance()->ActiveTerminal->CurrentDirectory);
      }
    }
  }
  APPLICATION_EXCEPTION_HACK_END;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormClose(TObject * /*Sender*/, TCloseAction & /*Action*/)
{
  FShowing = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewRead(TObject * /*Sender*/)
{
  TManagedTerminal * ManagedTerminal =
    dynamic_cast<TManagedTerminal *>(RemoteDirView->Terminal);
  if (ManagedTerminal != NULL)
  {
    ManagedTerminal->DirectoryLoaded = Now();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewSelectItem(TObject * Sender,
  TListItem * /*Item*/, bool /*Selected*/)
{
  TCustomDirView * DirView = reinterpret_cast<TCustomDirView *>(Sender);
  switch (DirView->LastSelectMethod)
  {
    case smKeyboard:
      Configuration->Usage->Inc(L"KeyboardSelections");
      break;

    case smMouse:
      Configuration->Usage->Inc(L"MouseSelections");
      break;
  }
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::AddSessionColor(TColor Color)
{
  if (Color != 0)
  {
    TBitmap * Bitmap = new TBitmap();
    Bitmap->SetSize(FSessionColors->Width, FSessionColors->Height);
    Bitmap->Canvas->Brush->Color = Color;
    Bitmap->Canvas->Brush->Style = bsSolid;
    Bitmap->Canvas->FillRect(TRect(0, 0, FSessionColors->Width, FSessionColors->Height));
    FSessionColors->AddMasked(Bitmap, (TColor)0);
    delete Bitmap;
    return FSessionColors->Count - 1;
  }
  else
  {
    return -1;
  }
}
//---------------------------------------------------------------------------
#pragma warn -8080
