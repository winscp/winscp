//---------------------------------------------------------------------------
#define NO_WIN32_LEAN_AND_MEAN
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "CustomScpExplorer.h"

#include <algorithm>
#include <Bookmarks.h>
#include <Interface.h>
#include <Exceptions.h>
#include <CoreMain.h>
#include <FileSystems.h>
#include <TextsCore.h>
#include <TextsWin.h>
#include <HelpWin.h>

#include <VCLCommon.h>
#include <Progress.h>
#include <SynchronizeProgress.h>

#include <DragExt.h>
#include <WinApi.h>

#include "NonVisual.h"
#include "Glyphs.h"
#include "Tools.h"
#include "WinConfiguration.h"
#include "TerminalManager.h"
#include "EditorManager.h"
#include "ProgParams.h"
#include "Setup.h"
#include <Consts.hpp>
#include <DateUtils.hpp>
#include <TB2Common.hpp>
#include <DirectoryMonitor.hpp>
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>
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
#pragma link "ThemePageControl"
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
#define WM_COMPONENT_HIDE (WM_WINSCP_USER + 4)
#define WM_PASTE_FILES (WM_WINSCP_USER + 14)
static const int SessionPanelCount = 3;
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
class TWindowLock
{
public:
  TWindowLock(TCustomScpExplorerForm * Form) :
    FForm(Form)
  {
    FForm->LockWindow();
  }

  ~TWindowLock()
  {
    FForm->UnlockWindow();
  }

private:
  TCustomScpExplorerForm * FForm;
};
//---------------------------------------------------------------------------
TAutoBatch::TAutoBatch(TCustomScpExplorerForm * Form) :
  FForm(Form)
{
  FForm->BatchStart(FBatchStorage);
}
//---------------------------------------------------------------------------
TAutoBatch::~TAutoBatch()
{
  FForm->BatchEnd(FBatchStorage);
}
//---------------------------------------------------------------------------
struct TTransferOperationParam
{
  TTransferOperationParam();

  UnicodeString TargetDirectory;
  bool Temp;
  bool DragDrop;
  int Options;
  TAutoSwitch Queue;
};
//---------------------------------------------------------------------------
TTransferOperationParam::TTransferOperationParam()
{
  Temp = false;
  DragDrop = false;
  Options = 0;
  Queue = asAuto;
}
//---------------------------------------------------------------------------
class TTransferPresetNoteData : public TObject
{
public:
  UnicodeString Message;
};
//---------------------------------------------------------------------------
class TTerminalNoteData : public TObject
{
public:
  TManagedTerminal * Terminal;
};
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::TCustomScpExplorerForm(TComponent* Owner):
    FFormRestored(false),
    TForm(Owner)
{
  AddStartupSequence(L"F");
  FInvalid = false;
  FCurrentSide = osRemote;
  FEverShown = false;
  FDocks = new TList();
  InitControls();
  DebugAssert(NonVisualDataModule && !NonVisualDataModule->ScpExplorer);
  // So that UpdateCustomCommandsToolbar called from RestoreParams works
  NonVisualDataModule->ScpExplorer = this;
  RestoreParams();
  ConfigurationChanged();
  RemoteDirView->Invalidate();
  FAutoOperation = false;
  FOnFileOperationFinished = NULL;
  FPrimaryOperation = foNone;
  FForceExecution = false;
  FIgnoreNextDialogChar = 0;
  FErrorList = NULL;
  FSynchronizeProgressForm = NULL;
  FProgressForm = NULL;
  FRefreshLocalDirectory = false;
  FRefreshRemoteDirectory = false;
  FDDExtMapFile = NULL;
  // CreateMutexW keeps failing with ERROR_NOACCESS
  FDDExtMutex = CreateMutexA(NULL, false, AnsiString(DRAG_EXT_MUTEX).c_str());
  DebugAssert(FDDExtMutex != NULL);
  FDDTargetControl = NULL;
  FDelayedDeletionTimer = NULL;
  FDelayedDeletionList = new TStringList();
  FDDFileList = NULL;
  FPendingTempSpaceWarn = false;
  FCapturedLog = NULL;
  FDragDropOperation = false;
  memset(&FHistoryMenu, 0, sizeof(FHistoryMenu));
  FAllowTransferPresetAutoSelect = true;
  FSessionChanging = false;
  FCopyParamDefault = L"";
  FSynchronizeController = NULL;
  FPendingQueueActionItem = NULL;
  FLockLevel = 0;
  FLockSuspendLevel = 0;
  FDisabledOnLockSuspend = false;
  FAlternativeDelete = false;
  FTrayIcon = new ::TTrayIcon(0);
  FTrayIcon->OnClick = TrayIconClick;
  FMaxQueueLength = 0;
  FLastContextPopupScreenPoint = TPoint(-1, -1);
  FTransferResumeList = NULL;
  FDeletedFiles = NULL;
  FMoveToQueue = false;
  StandaloneOperation = false;
  FOnFeedSynchronizeError = NULL;
  FOnSynchronizeAbort = NULL;
  FSynchronizeTerminal = NULL;
  FNeedSession = false;
  FDoNotIdleCurrentTerminal = 0;
  FIncrementalSearching = 0;
  FQueueFileList.reset(new TQueueFileList());
  FProgressSide = osCurrent;
  FCalculateSizeOperation = NULL;
  FDownloadingFromClipboard = false;
  FClipboardFakeMonitorsPendingReset = false;
  FHiContrastTheme = NULL;
  InitializeRemoteThumbnailMask();

  GlyphsModule->PixelsPerInch = CurrentPPI;

  FEditorManager = new TEditorManager();
  FEditorManager->OnFileChange = ExecutedFileChanged;
  FEditorManager->OnFileReload = ExecutedFileReload;
  FEditorManager->OnFileEarlyClosed = ExecutedFileEarlyClosed;
  FEditorManager->OnFileUploadComplete = ExecutedFileUploadComplete;

  FLocalEditors = new TList();

  FQueueStatus = NULL;
  FQueueStatusSection = new TCriticalSection();
  FQueueStatusInvalidated = false;
  FQueueItemInvalidated = false;
  FQueueStatusUpdating = false;
  FQueueActedItem = NULL;
  FQueueController = new TQueueController(QueueView3);
  FQueueScrollOnDragOver = new TListViewScrollOnDragOver(QueueView3, true);

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
  DebugAssert(TransferList != NULL);
  FTransferListHoverIndex = -1;
  TransferList->OnChange = TransferListChange;
  TransferList->OnDrawItem = TransferListDrawItem;

  SetSubmenu(dynamic_cast<TTBXCustomItem *>(static_cast<TObject *>(GetComponent(fcColorMenu))), true);
  SetSubmenu(NonVisualDataModule->ColorMenuItem, true);

  UseDesktopFont(SessionsPageControl);
  UseDesktopFont(RemoteDirView);
  UseDesktopFont(RemoteDriveView);
  UseDesktopFont(QueueView3);
  UseDesktopFont(QueueFileList);
  UseDesktopFont(QueueLabel);
  UseDesktopFont(RemoteStatusBar);

  reinterpret_cast<TLabel*>(QueueSplitter)->OnDblClick = QueueSplitterDblClick;
  QueueSplitter->ShowHint = true;
  reinterpret_cast<TLabel*>(QueueFileListSplitter)->OnDblClick = QueueFileListSplitterDblClick;
  QueueFileListSplitter->ShowHint = true;
  RemotePanelSplitter->ShowHint = true;

  UpdateImages();

  FCustomCommandMenu = CreateTBXPopupMenu(this);
  FCustomCommandLocalFileList = NULL;
  FCustomCommandRemoteFileList = NULL;

  FSessionColors = new TPngImageList(this);
  FSessionColors->ColorDepth = cd32Bit;
  AddFixedSessionImages();
  SessionsPageControl->Images = FSessionColors;
  // Needed for Explorer (Commander calls it implicitly on its own)
  UpdateSessionsPageControlHeight();
  UpdateQueueLabel();
  CheckStoreTransition();

  CreateHiddenWindow();
  StartUpdates();
  // Start loading drive data, so that they are ready once we need them for StartCreationDirectoryMonitorsOnEachDrive
  // (necessary for "explorer" only, as "commander" loads it for its drive menu)
  DriveInfo->NeedData();
}
//---------------------------------------------------------------------------
__fastcall TCustomScpExplorerForm::~TCustomScpExplorerForm()
{
  AppLog(L"Destroying main window");
  FInvalid = true;
  ReleaseHiContrastTheme();
  if (FClipboardTerminal != NULL)
  {
    ClipboardClear(); // implies ClipboardStop
  }
  // this has to be one of the very first things to do
  StopUpdateThread();
  // This is needed when shuting down Windows only. Otherwise it's already set NULL from Execute()
  TTerminalManager::Instance()->ScpExplorer = NULL;

  delete FCustomCommandLocalFileList;
  delete FCustomCommandRemoteFileList;
  delete FCustomCommandMenu;

  delete FTrayIcon;
  FTrayIcon = NULL;

  FEditorManager->CloseInternalEditors(ForceCloseInternalEditor);
  delete FEditorManager;

  ForceCloseLocalEditors();
  delete FLocalEditors;

  HideFileFindDialog();

  if (FDelayedDeletionTimer)
  {
    DoDelayedDeletion(NULL);
    SAFE_DESTROY(FDelayedDeletionTimer);
  }
  SAFE_DESTROY(FDelayedDeletionList);
  // sometimes we do not get DDEnd so the list is not released
  SAFE_DESTROY(FDDFileList);

  DebugAssert(FSynchronizeController == NULL);

  CloseHandle(FDDExtMutex);
  FDDExtMutex = NULL;

  FreeLibrary(FOle32Library);
  FOle32Library = NULL;
  FDragMoveCursor = NULL;

  DebugAssert(!FErrorList);
  if (FEverShown)
  {
    // when window is never shown (like when running command-line operation),
    // particularly window site is not restored correctly (BoundsRect value set
    // in RestoreForm gets lost during handle allocation), so we do not want
    // it to be stored
    AppLog(L"Storing main window configuration");
    StoreParams();
  }
  ManagedSession = NULL;
  Queue = NULL;
  DebugAssert(NonVisualDataModule && (NonVisualDataModule->ScpExplorer == this));
  NonVisualDataModule->ScpExplorer = NULL;

  delete FQueueScrollOnDragOver;
  FQueueScrollOnDragOver = NULL;
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

  SessionsPageControl->Images = NULL;
  // only after clearing Terminal (after SessionListChanged => ... => AddSessionColor is called)
  SAFE_DESTROY(FSessionColors);
  SAFE_DESTROY(FSessionsDragDropFilesEx);

  SAFE_DESTROY(FHistoryMenu[0][0]);
  SAFE_DESTROY(FHistoryMenu[0][1]);
  SAFE_DESTROY(FHistoryMenu[1][0]);
  SAFE_DESTROY(FHistoryMenu[1][1]);

  if (FHiddenWindow != NULL)
  {
    DestroyWindow(FHiddenWindow);
    FHiddenWindow = NULL;
  }
  AppLog(L"Main window destroyed");

}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::ReleaseHiContrastTheme()
{
  if (FHiContrastTheme != NULL)
  {
    ReleaseTBXTheme(FHiContrastTheme);
    FHiContrastTheme = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RefreshPanel(const UnicodeString & Session, const UnicodeString & Path)
{

  std::unique_ptr<TSessionData> Data;
  if (!Session.IsEmpty())
  {
    bool DefaultsOnly;
    Data.reset(StoredSessions->ParseUrl(Session, NULL, DefaultsOnly));
  }

  TTerminalManager * Manager = TTerminalManager::Instance();
  for (int Index = 0; Index < Manager->Count; Index++)
  {
    TTerminal * Terminal = Manager->Sessions[Index];
    if (Session.IsEmpty() ||
        Manager->IsActiveTerminalForSite(Terminal, Data.get()))
    {
      if (Path.IsEmpty())
      {
        Terminal->ClearCaches();
      }
      else
      {
        Terminal->DirectoryModified(Path, true);
      }
    }
  }

  // We should flag a pending refresh for the background terminals or busy foreground terminals
  if (HasActiveTerminal() &&
      CanCommandLineFromAnotherInstance() &&
      (Session.IsEmpty() || Manager->IsActiveTerminalForSite(Terminal, Data.get())) &&
      (Path.IsEmpty() || UnixIsChildPath(Path, Terminal->CurrentDirectory)))
  {
    Terminal->ReloadDirectory();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMCopyData(TMessage & Message)
{
  PCOPYDATASTRUCT CopyData = reinterpret_cast<PCOPYDATASTRUCT>(Message.LParam);

  AppLogFmt(L"Received copy data message with size %d", (int(CopyData->cbData)));
  size_t MessageSize = sizeof(TCopyDataMessage);
  bool Result = DebugAlwaysTrue(CopyData->cbData == MessageSize);
  if (Result)
  {
    const TCopyDataMessage & Message = *reinterpret_cast<const TCopyDataMessage *>(CopyData->lpData);

    Result = (Message.Version == TCopyDataMessage::Version1);

    if (Result)
    {
      switch (Message.Command)
      {
        case TCopyDataMessage::CommandCanCommandLine:
          Result = CanCommandLineFromAnotherInstance();
          if (Result)
          {
            AppLog(L"Can process command-line from other instance");
          }
          else
          {
            AppLog(L"Cannot process command-line from other instance");
          }
          break;

        case TCopyDataMessage::CommandCommandLine:
          {
            UnicodeString CommandLine(Message.CommandLine);
            Result = CommandLineFromAnotherInstance(CommandLine);
          }
          break;

        case TCopyDataMessage::RefreshPanel:
          RefreshPanel(Message.Refresh.Session, Message.Refresh.Path);
          break;

        case TCopyDataMessage::MainWindowCheck:
          Result = true;
          break;

        default:
          Result = false;
          break;
      }
    }
  }

  Message.Result = Result ? 1 : 0;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateHiddenWindow()
{
  WNDCLASS WindowClass = {0};
  WindowClass.lpfnWndProc = DefWindowProc;
  WindowClass.hInstance = HInstance;
  WindowClass.lpszClassName = HIDDEN_WINDOW_NAME;

  FHiddenWindow = NULL;

  if (RegisterClass(&WindowClass))
  {
    FHiddenWindow = CreateWindow(HIDDEN_WINDOW_NAME, L"",
      WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, NULL);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanConsole()
{
  return
    HasActiveTerminal() &&
    (Terminal->IsCapable[fcAnyCommand] || Terminal->IsCapable[fcSecondaryShell]);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanCommandLineFromAnotherInstance()
{
  bool Result = Visible && !NonVisualDataModule->Busy;
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CommandLineFromAnotherInstance(
  const UnicodeString & CommandLine)
{
  TProgramParams Params(CommandLine);
  bool Result = CanCommandLineFromAnotherInstance() && DebugAlwaysTrue(Params.ParamCount > 0);
  if (Result)
  {
    NonVisualDataModule->StartBusy();
    try
    {
      // this action is initiated from another process,
      // so it's likely that our window is not visible,
      // and user won't see what is going on
      Application->BringToFront();
      // reload sessions as we may be asked to open a session
      // just stored by another instance
      StoredSessions->Reload();
      UnicodeString SessionName = Params.Param[1];
      std::unique_ptr<TObjectList> DataList(new TObjectList());
      try
      {
        UnicodeString DownloadFile; // unused
        int Flags = GetCommandLineParseUrlFlags(&Params);
        GetLoginData(SessionName, &Params, DataList.get(), DownloadFile, true, this, Flags);
      }
      catch (EAbort &)
      {
        // Happens when opening session in PuTTY and in other situations,
        // in which we do not want to fail WM_COPYDATA, as that would cause master instance
        // to process the command-line on its own (or to pass it to yet another instance)
      }
      if (DataList->Count > 0)
      {
        TTerminalManager * Manager = TTerminalManager::Instance();
        Manager->ActiveSession = Manager->NewSessions(DataList.get());
      }
    }
    __finally
    {
      NonVisualDataModule->EndBusy();
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
TManagedTerminal * TCustomScpExplorerForm::GetTerminal()
{
  TManagedTerminal * Result;
  if ((FManagedSession != NULL) && !FManagedSession->LocalBrowser)
  {
    Result = FManagedSession;
  }
  else
  {
    Result = NULL;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetManagedSession(TManagedTerminal * value)
{
  if (FManagedSession != value)
  {
    TWindowLock Lock(this);
    SessionChanging();
    DoSetManagedSession(value, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSetManagedSession(TManagedTerminal * value, bool Replace)
{
  DebugAssert(!Replace || ((value != NULL) && !value->LocalBrowser));
  FManagedSession = value;
  {
    TValueRestorer<bool> AllowTransferPresetAutoSelectRestorer(FAllowTransferPresetAutoSelect, false);
    TAutoFlag SessionChangingFlag(FSessionChanging);

    SessionChanged(Replace);
  }

  if (Terminal != NULL)
  {
    TransferPresetAutoSelect();
  }
  // Update app and tab titles, prevented above by FSessionChanging.
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReplaceTerminal(TManagedTerminal * value)
{
  FEditorManager->ProcessFiles(FileTerminalReplaced, value);
  DoSetManagedSession(value, true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionChanging()
{
  if (ManagedSession != NULL)
  {
    UpdateSession(ManagedSession);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionChanged(bool Replaced)
{
  if (ManagedSession != NULL)
  {
    UpdateSessionColor(static_cast<TColor>(ManagedSession->StateData->Color));
  }
  if (!TTerminalManager::Instance()->Updating)
  {
    SessionListChanged();
  }

  if (Terminal != NULL)
  {
    TObject * State = NULL;
    if (WinConfiguration->PreservePanelState)
    {
      State = Terminal->RemoteExplorerState;
      DirView(osRemote)->AnnounceState(State);
    }

    try
    {
      if (Replaced)
      {
        RemoteDirView->ReplaceTerminal(Terminal);
      }
      else
      {
        RemoteDirView->Terminal = Terminal;
      }

      if (Terminal->Active)
      {
        Terminal->RefreshDirectory();
      }

      if (State != NULL)
      {
        DirView(osRemote)->RestoreState(State);
      }
    }
    __finally
    {
      DirView(osRemote)->AnnounceState(NULL);
    }
  }
  else
  {
    RemoteDirView->Terminal = NULL;
  }

  InitStatusBar();
  UpdateTransferList();
  // Update panels Enable state before refreshing the labels
  UpdateControls();
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
      DebugAssert(FQueue->OnListUpdate == NULL);
      FQueue->OnListUpdate = QueueListUpdate;
      DebugAssert(FQueue->OnQueueItemUpdate == NULL);
      FQueue->OnQueueItemUpdate = QueueItemUpdate;
    }
    QueueChanged();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3Deletion(TObject * /*Sender*/,
  TListItem * Item)
{
  if (FQueueActedItem == Item)
  {
    FQueueActedItem = NULL;
    if ((QueueView3->PopupMenu != NULL) &&
        (QueueView3->PopupMenu->PopupComponent == QueueView3))
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
bool TCustomScpExplorerForm::IsAnythingQueued()
{
  return (FQueueStatus != NULL) && (FQueueStatus->ActiveAndPendingPrimaryCount > 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueStatus(bool QueueChanging)
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

  {
    TAutoFlag Flag(FQueueStatusUpdating);
    FQueueController->UpdateQueueStatus(FQueueStatus);
  }
  UpdateQueueFileList();
  SetQueueProgress();

  UpdateQueueView();

  bool IsEmpty = !IsAnythingQueued();

  if (IsEmpty && (Terminal != NULL))
  {
    TOnceDoneOperation OnceDoneOperation =
      NonVisualDataModule->CurrentQueueOnceEmptyOperation();
    NonVisualDataModule->ResetQueueOnceEmptyOperation();

    if ((FQueue != NULL) && !WinConfiguration->EnableQueueByDefault && !QueueChanging)
    {
      FQueue->Enabled = false;
    }

    if ((OnceDoneOperation != odoIdle) && !NonVisualDataModule->Busy)
    {
      Terminal->CloseOnCompletion(OnceDoneOperation, LoadStr(CLOSED_ON_QUEUE_EMPTY));
    }
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::GetQueueProgressTitle()
{
  UnicodeString Result;
  if (FQueueStatus != NULL)
  {
    if (FQueueStatus->IsOnlyOneActiveAndNoPending())
    {
      TFileOperationProgressType * ProgressData =
        FQueueStatus->Items[FQueueStatus->DoneCount]->ProgressData;
      if ((ProgressData != NULL) && ProgressData->InProgress)
      {
        Result = TProgressForm::ProgressStr(NULL, ProgressData);
      }
    }
    else if (FQueueStatus->ActiveAndPendingPrimaryCount > 1)
    {
      Result = FMTLOAD(PROGRESS_IN_QUEUE, (FQueueStatus->ActiveAndPendingPrimaryCount));
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueView()
{
  ComponentVisible[fcQueueView] =
    !IsLocalBrowserMode() &&
    ((WinConfiguration->QueueView.Show == qvShow) ||
     ((WinConfiguration->QueueView.Show == qvHideWhenEmpty) &&
      (FQueueStatus != NULL) && (FQueueStatus->Count > 0)));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueChanged()
{
  if (FQueueStatus != NULL)
  {
    // UpdateFileList implementation relies on the status being recreated when session changes
    delete FQueueStatus;
    FQueueStatus = NULL;
  }
  UpdateQueueStatus(true);
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
bool __fastcall TCustomScpExplorerForm::IsQueueAutoPopup()
{
  // during standalone editing, we have no way to see/control queue,
  // so we have to always popup prompts automatically
  return StandaloneOperation || GUIConfiguration->QueueAutoPopup;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RefreshQueueItems()
{
  if (FQueueStatus != NULL)
  {
    bool QueueAutoPopup = IsQueueAutoPopup();
    bool NeedRefresh = FQueueController->NeedRefresh();
    bool Refresh = FQueueItemInvalidated || NeedRefresh;
    FQueueItemInvalidated = false;

    int Limit = Refresh ? FQueueStatus->Count : FQueueStatus->DoneAndActiveCount;

    bool Updated = false;
    TQueueItemProxy * QueueItem;
    bool WasUserAction;
    for (int Index = 0; Index < Limit; Index++)
    {
      bool Update = false;
      QueueItem = FQueueStatus->Items[Index];
      WasUserAction = TQueueItem::IsUserActionStatus(QueueItem->Status);
      if (!NonVisualDataModule->Busy && QueueAutoPopup && WasUserAction &&
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
      else if (FQueueController->QueueItemNeedsFrequentRefresh(QueueItem) || NeedRefresh)
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
      SetQueueProgress();
      UpdateQueueFileList();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTaskbarListProgressState(TBPFLAG Flags)
{
  if (FTaskbarList != NULL)
  {
    // Could now use Application->MainFormHandle, now that we implement Application->OnGetMainFormHandle
    FTaskbarList->SetProgressState(GetMainForm()->Handle, Flags);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTaskbarListProgressValue(int Progress)
{
  if (Progress >= 0)
  {
    if (FTaskbarList != NULL)
    {
      FTaskbarList->SetProgressValue(GetMainForm()->Handle, Progress, 100);
    }
  }
  else
  {
    SetTaskbarListProgressState(TBPF_INDETERMINATE);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetTaskbarListProgressValue(TFileOperationProgressType * ProgressData)
{
  int OverallProgress;
  // FProgressForm is null when this is called from SetQueueProgress
  if ((FProgressForm != NULL) && (FProgressForm->SynchronizeProgress != NULL))
  {
    OverallProgress = FProgressForm->SynchronizeProgress->Progress(ProgressData);
  }
  else if (!ProgressData->IsIndeterminate())
  {
    OverallProgress = ProgressData->OverallProgress();
  }
  else
  {
    OverallProgress = -1;
  }
  SetTaskbarListProgressValue(OverallProgress);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetQueueProgress()
{
  TTerminalManager::Instance()->QueueStatusUpdated();

  if ((FTaskbarList != NULL) && (FProgressForm == NULL))
  {
    // Do not animate task bar button, when items are queued, but processing is disabled
    if ((FQueueStatus != NULL) && (FQueueStatus->ActivePrimaryCount > 0))
    {
      if (FQueueStatus->IsOnlyOneActiveAndNoPending())
      {
        TFileOperationProgressType * ProgressData = NULL;
        if (FQueueStatus->Items[FQueueStatus->DoneCount] != NULL)
        {
          ProgressData = FQueueStatus->Items[FQueueStatus->DoneCount]->ProgressData;
        }

        if ((ProgressData != NULL) &&
            ProgressData->InProgress)
        {
          SetTaskbarListProgressValue(ProgressData);
        }
        else
        {
          SetTaskbarListProgressState(TBPF_NOPROGRESS);
        }
      }
      else
      {
        SetTaskbarListProgressState(TBPF_INDETERMINATE);
      }
    }
    else
    {
      SetTaskbarListProgressState(TBPF_NOPROGRESS);
    }
  }

  UpdateQueueLabel();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueLabel()
{
  UnicodeString Caption = LoadStr(QUEUE_CAPTION);
  if (FQueueStatus != NULL)
  {
    int ActiveAndPendingPrimaryCount = FQueueStatus->ActiveAndPendingPrimaryCount;
    if (ActiveAndPendingPrimaryCount > 0)
    {
      Caption = FORMAT("%s (%d)", (Caption, ActiveAndPendingPrimaryCount));
    }
  }
  QueueLabel->Caption = Caption;
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::CheckStoreTransition()
{
  if (WinConfiguration->StoreTransition == stInit)
  {
    if (IsUWP())
    {
      WinConfiguration->StoreTransition = stStoreFresh;
      AppLog(L"Fresh UWP/store installation");
    }
    else
    {
      WinConfiguration->StoreTransition = stStandard;
      AppLog(L"Fresh standard installation");
    }
  }
  else if ((WinConfiguration->StoreTransition == stStandard) && IsUWP())
  {
    WinConfiguration->StoreTransition = stStoreMigrated;
    WinConfiguration->FirstRun = WinConfiguration->FirstRun + L"=>" + StandardDatestamp();
    AppLog(L"Standard installation migrated to store installation");
  }

  if (WinConfiguration->StoreTransition == stStoreMigrated)
  {
    TTBXToolbar * Toolbar = new TTBXToolbar(MessageDock);
    Toolbar->Parent = MessageDock;
    Toolbar->FullSize = true;
    Toolbar->Color = Application->HintColor;

    TTBXLabelItem * LabelItem;
    LabelItem = new TTBXLabelItem(Toolbar);
    // Convert to explicit RGB "black" color, to make sure it is contrasting even in Dark theme
    // (TTBXOfficeXPTheme.GetStandardColor would convert clWindow to "white" in dark theme)
    LabelItem->FontSettings->Color = static_cast<TColor>(GetSysColor(clWindow));
    LabelItem->Margin = ScaleByTextHeight(MessageDock, 2); // Is this automatically rescaled?
    LabelItem->Caption = LoadStr(STORE_MIGRATION_THANKS);
    Toolbar->Items->Add(LabelItem);

    TTBXCustomItem * LinkItem = new TTBXItem(Toolbar);
    LinkItem->FontSettings->Color = LinkColor;
    LinkItem->Caption = LoadStr(STORE_MIGRATION_LEARN);
    LinkItem->OnClick = StoreTransitionLinkClick;
    Toolbar->Items->Add(LinkItem);

    AddMenuSeparator(Toolbar->Items);

    TTBXCustomItem * CloseItem = new TTBXItem(Toolbar);
    CloseItem->FontSettings->Color = static_cast<TColor>(GetSysColor(clWindow));
    CloseItem->Caption = LoadStr(MESSAGE_DISMISS);
    CloseItem->OnClick = StoreTransitionCloseClick;
    Toolbar->Items->Add(CloseItem);
    GiveTBItemPriority(CloseItem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreTransitionLinkClick(TObject *)
{
  ShowHelp(HELP_STORE_TRANSITION);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreTransitionCloseClick(TObject * Sender)
{
  TTBCustomItem * Item = DebugNotNull(dynamic_cast<TTBCustomItem *>(Sender));
  TTBXToolbar * Toolbar = DebugNotNull(dynamic_cast<TTBXToolbar *>(DebugNotNull(DebugNotNull(Item->Parent)->ParentComponent)));
  delete Toolbar;
  WinConfiguration->StoreTransition = stStoreAcknowledged;
  UpdateControls();
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
    TransferDropDown->Hint = FORMAT(L"%s|%s:\n%s",
      (FTransferDropDownHint, StripHotkey(Name),
       GUIConfiguration->CurrentCopyParam.GetInfoStr(L"; ",
         FLAGMASK(HasActiveTerminal(), Terminal->UsableCopyParamAttrs(0).General))));
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
  DebugAssert(Toolbar != NULL);

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
void __fastcall TCustomScpExplorerForm::UpdateSessionsPageControlHeight()
{
  SessionsPageControl->Height = SessionsPageControl->GetTabsHeight();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateRowSelect(TCustomDirView * DirView)
{
  if (DirView->RowSelect != WinConfiguration->FullRowSelect)
  {
    DirView->RowSelect = WinConfiguration->FullRowSelect;
    // Without this, the panel turns gray and unused part of header turns black
    DirView->Perform(CM_COLORCHANGED, 0, 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ConfigurationChanged()
{
  Color = GetBtnFaceColor();

  DebugAssert(Configuration && RemoteDirView);
  RemoteDirView->DDAllowMove = !WinConfiguration->DDDisableMove;
  RemoteDirView->DimmHiddenFiles = WinConfiguration->DimmHiddenFiles;
  RemoteDirView->ShowHiddenFiles = WinConfiguration->ShowHiddenFiles;
  RemoteDirView->FormatSizeBytes = WinConfiguration->FormatSizeBytes;
  RemoteDirView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;
  RemoteDirView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;
  RemoteDirView->AlwaysSortDirectoriesByName = WinConfiguration->AlwaysSortDirectoriesByName;

  UpdateRowSelect(RemoteDirView);

  RemoteDriveView->DDAllowMove = !WinConfiguration->DDDisableMove;
  RemoteDriveView->DimmHiddenDirs = WinConfiguration->DimmHiddenFiles;
  RemoteDriveView->ShowHiddenDirs = WinConfiguration->ShowHiddenFiles;
  RemoteDriveView->ShowInaccesibleDirectories = WinConfiguration->ShowInaccesibleDirectories;
  RemoteDriveView->NaturalOrderNumericalSorting = WinConfiguration->NaturalOrderNumericalSorting;

  UpdateSessionsPageControlHeight();

  SetDockAllowDrag(!WinConfiguration->LockToolbars);
  UpdateToolbarDisplayMode();

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

  // show only when keeping queue items forever,
  // otherwise, it's enough to have it in the context menu
  QueueDeleteAllDoneQueueToolbarItem->Visible =
    WinConfiguration->QueueKeepDoneItems && (WinConfiguration->QueueKeepDoneItemsFor < 0);

  if (FFileColorsCurrent != WinConfiguration->FileColors)
  {
    TFileColorData::LoadList(WinConfiguration->FileColors, FFileColors);
    FFileColorsCurrent = WinConfiguration->FileColors;
    FileColorsChanged();
  }

  if ((GetNewTabTab()->CaptionTruncation != GetNewTabTabCaptionTruncation()) &&
      (FSessionColors != NULL))
  {
    SessionListChanged();
  }

  if (GlyphsModule->LargerToolbar != WinConfiguration->LargerToolbar)
  {
    GlyphsModule->LargerToolbar = WinConfiguration->LargerToolbar;
    if (FSessionColors != NULL)
    {
      RegenerateSessionColorsImageList();
      UpdateSessionsPageControlHeight();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFileColorsChanged(TCustomDirView * DirView)
{
  DirView->OnGetItemColor = FFileColors.empty() ? TDirViewGetItemColorEvent(NULL) : TDirViewGetItemColorEvent(&DirViewGetItemColor);
  DirView->Invalidate();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileColorsChanged()
{
  DoFileColorsChanged(RemoteDirView);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileConfigurationChanged(
  const UnicodeString FileName, TEditedFileData * /*Data*/, TObject * Token,
  void * /*Arg*/)
{
  if (Token != NULL)
  {
    TForm * Editor = dynamic_cast<TForm*>(Token);
    DebugAssert(Editor != NULL);
    ReconfigureEditorForm(Editor);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::EnableDDTransferConfirmation(TObject * /*Sender*/)
{
  WinConfiguration->DDTransferConfirmation = asOn;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CopyParamDialogAfter(
  TTransferDirection /*Direction*/, bool /*Temp*/, const UnicodeString & /*TargetDirectory*/)
{
  // noop
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::GetDoNotShowCopyDialogDefault(bool DragDrop)
{
  return DragDrop && (WinConfiguration->DDTransferConfirmation == asAuto);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::HandleDoNotShowCopyDialogAgain(bool DragDrop, bool DoNotShowAgain)
{
  if (DoNotShowAgain)
  {
    if (DragDrop)
    {
      if (WinConfiguration->DDTransferConfirmation == asAuto)
      {
        PopupTrayBalloon(
          NULL, LoadStr(DD_TRANSFER_CONFIRM_OFF2), qtInformation, NULL, 0, EnableDDTransferConfirmation, NULL);
      }
      WinConfiguration->DDTransferConfirmation = asOff;
    }
    else
    {
      WinConfiguration->ConfirmTransferring = false;
    }
  }
  else
  {
    // User explicitly unchecked "do not show again",
    // so show him the dialog the next time
    if (DragDrop && (WinConfiguration->DDTransferConfirmation == asAuto))
    {
      WinConfiguration->DDTransferConfirmation = asOn;
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CopyParamDialog(
  TTransferDirection Direction, TTransferType Type, bool Temp,
  TStrings * FileList, UnicodeString & TargetDirectory, TGUICopyParamType & CopyParam,
  bool Confirm, bool DragDrop, int Options)
{
  bool Result = true;
  DebugAssert(HasActiveTerminal());

  // these parameters are known in advance
  int Params =
    FLAGMASK(Type == ttMove, cpDelete);
  bool ToTemp = (Temp && (Direction == tdToLocal));
  if (Result && Confirm && WinConfiguration->ConfirmTransferring)
  {
    bool DisableNewerOnly =
      (!Terminal->IsCapable[fcNewerOnlyUpload] && (Direction == tdToRemote)) ||
      ToTemp;
    Options |=
      FLAGMASK(ToTemp, coTemp) |
      FLAGMASK(!Terminal->IsCapable[fcBackgroundTransfers], coDisableQueue) |
      coDoNotShowAgain;
    TUsableCopyParamAttrs UsableCopyParamAttrs = Terminal->UsableCopyParamAttrs(Params);
    int CopyParamAttrs = (Direction == tdToRemote ?
      UsableCopyParamAttrs.Upload : UsableCopyParamAttrs.Download) |
      FLAGMASK(DisableNewerOnly, cpaNoNewerOnly);
    int OutputOptions = FLAGMASK(GetDoNotShowCopyDialogDefault(DragDrop), cooDoNotShowAgain);
    std::unique_ptr<TSessionData> SessionData(SessionDataForCode());
    FlashOnBackground(); // Particularly when called from ClipboardFakeCreated
    Result = DoCopyDialog(Direction == tdToRemote, Type == ttMove,
      FileList, TargetDirectory, &CopyParam, Options, CopyParamAttrs, SessionData.get(), &OutputOptions, -1);

    if (Result)
    {
      HandleDoNotShowCopyDialogAgain(DragDrop, FLAGSET(OutputOptions, cooDoNotShowAgain));

      CopyParamDialogAfter(Direction, Temp, TargetDirectory);
    }
  }

  if (Result && CopyParam.Queue && !ToTemp && Terminal->IsCapable[fcBackgroundTransfers])
  {

    Configuration->Usage->Inc(L"TransfersOnBackground");

    // these parameter are known only after transfer dialog
    Params |=
      (CopyParam.QueueNoConfirmation ? cpNoConfirmation : 0);

    AddQueueItem(Queue, Direction, FileList, TargetDirectory, CopyParam, Params);
    Result = false;

    ClearTransferSourceSelection(Direction);
  }

  return Result;
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::ClearOperationSelection(TOperationSide Side)
{
  if (FOnFileOperationFinished != NULL)
  {
    FOnFileOperationFinished(Side, UnicodeString(), true, true);
  }
  else
  {
    if (HasDirView[Side])
    {
      DirView(Side)->SelectAll(smNone);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClearTransferSourceSelection(TTransferDirection Direction)
{
  TOperationSide Side = ((Direction == tdToRemote) ? osLocal : osRemote);
  ClearOperationSelection(Side);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddQueueItem(
  TTerminalQueue * Queue, TTransferDirection Direction, TStrings * FileList,
  const UnicodeString TargetDirectory, TGUICopyParamType & CopyParam,
  int Params)
{
  DebugAssert(Queue != NULL);

  TQueueItem * QueueItem;
  if (Direction == tdToRemote)
  {
    CopyParam.IncludeFileMask.SetRoots(FileList, TargetDirectory);
    QueueItem =
      new TUploadQueueItem(
        Terminal, FileList, TargetDirectory, &CopyParam, Params, CopyParam.QueueParallel);
  }
  else
  {
    CopyParam.IncludeFileMask.SetRoots(TargetDirectory, FileList);
    QueueItem =
      new TDownloadQueueItem(
        Terminal, FileList, TargetDirectory, &CopyParam, Params, CopyParam.QueueParallel);
  }
  AddQueueItem(Queue, QueueItem, Terminal);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddQueueItem(TTerminalQueue * Queue, TQueueItem * QueueItem, TManagedTerminal * Terminal)
{
  if (Queue->IsEmpty)
  {
    Terminal->QueueOperationStart = Now();
  }
  Queue->AddItem(QueueItem);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreFormParams()
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InitControls()
{
  DebugAssert(FDocks != NULL);
  for (int Index = 0; Index < ComponentCount; Index++)
  {
    TTBDock * Dock = dynamic_cast<TTBDock *>(Components[Index]);
    if ((Dock != NULL) && (Dock->Tag == 0))
    {
      FDocks->Add(Dock);
    }
  }

  CollectItemsWithTextDisplayMode(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreParams()
{
  ConfigurationChanged();

  QueuePanel->Height =
    LoadDimension(WinConfiguration->QueueView.Height, WinConfiguration->QueueView.HeightPixelsPerInch, this);
  LoadListViewStr(QueueView3, WinConfiguration->QueueView.Layout);
  QueueDock->Visible = WinConfiguration->QueueView.ToolBar;
  QueueLabel->Visible = WinConfiguration->QueueView.Label;
  QueueFileList->Visible = WinConfiguration->QueueView.FileList;
  QueueFileList->Height =
    LoadDimension(WinConfiguration->QueueView.FileListHeight, WinConfiguration->QueueView.FileListHeightPixelsPerInch, this);
  if (QueueFileList->Visible)
  {
    AdjustQueueLayout();
  }
  UpdateCustomCommandsToolbar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StoreParams()
{
  int APixelsPerInch = GetControlPixelsPerInch(this);
  WinConfiguration->QueueView.Height = QueuePanel->Height;
  WinConfiguration->QueueView.HeightPixelsPerInch = APixelsPerInch;
  WinConfiguration->QueueView.Layout = GetListViewStr(QueueView3);
  WinConfiguration->QueueView.ToolBar = QueueDock->Visible;
  WinConfiguration->QueueView.Label = QueueLabel->Visible;
  WinConfiguration->QueueView.FileList = QueueFileList->Visible;
  WinConfiguration->QueueView.FileListHeight = QueueFileList->Height;
  WinConfiguration->QueueView.FileListHeightPixelsPerInch = APixelsPerInch;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Loaded()
{
  // Loaded is called in more deterministic moment than previously used CreateParams
  if (!FFormRestored)
  {
    FFormRestored = true;
    RestoreFormParams();
  }
  TForm::Loaded();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetDockAllowDrag(bool value)
{
  DebugAssert(FDocks != NULL);
  for (int Index = 0; Index < FDocks->Count; Index++)
  {
    static_cast<TTBDock*>(FDocks->Items[Index])->AllowDrag = value;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LoadToolbarsLayoutStr(UnicodeString LayoutStr, UnicodeString ButtonsStr)
{
  SetDockAllowDrag(true);

  while (!ButtonsStr.IsEmpty())
  {
    UnicodeString S = CutToChar(ButtonsStr, L';', true);
    UnicodeString Name = CutToChar(S, L'=', true);
    for (int ToolbarIndex = 0; ToolbarIndex < ComponentCount; ToolbarIndex++)
    {
      TTBCustomToolbar * Toolbar = dynamic_cast<TTBCustomToolbar *>(Components[ToolbarIndex]);
      if ((Toolbar != NULL) && SameText(Name, GetToolbarKey(Toolbar->Name)))
      {
        while (!S.IsEmpty())
        {
          UnicodeString S2 = CutToChar(S, L',', true);
          UnicodeString Name = CutToChar(S2, L':', true);
          for (int ItemIndex = 0; ItemIndex < Toolbar->Items->Count; ItemIndex++)
          {
            TTBCustomItem * Item = Toolbar->Items->Items[ItemIndex];
            if (SameText(GetToolbarItemName(Item), Name))
            {
              // forward compatibility
              UnicodeString S3 = CutToChar(S2, L':', true);
              bool Visible = StrToIntDef(S3, 0);
              Item->Visible = Visible;
            }
          }
        }
      }
    }
  }

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
UnicodeString __fastcall TCustomScpExplorerForm::GetToolbarItemName(TTBCustomItem * Item)
{
  UnicodeString Result;
  if (Item->Action != NULL)
  {
    Result = Item->Action->Name;
    Result = RemoveSuffix(Result, L"Action", true);
  }
  else
  {
    Result = Item->Name;
    Result = RemoveSuffix(Result, L"SubmenuItem", true);
    Result = RemoveSuffix(Result, L"Item", true);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::GetToolbarsButtonsStr()
{
  UnicodeString Result;
  for (int ToolbarIndex = 0; ToolbarIndex < ComponentCount; ToolbarIndex++)
  {
    UnicodeString ToolbarStr;
    TTBCustomToolbar * Toolbar = dynamic_cast<TTBCustomToolbar *>(Components[ToolbarIndex]);
    if ((Toolbar != NULL) && (Toolbar != QueueToolbar))
    {
      for (int ItemIndex = 0; ItemIndex < Toolbar->Items->Count; ItemIndex++)
      {
        TTBCustomItem * Item = Toolbar->Items->Items[ItemIndex];
        // Currently all buttons are visible by default, so we can safely remember all hidden buttons.
        // Once we introduce any buttons that are hidden by default, we would have to remember their initial state
        // and save the changes here only.
        if (NonVisualDataModule->IsCustomizableToolbarItem(Item) && !Item->Visible)
        {
          UnicodeString Name = GetToolbarItemName(Item);
          DebugAssert(Name.Pos(L"TBX") == 0);
          AddToList(ToolbarStr, FORMAT(L"%s:0", (Name)), L",");
        }
      }
    }

    if (!ToolbarStr.IsEmpty())
    {
      AddToList(Result, FORMAT(L"%s=%s", (GetToolbarKey(Toolbar->Name), ToolbarStr)), L";");
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateProgressForm(TSynchronizeProgress * SynchronizeProgress)
{
  DebugAssert(FProgressForm == NULL);
  bool AllowSkip = HasActiveTerminal() ? Terminal->IsCapable[fcSkipTransfer] : false;
  bool AllowMoveToQueue = (FTransferResumeList != NULL) || (FDeletedFiles != NULL);
  FProgressForm = new TProgressForm(Application, AllowMoveToQueue, AllowSkip, SynchronizeProgress);

  FProgressForm->DeleteLocalToRecycleBin =
    (WinConfiguration->DeleteToRecycleBin != FAlternativeDelete);
  FProgressForm->DeleteRemoteToRecycleBin =
    HasActiveTerminal() &&
    (Terminal->SessionData->DeleteToRecycleBin != FAlternativeDelete) &&
    !Terminal->SessionData->RecycleBinPath.IsEmpty();

  // As progress window has delayed show now, we need to lock ourselves, (at least) until then
  LockWindow();

  // Actually, do not know what hides the progress once the operation finishes
  // (it is possibly the SetQueueProgress - and we should not rely on that)
  SetTaskbarListProgressState(TBPF_NORMAL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DestroyProgressForm()
{
  UnlockWindow();
  SAFE_DESTROY(FProgressForm);
  SetQueueProgress();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileOperationProgress(
  TFileOperationProgressType & ProgressData)
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
    CreateProgressForm(NULL);
    DebugAssert(FProgressForm->OnceDoneOperation == odoIdle);
    FProgressForm->OnceDoneOperation = ProgressData.InitialOnceDoneOperation;
  }
  // operation is finished (or terminated), so we hide progress form
  else if (!ProgressData.InProgress && (FProgressForm != NULL) && (FProgressForm->SynchronizeProgress == NULL))
  {
    DestroyProgressForm();

    if (ProgressData.Operation == foCalculateSize)
    {
      // When calculating size before transfer, the abort caused by
      // cancel flag set due to "MoveToQueue" below,
      // is not propagated back to us in ExecuteFileOperation,
      // as it is silently swallowed.
      // So we explicitly (re)throw it here.
      if (FMoveToQueue)
      {
        Abort();
      }
    }
    else
    {
      if ((ProgressData.Cancel == csContinue) &&
          !FAutoOperation)
      {
        OperationComplete(ProgressData.StartTime);
      }
    }
  }

  if (FProgressForm &&
      // Can happen only during synchronization, when ending one of the operations
      ProgressData.InProgress)
  {
    FProgressForm->SetProgressData(ProgressData);

    DebugAssert(ProgressData.InProgress);
    SetTaskbarListProgressValue(&ProgressData);

    if (FProgressForm->Cancel > csContinue)
    {
      ProgressData.SetCancelAtLeast(FProgressForm->Cancel);
      if (FProgressForm->Cancel == csCancelFile)
      {
        FProgressForm->ClearCancel();
      }
      // cancel cancels even the move
      FMoveToQueue = false;
    }
    else if (FProgressForm->MoveToQueue)
    {
      FMoveToQueue = true;
      ProgressData.SetCancelAtLeast(csCancel);
    }

    if ((FTransferResumeList != NULL) &&
        ProgressData.InProgress &&
        ProgressData.IsTransfer() &&
        !ProgressData.FullFileName.IsEmpty())
    {
      if ((FTransferResumeList->Count == 0) ||
          (FTransferResumeList->Strings[FTransferResumeList->Count - 1] != ProgressData.FullFileName))
      {
        // note that we do not recognize directories from files here
        FTransferResumeList->Add(ProgressData.FullFileName);
      }
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
    UnicodeString BeepSound = GUIConfiguration->BeepSound;
    DWORD Sound;
    if (!ExtractFileExt(BeepSound).IsEmpty())
    {
      Sound = SND_FILENAME;
    }
    else
    {
      Sound = SND_ALIAS;
    }
    PlaySound(BeepSound.c_str(), NULL, Sound | SND_ASYNC);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OperationProgress(
  TFileOperationProgressType & ProgressData)
{
  // For example, when calling TFileOperationProgressType::Suspend in TTerminal::QueryReopen,
  // we cannot recurse back to TTerminal::Idle as that may detect another error, calling "suspend" again.
  TAutoNestingCounter Counter(FDoNotIdleCurrentTerminal);
  FileOperationProgress(ProgressData);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::GetProgressTitle()
{
  UnicodeString Result;
  if (FProgressForm != NULL)
  {
    Result = FProgressForm->ProgressStr();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::PanelOperation(TOperationSide /*Side*/,
  bool DragDrop)
{
  return (!DragDrop && (DropSourceControl == NULL)) ||
    (DropSourceControl == DirView(osOther));
}
//---------------------------------------------------------------------------
TListItem * TCustomScpExplorerForm::VisualiseOperationFinished(TOperationSide Side, const UnicodeString & FileName, bool Unselect)
{
  TCustomDirView * ADirView = DirView(Side);
  UnicodeString FileNameOnly = ExtractFileName(FileName, !IsSideLocalBrowser(Side));
  TListItem * Item = ADirView->FindFileItem(FileNameOnly);
  // this can happen when local drive is unplugged in the middle of the operation
  if (Item != NULL)
  {
    if (Unselect)
    {
      Item->Selected = false;
    }
    ADirView->MakeProgressVisible(Item);
  }
  return Item;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOperationFinished(
  TFileOperation Operation, TOperationSide Side,
  bool /*Temp*/, const UnicodeString & FileName, bool Success, bool NotCancelled,
  TOnceDoneOperation & OnceDoneOperation)
{
  if (!FAutoOperation)
  {
    // no selection on "/upload", form serves only as event handler
    // (it is not displayed)
    if (PanelOperation(Side, FDragDropOperation) && Visible &&
        ((Operation == FPrimaryOperation) ||
         ((Operation != foCalculateSize) && (Operation != foGetProperties) && (Operation != foCalculateChecksum))))
    {
      TOperationSide DViewSide;
      if ((FProgressSide != osCurrent) &&
          IsLocalBrowserMode() && (Side == osLocal)) // Only to limit the impact
      {
        // assume the operation is over the focused panel
        DViewSide = FProgressSide;
      }
      else
      {
        DViewSide = Side;
      }
      if (FOnFileOperationFinished != NULL)
      {
        FOnFileOperationFinished(DViewSide, FileName, Success, NotCancelled);
      }
      else
      {
        VisualiseOperationFinished(DViewSide, FileName, Success && NotCancelled);
      }
    }

    if (TFileOperationProgressType::IsTransferOperation(Operation))
    {
      if (DebugAlwaysTrue(!IsLocalBrowserMode()))
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
  }

  if (Success && (FSynchronizeController != NULL))
  {
    DebugAssert(!IsSideLocalBrowser(osRemote));
    if (Operation == foCopy)
    {
      DebugAssert(Side == osLocal);
      FSynchronizeController->LogOperation(soUpload, FileName);
    }
    else if (Operation == foDelete)
    {
      DebugAssert(Side == osRemote);
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
  bool Temp, const UnicodeString & FileName, bool Success, bool NotCancelled,
  TOnceDoneOperation & OnceDoneOperation)
{
  TAutoNestingCounter Counter(FDoNotIdleCurrentTerminal);
  DoOperationFinished(Operation, Side, Temp, FileName, Success, NotCancelled, OnceDoneOperation);
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::IsLocalBrowserMode()
{
  return false;
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::SupportsLocalBrowser()
{
  return false;
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::IsSideLocalBrowser(TOperationSide)
{
  return false;
}
//---------------------------------------------------------------------------
TCustomDirView * TCustomScpExplorerForm::GetCurrentLocalBrowser()
{
  return NULL;
}
//---------------------------------------------------------------------------
TCustomDirView * __fastcall TCustomScpExplorerForm::DirView(TOperationSide Side)
{
  DebugAssert(GetSide(Side) == osRemote);
  DebugUsedParam(Side);
  return RemoteDirView;
}
//---------------------------------------------------------------------------
TCustomDriveView * __fastcall TCustomScpExplorerForm::DriveView(TOperationSide Side)
{
  DebugAssert(GetSide(Side) == osRemote);
  DebugUsedParam(Side);
  return RemoteDriveView;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DirViewEnabled(TOperationSide Side)
{
  DebugAssert(GetSide(Side) == osRemote);
  DebugUsedParam(Side);
  return HasActiveTerminal();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableFocusedOperation(
  TOperationSide Side, int FilesOnly)
{
  return DirView(Side)->AnyFileSelected(true, (FilesOnly != 0), true);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::GetEnableSelectedOperation(
  TOperationSide Side, int FilesOnly)
{
  return DirView(Side)->AnyFileSelected(false, (FilesOnly != 0), true);
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
      Item->Caption = MinimizeName(DView->HistoryPath[Data.Index], 50, !IsSideLocalBrowser(Side));
      Item->Hint = DView->HistoryPath[Data.Index];
      DebugAssert(sizeof(int) == sizeof(THistoryItemData));
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
    // workaround
    // In Pascal the size of TTBXPopupMenu is 132, in C++ 136,
    // operator new allocates memory in Pascal code, but calls inline
    // constructor in C++, leading in problems, the function does
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
  TOperationSide Side = (Sender == DirView(osOther) ? osOther : osLocal);
  UpdateHistoryMenu(Side, true);
  UpdateHistoryMenu(Side, false);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CustomCommandRemoteAllowed()
{
  // remote custom commands can be executed only if the server supports shell commands
  // or have secondary shell
  return HasActiveTerminal() && (Terminal->IsCapable[fcSecondaryShell] || Terminal->IsCapable[fcShellAnyCommand]);
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::CustomCommandState(
  const TCustomCommandType & Command, bool OnFocused, TCustomCommandListType ListType)
{
  // -1 = hidden, 0 = disabled, 1 = enabled
  int Result;

  TFileCustomCommand RemoteCustomCommand;
  TLocalCustomCommand LocalCustomCommand;
  TFileCustomCommand * NonInteractiveCustomCommand =
    FLAGCLEAR(Command.Params, ccLocal) ? &RemoteCustomCommand : &LocalCustomCommand;
  TInteractiveCustomCommand InteractiveCustomCommand(NonInteractiveCustomCommand);
  UnicodeString Cmd = InteractiveCustomCommand.Complete(Command.Command, false);

  if (FLAGCLEAR(Command.Params, ccLocal))
  {
    int AllowedState = CustomCommandRemoteAllowed() ? 1 : 0;
    // custom command that does not operate with files can be executed anytime ...
    if (!NonInteractiveCustomCommand->IsFileCommand(Cmd))
    {
      // ... but do not show such command in remote file menu
      if ((ListType == ccltAll) || (ListType == ccltNonFile))
      {
        Result = AllowedState;
      }
      else
      {
        Result = -1;
      }
    }
    else
    {
      if ((ListType == ccltAll) || ((ListType == ccltFile) && !IsSideLocalBrowser(FCurrentSide)))
      {
        Result = (DirView(FCurrentSide)->AnyFileSelected(OnFocused, false, true)) ? AllowedState : 0;
      }
      else
      {
        Result = -1;
      }
    }
  }
  else
  {
    // custom command that does not operate with files can be executed anytime
    if (!NonInteractiveCustomCommand->IsFileCommand(Cmd))
    {
      Result = ((ListType == ccltAll) || (ListType == ccltNonFile)) ? 1 : -1;
    }
    else if (LocalCustomCommand.HasLocalFileName(Cmd))
    {
      if ((ListType == ccltAll) || (ListType == ccltFile))
      {
        // special case is "diff"-style command that can be executed over any side,
        // if we have both sides
        Result =
          // Cannot have focus on both panels, so we have to call AnyFileSelected
          // directly (instead of EnableSelectedOperation) to pass
          // false to FocusedFileOnlyWhenFocused when panel is inactive.
          ((HasDirView[osLocal] && !IsSideLocalBrowser(osRemote) &&
            DirView(osLocal)->AnyFileSelected(false, false, (FCurrentSide == osLocal))) &&
            DirView(osRemote)->AnyFileSelected(false, false, (FCurrentSide == osRemote))) ? 1 : 0;
      }
      else if (ListType == ccltBoth)
      {
        DebugAssert(!IsLocalBrowserMode());
        Result = 1;
      }
      else
      {
        Result = -1;
      }
    }
    else
    {
      if ((ListType == ccltAll) ||
          ((ListType == ccltFile) && (!IsSideLocalBrowser(FCurrentSide) || FLAGCLEAR(Command.Params, ccRemoteFiles))))
      {
        Result = DirView(FCurrentSide)->AnyFileSelected(OnFocused, false, true) ? 1 : 0;
      }
      else
      {
        Result = -1;
      }
    }

    if ((Result > 0) &&
        LocalCustomCommand.IsSessionCommand(Cmd) &&
        !HasActiveTerminal())
    {
      Result = 0;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteCustomCommand(
  TStrings * FileList, const TCustomCommandType & ACommand,
  const TCustomCommandData & Data, const UnicodeString & CommandCommand)
{
  if (EnsureCommandSessionFallback(fcShellAnyCommand))
  {
    TRemoteCustomCommand RemoteCustomCommand(Data, Terminal->CurrentDirectory);
    TWinInteractiveCustomCommand InteractiveCustomCommand(
      &RemoteCustomCommand, ACommand.Name, ACommand.HomePage);

    UnicodeString Command = InteractiveCustomCommand.Complete(CommandCommand, false);

    Configuration->Usage->Inc(L"RemoteCustomCommandRuns2");

    bool Capture =
      FLAGSET(ACommand.Params, ccShowResults) ||
      FLAGSET(ACommand.Params, ccCopyResults) ||
      FLAGSET(ACommand.Params, ccShowResultsInMsgBox);
    TCaptureOutputEvent OutputEvent = NULL;

    DebugAssert(FCapturedLog == NULL);
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
        else if (FLAGSET(ACommand.Params, ccShowResultsInMsgBox))
        {
          MessageDialog(FCapturedLog->Text, qtInformation, qaOK);
        }
      }
    }
    __finally
    {
      SAFE_DESTROY(FCapturedLog);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalCustomCommandPure(
  TStrings * FileList, const TCustomCommandType & ACommand, const UnicodeString & Command, TStrings * ALocalFileList,
  const TCustomCommandData & Data, bool LocalFileCommand, bool FileListCommand, UnicodeString * POutput)
{
  DebugAssert(!IsLocalBrowserMode());
  TStrings * LocalFileList = NULL;
  TStrings * RemoteFileList = NULL;
  TStrings * RemoteFileListFull = NULL;
  try
  {
    if (LocalFileCommand)
    {
      if (ALocalFileList == NULL)
      {
        DebugAssert(HasDirView[osLocal]);
        // Cannot have focus on both panels, so we have to call AnyFileSelected
        // directly (instead of EnableSelectedOperation) to pass
        // false to FocusedFileOnlyWhenFocused
        DebugAssert(DirView(osLocal)->AnyFileSelected(false, false, false));
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

    bool RemoteFiles = FLAGSET(ACommand.Params, ccRemoteFiles);
    if (!RemoteFiles)
    {
      TemporarilyDownloadFiles(FileList, false, RootTempDir, TempDir, false, true, true);
    }

    try
    {
      TDateTimes RemoteFileTimes;

      if (RemoteFiles)
      {
        RemoteFileList = FileList;
      }
      else
      {
        RemoteFileList = new TStringList();

        TMakeLocalFileListParams MakeFileListParam;
        MakeFileListParam.FileList = RemoteFileList;
        MakeFileListParam.FileTimes = &RemoteFileTimes;
        MakeFileListParam.IncludeDirs = FLAGSET(ACommand.Params, ccApplyToDirectories);
        MakeFileListParam.Recursive =
          FLAGSET(ACommand.Params, ccRecursive) && !FileListCommand;

        ProcessLocalDirectory(TempDir, Terminal->MakeLocalFileList, &MakeFileListParam);

        if (!MakeFileListParam.Recursive)
        {
          RemoteFileTimes.clear();

          RemoteFileListFull = new TStringList();

          MakeFileListParam.FileList = RemoteFileListFull;
          MakeFileListParam.Recursive = true;
          MakeFileListParam.IncludeDirs = false;

          ProcessLocalDirectory(TempDir, Terminal->MakeLocalFileList, &MakeFileListParam);
        }
        else
        {
          RemoteFileListFull = RemoteFileList;
        }
      }

      bool NonBlocking = FileListCommand && RemoteFiles && !POutput;

      TFileOperationProgressType Progress(&OperationProgress, &OperationFinished);

      if (!NonBlocking)
      {
        int Count;
        if (FileListCommand)
        {
          Count = 1;
        }
        else if (LocalFileCommand)
        {
          Count = std::max(LocalFileList->Count, RemoteFileList->Count);
        }
        else
        {
          Count = RemoteFileList->Count;
        }
        Progress.Start(foCustomCommand, osRemote, Count);
        DebugAssert(FProgressForm != NULL);
        FProgressForm->ReadOnly = true;
      }

      try
      {
        if (FileListCommand)
        {
          UnicodeString LocalFile;
          // MakeFileList does not delimit filenames
          UnicodeString FileList = MakeFileList(RemoteFileList);

          if (LocalFileCommand)
          {
            DebugAssert(LocalFileList->Count == 1);
            LocalFile = LocalFileList->Strings[0];
          }

          TLocalCustomCommand CustomCommand(Data,
            Terminal->CurrentDirectory, DefaultDownloadTargetDirectory(), L"", LocalFile, FileList);
          UnicodeString ShellCommand = CustomCommand.Complete(Command, true);

          if (NonBlocking)
          {
            DebugAssert(!POutput);
            ExecuteShellChecked(ShellCommand);
          }
          else
          {
            ExecuteProcessCheckedAndWait(ShellCommand, HelpKeyword, POutput);
          }
        }
        else if (LocalFileCommand)
        {
          if (LocalFileList->Count == 1)
          {
            UnicodeString LocalFile = LocalFileList->Strings[0];

            for (int Index = 0; Index < RemoteFileList->Count; Index++)
            {
              UnicodeString FileName = RemoteFileList->Strings[Index];
              UnicodeString FileNameForProgress = RemoteFiles ? FileName : ExtractFileName(FileName);
              Progress.SetFile(FileNameForProgress);
              TLocalCustomCommand CustomCommand(Data,
                Terminal->CurrentDirectory, DefaultDownloadTargetDirectory(), FileName, LocalFile, L"");
              ExecuteProcessCheckedAndWait(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
              TOnceDoneOperation OnceDoneOperation;
              // Do not unselect anything, as it won't work with two panels anyway
              Progress.Finish(UnicodeString(), true, OnceDoneOperation);
            }
          }
          else if (RemoteFileList->Count == 1)
          {
            UnicodeString FileName = RemoteFileList->Strings[0];

            for (int Index = 0; Index < LocalFileList->Count; Index++)
            {
              UnicodeString LocalFileName = LocalFileList->Strings[Index];
              UnicodeString LocalFileNameOnly = ExtractFileName(LocalFileName);
              Progress.SetFile(LocalFileNameOnly);
              TLocalCustomCommand CustomCommand(
                Data, Terminal->CurrentDirectory, DefaultDownloadTargetDirectory(),
                FileName, LocalFileName, L"");
              ExecuteProcessCheckedAndWait(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
              TOnceDoneOperation OnceDoneOperation;
              // Do not unselect anything, as it won't work with two panels anyway
              Progress.Finish(UnicodeString(), true, OnceDoneOperation);
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
              UnicodeString LocalFileName = LocalFileList->Strings[Index];
              UnicodeString LocalFileNameOnly = ExtractFileName(LocalFileName);
              Progress.SetFile(LocalFileNameOnly); // sic
              TLocalCustomCommand CustomCommand(
                Data, Terminal->CurrentDirectory, DefaultDownloadTargetDirectory(),
                FileName, LocalFileName, L"");
              ExecuteProcessCheckedAndWait(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
              TOnceDoneOperation OnceDoneOperation;
              // Do not unselect anything, as it won't work with two panels anyway
              Progress.Finish(UnicodeString(), true, OnceDoneOperation);
            }
          }
        }
        else
        {
          for (int Index = 0; Index < RemoteFileList->Count; Index++)
          {
            UnicodeString FileName = RemoteFileList->Strings[Index];
            UnicodeString FileNameForProgress = RemoteFiles ? FileName : ExtractFileName(FileName);
            Progress.SetFile(FileNameForProgress);
            TLocalCustomCommand CustomCommand(Data,
              Terminal->CurrentDirectory, DefaultDownloadTargetDirectory(),
              FileName, L"", L"");
            ExecuteProcessCheckedAndWait(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
            TOnceDoneOperation OnceDoneOperation;
            Progress.Finish(FileNameForProgress, true, OnceDoneOperation);
          }
        }
      }
      __finally
      {
        if (!NonBlocking)
        {
          Progress.Stop();
        }
      }

      DebugAssert(!FAutoOperation);

      if (!RemoteFiles)
      {
        TempDir = IncludeTrailingBackslash(TempDir);
        for (int Index = 0; Index < RemoteFileListFull->Count; Index++)
        {
          UnicodeString FileName = RemoteFileListFull->Strings[Index];
          if (DebugAlwaysTrue(SameText(TempDir, FileName.SubString(1, TempDir.Length()))) &&
              // Skip directories, process only nested files.
              // The check is redundant as FileAge fails for directories anyway.
              !DirectoryExists(FileName))
          {
            UnicodeString RemoteDir =
              UnixExtractFileDir(
                UnixIncludeTrailingBackslash(Terminal->CurrentDirectory) +
                ToUnixPath(FileName.SubString(TempDir.Length() + 1, FileName.Length() - TempDir.Length())));

            TSearchRecSmart SearchRec;
            if (FileSearchRec(FileName, SearchRec) &&
                (SearchRec.GetLastWriteTime() != RemoteFileTimes[Index]))
            {
              TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
              TemporaryFileCopyParam(CopyParam);
              CopyParam.FileMask = L"";

              FAutoOperation = true;
              FMoveToQueue = false;
              std::unique_ptr<TStrings> TemporaryFilesList(new TStringList());
              TemporaryFilesList->Add(FileName);

              Terminal->CopyToRemote(TemporaryFilesList.get(), RemoteDir, &CopyParam, cpTemporary, NULL);
            }
          }
        }
      }
    }
    __finally
    {
      FAutoOperation = false;
      if (!RootTempDir.IsEmpty() && DebugAlwaysTrue(!RemoteFiles))
      {
        RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir));
      }
    }
  }
  __finally
  {
    if (RemoteFileList != FileList)
    {
      delete RemoteFileList;
    }
    if (LocalFileList != ALocalFileList)
    {
      delete LocalFileList;
    }
    if (RemoteFileListFull != RemoteFileList)
    {
      delete RemoteFileListFull;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalCustomCommandWithLocalFiles(
  const TCustomCommandType & ACommand, const UnicodeString & Command, const TCustomCommandData & Data,
  bool FileListCommand, UnicodeString * POutput)
{
  std::unique_ptr<TStrings> SelectedFileList(DirView(osCurrent)->CreateFileList(false, true, NULL));

  std::unique_ptr<TStrings> LocalFileList(new TStringList());

  for (int Index = 0; Index < SelectedFileList->Count; Index++)
  {
    UnicodeString FileName = SelectedFileList->Strings[Index];
    if (DirectoryExists(FileName))
    {
      if (FLAGSET(ACommand.Params, ccApplyToDirectories))
      {
        LocalFileList->Add(FileName);
      }

      if (FLAGSET(ACommand.Params, ccRecursive))
      {
        TMakeLocalFileListParams MakeFileListParam;
        MakeFileListParam.FileList = LocalFileList.get();
        MakeFileListParam.FileTimes = NULL;
        MakeFileListParam.IncludeDirs = FLAGSET(ACommand.Params, ccApplyToDirectories);
        MakeFileListParam.Recursive = true;

        ProcessLocalDirectory(FileName, Terminal->MakeLocalFileList, &MakeFileListParam);
      }
    }
    else
    {
      LocalFileList->Add(FileName);
    }
  }

  UnicodeString RemotePath;
  if (Terminal != NULL)
  {
    RemotePath = Terminal->CurrentDirectory;
  }

  if (FileListCommand)
  {
    UnicodeString FileList = MakeFileList(LocalFileList.get());
    TLocalCustomCommand CustomCommand(
      Data, RemotePath, DefaultDownloadTargetDirectory(),
      L"", L"", FileList);
    ExecuteProcessChecked(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
  }
  else
  {
    TFileOperationProgressType Progress(&OperationProgress, &OperationFinished);

    Progress.Start(foCustomCommand, osLocal, LocalFileList->Count);
    DebugAssert(FProgressForm != NULL);
    FProgressForm->ReadOnly = true;

    try
    {
      for (int Index = 0; Index < LocalFileList->Count; Index++)
      {
        UnicodeString FileName = LocalFileList->Strings[Index];
        Progress.SetFile(FileName);
        TLocalCustomCommand CustomCommand(
          Data, RemotePath, DefaultDownloadTargetDirectory(),
          FileName, L"", L"");
        ExecuteProcessCheckedAndWait(CustomCommand.Complete(Command, true), HelpKeyword, POutput);
        TOnceDoneOperation OnceDoneOperation;
        Progress.Finish(FileName, true, OnceDoneOperation);
      }
    }
    __finally
    {
      Progress.Stop();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalCustomCommand(TStrings * FileList,
  const TCustomCommandType & ACommand, TStrings * ALocalFileList,
  const TCustomCommandData & Data, const UnicodeString & CommandCommand)
{
  UnicodeString RemotePath = (Terminal != NULL) ? Terminal->CurrentDirectory : UnicodeString();
  TLocalCustomCommand LocalCustomCommand(Data, RemotePath, DefaultDownloadTargetDirectory());
  TWinInteractiveCustomCommand InteractiveCustomCommand(
    &LocalCustomCommand, ACommand.Name, ACommand.HomePage);

  UnicodeString Command = InteractiveCustomCommand.Complete(CommandCommand, false);

  bool FileListCommand = LocalCustomCommand.IsFileListCommand(Command);
  bool LocalFileCommand = LocalCustomCommand.HasLocalFileName(Command);

  Configuration->Usage->Inc(L"LocalCustomCommandRuns2");

  std::unique_ptr<UnicodeString> POutput;
  if (FLAGSET(ACommand.Params, ccShowResultsInMsgBox) ||
      FLAGSET(ACommand.Params, ccCopyResults))
  {
    POutput.reset(new UnicodeString());
  }

  TValueRestorer<TOperationSide> ProgressSideRestorer(FProgressSide, FCurrentSide);

  if (!LocalCustomCommand.IsFileCommand(Command))
  {
    ExecuteProcessChecked(LocalCustomCommand.Complete(Command, true), HelpKeyword, POutput.get());
  }
  // remote files?
  else if (!IsSideLocalBrowser(FCurrentSide) || LocalFileCommand)
  {
    LocalCustomCommandPure(FileList, ACommand, Command, ALocalFileList, Data, LocalFileCommand, FileListCommand, POutput.get());
  }
  // local files
  else
  {
    LocalCustomCommandWithLocalFiles(ACommand, Command, Data, FileListCommand, POutput.get());
  }

  if (POutput.get() != NULL)
  {
    // If the output is single-line, we do not want the trailing CRLF, as the CopyToClipboard(TStrings *) does
    int P = POutput->Pos(sLineBreak);
    if (P == POutput->Length() - static_cast<int>(strlen(sLineBreak)) + 1)
    {
      POutput->SetLength(P - 1);
    }
    // Copy even empty output to clipboard
    if (FLAGSET(ACommand.Params, ccCopyResults))
    {
      CopyToClipboard(*POutput);
    }
    // But do not show an empty message box.
    // This way the ShowResultsInMsgBox can be used to suppress a console window of commands with no output
    if (FLAGSET(ACommand.Params, ccShowResultsInMsgBox) &&
        !POutput->IsEmpty())
    {
      TClipboardHandler ClipboardHandler;
      ClipboardHandler.Text = *POutput;

      TMessageParams Params;
      TQueryButtonAlias Aliases[1];
      Aliases[0].Button = qaRetry;
      Aliases[0].Alias = LoadStr(EDIT_COPY);
      Aliases[0].OnSubmit = &ClipboardHandler.Copy;
      Params.Aliases = Aliases;
      Params.AliasesCount = LENOF(Aliases);

      MessageDialog(*POutput, qtInformation, qaOK | qaRetry, HelpKeyword, &Params);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomCommand(TStrings * FileList,
  const TCustomCommandType & ACommand, TStrings * ALocalFileList)
{

  TCustomCommandData Data;
  UnicodeString Site;
  UnicodeString RemotePath;
  if (HasActiveTerminal())
  {
    std::unique_ptr<TSessionData> SessionData(SessionDataForCode());
    Data = TCustomCommandData(SessionData.get());
    Site = Terminal->SessionData->SessionKey;
    RemotePath = Terminal->CurrentDirectory;
  }
  UnicodeString HelpKeyword = ACommand.HomePage;

  std::unique_ptr<TStrings> CustomCommandOptions(CloneStrings(WinConfiguration->CustomCommandOptions));
  if (ACommand.AnyOptionWithFlag(TCustomCommandType::ofRun))
  {
    std::unique_ptr<TCustomCommand> CustomCommandForOptions;
    if (FLAGCLEAR(ACommand.Params, ccLocal))
    {
      CustomCommandForOptions.reset(new TRemoteCustomCommand(Data, RemotePath));
    }
    else
    {
      CustomCommandForOptions.reset(new TLocalCustomCommand(Data, RemotePath, DefaultDownloadTargetDirectory()));
    }

    if (!DoCustomCommandOptionsDialog(
           &ACommand, CustomCommandOptions.get(), NULL, TCustomCommandType::ofRun, CustomCommandForOptions.get(), Site, NULL))
    {
      Abort();
    }
  }

  UnicodeString CommandCommand = ACommand.GetCommandWithExpandedOptions(CustomCommandOptions.get(), Site);

  if (FLAGCLEAR(ACommand.Params, ccLocal))
  {
    RemoteCustomCommand(FileList, ACommand, Data, CommandCommand);
  }
  else
  {
    LocalCustomCommand(FileList, ACommand, ALocalFileList, Data, CommandCommand);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BothCustomCommand(
  const TCustomCommandType & Command)
{
  DebugAssert(FCustomCommandLocalFileList != NULL);
  DebugAssert(FCustomCommandRemoteFileList != NULL);
  DebugAssert(FCustomCommandLocalFileList->Count == FCustomCommandRemoteFileList->Count);

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
  TAction * Action, TStrings * LocalFileList, TStrings * RemoteFileList)
{
  delete FCustomCommandLocalFileList;
  delete FCustomCommandRemoteFileList;
  // takeover ownership,
  // the lists must survive the MenuPopup as OnClick occurs only after it exits
  FCustomCommandLocalFileList = LocalFileList;
  FCustomCommandRemoteFileList = RemoteFileList;

  TButton * Button = dynamic_cast<TButton *>(Action->ActionComponent);
  if (Button != NULL)
  {
    FCustomCommandMenu->Items->Clear();

    NonVisualDataModule->CreateCustomCommandsMenu(FCustomCommandMenu->Items, false, false, ccltBoth, NULL);
    MenuPopup(FCustomCommandMenu, Button);
  }
  else
  {
    NonVisualDataModule->CreateCustomCommandsMenu(Action, false, ccltBoth);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalCaptureLog(
  const UnicodeString & AddedLine, TCaptureOutputType OutputType)
{
  DebugAssert(FCapturedLog != NULL);
  if ((OutputType == cotOutput) || (OutputType == cotError))
  {
    FCapturedLog->Add(AddedLine);
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::IsFileControl(TObject * Control,
  TOperationSide Side)
{
  return
    (Side == osOther) &&
    ((Control == DirView(osOther)) || (Control == DriveView(osOther)));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewContextPopupDefaultItem(
  TOperationSide Side, TTBXCustomItem * Item,
  TResolvedDoubleClickAction DoubleClickAction1, TResolvedDoubleClickAction DoubleClickAction2)
{
  TCustomDirView * DView = DirView(Side);

  bool IsDefault = false;
  if (DView->ItemFocused != NULL)
  {
    TTerminal * ATerminal = !IsSideLocalBrowser(Side) ? Terminal : NULL;

    TResolvedDoubleClickAction DefaultAction =
      WinConfiguration->ResolveDoubleClickAction(DView->ItemIsDirectory(DView->ItemFocused), ATerminal);
    IsDefault = (DefaultAction == DoubleClickAction1) || (DefaultAction == DoubleClickAction2);
  }

  TTBItemOptions O = Item->Options;
  if (IsDefault)
  {
    Item->Options = O << tboDefault;
  }
  else
  {
    Item->Options = O >> tboDefault;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewContextPopup(
  TOperationSide Side, Byte PopupComponent, const TPoint &MousePos)
{
  TCustomDirView * DView = DirView(Side);
  TListItem * Item = DView->ItemFocused;
  if ((DView->GetItemAt(MousePos.x, MousePos.y) == Item) &&
      EnableFocusedOperation[Side])
  {
    TPoint ClientPoint;
    ClientPoint = ((MousePos.x < 0) && (MousePos.y < 0)) ?
      TPoint(0, 0) : MousePos;
    FLastContextPopupScreenPoint = DView->ClientToScreen(ClientPoint);

    reinterpret_cast<TPopupMenu*>(GetComponent(PopupComponent))->Popup(
      FLastContextPopupScreenPoint.x, FLastContextPopupScreenPoint.y);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewContextPopup(
      TObject * /*Sender*/, const TPoint &MousePos, bool &Handled)
{
  DirViewContextPopupDefaultItem(osRemote, NonVisualDataModule->RemoteOpenMenuItem, rdcaChangeDir, rdcaOpen);
  DirViewContextPopupDefaultItem(osRemote, NonVisualDataModule->RemoteEditMenuItem, rdcaEdit, rdcaNone);
  DirViewContextPopupDefaultItem(osRemote, NonVisualDataModule->RemoteCopyMenuItem, rdcaCopy, rdcaNone);

  DirViewContextPopup(osRemote, fcRemotePopup, MousePos);
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReloadLocalDirectory(const UnicodeString Directory)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchStart(void *& /*Storage*/)
{
  DebugAssert(FErrorList == NULL);
  if (WinConfiguration->ContinueOnError)
  {
    FErrorList = new TStringList();
    Configuration->Usage->Inc(L"ContinuationsOnError");
  }
  NonVisualDataModule->StartBusy();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BatchEnd(void * /*Storage*/)
{
  NonVisualDataModule->EndBusy();
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
bool __fastcall TCustomScpExplorerForm::ExecuteCopyMoveFileOperation(
  TFileOperation Operation, TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * AParam)
{
  DebugAssert(!IsLocalBrowserMode());
  TTransferDirection Direction = (Side == osLocal ? tdToRemote : tdToLocal);
  TTransferType Type = (Operation == foCopy ? ttCopy : ttMove);
  TTransferOperationParam DefaultParam;
  TTransferOperationParam & Param = (AParam != NULL) ? *static_cast<TTransferOperationParam *>(AParam) : DefaultParam;
  TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  switch (Param.Queue)
  {
    case asOn:
      CopyParam.Queue = true;
      break;

    case asOff:
      CopyParam.Queue = false;
      break;

    case asAuto:
    default:
      // keep default
      break;
  }
  bool Result =
    CopyParamDialog(Direction, Type, Param.Temp, FileList, Param.TargetDirectory, CopyParam, !NoConfirmation, Param.DragDrop, Param.Options);
  if (Result)
  {
    DebugAssert(Terminal);
    bool SelectionRestored = false;
    TCustomDirView * DView = NULL;
    if (HasDirView[Side])
    {
      DView = DirView(Side);
      DView->SaveSelection();
      DView->SaveSelectedNames();
    }

    UpdateCopyParamCounters(CopyParam);

    std::unique_ptr<TStringList> TransferResumeList(new TStringList());
    DebugAssert(FTransferResumeList == NULL);
    FTransferResumeList = Terminal->IsCapable[fcMoveToQueue] ? TransferResumeList.get() : NULL;
    FMoveToQueue = false;

    int Params = FLAGMASK(Operation == foMove, cpDelete);

    try
    {
      TStrings * PermanentFileList;
      std::unique_ptr<TStrings> PermanentFileListOwner;

      try
      {
        if (Side == osLocal)
        {
          PermanentFileList = FileList;

          Params |= FLAGMASK(Param.Temp, cpTemporary);

          CopyParam.IncludeFileMask.SetRoots(FileList, Param.TargetDirectory);

          Terminal->CopyToRemote(FileList, Param.TargetDirectory, &CopyParam, Params, NULL);
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
          // Clone the file list as it may refer to current directory files,
          // which get destroyed, when the source directory is reloaded after foMove operation.
          // We should actually clone the file list for whole ExecuteFileOperation to protect against reloads.
          // But for a hotfix, we are not going to do such a big change.
          PermanentFileListOwner.reset(TRemoteFileList::CloneStrings(FileList));
          PermanentFileList = PermanentFileListOwner.get();

          try
          {
            CopyParam.IncludeFileMask.SetRoots(Param.TargetDirectory, FileList);

            Terminal->CopyToLocal(FileList, Param.TargetDirectory, &CopyParam, Params, NULL);
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
            ReloadLocalDirectory(Param.TargetDirectory);
          }
        }

        // When the transfer batch is aborted between individual file transfers, the Abort is not called.
        if (FMoveToQueue)
        {
          Abort();
        }
      }
      catch (EAbort &)
      {
        if (FMoveToQueue)
        {
          Params |= (CopyParam.QueueNoConfirmation ? cpNoConfirmation : 0);

          DebugAssert(CopyParam.TransferSkipList == NULL);
          DebugAssert(CopyParam.TransferResumeFile.IsEmpty());
          if (TransferResumeList->Count > 0)
          {
            CopyParam.TransferResumeFile = TransferResumeList->Strings[TransferResumeList->Count - 1];
            TransferResumeList->Delete(TransferResumeList->Count - 1);
          }

          CopyParam.TransferSkipList = TransferResumeList.release();

          // not really needed, just to keep it consistent with TransferResumeList
          FTransferResumeList = NULL;
          FMoveToQueue = false;

          Configuration->Usage->Inc("MovesToBackground");

          AddQueueItem(Queue, Direction, PermanentFileList, Param.TargetDirectory, CopyParam, Params);
          ClearTransferSourceSelection(Direction);
        }

        throw;
      }
    }
    __finally
    {
      if (!SelectionRestored && (DView != NULL))
      {
        DView->DiscardSavedSelection();
      }
      FTransferResumeList = NULL;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::ExecuteDeleteFileOperation(
  TOperationSide Side, TStrings * FileList, void * Param)
{
  DebugAssert(FileList->Count);
  // We deliberately do not toggle alternative flag (Param), but use OR,
  // because the Param is set only when command is invoked using Shift-Del/F8 keyboard
  // shortcut of CurrentDeleteAlternativeAction
  bool Alternative =
    bool(Param) || UseAlternativeFunction();
  bool Recycle;
  if (IsSideLocalBrowser(Side))
  {
    Recycle = (WinConfiguration->DeleteToRecycleBin != Alternative);
  }
  else
  {
    Recycle =
      (Terminal->SessionData->DeleteToRecycleBin != Alternative) &&
      !Terminal->SessionData->RecycleBinPath.IsEmpty() &&
      !Terminal->IsRecycledFile(FileList->Strings[0]);
  }

  bool Result = !(Recycle ? WinConfiguration->ConfirmRecycling : WinConfiguration->ConfirmDeleting);
  if (!Result)
  {
    UnicodeString Query;
    if (FileList->Count == 1)
    {
      if (IsSideLocalBrowser(Side))
      {
        Query = ExtractFileName(FileList->Strings[0]);
      }
      else
      {
        Query = UnixExtractFileName(FileList->Strings[0]);
      }
      Query = FMTLOAD((Recycle ? CONFIRM_RECYCLE_FILE : CONFIRM_DELETE_FILE), (Query));
    }
    else
    {
      Query = FMTLOAD((Recycle ? CONFIRM_RECYCLE_FILES : CONFIRM_DELETE_FILES), (FileList->Count));
    }

    TMessageParams Params(mpNeverAskAgainCheck);
    Params.ImageName = L"Delete file";
    unsigned int Answer =
      MessageDialog(MainInstructions(Query), qtConfirmation, qaOK | qaCancel, HELP_DELETE_FILE, &Params);
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
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, TStrings * FileList, bool NoConfirmation, void * Param)
{
  TAutoBatch AutoBatch(this);
  bool Result;
  if (TFileOperationProgressType::IsTransferOperation(Operation))
  {
    Result = ExecuteCopyMoveFileOperation(Operation, Side, FileList, NoConfirmation, Param);
  }
  else if (Operation == foRename)
  {
    DebugAssert(DirView(Side)->ItemFocused);
    DirView(Side)->ItemFocused->EditCaption();
    Result = true;
  }
  else if (Operation == foDelete)
  {
    Result = ExecuteDeleteFileOperation(Side, FileList, Param);
  }
  else if (Operation == foSetProperties)
  {
    DirView(osRemote)->SaveSelectedNames();
    Result = SetProperties(Side, FileList);
  }
  else if (Operation == foCustomCommand)
  {
    DebugAssert(Param);
    DebugAssert(Side == osRemote); // Side carries no meaning for foCustomCommand

    DirView(osRemote)->SaveSelectedNames();
    const TCustomCommandType * Command = static_cast<const TCustomCommandType*>(Param);
    CustomCommand(FileList, *Command, NULL);
    Result = true;
  }
  else if ((Operation == foRemoteMove) || (Operation == foRemoteCopy))
  {
    DebugAssert(!IsSideLocalBrowser(Side));
    Result = RemoteTransferFiles(FileList, NoConfirmation,
      (Operation == foRemoteMove), reinterpret_cast<TManagedTerminal *>(Param));
  }
  else if (Operation == foLock)
  {
    DebugAssert(!IsSideLocalBrowser(Side));
    LockFiles(FileList, true);
    Result = true;
  }
  else if (Operation == foUnlock)
  {
    DebugAssert(!IsSideLocalBrowser(Side));
    LockFiles(FileList, false);
    Result = true;
  }
  else
  {
    DebugFail();
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
TOperationSide TCustomScpExplorerForm::GetOtherSide(TOperationSide Side)
{
  TOperationSide Result;
  switch (GetSide(Side))
  {
    case osLocal:
      Result = osOther;
      break;
    case osOther:
      Result = osLocal;
      break;
    default:
      DebugFail();
      Abort();
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::ExecuteFileOperation(TFileOperation Operation,
  TOperationSide Side, bool OnFocused, bool NoConfirmation, void * Param)
{
  Side = GetSide(Side);
  bool FullPath = IsSideLocalBrowser(Side);
  TCustomDirView * ADirView = DirView(Side);
  std::unique_ptr<TStringList> FileList(new TStringList());
  ADirView->CreateFileList(OnFocused, FullPath, FileList.get());

  bool ReloadProperties =
    !IsSideLocalBrowser(Side) && (Operation == foSetProperties) &&
    DebugAlwaysTrue(HasActiveTerminal()) && Terminal->IsCapable[fcLoadingAdditionalProperties];

  bool Result = ExecuteFileOperation(Operation, Side, FileList.get(), NoConfirmation, Param);

  if (Result && ReloadProperties)
  {
    std::unique_ptr<TStringList> FileList2(new TStringList());

    FileList->CaseSensitive = true;
    FileList->Sort();
    for (int Index = 0; Index < ADirView->Items->Count; Index++)
    {
      TListItem * Item = ADirView->Items->Item[Index];
      UnicodeString FileName = ADirView->ItemFileName(Item);
      int Index2;
      if (FileList->Find(FileName, Index2))
      {
        FileList2->AddObject(FileName, ADirView->ItemData(Item));
      }
    }

    LoadFilesProperties(FileList2.get());
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
    if (TFileOperationProgressType::IsTransferOperation(Operation))
    {
      DebugAssert(!IsLocalBrowserMode());
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
void __fastcall TCustomScpExplorerForm::ExecuteCopyOperationCommand(
  TOperationSide Side, bool OnFocused, unsigned int Flags)
{
  TTransferOperationParam Param;
  if ((WinConfiguration->Interface != ifCommander) ||
      WinConfiguration->ScpCommander.ExplorerKeyboardShortcuts)
  {
    Flags &= ~cocShortCutHint;
  }
  TCustomDirView * DView = DirView(Side);
  Param.Options =
    FLAGMASK(FLAGSET(Flags, cocShortCutHint), coShortCutHint) |
    FLAGMASK(SelectedAllFilesInDirView(DView), coAllFiles);
  if (FLAGSET(Flags, cocQueue))
  {
    Param.Queue = asOn;
  }
  else if (FLAGSET(Flags, cocNonQueue))
  {
    Param.Queue = asOff;
  }
  ExecuteFileOperationCommand(foCopy, Side, OnFocused, false, &Param);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::HandleErrorList(TStringList *& ErrorList)
{
  int Count = ErrorList->Count;
  try
  {
    if (ErrorList->Count)
    {
      UnicodeString Message = MainInstructions(FMTLOAD(ERROR_LIST_COUNT, (ErrorList->Count)));
      if (MessageDialog(Message, qtError,
          qaOK | qaCancel, HELP_NONE) == qaOK)
      {
        unsigned int Answer;
        int Index = 0;
        do
        {
          DebugAssert(Index >= 0 && Index < Count);
          TQueryButtonAlias Aliases[2];
          Aliases[0].Button = qaYes;
          Aliases[0].Alias = LoadStr(PREV_BUTTON);
          Aliases[1].Button = qaNo;
          Aliases[1].Alias = LoadStr(NEXT_BUTTON);
          TMessageParams Params;
          Params.Aliases = Aliases;
          Params.AliasesCount = LENOF(Aliases);

          int No = Index + 1;

          UnicodeString Message = ErrorList->Strings[Index];
          UnicodeString AMainInstructions;
          UnicodeString Details;
          if (ExtractMainInstructions(Message, AMainInstructions))
          {
            Details = Message;
            Message = AMainInstructions;
          }
          Message = MainInstructions(FMTLOAD(ERROR_LIST_NUMBER, (No, Count, Message))) + Details;
          Answer = MoreMessageDialog(
            Message,
            dynamic_cast<TStrings *>(ErrorList->Objects[Index]), qtError,
            (Index ? qaYes : 0) | (Index < Count - 1 ? qaNo : 0) |
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
    for (int i = 0; i < Count; i++)
    {
      delete List->Objects[i];
    }
    delete List;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteRemoteFile(
  const UnicodeString & FullFileName, TRemoteFile * File, TExecuteFileBy ExecuteFileBy)
{
  // needed for checking filemasks, as there's no directory object
  // associated with the file object
  File->FullFileName = FullFileName;

  TFileMasks::TParams MaskParams;
  MaskParams.Size = File->Size;
  MaskParams.Modification = File->Modification;

  ExecuteFile(osRemote, ExecuteFileBy, NULL, FullFileName, File, MaskParams);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::EditNew(TOperationSide Side)
{
  DebugAssert(!WinConfiguration->DisableOpenEdit);

  Side = GetSide(Side);

  TCustomDirView * DView = DirView(Side);
  TListItem * FocusedItem = DView->ItemFocused;
  UnicodeString Name;
  if ((FocusedItem != NULL) && !DView->ItemIsDirectory(FocusedItem))
  {
    Name = DView->ItemFileName(FocusedItem);
  }
  else
  {
    Name = LoadStr(NEW_FILE);
  }
  UnicodeString Names = Name;
  std::unique_ptr<TStrings> History(CloneStrings(CustomWinConfiguration->History[L"EditFile"]));
  if (InputDialog(LoadStr(EDIT_FILE_CAPTION), LoadStr(EDIT_FILE_PROMPT), Names,
        HELP_EDIT_NEW, History.get(), true, NULL, true, 444))
  {
    while (!Names.IsEmpty())
    {
      Name = CutToChar(Names, FileMasksDelimiters[1], false);
      CustomWinConfiguration->History[L"EditFile"] = History.get();
      UnicodeString TargetFileName;
      UnicodeString LocalFileName;
      UnicodeString RootTempDir;
      UnicodeString TempDir;
      UnicodeString RemoteDirectory;
      bool ExistingFile = false;
      if (!IsSideLocalBrowser(Side))
      {
        Name = AbsolutePath(Terminal->CurrentDirectory, Name);

        std::unique_ptr<TRemoteFile> File(Terminal->TryReadFile(Name));
        if ((File.get() != NULL) &&
            !File->IsDirectory)
        {
          ExecuteRemoteFile(Name, File.get(), efDefaultEditor);
          ExistingFile = true;
        }
        File.reset(NULL);

        if (!ExistingFile)
        {
          RemoteDirectory = UnixExtractFilePath(Name);
          TemporaryDirectoryForRemoteFiles(
            RemoteDirectory, GUIConfiguration->CurrentCopyParam, false, TempDir, RootTempDir);

          TargetFileName = UnixExtractFileName(Name);
          TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
          LocalFileName = TempDir +
            // We probably do not want to trim the VMS version here
            Terminal->ChangeFileName(&CopyParam, TargetFileName, osRemote, false);
        }
      }
      else
      {
        if (ExtractFilePath(Name).IsEmpty())
        {
          LocalFileName = IncludeTrailingBackslash(DView->PathName) + Name;
        }
        else
        {
          LocalFileName = ExpandFileName(Name);
        }

        TargetFileName = ExtractFileName(Name);
      }

      if (!ExistingFile)
      {
        bool NewFile = !FileExists(ApiPath(LocalFileName));
        if (NewFile)
        {
          int File = FileCreate(ApiPath(LocalFileName));
          if (File < 0)
          {
            if (!RootTempDir.IsEmpty())
            {
              RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir));
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
          ExternalEditor, RootTempDir, RemoteDirectory, NewFile, TDateTime());
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteExecuteForceText(
  TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor)
{
  DebugAssert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));
  DebugAssert(ExecuteFileBy != efDefaultEditor);

  return
    ((ExecuteFileBy == efInternalEditor)) ||
    ((ExecuteFileBy == efExternalEditor) && ExternalEditor->ExternalEditorText);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CustomExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, UnicodeString FileName, UnicodeString OriginalFileName,
  const TEditorData * ExternalEditor, UnicodeString LocalRootDirectory,
  UnicodeString RemoteDirectory, bool NewFile, const TDateTime & SourceTimestamp)
{
  DebugAssert(!WinConfiguration->DisableOpenEdit);
  DebugAssert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));
  DebugAssert(ExecuteFileBy != efDefaultEditor);

  Side = GetSide(Side);

  std::unique_ptr<TEditedFileData> Data(new TEditedFileData);
  if (!IsSideLocalBrowser(Side))
  {
    Data->Terminal = Terminal;
    Data->SessionData = CloneCurrentSessionData();
    Data->ForceText = RemoteExecuteForceText(ExecuteFileBy, ExternalEditor);
    Data->RemoteDirectory = RemoteDirectory;
    Data->SessionName = Terminal->SessionData->SessionName;
    Data->LocalRootDirectory = LocalRootDirectory;
    Data->OriginalFileName = OriginalFileName;
    Data->Command = L""; // will be changed later for external editor
    // Empty SourceTimestamp indicates that checking was disabled when the file was opened
    if (WinConfiguration->EditorCheckNotModified)
    {
      Data->SourceTimestamp = SourceTimestamp;
    }
  }

  if (ExecuteFileBy == efInternalEditor)
  {
    if (!IsSideLocalBrowser(Side))
    {
      UnicodeString RemotePath = UnixCombinePaths(RemoteDirectory, OriginalFileName);
      UnicodeString Caption = RemotePath + TitleSeparator + Terminal->SessionData->SessionName;
      TForm * Editor;
      try
      {
        Editor =
          ShowEditorForm(
            FileName, this,
            FEditorManager->FileChanged, FEditorManager->FileReload, FEditorManager->FileClosed,
            SaveAllInternalEditors, AnyInternalEditorModified,
            Caption, StandaloneOperation, SessionColor, Terminal->SessionData->InternalEditorEncoding, NewFile);
      }
      catch(...)
      {
        if (!LocalRootDirectory.IsEmpty())
        {
          RecursiveDeleteFile(ExcludeTrailingBackslash(LocalRootDirectory));
        }
        throw;
      }

      FEditorManager->AddFileInternal(FileName, Data.release(), Editor);
      AppLogFmt(L"Opened internal editor with \"%s\"", (FileName));
    }
    else
    {
      DebugAssert(!StandaloneOperation);
      TForm * Editor =
        ShowEditorForm(FileName, this, NULL, NULL, LocalEditorClosed,
          SaveAllInternalEditors, AnyInternalEditorModified,
          L"", false, SessionColor, -1, NewFile);
      FLocalEditors->Add(Editor);
    }
  }
  else
  {
    HANDLE Process;

    if (ExecuteFileBy == efExternalEditor)
    {
      UnicodeString Program, Params, Dir;
      Data->Command = ExternalEditor->ExternalEditor;
      ReformatFileNameCommand(Data->Command);
      SplitCommand(Data->Command, Program, Params, Dir);
      Params = ExpandFileNameCommand(Params, FileName);
      Program = ExpandEnvironmentVariables(Program);
      if (!ExecuteShell(Program, Params, Process))
      {
        throw EOSExtException(FMTLOAD(EDITOR_ERROR, (Program)));
      }
    }
    else
    {
      DebugAssert(!IsSideLocalBrowser(Side));
      if (!ExecuteShell(FileName, L"", Process))
      {
        throw EOSExtException(FMTLOAD(EXECUTE_FILE_ERROR, (FileName)));
      }
    }

    if (IsSideLocalBrowser(Side) ||
        ((ExecuteFileBy == efShell) &&
         !WinConfiguration->Editor.SDIShellEditor) ||
        ((ExecuteFileBy == efExternalEditor) &&
         !ExternalEditor->SDIExternalEditor))
    {
      // no need for handle
      if (Process != NULL)
      {
        DebugCheck(CloseHandle(Process));
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

    if (!IsSideLocalBrowser(Side))
    {
      FEditorManager->AddFileExternal(FileName, Data.release(), Process);
      AppLogFmt(L"Opened external editor with remote file \"%s\".", (FileName));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveInternalEditor(
  const UnicodeString /*FileName*/, TEditedFileData * /*Data*/, TObject * Token,
  void * /*Arg*/)
{
  if (Token != NULL)
  {
    EditorFormFileSave(static_cast<TForm *>(Token));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveAllInternalEditors(TObject * /*Sender*/)
{
  for (int Index = 0; Index < FLocalEditors->Count; Index++)
  {
    EditorFormFileSave(static_cast<TForm *>(FLocalEditors->Items[Index]));
  }

  FEditorManager->ProcessFiles(SaveInternalEditor, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InternalEditorModified(
  const UnicodeString /*FileName*/, TEditedFileData * /*Data*/, TObject * Token,
  void * Arg)
{
  if ((Token != NULL) &&
      IsEditorFormModified(static_cast<TForm *>(Token)))
  {
    *static_cast<bool *>(Arg) = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AnyInternalEditorModified(
  TObject * /*Sender*/, bool & Modified)
{
  for (int Index = 0; !Modified && (Index < FLocalEditors->Count); Index++)
  {
    if (IsEditorFormModified(static_cast<TForm *>(FLocalEditors->Items[Index])))
    {
      Modified = true;
    }
  }

  if (!Modified)
  {
    FEditorManager->ProcessFiles(InternalEditorModified, &Modified);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalEditorClosed(TObject * Sender, bool /*Forced*/)
{
  DebugCheck(FLocalEditors->Extract(Sender) >= 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporaryDirectoryForRemoteFiles(
  const UnicodeString & RemoteDirectory, const TCopyParamType & CopyParam, bool Simple,
  UnicodeString & Result, UnicodeString & RootDirectory)
{
  if (!WinConfiguration->TemporaryDirectoryDeterministic || Simple)
  {
    RootDirectory = IncludeTrailingBackslash(WinConfiguration->TemporaryDir());
    Result = RootDirectory;
  }
  else
  {
    RootDirectory = L"";
    Result = WinConfiguration->ExpandedTemporaryDirectory();
    Result = IncludeTrailingBackslash(Result);
  }

  if (WinConfiguration->TemporaryDirectoryAppendSession && !Simple)
  {
    Result = IncludeTrailingBackslash(Result + MakeValidFileName(Terminal->SessionData->SessionName));
  }

  if (WinConfiguration->TemporaryDirectoryAppendPath && !Simple)
  {
    UnicodeString ARemoteDirectory = RemoteDirectory;
    if (!ARemoteDirectory.IsEmpty() && (ARemoteDirectory[1] == L'/'))
    {
      ARemoteDirectory.Delete(1, 1);
    }
    Result = IncludeTrailingBackslash(Result + CopyParam.ValidLocalPath(FromUnixPath(ARemoteDirectory)));
  }

  if (!ForceDirectories(ApiPath(Result)))
  {
    throw EOSExtException(FMTLOAD(CREATE_TEMP_DIR_ERROR, (Result)));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporarilyDownloadFiles(
  TStrings * FileList, bool ForceText, UnicodeString & RootTempDir, UnicodeString & TempDir,
  bool GetTargetNames, bool AutoOperation, bool SimpleTempDir)
{
  DebugAssert(!IsLocalBrowserMode());
  TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  if (ForceText)
  {
    CopyParam.TransferMode = tmAscii;
  }
  TemporaryFileCopyParam(CopyParam);

  if (TempDir.IsEmpty())
  {
    TemporaryDirectoryForRemoteFiles(Terminal->CurrentDirectory, CopyParam, SimpleTempDir, TempDir, RootTempDir);
  }

  DebugAssert(!FAutoOperation);
  FAutoOperation = AutoOperation;
  FMoveToQueue = false;
  Terminal->ExceptionOnFail = true;
  try
  {
    try
    {
      // turn off confirmations, as for MDI editors we may possibly download
      // the same file over
      Terminal->CopyToLocal(
        FileList, TempDir, &CopyParam, cpNoConfirmation | cpTemporary, NULL);

      if (GetTargetNames)
      {
        for (int i = 0; i < FileList->Count; i++)
        {
          FileList->Strings[i] =
            Terminal->ChangeFileName(&CopyParam, UnixExtractFileName(FileList->Strings[i]), osRemote, false);
        }
      }
    }
    catch(...)
    {
      if (!RootTempDir.IsEmpty())
      {
        RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir));
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
void __fastcall TCustomScpExplorerForm::EditorAutoConfig()
{
  // Do not waste time checking for default editor list the next time,
  // or testing if default editor is not notepad.
  bool TryNextTime = false;
  UnicodeString UsageState;

  if (!WinConfiguration->EditorList->IsDefaultList())
  {
    UsageState = "H";
  }
  else
  {
    UnicodeString Executable;
    UnicodeString ExecutableDescription;
    if (DetectSystemExternalEditor(false, Executable, ExecutableDescription, UsageState, TryNextTime))
    {
      UnicodeString Message =
        FMTLOAD(EDITOR_AUTO_CONFIG2, (ExecutableDescription, ExecutableDescription));

      unsigned int Answer =
        MessageDialog(Message, qtConfirmation, qaOK | qaCancel, HELP_EDITOR_AUTO_CONFIG);
      if (Answer != qaOK)
      {
        UsageState = "R";
      }
      else
      {
        UsageState = "A";
        TEditorData EditorData;
        EditorData.Editor = edExternal;
        EditorData.ExternalEditor = FormatCommand(Executable, L"");
        EditorData.ExternalEditorOptionsAutodetect();

        TEditorList EditorList;
        EditorList = *WinConfiguration->EditorList;
        EditorList.Insert(0, new TEditorPreferences(EditorData));
        WinConfiguration->EditorList = &EditorList;
      }
    }
  }

  WinConfiguration->OfferedEditorAutoConfig = !TryNextTime;
  WinConfiguration->Usage->Set(L"EditorAutoConfig", UsageState);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFileNormalize(
  TExecuteFileBy & ExecuteFileBy, const TEditorData *& ExternalEditor,
  const UnicodeString & FileName, bool Local, const TFileMasks::TParams & MaskParams)
{
  if (ExecuteFileBy == efDefaultEditor)
  {
    if (!WinConfiguration->OfferedEditorAutoConfig)
    {
      EditorAutoConfig();
    }

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
    IsSideLocalBrowser(Side), MaskParams);

  UnicodeString Counter;
  UnicodeString LocalFileName;
  bool Handled = false;
  if (!IsSideLocalBrowser(Side))
  {
    // We need to trim VMS version here, so that we use name without version
    // when uploading back to create a new version of the file
    OriginalFileName = Terminal->GetBaseFileName(UnixExtractFileName(FullFileName));
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
        Handled = true;
      }
      else
      {
        throw Exception(FMTLOAD(ALREADY_EDITED_EXTERNALLY_OR_UPLOADED, (OriginalFileName)));
      }
    }

    if (!Handled)
    {
      TStringList * FileList1 = new TStringList();
      try
      {
        FileList1->AddObject(FullFileName, Object);
        TemporarilyDownloadFiles(FileList1,
          RemoteExecuteForceText(ExecuteFileBy, ExternalEditor),
          LocalRootDirectory, LocalDirectory, true, true, false);
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
          DebugFail();
      }
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
        DebugFail();
    }
  }

  if (!Handled)
  {
    Configuration->Usage->Inc(Counter);

    CustomExecuteFile(Side, ExecuteFileBy, LocalFileName, OriginalFileName,
      ExternalEditor, LocalRootDirectory, RemoteDirectory, false, MaskParams.Modification);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteFile(TOperationSide Side,
  TExecuteFileBy ExecuteFileBy, const TEditorData * ExternalEditor,
  bool AllSelected, bool OnFocused)
{
  DebugAssert(!WinConfiguration->DisableOpenEdit);
  DebugAssert((ExecuteFileBy == efExternalEditor) ==
    ((ExternalEditor != NULL) && (ExternalEditor->Editor == edExternal)));

  Side = GetSide(Side);

  TCustomDirView * DView = DirView(Side);
  TStrings * FileList =
    AllSelected ?
      DView->CreateFileList(OnFocused, IsSideLocalBrowser(Side)) :
      DView->CreateFocusedFileList(IsSideLocalBrowser(Side));
  try
  {
    DebugAssert(AllSelected || (FileList->Count == 1));
    for (int i = 0; i < FileList->Count; i++)
    {
      UnicodeString ListFileName = FileList->Strings[i];
      UnicodeString FileNameOnly =
        !IsSideLocalBrowser(Side) ? UnixExtractFileName(ListFileName) : ExtractFileName(ListFileName);
      TListItem * Item = DView->FindFileItem(FileNameOnly);
      if (!DView->ItemIsDirectory(Item))
      {
        UnicodeString FullFileName;
        if (!IsSideLocalBrowser(Side))
        {
          FullFileName = DirView(Side)->Path + ListFileName;
        }
        else
        {
          FullFileName = ListFileName;
        }

        TObject * Object = FileList->Objects[i];

        TFileMasks::TParams MaskParams;
        MaskParams.Size = DView->ItemFileSize(Item);
        TDateTimePrecision Precision;
        MaskParams.Modification = DView->ItemFileTime(Item, Precision);

        std::unique_ptr<TRemoteFile> File;
        if (!IsSideLocalBrowser(Side))
        {
          File.reset(Terminal->TryReadFile(FullFileName));
          if (File.get() != NULL)
          {
            File->FullFileName = FullFileName;
            Object = File.get();
            MaskParams.Size = File->Size;
            MaskParams.Modification = File->Modification;
          }
        }

        ExecuteFile(Side, ExecuteFileBy, ExternalEditor, FullFileName, Object, MaskParams);
      }
    }
  }
  __finally
  {
    delete FileList;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TemporaryFileCopyParam(TCopyParamType & CopyParam)
{
  CopyParam.FileNameCase = ncNoChange;
  CopyParam.PreserveRights = false;
  CopyParam.PreserveReadOnly = false;
  CopyParam.ReplaceInvalidChars = true;
  CopyParam.IncludeFileMask = TFileMasks();
  CopyParam.NewerOnly = false;
  CopyParam.FileMask = L"";
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::EditorCheckNotModified(const TEditedFileData * Data)
{
  return WinConfiguration->EditorCheckNotModified && (Data->SourceTimestamp != TDateTime());
}
//---------------------------------------------------------------------------
class TEditorUploadQueueItem : public TUploadQueueItem
{
public:
  __fastcall TEditorUploadQueueItem(
      TTerminal * Terminal, TStrings * FilesToCopy, const UnicodeString & TargetDir,
      const TCopyParamType * CopyParam, int Params) :
    TUploadQueueItem(Terminal, FilesToCopy, TargetDir, CopyParam, Params, false)
  {
  }

protected:
  virtual void __fastcall DoTransferExecute(TTerminal * Terminal, TParallelOperation * ParallelOperation)
  {
    TUploadQueueItem::DoTransferExecute(Terminal, ParallelOperation);
    TTerminalManager::Instance()->ScpExplorer->EditedFileUploaded(Terminal, CompleteEvent);
  }
};
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::EditedFileUploaded(TTerminal * ATerminal, HANDLE UploadCompleteEvent)
{
  if (WinConfiguration->EditorCheckNotModified) // optimization
  {
    UnicodeString RemoteFilePath;
    {
      TGuard Guard(FEditorManager->Section);
      TEditedFileData * Data = FEditorManager->FindByUploadCompleteEvent(UploadCompleteEvent);
      if (DebugAlwaysTrue(Data != NULL) &&
          EditorCheckNotModified(Data))
      {
        RemoteFilePath = UnixCombinePaths(Data->RemoteDirectory, Data->OriginalFileName);
      }
    }
    if (!RemoteFilePath.IsEmpty())
    {
      std::unique_ptr<TRemoteFile> File(ATerminal->TryReadFile(RemoteFilePath));
      if (File.get() != NULL)
      {
        TGuard Guard(FEditorManager->Section);
        TEditedFileData * Data = FEditorManager->FindByUploadCompleteEvent(UploadCompleteEvent);
        if (DebugAlwaysTrue(Data != NULL))
        {
          Data->SourceTimestamp = File->Modification;
          AppLogFmt(L"Uploaded edited remote file timestamp: %s", (StandardTimestamp(Data->SourceTimestamp)));
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileChanged(
  const UnicodeString & FileName, TEditedFileData * Data, HANDLE UploadCompleteEvent, bool & Retry)
{
  TTerminalManager * Manager = TTerminalManager::Instance();

  if ((Manager->ActiveTerminal == Data->Terminal) &&
      Manager->ScheduleTerminalReconnnect(Data->Terminal))
  {
    AppLog(L"Scheduled session reconnect, will retry the upload later");
    Retry = true;
  }
  else if (!IsActiveTerminal(Data->Terminal))
  {
    if (!NonVisualDataModule->Busy)
    {
      UnicodeString FileNameOnly = ExtractFileName(FileName);
      UnicodeString EditInactiveSessionReopenAcceptedCounter = L"EditInactiveSessionReopenAccepted";
      UnicodeString EditInactiveSessionReopenRejectedCounter = L"EditInactiveSessionReopenRejected";
      if (Data->Terminal == NULL)
      {
        TManagedTerminal * SameSiteTerminal = Manager->FindActiveTerminalForSite(Data->SessionData);
        if (SameSiteTerminal != NULL)
        {
          UnicodeString Message =
            FMTLOAD(EDIT_SESSION_REATTACH,
              (FileNameOnly, Data->SessionName, FileNameOnly));
          if (MessageDialog(Message, qtConfirmation, qaOK | qaCancel) == qaOK)
          {
            Data->Terminal = SameSiteTerminal;
            Data->SessionName = SameSiteTerminal->SessionData->SessionName;
            // We might also overwrite session data
            Configuration->Usage->Inc(EditInactiveSessionReopenAcceptedCounter);
          }
          else
          {
            Configuration->Usage->Inc(EditInactiveSessionReopenRejectedCounter);
            Abort();
          }
        }
      }
      // Foreground session should reconnect itself (except for those disconnected explicitly by the user)
      else if (DebugAlwaysTrue(Terminal->Disconnected || (Terminal != Data->Terminal)))
      {
        UnicodeString Message =
          MainInstructions(
            FMTLOAD(EDIT_SESSION_RECONNECT, (Data->SessionName, FileNameOnly)));
        if (MessageDialog(Message, qtConfirmation, qaOK | qaCancel) == qaOK)
        {
          if (Terminal->Disconnected)
          {
            Manager->ActiveSession = Terminal;
            ReconnectSession();
            // Was it reconnected? (if not, Data->Terminal should be nulled now)
            if (!IsActiveTerminal(Data->Terminal) || (Terminal != Data->Terminal))
            {
              Abort();
            }
          }
          else
          {
            Manager->SetActiveTerminalWithAutoReconnect(Data->Terminal);
          }
          Configuration->Usage->Inc(EditInactiveSessionReopenAcceptedCounter);
        }
        else
        {
          Configuration->Usage->Inc(EditInactiveSessionReopenRejectedCounter);
          Abort();
        }
      }
    }

    if (!IsActiveTerminal(Data->Terminal))
    {
      Configuration->Usage->Inc(L"EditInactiveSession");
      // Prevent this when not idle (!NonVisualDataModule->Busy)?
      throw Exception(FMTLOAD(EDIT_SESSION_CLOSED2,
        (ExtractFileName(FileName), Data->SessionName)));
    }
  }

  if (!Retry && EditorCheckNotModified(Data))
  {
    if (NonVisualDataModule->Busy)
    {
      Retry = true;
    }
    else
    {
      UnicodeString RemoteFilePath = UnixCombinePaths(Data->RemoteDirectory, Data->OriginalFileName);
      std::unique_ptr<TRemoteFile> File(Terminal->TryReadFile(RemoteFilePath));
      if (File.get() != NULL)
      {
        AppLogFmt(L"Edited remote file timestamp: %s, Original timestamp: %s", (StandardTimestamp(File->Modification), StandardTimestamp(Data->SourceTimestamp)));

        if (File->Modification != Data->SourceTimestamp)
        {
          UnicodeString Message = MainInstructions(LoadStr(EDIT_CHANGED_EXTERNALLY));
          if (MessageDialog(Message, qtConfirmation, qaOK | qaCancel) != qaOK)
          {
            Abort();
          }
        }
      }
    }
  }

  if (!Retry)
  {
    TStrings * FileList = new TStringList();
    try
    {
      FileList->Add(FileName);

      // Consider using the same settings (preset) as when the file was downloaded.
      // More over this does not reflect the actual session that will do the upload.
      TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
      TemporaryFileCopyParam(CopyParam);
      if (Data->ForceText)
      {
        CopyParam.TransferMode = tmAscii;
      }
      // so i do not need to worry if masking algorithm works in all cases
      // ("" means "copy file name", no masking is actually done)
      if (ExtractFileName(FileName) == Data->OriginalFileName)
      {
        CopyParam.FileMask = L"";
      }
      else
      {
        CopyParam.FileMask = DelimitFileNameMask(Data->OriginalFileName);
      }

      int Params = cpNoConfirmation | cpTemporary;
      if (Data->Terminal->IsCapable[fcBackgroundTransfers])
      {
        TTerminalQueue * AQueue = Manager->FindQueueForTerminal(Data->Terminal);
        TQueueItem * QueueItem =
          new TEditorUploadQueueItem(Data->Terminal, FileList, Data->RemoteDirectory, &CopyParam, Params);
        QueueItem->CompleteEvent = UploadCompleteEvent;
        AddQueueItem(AQueue, QueueItem, Data->Terminal);
      }
      else
      {
        if (NonVisualDataModule->Busy)
        {
          Retry = true;
        }
        else
        {
          FMoveToQueue = false;
          Data->Terminal->CopyToRemote(FileList, Data->RemoteDirectory, &CopyParam, Params, NULL);
          SetEvent(UploadCompleteEvent);
        }
      }
    }
    __finally
    {
      delete FileList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileReload(
  const UnicodeString & FileName, TEditedFileData * Data)
{
  // Sanity check, we should not be busy otherwise user would not be able to click Reload button.
  DebugAssert(!NonVisualDataModule->Busy);

  if (!IsActiveTerminal(Data->Terminal))
  {
    throw Exception(FMTLOAD(EDIT_SESSION_CLOSED_RELOAD,
      (ExtractFileName(FileName), Data->SessionName)));
  }

  TManagedTerminal * PrevSession = TTerminalManager::Instance()->ActiveSession;
  TTerminalManager::Instance()->ActiveSession = Data->Terminal;
  NonVisualDataModule->StartBusy();
  try
  {
    std::unique_ptr<TRemoteFile> File;
    UnicodeString RemoteFileName =
      UnixIncludeTrailingBackslash(Data->RemoteDirectory) + Data->OriginalFileName;
    Terminal->ExceptionOnFail = true;
    try
    {
      File.reset(Terminal->ReadFile(RemoteFileName));
      if (!File->HaveFullFileName)
      {
        File->FullFileName = RemoteFileName;
      }
    }
    __finally
    {
      Terminal->ExceptionOnFail = false;
    }
    std::unique_ptr<TStrings> FileList(new TStringList());
    FileList->AddObject(RemoteFileName, File.get());

    UnicodeString RootTempDir = Data->LocalRootDirectory;
    UnicodeString TempDir = ExtractFilePath(FileName);

    TemporarilyDownloadFiles(FileList.get(), Data->ForceText, RootTempDir, TempDir, true, true, false);

    if (EditorCheckNotModified(Data))
    {
      Data->SourceTimestamp = File->Modification;
    }

    // sanity check, the target file name should be still the same
    DebugAssert(ExtractFileName(FileName) == FileList->Strings[0]);
  }
  __finally
  {
    NonVisualDataModule->EndBusy();
    // it actually may not exist anymore...
    TTerminalManager::Instance()->ActiveSession = PrevSession;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileEarlyClosed(
  const TEditedFileData * Data, bool & KeepOpen)
{
  // Command is set for external editors only (not for "shell" open).
  if (!Data->Command.IsEmpty())
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
            (Editor->Data->ExternalEditor == Data->Command))
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
        unsigned int Answer = MessageDialog(FMTLOAD(EDITOR_EARLY_CLOSED2, (Data->OriginalFileName)), qtWarning,
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
              (Editor->Data->ExternalEditor == Data->Command) &&
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

    MessageDialog(FMTLOAD(APP_EARLY_CLOSED, (Data->OriginalFileName)), qtWarning,
      qaOK, HELP_APP_EARLY_CLOSED);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecutedFileUploadComplete(TObject * Sender)
{
  EditorFormFileUploadComplete(DebugNotNull(dynamic_cast<TForm *>(Sender)));
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewEnter(TObject * /*Sender*/)
{
  SideEnter(osRemote);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDriveViewEnter(TObject * /*Sender*/)
{
  MakeNextInTabOrder(RemoteDirPanel, RemoteDrivePanel);
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
void __fastcall TCustomScpExplorerForm::FileDeleted(
  TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled)
{
  VisualiseOperationFinished(Side, FileName, Success && NotCancelled);

  if (DebugAlwaysTrue(FDeletedFiles != NULL) && Success)
  {
    FDeletedFiles->Add(FileName);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DeleteFiles(TOperationSide Side,
  TStrings * FileList, bool Alternative)
{
  TCustomDirView * DView = DirView(Side);
  DView->SaveSelection();
  DView->SaveSelectedNames();
  DebugAssert(!FAlternativeDelete);
  FAlternativeDelete = Alternative;

  try
  {
    int Params = FLAGMASK(Alternative, dfAlternative);
    bool Remote = !IsSideLocalBrowser(Side);
    std::unique_ptr<TStringList> DeletedFiles;
    std::unique_ptr<TStrings> PermanentFileList;
    bool AllowMoveToQueue = (Terminal != NULL);
    if (AllowMoveToQueue)
    {
      DeletedFiles.reset(new TStringList());
      DeletedFiles->CaseSensitive = Remote;
    }

    try
    {
      DebugAssert(FDeletedFiles == NULL);
      TValueRestorer<TStrings *> DeletedFilesRestorer(FDeletedFiles, DeletedFiles.get());
      TValueRestorer<TFileOperationFinishedEvent> OnFileOperationFinishedRestorer(FOnFileOperationFinished);
      if (AllowMoveToQueue)
      {
        FOnFileOperationFinished = FileDeleted;
      }

      FMoveToQueue = false;

      if (Remote)
      {
        DebugAssert(AllowMoveToQueue);
        // Clone the file list as it may refer to current directory files, which get destroyed, when the directory is reloaded after the operation
        PermanentFileList.reset(TRemoteFileList::CloneStrings(FileList));
        DebugAssert(Terminal != NULL);

        Terminal->DeleteFiles(FileList, Params);
      }
      else
      {
        if (AllowMoveToQueue) // optimization)
        {
          PermanentFileList.reset(CloneStrings(FileList));
        }
        if (IsLocalBrowserMode())
        {
          Configuration->Usage->Inc(L"LocalLocalDeletes");
        }
        TValueRestorer<TOperationSide> ProgressSideRestorer(FProgressSide, FCurrentSide);
        try
        {
          TTerminalManager::Instance()->LocalTerminal->DeleteLocalFiles(FileList, Params);
        }
        __finally
        {
          ReloadLocalDirectory();
        }
      }

      // Probably not needed for deleting, just for consistency with transfer code
      if (FMoveToQueue)
      {
        Abort();
      }
    }
    catch (EAbort &)
    {
      if (FMoveToQueue && DebugAlwaysTrue(AllowMoveToQueue) && DebugAlwaysTrue(Terminal != NULL))
      {
        FMoveToQueue = false;

        DeletedFiles->Sorted = true;
        for (int Index = 0; Index < PermanentFileList->Count; Index++)
        {
          if (DeletedFiles->IndexOf(PermanentFileList->Strings[Index]) >= 0)
          {
            // We should always be deleting the first item => what can be used to optimize this code, if needed
            DebugAssert(Index == 0);
            PermanentFileList->Delete(Index);
            Index--;
          }
        }

        if (Remote)
        {
          Configuration->Usage->Inc("MovesToBackgroundDelete");

          TQueueItem * QueueItem = new TRemoteDeleteQueueItem(Terminal, PermanentFileList.get(), Params);
          AddQueueItem(Queue, QueueItem, Terminal);
        }
        else
        {
          Configuration->Usage->Inc("MovesToBackgroundDeleteLocal");

          TQueueItem * QueueItem = new TLocalDeleteQueueItem(PermanentFileList.get(), Params);
          AddQueueItem(Queue, QueueItem, Terminal);
        }

        ClearOperationSelection(Side);
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
void __fastcall TCustomScpExplorerForm::LockFiles(TStrings * FileList, bool Lock)
{
  DebugAssert(Terminal);
  DebugAssert(!IsLocalBrowserMode());
  RemoteDirView->SaveSelection();
  RemoteDirView->SaveSelectedNames();

  try
  {
    if (Lock)
    {
      Terminal->LockFiles(FileList);
    }
    else
    {
      Terminal->UnlockFiles(FileList);
    }
  }
  catch(...)
  {
    RemoteDirView->DiscardSavedSelection();
    throw;
  }
  RemoteDirView->RestoreSelection();
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::DoDirectoryExists(void * Session, const UnicodeString & Directory)
{
  TTerminal * ATerminal = (Session == NULL) ? Terminal : reinterpret_cast<TTerminal *>(Session);
  bool Result =
    IsActiveTerminal(ATerminal) &&
    ATerminal->DirectoryExists(Directory);
  return Result;
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::NeedSecondarySessionForRemoteCopy(TStrings * FileList)
{
  bool CopyDirsOnSecondarySession = Terminal->IsCapable[fcSecondaryShell];
  return
    !Terminal->IsCapable[fcRemoteCopy] ||
    (CopyDirsOnSecondarySession && TRemoteFileList::AnyDirectory(FileList));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteTransferDialog(TManagedTerminal *& Session,
  TStrings * FileList, UnicodeString & Target, UnicodeString & FileMask, bool & DirectCopy,
  bool NoConfirmation, bool Move)
{
  DebugAssert(Terminal != NULL);
  DebugAssert(Terminal == TTerminalManager::Instance()->ActiveTerminal);
  DebugAssert(!IsLocalBrowserMode());

  if (Session == NULL)
  {
    Session = Terminal;
  }

  bool TargetConfirmed = false;
  if (Session == Terminal)
  {
    if (RemoteDriveView->DropTarget != NULL)
    {
      Target = RemoteDriveView->NodePathName(RemoteDriveView->DropTarget);
      TargetConfirmed = true;
    }
    else if (RemoteDirView->DropTarget != NULL)
    {
      DebugAssert(RemoteDirView->ItemIsDirectory(RemoteDirView->DropTarget));
      Target = RemoteDirView->ItemFullFileName(RemoteDirView->DropTarget);
      TargetConfirmed = true;
    }
    else
    {
      Target = RemoteDirView->Path;
    }
  }
  else
  {
    Target = GetSessionPath(Session, osRemote);
  }

  Target = UnixIncludeTrailingBackslash(Target);
  if (FileList->Count == 1)
  {
    FileMask = DelimitFileNameMask(UnixExtractFileName(FileList->Strings[0]));
  }
  else
  {
    FileMask = AnyMask;
  }
  DirectCopy = Terminal->IsCapable[fcRemoteCopy] || Terminal->IsCapable[fcSecondaryShell];
  bool Result = true;
  if (!NoConfirmation)
  {
    bool Multi = (FileList->Count > 1);

    if (Move)
    {
      Result = DoRemoteMoveDialog(Multi, Target, FileMask, DoDirectoryExists);
    }
    else
    {
      std::unique_ptr<TStrings> Sessions(new TStringList);
      std::unique_ptr<TStrings> Directories(new TStringList);

      TStrings * SessionList = TTerminalManager::Instance()->SessionList;
      for (int Index = 0; Index < SessionList->Count; Index++)
      {
        TManagedTerminal * ASession = DebugNotNull(dynamic_cast<TManagedTerminal *>(SessionList->Objects[Index]));
        if (IsActiveTerminal(ASession) && DebugAlwaysTrue(!ASession->LocalBrowser))
        {
          Sessions->AddObject(SessionList->Strings[Index], ASession);
          Directories->Add(GetSessionPath(ASession, osRemote));
        }
      }

      bool AnyDirectory = false;
      bool CopyDirsOnSecondarySession = Terminal->IsCapable[fcSecondaryShell];
      if (CopyDirsOnSecondarySession && !Terminal->CommandSessionOpened) // optimization
      {
        for (int Index = 0; !AnyDirectory && (Index < FileList->Count); Index++)
        {
          AnyDirectory = DebugNotNull(dynamic_cast<TRemoteFile *>(FileList->Objects[Index]))->IsDirectory;
        }
      }

      TDirectRemoteCopy AllowDirectCopy;
      if (Terminal->CommandSessionOpened || !NeedSecondarySessionForRemoteCopy(FileList))
      {
        DebugAssert(DirectCopy);
        AllowDirectCopy = drcAllow;
      }
      else if (Terminal->IsCapable[fcSecondaryShell])
      {
        DebugAssert(DirectCopy);
        AllowDirectCopy = Terminal->IsCapable[fcRemoteCopy] ? drcConfirmCommandSessionDirs : drcConfirmCommandSession;
      }
      else
      {
        DebugAssert(!DirectCopy);
        AllowDirectCopy = drcDisallow;
      }
      void * ASession = Session;
      Result = DoRemoteCopyDialog(
        Sessions.get(), Directories.get(), AllowDirectCopy, Multi, ASession, Target, FileMask, DirectCopy, Terminal,
        DoDirectoryExists, TargetConfirmed);
      Session = static_cast<TManagedTerminal *>(ASession);
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteTransferFiles(
  TStrings * FileList, bool NoConfirmation, bool Move, TManagedTerminal * Session)
{
  DebugAssert(!IsLocalBrowserMode());
  bool DirectCopy;
  UnicodeString Target, FileMask;
  bool Result = RemoteTransferDialog(Session, FileList, Target, FileMask, DirectCopy, NoConfirmation, Move);
  if (Result)
  {
    if (!Move && !DirectCopy)
    {
      Configuration->Usage->Inc("RemoteCopyTemp");

      UnicodeString RootTempDir;
      UnicodeString TempDir;

      TemporarilyDownloadFiles(FileList, false, RootTempDir, TempDir, false, false, true);

      TStrings * TemporaryFilesList = new TStringList();

      try
      {
        TMakeLocalFileListParams MakeFileListParam;
        MakeFileListParam.FileList = TemporaryFilesList;
        MakeFileListParam.FileTimes = NULL;
        MakeFileListParam.IncludeDirs = true;
        MakeFileListParam.Recursive = false;

        ProcessLocalDirectory(TempDir, Terminal->MakeLocalFileList, &MakeFileListParam);

        TTerminalManager::Instance()->ActiveSession = Session;

        if (TemporaryFilesList->Count > 0)
        {
          TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
          CopyParam.FileMask = FileMask;

          DebugAssert(!FAutoOperation);
          FAutoOperation = true;
          FMoveToQueue = false;
          Terminal->CopyToRemote(TemporaryFilesList, Target, &CopyParam, cpTemporary, NULL);
        }
      }
      __finally
      {
        delete TemporaryFilesList;
        FAutoOperation = false;
        if (!RootTempDir.IsEmpty())
        {
          RecursiveDeleteFile(ExcludeTrailingBackslash(RootTempDir));
        }
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
          Configuration->Usage->Inc("RemoteMove");

          Terminal->MoveFiles(FileList, Target, FileMask, false);
        }
        else
        {
          Configuration->Usage->Inc("RemoteCopyDirect");

          DebugAssert(DirectCopy);
          DebugAssert(Session == Terminal);

          if (!NeedSecondarySessionForRemoteCopy(FileList) ||
              Terminal->CommandSessionOpened ||
              CommandSessionFallback())
          {
            Terminal->CopyFiles(FileList, Target, FileMask, false);
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
void __fastcall TCustomScpExplorerForm::CreateRemoteDirectory(
  const UnicodeString & Path, TRemoteProperties & Properties)
{
  DebugAssert(!IsLocalBrowserMode());
  Properties.Valid = Properties.Valid << vpEncrypt;
  Properties.Encrypt = GUIConfiguration->CurrentCopyParam.EncryptNewFiles;
  RemoteDirView->CreateDirectoryEx(Path, &Properties);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateDirectory(TOperationSide Side)
{
  Side = GetSide(Side);
  TRemoteProperties Properties = GUIConfiguration->NewDirectoryProperties;
  TRemoteProperties * AProperties = (!IsSideLocalBrowser(Side) ? &Properties : NULL);
  UnicodeString Name = LoadStr(NEW_FOLDER);
  int AllowedChanges =
    FLAGMASK(!IsSideLocalBrowser(Side) && Terminal->IsCapable[fcModeChanging], cpMode);
  bool SaveSettings = false;

  if (DoCreateDirectoryDialog(Name, AProperties, AllowedChanges, SaveSettings))
  {
    TWindowLock Lock(this);
    if (!IsSideLocalBrowser(Side))
    {
      if (SaveSettings)
      {
        GUIConfiguration->NewDirectoryProperties = Properties;
      }
      CreateRemoteDirectory(Name, Properties);
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
  TWindowLock Lock(this);
  DirView(Side)->ExecuteHomeDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenDirectory(TOperationSide Side)
{
  DoOpenDirectoryDialog(odBrowse, Side);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::OpenBookmark(TOperationSide Side, TBookmark * Bookmark)
{
  TOperationSide BookmarkSide = IsLocalBrowserMode() ? osLocal : Side;
  UnicodeString Path = Bookmark->GetSideDirectory(BookmarkSide);

  bool Result = !Path.IsEmpty();
  if (Result)
  {
    TryOpenDirectory(Side, Path);
  }
  return Result;
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
      TCalculateSizeParams Params;
      Params.Stats = &Stats;
      Terminal->CalculateFilesSize(FileList, Size, Params);
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
  if ((Terminal != NULL) && // terminal can be already closed (e.g. dropped connection)
      EnsureCommandSessionFallback(fcCalculatingChecksum))
  {
    Configuration->Usage->Inc(L"ChecksumCalculated");

    try
    {
      Terminal->CalculateFilesChecksum(Alg, FileList, OnCalculatedChecksum);
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
void TCustomScpExplorerForm::LoadFilesProperties(TStrings * FileList)
{
  if (Terminal->LoadFilesProperties(FileList))
  {
    RemoteDirView->Invalidate();
  }
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::CanCalculateChecksum()
{
  return
    Terminal->IsCapable[fcCalculatingChecksum] ||
    (Terminal->IsCapable[fcSecondaryShell] && !Terminal->IsEncryptingFiles());
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::SetProperties(TOperationSide Side, TStrings * FileList)
{
  bool Result;
  if (!IsSideLocalBrowser(Side))
  {
    TRemoteTokenList * GroupList = NULL;
    TRemoteTokenList * UserList = NULL;

    try
    {
      TRemoteProperties CurrentProperties;

      LoadFilesProperties(FileList);

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

      bool CapableAclChanging = Terminal->IsCapable[fcAclChangingFiles];
      if (CapableAclChanging && TRemoteFileList::AnyDirectory(FileList))
      {
        CapableAclChanging = false;
        CurrentProperties.Valid = CurrentProperties.Valid >> vpRights;
      }

      int Flags = 0;
      if (Terminal->IsCapable[fcModeChanging]) Flags |= cpMode;
      if (CapableAclChanging) Flags |= cpAcl;
      if (CapableOwnerChanging) Flags |= cpOwner;
      if (CapableGroupChanging) Flags |= cpGroup;

      TCalculateChecksumEvent CalculateChecksumEvent = NULL;
      if (CanCalculateChecksum())
      {
        CalculateChecksumEvent = CalculateChecksum;
      }

      std::unique_ptr<TStrings> ChecksumAlgs(new TStringList());
      Terminal->GetSupportedChecksumAlgs(ChecksumAlgs.get());

      int Options =
        FLAGMASK(Terminal->IsCapable[fcGroupOwnerChangingByID], poUserGroupByID) |
        FLAGMASK(Terminal->IsCapable[fcTags], poTags);

      TRemoteProperties NewProperties = CurrentProperties;
      Result =
        DoPropertiesDialog(FileList, RemoteDirView->PathName,
          GroupList, UserList, ChecksumAlgs.get(), &NewProperties, Flags, Options,
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
void __fastcall TCustomScpExplorerForm::KeyProcessed(Word & Key, TShiftState Shift)
{
  if (Shift * AllKeyShiftStates() == (TShiftState() << ssAlt))
  {
    FIgnoreNextDialogChar = Key;
  }
  Key = 0;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CheckCustomCommandShortCut(
  TCustomCommandList * List, Word & Key, Classes::TShiftState Shift, TShortCut KeyShortCut)
{
  const TCustomCommandType * Command = List->Find(KeyShortCut);
  if (Command != NULL)
  {
    KeyProcessed(Key, Shift);
    if (CustomCommandState(*Command, false, ccltAll) > 0)
    {
      TAutoFlag DontCopyCommandToClipboardFlag(DontCopyCommandToClipboard);
      ExecuteFileOperationCommand(foCustomCommand, osRemote,
        false, false, const_cast<TCustomCommandType *>(Command));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::KeyDown(Word & Key, Classes::TShiftState Shift)
{
  if (!NonVisualDataModule->Busy)
  {
    if (QueueView3->Focused() && (QueueView3->OnKeyDown != NULL))
    {
      QueueView3->OnKeyDown(QueueView3, Key, Shift);
    }

    if ((Key != 0) && !DirView(osCurrent)->IsEditing())
    {
      TShortCut KeyShortCut = ShortCut(Key, Shift);
      for (int Index = 0; Index < NonVisualDataModule->ExplorerActions->ActionCount; Index++)
      {
        // Note that handling shortcuts on panel (current control) popup menu has precendence over this.
        // But so far it's not a problem, as it does what we want.
        TAction * Action = (TAction *)NonVisualDataModule->ExplorerActions->Actions[Index];
        if (((Action->ShortCut == KeyShortCut) ||
             (Action->SecondaryShortCuts->IndexOfShortCut(KeyShortCut) >= 0)) &&
            AllowedAction(Action, aaShortCut))
        {
          // Has to be called before the action as the dialog char is already in queue.
          // So when the action consumes message queue, we already need to have the
          // FIgnoreNextDialogChar set
          KeyProcessed(Key, Shift);
          // Reset reference to previous component (when menu/toolbar was clicked).
          // Needed to detect that action was invoked by keyboard shortcut
          // in TNonVisualDataModule::ExplorerActionsExecute
          Action->ActionComponent = NULL;
          Action->Execute();
          return;
        }
      }
      for (int i = 0; i < TTerminalManager::Instance()->Count; i++)
      {
        if (NonVisualDataModule->OpenSessionShortCut(i) == KeyShortCut)
        {
          KeyProcessed(Key, Shift);
          TTerminalManager::Instance()->ActiveSessionIndex = i;
          return;
        }
      }
      if (Key == VK_TAB && Shift.Contains(ssCtrl))
      {
        KeyProcessed(Key, Shift);
        TTerminalManager::Instance()->CycleTerminals(!Shift.Contains(ssShift));
      }

      TShortCut CustomShortCut = NormalizeCustomShortCut(KeyShortCut);
      if (IsCustomShortCut(CustomShortCut))
      {
        CheckCustomCommandShortCut(WinConfiguration->CustomCommandList, Key, Shift, CustomShortCut);
        CheckCustomCommandShortCut(WinConfiguration->ExtensionList, Key, Shift, CustomShortCut);

        if (WinConfiguration->SharedBookmarks != NULL)
        {
          TBookmark * Bookmark = WinConfiguration->SharedBookmarks->FindByShortCut(CustomShortCut);
          if ((Bookmark != NULL) &&
              OpenBookmark(FCurrentSide, Bookmark))
          {
            KeyProcessed(Key, Shift);
          }
        }
      }
    }
  }

  TForm::KeyDown(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InitStatusBar()
{
  bool OpenedTerminal = false;
  UnicodeString ProtocolName;
  UnicodeString ProtocolNameHint;
  UnicodeString SecurityProtocolName;
  UnicodeString DurationHint;

  // See the comment in TScpCommanderForm::UpdateControls()
  TTerminal * ActiveTerminal = TTerminalManager::Instance()->ActiveTerminal;
  if (ActiveTerminal != NULL)
  {
    // Active is not enough here
    if (ActiveTerminal->Status == ssOpened)
    {
      const TSessionInfo & SessionInfo = ActiveTerminal->GetSessionInfo();
      const TFileSystemInfo & FileSystemInfo = ActiveTerminal->GetFileSystemInfo();
      if (FileSystemInfo.ProtocolName.IsEmpty())
      {
        ProtocolName = SessionInfo.ProtocolName;
      }
      else
      {
        ProtocolName = FileSystemInfo.ProtocolName;
      }
      ProtocolNameHint = LoadStr(STATUS_PROTOCOL_HINT);
      SecurityProtocolName = SessionInfo.SecurityProtocolName;

      DurationHint = LoadStr(STATUS_DURATION_HINT);
      OpenedTerminal = true;
    }
    else if (ActiveTerminal->Status > ssClosed)
    {
      ProtocolName = LoadStr(STATUS_CONNECTING);
    }
    else
    {
      ProtocolName = LoadStr(STATUS_NOT_CONNECTED2);
    }
  }

  TTBXStatusBar * SessionStatusBar = (TTBXStatusBar *)GetComponent(fcStatusBar);

  int Offset = SessionStatusBar->Panels->Count - SessionPanelCount;

  TTBXStatusPanel * ProtocolStatusPanel = SessionStatusBar->Panels->Items[Offset + 0];
  ProtocolStatusPanel->ViewPriority = (ActiveTerminal != NULL) ? 100 : 0;
  if (ProtocolStatusPanel->Caption != ProtocolName)
  {
    ProtocolStatusPanel->Caption = ProtocolName;
    std::unique_ptr<TCanvas> Canvas(CreateControlCanvas(SessionStatusBar));
    ProtocolStatusPanel->Size = Canvas->TextWidth(ProtocolName) + ScaleByTextHeight(SessionStatusBar, 32);
  }
  ProtocolStatusPanel->Hint = ProtocolNameHint;

  TTBXStatusPanel * SecurityStatusPanel = SessionStatusBar->Panels->Items[Offset + 1];
  SecurityStatusPanel->ViewPriority = OpenedTerminal ? 98 : 0;
  bool SecurityEnabled = !SecurityProtocolName.IsEmpty();
  SecurityStatusPanel->Enabled = SecurityEnabled;
  UnicodeString SecurityProtocolHint;
  if (SecurityEnabled)
  {
    SecurityProtocolHint = FMTLOAD(STATUS_SECURE, (SecurityProtocolName));
  }
  else
  {
    SecurityProtocolHint = LoadStr(STATUS_INSECURE);
  }
  SecurityStatusPanel->Hint = SecurityProtocolHint;

  TTBXStatusPanel * DurationStatusPanel = SessionStatusBar->Panels->Items[Offset + 2];
  DurationStatusPanel->ViewPriority = OpenedTerminal ? 99 : 0;
  DurationStatusPanel->Hint = DurationHint;

  UpdateStatusBar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusBar()
{
  TTBXStatusBar * SessionStatusBar = (TTBXStatusBar *)GetComponent(fcStatusBar);
  DebugAssert(SessionStatusBar != NULL);

  TTBXStatusPanel * NoteStatusPanel = SessionStatusBar->Panels->Items[0];
  if (!FNote.IsEmpty())
  {
    NoteStatusPanel->Caption = FNote;
  }
  else
  {
    UpdateStatusPanelText(NoteStatusPanel);
  }
  NoteStatusPanel->Hint = FNoteHints;

  UnicodeString LoginTime;
  // See the comment in TScpCommanderForm::UpdateControls()
  TTerminal * ActiveTerminal = TTerminalManager::Instance()->ActiveTerminal;
  // Active is not enough here
  if ((ActiveTerminal != NULL) && (ActiveTerminal->Status == ssOpened))
  {
    const TSessionInfo & SessionInfo = ActiveTerminal->GetSessionInfo();
    LoginTime = FormatDateTimeSpan(Now() - SessionInfo.LoginTime);
  }
  TTBXStatusPanel * DurationStatusPanel = SessionStatusBar->Panels->Items[SessionStatusBar->Panels->Count - 1];
  DurationStatusPanel->Caption = LoginTime;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateStatusPanelText(TTBXStatusPanel * Panel)
{
  Panel->Caption = L"";
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Idle()
{

  if (FShowing ||
      // Particularly for
      // - Standalone editing
      // - Detecting closed connection and automatically reconnect it while waiting for changes
      //   while "keeping remote directory up to date".
      StandaloneOperation)
  {
    FEditorManager->Check();

    // make sure that Idle is called before update queue, as it may invoke QueueEvent
    // that needs to know if queue view is visible (and it may be closed after queue update)
    TTerminalManager::Instance()->Idle(FDoNotIdleCurrentTerminal > 0);
  }

  if (FShowing)
  {
    if (!NonVisualDataModule->Busy &&
        // Menu is opened or drag&drop is going on
        (Mouse->Capture == NULL))
    {
      if (FRefreshRemoteDirectory)
      {
        if ((Terminal != NULL) && (Terminal->Status == ssOpened))
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
        if ((Terminal != NULL) && (Terminal->Status == ssOpened) &&
            (Now() - Terminal->DirectoryLoaded > WinConfiguration->RefreshRemotePanelInterval))
        {
          RemoteDirView->ReloadDirectory();
        }
      }
    }

    // TB2 shows accels on Alt key down, but hides it only after menu modal loop finishes.
    // But if Alt is used to switch apps or for keyboard shortcut, the menu modal loop does not happen and accelerators stay displayed.
    // It's difficult to fix this in TB2, as there's no "up" equivalent to CM_DIALOGKEY, which shows the accels.
    // And the Alt key can be released even when other window or application is active, so handling the key up in explorer window won't do either.
    TTBCustomToolbar * MenuToolbar = DebugNotNull(dynamic_cast<TTBCustomToolbar *>(GetComponent(fcMenu)));
    MenuToolbar->Idle();
  }

  if (FShowing || StandaloneOperation)
  {
    if (FQueueStatusInvalidated)
    {
      UpdateQueueStatus(false);
    }

    RefreshQueueItems();
  }

  if (FShowing)
  {
    UpdateStatusBar();
  }

  if (FClipboardFakeMonitorsPendingReset && !FDownloadingFromClipboard)
  {
    FClipboardFakeMonitorsPendingReset = false;
    FClipboardFakeMonitors.reset(NULL);
  }

  FIgnoreNextDialogChar = 0;

}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UserActionTimer(TObject * /*Sender*/)
{
  try
  {
    FUserActionTimer->Enabled = false;
    if (IsQueueAutoPopup() && (FPendingQueueActionItem != NULL))
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
void __fastcall TCustomScpExplorerForm::ApplicationMinimize(TObject * /*Sender*/)
{
  if (WinConfiguration->MinimizeToTray)
  {
    UpdateTrayIcon();
    FTrayIcon->Visible = true;
    if (Visible)
    {
      ShowWindow(Handle, SW_HIDE);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationRestore(TObject * /*Sender*/)
{
  // WORKAROUND
  // When restoring maximized window from minimization,
  // rarely some controls do not align properly.
  // Two instances seen (both for Commander):
  // - When restoring, window is temporarily narrower (not maximized),
  //   causing toolbars on TopDock to wrap and dock to expand horizontally.
  //   Once maximized already, top dock shinks back, but the session PageControl,
  //   do not align up, leaving space between TopDock and PageControl.
  // - Similar issue seen with LocalDirView not aligning down to status bar.
  for (int Index = 0; Index < ControlCount; Index++)
  {
    RealignControl(Controls[Index]);
  }

  if (FTrayIcon->Visible)
  {
    FTrayIcon->Visible = false;
    if (Visible)
    {
      ShowWindow(Handle, SW_SHOW);
    }
  }

  if (FNeedSession && DebugAlwaysTrue(Terminal == NULL))
  {
    FNeedSession = false;
    NonVisualDataModule->StartBusy();
    try
    {
      NeedSession(true);
    }
    __finally
    {
      NonVisualDataModule->EndBusy();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateTrayIcon()
{
  FTrayIcon->Hint = GetMainForm()->Caption;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationTitleChanged()
{
  UpdateTrayIcon();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreApp()
{
  // workaround
  // TApplication.WndProc sets TApplication.FAppIconic to false,
  // on WM_ACTIVATEAPP, what renders TApplication.Restore no-op function.
  // But WM_ACTIVATEAPP message can be received even when
  // the main window is minimized to the tray and internal editor window is focused
  // (after another application was previously active)
  if (::IsIconic(Handle))
  {
    if (!IsAppIconic())
    {
      SetAppIconic(true);
    }
  }
  ::ApplicationRestore();
  Application->BringToFront();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TrayIconClick(TObject * /*Sender*/)
{
  RestoreApp();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NewSession(const UnicodeString & SessionUrl)
{
  if (OpenInNewWindow())
  {
    ExecuteNewInstance(SessionUrl);
  }
  else
  {
    TTerminalManager::Instance()->NewSession(SessionUrl);
  }
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::NewTab(TOperationSide, bool)
{
  NewSession();
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::SaveHiddenDuplicateSession(TSessionData * SessionData)
{
  UnicodeString SessionName = StoredSessions->HiddenPrefix + ManagedSession->SessionData->SessionName;
  StoredSessions->NewSession(SessionName, SessionData);
  // modified only, explicit
  StoredSessions->Save(false, true);

  // encode session name because of slashes in hierarchical sessions
  return EncodeUrlString(SessionName);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::CreateHiddenDuplicateSession()
{
  // current working directories become defaults here, what is not right
  std::unique_ptr<TSessionData> SessionData(CloneCurrentSessionData());
  return SaveHiddenDuplicateSession(SessionData.get());
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DuplicateTab()
{
  if (OpenInNewWindow())
  {
    ExecuteNewInstance(CreateHiddenDuplicateSession());
  }
  else
  {
    // current working directories become defaults here, what is not right
    std::unique_ptr<TSessionData> SessionData(CloneCurrentSessionData());

    TTerminalManager * Manager = TTerminalManager::Instance();
    TManagedTerminal * ASession = Manager->NewManagedTerminal(SessionData.get());
    DebugAssert(ASession->LocalBrowser == ManagedSession->LocalBrowser);
    // We definitely want these
    ASession->Disconnected = ManagedSession->Disconnected;
    // Not sure about these two
    ASession->Permanent = ManagedSession->Permanent;
    ASession->DisconnectedTemporarily = ManagedSession->DisconnectedTemporarily;
    Manager->ActiveSession = ASession;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RenameTab()
{
  UnicodeString Name = ManagedSession->SessionData->SessionName;
  if (InputDialog(LoadStr(RENAME_TAB_TITLE), LoadStr(RENAME_TAB_PROMPT), Name, HELP_TAB_RENAME) &&
      // When submitting the default name, do not fix it
      (Name != ManagedSession->SessionData->SessionName))
  {
    // Checks for a slash only, so it's not that big deal that we do the check after submitting the dialog only.
    TSessionData::ValidateName(Name);

    // This is inconsistent with how color (for example) is handled.
    ManagedSession->SessionData->Name = Name;

    UpdateControls();
    // Add/Remove distinguishing paths from sessions of the same name.
    SessionListChanged();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanCloseQueue(TTerminalQueue * Queue)
{
  DebugAssert(Queue != NULL);
  bool Result = Queue->IsEmpty;
  if (!Result)
  {
    SetFocus();
    Result = (MessageDialog(LoadStr(PENDING_QUEUE_ITEMS2), qtWarning, qaOK | qaCancel, HELP_NONE) == qaOK);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanCloseQueue()
{
  return CanCloseQueue(FQueue);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseTab()
{
  if (CanCloseQueue())
  {
    TTerminalManager::Instance()->FreeActiveTerminal();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DisconnectSession()
{
  if (CanCloseQueue())
  {
    TTerminalManager::Instance()->DisconnectActiveTerminal();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalDisconnected()
{
  DetachTerminal(Terminal);
  RemoteDirView->Terminal = Terminal;
  UpdateRemotePathComboBox(false);
  InitStatusBar();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalConnecting()
{
  InitStatusBar();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReconnectSession()
{
  if (DebugAlwaysTrue(!Terminal->Active))
  {
    TTerminalManager::Instance()->ReconnectActiveTerminal();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenStoredSession(TSessionData * Data)
{
  if (OpenInNewWindow())
  {
    // encode session name because of slashes in hierarchical sessions
    ExecuteNewInstance(EncodeUrlString(Data->Name));
  }
  else
  {
    TTerminalManager * Manager = TTerminalManager::Instance();
    TManagedTerminal * Terminal = Manager->NewManagedTerminal(Data);
    Manager->ActiveSession = Terminal;
  }
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::DoOpenFolderOrWorkspace(
  const UnicodeString & Name, bool ConnectFirstTerminal, bool CheckMaxSessions)
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  std::unique_ptr<TObjectList> DataList(new TObjectList());
  // This asks for a master password. Maybe we should postpone asking until a connection is actually opened.
  StoredSessions->GetFolderOrWorkspace(Name, DataList.get());
  if (!CheckMaxSessions || (DataList->Count <= WinConfiguration->MaxSessions))
  {
    TManagedTerminal * FirstSession = Manager->NewSessions(DataList.get());
    // FirstSession can be null, if none of the workspace sites exist anymore
    // or if all workspace sessions are not suported by the interface
    // (when trying to open all local-local workspace in "explorer" interface)
    if (FirstSession != NULL)
    {
      if (!ConnectFirstTerminal)
      {
        FirstSession->Disconnected = true;
        FirstSession->DisconnectedTemporarily = true;
      }
      Manager->ActiveSession = FirstSession;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::OpenFolderOrWorkspace(const UnicodeString & Name)
{
  if (OpenInNewWindow())
  {
    ExecuteNewInstance(Name);
  }
  else
  {
    DoOpenFolderOrWorkspace(Name, true, false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormCloseQuery(TObject * /*Sender*/,
      bool &CanClose)
{
  AppLog(L"Windows close query");
  // See the comment in CloseApp()
  if (NonVisualDataModule->Busy)
  {
    CanClose = false;
  }
  else
  {
    TTerminalManager * Manager = TTerminalManager::Instance();

    int ActiveSessions = 0;
    for (int Index = 0; Index < Manager->Count; Index++)
    {
      if (Manager->Sessions[Index]->Active)
      {
        ActiveSessions++;
      }
    }
    // Confirm, when there's at least one active remote session or
    // when there are multiple (inactive or local) sessions and auto workspace saving is not enabled.
    if (((ActiveSessions > 0) || ((Manager->Count > 1) && !WinConfiguration->AutoSaveWorkspace)) &&
        WinConfiguration->ConfirmClosingSession)
    {
      unsigned int Result;
      TMessageParams Params(mpNeverAskAgainCheck);
      UnicodeString Message;
      int Answers = qaOK | qaCancel;
      // The current session is the only active session
      if ((ActiveSessions == 1) && (Terminal != NULL) && Terminal->Active)
      {
        if (!WinConfiguration->AutoSaveWorkspace)
        {
          Message = LoadStr(CLOSE_SESSION_WORKSPACE);
        }
        else
        {
          Message = LoadStr(CLOSE_SESSION);
        }
        Message = FORMAT(Message, (Terminal->SessionData->SessionName));
      }
      // Multiple active sessions or one active session, but it's not the current one
      else if (ActiveSessions > 0)
      {
        if (!WinConfiguration->AutoSaveWorkspace)
        {
          Message = LoadStr(CLOSE_SESSIONS_WORKSPACE3);
        }
        else
        {
          Message = LoadStr(CLOSE_SESSIONS);
        }
      }
      // Multiple tabs, but all are inactive or local, and saving of worspace is not enabled
      else
      {
        DebugAlwaysTrue(Manager->Count > 1);
        DebugAlwaysTrue(!WinConfiguration->AutoSaveWorkspace);
        Message = LoadStr(CLOSE_WORKSPACE);
      }

      UnicodeString Note;
      if (WinConfiguration->AutoSaveWorkspace)
      {
        Note = FMTLOAD(AUTO_WORKSPACE, (WorkspaceName()));
      }
      else
      {
        Note = LoadStr(AUTO_WORKSPACE_ENABLE);
        Answers = qaYes | qaNo | qaCancel;
      }
      Message = FORMAT("%s\n\n%s", (MainInstructions(Message), Note));

      SetFocus();
      Result = MessageDialog(Message, qtConfirmation,
        Answers, HELP_CLOSE_SESSION_WORKSPACE, &Params);

      if (Result == qaNeverAskAgain)
      {
        WinConfiguration->ConfirmClosingSession = false;
      }

      if (Result == qaNo)
      {
        CanClose = SaveWorkspace(true);
        // note that the workspace will be saved redundatly again from FormClose
      }
      else
      {
        CanClose =
          (Result == qaOK) ||
          (Result == qaYes) || // CLOSE_SESSIONS_WORKSPACE variant
          (Result == qaNeverAskAgain);
      }
    }

    if (CanClose && HasActiveTerminal())
    {
      CanClose = CanCloseQueue();
    }
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
        CanClose = FEditorManager->Empty(true);
        if (!CanClose)
        {
          SetFocus();
          CanClose =
            (MessageDialog(
              LoadStr(PENDING_EDITORS), qtWarning, qaIgnore | qaCancel, HELP_NONE) == qaIgnore);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseInternalEditor(TObject * Sender)
{
  TForm * Form = dynamic_cast<TForm *>(Sender);
  DebugAssert(Form != NULL);
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
void __fastcall TCustomScpExplorerForm::ComponentShowing(Byte Component, bool value)
{
  if (value)
  {
    if (Component == fcCustomCommandsBand)
    {
      UpdateCustomCommandsToolbar();
    }

    if (Component == fcQueueFileList)
    {
      DebugAssert(!QueueFileListSplitter->Visible);
      AdjustQueueLayout();
      UpdateQueueFileList();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AdjustQueueLayout()
{
  int MinHeightLimit =
    GetStaticQueuePanelComponentsHeight() +
    QueueFileListSplitter->Height +
    QueueFileList->Height +
    GetMinQueueViewHeight();
  if (QueuePanel->ClientHeight < MinHeightLimit)
  {
    QueuePanel->ClientHeight = MinHeightLimit;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetComponentVisible(Byte Component, Boolean value)
{
  TControl * Control = GetComponent(Component);
  DebugAssert(Control);
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

      int Reserve = ScaleByTextHeight(this, 32);
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
bool __fastcall TCustomScpExplorerForm::IsComponentPossible(Byte Component)
{
  bool Result;
  if ((Component == fcExplorerUpdatesBand) || (Component == fcCommanderUpdatesBand))
  {
    Result = !IsUWP();
  }
  else
  {
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::MakeFocusedItemVisible(TCustomDirView * DirView)
{
  if (DirView->ItemFocused != NULL)
  {
    DirView->ItemFocused->MakeVisible(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FixControlsPlacement()
{
  MakeFocusedItemVisible(RemoteDirView);
  QueueSplitter->Visible = QueuePanel->Visible;
  QueueFileListSplitter->Visible = QueueFileList->Visible;
  RemotePanelSplitter->Visible = RemoteDrivePanel->Visible;

  TControl * TopControlsOrder[] =
    { TopDock, MessageDock, SessionsPageControl };
  SetVerticalControlsOrder(TopControlsOrder, LENOF(TopControlsOrder));

  TControl * QueueControlsOrder[] =
    { QueueDock, QueueView3, QueueFileListSplitter, QueueFileList };
  SetVerticalControlsOrder(QueueControlsOrder, LENOF(QueueControlsOrder));

}
//---------------------------------------------------------------------------
TControl * __fastcall TCustomScpExplorerForm::GetComponent(Byte Component)
{
  switch (Component) {
    case fcStatusBar: return RemoteStatusBar;
    case fcRemotePopup: return reinterpret_cast<TControl *>(NonVisualDataModule->RemoteFilePopup);
    case fcQueueView: return QueuePanel;
    case fcQueueToolbar: return QueueDock;
    case fcRemoteTree: return RemoteDrivePanel;
    case fcSessionsTabs: return SessionsPageControl;
    case fcQueueFileList: return QueueFileList;
    default: return NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewColumnRightClick(
      TObject *Sender, TListColumn *Column, TPoint &Point)
{
  DebugAssert(NonVisualDataModule && Column && Sender);
  NonVisualDataModule->ListColumn = Column;
  TPopupMenu * DirViewColumnMenu;
  if (Sender == DirView(osOther))
  {
    DirViewColumnMenu = NonVisualDataModule->RemoteDirViewColumnPopup;
    NonVisualDataModule->RemoteSortByExtColumnPopupItem->Visible = (Column->Index == uvName);
    bool SizeColumn = (Column->Index == uvSize);
    NonVisualDataModule->RemoteFormatSizeBytesPopupItem->Visible = SizeColumn;
    NonVisualDataModule->RemoteCalculateDirectorySizesPopupItem->Visible = SizeColumn;
  }
  else
  {
    DirViewColumnMenu = NonVisualDataModule->LocalDirViewColumnPopup;
    NonVisualDataModule->LocalSortByExtColumnPopupItem->Visible = (Column->Index == dvName);
    bool SizeColumn = (Column->Index == dvSize);
    NonVisualDataModule->LocalFormatSizeBytesPopupItem->Visible = SizeColumn;
    NonVisualDataModule->LocalCalculateDirectorySizesPopupItem->Visible = SizeColumn;
  }

  TCustomListView * ListView = DebugNotNull(dynamic_cast<TCustomListView *>(Sender));
  TPoint ScreenPoint = ListView->ClientToScreen(Point);

  ScreenPoint.x -= GetScrollPos(ListView->Handle, SB_HORZ);

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
  DebugAssert(Sender && Item && Configuration);
  DebugAssert(AllowExec);
  TCustomDirView * ADirView = (TCustomDirView *)Sender;
  TOperationSide Side = ((ADirView == DirView(osRemote)) ? osRemote : osLocal);
  bool Remote = !IsSideLocalBrowser(Side);

  DebugAssert(Item == ADirView->ItemFocused);
  bool IsDirectory = ADirView->ItemIsDirectory(Item);
  TResolvedDoubleClickAction Action;

  if (FForceExecution)
  {
    Action = IsDirectory ? rdcaChangeDir : rdcaOpen;
  }
  else
  {
    TTerminal * ATerminal = !IsSideLocalBrowser(Side) ? Terminal : NULL;
    Action = WinConfiguration->ResolveDoubleClickAction(IsDirectory, ATerminal);
  }

  if (Action == rdcaCopy)
  {
    // To counter the lock in DirViewBusy (called from TCustomDirView.DoExecute)
    UnlockWindow();
    try
    {
      if (IsLocalBrowserMode())
      {
        LocalLocalCopy(foCopy, Side, true, !WinConfiguration->CopyOnDoubleClickConfirmation, false, 0);
      }
      else
      {
        if (HasActiveTerminal())
        {
          ExecuteFileOperation(foCopy, Side, true, !WinConfiguration->CopyOnDoubleClickConfirmation);
        }
      }
    }
    __finally
    {
      LockWindow();
    }
    AllowExec = false;
  }
  else if (Action == rdcaEdit)
  {
    if (!Remote || !WinConfiguration->DisableOpenEdit)
    {
      ExecuteFile(Side, efDefaultEditor);
      AllowExec = false;
    }
  }
  else if (Action == rdcaOpen)
  {
    if (DebugAlwaysTrue(!IsDirectory) && Remote && !WinConfiguration->DisableOpenEdit)
    {
      ExecuteFile(osRemote, efShell);
      AllowExec = false;
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
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeDirectories()
{
  DebugFail();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoSynchronizeDirectories(
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory, int UseDefaults)
{
  TSynchronizeParamType Params;
  Params.LocalDirectory = LocalDirectory;
  Params.RemoteDirectory = RemoteDirectory;
  int UnusedParams =
    (GUIConfiguration->SynchronizeParams &
      (TTerminal::spPreviewChanges | TTerminal::spTimestamp | TTerminal::spNotByTime | TTerminal::spBySize |
       TTerminal::spCaseSensitive | TTerminal::spMirror | TTerminal::spByChecksum));
  Params.Params = GUIConfiguration->SynchronizeParams & ~UnusedParams;
  Params.Options = GUIConfiguration->SynchronizeOptions;
  bool SaveSettings = false;
  TSynchronizeController Controller(&DoSynchronize, &DoSynchronizeInvalid,
    &DoSynchronizeTooManyDirectories);
  DebugAssert(FSynchronizeController == NULL);
  FSynchronizeController = &Controller;
  bool Result;
  try
  {
    TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
    int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).Upload;
    int Options =
      FLAGMASK(SynchronizeAllowSelectedOnly(), soAllowSelectedOnly);
    DebugAssert(FOnFeedSynchronizeError == NULL);
    DebugAssert(FOnSynchronizeAbort == NULL);
    DebugAssert(FSynchronizeTerminal == NULL);
    FSynchronizeTerminal = Terminal;
    Result = DoSynchronizeDialog(Params, &CopyParam, Controller.StartStop,
      SaveSettings, Options, CopyParamAttrs, GetSynchronizeOptions, SynchronizeSessionLog,
      FOnFeedSynchronizeError, FOnSynchronizeAbort, SynchronizeInNewWindow, UseDefaults);
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
    DebugAssert(FOnFeedSynchronizeError == NULL);
    FOnFeedSynchronizeError = NULL;
    DebugAssert(FOnSynchronizeAbort == NULL);
    FOnSynchronizeAbort = NULL;
    FSynchronizeTerminal = NULL;
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
  // Keep-up-to-date mode
  if (Terminal->Status == ssOpened)
  {
    try
    {
      int PParams = Params.Params;
      if (!Full)
      {
        PParams |= TTerminal::spNoRecurse | TTerminal::spUseCache | TTerminal::spDelayProgress;
        if (FLAGSET(Params.Options, soRecurse))
        {
          PParams |= TTerminal::spSubDirs;
        }
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
    catch (Exception & E)
    {
      if (!Terminal->Active)
      {
        ShowExtendedExceptionEx(Terminal, &E);
        // Do not abort the "keep up to date" when the session was succesfully reconnected.
        if (!Terminal->Active)
        {
          throw;
        }
      }
      else
      {
        if (FLAGSET(Params.Options, soContinueOnError) &&
            DebugAlwaysTrue(FOnFeedSynchronizeError != NULL))
        {
          // noop, already logged in MoreMessageDialog
        }
        else
        {
          // We get mostly EAbort here, so this is noop.
          // Exception is an error when listing directories while looking for differences.
          ShowExtendedExceptionEx(Terminal, &E);
          throw;
        }
      }
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
  // Keep-up-to-date mode
  DebugAssert(!FAutoOperation);
  void * BatchStorage;
  BatchStart(BatchStorage);
  FAutoOperation = true;

  bool AnyOperation = false;
  TDateTime StartTime = Now();
  TSynchronizeChecklist * AChecklist = NULL;
  try
  {
    FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, -1);
    if (FLAGCLEAR(Params, TTerminal::spDelayProgress))
    {
      FSynchronizeProgressForm->Start();
    }

    AChecklist = Terminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
      static_cast<TTerminal::TSynchronizeMode>(Mode),
      &CopyParam, Params | TTerminal::spNoConfirmation, TerminalSynchronizeDirectory,
      Options);

    SetTaskbarListProgressState(TBPF_NOPROGRESS);
    SAFE_DESTROY(FSynchronizeProgressForm);

    AnyOperation = (AChecklist->CheckedCount > 0);
    if (AnyOperation)
    {
      TSynchronizeProgress SynchronizeProgress(AChecklist);
      CreateProgressForm(&SynchronizeProgress);

      try
      {
        Terminal->SynchronizeApply(
          AChecklist, &CopyParam, Params | TTerminal::spNoConfirmation, TerminalSynchronizeDirectory,
          SynchronizeProcessedItem, NULL, NULL, NULL);
      }
      __finally
      {
        DestroyProgressForm();
      }
    }

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
    if (AnyOperation)
    {
      OperationComplete(StartTime);
    }
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
void __fastcall TCustomScpExplorerForm::SynchronizeSessionLog(const UnicodeString & Message)
{
  LogSynchronizeEvent(Terminal, Message);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetSynchronizeOptions(
  int Params, TSynchronizeOptions & Options)
{
  DebugAssert(!IsLocalBrowserMode());
  if (FLAGSET(Params, TTerminal::spSelectedOnly) && SynchronizeAllowSelectedOnly())
  {
    Options.Filter = new TStringList();
    Options.Filter->CaseSensitive = false;
    Options.Filter->Duplicates = Types::dupAccept;

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
UnicodeString __fastcall TCustomScpExplorerForm::SerializeCopyParamForCommandLine(const TCopyParamType * CopyParams)
{
  TCopyParamType Defaults;
  std::unique_ptr<THierarchicalStorage> ConfigStorage(Configuration->CreateConfigStorage());
  ConfigStorage->AccessMode = smRead;
  if (ConfigStorage->OpenSubKey(Configuration->ConfigurationSubKey, false))
  {
    GUIConfiguration->LoadCopyParam(ConfigStorage.get(), &Defaults);
  }
  ConfigStorage.reset(NULL);

  std::unique_ptr<TStringList> Options(new TStringList());
  std::unique_ptr<TOptionsStorage> OptionsStorage(new TOptionsStorage(Options.get(), true));
  CopyParams->Save(OptionsStorage.get(), &Defaults);

  UnicodeString Result;
  if (Options->Count > 0)
  {
    Result =
      FORMAT(L" %s%s", (TProgramParams::FormatSwitch(RAWTRANSFERSETTINGS_SWITCH), StringsToParams(Options.get())));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeInNewWindow(
  const TSynchronizeParamType & Params, const TCopyParamType * CopyParams)
{
  UnicodeString SessionName = CreateHiddenDuplicateSession();

  UnicodeString AdditionalParams =
    FORMAT(L"%s%s %s // \"%s\" \"%s\" %d %d", (
      TProgramParams::FormatSwitch(DEFAULTS_SWITCH),
      SerializeCopyParamForCommandLine(CopyParams),
      TProgramParams::FormatSwitch(KEEP_UP_TO_DATE_SWITCH),
      Params.LocalDirectory, Params.RemoteDirectory, Params.Params, Params.Options));

  ExecuteNewInstance(SessionName, AdditionalParams);
}
//---------------------------------------------------------------------------
struct TSynchronizeParams
{
  UnicodeString LocalDirectory;
  UnicodeString RemoteDirectory;
  TSynchronizeMode Mode;
  TSynchronizeChecklist * Checklist;
  int Params;
  TCopyParamType * CopyParam;
  TDateTime CollectElapsed;
  TDateTime * StartTime;
  TProcessedSynchronizationChecklistItem OnProcessedItem;
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FullSynchronize(
  TSynchronizeParams & Params, TProcessedSynchronizationChecklistItem OnProcessedItem,
  TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems)
{
  DebugAssert(!FAutoOperation);
  void * BatchStorage;
  BatchStart(BatchStorage);
  FAutoOperation = true;

  TFileOperationStatistics Statistics;
  TDateTime Start = Now();

  try
  {
    Params.OnProcessedItem = OnProcessedItem;

    TSynchronizeProgress SynchronizeProgress(Params.Checklist);
    CreateProgressForm(&SynchronizeProgress);

    Terminal->SynchronizeApply(
      Params.Checklist, Params.CopyParam, Params.Params | TTerminal::spNoConfirmation,
      TerminalSynchronizeDirectory, SynchronizeProcessedItem, OnUpdatedSynchronizationChecklistItems, &Params,
      &Statistics);
  }
  __finally
  {
    Params.OnProcessedItem = NULL;
    FAutoOperation = false;
    DestroyProgressForm();
    BatchEnd(BatchStorage);
    ReloadLocalDirectory();
  }

  if (WinConfiguration->SynchronizeSummary)
  {
    UnicodeString Message = MainInstructions(LoadStr(SYNCHRONIZE_COMPLETE)) + L"\n";

    // The statistics should be 0 anyway in this case
    if (FLAGCLEAR(Params.Params, TTerminal::spTimestamp))
    {
      Message += L"\n";
      if (Statistics.FilesUploaded > 0)
      {
        Message += FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 1), (FormatNumber(Statistics.FilesUploaded), FormatBytes(Statistics.TotalUploaded))) + L"\n";
      }
      if (Statistics.FilesDownloaded > 0)
      {
        Message += FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 2), (FormatNumber(Statistics.FilesDownloaded), FormatBytes(Statistics.TotalDownloaded))) + L"\n";
      }
      if (Statistics.FilesDeletedLocal > 0)
      {
        Message += FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 3), (FormatNumber(Statistics.FilesDeletedLocal))) + L"\n";
      }
      if (Statistics.FilesDeletedRemote > 0)
      {
        Message += FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 4), (FormatNumber(Statistics.FilesDeletedRemote))) + L"\n";
      }
    }

    TDateTime Elapsed = (Now() - Start);
    Message +=
      L"\n" +
      FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 5), (FormatDateTimeSpan(Params.CollectElapsed))) + L"\n" +
      FORMAT(LoadStrPart(SYNCHRONIZE_SUMMARY, 6), (FormatDateTimeSpan(Elapsed)));
    TMessageParams Params(mpNeverAskAgainCheck);
    unsigned int Result = MessageDialog(Message, qtInformation, qaOK, HELP_NONE, &Params);
    if (Result == qaNeverAskAgain)
    {
      WinConfiguration->SynchronizeSummary = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SynchronizeProcessedItem(void * Token, const TSynchronizeChecklist::TItem * ChecklistItem)
{
  if (DebugAlwaysTrue(FProgressForm != NULL) && DebugAlwaysTrue(FProgressForm->SynchronizeProgress != NULL))
  {
    FProgressForm->SynchronizeProgress->ItemProcessed(ChecklistItem);
  }

  // Not set in keep-up-to-date mode - Synchronize() method
  if (Token != NULL)
  {
    TSynchronizeParams & Params = *static_cast<TSynchronizeParams *>(Token);
    if (Params.OnProcessedItem != NULL)
    {
      Params.OnProcessedItem(NULL, ChecklistItem);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFullSynchronize(
  void * Token, TProcessedSynchronizationChecklistItem OnProcessedItem,
  TUpdatedSynchronizationChecklistItems OnUpdatedSynchronizationChecklistItems)
{
  TSynchronizeParams & Params = *static_cast<TSynchronizeParams *>(Token);
  *Params.StartTime = Now();
  FullSynchronize(Params, OnProcessedItem, OnUpdatedSynchronizationChecklistItems);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::DoQueueSynchronize(void * Token)
{
  TSynchronizeParams & Params = *static_cast<TSynchronizeParams *>(Token);

  int CopyParams = TTerminal::GetSynchronizeCopyParams(Params.Params);
  TCopyParamType SyncCopyParam = TTerminal::GetSynchronizeCopyParam(Params.CopyParam, Params.Params);

  bool Parallel = GUIConfiguration->CurrentCopyParam.QueueParallel;

  int Index = 0;
  const TSynchronizeChecklist::TItem * ChecklistItem;
  while (Params.Checklist->GetNextChecked(Index, ChecklistItem))
  {
    TQueueItem * QueueItem = Terminal->SynchronizeToQueue(ChecklistItem, &SyncCopyParam, CopyParams, Parallel);
    AddQueueItem(Queue, QueueItem, Terminal);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronizeChecklistCalculateSize(
  TSynchronizeChecklist * Checklist, const TSynchronizeChecklist::TItemList & Items, void * Token)
{
  // terminal can be already closed (e.g. dropped connection)
  if (Terminal != NULL)
  {
    TSynchronizeParams & Params = *static_cast<TSynchronizeParams *>(Token);
    Terminal->SynchronizeChecklistCalculateSize(Checklist, Items, Params.CopyParam);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronizeMove(
  TOperationSide Side, TStrings * FileList, const UnicodeString & NewFileName, bool TargetIsDirectory, void * Token)
{
  DebugAssert(!IsLocalBrowserMode());
  TAutoBatch AutoBatch(this);
  TAutoFlag AutoOperationFlag(FAutoOperation);

  TSynchronizeParams & Params = *static_cast<TSynchronizeParams *>(Token);

  if (Side == osRemote)
  {
    UnicodeString Target;
    UnicodeString FileMask;
    if (TargetIsDirectory)
    {
      Target = NewFileName;
      Terminal->CreateTargetDirectory(Target, faDirectory, Params.CopyParam);
    }
    else
    {
      Target = UnixExtractFileDir(NewFileName);
      FileMask = DelimitFileNameMask(UnixExtractFileName(NewFileName));
    }

    RemoteDirView->SaveSelection();
    RemoteDirView->SaveSelectedNames();
    try
    {
      Terminal->MoveFiles(FileList, Target, FileMask, false);
    }
    catch(...)
    {
      RemoteDirView->DiscardSavedSelection();
      throw;
    }
    RemoteDirView->RestoreSelection();
  }
  else if (DebugAlwaysTrue(Side == osLocal))
  {
    std::unique_ptr<TStrings> Directories(CreateSortedStringList());
    if (TargetIsDirectory)
    {
      if (!ForceDirectories(ApiPath(NewFileName)))
      {
        throw EOSExtException(FMTLOAD(CREATE_DIR_ERROR, (NewFileName)));
      }
      Directories->Add(NewFileName);
    }
    else
    {
      Directories->Add(ExtractFileDir(NewFileName));
    }

    for (int Index = 0; Index < FileList->Count; Index++)
    {
      UnicodeString FileName = FileList->Strings[Index];
      UnicodeString Directory = ExtractFileDir(FileName);
      Directories->Add(Directory);
      UnicodeString NewFilePath = NewFileName;
      if (TargetIsDirectory)
      {
        NewFilePath = TPath::Combine(NewFilePath, ExtractFileName(FileName));
      }
      if (!MoveFile(ApiPath(FileName).c_str(), ApiPath(NewFilePath).c_str()))
      {
        throw EOSExtException(FMTLOAD(RENAME_FILE_ERROR, (FileName, NewFilePath)));
      }
    }

    for (int Index = 0; Index < Directories->Count; Index++)
    {
      ReloadLocalDirectory(Directories->Strings[Index]);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSynchronizeBrowse(TOperationSide Side, TSynchronizeChecklist::TAction Action, const TSynchronizeChecklist::TItem * Item)
{
  UnicodeString LocalPath = ExcludeTrailingBackslash(Item->Local.Directory);
  if (Side == osLocal)
  {
    if (Action == TSynchronizeChecklist::saDownloadNew)
    {
      OpenFolderInExplorer(LocalPath);
    }
    else
    {
      OpenFileInExplorer(CombinePaths(LocalPath, Item->GetFileName()));
    }
  }
  else if (DebugAlwaysTrue(Side == osRemote))
  {
    // Similar to CreateHiddenDuplicateSession, except that it modifies the initial directories
    std::unique_ptr<TSessionData> SessionData(CloneCurrentSessionData());
    SessionData->RemoteDirectory = UnixExcludeTrailingBackslash(Item->Remote.Directory);
    if (!LocalPath.IsEmpty())
    {
      SessionData->LocalDirectory = LocalPath;
    }

    UnicodeString SessionName = SaveHiddenDuplicateSession(SessionData.get());
    ExecuteNewInstance(SessionName, FORMAT(L"%s=\"%s\"", (TProgramParams::FormatSwitch(BROWSE_SWITCH), Item->GetFileName())));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FullSynchronizeInNewWindow(
  TSynchronizeMode Mode, int Params, const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
   const TCopyParamType * CopyParams)
{
  UnicodeString SessionName = CreateHiddenDuplicateSession();

  UnicodeString AdditionalParams =
    FORMAT(L"%s%s %s // \"%s\" \"%s\" %d %d", (
      TProgramParams::FormatSwitch(DEFAULTS_SWITCH),
      SerializeCopyParamForCommandLine(CopyParams),
      TProgramParams::FormatSwitch(SYNCHRONIZE_SWITCH),
      LocalDirectory, RemoteDirectory, Mode, Params));

  ExecuteNewInstance(SessionName, AdditionalParams);
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::DoFullSynchronizeDirectories(
  UnicodeString & LocalDirectory, UnicodeString & RemoteDirectory,
  TSynchronizeMode & Mode, int Params, bool & SaveMode, int UseDefaults)
{
  int Result;

  bool SaveSettings = false;
  int Options =
    FLAGMASK(!Terminal->IsCapable[fcTimestampChanging], fsoDisableTimestamp) |
    FLAGMASK(!CanCalculateChecksum(), fsoDisableByChecksum) |
    FLAGMASK(SynchronizeAllowSelectedOnly(), fsoAllowSelectedOnly);
  TCopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  TUsableCopyParamAttrs CopyParamAttrs = Terminal->UsableCopyParamAttrs(0);
  bool Continue =
    ((UseDefaults == 0) ||
     DoFullSynchronizeDialog(Mode, Params, LocalDirectory, RemoteDirectory,
       &CopyParam, SaveSettings, SaveMode, Options, CopyParamAttrs, FullSynchronizeInNewWindow, UseDefaults)) &&
    (FLAGCLEAR(Params, TTerminal::spByChecksum) || EnsureCommandSessionFallback(fcCalculatingChecksum));
  if (Continue)
  {
    Configuration->Usage->Inc(L"Synchronizations");
    CopyParam.IncludeFileMask.SetRoots(LocalDirectory, RemoteDirectory);
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
      DebugAssert(!FAutoOperation);
      FAutoOperation = true;

      try
      {
        UnicodeString SessionKey = Terminal->SessionData->SessionKey;
        std::unique_ptr<TStrings> DataList(Configuration->LoadDirectoryStatisticsCache(SessionKey, RemoteDirectory, CopyParam));

        int Files = -1;
        if (DataList->Count >= 1)
        {
          Files = StrToIntDef(DataList->Strings[0], Files);
        }
        else
        {
          DataList->Add(UnicodeString());
        }

        FSynchronizeProgressForm = new TSynchronizeProgressForm(Application, true, Files);
        FSynchronizeProgressForm->Start();

        Checklist = Terminal->SynchronizeCollect(LocalDirectory, RemoteDirectory,
          static_cast<TTerminal::TSynchronizeMode>(Mode),
          &CopyParam, Params | TTerminal::spNoConfirmation, TerminalSynchronizeDirectory,
          &SynchronizeOptions);

        if (Terminal->SessionData->CacheDirectories)
        {
          DataList->Strings[0] = IntToStr(SynchronizeOptions.Files);
          Configuration->SaveDirectoryStatisticsCache(SessionKey, RemoteDirectory, CopyParam, DataList.get());
        }
      }
      __finally
      {
        FAutoOperation = false;
        SetTaskbarListProgressState(TBPF_NOPROGRESS);
        SAFE_DESTROY(FSynchronizeProgressForm);
      }

      TSynchronizeParams SynchronizeParams;
      SynchronizeParams.LocalDirectory = LocalDirectory;
      SynchronizeParams.RemoteDirectory = RemoteDirectory;
      SynchronizeParams.Mode = Mode;
      SynchronizeParams.CopyParam = &CopyParam;
      SynchronizeParams.Params = Params;
      SynchronizeParams.Checklist = Checklist;
      SynchronizeParams.CollectElapsed = (Now() - StartTime);
      SynchronizeParams.StartTime = &StartTime;
      SynchronizeParams.OnProcessedItem = NULL;
      Result = Checklist->Count;
      if (Checklist->Count > 0)
      {
        if (FLAGSET(Params, TTerminal::spPreviewChanges))
        {
          TQueueSynchronizeEvent OnQueueSynchronize = NULL;
          if (Visible && FLAGCLEAR(Params, TTerminal::spTimestamp))
          {
            OnQueueSynchronize = DoQueueSynchronize;
          }
          if (!DoSynchronizeChecklistDialog(
                Checklist, Mode, Params, LocalDirectory, RemoteDirectory, CustomCommandMenu, DoFullSynchronize,
                OnQueueSynchronize, DoSynchronizeChecklistCalculateSize, DoSynchronizeMove, DoSynchronizeBrowse,
                &SynchronizeParams))
          {
            Result = -1;
          }
        }
        else
        {
          FullSynchronize(SynchronizeParams, NULL, NULL);
        }
      }
      else
      {
        UnicodeString Message = MainInstructions(LoadStr(COMPARE_NO_DIFFERENCES));
        MessageDialog(Message, qtInformation, qaOK, HELP_SYNCHRONIZE_NO_DIFFERENCES);
      }
    }
    __finally
    {
      delete Checklist;
    }

    OperationComplete(StartTime);
  }
  else
  {
    Result = -1;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalSynchronizeDirectory(
  const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory,
  bool & Continue, bool Collect, const TSynchronizeOptions * Options)
{
  if (Collect)
  {
    DebugAssert(FSynchronizeProgressForm != NULL);
    if (!FSynchronizeProgressForm->Started)
    {
      FSynchronizeProgressForm->Start();
    }
    int CompareProgress = FSynchronizeProgressForm->SetData(LocalDirectory, RemoteDirectory, Options->Files, Continue);
    SetTaskbarListProgressValue(CompareProgress);
  }
  else
  {
    DebugAssert(FSynchronizeProgressForm == NULL);
    DebugAssert(FProgressForm != NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StandaloneEdit(const UnicodeString & FileName)
{
  UnicodeString FullFileName = AbsolutePath(Terminal->CurrentDirectory, FileName);

  std::unique_ptr<TRemoteFile> File(Terminal->ReadFile(FullFileName));
  if (File.get() != NULL)
  {
    ExecuteRemoteFile(FullFileName, File.get(), efInternalEditor);

    Application->ShowMainForm = false;

    Application->Run();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExploreLocalDirectory(TOperationSide)
{
  DebugFail();
}
//---------------------------------------------------------------------------
TSessionData * __fastcall TCustomScpExplorerForm::CloneCurrentSessionData()
{
  std::unique_ptr<TSessionData> SessionData(new TSessionData(L""));
  SessionData->Assign(ManagedSession->SessionData);
  UpdateSessionData(SessionData.get());
  DebugAssert(SessionData->IsLocalBrowser == IsLocalBrowserMode());
  if (IsActiveTerminal(ManagedSession))
  {
    ManagedSession->UpdateSessionCredentials(SessionData.get());
  }
  if (!ManagedSession->SessionData->HasSessionName())
  {
    // Particularly for "Workspace/XXXX" name, we need to reset it, as it would become user-visible
    // once IsWorkspace is cleared
    SessionData->Name = UnicodeString();
  }
  // Most uses of this method ends with saving the session, and we do not want to store the IsWorkspace flag.
  // Particularly, with SaveHiddenDuplicateSession, it won't work as TSessionData::ParseUrl explicitly ignored workspace sessions.
  SessionData->IsWorkspace = false;
  return SessionData.release();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SaveCurrentSession()
{
  TSessionData * SessionData = CloneCurrentSessionData();
  try
  {
    TSessionData * EditingSessionData = StoredSessions->FindSame(SessionData);

    DoSaveSession(SessionData, EditingSessionData, true, NULL);
  }
  __finally
  {
    delete SessionData;
  }
}
//---------------------------------------------------------------------------
TObjectList * __fastcall TCustomScpExplorerForm::DoCollectWorkspace()
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  std::unique_ptr<TObjectList> DataList(new TObjectList());

  if (DebugAlwaysTrue(ManagedSession != NULL))
  {
    // Update ManagedSession->StateData
    UpdateSession(ManagedSession);
  }
  Manager->SaveWorkspace(DataList.get());

  return DataList.release();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoSaveWorkspace(
  const UnicodeString & Name, TObjectList * DataList, bool SavePasswords, bool Explicit)
{
  WinConfiguration->LastWorkspace = Name;

  if (!SavePasswords)
  {
    for (int Index = 0; Index < DataList->Count; Index++)
    {
      TSessionData * SessionData = dynamic_cast<TSessionData *>(DataList->Items[Index]);

      if (SessionData->Link.IsEmpty())
      {
        SessionData->ClearSessionPasswords();
      }
    }
  }

  StoredSessions->NewWorkspace(Name, DataList);
  // modified only
  StoredSessions->Save(false, Explicit);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::WorkspaceName()
{
  return
    DefaultStr(WinConfiguration->LastWorkspace,
      DefaultStr(WinConfiguration->AutoWorkspace, LoadStr(NEW_WORKSPACE)));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::SaveWorkspace(bool EnableAutoSave)
{

  std::unique_ptr<TObjectList> DataList(DoCollectWorkspace());

  bool AnyNonStoredSessionWithPassword = false;
  bool AnyNonStoredNonWorkspaceSessionWithPassword = false;
  bool AllNonStoredSessionsAnonymous = true;

  TTerminalManager * Manager = TTerminalManager::Instance();
  for (int Index = 0; Index < DataList->Count; Index++)
  {
    TSessionData * SessionData = dynamic_cast<TSessionData *>(DataList->Items[Index]);

    if (SessionData->Link.IsEmpty())
    {
      if (SessionData->HasAnySessionPassword())
      {
        AnyNonStoredSessionWithPassword = true;
        if (!Manager->Sessions[Index]->SessionData->IsWorkspace)
        {
          AnyNonStoredNonWorkspaceSessionWithPassword = true;
        }
      }

      if (!SameText(SessionData->UserName, AnonymousUserName))
      {
        AllNonStoredSessionsAnonymous = false;
      }
    }
  }

  bool SavePasswords;
  bool * PSavePasswords;
  bool NotRecommendedSavingPasswords =
    !CustomWinConfiguration->UseMasterPassword &&
    !AllNonStoredSessionsAnonymous;

  if (Configuration->DisablePasswordStoring ||
      !AnyNonStoredSessionWithPassword)
  {
    PSavePasswords = NULL;
    SavePasswords = false;
  }
  else
  {
    PSavePasswords = &SavePasswords;
    SavePasswords =
      !AnyNonStoredNonWorkspaceSessionWithPassword ||
      !NotRecommendedSavingPasswords;
  }

  UnicodeString Name = WorkspaceName();
  bool CreateShortcut = false;

  bool Result =
    DoSaveWorkspaceDialog(
      Name, PSavePasswords, NotRecommendedSavingPasswords, CreateShortcut,
      EnableAutoSave);

  if (Result)
  {
    DoSaveWorkspace(Name, DataList.get(), SavePasswords, true);

    if (CreateShortcut)
    {
      TOperationVisualizer Visualizer;
      UnicodeString AdditionalParams = TProgramParams::FormatSwitch(DESKTOP_SWITCH);
      CreateDesktopSessionShortCut(Name, L"", AdditionalParams, -1, WORKSPACE_ICON);
    }

    if (EnableAutoSave)
    {
      WinConfiguration->AutoSaveWorkspace = true;
      WinConfiguration->AutoWorkspace = Name;
      if (PSavePasswords != NULL)
      {
        WinConfiguration->AutoSaveWorkspacePasswords = SavePasswords;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSession(TManagedTerminal * Session)
{
  DebugAssert(IsLocalBrowserMode() == Session->LocalBrowser);
  DebugAssert(Session == ManagedSession);
  if (!IsLocalBrowserMode())
  {
    SAFE_DESTROY(Session->RemoteExplorerState);

    if (WinConfiguration->PreservePanelState)
    {
      Session->RemoteExplorerState = RemoteDirView->SaveState();
    }
  }
  UpdateSessionData(Session->StateData);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionData(TSessionData * Data)
{
  // Keep in sync with TSessionData::CopyStateData

  DebugAssert(Data != NULL);

  // This is inconsistent with how color (for example) is handled.
  Data->Name = ManagedSession->SessionData->Name;

  if (!IsLocalBrowserMode())
  {
    // cannot use RemoteDirView->Path, because it is empty if connection
    // was already closed
    // also only peek, we may not be connected at all atm
    // so make sure we do not try retrieving current directory from the server
    // (particularly with FTP)
    UnicodeString ACurrentDirectory = Terminal->PeekCurrentDirectory();
    if (!ACurrentDirectory.IsEmpty())
    {
      Data->RemoteDirectory = ACurrentDirectory;
    }
  }
  Data->Color = SessionColor;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToolBarResize(TObject *Sender)
{
  TTBXToolbar * Toolbar = dynamic_cast<TTBXToolbar*>(Sender);
  DebugAssert(Toolbar != NULL);

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
      TControl * Control = this;
      ASize.x += ScaleByTextHeight(Control, 50) /* minimal combo width */;
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
      DebugAssert(RequiredSpace >= 0);
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
  // we should better use TCustomDirView::FCaseSensitive, but it is private
  TStringList * VisitedDirectories = CreateSortedStringList(!IsSideLocalBrowser(Side));
  try
  {
    TCustomDirView * DView = DirView(Side);

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
    Params.NeverAskAgainTitle = LoadStr(ADD_BOOKMARK_SHARED);
    Params.NeverAskAgainCheckedInitially = WinConfiguration->UseSharedBookmarks;

    UnicodeString Message = MainInstructions(FMTLOAD(ADD_BOOKMARK_CONFIRM, (DirView(Side)->PathName)));
    unsigned int Answer =
      MessageDialog(Message,
        qtConfirmation, qaOK | qaCancel, HELP_ADD_BOOKMARK_CONFIRM, &Params);
    if (Answer == qaNeverAskAgain)
    {
      Continue = true;
      WinConfiguration->UseSharedBookmarks = true;
    }
    else if (Answer == qaOK)
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
      bool Repeat;
      do
      {
        Repeat = false;
        TOperationSide BookmarkSide = IsSideLocalBrowser(Side) ? osLocal : osRemote;
        if (::DoOpenDirectoryDialog(Mode, BookmarkSide, Name, VisitedDirectories, Terminal,
              // do not allow switching to location profiles,
              // if we are not connected
              HasDirView[osLocal] && IsActiveTerminal(Terminal)))
        {
          TWindowLock Lock(this);
          if (!TryOpenDirectory(Side, Name))
          {
            Repeat = true;
            Mode = odBrowse;
          }
        }
      }
      while (Repeat);
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
  DebugAssert(!Terminal->CommandSessionOpened);

  return TTerminalManager::Instance()->ConnectTerminal(Terminal->CommandSession);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::EnsureCommandSessionFallback(TFSCapability Capability)
{
  bool Result = Terminal->IsCapable[Capability] ||
    Terminal->CommandSessionOpened;

  if (!Result)
  {
    // We might get here for example when "checksum" synchronization is requested from command-line
    // on server that supports neither checksum nor shell
    if (!Terminal->IsCapable[fcSecondaryShell])
    {
      NotSupported();
    }
    else if (!GUIConfiguration->ConfirmCommandSession)
    {
      Result = true;
    }
    else
    {
      TMessageParams Params(mpNeverAskAgainCheck);
      const TFileSystemInfo & FileSystemInfo = Terminal->GetFileSystemInfo();
      unsigned int Answer = MessageDialog(FMTLOAD(PERFORM_ON_COMMAND_SESSION2,
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
  if (!IsSideLocalBrowser(osRemote) &&
      IsFileControl(DropSourceControl, osRemote) &&
      (FDDExtMapFile != NULL))
  {
    Accept = true;
  }

  FDDTargetControl = dynamic_cast<TControl*>(Sender);
  DebugAssert(FDDTargetControl != NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDDragEnter(
  _di_IDataObject DataObj, int KeyState,
  const TPoint & Point, int & Effect, bool & Accept)
{
  FileControlDDDragEnter(SessionsPageControl, DataObj, KeyState, Point, Effect, Accept);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueDDDragEnter(
  _di_IDataObject DataObj, int KeyState,
  const TPoint & Point, int & Effect, bool & Accept)
{
  FileControlDDDragEnter(QueueView3, DataObj, KeyState, Point, Effect, Accept);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileControlDDDragLeave(
  TObject * Sender)
{
  DebugUsedParam(Sender);
  DebugAssert(FDDTargetControl == Sender);
  FDDTargetControl = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDDragLeave(int)
{
  FileControlDDDragLeave(SessionsPageControl);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueDDDragLeave(int)
{
  FileControlDDDragLeave(QueueView3);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddEditLink(TOperationSide Side, bool Add)
{
  DebugAssert(!IsSideLocalBrowser(Side));
  DebugUsedParam(Side);

  bool Edit = false;
  TRemoteFile * File = NULL;
  UnicodeString FileName;
  UnicodeString PointTo;
  bool SymbolicLink = true;

  if (RemoteDirView->ItemFocused)
  {
    DebugAssert(RemoteDirView->ItemFocused->Data);
    File = (TRemoteFile *)RemoteDirView->ItemFocused->Data;

    Edit = !Add && File->IsSymLink && Terminal->SessionData->ResolveSymlinks;
    if (Edit)
    {
      FileName = File->FileName;
      PointTo = File->LinkTo;
    }
    else if (!File->IsParentDirectory)
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
      DebugAssert(File->FileName == FileName);
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
bool __fastcall TCustomScpExplorerForm::CanAddEditLink(TOperationSide Side)
{
  return
    (IsSideLocalBrowser(Side) ||
     (HasActiveTerminal() &&
      Terminal->ResolvingSymlinks &&
      Terminal->IsCapable[fcSymbolicLink]));
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::LinkFocused()
{
  return
    !IsSideLocalBrowser(FCurrentSide) &&
    (RemoteDirView->ItemFocused != NULL) &&
    ((TRemoteFile *)RemoteDirView->ItemFocused->Data)->IsSymLink &&
    Terminal->SessionData->ResolveSymlinks;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteCurrentFile()
{
  DebugAssert(!WinConfiguration->DisableOpenEdit);
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
void __fastcall TCustomScpExplorerForm::ExecuteCurrentFileWith(bool OnFocused)
{
  TEditorData ExternalEditor;
  ExternalEditor.Editor = edExternal;
  bool Remember = false;

  if (DoEditorPreferencesDialog(&ExternalEditor, Remember, epmAdHoc, !IsSideLocalBrowser(osCurrent)))
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

    ExecuteFile(osCurrent, efExternalEditor, &ExternalEditor, true, OnFocused);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DetachTerminal(TObject * ATerminal)
{
  if (FFileFindTerminal == ATerminal)
  {
    FFileFindTerminal = NULL;
    HideFileFindDialog();
  }

  if (FClipboardTerminal == ATerminal)
  {
    ClipboardClear(); // implies ClipboardStop
  }

  if ((FOnSynchronizeAbort != NULL) &&
      (ATerminal == FManagedSession))
  {
    FOnSynchronizeAbort(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TerminalRemoved(TObject * Sender)
{
  FEditorManager->ProcessFiles(FileTerminalRemoved, Sender);
  DetachTerminal(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileTerminalReplaced(
  const UnicodeString FileName, TEditedFileData * Data, TObject * /*Token*/, void * Arg)
{
  TManagedTerminal * ATerminal = static_cast<TManagedTerminal *>(Arg);

  if (Data->Terminal == Terminal)
  {
    Data->Terminal = ATerminal;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileTerminalRemoved(const UnicodeString FileName,
  TEditedFileData * Data, TObject * /*Token*/, void * Arg)
{
  TTerminal * Terminal = static_cast<TTerminal *>(Arg);
  DebugAssert(Terminal != NULL);

  if (Data->Terminal == Terminal)
  {
    Data->Terminal = NULL;
  }
}
//---------------------------------------------------------------------------
TManagedTerminal * TCustomScpExplorerForm::GetReplacementForLastSession()
{
  return NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LastTerminalClosed()
{
  UpdateControls();
  SessionColor = TColor(0);
  UpdateRemotePathComboBox(false);
  NeedSession(false);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::NeedSession(bool Startup)
{
  try
  {
    // Cache, as the login dialog can change its value.
    // See also DoShow. Also see TTerminalManager::NewSession.
    bool ShowLogin = WinConfiguration->ShowLoginWhenNoSession;
    try
    {
      if (ShowLogin)
      {
        bool ReloadSessions = !Startup;
        TTerminalManager::Instance()->NewSession(L"", ReloadSessions, this, true);
      }
    }
    __finally
    {
      // Do not terminate, if we are only starting up and we are not showing Login dialog
      // (so there was no chance for the user to open any session yet)
      if ((ShowLogin || !Startup) &&
          !WinConfiguration->KeepOpenWhenNoSession &&
          // this rules out the implicit Commander's local-local browser
          ((Terminal == NULL) || (!Terminal->Active && !Terminal->Permanent)))
      {
        TerminateApplication();
      }
    }
  }
  catch (EAbort &)
  {
    // swallow
    // The TTerminalManager does not expect the OnLastTerminalClose to throw without trying to connect.
    // Also when called from TWinControl.UpdateShowing => CMShowingChanged, the showing is aborted on exception
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionListChanged(bool ForceTruncationUpdate)
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  DebugAssert(!Manager->Updating);
  TAutoFlag UpdatingSessionTabsFlag(FUpdatingSessionTabs);
  int ActiveSessionIndex = Manager->ActiveSessionIndex;

  Configuration->Usage->SetMax(L"MaxOpenedSessions", Manager->Count);

  SessionsPageControl->LockDrawing();
  try
  {
    int PrevTabsWidth = SessionsPageControl->TotalTabsWidth();
    while (SessionsPageControl->PageCount > Manager->Count + 1)
    {
      delete SessionsPageControl->Pages[SessionsPageControl->PageCount - 1];
    }

    for (int Index = 0; Index <= Manager->Count; Index++)
    {
      TThemeTabSheet * TabSheet;
      if (Index >= SessionsPageControl->PageCount)
      {
        TabSheet = new TThemeTabSheet(SessionsPageControl);
        TabSheet->PageControl = SessionsPageControl;
      }
      else
      {
        TabSheet = SessionsPageControl->Pages[Index];
      }

      bool IsSessionTab = (Index < Manager->Count);
      if (IsSessionTab)
      {
        TTerminal * Terminal = Manager->Sessions[Index];
        TabSheet->Tag = reinterpret_cast<int>(Terminal);
        TabSheet->CaptionTruncation = WinConfiguration->SessionTabCaptionTruncation ? tttEllipsis : tttNone;

        UpdateSessionTab(TabSheet);
      }
      else
      {
        TabSheet->Tag = 0; // not really needed
        TabSheet->Shadowed = false;
        TabSheet->Button = SupportsLocalBrowser() ? ttbDropDown : ttbNone;
        TabSheet->CaptionTruncation = GetNewTabTabCaptionTruncation();
        // We know that we are at the last page, otherwise we could not call this (it assumes that new session tab is the last one)
        UpdateNewTabTab();
      }
    }

    // Prevent flicker on the right side where scrolling buttons would be, unless really needed
    if (ForceTruncationUpdate || (SessionsPageControl->TotalTabsWidth() != PrevTabsWidth))
    {
      SessionsPageControl->UpdateTabsCaptionTruncation();
    }
  }
  __finally
  {
    SessionsPageControl->UnlockDrawing();
  }

  SessionsPageControl->ActivePageIndex = ActiveSessionIndex;
}
//---------------------------------------------------------------------------
TThemeTabCaptionTruncation TCustomScpExplorerForm::GetNewTabTabCaptionTruncation()
{
  return WinConfiguration->SessionTabCaptionTruncation ? tttNoText : tttNone;
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetNewTabTabCaption()
{
  return StripHotkey(StripTrailingPunctuation(NonVisualDataModule->NewTabAction->Caption));
}
//---------------------------------------------------------------------------
TThemeTabSheet * TCustomScpExplorerForm::GetNewTabTab()
{
  return SessionsPageControl->Pages[SessionsPageControl->PageCount - 1];
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateNewTabTab()
{
  TThemeTabSheet * TabSheet = GetNewTabTab();

  UnicodeString TabCaption;
  if (WinConfiguration->SelectiveToolbarText)
  {
    TabCaption = GetNewTabTabCaption();
  }
  TabSheet->BaseCaption = TabCaption;
  TabSheet->ImageIndex = GetNewTabTabImageIndex(osCurrent);
}
//---------------------------------------------------------------------------
TManagedTerminal * __fastcall TCustomScpExplorerForm::GetSessionTabSession(TTabSheet * TabSheet)
{
  return reinterpret_cast<TManagedTerminal *>(TabSheet->Tag);
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetLocalBrowserSessionTitle(TManagedTerminal *)
{
  DebugFail();
  return UnicodeString();
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::UpdateSessionTab(TThemeTabSheet * TabSheet)
{
  if (DebugAlwaysTrue(TabSheet != NULL))
  {
    TManagedTerminal * ASession = GetSessionTabSession(TabSheet);
    if (DebugAlwaysTrue(ASession != NULL))
    {
      TColor Color = (ASession == Terminal) ? FSessionColor : ASession->StateData->Color;
      if (ASession->LocalBrowser)
      {
        TabSheet->ImageIndex = FLocalBrowserTabImageIndex;
      }
      else
      {
        TabSheet->ImageIndex = AddSessionColor(Color);
      }

      TTerminalManager * Manager = TTerminalManager::Instance();

      TabSheet->Shadowed = !ASession->Active && !ASession->LocalBrowser;
      TabSheet->Button = CanCloseSession(ASession) ? ttbClose : ttbNone;
      TabSheet->BaseCaption = Manager->GetSessionTitle(ASession, true);
    }
  }
}
//---------------------------------------------------------------------------
bool TCustomScpExplorerForm::CanCloseSession(TManagedTerminal * Session)
{
  return !Session->LocalBrowser || (TTerminalManager::Instance()->Count > 1);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::SessionTabSwitched()
{
  DebugAssert(SessionsPageControl->ActivePage != NULL);
  TManagedTerminal * Session = GetSessionTabSession(SessionsPageControl->ActivePage);
  bool Result = (Session != NULL);
  if (Result)
  {
    TTerminalManager::Instance()->ActiveSession = Session;
  }
  else
  {
    try
    {
      NewTab();
    }
    __finally
    {
      SessionListChanged();
    }

    FSessionsPageControlNewTabTime = Now();
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlChange(TObject * /*Sender*/)
{
  TWindowLock Lock(this);
  SessionTabSwitched();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferListChange(TObject * Sender)
{
  TTBXStringList * TransferList = dynamic_cast<TTBXStringList *>(Sender);
  DebugAssert(TransferList != NULL);
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
  DebugAssert(ExistingPreset);
  if (ExistingPreset)
  {
    HDC DC = GetDC(0);
    TCanvas * Canvas = new TCanvas();
    try
    {
      Canvas->Handle = DC;
      Canvas->Font = GetToolbarFont(this);

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
            FLAGMASK(HasActiveTerminal(), Terminal->UsableCopyParamAttrs(0).General));
      int MaxWidth = TransferList->MinWidth - (2 * TransferLabel->Margin) - ScaleByTextHeight(this, 10);
      if (Canvas->TextExtent(InfoStr).cx > MaxWidth)
      {
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
    if (Command == APPCOMMAND_BROWSER_FAVORITES)
    {
      if (!NonVisualDataModule->Busy)
      {
        OpenDirectory(GetSide(osCurrent));
      }
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
void __fastcall TCustomScpExplorerForm::CMDialogChar(TMessage & AMessage)
{
  TCMDialogChar & Message = reinterpret_cast<TCMDialogChar &>(AMessage);
  if ((FIgnoreNextDialogChar != 0) &&
      (towupper(Message.CharCode) == towupper(FIgnoreNextDialogChar)))
  {
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(&Message);
  }
  FIgnoreNextDialogChar = 0;
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
void __fastcall TCustomScpExplorerForm::WMQueryEndSession(TMessage & Message)
{
  AppLog(L"Windows session end query");
  // We were actually never able to make ENDSESSION_CRITICAL happen.
  // Also there no point returning TRUE as we are not able to
  // handle the abrupt termination caused by subsequent WM_ENDSESSION cleanly.
  // Hence the process termination might be safer :)
  if ((Message.LParam != ENDSESSION_CRITICAL) &&
      (((FQueue != NULL) && !FQueue->IsEmpty) || (FProgressForm != NULL)))
  {
    Message.Result = FALSE;
  }
  else
  {
    Message.Result = TRUE;
  }
  // Do not call default handling as that triggers OnCloseQuery,
  // where our implementation will popup confirmation dialogs, what we do not want,
  // as per Vista guidelines:
  // https://learn.microsoft.com/en-us/windows/win32/shutdown/shutting-down
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMEndSession(TWMEndSession & Message)
{
  AppLog(L"Windows session end");
  if (Message.EndSession && IsApplicationMinimized())
  {
    // WORKAROUND
    // TApplication.WndProc() calls Application.Terminate() before Halt(),
    // when it receives WM_ENDSESSION,
    // but that sometimes (particularly when application is minimized) cause crashes.
    // Crash popups message that blocks log off.
    // Obviously application cannot shutdown cleanly after WM_ENDSESSION,
    // so we call ExitProcess() immediately, not even trying to cleanup.
    // It still causes beep, so there's likely some popup, but it does not block
    // log off.
    ExitProcess(0);
  }
  TForm::Dispatch(&Message);
}
#ifdef _DEBUG
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMWindowPosChanged(TWMWindowPosMsg & Message)
{
  TForm::Dispatch(&Message);
}
#endif
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SysResizing(unsigned int /*Cmd*/)
{
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoShow()
{
  // Called from TCustomScpExplorerForm::CMShowingChanged
  // only now are the controls resized finally, so the size constraints
  // will not conflict with possibly very small window size
  RestoreFormParams();
  // This indicates that the form was rescaled after parameters were loaded and
  // as most probably the parameters were actually saved with the current form scaling
  // (windows was the last time likely closed on the same monitor, where it is starting now),
  // rescaling happened twice already. Reload the parameters, as now they likely won't need any scaling
  // and no rounding problems will occur.
  // See also RestoreForm.
  // (we should be safe to call RestoreParams unconditionally)
  if (PixelsPerInch != Screen->PixelsPerInch)
  {
    RestoreParams();
  }

  FixControlsPlacement();

  if (Position == poDefaultPosOnly)
  {
    CutFormToDesktop(this);
  }

  TForm::DoShow();

  FSessionsDragDropFilesEx->DragDropControl = SessionsPageControl;
  FQueueDragDropFilesEx->DragDropControl = QueueView3;

  // This was previously in NeedSession, where other related code is.
  // But that is called from CMShowingChanged, when the window is already visible and the local-local window state
  // unpleasantly shows briefly.
  if ((ManagedSession == NULL) &&
      !WinConfiguration->ShowLoginWhenNoSession &&
      WinConfiguration->AutoSaveWorkspace && !WinConfiguration->AutoWorkspace.IsEmpty() &&
      // This detects if workspace was saved the last time the main widow was closed
      SameText(WinConfiguration->LastStoredSession, WinConfiguration->AutoWorkspace))
  {
    DoOpenFolderOrWorkspace(WinConfiguration->AutoWorkspace, false, true);
    Configuration->Usage->Inc(L"OpenedWorkspacesAuto");
  }

  // Before main window displays
  if (ManagedSession == NULL)
  {
    StartingWithoutSession();
  }

  UpdatePixelsPerInchMainWindowCounter();

  FShowing = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdatePixelsPerInchMainWindowCounter()
{
  Configuration->Usage->Set(L"PixelsPerInchMainWindow", PixelsPerInch);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StartingWithoutSession()
{
  SessionListChanged();
  InitStatusBar();
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PopupTrayBalloon(TTerminal * Terminal,
  const UnicodeString & Str, TQueryType Type, Exception * E, unsigned int Seconds,
  TNotifyEvent OnBalloonClick, TObject * UserData)
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
    UnicodeString Title;
    if ((Terminal == NULL) && (Type == qtInformation) && ExtractMainInstructions(Message, Title))
    {
      Message = TrimLeft(Message);
    }
    else
    {
      Message = UnformatMessage(Message);
      const ResourceString * Captions[] = { &_SMsgDlgConfirm, &_SMsgDlgWarning,
        &_SMsgDlgError, &_SMsgDlgInformation, NULL };
      Title = LoadResourceString(Captions[Type]);
      if (Terminal != NULL)
      {
        TManagedTerminal * Session = DebugNotNull(dynamic_cast<TManagedTerminal *>(Terminal->PrimaryTerminal));
        Title = TTerminalManager::Instance()->GetSessionTitle(Session, true) + TitleSeparator + Title;
      }
    }

    if (Seconds == 0)
    {
      Seconds = WinConfiguration->NotificationsTimeout;
    }
    FTrayIcon->PopupBalloon(Title, Message, Type, Seconds * MSecsPerSec, OnBalloonClick, UserData);
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall TCustomScpExplorerForm::MoreMessageDialog(const UnicodeString Message,
    TStrings * MoreMessages, TQueryType Type, unsigned int Answers,
    UnicodeString HelpKeyword, const TMessageParams * Params,
    TTerminal * Terminal)
{
  if (((WinConfiguration->ContinueOnError && (FErrorList != NULL)) ||
       (FOnFeedSynchronizeError != NULL)) &&
      (Params != NULL) && (Params->Params & mpAllowContinueOnError) )
  {
    if (FOnFeedSynchronizeError != NULL)
    {
      FOnFeedSynchronizeError(Message, MoreMessages, Type, HelpKeyword);
    }
    else
    {
      DebugAssert(FErrorList != NULL);
      TStringList * MoreMessagesCopy = NULL;
      if (MoreMessages)
      {
        MoreMessagesCopy = new TStringList();
        MoreMessagesCopy->Assign(MoreMessages);
      }
      FErrorList->AddObject(Message, MoreMessagesCopy);
    }

    PopupTrayBalloon(Terminal, Message, Type);

    return ContinueAnswer(Answers);
  }
  else
  {
    bool UseBalloon = FTrayIcon->Visible;
    if (UseBalloon)
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
      // cancel only balloon we popped up, otherwise we may cancel notification
      // balloon that was there before the message dialog
      // (such as "dd confirmation opt in balloon)
      if (UseBalloon)
      {
        FTrayIcon->CancelBalloon();
      }
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

  // particularly prevent opening new session from jump list,
  // while exception is shown
  NonVisualDataModule->StartBusy();
  try
  {
    ShowExtendedExceptionEx(Terminal, E);
  }
  __finally
  {
    NonVisualDataModule->EndBusy();
    FTrayIcon->CancelBalloon();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionReady()
{
  InitStatusBar();
  // cannot rely on active page being page for active terminal,
  // as it can happen that active page is the "new session" page
  // (e.g. when reconnecting active terminal, while login dialog
  // invoked from "new session" page is modal)
  int ActiveSessionIndex = TTerminalManager::Instance()->ActiveSessionIndex;
  UpdateSessionTab(SessionsPageControl->Pages[ActiveSessionIndex]);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::InactiveTerminalException(
  TTerminal * Terminal, Exception * E)
{
  Notify(Terminal, L"", qtError, false, NULL, NULL, E);

  if (!Terminal->Active)
  {
    int Index = TTerminalManager::Instance()->IndexOf(Terminal);
    if (DebugAlwaysTrue((Index >= 0) && (Index < SessionsPageControl->PageCount)))
    {
      UpdateSessionTab(SessionsPageControl->Pages[Index]);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Notify(TTerminal * Terminal,
  UnicodeString Message, TQueryType Type,
  bool Important, TNotifyEvent OnClick, TObject * UserData, Exception * E)
{
  if ((E == NULL) ||
      ExceptionMessage(E, Message))
  {
    unsigned int Seconds = WinConfiguration->NotificationsTimeout;
    if (Important)
    {
      Seconds *= 5;
    }

    UnicodeString NoteMessage(UnformatMessage(Message));
    if (Terminal != NULL)
    {
      TManagedTerminal * Session = DebugNotNull(dynamic_cast<TManagedTerminal *>(Terminal));
      NoteMessage = FORMAT(L"%s: %s",
        (TTerminalManager::Instance()->GetSessionTitle(Session, true), NoteMessage));
    }

    if (WinConfiguration->BalloonNotifications)
    {
      AddNote(NoteMessage);
      PopupTrayBalloon(Terminal, Message, Type, NULL, Seconds, OnClick, UserData);
    }
    else
    {
      FlashOnBackground();
      PostNote(NoteMessage, Seconds, OnClick, UserData);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueEmptyNoteClicked(TObject * Sender)
{
  RestoreApp();

  TTerminalNoteData * TerminalNoteData = dynamic_cast<TTerminalNoteData *>(Sender);
  if (DebugAlwaysTrue(TerminalNoteData != NULL) &&
      !NonVisualDataModule->Busy)
  {
    TManagedTerminal * Terminal = TerminalNoteData->Terminal;
    TTerminalManager::Instance()->ActiveSession = Terminal;
    if (!ComponentVisible[fcQueueView])
    {
      ToggleQueueVisibility();
      GoToQueue();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueEvent(TManagedTerminal * ATerminal,
  TTerminalQueue * /*Queue*/, TQueueEvent Event)
{
  UnicodeString Message;
  TNotifyEvent OnClick = NULL;
  TObject * UserData = NULL;
  bool QueueInvisible = !ComponentVisible[fcQueueView] || IsApplicationMinimized();
  switch (Event)
  {
    case qeEmptyButMonitored:
      if ((ATerminal != Terminal) || QueueInvisible)
      {
        Message = LoadStr(BALLOON_QUEUE_EMPTY);
        OnClick = QueueEmptyNoteClicked;
        TTerminalNoteData * TerminalNoteData = new TTerminalNoteData();
        TerminalNoteData->Terminal = ATerminal;
        UserData = TerminalNoteData;
      }
      break;

    case qeEmpty:
      OperationComplete(ATerminal->QueueOperationStart);
      break;

    case qePendingUserAction:
      if ((ATerminal != Terminal) ||
          (QueueInvisible && !IsQueueAutoPopup()))
      {
        Message = LoadStr(BALLOON_QUEUE_USER_ACTION);
      }
      break;

    default:
      DebugFail();
  }

  if (!Message.IsEmpty())
  {
    Notify(ATerminal, Message, qtInformation, false, OnClick, UserData);
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

  if (WinConfiguration->DDFakeFile)
  {
    DDFakeFileInitDrag(FileList, Created);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDFakeCreated(TObject * /*Sender*/, const UnicodeString FileName)
{
  if (DebugAlwaysTrue(!FDragFakeDirectory.IsEmpty()) &&
      SameText(ExtractFileName(FileName), ExtractFileName(FDragFakeDirectory)))
  {
    FFakeFileDropTarget = FileName;
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::CreateFakeTransferDirectory()
{
  UnicodeString Result = ExcludeTrailingBackslash(WinConfiguration->TemporaryDir());
  if (!ForceDirectories(ApiPath(Result)))
  {
    throw Exception(FMTLOAD(CREATE_TEMP_DIR_ERROR, (Result)));
  }
  FileSetAttr(ApiPath(Result), faHidden);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DDFakeFileInitDrag(TFileList * FileList,
  bool & Created)
{
  FFakeFileDropTarget = UnicodeString();
  FDragFakeDirectory = CreateFakeTransferDirectory();
  FileList->AddItem(NULL, FDragFakeDirectory);

  Created = true;

  if (!WinConfiguration->IsDDExtRunning() || WinConfiguration->IsDDExtBroken())
  {
    FDragFakeMonitors = StartCreationDirectoryMonitorsOnEachDrive(FILE_NOTIFY_CHANGE_DIR_NAME, DDFakeCreated);
  }

  FDDExtMapFile = CreateFileMappingA((HANDLE)0xFFFFFFFF, NULL, PAGE_READWRITE,
    0, sizeof(TDragExtCommStruct), AnsiString(DRAG_EXT_MAPPING).c_str());

  {
    TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
    TDragExtCommStruct* CommStruct;
    CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
      FILE_MAP_ALL_ACCESS, 0, 0, 0));
    DebugAssert(CommStruct != NULL);
    CommStruct->Version = TDragExtCommStruct::CurrentVersion;
    CommStruct->Dragging = true;
    wcsncpy(CommStruct->DropDest, FDragFakeDirectory.c_str(),
      LENOF(CommStruct->DropDest));
    NULL_TERMINATE(CommStruct->DropDest);
    UnmapViewOfFile(CommStruct);
  }

}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::RemoteFileControlFileOperation(
  TObject * Sender, TFileOperation Operation, bool NoConfirmation, void * Param)
{
  bool Result;
  DebugAssert(!IsLocalBrowserMode());
  if (Sender == RemoteDirView)
  {
    Result = ExecuteFileOperation(Operation, osRemote, true, NoConfirmation, Param);
  }
  else
  {
    DebugAssert(Sender == RemoteDriveView);
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
  DebugAssert(!IsLocalBrowserMode());
  // This also handles drops of remote files to queue.
  // Drops of local files (uploads) are handled in QueueDDProcessDropped.
  SAFE_DESTROY(FDDFileList);

  if (!FFakeFileDropTarget.IsEmpty())
  {
    RemoveDir(ApiPath(FFakeFileDropTarget));
  }

  if ((FDDExtMapFile != NULL) || (FDDTargetControl == QueueView3))
  {
    try
    {
      TDragResult DDResult = (Sender == RemoteDirView) ?
        RemoteDirView->LastDDResult : RemoteDriveView->LastDDResult;

      // Focus is moved to the target application,
      // but as we are going to present the UI, we need to steal the focus back.
      // This most likely won't work though (windows does not allow application
      // to steal focus most of the time)
      Application->BringToFront();

      // On older version of Windows we never got drMove here, see also comment below.
      // On Windows 10, we get the "move".
      if ((DDResult == drCopy) || (DDResult == drMove) || (DDResult == drInvalid))
      {
        UnicodeString TargetDirectory;
        TFileOperation Operation;

        // drInvalid may mean drMove, see comment below
        switch (DDResult)
        {
          case drCopy:
            Operation = foCopy;
            break;
          case drMove:
            Operation = foMove;
            break;
          default:
            DebugFail();
          case drInvalid:
            // prefer "copy" for safety
            Operation = FLAGSET(FLastDropEffect, DROPEFFECT_MOVE) ? foMove : foCopy;
            break;
        }

        TTransferOperationParam Param;
        UnicodeString CounterName;
        bool ForceQueue;
        if (!DDGetTarget(Param.TargetDirectory, ForceQueue, CounterName))
        {
          // we get drInvalid both if d&d was intercepted by ddext,
          // and when users drops on no-drop location.
          // we tell the difference by existence of response from ddext,
          // so we ignore absence of response in this case
          if (DDResult != drInvalid)
          {
            // here we know that the extension is installed,
            // as it is checked as soon as drag&drop starts from
            // RemoteFileControlDDCreateDragFileList
            Configuration->Usage->Inc(L"DownloadsDragDropExternalExtTargetUnknown");
            if (!WinConfiguration->DDExtInstalled || WinConfiguration->IsDDExtBroken())
            {
              throw ExtException(NULL, LoadStr(DD_TARGET_UNKNOWN), HELP_DD_TARGET_UNKNOWN);
            }
            else
            {
              throw ExtException(NULL, LoadStr(DRAGEXT_TARGET_UNKNOWN2), HELP_DRAGEXT_TARGET_UNKNOWN);
            }
          }
        }
        else
        {
          // download using fake file
          Param.Temp = false;
          Param.DragDrop = true;
          if (ForceQueue)
          {
            Param.Queue = asOn;
          }
          if (Sender == RemoteDirView)
          {
            Param.Options = FLAGMASK(SelectedAllFilesInDirView(RemoteDirView), coAllFiles);
          }

          if (RemoteFileControlFileOperation(Sender, Operation,
                (WinConfiguration->DDTransferConfirmation == asOff), &Param))
          {
            Configuration->Usage->Inc(CounterName);
          }
        }
      }
    }
    __finally
    {
      CloseHandle(FDDExtMapFile);
      FDDExtMapFile = NULL;
      delete FDragFakeMonitors;
      FDragFakeMonitors = NULL;
      RemoveDir(ApiPath(FDragFakeDirectory));
      FDragFakeDirectory = L"";
      FFakeFileDropTarget = UnicodeString();
    }
  }

  if (FDragDropSshTerminate.get() != NULL)
  {
    FDragDropSshTerminate->Rethrow();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDGiveFeedback(
  TObject * /*Sender*/, int dwEffect, HRESULT & /*Result*/)
{
  // Remember drop effect so we know (when user drops files), if we copy or move
  FLastDropEffect = dwEffect;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DDGetTarget(
  UnicodeString & Directory, bool & ForceQueue, UnicodeString & CounterName)
{
  bool Result;
  if (FDDTargetControl == QueueView3)
  {
    Directory = DefaultDownloadTargetDirectory();
    Result = true;
    CounterName = L"DownloadsDragDropQueue";
    ForceQueue = true;
  }
  else if (!FFakeFileDropTarget.IsEmpty())
  {
    Directory = ExcludeTrailingBackslash(ExtractFilePath(FFakeFileDropTarget));
    Result = true;
    ForceQueue = false;
    CounterName = L"DownloadsDragDropFakeFile";
  }
  else
  {
    ForceQueue = false;

    Enabled = false;
    try
    {
      int Timer = 0;
      Result = false;
      while (!Result && (Timer < WinConfiguration->DDExtTimeout))
      {
        {
          TMutexGuard Guard(FDDExtMutex, DRAGEXT_MUTEX_RELEASE_TIMEOUT);
          TDragExtCommStruct* CommStruct;
          CommStruct = static_cast<TDragExtCommStruct*>(MapViewOfFile(FDDExtMapFile,
            FILE_MAP_ALL_ACCESS, 0, 0, 0));
          DebugAssert(CommStruct != NULL);
          Result = !CommStruct->Dragging;
          if (Result)
          {
            Directory = ExtractFilePath(CommStruct->DropDest);
            CounterName = L"DownloadsDragDropExternalExt";
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
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddDelayedDirectoryDeletion(
  const UnicodeString TempDir, int SecDelay)
{
  TDateTime Alarm = IncSecond(Now(), SecDelay);
  FDelayedDeletionList->AddObject(TempDir, reinterpret_cast<TObject*>(Alarm.FileDate()));
  if (FDelayedDeletionTimer == NULL)
  {
    DebugAssert(HandleAllocated());
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
  DebugAssert(FDelayedDeletionList != NULL);

  TDateTime N = Now();
  TDateTime Alert;
  UnicodeString Directory;

  for (int Index = FDelayedDeletionList->Count-1; Index >= 0; Index--)
  {
    Alert = FileDateToDateTime(reinterpret_cast<int>(FDelayedDeletionList->Objects[Index]));
    if ((N >= Alert) || (Sender == NULL))
    {
      Directory = FDelayedDeletionList->Strings[Index];
      if (RecursiveDeleteFile(ExcludeTrailingBackslash(Directory)))
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
  DebugAssert(!IsLocalBrowserMode());
  if (IsFileControl(FDDTargetControl, osRemote) ||
      (FDDTargetControl == SessionsPageControl))
  {
    TManagedTerminal * TargetTerminal = NULL;
    TFileOperation Operation = foNone;
    if (FDDTargetControl == SessionsPageControl)
    {
      TPoint Point = SessionsPageControl->ScreenToClient(Mouse->CursorPos);
      int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
      // do not allow dropping on local-local, disconnected and the "+" tab (see also SessionsDDProcessDropped)
      TargetTerminal = GetSessionTabSession(SessionsPageControl->Pages[Index]);
      if (IsActiveTerminal(TargetTerminal) && DebugAlwaysTrue(!TargetTerminal->LocalBrowser))
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
      if (FLastDropEffect == DROPEFFECT_MOVE)
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
        Operation, (WinConfiguration->DDTransferConfirmation == asOff), TargetTerminal);
    }
    // abort drag&drop
    Abort();
  }
  else if ((FDDExtMapFile == NULL) && (FLastDropEffect != DROPEFFECT_NONE) &&
           // Drops of remote files to queue are handled in RemoteFileControlDDEnd
           (FDDTargetControl != QueueView3))
  {
    DebugAssert(!FDragTempDir.IsEmpty());
    TTransferType Type;
    UnicodeString TempDir = FDragTempDir;
    // We clear FDragTempDir before calling
    // just in case it fail (raises exception)
    FDragTempDir = L"";
    Type = (FLastDropEffect & DROPEFFECT_MOVE ? ttMove : Type = ttCopy);

    TGUICopyParamType CopyParams = GUIConfiguration->CurrentCopyParam;
    // empty directory parameter means temp directory -> don't display it!
    UnicodeString TargetDir = L"";
    int Options = 0;

    if (!CopyParamDialog(tdToLocal, Type, true, FDDFileList,
          TargetDir, CopyParams, (WinConfiguration->DDTransferConfirmation != asOff), true, Options))
    {
      Abort();
    }

    // TargetDir is set when dropped on local file control
    // (this was workaround for legacy dirview event handling, now it should be
    // made prettier)
    if (TargetDir.IsEmpty())
    {
      TargetDir = TempDir;

      if (ForceDirectories(ApiPath(TargetDir)))
      {
        DebugAssert(Terminal && !TargetDir.IsEmpty());
        FPendingTempSpaceWarn = true;
        try
        {
          FDragDropOperation = true;
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
void __fastcall TCustomScpExplorerForm::DDDownload(
  TStrings * FilesToCopy, const UnicodeString & TargetDir, TCopyParamType * CopyParam, int Params)
{
  DebugAssert(!IsLocalBrowserMode());
  TAutoBatch AutoBatch(this);
  CopyParam->IncludeFileMask.SetRoots(TargetDir, FilesToCopy);
  UpdateCopyParamCounters(*CopyParam);
  FMoveToQueue = false;
  Terminal->CopyToLocal(FilesToCopy, TargetDir, CopyParam, Params, NULL);
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
    bool RenderFilename) : TDataObjectFilesEx(AFileList, RenderPIDL, RenderFilename, true)
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
  DebugFail();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GoToTree()
{
  ComponentVisible[fcRemoteTree] = true;
  DriveView(osRemote)->SetFocus();
}
//---------------------------------------------------------------------------
TStrings * __fastcall TCustomScpExplorerForm::PanelExport(TOperationSide Side,
  TPanelExport Export)
{

  TCustomDirView * DirView = this->DirView(Side);
  std::unique_ptr<TStrings> ExportData(new TStringList());
  switch (Export)
  {
    case pePath:
      ExportData->Add(DirView->PathName);
      break;

    case peFileList:
    case peFullFileList:
      {
        bool FullPath = (Export == peFullFileList);
        DirView->CreateFileList(false, FullPath, ExportData.get());
        for (int Index = 0; Index < ExportData->Count; Index++)
        {
          if (ExportData->Strings[Index].Pos(L" ") > 0)
          {
            ExportData->Strings[Index] = FORMAT(L"\"%s\"", (ExportData->Strings[Index]));
          }
        }
      }
      break;

    default:
      DebugFail();
  }
  return ExportData.release();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExport(TOperationSide Side,
  TPanelExport Export, TPanelExportDestination Destination)
{
  std::unique_ptr<TStrings> ExportData(PanelExport(Side, Export));
  PanelExportStore(Side, Export, Destination, ExportData.get());
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PanelExportStore(TOperationSide /*Side*/,
  TPanelExport /*Export*/, TPanelExportDestination Destination,
  TStrings * ExportData)
{
  if (Destination == pedClipboard)
  {
    TInstantOperationVisualizer Visualizer;
    CopyToClipboard(ExportData);
  }
  else
  {
    DebugFail();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Filter(TOperationSide Side)
{
  TCustomDirView * DirView = this->DirView(Side);
  UnicodeString Mask = DirView->Mask;
  if (DoFilterMaskDialog(DirView, Mask))
  {
    DirView->Mask = TFileMasks::NormalizeMask(Mask);
    Configuration->Usage->Inc(L"Filters");
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
      return ComponentVisible[fcQueueView] && QueueView3->Enabled;

    case qoOnceEmpty:
      return IsAnythingQueued();

    default:
      return FQueueController->AllowOperation(Operation, Param);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GoToQueue()
{
  if (DebugAlwaysTrue(QueueView3->Visible))
  {
    QueueView3->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ExecuteQueueOperation(
  TQueueOperation Operation, void * Param)
{
  if (Operation == qoGoTo)
  {
    GoToQueue();
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
  DebugAssert(Queue != NULL);
  if (Queue != NULL)
  {
    Queue->Enabled = !Queue->Enabled;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3ContextPopup(
  TObject * /*Sender*/, TPoint & /*MousePos*/, bool & /*Handled*/)
{
  FQueueActedItem = QueueView3->ItemFocused;
}
//---------------------------------------------------------------------------
/*virtual*/ int __fastcall TCustomScpExplorerForm::GetStaticComponentsHeight()
{
  return TopDock->Height + (MessageDock->Visible ? MessageDock->Height : 0) + QueueSplitter->Height;
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::GetStaticQueuePanelComponentsHeight()
{
  return
    (QueueFileListSplitter->Visible ? QueueFileListSplitter->Height : 0) +
    (QueueDock->Visible ? QueueDock->Height : 0) +
    (QueueLabel->Visible ? QueueLabel->Height : 0);
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::GetMinQueueViewHeight()
{
  return ScaleByTextHeight(this, 48);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueSplitterCanResize(
  TObject * /*Sender*/, int & NewSize, bool & Accept)
{
  // when queue is hidden by double-clicking splitter, stray attempt to
  // resize the panel with strange value arrives, make sure it is ignored
  if (ComponentVisible[fcQueueView])
  {
    int MaxHeightLimit = ClientHeight - GetStaticComponentsHeight() - RemotePanel->Constraints->MinHeight;
    if (NewSize > MaxHeightLimit)
    {
      NewSize = MaxHeightLimit;
    }

    int MinHeightLimit =
      GetStaticQueuePanelComponentsHeight() +
      (QueueFileList->Visible ? QueueFileList->Height : 0) +
      GetMinQueueViewHeight();
    if (NewSize < MinHeightLimit)
    {
      NewSize = MinHeightLimit;
    }
  }
  else
  {
    Accept = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListSplitterCanResize(TObject *, int & NewSize, bool &)
{
  int TotalHeight = GetStaticQueuePanelComponentsHeight() + NewSize;
  int QueueViewHeight = QueuePanel->ClientHeight - TotalHeight;

  if (QueueViewHeight < GetMinQueueViewHeight())
  {
    NewSize -= (GetMinQueueViewHeight() - QueueViewHeight);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3StartDrag(TObject * /*Sender*/,
  TDragObject *& /*DragObject*/)
{
  FQueueActedItem = QueueView3->ItemFocused;
  FQueueScrollOnDragOver->StartDrag();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3EndDrag(
  TObject *, TObject * DebugUsedArg(Target), int DebugUsedArg(X), int DebugUsedArg(Y))
{
  FQueueScrollOnDragOver->EndDrag();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3DragOver(TObject * /*Sender*/,
  TObject * Source, int X, int Y, TDragState /*State*/, bool & Accept)
{
  Accept = true;
  if (Source == QueueView3)
  {
    TListItem * DropTarget = QueueView3->GetItemAt(X, Y);
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
    FQueueScrollOnDragOver->DragOver(TPoint(X, Y));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3DragDrop(TObject * /*Sender*/,
  TObject * /*Source*/, int /*X*/, int /*Y*/)
{
  if ((FQueueActedItem != NULL) && (QueueView3->DropTarget != NULL))
  {
    TQueueItemProxy * QueueItem;
    TQueueItemProxy * DestQueueItem;

    QueueItem = static_cast<TQueueItemProxy *>(FQueueActedItem->Data);
    DestQueueItem = static_cast<TQueueItemProxy *>(QueueView3->DropTarget->Data);
    QueueItem->Move(DestQueueItem);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3Enter(TObject * /*Sender*/)
{
  if ((QueueView3->ItemFocused == NULL) &&
      (QueueView3->Items->Count > 0))
  {
    QueueView3->ItemFocused = QueueView3->Items->Item[0];
  }

  QueueLabelUpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3Exit(TObject * /*Sender*/)
{
  QueueLabelUpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueLabelUpdateStatus()
{
  QueueLabel->UpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3SelectItem(
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
  else if (Sender == QueueView3)
  {
    Result = FQueueDragDropFilesEx;
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
  DebugAssert(Result != NULL);
  return Result;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::SelectedAllFilesInDirView(TCustomDirView * DView)
{
  return (DView->SelCount == DView->FilesCount);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DraggingAllFilesFromDirView(TOperationSide Side, TStrings * FileList)
{
  return HasDirView[Side] && (DropSourceControl == DirView(Side)) && (FileList->Count == DirView(Side)->FilesCount);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDragDropFileOperation(
  TObject * Sender, int Effect, UnicodeString TargetPath, bool ForceQueue, bool DragDrop)
{
  DebugAssert(!IsLocalBrowserMode());
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

  TDragDropFilesEx * DragDropFilesEx = DragDropFiles(Sender);
  // see a comment in TUnixDirView::PerformItemDragDropOperation
  if (DragDropFilesEx->FileList->Count > 0)
  {
    TStrings * FileList = new TStringList();
    try
    {
      for (int Index = 0; Index < DragDropFilesEx->FileList->Count; Index++)
      {
        FileList->Add(DragDropFilesEx->FileList->Items[Index]->Name);
      }

      FDragDropOperation = true;
      TTransferOperationParam Param;
      Param.TargetDirectory = TargetPath;
      // upload, no temp dirs
      Param.Temp = false;
      Param.DragDrop = DragDrop;
      Param.Options =
        FLAGMASK(DraggingAllFilesFromDirView(osLocal, FileList), coAllFiles);
      if (ForceQueue)
      {
        Param.Queue = asOn;
      }
      bool NoConfirmation = DragDrop ? (WinConfiguration->DDTransferConfirmation == asOff) : false;
      if (ExecuteFileOperation(Operation, osLocal, FileList, NoConfirmation, &Param))
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
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDFileOperation(
  TObject * Sender, int Effect, UnicodeString /*SourcePath*/,
  UnicodeString TargetPath, bool Paste, bool & /*DoOperation*/)
{
  RemoteFileControlDragDropFileOperation(Sender, Effect, TargetPath, false, !Paste);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileContolDDChooseEffect(
  TObject * Sender, int grfKeyState, int & dwEffect)
{
  DebugAssert(!IsLocalBrowserMode());
  // if any drop effect is allowed at all (e.g. no drop to self and drop to parent)
  if ((dwEffect != DROPEFFECT_NONE) &&
      IsFileControl(DropSourceControl, osRemote))
  {
    // do not allow drop on remote panel (on free space, still allow drop on directories)
    if ((Sender == RemoteDirView) && (DropSourceControl == RemoteDirView) &&
        (RemoteDirView->DropTarget == NULL))
    {
      dwEffect = DROPEFFECT_NONE;
    }
    else
    {
      if (dwEffect == DROPEFFECT_COPY)
      {
        bool MoveCapable = Terminal->IsCapable[fcRemoteMove];
        // currently we support copying always (at least via temporary directory);
        // remove associated checks once this all proves stable and working
        bool CopyCapable = true;
        // if we do not support neither of operations, there's no discussion
        if (!MoveCapable && !CopyCapable)
        {
          dwEffect = DROPEFFECT_NONE;
        }
        else
        {
          // The default effect inside remote panel is move,
          // unless we do not support it, but support copy
          if (FLAGCLEAR(grfKeyState, MK_CONTROL))
          {
            dwEffect = MoveCapable ? DROPEFFECT_MOVE : DROPEFFECT_COPY;
          }
          else
          {
            // with ctrl-down, we want copy unless it is not supported
            dwEffect = CopyCapable ? DROPEFFECT_COPY : DROPEFFECT_NONE;
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
  DebugAssert(!IsLocalBrowserMode());
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
  DebugAssert(!FDragTempDir.IsEmpty());
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
  FDragDropSshTerminate.reset(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFileControlDDQueryContinueDrag(
  TObject * /*Sender*/, LongBool /*FEscapePressed*/, int /*grfKeyState*/,
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
        FDragDropSshTerminate.reset(E.Clone());
      }
    }
    catch (Exception &E)
    {
      // If downloading fails we need to cancel drag&drop, otherwise
      // Explorer shows error
      // But by the way exception probably never reach this point as
      // it's catched on way.
      // Fatal exceptions get here (like when opening a secondary shell extension for file duplication fails).
      Result = DRAGDROP_S_CANCEL;
      ShowExtendedException(Terminal, &E);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewMatchMask(
  TObject * /*Sender*/, UnicodeString FileName, bool Directory, __int64 Size,
  TDateTime Modification, UnicodeString Masks, bool & Matches, bool AllowImplicitMatches)
{
  TFileMasks::TParams MaskParams;
  MaskParams.Size = Size;
  MaskParams.Modification = Modification;
  // this does not re-parse the mask if it is the same as the last time
  FDirViewMatchMask = Masks;
  bool ImplicitMatch;
  Matches =
    // Local and RecurseInclude parameters have no effect as there's no path.
    // This should use potential MatchesFileName overload with ImplicitMatch argument.
    FDirViewMatchMask.Matches(FileName, false, Directory, &MaskParams, true, ImplicitMatch) &&
    (AllowImplicitMatches || !ImplicitMatch);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewGetOverlay(
  TObject * Sender, TListItem * Item, WORD & Indexes)
{
  TCustomDirView * DirView = reinterpret_cast<TCustomDirView *>(Sender);
  if (GetPartialFileExtLen(DirView->ItemFileName(Item)) > 0)
  {
    Indexes |= oiPartial;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanPasteToDirViewFromClipBoard()
{
  return
    DirViewEnabled(osCurrent) &&
    DirView(osCurrent)->CanPasteFromClipBoard();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanPasteFromClipBoard()
{
  bool Result = false;

  if (CanPasteToDirViewFromClipBoard())
  {
    Result = true;
  }
  else
  {
    UnicodeString ClipboardText;
    if (NonEmptyTextFromClipboard(ClipboardText) &&
        (ClipboardText.Pos(L"\n") == 0)) // it's already trimmed
    {
      if (StoredSessions->IsUrl(ClipboardText))
      {
        Result = true;
      }
      else
      {
        Result = DirViewEnabled(osCurrent);
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PasteFromClipBoard()
{
  // Pasting to any local panel is handled in TScpCommanderForm::PasteFromClipBoard()
  if (DoesClipboardContainOurFiles())
  {
    if (DebugAlwaysTrue(CanPasteToDirViewFromClipBoard()))
    {
      DebugAssert(!IsLocalBrowserMode());
      TTerminalManager * Manager = TTerminalManager::Instance();
      TTerminal * TargetTerminal = Manager->ActiveTerminal;
      Manager->ActiveSession = FClipboardTerminal;

      ExecuteFileOperation(foRemoteCopy, osRemote, FClipboardFileList.get(), false, TargetTerminal);
    }
  }
  else if (CanPasteToDirViewFromClipBoard())
  {
    DirView(osCurrent)->PasteFromClipBoard();
  }
  else
  {
    UnicodeString ClipboardText;
    if (NonEmptyTextFromClipboard(ClipboardText))
    {
      if (StoredSessions->IsUrl(ClipboardText))
      {
        NewSession(ClipboardText);
      }
      else
      {
        DirView(osCurrent)->Path = ClipboardText;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileListFromClipboard()
{
  // TBD
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SelectAll(TOperationSide Side, TSelectMode Mode)
{
  TCustomDirView * ADirView = DirView(Side);
  ADirView->SelectAll(Mode);
  ADirView->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SelectByMask(TOperationSide Side, bool Select)
{
  TCustomDirView * ADirView = DirView(Side);

  TFileFilter Filter;
  DefaultFileFilter(Filter);
  if (DoSelectMaskDialog(ADirView, Select, Filter))
  {
    Configuration->Usage->Inc(L"MaskSelections");
    ADirView->SelectFiles(Filter, Select);
    ADirView->SetFocus();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RestoreSelectedNames(TOperationSide Side)
{
  TCustomDirView * ADirView = DirView(Side);
  ADirView->RestoreSelectedNames();
  ADirView->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SelectSameExt(bool Select)
{
  TCustomDirView * CurrentDirView = DirView(osCurrent);
  if (DebugAlwaysTrue(CurrentDirView->ItemFocused != NULL))
  {
    UnicodeString FileName = CurrentDirView->ItemFileName(CurrentDirView->ItemFocused);
    UnicodeString Ext;
    if (!IsSideLocalBrowser(osCurrent))
    {
      Ext = UnixExtractFileExt(FileName);
    }
    else
    {
      Ext = ExtractFileExt(FileName);
    }
    if (Ext.IsEmpty())
    {
      Ext = L".";
    }
    TFileFilter Filter;
    Filter.Masks = FORMAT(L"*%s", (Ext));
    Filter.Directories = false;
    CurrentDirView->SelectFiles(Filter, Select);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomScpExplorerForm::FileStatusBarText(
  const TStatusFileInfo & FileInfo, TOperationSide Side)
{
  UnicodeString Result;

  if (FIncrementalSearchState.Searching && (Side == GetSide(osCurrent)))
  {
    Result = FormatIncrementalSearchStatus(FIncrementalSearchState);
  }
  else if (!IsSideLocalBrowser(Side) && ((Terminal == NULL) || Terminal->Disconnected))
  {
    // noop
  }
  else
  {
    Result =
      FMTLOAD(FILE_INFO_FORMAT,
        (FormatBytes(FileInfo.SelectedSize),
         FormatBytes(FileInfo.FilesSize),
         FormatNumber(FileInfo.SelectedCount),
         FormatNumber(FileInfo.FilesCount)));
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileStatusBarPanelClick(
  TTBXStatusPanel * Panel, TOperationSide Side)
{
  if (Panel->Index == 1)
  {
    ToggleShowHiddenFiles();
  }
  else if (Panel->Index == 2)
  {
    Filter(Side);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateFileStatusBar(
  TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo, TOperationSide Side)
{
  DebugAssert(!StatusBar->SimplePanel);
  StatusBar->Panels->Items[0]->Caption = FileStatusBarText(FileInfo, Side);
  UpdateFileStatusExtendedPanels(StatusBar, FileInfo);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateFileStatusExtendedPanels(
  TTBXStatusBar * StatusBar, const TStatusFileInfo & FileInfo)
{
  DebugAssert(StatusBar->Panels->Count >= 3);

  TTBXStatusPanel * HiddenFilesPanel = StatusBar->Panels->Items[1];
  if (FileInfo.HiddenCount > 0)
  {
    HiddenFilesPanel->Caption = FMTLOAD(FILE_INFO_HIDDEN2, (FormatNumber(FileInfo.HiddenCount)));
    HiddenFilesPanel->ViewPriority = 90; // <100 allows hiding panel when it does not fit
  }
  else
  {
    HiddenFilesPanel->ViewPriority = 0;
    // not really necessary, just to cleanup no-longer-valid data
    HiddenFilesPanel->Caption = L"";
  }

  TTBXStatusPanel * FilteredFilesPanel = StatusBar->Panels->Items[2];
  if (FileInfo.FilteredCount > 0)
  {
    FilteredFilesPanel->Caption = FMTLOAD(FILE_INFO_FILTERED2, (FormatNumber(FileInfo.FilteredCount)));
    FilteredFilesPanel->ViewPriority = 90; // <100 allows hiding panel when it does not fit
  }
  else
  {
    FilteredFilesPanel->ViewPriority = 0;
    // not really necessary, just to cleanup no-longer-valid data
    FilteredFilesPanel->Caption = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteStatusBarClick(
  TObject * /*Sender*/)
{
  if (DirView(osOther)->Enabled)
  {
    DirView(osOther)->SetFocus();
  }
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
  if (Terminal != NULL)
  {
    switch (WinConfiguration->PathInCaption)
    {
      case picShort:
        Result = ExtractShortName(Terminal->CurrentDirectory, true);
        break;

      case picFull:
        Result = Terminal->CurrentDirectory;
        break;
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
TColor __fastcall TCustomScpExplorerForm::PanelColor()
{
  TColor Result = GetWindowColor(FSessionColor);
  return Result;
}
//---------------------------------------------------------------------------
TColor __fastcall TCustomScpExplorerForm::DisabledPanelColor()
{
  TColor Result = (WinConfiguration->UseDarkTheme() ? static_cast<TColor>(RGB(0x40, 0x40, 0x40)) : clBtnFace);
  return Result;
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::UpdatePanelControls(TCustomDirView * ADirView, TCustomDriveView * ADriveView)
{
  bool UseDarkTheme = WinConfiguration->UseDarkTheme();
  ADirView->DarkMode = UseDarkTheme;
  ADriveView->DarkMode = UseDarkTheme;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateControls()
{
  if (!FSessionChanging)
  {
    TTerminalManager::Instance()->UpdateAppTitle();
  }
  // WORKAROUND: Disabling list view when it is not showing yet does not set its
  // background to gray on Windows 7 (works on Windows 10).
  // See also EnableControl
  if (Showing)
  {
    if (ManagedSession != NULL)
    {
      // Update path when it changes
      if ((SessionsPageControl->ActivePage != NULL) &&
          (GetSessionTabSession(SessionsPageControl->ActivePage) == ManagedSession))
      {
        UpdateSessionTab(SessionsPageControl->ActivePage);
      }
    }

    NonVisualDataModule->ReconnectSessionAction->Update();
    ReconnectToolbar->Visible = NonVisualDataModule->ReconnectSessionAction->Visible;
    // ReconnectSessionAction is hidden when disabled, so enabling it actualy resizes the toolbar
    CenterReconnectToolbar();

    bool HasTerminal = HasActiveTerminal();

    RemoteDirView->Enabled = HasTerminal;
    RemoteDirView->Color = HasTerminal ? PanelColor() : DisabledPanelColor();
    RemoteDirView->Font->Color = GetWindowTextColor(RemoteDirView->Color);

    RemoteDriveView->Enabled = RemoteDirView->Enabled;
    RemoteDriveView->Color = RemoteDirView->Color;
    RemoteDriveView->Font->Color = RemoteDirView->Font->Color;

    QueueView3->Enabled = HasTerminal && Terminal->IsCapable[fcBackgroundTransfers];
    QueueView3->Color = QueueView3->Enabled ? GetWindowColor() : DisabledPanelColor();
    QueueView3->Font->Color =  GetWindowTextColor(QueueView3->Color);
    QueueFileList->Enabled = QueueView3->Enabled;
    QueueFileList->Color = QueueView3->Color;
    QueueFileList->Font->Color = QueueView3->Font->Color;
    QueueLabelUpdateStatus();

    UpdatePanelControls(RemoteDirView, RemoteDriveView);
    bool UseDarkTheme = WinConfiguration->UseDarkTheme();
    if (FImmersiveDarkMode != UseDarkTheme)
    {
      UpdateDarkMode();
    }

    SessionsPageControl->TabTheme = UseDarkTheme ? CurrentTheme : NULL;

    UnicodeString CurrentHiContrastThemeName = (FHiContrastTheme != NULL) ? FHiContrastTheme->Name : EmptyStr;
    bool HiContrast = WinConfiguration->HiContrast;
    UnicodeString NewHiContrastThemeName = HiContrast ? GetThemeName(!UseDarkTheme) : EmptyStr;
    if (CurrentHiContrastThemeName != NewHiContrastThemeName)
    {
      ReleaseHiContrastTheme();
      if (!NewHiContrastThemeName.IsEmpty())
      {
        FHiContrastTheme = GetTBXTheme(NewHiContrastThemeName);
      }
    }
    SessionsPageControl->ActiveTabTheme = HiContrast ? DebugNotNull(FHiContrastTheme) : NULL;

    reinterpret_cast<TTBCustomItem *>(GetComponent(fcRemotePathComboBox))->Enabled = HasTerminal || IsLocalBrowserMode();

    MessageDock->Visible = (MessageDock->ToolbarCount > 0);
  }
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
  DebugAssert(DirView != NULL);
  DoDirViewLoaded(DirView);
  TransferPresetAutoSelect();
  TUnixDirView * UnixDirView = dynamic_cast<TUnixDirView *>(DirView);
  if (UnixDirView != NULL)
  {
    TManagedTerminal * ATerminal = DebugNotNull(dynamic_cast<TManagedTerminal *>(UnixDirView->Terminal));
    TTerminalManager::Instance()->TerminalLoadedDirectory(ATerminal);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::StartUpdates()
{
  TUpdatesConfiguration Updates = WinConfiguration->Updates;
  // first run after installation
  if (double(Updates.LastCheck) == 0)
  {
    AppLog(L"First run, scheduling updates check for the next run");
    // make sure next time there will be an update (if enabled)
    Updates.LastCheck = TDateTime(1);
    WinConfiguration->Updates = Updates;
  }
  else
  {
    TDateTime Period;
    if (IsUWP() && !IsOfficialPackage())
    {
      Period = DefaultUpdatesPeriod;
      AppLog(L"Thirdparty UWP package, using default updates check period");
    }
    else
    {
      Period = Updates.Period;
    }
    AppLogFmt(L"Updates check period: %.2f", (double(Period)));
    if (double(Period) > 0)
    {
      TDateTime Interval = Now() - Updates.LastCheck;
      AppLogFmt(L"Interval since the last updates check: %.2f", (double(Interval)));
      if (Interval >= Period)
      {
        StartUpdateThread(UpdatesChecked);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdatesChecked()
{
  AppLog(L"Updates check done");
  UnicodeString Message;
  bool New;
  TQueryType Type;
  GetUpdatesMessage(Message, New, Type, false);
  if (!Message.IsEmpty())
  {
    AppLogFmt(L"Updates check message: %s", (Message));
    if (New)
    {
      AppLog(L"New version detected");
      Message = FMTLOAD(NEW_VERSION_CLICK, (Message));
    }
    if (!New && (Type != qtWarning))
    {
      AppLog(L"Posting non-critical updates note");
      PostNote(UnformatMessage(Message), 0, UpdatesNoteClicked, NULL);
    }
    else
    {
      AppLog(L"Posting new version or critical message notification");
      Notify(NULL, Message, Type, true, UpdatesNoteClicked);
    }

    if (New)
    {
      Configuration->Usage->Inc(L"UpdateNotifications");
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdatesNoteClicked(TObject * /*Sender*/)
{
  RestoreApp();

  bool CanDisplay = !NonVisualDataModule->Busy;
  if (!CanDisplay && (Screen->ActiveForm != NULL))
  {
    CanDisplay = (Screen->ActiveForm->Perform(WM_CAN_DISPLAY_UPDATES, 0, 0) != 0);
  }

  if (CanDisplay)
  {
    Configuration->Usage->Inc(L"UpdateNotificationsClicked");
    // Probably not needed as CheckForUpdates is blocking since we started using neon for HTTP implementation.
    NonVisualDataModule->StartBusy();
    try
    {
      CheckForUpdates(true);
    }
    __finally
    {
      NonVisualDataModule->EndBusy();
    }
  }
  else
  {
    Configuration->Usage->Inc(L"UpdateNotificationsBusy");
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GetTransferPresetAutoSelectData(
  TCopyParamRuleData & Data)
{
  DebugAssert(Terminal != NULL);
  Data.HostName = Terminal->SessionData->HostNameExpanded;
  Data.UserName = Terminal->SessionData->UserNameExpanded;
  Data.RemoteDirectory = RemoteDirView->PathName;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetAutoSelect()
{
  // Terminal can be null when we are changing local directory implicitly,
  // because it has been deleted, while no session is connected
  // (Login dialog is open)
  if (FAllowTransferPresetAutoSelect && (Terminal != NULL))
  {
    DebugAssert(!IsLocalBrowserMode());
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
        UnicodeString Message = FMTLOAD(Fmt, (StripHotkey(GUIConfiguration->CopyParamCurrent)));
        Data->Message = MainInstructions(Message);

        int CopyParamAttrs = Terminal->UsableCopyParamAttrs(0).General;
        UnicodeString Info = GUIConfiguration->CurrentCopyParam.GetInfoStr(L"\n",
          CopyParamAttrs);
        if (CopyParamIndex >= 0)
        {
          DebugAssert(GUIConfiguration->CopyParamList->Rules[CopyParamIndex] != NULL);
          Info = FORMAT(L"%s\n \n%s", (Info,
            FMTLOAD(COPY_PARAM_RULE,
              (GUIConfiguration->CopyParamList->Rules[CopyParamIndex]->GetInfoStr(L"\n")))));
        }
        Data->Message += L"\n\n" + Info;

        if (WinConfiguration->CopyParamAutoSelectNotice)
        {
          TransferPresetNoteMessage(Data, true);
        }
        else
        {
          PostNote(Message, 0, TransferPresetNoteClicked, Data);
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
  DebugAssert(NoteData != NULL);
  DebugAssert(!IsLocalBrowserMode());

  TMessageParams Params(AllowNeverAskAgain ? mpNeverAskAgainCheck : 0);

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaIgnore; // "ignore" is after "ok"
  Aliases[0].Alias = LoadStr(CONFIGURE_BUTTON);

  Params.Aliases = Aliases;
  Params.AliasesCount = LENOF(Aliases);

  unsigned int Result =
    MoreMessageDialog(NoteData->Message, NULL, qtInformation,
      qaOK | qaIgnore, HELP_COPY_PARAM_AUTOSELECTED, &Params);

  switch (Result)
  {
    case qaNeverAskAgain:
      DebugAssert(AllowNeverAskAgain);
      WinConfiguration->CopyParamAutoSelectNotice = false;
      break;

    case qaIgnore:
      PreferencesDialog(pmPresets);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::TransferPresetNoteClicked(TObject * Sender)
{
  // as of now useless, as this is used for notes only, never for balloons, ...
  RestoreApp();

  // .. and we should never be busy here
  if (DebugAlwaysTrue(!NonVisualDataModule->Busy))
  {
    TransferPresetNoteMessage(DebugNotNull(dynamic_cast<TTransferPresetNoteData *>(Sender)), false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PreferencesDialog(
  TPreferencesMode APreferencesMode)
{
  std::unique_ptr<TPreferencesDialogData> PreferencesData;
  TCopyParamRuleData Data;
  if (Terminal != NULL)
  {
    PreferencesData.reset(new TPreferencesDialogData());
    GetTransferPresetAutoSelectData(Data);
    PreferencesData->CopyParamRuleData = &Data;
  }
  DoPreferencesDialog(APreferencesMode, PreferencesData.get());
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AdHocCustomCommandValidate(
  const TCustomCommandType & Command)
{
  if (CustomCommandState(Command, FEditingFocusedAdHocCommand, ccltAll) <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE2, (Command.Command)));
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
  bool LocalSide = IsSideLocalBrowser(FCurrentSide);
  int Options =
    FLAGMASK((!RemoteAllowed || LocalSide), ccoDisableRemote) |
    FLAGMASK(LocalSide, ccoDisableRemoteFiles);
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
  DebugAssert(!FLastCustomCommand.Command.IsEmpty());

  int State = CustomCommandState(FLastCustomCommand, OnFocused, ccltAll);
  DebugAssert(State > 0);
  if (State <= 0)
  {
    throw Exception(FMTLOAD(CUSTOM_COMMAND_IMPOSSIBLE2, (FLastCustomCommand.Command)));
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

    State = CustomCommandState(FLastCustomCommand, OnFocused, ccltAll);
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BeforeAction()
{
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PostComponentHide(Byte Component)
{
  DebugAssert(ComponentVisible[Component]);
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
void __fastcall TCustomScpExplorerForm::QueueFileListSplitterDblClick(TObject *)
{
  ComponentVisible[fcQueueFileList] = false;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ThemeChanged()
{
  // We hoped this will refresh scrollbar colors, but it does not have any effect here.
  RefreshColorMode();
  ResetSysDarkTheme();
  ConfigurationChanged();
  ConfigureInterface();
  // To update references to recreated/destroyed themes in SessionsPageControl
  UpdateControls();
  // Should be called for all controls
  RemoteDirView->Perform(WM_THEMECHANGED, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMSettingChange(TMessage & Message)
{
  // Do not handle, when shutting down anyway (maybe also when not setup completely yet?)
  if (!FInvalid &&
      (Message.LParam != 0) &&
      (wcscmp(reinterpret_cast<LPCWCH>(Message.LParam), L"ImmersiveColorSet") == 0))
  {
    ThemeChanged();
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Dispatch(void * Message)
{
  TMessage * M = static_cast<TMessage*>(Message);
  switch (M->Msg)
  {
    case CM_DIALOGCHAR:
      CMDialogChar(*M);
      break;

    case WM_APPCOMMAND:
      WMAppCommand(*M);
      break;

    case WM_SYSCOMMAND:
      WMSysCommand(*M);
      break;

    case WM_QUERYENDSESSION:
      WMQueryEndSession(*M);
      break;

    case WM_ENDSESSION:
      WMEndSession(*reinterpret_cast<TWMEndSession *>(M));
      break;

#ifdef _DEBUG
    case WM_WINDOWPOSCHANGED:
      WMWindowPosChanged(*reinterpret_cast<TWMWindowPosMsg *>(M));
      break;

    case CM_CUSTOMSTYLECHANGED:
      TForm::Dispatch(Message);
      break;

#endif
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
            DebugAssert(!ComponentVisible[fcQueueView]);
          }
          else
          {
            ComponentVisible[Component] = false;
          }
        }
      }
      break;

    case WM_COPYDATA:
      WMCopyData(*M);
      break;

    case WM_MANAGES_CAPTION:
      // caption managed in TTerminalManager::UpdateAppTitle()
      M->Result = 1;
      break;

    case WM_WANTS_MOUSEWHEEL:
      M->Result = 1;
      break;

    case CM_SHOWINGCHANGED:
      CMShowingChanged(*M);
      break;

    case WM_CLOSE:
      WMClose(*M);
      break;

    case WM_DPICHANGED:
      WMDpiChanged(*M);
      break;

    case WM_SETTINGCHANGE:
      WMSettingChange(*M);
      break;

    case CM_DIALOGKEY:
      CMDialogKey(*reinterpret_cast<TWMKeyDown *>(M));
      break;

    case WM_PASTE_FILES:
      PasteFiles();
      break;

    case WM_IS_HIDDEN:
      // To treat master password prompt as a main window while showing it when workspace is loading while
      // the exporer is showing up (Visible is true already)
      M->Result = !FShowing;
      break;

    case WM_SHOWWINDOW:
      if (M->WParam)
      {
        AddStartupSequence(L"J");
      }
      TForm::Dispatch(Message);
      break;

    case WM_QUEUE_CALLBACK:
      WMQueueCallback(*M);
      break;

    default:
      TForm::Dispatch(Message);
      break;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateImages()
{
  // noop
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::RegenerateSessionColorsImageList()
{
  FSessionColors->Clear();
  AddFixedSessionImages();
  ::RegenerateSessionColorsImageList(FSessionColors, FSessionColorMaskImageIndex);
  UpdateImages();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMDpiChanged(TMessage & Message)
{
  TForm::Dispatch(&Message);
  Configuration->Usage->Inc(L"PixelsPerInchChanged");
  UpdatePixelsPerInchMainWindowCounter();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::WMClose(TMessage & Message)
{
  AppLog(L"Main window WM_CLOSE");
  // Cannot close window while we are busy.
  // We cannot test this in FormCloseQuery as that is called also from
  // Close(), which is called by CloseApplicationAction. So we can be busy
  // there already even, when it is legitimate to close the application.
  // Possibly a better place to handle this would be WMSysCommand.
  if (NonVisualDataModule->Busy)
  {
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CMShowingChanged(TMessage & Message)
{
  if (!FShowingChanged)
  {
    AddStartupSequence(L"D");
    FShowingChanged = true;
  }
  // Ignoring implicit local-local browser of Commander, opened from DoShow
  bool NoSession = (ManagedSession == NULL);
  // This calls TCustomScpExplorerForm::DoShow
  TForm::Dispatch(&Message);

  // Now the window is visible (TForm::Dispatch is what usually takes long, like when loading a "local" network directory)
  if (Showing && !FStarted)
  {
    FStarted = true;
    InterfaceStarted();
  }

  if (Showing && NoSession)
  {
    // When we are starting minimized (i.e. from an installer),
    // postpone showing Login dialog until we get restored.
    // Otherwise the Login dialog (and Authentication window) show restored
    // over invisible (minimized) main window.
    if (WindowState == wsMinimized)
    {
      FNeedSession = true;
    }
    else
    {
      // This happens before application ever goes idle, so the toolbars would
      // stay enabled (initial state) until the Login dialog is dismissed.
      UpdateActions();
      NonVisualDataModule->StartBusy();
      try
      {
        // Need to process WM_ACTIVATEAPP before showing the Login dialog,
        // otherwise the dialog does not receive focus.
        // With Commander interface the ProcessMessages is called already
        // by TDriveView, but with Explorer interface, we need to call it explicily
        Application->ProcessMessages();
        // do not reload sessions, they have been loaded just now (optimization)
        NeedSession(true);
      }
      __finally
      {
        NonVisualDataModule->EndBusy();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CenterReconnectToolbar()
{
  ReconnectToolbar->Left = (ReconnectToolbar->Parent->ClientWidth - ReconnectToolbar->Width) / 2;
  ReconnectToolbar->Top = (ReconnectToolbar->Parent->ClientHeight - ReconnectToolbar->Height) / 2;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormConstrainedResize(
  TObject * /*Sender*/, int & MinWidth, int & MinHeight, int & MaxWidth,
  int & MaxHeight)
{
  // workaround for bug in TWinControl.CalcConstraints
  // Check for empty rect (restore from iconic state) is done there only after
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
TSessionData * __fastcall TCustomScpExplorerForm::SessionDataForCode()
{
  std::unique_ptr<TSessionData> Data(CloneCurrentSessionData());
  Terminal->FillSessionDataForCode(Data.get());
  return Data.release();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::GenerateUrl(TStrings * Paths)
{
  std::unique_ptr<TSessionData> Data(SessionDataForCode());
  DoGenerateUrlDialog(Data.get(), Paths);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionGenerateUrl()
{
  GenerateUrl(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FileGenerateUrl()
{
  std::unique_ptr<TStrings> Paths(new TStringList());
  DirView(osCurrent)->CreateFileList(false, true, Paths.get());
  GenerateUrl(Paths.get());
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateSessionColor(TColor value)
{
  FSessionColor = value;

  TColor C = (value != 0 ? value : Vcl::Graphics::clNone);

  TTBXColorItem * ColorItem = dynamic_cast<TTBXColorItem *>(
    static_cast<TObject *>(GetComponent(fcColorMenu)));
  DebugAssert(ColorItem != NULL);
  ColorItem->Color = C;

  NonVisualDataModule->ColorMenuItem->Color = C;

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetSessionColor(TColor value)
{
  if (value != FSessionColor)
  {
    UpdateSessionColor(value);

    // Is null when called from LastTerminalClosed
    if (Terminal != NULL)
    {
      SessionsPageControl->ActivePage->ImageIndex = AddSessionColor(value);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CancelNote(bool Force)
{
  bool Result = FNoteTimer->Enabled;
  if (Result)
  {
    // cannot cancel note too early
    bool NotEarly =
      (Now() - FNoteShown >
          EncodeTimeVerbose(0, 0, (unsigned short)(WinConfiguration->NotificationsStickTime), 0));
    if (Force || NotEarly)
    {
      FNoteTimer->Enabled = false;
      FNote = L"";
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
  DebugAssert(FNoteTimer->Enabled);
  CancelNote(true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateNoteHints()
{
  FNoteHints = FNotes->Text.TrimRight();
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
    UpdateNoteHints();
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

  UpdateNoteHints();
  FNote = Note;
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
  if (NonVisualDataModule->SynchronizeBrowsingAction2->Checked)
  {
    Configuration->Usage->Inc(L"SynchronizeBrowsingEnabled");
  }

  PostNote(FORMAT(LoadStrPart(SYNC_DIR_BROWSE_TOGGLE, 1),
    (LoadStrPart(SYNC_DIR_BROWSE_TOGGLE,
      (NonVisualDataModule->SynchronizeBrowsingAction2->Checked ? 2 : 3)))),
    0, NULL, NULL);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ToggleShowHiddenFiles()
{
  bool NewShowHiddenFiles = !WinConfiguration->ShowHiddenFiles;
  UnicodeString Note =
    FORMAT(LoadStrPart(SHOW_HIDDEN_FILES_TOGGLE, 1), (LoadStrPart(SHOW_HIDDEN_FILES_TOGGLE, (NewShowHiddenFiles ? 2 : 3))));
  PostNote(Note, 0, NULL, NULL);
  GetComponent(fcStatusBar)->Repaint(); // toggling ShowHiddenFiles takes time, force repaint beforehand
  WinConfiguration->ShowHiddenFiles = NewShowHiddenFiles;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SetFormatSizeBytes(TFormatBytesStyle Style)
{
  WinConfiguration->FormatSizeBytes = Style;
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
  TTBXCustomStatusBar * Sender, TTBXStatusPanel * Panel)
{
  if (Panel->Index == 0)
  {
    if (FOnNoteClick != NULL)
    {
      // prevent the user data from being freed by possible call
      // to CancelNote or PostNote during call to OnNoteClick
      std::unique_ptr<TObject> NoteData(FNoteData);
      TNotifyEvent OnNoteClick = FOnNoteClick;
      FNoteData = NULL;
      // need to cancel the note as we are going to delete its user data
      CancelNote(true);
      OnNoteClick(NoteData.get());
    }
  }
  if (HasActiveTerminal() &&
      (Panel->Index >= Sender->Panels->Count - SessionPanelCount))
  {
    FileSystemInfo();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LockWindow(bool Force)
{
  // workaround:
  // 1) for unknown reason, disabling window, while minimized,
  // prevents it from restoring, even if it was enabled again meanwhile
  // 2) when disabling the main window, while another has focus
  // minimize is no longer possible
  // ("keep up to date" dialog - though does not seem to be the case with "find file" -
  // so it has probably something to do with the semi-modalness of the "keep up to date" dialog)
  // Shouldn't we use IsApplicationMinimized() here?
  if ((FLockSuspendLevel == 0) && !IsIconic(Application->Handle) && (Force || (Screen->ActiveForm == this)))
  {
    AppLog(L"Disabling window");
    Enabled = false;
  }

  FLockLevel++;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UnlockWindow()
{
  DebugAssert(FLockLevel > 0);
  FLockLevel--;

  if (FLockLevel == 0)
  {
    DebugAssert(FLockSuspendLevel == 0);
    AppLog(L"Re-enabling window");
    Enabled = true;
    // VCL_COPY (TCustomForm.SetWindowFocus)
    if (Active && (ActiveControl != NULL))
    {
      ::SetFocus(ActiveControl->Handle);
      if (GetFocus() == ActiveControl->Handle)
      {
        ActiveControl->Perform(CM_UIACTIVATE, 0, 0);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SuspendWindowLock()
{
  // We need to make sure that window is enabled when the last modal window closes
  // otherwise focus is not restored correctly.
  // So we re-enable the window when modal window opens and
  // disable it back after is closes
  if (FLockLevel > 0)
  {
    // while we have nesting counter, we know that we never be called
    // recursivelly as Application->OnModalBegin is called only
    // for the top-level modal window
    if (DebugAlwaysTrue(FLockSuspendLevel == 0))
    {
      // won't be disabled when conditions in LockWindow() were not satisfied
      FDisabledOnLockSuspend = !Enabled;
      // When minimized to tray (or actually when set to SW_HIDE),
      // setting Enabled makes the window focusable even when there's
      // modal window over it
      if (!FTrayIcon->Visible)
      {
        Enabled = true;
      }
    }
    FLockSuspendLevel++;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ResumeWindowLock()
{
  if (FLockSuspendLevel > 0)
  {
    DebugAssert(FLockLevel > 0);
    FLockSuspendLevel--;
    // see comment in SuspendWindowLock
    if (DebugAlwaysTrue(FLockSuspendLevel == 0))
    {
      // Note that window can be enabled here, when we were minized to tray when
      // was SuspendWindowLock() called.

      // We should possibly do the same check as in LockWindow(),
      // if it is ever possible that the consitions change between
      // SuspendWindowLock() and ResumeWindowLock()
      Enabled = !FDisabledOnLockSuspend;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateRemotePathComboBox(bool TextOnly)
{
  TTBXComboBoxItem * RemotePathComboBox =
    reinterpret_cast<TTBXComboBoxItem *>(GetComponent(fcRemotePathComboBox));

  if (!TextOnly)
  {
    TStrings * Items = RemotePathComboBox->Strings;
    Items->BeginUpdate();
    try
    {
      Items->Clear();
      if ((Terminal != NULL) && !RemoteDirView->Path.IsEmpty())
      {
        UnicodeString APath = UnixExcludeTrailingBackslash(RemoteDirView->Path);
        while (!IsUnixRootPath(APath))
        {
          int P = APath.LastDelimiter(L'/');
          DebugAssert(P >= 0);
          Items->Insert(0, APath.SubString(P + 1, APath.Length() - P));
          APath.SetLength(P - 1);
        }
        Items->Insert(0, Customunixdirview_SUnixDefaultRootName);
      }
    }
    __finally
    {
      Items->EndUpdate();
    }
  }

  RemotePathComboBox->ItemIndex = RemotePathComboBox->Strings->Count - 1;
  // Setting ItemIndex to -1 does not reset its text
  if (RemotePathComboBox->Strings->Count == 0)
  {
    RemotePathComboBox->Text = L"";
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoRemotePathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString DebugUsedArg(AText), int AIndex, int & ImageIndex)
{
  if (AIndex < 0)
  {
    AIndex = Sender->ItemIndex;
  }
  ImageIndex = (AIndex < Sender->Strings->Count - 1 ? StdDirIcon : StdDirSelIcon);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString AText, int AIndex,
  int & ImageIndex)
{
  DoRemotePathComboBoxAdjustImageIndex(Sender, AText, AIndex, ImageIndex);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxDrawItem(
  TTBXCustomList * /*Sender*/, TCanvas * /*ACanvas*/, TRect & ARect, int AIndex,
  int /*AHoverIndex*/, bool & /*DrawDefault*/)
{
  if (!IsLocalBrowserMode())
  {
    ARect.Left += (10 * AIndex);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxMeasureWidth(
  TTBXCustomList * /*Sender*/, TCanvas * /*ACanvas*/, int AIndex, int &AWidth)
{
  AWidth += (10 * AIndex);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoRemotePathComboBoxItemClick(TObject * Sender)
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
    DebugAssert(RemotePathComboBox->ItemIndex == 0);
    APath = ROOTDIRECTORY;
  }
  if (RemoteDirView->Path != APath)
  {
    RemoteDirView->Path = APath;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxItemClick(TObject * Sender)
{
  DoRemotePathComboBoxItemClick(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoRemotePathComboBoxCancel(TObject * Sender)
{
  DebugAssert(Sender == GetComponent(fcRemotePathComboBox));
  DebugUsedParam(Sender);
  UpdateRemotePathComboBox(true);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemotePathComboBoxCancel(TObject * Sender)
{
  DoRemotePathComboBoxCancel(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewEditing(
  TObject * Sender, TListItem * Item, bool & /*AllowEdit*/)
{
  ResetIncrementalSearch();
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  DebugAssert(DirView != NULL);
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
//---------------------------------------------------------------------------
TDragDropFilesEx * __fastcall TCustomScpExplorerForm::CreateDragDropFilesEx()
{
  TDragDropFilesEx * Result = new TDragDropFilesEx(this);
  Result->AutoDetectDnD = false;
  Result->NeedValid = TFileExMustDnDSet() << nvFilename;
  Result->RenderDataOn = rdoEnterAndDropSync;
  Result->TargetEffects = TDropEffectSet() << deCopy << deMove;
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateDarkMode()
{
  if (IsWin10Build(19041))
  {
    FImmersiveDarkMode = WinConfiguration->UseDarkTheme();
    BOOL DarkMode = FImmersiveDarkMode ? TRUE : FALSE;
    const int DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
    DwmSetWindowAttribute(Handle, DWMWA_USE_IMMERSIVE_DARK_MODE, &DarkMode, sizeof(DarkMode));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateWnd()
{
  TForm::CreateWnd();

  // win32-darkmode calls AllowDarkModeForWindow(this, true) here, but it does not seem to have any effect
  UpdateDarkMode();

  if (FSessionsDragDropFilesEx == NULL)
  {
    FSessionsDragDropFilesEx = CreateDragDropFilesEx();
    FSessionsDragDropFilesEx->OnDragOver = SessionsDDDragOver;
    FSessionsDragDropFilesEx->OnProcessDropped = SessionsDDProcessDropped;
    FSessionsDragDropFilesEx->OnDragEnter = SessionsDDDragEnter;
    FSessionsDragDropFilesEx->OnDragLeave = SessionsDDDragLeave;
  }
  if (FQueueDragDropFilesEx == NULL)
  {
    FQueueDragDropFilesEx = CreateDragDropFilesEx();
    // No need to set OnDragOver as we do not have any restrictions
    FQueueDragDropFilesEx->OnProcessDropped = QueueDDProcessDropped;
    FQueueDragDropFilesEx->OnDragEnter = QueueDDDragEnter;
    FQueueDragDropFilesEx->OnDragLeave = QueueDDDragLeave;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DestroyWnd()
{
  TForm::DestroyWnd();
  FSessionsDragDropFilesEx->DragDropControl = NULL;
  FQueueDragDropFilesEx->DragDropControl = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormShow(TObject * /*Sender*/)
{
  SideEnter(FCurrentSide);
  FEverShown = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFindFiles(
  TTerminal * ATerminal, UnicodeString Directory, const TFileMasks & FileMask,
  TFileFoundEvent OnFileFound, TFindingFileEvent OnFindingFile)
{
  if (!NonVisualDataModule->Busy)
  {
    TTerminalManager::Instance()->ActiveSession = DebugNotNull(dynamic_cast<TManagedTerminal *>(ATerminal));
    Configuration->Usage->Inc(L"FileFinds");
    LockWindow(true);
    NonVisualDataModule->StartBusy();
    WinConfiguration->LockedInterface = true;
    try
    {
      Terminal->FilesFind(Directory, FileMask, OnFileFound, OnFindingFile);
    }
    __finally
    {
      WinConfiguration->LockedInterface = false;
      NonVisualDataModule->EndBusy();
      UnlockWindow();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoFocusRemotePath(TTerminal * ATerminal, const UnicodeString & Path)
{
  if (!NonVisualDataModule->Busy)
  {
    TTerminalManager::Instance()->ActiveSession = DebugNotNull(dynamic_cast<TManagedTerminal *>(ATerminal));
    SetFocus();
    RemoteDirView->Path = UnixExtractFilePath(Path);
    UnicodeString FileName = UnixExtractFileName(Path);
    TListItem * Item = RemoteDirView->FindFileItem(FileName);
    // If not found, it can because we have loaded directory from cache and the file is new, try again after reload.
    // (tbd: we should not even try this, if we have not loaded the directory from the cache)
    if (Item == NULL)
    {
      ReloadDirectory(osRemote);
      Item = RemoteDirView->FindFileItem(FileName);
    }
    if (Item != NULL)
    {
      RemoteDirView->ItemFocused = Item;
      RemoteDirView->ItemFocused->MakeVisible(false);
      RemoteDirView->SetFocus();
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanOperateOnFoundFiles(TTerminal * ATerminal)
{
  bool Result = !NonVisualDataModule->Busy;
  if (Result)
  {
    TTerminalManager::Instance()->ActiveSession = DebugNotNull(dynamic_cast<TManagedTerminal *>(ATerminal));
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoOperationOnFoundFiles(
  TFileOperation Operation, TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished)
{
  if (CanOperateOnFoundFiles(ATerminal))
  {
    TValueRestorer<TFileOperationFinishedEvent> OnFileOperationFinishedRestorer(FOnFileOperationFinished, OnFileOperationFinished);
    ExecuteFileOperation(Operation, osRemote, FileList, false, NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDeleteFoundFiles(
  TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished)
{
  DoOperationOnFoundFiles(foDelete, ATerminal, FileList, OnFileOperationFinished);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoDownloadFoundFiles(
  TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished)
{
  DoOperationOnFoundFiles(foCopy, ATerminal, FileList, OnFileOperationFinished);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoEditFoundFiles(
  TTerminal * ATerminal, TStrings * FileList, TFileOperationFinishedEvent OnFileOperationFinished)
{
  if (CanOperateOnFoundFiles(ATerminal))
  {
    for (int Index = 0; Index < FileList->Count; Index++)
    {
      UnicodeString FileName = FileList->Strings[Index];
      TRemoteFile * File = static_cast<TRemoteFile *>(FileList->Objects[Index]);
      ExecuteRemoteFile(FileName, File, efDefaultEditor);
      OnFileOperationFinished(osRemote, FileName, true, true);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteFindFiles()
{
  FFileFindTerminal = Terminal;
  ShowFileFindDialog(
    Terminal, RemoteDirView->Path, DoFindFiles, DoFocusRemotePath, DoDeleteFoundFiles,
    DoDownloadFoundFiles, DoEditFoundFiles);
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
    if (Button == mbLeft)
    {
      // WORKAROUND: "Mouse down" is raised only after tab is switched.
      // If switching tab (switching session) takes long enough for user
      // to actually release the button, "mouse down" is still raised,
      // but we do not get "mouse up" event, so dragging is not cancelled,
      // prevent that by not beginning dragging in the first place.
      int Key = ((GetSystemMetrics(SM_SWAPBUTTON) == 0) ? VK_LBUTTON : VK_RBUTTON);
      if (IsKeyPressed(Key))
      {
        // when user clicks the "+", we get mouse down only after the session
        // is closed, when new session tab is already on X:Y, so dragging
        // starts, prevent that
        if (MilliSecondsBetween(Now(), FSessionsPageControlNewTabTime) > 500)
        {
          TTerminal * Terminal = GetSessionTabSession(SessionsPageControl->Pages[Index]);
          if (Terminal != NULL)
          {
            SessionsPageControl->BeginDrag(false);
          }
        }
      }
    }
    else if (Button == mbMiddle)
    {
      // ignore middle-click for "New session tab" and the last local-local tab
      TManagedTerminal * Session = GetSessionTabSession(SessionsPageControl->Pages[Index]);
      if ((Session != NULL) && CanCloseSession(Session))
      {
        CloseSessionTab(Index);
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
  TTerminal * TargetTerminal = GetSessionTabSession(SessionsPageControl->Pages[Index]);
  if ((TargetTerminal != NULL) &&
      (SessionsPageControl->ActivePage->PageIndex != Index))
  {
    Configuration->Usage->Inc(L"SessionTabMoves");
    // this is almost redundant as we would recreate tabs in SessionListChanged,
    // but we want to actually prevent that to avoid flicker
    SessionsPageControl->ActivePage->PageIndex = Index;
    TTerminal * Terminal = GetSessionTabSession(SessionsPageControl->ActivePage);
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
    TTerminal * Terminal = GetSessionTabSession(SessionsPageControl->Pages[Index]);
    // do not allow dragging to the "+" tab
    Accept = (Terminal != NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDDragOver(
  int DebugUsedArg(KeyState), const TPoint & Point, int & Effect, int DebugUsedArg(PreferredEffect))
{
  int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
  if (Index < 0)
  {
    Effect = DROPEFFECT_NONE;
  }
  else
  {
    TManagedTerminal * TargetSession = GetSessionTabSession(SessionsPageControl->Pages[Index]);
    // do not allow dropping on the "+" tab or on disconnected or local-local tabs
    if (!IsActiveTerminal(TargetSession) || DebugAlwaysFalse(TargetSession->LocalBrowser))
    {
      Effect = DROPEFFECT_NONE;
    }
    else if ((TargetSession != Terminal) && (Effect == DROPEFFECT_MOVE))
    {
      Effect = DROPEFFECT_COPY;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsDDProcessDropped(
  TObject * /*Sender*/, int /*KeyState*/, const TPoint & Point, int Effect)
{
  int Index = SessionsPageControl->IndexOfTabAt(Point.X, Point.Y);
  // do not allow dropping on local-local, disconnected and the "+" tab
  // (though we do not seem to get here in that case anyway, contrary to RemoteFileControlDDTargetDrop)
  TManagedTerminal * TargetTerminal = GetSessionTabSession(SessionsPageControl->Pages[Index]);
  if (DebugAlwaysTrue(IsActiveTerminal(TargetTerminal) && !TargetTerminal->LocalBrowser))
  {
    DebugAssert(!IsFileControl(DropSourceControl, osRemote));
    if (!IsFileControl(DropSourceControl, osRemote))
    {
      TTerminalManager::Instance()->ActiveSession = TargetTerminal;
      RemoteFileControlDragDropFileOperation(SessionsPageControl, Effect,
        // Why don't we use Terminal->CurrentDirectory directly?
        TTerminalManager::Instance()->ActiveTerminal->CurrentDirectory,
        // do not force queue, drag drop
        false, true);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueDDProcessDropped(
  TObject * /*Sender*/, int /*KeyState*/, const TPoint & /*Point*/, int Effect)
{
  // Downloads are handled in RemoteFileControlDDEnd
  if (!IsFileControl(DropSourceControl, osRemote))
  {
    RemoteFileControlDragDropFileOperation(QueueView3, Effect,
      Terminal->CurrentDirectory,
      // force queue, drag drop
      true, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormClose(TObject * /*Sender*/, TCloseAction & /*Action*/)
{

  FShowing = false;

  // Do not save empty workspace
  if (WinConfiguration->AutoSaveWorkspace && (ManagedSession != NULL))
  {
    std::unique_ptr<TObjectList> DataList(DoCollectWorkspace());
    UnicodeString Name = WorkspaceName();
    DoSaveWorkspace(
      Name, DataList.get(),
      !Configuration->DisablePasswordStoring &&
      WinConfiguration->AutoSaveWorkspacePasswords, false);
    WinConfiguration->LastStoredSession = Name;
  }

  AppLog(L"Closing main window");
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
    return GetSessionColorImage(FSessionColors, Color, FSessionColorMaskImageIndex);
  }
  else
  {
    return FSessionTabImageIndex;
  }
}
//---------------------------------------------------------------------------
int __fastcall TCustomScpExplorerForm::AddFixedSessionImage(int GlyphsSourceIndex)
{
  TPngImageCollectionItem * Item =
    GlyphsModule->ExplorerImages->PngImages->Items[GlyphsSourceIndex];
  FSessionColors->AddPng(Item->PngImage);
  return FSessionColors->Count - 1;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::AddFixedSessionImages()
{
  FSessionColors->SetSize(GlyphsModule->ExplorerImages->Width, GlyphsModule->ExplorerImages->Height);
  FNewRemoteTabTabImageIndex = AddFixedSessionImage(NonVisualDataModule->NewRemoteTabAction->ImageIndex);
  FNewLocalTabTabImageIndex = AddFixedSessionImage(NonVisualDataModule->NewLocalTabAction->ImageIndex);
  FSessionTabImageIndex = AddFixedSessionImage(SiteImageIndex);
  FSessionColorMaskImageIndex = AddFixedSessionImage(SiteColorMaskImageIndex);
  FLocalBrowserTabImageIndex = AddFixedSessionImage(LocalBrowserImageIndex);
}
//---------------------------------------------------------------------------
int TCustomScpExplorerForm::GetNewTabActionImageIndex()
{
  return NonVisualDataModule->NewRemoteTabAction->ImageIndex;
}
//---------------------------------------------------------------------------
int TCustomScpExplorerForm::GetNewTabTabImageIndex(TOperationSide Side)
{
  switch (Side)
  {
    case osLocal:
      return FNewLocalTabTabImageIndex;

    case osRemote:
    case osCurrent:
      return FNewRemoteTabTabImageIndex;

    default:
      DebugFail();
      return -1;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CollectItemsWithTextDisplayMode(TWinControl * Control)
{
  for (int Index = 0; Index < Control->ControlCount; Index++)
  {
    TControl * AControl = Control->Controls[Index];
    TWinControl * WinControl = dynamic_cast<TWinControl *>(AControl);
    if (WinControl != NULL)
    {
      CollectItemsWithTextDisplayMode(WinControl);
    }

    TTBCustomToolbar * Toolbar = dynamic_cast<TTBCustomToolbar *>(AControl);
    if (Toolbar != NULL)
    {
      // we care for top-level items only
      TTBCustomItem * Items = Toolbar->Items;
      for (int ItemIndex = 0; ItemIndex < Items->Count; ItemIndex++)
      {
        TTBCustomItem * Item = Items->Items[ItemIndex];
        if (((Item->DisplayMode == nbdmImageAndText) ||
             (dynamic_cast<TTBXLabelItem *>(Item) != NULL)) &&
            EligibleForImageDisplayMode(Item))
        {
          FItemsWithTextDisplayMode.insert(Item);
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::EligibleForImageDisplayMode(TTBCustomItem * /*Item*/)
{
  return true;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::UpdateToolbarDisplayMode()
{
  bool SelectiveToolbarText = WinConfiguration->SelectiveToolbarText;
  TTBItemDisplayMode DisplayMode = (SelectiveToolbarText ? nbdmImageAndText : nbdmDefault);

  typedef std::set<TTBCustomItem *> TItemsWithTextDisplayMode;
  TItemsWithTextDisplayMode::iterator i = FItemsWithTextDisplayMode.begin();
  bool Result = true;
  while (Result && (i != FItemsWithTextDisplayMode.end()))
  {
    TTBCustomItem * Item = *i;
    TTBXLabelItem * Label = dynamic_cast<TTBXLabelItem *>(Item);
    if (Label != NULL)
    {
      // optimization
      if (Label->Visible == SelectiveToolbarText)
      {
        Result = false;
      }
      else
      {
        Label->Visible = SelectiveToolbarText;
      }
    }
    else
    {
      // optimization
      if (Item->DisplayMode == DisplayMode)
      {
        Result = false;
      }
      else
      {
        Item->DisplayMode = DisplayMode;
      }
    }
    ++i;
  }

  UpdateNewTabTab();

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DisplaySystemContextMenu()
{
  DebugFail();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::IsBusy()
{
  // Among other, a lock level is non zero, while directory is being loaded,
  // even when it happens as a result of dirview navigation,
  // i.e. when TNonVisualDataModule is NOT FBusy.
  // That's why the TNonVisualDataModule::GetBusy calls this method.
  // Among other this prevents a panel auto update to occur while
  // directory is changing.
  return (FLockLevel > 0) || DirView(osCurrent)->IsEditing();
}
//---------------------------------------------------------------------------
Boolean __fastcall TCustomScpExplorerForm::AllowedAction(TAction * Action, TActionAllowed Allowed)
{
  // While the window is disabled, we still seem to process menu shortcuts at least,
  // so stop it at least here.
  // See also TCustomScpExplorerForm::RemoteDirViewBusy
  bool Result;
  if (Allowed == aaUpdate)
  {
    Result = true;
  }
  else
  {
    Result = !NonVisualDataModule->Busy;
    if (Result && (Allowed == aaShortCut))
    {
      // Otherwise F5 for local-local will be swallowed by disabled local<>remote actions in TCustomScpExplorerForm::KeyDown
      Action->Update();
      Result = Action->Enabled;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::EditMenuItemPopup(TTBCustomItem * Sender, bool FromLink)
{
  NonVisualDataModule->EditMenuItemPopup(Sender, FromLink);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewBusy(TObject * /*Sender*/, int Busy, bool & State)
{
  // This is somewhat redundant to LockWindow() call from
  // TTerminalManager::TerminalReadDirectoryProgress.
  // But disabling window is known not to block keyboard shorcuts
  // (see TCustomScpExplorerForm::AllowedAction), this hopefully works.
  if (Busy > 0)
  {
    if (NonVisualDataModule->Busy)
    {
      State = false;
    }
    else
    {
      NonVisualDataModule->StartBusy();
      // Will be countered for TCustomDirView.DoExecute in DoDirViewExecFile
      LockWindow();
    }
  }
  else if (Busy < 0)
  {
    UnlockWindow();
    NonVisualDataModule->EndBusy();
  }
  else
  {
    State = NonVisualDataModule->Busy;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlContextPopup(TObject * /*Sender*/, TPoint & MousePos, bool & Handled)
{
  int Index = SessionsPageControl->IndexOfTabAt(MousePos.X, MousePos.Y);
  if (Index >= 0)
  {
    TPopupMenu * PopupMenu = NULL;

    TManagedTerminal * Session = GetSessionTabSession(SessionsPageControl->Pages[Index]);
    if (Session != NULL)
    {
      SessionsPageControl->ActivePageIndex = Index;

      if (DebugAlwaysTrue(SessionTabSwitched()))
      {
        // copied from TControl.WMContextMenu
        SendCancelMode(SessionsPageControl);

        // explicit popup instead of using PopupMenu property
        // to avoid menu to popup somewhere within SessionTabSwitched above,
        // while connecting yet not-connected session and hence
        // allowing an access to commands over not-completely connected session
        PopupMenu = Session->LocalBrowser ? NonVisualDataModule->LocalBrowserPopup : NonVisualDataModule->SessionsPopup;
      }
    }
    else
    {
      if (SupportsLocalBrowser())
      {
        PopupMenu = NonVisualDataModule->NewTabPopup;
      }
    }

    if (PopupMenu != NULL)
    {
      TPoint Point = SessionsPageControl->ClientToScreen(MousePos);
      PopupMenu->PopupComponent = SessionsPageControl;
      PopupMenu->Popup(Point.x, Point.y);
    }
  }
  Handled = true;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanChangePassword()
{
  return HasActiveTerminal() && Terminal->IsCapable[fcChangePassword];
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ChangePassword()
{
  Configuration->Usage->Inc(L"PasswordChanges");
  std::unique_ptr<TSessionData> ChangePasswordData(Terminal->SessionData->Clone());
  ChangePasswordData->DisableAuthentationsExceptPassword();
  ChangePasswordData->ChangePassword = true;

  std::unique_ptr<TTerminal> ChangePasswordSession(
    Terminal->CreateSecondarySession(L"ChangePassword", ChangePasswordData.get()));
  ChangePasswordSession->AutoReadDirectory = false;

  if (TTerminalManager::Instance()->ConnectTerminal(ChangePasswordSession.get()))
  {
    MessageDialog(MainInstructions(LoadStr(PASSWORD_CHANGED)), qtInformation, qaOK, HELP_CHANGE_PASSWORD);
  }
  else
  {
    Configuration->Usage->Inc(L"PasswordChangesFailed");
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::CanPrivateKeyUpload()
{
  // No nice way to assert SSH2
  return HasActiveTerminal() && (Terminal->FSProtocol == cfsSFTP);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::PrivateKeyUpload()
{
  TOperationVisualizer OperationVisualizer;
  UnicodeString FileName = Terminal->SessionData->PublicKeyFile;
  TTerminalManager::Instance()->UploadPublicKey(Terminal, NULL, FileName);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  DebugUsedParam(OldDPI);
  DebugAssert(NewDPI == GetControlPixelsPerInch(this));
  GlyphsModule->PixelsPerInch = NewDPI;
  // Can be called too early when primary monitor has non-system dpi
  if (FSessionColors != NULL)
  {
    RegenerateSessionColorsImageList();
  }
  UpdateTabsSize();
  UpdateSessionsPageControlHeight();
  QueueFileListColumnAutoSize();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DockContextPopup(TObject * Sender, TPoint & MousePos, bool & /*Handled*/)
{
  NonVisualDataModule->ControlContextPopup(Sender, MousePos);
}
//---------------------------------------------------------------------
class TPublicControl : public TControl
{
friend class TCustomScpExplorerForm;
};
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CopyPopup(TControl * DestControl, TControl * SourceControl)
{
  static_cast<TPublicControl *>(DestControl)->PopupMenu = static_cast<TPublicControl *>(SourceControl)->PopupMenu;
  static_cast<TPublicControl *>(DestControl)->OnContextPopup = static_cast<TPublicControl *>(SourceControl)->OnContextPopup;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DoBookmarkClick(TOperationSide Side, TObject * Sender)
{
  TBookmark * Bookmark = DebugNotNull(reinterpret_cast<TBookmark *>(dynamic_cast<TTBCustomItem *>(Sender)->Tag));
  OpenBookmark(Side, Bookmark);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::LocalBookmarkClick(TObject * Sender)
{
  DoBookmarkClick(osLocal, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteBookmarkClick(TObject * Sender)
{
  DoBookmarkClick(osRemote, Sender);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateOpenDirMenuList(
  TTBCustomItem * Menu, TOperationSide Side, TBookmarkList * BookmarkList)
{
  if (BookmarkList != NULL)
  {
    TNotifyEvent OnBookmarkClick = (Side == osLocal) ? &LocalBookmarkClick : &RemoteBookmarkClick;
    if (IsLocalBrowserMode())
    {
      Side = osLocal;
    }

    if (!WinConfiguration->UseLocationProfiles)
    {
      std::unique_ptr<TStringList> Directories(new TStringList());
      Directories->CaseSensitive = (Side == osRemote);
      for (int Index = 0; Index < BookmarkList->Count; Index++)
      {
        TBookmark * Bookmark = BookmarkList->Bookmarks[Index];
        UnicodeString Directory = Bookmark->GetSideDirectory(Side);
        if (!Directory.IsEmpty() && (Directories->IndexOf(Directory) < 0))
        {
          std::unique_ptr<TTBCustomItem> Item(new TTBXItem(Owner));
          Item->Caption = EscapeHotkey(Directory);
          Item->ShortCut = Bookmark->ShortCut;
          Item->OnClick = OnBookmarkClick;
          Item->Tag = reinterpret_cast<int>(Bookmark);
          Directories->Add(Directory);
          Menu->Add(Item.release());
        }
      }
    }
    else
    {
      std::unique_ptr<TStrings> Folders(CreateSortedStringList());
      for (int Index = 0; Index < BookmarkList->Count; Index++)
      {
        TBookmark * Bookmark = BookmarkList->Bookmarks[Index];
        if (!Bookmark->Node.IsEmpty())
        {
          Folders->Add(Bookmark->Node);
        }
      }

      for (int Index = 0; Index < Folders->Count; Index++)
      {
        std::unique_ptr<TTBCustomItem> Item(new TTBXSubmenuItem(Owner));
        Item->Caption = Folders->Strings[Index];
        Item->ImageIndex = NonVisualDataModule->RemoteChangePathAction2->ImageIndex;
        Folders->Objects[Index] = Item.get();
        Menu->Add(Item.release());
      }

      for (int Index = 0; Index < BookmarkList->Count; Index++)
      {
        TBookmark * Bookmark = BookmarkList->Bookmarks[Index];
        TTBCustomItem * Parent;
        if (!Bookmark->Node.IsEmpty())
        {
          DebugAssert(Folders->IndexOf(Bookmark->Node) >= 0);
          Parent = dynamic_cast<TTBCustomItem *>(Folders->Objects[Folders->IndexOf(Bookmark->Node)]);
        }
        else
        {
          Parent = Menu;
        }
        std::unique_ptr<TTBCustomItem> Item(new TTBXItem(Owner));
        Item->Caption = Bookmark->Name;
        Item->ShortCut = Bookmark->ShortCut;
        Item->OnClick = OnBookmarkClick;
        Item->Tag = reinterpret_cast<int>(Bookmark);

        if (((Bookmark->Name == Bookmark->Local) || (Bookmark->Name == Bookmark->Remote)) &&
            (Bookmark->Local.IsEmpty() || Bookmark->Remote.IsEmpty()))
        {
          // No hint for location profiles that are actually mere "bookmarks"
        }
        else
        {
          UnicodeString Hint = FORMAT(LoadStrPart(LOCATION_PROFILE_HINT, 1), (Bookmark->Name));
          UnicodeString LongHint;
          if (!Bookmark->Local.IsEmpty())
          {
            AddToList(LongHint, FORMAT(LoadStrPart(LOCATION_PROFILE_HINT, 2), (Bookmark->Local)), L"\n");
          }
          if (!Bookmark->Remote.IsEmpty())
          {
            AddToList(LongHint, FORMAT(LoadStrPart(LOCATION_PROFILE_HINT, 3), (Bookmark->Remote)), L"\n");
          }
          AddToList(Hint, LongHint, L"|");
          Item->Hint = Hint;
        }

        Parent->Add(Item.release());
      }
    }
    AddMenuSeparator(Menu);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CreateOpenDirMenu(TTBCustomItem * Menu, TOperationSide Side)
{
  Menu->Clear();

  std::unique_ptr<TTBCustomItem> Item;

  Item.reset(new TTBXItem(Owner));
  Item->Action = (Side == osLocal) ? NonVisualDataModule->LocalOpenDirAction : NonVisualDataModule->RemoteOpenDirAction;
  Item->Options = Item->Options << tboDefault;
  Menu->Add(Item.release());
  AddMenuSeparator(Menu);

  if (Terminal != NULL)
  {
    CreateOpenDirMenuList(Menu, Side, WinConfiguration->Bookmarks[Terminal->SessionData->SessionKey]);
  }

  CreateOpenDirMenuList(Menu, Side, WinConfiguration->SharedBookmarks);

  Item.reset(new TTBXItem(Owner));
  Item->Action = (Side == osLocal) ? NonVisualDataModule->LocalAddBookmarkAction2 : NonVisualDataModule->RemoteAddBookmarkAction2;
  Menu->Add(Item.release());
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::TryOpenDirectory(TOperationSide Side, const UnicodeString & Path)
{
  bool Result = true;
  bool Remote = !IsSideLocalBrowser(Side);
  try
  {
    UnicodeString APath = Path;
    if (Remote)
    {
      Terminal->ExceptionOnFail = true;
    }
    else
    {
      APath = ExpandEnvironmentVariables(Path);
    }

    try
    {
      DirView(Side)->Path = APath;
    }
    __finally
    {
      if (Remote)
      {
        Terminal->ExceptionOnFail = false;
      }
    }
  }
  catch (Exception & E)
  {
    if (!Remote || Terminal->Active)
    {
      ShowExtendedExceptionEx(Terminal, &E);
      Result = false;
    }
    else
    {
      throw;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ReloadDirectory(TOperationSide Side)
{
  // Make sure there some feedback even when loading tiny local folders
  TInstantOperationVisualizer Visualizer;
  DirView(Side)->ReloadDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseSessionTab(int Index)
{
  if (Index == SessionsPageControl->TabIndex)
  {
    CloseTab();
  }
  else
  {
    TTabSheet * TabSheet = SessionsPageControl->Pages[Index];
    TTerminal * Terminal = GetSessionTabSession(TabSheet);
    TTerminalManager * Manager = TTerminalManager::Instance();
    if (!Terminal->Active)
    {
      Manager->FreeTerminal(Terminal);
    }
    else
    {
      TTerminalQueue * Queue = Manager->FindQueueForTerminal(Terminal);
      if (CanCloseQueue(Queue))
      {
        Manager->FreeTerminal(Terminal);
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlTabButtonClick(TPageControl *, int Index)
{
  TManagedTerminal * Session = GetSessionTabSession(SessionsPageControl->Pages[Index]);
  if (Session == NULL)
  {
    TRect ButtonRect = SessionsPageControl->TabButtonRect(Index);
    ButtonRect = TRect(SessionsPageControl->ClientToScreen(ButtonRect.TopLeft()), SessionsPageControl->ClientToScreen(ButtonRect.BottomRight()));
    //TPoint P = TPoint(ButtonRect.Left, ButtonRect.Bottom);
    //P = SessionsPageControl->ClientToScreen(P);
    MenuPopup(NonVisualDataModule->NewTabPopup, ButtonRect, SessionsPageControl);
  }
  else
  {
    CloseSessionTab(Index);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CopyFilesToClipboard(TOperationSide Side, bool OnFocused)
{
  if (DebugAlwaysTrue(!IsSideLocalBrowser(Side)))
  {
    TInstantOperationVisualizer Visualizer;
    // To trigger ClipboardDataObjectRelease (in case the clipboard already contains our data)
    // before we create a new data object
    ClipboardClear();

    DebugAssert(FClipboardFakeDirectory.IsEmpty());
    DebugAssert(FClipboardFakeMonitors.get() == NULL);

    FClipboardFakeDirectory = CreateFakeTransferDirectory();
    // Intermediate variable to WORKAROUND a crash in compiler
    TObjectList * ClipboardFakeMonitors =
      StartCreationDirectoryMonitorsOnEachDrive(FILE_NOTIFY_CHANGE_DIR_NAME, ClipboardFakeCreated);
    FClipboardFakeMonitors.reset(ClipboardFakeMonitors);

    if (FClipboardDragDropFilesEx.get() == NULL)
    {
      FClipboardDragDropFilesEx.reset(new TDragDropFilesEx(this));
      FClipboardDragDropFilesEx->OnDataObjectRelease = ClipboardDataObjectRelease;
    }
    FClipboardDragDropFilesEx->FileList->Clear();
    FClipboardDragDropFilesEx->FileList->AddItem(NULL, FClipboardFakeDirectory);
    FClipboardDragDropFilesEx->CopyToClipboard();

    FClipboardTerminal = Terminal;

    // Need full paths, as cwd can be different once files are pasted
    FClipboardFileList.reset(TRemoteFileList::CloneStrings(RemoteDirView->CreateFileList(OnFocused, true)));
    for (int Index = 0; Index < FClipboardFileList->Count; Index++)
    {
      FClipboardFileList->Strings[Index] = UnixExcludeTrailingBackslash(FClipboardFileList->Strings[Index]);
    }
    AppLogFmt(L"Created fake clipboard directory \"%s\" and started drive monitors", (FClipboardFakeDirectory));
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClipboardClear()
{
  if (OpenClipboard(0))
  {
    AppLog(L"Clearing clipboard");
    EmptyClipboard(); // Calls ClipboardDataObjectRelease, if clipboard contains our data
    CloseClipboard();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::DoesClipboardContainOurFiles()
{
  return !FClipboardFakeDirectory.IsEmpty();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClipboardStop()
{
  bool Deleted = RemoveDir(ApiPath(FClipboardFakeDirectory));
  AppLogFmt(L"Clearing clipboard, deleted clipboard temporary folder \"%s\" - %d", (FClipboardFakeDirectory, int(Deleted)));
  FClipboardFakeDirectory = UnicodeString();
  FClipboardTerminal = NULL;
  if (FDownloadingFromClipboard)
  {
    // We are called by the monitor, so attempt to release it would deadlock
    FClipboardFakeMonitorsPendingReset = true;
  }
  else
  {
    FClipboardFakeMonitors.reset(NULL);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClipboardDataObjectRelease(TObject * /*Sender*/)
{
  if (DebugAlwaysTrue(!FClipboardFakeDirectory.IsEmpty()))
  {
    ClipboardStop();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClipboardDownload(const UnicodeString & TargetDirectory, bool NoConfirmation, bool DragDrop)
{
  TTerminalManager * Manager = TTerminalManager::Instance();
  Manager->ActiveSession = FClipboardTerminal;

  TTransferOperationParam Params;
  Params.TargetDirectory = TargetDirectory;
  Params.DragDrop = DragDrop;
  ExecuteFileOperation(foCopy, osRemote, FClipboardFileList.get(), NoConfirmation, &Params);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::PasteFiles()
{
  // Guard against possible race conditions
  if (!FClipboardPasteTarget.IsEmpty())
  {
    // ClipboardFakeCreated might be called while we are here, so make sure we finish operating on the same path we started with
    UnicodeString Target = FClipboardPasteTarget;
    FClipboardPasteTarget = EmptyStr;
    // Can fail as it can be e.g. locked by AV, so we retry that later
    bool Removed = RemoveDir(ApiPath(Target));
    AppLogFmt(L"First attempt to delete pasted fake clipboard directory \"%s\" - %d", (Target, int(Removed)));

    try
    {
      if (NonVisualDataModule->Busy)
      {
        AppLog(L"Is busy, cannot paste");
      }
      else
      {
        DebugAssert(!FClipboardFakeMonitorsPendingReset);
        FClipboardFakeMonitorsPendingReset = false;
        bool NoConfirmation = (WinConfiguration->DDTransferConfirmation == asOff);
        TAutoFlag Flag(FDownloadingFromClipboard);
        AppLog(L"Downloading to paste target...");
        ClipboardDownload(ExtractFilePath(Target), NoConfirmation, true);
        AppLog(L"Downloaded to paste target...");
      }
    }
    __finally
    {
      if (!Removed)
      {
        Removed = RemoveDir(ApiPath(Target));
        AppLogFmt(L"Second attempt to delete pasted fake clipboard directory \"%s\" - %d", (Target, int(Removed)));
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ClipboardFakeCreated(TObject * /*Sender*/, const UnicodeString FileName)
{
  // It can actually rarelly happen that some random file is created, while we are shutting down the monitor
  // (as it pumps a Windows message queue while being shutted down)
  if (DebugAlwaysTrue(!FClipboardFakeDirectory.IsEmpty()) &&
      // Is can happen that creation of our temporary directory is detected (even though we are creating it before starting the monitors).
      // It tent do happen with later copy-pastes, probably because everything is cached already and monitors start quickly.
      !IsPathToSameFile(FClipboardFakeDirectory, FileName) &&
      SameText(ExtractFileName(FileName), ExtractFileName(FClipboardFakeDirectory)))
  {
    AppLogFmt(L"Fake clipboard directory pasted to \"%s\"", (FileName));
    FClipboardPasteTarget = FileName;
    // Particularly when pasting to external HDD, the fake directory is strangely recreated if removed here in the monitor callback.
    // Postponing removing (or actually whole pasting operation) seems to help.
    PostMessage(Handle, WM_PASTE_FILES, 0, 0);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewGetItemColor(
  TObject * Sender, UnicodeString FileName, bool Directory, __int64 Size, TDateTime Modification, TColor & Color)
{
  TFileMasks::TParams MaskParams;
  MaskParams.Size = Size;
  MaskParams.Modification = Modification;

  TCustomDirView * DirView = DebugNotNull(dynamic_cast<TCustomDirView *>(Sender));
  bool Local = (dynamic_cast<TUnixDirView *>(DirView) == NULL);
  // TUnixDirView::ItemFullFileName returns trailing backslash for directories, what filemask cannot deal with
  if (!Local && Directory)
  {
    FileName = UnixExcludeTrailingBackslash(FileName);
  }
  for (TFileColorData::TList::const_iterator Iter = FFileColors.begin(); Iter != FFileColors.end(); Iter++)
  {
    bool ImplicitMatch;
    if (Iter->FileMask.Matches(FileName, Local, Directory, &MaskParams, false, ImplicitMatch) &&
        !ImplicitMatch)
    {
      Color = Iter->Color;
      break;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewKeyDown(TObject * Sender, WORD & Key, TShiftState)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  if (!DirView->IsEditing())
  {
    // Handled here, instead of RemoteDirViewKeyPress (as on Login dialog),
    // as VK_BACK is handled by TCustomDirView.KeyDown and we can swallow it here,
    // while searching
    if (Key == VK_BACK)
    {
      if (FIncrementalSearchState.Searching)
      {
        if (FIncrementalSearchState.Text.Length() <= 1)
        {
          ResetIncrementalSearch();
        }
        else
        {
          UnicodeString NewText = LeftStr(FIncrementalSearchState.Text, FIncrementalSearchState.Text.Length() - 1);
          IncrementalSearch(NewText, false, false);
        }
        Key = 0;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewKeyPress(TObject * Sender, wchar_t & Key)
{
  TCustomDirView * DirView = dynamic_cast<TCustomDirView *>(Sender);
  if (!DirView->IsEditing() && (WinConfiguration->PanelSearch != isOff))
  {
    // Filter control sequences.
    // When not searching yet, prefer use of the space for toggling file selection
    // (so we cannot incrementally search for a string starting with a space).
    if ((Key > VK_SPACE) ||
        ((Key == VK_SPACE) && (GetKeyState(VK_CONTROL) >= 0) && FIncrementalSearchState.Searching))
    {
      IncrementalSearch(FIncrementalSearchState.Text + Key, false, false);
      Key = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ResetIncrementalSearch()
{
  if (FIncrementalSearchState.Searching)
  {
    FIncrementalSearchState.Reset();
    DirView(osCurrent)->UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::IncrementalSearch(const UnicodeString & Text, bool SkipCurrent, bool Reverse)
{
  TListItem * Item = SearchFile(Text, SkipCurrent, Reverse);

  if (Item == NULL)
  {
    MessageBeep(MB_ICONHAND);
  }
  else
  {
    TCustomDirView * ADirView = DirView(osCurrent);
    {
      TAutoNestingCounter Guard(FIncrementalSearching);
      ADirView->FocusItem(Item);
      if (ADirView->NortonLike == nlOff)
      {
        if ((ADirView->Selected != Item) ||
            (ADirView->SelCount > 1))
        {
          ADirView->ClearSelection();
          ADirView->Selected = Item;
        }
      }
    }
    FIncrementalSearchState.Searching = true;
    FIncrementalSearchState.Text = Text;
    Item->MakeVisible(false);

    TListItem * NextItem = SearchFile(Text, true, Reverse);
    FIncrementalSearchState.HaveNext = (NextItem != NULL) && (NextItem != Item);

    ADirView->UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
TListItem * __fastcall TCustomScpExplorerForm::GetNextFile(TListItem * Item, bool Reverse)
{
  int Index = Item->Index;
  if (!Reverse)
  {
    Index++;
    if (Index >= Item->Owner->Count)
    {
      Index = 0;
    }
  }
  else
  {
    Index--;
    if (Index < 0)
    {
      Index = Item->Owner->Count - 1;
    }
  }
  return Item->Owner->Item[Index];
}
//---------------------------------------------------------------------------
TListItem * __fastcall TCustomScpExplorerForm::SearchFile(const UnicodeString & Text, bool SkipCurrent, bool Reverse)
{
  TCustomDirView * ADirView = DirView(osCurrent);
  if (ADirView->Items->Count == 0)
  {
    return NULL;
  }
  else
  {
    TListItem * CurrentItem = (ADirView->ItemFocused != NULL) ? ADirView->ItemFocused : ADirView->Items->Item[0];
    TListItem * Item = CurrentItem;
    if (SkipCurrent)
    {
      Item = DebugNotNull(GetNextFile(Item, Reverse));
    }

    while (true)
    {
      bool Matches = false;

      switch (WinConfiguration->PanelSearch)
      {
        case isNameStartOnly:
          Matches = ContainsTextSemiCaseSensitive(ADirView->ItemFileName(Item).SubString(1, Text.Length()), Text);
          break;
        case isName:
          Matches = ContainsTextSemiCaseSensitive(ADirView->ItemFileName(Item), Text);
          break;
        case isAll:
          int ColCount = (ADirView->DirViewStyle == dvsReport) ? ADirView->ColProperties->Count : 1;
          int Index = 0;
          while ((Index < ColCount) && !Matches)
          {
            bool Visible = (ADirView->DirViewStyle == dvsReport) ? ADirView->ColProperties->Visible[Index] : true;
            if (Visible)
            {
              Matches = ContainsTextSemiCaseSensitive(ADirView->GetColumnText(Item, Index), Text);
            }
            Index++;
          }
          break;
      }

      if (Matches)
      {
        return Item;
      }

      Item = GetNextFile(Item, Reverse);

      if (Item == CurrentItem)
      {
        return NULL;
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewExit(TObject *)
{
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CMDialogKey(TWMKeyDown & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    if (!FIncrementalSearchState.Text.IsEmpty())
    {
      TShiftState Shift = KeyDataToShiftState(Message.KeyData);
      bool Reverse = Shift.Contains(ssShift);
      IncrementalSearch(FIncrementalSearchState.Text, true, Reverse);
      Message.Result = 1;
      return;
    }
  }
  else if (Message.CharCode == VK_ESCAPE)
  {
    if (FIncrementalSearchState.Searching)
    {
      ResetIncrementalSearch();
      Message.Result = 1;
      return;
    }
  }
  TForm::Dispatch(&Message);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::Deactivate()
{
  TForm::Deactivate();
  // E.g. when switching to an internal editor window
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationEventsDeactivate(TObject *)
{
  // When switching to another application
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::ApplicationEventsModalBegin(TObject *)
{
  ResetIncrementalSearch();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirViewChangeFocus(TObject *, TListItem *)
{
  if (FIncrementalSearching <= 0)
  {
    ResetIncrementalSearch();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteStatusBarMouseDown(TObject *, TMouseButton, TShiftState, int, int)
{
  CountClicksForWindowPrint(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewResize(TObject *)
{
  CenterReconnectToolbar();
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::DoBrowseFile(TCustomDirView * DirView, const UnicodeString & FileName)
{
  if (!FileName.IsEmpty())
  {
    DirView->FocusByName(FileName);
    if (DirView->ItemFocused != NULL)
    {
      DirView->ItemFocused->Selected = true;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::BrowseFile(const UnicodeString & FileName)
{
  DoBrowseFile(RemoteDirView, FileName);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::UpdateQueueFileList()
{
  if (!FQueueStatusUpdating)
  {
    bool Refresh;
    if ((FQueueStatus != NULL) && QueueFileList->Visible)
    {
      Refresh = FQueueStatus->UpdateFileList(FQueueController->GetFocusedPrimaryItem(), FQueueFileList.get());
    }
    else
    {
      if (FQueueFileList->GetCount() > 0)
      {
        FQueueFileList->Clear();
        Refresh = true;
      }
    }
    int Count = FQueueFileList->GetCount();
    if (QueueFileList->Items->Count != Count)
    {
      DebugAssert(Refresh);
      QueueFileList->Items->Count = Count;
    }
    if (Refresh)
    {
      QueueFileList->Invalidate();
    }
    DebugAssert(QueueFileList->Items->Count == FQueueFileList->GetCount());
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListData(TObject *, TListItem * Item)
{
  Item->Caption = FQueueFileList->GetFileName(Item->Index);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueView3Change(TObject *, TListItem *, TItemChange)
{
  // Should be improved, once we do not refresh the list when switching between details of the same batch.
  // See TSynchronizeChecklistDialog::UpdateTimer
  if (QueueFileList->Items->Count > 0)
  {
    QueueFileList->Items->Item[0]->MakeVisible(false);
  }
  UpdateQueueFileList();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueLabelGetStatus(TCustomPathLabel *, bool & Active)
{
  Active = QueueView3->Focused() || QueueFileList->Focused();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListEnterExit(TObject *)
{
  QueueLabelUpdateStatus();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListCustomDrawItem(
  TCustomListView * Sender, TListItem * Item, TCustomDrawState, bool & DebugUsedArg(DefaultDraw))
{
  int State = FQueueFileList->GetState(Item->Index);
  if (State == qfsProcessed)
  {
    Sender->Canvas->Font->Color = clGrayText;
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListColumnAutoSize()
{
  // We do not really need whole width, so we always preemptivelly keep free space for the vertical scrollbar
  QueueFileList->Column[0]->Width =
    QueueFileList->ClientWidth - GetSystemMetricsForControl(QueueFileList, SM_CXVSCROLL) - ScaleByTextHeight(QueueFileList, 8);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::QueueFileListResize(TObject *)
{
  QueueFileListColumnAutoSize();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::CloseApp()
{
  // Called from TNonVisualDataModule::ExplorerActionsExecute, which sets busy flag.
  // FormCloseQuery must check busy state, so that the application cannot be closed while busy by Alt+F4 and X button.
  // So we clear the flag here.
  DebugAssert(NonVisualDataModule->Busy);
  NonVisualDataModule->EndBusy();
  try
  {
    Close();
  }
  __finally
  {
    NonVisualDataModule->StartBusy();
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::IsActiveTerminal(TTerminal * Terminal)
{
  return (Terminal != NULL) && Terminal->Active;
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::HasManagedSession()
{
  return (ManagedSession != NULL);
}
//---------------------------------------------------------------------------
bool __fastcall TCustomScpExplorerForm::HasActiveTerminal()
{
  return IsActiveTerminal(Terminal);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::LocalLocalCopy(
  TFileOperation, TOperationSide, bool DebugUsedArg(OnFocused), bool DebugUsedArg(NoConfirmation),
  bool DebugUsedArg(DragDrop), unsigned int DebugUsedArg(Flags))
{
  DebugFail();
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::LocalLocalCopyCommand(TFileOperation Operation, TOperationSide Side, bool OnFocused, unsigned int Flags)
{
  LocalLocalCopy(Operation, Side, OnFocused, !WinConfiguration->ConfirmTransferring, false, Flags);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::UpdateTabsSize()
{
  // 1) Changing tab list triggers the OnResize.
  // 2) Is called in TForm constructor, when we are not ready to call SessionListChanged (particularly when starting with colored session)
  if (!FUpdatingSessionTabs && (FSessionColors != NULL))
  {
    // UpdateTabsCaptionTruncation should be enough
    SessionListChanged(true);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlResize(TObject *)
{
  UpdateTabsSize();
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetSessionPath(TManagedTerminal * ASession, TOperationSide Side)
{
  if (ASession == ManagedSession)
  {
    UpdateSession(ASession);
  }
  TSessionData * Data = ASession->StateData;
  UnicodeString Result;
  if (Side == osLocal)
  {
    Result = Data->LocalDirectory;
  }
  else if (DebugAlwaysTrue(Side == osOther))
  {
    Result = ASession->LocalBrowser ? Data->OtherLocalDirectory : Data->RemoteDirectory;
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetTabHintSessionDetails(TManagedTerminal * ASession)
{
  UnicodeString Title = TTerminalManager::Instance()->GetSessionTitle(ASession, false);
  UnicodeString Result = ASession->SessionData->DefaultSessionName;
  if (Title == Result)
  {
    Result = EmptyStr;
  }
  else
  {
    Result += L"\n";
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetTabHintDetails(TManagedTerminal * ASession)
{
  UnicodeString Result = GetTabHintSessionDetails(ASession);
  if (!ASession->Active)
  {
    Result += LoadStr(STATUS_NOT_CONNECTED2);
  }
  else
  {
    Result += GetSessionPath(ASession, osRemote);
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString TCustomScpExplorerForm::GetNewTabHintDetails()
{
  return LoadStr(NEW_REMOTE_TAB_HINT);
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::SessionsPageControlTabHint(TPageControl *, int Index, UnicodeString & Hint)
{
  if (Index >= 0)
  {
    TThemeTabSheet * TabSheet = SessionsPageControl->Pages[Index];
    TManagedTerminal * TabSession = GetSessionTabSession(TabSheet);
    if (TabSession == NULL)
    {
      UnicodeString NewTabTabCaption = GetNewTabTabCaption();
      if (NewTabTabCaption != TabSheet->Caption)
      {
        Hint = NewTabTabCaption + L"|" + GetNewTabHintDetails();
      }
    }
    else
    {
      TTerminalManager * Manager = TTerminalManager::Instance();
      UnicodeString TabCaption = Manager->GetSessionTitle(TabSession, true);
      Hint = TabCaption + L"|" + GetTabHintDetails(TabSession);
    }
  }
}
//---------------------------------------------------------------------------
typedef std::vector<TListItem *> TListItemsVector;
struct TCalculateSizeOperation
{
  TCalculateSizeStats Stats;
  TListItemsVector ListItems;
  size_t Index;
};
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::CalculateDirectorySizes(TOperationSide Side)
{
  TCalculateSizeOperation CalculateSizeOperation;
  Side = GetSide(Side);
  TCustomDirView * ADirView = DirView(Side);
  bool LocalSide = IsSideLocalBrowser(Side);
  std::unique_ptr<TStrings> AllFileList(ADirView->CreateFileList(false, LocalSide, NULL, true));
  std::unique_ptr<TStrings> FileList(new TStringList());
  for (int Index = 0; Index < AllFileList->Count; Index++)
  {
    TListItem * Item = DebugNotNull(dynamic_cast<TListItem *>(AllFileList->Objects[Index]));
    if (ADirView->ItemIsDirectory(Item))
    {
      FileList->AddObject(AllFileList->Strings[Index], ADirView->ItemData(Item));
      CalculateSizeOperation.ListItems.push_back(Item);
    }
  }

  CalculateSizeOperation.Index = 0;

  __int64 Size = 0;
  TValueRestorer<TCalculateSizeOperation *> DirectorySizeOperationRestorer(FCalculateSizeOperation, &CalculateSizeOperation);
  TValueRestorer<TFileOperationFinishedEvent> OnFileOperationFinishedRestorer(FOnFileOperationFinished);
  FOnFileOperationFinished = DirectorySizeCalculated;
  TValueRestorer<TFileOperation> PrimaryOperationRestorer(FPrimaryOperation, foCalculateSize);

  TCalculatedSizes CalculatedSizes;
  CalculateSizeOperation.Stats.CalculatedSizes = &CalculatedSizes;

  if (LocalSide)
  {
    TValueRestorer<TOperationSide> ProgressSideRestorer(FProgressSide, Side);
    ManagedSession->CalculateLocalFilesSize(FileList.get(), Size, NULL, true, NULL, &CalculatedSizes);
  }
  else
  {
    TCalculateSizeParams Params;
    Params.Stats = &CalculateSizeOperation.Stats;
    Params.UseCache = true;
    Terminal->CalculateFilesSize(FileList.get(), Size, Params);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::DirectorySizeCalculated(
  TOperationSide Side, const UnicodeString & FileName, bool Success, bool NotCancelled)
{
  DebugAlwaysTrue(FCalculateSizeOperation != NULL);

  TListItem * Item = VisualiseOperationFinished(Side, FileName, false);
  TCustomDirView * ADirView = dynamic_cast<TCustomDirView *>(Item->Owner->Owner);
  // optimization
  bool GotExpectedItem =
    (FCalculateSizeOperation->Index < FCalculateSizeOperation->ListItems.size()) &&
    (FCalculateSizeOperation->ListItems[FCalculateSizeOperation->Index] == Item);
  int Index = -1;
  if (DebugAlwaysTrue(GotExpectedItem))
  {
    Index = FCalculateSizeOperation->Index;
    FCalculateSizeOperation->Index++;
  }
  else
  {
    TListItemsVector::iterator I =
      std::find(FCalculateSizeOperation->ListItems.begin(), FCalculateSizeOperation->ListItems.end(), Item);
    if (DebugAlwaysTrue(I != FCalculateSizeOperation->ListItems.end()))
    {
      Index = I - FCalculateSizeOperation->ListItems.begin();
    }
  }
  if (DebugAlwaysTrue(Index >= 0) && Success && NotCancelled)
  {
    __int64 Size = (*FCalculateSizeOperation->Stats.CalculatedSizes)[Index];
    ADirView->SetItemCalculatedSize(Item, Size);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::MessageDockRequestDock(TObject *, TTBCustomDockableWindow *, bool & Accept)
{
  Accept = false;
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::AutoSizeColumns(TOperationSide Side)
{
  // Send Ctrl+Gray+ to the list view
  TKeyboardState KeyState;
  GetKeyboardState(KeyState);
  TKeyboardState NewKeyState;
  memset(NewKeyState, 0, sizeof(NewKeyState));
  NewKeyState[VK_CONTROL] = 0x81;
  SetKeyboardState(NewKeyState);

  SendMessage(DirView(Side)->Handle, WM_KEYDOWN, VK_ADD, 0);
  SetKeyboardState(KeyState);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::QueueResetLayoutColumns()
{
  LoadListViewStr(QueueView3, QueueViewLayoutDefault);
}
//---------------------------------------------------------------------------
void * TCustomScpExplorerForm::SaveFocus()
{
  return NULL;
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::RestoreFocus(void * Focus)
{
  DebugUsedParam(Focus);
  DebugAssert(Focus == NULL);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::IncrementalSearchStart()
{
  TCustomDirView * ADirView = DirView(osCurrent);
  ADirView->SetFocus();
  if (!FIncrementalSearchState.Searching)
  {
    FIncrementalSearchState.Searching = true;
    FIncrementalSearchState.HaveNext = false;
    DebugAssert(FIncrementalSearchState.Text.IsEmpty());
    ADirView->UpdateStatusBar();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewThumbnailNeeded(
  TUnixDirView *, TListItem * Item, TRemoteFile * File, const TSize & Size, TBitmap *& Bitmap)
{
  // though when downloading to temporary folder, encryption should not be a problem
  if (Terminal->IsCapable[fcBackgroundTransfers] &&
      !File->IsDirectory)
  {
    UnicodeString FileName = File->FullFileName;

    TFileMasks::TParams MaskParams;
    MaskParams.Size = File->Size;
    MaskParams.Modification = File->Modification;

    if ((File->Size <= static_cast<__int64>(WinConfiguration->RemoteThumbnailSizeLimit) * 1024) &&
        FRemoteThumbnailMask.Matches(FileName, false, File->IsDirectory, &MaskParams))
    {
      Bitmap = TTerminalManager::Instance()->ThumbnailNeeded(Terminal, Item->Index, File, Size);
    }
  }
}
//---------------------------------------------------------------------------
TThumbnailDownloadQueueItem * TCustomScpExplorerForm::AddThumbnailDownloadQueueItem(TManagedTerminal * ATerminal)
{
  DebugAssert(ATerminal == Terminal);
  TGUICopyParamType CopyParam = GUIConfiguration->CurrentCopyParam;
  TemporaryFileCopyParam(CopyParam);
  CopyParam.TransferMode = tmBinary;

  UnicodeString SourceDir = ATerminal->CurrentDirectory;
  UnicodeString TempDir, RootTempDir;
  TemporaryDirectoryForRemoteFiles(SourceDir, CopyParam, true, TempDir, RootTempDir);

  TTerminalQueue * AQueue = TTerminalManager::Instance()->FindQueueForTerminal(ATerminal);
  DebugAssert((Queue == NULL) || (AQueue == Queue)); // we might get here before queue is set
  TThumbnailDownloadQueueItem * QueueItem =
    new TThumbnailDownloadQueueItem(this, Terminal, SourceDir, TempDir, &CopyParam);
  AddQueueItem(AQueue, QueueItem, ATerminal);
  return QueueItem;
}
//---------------------------------------------------------------------------
enum { qccRemoteItemVisible, qccRemoteItemRedraw };
typedef std::pair<int, UnicodeString> TRemoteItemVisibleData;
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::PostThumbnailVisibleQueueQuery(int Index, const UnicodeString & FileName)
{
  // The queue might be destroyed before the WM_QUEUE_CALLBACK is processed
  TRemoteItemVisibleData * Data = new TRemoteItemVisibleData();
  Data->first = Index;
  Data->second = FileName;
  PostMessage(Handle, WM_QUEUE_CALLBACK, qccRemoteItemVisible, reinterpret_cast<WPARAM>(Data));
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::PostThumbnailDrawRequest(int Index)
{
  PostMessage(Handle, WM_QUEUE_CALLBACK, qccRemoteItemRedraw, Index);
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::WMQueueCallback(TMessage & Message)
{
  int Command = Message.WParam;
  if (Command == qccRemoteItemVisible)
  {
    TRemoteItemVisibleData * Data = reinterpret_cast<TRemoteItemVisibleData *>(Message.LParam);
    int Index = Data->first;
    UnicodeString FileName = Data->second;
    delete Data;

    TGuard Guard(Terminal->ThumbnailsSection.get());
    bool Visible = False;
    if (Terminal->ThumbnailsEnabled &&
        // though we should better also disable thumbnails once we switch to different view style
        (RemoteDirView->DirViewStyle == dvsThumbnail))
    {
      // The message might be processed, while we are already loading a different directory
      if ((Index >= 0) &&
          (Index < RemoteDirView->Items->Count))
      {
        TListItem * Item = RemoteDirView->Items->Item[Index];
        Visible =
          (RemoteDirView->ItemFullFileName(Item) == FileName) &&
          ListView_IsItemVisible(RemoteDirView->Handle, Index);
      }
    }
    Terminal->ThumbnailVisible(Index, FileName, Visible);
  }
  else if (Command == qccRemoteItemRedraw)
  {
    int Index = Message.LParam;
    if ((Index >= 0) &&
        (Index < RemoteDirView->Items->Count))
    {
      RemoteDirView->InvalidateItem(RemoteDirView->Items->Item[Index]);
    }
  }
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::InitializeRemoteThumbnailMask()
{
  UnicodeString DefaultRemoteThumbnailMask = L"*.jpg;*.jpeg;*.gif;*.png;*.svg;*.bmp;*.raw;*.ico;*.heic;*.webp";
  UnicodeString RemoteThumbnailMask = WinConfiguration->RemoteThumbnailMask;
  if (RemoteThumbnailMask.IsEmpty() ||
      StartsStr(L"+", RemoteThumbnailMask) || StartsStr(L"-", RemoteThumbnailMask) || StartsStr(L"*", RemoteThumbnailMask))
  {
    std::unique_ptr<TStrings> RemoteThumbnailMasks(new TStringList());
    while (!DefaultRemoteThumbnailMask.IsEmpty())
    {
      RemoteThumbnailMasks->Add(CutToChar(DefaultRemoteThumbnailMask, L';', true));
    }

    RemoteThumbnailMasks.reset(ProcessFeatures(RemoteThumbnailMasks.get(), RemoteThumbnailMask));

    RemoteThumbnailMask = EmptyStr;
    for (int Index = 0; Index < RemoteThumbnailMasks->Count; Index++)
    {
      AddToList(RemoteThumbnailMask, RemoteThumbnailMasks->Strings[Index], L";");
    }
  }

  FRemoteThumbnailMask.Masks = RemoteThumbnailMask;
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewStartLoading(TObject *)
{
  Terminal->StartLoadingDirectory();
}
//---------------------------------------------------------------------------
void __fastcall TCustomScpExplorerForm::RemoteDirViewStartReading(TObject *)
{
  Terminal->DisableThumbnails();
}
//---------------------------------------------------------------------------
void TCustomScpExplorerForm::ChangeDirViewStyle(TOperationSide Side, TDirViewStyle DirViewStyle)
{
  TCustomDirView * ADirView = DirView(Side);
  if (ADirView->DirViewStyle != DirViewStyle)
  {
    if (DirViewStyle == dvsThumbnail)
    {
      if (dynamic_cast<TUnixDirView *>(ADirView) != NULL)
      {
        Configuration->Usage->Inc(L"ThumbnailsRemote");
      }
      else
      {
        Configuration->Usage->Inc(L"ThumbnailsLocal");
      }
    }
    ADirView->DirViewStyle = DirViewStyle;
    UpdateControls();
  }
}
