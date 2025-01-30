//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <CustomWinConfiguration.h>
#include <GUITools.h>
#include <BaseUtils.hpp>
#include <DateUtils.hpp>
#include <Consts.hpp>
#include <HistoryComboBox.hpp>
#include <windowsx.h>

#include "Progress.h"
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma link "PngImageList"
#pragma link "TB2Dock"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBX"
#pragma link "TB2ExtItems"
#pragma link "TBXExtItems"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
static IsIndeterminate(const TSynchronizeProgress * SynchronizeProgress, const TFileOperationProgressType * ProgressData)
{
  bool Result;
  // TSynchronizeProgress has its own way how to take atomic operations into account
  if (SynchronizeProgress != NULL)
  {
    Result = TFileOperationProgressType::IsIndeterminateOperation(ProgressData->Operation);
  }
  else
  {
    Result = ProgressData->IsIndeterminate();
  }
  return Result;
}
//---------------------------------------------------------------------
UnicodeString __fastcall TProgressForm::ProgressStr(
  const TSynchronizeProgress * SynchronizeProgress, const TFileOperationProgressType * ProgressData)
{
  static const int Captions[] = { 0, 0, PROGRESS_DELETE,
    PROGRESS_SETPROPERTIES, 0, PROGRESS_CUSTOM_COMAND, PROGRESS_CALCULATE_SIZE,
    PROGRESS_REMOTE_MOVE, PROGRESS_REMOTE_COPY, PROGRESS_GETPROPERTIES,
    PROGRESS_CALCULATE_CHECKSUM, PROGRESS_LOCK, PROGRESS_UNLOCK };
  DebugAssert((unsigned int)ProgressData->Operation >= 1 && ((unsigned int)ProgressData->Operation - 1) < LENOF(Captions));
  int Id;
  if (ProgressData->IsTransfer())
  {
    Id = (ProgressData->Side == osLocal) ? PROGRESS_UPLOAD : PROGRESS_DOWNLOAD;
  }
  else
  {
    Id = Captions[(int)ProgressData->Operation - 1];
    DebugAssert(Id != 0);
  }
  UnicodeString Result = LoadStr(Id);
  if (SynchronizeProgress != NULL)
  {
    Result = LoadStr(SYNCHRONIZE_PROGRESS_SYNCHRONIZE2) + TitleSeparator + Result;
  }
  if (!IsIndeterminate(SynchronizeProgress, ProgressData))
  {
    int OverallProgress;
    if (SynchronizeProgress != NULL)
    {
      OverallProgress = SynchronizeProgress->Progress(ProgressData);
    }
    else
    {
      OverallProgress = ProgressData->OverallProgress();
    }
    Result = FORMAT(L"%d%% %s", (OverallProgress, Result));
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TProgressForm::TProgressForm(
  TComponent * AOwner, bool AllowMoveToQueue, bool AllowSkip, TSynchronizeProgress * SynchronizeProgress)
    : FData(), TForm(AOwner)
{
  FLastOperation = foNone;
  FLastSide = (TOperationSide)-1;
  FLastTotalSizeSet = false;
  FDataGot = false;
  FDataReceived = false;
  FCancel = csContinue;
  FMoveToQueue = false;
  FMinimizedByMe = false;
  FUpdateCounter = 0;
  FDeleteLocalToRecycleBin = false;
  FDeleteRemoteToRecycleBin = false;
  FReadOnly = false;
  FShowAsModalStorage = NULL;
  FStarted = Now();
  FModalBeginHooked = false;
  FModalLevel = -1;
  FPendingSkip = false;
  FSynchronizeProgress = SynchronizeProgress;
  FAllowSkip = AllowSkip;
  FWheelDelta = 0;
  UseSystemSettings(this);

  FOnceDoneItems.Add(odoIdle, IdleOnceDoneItem);
  FOnceDoneItems.Add(odoDisconnect, DisconnectOnceDoneItem);
  FOnceDoneItems.Add(odoSuspend, SuspendOnceDoneItem);
  FOnceDoneItems.Add(odoShutDown, ShutDownOnceDoneItem);
  ResetOnceDoneOperation();
  HideComponentsPanel(this);
  SelectScaledImageList(ImageList);

  SetGlobalMinimizeHandler(this, GlobalMinimize);
  MoveToQueueItem->Visible = AllowMoveToQueue;
}
//---------------------------------------------------------------------------
__fastcall TProgressForm::~TProgressForm()
{
  // to prevent raising assertion (e.g. IsProgress == True)
  FData.Clear();

  ClearGlobalMinimizeHandler(GlobalMinimize);

  if (IsApplicationMinimized() && FMinimizedByMe)
  {
    ShowNotification(
      NULL, MainInstructions(LoadStr(BALLOON_OPERATION_COMPLETE)),
      qtInformation);
  }

  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------
UnicodeString __fastcall TProgressForm::ProgressStr()
{
  return FProgressStr;
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::UpdateControls()
{
  DebugAssert((FData.Operation >= foCopy) && (FData.Operation <= foUnlock) &&
    (FData.Operation != foRename));

  bool TransferOperation = FData.IsTransfer();
  bool Indeterminate = IsIndeterminate(SynchronizeProgress, &FData);

  CancelItem->Enabled = !FReadOnly && (FCancel < csCancel);
  SkipItem->Visible = TransferOperation && FAllowSkip;
  SkipItem->Enabled = !FReadOnly && (FCancel < csCancelFile) && !FPendingSkip;
  MoveToQueueItem->Enabled = !FMoveToQueue && (FCancel == csContinue) && !FPendingSkip;
  bool HideOnceDone =
    FReadOnly ||
    (FData.Operation == foCalculateSize) ||
    (FData.Operation == foGetProperties) ||
    (FData.Operation == foCalculateChecksum) ||
    ((FData.Operation == foDelete) && (FData.Side == osLocal));
  CycleOnceDoneItem->Visible = !HideOnceDone;
  CycleOnceDoneItem->ImageIndex = CurrentOnceDoneItem()->ImageIndex;
  SpeedComboBoxItem->Visible = TransferOperation;

  if ((FData.Operation != FLastOperation) ||
      (FData.Side != FLastSide))
  {
    UnicodeString Animation;
    UnicodeString CancelCaption = Vcl_Consts_SMsgDlgCancel;
    int MoveToQueueImageIndex = -1;
    FPendingSkip = false; // just in case

    int MoveTransferToQueueImageIndex;
    if (FData.Side == osRemote)
    {
      MoveTransferToQueueImageIndex = 7;
    }
    else
    {
      MoveTransferToQueueImageIndex = 8;
    }

    switch (FData.Operation)
    {
      case foCopy:
        if (FData.Side == osRemote)
        {
          Animation = L"CopyRemote";
        }
        else
        {
          Animation = L"CopyLocal";
        }
        MoveToQueueImageIndex = MoveTransferToQueueImageIndex;
        break;

      case foMove:
        if (FData.Side == osRemote)
        {
          Animation = L"MoveRemote";
        }
        else
        {
          Animation = L"MoveLocal";
        }
        MoveToQueueImageIndex = MoveTransferToQueueImageIndex;
        break;

      case foDelete:
        Animation = ((FData.Side == osRemote) ? DeleteRemoteToRecycleBin : DeleteLocalToRecycleBin) ? L"Recycle" : L"Delete";
        MoveToQueueImageIndex = 10;
        break;

      case foCalculateSize:
        Animation = L"CalculateSize";
        CancelCaption = LoadStr(SKIP_BUTTON);
        MoveToQueueImageIndex = MoveTransferToQueueImageIndex;
        break;

      case foSetProperties:
        Animation = "SetProperties";
        break;

      case foRemoteCopy:
        Animation = "DuplicateLtoR";
        break;

      case foRemoteMove:
        Animation = "MoveLtoR";
        break;

      default:
        DebugAssert(
          (FData.Operation == foCustomCommand) ||
          (FData.Operation == foGetProperties) ||
          (FData.Operation == foCalculateChecksum) ||
          (FData.Operation == foLock) ||
          (FData.Operation == foUnlock));
        break;
    }

    CancelItem->Caption = CancelCaption;

    OperationProgress->Style = Indeterminate ? pbstMarquee : pbstNormal;

    if (SynchronizeProgress != NULL)
    {
      Animation = L"SynchronizeDirectories";
    }

    FFrameAnimation.Init(AnimationPaintBox, Animation);
    FFrameAnimation.Start();

    int Delta = 0;
    if (TransferOperation && !TransferPanel->Visible) Delta += TransferPanel->Height;
      else
    if (!TransferOperation && TransferPanel->Visible) Delta += -TransferPanel->Height;
    TransferPanel->Visible = TransferOperation;

    ClientHeight = ClientHeight + Delta;

    TargetLabel->Visible = TransferOperation;
    TargetPathLabel->Visible = TransferOperation;
    TargetPathLabel->UnixPath = (FData.Side == osLocal);

    FileLabel->UnixPath = (FData.Side == osRemote);
    PathLabel->Caption =
      LoadStr((FData.Operation == foCalculateSize) ? PROGRESS_PATH_LABEL : PROGRESS_FILE_LABEL);

    DebugAssert(!MoveToQueueItem->Visible || (MoveToQueueImageIndex >= 0));
    MoveToQueueItem->ImageIndex = MoveToQueueImageIndex;

    FLastOperation = FData.Operation;
    FLastSide = FData.Side;
    FLastTotalSizeSet = !FData.TotalSizeSet;
  }

  if (FLastTotalSizeSet != FData.TotalSizeSet)
  {
    StartTimeLabelLabel->Visible = !FData.TotalSizeSet;
    StartTimeLabel->Visible = !FData.TotalSizeSet;
    TimeLeftLabelLabel->Visible = FData.TotalSizeSet;
    TimeLeftLabel->Visible = FData.TotalSizeSet;
    FLastTotalSizeSet = FData.TotalSizeSet;
  }

  UnicodeString FileCaption;
  if ((FData.Operation == foCalculateSize) && DebugAlwaysTrue(!FData.Temp))
  {
    if (FData.Side == osRemote)
    {
      FileCaption = UnixExtractFileDir(FData.FullFileName);
    }
    else
    {
      FileCaption = ExtractFileDir(FData.FullFileName);
    }
  }
  else if ((FData.Side == osRemote) || !FData.Temp)
  {
    FileCaption = FData.FileName;
  }
  else
  {
    FileCaption = ExtractFileName(FData.FileName);
  }
  if (FileLabel->Caption != FileCaption)
  {
    FileLabel->Caption = FileCaption;
    FPendingSkip = false;
  }

  int OverallProgress;
  // as a side effect this prevents calling TSynchronizeProgress::Progress when we do not know total size yet
  // (what would cache wrong values forever)
  if (Indeterminate)
  {
    OverallProgress = -1;
  }
  else
  {
    if (SynchronizeProgress != NULL)
    {
      OverallProgress = SynchronizeProgress->Progress(&FData);
    }
    else
    {
      OverallProgress = FData.OverallProgress();
    }
  }
  OperationProgress->Position = std::max(0, OverallProgress);
  OperationProgress->Hint = (OverallProgress < 0) ? UnicodeString() : FORMAT(L"%d%%", (OverallProgress));
  FProgressStr = ProgressStr(SynchronizeProgress, &FData);
  Caption = FormatFormCaption(this, FProgressStr);

  if (TransferOperation)
  {
    if ((FData.Side == osLocal) || !FData.Temp)
    {
      TargetPathLabel->Caption = FData.Directory;
    }
    else
    {
      TargetPathLabel->Caption = LoadStr(PROGRESS_TEMP_DIR);
    }

    StartTimeLabel->Caption = FData.StartTime.TimeString();
    if (FData.TotalSizeSet)
    {
      UnicodeString TimeLeftCaption;
      bool CanShow = CanShowTimeEstimate(FData.StartTime);
      if (CanShow)
      {
        TDateTime TimeLeft;
        if (SynchronizeProgress != NULL)
        {
          TimeLeft = SynchronizeProgress->TimeLeft(&FData);
        }
        else
        {
          TimeLeft = FData.TotalTimeLeft();
        }
        TimeLeftCaption = FormatDateTimeSpan(TimeLeft);
      }
      else
      {
        TimeLeftCaption = LoadStr(PROGRESS_TIME_LEFT_CALCULATING);
      }
      TimeLeftLabel->Caption = TimeLeftCaption;
    }
    TDateTime Elapsed = FData.TimeElapsed();
    TimeElapsedLabel->Caption = FormatDateTimeSpan(Elapsed);
    BytesTransferredLabel->Caption = FormatBytes(FData.TotalTransferred);
    int CPS = FData.CPS();
    CPSLabel->Caption = FORMAT(L"%s/s", (FormatBytes(CPS)));
    FileProgress->Position = FData.TransferProgress();
    FileProgress->Hint = FORMAT(L"%d%%", (FileProgress->Position));
  }

  SizeToolbar(this, Toolbar, Dock, ToolbarPanel);
}
//---------------------------------------------------------------------
void TProgressForm::SizeToolbar(TForm * Form, TTBXToolbar * Toolbar, TTBXDock * Dock, TPanel * ToolbarPanel)
{
  // particularly to adjust to LargerToolbar
  Dock->ClientHeight = Toolbar->Height;
  int Delta = Dock->Height - ToolbarPanel->ClientHeight;
  if (Delta != 0)
  {
    ToolbarPanel->ClientHeight += Delta;
    DebugAssert(ToolbarPanel->Anchors.Contains(akBottom));
    ToolbarPanel->Top -= Delta; // to counter akBottom anchoring
    Form->ClientHeight += Delta;
  }
}
//---------------------------------------------------------------------
static __int64 DelayStartInterval = MSecsPerSec / 2;
static __int64 UpdateInterval = 1 * MSecsPerSec;
//---------------------------------------------------------------------
bool __fastcall TProgressForm::ReceiveData(bool Force, int ModalLevelOffset)
{
  bool Result = false;
  if (FDataGot && !FDataReceived)
  {
    // CPS limit is set set only once from TFileOperationProgressType::Start.
    // Needs to be set even when data are not accepted yet, otherwise we would
    // write default value to FData in TProgressForm::SetProgressData
    FCPSLimit = FData.CPSLimit;

    // Never popup over dialog that appeared later than we started
    // (this can happen from UpdateTimerTimer when application is
    // restored while overwrite confirmation dialog [or any other]
    // is already shown).
    // TODO We should probably take as-modal windows into account too
    // (for extreme cases like restoring while reconnecting [as-modal TAuthenticateForm]).
    if ((FModalLevel < 0) || (Application->ModalLevel + ModalLevelOffset <= FModalLevel))
    {
      // Delay showing the progress until the application is restored,
      // otherwise the form popups up unminimized.
      // See solution in TMessageForm::CMShowingChanged.
      if (!IsApplicationMinimized() &&
          (Force || (MilliSecondsBetween(Now(), FStarted) > DelayStartInterval)))
      {
        FDataReceived = true;
        SpeedComboBoxItem->Text = SetSpeedLimit(FCPSLimit);
        ShowAsModal(this, FShowAsModalStorage);
        // particularly needed for the case, when we are showing the form delayed
        // because application was minimized when operation started
        Result = true;
      }
      else if (!FModalBeginHooked && DebugAlwaysTrue(FModalLevel < 0))
      {
        // record state as of time, the window should be shown,
        // had not we implemented delayed show
        ApplicationEvents->OnModalBegin = ApplicationModalBegin;
        FModalBeginHooked = true;
        FModalLevel = Application->ModalLevel;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::ApplicationModalBegin(TObject * /*Sender*/)
{
  // Popup before any modal dialog shows (typically overwrite confirmation,
  // as that popups nearly instantly, i.e. less than DelayStartInterval).
  // The Application->ModalLevel is already incremented, but we should treat it as
  // if it were not as the dialog is not created yet (so we can popup if we are not yet).
  ReceiveData(true, -1);
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::SetProgressData(TFileOperationProgressType & AData)
{
  bool InstantUpdate = false;

  // workaround: to force displaing first file data immediately,
  // otherwise form dialog uses to be blank for first second
  // (until UpdateTimerTimer)
  if (FileLabel->Caption.IsEmpty() && !AData.FileName.IsEmpty())
  {
    InstantUpdate = true;
  }

  FData.AssignButKeepSuspendState(AData);
  FDataGot = true;
  if (!UpdateTimer->Enabled)
  {
    UpdateTimer->Interval = static_cast<int>(DelayStartInterval);
    UpdateTimer->Enabled = true;
    FSinceLastUpdate = 0;
  }

  if (ReceiveData(false, 0))
  {
    InstantUpdate = true;
  }

  if (InstantUpdate)
  {
    UpdateControls();
    Application->ProcessMessages();
  }
  if (ProcessGUI(FUpdateCounter % 5 == 0))
  {
    FUpdateCounter = 0;
  }
  FUpdateCounter++;

  AData.SetCPSLimit(FCPSLimit);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::UpdateTimerTimer(TObject * /*Sender*/)
{
  // popup the progress window at least here, if SetProgressData is
  // not being called (typically this happens when using custom command
  // that launches long-lasting external process, such as visual diff)
  ReceiveData(false, 0);

  if (UpdateTimer->Interval == DelayStartInterval)
  {
    UpdateTimer->Interval = static_cast<int>(GUIUpdateInterval);
  }

  if (FDataReceived)
  {
    FSinceLastUpdate += UpdateTimer->Interval;
    if (FSinceLastUpdate >= UpdateInterval)
    {
      UpdateControls();
      FSinceLastUpdate = 0;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormShow(TObject * /*Sender*/)
{
  CopySpeedLimits(CustomWinConfiguration->History[L"SpeedLimit"], SpeedComboBoxItem->Strings);
  ReceiveData(false, 0);
  if (FDataReceived)
  {
    UpdateControls();
  }
  // HACK: In command-line run (/upload), FormShow gets called twice,
  // leading to duplicate hook and memory leak. Make sure we unhook, just in case.
  // Calling unhook without hooking first is noop.
  UnhookFormActivation(this);
  HookFormActivation(this);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormHide(TObject * /*Sender*/)
{
  UnhookFormActivation(this);
  // This is to counter the "infinite" timestamp in
  // TTerminalManager::ApplicationShowHint.
  // Because if form disappears on its own, hint is not hidden.
  Application->CancelHint();
  CustomWinConfiguration->History[L"SpeedLimit"] = SpeedComboBoxItem->Strings;
  UpdateTimer->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelItemClick(TObject * /*Sender*/)
{
  CancelOperation();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::Minimize(TObject * Sender)
{
  CallGlobalMinimizeHandler(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MinimizeItemClick(TObject * Sender)
{
  Minimize(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelOperation()
{
  DebugAssert(FDataReceived);
  if (!FData.Suspended)
  {
    // mostly useless, as suspend is called over copy of actual progress data
    FData.Suspend();
    UpdateControls();
    try
    {
      TCancelStatus ACancel;
      if (FData.TransferringFile &&
          (FData.TimeExpected() > GUIConfiguration->IgnoreCancelBeforeFinish))
      {
        int Result = MessageDialog(LoadStr(CANCEL_OPERATION_FATAL2), qtWarning,
          qaYes | qaNo | qaCancel, HELP_PROGRESS_CANCEL);
        switch (Result)
        {
          case qaYes:
            ACancel = csCancelTransfer; break;
          case qaNo:
            ACancel = csCancel; break;
          default:
            ACancel = csContinue; break;
        }
      }
      else
      {
        ACancel = csCancel;
      }

      SetCancelLower(ACancel);
    }
    __finally
    {
      FData.Resume();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::GlobalMinimize(TObject * /*Sender*/)
{
  ApplicationMinimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
TTBCustomItem * __fastcall TProgressForm::CurrentOnceDoneItem()
{
  TOnceDoneItems::const_iterator Iterator = FOnceDoneItems.begin();
  while (Iterator != FOnceDoneItems.end())
  {
    if (Iterator->second->Checked)
    {
      return Iterator->second;
    }
    Iterator++;
  }

  DebugFail();
  return NULL;
}
//---------------------------------------------------------------------------
TOnceDoneOperation __fastcall TProgressForm::GetOnceDoneOperation()
{
  return FOnceDoneItems.LookupFirst(CurrentOnceDoneItem());
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetOnceDoneItem(TTBCustomItem * Item)
{
  TTBCustomItem * Current = CurrentOnceDoneItem();
  if (Current != Item)
  {
    Current->Checked = false;
    Item->Checked = true;
    // Not until we have any data to update.
    // Happens when set to odoDisconnect in command-line upload/download
    // mode from TCustomScpExplorerForm::FileOperationProgress.
    if (FDataGot)
    {
      UpdateControls();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetOnceDoneOperation(TOnceDoneOperation value)
{
  SetOnceDoneItem(FOnceDoneItems.LookupSecond(value));
}
//---------------------------------------------------------------------------
bool __fastcall TProgressForm::GetAllowMinimize()
{
  return MinimizeItem->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetAllowMinimize(bool value)
{
  MinimizeItem->Visible = value;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetReadOnly(bool value)
{
  if (FReadOnly != value)
  {
    FReadOnly = value;
    if (!value)
    {
      ResetOnceDoneOperation();
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::ResetOnceDoneOperation()
{
  SetOnceDoneOperation(odoIdle);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CMDialogKey(TCMDialogKey & Message)
{
  if (Message.CharCode == VK_TAB)
  {
    Toolbar->KeyboardOpen(L'\0', false);
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::Dispatch(void * AMessage)
{
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if (Message.Msg == WM_CLOSE)
  {
    CancelOperation();
  }
  else if (Message.Msg == CM_DIALOGKEY)
  {
    CMDialogKey(reinterpret_cast<TCMDialogKey &>(Message));
  }
  else if (Message.Msg == WM_MANAGES_CAPTION)
  {
    Message.Result = 1;
  }
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MoveToQueueItemClick(TObject * /*Sender*/)
{
  FMoveToQueue = true;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::OnceDoneItemClick(TObject * Sender)
{
  SetOnceDoneItem(dynamic_cast<TTBCustomItem *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CycleOnceDoneItemClick(TObject * /*Sender*/)
{
  TTBCustomItem * Item = CurrentOnceDoneItem();
  int Index = Item->Parent->IndexOf(Item);
  DebugAssert(Index >= 0);
  if (Index < Item->Parent->Count - 1)
  {
    Index++;
  }
  else
  {
    Index = 0;
  }
  SetOnceDoneItem(Item->Parent->Items[Index]);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TProgressForm::ItemSpeed(const UnicodeString & Text,
  TTBXComboBoxItem * Item)
{
  // Keep in sync with TNonVisualDataModule::QueueItemSpeed
  FCPSLimit = GetSpeedLimit(Text);

  UnicodeString Result = SetSpeedLimit(FCPSLimit);
  SaveToHistory(Item->Strings, Result);
  CustomWinConfiguration->History[L"SpeedLimit"] = Item->Strings;
  FWheelDelta = 0;

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboBoxItemAcceptText(TObject * Sender,
  UnicodeString & NewText, bool & /*Accept*/)
{
  TTBXComboBoxItem * Item = dynamic_cast<TTBXComboBoxItem *>(Sender);
  NewText = ItemSpeed(NewText, Item);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboBoxItemItemClick(TObject * Sender)
{
  TTBXComboBoxItem * Item = dynamic_cast<TTBXComboBoxItem *>(Sender);
  Item->Text = ItemSpeed(Item->Text, Item);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboBoxItemAdjustImageIndex(
  TTBXComboBoxItem * Sender, const UnicodeString /*AText*/, int /*AIndex*/, int & ImageIndex)
{
  // Use fixed image (do not change image by item index)
  ImageIndex = Sender->ImageIndex;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboBoxItemClick(TObject * Sender)
{
  ClickToolbarItem(DebugNotNull(dynamic_cast<TTBCustomItem *>(Sender)), false);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::ClearCancel()
{
  if (DebugAlwaysTrue(FCancel == csCancelFile))
  {
    FPendingSkip = true;
  }
  FCancel = csContinue;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetCancelLower(TCancelStatus ACancel)
{
  if (FCancel < ACancel)
  {
    FCancel = ACancel;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SkipItemClick(TObject * /*Sender*/)
{
  SetCancelLower(csCancelFile);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MouseWheelHandler(TMessage & Message)
{
  int X = GET_X_LPARAM(Message.LParam);
  int Y = GET_Y_LPARAM(Message.LParam);
  TPoint P = Toolbar->ScreenToClient(TPoint(X, Y));
  if (Toolbar->ClientRect.Contains(P))
  {
    TTBItemViewer * Viewer = Toolbar->View->Find(SpeedComboBoxItem);
    if (Viewer->BoundsRect.Contains(P))
    {
      int Delta = GET_WHEEL_DELTA_WPARAM(Message.WParam);
      FWheelDelta += Delta;
      unsigned long CurrentSpeed = GetSpeedLimit(SpeedComboBoxItem->Text);
      unsigned long Speed = CurrentSpeed;
      unsigned int CPS = FData.CPS();
      int Step = 4 * WHEEL_DELTA;
      if (FWheelDelta > 0)
      {
        while (FWheelDelta > Step)
        {
          if (Speed > 0)
          {
            Speed *= 2;
            if (Speed > std::max(MaxSpeed, static_cast<unsigned long>(CPS) * 2))
            {
              Speed = 0;
            }
          }
          FWheelDelta -= Step;
        }
      }
      else if (FWheelDelta < 0)
      {
        while (FWheelDelta < -Step)
        {
          if (Speed == 0)
          {
            Speed = 8;
            while (Speed * 2 < CPS)
            {
              Speed *= 2;
            }
          }
          else
          {
            if (Speed > MinSpeed)
            {
              Speed /= 2;
            }
          }
          FWheelDelta += Step;
        }
      }

      if (Speed != CurrentSpeed)
      {
        TTBEditItemViewer * EditViewer = dynamic_cast<TTBEditItemViewer *>(Viewer);
        if (EditViewer->EditControl != NULL)
        {
          EditViewer->View->CancelMode();
        }

        FCPSLimit = Speed;
        SpeedComboBoxItem->Text = SetSpeedLimit(Speed);
      }

      Message.Result = 1;
    }
  }

  TForm::MouseWheelHandler(Message);
}
//---------------------------------------------------------------------------
