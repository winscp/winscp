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

#include "Progress.h"
//---------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma link "PathLabel"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
UnicodeString __fastcall TProgressForm::OperationName(TFileOperation Operation, TOperationSide Side)
{
  static const int Captions[] = { 0, 0, PROGRESS_DELETE,
    PROGRESS_SETPROPERTIES, 0, PROGRESS_CUSTOM_COMAND, PROGRESS_CALCULATE_SIZE,
    PROGRESS_REMOTE_MOVE, PROGRESS_REMOTE_COPY, PROGRESS_GETPROPERTIES,
    PROGRESS_CALCULATE_CHECKSUM };
  assert((unsigned int)Operation >= 1 && ((unsigned int)Operation - 1) < LENOF(Captions));
  int Id;
  if ((Operation == foCopy) || (Operation == foMove))
  {
    Id = (Side == osLocal) ? PROGRESS_UPLOAD : PROGRESS_DOWNLOAD;
  }
  else
  {
    Id = Captions[(int)Operation - 1];
    assert(Id != 0);
  }
  return LoadStr(Id);
}
//---------------------------------------------------------------------
__fastcall TProgressForm::TProgressForm(TComponent * AOwner, bool AllowMoveToQueue)
    : FData(), TForm(AOwner)
{
  FLastOperation = foNone;
  FLastTotalSizeSet = false;
  FDataGot = false;
  FDataReceived = false;
  FAsciiTransferChanged = false;
  FResumeStatusChanged = false;
  FCancel = csContinue;
  FMoveToQueue = false;
  FMinimizedByMe = false;
  FUpdateCounter = 0;
  FLastUpdate = 0;
  FDeleteToRecycleBin = false;
  FReadOnly = false;
  FShowAsModalStorage = NULL;
  FStarted = Now();
  FModalBeginHooked = false;
  FPrevApplicationModalBegin = NULL;
  FModalLevel = -1;
  FAllowMoveToQueue = AllowMoveToQueue;
  UseSystemSettings(this);
  ResetOnceDoneOperation();

  if (CustomWinConfiguration->OperationProgressOnTop)
  {
    FOperationProgress = TopProgress;
    FFileProgress = BottomProgress;
  }
  else
  {
    FOperationProgress = BottomProgress;
    FFileProgress = TopProgress;
  }

  SetGlobalMinimizeHandler(this, GlobalMinimize);
  if (FAllowMoveToQueue)
  {
    MenuButton(MinimizeButton);
  }
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

  if (FModalBeginHooked)
  {
    assert(Application->OnModalBegin == ApplicationModalBegin);
    Application->OnModalBegin = FPrevApplicationModalBegin;
    FModalBeginHooked = false;
  }

  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::UpdateControls()
{
  assert((FData.Operation >= foCopy) && (FData.Operation <= foCalculateChecksum) &&
    FData.Operation != foRename );

  CancelButton->Enabled = !FReadOnly;
  OnceDoneOperationCombo->Enabled =
    !FReadOnly && (FData.Operation != foCalculateSize) &&
    (FData.Operation != foGetProperties) &&
    (FData.Operation != foCalculateChecksum);
  OnceDoneOperationLabel->Enabled = OnceDoneOperationCombo->Enabled;

  bool TransferOperation =
    ((FData.Operation == foCopy) || (FData.Operation == foMove));

  if (FData.Operation != FLastOperation)
  {
    bool AVisible;

    // Wine does have static text "Searching" instead of actual animations
    if (!IsWine())
    {
      try
      {
        THandle ShellModule;
        AVisible = true;
        TProgressBarStyle Style = pbstNormal;
        switch (FData.Operation) {
          case foCopy:
          case foMove:
          case foRemoteMove:
          case foRemoteCopy:
            if (FData.Count == 1) Animate->CommonAVI = aviCopyFile;
              else Animate->CommonAVI = aviCopyFiles;
            break;

          case foDelete:
            Animate->CommonAVI = (DeleteToRecycleBin ? aviRecycleFile : aviDeleteFile);
            break;

          case foSetProperties:
          case foGetProperties:
            ShellModule = SafeLoadLibrary(L"shell32.dll");
            if (!ShellModule)
            {
              Abort();
            }
            // workaround, VCL is not able to set both ResId and ResHandle otherwise
            Animate->Active = false;
            Animate->ResHandle = 0;
            Animate->ComponentState << csLoading;
            Animate->ResId = 165;
            Animate->ResHandle = ShellModule;
            Animate->ComponentState >> csLoading;
            Animate->Active = true;
            break;

          case foCalculateSize:
            Animate->CommonAVI = aviNone;
            AVisible = false;
            Style = pbstMarquee;
            break;

          default:
            assert(FData.Operation == foCustomCommand ||
              FData.Operation == foCalculateChecksum);
            Animate->CommonAVI = aviNone;
            AVisible = false;
        }
        TopProgress->Style = Style;
      }
      catch (...)
      {
        AVisible = false;
      };
    }
    else
    {
      Animate->CommonAVI = aviNone;
      AVisible = false;
    }

    int Delta = 0;
    if (AVisible && !Animate->Visible) Delta = Animate->Height;
      else
    if (!AVisible && Animate->Visible) Delta = -Animate->Height;

    MainPanel->Top = MainPanel->Top + Delta;
    TransferPanel->Top = TransferPanel->Top + Delta;
    Animate->Visible = AVisible;
    Animate->Active = AVisible;

    if (TransferOperation && !TransferPanel->Visible) Delta += TransferPanel->Height;
      else
    if (!TransferOperation && TransferPanel->Visible) Delta += -TransferPanel->Height;
    TransferPanel->Visible = TransferOperation;
    // when animation is hidden for transfers (on wine) speed panel does not fit,
    // so we hide it (temporary solution before window redesign)
    SpeedPanel->Visible = TransferOperation && AVisible;

    ClientHeight = ClientHeight + Delta;

    TargetLabel->Visible = TransferOperation;
    TargetPathLabel->Visible = TransferOperation;
    TargetPathLabel->UnixPath = (FData.Side == osLocal);

    FileLabel->UnixPath = (FData.Side == osRemote);

    FLastOperation = FData.Operation;
    FLastTotalSizeSet = !FData.TotalSizeSet;
  };

  if (FLastTotalSizeSet != FData.TotalSizeSet)
  {
    StartTimeLabelLabel->Visible = !FData.TotalSizeSet;
    StartTimeLabel->Visible = !FData.TotalSizeSet;
    TimeLeftLabelLabel->Visible = FData.TotalSizeSet;
    TimeLeftLabel->Visible = FData.TotalSizeSet;
    FLastTotalSizeSet = FData.TotalSizeSet;
  }

  if ((FData.Side == osRemote) || !FData.Temp)
  {
    FileLabel->Caption = FData.FileName;
  }
  else
  {
    FileLabel->Caption = ExtractFileName(FData.FileName);
  }
  int OverallProgress = FData.OverallProgress();
  FOperationProgress->Position = OverallProgress;
  FOperationProgress->Hint = FORMAT(L"%d%%", (OverallProgress));
  Caption = FormatFormCaption(this, FORMAT(L"%d%% %s", (OverallProgress, OperationName(FData.Operation, FData.Side))));

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
      TimeLeftLabel->Caption = FormatDateTimeSpan(Configuration->TimeFormat,
        FData.TotalTimeLeft());
    }
    TimeElapsedLabel->Caption = FormatDateTimeSpan(Configuration->TimeFormat, FData.TimeElapsed());
    BytesTransferedLabel->Caption = FormatBytes(FData.TotalTransfered);
    CPSLabel->Caption = FORMAT(L"%s/s", (FormatBytes(FData.CPS())));
    FFileProgress->Position = FData.TransferProgress();
    FFileProgress->Hint = FORMAT(L"%d%%", (FFileProgress->Position));
  }
}
//---------------------------------------------------------------------
static TDateTime DelayStartInterval(static_cast<double>(OneSecond/5));
static TDateTime UpdateInterval(static_cast<double>(OneSecond));
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
      // delay showing the progress until the application is restored,
      // otherwise the form popups up unminimized.
      if (!IsApplicationMinimized() &&
          (Force || ((Now() - FStarted) > DelayStartInterval)))
      {
        FDataReceived = true;
        SpeedCombo->Text = SetSpeedLimit(FCPSLimit);
        ShowAsModal(this, FShowAsModalStorage);
        // particularly needed for the case, when we are showing the form delayed
        // because application was minimized when operation started
        Result = true;
      }
      else if (!FModalBeginHooked && ALWAYS_TRUE(FModalLevel < 0))
      {
        // record state as of time, the window should be shown,
        // had not we implemented delayed show
        FPrevApplicationModalBegin = Application->OnModalBegin;
        Application->OnModalBegin = ApplicationModalBegin;
        FModalBeginHooked = true;
        FModalLevel = Application->ModalLevel;
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::ApplicationModalBegin(TObject * Sender)
{
  // Popup before any modal dialog shows (typically overwrite confirmation,
  // as that popups nerly instantly, i.e. less than DelayStartInterval).
  // The Application->ModalLevel is already incremented, but we should treat is as
  // if it were not as the dialog is not created yet (so we can popup if we are not yet).
  ReceiveData(true, -1);

  if (FPrevApplicationModalBegin != NULL)
  {
    FPrevApplicationModalBegin(Sender);
  }
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::SetProgressData(TFileOperationProgressType & AData)
{
  TDateTime N = Now();
  bool InstantUpdate = false;

  // workaround: to force displaing first file data immediately,
  // otherwise form dialog uses to be blank for first second
  // (until UpdateTimerTimer)
  if (FileLabel->Caption.IsEmpty() && !AData.FileName.IsEmpty())
  {
    InstantUpdate = true;
  }

  if (!FAsciiTransferChanged && FData.AsciiTransfer != AData.AsciiTransfer)
  {
    FAsciiTransferChanged = true;
    InstantUpdate = true;
  }

  if (!FResumeStatusChanged && FData.ResumeStatus != AData.ResumeStatus)
  {
    FResumeStatusChanged = true;
    InstantUpdate = true;
  }

  FData = AData;
  FDataGot = true;
  if (!UpdateTimer->Enabled)
  {
    UpdateTimer->Interval = static_cast<unsigned int>(MilliSecondsBetween(TDateTime(), DelayStartInterval));
    UpdateTimer->Enabled = true;
    FSinceLastUpdate = TDateTime();
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
  static double UpdateInterval = static_cast<double>(OneSecond/5);  // 1/5 sec
  if ((FUpdateCounter % 5 == 0) ||
      (double(N) - double(FLastUpdate) > UpdateInterval))
  {
    FLastUpdate = N;
    FUpdateCounter = 0;
    Application->ProcessMessages();
  }
  FUpdateCounter++;

  AData.CPSLimit = FCPSLimit;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::UpdateTimerTimer(TObject * /*Sender*/)
{
  // popup the progress window at least here, if SetProgressData is
  // not being called (typically this happens when using custom command
  // that launches long-lasting external process, such as visual diff)
  ReceiveData(false, 0);

  if (FDataReceived)
  {
    FSinceLastUpdate = IncMilliSecond(FSinceLastUpdate, UpdateTimer->Interval);
    if (FSinceLastUpdate >= UpdateInterval)
    {
      UpdateControls();
      FSinceLastUpdate = TDateTime();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormShow(TObject * /*Sender*/)
{
  SpeedCombo->Items = CustomWinConfiguration->History[L"SpeedLimit"];
  ReceiveData(false, 0);
  if (FDataReceived)
  {
    UpdateControls();
  }
  FLastUpdate = 0;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormHide(TObject * /*Sender*/)
{
  // This is to counter the "infinite" timestamp in
  // TTerminalManager::ApplicationShowHint.
  // Because if form disappears on its own, hint is not hidden.
  Application->CancelHint();
  CustomWinConfiguration->History[L"SpeedLimit"] = SpeedCombo->Items;
  UpdateTimer->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelButtonClick(TObject * /*Sender*/)
{
  CancelOperation();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MinimizeButtonClick(TObject * Sender)
{
  if (FAllowMoveToQueue)
  {
    MenuPopup(MinimizeMenu, MinimizeButton);
  }
  else
  {
    Minimize(Sender);
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::Minimize(TObject * Sender)
{
  CallGlobalMinimizeHandler(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MinimizeMenuItemClick(TObject * Sender)
{
  Minimize(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelOperation()
{
  assert(FDataReceived);
  if (!FData.Suspended)
  {
    // mostly useless, as suspend is called over copy of actual progress data
    FData.Suspend();
    UpdateControls();
    try
    {
      TCancelStatus ACancel;
      if (FData.TransferingFile &&
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
        int Result = MessageDialog(LoadStr(CANCEL_OPERATION2), qtConfirmation,
          qaYes | qaNo, HELP_PROGRESS_CANCEL);
        switch (Result)
        {
          case qaYes:
            ACancel = csCancel; break;
          default:
            ACancel = csContinue; break;
        }
      }

      if (FCancel < ACancel)
      {
        FCancel = ACancel;
      }
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
void __fastcall TProgressForm::SetOnceDoneOperation(TOnceDoneOperation value)
{
  int Index = 0;
  switch (value)
  {
    case odoIdle:
      Index = 0;
      break;

    case odoDisconnect:
      Index = 1;
      break;

    case odoShutDown:
      Index = 2;
      break;

    default:
      FAIL;
  }
  OnceDoneOperationCombo->ItemIndex = Index;
  OnceDoneOperationComboSelect(NULL);
}
//---------------------------------------------------------------------------
bool __fastcall TProgressForm::GetAllowMinimize()
{
  return MinimizeButton->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetAllowMinimize(bool value)
{
  MinimizeButton->Visible = value;
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
void __fastcall TProgressForm::ApplyCPSLimit()
{
  try
  {
    FCPSLimit = GetSpeedLimit(SpeedCombo->Text);
  }
  catch(...)
  {
    SpeedCombo->SetFocus();
    throw;
  }

  SpeedCombo->Text = SetSpeedLimit(FCPSLimit);
  // visualize application
  SpeedCombo->SelectAll();
  CancelButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboExit(TObject * /*Sender*/)
{
  SpeedCombo->Text = SetSpeedLimit(FCPSLimit);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboSelect(TObject * /*Sender*/)
{
  ApplyCPSLimit();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SpeedComboKeyPress(TObject * /*Sender*/,
  wchar_t & Key)
{
  // using OnKeyPress instead of OnKeyDown to catch "enter" prevents
  // system beep for unhandled key
  if (Key == L'\r')
  {
    Key = L'\0';
    ApplyCPSLimit();
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::ResetOnceDoneOperation()
{
  OnceDoneOperationCombo->ItemIndex = 0;
  OnceDoneOperationComboSelect(NULL);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::OnceDoneOperationComboSelect(TObject * /*Sender*/)
{
  switch (OnceDoneOperationCombo->ItemIndex)
  {
    case 0:
      FOnceDoneOperation = odoIdle;
      break;

    case 1:
      FOnceDoneOperation = odoDisconnect;
      break;

    case 2:
      FOnceDoneOperation = odoShutDown;
      break;

    default:
      FAIL;
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::OnceDoneOperationComboCloseUp(TObject * /*Sender*/)
{
  CancelButton->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::Dispatch(void * AMessage)
{
  TMessage & Message = *reinterpret_cast<TMessage *>(AMessage);
  if (Message.Msg == WM_CLOSE)
  {
    CancelOperation();
  }
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MoveToQueueMenuItemClick(TObject * /*Sender*/)
{
  FMoveToQueue = true;
}
//---------------------------------------------------------------------------
