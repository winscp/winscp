//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <WinInterface.h>
#include <VCLCommon.h>
#include <GUIConfiguration.h>
#include <GUITools.h>

#include "Progress.h"
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
AnsiString __fastcall TProgressForm::OperationName(TFileOperation Operation)
{
  static const int Captions[] = { PROGRESS_COPY, PROGRESS_MOVE, PROGRESS_DELETE,
    PROGRESS_SETPROPERTIES, 0, PROGRESS_CUSTOM_COMAND, PROGRESS_CALCULATE_SIZE,
    PROGRESS_REMOTE_MOVE, PROGRESS_REMOTE_COPY, PROGRESS_GETPROPERTIES,
    PROGRESS_CALCULATE_CHECKSUM };
  assert((int)Operation >= 1 && ((int)Operation - 1) < LENOF(Captions));
  return LoadStr(Captions[(int)Operation - 1]);
}
//---------------------------------------------------------------------
__fastcall TProgressForm::TProgressForm(TComponent* AOwner)
    : FData(), TForm(AOwner)
{
  FLastOperation = foNone;
  FLastTotalSizeSet = false;
  FDataReceived = false;
  FAsciiTransferChanged = false;
  FResumeStatusChanged = false;
  FCancel = csContinue;
  FMinimizedByMe = false;
  FUpdateCounter = 0;
  FLastUpdate = 0;
  FDeleteToRecycleBin = false;
  FReadOnly = false;
  FShowAsModalStorage = NULL;
  UseSystemSettings(this);
  if (!IsGlobalMinimizeHandler())
  {
    SetGlobalMinimizeHandler(GlobalMinimize);
  };
}
//---------------------------------------------------------------------------
__fastcall TProgressForm::~TProgressForm()
{
  // to prevent raising assertion (e.g. IsProgress == True)
  FData.Clear();

  if (GetGlobalMinimizeHandler() == GlobalMinimize)
  {
    SetGlobalMinimizeHandler(NULL);
  }

  if (IsIconic(Application->Handle) && FMinimizedByMe)
  {
    Application->Restore();
    Application->BringToFront();
  }

  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::UpdateControls()
{
  assert((FData.Operation >= foCopy) && (FData.Operation <= foCalculateChecksum) &&
    FData.Operation != foRename );

  CancelButton->Enabled = !FReadOnly;
  DisconnectWhenCompleteCheck->Enabled =
    !FReadOnly && (FData.Operation != foCalculateSize) &&
    (FData.Operation != foGetProperties) &&
    (FData.Operation != foCalculateChecksum);

  bool TransferOperation =
    ((FData.Operation == foCopy) || (FData.Operation == foMove));

  if (FData.Operation != FLastOperation)
  {
    bool AVisible;
    THandle ShellModule;

    try
    {
      AVisible = true;
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
          ShellModule = SafeLoadLibrary("shell32.dll");
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

        default:
          assert(FData.Operation == foCustomCommand ||
            FData.Operation == foCalculateSize ||
            FData.Operation == foCalculateChecksum);
          Animate->CommonAVI = aviNone;
          AVisible = false;
      }
    }
    catch (...)
    {
      AVisible = false;
    };

    int Delta = 0;
    if (AVisible && !Animate->Visible) Delta = Animate->Height;
      else
    if (!AVisible && Animate->Visible) Delta = -Animate->Height;

    MainPanel->Top = MainPanel->Top + Delta;
    TransferPanel->Top = TransferPanel->Top + Delta;
    SpeedPanel->Top = SpeedPanel->Top + Delta;
    Animate->Visible = AVisible;
    Animate->Active = AVisible;

    if (TransferOperation && !TransferPanel->Visible) Delta += TransferPanel->Height;
      else
    if (!TransferOperation && TransferPanel->Visible) Delta += -TransferPanel->Height;
    TransferPanel->Visible = TransferOperation;
    SpeedPanel->Visible = TransferOperation;

    ClientHeight = ClientHeight + Delta;
    DisconnectWhenCompleteCheck->Top = DisconnectWhenCompleteCheck->Top + Delta;

    Caption = OperationName(FData.Operation);

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
  OperationProgress->Position = OverallProgress;
  OperationProgress->Hint = FORMAT("%d%%", (OverallProgress));
  Caption = FORMAT("%d%% %s", (OverallProgress, OperationName(FData.Operation)));

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
    CPSLabel->Caption = FORMAT("%s/s", (FormatBytes(FData.CPS())));
    FileProgress->Position = FData.TransferProgress();
    FileProgress->Hint = FORMAT("%d%%", (FileProgress->Position));

    TransferModeLabel->Caption =
      LoadStr(FData.AsciiTransfer ? TRANSFER_ASCII : TRANSFER_BINARY);

    AnsiString ResumeStatusStr;
    switch (FData.ResumeStatus) {
      case rsEnabled: ResumeStatusStr = LoadStr(RESUME_ENABLED); break;
      case rsDisabled: ResumeStatusStr = LoadStr(RESUME_DISABLED); break;
      case rsNotAvailable: ResumeStatusStr = LoadStr(RESUME_NOT_AVAILABLE); break;
      default:  assert(false);
    }
    ResumeLabel->Caption = ResumeStatusStr;
  }
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::SetProgressData(const TFileOperationProgressType &AData)
{
  bool InstantUpdate = false;

  // workaround: to force displaing first file data immediatelly,
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
  if (!FDataReceived)
  {
    FDataReceived = true;
    ShowAsModal(this, FShowAsModalStorage);
  }

  if (InstantUpdate)
  {
    UpdateControls();
    Application->ProcessMessages();
  }
  TDateTime N = Now();
  if ((double)FLastUpdate && SpeedPanel->Visible && SpeedBar->Position < 100)
  {
    assert(SpeedBar->Position > 0);
    __int64 MilliSecondsTr = (double(N) - double(FLastUpdate)) * MSecsPerDay;
    int MilliSecondsWait = int(double(MilliSecondsTr > 100000 ? 100000 : MilliSecondsTr) *
      (double(100)/double(SpeedBar->Position) - 1));
    MilliSecondsWait = (MilliSecondsWait > 2000) ? 2000 : MilliSecondsWait;
    while (MilliSecondsWait > 0 && FCancel == csContinue)
    {
      SleepEx(MilliSecondsWait > 300 ? 300 : MilliSecondsWait, true);
      MilliSecondsWait -= 300;
      Application->ProcessMessages();
    }
  }
  static double UpdateInterval = double(1)/(24*60*60*5);  // 1/5 sec
  if ((FUpdateCounter % 5 == 0) ||
      (double(N) - double(FLastUpdate) > UpdateInterval))
  {
    FLastUpdate = N;
    FUpdateCounter = 0;
    Application->ProcessMessages();
  }
  FUpdateCounter++;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::UpdateTimerTimer(TObject * /*Sender*/)
{
  if (FDataReceived) UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormShow(TObject * /*Sender*/)
{
  UpdateTimer->Enabled = true;
  if (FDataReceived) UpdateControls();
  SpeedBar->Position = 100;
  FLastUpdate = 0;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormHide(TObject * /*Sender*/)
{
  UpdateTimer->Enabled = false;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelButtonClick(TObject * /*Sender*/)
{
  CancelOperation();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::MinimizeButtonClick(TObject * /*Sender*/)
{
  TNotifyEvent OnMinimize = GetGlobalMinimizeHandler();
  if (OnMinimize != NULL)
  {
    OnMinimize(this);
  }
  else
  {
    MinimizeApp();
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelOperation()
{
  // partially duplicated in TWinSCPFileSystem::CancelConfiguration (far\WinSCPFileSystem)
  assert(FDataReceived);
  if (!FData.Suspended)
  {
    FData.Suspend();
    UpdateControls();
    try
    {
      TCancelStatus ACancel;
      int Result;
      if (FData.TransferingFile &&
          (FData.TimeExpected() > GUIConfiguration->IgnoreCancelBeforeFinish))
      {
        Result = MessageDialog(LoadStr(CANCEL_OPERATION_FATAL), qtWarning,
          qaYes | qaNo | qaCancel, HELP_PROGRESS_CANCEL);
      }
      else
      {
        Result = MessageDialog(LoadStr(CANCEL_OPERATION), qtConfirmation,
          qaOK | qaCancel, HELP_PROGRESS_CANCEL);
      }
      switch (Result) {
        case qaYes:
          ACancel = csCancelTransfer; break;
        case qaOK:
        case qaNo:
          ACancel = csCancel; break;
        default:
          ACancel = csContinue; break;
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
void __fastcall TProgressForm::MinimizeApp()
{
  Application->Minimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::GlobalMinimize(TObject * /*Sender*/)
{
  MinimizeApp();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::SetDisconnectWhenComplete(bool value)
{
  DisconnectWhenCompleteCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TProgressForm::GetDisconnectWhenComplete()
{
  return DisconnectWhenCompleteCheck->Checked;
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
      DisconnectWhenCompleteCheck->Checked = false;
    }
    UpdateControls();
  }
}
