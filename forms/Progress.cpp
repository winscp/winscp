//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <AssociatedStatusBar.hpp>

#include <Common.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>
#include <WinInterface.h>

#include "Progress.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "PathLabel"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
__fastcall TProgressForm::TProgressForm(TComponent* AOwner)
	: FData(), TForm(AOwner)
{
  FLastOperation = foNone;
  FDataReceived = false;
  FAsciiTransferChanged = false;
  FResumeStatusChanged = false;
  FCancel = csContinue;
  FMinimizedByMe = false;
  FUpdateCounter = 0;
  FLastUpdate = 0;
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TProgressForm::~TProgressForm()
{
  // to prevent raising assertion (e.g. IsProgress == True)
  FData.Clear();
  if (IsIconic(Application->Handle) && FMinimizedByMe)
    Application->Restore();
    
  if (FFormState.Contains(fsModal))
  {
    HideAsModal();
  }
}
//---------------------------------------------------------------------
void __fastcall TProgressForm::UpdateControls()
{
  assert((FData.Operation >= foCopy) && (FData.Operation <= foSetProperties));
  bool TransferOperation =
    ((FData.Operation == foCopy) || (FData.Operation == foMove));
    
  if (FData.Operation != FLastOperation)
  {
    bool AVisible = (FData.Operation != foSetProperties);
    int Delta = 0;
    if (AVisible && !Animate->Visible) Delta = Animate->Height;
      else
    if (!AVisible && Animate->Visible) Delta = -Animate->Height;

    MainPanel->Top = MainPanel->Top + Delta;
    TransferPanel->Top = TransferPanel->Top + Delta;
    SpeedPanel->Top = SpeedPanel->Top + Delta;
    Animate->Visible = AVisible;

    if (TransferOperation && !TransferPanel->Visible) Delta += TransferPanel->Height;
      else
    if (!TransferOperation && TransferPanel->Visible) Delta += -TransferPanel->Height;
    TransferPanel->Visible = TransferOperation;
    SpeedPanel->Visible = TransferOperation && WinConfiguration->ExpertMode;

    ClientHeight = ClientHeight + Delta;
    DisconnectWhenCompleteCheck->Top = DisconnectWhenCompleteCheck->Top + Delta;

    try
    {
      switch (FData.Operation) {
        case foCopy:
        case foMove:
          if (FData.Count == 1) Animate->CommonAVI = aviCopyFile;
            else Animate->CommonAVI = aviCopyFiles;
          break;

        case foDelete:
          if ((FData.Side == osLocal) && WinConfiguration->DeleteToRecycleBin)
            Animate->CommonAVI = aviRecycleFile;
          else
            Animate->CommonAVI = aviDeleteFile;
          break;

        default:
          assert(FData.Operation == foSetProperties);
          Animate->CommonAVI = aviNone;
      }
      Animate->Active = (Animate->CommonAVI != aviNone);
    }
    catch (...)
    {
    };

    const int Captions[] = {PROGRESS_COPY, PROGRESS_MOVE, PROGRESS_DELETE, PROGRESS_SETPROPERTIES};
    Caption = LoadStr(Captions[(int)FData.Operation - 1]);

    TargetLabel->Visible = TransferOperation;
    TargetPathLabel->Visible = TransferOperation;
    TargetPathLabel->UnixPath = (FData.Side == osLocal);

    FileLabel->UnixPath = (FData.Side == osRemote);
    
    FLastOperation = FData.Operation;
  };

  FileLabel->Caption = FData.FileName;
  OperationProgress->Position = FData.OperationProgress();
  OperationProgress->Hint = FORMAT("%d%%", (OperationProgress->Position));

  if (TransferOperation)
  {
    if (!FData.DragDrop || (FData.Side == osLocal))
    {
      TargetPathLabel->Caption = FData.Directory;
    }
    else
    {
      TargetPathLabel->Caption = LoadStr(PROGRESS_DRAGDROP_TARGET);
    }
    StartTimeLabel->Caption = FData.StartTime.TimeString();
    TimeElapsedLabel->Caption = FormatDateTime(Configuration->TimeFormat, FData.TimeElapsed());
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
    ShowAsModal(); 
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
  MinimizeApp();
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
          (FData.TimeExpected() > Configuration->IgnoreCancelBeforeFinish))
      {
        Result = MessageDialog(LoadStr(CANCEL_OPERATION_FATAL), qtWarning,
          qaYes | qaNo | qaCancel, 0);
      }
      else
      {
        Result = MessageDialog(LoadStr(CANCEL_OPERATION), qtConfirmation,
          qaOK | qaCancel, 0);
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
void __fastcall TProgressForm::ShowAsModal()
{
  // method duplicated in TOperationStatusForm
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  FFormState << fsModal;
  FFocusActiveWindow = GetActiveWindow();

  FFocusWindowList = DisableTaskWindows(0);
  Show();
  SendMessage(Handle, CM_ACTIVATE, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::HideAsModal()
{
  // method duplicated in TOperationStatusForm
  assert(FFormState.Contains(fsModal));
  SendMessage(Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Handle)
  {
    FFocusActiveWindow = 0;
  }
  Hide();

  EnableTaskWindows(FFocusWindowList);

  if (FFocusActiveWindow != 0)
  {
    SetActiveWindow(FFocusActiveWindow);
  }

  FFormState >> fsModal;
}


