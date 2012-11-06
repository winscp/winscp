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
UnicodeString __fastcall TProgressForm::OperationName(TFileOperation Operation)
{
  static const int Captions[] = { PROGRESS_COPY, PROGRESS_MOVE, PROGRESS_DELETE,
    PROGRESS_SETPROPERTIES, 0, PROGRESS_CUSTOM_COMAND, PROGRESS_CALCULATE_SIZE,
    PROGRESS_REMOTE_MOVE, PROGRESS_REMOTE_COPY, PROGRESS_GETPROPERTIES,
    PROGRESS_CALCULATE_CHECKSUM };
  assert((unsigned int)Operation >= 1 && ((unsigned int)Operation - 1) < LENOF(Captions));
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

  if (IsApplicationMinimized() && FMinimizedByMe)
  {
    ShowNotification(NULL, LoadStr(BALLOON_OPERATION_COMPLETE), qtInformation);
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
  FOperationProgress->Position = OverallProgress;
  FOperationProgress->Hint = FORMAT(L"%d%%", (OverallProgress));
  Caption = FORMAT(L"%d%% %s", (OverallProgress, OperationName(FData.Operation)));

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
void __fastcall TProgressForm::SetProgressData(TFileOperationProgressType & AData)
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
    // CPS limit is set set only once from TFileOperationProgressType::Start
    FCPSLimit = AData.CPSLimit;
    SpeedCombo->Text = SetSpeedLimit(FCPSLimit);
    ShowAsModal(this, FShowAsModalStorage);
  }

  if (InstantUpdate)
  {
    UpdateControls();
    Application->ProcessMessages();
  }
  TDateTime N = Now();
  static double UpdateInterval = static_cast<double>(OneSecond*5);  // 1/5 sec
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
  if (FDataReceived) UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::FormShow(TObject * /*Sender*/)
{
  UpdateTimer->Enabled = true;
  SpeedCombo->Items = CustomWinConfiguration->History[L"SpeedLimit"];
  if (FDataReceived) UpdateControls();
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
  GetGlobalMinimizeHandler()(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::CancelOperation()
{
  // partially duplicated in TWinSCPFileSystem::CancelConfiguration (far\WinSCPFileSystem)
  assert(FDataReceived);
  if (!FData.Suspended)
  {
    // mostly useless, as suspend is called over copy of actual progress data
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
      assert(false);
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
      assert(false);
  }
}
//---------------------------------------------------------------------------
void __fastcall TProgressForm::OnceDoneOperationComboCloseUp(TObject * /*Sender*/)
{
  CancelButton->SetFocus();
}
//---------------------------------------------------------------------------
#pragma warn -8080
