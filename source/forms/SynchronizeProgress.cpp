//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <WinInterface.h>
#include "SynchronizeProgress.h"
#include <Common.h>
#include <Configuration.h>
#include <CoreMain.h>
#include <TextsWin.h>
#include <VCLCommon.h>
#include <GUITools.h>
#include <Progress.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PathLabel"
#pragma link "PngImageList"
#pragma link "TB2Dock"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBX"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
// Used for comparing only
__fastcall TSynchronizeProgressForm::TSynchronizeProgressForm(TComponent * Owner, bool AllowMinimize, int Files)
  : TForm(Owner)
{
  FStarted = false;
  FCanceled = false;
  FShowAsModalStorage = NULL;
  FMinimizedByMe = false;
  bool KnowsTotalFiles = (Files >= 0);
  OperationProgress->Style = (KnowsTotalFiles ? pbstNormal : pbstMarquee);
  OperationProgress->Max = (KnowsTotalFiles ? Files : 1);
  TimeLeftLabelLabel->Visible = KnowsTotalFiles;
  TimeLeftLabel->Visible = KnowsTotalFiles;
  StartTimeLabelLabel->Visible = !KnowsTotalFiles;
  StartTimeLabel->Visible = !KnowsTotalFiles;
  UseSystemSettings(this);
  HideComponentsPanel(this);
  SelectScaledImageList(ImageList);
  if (!AllowMinimize)
  {
    MinimizeItem->Visible = false;
  }
  else
  {
    SetGlobalMinimizeHandler(this, GlobalMinimize);
  }
  FFrameAnimation.Init(AnimationPaintBox, L"SynchronizeDirectories");
}
//---------------------------------------------------------------------------
__fastcall TSynchronizeProgressForm::~TSynchronizeProgressForm()
{
  ClearGlobalMinimizeHandler(GlobalMinimize);

  ReleaseAsModal(this, FShowAsModalStorage);
  UnhookFormActivation(this);

  if (IsApplicationMinimized() && FMinimizedByMe)
  {
    ShowNotification(
      NULL, MainInstructions(LoadStr(BALLOON_OPERATION_COMPLETE)),
      qtInformation);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::Start()
{
  FStarted = true;
  FStartTime = Now();
  UpdateTimer->Enabled = true;
  StartTimeLabel->Caption = FStartTime.TimeString();
  OperationProgress->Position = OperationProgress->Min;
  UpdateControls();
  if (!IsApplicationMinimized())
  {
    // Do not show the progress when the application is minimized,
    // otherwise the form popups up unminimized.
    // Quick and dirty hack: with this form, we do not support showing it
    // once the application restores,
    // otherwise we would have to synchronize it somehow with the TProgressForm,
    // not to show it over the TProgressForm
    // See solution in TMessageForm::CMShowingChanged.
    ShowAsModal(this, FShowAsModalStorage);
    HookFormActivation(this);
  }
  FFrameAnimation.Start();
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeProgressForm::SetData(
  const UnicodeString & LocalDirectory, const UnicodeString & RemoteDirectory, int Progress, bool & Continue)
{
  DebugAssert(FStarted);
  LocalDirectoryLabel->Caption = LocalDirectory;
  RemoteDirectoryLabel->Caption = RemoteDirectory;
  OperationProgress->Position = Progress;
  Continue = !FCanceled;

  UpdateControls();
  Application->ProcessMessages();
  return CalculateProgress();
}
//---------------------------------------------------------------------------
int __fastcall TSynchronizeProgressForm::CalculateProgress()
{
  return (((OperationProgress->Style == pbstMarquee) || (OperationProgress->Max == 0)) ? -1 : ((OperationProgress->Position * 100) / OperationProgress->Max));
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::UpdateControls()
{
  TDateTime Elapsed;
  UnicodeString ACaption = FormatFormCaption(this, LoadStr(SYNCHRONIZE_PROGRESS_COMPARE));
  if (FStarted)
  {
    Elapsed = Now() - FStartTime;
    int Progress = CalculateProgress();
    if (Progress >= 0)
    {
      ACaption = FORMAT(L"%d%% %s", (Progress, ACaption));
    }
  }
  else
  {
    Elapsed = EncodeTimeVerbose(0, 0, 0, 0);
  }
  Caption = ACaption;
  TimeElapsedLabel->Caption = FormatDateTimeSpan(Elapsed);
  UnicodeString TimeLeftCaption;
  int Position = OperationProgress->Position;
  if (FStarted && CanShowTimeEstimate(FStartTime) && (Position > 0))
  {
    TDateTime TimeLeft = TDateTime(double(double(Elapsed) * (OperationProgress->Max - Position) / Position));
    TimeLeftCaption = FormatDateTimeSpan(TimeLeft);
  }
  else
  {
    TimeLeftCaption = LoadStr(PROGRESS_TIME_LEFT_CALCULATING);
  }
  TimeLeftLabel->Caption = TimeLeftCaption;
  CancelItem->Enabled = !FCanceled;

  TProgressForm::SizeToolbar(this, Toolbar, Dock, ToolbarPanel);
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::CancelOperation()
{
  FCanceled = true;
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::UpdateTimerTimer(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::GlobalMinimize(TObject * /*Sender*/)
{
  ApplicationMinimize();
  FMinimizedByMe = true;
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::CMDialogKey(TCMDialogKey & Message)
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
void __fastcall TSynchronizeProgressForm::Dispatch(void * AMessage)
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
  else
  {
    TForm::Dispatch(AMessage);
  }
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::CancelItemClick(TObject * /*Sender*/)
{
  CancelOperation();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::MinimizeItemClick(TObject * Sender)
{
  CallGlobalMinimizeHandler(Sender);
}
//---------------------------------------------------------------------------
