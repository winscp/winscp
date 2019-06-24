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
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "PathLabel"
#ifndef NO_RESOURCES
#pragma link "PngImageList"
#pragma link "TB2Dock"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBX"
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
// Used for comparing only
__fastcall TSynchronizeProgressForm::TSynchronizeProgressForm(TComponent * Owner, bool AllowMinimize)
  : TForm(Owner)
{
  FStarted = false;
  FCanceled = false;
  FElapsed = EncodeTimeVerbose(0, 0, 0, 0);
  FShowAsModalStorage = NULL;
  FMinimizedByMe = false;
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
  Caption = FormatFormCaption(this, LoadStr(SYNCHRONIZE_PROGRESS_COMPARE));
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
void __fastcall TSynchronizeProgressForm::SetData(const UnicodeString LocalDirectory,
  const UnicodeString RemoteDirectory, bool & Continue)
{
  DebugAssert(FStarted);
  LocalDirectoryLabel->Caption = LocalDirectory;
  RemoteDirectoryLabel->Caption = RemoteDirectory;
  Continue = !FCanceled;

  UpdateControls();
  Application->ProcessMessages();
}
//---------------------------------------------------------------------------
void __fastcall TSynchronizeProgressForm::UpdateControls()
{
  if (FStarted)
  {
    FElapsed = Now() - FStartTime;
  }
  TimeElapsedLabel->Caption = FormatDateTimeSpan(Configuration->TimeFormat, FElapsed);
  CancelItem->Enabled = !FCanceled;
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
