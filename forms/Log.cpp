//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <FileCtrl.hpp>

#include <VCLCommon.h>
#include <Common.h>
#include <ScpMain.h>
#include <TextsWin.h>

#include "Log.h"
#include "NonVisual.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "LogMemo"
#pragma resource "*.dfm"
TLogForm *LogForm = NULL;
//---------------------------------------------------------------------------
TLogForm * __fastcall CreateLogForm(TLogMemo *ALogMemo)
{
  assert(!LogForm);
  TLogForm * aLogForm = new TLogForm(Application);
  try
  {
    aLogForm->LogMemo = ALogMemo;
    aLogForm->Show();
  }
  catch (...)
  {
    delete aLogForm;
    throw;
  }
  LogForm = aLogForm;
  return aLogForm;
}
//---------------------------------------------------------------------------
TLogForm * __fastcall RequireLogForm(TLogMemo *ALogMemo)
{
  if (!LogForm)
  {
    CreateLogForm(ALogMemo);
  }
  return LogForm;
}
//---------------------------------------------------------------------------
void __fastcall FreeLogForm()
{
  if (LogForm)
  {
    TLogForm * PLogForm = LogForm;
    LogForm = NULL;
    // would also free form see TLogForm::FormClose()
    // we can't free form directly (cause exception when form is closed by
    // button on toolbar, beacuse it destroys the button too)
    PLogForm->Close();
  }
}
//---------------------------------------------------------------------------
__fastcall TLogForm::TLogForm(TComponent* Owner)
        : FFormRestored(False), TForm(Owner)
{
  FStatusBarFile = false;
  FLogMemo = NULL;
  FSessionLog = NULL;
  ShowWindow(Handle, SW_SHOWNA);
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TLogForm::~TLogForm()
{
  LogForm = NULL;
  LogMemo = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::SetLogMemo(TLogMemo * value)
{
  if (LogMemo != value)
  {
    TLogMemo * OldLogMemo = LogMemo;

    FLogMemo = value;

    if (OldLogMemo)
    {
      assert((OldLogMemo->Parent == this) && (OldLogMemo->OnChange == LogMemoChange));
      OldLogMemo->OnChange = NULL;
      if (SessionLog == OldLogMemo->SessionLog) SessionLog = NULL;
      OldLogMemo->Parent = NULL;
    }

    if (LogMemo)
    {
      LogMemo->Align = alClient;
      if (!SessionLog) SessionLog = LogMemo->SessionLog;
      LogMemo->Parent = this;
      // setting Parent usually calls OnChange many times (pending changes are
      // inserted to TLogMemo), so we set OnChange handler after Parent.
      LogMemo->OnChange = LogMemoChange;
      LogMemoChange(LogMemo);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::LogMemoChange(TObject * /*Sender*/)
{
  assert(LogMemo);
  Application->ProcessMessages();
  if (!ComponentState.Contains(csDestroying))
  {
    UpdateActions();
    /*if (!SessionLog->Count)
    {
      LogMemo->Invalidate();
    } */
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::FormClose(TObject * /*Sender*/, TCloseAction & Action)
{
  assert(Configuration);
  // If log window feature is turned off (log window is being closed
  // by turning off this feature e.g. in Preferences Window), really close it
  // If log window feature is turned on (log window is being closed by
  // close command if this window) we only disable log window feature
  // (this function will be than called again, see case 1)
  LogMemo = NULL;
  if (!Configuration->Logging || (WinConfiguration->LogView != lvWindow) ||
      Application->Terminated)
  {
    Action = caFree;
  }
  else
  {
    WinConfiguration->LogView = lvNone;
    Action = caFree;
  }
  WinConfiguration->LogWindowParams = WinConfiguration->StoreForm(this);
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::SetSessionLog(TSessionLog * value)
{
  if (FSessionLog != value)
  {
    FSessionLog = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::UpdateControls()
{
  if (SessionLog)
  {
    FStatusBarFile = SessionLog->LoggingToFile;
    SetStatusBarText(SessionLog->LoggingToFile ? SessionLog->LogFileName :
      LoadStr(LOG_NOLOGFILE));

    assert(SessionLog->Data);
    Caption = FMTLOAD(LOG_CAPTION, (SessionLog->Data->SessionName));
  }
  else
  {
    FStatusBarFile = False;
    SetStatusBarText(LoadStr(LOG_NOLOG));
    Caption = LoadStr(LOG_NOLOGCAPTION);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::SetStatusBarText(AnsiString Text)
{
  StatusBar->Hint = Text;
  if (StatusBar->Canvas->TextWidth(Text) > StatusBar->ClientWidth - 20)
  {
    StatusBar->ShowHint = true;

    if (!FStatusBarFile)
    {
      Text += "...";
      while ((Text.Length() > 3) &&
        (StatusBar->Canvas->TextWidth(Text) > StatusBar->ClientWidth - 20))
      {
        Text.Delete(Text.Length() - 3, 1);
      }
    }
    else
    {
      Text = MinimizeName(Text, StatusBar->Canvas, StatusBar->ClientWidth - 20);
    }
  }
  else
  {
    StatusBar->ShowHint = false;
  }
  StatusBar->SimpleText = Text;
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::StatusBarResize(TObject * /*Sender*/)
{
  SetStatusBarText(StatusBar->Hint);
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = True;
    assert(Configuration);
    WinConfiguration->RestoreForm(WinConfiguration->LogWindowParams, this);
  }
  TForm::CreateParams(Params);
  Params.WndParent = GetDesktopWindow();
}
