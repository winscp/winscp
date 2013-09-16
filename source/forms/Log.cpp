//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <FileCtrl.hpp>

#include <VCLCommon.h>
#include <Common.h>
#include <CoreMain.h>
#include <TextsWin.h>

#include "Log.h"
#include "Glyphs.h"
#include "NonVisual.h"
#include "WinConfiguration.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "LogMemo"
#pragma link "TB2Dock"
#pragma link "TB2Item"
#pragma link "TB2Toolbar"
#pragma link "TBX"
#pragma link "TBXStatusBars"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
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
        : TForm(Owner)
{
  FLogMemo = NULL;
  FSessionLog = NULL;
  ShowWindow(Handle, SW_SHOWNA);
  UseSystemSettings(this);
  UseDesktopFont(StatusBar);
  SetFormIcons(this, L"Z_ICON_LOG_BIG", L"Z_ICON_LOG_SMALL");
}
//---------------------------------------------------------------------------
__fastcall TLogForm::~TLogForm()
{
  // deassociate us from session log state change handler
  SessionLog = NULL;
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
  WinConfiguration->LogWindowParams = StoreForm(this);
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::SetSessionLog(TSessionLog * value)
{
  if (FSessionLog != value)
  {
    if (SessionLog)
    {
      assert(SessionLog->OnStateChange == SessionLogStateChange);
      SessionLog->OnStateChange = NULL;
    }
    FSessionLog = value;
    if (SessionLog)
    {
      assert(SessionLog->OnStateChange == NULL);
      SessionLog->OnStateChange = SessionLogStateChange;
    }
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::SessionLogStateChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::UpdateControls()
{
  TTBXStatusPanel * Panel = StatusBar->Panels->Items[0];
  if (SessionLog)
  {
    if (SessionLog->LoggingToFile)
    {
      Panel->TextTruncation = twPathEllipsis;
      Panel->Caption = SessionLog->CurrentFileName;
    }
    else
    {
      Panel->TextTruncation = twEndEllipsis;
      Panel->Caption = LoadStr(LOG_NOLOGFILE);
    }
    Caption = FMTLOAD(LOG_CAPTION, (SessionLog->SessionName));
  }
  else
  {
    Panel->TextTruncation = twEndEllipsis;
    Panel->Caption = LoadStr(LOG_NOLOG);
    Caption = LoadStr(LOG_NOLOGCAPTION);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLogForm::CreateParams(TCreateParams & Params)
{
  if (!FFormRestored)
  {
    FFormRestored = True;
    assert(Configuration);
    RestoreForm(WinConfiguration->LogWindowParams, this);
  }
  TForm::CreateParams(Params);
  Params.WndParent = GetDesktopWindow();
}
