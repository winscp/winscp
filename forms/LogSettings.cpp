//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "LogSettings.h"

#include <ScpMain.h>

#include <VCLCommon.h>
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ComboEdit"
#pragma link "UpDownEdit"
#pragma link "XPGroupBox"
#pragma resource "*.dfm"
TLoggingFrame *LoggingFrame;
//---------------------------------------------------------------------------
__fastcall TLoggingFrame::TLoggingFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FEnableLogWindow = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LoadConfiguration()
{
  //Log tab
  LoggingCheck->Checked = Configuration->Logging;
  LogToFileCheck->Checked = Configuration->LogToFile;
  LogFileNameEdit->Text = Configuration->LogFileName;
  if (Configuration->LogFileAppend)
    LogFileAppendButton->Checked = True;
  else
    LogFileOverwriteButton->Checked = True;
  LogShowWindowCheck->Checked = (CustomWinConfiguration->LogView == lvWindow);
  if (Configuration->LogWindowComplete)
    LogWindowCompleteButton->Checked = True;
  else
    LogWindowLinesButton->Checked = True;
  if (!Configuration->LogWindowComplete)
    LogWindowLinesEdit->AsInteger = Configuration->LogWindowLines;
  else
    LogWindowLinesEdit->AsInteger = 500;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::SaveConfiguration()
{
  Configuration->BeginUpdate();
  try
  {
    Configuration->Logging = LoggingCheck->Checked;
    Configuration->LogToFile = LogToFileCheck->Checked;
    if (LogToFileCheck->Checked)
    {
      Configuration->LogFileName = LogFileNameEdit->Text;
    }
    Configuration->LogFileAppend = LogFileAppendButton->Checked;
    if (EnableLogWindow)
    {
      CustomWinConfiguration->LogView = LogShowWindowCheck->Checked ? lvWindow : lvNone;
      Configuration->LogWindowComplete = LogWindowCompleteButton->Checked;
      if (!LogWindowCompleteButton->Checked)
      {
        Configuration->LogWindowLines = LogWindowLinesEdit->AsInteger;
      }
    }
  }
  __finally
  {
    Configuration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::UpdateControls()
{
  if (!LoggingCheck->Checked)
  {
    EnableControl(LoggingGroup, False);
  }
  else
  {
    LoggingGroup->Enabled = True;

    EnableControl(LogToFileCheck, True);
    EnableControl(LogFileNameEdit, LogToFileCheck->Checked);
    EnableControl(LogFilePanel, LogToFileCheck->Checked);

    EnableControl(LogShowWindowCheck, True && EnableLogWindow);
    EnableControl(LogWindowCompleteButton, LogShowWindowCheck->Checked && EnableLogWindow);
    EnableControl(LogWindowLinesButton, LogShowWindowCheck->Checked && EnableLogWindow);
    EnableControl(LogWindowLinesText, LogShowWindowCheck->Checked && EnableLogWindow);
    EnableControl(LogWindowLinesEdit, LogShowWindowCheck->Checked &&
      LogWindowLinesButton->Checked && EnableLogWindow);
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogToFileCheckChange(TObject *Sender)
{
  if (LogToFileCheck->Checked && LogFileNameEdit->Text.IsEmpty())
  {
    LogFileNameEdit->Text = DefaultLogFileName;
  }
  DataChange(Sender);
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::DataChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLoggingFrame::GetDefaultLogFileName()
{
  assert(FOnGetDefaultLogFileName);
  AnsiString Result;
  FOnGetDefaultLogFileName(this, Result);
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::SetEnableLogWindow(bool value)
{
  if (EnableLogWindow != value)
  {
    FEnableLogWindow = value;
    UpdateControls();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEditKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState Shift)
{
  PathEditKeyDown(LogFileNameEdit, Key, Shift, false);
}
//---------------------------------------------------------------------------

