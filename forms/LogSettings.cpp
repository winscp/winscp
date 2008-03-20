//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "LogSettings.h"

#include <CoreMain.h>
#include <TextsWin.h>

#include <VCLCommon.h>
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ComboEdit"
#pragma link "UpDownEdit"
#pragma resource "*.dfm"
TLoggingFrame *LoggingFrame;
//---------------------------------------------------------------------------
__fastcall TLoggingFrame::TLoggingFrame(TComponent* Owner)
        : TFrame(Owner)
{
  FEnableLogWindow = true;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::Init()
{
  InstallPathWordBreakProc(LogFileNameEdit);
  HintLabel(LogFileNameHintText, LoadStr(LOG_FILE_HINT2));

  // anchors does not apply for some reason to this particular control
  LogFileNameHintText->Left = LogFileNameEdit->Left + LogFileNameEdit->Width -
    LogFileNameHintText->Width;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LoadConfiguration()
{
  //Log tab
  LoggingCheck->Checked = Configuration->Logging;
  LogProtocolCombo->ItemIndex = Configuration->LogProtocol;
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
    Configuration->LogProtocol = LogProtocolCombo->ItemIndex;
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
    EnableControl(LogProtocolLabel, True);
    EnableControl(LogProtocolCombo, True);
    EnableControl(LogFileNameEdit, LogToFileCheck->Checked);
    EnableControl(LogFileNameHintText, LogFileNameEdit->Enabled);
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
  return IncludeTrailingBackslash(SystemTemporaryDirectory()) + "&s.log";
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
void __fastcall TLoggingFrame::LogFileNameEditBeforeDialog(TObject * /*Sender*/,
  AnsiString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEditAfterDialog(TObject * /*Sender*/,
  AnsiString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }
}
//---------------------------------------------------------------------------
