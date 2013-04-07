//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "LogSettings.h"

#include <CoreMain.h>
#include <TextsWin.h>

#include <VCLCommon.h>
#include <Tools.h>
#include "CustomWinConfiguration.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "ComboEdit"
#pragma link "UpDownEdit"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
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
  HintLabel(LogFileNameHintText, LoadStr(LOG_FILE_HINT3));
  HintLabel(ActionsLogFileNameHintText, LoadStr(LOG_FILE_HINT3));

  // anchors does not apply for some reason to this particular control
  LogFileNameHintText->Left = LogFileNameEdit3->Left + LogFileNameEdit3->Width -
    LogFileNameHintText->Width;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LoadConfiguration()
{
  EnableLoggingCheck->Checked = Configuration->Logging;
  LogProtocolCombo->ItemIndex = Configuration->LogProtocol;
  LogToFileCheck->Checked = Configuration->LogToFile;
  LogFileNameEdit3->Text = !Configuration->LogFileName.IsEmpty() ? Configuration->LogFileName : Configuration->DefaultLogFileName;
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

  EnableActionsLoggingCheck->Checked = Configuration->LogActions;
  ActionsLogFileNameEdit->Text = Configuration->ActionsLogFileName;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::SaveConfiguration()
{
  Configuration->BeginUpdate();
  try
  {
    Configuration->Logging = EnableLoggingCheck->Checked;
    Configuration->LogProtocol = LogProtocolCombo->ItemIndex;
    Configuration->LogFileName = LogToFileCheck->Checked ? LogFileNameEdit3->Text : UnicodeString();
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

    Configuration->LogActions = EnableActionsLoggingCheck->Checked;
    Configuration->ActionsLogFileName = ActionsLogFileNameEdit->Text;
  }
  __finally
  {
    Configuration->EndUpdate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::UpdateControls()
{
  EnableControl(LogProtocolCombo, EnableLoggingCheck->Checked);
  EnableControl(LogToFileCheck, LogProtocolCombo->Enabled);
  EnableControl(LogFileNameEdit3, LogToFileCheck->Enabled && LogToFileCheck->Checked);
  EnableControl(LogFileNameHintText, LogFileNameEdit3->Enabled);
  EnableControl(LogFileAppendButton, LogFileNameEdit3->Enabled);
  EnableControl(LogFileOverwriteButton, LogFileNameEdit3->Enabled);

  EnableControl(LogShowWindowCheck, LogProtocolCombo->Enabled && EnableLogWindow);
  EnableControl(LogWindowCompleteButton, LogShowWindowCheck->Enabled && LogShowWindowCheck->Checked);
  EnableControl(LogWindowLinesButton, LogWindowCompleteButton->Enabled);
  EnableControl(LogWindowLinesText, LogWindowCompleteButton->Enabled);
  EnableControl(LogWindowLinesEdit, LogWindowLinesButton->Enabled && LogWindowLinesButton->Checked);

  EnableControl(ActionsLogFileNameEdit, EnableActionsLoggingCheck->Checked);
  EnableControl(ActionsLogFileNameHintText, ActionsLogFileNameEdit->Enabled);
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::DataChange(TObject * /*Sender*/)
{
  UpdateControls();
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
  UnicodeString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEditAfterDialog(TObject * /*Sender*/,
  UnicodeString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEditCreateEditDialog(
  TObject * Sender, TFileDialogKind DialogKind, TOpenDialog *& Dialog)
{
  USEDPARAM(DialogKind);
  assert(DialogKind == dkOpen);
  Dialog = new TOpenDialog(dynamic_cast<TComponent *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::CreateWnd()
{
  TFrame::CreateWnd();

  if (LogFileNameEdit3 != NULL)
  {
    InstallPathWordBreakProc(LogFileNameEdit3);
  }
  if (ActionsLogFileNameEdit != NULL)
  {
    InstallPathWordBreakProc(ActionsLogFileNameEdit);
  }
}
//---------------------------------------------------------------------------
