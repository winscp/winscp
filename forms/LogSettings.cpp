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
  InstallPathWordBreakProc(LogFileNameEdit2);
  HintLabel(LogFileNameHintText, LoadStr(LOG_FILE_HINT2));

  // anchors does not apply for some reason to this particular control
  LogFileNameHintText->Left = LogFileNameEdit2->Left + LogFileNameEdit2->Width -
    LogFileNameHintText->Width;
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LoadConfiguration()
{
  //Log tab
  LoggingOffButton->Checked = !Configuration->Logging;
  LoggingOnButton->Checked = Configuration->Logging && !Configuration->LogActions;
  LoggingActionsButton->Checked = Configuration->Logging && Configuration->LogActions;
  LogProtocolCombo->ItemIndex = Configuration->LogProtocol;
  LogToFileCheck->Checked = Configuration->LogToFile;
  LogFileNameEdit2->Text = Configuration->LogFileName;
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
    Configuration->Logging = LoggingOnButton->Checked || LoggingActionsButton->Checked;
    Configuration->LogActions = LoggingActionsButton->Checked;
    Configuration->LogProtocol = LogProtocolCombo->ItemIndex;
    Configuration->LogToFile = LogToFileCheck->Checked;
    if (LogToFileCheck->Checked)
    {
      Configuration->LogFileName = LogFileNameEdit2->Text;
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
  if (!LoggingOnButton->Checked && !LoggingActionsButton->Checked)
  {
    EnableControl(LoggingGroup, False);
  }
  else
  {
    LoggingGroup->Enabled = True;

    EnableControl(LogProtocolCombo, LoggingOnButton->Checked);
    EnableControl(LogProtocolLabel, LogProtocolCombo->Enabled);
    EnableControl(LogToFileCheck, True);
    EnableControl(LogFileNameEdit2, LogToFileCheck->Checked);
    EnableControl(LogFileNameHintText, LogFileNameEdit2->Enabled);
    EnableControl(LogFileAppendButton, LogFileNameEdit2->Enabled && LoggingOnButton->Checked);
    EnableControl(LogFileOverwriteButton, LogFileNameEdit2->Enabled && LoggingOnButton->Checked);
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
void __fastcall TLoggingFrame::LogToFileCheckChange(TObject * /*Sender*/)
{
  if (LogToFileCheck->Checked && LogFileNameEdit2->Text.IsEmpty())
  {
    LogFileNameEdit2->Text =
      IncludeTrailingBackslash(SystemTemporaryDirectory()) + "!s." + GetLogFileExt();
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::DataChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLoggingFrame::GetLogFileExt()
{
  return (LoggingOnButton->Checked ? "log" : "xml");
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
void __fastcall TLoggingFrame::LogFileNameEdit2BeforeDialog(TObject * /*Sender*/,
  AnsiString & Name, bool & /*Action*/)
{
  FBeforeDialogPath = Name;
  Name = ExpandEnvironmentVariables(Name);
  LogFileNameEdit2->DefaultExt = GetLogFileExt();
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEdit2AfterDialog(TObject * /*Sender*/,
  AnsiString & Name, bool & /*Action*/)
{
  if (CompareFileName(Name, ExpandEnvironmentVariables(FBeforeDialogPath)))
  {
    Name = FBeforeDialogPath;
  }
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LogFileNameEdit2CreateEditDialog(
  TObject * Sender, TFileDialogKind DialogKind, TOpenDialog *& Dialog)
{
  USEDPARAM(DialogKind);
  assert(DialogKind == dkOpen);
  Dialog = CreateOpenDialog(dynamic_cast<TComponent *>(Sender));
}
//---------------------------------------------------------------------------
void __fastcall TLoggingFrame::LoggingButtonClick(TObject * /*Sender*/)
{
  if (LoggingOnButton->Checked || LoggingActionsButton->Checked)
  {
    AnsiString Ext = ExtractFileExt(LogFileNameEdit2->Text);
    if (AnsiSameText(Ext, ".log") || AnsiSameText(Ext, ".xml"))
    {
      AnsiString NewExt = AnsiString(".") + GetLogFileExt();
      if (!AnsiSameText(Ext, NewExt))
      {
        LogFileNameEdit2->Text = ChangeFileExt(LogFileNameEdit2->Text, NewExt);
      }
    }
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
