//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Terminal.h>
#include <TextsWin.h>
#include <WinConfiguration.h>
#include <WinInterface.h>
#include <GUITools.h>
#include <CoreMain.h>
#include "CustomCommand.h"
#include "VCLCommon.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoCustomCommandDialog(TCustomCommandType & Command,
  const TCustomCommandList * CustomCommandList,
  TCustomCommandsMode Mode, int Options, TCustomCommandValidate OnValidate,
  const TShortCuts * ShortCuts)
{
  bool Result;
  TCustomCommandDialog * Dialog = new TCustomCommandDialog(
    Application, CustomCommandList, Mode, Options, OnValidate, ShortCuts);
  try
  {
    Result = Dialog->Execute(Command);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandDialog::TCustomCommandDialog(TComponent* Owner,
  const TCustomCommandList * CustomCommandList, TCustomCommandsMode Mode,
  int Options, TCustomCommandValidate OnValidate, const TShortCuts * ShortCuts)
  : TForm(Owner)
{
  SetCorrectFormParent(this);
  UseSystemSettings(this);
  FCustomCommandList = CustomCommandList;
  FMode = Mode;
  FOnValidate = OnValidate;
  InstallPathWordBreakProc(CommandEdit);
  HintLabel(HintText, LoadStr(CUSTOM_COMMAND_PATTERNS_HINT2));

  int CaptionRes;
  switch (FMode)
  {
    case ccmAdd:
      CaptionRes = CUSTOM_COMMAND_ADD;
      break;
    case ccmEdit:
      CaptionRes = CUSTOM_COMMAND_EDIT;
      break;
    case ccmAdHoc:
    default:
      CaptionRes = CUSTOM_COMMAND_AD_HOC;
      break;
  }
  Caption = LoadStr(CaptionRes);

  if (FMode == ccmAdHoc)
  {
    int Shift = CommandEdit->Top - DescriptionEdit->Top;
    int Shift2 = Group->Height - ShortCutLabel->Top;

    DescriptionLabel->Visible = false;
    DescriptionEdit->Visible = false;
    for (int i = 0; i < Group->ControlCount; i++)
    {
      TControl * Control = Group->Controls[i];
      if (Control->Visible)
      {
        if (Control->Top > DescriptionLabel->Top)
        {
          Control->Top = Control->Top - Shift;
        }
      }
    }

    ShortCutLabel->Visible = false;
    ShortCutCombo->Visible = false;

    ClientHeight = ClientHeight - Shift - Shift2;
  }
  else
  {
    assert(ShortCuts != NULL);
    InitializeShortCutCombo(ShortCutCombo, *ShortCuts);
  }

  EnableControl(RemoteCommandButton, FLAGCLEAR(Options, ccoDisableRemote));

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::UpdateControls()
{
  AnsiString Command = CommandEdit->Text;
  EnableControl(OkButton, !Command.IsEmpty() && !DescriptionEdit->Text.IsEmpty());

  bool RemoteCommand = RemoteCommandButton->Checked;
  bool AllowRecursive = true;
  bool AllowApplyToDirectories = true;
  try
  {
    TRemoteCustomCommand RemoteCustomCommand;
    TLocalCustomCommand LocalCustomCommand;
    TFileCustomCommand * FileCustomCommand =
      (RemoteCommand ? &RemoteCustomCommand : &LocalCustomCommand);

    TInteractiveCustomCommand InteractiveCustomCommand(FileCustomCommand);
    AnsiString Cmd = InteractiveCustomCommand.Complete(Command, false);
    bool FileCommand = FileCustomCommand->IsFileCommand(Cmd);
    AllowRecursive = FileCommand && !FileCustomCommand->IsFileListCommand(Cmd);
    if (AllowRecursive && !RemoteCommand)
    {
      AllowRecursive = !LocalCustomCommand.HasLocalFileName(Cmd);
    }
    AllowApplyToDirectories = FileCommand;
  }
  catch(...)
  {
  }

  EnableControl(RecursiveCheck, AllowRecursive);
  EnableControl(ApplyToDirectoriesCheck, AllowApplyToDirectories);
  EnableControl(ShowResultsCheck, RemoteCommand);
  EnableControl(CopyResultsCheck, RemoteCommand);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::SetParams(int value)
{
  FParams = value;
  ApplyToDirectoriesCheck->Checked = FLAGSET(value, ccApplyToDirectories);
  RecursiveCheck->Checked = FLAGSET(value, ccRecursive);
  (FLAGSET(value, ccLocal) ? LocalCommandButton : RemoteCommandButton)->Checked = true;
  ShowResultsCheck->Checked = FLAGSET(value, ccShowResults);
  CopyResultsCheck->Checked = FLAGSET(value, ccCopyResults);
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandDialog::GetParams()
{
  return
    (FParams & ~(ccApplyToDirectories | ccRecursive | ccLocal |
       ccShowResults | ccCopyResults)) |
    FLAGMASK(!RemoteCommandButton->Checked, ccLocal) |
    FLAGMASK(ApplyToDirectoriesCheck->Checked, ccApplyToDirectories) |
    FLAGMASK(RecursiveCheck->Checked && RecursiveCheck->Enabled, ccRecursive) |
    FLAGMASK(ShowResultsCheck->Checked && ShowResultsCheck->Enabled, ccShowResults) |
    FLAGMASK(CopyResultsCheck->Checked && CopyResultsCheck->Enabled, ccCopyResults);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandDialog::Execute(TCustomCommandType & Command)
{
  CommandEdit->Items = CustomWinConfiguration->History["CustomCommand"];
  if (CommandEdit->Items->Count == 0)
  {
    for (int i = 0; i < FCustomCommandList->Count; i++)
    {
      CommandEdit->Items->Add(FCustomCommandList->Commands[i]->Name);
    }
  }

  DescriptionEdit->Text = Command.Name;
  FOrigDescription = Command.Name;
  CommandEdit->Text = Command.Command;
  SetParams(Command.Params);
  SetShortCutCombo(ShortCutCombo, Command.ShortCut);

  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    GetCommand(Command);

    CommandEdit->SaveToHistory();
    CustomWinConfiguration->History["CustomCommand"] = CommandEdit->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    if ((FMode == ccmAdd) || (FMode == ccmEdit))
    {
      AnsiString Desc = DescriptionEdit->Text;

      if (Desc.Pos("=") > 0)
      {
        DescriptionEdit->SetFocus();
        throw Exception(FMTLOAD(CUSTOM_COMMAND_INVALID, ("=")));
      }

      if (((FMode == ccmAdd) || ((FMode == ccmEdit) && (Desc != FOrigDescription))) &&
          (FCustomCommandList->Find(Desc) != 0))
      {
        DescriptionEdit->SetFocus();
        throw Exception(FMTLOAD(CUSTOM_COMMAND_DUPLICATE, (Desc)));
      }
    }

    try
    {
      bool RemoteCommand = RemoteCommandButton->Checked;

      TRemoteCustomCommand RemoteCustomCommand;
      TLocalCustomCommand LocalCustomCommand;
      TFileCustomCommand * FileCustomCommand =
        (RemoteCommand ? &RemoteCustomCommand : &LocalCustomCommand);

      TInteractiveCustomCommand InteractiveCustomCommand(FileCustomCommand);

      AnsiString Command = CommandEdit->Text;
      InteractiveCustomCommand.Validate(Command);
      Command = InteractiveCustomCommand.Complete(Command, false);
      FileCustomCommand->Validate(Command);
    }
    catch(...)
    {
      CommandEdit->SetFocus();
      throw;
    }

    if (FOnValidate)
    {
      TCustomCommandType Command;
      GetCommand(Command);
      FOnValidate(Command);
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::CommandEditGetData(
  THistoryComboBox * /*Sender*/, Pointer & Data)
{
  Data = reinterpret_cast<void *>(ccSet | GetParams());
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::CommandEditSetData(
  THistoryComboBox * /*Sender*/, Pointer Data)
{
  int IData = reinterpret_cast<int>(Data);
  if (FLAGSET(IData, ccSet))
  {
    SetParams(IData & ~ccSet);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::GetCommand(TCustomCommandType & Command)
{
  Command.Name = DescriptionEdit->Text;
  Command.Command = CommandEdit->Text;
  Command.Params = GetParams();
  Command.ShortCut = GetShortCutCombo(ShortCutCombo);
}
//---------------------------------------------------------------------------
