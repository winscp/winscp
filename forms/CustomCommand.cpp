//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <Terminal.h>
#include <TextsWin.h>
#include <WinConfiguration.h>
#include <GUITools.h>
#include "CustomCommand.h"
#include "VCLCommon.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPThemes"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCustomCommandDialog(AnsiString & Description,
  AnsiString & Command, int & Params, const TCustomCommands * CustomCommands, bool Edit)
{
  bool Result;
  TCustomCommandDialog * Dialog = new TCustomCommandDialog(Application);
  try
  {
    Dialog->Description = Description;
    Dialog->Command = Command;
    Dialog->Params = Params;
    Dialog->CustomCommands = CustomCommands;
    Dialog->Edit = Edit;
    Result = Dialog->Execute();
    if (Result)
    {
      Description = Dialog->Description;
      Command = Dialog->Command;
      Params = Dialog->Params;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TCustomCommandDialog::TCustomCommandDialog(TComponent* Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FCustomCommands = NULL;
  FEdit = true;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::UpdateControls()
{
  EnableControl(OkButton, !Command.IsEmpty() && !Description.IsEmpty());

  bool RemoteCommand = RemoteCommandButton->Checked;
  bool AllowRecursive = true;
  try
  {
    TRemoteCustomCommand RemoteCustomCommand;
    TLocalCustomCommand LocalCustomCommand;
    TFileCustomCommand * FileCustomCommand =
      (RemoteCommand ? &RemoteCustomCommand : &LocalCustomCommand);

    TInteractiveCustomCommand InteractiveCustomCommand(FileCustomCommand);
    AnsiString Cmd = InteractiveCustomCommand.Complete(Command, false);
    AllowRecursive = !FileCustomCommand->IsFileListCommand(Cmd);
    if (AllowRecursive && !RemoteCommand)
    {
      AllowRecursive = !LocalCustomCommand.HasLocalFileName(Cmd);
    }
  }
  catch(...)
  {
  }

  EnableControl(RecursiveCheck, AllowRecursive);
  EnableControl(ShowResultsCheck, RemoteCommand);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::SetCommand(AnsiString value)
{
  CommandEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomCommandDialog::GetCommand()
{
  return CommandEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::SetDescription(AnsiString value)
{
  FOrigDescription = value;
  DescriptionEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TCustomCommandDialog::GetDescription()
{
  return DescriptionEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::SetParams(int value)
{
  FParams = value;
  ApplyToDirectoriesCheck->Checked = FLAGSET(value, ccApplyToDirectories);
  RecursiveCheck->Checked = FLAGSET(value, ccRecursive);
  (FLAGSET(value, ccLocal) ? LocalCommandButton : RemoteCommandButton)->Checked = true;
  ShowResultsCheck->Checked = FLAGSET(value, ccShowResults);
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandDialog::GetParams()
{
  return
    (FParams & ~(ccApplyToDirectories | ccRecursive | ccLocal | ccShowResults)) |
    FLAGMASK(!RemoteCommandButton->Checked, ccLocal) |
    FLAGMASK(ApplyToDirectoriesCheck->Checked, ccApplyToDirectories) |
    FLAGMASK(RecursiveCheck->Checked && RecursiveCheck->Enabled, ccRecursive) |
    FLAGMASK(ShowResultsCheck->Checked && ShowResultsCheck->Enabled, ccShowResults);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandDialog::Execute()
{
  return (ShowModal() == mrOk);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormShow(TObject * /*Sender*/)
{
  Caption = LoadStr(Edit ? CUSTOM_COMMAND_EDIT : CUSTOM_COMMAND_ADD);
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::PathEditsKeyDown(TObject * /*Sender*/,
  WORD & Key, TShiftState Shift)
{
  PathEditKeyDown(CommandEdit, Key, Shift, RemoteCommandButton->Checked);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    AnsiString Desc = Description;
    
    if (Desc.Pos("=") > 0)
    {
      DescriptionEdit->SetFocus();
      throw Exception(FMTLOAD(CUSTOM_COMMAND_INVALID, ("=")));
    }

    try
    {
      bool RemoteCommand = RemoteCommandButton->Checked;
      
      TRemoteCustomCommand RemoteCustomCommand;
      TLocalCustomCommand LocalCustomCommand;
      TFileCustomCommand * FileCustomCommand =
        (RemoteCommand ? &RemoteCustomCommand : &LocalCustomCommand);

      TInteractiveCustomCommand InteractiveCustomCommand(FileCustomCommand);

      AnsiString Cmd = Command;
      InteractiveCustomCommand.Validate(Cmd);
      Cmd = InteractiveCustomCommand.Complete(Cmd, false);
      FileCustomCommand->Validate(Cmd);
    }
    catch(...)
    {
      CommandEdit->SetFocus();
      throw;
    }

    if ((!Edit || (Desc != FOrigDescription)) &&
        (const_cast<TCustomCommands*>(FCustomCommands)->IndexOfName(Desc) >= 0))
    {
      DescriptionEdit->SetFocus();
      throw Exception(FMTLOAD(CUSTOM_COMMAND_DUPLICATE, (Desc)));
    }
  }
}
//---------------------------------------------------------------------------

