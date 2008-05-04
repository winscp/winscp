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
bool __fastcall DoCustomCommandDialog(AnsiString & Description,
  AnsiString & Command, int & Params, const TCustomCommands * CustomCommands,
  TCustomCommandsMode Mode, int Options, TCustomCommandValidate OnValidate)
{
  bool Result;
  TCustomCommandDialog * Dialog = new TCustomCommandDialog(Application, Options);
  try
  {
    Dialog->Description = Description;
    Dialog->Command = Command;
    Dialog->Params = Params;
    Dialog->CustomCommands = CustomCommands;
    Dialog->Mode = Mode;
    Dialog->OnValidate = OnValidate;
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
__fastcall TCustomCommandDialog::TCustomCommandDialog(TComponent* Owner, unsigned int Options)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FCustomCommands = NULL;
  FMode = ccmEdit;
  FOnValidate = NULL;
  FOptions = Options;
  InstallPathWordBreakProc(CommandEdit);
  HintLabel(HintText, LoadStr(CUSTOM_COMMAND_PATTERNS_HINT2));
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::UpdateControls()
{
  EnableControl(OkButton, !Command.IsEmpty() && !Description.IsEmpty());
  EnableControl(RemoteCommandButton, FLAGCLEAR(FOptions, ccoDisableRemote));

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
bool __fastcall TCustomCommandDialog::Execute()
{
  CommandEdit->Items = CustomWinConfiguration->History["CustomCommand"];
  if (CommandEdit->Items->Count == 0)
  {
    TCustomCommands * CustomCommands = const_cast<TCustomCommands*>(FCustomCommands);
    for (int i = 0; i < CustomCommands->Count; i++)
    {
      CommandEdit->Items->Add(CustomCommands->Values[CustomCommands->Names[i]]);
    }
  }
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    CommandEdit->SaveToHistory();
    CustomWinConfiguration->History["CustomCommand"] = CommandEdit->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormShow(TObject * /*Sender*/)
{
  int CaptionRes;
  switch (Mode)
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

  if (Mode == ccmAdHoc)
  {
    int Shift = CommandEdit->Top - DescriptionEdit->Top;

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

    ClientHeight = ClientHeight - Shift;
  }

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    if ((Mode == ccmAdd) || (Mode == ccmEdit))
    {
      AnsiString Desc = Description;

      if (Desc.Pos("=") > 0)
      {
        DescriptionEdit->SetFocus();
        throw Exception(FMTLOAD(CUSTOM_COMMAND_INVALID, ("=")));
      }

      if (((Mode == ccmAdd) || ((Mode == ccmEdit) && (Desc != FOrigDescription))) &&
          (const_cast<TCustomCommands*>(FCustomCommands)->IndexOfName(Desc) >= 0))
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

    if (FOnValidate)
    {
      FOnValidate(Command, Params);
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
  Data = reinterpret_cast<void *>(ccSet | Params);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::CommandEditSetData(
  THistoryComboBox * /*Sender*/, Pointer Data)
{
  int IData = reinterpret_cast<int>(Data);
  if (FLAGSET(IData, ccSet))
  {
    Params = (IData & ~ccSet);
  }
}
//---------------------------------------------------------------------------
