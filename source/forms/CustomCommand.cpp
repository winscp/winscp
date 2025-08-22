//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <Terminal.h>
#include "CustomCommand.h"
//---------------------------------------------------------------------------
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCustomCommandDialog(TCustomCommandType & Command,
  const TCustomCommandList * CustomCommandList,
  TCustomCommandsMode Mode, int Options, TCustomCommandValidate OnValidate,
  const TShortCuts * ShortCuts)
{
  bool Result;
  TCustomCommandDialog * Dialog = new TCustomCommandDialog(
    GetFormOwner(), CustomCommandList, Mode, Options, OnValidate, ShortCuts);
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
  std::unique_ptr<TStrings> HintStrings(TextToStringList(LoadStr(CUSTOM_COMMAND_PATTERNS_HINT5)));
  HintStrings->Insert(6, LoadStr(CUSTOM_COMMAND_PATTERNS_HINT6));
  HintStrings->Insert(11, LoadStr(PATTERNS_HINT_K));
  HintStrings->Insert(18, LoadStr(PATTERNS_HINT_BACKSLASH));
  HintLabel(HintText, TrimRight(StringsToText(HintStrings.get())));

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
    DebugAssert(ShortCuts != NULL);
    InitializeShortCutCombo(ShortCutCombo, *ShortCuts);
  }

  FOptions = Options;

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::UpdateControls()
{
  EnableControl(RemoteCommandButton, FLAGCLEAR(FOptions, ccoDisableRemote));

  UnicodeString Command = CommandEdit->Text;
  EnableControl(OkButton, !Command.IsEmpty() && !DescriptionEdit->Text.IsEmpty());

  bool RemoteCommand = RemoteCommandButton->Checked;
  bool AllowRecursive = true;
  bool AllowApplyToDirectories = true;
  bool AllowRemoteFiles = false;
  try
  {
    TRemoteCustomCommand RemoteCustomCommand;
    TLocalCustomCommand LocalCustomCommand;
    TFileCustomCommand * FileCustomCommand =
      (RemoteCommand ? &RemoteCustomCommand : &LocalCustomCommand);

    TInteractiveCustomCommand InteractiveCustomCommand(FileCustomCommand);
    UnicodeString Cmd = InteractiveCustomCommand.Complete(Command, false);
    bool FileCommand = FileCustomCommand->IsFileCommand(Cmd);
    AllowRemoteFiles = !RemoteCommand && FileCustomCommand->IsRemoteFileCommand(Cmd);
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

  EnableControl(RecursiveCheck, AllowRecursive && (!RemoteFilesCheck->Enabled || !RemoteFilesCheck->Checked));
  EnableControl(ApplyToDirectoriesCheck, AllowApplyToDirectories);
  EnableControl(ShowResultsCheck, RemoteCommand);
  EnableControl(RemoteFilesCheck,
    FLAGCLEAR(FOptions, ccoDisableRemoteFiles) && AllowRemoteFiles &&
    (!RecursiveCheck->Enabled || !RecursiveCheck->Checked));
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
  RemoteFilesCheck->Checked = FLAGSET(value, ccRemoteFiles);
}
//---------------------------------------------------------------------------
int __fastcall TCustomCommandDialog::GetParams()
{
  return
    (FParams & ~(ccApplyToDirectories | ccRecursive | ccLocal |
       ccShowResults | ccCopyResults | ccRemoteFiles)) |
    FLAGMASK(!RemoteCommandButton->Checked, ccLocal) |
    FLAGMASK(ApplyToDirectoriesCheck->Checked, ccApplyToDirectories) |
    FLAGMASK(RecursiveCheck->Checked && RecursiveCheck->Enabled, ccRecursive) |
    FLAGMASK(ShowResultsCheck->Checked && ShowResultsCheck->Enabled, ccShowResults) |
    FLAGMASK(CopyResultsCheck->Checked && CopyResultsCheck->Enabled, ccCopyResults) |
    FLAGMASK(RemoteFilesCheck->Checked && RemoteFilesCheck->Enabled, ccRemoteFiles);
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TCustomCommandDialog::Execute(TCustomCommandType & Command)
{
  CommandEdit->Items = CustomWinConfiguration->History[L"CustomCommand"];
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
  if (FMode != ccmAdHoc)
  {
    SetShortCutCombo(ShortCutCombo, Command.ShortCut);
  }

  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    GetCommand(Command);

    CommandEdit->SaveToHistory();
    CustomWinConfiguration->History[L"CustomCommand"] = CommandEdit->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    if ((FMode == ccmAdd) || (FMode == ccmEdit))
    {
      UnicodeString Desc = DescriptionEdit->Text;

      if (Desc.Pos(L"=") > 0)
      {
        DescriptionEdit->SetFocus();
        throw Exception(FMTLOAD(CUSTOM_COMMAND_INVALID, (L"=")));
      }

      if (((FMode == ccmAdd) || ((FMode == ccmEdit) && (Desc != FOrigDescription))) &&
          (FCustomCommandList->Find(Desc) != NULL))
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

      UnicodeString Command = CommandEdit->Text;
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
  if (FMode != ccmAdHoc)
  {
    Command.ShortCut = GetShortCutCombo(ShortCutCombo);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCustomCommandDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(CommandEdit);
}
//---------------------------------------------------------------------------
