//---------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include <RemoteFiles.h>
#include "CreateDirectory.h"
//---------------------------------------------------------------------
#pragma link "Rights"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoCreateDirectoryDialog(UnicodeString & Directory,
  TRemoteProperties * Properties, int AllowedChanges, bool & SaveSettings)
{
  bool Result;
  TCreateDirectoryDialog * Dialog = new TCreateDirectoryDialog(Application, AllowedChanges, (Properties != NULL));
  try
  {
    Result = Dialog->Execute(Directory, Properties, SaveSettings);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TCreateDirectoryDialog::TCreateDirectoryDialog(TComponent * AOwner, int AllowedChanges, bool Remote):
  TForm(AOwner)
{
  UseSystemSettings(this);

  RightsFrame->AllowAddXToDirectories = false;
  FAllowedChanges = AllowedChanges;
  if (!Remote)
  {
    AttributesGroup->Visible = false;
    ClientHeight = ClientHeight - AttributesGroup->Height;
  }
}
//---------------------------------------------------------------------
__fastcall TCreateDirectoryDialog::~TCreateDirectoryDialog()
{
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::UpdateControls()
{
  EnableControl(OKBtn, !DirectoryEdit->Text.Trim().IsEmpty());
  EnableControl(SetRightsCheck, FLAGSET(FAllowedChanges, cpMode));
  EnableControl(RightsFrame, SetRightsCheck->Enabled && SetRightsCheck->Checked);
  EnableControl(SaveSettingsCheck, (FAllowedChanges != 0));
}
//---------------------------------------------------------------------------
bool __fastcall TCreateDirectoryDialog::Execute(UnicodeString & Directory,
  TRemoteProperties * Properties, bool & SaveSettings)
{
  DirectoryEdit->Text = Directory;
  SaveSettingsCheck->Checked = SaveSettings;
  if (Properties != NULL)
  {
    bool SetRights = Properties->Valid.Contains(vpRights);
    SetRightsCheck->Checked = SetRights;
    // expect sensible value even if rights are not set valid
    RightsFrame->Rights = Properties->Rights;
  }

  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    Directory = DirectoryEdit->Text;
    SaveSettings = SaveSettingsCheck->Checked;
    if (Properties != NULL)
    {
      if (SetRightsCheck->Enabled && SetRightsCheck->Checked)
      {
        Properties->Valid = Properties->Valid << vpRights;
        Properties->Rights = RightsFrame->Rights;
      }
      else
      {
        Properties->Valid = Properties->Valid >> vpRights;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::DirectoryEditChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(DirectoryEdit);

  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TCreateDirectoryDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
