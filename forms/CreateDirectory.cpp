//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <CoreMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <Common.h>
#include <WinInterface.h>
#include <Tools.h>

#include "CreateDirectory.h"
//---------------------------------------------------------------------
#pragma link "RightsExt"
#pragma link "Rights"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------
bool __fastcall DoCreateDirectoryDialog(AnsiString & Directory,
  TRemoteProperties * Properties, bool & SaveSettings)
{
  bool Result;
  TCreateDirectoryDialog * Dialog = new TCreateDirectoryDialog(Application);
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
__fastcall TCreateDirectoryDialog::TCreateDirectoryDialog(TComponent * AOwner):
  TForm(AOwner)
{
  UseSystemSettings(this);

  InstallPathWordBreakProc(DirectoryEdit);

  RightsFrame->AllowAddXToDirectories = false;
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
  EnableControl(RightsFrame, SetRightsCheck->Checked);
}
//---------------------------------------------------------------------------
bool __fastcall TCreateDirectoryDialog::Execute(AnsiString & Directory,
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

  bool Result = (ShowModal() != mrCancel);
  if (Result)
  {
    Directory = DirectoryEdit->Text;
    SaveSettings = SaveSettingsCheck->Checked;
    if (Properties != NULL)
    {
      if (SetRightsCheck->Checked)
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
  if (ModalResult != mrCancel)
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
