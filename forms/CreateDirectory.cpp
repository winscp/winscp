//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <ScpMain.h>
#include <Configuration.h>
#include <RemoteFiles.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <Common.h>
#include <WinInterface.h>
#include <Tools.h>

#include "CreateDirectory.h"
#include "WinConfiguration.h"
//---------------------------------------------------------------------
#pragma link "XPThemes"
#pragma link "MoreButton"
#pragma link "RightsExt"
#pragma link "Rights"
#pragma resource "*.dfm"
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
    MoreButton->Expanded = GUIConfiguration->CopyParamDialogExpanded;
  }
  else
  {
    MoreButton->Expanded = false;
    MoreButton->Visible = false;
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
      GUIConfiguration->CopyParamDialogExpanded = MoreButton->Expanded;
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
