//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "FullSynchronize.h"
#include "VCLCommon.h"

#include <ScpMain.h>
#include <Configuration.h>
#include <TextsWin.h>
#include <CustomWinConfiguration.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPThemes"
#pragma link "HistoryComboBox"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoFullSynchronizeDialog(TSynchronizeMode & Mode, int & Params,
  AnsiString & LocalDirectory, AnsiString & RemoteDirectory, bool & SaveSettings)
{
  bool Result;
  TFullSynchronizeDialog * Dialog = new TFullSynchronizeDialog(Application);
  try
  {
    Dialog->Mode = Mode;
    Dialog->Params = Params;
    Dialog->LocalDirectory = LocalDirectory;
    Dialog->RemoteDirectory = RemoteDirectory;
    Dialog->SaveSettings = SaveSettings;
    Result = Dialog->Execute();
    if (Result)
    {
      Mode = Dialog->Mode;
      Params = Dialog->Params;
      LocalDirectory = Dialog->LocalDirectory;
      RemoteDirectory = Dialog->RemoteDirectory;
      SaveSettings = Dialog->SaveSettings;
    }
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TFullSynchronizeDialog::TFullSynchronizeDialog(TComponent* Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  FParams = 0;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::UpdateControls()
{
  EnableControl(SynchronizeDeleteCheck, !SynchronizeBothButton->Checked);
  EnableControl(OkButton, !LocalDirectoryEdit->Text.IsEmpty() &&
    !RemoteDirectoryEdit->Text.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::Execute()
{
  LocalDirectoryEdit->Items = CustomWinConfiguration->History["LocalDirectory"];
  RemoteDirectoryEdit->Items = CustomWinConfiguration->History["RemoteDirectory"];
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    LocalDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["LocalDirectory"] = LocalDirectoryEdit->Items;
    RemoteDirectoryEdit->SaveToHistory();
    CustomWinConfiguration->History["RemoteDirectory"] = RemoteDirectoryEdit->Items;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetRemoteDirectory(const AnsiString value)
{
  RemoteDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFullSynchronizeDialog::GetRemoteDirectory()
{
  return RemoteDirectoryEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetLocalDirectory(const AnsiString value)
{
  LocalDirectoryEdit->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TFullSynchronizeDialog::GetLocalDirectory()
{
  return LocalDirectoryEdit->Text;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetMode(TSynchronizeMode value)
{
  switch (value)
  {
    case smRemote:
       SynchronizeRemoteButton->Checked = true;
       break;

    case smLocal:
       SynchronizeLocalButton->Checked = true;
       break;

    case smBoth:
       SynchronizeBothButton->Checked = true;
       break;

    default:
      assert(false);
  }
}
//---------------------------------------------------------------------------
TSynchronizeMode __fastcall TFullSynchronizeDialog::GetMode()
{
  if (SynchronizeRemoteButton->Checked)
  {
    return smRemote;
  }
  else if (SynchronizeLocalButton->Checked)
  {
    return smLocal;
  }
  else
  {
    assert(SynchronizeBothButton->Checked);
    return smBoth;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetParams(int value)
{
  FParams = value & ~(spDelete | spNoConfirmation | spExistingOnly | spPreviewChanges);
  SynchronizeDeleteCheck->Checked = FLAGSET(value, spDelete);
  SynchronizeNoConfirmationCheck->Checked = FLAGSET(value, spNoConfirmation);
  SynchronizeExistingOnlyCheck->Checked = FLAGSET(value, spExistingOnly);
  SynchronizePreviewChangesCheck->Checked = FLAGSET(value, spPreviewChanges);
}
//---------------------------------------------------------------------------
int __fastcall TFullSynchronizeDialog::GetParams()
{
  return FParams |
    (SynchronizeDeleteCheck->Checked ? spDelete : 0) |
    (SynchronizeNoConfirmationCheck->Checked ? spNoConfirmation : 0) |
    (SynchronizeExistingOnlyCheck->Checked ? spExistingOnly : 0) |
    (SynchronizePreviewChangesCheck->Checked ? spPreviewChanges : 0);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::LocalDirectoryBrowseButtonClick(
      TObject * /*Sender*/)
{
  AnsiString Directory = LocalDirectoryEdit->Text;
  if (SelectDirectory(Directory, LoadStr(SELECT_LOCAL_DIRECTORY), false))
  {
    LocalDirectoryEdit->Text = Directory;
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & CanClose)
{
  if (ModalResult != mrCancel)
  {
    AnsiString Dir = LocalDirectoryEdit->Text;
    AnsiString Drive = ExtractFileDrive(Dir);
    if (Drive.IsEmpty() || (Drive.Length() != 2) || (Drive[2] != ':'))
    {
      LocalDirectoryEdit->SetFocus();
      LocalDirectoryEdit->SelectAll();
      SimpleErrorDialog(LoadStr(ABSOLUTE_PATH_REQUIRED));
      CanClose = false;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::SetSaveSettings(bool value)
{
  SaveSettingsCheck->Checked = value;
}
//---------------------------------------------------------------------------
bool __fastcall TFullSynchronizeDialog::GetSaveSettings()
{
  return SaveSettingsCheck->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::DirectoryEditKeyDown(
  TObject * Sender, WORD & Key, TShiftState Shift)
{
  PathComboBoxKeyDown(dynamic_cast<TCustomComboBox*>(Sender), Key, Shift,
    (Sender == RemoteDirectoryEdit));
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::TransferPreferencesButtonClick(
  TObject * /*Sender*/)
{
  DoPreferencesDialog(pmTransfer);
}
//---------------------------------------------------------------------------
void __fastcall TFullSynchronizeDialog::FormShow(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------

