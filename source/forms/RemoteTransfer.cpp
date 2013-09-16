//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <VCLCommon.h>
#include <TextsWin.h>
#include <CustomWinConfiguration.h>
#include <CoreMain.h>
#include <WinInterface.h>

#include "RemoteTransfer.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "HistoryComboBox"
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
bool __fastcall DoRemoteCopyDialog(TStrings * Sessions, TStrings * Directories,
  TDirectRemoteCopy AllowDirectCopy, void *& Session, UnicodeString & Target, UnicodeString & FileMask,
  bool & DirectCopy)
{
  bool Result;
  TRemoteTransferDialog * Dialog = SafeFormCreate<TRemoteTransferDialog>();
  try
  {
    Dialog->Init(Sessions, Directories, AllowDirectCopy);
    Result = Dialog->Execute(Session, Target, FileMask, DirectCopy);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall TRemoteTransferDialog::TRemoteTransferDialog(TComponent * Owner)
  : TForm(Owner)
{
  UseSystemSettings(this);

  Caption = LoadStr(REMOTE_COPY_TITLE);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::Init(TStrings * Sessions,
  TStrings * Directories, TDirectRemoteCopy AllowDirectCopy)
{
  SessionCombo->Items = Sessions;
  FDirectories = Directories;
  assert(SessionCombo->Items->Count > 0);
  assert(SessionCombo->Items->Count == FDirectories->Count);
  FAllowDirectCopy = AllowDirectCopy;
}
//---------------------------------------------------------------------------
bool __fastcall TRemoteTransferDialog::Execute(void *& Session, UnicodeString & Target,
  UnicodeString & FileMask, bool & DirectCopy)
{
  FCurrentSession = -1;
  for (int Index = 0; Index < SessionCombo->Items->Count; Index++)
  {
    if (SessionCombo->Items->Objects[Index] == Session)
    {
      FCurrentSession = Index;
      SessionCombo->ItemIndex = Index;
      break;
    }
  }
  assert(FCurrentSession >= 0);
  DirectoryEdit->Items = CustomWinConfiguration->History[L"RemoteTarget"];
  DirectoryEdit->Text = UnixIncludeTrailingBackslash(Target) + FileMask;
  FDirectCopy = DirectCopy;
  NotDirectCopyCheck->Checked = !DirectCopy;
  bool Result = (ShowModal() == DefaultResult(this));
  if (Result)
  {
    Session = SessionCombo->Items->Objects[SessionCombo->ItemIndex];
    CustomWinConfiguration->History[L"RemoteTarget"] = DirectoryEdit->Items;
    Target = UnixExtractFilePath(DirectoryEdit->Text);
    FileMask = UnixExtractFileName(DirectoryEdit->Text);
    DirectCopy = !NotDirectCopyCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::UpdateControls()
{
  EnableControl(NotDirectCopyCheck,
    (SessionCombo->ItemIndex == FCurrentSession) &&
    (FAllowDirectCopy != drcDisallow));
  EnableControl(OkButton, !DirectoryEdit->Text.IsEmpty());
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::ControlChange(TObject * /*Sender*/)
{
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::FormShow(TObject * /*Sender*/)
{
  InstallPathWordBreakProc(DirectoryEdit);

  UpdateControls();
  DirectoryEdit->SetFocus();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::SessionComboChange(TObject * /*Sender*/)
{
  DirectoryEdit->Text =
    UnixIncludeTrailingBackslash(FDirectories->Strings[SessionCombo->ItemIndex]) +
    UnixExtractFileName(DirectoryEdit->Text);
  if (SessionCombo->ItemIndex == FCurrentSession)
  {
    NotDirectCopyCheck->Checked = !FDirectCopy;
  }
  else
  {
    NotDirectCopyCheck->Checked = true;
  }
  UpdateControls();
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult == DefaultResult(this))
  {
    if ((SessionCombo->ItemIndex == FCurrentSession) &&
        (FAllowDirectCopy == drcConfirmCommandSession) &&
        !NotDirectCopyCheck->Checked &&
        GUIConfiguration->ConfirmCommandSession)
    {
      TMessageParams Params(mpNeverAskAgainCheck);
      unsigned int Answer = MessageDialog(LoadStr(REMOTE_COPY_COMMAND_SESSION),
        qtConfirmation, qaOK | qaCancel, HelpKeyword, &Params);
      if (Answer == qaNeverAskAgain)
      {
        GUIConfiguration->ConfirmCommandSession = false;
      }
      else if (Answer != qaOK)
      {
        Abort();
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TRemoteTransferDialog::NotDirectCopyCheckClick(
  TObject * /*Sender*/)
{
  if (SessionCombo->ItemIndex == FCurrentSession)
  {
    FDirectCopy = !NotDirectCopyCheck->Checked;
  }
}
//---------------------------------------------------------------------------
