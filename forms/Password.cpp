//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <assert.h>
#include <VCLCommon.h>
#include <TextsWin.h>

#include "Password.h"
//---------------------------------------------------------------------
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPasswordDialog(const AnsiString Caption,
  TPasswordKind Kind, AnsiString &Password)
{
  bool Result = false;
  TPasswordDialog * PasswordDialog = new TPasswordDialog(Application);
  try
  {
    PasswordDialog->PasswordCaption = Caption;
    PasswordDialog->Password = "";
    PasswordDialog->Kind = Kind;
    Result = (bool)(PasswordDialog->ShowModal() == mrOk);
    
    if (Result)
    {
      Password = PasswordDialog->Password;
    }
  }
  __finally
  {
    delete PasswordDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPasswordDialog::TPasswordDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  UseSystemSettings(this);
  Kind = pkPassword;
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetPasswordCaption(const AnsiString value)
{
  PasswordLabel->Caption = value;
}
//---------------------------------------------------------------------
AnsiString __fastcall TPasswordDialog::GetPasswordCaption()
{
  return PasswordLabel->Caption;
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetPassword(const AnsiString value)
{
  PasswordEdit->Text = value;
}
//---------------------------------------------------------------------
AnsiString __fastcall TPasswordDialog::GetPassword()
{
  return PasswordEdit->Text;
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetKind(TPasswordKind value)
{
  FKind = value;
  int Title;
  switch (Kind) {
    case pkPassword: Title = PASSWORD_TITLE; break;
    case pkPassphrase: Title = PASSPHRASE_TITLE; break;
    case pkServerPrompt: Title = SERVER_PASSWORD_TITLE; break;
    default: assert(false);
  }
  Caption = LoadStr(Title);

  bool ShowServerPanel = (Kind == pkServerPrompt);
  if (ShowServerPanel != ServerPromptPanel->Visible)
  {
    ServerPromptPanel->Visible = ShowServerPanel;
    ClientHeight += (ShowServerPanel ? 1 : -1) * ServerPromptPanel->Height;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPasswordDialog::HideTypingCheckClick(TObject * /*Sender*/)
{
  PasswordEdit->Password = HideTypingCheck->Checked;
}
//---------------------------------------------------------------------------

