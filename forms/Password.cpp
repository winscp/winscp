//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <VCLCommon.h>
#include "Password.h"
//---------------------------------------------------------------------
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
Boolean __fastcall DoPasswordDialog(
  const AnsiString Caption, AnsiString &Password)
{
  Boolean Result = False;
  TPasswordDialog *PasswordDialog = new TPasswordDialog(Application);
  try {
    PasswordDialog->PasswordCaption = Caption;
    PasswordDialog->Password = "";
    Result = (Boolean)(PasswordDialog->ShowModal() == mrOk);
    if (Result) Password = PasswordDialog->Password;
  } __finally {
    delete PasswordDialog;
  }
  return Result;
}
//---------------------------------------------------------------------
__fastcall TPasswordDialog::TPasswordDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  UseSystemFont(this);
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetPasswordCaption(AnsiString value)
{
  PasswordLabel->Caption = value;
}
//---------------------------------------------------------------------
AnsiString __fastcall TPasswordDialog::GetPasswordCaption()
{
  return PasswordLabel->Caption;
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetPassword(AnsiString value)
{
  PasswordEdit->Text = value;
}
//---------------------------------------------------------------------
AnsiString __fastcall TPasswordDialog::GetPassword()
{
  return PasswordEdit->Text;
}
