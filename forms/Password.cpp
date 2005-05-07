//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <VCLCommon.h>
#include <TextsWin.h>

#include "WinInterface.h"
#include "Password.h"
//---------------------------------------------------------------------
#pragma link "PasswordEdit"
#pragma resource "*.dfm"
//---------------------------------------------------------------------
bool __fastcall DoPasswordDialog(const AnsiString Caption,
  TPromptKind Kind, AnsiString &Password)
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
  FApplicationShowHint = Application->OnShowHint;
  Application->OnShowHint = ApplicationShowHint;
}
//---------------------------------------------------------------------
__fastcall TPasswordDialog::~TPasswordDialog()
{
  assert(Application->OnShowHint == ApplicationShowHint);
  Application->OnShowHint = FApplicationShowHint;
}
//---------------------------------------------------------------------
void __fastcall TPasswordDialog::SetPasswordCaption(const AnsiString value)
{
  AnsiString Caption = value;
  bool MultiLine = false;
  int P = Caption.Pos("\n");
  if (P > 0)
  {
    MultiLine = true;
    Caption.SetLength(P - 1);
  }
  P = Caption.Pos("\r");
  if (P > 0)
  {
    MultiLine = true;
    Caption.SetLength(P - 1);
  }

  bool NeedTrim = MultiLine ||
    (PasswordLabel->Canvas->TextWidth(Caption) > PasswordLabel->Width);
  if (NeedTrim)
  {
    static AnsiString Ellipsis(" ...");
    while (PasswordLabel->Canvas->TextWidth(Caption + Ellipsis) >
        PasswordLabel->Width)
    {
      Caption.SetLength(Caption.Length() - 1);
    }
    Caption = Caption + Ellipsis;
  }

  PasswordLabel->Caption = Caption;
  PasswordLabel->Hint = value;
  PasswordLabel->ShowHint = NeedTrim;
}
//---------------------------------------------------------------------
AnsiString __fastcall TPasswordDialog::GetPasswordCaption()
{
  return PasswordLabel->Hint;
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
void __fastcall TPasswordDialog::SetKind(TPromptKind value)
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
void __fastcall TPasswordDialog::ApplicationShowHint(AnsiString & HintStr,
  bool & CanShow, THintInfo & HintInfo)
{
  if (FApplicationShowHint != NULL)
  {
    FApplicationShowHint(HintStr, CanShow, HintInfo);
  }

  if (HintInfo.HintControl == PasswordLabel)
  {
    HintInfo.HintPos.x = PasswordLabel->ClientOrigin.x - 3;
    HintInfo.HintPos.y = PasswordLabel->ClientOrigin.y - 3;
    HintInfo.HideTimeout = 2500;
  }
}
//---------------------------------------------------------------------------
void __fastcall TPasswordDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
