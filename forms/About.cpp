//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
  
#include <SysUtils.hpp>
//---------------------------------------------------------------------
#include <VCLCommon.h>
#include <Common.h>
#include "About.h"
#include "WinInterface.h"
#include "TextsCore.h"
#include "TextsWin.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TAboutDialog::TAboutDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  ThirdPartyBox->VertScrollBar->Position = 0;
  UseSystemSettings(this);
  LinkLabel(HomepageLabel);
  LinkLabel(ForumUrlLabel);
  LinkLabel(PuttyLicenceLabel);
  LinkLabel(PuttyHomepageLabel);
  LinkLabel(FilemanagerHomepageLabel);
  ApplicationLabel->Caption = AppName;
  HomepageLabel->Caption = LoadStr(HOMEPAGE_URL);
  ForumUrlLabel->Caption = LoadStr(FORUM_URL);
  PuttyHomepageLabel->Caption = LoadStr(PUTTY_URL);
  PuttyVersionLabel->Caption = FMTLOAD(PUTTY_BASED_ON, (LoadStr(PUTTY_VERSION)));
  PuttyCopyrightLabel->Caption = LoadStr(PUTTY_COPYRIGHT);
  WinSCPCopyrightLabel->Caption = LoadStr(WINSCP_COPYRIGHT);
  TranslatorLabel->Caption = LoadStr(TRANSLATOR_INFO);
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::SetConfiguration(TConfiguration * value)
{
  if (FConfiguration != value)
  {
    FConfiguration = value;
    LoadData();
  }
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LoadData()
{
  VersionLabel->Caption = Configuration->VersionStr;
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::OpenAddress(const AnsiString Address)
{
  ShellExecute(Handle, "open", Address.c_str(),
    NULL, NULL, SW_SHOWNORMAL);
} 
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::HomepageLabelClick(TObject * Sender)
{
  OpenAddress(((TLabel*)Sender)->Caption);
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::EmailLabelClick(TObject * Sender)
{
  OpenAddress("mailto:" + ((TLabel*)Sender)->Caption);
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::DisplayLicence(TObject * Sender)
{
  DoLicenceDialog((TLicence)((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LicenceButtonClick(TObject * /*Sender*/)
{
  DoProductLicence();
}
//---------------------------------------------------------------------------
bool __fastcall TAboutDialog::GetAllowLicence()
{
  return LicenceButton->Visible;
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::SetAllowLicence(bool value)
{
  LicenceButton->Visible = value;
}
//---------------------------------------------------------------------------

