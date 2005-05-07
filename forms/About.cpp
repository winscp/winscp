//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
  
#include <SysUtils.hpp>
//---------------------------------------------------------------------
#include <VCLCommon.h>
#include <Common.h>
#include <Tools.h>
#include "WinInterface.h"
#include "About.h"
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
  LinkLabel(Toolbar2000HomepageLabel);
  LinkLabel(TBXHomepageLabel);
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
void __fastcall TAboutDialog::HomepageLabelClick(TObject * Sender)
{
  OpenBrowser(((TLabel*)Sender)->Caption);
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::EmailLabelClick(TObject * Sender)
{
  OpenBrowser("mailto:" + ((TLabel*)Sender)->Caption);
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
void __fastcall TAboutDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

