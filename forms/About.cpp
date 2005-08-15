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
  LinkLabel(HomepageLabel, LoadStr(HOMEPAGE_URL));
  LinkLabel(ForumUrlLabel, LoadStr(FORUM_URL));
  LinkLabel(PuttyLicenceLabel, "", FirstScrollingControlEnter);
  LinkLabel(PuttyHomepageLabel, LoadStr(PUTTY_URL));
  LinkLabel(Toolbar2000HomepageLabel);
  LinkLabel(TBXHomepageLabel, "", LastScrollingControlEnter);
  ApplicationLabel->ParentFont = true;
  ApplicationLabel->Font->Style = ApplicationLabel->Font->Style << fsBold;
  ApplicationLabel->Caption = AppName;
  PuttyVersionLabel->Caption = FMTLOAD(PUTTY_BASED_ON, (LoadStr(PUTTY_VERSION)));
  PuttyCopyrightLabel->Caption = LoadStr(PUTTY_COPYRIGHT);
  WinSCPCopyrightLabel->Caption = LoadStr(WINSCP_COPYRIGHT);
  AnsiString Translator = LoadStr(TRANSLATOR_INFO);
  if (Translator.IsEmpty())
  {
    TranslatorLabel->Visible = false;
    TranslatorUrlLabel->Visible = false;
    ClientHeight = ClientHeight -
      (TranslatorLabel->Top - ProductSpecificMessageLabel->Top);
  }
  else
  {
    TranslatorLabel->Caption = LoadStr(TRANSLATOR_INFO);
    LinkLabel(TranslatorUrlLabel, LoadStr(TRANSLATOR_URL));
  }
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
  AnsiString Version = Configuration->VersionStr;
  if (Configuration->Version != Configuration->ProductVersion)
  {
    Version = FMTLOAD(ABOUT_BASED_ON_PRODUCT,
      (Configuration->ProductName, Configuration->ProductVersion));
  }
  VersionLabel->Caption = Version;
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
void __fastcall TAboutDialog::FirstScrollingControlEnter(TObject * /*Sender*/)
{
  ThirdPartyBox->VertScrollBar->Position = 0;
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LastScrollingControlEnter(TObject * /*Sender*/)
{
  ThirdPartyBox->VertScrollBar->Position =
    ThirdPartyBox->VertScrollBar->Range - ThirdPartyBox->ClientHeight;
}
//---------------------------------------------------------------------------

