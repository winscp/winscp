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
void __fastcall DoAboutDialog(TConfiguration * Configuration,
  bool AllowLicence, TRegistration * Registration)
{
  TAboutDialog * AboutDialog = NULL;
  try
  {
    AboutDialog = new TAboutDialog(Application, Configuration, AllowLicence,
      Registration);
    AboutDialog->ShowModal();
  }
  __finally
  {
    delete AboutDialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TAboutDialog::TAboutDialog(TComponent * AOwner,
  TConfiguration * Configuration, bool AllowLicence, TRegistration * Registration)
  : TForm(AOwner)
{
  FConfiguration = Configuration;
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

  if (Registration == NULL)
  {
    RegistrationLabel->Visible = false;
    RegistrationBox->Visible = false;
    ClientHeight = ClientHeight -
      (ThirdPartyBox->Top - RegistrationBox->Top);
  }
  else
  {
    RegistrationSubjectLabel->Caption = Registration->Subject;
    if (Registration->Registered)
    {
      AnsiString Text;
      Text = FORMAT(LoadStrPart(ABOUT_REGISTRATION_LICENCES, 1),
        (Registration->Licences >= 0 ? IntToStr(Registration->Licences) :
          LoadStrPart(ABOUT_REGISTRATION_LICENCES, 2)));
      if (!Registration->NeverExpires)
      {
        Text = FMTLOAD(ABOUT_REGISTRATION_EXPIRES,
          (Text, FormatDateTime("ddddd", Registration->Expiration)));
      }
      RegistrationLicencesLabel->Caption = Text;
      Text = FMTLOAD(ABOUT_REGISTRATION_PRODUCTID, (Registration->ProductId));
      if (Registration->EduLicense)
      {
        Text = FMTLOAD(ABOUT_REGISTRATION_EDULICENCE, (Text));
      }
      RegistrationProductIdLabel->Caption = Text;
      RegistrationProductIdLabel->Font->Style =
        RegistrationProductIdLabel->Font->Style << fsBold;
    }
    else
    {
      RegistrationLicencesLabel->Visible = false;
      RegistrationProductIdLabel->Visible = false;
    }
  }

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
  LicenceButton->Visible = AllowLicence;
  LoadData();
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LoadData()
{
  AnsiString Version = FConfiguration->VersionStr;
  if (FConfiguration->Version != FConfiguration->ProductVersion)
  {
    Version = FMTLOAD(ABOUT_BASED_ON_PRODUCT,
      (Version, FConfiguration->ProductName, FConfiguration->ProductVersion));
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

