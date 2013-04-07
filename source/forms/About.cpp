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
#ifndef NO_RESOURCES
#pragma resource "*.dfm"
#endif
//---------------------------------------------------------------------------
void __fastcall DoAboutDialog(TConfiguration * Configuration,
  bool AllowLicense, TRegistration * Registration)
{
  TAboutDialog * AboutDialog = NULL;
  try
  {
    AboutDialog = new TAboutDialog(Application, Configuration, AllowLicense,
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
  TConfiguration * Configuration, bool AllowLicense, TRegistration * Registration)
  : TForm(AOwner)
{
  FConfiguration = Configuration;
  ThirdPartyBox->VertScrollBar->Position = 0;
  UseSystemSettings(this);
  LinkLabel(HomepageLabel, LoadStr(HOMEPAGE_URL));
  LinkLabel(ForumUrlLabel, LoadStr(FORUM_URL));
  LinkLabel(PuttyLicenseLabel, L"", FirstScrollingControlEnter);
  LinkLabel(PuttyHomepageLabel, LoadStr(PUTTY_URL));
  LinkLabel(FileZillaHomepageLabel, LoadStr(FILEZILLA_URL));
  LinkLabel(OpenSSLHomepageLabel, LoadStr(OPENSSL_URL));
  LinkLabel(Toolbar2000HomepageLabel);
  LinkLabel(TBXHomepageLabel, L"", LastScrollingControlEnter);
  ApplicationLabel->ParentFont = true;
  ApplicationLabel->Font->Style = ApplicationLabel->Font->Style << fsBold;
  ApplicationLabel->Caption = AppName;
  PuttyVersionLabel->Caption = FMTLOAD(PUTTY_BASED_ON, (LoadStr(PUTTY_VERSION)));
  PuttyCopyrightLabel->Caption = LoadStr(PUTTY_COPYRIGHT);
  FileZillaVersionLabel->Caption = FMTLOAD(FILEZILLA_BASED_ON, (LoadStr(FILEZILLA_VERSION)));
  FileZillaCopyrightLabel->Caption = LoadStr(FILEZILLA_COPYRIGHT);
  OpenSSLVersionLabel->Caption = FMTLOAD(OPENSSL_BASED_ON, (LoadStr(OPENSSL_VERSION)));
  OpenSSLCopyrightLabel->Caption = LoadStr(OPENSSL_COPYRIGHT);
  WinSCPCopyrightLabel->Caption = LoadStr(WINSCP_COPYRIGHT);
  UnicodeString Translator = LoadStr(TRANSLATOR_INFO);

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
      UnicodeString Text;
      Text = FORMAT(LoadStrPart(ABOUT_REGISTRATION_LICENSES, 1),
        (Registration->Licenses >= 0 ? IntToStr(Registration->Licenses) :
          UnicodeString(LoadStrPart(ABOUT_REGISTRATION_LICENSES, 2))));
      if (!Registration->NeverExpires)
      {
        Text = FMTLOAD(ABOUT_REGISTRATION_EXPIRES,
          (Text, FormatDateTime(L"ddddd", Registration->Expiration)));
      }
      RegistrationLicensesLabel->Caption = Text;
      Text = FMTLOAD(ABOUT_REGISTRATION_PRODUCTID, (Registration->ProductId));
      if (Registration->EduLicense)
      {
        Text = FMTLOAD(ABOUT_REGISTRATION_EDULICENSE, (Text));
      }
      RegistrationProductIdLabel->Caption = Text;
      RegistrationProductIdLabel->Font->Style =
        RegistrationProductIdLabel->Font->Style << fsBold;
    }
    else
    {
      RegistrationLicensesLabel->Visible = false;
      FOnRegistrationLink = Registration->OnRegistrationLink;
      RegistrationProductIdLabel->Caption = LoadStr(ABOUT_REGISTRATION_LINK);
      LinkLabel(RegistrationProductIdLabel, L"");
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
    UnicodeString TranslatorUrl = LoadStr(TRANSLATOR_URL);
    if (!TranslatorUrl.IsEmpty())
    {
      LinkLabel(TranslatorUrlLabel, TranslatorUrl);
    }
    else
    {
      TranslatorUrlLabel->Visible = false;
    }
  }

  #ifdef NO_FILEZILLA
  int FileZillaHeight = Label1->Top - FileZillaVersionLabel->Top;
  FileZillaVersionLabel->Visible = false;
  FileZillaCopyrightLabel->Visible = false;
  FileZillaHomepageLabel->Visible = false;
  OpenSSLVersionLabel->Visible = false;
  OpenSSLCopyrightLabel->Visible = false;
  OpenSSLHomepageLabel->Visible = false;

  for (int Index = 0; Index < ThirdPartyBox->ControlCount; Index++)
  {
    TControl * Control = ThirdPartyBox->Controls[Index];
    if (Control->Top > FileZillaHomepageLabel->Top)
    {
      Control->Top = Control->Top - FileZillaHeight;
    }
  }

  ThirdPartyBox->VertScrollBar->Range = ThirdPartyBox->VertScrollBar->Range - FileZillaHeight;
  #endif

  #ifdef NO_COMPONENTS
  int ComponentsHeight = ThirdPartyBox->VertScrollBar->Range - Label1->Top;
  Label1->Visible = false;
  Label2->Visible = false;
  Toolbar2000HomepageLabel->Visible = false;
  Label5->Visible = false;
  Label6->Visible = false;
  TBXHomepageLabel->Visible = false;
  Label8->Visible = false;
  Label10->Visible = false;

  ThirdPartyBox->VertScrollBar->Range = ThirdPartyBox->VertScrollBar->Range - ComponentsHeight;
  #endif

  #ifdef NO_FILEZILLA
  #ifdef NO_COMPONENTS
  ThirdPartyBox->VertScrollBar->Range = ThirdPartyBox->ClientHeight;
  #endif
  #endif

  LicenseButton->Visible = AllowLicense;
  LoadData();
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LoadData()
{
  UnicodeString Version = FConfiguration->VersionStr;
  if (!FConfiguration->ProductName.IsEmpty() &&
      (FConfiguration->Version != FConfiguration->ProductVersion))
  {
    Version = FMTLOAD(ABOUT_BASED_ON_PRODUCT,
      (Version, FConfiguration->ProductName, FConfiguration->ProductVersion));
  }
  VersionLabel->Caption = Version;
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::PuttyLicenseLabelClick(TObject * /*Sender*/)
{
  OpenBrowser(LoadStr(PUTTY_LICENSE_URL));
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LicenseButtonClick(TObject * /*Sender*/)
{
  DoProductLicense();
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
void __fastcall TAboutDialog::RegistrationProductIdLabelClick(
  TObject * /*Sender*/)
{
  if (FOnRegistrationLink != NULL)
  {
    FOnRegistrationLink(this);
  }
}
//---------------------------------------------------------------------------
