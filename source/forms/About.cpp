//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <SysUtils.hpp>
//---------------------------------------------------------------------
#include <VCLCommon.h>
#include <Common.h>
#include <Tools.h>
#include <GUITools.h>
#include <CoreMain.h>
#include "WinInterface.h"
#include "About.h"
#include "TextsCore.h"
#include "TextsWin.h"
#ifndef NO_COMPONENTS
// must be included before WebBrowserEx.hpp to avoid ambiguity of tagLOGFONTW
#include <TB2Version.hpp>
#include <TBX.hpp>
#endif
#include <WebBrowserEx.hpp>
#include <JclBase.hpp>
#include <StrUtils.hpp>
#ifndef NO_FILEZILLA
#include <FtpFileSystem.h>
#endif
//---------------------------------------------------------------------
#pragma link "SHDocVw_OCX"
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
  UseSystemSettings(this);
  LinkLabel(HomepageLabel, LoadStr(HOMEPAGE_URL));
  LinkLabel(ForumUrlLabel, LoadStr(FORUM_URL));
  ApplicationLabel->ParentFont = true;
  ApplicationLabel->Font->Style = ApplicationLabel->Font->Style << fsBold;
  ApplicationLabel->Caption = AppName;
  WinSCPCopyrightLabel->Caption = LoadStr(WINSCP_COPYRIGHT);
  UnicodeString Translator = LoadStr(TRANSLATOR_INFO);

  if (Registration == NULL)
  {
    RegistrationLabel->Visible = false;
    RegistrationBox->Visible = false;
    ClientHeight = ClientHeight -
      (ThirdPartyPanel->Top - RegistrationBox->Top);
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

  LicenseButton->Visible = AllowLicense;

  FThirdPartyWebBrowser = CreateBrowserViewer(ThirdPartyPanel, L"");

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

  LoadThirdParty();
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::LoadThirdParty()
{
  reinterpret_cast<TLabel *>(FThirdPartyWebBrowser)->Color = clBtnFace;

  FThirdPartyWebBrowser->Navigate(L"about:blank");
  while (FThirdPartyWebBrowser->ReadyState < ::READYSTATE_INTERACTIVE)
  {
    Application->ProcessMessages();
  }

  std::unique_ptr<TFont> DefaultFont(new TFont());

  UnicodeString ThirdParty;

  ThirdParty +=
    L"<!DOCTYPE html>\n"
    L"<meta charset=\"utf-8\">\n"
    L"<html>\n"
    L"<head>\n"
    L"<style>\n"
    L"\n"
    L"body\n"
    L"{\n"
    L"  font-family: '" + DefaultFont->Name + L"';\n"
    L"  margin: 0.5em;\n"
    L"  background-color: " + ColorToWebColorStr(Color) + L";\n"
    L"}\n"
    L"\n"
    L"body\n"
    L"{\n"
    L"    font-size: " + IntToStr(DefaultFont->Size) + L"pt;\n"
    L"}\n"
    L"\n"
    L"p\n"
    L"{\n"
    L"    margin-top: 0;\n"
    L"    margin-bottom: 1em;\n"
    L"}\n"
    L"\n"
    L"a, a:visited, a:hover, a:visited, a:current\n"
    L"{\n"
    L"    color: " + ColorToWebColorStr(LinkColor) + L";\n"
    L"}\n"
    L"</style>\n"
    L"</head>\n"
    L"<body>\n";

  UnicodeString Br = "<br/>\n";
  AddPara(ThirdParty, LoadStr(ABOUT_THIRDPARTY_HEADER));

  AddPara(ThirdParty,
    FMTLOAD(PUTTY_BASED_ON, (LoadStr(PUTTY_VERSION))) + Br +
    LoadStr(PUTTY_COPYRIGHT) + Br +
    CreateLink(LoadStr(PUTTY_LICENSE_URL), LoadStr(ABOUT_THIRDPARTY_LICENSE)) + Br +
    CreateLink(LoadStr(PUTTY_URL)));

#ifndef NO_FILEZILLA

  UnicodeString OpenSSLVersionText = GetOpenSSLVersionText();
  CutToChar(OpenSSLVersionText, L' ', true); // "OpenSSL"
  UnicodeString OpenSSLVersion = CutToChar(OpenSSLVersionText, L' ', true);
  CutToChar(OpenSSLVersionText, L' ', true); // day
  CutToChar(OpenSSLVersionText, L' ', true); // month
  UnicodeString OpenSSLYear = CutToChar(OpenSSLVersionText, L' ', true);

  AddPara(ThirdParty,
    FMTLOAD(OPENSSL_BASED_ON, (OpenSSLVersion)) + Br +
    FMTLOAD(OPENSSL_COPYRIGHT2, (OpenSSLYear)) + Br +
    CreateLink(LoadStr(OPENSSL_URL)));

  AddPara(ThirdParty,
    LoadStr(FILEZILLA_BASED_ON2) + Br +
    LoadStr(FILEZILLA_COPYRIGHT2) + Br +
    CreateLink(LoadStr(FILEZILLA_URL)));

#endif

  AddPara(ThirdParty,
    FMTLOAD(NEON_BASED_ON, (NeonVersion())) + Br +
    LoadStr(NEON_COPYRIGHT) + Br +
    CreateLink(LoadStr(NEON_URL)));

  #define EXPAT_LICENSE_URL L"license:expat"

  AddPara(ThirdParty,
    FMTLOAD(EXPAT_BASED_ON, (ExpatVersion())) + Br +
    CreateLink(EXPAT_LICENSE_URL, LoadStr(ABOUT_THIRDPARTY_LICENSE)) + Br +
    CreateLink(LoadStr(EXPAT_URL)));

  AddBrowserLinkHandler(FThirdPartyWebBrowser, EXPAT_LICENSE_URL, ExpatLicenceHandler);

#ifndef NO_COMPONENTS

  AddPara(ThirdParty,
    FMTLOAD(ABOUT_TOOLBAR2000, (Toolbar2000Version)) + Br +
    LoadStr(ABOUT_TOOLBAR2000_COPYRIGHT) + Br +
    CreateLink(LoadStr(ABOUT_TOOLBAR2000_URL)));

  AddPara(ThirdParty,
    FMTLOAD(ABOUT_TBX, (TBXVersionString)) + Br +
    LoadStr(ABOUT_TBX_COPYRIGHT) + Br +
    CreateLink(LoadStr(ABOUT_TBX_URL)));

  AddPara(ThirdParty,
    LoadStr(ABOUT_FILEMANAGER) + Br +
    LoadStr(ABOUT_FILEMANAGER_COPYRIGHT));

#endif

  UnicodeString JclVersion =
    FormatVersion(JclVersionMajor, JclVersionMinor, JclVersionRelease);
  AddPara(ThirdParty,
    FMTLOAD(ABOUT_JCL, (JclVersion)) + Br +
    CreateLink(LoadStr(ABOUT_JCL_URL)));

  AddPara(ThirdParty,
    LoadStr(ABOUT_PNG) + Br +
    LoadStr(ABOUT_PNG_COPYRIGHT) + Br +
    CreateLink(LoadStr(ABOUT_PNG_URL)));

  ThirdParty +=
    L"</body>\n"
    L"</html>\n";

  std::unique_ptr<TMemoryStream> ThirdPartyStream(new TMemoryStream());
  UTF8String ThirdPartyUTF8 = UTF8String(ThirdParty);
  ThirdPartyStream->Write(ThirdPartyUTF8.c_str(), ThirdPartyUTF8.Length());
  ThirdPartyStream->Seek(0, 0);

  // For steam-loaded document, when set only after loading from OnDocumentComplete,
  // browser stops working
  SetBrowserDesignModeOff(FThirdPartyWebBrowser);

  TStreamAdapter * ThirdPartyStreamAdapter = new TStreamAdapter(ThirdPartyStream.get(), soReference);
  IPersistStreamInit * PersistStreamInit = NULL;
  if (ALWAYS_TRUE(FThirdPartyWebBrowser->Document != NULL) &&
      SUCCEEDED(FThirdPartyWebBrowser->Document->QueryInterface(IID_IPersistStreamInit, (void **)&PersistStreamInit)) &&
      ALWAYS_TRUE(PersistStreamInit != NULL))
  {
    PersistStreamInit->Load(static_cast<_di_IStream>(*ThirdPartyStreamAdapter));
  }
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::AddPara(UnicodeString & Text, const UnicodeString & S)
{
  Text += L"<p>" + S + L"</p>\n";
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TAboutDialog::CreateLink(const UnicodeString & URL, const UnicodeString & Title)
{
  return FORMAT(L"<a href=\"%s\">%s</a>", (URL, Title.IsEmpty() ? URL : Title));
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
void __fastcall TAboutDialog::RegistrationProductIdLabelClick(
  TObject * /*Sender*/)
{
  if (FOnRegistrationLink != NULL)
  {
    FOnRegistrationLink(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::OKButtonMouseDown(TObject * /*Sender*/,
  TMouseButton Button, TShiftState Shift, int /*X*/, int /*Y*/)
{
  if ((Button == mbRight) && Shift.Contains(ssAlt))
  {
    try
    {
      ACCESS_VIOLATION_TEST;
    }
    catch (Exception & E)
    {
      throw ExtException(&E, MainInstructions(L"Internal error test."));
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TAboutDialog::ExpatLicenceHandler(TObject * /*Sender*/)
{
  DoLicenseDialog(lcExpat);
}
//---------------------------------------------------------------------------
