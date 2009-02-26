//----------------------------------------------------------------------------
#ifndef AboutH
#define AboutH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
//----------------------------------------------------------------------------
#include <Configuration.h>
//----------------------------------------------------------------------------
class TAboutDialog : public TForm
{
__published:
  TLabel *ApplicationLabel;
  TLabel *VersionLabel;
  TLabel *WinSCPCopyrightLabel;
  TStaticText *HomepageLabel;
  TLabel *ProductSpecificMessageLabel;
  TStaticText *ForumUrlLabel;
  TScrollBox *ThirdPartyBox;
  TLabel *PuttyVersionLabel;
  TLabel *PuttyCopyrightLabel;
  TStaticText *PuttyHomepageLabel;
  TLabel *Label7;
  TLabel *Label8;
  TLabel *Label10;
  TButton *OKButton;
  TButton *LicenseButton;
  TStaticText *PuttyLicenseLabel;
  TLabel *TranslatorLabel;
  TLabel *Label1;
  TLabel *Label2;
  TStaticText *Toolbar2000HomepageLabel;
  TLabel *Label5;
  TLabel *Label6;
  TStaticText *TBXHomepageLabel;
  TButton *HelpButton;
  TImage *Image;
  TStaticText *TranslatorUrlLabel;
  TLabel *Label3;
  TLabel *RegistrationLabel;
  TScrollBox *RegistrationBox;
  TLabel *RegistrationLicensesLabel;
  TStaticText *RegistrationProductIdLabel;
  TLabel *RegistrationSubjectLabel;
  TLabel *Label4;
  TLabel *FileZillaVersionLabel;
  TLabel *FileZillaCopyrightLabel;
  TStaticText *FileZillaHomepageLabel;
  TLabel *OpenSSLVersionLabel;
  TStaticText *OpenSSLHomepageLabel;
  TLabel *OpenSSLCopyrightLabel;
  void __fastcall DisplayLicense(TObject *Sender);
  void __fastcall LicenseButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall RegistrationProductIdLabelClick(TObject *Sender);
private:
  TConfiguration * FConfiguration;
  TNotifyEvent FOnRegistrationLink;
  void __fastcall FirstScrollingControlEnter(TObject * Sender);
  void __fastcall LastScrollingControlEnter(TObject * Sender);
public:
  virtual __fastcall TAboutDialog(TComponent * AOwner,
    TConfiguration * Configuration, bool AllowLicense, TRegistration * Registration);
  void __fastcall LoadData();
};
//----------------------------------------------------------------------------
#endif
