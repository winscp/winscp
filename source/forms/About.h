//----------------------------------------------------------------------------
#ifndef AboutH
#define AboutH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.ExtCtrls.hpp>
#include <Vcl.Forms.hpp>
#include <Vcl.Graphics.hpp>
#include <Vcl.StdCtrls.hpp>
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
  TLabel *FileZillaVersionLabel;
  TLabel *FileZillaCopyrightLabel;
  TStaticText *FileZillaHomepageLabel;
  TLabel *OpenSSLVersionLabel;
  TStaticText *OpenSSLHomepageLabel;
  TLabel *OpenSSLCopyrightLabel;
  void __fastcall PuttyLicenseLabelClick(TObject *Sender);
  void __fastcall LicenseButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall RegistrationProductIdLabelClick(TObject *Sender);
private:
  TConfiguration * FConfiguration;
  TNotifyEvent FOnRegistrationLink;
  void __fastcall FirstScrollingControlEnter(TObject * Sender);
  void __fastcall LastScrollingControlEnter(TObject * Sender);
  void __fastcall FixWrappedLabelSize(TLabel * Label);
public:
  virtual __fastcall TAboutDialog(TComponent * AOwner,
    TConfiguration * Configuration, bool AllowLicense, TRegistration * Registration);
  void __fastcall LoadData();
};
//----------------------------------------------------------------------------
#endif
