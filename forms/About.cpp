//---------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <SysUtils.hpp>
//---------------------------------------------------------------------
#include <VCLCommon.h>
#include "About.h"
#include "WinInterface.h"
#include "UserInterface.h"
//---------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TAboutDialog::TAboutDialog(TComponent* AOwner)
	: TForm(AOwner)
{
  ThirdPartyBox->VertScrollBar->Position = 0;
  UseSystemFont(this);
  LinkLabel(HomepageLabel);
  LinkLabel(ProductUrlLabel);
  LinkLabel(PuttyLicenceLabel);
  LinkLabel(PuttyHomepageLabel);
  LinkLabel(FilemanagerHomepageLabel);
  ApplicationLabel->Caption = AppName;
  HomepageLabel->Caption = HomepageUrl;
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

