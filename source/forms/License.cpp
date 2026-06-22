//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "License.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
static UnicodeString LicenseStr[2] =
  { L"LICENSE", L"LICENSE_EXPAT" };
//---------------------------------------------------------------------------
void __fastcall DoLicenseDialog(TLicense License)
{
  TLicenseDialog * LicenseDialog = NULL;
  try
  {
    LicenseDialog = new TLicenseDialog(Application, License);
    LicenseDialog->ShowModal();
  }
  __finally
  {
    delete LicenseDialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TLicenseDialog::TLicenseDialog(TComponent * Owner, TLicense License)
  : TForm(Owner)
{
  UseSystemSettings(this);

  TStrings * LicenseList = new TStringList();
  try
  {
    LicenseList->Text = ReadResource(LicenseStr[License]);
    DebugAssert(LicenseList->Count > 0);
    Caption = FMTLOAD(LICENSE_CAPTION, (LicenseList->Strings[0]));
    LicenseList->Delete(0);
    LicenseMemo->Lines->Text = LicenseList->Text;
  }
  __finally
  {
    delete LicenseList;
  }
}
