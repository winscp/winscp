//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <TextsWin.h>

#include <VCLCommon.h>
#include "WinInterface.h"
#include "Licence.h"
#include "Tools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
AnsiString LicenceStr[2] = { "LICENCE", "LICENCE_PUTTY" };
//---------------------------------------------------------------------------
void __fastcall DoLicenceDialog(TLicence Licence)
{
  TLicenceDialog *LicenceDialog = NULL;
  try
  {
    LicenceDialog = new TLicenceDialog(Application);
    LicenceDialog->Licence = Licence;
    LicenceDialog->ShowModal();
  }
  __finally
  {
    delete LicenceDialog;
  }
}
//---------------------------------------------------------------------------
void __fastcall DoLicenceDialog(const AnsiString LicenceText)
{
  TLicenceDialog *LicenceDialog = NULL;
  try
  {
    LicenceDialog = new TLicenceDialog(Application);
    LicenceDialog->LicenceText = LicenceText;
    LicenceDialog->ShowModal();
  }
  __finally
  {
    delete LicenceDialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TLicenceDialog::TLicenceDialog(TComponent* Owner)
        : TForm(Owner)
{
  FLicence = lcNoLicence;
  UseSystemSettings(this);
}
//---------------------------------------------------------------------------
void __fastcall TLicenceDialog::SetLicence(TLicence value)
{
  if (FLicence != value)
  {
    FLicence = value;
    TStrings * LicenceList = new TStringList();
    try
    {
      LicenceList->Text = ReadResource(LicenceStr[FLicence]);
      assert(LicenceList->Count > 0);
      Caption = FMTLOAD(LICENCE_CAPTION, (LicenceList->Strings[0]));
      LicenceList->Delete(0);
      LicenceText = LicenceList->Text;
    }
    __finally
    {
      delete LicenceList;
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLicenceDialog::SetLicenceText(AnsiString value)
{
  LicenceMemo->Lines->Text = value;
}
//---------------------------------------------------------------------------
AnsiString __fastcall TLicenceDialog::GetLicenceText()
{
  return LicenceMemo->Lines->Text;
}

