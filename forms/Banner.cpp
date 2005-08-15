//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include <VCLCommon.h>
#include "WinInterface.h"
#include "Banner.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
void __fastcall DoBannerDialog(AnsiString SessionName, const AnsiString & Banner,
  bool & NeverShowAgain)
{
  TBannerDialog * BannerDialog = NULL;
  try
  {
    BannerDialog = new TBannerDialog(Application, SessionName, Banner);
    BannerDialog->Execute(NeverShowAgain);
  }
  __finally
  {
    delete BannerDialog;
  }
}
//---------------------------------------------------------------------------
__fastcall TBannerDialog::TBannerDialog(TComponent * Owner,
  AnsiString SessionName, const AnsiString & Banner)
  : TForm(Owner)
{
  UseSystemSettings(this);
  Caption = FORMAT("%s - %s", (Caption, SessionName));
  BannerMemo->Lines->Text = Banner;
}
//---------------------------------------------------------------------------
bool __fastcall TBannerDialog::Execute(bool & NeverShowAgain)
{
  NeverShowAgainCheck->Checked = NeverShowAgain;
  bool Result = (ShowModal() == mrOk);
  if (Result)
  {
    NeverShowAgain = NeverShowAgainCheck->Checked;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TBannerDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------

