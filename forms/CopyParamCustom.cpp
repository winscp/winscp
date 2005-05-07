//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>
#include <TextsWin.h>
#include <GUIConfiguration.h>
#include <GUITools.h>
#include "CopyParamCustom.h"
#include "VCLCommon.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "XPThemes"
#pragma link "CopyParams"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCopyParamCustomDialog(TCopyParamType & CopyParam, int Options)
{
  bool Result;
  TCopyParamCustomDialog * Dialog = new TCopyParamCustomDialog(
    Application, Options, 0);
  try
  {
    Result = Dialog->Execute(CopyParam);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
// Dummy parameter distinguishes the constructor from Object Pascals TForm::CreateNew
__fastcall TCopyParamCustomDialog::TCopyParamCustomDialog(TComponent * Owner,
  int Options, int /*Dummy*/)
  : TForm(Owner)
{
  UseSystemSettings(this);
  CopyParamsFrame->Direction = pdAll;
  if (Options >= 0)
  {
    CopyParamsFrame->Options = Options;
  }
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamCustomDialog::Execute(TCopyParamType & CopyParam)
{
  CopyParamsFrame->Params = CopyParam;

  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == mrOk);

  if (Result)
  {
    CopyParamsFrame->AfterExecute();

    CopyParam = CopyParamsFrame->Params;
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamCustomDialog::FormCloseQuery(TObject * /*Sender*/,
  bool & /*CanClose*/)
{
  if (ModalResult != mrCancel)
  {
    CopyParamsFrame->Validate();
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamCustomDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
