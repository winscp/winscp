//---------------------------------------------------------------------------
#include <FormsPCH.h>
#pragma hdrstop

#include "CopyParamCustom.h"
//---------------------------------------------------------------------------
#pragma link "CopyParams"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
bool __fastcall DoCopyParamCustomDialog(TCopyParamType & CopyParam,
  int CopyParamAttrs)
{
  bool Result;
  TCopyParamCustomDialog * Dialog = new TCopyParamCustomDialog(
    GetFormOwner(), CopyParamAttrs, 0);
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
  int CopyParamAttrs, int /*Dummy*/)
  : TForm(Owner)
{
  UseSystemSettings(this);
  CopyParamsFrame->CopyParamAttrs = CopyParamAttrs;
}
//---------------------------------------------------------------------------
bool __fastcall TCopyParamCustomDialog::Execute(TCopyParamType & CopyParam)
{
  CopyParamsFrame->Params = CopyParam;

  CopyParamsFrame->BeforeExecute();
  bool Result = (ShowModal() == DefaultResult(this));

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
  if (ModalResult == DefaultResult(this))
  {
    ExitActiveControl(this);
  }
}
//---------------------------------------------------------------------------
void __fastcall TCopyParamCustomDialog::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
