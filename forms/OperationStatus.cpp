//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Common.h>

#include "OperationStatus.h"

#include <VCLCommon.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TOperationStatusForm::TOperationStatusForm(TComponent* Owner)
        : TForm(Owner)
{
  UseSystemSettings(this);
  FShowAsModalStorage = NULL;
  StatusLabel->Caption = "";
}
//---------------------------------------------------------------------------
__fastcall TOperationStatusForm::~TOperationStatusForm()
{
  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::SetStatus(const AnsiString value)
{
  if (StatusLabel->Caption != value)
  {
    StatusLabel->Caption = value;
    Application->ProcessMessages();
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TOperationStatusForm::GetStatus()
{
  return StatusLabel->Caption;
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::ShowAsModal()
{
  ::ShowAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::HideAsModal()
{
  ::HideAsModal(this, FShowAsModalStorage);
}

