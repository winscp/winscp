//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "OperationStatus.h"

#include <Common.h>
#include <TextsWin.h>

#include <VCLCommon.h>
//---------------------------------------------------------------------------
const int ConnectionStatusStrings[] =
  { STATUS_CLOSED, STATUS_INITWINSOCK, STATUS_LOOKUPHOST, STATUS_CONNECT,
    STATUS_AUTHENTICATE, STATUS_AUTHENTICATED, STATUS_STARTUP,
    STATUS_OPEN_DIRECTORY, STATUS_READY };
#define ConnectionStatusStringsCount (sizeof(ConnectionStatusStrings) / sizeof(int))
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
__fastcall TOperationStatusForm::TOperationStatusForm(TComponent* Owner)
        : TForm(Owner)
{
  FSecureShell = NULL;
  UseSystemSettings(this);
  FShowAsModalStorage = NULL;
}
//---------------------------------------------------------------------------
__fastcall TOperationStatusForm::~TOperationStatusForm()
{
  SecureShell = NULL;

  ReleaseAsModal(this, FShowAsModalStorage);
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::SecureShellUpdateStatus(TObject * /*Sender*/)
{
  assert(FSecureShell && (FSecureShell->Status >= 0) &&
    (FSecureShell->Status < ConnectionStatusStringsCount) && Application);
  Status = LoadStr((int)ConnectionStatusStrings[FSecureShell->Status]);
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::SetSecureShell(TSecureShell * value)
{
  if (SecureShell != value)
  {
    if (SecureShell)
    {
      if (SecureShell->OnUpdateStatus == SecureShellUpdateStatus)
      {
        SecureShell->OnUpdateStatus = FPrevOnUpdateStatus;
      }
    }
    FSecureShell = value;
    if (SecureShell)
    {
      FPrevOnUpdateStatus = SecureShell->OnUpdateStatus;
      SecureShell->OnUpdateStatus = SecureShellUpdateStatus;
      SecureShellUpdateStatus(SecureShell);
    }
  }
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

