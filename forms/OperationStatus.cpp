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
  UseSystemFont(this);
}
//---------------------------------------------------------------------------
__fastcall TOperationStatusForm::~TOperationStatusForm()
{
  SecureShell = NULL;
    
  if (FFormState.Contains(fsModal))
  {
    HideAsModal();
  }
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
  // method duplicated in TProgressForm
  CancelDrag();
  if (GetCapture() != 0) SendMessage(GetCapture(), WM_CANCELMODE, 0, 0);
  ReleaseCapture();
  FFormState << fsModal;
  FFocusActiveWindow = GetActiveWindow();

  FFocusWindowList = DisableTaskWindows(0);
  Show();
  SendMessage(Handle, CM_ACTIVATE, 0, 0);
}
//---------------------------------------------------------------------------
void __fastcall TOperationStatusForm::HideAsModal()
{
  // method duplicated in TProgressForm
  assert(FFormState.Contains(fsModal));
  SendMessage(Handle, CM_DEACTIVATE, 0, 0);
  if (GetActiveWindow() != Handle)
  {
    FFocusActiveWindow = 0;
  }
  Hide();

  EnableTaskWindows(FFocusWindowList);

  if (FFocusActiveWindow != 0)
  {
    SetActiveWindow(FFocusActiveWindow);
  }

  FFormState >> fsModal;
}

