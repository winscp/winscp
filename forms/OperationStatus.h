//---------------------------------------------------------------------------
#ifndef OperationStatusH
#define OperationStatusH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>

#include <SecureShell.h>
//---------------------------------------------------------------------------
class TOperationStatusForm : public TForm
{
__published:
  TLabel *StatusLabel;
  TBevel *Bevel1;
private:
  TSecureShell * FSecureShell;
  void * FShowAsModalStorage;
  TNotifyEvent FPrevOnUpdateStatus;

  void __fastcall SetSecureShell(TSecureShell * value);
  void __fastcall SetStatus(const AnsiString value);
  AnsiString __fastcall GetStatus();
public:
  __fastcall TOperationStatusForm(TComponent* Owner);
  virtual __fastcall ~TOperationStatusForm();
  void __fastcall HideAsModal();
  void __fastcall ShowAsModal();
  
  void __fastcall SecureShellUpdateStatus(TObject * Sender);
  __property TSecureShell * SecureShell = { read = FSecureShell, write = SetSecureShell };
  __property AnsiString Status = { read = GetStatus, write = SetStatus };
};
//---------------------------------------------------------------------------
#endif
