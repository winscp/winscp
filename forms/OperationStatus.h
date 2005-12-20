//---------------------------------------------------------------------------
#ifndef OperationStatusH
#define OperationStatusH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TOperationStatusForm : public TForm
{
__published:
  TLabel *StatusLabel;
  TBevel *Bevel1;
private:
  void * FShowAsModalStorage;

  void __fastcall SetStatus(const AnsiString value);
  AnsiString __fastcall GetStatus();
public:
  __fastcall TOperationStatusForm(TComponent* Owner);
  virtual __fastcall ~TOperationStatusForm();
  void __fastcall HideAsModal();
  void __fastcall ShowAsModal();
  
  __property AnsiString Status = { read = GetStatus, write = SetStatus };
};
//---------------------------------------------------------------------------
#endif
