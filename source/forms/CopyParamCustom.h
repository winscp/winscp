//---------------------------------------------------------------------------
#ifndef CopyParamCustomH
#define CopyParamCustomH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CopyParams.h"
//---------------------------------------------------------------------------
class TCopyParamCustomDialog : public TForm
{
__published:
  TButton *OkButton;
  TButton *CancelButton;
  TCopyParamsFrame *CopyParamsFrame;
  TButton *HelpButton;
  void __fastcall FormCloseQuery(TObject * Sender, bool & CanClose);
  void __fastcall HelpButtonClick(TObject *Sender);

public:
  __fastcall TCopyParamCustomDialog(TComponent * Owner, int Options, int Dummy);

  bool __fastcall Execute(TCopyParamType & CopyParam);

protected:
  INTERFACE_HOOK
};
//---------------------------------------------------------------------------
#endif
