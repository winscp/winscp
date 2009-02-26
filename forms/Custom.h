//----------------------------------------------------------------------------
#ifndef CustomH
#define CustomH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <Windows.h>
//----------------------------------------------------------------------------
class TCustomDialog : public TForm
{
__published:
  TButton * OKButton;
  TButton * CancelButton;
  TComboBox *Combo;
  TLabel *ComboLabel;
  TButton *HelpButton;
  TCheckBox *Check;
  void __fastcall Change(TObject * Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);

public:
  __fastcall TCustomDialog(TComponent * AOwner, const TDialogParams & Params);

  bool __fastcall Execute(TDialogData & Data);

private:
  TDialogControls FControls;
  const TDialogParams & FParams;

  void __fastcall DoChange();
  void __fastcall SaveData(TDialogData & Data);
};
//----------------------------------------------------------------------------
#endif
