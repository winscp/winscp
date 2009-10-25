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
#include "PasswordEdit.hpp"
//----------------------------------------------------------------------------
class TCustomDialog : public TForm
{
__published:
  TButton * OKButton;
  TButton * CancelButton;
  TButton * HelpButton;
  void __fastcall HelpButtonClick(TObject *Sender);

private:
  int FPos;
  short FCount;

  void __fastcall Change(TObject * Sender);
  void __fastcall AddWinControl(TWinControl * Control);
  void __fastcall Changed();

protected:
  DYNAMIC void __fastcall DoShow();
  virtual bool __fastcall CloseQuery();

  virtual void __fastcall DoChange(bool & CanSubmit);
  virtual void __fastcall DoValidate();

public:
  __fastcall TCustomDialog(AnsiString HelpKeyword);

  TLabel * __fastcall CreateLabel(AnsiString Label);
  void __fastcall AddEditLikeControl(TWinControl * Edit, TLabel * Label);
  void __fastcall AddEdit(TCustomEdit * Edit, TLabel * Label);
  void __fastcall AddComboBox(TCustomCombo * Combo, TLabel * Label);
  void __fastcall AddButtonControl(TButtonControl * Control);

  bool __fastcall Execute();
};
//----------------------------------------------------------------------------
#endif
