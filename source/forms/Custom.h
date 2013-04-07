//----------------------------------------------------------------------------
#ifndef CustomH
#define CustomH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
//----------------------------------------------------------------------------
#include <Windows.h>
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
  __fastcall TCustomDialog(UnicodeString HelpKeyword);

  TLabel * __fastcall CreateLabel(UnicodeString Label);
  void __fastcall AddEditLikeControl(TWinControl * Edit, TLabel * Label);
  void __fastcall AddEdit(TCustomEdit * Edit, TLabel * Label);
  void __fastcall AddComboBox(TCustomCombo * Combo, TLabel * Label);
  void __fastcall AddButtonControl(TButtonControl * Control);

  bool __fastcall Execute();
};
//----------------------------------------------------------------------------
#endif
