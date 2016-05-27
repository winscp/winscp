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
  int FPrePos;
  int FIndent;
  int FHorizontalMargin;
  int FControlPadding;
  short FCount;

  void __fastcall Change(TObject * Sender);
  void __fastcall Changed();
  int __fastcall GetMaxControlWidth(TControl * Control);
  void __fastcall AdjustHeight(TControl * Control);

protected:
  DYNAMIC void __fastcall DoShow();
  virtual bool __fastcall CloseQuery();

  virtual void __fastcall DoChange(bool & CanSubmit);
  virtual void __fastcall DoValidate();
  virtual void __fastcall DoHelp();

public:
  __fastcall TCustomDialog(UnicodeString HelpKeyword);

  TLabel * __fastcall CreateLabel(UnicodeString Label);
  TCheckBox * __fastcall CreateAndAddCheckBox(const UnicodeString & Caption);
  void __fastcall AddEditLikeControl(TWinControl * Edit, TLabel * Label, bool OneLine = false);
  void __fastcall AddEdit(TCustomEdit * Edit, TLabel * Label);
  void __fastcall AddComboBox(TCustomCombo * Combo, TLabel * Label, TStrings * Items = NULL, bool OneLine = false);
  void __fastcall AddButtonControl(TButtonControl * Control);
  void __fastcall AddImage(const UnicodeString & ImageName);
  void __fastcall AddWinControl(TWinControl * Control);
  void __fastcall AddText(TLabel * Label);
  void __fastcall AddText(TStaticText * Label);
  void __fastcall AddSeparator();

  void __fastcall ScaleButtonControl(TButtonControl * Control);

  bool __fastcall Execute();
};
//----------------------------------------------------------------------------
#endif
