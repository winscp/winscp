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
  TGroupBox * FGroupBox;

  void __fastcall Changed();
  int __fastcall GetMaxControlWidth(TControl * Control);
  int __fastcall GetParentClientWidth();
  void __fastcall AdjustHeight(TControl * Control);
  void __fastcall SetUpComboBox(TCustomCombo * Combo, TStrings * Items, bool OneLine);

protected:
  DYNAMIC void __fastcall DoShow();
  virtual bool __fastcall CloseQuery();

  virtual void __fastcall DoChange(bool & CanSubmit);
  virtual void __fastcall DoValidate();
  virtual void __fastcall DoHelp();
  void __fastcall Change(TObject * Sender);

public:
  __fastcall TCustomDialog(UnicodeString HelpKeyword);

  void __fastcall StartGroup(const UnicodeString & Caption);
  TLabel * __fastcall CreateLabel(UnicodeString Label);
  TCheckBox * __fastcall CreateAndAddCheckBox(const UnicodeString & Caption);
  void __fastcall AddEditLikeControl(TWinControl * Edit, TLabel * Label, bool OneLine = false);
  void __fastcall AddEdit(TCustomEdit * Edit, TLabel * Label, bool OneLine = false);
  void __fastcall AddComboBox(TCustomCombo * Combo, TLabel * Label, TStrings * Items = NULL, bool OneLine = false);
  void __fastcall AddShortCutComboBox(TComboBox * Combo, TLabel * Label, const TShortCuts & ShortCuts);
  void __fastcall AddButtonControl(TButtonControl * Control);
  void AddButtonNextToEdit(TButton * Control, TWinControl * Edit);
  void __fastcall AddImage(const UnicodeString & ImageName);
  void __fastcall AddWinControl(TWinControl * Control);
  void __fastcall AddText(TLabel * Label);
  void __fastcall AddText(TStaticText * Label);
  void __fastcall AddSeparator();

  void __fastcall AddDialogButton(TButton * Button);
  void __fastcall RemoveCancelButton();

  void __fastcall ScaleButtonControl(TButtonControl * Control);
  TWinControl * __fastcall GetDefaultParent();
  __property int HorizontalMargin = { read = FHorizontalMargin };

  bool __fastcall Execute();
};
//----------------------------------------------------------------------------
#endif
