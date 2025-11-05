//---------------------------------------------------------------------------
#ifndef SelectMaskH
#define SelectMaskH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.h>
#include <CustomDirView.hpp>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
#include <WinInterface.h>
#include <GUITools.h>
//---------------------------------------------------------------------------
class TSelectMaskDialog : public TForm
{
__published:
  TButton * OKBtn;
  TButton * CancelBtn;
  TGroupBox * MaskGroup;
  TLabel * Label3;
  THistoryComboBox * MaskEdit;
  TCheckBox * ApplyToDirectoriesCheck;
  TButton * HelpButton;
  TStaticText * HintText;
  TButton * ClearButton;
  TButton * MaskButton;
  TButton *ColorButton;
  TLabel *ColorFileNamesLabel;
  TLabel *ColorSizesLabel;
  TLabel *ColorPaddingLabel;
  void __fastcall FormCloseQuery(TObject * Sender, bool & CanClose);
  void __fastcall MaskEditExit(TObject * Sender);
  void __fastcall HelpButtonClick(TObject * Sender);
  void __fastcall ClearButtonClick(TObject * Sender);
  void __fastcall FormShow(TObject * Sender);
  void __fastcall MaskButtonClick(TObject * Sender);
  void __fastcall ColorButtonClick(TObject *Sender);
  void __fastcall MaskEditChange(TObject *Sender);
private:
  TControl * FParent;
  std::unique_ptr<TPopupMenu> FColorPopupMenu;
  TColor FColor;

  void __fastcall ColorChange(TColor Color);
  void __fastcall UpdateControls();

  INTERFACE_HOOK

public:
  enum TMode { smSelect, smDeselect, smFilter, smFileColor };
  __fastcall TSelectMaskDialog(TComponent* Owner);
  void __fastcall Init(TMode Mode, TControl * Parent);
  bool __fastcall Execute(TFileFilter & FileFilter, TColor & Color);
  bool __fastcall Execute(UnicodeString & Mask, TColor & Color);
};
//---------------------------------------------------------------------------
#endif
