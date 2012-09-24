//---------------------------------------------------------------------------
#ifndef SelectMaskH
#define SelectMaskH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <HistoryComboBox.hpp>
#include <CustomDirView.hpp>

#include <WinInterface.h>
#include <Buttons.hpp>
//---------------------------------------------------------------------------
class TSelectMaskDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TGroupBox *MaskGroup;
  TLabel *Label3;
  THistoryComboBox *MaskEdit;
  TCheckBox *ApplyToDirectoriesCheck;
  TButton *HelpButton;
  TStaticText *HintText;
  TButton *ClearButton;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall MaskEditExit(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall ClearButtonClick(TObject *Sender);
private:
  TFileFilter FFileFilter;
  void __fastcall SetFileFilter(TFileFilter value);
  TFileFilter __fastcall GetFileFilter();
public:
  enum TMode { smSelect, smDeselect, smFilter };
  __fastcall TSelectMaskDialog(TComponent* Owner);
  void __fastcall Init(TMode Mode);
  bool __fastcall Execute();
  __property TFileFilter FileFilter = { read = GetFileFilter, write = SetFileFilter };
};
//---------------------------------------------------------------------------
#endif
