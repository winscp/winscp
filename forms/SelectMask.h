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
#include "XPGroupBox.hpp"
//---------------------------------------------------------------------------
class TSelectMaskDialog : public TForm
{
__published:
  TButton *OKBtn;
  TButton *CancelBtn;
  TXPGroupBox *MaskGroup;
  TLabel *Label1;
  THistoryComboBox *MaskEdit;
  TCheckBox *IncludingDirectoriesCheck;
  TLabel *Label2;
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall MaskEditExit(TObject *Sender);
private:
  TFileFilter FFileFilter;
  bool FSelect;
  void __fastcall SetFileFilter(TFileFilter value);
  TFileFilter __fastcall GetFileFilter();
  void __fastcall SetSelect(bool value);
public:
  bool __fastcall Execute();
  __fastcall TSelectMaskDialog(TComponent* Owner);
  __property TFileFilter FileFilter = { read = GetFileFilter, write = SetFileFilter };
  __property bool Select = { read = FSelect, write = SetSelect };
};
//---------------------------------------------------------------------------
#endif
