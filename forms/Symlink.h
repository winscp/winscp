//---------------------------------------------------------------------------
#ifndef SymlinkH
#define SymlinkH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "CopyParam.h"
//---------------------------------------------------------------------------
class TSymlinkDialog : public TForm
{
__published:
  TGroupBox *SymlinkGroup;
  TButton *OkButton;
  TButton *CancelButton;
  TLabel *FileNameLabel;
  TEdit *FileNameEdit;
  TLabel *Label1;
  TEdit *PointToEdit;
  TCheckBox *SymbolicCheck;
  TButton *HelpButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
private:
  bool FAllowSymbolic;
  bool FEdit;
  TOperationSide FSide;
  void __fastcall SetFileName(AnsiString value);
  AnsiString __fastcall GetFileName();
  void __fastcall SetPointTo(AnsiString value);
  AnsiString __fastcall GetPointTo();
  void __fastcall SetSymbolicLink(bool value);
  bool __fastcall GetSymbolicLink();
  void __fastcall SetAllowSymbolic(bool value);
  void __fastcall SetEdit(bool value);
  void __fastcall SetSide(TOperationSide value);
public:
  bool __fastcall Execute();
  __fastcall TSymlinkDialog(TComponent* Owner);
  __property bool AllowSymbolic = { read = FAllowSymbolic, write = SetAllowSymbolic };
  __property bool Edit = { read = FEdit, write = SetEdit };
  __property AnsiString FileName = { read = GetFileName, write = SetFileName };
  __property AnsiString PointTo = { read = GetPointTo, write = SetPointTo };
  __property TOperationSide Side = { read = FSide, write = SetSide };
  __property bool SymbolicLink = { read = GetSymbolicLink, write = SetSymbolicLink };
protected:
  void __fastcall UpdateControls();
};
//---------------------------------------------------------------------------
#endif
