//---------------------------------------------------------------------------
#ifndef SymlinkH
#define SymlinkH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
//---------------------------------------------------------------------------
#include "CopyParam.h"
#include "GUITools.h"
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
  TCheckBox *HardLinkCheck;
  TButton *HelpButton;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
private:
  bool FAllowHardLink;
  bool FEdit;
  TOperationSide FSide;
  void __fastcall SetFileName(UnicodeString value);
  UnicodeString __fastcall GetFileName();
  void __fastcall SetPointTo(UnicodeString value);
  UnicodeString __fastcall GetPointTo();
  void __fastcall SetSymbolicLink(bool value);
  bool __fastcall GetSymbolicLink();
  void __fastcall SetAllowHardLink(bool value);
  void __fastcall SetEdit(bool value);
  void __fastcall SetSide(TOperationSide value);
public:
  bool __fastcall Execute();
  __fastcall TSymlinkDialog(TComponent* Owner);
  __property bool AllowHardLink = { read = FAllowHardLink, write = SetAllowHardLink };
  __property bool Edit = { read = FEdit, write = SetEdit };
  __property UnicodeString FileName = { read = GetFileName, write = SetFileName };
  __property UnicodeString PointTo = { read = GetPointTo, write = SetPointTo };
  __property TOperationSide Side = { read = FSide, write = SetSide };
  __property bool SymbolicLink = { read = GetSymbolicLink, write = SetSymbolicLink };
protected:
  void __fastcall UpdateControls();

  INTERFACE_HOOK
};
//---------------------------------------------------------------------------
#endif
