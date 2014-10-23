//----------------------------------------------------------------------------
#ifndef ImportSessionsH
#define ImportSessionsH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.ComCtrls.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
//---------------------------------------------------------------------
#include <SessionData.h>
#include <Vcl.ExtCtrls.hpp>
//---------------------------------------------------------------------
class TImportSessionsDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *SessionListView2;
  TLabel *Label;
  TButton *CheckAllButton;
  TCheckBox *ImportKeysCheck;
  TButton *HelpButton;
  TComboBox *SourceComboBox;
  TPanel *ErrorPanel;
  TLabel *ErrorLabel;
  void __fastcall SessionListView2InfoTip(TObject *Sender,
    TListItem *Item, UnicodeString &InfoTip);
  void __fastcall SessionListView2MouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SessionListView2KeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall SourceComboBoxSelect(TObject *Sender);

private:
  TStrings * FErrors;
  void __fastcall UpdateControls();
  void __fastcall LoadSessions();
  void __fastcall ClearSelections();
  void __fastcall SaveSelection();
  TStoredSessionList * __fastcall GetSessionList(int Index);

public:
  virtual __fastcall TImportSessionsDialog(TComponent * AOwner);
  void __fastcall Init(TList * SessionListsList, TStrings * Errors);
  bool __fastcall Execute(bool & ImportKeys);
};
//----------------------------------------------------------------------------
#endif
