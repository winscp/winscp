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
//---------------------------------------------------------------------
class TImportSessionsDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *SessionListView;
  TLabel *Label;
  TButton *CheckAllButton;
  TCheckBox *ImportKeysCheck;
  TButton *HelpButton;
  TComboBox *SourceComboBox;
  void __fastcall SessionListViewInfoTip(TObject *Sender,
    TListItem *Item, UnicodeString &InfoTip);
  void __fastcall SessionListViewMouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SessionListViewKeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall SourceComboBoxSelect(TObject *Sender);

private:
  void __fastcall UpdateControls();
  void __fastcall LoadSessions();
  void __fastcall ClearSelections();
  void __fastcall SaveSelection();
  TStoredSessionList * __fastcall GetSessionList(int Index);

public:
  virtual __fastcall TImportSessionsDialog(TComponent * AOwner,
    TList * SessionListsList);
  bool __fastcall Execute(bool & ImportKeys);
};
//----------------------------------------------------------------------------
#endif
