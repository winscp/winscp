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
  TLabel *Label1;
  TButton *CheckAllButton;
  TCheckBox *ImportKeysCheck;
  TButton *HelpButton;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall SessionListViewInfoTip(TObject *Sender,
    TListItem *Item, UnicodeString &InfoTip);
  void __fastcall SessionListViewMouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SessionListViewKeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
  void __fastcall CheckAllButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
private:
  TStoredSessionList *FSessionList;
  void __fastcall UpdateControls();
  void __fastcall SetSessionList(TStoredSessionList *value);
  void __fastcall LoadSessions();
  bool __fastcall GetImportKeys();
public:
  virtual __fastcall TImportSessionsDialog(TComponent* AOwner);
  __property TStoredSessionList *SessionList  = { read=FSessionList, write=SetSessionList };
  __property bool ImportKeys = { read=GetImportKeys };
};
//----------------------------------------------------------------------------
#endif
