//----------------------------------------------------------------------------
#ifndef ImportSessionsH
#define ImportSessionsH
//----------------------------------------------------------------------------
#include <vcl\System.hpp>
#include <vcl\Windows.hpp>
#include <vcl\SysUtils.hpp>
#include <vcl\Classes.hpp>
#include <vcl\Graphics.hpp>
#include <vcl\StdCtrls.hpp>
#include <vcl\Forms.hpp>
#include <vcl\Controls.hpp>
#include <vcl\Buttons.hpp>
#include <vcl\ExtCtrls.hpp>
#include <ComCtrls.hpp>

#include <SessionData.h>
//---------------------------------------------------------------------
class TImportSessionsDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TListView *SessionListView;
  TLabel *Label1;
  void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
  void __fastcall SessionListViewInfoTip(TObject *Sender,
    TListItem *Item, AnsiString &InfoTip);
  void __fastcall SessionListViewMouseDown(TObject *Sender,
    TMouseButton Button, TShiftState Shift, int X, int Y);
  void __fastcall SessionListViewKeyUp(TObject *Sender, WORD &Key,
    TShiftState Shift);
  void __fastcall FormShow(TObject *Sender);
private:
  TStoredSessionList *FSessionList;
  void __fastcall UpdateControls();
  void __fastcall SetSessionList(TStoredSessionList *value);
  void LoadSessions();
public:
  virtual __fastcall TImportSessionsDialog(TComponent* AOwner);
  __property TStoredSessionList *SessionList  = { read=FSessionList, write=SetSessionList };
};
//----------------------------------------------------------------------------
#endif
