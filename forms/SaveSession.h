//----------------------------------------------------------------------------
#ifndef SaveSessionH
#define SaveSessionH
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
//----------------------------------------------------------------------------
#include <SessionData.h>
//----------------------------------------------------------------------------
class TSaveSessionDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TComboBox *SessionNameBox;
  TLabel *Label1;
  void __fastcall SessionNameBoxChange(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
private:
  TStoredSessionList * FSessionList;
  void __fastcall SetSessionList(TStoredSessionList * value);
  void __fastcall SetSessionName(AnsiString value);
  AnsiString __fastcall GetSessionName();
public:
  virtual __fastcall TSaveSessionDialog(TComponent* AOwner);
  void __fastcall UpdateControls();
  __property TStoredSessionList * SessionList  = { read=FSessionList, write=SetSessionList };
  __property AnsiString SessionName  = { read=GetSessionName, write=SetSessionName };
};
//----------------------------------------------------------------------------
#endif
