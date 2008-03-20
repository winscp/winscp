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
  TButton * OKButton;
  TButton * CancelButton;
  TComboBox * InputCombo;
  TLabel * InputLabel;
  TButton * HelpButton;
  TCheckBox *SavePasswordCheck;
  void __fastcall InputComboChange(TObject * Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall FormCloseQuery(TObject *Sender, bool &CanClose);
  void __fastcall FormShow(TObject *Sender);

public:
  virtual __fastcall TSaveSessionDialog(TComponent * AOwner, TStrings * Items,
    TSessionData * OriginalSession, bool AllowSavePassword);

  bool __fastcall Execute(AnsiString & SessionName, bool & SavePassword);

private:
  TSessionData * FOriginalSession;

  void __fastcall UpdateControls();
};
//----------------------------------------------------------------------------
#endif
