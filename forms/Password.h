//----------------------------------------------------------------------------
#ifndef PasswordH
#define PasswordH
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
#include "PasswordEdit.hpp"
#include "WinInterface.h"
//----------------------------------------------------------------------------
class TPasswordDialog : public TForm
{
__published:
  TButton *OKButton;
  TButton *CancelButton;
  TPanel *PasswordPanel;
  TLabel *PasswordLabel;
  TPasswordEdit *PasswordEdit;
  TPanel *ServerPromptPanel;
  TLabel *ServerPromptLabel;
  TCheckBox *HideTypingCheck;
  void __fastcall HideTypingCheckClick(TObject *Sender);

private:
  TPasswordKind FKind;

  void __fastcall SetPasswordCaption(const AnsiString value);
  AnsiString __fastcall GetPasswordCaption();
  void __fastcall SetPassword(const AnsiString value);
  AnsiString __fastcall GetPassword();
  void __fastcall SetKind(TPasswordKind value);

public:
  virtual __fastcall TPasswordDialog(TComponent* AOwner);
  __property AnsiString PasswordCaption  = { read=GetPasswordCaption, write=SetPasswordCaption };
  __property AnsiString Password  = { read=GetPassword, write=SetPassword };
  __property TPasswordKind Kind = { read=FKind, write=SetKind };
};
//----------------------------------------------------------------------------
#endif
