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
  TStaticText *PasswordLabel;
  TPasswordEdit *PasswordEdit;
  TPanel *ServerPromptPanel;
  TLabel *ServerPromptLabel;
  TCheckBox *HideTypingCheck;
  TButton *HelpButton;
  void __fastcall HideTypingCheckClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);

private:
  TPromptKind FKind;

  void __fastcall SetPasswordCaption(const AnsiString value);
  AnsiString __fastcall GetPasswordCaption();
  void __fastcall SetPassword(const AnsiString value);
  AnsiString __fastcall GetPassword();
  void __fastcall SetKind(TPromptKind value);
  void __fastcall ApplicationShowHint(AnsiString & HintStr,
    bool & CanShow, THintInfo & HintInfo);

public:
  virtual __fastcall TPasswordDialog(TComponent* AOwner);
  __fastcall ~TPasswordDialog();

  __property AnsiString PasswordCaption  = { read=GetPasswordCaption, write=SetPasswordCaption };
  __property AnsiString Password  = { read=GetPassword, write=SetPassword };
  __property TPromptKind Kind = { read=FKind, write=SetKind };
};
//----------------------------------------------------------------------------
#endif
