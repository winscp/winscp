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
//----------------------------------------------------------------------------
class TPasswordDialog : public TForm
{
__published:
  TLabel *PasswordLabel;
  TButton *OKButton;
  TButton *CancelButton;
  TEdit *PasswordEdit;
private:
  void __fastcall SetPasswordCaption(AnsiString value);
  AnsiString __fastcall GetPasswordCaption();
  void __fastcall SetPassword(AnsiString value);
  AnsiString __fastcall GetPassword();
public:
  virtual __fastcall TPasswordDialog(TComponent* AOwner);
  __property AnsiString PasswordCaption  = { read=GetPasswordCaption, write=SetPasswordCaption };
  __property AnsiString Password  = { read=GetPassword, write=SetPassword };
};
//----------------------------------------------------------------------------
#endif
