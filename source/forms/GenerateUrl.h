//---------------------------------------------------------------------------
#ifndef GenerateUrlH
#define GenerateUrlH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "SessionData.h"
//---------------------------------------------------------------------------
class TGenerateUrlDialog : public TForm
{
__published:
  TGroupBox *OptionsGroup;
  TGroupBox *UrlGroup;
  TMemo *UrlMemo;
  TButton *CancelBtn;
  TButton *HelpButton;
  TButton *ClipboardButton;
  TCheckBox *UserNameCheck;
  TCheckBox *PasswordCheck;
  TCheckBox *HostKeyCheck;
  TCheckBox *RemoteDirectoryCheck;
  TCheckBox *WinSCPSpecificCheck;
  TCheckBox *SaveExtensionCheck;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ClipboardButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);

private:
  TSessionData * FData;
  TStrings * FPaths;
  bool FChanging;

protected:
  void __fastcall UpdateControls();
  UnicodeString __fastcall GenerateUrl(UnicodeString Path);

public:
  __fastcall TGenerateUrlDialog(TComponent * Owner, TSessionData * Data, TStrings * Paths);
  void __fastcall Execute();
};
//---------------------------------------------------------------------------
#endif
