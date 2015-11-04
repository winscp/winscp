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
  TGroupBox *ResultGroup;
  TMemo *ResultMemo;
  TButton *CancelBtn;
  TButton *HelpButton;
  TButton *ClipboardButton;
  TCheckBox *UserNameCheck;
  TCheckBox *PasswordCheck;
  TCheckBox *HostKeyCheck;
  TCheckBox *RemoteDirectoryCheck;
  TCheckBox *WinSCPSpecificCheck;
  TCheckBox *SaveExtensionCheck;
  TGroupBox *GenerateGroup;
  TRadioButton *UrlButton;
  TRadioButton *ScriptButton;
  TRadioButton *AssemblyButton;
  TGroupBox *AssemblyOptionsGroup;
  TLabel *Label1;
  TComboBox *AssemblyLanguageCombo;
  TGroupBox *ScriptOptionsGroup;
  TLabel *Label2;
  TComboBox *ScriptFormatCombo;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ClipboardButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);

private:
  TSessionData * FData;
  TStrings * FPaths;
  bool FChanging;
  int FGroupBoxPadding;

protected:
  void __fastcall UpdateControls();
  UnicodeString __fastcall GenerateUrl(UnicodeString Path);
  bool __fastcall IsFileUrl();

public:
  __fastcall TGenerateUrlDialog(TComponent * Owner, TSessionData * Data, TStrings * Paths);
  void __fastcall Execute();
};
//---------------------------------------------------------------------------
#endif
