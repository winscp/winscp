//---------------------------------------------------------------------------
#ifndef GenerateUrlH
#define GenerateUrlH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "SessionData.h"
#include <Vcl.ComCtrls.hpp>
//---------------------------------------------------------------------------
class TGenerateUrlDialog : public TForm
{
__published:
  TGroupBox *ResultGroup;
  TRichEdit *ResultMemo;
  TButton *CancelBtn;
  TButton *HelpButton;
  TButton *ClipboardButton;
  TPageControl *OptionsPageControl;
  TTabSheet *UrlSheet;
  TTabSheet *ScriptSheet;
  TTabSheet *AssemblySheet;
  TCheckBox *UserNameCheck;
  TCheckBox *HostKeyCheck;
  TCheckBox *WinSCPSpecificCheck;
  TCheckBox *SaveExtensionCheck;
  TCheckBox *RemoteDirectoryCheck;
  TCheckBox *PasswordCheck;
  TLabel *Label2;
  TComboBox *ScriptFormatCombo;
  TLabel *Label1;
  TComboBox *AssemblyLanguageCombo;
  TLabel *ScriptDescriptionLabel;
  TLabel *AssemblyDescriptionLabel;
  void __fastcall ControlChange(TObject *Sender);
  void __fastcall ClipboardButtonClick(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall WMNCCreate(TWMNCCreate & Message);

private:
  TSessionData * FData;
  TStrings * FPaths;
  bool FChanging;
  UnicodeString FPlainResult;

protected:
  void __fastcall UpdateControls();
  UnicodeString __fastcall GenerateUrl(UnicodeString Path);
  bool __fastcall IsFileUrl();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall Dispatch(void * AMessage);

public:
  __fastcall TGenerateUrlDialog(TComponent * Owner, TSessionData * Data, TStrings * Paths);
  void __fastcall Execute();
};
//---------------------------------------------------------------------------
#endif
