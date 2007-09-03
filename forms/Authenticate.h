//---------------------------------------------------------------------------
#ifndef AuthenticateH
#define AuthenticateH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include "PasswordEdit.hpp"
#include "WinInterface.h"
//---------------------------------------------------------------------------
class TAuthenticateForm : public TForm
{
__published:
  TListView *LogView;
  TPanel *PasswordPanel;
  TPanel *PasswordEditPanel;
  TLabel *PasswordLabel;
  TPasswordEdit *PasswordEdit;
  TPanel *ServerPromptPanel;
  TLabel *ServerPromptLabel;
  TCheckBox *HideTypingCheck;
  TButton *PasswordOKButton;
  TButton *PasswordCancelButton;
  TButton *PasswordHelpButton;
  TPanel *BannerPanel;
  TMemo *BannerMemo;
  TCheckBox *NeverShowAgainCheck;
  TButton *BannerCloseButton;
  TButton *BannerHelpButton;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall HideTypingCheckClick(TObject *Sender);

public:
  __fastcall TAuthenticateForm(TComponent * Owner, AnsiString SessionName);
  virtual __fastcall ~TAuthenticateForm();

  void __fastcall ShowAsModal();
  void __fastcall HideAsModal();
  void __fastcall Log(const AnsiString Message);
  bool __fastcall PromptUser(AnsiString Caption,
    TPromptKind Kind, AnsiString &Response, bool ForceLog);
  void __fastcall Banner(const AnsiString & Banner, bool & NeverShowAgain,
    int Options);

protected:
  void __fastcall ClearLog();
  void __fastcall AdjustControls();
  void __fastcall UpdateControls();
  bool __fastcall Execute(AnsiString Status, TControl * Control,
    TWinControl * FocusControl, TButton * DefaultButton, TButton * CancelButton,
    bool FixHeight, bool Zoom, bool ForceLog);

private:
  void * FShowAsModalStorage;
  TWinControl * FFocusControl;
  AnsiString FSessionName;
  AnsiString FStatus;
  AnsiString FPasswordCaption;
  bool FHideTypingOnServerPrompt;
};
//---------------------------------------------------------------------------
#endif
