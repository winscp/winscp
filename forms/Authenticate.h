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
  TPanel *PromptEditPanel;
  TLabel *PromptLabel1;
  TPasswordEdit *PromptEdit1;
  TPanel *BannerPanel;
  TMemo *BannerMemo;
  TCheckBox *NeverShowAgainCheck;
  TButton *BannerCloseButton;
  TButton *BannerHelpButton;
  TPanel *SavePasswordPanel;
  TCheckBox *SavePasswordCheck;
  TPanel *ButtonsPanel;
  TButton *PasswordOKButton;
  TButton *PasswordCancelButton;
  TButton *PasswordHelpButton;
  TLabel *InstructionsLabel;
  TLabel *PromptLabel2;
  TPasswordEdit *PromptEdit2;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);

public:
  __fastcall TAuthenticateForm(TComponent * Owner);
  virtual __fastcall ~TAuthenticateForm();

  void __fastcall Init(TSessionData * SessionData);
  void __fastcall ShowAsModal();
  void __fastcall HideAsModal();
  void __fastcall Log(const AnsiString Message);
  bool __fastcall PromptUser(TPromptKind Kind, AnsiString Name, AnsiString Instructions,
    TStrings * Prompts, TStrings * Results, bool ForceLog, bool StoredCredentialsTried);
  void __fastcall Banner(const AnsiString & Banner, bool & NeverShowAgain,
    int Options);

protected:
  void __fastcall ClearLog();
  void __fastcall AdjustControls();
  bool __fastcall Execute(AnsiString Status, TPanel * Panel,
    TWinControl * FocusControl, TButton * DefaultButton, TButton * CancelButton,
    bool FixHeight, bool Zoom, bool ForceLog);
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall Dispatch(void * AMessage);
  void __fastcall WMNCCreate(TWMNCCreate & Message);
  TLabel * __fastcall GenerateLabel(int Current, AnsiString Caption);
  TCustomEdit * __fastcall GenerateEdit(int Current, bool Echo, int MaxLen);
  TList * __fastcall GeneratePrompt(AnsiString Instructions, TStrings * Prompts,
    TStrings * Results);

private:
  void * FShowAsModalStorage;
  TWinControl * FFocusControl;
  TSessionData * FSessionData;
  AnsiString FStatus;
  TWinControl * FPromptParent;
  int FPromptLeft;
  int FPromptTop;
  int FPromptRight;
  int FPromptEditGap;
  int FPromptsGap;
};
//---------------------------------------------------------------------------
#endif
