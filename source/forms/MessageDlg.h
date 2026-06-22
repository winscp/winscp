//----------------------------------------------------------------------------
#ifndef MessageDlgH
#define MessageDlgH
//----------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
//---------------------------------------------------------------------------
class TMessageForm : public TForm
{
__published:
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

public:
  static TForm * __fastcall Create(const UnicodeString & Msg, TStrings * MoreMessages,
    TMsgDlgType DlgType, unsigned int Answers,
    const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
    unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & ImageName,
    const UnicodeString & NeverAskAgainCaption, const UnicodeString & MoreMessagesUrl,
    TSize MoreMessagesSize, const UnicodeString & CustomCaption);

  virtual int __fastcall ShowModal();
  void __fastcall InsertPanel(TPanel * Panel);
  int __fastcall GetContentWidth();
  void __fastcall NavigateToUrl(const UnicodeString & Url);

protected:
  __fastcall TMessageForm(TComponent * AOwner);
  virtual __fastcall ~TMessageForm();

  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  DYNAMIC void __fastcall KeyUp(Word & Key, TShiftState Shift);
  UnicodeString __fastcall GetFormText();
  UnicodeString __fastcall GetReportText();
  UnicodeString __fastcall NormalizeNewLines(UnicodeString Text);
  virtual void __fastcall CreateParams(TCreateParams & Params);
  DYNAMIC void __fastcall DoShow();
  virtual void __fastcall Dispatch(void * Message);
  void __fastcall MenuItemClick(TObject * Sender);
  void __fastcall ButtonSubmit(TObject * Sender);
  void __fastcall ButtonDropDownClick(TObject * Sender);
  void __fastcall UpdateForShiftStateTimer(TObject * Sender);
  DYNAMIC void __fastcall SetZOrder(bool TopMost);
  void __fastcall LoadMessageBrowser();

private:
  typedef std::map<unsigned int, TButton *> TAnswerButtons;

  UnicodeString MessageText;
  TPanel * ContentsPanel;
  TMemo * MessageMemo;
  TPanel * MessageBrowserPanel;
  TWebBrowserEx * MessageBrowser;
  UnicodeString MessageBrowserUrl;
  TShiftState FShiftState;
  TTimer * FUpdateForShiftStateTimer;
  bool FShowNoActivate;
  std::map<TObject *, TButtonSubmitEvent> FButtonSubmitEvents;
  TCheckBox * NeverAskAgainCheck;

  void __fastcall HelpButtonSubmit(TObject * Sender, unsigned int & Answer);
  void __fastcall ReportButtonSubmit(TObject * Sender, unsigned int & Answer);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall CMShowingChanged(TMessage & Message);
  void __fastcall UpdateForShiftState();
  TButton * __fastcall CreateButton(
    UnicodeString Name, UnicodeString Caption, unsigned int Answer,
    TButtonSubmitEvent OnSubmit, bool IsTimeoutButton,
    int GroupWith, TShiftState GrouppedShiftState, bool ElevationRequired, bool MenuButton,
    TAnswerButtons & AnswerButtons, bool HasMoreMessages, int & ButtonWidths);
};
//----------------------------------------------------------------------------
#endif
