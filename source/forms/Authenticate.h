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
#include "GUITools.h"
#include <System.Actions.hpp>
#include <Vcl.ActnList.hpp>
#include <Vcl.Menus.hpp>
#include <Vcl.StdActns.hpp>
//---------------------------------------------------------------------------
class TAuthenticateForm : public TForm
{
__published:
  TListBox *LogView;
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
  TPanel *SessionRememberPasswordPanel;
  TCheckBox *SessionRememberPasswordCheck;
  TPanel *TopPanel;
  TPanel *LeftPanel;
  TPaintBox *AnimationPaintBox;
  TActionList *ActionList;
  TEditCopy *EditCopyAction;
  TEditSelectAll *EditSelectAllAction;
  TAction *BannerMonospacedFontAction;
  TPopupMenu *BannerPopupMenu;
  TMenuItem *CopyItem;
  TMenuItem *SelectAllItem;
  TMenuItem *N1;
  TMenuItem *AdjustWindowItem;
  TPopupMenu *LabelPopupMenu;
  TMenuItem *N2;
  TAction *LabelCopyAction;
  TAction *LabelOpenLinkAction2;
  TMenuItem *Open1;
  TMenuItem *Copy1;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall HelpButtonClick(TObject *Sender);
  void __fastcall LogViewMeasureItem(TWinControl *Control, int Index, int &Height);
  void __fastcall LogViewDrawItem(TWinControl *Control, int Index, TRect &Rect, TOwnerDrawState State);
  void __fastcall FormResize(TObject *Sender);
  void __fastcall BannerMemoContextPopup(TObject *Sender, TPoint &MousePos, bool &Handled);
  void __fastcall BannerMonospacedFontActionExecute(TObject *Sender);
  void __fastcall LogViewMouseMove(TObject *Sender, TShiftState Shift, int X, int Y);
  void __fastcall LabelCopyActionExecute(TObject *Sender);
  void __fastcall LabelOpenLinkAction2Execute(TObject *Sender);
  void __fastcall FormAfterMonitorDpiChanged(TObject *Sender, int OldDPI, int NewDPI);

public:
  __fastcall TAuthenticateForm(TComponent * Owner);
  virtual __fastcall ~TAuthenticateForm();

  void __fastcall Init(TTerminal * Terminal);
  void __fastcall ShowAsModal();
  void __fastcall Log(const UnicodeString & Message, const UnicodeString & Additional = UnicodeString());
  bool __fastcall PromptUser(TPromptKind Kind, UnicodeString Name, UnicodeString Instructions,
    TStrings * Prompts, TStrings * Results, bool ForceLog, bool StoredCredentialsTried);
  void __fastcall Banner(const UnicodeString & Banner, bool & NeverShowAgain,
    int Options, unsigned int & Params);

  __property TTerminal * Terminal = { read = FTerminal };
  __property TNotifyEvent OnCancel = { read = FOnCancel, write = FOnCancel };

protected:
  void __fastcall AdjustControls();
  bool __fastcall Execute(UnicodeString Status, TPanel * Panel,
    TWinControl * FocusControl, TButton * DefaultButton, TButton * CancelButton,
    bool FixHeight, bool Zoom, bool ForceLog);
  virtual void __fastcall CreateParams(TCreateParams & Params);
  virtual void __fastcall Dispatch(void * AMessage);
  void __fastcall WMNCCreate(TWMNCCreate & Message);
  TLabel * __fastcall GenerateLabel(int Current, UnicodeString Caption);
  TCustomEdit * __fastcall GenerateEdit(int Current, bool Echo);
  TList * __fastcall GeneratePrompt(TPromptKind Kind, const UnicodeString & Instructions, TStrings * Prompts);
  void __fastcall DoCancel();
  void __fastcall AdjustLogView();
  void __fastcall MakeLogItemVisible(int Index);
  int __fastcall LogItemHeight(int Index);
  void __fastcall RedrawLog();
  void __fastcall CMShowingChanged(TMessage & Message);
  void __fastcall UpdateBannerFont();
  void __fastcall DoAdjustWindow();
  void __fastcall LabelContextPopup(TObject* Sender, const TPoint & MousePos, bool & Handled);
  bool ExtractUrl(const UnicodeString & Text, UnicodeString & Url);
  void ExternalLabel(TLabel * Label);
  void __fastcall LinkClick(TObject * Sender);
  void LabelOpen(TLabel * Label);

private:
  void * FShowAsModalStorage;
  TWinControl * FFocusControl;
  TSessionData * FSessionData;
  TTerminal * FTerminal;
  UnicodeString FStatus;
  TWinControl * FPromptParent;
  int FPromptLeft;
  int FPromptTop;
  int FPromptRight;
  int FPromptEditGap;
  int FPromptsGap;
  TNotifyEvent FOnCancel;
  TFrameAnimation FFrameAnimation;
  bool FAnimationPainted;
  int FHorizontalLogPadding;
  int FVerticalLogPadding;
  TTextFormat FLogTextFormat;
  bool FShowNoActivate;
  std::vector<UnicodeString> FHints;
  int FHintIndex;
  TLabel * FContextLabel;

  INTERFACE_HOOK;
};
//---------------------------------------------------------------------------
#endif
