//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Consts.hpp>
#include <GUITools.h>

#include <Common.h>
#include <VCLCommon.h>
#include <CoreMain.h>
#include <WinInterface.h>
#include <Tools.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <Vcl.Imaging.pngimage.hpp>
#include <StrUtils.hpp>
#include <PasTools.hpp>
#include <Math.hpp>
#include <WebBrowserEx.hpp>
#include <RegularExpressions.hpp>
#include <Setup.h>
#include <WinApi.h>
#include "MessageDlg.h"
//---------------------------------------------------------------------------
#pragma resource "*.dfm"
//---------------------------------------------------------------------------
const UnicodeString MessagePanelName(L"Panel");
const UnicodeString MainMessageLabelName(L"MainMessage");
const UnicodeString MessageLabelName(L"Message");
const UnicodeString YesButtonName(L"Yes");
const UnicodeString OKButtonName(L"OK");
//---------------------------------------------------------------------------
class TMessageButton : public TButton
{
public:
  __fastcall TMessageButton(TComponent * Owner);
protected:
  virtual void __fastcall Dispatch(void * Message);
private:
  void __fastcall WMGetDlgCode(TWMGetDlgCode & Message);
};
//---------------------------------------------------------------------------
__fastcall TMessageButton::TMessageButton(TComponent * Owner) :
  TButton(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TMessageButton::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == WM_GETDLGCODE)
  {
    WMGetDlgCode(*((TWMGetDlgCode *)Message));
  }
  else
  {
    TButton::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageButton::WMGetDlgCode(TWMGetDlgCode & Message)
{
  TButton::Dispatch(&Message);
  // WORKAROUND
  // Windows default handler returns DLGC_WANTARROWS for split buttons,
  // what prevent left/right keys from being used for focusing next/previous buttons/controls.
  // Overrwide that. Though note that we need to pass the up/down keys back to button
  // to allow drop down, see TMessageForm::CMDialogKey
  Message.Result = Message.Result & ~DLGC_WANTARROWS;
}
//---------------------------------------------------------------------------
__fastcall TMessageForm::TMessageForm(TComponent * AOwner) : TForm(AOwner)
{
  FShowNoActivate = false;
  MessageMemo = NULL;
  MessageBrowserPanel = NULL;
  MessageBrowser = NULL;
  NeverAskAgainCheck = NULL;
  FUpdateForShiftStateTimer = NULL;
  UseSystemSettingsPre(this);
}
//---------------------------------------------------------------------------
__fastcall TMessageForm::~TMessageForm()
{
  SAFE_DESTROY(FUpdateForShiftStateTimer);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::HelpButtonSubmit(TObject * /*Sender*/, unsigned int & /*Answer*/)
{
  if (HelpKeyword != HELP_NONE)
  {
    FormHelp(this);
  }
  else
  {
    MessageWithNoHelp(GetReportText());
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::ReportButtonSubmit(TObject * /*Sender*/, unsigned int & /*Answer*/)
{
  // Report text goes last, as it may exceed URL parameters limit (2048) and get truncated.
  // And we need to preserve the other parameters.
  UnicodeString Url =
    FMTLOAD(ERROR_REPORT_URL2,
      (Configuration->ProductVersion, GUIConfiguration->AppliedLocaleHex,
       EncodeUrlString(GetReportText())));

  OpenBrowser(Url);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::UpdateForShiftState()
{

  TShiftState ShiftState =
    KeyboardStateToShiftState() * AllKeyShiftStates();

  if (FShiftState != ShiftState)
  {
    FShiftState = ShiftState;

    for (int ComponentIndex = 0; ComponentIndex < ComponentCount - 1; ComponentIndex++)
    {
      TButton * Button = dynamic_cast<TButton*>(Components[ComponentIndex]);
      if ((Button != NULL) && (Button->DropDownMenu != NULL))
      {
        TMenuItem * MenuItems = Button->DropDownMenu->Items;
        for (int ItemIndex = 0; ItemIndex < MenuItems->Count; ItemIndex++)
        {
          TMenuItem * Item = MenuItems->Items[ItemIndex];
          TShiftState GrouppedShiftState(Item->Tag >> 16);
          if (Item->Enabled &&
              ((ShiftState.Empty() && Item->Default) ||
               (!ShiftState.Empty() && (ShiftState == GrouppedShiftState))))
          {
            Button->Caption = CopyToChar(Item->Caption, L'\t', false);
            Button->ModalResult = Item->Tag & 0xFFFF;
            DebugAssert(Button->OnClick == NULL);
            DebugAssert(Item->OnClick == MenuItemClick);
            break;
          }
        }
      }
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::KeyUp(Word & Key, TShiftState Shift)
{

  UpdateForShiftState();

  TForm::KeyUp(Key, Shift);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::KeyDown(Word & Key, TShiftState Shift)
{
  if (Shift.Contains(ssCtrl) && (Key == L'C'))
  {
    AppLog(L"Copying message to clipboard");
    TInstantOperationVisualizer Visualizer;
    CopyToClipboard(GetFormText());
  }
  else
  {
    if (!Shift.Contains(ssCtrl))
    {
      for (int ComponentIndex = 0; ComponentIndex < ComponentCount - 1; ComponentIndex++)
      {
        TButton * Button = dynamic_cast<TButton*>(Components[ComponentIndex]);
        if ((Button != NULL) && (Button->DropDownMenu != NULL))
        {
          TMenuItem * MenuItems = Button->DropDownMenu->Items;
          for (int ItemIndex = 0; ItemIndex < MenuItems->Count; ItemIndex++)
          {
            TMenuItem * Item = MenuItems->Items[ItemIndex];
            if (IsAccel(Key, MenuItems->Items[ItemIndex]->Caption))
            {
              Item->OnClick(Item);
              Key = 0;
              break;
            }
          }
        }

        if (Key == 0)
        {
          break;
        }
      }
    }

    UpdateForShiftState();

    TForm::KeyDown(Key, Shift);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TMessageForm::NormalizeNewLines(UnicodeString Text)
{
  Text = ReplaceStr(Text, L"\r", L"");
  Text = ReplaceStr(Text, L"\n", L"\r\n");
  return Text;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TMessageForm::GetFormText()
{
  UnicodeString DividerLine, ButtonCaptions;

  DividerLine = GetDividerLine() + sLineBreak;
  for (int i = 0; i < ComponentCount - 1; i++)
  {
    if (dynamic_cast<TButton*>(Components[i]) != NULL)
    {
      ButtonCaptions += dynamic_cast<TButton*>(Components[i])->Caption +
        UnicodeString::StringOfChar(L' ', 3);
    }
  }
  ButtonCaptions = ReplaceStr(ButtonCaptions, L"&", L"");
  UnicodeString MoreMessages;
  if (MessageMemo != NULL)
  {
    MoreMessages = MessageMemo->Text + DividerLine;
  }
  else if ((MessageBrowser != NULL) && CopyTextFromBrowser(MessageBrowser, MoreMessages))
  {
    if (!EndsStr(sLineBreak, MoreMessages))
    {
      MoreMessages += sLineBreak;
    }
    MoreMessages += DividerLine;
  }
  UnicodeString MessageCaption = NormalizeNewLines(MessageText);
  UnicodeString Result = FORMAT(L"%s%s%s%s%s%s%s%s%s%s%s", (DividerLine, Caption, sLineBreak,
    DividerLine, MessageCaption, sLineBreak, DividerLine, MoreMessages,
    ButtonCaptions, sLineBreak, DividerLine));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TMessageForm::GetReportText()
{
  UnicodeString Text = MessageText;
  Text = Text.TrimRight();
  if (MessageMemo != NULL)
  {
    Text += L"\n\n" + MessageMemo->Text;
  }
  // Currently we use browser for updates box only and it has help context,
  // and does not have Report button, so we cannot get here.
  DebugAssert(MessageBrowser == NULL);
  Text = NormalizeNewLines(Text);
  UnicodeString ReportErrorText = NormalizeNewLines(FMTLOAD(REPORT_ERROR, (L"")));
  Text = ReplaceStr(Text, ReportErrorText, L"");
  Text = Trim(Text);
  return Text;
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::CMDialogKey(TWMKeyDown & Message)
{
  // this gets used in WinInterface.cpp SetTimeoutEvents
  if (OnKeyDown != NULL)
  {
    OnKeyDown(this, Message.CharCode, KeyDataToShiftState(Message.KeyData));
  }

  if (Message.CharCode == VK_MENU)
  {
    bool AnyButtonWithGrouppedCommandsWithShiftState = false;
    for (int ComponentIndex = 0; ComponentIndex < ComponentCount - 1; ComponentIndex++)
    {
      TButton * Button = dynamic_cast<TButton*>(Components[ComponentIndex]);
      if ((Button != NULL) && (Button->DropDownMenu != NULL))
      {
        // we should check if there are any commands with shift state,
        // but it's bit overkill
        AnyButtonWithGrouppedCommandsWithShiftState = true;
        break;
      }
    }

    // this is to make Alt only alter button meaning (if there is any
    // alternable button) and not popup system menu
    if (AnyButtonWithGrouppedCommandsWithShiftState)
    {
      Message.Result = 1;
      UpdateForShiftState();
    }
    else
    {
      TForm::Dispatch(&Message);
    }
  }
  else if ((Message.CharCode == VK_UP) || (Message.CharCode == VK_DOWN))
  {
    // WORKAROUND
    // noop to make up/down be passed back to button to allow drop down,
    // see TMessageButton::WMGetDlgCode
  }
  else
  {
    TForm::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
int __fastcall TMessageForm::ShowModal()
{
  if (IsApplicationMinimized())
  {
    FShowNoActivate = true;
  }

  int Result = TForm::ShowModal();

  UnhookFormActivation(this);

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::SetZOrder(bool TopMost)
{
  // WORKAROUND: If application is minimized,
  // swallow call to BringToFront() from TForm::ShowModal()
  if (FShowNoActivate && TopMost)
  {
    // noop
  }
  else
  {
    TForm::SetZOrder(TopMost);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::CMShowingChanged(TMessage & Message)
{
  if (Showing && FShowNoActivate)
  {
    ShowFormNoActivate(this);
  }
  else
  {
    TForm::Dispatch(&Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::Dispatch(void * Message)
{
  TMessage * M = reinterpret_cast<TMessage*>(Message);
  if (M->Msg == CM_DIALOGKEY)
  {
    CMDialogKey(*((TWMKeyDown *)Message));
  }
  else if (M->Msg == CM_SHOWINGCHANGED)
  {
    CMShowingChanged(*M);
  }
  else
  {
    TForm::Dispatch(Message);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::FormAfterMonitorDpiChanged(TObject *, int OldDPI, int NewDPI)
{
  DebugUsedParam2(OldDPI, NewDPI);
  if (MessageBrowser != NULL)
  {
    LoadMessageBrowser();
  }
  if (NeverAskAgainCheck != NULL)
  {
    AutoSizeCheckBox(NeverAskAgainCheck);
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::CreateParams(TCreateParams & Params)
{
  TForm::CreateParams(Params);
  if ((Screen != NULL) && (Screen->ActiveForm != NULL) &&
      Screen->ActiveForm->HandleAllocated())
  {
    Params.WndParent = Screen->ActiveForm->Handle;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::LoadMessageBrowser()
{
  NavigateToUrl(MessageBrowserUrl);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::DoShow()
{
  UseSystemSettingsPost(this);

  if (!MessageBrowserUrl.IsEmpty() &&
      // Guard against repeated calls to DoOpen()
      (MessageBrowser == NULL))
  {
    // Web Browser component does not seem to work,
    // when created before any window is shown.
    // I.e. when the message dialog is the first window (like when /update is used).
    // So we have to delay its creation until at least the dialog box is shown.
    MessageBrowser = CreateBrowserViewer(MessageBrowserPanel, LoadStr(MESSAGE_LOADING));
    MessageBrowser->SendToBack();

    LoadMessageBrowser();
  }

  // Need OnShow to be called only after MessageBrowser is created,
  // so that the implementation (InsertDonateLink) can call InsertPanelToMessageDialog
  TForm::DoShow();

}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::ButtonSubmit(TObject * Sender)
{
  unsigned int Answer = 0;
  FButtonSubmitEvents[Sender](Sender, Answer);
  if (Answer != 0)
  {
    ModalResult = Answer;
  }
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::MenuItemClick(TObject * Sender)
{
  TMenuItem * Item = DebugNotNull(dynamic_cast<TMenuItem *>(Sender));
  ModalResult = (Item->Tag & 0xFFFF);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::UpdateForShiftStateTimer(TObject * /*Sender*/)
{
  // this is needed to reflect shift state, even when we do not have a keyboard
  // focus, what happens when drop down menu is popped up
  UpdateForShiftState();
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::ButtonDropDownClick(TObject * /*Sender*/)
{
  // as optimization, do not waste time running timer, unless
  // user pops up drop down menu. we do not have a way to stop timer, once
  // it closes, but functionaly it does not matter
  if (FUpdateForShiftStateTimer == NULL)
  {
    FUpdateForShiftStateTimer = new TTimer(this);
    FUpdateForShiftStateTimer->Interval = 50;
    FUpdateForShiftStateTimer->OnTimer = UpdateForShiftStateTimer;
  }
}
//---------------------------------------------------------------------------
const ResourceString * Captions[] = { &_SMsgDlgWarning, &_SMsgDlgError, &_SMsgDlgInformation,
  &_SMsgDlgConfirm, NULL };
const wchar_t * ImageNames[] = { L"Warning", L"Error", L"Information",
  L"Help Blue", NULL };
const int mcHorzMargin = 8;
const int mcVertMargin = 13;
const int mcHorzSpacing = 12;
const int mcButtonVertMargin = 7;
const int mcButtonSpacing = 5;
 // includes mcVertMargin
const int mcMoreMessageHeight = 86;
// approximately what Windows Vista task dialogs use,
// actually they probably has fixed width
const int mcMaxDialogWidth = 340;
const int mcMinDialogWidth = 310;
const int mcMinDialogwithMoreMessagesWidth = 400;
//---------------------------------------------------------------------------
static UnicodeString __fastcall GetKeyNameStr(int Key)
{
  wchar_t Buf[MAX_PATH];
  LONG VirtualKey = MapVirtualKey(Key, MAPVK_VK_TO_VSC);
  VirtualKey <<= 16;
  if (GetKeyNameText(VirtualKey, Buf, LENOF(Buf)) > 0)
  {
    NULL_TERMINATE(Buf);
  }
  else
  {
    Buf[0] = L'\0';
  }
  return Buf;
}
//---------------------------------------------------------------------------
TButton * __fastcall TMessageForm::CreateButton(
  UnicodeString Name, UnicodeString Caption, unsigned int Answer,
  TButtonSubmitEvent OnSubmit, bool IsTimeoutButton,
  int GroupWith, TShiftState GrouppedShiftState, bool ElevationRequired, bool MenuButton,
  TAnswerButtons & AnswerButtons, bool HasMoreMessages, int & ButtonWidths)
{
  UnicodeString MeasureCaption = Caption;
  if (IsTimeoutButton)
  {
    MeasureCaption = FMTLOAD(TIMEOUT_BUTTON, (MeasureCaption, 99));
  }

  TRect TextRect;
  DrawText(Canvas->Handle,
    UnicodeString(MeasureCaption).c_str(), -1,
    &TextRect, DT_CALCRECT | DT_LEFT | DT_SINGLELINE |
    DrawTextBiDiModeFlagsReadingOnly());
  int CurButtonWidth = TextRect.Right - TextRect.Left + ScaleByTextHeightRunTime(this, 16);
  if (ElevationRequired)
  {
    // Elevation icon
    CurButtonWidth += ScaleByTextHeightRunTime(this, 16);
  }
  if (MenuButton)
  {
    CurButtonWidth += ScaleByTextHeightRunTime(this, 16);
  }

  TButton * Button = NULL;

  if ((GroupWith >= 0) &&
      DebugAlwaysTrue(AnswerButtons.find(GroupWith) != AnswerButtons.end()))
  {
    TButton * GroupWithButton = AnswerButtons[GroupWith];

    if (GroupWithButton->DropDownMenu == NULL)
    {
      GroupWithButton->Style = TCustomButton::bsSplitButton;
      GroupWithButton->DropDownMenu = new TPopupMenu(this);
      // cannot handle subitems with shift state,
      // if the button has its own handler
      // (though it may not be the case still here)
      DebugAssert(GroupWithButton->OnClick == NULL);

      TMenuItem * Item = new TMenuItem(GroupWithButton->DropDownMenu);
      GroupWithButton->DropDownMenu->Items->Add(Item);
      GroupWithButton->OnDropDownClick = ButtonDropDownClick;

      Item->Caption = GroupWithButton->Caption;
      Item->OnClick = MenuItemClick;
      DebugAssert(GroupWithButton->ModalResult <= 0xFFFF);
      Item->Tag = GroupWithButton->ModalResult;
      Item->Default = true;
    }

    TMenuItem * Item = new TMenuItem(GroupWithButton->DropDownMenu);
    GroupWithButton->DropDownMenu->Items->Add(Item);

    // See ShortCutToText in Vcl.Menus.pas
    if (GrouppedShiftState == (TShiftState() << ssAlt))
    {
      Caption = Caption + L"\t" + GetKeyNameStr(VK_MENU);
    }
    else if (GrouppedShiftState == (TShiftState() << ssCtrl))
    {
      Caption = Caption + L"\t" + GetKeyNameStr(VK_CONTROL);
    }
    else if (GrouppedShiftState == (TShiftState() << ssShift))
    {
      Caption = Caption + L"\t" + GetKeyNameStr(VK_SHIFT);
    }
    else
    {
      // do not support combined shift states yet
      DebugAssert(GrouppedShiftState == TShiftState());
    }

    Item->Caption = Caption;
    if (OnSubmit != NULL)
    {
      Item->OnClick = ButtonSubmit;
      FButtonSubmitEvents[Item] = OnSubmit;
    }
    else
    {
      Item->OnClick = MenuItemClick;
      DebugAssert((Answer <= 0xFFFF) && (GrouppedShiftState.ToInt() <= 0xFFFF));
      Item->Tag = Answer + (GrouppedShiftState.ToInt() << 16);
    }

    // Hard-coded drop down button width (do not know how to ask for system width).
    // Also we do not update the max button width for the default groupped
    // button caption. We just blindly hope that captions of advanced commands
    // are always longer than the caption of simple default command
    CurButtonWidth += ScaleByTextHeightRunTime(this, 15);

    // never shrink buttons below their default width
    if (GroupWithButton->Width < CurButtonWidth)
    {
      ButtonWidths += CurButtonWidth - GroupWithButton->Width;
      GroupWithButton->Width = CurButtonWidth;
    }

    DebugAssert(!ElevationRequired);
  }
  else
  {
    Button = new TMessageButton(this);

    Button->Name = Name;
    Button->Parent = this;
    Button->Caption = Caption;

    if (OnSubmit != NULL)
    {
      Button->OnClick = ButtonSubmit;
      FButtonSubmitEvents[Button] = OnSubmit;
    }
    else
    {
      Button->ModalResult = Answer;
    }

    if (HasMoreMessages)
    {
      Button->Anchors = TAnchors() << akBottom << akLeft;
    }

    // never shrink buttons below our default UI button with
    Button->Width = std::max(ScaleByTextHeightRunTime(this, 80), CurButtonWidth);

    Button->ElevationRequired = ElevationRequired;
    ButtonWidths += Button->Width;

    if (MenuButton)
    {
      ::MenuButton(Button);
    }
  }

  return Button;
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::InsertPanel(TPanel * Panel)
{
  if (DebugAlwaysTrue(MessageBrowser != NULL))
  {
    // we currently use this for updates message box only
    TControl * ContentsControl = static_cast<TControl *>(DebugNotNull(MessageBrowser))->Parent;

    Panel->Parent = ContentsPanel;
    Panel->Width = ContentsControl->Width;
    Panel->Left = ContentsControl->Left;
    int ContentsBottom = ContentsControl->Top + ContentsControl->Height;
    Panel->Top = ContentsBottom + ((ContentsPanel->Height - ContentsBottom) / 2);
    Height = Height + Panel->Height;
    ContentsPanel->Height = ContentsPanel->Height + Panel->Height;
    // The panel itself does not need this, as the ParentBackground (true by default)
    // has the same effect, but an eventual TStaticText on the panel
    // uses a wrong background color, if panel's ParentColor is not set.
    Panel->ParentColor = true;
  }
}
//---------------------------------------------------------------------------
int __fastcall TMessageForm::GetContentWidth()
{
  int Result = 0;
  if (DebugAlwaysTrue(MessageBrowser != NULL))
  {
    // we currently use this for updates message box only
    TControl * ContentsControl = static_cast<TControl *>(DebugNotNull(MessageBrowser))->Parent;
    Result = ContentsControl->Width;
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::NavigateToUrl(const UnicodeString & Url)
{
  if (DebugAlwaysTrue(MessageBrowser != NULL))
  {
    UnicodeString FontSizeParam = FORMAT(L"fontsize=%d", (Font->Size));
    UnicodeString FullUrl = AppendUrlParams(Url, FontSizeParam);
    NavigateBrowserToUrl(MessageBrowser, FullUrl);
  }
}
//---------------------------------------------------------------------------
void __fastcall AnswerNameAndCaption(
  unsigned int Answer, UnicodeString & Name, UnicodeString & Caption)
{
  switch (Answer)
  {
    case qaYes:
      Caption = LoadStr(_SMsgDlgYes.Identifier);
      Name = YesButtonName;
      break;

    case qaNo:
      Caption = LoadStr(_SMsgDlgNo.Identifier);
      Name = L"No";
      break;

    case qaOK:
      Caption = LoadStr(_SMsgDlgOK.Identifier);
      Name = OKButtonName;
      break;

    case qaCancel:
      Caption = LoadStr(_SMsgDlgCancel.Identifier);
      Name = L"Cancel";
      break;

    case qaAbort:
      Caption = LoadStr(_SMsgDlgAbort.Identifier);
      Name = L"Abort";
      break;

    case qaRetry:
      Caption = LoadStr(_SMsgDlgRetry.Identifier);
      Name = L"Retry";
      break;

    case qaIgnore:
      Caption = LoadStr(_SMsgDlgIgnore.Identifier);
      Name = L"Ignore";
      break;

    // Own variant to avoid accelerator conflict with "Abort" button.
    // Note that as of now, ALL_BUTTON is never actually used,
    // because qaAll is always aliased
    case qaAll:
      Caption = LoadStr(ALL_BUTTON);
      Name = L"All";
      break;

    case qaNoToAll:
      Caption = LoadStr(_SMsgDlgNoToAll.Identifier);
      Name = L"NoToAll";
      break;

    // Own variant to avoid accelerator conflict with "Abort" button.
    case qaYesToAll:
      Caption = LoadStr(YES_TO_ALL_BUTTON);
      Name = L"YesToAll";
      break;

    case qaHelp:
      Caption = LoadStr(_SMsgDlgHelp.Identifier);
      Name = L"Help";
      break;

    case qaSkip:
      Caption = LoadStr(SKIP_BUTTON);
      Name = L"Skip";
      break;

    case qaReport:
      Caption = LoadStr(REPORT_BUTTON);
      Name = L"Report";
      break;

    default:
      DebugFail();
      throw Exception(L"Undefined answer");
  }
}
//---------------------------------------------------------------------------
static int __fastcall CalculateWidthOnCanvas(UnicodeString Text, void * Arg)
{
  TCanvas * Canvas = static_cast<TCanvas *>(Arg);
  return Canvas->TextWidth(Text);
}
//---------------------------------------------------------------------------
TForm * __fastcall TMessageForm::Create(const UnicodeString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, unsigned int Answers,
  const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & AImageName,
  const UnicodeString & NeverAskAgainCaption, const UnicodeString & MoreMessagesUrl,
  TSize MoreMessagesSize, const UnicodeString & CustomCaption)
{
  unsigned int DefaultAnswer;
  if (FLAGSET(Answers, qaOK))
  {
    DefaultAnswer = qaOK;
  }
  else if (FLAGSET(Answers, qaYes))
  {
    DefaultAnswer = qaYes;
  }
  else
  {
    DefaultAnswer = qaRetry;
  }

  unsigned int CancelAnswer = ::CancelAnswer(Answers);

  if (TimeoutButton != NULL)
  {
    *TimeoutButton = NULL;
  }

  TColor MainInstructionColor;
  HFONT MainInstructionFont;
  HFONT InstructionFont;
  GetInstrutionsTheme(MainInstructionColor, MainInstructionFont, InstructionFont);

  TMessageForm * Result = SafeFormCreate<TMessageForm>();
  if (InstructionFont != 0)
  {
    Result->Font->Handle = InstructionFont;
  }
  else
  {
    Result->Font->Assign(Screen->MessageFont);
  }

  // Can be possibly nul when error occurs before configuration is created
  if (Configuration != NULL)
  {
    Configuration->Usage->Set(L"ThemeMessageFontSize", Result->Font->Size);
  }

  // make sure we consider sizes of the monitor,
  // that is set in DoFormWindowProc(CM_SHOWINGCHANGED) later.
  Forms::TMonitor * Monitor = FormMonitor(Result);

  bool HasMoreMessages = (MoreMessages != NULL) || !MoreMessagesUrl.IsEmpty();

  Result->BiDiMode = Application->BiDiMode;
  Result->BorderStyle = bsDialog;
  Result->Canvas->Font = Result->Font;
  Result->KeyPreview = true;
  int HorzMargin = ScaleByTextHeightRunTime(Result, mcHorzMargin);
  int VertMargin = ScaleByTextHeightRunTime(Result, mcVertMargin);
  int HorzSpacing = ScaleByTextHeightRunTime(Result, mcHorzSpacing);
  int ButtonVertMargin = ScaleByTextHeightRunTime(Result, mcButtonVertMargin);
  int ButtonWidths = 0;
  int ButtonHeight = -1;
  std::vector<TButton *> ButtonControls;
  TStaticText * LinkControl = NULL;
  TAnswerButtons AnswerButtons;
  for (unsigned int Answer = qaFirst; Answer <= qaLast; Answer = Answer << 1)
  {
    if (FLAGSET(Answers, Answer))
    {
      DebugAssert(Answer != mrCancel);
      UnicodeString Caption;
      UnicodeString Name;

      AnswerNameAndCaption(Answer, Name, Caption);

      TButtonSubmitEvent OnSubmit = NULL;
      int GroupWith = -1;
      TShiftState GrouppedShiftState;
      bool ElevationRequired = false;
      bool MenuButton = false;
      UnicodeString ActionAlias;
      if (Aliases != NULL)
      {
        for (unsigned int i = 0; i < AliasesCount; i++)
        {
          if (Answer == Aliases[i].Button)
          {
            if (!Aliases[i].Alias.IsEmpty())
            {
              Caption = Aliases[i].Alias;
            }
            OnSubmit = Aliases[i].OnSubmit;
            GroupWith = Aliases[i].GroupWith;
            GrouppedShiftState = Aliases[i].GrouppedShiftState;
            ElevationRequired = Aliases[i].ElevationRequired;
            MenuButton = Aliases[i].MenuButton;
            ActionAlias = Aliases[i].ActionAlias;
            DebugAssert((OnSubmit == NULL) || (GrouppedShiftState == TShiftState()));
            break;
          }
        }
      }

      // implemented for a one link only for now
      if (!ActionAlias.IsEmpty() &&
          DebugAlwaysTrue(LinkControl == NULL) &&
          DebugAlwaysTrue(OnSubmit != NULL) &&
          DebugAlwaysTrue(GroupWith < 0))
      {
        LinkControl = new TStaticText(Result);
        LinkControl->Name = Name;
        LinkControl->Caption = ActionAlias;
        LinkControl->Alignment = taRightJustify;
        LinkControl->Anchors = TAnchors() << akRight << akTop;
        LinkControl->OnClick = Result->ButtonSubmit;
        Result->FButtonSubmitEvents[LinkControl] = OnSubmit;
      }
      else
      {
        // we hope that all grouped-with buttons are for answer with greater
        // value that the answer to be grouped with
        if (GroupWith >= 0)
        {
          if (DebugAlwaysFalse(GroupWith >= static_cast<int>(Answer)) ||
              DebugAlwaysFalse(Answer == TimeoutAnswer) &&
              DebugAlwaysFalse(Answer == DefaultAnswer) &&
              DebugAlwaysFalse(Answer == CancelAnswer))
          {
            GroupWith = -1;
          }
        }

        bool IsTimeoutButton = (TimeoutButton != NULL) && (Answer == TimeoutAnswer);

        if (Answer == qaHelp)
        {
          DebugAssert(OnSubmit == NULL);
          OnSubmit = Result->HelpButtonSubmit;
        }

        if (Answer == qaReport)
        {
          DebugAssert(OnSubmit == NULL);
          OnSubmit = Result->ReportButtonSubmit;
        }

        TButton * Button = Result->CreateButton(
          Name, Caption, Answer,
          OnSubmit, IsTimeoutButton, GroupWith, GrouppedShiftState, ElevationRequired, MenuButton,
          AnswerButtons, HasMoreMessages, ButtonWidths);

        if (Button != NULL)
        {
          ButtonControls.push_back(Button);

          Button->Default = (Answer == DefaultAnswer);
          Button->Cancel = (Answer == CancelAnswer);
          if (ButtonHeight < 0)
          {
            ButtonHeight = Button->Height;
          }
          DebugAssert(ButtonHeight == Button->Height);

          AnswerButtons.insert(TAnswerButtons::value_type(Answer, Button));

          if (IsTimeoutButton)
          {
            *TimeoutButton = Button;
          }
        }
      }
    }
  }

  int NeverAskAgainWidth = 0;
  int NeverAskAgainBaseWidth = 0;
  if (!NeverAskAgainCaption.IsEmpty())
  {
    NeverAskAgainBaseWidth = CalculateCheckBoxWidth(Result, NeverAskAgainCaption);
    NeverAskAgainWidth = NeverAskAgainBaseWidth + ScaleByTextHeightRunTime(Result, 8); // even more margin
  }

  int ButtonSpacing = ScaleByTextHeightRunTime(Result, mcButtonSpacing);
  int ButtonGroupWidth = NeverAskAgainWidth;
  if (!ButtonControls.empty())
  {
    ButtonGroupWidth += ButtonWidths +
      ButtonSpacing * (ButtonControls.size() - 1);
  }

  DebugAssert((ButtonHeight > 0) && (ButtonWidths > 0));

  TPanel * Panel = CreateBlankPanel(Result);
  Result->ContentsPanel = Panel;
  Panel->Name = MessagePanelName;
  Panel->Parent = Result;
  Panel->Color = clWindow;
  Panel->ParentBackground = false;
  Panel->Anchors = TAnchors() << akLeft << akRight << akTop;
  Panel->Caption = L"";

  int IconWidth = 0;
  int IconHeight = 0;

  UnicodeString ImageName = AImageName;
  if (ImageName.IsEmpty() &&
      DebugAlwaysTrue(ImageNames[DlgType] != NULL))
  {
    ImageName = ImageNames[DlgType];
  }

  if (DebugAlwaysTrue(!ImageName.IsEmpty()))
  {
    TImage * Image = new TImage(Result);
    Image->Name = L"Image";
    Image->Parent = Panel;
    LoadDialogImage(Image, ImageName);
    Image->SetBounds(HorzMargin, VertMargin, Image->Picture->Width, Image->Picture->Height);
    IconWidth = Image->Width + HorzSpacing;
    IconHeight = Image->Height;
  }

  int MaxTextWidth = ScaleByTextHeightRunTime(Result, mcMaxDialogWidth);
  // If the message contains SHA-256 hex fingerprint (CERT_TEXT2 on TLS/SSL certificate verification dialog),
  // allow wider box to fit it
  if (TRegEx::IsMatch(Msg, L"([0-9a-fA-F]{2}[:\-]){31}[0-9a-fA-F]{2}"))
  {
    MaxTextWidth = MaxTextWidth * 3 / 2;
  }
  // if the dialog would be wide anyway (overwrite confirmation on Windows XP),
  // to fit the buttons, do not restrict the text
  if (MaxTextWidth < ButtonGroupWidth - IconWidth)
  {
    MaxTextWidth = ButtonGroupWidth - IconWidth;
  }

  UnicodeString BodyMsg = Msg;
  BodyMsg = RemoveInteractiveMsgTag(BodyMsg);
  UnicodeString MainMsg;
  if (ExtractMainInstructions(BodyMsg, MainMsg))
  {
    Result->MessageText = MainMsg + BodyMsg;
    BodyMsg = BodyMsg.TrimLeft();
  }
  else
  {
    Result->MessageText = BodyMsg;
  }

  ApplyTabs(Result->MessageText, L' ', NULL, NULL);

  // Windows XP (not sure about Vista) does not support Hair space.
  // For Windows XP, we still keep the existing hack by using hard-coded spaces
  // in resource string
  if (IsWin7())
  {
    // Have to be padding with spaces (the smallest space defined, hair space = 1px),
    // as tabs actually do not tab, just expand to 8 spaces.
    // Otherwise we would have to do custom drawing
    // (using GetTabbedTextExtent and TabbedTextOut)
    const wchar_t HairSpace = L'\x200A';
    ApplyTabs(BodyMsg, HairSpace, CalculateWidthOnCanvas, Result->Canvas);
  }

  DebugAssert(MainMsg.Pos(L"\t") == 0);

  int IconTextWidth = -1;
  int IconTextHeight = 0;
  int ALeft = IconWidth + HorzMargin;

  for (int MessageIndex = 0; MessageIndex <= 1; MessageIndex++)
  {
    UnicodeString LabelMsg;
    UnicodeString LabelName;
    TColor LabelColor = Graphics::clNone;
    HFONT LabelFont = 0;
    switch (MessageIndex)
    {
      case 0:
        LabelMsg = MainMsg;
        LabelName = MainMessageLabelName;
        LabelColor = MainInstructionColor;
        LabelFont = MainInstructionFont;
        break;

      case 1:
        LabelMsg = BodyMsg;
        LabelName = MessageLabelName;
        break;

      default:
        DebugFail();
        break;
    }

    if (!LabelMsg.IsEmpty())
    {
      TLabel * Message = new TLabel(Panel);
      Message->Parent = Panel;
      Message->Name = LabelName;
      Message->WordWrap = true;
      Message->Caption = LabelMsg;
      Message->BiDiMode = Result->BiDiMode;
      // added to show & as & for messages containing !& pattern of custom commands
      // (suppose that we actually never want to use & as accel in message text)
      Message->ShowAccelChar = false;
      if (LabelFont != 0)
      {
        Message->Font->Handle = LabelFont;
        if (DebugAlwaysTrue(LabelFont == MainInstructionFont) &&
             // When showing an early error message
            (Configuration != NULL))
        {
          Configuration->Usage->Set(L"ThemeMainInstructionFontSize", Message->Font->Size);
        }
      }
      if (LabelColor != Graphics::clNone)
      {
        Message->Font->Color = LabelColor;
      }

      TRect TextRect;
      SetRect(&TextRect, 0, 0, MaxTextWidth, 0);
      DrawText(Message->Canvas->Handle, LabelMsg.c_str(), LabelMsg.Length() + 1, &TextRect,
        DT_EXPANDTABS | DT_CALCRECT | DT_WORDBREAK | DT_NOPREFIX |
        Result->DrawTextBiDiModeFlagsReadingOnly());
      int MaxWidth = Monitor->Width - HorzMargin * 2 - IconWidth - 30;
      // 5% buffer for potential WM_DPICHANGED, as after re-scaling the text can otherwise narrowly not fit in.
      // Though note that the buffer is lost on the first re-scale due to the AutoSize
      TextRect.right = MulDiv(TextRect.right, 105, 100);
      // this will truncate the text, we should implement something smarter eventually
      TextRect.right = Min(TextRect.right, MaxWidth);

      IconTextWidth = Max(IconTextWidth, IconWidth + TextRect.Right);

      if (IconTextHeight > 0)
      {
        IconTextHeight += VertMargin;
      }
      Message->SetBounds(ALeft, VertMargin + IconTextHeight, TextRect.Right, TextRect.Bottom);
      IconTextHeight += TextRect.Bottom;
    }
  }

  if (LinkControl != NULL)
  {
    LinkControl->Parent = Panel;
    LinkActionLabel(LinkControl);
    LinkControl->Left = Panel->ClientWidth - HorzMargin - LinkControl->Width;
    LinkControl->Top = VertMargin + IconTextHeight + VertMargin;
    IconTextHeight += VertMargin + LinkControl->Height;
  }

  DebugAssert((IconTextWidth > 0) && (IconTextHeight > 0));

  IconTextHeight = Max(IconTextHeight, IconHeight);

  int MoreMessageHeight =
    (HasMoreMessages ?
      ScaleByTextHeightRunTime(Result, (MoreMessagesSize.Height > 0 ?  MoreMessagesSize.Height : mcMoreMessageHeight)) : 0);

  Panel->SetBounds(0, 0, Result->ClientWidth, VertMargin + IconTextHeight + VertMargin + MoreMessageHeight);

  TControl * MoreMessagesControl = NULL;
  if (HasMoreMessages)
  {
    if (MoreMessages != NULL)
    {
      DebugAssert(MoreMessagesUrl.IsEmpty());

      TMemo * MessageMemo = new TMemo(Panel);
      MoreMessagesControl = MessageMemo;
      MessageMemo->Name = L"MessageMemo";
      MessageMemo->Parent = Panel;
      MessageMemo->ReadOnly = true;
      MessageMemo->WantReturns = False;
      MessageMemo->ScrollBars = ssVertical;
      MessageMemo->Anchors = TAnchors() << akLeft << akRight << akTop;
      MessageMemo->Lines->Text = MoreMessages->Text;

      Result->MessageMemo = MessageMemo;
    }
    else if (DebugAlwaysTrue(!MoreMessagesUrl.IsEmpty()))
    {
      TPanel * MessageBrowserPanel = CreateBlankPanel(Panel);
      MessageBrowserPanel->Parent = Panel;
      MessageBrowserPanel->Anchors = TAnchors() << akLeft << akRight << akTop;
      MessageBrowserPanel->BevelKind = bkTile; // flat border
      Result->MessageBrowserPanel = MessageBrowserPanel;

      MoreMessagesControl = Result->MessageBrowserPanel;

      Result->MessageBrowserUrl =  CampaignUrl(MoreMessagesUrl);
    }
  }

  int MinClientWidth =
    ScaleByTextHeightRunTime(Result,
      HasMoreMessages ? (MoreMessagesSize.Width > 0 ? MoreMessagesSize.Width : mcMinDialogwithMoreMessagesWidth) : mcMinDialogWidth);
  int AClientWidth =
    Max(
      (IconTextWidth > ButtonGroupWidth ? IconTextWidth : ButtonGroupWidth) +
        HorzMargin * 2,
      MinClientWidth);

  Result->ClientWidth = AClientWidth;
  Result->ClientHeight =
    Panel->Height + ButtonVertMargin + ButtonHeight + ButtonVertMargin;
  if (!CustomCaption.IsEmpty())
  {
    Result->Caption = CustomCaption;
  }
  else if (DebugAlwaysTrue(DlgType != mtCustom))
  {
    Result->Caption = LoadResourceString(Captions[DlgType]);
  }
  else
  {
    Result->Caption = Application->Title;
  }

  if (MoreMessagesControl != NULL)
  {
    MoreMessagesControl->SetBounds(
      ALeft,
      Panel->Height - MoreMessageHeight,
      Result->ClientWidth - ALeft - HorzMargin,
      MoreMessageHeight - VertMargin);
  }

  int ButtonTop = Panel->Height + ButtonVertMargin;
  int X = Result->ClientWidth - ButtonGroupWidth + NeverAskAgainWidth - HorzMargin;
  for (unsigned int i = 0; i < ButtonControls.size(); i++)
  {
    ButtonControls[i]->SetBounds(
      X, ButtonTop, ButtonControls[i]->Width, ButtonControls[i]->Height);
    X += ButtonControls[i]->Width + ButtonSpacing;
  }

  if (!NeverAskAgainCaption.IsEmpty() &&
      !ButtonControls.empty())
  {
    Result->NeverAskAgainCheck = new TCheckBox(Result);
    Result->NeverAskAgainCheck->Name = L"NeverAskAgainCheck";
    Result->NeverAskAgainCheck->Parent = Result;
    Result->NeverAskAgainCheck->Caption = NeverAskAgainCaption;
    // Previously we set anchor to akBottom, but as we do not do that for buttons, we removed that.
    // When showing window on 100% DPI monitor, with system DPI 100%, but main monitor 150%,
    // the title bar seems to start on 150% DPI reducing later, leaving the form client height
    // sligtly higher than needed and the checkbox being aligned differently than the button.
    // Removing the akBottom aligning improves this sligtly, while the main problem still should be fixed.

    TButton * FirstButton = ButtonControls[0];
    int NeverAskAgainHeight = ScaleByTextHeightRunTime(Result, Result->NeverAskAgainCheck->Height);
    int NeverAskAgainTop = FirstButton->Top + ((FirstButton->Height - NeverAskAgainHeight) / 2);
    int NeverAskAgainLeft = HorzMargin + ScaleByTextHeightRunTime(Result, 2); // checkbox indentation

    Result->NeverAskAgainCheck->SetBounds(
      NeverAskAgainLeft, NeverAskAgainTop, NeverAskAgainBaseWidth, NeverAskAgainHeight);
  }

  return Result;
}
//---------------------------------------------------------------------------
TForm * __fastcall CreateMoreMessageDialog(const UnicodeString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, unsigned int Answers,
  const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & ImageName,
  const UnicodeString & NeverAskAgainCaption, const UnicodeString & MoreMessagesUrl,
  TSize MoreMessagesSize, const UnicodeString & CustomCaption)
{
  return TMessageForm::Create(Msg, MoreMessages, DlgType, Answers,
    Aliases, AliasesCount, TimeoutAnswer, TimeoutButton, ImageName,
    NeverAskAgainCaption, MoreMessagesUrl, MoreMessagesSize, CustomCaption);
}
//---------------------------------------------------------------------------
void __fastcall InsertPanelToMessageDialog(TCustomForm * Form, TPanel * Panel)
{
  TMessageForm * MessageForm = DebugNotNull(dynamic_cast<TMessageForm *>(Form));
  MessageForm->InsertPanel(Panel);
}
//---------------------------------------------------------------------------
void __fastcall NavigateMessageDialogToUrl(TCustomForm * Form, const UnicodeString & Url)
{
  TMessageForm * MessageForm = DebugNotNull(dynamic_cast<TMessageForm *>(Form));
  MessageForm->NavigateToUrl(Url);
}
//---------------------------------------------------------------------------
int __fastcall GetMessageDialogContentWidth(TCustomForm * Form)
{
  TMessageForm * MessageForm = DebugNotNull(dynamic_cast<TMessageForm *>(Form));
  return MessageForm->GetContentWidth();
}
