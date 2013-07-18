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
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TMessageForm : public TForm
{
public:
  static TForm * __fastcall Create(const UnicodeString & Msg, TStrings * MoreMessages,
    TMsgDlgType DlgType, unsigned int Answers,
    const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
    unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & ImageName);

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
  void __fastcall ButtonDropDownClick(TObject * Sender);
  void __fastcall UpdateForShiftStateTimer(TObject * Sender);

private:
  typedef std::map<unsigned int, TButton *> TAnswerButtons;

  TLabel * Message;
  TMemo * MessageMemo;
  TShiftState FShiftState;
  TTimer * FUpdateForShiftStateTimer;

  void __fastcall HelpButtonClick(TObject * Sender);
  void __fastcall ReportButtonClick(TObject * Sender);
  void __fastcall CMDialogKey(TWMKeyDown & Message);
  void __fastcall UpdateForShiftState();
  TButton * __fastcall CreateButton(
    UnicodeString Name, UnicodeString Caption, unsigned int Answer,
    TNotifyEvent OnClick, bool IsTimeoutButton,
    int GroupWith, TShiftState GrouppedShiftState,
    TAnswerButtons & AnswerButtons, bool HasMoreMessages, int & ButtonWidth);
};
//---------------------------------------------------------------------------
__fastcall TMessageForm::TMessageForm(TComponent * AOwner) : TForm(AOwner, 0)
{
  Message = NULL;
  MessageMemo = NULL;
  FUpdateForShiftStateTimer = NULL;
  TNonClientMetrics NonClientMetrics;
  NonClientMetrics.cbSize = sizeof(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &NonClientMetrics, 0))
  {
    Font->Handle = CreateFontIndirect(&NonClientMetrics.lfMessageFont);
  }
  Position = poOwnerFormCenter;
  UseSystemSettingsPre(this);
}
//---------------------------------------------------------------------------
__fastcall TMessageForm::~TMessageForm()
{
  SAFE_DESTROY(FUpdateForShiftStateTimer);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::HelpButtonClick(TObject * /*Sender*/)
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
void __fastcall TMessageForm::ReportButtonClick(TObject * /*Sender*/)
{
  UnicodeString Url =
    FMTLOAD(ERROR_REPORT_URL,
      (EncodeUrlString(GetReportText()), Configuration->ProductVersion,
       IntToHex(__int64(GUIConfiguration->Locale), 4)));

  OpenBrowser(Url);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::UpdateForShiftState()
{

  TShiftState ShiftState =
    KeyboardStateToShiftState() *
    (TShiftState() << ssShift << ssCtrl << ssAlt);

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
            int From = 1;
            Button->Caption = CopyToChars(Item->Caption, From, L"\t", false);
            Button->ModalResult = Item->Tag & 0xFFFF;
            assert(Button->OnClick == NULL);
            assert(Item->OnClick == MenuItemClick);
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

  DividerLine = UnicodeString::StringOfChar(L'-', 27) + sLineBreak;
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
  UnicodeString MessageCaption = NormalizeNewLines(Message->Caption);
  UnicodeString Result = FORMAT(L"%s%s%s%s%s%s%s%s%s%s%s", (DividerLine, Caption, sLineBreak,
    DividerLine, MessageCaption, sLineBreak, DividerLine, MoreMessages,
    ButtonCaptions, sLineBreak, DividerLine));
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TMessageForm::GetReportText()
{
  UnicodeString Text = Message->Caption;
  if (MessageMemo != NULL)
  {
    Text += L"\n" + MessageMemo->Text;
  }
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
  else
  {
    TForm::Dispatch(Message);
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
void __fastcall TMessageForm::DoShow()
{
  UseSystemSettingsPost(this);

  TForm::DoShow();
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::MenuItemClick(TObject * Sender)
{
  TMenuItem * Item = NOT_NULL(dynamic_cast<TMenuItem *>(Sender));
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
  // it closes, but functionaly is does not matter
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
const wchar_t * IconIDs[] = { IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK,
  IDI_QUESTION, NULL };
const int mcHorzMargin = 8;
const int mcVertMargin = 8;
const int mcHorzSpacing = 10;
const int mcVertSpacing = 10;
const int mcButtonWidth = 50;
const int mcButtonHeight = 14;
const int mcButtonSpacing = 4;
const int mcMoreMessageWidth = 320;
const int mcMoreMessageHeight = 80;
//---------------------------------------------------------------------------
static UnicodeString __fastcall GetKeyNameStr(int Key)
{
  wchar_t Buf[MAX_PATH];
  LONG VirtualKey = MapVirtualKey(Key, MAPVK_VK_TO_VSC);
  VirtualKey <<= 16;
  if (GetKeyNameText(VirtualKey, Buf, LENOF(Buf)) > 0)
  {
    Buf[LENOF(Buf) - 1] = L'\0';
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
  TNotifyEvent OnClick, bool IsTimeoutButton,
  int GroupWith, TShiftState GrouppedShiftState,
  TAnswerButtons & AnswerButtons, bool HasMoreMessages, int & ButtonWidth)
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
  int CurButtonWidth = TextRect.Right - TextRect.Left + 8;

  TButton * Button = NULL;

  if (SupportsSplitButton() &&
      (GroupWith >= 0) &&
      ALWAYS_TRUE(AnswerButtons.find(GroupWith) != AnswerButtons.end()))
  {
    TButton * GroupWithButton = AnswerButtons[GroupWith];

    if (GroupWithButton->DropDownMenu == NULL)
    {
      GroupWithButton->Style = TCustomButton::bsSplitButton;
      GroupWithButton->DropDownMenu = new TPopupMenu(this);
      // cannot handle subitems with shift state,
      // if the button has its own handler
      // (though it may not be the case still here)
      assert(GroupWithButton->OnClick == NULL);

      TMenuItem * Item = new TMenuItem(GroupWithButton->DropDownMenu);
      GroupWithButton->DropDownMenu->Items->Add(Item);
      GroupWithButton->OnDropDownClick = ButtonDropDownClick;

      Item->Caption = GroupWithButton->Caption;
      Item->OnClick = MenuItemClick;
      assert(GroupWithButton->ModalResult <= 0xFFFF);
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
      assert(GrouppedShiftState == TShiftState());
    }

    Item->Caption = Caption;
    if (OnClick != NULL)
    {
      Item->OnClick = OnClick;
    }
    else
    {
      Item->OnClick = MenuItemClick;
      assert((Answer <= 0xFFFF) && (GrouppedShiftState.ToInt() <= 0xFFFF));
      Item->Tag = Answer + (GrouppedShiftState.ToInt() << 16);
    }

    // Hard-coded drop down button width (do not know how to ask for system width).
    // Also we do not update the max button width for the default groupped
    // button caption. We just blindly hope that captions of advanced commands
    // are always longer than the caption of simple default command
    CurButtonWidth += 15;
  }
  else
  {
    Button = new TButton(this);

    Button->Name = Name;
    Button->Parent = this;
    Button->Caption = Caption;

    if (OnClick != NULL)
    {
      Button->OnClick = OnClick;
    }
    else
    {
      Button->ModalResult = Answer;
    }

    if (HasMoreMessages)
    {
      Button->Anchors = TAnchors() << akBottom << akLeft;
    }
  }

  if (CurButtonWidth > ButtonWidth)
  {
    ButtonWidth = CurButtonWidth;
  }

  return Button;
}
//---------------------------------------------------------------------------
void __fastcall AnswerNameAndCaption(
  unsigned int Answer, UnicodeString & Name, UnicodeString & Caption)
{
  switch (Answer)
  {
    case qaYes:
      Caption = LoadStr(_SMsgDlgYes.Identifier);
      Name = L"Yes";
      break;

    case qaNo:
      Caption = LoadStr(_SMsgDlgNo.Identifier);
      Name = L"No";
      break;

    case qaOK:
      Caption = LoadStr(_SMsgDlgOK.Identifier);
      Name = L"OK";
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
      FAIL;
      throw Exception(L"Undefined answer");
  }
}
//---------------------------------------------------------------------------
TForm * __fastcall TMessageForm::Create(const UnicodeString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, unsigned int Answers,
  const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & ImageName)
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

  TMessageForm * Result = SafeFormCreate<TMessageForm>();

  Result->BiDiMode = Application->BiDiMode;
  Result->BorderStyle = bsDialog;
  Result->Canvas->Font = Result->Font;
  Result->KeyPreview = true;
  TPoint DialogUnits = GetAveCharSize(Result->Canvas);
  int HorzMargin = MulDiv(mcHorzMargin, DialogUnits.x, 4);
  int VertMargin = MulDiv(mcVertMargin, DialogUnits.y, 8);
  int HorzSpacing = MulDiv(mcHorzSpacing, DialogUnits.x, 4);
  int VertSpacing = MulDiv(mcVertSpacing, DialogUnits.y, 8);
  int ButtonWidth = MulDiv(mcButtonWidth, DialogUnits.x, 4);
  std::vector<TButton *> ButtonControls;
  TAnswerButtons AnswerButtons;
  for (unsigned int Answer = qaFirst; Answer <= qaLast; Answer = Answer << 1)
  {
    if (FLAGSET(Answers, Answer))
    {
      assert(Answer != mrCancel);
      UnicodeString Caption;
      UnicodeString Name;

      AnswerNameAndCaption(Answer, Name, Caption);

      TNotifyEvent OnClick = NULL;
      int GroupWith = -1;
      TShiftState GrouppedShiftState;
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
            OnClick = Aliases[i].OnClick;
            GroupWith = Aliases[i].GroupWith;
            GrouppedShiftState = Aliases[i].GrouppedShiftState;
            assert((OnClick == NULL) || (GrouppedShiftState == TShiftState()));
            break;
          }
        }
      }

      // we hope that all grouped-with buttons are for answer with greater
      // value that the answer to be grouped with
      if (GroupWith >= 0)
      {
        if (ALWAYS_FALSE(GroupWith >= static_cast<int>(Answer)) ||
            ALWAYS_FALSE(Answer == TimeoutAnswer) &&
            ALWAYS_FALSE(Answer == DefaultAnswer) &&
            ALWAYS_FALSE(Answer == CancelAnswer))
        {
          GroupWith = -1;
        }
      }

      bool IsTimeoutButton = (TimeoutButton != NULL) && (Answer == TimeoutAnswer);

      if (Answer == qaHelp)
      {
        assert(OnClick == NULL);
        OnClick = Result->HelpButtonClick;
      }

      if (Answer == qaReport)
      {
        assert(OnClick == NULL);
        OnClick = Result->ReportButtonClick;
      }

      TButton * Button = Result->CreateButton(
        Name, Caption, Answer,
        OnClick, IsTimeoutButton, GroupWith, GrouppedShiftState,
        AnswerButtons, (MoreMessages != NULL), ButtonWidth);

      if (Button != NULL)
      {
        ButtonControls.push_back(Button);

        Button->Default = (Answer == DefaultAnswer);
        Button->Cancel = (Answer == CancelAnswer);

        AnswerButtons.insert(TAnswerButtons::value_type(Answer, Button));

        if (IsTimeoutButton)
        {
          *TimeoutButton = Button;
        }
      }
    }
  }

  int ButtonHeight = MulDiv(mcButtonHeight, DialogUnits.y, 8);
  int ButtonSpacing = MulDiv(mcButtonSpacing, DialogUnits.x, 4);
  TRect TextRect;
  SetRect(&TextRect, 0, 0, Screen->Width / 2, 0);
  DrawText(Result->Canvas->Handle, Msg.c_str(), Msg.Length() + 1, &TextRect,
    DT_EXPANDTABS | DT_CALCRECT | DT_WORDBREAK |
    Result->DrawTextBiDiModeFlagsReadingOnly());
  int MaxWidth = Screen->Width - HorzMargin * 2 - 32 - HorzSpacing - 30;
  if (TextRect.right > MaxWidth)
  {
    // this will truncate the text, we should implement something smarter eventually
    TextRect.right = MaxWidth;
  }
  const wchar_t * IconID = IconIDs[DlgType];
  int IconTextWidth = TextRect.Right;
  int IconTextHeight = TextRect.Bottom;
  if ((IconID != NULL) || !ImageName.IsEmpty())
  {
    IconTextWidth += 32 + HorzSpacing;
    if (IconTextHeight < 32)
    {
      IconTextHeight = 32;
    }
  }

  if (MoreMessages != NULL)
  {
    TMemo * MessageMemo = new TMemo(Result);
    MessageMemo->Parent = Result;
    MessageMemo->ReadOnly = true;
    MessageMemo->WantReturns = False;
    MessageMemo->ScrollBars = ssVertical;
    MessageMemo->Anchors = TAnchors() << akLeft << akRight << akTop; //akBottom;
    MessageMemo->Color = clBtnFace;
    MessageMemo->Lines->Text = MoreMessages->Text;

    Result->MessageMemo = MessageMemo;
  }

  int ButtonGroupWidth = 0;
  if (!ButtonControls.empty())
  {
    ButtonGroupWidth = ButtonWidth * ButtonControls.size() +
      ButtonSpacing * (ButtonControls.size() - 1);
  }

  int MoreMessageWidth = (MoreMessages != NULL ?
    MulDiv(mcMoreMessageWidth, DialogUnits.x, 4) : 0);
  int MoreMessageHeight = (MoreMessages != NULL ?
    MulDiv(mcMoreMessageHeight, DialogUnits.y, 12) : 0);

  int AClientWidth =
    (IconTextWidth > ButtonGroupWidth ? IconTextWidth : ButtonGroupWidth) +
    HorzMargin * 2;
  Result->ClientWidth = (AClientWidth > MoreMessageWidth ?
    AClientWidth : MoreMessageWidth);
  Result->ClientHeight = IconTextHeight + ButtonHeight + VertSpacing +
    VertMargin * 2 + MoreMessageHeight;
  Result->Left = (Screen->Width / 2) - (Result->Width / 2);
  Result->Top = (Screen->Height / 2) - (Result->Height / 2);
  if (DlgType != mtCustom)
  {
    Result->Caption = LoadResourceString(Captions[DlgType]);
  }
  else
  {
    Result->Caption = Application->Title;
  }

  if ((IconID != NULL) || !ImageName.IsEmpty())
  {
    TImage * Image = new TImage(Result);
    Image->Name = L"Image";
    Image->Parent = Result;
    if (!ImageName.IsEmpty())
    {
      std::auto_ptr<TPngImage> Png(new TPngImage());
      Png->LoadFromResourceName(0, ImageName);
      Image->Picture->Assign(Png.get());
    }
    else
    {
      Image->Picture->Icon->Handle = LoadIcon(0, IconID);
    }
    Image->SetBounds(HorzMargin, VertMargin, 32, 32);
  }

  TLabel * Message = new TLabel(Result);
  Result->Message = Message;
  Message->Name = L"Message";
  Message->Parent = Result;
  Message->WordWrap = true;
  Message->Caption = Msg;
  Message->BoundsRect = TextRect;
  Message->BiDiMode = Result->BiDiMode;
  // added to show & as & for messages containing !& pattern of custom commands
  // (suppose that we actually never want to use & as accel in message text)
  Message->ShowAccelChar = false;
  int ALeft = IconTextWidth - TextRect.Right + HorzMargin;
  Message->SetBounds(ALeft, VertMargin, TextRect.Right, TextRect.Bottom);
  int ButtonTop = IconTextHeight + VertMargin + VertSpacing + MoreMessageHeight;

  if (Result->MessageMemo != NULL)
  {
    Result->MessageMemo->BoundsRect = TRect(Message->Left,
      Message->Top + Message->Height + VertSpacing,
      Result->ClientWidth - HorzMargin,
      Message->Top + Message->Height + VertSpacing + MoreMessageHeight);
    // rather hack, whole control positioning is wrong
    if (Result->MessageMemo->Top + Result->MessageMemo->Height > ButtonTop - VertSpacing)
    {
      Result->MessageMemo->Height =
        (ButtonTop - VertSpacing) - Result->MessageMemo->Top;
    }
  }

  int X = (Result->ClientWidth - ButtonGroupWidth) / 2;
  for (unsigned int i = 0; i < ButtonControls.size(); i++)
  {
    ButtonControls[i]->SetBounds(X,
      ButtonTop, ButtonWidth, ButtonHeight);
    X += ButtonWidth + ButtonSpacing;
  }

  return Result;
}
//---------------------------------------------------------------------------
TForm * __fastcall CreateMoreMessageDialog(const UnicodeString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, unsigned int Answers,
  const TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  unsigned int TimeoutAnswer, TButton ** TimeoutButton, const UnicodeString & ImageName)
{
  return TMessageForm::Create(Msg, MoreMessages, DlgType, Answers,
    Aliases, AliasesCount, TimeoutAnswer, TimeoutButton, ImageName);
}
