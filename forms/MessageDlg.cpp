//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Consts.hpp>
#include <MoreButton.hpp>
#include <GUITools.h>

#include <Common.h>
#include <VCLCommon.h>
#include <WinInterface.h>
#include <TextsWin.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
class TMessageForm : public TForm
{
public:
  static TForm * __fastcall Create(const AnsiString & Msg, TStrings * MoreMessages,
    TMsgDlgType DlgType, TMsgDlgButtons Buttons,
    TQueryButtonAlias * Aliases, unsigned int AliasesCount,
    TMsgDlgBtn TimeoutResult, TButton ** TimeoutButton);

protected:
  __fastcall TMessageForm(TComponent * AOwner);

  DYNAMIC void __fastcall KeyDown(Word & Key, TShiftState Shift);
  AnsiString __fastcall GetFormText();
  virtual void __fastcall CreateParams(TCreateParams & Params);
  DYNAMIC void __fastcall DoShow();

private:
  TLabel * Message;
  TMemo * MessageMemo;

  void __fastcall HelpButtonClick(TObject * Sender);
};
//---------------------------------------------------------------------------
__fastcall TMessageForm::TMessageForm(TComponent * AOwner) : TForm(AOwner, 0)
{
  Message = NULL;
  MessageMemo = NULL;
  TNonClientMetrics NonClientMetrics;
  NonClientMetrics.cbSize = sizeof(NonClientMetrics);
  if (SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, &NonClientMetrics, 0))
  {
    Font->Handle = CreateFontIndirect(&NonClientMetrics.lfMessageFont);
  }
  Position = poMainFormCenter;
  UseSystemSettingsPre(this);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::HelpButtonClick(TObject * /*Sender*/)
{
  FormHelp(this);
}
//---------------------------------------------------------------------------
void __fastcall TMessageForm::KeyDown(Word & Key, TShiftState Shift)
{
  if (Shift.Contains(ssCtrl) && (Key == 'C'))
  {
    CopyToClipboard(GetFormText());
  }
}
//---------------------------------------------------------------------------
AnsiString __fastcall TMessageForm::GetFormText()
{
  AnsiString DividerLine, ButtonCaptions;

  DividerLine = AnsiString::StringOfChar('-', 27) + sLineBreak;
  for (int i = 0; i < ComponentCount - 1; i++)
  {
    if ((dynamic_cast<TButton*>(Components[i]) != NULL) &&
        (dynamic_cast<TMoreButton*>(Components[i]) == NULL))
    {
      ButtonCaptions += dynamic_cast<TButton*>(Components[i])->Caption +
        AnsiString::StringOfChar(' ', 3);
    }
  }
  ButtonCaptions = StringReplace(ButtonCaptions, "&", "",
    TReplaceFlags() << rfReplaceAll);
  AnsiString MoreMessages;
  if (MessageMemo != NULL)
  {
    MoreMessages = MessageMemo->Text + DividerLine;
  }
  AnsiString MessageCaption;
  MessageCaption = StringReplace(Message->Caption, "\r", "", TReplaceFlags() << rfReplaceAll);
  MessageCaption = StringReplace(MessageCaption, "\n", "\r\n", TReplaceFlags() << rfReplaceAll);
  AnsiString Result = FORMAT("%s%s%s%s%s%s%s%s%s%s%s", (DividerLine, Caption, sLineBreak,
    DividerLine, MessageCaption, sLineBreak, DividerLine, MoreMessages,
    ButtonCaptions, sLineBreak, DividerLine));
  return Result;
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
const ResourceString * Captions[] = { &_SMsgDlgWarning, &_SMsgDlgError, &_SMsgDlgInformation,
  &_SMsgDlgConfirm, NULL };
const char * IconIDs[] = { IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK,
  IDI_QUESTION, NULL };
const int ButtonCount = 11;
const AnsiString ButtonNames[ButtonCount] = {
  "Yes", "No", "OK", "Cancel", "Abort", "Retry", "Ignore", "All", "NoToAll",
  "YesToAll", "Help" };
const ResourceString * ButtonCaptions[ButtonCount] = {
  &_SMsgDlgYes, &_SMsgDlgNo, &_SMsgDlgOK, &_SMsgDlgCancel, &_SMsgDlgAbort,
  &_SMsgDlgRetry, &_SMsgDlgIgnore, &_SMsgDlgAll, &_SMsgDlgNoToAll, &_SMsgDlgYesToAll,
  &_SMsgDlgHelp };
extern const int ModalResults[ButtonCount] = {
  mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
  mrYesToAll, 0 };
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
TForm * __fastcall TMessageForm::Create(const AnsiString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, TMsgDlgButtons Buttons,
  TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  TMsgDlgBtn TimeoutResult, TButton ** TimeoutButton)
{
  TRect TextRect;

  TMsgDlgBtn DefaultButton, CancelButton;
  if (Buttons.Contains(mbOK))
  {
    DefaultButton = mbOK;
  }
  else if (Buttons.Contains(mbYes))
  {
    DefaultButton = mbYes;
  }
  else
  {
    DefaultButton = mbRetry;
  }

  if (Buttons.Contains(mbCancel))
  {
    CancelButton = mbCancel;
  }
  else if (Buttons.Contains(mbNo))
  {
    CancelButton = mbNo;
  }
  else if (Buttons.Contains(mbAbort))
  {
    CancelButton = mbAbort;
  }
  else
  {
    CancelButton = mbOK;
  }

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
  TButton * ButtonControls[ButtonCount + 1];
  int ButtonControlsCount = 0;
  for (unsigned int B = mbYes; B <= mbHelp; B++)
  {
    assert(B < ButtonCount);
    if (Buttons.Contains(TMsgDlgBtn(B)))
    {
      TextRect = Rect(0,0,0,0);
      AnsiString Caption = LoadResourceString(ButtonCaptions[B]);

      // temporary fix of accelerators (&Abort vs. &All/Yes to &All)
      // must be removed
      if (Caption == "&All")
      {
        Caption = "A&ll";
      }
      else if (Caption == "Yes to &All")
      {
        Caption = "Yes to A&ll";
      }

      TNotifyEvent OnClick = NULL;
      if (Aliases != NULL)
      {
        for (unsigned int i = 0; i < AliasesCount; i++)
        {
          if (B == Aliases[i].Button)
          {
            Caption = Aliases[i].Alias;
            OnClick = Aliases[i].OnClick;
            break;
          }
        }
      }

      TButton * Button = new TButton(Result);

      AnsiString MeasureCaption = Caption;
      if ((TimeoutButton != NULL) && (B == TimeoutResult))
      {
        MeasureCaption = FMTLOAD(TIMEOUT_BUTTON, (MeasureCaption, 99));
        *TimeoutButton = Button;
      }

      DrawText(Result->Canvas->Handle,
        MeasureCaption.c_str(), -1,
        &TextRect, DT_CALCRECT | DT_LEFT | DT_SINGLELINE |
        Result->DrawTextBiDiModeFlagsReadingOnly());
      int CurButtonWidth = TextRect.Right - TextRect.Left + 8;
      if (CurButtonWidth > ButtonWidth)
      {
        ButtonWidth = CurButtonWidth;
      }

      Button->Name = ButtonNames[TMsgDlgBtn(B)];
      Button->Parent = Result;
      Button->Caption = Caption;
      if (OnClick != NULL)
      {
        Button->OnClick = OnClick;
      }
      else
      {
        Button->ModalResult = ModalResults[B];
        Button->Default = (B == DefaultButton);
        Button->Cancel = (B == CancelButton);
      }
      if (MoreMessages != NULL)
      {
        Button->Anchors = TAnchors() << akBottom << akLeft;
      }
      if (B == mbHelp)
      {
        Button->OnClick = Result->HelpButtonClick;
      }

      ButtonControls[ButtonControlsCount] = Button;
      ButtonControlsCount++;
    }
  }

  int ButtonHeight = MulDiv(mcButtonHeight, DialogUnits.y, 8);
  int ButtonSpacing = MulDiv(mcButtonSpacing, DialogUnits.x, 4);
  SetRect(&TextRect, 0, 0, Screen->Width / 2, 0);
  DrawText(Result->Canvas->Handle, Msg.c_str(), Msg.Length() + 1, &TextRect,
    DT_EXPANDTABS | DT_CALCRECT | DT_WORDBREAK |
    Result->DrawTextBiDiModeFlagsReadingOnly());
  const char * IconID = IconIDs[DlgType];
  int IconTextWidth = TextRect.Right;
  int IconTextHeight = TextRect.Bottom;
  if (IconID != NULL)
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

    TMoreButton * MoreButton = new TMoreButton(Result);
    MoreButton->Parent = Result;
    MoreButton->Panel = MessageMemo;
    MoreButton->RepositionForm = true;
    MoreButton->Name = "MoreButton";

    MessageMemo->TabOrder = static_cast<short>(MoreButton->TabOrder + 1);

    ButtonControls[ButtonControlsCount] = MoreButton;
    ButtonControlsCount++;
  }

  int ButtonGroupWidth = 0;
  if (ButtonControlsCount > 0)
  {
    ButtonGroupWidth = ButtonWidth * ButtonControlsCount +
      ButtonSpacing * (ButtonControlsCount - 1);
  }

  int MoreMessageWidth = (MoreMessages != NULL ?
    MulDiv(mcMoreMessageWidth, DialogUnits.x, 4) : 0);
  int MoreMessageHeight = (MoreMessages != NULL ?
    MulDiv(mcMoreMessageHeight, DialogUnits.y, 8) : 0);

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

  if (IconID != NULL)
  {
    TImage * Image = new TImage(Result);
    Image->Name = "Image";
    Image->Parent = Result;
    Image->Picture->Icon->Handle = LoadIcon(0, IconID);
    Image->SetBounds(HorzMargin, VertMargin, 32, 32);
  }

  TLabel * Message = new TLabel(Result);
  Result->Message = Message;
  Message->Name = "Message";
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
  for (int i = 0; i < ButtonControlsCount; i++)
  {
    ButtonControls[i]->SetBounds(X,
      ButtonTop, ButtonWidth, ButtonHeight);
    X += ButtonWidth + ButtonSpacing;
  }

  return Result;
}
//---------------------------------------------------------------------------
TForm * __fastcall CreateMoreMessageDialog(const AnsiString & Msg,
  TStrings * MoreMessages, TMsgDlgType DlgType, TMsgDlgButtons Buttons,
  TQueryButtonAlias * Aliases, unsigned int AliasesCount,
  TMsgDlgBtn TimeoutResult, TButton ** TimeoutButton)
{
  return TMessageForm::Create(Msg, MoreMessages, DlgType, Buttons,
    Aliases, AliasesCount, TimeoutResult, TimeoutButton);
}
