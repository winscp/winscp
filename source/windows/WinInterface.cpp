//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <shlwapi.h>

#include <Common.h>
#include <Exceptions.h>
#include <CoreMain.h>
#include <TextsCore.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <Interface.h>
#include <VCLCommon.h>

#include "WinInterface.h"
#include "CustomWinConfiguration.h"
#include "GUITools.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
#define WM_TRAY_ICON (WM_WINSCP_USER + 5)
//---------------------------------------------------------------------
TNotifyEvent GlobalOnMinimize = NULL;
//---------------------------------------------------------------------
void __fastcall FormHelp(TForm * Form)
{
  InvokeHelp(Form->ActiveControl != NULL ? Form->ActiveControl : Form);
}
//---------------------------------------------------------------------------
TMessageParams::TMessageParams(unsigned int AParams)
{
  Reset();
  Params = AParams;
}
//---------------------------------------------------------------------------
TMessageParams::TMessageParams(const TQueryParams * AParams)
{
  Reset();

  if (AParams != NULL)
  {
    Aliases = AParams->Aliases;
    AliasesCount = AParams->AliasesCount;
    Timer = AParams->Timer;
    TimerEvent = AParams->TimerEvent;
    TimerMessage = AParams->TimerMessage;
    TimerAnswers = AParams->TimerAnswers;
    Timeout = AParams->Timeout;
    TimeoutAnswer = AParams->TimeoutAnswer;

    if (FLAGSET(AParams->Params, qpNeverAskAgainCheck))
    {
      Params |= mpNeverAskAgainCheck;
    }
    if (FLAGSET(AParams->Params, qpAllowContinueOnError))
    {
      Params |= mpAllowContinueOnError;
    }
  }
}
//---------------------------------------------------------------------------
inline void TMessageParams::Reset()
{
  Params = 0;
  Aliases = NULL;
  AliasesCount = 0;
  Timer = 0;
  TimerEvent = NULL;
  TimerMessage = L"";
  TimerAnswers = 0;
  Timeout = 0;
  TimeoutAnswer = 0;
  NewerAskAgainTitle = L"";
  NewerAskAgainAnswer = 0;
  NewerAskAgainCheckedInitially = false;
  AllowHelp = true;
  ImageName = L"";
}
//---------------------------------------------------------------------------
inline bool MapButton(unsigned int Answer, TMsgDlgBtn & Button)
{
  bool Result = true;
  #define MAP_BUTTON(TYPE) if (Answer == qa ## TYPE) Button = mb ## TYPE; else
  MAP_BUTTON(Yes)
  MAP_BUTTON(No)
  MAP_BUTTON(OK)
  MAP_BUTTON(Cancel)
  MAP_BUTTON(Abort)
  MAP_BUTTON(Retry)
  MAP_BUTTON(Ignore)
  MAP_BUTTON(All)
  MAP_BUTTON(NoToAll)
  MAP_BUTTON(YesToAll)
  MAP_BUTTON(Help)
  #undef MAP_BUTTON
  if (Answer == qaSkip) Button = mbIgnore;
    else
  Result = false;

  return Result;
}
//---------------------------------------------------------------------------
static void __fastcall NeverAskAgainCheckClick(void * /*Data*/, TObject * Sender)
{
  TCheckBox * CheckBox = dynamic_cast<TCheckBox *>(Sender);
  assert(CheckBox != NULL);
  TForm * Dialog = dynamic_cast<TForm *>(CheckBox->Owner);
  assert(Dialog != NULL);

  TModalResult PositiveAnswer = mrNone;

  if (CheckBox->Checked)
  {
    if (CheckBox->Tag > 0)
    {
      PositiveAnswer = CheckBox->Tag - 1;
    }
    else
    {
      TModalResult PositiveAnswers[] = { mrYes, mrOk, mrYesToAll };
      for (size_t i = 0; i < LENOF(PositiveAnswers); i++)
      {
        for (int ii = 0; ii < Dialog->ControlCount; ii++)
        {
          TButton * Button = dynamic_cast<TButton *>(Dialog->Controls[ii]);
          if (Button != NULL)
          {
            if (Button->ModalResult == PositiveAnswers[i])
            {
              PositiveAnswer = PositiveAnswers[i];
              break;
            }
          }
        }

        if (PositiveAnswer != mrNone)
        {
          break;
        }
      }
    }

    assert(PositiveAnswer != mrNone);
  }

  for (int ii = 0; ii < Dialog->ControlCount; ii++)
  {
    TButton * Button = dynamic_cast<TButton *>(Dialog->Controls[ii]);
    if ((Button != NULL) && (Button->ModalResult != mrNone) &&
        (Button->ModalResult != mrCancel))
    {
      Button->Enabled = !CheckBox->Checked || (Button->ModalResult == PositiveAnswer);
    }
  }
}
//---------------------------------------------------------------------------
unsigned int MapResult(unsigned int Result, unsigned int Answers)
{
  int Answer;

  switch (Result)
  {
    #define MAP_RESULT(RESULT) case mr ## RESULT: Answer = qa ## RESULT; break;
    MAP_RESULT(Abort);
    MAP_RESULT(All);
    MAP_RESULT(NoToAll);
    MAP_RESULT(YesToAll);
    MAP_RESULT(Yes);
    MAP_RESULT(No);
    MAP_RESULT(Retry);
    #undef MAP_RESULT

    case mrCancel:
      // mrCancel is returned always when X button is pressed, despite
      // no Cancel button was on the dialog. Find valid "cancel" answer.
      Answer = CancelAnswer(Answers);
      break;

    case mrOk:
      Answer = qaOK;
      break;

    case mrIgnore:
      Answer = (Answers & qaSkip) ? qaSkip : qaIgnore;
      break;

    default:
      assert(false);
      Answer = CancelAnswer(Answers);
      break;
  }

  return Answer;
}
//---------------------------------------------------------------------------
TForm * __fastcall CreateMessageDialogEx(const UnicodeString Msg,
  TStrings * MoreMessages, TQueryType Type, unsigned int Answers, UnicodeString HelpKeyword,
  const TMessageParams * Params, TButton *& TimeoutButton)
{
  TMsgDlgBtn TimeoutResult = mbHelp;
  TMsgDlgButtons Buttons;
  TMsgDlgType DlgType;

  switch (Type) {
    case qtConfirmation: DlgType = mtConfirmation; break;
    case qtInformation: DlgType = mtInformation; break;
    case qtError: DlgType = mtError; break;
    case qtWarning: DlgType = mtWarning; break;
    default: assert(false);
  }

  int AAnswers = Answers;
  unsigned int Answer = 0x01;
  while (AAnswers > 0)
  {
    if ((AAnswers & Answer) != 0)
    {
      TMsgDlgBtn Button;
      if (MapButton(Answer, Button))
      {
        Buttons << Button;
        if ((Params != NULL) && (Params->Timeout > 0) &&
            (Params->TimeoutAnswer == Answer))
        {
          TimeoutResult = Button;
        }
      }
      AAnswers &= ~Answer;
    }
    Answer <<= 1;
  }

  assert(!Buttons.Empty());

  if ((Params == NULL) || Params->AllowHelp)
  {
    Buttons << mbHelp;
  }

  if ((MoreMessages != NULL) && (MoreMessages->Count == 0))
  {
    MoreMessages = NULL;
  }

  UnicodeString ImageName;
  if (Params != NULL)
  {
    ImageName = Params->ImageName;
  }

  TForm * Dialog;

  if ((Params == NULL) || (Params->Aliases == NULL))
  {
    Dialog = CreateMoreMessageDialog(Msg, MoreMessages, DlgType, Buttons,
      NULL, 0, TimeoutResult, &TimeoutButton, ImageName);
  }
  else
  {
    TQueryButtonAlias * Aliases = new TQueryButtonAlias[Params->AliasesCount];
    try
    {
      for (unsigned int i = 0; i < Params->AliasesCount; i++)
      {
        TMsgDlgBtn Button;
        CHECK(MapButton(Params->Aliases[i].Button, Button));
        Aliases[i].Button = Button;
        Aliases[i].Alias = Params->Aliases[i].Alias;
        Aliases[i].OnClick = Params->Aliases[i].OnClick;
      }
      Dialog = CreateMoreMessageDialog(Msg, MoreMessages, DlgType, Buttons,
        Aliases, Params->AliasesCount, TimeoutResult, &TimeoutButton, ImageName);
    }
    __finally
    {
      delete[] Aliases;
    }
  }

  try
  {
    if (Answers & qaSkip)
    {
      TButton * IgnoreButton = dynamic_cast<TButton *>(Dialog->FindComponent("Ignore"));
      assert(IgnoreButton);
      IgnoreButton->Caption = LoadStr(SKIP_BUTTON);
    }

    if ((Params != NULL) && (Params->Params & mpNeverAskAgainCheck))
    {
      static const int VertSpace = 20;
      Dialog->ClientHeight = Dialog->ClientHeight + VertSpace;

      for (int Index = 0; Index < Dialog->ControlCount; Index++)
      {
        TControl * Control = Dialog->Controls[Index];
        if (Control->Anchors.Contains(akBottom))
        {
          if (Control->Anchors.Contains(akTop))
          {
            Control->Height = Control->Height - VertSpace;
          }
          else
          {
            Control->Top = Control->Top - VertSpace;
          }
        }
      }

      TCheckBox * NeverAskAgainCheck = new TCheckBox(Dialog);
      NeverAskAgainCheck->Name = L"NeverAskAgainCheck";
      NeverAskAgainCheck->Parent = Dialog;
      NeverAskAgainCheck->BoundsRect =  TRect(60, Dialog->ClientHeight - 27,
        Dialog->ClientWidth - 10, Dialog->ClientHeight - 5);
      NeverAskAgainCheck->Caption =
        !Params->NewerAskAgainTitle.IsEmpty() ?
          (UnicodeString)Params->NewerAskAgainTitle :
          // qaOK | qaIgnore is used, when custom "non-answer" button is required
          LoadStr(((Answers == qaOK) || (Answers == (qaOK | qaIgnore))) ?
            NEVER_SHOW_AGAIN : NEVER_ASK_AGAIN);
      NeverAskAgainCheck->Checked = Params->NewerAskAgainCheckedInitially;
      NeverAskAgainCheck->Anchors = TAnchors() << akBottom << akLeft;
      if (Params->NewerAskAgainAnswer > 0)
      {
        TMsgDlgBtn Button;
        if (MapButton(Params->NewerAskAgainAnswer, Button))
        {
          extern const int ModalResults[];
          NeverAskAgainCheck->Tag = ModalResults[Button] + 1;
        }
      }
      TNotifyEvent OnClick;
      ((TMethod*)&OnClick)->Code = NeverAskAgainCheckClick;
      NeverAskAgainCheck->OnClick = OnClick;
    }

    Dialog->HelpKeyword = HelpKeyword;
    ResetSystemSettings(Dialog);
  }
  catch(...)
  {
    delete Dialog;
    throw;
  }
  return Dialog;
}
//---------------------------------------------------------------------------
unsigned int __fastcall ExecuteMessageDialog(TForm * Dialog, unsigned int Answers, const TMessageParams * Params)
{
  FlashOnBackground();
  unsigned int Answer = MapResult(Dialog->ShowModal(), Answers);

  if ((Params != NULL) && (Params->Params & mpNeverAskAgainCheck))
  {
    TCheckBox * NeverAskAgainCheck =
      dynamic_cast<TCheckBox *>(Dialog->FindComponent(L"NeverAskAgainCheck"));
    assert(NeverAskAgainCheck);

    if (NeverAskAgainCheck->Checked)
    {
      bool PossitiveAnswer =
        (Params->NewerAskAgainAnswer > 0) ?
          (Answer == Params->NewerAskAgainAnswer) :
          (Answer == qaYes || Answer == qaOK || Answer == qaYesToAll);
      if (PossitiveAnswer)
      {
        Answer = qaNeverAskAgain;
      }
    }
  }

  return Answer;
}
//---------------------------------------------------------------------------
class TMessageTimer : public TTimer
{
public:
  TQueryParamsTimerEvent Event;
  unsigned int Result;
  TForm * Dialog;

  __fastcall TMessageTimer(TComponent * AOwner);

protected:
  void __fastcall DoTimer(TObject * Sender);
};
//---------------------------------------------------------------------------
__fastcall TMessageTimer::TMessageTimer(TComponent * AOwner) : TTimer(AOwner)
{
  Result = 0;
  Event = NULL;
  OnTimer = DoTimer;
  Dialog = NULL;
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimer::DoTimer(TObject * /*Sender*/)
{
  if (Event != NULL)
  {
    Event(Result);
    if (Result != 0)
    {
      Dialog->ModalResult = mrCancel;
    }
  }
}
//---------------------------------------------------------------------------
class TMessageTimeout : public TTimer
{
public:
  __fastcall TMessageTimeout(TComponent * AOwner, unsigned int Timeout,
    TButton * Button);

  void __fastcall Reset();
  void __fastcall Cancel();

protected:
  unsigned int FOrigTimeout;
  unsigned int FTimeout;
  TButton * FButton;
  UnicodeString FOrigCaption;

  void __fastcall DoTimer(TObject * Sender);
  void __fastcall UpdateButton();
};
//---------------------------------------------------------------------------
__fastcall TMessageTimeout::TMessageTimeout(TComponent * AOwner,
  unsigned int Timeout, TButton * Button) :
  TTimer(AOwner), FOrigTimeout(Timeout), FTimeout(Timeout), FButton(Button)
{
  OnTimer = DoTimer;
  Interval = MSecsPerSec;
  FOrigCaption = FButton->Caption;
  UpdateButton();
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::Reset()
{
  FTimeout = FOrigTimeout;
  UpdateButton();
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::Cancel()
{
  Enabled = false;
  UpdateButton();
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::UpdateButton()
{
  assert(FButton != NULL);
  FButton->Caption =
    !Enabled ? FOrigCaption : FMTLOAD(TIMEOUT_BUTTON, (FOrigCaption, int(FTimeout / MSecsPerSec)));
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::DoTimer(TObject * /*Sender*/)
{
  if (FTimeout <= MSecsPerSec)
  {
    assert(FButton != NULL);
    TForm * Dialog = dynamic_cast<TForm *>(FButton->Parent);
    assert(Dialog != NULL);

    Dialog->ModalResult = FButton->ModalResult;
  }
  else
  {
    FTimeout -= MSecsPerSec;
    UpdateButton();
  }
}
//---------------------------------------------------------------------
class TPublicControl : public TControl
{
friend void __fastcall MenuPopup(TObject * Sender, const TPoint & MousePos, bool & Handled);
friend void __fastcall SetTimeoutEvents(TControl * Control, TMessageTimeout * Timeout);
};
//---------------------------------------------------------------------
class TPublicWinControl : public TWinControl
{
friend void __fastcall SetTimeoutEvents(TControl * Control, TMessageTimeout * Timeout);
};
//---------------------------------------------------------------------------
static void __fastcall MessageDialogMouseMove(void * Data, TObject * /*Sender*/,
  TShiftState /*Shift*/, int /*X*/, int /*Y*/)
{
  assert(Data != NULL);
  TMessageTimeout * Timeout = static_cast<TMessageTimeout *>(Data);
  Timeout->Reset();
}
//---------------------------------------------------------------------------
static void __fastcall MessageDialogMouseDown(void * Data, TObject * /*Sender*/,
  TMouseButton /*Button*/, TShiftState /*Shift*/, int /*X*/, int /*Y*/)
{
  assert(Data != NULL);
  TMessageTimeout * Timeout = static_cast<TMessageTimeout *>(Data);
  Timeout->Cancel();
}
//---------------------------------------------------------------------------
static void __fastcall MessageDialogKeyDownUp(void * Data, TObject * /*Sender*/,
  Word & /*Key*/, TShiftState /*Shift*/)
{
  assert(Data != NULL);
  TMessageTimeout * Timeout = static_cast<TMessageTimeout *>(Data);
  Timeout->Cancel();
}
//---------------------------------------------------------------------------
void __fastcall SetTimeoutEvents(TControl * Control, TMessageTimeout * Timeout)
{
  TPublicControl * PublicControl = reinterpret_cast<TPublicControl *>(Control);
  assert(PublicControl->OnMouseMove == NULL);
  PublicControl->OnMouseMove = MakeMethod<TMouseMoveEvent>(Timeout, MessageDialogMouseMove);
  assert(PublicControl->OnMouseDown == NULL);
  PublicControl->OnMouseDown = MakeMethod<TMouseEvent>(Timeout, MessageDialogMouseDown);

  TWinControl * WinControl = dynamic_cast<TWinControl *>(Control);
  if (WinControl != NULL)
  {
    TPublicWinControl * PublicWinControl = reinterpret_cast<TPublicWinControl *>(Control);
    assert(PublicWinControl->OnKeyDown == NULL);
    PublicWinControl->OnKeyDown = MakeMethod<TKeyEvent>(Timeout, MessageDialogKeyDownUp);
    assert(PublicWinControl->OnKeyUp == NULL);
    PublicWinControl->OnKeyUp = MakeMethod<TKeyEvent>(Timeout, MessageDialogKeyDownUp);

    for (int Index = 0; Index < WinControl->ControlCount; Index++)
    {
      SetTimeoutEvents(WinControl->Controls[Index], Timeout);
    }
  }
}
//---------------------------------------------------------------------------
unsigned int __fastcall MoreMessageDialog(const UnicodeString Message, TStrings * MoreMessages,
  TQueryType Type, unsigned int Answers, UnicodeString HelpKeyword, const TMessageParams * Params)
{
  unsigned int Result;
  TForm * Dialog = NULL;
  TMessageTimer * Timer = NULL;
  TMessageTimeout * Timeout = NULL;
  try
  {
    UnicodeString AMessage = Message;

    if ((Params != NULL) && (Params->Timer > 0))
    {
      Timer = new TMessageTimer(Application);
      Timer->Interval = Params->Timer;
      Timer->Event = Params->TimerEvent;
      if (Params->TimerAnswers > 0)
      {
        Answers = Params->TimerAnswers;
      }
      if (!Params->TimerMessage.IsEmpty())
      {
        AMessage = Params->TimerMessage;
      }
    }

    TButton * TimeoutButton = NULL;
    Dialog = CreateMessageDialogEx(AMessage, MoreMessages, Type, Answers,
      HelpKeyword, Params, TimeoutButton);

    if (Timer != NULL)
    {
      Timer->Dialog = Dialog;
    }

    if (Params != NULL)
    {
      if (Params->Timeout > 0)
      {
        Timeout = new TMessageTimeout(Application, Params->Timeout, TimeoutButton);
        SetTimeoutEvents(Dialog, Timeout);
      }
    }

    Result = ExecuteMessageDialog(Dialog, Answers, Params);

    if ((Timer != NULL) && (Timer->Result != 0))
    {
      Result = Timer->Result;
    }
  }
  __finally
  {
    delete Dialog;
    delete Timer;
    delete Timeout;
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall MessageDialog(const UnicodeString Msg, TQueryType Type,
  unsigned int Answers, UnicodeString HelpKeyword, const TMessageParams * Params)
{
  return MoreMessageDialog(Msg, NULL, Type, Answers, HelpKeyword, Params);
}
//---------------------------------------------------------------------------
unsigned int __fastcall SimpleErrorDialog(const UnicodeString Msg, const UnicodeString MoreMessages)
{
  unsigned int Result;
  TStrings * More = NULL;
  try
  {
    if (!MoreMessages.IsEmpty())
    {
      More = new TStringList();
      More->Text = MoreMessages;
    }
    Result = MoreMessageDialog(Msg, More, qtError, qaOK, HELP_NONE);
  }
  __finally
  {
    delete More;
  }
  return Result;
}
//---------------------------------------------------------------------------
unsigned int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const UnicodeString MessageFormat, unsigned int Answers, UnicodeString HelpKeyword,
  const TMessageParams * Params)
{
  TStrings * MoreMessages = NULL;
  ExtException * EE = dynamic_cast<ExtException *>(E);
  if (EE != NULL)
  {
    MoreMessages = EE->MoreMessages;
    if (!EE->HelpKeyword.IsEmpty())
    {
      // we have to yet decide what to do now
      assert(HelpKeyword.IsEmpty());
      HelpKeyword = EE->HelpKeyword;
    }
  }
  UnicodeString Message;
  // this is always called from within ExceptionMessage check,
  // so it should never fail here
  CHECK(ExceptionMessage(E, Message));

  return MoreMessageDialog(
    FORMAT(MessageFormat.IsEmpty() ? UnicodeString(L"%s") : MessageFormat, (Message)),
    MoreMessages, Type, Answers, HelpKeyword, Params);
}
//---------------------------------------------------------------------------
unsigned int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  int SessionReopenTimeout, const UnicodeString MessageFormat, unsigned int Answers,
  UnicodeString HelpKeyword, const TMessageParams * Params)
{
  assert(FLAGCLEAR(Answers, qaRetry));
  Answers |= qaRetry;

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(RECONNECT_BUTTON);

  TMessageParams AParams;
  if (Params != NULL)
  {
    AParams = *Params;
  }
  assert(AParams.Timeout == 0);
  // the condition is de facto excess
  if (SessionReopenTimeout > 0)
  {
    AParams.Timeout = SessionReopenTimeout;
    AParams.TimeoutAnswer = qaRetry;
  }
  assert(AParams.Aliases == NULL);
  AParams.Aliases = Aliases;
  AParams.AliasesCount = LENOF(Aliases);

  return ExceptionMessageDialog(E, Type, MessageFormat, Answers, HelpKeyword, &AParams);
}
//---------------------------------------------------------------------------
void __fastcall Busy(bool Start)
{
  static int Busy = 0;
  static TCursor PrevCursor;
  if (Start)
  {
    if (!Busy)
    {
      PrevCursor = Screen->Cursor;
      Screen->Cursor = crHourGlass;
    }
    Busy++;
    assert(Busy < 10);
  }
  else
  {
    assert(Busy > 0);
    Busy--;
    if (!Busy)
    {
      Screen->Cursor = PrevCursor;
    }
  }
}
//---------------------------------------------------------------------------
static void __fastcall CopyParamListBoolToggleClick(TMenuItem * Item)
{
  bool * BoolToggle = reinterpret_cast<bool *>(Item->Tag);
  *BoolToggle = !*BoolToggle;
  Item->Checked = *BoolToggle;
}
//---------------------------------------------------------------------------
bool __fastcall DoRemoteMoveDialog(UnicodeString & Target, UnicodeString & FileMask)
{
  UnicodeString Value = UnixIncludeTrailingBackslash(Target) + FileMask;
  TStrings * History = CustomWinConfiguration->History[L"RemoteTarget"];
  bool Result = InputDialog(
    LoadStr(REMOTE_MOVE_TITLE), LoadStr(REMOTE_TRANSFER_PROMPT),
    Value, HELP_REMOTE_MOVE, History, true);
  if (Result)
  {
    CustomWinConfiguration->History[L"RemoteTarget"] = History;
    Target = UnixExtractFilePath(Value);
    FileMask = UnixExtractFileName(Value);
  }
  return Result;
}
//---------------------------------------------------------------------------
const int cpiDefault = -1;
const int cpiConfigure = -2;
const int cpiCustom = -3;
const int cpiSaveSettings = -4;
//---------------------------------------------------------------------------
void __fastcall CopyParamListPopup(TPoint P, TPopupMenu * Menu,
  const TCopyParamType & Param, UnicodeString Preset, TNotifyEvent OnClick,
  int Options, int CopyParamAttrs, bool SaveSettings)
{
  Menu->Items->Clear();

  TMenuItem * CustomizeItem = NULL;
  TMenuItem * Item;

  if (FLAGSET(Options, cplCustomize))
  {
    Item = new TMenuItem(Menu);
    Item->Caption = LoadStr(COPY_PARAM_CUSTOM);
    Item->Tag = cpiCustom;
    Item->Default = FLAGSET(Options, cplCustomizeDefault);
    Item->OnClick = OnClick;
    Menu->Items->Add(Item);
    CustomizeItem = Item;
  }

  if (FLAGSET(Options, cplSaveSettings))
  {
    Item = new TMenuItem(Menu);
    Item->Caption = LoadStr(COPY_PARAM_SAVE_SETTINGS);
    Item->Tag = cpiSaveSettings;
    Item->Checked = SaveSettings;
    Item->OnClick = OnClick;
    Menu->Items->Add(Item);
  }

  Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_PRESET_HEADER);
  Item->Visible = false;
  Item->Enabled = false;
  Menu->Items->Add(Item);

  bool AnyChecked = false;
  Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_DEFAULT);
  Item->Tag = cpiDefault;
  Item->Checked =
    Preset.IsEmpty() && (GUIConfiguration->CopyParamPreset[L""] == Param);
  AnyChecked = AnyChecked || Item->Checked;
  Item->OnClick = OnClick;
  Menu->Items->Add(Item);

  TCopyParamType DefaultParam;
  const TCopyParamList * CopyParamList = GUIConfiguration->CopyParamList;
  for (int i = 0; i < CopyParamList->Count; i++)
  {
    UnicodeString Name = CopyParamList->Names[i];
    TCopyParamType AParam = GUIConfiguration->CopyParamPreset[Name];
    if (AParam.AnyUsableCopyParam(CopyParamAttrs) ||
        // This makes "Binary" preset visible,
        // as long as we care about transfer mode
        ((AParam == DefaultParam) &&
         FLAGCLEAR(CopyParamAttrs, cpaIncludeMaskOnly) &&
         FLAGCLEAR(CopyParamAttrs, cpaNoTransferMode)))
    {
      Item = new TMenuItem(Menu);
      Item->Caption = Name;
      Item->Tag = i;
      Item->Checked =
        (Preset == Name) && (AParam == Param);
      AnyChecked = AnyChecked || Item->Checked;
      Item->OnClick = OnClick;
      Menu->Items->Add(Item);
    }
  }

  if (CustomizeItem != NULL)
  {
    CustomizeItem->Checked = !AnyChecked;
  }

  Item = new TMenuItem(Menu);
  Item->Caption = L"-";
  Menu->Items->Add(Item);

  Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_CONFIGURE);
  Item->Tag = cpiConfigure;
  Item->OnClick = OnClick;
  Menu->Items->Add(Item);

  MenuPopup(Menu, P, NULL);
}
//---------------------------------------------------------------------------
bool __fastcall CopyParamListPopupClick(TObject * Sender,
  TCopyParamType & Param, UnicodeString & Preset, int CopyParamAttrs,
  bool * SaveSettings)
{
  TComponent * Item = dynamic_cast<TComponent *>(Sender);
  assert(Item != NULL);
  assert((Item->Tag >= cpiSaveSettings) && (Item->Tag < GUIConfiguration->CopyParamList->Count));

  bool Result;
  if (Item->Tag == cpiConfigure)
  {
    bool MatchedPreset = (GUIConfiguration->CopyParamPreset[Preset] == Param);
    DoPreferencesDialog(pmPresets);
    Result = (MatchedPreset && GUIConfiguration->HasCopyParamPreset[Preset]);
    if (Result)
    {
      Param = GUIConfiguration->CopyParamPreset[Preset];
    }
  }
  else if (Item->Tag == cpiCustom)
  {
    Result = DoCopyParamCustomDialog(Param, CopyParamAttrs);
  }
  else if (Item->Tag == cpiSaveSettings)
  {
    if (ALWAYS_TRUE(SaveSettings != NULL))
    {
      *SaveSettings = !*SaveSettings;
    }
    Result = false;
  }
  else
  {
    Preset = (Item->Tag >= 0) ?
      GUIConfiguration->CopyParamList->Names[Item->Tag] : UnicodeString();
    Param = GUIConfiguration->CopyParamPreset[Preset];
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
TWinInteractiveCustomCommand::TWinInteractiveCustomCommand(
  TCustomCommand * ChildCustomCommand, const UnicodeString CustomCommandName) :
  TInteractiveCustomCommand(ChildCustomCommand)
{
  FCustomCommandName = StripHotkey(CustomCommandName);
}
//---------------------------------------------------------------------------
void __fastcall TWinInteractiveCustomCommand::Prompt(int /*Index*/,
  const UnicodeString & Prompt, UnicodeString & Value)
{
  UnicodeString APrompt = Prompt;
  if (APrompt.IsEmpty())
  {
    APrompt = FMTLOAD(CUSTOM_COMMANDS_PARAM_PROMPT, (FCustomCommandName));
  }
  TStrings * History = CustomWinConfiguration->History[L"CustomCommandParam"];
  if (InputDialog(FMTLOAD(CUSTOM_COMMANDS_PARAM_TITLE, (FCustomCommandName)),
        APrompt, Value, HELP_CUSTOM_COMMAND_PARAM, History))
  {
    CustomWinConfiguration->History[L"CustomCommandParam"] = History;
  }
  else
  {
    Abort();
  }
}
//---------------------------------------------------------------------------
static unsigned int __fastcall ShellDllVersion()
{
  static unsigned int Result = 0;

  if (Result == 0)
  {
    Result = 4;
    HINSTANCE ShellDll = LoadLibrary(L"shell32.dll");
    if (ShellDll != NULL)
    {
      try
      {
        DLLGETVERSIONPROC DllGetVersion =
          (DLLGETVERSIONPROC)GetProcAddress(ShellDll, "DllGetVersion");
        if(DllGetVersion != NULL)
        {
          DLLVERSIONINFO VersionInfo;
          ZeroMemory(&VersionInfo, sizeof(VersionInfo));
          VersionInfo.cbSize = sizeof(VersionInfo);
          if (SUCCEEDED(DllGetVersion(&VersionInfo)))
          {
            Result = VersionInfo.dwMajorVersion;
          }
        }
      }
      __finally
      {
        FreeLibrary(ShellDll);
      }
    }
  }

  return Result;
}
//---------------------------------------------------------------------------
void __fastcall MenuPopup(TPopupMenu * Menu, TButtonControl * Button)
{
  TPoint Point = Button->ClientToScreen(TPoint(2, Button->Height));
  MenuPopup(Menu, Point, Button);
}
//---------------------------------------------------------------------------
void __fastcall MenuPopup(TObject * Sender, const TPoint & MousePos, bool & Handled)
{
  TControl * Control = dynamic_cast<TControl *>(Sender);
  assert(Control != NULL);
  TPoint Point;
  if ((MousePos.x == -1) && (MousePos.y == -1))
  {
    Point = Control->ClientToScreen(TPoint(0, 0));
  }
  else
  {
    Point = Control->ClientToScreen(MousePos);
  }
  TPopupMenu * PopupMenu = (reinterpret_cast<TPublicControl *>(Control))->PopupMenu;
  assert(PopupMenu != NULL);
  MenuPopup(PopupMenu, Point, Control);
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall SetGlobalMinimizeHandler(TCustomForm * /*Form*/, TNotifyEvent OnMinimize)
{
  if (GlobalOnMinimize == NULL)
  {
    GlobalOnMinimize = OnMinimize;
  }
}
//---------------------------------------------------------------------------
void __fastcall ClearGlobalMinimizeHandler(TNotifyEvent OnMinimize)
{
  if (GlobalOnMinimize == OnMinimize)
  {
    GlobalOnMinimize = NULL;
  }
}
//---------------------------------------------------------------------------
void __fastcall CallGlobalMinimizeHandler(TObject * Sender)
{
  if (ALWAYS_TRUE(GlobalOnMinimize != NULL))
  {
    GlobalOnMinimize(Sender);
  }
}
//---------------------------------------------------------------------------
bool __fastcall IsApplicationMinimized()
{
  // VCL help recommends handling Application->OnMinimize/OnRestore
  // for tracking state, but OnRestore is actually not called
  // (OnMinimize is), when app is minimized from e.g. Progress window
  bool AppMinimized = IsIconic(Application->Handle);
  bool MainFormMinimized = IsIconic(Application->MainFormHandle);
  return AppMinimized || MainFormMinimized;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TranslateDateFormat(UnicodeString FormatStr)
{
  UnicodeString Result;
  CALTYPE CalendarType = StrToIntDef(GetLocaleStr(GetDefaultLCID(), LOCALE_ICALENDARTYPE, L"1"), 1);
  if ((CalendarType != CAL_JAPAN) && (CalendarType != CAL_TAIWAN) && (CalendarType != CAL_KOREA))
  {
    bool RemoveEra =
      (SysLocale.PriLangID == LANG_JAPANESE) ||
      (SysLocale.PriLangID == LANG_CHINESE) ||
      (SysLocale.PriLangID == LANG_KOREAN);
    if (RemoveEra)
    {
      int I = 1;
      while (I <= FormatStr.Length())
      {
        if ((FormatStr[I] != L'g') && (FormatStr[I] != L'G'))
        {
          Result += FormatStr[I];
        }
        I++;
      }
    }
    else
    {
      Result = FormatStr;
    }
  }
  else
  {
    int I = 1;
    while (I <= FormatStr.Length())
    {
      if (FormatStr.IsLeadSurrogate(I))
      {
        int L = CharLength(FormatStr, I);
        Result += FormatStr.SubString(I, L);
        I += L;
      }
      else
      {
        if (StrLIComp(FormatStr.c_str() + I - 1, L"gg", 2) == 0)
        {
          Result += L"ggg";
          I++;
        }
        else if (StrLIComp(FormatStr.c_str() + I - 1, L"yyyy", 4) == 0)
        {
          Result += L"eeee";
          I += 4 - 1;
        }
        else if (StrLIComp(FormatStr.c_str() + I - 1, L"yy", 2) == 0)
        {
          Result += L"ee";
          I += 2 - 1;
        }
        else if ((FormatStr[I] == L'y') || (FormatStr[I] == L'Y'))
        {
          Result += L"e";
        }
        else
        {
          Result += FormatStr[I];
        }
        I++;
      }
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall GetFormatSettingsFix()
{
  // todo InitSysLocale
  // todo GetMonthDayNames
  // todo GetEraNamesAndYearOffsets
  LCID DefaultLCID = GetDefaultLCID();
  FormatSettings = TFormatSettings::Create(DefaultLCID);
#pragma warn -8111
  CurrencyString = GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, L"");
  CurrencyFormat = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, L"0"), 0);
  NegCurrFormat = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, L"0"), 0);
  ThousandSeparator = GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, L',');
  DecimalSeparator = GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, L'.');
  CurrencyDecimals = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, L"0"), 0);
  DateSeparator = GetLocaleChar(DefaultLCID, LOCALE_SDATE, L'/');
  ShortDateFormat = TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, L"m/d/yy"));
  LongDateFormat = TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, L"mmmm d, yyyy"));
  TimeSeparator = GetLocaleChar(DefaultLCID, LOCALE_STIME, L':');
  TimeAMString = GetLocaleStr(DefaultLCID, LOCALE_S1159, L"am");
  TimePMString = GetLocaleStr(DefaultLCID, LOCALE_S2359, L"pm");
  UnicodeString HourFormat;
  if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, L"0"), 0) == 0)
  {
    HourFormat = L"h";
  }
  else
  {
    HourFormat = L"hh";
  }
  UnicodeString TimePrefix, TimePostfix;
  if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, L"0"), 0) == 0)
  {
    if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, L"0"), 0) == 0)
    {
      TimePostfix = L" AMPM";
    }
    else
    {
      TimePrefix = L"AMPM ";
    }
  }
  ShortTimeFormat = TimePrefix + HourFormat + L":mm" + TimePostfix;
  LongTimeFormat = TimePrefix + HourFormat + L":mm:ss" + TimePostfix;
  ListSeparator = GetLocaleChar(DefaultLCID, LOCALE_SLIST, L',');
#pragma warn .8111
}
//---------------------------------------------------------------------------
void __fastcall WinInitialize()
{
  if (IsWin7())
  {
    GetFormatSettingsFix();
  }

#pragma warn -8111
#pragma warn .8111

}
//---------------------------------------------------------------------------
struct TNotifyIconData5
{
  DWORD cbSize;
  HWND hWnd;
  UINT uID;
  UINT uFlags;
  UINT uCallbackMessage;
  HICON hIcon;
  TCHAR szTip[128];
  DWORD dwState;
  DWORD dwStateMask;
  TCHAR szInfo[256];
  union {
    UINT uTimeout;
    UINT uVersion;
  } DUMMYUNIONNAME;
  TCHAR szInfoTitle[64];
  DWORD dwInfoFlags;
};
//---------------------------------------------------------------------------
#undef NOTIFYICONDATA_V1_SIZE
#define NOTIFYICONDATA_V1_SIZE FIELD_OFFSET(TNotifyIconData5, szTip[64])
//---------------------------------------------------------------------------
__fastcall ::TTrayIcon::TTrayIcon(unsigned int Id)
{
  FVisible = false;
  FOnClick = NULL;
  FOnBalloonClick = NULL;

  FTrayIcon = new TNotifyIconData5;
  memset(FTrayIcon, 0, sizeof(*FTrayIcon));
  FTrayIcon->cbSize = ((ShellDllVersion() >= 5) ? sizeof(*FTrayIcon) : NOTIFYICONDATA_V1_SIZE);
  FTrayIcon->uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP;
  FTrayIcon->hIcon = Application->Icon->Handle;
  FTrayIcon->uID = Id;
  FTrayIcon->hWnd = AllocateHWnd(WndProc);
  FTrayIcon->uCallbackMessage = WM_TRAY_ICON;

  FTaskbarCreatedMsg = RegisterWindowMessage(L"TaskbarCreated");
}
//---------------------------------------------------------------------------
__fastcall ::TTrayIcon::~TTrayIcon()
{
  // make sure we hide icon even in case it was shown just to pop up the balloon
  // (in which case Visible == false)
  CancelBalloon();
  Visible = false;
  DeallocateHWnd(FTrayIcon->hWnd);
  delete FTrayIcon;
}
//---------------------------------------------------------------------------
bool __fastcall ::TTrayIcon::SupportsBalloons()
{
  return (ShellDllVersion() >= 5);
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::PopupBalloon(UnicodeString Title,
  const UnicodeString & Str, TQueryType QueryType, unsigned int Timeout,
  TNotifyEvent OnBalloonClick)
{
  if (SupportsBalloons())
  {
    if (Timeout > 30000)
    {
      // this is probably system limit, do not try more, especially for
      // the timeout-driven hiding of the tray icon (for Win2k)
      Timeout = 30000;
    }
    FTrayIcon->uFlags |= NIF_INFO;
    Title = FORMAT(L"%s - %s", (Title, AppNameString()));
    StrPLCopy(FTrayIcon->szInfoTitle, Title, LENOF(FTrayIcon->szInfoTitle) - 1);
    StrPLCopy(FTrayIcon->szInfo, Str, LENOF(FTrayIcon->szInfo) - 1);
    FTrayIcon->uTimeout = Timeout;
    switch (QueryType)
    {
      case qtError:
        FTrayIcon->dwInfoFlags = NIIF_ERROR;
        break;

      case qtInformation:
      case qtConfirmation:
        FTrayIcon->dwInfoFlags = NIIF_INFO;
        break;

      case qtWarning:
      default:
        FTrayIcon->dwInfoFlags = NIIF_WARNING;
        break;
    }

    KillTimer(FTrayIcon->hWnd, 1);
    if (Visible)
    {
      Update();
    }
    else
    {
      Notify(NIM_ADD);

      // can be 5 only anyway, no older version supports balloons
      if (ShellDllVersion() <= 5)
      {
        // version 5 does not notify us when the balloon is hidden, so we must
        // remove the icon ourselves after own timeout
        SetTimer(FTrayIcon->hWnd, 1, Timeout, NULL);
      }
    }

    FOnBalloonClick = OnBalloonClick;

    // Clearing the flag ensures that subsequent updates does not hide the baloon
    // unless CancelBalloon is called explicitly
    FTrayIcon->uFlags = FTrayIcon->uFlags & ~NIF_INFO;
  }
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::BalloonCancelled()
{
  FOnBalloonClick = NULL;
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::CancelBalloon()
{
  if (SupportsBalloons())
  {
    KillTimer(FTrayIcon->hWnd, 1);
    if (Visible)
    {
      FTrayIcon->uFlags |= NIF_INFO;
      FTrayIcon->szInfo[0] = L'\0';
      Update();
      FTrayIcon->uFlags = FTrayIcon->uFlags & ~NIF_INFO;
    }
    else
    {
      Notify(NIM_DELETE);
    }

    BalloonCancelled();
  }
}
//---------------------------------------------------------------------------
bool __fastcall ::TTrayIcon::Notify(unsigned int Message)
{
  bool Result = SUCCEEDED(Shell_NotifyIcon(Message, (NOTIFYICONDATA*)FTrayIcon));
  if (Result && (Message == NIM_ADD))
  {
    UINT Timeout = FTrayIcon->uTimeout;
    try
    {
      FTrayIcon->uVersion = NOTIFYICON_VERSION;
      Result = SUCCEEDED(Shell_NotifyIcon(NIM_SETVERSION, (NOTIFYICONDATA*)FTrayIcon));
    }
    __finally
    {
      FTrayIcon->uTimeout = Timeout;
    }
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::Update()
{
  if (Visible)
  {
    Notify(NIM_MODIFY);
  }
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::SetVisible(bool value)
{
  if (Visible != value)
  {
    if (value)
    {
      FVisible = Notify(NIM_ADD);
    }
    else
    {
      FVisible = false;
      KillTimer(FTrayIcon->hWnd, 1);
      Notify(NIM_DELETE);
      BalloonCancelled();
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::WndProc(TMessage & Message)
{
  try
  {
    if (Message.Msg == WM_TRAY_ICON)
    {
      assert(Message.WParam == 0);
      switch (Message.LParam)
      {
        // old shell32
        case WM_LBUTTONUP:
        case WM_RBUTTONUP:
        // new shell32:
        case WM_CONTEXTMENU:
          if (OnClick != NULL)
          {
            OnClick(NULL);
          }
          Message.Result = true;
          break;
      }

      if (Message.LParam == NIN_BALLOONUSERCLICK)
      {
        if (FOnBalloonClick != NULL)
        {
          FOnBalloonClick(NULL);
        }
        else if (OnClick != NULL)
        {
          OnClick(NULL);
        }
      }

      switch (Message.LParam)
      {
        case NIN_BALLOONHIDE:
        case NIN_BALLOONTIMEOUT:
        case NIN_BALLOONUSERCLICK:
          KillTimer(FTrayIcon->hWnd, 1);
          // if icon was shown just to display balloon, hide it with the balloon
          if (!Visible)
          {
            Notify(NIM_DELETE);
          }
          BalloonCancelled();
          break;
      }
    }
    else if (Message.Msg == WM_TIMER)
    {
      // sanity check
      Notify(NIM_DELETE);
      BalloonCancelled();
    }
    else if (Message.Msg == FTaskbarCreatedMsg)
    {
      if (Visible)
      {
        // force recreation
        Visible = false;
        Visible = true;
      }
    }
    else
    {
      Message.Result = DefWindowProc(FTrayIcon->hWnd, Message.Msg, Message.WParam, Message.LParam);
    }
  }
  catch(Exception & E)
  {
    Application->HandleException(&E);
  }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall ::TTrayIcon::GetHint()
{
  return FTrayIcon->szTip;
}
//---------------------------------------------------------------------------
void __fastcall ::TTrayIcon::SetHint(UnicodeString value)
{
  if (Hint != value)
  {
    unsigned int Max = ((ShellDllVersion() >= 5) ? LENOF(FTrayIcon->szTip) : 64);
    StrPLCopy(FTrayIcon->szTip, value, Max - 1);
    Update();
  }
}
//---------------------------------------------------------------------------
