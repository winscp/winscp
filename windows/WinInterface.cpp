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
  TimerMessage = "";
  TimerAnswers = 0;
  Timeout = 0;
  TimeoutAnswer = 0;
  NewerAskAgainTitle = "";
  NewerAskAgainAnswer = 0;
  NewerAskAgainCheckedInitially = false;
  AllowHelp = true;
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
      for (int i = 0; i < LENOF(PositiveAnswers); i++)
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
int MapResult(int Result, unsigned int Answers)
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
TForm * __fastcall CreateMessageDialogEx(const AnsiString Msg,
  TStrings * MoreMessages, TQueryType Type, int Answers, AnsiString HelpKeyword,
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
  TForm * Dialog;

  if ((Params == NULL) || (Params->Aliases == NULL))
  {
    Dialog = CreateMoreMessageDialog(Msg, MoreMessages, DlgType, Buttons,
      NULL, 0, TimeoutResult, &TimeoutButton);
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
        Aliases, Params->AliasesCount, TimeoutResult, &TimeoutButton);
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
      NeverAskAgainCheck->Name = "NeverAskAgainCheck";
      NeverAskAgainCheck->Parent = Dialog;
      NeverAskAgainCheck->BoundsRect =  TRect(60, Dialog->ClientHeight - 27,
        Dialog->ClientWidth - 10, Dialog->ClientHeight - 5);
      NeverAskAgainCheck->Caption =
        !Params->NewerAskAgainTitle.IsEmpty() ?
          Params->NewerAskAgainTitle :
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
int __fastcall ExecuteMessageDialog(TForm * Dialog, int Answers, const TMessageParams * Params)
{
  FlashOnBackground();
  int Answer = MapResult(Dialog->ShowModal(), Answers);

  if ((Params != NULL) && (Params->Params & mpNeverAskAgainCheck))
  {
    TCheckBox * NeverAskAgainCheck =
      dynamic_cast<TCheckBox *>(Dialog->FindComponent("NeverAskAgainCheck"));
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

protected:
  unsigned int FTimeout;
  TButton * FButton;
  AnsiString FOrigCaption;

  void __fastcall DoTimer(TObject * Sender);
  void __fastcall UpdateButton();
};
//---------------------------------------------------------------------------
__fastcall TMessageTimeout::TMessageTimeout(TComponent * AOwner,
  unsigned int Timeout, TButton * Button) :
  TTimer(AOwner), FTimeout(Timeout), FButton(Button)
{
  OnTimer = DoTimer;
  Interval = 1000;
  FOrigCaption = FButton->Caption;
  UpdateButton();
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::UpdateButton()
{
  assert(FButton != NULL);
  FButton->Caption = FMTLOAD(TIMEOUT_BUTTON, (FOrigCaption, int(FTimeout / 1000)));
}
//---------------------------------------------------------------------------
void __fastcall TMessageTimeout::DoTimer(TObject * /*Sender*/)
{
  if (FTimeout <= 1000)
  {
    assert(FButton != NULL);
    TForm * Dialog = dynamic_cast<TForm *>(FButton->Parent);
    assert(Dialog != NULL);

    Dialog->ModalResult = FButton->ModalResult;
  }
  else
  {
    FTimeout -= 1000;
    UpdateButton();
  }
}
//---------------------------------------------------------------------------
int __fastcall MoreMessageDialog(const AnsiString Message, TStrings * MoreMessages,
  TQueryType Type, int Answers, AnsiString HelpKeyword, const TMessageParams * Params)
{
  int Result;
  TForm * Dialog = NULL;
  TMessageTimer * Timer = NULL;
  TMessageTimeout * Timeout = NULL;
  try
  {
    AnsiString AMessage = Message;

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
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, AnsiString HelpKeyword, const TMessageParams * Params)
{
  return MoreMessageDialog(Msg, NULL, Type, Answers, HelpKeyword, Params);
}
//---------------------------------------------------------------------------
int __fastcall SimpleErrorDialog(const AnsiString Msg, const AnsiString MoreMessages)
{
  int Result;
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
int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat, int Answers, AnsiString HelpKeyword,
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
  AnsiString Message;
  // this is always called from within ExceptionMessage check,
  // so it should never fail here
  CHECK(ExceptionMessage(E, Message));

  return MoreMessageDialog(
    FORMAT(MessageFormat.IsEmpty() ? AnsiString("%s") : MessageFormat, (Message)),
    MoreMessages, Type, Answers, HelpKeyword, Params);
}
//---------------------------------------------------------------------------
int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  int SessionReopenTimeout, const AnsiString MessageFormat, int Answers,
  AnsiString HelpKeyword, const TMessageParams * Params)
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
static void __fastcall CopyParamListSaveSettingsClick(TMenuItem * Item)
{
  bool * SaveSettings = reinterpret_cast<bool *>(Item->Tag);
  *SaveSettings = !*SaveSettings;
  Item->Checked = *SaveSettings;
}
//---------------------------------------------------------------------------
bool __fastcall DoRemoteMoveDialog(AnsiString & Target, AnsiString & FileMask)
{
  AnsiString Value = UnixIncludeTrailingBackslash(Target) + FileMask;
  TStrings * History = CustomWinConfiguration->History["RemoteTarget"];
  bool Result = InputDialog(
    LoadStr(REMOTE_MOVE_TITLE), LoadStr(REMOTE_TRANSFER_PROMPT),
    Value, HELP_REMOTE_MOVE, History, true);
  if (Result)
  {
    CustomWinConfiguration->History["RemoteTarget"] = History;
    Target = UnixExtractFilePath(Value);
    FileMask = UnixExtractFileName(Value);
  }
  return Result;
}
//---------------------------------------------------------------------------
void __fastcall CopyParamListPopup(TPoint P, TPopupMenu * Menu,
  const TCopyParamType & Param, AnsiString Preset, TNotifyEvent OnClick,
  int Options, bool * SaveSettings)
{
  Menu->Items->Clear();

  bool AnyChecked = false;
  TMenuItem * Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_DEFAULT);
  Item->Tag = -1;
  Item->Checked =
    Preset.IsEmpty() && (GUIConfiguration->CopyParamPreset[""] == Param);
  AnyChecked = AnyChecked || Item->Checked;
  Item->OnClick = OnClick;
  Menu->Items->Add(Item);

  const TCopyParamList * CopyParamList = GUIConfiguration->CopyParamList;
  for (int i = 0; i < CopyParamList->Count; i++)
  {
    Item = new TMenuItem(Menu);
    AnsiString Name = CopyParamList->Names[i];
    Item->Caption = Name;
    Item->Tag = i;
    Item->Checked =
      (Preset == Name) && (GUIConfiguration->CopyParamPreset[Name] == Param);
    AnyChecked = AnyChecked || Item->Checked;
    Item->OnClick = OnClick;
    Menu->Items->Add(Item);
  }

  if (FLAGSET(Options, cplCustomize))
  {
    Item = new TMenuItem(Menu);
    Item->Caption = LoadStr(COPY_PARAM_CUSTOM);
    Item->Tag = -3;
    Item->Checked = !AnyChecked;
    Item->Default = FLAGSET(Options, cplCustomizeDefault);
    Item->OnClick = OnClick;
    Menu->Items->Add(Item);
  }

  if (FLAGSET(Options, cplSaveSettings))
  {
    assert(SaveSettings != NULL);

    Item = new TMenuItem(Menu);
    Item->Caption = LoadStr(COPY_PARAM_SAVE_SETTINGS);
    Item->Tag = int(SaveSettings);
    Item->Checked = *SaveSettings;

    TNotifyEvent SaveSettingsOnClick;
    ((TMethod*)&SaveSettingsOnClick)->Data = Item;
    ((TMethod*)&SaveSettingsOnClick)->Code = CopyParamListSaveSettingsClick;
    Item->OnClick = SaveSettingsOnClick;

    Menu->Items->Add(Item);
  }

  Item = new TMenuItem(Menu);
  Item->Caption = "-";
  Menu->Items->Add(Item);

  Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_CONFIGURE);
  Item->Tag = -2;
  Item->OnClick = OnClick;
  Menu->Items->Add(Item);

  MenuPopup(Menu, P, NULL);
}
//---------------------------------------------------------------------------
bool __fastcall CopyParamListPopupClick(TObject * Sender,
  TCopyParamType & Param, AnsiString & Preset, int CopyParamAttrs)
{
  TComponent * Item = dynamic_cast<TComponent *>(Sender);
  assert(Item != NULL);
  assert((Item->Tag >= -3) && (Item->Tag < GUIConfiguration->CopyParamList->Count));

  bool Result;
  if (Item->Tag == -2)
  {
    DoPreferencesDialog(pmPresets);
    Result = false;
  }
  else if (Item->Tag == -3)
  {
    Result = DoCopyParamCustomDialog(Param, CopyParamAttrs);
  }
  else
  {
    Preset = (Item->Tag >= 0) ?
      GUIConfiguration->CopyParamList->Names[Item->Tag] : AnsiString();
    Param = GUIConfiguration->CopyParamPreset[Preset];
    Result = true;
  }
  return Result;
}
//---------------------------------------------------------------------------
TWinInteractiveCustomCommand::TWinInteractiveCustomCommand(
  TCustomCommand * ChildCustomCommand, const AnsiString CustomCommandName) :
  TInteractiveCustomCommand(ChildCustomCommand)
{
  FCustomCommandName = StripHotkey(CustomCommandName);
}
//---------------------------------------------------------------------------
void __fastcall TWinInteractiveCustomCommand::Prompt(int /*Index*/,
  const AnsiString & Prompt, AnsiString & Value)
{
  AnsiString APrompt = Prompt;
  if (APrompt.IsEmpty())
  {
    APrompt = FMTLOAD(CUSTOM_COMMANDS_PARAM_PROMPT, (FCustomCommandName));
  }
  TStrings * History = CustomWinConfiguration->History["CustomCommandParam"];
  if (InputDialog(FMTLOAD(CUSTOM_COMMANDS_PARAM_TITLE, (FCustomCommandName)),
        APrompt, Value, HELP_CUSTOM_COMMAND_PARAM, History))
  {
    CustomWinConfiguration->History["CustomCommandParam"] = History;
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
    HINSTANCE ShellDll = LoadLibrary("shell32.dll");
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
  class TPublicControl : public TControl
  {
  friend void __fastcall MenuPopup(TObject * Sender, const TPoint & MousePos, bool & Handled);
  };

  TControl * Control = dynamic_cast<TControl *>(Sender);
  TPoint Point;
  if ((MousePos.x == -1) && (MousePos.y == -1))
  {
    Point = Control->ClientToScreen(TPoint(0, 0));
  }
  else
  {
    Point = Control->ClientToScreen(MousePos);
  }
  assert(Control != NULL);
  TPopupMenu * PopupMenu = (reinterpret_cast<TPublicControl *>(Control))->PopupMenu;
  assert(PopupMenu != NULL);
  MenuPopup(PopupMenu, Point, Control);
  Handled = true;
}
//---------------------------------------------------------------------------
void __fastcall SetGlobalMinimizeHandler(TNotifyEvent OnMinimize)
{
  GlobalOnMinimize = OnMinimize;
}
//---------------------------------------------------------------------------
TNotifyEvent __fastcall GetGlobalMinimizeHandler()
{
  return GlobalOnMinimize;
}
//---------------------------------------------------------------------------
bool __fastcall IsGlobalMinimizeHandler()
{
  return (GlobalOnMinimize != NULL);
}
//---------------------------------------------------------------------------
LCID __fastcall GetDefaultLCID()
{
  return Is2000() ? GetUserDefaultLCID() : GetThreadLocale();
}
//---------------------------------------------------------------------------
AnsiString __fastcall TranslateDateFormat(AnsiString FormatStr)
{
  AnsiString Result;
  CALTYPE CalendarType = StrToIntDef(GetLocaleStr(GetDefaultLCID(), LOCALE_ICALENDARTYPE, "1"), 1);
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
        if ((FormatStr[I] != 'g') && (FormatStr[I] != 'G'))
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
      if (FormatStr.IsLeadByte(I))
      {
        int L = CharLength(FormatStr, I);
        Result += FormatStr.SubString(I, L);
        I += L;
      }
      else
      {
        if (StrLIComp(FormatStr.c_str() + I - 1, "gg", 2) == 0)
        {
          Result += "ggg";
          I++;
        }
        else if (StrLIComp(FormatStr.c_str() + I - 1, "yyyy", 4) == 0)
        {
          Result += "eeee";
          I += 4 - 1;
        }
        else if (StrLIComp(FormatStr.c_str() + I - 1, "yy", 2) == 0)
        {
          Result += "ee";
          I += 2 - 1;
        }
        else if ((FormatStr[I] == 'y') || (FormatStr[I] == 'Y'))
        {
          Result += "e";
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
  CurrencyString = GetLocaleStr(DefaultLCID, LOCALE_SCURRENCY, "");
  CurrencyFormat = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRENCY, "0"), 0);
  NegCurrFormat = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_INEGCURR, "0"), 0);
  ThousandSeparator = GetLocaleChar(DefaultLCID, LOCALE_STHOUSAND, ',');
  DecimalSeparator = GetLocaleChar(DefaultLCID, LOCALE_SDECIMAL, '.');
  CurrencyDecimals = (Byte) StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ICURRDIGITS, "0"), 0);
  DateSeparator = GetLocaleChar(DefaultLCID, LOCALE_SDATE, '/');
  ShortDateFormat = TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, "m/d/yy"));
  LongDateFormat = TranslateDateFormat(GetLocaleStr(DefaultLCID, LOCALE_SLONGDATE, "mmmm d, yyyy"));
  TimeSeparator = GetLocaleChar(DefaultLCID, LOCALE_STIME, ':');
  TimeAMString = GetLocaleStr(DefaultLCID, LOCALE_S1159, "am");
  TimePMString = GetLocaleStr(DefaultLCID, LOCALE_S2359, "pm");
  AnsiString HourFormat;
  if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITLZERO, "0"), 0) == 0)
  {
    HourFormat = "h";
  }
  else
  {
    HourFormat = "hh";
  }
  AnsiString TimePrefix, TimePostfix;
  if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIME, "0"), 0) == 0)
  {
    if (StrToIntDef(GetLocaleStr(DefaultLCID, LOCALE_ITIMEMARKPOSN, "0"), 0) == 0)
    {
      TimePostfix = " AMPM";
    }
    else
    {
      TimePrefix = "AMPM ";
    }
  }
  ShortTimeFormat = TimePrefix + HourFormat + ":mm" + TimePostfix;
  LongTimeFormat = TimePrefix + HourFormat + ":mm:ss" + TimePostfix;
  ListSeparator = GetLocaleChar(DefaultLCID, LOCALE_SLIST, ',');
}
//---------------------------------------------------------------------------
void __fastcall WinInitialize()
{
  if (IsWin7())
  {
    GetFormatSettingsFix();
  }


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
  CHAR szTip[128];
  DWORD dwState;
  DWORD dwStateMask;
  CHAR szInfo[256];
  union {
    UINT uTimeout;
    UINT uVersion;
  } DUMMYUNIONNAME;
  CHAR szInfoTitle[64];
  DWORD dwInfoFlags;
};
//---------------------------------------------------------------------------
#undef NOTIFYICONDATA_V1_SIZE
#define NOTIFYICONDATA_V1_SIZE FIELD_OFFSET(TNotifyIconData5, szTip[64])
//---------------------------------------------------------------------------
__fastcall TTrayIcon::TTrayIcon(unsigned int Id)
{
  FVisible = false;
  FOnClick = NULL;

  FTrayIcon = new TNotifyIconData5;
  memset(FTrayIcon, 0, sizeof(*FTrayIcon));
  FTrayIcon->cbSize = ((ShellDllVersion() >= 5) ? sizeof(*FTrayIcon) : NOTIFYICONDATA_V1_SIZE);
  FTrayIcon->uFlags = NIF_MESSAGE | NIF_ICON | NIF_TIP;
  FTrayIcon->hIcon = Application->Icon->Handle;
  FTrayIcon->uID = Id;
  FTrayIcon->hWnd = AllocateHWnd(WndProc);
  FTrayIcon->uCallbackMessage = WM_TRAY_ICON;
}
//---------------------------------------------------------------------------
__fastcall TTrayIcon::~TTrayIcon()
{
  // make sure we hide icon even in case it was shown just to pop up the balloon
  // (in which case Visible == false)
  CancelBalloon();
  Visible = false;
  DeallocateHWnd(FTrayIcon->hWnd);
  delete FTrayIcon;
}
//---------------------------------------------------------------------------
bool __fastcall TTrayIcon::SupportsBalloons()
{
  return (ShellDllVersion() >= 5);
}
//---------------------------------------------------------------------------
void __fastcall TTrayIcon::PopupBalloon(AnsiString Title,
  const AnsiString & Str, TQueryType QueryType, unsigned int Timeout)
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
    Title = FORMAT("%s - %s", (Title, AppNameString()));
    StrPLCopy(FTrayIcon->szInfoTitle, Title, sizeof(FTrayIcon->szInfoTitle) - 1);
    StrPLCopy(FTrayIcon->szInfo, Str, sizeof(FTrayIcon->szInfo) - 1);
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

    // Clearing the flag ensures that subsequent updates does not hide the baloon
    // unless CancelBalloon is called explicitly
    FTrayIcon->uFlags = FTrayIcon->uFlags & ~NIF_INFO;
  }
}
//---------------------------------------------------------------------------
void __fastcall TTrayIcon::CancelBalloon()
{
  if (SupportsBalloons())
  {
    KillTimer(FTrayIcon->hWnd, 1);
    if (Visible)
    {
      FTrayIcon->uFlags |= NIF_INFO;
      FTrayIcon->szInfo[0] = '\0';
      Update();
      FTrayIcon->uFlags = FTrayIcon->uFlags & ~NIF_INFO;
    }
    else
    {
      Notify(NIM_DELETE);
    }
  }
}
//---------------------------------------------------------------------------
bool __fastcall TTrayIcon::Notify(unsigned int Message)
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
void __fastcall TTrayIcon::Update()
{
  if (Visible)
  {
    Notify(NIM_MODIFY);
  }
}
//---------------------------------------------------------------------------
void __fastcall TTrayIcon::SetVisible(bool value)
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
    }
  }
}
//---------------------------------------------------------------------------
void __fastcall TTrayIcon::WndProc(TMessage & Message)
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
        case NIN_BALLOONUSERCLICK:
          if (OnClick != NULL)
          {
            OnClick(NULL);
          }
          Message.Result = true;
          break;
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
          break;
      }
    }
    else if (Message.Msg == WM_TIMER)
    {
      // sanity check
      Notify(NIM_DELETE);
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
AnsiString __fastcall TTrayIcon::GetHint()
{
  return FTrayIcon->szTip;
}
//---------------------------------------------------------------------------
void __fastcall TTrayIcon::SetHint(AnsiString value)
{
  if (Hint != value)
  {
    unsigned int Max = ((ShellDllVersion() >= 5) ? sizeof(FTrayIcon->szTip) : 64);
    StrPLCopy(FTrayIcon->szTip, value, Max - 1);
    Update();
  }
}
//---------------------------------------------------------------------------
