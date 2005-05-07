//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <MoreButton.hpp>

#include <Common.h>
#include <Net.h>
#include <SecureShell.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <HelpWin.h>
#include <Interface.h>
#include <VCLCommon.h>

#include "WinInterface.h"
#include "CustomWinConfiguration.h"
#include "GUITools.h"

#define mrCustom (mrYesToAll + 1)
//---------------------------------------------------------------------------
#pragma package(smart_init)
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
  Timer = NULL;
  TimerEvent = NULL;
  TimerMessage = "";
  TimerAnswers = 0;
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
  const TMessageParams * Params)
{
  // Original int HelpContext parameter to message boxes functions 
  // were replaced with AnsiString HelpKeyword. We need to find all calls
  // that passed 0
  // TODO: Eventually remote this all.
  assert(HelpKeyword != "0");
  if (HelpKeyword == "0")
  {
    HelpKeyword = HELP_NONE;
  }

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
  int Answer = 0x01;
  while (AAnswers > 0)
  {
    if ((AAnswers & Answer) != 0)
    {
      TMsgDlgBtn Button;
      if (MapButton(Answer, Button))
      {
        Buttons << Button;
      }
      AAnswers &= ~Answer;
    }
    Answer <<= 1;
  }

  assert(!Buttons.Empty());

  if (!HelpKeyword.IsEmpty())
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
    Dialog = CreateMoreMessageDialog(Msg, MoreMessages, DlgType, Buttons);
  }
  else
  {
    TQueryButtonAlias * Aliases = new TQueryButtonAlias[Params->AliasesCount];
    try
    {
      for (unsigned int i = 0; i < Params->AliasesCount; i++)
      {
        TMsgDlgBtn Button;
        bool R = MapButton(Params->Aliases[i].Button, Button);
        USEDPARAM(R);
        assert(R);
        Aliases[i].Button = Button;
        Aliases[i].Alias = Params->Aliases[i].Alias;
      }
      Dialog = CreateMoreMessageDialog(Msg, MoreMessages, DlgType, Buttons,
        Aliases, Params->AliasesCount);
    }
    __finally
    {
      delete Aliases;
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
        LoadStr(Answers == qaOK ? NEVER_SHOW_AGAIN : NEVER_ASK_AGAIN);
      NeverAskAgainCheck->Checked = false;
      NeverAskAgainCheck->Anchors = TAnchors() << akBottom << akLeft;
    }

    Dialog->HelpKeyword = HelpKeyword;
    Dialog->Position = poMainFormCenter;
    // must be called after setting Position
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
  TMoreButton * MoreButton = dynamic_cast<TMoreButton *>(Dialog->FindComponent("MoreButton"));
  if (MoreButton != NULL)
  {
    // WinConfiguration may be destroyed already, if called from
    // try ... catch statement of main()
    MoreButton->Expanded = (GUIConfiguration != NULL) &&
      GUIConfiguration->ErrorDialogExpanded;
  }

  FlashOnBackground();
  int Answer = MapResult(Dialog->ShowModal(), Answers);

  if ((Params != NULL) && (Params->Params & mpNeverAskAgainCheck))
  {
    TCheckBox * NeverAskAgainCheck =
      dynamic_cast<TCheckBox *>(Dialog->FindComponent("NeverAskAgainCheck"));
    assert(NeverAskAgainCheck);

    if (NeverAskAgainCheck->Checked &&
        (Answer == qaYes || Answer == qaOK || Answer == qaYesToAll))
    {
      Answer = qaNeverAskAgain;
    }
  }

  if ((MoreButton != NULL) && (GUIConfiguration != NULL))
  {
    // store state even when user selects 'Cancel'?
    GUIConfiguration->ErrorDialogExpanded = MoreButton->Expanded;
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
int __fastcall MoreMessageDialog(const AnsiString Message, TStrings * MoreMessages,
  TQueryType Type, int Answers, AnsiString HelpKeyword, const TMessageParams * Params)
{
  int Result;
  TForm * Dialog = NULL;
  TMessageTimer * Timer = NULL;
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

    Dialog = CreateMessageDialogEx(AMessage, MoreMessages, Type, Answers,
      HelpKeyword, Params);

    if (Timer != NULL)
    {
      Timer->Dialog = Dialog;
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
  }
  AnsiString Message = TranslateExceptionMessage(E);
  
  return MoreMessageDialog(
    FORMAT(MessageFormat.IsEmpty() ? AnsiString("%s") : MessageFormat, (Message)),
    MoreMessages, Type, Answers, HelpKeyword, Params);
}
//---------------------------------------------------------------------------
int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat, int Answers, AnsiString HelpKeyword,
  const TMessageParams * Params)
{
  assert((Answers & qaRetry) == 0);
  Answers |= qaRetry;

  TQueryButtonAlias Aliases[1];
  Aliases[0].Button = qaRetry;
  Aliases[0].Alias = LoadStr(RECONNECT_BUTTON);

  TMessageParams AParams;
  if (Params != NULL)
  {
    assert(Params->Aliases == NULL);
    AParams = *Params;
  }
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
bool __fastcall DoRemoteTransferDialog(TStrings * FileList, AnsiString & Target,
  AnsiString & FileMask, bool Move)
{
  AnsiString Prompt = FileNameFormatString(
    LoadStr(Move ? REMOTE_MOVE_FILE : REMOTE_COPY_FILE),
    LoadStr(Move ? REMOTE_MOVE_FILES : REMOTE_COPY_FILES), FileList, true);

  AnsiString Value = UnixIncludeTrailingBackslash(Target) + FileMask;
  TStrings * History = CustomWinConfiguration->History["RemoteTarget"];
  bool Result = InputDialog(
    LoadStr(Move ? REMOTE_MOVE_TITLE : REMOTE_COPY_TITLE), Prompt,
    Value, HELP_REMOTE_TRANSFER, History, true);
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
  int Options)
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
  
  Item = new TMenuItem(Menu);
  Item->Caption = "-";
  Menu->Items->Add(Item);

  Item = new TMenuItem(Menu);
  Item->Caption = LoadStr(COPY_PARAM_CONFIGURE);
  Item->Tag = -2;
  Item->OnClick = OnClick;
  Menu->Items->Add(Item);
  
  Menu->Popup(P.x, P.y);
}
//---------------------------------------------------------------------------
bool __fastcall CopyParamListPopupClick(TObject * Sender,
  TCopyParamType & Param, AnsiString & Preset, int CustomizeOptions)
{
  TMenuItem * Item = dynamic_cast<TMenuItem*>(Sender);
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
    Result = DoCopyParamCustomDialog(Param, CustomizeOptions);
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
