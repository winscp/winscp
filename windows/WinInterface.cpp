//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <MoreButton.hpp>

#include <Common.h>
#include <Net.h>
#include <SecureShell.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <TextsCore.h>
#include <Interface.h>

#include "WinInterface.h"
#include "CustomWinConfiguration.h"
#include "GUITools.h"

#define mrCustom (mrYesToAll + 1)
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TMessageParams::TMessageParams(unsigned int AParams)
{
  Params = AParams;
  Aliases = NULL;
  AliasesCount = 0;
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
TForm * __fastcall CreateMessageDialogEx(const AnsiString Msg,
  TStrings * MoreMessages, TQueryType Type, int Answers, int HelpCtx,
  const TMessageParams * Params)
{
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
      Dialog->ClientHeight = Dialog->ClientHeight + 20;

      TCheckBox * NeverAskAgainCheck = new TCheckBox(Dialog);
      NeverAskAgainCheck->Name = "NeverAskAgainCheck";
      NeverAskAgainCheck->Parent = Dialog;
      NeverAskAgainCheck->BoundsRect =  TRect(60, Dialog->ClientHeight - 27,
        Dialog->ClientWidth - 10, Dialog->ClientHeight - 5);
      NeverAskAgainCheck->Caption = LoadStr(NEVER_ASK_AGAIN);
      NeverAskAgainCheck->Checked = false;
    }

    Dialog->HelpContext = HelpCtx;
    Dialog->Position = poMainFormCenter;
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

  int Result, Answer;
  FlashOnBackground();
  Result = Dialog->ShowModal();

  switch (Result) {
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
  }

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
int __fastcall MoreMessageDialog(const AnsiString Message, TStrings * MoreMessages,
  TQueryType Type, int Answers, int HelpCtx, const TMessageParams * Params)
{
  int Result;
  TForm * Dialog = CreateMessageDialogEx(Message, MoreMessages, Type, Answers,
    HelpCtx, Params);
  try
  {
    Result = ExecuteMessageDialog(Dialog, Answers, Params);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, int HelpCtx, const TMessageParams * Params)
{
  return MoreMessageDialog(Msg, NULL, Type, Answers, HelpCtx, Params);
}
//---------------------------------------------------------------------------
int __fastcall SimpleErrorDialog(const AnsiString Msg)
{
  return MoreMessageDialog(Msg, NULL, qtError, qaOK, NULL);
}
//---------------------------------------------------------------------------
AnsiString __fastcall TranslateExceptionMessage(const Exception * E)
{
  if (dynamic_cast<const EAccessViolation*>(E) != NULL)
  {
    return LoadStr(ACCESS_VIOLATION_ERROR);
  }
  else
  {
    return E->Message;
  }
}
//---------------------------------------------------------------------------
int __fastcall ExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat, int Answers, int HelpCtx, const TMessageParams * Params)
{
  TStrings * MoreMessages = NULL;
  ExtException * EE = dynamic_cast<ExtException *>(E);
  if (EE != NULL)
  {
    MoreMessages = EE->MoreMessages;
  }
  AnsiString Message = TranslateExceptionMessage(E);
  
  return MoreMessageDialog(FORMAT(MessageFormat, (Message)),
    MoreMessages, Type, Answers, HelpCtx, Params);
}
//---------------------------------------------------------------------------
int __fastcall FatalExceptionMessageDialog(Exception * E, TQueryType Type,
  const AnsiString MessageFormat, int Answers, int HelpCtx, const TMessageParams * Params)
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

  return ExceptionMessageDialog(E, Type, MessageFormat, Answers, HelpCtx, &AParams);
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
bool __fastcall DoRemoteMoveDialog(TStrings * FileList, AnsiString & Target,
  AnsiString & FileMask)
{
  AnsiString Prompt = FileNameFormatString(LoadStr(REMOTE_MOVE_FILE),
    LoadStr(REMOTE_MOVE_FILES), FileList, true);

  AnsiString Value = UnixIncludeTrailingBackslash(Target) + FileMask;
  TStrings * History = CustomWinConfiguration->History["RemoteTarget"];
  bool Result = InputDialog(LoadStr(REMOTE_MOVE_TITLE), Prompt,
    Value, History);
  if (Result)
  {
    CustomWinConfiguration->History["RemoteTarget"] = History;
    Target = UnixExtractFilePath(Value);
    FileMask = UnixExtractFileName(Value);
  }
  return Result;
}
