//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <MoreButton.hpp>

#include <Common.h>
#include <Net.h>
#include <SecureShell.h>
#include <ScpMain.h>
#include <TextsWin.h>
#include <Interface.h>

#include "WinInterface.h"
#include "CustomWinConfiguration.h"
#include "GUITools.h"

#define mrCustom (mrYesToAll + 1)
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
TForm * __fastcall CreateMessageDialogEx(const AnsiString Msg, TQueryType Type,
  int Answers, int HelpCtx, int Params)
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

  #define ADD_BUTTON(TYPE) if (Answers & qa ## TYPE) Buttons << mb ## TYPE;
  ADD_BUTTON(Yes);
  ADD_BUTTON(No);
  ADD_BUTTON(OK);
  ADD_BUTTON(Cancel);
  ADD_BUTTON(Abort);
  ADD_BUTTON(Retry);
  ADD_BUTTON(Ignore);
  ADD_BUTTON(All);
  ADD_BUTTON(NoToAll);
  ADD_BUTTON(YesToAll);
  ADD_BUTTON(Help);
  #undef ADD_BUTTON

  if (Answers & qaSkip)
  {
    assert((Answers & qaIgnore) == 0);
    Buttons << mbIgnore;
  }

  if (Answers & qaPrev)
  {
    assert((Answers & qaYes) == 0);
    Buttons << mbYes;
  }

  if (Answers & qaNext)
  {
    assert((Answers & qaNo) == 0);
    Buttons << mbNo;
  }

  if (Answers & qaCustom)
  {
    assert((Answers & qaHelp) == 0);
    Buttons << mbHelp;
  }

  if (Answers & qaAppend)
  {
    assert((Answers & qaRetry) == 0);
    Buttons << mbRetry;
  }

  assert(!Buttons.Empty());

  TForm * Dialog = CreateMessageDialog(Msg, DlgType, Buttons);

  try
  {
    if (Answers & qaSkip)
    {
      TButton * IgnoreButton = dynamic_cast<TButton *>(Dialog->FindComponent("Ignore"));
      assert(IgnoreButton);
      IgnoreButton->Caption = LoadStr(SKIP_BUTTON);
    }

    if (Answers & qaPrev)
    {
      TButton * YesButton = dynamic_cast<TButton *>(Dialog->FindComponent("Yes"));
      assert(YesButton);
      YesButton->Caption = LoadStr(PREV_BUTTON);
    }

    if (Answers & qaNext)
    {
      TButton * NoButton = dynamic_cast<TButton *>(Dialog->FindComponent("No"));
      assert(NoButton);
      NoButton->Caption = LoadStr(NEXT_BUTTON);
    }

    if (Answers & qaAppend)
    {
      TButton * RetryButton = dynamic_cast<TButton *>(Dialog->FindComponent("Retry"));
      assert(RetryButton);
      RetryButton->Caption = LoadStr(APPEND_BUTTON);
    }

    if (Answers & qaCustom)
    {
      TButton * HelpButton = dynamic_cast<TButton *>(Dialog->FindComponent("Help"));
      assert(HelpButton);
      HelpButton->Name = "Custom";
      HelpButton->ModalResult = mrCustom;
    }

    // temporary fix of accelerators (&Abort vs. &All/Yes to &All)
    // must be removed
    for (int Index = 0; Index < Dialog->ComponentCount; Index++)
    {
      TButton * B = dynamic_cast<TButton *>(Dialog->Components[Index]);
      if (B)
      {
        if (B->Caption == "&All") B->Caption = "A&ll";
          else
        if (B->Caption == "Yes to &All") B->Caption = "Yes to A&ll";
      }
    }

    if (Params & mpNeverAskAgainCheck)
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
int __fastcall ExecuteMessageDialog(TForm * Dialog, int Answers, int Params)
{
  int Result, Answer;
  FlashOnBackground();
  Result = Dialog->ShowModal();

  switch (Result) {
    #define MAP_RESULT(RESULT) case mr ## RESULT: Answer = qa ## RESULT; break;
    MAP_RESULT(Cancel);
    MAP_RESULT(Abort);
    MAP_RESULT(All);
    MAP_RESULT(NoToAll);
    MAP_RESULT(YesToAll);

    MAP_RESULT(Custom);
    #undef MAP_RESULT

    case mrOk:
      Answer = qaOK;
      break;

    case mrIgnore:
      Answer = (Answers & qaSkip) ? qaSkip : qaIgnore;
      break;

    case mrYes:
      Answer = (Answers & qaPrev) ? qaPrev : qaYes;
      break;

    case mrNo:
      Answer = (Answers & qaNext) ? qaNext : qaNo;
      break;

    case mrRetry:
      Answer = (Answers & qaAppend) ? qaAppend : qaRetry;
      break;
  }

  if (Params & mpNeverAskAgainCheck)
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

  TMoreButton * MoreButton = dynamic_cast<TMoreButton *>(Dialog->FindComponent("MoreButton"));
  // WinConfiguration may be destroyed already, if called from
  // try ... catch statement of main()
  if (MoreButton && GUIConfiguration)
  {
    // store state even when user selects 'Cancel'?
    GUIConfiguration->ErrorDialogExpanded = MoreButton->Expanded;
  }

  return Answer;
}
//---------------------------------------------------------------------------
int __fastcall MessageDialog(const AnsiString Msg, TQueryType Type,
  int Answers, int HelpCtx, int Params)
{
  int Result;
  TForm * Dialog = CreateMessageDialogEx(Msg, Type, Answers, HelpCtx, Params);
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
TForm * __fastcall CreateMoreMessageDialog(const AnsiString Message,
  TStrings * MoreMessages, TQueryType Type, int Answers,
  int HelpCtx, int Params)
{
  if (!MoreMessages || (MoreMessages->Count == 0))
  {
    return CreateMessageDialogEx(Message, Type, Answers, HelpCtx, Params);
  }
  else
  {
    TForm * Dialog;
    Answers |= qaCustom;

    Dialog = CreateMessageDialogEx(Message, Type, Answers, HelpCtx, Params);
    try
    {
      TButton * CustomButton = dynamic_cast<TButton *>(Dialog->FindComponent("Custom"));
      TLabel * MsgLabel = dynamic_cast<TLabel *>(Dialog->FindComponent("Message"));
      assert(MsgLabel && CustomButton);

      int OrigButtonTop = CustomButton->Top;
      int WidthDelta = 0;
      if (Dialog->ClientWidth < 400)
      {
        WidthDelta = (400 - Dialog->ClientWidth);
      }
      Dialog->ClientHeight = Dialog->ClientHeight + 130;
      Dialog->ClientWidth = Dialog->ClientWidth + WidthDelta;
      MsgLabel->Width = MsgLabel->Width + WidthDelta;

      for (int Index = 0; Index < Dialog->ControlCount; Index++)
      {
        if (Dialog->Controls[Index]->InheritsFrom(__classid(TButton)))
        {
          TControl * Control = Dialog->Controls[Index];
          Control->Anchors = TAnchors() << akBottom << akLeft;
          Control->Top = Control->Top + 130;
          Control->Left = Control->Left + (WidthDelta / 2);
        }
      }

      TMemo * MessageMemo = new TMemo(Dialog);
      MessageMemo->Parent = Dialog;
      MessageMemo->ReadOnly = True;
      MessageMemo->WantReturns = False;
      MessageMemo->ScrollBars = ssVertical;
      MessageMemo->Anchors = TAnchors() << akLeft << akRight << akTop; //akBottom;
      MessageMemo->BoundsRect = TRect(MsgLabel->Left, OrigButtonTop - 10,
        Dialog->ClientWidth - 20, CustomButton->Top - 10);
      MessageMemo->Color = clBtnFace;
      MessageMemo->Lines->Text = MoreMessages->Text;

      TMoreButton * MoreButton = new TMoreButton(Dialog);
      MoreButton->Parent = Dialog;
      MoreButton->BoundsRect = CustomButton->BoundsRect;
      MoreButton->Anchors = CustomButton->Anchors;
      MoreButton->Panel = MessageMemo;
      // WinConfiguration may be destroyed already, if called from
      // try ... catch statement of main()
      MoreButton->Expanded = GUIConfiguration && GUIConfiguration->ErrorDialogExpanded;
      MoreButton->Name = "MoreButton";

      MessageMemo->TabOrder = 20;

      delete CustomButton;
    }
    catch(...)
    {
      delete Dialog;
      throw;
    }

    return Dialog;
  }
}
//---------------------------------------------------------------------------
int __fastcall MoreMessageDialog(const AnsiString Message, TStrings * MoreMessages,
  TQueryType Type, int Answers, int HelpCtx, int Params)
{
  int Result;
  TForm * Dialog;

  Dialog = CreateMoreMessageDialog(Message, MoreMessages, Type,
    Answers, HelpCtx, Params);
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
int __fastcall SimpleErrorDialog(const AnsiString Msg)
{
  return MessageDialog(Msg, qtError, qaOK, 0);
}
//---------------------------------------------------------------------------
int __fastcall ExceptionMessageDialog(Exception * E,
  TQueryType Type, int Answers, int HelpCtx)
{
  TStrings * MoreMessages = NULL;
  if (E->InheritsFrom(__classid(ExtException)))
  {
    MoreMessages = ((ExtException *)E)->MoreMessages;
  }

  int Result;
  TForm * Dialog;

  Dialog = CreateMoreMessageDialog(E->Message, MoreMessages, Type,
    Answers, HelpCtx, 0);
  try
  {
    Result = ExecuteMessageDialog(Dialog, Answers, 0);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall FatalExceptionMessageDialog(Exception * E,
  TQueryType Type, int HelpCtx)
{
  TStrings * MoreMessages = NULL;
  if (E->InheritsFrom(__classid(ExtException)))
  {
    MoreMessages = ((ExtException *)E)->MoreMessages;
  }

  int Result, Answers;

  Answers = qaOK | qaRetry;

  TForm * Dialog = CreateMoreMessageDialog(E->Message, MoreMessages, Type,
    Answers, HelpCtx, 0);
  try
  {
    TButton * RetryButton = dynamic_cast<TButton *>(Dialog->FindComponent("Retry"));
    assert(RetryButton);
    RetryButton->Caption = LoadStr(RECONNECT_BUTTON);

    Result = ExecuteMessageDialog(Dialog, Answers, 0);
  }
  __finally
  {
    delete Dialog;
  }
  return Result;
}
//---------------------------------------------------------------------------
int __fastcall GetSessionPassword(AnsiString Prompt,
  TPasswordKind Kind, AnsiString & Password)
{
  return DoPasswordDialog(Prompt, Kind, Password);
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
