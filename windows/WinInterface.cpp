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

#define mrResume (mrYesToAll    + 1)
#define mrCustom (mrResume  + 1)
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall FlashOnBackground()
{
  assert(Application);
  if (GetForegroundWindow() != GetActiveWindow())
  {
    FlashWindow(Application->Handle, true);
  }
}
//---------------------------------------------------------------------------
void __fastcall ShowExtendedException(Exception * E, TObject * Sender)
{
  if (!E->Message.IsEmpty())
  {
    if (E->InheritsFrom(__classid(Exception)))
    {
      if (!E->InheritsFrom(__classid(EAbort)))
      {
        TQueryType Type;
        Type = (E->InheritsFrom(__classid(ESshTerminate)) ?
          qtInformation : qtError);
        if (E->InheritsFrom(__classid(EFatal)))
        {
          if (FatalExceptionMessageDialog(E, Type) == qaRetry)
          {
            ReconnectTerminal();
          }
        }
        else
        {
          ExceptionMessageDialog(E, Type, qaOK);
        }
      }
    }
    else
    {
      FlashOnBackground();
      ShowException(ExceptObject(), ExceptAddr());
    }
  }
  HandleExtendedException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall HandleExtendedException(Exception * E, TObject* /*Sender*/)
{
  if (CurrentSSH)
  {
    CurrentSSH->Log->AddException(E);
  }

  if (E->InheritsFrom(__classid(EFatal)))
  {
    Application->Terminate();
  }
}
//---------------------------------------------------------------------------
TForm * CreateMessageDialogEx(const AnsiString Msg, TQueryType Type,
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

  if ((Answers & qaResume) || (Answers & qaCustom))
  {
    assert((Answers & qaHelp) == 0);
    assert(((Answers & qaResume) == 0) || ((Answers & qaCustom) == 0));
    Buttons << mbHelp;
  }

  assert(!Buttons.Empty());

  TForm * Dialog = CreateMessageDialog(Msg, DlgType, Buttons);

  try
  {
    if (Answers & qaSkip)
    {
      TButton * IgnoreButton = (TButton*)(Dialog->FindComponent("Ignore"));
      assert(IgnoreButton);
      IgnoreButton->Caption = LoadStr(SKIP_BUTTON);
    }

    if ((Answers & qaResume) || (Answers & qaCustom))
    {
      TButton * HelpButton = (TButton*)(Dialog->FindComponent("Help"));
      assert(HelpButton);
      if (Answers & qaResume)
      {
        HelpButton->Caption = LoadStr(RESUME_BUTTON);
        HelpButton->ModalResult = mrResume;
      }
      else
      {
        HelpButton->Name = "Custom";
        HelpButton->ModalResult = mrCustom;
      }
    }

    // temporary fix of accelerators (&Abort vs. &All/Yes to &All)
    // must be removed
    for (int Index = 0; Index < Dialog->ComponentCount; Index++)
    {
      TButton * B = (TButton *)Dialog->Components[Index];
      if (B->InheritsFrom(__classid(TButton)))
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
int ExecuteMessageDialog(TForm * Dialog, int Answers, int Params)
{
  int Result, Answer;
  FlashOnBackground();
  Result = Dialog->ShowModal();

  switch (Result) {
    #define MAP_RESULT(RESULT) case mr ## RESULT: Answer = qa ## RESULT; break;
    MAP_RESULT(Yes);
    MAP_RESULT(No);
    MAP_RESULT(Cancel);
    MAP_RESULT(Abort);
    MAP_RESULT(Retry);
    MAP_RESULT(All);
    MAP_RESULT(NoToAll);
    MAP_RESULT(YesToAll);

    MAP_RESULT(Resume);
    MAP_RESULT(Custom);
    #undef MAP_RESULT

    case mrOk:
      Answer = qaOK;
      break;

    case mrIgnore:
      if (Answers & qaSkip)
      {
        Answer = qaSkip;
      }
      else
      {
        Answer = qaIgnore;
      }
      break;

    /*case mrHelp:
      if (Answers & qaResume)
      {
        Answer = qaResult;
      }
      else if (Answers & qaCustom)
      {
        Answer = qaCustom;
      }
      else
      {
        assert(false);
        Answer = qaCustom;
      }
      break;*/
  }

  if (Params & mpNeverAskAgainCheck)
  {
    TCheckBox * NeverAskAgainCheck =
      (TCheckBox *)(Dialog->FindComponent("NeverAskAgainCheck"));
    assert(NeverAskAgainCheck);

    if (NeverAskAgainCheck->Checked &&
        (Answer == qaYes || Answer == qaOK || Answer == qaYesToAll))
    {
      Answer = qaNeverAskAgain;
    }
  }

  TMoreButton * MoreButton = (TMoreButton *)(Dialog->FindComponent("Custom"));
  if (MoreButton)
  {
    // store state even when user selects 'Cancel'?
    Configuration->ErrorDialogExpanded = MoreButton->Expanded;
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
      TButton * CustomButton = (TButton*)(Dialog->FindComponent("Custom"));
      TLabel * MsgLabel = (TLabel*)(Dialog->FindComponent("Message"));
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
      MoreButton->Expanded = Configuration->ErrorDialogExpanded;
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
    TButton * RetryButton = (TButton *)(Dialog->FindComponent("Retry"));
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
int GetSessionPassword(AnsiString Prompt, AnsiString & Password)
{
  return DoPasswordDialog(Prompt, Password);
}
