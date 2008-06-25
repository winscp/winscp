//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Common.h"
#include "Exceptions.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception * E) :
  Exception("")
{
  AddMoreMessages(E);
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception* E, AnsiString Msg):
        Exception(Msg)
{
  AddMoreMessages(E);
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(AnsiString Msg, Exception* E) :
  Exception("")
{
  // "copy exception"
  AddMoreMessages(E);
  // and append message to the end to more messages
  if (!Msg.IsEmpty())
  {
    if (Message.IsEmpty())
    {
      Message = Msg;
    }
    else
    {
      if (FMoreMessages == NULL)
      {
        FMoreMessages = new TStringList();
      }
      FMoreMessages->Append(Msg);
    }
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception* E, int Ident):
        Exception(Ident)
{
  AddMoreMessages(E);
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(AnsiString Msg, AnsiString MoreMessages) :
  Exception(Msg)
{
  if (!MoreMessages.IsEmpty())
  {
    FMoreMessages = new TStringList();
    FMoreMessages->Text = MoreMessages;
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(AnsiString Msg, TStrings* MoreMessages,
  bool Own) :
  Exception(Msg)
{
  if (Own)
  {
    FMoreMessages = MoreMessages;
  }
  else
  {
    FMoreMessages = new TStringList();
    FMoreMessages->Assign(MoreMessages);
  }
}
//---------------------------------------------------------------------------
void __fastcall ExtException::AddMoreMessages(Exception* E)
{
  if (E != NULL)
  {
    if (FMoreMessages == NULL)
    {
      FMoreMessages = new TStringList();
    }

    ExtException * ExtE = dynamic_cast<ExtException *>(E);
    if ((ExtE != NULL) &&
        (ExtE->MoreMessages != NULL))
    {
      FMoreMessages->Assign(ExtE->MoreMessages);
    }

    AnsiString Msg;
    if (dynamic_cast<EAccessViolation*>(E) != NULL)
    {
      Msg = LoadStr(ACCESS_VIOLATION_ERROR);
    }
    else if (!E->Message.IsEmpty() && (dynamic_cast<EAbort *>(E) == NULL))
    {
      Msg = E->Message;
    }

    // new exception does not have own message, this is in fact duplication of
    // the exception data, but the exception class may being changed
    if (Message.IsEmpty())
    {
      Message = Msg;
    }
    else if (!Msg.IsEmpty())
    {
      FMoreMessages->Insert(0, Msg);
    }

    if (FMoreMessages->Count == 0)
    {
      delete FMoreMessages;
      FMoreMessages = NULL;
    }
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::~ExtException()
{
  delete FMoreMessages;
}
//---------------------------------------------------------------------------
AnsiString __fastcall LastSysErrorMessage()
{
  int LastError = GetLastError();
  AnsiString Result;
  if (LastError != 0)
  {
    Result = FORMAT(Sysconst_SOSError, (LastError, SysErrorMessage(LastError)));
  }
  return Result;
}
//---------------------------------------------------------------------------
__fastcall EOSExtException::EOSExtException(AnsiString Msg) :
  ExtException(Msg, LastSysErrorMessage())
{
}
