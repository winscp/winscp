//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Exceptions.h"
#include "TextsCore.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall ExtException::ExtException(Exception* E, AnsiString Msg):
        Exception(Msg)
{
  AddMoreMessages(E);
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
