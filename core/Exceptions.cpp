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

    if (dynamic_cast<EAccessViolation*>(E) != NULL)
    {
      FMoreMessages->Insert(0, LoadStr(ACCESS_VIOLATION_ERROR));
    }
    else if (!E->Message.IsEmpty() && (dynamic_cast<EAbort *>(E) == NULL))
    {
      FMoreMessages->Insert(0, E->Message);
    }
  }
}
//---------------------------------------------------------------------------
__fastcall ExtException::~ExtException()
{
  delete FMoreMessages;
}
