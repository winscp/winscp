//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Exceptions.h"
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
  if (E)
  {
  	if (!FMoreMessages) FMoreMessages = new TStringList();

    if (E->InheritsFrom(__classid(ExtException)) &&
    		((ExtException*)E)->MoreMessages)
    {
    	FMoreMessages->Assign(((ExtException*)E)->MoreMessages);
    }

    if (!E->Message.IsEmpty())
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
