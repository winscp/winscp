//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include <Interface.h>
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
void __fastcall ShowExtendedException(Exception* E, TObject* Sender)
{
  HandleExtendedException(E, Sender);
}
//---------------------------------------------------------------------------
void __fastcall HandleExtendedException(Exception* E, TObject* Sender)
{
  Application->ShowException(E);
}
//---------------------------------------------------------------------------
void ConfirmHostKey(int UnknownKey, char *host, int port, char *keytype,
  char *keystr, char *fingerprint)
{
}
