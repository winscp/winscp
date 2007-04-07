//---------------------------------------------------------------------------
#ifndef PuttyIntfH
#define PuttyIntfH
//---------------------------------------------------------------------------
void __fastcall PuttyInitialize();
void __fastcall PuttyFinalize();
//---------------------------------------------------------------------------
void __fastcall DontSaveRandomSeed();
//---------------------------------------------------------------------------
int __fastcall ProtocolByName(const AnsiString & Name);
AnsiString __fastcall ProtocolName(int Protocol);
//---------------------------------------------------------------------------
#include "PuttyTools.h"
//---------------------------------------------------------------------------
#define MPEXT
extern "C"
{
#include <putty.h>
#include <puttyexp.h>
#include <ssh.h>
#include <proxy.h>
#include <storage.h>
}
//---------------------------------------------------------------------------
#endif
