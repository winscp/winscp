//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileSystems.h"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TCustomFileSystem::TCustomFileSystem(TTerminal * ATerminal):
  TObject(), FTerminal(ATerminal)
{
  assert(FTerminal);
}

