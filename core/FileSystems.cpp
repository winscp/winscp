//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "SecureShell.h"
#include "FileSystems.h"
#include "RemoteFiles.h"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TCustomFileSystem::TCustomFileSystem(TTerminal * ATerminal):
  TObject(), FTerminal(ATerminal)
{
  assert(FTerminal);
}

