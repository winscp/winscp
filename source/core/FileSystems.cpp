//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "FileSystems.h"
#include "RemoteFiles.h"
#include "Common.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
__fastcall TCustomFileSystem::TCustomFileSystem(TTerminal * ATerminal):
  FTerminal(ATerminal)
{
  DebugAssert(FTerminal);
}
//---------------------------------------------------------------------------
__fastcall TCustomFileSystem::~TCustomFileSystem()
{
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TCustomFileSystem::GetHomeDirectory()
{
  throw Exception(L"Not implemented");
}
