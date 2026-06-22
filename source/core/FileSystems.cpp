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
  NotImplemented();
  UNREACHABLE_AFTER_NORETURN(return EmptyStr);
}
//---------------------------------------------------------------------------
UnicodeString TCustomFileSystem::CalculateFilesChecksumInitialize(const UnicodeString & DebugUsedArg(Alg))
{
  NotImplemented();
  UNREACHABLE_AFTER_NORETURN(return EmptyStr);
}
//---------------------------------------------------------------------------
void __fastcall TCustomFileSystem::TransferOnDirectory(
  const UnicodeString & Directory, const TCopyParamType *, int Params)
{
  DebugUsedParam2(Directory, Params);
}
//---------------------------------------------------------------------------
void __fastcall TCustomFileSystem::DirectorySunk(
  const UnicodeString & DestFullName, const TRemoteFile *, const TCopyParamType *)
{
  DebugUsedParam(DestFullName);
}
