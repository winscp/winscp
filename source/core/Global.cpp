//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "Global.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
//---------------------------------------------------------------------------
const UnicodeString EmptyString(TraceInitStr(L"\1\1\1")); // magic
//---------------------------------------------------------------------------
UnicodeString NormalizeString(const UnicodeString & S)
{
  UnicodeString Result = S;
  if (Result == EmptyString)
  {
    Result = UnicodeString();
  }
  return Result;
}
//---------------------------------------------------------------------------
UnicodeString DenormalizeString(const UnicodeString & S)
{
  UnicodeString Result = S;
  if (Result.IsEmpty())
  {
    Result = EmptyString;
  }
  return Result;
}
//---------------------------------------------------------------------------
// TGuard
//---------------------------------------------------------------------------
__fastcall TGuard::TGuard(TCriticalSection * ACriticalSection) :
  FCriticalSection(ACriticalSection)
{
  DebugAssert(ACriticalSection != NULL);
  FCriticalSection->Enter();
}
//---------------------------------------------------------------------------
__fastcall TGuard::~TGuard()
{
  FCriticalSection->Leave();
}
//---------------------------------------------------------------------------
// TUnguard
//---------------------------------------------------------------------------
__fastcall TUnguard::TUnguard(TCriticalSection * ACriticalSection) :
  FCriticalSection(ACriticalSection)
{
  DebugAssert(ACriticalSection != NULL);
  FCriticalSection->Leave();
}
//---------------------------------------------------------------------------
__fastcall TUnguard::~TUnguard()
{
  FCriticalSection->Enter();
}
//---------------------------------------------------------------------------
