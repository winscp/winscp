//---------------------------------------------------------------------------
#pragma warn -pch // WORKAROUND (initialized data in header in xlocnum)
#include <basepch.h>
#pragma hdrstop
#pragma package(smart_init)
//---------------------------------------------------------------------------

//   Package source.
//---------------------------------------------------------------------------


#pragma argsused
int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
  return 1;
}
//---------------------------------------------------------------------------
