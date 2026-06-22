// $jrsoftware: tb2k.cpp,v 1.2 2002/11/14 18:07:19 jr Exp $
//---------------------------------------------------------------------------
#pragma warn -pch // WORKAROUND (see My.cpp)
#include <vcl.h>
#pragma hdrstop
#pragma package(smart_init)
//---------------------------------------------------------------------------
//   Package source.
//---------------------------------------------------------------------------
int WINAPI DllEntryPoint(HINSTANCE /*hinst*/, unsigned long /*reason*/, void*)
{
    return 1;
}
//---------------------------------------------------------------------------
