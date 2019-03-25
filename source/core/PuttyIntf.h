//---------------------------------------------------------------------------
#ifndef PuttyIntfH
#define PuttyIntfH
//---------------------------------------------------------------------------
void __fastcall PuttyInitialize();
void __fastcall PuttyFinalize();
//---------------------------------------------------------------------------
void __fastcall DontSaveRandomSeed();
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
// Defined in misc.h - Conflicts with std::min/max
#undef min
#undef max
// Defined in marshal.h - Conflicts with xml.xmldom.hpp
#undef get_data
}
//---------------------------------------------------------------------------
#endif
