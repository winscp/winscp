// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'MaskSearch.pas' rev: 6.00

#ifndef MaskSearchHPP
#define MaskSearchHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Masksearch
{
//-- type declarations -------------------------------------------------------
//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall BuildMask(AnsiString Str, Classes::TStringList* MaskList);
extern PACKAGE bool __fastcall FileMatches(AnsiString AFile, Classes::TStringList* MaskList);

}	/* namespace Masksearch */
using namespace Masksearch;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// MaskSearch
