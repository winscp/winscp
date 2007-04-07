// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'UxTheme.pas' rev: 6.00

#ifndef UxThemeHPP
#define UxThemeHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

#include "uxtheme.h"


namespace Uxtheme
{
//-- type declarations -------------------------------------------------------
typedef unsigned HIMAGELIST;

typedef THEMESIZE TThemeSize;

typedef _MARGINS  TMargins;

typedef _INTLIST  TIntList;

typedef PROPERTYORIGIN TPropertyOrigin;

//-- var, const, procedure ---------------------------------------------------
static const Word WM_THEMECHANGED = 0x31a;
extern PACKAGE void __fastcall FreeThemeLibrary(void);
extern PACKAGE bool __fastcall InitThemeLibrary(void);
extern PACKAGE bool __fastcall UseThemes(void);

}	/* namespace Uxtheme */
using namespace Uxtheme;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// UxTheme
