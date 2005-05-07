// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Anim.pas' rev: 6.00

#ifndef TB2AnimHPP
#define TB2AnimHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Classes.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2anim
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum TB2Anim__1 { tbadLeft, tbadRight, tbadDown, tbadUp };
#pragma option pop

typedef Set<TB2Anim__1, tbadLeft, tbadUp>  TTBAnimationDirection;

//-- var, const, procedure ---------------------------------------------------
static const Word WM_TB2K_STEPANIMATION = 0x955;
static const Word WM_TB2K_ANIMATIONENDED = 0x956;
extern PACKAGE void __fastcall TBEndAnimation(const HWND Wnd);
extern PACKAGE void __fastcall TBStartAnimation(const HWND AWnd, const int ATime, const bool ABlend, const TTBAnimationDirection ADirection);
extern PACKAGE void __fastcall TBStepAnimation(const Messages::TMessage &Msg);

}	/* namespace Tb2anim */
using namespace Tb2anim;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Anim
