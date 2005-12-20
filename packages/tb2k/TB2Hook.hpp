// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Hook.pas' rev: 6.00

#ifndef TB2HookHPP
#define TB2HookHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2hook
{
//-- type declarations -------------------------------------------------------
#pragma option push -b-
enum THookProcCode { hpSendActivate, hpSendActivateApp, hpSendWindowPosChanged, hpPreDestroy, hpGetMessage };
#pragma option pop

typedef Set<THookProcCode, hpSendActivate, hpGetMessage>  THookProcCodes;

typedef void __fastcall (*THookProc)(THookProcCode Code, HWND Wnd, int WParam, int LParam);

//-- var, const, procedure ---------------------------------------------------
extern PACKAGE void __fastcall InstallHookProc(System::TObject* AUser, THookProc AProc, THookProcCodes ACodes);
extern PACKAGE void __fastcall UninstallHookProc(System::TObject* AUser, THookProc AProc);

}	/* namespace Tb2hook */
using namespace Tb2hook;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Hook
