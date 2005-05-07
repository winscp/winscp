// Borland C++ Builder
// Copyright (c) 1995, 2002 by Borland Software Corporation
// All rights reserved

// (DO NOT EDIT: machine generated header) 'TB2Common.pas' rev: 6.00

#ifndef TB2CommonHPP
#define TB2CommonHPP

#pragma delphiheader begin
#pragma option push -w-
#pragma option push -Vx
#include <Forms.hpp>	// Pascal unit
#include <Controls.hpp>	// Pascal unit
#include <Messages.hpp>	// Pascal unit
#include <SysUtils.hpp>	// Pascal unit
#include <Classes.hpp>	// Pascal unit
#include <Windows.hpp>	// Pascal unit
#include <SysInit.hpp>	// Pascal unit
#include <System.hpp>	// Pascal unit

//-- user supplied -----------------------------------------------------------

namespace Tb2common
{
//-- type declarations -------------------------------------------------------
typedef int __fastcall (*TListSortExCompare)(const void * Item1, const void * Item2, const void * ExtraData);

typedef void __fastcall (*THandleWMPrintNCPaintProc)(HWND Wnd, HDC DC, int AppData);

//-- var, const, procedure ---------------------------------------------------
static const Shortint PopupMenuWindowNCSize = 0x3;
static const int DT_HIDEPREFIX = 0x100000;
extern PACKAGE BOOL __stdcall (*TrackMouseEventFunc)(tagTRACKMOUSEEVENT &EventTrack);
extern PACKAGE bool __fastcall ApplicationIsActive(void);
extern PACKAGE void __fastcall ListSortEx(const Classes::TList* List, const TListSortExCompare Compare, const void * ExtraData);
extern PACKAGE void __fastcall HandleWMPrint(const HWND Wnd, Messages::TMessage &Message, const THandleWMPrintNCPaintProc NCPaintFunc, const int AppData);
extern PACKAGE void __fastcall HandleWMPrintClient(const Controls::TWinControl* Control, Messages::TMessage &Message);
extern PACKAGE int __fastcall DivRoundUp(const int Dividend, const int Divisor);
extern PACKAGE int __fastcall GetTextHeight(const HDC DC);
extern PACKAGE AnsiString __fastcall StripAccelChars(const AnsiString S);
extern PACKAGE AnsiString __fastcall EscapeAmpersands(const AnsiString S);
extern PACKAGE AnsiString __fastcall StripTrailingPunctuation(const AnsiString S);
extern PACKAGE int __fastcall GetTextWidth(const HDC DC, AnsiString S, const bool Prefix);
extern PACKAGE void __fastcall ProcessPaintMessages(void);
extern PACKAGE void __fastcall RemoveMessages(const int AMin, const int AMax);
extern PACKAGE void __fastcall SelectNCUpdateRgn(HWND Wnd, HDC DC, HRGN Rgn);
extern PACKAGE bool __fastcall AddToList(Classes::TList* &List, void * Item);
extern PACKAGE bool __fastcall AddToFrontOfList(Classes::TList* &List, void * Item);
extern PACKAGE void __fastcall RemoveFromList(Classes::TList* &List, void * Item);
extern PACKAGE int __fastcall GetMenuShowDelay(void);
extern PACKAGE bool __fastcall AreFlatMenusEnabled(void);
extern PACKAGE bool __fastcall AreKeyboardCuesEnabled(void);
extern PACKAGE HRGN __fastcall CreateNullRegion(void);
extern PACKAGE void __fastcall DrawInvertRect(const HDC DC, const Types::PRect NewRect, const Types::PRect OldRect, const tagSIZE &NewSize, const tagSIZE &OldSize, const HBRUSH Brush, HBRUSH BrushLast);
extern PACKAGE HBRUSH __fastcall CreateHalftoneBrush(void);
extern PACKAGE void __fastcall DrawHalftoneInvertRect(const HDC DC, const Types::PRect NewRect, const Types::PRect OldRect, const tagSIZE &NewSize, const tagSIZE &OldSize);
extern PACKAGE bool __fastcall MethodsEqual(const System::TMethod &M1, const System::TMethod &M2);
extern PACKAGE Types::TRect __fastcall GetRectOfPrimaryMonitor(const bool WorkArea);
extern PACKAGE bool __fastcall UsingMultipleMonitors(void);
extern PACKAGE Types::TRect __fastcall GetRectOfMonitorContainingRect(const Types::TRect &R, const bool WorkArea);
extern PACKAGE Types::TRect __fastcall GetRectOfMonitorContainingPoint(const Types::TPoint &P, const bool WorkArea);
extern PACKAGE Types::TRect __fastcall GetRectOfMonitorContainingWindow(const HWND W, const bool WorkArea);
extern PACKAGE void __fastcall InitTrackMouseEvent(void);
extern PACKAGE bool __fastcall CallTrackMouseEvent(const HWND Wnd, const unsigned Flags);
extern PACKAGE HFONT __fastcall CreateRotatedFont(HDC DC);
extern PACKAGE void __fastcall DrawRotatedText(const HDC DC, AnsiString AText, const Types::TRect &ARect, const unsigned AFormat);
extern PACKAGE bool __fastcall NeedToPlaySound(const AnsiString Alias);
extern PACKAGE int __fastcall Max(int A, int B);
extern PACKAGE int __fastcall Min(int A, int B);
extern PACKAGE char __fastcall FindAccelChar(const AnsiString S);
extern PACKAGE bool __fastcall IsWindowsXP(void);
extern PACKAGE unsigned __fastcall GetInputLocaleCodePage(void);

}	/* namespace Tb2common */
using namespace Tb2common;
#pragma option pop	// -w-
#pragma option pop	// -Vx

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// TB2Common
