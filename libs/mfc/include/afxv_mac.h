// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// afxv_mac.h - target version/configuration control for Macintosh OS

#if !defined(_MAC)
	#error afxv_mac.h is used only for Macintosh-targeted builds
#endif

#if !defined(_M_M68K) && !defined(_M_MPPC)
	#error afxv_mac.h is used only for Motorola M68000 and Motorola PowerPC builds
#endif

#define SystemSevenOrLater 1

#define _beginthreadex(p1, p2, p3, p4, p5, p6)  NULL
#define _endthreadex(p1)

// wcslen is defined in wlm
extern "C" size_t WINAPI wcslen(const wchar_t*);

#ifdef _68K_
	#define _AFX_NO_DEBUG_CRT
	#define _AFX_NO_SOCKET_SUPPORT
#endif

#define _AFX_NO_SYNC_SUPPORT
#define _AFX_NO_DAO_SUPPORT
#ifndef MACOCX
	#define _AFX_NO_OCX_SUPPORT
	#define _AFX_NO_OCC_SUPPORT
#endif
#define _AFX_NO_DOCOBJECT_SUPPORT
#define _AFX_NO_ATLSERVER_SUPPORT
#define OLE2ANSI

#ifdef _AFX_NO_DEBUG_CRT
#ifdef _68K_
	pascal void _AfxDebugBreak(void) = 0xA9FF;
	#define AfxDebugBreak() _AfxDebugBreak()
#else
	extern "C" pascal void Debugger(void);
	#define AfxDebugBreak() Debugger()
#endif
#endif

/////////////////////////////////////////////////////////////////////////////
