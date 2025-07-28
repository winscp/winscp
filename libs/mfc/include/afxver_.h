// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// afxver_.h - target version/configuration control

/////////////////////////////////////////////////////////////////////////////
// Master version numbers

#define _AFX     1      // Microsoft Application Framework Classes
#ifndef _MFC_VER
#define _MFC_VER 0x0600 // Microsoft Foundation Classes version 6.00, VC++ 6.0
#endif

/////////////////////////////////////////////////////////////////////////////

#define _AFX_ENABLE_INLINES

/////////////////////////////////////////////////////////////////////////////
// special include files

#ifndef AFX_INLINE
	#define AFX_INLINE inline /*__forceinline*/
#endif

#include <afxv_w32.h>

#if defined (__BORLANDC__)
#define _AFX_PACKING 8      // __BORLANDC__
#else
// setup default packing value
#define _AFX_PACKING    4   // default packs structs at 4 bytes
#endif

/////////////////////////////////////////////////////////////////////////////
// Standard preprocessor symbols if not already defined
/////////////////////////////////////////////////////////////////////////////

// PASCAL is used for static member functions
#ifndef PASCAL
	#define PASCAL  __stdcall
#endif

// FASTCALL is used for static member functions with little or no params
#ifndef FASTCALL
	#define FASTCALL __fastcall
#endif

// CDECL and EXPORT are defined in case WINDOWS.H doesn't
#ifndef CDECL
	#define CDECL __cdecl
#endif

#ifndef EXPORT
	#define EXPORT
#endif

// AFXAPI is used on global public functions
#ifndef AFXAPI
	#define AFXAPI __stdcall
#endif

// AFX_CDECL is used for rare functions taking variable arguments
#ifndef AFX_CDECL
	#define AFX_CDECL __cdecl
#endif

#ifndef AFX_STATIC
	#define AFX_STATIC extern
	#define AFX_STATIC_DATA extern __declspec(selectany)
#endif

// The following macros are used to enable export/import

// This macro is used to reduce size requirements of some classes
#ifndef AFX_ALWAYS_VTABLE
#ifndef AFX_NOVTABLE
#if _MSC_VER >= 1100
#define AFX_NOVTABLE __declspec(novtable)
#else
#define AFX_NOVTABLE
#endif
#endif
#endif

// for global data that should be in COMDATs (packaged data)
#ifndef AFX_COMDAT
	#define AFX_COMDAT
#endif

/////////////////////////////////////////////////////////////////////////////
