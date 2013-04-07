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
// Default swap tuning for AFX library

#define _TEXTSEG(name)  ".text$" #name

// Most segments are tuned via function order list (DLL version)
#ifndef _AFX_FUNCTION_ORDER
#define AFX_CORE1_SEG   _TEXTSEG(AFX_CORE1) // core functionality
#define AFX_CORE2_SEG   _TEXTSEG(AFX_CORE2) // more core functionality
#define AFX_CORE3_SEG   _TEXTSEG(AFX_CORE3) // more core functionality
#define AFX_CORE4_SEG   _TEXTSEG(AFX_CORE4) // more core functionality
#define AFX_AUX_SEG     _TEXTSEG(AFX_AUX)   // auxilliary functionality
#define AFX_CMNCTL_SEG  _TEXTSEG(AFX_CMNCTL)// most common controls
#define AFX_COLL_SEG    _TEXTSEG(AFX_COL1)  // collections
#define AFX_COLL2_SEG   _TEXTSEG(AFX_COL2)  // more collections
#define AFX_INET_SEG    _TEXTSEG(AFX_INET)  // Internet client-side stuff
#define AFX_OLE_SEG     _TEXTSEG(AFX_OLE1)  // OLE support
#define AFX_OLE2_SEG    _TEXTSEG(AFX_OLE2)  // more OLE support
#define AFX_OLE3_SEG    _TEXTSEG(AFX_OLE3)  // and more OLE support
#define AFX_OLE4_SEG    _TEXTSEG(AFX_OLE4)  // and more OLE support
#define AFX_OLE5_SEG    _TEXTSEG(AFX_OLE5)  // and even more OLE support
#define AFX_OLERA_SEG   _TEXTSEG(AFX_OLERA) // (reserved for future use)
#define AFX_PRINT_SEG   _TEXTSEG(AFX_PRNT)  // Printing functionality
#define AFX_DBG1_SEG    _TEXTSEG(AFX_DBG1)  // inlines go here in _DEBUG
#define AFX_DBG2_SEG    _TEXTSEG(AFX_DBG2)  // inlines go here in _DEBUG
#define AFX_VDEL_SEG    _TEXTSEG(AFX_VDEL)  // vector deleting destructors
#define AFX_TERM_SEG    _TEXTSEG(AFX_TERM)  // cleanup routines
#define AFX_MAPI_SEG    _TEXTSEG(AFX_MAPI)  // simple MAPI support
#define AFX_SOCK_SEG    _TEXTSEG(AFX_SOCK)  // windows sockets support
#else
#define AFX_CORE1_SEG                       // core functionality
#define AFX_CORE2_SEG                       // more core functionality
#define AFX_CORE3_SEG                       // more core functionality
#define AFX_CORE4_SEG                       // more core functionality
#define AFX_AUX_SEG                         // auxilliary functionality
#define AFX_CMNCTL_SEG                      // most common controls
#define AFX_COLL_SEG                        // collections
#define AFX_COLL2_SEG                       // more collections
#define AFX_INET_SEG                        // Internet client-side stuff
#define AFX_OLE_SEG                         // OLE support
#define AFX_OLE2_SEG                        // more OLE support
#define AFX_OLE3_SEG                        // and more OLE support
#define AFX_OLE4_SEG                        // and more OLE support
#define AFX_OLE5_SEG                        // and even more OLE support
#define AFX_OLERA_SEG                       // (reserved for future use)
#define AFX_PRINT_SEG                       // Printing functionality
#define AFX_DBG1_SEG                        // inlines go here in _DEBUG
#define AFX_DBG2_SEG                        // inlines go here in _DEBUG
#define AFX_VDEL_SEG                        // vector deleting destructors
#define AFX_TERM_SEG                        // cleanup routines
#define AFX_MAPI_SEG                        // simple MAPI support
#define AFX_SOCK_SEG                        // windows sockets support
#endif

// AFX_INIT_SEG is hand tuned even in DLL version
#define AFX_INIT_SEG    _TEXTSEG(AFX_INIT)  // initialization

/////////////////////////////////////////////////////////////////////////////
// turn off reference tracking for certain often used symbols

#ifndef _AFX_PORTABLE
#pragma component(browser, off, references, "ASSERT")
#pragma component(browser, off, references, "AfxAssertFailedLine")
#pragma component(browser, off, references, "AfxDebugBreak")
#pragma component(browser, off, references, "BOOL")
#pragma component(browser, off, references, "BYTE")
#pragma component(browser, off, references, "DECLSPEC_IMPORT")
#pragma component(browser, off, references, "DWORD")
#pragma component(browser, off, references, "FALSE")
#pragma component(browser, off, references, "FAR")
#pragma component(browser, off, references, "LPSTR")
#pragma component(browser, off, references, "LPTSTR")
#pragma component(browser, off, references, "LPCSTR")
#pragma component(browser, off, references, "LPCTSTR")
#pragma component(browser, off, references, "NULL")
#pragma component(browser, off, references, "PASCAL")
#pragma component(browser, off, references, "THIS_FILE")
#pragma component(browser, off, references, "TRUE")
#pragma component(browser, off, references, "UINT")
#pragma component(browser, off, references, "WINAPI")
#pragma component(browser, off, references, "WORD")
#endif  //!_AFX_PORTABLE

/////////////////////////////////////////////////////////////////////////////
// For target version (one of)
//   _CUSTOM   : for custom configurations (causes afxv_cfg.h to be included)
//
// Additional build options:
//  _DEBUG              debug versions (full diagnostics)
//  _AFXDLL             use shared MFC DLL
//  _AFXEXT             extension DLL version, implies _AFXDLL
//  _USRDLL             create regular DLL (_AFXDLL is valid too)
//

#ifndef _DEBUG
	#define _AFX_ENABLE_INLINES
#endif

#define _AFX_NO_NESTED_DERIVATION

/////////////////////////////////////////////////////////////////////////////
// Special configurations

// _AFXEXT implies _AFXDLL
#if defined(_AFXEXT) && !defined(_AFXDLL)
	#define _AFXDLL
#endif

#ifdef __BORLANDC__
#  if defined(_AFXDLL) && !defined(_RTLDLL)
#    error The dynamic MFC library requires the dynamic RTL.
#  endif
#else
#  if defined(_AFXDLL) && !defined(_DLL)
#    error Please use the /MD switch for _AFXDLL builds
#  endif
#endif

#if defined(_AFXDLL) && !defined(_MT)
	#error Please use the /MD switch (multithreaded DLL C-runtime)
#endif

/////////////////////////////////////////////////////////////////////////////
// special include files

#ifndef AFX_INLINE
	#define AFX_INLINE inline /*__forceinline*/
#endif

#include <afxv_w32.h>

// Include any non-Intel platform specific items
#ifndef _X86_
	#include <afxv_cpu.h>
#endif

#ifdef _X86_
	#define _AFX_MINREBUILD
#endif

#if defined (_CUSTOM) || defined (__BORLANDC__)
// Put any custom configuration items in afxv_cfg.h
	#include <afxv_cfg.h>
#endif

// setup default packing value
#ifndef _AFX_PACKING
	#define _AFX_PACKING    4   // default packs structs at 4 bytes
#endif

#ifdef _AFXDLL
	#include <afxv_dll.h>
#endif

// Define this virtual key for use by status bar
#ifndef VK_KANA
#define VK_KANA             0x15
#endif

/////////////////////////////////////////////////////////////////////////////
// Special AfxDebugBreak: used to break into debugger at critical times

#ifndef AfxDebugBreak
#ifdef _AFX_NO_DEBUG_CRT
// by default, debug break is asm int 3, or a call to DebugBreak, or nothing
#if defined(_M_IX86) && !defined(_AFX_PORTABLE)
#define AfxDebugBreak() _asm { int 3 }
#else
#define AfxDebugBreak() DebugBreak()
#endif
#else
#define AfxDebugBreak() _CrtDbgBreak()
#endif
#endif

#ifndef _DEBUG
#ifdef AfxDebugBreak
#undef AfxDebugBreak
#endif
#define AfxDebugBreak()
#endif  // _DEBUG

/////////////////////////////////////////////////////////////////////////////
// Standard preprocessor symbols if not already defined
/////////////////////////////////////////////////////////////////////////////

// SIZE_T_MAX is used by the collection classes
#ifndef SIZE_T_MAX
	#define SIZE_T_MAX  UINT_MAX
#endif

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

// UNALIGNED is used for unaligned data access (in CArchive mostly)
#ifndef UNALIGNED
	#define UNALIGNED
#endif

// AFXAPI is used on global public functions
#ifndef AFXAPI
	#define AFXAPI __stdcall
#endif

// AFXOLEAPI is used for some special OLE functions
#ifndef AFXOLEAPI
	#define AFXOLEAPI __stdcall
#endif

// AFX_CDECL is used for rare functions taking variable arguments
#ifndef AFX_CDECL
	#define AFX_CDECL __cdecl
#endif

// AFX_EXPORT is used for functions which need to be exported
#ifndef AFX_EXPORT
	#define AFX_EXPORT EXPORT
#endif

#ifndef AFX_STATIC
	#define AFX_STATIC extern
	#define AFX_STATIC_DATA extern __declspec(selectany)
#endif

// The following macros are used to enable export/import

// for data
#ifndef AFX_DATA_EXPORT
	#define AFX_DATA_EXPORT __declspec(dllexport)
#endif
#ifndef AFX_DATA_IMPORT
	#define AFX_DATA_IMPORT __declspec(dllimport)
#endif

// for classes
#ifndef AFX_CLASS_EXPORT
	#define AFX_CLASS_EXPORT __declspec(dllexport)
#endif
#ifndef AFX_CLASS_IMPORT
	#define AFX_CLASS_IMPORT __declspec(dllimport)
#endif

// for global APIs
#ifndef AFX_API_EXPORT
	#define AFX_API_EXPORT __declspec(dllexport)
#endif
#ifndef AFX_API_IMPORT
	#define AFX_API_IMPORT __declspec(dllimport)
#endif

// This macro is used to reduce size requirements of some classes
#ifndef AFX_ALWAYS_VTABLE
#ifndef AFX_NOVTABLE
#if _MSC_VER >= 1100 && !defined(_DEBUG)
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

// The following macros are used on data declarations/definitions
//  (they are redefined for extension DLLs and the shared MFC DLL)
#define AFX_DATA
#define AFX_DATADEF
#define AFX_API

// used when building the "core" MFC42.DLL
#ifndef AFX_CORE_DATA
	#define AFX_CORE_DATA
	#define AFX_CORE_DATADEF
#endif

// used when building the MFC/OLE support MFCO42.DLL
#ifndef AFX_OLE_DATA
	#define AFX_OLE_DATA
	#define AFX_OLE_DATADEF
#endif

// used when building the MFC/DB support MFCD42.DLL
#ifndef AFX_DB_DATA
	#define AFX_DB_DATA
	#define AFX_DB_DATADEF
#endif

// used when building the MFC/NET support MFCN42.DLL
#ifndef AFX_NET_DATA
	#define AFX_NET_DATA
	#define AFX_NET_DATADEF
#endif

// used when building extension DLLs
#ifndef AFX_EXT_DATA
	#define AFX_EXT_DATA
	#define AFX_EXT_DATADEF
	#define AFX_EXT_CLASS
	#define AFX_EXT_API
#endif

// BASED_XXXX macros are provided for backward compatibility
#ifndef BASED_CODE
	#define BASED_CODE
#endif

#ifndef BASED_DEBUG
	#define BASED_DEBUG
#endif

#ifndef BASED_STACK
	#define BASED_STACK
#endif

// setup default code segment
#ifdef AFX_DEF_SEG
	#pragma code_seg(AFX_DEF_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
