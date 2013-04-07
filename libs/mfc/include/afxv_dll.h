// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// special header for _AFXDLL variant.

// default AFX_XXX_DATA and AFX_XXX_DATADEF macros for using MFC DLLs

#ifndef AFX_CORE_DATA
	#define AFX_CORE_DATA       AFX_DATA_IMPORT
	#define AFX_CORE_DATADEF
#endif

#ifndef AFX_OLE_DATA
	#define AFX_OLE_DATA        AFX_DATA_IMPORT
	#define AFX_OLE_DATADEF
#endif

#ifndef AFX_DB_DATA
	#define AFX_DB_DATA         AFX_DATA_IMPORT
	#define AFX_DB_DATADEF
#endif

#ifndef AFX_NET_DATA
	#define AFX_NET_DATA        AFX_DATA_IMPORT
	#define AFX_NET_DATADEF
#endif

// default AFX_EXT_DATA and AFX_EXT_DATADEF macros for using or
//  creating MFC extension DLLs, depending on _AFX_EXT_IMPL
// AFX_EXT_CLASS can be used to import or export entire classes
//  in an extension DLL without the hassle of creating a .DEF file
//  with decorated names.

#ifndef AFX_EXT_DATA
	#ifdef _AFXEXT
		#define AFX_EXT_CLASS       AFX_CLASS_EXPORT
		#define AFX_EXT_API         AFX_API_EXPORT
		#define AFX_EXT_DATA        AFX_DATA_EXPORT
		#define AFX_EXT_DATADEF
	#else
		#define AFX_EXT_CLASS       AFX_CLASS_IMPORT
		#define AFX_EXT_API         AFX_API_IMPORT
		#define AFX_EXT_DATA        AFX_DATA_IMPORT
		#define AFX_EXT_DATADEF
	#endif
#endif
