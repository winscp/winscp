// This is a part of the Active Template Library.
// Copyright (C) 1996-1997 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Active Template Library Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Active Template Library product.

#ifndef __ATLCONV_H__
#define __ATLCONV_H__

#ifndef __cplusplus
	#error ATL requires C++ compilation (use a .cpp suffix)
#endif

#include <malloc.h>

#pragma pack(push,8)

// Make sure MFC's afxconv.h hasn't already been loaded to do this
#ifndef USES_CONVERSION

#define USES_CONVERSION int _convert; _convert

/////////////////////////////////////////////////////////////////////////////
// Global UNICODE<>ANSI translation helpers
LPWSTR WINAPI AtlA2WHelper(LPWSTR lpw, LPCSTR lpa, int nChars);
LPSTR WINAPI AtlW2AHelper(LPSTR lpa, LPCWSTR lpw, int nChars);

#ifndef ATLA2WHELPER
#define ATLA2WHELPER AtlA2WHelper
#define ATLW2AHELPER AtlW2AHelper
#endif

#define A2W(lpa) (\
	(static_cast<LPCSTR>(lpa) == NULL) ? NULL : (\
		_convert = (lstrlenA(lpa)+1),\
		ATLA2WHELPER(static_cast<LPWSTR>(alloca(_convert*2)), lpa, _convert)))

#define W2A(lpw) (\
	(static_cast<LPCWSTR>(lpw) == NULL) ? NULL : (\
		_convert = (lstrlenW(lpw)+1)*2,\
		ATLW2AHELPER(static_cast<LPSTR>(alloca(_convert)), lpw, _convert)))

#define A2CW(lpa) (static_cast<LPCWSTR>(A2W(lpa)))
#define W2CA(lpw) (static_cast<LPCSTR>(W2A(lpw)))

#define T2A W2A
#define A2T A2W
inline LPWSTR T2W(LPTSTR lp) { return lp; }
inline LPTSTR W2T(LPWSTR lp) { return lp; }
#define T2CA W2CA
#define A2CT A2CW
inline LPCWSTR T2CW(LPCTSTR lp) { return lp; }
inline LPCTSTR W2CT(LPCWSTR lp) { return lp; }

#else //!USES_CONVERSION

// if USES_CONVERSION already defined (i.e. MFC_VER < 4.21 )
// flip this switch to avoid atlconv.cpp
#define _ATL_NO_CONVERSIONS

#endif //!USES_CONVERSION

#pragma pack(pop)

#endif // __ATLCONV_H__

/////////////////////////////////////////////////////////////////////////////
