// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXCONV_H__
#define __AFXCONV_H__

#ifndef _OBJBASE_H_
#include <objbase.h>
#endif

/////////////////////////////////////////////////////////////////////////////
// Global UNICODE<>ANSI translation helpers

#ifdef _WINGDI_
LPDEVMODEW AFXAPI AfxDevModeA2W(LPDEVMODEW lpDevModeW, LPDEVMODEA lpDevModeA);
LPDEVMODEA AFXAPI AfxDevModeW2A(LPDEVMODEA lpDevModeA, LPDEVMODEW lpDevModeW);
LPTEXTMETRICW AFXAPI AfxTextMetricA2W(LPTEXTMETRICW lptmW, LPTEXTMETRICA lptmA);
LPTEXTMETRICA AFXAPI AfxTextMetricW2A(LPTEXTMETRICA lptmA, LPTEXTMETRICW lptmW);

#ifndef ATLDEVMODEA2W
#define ATLDEVMODEA2W AfxDevModeA2W
#define ATLDEVMODEW2A AfxDevModeW2A
#define ATLTEXTMETRICA2W AfxTextMetricA2W
#define ATLTEXTMETRICW2A AfxTextMetricW2A
#endif

#endif //_WINGDI

LPWSTR AFXAPI AfxA2WHelper(LPWSTR lpw, LPCSTR lpa, int nChars);
LPSTR AFXAPI AfxW2AHelper(LPSTR lpa, LPCWSTR lpw, int nChars);

#ifndef ATLA2WHELPER
#define ATLA2WHELPER AfxA2WHelper
#define ATLW2AHELPER AfxW2AHelper
#endif

#define OLESTDDELIMOLE OLESTR("\\")

#if defined(_UNICODE)
// in these cases the default (TCHAR) is the same as OLECHAR
	AFX_INLINE LPOLESTR TASKSTRINGT2OLE(LPOLESTR lp) { return lp; }
	AFX_INLINE LPTSTR TASKSTRINGOLE2T(LPOLESTR lp) { return lp; }
	AFX_INLINE BSTR BSTR2TBSTR(BSTR bstr) { return bstr;}
#elif defined(OLE2ANSI)
// in these cases the default (TCHAR) is the same as OLECHAR
	AFX_INLINE LPOLESTR TASKSTRINGT2OLE(LPOLESTR lp) { return lp; }
	AFX_INLINE LPTSTR TASKSTRINGOLE2T(LPOLESTR lp) { return lp; }
	AFX_INLINE BSTR BSTR2TBSTR(BSTR bstr) { return bstr; }
#else
	#define TASKSTRINGT2OLE(lpa)    AfxTaskStringA2W(lpa)
	#define TASKSTRINGOLE2T(lpo) AfxTaskStringW2A(lpo)
	#define BSTR2TBSTR(bstr) AfxBSTR2ABSTR(bstr)
#endif

#include <atlconv.h>

#endif //__AFXCONV_H__
