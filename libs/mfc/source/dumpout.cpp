// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#include "stdafx.h"
#include <stdarg.h>

#ifdef _DEBUG   // entire file

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Helper routines that can be called from debugger

void AFXAPI AfxDump(const CObject* pOb)
{
	afxDump << pOb;
}

/////////////////////////////////////////////////////////////////////////////
// Diagnostic Trace

void AFX_CDECL AfxTrace(LPCTSTR lpszFormat, ...)
{
#ifdef _DEBUG // all AfxTrace output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return;
#endif

	va_list args;
	va_start(args, lpszFormat);

	int nBuf;
	TCHAR szBuffer[512];

	nBuf = _vsntprintf(szBuffer, _countof(szBuffer), lpszFormat, args);

	// was there an error? was the expanded string too long?
	ASSERT(nBuf >= 0);

	if ((afxTraceFlags & traceMultiApp) && (AfxGetApp() != NULL))
		afxDump << AfxGetApp()->m_pszExeName << ": ";
	afxDump << szBuffer;

	va_end(args);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
