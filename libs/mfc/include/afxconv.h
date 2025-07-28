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

LPWSTR AFXAPI AfxA2WHelper(LPWSTR lpw, LPCSTR lpa, int nChars);
LPSTR AFXAPI AfxW2AHelper(LPSTR lpa, LPCWSTR lpw, int nChars);

#ifndef ATLA2WHELPER
#define ATLA2WHELPER AfxA2WHelper
#define ATLW2AHELPER AfxW2AHelper
#endif

#include <atlconv.h>

#endif //__AFXCONV_H__
