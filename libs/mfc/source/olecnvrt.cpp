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

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// OLE UNICODE conversion support

void AFXAPI AfxBSTR2CString(CString* pStr, BSTR bstr)
{
	ASSERT(pStr != NULL);
	int nLen = SysStringLen(bstr);
#if defined(_UNICODE) || defined(OLE2ANSI)
	LPTSTR lpsz = pStr->GetBufferSetLength(nLen);
	ASSERT(lpsz != NULL);
	memcpy(lpsz, bstr, nLen*sizeof(TCHAR));
#else
	int nBytes = WideCharToMultiByte(CP_ACP, 0, bstr, nLen, NULL, NULL, NULL,
		NULL);
	LPSTR lpsz = pStr->GetBufferSetLength(nBytes);
	ASSERT(lpsz != NULL);
	WideCharToMultiByte(CP_ACP, 0, bstr, nLen, lpsz, nBytes, NULL, NULL);
#endif
}

#if !defined(_UNICODE) && !defined(OLE2ANSI)
// this function creates a BSTR but it actually has an ANSI string inside
BSTR AFXAPI AfxBSTR2ABSTR(BSTR bstrW)
{
	int nLen = SysStringLen(bstrW); //not including NULL
	int nBytes = WideCharToMultiByte(CP_ACP, 0, bstrW, nLen,
		NULL, NULL, NULL, NULL); //number of bytes not including NULL
	BSTR bstrA = SysAllocStringByteLen(NULL, nBytes); // allocates nBytes
	VERIFY(WideCharToMultiByte(CP_ACP, 0, bstrW, nLen, (LPSTR)bstrA, nBytes, NULL,
		NULL) == nBytes);
	return bstrA;
}

LPWSTR AFXAPI AfxTaskStringA2W(LPCSTR lpa)
{
	LPWSTR lpw = AfxAllocTaskWideString(lpa);
	CoTaskMemFree((void*)lpa);
	return lpw;
}

LPSTR AFXAPI AfxTaskStringW2A(LPCWSTR lpw)
{
	LPSTR lpa = AfxAllocTaskAnsiString(lpw);
	CoTaskMemFree((void*)lpw);
	return lpa;
}

LPDEVMODEW AFXAPI AfxDevModeA2W(LPDEVMODEW lpDevModeW, LPDEVMODEA lpDevModeA)
{
	if (lpDevModeA == NULL)
		return NULL;
	ASSERT(lpDevModeW != NULL);
	AfxA2WHelper(lpDevModeW->dmDeviceName, (LPCSTR)lpDevModeA->dmDeviceName, 32*sizeof(WCHAR));
	memcpy(&lpDevModeW->dmSpecVersion, &lpDevModeA->dmSpecVersion,
		offsetof(DEVMODEW, dmFormName) - offsetof(DEVMODEW, dmSpecVersion));
	AfxA2WHelper(lpDevModeW->dmFormName, (LPCSTR)lpDevModeA->dmFormName, 32*sizeof(WCHAR));
	memcpy(&lpDevModeW->dmLogPixels, &lpDevModeA->dmLogPixels,
		sizeof(DEVMODEW) - offsetof(DEVMODEW, dmLogPixels));
	if (lpDevModeA->dmDriverExtra != 0)
		memcpy(lpDevModeW+1, lpDevModeA+1, lpDevModeA->dmDriverExtra);
	lpDevModeW->dmSize = sizeof(DEVMODEW);
	return lpDevModeW;
}

LPDEVMODEA AFXAPI AfxDevModeW2A(LPDEVMODEA lpDevModeA, LPDEVMODEW lpDevModeW)
{
	if (lpDevModeW == NULL)
		return NULL;
	ASSERT(lpDevModeA != NULL);
	AfxW2AHelper((LPSTR)lpDevModeA->dmDeviceName, lpDevModeW->dmDeviceName, 32*sizeof(char));
	memcpy(&lpDevModeA->dmSpecVersion, &lpDevModeW->dmSpecVersion,
		offsetof(DEVMODEA, dmFormName) - offsetof(DEVMODEA, dmSpecVersion));
	AfxW2AHelper((LPSTR)lpDevModeA->dmFormName, lpDevModeW->dmFormName, 32*sizeof(char));
	memcpy(&lpDevModeA->dmLogPixels, &lpDevModeW->dmLogPixels,
		sizeof(DEVMODEA) - offsetof(DEVMODEA, dmLogPixels));
	if (lpDevModeW->dmDriverExtra != 0)
		memcpy(lpDevModeA+1, lpDevModeW+1, lpDevModeW->dmDriverExtra);
	lpDevModeA->dmSize = sizeof(DEVMODEA);
	return lpDevModeA;
}

LPTEXTMETRICW AFXAPI AfxTextMetricA2W(LPTEXTMETRICW lptmW, LPTEXTMETRICA lptmA)
{
	if (lptmA == NULL)
		return NULL;
	ASSERT(lptmW != NULL);
	memcpy(lptmW, lptmA, sizeof(LONG) * 11);
	memcpy(&lptmW->tmItalic, &lptmA->tmItalic, sizeof(BYTE) * 5);
	MultiByteToWideChar(CP_ACP, 0, (LPCSTR)&lptmA->tmFirstChar, 1, &lptmW->tmFirstChar, 1);
	MultiByteToWideChar(CP_ACP, 0, (LPCSTR)&lptmA->tmLastChar, 1, &lptmW->tmLastChar, 1);
	MultiByteToWideChar(CP_ACP, 0, (LPCSTR)&lptmA->tmDefaultChar, 1, &lptmW->tmDefaultChar, 1);
	MultiByteToWideChar(CP_ACP, 0, (LPCSTR)&lptmA->tmBreakChar, 1, &lptmW->tmBreakChar, 1);
	return lptmW;
}

LPTEXTMETRICA AFXAPI AfxTextMetricW2A(LPTEXTMETRICA lptmA, LPTEXTMETRICW lptmW)
{
	if (lptmW == NULL)
		return NULL;
	ASSERT(lptmA != NULL);
	memcpy(lptmA, lptmW, sizeof(LONG) * 11);
	memcpy(&lptmA->tmItalic, &lptmW->tmItalic, sizeof(BYTE) * 5);
	WideCharToMultiByte(CP_ACP, 0, &lptmW->tmFirstChar, 1, (LPSTR)&lptmA->tmFirstChar, 1, NULL, NULL);
	WideCharToMultiByte(CP_ACP, 0, &lptmW->tmLastChar, 1, (LPSTR)&lptmA->tmLastChar, 1, NULL, NULL);
	WideCharToMultiByte(CP_ACP, 0, &lptmW->tmDefaultChar, 1, (LPSTR)&lptmA->tmDefaultChar, 1, NULL, NULL);
	WideCharToMultiByte(CP_ACP, 0, &lptmW->tmBreakChar, 1, (LPSTR)&lptmA->tmBreakChar, 1, NULL, NULL);
	return lptmA;
}
#endif

/////////////////////////////////////////////////////////////////////////////
// OLE task memory allocation support

LPWSTR AFXAPI AfxAllocTaskWideString(LPCWSTR lpszString)
{
	if (lpszString == NULL)
		return NULL;
	UINT nSize = (wcslen(lpszString)+1) * sizeof(WCHAR);
	LPWSTR lpszResult = (LPWSTR)CoTaskMemAlloc(nSize);
	if (lpszResult != NULL)
		memcpy(lpszResult, lpszString, nSize);
	return lpszResult;
}

LPWSTR AFXAPI AfxAllocTaskWideString(LPCSTR lpszString)
{
	if (lpszString == NULL)
		return NULL;
	UINT nLen = lstrlenA(lpszString)+1;
	LPWSTR lpszResult = (LPWSTR)CoTaskMemAlloc(nLen*sizeof(WCHAR));
	if (lpszResult != NULL)
		VERIFY(MultiByteToWideChar(CP_ACP, 0, lpszString, -1, lpszResult, nLen));
	return lpszResult;
}

LPSTR AFXAPI AfxAllocTaskAnsiString(LPCWSTR lpszString)
{
	if (lpszString == NULL)
		return NULL;
	UINT nBytes = (wcslen(lpszString)+1)*2;
	LPSTR lpszResult = (LPSTR)CoTaskMemAlloc(nBytes);
	if (lpszResult != NULL)
		VERIFY(WideCharToMultiByte(CP_ACP, 0, lpszString, -1, lpszResult, nBytes, NULL, NULL));
	return lpszResult;
}

LPSTR AFXAPI AfxAllocTaskAnsiString(LPCSTR lpszString)
{
	if (lpszString == NULL)
		return NULL;
	UINT nSize = lstrlenA(lpszString)+1;
	LPSTR lpszResult = (LPSTR)CoTaskMemAlloc(nSize);
	if (lpszResult != NULL)
		memcpy(lpszResult, lpszString, nSize);
	return lpszResult;
}
