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

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Help and other support

// Strings in format ".....%1 .... %2 ...." etc.

void AFXAPI AfxFormatStrings(CString& rString, UINT nIDS,
		LPCTSTR const* rglpsz, int nString)
{
	TCHAR szFormat[256];
	if (!AfxLoadString(nIDS, szFormat) != 0)
	{
		TRACE1("Error: failed to load AfxFormatString string 0x%04x.\n", nIDS);
		ASSERT(FALSE);
		return;
	}
	AfxFormatStrings(rString, szFormat, rglpsz, nString);
}

void AFXAPI AfxFormatStrings(CString& rString, LPCTSTR lpszFormat,
		LPCTSTR const* rglpsz, int nString)
{
	// determine length of destination string
	int nTotalLen = 0;
	LPCTSTR pchSrc = lpszFormat;
	while (*pchSrc != '\0')
	{
		if (pchSrc[0] == '%' &&
			 ( (pchSrc[1] >= '0' && pchSrc[1] <= '9') ||
				(pchSrc[1] >= 'A' && pchSrc[1] <= 'Z')) )
		{
			// %A comes after %9 -- we'll need it someday
			int i;
			if (pchSrc[1] > '9')
				i = 9 + (pchSrc[1] - 'A');
			else
				i = pchSrc[1] - '1';
			pchSrc += 2;
			if (i >= nString)
				++nTotalLen;
			else if (rglpsz[i] != NULL)
				nTotalLen += lstrlen(rglpsz[i]);
		}
		else
		{
			if (_istlead(*pchSrc))
				++nTotalLen, ++pchSrc;
			++pchSrc;
			++nTotalLen;
		}
	}

	pchSrc = lpszFormat;
	LPTSTR pchDest = rString.GetBuffer(nTotalLen);
	while (*pchSrc != '\0')
	{
		if (pchSrc[0] == '%' &&
			 ( (pchSrc[1] >= '0' && pchSrc[1] <= '9') ||
				(pchSrc[1] >= 'A' && pchSrc[1] <= 'Z')) )
		{
			// %A comes after %9 -- we'll need it someday
			int i;
			if (pchSrc[1] > '9')
				i = 9 + (pchSrc[1] - 'A');
			else
				i = pchSrc[1] - '1';
			pchSrc += 2;
			if (i >= nString)
			{
				TRACE1("Error: illegal string index requested %d.\n", i);
				*pchDest++ = '?';
			}
			else if (rglpsz[i] != NULL)
			{
				lstrcpy(pchDest, rglpsz[i]);
				pchDest += lstrlen(pchDest);
			}
		}
		else
		{
			if (_istlead(*pchSrc))
				*pchDest++ = *pchSrc++; // copy first of 2 bytes
			*pchDest++ = *pchSrc++;
		}
	}
	rString.ReleaseBuffer((int)((LPCTSTR)pchDest - (LPCTSTR)rString));
		// ReleaseBuffer will assert if we went too far
}

void AFXAPI AfxFormatString1(CString& rString, UINT nIDS, LPCTSTR lpsz1)
{
	AfxFormatStrings(rString, nIDS, &lpsz1, 1);
}

void AFXAPI AfxFormatString2(CString& rString, UINT nIDS, LPCTSTR lpsz1,
		LPCTSTR lpsz2)
{
	LPCTSTR rglpsz[2];
	rglpsz[0] = lpsz1;
	rglpsz[1] = lpsz2;
	AfxFormatStrings(rString, nIDS, rglpsz, 2);
}

/////////////////////////////////////////////////////////////////////////////
