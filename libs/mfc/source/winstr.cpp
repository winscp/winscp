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

/////////////////////////////////////////////////////////////////////////////
// Windows extensions to strings

#define CHAR_FUDGE 1    // one TCHAR unused is good enough

BOOL CString::LoadString(UINT nID)
{
	// try fixed buffer first (to avoid wasting space in the heap)
	TCHAR szTemp[256];
	int nLen = AfxLoadString(nID, szTemp, std::size(szTemp));
	if (std::size(szTemp) - nLen > CHAR_FUDGE)
	{
		*this = szTemp;
		return nLen > 0;
	}

	// Shouldn't happen as we do not have such long string in FileZilla
	// try buffer size of 512, then larger size until entire string is retrieved
	int nSize = 256;
	do
	{
		nSize += 256;
		m_Data.SetLength(nSize - 1);
		nLen = AfxLoadString(nID, m_Data.c_str(), nSize);
	} while (nSize - nLen <= CHAR_FUDGE);
	m_Data.SetLength(nLen);

	return nLen > 0;
}

int AFXAPI AfxLoadString(UINT nID, LPTSTR lpszBuf, UINT nMaxBuf)
{
	ASSERT(AfxIsValidAddress(lpszBuf, nMaxBuf*sizeof(TCHAR)));
	int nLen = ::LoadString(AfxGetResourceHandle(), nID, lpszBuf, nMaxBuf);
	if (nLen == 0)
		lpszBuf[0] = '\0';
	return nLen;
}

/////////////////////////////////////////////////////////////////////////////
