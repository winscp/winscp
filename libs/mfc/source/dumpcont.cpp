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

#ifdef AFX_DBG1_SEG
#pragma code_seg(AFX_DBG1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Diagnostic Stream output

void CDumpContext::OutputString(LPCTSTR lpsz)
{
#ifdef _DEBUG
	// all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return;
#endif

	// use C-runtime/OutputDebugString when m_pFile is NULL
	if (m_pFile == NULL)
	{
		AfxOutputDebugString(lpsz);
		return;
	}

	// otherwise, write the string to the file
	m_pFile->Write(lpsz, lstrlen(lpsz)*sizeof(TCHAR));
}

CDumpContext::CDumpContext(CFile* pFile)
{
	if (pFile)
		ASSERT_VALID(pFile);

	m_pFile = pFile;
	m_nDepth = 0;
}

void CDumpContext::Flush()
{
	if (m_pFile)
		m_pFile->Flush();
}

CDumpContext& CDumpContext::operator<<(LPCTSTR lpsz)
{
	if (lpsz == NULL)
	{
		OutputString(_T("(NULL)"));
		return *this;
	}

#ifdef _DEBUG // all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return *this;
#endif //_DEBUG

	if (m_pFile == NULL)
	{
		TCHAR szBuffer[512];
		LPTSTR lpBuf = szBuffer;
		while (*lpsz != '\0')
		{
			if (lpBuf > szBuffer + _countof(szBuffer) - 3)
			{
				*lpBuf = '\0';
				OutputString(szBuffer);
				lpBuf = szBuffer;
			}
			if (*lpsz == '\n')
				*lpBuf++ = '\r';
			*lpBuf++ = *lpsz++;
		}
		*lpBuf = '\0';
		OutputString(szBuffer);
		return *this;
	}

	m_pFile->Write(lpsz, lstrlen(lpsz)*sizeof(TCHAR));
	return *this;
}

CDumpContext& CDumpContext::operator<<(BYTE by)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("%d"), (int)by);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(WORD w)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("%u"), (UINT) w);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(UINT u)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("0x%X"), u);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(LONG l)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("%ld"), l);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(DWORD dw)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("%lu"), dw);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(int n)
{
	TCHAR szBuffer[32];

	wsprintf(szBuffer, _T("%d"), n);
	OutputString(szBuffer);

	return *this;
}

CDumpContext& CDumpContext::operator<<(const CObject* pOb)
{
#ifdef _DEBUG // all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return *this;
#endif //_DEBUG

	if (pOb == NULL)
		*this << _T("NULL");
	else
#ifdef _AFXDLL
		pOb->Dump(*this);
#else
		*this << _T("Unable to dump object in static release builds");
#endif

	return *this;
}

CDumpContext& CDumpContext::operator<<(const CObject& ob)
{
	return *this << &ob;
}

CDumpContext& CDumpContext::operator<<(const void* lp)
{
	TCHAR szBuffer[32];

	// prefix a pointer with "$" and print in hex
	wsprintf(szBuffer, _T("$%lX"), (LONG)lp);
	OutputString(szBuffer);

	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// Formatted output

void CDumpContext::HexDump(LPCTSTR lpszLine, BYTE* pby,
	int nBytes, int nWidth)
// do a simple hex-dump (8 per line) to a CDumpContext
//  the "lpszLine" is a string to print at the start of each line
//    (%lx should be used to expand the current address)
{
	ASSERT(nBytes > 0);
	ASSERT(nWidth > 0);
	ASSERT(AfxIsValidString(lpszLine));
	ASSERT(AfxIsValidAddress(pby, nBytes, FALSE));

#ifdef _DEBUG // all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return;
#endif //_DEBUG

	int nRow = 0;
	TCHAR szBuffer[32];

	while (nBytes--)
	{
		if (nRow == 0)
		{
			wsprintf(szBuffer, lpszLine, pby);
			*this << szBuffer;
		}

		wsprintf(szBuffer, _T(" %02X"), *pby++);
		*this << szBuffer;

		if (++nRow >= nWidth)
		{
			*this << _T("\n");
			nRow = 0;
		}
	}
	if (nRow != 0)
		*this << _T("\n");
}

/////////////////////////////////////////////////////////////////////////////

#ifdef _UNICODE
// special version for ANSI characters
CDumpContext& CDumpContext::operator<<(LPCSTR lpsz)
{
	if (lpsz == NULL)
	{
		OutputString(L"(NULL)");
		return *this;
	}

#ifdef _DEBUG // all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return *this;
#endif //_DEBUG

	// limited length
	TCHAR szBuffer[512];
	_mbstowcsz(szBuffer, lpsz, _countof(szBuffer));
	return *this << szBuffer;
}
#else   //_UNICODE
// special version for WIDE characters
CDumpContext& CDumpContext::operator<<(LPCWSTR lpsz)
{
	if (lpsz == NULL)
	{
		OutputString("(NULL)");
		return *this;
	}

#ifdef _DEBUG // all CDumpContext output is controlled by afxTraceEnabled
	if (!afxTraceEnabled)
		return *this;
#endif //_DEBUG

	// limited length
	char szBuffer[512];
	_wcstombsz(szBuffer, lpsz, _countof(szBuffer));
	return *this << szBuffer;
}
#endif  //!_UNICODE

/////////////////////////////////////////////////////////////////////////////
