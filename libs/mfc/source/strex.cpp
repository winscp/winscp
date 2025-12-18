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
#include <StrUtils.hpp>

//////////////////////////////////////////////////////////////////////////////
// More sophisticated construction

CString::CString(LPCTSTR lpch, int nLength)
{
	m_Data = UnicodeString(lpch, nLength);
}

/////////////////////////////////////////////////////////////////////////////
// Special conversion constructors

CString::CString(LPCSTR lpsz, int nLength)
{
	m_Data = UnicodeString(AnsiString(lpsz, nLength));
}

//////////////////////////////////////////////////////////////////////////////
// Assignment operators

const CString& CString::operator=(TCHAR ch)
{
	m_Data = ch;
	return *this;
}

//////////////////////////////////////////////////////////////////////////////
// less common string expressions

CString AFXAPI operator+(const CString& string1, TCHAR ch)
{
	return CString(string1.m_Data + ch);
}

CString AFXAPI operator+(TCHAR ch, const CString& string)
{
	return CString(UnicodeString(ch) + string.m_Data);
}

//////////////////////////////////////////////////////////////////////////////
// Advanced manipulation

int CString::Delete(int nIndex, int nCount /* = 1 */)
{
	if (nIndex < 0)
		nIndex = 0;
	int nNewLength = m_Data.Length();
	m_Data.Delete(nIndex + 1, nCount);

	return nNewLength;
}

int CString::Replace(TCHAR chOld, TCHAR chNew)
{
	int nCount = 0;

	// short-circuit the nop case
	if (chOld != chNew)
	{
		// otherwise modify each character that matches in the string
		m_Data.Unique();
		LPTSTR psz = m_Data.c_str();
		LPTSTR pszEnd = psz + m_Data.Length();
		while (psz < pszEnd)
		{
			// replace instances of the specified character only
			if (*psz == chOld)
			{
				*psz = chNew;
				nCount++;
			}
			psz = _tcsinc(psz);
		}
	}
	return nCount;
}

BOOL CString::Replace(LPCTSTR lpszOld, LPCTSTR lpszNew)
{
	UnicodeString prev = m_Data;
	m_Data = ReplaceStr(m_Data, lpszOld, lpszNew);
	return (prev != m_Data);
}

//////////////////////////////////////////////////////////////////////////////
// Very simple sub-string extraction

CString CString::Mid(int nFirst) const
{
	return Mid(nFirst, m_Data.Length() - nFirst);
}

CString CString::Mid(int nFirst, int nCount) const
{
	// out-of-bounds requests return sensible things
	if (nFirst < 0)
		nFirst = 0;
	if (nCount < 0)
		nCount = 0;

	if (nFirst + nCount > m_Data.Length())
		nCount = m_Data.Length() - nFirst;
	if (nFirst > m_Data.Length())
		nCount = 0;

	ASSERT(nFirst >= 0);
	ASSERT(nFirst + nCount <= m_Data.Length());

	// optimize case of returning entire string
	if (nFirst == 0 && nFirst + nCount == m_Data.Length())
		return *this;

	return CString(m_Data.SubString(nFirst + 1, nCount));
}

CString CString::Right(int nCount) const
{
	if (nCount < 0)
		nCount = 0;
	if (nCount >= m_Data.Length())
		return *this;

	return CString(m_Data.SubString(m_Data.Length() - nCount + 1, nCount));
}

CString CString::Left(int nCount) const
{
	if (nCount < 0)
		nCount = 0;
	if (nCount >= m_Data.Length())
		return *this;

	return CString(m_Data.SubString(1, nCount));
}

//////////////////////////////////////////////////////////////////////////////
// Finding

int CString::ReverseFind(TCHAR ch) const
{
	// find last single character
	LPTSTR lpsz = _tcsrchr(m_Data.c_str(), (_TUCHAR) ch);

	// return -1 if not found, distance from beginning otherwise
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}

// find a sub-string (like strstr)
int CString::Find(LPCTSTR lpszSub) const
{
	return Find(lpszSub, 0);
}

int CString::Find(LPCTSTR lpszSub, int nStart) const
{
	int nLength = m_Data.Length();
	if (nStart > nLength)
		return -1;

	// find first matching substring
	LPTSTR lpsz = _tcsstr(m_Data.c_str() + nStart, lpszSub);

	// return -1 for not found, distance from beginning otherwise
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}


/////////////////////////////////////////////////////////////////////////////
// CString formatting

void CString::FormatV(LPCTSTR lpszFormat, va_list argList)
{
	m_Data.vprintf(lpszFormat, argList);
}

// formatting (using wsprintf style formatting)
void CString::Format(LPCTSTR lpszFormat, ...)
{
	va_list argList;
	va_start(argList, lpszFormat);
	FormatV(lpszFormat, argList);
	va_end(argList);
}

void CString::Format(UINT nFormatID, ...)
{
	UnicodeString strFormat = LoadStr(nFormatID);

	va_list argList;
	va_start(argList, nFormatID);
	FormatV(strFormat.c_str(), argList);
	va_end(argList);
}

void CString::TrimRight(LPCTSTR lpszTargetList)
{
	UnicodeString TargetList(lpszTargetList);
	while (!m_Data.IsEmpty() && m_Data.IsDelimiter(TargetList, m_Data.Length()))
	{
		m_Data.SetLength(m_Data.Length() - 1);
	}
}

void CString::TrimRight(TCHAR chTarget)
{
	TrimRight(UnicodeString(chTarget).c_str());
}

void CString::TrimLeft(LPCTSTR lpszTargets)
{
	UnicodeString Targets(lpszTargets);
	while (!m_Data.IsEmpty() && m_Data.IsDelimiter(Targets, 1))
	{
		m_Data.Delete(1, 1);
	}
}

void CString::TrimLeft(TCHAR chTarget)
{
	TrimLeft(UnicodeString(chTarget).c_str());
}

///////////////////////////////////////////////////////////////////////////////
