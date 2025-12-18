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
#include <SysUtils.hpp>

HINSTANCE afxCurrentResourceHandle;

//////////////////////////////////////////////////////////////////////////////
// Construction/Destruction

CString::CString(const CString& stringSrc)
{
	m_Data = stringSrc.m_Data;
}

void CString::Empty()
{
	m_Data = EmptyStr;
}

//////////////////////////////////////////////////////////////////////////////
// More sophisticated construction

CString::CString(LPCTSTR lpsz)
{
	if (lpsz != NULL && HIWORD(lpsz) == NULL)
	{
		UINT nID = LOWORD((DWORD)lpsz);
		LoadString(nID);
	}
	else
	{
		m_Data = lpsz;
	}
}

/////////////////////////////////////////////////////////////////////////////
// Special conversion constructors

CString::CString(LPCSTR lpsz)
{
	m_Data = UnicodeString(lpsz);
}

CString::CString(const UnicodeString& str)
{
	m_Data = str;
}

//////////////////////////////////////////////////////////////////////////////
// Assignment operators
//  All assign a new value to the string
//
//  All routines return the new string (but as a 'const CString&' so that
//      assigning it again will cause a copy, eg: s1 = s2 = "hi there".
//

const CString& CString::operator=(const CString& stringSrc)
{
	m_Data = stringSrc.m_Data;
	return *this;
}

const CString& CString::operator=(LPCTSTR lpsz)
{
	m_Data = lpsz;
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// Special conversion assignment

const CString& CString::operator=(LPCSTR lpsz)
{
	m_Data = UnicodeString(lpsz);
	return *this;
}

//////////////////////////////////////////////////////////////////////////////
// concatenation

// NOTE: "operator+" is done as friend functions for simplicity
//      There are three variants:
//          CString + CString
// and for ? = TCHAR, LPCTSTR
//          CString + ?
//          ? + CString

CString AFXAPI operator+(const CString& string1, const CString& string2)
{
	return CString(string1.m_Data + string2.m_Data);
}

CString AFXAPI operator+(const CString& string, LPCTSTR lpsz)
{
	return CString(string.m_Data + lpsz);
}

CString AFXAPI operator+(LPCTSTR lpsz, const CString& string)
{
	return CString(lpsz + string.m_Data);
}

//////////////////////////////////////////////////////////////////////////////
// concatenate in place

const CString& CString::operator+=(LPCTSTR lpsz)
{
	ASSERT(lpsz == NULL || AfxIsValidString(lpsz));
	m_Data += lpsz;
	return *this;
}

const CString& CString::operator+=(TCHAR ch)
{
	m_Data += ch;
	return *this;
}

const CString& CString::operator+=(const CString& string)
{
	m_Data += string.m_Data;
	return *this;
}

///////////////////////////////////////////////////////////////////////////////
// Commonly used routines (rarely used routines in STREX.CPP)

int CString::Find(TCHAR ch) const
{
	return Find(ch, 0);
}

int CString::Find(TCHAR ch, int nStart) const
{
	int nLength = m_Data.Length();
	if (nStart >= nLength)
		return -1;

	// find first single character
	LPTSTR lpsz = _tcschr(m_Data.c_str() + nStart, (_TUCHAR)ch);

	// return -1 if not found and index otherwise
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}

int CString::FindOneOf(LPCTSTR lpszCharSet) const
{
	ASSERT(AfxIsValidString(lpszCharSet));
	LPTSTR lpsz = _tcspbrk(m_Data.c_str(), lpszCharSet);
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}

void CString::MakeLower()
{
	m_Data = m_Data.LowerCase();
}

void CString::SetAt(int nIndex, TCHAR ch)
{
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());

	// Implies Unique()
	m_Data[nIndex + 1] = ch;
}

///////////////////////////////////////////////////////////////////////////////
