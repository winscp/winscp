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

CString::CString(const wchar_t * lpsz)
{
	m_Data = lpsz;
}

/////////////////////////////////////////////////////////////////////////////
// Special conversion constructors

CString::CString(const char * lpsz)
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

const CString& CString::operator=(const wchar_t * lpsz)
{
	m_Data = lpsz;
	return *this;
}

/////////////////////////////////////////////////////////////////////////////
// Special conversion assignment

const CString& CString::operator=(const char * lpsz)
{
	m_Data = UnicodeString(lpsz);
	return *this;
}

//////////////////////////////////////////////////////////////////////////////
// concatenation

// NOTE: "operator+" is done as friend functions for simplicity
//      There are three variants:
//          CString + CString
// and for ? = wchar_t, const wchar_t *
//          CString + ?
//          ? + CString

CString operator+(const CString& string1, const CString& string2)
{
	return CString(string1.m_Data + string2.m_Data);
}

CString operator+(const CString& string, const wchar_t * lpsz)
{
	return CString(string.m_Data + lpsz);
}

CString operator+(const wchar_t * lpsz, const CString& string)
{
	return CString(lpsz + string.m_Data);
}

//////////////////////////////////////////////////////////////////////////////
// concatenate in place

const CString& CString::operator+=(const wchar_t * lpsz)
{
	m_Data += lpsz;
	return *this;
}

const CString& CString::operator+=(wchar_t ch)
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

int CString::Find(wchar_t ch) const
{
	return Find(ch, 0);
}

int CString::Find(wchar_t ch, int nStart) const
{
	int nLength = m_Data.Length();
	if (nStart >= nLength)
		return -1;

	// find first single character
	wchar_t * lpsz = wcschr(m_Data.c_str() + nStart, ch);

	// return -1 if not found and index otherwise
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}

int CString::FindOneOf(const wchar_t * lpszCharSet) const
{
	wchar_t * lpsz = wcspbrk(m_Data.c_str(), lpszCharSet);
	return (lpsz == NULL) ? -1 : (int)(lpsz - m_Data.c_str());
}

void CString::MakeLower()
{
	m_Data = m_Data.LowerCase();
}

void CString::SetAt(int nIndex, wchar_t ch)
{
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());

	// Implies Unique()
	m_Data[nIndex + 1] = ch;
}

///////////////////////////////////////////////////////////////////////////////
