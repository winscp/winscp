// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFX.H

#ifdef _AFX_INLINE

_AFX_INLINE CFileException::CFileException(int cause, LONG lOsError,
	LPCTSTR pstrFileName /* = NULL */)
	{ m_cause = cause; m_lOsError = lOsError; m_strFileName = pstrFileName; }

// CString
_AFX_INLINE CString::CString()
	{ }
_AFX_INLINE CString::CString(const unsigned char* lpsz)
	{ *this = reinterpret_cast<LPCSTR>(lpsz); }
_AFX_INLINE const CString& CString::operator=(const unsigned char* lpsz)
	{ *this = reinterpret_cast<LPCSTR>(lpsz); return *this; }
_AFX_INLINE const CString& CString::operator+=(char ch)
	{ *this += static_cast<TCHAR>(ch); return *this; }
_AFX_INLINE const CString& CString::operator=(char ch)
	{ *this = static_cast<TCHAR>(ch); return *this; }
_AFX_INLINE CString AFXAPI operator+(const CString& string, char ch)
	{ return string + static_cast<TCHAR>(ch); }
_AFX_INLINE CString AFXAPI operator+(char ch, const CString& string)
	{ return static_cast<TCHAR>(ch) + string; }

_AFX_INLINE int CString::GetLength() const
	{ return m_Data.Length(); }
_AFX_INLINE BOOL CString::IsEmpty() const
	{ return m_Data.IsEmpty(); }
_AFX_INLINE CString::operator LPCTSTR() const
	{ return m_Data.c_str(); }

// CString support (windows specific)
_AFX_INLINE int CString::Compare(LPCTSTR lpsz) const
	{ return _tcscmp(m_Data.c_str(), lpsz); }    // MBCS/Unicode aware
_AFX_INLINE int CString::CompareNoCase(LPCTSTR lpsz) const
	{ return _tcsicmp(m_Data.c_str(), lpsz); }   // MBCS/Unicode aware

_AFX_INLINE TCHAR CString::GetAt(int nIndex) const
{
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());
	return m_Data[nIndex + 1];
}
_AFX_INLINE TCHAR CString::operator[](int nIndex) const
{
	// same as GetAt
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());
	return m_Data[nIndex + 1];
}
_AFX_INLINE bool AFXAPI operator==(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) == 0; }
_AFX_INLINE bool AFXAPI operator==(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) == 0; }
_AFX_INLINE bool AFXAPI operator==(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) == 0; }
_AFX_INLINE bool AFXAPI operator!=(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) != 0; }
_AFX_INLINE bool AFXAPI operator!=(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) != 0; }
_AFX_INLINE bool AFXAPI operator!=(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) != 0; }
_AFX_INLINE bool AFXAPI operator<(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) < 0; }
_AFX_INLINE bool AFXAPI operator<(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) < 0; }
_AFX_INLINE bool AFXAPI operator<(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) > 0; }

// CTime and CTimeSpan
_AFX_INLINE CTimeSpan::CTimeSpan(time_t time)
	{ m_timeSpan = time; }
_AFX_INLINE CTimeSpan::CTimeSpan(const CTimeSpan& timeSpanSrc)
	{ m_timeSpan = timeSpanSrc.m_timeSpan; }
_AFX_INLINE const CTimeSpan& CTimeSpan::operator=(const CTimeSpan& timeSpanSrc)
	{ m_timeSpan = timeSpanSrc.m_timeSpan; return *this; }
_AFX_INLINE LONG CTimeSpan::GetTotalSeconds() const
	{ return m_timeSpan; }
_AFX_INLINE BOOL CTimeSpan::operator==(CTimeSpan timeSpan) const
	{ return m_timeSpan == timeSpan.m_timeSpan; }
_AFX_INLINE BOOL CTimeSpan::operator!=(CTimeSpan timeSpan) const
	{ return m_timeSpan != timeSpan.m_timeSpan; }


_AFX_INLINE CTime::CTime()
	{ }
_AFX_INLINE CTime::CTime(time_t time)
	{ m_time = time; }
_AFX_INLINE CTime::CTime(const CTime& timeSrc)
	{ m_time = timeSrc.m_time; }
_AFX_INLINE const CTime& CTime::operator=(const CTime& timeSrc)
	{ m_time = timeSrc.m_time; return *this; }
_AFX_INLINE const CTime& CTime::operator=(time_t t)
	{ m_time = t; return *this; }
_AFX_INLINE time_t CTime::GetTime() const
	{ return m_time; }
_AFX_INLINE int CTime::GetYear() const
	{ return (GetLocalTm(NULL)->tm_year) + 1900; }
_AFX_INLINE int CTime::GetMonth() const
	{ return GetLocalTm(NULL)->tm_mon + 1; }
_AFX_INLINE int CTime::GetDay() const
	{ return GetLocalTm(NULL)->tm_mday; }
_AFX_INLINE int CTime::GetHour() const
	{ return GetLocalTm(NULL)->tm_hour; }
_AFX_INLINE int CTime::GetMinute() const
	{ return GetLocalTm(NULL)->tm_min; }
_AFX_INLINE CTimeSpan CTime::operator-(CTime time) const
	{ return CTimeSpan(m_time - time.m_time); }
_AFX_INLINE BOOL CTime::operator==(CTime time) const
	{ return m_time == time.m_time; }
_AFX_INLINE BOOL CTime::operator!=(CTime time) const
	{ return m_time != time.m_time; }

/////////////////////////////////////////////////////////////////////////////

#endif //_AFX_INLINE
