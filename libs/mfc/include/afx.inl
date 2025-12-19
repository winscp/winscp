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

inline CFileException::CFileException(int cause, LONG lOsError,
	LPCTSTR pstrFileName /* = NULL */)
	{ m_cause = cause; m_lOsError = lOsError; m_strFileName = pstrFileName; }

// CString
inline CString::CString()
	{ }
inline CString::CString(const unsigned char* lpsz)
	{ *this = reinterpret_cast<LPCSTR>(lpsz); }
inline const CString& CString::operator=(const unsigned char* lpsz)
	{ *this = reinterpret_cast<LPCSTR>(lpsz); return *this; }
inline const CString& CString::operator+=(char ch)
	{ *this += static_cast<TCHAR>(ch); return *this; }
inline const CString& CString::operator=(char ch)
	{ *this = static_cast<TCHAR>(ch); return *this; }
inline CString AFXAPI operator+(const CString& string, char ch)
	{ return string + static_cast<TCHAR>(ch); }
inline CString AFXAPI operator+(char ch, const CString& string)
	{ return static_cast<TCHAR>(ch) + string; }

inline int CString::GetLength() const
	{ return m_Data.Length(); }
inline BOOL CString::IsEmpty() const
	{ return m_Data.IsEmpty(); }
inline CString::operator LPCTSTR() const
	{ return m_Data.c_str(); }

// CString support (windows specific)
inline int CString::Compare(LPCTSTR lpsz) const
	{ return _tcscmp(m_Data.c_str(), lpsz); }    // MBCS/Unicode aware
inline int CString::CompareNoCase(LPCTSTR lpsz) const
	{ return _tcsicmp(m_Data.c_str(), lpsz); }   // MBCS/Unicode aware

inline TCHAR CString::GetAt(int nIndex) const
{
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());
	return m_Data[nIndex + 1];
}
inline TCHAR CString::operator[](int nIndex) const
{
	// same as GetAt
	ASSERT(nIndex >= 0);
	ASSERT(nIndex < m_Data.Length());
	return m_Data[nIndex + 1];
}
inline bool AFXAPI operator==(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) == 0; }
inline bool AFXAPI operator==(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) == 0; }
inline bool AFXAPI operator==(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) == 0; }
inline bool AFXAPI operator!=(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) != 0; }
inline bool AFXAPI operator!=(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) != 0; }
inline bool AFXAPI operator!=(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) != 0; }
inline bool AFXAPI operator<(const CString& s1, const CString& s2)
	{ return s1.Compare(s2) < 0; }
inline bool AFXAPI operator<(const CString& s1, LPCTSTR s2)
	{ return s1.Compare(s2) < 0; }
inline bool AFXAPI operator<(LPCTSTR s1, const CString& s2)
	{ return s2.Compare(s1) > 0; }

// CTime and CTimeSpan
inline CTimeSpan::CTimeSpan(time_t time)
	{ m_timeSpan = time; }
inline CTimeSpan::CTimeSpan(const CTimeSpan& timeSpanSrc)
	{ m_timeSpan = timeSpanSrc.m_timeSpan; }
inline const CTimeSpan& CTimeSpan::operator=(const CTimeSpan& timeSpanSrc)
	{ m_timeSpan = timeSpanSrc.m_timeSpan; return *this; }
inline LONG CTimeSpan::GetTotalSeconds() const
	{ return m_timeSpan; }
inline BOOL CTimeSpan::operator==(CTimeSpan timeSpan) const
	{ return m_timeSpan == timeSpan.m_timeSpan; }
inline BOOL CTimeSpan::operator!=(CTimeSpan timeSpan) const
	{ return m_timeSpan != timeSpan.m_timeSpan; }


inline CTime::CTime()
	{ }
inline CTime::CTime(time_t time)
	{ m_time = time; }
inline CTime::CTime(const CTime& timeSrc)
	{ m_time = timeSrc.m_time; }
inline const CTime& CTime::operator=(const CTime& timeSrc)
	{ m_time = timeSrc.m_time; return *this; }
inline const CTime& CTime::operator=(time_t t)
	{ m_time = t; return *this; }
inline time_t CTime::GetTime() const
	{ return m_time; }
inline int CTime::GetYear() const
	{ return (GetLocalTm(NULL)->tm_year) + 1900; }
inline int CTime::GetMonth() const
	{ return GetLocalTm(NULL)->tm_mon + 1; }
inline int CTime::GetDay() const
	{ return GetLocalTm(NULL)->tm_mday; }
inline int CTime::GetHour() const
	{ return GetLocalTm(NULL)->tm_hour; }
inline int CTime::GetMinute() const
	{ return GetLocalTm(NULL)->tm_min; }
inline CTimeSpan CTime::operator-(CTime time) const
	{ return CTimeSpan(m_time - time.m_time); }
inline BOOL CTime::operator==(CTime time) const
	{ return m_time == time.m_time; }
inline BOOL CTime::operator!=(CTime time) const
	{ return m_time != time.m_time; }

/////////////////////////////////////////////////////////////////////////////
