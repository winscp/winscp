// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXINET.H

/////////////////////////////////////////////////////////////////////////////
//

#ifdef _AFXINET_INLINE

_AFXINET_INLINE DWORD CInternetSession::GetContext() const
	{ return m_dwContext; }

_AFXINET_INLINE CString CInternetConnection::GetServerName() const
	{ return m_strServerName; }

_AFXINET_INLINE CInternetSession* CInternetConnection::GetSession() const
	{ return m_pSession; }

_AFXINET_INLINE CInternetSession::operator HINTERNET() const
	{ return m_hSession; }

_AFXINET_INLINE BOOL CInternetSession::SetOption(DWORD dwOption, DWORD dwValue,
	DWORD dwFlags /* = 0 */)
	{ ASSERT((dwFlags & INTERNET_FLAG_ASYNC) == 0); return SetOption(dwOption, &dwValue, sizeof(dwValue), dwFlags); }


_AFXINET_INLINE CGopherLocator::operator LPCTSTR() const
	{ return (LPCTSTR) m_Locator; }

_AFXINET_INLINE BOOL CGopherLocator::GetLocatorType(DWORD& dwRef) const
	{ return GopherGetLocatorType((LPCTSTR) m_Locator, &dwRef); }

_AFXINET_INLINE CGopherLocator::CGopherLocator(const CGopherLocator& ref)
	{ m_Locator = ref.m_Locator; m_dwBufferLength = ref.m_dwBufferLength; }


_AFXINET_INLINE CInternetConnection::operator HINTERNET() const
	{ return m_hConnection; }

_AFXINET_INLINE DWORD CInternetConnection::GetContext() const
	{ return m_dwContext; }

_AFXINET_INLINE BOOL CInternetConnection::SetOption(DWORD dwOption,
	DWORD dwValue, DWORD dwFlags /* = 0 */)
	{ return SetOption(dwOption, &dwValue, sizeof(dwValue), dwFlags); }


_AFXINET_INLINE DWORD CInternetFile::GetContext() const
	{ return m_dwContext; }

_AFXINET_INLINE CInternetFile::operator HINTERNET() const
	{ return m_hFile; }

_AFXINET_INLINE BOOL CInternetFile::SetOption(DWORD dwOption, DWORD dwValue,
	DWORD dwFlags /* = 0 */)
	{ ASSERT((dwFlags & INTERNET_FLAG_ASYNC) == 0); return SetOption(dwOption, &dwValue, sizeof(dwValue), dwFlags); }


#endif //_AFXINET_INLINE

/////////////////////////////////////////////////////////////////////////////
