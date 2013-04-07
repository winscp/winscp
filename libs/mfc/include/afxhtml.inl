// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXHTML.H

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFXHTML_INLINE

_AFXHTML_INLINE void CHtmlView::SetRegisterAsBrowser(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_RegisterAsBrowser((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetRegisterAsDropTarget(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_RegisterAsDropTarget((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetTheaterMode(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_TheaterMode((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetVisible(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Visible((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetMenuBar(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_MenuBar((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetToolBar(int nNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_ToolBar(nNewValue); }

_AFXHTML_INLINE void CHtmlView::SetOffline(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Offline((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetSilent(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Silent((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::GoBack()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->GoBack(); }

_AFXHTML_INLINE void CHtmlView::GoForward()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->GoForward(); }

_AFXHTML_INLINE void CHtmlView::GoHome()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->GoHome(); }

_AFXHTML_INLINE void CHtmlView::GoSearch()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->GoSearch(); }

_AFXHTML_INLINE void CHtmlView::Refresh()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->Refresh(); }

_AFXHTML_INLINE void CHtmlView::Refresh2(int nLevel)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->Refresh2(COleVariant((long) nLevel, VT_I4)); }

_AFXHTML_INLINE void CHtmlView::Stop()
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->Stop(); }

_AFXHTML_INLINE void CHtmlView::SetFullScreen(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_FullScreen((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetAddressBar(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_AddressBar((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

_AFXHTML_INLINE void CHtmlView::SetHeight(long nNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Height(nNewValue); }

_AFXHTML_INLINE void CHtmlView::PutProperty(LPCTSTR lpszPropertyName, long lValue)
	{ ASSERT(m_pBrowserApp != NULL); ASSERT(m_pBrowserApp != NULL); PutProperty(lpszPropertyName, COleVariant(lValue, VT_UI4)); }

_AFXHTML_INLINE void CHtmlView::PutProperty(LPCTSTR lpszPropertyName, short nValue)
	{ ASSERT(m_pBrowserApp != NULL); ASSERT(m_pBrowserApp != NULL); PutProperty(lpszPropertyName, COleVariant(nValue, VT_UI2)); }

_AFXHTML_INLINE void CHtmlView::PutProperty(LPCTSTR lpszPropertyName, LPCTSTR lpszValue)
	{ ASSERT(m_pBrowserApp != NULL); ASSERT(m_pBrowserApp != NULL); PutProperty(lpszPropertyName, COleVariant(lpszValue, VT_BSTR)); }

_AFXHTML_INLINE void CHtmlView::PutProperty(LPCTSTR lpszPropertyName, double dValue)
	{ ASSERT(m_pBrowserApp != NULL); ASSERT(m_pBrowserApp != NULL); PutProperty(lpszPropertyName, COleVariant(dValue)); }

_AFXHTML_INLINE void CHtmlView::SetTop(long nNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Top(nNewValue); }

_AFXHTML_INLINE void CHtmlView::SetLeft(long nNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_Left(nNewValue); }

_AFXHTML_INLINE void CHtmlView::SetStatusBar(BOOL bNewValue)
	{ ASSERT(m_pBrowserApp != NULL); m_pBrowserApp->put_StatusBar((short) (bNewValue ? AFX_OLE_TRUE : AFX_OLE_FALSE)); }

#endif //_AFXHTML_INLINE

/////////////////////////////////////////////////////////////////////////////
