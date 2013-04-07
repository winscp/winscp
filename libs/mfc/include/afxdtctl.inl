// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXDTCTL.H

/////////////////////////////////////////////////////////////////////////////
// Date/Time common control inlines

#ifdef _AFXDTCTL_INLINE

//CDateTimeCtrl
_AFXDTCTL_INLINE CDateTimeCtrl::CDateTimeCtrl()
	{ }
_AFXDTCTL_INLINE CFont* CDateTimeCtrl::GetMonthCalFont() const
	{ ASSERT(::IsWindow(m_hWnd)); return CFont::FromHandle((HFONT) ::SendMessage(m_hWnd, DTM_GETMCFONT, 0, 0)); }
_AFXDTCTL_INLINE CMonthCalCtrl* CDateTimeCtrl::GetMonthCalCtrl() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CMonthCalCtrl*) CWnd::FromHandle((HWND) ::SendMessage(m_hWnd, DTM_GETMONTHCAL, 0, 0)); }
_AFXDTCTL_INLINE void CDateTimeCtrl::SetMonthCalFont(HFONT hFont, BOOL bRedraw /* = TRUE */)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, DTM_SETMCFONT, (WPARAM) hFont, MAKELONG(bRedraw, 0)); }
_AFXDTCTL_INLINE COLORREF CDateTimeCtrl::SetMonthCalColor(int iColor, COLORREF ref)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, DTM_SETMCCOLOR, (WPARAM) iColor, (LPARAM) ref); }
_AFXDTCTL_INLINE DWORD CDateTimeCtrl::GetTime(LPSYSTEMTIME pTimeDest) const
	{ ASSERT(::IsWindow(m_hWnd)); ASSERT(pTimeDest != NULL); return (DWORD) ::SendMessage(m_hWnd, DTM_GETSYSTEMTIME, 0, (LPARAM) pTimeDest); }
_AFXDTCTL_INLINE COLORREF CDateTimeCtrl::GetMonthCalColor(int iColor) const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, DTM_GETMCCOLOR, (WPARAM) iColor, 0); }
_AFXDTCTL_INLINE BOOL CDateTimeCtrl::SetFormat(LPCTSTR pstrFormat)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, DTM_SETFORMAT, 0, (LPARAM) pstrFormat); }

//CMonthCalCtrl
_AFXDTCTL_INLINE CMonthCalCtrl::CMonthCalCtrl()
	{ }
_AFXDTCTL_INLINE DWORD CMonthCalCtrl::HitTest(PMCHITTESTINFO pMCHitTest)
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, MCM_HITTEST, 0, (LPARAM) pMCHitTest); }
_AFXDTCTL_INLINE BOOL CMonthCalCtrl::GetMinReqRect(RECT* pRect) const
	{ ASSERT(m_hWnd != NULL); return (BOOL) ::SendMessage(m_hWnd, MCM_GETMINREQRECT, 0, (LPARAM) pRect); }
_AFXDTCTL_INLINE int CMonthCalCtrl::SetMonthDelta(int iDelta)
	{ ASSERT(m_hWnd != NULL); return (int) ::SendMessage(m_hWnd, MCM_SETMONTHDELTA, (WPARAM) iDelta, 0); }
_AFXDTCTL_INLINE int CMonthCalCtrl::GetMonthDelta() const
	{ ASSERT(m_hWnd != NULL); return (int) ::SendMessage(m_hWnd, MCM_GETMONTHDELTA, 0, 0); }
_AFXDTCTL_INLINE COLORREF CMonthCalCtrl::GetColor(int nRegion) const
	{ ASSERT(m_hWnd != NULL); return (COLORREF) ::SendMessage(m_hWnd, MCM_GETCOLOR, (WPARAM) nRegion, 0); }
_AFXDTCTL_INLINE COLORREF CMonthCalCtrl::SetColor(int nRegion, COLORREF ref)
	{ ASSERT(m_hWnd != NULL); return (COLORREF) ::SendMessage(m_hWnd, MCM_SETCOLOR, (WPARAM) nRegion, (LPARAM) ref); }
_AFXDTCTL_INLINE BOOL CMonthCalCtrl::SetMaxSelCount(int nMax)
	{ ASSERT(m_hWnd != NULL); return (BOOL) ::SendMessage(m_hWnd, MCM_SETMAXSELCOUNT, nMax, 0); }
_AFXDTCTL_INLINE int CMonthCalCtrl::GetMaxSelCount() const
	{ ASSERT(m_hWnd != NULL); return (int) ::SendMessage(m_hWnd, MCM_GETMAXSELCOUNT, 0, 0); }
_AFXDTCTL_INLINE void CMonthCalCtrl::SetToday(const LPSYSTEMTIME pDateTime)
	{ ASSERT(m_hWnd != NULL); ::SendMessage(m_hWnd, MCM_SETTODAY, 0, (LPARAM) pDateTime); }
_AFXDTCTL_INLINE BOOL CMonthCalCtrl::GetToday(LPSYSTEMTIME pDateTime) const
	{ ASSERT(m_hWnd != NULL); return (BOOL) ::SendMessage(m_hWnd, MCM_GETTODAY, 0, (LPARAM) pDateTime); }
_AFXDTCTL_INLINE BOOL CMonthCalCtrl::SetCurSel(const LPSYSTEMTIME pDateTime)
	{ ASSERT(m_hWnd != NULL); return (BOOL) ::SendMessage(m_hWnd, MCM_SETCURSEL, 0, (LPARAM) pDateTime); }
_AFXDTCTL_INLINE BOOL CMonthCalCtrl::GetCurSel(LPSYSTEMTIME pDateTime) const
	{ ASSERT(m_hWnd != NULL); return (BOOL) ::SendMessage(m_hWnd, MCM_GETCURSEL, 0, (LPARAM) pDateTime); }

#endif //_AFXDTCTL_INLINE
/////////////////////////////////////////////////////////////////////////////
