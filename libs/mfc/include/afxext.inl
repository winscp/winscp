// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXEXT.H

#ifdef _AFXEXT_INLINE

// CCreateContext
_AFXEXT_INLINE CCreateContext::CCreateContext()
	{ memset(this, 0, sizeof(*this)); }

// CMetaFileDC
_AFXEXT_INLINE BOOL CMetaFileDC::Create(LPCTSTR lpszFilename)
	{ return Attach(::CreateMetaFile(lpszFilename)); }
_AFXEXT_INLINE HMETAFILE CMetaFileDC::Close()
	{ return ::CloseMetaFile(Detach()); }
_AFXEXT_INLINE BOOL CMetaFileDC::CreateEnhanced(CDC* pDCRef,
		LPCTSTR lpszFileName, LPCRECT lpBounds, LPCTSTR lpszDescription)
	{ return Attach(::CreateEnhMetaFile(pDCRef->GetSafeHdc(),
		lpszFileName, lpBounds, lpszDescription)); }
_AFXEXT_INLINE HENHMETAFILE CMetaFileDC::CloseEnhanced()
	{ return ::CloseEnhMetaFile(Detach()); }
_AFXEXT_INLINE CPoint CMetaFileDC::SetViewportOrg(POINT point)
	{ ASSERT(m_hDC != NULL); return SetViewportOrg(point.x, point.y); }
_AFXEXT_INLINE CSize CMetaFileDC::SetViewportExt(SIZE size)
	{ ASSERT(m_hDC != NULL); return SetViewportExt(size.cx, size.cy); }
_AFXEXT_INLINE BOOL CMetaFileDC::TextOut(int x, int y, const CString& str)
	{ ASSERT(m_hDC != NULL); return TextOut(x, y, (LPCTSTR)str, str.GetLength()); }
_AFXEXT_INLINE BOOL CMetaFileDC::ExtTextOut(int x, int y, UINT nOptions, LPCRECT lpRect,
	const CString& str, LPINT lpDxWidths)
	{ ASSERT(m_hDC != NULL); return ::ExtTextOut(m_hDC, x, y, nOptions, lpRect,
		str, str.GetLength(), lpDxWidths); }
_AFXEXT_INLINE CSize CMetaFileDC::TabbedTextOut(int x, int y, const CString& str,
	int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin)
	{ ASSERT(m_hDC != NULL); return ::TabbedTextOut(m_hDC, x, y, str, str.GetLength(),
		nTabPositions, lpnTabStopPositions, nTabOrigin); }
_AFXEXT_INLINE int CMetaFileDC::DrawText(const CString& str, LPRECT lpRect, UINT nFormat)
	{ ASSERT(m_hDC != NULL);
		return DrawText((LPCTSTR)str, str.GetLength(), lpRect, nFormat); }
_AFXEXT_INLINE BOOL CMetaFileDC::PtVisible(POINT point) const
	{ ASSERT(m_hDC != NULL); return PtVisible(point.x, point.y); }

// CSplitterWnd
_AFXEXT_INLINE int CSplitterWnd::GetRowCount() const
	{ return m_nRows; }
_AFXEXT_INLINE int CSplitterWnd::GetColumnCount() const
	{ return m_nCols; }
// obsolete functions
_AFXEXT_INLINE BOOL CSplitterWnd::IsChildPane(CWnd* pWnd, int& row, int& col)
	{ return IsChildPane(pWnd, &row, &col); }
_AFXEXT_INLINE CWnd* CSplitterWnd::GetActivePane(int& row, int& col)
	{ return GetActivePane(&row, &col); }
_AFXEXT_INLINE BOOL CSplitterWnd::IsTracking()
	{ return m_bTracking; }

// CControlBar
_AFXEXT_INLINE int CControlBar::GetCount() const
	{ return m_nCount; }
_AFXEXT_INLINE DWORD CControlBar::GetBarStyle()
	{ return m_dwStyle; }
_AFXEXT_INLINE void CControlBar::SetBorders(LPCRECT lpRect)
	{ SetBorders(lpRect->left, lpRect->top, lpRect->right, lpRect->bottom); }
_AFXEXT_INLINE CRect CControlBar::GetBorders() const
	{ return CRect(m_cxLeftBorder, m_cyTopBorder, m_cxRightBorder, m_cyBottomBorder); }

// CToolBar
_AFXEXT_INLINE BOOL CToolBar::LoadToolBar(UINT nIDResource)
	{ return LoadToolBar(MAKEINTRESOURCE(nIDResource)); }
_AFXEXT_INLINE BOOL CToolBar::LoadBitmap(UINT nIDResource)
	{ return LoadBitmap(MAKEINTRESOURCE(nIDResource)); }
_AFXEXT_INLINE CToolBarCtrl& CToolBar::GetToolBarCtrl() const
	{ return *(CToolBarCtrl*)this; }

// CDialogBar
_AFXEXT_INLINE BOOL CDialogBar::Create(CWnd* pParentWnd, UINT nIDTemplate,
		UINT nStyle, UINT nID)
	{ return Create(pParentWnd, MAKEINTRESOURCE(nIDTemplate), nStyle, nID); }

// CStatusBar
_AFXEXT_INLINE CStatusBarCtrl& CStatusBar::GetStatusBarCtrl() const
	{ return *(CStatusBarCtrl*)this; }
_AFXEXT_INLINE void CStatusBar::SetBorders(LPCRECT lpRect)
	{ SetBorders(lpRect->left, lpRect->top, lpRect->right, lpRect->bottom); }
_AFXEXT_INLINE void CStatusBar::SetBorders(int cxLeft, int cyTop, int cxRight, int cyBottom)
	{ ASSERT(cyTop >= 2); CControlBar::SetBorders(cxLeft, cyTop, cxRight, cyBottom); }
#ifdef _DEBUG
// status bars do not support docking
_AFXEXT_INLINE void CStatusBar::EnableDocking(DWORD)
	{ ASSERT(FALSE); }
#endif

// CReBar
_AFXEXT_INLINE CReBarCtrl& CReBar::GetReBarCtrl() const
	{ return *(CReBarCtrl*)this; }
#ifdef _DEBUG
// rebars do not support docking
_AFXEXT_INLINE void CReBar::EnableDocking(DWORD)
	{ ASSERT(FALSE); }
#endif

// CRectTracker
_AFXEXT_INLINE CRectTracker::CRectTracker()
	{ Construct(); }

// CBitmapButton
_AFXEXT_INLINE CBitmapButton::CBitmapButton()
	{ }
_AFXEXT_INLINE BOOL CBitmapButton::LoadBitmaps(UINT nIDBitmapResource,
	UINT nIDBitmapResourceSel, UINT nIDBitmapResourceFocus,
	UINT nIDBitmapResourceDisabled)
	{ return LoadBitmaps(MAKEINTRESOURCE(nIDBitmapResource),
		MAKEINTRESOURCE(nIDBitmapResourceSel),
		MAKEINTRESOURCE(nIDBitmapResourceFocus),
		MAKEINTRESOURCE(nIDBitmapResourceDisabled)); }

// CPrintInfo
_AFXEXT_INLINE void CPrintInfo::SetMinPage(UINT nMinPage)
	{ m_pPD->m_pd.nMinPage = (WORD)nMinPage; }
_AFXEXT_INLINE void CPrintInfo::SetMaxPage(UINT nMaxPage)
	{ m_pPD->m_pd.nMaxPage = (WORD)nMaxPage; }
_AFXEXT_INLINE UINT CPrintInfo::GetMinPage() const
	{ return m_pPD->m_pd.nMinPage; }
_AFXEXT_INLINE UINT CPrintInfo::GetMaxPage() const
	{ return m_pPD->m_pd.nMaxPage; }
_AFXEXT_INLINE UINT CPrintInfo::GetFromPage() const
	{ return m_pPD->m_pd.nFromPage; }
_AFXEXT_INLINE UINT CPrintInfo::GetToPage() const
	{ return m_pPD->m_pd.nToPage; }
// CEditView
_AFXEXT_INLINE CEdit& CEditView::GetEditCtrl() const
	{ return *(CEdit*)this; }

#endif //_AFXEXT_INLINE

/////////////////////////////////////////////////////////////////////////////
