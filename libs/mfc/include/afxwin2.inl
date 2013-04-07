// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXWIN.H (part 2)

#ifdef _AFXWIN_INLINE

// CWnd
_AFXWIN_INLINE CWnd::operator HWND() const
	{ return this == NULL ? NULL : m_hWnd; }
_AFXWIN_INLINE BOOL CWnd::operator==(const CWnd& wnd) const
	{ return ((HWND) wnd) == m_hWnd; }
_AFXWIN_INLINE BOOL CWnd::operator!=(const CWnd& wnd) const
	{ return ((HWND) wnd) != m_hWnd; }
_AFXWIN_INLINE HWND CWnd::GetSafeHwnd() const
	{ return this == NULL ? NULL : m_hWnd; }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE DWORD CWnd::GetStyle() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD)GetWindowLong(m_hWnd, GWL_STYLE); }
_AFXWIN_INLINE DWORD CWnd::GetExStyle() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD)GetWindowLong(m_hWnd, GWL_EXSTYLE); }
_AFXWIN_INLINE BOOL CWnd::ModifyStyle(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
	{ ASSERT(::IsWindow(m_hWnd)); return ModifyStyle(m_hWnd, dwRemove, dwAdd, nFlags); }
_AFXWIN_INLINE BOOL CWnd::ModifyStyleEx(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
	{ ASSERT(::IsWindow(m_hWnd)); return ModifyStyleEx(m_hWnd, dwRemove, dwAdd, nFlags); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* CWnd::GetOwner() const
	{ return m_hWndOwner != NULL ? CWnd::FromHandle(m_hWndOwner) : GetParent(); }
_AFXWIN_INLINE void CWnd::SetOwner(CWnd* pOwnerWnd)
	{ m_hWndOwner = pOwnerWnd != NULL ? pOwnerWnd->m_hWnd : NULL; }
_AFXWIN_INLINE LRESULT CWnd::SendMessage(UINT message, WPARAM wParam, LPARAM lParam)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, message, wParam, lParam); }
_AFXWIN_INLINE BOOL CWnd::PostMessage(UINT message, WPARAM wParam, LPARAM lParam)
	{ ASSERT(::IsWindow(m_hWnd)); return ::PostMessage(m_hWnd, message, wParam, lParam); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE void CWnd::SetWindowText(LPCTSTR lpszString)
	{ ASSERT(::IsWindow(m_hWnd)); ::SetWindowText(m_hWnd, lpszString); }
_AFXWIN_INLINE int CWnd::GetWindowText(LPTSTR lpszString, int nMaxCount) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetWindowText(m_hWnd, lpszString, nMaxCount); }
_AFXWIN_INLINE int CWnd::GetWindowTextLength() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetWindowTextLength(m_hWnd); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE void CWnd::SetFont(CFont* pFont, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_SETFONT, (WPARAM)pFont->GetSafeHandle(), bRedraw); }
_AFXWIN_INLINE CFont* CWnd::GetFont() const
	{ ASSERT(::IsWindow(m_hWnd)); return CFont::FromHandle(
		(HFONT)::SendMessage(m_hWnd, WM_GETFONT, 0, 0)); }
_AFXWIN_INLINE CMenu* CWnd::GetMenu() const
	{ ASSERT(::IsWindow(m_hWnd)); return CMenu::FromHandle(::GetMenu(m_hWnd)); }
_AFXWIN_INLINE BOOL CWnd::SetMenu(CMenu* pMenu)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetMenu(m_hWnd, pMenu->GetSafeHmenu()); }
_AFXWIN_INLINE void CWnd::DrawMenuBar()
	{ ASSERT(::IsWindow(m_hWnd)); ::DrawMenuBar(m_hWnd); }
_AFXWIN_INLINE CMenu* CWnd::GetSystemMenu(BOOL bRevert) const
	{ ASSERT(::IsWindow(m_hWnd)); return CMenu::FromHandle(::GetSystemMenu(m_hWnd, bRevert)); }
_AFXWIN_INLINE BOOL CWnd::HiliteMenuItem(CMenu* pMenu, UINT nIDHiliteItem, UINT nHilite)
	{ ASSERT(::IsWindow(m_hWnd)); return ::HiliteMenuItem(m_hWnd, pMenu->m_hMenu, nIDHiliteItem, nHilite); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE int CWnd::GetDlgCtrlID() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetDlgCtrlID(m_hWnd); }
_AFXWIN_INLINE int CWnd::SetDlgCtrlID(int nID)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SetWindowLong(m_hWnd, GWL_ID, nID); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::IsIconic() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsIconic(m_hWnd); }
_AFXWIN_INLINE BOOL CWnd::IsZoomed() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsZoomed(m_hWnd); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE void CWnd::MoveWindow(int x, int y, int nWidth, int nHeight, BOOL bRepaint)
	{ ASSERT(::IsWindow(m_hWnd)); ::MoveWindow(m_hWnd, x, y, nWidth, nHeight, bRepaint); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE void CWnd::MoveWindow(LPCRECT lpRect, BOOL bRepaint)
	{ MoveWindow(lpRect->left, lpRect->top, lpRect->right - lpRect->left,
		lpRect->bottom - lpRect->top, bRepaint); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::SetWindowPos(const CWnd* pWndInsertAfter, int x, int y, int cx, int cy, UINT nFlags)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetWindowPos(m_hWnd, pWndInsertAfter->GetSafeHwnd(),
		x, y, cx, cy, nFlags); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE UINT CWnd::ArrangeIconicWindows()
	{ ASSERT(::IsWindow(m_hWnd)); return ::ArrangeIconicWindows(m_hWnd); }
_AFXWIN_INLINE int CWnd::SetWindowRgn(HRGN hRgn, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetWindowRgn(m_hWnd, hRgn, bRedraw); }
_AFXWIN_INLINE int CWnd::GetWindowRgn(HRGN hRgn) const
	{ ASSERT(::IsWindow(m_hWnd) && hRgn != NULL); return ::GetWindowRgn(m_hWnd, hRgn); }
_AFXWIN_INLINE void CWnd::BringWindowToTop()
	{ ASSERT(::IsWindow(m_hWnd)); ::BringWindowToTop(m_hWnd); }
_AFXWIN_INLINE void CWnd::GetWindowRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::GetWindowRect(m_hWnd, lpRect); }
_AFXWIN_INLINE void CWnd::GetClientRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::GetClientRect(m_hWnd, lpRect); }
_AFXWIN_INLINE void CWnd::MapWindowPoints(CWnd* pwndTo, LPPOINT lpPoint, UINT nCount) const
	{ ASSERT(::IsWindow(m_hWnd)); ::MapWindowPoints(m_hWnd, pwndTo->GetSafeHwnd(), lpPoint, nCount); }
_AFXWIN_INLINE void CWnd::MapWindowPoints(CWnd* pwndTo, LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::MapWindowPoints(m_hWnd, pwndTo->GetSafeHwnd(), (LPPOINT)lpRect, 2); }
_AFXWIN_INLINE void CWnd::ClientToScreen(LPPOINT lpPoint) const
	{ ASSERT(::IsWindow(m_hWnd)); ::ClientToScreen(m_hWnd, lpPoint); }
_AFXWIN_INLINE void CWnd::ScreenToClient(LPPOINT lpPoint) const
	{ ASSERT(::IsWindow(m_hWnd)); ::ScreenToClient(m_hWnd, lpPoint); }
_AFXWIN_INLINE CDC* CWnd::BeginPaint(LPPAINTSTRUCT lpPaint)
	{ ASSERT(::IsWindow(m_hWnd)); return CDC::FromHandle(::BeginPaint(m_hWnd, lpPaint)); }
_AFXWIN_INLINE void CWnd::EndPaint(LPPAINTSTRUCT lpPaint)
	{ ASSERT(::IsWindow(m_hWnd)); ::EndPaint(m_hWnd, lpPaint); }
_AFXWIN_INLINE CDC* CWnd::GetDC()
	{ ASSERT(::IsWindow(m_hWnd)); return CDC::FromHandle(::GetDC(m_hWnd)); }
_AFXWIN_INLINE CDC* CWnd::GetWindowDC()
	{ ASSERT(::IsWindow(m_hWnd)); return CDC::FromHandle(::GetWindowDC(m_hWnd)); }
_AFXWIN_INLINE int CWnd::ReleaseDC(CDC* pDC)
	{ ASSERT(::IsWindow(m_hWnd)); return ::ReleaseDC(m_hWnd, pDC->m_hDC); }
_AFXWIN_INLINE void CWnd::UpdateWindow()
	{ ASSERT(::IsWindow(m_hWnd)); ::UpdateWindow(m_hWnd); }
_AFXWIN_INLINE void CWnd::SetRedraw(BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_SETREDRAW, bRedraw, 0); }
_AFXWIN_INLINE BOOL CWnd::GetUpdateRect(LPRECT lpRect, BOOL bErase)
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetUpdateRect(m_hWnd, lpRect, bErase); }
_AFXWIN_INLINE int CWnd::GetUpdateRgn(CRgn* pRgn, BOOL bErase)
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetUpdateRgn(m_hWnd, (HRGN)pRgn->GetSafeHandle(), bErase); }
_AFXWIN_INLINE void CWnd::Invalidate(BOOL bErase)
	{ ASSERT(::IsWindow(m_hWnd)); ::InvalidateRect(m_hWnd, NULL, bErase); }
_AFXWIN_INLINE void CWnd::InvalidateRect(LPCRECT lpRect, BOOL bErase)
	{ ASSERT(::IsWindow(m_hWnd)); ::InvalidateRect(m_hWnd, lpRect, bErase); }
_AFXWIN_INLINE void CWnd::InvalidateRgn(CRgn* pRgn, BOOL bErase)
	{ ASSERT(::IsWindow(m_hWnd)); ::InvalidateRgn(m_hWnd, (HRGN)pRgn->GetSafeHandle(), bErase); }
_AFXWIN_INLINE void CWnd::ValidateRect(LPCRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::ValidateRect(m_hWnd, lpRect); }
_AFXWIN_INLINE void CWnd::ValidateRgn(CRgn* pRgn)
	{ ASSERT(::IsWindow(m_hWnd)); ::ValidateRgn(m_hWnd, (HRGN)pRgn->GetSafeHandle()); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::ShowWindow(int nCmdShow)
	{ ASSERT(::IsWindow(m_hWnd)); return ::ShowWindow(m_hWnd, nCmdShow); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::IsWindowVisible() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsWindowVisible(m_hWnd); }
_AFXWIN_INLINE void CWnd::ShowOwnedPopups(BOOL bShow)
	{ ASSERT(::IsWindow(m_hWnd)); ::ShowOwnedPopups(m_hWnd, bShow); }
_AFXWIN_INLINE void CWnd::SendMessageToDescendants(
	UINT message, WPARAM wParam, LPARAM lParam, BOOL bDeep, BOOL bOnlyPerm)
	{ ASSERT(::IsWindow(m_hWnd)); CWnd::SendMessageToDescendants(m_hWnd, message, wParam, lParam, bDeep,
		bOnlyPerm); }
_AFXWIN_INLINE CWnd* CWnd::GetDescendantWindow(int nID, BOOL bOnlyPerm) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::GetDescendantWindow(m_hWnd, nID, bOnlyPerm); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::IsDialogMessage(LPMSG lpMsg)
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsDialogMessage(m_hWnd, lpMsg); }
#endif

_AFXWIN_INLINE CDC* CWnd::GetDCEx(CRgn* prgnClip, DWORD flags)
	{ ASSERT(::IsWindow(m_hWnd)); return CDC::FromHandle(::GetDCEx(m_hWnd, (HRGN)prgnClip->GetSafeHandle(), flags)); }
_AFXWIN_INLINE BOOL CWnd::LockWindowUpdate()
	{ ASSERT(::IsWindow(m_hWnd)); return ::LockWindowUpdate(m_hWnd); }
_AFXWIN_INLINE void CWnd::UnlockWindowUpdate()
	{ ASSERT(::IsWindow(m_hWnd)); ::LockWindowUpdate(NULL); }
_AFXWIN_INLINE BOOL CWnd::RedrawWindow(LPCRECT lpRectUpdate, CRgn* prgnUpdate,
	UINT flags)
	{ ASSERT(::IsWindow(m_hWnd)); return ::RedrawWindow(m_hWnd, lpRectUpdate, (HRGN)prgnUpdate->GetSafeHandle(), flags); }
_AFXWIN_INLINE BOOL CWnd::EnableScrollBar(int nSBFlags, UINT nArrowFlags)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::EnableScrollBar(m_hWnd, nSBFlags, nArrowFlags); }

_AFXWIN_INLINE UINT CWnd::SetTimer(UINT nIDEvent, UINT nElapse,
		void (CALLBACK* lpfnTimer)(HWND, UINT, UINT, DWORD))
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetTimer(m_hWnd, nIDEvent, nElapse,
		(TIMERPROC)lpfnTimer); }
_AFXWIN_INLINE BOOL CWnd::KillTimer(int nIDEvent)
	{ ASSERT(::IsWindow(m_hWnd)); return ::KillTimer(m_hWnd, nIDEvent); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE BOOL CWnd::IsWindowEnabled() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsWindowEnabled(m_hWnd); }
_AFXWIN_INLINE BOOL CWnd::EnableWindow(BOOL bEnable)
	{ ASSERT(::IsWindow(m_hWnd)); return ::EnableWindow(m_hWnd, bEnable); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetActiveWindow()
	{ return CWnd::FromHandle(::GetActiveWindow()); }
_AFXWIN_INLINE CWnd* CWnd::SetActiveWindow()
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::SetActiveWindow(m_hWnd)); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetCapture()
	{ return CWnd::FromHandle(::GetCapture()); }
_AFXWIN_INLINE CWnd* CWnd::SetCapture()
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::SetCapture(m_hWnd)); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetFocus()
	{ return CWnd::FromHandle(::GetFocus()); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* CWnd::SetFocus()
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::SetFocus(m_hWnd)); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetDesktopWindow()
	{ return CWnd::FromHandle(::GetDesktopWindow()); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE void CWnd::CheckDlgButton(int nIDButton, UINT nCheck)
	{ ASSERT(::IsWindow(m_hWnd)); ::CheckDlgButton(m_hWnd, nIDButton, nCheck); }
_AFXWIN_INLINE void CWnd::CheckRadioButton(int nIDFirstButton, int nIDLastButton,
		int nIDCheckButton)
	{ ASSERT(::IsWindow(m_hWnd)); ::CheckRadioButton(m_hWnd, nIDFirstButton, nIDLastButton, nIDCheckButton); }
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE int CWnd::DlgDirList(LPTSTR lpPathSpec, int nIDListBox,
		int nIDStaticPath, UINT nFileType)
	{ ASSERT(::IsWindow(m_hWnd)); return ::DlgDirList(m_hWnd, lpPathSpec, nIDListBox,
			nIDStaticPath, nFileType); }
_AFXWIN_INLINE int CWnd::DlgDirListComboBox(LPTSTR lpPathSpec, int nIDComboBox,
		int nIDStaticPath, UINT nFileType)
	{ ASSERT(::IsWindow(m_hWnd)); return ::DlgDirListComboBox(m_hWnd, lpPathSpec,
			nIDComboBox, nIDStaticPath, nFileType); }
_AFXWIN_INLINE BOOL CWnd::DlgDirSelect(LPTSTR lpString, int nIDListBox)
	{ ASSERT(::IsWindow(m_hWnd)); return ::DlgDirSelectEx(m_hWnd, lpString, _MAX_PATH, nIDListBox); }
_AFXWIN_INLINE BOOL CWnd::DlgDirSelectComboBox(LPTSTR lpString, int nIDComboBox)
	{ ASSERT(::IsWindow(m_hWnd)); return ::DlgDirSelectComboBoxEx(m_hWnd, lpString, _MAX_PATH, nIDComboBox);}
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* CWnd::GetDlgItem(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetDlgItem(m_hWnd, nID)); }
_AFXWIN_INLINE void CWnd::GetDlgItem(int nID, HWND* phWnd) const
	{ ASSERT(::IsWindow(m_hWnd)); ASSERT(phWnd != NULL); *phWnd = ::GetDlgItem(m_hWnd, nID); }
_AFXWIN_INLINE UINT CWnd::GetDlgItemInt(int nID, BOOL* lpTrans,
		BOOL bSigned) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetDlgItemInt(m_hWnd, nID, lpTrans, bSigned);}
_AFXWIN_INLINE int CWnd::GetDlgItemText(int nID, LPTSTR lpStr, int nMaxCount) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetDlgItemText(m_hWnd, nID, lpStr, nMaxCount);}
#endif //_AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE CWnd* CWnd::GetNextDlgGroupItem(CWnd* pWndCtl, BOOL bPrevious) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetNextDlgGroupItem(m_hWnd,
			pWndCtl->GetSafeHwnd(), bPrevious)); }
_AFXWIN_INLINE CWnd* CWnd::GetNextDlgTabItem(CWnd* pWndCtl, BOOL bPrevious) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetNextDlgTabItem(m_hWnd,
			pWndCtl->GetSafeHwnd(), bPrevious)); }
#ifdef _AFX_NO_OCC_SUPPORT
_AFXWIN_INLINE UINT CWnd::IsDlgButtonChecked(int nIDButton) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsDlgButtonChecked(m_hWnd, nIDButton); }
_AFXWIN_INLINE LPARAM CWnd::SendDlgItemMessage(int nID, UINT message, WPARAM wParam, LPARAM lParam)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendDlgItemMessage(m_hWnd, nID, message, wParam, lParam); }
_AFXWIN_INLINE void CWnd::SetDlgItemInt(int nID, UINT nValue, BOOL bSigned)
	{ ASSERT(::IsWindow(m_hWnd)); ::SetDlgItemInt(m_hWnd, nID, nValue, bSigned); }
_AFXWIN_INLINE void CWnd::SetDlgItemText(int nID, LPCTSTR lpszString)
	{ ASSERT(::IsWindow(m_hWnd)); ::SetDlgItemText(m_hWnd, nID, lpszString); }
_AFXWIN_INLINE int CWnd::ScrollWindowEx(int dx, int dy,
	LPCRECT lpRectScroll, LPCRECT lpRectClip,
	CRgn* prgnUpdate, LPRECT lpRectUpdate, UINT flags)
	{ ASSERT(::IsWindow(m_hWnd)); return ::ScrollWindowEx(m_hWnd, dx, dy, lpRectScroll, lpRectClip,
			(HRGN)prgnUpdate->GetSafeHandle(), lpRectUpdate, flags); }
#endif //_AFX_NO_OCC_SUPPORT

_AFXWIN_INLINE void CWnd::ShowScrollBar(UINT nBar, BOOL bShow)
	{ ASSERT(::IsWindow(m_hWnd)); ::ShowScrollBar(m_hWnd, nBar, bShow); }
_AFXWIN_INLINE CWnd* CWnd::ChildWindowFromPoint(POINT point) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::ChildWindowFromPoint(m_hWnd, point)); }
_AFXWIN_INLINE CWnd* CWnd::ChildWindowFromPoint(POINT point, UINT nFlags) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::ChildWindowFromPointEx(m_hWnd, point, nFlags)); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::FindWindow(LPCTSTR lpszClassName, LPCTSTR lpszWindowName)
	{ return CWnd::FromHandle(::FindWindow(lpszClassName, lpszWindowName)); }
_AFXWIN_INLINE CWnd* CWnd::GetNextWindow(UINT nFlag) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetNextWindow(m_hWnd, nFlag)); }
_AFXWIN_INLINE CWnd* CWnd::GetTopWindow() const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetTopWindow(m_hWnd)); }
_AFXWIN_INLINE CWnd* CWnd::GetWindow(UINT nCmd) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetWindow(m_hWnd, nCmd)); }
_AFXWIN_INLINE CWnd* CWnd::GetLastActivePopup() const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetLastActivePopup(m_hWnd)); }
_AFXWIN_INLINE BOOL CWnd::IsChild(const CWnd* pWnd) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::IsChild(m_hWnd, pWnd->GetSafeHwnd()); }
_AFXWIN_INLINE CWnd* CWnd::GetParent() const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::GetParent(m_hWnd)); }
_AFXWIN_INLINE CWnd* CWnd::SetParent(CWnd* pWndNewParent)
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle(::SetParent(m_hWnd,
			pWndNewParent->GetSafeHwnd())); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::WindowFromPoint(POINT point)
	{ return CWnd::FromHandle(::WindowFromPoint(point)); }
_AFXWIN_INLINE BOOL CWnd::FlashWindow(BOOL bInvert)
	{ ASSERT(::IsWindow(m_hWnd)); return ::FlashWindow(m_hWnd, bInvert); }
_AFXWIN_INLINE BOOL CWnd::ChangeClipboardChain(HWND hWndNext)
	{ ASSERT(::IsWindow(m_hWnd)); return ::ChangeClipboardChain(m_hWnd, hWndNext); }
_AFXWIN_INLINE HWND CWnd::SetClipboardViewer()
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetClipboardViewer(m_hWnd); }
_AFXWIN_INLINE BOOL CWnd::OpenClipboard()
	{ ASSERT(::IsWindow(m_hWnd)); return ::OpenClipboard(m_hWnd); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetOpenClipboardWindow()
	{ return CWnd::FromHandle(::GetOpenClipboardWindow()); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetClipboardOwner()
	{ return CWnd::FromHandle(::GetClipboardOwner()); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetClipboardViewer()
	{ return CWnd::FromHandle(::GetClipboardViewer()); }
_AFXWIN_INLINE void CWnd::CreateCaret(CBitmap* pBitmap)
	{ ASSERT(::IsWindow(m_hWnd)); ::CreateCaret(m_hWnd, (HBITMAP)pBitmap->GetSafeHandle(), 0, 0); }
_AFXWIN_INLINE void CWnd::CreateSolidCaret(int nWidth, int nHeight)
	{ ASSERT(::IsWindow(m_hWnd)); ::CreateCaret(m_hWnd, (HBITMAP)0, nWidth, nHeight); }
_AFXWIN_INLINE void CWnd::CreateGrayCaret(int nWidth, int nHeight)
	{ ASSERT(::IsWindow(m_hWnd)); ::CreateCaret(m_hWnd, (HBITMAP)1, nWidth, nHeight); }
_AFXWIN_INLINE CPoint PASCAL CWnd::GetCaretPos()
	{ CPoint point; ::GetCaretPos((LPPOINT)&point); return point; }
_AFXWIN_INLINE void PASCAL CWnd::SetCaretPos(POINT point)
	{ ::SetCaretPos(point.x, point.y); }
_AFXWIN_INLINE void CWnd::HideCaret()
	{ ASSERT(::IsWindow(m_hWnd)); ::HideCaret(m_hWnd); }
_AFXWIN_INLINE void CWnd::ShowCaret()
	{ ASSERT(::IsWindow(m_hWnd)); ::ShowCaret(m_hWnd); }
_AFXWIN_INLINE BOOL CWnd::SetForegroundWindow()
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetForegroundWindow(m_hWnd); }
_AFXWIN_INLINE CWnd* PASCAL CWnd::GetForegroundWindow()
	{ return CWnd::FromHandle(::GetForegroundWindow()); }

_AFXWIN_INLINE BOOL CWnd::SendNotifyMessage(UINT message, WPARAM wParam, LPARAM lParam)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendNotifyMessage(m_hWnd, message, wParam, lParam); }

// Win4
_AFXWIN_INLINE HICON CWnd::SetIcon(HICON hIcon, BOOL bBigIcon)
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, WM_SETICON, bBigIcon, (LPARAM)hIcon); }
_AFXWIN_INLINE HICON CWnd::GetIcon(BOOL bBigIcon) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, WM_GETICON, bBigIcon, 0); }
_AFXWIN_INLINE void CWnd::Print(CDC* pDC, DWORD dwFlags) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_PRINT, (WPARAM)pDC->GetSafeHdc(), dwFlags); }
_AFXWIN_INLINE void CWnd::PrintClient(CDC* pDC, DWORD dwFlags) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_PRINTCLIENT, (WPARAM)pDC->GetSafeHdc(), dwFlags); }
_AFXWIN_INLINE BOOL CWnd::SetWindowContextHelpId(DWORD dwContextHelpId)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetWindowContextHelpId(m_hWnd, dwContextHelpId); }
_AFXWIN_INLINE DWORD CWnd::GetWindowContextHelpId() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetWindowContextHelpId(m_hWnd); }


// Default message map implementations
_AFXWIN_INLINE void CWnd::OnActivateApp(BOOL, HTASK)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnActivate(UINT, CWnd*, BOOL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnCancelMode()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnChildActivate()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnClose()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnContextMenu(CWnd*, CPoint)
	{ Default(); }
_AFXWIN_INLINE int CWnd::OnCopyData(CWnd*, COPYDATASTRUCT*)
	{ return (int)Default(); }
_AFXWIN_INLINE int CWnd::OnCreate(LPCREATESTRUCT)
	{ return (int)Default(); }
_AFXWIN_INLINE void CWnd::OnEnable(BOOL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnEndSession(BOOL)
	{ Default(); }
_AFXWIN_INLINE BOOL CWnd::OnEraseBkgnd(CDC*)
	{ return (BOOL)Default(); }
_AFXWIN_INLINE void CWnd::OnGetMinMaxInfo(MINMAXINFO*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnIconEraseBkgnd(CDC*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnKillFocus(CWnd*)
	{ Default(); }
_AFXWIN_INLINE LRESULT CWnd::OnMenuChar(UINT, UINT, CMenu*)
	{ return Default(); }
_AFXWIN_INLINE void CWnd::OnMenuSelect(UINT, UINT, HMENU)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnMove(int, int)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnPaint()
	{ Default(); }
_AFXWIN_INLINE HCURSOR CWnd::OnQueryDragIcon()
	{ return (HCURSOR)Default(); }
_AFXWIN_INLINE BOOL CWnd::OnQueryEndSession()
	{ return (BOOL)Default(); }
_AFXWIN_INLINE BOOL CWnd::OnQueryNewPalette()
	{ return (BOOL)Default(); }
_AFXWIN_INLINE BOOL CWnd::OnQueryOpen()
	{ return (BOOL)Default(); }
_AFXWIN_INLINE BOOL CWnd::OnSetCursor(CWnd*, UINT, UINT)
	{ return (BOOL)Default(); }
_AFXWIN_INLINE void CWnd::OnSetFocus(CWnd*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnShowWindow(BOOL, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSize(UINT, int, int)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnTCard(UINT, DWORD)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnWindowPosChanging(WINDOWPOS*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnWindowPosChanged(WINDOWPOS*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnDropFiles(HDROP)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnPaletteIsChanging(CWnd*)
	{ Default(); }
_AFXWIN_INLINE BOOL CWnd::OnNcActivate(BOOL)
	{ return (BOOL)Default(); }
_AFXWIN_INLINE void CWnd::OnNcCalcSize(BOOL, NCCALCSIZE_PARAMS*)
	{ Default(); }
_AFXWIN_INLINE BOOL CWnd::OnNcCreate(LPCREATESTRUCT)
	{ return (BOOL)Default(); }
_AFXWIN_INLINE UINT CWnd::OnNcHitTest(CPoint)
	{ return (UINT)Default(); }
_AFXWIN_INLINE void CWnd::OnNcLButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcLButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcLButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcMButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcMButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcMButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcMouseMove(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcPaint()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcRButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcRButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnNcRButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSysChar(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSysCommand(UINT, LPARAM)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSysDeadChar(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSysKeyDown(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSysKeyUp(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnCompacting(UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnFontChange()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnPaletteChanged(CWnd*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSpoolerStatus(UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnTimeChange()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnChar(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnDeadChar(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnKeyDown(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnKeyUp(UINT, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnLButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnLButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnLButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnMButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnMButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnMButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE int CWnd::OnMouseActivate(CWnd*, UINT, UINT)
	{ return (int)Default(); }
_AFXWIN_INLINE void CWnd::OnMouseMove(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE BOOL CWnd::OnMouseWheel(UINT, short, CPoint)
	{ return (BOOL)Default(); }
_AFXWIN_INLINE LRESULT CWnd::OnRegisteredMouseWheel(WPARAM, LPARAM)
	{ return Default(); }
_AFXWIN_INLINE void CWnd::OnRButtonDblClk(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnRButtonDown(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnRButtonUp(UINT, CPoint)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnTimer(UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnInitMenu(CMenu*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnInitMenuPopup(CMenu*, UINT, BOOL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnAskCbFormatName(UINT, LPTSTR)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnChangeCbChain(HWND, HWND)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnDestroyClipboard()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnDrawClipboard()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnHScrollClipboard(CWnd*, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnPaintClipboard(CWnd*, HGLOBAL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnRenderAllFormats()
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnRenderFormat(UINT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSizeClipboard(CWnd*, HGLOBAL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnVScrollClipboard(CWnd*, UINT, UINT)
	{ Default(); }
_AFXWIN_INLINE UINT CWnd::OnGetDlgCode()
	{ return (UINT)Default(); }
_AFXWIN_INLINE void CWnd::OnMDIActivate(BOOL, CWnd*, CWnd*)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnEnterMenuLoop(BOOL)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnExitMenuLoop(BOOL)
	{ Default(); }
// Win4 support
_AFXWIN_INLINE void CWnd::OnStyleChanged(int, LPSTYLESTRUCT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnStyleChanging(int, LPSTYLESTRUCT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnSizing(UINT, LPRECT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnMoving(UINT, LPRECT)
	{ Default(); }
_AFXWIN_INLINE void CWnd::OnCaptureChanged(CWnd*)
	{ Default(); }
_AFXWIN_INLINE BOOL CWnd::OnDeviceChange(UINT, DWORD)
	{ return (BOOL)Default(); }

// CWnd dialog data support
_AFXWIN_INLINE void CWnd::DoDataExchange(CDataExchange*)
	{ } // default does nothing

// CWnd modality support
_AFXWIN_INLINE void CWnd::BeginModalState()
	{ ::EnableWindow(m_hWnd, FALSE); }
_AFXWIN_INLINE void CWnd::EndModalState()
	{ ::EnableWindow(m_hWnd, TRUE); }

// CFrameWnd
_AFXWIN_INLINE void CFrameWnd::DelayUpdateFrameTitle()
	{ m_nIdleFlags |= idleTitle; }
_AFXWIN_INLINE void CFrameWnd::DelayRecalcLayout(BOOL bNotify)
	{ m_nIdleFlags |= (idleLayout | (bNotify ? idleNotify : 0)); };
_AFXWIN_INLINE BOOL CFrameWnd::InModalState() const
	{ return m_cModalStack != 0; }
_AFXWIN_INLINE void CFrameWnd::AddControlBar(CControlBar *pBar)
	{ m_listControlBars.AddTail(pBar); }
_AFXWIN_INLINE void CFrameWnd::SetTitle(LPCTSTR lpszTitle)
	{ m_strTitle = lpszTitle; }
_AFXWIN_INLINE CString CFrameWnd::GetTitle() const
	{ return m_strTitle; }

// CDialog
_AFXWIN_INLINE BOOL CDialog::Create(UINT nIDTemplate, CWnd* pParentWnd)
	{ return Create(MAKEINTRESOURCE(nIDTemplate), pParentWnd); }
_AFXWIN_INLINE void CDialog::MapDialogRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::MapDialogRect(m_hWnd, lpRect); }
_AFXWIN_INLINE void CDialog::SetHelpID(UINT nIDR)
	{ m_nIDHelp = nIDR; }
_AFXWIN_INLINE void CDialog::NextDlgCtrl() const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_NEXTDLGCTL, 0, 0); }
_AFXWIN_INLINE void CDialog::PrevDlgCtrl() const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_NEXTDLGCTL, 1, 0); }
_AFXWIN_INLINE void CDialog::GotoDlgCtrl(CWnd* pWndCtrl)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_NEXTDLGCTL, (WPARAM)pWndCtrl->m_hWnd, 1L); }
_AFXWIN_INLINE void CDialog::SetDefID(UINT nID)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, DM_SETDEFID, nID, 0); }
_AFXWIN_INLINE DWORD CDialog::GetDefID() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, DM_GETDEFID, 0, 0); }

// Window control functions
_AFXWIN_INLINE CStatic::CStatic()
	{ }
_AFXWIN_INLINE HICON CStatic::SetIcon(HICON hIcon)
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, STM_SETICON, (WPARAM)hIcon, 0L); }
_AFXWIN_INLINE HICON CStatic::GetIcon() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, STM_GETICON, 0, 0L); }
#if (WINVER >= 0x400)
_AFXWIN_INLINE HENHMETAFILE CStatic::SetEnhMetaFile(HENHMETAFILE hMetaFile)
	{ ASSERT(::IsWindow(m_hWnd)); return (HENHMETAFILE)::SendMessage(m_hWnd, STM_SETIMAGE, IMAGE_ENHMETAFILE, (LPARAM)hMetaFile); }
_AFXWIN_INLINE HENHMETAFILE CStatic::GetEnhMetaFile() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HENHMETAFILE)::SendMessage(m_hWnd, STM_GETIMAGE, IMAGE_ENHMETAFILE, 0L); }
_AFXWIN_INLINE HBITMAP CStatic::SetBitmap(HBITMAP hBitmap)
	{ ASSERT(::IsWindow(m_hWnd)); return (HBITMAP)::SendMessage(m_hWnd, STM_SETIMAGE, IMAGE_BITMAP, (LPARAM)hBitmap); }
_AFXWIN_INLINE HBITMAP CStatic::GetBitmap() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HBITMAP)::SendMessage(m_hWnd, STM_GETIMAGE, IMAGE_BITMAP, 0L); }
_AFXWIN_INLINE HCURSOR CStatic::SetCursor(HCURSOR hCursor)
	{ ASSERT(::IsWindow(m_hWnd)); return (HCURSOR)::SendMessage(m_hWnd, STM_SETIMAGE, IMAGE_CURSOR, (LPARAM)hCursor); }
_AFXWIN_INLINE HCURSOR CStatic::GetCursor()
	{ ASSERT(::IsWindow(m_hWnd)); return (HCURSOR)::SendMessage(m_hWnd, STM_GETIMAGE, IMAGE_CURSOR, 0L); }
#endif

_AFXWIN_INLINE CButton::CButton()
	{ }
_AFXWIN_INLINE UINT CButton::GetState() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, BM_GETSTATE, 0, 0); }
_AFXWIN_INLINE void CButton::SetState(BOOL bHighlight)
	{ ::SendMessage(m_hWnd, BM_SETSTATE, bHighlight, 0); }
_AFXWIN_INLINE int CButton::GetCheck() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, BM_GETCHECK, 0, 0); }
_AFXWIN_INLINE void CButton::SetCheck(int nCheck)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, BM_SETCHECK, nCheck, 0); }
_AFXWIN_INLINE UINT CButton::GetButtonStyle() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)GetWindowLong(m_hWnd, GWL_STYLE) & 0xff; }
_AFXWIN_INLINE void CButton::SetButtonStyle(UINT nStyle, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, BM_SETSTYLE, nStyle, (LPARAM)bRedraw); }
// Win4
#if (WINVER >= 0x400)
_AFXWIN_INLINE HICON CButton::SetIcon(HICON hIcon)
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, BM_SETIMAGE, IMAGE_ICON, (LPARAM)hIcon); }
_AFXWIN_INLINE HICON CButton::GetIcon() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HICON)::SendMessage(m_hWnd, BM_GETIMAGE, IMAGE_ICON, 0L); }
_AFXWIN_INLINE HBITMAP CButton::SetBitmap(HBITMAP hBitmap)
	{ ASSERT(::IsWindow(m_hWnd)); return (HBITMAP)::SendMessage(m_hWnd, BM_SETIMAGE, IMAGE_BITMAP, (LPARAM)hBitmap); }
_AFXWIN_INLINE HBITMAP CButton::GetBitmap() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HBITMAP)::SendMessage(m_hWnd, BM_GETIMAGE, IMAGE_BITMAP, 0L); }
_AFXWIN_INLINE HCURSOR CButton::SetCursor(HCURSOR hCursor)
	{ ASSERT(::IsWindow(m_hWnd)); return (HCURSOR)::SendMessage(m_hWnd, BM_SETIMAGE, IMAGE_CURSOR, (LPARAM)hCursor); }
_AFXWIN_INLINE HCURSOR CButton::GetCursor()
	{ ASSERT(::IsWindow(m_hWnd)); return (HCURSOR)::SendMessage(m_hWnd, BM_GETIMAGE, IMAGE_CURSOR, 0L); }
#endif

_AFXWIN_INLINE CListBox::CListBox()
	{ }
_AFXWIN_INLINE int CListBox::GetCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETCOUNT, 0, 0); }
_AFXWIN_INLINE int CListBox::GetCurSel() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETCURSEL, 0, 0); }
_AFXWIN_INLINE int CListBox::SetCurSel(int nSelect)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETCURSEL, nSelect, 0); }
_AFXWIN_INLINE int CListBox::GetHorizontalExtent() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETHORIZONTALEXTENT,
		0, 0); }
_AFXWIN_INLINE void CListBox::SetHorizontalExtent(int cxExtent)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, LB_SETHORIZONTALEXTENT, cxExtent, 0); }
_AFXWIN_INLINE int CListBox::GetSelCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETSELCOUNT, 0, 0); }
_AFXWIN_INLINE int CListBox::GetSelItems(int nMaxItems, LPINT rgIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETSELITEMS, nMaxItems, (LPARAM)rgIndex); }
_AFXWIN_INLINE int CListBox::GetTopIndex() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETTOPINDEX, 0, 0); }
_AFXWIN_INLINE int CListBox::SetTopIndex(int nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETTOPINDEX, nIndex, 0);}
_AFXWIN_INLINE DWORD CListBox::GetItemData(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, LB_GETITEMDATA, nIndex, 0); }
_AFXWIN_INLINE int CListBox::SetItemData(int nIndex, DWORD dwItemData)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETITEMDATA, nIndex, (LPARAM)dwItemData); }
_AFXWIN_INLINE void* CListBox::GetItemDataPtr(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (LPVOID)::SendMessage(m_hWnd, LB_GETITEMDATA, nIndex, 0); }
_AFXWIN_INLINE int CListBox::SetItemDataPtr(int nIndex, void* pData)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItemData(nIndex, (DWORD)(LPVOID)pData); }
_AFXWIN_INLINE int CListBox::GetItemRect(int nIndex, LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETITEMRECT, nIndex, (LPARAM)lpRect); }
_AFXWIN_INLINE int CListBox::GetSel(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETSEL, nIndex, 0); }
_AFXWIN_INLINE int CListBox::SetSel(int nIndex, BOOL bSelect)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETSEL, bSelect, nIndex); }
_AFXWIN_INLINE int CListBox::GetText(int nIndex, LPTSTR lpszBuffer) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETTEXT, nIndex, (LPARAM)lpszBuffer); }
_AFXWIN_INLINE int CListBox::GetTextLen(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETTEXTLEN, nIndex, 0); }
_AFXWIN_INLINE void CListBox::SetColumnWidth(int cxWidth)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, LB_SETCOLUMNWIDTH, cxWidth, 0); }
_AFXWIN_INLINE BOOL CListBox::SetTabStops(int nTabStops, LPINT rgTabStops)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, LB_SETTABSTOPS, nTabStops, (LPARAM)rgTabStops); }
_AFXWIN_INLINE void CListBox::SetTabStops()
	{ ASSERT(::IsWindow(m_hWnd)); VERIFY(::SendMessage(m_hWnd, LB_SETTABSTOPS, 0, 0)); }
_AFXWIN_INLINE BOOL CListBox::SetTabStops(const int& cxEachStop)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, LB_SETTABSTOPS, 1, (LPARAM)(LPINT)&cxEachStop); }
_AFXWIN_INLINE int CListBox::SetItemHeight(int nIndex, UINT cyItemHeight)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETITEMHEIGHT, nIndex, MAKELONG(cyItemHeight, 0)); }
_AFXWIN_INLINE int CListBox::GetItemHeight(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETITEMHEIGHT, nIndex, 0L); }
_AFXWIN_INLINE int CListBox::FindStringExact(int nIndexStart, LPCTSTR lpszFind) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_FINDSTRINGEXACT, nIndexStart, (LPARAM)lpszFind); }
_AFXWIN_INLINE int CListBox::GetCaretIndex() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETCARETINDEX, 0, 0L); }
_AFXWIN_INLINE int CListBox::SetCaretIndex(int nIndex, BOOL bScroll)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SETCARETINDEX, nIndex, MAKELONG(bScroll, 0)); }
_AFXWIN_INLINE int CListBox::AddString(LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_ADDSTRING, 0, (LPARAM)lpszItem); }
_AFXWIN_INLINE int CListBox::DeleteString(UINT nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_DELETESTRING, nIndex, 0); }
_AFXWIN_INLINE int CListBox::InsertString(int nIndex, LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_INSERTSTRING, nIndex, (LPARAM)lpszItem); }
_AFXWIN_INLINE void CListBox::ResetContent()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, LB_RESETCONTENT, 0, 0); }
_AFXWIN_INLINE int CListBox::Dir(UINT attr, LPCTSTR lpszWildCard)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_DIR, attr, (LPARAM)lpszWildCard); }
_AFXWIN_INLINE int CListBox::FindString(int nStartAfter, LPCTSTR lpszItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_FINDSTRING,
		nStartAfter, (LPARAM)lpszItem); }
_AFXWIN_INLINE int CListBox::SelectString(int nStartAfter, LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_SELECTSTRING,
		nStartAfter, (LPARAM)lpszItem); }
_AFXWIN_INLINE int CListBox::SelItemRange(BOOL bSelect, int nFirstItem, int nLastItem)
	{ ASSERT(::IsWindow(m_hWnd)); return bSelect ?
		(int)::SendMessage(m_hWnd, LB_SELITEMRANGEEX, nFirstItem, nLastItem) :
		(int)::SendMessage(m_hWnd, LB_SELITEMRANGEEX, nLastItem, nFirstItem); }
_AFXWIN_INLINE void CListBox::SetAnchorIndex(int nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, LB_SETANCHORINDEX, nIndex, 0); }
_AFXWIN_INLINE int CListBox::GetAnchorIndex() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_GETANCHORINDEX, 0, 0); }
_AFXWIN_INLINE LCID CListBox::GetLocale() const
	{ ASSERT(::IsWindow(m_hWnd)); return (LCID)::SendMessage(m_hWnd, LB_GETLOCALE, 0, 0); }
_AFXWIN_INLINE LCID CListBox::SetLocale(LCID nNewLocale)
	{ ASSERT(::IsWindow(m_hWnd)); return (LCID)::SendMessage(m_hWnd, LB_SETLOCALE, (WPARAM)nNewLocale, 0); }
#if (WINVER >= 0x400)
_AFXWIN_INLINE int CListBox::InitStorage(int nItems, UINT nBytes)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, LB_INITSTORAGE, (WPARAM)nItems, nBytes); }
#endif

_AFXWIN_INLINE CCheckListBox::CCheckListBox()
	{ m_cyText = 0; m_nStyle = 0; }
_AFXWIN_INLINE UINT CCheckListBox::GetCheckStyle()
	{ return m_nStyle; }

_AFXWIN_INLINE CComboBox::CComboBox()
	{ }
_AFXWIN_INLINE int CComboBox::GetCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETCOUNT, 0, 0); }
_AFXWIN_INLINE int CComboBox::GetCurSel() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETCURSEL, 0, 0); }
_AFXWIN_INLINE int CComboBox::SetCurSel(int nSelect)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETCURSEL, nSelect, 0); }
_AFXWIN_INLINE DWORD CComboBox::GetEditSel() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, CB_GETEDITSEL, 0, 0); }
_AFXWIN_INLINE BOOL CComboBox::LimitText(int nMaxChars)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, CB_LIMITTEXT, nMaxChars, 0); }
_AFXWIN_INLINE BOOL CComboBox::SetEditSel(int nStartChar, int nEndChar)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, CB_SETEDITSEL, 0, MAKELONG(nStartChar, nEndChar)); }
_AFXWIN_INLINE DWORD CComboBox::GetItemData(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, CB_GETITEMDATA, nIndex, 0); }
_AFXWIN_INLINE int CComboBox::SetItemData(int nIndex, DWORD dwItemData)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETITEMDATA, nIndex, (LPARAM)dwItemData); }
_AFXWIN_INLINE void* CComboBox::GetItemDataPtr(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (LPVOID)GetItemData(nIndex); }
_AFXWIN_INLINE int CComboBox::SetItemDataPtr(int nIndex, void* pData)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItemData(nIndex, (DWORD)(LPVOID)pData); }
_AFXWIN_INLINE int CComboBox::GetLBText(int nIndex, LPTSTR lpszText) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETLBTEXT, nIndex, (LPARAM)lpszText); }
_AFXWIN_INLINE int CComboBox::GetLBTextLen(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETLBTEXTLEN, nIndex, 0); }
_AFXWIN_INLINE void CComboBox::ShowDropDown(BOOL bShowIt)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, CB_SHOWDROPDOWN, bShowIt, 0); }
_AFXWIN_INLINE int CComboBox::AddString(LPCTSTR lpszString)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_ADDSTRING, 0, (LPARAM)lpszString); }
_AFXWIN_INLINE int CComboBox::DeleteString(UINT nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_DELETESTRING, nIndex, 0);}
_AFXWIN_INLINE int CComboBox::InsertString(int nIndex, LPCTSTR lpszString)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_INSERTSTRING, nIndex, (LPARAM)lpszString); }
_AFXWIN_INLINE void CComboBox::ResetContent()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, CB_RESETCONTENT, 0, 0); }
_AFXWIN_INLINE int CComboBox::Dir(UINT attr, LPCTSTR lpszWildCard)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_DIR, attr, (LPARAM)lpszWildCard); }
_AFXWIN_INLINE int CComboBox::FindString(int nStartAfter, LPCTSTR lpszString) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_FINDSTRING, nStartAfter,
		(LPARAM)lpszString); }
_AFXWIN_INLINE int CComboBox::SelectString(int nStartAfter, LPCTSTR lpszString)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SELECTSTRING,
		nStartAfter, (LPARAM)lpszString); }
_AFXWIN_INLINE void CComboBox::Clear()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CLEAR, 0, 0); }
_AFXWIN_INLINE void CComboBox::Copy()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_COPY, 0, 0); }
_AFXWIN_INLINE void CComboBox::Cut()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CUT, 0, 0); }
_AFXWIN_INLINE void CComboBox::Paste()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_PASTE, 0, 0); }
_AFXWIN_INLINE int CComboBox::SetItemHeight(int nIndex, UINT cyItemHeight)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETITEMHEIGHT, nIndex, MAKELONG(cyItemHeight, 0)); }
_AFXWIN_INLINE int CComboBox::GetItemHeight(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETITEMHEIGHT, nIndex, 0L); }
_AFXWIN_INLINE int CComboBox::FindStringExact(int nIndexStart, LPCTSTR lpszFind) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_FINDSTRINGEXACT, nIndexStart, (LPARAM)lpszFind); }
_AFXWIN_INLINE int CComboBox::SetExtendedUI(BOOL bExtended )
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETEXTENDEDUI, bExtended, 0L); }
_AFXWIN_INLINE BOOL CComboBox::GetExtendedUI() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, CB_GETEXTENDEDUI, 0, 0L); }
_AFXWIN_INLINE void CComboBox::GetDroppedControlRect(LPRECT lprect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, CB_GETDROPPEDCONTROLRECT, 0, (DWORD)lprect); }
_AFXWIN_INLINE BOOL CComboBox::GetDroppedState() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, CB_GETDROPPEDSTATE, 0, 0L); }
_AFXWIN_INLINE LCID CComboBox::GetLocale() const
	{ ASSERT(::IsWindow(m_hWnd)); return (LCID)::SendMessage(m_hWnd, CB_GETLOCALE, 0, 0); }
_AFXWIN_INLINE LCID CComboBox::SetLocale(LCID nNewLocale)
	{ ASSERT(::IsWindow(m_hWnd)); return (LCID)::SendMessage(m_hWnd, CB_SETLOCALE, (WPARAM)nNewLocale, 0); }
#if (WINVER >= 0x400)
_AFXWIN_INLINE int CComboBox::GetTopIndex() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETTOPINDEX, 0, 0); }
_AFXWIN_INLINE int CComboBox::SetTopIndex(int nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETTOPINDEX, nIndex, 0); }
_AFXWIN_INLINE int CComboBox::InitStorage(int nItems, UINT nBytes)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_INITSTORAGE, (WPARAM)nItems, nBytes); }
_AFXWIN_INLINE void CComboBox::SetHorizontalExtent(UINT nExtent)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, CB_SETHORIZONTALEXTENT, nExtent, 0); }
_AFXWIN_INLINE UINT CComboBox::GetHorizontalExtent() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, CB_GETHORIZONTALEXTENT, 0, 0); }
_AFXWIN_INLINE int CComboBox::SetDroppedWidth(UINT nWidth)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_SETDROPPEDWIDTH, nWidth, 0); }
_AFXWIN_INLINE int CComboBox::GetDroppedWidth() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, CB_GETDROPPEDWIDTH, 0, 0); }
#endif
_AFXWIN_INLINE CEdit::CEdit()
	{ }
_AFXWIN_INLINE BOOL CEdit::CanUndo() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_CANUNDO, 0, 0); }
_AFXWIN_INLINE int CEdit::GetLineCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_GETLINECOUNT, 0, 0); }
_AFXWIN_INLINE BOOL CEdit::GetModify() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_GETMODIFY, 0, 0); }
_AFXWIN_INLINE void CEdit::SetModify(BOOL bModified)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETMODIFY, bModified, 0); }
_AFXWIN_INLINE void CEdit::GetRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_GETRECT, 0, (LPARAM)lpRect); }
_AFXWIN_INLINE void CEdit::GetSel(int& nStartChar, int& nEndChar) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_GETSEL, (WPARAM)&nStartChar,(LPARAM)&nEndChar); }
_AFXWIN_INLINE DWORD CEdit::GetSel() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, EM_GETSEL, 0, 0); }
_AFXWIN_INLINE HLOCAL CEdit::GetHandle() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HLOCAL)::SendMessage(m_hWnd, EM_GETHANDLE, 0, 0); }
_AFXWIN_INLINE void CEdit::SetHandle(HLOCAL hBuffer)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETHANDLE, (WPARAM)hBuffer, 0); }
_AFXWIN_INLINE int CEdit::GetLine(int nIndex, LPTSTR lpszBuffer) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_GETLINE, nIndex, (LPARAM)lpszBuffer); }
_AFXWIN_INLINE int CEdit::GetLine(int nIndex, LPTSTR lpszBuffer, int nMaxLength) const
	{
		ASSERT(::IsWindow(m_hWnd));
		*(LPWORD)lpszBuffer = (WORD)nMaxLength;
		return (int)::SendMessage(m_hWnd, EM_GETLINE, nIndex, (LPARAM)lpszBuffer);
	}
_AFXWIN_INLINE void CEdit::EmptyUndoBuffer()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_EMPTYUNDOBUFFER, 0, 0); }
_AFXWIN_INLINE BOOL CEdit::FmtLines(BOOL bAddEOL)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_FMTLINES, bAddEOL, 0); }
_AFXWIN_INLINE void CEdit::LimitText(int nChars)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_LIMITTEXT, nChars, 0); }
_AFXWIN_INLINE int CEdit::LineFromChar(int nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_LINEFROMCHAR, nIndex, 0); }
_AFXWIN_INLINE int CEdit::LineIndex(int nLine) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_LINEINDEX, nLine, 0); }
_AFXWIN_INLINE int CEdit::LineLength(int nLine) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_LINELENGTH, nLine, 0); }
_AFXWIN_INLINE void CEdit::LineScroll(int nLines, int nChars)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_LINESCROLL, nChars, nLines); }
_AFXWIN_INLINE void CEdit::ReplaceSel(LPCTSTR lpszNewText, BOOL bCanUndo)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_REPLACESEL, (WPARAM) bCanUndo, (LPARAM)lpszNewText); }
_AFXWIN_INLINE void CEdit::SetPasswordChar(TCHAR ch)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETPASSWORDCHAR, ch, 0); }
_AFXWIN_INLINE void CEdit::SetRect(LPCRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETRECT, 0, (LPARAM)lpRect); }
_AFXWIN_INLINE void CEdit::SetRectNP(LPCRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETRECTNP, 0, (LPARAM)lpRect); }
_AFXWIN_INLINE void CEdit::SetSel(DWORD dwSelection, BOOL bNoScroll)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETSEL,
		LOWORD(dwSelection), HIWORD(dwSelection));
	  if (!bNoScroll)
		::SendMessage(m_hWnd, EM_SCROLLCARET, 0, 0); }
_AFXWIN_INLINE void CEdit::SetSel(int nStartChar, int nEndChar, BOOL bNoScroll)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETSEL, nStartChar, nEndChar);
	  if (!bNoScroll)
		::SendMessage(m_hWnd, EM_SCROLLCARET, 0, 0); }
_AFXWIN_INLINE BOOL CEdit::SetTabStops(int nTabStops, LPINT rgTabStops)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETTABSTOPS, nTabStops,
		(LPARAM)rgTabStops); }
_AFXWIN_INLINE void CEdit::SetTabStops()
	{ ASSERT(::IsWindow(m_hWnd)); VERIFY(::SendMessage(m_hWnd, EM_SETTABSTOPS, 0, 0)); }
_AFXWIN_INLINE BOOL CEdit::SetTabStops(const int& cxEachStop)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETTABSTOPS,
		1, (LPARAM)(LPINT)&cxEachStop); }
_AFXWIN_INLINE BOOL CEdit::Undo()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_UNDO, 0, 0); }
_AFXWIN_INLINE void CEdit::Clear()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CLEAR, 0, 0); }
_AFXWIN_INLINE void CEdit::Copy()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_COPY, 0, 0); }
_AFXWIN_INLINE void CEdit::Cut()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CUT, 0, 0); }
_AFXWIN_INLINE void CEdit::Paste()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_PASTE, 0, 0); }
_AFXWIN_INLINE BOOL CEdit::SetReadOnly(BOOL bReadOnly )
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETREADONLY, bReadOnly, 0L); }
_AFXWIN_INLINE int CEdit::GetFirstVisibleLine() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_GETFIRSTVISIBLELINE, 0, 0L); }
_AFXWIN_INLINE TCHAR CEdit::GetPasswordChar() const
	{ ASSERT(::IsWindow(m_hWnd)); return (TCHAR)::SendMessage(m_hWnd, EM_GETPASSWORDCHAR, 0, 0L); }
#if (WINVER >= 0x400)
_AFXWIN_INLINE void CEdit::SetMargins(UINT nLeft, UINT nRight)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETMARGINS, EC_LEFTMARGIN|EC_RIGHTMARGIN, MAKELONG(nLeft, nRight)); }
_AFXWIN_INLINE DWORD CEdit::GetMargins() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD)::SendMessage(m_hWnd, EM_GETMARGINS, 0, 0); }
_AFXWIN_INLINE void CEdit::SetLimitText(UINT nMax)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETLIMITTEXT, nMax, 0); }
_AFXWIN_INLINE UINT CEdit::GetLimitText() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, EM_GETLIMITTEXT, 0, 0); }
_AFXWIN_INLINE CPoint CEdit::PosFromChar(UINT nChar) const
	{ ASSERT(::IsWindow(m_hWnd)); return CPoint( (DWORD)::SendMessage(m_hWnd, EM_POSFROMCHAR, nChar, 0)); }
_AFXWIN_INLINE int CEdit::CharFromPos(CPoint pt) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_CHARFROMPOS, 0, MAKELPARAM(pt.x, pt.y)); }
#endif

_AFXWIN_INLINE CScrollBar::CScrollBar()
	{ }
_AFXWIN_INLINE int CScrollBar::GetScrollPos() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::GetScrollPos(m_hWnd, SB_CTL); }
_AFXWIN_INLINE int CScrollBar::SetScrollPos(int nPos, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); return ::SetScrollPos(m_hWnd, SB_CTL, nPos, bRedraw); }
_AFXWIN_INLINE void CScrollBar::GetScrollRange(LPINT lpMinPos, LPINT lpMaxPos) const
	{ ASSERT(::IsWindow(m_hWnd)); ::GetScrollRange(m_hWnd, SB_CTL, lpMinPos, lpMaxPos); }
_AFXWIN_INLINE void CScrollBar::SetScrollRange(int nMinPos, int nMaxPos, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SetScrollRange(m_hWnd, SB_CTL, nMinPos, nMaxPos, bRedraw); }
_AFXWIN_INLINE void CScrollBar::ShowScrollBar(BOOL bShow)
	{ ASSERT(::IsWindow(m_hWnd)); ::ShowScrollBar(m_hWnd, SB_CTL, bShow); }
_AFXWIN_INLINE BOOL CScrollBar::EnableScrollBar(UINT nArrowFlags)
	{ ASSERT(::IsWindow(m_hWnd)); return ::EnableScrollBar(m_hWnd, SB_CTL, nArrowFlags); }
_AFXWIN_INLINE BOOL CScrollBar::SetScrollInfo(LPSCROLLINFO lpScrollInfo, BOOL bRedraw)
	{ return CWnd::SetScrollInfo(SB_CTL, lpScrollInfo, bRedraw); }
_AFXWIN_INLINE BOOL CScrollBar::GetScrollInfo(LPSCROLLINFO lpScrollInfo, UINT nMask)
	{ return CWnd::GetScrollInfo(SB_CTL, lpScrollInfo, nMask); }
_AFXWIN_INLINE int CScrollBar::GetScrollLimit()
	{ return CWnd::GetScrollLimit(SB_CTL); }


// MDI functions
_AFXWIN_INLINE void CMDIFrameWnd::MDIActivate(CWnd* pWndActivate)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDIACTIVATE,
		(WPARAM)pWndActivate->m_hWnd, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDIIconArrange()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDIICONARRANGE, 0, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDIMaximize(CWnd* pWnd)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDIMAXIMIZE, (WPARAM)pWnd->m_hWnd, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDINext()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDINEXT, 0, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDIRestore(CWnd* pWnd)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDIRESTORE, (WPARAM)pWnd->m_hWnd, 0); }
_AFXWIN_INLINE CMenu* CMDIFrameWnd::MDISetMenu(CMenu* pFrameMenu, CMenu* pWindowMenu)
	{ ASSERT(::IsWindow(m_hWnd)); return CMenu::FromHandle((HMENU)::SendMessage(
		m_hWndMDIClient, WM_MDISETMENU, (WPARAM)pFrameMenu->GetSafeHmenu(),
		(LPARAM)pWindowMenu->GetSafeHmenu())); }
_AFXWIN_INLINE void CMDIFrameWnd::MDITile()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDITILE, 0, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDICascade()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDICASCADE, 0, 0); }

_AFXWIN_INLINE void CMDIFrameWnd::MDICascade(int nType)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDICASCADE, nType, 0); }
_AFXWIN_INLINE void CMDIFrameWnd::MDITile(int nType)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWndMDIClient, WM_MDITILE, nType, 0); }
_AFXWIN_INLINE void CMDIChildWnd::MDIDestroy()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(GetParent()->m_hWnd, WM_MDIDESTROY, (WPARAM)m_hWnd, 0L); }
_AFXWIN_INLINE void CMDIChildWnd::MDIActivate()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(GetParent()->m_hWnd, WM_MDIACTIVATE, (WPARAM)m_hWnd, 0L); }
_AFXWIN_INLINE void CMDIChildWnd::MDIMaximize()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(GetParent()->m_hWnd, WM_MDIMAXIMIZE, (WPARAM)m_hWnd, 0L); }
_AFXWIN_INLINE void CMDIChildWnd::MDIRestore()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(GetParent()->m_hWnd, WM_MDIRESTORE, (WPARAM)m_hWnd, 0L); }

// CView
_AFXWIN_INLINE CDocument* CView::GetDocument() const
	{ ASSERT(this != NULL); return m_pDocument; }
_AFXWIN_INLINE CSize CScrollView::GetTotalSize() const
	{ ASSERT(this != NULL); return m_totalLog; }

// CDocument
_AFXWIN_INLINE const CString& CDocument::GetTitle() const
	{ ASSERT(this != NULL); return m_strTitle; }
_AFXWIN_INLINE const CString& CDocument::GetPathName() const
	{ ASSERT(this != NULL); return m_strPathName; }
_AFXWIN_INLINE CDocTemplate* CDocument::GetDocTemplate() const
	{ ASSERT(this != NULL); return m_pDocTemplate; }
_AFXWIN_INLINE BOOL CDocument::IsModified()
	{ ASSERT(this != NULL); return m_bModified; }
_AFXWIN_INLINE void CDocument::SetModifiedFlag(BOOL bModified)
	{ ASSERT(this != NULL); m_bModified = bModified; }

// CWinThread
_AFXWIN_INLINE CWinThread::operator HANDLE() const
	{ return this == NULL ? NULL : m_hThread; }
_AFXWIN_INLINE BOOL CWinThread::SetThreadPriority(int nPriority)
	{ ASSERT(m_hThread != NULL); return ::SetThreadPriority(m_hThread, nPriority); }
_AFXWIN_INLINE int CWinThread::GetThreadPriority()
	{ ASSERT(m_hThread != NULL); return ::GetThreadPriority(m_hThread); }
_AFXWIN_INLINE DWORD CWinThread::ResumeThread()
	{ ASSERT(m_hThread != NULL); return ::ResumeThread(m_hThread); }
_AFXWIN_INLINE DWORD CWinThread::SuspendThread()
	{ ASSERT(m_hThread != NULL); return ::SuspendThread(m_hThread); }
_AFXWIN_INLINE BOOL CWinThread::PostThreadMessage(UINT message, WPARAM wParam, LPARAM lParam)
	{ ASSERT(m_hThread != NULL); return ::PostThreadMessage(m_nThreadID, message, wParam, lParam); }

// CWinApp
_AFXWIN_INLINE HCURSOR CWinApp::LoadCursor(LPCTSTR lpszResourceName) const
	{ return ::LoadCursor(AfxFindResourceHandle(lpszResourceName,
		RT_GROUP_CURSOR), lpszResourceName); }
_AFXWIN_INLINE HCURSOR CWinApp::LoadCursor(UINT nIDResource) const
	{ return ::LoadCursor(AfxFindResourceHandle(MAKEINTRESOURCE(nIDResource),
		RT_GROUP_CURSOR), MAKEINTRESOURCE(nIDResource)); }
_AFXWIN_INLINE HCURSOR CWinApp::LoadStandardCursor(LPCTSTR lpszCursorName) const
	{ return ::LoadCursor(NULL, lpszCursorName); }
_AFXWIN_INLINE HCURSOR CWinApp::LoadOEMCursor(UINT nIDCursor) const
	{ return ::LoadCursor(NULL, MAKEINTRESOURCE(nIDCursor)); }
_AFXWIN_INLINE HICON CWinApp::LoadIcon(LPCTSTR lpszResourceName) const
	{ return ::LoadIcon(AfxFindResourceHandle(lpszResourceName,
		RT_GROUP_ICON), lpszResourceName); }
_AFXWIN_INLINE HICON CWinApp::LoadIcon(UINT nIDResource) const
	{ return ::LoadIcon(AfxFindResourceHandle(MAKEINTRESOURCE(nIDResource),
		RT_GROUP_ICON), MAKEINTRESOURCE(nIDResource)); }
_AFXWIN_INLINE HICON CWinApp::LoadStandardIcon(LPCTSTR lpszIconName) const
	{ return ::LoadIcon(NULL, lpszIconName); }
_AFXWIN_INLINE HICON CWinApp::LoadOEMIcon(UINT nIDIcon) const
	{ return ::LoadIcon(NULL, MAKEINTRESOURCE(nIDIcon)); }

_AFXWIN_INLINE CWaitCursor::CWaitCursor()
	{ AfxGetApp()->BeginWaitCursor(); }
_AFXWIN_INLINE CWaitCursor::~CWaitCursor()
	{ AfxGetApp()->EndWaitCursor(); }
_AFXWIN_INLINE void CWaitCursor::Restore()
	{ AfxGetApp()->RestoreWaitCursor(); }

/////////////////////////////////////////////////////////////////////////////
// Obsolete and non-portable

_AFXWIN_INLINE void CWnd::CloseWindow()
	{ ASSERT(::IsWindow(m_hWnd)); ::CloseWindow(m_hWnd); }
_AFXWIN_INLINE BOOL CWnd::OpenIcon()
	{ ASSERT(::IsWindow(m_hWnd)); return ::OpenIcon(m_hWnd); }

/////////////////////////////////////////////////////////////////////////////

#endif //_AFXWIN_INLINE
