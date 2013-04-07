// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXCMN.H

#ifdef _AFXCMN_INLINE

_AFXCMN_INLINE CDragListBox::CDragListBox()
	{ m_nLast = -1;}
_AFXCMN_INLINE int CDragListBox::ItemFromPt(CPoint pt, BOOL bAutoScroll) const
	{ASSERT(::IsWindow(m_hWnd)); return ::LBItemFromPt(m_hWnd, pt, bAutoScroll); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CToolBarCtrl::CToolBarCtrl()
	{ }
_AFXCMN_INLINE BOOL CToolBarCtrl::EnableButton(int nID, BOOL bEnable)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ENABLEBUTTON, nID, MAKELPARAM(bEnable, 0)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::CheckButton(int nID, BOOL bCheck)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_CHECKBUTTON, nID, MAKELPARAM(bCheck, 0)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::PressButton(int nID, BOOL bPress)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_PRESSBUTTON, nID, MAKELPARAM(bPress, 0)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::HideButton(int nID, BOOL bHide)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_HIDEBUTTON, nID, MAKELPARAM(bHide, 0)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::Indeterminate(int nID, BOOL bIndeterminate)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_INDETERMINATE, nID, MAKELPARAM(bIndeterminate, 0)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::IsButtonEnabled(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ISBUTTONENABLED, nID, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::IsButtonChecked(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ISBUTTONCHECKED, nID, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::IsButtonPressed(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ISBUTTONPRESSED, nID, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::IsButtonHidden(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ISBUTTONHIDDEN, nID, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::IsButtonIndeterminate(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ISBUTTONINDETERMINATE, nID, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::SetState(int nID, UINT nState)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_SETSTATE, nID, MAKELPARAM(nState, 0)); }
_AFXCMN_INLINE int CToolBarCtrl::GetState(int nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TB_GETSTATE, nID, 0L); }
_AFXCMN_INLINE BOOL CToolBarCtrl::AddButtons(int nNumButtons, LPTBBUTTON lpButtons)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_ADDBUTTONS, nNumButtons, (LPARAM)lpButtons); }
_AFXCMN_INLINE BOOL CToolBarCtrl::InsertButton(int nIndex, LPTBBUTTON lpButton)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_INSERTBUTTON, nIndex, (LPARAM)lpButton); }
_AFXCMN_INLINE BOOL CToolBarCtrl::DeleteButton(int nIndex)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_DELETEBUTTON, nIndex, 0); }
_AFXCMN_INLINE BOOL CToolBarCtrl::GetButton(int nIndex, LPTBBUTTON lpButton) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_GETBUTTON, nIndex, (LPARAM)lpButton); }
_AFXCMN_INLINE int CToolBarCtrl::GetButtonCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TB_BUTTONCOUNT, 0, 0L); }
_AFXCMN_INLINE UINT CToolBarCtrl::CommandToIndex(UINT nID) const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, TB_COMMANDTOINDEX, nID, 0L); }
_AFXCMN_INLINE void CToolBarCtrl::Customize()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_CUSTOMIZE, 0, 0L); }
// lpszStrings are separated by zeroes, last one is marked by two zeroes
_AFXCMN_INLINE int CToolBarCtrl::AddStrings(LPCTSTR lpszStrings)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TB_ADDSTRING, 0, (LPARAM)lpszStrings); }
_AFXCMN_INLINE BOOL CToolBarCtrl::GetItemRect(int nIndex, LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_GETITEMRECT, nIndex, (LPARAM)lpRect); }
_AFXCMN_INLINE void CToolBarCtrl::SetButtonStructSize(int nSize)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_BUTTONSTRUCTSIZE, nSize, 0L); }
_AFXCMN_INLINE BOOL CToolBarCtrl::SetButtonSize(CSize size)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_SETBUTTONSIZE, 0, MAKELPARAM(size.cx, size.cy)); }
_AFXCMN_INLINE BOOL CToolBarCtrl::SetBitmapSize(CSize size)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_SETBITMAPSIZE, 0, MAKELPARAM(size.cx, size.cy)); }
_AFXCMN_INLINE void CToolBarCtrl::AutoSize()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_AUTOSIZE, 0, 0L); }
_AFXCMN_INLINE CToolTipCtrl* CToolBarCtrl::GetToolTips() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CToolTipCtrl*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TB_GETTOOLTIPS, 0, 0L)); }
_AFXCMN_INLINE void CToolBarCtrl::SetToolTips(CToolTipCtrl* pTip)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_SETTOOLTIPS, (WPARAM)pTip->GetSafeHwnd(), 0L); }
_AFXCMN_INLINE void CToolBarCtrl::SetOwner(CWnd* pWnd)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_SETPARENT, (WPARAM)pWnd->GetSafeHwnd(), 0L); }
_AFXCMN_INLINE void CToolBarCtrl::SetRows(int nRows, BOOL bLarger, LPRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TB_SETROWS, MAKELPARAM(nRows, bLarger), (LPARAM)lpRect); }
_AFXCMN_INLINE int CToolBarCtrl::GetRows() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TB_GETROWS, 0, 0L); }
_AFXCMN_INLINE BOOL CToolBarCtrl::SetCmdID(int nIndex, UINT nID)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TB_SETCMDID, nIndex, nID); }
_AFXCMN_INLINE UINT CToolBarCtrl::GetBitmapFlags() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, TB_GETBITMAPFLAGS, 0, 0L); }
_AFXCMN_INLINE int CToolBarCtrl::HitTest(LPPOINT ppt) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TB_HITTEST, 0, (LPARAM)ppt); }
_AFXCMN_INLINE DWORD CToolBarCtrl::GetExtendedStyle() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, TB_GETEXTENDEDSTYLE, 0, 0L); }
_AFXCMN_INLINE DWORD CToolBarCtrl::SetExtendedStyle(DWORD dwExStyle)
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, TB_SETEXTENDEDSTYLE, 0, dwExStyle); }
_AFXCMN_INLINE COLORREF CToolBarCtrl::GetInsertMarkColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TB_GETINSERTMARKCOLOR, 0, 0); }
_AFXCMN_INLINE COLORREF CToolBarCtrl::SetInsertMarkColor(COLORREF clrNew)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TB_SETINSERTMARKCOLOR, 0, (LPARAM) clrNew); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CStatusBarCtrl::CStatusBarCtrl()
	{ }
_AFXCMN_INLINE BOOL CStatusBarCtrl::SetText(LPCTSTR lpszText, int nPane, int nType)
	{ ASSERT(::IsWindow(m_hWnd)); ASSERT(nPane < 256); return (BOOL) ::SendMessage(m_hWnd, SB_SETTEXT, (nPane|nType), (LPARAM)lpszText); }
_AFXCMN_INLINE BOOL CStatusBarCtrl::SetParts(int nParts, int* pWidths)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, SB_SETPARTS, nParts, (LPARAM)pWidths); }
_AFXCMN_INLINE int CStatusBarCtrl::GetParts(int nParts, int* pParts) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, SB_GETPARTS, nParts, (LPARAM)pParts); }
_AFXCMN_INLINE BOOL CStatusBarCtrl::GetBorders(int* pBorders) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, SB_GETBORDERS, 0, (LPARAM)pBorders); }
_AFXCMN_INLINE void CStatusBarCtrl::SetMinHeight(int nMin)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, SB_SETMINHEIGHT, nMin, 0L); }
_AFXCMN_INLINE BOOL CStatusBarCtrl::SetSimple(BOOL bSimple)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, SB_SIMPLE, bSimple, 0L); }
_AFXCMN_INLINE BOOL CStatusBarCtrl::GetRect(int nPane, LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, SB_GETRECT, nPane, (LPARAM)lpRect); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CListCtrl::CListCtrl()
	{ }
_AFXCMN_INLINE COLORREF CListCtrl::GetBkColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, LVM_GETBKCOLOR, 0, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::SetBkColor(COLORREF cr)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETBKCOLOR, 0, cr); }
_AFXCMN_INLINE CImageList* CListCtrl::GetImageList(int nImageListType) const
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST) ::SendMessage(m_hWnd, LVM_GETIMAGELIST, nImageListType, 0L)); }
_AFXCMN_INLINE CImageList* CListCtrl::SetImageList(CImageList* pImageList, int nImageList)
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST) ::SendMessage(m_hWnd, LVM_SETIMAGELIST, nImageList, (LPARAM)pImageList->GetSafeHandle())); }
_AFXCMN_INLINE int CListCtrl::GetItemCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETITEMCOUNT, 0, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::GetItem(LVITEM* pItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_GETITEM, 0, (LPARAM)pItem); }
_AFXCMN_INLINE BOOL CListCtrl::SetItem(const LVITEM* pItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETITEM, 0, (LPARAM)pItem); }
_AFXCMN_INLINE BOOL CListCtrl::SetItemData(int nItem, DWORD dwData)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItem(nItem, 0, LVIF_PARAM, NULL, 0, 0, 0, (LPARAM)dwData); }
_AFXCMN_INLINE int CListCtrl::InsertItem(const LVITEM* pItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_INSERTITEM, 0, (LPARAM)pItem); }
_AFXCMN_INLINE int CListCtrl::InsertItem(int nItem, LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return InsertItem(LVIF_TEXT, nItem, lpszItem, 0, 0, 0, 0); }
_AFXCMN_INLINE int CListCtrl::InsertItem(int nItem, LPCTSTR lpszItem, int nImage)
	{ ASSERT(::IsWindow(m_hWnd)); return InsertItem(LVIF_TEXT|LVIF_IMAGE, nItem, lpszItem, 0, 0, nImage, 0); }
_AFXCMN_INLINE BOOL CListCtrl::DeleteItem(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_DELETEITEM, nItem, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::DeleteAllItems()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_DELETEALLITEMS, 0, 0L); }
_AFXCMN_INLINE UINT CListCtrl::GetCallbackMask() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, LVM_GETCALLBACKMASK, 0, 0); }
_AFXCMN_INLINE BOOL CListCtrl::SetCallbackMask(UINT nMask)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETCALLBACKMASK, nMask, 0); }
_AFXCMN_INLINE int CListCtrl::GetNextItem(int nItem, int nFlags) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETNEXTITEM, nItem, MAKELPARAM(nFlags, 0)); }
_AFXCMN_INLINE POSITION CListCtrl::GetFirstSelectedItemPosition() const
	{ ASSERT(::IsWindow(m_hWnd)); return (POSITION) (1+GetNextItem(-1, LVIS_SELECTED)); }
_AFXCMN_INLINE int CListCtrl::GetNextSelectedItem(POSITION& pos) const
	{ ASSERT(::IsWindow(m_hWnd)); int nOldPos = (int)pos-1; pos = (POSITION) (1+GetNextItem(nOldPos, LVIS_SELECTED)); return nOldPos; }
_AFXCMN_INLINE int CListCtrl::FindItem(LVFINDINFO* pFindInfo, int nStart) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_FINDITEM, nStart, (LPARAM)pFindInfo); }
_AFXCMN_INLINE int CListCtrl::HitTest(LVHITTESTINFO* pHitTestInfo) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_HITTEST, 0, (LPARAM)pHitTestInfo); }
_AFXCMN_INLINE BOOL CListCtrl::SetItemPosition(int nItem, POINT pt)
	{ ASSERT(::IsWindow(m_hWnd)); ASSERT((GetStyle() & LVS_OWNERDATA)==0); return (BOOL) ::SendMessage(m_hWnd, LVM_SETITEMPOSITION32, nItem, (LPARAM)&pt); }
_AFXCMN_INLINE BOOL CListCtrl::GetItemPosition(int nItem, LPPOINT lpPoint) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_GETITEMPOSITION, nItem, (LPARAM)lpPoint); }
_AFXCMN_INLINE int CListCtrl::GetStringWidth(LPCTSTR lpsz) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETSTRINGWIDTH, 0, (LPARAM)lpsz); }
_AFXCMN_INLINE BOOL CListCtrl::EnsureVisible(int nItem, BOOL bPartialOK)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_ENSUREVISIBLE, nItem, MAKELPARAM(bPartialOK, 0)); }
_AFXCMN_INLINE BOOL CListCtrl::Scroll(CSize size)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SCROLL, size.cx, size.cy); }
_AFXCMN_INLINE BOOL CListCtrl::RedrawItems(int nFirst, int nLast)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_REDRAWITEMS, nFirst, nLast); }
_AFXCMN_INLINE BOOL CListCtrl::Arrange(UINT nCode)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_ARRANGE, nCode, 0L); }
_AFXCMN_INLINE CEdit* CListCtrl::EditLabel(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (CEdit*)CWnd::FromHandle( (HWND)::SendMessage(m_hWnd, LVM_EDITLABEL, nItem, 0L)); }
_AFXCMN_INLINE CEdit* CListCtrl::GetEditControl() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CEdit*)CWnd::FromHandle( (HWND)::SendMessage(m_hWnd, LVM_GETEDITCONTROL, 0, 0L)); }
_AFXCMN_INLINE BOOL CListCtrl::GetColumn(int nCol, LVCOLUMN* pColumn) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_GETCOLUMN, nCol, (LPARAM)pColumn); }
_AFXCMN_INLINE BOOL CListCtrl::SetColumn(int nCol, const LVCOLUMN* pColumn)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETCOLUMN, nCol, (LPARAM)pColumn); }
_AFXCMN_INLINE int CListCtrl::InsertColumn(int nCol, const LVCOLUMN* pColumn)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_INSERTCOLUMN, nCol, (LPARAM)pColumn); }
_AFXCMN_INLINE BOOL CListCtrl::DeleteColumn(int nCol)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_DELETECOLUMN, nCol, 0); }
_AFXCMN_INLINE int CListCtrl::GetColumnWidth(int nCol) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETCOLUMNWIDTH, nCol, 0); }
_AFXCMN_INLINE BOOL CListCtrl::SetColumnWidth(int nCol, int cx)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETCOLUMNWIDTH, nCol, MAKELPARAM(cx, 0)); }
_AFXCMN_INLINE BOOL CListCtrl::GetViewRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_GETVIEWRECT, 0, (LPARAM)lpRect); }
_AFXCMN_INLINE COLORREF CListCtrl::GetTextColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, LVM_GETTEXTCOLOR, 0, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::SetTextColor(COLORREF cr)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETTEXTCOLOR, 0, cr); }
_AFXCMN_INLINE COLORREF CListCtrl::GetTextBkColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, LVM_GETTEXTBKCOLOR, 0, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::SetTextBkColor(COLORREF cr)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETTEXTBKCOLOR, 0, cr); }
_AFXCMN_INLINE int CListCtrl::GetTopIndex() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETTOPINDEX, 0, 0); }
_AFXCMN_INLINE int CListCtrl::GetCountPerPage() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, LVM_GETCOUNTPERPAGE, 0, 0); }
_AFXCMN_INLINE BOOL CListCtrl::GetOrigin(LPPOINT lpPoint) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_GETORIGIN, 0, (LPARAM)lpPoint); }
_AFXCMN_INLINE BOOL CListCtrl::Update(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_UPDATE, nItem, 0L); }
_AFXCMN_INLINE BOOL CListCtrl::SetItemState(int nItem, LVITEM* pItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, LVM_SETITEMSTATE, nItem, (LPARAM)pItem); }
_AFXCMN_INLINE UINT CListCtrl::GetItemState(int nItem, UINT nMask) const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, LVM_GETITEMSTATE, nItem, nMask); }
_AFXCMN_INLINE void CListCtrl::SetItemCount(int nItems)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, LVM_SETITEMCOUNT, nItems, 0); }
_AFXCMN_INLINE BOOL CListCtrl::SortItems(PFNLVCOMPARE pfnCompare, DWORD dwData)
	{ ASSERT(::IsWindow(m_hWnd)); ASSERT((GetStyle() & LVS_OWNERDATA)==0); return (BOOL) ::SendMessage(m_hWnd, LVM_SORTITEMS, dwData, (LPARAM)pfnCompare); }
_AFXCMN_INLINE UINT CListCtrl::GetSelectedCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, LVM_GETSELECTEDCOUNT, 0, 0L); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CTreeCtrl::CTreeCtrl()
	{ }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::InsertItem(LPTVINSERTSTRUCT lpInsertStruct)
	{ ASSERT(::IsWindow(m_hWnd));  return (HTREEITEM)::SendMessage(m_hWnd, TVM_INSERTITEM, 0, (LPARAM)lpInsertStruct); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::InsertItem(LPCTSTR lpszItem, int nImage,
	int nSelectedImage, HTREEITEM hParent, HTREEITEM hInsertAfter)
	{ ASSERT(::IsWindow(m_hWnd)); return InsertItem(TVIF_TEXT | TVIF_IMAGE | TVIF_SELECTEDIMAGE, lpszItem, nImage, nSelectedImage, 0, 0, 0, hParent, hInsertAfter); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::InsertItem(LPCTSTR lpszItem, HTREEITEM hParent, HTREEITEM hInsertAfter)
	{ ASSERT(::IsWindow(m_hWnd)); return InsertItem(TVIF_TEXT, lpszItem, 0, 0, 0, 0, 0, hParent, hInsertAfter); }
_AFXCMN_INLINE BOOL CTreeCtrl::DeleteItem(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_DELETEITEM, 0, (LPARAM)hItem); }
#pragma warning(disable: 4310)
_AFXCMN_INLINE BOOL CTreeCtrl::DeleteAllItems()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_DELETEITEM, 0, (LPARAM)TVI_ROOT); }
#pragma warning(default: 4310)
_AFXCMN_INLINE BOOL CTreeCtrl::Expand(HTREEITEM hItem, UINT nCode)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_EXPAND, nCode, (LPARAM)hItem); }
_AFXCMN_INLINE UINT CTreeCtrl::GetCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, TVM_GETCOUNT, 0, 0); }
_AFXCMN_INLINE UINT CTreeCtrl::GetIndent() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, TVM_GETINDENT, 0, 0); }
_AFXCMN_INLINE void CTreeCtrl::SetIndent(UINT nIndent)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TVM_SETINDENT, nIndent, 0); }
_AFXCMN_INLINE CImageList* CTreeCtrl::GetImageList(UINT nImageList) const
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST)::SendMessage(m_hWnd, TVM_GETIMAGELIST, (UINT)nImageList, 0)); }
_AFXCMN_INLINE CImageList* CTreeCtrl::SetImageList(CImageList* pImageList, int nImageListType)
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST)::SendMessage(m_hWnd, TVM_SETIMAGELIST, (UINT)nImageListType, (LPARAM)pImageList->GetSafeHandle())); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetNextItem(HTREEITEM hItem, UINT nCode) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, nCode, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetChildItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_CHILD, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetNextSiblingItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_NEXT, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetPrevSiblingItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_PREVIOUS, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetParentItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_PARENT, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetFirstVisibleItem() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_FIRSTVISIBLE, 0); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetNextVisibleItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_NEXTVISIBLE, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetPrevVisibleItem(HTREEITEM hItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_PREVIOUSVISIBLE, (LPARAM)hItem); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetSelectedItem() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_CARET, 0); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetDropHilightItem() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_DROPHILITE, 0); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::GetRootItem() const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_GETNEXTITEM, TVGN_ROOT, 0); }
_AFXCMN_INLINE BOOL CTreeCtrl::Select(HTREEITEM hItem, UINT nCode)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_SELECTITEM, nCode, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SelectItem(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TVM_SELECTITEM, TVGN_CARET, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SelectDropTarget(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TVM_SELECTITEM, TVGN_DROPHILITE, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SelectSetFirstVisible(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TVM_SELECTITEM, TVGN_FIRSTVISIBLE, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::GetItem(TVITEM* pItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_GETITEM, 0, (LPARAM)pItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetItem(TVITEM* pItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_SETITEM, 0, (LPARAM)pItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetItemText(HTREEITEM hItem, LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItem(hItem, TVIF_TEXT, lpszItem, 0, 0, 0, 0, NULL); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetItemImage(HTREEITEM hItem, int nImage, int nSelectedImage)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItem(hItem, TVIF_IMAGE|TVIF_SELECTEDIMAGE, NULL, nImage, nSelectedImage, 0, 0, NULL); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetItemState(HTREEITEM hItem, UINT nState, UINT nStateMask)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItem(hItem, TVIF_STATE, NULL, 0, 0, nState, nStateMask, NULL); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetItemData(HTREEITEM hItem, DWORD dwData)
	{ ASSERT(::IsWindow(m_hWnd)); return SetItem(hItem, TVIF_PARAM, NULL, 0, 0, 0, 0, (LPARAM)dwData); }
_AFXCMN_INLINE CEdit* CTreeCtrl::EditLabel(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (CEdit*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TVM_EDITLABEL, 0, (LPARAM)hItem)); }
_AFXCMN_INLINE HTREEITEM CTreeCtrl::HitTest(TVHITTESTINFO* pHitTestInfo) const
	{ ASSERT(::IsWindow(m_hWnd)); return (HTREEITEM)::SendMessage(m_hWnd, TVM_HITTEST, 0, (LPARAM)pHitTestInfo); }
_AFXCMN_INLINE CEdit* CTreeCtrl::GetEditControl() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CEdit*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TVM_GETEDITCONTROL, 0, 0)); }
_AFXCMN_INLINE UINT CTreeCtrl::GetVisibleCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT)::SendMessage(m_hWnd, TVM_GETVISIBLECOUNT, 0, 0); }
_AFXCMN_INLINE BOOL CTreeCtrl::SortChildren(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_SORTCHILDREN, 0, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::EnsureVisible(HTREEITEM hItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_ENSUREVISIBLE, 0, (LPARAM)hItem); }
_AFXCMN_INLINE BOOL CTreeCtrl::SortChildrenCB(LPTVSORTCB pSort)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TVM_SORTCHILDRENCB, 0, (LPARAM)pSort); }
_AFXCMN_INLINE CToolTipCtrl* CTreeCtrl::GetToolTips() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CToolTipCtrl*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TVM_GETTOOLTIPS, 0, 0L)); }
_AFXCMN_INLINE CToolTipCtrl* CTreeCtrl::SetToolTips(CToolTipCtrl* pWndTip)
	{ ASSERT(::IsWindow(m_hWnd)); return (CToolTipCtrl*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TVM_SETTOOLTIPS, (WPARAM)pWndTip->GetSafeHwnd(), 0L)); }
_AFXCMN_INLINE COLORREF CTreeCtrl::GetBkColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_GETBKCOLOR, 0, 0L); }
_AFXCMN_INLINE COLORREF CTreeCtrl::SetBkColor(COLORREF clr)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_SETBKCOLOR, 0, (LPARAM)clr); }
_AFXCMN_INLINE SHORT CTreeCtrl::GetItemHeight() const
	{ ASSERT(::IsWindow(m_hWnd)); return (SHORT) ::SendMessage(m_hWnd, TVM_GETITEMHEIGHT, 0, 0L); }
_AFXCMN_INLINE SHORT CTreeCtrl::SetItemHeight(SHORT cyHeight)
	{ ASSERT(::IsWindow(m_hWnd)); return (SHORT) ::SendMessage(m_hWnd, TVM_SETITEMHEIGHT, cyHeight, 0L); }
_AFXCMN_INLINE COLORREF CTreeCtrl::GetTextColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_GETTEXTCOLOR, 0, 0L); }
_AFXCMN_INLINE COLORREF CTreeCtrl::SetTextColor(COLORREF clr)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_SETTEXTCOLOR, 0, (LPARAM)clr); }
_AFXCMN_INLINE BOOL CTreeCtrl::SetInsertMark(HTREEITEM hItem, BOOL fAfter)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) ::SendMessage(m_hWnd, TVM_SETINSERTMARK, fAfter, (LPARAM)hItem); }
_AFXCMN_INLINE COLORREF CTreeCtrl::GetInsertMarkColor() const
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_GETINSERTMARKCOLOR, 0, 0L); }
_AFXCMN_INLINE COLORREF CTreeCtrl::SetInsertMarkColor(COLORREF clrNew)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF) ::SendMessage(m_hWnd, TVM_SETINSERTMARKCOLOR, 0, (LPARAM)clrNew); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CHotKeyCtrl::CHotKeyCtrl()
	{ }
_AFXCMN_INLINE void CHotKeyCtrl::SetHotKey(WORD wVirtualKeyCode, WORD wModifiers)
	{ ASSERT(::IsWindow(m_hWnd));  ::SendMessage(m_hWnd, HKM_SETHOTKEY, MAKEWORD(wVirtualKeyCode, wModifiers), 0L); }
_AFXCMN_INLINE DWORD CHotKeyCtrl::GetHotKey() const
	{ ASSERT(::IsWindow(m_hWnd)); return ::SendMessage(m_hWnd, HKM_GETHOTKEY, 0, 0L); }
_AFXCMN_INLINE void CHotKeyCtrl::SetRules(WORD wInvalidComb, WORD wModifiers)
	{ ASSERT(::IsWindow(m_hWnd));  ::SendMessage(m_hWnd, HKM_SETRULES, wInvalidComb, MAKELPARAM(wModifiers, 0)); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE void CToolTipCtrl::Activate(BOOL bActivate)
	{ ASSERT(::IsWindow(m_hWnd));  ::SendMessage(m_hWnd, TTM_ACTIVATE, bActivate, 0L); }
_AFXCMN_INLINE void CToolTipCtrl::SetToolInfo(LPTOOLINFO lpToolInfo)
	{ ASSERT(::IsWindow(m_hWnd));  ::SendMessage(m_hWnd, TTM_SETTOOLINFO, 0, (LPARAM)lpToolInfo); }
_AFXCMN_INLINE void CToolTipCtrl::RelayEvent(LPMSG lpMsg)
	{ ASSERT(::IsWindow(m_hWnd));  ::SendMessage(m_hWnd, TTM_RELAYEVENT, 0, (LPARAM)lpMsg); }
_AFXCMN_INLINE int CToolTipCtrl::GetToolCount() const
	{ ASSERT(::IsWindow(m_hWnd));  return (int) ::SendMessage(m_hWnd, TTM_GETTOOLCOUNT, 0, 0L); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CSpinButtonCtrl::CSpinButtonCtrl()
	{ }
_AFXCMN_INLINE UINT CSpinButtonCtrl::GetAccel(int nAccel, UDACCEL* pAccel) const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) LOWORD(::SendMessage(m_hWnd, UDM_GETACCEL, nAccel, (LPARAM)pAccel)); }
_AFXCMN_INLINE UINT CSpinButtonCtrl::GetBase() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) LOWORD(::SendMessage(m_hWnd, UDM_GETBASE, 0, 0l)); }
_AFXCMN_INLINE CWnd* CSpinButtonCtrl::GetBuddy() const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle((HWND) ::SendMessage(m_hWnd, UDM_GETBUDDY, 0, 0l)); }
_AFXCMN_INLINE int CSpinButtonCtrl::GetPos() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) (::SendMessage(m_hWnd, UDM_GETPOS, 0, 0l)); }
_AFXCMN_INLINE DWORD CSpinButtonCtrl::GetRange() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, UDM_GETRANGE, 0, 0l); }
_AFXCMN_INLINE BOOL CSpinButtonCtrl::SetAccel(int nAccel, UDACCEL* pAccel)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL) LOWORD(::SendMessage(m_hWnd, UDM_SETACCEL, nAccel, (LPARAM)pAccel)); }
_AFXCMN_INLINE int CSpinButtonCtrl::SetBase(int nBase)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, UDM_SETBASE, nBase, 0L); }
_AFXCMN_INLINE CWnd* CSpinButtonCtrl::SetBuddy(CWnd* pWndBuddy)
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle((HWND) ::SendMessage(m_hWnd, UDM_SETBUDDY, (WPARAM)pWndBuddy->GetSafeHwnd(), 0L)); }
_AFXCMN_INLINE int CSpinButtonCtrl::SetPos(int nPos)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) (short) LOWORD(::SendMessage(m_hWnd, UDM_SETPOS, 0, MAKELPARAM(nPos, 0))); }
_AFXCMN_INLINE void CSpinButtonCtrl::SetRange(int nLower, int nUpper)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, UDM_SETRANGE, 0, MAKELPARAM(nUpper, nLower)); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CSliderCtrl::CSliderCtrl()
	{ }
_AFXCMN_INLINE int CSliderCtrl::GetLineSize() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETLINESIZE, 0, 0l); }
_AFXCMN_INLINE int CSliderCtrl::SetLineSize(int nSize)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_SETLINESIZE, 0, nSize); }
_AFXCMN_INLINE int CSliderCtrl::GetPageSize() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETPAGESIZE, 0, 0l); }
_AFXCMN_INLINE int CSliderCtrl::SetPageSize(int nSize)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_SETPAGESIZE, 0, nSize); }
_AFXCMN_INLINE int CSliderCtrl::GetRangeMax() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETRANGEMAX, 0, 0l); }
_AFXCMN_INLINE int CSliderCtrl::GetRangeMin() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETRANGEMIN, 0, 0l); }
_AFXCMN_INLINE void CSliderCtrl::SetRangeMin(int nMin, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETRANGEMIN, bRedraw, nMin); }
_AFXCMN_INLINE void CSliderCtrl::SetRangeMax(int nMax, BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETRANGEMAX, bRedraw, nMax); }
_AFXCMN_INLINE void CSliderCtrl::ClearSel(BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_CLEARSEL, bRedraw, 0l); }
_AFXCMN_INLINE void CSliderCtrl::GetChannelRect(LPRECT lprc) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_GETCHANNELRECT, 0, (LPARAM)lprc); }
_AFXCMN_INLINE void CSliderCtrl::GetThumbRect(LPRECT lprc) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_GETTHUMBRECT, 0, (LPARAM)lprc); }
_AFXCMN_INLINE int CSliderCtrl::GetPos() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETPOS, 0, 0l); }
_AFXCMN_INLINE void CSliderCtrl::SetPos(int nPos)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETPOS, TRUE, nPos); }
_AFXCMN_INLINE void CSliderCtrl::VerifyPos()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETPOS, FALSE, 0L); }
_AFXCMN_INLINE void CSliderCtrl::ClearTics(BOOL bRedraw)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_CLEARTICS, bRedraw, 0l); }
_AFXCMN_INLINE UINT CSliderCtrl::GetNumTics() const
	{ ASSERT(::IsWindow(m_hWnd)); return (UINT) ::SendMessage(m_hWnd, TBM_GETNUMTICS, 0, 0l); }
_AFXCMN_INLINE DWORD* CSliderCtrl::GetTicArray() const
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD*) ::SendMessage(m_hWnd, TBM_GETPTICS, 0, 0l); }
_AFXCMN_INLINE int CSliderCtrl::GetTic(int nTic) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETTIC, nTic, 0L); }
_AFXCMN_INLINE int CSliderCtrl::GetTicPos(int nTic) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, TBM_GETTICPOS, nTic, 0L); }
_AFXCMN_INLINE BOOL CSliderCtrl::SetTic(int nTic)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TBM_SETTIC, 0, nTic); }
_AFXCMN_INLINE void CSliderCtrl::SetTicFreq(int nFreq)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETTICFREQ, nFreq, 0L); }
_AFXCMN_INLINE CWnd* CSliderCtrl::GetBuddy(BOOL fLocation) const
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle((HWND) ::SendMessage(m_hWnd, TBM_GETBUDDY, fLocation, 0l)); }
_AFXCMN_INLINE CWnd* CSliderCtrl::SetBuddy(CWnd* pWndBuddy, BOOL fLocation)
	{ ASSERT(::IsWindow(m_hWnd)); return CWnd::FromHandle((HWND) ::SendMessage(m_hWnd, TBM_SETBUDDY, fLocation, (LPARAM)pWndBuddy->GetSafeHwnd())); }
_AFXCMN_INLINE CToolTipCtrl* CSliderCtrl::GetToolTips() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CToolTipCtrl*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TBM_GETTOOLTIPS, 0, 0L)); }
_AFXCMN_INLINE void CSliderCtrl::SetToolTips(CToolTipCtrl* pTip)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TBM_SETTOOLTIPS, (WPARAM)pTip->GetSafeHwnd(), 0L); }
_AFXCMN_INLINE int CSliderCtrl::SetTipSide(int nLocation)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TBM_SETTIPSIDE, nLocation, 0L); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CProgressCtrl::CProgressCtrl()
	{ }
_AFXCMN_INLINE void CProgressCtrl::SetRange(short nLower, short nUpper)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, PBM_SETRANGE, 0, MAKELPARAM(nLower, nUpper)); }
_AFXCMN_INLINE void CProgressCtrl::SetRange32(int nLower, int nUpper)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, PBM_SETRANGE32, (WPARAM) nLower, (LPARAM) nUpper); }
_AFXCMN_INLINE int CProgressCtrl::GetPos()
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, PBM_GETPOS, 0, 0); }
_AFXCMN_INLINE int CProgressCtrl::OffsetPos(int nPos)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, PBM_DELTAPOS, nPos, 0L); }
_AFXCMN_INLINE int CProgressCtrl::SetStep(int nStep)
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, PBM_SETSTEP, nStep, 0L); }
_AFXCMN_INLINE int CProgressCtrl::StepIt()
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, PBM_STEPIT, 0, 0L); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CHeaderCtrl::CHeaderCtrl()
	{ }
_AFXCMN_INLINE int CHeaderCtrl::GetItemCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int) ::SendMessage(m_hWnd, HDM_GETITEMCOUNT, 0, 0L); }
_AFXCMN_INLINE int CHeaderCtrl::InsertItem(int nPos, HDITEM* phdi)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, HDM_INSERTITEM, nPos, (LPARAM)phdi); }
_AFXCMN_INLINE BOOL CHeaderCtrl::DeleteItem(int nPos)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, HDM_DELETEITEM, nPos, 0L); }
_AFXCMN_INLINE BOOL CHeaderCtrl::GetItem(int nPos, HDITEM* pHeaderItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, HDM_GETITEM, nPos, (LPARAM)pHeaderItem); }
_AFXCMN_INLINE BOOL CHeaderCtrl::SetItem(int nPos, HDITEM* pHeaderItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, HDM_SETITEM, nPos, (LPARAM)pHeaderItem); }
_AFXCMN_INLINE BOOL CHeaderCtrl::Layout(HDLAYOUT* pHeaderLayout)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, HDM_LAYOUT, 0, (LPARAM)pHeaderLayout); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CImageList::operator HIMAGELIST() const
	{ return m_hImageList;}
_AFXCMN_INLINE HIMAGELIST CImageList::GetSafeHandle() const
	{ return (this == NULL) ? NULL : m_hImageList; }
_AFXCMN_INLINE int CImageList::GetImageCount() const
	{ ASSERT(m_hImageList != NULL); return ImageList_GetImageCount(m_hImageList); }
_AFXCMN_INLINE int CImageList::Add(CBitmap* pbmImage, CBitmap* pbmMask)
	{ ASSERT(m_hImageList != NULL); return ImageList_Add(m_hImageList, (HBITMAP)pbmImage->GetSafeHandle(), (HBITMAP)pbmMask->GetSafeHandle()); }
_AFXCMN_INLINE int CImageList::Add(CBitmap* pbmImage, COLORREF crMask)
	{ ASSERT(m_hImageList != NULL); return ImageList_AddMasked(m_hImageList, (HBITMAP)pbmImage->GetSafeHandle(), crMask); }
_AFXCMN_INLINE BOOL CImageList::Remove(int nImage)
	{ ASSERT(m_hImageList != NULL); return ImageList_Remove(m_hImageList, nImage); }
_AFXCMN_INLINE BOOL CImageList::Replace(int nImage, CBitmap* pbmImage, CBitmap* pbmMask)
	{ ASSERT(m_hImageList != NULL); return ImageList_Replace(m_hImageList, nImage, (HBITMAP)pbmImage->GetSafeHandle(), (HBITMAP)pbmMask->GetSafeHandle()); }
_AFXCMN_INLINE int CImageList::Add(HICON hIcon)
	{ ASSERT(m_hImageList != NULL); return ImageList_AddIcon(m_hImageList, hIcon); }
_AFXCMN_INLINE int CImageList::Replace(int nImage, HICON hIcon)
	{ ASSERT(m_hImageList != NULL); return ImageList_ReplaceIcon(m_hImageList, nImage, hIcon); }
_AFXCMN_INLINE HICON CImageList::ExtractIcon(int nImage)
	{ ASSERT(m_hImageList != NULL); return ImageList_ExtractIcon(NULL, m_hImageList, nImage); }
_AFXCMN_INLINE BOOL CImageList::Draw(CDC* pDC, int nImage, POINT pt, UINT nStyle)
	{ ASSERT(m_hImageList != NULL); ASSERT(pDC != NULL); return ImageList_Draw(m_hImageList, nImage, pDC->GetSafeHdc(), pt.x, pt.y, nStyle); }
_AFXCMN_INLINE COLORREF CImageList::SetBkColor(COLORREF cr)
	{ ASSERT(m_hImageList != NULL); return ImageList_SetBkColor(m_hImageList, cr); }
_AFXCMN_INLINE COLORREF CImageList::GetBkColor() const
	{ ASSERT(m_hImageList != NULL); return ImageList_GetBkColor(m_hImageList); }
_AFXCMN_INLINE BOOL CImageList::SetOverlayImage(int nImage, int nOverlay)
	{ ASSERT(m_hImageList != NULL); return ImageList_SetOverlayImage(m_hImageList, nImage, nOverlay); }
_AFXCMN_INLINE BOOL CImageList::GetImageInfo(int nImage, IMAGEINFO* pImageInfo) const
	{ ASSERT(m_hImageList != NULL); return ImageList_GetImageInfo(m_hImageList, nImage, pImageInfo); }
_AFXCMN_INLINE BOOL CImageList::BeginDrag(int nImage, CPoint ptHotSpot)
	{ ASSERT(m_hImageList != NULL); return ImageList_BeginDrag(m_hImageList, nImage, ptHotSpot.x, ptHotSpot.y); }
_AFXCMN_INLINE void PASCAL CImageList::EndDrag()
	{ ImageList_EndDrag(); }
_AFXCMN_INLINE BOOL PASCAL CImageList::DragMove(CPoint pt)
	{ return ImageList_DragMove(pt.x, pt.y); }
_AFXCMN_INLINE BOOL CImageList::SetDragCursorImage(int nDrag, CPoint ptHotSpot)
	{ ASSERT(m_hImageList != NULL); return ImageList_SetDragCursorImage(m_hImageList, nDrag, ptHotSpot.x, ptHotSpot.y); }
_AFXCMN_INLINE BOOL PASCAL CImageList::DragShowNolock(BOOL bShow)
	{return ImageList_DragShowNolock(bShow);}
_AFXCMN_INLINE CImageList* PASCAL CImageList::GetDragImage(LPPOINT lpPoint, LPPOINT lpPointHotSpot)
	{return CImageList::FromHandle(ImageList_GetDragImage(lpPoint, lpPointHotSpot));}
_AFXCMN_INLINE BOOL PASCAL CImageList::DragEnter(CWnd* pWndLock, CPoint point)
	{ return ImageList_DragEnter(pWndLock->GetSafeHwnd(), point.x, point.y); }
_AFXCMN_INLINE BOOL PASCAL CImageList::DragLeave(CWnd* pWndLock)
	{ return ImageList_DragLeave(pWndLock->GetSafeHwnd()); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CTabCtrl::CTabCtrl()
	{ }
_AFXCMN_INLINE CImageList* CTabCtrl::GetImageList() const
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST)::SendMessage(m_hWnd, TCM_GETIMAGELIST, 0, 0L)); }
_AFXCMN_INLINE CImageList* CTabCtrl::SetImageList(CImageList* pImageList)
	{ ASSERT(::IsWindow(m_hWnd)); return CImageList::FromHandle((HIMAGELIST)::SendMessage(m_hWnd, TCM_SETIMAGELIST, 0, (LPARAM)pImageList->GetSafeHandle())); }
_AFXCMN_INLINE int CTabCtrl::GetItemCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_GETITEMCOUNT, 0, 0L); }
_AFXCMN_INLINE BOOL CTabCtrl::GetItem(int nItem, TCITEM* pTabCtrlItem) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_GETITEM, nItem, (LPARAM)pTabCtrlItem); }
_AFXCMN_INLINE BOOL CTabCtrl::SetItem(int nItem, TCITEM* pTabCtrlItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_SETITEM, nItem, (LPARAM)pTabCtrlItem); }
_AFXCMN_INLINE BOOL CTabCtrl::SetItemExtra(int nBytes)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_SETITEMEXTRA, (WPARAM) nBytes, 0); }
_AFXCMN_INLINE void CTabCtrl::SetCurFocus(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_SETCURFOCUS, (WPARAM) nItem, 0); }
_AFXCMN_INLINE BOOL CTabCtrl::InsertItem(int nItem, TCITEM* pTabCtrlItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_INSERTITEM, nItem, (LPARAM)pTabCtrlItem); }
_AFXCMN_INLINE BOOL CTabCtrl::InsertItem(int nItem, LPCTSTR lpszItem)
	{ ASSERT(::IsWindow(m_hWnd)); return CTabCtrl::InsertItem(TCIF_TEXT, nItem, lpszItem, 0, 0); }
_AFXCMN_INLINE BOOL CTabCtrl::InsertItem(int nItem, LPCTSTR lpszItem, int nImage)
	{ ASSERT(::IsWindow(m_hWnd)); return CTabCtrl::InsertItem(TCIF_TEXT|TCIF_IMAGE, nItem, lpszItem, nImage, 0); }
_AFXCMN_INLINE BOOL CTabCtrl::DeleteItem(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_DELETEITEM, nItem, 0L); }
_AFXCMN_INLINE BOOL CTabCtrl::DeleteAllItems()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_DELETEALLITEMS, 0, 0L); }
_AFXCMN_INLINE BOOL CTabCtrl::GetItemRect(int nItem, LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_GETITEMRECT, nItem, (LPARAM)lpRect); }
_AFXCMN_INLINE int CTabCtrl::GetCurSel() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_GETCURSEL, 0, 0L); }
_AFXCMN_INLINE int CTabCtrl::SetCurSel(int nItem)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_SETCURSEL, nItem, 0L); }
_AFXCMN_INLINE int CTabCtrl::HitTest(TCHITTESTINFO* pHitTestInfo) const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_HITTEST, 0, (LPARAM) pHitTestInfo); }
_AFXCMN_INLINE void CTabCtrl::AdjustRect(BOOL bLarger, LPRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_ADJUSTRECT, bLarger, (LPARAM)lpRect); }
_AFXCMN_INLINE CSize CTabCtrl::SetItemSize(CSize size)
	{ ASSERT(::IsWindow(m_hWnd)); return (CSize)::SendMessage(m_hWnd, TCM_SETITEMSIZE, 0, MAKELPARAM(size.cx,size.cy)); }
_AFXCMN_INLINE void CTabCtrl::RemoveImage(int nImage)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_REMOVEIMAGE, nImage, 0L); }
_AFXCMN_INLINE void CTabCtrl::SetPadding(CSize size)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_SETPADDING, 0, MAKELPARAM(size.cx, size.cy)); }
_AFXCMN_INLINE int CTabCtrl::GetRowCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_GETROWCOUNT, 0, 0L); }
_AFXCMN_INLINE CToolTipCtrl* CTabCtrl::GetToolTips() const
	{ ASSERT(::IsWindow(m_hWnd)); return (CToolTipCtrl*)CWnd::FromHandle((HWND)::SendMessage(m_hWnd, TCM_GETTOOLTIPS, 0, 0L)); }
_AFXCMN_INLINE void CTabCtrl::SetToolTips(CToolTipCtrl* pWndTip)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_SETTOOLTIPS, (WPARAM)pWndTip->GetSafeHwnd(), 0L); }
_AFXCMN_INLINE int CTabCtrl::GetCurFocus() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_GETCURFOCUS, 0, 0L); }
_AFXCMN_INLINE int CTabCtrl::SetMinTabWidth(int cx)
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, TCM_SETMINTABWIDTH, 0, (LPARAM)cx); }
_AFXCMN_INLINE void CTabCtrl::DeselectAll(BOOL fExcludeFocus)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, TCM_DESELECTALL, fExcludeFocus, 0); }
_AFXCMN_INLINE BOOL CTabCtrl::HighlightItem(int idItem, BOOL fHighlight /*= TRUE*/)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, TCM_HIGHLIGHTITEM, idItem, MAKELPARAM(fHighlight, 0)); }
_AFXCMN_INLINE DWORD CTabCtrl::GetExtendedStyle()
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, TCM_GETEXTENDEDSTYLE, 0, 0); }
_AFXCMN_INLINE DWORD CTabCtrl::SetExtendedStyle(DWORD dwNewStyle, DWORD dwExMask /*= 0*/)
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD) ::SendMessage(m_hWnd, TCM_SETEXTENDEDSTYLE, dwExMask, dwNewStyle); }

/////////////////////////////////////////////////////////////////////////////

_AFXCMN_INLINE CAnimateCtrl::CAnimateCtrl()
	{ }
_AFXCMN_INLINE BOOL CAnimateCtrl::Open(LPCTSTR lpszName)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_OPEN, 0, (LPARAM)lpszName); }
_AFXCMN_INLINE BOOL CAnimateCtrl::Open(UINT nID)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_OPEN, 0, nID); }
_AFXCMN_INLINE BOOL CAnimateCtrl::Play(UINT nFrom, UINT nTo, UINT nRep)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_PLAY, nRep, MAKELPARAM(nFrom, nTo)); }
_AFXCMN_INLINE BOOL CAnimateCtrl::Stop()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_STOP, 0, 0L); }
_AFXCMN_INLINE BOOL CAnimateCtrl::Close()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_OPEN, 0, 0L); }
_AFXCMN_INLINE BOOL CAnimateCtrl::Seek(UINT nTo)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, ACM_PLAY, 0, MAKELPARAM(nTo, nTo)); }

/////////////////////////////////////////////////////////////////////////////

#ifndef _AFX_NO_RICHEDIT_SUPPORT
_AFXCMN_INLINE CRichEditCtrl::CRichEditCtrl()
	{ }
_AFXCMN_INLINE BOOL CRichEditCtrl::CanUndo() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_CANUNDO, 0, 0); }
_AFXCMN_INLINE int CRichEditCtrl::GetLineCount() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_GETLINECOUNT, 0, 0); }
_AFXCMN_INLINE BOOL CRichEditCtrl::GetModify() const
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_GETMODIFY, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::SetModify(BOOL bModified /* = TRUE */)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETMODIFY, bModified, 0);}
_AFXCMN_INLINE void CRichEditCtrl::GetRect(LPRECT lpRect) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_GETRECT, 0, (LPARAM)lpRect); }
_AFXCMN_INLINE CPoint CRichEditCtrl::GetCharPos(long lChar) const
	{ ASSERT(::IsWindow(m_hWnd)); CPoint pt; ::SendMessage(m_hWnd, EM_POSFROMCHAR, (WPARAM)&pt, (LPARAM)lChar); return pt;}
_AFXCMN_INLINE void CRichEditCtrl::SetOptions(WORD wOp, DWORD dwFlags)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETOPTIONS, (WPARAM)wOp, (LPARAM)dwFlags); }
_AFXCMN_INLINE void CRichEditCtrl::EmptyUndoBuffer()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_EMPTYUNDOBUFFER, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::ReplaceSel(LPCTSTR lpszNewText, BOOL bCanUndo)
	{ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_REPLACESEL, (WPARAM) bCanUndo, (LPARAM)lpszNewText); }
_AFXCMN_INLINE void CRichEditCtrl::SetRect(LPCRECT lpRect)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_SETRECT, 0, (LPARAM)lpRect); }
_AFXCMN_INLINE BOOL CRichEditCtrl::Undo()
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_UNDO, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::Clear()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CLEAR, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::Copy()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_COPY, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::Cut()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_CUT, 0, 0); }
_AFXCMN_INLINE void CRichEditCtrl::Paste()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, WM_PASTE, 0, 0); }
_AFXCMN_INLINE BOOL CRichEditCtrl::SetReadOnly(BOOL bReadOnly /* = TRUE */ )
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETREADONLY, bReadOnly, 0L); }
_AFXCMN_INLINE int CRichEditCtrl::GetFirstVisibleLine() const
	{ ASSERT(::IsWindow(m_hWnd)); return (int)::SendMessage(m_hWnd, EM_GETFIRSTVISIBLELINE, 0, 0L); }
_AFXCMN_INLINE BOOL CRichEditCtrl::DisplayBand(LPRECT pDisplayRect)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_DISPLAYBAND, 0, (LPARAM)pDisplayRect); }
_AFXCMN_INLINE void CRichEditCtrl::GetSel(CHARRANGE &cr) const
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_EXGETSEL, 0, (LPARAM)&cr); }

_AFXCMN_INLINE void CRichEditCtrl::LimitText(long nChars)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_EXLIMITTEXT, 0, nChars); }

_AFXCMN_INLINE long CRichEditCtrl::LineFromChar(long nIndex) const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_EXLINEFROMCHAR, 0, nIndex); }

_AFXCMN_INLINE void CRichEditCtrl::SetSel(CHARRANGE &cr)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_EXSETSEL, 0, (LPARAM)&cr); }

_AFXCMN_INLINE long CRichEditCtrl::FindText(DWORD dwFlags, FINDTEXTEX* pFindText) const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_FINDTEXTEX, dwFlags, (LPARAM)pFindText); }

_AFXCMN_INLINE long CRichEditCtrl::FormatRange(FORMATRANGE* pfr, BOOL bDisplay)
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_FORMATRANGE, (WPARAM)bDisplay, (LPARAM)pfr); }

_AFXCMN_INLINE long CRichEditCtrl::GetEventMask() const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_GETEVENTMASK, 0, 0L); }

_AFXCMN_INLINE long CRichEditCtrl::GetLimitText() const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_GETLIMITTEXT, 0, 0L); }

_AFXCMN_INLINE long CRichEditCtrl::GetSelText(LPSTR lpBuf) const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_GETSELTEXT, 0, (LPARAM)lpBuf); }

_AFXCMN_INLINE void CRichEditCtrl::HideSelection(BOOL bHide, BOOL bPerm)
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_HIDESELECTION, bHide, bPerm); }

_AFXCMN_INLINE void CRichEditCtrl::RequestResize()
	{ ASSERT(::IsWindow(m_hWnd)); ::SendMessage(m_hWnd, EM_REQUESTRESIZE, 0, 0L); }

_AFXCMN_INLINE WORD CRichEditCtrl::GetSelectionType() const
	{ ASSERT(::IsWindow(m_hWnd)); return (WORD)::SendMessage(m_hWnd, EM_SELECTIONTYPE, 0, 0L); }

_AFXCMN_INLINE COLORREF CRichEditCtrl::SetBackgroundColor(BOOL bSysColor, COLORREF cr)
	{ ASSERT(::IsWindow(m_hWnd)); return (COLORREF)::SendMessage(m_hWnd, EM_SETBKGNDCOLOR, bSysColor, cr); }

_AFXCMN_INLINE DWORD CRichEditCtrl::SetEventMask(DWORD dwEventMask)
	{ ASSERT(::IsWindow(m_hWnd)); return (DWORD)::SendMessage(m_hWnd, EM_SETEVENTMASK, 0, dwEventMask); }

_AFXCMN_INLINE BOOL CRichEditCtrl::SetOLECallback(IRichEditOleCallback* pCallback)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETOLECALLBACK, 0, (LPARAM)pCallback); }

_AFXCMN_INLINE BOOL CRichEditCtrl::SetTargetDevice(HDC hDC, long lLineWidth)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETTARGETDEVICE, (WPARAM)hDC, lLineWidth); }

_AFXCMN_INLINE BOOL CRichEditCtrl::SetTargetDevice(CDC &dc, long lLineWidth)
	{ ASSERT(::IsWindow(m_hWnd)); return (BOOL)::SendMessage(m_hWnd, EM_SETTARGETDEVICE, (WPARAM)dc.m_hDC, lLineWidth); }

_AFXCMN_INLINE long CRichEditCtrl::StreamIn(int nFormat, EDITSTREAM &es)
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_STREAMIN, nFormat, (LPARAM)&es); }

_AFXCMN_INLINE long CRichEditCtrl::StreamOut(int nFormat, EDITSTREAM &es)
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, EM_STREAMOUT, nFormat, (LPARAM)&es); }

_AFXCMN_INLINE long CRichEditCtrl::GetTextLength() const
	{ ASSERT(::IsWindow(m_hWnd)); return (long)::SendMessage(m_hWnd, WM_GETTEXTLENGTH, NULL, NULL); }

#endif //!_AFX_NO_RICHEDIT_SUPPORT
/////////////////////////////////////////////////////////////////////////////

#endif //_AFXCMN_INLINE

/////////////////////////////////////////////////////////////////////////////
