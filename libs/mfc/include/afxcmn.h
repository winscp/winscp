// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXCMN_H__
#define __AFXCMN_H__

#ifdef _AFX_NO_AFXCMN_SUPPORT
	#error Windows Common Control classes not supported in this library variant.
#endif

#ifndef __AFXWIN_H__
	#include <afxwin.h>
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifndef IMAGE_BITMAP
#define IMAGE_BITMAP 0
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

#ifndef _AFX_NO_RICHEDIT_SUPPORT
	#ifndef _RICHEDIT_
		#include <richedit.h>
	#endif
	#ifdef __AFXOLE_H__  // only include richole if OLE support is included
		#ifndef _RICHOLE_
			#include <richole.h>
			#define _RICHOLE_
		#endif
	#else
		struct IRichEditOle;
		struct IRichEditOleCallback;
	#endif
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXCMN - MFC COMCTL32 Control Classes

// Classes declared in this file

//TOOLINFO
	class CToolInfo;

//CObject
	class CImageList;
	//CCmdTarget;
		//CWnd
			// class CListBox;
				class CDragListBox;
			class CListCtrl;
			class CTreeCtrl;
			class CSpinButtonCtrl;
			class CHeaderCtrl;
			class CSliderCtrl;
			class CProgressCtrl;
			// class CComboBox;
				class CComboBoxEx;
			class CHotKeyCtrl;
			class CToolTipCtrl;
			class CTabCtrl;
			class CAnimateCtrl;
			class CToolBarCtrl;
			class CStatusBarCtrl;
			class CRichEditCtrl;
			class CIPAddressCtrl;

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////
// CToolInfo

#ifdef _UNICODE
class CToolInfo : public tagTOOLINFOW
#else
class CToolInfo : public tagTOOLINFOA
#endif
{
public:
	TCHAR szText[256];
};

/////////////////////////////////////////////////////////////////////////////
// CDragListBox

class CDragListBox : public CListBox
{
	DECLARE_DYNAMIC(CDragListBox)

// Constructors
public:
	CDragListBox();

// Attributes
	int ItemFromPt(CPoint pt, BOOL bAutoScroll = TRUE) const;

// Operations
	virtual void DrawInsert(int nItem);

// Overridables
	virtual BOOL BeginDrag(CPoint pt);
	virtual void CancelDrag(CPoint pt);
	virtual UINT Dragging(CPoint pt);
	virtual void Dropped(int nSrcIndex, CPoint pt);

// Implementation
public:
	int m_nLast;
	void DrawSingle(int nIndex);
	virtual void PreSubclassWindow();
	virtual ~CDragListBox();
protected:
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);
};

/////////////////////////////////////////////////////////////////////////////
// CStatusBarCtrl

class CStatusBarCtrl : public CWnd
{
	DECLARE_DYNAMIC(CStatusBarCtrl)

// Constructors
public:
	CStatusBarCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	BOOL SetText(LPCTSTR lpszText, int nPane, int nType);
	CString GetText(int nPane, int* pType = NULL) const;
	int GetText(LPCTSTR lpszText, int nPane, int* pType = NULL) const;
	int GetTextLength(int nPane, int* pType = NULL) const;
	BOOL SetParts(int nParts, int* pWidths);
	int GetParts(int nParts, int* pParts) const;
	BOOL GetBorders(int* pBorders) const;
	BOOL GetBorders(int& nHorz, int& nVert, int& nSpacing) const;
	void SetMinHeight(int nMin);
	BOOL SetSimple(BOOL bSimple = TRUE);
	BOOL GetRect(int nPane, LPRECT lpRect) const;
	BOOL IsSimple() const;
	CString GetTipText(int nPane) const;
	void SetTipText(int nPane, LPCTSTR pszTipText);
	COLORREF SetBkColor(COLORREF cr);
	BOOL SetIcon(int nPane, HICON hIcon);

// Overridables
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);

// Implementation
public:
	virtual ~CStatusBarCtrl();
protected:
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);
};

/////////////////////////////////////////////////////////////////////////////
// CListCtrl

class CListCtrl : public CWnd
{
	DECLARE_DYNAMIC(CListCtrl)

// Constructors
public:
	CListCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	COLORREF GetBkColor() const;
	BOOL SetBkColor(COLORREF cr);
	CImageList* GetImageList(int nImageList) const;
	CImageList* SetImageList(CImageList* pImageList, int nImageListType);
	int GetItemCount() const;
	BOOL GetItem(LVITEM* pItem) const;
	BOOL SetItem(const LVITEM* pItem);
	BOOL SetItem(int nItem, int nSubItem, UINT nMask, LPCTSTR lpszItem,
		int nImage, UINT nState, UINT nStateMask, LPARAM lParam);
	UINT GetCallbackMask() const;
	BOOL SetCallbackMask(UINT nMask);
	int GetNextItem(int nItem, int nFlags) const;
	POSITION GetFirstSelectedItemPosition() const;
	int GetNextSelectedItem(POSITION& pos) const;
	BOOL GetItemRect(int nItem, LPRECT lpRect, UINT nCode) const;
	BOOL SetItemPosition(int nItem, POINT pt);
	BOOL GetItemPosition(int nItem, LPPOINT lpPoint) const;
	int GetStringWidth(LPCTSTR lpsz) const;
	CEdit* GetEditControl() const;
	BOOL GetColumn(int nCol, LVCOLUMN* pColumn) const;
	BOOL SetColumn(int nCol, const LVCOLUMN* pColumn);
	int GetColumnWidth(int nCol) const;
	BOOL SetColumnWidth(int nCol, int cx);
	BOOL GetViewRect(LPRECT lpRect) const;
	COLORREF GetTextColor() const;
	BOOL SetTextColor(COLORREF cr);
	COLORREF GetTextBkColor() const;
	BOOL SetTextBkColor(COLORREF cr);
	int GetTopIndex() const;
	int GetCountPerPage() const;
	BOOL GetOrigin(LPPOINT lpPoint) const;
	BOOL SetItemState(int nItem, LVITEM* pItem);
	BOOL SetItemState(int nItem, UINT nState, UINT nMask);
	UINT GetItemState(int nItem, UINT nMask) const;
	CString GetItemText(int nItem, int nSubItem) const;
	int GetItemText(int nItem, int nSubItem, LPTSTR lpszText, int nLen) const;
	BOOL SetItemText(int nItem, int nSubItem, LPCTSTR lpszText);
	void SetItemCount(int nItems);
	BOOL SetItemData(int nItem, DWORD dwData);
	DWORD GetItemData(int nItem) const;
	UINT GetSelectedCount() const;
	BOOL SetColumnOrderArray(int iCount, LPINT piArray);
	BOOL GetColumnOrderArray(LPINT piArray, int iCount = -1);
	CSize SetIconSpacing(CSize size);
	CSize SetIconSpacing(int cx, int cy);
	CHeaderCtrl* GetHeaderCtrl();
	HCURSOR GetHotCursor();
	HCURSOR SetHotCursor(HCURSOR hc);
	BOOL GetSubItemRect(int iItem, int iSubItem, int nArea, CRect& ref);
	int GetHotItem();
	int SetHotItem(int iIndex);
	int GetSelectionMark();
	int SetSelectionMark(int iIndex);
	DWORD GetExtendedStyle();
	DWORD SetExtendedStyle(DWORD dwNewStyle);
	int SubItemHitTest(LPLVHITTESTINFO pInfo);
	void SetWorkAreas(int nWorkAreas, LPRECT lpRect);
	BOOL SetItemCountEx(int iCount, DWORD dwFlags = LVSICF_NOINVALIDATEALL);
	CSize ApproximateViewRect(CSize sz = CSize(-1, -1), int iCount = -1) const;
	BOOL GetBkImage(LVBKIMAGE* plvbkImage) const;
	DWORD GetHoverTime() const;
	void GetWorkAreas(int nWorkAreas, LPRECT prc) const;
	BOOL SetBkImage(HBITMAP hbm, BOOL fTile = TRUE, int xOffsetPercent = 0, int yOffsetPercent = 0);
	BOOL SetBkImage(LPTSTR pszUrl, BOOL fTile = TRUE, int xOffsetPercent = 0, int yOffsetPercent = 0);
	BOOL SetBkImage(LVBKIMAGE* plvbkImage);
	DWORD SetHoverTime(DWORD dwHoverTime = (DWORD)-1);
	UINT GetNumberOfWorkAreas() const;
	BOOL GetCheck(int nItem) const;
	BOOL SetCheck(int nItem, BOOL fCheck = TRUE);

// Operations
	int InsertItem(const LVITEM* pItem);
	int InsertItem(int nItem, LPCTSTR lpszItem);
	int InsertItem(int nItem, LPCTSTR lpszItem, int nImage);
	BOOL DeleteItem(int nItem);
	BOOL DeleteAllItems();
	int FindItem(LVFINDINFO* pFindInfo, int nStart = -1) const;
	int HitTest(LVHITTESTINFO* pHitTestInfo) const;
	int HitTest(CPoint pt, UINT* pFlags = NULL) const;
	BOOL EnsureVisible(int nItem, BOOL bPartialOK);
	BOOL Scroll(CSize size);
	BOOL RedrawItems(int nFirst, int nLast);
	BOOL Arrange(UINT nCode);
	CEdit* EditLabel(int nItem);
	int InsertColumn(int nCol, const LVCOLUMN* pColumn);
	int InsertColumn(int nCol, LPCTSTR lpszColumnHeading,
		int nFormat = LVCFMT_LEFT, int nWidth = -1, int nSubItem = -1);
	BOOL DeleteColumn(int nCol);
	CImageList* CreateDragImage(int nItem, LPPOINT lpPoint);
	BOOL Update(int nItem);
	BOOL SortItems(PFNLVCOMPARE pfnCompare, DWORD dwData);

// Overridables
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);

// Implementation
public:
	int InsertItem(UINT nMask, int nItem, LPCTSTR lpszItem, UINT nState,
		UINT nStateMask, int nImage, LPARAM lParam);
	virtual ~CListCtrl();
protected:
	void RemoveImageList(int nImageList);
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);
protected:
	//{{AFX_MSG(CListCtrl)
	afx_msg void OnNcDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CTreeCtrl

class CTreeCtrl : public CWnd
{
	DECLARE_DYNAMIC(CTreeCtrl)

// Constructors
public:
	CTreeCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	BOOL GetItemRect(HTREEITEM hItem, LPRECT lpRect, BOOL bTextOnly) const;
	UINT GetCount() const;
	UINT GetIndent() const;
	void SetIndent(UINT nIndent);
	CImageList* GetImageList(UINT nImageList) const;
	CImageList* SetImageList(CImageList* pImageList, int nImageListType);
	HTREEITEM GetNextItem(HTREEITEM hItem, UINT nCode) const;
	HTREEITEM GetChildItem(HTREEITEM hItem) const;
	HTREEITEM GetNextSiblingItem(HTREEITEM hItem) const;
	HTREEITEM GetPrevSiblingItem(HTREEITEM hItem) const;
	HTREEITEM GetParentItem(HTREEITEM hItem) const;
	HTREEITEM GetFirstVisibleItem() const;
	HTREEITEM GetNextVisibleItem(HTREEITEM hItem) const;
	HTREEITEM GetPrevVisibleItem(HTREEITEM hItem) const;
	HTREEITEM GetSelectedItem() const;
	HTREEITEM GetDropHilightItem() const;
	HTREEITEM GetRootItem() const;
	BOOL GetItem(TVITEM* pItem) const;
	CString GetItemText(HTREEITEM hItem) const;
	BOOL GetItemImage(HTREEITEM hItem, int& nImage, int& nSelectedImage) const;
	UINT GetItemState(HTREEITEM hItem, UINT nStateMask) const;
	DWORD GetItemData(HTREEITEM hItem) const;
	BOOL SetItem(TVITEM* pItem);
	BOOL SetItem(HTREEITEM hItem, UINT nMask, LPCTSTR lpszItem, int nImage,
		int nSelectedImage, UINT nState, UINT nStateMask, LPARAM lParam);
	BOOL SetItemText(HTREEITEM hItem, LPCTSTR lpszItem);
	BOOL SetItemImage(HTREEITEM hItem, int nImage, int nSelectedImage);
	BOOL SetItemState(HTREEITEM hItem, UINT nState, UINT nStateMask);
	BOOL SetItemData(HTREEITEM hItem, DWORD dwData);
	BOOL ItemHasChildren(HTREEITEM hItem) const;
	CEdit* GetEditControl() const;
	UINT GetVisibleCount() const;
	CToolTipCtrl* GetToolTips() const;
	CToolTipCtrl* SetToolTips(CToolTipCtrl* pWndTip);
	COLORREF GetBkColor() const;
	COLORREF SetBkColor(COLORREF clr);
	SHORT GetItemHeight() const;
	SHORT SetItemHeight(SHORT cyHeight);
	COLORREF GetTextColor() const;
	COLORREF SetTextColor(COLORREF clr);
	BOOL SetInsertMark(HTREEITEM hItem, BOOL fAfter = TRUE);
	BOOL GetCheck(HTREEITEM hItem) const;
	BOOL SetCheck(HTREEITEM hItem, BOOL fCheck = TRUE);
	COLORREF GetInsertMarkColor() const;
	COLORREF SetInsertMarkColor(COLORREF clrNew);

// Operations
	HTREEITEM InsertItem(LPTVINSERTSTRUCT lpInsertStruct);
	HTREEITEM InsertItem(UINT nMask, LPCTSTR lpszItem, int nImage,
		int nSelectedImage, UINT nState, UINT nStateMask, LPARAM lParam,
		HTREEITEM hParent, HTREEITEM hInsertAfter);
	HTREEITEM InsertItem(LPCTSTR lpszItem, HTREEITEM hParent = TVI_ROOT,
		HTREEITEM hInsertAfter = TVI_LAST);
	HTREEITEM InsertItem(LPCTSTR lpszItem, int nImage, int nSelectedImage,
		HTREEITEM hParent = TVI_ROOT, HTREEITEM hInsertAfter = TVI_LAST);
	BOOL DeleteItem(HTREEITEM hItem);
	BOOL DeleteAllItems();
	BOOL Expand(HTREEITEM hItem, UINT nCode);
	BOOL Select(HTREEITEM hItem, UINT nCode);
	BOOL SelectItem(HTREEITEM hItem);
	BOOL SelectDropTarget(HTREEITEM hItem);
	BOOL SelectSetFirstVisible(HTREEITEM hItem);
	CEdit* EditLabel(HTREEITEM hItem);
	HTREEITEM HitTest(CPoint pt, UINT* pFlags = NULL) const;
	HTREEITEM HitTest(TVHITTESTINFO* pHitTestInfo) const;
	CImageList* CreateDragImage(HTREEITEM hItem);
	BOOL SortChildren(HTREEITEM hItem);
	BOOL EnsureVisible(HTREEITEM hItem);
	BOOL SortChildrenCB(LPTVSORTCB pSort);

// Implementation
protected:
	void RemoveImageList(int nImageList);
public:
	virtual ~CTreeCtrl();
	//{{AFX_MSG(CTreeCtrl)
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CSpinButtonCtrl

class CSpinButtonCtrl : public CWnd
{
	DECLARE_DYNAMIC(CSpinButtonCtrl)

// Constructors
public:
	CSpinButtonCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	BOOL SetAccel(int nAccel, UDACCEL* pAccel);
	UINT GetAccel(int nAccel, UDACCEL* pAccel) const;
	int SetBase(int nBase);
	UINT GetBase() const;
	CWnd* SetBuddy(CWnd* pWndBuddy);
	CWnd* GetBuddy() const;
	int SetPos(int nPos);
	int GetPos() const;
	void SetRange(int nLower, int nUpper);
	void SetRange32(int nLower, int nUpper);
	DWORD GetRange() const;
	void GetRange(int &lower, int& upper) const;
	void GetRange32(int &lower, int& upper) const;

// Implementation
public:
	virtual ~CSpinButtonCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CSliderCtrl

class CSliderCtrl : public CWnd
{
	DECLARE_DYNAMIC(CSliderCtrl)

// Constructors
public:
	CSliderCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	int GetLineSize() const;
	int SetLineSize(int nSize);
	int GetPageSize() const;
	int SetPageSize(int nSize);
	int GetRangeMax() const;
	int GetRangeMin() const;
	void GetRange(int& nMin, int& nMax) const;
	void SetRangeMin(int nMin, BOOL bRedraw = FALSE);
	void SetRangeMax(int nMax, BOOL bRedraw = FALSE);
	void SetRange(int nMin, int nMax, BOOL bRedraw = FALSE);
	void GetSelection(int& nMin, int& nMax) const;
	void SetSelection(int nMin, int nMax);
	void GetChannelRect(LPRECT lprc) const;
	void GetThumbRect(LPRECT lprc) const;
	int GetPos() const;
	void SetPos(int nPos);
	UINT GetNumTics() const;
	DWORD* GetTicArray() const;
	int GetTic(int nTic) const;
	int GetTicPos(int nTic) const;
	BOOL SetTic(int nTic);
	void SetTicFreq(int nFreq);
	CWnd* GetBuddy(BOOL fLocation = TRUE) const;
	CWnd* SetBuddy(CWnd* pWndBuddy, BOOL fLocation = TRUE);
	CToolTipCtrl* GetToolTips() const;
	void SetToolTips(CToolTipCtrl* pWndTip);
	int SetTipSide(int nLocation);

// Operations
	void ClearSel(BOOL bRedraw = FALSE);
	void VerifyPos();
	void ClearTics(BOOL bRedraw = FALSE);

// Implementation
public:
	virtual ~CSliderCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CProgressCtrl

class CProgressCtrl : public CWnd
{
	DECLARE_DYNAMIC(CProgressCtrl)

// Constructors
public:
	CProgressCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	void SetRange(short nLower, short nUpper);
	void SetRange32(int nLower, int nUpper);
	void GetRange(int& nLower, int& nUpper);
	int GetPos();
	int SetPos(int nPos);
	int OffsetPos(int nPos);
	int SetStep(int nStep);

// Operations
	int StepIt();

// Implementation
public:
	virtual ~CProgressCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CComboBoxEx

class CComboBoxEx : public CComboBox
{
	DECLARE_DYNAMIC(CComboBoxEx)

// Constructors
public:
	CComboBoxEx();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Operations
	int DeleteItem(int iIndex);
	BOOL GetItem(COMBOBOXEXITEM* pCBItem);
	int InsertItem(const COMBOBOXEXITEM* pCBItem);
	BOOL SetItem(const COMBOBOXEXITEM* pCBItem);

// Attributes
	BOOL HasEditChanged();
	DWORD GetExtendedStyle() const;
	DWORD SetExtendedStyle(DWORD dwExMask, DWORD dwExStyles);
	CEdit* GetEditCtrl();
	CComboBox* GetComboBoxCtrl();
	CImageList* GetImageList() const;
	CImageList* SetImageList(CImageList* pImageList);

	// These functions are supported by the Windows ComboBox control,
	// but not supported by the Windows ComboBoxEx control.

	int Dir(UINT attr, LPCTSTR lpszWildCard);
	int FindString(int nIndexStart, LPCTSTR lpszFind) const;
	int AddString(LPCTSTR lpszString);
	BOOL SetEditSel(int nStartChar, int nEndChar);
	int InsertString(int nIndex, LPCTSTR lpszString);

// Implementation
public:
	virtual ~CComboBoxEx();
};

/////////////////////////////////////////////////////////////////////////////
// CHeaderCtrl

class CHeaderCtrl : public CWnd
{
	DECLARE_DYNAMIC(CHeaderCtrl)

// Constructors
public:
	CHeaderCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	int GetItemCount() const;
	BOOL GetItem(int nPos, HDITEM* pHeaderItem) const;
	BOOL SetItem(int nPos, HDITEM* pHeaderItem);
	CImageList* GetImageList() const;
	CImageList* SetImageList(CImageList* pImageList);
	BOOL GetItemRect(int nIndex, LPRECT lpRect) const;
	BOOL GetOrderArray(LPINT piArray, int iCount = -1);
	BOOL SetOrderArray(int iCount, LPINT piArray);
	int OrderToIndex(int nOrder) const;

// Operations
	int InsertItem(int nPos, HDITEM* phdi);
	BOOL DeleteItem(int nPos);
	BOOL Layout(HDLAYOUT* pHeaderLayout);
	CImageList* CreateDragImage(int nIndex);
	int SetHotDivider(CPoint pt);
	int SetHotDivider(int nIndex);

// Overridables
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);

// Implementation
public:
	virtual ~CHeaderCtrl();
protected:
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);

};

/////////////////////////////////////////////////////////////////////////////
// CHotKeyCtrl

class CHotKeyCtrl : public CWnd
{
	DECLARE_DYNAMIC(CHotKeyCtrl)

// Constructors
public:
	CHotKeyCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	void SetHotKey(WORD wVirtualKeyCode, WORD wModifiers);
	DWORD GetHotKey() const;
	void GetHotKey(WORD &wVirtualKeyCode, WORD &wModifiers) const;

// Operations
	void SetRules(WORD wInvalidComb, WORD wModifiers);

// Implementation
public:
	virtual ~CHotKeyCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CToolTipCtrl

class CToolTipCtrl : public CWnd
{
	DECLARE_DYNAMIC(CToolTipCtrl)

// Constructors
public:
	CToolTipCtrl();
	BOOL Create(CWnd* pParentWnd, DWORD dwStyle = 0);

// Attributes
	void GetText(CString& str, CWnd* pWnd, UINT nIDTool = 0) const;
	BOOL GetToolInfo(CToolInfo& ToolInfo, CWnd* pWnd, UINT nIDTool = 0) const;
	void SetToolInfo(LPTOOLINFO lpToolInfo);
	void SetToolRect(CWnd* pWnd, UINT nIDTool, LPCRECT lpRect);
	int GetToolCount() const;
	int GetDelayTime(DWORD dwDuration) const;
	void SetDelayTime(DWORD dwDuration, int iTime);
	void GetMargin(LPRECT lprc) const;
	void SetMargin(LPRECT lprc);
	int GetMaxTipWidth() const;
	int SetMaxTipWidth(int iWidth);
	COLORREF GetTipBkColor() const;
	void SetTipBkColor(COLORREF clr);
	COLORREF GetTipTextColor() const;
	void SetTipTextColor(COLORREF clr);

// Operations
	void Activate(BOOL bActivate);

	BOOL AddTool(CWnd* pWnd, UINT nIDText, LPCRECT lpRectTool = NULL,
		UINT nIDTool = 0);
	BOOL AddTool(CWnd* pWnd, LPCTSTR lpszText = LPSTR_TEXTCALLBACK,
		LPCRECT lpRectTool = NULL, UINT nIDTool = 0);

	void DelTool(CWnd* pWnd, UINT nIDTool = 0);

	BOOL HitTest(CWnd* pWnd, CPoint pt, LPTOOLINFO lpToolInfo) const;
	void RelayEvent(LPMSG lpMsg);
	void SetDelayTime(UINT nDelay);
	void UpdateTipText(LPCTSTR lpszText, CWnd* pWnd, UINT nIDTool = 0);
	void UpdateTipText(UINT nIDText, CWnd* pWnd, UINT nIDTool = 0);
	void Update();

	void Pop();

// Implementation
public:
	void FillInToolInfo(TOOLINFO& ti, CWnd* pWnd, UINT nIDTool) const;
	virtual ~CToolTipCtrl();
#ifndef _AFXDLL
	virtual BOOL DestroyToolTipCtrl();
#else
	BOOL DestroyToolTipCtrl();
#endif

protected:
	//{{AFX_MSG(CToolTipCtrl)
	afx_msg LRESULT OnDisableModal(WPARAM, LPARAM);
	afx_msg LRESULT OnWindowFromPoint(WPARAM, LPARAM);
	afx_msg LRESULT OnAddTool(WPARAM, LPARAM);
	afx_msg void OnEnable(BOOL bEnable);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	CMapStringToPtr m_mapString;

	friend class CWnd;
	friend class CToolBar;
};

/////////////////////////////////////////////////////////////////////////////
// CTabCtrl

class CTabCtrl : public CWnd
{
	DECLARE_DYNAMIC(CTabCtrl)

// Constructors
public:
	CTabCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	CImageList* GetImageList() const;
	CImageList* SetImageList(CImageList* pImageList);
	int GetItemCount() const;
	BOOL GetItem(int nItem, TCITEM* pTabCtrlItem) const;
	BOOL SetItem(int nItem, TCITEM* pTabCtrlItem);
	BOOL SetItemExtra(int nBytes);
	BOOL GetItemRect(int nItem, LPRECT lpRect) const;
	int GetCurSel() const;
	int SetCurSel(int nItem);
	void SetCurFocus(int nItem);
	CSize SetItemSize(CSize size);
	void SetPadding(CSize size);
	int GetRowCount() const;
	CToolTipCtrl* GetToolTips() const;
	void SetToolTips(CToolTipCtrl* pWndTip);
	int GetCurFocus() const;
	int SetMinTabWidth(int cx);
	DWORD GetExtendedStyle();
	DWORD SetExtendedStyle(DWORD dwNewStyle, DWORD dwExMask = 0);
	DWORD GetItemState(int nItem, DWORD dwMask) const;
	BOOL SetItemState(int nItem, DWORD dwMask, DWORD dwState);

// Operations
	BOOL InsertItem(int nItem, TCITEM* pTabCtrlItem);
	BOOL InsertItem(int nItem, LPCTSTR lpszItem);
	BOOL InsertItem(int nItem, LPCTSTR lpszItem, int nImage);
	BOOL InsertItem(UINT nMask, int nItem, LPCTSTR lpszItem,
		int nImage, LPARAM lParam);
	BOOL InsertItem(UINT nMask, int nItem, LPCTSTR lpszItem,
		int nImage, LPARAM lParam, DWORD dwState, DWORD dwStateMask);
	BOOL DeleteItem(int nItem);
	BOOL DeleteAllItems();
	void AdjustRect(BOOL bLarger, LPRECT lpRect);
	void RemoveImage(int nImage);
	int HitTest(TCHITTESTINFO* pHitTestInfo) const;
	void DeselectAll(BOOL fExcludeFocus);
	BOOL HighlightItem(int idItem, BOOL fHighlight = TRUE);

// Overridables
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct);

// Implementation
public:
	virtual ~CTabCtrl();
protected:
	virtual BOOL OnChildNotify(UINT, WPARAM, LPARAM, LRESULT*);
	//{{AFX_MSG(CTabCtrl)
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// CAnimateCtrl

class CAnimateCtrl : public CWnd
{
	DECLARE_DYNAMIC(CAnimateCtrl)

// Constructors
public:
	CAnimateCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Operations
	BOOL Open(LPCTSTR lpszFileName);
	BOOL Open(UINT nID);
	BOOL Play(UINT nFrom, UINT nTo, UINT nRep);
	BOOL Stop();
	BOOL Close();
	BOOL Seek(UINT nTo);

// Implementation
public:
	virtual ~CAnimateCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CImageList

class CImageList : public CObject
{
	DECLARE_DYNCREATE(CImageList)

// Constructors
public:
	CImageList();
	BOOL Create(int cx, int cy, UINT nFlags, int nInitial, int nGrow);
	BOOL Create(UINT nBitmapID, int cx, int nGrow, COLORREF crMask);
	BOOL Create(LPCTSTR lpszBitmapID, int cx, int nGrow, COLORREF crMask);
	BOOL Create(CImageList& imagelist1, int nImage1, CImageList& imagelist2,
		int nImage2, int dx, int dy);
	BOOL Create(CImageList* pImageList);

// Attributes
	HIMAGELIST m_hImageList;            // must be first data member
	operator HIMAGELIST() const;
	HIMAGELIST GetSafeHandle() const;

	static CImageList* PASCAL FromHandle(HIMAGELIST hImageList);
	static CImageList* PASCAL FromHandlePermanent(HIMAGELIST hImageList);
	static void PASCAL DeleteTempMap();
	BOOL Attach(HIMAGELIST hImageList);
	HIMAGELIST Detach();

	int GetImageCount() const;
	COLORREF SetBkColor(COLORREF cr);
	COLORREF GetBkColor() const;
	BOOL GetImageInfo(int nImage, IMAGEINFO* pImageInfo) const;

// Operations
	BOOL DeleteImageList();
	BOOL SetImageCount(UINT uNewCount);

	int Add(CBitmap* pbmImage, CBitmap* pbmMask);
	int Add(CBitmap* pbmImage, COLORREF crMask);
	BOOL Remove(int nImage);
	BOOL Replace(int nImage, CBitmap* pbmImage, CBitmap* pbmMask);
	int Add(HICON hIcon);
	int Replace(int nImage, HICON hIcon);
	HICON ExtractIcon(int nImage);
	BOOL Draw(CDC* pDC, int nImage, POINT pt, UINT nStyle);
	BOOL SetOverlayImage(int nImage, int nOverlay);
	BOOL Copy(int iDst, int iSrc, UINT uFlags = ILCF_MOVE);
	BOOL Copy(int iDst, CImageList* pSrc, int iSrc, UINT uFlags = ILCF_MOVE);
	BOOL DrawIndirect(IMAGELISTDRAWPARAMS* pimldp);
	BOOL DrawIndirect(CDC* pDC, int nImage, POINT pt, SIZE sz, POINT ptOrigin,
			UINT fStyle = ILD_NORMAL, DWORD dwRop = SRCCOPY,
			COLORREF rgbBack = CLR_DEFAULT, COLORREF rgbFore = CLR_DEFAULT);

#ifndef _AFX_NO_OLE_SUPPORT
	BOOL Read(CArchive* pArchive);
	BOOL Write(CArchive* pArchive);
#endif

// Drag APIs
	BOOL BeginDrag(int nImage, CPoint ptHotSpot);
	static void PASCAL EndDrag();
	static BOOL PASCAL DragMove(CPoint pt);
	BOOL SetDragCursorImage(int nDrag, CPoint ptHotSpot);
	static BOOL PASCAL DragShowNolock(BOOL bShow);
	static CImageList* PASCAL GetDragImage(LPPOINT lpPoint, LPPOINT lpPointHotSpot);
	static BOOL PASCAL DragEnter(CWnd* pWndLock, CPoint point);
	static BOOL PASCAL DragLeave(CWnd* pWndLock);

// Implementation
public:
	virtual ~CImageList();
#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
	virtual void AssertValid() const;
#endif
};

/////////////////////////////////////////////////////////////////////////////
// CToolBarCtrl

struct IDropTarget; // forward declaration

class CToolBarCtrl : public CWnd
{
	DECLARE_DYNAMIC(CToolBarCtrl)
// Construction
public:
	CToolBarCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
public:
	BOOL IsButtonEnabled(int nID) const;
	BOOL IsButtonChecked(int nID) const;
	BOOL IsButtonPressed(int nID) const;
	BOOL IsButtonHidden(int nID) const;
	BOOL IsButtonIndeterminate(int nID) const;
	BOOL SetState(int nID, UINT nState);
	int GetState(int nID) const;
	BOOL GetButton(int nIndex, LPTBBUTTON lpButton) const;
	int GetButtonCount() const;
	BOOL GetItemRect(int nIndex, LPRECT lpRect) const;
	BOOL GetRect(int nID, LPRECT lpRect) const;
	void SetButtonStructSize(int nSize);
	DWORD GetButtonSize() const;
	BOOL SetButtonSize(CSize size);
	BOOL SetBitmapSize(CSize size);
	CToolTipCtrl* GetToolTips() const;
	void SetToolTips(CToolTipCtrl* pTip);
	void SetOwner(CWnd* pWnd);
	void SetRows(int nRows, BOOL bLarger, LPRECT lpRect);
	int GetRows() const;
	BOOL SetCmdID(int nIndex, UINT nID);
	UINT GetBitmapFlags() const;
	CImageList* GetDisabledImageList() const;
	CImageList* GetHotImageList() const;
	CImageList* GetImageList() const;
	DWORD GetStyle() const;
	int GetMaxTextRows() const;
	BOOL IsButtonHighlighted(int nID) const;
	BOOL SetButtonWidth(int cxMin, int cxMax);
	CImageList* SetDisabledImageList(CImageList* pImageList);
	CImageList* SetHotImageList(CImageList* pImageList);
	CImageList* SetImageList(CImageList* pImageList);
	HRESULT GetDropTarget(IDropTarget** ppDropTarget) const;
	BOOL SetIndent(int iIndent);
	BOOL SetMaxTextRows(int iMaxRows);
	void SetStyle(DWORD dwStyle);
	BOOL GetButtonInfo(int nID, TBBUTTONINFO* ptbbi) const;
	BOOL SetButtonInfo(int nID, TBBUTTONINFO* ptbbi);
	DWORD SetDrawTextFlags(DWORD dwMask, DWORD dwDTFlags);
	BOOL GetAnchorHighlight() const;
	BOOL SetAnchorHighlight(BOOL fAnchor = TRUE);
	int GetHotItem() const;
	int SetHotItem(int nHot);
	void GetInsertMark(TBINSERTMARK* ptbim) const;
	void SetInsertMark(TBINSERTMARK* ptbim);
	BOOL GetMaxSize(LPSIZE pSize) const;
	BOOL InsertMarkHitTest(LPPOINT ppt, LPTBINSERTMARK ptbim) const;
	DWORD GetExtendedStyle() const;
	DWORD SetExtendedStyle(DWORD dwExStyle);
	COLORREF GetInsertMarkColor() const;
	COLORREF SetInsertMarkColor(COLORREF clrNew);

// Operations
public:
	BOOL EnableButton(int nID, BOOL bEnable = TRUE);
	BOOL CheckButton(int nID, BOOL bCheck = TRUE);
	BOOL PressButton(int nID, BOOL bPress = TRUE);
	BOOL HideButton(int nID, BOOL bHide = TRUE);
	BOOL Indeterminate(int nID, BOOL bIndeterminate = TRUE);
	int AddBitmap(int nNumButtons, UINT nBitmapID);
	int AddBitmap(int nNumButtons, CBitmap* pBitmap);
	BOOL AddButtons(int nNumButtons, LPTBBUTTON lpButtons);
	BOOL InsertButton(int nIndex, LPTBBUTTON lpButton);
	BOOL DeleteButton(int nIndex);
	UINT CommandToIndex(UINT nID) const;
	void SaveState(HKEY hKeyRoot, LPCTSTR lpszSubKey,
		LPCTSTR lpszValueName);
	void RestoreState(HKEY hKeyRoot, LPCTSTR lpszSubKey,
		LPCTSTR lpszValueName);
	void LoadImages(int iBitmapID, HINSTANCE hinst);
	BOOL MapAccelerator(TCHAR chAccel, UINT* pIDBtn);
	BOOL MarkButton(int nID, BOOL fHighlight = TRUE);
	BOOL MoveButton(UINT nOldPos, UINT nNewPos);
	int HitTest(LPPOINT ppt) const;

	void Customize();
	int AddString(UINT nStringID);
	int AddStrings(LPCTSTR lpszStrings);
	void AutoSize();

// Implementation
public:
	virtual ~CToolBarCtrl();

protected:
	//{{AFX_MSG(CToolBarCtrl)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

class CReBarCtrl : public CWnd
{
	DECLARE_DYNAMIC(CReBarCtrl)
// Construction
public:
	CReBarCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
public:
	UINT GetBandCount() const;
	BOOL GetBandInfo(UINT uBand, REBARBANDINFO* prbbi) const;
	UINT GetBarHeight() const;
	BOOL GetBarInfo(REBARINFO* prbi) const;
	COLORREF GetBkColor() const;
	IDropTarget* GetDropTarget() const;
	BOOL GetRect(UINT uBand, LPRECT prc) const;
	UINT GetRowCount() const;
	UINT GetRowHeight(UINT uRow) const;
	COLORREF GetTextColor() const;
	CToolTipCtrl* GetToolTips() const;
	int IDToIndex(UINT uBandID) const;
	BOOL SetBandInfo(UINT uBand, REBARBANDINFO* prbbi);
	BOOL SetBarInfo(REBARINFO* prbi);
	COLORREF SetBkColor(COLORREF clr);
	CWnd* SetOwner(CWnd* pWnd);
	COLORREF SetTextColor(COLORREF clr);
	void SetToolTips(CToolTipCtrl* pToolTip);
	CImageList* GetImageList() const;
	BOOL SetImageList(CImageList* pImageList);
	void GetBandBorders(UINT uBand, LPRECT prc) const;
	CPalette* GetPalette() const;
	CPalette* SetPalette(HPALETTE hPal);
	BOOL GetColorScheme(COLORSCHEME* lpcs);
	void SetColorScheme(const COLORSCHEME* lpcs);

// Operations
public:
	void BeginDrag(UINT uBand, DWORD dwPos = (DWORD)-1);
	BOOL DeleteBand(UINT uBand);
	void DragMove(DWORD dwPos = (DWORD)-1);
	void EndDrag();
	int HitTest(RBHITTESTINFO* prbht);
	BOOL InsertBand(UINT uIndex, REBARBANDINFO* prbbi);
	void MaximizeBand(UINT uBand);
	void MinimizeBand(UINT uBand);
	BOOL ShowBand(UINT uBand, BOOL fShow = TRUE);
	BOOL SizeToRect(CRect& rect);
	BOOL MoveBand(UINT uFrom, UINT uTo);
};

#ifndef _AFX_NO_RICHEDIT_SUPPORT
/////////////////////////////////////////////////////////////////////////////
// CRichEditCtrl

class CRichEditCtrl : public CWnd
{
	DECLARE_DYNAMIC(CRichEditCtrl)

// Constructors
public:
	CRichEditCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	BOOL CanUndo() const;
	int GetLineCount() const;
	BOOL GetModify() const;
	void SetModify(BOOL bModified = TRUE);
	void GetRect(LPRECT lpRect) const;
	CPoint GetCharPos(long lChar) const;
	void SetOptions(WORD wOp, DWORD dwFlags);

	// NOTE: first word in lpszBuffer must contain the size of the buffer!
	int GetLine(int nIndex, LPTSTR lpszBuffer) const;
	int GetLine(int nIndex, LPTSTR lpszBuffer, int nMaxLength) const;

	BOOL CanPaste(UINT nFormat = 0) const;
	void GetSel(long& nStartChar, long& nEndChar) const;
	void GetSel(CHARRANGE &cr) const;
	void LimitText(long nChars = 0);
	long LineFromChar(long nIndex) const;
	void SetSel(long nStartChar, long nEndChar);
	void SetSel(CHARRANGE &cr);
	DWORD GetDefaultCharFormat(CHARFORMAT &cf) const;
	DWORD GetSelectionCharFormat(CHARFORMAT &cf) const;
	long GetEventMask() const;
	long GetLimitText() const;
	DWORD GetParaFormat(PARAFORMAT &pf) const;
	// richedit EM_GETSELTEXT is ANSI
	long GetSelText(LPSTR lpBuf) const;
	CString GetSelText() const;
	WORD GetSelectionType() const;
	COLORREF SetBackgroundColor(BOOL bSysColor, COLORREF cr);
	BOOL SetDefaultCharFormat(CHARFORMAT &cf);
	BOOL SetSelectionCharFormat(CHARFORMAT &cf);
	BOOL SetWordCharFormat(CHARFORMAT &cf);
	DWORD SetEventMask(DWORD dwEventMask);
	BOOL SetParaFormat(PARAFORMAT &pf);
	BOOL SetTargetDevice(HDC hDC, long lLineWidth);
	BOOL SetTargetDevice(CDC &dc, long lLineWidth);
	long GetTextLength() const;
	BOOL SetReadOnly(BOOL bReadOnly = TRUE);
	int GetFirstVisibleLine() const;

// Operations
	void EmptyUndoBuffer();

	int LineIndex(int nLine = -1) const;
	int LineLength(int nLine = -1) const;
	void LineScroll(int nLines, int nChars = 0);
	void ReplaceSel(LPCTSTR lpszNewText, BOOL bCanUndo = FALSE);
	void SetRect(LPCRECT lpRect);

	BOOL DisplayBand(LPRECT pDisplayRect);
	long FindText(DWORD dwFlags, FINDTEXTEX* pFindText) const;
	long FormatRange(FORMATRANGE* pfr, BOOL bDisplay = TRUE);
	void HideSelection(BOOL bHide, BOOL bPerm);
	void PasteSpecial(UINT nClipFormat, DWORD dvAspect = 0, HMETAFILE hMF = 0);
	void RequestResize();
	long StreamIn(int nFormat, EDITSTREAM &es);
	long StreamOut(int nFormat, EDITSTREAM &es);

	// Clipboard operations
	BOOL Undo();
	void Clear();
	void Copy();
	void Cut();
	void Paste();

// OLE support
	IRichEditOle* GetIRichEditOle() const;
	BOOL SetOLECallback(IRichEditOleCallback* pCallback);

// Implementation
public:
	virtual ~CRichEditCtrl();
};
#endif //!_AFX_NO_RICHEDIT_SUPPORT


/////////////////////////////////////////////////////////////////////////////
// CIPAddressCtrl

class CIPAddressCtrl : public CWnd
{
	DECLARE_DYNAMIC(CIPAddressCtrl)

// Constructors
	CIPAddressCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	BOOL IsBlank() const;
	void ClearAddress();
	int GetAddress(BYTE& nField0, BYTE& nField1, BYTE& nField2, BYTE& nField3);
	int GetAddress(DWORD& dwAddress);
	void SetAddress(DWORD dwAddress); //TODO: return value?
	void SetAddress(BYTE nField0, BYTE nField1, BYTE nField2, BYTE nField3); //TODO: return value?
	void SetFieldFocus(WORD nField); //TODO: return value?
	void SetFieldRange(int nField, BYTE nLower, BYTE nUpper); //TODO: return value?

// Implementation
public:
	virtual ~CIPAddressCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXCMN_INLINE AFX_INLINE
#include <afxcmn.inl>
#include <afxcmn2.inl>
#undef _AFXCMN_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXCMN_H__

/////////////////////////////////////////////////////////////////////////////
