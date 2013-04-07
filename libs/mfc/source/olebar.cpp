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

#ifdef AFX_OLE4_SEG
#pragma code_seg(AFX_OLE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleResizeBar

COleResizeBar::COleResizeBar()
{
	// setup the state flags to do resize handles outside with hatched border
	m_tracker.m_nStyle =
		CRectTracker::hatchedBorder|CRectTracker::resizeOutside|
		CRectTracker::solidLine;

	// the actual rectangle is updated in COleResizeBar::OnSize
}

COleResizeBar::~COleResizeBar()
{
}

BOOL COleResizeBar::Create(CWnd* pParentWnd, DWORD dwStyle, UINT nID)
{
	ASSERT_VALID(this);

	if (pParentWnd != NULL)
		ASSERT_VALID(pParentWnd);   // must have a parent

	// force WS_CLIPSIBLINGS (avoids SetWindowPos bugs)
	dwStyle |= WS_CLIPSIBLINGS;

	VERIFY(AfxDeferRegisterClass(AFX_WNDCONTROLBAR_REG));

	// create the HWND
	CRect rect;
	rect.SetRectEmpty();
	// Note: Parent must resize itself for control bar to be resized
	return CWnd::Create(AFX_WNDCONTROLBAR, NULL, dwStyle, rect, pParentWnd, nID);
}

/////////////////////////////////////////////////////////////////////////////
// COleResizeBar message handling

BEGIN_MESSAGE_MAP(COleResizeBar, CControlBar)
	//{{AFX_MSG_MAP(COleResizeBar)
	ON_WM_ERASEBKGND()
	ON_WM_PAINT()
	ON_WM_SIZE()
	ON_WM_SETCURSOR()
	ON_WM_LBUTTONDOWN()
	ON_MESSAGE(WM_SIZEPARENT, OnSizeParent)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL COleResizeBar::OnEraseBkgnd(CDC*)
{
	return TRUE;    // no erasing necessary
}

void COleResizeBar::OnPaint()
{
	CPaintDC dc(this);

	// always use the same brush origin
	CRect rect;
	GetWindowRect(&rect);
	dc.SetBrushOrg(rect.left & 7, rect.top & 7);

	// draw it
	m_tracker.Draw(&dc);
}

void COleResizeBar::OnSize(UINT /*nType*/, int /*cx*/, int /*cy*/)
{
	GetClientRect(&m_tracker.m_rect);
	int nHandleSize = m_tracker.m_nHandleSize;
	m_tracker.m_rect.InflateRect(-nHandleSize, -nHandleSize);
}

BOOL COleResizeBar::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
	// hit-test the tracker -- we only care about hits on handles
	CPoint point;
	::GetCursorPos(&point);
	ScreenToClient(&point);
	int hit = m_tracker.HitTest(point);
	if (hit < 0)
		return CControlBar::OnSetCursor(pWnd, nHitTest, message);

	// let the tracker handle setting the cursor
	return m_tracker.SetCursor(pWnd, nHitTest);
}

void COleResizeBar::OnLButtonDown(UINT /*nFlags*/, CPoint point)
{
	// track to parent of the parent
	CWnd* pFrameWnd = GetParentFrame();
	ASSERT_VALID(pFrameWnd);
	CWnd* pParent = pFrameWnd->GetParent();

	pFrameWnd->UpdateWindow();  // update ourselves

	// limit tracking to parent client rectangle
	if (pParent != NULL)
	{
		pParent->UpdateWindow();    // always update before tracking

		// clip cursor to parent window
		CRect rect;
		pParent->GetClientRect(&rect);
		pParent->ClientToScreen(&rect);
		::ClipCursor(&rect);
	}

	// save the rect, track, then restore
	CRect rectSave = m_tracker.m_rect;
	BOOL bNotify = m_tracker.Track(this, point, FALSE, pParent);
	CRect rectNew = m_tracker.m_rect;
	m_tracker.m_rect = rectSave;

	// allow full mouse movement again
	::ClipCursor(NULL);

	// notify owner window if tracker changed
	if (bNotify)
	{
		CWnd* pOwner = GetOwner();
		ASSERT_VALID(pOwner);

		// convert relative to parent coordinates
		ClientToScreen(&rectNew);
		pOwner->ScreenToClient(&rectNew);

		// send notification to owner
		pOwner->SendMessage(WM_SIZECHILD, (WPARAM)_AfxGetDlgCtrlID(m_hWnd),
			(LPARAM)(LPCRECT)&rectNew);
	}
}

LRESULT COleResizeBar::OnSizeParent(WPARAM, LPARAM lParam)
{
	AFX_SIZEPARENTPARAMS* lpLayout = (AFX_SIZEPARENTPARAMS*)lParam;

	// only resize the window if doing layout and not just rect query
	if (lpLayout->hDWP != NULL)
		AfxRepositionWindow(lpLayout, m_hWnd, &lpLayout->rect);

	// always adjust the rectangle after the resize
	int nHandleSize = m_tracker.m_nHandleSize;
	::InflateRect(&lpLayout->rect, -nHandleSize, -nHandleSize);

	return 0;
}

void COleResizeBar::OnUpdateCmdUI(CFrameWnd* /*pTarget*/,
	BOOL /*bDisableIfNoHndler*/)
{
	// just do nothing
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleResizeBar, CControlBar)

/////////////////////////////////////////////////////////////////////////////
