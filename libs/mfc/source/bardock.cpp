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

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CDockBar

BEGIN_MESSAGE_MAP(CDockBar, CControlBar)
	//{{AFX_MSG_MAP(CDockBar)
	ON_WM_NCCALCSIZE()
	ON_WM_NCPAINT()
	ON_WM_WINDOWPOSCHANGING()
	ON_WM_PAINT()
	ON_MESSAGE(WM_SIZEPARENT, OnSizeParent)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CDockBar construction

CDockBar::CDockBar(BOOL bFloating)
{
	m_bFloating = bFloating;
	m_bAutoDelete = TRUE;
	m_arrBars.Add(NULL);
	m_bLayoutQuery = FALSE;
	m_rectLayout.SetRectEmpty();

	// assume no margins
	m_cxLeftBorder = m_cxRightBorder = m_cyBottomBorder = m_cyTopBorder = 0;
}

CDockBar::~CDockBar()
{
	for (int i = 0; i < m_arrBars.GetSize(); i++)
	{
		CControlBar* pBar = GetDockedControlBar(i);
		if (pBar != NULL && pBar->m_pDockBar == this)
			pBar->m_pDockBar = NULL;
	}
}

BOOL CDockBar::Create(CWnd* pParentWnd, DWORD dwStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT_KINDOF(CFrameWnd, pParentWnd);

	// save the style
	m_dwStyle = (dwStyle & CBRS_ALL);

	VERIFY(AfxDeferRegisterClass(AFX_WNDCONTROLBAR_REG));

	// create the HWND
	CRect rect;
	rect.SetRectEmpty();

	// Note: Parent must resize itself for control bar to be resized
	return CWnd::Create(_afxWndControlBar, NULL, dwStyle, rect, pParentWnd, nID);
}

BOOL CDockBar::IsDockBar() const
{
	return TRUE;
}

int CDockBar::GetDockedCount() const
{
	int nCount = 0;
	for (int i = 0; i < m_arrBars.GetSize(); i++)
	{
		if (GetDockedControlBar(i) != NULL)
			nCount++;
	}
	return nCount;
}

int CDockBar::GetDockedVisibleCount() const
{
	int nCount = 0;
	for (int i = 0; i < m_arrBars.GetSize(); i++)
	{
		CControlBar* pBar = STATIC_DOWNCAST(CControlBar, (CObject*)GetDockedControlBar(i));
		if (pBar != NULL && pBar->IsVisible())
			nCount++;
	}
	return nCount;
}

/////////////////////////////////////////////////////////////////////////////
// CDockBar operations

void CDockBar::DockControlBar(CControlBar* pBar, LPCRECT lpRect)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pBar);
	ASSERT_KINDOF(CControlBar, pBar);

	CRect rectBar;
	pBar->GetWindowRect(&rectBar);
	if (pBar->m_pDockBar == this && (lpRect == NULL || rectBar == *lpRect))
	{
		// already docked and no change in position
		return;
	}

	// set CBRS_FLOAT_MULTI style if docking bar has it
	if (m_bFloating && (pBar->m_dwDockStyle & CBRS_FLOAT_MULTI))
		m_dwStyle |= CBRS_FLOAT_MULTI;

	m_dwStyle &= ~(CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);
	m_dwStyle |= pBar->m_dwStyle & (CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);

	if (!(m_dwStyle & CBRS_FLOAT_MULTI))
	{
		TCHAR szTitle[_MAX_PATH];
		pBar->GetWindowText(szTitle, _countof(szTitle));
		AfxSetWindowText(m_hWnd, szTitle);
	}

	// align correctly and turn on all borders
	DWORD dwStyle = pBar->GetBarStyle();
	dwStyle &= ~(CBRS_ALIGN_ANY);
	dwStyle |=  (m_dwStyle & CBRS_ALIGN_ANY) | CBRS_BORDER_ANY;

	if (m_bFloating)
		dwStyle |= CBRS_FLOATING;
	else
		dwStyle &= ~CBRS_FLOATING;

	pBar->SetBarStyle(dwStyle);

	// hide first if changing to a new docking site to avoid flashing
	BOOL bShow = FALSE;
	if (pBar->m_pDockBar != this && pBar->IsWindowVisible())
	{
		pBar->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_HIDEWINDOW);
		bShow = TRUE;
	}

	int nPos = -1;
	if (lpRect != NULL)
	{
		// insert into appropriate row
		CRect rect(lpRect);
		ScreenToClient(&rect);
		CPoint ptMid(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
		nPos = Insert(pBar, rect, ptMid);

		// position at requested position
		pBar->SetWindowPos(NULL, rect.left, rect.top, rect.Width(),
			rect.Height(), SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}
	else
	{
		// always add on current row, then create new one
		m_arrBars.Add(pBar);
		m_arrBars.Add(NULL);

		// align off the edge initially
		pBar->SetWindowPos(NULL, -afxData.cxBorder2, -afxData.cyBorder2, 0, 0,
			SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}

	// attach it to the docking site
	if (pBar->GetParent() != this)
		pBar->SetParent(this);
	if (pBar->m_pDockBar == this)
		pBar->m_pDockBar->RemoveControlBar(pBar, nPos);
	else if (pBar->m_pDockBar != NULL)
		pBar->m_pDockBar->RemoveControlBar(pBar, -1, m_bFloating && !pBar->m_pDockBar->m_bFloating);
	pBar->m_pDockBar = this;

	if (bShow)
	{
		ASSERT(!pBar->IsWindowVisible());
		pBar->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);
	}

	// remove any place holder for pBar in this dockbar
	RemovePlaceHolder(pBar);

	// get parent frame for recalc layout
	CFrameWnd* pFrameWnd = GetDockingFrame();
	pFrameWnd->DelayRecalcLayout();
}

void CDockBar::ReDockControlBar(CControlBar* pBar, LPCRECT lpRect)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pBar);
	ASSERT_KINDOF(CControlBar, pBar);
	ASSERT(pBar->m_pDockBar != this); // can't redock here if already docked here

	CRect rectBar;
	pBar->GetWindowRect(&rectBar);
	if (pBar->m_pDockBar == this && (lpRect == NULL || rectBar == *lpRect))
	{
		// already docked and no change in position
		return;
	}

	// set CBRS_FLOAT_MULTI style if docking bar has it
	if (m_bFloating && (pBar->m_dwDockStyle & CBRS_FLOAT_MULTI))
		m_dwStyle |= CBRS_FLOAT_MULTI;

	m_dwStyle &= ~(CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);
	m_dwStyle |= pBar->m_dwStyle & (CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);

	if (!(m_dwStyle & CBRS_FLOAT_MULTI))
	{
		TCHAR szTitle[_MAX_PATH];
		pBar->GetWindowText(szTitle, _countof(szTitle));
		AfxSetWindowText(m_hWnd, szTitle);
	}

	// align correctly and turn on all borders
	DWORD dwStyle = pBar->GetBarStyle();
	dwStyle &= ~(CBRS_ALIGN_ANY);
	dwStyle |=  (m_dwStyle & CBRS_ALIGN_ANY) | CBRS_BORDER_ANY;

	if (m_bFloating)
		dwStyle |= CBRS_FLOATING;
	else
		dwStyle &= ~CBRS_FLOATING;

	pBar->SetBarStyle(dwStyle);

	int nPos = FindBar((CControlBar*)_AfxGetDlgCtrlID(pBar->m_hWnd));
	if (nPos > 0)
		m_arrBars[nPos] = pBar;

	if (lpRect != NULL)
	{
		CRect rect(lpRect);
		ScreenToClient(&rect);

		if (nPos < 1)
		{
			CPoint ptMid(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
			nPos = Insert(pBar, rect, ptMid);
		}

		// position at requested position
		pBar->SetWindowPos(NULL, rect.left, rect.top, rect.Width(),
			rect.Height(), SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}
	else
	{
		if (nPos < 1)
		{
			// always add on current row, then create new one
			m_arrBars.Add(pBar);
			m_arrBars.Add(NULL);
		}

		// align off the edge initially
		pBar->SetWindowPos(NULL, -afxData.cxBorder2, -afxData.cyBorder2, 0, 0,
			SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}

	// attach it to the docking site
	if (pBar->GetParent() != this)
		pBar->SetParent(this);
	if (pBar->m_pDockBar != NULL)
		pBar->m_pDockBar->RemoveControlBar(pBar);
	pBar->m_pDockBar = this;

	// get parent frame for recalc layout
	CFrameWnd* pFrameWnd = GetDockingFrame();
	pFrameWnd->DelayRecalcLayout();
}

void CDockBar::RemovePlaceHolder(CControlBar* pBar)
{
	// remove remembered docking position
	if (HIWORD(pBar) != 0)
		pBar = (CControlBar*)_AfxGetDlgCtrlID(pBar->m_hWnd);
	int nOldPos = FindBar(pBar);
	if (nOldPos > 0)
	{
		m_arrBars.RemoveAt(nOldPos);

		// remove section indicator (NULL) if nothing else in section
		if (m_arrBars[nOldPos-1] == NULL && m_arrBars[nOldPos] == NULL)
			m_arrBars.RemoveAt(nOldPos);
	}
}

BOOL CDockBar::RemoveControlBar(CControlBar* pBar, int nPosExclude, int nAddPlaceHolder)
{
	ASSERT(nAddPlaceHolder == 1 || nAddPlaceHolder == 0 || nAddPlaceHolder == -1);
	ASSERT_VALID(this);
	ASSERT(pBar != NULL);
	int nPos = FindBar(pBar, nPosExclude);
	ASSERT(nPos > 0);

	if (nAddPlaceHolder == 1)
	{
		m_arrBars[nPos] = (void*)_AfxGetDlgCtrlID(pBar->m_hWnd);

		// check for already existing place holder
		int nPosOld = FindBar((CControlBar*)m_arrBars[nPos], nPos);
		if (nPosOld > 0)
		{
			m_arrBars.RemoveAt(nPos);

			// remove section indicator (NULL) if nothing else in section
			if (m_arrBars[nPos-1] == NULL && m_arrBars[nPos] == NULL)
				m_arrBars.RemoveAt(nPos);
		}
	}
	else
	{
		m_arrBars.RemoveAt(nPos);
		if (m_arrBars[nPos-1] == NULL && m_arrBars[nPos] == NULL)
			m_arrBars.RemoveAt(nPos);

		// Remove any pre-existing place holders.
		if (nAddPlaceHolder != -1)
			RemovePlaceHolder(pBar);
	}

	// don't do anything more in the shutdown case!
	if (pBar->m_pDockContext == NULL)
		return FALSE;

	// get parent frame for recalc layout/frame destroy
	CFrameWnd* pFrameWnd = GetDockingFrame();
	if (m_bFloating && GetDockedVisibleCount() == 0)
	{
		if (GetDockedCount() == 0)
		{
			pFrameWnd->DestroyWindow();
			return TRUE; // Self-Destruct
		}
		else
			pFrameWnd->ShowWindow(SW_HIDE);
	}
	else
		pFrameWnd->DelayRecalcLayout();

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// CDockBar layout

CSize CDockBar::CalcFixedLayout(BOOL bStretch, BOOL bHorz)
{
	ASSERT_VALID(this);

	CSize sizeFixed = CControlBar::CalcFixedLayout(bStretch, bHorz);

	// get max size
	CSize sizeMax;
	if (!m_rectLayout.IsRectEmpty())
		sizeMax = m_rectLayout.Size();
	else
	{
		CRect rectFrame;
		CFrameWnd* pFrame = GetParentFrame();
		pFrame->GetClientRect(&rectFrame);
		sizeMax = rectFrame.Size();
	}

	// prepare for layout
	AFX_SIZEPARENTPARAMS layout;
	layout.hDWP = m_bLayoutQuery ?
		NULL : ::BeginDeferWindowPos(m_arrBars.GetSize());
	CPoint pt(-afxData.cxBorder2, -afxData.cyBorder2);
	int nWidth = 0;

	BOOL bWrapped = FALSE;

	// layout all the control bars
	for (int nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{
		CControlBar* pBar = GetDockedControlBar(nPos);
		void* pVoid = m_arrBars[nPos];

		if (pBar != NULL)
		{
			if (pBar->IsVisible())
			{
				// get ideal rect for bar
				DWORD dwMode = 0;
				if ((pBar->m_dwStyle & CBRS_SIZE_DYNAMIC) &&
					(pBar->m_dwStyle & CBRS_FLOATING))
					dwMode |= LM_HORZ | LM_MRUWIDTH;
				else if (pBar->m_dwStyle & CBRS_ORIENT_HORZ)
					dwMode |= LM_HORZ | LM_HORZDOCK;
				else
					dwMode |=  LM_VERTDOCK;

				CSize sizeBar = pBar->CalcDynamicLayout(-1, dwMode);

				CRect rect(pt, sizeBar);

				// get current rect for bar
				CRect rectBar;
				pBar->GetWindowRect(&rectBar);
				ScreenToClient(&rectBar);

				if (bHorz)
				{
					// Offset Calculated Rect out to Actual
					if (rectBar.left > rect.left && !m_bFloating)
						rect.OffsetRect(rectBar.left - rect.left, 0);

					// If ControlBar goes off the right, then right justify
					if (rect.right > sizeMax.cx && !m_bFloating)
					{
						int x = rect.Width() - afxData.cxBorder2;
						x = max(sizeMax.cx - x, pt.x);
						rect.OffsetRect(x - rect.left, 0);
					}

					// If ControlBar has been wrapped, then left justify
					if (bWrapped)
					{
						bWrapped = FALSE;
						rect.OffsetRect(-(rect.left + afxData.cxBorder2), 0);
					}
					// If ControlBar is completely invisible, then wrap it
					else if ((rect.left >= (sizeMax.cx - afxData.cxBorder2)) &&
						(nPos > 0) && (m_arrBars[nPos - 1] != NULL))
					{
						m_arrBars.InsertAt(nPos, (CObject*)NULL);
						pBar = NULL; pVoid = NULL;
						bWrapped = TRUE;
					}
					if (!bWrapped)
					{
						if (rect != rectBar)
						{
							if (!m_bLayoutQuery &&
								!(pBar->m_dwStyle & CBRS_FLOATING))
							{
								pBar->m_pDockContext->m_rectMRUDockPos = rect;
							}
							AfxRepositionWindow(&layout, pBar->m_hWnd, &rect);
						}
						pt.x = rect.left + sizeBar.cx - afxData.cxBorder2;
						nWidth = max(nWidth, sizeBar.cy);
					}
				}
				else
				{
					// Offset Calculated Rect out to Actual
					if (rectBar.top > rect.top && !m_bFloating)
						rect.OffsetRect(0, rectBar.top - rect.top);

					// If ControlBar goes off the bottom, then bottom justify
					if (rect.bottom > sizeMax.cy && !m_bFloating)
					{
						int y = rect.Height() - afxData.cyBorder2;
						y = max(sizeMax.cy - y, pt.y);
						rect.OffsetRect(0, y - rect.top);
					}

					// If ControlBar has been wrapped, then top justify
					if (bWrapped)
					{
						bWrapped = FALSE;
						rect.OffsetRect(0, -(rect.top + afxData.cyBorder2));
					}
					// If ControlBar is completely invisible, then wrap it
					else if ((rect.top >= (sizeMax.cy - afxData.cyBorder2)) &&
						(nPos > 0) && (m_arrBars[nPos - 1] != NULL))
					{
						m_arrBars.InsertAt(nPos, (CObject*)NULL);
						pBar = NULL; pVoid = NULL;
						bWrapped = TRUE;
					}
					if (!bWrapped)
					{
						if (rect != rectBar)
						{
							if (!m_bLayoutQuery &&
								!(pBar->m_dwStyle & CBRS_FLOATING))
							{
								pBar->m_pDockContext->m_rectMRUDockPos = rect;
							}
							AfxRepositionWindow(&layout, pBar->m_hWnd, &rect);
						}
						pt.y = rect.top + sizeBar.cy - afxData.cyBorder2;
						nWidth = max(nWidth, sizeBar.cx);
					}
				}
			}
			if (!bWrapped)
			{
				// handle any delay/show hide for the bar
				pBar->RecalcDelayShow(&layout);
			}
		}
		if (pBar == NULL && pVoid == NULL && nWidth != 0)
		{
			// end of row because pBar == NULL
			if (bHorz)
			{
				pt.y += nWidth - afxData.cyBorder2;
				sizeFixed.cx = max(sizeFixed.cx, pt.x);
				sizeFixed.cy = max(sizeFixed.cy, pt.y);
				pt.x = -afxData.cxBorder2;
			}
			else
			{
				pt.x += nWidth - afxData.cxBorder2;
				sizeFixed.cx = max(sizeFixed.cx, pt.x);
				sizeFixed.cy = max(sizeFixed.cy, pt.y);
				pt.y = -afxData.cyBorder2;
			}
			nWidth = 0;
		}
	}
	if (!m_bLayoutQuery)
	{
		// move and resize all the windows at once!
		if (layout.hDWP == NULL || !::EndDeferWindowPos(layout.hDWP))
			TRACE0("Warning: DeferWindowPos failed - low system resources.\n");
	}

	// adjust size for borders on the dock bar itself
	CRect rect;
	rect.SetRectEmpty();
	CalcInsideRect(rect, bHorz);

	if ((!bStretch || !bHorz) && sizeFixed.cx != 0)
		sizeFixed.cx += -rect.right + rect.left;
	if ((!bStretch || bHorz) && sizeFixed.cy != 0)
		sizeFixed.cy += -rect.bottom + rect.top;

	return sizeFixed;
}

LRESULT CDockBar::OnSizeParent(WPARAM wParam, LPARAM lParam)
{
	AFX_SIZEPARENTPARAMS* lpLayout = (AFX_SIZEPARENTPARAMS*)lParam;

	// set m_bLayoutQuery to TRUE if lpLayout->hDWP == NULL
	BOOL bLayoutQuery = m_bLayoutQuery;
	CRect rectLayout = m_rectLayout;
	m_bLayoutQuery = (lpLayout->hDWP == NULL);
	m_rectLayout = lpLayout->rect;
	LRESULT lResult = CControlBar::OnSizeParent(wParam, lParam);
	// restore m_bLayoutQuery
	m_bLayoutQuery = bLayoutQuery;
	m_rectLayout = rectLayout;

	return lResult;
}

/////////////////////////////////////////////////////////////////////////////
// CDockBar message handlers

void CDockBar::OnNcCalcSize(BOOL /*bCalcValidRects*/, NCCALCSIZE_PARAMS* lpncsp)
{
	// calculate border space (will add to top/bottom, subtract from right/bottom)
	CRect rect;
	rect.SetRectEmpty();
	CalcInsideRect(rect, m_dwStyle & CBRS_ORIENT_HORZ);

	// adjust non-client area for border space
	lpncsp->rgrc[0].left += rect.left;
	lpncsp->rgrc[0].top += rect.top;
	lpncsp->rgrc[0].right += rect.right;
	lpncsp->rgrc[0].bottom += rect.bottom;
}

void CDockBar::OnNcPaint()
{
	EraseNonClient();
}

void CDockBar::DoPaint(CDC*)
{
	// border painting is done in non-client area
}

void CDockBar::OnPaint()
{
	// background is already filled in gray
	CPaintDC dc(this);
	if (IsVisible() && GetDockedVisibleCount() != 0)
		DoPaint(&dc);       // delegate to paint helper
}

void CDockBar::OnWindowPosChanging(LPWINDOWPOS lpWndPos)
{
	// not necessary to invalidate the borders
	DWORD dwStyle = m_dwStyle;
	m_dwStyle &= ~(CBRS_BORDER_ANY);
	CControlBar::OnWindowPosChanging(lpWndPos);
	m_dwStyle = dwStyle;
}

/////////////////////////////////////////////////////////////////////////////
// CDockBar utility/implementation

int CDockBar::FindBar(CControlBar* pBar, int nPosExclude)
{
	for (int nPos = 0; nPos< m_arrBars.GetSize(); nPos++)
	{
		if (nPos != nPosExclude && m_arrBars[nPos] == pBar)
			return nPos;
	}
	return -1;
}

void CDockBar::ShowAll(BOOL bShow)
{
	for (int nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{
		CControlBar* pBar = GetDockedControlBar(nPos);
		if (pBar != NULL)
		{
			CFrameWnd* pFrameWnd = pBar->GetDockingFrame();
			pFrameWnd->ShowControlBar(pBar, bShow, TRUE);
		}
	}
}

CControlBar* CDockBar::GetDockedControlBar(int nPos) const
{
	CControlBar* pResult = (CControlBar*)m_arrBars[nPos];
	if (HIWORD(pResult) == 0)
		return NULL;
	return pResult;
}

int CDockBar::Insert(CControlBar* pBarIns, CRect rect, CPoint ptMid)
{
	ASSERT_VALID(this);
	ASSERT(pBarIns != NULL);

	int nPos = 0;
	int nPosInsAfter = 0;
	int nWidth = 0;
	int nTotalWidth = 0;
	BOOL bHorz = m_dwStyle & CBRS_ORIENT_HORZ;

	for (nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{
		CControlBar* pBar = GetDockedControlBar(nPos);
		if (pBar != NULL && pBar->IsVisible())
		{
			CRect rectBar;
			pBar->GetWindowRect(&rectBar);
			ScreenToClient(&rectBar);
			nWidth = max(nWidth,
				bHorz ? rectBar.Size().cy : rectBar.Size().cx - 1);
			if (bHorz ? rect.left > rectBar.left : rect.top > rectBar.top)
				nPosInsAfter = nPos;
		}
		else // end of row because pBar == NULL
		{
			nTotalWidth += nWidth - afxData.cyBorder2;
			nWidth = 0;
			if ((bHorz ? ptMid.y : ptMid.x) < nTotalWidth)
			{
				if (nPos == 0) // first section
					m_arrBars.InsertAt(nPosInsAfter+1, (CObject*)NULL);
				m_arrBars.InsertAt(nPosInsAfter+1, pBarIns);
				return nPosInsAfter+1;
			}
			nPosInsAfter = nPos;
		}
	}

	// create a new row
	m_arrBars.InsertAt(nPosInsAfter+1, (CObject*)NULL);
	m_arrBars.InsertAt(nPosInsAfter+1, pBarIns);

	return nPosInsAfter+1;
}

void CDockBar::OnUpdateCmdUI(CFrameWnd* /*pTarget*/, BOOL /*bDisableIfNoHndler*/)
{
}

#ifdef _DEBUG
void CDockBar::AssertValid() const
{
	CControlBar::AssertValid();
	ASSERT(m_arrBars.GetSize() != 0);
	ASSERT(m_arrBars[0] == NULL);
	ASSERT(m_arrBars[m_arrBars.GetUpperBound()] == NULL);
}

void CDockBar::Dump(CDumpContext& dc) const
{
	CControlBar::Dump(dc);

	dc << "m_arrBars " << m_arrBars;
	dc << "\nm_bFloating " << m_bFloating;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
// CControlBar docking helpers

void CControlBar::EnableDocking(DWORD dwDockStyle)
{
	// must be CBRS_ALIGN_XXX or CBRS_FLOAT_MULTI only
	ASSERT((dwDockStyle & ~(CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI)) == 0);
	// CBRS_SIZE_DYNAMIC toolbar cannot have the CBRS_FLOAT_MULTI style
	ASSERT(((dwDockStyle & CBRS_FLOAT_MULTI) == 0) || ((m_dwStyle & CBRS_SIZE_DYNAMIC) == 0));

	m_dwDockStyle = dwDockStyle;
	if (m_pDockContext == NULL)
		m_pDockContext = new CDockContext(this);

	// permanently wire the bar's owner to its current parent
	if (m_hWndOwner == NULL)
		m_hWndOwner = ::GetParent(m_hWnd);
}

/////////////////////////////////////////////////////////////////////////////
// CMiniDockFrameWnd

BEGIN_MESSAGE_MAP(CMiniDockFrameWnd, CMiniFrameWnd)
	//{{AFX_MSG_MAP(CMiniDockFrameWnd)
	ON_WM_CLOSE()
	ON_WM_NCLBUTTONDOWN()
	ON_WM_NCLBUTTONDBLCLK()
	//}}AFX_MSG_MAP
	ON_WM_MOUSEACTIVATE()
END_MESSAGE_MAP()

int CMiniDockFrameWnd::OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message)
{
	if (nHitTest >= HTSIZEFIRST && nHitTest <= HTSIZELAST) // resizing
		return MA_NOACTIVATE;
	return CMiniFrameWnd::OnMouseActivate(pDesktopWnd, nHitTest, message);
}

CMiniDockFrameWnd::CMiniDockFrameWnd() : m_wndDockBar(TRUE)
{
	m_wndDockBar.m_bAutoDelete = FALSE;
}

BOOL CMiniDockFrameWnd::Create(CWnd* pParent, DWORD dwBarStyle)
{
	// set m_bInRecalcLayout to avoid flashing during creation
	// RecalcLayout will be called once something is docked
	m_bInRecalcLayout = TRUE;

	DWORD dwStyle = WS_POPUP|WS_CAPTION|WS_SYSMENU|MFS_MOVEFRAME|
		MFS_4THICKFRAME|MFS_SYNCACTIVE|MFS_BLOCKSYSMENU|
		FWS_SNAPTOBARS;

	if (dwBarStyle & CBRS_SIZE_DYNAMIC)
		dwStyle &= ~MFS_MOVEFRAME;

	DWORD dwExStyle = 0;
	if (!CMiniFrameWnd::CreateEx(dwExStyle,
		NULL, &afxChNil, dwStyle, rectDefault, pParent))
	{
		m_bInRecalcLayout = FALSE;
		return FALSE;
	}
	dwStyle = dwBarStyle & (CBRS_ALIGN_LEFT|CBRS_ALIGN_RIGHT) ?
		CBRS_ALIGN_LEFT : CBRS_ALIGN_TOP;
	dwStyle |= dwBarStyle & CBRS_FLOAT_MULTI;
	CMenu* pSysMenu = GetSystemMenu(FALSE);
	pSysMenu->DeleteMenu(SC_SIZE, MF_BYCOMMAND);
	pSysMenu->DeleteMenu(SC_MINIMIZE, MF_BYCOMMAND);
	pSysMenu->DeleteMenu(SC_MAXIMIZE, MF_BYCOMMAND);
	pSysMenu->DeleteMenu(SC_RESTORE, MF_BYCOMMAND);
	CString strHide;
	if (strHide.LoadString(AFX_IDS_HIDE))
	{
		pSysMenu->DeleteMenu(SC_CLOSE, MF_BYCOMMAND);
		pSysMenu->AppendMenu(MF_STRING|MF_ENABLED, SC_CLOSE, strHide);
	}

	// must initially create with parent frame as parent
	if (!m_wndDockBar.Create(pParent, WS_CHILD | WS_VISIBLE | dwStyle,
		AFX_IDW_DOCKBAR_FLOAT))
	{
		m_bInRecalcLayout = FALSE;
		return FALSE;
	}

	// set parent to CMiniDockFrameWnd
	m_wndDockBar.SetParent(this);
	m_bInRecalcLayout = FALSE;

	return TRUE;
}

void CMiniDockFrameWnd::RecalcLayout(BOOL bNotify)
{
	if (!m_bInRecalcLayout)
	{
		CMiniFrameWnd::RecalcLayout(bNotify);

		// syncronize window text of frame window with dockbar itself
		TCHAR szTitle[_MAX_PATH];
		m_wndDockBar.GetWindowText(szTitle, _countof(szTitle));
		AfxSetWindowText(m_hWnd, szTitle);
	}
}

void CMiniDockFrameWnd::OnClose()
{
	m_wndDockBar.ShowAll(FALSE);
}

void CMiniDockFrameWnd::OnNcLButtonDown(UINT nHitTest, CPoint point)
{
	if (nHitTest == HTCAPTION)
	{
		// special activation for floating toolbars
		ActivateTopParent();

		// initiate toolbar drag for non-CBRS_FLOAT_MULTI toolbars
		if ((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0)
		{
			int nPos = 1;
			CControlBar* pBar = NULL;
			while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
				pBar = m_wndDockBar.GetDockedControlBar(nPos++);

			ASSERT(pBar != NULL);
			ASSERT_KINDOF(CControlBar, pBar);
			ASSERT(pBar->m_pDockContext != NULL);
			pBar->m_pDockContext->StartDrag(point);
			return;
		}
	}
	else if (nHitTest >= HTSIZEFIRST && nHitTest <= HTSIZELAST)
	{
		// special activation for floating toolbars
		ActivateTopParent();

		int nPos = 1;
		CControlBar* pBar = NULL;
		while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
			pBar = m_wndDockBar.GetDockedControlBar(nPos++);

		ASSERT(pBar != NULL);
		ASSERT_KINDOF(CControlBar, pBar);
		ASSERT(pBar->m_pDockContext != NULL);

		// CBRS_SIZE_DYNAMIC toolbars cannot have the CBRS_FLOAT_MULTI style
		ASSERT((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0);
		pBar->m_pDockContext->StartResize(nHitTest, point);
		return;
	}
	CMiniFrameWnd::OnNcLButtonDown(nHitTest, point);
}

void CMiniDockFrameWnd::OnNcLButtonDblClk(UINT nHitTest, CPoint point)
{
	if (nHitTest == HTCAPTION)
	{
		// special activation for floating toolbars
		ActivateTopParent();

		// initiate toolbar toggle for non-CBRS_FLOAT_MULTI toolbars
		if ((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0)
		{
			int nPos = 1;
			CControlBar* pBar = NULL;
			while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
				pBar = m_wndDockBar.GetDockedControlBar(nPos++);

			ASSERT(pBar != NULL);
			ASSERT_KINDOF(CControlBar, pBar);
			ASSERT(pBar->m_pDockContext != NULL);
			pBar->m_pDockContext->ToggleDocking();
			return;
		}
	}
	CMiniFrameWnd::OnNcLButtonDblClk(nHitTest, point);
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDockBar, CControlBar)
IMPLEMENT_DYNCREATE(CMiniDockFrameWnd, CMiniFrameWnd)

/////////////////////////////////////////////////////////////////////////////
