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

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

// dwDockBarMap
const DWORD CFrameWnd::dwDockBarMap[4][2] =
{
	{ AFX_IDW_DOCKBAR_TOP,      CBRS_TOP    },
	{ AFX_IDW_DOCKBAR_BOTTOM,   CBRS_BOTTOM },
	{ AFX_IDW_DOCKBAR_LEFT,     CBRS_LEFT   },
	{ AFX_IDW_DOCKBAR_RIGHT,    CBRS_RIGHT  },
};

/////////////////////////////////////////////////////////////////////////////
// Dockable control bar helpers

CMiniDockFrameWnd* CFrameWnd::CreateFloatingFrame(DWORD dwStyle)
{
	CMiniDockFrameWnd* pFrame = NULL;
	ASSERT(m_pFloatingFrameClass != NULL);
	pFrame = (CMiniDockFrameWnd*)m_pFloatingFrameClass->CreateObject();
	if (pFrame == NULL)
		AfxThrowMemoryException();
	ASSERT_KINDOF(CMiniDockFrameWnd, pFrame);
	if (!pFrame->Create(this, dwStyle))
		AfxThrowResourceException();
	return pFrame;
}

// dock bars will be created in the order specified by dwDockBarMap
// this also controls which gets priority during layout
// this order can be changed by calling EnableDocking repetitively
// with the exact order of priority
void CFrameWnd::EnableDocking(DWORD dwDockStyle)
{
	// must be CBRS_ALIGN_XXX or CBRS_FLOAT_MULTI only
	ASSERT((dwDockStyle & ~(CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI)) == 0);

	m_pFloatingFrameClass = RUNTIME_CLASS(CMiniDockFrameWnd);
	for (int i = 0; i < 4; i++)
	{
		if (dwDockBarMap[i][1] & dwDockStyle & CBRS_ALIGN_ANY)
		{
			CDockBar* pDock = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
			if (pDock == NULL)
			{
				pDock = new CDockBar;
				if (!pDock->Create(this,
					WS_CLIPSIBLINGS|WS_CLIPCHILDREN|WS_CHILD|WS_VISIBLE |
						dwDockBarMap[i][1], dwDockBarMap[i][0]))
				{
					AfxThrowResourceException();
				}
			}
		}
	}
}

void CFrameWnd::DockControlBar(CControlBar* pBar, UINT nDockBarID, LPCRECT lpRect)
{
	CDockBar* pDockBar = (nDockBarID == 0) ? NULL :
		(CDockBar*)GetControlBar(nDockBarID);
	DockControlBar(pBar, pDockBar, lpRect);
}

void CFrameWnd::DockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
{
	ASSERT(pBar != NULL);
	// make sure CControlBar::EnableDocking has been called
	ASSERT(pBar->m_pDockContext != NULL);

	if (pDockBar == NULL)
	{
		for (int i = 0; i < 4; i++)
		{
			if ((dwDockBarMap[i][1] & CBRS_ALIGN_ANY) ==
				(pBar->m_dwStyle & CBRS_ALIGN_ANY))
			{
				pDockBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
				ASSERT(pDockBar != NULL);
				// assert fails when initial CBRS_ of bar does not
				// match available docking sites, as set by EnableDocking()
				break;
			}
		}
	}
	ASSERT(pDockBar != NULL);
	ASSERT(m_listControlBars.Find(pBar) != NULL);
	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not initially
	// this CFrameWnd when pBar's OnCreate was called
	// i.e. this control bar should have been created with a different parent initially

	pDockBar->DockControlBar(pBar, lpRect);
}

void CFrameWnd::ReDockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
{
	ASSERT(pBar != NULL);
	// make sure CControlBar::EnableDocking has been called
	ASSERT(pBar->m_pDockContext != NULL);

	if (pDockBar == NULL)
	{
		// Search for the place holder.

		// In case we don't find a place holder, find a bar with the correct alignment
		// and keep it in pPossibleBar.
		CDockBar* pPossibleBar = NULL;
		for (int i = 0; i < 4; i++)
		{
			CDockBar* pTempBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
			if (pTempBar != NULL)
			{
				// Is this the same bar we docked with before?
				if (pTempBar->FindBar((CControlBar*)_AfxGetDlgCtrlID(pBar->m_hWnd)) > 0)
				{
					pDockBar = pTempBar;
					break;
				}
			}

			if ((dwDockBarMap[i][1] & CBRS_ALIGN_ANY) ==
				(pBar->m_dwStyle & CBRS_ALIGN_ANY))
			{
				pPossibleBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
				ASSERT(pPossibleBar != NULL);
				// assert fails when initial CBRS_ of bar does not
				// match available docking sites, as set by EnableDocking()
			}
		}

		// Did we find the place holder?
		if (pDockBar == NULL)
			pDockBar = pPossibleBar;
	}
	ASSERT(pDockBar != NULL);
	ASSERT(m_listControlBars.Find(pBar) != NULL);
	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not initially
	// this CFrameWnd when pBar's OnCreate was called
	// i.e. this control bar should have been created with a different parent initially

	pDockBar->ReDockControlBar(pBar, lpRect);
}

void CFrameWnd::FloatControlBar(CControlBar* pBar, CPoint point, DWORD dwStyle)
{
	ASSERT(pBar != NULL);

	// if the bar is already floating and the dock bar only contains this
	// bar and same orientation then move the window rather than recreating
	// the frame
	if (pBar->m_pDockSite != NULL && pBar->m_pDockBar != NULL)
	{
		CDockBar* pDockBar = pBar->m_pDockBar;
		ASSERT_KINDOF(CDockBar, pDockBar);
		if (pDockBar->m_bFloating && pDockBar->GetDockedCount() == 1 &&
			(dwStyle & pDockBar->m_dwStyle & CBRS_ALIGN_ANY) != 0)
		{
			CMiniDockFrameWnd* pDockFrame =
				(CMiniDockFrameWnd*)pDockBar->GetParent();
			ASSERT(pDockFrame != NULL);
			ASSERT_KINDOF(CMiniDockFrameWnd, pDockFrame);
			pDockFrame->SetWindowPos(NULL, point.x, point.y, 0, 0,
				SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
			pDockFrame->RecalcLayout(TRUE);
			pDockFrame->UpdateWindow();
			return;
		}
	}

	if (pBar->m_dwStyle & CBRS_SIZE_DYNAMIC)
	{
		dwStyle |= CBRS_SIZE_DYNAMIC;
		if (dwStyle & CBRS_ORIENT_VERT)
		{
			dwStyle &= ~CBRS_ALIGN_ANY;
			dwStyle |= CBRS_ALIGN_TOP;
		}
	}

	CMiniDockFrameWnd* pDockFrame = CreateFloatingFrame(dwStyle);
	ASSERT(pDockFrame != NULL);
	pDockFrame->SetWindowPos(NULL, point.x, point.y, 0, 0,
		SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
	if (pDockFrame->m_hWndOwner == NULL)
		pDockFrame->m_hWndOwner = pBar->m_hWnd;

	CDockBar* pDockBar = (CDockBar*)pDockFrame->GetDlgItem(AFX_IDW_DOCKBAR_FLOAT);
	ASSERT(pDockBar != NULL);
	ASSERT_KINDOF(CDockBar, pDockBar);

	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not
	//  initially this CFrameWnd when pBar's OnCreate was called
	// (this control bar should have been created with a different
	//  parent initially)

	pDockBar->DockControlBar(pBar);
	pDockFrame->RecalcLayout(TRUE);
	if (GetWindowLong(pBar->m_hWnd, GWL_STYLE) & WS_VISIBLE)
	{
		pDockFrame->ShowWindow(SW_SHOWNA);
		pDockFrame->UpdateWindow();
	}
}

DWORD CFrameWnd::CanDock(CRect rect, DWORD dwDockStyle, CDockBar** ppDockBar)
{
	// dwDockStyle -- allowable styles of bar
	// don't allow to dock to floating unless multi is specified
	dwDockStyle &= CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI;

	if (ppDockBar != NULL)
		*ppDockBar = NULL;
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CDockBar* pDockBar = (CDockBar*)m_listControlBars.GetNext(pos);
		if (pDockBar->IsDockBar() && pDockBar->IsWindowVisible() &&
			(pDockBar->m_dwStyle & dwDockStyle & CBRS_ALIGN_ANY) &&
			(!pDockBar->m_bFloating ||
				(dwDockStyle & pDockBar->m_dwStyle & CBRS_FLOAT_MULTI)))
		{
			CRect rectBar;
			pDockBar->GetWindowRect(&rectBar);
			if (rectBar.Width() == 0)
				rectBar.right++;
			if (rectBar.Height() == 0)
				rectBar.bottom++;
			if (rectBar.IntersectRect(rectBar, rect))
			{
				if (ppDockBar != NULL)
					*ppDockBar = pDockBar;
				return pDockBar->m_dwStyle & dwDockStyle;
			}
		}
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
