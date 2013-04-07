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

/////////////////////////////////////////////////////////////////////////////
// Visual attributes and other constants

// HitTest return values (values and spacing between values is important)
enum HitTestValue
{
	noHit                   = 0,
	vSplitterBox            = 1,
	hSplitterBox            = 2,
	bothSplitterBox         = 3,        // just for keyboard
	vSplitterBar1           = 101,
	vSplitterBar15          = 115,
	hSplitterBar1           = 201,
	hSplitterBar15          = 215,
	splitterIntersection1   = 301,
	splitterIntersection225 = 525
};

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd

BEGIN_MESSAGE_MAP(CSplitterWnd, CWnd)
	//{{AFX_MSG_MAP(CSplitterWnd)
	ON_WM_SETCURSOR()
	ON_WM_MOUSEMOVE()
	ON_WM_PAINT()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_LBUTTONUP()
	ON_WM_KEYDOWN()
	ON_WM_SIZE()
	ON_WM_HSCROLL()
	ON_WM_VSCROLL()
	ON_WM_NCCREATE()
	ON_WM_SYSCOMMAND()
	ON_WM_CANCELMODE()
	ON_MESSAGE_VOID(WM_DISPLAYCHANGE, OnDisplayChange)
	ON_MESSAGE_VOID(WM_WININICHANGE, OnDisplayChange)
	ON_MESSAGE_VOID(WM_SETTINGCHANGE, OnDisplayChange)
	ON_WM_MOUSEWHEEL()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd construction/destruction

CSplitterWnd::CSplitterWnd()
{
	AFX_ZERO_INIT_OBJECT(CWnd);

	// default splitter box/bar sizes (includes borders)
	if (!afxData.bWin4)
	{
		m_cxSplitter = m_cySplitter = 4;
		m_cxBorderShare = m_cyBorderShare = 1;
		m_cxSplitterGap = m_cySplitterGap = 4 + 1 + 1;
		ASSERT(m_cxBorder == 0 && m_cyBorder == 0);
	}
	else
	{
		m_cxSplitter = m_cySplitter = 3 + 2 + 2;
		m_cxBorderShare = m_cyBorderShare = 0;
		m_cxSplitterGap = m_cySplitterGap = 3 + 2 + 2;
		m_cxBorder = m_cyBorder = 2;
	}

#ifdef _DEBUG
	if (GetSystemMetrics(SM_CXBORDER) != 1 ||
		GetSystemMetrics(SM_CYBORDER) != 1)
	{
		TRACE0("Warning: CSplitterWnd assumes 1 pixel border.\n");
		// will look ugly if borders are not 1 pixel wide and 1 pixel high
	}
#endif
}

CSplitterWnd::~CSplitterWnd()
{
	delete[] m_pRowInfo;
	delete[] m_pColInfo;
}

BOOL CSplitterWnd::Create(CWnd* pParentWnd,
	int nMaxRows, int nMaxCols, SIZE sizeMin,
	CCreateContext* pContext, DWORD dwStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(sizeMin.cx > 0 && sizeMin.cy > 0);   // minimum must be non-zero

	ASSERT(pContext != NULL);
	ASSERT(pContext->m_pNewViewClass != NULL);
	ASSERT(dwStyle & WS_CHILD);
	ASSERT(dwStyle & SPLS_DYNAMIC_SPLIT);   // must have dynamic split behavior

	// Dynamic splitters are limited to 2x2
	ASSERT(nMaxRows >= 1 && nMaxRows <= 2);
	ASSERT(nMaxCols >= 1 && nMaxCols <= 2);
	ASSERT(nMaxCols > 1 || nMaxRows > 1);       // 1x1 is not permitted

	m_nMaxRows = nMaxRows;
	m_nMaxCols = nMaxCols;
	ASSERT(m_nRows == 0 && m_nCols == 0);       // none yet
	m_nRows = m_nCols = 1;      // start off as 1x1
	if (!CreateCommon(pParentWnd, sizeMin, dwStyle, nID))
		return FALSE;
	ASSERT(m_nRows == 1 && m_nCols == 1);       // still 1x1

	ASSERT(pContext->m_pNewViewClass->IsDerivedFrom(RUNTIME_CLASS(CWnd)));
	m_pDynamicViewClass = pContext->m_pNewViewClass;
		// save for later dynamic creations

	// add the first initial pane
	if (!CreateView(0, 0, m_pDynamicViewClass, sizeMin, pContext))
	{
		DestroyWindow(); // will clean up child windows
		return FALSE;
	}
	m_pColInfo[0].nIdealSize = sizeMin.cx;
	m_pRowInfo[0].nIdealSize = sizeMin.cy;

	return TRUE;
}

// simple "wiper" splitter
BOOL CSplitterWnd::CreateStatic(CWnd* pParentWnd,
	int nRows, int nCols, DWORD dwStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(nRows >= 1 && nRows <= 16);
	ASSERT(nCols >= 1 && nCols <= 16);
	ASSERT(nCols > 1 || nRows > 1);     // 1x1 is not permitted
	ASSERT(dwStyle & WS_CHILD);
	ASSERT(!(dwStyle & SPLS_DYNAMIC_SPLIT)); // can't have dynamic split

	ASSERT(m_nRows == 0 && m_nCols == 0);       // none yet
	m_nRows = m_nMaxRows = nRows;
	m_nCols = m_nMaxCols = nCols;

	// create with zero minimum pane size
	if (!CreateCommon(pParentWnd, CSize(0, 0), dwStyle, nID))
		return FALSE;

	// all panes must be created with explicit calls to CreateView
	return TRUE;
}

BOOL CSplitterWnd::CreateCommon(CWnd* pParentWnd,
	SIZE sizeMin, DWORD dwStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(sizeMin.cx >= 0 && sizeMin.cy >= 0);
	ASSERT(dwStyle & WS_CHILD);
	ASSERT(nID != 0);

	ASSERT(m_pColInfo == NULL && m_pRowInfo == NULL);   // only do once
	ASSERT(m_nMaxCols > 0 && m_nMaxRows > 0);

	// the Windows scroll bar styles bits turn on the smart scrollbars
	DWORD dwCreateStyle = dwStyle & ~(WS_HSCROLL|WS_VSCROLL);
	if (afxData.bWin4)
		dwCreateStyle &= ~WS_BORDER;

	VERIFY(AfxDeferRegisterClass(AFX_WNDMDIFRAME_REG));

	// create with the same wnd-class as MDI-Frame (no erase bkgnd)
	if (!CreateEx(0, _afxWndMDIFrame, NULL, dwCreateStyle, 0, 0, 0, 0,
	  pParentWnd->m_hWnd, (HMENU)nID, NULL))
		return FALSE;       // create invisible

	// attach the initial splitter parts
	TRY
	{
		m_pColInfo = new CRowColInfo[m_nMaxCols];
		for (int col = 0; col < m_nMaxCols; col++)
		{
			m_pColInfo[col].nMinSize = m_pColInfo[col].nIdealSize = sizeMin.cx;
			m_pColInfo[col].nCurSize = -1; // will be set in RecalcLayout
		}
		m_pRowInfo = new CRowColInfo[m_nMaxRows];
		for (int row = 0; row < m_nMaxRows; row++)
		{
			m_pRowInfo[row].nMinSize = m_pRowInfo[row].nIdealSize = sizeMin.cy;
			m_pRowInfo[row].nCurSize = -1; // will be set in RecalcLayout
		}

		// create scroll bars by setting the style
		SetScrollStyle(dwStyle);
	}
	CATCH_ALL(e)
	{
		DestroyWindow(); // will clean up child windows
		// Note: DELETE_EXCEPTION(e) not required
		return FALSE;
	}
	END_CATCH_ALL

	return TRUE;
}

BOOL CSplitterWnd::OnNcCreate(LPCREATESTRUCT lpcs)
{
	if (!CWnd::OnNcCreate(lpcs))
		return FALSE;

	// remove WS_EX_CLIENTEDGE style from parent window
	//  (the splitter itself will provide the 3d look)
	CWnd* pParent = GetParent();
	ASSERT_VALID(pParent);
	pParent->ModifyStyleEx(WS_EX_CLIENTEDGE, 0, SWP_DRAWFRAME);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd default creation of parts

// You must create ALL panes unless DYNAMIC_SPLIT is defined!
//  Usually the splitter window is invisible when creating a pane
BOOL CSplitterWnd::CreateView(int row, int col,
	CRuntimeClass* pViewClass, SIZE sizeInit, CCreateContext* pContext)
{
#ifdef _DEBUG
	ASSERT_VALID(this);
	ASSERT(row >= 0 && row < m_nRows);
	ASSERT(col >= 0 && col < m_nCols);
	ASSERT(pViewClass != NULL);
	ASSERT(pViewClass->IsDerivedFrom(RUNTIME_CLASS(CWnd)));
	ASSERT(AfxIsValidAddress(pViewClass, sizeof(CRuntimeClass), FALSE));

	if (GetDlgItem(IdFromRowCol(row, col)) != NULL)
	{
		TRACE2("Error: CreateView - pane already exists for row %d, col %d.\n",
			row, col);
		ASSERT(FALSE);
		return FALSE;
	}
#endif

	// set the initial size for that pane
	m_pColInfo[col].nIdealSize = sizeInit.cx;
	m_pRowInfo[row].nIdealSize = sizeInit.cy;

	BOOL bSendInitialUpdate = FALSE;

	CCreateContext contextT;
	if (pContext == NULL)
	{
		// if no context specified, generate one from the currently selected
		//  client if possible
		CView* pOldView = (CView*)GetActivePane();
		if (pOldView != NULL && pOldView->IsKindOf(RUNTIME_CLASS(CView)))
		{
			// set info about last pane
			ASSERT(contextT.m_pCurrentFrame == NULL);
			contextT.m_pLastView = pOldView;
			contextT.m_pCurrentDoc = pOldView->GetDocument();
			if (contextT.m_pCurrentDoc != NULL)
				contextT.m_pNewDocTemplate =
				  contextT.m_pCurrentDoc->GetDocTemplate();
		}
		pContext = &contextT;
		bSendInitialUpdate = TRUE;
	}

	CWnd* pWnd;
	TRY
	{
		pWnd = (CWnd*)pViewClass->CreateObject();
		if (pWnd == NULL)
			AfxThrowMemoryException();
	}
	CATCH_ALL(e)
	{
		TRACE0("Out of memory creating a splitter pane.\n");
		// Note: DELETE_EXCEPTION(e) not required
		return FALSE;
	}
	END_CATCH_ALL

	ASSERT_KINDOF(CWnd, pWnd);
	ASSERT(pWnd->m_hWnd == NULL);       // not yet created

	DWORD dwStyle = AFX_WS_DEFAULT_VIEW;
	if (afxData.bWin4)
		dwStyle &= ~WS_BORDER;

	// Create with the right size (wrong position)
	CRect rect(CPoint(0,0), sizeInit);
	if (!pWnd->Create(NULL, NULL, dwStyle,
		rect, this, IdFromRowCol(row, col), pContext))
	{
		TRACE0("Warning: couldn't create client pane for splitter.\n");
			// pWnd will be cleaned up by PostNcDestroy
		return FALSE;
	}
	ASSERT((int)_AfxGetDlgCtrlID(pWnd->m_hWnd) == IdFromRowCol(row, col));

	// send initial notification message
	if (bSendInitialUpdate)
		pWnd->SendMessage(WM_INITIALUPDATE);

	return TRUE;
}

BOOL CSplitterWnd::CreateScrollBarCtrl(DWORD dwStyle, UINT nID)
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);

	HWND hWnd = ::CreateWindow(_T("SCROLLBAR"), NULL,
		dwStyle | WS_VISIBLE | WS_CHILD,
		0, 0, 1, 1, m_hWnd, (HMENU)nID,
		AfxGetInstanceHandle(), NULL);

#ifdef _DEBUG
	if (hWnd == NULL)
		TRACE1("Warning: Window creation failed: GetLastError returns 0x%8.8X\n",
			GetLastError());
#endif

	return hWnd != NULL;
}

int CSplitterWnd::IdFromRowCol(int row, int col) const
{
	ASSERT_VALID(this);
	ASSERT(row >= 0);
	ASSERT(row < m_nRows);
	ASSERT(col >= 0);
	ASSERT(col < m_nCols);

	return AFX_IDW_PANE_FIRST + row * 16 + col;
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd attributes

CWnd* CSplitterWnd::GetPane(int row, int col) const
{
	ASSERT_VALID(this);

	CWnd* pView = GetDlgItem(IdFromRowCol(row, col));
	ASSERT(pView != NULL);  // panes can be a CWnd, but are usually CViews
	return pView;
}

BOOL CSplitterWnd::IsChildPane(CWnd* pWnd, int* pRow, int* pCol)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pWnd);

	UINT nID = _AfxGetDlgCtrlID(pWnd->m_hWnd);
	if (IsChild(pWnd) && nID >= AFX_IDW_PANE_FIRST && nID <= AFX_IDW_PANE_LAST)
	{
		if (pRow != NULL)
			*pRow = (nID - AFX_IDW_PANE_FIRST) / 16;
		if (pCol != NULL)
			*pCol = (nID - AFX_IDW_PANE_FIRST) % 16;
		ASSERT(pRow == NULL || *pRow < m_nRows);
		ASSERT(pCol == NULL || *pCol < m_nCols);
		return TRUE;
	}
	else
	{
		if (pRow != NULL)
			*pRow = -1;
		if (pCol != NULL)
			*pCol = -1;
		return FALSE;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd information access

// The get routines return the current size
// The set routines set the ideal size
//  RecalcLayout must be called to update current size

void CSplitterWnd::GetRowInfo(int row, int& cyCur, int& cyMin) const
{
	ASSERT_VALID(this);
	ASSERT(row >= 0 && row < m_nMaxRows);

	cyCur = m_pRowInfo[row].nCurSize;
	cyMin = m_pRowInfo[row].nMinSize;
}

void CSplitterWnd::SetRowInfo(int row, int cyIdeal, int cyMin)
{
	ASSERT_VALID(this);
	ASSERT(row >= 0 && row < m_nMaxRows);
	ASSERT(cyIdeal >= 0);
	ASSERT(cyMin >= 0);

	m_pRowInfo[row].nIdealSize = cyIdeal;
	m_pRowInfo[row].nMinSize = cyMin;
}

void CSplitterWnd::GetColumnInfo(int col, int& cxCur, int& cxMin) const
{
	ASSERT_VALID(this);
	ASSERT(col >= 0 && col < m_nMaxCols);

	cxCur = m_pColInfo[col].nCurSize;
	cxMin = m_pColInfo[col].nMinSize;
}

void CSplitterWnd::SetColumnInfo(int col, int cxIdeal, int cxMin)
{
	ASSERT_VALID(this);
	ASSERT(col >= 0 && col < m_nMaxCols);
	ASSERT(cxIdeal >= 0);
	ASSERT(cxMin >= 0);

	m_pColInfo[col].nIdealSize = cxIdeal;
	m_pColInfo[col].nMinSize = cxMin;
}

DWORD CSplitterWnd::GetScrollStyle() const
{
	DWORD dwStyle = 0;
	if (m_bHasHScroll)
		dwStyle |= WS_HSCROLL;
	if (m_bHasVScroll)
		dwStyle |= WS_VSCROLL;
	return dwStyle;
}

void CSplitterWnd::SetScrollStyle(DWORD dwStyle)
{
	// optimize for scroll info already set correctly
	dwStyle &= (WS_HSCROLL|WS_VSCROLL);
	if (GetScrollStyle() == dwStyle)
		return;

	// update to new state
	m_bHasHScroll = (dwStyle & WS_HSCROLL) != 0;
	m_bHasVScroll = (dwStyle & WS_VSCROLL) != 0;

	CWnd* pScrollBar;

	// show/hide all the shared horz scroll bars
	for (int col = 0; col < m_nCols; col++)
	{
		pScrollBar = GetDlgItem(AFX_IDW_HSCROLL_FIRST + col);
		if (pScrollBar == NULL)
		{
			// create the scroll bar when necessary
			if (!CreateScrollBarCtrl(SBS_HORZ, AFX_IDW_HSCROLL_FIRST + col))
				AfxThrowResourceException();
			pScrollBar = GetDlgItem(AFX_IDW_HSCROLL_FIRST + col);
		}
		pScrollBar->ShowWindow(m_bHasHScroll ? SW_SHOW : SW_HIDE);
	}

	// show/hide all the shared vert scroll bars
	for (int row = 0; row < m_nRows; row++)
	{
		pScrollBar = GetDlgItem(AFX_IDW_VSCROLL_FIRST + row);
		if (pScrollBar == NULL)
		{
			// create the scroll bar when necessary
			if (!CreateScrollBarCtrl(SBS_VERT, AFX_IDW_VSCROLL_FIRST + row))
				AfxThrowResourceException();
			pScrollBar = GetDlgItem(AFX_IDW_VSCROLL_FIRST + row);
		}
		pScrollBar->ShowWindow(m_bHasVScroll ? SW_SHOW : SW_HIDE);
	}

	// show/destroy size box if necessary
	if (m_bHasVScroll && m_bHasHScroll)
	{
		pScrollBar = GetDlgItem(AFX_IDW_SIZE_BOX);
		if (pScrollBar == NULL)
		{
			// create size box when necessary
			if (!CreateScrollBarCtrl(SBS_SIZEBOX|WS_DISABLED, AFX_IDW_SIZE_BOX))
				AfxThrowResourceException();
			pScrollBar = GetDlgItem(AFX_IDW_SIZE_BOX);
		}
		pScrollBar->ShowWindow(SW_SHOW);
	}
	else
	{
		// the size box can be destroyed instead of hidden
		pScrollBar = GetDlgItem(AFX_IDW_SIZE_BOX);
		if (pScrollBar != NULL)
			pScrollBar->DestroyWindow();
	}

	// Note: call RecalcLayout for the new layout to take effect
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd client operations/overridables

void CSplitterWnd::DeleteView(int row, int col)
{
	ASSERT_VALID(this);

	// if active child is being deleted - activate next
	CWnd* pPane = GetPane(row, col);
	ASSERT_KINDOF(CView, pPane);
	if (GetActivePane() == pPane)
		ActivateNext(FALSE);

	// default implementation assumes view will auto delete in PostNcDestroy
	pPane->DestroyWindow();
}

void CSplitterWnd::OnDrawSplitter(CDC* pDC, ESplitType nType,
	const CRect& rectArg)
{
	// if pDC == NULL, then just invalidate
	if (pDC == NULL)
	{
		RedrawWindow(rectArg, NULL, RDW_INVALIDATE|RDW_NOCHILDREN);
		return;
	}
	ASSERT_VALID(pDC);

	// otherwise, actually draw
	CRect rect = rectArg;
	switch (nType)
	{
	case splitBorder:
		ASSERT(afxData.bWin4);
		pDC->Draw3dRect(rect, afxData.clrBtnShadow, afxData.clrBtnHilite);
		rect.InflateRect(-CX_BORDER, -CY_BORDER);
		pDC->Draw3dRect(rect, afxData.clrWindowFrame, afxData.clrBtnFace);
		return;

	case splitIntersection:
		ASSERT(!afxData.bWin4);
		break;

	case splitBox:
		if (afxData.bWin4)
		{
			pDC->Draw3dRect(rect, afxData.clrBtnFace, afxData.clrWindowFrame);
			rect.InflateRect(-CX_BORDER, -CY_BORDER);
			pDC->Draw3dRect(rect, afxData.clrBtnHilite, afxData.clrBtnShadow);
			rect.InflateRect(-CX_BORDER, -CY_BORDER);
			break;
		}
		// fall through...
	case splitBar:
		if (!afxData.bWin4)
		{
			pDC->Draw3dRect(rect, afxData.clrBtnHilite, afxData.clrBtnShadow);
			rect.InflateRect(-CX_BORDER, -CY_BORDER);
		}
		break;

	default:
		ASSERT(FALSE);  // unknown splitter type
	}

	// fill the middle
	COLORREF clr = afxData.clrBtnFace;
	pDC->FillSolidRect(rect, clr);
}

/////////////////////////////////////////////////////////////////////////////
// Dynamic row/col split etc

AFX_STATIC int AFXAPI _AfxCanSplitRowCol(CSplitterWnd::CRowColInfo* pInfoBefore,
	int nBeforeSize, int nSizeSplitter)
	// request to split Before row at point nBeforeSize
	// returns size of new pane (nBeforeSize will be new size of Before pane)
	// return -1 if not big enough
{
	ASSERT(pInfoBefore->nCurSize > 0);
	ASSERT(pInfoBefore->nMinSize > 0);
	ASSERT(nBeforeSize <= pInfoBefore->nCurSize);

	// space gets take from before pane (weird UI for > 2 splits)
	if (nBeforeSize < pInfoBefore->nMinSize)
	{
		TRACE0("Warning: split too small to fit in a new pane.\n");
		return -1;
	}

	int nNewSize = pInfoBefore->nCurSize - nBeforeSize - nSizeSplitter;
	if (nBeforeSize < pInfoBefore->nMinSize)
	{
		TRACE0("Warning: split too small to shrink old pane.\n");
		return -1;
	}
	if (nNewSize < (pInfoBefore+1)->nMinSize)
	{
		TRACE0("Warning: split too small to create new pane.\n");
		return -1;
	}
	return nNewSize;
}

BOOL CSplitterWnd::SplitRow(int cyBefore)
{
	ASSERT_VALID(this);
	ASSERT(GetStyle() & SPLS_DYNAMIC_SPLIT);
	ASSERT(m_pDynamicViewClass != NULL);
	ASSERT(m_nRows < m_nMaxRows);

	cyBefore -= m_cyBorder;
	int rowNew = m_nRows;
	int cyNew = _AfxCanSplitRowCol(&m_pRowInfo[rowNew-1], cyBefore, m_cySplitter);
	if (cyNew == -1)
		return FALSE;   // too small to split

	// create the scroll bar first (so new views can see that it is there)
	if (m_bHasVScroll &&
		!CreateScrollBarCtrl(SBS_VERT, AFX_IDW_VSCROLL_FIRST + rowNew))
	{
		TRACE0("Warning: SplitRow failed to create scroll bar.\n");
		return FALSE;
	}

	m_nRows++;  // bump count during view creation

	// create new views to fill the new row (RecalcLayout will position)
	for (int col = 0; col < m_nCols; col++)
	{
		CSize size(m_pColInfo[col].nCurSize, cyNew);
		if (!CreateView(rowNew, col, m_pDynamicViewClass, size, NULL))
		{
			TRACE0("Warning: SplitRow failed to create new row.\n");
			// delete anything we partially created 'col' = # columns created
			while (col > 0)
				DeleteView(rowNew, --col);
			if (m_bHasVScroll)
				GetDlgItem(AFX_IDW_VSCROLL_FIRST + rowNew)->DestroyWindow();
			m_nRows--;      // it didn't work out
			return FALSE;
		}
	}

	// new parts created - resize and re-layout
	m_pRowInfo[rowNew-1].nIdealSize = cyBefore;
	m_pRowInfo[rowNew].nIdealSize = cyNew;
	ASSERT(m_nRows == rowNew+1);
	RecalcLayout();

	return TRUE;
}

BOOL CSplitterWnd::SplitColumn(int cxBefore)
{
	ASSERT_VALID(this);
	ASSERT(GetStyle() & SPLS_DYNAMIC_SPLIT);
	ASSERT(m_pDynamicViewClass != NULL);
	ASSERT(m_nCols < m_nMaxCols);

	cxBefore -= m_cxBorder;
	int colNew = m_nCols;
	int cxNew = _AfxCanSplitRowCol(&m_pColInfo[colNew-1], cxBefore, m_cxSplitter);
	if (cxNew == -1)
		return FALSE;   // too small to split

	// create the scroll bar first (so new views can see that it is there)
	if (m_bHasHScroll &&
		!CreateScrollBarCtrl(SBS_HORZ, AFX_IDW_HSCROLL_FIRST + colNew))
	{
		TRACE0("Warning: SplitRow failed to create scroll bar.\n");
		return FALSE;
	}

	m_nCols++;  // bump count during view creation

	// create new views to fill the new column (RecalcLayout will position)
	for (int row = 0; row < m_nRows; row++)
	{
		CSize size(cxNew, m_pRowInfo[row].nCurSize);
		if (!CreateView(row, colNew, m_pDynamicViewClass, size, NULL))
		{
			TRACE0("Warning: SplitColumn failed to create new column.\n");
			// delete anything we partially created 'col' = # columns created
			while (row > 0)
				DeleteView(--row, colNew);
			if (m_bHasHScroll)
				GetDlgItem(AFX_IDW_HSCROLL_FIRST + colNew)->DestroyWindow();
			m_nCols--;      // it didn't work out
			return FALSE;
		}
	}

	// new parts created - resize and re-layout
	m_pColInfo[colNew-1].nIdealSize = cxBefore;
	m_pColInfo[colNew].nIdealSize = cxNew;
	ASSERT(m_nCols == colNew+1);
	RecalcLayout();

	return TRUE;
}

void CSplitterWnd::DeleteRow(int rowDelete)
{
	ASSERT_VALID(this);
	ASSERT(GetStyle() & SPLS_DYNAMIC_SPLIT);

	ASSERT(m_nRows > 1);
	ASSERT(rowDelete < m_nRows);

	int rowActive, colActive;
	if (GetActivePane(&rowActive, &colActive) != NULL && rowActive == rowDelete)
	{
		if (++rowActive >= m_nRows)
			rowActive = 0;
		SetActivePane(rowActive, colActive);
	}

	CWnd* pScrollDel = m_bHasVScroll ?
		GetDlgItem(AFX_IDW_VSCROLL_FIRST+rowDelete) : NULL;
	for (int col = 0; col < m_nCols; col++)
	{
		DeleteView(rowDelete, col);
		for (int row = rowDelete+1; row < m_nRows; row++)
		{
			CWnd* pPane = GetPane(row, col);
			ASSERT(pPane != NULL);
			pPane->SetDlgCtrlID(IdFromRowCol(row-1, col));
			if (m_bHasVScroll && col == m_nCols-1)
			{
				CWnd* pScroll = GetDlgItem(AFX_IDW_VSCROLL_FIRST+row);
				if (pScroll != NULL)
					pScroll->SetDlgCtrlID(AFX_IDW_VSCROLL_FIRST+row-1);
			}
		}
	}
	m_nRows--;
	if (pScrollDel != NULL)
		pScrollDel->DestroyWindow();

	RecalcLayout();     // re-assign the space
}

void CSplitterWnd::DeleteColumn(int colDelete)
{
	ASSERT_VALID(this);
	ASSERT(GetStyle() & SPLS_DYNAMIC_SPLIT);

	ASSERT(m_nCols > 1);
	ASSERT(colDelete < m_nCols);

	int rowActive, colActive;
	if (GetActivePane(&rowActive, &colActive) != NULL && colActive == colDelete)
	{
		if (++colActive >= m_nCols)
			colActive = 0;
		SetActivePane(rowActive, colActive);
	}

	CWnd* pScrollDel = m_bHasHScroll ?
		GetDlgItem(AFX_IDW_HSCROLL_FIRST+colDelete) : NULL;
	for (int row = 0; row < m_nRows; row++)
	{
		DeleteView(row, colDelete);
		for (int col = colDelete+1; col < m_nCols; col++)
		{
			CWnd* pPane = GetPane(row, col);
			ASSERT(pPane != NULL);
			pPane->SetDlgCtrlID(IdFromRowCol(row, col-1));
			if (m_bHasHScroll && row == m_nRows-1)
			{
				CWnd* pScroll = GetDlgItem(AFX_IDW_HSCROLL_FIRST+col);
				if (pScroll != NULL)
					pScroll->SetDlgCtrlID(AFX_IDW_HSCROLL_FIRST+col-1);
			}
		}
	}
	m_nCols--;
	if (pScrollDel != NULL)
		pScrollDel->DestroyWindow();

	RecalcLayout();     // re-assign the space
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd tracking support

// like GetClientRect but inset by shared scrollbars
void CSplitterWnd::GetInsideRect(CRect& rect) const
{
	ASSERT_VALID(this);

	GetClientRect(rect);
	ASSERT(rect.left == 0 && rect.top == 0);

	// subtract space for 3d borders
	rect.InflateRect(-m_cxBorder, -m_cyBorder);

	// subtract scrollbar clearance
	if (m_bHasVScroll)
		rect.right -= afxData.cxVScroll - CX_BORDER;
	if (m_bHasHScroll)
		rect.bottom -= afxData.cyHScroll - CY_BORDER;
}

void CSplitterWnd::StartTracking(int ht)
{
	ASSERT_VALID(this);
	if (ht == noHit)
		return;

	// GetHitRect will restrict 'm_rectLimit' as appropriate
	GetInsideRect(m_rectLimit);

	if (ht >= splitterIntersection1 && ht <= splitterIntersection225)
	{
		// split two directions (two tracking rectangles)
		int row = (ht - splitterIntersection1) / 15;
		int col = (ht - splitterIntersection1) % 15;

		GetHitRect(row + vSplitterBar1, m_rectTracker);
		int yTrackOffset = m_ptTrackOffset.y;
		m_bTracking2 = TRUE;
		GetHitRect(col + hSplitterBar1, m_rectTracker2);
		m_ptTrackOffset.y = yTrackOffset;
	}
	else if (ht == bothSplitterBox)
	{
		// hit on splitter boxes (for keyboard)
		GetHitRect(vSplitterBox, m_rectTracker);
		int yTrackOffset = m_ptTrackOffset.y;
		m_bTracking2 = TRUE;
		GetHitRect(hSplitterBox, m_rectTracker2);
		m_ptTrackOffset.y = yTrackOffset;

		// center it
		m_rectTracker.OffsetRect(0, m_rectLimit.Height()/2);
		m_rectTracker2.OffsetRect(m_rectLimit.Width()/2, 0);
	}
	else
	{
		// only hit one bar
		GetHitRect(ht, m_rectTracker);
	}

	// allow active view to preserve focus before taking it away
	CView* pView = (CView*)GetActivePane();
	if (pView != NULL && pView->IsKindOf(RUNTIME_CLASS(CView)))
	{
		ASSERT_VALID(pView);
		CFrameWnd* pFrameWnd = GetParentFrame();
		ASSERT_VALID(pFrameWnd);
		pView->OnActivateFrame(WA_INACTIVE, pFrameWnd);
	}

	// steal focus and capture
	SetCapture();
	SetFocus();

	// make sure no updates are pending
	RedrawWindow(NULL, NULL, RDW_ALLCHILDREN | RDW_UPDATENOW);

	// set tracking state and appropriate cursor
	m_bTracking = TRUE;
	OnInvertTracker(m_rectTracker);
	if (m_bTracking2)
		OnInvertTracker(m_rectTracker2);
	m_htTrack = ht;
	SetSplitCursor(ht);
}

void CSplitterWnd::TrackRowSize(int y, int row)
{
	ASSERT_VALID(this);
	ASSERT(m_nRows > 1);

	CPoint pt(0, y);
	ClientToScreen(&pt);
	GetPane(row, 0)->ScreenToClient(&pt);
	m_pRowInfo[row].nIdealSize = pt.y;      // new size
	if (pt.y < m_pRowInfo[row].nMinSize)
	{
		// resized too small
		m_pRowInfo[row].nIdealSize = 0; // make it go away
		if (GetStyle() & SPLS_DYNAMIC_SPLIT)
			DeleteRow(row);
	}
	else if (m_pRowInfo[row].nCurSize + m_pRowInfo[row+1].nCurSize
			< pt.y + m_pRowInfo[row+1].nMinSize)
	{
		// not enough room for other pane
		if (GetStyle() & SPLS_DYNAMIC_SPLIT)
			DeleteRow(row + 1);
	}
}

void CSplitterWnd::TrackColumnSize(int x, int col)
{
	ASSERT_VALID(this);
	ASSERT(m_nCols > 1);

	CPoint pt(x, 0);
	ClientToScreen(&pt);
	GetPane(0, col)->ScreenToClient(&pt);
	m_pColInfo[col].nIdealSize = pt.x;      // new size
	if (pt.x < m_pColInfo[col].nMinSize)
	{
		// resized too small
		m_pColInfo[col].nIdealSize = 0; // make it go away
		if (GetStyle() & SPLS_DYNAMIC_SPLIT)
			DeleteColumn(col);
	}
	else if (m_pColInfo[col].nCurSize + m_pColInfo[col+1].nCurSize
			< pt.x + m_pColInfo[col+1].nMinSize)
	{
		// not enough room for other pane
		if (GetStyle() & SPLS_DYNAMIC_SPLIT)
			DeleteColumn(col + 1);
	}
}

void CSplitterWnd::StopTracking(BOOL bAccept)
{
	ASSERT_VALID(this);

	if (!m_bTracking)
		return;

	ReleaseCapture();

	// erase tracker rectangle
	OnInvertTracker(m_rectTracker);
	if (m_bTracking2)
		OnInvertTracker(m_rectTracker2);
	m_bTracking = m_bTracking2 = FALSE;

	// save old active view
	CWnd* pOldActiveView = GetActivePane();

	// m_rectTracker is set to the new splitter position (without border)
	// (so, adjust relative to where the border will be)
	m_rectTracker.OffsetRect(-CX_BORDER , -CY_BORDER);
	m_rectTracker2.OffsetRect(-CX_BORDER, -CY_BORDER);

	if (bAccept)
	{
		if (m_htTrack == vSplitterBox)
		{
			SplitRow(m_rectTracker.top);
		}
		else if (m_htTrack >= vSplitterBar1 && m_htTrack <= vSplitterBar15)
		{
			// set row height
			TrackRowSize(m_rectTracker.top, m_htTrack - vSplitterBar1);
			RecalcLayout();
		}
		else if (m_htTrack == hSplitterBox)
		{
			SplitColumn(m_rectTracker.left);
		}
		else if (m_htTrack >= hSplitterBar1 && m_htTrack <= hSplitterBar15)
		{
			// set column width
			TrackColumnSize(m_rectTracker.left, m_htTrack - hSplitterBar1);
			RecalcLayout();
		}
		else if (m_htTrack >= splitterIntersection1 &&
			m_htTrack <= splitterIntersection225)
		{
			// set row height and column width
			int row = (m_htTrack - splitterIntersection1) / 15;
			int col = (m_htTrack - splitterIntersection1) % 15;

			TrackRowSize(m_rectTracker.top, row);
			TrackColumnSize(m_rectTracker2.left, col);
			RecalcLayout();
		}
		else if (m_htTrack == bothSplitterBox)
		{
			// rectTracker is vSplitter (splits rows)
			// rectTracker2 is hSplitter (splits cols)
			SplitRow(m_rectTracker.top);
			SplitColumn(m_rectTracker2.left);
		}
	}

	if (pOldActiveView == GetActivePane())
	{
		if (pOldActiveView != NULL)
		{
			SetActivePane(-1, -1, pOldActiveView); // re-activate
			pOldActiveView->SetFocus(); // make sure focus is restored
		}
	}
}

void CSplitterWnd::GetHitRect(int ht, CRect& rectHit)
{
	ASSERT_VALID(this);

	CRect rectClient;
	GetClientRect(&rectClient);
	rectClient.InflateRect(-m_cxBorder, -m_cyBorder);
	int cx = rectClient.Width();
	int cy = rectClient.Height();
	int x = rectClient.top;
	int y = rectClient.left;

	// hit rectangle does not include border
	// m_rectLimit will be limited to valid tracking rect
	// m_ptTrackOffset will be set to appropriate tracking offset
	m_ptTrackOffset.x = 0;
	m_ptTrackOffset.y = 0;

	if (ht == vSplitterBox)
	{
		cy = m_cySplitter - (2*m_cyBorder - afxData.bWin4);
		m_ptTrackOffset.y = -(cy / 2);
		ASSERT(m_pRowInfo[0].nCurSize > 0);
		m_rectLimit.bottom -= cy;
	}
	else if (ht == hSplitterBox)
	{
		cx = m_cxSplitter - (2*m_cxBorder - afxData.bWin4);
		m_ptTrackOffset.x = -(cx / 2);
		ASSERT(m_pColInfo[0].nCurSize > 0);
		m_rectLimit.right -= cx;
	}
	else if (ht >= vSplitterBar1 && ht <= vSplitterBar15)
	{
		cy = m_cySplitter - (2*m_cyBorder - afxData.bWin4);
		m_ptTrackOffset.y = -(cy / 2);
		for (int row = 0; row < ht - vSplitterBar1; row++)
			y += m_pRowInfo[row].nCurSize + m_cySplitterGap;
		m_rectLimit.top = y;
		y += m_pRowInfo[row].nCurSize + m_cyBorderShare + afxData.bWin4;
		m_rectLimit.bottom -= cy;
	}
	else if (ht >= hSplitterBar1 && ht <= hSplitterBar15)
	{
		cx = m_cxSplitter - (2*m_cxBorder - afxData.bWin4);
		m_ptTrackOffset.x = -(cx / 2);
		for (int col = 0; col < ht - hSplitterBar1; col++)
			x += m_pColInfo[col].nCurSize + m_cxSplitterGap;
		m_rectLimit.left = x;
		x += m_pColInfo[col].nCurSize + m_cxBorderShare + afxData.bWin4;
		m_rectLimit.right -= cx;
	}
	else
	{
		TRACE1("Error: GetHitRect(%d): Not Found!\n", ht);
		ASSERT(FALSE);
	}

	rectHit.right = (rectHit.left = x) + cx;
	rectHit.bottom = (rectHit.top = y) + cy;
}

int CSplitterWnd::HitTest(CPoint pt) const
{
	ASSERT_VALID(this);

	CRect rectClient;
	GetClientRect(&rectClient);
	rectClient.InflateRect(-m_cxBorder, -m_cyBorder);

	CRect rectInside;
	GetInsideRect(rectInside);

	if (m_bHasVScroll && m_nRows < m_nMaxRows &&
		CRect(rectInside.right, rectClient.top, rectClient.right,
		rectClient.top + m_cySplitter - afxData.bWin4).PtInRect(pt))
	{
		return vSplitterBox;
	}

	if (m_bHasHScroll && m_nCols < m_nMaxCols &&
		CRect(rectClient.left, rectInside.bottom,
		 rectClient.left + m_cxSplitter - afxData.bWin4,
		 rectClient.bottom).PtInRect(pt))
	{
		return hSplitterBox;
	}

	// for hit detect, include the border of splitters
	CRect rect;
	rect = rectClient;
	for (int col = 0; col < m_nCols - 1; col++)
	{
		rect.left += m_pColInfo[col].nCurSize;
		rect.right = rect.left + m_cxSplitterGap;
		if (rect.PtInRect(pt))
			break;
		rect.left = rect.right;
	}

	rect = rectClient;
	for (int row = 0; row < m_nRows - 1; row++)
	{
		rect.top += m_pRowInfo[row].nCurSize;
		rect.bottom = rect.top + m_cySplitterGap;
		if (rect.PtInRect(pt))
			break;
		rect.top = rect.bottom;
	}

	// row and col set for hit splitter (if not hit will be past end)
	if (col != m_nCols - 1)
	{
		if (row != m_nRows - 1)
			return splitterIntersection1 + row * 15 + col;
		return hSplitterBar1 + col;
	}

	if (row != m_nRows - 1)
		return vSplitterBar1 + row;

	return noHit;
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd tracking visuals

void CSplitterWnd::OnInvertTracker(const CRect& rect)
{
	ASSERT_VALID(this);
	ASSERT(!rect.IsRectEmpty());
	ASSERT((GetStyle() & WS_CLIPCHILDREN) == 0);

	// pat-blt without clip children on
	CDC* pDC = GetDC();
	// invert the brush pattern (looks just like frame window sizing)
	CBrush* pBrush = CDC::GetHalftoneBrush();
	HBRUSH hOldBrush = NULL;
	if (pBrush != NULL)
		hOldBrush = (HBRUSH)SelectObject(pDC->m_hDC, pBrush->m_hObject);
	pDC->PatBlt(rect.left, rect.top, rect.Width(), rect.Height(), PATINVERT);
	if (hOldBrush != NULL)
		SelectObject(pDC->m_hDC, hOldBrush);
	ReleaseDC(pDC);
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd commands

// Keyboard interface
BOOL CSplitterWnd::DoKeyboardSplit()
{
	ASSERT_VALID(this);

	int ht;
	if (m_nRows > 1 && m_nCols > 1)
		ht = splitterIntersection1; // split existing row+col
	else if (m_nRows > 1)
		ht = vSplitterBar1;         // split existing row
	else if (m_nCols > 1)
		ht = hSplitterBar1;         // split existing col
	else if (m_nMaxRows > 1 && m_nMaxCols > 1)
		ht = bothSplitterBox;       // we can split both
	else if (m_nMaxRows > 1)
		ht = vSplitterBox;          // we can split rows
	else if (m_nMaxCols > 1)
		ht = hSplitterBox;          // we can split columns
	else
		return FALSE;               // can't split

	// start tracking
	StartTracking(ht);

	CRect rect;
	rect.left = m_rectTracker.Width() / 2;
	rect.top = m_rectTracker.Height() / 2;
	if (m_ptTrackOffset.y != 0)
		rect.top = m_rectTracker.top;
	if (m_ptTrackOffset.x != 0)
		rect.left = m_bTracking2 ? m_rectTracker2.left :m_rectTracker.left;
	rect.OffsetRect(-m_ptTrackOffset.x, -m_ptTrackOffset.y);
	ClientToScreen(&rect);
	SetCursorPos(rect.left, rect.top);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Main drawing and layout

void CSplitterWnd::OnDisplayChange()
{
	if (!IsIconic() && IsWindowVisible())
		RecalcLayout();
}

void CSplitterWnd::OnSize(UINT nType, int cx, int cy)
{
	if (nType != SIZE_MINIMIZED && cx > 0 && cy > 0)
		RecalcLayout();

	CWnd::OnSize(nType, cx, cy);
}

// Generic routine:
//  for X direction: pInfo = m_pColInfo, nMax = m_nMaxCols, nSize = cx
//  for Y direction: pInfo = m_pRowInfo, nMax = m_nMaxRows, nSize = cy
AFX_STATIC void AFXAPI _AfxLayoutRowCol(CSplitterWnd::CRowColInfo* pInfoArray,
	int nMax, int nSize, int nSizeSplitter)
{
	ASSERT(pInfoArray != NULL);
	ASSERT(nMax > 0);
	ASSERT(nSizeSplitter > 0);

	CSplitterWnd::CRowColInfo* pInfo;
	int i;

	if (nSize < 0)
		nSize = 0;  // if really too small, layout as zero size

	// start with ideal sizes
	for (i = 0, pInfo = pInfoArray; i < nMax-1; i++, pInfo++)
	{
		if (pInfo->nIdealSize < pInfo->nMinSize)
			pInfo->nIdealSize = 0;      // too small to see
		pInfo->nCurSize = pInfo->nIdealSize;
	}
	pInfo->nCurSize = INT_MAX;  // last row/column takes the rest

	for (i = 0, pInfo = pInfoArray; i < nMax; i++, pInfo++)
	{
		ASSERT(nSize >= 0);
		if (nSize == 0)
		{
			// no more room (set pane to be invisible)
			pInfo->nCurSize = 0;
			continue;       // don't worry about splitters
		}
		else if (nSize < pInfo->nMinSize && i != 0)
		{
			// additional panes below the recommended minimum size
			//   aren't shown and the size goes to the previous pane
			pInfo->nCurSize = 0;

			// previous pane already has room for splitter + border
			//   add remaining size and remove the extra border
			ASSERT(afxData.cxBorder2 == afxData.cyBorder2);
			(pInfo-1)->nCurSize += nSize + afxData.cxBorder2;
			nSize = 0;
		}
		else
		{
			// otherwise we can add the second pane
			ASSERT(nSize > 0);
			if (pInfo->nCurSize == 0)
			{
				// too small to see
				if (i != 0)
					pInfo->nCurSize = 0;
			}
			else if (nSize < pInfo->nCurSize)
			{
				// this row/col won't fit completely - make as small as possible
				pInfo->nCurSize = nSize;
				nSize = 0;
			}
			else
			{
				// can fit everything
				nSize -= pInfo->nCurSize;
			}
		}

		// see if we should add a splitter
		ASSERT(nSize >= 0);
		if (i != nMax - 1)
		{
			// should have a splitter
			if (nSize > nSizeSplitter)
			{
				nSize -= nSizeSplitter; // leave room for splitter + border
				ASSERT(nSize > 0);
			}
			else
			{
				// not enough room - add left over less splitter size
				ASSERT(afxData.cxBorder2 == afxData.cyBorder2);
				pInfo->nCurSize += nSize;
				if (pInfo->nCurSize > (nSizeSplitter - afxData.cxBorder2))
					pInfo->nCurSize -= (nSizeSplitter - afxData.cyBorder2);
				nSize = 0;
			}
		}
	}
	ASSERT(nSize == 0); // all space should be allocated
}

// repositions client area of specified window
// assumes everything has WS_BORDER or is inset like it does
//  (includes scroll bars)
AFX_STATIC void AFXAPI _AfxDeferClientPos(AFX_SIZEPARENTPARAMS* lpLayout,
	CWnd* pWnd, int x, int y, int cx, int cy, BOOL bScrollBar)
{
	ASSERT(pWnd != NULL);
	ASSERT(pWnd->m_hWnd != NULL);

	if (bScrollBar)
	{
		// if there is enough room, draw scroll bar without border
		// if there is not enough room, set the WS_BORDER bit so that
		//   we will at least get a proper border drawn
		BOOL bNeedBorder = (cx <= CX_BORDER || cy <= CY_BORDER);
		pWnd->ModifyStyle(bNeedBorder ? 0 : WS_BORDER,
			bNeedBorder ? WS_BORDER : 0);
	}
	CRect rect(x, y, x+cx, y+cy);

	// adjust for border size (even if zero client size)
	if (!afxData.bWin4)
	{
		if (bScrollBar)
			rect.InflateRect(CX_BORDER, CY_BORDER);
		else
			pWnd->CalcWindowRect(&rect);
	}

	// adjust for 3d border (splitter windows have implied border)
	if ((pWnd->GetExStyle() & WS_EX_CLIENTEDGE) ||
		  pWnd->IsKindOf(RUNTIME_CLASS(CSplitterWnd)))
		rect.InflateRect(afxData.cxBorder2, afxData.cyBorder2);

	// first check if the new rectangle is the same as the current
	CRect rectOld;
	pWnd->GetWindowRect(rectOld);
	pWnd->GetParent()->ScreenToClient(&rectOld);
	if (rect != rectOld)
		AfxRepositionWindow(lpLayout, pWnd->m_hWnd, rect);
}

CWnd* CSplitterWnd::GetSizingParent()
{
	ASSERT_VALID(this);

	if (!afxData.bWin4)
		return NULL;

	// size box is in lower right corner of this window
	CRect rectClient;
	GetClientRect(rectClient);

	// find sizeable parent window
	CWnd* pParent = this;
	if (!(pParent->GetStyle() & WS_THICKFRAME))
		pParent = GetParent();

	// only allow if not maximized and has thick frame
	ASSERT_VALID(pParent);
	if ((pParent->GetStyle() & (WS_THICKFRAME|WS_MAXIMIZE)) == WS_THICKFRAME)
	{
		// convert client area of frame window relative to splitter
		CRect rect;
		pParent->GetClientRect(rect);
		pParent->ClientToScreen(rect);
		ScreenToClient(rect);

		// must match exactly to get the size box
		if (rectClient.BottomRight() == rect.BottomRight())
			return pParent;
	}

	return NULL;    // no sizeable parent found
}

void CSplitterWnd::RecalcLayout()
{
	ASSERT_VALID(this);
	ASSERT(m_nRows > 0 && m_nCols > 0); // must have at least one pane

	CRect rectClient;
	GetClientRect(rectClient);
	rectClient.InflateRect(-m_cxBorder, -m_cyBorder);

	CRect rectInside;
	GetInsideRect(rectInside);

	// layout columns (restrict to possible sizes)
	_AfxLayoutRowCol(m_pColInfo, m_nCols, rectInside.Width(), m_cxSplitterGap);
	_AfxLayoutRowCol(m_pRowInfo, m_nRows, rectInside.Height(), m_cySplitterGap);

	// adjust the panes (and optionally scroll bars)

	// give the hint for the maximum number of HWNDs
	AFX_SIZEPARENTPARAMS layout;
	layout.hDWP = ::BeginDeferWindowPos((m_nCols + 1) * (m_nRows + 1) + 1);

	// size of scrollbars
	int cx = (rectClient.right - rectInside.right) - afxData.bNotWin4;
	int cy = (rectClient.bottom - rectInside.bottom) - afxData.bNotWin4;

	// reposition size box
	if (m_bHasHScroll && m_bHasVScroll)
	{
		CWnd* pScrollBar = GetDlgItem(AFX_IDW_SIZE_BOX);
		ASSERT(pScrollBar != NULL);

		// fix style if necessary
		BOOL bSizingParent = (GetSizingParent() != NULL);
		// modifyStyle returns TRUE if style changes
		if (pScrollBar->ModifyStyle(SBS_SIZEGRIP|SBS_SIZEBOX,
				bSizingParent ? SBS_SIZEGRIP : SBS_SIZEBOX))
			pScrollBar->Invalidate();
		pScrollBar->EnableWindow(bSizingParent);

		// reposition the size box
		_AfxDeferClientPos(&layout, pScrollBar,
			rectInside.right + afxData.bNotWin4,
			rectInside.bottom + afxData.bNotWin4, cx, cy, TRUE);
	}

	// reposition scroll bars
	if (m_bHasHScroll)
	{
		int cxSplitterBox = m_cxSplitter + afxData.bNotWin4;// split box bigger
		int x = rectClient.left;
		int y = rectInside.bottom + afxData.bNotWin4;
		for (int col = 0; col < m_nCols; col++)
		{
			CWnd* pScrollBar = GetDlgItem(AFX_IDW_HSCROLL_FIRST + col);
			ASSERT(pScrollBar != NULL);
			int cx = m_pColInfo[col].nCurSize;
			if (col == 0 && m_nCols < m_nMaxCols)
				x += cxSplitterBox, cx -= cxSplitterBox;
			_AfxDeferClientPos(&layout, pScrollBar, x, y, cx, cy, TRUE);
			x += cx + m_cxSplitterGap;
		}
	}

	if (m_bHasVScroll)
	{
		int cySplitterBox = m_cySplitter + afxData.bNotWin4;// split box bigger
		int x = rectInside.right + afxData.bNotWin4;
		int y = rectClient.top;
		for (int row = 0; row < m_nRows; row++)
		{
			CWnd* pScrollBar = GetDlgItem(AFX_IDW_VSCROLL_FIRST + row);
			ASSERT(pScrollBar != NULL);
			int cy = m_pRowInfo[row].nCurSize;
			if (row == 0 && m_nRows < m_nMaxRows)
				y += cySplitterBox, cy -= cySplitterBox;
			_AfxDeferClientPos(&layout, pScrollBar, x, y, cx, cy, TRUE);
			y += cy + m_cySplitterGap;
		}
	}

	//BLOCK: Reposition all the panes
	{
		int x = rectClient.left;
		for (int col = 0; col < m_nCols; col++)
		{
			int cx = m_pColInfo[col].nCurSize;
			int y = rectClient.top;
			for (int row = 0; row < m_nRows; row++)
			{
				int cy = m_pRowInfo[row].nCurSize;
				CWnd* pWnd = GetPane(row, col);
				_AfxDeferClientPos(&layout, pWnd, x, y, cx, cy, FALSE);
				y += cy + m_cySplitterGap;
			}
			x += cx + m_cxSplitterGap;
		}
	}

	// move and resize all the windows at once!
	if (layout.hDWP == NULL || !::EndDeferWindowPos(layout.hDWP))
		TRACE0("Warning: DeferWindowPos failed - low system resources.\n");

	// invalidate all the splitter bars (with NULL pDC)
	DrawAllSplitBars(NULL, rectInside.right, rectInside.bottom);
}

void CSplitterWnd::DrawAllSplitBars(CDC* pDC, int cxInside, int cyInside)
{
	ASSERT_VALID(this);

	// draw column split bars
	CRect rect;
	GetClientRect(rect);
	rect.left += m_cxBorder;
	for (int col = 0; col < m_nCols - 1; col++)
	{
		rect.left += m_pColInfo[col].nCurSize + m_cxBorderShare;
		rect.right = rect.left + m_cxSplitter;
		if (rect.left > cxInside)
			break;      // stop if not fully visible
		OnDrawSplitter(pDC, splitBar, rect);
		rect.left = rect.right + m_cxBorderShare;
	}

	// draw row split bars
	GetClientRect(rect);
	rect.top += m_cyBorder;
	for (int row = 0; row < m_nRows - 1; row++)
	{
		rect.top += m_pRowInfo[row].nCurSize + m_cyBorderShare;
		rect.bottom = rect.top + m_cySplitter;
		if (rect.top > cyInside)
			break;      // stop if not fully visible
		OnDrawSplitter(pDC, splitBar, rect);
		rect.top = rect.bottom + m_cyBorderShare;
	}

	// draw pane borders
	if (afxData.bWin4)
	{
		GetClientRect(rect);
		int x = rect.left;
		for (col = 0; col < m_nCols; col++)
		{
			int cx = m_pColInfo[col].nCurSize + 2*m_cxBorder;
			if (col == m_nCols-1 && m_bHasVScroll)
				cx += afxData.cxVScroll - CX_BORDER;
			int y = rect.top;
			for (int row = 0; row < m_nRows; row++)
			{
				int cy = m_pRowInfo[row].nCurSize + 2*m_cyBorder;
				if (row == m_nRows-1 && m_bHasHScroll)
					cy += afxData.cyHScroll - CX_BORDER;
				OnDrawSplitter(pDC, splitBorder, CRect(x, y, x+cx, y+cy));
				y += cy + m_cySplitterGap - 2*m_cyBorder;
			}
			x += cx + m_cxSplitterGap - 2*m_cxBorder;
		}
	}
}

void CSplitterWnd::OnPaint()
{
	ASSERT_VALID(this);
	CPaintDC dc(this);

	CRect rectClient;
	GetClientRect(&rectClient);
	rectClient.InflateRect(-m_cxBorder, -m_cyBorder);

	CRect rectInside;
	GetInsideRect(rectInside);

	// draw the splitter boxes
	if (m_bHasVScroll && m_nRows < m_nMaxRows)
	{
		OnDrawSplitter(&dc, splitBox,
			CRect(rectInside.right + afxData.bNotWin4, rectClient.top,
				rectClient.right, rectClient.top + m_cySplitter));
	}

	if (m_bHasHScroll && m_nCols < m_nMaxCols)
	{
		OnDrawSplitter(&dc, splitBox,
			CRect(rectClient.left, rectInside.bottom + afxData.bNotWin4,
				rectClient.left + m_cxSplitter, rectClient.bottom));
	}

	// extend split bars to window border (past margins)
	DrawAllSplitBars(&dc, rectInside.right, rectInside.bottom);

	if (!afxData.bWin4)
	{
		// draw splitter intersections (inside only)
		GetInsideRect(rectInside);
		dc.IntersectClipRect(rectInside);
		CRect rect;
		rect.top = rectInside.top;
		for (int row = 0; row < m_nRows - 1; row++)
		{
			rect.top += m_pRowInfo[row].nCurSize + m_cyBorderShare;
			rect.bottom = rect.top + m_cySplitter;
			rect.left = rectInside.left;
			for (int col = 0; col < m_nCols - 1; col++)
			{
				rect.left += m_pColInfo[col].nCurSize + m_cxBorderShare;
				rect.right = rect.left + m_cxSplitter;
				OnDrawSplitter(&dc, splitIntersection, rect);
				rect.left = rect.right + m_cxBorderShare;
			}
			rect.top = rect.bottom + m_cxBorderShare;
		}
	}
}

BOOL CSplitterWnd::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
	if (nHitTest == HTCLIENT && pWnd == this && !m_bTracking)
		return TRUE;    // we will handle it in the mouse move

	return CWnd::OnSetCursor(pWnd, nHitTest, message);
}

// cache of last needed cursor
AFX_STATIC_DATA HCURSOR _afx_hcurLast = NULL;
AFX_STATIC_DATA HCURSOR _afx_hcurDestroy = NULL;
AFX_STATIC_DATA UINT _afx_idcPrimaryLast = 0; // store the primary IDC

void CSplitterWnd::SetSplitCursor(int ht)
{
	UINT idcPrimary;        // app supplied cursor
	LPCTSTR idcSecondary;    // system supplied cursor (MAKEINTRESOURCE)

	AfxLockGlobals(CRIT_SPLITTERWND);
	if (ht == vSplitterBox ||
		ht >= vSplitterBar1 && ht <= vSplitterBar15)
	{
		idcPrimary = AFX_IDC_VSPLITBAR;
		idcSecondary = IDC_SIZENS;
	}
	else if (ht == hSplitterBox ||
		ht >= hSplitterBar1 && ht <= hSplitterBar15)
	{
		idcPrimary = AFX_IDC_HSPLITBAR;
		idcSecondary = IDC_SIZEWE;
	}
	else if (ht == bothSplitterBox ||
		(ht >= splitterIntersection1 && ht <= splitterIntersection225))
	{
		idcPrimary = AFX_IDC_SMALLARROWS;
		idcSecondary = IDC_SIZEALL;
	}
	else
	{
		SetCursor(afxData.hcurArrow);
		idcPrimary = 0;     // don't use it
		idcSecondary = 0;   // don't use it
	}

	if (idcPrimary != 0)
	{
		HCURSOR hcurToDestroy = NULL;
		if (idcPrimary != _afx_idcPrimaryLast)
		{
			HINSTANCE hInst = AfxFindResourceHandle(
				MAKEINTRESOURCE(idcPrimary), RT_GROUP_CURSOR);

			// load in another cursor
			hcurToDestroy = _afx_hcurDestroy;

			// Note: If this LoadCursor call fails, it is likely that
			//  _AFX_NO_SPLITTER_RESOURCES is defined in your .RC file.
			// To correct the situation, remove the following line from your
			//  resource script:
			//      #define _AFX_NO_SPLITTER_RESOURCES
			// This should be done using the Resource.Set Includes... command.

			if ((_afx_hcurDestroy = _afx_hcurLast =
			   ::LoadCursor(hInst, MAKEINTRESOURCE(idcPrimary))) == NULL)
			{
				// will not look as good
				TRACE0("Warning: Could not find splitter cursor - using system provided alternative.\n");

				ASSERT(_afx_hcurDestroy == NULL);    // will not get destroyed
				_afx_hcurLast = ::LoadCursor(NULL, idcSecondary);
				ASSERT(_afx_hcurLast != NULL);
			}
			_afx_idcPrimaryLast = idcPrimary;
		}
		ASSERT(_afx_hcurLast != NULL);
		::SetCursor(_afx_hcurLast);
		ASSERT(_afx_hcurLast != hcurToDestroy);
		if (hcurToDestroy != NULL)
			::DestroyCursor(hcurToDestroy); // destroy after being set
	}
	AfxUnlockGlobals(CRIT_SPLITTERWND);
}

void CSplitterWnd::OnMouseMove(UINT /*nFlags*/, CPoint pt)
{
	if (GetCapture() != this)
		StopTracking(FALSE);

	if (m_bTracking)
	{
		// move tracker to current cursor position

		pt.Offset(m_ptTrackOffset); // pt is the upper right of hit detect
		// limit the point to the valid split range
		if (pt.y < m_rectLimit.top)
			pt.y = m_rectLimit.top;
		else if (pt.y > m_rectLimit.bottom)
			pt.y = m_rectLimit.bottom;
		if (pt.x < m_rectLimit.left)
			pt.x = m_rectLimit.left;
		else if (pt.x > m_rectLimit.right)
			pt.x = m_rectLimit.right;

		if (m_htTrack == vSplitterBox ||
			m_htTrack >= vSplitterBar1 && m_htTrack <= vSplitterBar15)
		{
			if (m_rectTracker.top != pt.y)
			{
				OnInvertTracker(m_rectTracker);
				m_rectTracker.OffsetRect(0, pt.y - m_rectTracker.top);
				OnInvertTracker(m_rectTracker);
			}
		}
		else if (m_htTrack == hSplitterBox ||
			m_htTrack >= hSplitterBar1 && m_htTrack <= hSplitterBar15)
		{
			if (m_rectTracker.left != pt.x)
			{
				OnInvertTracker(m_rectTracker);
				m_rectTracker.OffsetRect(pt.x - m_rectTracker.left, 0);
				OnInvertTracker(m_rectTracker);
			}
		}
		else if (m_htTrack == bothSplitterBox ||
		   (m_htTrack >= splitterIntersection1 &&
			m_htTrack <= splitterIntersection225))
		{
			if (m_rectTracker.top != pt.y)
			{
				OnInvertTracker(m_rectTracker);
				m_rectTracker.OffsetRect(0, pt.y - m_rectTracker.top);
				OnInvertTracker(m_rectTracker);
			}
			if (m_rectTracker2.left != pt.x)
			{
				OnInvertTracker(m_rectTracker2);
				m_rectTracker2.OffsetRect(pt.x - m_rectTracker2.left, 0);
				OnInvertTracker(m_rectTracker2);
			}
		}
	}
	else
	{
		// simply hit-test and set appropriate cursor

		int ht = HitTest(pt);
		SetSplitCursor(ht);
	}
}

void CSplitterWnd::OnLButtonDown(UINT /*nFlags*/, CPoint pt)
{
	if (m_bTracking)
		return;

	StartTracking(HitTest(pt));
}

void CSplitterWnd::OnLButtonDblClk(UINT /*nFlags*/, CPoint pt)
{
	int ht = HitTest(pt);
	CRect rect;

	StopTracking(FALSE);

	if ((GetStyle() & SPLS_DYNAMIC_SPLIT) == 0)
		return;     // do nothing if layout is static

	if (ht == vSplitterBox)
	{
		// half split
		SplitRow(m_pRowInfo[0].nCurSize / 2);
	}
	else if (ht == hSplitterBox)
	{
		// half split
		SplitColumn(m_pColInfo[0].nCurSize / 2);
	}
	else if (ht >= vSplitterBar1 && ht <= vSplitterBar15)
	{
		int rowDelete = ht - vSplitterBar1;
		// don't delete the active row
		int row;
		if (GetActivePane(&row, NULL) != NULL && rowDelete == row)
			++rowDelete;
		DeleteRow(rowDelete);
	}
	else if (ht >= hSplitterBar1 && ht <= hSplitterBar15)
	{
		int colDelete = ht - hSplitterBar1;
		// don't delete the active column
		int col;
		if (GetActivePane(NULL, &col) != NULL && colDelete == col)
			++colDelete;
		DeleteColumn(colDelete);
	}
	else if (ht >= splitterIntersection1 && ht <= splitterIntersection225)
	{
		int rowDelete = (ht - splitterIntersection1) / 15;
		int colDelete = (ht - splitterIntersection1) % 15;
		int row, col;
		if (GetActivePane(&row, &col) != NULL)
		{
			// don't delete the active row or column
			if (col == colDelete)
				++colDelete;
			if (row == rowDelete)
				++rowDelete;
		}
		DeleteRow(rowDelete);
		DeleteColumn(colDelete);
	}
}

void CSplitterWnd::OnLButtonUp(UINT /*nFlags*/, CPoint /*pt*/)
{
	StopTracking(TRUE);
}

void CSplitterWnd::OnCancelMode()
{
	StopTracking(FALSE);
}

void CSplitterWnd::OnKeyDown(UINT nChar, UINT /*nRepCnt*/, UINT /*nFlags*/)
{
	CPoint pt;
	GetCursorPos(&pt);

	int cz = GetKeyState(VK_CONTROL) < 0 ? 1 : 16;
	int dx = 0;
	int dy = 0;

	switch (nChar)
	{
	case VK_ESCAPE:
		StopTracking(FALSE);
		return;
	case VK_RETURN:
		StopTracking(TRUE);
		return;

	case VK_LEFT:
		dx = -1;
		break;
	case VK_RIGHT:
		dx = +1;
		break;
	case VK_UP:
		dy = -1;
		break;
	case VK_DOWN:
		dy = +1;
		break;

	default:
		Default();  // pass other keys through
		return;
	}

	if (m_htTrack == vSplitterBox ||
		m_htTrack >= vSplitterBar1 && m_htTrack <= vSplitterBar15)
	{
		// no movement along X axis
		dx = 0;
	}
	if (m_htTrack == hSplitterBox ||
		m_htTrack >= hSplitterBar1 && m_htTrack <= hSplitterBar15)
	{
		// no movement along Y axis
		dy = 0;
	}

	// adjust to new position
	pt.x += dx * cz;
	pt.y += dy * cz;

	// make sure within valid limits
	ScreenToClient(&pt);
	if (pt.y < m_rectLimit.top)
		pt.y = m_rectLimit.top;
	else if (pt.y > m_rectLimit.bottom)
		pt.y = m_rectLimit.bottom;
	if (pt.x < m_rectLimit.left)
		pt.x = m_rectLimit.left;
	else if (pt.x > m_rectLimit.right)
		pt.x = m_rectLimit.right;
	ClientToScreen(&pt);

	// cause WM_MOUSEMOVE to filter through
	SetCursorPos(pt.x, pt.y);
}

void CSplitterWnd::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == SC_SIZE)
	{
		CWnd* pParent = GetSizingParent();
		if (pParent != NULL)
		{
			pParent->SendMessage(WM_SYSCOMMAND, (WPARAM)nID, lParam);
			return;
		}
	}
	CWnd::OnSysCommand(nID, lParam);
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd command routing

BOOL CSplitterWnd::OnCommand(WPARAM wParam, LPARAM lParam)
{
	if (CWnd::OnCommand(wParam, lParam))
		return TRUE;

	// route commands to the splitter to the parent frame window
	return GetParentFrame()->SendMessage(WM_COMMAND, wParam, lParam);
}

BOOL CSplitterWnd::OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult)
{
	if (CWnd::OnNotify(wParam, lParam, pResult))
		return TRUE;

	// route commands to the splitter to the parent frame window
	*pResult = GetParentFrame()->SendMessage(WM_NOTIFY, wParam, lParam);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Scroll messages

BOOL CSplitterWnd::OnMouseWheel(UINT fFlags, short zDelta, CPoint point)
{
	BOOL bRetVal = FALSE;
	int row;
	int col;

	// find panes in the splitter that has scroll bars
	// and have them do their scrolling

	for (row = 0; row < m_nRows; row++)
	{
		for (col = 0; col < m_nCols; col++)
		{
			// only do the scrolling if the window is-a CScrollView

			CWnd* pPane = GetPane(row, col);
			CScrollView* pView = DYNAMIC_DOWNCAST(CScrollView, pPane);
			if (pView != NULL)
			{
				// prefer to scroll vertically if available

				CScrollBar* pBar = pView->GetScrollBarCtrl(SB_VERT);
				if (pBar == NULL)
				{
					pBar = pView->GetScrollBarCtrl(SB_HORZ);
					if (pBar == NULL)
						continue;
				}

				// get the old position, do the scrolling, and
				// then trigger the repaint

				int nOldPos = pBar->GetScrollPos();
				if (pView->DoMouseWheel(fFlags, zDelta, point))
					bRetVal = TRUE;

				if (col < m_nCols -1)
					pBar->SetScrollPos(nOldPos, FALSE);
			}
		}
	}

	return TRUE;
}

void CSplitterWnd::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	ASSERT(pScrollBar != NULL);
	int col = _AfxGetDlgCtrlID(pScrollBar->m_hWnd) - AFX_IDW_HSCROLL_FIRST;
	ASSERT(col >= 0 && col < m_nMaxCols);

	ASSERT(m_nRows > 0);
	int nOldPos = pScrollBar->GetScrollPos();
#ifdef _DEBUG
	int nNewPos;
#endif
	for (int row = 0; row < m_nRows; row++)
	{
		GetPane(row, col)->SendMessage(WM_HSCROLL,
			MAKELONG(nSBCode, nPos), (LPARAM)pScrollBar->m_hWnd);
#ifdef _DEBUG
		if (row == 0)
		{
			nNewPos = pScrollBar->GetScrollPos();
			if (pScrollBar->GetScrollPos() != nNewPos)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
		}
#endif //_DEBUG
		// set the scroll pos to the value it was originally for the next pane
		if (row < m_nRows - 1)
			pScrollBar->SetScrollPos(nOldPos, FALSE);
	}
}

void CSplitterWnd::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	ASSERT(pScrollBar != NULL);
	int row = _AfxGetDlgCtrlID(pScrollBar->m_hWnd) - AFX_IDW_VSCROLL_FIRST;
	ASSERT(row >= 0 && row < m_nMaxRows);

	ASSERT(m_nCols > 0);
	int nOldPos = pScrollBar->GetScrollPos();
#ifdef _DEBUG
	int nNewPos;
#endif
	for (int col = 0; col < m_nCols; col++)
	{
		GetPane(row, col)->SendMessage(WM_VSCROLL,
			MAKELONG(nSBCode, nPos), (LPARAM)pScrollBar->m_hWnd);
#ifdef _DEBUG
		if (col == 0)
		{
			nNewPos = pScrollBar->GetScrollPos();
			if (pScrollBar->GetScrollPos() != nNewPos)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
		}
#endif //_DEBUG
		// set the scroll pos to the value it was originally for the next pane
		if (col < m_nCols - 1)
			pScrollBar->SetScrollPos(nOldPos, FALSE);
	}
}

// synchronized scrolling
BOOL CSplitterWnd::DoScroll(CView* pViewFrom, UINT nScrollCode, BOOL bDoScroll)
{
	ASSERT_VALID(pViewFrom);

	int rowFrom, colFrom;
	if (!IsChildPane(pViewFrom, &rowFrom, &colFrom))
		return FALSE;

	BOOL bResult = FALSE;

	// save original positions
	int nOldVert = 0;
	CScrollBar* pScrollVert = pViewFrom->GetScrollBarCtrl(SB_VERT);
	if (pScrollVert != NULL)
		nOldVert = pScrollVert->GetScrollPos();
	int nOldHorz = 0;
	CScrollBar* pScrollHorz = pViewFrom->GetScrollBarCtrl(SB_HORZ);
	if (pScrollHorz != NULL)
		nOldHorz = pScrollHorz->GetScrollPos();

	// scroll the view from which the message is from
	if (pViewFrom->OnScroll(nScrollCode, 0, bDoScroll))
		bResult = TRUE;

	if (pScrollVert != NULL)
	{
#ifdef _DEBUG
		int nNewVert = pScrollVert->GetScrollPos();
#endif
		// scroll related columns
		for (int col = 0; col < m_nCols; col++)
		{
			if (col == colFrom)
				continue;

			// set the scroll pos to the value it was originally
			pScrollVert->SetScrollPos(nOldVert, FALSE);

			// scroll the pane
			CView* pView = (CView*)GetPane(rowFrom, col);
			ASSERT_KINDOF(CView, pView);
			ASSERT(pView != pViewFrom);
			if (pView->OnScroll(MAKEWORD(-1, HIBYTE(nScrollCode)), 0,
				bDoScroll))
			{
				bResult = TRUE;
			}

#ifdef _DEBUG
			if (pScrollVert->GetScrollPos() != nNewVert)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
#endif
		}
	}

	if (pScrollHorz != NULL)
	{
#ifdef _DEBUG
		int nNewHorz = pScrollHorz->GetScrollPos();
#endif
		// scroll related rows
		for (int row = 0; row < m_nRows; row++)
		{
			if (row == rowFrom)
				continue;

			// set the scroll pos to the value it was originally
			pScrollHorz->SetScrollPos(nOldHorz, FALSE);

			// scroll the pane
			CView* pView = (CView*)GetPane(row, colFrom);
			ASSERT_KINDOF(CView, pView);
			ASSERT(pView != pViewFrom);
			if (pView->OnScroll(MAKEWORD(LOBYTE(nScrollCode), -1), 0,
				bDoScroll))
			{
				bResult = TRUE;
			}

#ifdef _DEBUG
			if (pScrollHorz->GetScrollPos() != nNewHorz)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
#endif
		}
	}

	return bResult;
}

BOOL CSplitterWnd::DoScrollBy(CView* pViewFrom, CSize sizeScroll, BOOL bDoScroll)
{
	int rowFrom, colFrom;
	if (!IsChildPane(pViewFrom, &rowFrom, &colFrom))
		return FALSE;

	BOOL bResult = FALSE;

	// save original positions
	int nOldVert = 0;
	CScrollBar* pScrollVert = pViewFrom->GetScrollBarCtrl(SB_VERT);
	if (pScrollVert != NULL)
		nOldVert = pScrollVert->GetScrollPos();
	int nOldHorz = 0;
	CScrollBar* pScrollHorz = pViewFrom->GetScrollBarCtrl(SB_HORZ);
	if (pScrollHorz != NULL)
		nOldHorz = pScrollHorz->GetScrollPos();

	// scroll the view from which the message is from
	if (pViewFrom->OnScrollBy(sizeScroll, bDoScroll))
		bResult = TRUE;

	if (pScrollVert != NULL)
	{
#ifdef _DEBUG
		int nNewVert = pScrollVert->GetScrollPos();
#endif
		// scroll related columns
		for (int col = 0; col < m_nCols; col++)
		{
			if (col == colFrom)
				continue;

			// set the scroll pos to the value it was originally for the next pane
			pScrollVert->SetScrollPos(nOldVert, FALSE);

			// scroll the pane
			CView* pView = (CView*)GetPane(rowFrom, col);
			ASSERT_KINDOF(CView, pView);
			ASSERT(pView != pViewFrom);
			if (pView->OnScrollBy(CSize(0, sizeScroll.cy), bDoScroll))
				bResult = TRUE;

#ifdef _DEBUG
			if (pScrollVert->GetScrollPos() != nNewVert)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
#endif
		}
	}

	if (pScrollHorz != NULL)
	{
#ifdef _DEBUG
	int nNewHorz = pScrollHorz->GetScrollPos();
#endif
		// scroll related rows
		for (int row = 0; row < m_nRows; row++)
		{
			if (row == rowFrom)
				continue;

			// set the scroll pos to the value it was originally for the next pane
			pScrollHorz->SetScrollPos(nOldHorz, FALSE);

			// scroll the pane
			CView* pView = (CView*)GetPane(row, colFrom);
			ASSERT_KINDOF(CView, pView);
			ASSERT(pView != pViewFrom);
			if (pView->OnScrollBy(CSize(sizeScroll.cx, 0), bDoScroll))
				bResult = TRUE;

#ifdef _DEBUG
			if (pScrollHorz->GetScrollPos() != nNewHorz)
			{
				TRACE0("Warning: scroll panes setting different scroll positions.\n");
				// stick with the last one set
			}
#endif
		}
	}

	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// Focus control and control over the current pane/child

BOOL CSplitterWnd::CanActivateNext(BOOL)
{
	ASSERT_VALID(this);

	if (GetActivePane() == NULL)
	{
		TRACE0("Warning: Can't go to next pane - there is no current pane.\n");
		return FALSE;
	}
	ASSERT(m_nRows != 0);
	ASSERT(m_nCols != 0);
	// if more than 1x1 we can go to the next or prev pane
	return (m_nRows > 1) || (m_nCols > 1);
}

void CSplitterWnd::ActivateNext(BOOL bPrev)
{
	ASSERT_VALID(this);

	// find the coordinate of the current pane
	int row, col;
	if (GetActivePane(&row, &col) == NULL)
	{
		TRACE0("Warning: Cannot go to next pane - there is no current view.\n");
		return;
	}
	ASSERT(row >= 0 && row < m_nRows);
	ASSERT(col >= 0 && col < m_nCols);

	// determine next pane
	if (bPrev)
	{
		// prev
		if (--col < 0)
		{
			col = m_nCols - 1;
			if (--row < 0)
				row = m_nRows - 1;
		}
	}
	else
	{
		// next
		if (++col >= m_nCols)
		{
			col = 0;
			if (++row >= m_nRows)
				row = 0;
		}
	}

	// set newly active pane
	SetActivePane(row, col);
}

void CSplitterWnd::SetActivePane(int row, int col, CWnd* pWnd)
{
	// set the focus to the pane
	CWnd* pPane = pWnd == NULL ? GetPane(row, col) : pWnd;
	if (pPane->IsKindOf(RUNTIME_CLASS(CView)))
	{
		CFrameWnd* pFrameWnd = GetParentFrame();
		ASSERT_VALID(pFrameWnd);
		pFrameWnd->SetActiveView((CView*)pPane);
	}
	else
	{
		TRACE0("Warning: Next pane is not a view - calling SetFocus.\n");
		pPane->SetFocus();
	}
}

CWnd* CSplitterWnd::GetActivePane(int* pRow, int* pCol)
	// return active view, NULL when no active view
{
	ASSERT_VALID(this);

	// attempt to use active view of frame window
	CWnd* pView = NULL;
	CFrameWnd* pFrameWnd = GetParentFrame();
	ASSERT_VALID(pFrameWnd);
	pView = pFrameWnd->GetActiveView();

	// failing that, use the current focus
	if (pView == NULL)
		pView = GetFocus();

	// make sure the pane is a child pane of the splitter
	if (pView != NULL && !IsChildPane(pView, pRow, pCol))
		pView = NULL;

	return pView;
}

/////////////////////////////////////////////////////////////////////////////
// CSplitterWnd diagnostics

#ifdef _DEBUG
void CSplitterWnd::AssertValid() const
{
	CWnd::AssertValid();
	ASSERT(m_nMaxRows >= 1);
	ASSERT(m_nMaxCols >= 1);
	ASSERT(m_nMaxCols > 1 || m_nMaxRows > 1);       // 1x1 is not permitted
	ASSERT(m_nRows >= 1);
	ASSERT(m_nCols >= 1);
	ASSERT(m_nRows <= m_nMaxRows);
	ASSERT(m_nCols <= m_nMaxCols);
}

void CSplitterWnd::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);

	if (m_pDynamicViewClass != NULL)
		dc << "m_pDynamicViewClass = " << m_pDynamicViewClass->m_lpszClassName;
	dc << "\nm_nMaxRows = " << m_nMaxRows;
	dc << "\nm_nMaxCols = " << m_nMaxCols;
	dc << "\nm_nRows = " << m_nRows;
	dc << "\nm_nCols = " << m_nCols;
	dc << "\nm_bHasHScroll = " << m_bHasHScroll;
	dc << "\nm_bHasVScroll = " << m_bHasVScroll;
	dc << "\nm_cxSplitter = " << m_cxSplitter;
	dc << "\nm_cySplitter = " << m_cySplitter;
	if (m_bTracking)
	{
		dc << "\nTRACKING: m_htTrack = " << m_htTrack;
		dc << "\nm_rectLimit = " << m_rectLimit;
		dc << "\nm_ptTrackOffset = " << m_ptTrackOffset;
		dc << "\nm_rectTracker = " << m_rectTracker;
		if (m_bTracking2)
			dc << "\nm_rectTracker2 = " << m_rectTracker2;
	}

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
