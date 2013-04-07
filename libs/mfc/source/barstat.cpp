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
// CStatusBar creation, etc

#define SBPF_UPDATE 0x0001  // pending update of text

struct AFX_STATUSPANE
{
	UINT    nID;        // IDC of indicator: 0 => normal text area
	int     cxText;     // width of string area in pixels
						//   on both sides there is a 3 pixel gap and
						//   a one pixel border, making a pane 6 pixels wider
	UINT    nStyle;     // style flags (SBPS_*)
	UINT    nFlags;     // state flags (SBPF_*)
	CString strText;    // text in the pane
};

inline AFX_STATUSPANE* CStatusBar::_GetPanePtr(int nIndex) const
{
	ASSERT((nIndex >= 0 && nIndex < m_nCount) || m_nCount == 0);
	return ((AFX_STATUSPANE*)m_pData) + nIndex;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#define CX_PANE_BORDER 6    // 3 pixels on each side of each pane

CStatusBar::CStatusBar()
{
	// setup default border/margin depending on type of system
	m_cyTopBorder = 2;
	if (afxData.bWin4)
	{
		m_cxLeftBorder = 0;
		m_cxRightBorder = 0;
		m_cyBottomBorder = 0;
	}
	else
	{
		m_cxLeftBorder = 2;
		m_cxRightBorder = 2;
		m_cyBottomBorder = 1;
	}
	// minimum height set with SB_SETMINHEIGHT is cached
	m_nMinHeight = 0;
}

CStatusBar::~CStatusBar()
{
	AllocElements(0, 0);    // destroys existing elements
}

BOOL CStatusBar::Create(CWnd* pParentWnd, DWORD dwStyle, UINT nID)
{
	return CreateEx(pParentWnd, 0, dwStyle, nID);
}

BOOL CStatusBar::CreateEx(CWnd* pParentWnd, DWORD dwCtrlStyle, DWORD dwStyle, UINT nID)
{
	ASSERT_VALID(pParentWnd);   // must have a parent

	// save the style (some of these style bits are MFC specific)
	m_dwStyle = (dwStyle & CBRS_ALL);

	// translate MFC style bits to windows style bits
	dwStyle &= ~CBRS_ALL;
	dwStyle |= CCS_NOPARENTALIGN|CCS_NOMOVEY|CCS_NODIVIDER|CCS_NORESIZE;
	if (pParentWnd->GetStyle() & WS_THICKFRAME)
		dwStyle |= SBARS_SIZEGRIP;
	dwStyle |= dwCtrlStyle;

	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTL_BAR_REG));

	// create the HWND
	CRect rect; rect.SetRectEmpty();
	return CWnd::Create(STATUSCLASSNAME, NULL, dwStyle, rect, pParentWnd, nID);
}

BOOL CStatusBar::PreCreateWindow(CREATESTRUCT& cs)
{
	// in Win4, status bars do not have a border at all, since it is
	//  provided by the client area.
	if (afxData.bWin4 &&
		(m_dwStyle & (CBRS_ALIGN_ANY|CBRS_BORDER_ANY)) == CBRS_BOTTOM)
	{
		m_dwStyle &= ~(CBRS_BORDER_ANY|CBRS_BORDER_3D);
	}
	return CControlBar::PreCreateWindow(cs);
}

BOOL CStatusBar::SetIndicators(const UINT* lpIDArray, int nIDCount)
{
	ASSERT_VALID(this);
	ASSERT(nIDCount >= 1);  // must be at least one of them
	ASSERT(lpIDArray == NULL ||
		AfxIsValidAddress(lpIDArray, sizeof(UINT) * nIDCount, FALSE));
	ASSERT(::IsWindow(m_hWnd));

	// first allocate array for panes and copy initial data
	if (!AllocElements(nIDCount, sizeof(AFX_STATUSPANE)))
		return FALSE;
	ASSERT(nIDCount == m_nCount);

	// copy initial data from indicator array
	BOOL bResult = TRUE;
	if (lpIDArray != NULL)
	{
		HFONT hFont = (HFONT)SendMessage(WM_GETFONT);
		CClientDC dcScreen(NULL);
		HGDIOBJ hOldFont = NULL;
		if (hFont != NULL)
			hOldFont = dcScreen.SelectObject(hFont);

		AFX_STATUSPANE* pSBP = _GetPanePtr(0);
		for (int i = 0; i < nIDCount; i++)
		{
			pSBP->nID = *lpIDArray++;
			pSBP->nFlags |= SBPF_UPDATE;
			if (pSBP->nID != 0)
			{
				if (!pSBP->strText.LoadString(pSBP->nID))
				{
					TRACE1("Warning: failed to load indicator string 0x%04X.\n",
						pSBP->nID);
					bResult = FALSE;
					break;
				}
				pSBP->cxText = dcScreen.GetTextExtent(pSBP->strText).cx;
				ASSERT(pSBP->cxText >= 0);
				if (!SetPaneText(i, pSBP->strText, FALSE))
				{
					bResult = FALSE;
					break;
				}
			}
			else
			{
				// no indicator (must access via index)
				// default to 1/4 the screen width (first pane is stretchy)
				pSBP->cxText = ::GetSystemMetrics(SM_CXSCREEN)/4;
				if (i == 0)
					pSBP->nStyle |= (SBPS_STRETCH | SBPS_NOBORDERS);
			}
			++pSBP;
		}
		if (hOldFont != NULL)
			dcScreen.SelectObject(hOldFont);
	}
	UpdateAllPanes(TRUE, TRUE);

	return bResult;
}

BOOL CStatusBar::AllocElements(int nElements, int cbElement)
{
	// destruct old elements
	AFX_STATUSPANE* pSBP = _GetPanePtr(0);
	for (int i = 0; i < m_nCount; i++)
	{
		pSBP->strText.~CString();
		++pSBP;
	}

	// allocate new elements
	if (!CControlBar::AllocElements(nElements, cbElement))
		return FALSE;

	// construct new elements
	pSBP = _GetPanePtr(0);
	for (i = 0; i < m_nCount; i++)
	{
		memcpy(&pSBP->strText, &afxEmptyString, sizeof(CString));
		++pSBP;
	}
	return TRUE;
}

void CStatusBar::CalcInsideRect(CRect& rect, BOOL bHorz) const
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(bHorz);  // vertical status bar not supported

	// subtract standard CControlBar borders
	CControlBar::CalcInsideRect(rect, bHorz);

	// subtract size grip if present
	if ((GetStyle() & SBARS_SIZEGRIP) && !::IsZoomed(::GetParent(m_hWnd)))
	{
		// get border metrics from common control
		int rgBorders[3];
		CStatusBar* pBar = (CStatusBar*)this;
		pBar->DefWindowProc(SB_GETBORDERS, 0, (LPARAM)&rgBorders);

		// size grip uses a border + size of scrollbar + cx border
		rect.right -= rgBorders[0] + ::GetSystemMetrics(SM_CXVSCROLL) +
			::GetSystemMetrics(SM_CXBORDER) * 2;
	}
}

void CStatusBar::UpdateAllPanes(BOOL bUpdateRects, BOOL bUpdateText)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	// update the status pane locations
	if (bUpdateRects)
	{
		// get border information and client work area
		CRect rect; GetWindowRect(rect);
		rect.OffsetRect(-rect.left, -rect.top);
		CalcInsideRect(rect, TRUE);
		int rgBorders[3];
		VERIFY((BOOL)DefWindowProc(SB_GETBORDERS, 0, (LPARAM)&rgBorders));

		// determine extra space for stretchy pane
		int cxExtra = rect.Width() + rgBorders[2];
		int nStretchyCount = 0;
		AFX_STATUSPANE* pSBP = _GetPanePtr(0);
		for (int i = 0; i < m_nCount; i++)
		{
			if (pSBP->nStyle & SBPS_STRETCH)
				++nStretchyCount;
			cxExtra -= (pSBP->cxText+CX_PANE_BORDER + rgBorders[2]);
			++pSBP;
		}

		// determine right edge of each pane
		int* rgRights = (int*)_alloca(m_nCount * sizeof(int));
		int right = rgBorders[0];
		pSBP = _GetPanePtr(0);
		for (i = 0; i < m_nCount; i++)
		{
			// determine size of the pane
			ASSERT(pSBP->cxText >= 0);
			right += pSBP->cxText+CX_PANE_BORDER;
			if ((pSBP->nStyle & SBPS_STRETCH) && cxExtra > 0)
			{
				ASSERT(nStretchyCount != 0);
				int cxAddExtra = cxExtra / nStretchyCount;
				right += cxAddExtra;
				--nStretchyCount;
				cxExtra -= cxAddExtra;
			}
			rgRights[i] = right;

			// next pane
			++pSBP;
			right += rgBorders[2];
		}

		// set new right edges for all panes
		DefWindowProc(SB_SETPARTS, m_nCount, (LPARAM)rgRights);
	}

	// update text in the status panes if specified
	if (bUpdateText)
	{
		AFX_STATUSPANE* pSBP = _GetPanePtr(0);
		for (int i = 0; i < m_nCount; i++)
		{
			if (pSBP->nFlags & SBPF_UPDATE)
				SetPaneText(i, pSBP->strText);
			++pSBP;
		}
	}
}

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
// CStatusBar attribute access

int CStatusBar::CommandToIndex(UINT nIDFind) const
{
	ASSERT_VALID(this);

	if (m_nCount <= 0)
		return -1;

	AFX_STATUSPANE* pSBP = _GetPanePtr(0);
	for (int i = 0; i < m_nCount; i++, pSBP++)
		if (pSBP->nID == nIDFind)
			return i;

	return -1;
}

UINT CStatusBar::GetItemID(int nIndex) const
{
	ASSERT_VALID(this);
	return _GetPanePtr(nIndex)->nID;
}

void CStatusBar::GetItemRect(int nIndex, LPRECT lpRect) const
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	CStatusBar* pBar = (CStatusBar*)this;
	if (!pBar->DefWindowProc(SB_GETRECT, nIndex, (LPARAM)lpRect))
		::SetRectEmpty(lpRect);
}

UINT CStatusBar::GetPaneStyle(int nIndex) const
{
	return _GetPanePtr(nIndex)->nStyle;
}

void CStatusBar::SetPaneStyle(int nIndex, UINT nStyle)
{
	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
	if (pSBP->nStyle != nStyle)
	{
		// if the pane is changing SBPS_STRETCH, then...
		if ((pSBP->nStyle ^ nStyle) & SBPS_STRETCH)
		{
			// ... we need to re-layout the panes
			pSBP->nStyle = nStyle;
			UpdateAllPanes(TRUE, FALSE);
		}

		// use SetPaneText, since it updates the style and text
		pSBP->nStyle = nStyle;
		pSBP->nFlags |= SBPF_UPDATE;
		SetPaneText(nIndex, pSBP->strText);
	}
}

void CStatusBar::GetPaneInfo(int nIndex, UINT& nID, UINT& nStyle,
	int& cxWidth) const
{
	ASSERT_VALID(this);

	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
	nID = pSBP->nID;
	nStyle = pSBP->nStyle;
	cxWidth = pSBP->cxText;
}

void CStatusBar::SetPaneInfo(int nIndex, UINT nID, UINT nStyle, int cxWidth)
{
	ASSERT_VALID(this);

	BOOL bChanged = FALSE;
	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
	pSBP->nID = nID;
	if (pSBP->nStyle != nStyle)
	{
		if ((pSBP->nStyle ^ nStyle) & SBPS_STRETCH)
			bChanged = TRUE;
		else
		{
			pSBP->nStyle = nStyle;
			pSBP->nFlags |= SBPF_UPDATE;
			SetPaneText(nIndex, pSBP->strText);
		}
		pSBP->nStyle = nStyle;
	}
	if (cxWidth != pSBP->cxText)
	{
		// change width of one pane -> invalidate the entire status bar
		pSBP->cxText = cxWidth;
		bChanged = TRUE;
	}
	if (bChanged)
		UpdateAllPanes(TRUE, FALSE);
}

void CStatusBar::GetPaneText(int nIndex, CString& s) const
{
	ASSERT_VALID(this);

	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
	s = pSBP->strText;
}

CString CStatusBar::GetPaneText(int nIndex) const
{
	ASSERT_VALID(this);

	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
	return pSBP->strText;
}

BOOL CStatusBar::SetPaneText(int nIndex, LPCTSTR lpszNewText, BOOL bUpdate)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);

	if (!(pSBP->nFlags & SBPF_UPDATE) &&
		((lpszNewText == NULL && pSBP->strText.IsEmpty()) ||
		 (lpszNewText != NULL && pSBP->strText.Compare(lpszNewText) == 0)))
	{
		// nothing to change
		return TRUE;
	}

	TRY
	{
		if (lpszNewText != NULL)
			pSBP->strText = lpszNewText;
		else
			pSBP->strText.Empty();
	}
	CATCH_ALL(e)
	{
		// Note: DELETE_EXCEPTION(e) not required
		return FALSE;
	}
	END_CATCH_ALL

	if (!bUpdate)
	{
		// can't update now, wait until later
		pSBP->nFlags |= SBPF_UPDATE;
		return TRUE;
	}

	pSBP->nFlags &= ~SBPF_UPDATE;
	DefWindowProc(SB_SETTEXT, ((WORD)pSBP->nStyle)|nIndex,
		(pSBP->nStyle & SBPS_DISABLED) ? NULL :
		(LPARAM)(LPCTSTR)pSBP->strText);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CStatusBar implementation

CSize CStatusBar::CalcFixedLayout(BOOL, BOOL bHorz)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	// determinme size of font being used by the status bar
	TEXTMETRIC tm;
	{
		CClientDC dc(NULL);
		HFONT hFont = (HFONT)SendMessage(WM_GETFONT);
		HGDIOBJ hOldFont = NULL;
		if (hFont != NULL)
			hOldFont = dc.SelectObject(hFont);
		VERIFY(dc.GetTextMetrics(&tm));
		if (hOldFont != NULL)
			dc.SelectObject(hOldFont);
	}

	// get border information
	CRect rect; rect.SetRectEmpty();
	CalcInsideRect(rect, bHorz);
	int rgBorders[3];
	DefWindowProc(SB_GETBORDERS, 0, (LPARAM)&rgBorders);

	// determine size, including borders
	CSize size;
	size.cx = 32767;
	size.cy = tm.tmHeight - tm.tmInternalLeading - 1
		+ rgBorders[1] * 2 + ::GetSystemMetrics(SM_CYBORDER) * 2
		- rect.Height();
	if (size.cy < m_nMinHeight)
		size.cy = m_nMinHeight;

	return size;
}

/////////////////////////////////////////////////////////////////////////////
// CStatusBar message handlers

BEGIN_MESSAGE_MAP(CStatusBar, CControlBar)
	//{{AFX_MSG_MAP(CStatusBar)
	ON_WM_NCHITTEST()
	ON_WM_NCPAINT()
	ON_WM_PAINT()
	ON_WM_NCCALCSIZE()
	ON_WM_SIZE()
	ON_WM_WINDOWPOSCHANGING()
	ON_MESSAGE(WM_SETTEXT, OnSetText)
	ON_MESSAGE(WM_GETTEXT, OnGetText)
	ON_MESSAGE(WM_GETTEXTLENGTH, OnGetTextLength)
	ON_MESSAGE(SB_SETMINHEIGHT, OnSetMinHeight)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

UINT CStatusBar::OnNcHitTest(CPoint)
{
	UINT nResult = (UINT)Default();
	if (nResult == HTBOTTOMRIGHT)
		return HTBOTTOMRIGHT;
	else
		return HTCLIENT;
}

void CStatusBar::OnNcCalcSize(BOOL /*bCalcValidRects*/, NCCALCSIZE_PARAMS* lpncsp)
{
	// calculate border space (will add to top/bottom, subtract from right/bottom)
	CRect rect; rect.SetRectEmpty();
	CControlBar::CalcInsideRect(rect, TRUE);
	ASSERT(rect.top >= 2);

	// adjust non-client area for border space
	lpncsp->rgrc[0].left += rect.left;
	lpncsp->rgrc[0].top += rect.top - 2;
	lpncsp->rgrc[0].right += rect.right;
	lpncsp->rgrc[0].bottom += rect.bottom;
}

void CStatusBar::OnBarStyleChange(DWORD dwOldStyle, DWORD dwNewStyle)
{
	if (m_hWnd != NULL &&
		((dwOldStyle & CBRS_BORDER_ANY) != (dwNewStyle & CBRS_BORDER_ANY)))
	{
		// recalc non-client area when border styles change
		SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOMOVE | SWP_NOSIZE | SWP_NOACTIVATE | SWP_DRAWFRAME);
	}
}

void CStatusBar::OnNcPaint()
{
	EraseNonClient();
}

// Derived class is responsible for implementing all of these handlers
//  for owner/self draw controls.
void CStatusBar::DrawItem(LPDRAWITEMSTRUCT)
{
	ASSERT(FALSE);
}

BOOL CStatusBar::OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam, LRESULT* pResult)
{
	if (message != WM_DRAWITEM)
		return CWnd::OnChildNotify(message, wParam, lParam, pResult);

	ASSERT(pResult == NULL);
	UNUSED(pResult); // unused in release builds
	DrawItem((LPDRAWITEMSTRUCT)lParam);
	return TRUE;
}

void CStatusBar::OnPaint()
{
	UpdateAllPanes(FALSE, TRUE);

	Default();
}

void CStatusBar::OnSize(UINT nType, int cx, int cy)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	CControlBar::OnSize(nType, cx, cy);

	// need to adjust pane right edges (because of stretchy pane)
	UpdateAllPanes(TRUE, FALSE);
}

void CStatusBar::OnWindowPosChanging(LPWINDOWPOS lpWndPos)
{
	// not necessary to invalidate the borders
	DWORD dwStyle = m_dwStyle;
	m_dwStyle &= ~(CBRS_BORDER_ANY);
	CControlBar::OnWindowPosChanging(lpWndPos);
	m_dwStyle = dwStyle;
}

LRESULT CStatusBar::OnSetText(WPARAM, LPARAM lParam)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	int nIndex = CommandToIndex(0);
	if (nIndex < 0)
		return -1;
	return SetPaneText(nIndex, (LPCTSTR)lParam) ? 0 : -1;
}

LRESULT CStatusBar::OnGetText(WPARAM wParam, LPARAM lParam)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	int nMaxLen = (int)wParam;
	if (nMaxLen == 0)
		return 0;       // nothing copied
	LPTSTR lpszDest = (LPTSTR)lParam;

	int nLen = 0;
	int nIndex = CommandToIndex(0); // use pane with ID zero
	if (nIndex >= 0)
	{
		AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
		nLen = pSBP->strText.GetLength();
		if (nLen > nMaxLen)
			nLen = nMaxLen - 1; // number of characters to copy (less term.)
		memcpy(lpszDest, (LPCTSTR)pSBP->strText, nLen*sizeof(TCHAR));
	}
	lpszDest[nLen] = '\0';
	return nLen+1;      // number of bytes copied
}

LRESULT CStatusBar::OnGetTextLength(WPARAM, LPARAM)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));

	int nLen = 0;
	int nIndex = CommandToIndex(0); // use pane with ID zero
	if (nIndex >= 0)
	{
		AFX_STATUSPANE* pSBP = _GetPanePtr(nIndex);
		nLen = pSBP->strText.GetLength();
	}
	return nLen;
}

LRESULT CStatusBar::OnSetMinHeight(WPARAM wParam, LPARAM)
{
	LRESULT lResult = Default();
	m_nMinHeight = wParam;
	return lResult;
}

/////////////////////////////////////////////////////////////////////////////
// CStatusBar idle update through CStatusCmdUI class

class CStatusCmdUI : public CCmdUI      // class private to this file!
{
public: // re-implementations only
	virtual void Enable(BOOL bOn);
	virtual void SetCheck(int nCheck);
	virtual void SetText(LPCTSTR lpszText);
};

void CStatusCmdUI::Enable(BOOL bOn)
{
	m_bEnableChanged = TRUE;
	CStatusBar* pStatusBar = (CStatusBar*)m_pOther;
	ASSERT(pStatusBar != NULL);
	ASSERT_KINDOF(CStatusBar, pStatusBar);
	ASSERT(m_nIndex < m_nIndexMax);

	UINT nNewStyle = pStatusBar->GetPaneStyle(m_nIndex) & ~SBPS_DISABLED;
	if (!bOn)
		nNewStyle |= SBPS_DISABLED;
	pStatusBar->SetPaneStyle(m_nIndex, nNewStyle);
}

void CStatusCmdUI::SetCheck(int nCheck) // "checking" will pop out the text
{
	CStatusBar* pStatusBar = (CStatusBar*)m_pOther;
	ASSERT(pStatusBar != NULL);
	ASSERT_KINDOF(CStatusBar, pStatusBar);
	ASSERT(m_nIndex < m_nIndexMax);

	UINT nNewStyle = pStatusBar->GetPaneStyle(m_nIndex) & ~SBPS_POPOUT;
	if (nCheck != 0)
		nNewStyle |= SBPS_POPOUT;
	pStatusBar->SetPaneStyle(m_nIndex, nNewStyle);
}

void CStatusCmdUI::SetText(LPCTSTR lpszText)
{
	CStatusBar* pStatusBar = (CStatusBar*)m_pOther;
	ASSERT(pStatusBar != NULL);
	ASSERT_KINDOF(CStatusBar, pStatusBar);
	ASSERT(m_nIndex < m_nIndexMax);

	pStatusBar->SetPaneText(m_nIndex, lpszText);
}

void CStatusBar::OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler)
{
	CStatusCmdUI state;
	state.m_pOther = this;
	state.m_nIndexMax = (UINT)m_nCount;
	for (state.m_nIndex = 0; state.m_nIndex < state.m_nIndexMax;
		state.m_nIndex++)
	{
		state.m_nID = _GetPanePtr(state.m_nIndex)->nID;

		// allow the statusbar itself to have update handlers
		if (CWnd::OnCmdMsg(state.m_nID, CN_UPDATE_COMMAND_UI, &state, NULL))
			continue;

		// allow target (owner) to handle the remaining updates
		state.DoUpdate(pTarget, FALSE);
	}

	// update the dialog controls added to the status bar
	UpdateDialogControls(pTarget, bDisableIfNoHndler);
}

/////////////////////////////////////////////////////////////////////////////
// CStatusBar diagnostics

#ifdef _DEBUG
void CStatusBar::AssertValid() const
{
	CControlBar::AssertValid();
}

void CStatusBar::Dump(CDumpContext& dc) const
{
	CControlBar::Dump(dc);

	if (dc.GetDepth() > 0)
	{
		for (int i = 0; i < m_nCount; i++)
		{
			dc << "\nstatus pane[" << i << "] = {";
			dc << "\n\tnID = " << _GetPanePtr(i)->nID;
			dc << "\n\tnStyle = " << _GetPanePtr(i)->nStyle;
			dc << "\n\tcxText = " << _GetPanePtr(i)->cxText;
			dc << "\n\tstrText = " << _GetPanePtr(i)->strText;
			dc << "\n\t}";
		}
	}
	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CStatusBar, CControlBar)

/////////////////////////////////////////////////////////////////////////////
