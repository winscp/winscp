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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleCntrFrameWnd implementation

COleCntrFrameWnd::COleCntrFrameWnd(COleIPFrameWnd* pInPlaceFrame)
{
	m_pInPlaceFrame = pInPlaceFrame;
	RemoveFrameWnd();
}

COleCntrFrameWnd::~COleCntrFrameWnd()
{
	AddFrameWnd();
	Detach();
}

void COleCntrFrameWnd::RecalcLayout(BOOL /*bNotify*/)
{
	if (!m_bInRecalcLayout)
	{
		m_bInRecalcLayout = TRUE;
		m_nIdleFlags &= ~(idleLayout|idleNotify);

		ASSERT_VALID(m_pInPlaceFrame);
		COleServerDoc* pDoc = (COleServerDoc*)m_pInPlaceFrame->GetActiveDocument();
		if (pDoc != NULL && AfxGetThread()->m_pActiveWnd == m_pInPlaceFrame)
		{
			ASSERT_VALID(pDoc);
			ASSERT_KINDOF(COleServerDoc, pDoc);

			if (this == m_pInPlaceFrame->m_pMainFrame)
				pDoc->OnResizeBorder(NULL, m_pInPlaceFrame->m_lpFrame, TRUE);
			if (this == m_pInPlaceFrame->m_pDocFrame)
				pDoc->OnResizeBorder(NULL, m_pInPlaceFrame->m_lpDocFrame, FALSE);
		}
		m_bInRecalcLayout = FALSE;
	}
}

void COleCntrFrameWnd::OnIdleUpdateCmdUI()
{
	// do frame delayed recalc
	if (m_nIdleFlags & idleLayout)
		RecalcLayout(m_nIdleFlags & idleNotify);

	// update control bars
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CControlBar* pBar = (CControlBar*)m_listControlBars.GetNext(pos);
		ASSERT(pBar != NULL);
		ASSERT_VALID(pBar);
		AfxCallWndProc(pBar, pBar->m_hWnd, WM_IDLEUPDATECMDUI, TRUE, 0);
	}
}

BOOL COleCntrFrameWnd::OnCmdMsg(UINT nID, int nCode, void* pExtra,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	ASSERT_VALID(m_pInPlaceFrame);

	// pump through inplace frame
	CPushRoutingFrame push(this);
	return m_pInPlaceFrame->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);
}

void COleCntrFrameWnd::PostNcDestroy()
{
	// do nothing to avoid destroying window
}

#ifdef _DEBUG
void COleCntrFrameWnd::AssertValid() const
{
	// COleCntrFrameWnd bends the CWnd rules just a little bit.

	ASSERT(m_hWnd == NULL || ::IsWindow(m_hWnd));
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// COleIPFrameWnd implementation

BEGIN_MESSAGE_MAP(COleIPFrameWnd, CFrameWnd)
	//{{AFX_MSG_MAP(COleIPFrameWnd)
	ON_WM_SIZE()
	ON_MESSAGE(WM_RECALCPARENT, OnRecalcParent)
	ON_MESSAGE_VOID(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
	ON_WM_WINDOWPOSCHANGING()
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_MESSAGE(WM_SIZECHILD, OnResizeChild)
	ON_MESSAGE(WM_SETMESSAGESTRING, OnSetMessageString)
	ON_UPDATE_COMMAND_UI(ID_VIEW_STATUS_BAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_STATUS_BAR, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_TOOLBAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_TOOLBAR, OnBarCheck)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

COleIPFrameWnd::COleIPFrameWnd()
{
	// initialize OLEINPLACEFRAMEINFO struct
	memset(&m_frameInfo, 0, sizeof(m_frameInfo));
	m_frameInfo.cb = sizeof m_frameInfo;

	// initialize in-place state
	m_bUIActive = FALSE;
	m_lpFrame = NULL;
	m_lpDocFrame = NULL;
	m_hOleMenu = NULL;
	m_rectPos.SetRectEmpty();
	m_rectClip.SetRectEmpty();
	m_bInsideRecalc = FALSE;
	m_hSharedMenu = NULL;
	m_pMainFrame = NULL;
	m_pDocFrame = NULL;
	ASSERT_VALID(this);
}

COleIPFrameWnd::~COleIPFrameWnd()
{
	ASSERT_VALID(this);

	// destroy wrappers of container's frames
	delete m_pMainFrame;
	delete m_pDocFrame;

	// destroy shared menu
	if (m_hSharedMenu != NULL)
		::DestroyMenu(m_hSharedMenu);

	// interfaces to the container should already be released
	RELEASE(m_lpFrame);
	RELEASE(m_lpDocFrame);
}

int COleIPFrameWnd::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	ASSERT_VALID(this);

	if (CFrameWnd::OnCreate(lpCreateStruct) < 0)
		return -1;

	// remove WS_EX_CLIENTEDGE style if present
	ModifyStyleEx(WS_EX_CLIENTEDGE, 0, 0);

	// need to remove the pending WM_SETMESSAGESTRING from the queue
	MSG msg;
	PeekMessage(&msg, m_hWnd, WM_SETMESSAGESTRING, WM_SETMESSAGESTRING,
		PM_REMOVE|PM_NOYIELD);

	ASSERT_VALID(this);
	return 0;
}

void COleIPFrameWnd::OnDestroy()
{
	// notify the container that the rectangle has changed!
	COleServerDoc* pDoc = (COleServerDoc*)GetActiveDocument();
	if (pDoc != NULL)
	{
		ASSERT_KINDOF(COleServerDoc, pDoc);

		// close and abort changes to the document
		pDoc->DisconnectViews();
		pDoc->OnCloseDocument();
	}

	// explicitly destroy all of the dock bars since this window
	// is actually in the container and will not be destroyed
	if (m_pMainFrame != NULL)
		m_pMainFrame->DestroyDockBars();
	if (m_pDocFrame != NULL)
		m_pDocFrame->DestroyDockBars();

	CFrameWnd::OnDestroy();
}

BOOL COleIPFrameWnd::OnCreateControlBars(CWnd* pWndFrame, CWnd* /*pWndDoc*/)
{
	ASSERT(pWndFrame != NULL);
	UNUSED(pWndFrame);  // not used in release builds

	return TRUE;
}

BOOL COleIPFrameWnd::OnCreateControlBars(CFrameWnd* pWndFrame,
	CFrameWnd* pWndDoc)
{
	return OnCreateControlBars((CWnd*)pWndFrame, (CWnd*)pWndDoc);
}

void COleIPFrameWnd::OnIdleUpdateCmdUI()
{
	// update toolbars which may be on the container
	// and allow delayed recalc layouts to execute
	if (m_pMainFrame != NULL)
		m_pMainFrame->OnIdleUpdateCmdUI();
	if (m_pDocFrame != NULL)
		m_pDocFrame->OnIdleUpdateCmdUI();
}

void COleIPFrameWnd::OnWindowPosChanging(LPWINDOWPOS lpWndPos)
{
	MSG msg;
	if (!::PeekMessage(&msg, NULL, WM_KICKIDLE, WM_KICKIDLE, PM_NOREMOVE))
		::PostThreadMessage(GetCurrentThreadId(), WM_KICKIDLE, 0, 0);

	CFrameWnd::OnWindowPosChanging(lpWndPos);
}

LRESULT COleIPFrameWnd::OnSetMessageString(WPARAM wParam, LPARAM lParam)
{
	USES_CONVERSION;

	if (m_lpFrame != NULL)
	{
		LPCTSTR lpsz = NULL;
		CString strMessage;

		// set the message bar text
		if (lParam != NULL)
		{
			ASSERT(wParam == 0);    // can't have both an ID and a string
			lpsz = (LPCTSTR)lParam; // set an explicit string
		}
		else if (wParam != 0)
		{
			// get message associated with the ID indicated by wParam
			GetMessageString(wParam, strMessage);
			lpsz = strMessage;
		}

		// notify container of new status text
		if (lpsz == NULL)
			lpsz = _T("");
		m_lpFrame->SetStatusText(T2COLE(lpsz));
	}

	UINT nIDLast = m_nIDLastMessage;
	m_nIDLastMessage = (UINT)wParam;    // new ID (or 0)
	m_nIDTracking = (UINT)wParam;       // so F1 on toolbar buttons work
	return nIDLast;
}

BOOL COleIPFrameWnd::LoadFrame(UINT nIDResource, DWORD dwDefaultStyle,
	CWnd* pParentWnd, CCreateContext* pContext)
{
	if (pParentWnd != NULL)
		ASSERT_VALID(pParentWnd);

	// only do this once
	ASSERT_VALID_IDR(nIDResource);
	ASSERT(m_nIDHelp == 0 || m_nIDHelp == nIDResource);

	m_nIDHelp = nIDResource;    // ID for help context (+HID_BASE_RESOURCE)

	// create the window (use child window style create)
	CRect rect(0, 0, 0, 0);
	if (!CWnd::Create(NULL, NULL, dwDefaultStyle, rect, pParentWnd,
		nIDResource, pContext))
	{
		return FALSE;   // will self destruct on failure normally
	}

	// load accelerator resource
	LoadAccelTable(MAKEINTRESOURCE(nIDResource));

	return TRUE;
}

void COleIPFrameWnd::OnSize(UINT /*nType*/, int /*cx*/, int /*cy*/)
{
	// recalc layout is not called in OnSize since COleIPFrameWnd does
	//  "inside out" recalc -- which is driven by the size of the
	//  inner most window changing, not the outer most!
}

LRESULT COleIPFrameWnd::OnResizeChild(WPARAM, LPARAM lParam)
{
	// notify the container that the rectangle has changed!
	COleServerDoc* pDoc = (COleServerDoc*)GetActiveDocument();
	if (pDoc == NULL)
		return 0;

	ASSERT_KINDOF(COleServerDoc, pDoc);

	// get new rect and parent
	CRect rectNew;
	rectNew.CopyRect((LPCRECT)lParam);
	CWnd* pParentWnd = GetParent();
	ASSERT_VALID(pParentWnd);

	// convert rectNew relative to pParentWnd
	ClientToScreen(&rectNew);
	pParentWnd->ScreenToClient(&rectNew);

	// adjust the new rectangle for the current control bars
	CWnd* pLeftOver = GetDlgItem(AFX_IDW_PANE_FIRST);
	ASSERT(pLeftOver != NULL);
	CRect rectCur = m_rectPos;
	pLeftOver->CalcWindowRect(&rectCur, CWnd::adjustOutside);
	rectNew.left += m_rectPos.left - rectCur.left;
	rectNew.top += m_rectPos.top - rectCur.top;
	rectNew.right -= rectCur.right - m_rectPos.right;
	rectNew.bottom -= rectCur.bottom - m_rectPos.bottom;
	OnRequestPositionChange(rectNew);

	return 0;
}

void COleIPFrameWnd::OnRequestPositionChange(LPCRECT lpRect)
{
	COleServerDoc* pDoc = (COleServerDoc*)GetActiveDocument();
	ASSERT_VALID(pDoc);
	ASSERT_KINDOF(COleServerDoc, pDoc);

	// DocObjects don't need to generate OnPosRectChange calls,
	// so we can just return if this is a DoCobject

	if (pDoc->IsDocObject())
		return;

	// The default behavior is to not affect the extent during the
	//  call to RequestPositionChange.  This results in consistent
	//  scaling behavior.

	pDoc->RequestPositionChange(lpRect);
}

LRESULT COleIPFrameWnd::OnRecalcParent(WPARAM, LPARAM lParam)
{
	// simply call recalc layout
	RepositionFrame(&m_rectPos, &m_rectClip);

	// fill in the new rectangle if specified
	if ((LPRECT)lParam != NULL)
		*(LPRECT)lParam = m_rectPos;

	return TRUE;
}

void COleIPFrameWnd::RecalcLayout(BOOL /*bNotify*/)
{
	ASSERT_VALID(this);

	// better have a parent window (only used for inplace)
	CWnd* pParentWnd = GetParent();
	ASSERT_VALID(pParentWnd);

	// see if this frame is supporting a normal in-place object or
	// a DocObject. DocObjects put scrollbars on the inside of the rect

	UINT nAdjustType = CWnd::adjustBorder;
	COleServerDoc* pDoc = (COleServerDoc*) GetActiveDocument();
	if (pDoc != NULL)
	{
		ASSERT_VALID(pDoc);
		if (pDoc->IsDocObject())
			nAdjustType = CWnd::adjustOutside;
	}

	// first call reposition bars with arbitarily large rect just to
	//  see how much space the bars will take up
	CRect rectBig(0, 0, INT_MAX/2, INT_MAX/2);
	CRect rectLeft;
	RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposQuery,
		&rectLeft, &rectBig);

	// grow the rect by the size of the control bars
	CRect rect = m_rectPos;
	rect.left -= rectLeft.left;
	rect.top -= rectLeft.top;
	rect.right += INT_MAX/2 - rectLeft.right;
	rect.bottom += INT_MAX/2 - rectLeft.bottom;

	// see how much extra space for non-client areas (such as scrollbars)
	//  that the view needs.
	CWnd* pLeftOver = GetDlgItem(AFX_IDW_PANE_FIRST);
	if (pLeftOver != NULL)
	{
		rectBig = m_rectPos;
		pLeftOver->CalcWindowRect(&rectBig, CWnd::adjustOutside);
		rect.left -= m_rectPos.left - rectBig.left;
		rect.top -= m_rectPos.top - rectBig.top;
		rect.right += rectBig.right - m_rectPos.right;
		rect.bottom += rectBig.bottom - m_rectPos.bottom;
	}

	// adjust for non-client area on the frame window
	CalcWindowRect(&rect, nAdjustType);

	// the frame window must be clipped to the visible part in the container
	CRect rectVis;
	rectVis.IntersectRect(&rect, &m_rectClip);

	// move the window
	AfxRepositionWindow(NULL, m_hWnd, &rectVis);

	// now resize the control bars relative to the (now moved) frame
	pParentWnd->ClientToScreen(&rect);
	ScreenToClient(&rect);
	RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST,
		CWnd::reposDefault, NULL, &rect);
}

void COleIPFrameWnd::RepositionFrame(LPCRECT lpPosRect, LPCRECT lpClipRect)
{
	ASSERT(AfxIsValidAddress(lpPosRect, sizeof(RECT), FALSE));
	ASSERT(AfxIsValidAddress(lpClipRect, sizeof(RECT), FALSE));

	// gaurd against recursion
	if (m_bInsideRecalc)
		return;
	m_bInsideRecalc = TRUE;

	// remember the client area for later
	m_rectPos.CopyRect(lpPosRect);
	m_rectClip.CopyRect(lpClipRect);

	// recalc layout based on new position & clipping rectangles
	RecalcLayout();

	// remove recursion lockout
	m_bInsideRecalc = FALSE;
}

BOOL COleIPFrameWnd::PreTranslateMessage(MSG* pMsg)
{
	// check server's accelerators first
	if (CFrameWnd::PreTranslateMessage(pMsg))
		return TRUE;

	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
	{
		// always check to see if they exist in the default accel table
		//  (they may exist but not be translated when disabled)
		HACCEL hAccel = GetDefaultAccelerator();
		if (hAccel != NULL && IsAccelerator(hAccel,
			CopyAcceleratorTable(hAccel, NULL, 0), pMsg, NULL))
		{
			return TRUE;
		}

		// check container's accelerators as last chance
		OLEINPLACEFRAMEINFO frameInfo = m_frameInfo;
		if (::OleTranslateAccelerator(m_lpFrame, &frameInfo, pMsg) == S_OK)
			return TRUE;
	}

	return FALSE;   // keystroke not processed.
}

void COleIPFrameWnd::OnUpdateControlBarMenu(CCmdUI* pCmdUI)
{
	if (GetControlBar(pCmdUI->m_nID) != NULL)
		CFrameWnd::OnUpdateControlBarMenu(pCmdUI);
	else if (m_pMainFrame != NULL &&
		m_pMainFrame->GetControlBar(pCmdUI->m_nID) != NULL)
	{
		m_pMainFrame->OnUpdateControlBarMenu(pCmdUI);
	}
	else if (m_pDocFrame != NULL &&
		m_pDocFrame->GetControlBar(pCmdUI->m_nID) != NULL)
	{
		m_pDocFrame->OnUpdateControlBarMenu(pCmdUI);
	}
	else
		pCmdUI->ContinueRouting();
}

BOOL COleIPFrameWnd::OnBarCheck(UINT nID)
{
	if (GetControlBar(nID) != NULL)
		return CFrameWnd::OnBarCheck(nID);
	else if (m_pMainFrame != NULL && m_pMainFrame->GetControlBar(nID) != NULL)
		return m_pMainFrame->OnBarCheck(nID);
	else if (m_pDocFrame != NULL && m_pDocFrame->GetControlBar(nID) != NULL)
		return m_pDocFrame->OnBarCheck(nID);
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Special-case context sensitive help

void COleIPFrameWnd::OnContextHelp()
{
	if (m_bHelpMode == HELP_ACTIVE || !CanEnterHelpMode())
		return;

	// notify container that we are entering context sensitive help
	BOOL bHelpMode = m_bHelpMode;
	m_bHelpMode = HELP_ACTIVE;
	ASSERT(m_lpFrame != NULL);
	if (m_lpFrame->ContextSensitiveHelp(TRUE) != S_OK ||
		(m_lpDocFrame != NULL && m_lpDocFrame->ContextSensitiveHelp(TRUE) != S_OK))
	{
		m_bHelpMode = HELP_INACTIVE;
		return;
	}
	m_bHelpMode = bHelpMode;

	// echo help mode to top-level frame
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	if (pFrameWnd != this)
		pFrameWnd->m_bHelpMode = HELP_ACTIVE;

	// now enter context sensitive help mode ourselves
	CFrameWnd::OnContextHelp();

	// echo help mode to top-level frame
	if (pFrameWnd != this)
		pFrameWnd->m_bHelpMode = m_bHelpMode;

	if (m_bHelpMode == HELP_INACTIVE)
	{
		// make sure container exits context sensitive help mode
		m_lpFrame->ContextSensitiveHelp(FALSE);
		if (m_lpDocFrame != NULL)
			m_lpDocFrame->ContextSensitiveHelp(FALSE);
	}
}

/////////////////////////////////////////////////////////////////////////////
// In-place activation startup

HMENU COleIPFrameWnd::GetInPlaceMenu()
{
	// get active document associated with this frame window
	CDocument* pDoc = GetActiveDocument();
	ASSERT_VALID(pDoc);

	// get in-place menu from the doc template
	CDocTemplate* pTemplate = pDoc->GetDocTemplate();
	ASSERT_VALID(pTemplate);
	return pTemplate->m_hMenuInPlaceServer;
}

BOOL COleIPFrameWnd::BuildSharedMenu()
{
	HMENU hMenu = GetInPlaceMenu();

	// create shared menu
	ASSERT(m_hSharedMenu == NULL);
	if ((m_hSharedMenu = ::CreateMenu()) == NULL)
		return FALSE;

	// start out by getting menu from container
	memset(&m_menuWidths, 0, sizeof m_menuWidths);
	if (m_lpFrame->InsertMenus(m_hSharedMenu, &m_menuWidths) != S_OK)
	{
		::DestroyMenu(m_hSharedMenu);
		m_hSharedMenu = NULL;
		return FALSE;
	}
	// container shouldn't touch these
	ASSERT(m_menuWidths.width[1] == 0);
	ASSERT(m_menuWidths.width[3] == 0);
	ASSERT(m_menuWidths.width[5] == 0);

	// only copy the popups if there is a menu loaded
	if (hMenu == NULL)
		return TRUE;

	// insert our menu popups amongst the container menus
	AfxMergeMenus(m_hSharedMenu, hMenu, &m_menuWidths.width[0], 1);

	// finally create the special OLE menu descriptor
	m_hOleMenu = ::OleCreateMenuDescriptor(m_hSharedMenu, &m_menuWidths);

	return m_hOleMenu != NULL;
}

void COleIPFrameWnd::DestroySharedMenu()
{
	if (m_hSharedMenu == NULL)
	{
		ASSERT(m_hOleMenu == NULL);
		return;
	}

	// get in-place menu to be unmerged (must be same as during activation)
	HMENU hMenu = GetInPlaceMenu();
	if (hMenu == NULL)
		return;

	// remove our menu popups from the shared menu
	AfxUnmergeMenus(m_hSharedMenu, hMenu);

	// allow container to remove its items from the menu
	ASSERT(m_lpFrame != NULL);
	VERIFY(m_lpFrame->RemoveMenus(m_hSharedMenu) == S_OK);

	// now destroy the menu
	::DestroyMenu(m_hSharedMenu);
	m_hSharedMenu = NULL;

	if (m_hOleMenu != NULL)
	{
		VERIFY(::OleDestroyMenuDescriptor(m_hOleMenu) == S_OK);
		m_hOleMenu = NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleIPFrameWnd diagnostics

#ifdef _DEBUG
void COleIPFrameWnd::AssertValid() const
{
	CFrameWnd::AssertValid();
	if (m_hSharedMenu != NULL)
		ASSERT(::IsMenu(m_hSharedMenu));
}

void COleIPFrameWnd::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);

	dc << "m_lpFrame = " << m_lpFrame;
	dc << "\nm_lpDocFrame = " << m_lpDocFrame;
	dc << "\nm_hOleMenu = " << m_hOleMenu;
	dc << "\nm_rectPos = " << m_rectPos;
	dc << "\nm_rectClip = " << m_rectClip;
	dc << "\nm_bInsideRecalc = " << m_bInsideRecalc;
	dc << "\nm_hSharedMenu = " << m_hSharedMenu;

	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(COleIPFrameWnd, CFrameWnd)

/////////////////////////////////////////////////////////////////////////////
