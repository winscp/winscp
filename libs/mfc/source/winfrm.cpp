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
#include <dde.h>        // for DDE execute shell requests

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CRect for creating windows with the default position/size

const AFX_DATADEF CRect CFrameWnd::rectDefault(
	CW_USEDEFAULT, CW_USEDEFAULT,
	0 /* 2*CW_USEDEFAULT */, 0 /* 2*CW_USEDEFAULT */);

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd

// register for Windows 95 or Windows NT 3.51
AFX_STATIC UINT _afxMsgMouseWheel =
   (((::GetVersion() & 0x80000000) && LOBYTE(LOWORD(::GetVersion()) == 4)) ||
	 (!(::GetVersion() & 0x80000000) && LOBYTE(LOWORD(::GetVersion()) == 3)))
	 ? ::RegisterWindowMessage(MSH_MOUSEWHEEL) : 0;

BEGIN_MESSAGE_MAP(CFrameWnd, CWnd)
	//{{AFX_MSG_MAP(CFrameWnd)
	ON_WM_INITMENU()
	ON_WM_INITMENUPOPUP()
	ON_WM_MENUSELECT()
	ON_MESSAGE(WM_POPMESSAGESTRING, OnPopMessageString)
	ON_MESSAGE(WM_SETMESSAGESTRING, OnSetMessageString)
	ON_MESSAGE(WM_HELPPROMPTADDR, OnHelpPromptAddr)
	ON_MESSAGE_VOID(WM_IDLEUPDATECMDUI, OnIdleUpdateCmdUI)
	ON_WM_ENTERIDLE()
	ON_WM_HSCROLL()
	ON_WM_VSCROLL()
	ON_WM_SETFOCUS()
	ON_WM_CREATE()
	ON_WM_DESTROY()
	ON_WM_CLOSE()
	ON_WM_SIZE()
	ON_WM_ERASEBKGND()
	ON_WM_ACTIVATE()
	ON_WM_NCACTIVATE()
	ON_WM_SYSCOMMAND()
	ON_WM_DROPFILES()
	ON_WM_QUERYENDSESSION()
	ON_WM_ENDSESSION()
	ON_WM_SETCURSOR()
	ON_WM_ENABLE()
	// OLE palette support
	ON_WM_QUERYNEWPALETTE()
	ON_WM_PALETTECHANGED()
	ON_MESSAGE(WM_COMMANDHELP, OnCommandHelp)
	ON_MESSAGE(WM_HELPHITTEST, OnHelpHitTest)
	ON_MESSAGE(WM_ACTIVATETOPLEVEL, OnActivateTopLevel)
	// turning on and off standard frame gadgetry
	ON_UPDATE_COMMAND_UI(ID_VIEW_STATUS_BAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_STATUS_BAR, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_TOOLBAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_TOOLBAR, OnBarCheck)
	ON_UPDATE_COMMAND_UI(ID_VIEW_REBAR, OnUpdateControlBarMenu)
	ON_COMMAND_EX(ID_VIEW_REBAR, OnBarCheck)
	// turning on and off standard mode indicators
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_CAPS, OnUpdateKeyIndicator)
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_NUM, OnUpdateKeyIndicator)
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_SCRL, OnUpdateKeyIndicator)
	ON_UPDATE_COMMAND_UI(ID_INDICATOR_KANA, OnUpdateKeyIndicator)
	// standard help handling
	ON_UPDATE_COMMAND_UI(ID_CONTEXT_HELP, OnUpdateContextHelp)
	// toolbar "tooltip" notification
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTW, 0, 0xFFFF, OnToolTipText)
	ON_NOTIFY_EX_RANGE(TTN_NEEDTEXTA, 0, 0xFFFF, OnToolTipText)
	//}}AFX_MSG_MAP
	// message handling for standard DDE commands
	ON_MESSAGE(WM_DDE_INITIATE, OnDDEInitiate)
	ON_MESSAGE(WM_DDE_EXECUTE, OnDDEExecute)
	ON_MESSAGE(WM_DDE_TERMINATE, OnDDETerminate)
	ON_REGISTERED_MESSAGE(_afxMsgMouseWheel, OnRegisteredMouseWheel)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd construction/destruction

CFrameWnd::CFrameWnd()
{
	ASSERT(m_hWnd == NULL);

	m_nWindow = -1;                 // unknown window ID
	m_bAutoMenuEnable = TRUE;       // auto enable on by default
	m_lpfnCloseProc = NULL;
	m_hMenuDefault = NULL;
	m_hAccelTable = NULL;
	m_nIDHelp = 0;
	m_nIDTracking = 0;
	m_nIDLastMessage = 0;
	m_pViewActive = NULL;

	m_cModalStack = 0;              // initialize modality support
	m_phWndDisable = NULL;
	m_pNotifyHook = NULL;
	m_hMenuAlt = NULL;
	m_nIdleFlags = 0;               // no idle work at start
	m_rectBorder.SetRectEmpty();

	m_bHelpMode = HELP_INACTIVE;    // not in Shift+F1 help mode
	m_dwPromptContext = 0;

	m_pNextFrameWnd = NULL;         // not in list yet

	m_bInRecalcLayout = FALSE;
	m_pFloatingFrameClass = NULL;
	m_nShowDelay = -1;              // no delay pending

	AddFrameWnd();
}

CFrameWnd::~CFrameWnd()
{
	RemoveFrameWnd();
	if (m_phWndDisable != NULL)
		delete[] (void*)m_phWndDisable;
}

void CFrameWnd::AddFrameWnd()
{
	// hook it into the CFrameWnd list
	AFX_MODULE_THREAD_STATE* pState = _AFX_CMDTARGET_GETSTATE()->m_thread;
	pState->m_frameList.AddHead(this);
}

void CFrameWnd::RemoveFrameWnd()
{
	// remove this frame window from the list of frame windows
	AFX_MODULE_THREAD_STATE* pState = _AFX_CMDTARGET_GETSTATE()->m_thread;
	pState->m_frameList.Remove(this);
}

/////////////////////////////////////////////////////////////////////////////
// Special processing etc

BOOL CFrameWnd::LoadAccelTable(LPCTSTR lpszResourceName)
{
	ASSERT(m_hAccelTable == NULL);  // only do once
	ASSERT(lpszResourceName != NULL);

	HINSTANCE hInst = AfxFindResourceHandle(lpszResourceName, RT_ACCELERATOR);
	m_hAccelTable = ::LoadAccelerators(hInst, lpszResourceName);
	return (m_hAccelTable != NULL);
}

HACCEL CFrameWnd::GetDefaultAccelerator()
{
	// use document specific accelerator table over m_hAccelTable
	HACCEL hAccelTable = m_hAccelTable;
	HACCEL hAccel;
	CDocument* pDoc = GetActiveDocument();
	if (pDoc != NULL && (hAccel = pDoc->GetDefaultAccelerator()) != NULL)
		hAccelTable = hAccel;

	return hAccelTable;
}

BOOL CFrameWnd::PreTranslateMessage(MSG* pMsg)
{
	// check for special cancel modes for combo boxes
	if (pMsg->message == WM_LBUTTONDOWN || pMsg->message == WM_NCLBUTTONDOWN)
		AfxCancelModes(pMsg->hwnd);    // filter clicks

	// allow tooltip messages to be filtered
	if (CWnd::PreTranslateMessage(pMsg))
		return TRUE;

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to consume message
	if (m_pNotifyHook != NULL && m_pNotifyHook->OnPreTranslateMessage(pMsg))
		return TRUE;
#endif

	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
	{
		// finally, translate the message
		HACCEL hAccel = GetDefaultAccelerator();
		return hAccel != NULL &&  ::TranslateAccelerator(m_hWnd, hAccel, pMsg);
	}
	return FALSE;
}

void CFrameWnd::PostNcDestroy()
{
	// default for frame windows is to allocate them on the heap
	//  the default post-cleanup is to 'delete this'.
	// never explicitly call 'delete' on a CFrameWnd, use DestroyWindow instead
	delete this;
}

void CFrameWnd::OnPaletteChanged(CWnd* pFocusWnd)
{
	CWnd::OnPaletteChanged(pFocusWnd);
#ifndef _AFX_NO_OLE_SUPPORT
	if (m_pNotifyHook != NULL)
		m_pNotifyHook->OnPaletteChanged(pFocusWnd);
#endif
}

BOOL CFrameWnd::OnQueryNewPalette()
{
#ifndef _AFX_NO_OLE_SUPPORT
	if (m_pNotifyHook != NULL && m_pNotifyHook->OnQueryNewPalette())
		return TRUE;
#endif
	return CWnd::OnQueryNewPalette();
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd support for context sensitive help.

void CFrameWnd::ExitHelpMode()
{
	// if not in help mode currently, this is a no-op
	if (!m_bHelpMode)
		return;

	// only post new WM_EXITHELPMODE message if one doesn't already exist
	//  in the queue.
	MSG msg;
	if (!::PeekMessage(&msg, m_hWnd, WM_EXITHELPMODE, WM_EXITHELPMODE,
		PM_REMOVE|PM_NOYIELD))
	{
		VERIFY(::PostMessage(m_hWnd, WM_EXITHELPMODE, 0, 0));
	}

	// release capture if this window has it
	if (::GetCapture() == m_hWnd)
		ReleaseCapture();

	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);
	pFrameWnd->m_bHelpMode = m_bHelpMode = HELP_INACTIVE;
	PostMessage(WM_KICKIDLE);   // trigger idle update
}

BOOL CFrameWnd::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);
	if (pFrameWnd->m_bHelpMode)
	{
		SetCursor(afxData.hcurHelp);
		return TRUE;
	}
	return CWnd::OnSetCursor(pWnd, nHitTest, message);
}

LRESULT CFrameWnd::OnCommandHelp(WPARAM, LPARAM lParam)
{
	if (lParam == 0)
	{
		if (IsTracking())
			lParam = HID_BASE_COMMAND+m_nIDTracking;
		else
			lParam = HID_BASE_RESOURCE+m_nIDHelp;
	}
	if (lParam != 0)
	{
		CWinApp* pApp = AfxGetApp();
		if (pApp != NULL)
			pApp->WinHelp(lParam);
		return TRUE;
	}
	return FALSE;
}

LRESULT CFrameWnd::OnHelpHitTest(WPARAM, LPARAM)
{
	if (m_nIDHelp != 0)
		return HID_BASE_RESOURCE+m_nIDHelp;
	else
		return 0;
}

BOOL CFrameWnd::OnCommand(WPARAM wParam, LPARAM lParam)
	// return TRUE if command invocation was attempted
{
	HWND hWndCtrl = (HWND)lParam;
	UINT nID = LOWORD(wParam);

	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);
	if (pFrameWnd->m_bHelpMode && hWndCtrl == NULL &&
		nID != ID_HELP && nID != ID_DEFAULT_HELP && nID != ID_CONTEXT_HELP)
	{
		// route as help
		if (!SendMessage(WM_COMMANDHELP, 0, HID_BASE_COMMAND+nID))
			SendMessage(WM_COMMAND, ID_DEFAULT_HELP);
		return TRUE;
	}

	// route as normal command
	return CWnd::OnCommand(wParam, lParam);
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd support for modality

BOOL AFXAPI AfxIsDescendant(HWND hWndParent, HWND hWndChild)
	// helper for detecting whether child descendent of parent
	//  (works with owned popups as well)
{
	ASSERT(::IsWindow(hWndParent));
	ASSERT(::IsWindow(hWndChild));

	do
	{
		if (hWndParent == hWndChild)
			return TRUE;

		hWndChild = AfxGetParentOwner(hWndChild);
	} while (hWndChild != NULL);

	return FALSE;
}

void CFrameWnd::BeginModalState()
{
	ASSERT(m_hWnd != NULL);
	ASSERT(::IsWindow(m_hWnd));

	// allow stacking, but don't do anything
	if (++m_cModalStack > 1)
		return;

	// determine top-level parent, since that is the true parent of any
	//  modeless windows anyway...
	CWnd* pParent = GetTopLevelParent();

	// first count all windows that need to be disabled
	UINT nCount = 0;
	HWND hWnd = ::GetWindow(::GetDesktopWindow(), GW_CHILD);
	while (hWnd != NULL)
	{
		if (::IsWindowEnabled(hWnd) &&
			CWnd::FromHandlePermanent(hWnd) != NULL &&
			AfxIsDescendant(pParent->m_hWnd, hWnd) &&
			::SendMessage(hWnd, WM_DISABLEMODAL, 0, 0) == 0)
		{
			++nCount;
		}
		hWnd = ::GetWindow(hWnd, GW_HWNDNEXT);
	}
	if (nCount == 0)
		return;

	m_phWndDisable = new HWND[nCount+1];

	// disable all windows connected to this frame (and add them to the list)
	UINT nIndex = 0;
	hWnd = ::GetWindow(::GetDesktopWindow(), GW_CHILD);
	while (hWnd != NULL)
	{
		if (::IsWindowEnabled(hWnd) &&
			CWnd::FromHandlePermanent(hWnd) != NULL &&
			AfxIsDescendant(pParent->m_hWnd, hWnd) &&
			::SendMessage(hWnd, WM_DISABLEMODAL, 0, 0) == 0)
		{
			::EnableWindow(hWnd, FALSE);
			ASSERT(nIndex < nCount);
			m_phWndDisable[nIndex] = hWnd;
			++nIndex;
		}
		hWnd = ::GetWindow(hWnd, GW_HWNDNEXT);
	}

	// terminate the list with a NULL
	ASSERT(nIndex < nCount+1);
	m_phWndDisable[nIndex] = NULL;
}

void CFrameWnd::EndModalState()
{
	// pop one off the stack (don't undo modalness unless stack is down to zero)
	if (m_cModalStack == 0 || --m_cModalStack > 0 || m_phWndDisable == NULL)
		return;

	// enable all the windows disabled by BeginModalState
	ASSERT(m_phWndDisable != NULL);
	UINT nIndex = 0;
	while (m_phWndDisable[nIndex] != NULL)
	{
		ASSERT(m_phWndDisable[nIndex] != NULL);
		if (::IsWindow(m_phWndDisable[nIndex]))
			::EnableWindow(m_phWndDisable[nIndex], TRUE);
		++nIndex;
	}
	delete[] (void*)m_phWndDisable;
	m_phWndDisable = NULL;
}

void CFrameWnd::ShowOwnedWindows(BOOL bShow)
{
	// walk through all top-level windows
	HWND hWnd = ::GetWindow(::GetDesktopWindow(), GW_CHILD);
	while (hWnd != NULL)
	{
		CWnd* pWnd = CWnd::FromHandlePermanent(hWnd);
		if (pWnd != NULL && m_hWnd != hWnd && AfxIsDescendant(m_hWnd, hWnd))
		{
			DWORD dwStyle = ::GetWindowLong(hWnd, GWL_STYLE);
			if (!bShow && (dwStyle & (WS_VISIBLE|WS_DISABLED)) == WS_VISIBLE)
			{
				::ShowWindow(hWnd, SW_HIDE);
				pWnd->m_nFlags |= WF_TEMPHIDE;
			}
			else if (bShow && (dwStyle & (WS_VISIBLE|WS_DISABLED)) == 0 &&
				(pWnd->m_nFlags & WF_TEMPHIDE))
			{
				::ShowWindow(hWnd, SW_SHOWNOACTIVATE);
				pWnd->m_nFlags &= ~WF_TEMPHIDE;
			}
		}
		hWnd = ::GetWindow(hWnd, GW_HWNDNEXT);
	}
}

void CFrameWnd::OnEnable(BOOL bEnable)
{
	if (bEnable && (m_nFlags & WF_STAYDISABLED))
	{
		// Work around for MAPI support. This makes sure the main window
		// remains disabled even when the mail system is booting.
		EnableWindow(FALSE);
		::SetFocus(NULL);
		return;
	}

	// only for top-level (and non-owned) windows
	if (GetParent() != NULL)
		return;

	// this causes modal dialogs to be "truly modal"
	if (!bEnable && !InModalState())
	{
		ASSERT((m_nFlags & WF_MODALDISABLE) == 0);
		m_nFlags |= WF_MODALDISABLE;
		BeginModalState();
	}
	else if (bEnable && (m_nFlags & WF_MODALDISABLE))
	{
		m_nFlags &= ~WF_MODALDISABLE;
		EndModalState();

		// cause normal focus logic to kick in
		if (::GetActiveWindow() == m_hWnd)
			SendMessage(WM_ACTIVATE, WA_ACTIVE);
	}

	// force WM_NCACTIVATE because Windows may think it is unecessary
	if (bEnable && (m_nFlags & WF_STAYACTIVE))
		SendMessage(WM_NCACTIVATE, TRUE);
	// force WM_NCACTIVATE for floating windows too
	NotifyFloatingWindows(bEnable ? FS_ENABLE : FS_DISABLE);
}

void CFrameWnd::NotifyFloatingWindows(DWORD dwFlags)
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);

	// get top level parent frame window first unless this is a child window
	CFrameWnd* pParent = (GetStyle() & WS_CHILD) ? this : GetTopLevelFrame();
	ASSERT(pParent != NULL);
	if (dwFlags & (FS_DEACTIVATE|FS_ACTIVATE))
	{
		// update parent window activation state
		BOOL bActivate = !(dwFlags & FS_DEACTIVATE);
		BOOL bEnabled = pParent->IsWindowEnabled();

		if (bActivate && bEnabled && pParent != this)
		{
			// Excel will try to Activate itself when it receives a
			// WM_NCACTIVATE so we need to keep it from doing that here.
			m_nFlags |= WF_KEEPMINIACTIVE;
			pParent->SendMessage(WM_NCACTIVATE, TRUE);
			m_nFlags &= ~WF_KEEPMINIACTIVE;
		}
		else
		{
			pParent->SendMessage(WM_NCACTIVATE, FALSE);
		}
	}

	// then update the state of all floating windows owned by the parent
	HWND hWnd = ::GetWindow(::GetDesktopWindow(), GW_CHILD);
	while (hWnd != NULL)
	{
		if (AfxIsDescendant(pParent->m_hWnd, hWnd))
			::SendMessage(hWnd, WM_FLOATSTATUS, dwFlags, 0);
		hWnd = ::GetWindow(hWnd, GW_HWNDNEXT);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd second phase creation

BOOL CFrameWnd::PreCreateWindow(CREATESTRUCT& cs)
{
	if (cs.lpszClass == NULL)
	{
		VERIFY(AfxDeferRegisterClass(AFX_WNDFRAMEORVIEW_REG));
		cs.lpszClass = _afxWndFrameOrView;  // COLOR_WINDOW background
	}

	if ((cs.style & FWS_ADDTOTITLE) && afxData.bWin4)
		cs.style |= FWS_PREFIXTITLE;

	if (afxData.bWin4)
		cs.dwExStyle |= WS_EX_CLIENTEDGE;

	return TRUE;
}

BOOL CFrameWnd::Create(LPCTSTR lpszClassName,
	LPCTSTR lpszWindowName,
	DWORD dwStyle,
	const RECT& rect,
	CWnd* pParentWnd,
	LPCTSTR lpszMenuName,
	DWORD dwExStyle,
	CCreateContext* pContext)
{
	HMENU hMenu = NULL;
	if (lpszMenuName != NULL)
	{
		// load in a menu that will get destroyed when window gets destroyed
		HINSTANCE hInst = AfxFindResourceHandle(lpszMenuName, RT_MENU);
		if ((hMenu = ::LoadMenu(hInst, lpszMenuName)) == NULL)
		{
			TRACE0("Warning: failed to load menu for CFrameWnd.\n");
			PostNcDestroy();            // perhaps delete the C++ object
			return FALSE;
		}
	}

	m_strTitle = lpszWindowName;    // save title for later

	if (!CreateEx(dwExStyle, lpszClassName, lpszWindowName, dwStyle,
		rect.left, rect.top, rect.right - rect.left, rect.bottom - rect.top,
		pParentWnd->GetSafeHwnd(), hMenu, (LPVOID)pContext))
	{
		TRACE0("Warning: failed to create CFrameWnd.\n");
		if (hMenu != NULL)
			DestroyMenu(hMenu);
		return FALSE;
	}

	return TRUE;
}

CWnd* CFrameWnd::CreateView(CCreateContext* pContext, UINT nID)
{
	ASSERT(m_hWnd != NULL);
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pContext != NULL);
	ASSERT(pContext->m_pNewViewClass != NULL);

	// Note: can be a CWnd with PostNcDestroy self cleanup
	CWnd* pView = (CWnd*)pContext->m_pNewViewClass->CreateObject();
	if (pView == NULL)
	{
		TRACE1("Warning: Dynamic create of view type %hs failed.\n",
			pContext->m_pNewViewClass->m_lpszClassName);
		return NULL;
	}
	ASSERT_KINDOF(CWnd, pView);

	// views are always created with a border!
	if (!pView->Create(NULL, NULL, AFX_WS_DEFAULT_VIEW,
		CRect(0,0,0,0), this, nID, pContext))
	{
		TRACE0("Warning: could not create view for frame.\n");
		return NULL;        // can't continue without a view
	}

	if (afxData.bWin4 && (pView->GetExStyle() & WS_EX_CLIENTEDGE))
	{
		// remove the 3d style from the frame, since the view is
		//  providing it.
		// make sure to recalc the non-client area
		ModifyStyleEx(WS_EX_CLIENTEDGE, 0, SWP_FRAMECHANGED);
	}
	return pView;
}

BOOL CFrameWnd::OnCreateClient(LPCREATESTRUCT, CCreateContext* pContext)
{
	// default create client will create a view if asked for it
	if (pContext != NULL && pContext->m_pNewViewClass != NULL)
	{
		if (CreateView(pContext, AFX_IDW_PANE_FIRST) == NULL)
			return FALSE;
	}
	return TRUE;
}

int CFrameWnd::OnCreate(LPCREATESTRUCT lpcs)
{
	CCreateContext* pContext = (CCreateContext*)lpcs->lpCreateParams;
	return OnCreateHelper(lpcs, pContext);
}

int CFrameWnd::OnCreateHelper(LPCREATESTRUCT lpcs, CCreateContext* pContext)
{
	if (CWnd::OnCreate(lpcs) == -1)
		return -1;

	// create special children first
	if (!OnCreateClient(lpcs, pContext))
	{
		TRACE0("Failed to create client pane/view for frame.\n");
		return -1;
	}

	// post message for initial message string
	PostMessage(WM_SETMESSAGESTRING, AFX_IDS_IDLEMESSAGE);

	// make sure the child windows have been properly sized
	RecalcLayout();

	return 0;   // create ok
}

LPCTSTR CFrameWnd::GetIconWndClass(DWORD dwDefaultStyle, UINT nIDResource)
{
	ASSERT_VALID_IDR(nIDResource);
	HINSTANCE hInst = AfxFindResourceHandle(
		MAKEINTRESOURCE(nIDResource), RT_GROUP_ICON);
	HICON hIcon = ::LoadIcon(hInst, MAKEINTRESOURCE(nIDResource));
	if (hIcon != NULL)
	{
		CREATESTRUCT cs;
		memset(&cs, 0, sizeof(CREATESTRUCT));
		cs.style = dwDefaultStyle;
		PreCreateWindow(cs);
			// will fill lpszClassName with default WNDCLASS name
			// ignore instance handle from PreCreateWindow.

		WNDCLASS wndcls;
		if (cs.lpszClass != NULL &&
			GetClassInfo(AfxGetInstanceHandle(), cs.lpszClass, &wndcls) &&
			wndcls.hIcon != hIcon)
		{
			// register a very similar WNDCLASS
			return AfxRegisterWndClass(wndcls.style,
				wndcls.hCursor, wndcls.hbrBackground, hIcon);
		}
	}
	return NULL;        // just use the default
}

BOOL CFrameWnd::LoadFrame(UINT nIDResource, DWORD dwDefaultStyle,
	CWnd* pParentWnd, CCreateContext* pContext)
{
	// only do this once
	ASSERT_VALID_IDR(nIDResource);
	ASSERT(m_nIDHelp == 0 || m_nIDHelp == nIDResource);

	m_nIDHelp = nIDResource;    // ID for help context (+HID_BASE_RESOURCE)

	CString strFullString;
	if (strFullString.LoadString(nIDResource))
		AfxExtractSubString(m_strTitle, strFullString, 0);    // first sub-string

	VERIFY(AfxDeferRegisterClass(AFX_WNDFRAMEORVIEW_REG));

	// attempt to create the window
	LPCTSTR lpszClass = GetIconWndClass(dwDefaultStyle, nIDResource);
	LPCTSTR lpszTitle = m_strTitle;
	if (!Create(lpszClass, lpszTitle, dwDefaultStyle, rectDefault,
	  pParentWnd, MAKEINTRESOURCE(nIDResource), 0L, pContext))
	{
		return FALSE;   // will self destruct on failure normally
	}

	// save the default menu handle
	ASSERT(m_hWnd != NULL);
	m_hMenuDefault = ::GetMenu(m_hWnd);

	// load accelerator resource
	LoadAccelTable(MAKEINTRESOURCE(nIDResource));

	if (pContext == NULL)   // send initial update
		SendMessageToDescendants(WM_INITIALUPDATE, 0, 0, TRUE, TRUE);

	return TRUE;
}

void CFrameWnd::OnUpdateFrameMenu(HMENU hMenuAlt)
{
	if (hMenuAlt == NULL)
	{
		// attempt to get default menu from document
		CDocument* pDoc = GetActiveDocument();
		if (pDoc != NULL)
			hMenuAlt = pDoc->GetDefaultMenu();
		// use default menu stored in frame if none from document
		if (hMenuAlt == NULL)
			hMenuAlt = m_hMenuDefault;
	}
	// finally, set the menu
	::SetMenu(m_hWnd, hMenuAlt);
}

void CFrameWnd::InitialUpdateFrame(CDocument* pDoc, BOOL bMakeVisible)
{
	// if the frame does not have an active view, set to first pane
	CView* pView = NULL;
	if (GetActiveView() == NULL)
	{
		CWnd* pWnd = GetDescendantWindow(AFX_IDW_PANE_FIRST, TRUE);
		if (pWnd != NULL && pWnd->IsKindOf(RUNTIME_CLASS(CView)))
		{
			pView = (CView*)pWnd;
			SetActiveView(pView, FALSE);
		}
	}

	if (bMakeVisible)
	{
		// send initial update to all views (and other controls) in the frame
		SendMessageToDescendants(WM_INITIALUPDATE, 0, 0, TRUE, TRUE);

		// give view a chance to save the focus (CFormView needs this)
		if (pView != NULL)
			pView->OnActivateFrame(WA_INACTIVE, this);

		// finally, activate the frame
		// (send the default show command unless the main desktop window)
		int nCmdShow = -1;      // default
		CWinApp* pApp = AfxGetApp();
		if (pApp != NULL && pApp->m_pMainWnd == this)
		{
			nCmdShow = pApp->m_nCmdShow; // use the parameter from WinMain
			pApp->m_nCmdShow = -1; // set to default after first time
		}
		ActivateFrame(nCmdShow);
		if (pView != NULL)
			pView->OnActivateView(TRUE, pView, pView);
	}

	// update frame counts and frame title (may already have been visible)
	if (pDoc != NULL)
		pDoc->UpdateFrameCounts();
	OnUpdateFrameTitle(TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd closing down

void CFrameWnd::OnClose()
{
	if (m_lpfnCloseProc != NULL && !(*m_lpfnCloseProc)(this))
		return;

	// Note: only queries the active document
	CDocument* pDocument = GetActiveDocument();
	if (pDocument != NULL && !pDocument->CanCloseFrame(this))
	{
		// document can't close right now -- don't close it
		return;
	}
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL && pApp->m_pMainWnd == this)
	{
		// attempt to save all documents
		if (pDocument == NULL && !pApp->SaveAllModified())
			return;     // don't close it

		// hide the application's windows before closing all the documents
		pApp->HideApplication();

		// close all documents first
		pApp->CloseAllDocuments(FALSE);

		// don't exit if there are outstanding component objects
		if (!AfxOleCanExitApp())
		{
			// take user out of control of the app
			AfxOleSetUserCtrl(FALSE);

			// don't destroy the main window and close down just yet
			//  (there are outstanding component (OLE) objects)
			return;
		}

		// there are cases where destroying the documents may destroy the
		//  main window of the application.
		if (!afxContextIsDLL && pApp->m_pMainWnd == NULL)
		{
			AfxPostQuitMessage(0);
			return;
		}
	}

	// detect the case that this is the last frame on the document and
	// shut down with OnCloseDocument instead.
	if (pDocument != NULL && pDocument->m_bAutoDelete)
	{
		BOOL bOtherFrame = FALSE;
		POSITION pos = pDocument->GetFirstViewPosition();
		while (pos != NULL)
		{
			CView* pView = pDocument->GetNextView(pos);
			ASSERT_VALID(pView);
			if (pView->GetParentFrame() != this)
			{
				bOtherFrame = TRUE;
				break;
			}
		}
		if (!bOtherFrame)
		{
			pDocument->OnCloseDocument();
			return;
		}

		// allow the document to cleanup before the window is destroyed
		pDocument->PreCloseFrame(this);
	}

	// then destroy the window
	DestroyWindow();
}

void CFrameWnd::OnDestroy()
{
	DestroyDockBars();

	// reset menu to default before final shutdown
	if (m_hMenuDefault != NULL && ::GetMenu(m_hWnd) != m_hMenuDefault)
	{
		::SetMenu(m_hWnd, m_hMenuDefault);
		ASSERT(::GetMenu(m_hWnd) == m_hMenuDefault);
	}

	// Automatically quit when the main window is destroyed.
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL && pApp->m_pMainWnd == this)
	{
		// closing the main application window
		::WinHelp(m_hWnd, NULL, HELP_QUIT, 0L);

		// will call PostQuitMessage in CWnd::OnNcDestroy
	}
	CWnd::OnDestroy();
}

void CFrameWnd::RemoveControlBar(CControlBar *pBar)
{
	POSITION pos = m_listControlBars.Find(pBar);
	if (pos != NULL)
		m_listControlBars.RemoveAt(pos);
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd command/message routing

BOOL CFrameWnd::OnCmdMsg(UINT nID, int nCode, void* pExtra,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	CPushRoutingFrame push(this);

	// pump through current view FIRST
	CView* pView = GetActiveView();
	if (pView != NULL && pView->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

	// then pump through frame
	if (CWnd::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

	// last but not least, pump through app
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL && pApp->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

	return FALSE;
}

// Delegate scroll messages to active view as well
void CFrameWnd::OnHScroll(UINT, UINT, CScrollBar*)
{
	CWnd* pActiveView = GetActiveView();
	if (pActiveView != NULL)
	{
		const MSG* pMsg = GetCurrentMessage();
		pActiveView->SendMessage(WM_HSCROLL, pMsg->wParam, pMsg->lParam);
	}
}

void CFrameWnd::OnVScroll(UINT, UINT, CScrollBar*)
{
	CWnd* pActiveView = GetActiveView();
	if (pActiveView != NULL)
	{
		const MSG* pMsg = GetCurrentMessage();
		pActiveView->SendMessage(WM_VSCROLL, pMsg->wParam, pMsg->lParam);
	}
}

LRESULT CFrameWnd::OnActivateTopLevel(WPARAM wParam, LPARAM lParam)
{
	CWnd::OnActivateTopLevel(wParam, lParam);

	// exit Shift+F1 help mode on activation changes
	ExitHelpMode();

#ifndef _AFX_NO_OLE_SUPPORT
	// allow OnFrameWindowActivate to be sent to in-place items
	if (m_pNotifyHook != NULL)
	{
		// activate when active and when not minimized
		m_pNotifyHook->OnActivate(
			LOWORD(wParam) != WA_INACTIVE && !HIWORD(wParam));
	}
#endif

	// deactivate current active view
	if (AfxGetThread()->m_pMainWnd == this)
	{
		CView* pActiveView = GetActiveView();
		if (pActiveView == NULL)
			pActiveView = GetActiveFrame()->GetActiveView();
		if (pActiveView != NULL)
			pActiveView->OnActivateView(FALSE, pActiveView, pActiveView);
	}

	// force idle processing to update any key state indicators
	PostMessage(WM_KICKIDLE);

	return 0;
}

void CFrameWnd::OnActivate(UINT nState, CWnd* pWndOther, BOOL bMinimized)
{
	CWnd::OnActivate(nState, pWndOther, bMinimized);

	// get top level frame unless this is a child window
	// determine if window should be active or not
	CFrameWnd* pTopLevel = (GetStyle() & WS_CHILD) ? this : GetTopLevelFrame();
	ASSERT(pTopLevel != NULL);
	CWnd* pActive = (nState == WA_INACTIVE ? pWndOther : this);
	BOOL bStayActive =
		(pTopLevel == pActive ||
		(pTopLevel == pActive->GetTopLevelFrame() &&
		(pActive == pTopLevel ||
			pActive->SendMessage(WM_FLOATSTATUS, FS_SYNCACTIVE) != 0)));
	pTopLevel->m_nFlags &= ~WF_STAYACTIVE;
	if (bStayActive)
		pTopLevel->m_nFlags |= WF_STAYACTIVE;

	// sync floating windows to the new state
	NotifyFloatingWindows(bStayActive ? FS_ACTIVATE : FS_DEACTIVATE);

	// get active view (use active frame if no active view)
	CView* pActiveView = GetActiveView();
	if (pActiveView == NULL)
		pActiveView = GetActiveFrame()->GetActiveView();

	// when frame gets activated, re-activate current view
	if (pActiveView != NULL)
	{
		if (nState != WA_INACTIVE && !bMinimized)
			pActiveView->OnActivateView(TRUE, pActiveView, pActiveView);

		// always notify the view of frame activations
		pActiveView->OnActivateFrame(nState, this);
	}
}

BOOL CFrameWnd::OnNcActivate(BOOL bActive)
{
	// stay active if WF_STAYACTIVE bit is on
	if (m_nFlags & WF_STAYACTIVE)
		bActive = TRUE;

	// but do not stay active if the window is disabled
	if (!IsWindowEnabled())
		bActive = FALSE;

	// do not call the base class because it will call Default()
	//  and we may have changed bActive.
	return (BOOL)DefWindowProc(WM_NCACTIVATE, bActive, 0L);
}

void CFrameWnd::OnSysCommand(UINT nID, LONG lParam)
{
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);

	// set status bar as appropriate
	UINT nItemID = (nID & 0xFFF0);

	// don't interfere with system commands if not in help mode
	if (pFrameWnd->m_bHelpMode)
	{
		switch (nItemID)
		{
		case SC_SIZE:
		case SC_MOVE:
		case SC_MINIMIZE:
		case SC_MAXIMIZE:
		case SC_NEXTWINDOW:
		case SC_PREVWINDOW:
		case SC_CLOSE:
		case SC_RESTORE:
		case SC_TASKLIST:
			if (!SendMessage(WM_COMMANDHELP, 0,
			  HID_BASE_COMMAND+ID_COMMAND_FROM_SC(nItemID)))
				SendMessage(WM_COMMAND, ID_DEFAULT_HELP);
			return;
		}
	}

	// call default functionality
	CWnd::OnSysCommand(nID, lParam);
}

/////////////////////////////////////////////////////////////////////////////
// default frame processing

// default drop processing will attempt to open the file
void CFrameWnd::OnDropFiles(HDROP hDropInfo)
{
	SetActiveWindow();      // activate us first !
	UINT nFiles = ::DragQueryFile(hDropInfo, (UINT)-1, NULL, 0);

	CWinApp* pApp = AfxGetApp();
	ASSERT(pApp != NULL);
	for (UINT iFile = 0; iFile < nFiles; iFile++)
	{
		TCHAR szFileName[_MAX_PATH];
		::DragQueryFile(hDropInfo, iFile, szFileName, _MAX_PATH);
		pApp->OpenDocumentFile(szFileName);
	}
	::DragFinish(hDropInfo);
}

// query end session for main frame will attempt to close it all down
BOOL CFrameWnd::OnQueryEndSession()
{
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL && pApp->m_pMainWnd == this)
		return pApp->SaveAllModified();

	return TRUE;
}

// when Windows session ends, close all documents
void CFrameWnd::OnEndSession(BOOL bEnding)
{
	if (!bEnding)
		return;

	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL && pApp->m_pMainWnd == this)
	{
		AfxOleSetUserCtrl(TRUE);    // keeps from randomly shutting down
		pApp->CloseAllDocuments(TRUE);

		// allow application to save settings, etc.
		pApp->ExitInstance();
	}
}

/////////////////////////////////////////////////////////////////////////////
// Support for Shell DDE Execute messages

LRESULT CFrameWnd::OnDDEInitiate(WPARAM wParam, LPARAM lParam)
{
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL &&
		LOWORD(lParam) != 0 && HIWORD(lParam) != 0 &&
		(ATOM)LOWORD(lParam) == pApp->m_atomApp &&
		(ATOM)HIWORD(lParam) == pApp->m_atomSystemTopic)
	{
		// make duplicates of the incoming atoms (really adding a reference)
		TCHAR szAtomName[_MAX_PATH];
		VERIFY(GlobalGetAtomName(pApp->m_atomApp,
			szAtomName, _MAX_PATH - 1) != 0);
		VERIFY(GlobalAddAtom(szAtomName) == pApp->m_atomApp);
		VERIFY(GlobalGetAtomName(pApp->m_atomSystemTopic,
			szAtomName, _MAX_PATH - 1) != 0);
		VERIFY(GlobalAddAtom(szAtomName) == pApp->m_atomSystemTopic);

		// send the WM_DDE_ACK (caller will delete duplicate atoms)
		::SendMessage((HWND)wParam, WM_DDE_ACK, (WPARAM)m_hWnd,
			MAKELPARAM(pApp->m_atomApp, pApp->m_atomSystemTopic));
	}
	return 0L;
}

// always ACK the execute command - even if we do nothing
LRESULT CFrameWnd::OnDDEExecute(WPARAM wParam, LPARAM lParam)
{
	// unpack the DDE message
	UINT unused;
	HGLOBAL hData;
	VERIFY(UnpackDDElParam(WM_DDE_EXECUTE, lParam, &unused, (UINT*)&hData));

	// get the command string
	TCHAR szCommand[_MAX_PATH * 2];
	LPCTSTR lpsz = (LPCTSTR)GlobalLock(hData);
	lstrcpyn(szCommand, lpsz, _countof(szCommand));
	GlobalUnlock(hData);

	// acknowledge now - before attempting to execute
	::PostMessage((HWND)wParam, WM_DDE_ACK, (WPARAM)m_hWnd,
		ReuseDDElParam(lParam, WM_DDE_EXECUTE, WM_DDE_ACK,
		(UINT)0x8000, (UINT)hData));

	// don't execute the command when the window is disabled
	if (!IsWindowEnabled())
	{
		TRACE1("Warning: DDE command '%s' ignored because window is disabled.\n",
			szCommand);
		return 0;
	}

	// execute the command
	if (!AfxGetApp()->OnDDECommand(szCommand))
		TRACE1("Error: failed to execute DDE command '%s'.\n", szCommand);

	return 0L;
}

LRESULT CFrameWnd::OnDDETerminate(WPARAM wParam, LPARAM lParam)
{
	::PostMessage((HWND)wParam, WM_DDE_TERMINATE, (WPARAM)m_hWnd, lParam);
	return 0L;
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd attributes

CView* CFrameWnd::GetActiveView() const
{
	ASSERT(m_pViewActive == NULL ||
		m_pViewActive->IsKindOf(RUNTIME_CLASS(CView)));
	return m_pViewActive;
}

void CFrameWnd::SetActiveView(CView* pViewNew, BOOL bNotify)
{
#ifdef _DEBUG
	if (pViewNew != NULL)
	{
		ASSERT(IsChild(pViewNew));
		ASSERT_KINDOF(CView, pViewNew);
	}
#endif //_DEBUG

	CView* pViewOld = m_pViewActive;
	if (pViewNew == pViewOld)
		return;     // do not re-activate if SetActiveView called more than once

	m_pViewActive = NULL;   // no active for the following processing

	// deactivate the old one
	if (pViewOld != NULL)
		pViewOld->OnActivateView(FALSE, pViewNew, pViewOld);

	// if the OnActivateView moves the active window,
	//    that will veto this change
	if (m_pViewActive != NULL)
		return;     // already set
	m_pViewActive = pViewNew;

	// activate
	if (pViewNew != NULL && bNotify)
		pViewNew->OnActivateView(TRUE, pViewNew, pViewOld);
}

/////////////////////////////////////////////////////////////////////////////
// Special view swapping/activation

void CFrameWnd::OnSetFocus(CWnd* pOldWnd)
{
	if (m_pViewActive != NULL)
		m_pViewActive->SetFocus();
	else
		CWnd::OnSetFocus(pOldWnd);
}

CDocument* CFrameWnd::GetActiveDocument()
{
	ASSERT_VALID(this);
	CView* pView = GetActiveView();
	if (pView != NULL)
		return pView->GetDocument();
	return NULL;
}

void CFrameWnd::ShowControlBar(CControlBar* pBar, BOOL bShow, BOOL bDelay)
{
	ASSERT(pBar != NULL);
	CFrameWnd* pParentFrame = pBar->GetDockingFrame();
	ASSERT(pParentFrame->GetTopLevelParent() == GetTopLevelParent());
		// parent frame of bar must be related

	if (bDelay)
	{
		pBar->DelayShow(bShow);
		pParentFrame->DelayRecalcLayout();
	}
	else
	{
		pBar->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOZORDER|SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|
			(bShow ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
		// call DelayShow to clear any contradictory DelayShow
		pBar->DelayShow(bShow);
		if (bShow || !pBar->IsFloating())
			pParentFrame->RecalcLayout(FALSE);
	}

	// show or hide the floating frame as appropriate
	if (pBar->IsFloating())
	{
		int nVisCount = pBar->m_pDockBar != NULL ?
			pBar->m_pDockBar->GetDockedVisibleCount() : bShow ? 1 : 0;
		if (nVisCount == 1 && bShow)
		{
			pParentFrame->m_nShowDelay = -1;
			if (bDelay)
			{
				pParentFrame->m_nShowDelay = SW_SHOWNA;
				pParentFrame->RecalcLayout(FALSE);
			}
			else
				pParentFrame->ShowWindow(SW_SHOWNA);
		}
		else if (nVisCount == 0)
		{
			ASSERT(!bShow);
			pParentFrame->m_nShowDelay = -1;
			if (bDelay)
				pParentFrame->m_nShowDelay = SW_HIDE;
			else
				pParentFrame->ShowWindow(SW_HIDE);
		}
		else if (!bDelay)
		{
			pParentFrame->RecalcLayout(FALSE);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// Command prompts

void CFrameWnd::OnInitMenu(CMenu* pMenu)
{
#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to consume message
	if (m_pNotifyHook != NULL)
	{
#ifdef _AFXDLL
		ASSERT(m_pModuleState != NULL);
		if (m_pModuleState->m_dwVersion >= 0x423)
#endif
			m_pNotifyHook->OnInitMenu(pMenu);
#endif
	}

	Default();
}

void CFrameWnd::OnInitMenuPopup(CMenu* pMenu, UINT nIndex, BOOL bSysMenu)
{
	AfxCancelModes(m_hWnd);

	if (bSysMenu)
		return;     // don't support system menu

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to consume message
	if (m_pNotifyHook != NULL)
	{
#ifdef _AFXDLL
		ASSERT(m_pModuleState != NULL);
		if (m_pModuleState->m_dwVersion >= 0x423)
#endif
			if (m_pNotifyHook->OnInitMenuPopup(pMenu, nIndex, bSysMenu))
				return;
	}
#endif

	ASSERT(pMenu != NULL);
	// check the enabled state of various menu items

	CCmdUI state;
	state.m_pMenu = pMenu;
	ASSERT(state.m_pOther == NULL);
	ASSERT(state.m_pParentMenu == NULL);

	// determine if menu is popup in top-level menu and set m_pOther to
	//  it if so (m_pParentMenu == NULL indicates that it is secondary popup)
	HMENU hParentMenu;
	if (AfxGetThreadState()->m_hTrackingMenu == pMenu->m_hMenu)
		state.m_pParentMenu = pMenu;    // parent == child for tracking popup
	else if ((hParentMenu = ::GetMenu(m_hWnd)) != NULL)
	{
		CWnd* pParent = GetTopLevelParent();
			// child windows don't have menus -- need to go to the top!
		if (pParent != NULL &&
			(hParentMenu = ::GetMenu(pParent->m_hWnd)) != NULL)
		{
			int nIndexMax = ::GetMenuItemCount(hParentMenu);
			for (int nIndex = 0; nIndex < nIndexMax; nIndex++)
			{
				if (::GetSubMenu(hParentMenu, nIndex) == pMenu->m_hMenu)
				{
					// when popup is found, m_pParentMenu is containing menu
					state.m_pParentMenu = CMenu::FromHandle(hParentMenu);
					break;
				}
			}
		}
	}

	state.m_nIndexMax = pMenu->GetMenuItemCount();
	for (state.m_nIndex = 0; state.m_nIndex < state.m_nIndexMax;
	  state.m_nIndex++)
	{
		state.m_nID = pMenu->GetMenuItemID(state.m_nIndex);
		if (state.m_nID == 0)
			continue; // menu separator or invalid cmd - ignore it

		ASSERT(state.m_pOther == NULL);
		ASSERT(state.m_pMenu != NULL);
		if (state.m_nID == (UINT)-1)
		{
			// possibly a popup menu, route to first item of that popup
			state.m_pSubMenu = pMenu->GetSubMenu(state.m_nIndex);
			if (state.m_pSubMenu == NULL ||
				(state.m_nID = state.m_pSubMenu->GetMenuItemID(0)) == 0 ||
				state.m_nID == (UINT)-1)
			{
				continue;       // first item of popup can't be routed to
			}
			state.DoUpdate(this, FALSE);    // popups are never auto disabled
		}
		else
		{
			// normal menu item
			// Auto enable/disable if frame window has 'm_bAutoMenuEnable'
			//    set and command is _not_ a system command.
			state.m_pSubMenu = NULL;
			state.DoUpdate(this, m_bAutoMenuEnable && state.m_nID < 0xF000);
		}

		// adjust for menu deletions and additions
		UINT nCount = pMenu->GetMenuItemCount();
		if (nCount < state.m_nIndexMax)
		{
			state.m_nIndex -= (state.m_nIndexMax - nCount);
			while (state.m_nIndex < nCount &&
				pMenu->GetMenuItemID(state.m_nIndex) == state.m_nID)
			{
				state.m_nIndex++;
			}
		}
		state.m_nIndexMax = nCount;
	}
}

void CFrameWnd::OnMenuSelect(UINT nItemID, UINT nFlags, HMENU hSysMenu)
{
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to consume message
	if (m_pNotifyHook != NULL)
	{
#ifdef _AFXDLL
		ASSERT(m_pModuleState != NULL);
		if (m_pModuleState->m_dwVersion >= 0x423)
#endif
			if (m_pNotifyHook->OnMenuSelect(nItemID, nFlags, hSysMenu))
				return;
	}
#endif

	// set the tracking state (update on idle)
	if (nFlags == 0xFFFF)
	{
		// cancel menu operation (go back to idle now)
		m_nFlags &= ~WF_NOPOPMSG;
		if (!pFrameWnd->m_bHelpMode)
			m_nIDTracking = AFX_IDS_IDLEMESSAGE;
		else
			m_nIDTracking = AFX_IDS_HELPMODEMESSAGE;
		SendMessage(WM_SETMESSAGESTRING, (WPARAM)m_nIDTracking);
		ASSERT(m_nIDTracking == m_nIDLastMessage);

		// update right away
		CWnd* pWnd = GetMessageBar();
		if (pWnd != NULL)
			pWnd->UpdateWindow();
	}
	else
	{
		if (nItemID == 0 || nFlags & (MF_SEPARATOR|MF_POPUP))
		{
			// nothing should be displayed
			m_nIDTracking = 0;
		}
		else if (nItemID >= 0xF000 && nItemID < 0xF1F0) // max of 31 SC_s
		{
			// special strings table entries for system commands
			m_nIDTracking = ID_COMMAND_FROM_SC(nItemID);
			ASSERT(m_nIDTracking >= AFX_IDS_SCFIRST &&
				m_nIDTracking < AFX_IDS_SCFIRST + 31);
		}
		else if (nItemID >= AFX_IDM_FIRST_MDICHILD)
		{
			// all MDI Child windows map to the same help id
			m_nIDTracking = AFX_IDS_MDICHILD;
		}
		else
		{
			// track on idle
			m_nIDTracking = nItemID;
		}
		pFrameWnd->m_nFlags |= WF_NOPOPMSG;
	}

	// when running in-place, it is necessary to cause a message to
	//  be pumped through the queue.
	if (m_nIDTracking != m_nIDLastMessage && GetParent() != NULL)
		PostMessage(WM_KICKIDLE);
}

void CFrameWnd::GetMessageString(UINT nID, CString& rMessage) const
{
	// load appropriate string
	LPTSTR lpsz = rMessage.GetBuffer(255);
	if (AfxLoadString(nID, lpsz) != 0)
	{
		// first newline terminates actual string
		lpsz = _tcschr(lpsz, '\n');
		if (lpsz != NULL)
			*lpsz = '\0';
	}
	else
	{
		// not found
		TRACE1("Warning: no message line prompt for ID 0x%04X.\n", nID);
	}
	rMessage.ReleaseBuffer();
}

LRESULT CFrameWnd::OnPopMessageString(WPARAM wParam, LPARAM lParam)
{
	if (m_nFlags & WF_NOPOPMSG)
		return 0;

	return SendMessage(WM_SETMESSAGESTRING, wParam, lParam);
}

LRESULT CFrameWnd::OnSetMessageString(WPARAM wParam, LPARAM lParam)
{
	UINT nIDLast = m_nIDLastMessage;
	m_nFlags &= ~WF_NOPOPMSG;

	CWnd* pMessageBar = GetMessageBar();
	if (pMessageBar != NULL)
	{
		LPCTSTR lpsz = NULL;
		CString strMessage;

		// set the message bar text
		if (lParam != 0)
		{
			ASSERT(wParam == 0);    // can't have both an ID and a string
			lpsz = (LPCTSTR)lParam; // set an explicit string
		}
		else if (wParam != 0)
		{
			// map SC_CLOSE to PREVIEW_CLOSE when in print preview mode
			if (wParam == AFX_IDS_SCCLOSE && m_lpfnCloseProc != NULL)
				wParam = AFX_IDS_PREVIEW_CLOSE;

			// get message associated with the ID indicated by wParam
			GetMessageString(wParam, strMessage);
			lpsz = strMessage;
		}
		pMessageBar->SetWindowText(lpsz);

		// update owner of the bar in terms of last message selected
		CFrameWnd* pFrameWnd = pMessageBar->GetParentFrame();
		if (pFrameWnd != NULL)
		{
			pFrameWnd->m_nIDLastMessage = (UINT)wParam;
			pFrameWnd->m_nIDTracking = (UINT)wParam;
		}
	}

	m_nIDLastMessage = (UINT)wParam;    // new ID (or 0)
	m_nIDTracking = (UINT)wParam;       // so F1 on toolbar buttons work
	return nIDLast;
}

LRESULT CFrameWnd::OnHelpPromptAddr(WPARAM, LPARAM)
{
	return (LRESULT)&m_dwPromptContext;
}

CWnd* CFrameWnd::GetMessageBar()
{
	return GetDescendantWindow(AFX_IDW_STATUS_BAR, TRUE);
}

void CFrameWnd::OnEnterIdle(UINT nWhy, CWnd* pWho)
{
	CWnd::OnEnterIdle(nWhy, pWho);

	if (nWhy != MSGF_MENU || m_nIDTracking == m_nIDLastMessage)
		return;

	SetMessageText(m_nIDTracking);
	ASSERT(m_nIDTracking == m_nIDLastMessage);
}

void CFrameWnd::SetMessageText(LPCTSTR lpszText)
{
	SendMessage(WM_SETMESSAGESTRING, 0, (LPARAM)lpszText);
}

void CFrameWnd::SetMessageText(UINT nID)
{
	SendMessage(WM_SETMESSAGESTRING, (WPARAM)nID);
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd standard control bar management

void CFrameWnd::DestroyDockBars()
{
	// create a list of all the dock bars
	// this is necessary because m_listControlBars will change
	// as the dock bars and floating frames are destroyed
	CPtrList listDockBars;
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CDockBar* pDockBar = (CDockBar*)m_listControlBars.GetNext(pos);
		ASSERT(pDockBar != NULL);
		if (pDockBar->IsDockBar())
			listDockBars.AddTail(pDockBar);
	}
	pos = listDockBars.GetHeadPosition();
	while (pos != NULL)
	{
		CDockBar* pDockBar = (CDockBar*)listDockBars.GetNext(pos);
		if (pDockBar->m_bFloating)
		{
			CFrameWnd* pFrameWnd = pDockBar->GetParentFrame();
			ASSERT_VALID(pFrameWnd);
			pFrameWnd->DestroyWindow();
		}
		else
			pDockBar->DestroyWindow();
	}
}

CControlBar* CFrameWnd::GetControlBar(UINT nID)
{
	if (nID == 0)
		return NULL;
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CControlBar* pBar = (CControlBar*)m_listControlBars.GetNext(pos);
		ASSERT(pBar != NULL);
		if (_AfxGetDlgCtrlID(pBar->m_hWnd) == nID)
		{
			ASSERT_KINDOF(CControlBar, pBar);
			return pBar;
		}
	}
	return NULL;
}

void CFrameWnd::OnUpdateControlBarMenu(CCmdUI* pCmdUI)
{
	ASSERT(ID_VIEW_STATUS_BAR == AFX_IDW_STATUS_BAR);
	ASSERT(ID_VIEW_TOOLBAR == AFX_IDW_TOOLBAR);
	ASSERT(ID_VIEW_REBAR == AFX_IDW_REBAR);

	CControlBar* pBar = GetControlBar(pCmdUI->m_nID);
	if (pBar != NULL)
	{
		pCmdUI->SetCheck((pBar->GetStyle() & WS_VISIBLE) != 0);
		return;
	}
	pCmdUI->ContinueRouting();
}

BOOL CFrameWnd::OnBarCheck(UINT nID)
{
	ASSERT(ID_VIEW_STATUS_BAR == AFX_IDW_STATUS_BAR);
	ASSERT(ID_VIEW_TOOLBAR == AFX_IDW_TOOLBAR);
	ASSERT(ID_VIEW_REBAR == AFX_IDW_REBAR);

	CControlBar* pBar = GetControlBar(nID);
	if (pBar != NULL)
	{
		ShowControlBar(pBar, (pBar->GetStyle() & WS_VISIBLE) == 0, FALSE);
		return TRUE;
	}
	return FALSE;
}

BOOL CFrameWnd::OnToolTipText(UINT, NMHDR* pNMHDR, LRESULT* pResult)
{
	ASSERT(pNMHDR->code == TTN_NEEDTEXTA || pNMHDR->code == TTN_NEEDTEXTW);

	// need to handle both ANSI and UNICODE versions of the message
	TOOLTIPTEXTA* pTTTA = (TOOLTIPTEXTA*)pNMHDR;
	TOOLTIPTEXTW* pTTTW = (TOOLTIPTEXTW*)pNMHDR;
	TCHAR szFullText[256];
	CString strTipText;
	UINT nID = pNMHDR->idFrom;
	if (pNMHDR->code == TTN_NEEDTEXTA && (pTTTA->uFlags & TTF_IDISHWND) ||
		pNMHDR->code == TTN_NEEDTEXTW && (pTTTW->uFlags & TTF_IDISHWND))
	{
		// idFrom is actually the HWND of the tool
		nID = _AfxGetDlgCtrlID((HWND)nID);
	}

	if (nID != 0) // will be zero on a separator
	{
		// don't handle the message if no string resource found
		if (AfxLoadString(nID, szFullText) == 0)
			return FALSE;

		// this is the command id, not the button index
		AfxExtractSubString(strTipText, szFullText, 1, '\n');
	}
#ifndef _UNICODE
	if (pNMHDR->code == TTN_NEEDTEXTA)
		lstrcpyn(pTTTA->szText, strTipText, _countof(pTTTA->szText));
	else
		_mbstowcsz(pTTTW->szText, strTipText, _countof(pTTTW->szText));
#else
	if (pNMHDR->code == TTN_NEEDTEXTA)
		_wcstombsz(pTTTA->szText, strTipText, _countof(pTTTA->szText));
	else
		lstrcpyn(pTTTW->szText, strTipText, _countof(pTTTW->szText));
#endif
	*pResult = 0;

	// bring the tooltip window above other popup windows
	::SetWindowPos(pNMHDR->hwndFrom, HWND_TOP, 0, 0, 0, 0,
		SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOMOVE|SWP_NOOWNERZORDER);

	return TRUE;    // message was handled
}

/////////////////////////////////////////////////////////////////////////////
// Support for standard status bar

void CFrameWnd::OnUpdateKeyIndicator(CCmdUI* pCmdUI)
{
	UINT nVK;
	UINT flag = 0x0001;

	switch (pCmdUI->m_nID)
	{
	case ID_INDICATOR_CAPS:
		nVK = VK_CAPITAL;
		break;

	case ID_INDICATOR_NUM:
		nVK = VK_NUMLOCK;
		break;

	case ID_INDICATOR_SCRL:
		nVK = VK_SCROLL;
		break;

	case ID_INDICATOR_KANA:
		nVK = VK_KANA;
		// WINBUG: Special case for Windows 3.x.  The wrong bit was toggled
		// in those systems so this must be special cased.  This is fixed
		// on systems whose version is 4.x or greater.
		if (!afxData.bWin4)
			flag = 0x8000;
		break;

	default:
		TRACE1("Warning: OnUpdateKeyIndicator - unknown indicator 0x%04X.\n",
			pCmdUI->m_nID);
		pCmdUI->ContinueRouting();
		return; // not for us
	}

	pCmdUI->Enable(::GetKeyState(nVK) & flag);
		// enable static text based on toggled key state
	ASSERT(pCmdUI->m_bEnableChanged);
}

void CFrameWnd::OnUpdateContextHelp(CCmdUI* pCmdUI)
{
	if (AfxGetMainWnd() == this)
		pCmdUI->SetCheck(!!m_bHelpMode);
	else
		pCmdUI->ContinueRouting();
}

/////////////////////////////////////////////////////////////////////////////
// Setting title of frame window - UISG standard

void CFrameWnd::OnUpdateFrameTitle(BOOL bAddToTitle)
{
	if ((GetStyle() & FWS_ADDTOTITLE) == 0)
		return;     // leave it alone!

#ifndef _AFX_NO_OLE_SUPPORT
	// allow hook to set the title (used for OLE support)
	if (m_pNotifyHook != NULL && m_pNotifyHook->OnUpdateFrameTitle())
		return;
#endif

	CDocument* pDocument = GetActiveDocument();
	if (bAddToTitle && pDocument != NULL)
		UpdateFrameTitleForDocument(pDocument->GetTitle());
	else
		UpdateFrameTitleForDocument(NULL);
}

void CFrameWnd::UpdateFrameTitleForDocument(LPCTSTR lpszDocName)
{
	// copy first part of title loaded at time of frame creation
	TCHAR szText[256+_MAX_PATH];

	if (GetStyle() & FWS_PREFIXTITLE)
	{
		szText[0] = '\0';   // start with nothing

		// get name of currently active view
		if (lpszDocName != NULL)
		{
			lstrcpy(szText, lpszDocName);
			// add current window # if needed
			if (m_nWindow > 0)
				wsprintf(szText + lstrlen(szText), _T(":%d"), m_nWindow);
			lstrcat(szText, _T(" - "));
		}
		lstrcat(szText, m_strTitle);
	}
	else
	{
		// get name of currently active view
		lstrcpy(szText, m_strTitle);
		if (lpszDocName != NULL)
		{
			lstrcat(szText, _T(" - "));
			lstrcat(szText, lpszDocName);
			// add current window # if needed
			if (m_nWindow > 0)
				wsprintf(szText + lstrlen(szText), _T(":%d"), m_nWindow);
		}
	}

	// set title if changed, but don't remove completely
	// Note: will be excessive for MDI Frame with maximized child
	AfxSetWindowText(m_hWnd, szText);
}

/////////////////////////////////////////////////////////////////////////////

void CFrameWnd::OnSetPreviewMode(BOOL bPreview, CPrintPreviewState* pState)
{
	// default implementation changes control bars, menu and main pane window

#ifndef _AFX_NO_OLE_SUPPORT
	CFrameWnd* pActiveFrame = GetActiveFrame();
	ASSERT_VALID(pActiveFrame);
	if (bPreview && pActiveFrame->m_pNotifyHook != NULL)
		pActiveFrame->m_pNotifyHook->OnDocActivate(FALSE);
#endif

	// Set visibility of standard ControlBars (only the first 32)
	DWORD dwOldStates = 0;
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CControlBar* pBar = (CControlBar*)m_listControlBars.GetNext(pos);
		ASSERT_VALID(pBar);
		UINT nID = _AfxGetDlgCtrlID(pBar->m_hWnd);
		if (nID >= AFX_IDW_CONTROLBAR_FIRST && nID <= AFX_IDW_CONTROLBAR_FIRST+31)
		{
			DWORD dwMask = 1L << (nID - AFX_IDW_CONTROLBAR_FIRST);
			if (pBar->IsVisible())
				dwOldStates |= dwMask;      // save if previously visible
			if (!pBar->IsDockBar() || nID != AFX_IDW_DOCKBAR_FLOAT)
				ShowControlBar(pBar, (pState->dwStates & dwMask), TRUE);
		}
	}
	pState->dwStates = dwOldStates; // save for restore

	if (bPreview)
	{
		// Entering Print Preview
		ASSERT(m_lpfnCloseProc == NULL);    // no chaining
		m_lpfnCloseProc = pState->lpfnCloseProc;

		// show any modeless dialogs, popup windows, float tools, etc
		ShowOwnedWindows(FALSE);

		// Hide the main pane
		HWND hWnd = ::GetDlgItem(m_hWnd, pState->nIDMainPane);
		ASSERT(hWnd != NULL);       // must be one that we are hiding!
		::ShowWindow(hWnd, SW_HIDE);

		// Get rid of the menu first (will resize the window)
		pState->hMenu = ::GetMenu(m_hWnd);
		if (pState->hMenu != NULL)
		{
			// Invalidate before SetMenu since we are going to replace
			//  the frame's client area anyway
			Invalidate();
			SetMenu(NULL);
			m_nIdleFlags &= ~idleMenu;  // avoid any idle menu processing
		}

		// Save the accelerator table and remove it.
		pState->hAccelTable = m_hAccelTable;
		m_hAccelTable = NULL;
		LoadAccelTable(MAKEINTRESOURCE(AFX_IDR_PREVIEW_ACCEL));

		// Make room for the PreviewView by changing AFX_IDW_PANE_FIRST's ID
		//  to AFX_IDW_PREVIEW_FIRST
		if (pState->nIDMainPane != AFX_IDW_PANE_FIRST)
			hWnd = ::GetDlgItem(m_hWnd, AFX_IDW_PANE_FIRST);
		if (hWnd != NULL)
			_AfxSetDlgCtrlID(hWnd, AFX_IDW_PANE_SAVE);

#ifdef _DEBUG
		if ((::GetWindowLong(m_hWnd, GWL_STYLE) & (WS_HSCROLL|WS_VSCROLL)) != 0)
			TRACE0("Warning: scroll bars in frame windows may cause unusual behaviour.\n");
#endif
	}
	else
	{
		// Leaving Preview
		m_lpfnCloseProc = NULL;

		// shift original AFX_IDW_PANE_FIRST back to its rightful ID
		HWND hWnd = ::GetDlgItem(m_hWnd, AFX_IDW_PANE_SAVE);
		if (hWnd != NULL)
		{
			HWND hWndTemp = ::GetDlgItem(m_hWnd, AFX_IDW_PANE_FIRST);
			if (hWndTemp != NULL)
				_AfxSetDlgCtrlID(hWndTemp, AFX_IDW_PANE_SAVE);
			_AfxSetDlgCtrlID(hWnd, AFX_IDW_PANE_FIRST);
		}

		// put the menu back in place if it was removed before
		if (pState->hMenu != NULL)
		{
			// Invalidate before SetMenu since we are going to replace
			//  the frame's client area anyway
			Invalidate();
			::SetMenu(m_hWnd, pState->hMenu);
		}

		// recalc layout now, before showing the main pane
#ifndef _AFX_NO_OLE_SUPPORT
		if (pActiveFrame->m_pNotifyHook != NULL)
			pActiveFrame->m_pNotifyHook->OnDocActivate(TRUE);
#endif
		RecalcLayout();

		// now show main pane that was hidden
		if (pState->nIDMainPane != AFX_IDW_PANE_FIRST)
			hWnd = ::GetDlgItem(m_hWnd, pState->nIDMainPane);
		ASSERT(hWnd != NULL);
		::ShowWindow(hWnd, SW_SHOW);

		// Restore the Accelerator table
		m_hAccelTable = pState->hAccelTable;

		// show any modeless dialogs, popup windows, float tools, etc
		ShowOwnedWindows(TRUE);
	}
}

void CFrameWnd::DelayUpdateFrameMenu(HMENU hMenuAlt)
{
	m_hMenuAlt = hMenuAlt;
	m_nIdleFlags |= idleMenu;
}

void CFrameWnd::OnIdleUpdateCmdUI()
{
	// update menu if necessary
	if (m_nIdleFlags & idleMenu)
		OnUpdateFrameMenu(m_hMenuAlt);

	// update title if necessary
	if (m_nIdleFlags & idleTitle)
		OnUpdateFrameTitle(TRUE);

	// recalc layout if necessary
	if (m_nIdleFlags & idleLayout)
	{
		RecalcLayout(m_nIdleFlags & idleNotify);
		UpdateWindow();
	}

	// set the current message string if necessary
	if (m_nIDTracking != m_nIDLastMessage)
	{
		SetMessageText(m_nIDTracking);
		ASSERT(m_nIDTracking == m_nIDLastMessage);
	}
	m_nIdleFlags = 0;
}

CFrameWnd* CFrameWnd::GetActiveFrame()
{
	// by default, the active frame is the frame itself (MDI is different)
	return this;
}

void CFrameWnd::RecalcLayout(BOOL bNotify)
{
	if (m_bInRecalcLayout)
		return;

	m_bInRecalcLayout = TRUE;
	// clear idle flags for recalc layout if called elsewhere
	if (m_nIdleFlags & idleNotify)
		bNotify = TRUE;
	m_nIdleFlags &= ~(idleLayout|idleNotify);

#ifndef _AFX_NO_OLE_SUPPORT
	// call the layout hook -- OLE support uses this hook
	if (bNotify && m_pNotifyHook != NULL)
		m_pNotifyHook->OnRecalcLayout();
#endif

	// reposition all the child windows (regardless of ID)
	if (GetStyle() & FWS_SNAPTOBARS)
	{
		CRect rect(0, 0, 32767, 32767);
		RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposQuery,
			&rect, &rect, FALSE);
		RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposExtra,
			&m_rectBorder, &rect, TRUE);
		CalcWindowRect(&rect);
		SetWindowPos(NULL, 0, 0, rect.Width(), rect.Height(),
			SWP_NOACTIVATE|SWP_NOMOVE|SWP_NOZORDER);
	}
	else
		RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposExtra, &m_rectBorder);
	m_bInRecalcLayout = FALSE;
}

// CFrameWnd implementation of OLE border space negotiation
BOOL CFrameWnd::NegotiateBorderSpace(UINT nBorderCmd, LPRECT lpRectBorder)
{
	CRect border, request;

	switch (nBorderCmd)
	{
	case borderGet:
		ASSERT(lpRectBorder != NULL);
		RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, reposQuery,
			lpRectBorder);
		break;

	case borderRequest:
		return TRUE;

	case borderSet:
		if (lpRectBorder == NULL)
		{
			if (!m_rectBorder.IsRectNull())
			{
				// releasing all border space -- recalc needed
				m_rectBorder.SetRectEmpty();
				return TRUE;
			}
			// original rect is empty & lpRectBorder is NULL, no recalc needed
			return FALSE;
		}
		if (!::EqualRect(m_rectBorder, lpRectBorder))
		{
			// the rects are different -- recalc needed
			m_rectBorder.CopyRect(lpRectBorder);
			return TRUE;
		}
		return FALSE;   // no recalc needed

	default:
		ASSERT(FALSE);  // invalid CFrameWnd::BorderCmd
	}

	return TRUE;
}

void CFrameWnd::OnSize(UINT nType, int cx, int cy)
{
	CWnd::OnSize(nType, cx, cy);    // important for MDI Children
	if (nType != SIZE_MINIMIZED)
		RecalcLayout();
}

BOOL CFrameWnd::OnEraseBkgnd(CDC* pDC)
{
	if (m_pViewActive != NULL)
		return TRUE;        // active view will erase/paint itself
	// for view-less frame just use the default background fill
	return CWnd::OnEraseBkgnd(pDC);
}

LRESULT CFrameWnd::OnRegisteredMouseWheel(WPARAM wParam, LPARAM lParam)
{
	// convert from MSH_MOUSEWHEEL to WM_MOUSEWHEEL

	WORD keyState = 0;
	keyState |= (::GetKeyState(VK_CONTROL) < 0) ? MK_CONTROL : 0;
	keyState |= (::GetKeyState(VK_SHIFT) < 0) ? MK_SHIFT : 0;

	LRESULT lResult;
	HWND hwFocus = ::GetFocus();
	const HWND hwDesktop = ::GetDesktopWindow();

	if (hwFocus == NULL)
		lResult = SendMessage(WM_MOUSEWHEEL, (wParam << 16) | keyState, lParam);
	else
	{
		do {
			lResult = ::SendMessage(hwFocus, WM_MOUSEWHEEL,
				(wParam << 16) | keyState, lParam);
			hwFocus = ::GetParent(hwFocus);
		}
		while (lResult == 0 && hwFocus != NULL && hwFocus != hwDesktop);
	}
	return lResult;
}

void CFrameWnd::ActivateFrame(int nCmdShow)
	// nCmdShow is the normal show mode this frame should be in
{
	// translate default nCmdShow (-1)
	if (nCmdShow == -1)
	{
		if (!IsWindowVisible())
			nCmdShow = SW_SHOWNORMAL;
		else if (IsIconic())
			nCmdShow = SW_RESTORE;
	}

	// bring to top before showing
	BringToTop(nCmdShow);

	if (nCmdShow != -1)
	{
		// show the window as specified
		ShowWindow(nCmdShow);

		// and finally, bring to top after showing
		BringToTop(nCmdShow);
	}
}

void CFrameWnd::BringToTop(int nCmdShow)
{
	// place the window on top except for certain nCmdShow
	if (nCmdShow != SW_HIDE &&
		nCmdShow != SW_MINIMIZE && nCmdShow != SW_SHOWMINNOACTIVE &&
		nCmdShow != SW_SHOWNA && nCmdShow != SW_SHOWNOACTIVATE)
	{
		// if no last active popup, it will return m_hWnd
		HWND hWndLastPop = ::GetLastActivePopup(m_hWnd);
		::BringWindowToTop(hWndLastPop);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CFrameWnd Diagnostics

#ifdef _DEBUG
void CFrameWnd::AssertValid() const
{
	CWnd::AssertValid();
	if (m_pViewActive != NULL)
		ASSERT_VALID(m_pViewActive);
}

void CFrameWnd::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);

	dc << "m_hAccelTable = " << (UINT)m_hAccelTable;
	dc << "\nm_nWindow = " << m_nWindow;
	dc << "\nm_nIDHelp = " << m_nIDHelp;
	dc << "\nm_nIDTracking = " << m_nIDTracking;
	dc << "\nm_nIDLastMessage = " << m_nIDLastMessage;
	if (m_pViewActive != NULL)
		dc << "\nwith active view: " << m_pViewActive;
	else
		dc << "\nno active view";

	dc << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CControlBar segmentation

CFrameWnd* CControlBar::GetDockingFrame() const
{
	CFrameWnd* pFrameWnd = GetParentFrame();
	if (pFrameWnd == NULL)
		pFrameWnd = m_pDockSite;

	ASSERT(pFrameWnd != NULL);
	ASSERT_KINDOF(CFrameWnd, pFrameWnd);
	return pFrameWnd;
}

BOOL CControlBar::IsFloating() const
{
	if (IsDockBar())
		return ((CDockBar*)this)->m_bFloating;
	else
		return m_pDockBar != NULL && m_pDockBar->m_bFloating;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

// in this file for IsKindOf library granularity (IsKindOf references these)
IMPLEMENT_DYNCREATE(CFrameWnd, CWnd)
IMPLEMENT_DYNAMIC(CView, CWnd)
IMPLEMENT_DYNAMIC(CControlBar, CWnd)

/////////////////////////////////////////////////////////////////////////////
