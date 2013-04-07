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
// Basic Help support

void CWnd::OnHelp()  // use context to derive help context
{
	// attempt to get help from whoever is tracking
	HWND hWnd = ::GetCapture();
	while (hWnd != NULL)
	{
		// attempt to process help
		if (::SendMessage(hWnd, WM_COMMANDHELP, 0, 0))
			return;

		// check next parent/owner in the parent/owner chain
		hWnd = AfxGetParentOwner(hWnd);
	}
	// attempt to get help from whoever has the focus
	hWnd = ::GetFocus();
	while (hWnd != NULL)
	{
		// attempt to process help
		if (::SendMessage(hWnd, WM_COMMANDHELP, 0, 0))
			return;

		// check next parent/owner in the parent/owner chain
		hWnd = AfxGetParentOwner(hWnd);
	}
	// attempt to get help from the active window
	CWnd* pWnd = GetTopLevelParent();
	hWnd = ::GetLastActivePopup(pWnd->GetSafeHwnd());
	while (hWnd != NULL)
	{
		// attempt to process help
		if (::SendMessage(hWnd, WM_COMMANDHELP, 0, 0))
			return;

		// check next parent/owner in the parent/owner chain
		hWnd = AfxGetParentOwner(hWnd);
	}
	// No context available, bring up default.
	SendMessage(WM_COMMAND, ID_DEFAULT_HELP);
}

void CFrameWnd::OnHelp()
{
	// Be careful not call WinHelp when the error is failing to lauch help
	if (m_dwPromptContext != 0)
	{
		if (m_dwPromptContext != HID_BASE_PROMPT+AFX_IDP_FAILED_TO_LAUNCH_HELP)
			AfxGetApp()->WinHelp(m_dwPromptContext);
		return;
	}
	CWnd::OnHelp();
}

void CWnd::OnHelpIndex()
{
	AfxGetApp()->WinHelp(0L, HELP_INDEX);
}

void CWnd::OnHelpFinder()
{
	AfxGetApp()->WinHelp(0L, HELP_FINDER);
}

void CWnd::OnHelpUsing()
{
	AfxGetApp()->WinHelp(0L, HELP_HELPONHELP);
}

/////////////////////////////////////////////////////////////////////////////
// Context Help Mode support

BOOL CFrameWnd::CanEnterHelpMode()
{
	ASSERT(m_bHelpMode != HELP_ACTIVE); // already in help mode?

	// unable to start help if the cursor cannot be loaded from the resources
	if (afxData.hcurHelp == NULL)
	{
		afxData.hcurHelp = ::LoadCursor(NULL, IDC_HELP);
		if (afxData.hcurHelp == NULL)
		{
			// load help cursor after handles have been setup
			HINSTANCE hInst = AfxFindResourceHandle(
				MAKEINTRESOURCE(AFX_IDC_CONTEXTHELP), RT_GROUP_CURSOR);
			afxData.hcurHelp = LoadCursor(hInst,
				MAKEINTRESOURCE(AFX_IDC_CONTEXTHELP));
		}
		if (afxData.hcurHelp == NULL)
			return FALSE;
	}

	// return TRUE if there is a handler for ID_CONTEXT_HELP
	AFX_CMDHANDLERINFO info;
	return OnCmdMsg(ID_CONTEXT_HELP, CN_COMMAND, NULL, &info);
}

void CFrameWnd::OnContextHelp()
{
	// don't enter twice, and don't enter if initialization fails
	if (m_bHelpMode == HELP_ACTIVE || !CanEnterHelpMode())
		return;

	// don't enter help mode with pending WM_EXITHELPMODE message
	MSG msg;
	if (PeekMessage(&msg, m_hWnd, WM_EXITHELPMODE, WM_EXITHELPMODE,
		PM_REMOVE|PM_NOYIELD))
	{
		return;
	}

	BOOL bHelpMode = m_bHelpMode;
	ASSERT(m_bHelpMode == HELP_INACTIVE || m_bHelpMode == HELP_ENTERING);
	m_bHelpMode = HELP_ACTIVE;

#ifndef _AFX_NO_OLE_SUPPORT
	// allow any in-place active servers to go into help mode
	if (bHelpMode != HELP_ENTERING && m_pNotifyHook != NULL &&
		!m_pNotifyHook->OnContextHelp(TRUE))
	{
		TRACE0("Error: an in-place server failed to enter context help mode.\n");
		m_pNotifyHook->OnContextHelp(FALSE);    // undo partial help mode
		m_bHelpMode = HELP_INACTIVE;
		return;
	}
#endif

	if (bHelpMode == HELP_INACTIVE)
	{
		// need to delay help startup until later
		PostMessage(WM_COMMAND, ID_CONTEXT_HELP);
		m_bHelpMode = HELP_ENTERING;
		return;
	}

	ASSERT(m_bHelpMode == HELP_ACTIVE);

	// display special help mode message on status bar
	UINT nMsgSave = (UINT)SendMessage(WM_SETMESSAGESTRING,
		(WPARAM)AFX_IDS_HELPMODEMESSAGE);
	if (nMsgSave == 0)
		nMsgSave = AFX_IDS_IDLEMESSAGE;

	DWORD   dwContext = 0;
	POINT   point;

	GetCursorPos(&point);
	SetHelpCapture(point, NULL);
	LONG lIdleCount = 0;
	CWinApp* pApp = AfxGetApp();

	while (m_bHelpMode)
	{
		if (PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
		{
			if (!ProcessHelpMsg(msg, &dwContext))
				break;
			ASSERT(dwContext == 0);
		}
		else if (!pApp->OnIdle(lIdleCount++))
		{
			lIdleCount = 0;
			WaitMessage();
		}
	}

	m_bHelpMode = HELP_INACTIVE;
	ReleaseCapture();

	// make sure the cursor is set appropriately
	SetCapture();
	ReleaseCapture();

	// restore original status bar text
	SendMessage(WM_SETMESSAGESTRING, (WPARAM)nMsgSave);

#ifndef _AFX_NO_OLE_SUPPORT
	// tell in-place servers to exit Shift+F1 help mode
	if (m_pNotifyHook != NULL)
		m_pNotifyHook->OnContextHelp(FALSE);
#endif

	if (dwContext != 0)
	{
		if (dwContext == -1)
			SendMessage(WM_COMMAND, ID_DEFAULT_HELP);
		else
			pApp->WinHelp(dwContext);
	}
	PostMessage(WM_KICKIDLE);    // trigger idle update
}

/////////////////////////////////////////////////////////////////////////////
// OnContextHelp helpers.

HWND CFrameWnd::SetHelpCapture(POINT point, BOOL* pbDescendant)
	// set or release capture, depending on where the mouse is
	// also assign the proper cursor to be displayed.
{
	if (!m_bHelpMode)
		return NULL;

	HWND hWndCapture = ::GetCapture();
	CWnd* pWndHit = WindowFromPoint(point);
	HWND hWndHit = pWndHit->GetSafeHwnd();
	CWnd* pTopHit = pWndHit->GetTopLevelParent();
	CWnd* pTopActive = GetActiveWindow()->GetTopLevelParent();
	BOOL bDescendant = FALSE;
	HTASK hCurTask = (HTASK)GetCurrentThreadId();
	HTASK hTaskHit = hWndHit != NULL ? ::GetWindowTask(hWndHit) : NULL;

	if (pTopActive == NULL || hWndHit == ::GetDesktopWindow())
	{
		if (hWndCapture == m_hWnd)
			ReleaseCapture();
		SetCursor(afxData.hcurArrow);
	}
	else if (pTopActive == NULL ||
		hWndHit == NULL || hCurTask != hTaskHit ||
		!AfxIsDescendant(m_hWnd, hWndHit))
	{
		if (hCurTask != hTaskHit)
			hWndHit = NULL;
		if (hWndCapture == m_hWnd)
			ReleaseCapture();
	}
	else
	{
		bDescendant = TRUE;
		if (pTopActive != pTopHit)
			hWndHit = NULL;
		else
		{
			if (hWndCapture != m_hWnd)
				::SetCapture(m_hWnd);
			SetCursor(afxData.hcurHelp);
		}
	}
	if (pbDescendant != NULL)
		*pbDescendant = bDescendant;
	return hWndHit;
}

AFX_STATIC DWORD AFXAPI _AfxMapClientArea(HWND hWnd, POINT point)
{
	DWORD dwContext;

	do
	{
		ASSERT(::IsWindow(hWnd));

		// check current window
		::ScreenToClient(hWnd, &point);
		dwContext = ::SendMessage(hWnd, WM_HELPHITTEST, 0,
			MAKELONG(point.x, point.y));
		::ClientToScreen(hWnd, &point);

		// don't use owner's of popup windows, just child/parent relationship
		if ((GetWindowLong(hWnd, GWL_STYLE) & WS_CHILD) == 0)
			break;
		// check parent window
		hWnd = ::GetParent(hWnd);
	}
	while (hWnd && dwContext == 0);

	return dwContext == 0 ? -1 : dwContext;
}

AFX_STATIC DWORD AFXAPI _AfxMapNonClientArea(int iHit)
{
	ASSERT(iHit != HTCLIENT);

	if (iHit < 0 || iHit > HTHELP)
		return (DWORD)-1;

	return HID_BASE_NCAREAS+iHit;
}

BOOL CFrameWnd::ProcessHelpMsg(MSG& msg, DWORD* pContext)
{
	ASSERT(pContext != NULL);

	if (msg.message == WM_EXITHELPMODE ||
		(msg.message == WM_KEYDOWN && msg.wParam == VK_ESCAPE))
	{
		PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE);
		return FALSE;
	}

	CPoint point;
	if ((msg.message >= WM_MOUSEFIRST && msg.message <= WM_MOUSELAST) ||
		(msg.message >= WM_NCMOUSEFIRST && msg.message <= WM_NCMOUSELAST))
	{
		BOOL bDescendant;
		HWND hWndHit = SetHelpCapture(msg.pt, &bDescendant);
		if (hWndHit == NULL)
			return TRUE;

		if (bDescendant)
		{
			if (msg.message != WM_LBUTTONDOWN)
			{
				// Hit one of our owned windows -- eat the message.
				PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE);
				return TRUE;
			}
			int iHit = (int)::SendMessage(hWndHit, WM_NCHITTEST, 0,
				MAKELONG(msg.pt.x, msg.pt.y));
			if (iHit == HTMENU || iHit == HTSYSMENU)
			{
				ASSERT(::GetCapture() == m_hWnd);
				ReleaseCapture();
				// the message we peeked changes into a non-client because
				// of the release capture.
				GetMessage(&msg, NULL, WM_NCLBUTTONDOWN, WM_NCLBUTTONDOWN);
				DispatchMessage(&msg);
				GetCursorPos(&point);
				SetHelpCapture(point, NULL);
			}
			else if (iHit == HTCLIENT)
			{
				*pContext = _AfxMapClientArea(hWndHit, msg.pt);
				PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE);
				return FALSE;
			}
			else
			{
				*pContext = _AfxMapNonClientArea(iHit);
				PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE);
				return FALSE;
			}
		}
		else
		{
			// Hit one of our apps windows (or desktop) -- dispatch the message.
			PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE);

			// Dispatch mouse messages that hit the desktop!
			DispatchMessage(&msg);
		}
	}
	else if (msg.message == WM_SYSCOMMAND ||
			 (msg.message >= WM_KEYFIRST && msg.message <= WM_KEYLAST))
	{
		if (::GetCapture() != NULL)
		{
			ReleaseCapture();
			MSG msg;
			while (PeekMessage(&msg, NULL, WM_MOUSEFIRST,
				WM_MOUSELAST, PM_REMOVE|PM_NOYIELD));
		}
		if (PeekMessage(&msg, NULL, msg.message, msg.message, PM_NOREMOVE))
		{
			GetMessage(&msg, NULL, msg.message, msg.message);
			if (!PreTranslateMessage(&msg))
			{
				TranslateMessage(&msg);
				if (msg.message == WM_SYSCOMMAND ||
				  (msg.message >= WM_SYSKEYFIRST &&
					msg.message <= WM_SYSKEYLAST))
				{
					// only dispatch system keys and system commands
					ASSERT(msg.message == WM_SYSCOMMAND ||
						 (msg.message >= WM_SYSKEYFIRST &&
						  msg.message <= WM_SYSKEYLAST));
					DispatchMessage(&msg);
				}
			}
		}
		GetCursorPos(&point);
		SetHelpCapture(point, NULL);
	}
	else
	{
		// allow all other messages to go through (capture still set)
		if (PeekMessage(&msg, NULL, msg.message, msg.message, PM_REMOVE))
			DispatchMessage(&msg);
	}

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
