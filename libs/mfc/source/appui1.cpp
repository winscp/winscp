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
// CWinApp UI related functions

void CWinApp::EnableModeless(BOOL bEnable)
{
#ifdef _AFX_NO_OLE_SUPPORT
	UNUSED(bEnable);
#endif

	// no-op if main window is NULL or not a CFrameWnd
	CWnd* pMainWnd = AfxGetMainWnd();
	if (pMainWnd == NULL || !pMainWnd->IsFrameWnd())
		return;

#ifndef _AFX_NO_OLE_SUPPORT
	// check if notify hook installed
	ASSERT_KINDOF(CFrameWnd, pMainWnd);
	CFrameWnd* pFrameWnd = (CFrameWnd*)pMainWnd;
	if (pFrameWnd->m_pNotifyHook != NULL)
		pFrameWnd->m_pNotifyHook->OnEnableModeless(bEnable);
#endif
}

int CWinApp::DoMessageBox(LPCTSTR lpszPrompt, UINT nType, UINT nIDPrompt)
{
	// disable windows for modal dialog
	EnableModeless(FALSE);
	HWND hWndTop;
	HWND hWnd = CWnd::GetSafeOwner_(NULL, &hWndTop);

	// set help context if possible
	DWORD* pdwContext = NULL;
	if (hWnd != NULL)
	{
		// use app-level context or frame level context
		LRESULT lResult = ::SendMessage(hWndTop, WM_HELPPROMPTADDR, 0, 0);
		if (lResult != 0)
			pdwContext = (DWORD*)lResult;
	}
	// for backward compatibility use app context if possible
	if (pdwContext == NULL && this != NULL)
		pdwContext = &m_dwPromptContext;

	DWORD dwOldPromptContext = 0;
	if (pdwContext != NULL)
	{
		// save old prompt context for restoration later
		dwOldPromptContext = *pdwContext;
		if (nIDPrompt != 0)
			*pdwContext = HID_BASE_PROMPT+nIDPrompt;
	}

	// determine icon based on type specified
	if ((nType & MB_ICONMASK) == 0)
	{
		switch (nType & MB_TYPEMASK)
		{
		case MB_OK:
		case MB_OKCANCEL:
			nType |= MB_ICONEXCLAMATION;
			break;

		case MB_YESNO:
		case MB_YESNOCANCEL:
			nType |= MB_ICONEXCLAMATION;
			break;

		case MB_ABORTRETRYIGNORE:
		case MB_RETRYCANCEL:
			// No default icon for these types, since they are rarely used.
			// The caller should specify the icon.
			break;
		}
	}

#ifdef _DEBUG
	if ((nType & MB_ICONMASK) == 0)
		TRACE0("Warning: no icon specified for message box.\n");
#endif

	TCHAR szAppName[_MAX_PATH];
	LPCTSTR pszAppName;
	if (this != NULL)
		pszAppName = m_pszAppName;
	else
	{
		pszAppName = szAppName;
		GetModuleFileName(NULL, szAppName, _MAX_PATH);
	}

	int nResult =
		::MessageBox(hWnd, lpszPrompt, pszAppName, nType);

	// restore prompt context if possible
	if (pdwContext != NULL)
		*pdwContext = dwOldPromptContext;

	// re-enable windows
	if (hWndTop != NULL)
		::EnableWindow(hWndTop, TRUE);
	EnableModeless(TRUE);

	return nResult;
}

int AFXAPI AfxMessageBox(LPCTSTR lpszText, UINT nType, UINT nIDHelp)
{
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		return pApp->DoMessageBox(lpszText, nType, nIDHelp);
	else
		return pApp->CWinApp::DoMessageBox(lpszText, nType, nIDHelp);
}

int AFXAPI AfxMessageBox(UINT nIDPrompt, UINT nType, UINT nIDHelp)
{
	CString string;
	if (!string.LoadString(nIDPrompt))
	{
		TRACE1("Error: failed to load message box prompt string 0x%04x.\n",
			nIDPrompt);
		ASSERT(FALSE);
	}
	if (nIDHelp == (UINT)-1)
		nIDHelp = nIDPrompt;
	return AfxMessageBox(string, nType, nIDHelp);
}

////////////////////////////////////////////////////////////////////////////
// UI related CWnd functions

HWND PASCAL CWnd::GetSafeOwner_(HWND hParent, HWND* pWndTop)
{
	// get window to start with
	HWND hWnd = hParent;
	if (hWnd == NULL)
	{
		CFrameWnd* pFrame = CCmdTarget::GetRoutingFrame_();
		if (pFrame != NULL)
			hWnd = pFrame->GetSafeHwnd();
		else
			hWnd = AfxGetMainWnd()->GetSafeHwnd();
	}

	// a popup window cannot be owned by a child window
	while (hWnd != NULL && (::GetWindowLong(hWnd, GWL_STYLE) & WS_CHILD))
		hWnd = ::GetParent(hWnd);

	// determine toplevel window to disable as well
	HWND hWndTop = hWnd, hWndTemp = hWnd;
	for (;;)
	{
		if (hWndTemp == NULL)
			break;
		else
			hWndTop = hWndTemp;
		hWndTemp = ::GetParent(hWndTop);
	}

	// get last active popup of first non-child that was found
	if (hParent == NULL && hWnd != NULL)
		hWnd = ::GetLastActivePopup(hWnd);

	// disable and store top level parent window if specified
	if (pWndTop != NULL)
	{
		if (hWndTop != NULL && ::IsWindowEnabled(hWndTop) && hWndTop != hWnd)
		{
			*pWndTop = hWndTop;
			::EnableWindow(hWndTop, FALSE);
		}
		else
			*pWndTop = NULL;
	}

	return hWnd;    // return the owner as HWND
}

/////////////////////////////////////////////////////////////////////////////
// UI related CCmdTarget functions

CView* PASCAL CCmdTarget::GetRoutingView_()
{
	CView* pView = AfxGetThreadState()->m_pRoutingView;
	if (pView != NULL)
		ASSERT_VALID(pView);
	return pView;
}

CFrameWnd* PASCAL CCmdTarget::GetRoutingFrame_()
{
	CFrameWnd* pFrame = AfxGetThreadState()->m_pRoutingFrame;
	if (pFrame != NULL)
		ASSERT_VALID(pFrame);
	return pFrame;
}

/////////////////////////////////////////////////////////////////////////////
