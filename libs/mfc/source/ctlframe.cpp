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

#ifdef _AFXDLL

#ifdef AFXCTL_CORE3_SEG
#pragma code_seg(AFXCTL_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Message map

BEGIN_MESSAGE_MAP(CControlFrameWnd, CWnd)
	//{{AFX_MSG_MAP(CControlFrameWnd)
	ON_WM_CLOSE()
	ON_WM_ACTIVATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CControlFrameWnd implementation

CControlFrameWnd::CControlFrameWnd(COleControl* pCtrl) :
	m_pCtrl(pCtrl)
{
}

BOOL CControlFrameWnd::PreCreateWindow(CREATESTRUCT& cs)
{
	// make sure the default window class is registered
	VERIFY(AfxDeferRegisterClass(AFX_WND_REG));

	if (cs.lpszClass == NULL)
		cs.lpszClass = AFX_WND;

	return TRUE;
}

BOOL CControlFrameWnd::Create(LPCTSTR pszFrameTitle)
{
	if (!CreateEx(
			0,
			NULL, pszFrameTitle,
			WS_OVERLAPPED | WS_CAPTION | WS_SYSMENU,
			CW_USEDEFAULT, CW_USEDEFAULT,
			CW_USEDEFAULT, CW_USEDEFAULT,
			NULL, NULL, NULL))
	{
		TRACE0("Warning: failed to create CControlFrameWnd\n");
		return FALSE;
	}

	return TRUE;
}

void CControlFrameWnd::OnClose()
{
	m_pCtrl->OnFrameClose();
	DestroyWindow();
}

void CControlFrameWnd::PostNcDestroy()
{
	delete this;
}

void CControlFrameWnd::OnActivate(UINT nState, CWnd* pWndOther,
	BOOL bMinimized)
{
	CWnd::OnActivate(nState, pWndOther, bMinimized);
	m_pCtrl->SetFocus();
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#endif //_AFXDLL
