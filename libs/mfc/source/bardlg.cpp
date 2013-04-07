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
#include "occimpl.h"

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

CDialogBar::CDialogBar()
{
#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
	m_pOccDialogInfo = NULL;
#endif
}

CDialogBar::~CDialogBar()
{
	DestroyWindow();    // avoid PostNcDestroy problems
}

BOOL CDialogBar::Create(CWnd* pParentWnd, LPCTSTR lpszTemplateName,
	UINT nStyle, UINT nID)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(lpszTemplateName != NULL);

#ifdef _DEBUG
	// dialog template must exist and be invisible with WS_CHILD set
	if (!_AfxCheckDialogTemplate(lpszTemplateName, TRUE))
	{
		ASSERT(FALSE);          // invalid dialog template name
		PostNcDestroy();        // cleanup if Create fails too soon
		return FALSE;
	}
#endif //_DEBUG

	// allow chance to modify styles
	m_dwStyle = (nStyle & CBRS_ALL);
	CREATESTRUCT cs;
	memset(&cs, 0, sizeof(cs));
	cs.lpszClass = _afxWndControlBar;
	cs.style = (DWORD)nStyle | WS_CHILD;
	cs.hMenu = (HMENU)nID;
	cs.hInstance = AfxGetInstanceHandle();
	cs.hwndParent = pParentWnd->GetSafeHwnd();
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog

#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = lpszTemplateName;
#endif

	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	BOOL bSuccess = CreateDlg(lpszTemplateName, pParentWnd);

#ifndef _AFX_NO_OCC_SUPPORT
	m_lpszTemplateName = NULL;
#endif

	if (!bSuccess)
		return FALSE;

	// dialog template MUST specify that the dialog
	//  is an invisible child window
	SetDlgCtrlID(nID);
	CRect rect;
	GetWindowRect(&rect);
	m_sizeDefault = rect.Size();    // set fixed size

	// force WS_CLIPSIBLINGS
	ModifyStyle(0, WS_CLIPSIBLINGS);

	if (!ExecuteDlgInit(lpszTemplateName))
		return FALSE;

	// force the size to zero - resizing bar will occur later
	SetWindowPos(NULL, 0, 0, 0, 0, SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);

	return TRUE;
}

CSize CDialogBar::CalcFixedLayout(BOOL bStretch, BOOL bHorz)
{
	if (bStretch) // if not docked stretch to fit
		return CSize(bHorz ? 32767 : m_sizeDefault.cx,
			bHorz ? m_sizeDefault.cy : 32767);
	else
		return m_sizeDefault;
}

void CDialogBar::OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler)
{
	UpdateDialogControls(pTarget, bDisableIfNoHndler);
}

#ifndef _AFX_NO_OCC_SUPPORT

//{{AFX_MSG_MAP(CDialogBar)
BEGIN_MESSAGE_MAP(CDialogBar, CControlBar)
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
END_MESSAGE_MAP()
//}}AFX_MSG_MAP

LRESULT CDialogBar::HandleInitDialog(WPARAM, LPARAM)
{
	Default();  // allow default to initialize first (common dialogs/etc)

	// create OLE controls
	COccManager* pOccManager = afxOccManager;
	if ((pOccManager != NULL) && (m_pOccDialogInfo != NULL))
	{
		if (!pOccManager->CreateDlgControls(this, m_lpszTemplateName,
			m_pOccDialogInfo))
		{
			TRACE0("Warning: CreateDlgControls failed during dialog bar init.\n");
			return FALSE;
		}
	}

	return TRUE;
}

BOOL CDialogBar::SetOccDialogInfo(_AFX_OCC_DIALOG_INFO* pOccDialogInfo)
{
	m_pOccDialogInfo = pOccDialogInfo;
	return TRUE;
}

#endif //!_AFX_NO_OCC_SUPPORT

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDialogBar, CControlBar)

///////////////////////////////////////////////////////////////////////////
