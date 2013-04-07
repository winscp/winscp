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

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////

BEGIN_MESSAGE_MAP(CFormView, CScrollView)
	//{{AFX_MSG_MAP(CFormView)
	ON_WM_SETFOCUS()
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
#ifndef _AFX_NO_CTL3D_SUPPORT
	ON_MESSAGE(WM_QUERY3DCONTROLS, OnQuery3dControls)
#endif
#ifndef _AFX_NO_OCC_SUPPORT
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
#endif
END_MESSAGE_MAP()

CFormView::CFormView(LPCTSTR lpszTemplateName)
{
	m_lpszTemplateName = lpszTemplateName;
	m_pCreateContext = NULL;
	m_hWndFocus = NULL;     // focus window is unknown
}

CFormView::CFormView(UINT nIDTemplate)
{
	ASSERT_VALID_IDR(nIDTemplate);
	m_lpszTemplateName = MAKEINTRESOURCE(nIDTemplate);
	m_pCreateContext = NULL;
	m_hWndFocus = NULL;     // focus window is unknown
}

// virtual override of CWnd::Create
BOOL CFormView::Create(LPCTSTR /*lpszClassName*/, LPCTSTR /*lpszWindowName*/,
	DWORD dwRequestedStyle, const RECT& rect, CWnd* pParentWnd, UINT nID,
	CCreateContext* pContext)
{
	ASSERT(pParentWnd != NULL);
	ASSERT(m_lpszTemplateName != NULL);

	m_pCreateContext = pContext;    // save state for later OnCreate

#ifdef _DEBUG
	// dialog template must exist and be invisible with WS_CHILD set
	if (!_AfxCheckDialogTemplate(m_lpszTemplateName, TRUE))
	{
		ASSERT(FALSE);          // invalid dialog template name
		PostNcDestroy();        // cleanup if Create fails too soon
		return FALSE;
	}
#endif //_DEBUG

	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	// call PreCreateWindow to get prefered extended style
	CREATESTRUCT cs; memset(&cs, 0, sizeof(CREATESTRUCT));
	if (dwRequestedStyle == 0)
		dwRequestedStyle = AFX_WS_DEFAULT_VIEW;
	cs.style = dwRequestedStyle;
	if (!PreCreateWindow(cs))
		return FALSE;

	// create a modeless dialog
	if (!CreateDlg(m_lpszTemplateName, pParentWnd))
		return FALSE;

	m_pCreateContext = NULL;

	// we use the style from the template - but make sure that
	//  the WS_BORDER bit is correct
	// the WS_BORDER bit will be whatever is in dwRequestedStyle
	ModifyStyle(WS_BORDER|WS_CAPTION, cs.style & (WS_BORDER|WS_CAPTION));
	ModifyStyleEx(WS_EX_CLIENTEDGE, cs.dwExStyle & WS_EX_CLIENTEDGE);

	SetDlgCtrlID(nID);

	CRect rectTemplate;
	GetWindowRect(rectTemplate);
	SetScrollSizes(MM_TEXT, rectTemplate.Size());

	// initialize controls etc
	if (!ExecuteDlgInit(m_lpszTemplateName))
		return FALSE;

	// force the size requested
	SetWindowPos(NULL, rect.left, rect.top,
		rect.right - rect.left, rect.bottom - rect.top,
		SWP_NOZORDER|SWP_NOACTIVATE);

	// make visible if requested
	if (dwRequestedStyle & WS_VISIBLE)
		ShowWindow(SW_NORMAL);

	return TRUE;
}

void CFormView::OnInitialUpdate()
{
	ASSERT_VALID(this);

	if (!UpdateData(FALSE))
		TRACE0("UpdateData failed during formview initial update.\n");

	CScrollView::OnInitialUpdate();
}

int CFormView::OnCreate(LPCREATESTRUCT lpcs)
{
	// since we can't get the create context parameter passed in
	//  through CreateDialog, we use a temporary member variable
	lpcs->lpCreateParams = (LPVOID)m_pCreateContext;
	return CScrollView::OnCreate(lpcs);
}

void CFormView::OnActivateView(
	BOOL bActivate, CView* pActivateView, CView* pDeactiveView)
{
	if (SaveFocusControl())
		return;     // don't call base class when focus is already set

	CView::OnActivateView(bActivate, pActivateView, pDeactiveView);
}

void CFormView::OnActivateFrame(UINT nState, CFrameWnd* /*pFrameWnd*/)
{
	if (nState == WA_INACTIVE)
		SaveFocusControl();     // save focus when frame loses activation
}

BOOL CFormView::SaveFocusControl()
{
	// save focus window if focus is on this window's controls
	HWND hWndFocus = ::GetFocus();
	if (hWndFocus != NULL && ::IsChild(m_hWnd, hWndFocus))
	{
		m_hWndFocus = hWndFocus;
		return TRUE;
	}
	return FALSE;
}

void CFormView::OnSetFocus(CWnd*)
{
	if (!::IsWindow(m_hWndFocus) || !::IsChild(m_hWnd, m_hWndFocus))
	{
		// invalid or unknown focus window... let windows handle it
		m_hWndFocus = NULL;
		Default();
		return;
	}
	// otherwise, set focus to the last known focus window
	::SetFocus(m_hWndFocus);
}

BOOL CFormView::PreTranslateMessage(MSG* pMsg)
{
	ASSERT(pMsg != NULL);
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);

	// allow tooltip messages to be filtered
	if (CView::PreTranslateMessage(pMsg))
		return TRUE;

	// don't translate dialog messages when in Shift+F1 help mode
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	if (pFrameWnd != NULL && pFrameWnd->m_bHelpMode)
		return FALSE;

	// since 'IsDialogMessage' will eat frame window accelerators,
	//   we call all frame windows' PreTranslateMessage first
	pFrameWnd = GetParentFrame();   // start with first parent frame
	while (pFrameWnd != NULL)
	{
		// allow owner & frames to translate before IsDialogMessage does
		if (pFrameWnd->PreTranslateMessage(pMsg))
			return TRUE;

		// try parent frames until there are no parent frames
		pFrameWnd = pFrameWnd->GetParentFrame();
	}

	// don't call IsDialogMessage if form is empty
	if (::GetWindow(m_hWnd, GW_CHILD) == NULL)
		return FALSE;

	// filter both messages to dialog and from children
	return PreTranslateInput(pMsg);
}

void CFormView::OnDraw(CDC* pDC)
{
	ASSERT_VALID(this);

	// do nothing - dialog controls will paint themselves,
	//   and Windows dialog controls do not support printing
#ifdef _DEBUG
	if (pDC->IsPrinting())
		TRACE0("Warning: CFormView does not support printing.\n");
#endif

	UNUSED(pDC);     // unused in release build
}

#ifndef _AFX_NO_OCC_SUPPORT

LRESULT CFormView::HandleInitDialog(WPARAM, LPARAM)
{
	Default();  // allow default to initialize first (common dialogs/etc)

	// create OLE controls
	COccManager* pOccManager = afxOccManager;
	if ((pOccManager != NULL) && (m_pOccDialogInfo != NULL))
	{
		if (!pOccManager->CreateDlgControls(this, m_lpszTemplateName,
			m_pOccDialogInfo))
		{
			TRACE0("Warning: CreateDlgControls failed during form view init.\n");
			return FALSE;
		}
	}

	return FALSE;   // don't set focus until later
}

BOOL CFormView::SetOccDialogInfo(_AFX_OCC_DIALOG_INFO* pOccDialogInfo)
{
	m_pOccDialogInfo = pOccDialogInfo;
	return TRUE;
}

#endif //!_AFX_NO_OCC_SUPPORT

//////////////////////////////////////////////////////////////////////////
// CFormView diagnostics

#ifdef _DEBUG
void CFormView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);

	dc << "m_lpszTemplateName = ";
	if (HIWORD(m_lpszTemplateName) == 0)
		dc << (int)LOWORD((DWORD)m_lpszTemplateName);
	else
		dc << m_lpszTemplateName;

	dc << "\n";
}

void CFormView::AssertValid() const
{
	CView::AssertValid();
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CFormView, CScrollView)

//////////////////////////////////////////////////////////////////////////
