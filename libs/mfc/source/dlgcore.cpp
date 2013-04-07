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

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// AfxDlgProc - does nothing since all messages are handled via AfxWndProc

BOOL CALLBACK AfxDlgProc(HWND hWnd, UINT message, WPARAM, LPARAM)
{
	if (message == WM_INITDIALOG)
	{
		// special case for WM_INITDIALOG
		CDialog* pDlg = DYNAMIC_DOWNCAST(CDialog, CWnd::FromHandlePermanent(hWnd));
		if (pDlg != NULL)
			return pDlg->OnInitDialog();
		else
			return 1;
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CDialog - Modeless and Modal

BEGIN_MESSAGE_MAP(CDialog, CWnd)
#ifndef _AFX_NO_GRAYDLG_SUPPORT
	ON_WM_CTLCOLOR()
#endif
	//{{AFX_MSG_MAP(CDialog)
	ON_COMMAND(IDOK, OnOK)
	ON_COMMAND(IDCANCEL, OnCancel)
	ON_MESSAGE(WM_COMMANDHELP, OnCommandHelp)
	ON_MESSAGE(WM_HELPHITTEST, OnHelpHitTest)
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_MESSAGE(WM_SETFONT, HandleSetFont)
	//}}AFX_MSG_MAP
#ifndef _AFX_NO_CTL3D_SUPPORT
	ON_MESSAGE(WM_QUERY3DCONTROLS, OnQuery3dControls)
#endif
END_MESSAGE_MAP()

BOOL CDialog::PreTranslateMessage(MSG* pMsg)
{
	// for modeless processing (or modal)
	ASSERT(m_hWnd != NULL);

	// allow tooltip messages to be filtered
	if (CWnd::PreTranslateMessage(pMsg))
		return TRUE;

	// don't translate dialog messages when in Shift+F1 help mode
	CFrameWnd* pFrameWnd = GetTopLevelFrame();
	if (pFrameWnd != NULL && pFrameWnd->m_bHelpMode)
		return FALSE;

	// fix around for VK_ESCAPE in a multiline Edit that is on a Dialog
	// that doesn't have a cancel or the cancel is disabled.
	if (pMsg->message == WM_KEYDOWN &&
		(pMsg->wParam == VK_ESCAPE || pMsg->wParam == VK_CANCEL) &&
		(::GetWindowLong(pMsg->hwnd, GWL_STYLE) & ES_MULTILINE) &&
		_AfxCompareClassName(pMsg->hwnd, _T("Edit")))
	{
		HWND hItem = ::GetDlgItem(m_hWnd, IDCANCEL);
		if (hItem == NULL || ::IsWindowEnabled(hItem))
		{
			SendMessage(WM_COMMAND, IDCANCEL, 0);
			return TRUE;
		}
	}
	// filter both messages to dialog and from children
	return PreTranslateInput(pMsg);
}

BOOL CDialog::OnCmdMsg(UINT nID, int nCode, void* pExtra,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	if (CWnd::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

	if ((nCode != CN_COMMAND && nCode != CN_UPDATE_COMMAND_UI) ||
			!IS_COMMAND_ID(nID) || nID >= 0xf000)
	{
		// control notification or non-command button or system command
		return FALSE;       // not routed any further
	}

	// if we have an owner window, give it second crack
	CWnd* pOwner = GetParent();
	if (pOwner != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceCmdRouting)
			TRACE1("Routing command id 0x%04X to owner window.\n", nID);
#endif
		ASSERT(pOwner != this);
		if (pOwner->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
			return TRUE;
	}

	// last crack goes to the current CWinThread object
	CWinThread* pThread = AfxGetThread();
	if (pThread != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceCmdRouting)
			TRACE1("Routing command id 0x%04X to app.\n", nID);
#endif
		if (pThread->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
			return TRUE;
	}

#ifdef _DEBUG
	if (afxTraceFlags & traceCmdRouting)
	{
		TRACE2("IGNORING command id 0x%04X sent to %hs dialog.\n", nID,
				GetRuntimeClass()->m_lpszClassName);
	}
#endif
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Modeless Dialogs have 2-phase construction

CDialog::CDialog()
{
	ASSERT(m_hWnd == NULL);
	AFX_ZERO_INIT_OBJECT(CWnd);
}

CDialog::~CDialog()
{
	if (m_hWnd != NULL)
	{
		TRACE0("Warning: calling DestroyWindow in CDialog::~CDialog --\n");
		TRACE0("\tOnDestroy or PostNcDestroy in derived class will not be called.\n");
		DestroyWindow();
	}
}

BOOL CDialog::Create(LPCTSTR lpszTemplateName, CWnd* pParentWnd)
{
	ASSERT(HIWORD(lpszTemplateName) == 0 ||
		AfxIsValidString(lpszTemplateName));

	m_lpszTemplateName = lpszTemplateName;  // used for help
	if (HIWORD(m_lpszTemplateName) == 0 && m_nIDHelp == 0)
		m_nIDHelp = LOWORD((DWORD)m_lpszTemplateName);

#ifdef _DEBUG
	if (!_AfxCheckDialogTemplate(lpszTemplateName, FALSE))
	{
		ASSERT(FALSE);          // invalid dialog template name
		PostNcDestroy();        // cleanup if Create fails too soon
		return FALSE;
	}
#endif //_DEBUG

	HINSTANCE hInst = AfxFindResourceHandle(lpszTemplateName, RT_DIALOG);
	HRSRC hResource = ::FindResource(hInst, lpszTemplateName, RT_DIALOG);
	HGLOBAL hTemplate = LoadResource(hInst, hResource);
	BOOL bResult = CreateIndirect(hTemplate, pParentWnd, hInst);
	FreeResource(hTemplate);

	return bResult;
}

// for backward compatibility
BOOL CDialog::CreateIndirect(HGLOBAL hDialogTemplate, CWnd* pParentWnd)
{
	return CreateIndirect(hDialogTemplate, pParentWnd, NULL);
}

BOOL CDialog::CreateIndirect(HGLOBAL hDialogTemplate, CWnd* pParentWnd,
	HINSTANCE hInst)
{
	ASSERT(hDialogTemplate != NULL);

	LPCDLGTEMPLATE lpDialogTemplate = (LPCDLGTEMPLATE)LockResource(hDialogTemplate);
	BOOL bResult = CreateIndirect(lpDialogTemplate, pParentWnd, NULL, hInst);
	UnlockResource(hDialogTemplate);

	return bResult;
}

// for backward compatibility
BOOL CDialog::CreateIndirect(LPCDLGTEMPLATE lpDialogTemplate, CWnd* pParentWnd,
	void* lpDialogInit)
{
	return CreateIndirect(lpDialogTemplate, pParentWnd, lpDialogInit, NULL);
}

BOOL CDialog::CreateIndirect(LPCDLGTEMPLATE lpDialogTemplate, CWnd* pParentWnd,
	void* lpDialogInit, HINSTANCE hInst)
{
	ASSERT(lpDialogTemplate != NULL);

	if (pParentWnd == NULL)
		pParentWnd = AfxGetMainWnd();
	m_lpDialogInit = lpDialogInit;

	return CreateDlgIndirect(lpDialogTemplate, pParentWnd, hInst);
}

BOOL CWnd::CreateDlg(LPCTSTR lpszTemplateName, CWnd* pParentWnd)
{
	// load resource
	LPCDLGTEMPLATE lpDialogTemplate = NULL;
	HGLOBAL hDialogTemplate = NULL;
	HINSTANCE hInst = AfxFindResourceHandle(lpszTemplateName, RT_DIALOG);
	HRSRC hResource = ::FindResource(hInst, lpszTemplateName, RT_DIALOG);
	hDialogTemplate = LoadResource(hInst, hResource);
	if (hDialogTemplate != NULL)
		lpDialogTemplate = (LPCDLGTEMPLATE)LockResource(hDialogTemplate);
	ASSERT(lpDialogTemplate != NULL);

	// create a modeless dialog
	BOOL bSuccess = CreateDlgIndirect(lpDialogTemplate, pParentWnd, hInst);

	// free resource
	UnlockResource(hDialogTemplate);
	FreeResource(hDialogTemplate);

	return bSuccess;
}

// for backward compatibility
BOOL CWnd::CreateDlgIndirect(LPCDLGTEMPLATE lpDialogTemplate, CWnd* pParentWnd)
{
	return CreateDlgIndirect(lpDialogTemplate, pParentWnd, NULL);
}

BOOL CWnd::CreateDlgIndirect(LPCDLGTEMPLATE lpDialogTemplate,
	CWnd* pParentWnd, HINSTANCE hInst)
{
	ASSERT(lpDialogTemplate != NULL);
	if (pParentWnd != NULL)
		ASSERT_VALID(pParentWnd);

	if (hInst == NULL)
		hInst = AfxGetInstanceHandle();

#ifndef _AFX_NO_OCC_SUPPORT
	_AFX_OCC_DIALOG_INFO occDialogInfo;
	COccManager* pOccManager = afxOccManager;
#endif

	HGLOBAL hTemplate = NULL;

	HWND hWnd = NULL;
#ifdef _DEBUG
	DWORD dwError = 0;
#endif

	TRY
	{
		VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
		AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

#ifndef _AFX_NO_OCC_SUPPORT
		// separately create OLE controls in the dialog template
		if (pOccManager != NULL)
		{
			if (!SetOccDialogInfo(&occDialogInfo))
				return FALSE;

			lpDialogTemplate = pOccManager->PreCreateDialog(&occDialogInfo,
				lpDialogTemplate);
		}

		if (lpDialogTemplate == NULL)
			return FALSE;
#endif //!_AFX_NO_OCC_SUPPORT

		// If no font specified, set the system font.
		CString strFace;
		WORD wSize = 0;
		BOOL bSetSysFont = !CDialogTemplate::GetFont(lpDialogTemplate, strFace,
			wSize);

		// On DBCS systems, also change "MS Sans Serif" or "Helv" to system font.
		if ((!bSetSysFont) && GetSystemMetrics(SM_DBCSENABLED))
		{
			bSetSysFont = (strFace == _T("MS Shell Dlg") ||
				strFace == _T("MS Sans Serif") || strFace == _T("Helv"));
			if (bSetSysFont && (wSize == 8))
				wSize = 0;
		}

		if (bSetSysFont)
		{
			CDialogTemplate dlgTemp(lpDialogTemplate);
			dlgTemp.SetSystemFont(wSize);
			hTemplate = dlgTemp.Detach();
		}

		if (hTemplate != NULL)
			lpDialogTemplate = (DLGTEMPLATE*)GlobalLock(hTemplate);

		// setup for modal loop and creation
		m_nModalResult = -1;
		m_nFlags |= WF_CONTINUEMODAL;

		// create modeless dialog
		AfxHookWindowCreate(this);
		hWnd = ::CreateDialogIndirect(hInst, lpDialogTemplate,
			pParentWnd->GetSafeHwnd(), AfxDlgProc);
#ifdef _DEBUG
		dwError = ::GetLastError();
#endif
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);
		m_nModalResult = -1;
	}
	END_CATCH_ALL

#ifndef _AFX_NO_OCC_SUPPORT
	if (pOccManager != NULL)
	{
		pOccManager->PostCreateDialog(&occDialogInfo);
		if (hWnd != NULL)
			SetOccDialogInfo(NULL);
	}
#endif //!_AFX_NO_OCC_SUPPORT

	if (!AfxUnhookWindowCreate())
		PostNcDestroy();        // cleanup if Create fails too soon

	// handle EndDialog calls during OnInitDialog
	if (hWnd != NULL && !(m_nFlags & WF_CONTINUEMODAL))
	{
		::DestroyWindow(hWnd);
		hWnd = NULL;
	}

	if (hTemplate != NULL)
	{
		GlobalUnlock(hTemplate);
		GlobalFree(hTemplate);
	}

	// help with error diagnosis (only if WM_INITDIALOG didn't EndDialog())
	if (hWnd == NULL && (m_nFlags & WF_CONTINUEMODAL))
	{
#ifdef _DEBUG
#ifndef _AFX_NO_OCC_SUPPORT
		if (afxOccManager == NULL)
		{
			TRACE0(">>> If this dialog has OLE controls:\n");
			TRACE0(">>> AfxEnableControlContainer has not been called yet.\n");
			TRACE0(">>> You should call it in your app's InitInstance function.\n");
		}
		else if (dwError != 0)
		{
			TRACE1("Warning: Dialog creation failed!  GetLastError returns 0x%8.8X\n", dwError);
		}
#endif //!_AFX_NO_OCC_SUPPORT
#endif //_DEBUG
		return FALSE;
	}

	ASSERT(hWnd == m_hWnd);
	return TRUE;
}

#ifndef _AFX_NO_OCC_SUPPORT

BOOL CDialog::SetOccDialogInfo(_AFX_OCC_DIALOG_INFO* pOccDialogInfo)
{
	m_pOccDialogInfo = pOccDialogInfo;
	return TRUE;
}

#endif

/////////////////////////////////////////////////////////////////////////////
// Modal Dialogs

// Modal Constructors just save parameters
CDialog::CDialog(LPCTSTR lpszTemplateName, CWnd* pParentWnd)
{
	ASSERT(HIWORD(lpszTemplateName) == 0 ||
		AfxIsValidString(lpszTemplateName));

	AFX_ZERO_INIT_OBJECT(CWnd);

	m_pParentWnd = pParentWnd;
	m_lpszTemplateName = lpszTemplateName;
	if (HIWORD(m_lpszTemplateName) == 0)
		m_nIDHelp = LOWORD((DWORD)m_lpszTemplateName);
}

CDialog::CDialog(UINT nIDTemplate, CWnd* pParentWnd)
{
	AFX_ZERO_INIT_OBJECT(CWnd);

	m_pParentWnd = pParentWnd;
	m_lpszTemplateName = MAKEINTRESOURCE(nIDTemplate);
	m_nIDHelp = nIDTemplate;
}

BOOL CDialog::InitModalIndirect(HGLOBAL hDialogTemplate, CWnd* pParentWnd)
{
	// must be called on an empty constructed CDialog
	ASSERT(m_lpszTemplateName == NULL);
	ASSERT(m_hDialogTemplate == NULL);
	ASSERT(hDialogTemplate != NULL);

	if (m_pParentWnd == NULL)
		m_pParentWnd = pParentWnd;

	m_hDialogTemplate = hDialogTemplate;

	return TRUE;    // always ok (DoModal actually brings up dialog)
}

BOOL CDialog::InitModalIndirect(LPCDLGTEMPLATE lpDialogTemplate, CWnd* pParentWnd,
	void* lpDialogInit)
{
	// must be called on an empty constructed CDialog
	ASSERT(m_lpszTemplateName == NULL);
	ASSERT(m_lpDialogTemplate == NULL);
	ASSERT(lpDialogTemplate != NULL);

	if (m_pParentWnd == NULL)
		m_pParentWnd = pParentWnd;

	m_lpDialogTemplate = lpDialogTemplate;
	m_lpDialogInit = lpDialogInit;

	return TRUE;    // always ok (DoModal actually brings up dialog)
}

HWND CDialog::PreModal()
{
	// cannot call DoModal on a dialog already constructed as modeless
	ASSERT(m_hWnd == NULL);

	// allow OLE servers to disable themselves
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		pApp->EnableModeless(FALSE);

	// find parent HWND
	HWND hWnd = CWnd::GetSafeOwner_(m_pParentWnd->GetSafeHwnd(), &m_hWndTop);

	// hook for creation of dialog
	AfxHookWindowCreate(this);

	// return window to use as parent for dialog
	return hWnd;
}

void CDialog::PostModal()
{
	AfxUnhookWindowCreate();   // just in case
	Detach();               // just in case

	// re-enable windows
	if (::IsWindow(m_hWndTop))
		::EnableWindow(m_hWndTop, TRUE);
	m_hWndTop = NULL;
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		pApp->EnableModeless(TRUE);
}

int CDialog::DoModal()
{
	// can be constructed with a resource template or InitModalIndirect
	ASSERT(m_lpszTemplateName != NULL || m_hDialogTemplate != NULL ||
		m_lpDialogTemplate != NULL);

	// load resource as necessary
	LPCDLGTEMPLATE lpDialogTemplate = m_lpDialogTemplate;
	HGLOBAL hDialogTemplate = m_hDialogTemplate;
	HINSTANCE hInst = AfxGetResourceHandle();
	if (m_lpszTemplateName != NULL)
	{
		hInst = AfxFindResourceHandle(m_lpszTemplateName, RT_DIALOG);
		HRSRC hResource = ::FindResource(hInst, m_lpszTemplateName, RT_DIALOG);
		hDialogTemplate = LoadResource(hInst, hResource);
	}
	if (hDialogTemplate != NULL)
		lpDialogTemplate = (LPCDLGTEMPLATE)LockResource(hDialogTemplate);

	// return -1 in case of failure to load the dialog template resource
	if (lpDialogTemplate == NULL)
		return -1;

	// disable parent (before creating dialog)
	HWND hWndParent = PreModal();
	AfxUnhookWindowCreate();
	BOOL bEnableParent = FALSE;
	if (hWndParent != NULL && ::IsWindowEnabled(hWndParent))
	{
		::EnableWindow(hWndParent, FALSE);
		bEnableParent = TRUE;
	}

	TRY
	{
		// create modeless dialog
		AfxHookWindowCreate(this);
		if (CreateDlgIndirect(lpDialogTemplate,
						CWnd::FromHandle(hWndParent), hInst))
		{
			if (m_nFlags & WF_CONTINUEMODAL)
			{
				// enter modal loop
				DWORD dwFlags = MLF_SHOWONIDLE;
				if (GetStyle() & DS_NOIDLEMSG)
					dwFlags |= MLF_NOIDLEMSG;
				VERIFY(RunModalLoop(dwFlags) == m_nModalResult);
			}

			// hide the window before enabling the parent, etc.
			if (m_hWnd != NULL)
				SetWindowPos(NULL, 0, 0, 0, 0, SWP_HIDEWINDOW|
					SWP_NOSIZE|SWP_NOMOVE|SWP_NOACTIVATE|SWP_NOZORDER);
		}
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);
		m_nModalResult = -1;
	}
	END_CATCH_ALL

	if (bEnableParent)
		::EnableWindow(hWndParent, TRUE);
	if (hWndParent != NULL && ::GetActiveWindow() == m_hWnd)
		::SetActiveWindow(hWndParent);

	// destroy modal window
	DestroyWindow();
	PostModal();

	// unlock/free resources as necessary
	if (m_lpszTemplateName != NULL || m_hDialogTemplate != NULL)
		UnlockResource(hDialogTemplate);
	if (m_lpszTemplateName != NULL)
		FreeResource(hDialogTemplate);

	return m_nModalResult;
}

void CDialog::EndDialog(int nResult)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_nFlags & (WF_MODALLOOP|WF_CONTINUEMODAL))
		EndModalLoop(nResult);

	::EndDialog(m_hWnd, nResult);
}

/////////////////////////////////////////////////////////////////////////////
// Standard CDialog implementation

LRESULT CDialog::HandleSetFont(WPARAM wParam, LPARAM)
{
	OnSetFont(CFont::FromHandle((HFONT)wParam));
	return Default();
}

void CDialog::PreInitDialog()
{
	// ignore it
}

LRESULT CDialog::HandleInitDialog(WPARAM, LPARAM)
{
	PreInitDialog();

#ifndef _AFX_NO_OCC_SUPPORT
	// create OLE controls
	COccManager* pOccManager = afxOccManager;
	if ((pOccManager != NULL) && (m_pOccDialogInfo != NULL))
	{
		BOOL bDlgInit;
		if (m_lpDialogInit != NULL)
			bDlgInit = pOccManager->CreateDlgControls(this, m_lpDialogInit,
				m_pOccDialogInfo);
		else
			bDlgInit = pOccManager->CreateDlgControls(this, m_lpszTemplateName,
				m_pOccDialogInfo);

		if (!bDlgInit)
		{
			TRACE0("Warning: CreateDlgControls failed during dialog init.\n");
			EndDialog(-1);
			return FALSE;
		}
	}
#endif

	// Default will call the dialog proc, and thus OnInitDialog
	BOOL bResult = Default();

#ifndef _AFX_NO_OCC_SUPPORT
	if (bResult && (m_nFlags & WF_OLECTLCONTAINER))
	{
		CWnd* pWndNext = GetNextDlgTabItem(NULL);
		if (pWndNext != NULL)
		{
			pWndNext->SetFocus();   // UI Activate OLE control
			bResult = FALSE;
		}
	}
#endif

	return bResult;
}

BOOL AFXAPI AfxHelpEnabled()
{
	if (AfxGetApp() == NULL)
		return FALSE;

	// help is enabled if the app has a handler for ID_HELP
	AFX_CMDHANDLERINFO info;

	// check main window first
	CWnd* pWnd = AfxGetMainWnd();
	if (pWnd != NULL && pWnd->OnCmdMsg(ID_HELP, CN_COMMAND, NULL, &info))
		return TRUE;

	// check app last
	return AfxGetApp()->OnCmdMsg(ID_HELP, CN_COMMAND, NULL, &info);
}

void CDialog::OnSetFont(CFont*)
{
	// ignore it
}

BOOL CDialog::OnInitDialog()
{
	// execute dialog RT_DLGINIT resource
	BOOL bDlgInit;
	if (m_lpDialogInit != NULL)
		bDlgInit = ExecuteDlgInit(m_lpDialogInit);
	else
		bDlgInit = ExecuteDlgInit(m_lpszTemplateName);

	if (!bDlgInit)
	{
		TRACE0("Warning: ExecuteDlgInit failed during dialog init.\n");
		EndDialog(-1);
		return FALSE;
	}

	// transfer data into the dialog from member variables
	if (!UpdateData(FALSE))
	{
		TRACE0("Warning: UpdateData failed during dialog init.\n");
		EndDialog(-1);
		return FALSE;
	}

	// enable/disable help button automatically
	CWnd* pHelpButton = GetDlgItem(ID_HELP);
	if (pHelpButton != NULL)
		pHelpButton->ShowWindow(AfxHelpEnabled() ? SW_SHOW : SW_HIDE);

	return TRUE;    // set focus to first one
}

void CDialog::OnOK()
{
	if (!UpdateData(TRUE))
	{
		TRACE0("UpdateData failed during dialog termination.\n");
		// the UpdateData routine will set focus to correct item
		return;
	}
	EndDialog(IDOK);
}

void CDialog::OnCancel()
{
	EndDialog(IDCANCEL);
}

BOOL CDialog::CheckAutoCenter()
{
	// load resource as necessary
	LPCDLGTEMPLATE lpDialogTemplate = m_lpDialogTemplate;
	HGLOBAL hDialogTemplate = m_hDialogTemplate;
	if (m_lpszTemplateName != NULL)
	{
		HINSTANCE hInst = AfxFindResourceHandle(m_lpszTemplateName, RT_DIALOG);
		HRSRC hResource = ::FindResource(hInst, m_lpszTemplateName, RT_DIALOG);
		hDialogTemplate = LoadResource(hInst, hResource);
	}
	if (hDialogTemplate != NULL)
		lpDialogTemplate = (LPCDLGTEMPLATE)LockResource(hDialogTemplate);

	// determine if dialog should be centered
	BOOL bResult = TRUE;

	if (lpDialogTemplate != NULL)
	{
		DWORD dwStyle = lpDialogTemplate->style;
		short x;
		short y;

		if (((DLGTEMPLATEEX*)lpDialogTemplate)->signature == 0xFFFF)
		{
			// it's a DIALOGEX resource
			dwStyle = ((DLGTEMPLATEEX*)lpDialogTemplate)->style;
			x = ((DLGTEMPLATEEX*)lpDialogTemplate)->x;
			y = ((DLGTEMPLATEEX*)lpDialogTemplate)->y;
		}
		else
		{
			// it's a DIALOG resource
			x = lpDialogTemplate->x;
			y = lpDialogTemplate->y;
		}

		bResult = !(dwStyle & (DS_CENTER|DS_CENTERMOUSE|DS_ABSALIGN)) &&
			x == 0 && y == 0;
	}

	// unlock/free resources as necessary
	if (m_lpszTemplateName != NULL || m_hDialogTemplate != NULL)
		UnlockResource(hDialogTemplate);
	if (m_lpszTemplateName != NULL)
		FreeResource(hDialogTemplate);

	return bResult; // TRUE if auto-center is ok
}

/////////////////////////////////////////////////////////////////////////////
// Gray background support

#ifndef _AFX_NO_GRAYDLG_SUPPORT
HBRUSH CDialog::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	// use helper in CWnd
	return OnGrayCtlColor(pDC, pWnd, nCtlColor);
}
#endif //!_AFX_NO_GRAYDLG_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// CDialog support for context sensitive help.

LRESULT CDialog::OnCommandHelp(WPARAM, LPARAM lParam)
{
	if (lParam == 0 && m_nIDHelp != 0)
		lParam = HID_BASE_RESOURCE + m_nIDHelp;
	if (lParam != 0)
	{
		CWinApp* pApp = AfxGetApp();
		if (pApp != NULL)
			pApp->WinHelp(lParam);
		return TRUE;
	}
	return FALSE;
}

LRESULT CDialog::OnHelpHitTest(WPARAM, LPARAM)
{
	if (m_nIDHelp != 0)
		return HID_BASE_RESOURCE + m_nIDHelp;
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CDialog Diagnostics

#ifdef _DEBUG
void CDialog::AssertValid() const
{
	CWnd::AssertValid();
}

void CDialog::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);

	dc << "m_lpszTemplateName = ";
	if (HIWORD(m_lpszTemplateName) == 0)
		dc << (int)LOWORD((DWORD)m_lpszTemplateName);
	else
		dc << m_lpszTemplateName;

	dc << "\nm_hDialogTemplate = " << (UINT)m_hDialogTemplate;
	dc << "\nm_lpDialogTemplate = " << (UINT)m_lpDialogTemplate;
	dc << "\nm_pParentWnd = " << (void*)m_pParentWnd;
	dc << "\nm_nIDHelp = " << m_nIDHelp;

	dc << "\n";
}

// diagnostic routine to check for and decode dialog templates
// return FALSE if a program error occurs (i.e. bad resource ID or
//   bad dialog styles).
BOOL AFXAPI _AfxCheckDialogTemplate(LPCTSTR lpszResource, BOOL bInvisibleChild)
{
	ASSERT(lpszResource != NULL);
	HINSTANCE hInst = AfxFindResourceHandle(lpszResource, RT_DIALOG);
	HRSRC hResource = ::FindResource(hInst, lpszResource, RT_DIALOG);
	if (hResource == NULL)
	{
		if (HIWORD(lpszResource) != 0)
			TRACE1("ERROR: Cannot find dialog template named '%s'.\n",
				lpszResource);
		else
			TRACE1("ERROR: Cannot find dialog template with IDD 0x%04X.\n",
				LOWORD((DWORD)lpszResource));
		return FALSE;
	}

	if (!bInvisibleChild)
		return TRUE;        // that's all we need to check

	// we must check that the dialog template is for an invisible child
	//  window that can be used for a form-view or dialog-bar
	HGLOBAL hTemplate = LoadResource(hInst, hResource);
	if (hTemplate == NULL)
	{
		TRACE0("Warning: LoadResource failed for dialog template.\n");
		// this is only a warning, the real call to CreateDialog will fail
		return TRUE;        // not a program error - just out of memory
	}
	DLGTEMPLATEEX* pTemplate = (DLGTEMPLATEEX*)LockResource(hTemplate);
	DWORD dwStyle;
	if (pTemplate->signature == 0xFFFF)
		dwStyle = pTemplate->style;
	else
		dwStyle = ((DLGTEMPLATE*)pTemplate)->style;
	UnlockResource(hTemplate);
	FreeResource(hTemplate);

	if (dwStyle & WS_VISIBLE)
	{
		if (HIWORD(lpszResource) != 0)
			TRACE1("ERROR: Dialog named '%s' must be invisible.\n",
				lpszResource);
		else
			TRACE1("ERROR: Dialog with IDD 0x%04X must be invisible.\n",
				LOWORD((DWORD)lpszResource));
		return FALSE;
	}
	if (!(dwStyle & WS_CHILD))
	{
		if (HIWORD(lpszResource) != 0)
			TRACE1("ERROR: Dialog named '%s' must have the child style.\n",
				lpszResource);
		else
			TRACE1("ERROR: Dialog with IDD 0x%04X must have the child style.\n",
				LOWORD((DWORD)lpszResource));
		return FALSE;
	}

	return TRUE;
}

#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDialog, CWnd)

/////////////////////////////////////////////////////////////////////////////
