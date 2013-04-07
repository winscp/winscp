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

#ifdef AFX_OLE2_SEG
#pragma code_seg(AFX_OLE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Common code for all OLE UI dialogs

UINT CALLBACK
AfxOleHookProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
	if (message == WM_INITDIALOG)
		return (UINT)AfxDlgProc(hWnd, message, wParam, lParam);

	if (message == WM_COMMAND && LOWORD(wParam) == IDC_OLEUIHELP)
	{
		// just translate the message into the AFX standard help command.
		SendMessage(hWnd, WM_COMMAND, ID_HELP, 0);
		return TRUE;
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////

COleDialog::COleDialog(CWnd* pParentWnd) : CCommonDialog(pParentWnd)
{
	m_nLastError = (UINT)-1;       // no error
}

int COleDialog::MapResult(UINT nResult)
{
	// store it for GetLastError()
	m_nLastError = nResult;

	// map OLEUI_OK & OLEUI_CANCEL to IDOK & IDCANCEL
	if (nResult == OLEUI_OK)
		return IDOK;
	if (nResult == OLEUI_CANCEL)
		return IDCANCEL;

	// otherwise, some sort of error
	return -1;
}

/////////////////////////////////////////////////////////////////////////////
// COleDialog diagnostics

#ifdef _DEBUG
void COleDialog::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	dc << "m_nLastError = " << m_nLastError;
	dc << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// COleBusyDialog implementation

COleBusyDialog::COleBusyDialog(HTASK htaskBlocking, BOOL bNotResponding,
	DWORD dwFlags, CWnd* pParentWnd) : COleDialog(pParentWnd)
{
	memset(&m_bz, 0, sizeof(m_bz)); // initialize structure to 0/NULL

	// fill in common part
	m_bz.cbStruct = sizeof(m_bz);
	m_bz.dwFlags = dwFlags;
	if (bNotResponding)
		m_bz.dwFlags |= BZ_NOTRESPONDINGDIALOG;
	m_bz.lpfnHook = AfxOleHookProc;

	// specific for this dialog
	m_bz.hTask = htaskBlocking;

	ASSERT_VALID(this);
}

COleBusyDialog::~COleBusyDialog()
{
}

int COleBusyDialog::DoModal()
{
	// Note: we don't call EnableModeless because that in itself implies
	//  outgoing calls.  This dialog is normally brought up when an outgoing
	//  call cannot be made.

	// find parent HWND
	HWND hWndTop;
	HWND hParent = CWnd::GetSafeOwner_(m_pParentWnd->GetSafeHwnd(), &hWndTop);
	m_bz.hWndOwner = hParent;

	// run the dialog
	AfxHookWindowCreate(this);
	int iResult = ::OleUIBusy(&m_bz);
	AfxUnhookWindowCreate();   // just in case
	Detach();   // just in case

	// enable top level window
	if (hWndTop != NULL)
		::EnableWindow(hWndTop, TRUE);

	// map the result
	if (iResult == OLEUI_CANCEL)
		return IDCANCEL;

	if (iResult == OLEUI_BZ_SWITCHTOSELECTED)
		m_selection = switchTo;
	else if (iResult == OLEUI_BZ_RETRYSELECTED)
		m_selection = retry;
	else if (iResult == OLEUI_BZ_CALLUNBLOCKED)
		m_selection = callUnblocked;
	else
		m_selection = (Selection)MapResult(iResult);

	return IDOK;
}

/////////////////////////////////////////////////////////////////////////////
// COleBusyDialog diagnostics

#ifdef _DEBUG
void COleBusyDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_bz.cbStruct = " << m_bz.cbStruct;
	dc << "\nm_bz.dwFlags = " << (LPVOID)m_bz.dwFlags;
	dc << "\nm_bz.hWndOwner = " << (UINT)m_bz.hWndOwner;
	dc << "\nm_bz.lpszCaption = " << m_bz.lpszCaption;
	dc << "\nm_bz.lCustData = " << (LPVOID)m_bz.lCustData;
	dc << "\nm_bz.hInstance = " << (UINT)m_bz.hInstance;
	dc << "\nm_bz.lpszTemplate = " << (LPVOID)m_bz.lpszTemplate;
	dc << "\nm_bz.hResource = " << (UINT)m_bz.hResource;
	if (m_bz.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";
	dc << "\nm_bz.hTask = " << (UINT)m_bz.hTask;

	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleDialog, CDialog)
IMPLEMENT_DYNAMIC(COleBusyDialog, COleDialog)

/////////////////////////////////////////////////////////////////////////////
