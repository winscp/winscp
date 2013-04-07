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
#pragma warning(disable: 4228)
#include <mapi.h>
#pragma warning(default: 4228)

#ifdef AFX_MAPI_SEG
#pragma code_seg(AFX_MAPI_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// MAPI implementation helpers and globals

AFX_STATIC_DATA BOOL _afxIsMailAvail = (BOOL)-1;    // start out not determined

/////////////////////////////////////////////////////////////////////////////
// _AFX_MAIL_STATE

class _AFX_MAIL_STATE : public CNoTrackObject
{
public:
	HINSTANCE m_hInstMail;      // handle to MAPI32.DLL
	virtual ~_AFX_MAIL_STATE();
};

_AFX_MAIL_STATE::~_AFX_MAIL_STATE()
{
	if (m_hInstMail != NULL)
		::FreeLibrary(m_hInstMail);
}

EXTERN_PROCESS_LOCAL(_AFX_MAIL_STATE, _afxMailState)

/////////////////////////////////////////////////////////////////////////////
// CDocument MAPI support

void CDocument::OnFileSendMail()
{
	ASSERT_VALID(this);
	ASSERT(_afxIsMailAvail);   // update handler always gets called first

	CWaitCursor wait;

	_AFX_MAIL_STATE* pMailState = _afxMailState;
	if (pMailState->m_hInstMail == NULL)
		pMailState->m_hInstMail = ::LoadLibraryA("MAPI32.DLL");

	if (pMailState->m_hInstMail == NULL)
	{
		AfxMessageBox(AFX_IDP_FAILED_MAPI_LOAD);
		return;
	}
	ASSERT(pMailState->m_hInstMail != NULL);

	ULONG (PASCAL *lpfnSendMail)(ULONG, ULONG, MapiMessage*, FLAGS, ULONG);
	(FARPROC&)lpfnSendMail = GetProcAddress(pMailState->m_hInstMail, "MAPISendMail");
	if (lpfnSendMail == NULL)
	{
		AfxMessageBox(AFX_IDP_INVALID_MAPI_DLL);
		return;
	}
	ASSERT(lpfnSendMail != NULL);

	TCHAR szTempName[_MAX_PATH];
	TCHAR szPath[_MAX_PATH];
	BOOL bRemoveTemp = FALSE;
	if (m_strPathName.IsEmpty() || IsModified())
	{
		// save to temporary path
		VERIFY(GetTempPath(_countof(szPath), szPath) != 0);
		VERIFY(GetTempFileName(szPath, _T("afx"), 0, szTempName) != 0);

		// save it, but remember original modified flag
		BOOL bModified = IsModified();
		BOOL bResult = DoSave(szTempName, FALSE);
		SetModifiedFlag(bModified);
		if (!bResult)
		{
			TRACE0("Warning: file save failed during File.Send Mail.\n");
			return;
		}
		bRemoveTemp = TRUE;
	}
	else
	{
		// use actual file since it isn't modified
		lstrcpyn(szTempName, m_strPathName, _countof(szTempName));
	}
#ifdef _UNICODE
	char szTempNameA[_MAX_PATH];
	_wcstombsz(szTempNameA, szTempName, _countof(szTempNameA));
#endif

	// build an appropriate title for the attachment
	TCHAR szTitle[_MAX_PATH];
	if (!m_strPathName.IsEmpty())
		AfxGetFileName(m_strPathName, szTitle, _countof(szTitle));
	else
	{
		lstrcpyn(szTitle, m_strTitle, _countof(szTitle));
		if (m_strTitle.Find('.') == -1) // no extension
		{
			// append the default suffix if there is one
			CString strExt;
			CDocTemplate* pTemplate = GetDocTemplate();
			if (pTemplate != NULL &&
				pTemplate->GetDocString(strExt, CDocTemplate::filterExt))
			{
				lstrcat(szTitle, strExt);
			}
		}
	}

#ifdef _UNICODE
	char szTitleA[_MAX_PATH];
	_wcstombsz(szTitleA, szTitle, _countof(szTitleA));
#endif

	// prepare the file description (for the attachment)
	MapiFileDesc fileDesc;
	memset(&fileDesc, 0, sizeof(fileDesc));
	fileDesc.nPosition = (ULONG)-1;
#ifdef _UNICODE
	fileDesc.lpszPathName = szTempNameA;
	fileDesc.lpszFileName = szTitleA;
#else
	fileDesc.lpszPathName = szTempName;
	fileDesc.lpszFileName = szTitle;
#endif

	// prepare the message (empty with 1 attachment)
	MapiMessage message;
	memset(&message, 0, sizeof(message));
	message.nFileCount = 1;
	message.lpFiles = &fileDesc;

	// prepare for modal dialog box
	AfxGetApp()->EnableModeless(FALSE);
	HWND hWndTop;
	CWnd* pParentWnd = CWnd::GetSafeOwner(NULL, &hWndTop);

	// some extra precautions are required to use MAPISendMail as it
	// tends to enable the parent window in between dialogs (after
	// the login dialog, but before the send note dialog).
	pParentWnd->SetCapture();
	::SetFocus(NULL);
	pParentWnd->m_nFlags |= WF_STAYDISABLED;

	int nError = lpfnSendMail(0, (ULONG)pParentWnd->GetSafeHwnd(),
		&message, MAPI_LOGON_UI|MAPI_DIALOG, 0);

	// after returning from the MAPISendMail call, the window must
	// be re-enabled and focus returned to the frame to undo the workaround
	// done before the MAPI call.
	::ReleaseCapture();
	pParentWnd->m_nFlags &= ~WF_STAYDISABLED;

	pParentWnd->EnableWindow(TRUE);
	::SetActiveWindow(NULL);
	pParentWnd->SetActiveWindow();
	pParentWnd->SetFocus();
	if (hWndTop != NULL)
		::EnableWindow(hWndTop, TRUE);
	AfxGetApp()->EnableModeless(TRUE);

	if (nError != SUCCESS_SUCCESS &&
		nError != MAPI_USER_ABORT && nError != MAPI_E_LOGIN_FAILURE)
	{
		AfxMessageBox(AFX_IDP_FAILED_MAPI_SEND);
	}

	// remove temporary file, if temporary file was used
	if (bRemoveTemp)
		CFile::Remove(szTempName);
}

void CDocument::OnUpdateFileSendMail(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);

	if (_afxIsMailAvail == (BOOL)-1)
	{
		_afxIsMailAvail = ::GetProfileInt(_T("MAIL"), _T("MAPI"), 0) != 0 &&
			SearchPath(NULL, _T("MAPI32.DLL"), NULL, 0, NULL, NULL) != 0;
	}

	// enable the Send... menu item if available
	pCmdUI->Enable(_afxIsMailAvail);
	CMenu* pMenu = pCmdUI->m_pMenu;
	if (!_afxIsMailAvail && pMenu != NULL)
	{
		// remove the Send... menu and surrounding separators
		UINT nStateAbove = pMenu->GetMenuState(pCmdUI->m_nIndex-1, MF_BYPOSITION);
		UINT nStateBelow = pMenu->GetMenuState(pCmdUI->m_nIndex+1, MF_BYPOSITION);
		pMenu->RemoveMenu(pCmdUI->m_nIndex, MF_BYPOSITION);
		if (nStateAbove & nStateBelow & MF_SEPARATOR)
		{
			// a separator must be removed since the Send... is gone
			if (nStateAbove != (UINT)-1)
				pMenu->RemoveMenu(pCmdUI->m_nIndex-1, MF_BYPOSITION);
			else if (nStateBelow != (UINT)-1)
				pMenu->RemoveMenu(pCmdUI->m_nIndex, MF_BYPOSITION);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleDocument MAPI support

void COleDocument::OnFileSendMail()
{
	ASSERT_VALID(this);
	ASSERT(m_bRemember);

	LPSTORAGE lpOrigStg = m_lpRootStg;
	m_lpRootStg = NULL;

	TRY
	{
		m_bRemember = FALSE;
		CDocument::OnFileSendMail();
	}
	CATCH_ALL(e)
	{
		m_lpRootStg = lpOrigStg;
		m_bRemember = TRUE;
		THROW_LAST();
	}
	END_CATCH_ALL

	m_lpRootStg = lpOrigStg;
	m_bRemember = TRUE;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_MAIL_STATE, _afxMailState)

/////////////////////////////////////////////////////////////////////////////
