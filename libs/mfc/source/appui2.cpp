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

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// DDE and ShellExecute support

BOOL CWinApp::ProcessShellCommand(CCommandLineInfo& rCmdInfo)
{
	BOOL bResult = TRUE;
	switch (rCmdInfo.m_nShellCommand)
	{
	case CCommandLineInfo::FileNew:
		if (!AfxGetApp()->OnCmdMsg(ID_FILE_NEW, 0, NULL, NULL))
			OnFileNew();
		if (m_pMainWnd == NULL)
			bResult = FALSE;
		break;

		// If we've been asked to open a file, call OpenDocumentFile()

	case CCommandLineInfo::FileOpen:
		if (!OpenDocumentFile(rCmdInfo.m_strFileName))
			bResult = FALSE;
		break;

		// If the user wanted to print, hide our main window and
		// fire a message to ourselves to start the printing

	case CCommandLineInfo::FilePrintTo:
	case CCommandLineInfo::FilePrint:
		m_nCmdShow = SW_HIDE;
		ASSERT(m_pCmdInfo == NULL);
		OpenDocumentFile(rCmdInfo.m_strFileName);
		m_pCmdInfo = &rCmdInfo;
		m_pMainWnd->SendMessage(WM_COMMAND, ID_FILE_PRINT_DIRECT);
		m_pCmdInfo = NULL;
		bResult = FALSE;
		break;

		// If we're doing DDE, hide ourselves

	case CCommandLineInfo::FileDDE:
		m_pCmdInfo = (CCommandLineInfo*)m_nCmdShow;
		m_nCmdShow = SW_HIDE;
		break;

	// If we've been asked to unregister, unregister and then terminate
	case CCommandLineInfo::AppUnregister:
		{
			UnregisterShellFileTypes();
			BOOL bUnregistered = Unregister();

			// if you specify /EMBEDDED, we won't make an success/failure box
			// this use of /EMBEDDED is not related to OLE

			if (!rCmdInfo.m_bRunEmbedded)
			{
				if (bUnregistered)
					AfxMessageBox(AFX_IDP_UNREG_DONE);
				else
					AfxMessageBox(AFX_IDP_UNREG_FAILURE);
			}
			bResult = FALSE;    // that's all we do

			// If nobody is using it already, we can use it.
			// We'll flag that we're unregistering and not save our state
			// on the way out. This new object gets deleted by the
			// app object destructor.

			if (m_pCmdInfo == NULL)
			{
				m_pCmdInfo = new CCommandLineInfo;
				m_pCmdInfo->m_nShellCommand = CCommandLineInfo::AppUnregister;
			}
		}
		break;
	}
	return bResult;
}


BOOL CWinApp::Unregister()
{
	HKEY    hKey = 0;
	TCHAR   szBuf[MAX_PATH+1];
	LONG    cSize;
	BOOL    bRet = TRUE;

	POSITION pos = GetFirstDocTemplatePosition();
	while (pos != NULL)
	{
		CDocTemplate* pTempl = GetNextDocTemplate(pos);
		if (pTempl != NULL)
			pTempl->OnCmdMsg(0, CN_OLE_UNREGISTER, NULL, NULL);
	}

	// Remove profile information -- the registry entries exist if
	// SetRegistryKey() was used.

	if (m_pszRegistryKey)
	{
		ASSERT(m_pszProfileName);

		CString strKey = _T("Software\\");
		strKey += m_pszRegistryKey;
		CString strSubKey = strKey + _T("\\") + m_pszProfileName;

		DelRegTree(HKEY_CURRENT_USER, strSubKey);

		// If registry key is empty then remove it

		DWORD   dwResult;
		if ((dwResult = ::RegOpenKey(HKEY_CURRENT_USER, strKey, &hKey)) ==
			ERROR_SUCCESS)
		{
			if (::RegEnumKey(hKey, 0, szBuf, _MAX_PATH) == ERROR_NO_MORE_ITEMS)
				DelRegTree(HKEY_CURRENT_USER, strKey);
			::RegCloseKey(hKey);
		}
		if (RegQueryValue(HKEY_CURRENT_USER, strSubKey, szBuf, &cSize) == ERROR_SUCCESS)
			bRet = TRUE;
	}
	return bRet;
}

// Under Win32, a reg key may not be deleted unless it is empty.
// Thus, to delete a tree,  one must recursively enumerate and
// delete all of the sub-keys.

LONG CWinApp::DelRegTree(HKEY hParentKey, const CString& strKeyName)
{
	return AfxDelRegTreeHelper(hParentKey, strKeyName);
}

LONG AFXAPI AfxDelRegTreeHelper(HKEY hParentKey, const CString& strKeyName)
{
	TCHAR   szSubKeyName[256];
	HKEY    hCurrentKey;
	DWORD   dwResult;

	if ((dwResult = RegOpenKey(hParentKey, strKeyName, &hCurrentKey)) ==
		ERROR_SUCCESS)
	{
		// Remove all subkeys of the key to delete
		while ((dwResult = RegEnumKey(hCurrentKey, 0, szSubKeyName, 255)) ==
				ERROR_SUCCESS)
		{
			if ((dwResult = AfxDelRegTreeHelper(hCurrentKey, szSubKeyName)) != ERROR_SUCCESS)
				break;
		}

		// If all went well, we should now be able to delete the requested key
		if ((dwResult == ERROR_NO_MORE_ITEMS) || (dwResult == ERROR_BADKEY))
		{
			dwResult = RegDeleteKey(hParentKey, strKeyName);
		}
	}

	RegCloseKey(hCurrentKey);
	return dwResult;
}

void CWinApp::EnableShellOpen()
{
	ASSERT(m_atomApp == NULL && m_atomSystemTopic == NULL); // do once

	m_atomApp = ::GlobalAddAtom(m_pszExeName);
	m_atomSystemTopic = ::GlobalAddAtom(_T("system"));
}

void CWinApp::RegisterShellFileTypes(BOOL bCompat)
{
	ASSERT(m_pDocManager != NULL);
	m_pDocManager->RegisterShellFileTypes(bCompat);
}

void CWinApp::RegisterShellFileTypesCompat()
{
	ASSERT(m_pDocManager != NULL);
	m_pDocManager->RegisterShellFileTypes(TRUE);
}

void CWinApp::UnregisterShellFileTypes()
{
	ASSERT(m_pDocManager != NULL);
	m_pDocManager->UnregisterShellFileTypes();
}

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

int CWinApp::GetOpenDocumentCount()
{
	ASSERT(m_pDocManager != NULL);
	return m_pDocManager->GetOpenDocumentCount();
}

/////////////////////////////////////////////////////////////////////////////
// Doc template support

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

void CWinApp::AddDocTemplate(CDocTemplate* pTemplate)
{
	if (m_pDocManager == NULL)
		m_pDocManager = new CDocManager;
	m_pDocManager->AddDocTemplate(pTemplate);
}

POSITION CWinApp::GetFirstDocTemplatePosition() const
{
	if (m_pDocManager == NULL)
		return NULL;
	return m_pDocManager->GetFirstDocTemplatePosition();
}

CDocTemplate* CWinApp::GetNextDocTemplate(POSITION& rPosition) const
{
	ASSERT(m_pDocManager != NULL);
	return m_pDocManager->GetNextDocTemplate(rPosition);
}

/////////////////////////////////////////////////////////////////////////////
