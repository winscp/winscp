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

BOOL AFXAPI AfxWinInit(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPTSTR lpCmdLine, int nCmdShow)
{
	ASSERT(hPrevInstance == NULL);

	// handle critical errors and avoid Windows message boxes
	SetErrorMode(SetErrorMode(0) |
		SEM_FAILCRITICALERRORS|SEM_NOOPENFILEERRORBOX);

	// set resource handles
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	pModuleState->m_hCurrentInstanceHandle = hInstance;
	pModuleState->m_hCurrentResourceHandle = hInstance;

	// fill in the initial state for the application
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
	{
		// Windows specific initialization (not done if no CWinApp)
		pApp->m_hInstance = hInstance;
		pApp->m_hPrevInstance = hPrevInstance;
		pApp->m_lpCmdLine = lpCmdLine;
		pApp->m_nCmdShow = nCmdShow;
		pApp->SetCurrentHandles();
	}

	// initialize thread specific data (for main thread)
	if (!afxContextIsDLL)
		AfxInitThread();

	return TRUE;
}

///////////////////////////////////////////////////////////////////////////
// CWinApp Initialization

void CWinApp::SetCurrentHandles()
{
	ASSERT(this == afxCurrentWinApp);
	ASSERT(afxCurrentAppName == NULL);

	AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
	pModuleState->m_hCurrentInstanceHandle = m_hInstance;
	pModuleState->m_hCurrentResourceHandle = m_hInstance;

	// Note: there are a number of _tcsdup (aka strdup) calls that are
	// made here for the exe path, help file path, etc.  In previous
	// versions of MFC, this memory was never freed.  In this and future
	// versions this memory is automatically freed during CWinApp's
	// destructor.  If you are freeing the memory yourself, you should
	// either remove the code or set the pointers to NULL after freeing
	// the memory.

	// get path of executable
	TCHAR szBuff[_MAX_PATH];
	VERIFY(::GetModuleFileName(m_hInstance, szBuff, _MAX_PATH));

	LPTSTR lpszExt = _tcsrchr(szBuff, '.');
	ASSERT(lpszExt != NULL);
	ASSERT(*lpszExt == '.');
	*lpszExt = 0;       // no suffix

	TCHAR szExeName[_MAX_PATH];
	TCHAR szTitle[256];
	// get the exe title from the full path name [no extension]
	VERIFY(AfxGetFileName(szBuff, szExeName, _MAX_PATH) == 0);
	if (m_pszExeName == NULL)
	{
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
		m_pszExeName = _tcsdup(szExeName); // save non-localized name
		AfxEnableMemoryTracking(bEnable);
	}

	// m_pszAppName is the name used to present to the user
	if (m_pszAppName == NULL)
	{
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
		if (AfxLoadString(AFX_IDS_APP_TITLE, szTitle) != 0)
			m_pszAppName = _tcsdup(szTitle);    // human readable title
		else
			m_pszAppName = _tcsdup(m_pszExeName);   // same as EXE
		AfxEnableMemoryTracking(bEnable);
	}

	pModuleState->m_lpszCurrentAppName = m_pszAppName;
	ASSERT(afxCurrentAppName != NULL);

	// get path of .HLP file
	if (m_pszHelpFilePath == NULL)
	{
		lstrcpy(lpszExt, _T(".HLP"));
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
		m_pszHelpFilePath = _tcsdup(szBuff);
		AfxEnableMemoryTracking(bEnable);
		*lpszExt = '\0';       // back to no suffix
	}

	if (m_pszProfileName == NULL)
	{
		lstrcat(szExeName, _T(".INI")); // will be enough room in buffer
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
		m_pszProfileName = _tcsdup(szExeName);
		AfxEnableMemoryTracking(bEnable);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CFile implementation helpers

#ifdef AfxGetFileName
#undef AfxGetFileName
#endif

UINT AFXAPI AfxGetFileName(LPCTSTR lpszPathName, LPTSTR lpszTitle, UINT nMax)
{
	ASSERT(lpszTitle == NULL ||
		AfxIsValidAddress(lpszTitle, _MAX_FNAME));
	ASSERT(AfxIsValidString(lpszPathName));

	// always capture the complete file name including extension (if present)
	LPTSTR lpszTemp = (LPTSTR)lpszPathName;
	for (LPCTSTR lpsz = lpszPathName; *lpsz != '\0'; lpsz = _tcsinc(lpsz))
	{
		// remember last directory/drive separator
		if (*lpsz == '\\' || *lpsz == '/' || *lpsz == ':')
			lpszTemp = (LPTSTR)_tcsinc(lpsz);
	}

	// lpszTitle can be NULL which just returns the number of bytes
	if (lpszTitle == NULL)
		return lstrlen(lpszTemp)+1;

	// otherwise copy it into the buffer provided
	lstrcpyn(lpszTitle, lpszTemp, nMax);
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
