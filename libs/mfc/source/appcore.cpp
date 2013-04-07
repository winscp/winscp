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
#include <malloc.h>

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

AFX_STATIC_DATA const TCHAR _afxFileSection[] = _T("Recent File List");
AFX_STATIC_DATA const TCHAR _afxFileEntry[] = _T("File%d");
AFX_STATIC_DATA const TCHAR _afxPreviewSection[] = _T("Settings");
AFX_STATIC_DATA const TCHAR _afxPreviewEntry[] = _T("PreviewPages");

/////////////////////////////////////////////////////////////////////////////
// globals (internal library use)

// CDocManager statics are in this file for granularity reasons
BOOL CDocManager::bStaticInit = TRUE;
CDocManager* CDocManager::pStaticDocManager = NULL;
CPtrList* CDocManager::pStaticList = NULL;

BEGIN_MESSAGE_MAP(CWinApp, CCmdTarget)
	//{{AFX_MSG_MAP(CWinApp)
	// Global File commands
	ON_COMMAND(ID_APP_EXIT, OnAppExit)
	// MRU - most recently used file menu
	ON_UPDATE_COMMAND_UI(ID_FILE_MRU_FILE1, OnUpdateRecentFileMenu)
	ON_COMMAND_EX_RANGE(ID_FILE_MRU_FILE1, ID_FILE_MRU_FILE16, OnOpenRecentFile)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// _AFX_WIN_STATE implementation

#ifndef _AFX_NO_GRAYDLG_SUPPORT

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

_AFX_WIN_STATE::_AFX_WIN_STATE()
{
	// Note: it is only necessary to intialize non-zero data.
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

_AFX_WIN_STATE::~_AFX_WIN_STATE()
{
	AfxDeleteObject((HGDIOBJ*)&m_hDlgBkBrush);
}

#endif //!_AFX_NO_GRAYDLG_SUPPORT

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

CWinApp::CWinApp(LPCTSTR lpszAppName)
{
	if (lpszAppName != NULL)
		m_pszAppName = _tcsdup(lpszAppName);
	else
		m_pszAppName = NULL;

	// initialize CWinThread state
	AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
	AFX_MODULE_THREAD_STATE* pThreadState = pModuleState->m_thread;
	ASSERT(AfxGetThread() == NULL);
	pThreadState->m_pCurrentWinThread = this;
	ASSERT(AfxGetThread() == this);
	m_hThread = ::GetCurrentThread();
	m_nThreadID = ::GetCurrentThreadId();

	// initialize CWinApp state
	ASSERT(afxCurrentWinApp == NULL); // only one CWinApp object please
	pModuleState->m_pCurrentWinApp = this;
	ASSERT(AfxGetApp() == this);

	// in non-running state until WinMain
	m_hInstance = NULL;
	m_pszHelpFilePath = NULL;
	m_pszProfileName = NULL;
	m_pszRegistryKey = NULL;
	m_pszExeName = NULL;
	m_pRecentFileList = NULL;
	m_pDocManager = NULL;
	m_atomApp = m_atomSystemTopic = NULL;
	m_lpCmdLine = NULL;
	m_pCmdInfo = NULL;

	// initialize wait cursor state
	m_nWaitCursorCount = 0;
	m_hcurWaitCursorRestore = NULL;

	// initialize current printer state
	m_hDevMode = NULL;
	m_hDevNames = NULL;
	m_nNumPreviewPages = 0;     // not specified (defaults to 1)

	// initialize DAO state
	m_lpfnDaoTerm = NULL;   // will be set if AfxDaoInit called

	// other initialization
	m_bHelpMode = FALSE;
	m_nSafetyPoolSize = 512;        // default size
}

BOOL CWinApp::InitApplication()
{
	if (CDocManager::pStaticDocManager != NULL)
	{
		if (m_pDocManager == NULL)
			m_pDocManager = CDocManager::pStaticDocManager;
		CDocManager::pStaticDocManager = NULL;
	}

	if (m_pDocManager != NULL)
		m_pDocManager->AddDocTemplate(NULL);
	else
		CDocManager::bStaticInit = FALSE;

	return TRUE;
}

BOOL CWinApp::InitInstance()
{
	return TRUE;
}

void CWinApp::LoadStdProfileSettings(UINT nMaxMRU)
{
	ASSERT_VALID(this);
	ASSERT(m_pRecentFileList == NULL);

	if (nMaxMRU != 0)
	{
		// create file MRU since nMaxMRU not zero
		m_pRecentFileList = new CRecentFileList(0, _afxFileSection, _afxFileEntry,
			nMaxMRU);
		m_pRecentFileList->ReadList();
	}
	// 0 by default means not set
	m_nNumPreviewPages = GetProfileInt(_afxPreviewSection, _afxPreviewEntry, 0);
}

void CWinApp::ParseCommandLine(CCommandLineInfo& rCmdInfo)
{
	for (int i = 1; i < __argc; i++)
	{
		LPCTSTR pszParam = __targv[i];
		BOOL bFlag = FALSE;
		BOOL bLast = ((i + 1) == __argc);
		if (pszParam[0] == '-' || pszParam[0] == '/')
		{
			// remove flag specifier
			bFlag = TRUE;
			++pszParam;
		}
		rCmdInfo.ParseParam(pszParam, bFlag, bLast);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CCommandLineInfo implementation

CCommandLineInfo::CCommandLineInfo()
{
	m_bShowSplash = TRUE;
	m_bRunEmbedded = FALSE;
	m_bRunAutomated = FALSE;
	m_nShellCommand = FileNew;
}

CCommandLineInfo::~CCommandLineInfo()
{
}

void CCommandLineInfo::ParseParam(const TCHAR* pszParam,BOOL bFlag,BOOL bLast)
{
	if (bFlag)
	{
		USES_CONVERSION;
		ParseParamFlag(T2CA(pszParam));
	}
	else
		ParseParamNotFlag(pszParam);

	ParseLast(bLast);
}

#ifdef UNICODE
void CCommandLineInfo::ParseParam(const char* pszParam, BOOL bFlag, BOOL bLast)
{
	if (bFlag)
		ParseParamFlag(pszParam);
	else
		ParseParamNotFlag(pszParam);

	ParseLast(bLast);
}
#endif // UNICODE

void CCommandLineInfo::ParseParamFlag(const char* pszParam)
{
	// OLE command switches are case insensitive, while
	// shell command switches are case sensitive

	if (lstrcmpA(pszParam, "pt") == 0)
		m_nShellCommand = FilePrintTo;
	else if (lstrcmpA(pszParam, "p") == 0)
		m_nShellCommand = FilePrint;
	else if (lstrcmpiA(pszParam, "Unregister") == 0 ||
			 lstrcmpiA(pszParam, "Unregserver") == 0)
		m_nShellCommand = AppUnregister;
	else if (lstrcmpA(pszParam, "dde") == 0)
	{
		AfxOleSetUserCtrl(FALSE);
		m_nShellCommand = FileDDE;
	}
	else if (lstrcmpiA(pszParam, "Embedding") == 0)
	{
		AfxOleSetUserCtrl(FALSE);
		m_bRunEmbedded = TRUE;
		m_bShowSplash = FALSE;
	}
	else if (lstrcmpiA(pszParam, "Automation") == 0)
	{
		AfxOleSetUserCtrl(FALSE);
		m_bRunAutomated = TRUE;
		m_bShowSplash = FALSE;
	}
}

void CCommandLineInfo::ParseParamNotFlag(const TCHAR* pszParam)
{
	if (m_strFileName.IsEmpty())
		m_strFileName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strPrinterName.IsEmpty())
		m_strPrinterName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strDriverName.IsEmpty())
		m_strDriverName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strPortName.IsEmpty())
		m_strPortName = pszParam;
}

#ifdef UNICODE
void CCommandLineInfo::ParseParamNotFlag(const char* pszParam)
{
	if (m_strFileName.IsEmpty())
		m_strFileName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strPrinterName.IsEmpty())
		m_strPrinterName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strDriverName.IsEmpty())
		m_strDriverName = pszParam;
	else if (m_nShellCommand == FilePrintTo && m_strPortName.IsEmpty())
		m_strPortName = pszParam;
}
#endif

void CCommandLineInfo::ParseLast(BOOL bLast)
{
	if (bLast)
	{
		if (m_nShellCommand == FileNew && !m_strFileName.IsEmpty())
			m_nShellCommand = FileOpen;
		m_bShowSplash = !m_bRunEmbedded && !m_bRunAutomated;
	}
}

/////////////////////////////////////////////////////////////////////////////
// App termination

CWinApp::~CWinApp()
{
	// free doc manager
	if (m_pDocManager != NULL)
		delete m_pDocManager;

	// free recent file list
	if (m_pRecentFileList != NULL)
		delete m_pRecentFileList;

	// free static list of document templates
	if (!afxContextIsDLL)
	{
		if (CDocManager::pStaticList != NULL)
		{
			delete CDocManager::pStaticList;
			CDocManager::pStaticList = NULL;
		}
		if (CDocManager::pStaticDocManager != NULL)
		{
			delete CDocManager::pStaticDocManager;
			CDocManager::pStaticDocManager = NULL;
		}
	}

	// free printer info
	if (m_hDevMode != NULL)
		AfxGlobalFree(m_hDevMode);
	if (m_hDevNames != NULL)
		AfxGlobalFree(m_hDevNames);

	// free atoms if used
	if (m_atomApp != NULL)
		::GlobalDeleteAtom(m_atomApp);
	if (m_atomSystemTopic != NULL)
		::GlobalDeleteAtom(m_atomSystemTopic);

	// free cached commandline
	if (m_pCmdInfo != NULL)
		delete m_pCmdInfo;

	// cleanup module state
	AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
	if (pModuleState->m_lpszCurrentAppName == m_pszAppName)
		pModuleState->m_lpszCurrentAppName = NULL;
	if (pModuleState->m_pCurrentWinApp == this)
		pModuleState->m_pCurrentWinApp = NULL;

	// free various strings allocated with _tcsdup
	free((void*)m_pszAppName);
	free((void*)m_pszRegistryKey);
	free((void*)m_pszExeName);
	free((void*)m_pszHelpFilePath);
	free((void*)m_pszProfileName);

	// avoid calling CloseHandle() on our own thread handle
	// during the CWinThread destructor
	m_hThread = NULL;
}

void CWinApp::SaveStdProfileSettings()
{
	ASSERT_VALID(this);

	if (m_pRecentFileList != NULL)
		m_pRecentFileList->WriteList();

	if (m_nNumPreviewPages != 0)
		WriteProfileInt(_afxPreviewSection, _afxPreviewEntry, m_nNumPreviewPages);
}

int CWinApp::ExitInstance()
{
	// if we remember that we're unregistering,
	// don't save our profile settings

	if (m_pCmdInfo == NULL ||
		m_pCmdInfo->m_nShellCommand != CCommandLineInfo::AppUnregister)
	{
		if (!afxContextIsDLL)
			SaveStdProfileSettings();
	}

	// Cleanup DAO if necessary
	if (m_lpfnDaoTerm != NULL)
	{
		// If a DLL, YOU must call AfxDaoTerm prior to ExitInstance
		ASSERT(!afxContextIsDLL);
		(*m_lpfnDaoTerm)();
	}

	return m_msgCur.wParam; // returns the value from PostQuitMessage
}

/////////////////////////////////////////////////////////////////////////////

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

// Main running routine until application exits
int CWinApp::Run()
{
	if (m_pMainWnd == NULL && AfxOleGetUserCtrl())
	{
		// Not launched /Embedding or /Automation, but has no main window!
		TRACE0("Warning: m_pMainWnd is NULL in CWinApp::Run - quitting application.\n");
		AfxPostQuitMessage(0);
	}
	return CWinThread::Run();
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

void AFXAPI AfxPostQuitMessage(int nExitCode)
{
	// cleanup OLE libraries
	CWinThread* pThread = AfxGetThread();
	if (pThread != NULL && pThread->m_lpfnOleTermOrFreeLib != NULL)
		(*pThread->m_lpfnOleTermOrFreeLib)(TRUE, TRUE);

	::PostQuitMessage(nExitCode);
}

/////////////////////////////////////////////////////////////////////////////
// WinHelp Helper

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

void CWinApp::WinHelp(DWORD dwData, UINT nCmd)
{
	CWnd* pMainWnd = AfxGetMainWnd();
	ASSERT_VALID(pMainWnd);

	// return global app help mode state to FALSE (backward compatibility)
	m_bHelpMode = FALSE;
	pMainWnd->PostMessage(WM_KICKIDLE); // trigger idle update

	pMainWnd->WinHelp(dwData, nCmd);
}

/////////////////////////////////////////////////////////////////////////////
// Special exception handling

LRESULT CWinApp::ProcessWndProcException(CException* e, const MSG* pMsg)
{
	// handle certain messages in CWinThread
	switch (pMsg->message)
	{
	case WM_CREATE:
	case WM_PAINT:
		return CWinThread::ProcessWndProcException(e, pMsg);
	}

	// handle all the rest
	UINT nIDP = AFX_IDP_INTERNAL_FAILURE;   // generic message string
	LRESULT lResult = 0;        // sensible default
	if (pMsg->message == WM_COMMAND)
	{
		if ((HWND)pMsg->lParam == NULL)
			nIDP = AFX_IDP_COMMAND_FAILURE; // command (not from a control)
		lResult = (LRESULT)TRUE;        // pretend the command was handled
	}
	if (e->IsKindOf(RUNTIME_CLASS(CMemoryException)))
	{
		e->ReportError(MB_ICONEXCLAMATION|MB_SYSTEMMODAL, nIDP);
	}
	else if (!e->IsKindOf(RUNTIME_CLASS(CUserException)))
	{
		// user has not been alerted yet of this catastrophic problem
		e->ReportError(MB_ICONSTOP, nIDP);
	}
	return lResult; // sensible default return from most WndProc functions
}

/////////////////////////////////////////////////////////////////////////////
// CWinApp idle processing

BOOL CWinApp::OnIdle(LONG lCount)
{
	if (lCount <= 0)
	{
		CWinThread::OnIdle(lCount);

		// call doc-template idle hook
		POSITION pos = NULL;
		if (m_pDocManager != NULL)
			pos = m_pDocManager->GetFirstDocTemplatePosition();

		while (pos != NULL)
		{
			CDocTemplate* pTemplate = m_pDocManager->GetNextDocTemplate(pos);
			ASSERT_KINDOF(CDocTemplate, pTemplate);
			pTemplate->OnIdle();
		}
	}
	else if (lCount == 1)
	{
		VERIFY(!CWinThread::OnIdle(lCount));
	}
	return lCount < 1;  // more to do if lCount < 1
}

/////////////////////////////////////////////////////////////////////////////
// CWinApp idle processing

void CWinApp::DevModeChange(LPTSTR lpDeviceName)
{
	if (m_hDevNames == NULL)
		return;

	LPDEVNAMES lpDevNames = (LPDEVNAMES)::GlobalLock(m_hDevNames);
	ASSERT(lpDevNames != NULL);
	if (lstrcmp((LPCTSTR)lpDevNames + lpDevNames->wDeviceOffset,
		lpDeviceName) == 0)
	{
		HANDLE hPrinter;
		if (!OpenPrinter(lpDeviceName, &hPrinter, NULL))
			return;

		// DEVMODE changed for the current printer
		if (m_hDevMode != NULL)
			AfxGlobalFree(m_hDevMode);

		// A zero for last param returns the size of buffer needed.
		int nSize = DocumentProperties(NULL, hPrinter, lpDeviceName,
			NULL, NULL, 0);
		ASSERT(nSize >= 0);
		m_hDevMode = GlobalAlloc(GHND, nSize);
		LPDEVMODE lpDevMode = (LPDEVMODE)GlobalLock(m_hDevMode);

		// Fill in the rest of the structure.
		if (DocumentProperties(NULL, hPrinter, lpDeviceName, lpDevMode,
			NULL, DM_OUT_BUFFER) != IDOK)
		{
			AfxGlobalFree(m_hDevMode);
			m_hDevMode = NULL;
		}
		ClosePrinter(hPrinter);
	}
}

///////////////////////////////////////////////////////////////////////////
// CWinApp diagnostics

#ifdef _DEBUG
void CWinApp::AssertValid() const
{
	CWinThread::AssertValid();

	ASSERT(afxCurrentWinApp == this);
	ASSERT(afxCurrentInstanceHandle == m_hInstance);

	if (AfxGetThread() != (CWinThread*)this)
		return;     // only do subset if called from different thread

	if (m_pDocManager != NULL)
		ASSERT_VALID(m_pDocManager);
}

void CWinApp::Dump(CDumpContext& dc) const
{
	CWinThread::Dump(dc);

	dc << "m_hInstance = " << (UINT)m_hInstance;
	dc << "\nm_hPrevInstance = " << (UINT)m_hPrevInstance;
	dc << "\nm_lpCmdLine = " << m_lpCmdLine;
	dc << "\nm_nCmdShow = " << m_nCmdShow;
	dc << "\nm_pszAppName = " << m_pszAppName;
	dc << "\nm_bHelpMode = " << m_bHelpMode;
	dc << "\nm_pszExeName = " << m_pszExeName;
	dc << "\nm_pszHelpFilePath = " << m_pszHelpFilePath;
	dc << "\nm_pszProfileName = " << m_pszProfileName;
	dc << "\nm_hDevMode = " << (UINT)m_hDevMode;
	dc << "\nm_hDevNames = " << (UINT)m_hDevNames;
	dc << "\nm_dwPromptContext = " << m_dwPromptContext;

	if (m_pRecentFileList != NULL)
	{
		dc << "\nm_strRecentFiles[] = ";
		int nSize = m_pRecentFileList->GetSize();
		for (int i = 0; i < nSize; i++)
		{
			if ((*m_pRecentFileList)[i].GetLength() != 0)
				dc << "\n\tFile: " << (*m_pRecentFileList)[i];
		}
	}

	if (m_pDocManager != NULL)
		m_pDocManager->Dump(dc);

	dc << "\nm_nWaitCursorCount = " << m_nWaitCursorCount;
	dc << "\nm_hcurWaitCursorRestore = " << (UINT)m_hcurWaitCursorRestore;
	dc << "\nm_nNumPreviewPages = " << m_nNumPreviewPages;

	dc << "\nm_msgCur = {";
	dc << "\n\thwnd = " << (UINT)m_msgCur.hwnd;
	dc << "\n\tmessage = " << (UINT)m_msgCur.message;
	dc << "\n\twParam = " << (UINT)m_msgCur.wParam;
	dc << "\n\tlParam = " << (void*)m_msgCur.lParam;
	dc << "\n\ttime = " << m_msgCur.time;
	dc << "\n\tpt = " << CPoint(m_msgCur.pt);
	dc << "\n}";

	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CWinApp, CWinThread)

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_WIN_STATE, _afxWinState)

/////////////////////////////////////////////////////////////////////////////
