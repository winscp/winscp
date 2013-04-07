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
#include <process.h>    // for _beginthreadex and _endthreadex
#include <ddeml.h>  // for MSGF_DDEMGR

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Thread entry point

#ifdef _MT

struct _AFX_THREAD_STARTUP
{
	// following are "in" parameters to thread startup
	_AFX_THREAD_STATE* pThreadState;    // thread state of parent thread
	CWinThread* pThread;    // CWinThread for new thread
	DWORD dwCreateFlags;    // thread creation flags
	_PNH pfnNewHandler;     // new handler for new thread

	HANDLE hEvent;          // event triggered after success/non-success
	HANDLE hEvent2;         // event triggered after thread is resumed

	// strictly "out" -- set after hEvent is triggered
	BOOL bError;    // TRUE if error during startup
};

UINT APIENTRY _AfxThreadEntry(void* pParam)
{
	_AFX_THREAD_STARTUP* pStartup = (_AFX_THREAD_STARTUP*)pParam;
	ASSERT(pStartup != NULL);
	ASSERT(pStartup->pThreadState != NULL);
	ASSERT(pStartup->pThread != NULL);
	ASSERT(pStartup->hEvent != NULL);
	ASSERT(!pStartup->bError);

	CWinThread* pThread = pStartup->pThread;
	CWnd threadWnd;
	TRY
	{
		// inherit parent's module state
		_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
		pThreadState->m_pModuleState = pStartup->pThreadState->m_pModuleState;

		// set current thread pointer for AfxGetThread
		AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
#ifdef _AFXDLL
		pThread->m_pModuleState = pModuleState;
#endif
		AFX_MODULE_THREAD_STATE* pState = pModuleState->m_thread;
		pState->m_pCurrentWinThread = pThread;

		// forced initialization of the thread
		AfxInitThread();

		// thread inherits app's main window if not already set
		CWinApp* pApp = AfxGetApp();
		if (pApp != NULL &&
			pThread->m_pMainWnd == NULL && pApp->m_pMainWnd->GetSafeHwnd() != NULL)
		{
			// just attach the HWND
			threadWnd.Attach(pApp->m_pMainWnd->m_hWnd);
			pThread->m_pMainWnd = &threadWnd;
		}
	}
	CATCH_ALL(e)
	{
		// Note: DELETE_EXCEPTION(e) not required.

		// exception happened during thread initialization!!
		TRACE0("Warning: Error during thread initialization!\n");

		// set error flag and allow the creating thread to notice the error
		threadWnd.Detach();
		pStartup->bError = TRUE;
		VERIFY(::SetEvent(pStartup->hEvent));
		AfxEndThread((UINT)-1, FALSE);
		ASSERT(FALSE);  // unreachable
	}
	END_CATCH_ALL

	// pStartup is invaid after the following SetEvent (but hEvent2 is valid)
	HANDLE hEvent2 = pStartup->hEvent2;

	// allow the creating thread to return from CWinThread::CreateThread
	VERIFY(::SetEvent(pStartup->hEvent));

	// wait for thread to be resumed
	VERIFY(::WaitForSingleObject(hEvent2, INFINITE) == WAIT_OBJECT_0);
	::CloseHandle(hEvent2);

	// first -- check for simple worker thread
	DWORD nResult = 0;
	if (pThread->m_pfnThreadProc != NULL)
	{
		nResult = (*pThread->m_pfnThreadProc)(pThread->m_pThreadParams);
		ASSERT_VALID(pThread);
	}
	// else -- check for thread with message loop
	else if (!pThread->InitInstance())
	{
		ASSERT_VALID(pThread);
		nResult = pThread->ExitInstance();
	}
	else
	{
		// will stop after PostQuitMessage called
		ASSERT_VALID(pThread);
		nResult = pThread->Run();
	}

	// cleanup and shutdown the thread
	threadWnd.Detach();
	AfxEndThread(nResult);

	return 0;   // not reached
}

#endif //_MT

CWinThread* AFXAPI AfxGetThread()
{
	// check for current thread in module thread state
	AFX_MODULE_THREAD_STATE* pState = AfxGetModuleThreadState();
	CWinThread* pThread = pState->m_pCurrentWinThread;

	// if no CWinThread for the module, then use the global app
	if (pThread == NULL)
		pThread = AfxGetApp();

	return pThread;
}

CWinThread* AFXAPI AfxBeginThread(AFX_THREADPROC pfnThreadProc, LPVOID pParam,
	int nPriority, UINT nStackSize, DWORD dwCreateFlags,
	LPSECURITY_ATTRIBUTES lpSecurityAttrs)
{
#ifndef _MT
	pfnThreadProc;
	pParam;
	nPriority;
	nStackSize;
	dwCreateFlags;
	lpSecurityAttrs;

	return NULL;
#else
	ASSERT(pfnThreadProc != NULL);

	CWinThread* pThread = DEBUG_NEW CWinThread(pfnThreadProc, pParam);
	ASSERT_VALID(pThread);

	if (!pThread->CreateThread(dwCreateFlags|CREATE_SUSPENDED, nStackSize,
		lpSecurityAttrs))
	{
		pThread->Delete();
		return NULL;
	}
	VERIFY(pThread->SetThreadPriority(nPriority));
	if (!(dwCreateFlags & CREATE_SUSPENDED))
		VERIFY(pThread->ResumeThread() != (DWORD)-1);

	return pThread;
#endif //!_MT)
}

CWinThread* AFXAPI AfxBeginThread(CRuntimeClass* pThreadClass,
	int nPriority, UINT nStackSize, DWORD dwCreateFlags,
	LPSECURITY_ATTRIBUTES lpSecurityAttrs)
{
#ifndef _MT
	pThreadClass;
	nPriority;
	nStackSize;
	dwCreateFlags;
	lpSecurityAttrs;

	return NULL;
#else
	ASSERT(pThreadClass != NULL);
	ASSERT(pThreadClass->IsDerivedFrom(RUNTIME_CLASS(CWinThread)));

	CWinThread* pThread = (CWinThread*)pThreadClass->CreateObject();
	if (pThread == NULL)
		AfxThrowMemoryException();
	ASSERT_VALID(pThread);

	pThread->m_pThreadParams = NULL;
	if (!pThread->CreateThread(dwCreateFlags|CREATE_SUSPENDED, nStackSize,
		lpSecurityAttrs))
	{
		pThread->Delete();
		return NULL;
	}
	VERIFY(pThread->SetThreadPriority(nPriority));
	if (!(dwCreateFlags & CREATE_SUSPENDED))
		VERIFY(pThread->ResumeThread() != (DWORD)-1);

	return pThread;
#endif //!_MT
}

void AFXAPI AfxEndThread(UINT nExitCode, BOOL bDelete)
{
#ifndef _MT
	nExitCode;
	bDelete;
#else
	// remove current CWinThread object from memory
	AFX_MODULE_THREAD_STATE* pState = AfxGetModuleThreadState();
	CWinThread* pThread = pState->m_pCurrentWinThread;
	if (pThread != NULL)
	{
		ASSERT_VALID(pThread);
		ASSERT(pThread != AfxGetApp());

		// cleanup OLE if required
		if (pThread->m_lpfnOleTermOrFreeLib != NULL)
			(*pThread->m_lpfnOleTermOrFreeLib)(TRUE, FALSE);

		if (bDelete)
			pThread->Delete();
		pState->m_pCurrentWinThread = NULL;
	}

	// allow cleanup of any thread local objects
	AfxTermThread();

	// allow C-runtime to cleanup, and exit the thread
	_endthreadex(nExitCode);
#endif //!_MT
}

/////////////////////////////////////////////////////////////////////////////
// Global functions for thread initialization and thread cleanup

LRESULT CALLBACK _AfxMsgFilterHook(int code, WPARAM wParam, LPARAM lParam);

void AFXAPI AfxInitThread()
{
	if (!afxContextIsDLL)
	{
		// set message filter proc
		_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
		ASSERT(pThreadState->m_hHookOldMsgFilter == NULL);
		pThreadState->m_hHookOldMsgFilter = ::SetWindowsHookEx(WH_MSGFILTER,
			_AfxMsgFilterHook, NULL, ::GetCurrentThreadId());

#ifndef _AFX_NO_CTL3D_SUPPORT
		// intialize CTL3D for this thread
		_AFX_CTL3D_STATE* pCtl3dState = _afxCtl3dState;
		if (pCtl3dState->m_pfnAutoSubclass != NULL)
			(*pCtl3dState->m_pfnAutoSubclass)(AfxGetInstanceHandle());

		// allocate thread local _AFX_CTL3D_THREAD just for automatic termination
		_AFX_CTL3D_THREAD* pTemp = _afxCtl3dThread;
		pTemp;  // avoid unused warning
#endif
	}
}

extern CThreadSlotData* _afxThreadData;
void AFXAPI AfxTermThread(HINSTANCE hInstTerm)
{
#ifdef _DEBUG
	// check for missing AfxLockTempMap calls
	if (AfxGetModuleThreadState()->m_nTempMapLock != 0)
	{
		TRACE1("Warning: Temp map lock count non-zero (%ld).\n",
			AfxGetModuleThreadState()->m_nTempMapLock);
	}
#endif
	AfxLockTempMaps();
	AfxUnlockTempMaps(-1);

	// cleanup thread local tooltip window
	if (hInstTerm == NULL)
	{
		_AFX_THREAD_STATE* pThreadState = _afxThreadState.GetDataNA();
		if ((pThreadState != NULL) &&
			(pThreadState->m_pToolTip != NULL))
		{
			pThreadState->m_pToolTip->DestroyWindow();
			delete pThreadState->m_pToolTip;
			pThreadState->m_pToolTip=NULL;
		}
	}

	// cleanup the rest of the thread local data
	if (_afxThreadData != NULL)
		_afxThreadData->DeleteValues(hInstTerm, FALSE);
}

/////////////////////////////////////////////////////////////////////////////
// CWinThread construction

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

CWinThread::CWinThread(AFX_THREADPROC pfnThreadProc, LPVOID pParam)
{
	m_pfnThreadProc = pfnThreadProc;
	m_pThreadParams = pParam;

	CommonConstruct();
}

CWinThread::CWinThread()
{
	m_pThreadParams = NULL;
	m_pfnThreadProc = NULL;

	CommonConstruct();
}

void CWinThread::CommonConstruct()
{
	m_pMainWnd = NULL;
	m_pActiveWnd = NULL;

	// no HTHREAD until it is created
	m_hThread = NULL;
	m_nThreadID = 0;

	// initialize message pump
#ifdef _DEBUG
	m_nDisablePumpCount = 0;
#endif
	m_msgCur.message = WM_NULL;
	m_nMsgLast = WM_NULL;
	::GetCursorPos(&m_ptCursorLast);

	// most threads are deleted when not needed
	m_bAutoDelete = TRUE;

	// initialize OLE state
	m_pMessageFilter = NULL;
	m_lpfnOleTermOrFreeLib = NULL;
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

CWinThread::~CWinThread()
{
	// free thread object
	if (m_hThread != NULL)
		CloseHandle(m_hThread);

	// cleanup module state
	AFX_MODULE_THREAD_STATE* pState = AfxGetModuleThreadState();
	if (pState->m_pCurrentWinThread == this)
		pState->m_pCurrentWinThread = NULL;
}

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

BOOL CWinThread::CreateThread(DWORD dwCreateFlags, UINT nStackSize,
	LPSECURITY_ATTRIBUTES lpSecurityAttrs)
{
#ifndef _MT
	dwCreateFlags;
	nStackSize;
	lpSecurityAttrs;

	return FALSE;
#else
	ASSERT(m_hThread == NULL);  // already created?

	// setup startup structure for thread initialization
	_AFX_THREAD_STARTUP startup; memset(&startup, 0, sizeof(startup));
	startup.pThreadState = AfxGetThreadState();
	startup.pThread = this;
	startup.hEvent = ::CreateEvent(NULL, TRUE, FALSE, NULL);
	startup.hEvent2 = ::CreateEvent(NULL, TRUE, FALSE, NULL);
	startup.dwCreateFlags = dwCreateFlags;
	if (startup.hEvent == NULL || startup.hEvent2 == NULL)
	{
		TRACE0("Warning: CreateEvent failed in CWinThread::CreateThread.\n");
		if (startup.hEvent != NULL)
			::CloseHandle(startup.hEvent);
		if (startup.hEvent2 != NULL)
			::CloseHandle(startup.hEvent2);
		return FALSE;
	}

	// create the thread (it may or may not start to run)
	m_hThread = (HANDLE)_beginthreadex(lpSecurityAttrs, nStackSize,
		&_AfxThreadEntry, &startup, dwCreateFlags | CREATE_SUSPENDED, (UINT*)&m_nThreadID);
	if (m_hThread == NULL)
		return FALSE;

	// start the thread just for MFC initialization
	VERIFY(ResumeThread() != (DWORD)-1);
	VERIFY(::WaitForSingleObject(startup.hEvent, INFINITE) == WAIT_OBJECT_0);
	::CloseHandle(startup.hEvent);

	// if created suspended, suspend it until resume thread wakes it up
	if (dwCreateFlags & CREATE_SUSPENDED)
		VERIFY(::SuspendThread(m_hThread) != (DWORD)-1);

	// if error during startup, shut things down
	if (startup.bError)
	{
		VERIFY(::WaitForSingleObject(m_hThread, INFINITE) == WAIT_OBJECT_0);
		::CloseHandle(m_hThread);
		m_hThread = NULL;
		::CloseHandle(startup.hEvent2);
		return FALSE;
	}

	// allow thread to continue, once resumed (it may already be resumed)
	::SetEvent(startup.hEvent2);
	return TRUE;
#endif //!_MT
}

void CWinThread::Delete()
{
	// delete thread if it is auto-deleting
	if (m_bAutoDelete)
		delete this;
}

/////////////////////////////////////////////////////////////////////////////
// CWinThread default implementation

BOOL CWinThread::InitInstance()
{
	ASSERT_VALID(this);

	return FALSE;   // by default don't enter run loop
}

// main running routine until thread exits
int CWinThread::Run()
{
	ASSERT_VALID(this);

	// for tracking the idle time state
	BOOL bIdle = TRUE;
	LONG lIdleCount = 0;

	// acquire and dispatch messages until a WM_QUIT message is received.
	for (;;)
	{
		// phase1: check to see if we can do idle work
		while (bIdle &&
			!::PeekMessage(&m_msgCur, NULL, NULL, NULL, PM_NOREMOVE))
		{
			// call OnIdle while in bIdle state
			if (!OnIdle(lIdleCount++))
				bIdle = FALSE; // assume "no idle" state
		}

		// phase2: pump messages while available
		do
		{
			// pump message, but quit on WM_QUIT
			if (!PumpMessage())
				return ExitInstance();

			// reset "no idle" state after pumping "normal" message
			if (IsIdleMessage(&m_msgCur))
			{
				bIdle = TRUE;
				lIdleCount = 0;
			}

		} while (::PeekMessage(&m_msgCur, NULL, NULL, NULL, PM_NOREMOVE));
	}

	ASSERT(FALSE);  // not reachable
}

BOOL CWinThread::IsIdleMessage(MSG* pMsg)
{
	// Return FALSE if the message just dispatched should _not_
	// cause OnIdle to be run.  Messages which do not usually
	// affect the state of the user interface and happen very
	// often are checked for.

	// redundant WM_MOUSEMOVE and WM_NCMOUSEMOVE
	if (pMsg->message == WM_MOUSEMOVE || pMsg->message == WM_NCMOUSEMOVE)
	{
		// mouse move at same position as last mouse move?
		if (m_ptCursorLast == pMsg->pt && pMsg->message == m_nMsgLast)
			return FALSE;

		m_ptCursorLast = pMsg->pt;  // remember for next time
		m_nMsgLast = pMsg->message;
		return TRUE;
	}

	// WM_PAINT and WM_SYSTIMER (caret blink)
	return pMsg->message != WM_PAINT && pMsg->message != 0x0118;
}

int CWinThread::ExitInstance()
{
	ASSERT_VALID(this);
	ASSERT(AfxGetApp() != this);

	int nResult = m_msgCur.wParam;  // returns the value from PostQuitMessage
	return nResult;
}

BOOL CWinThread::OnIdle(LONG lCount)
{
	ASSERT_VALID(this);

#if defined(_DEBUG) && !defined(_AFX_NO_DEBUG_CRT)
	// check MFC's allocator (before idle)
	if (_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG) & _CRTDBG_CHECK_ALWAYS_DF)
		ASSERT(AfxCheckMemory());
#endif

	if (lCount <= 0)
	{
		// send WM_IDLEUPDATECMDUI to the main window
		CWnd* pMainWnd = m_pMainWnd;
		if (pMainWnd != NULL && pMainWnd->m_hWnd != NULL &&
			pMainWnd->IsWindowVisible())
		{
			AfxCallWndProc(pMainWnd, pMainWnd->m_hWnd,
				WM_IDLEUPDATECMDUI, (WPARAM)TRUE, 0);
			pMainWnd->SendMessageToDescendants(WM_IDLEUPDATECMDUI,
				(WPARAM)TRUE, 0, TRUE, TRUE);
		}
		// send WM_IDLEUPDATECMDUI to all frame windows
		AFX_MODULE_THREAD_STATE* pState = _AFX_CMDTARGET_GETSTATE()->m_thread;
		CFrameWnd* pFrameWnd = pState->m_frameList;
		while (pFrameWnd != NULL)
		{
			if (pFrameWnd->m_hWnd != NULL && pFrameWnd != pMainWnd)
			{
				if (pFrameWnd->m_nShowDelay == SW_HIDE)
					pFrameWnd->ShowWindow(pFrameWnd->m_nShowDelay);
				if (pFrameWnd->IsWindowVisible() ||
					pFrameWnd->m_nShowDelay >= 0)
				{
					AfxCallWndProc(pFrameWnd, pFrameWnd->m_hWnd,
						WM_IDLEUPDATECMDUI, (WPARAM)TRUE, 0);
					pFrameWnd->SendMessageToDescendants(WM_IDLEUPDATECMDUI,
						(WPARAM)TRUE, 0, TRUE, TRUE);
				}
				if (pFrameWnd->m_nShowDelay > SW_HIDE)
					pFrameWnd->ShowWindow(pFrameWnd->m_nShowDelay);
				pFrameWnd->m_nShowDelay = -1;
			}
			pFrameWnd = pFrameWnd->m_pNextFrameWnd;
		}
	}
	else if (lCount >= 0)
	{
		AFX_MODULE_THREAD_STATE* pState = _AFX_CMDTARGET_GETSTATE()->m_thread;
		if (pState->m_nTempMapLock == 0)
		{
			// free temp maps, OLE DLLs, etc.
			AfxLockTempMaps();
			AfxUnlockTempMaps();
		}
	}

#if defined(_DEBUG) && !defined(_AFX_NO_DEBUG_CRT)
	// check MFC's allocator (after idle)
	if (_CrtSetDbgFlag(_CRTDBG_REPORT_FLAG) & _CRTDBG_CHECK_ALWAYS_DF)
		ASSERT(AfxCheckMemory());
#endif

	return lCount < 0;  // nothing more to do if lCount >= 0
}

void CWinThread::DispatchThreadMessage(MSG* pMsg)
{
	DispatchThreadMessageEx(pMsg);
}

BOOL CWinThread::DispatchThreadMessageEx(MSG* pMsg)
{
	const AFX_MSGMAP* pMessageMap; pMessageMap = GetMessageMap();
	const AFX_MSGMAP_ENTRY* lpEntry;

#ifdef _AFXDLL
	for (/* pMessageMap already init'ed */; pMessageMap != NULL;
		pMessageMap = (*pMessageMap->pfnGetBaseMap)())
#else
	for (/* pMessageMap already init'ed */; pMessageMap != NULL;
		pMessageMap = pMessageMap->pBaseMap)
#endif

	{
		// Note: catch not so common but fatal mistake!!
		//      BEGIN_MESSAGE_MAP(CMyThread, CMyThread)
#ifdef _AFXDLL
		ASSERT(pMessageMap != (*pMessageMap->pfnGetBaseMap)());
#else
		ASSERT(pMessageMap != pMessageMap->pBaseMap);
#endif

		if (pMsg->message < 0xC000)
		{
			// constant window message
			if ((lpEntry = AfxFindMessageEntry(pMessageMap->lpEntries,
				pMsg->message, 0, 0)) != NULL)
				goto LDispatch;
		}
		else
		{
			// registered windows message
			lpEntry = pMessageMap->lpEntries;
			while ((lpEntry = AfxFindMessageEntry(lpEntry, 0xC000, 0, 0)) != NULL)
			{
				UINT* pnID = (UINT*)(lpEntry->nSig);
				ASSERT(*pnID >= 0xC000);
					// must be successfully registered
				if (*pnID == pMsg->message)
					goto LDispatch;
				lpEntry++;      // keep looking past this one
			}
		}
	}
	return FALSE;

LDispatch:
	union MessageMapFunctions mmf;
	mmf.pfn = lpEntry->pfn;

	// always posted, so return value is meaningless

	(this->*mmf.pfn_THREAD)(pMsg->wParam, pMsg->lParam);
	return TRUE;
}

BOOL CWinThread::PreTranslateMessage(MSG* pMsg)
{
	ASSERT_VALID(this);

	// if this is a thread-message, short-circuit this function
	if (pMsg->hwnd == NULL && DispatchThreadMessageEx(pMsg))
		return TRUE;

	// walk from target to main window
	CWnd* pMainWnd = AfxGetMainWnd();
	if (CWnd::WalkPreTranslateTree(pMainWnd->GetSafeHwnd(), pMsg))
		return TRUE;

	// in case of modeless dialogs, last chance route through main
	//   window's accelerator table
	if (pMainWnd != NULL)
	{
		 CWnd* pWnd = CWnd::FromHandle(pMsg->hwnd);
		 if (pWnd->GetTopLevelParent() != pMainWnd)
			return pMainWnd->PreTranslateMessage(pMsg);
	}

	return FALSE;   // no special processing
}

LRESULT CWinThread::ProcessWndProcException(CException*, const MSG* pMsg)
{
	if (pMsg->message == WM_CREATE)
	{
		return -1;  // just fail
	}
	else if (pMsg->message == WM_PAINT)
	{
		// force validation of window to prevent getting WM_PAINT again
		ValidateRect(pMsg->hwnd, NULL);
		return 0;
	}
	return 0;   // sensible default for rest of commands
}

/////////////////////////////////////////////////////////////////////////////
// Message Filter processing (WH_MSGFILTER)

LRESULT CALLBACK _AfxMsgFilterHook(int code, WPARAM wParam, LPARAM lParam)
{
	CWinThread* pThread;
	if (afxContextIsDLL || (code < 0 && code != MSGF_DDEMGR) ||
		(pThread = AfxGetThread()) == NULL)
	{
		return ::CallNextHookEx(_afxThreadState->m_hHookOldMsgFilter,
			code, wParam, lParam);
	}
	ASSERT(pThread != NULL);
	return (LRESULT)pThread->ProcessMessageFilter(code, (LPMSG)lParam);
}

AFX_STATIC BOOL AFXAPI IsHelpKey(LPMSG lpMsg)
	// return TRUE only for non-repeat F1 keydowns.
{
	return lpMsg->message == WM_KEYDOWN &&
		   lpMsg->wParam == VK_F1 &&
		   !(HIWORD(lpMsg->lParam) & KF_REPEAT) &&
		   GetKeyState(VK_SHIFT) >= 0 &&
		   GetKeyState(VK_CONTROL) >= 0 &&
		   GetKeyState(VK_MENU) >= 0;
}

AFX_STATIC inline BOOL IsEnterKey(LPMSG lpMsg)
	{ return lpMsg->message == WM_KEYDOWN && lpMsg->wParam == VK_RETURN; }

AFX_STATIC inline BOOL IsButtonUp(LPMSG lpMsg)
	{ return lpMsg->message == WM_LBUTTONUP; }

BOOL CWinThread::ProcessMessageFilter(int code, LPMSG lpMsg)
{
	if (lpMsg == NULL)
		return FALSE;   // not handled

	CFrameWnd* pTopFrameWnd;
	CWnd* pMainWnd;
	CWnd* pMsgWnd;
	switch (code)
	{
	case MSGF_DDEMGR:
		// Unlike other WH_MSGFILTER codes, MSGF_DDEMGR should
		//  never call the next hook.
		// By returning FALSE, the message will be dispatched
		//  instead (the default behavior).
		return FALSE;

	case MSGF_MENU:
		pMsgWnd = CWnd::FromHandle(lpMsg->hwnd);
		if (pMsgWnd != NULL)
		{
			pTopFrameWnd = pMsgWnd->GetTopLevelFrame();
			if (pTopFrameWnd != NULL && pTopFrameWnd->IsTracking() &&
				pTopFrameWnd->m_bHelpMode)
			{
				pMainWnd = AfxGetMainWnd();
				if ((m_pMainWnd != NULL) && (IsEnterKey(lpMsg) || IsButtonUp(lpMsg)))
				{
					pMainWnd->SendMessage(WM_COMMAND, ID_HELP);
					return TRUE;
				}
			}
		}
		// fall through...

	case MSGF_DIALOGBOX:    // handles message boxes as well.
		pMainWnd = AfxGetMainWnd();
		if (afxData.nWinVer < 0x333 && pMainWnd != NULL && IsHelpKey(lpMsg))
		{
			pMainWnd->SendMessage(WM_COMMAND, ID_HELP);
			return TRUE;
		}
		if (code == MSGF_DIALOGBOX && m_pActiveWnd != NULL &&
			lpMsg->message >= WM_KEYFIRST && lpMsg->message <= WM_KEYLAST)
		{
			// need to translate messages for the in-place container
			_AFX_THREAD_STATE* pThreadState = _afxThreadState.GetData();
			if (pThreadState->m_bInMsgFilter)
				return FALSE;
			pThreadState->m_bInMsgFilter = TRUE;    // avoid reentering this code
			MSG msg = *lpMsg;
			if (m_pActiveWnd->IsWindowEnabled() && PreTranslateMessage(&msg))
			{
				pThreadState->m_bInMsgFilter = FALSE;
				return TRUE;
			}
			pThreadState->m_bInMsgFilter = FALSE;    // ok again
		}
		break;
	}

	return FALSE;   // default to not handled
}

/////////////////////////////////////////////////////////////////////////////
// Access to m_pMainWnd & m_pActiveWnd

CWnd* CWinThread::GetMainWnd()
{
	if (m_pActiveWnd != NULL)
		return m_pActiveWnd;    // probably in-place active

	// when not inplace active, just return main window
	if (m_pMainWnd != NULL)
		return m_pMainWnd;

	return CWnd::GetActiveWindow();
}

/////////////////////////////////////////////////////////////////////////////
// CWinThread implementation helpers

BOOL CWinThread::PumpMessage()
{
	ASSERT_VALID(this);

	if (!::GetMessage(&m_msgCur, NULL, NULL, NULL))
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceAppMsg)
			TRACE0("CWinThread::PumpMessage - Received WM_QUIT.\n");
		m_nDisablePumpCount++; // application must die
			// Note: prevents calling message loop things in 'ExitInstance'
			// will never be decremented
#endif
		return FALSE;
	}

#ifdef _DEBUG
	if (m_nDisablePumpCount != 0)
	{
		TRACE0("Error: CWinThread::PumpMessage called when not permitted.\n");
		ASSERT(FALSE);
	}
#endif

#ifdef _DEBUG
	if (afxTraceFlags & traceAppMsg)
		_AfxTraceMsg(_T("PumpMessage"), &m_msgCur);
#endif

	// process this message

	if (m_msgCur.message != WM_KICKIDLE && !PreTranslateMessage(&m_msgCur))
	{
		::TranslateMessage(&m_msgCur);
		::DispatchMessage(&m_msgCur);
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CWinThread diagnostics

#ifdef _DEBUG
void CWinThread::AssertValid() const
{
	CCmdTarget::AssertValid();
}

void CWinThread::Dump(CDumpContext& dc) const
{
	CCmdTarget::Dump(dc);

	dc << "m_pThreadParams = " << m_pThreadParams;
	dc << "\nm_pfnThreadProc = " << (void*)m_pfnThreadProc;
	dc << "\nm_bAutoDelete = " << m_bAutoDelete;
	dc << "\nm_hThread = " << (void*)m_hThread;
	dc << "\nm_nThreadID = " << m_nThreadID;
	dc << "\nm_nDisablePumpCount = " << m_nDisablePumpCount;
	if (AfxGetThread() == this)
		dc << "\nm_pMainWnd = " << m_pMainWnd;

	dc << "\nm_msgCur = {";
	dc << "\n\thwnd = " << (UINT)m_msgCur.hwnd;
	dc << "\n\tmessage = " << (UINT)m_msgCur.message;
	dc << "\n\twParam = " << (UINT)m_msgCur.wParam;
	dc << "\n\tlParam = " << (void*)m_msgCur.lParam;
	dc << "\n\ttime = " << m_msgCur.time;
	dc << "\n\tpt = " << CPoint(m_msgCur.pt);
	dc << "\n}";

	dc << "\nm_pThreadParams = " << m_pThreadParams;
	dc << "\nm_pfnThreadProc = " << (void*)m_pfnThreadProc;
	dc << "\nm_ptCursorLast = " << m_ptCursorLast;
	dc << "\nm_nMsgLast = " << m_nMsgLast;

	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CWinThread, CCmdTarget)

/////////////////////////////////////////////////////////////////////////////
