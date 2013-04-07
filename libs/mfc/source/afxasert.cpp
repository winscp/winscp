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

#ifdef _DEBUG   // entire file

#ifdef AFX_DBG1_SEG
#pragma code_seg(AFX_DBG1_SEG)
#endif

// NOTE: in separate module so it can replaced if needed

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#ifdef _AFX_NO_DEBUG_CRT
LONG afxAssertBusy = -1;
LONG afxAssertReallyBusy = -1;
BOOL (AFXAPI* afxAssertFailedLine)(LPCSTR, int);
#endif

BOOL AFXAPI AfxAssertFailedLine(LPCSTR lpszFileName, int nLine)
{
#ifndef _AFX_NO_DEBUG_CRT
	// we remove WM_QUIT because if it is in the queue then the message box
	// won't display
	MSG msg;
	BOOL bQuit = PeekMessage(&msg, NULL, WM_QUIT, WM_QUIT, PM_REMOVE);
	BOOL bResult = _CrtDbgReport(_CRT_ASSERT, lpszFileName, nLine, NULL, NULL);
	if (bQuit)
		PostQuitMessage(msg.wParam);
	return bResult;
#else
	TCHAR szMessage[_MAX_PATH*2];

	// handle the (hopefully rare) case of AfxGetAllocState ASSERT
	if (InterlockedIncrement(&afxAssertReallyBusy) > 0)
	{
		// assume the debugger or auxiliary port
		wsprintf(szMessage, _T("Assertion Failed: File %hs, Line %d\n"),
			lpszFileName, nLine);
		OutputDebugString(szMessage);
		InterlockedDecrement(&afxAssertReallyBusy);

		// assert w/in assert (examine call stack to determine first one)
		AfxDebugBreak();
		return FALSE;
	}

	// check for special hook function (for testing diagnostics)
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	InterlockedDecrement(&afxAssertReallyBusy);
	if (afxAssertFailedLine != NULL)
		return (*afxAssertFailedLine)(lpszFileName, nLine);

	// get app name or NULL if unknown (don't call assert)
	LPCTSTR lpszAppName = afxCurrentAppName;
	if (lpszAppName == NULL)
		lpszAppName = _T("<unknown application>");

	// format message into buffer
	wsprintf(szMessage, _T("%s: File %hs, Line %d"),
		lpszAppName, lpszFileName, nLine);

	if (afxTraceEnabled)
	{
		// assume the debugger or auxiliary port
		TCHAR szT[_MAX_PATH*2 + 20];
		wsprintf(szT, _T("Assertion Failed: %s\n"), szMessage);
		OutputDebugString(szT);
	}
	if (InterlockedIncrement(&afxAssertBusy) > 0)
	{
		InterlockedDecrement(&afxAssertBusy);

		// assert within assert (examine call stack to determine first one)
		AfxDebugBreak();
		return FALSE;
	}

	// active popup window for the current thread
	HWND hWndParent = GetActiveWindow();
	if (hWndParent != NULL)
		hWndParent = GetLastActivePopup(hWndParent);

	// we remove WM_QUIT because if it is in the queue then the message box
	// won't display
	MSG msg;
	BOOL bQuit = ::PeekMessage(&msg, NULL, WM_QUIT, WM_QUIT, PM_REMOVE);
	// display the assert
	int nCode = ::MessageBox(hWndParent, szMessage, _T("Assertion Failed!"),
		MB_TASKMODAL|MB_ICONHAND|MB_ABORTRETRYIGNORE|MB_SETFOREGROUND);
	if (bQuit)
		PostQuitMessage(msg.wParam);

	// cleanup
	InterlockedDecrement(&afxAssertBusy);

	if (nCode == IDIGNORE)
		return FALSE;   // ignore

	if (nCode == IDRETRY)
		return TRUE;    // will cause AfxDebugBreak

	AfxAbort();     // should not return (but otherwise AfxDebugBreak)
	return TRUE;
#endif // _AFX_NO_DEBUG_CRT
}
#endif // _DEBUG
