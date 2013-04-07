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

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
// Standard WinMain implementation
//  Can be replaced as long as 'AfxWinInit' is called first

int AFXAPI AfxWinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
	LPTSTR lpCmdLine, int nCmdShow)
{
	ASSERT(hPrevInstance == NULL);

	int nReturnCode = -1;
	CWinThread* pThread = AfxGetThread();
	CWinApp* pApp = AfxGetApp();

	// AFX internal initialization
	if (!AfxWinInit(hInstance, hPrevInstance, lpCmdLine, nCmdShow))
		goto InitFailure;

	// App global initializations (rare)
	if (pApp != NULL && !pApp->InitApplication())
		goto InitFailure;

	// Perform specific initializations
	if (!pThread->InitInstance())
	{
		if (pThread->m_pMainWnd != NULL)
		{
			TRACE0("Warning: Destroying non-NULL m_pMainWnd\n");
			pThread->m_pMainWnd->DestroyWindow();
		}
		nReturnCode = pThread->ExitInstance();
		goto InitFailure;
	}
	nReturnCode = pThread->Run();

InitFailure:
#ifdef _DEBUG
	// Check for missing AfxLockTempMap calls
	if (AfxGetModuleThreadState()->m_nTempMapLock != 0)
	{
		TRACE1("Warning: Temp map lock count non-zero (%ld).\n",
			AfxGetModuleThreadState()->m_nTempMapLock);
	}
	AfxLockTempMaps();
	AfxUnlockTempMaps(-1);
#endif

	AfxWinTerm();
	return nReturnCode;
}

/////////////////////////////////////////////////////////////////////////////
