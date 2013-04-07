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
#include <ddeml.h> // for MSGF_DDEMGR

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// other globals (internal library use)

/////////////////////////////////////////////////////////////////////////////
// Standard cleanup called by WinMain and AfxAbort

void AFXAPI AfxWinTerm(void)
{
	// unregister Window classes
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_REGCLASSLIST);
	LPTSTR lpsz = pModuleState->m_szUnregisterList;
	while (*lpsz != 0)
	{
		LPTSTR lpszEnd = _tcschr(lpsz, '\n');
		ASSERT(lpszEnd != NULL);
		*lpszEnd = 0;
		UnregisterClass(lpsz, AfxGetInstanceHandle());
		lpsz = lpszEnd + 1;
	}
	pModuleState->m_szUnregisterList[0] = 0;
	AfxUnlockGlobals(CRIT_REGCLASSLIST);

	// cleanup OLE if required
	CWinThread* pThread = AfxGetApp();
	if (pThread != NULL && pThread->m_lpfnOleTermOrFreeLib != NULL)
		(*pThread->m_lpfnOleTermOrFreeLib)(TRUE, FALSE);

	// cleanup thread local tooltip window
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	if (pThreadState->m_pToolTip != NULL)
	{
		if (pThreadState->m_pToolTip->DestroyToolTipCtrl())
			pThreadState->m_pToolTip = NULL;
	}

	if (!afxContextIsDLL)
	{
		// unhook windows hooks
		if (pThreadState->m_hHookOldMsgFilter != NULL)
		{
			::UnhookWindowsHookEx(pThreadState->m_hHookOldMsgFilter);
			pThreadState->m_hHookOldMsgFilter = NULL;
		}
		if (pThreadState->m_hHookOldCbtFilter != NULL)
		{
			::UnhookWindowsHookEx(pThreadState->m_hHookOldCbtFilter);
			pThreadState->m_hHookOldCbtFilter = NULL;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
