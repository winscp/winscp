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
// Support for gray background in dialogs (and message boxes)

#ifndef _AFX_NO_GRAYDLG_SUPPORT

LRESULT CALLBACK
_AfxCbtFilterHook(int code, WPARAM wParam, LPARAM lParam);

void CWinApp::SetDialogBkColor(COLORREF clrCtlBk, COLORREF clrCtlText)
{
	if (!afxContextIsDLL)
	{
		_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
		if (pThreadState->m_hHookOldCbtFilter == NULL)
		{
			pThreadState->m_hHookOldCbtFilter = ::SetWindowsHookEx(WH_CBT,
				_AfxCbtFilterHook, NULL, ::GetCurrentThreadId());
			if (pThreadState->m_hHookOldCbtFilter == NULL)
				AfxThrowMemoryException();
		}
	}

	// set up for gray backgrounds for dialogs
	_AFX_WIN_STATE* pWinState = _afxWinState;
	AfxDeleteObject((HGDIOBJ*)&pWinState->m_hDlgBkBrush);

	pWinState->m_hDlgBkBrush = ::CreateSolidBrush(clrCtlBk);
	pWinState->m_crDlgTextClr = clrCtlText;
	if (pWinState->m_hDlgBkBrush == NULL)
		AfxThrowResourceException();
}

#endif //!_AFX_NO_GRAYDLG_SUPPORT

/////////////////////////////////////////////////////////////////////////////
