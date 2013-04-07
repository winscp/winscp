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

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// OLE OLE_DATA init structure

OLE_DATA _oleData;

OLE_DATA::OLE_DATA()
{
	// OLE 1.0 Clipboard formats
	cfNative = ::RegisterClipboardFormat(_T("Native"));
	cfOwnerLink = ::RegisterClipboardFormat(_T("OwnerLink"));
	cfObjectLink = ::RegisterClipboardFormat(_T("ObjectLink"));

	// OLE 2.0 Clipboard formats
	cfEmbeddedObject = ::RegisterClipboardFormat(_T("Embedded Object"));
	cfEmbedSource = ::RegisterClipboardFormat(_T("Embed Source"));
	cfLinkSource = ::RegisterClipboardFormat(_T("Link Source"));
	cfObjectDescriptor = ::RegisterClipboardFormat(_T("Object Descriptor"));
	cfLinkSourceDescriptor = ::RegisterClipboardFormat(_T("Link Source Descriptor"));
	cfFileName = ::RegisterClipboardFormat(_T("FileName"));
	cfFileNameW = ::RegisterClipboardFormat(_T("FileNameW"));
	cfRichTextFormat = ::RegisterClipboardFormat(_T("Rich Text Format"));
	cfRichTextAndObjects = ::RegisterClipboardFormat(_T("RichEdit Text and Objects"));
}

/////////////////////////////////////////////////////////////////////////////
// OLE initialization & termination

BOOL AFXAPI AfxOleInit()
{
	_AFX_THREAD_STATE* pState = AfxGetThreadState();
	ASSERT(!pState->m_bNeedTerm);    // calling it twice?

	// Special case DLL context to assume that the calling app initializes OLE.
	// For DLLs where this is not the case, those DLLs will need to initialize
	// OLE for themselves via OleInitialize.  This is done since MFC cannot provide
	// automatic uninitialize for DLLs because it is not valid to shutdown OLE
	// during a DLL_PROCESS_DETACH.
	if (afxContextIsDLL)
	{
		pState->m_bNeedTerm = -1;  // -1 is a special flag
		return TRUE;
	}

	// first, initialize OLE
	SCODE sc = ::OleInitialize(NULL);
	if (FAILED(sc))
	{
		// warn about non-NULL success codes
		TRACE1("Warning: OleInitialize returned scode = %s.\n",
			AfxGetFullScodeString(sc));
		goto InitFailed;
	}
	// termination required when OleInitialize does not fail
	pState->m_bNeedTerm = TRUE;

	// hook idle time and exit time for required OLE cleanup
	CWinThread* pThread; pThread = AfxGetThread();
	pThread->m_lpfnOleTermOrFreeLib = AfxOleTermOrFreeLib;

	// allocate and initialize default message filter
	if (pThread->m_pMessageFilter == NULL)
	{
		pThread->m_pMessageFilter = new COleMessageFilter;
		ASSERT(AfxOleGetMessageFilter() != NULL);
		AfxOleGetMessageFilter()->Register();
	}
	return TRUE;

InitFailed:
	AfxOleTerm();
	return FALSE;
}

void AFXAPI AfxOleTerm(BOOL bJustRevoke)
{
	// release clipboard ownership
	COleDataSource::FlushClipboard();

	// revoke all class factories
	COleObjectFactory::RevokeAll();

#ifndef _AFX_NO_OCC_SUPPORT
	AfxOleUnlockAllControls();
#endif

	if (!bJustRevoke)
	{
		CWinThread* pThread = AfxGetThread();
		if (pThread != NULL)
		{
			// destroy message filter (may be derived class)
			delete pThread->m_pMessageFilter;
			pThread->m_pMessageFilter = NULL;
		}

		// terminate OLE last
		_AFX_THREAD_STATE* pState = AfxGetThreadState();
		// -1 is special case, so need to compare against TRUE
		if (pState->m_bNeedTerm == TRUE)
		{
			CoFreeUnusedLibraries();
			::OleUninitialize();
			pState->m_bNeedTerm = FALSE;
		}
	}
}

AFX_STATIC_DATA DWORD _afxTickCount = (DWORD)-1;
AFX_STATIC_DATA BOOL _afxTickInit = FALSE;

void AFXAPI AfxOleTermOrFreeLib(BOOL bTerm, BOOL bJustRevoke)
{
	if (bTerm)
	{
		AfxOleTerm(bJustRevoke);
	}
	else
	{
		// initialize _afxTickCount if necessary
		if (!_afxTickInit)
		{
			_afxTickCount = ::GetTickCount();
			++_afxTickInit;
		}

		// only call CoFreeUnusedLibraries if one minute has gone by
		if (GetTickCount() - _afxTickCount > 60000)
		{
			CoFreeUnusedLibraries();
			_afxTickCount = ::GetTickCount();
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CWinApp support for parsing OLE command line

AFX_STATIC BOOL AFXAPI _AfxParseOption(LPTSTR lpszCmdLine, LPCTSTR lpszOption)
{
	int nLen = lstrlen(lpszOption);
	while (*lpszCmdLine != 0)
	{
		if ((*lpszCmdLine == '-' || *lpszCmdLine == '/') &&
			_tcsncmp(lpszOption, lpszCmdLine+1, nLen) == 0)
		{
			// remove the option from the command line
			int nCmdLen = lstrlen(lpszCmdLine);
			memmove(lpszCmdLine, lpszCmdLine + nLen + 1,
				(nCmdLen - nLen) * sizeof(TCHAR));
			return TRUE;
		}
		lpszCmdLine++;
	}
	return FALSE;
}

BOOL CWinApp::RunEmbedded()
{
	ASSERT(m_lpCmdLine != NULL);

	// hard coded non-localized name
	if (_AfxParseOption(m_lpCmdLine, _T("Embedding")))
	{
		AfxOleSetUserCtrl(FALSE);
		return TRUE;
	}
	return FALSE;   // not run with /Embedding
}

BOOL CWinApp::RunAutomated()
{
	ASSERT(m_lpCmdLine != NULL);

	// hard coded non-localized name
	if (_AfxParseOption(m_lpCmdLine, _T("Automation")))
	{
		AfxOleSetUserCtrl(FALSE);
		return TRUE;
	}
	return FALSE;   // not run with /Automation
}

/////////////////////////////////////////////////////////////////////////////
