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

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

// You must have AFX.INI (from \MSVC20\MFC\SRC) in your Windows
// directory if you desire diagnostic output.

// See Technical note TN007 for a description of
//   afxTraceFlags and afxTraceEnabled.

AFX_DATADEF CDumpContext afxDump;
AFX_DATADEF BOOL afxTraceEnabled;
AFX_DATADEF UINT afxTraceFlags;
BOOL _afxDiagnosticInit = AfxDiagnosticInit();

/////////////////////////////////////////////////////////////////////////////
// _AFX_DEBUG_STATE implementation

static const TCHAR szIniFile[] = _T("AFX.INI");
static const TCHAR szDiagSection[] = _T("Diagnostics");
static const TCHAR szTraceEnabled[] = _T("TraceEnabled");
static const TCHAR szTraceFlags[] = _T("TraceFlags");

#ifndef _AFX_NO_DEBUG_CRT
static _CRT_DUMP_CLIENT pfnOldCrtDumpClient = NULL;
static _CRT_REPORT_HOOK pfnOldCrtReportHook = NULL;

void __cdecl _AfxCrtDumpClient(void * pvData, unsigned int nBytes)
{
	char sz[256];
	CObject* pObject = (CObject*)pvData;

#ifndef _AFX_PORTABLE
	// use SEH (structured exception handling) to catch even GPFs
	//  that result from partially valid objects.
	__try
#endif
	{
		// with vtable, verify that object and vtable are valid
		if (!AfxIsValidAddress(*(void**)pObject, sizeof(void*), FALSE) ||
			!AfxIsValidAddress(pObject, pObject->GetRuntimeClass()->m_nObjectSize, FALSE))
		{
			// short form for invalid objects
			wsprintfA(sz, "an invalid object at $%08lX, %u bytes long\n",
				pvData, nBytes);
			afxDump << sz;
		}
		else if (afxDump.GetDepth() > 0)
		{
			// long form
			pObject->Dump(afxDump);
			afxDump << "\n";
		}
		else
		{
			// short form
			wsprintfA(sz, "a %hs object at $%08lX, %u bytes long\n",
				pObject->GetRuntimeClass()->m_lpszClassName, pvData, nBytes);
			afxDump << sz;
		}
	}
#ifndef _AFX_PORTABLE
	__except(EXCEPTION_EXECUTE_HANDLER)
	{
		// short form for trashed objects
		wsprintfA(sz, "faulted while dumping object at $%08lX, %u bytes long\n",
			pvData, nBytes);
		afxDump << sz;
	}
#endif

	if (pfnOldCrtDumpClient != NULL)
		(*pfnOldCrtDumpClient)(pvData, nBytes);
	return;
}

int __cdecl _AfxCrtReportHook(int nRptType, char *szMsg, int* pResult)
{
	// call the old report hook if there was one
	if (pfnOldCrtReportHook != NULL &&
		(*pfnOldCrtReportHook)(nRptType, szMsg, pResult))
	{
		return TRUE;
	}

	// no hook on asserts or when m_pFile is NULL
	if (nRptType == _CRT_ASSERT || afxDump.m_pFile == NULL)
		return FALSE;

	// non-NULL m_pFile, so go through afxDump for the message
	*pResult = FALSE;
	afxDump << szMsg;
	return TRUE;
}
#endif // _AFX_NO_DEBUG_CRT

_AFX_DEBUG_STATE::_AFX_DEBUG_STATE()
{
	afxTraceEnabled = ::GetPrivateProfileInt(szDiagSection, szTraceEnabled,
		TRUE, szIniFile);
	afxTraceFlags = ::GetPrivateProfileInt(szDiagSection, szTraceFlags,
		0, szIniFile);

#ifndef _AFX_NO_DEBUG_CRT
	ASSERT(pfnOldCrtDumpClient == NULL);
	pfnOldCrtDumpClient = _CrtSetDumpClient(_AfxCrtDumpClient);

	ASSERT(pfnOldCrtReportHook == NULL);
	pfnOldCrtReportHook = _CrtSetReportHook(_AfxCrtReportHook);
	_CrtSetReportMode(_CRT_ASSERT, _CRTDBG_MODE_WNDW);
#endif // _AFX_NO_DEBUG_CRT
}

_AFX_DEBUG_STATE::~_AFX_DEBUG_STATE()
{
#ifndef _AFX_NO_DEBUG_CRT
	_CrtDumpMemoryLeaks();
	int nOldState = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
	_CrtSetDbgFlag(nOldState & ~_CRTDBG_LEAK_CHECK_DF);

	_CrtSetReportHook(pfnOldCrtReportHook);
	_CrtSetDumpClient(pfnOldCrtDumpClient);
#endif // _AFX_NO_DEBUG_CRT
}

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_DEBUG_STATE, afxDebugState)

BOOL AFXAPI AfxDiagnosticInit(void)
{
	// just get the debug state to cause initialization
	_AFX_DEBUG_STATE* pState = afxDebugState.GetData();
	ASSERT(pState != NULL);

	return TRUE;
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
