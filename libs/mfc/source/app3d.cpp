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
// _AFX_CTL3D_STATE

#ifndef _AFX_NO_CTL3D_SUPPORT

_AFX_CTL3D_STATE::~_AFX_CTL3D_STATE()
{
	if (m_pfnUnregister != NULL)
		(*m_pfnUnregister)(NULL);

	if (m_hCtl3dLib != NULL)
		::FreeLibrary(m_hCtl3dLib);
}

_AFX_CTL3D_THREAD::~_AFX_CTL3D_THREAD()
{
	_AFX_CTL3D_STATE* pCtl3dState = _afxCtl3dState.GetDataNA();
	if (pCtl3dState != NULL && pCtl3dState->m_pfnUnAutoSubclass != NULL)
		(*pCtl3dState->m_pfnUnAutoSubclass)();
}

/////////////////////////////////////////////////////////////////////////////
// Support for CTL3D32.DLL (3D controls DLL)

BOOL CWinApp::Enable3dControls()
{
	ASSERT(!afxContextIsDLL);   // Should only be called by apps

	// 3d controls and dialogs are automatic on newer versions of Windows
	if (afxData.bWin4)
		return TRUE;

	// otherwise, attempt to load CTL3D32.DLL
	_AFX_CTL3D_STATE* pCtl3dState = _afxCtl3dState.GetData();
	if (!pCtl3dState->m_bCtl3dInited)
	{
		pCtl3dState->m_hCtl3dLib = ::LoadLibraryA("CTL3D32.DLL");
		if (pCtl3dState->m_hCtl3dLib != NULL)
		{
			// get address of Ctl3d functions
			(FARPROC&)pCtl3dState->m_pfnRegister =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)12);
			(FARPROC&)pCtl3dState->m_pfnUnregister =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)13);
			(FARPROC&)pCtl3dState->m_pfnAutoSubclass =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)16);
			(FARPROC&)pCtl3dState->m_pfnUnAutoSubclass =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)24);
			(FARPROC&)pCtl3dState->m_pfnColorChange =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)6);
			(FARPROC&)pCtl3dState->m_pfnSubclassDlgEx =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)21);
			(FARPROC&)pCtl3dState->m_pfnWinIniChange =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)22);
			(FARPROC&)pCtl3dState->m_pfnSubclassCtl =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)3);
			(FARPROC&)pCtl3dState->m_pfnSubclassCtlEx =
				GetProcAddress(pCtl3dState->m_hCtl3dLib, (LPCSTR)25);
		}

		// may be incorrect version -- check for errors
		if (pCtl3dState->m_pfnRegister == NULL ||
			pCtl3dState->m_pfnAutoSubclass == NULL ||
			pCtl3dState->m_pfnColorChange == NULL ||
			pCtl3dState->m_pfnSubclassDlgEx == NULL ||
			pCtl3dState->m_pfnUnregister == NULL ||
			!pCtl3dState->m_pfnRegister(AfxGetInstanceHandle()))
		{
			// don't want to be partially initialized
			pCtl3dState->m_pfnRegister = NULL;
			pCtl3dState->m_pfnUnregister = NULL;
			pCtl3dState->m_pfnAutoSubclass = NULL;
			pCtl3dState->m_pfnUnAutoSubclass = NULL;
			pCtl3dState->m_pfnColorChange = NULL;
			pCtl3dState->m_pfnSubclassDlgEx = NULL;
			pCtl3dState->m_pfnWinIniChange = NULL;
			pCtl3dState->m_pfnSubclassCtl = NULL;
			pCtl3dState->m_pfnSubclassCtlEx = NULL;

			// only try once -- but return FALSE
			if (pCtl3dState->m_hCtl3dLib != NULL)
			{
				::FreeLibrary(pCtl3dState->m_hCtl3dLib);
				pCtl3dState->m_hCtl3dLib = NULL;
			}
		}
		pCtl3dState->m_bCtl3dInited = TRUE;
	}

	// check that library was loaded and all entry-points were found
	if (pCtl3dState->m_hCtl3dLib == NULL)
		return FALSE;

	// turn on auto subclassing (for primary thread)
	return (*pCtl3dState->m_pfnAutoSubclass)(AfxGetInstanceHandle());
}

_AFX_CTL3D_STATE* AFXAPI AfxGetCtl3dState()
{
	return _afxCtl3dState.GetData();
}

#pragma warning(disable: 4074)
#pragma init_seg(lib)

THREAD_LOCAL(_AFX_CTL3D_THREAD, _afxCtl3dThread)
PROCESS_LOCAL(_AFX_CTL3D_STATE, _afxCtl3dState)

#endif //!_AFX_NO_CTL3D_SUPPORT

/////////////////////////////////////////////////////////////////////////////
