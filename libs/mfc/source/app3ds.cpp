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

#ifndef _AFX_NO_CTL3D_SUPPORT

#include "ctl3d.h"
#ifdef __BORLANDC__
  #pragma comment(lib, "ctl3d32.lib")
#else
  #pragma comment(lib, "ctl3d32s.lib")
#endif

/////////////////////////////////////////////////////////////////////////////
// Support for CTL3D32.LIB (3D controls static library)

BOOL CWinApp::Enable3dControlsStatic()
{
	ASSERT(!afxContextIsDLL);   // Should only be called by apps

	// 3d controls and dialogs are automatic on newer versions of Windows
	if (afxData.bWin4)
		return TRUE;

	// otherwise, attempt to load CTL3D32.DLL
	_AFX_CTL3D_STATE* pCtl3dState = _afxCtl3dState;
	if (!pCtl3dState->m_bCtl3dInited)
	{
		// get address of Ctl3d functions
		pCtl3dState->m_pfnRegister = &Ctl3dRegister;
		pCtl3dState->m_pfnUnregister = &Ctl3dUnregister;
		pCtl3dState->m_pfnAutoSubclass = &Ctl3dAutoSubclass;
		pCtl3dState->m_pfnUnAutoSubclass = &Ctl3dUnAutoSubclass;
		pCtl3dState->m_pfnColorChange = &Ctl3dColorChange;
		pCtl3dState->m_pfnSubclassDlgEx = &Ctl3dSubclassDlgEx;
		pCtl3dState->m_pfnWinIniChange = &Ctl3dWinIniChange;
		pCtl3dState->m_pfnSubclassCtl = &Ctl3dSubclassCtl;
		pCtl3dState->m_pfnSubclassCtlEx = &Ctl3dSubclassCtlEx;

		// may be incorrect version -- check for errors
		if (!pCtl3dState->m_pfnRegister(AfxGetInstanceHandle()))
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
		}
		pCtl3dState->m_bCtl3dInited = TRUE;
	}

	if (pCtl3dState->m_pfnAutoSubclass == NULL)
		return FALSE;

	// turn on auto subclassing (for primary thread)
	return (*pCtl3dState->m_pfnAutoSubclass)(AfxGetInstanceHandle());
}

#endif //!_AFX_NO_CTL3D_SUPPORT

/////////////////////////////////////////////////////////////////////////////
