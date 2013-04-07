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

#ifndef _AFX_NO_OCX_SUPPORT

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
// COleControlModule::InitInstance

IMPLEMENT_DYNAMIC(COleControlModule, CWinApp)

BOOL COleControlModule::InitInstance()
{
#ifdef _AFXDLL
	// wire up resources from OLE DLL
	AfxOleInitModule();
#endif

	COleObjectFactory::RegisterAll();
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlModule::ExitInstance

int COleControlModule::ExitInstance()
{
	COleObjectFactory::RevokeAll();
	return CWinApp::ExitInstance();
}

#endif // !_AFX_NO_OCX_SUPPORT
