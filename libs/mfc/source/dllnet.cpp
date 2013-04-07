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
// Initialization of MFC extension DLL

static AFX_EXTENSION_MODULE extensionDLL;

/////////////////////////////////////////////////////////////////////////////
// Library initialization and cleanup

extern "C" BOOL WINAPI RawDllMain(HINSTANCE, DWORD dwReason, LPVOID);
extern "C" BOOL (WINAPI* _pRawDllMain)(HINSTANCE, DWORD, LPVOID) = &RawDllMain;

#ifdef _DEBUG
#ifndef _UNICODE
#ifdef __BORLANDC__
  #define MFC42_DLL "BFC42D.DLL"
#else
  #define MFC42_DLL "MFC42D.DLL"
#endif // __BORLANDC__
#else
#define MFC42_DLL "MFC42UD.DLL"
#endif
#else
#ifndef _UNICODE
#ifdef __BORLANDC__
  #define MFC42_DLL "BFC42.DLL"
#else
  #define MFC42_DLL "MFC42.DLL"
#endif // __BORLANDC__
#else
#define MFC42_DLL "MFC42U.DLL"
#endif
#endif

extern "C"
BOOL WINAPI RawDllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID)
{
	UNUSED_ALWAYS(hInstance);
	if (dwReason == DLL_PROCESS_ATTACH)
	{
		// Prevent the MFC DLL from being unloaded prematurely
		LoadLibraryA(MFC42_DLL);

		// make sure we have enough memory to attempt to start (8kb)
		void* pMinHeap = LocalAlloc(NONZEROLPTR, 0x2000);
		if (pMinHeap == NULL)
			return FALSE;   // fail if memory alloc fails
		LocalFree(pMinHeap);

		// save critical data pointers before running the constructors
		AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
		pModuleState->m_pClassInit = pModuleState->m_classList;
		pModuleState->m_pFactoryInit = pModuleState->m_factoryList;
		pModuleState->m_classList.m_pHead = NULL;
		pModuleState->m_factoryList.m_pHead = NULL;
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
		// Now it's OK for the MFC  DLL to be unloaded (see LoadLibrary above)
		FreeLibrary(GetModuleHandleA(MFC42_DLL));
	}
	return TRUE;    // ok
}

extern "C"
BOOL WINAPI DllMain(HINSTANCE hInstance, DWORD dwReason, LPVOID /*lpReserved*/)
{
	if (dwReason == DLL_PROCESS_ATTACH)
	{
		// shared initialization
		VERIFY(AfxInitExtensionModule(extensionDLL, hInstance));

		// wire up this DLL into the resource chain
		CDynLinkLibrary* pDLL = new CDynLinkLibrary(extensionDLL, TRUE);
		ASSERT(pDLL != NULL);
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
		AfxTermExtensionModule(extensionDLL);
	}
	else if (dwReason == DLL_THREAD_DETACH)
	{
		AfxTermThread(hInstance);
	}

	return TRUE;    // ok
}

////////////////////////////////////////////////////////////////////////////
// Special initialization entry point for controls

void AFXAPI AfxNetInitModule()
{
	ASSERT(AfxGetModuleState() != AfxGetAppModuleState());

	CDynLinkLibrary* pDLL = new CDynLinkLibrary(extensionDLL, TRUE);
	ASSERT(pDLL != NULL);
}

/////////////////////////////////////////////////////////////////////////////
// Special code to wire up vector deleting destructors

#ifdef AFX_VDEL_SEG
#pragma code_seg(AFX_VDEL_SEG)
#endif
static void _AfxForceVectorDelete()
{
	ASSERT(FALSE);  // never called

	new CAsyncSocket[2];
	new CSocket[2];
}
void (*_afxForceVectorDelete_mfcn)() = &_AfxForceVectorDelete;

/////////////////////////////////////////////////////////////////////////////
