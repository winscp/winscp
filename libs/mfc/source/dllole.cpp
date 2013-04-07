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
AFX_MODULE_STATE* AFXAPI _AfxGetOleModuleState();

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
BOOL WINAPI RawDllMain(HINSTANCE, DWORD dwReason, LPVOID)
{
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

		// set module state before initialization
		pModuleState = _AfxGetOleModuleState();
		_AFX_THREAD_STATE* pState = AfxGetThreadState();
		pState->m_pPrevModuleState = AfxSetModuleState(pModuleState);
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
		// restore previously-saved module state
		VERIFY(AfxSetModuleState(AfxGetThreadState()->m_pPrevModuleState) ==
			_AfxGetOleModuleState());
		DEBUG_ONLY(AfxGetThreadState()->m_pPrevModuleState = NULL);

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
		BOOL bRegister = !extensionDLL.bInitialized;
		AFX_MODULE_STATE* pModuleState = _AfxGetOleModuleState();
		pModuleState->m_hCurrentInstanceHandle = hInstance;
		pModuleState->m_hCurrentResourceHandle = hInstance;
		pModuleState->m_pClassInit = pModuleState->m_classList.GetHead();
		pModuleState->m_pFactoryInit = pModuleState->m_factoryList.GetHead();
		VERIFY(AfxInitExtensionModule(extensionDLL, hInstance));

		// wire up base DLL class list into private module state
		AfxCoreInitModule();

		AfxWinInit(hInstance, NULL, _T(""), 0);

		// Register class factories in context of private module state
		if (bRegister)
			COleObjectFactory::RegisterAll();

		// restore previously-saved module state
		VERIFY(AfxSetModuleState(AfxGetThreadState()->m_pPrevModuleState) ==
			_AfxGetOleModuleState());
		DEBUG_ONLY(AfxGetThreadState()->m_pPrevModuleState = NULL);

		// wire up this DLL into the base module state resource chain
		CDynLinkLibrary* pDLL = new CDynLinkLibrary(extensionDLL, TRUE);
		ASSERT(pDLL != NULL);
		pDLL->m_factoryList.m_pHead = NULL;
	}
	else if (dwReason == DLL_PROCESS_DETACH)
	{
		// cleanup module state in base module state
		AfxTermExtensionModule(extensionDLL);

		// set module state for cleanup
		ASSERT(AfxGetThreadState()->m_pPrevModuleState == NULL);
		AfxGetThreadState()->m_pPrevModuleState =
			AfxSetModuleState(_AfxGetOleModuleState());

		// cleanup module state in OLE private module state
		AfxTermExtensionModule(extensionDLL, TRUE);
	}
	else if (dwReason == DLL_THREAD_DETACH)
	{
		AfxTermThread(hInstance);
	}

	return TRUE;    // ok
}

////////////////////////////////////////////////////////////////////////////
// Special initialization entry point for controls

void AFXAPI AfxOleInitModule()
{
	ASSERT(AfxGetModuleState() != AfxGetAppModuleState());

	CDynLinkLibrary* pDLL = new CDynLinkLibrary(extensionDLL, TRUE);
	ASSERT(pDLL != NULL);
	pDLL->m_factoryList.m_pHead = NULL;
}

////////////////////////////////////////////////////////////////////////////
// COM entry points

STDAPI DllGetClassObject(REFCLSID rclsid, REFIID riid, LPVOID* ppv)
{
	AFX_MANAGE_STATE(_AfxGetOleModuleState());
	return AfxDllGetClassObject(rclsid, riid, ppv);
}

STDAPI DllCanUnloadNow(void)
{
	AFX_MANAGE_STATE(_AfxGetOleModuleState());
	return AfxDllCanUnloadNow();
}

////////////////////////////////////////////////////////////////////////////
// Server registration

STDAPI DllRegisterServer(void)
{
	AFX_MANAGE_STATE(_AfxGetOleModuleState());

	if (!COleObjectFactoryEx::UpdateRegistryAll(TRUE))
		return SELFREG_E_CLASS;

	return S_OK;
}

STDAPI DllUnregisterServer(void)
{
	AFX_MANAGE_STATE(_AfxGetOleModuleState());

	if (!COleObjectFactoryEx::UpdateRegistryAll(FALSE))
		return SELFREG_E_CLASS;

	return S_OK;
}

// This CWinApp is required so this module state has a CWinApp object!
CWinApp _afxOleWinApp;

/////////////////////////////////////////////////////////////////////////////
// static-linked version of AfxWndProc for use by this module

#undef AfxWndProc

LRESULT CALLBACK
AfxWndProcDllOle(HWND hWnd, UINT nMsg, WPARAM wParam, LPARAM lParam)
{
	AFX_MANAGE_STATE(_AfxGetOleModuleState());
	return AfxWndProc(hWnd, nMsg, wParam, lParam);
}

/////////////////////////////////////////////////////////////////////////////
// initialize app state such that it points to this module's core state

#ifndef _AFX_NO_OCC_SUPPORT
class _AFX_INIT_OLE_DLL_STATE
{
public:
	~_AFX_INIT_OLE_DLL_STATE();
};

_AFX_INIT_OLE_DLL_STATE::~_AFX_INIT_OLE_DLL_STATE()
{
	// OLE dll is not loaded any more, so no OLE controls capability
	afxOccManager = NULL;
}
#endif

/////////////////////////////////////////////////////////////////////////////

// force initialization early
#pragma warning(disable: 4074)
#pragma init_seg(lib)

static AFX_MODULE_STATE _afxOleModuleState(TRUE, &AfxWndProcDllOle,
	_MFC_VER, TRUE);
#ifndef _AFX_NO_OCC_SUPPORT
_AFX_INIT_OLE_DLL_STATE _afxInitOleDllState;
#endif

AFX_MODULE_STATE* AFXAPI _AfxGetOleModuleState()
{
	return &_afxOleModuleState;
}

/////////////////////////////////////////////////////////////////////////////

#ifdef AFX_VDEL_SEG
#pragma code_seg(AFX_VDEL_SEG)
#endif
static void _AfxForceVectorDelete()
{
	ASSERT(FALSE);  // never called

	new COleDataSource[2];
	new COleDispatchDriver[2];
	new COleDropSource[2];
	new COleResizeBar[2];
	new COleStreamFile[2];
	new CMonikerFile[2];
	new COleTemplateServer[2];
	new COleDataObject[2];
	new COleDropTarget[2];
	new COleIPFrameWnd[2];

	new COleDocIPFrameWnd[2];
	new CAsyncMonikerFile[2];
	new CCachedDataPathProperty[2];
	new CDataPathProperty[2];

	new COleVariant[2];

	new CRichEditView[2];
	new CRichEditCntrItem[2];
}
void (*_afxForceVectorDelete_mfco)() = &_AfxForceVectorDelete;

/////////////////////////////////////////////////////////////////////////////
