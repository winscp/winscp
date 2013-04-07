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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleObjectFactory implementation

BEGIN_INTERFACE_MAP(COleObjectFactory, CCmdTarget)
	INTERFACE_PART(COleObjectFactory, IID_IClassFactory, ClassFactory)
	INTERFACE_PART(COleObjectFactory, IID_IClassFactory2, ClassFactory)
END_INTERFACE_MAP()

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

COleObjectFactory::COleObjectFactory(REFCLSID clsid,
	CRuntimeClass* pRuntimeClass, BOOL bMultiInstance, LPCTSTR lpszProgID)
{
	ASSERT(pRuntimeClass == NULL ||
		pRuntimeClass->IsDerivedFrom(RUNTIME_CLASS(CCmdTarget)));
	ASSERT(AfxIsValidAddress(&clsid, sizeof(CLSID), FALSE));
	ASSERT(lpszProgID == NULL || AfxIsValidString(lpszProgID));

	// initialize to unregistered state
	m_dwRegister = 0;   // not registered yet
	m_bRegistered = FALSE;
	m_clsid = clsid;
	m_pRuntimeClass = pRuntimeClass;
	m_bMultiInstance = bMultiInstance;
	m_lpszProgID = lpszProgID;
	m_bOAT = (BYTE) OAT_UNKNOWN;

	// licensing information
	m_bLicenseChecked = FALSE;
	m_bLicenseValid = FALSE;

	// add this factory to the list of factories
	m_pNextFactory = NULL;
	AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	pModuleState->m_factoryList.AddHead(this);
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);

	ASSERT_VALID(this);
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

COleObjectFactory::~COleObjectFactory()
{
	ASSERT_VALID(this);

#ifdef _AFXDLL
	if (m_pModuleState == NULL)
		return;
#endif

	// deregister this class factory
	Revoke();

	// remove this class factory from the list of active class factories
#ifdef _AFXDLL
	AFX_MODULE_STATE* pModuleState = m_pModuleState;
#else
	AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
#endif
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	BOOL bResult = pModuleState->m_factoryList.Remove(this);
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
	if (bResult)
		return;

	// check CDynLinkLibrary objects in case it was transfered during init
#ifdef _AFXDLL
	AfxLockGlobals(CRIT_DYNLINKLIST);
	for (CDynLinkLibrary* pDLL = pModuleState->m_libraryList; pDLL != NULL;
		pDLL = pDLL->m_pNextDLL)
	{
		if (pDLL->m_factoryList.Remove(this))
		{
			AfxUnlockGlobals(CRIT_DYNLINKLIST);
			return;
		}
	}
	AfxUnlockGlobals(CRIT_DYNLINKLIST);
#endif
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

BOOL COleObjectFactory::Unregister()
{
	return TRUE;
}

BOOL COleObjectFactory::Register()
{
	ASSERT_VALID(this);
	ASSERT(!m_bRegistered);  // registering server/factory twice?
	ASSERT(m_clsid != CLSID_NULL);

	if (!afxContextIsDLL)
	{
		// In the application variants, the IClassFactory is registered
		//  with the OLE DLLs.

		SCODE sc = ::CoRegisterClassObject(m_clsid, &m_xClassFactory,
			CLSCTX_LOCAL_SERVER,
			m_bMultiInstance ? REGCLS_SINGLEUSE : REGCLS_MULTIPLEUSE,
			&m_dwRegister);
		if (sc != S_OK)
		{
#ifdef _DEBUG
			TRACE1("Warning: CoRegisterClassObject failed scode = %s.\n",
				::AfxGetFullScodeString(sc));
#endif
			// registration failed.
			return FALSE;
		}
		ASSERT(m_dwRegister != 0);
	}

	++m_bRegistered;
	return TRUE;
}

BOOL PASCAL COleObjectFactory::UnregisterAll()
{
	BOOL bResult = TRUE;
	// register application factories
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	for (COleObjectFactory* pFactory = pModuleState->m_factoryList;
		pFactory != NULL; pFactory = pFactory->m_pNextFactory)
	{
		// unregister any registered, non-doctemplate factories
		if (pFactory->IsRegistered() && !pFactory->Unregister())
		{
			bResult = FALSE;
		}
	}
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
	return bResult;
}

BOOL PASCAL COleObjectFactory::RegisterAll()
{
	BOOL bResult = TRUE;
	// register application factories
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	for (COleObjectFactory* pFactory = pModuleState->m_factoryList;
		pFactory != NULL; pFactory = pFactory->m_pNextFactory)
	{
		// register any non-registered, non-doctemplate factories
		if (!pFactory->IsRegistered() &&
			pFactory->m_clsid != CLSID_NULL && !pFactory->Register())
		{
			bResult = FALSE;
		}
	}
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
#ifdef _AFXDLL
	// register extension DLL factories
	AfxLockGlobals(CRIT_DYNLINKLIST);
	for (CDynLinkLibrary* pDLL = pModuleState->m_libraryList; pDLL != NULL;
		pDLL = pDLL->m_pNextDLL)
	{
		for (pFactory = pDLL->m_factoryList;
			pFactory != NULL; pFactory = pFactory->m_pNextFactory)
		{
			// register any non-registered, non-doctemplate factories
			if (!pFactory->IsRegistered() &&
				pFactory->m_clsid != CLSID_NULL && !pFactory->Register())
			{
				bResult = FALSE;
			}
		}
	}
	AfxUnlockGlobals(CRIT_DYNLINKLIST);
#endif
	return bResult;
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

void COleObjectFactory::Revoke()
{
	ASSERT_VALID(this);

	if (m_bRegistered)
	{
		// revoke the registration of the class itself
		if (m_dwRegister != 0)
		{
			::CoRevokeClassObject(m_dwRegister);
			m_dwRegister = 0;
		}
		m_bRegistered = FALSE;
	}
}

void PASCAL COleObjectFactory::RevokeAll()
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	for (COleObjectFactory* pFactory = pModuleState->m_factoryList;
		pFactory != NULL; pFactory = pFactory->m_pNextFactory)
	{
		pFactory->Revoke();
	}
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
#ifdef _AFXDLL
	AfxLockGlobals(CRIT_DYNLINKLIST);
	// register extension DLL factories
	for (CDynLinkLibrary* pDLL = pModuleState->m_libraryList; pDLL != NULL;
		pDLL = pDLL->m_pNextDLL)
	{
		for (pFactory = pDLL->m_factoryList;
			pFactory != NULL; pFactory = pFactory->m_pNextFactory)
		{
			pFactory->Revoke();
		}
	}
	AfxUnlockGlobals(CRIT_DYNLINKLIST);
#endif
}

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

void COleObjectFactory::UpdateRegistry(LPCTSTR lpszProgID)
{
	ASSERT_VALID(this);
	ASSERT(lpszProgID == NULL || AfxIsValidString(lpszProgID));

	// use default prog-id if specific prog-id not given
	if (lpszProgID == NULL)
	{
		lpszProgID = m_lpszProgID;
		if (lpszProgID == NULL) // still no valid progID?
			return;
	}

	// call global helper to modify system registry
	//  (progid, shortname, and long name are all equal in this case)
	AfxOleRegisterServerClass(m_clsid, lpszProgID, lpszProgID, lpszProgID,
		OAT_DISPATCH_OBJECT);
}

BOOL PASCAL COleObjectFactory::UpdateRegistryAll(BOOL bRegister)
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_OBJECTFACTORYLIST);
	for (COleObjectFactory* pFactory = pModuleState->m_factoryList;
		pFactory != NULL; pFactory = pFactory->m_pNextFactory)
	{
		if (!pFactory->UpdateRegistry(bRegister))
		{
			AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
			return FALSE;
		}
	}
	AfxUnlockGlobals(CRIT_OBJECTFACTORYLIST);
#ifdef _AFXDLL
	AfxLockGlobals(CRIT_DYNLINKLIST);
	// register extension DLL factories
	for (CDynLinkLibrary* pDLL = pModuleState->m_libraryList; pDLL != NULL;
		pDLL = pDLL->m_pNextDLL)
	{
		for (pFactory = pDLL->m_factoryList;
			pFactory != NULL; pFactory = pFactory->m_pNextFactory)
		{
			if (!pFactory->UpdateRegistry(bRegister))
			{
				AfxUnlockGlobals(CRIT_DYNLINKLIST);
				return FALSE;
			}
		}
	}
	AfxUnlockGlobals(CRIT_DYNLINKLIST);
#endif

	return TRUE;
}

CCmdTarget* COleObjectFactory::OnCreateObject()
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(m_pRuntimeClass, sizeof(CRuntimeClass), FALSE));
		// this implementation needs a runtime class

	// allocate object, throw exception on failure
	CCmdTarget* pTarget = (CCmdTarget*)m_pRuntimeClass->CreateObject();
	if (pTarget == NULL)
		AfxThrowMemoryException();

	// make sure it is a CCmdTarget
	ASSERT_KINDOF(CCmdTarget, pTarget);
	ASSERT_VALID(pTarget);

	// return the new CCmdTarget object
	return pTarget;
}

BOOL COleObjectFactory::IsLicenseValid()
{
	if (!m_bLicenseChecked)
	{
		m_bLicenseValid = (BYTE)VerifyUserLicense();
		m_bLicenseChecked = TRUE;
	}
	return m_bLicenseValid;
}

BOOL COleObjectFactory::UpdateRegistry(BOOL bRegister)
{
	if (bRegister)
		UpdateRegistry();   // will register with default m_lpszProgID

	return TRUE;
}

BOOL COleObjectFactory::VerifyUserLicense()
{
	// May be overridden by subclass
	return TRUE;
}

BOOL COleObjectFactory::GetLicenseKey(DWORD, BSTR*)
{
	// May be overridden by subclass
	return FALSE;
}

BOOL COleObjectFactory::VerifyLicenseKey(BSTR bstrKey)
{
	// May be overridden by subclass

	BOOL bLicensed = FALSE;
	BSTR bstr = NULL;

	if ((bstrKey != NULL) && GetLicenseKey(0, &bstr))
	{
		ASSERT(bstr != NULL);

		// if length and content match, it's good!

		UINT cch = SysStringByteLen(bstr);
		if ((cch == SysStringByteLen(bstrKey)) &&
			(memcmp(bstr, bstrKey, cch) == 0))
		{
			bLicensed = TRUE;
		}

		SysFreeString(bstr);
	}

	return bLicensed;
}

/////////////////////////////////////////////////////////////////////////////
// Implementation of COleObjectFactory::IClassFactory interface

STDMETHODIMP_(ULONG) COleObjectFactory::XClassFactory::AddRef()
{
	METHOD_PROLOGUE_EX_(COleObjectFactory, ClassFactory)
	return pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleObjectFactory::XClassFactory::Release()
{
	METHOD_PROLOGUE_EX_(COleObjectFactory, ClassFactory)
	return pThis->InternalRelease();
}

STDMETHODIMP COleObjectFactory::XClassFactory::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleObjectFactory, ClassFactory)
	return pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleObjectFactory::XClassFactory::CreateInstance(
	IUnknown* pUnkOuter, REFIID riid, LPVOID* ppvObject)
{
	return CreateInstanceLic(pUnkOuter, NULL, riid, NULL, ppvObject);
}

STDMETHODIMP COleObjectFactory::XClassFactory::LockServer(BOOL fLock)
{
	METHOD_PROLOGUE_EX(COleObjectFactory, ClassFactory)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		if (fLock)
			AfxOleLockApp();
		else
			AfxOleUnlockApp();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleObjectFactory::XClassFactory::GetLicInfo(
	LPLICINFO pLicInfo)
{
	METHOD_PROLOGUE_EX(COleObjectFactory, ClassFactory)
	ASSERT_VALID(pThis);

	BSTR bstr = NULL;
	pLicInfo->fLicVerified = pThis->IsLicenseValid();
	pLicInfo->fRuntimeKeyAvail = pThis->GetLicenseKey(0, &bstr);
	if (bstr != NULL)
		SysFreeString(bstr);

	return S_OK;
}

STDMETHODIMP COleObjectFactory::XClassFactory::RequestLicKey(
	DWORD dwReserved, BSTR* pbstrKey)
{
	METHOD_PROLOGUE_EX(COleObjectFactory, ClassFactory)
	ASSERT_VALID(pThis);

	ASSERT(pbstrKey != NULL);

	*pbstrKey = NULL;

	if (pThis->IsLicenseValid())
	{
		if (pThis->GetLicenseKey(dwReserved, pbstrKey))
			return S_OK;
		else
			return E_FAIL;
	}
	else
		return CLASS_E_NOTLICENSED;
}

STDMETHODIMP COleObjectFactory::XClassFactory::CreateInstanceLic(
	LPUNKNOWN pUnkOuter, LPUNKNOWN /* pUnkReserved */, REFIID riid,
	BSTR bstrKey, LPVOID* ppvObject)
{
	METHOD_PROLOGUE_EX(COleObjectFactory, ClassFactory)
	ASSERT_VALID(pThis);

	if (ppvObject == NULL)
		return E_POINTER;
	*ppvObject = NULL;

	if (((bstrKey != NULL) && !pThis->VerifyLicenseKey(bstrKey)) ||
		((bstrKey == NULL) && !pThis->IsLicenseValid()))
		return CLASS_E_NOTLICENSED;

	// outer objects must ask for IUnknown only
	ASSERT(pUnkOuter == NULL || riid == IID_IUnknown);

	// attempt to create the object
	CCmdTarget* pTarget = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		// attempt to create the object
		pTarget = pThis->OnCreateObject();
		if (pTarget != NULL)
		{
			// check for aggregation on object not supporting it
			sc = CLASS_E_NOAGGREGATION;
			if (pUnkOuter == NULL || pTarget->m_xInnerUnknown != 0)
			{
				// create aggregates used by the object
				pTarget->m_pOuterUnknown = pUnkOuter;
				sc = E_OUTOFMEMORY;
				if (pTarget->OnCreateAggregates())
					sc = S_OK;
			}
		}
	}
	END_TRY

	// finish creation
	if (sc == S_OK)
	{
		DWORD dwRef = 1;
		if (pUnkOuter != NULL)
		{
			// return inner unknown instead of IUnknown
			*ppvObject = &pTarget->m_xInnerUnknown;
		}
		else
		{
			// query for requested interface
			sc = pTarget->InternalQueryInterface(&riid, ppvObject);
			if (sc == S_OK)
			{
				dwRef = pTarget->InternalRelease();
				ASSERT(dwRef != 0);
			}
		}
		if (dwRef != 1)
			TRACE1("Warning: object created with reference of %ld\n", dwRef);
	}

	// cleanup in case of errors
	if (sc != S_OK)
		delete pTarget;

	return sc;
}

//////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void COleObjectFactory::AssertValid() const
{
	CCmdTarget::AssertValid();
	ASSERT(m_lpszProgID == NULL || AfxIsValidString(m_lpszProgID));
	ASSERT(m_pRuntimeClass == NULL ||
		AfxIsValidAddress(m_pRuntimeClass, sizeof(CRuntimeClass), FALSE));
	ASSERT(m_pNextFactory == NULL ||
		AfxIsValidAddress(m_pNextFactory, sizeof(COleObjectFactory)));
}

void COleObjectFactory::Dump(CDumpContext& dc) const
{
	USES_CONVERSION;

	CCmdTarget::Dump(dc);

	dc << "m_pNextFactory = " << (void*)m_pNextFactory;
	dc << "\nm_dwRegister = " << m_dwRegister;
	dc << "\nm_bRegistered = " << m_bRegistered;
	LPOLESTR lpszClassID = NULL;
	if (StringFromCLSID(m_clsid, &lpszClassID) == S_OK)
	{
		dc << "\nm_clsid = " << OLE2CT(lpszClassID);
		CoTaskMemFree(lpszClassID);
	}
	dc << "\nm_pRuntimeClass = " << m_pRuntimeClass;
	dc << "\nm_bMultiInstance = " << m_bMultiInstance;
	dc << "\nm_lpszProgID = " << m_lpszProgID;
	dc << "\nm_bLicenseChecked = " << m_bLicenseChecked;
	dc << "\nm_bLicenseValid = " << m_bLicenseValid;

	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleObjectFactory, CCmdTarget)

/////////////////////////////////////////////////////////////////////////////
