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
#include "occimpl.h"

#ifdef AFX_OCC_SEG
#pragma code_seg(AFX_OCC_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////

COleControlLock::COleControlLock(REFCLSID clsid)
{
	// initialize the object
	m_pNextLock = NULL;
	m_clsid = clsid;
	m_pClassFactory = NULL;

	// initialize OLE, if necessary
	_AFX_THREAD_STATE* pState = AfxGetThreadState();
	if (!pState->m_bNeedTerm && !AfxOleInit())
		return;

	// attempt to lock the class factory of the control
	if (SUCCEEDED(::CoGetClassObject(clsid,
		CLSCTX_INPROC_SERVER | CLSCTX_INPROC_HANDLER, NULL, IID_IClassFactory,
		(void**)&m_pClassFactory)))
	{
		ASSERT(m_pClassFactory != NULL);
		if (FAILED(m_pClassFactory->LockServer(TRUE)))
		{
			m_pClassFactory->Release();
			m_pClassFactory = NULL;
		}
	}
}

COleControlLock::~COleControlLock()
{
	if (m_pClassFactory != NULL)
	{
		m_pClassFactory->LockServer(FALSE);
		m_pClassFactory->Release();
		m_pClassFactory = NULL;
	}
}

BOOL AFXAPI AfxOleLockControl(REFCLSID clsid)
{
	COleControlLock* pLock = NULL;

	TRY
	{
		pLock = new COleControlLock(clsid);
		if (pLock->m_pClassFactory == NULL)
		{
			delete pLock;
			pLock = NULL;
		}
		else
		{
			AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
			AfxLockGlobals(CRIT_CTLLOCKLIST);
			pModuleState->m_lockList.AddHead(pLock);
			AfxUnlockGlobals(CRIT_CTLLOCKLIST);
		}
	}
	CATCH_ALL (e)
	{
		// Note: DELETE_EXCEPTION(e) not necessary
		pLock = NULL;
	}
	END_CATCH_ALL

	return pLock != NULL;
}

BOOL AFXAPI AfxOleLockControl(LPCTSTR lpszProgID)
{
	// map prog id to CLSID
	CLSID clsid;
	HRESULT hr = AfxGetClassIDFromString(lpszProgID, &clsid);
	if (FAILED(hr))
		return FALSE;

	return AfxOleLockControl(clsid);
}

BOOL AFXAPI AfxOleUnlockControl(LPCTSTR lpszProgID)
{
	// map prog id to CLSID
	CLSID clsid;
	HRESULT hr = AfxGetClassIDFromString(lpszProgID, &clsid);
	if (FAILED(hr))
		return FALSE;

	return AfxOleUnlockControl(clsid);
}

BOOL AFXAPI AfxOleUnlockControl(REFCLSID clsid)
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AfxLockGlobals(CRIT_CTLLOCKLIST);
	COleControlLock* pLock = pModuleState->m_lockList;
	BOOL bResult = FALSE;
	while (pLock != NULL)
	{
		COleControlLock* pNext = pLock->m_pNextLock;
		if (clsid == pLock->m_clsid)
		{
			// remove lock from list and delete it
			pModuleState->m_lockList.Remove(pLock);
			delete pLock;
			bResult = TRUE;
		}
		pLock = pNext;
	}
	AfxUnlockGlobals(CRIT_CTLLOCKLIST);
	return bResult;
}

void AFXAPI AfxOleUnlockAllControls()
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	COleControlLock* pLock;
	AfxLockGlobals(CRIT_CTLLOCKLIST);
	while ((pLock = pModuleState->m_lockList) != NULL)
	{
		pModuleState->m_lockList.Remove(pLock);
		delete pLock;
	}
	ASSERT(pModuleState->m_lockList.IsEmpty());
	AfxUnlockGlobals(CRIT_CTLLOCKLIST);
}
