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

#ifdef AFXCTL_CORE2_SEG
#pragma code_seg(AFXCTL_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CCmdTarget::EnableTypeLib - locks the typelib cache for this class

void CCmdTarget::EnableTypeLib()
{
	AfxLockGlobals(CRIT_TYPELIBCACHE);

	CTypeLibCache* pTypeLibCache = GetTypeLibCache();
	ASSERT(pTypeLibCache != NULL);  // must override GetTypeLibCache

	if (pTypeLibCache != NULL)
		pTypeLibCache->Lock();      // will be unlocked in OnFinalRelease

	AfxUnlockGlobals(CRIT_TYPELIBCACHE);
}

/////////////////////////////////////////////////////////////////////////////
// CCmdTarget::GetTypeInfoOfGuid - Returns typeinfo

HRESULT CCmdTarget::GetTypeInfoOfGuid(LCID lcid, REFGUID guid,
	LPTYPEINFO* ppTypeInfo)
{
	USES_CONVERSION;

	AfxLockGlobals(CRIT_TYPELIBCACHE);

	HRESULT hr = TYPE_E_CANTLOADLIBRARY;
	CTypeLibCache* pTypeLibCache = GetTypeLibCache();
	LPTYPELIB pTypeLib = NULL;

	// If type info is already cached, just return it.
	if (pTypeLibCache->LookupTypeInfo(lcid, guid, ppTypeInfo))
	{
		hr = S_OK;
	}
	else
	{
		// If type library isn't already cached, try to locate it.
		if (!pTypeLibCache->Lookup(lcid, &pTypeLib))
		{
			// First, try getting the subclass to load the type library
			// (normally this goes through LoadRegTypeLib).

			if (FAILED(GetTypeLib(lcid, &pTypeLib)))
			{
				AFX_MANAGE_STATE(m_pModuleState);

				// If that failed, try loading the type library from our own
				// resources.

				TCHAR szPath[_MAX_PATH];
				GetModuleFileName(AfxGetInstanceHandle(), szPath, _MAX_PATH);

				if (FAILED(LoadTypeLib(T2COLE(szPath), &pTypeLib)))
					pTypeLib = NULL;
			}

			pTypeLibCache->Cache(lcid, pTypeLib);
		}

		// If we got a type library, extract the requested type info.
		if (pTypeLib != NULL)
		{
			hr = pTypeLib->GetTypeInfoOfGuid(guid, ppTypeInfo);
			pTypeLib->Release();
			pTypeLibCache->CacheTypeInfo(lcid, guid, *ppTypeInfo);
		}
	}

	AfxUnlockGlobals(CRIT_TYPELIBCACHE);

	return hr;
}

/////////////////////////////////////////////////////////////////////////////
// AfxGetTypeLibCache

CTypeLibCache* AFXAPI AfxGetTypeLibCache(const GUID* pTypeLibID)
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	ASSERT(pModuleState != NULL);

	if (pModuleState->m_pTypeLibCacheMap == NULL)
	{
		const GUID* pCachedTypeLibID = pModuleState->m_typeLibCache.m_pTypeLibID;

		if (pCachedTypeLibID == NULL)
		{
			// Cache not initialized yet; initialize it now
			pModuleState->m_typeLibCache.m_pTypeLibID = pTypeLibID;
			return &pModuleState->m_typeLibCache;
		}
		else if (pCachedTypeLibID == pTypeLibID)
		{
			// This is the cache we need; return it
			return &pModuleState->m_typeLibCache;
		}
		else
		{
			// Another cache is needed; initialize the map
			pModuleState->m_pTypeLibCacheMap = new CTypeLibCacheMap;
			pModuleState->m_pTypeLibCacheMap->SetAt(
				(void*)(const void*)pModuleState->m_typeLibCache.m_pTypeLibID,
				&pModuleState->m_typeLibCache);

			// FALL THRU
		}
	}

	ASSERT(pModuleState->m_pTypeLibCacheMap != NULL);

	// Try to locate cache in the map
	CTypeLibCache* pCache;
	if (!pModuleState->m_pTypeLibCacheMap->Lookup((void*)(const void*)pTypeLibID,
		(void*&)pCache))
	{
		// Create new cache and add it to the map
		pCache = new CTypeLibCache;
		pCache->m_pTypeLibID = pTypeLibID;
		pModuleState->m_pTypeLibCacheMap->SetAt((void*)(const void*)pTypeLibID, pCache);
	}

	ASSERT(pCache != NULL);
	ASSERT(pCache->m_pTypeLibID == pTypeLibID);
	return pCache;
}

/////////////////////////////////////////////////////////////////////////////
// CTypeLibCache
// (Note: CTypeLibCache::Unlock is implemented in afxstate.cpp)

void CTypeLibCache::Lock()
{
	ASSERT(m_cRef >= 0);

	if (m_cRef == 0)
		m_lcid = (LCID)-1;

	InterlockedIncrement(&m_cRef);
}

BOOL CTypeLibCache::Lookup(LCID lcid, LPTYPELIB* pptlib)
{
	if ((m_lcid != -1) && (m_lcid == lcid))
	{
		ASSERT(m_ptlib != NULL);
		*pptlib = m_ptlib;
		m_ptlib->AddRef();
		return TRUE;
	}
	else
	{
		*pptlib = NULL;
		return FALSE;
	}
}

void CTypeLibCache::Cache(LCID lcid, LPTYPELIB ptlib)
{
	if (ptlib != NULL)
	{
		m_lcid = lcid;
		m_guidInfo = GUID_NULL;
		RELEASE(m_ptinfo);
		RELEASE(m_ptlib);
		m_ptlib = ptlib;
		m_ptlib->AddRef();
	}
}

BOOL CTypeLibCache::LookupTypeInfo(LCID lcid, REFGUID guid,
	LPTYPEINFO* pptinfo)
{
	if ((m_lcid != -1) && (m_lcid == lcid) && (m_ptinfo != NULL) &&
		IsEqualGUID(m_guidInfo, guid))
	{
		ASSERT(m_ptlib != NULL);
		ASSERT(m_ptinfo != NULL);
		*pptinfo = m_ptinfo;
		m_ptinfo->AddRef();
		return TRUE;
	}
	else
	{
		*pptinfo = NULL;
		return FALSE;
	}
}

void CTypeLibCache::CacheTypeInfo(LCID lcid, REFGUID guid, LPTYPEINFO ptinfo)
{
	if ((ptinfo != NULL) && (m_lcid == lcid))
	{
		m_guidInfo = guid;
		RELEASE(m_ptinfo);
		m_ptinfo = ptinfo;
		m_ptinfo->AddRef();
	}
}
