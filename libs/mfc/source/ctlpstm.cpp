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

#ifdef AFXCTL_PROP_SEG
#pragma code_seg(AFXCTL_PROP_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleControl::XPersistStreamInit

STDMETHODIMP_(ULONG) COleControl::XPersistStreamInit::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistStreamInit)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XPersistStreamInit::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistStreamInit)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XPersistStreamInit::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, PersistStreamInit)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XPersistStreamInit::GetClassID(LPCLSID lpClassID)
{
	METHOD_PROLOGUE_EX_(COleControl, PersistStreamInit)
	return pThis->GetClassID(lpClassID);
}

STDMETHODIMP COleControl::XPersistStreamInit::IsDirty()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistStreamInit)
	return pThis->m_bModified ? S_OK : S_FALSE;
}

STDMETHODIMP COleControl::XPersistStreamInit::Load(LPSTREAM pStm)
{
	METHOD_PROLOGUE_EX(COleControl, PersistStreamInit)
	return pThis->LoadState(pStm);
}

STDMETHODIMP COleControl::XPersistStreamInit::Save(LPSTREAM pStm,
	BOOL fClearDirty)
{
	METHOD_PROLOGUE_EX(COleControl, PersistStreamInit)

	// Delegate to SaveState.
	HRESULT hr = pThis->SaveState(pStm);

	// Bookkeeping:  Clear the dirty flag, if requested.
	if (fClearDirty)
		pThis->m_bModified = FALSE;

	return hr;
}

STDMETHODIMP COleControl::XPersistStreamInit::GetSizeMax(ULARGE_INTEGER*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XPersistStreamInit::InitNew()
{
	METHOD_PROLOGUE_EX(COleControl, PersistStreamInit)

	// Delegate to OnResetState.
	pThis->OnResetState();

	// Unless IOleObject::SetClientSite is called after this, we can
	// count on ambient properties being available while loading.
	pThis->m_bCountOnAmbients = TRUE;

	// Properties have been initialized
	pThis->m_bInitialized = TRUE;

	// Uncache cached ambient properties
	_afxAmbientCache->Cache(NULL);

	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XPersistMemory

STDMETHODIMP_(ULONG) COleControl::XPersistMemory::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistMemory)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XPersistMemory::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistMemory)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XPersistMemory::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, PersistMemory)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XPersistMemory::GetClassID(LPCLSID lpClassID)
{
	METHOD_PROLOGUE_EX_(COleControl, PersistMemory)
	return pThis->GetClassID(lpClassID);
}

STDMETHODIMP COleControl::XPersistMemory::IsDirty()
{
	METHOD_PROLOGUE_EX_(COleControl, PersistMemory)
	return pThis->m_bModified ? S_OK : S_FALSE;
}

STDMETHODIMP COleControl::XPersistMemory::Load(LPVOID lpStream, ULONG cbSize)
{
	METHOD_PROLOGUE_EX(COleControl, PersistMemory)

	HRESULT hr = S_OK;
	TRY
	{
		// Delegate to the Serialize method.
		CMemFile file((LPBYTE)lpStream, cbSize);
		CArchive ar(&file, CArchive::load);
		pThis->Serialize(ar);
	}
	CATCH_ALL(e)
	{
		// The load failed.  Delete any partially-initialized state.
		pThis->OnResetState();
		pThis->m_bInitialized = TRUE;
		hr = E_FAIL;
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	// Clear the modified flag.
	pThis->m_bModified = FALSE;

	// Unless IOleObject::SetClientSite is called after this, we can
	// count on ambient properties being available while loading.
	pThis->m_bCountOnAmbients = TRUE;

	// Properties have been initialized
	pThis->m_bInitialized = TRUE;

	// Uncache cached ambient properties
	_afxAmbientCache->Cache(NULL);

	return hr;
}

STDMETHODIMP COleControl::XPersistMemory::Save(LPVOID, BOOL, ULONG)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XPersistMemory::GetSizeMax(ULONG*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XPersistMemory::InitNew()
{
	METHOD_PROLOGUE_EX(COleControl, PersistMemory)

	// Delegate to OnResetState.
	pThis->OnResetState();

	// Unless IOleObject::SetClientSite is called after this, we can
	// count on ambient properties being available while loading.
	pThis->m_bCountOnAmbients = TRUE;

	// Properties have been initialized
	pThis->m_bInitialized = TRUE;

	// Uncache cached ambient properties
	_afxAmbientCache->Cache(NULL);

	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
