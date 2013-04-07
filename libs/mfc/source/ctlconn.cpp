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

#ifdef AFXCTL_CORE3_SEG
#pragma code_seg(AFXCTL_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Connection map

BEGIN_CONNECTION_MAP(COleControl, CCmdTarget)
	CONNECTION_PART(COleControl, IID_IPropertyNotifySink, PropConnPt)
END_CONNECTION_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleControl::GetConnectionHook - override the connection map

LPCONNECTIONPOINT COleControl::GetConnectionHook(REFIID iid)
{
	if ((m_piidEvents != NULL) && IsEqualIID(iid, *m_piidEvents))
		return (LPCONNECTIONPOINT)((char*)&m_xEventConnPt +
				offsetof(CConnectionPoint, m_xConnPt));
	else
		return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::GetExtraConnectionPoints - override the connection map

BOOL COleControl::GetExtraConnectionPoints(CPtrArray* pConnPoints)
{
	pConnPoints->Add((char*)&m_xEventConnPt +
		offsetof(CConnectionPoint, m_xConnPt));
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XProvideClassInfo

STDMETHODIMP_(ULONG) COleControl::XProvideClassInfo::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, ProvideClassInfo)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XProvideClassInfo::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, ProvideClassInfo)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XProvideClassInfo::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, ProvideClassInfo)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XProvideClassInfo::GetClassInfo(
	LPTYPEINFO* ppTypeInfo)
{
	METHOD_PROLOGUE_EX(COleControl, ProvideClassInfo)

	CLSID clsid;
	pThis->GetClassID(&clsid);

	return pThis->GetTypeInfoOfGuid(GetUserDefaultLCID(), clsid, ppTypeInfo);
}

STDMETHODIMP COleControl::XProvideClassInfo::GetGUID(DWORD dwGuidKind,
	GUID* pGUID)
{
	METHOD_PROLOGUE_EX_(COleControl, ProvideClassInfo)

	if (dwGuidKind == GUIDKIND_DEFAULT_SOURCE_DISP_IID)
	{
		*pGUID = *pThis->m_piidEvents;
		return NOERROR;
	}
	else
	{
		*pGUID = GUID_NULL;
		return E_INVALIDARG;
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XEventConnPt

void COleControl::XEventConnPt::OnAdvise(BOOL bAdvise)
{
	METHOD_PROLOGUE_EX(COleControl, EventConnPt)
	pThis->OnEventAdvise(bAdvise);
}

REFIID COleControl::XEventConnPt::GetIID()
{
	METHOD_PROLOGUE_EX_(COleControl, EventConnPt)
	if (pThis->m_piidEvents != NULL)
		return *(pThis->m_piidEvents);
	else
		return GUID_NULL;
}

LPUNKNOWN COleControl::XEventConnPt::QuerySinkInterface(LPUNKNOWN pUnkSink)
{
	METHOD_PROLOGUE_EX_(COleControl, EventConnPt)

	// First, QI for control-specific IID; failing that, QI for IDispatch
	LPUNKNOWN pUnkReturn = NULL;
	if (FAILED(pUnkSink->QueryInterface(*(pThis->m_piidEvents),
		reinterpret_cast<void**>(&pUnkReturn))))
	{
		pUnkSink->QueryInterface(IID_IDispatch,
			reinterpret_cast<void**>(&pUnkReturn));
	}
	return pUnkReturn;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::OnEventAdvise - called by XEventConnPt::OnAdvise

void COleControl::OnEventAdvise(BOOL)
{
	// May be overridden by subclass
}

/////////////////////////////////////////////////////////////////////////////
// AfxConnectionAdvise

BOOL AFXAPI AfxConnectionAdvise(LPUNKNOWN pUnkSrc, REFIID iid,
	LPUNKNOWN pUnkSink, BOOL bRefCount, DWORD* pdwCookie)
{
	ASSERT_POINTER(pUnkSrc, IUnknown);
	ASSERT_POINTER(pUnkSink, IUnknown);
	ASSERT_POINTER(pdwCookie, DWORD);

	BOOL bSuccess = FALSE;

	LPCONNECTIONPOINTCONTAINER pCPC;

	if (SUCCEEDED(pUnkSrc->QueryInterface(
					IID_IConnectionPointContainer,
					(LPVOID*)&pCPC)))
	{
		ASSERT_POINTER(pCPC, IConnectionPointContainer);

		LPCONNECTIONPOINT pCP;

		if (SUCCEEDED(pCPC->FindConnectionPoint(iid, &pCP)))
		{
			ASSERT_POINTER(pCP, IConnectionPoint);

			if (SUCCEEDED(pCP->Advise(pUnkSink, pdwCookie)))
				bSuccess = TRUE;

			pCP->Release();

			// The connection point just AddRef'ed us.  If we don't want to
			// keep this reference count (because it would prevent us from
			// being deleted; our reference count wouldn't go to zero), then
			// we need to cancel the effects of the AddRef by calling
			// Release.

			if (bSuccess && !bRefCount)
				pUnkSink->Release();
		}

		pCPC->Release();
	}

	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// AfxConnectionUnadvise

BOOL AFXAPI AfxConnectionUnadvise(LPUNKNOWN pUnkSrc, REFIID iid,
	LPUNKNOWN pUnkSink, BOOL bRefCount, DWORD dwCookie)
{
	ASSERT_POINTER(pUnkSrc, IUnknown);
	ASSERT_POINTER(pUnkSink, IUnknown);

	// When we call Unadvise, the connection point will Release us.  If we
	// didn't keep the reference count when we called Advise, we need to
	// AddRef now, to keep our reference count consistent.  Note that if
	// the Unadvise fails, then we need to undo this extra AddRef by
	// calling Release before we return.

	if (!bRefCount)
		pUnkSink->AddRef();

	BOOL bSuccess = FALSE;

	LPCONNECTIONPOINTCONTAINER pCPC;

	if (SUCCEEDED(pUnkSrc->QueryInterface(
					IID_IConnectionPointContainer,
					(LPVOID*)&pCPC)))
	{
		ASSERT_POINTER(pCPC, IConnectionPointContainer);

		LPCONNECTIONPOINT pCP;

		if (SUCCEEDED(pCPC->FindConnectionPoint(iid, &pCP)))
		{
			ASSERT_POINTER(pCP, IConnectionPoint);

			if (SUCCEEDED(pCP->Unadvise(dwCookie)))
				bSuccess = TRUE;

			pCP->Release();
		}

		pCPC->Release();
	}

	// If we failed, undo the earlier AddRef.

	if (!bRefCount && !bSuccess)
		pUnkSink->Release();

	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
