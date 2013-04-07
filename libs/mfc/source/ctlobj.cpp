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
// COleControl::XOleObject

STDMETHODIMP_(ULONG) COleControl::XOleObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XOleObject::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XOleObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XOleObject::SetClientSite(LPOLECLIENTSITE pClientSite)
{
	METHOD_PROLOGUE_EX(COleControl, OleObject)

	ASSERT_NULL_OR_POINTER(pClientSite, IOleClientSite);

	// maintain reference counts
	if (pClientSite != NULL)
		pClientSite->AddRef();
	RELEASE(pThis->m_pClientSite);
	pThis->m_pClientSite = pClientSite;

	// Release existing pointer to ambient property dispinterface.
	pThis->m_ambientDispDriver.ReleaseDispatch();

	if (pClientSite != NULL)
	{
		BOOL bValue;
		pThis->m_bAutoClip =
			pThis->GetAmbientProperty(DISPID_AMBIENT_AUTOCLIP, VT_BOOL, &bValue) ? bValue : 0;
		pThis->m_bMsgReflect =
			pThis->GetAmbientProperty(DISPID_AMBIENT_MESSAGEREFLECT, VT_BOOL, &bValue) ? bValue : 0;
		pThis->m_bUIDead = (BYTE)(pThis->AmbientUIDead());
	}

	// Release existing pointer to in-place site, if any.
	RELEASE(pThis->m_pInPlaceSite);

	// Initialize pointer to control site
	LPVOID pInterface;
	if (pClientSite == NULL ||
		FAILED(pClientSite->QueryInterface(IID_IOleControlSite, &pInterface)))
	{
		pInterface = NULL;
	}

	RELEASE(pThis->m_pControlSite);
	pThis->m_pControlSite = (LPOLECONTROLSITE) pInterface;

	// Initialize pointer to simple frame site
	if (pClientSite == NULL || !pThis->m_bSimpleFrame ||
		FAILED(pClientSite->QueryInterface(IID_ISimpleFrameSite, &pInterface)))
	{
		pInterface = NULL;
	}

	RELEASE(pThis->m_pSimpleFrameSite);
	pThis->m_pSimpleFrameSite = (LPSIMPLEFRAMESITE) pInterface;

	// Let the control run its own code here.
	pThis->OnSetClientSite();

	// Unless IPersist*::Load or IPersist*::InitNew is called after this,
	// we can't count on ambient properties being available while loading.
	pThis->m_bCountOnAmbients = FALSE;

	return S_OK;
}

STDMETHODIMP COleControl::XOleObject::GetClientSite(LPOLECLIENTSITE* ppClientSite)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	ASSERT_POINTER(ppClientSite, LPOLECLIENTSITE);

	LPOLECLIENTSITE pClientSite = pThis->m_pClientSite;
	*ppClientSite = pClientSite;
	if (pClientSite != NULL)
		pClientSite->AddRef();

	return (pClientSite != NULL) ? S_OK : E_FAIL;
}

STDMETHODIMP COleControl::XOleObject::SetHostNames(LPCOLESTR, LPCOLESTR)
{
	return S_OK;
}

STDMETHODIMP COleControl::XOleObject::Close(DWORD dwSaveOption)
{
	METHOD_PROLOGUE_EX(COleControl, OleObject)
	pThis->OnClose(dwSaveOption);
	return S_OK;
}

void COleControl::OnClose(DWORD dwSaveOption)
{
	if (m_bInPlaceActive)
		m_xOleInPlaceObject.InPlaceDeactivate();

	if (((dwSaveOption == OLECLOSE_SAVEIFDIRTY) || (dwSaveOption == OLECLOSE_PROMPTSAVE)) &&
		m_bModified)
	{
		SendAdvise(OBJECTCODE_SAVEOBJECT);
		SendAdvise(OBJECTCODE_SAVED);
	}
}

STDMETHODIMP COleControl::XOleObject::SetMoniker(DWORD, LPMONIKER)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleObject::GetMoniker(DWORD, DWORD, LPMONIKER*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleObject::InitFromData(LPDATAOBJECT, BOOL, DWORD)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleObject::GetClipboardData(DWORD, LPDATAOBJECT*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleObject::DoVerb(LONG iVerb, LPMSG lpmsg,
	LPOLECLIENTSITE pActiveSite, LONG lindex, HWND hwndParent,
	LPCRECT lprcPosRect)
{
	METHOD_PROLOGUE_EX(COleControl, OleObject)

	TRY
	{
		if (pThis->OnDoVerb(iVerb, lpmsg, hwndParent, lprcPosRect))
			return S_OK;                 // Custom verb succeeded
	}
	CATCH (CException, e)
	{
		return E_FAIL;     // Custom verb failed
	}
	END_CATCH

	// Custom verb not found, invoke standard verb instead.

	HRESULT hr;

	switch (iVerb)
	{
	case OLEIVERB_HIDE:
		pThis->m_xOleInPlaceObject.UIDeactivate();
		return pThis->OnHide();


	case OLEIVERB_SHOW:
		return pThis->OnOpen(-1, lpmsg);      // Try in-place first

	case OLEIVERB_UIACTIVATE:
	case OLEIVERB_INPLACEACTIVATE:
#ifdef _AFXDLL
		if (pThis->m_bOpen)
			return OLE_E_NOT_INPLACEACTIVE;    // Already open
#endif

		// FALL THRU

	case OLEIVERB_PRIMARY:
		if (lprcPosRect != NULL)
			CopyRect(&pThis->m_rcPos, lprcPosRect);
		else
			memset(&pThis->m_rcPos, 0, sizeof(pThis->m_rcPos));
		return pThis->OnActivateInPlace((iVerb != OLEIVERB_INPLACEACTIVATE), lpmsg);

#ifdef _AFXDLL
	case OLEIVERB_OPEN:
		return pThis->OnOpen(FALSE, lpmsg);     // Don't try in-place
#endif

	case OLEIVERB_PROPERTIES:
		return pThis->OnProperties(lpmsg, hwndParent, lprcPosRect) ?
				S_OK : E_FAIL;

	default:
		// negative verbs not understood should return E_NOTIMPL
		if (iVerb < 0)
			return E_NOTIMPL;

		// positive verb not processed --
		// according to OLE spec, primary verb should be executed
		// instead.
		if (SUCCEEDED(hr = DoVerb(OLEIVERB_PRIMARY, lpmsg, pActiveSite,
								lindex, hwndParent, lprcPosRect)))
			return OLEOBJ_S_INVALIDVERB;
		else
			return hr;
	}
}

BOOL COleControl::OnDoVerb(LONG iVerb, LPMSG lpMsg, HWND hWndParent,
	LPCRECT lpRect)
{
	return DoOleVerb(iVerb, lpMsg, hWndParent, lpRect);
}

STDMETHODIMP COleControl::XOleObject::EnumVerbs(LPENUMOLEVERB* ppenumOleVerb)
{
	METHOD_PROLOGUE_EX(COleControl, OleObject)
	return pThis->OnEnumVerbs(ppenumOleVerb) ?
		S_OK : OLEOBJ_E_NOVERBS;
}

BOOL COleControl::OnEnumVerbs(LPENUMOLEVERB* ppenumOleVerb)
{
	return EnumOleVerbs(ppenumOleVerb);
}

STDMETHODIMP COleControl::XOleObject::Update()
{
	return S_OK;
}

STDMETHODIMP COleControl::XOleObject::IsUpToDate()
{
	return S_OK;
}

STDMETHODIMP COleControl::XOleObject::GetUserClassID(CLSID* pClsid)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)
	return pThis->GetClassID(pClsid);
}

STDMETHODIMP COleControl::XOleObject::GetUserType(DWORD, LPOLESTR* ppszUserType)
{
	METHOD_PROLOGUE_EX(COleControl, OleObject)
	UNUSED(ppszUserType);   // not used in release builds
	ASSERT_POINTER(ppszUserType, LPOLESTR);
	TCHAR szUserType[256];
	pThis->GetUserType(szUserType);
	*ppszUserType = AfxAllocTaskOleString(szUserType);
	return S_OK;
}

void COleControl::GetUserType(LPTSTR pszUserType)
{
	ASSERT(pszUserType != NULL);
	pszUserType[0] = '\0';

	TRY
	{
		AfxLoadString(GetUserTypeNameID(), pszUserType);
	}
	END_TRY
}

STDMETHODIMP COleControl::XOleObject::SetExtent(DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	if (dwDrawAspect & DVASPECT_CONTENT)
		return pThis->OnSetExtent(lpsizel) ? S_OK : E_FAIL;
	else
		return E_NOTIMPL;
}

BOOL COleControl::OnSetExtent(LPSIZEL lpSizeL)
{
	if (m_bChangingExtent)  // Prevent infinite recursion!
		return FALSE;

	m_bChangingExtent = TRUE;

	// Update the control's extent.
	m_cxExtent = lpSizeL->cx;
	m_cyExtent = lpSizeL->cy;

	// Mark the control dirty and force a repaint.
	SetModifiedFlag();
	InvalidateControl();

	SIZEL szlPixels;
	_AfxXformSizeInHimetricToPixels(NULL, lpSizeL, &szlPixels);

	CRect rectNew(m_rcPos);
	rectNew.right = rectNew.left + (int)szlPixels.cx;
	rectNew.bottom = rectNew.top + (int)szlPixels.cy;

	if ((m_pInPlaceSite != NULL) && m_bInPlaceActive)
	{
		// If the control is in-place active, tell the container to resize.
		m_pInPlaceSite->OnPosRectChange(&rectNew);
	}
#ifdef _AFXDLL
	else if (m_bOpen)
	{
		// If the control is open, resize it.
		ResizeOpenControl((int)szlPixels.cx, (int)szlPixels.cy);
	}
#endif
	else
	{
		CopyRect(m_rcPos, rectNew);

		// Resize off-screen window, if any.
		if (m_hWnd != NULL)
			::SetWindowPos(m_hWnd, NULL, 0, 0, (int)szlPixels.cx,
				(int)szlPixels.cy, SWP_NOZORDER|SWP_NOMOVE|SWP_NOACTIVATE);
	}

	m_bChangingExtent = FALSE;
	return TRUE;
}

STDMETHODIMP COleControl::XOleObject::GetExtent(DWORD dwDrawAspect,
	LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	if (dwDrawAspect & DVASPECT_CONTENT)
	{
		lpsizel->cx = pThis->m_cxExtent;
		lpsizel->cy = pThis->m_cyExtent;
		return S_OK;
	}
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleObject::Advise(LPADVISESINK pAdvSink,
	DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	// If no advise holder exists, create one.
	// Then delegate this call to the advise holder.

	if (pThis->m_pOleAdviseHolder == NULL)
	{
		HRESULT hr;
		if (FAILED(hr = CreateOleAdviseHolder(&pThis->m_pOleAdviseHolder)))
			return hr;
	}

	return pThis->m_pOleAdviseHolder->Advise(pAdvSink, pdwConnection);
}

STDMETHODIMP COleControl::XOleObject::Unadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	if (pThis->m_pOleAdviseHolder != NULL)
		return pThis->m_pOleAdviseHolder->Unadvise(dwConnection);

	return E_FAIL;
}

STDMETHODIMP COleControl::XOleObject::EnumAdvise(LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)

	if (pThis->m_pOleAdviseHolder != NULL)
		return pThis->m_pOleAdviseHolder->EnumAdvise(ppenumAdvise);

	return E_FAIL;
}

STDMETHODIMP COleControl::XOleObject::GetMiscStatus(DWORD dwAspect,
	DWORD* pdwStatus)
{
	METHOD_PROLOGUE_EX_(COleControl, OleObject)
	ASSERT_POINTER(pdwStatus, DWORD);

	if (dwAspect == DVASPECT_CONTENT)
		*pdwStatus = pThis->GetMiscStatus();
	else
		*pdwStatus = 0;

	return S_OK;
}

STDMETHODIMP COleControl::XOleObject::SetColorScheme(LPLOGPALETTE)
{
	return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
