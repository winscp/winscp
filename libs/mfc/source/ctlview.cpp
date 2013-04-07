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
// COleControl overridables for IViewObject implementation

BOOL COleControl::OnGetColorSet(DVTARGETDEVICE*, HDC, LPLOGPALETTE*)
{
	// Can be overridden by subclass
	return FALSE;
}

BOOL COleControl::OnGetViewExtent(DWORD dwDrawAspect, LONG /* lindex */,
	DVTARGETDEVICE* /* ptd */, LPSIZEL lpsizel)
{
	// Can be overridden by subclass for two-pass drawing
	if (dwDrawAspect == DVASPECT_OPAQUE || dwDrawAspect == DVASPECT_TRANSPARENT)
		dwDrawAspect = DVASPECT_CONTENT;
	return SUCCEEDED(m_xOleObject.GetExtent(dwDrawAspect, lpsizel));
}

BOOL COleControl::OnGetViewRect(DWORD dwAspect, LPRECTL pRect)
{
	// Can be overridden by subclass for two-pass drawing
	SIZEL size;
	OnGetViewExtent(dwAspect, -1, NULL, &size);
	pRect->left = 0;
	pRect->top = 0;
	pRect->right = size.cx;
	pRect->bottom = -size.cy;
	return TRUE;
}

DWORD COleControl::OnGetViewStatus()
{
	// Can be overridden by subclass for two-pass drawing
	return VIEWSTATUS_OPAQUE;
}

BOOL COleControl::OnQueryHitPoint(DWORD dwAspect, LPCRECT /* pRectBounds */,
	POINT /* ptlLoc */, LONG /* lCloseHint */, DWORD* pHitResult)
{
	// Can be overridden by subclass for non-rectangular hit-testing
	if (dwAspect == DVASPECT_CONTENT)
	{
		*pHitResult = HITRESULT_HIT;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

BOOL COleControl::OnQueryHitRect(DWORD dwAspect, LPCRECT /* pRectBounds */,
	LPCRECT /* prcLoc */, LONG /* lCloseHint */, DWORD* pHitResult)
{
	// Can be overridden by subclass for non-rectangular hit-testing
	if (dwAspect == DVASPECT_CONTENT)
	{
		*pHitResult = HITRESULT_HIT;
		return TRUE;
	}
	else
	{
		return FALSE;
	}
}

BOOL COleControl::OnGetNaturalExtent(DWORD /* dwAspect */, LONG /* lindex */,
	DVTARGETDEVICE* /* ptd */, HDC /* hicTargetDev */,
	DVEXTENTINFO* /* pExtentInfo */, LPSIZEL /* psizel */)
{
	// Can be overridden by subclass to provide sizing hints
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XViewObject

STDMETHODIMP_(ULONG) COleControl::XViewObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XViewObject::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XViewObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}


STDMETHODIMP COleControl::XViewObject::Draw(DWORD dwDrawAspect, LONG lindex,
	void* pvAspect, DVTARGETDEVICE* ptd, HDC hicTargetDev, HDC hdcDraw,
	LPCRECTL lprcBounds, LPCRECTL lprcWBounds,
	BOOL (CALLBACK* pfnContinue)(DWORD), DWORD dwContinue)
{
	METHOD_PROLOGUE_EX(COleControl, ViewObject)

	HRESULT hResult = S_OK;

	CRect rc;

	if (lprcBounds == NULL)
	{
		if (pThis->m_bInPlaceSiteWndless)
			rc.CopyRect(pThis->m_rcPos);
		else
			return E_INVALIDARG;
	}
	else
	{
		rc.SetRect((int)lprcBounds->left, (int)lprcBounds->top,
			(int)lprcBounds->right, (int)lprcBounds->bottom);
	}

	// Check if optimized drawing is permitted
	if (pvAspect != NULL && (pThis->GetControlFlags() & canOptimizeDraw))
	{
		pThis->m_bOptimizedDraw = (((DVASPECTINFO*)pvAspect)->dwFlags &
			DVASPECTINFOFLAG_CANOPTIMIZE);
	}

	AfxLockTempMaps();

	// Convert from rectangle from logical to device coordinates,
	// save DC state, and switch to MM_TEXT mode.  After drawing,
	// restore DC state.

	switch (dwDrawAspect)
	{
	case DVASPECT_CONTENT:
		if (GetDeviceCaps(hdcDraw, TECHNOLOGY) == DT_METAFILE)
		{
			// If attributes DC is NULL, create one, based on ptd.
			HDC hAttribDC = hicTargetDev;
			if (hicTargetDev == NULL)
				hAttribDC = ::_AfxOleCreateDC(ptd);

			// Draw into the metafile DC.
			CMetaFileDC dc;
			dc.Attach(hdcDraw);
			dc.SetAttribDC(hAttribDC);

			pThis->DrawMetafile(&dc, rc);

			dc.SetAttribDC(NULL);
			dc.Detach();

			// If we created an attributes DC, delete it now.
			if (hicTargetDev == NULL)
				::DeleteDC(hAttribDC);
		}
		else
		{
			CDC* pDC = CDC::FromHandle(hdcDraw);
			pThis->DrawContent(pDC, rc);
		}
		break;

	default:
		if (pThis->m_pDefIViewObject == NULL)
			pThis->m_pDefIViewObject =
				(LPVIEWOBJECT)pThis->QueryDefHandler(IID_IViewObject);

		if (pThis->m_pDefIViewObject != NULL)
		{
			hResult = pThis->m_pDefIViewObject->Draw(
				dwDrawAspect, lindex, pvAspect, ptd, hicTargetDev, hdcDraw,
				lprcBounds, lprcWBounds, pfnContinue, dwContinue);
		}
	}

	AfxUnlockTempMaps();
	pThis->m_bOptimizedDraw = FALSE;
	return hResult;
}

STDMETHODIMP COleControl::XViewObject::GetColorSet(DWORD dwDrawAspect,
	LONG lindex, void*, DVTARGETDEVICE* ptd, HDC hicTargetDev,
	LPLOGPALETTE* ppColorSet)
{
	METHOD_PROLOGUE_EX(COleControl, ViewObject)

	HRESULT hr = E_FAIL;

	if ((dwDrawAspect == DVASPECT_CONTENT) && (lindex == -1) &&
		pThis->OnGetColorSet(ptd, hicTargetDev, ppColorSet))
	{
		hr = S_OK;
	}

	return hr;
}

STDMETHODIMP COleControl::XViewObject::Freeze(DWORD, LONG, void*, DWORD*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XViewObject::Unfreeze(DWORD)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XViewObject::SetAdvise(DWORD aspects, DWORD advf,
					LPADVISESINK pAdvSink)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)

	_AFXCTL_ADVISE_INFO** ppAdviseInfo = &pThis->m_pAdviseInfo;

	// Allocate space for advise info, if necessary.
	if (*ppAdviseInfo == NULL)
	{
		TRY
			*ppAdviseInfo = new _AFXCTL_ADVISE_INFO;
		END_TRY

		if (*ppAdviseInfo == NULL)
			return E_OUTOFMEMORY;
	}

	_AFXCTL_ADVISE_INFO* pAdviseInfo = *ppAdviseInfo;

	// Release previous sink, if any.
	if (pAdviseInfo->m_pAdvSink != NULL)
		pAdviseInfo->m_pAdvSink->Release();

	// Store new advise info.
	pAdviseInfo->m_dwAspects = aspects;
	pAdviseInfo->m_dwAdvf = advf;
	pAdviseInfo->m_pAdvSink = pAdvSink;
	if (pAdvSink != NULL)
		pAdvSink->AddRef();

	return S_OK;
}

STDMETHODIMP COleControl::XViewObject::GetAdvise(DWORD* pAspects, DWORD* pAdvf,
	LPADVISESINK* ppAdvSink)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)

	_AFXCTL_ADVISE_INFO* pAdviseInfo = pThis->m_pAdviseInfo;

	if ((pAdviseInfo != NULL) && (pAdviseInfo->m_pAdvSink != NULL))
	{
		if (pAspects != NULL)
			*pAspects = pAdviseInfo->m_dwAspects;

		if (pAdvf != NULL)
			*pAdvf = pAdviseInfo->m_dwAdvf;

		if (ppAdvSink != NULL)
		{
			*ppAdvSink = pAdviseInfo->m_pAdvSink;
			if (*ppAdvSink != NULL)
				(*ppAdvSink)->AddRef();
		}
	}
	else
	{
		if (pAspects != NULL)
			*pAspects = 0;

		if (pAdvf != NULL)
			*pAdvf = 0;

		if (ppAdvSink != NULL)
			*ppAdvSink = NULL;
	}

	return S_OK;
}

STDMETHODIMP COleControl::XViewObject::GetExtent(DWORD dwDrawAspect,
	LONG lindex, DVTARGETDEVICE* ptd, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return pThis->OnGetViewExtent(dwDrawAspect, lindex, ptd, lpsizel) ? S_OK :
		E_FAIL;
}

STDMETHODIMP COleControl::XViewObject::GetRect(DWORD dwAspect, LPRECTL pRect)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return pThis->OnGetViewRect(dwAspect, pRect) ? S_OK : DV_E_DVASPECT;
}

STDMETHODIMP COleControl::XViewObject::GetViewStatus(DWORD* pdwStatus)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	*pdwStatus = pThis->OnGetViewStatus();
	return S_OK;
}

STDMETHODIMP COleControl::XViewObject::QueryHitPoint(DWORD dwAspect,
	LPCRECT pRectBounds, POINT ptlLoc, LONG lCloseHint, DWORD* pHitResult)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return pThis->OnQueryHitPoint(dwAspect, pRectBounds, ptlLoc, lCloseHint,
		pHitResult) ? S_OK : E_FAIL;
}

STDMETHODIMP COleControl::XViewObject::QueryHitRect(DWORD dwAspect,
	LPCRECT pRectBounds, LPCRECT prcLoc, LONG lCloseHint, DWORD* pHitResult)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return pThis->OnQueryHitRect(dwAspect, pRectBounds, prcLoc, lCloseHint,
		pHitResult) ? S_OK : E_FAIL;
}

STDMETHODIMP COleControl::XViewObject::GetNaturalExtent(DWORD dwAspect,
	LONG lindex, DVTARGETDEVICE* ptd, HDC hicTargetDev,
	DVEXTENTINFO* pExtentInfo, LPSIZEL psizel)
{
	METHOD_PROLOGUE_EX_(COleControl, ViewObject)
	return pThis->OnGetNaturalExtent(dwAspect, lindex, ptd, hicTargetDev,
		pExtentInfo, psizel) ? S_OK : E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
