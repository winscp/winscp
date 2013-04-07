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

#ifdef AFXCTL_CORE1_SEG
#pragma code_seg(AFXCTL_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CWindowlessDC - used by COleControl::GetDC and COleControl::ReleaseDC

class CWindowlessDC : public CDC
{
	DECLARE_DYNAMIC(CWindowlessDC)
public:
	CWindowlessDC(HDC hDC, CPoint& pointOrigin);
	HDC Detach();
protected:
	CPoint m_pointOrigin;
};

IMPLEMENT_DYNAMIC(CWindowlessDC, CDC)

CWindowlessDC::CWindowlessDC(HDC hDC, CPoint& pointOrigin)
{
	m_hDC = m_hAttribDC = hDC;
	m_pointOrigin =  GetViewportOrg();
	SetViewportOrg(m_pointOrigin + pointOrigin);
}

HDC CWindowlessDC::Detach()
{
	SetViewportOrg(m_pointOrigin);
	HDC hDC = m_hDC;
	m_hDC = m_hAttribDC = NULL;
	return hDC;
}

/////////////////////////////////////////////////////////////////////////////
// Overridables used with the various windowless interfaces

void COleControl::GetClientOffset(long* pdxOffset, long* pdyOffset) const
{
	int nOffset = (m_sBorderStyle == 1) + (2 * (m_sAppearance == 1));

	if (nOffset > 0)
	{
		*pdxOffset = nOffset * GetSystemMetrics(SM_CXBORDER);
		*pdyOffset = nOffset * GetSystemMetrics(SM_CYBORDER);
	}
	else
	{
		*pdxOffset = *pdyOffset = 0;
	}
}

UINT COleControl::ParentToClient(LPCRECT lprcBounds, LPPOINT pPoint,
	BOOL bHitTest) const
{
	long dxOffset;
	long dyOffset;
	GetClientOffset(&dxOffset, &dyOffset);

	UINT nHitTest = HTNOWHERE;

	if (bHitTest && ::PtInRect(lprcBounds, *pPoint))
	{
		if (dxOffset > 0)
		{
			CRect rectClient(lprcBounds);
			rectClient.InflateRect(-dxOffset, -dyOffset);
			nHitTest = rectClient.PtInRect(*pPoint) ? HTCLIENT : HTBORDER;
		}
		else
		{
			nHitTest = HTCLIENT;
		}
	}

	pPoint->x -= lprcBounds->left + dxOffset;
	pPoint->y -= lprcBounds->top  + dyOffset;

	return nHitTest;
}

void COleControl::ClientToParent(LPCRECT lprcBounds, LPPOINT pPoint) const
{
	long dxOffset;
	long dyOffset;
	GetClientOffset(&dxOffset, &dyOffset);
	pPoint->x += lprcBounds->left + dxOffset;
	pPoint->y += lprcBounds->top  + dyOffset;
}

/////////////////////////////////////////////////////////////////////////////
// Overridables for IPointerInactive methods

DWORD COleControl::GetActivationPolicy()
{
	return 0;
}

BOOL COleControl::OnInactiveSetCursor(LPCRECT lprcBounds, long x, long y,
	DWORD dwMouseMsg, BOOL bSetAlways)
{
	CPoint point(x, y);
	UINT nHitTest = ParentToClient(lprcBounds, &point, TRUE);

	LRESULT lResult = 0;
	OnWndMsg(WM_SETCURSOR, nHitTest, dwMouseMsg, &lResult);

	if (bSetAlways && ! lResult)
		::SetCursor(::LoadCursor(NULL, IDC_ARROW));

	return bSetAlways || lResult;
}

void COleControl::OnInactiveMouseMove(LPCRECT lprcBounds, long x, long y,
	DWORD dwKeyState)
{
	CPoint point(x, y);
	ParentToClient(lprcBounds, &point);
	OnWndMsg(WM_MOUSEMOVE, dwKeyState, MAKELONG(point.x, point.y), NULL);
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XPointerInactive

STDMETHODIMP_(ULONG) COleControl::XPointerInactive::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XPointerInactive::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XPointerInactive::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XPointerInactive::GetActivationPolicy(
   DWORD* pdwPolicy)
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	*pdwPolicy = pThis->GetActivationPolicy();
	return S_OK;
}

STDMETHODIMP COleControl::XPointerInactive::OnInactiveSetCursor(
	LPCRECT lprcBounds, long x, long y, DWORD dwMouseMsg, BOOL bSetAlways)
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	return pThis->OnInactiveSetCursor(lprcBounds, x, y, dwMouseMsg, bSetAlways) ?
		S_OK : S_FALSE;
}

STDMETHODIMP COleControl::XPointerInactive::OnInactiveMouseMove(
	LPCRECT lprcBounds, long x, long y, DWORD dwKeyState)
{
	METHOD_PROLOGUE_EX_(COleControl, PointerInactive)
	pThis->OnInactiveMouseMove(lprcBounds, x, y, dwKeyState);
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// Overridables for IOleInPlaceObjectWindowless methods

BOOL COleControl::OnWindowlessMessage(UINT msg, WPARAM wParam, LPARAM lParam,
	LRESULT* plResult)
{
	if ((msg >= WM_MOUSEFIRST && msg <= WM_MOUSELAST) || (msg == WM_CONTEXTMENU))
	{
		CPoint point(LOWORD(lParam), HIWORD(lParam));
		ParentToClient(m_rcPos, &point, FALSE);
		lParam = MAKELONG(point.x, point.y);
	}

	return OnWndMsg(msg, wParam, lParam, plResult);
}

IDropTarget* COleControl::GetWindowlessDropTarget()
{
	return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XOleInPlaceObject (IOleInPlaceObjectWindowless methods)

STDMETHODIMP COleControl::XOleInPlaceObject::OnWindowMessage(UINT msg,
	WPARAM wParam, LPARAM lParam, LRESULT* plResult)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)
	return pThis->OnWindowlessMessage(msg, wParam, lParam, plResult) ?
		S_OK : S_FALSE;
}

STDMETHODIMP COleControl::XOleInPlaceObject::GetDropTarget(
	IDropTarget** ppDropTarget)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)

	*ppDropTarget = pThis->GetWindowlessDropTarget();
	return (*ppDropTarget != NULL) ? S_OK : E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// Cover functions for IOleInPlaceSiteWindowless methods

CWnd* COleControl::SetCapture()
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CWnd* pWndPrev = GetCapture();
		m_pInPlaceSiteWndless->SetCapture(TRUE);
		return pWndPrev;
	}
	else
	{
		return CWnd::SetCapture();
	}
}

BOOL COleControl::ReleaseCapture()
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
		return (m_pInPlaceSiteWndless->SetCapture(FALSE) == S_OK);
	else
		return ::ReleaseCapture();
}

CWnd* COleControl::GetCapture()
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		return (m_pInPlaceSiteWndless->GetCapture() == S_OK) ?
			this : NULL;
	}
	else
	{
		return CWnd::GetCapture();
	}
}

CWnd* COleControl::SetFocus()
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CWnd* pWndPrev = GetFocus();
		m_pInPlaceSiteWndless->SetFocus(TRUE);
		return pWndPrev;
	}
	else
	{
		return CWnd::SetFocus();
	}
}

CWnd* COleControl::GetFocus()
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		return (m_pInPlaceSiteWndless->GetFocus() == S_OK) ?
			this : NULL;
	}
	else
	{
		return CWnd::GetFocus();
	}
}

CDC* COleControl::GetDC(LPCRECT lprcRect, DWORD dwFlags)
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CPoint point(0, 0);
		ClientToParent(m_rcPos, &point);
		CRect rect;
		if (lprcRect != NULL)
		{
			rect.CopyRect(lprcRect);
			rect.OffsetRect(point);
			lprcRect = &rect;
		}

		HDC hDC;
		if (FAILED(m_pInPlaceSiteWndless->GetDC(lprcRect, dwFlags, &hDC)))
			return NULL;

		CDC* pDC = NULL;
		TRY
		{
			pDC = new CWindowlessDC(hDC, point);
		}
		CATCH_ALL(e)
		{
			m_pInPlaceSiteWndless->ReleaseDC(hDC);
		}
		END_CATCH_ALL

		return pDC;
	}
	else
	{
		// NOTE: can only use non-default values for these parameters when
		// activated windowless.
		ASSERT(lprcRect == NULL);
		ASSERT(dwFlags == OLEDC_PAINTBKGND);
		return CWnd::GetDC();
	}
}

BOOL COleControl::ReleaseDC(CDC* pDC)
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CWindowlessDC* pWindowlessDC = DYNAMIC_DOWNCAST(CWindowlessDC, pDC);
		ASSERT(pWindowlessDC != NULL);
		HDC hDC = pWindowlessDC->Detach();
		delete pWindowlessDC;
		return m_pInPlaceSiteWndless->ReleaseDC(hDC);
	}
	else
	{
		return CWnd::ReleaseDC(pDC);
	}
}

void COleControl::InvalidateRgn(CRgn* pRgn, BOOL bErase)
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CRgn rgn;
		if (pRgn != NULL)
		{
			CPoint point(0, 0);
			ClientToParent(m_rcPos, &point);
			rgn.CopyRgn(pRgn);
			rgn.OffsetRgn(point);
		}
		m_pInPlaceSiteWndless->InvalidateRgn(rgn, bErase);
	}
	else
	{
		CWnd::InvalidateRgn(pRgn, bErase);
	}
}

void COleControl::ScrollWindow(int xAmount, int yAmount, LPCRECT lpRect,
	LPCRECT lpClipRect)
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		if (lpRect != NULL || lpClipRect != NULL)
		{
			CPoint point(0, 0);
			ClientToParent(m_rcPos, &point);
			CRect rect;
			CRect rectClip;
			if (lpRect != NULL)
			{
				rect.CopyRect(lpRect);
				rect.OffsetRect(point);
				lpRect = &rect;
			}
			if (lpClipRect != NULL)
			{
				rectClip.CopyRect(lpClipRect);
				rectClip.OffsetRect(point);
				lpClipRect = &rectClip;
			}
		}

		m_pInPlaceSiteWndless->ScrollRect(xAmount, yAmount, lpRect, lpClipRect);
	}
	else
	{
		CWnd::ScrollWindow(xAmount, yAmount, lpRect, lpClipRect);
	}
}

BOOL COleControl::ClipCaretRect(LPRECT lpRect)
{
	BOOL bNotEmpty = FALSE;

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		CPoint point(0, 0);
		ClientToParent(m_rcPos, &point);
		CRect rect(lpRect);
		rect.OffsetRect(point);
		bNotEmpty = (m_pInPlaceSiteWndless->AdjustRect(rect) == S_OK);
		rect.OffsetRect(-point.x, -point.y);
	}

	return bNotEmpty;
}

/////////////////////////////////////////////////////////////////////////////
// Helper functions for windowless controls

void COleControl::GetClientRect(LPRECT lpRect) const
{
	ASSERT((m_hWnd != NULL) || (m_bInPlaceSiteWndless && m_bInPlaceActive));

	if (m_bInPlaceSiteWndless && m_bInPlaceActive)
	{
		long dxOffset;
		long dyOffset;
		GetClientOffset(&dxOffset, &dyOffset);
		::CopyRect(lpRect, m_rcPos);
		::InflateRect(lpRect, -dxOffset, -dyOffset);

#ifdef _DEBUG
		CPoint point(0, 0);
		ClientToParent(m_rcPos, &point);
		ASSERT(point.x == lpRect->left && point.y == lpRect->top);
#endif

		::OffsetRect(lpRect, -lpRect->left, -lpRect->top);
	}
	else if (m_hWnd != NULL)
	{
		CWnd::GetClientRect(lpRect);
	}
	else
	{
		::SetRectEmpty(lpRect);
	}
}
