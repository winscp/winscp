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

struct _AFXCTL_UIACTIVE_INFO
{
	OLEMENUGROUPWIDTHS m_menuWidths;
	HMENU m_hSharedMenu;
	HOLEMENU m_hOleMenu;

	_AFXCTL_UIACTIVE_INFO(HMENU hInPlaceMenu, LPOLEINPLACEFRAME pInPlaceFrame);
	~_AFXCTL_UIACTIVE_INFO();
};

_AFXCTL_UIACTIVE_INFO::_AFXCTL_UIACTIVE_INFO(HMENU hInPlaceMenu,
	LPOLEINPLACEFRAME pInPlaceFrame)
{
	memset(&m_menuWidths, 0, sizeof m_menuWidths);
	m_hSharedMenu = NULL;
	m_hOleMenu = NULL;

	if (hInPlaceMenu != NULL)
	{
		// Create shared menu
		if ((m_hSharedMenu = ::CreateMenu()) == NULL)
			return;

		// Start out by getting menu from container
		if (pInPlaceFrame->InsertMenus(m_hSharedMenu, &m_menuWidths) != S_OK)
		{
			::DestroyMenu(m_hSharedMenu);
			m_hSharedMenu = NULL;
		}
		else
		{
			// Container shouldn't touch these
			ASSERT(m_menuWidths.width[1] == 0);
			ASSERT(m_menuWidths.width[3] == 0);
			ASSERT(m_menuWidths.width[5] == 0);

			// Only copy the popups if there is a menu loaded
			if (hInPlaceMenu != NULL)
			{
				// Insert our menu popups amongst the container menus
				AfxMergeMenus(m_hSharedMenu, hInPlaceMenu,
					&m_menuWidths.width[0], 1);
			}
		}
	}

	// Finally create the special OLE menu descriptor
	m_hOleMenu = ::OleCreateMenuDescriptor(m_hSharedMenu, &m_menuWidths);
}

_AFXCTL_UIACTIVE_INFO::~_AFXCTL_UIACTIVE_INFO()
{
	if (m_hSharedMenu != NULL)
		::DestroyMenu(m_hSharedMenu);

	if (m_hOleMenu != NULL)
		VERIFY(::OleDestroyMenuDescriptor(m_hOleMenu) == S_OK);
}

short AFXAPI _AfxShiftState();

void AFXAPI _GetClippingCoordinates(LPCRECT pPosRect, LPCRECT pClipRect,
	LPRECT pIntersectRect, LPPOINT pOffsetPoint)
{
	int clipLeft = 0;
	int clipTop = 0;

	if ((pClipRect == NULL) || IsRectEmpty(pClipRect))
	{
		CopyRect(pIntersectRect, pPosRect);
	}
	else
	{
		IntersectRect(pIntersectRect, pPosRect, pClipRect);
		clipLeft = pClipRect->left;
		clipTop = pClipRect->top;
	}

	pOffsetPoint->x = min(0, pPosRect->left - clipLeft);
	pOffsetPoint->y = min(0, pPosRect->top - clipTop);
}

HRESULT COleControl::OnActivateInPlace(BOOL bUIActivate, LPMSG pMsg)
{
#ifdef _AFXDLL
	if (m_bOpen)
	{
		m_pWndOpenFrame->SetActiveWindow();
		SendAdvise(OBJECTCODE_SHOWWINDOW);
		return S_OK;
	}
#endif

	// Initialize pointer to in-place site, if necessary.
	if (m_pInPlaceSite == NULL)
	{
		if (m_pClientSite == NULL)
			return E_UNEXPECTED;

		if ((GetControlFlags() & windowlessActivate) &&
			SUCCEEDED(m_pClientSite->QueryInterface(IID_IOleInPlaceSiteWindowless,
			reinterpret_cast<void**>(&m_pInPlaceSiteWndless))))
		{
			m_bInPlaceSiteWndless = m_bInPlaceSiteEx = TRUE;
		}
		else if ((GetControlFlags() & noFlickerActivate) &&
			SUCCEEDED(m_pClientSite->QueryInterface(IID_IOleInPlaceSiteEx,
			reinterpret_cast<void**>(&m_pInPlaceSiteEx))))
		{
			m_bInPlaceSiteEx = TRUE;
		}
		else if (SUCCEEDED(m_pClientSite->QueryInterface(IID_IOleInPlaceSite,
			reinterpret_cast<void**>(&m_pInPlaceSite))))
		{
			m_bInPlaceSiteEx = FALSE;
		}
		else
		{
			m_pInPlaceSite = NULL;
			return E_FAIL;
		}
	}

	ASSERT(m_pInPlaceSite != NULL);

	if ((m_bInPlaceActive && !bUIActivate) || m_bUIActive)
	{
		CWnd* pWndOuter = GetOuterWindow();
		HWND hwndParent;
		if ((pWndOuter != NULL) &&
			SUCCEEDED(m_pInPlaceSite->GetWindow(&hwndParent)) &&
			(hwndParent == ::GetParent(pWndOuter->m_hWnd)))
		{
			::SetWindowPos(pWndOuter->m_hWnd, NULL, 0, 0, 0, 0,
				SWP_NOZORDER|SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|
				SWP_SHOWWINDOW);
			OnSetObjectRects(m_rcPos, NULL);
			return S_OK;
		}
	}

	// Check if container allows windowless activation.
	if (m_bInPlaceSiteWndless)
	{
		if (m_pInPlaceSiteWndless->CanWindowlessActivate() != S_OK)
			m_bInPlaceSiteWndless = FALSE;
	}

	HRESULT hr = E_FAIL;
	if (m_pInPlaceSite != NULL)
		hr = m_pInPlaceSite->CanInPlaceActivate();

	if (hr != NOERROR)
	{
		// Site doesn't allow in-place activation.
		return OnOpen(FALSE, pMsg);
	}

	if (!m_bInPlaceActive)
	{
		if (m_bInPlaceSiteEx)
		{
			// flicker-free and/or windowless activation
			BOOL bNoRedraw;
			m_pInPlaceSiteEx->OnInPlaceActivateEx(&bNoRedraw,
				m_bInPlaceSiteWndless ? ACTIVATE_WINDOWLESS : 0);
			if (GetControlFlags() & noFlickerActivate)
				m_bNoRedraw = bNoRedraw;
		}
		else
		{
			// old-style activation
			m_pInPlaceSite->OnInPlaceActivate();
		}
	}

	HWND hwndParent = NULL;

	if (SUCCEEDED(m_pInPlaceSite->GetWindow(&hwndParent)))
	{
		CRect rcClip;
		m_frameInfo.cb = sizeof(OLEINPLACEFRAMEINFO);

		RELEASE(m_pInPlaceFrame);
		RELEASE(m_pInPlaceDoc);

		if (SUCCEEDED(m_pInPlaceSite->GetWindowContext(
						&m_pInPlaceFrame, &m_pInPlaceDoc,
						&m_rcPos, &rcClip, &m_frameInfo)))
		{
			ASSERT(m_pInPlaceFrame != NULL);

			CRect rectClip;
			if (!m_bInPlaceSiteWndless)
			{
				_GetClippingCoordinates(&m_rcPos, &rcClip, rectClip,
					&m_ptOffset);
				m_bInPlaceActive = CreateControlWindow(hwndParent, m_rcPos,
					rectClip);
			}
			else
			{
				m_bInPlaceActive = TRUE;
			}

			if (m_bInPlaceActive)
			{
				if (bUIActivate)
				{
					if (m_bInPlaceSiteEx)
					{
						if (m_pInPlaceSiteEx->RequestUIActivate() != S_OK)
							m_pInPlaceSite->OnUIDeactivate(FALSE);
					}

					BuildSharedMenu();

					m_bUIActive = TRUE;
					m_pInPlaceSite->OnUIActivate();

					m_pInPlaceFrame->SetActiveObject(
						&m_xOleInPlaceActiveObject, NULL);
					if (m_pInPlaceDoc != NULL)
						m_pInPlaceDoc->SetActiveObject(
							&m_xOleInPlaceActiveObject, NULL);

					if (m_hWnd != NULL)
					{
						BOOL bHandles = AmbientShowGrabHandles();
						BOOL bHatching = AmbientShowHatching();

						if (bHandles || bHatching)
							CreateTracker(bHandles, bHatching, rcClip);
					}

					AddFrameLevelUI();

					if (bUIActivate != -1 &&
						(m_hWnd != NULL) && !IsChild(GetFocus()))
					{
						SetFocus();
					}
				}

				// Pass thru the window message that caused us to be activated
				if ((m_hWnd != NULL || m_bInPlaceSiteWndless) && (pMsg != NULL))
					ForwardActivationMsg(pMsg);

				// Send appropriate notifications...
				SendAdvise(OBJECTCODE_SHOWOBJECT);

				return S_OK;
			}
		}
	}

	RELEASE(m_pInPlaceFrame);
	RELEASE(m_pInPlaceDoc);

	return E_FAIL;
}

void COleControl::ForwardActivationMsg(LPMSG pMsg)
{
	UINT uMsg = pMsg->message;
	LPARAM lParam = pMsg->lParam;

	if (m_bInPlaceSiteWndless)
	{
		// For mouse messages, convert to container window's coordinates
		if ((uMsg >= WM_MOUSEFIRST) && (uMsg <= WM_MOUSELAST))
		{
			POINT ptNew = pMsg->pt;
			HWND hWndContainer = NULL;
			if (SUCCEEDED(m_pInPlaceSite->GetWindow(&hWndContainer)))
			{
				::ScreenToClient(hWndContainer, &ptNew);
				lParam = MAKELONG((short)ptNew.x, (short)ptNew.y);
			}
		}

		// Process message on behalf of windowless control
		OnWindowlessMessage(uMsg, pMsg->wParam, lParam, NULL);
	}
	else
	{
		// For mouse messages, convert to this window's coordinates
		if ((uMsg >= WM_MOUSEFIRST) && (uMsg <= WM_MOUSELAST))
		{
			POINT ptNew = pMsg->pt;
			ScreenToClient(&ptNew);
			lParam = MAKELONG((short)ptNew.x, (short)ptNew.y);
		}

		// Pass the message along to the control's window
		SendMessage(uMsg, pMsg->wParam, lParam);
	}
}

HMENU COleControl::OnGetInPlaceMenu()
{
	// Default: no in-place menu
	return NULL;
}

BOOL COleControl::BuildSharedMenu()
{
	// This can be called more than once on mouse clicks
	if (m_pUIActiveInfo != NULL)
	{
		ASSERT(m_pUIActiveInfo->m_hSharedMenu != NULL);
		return TRUE;
	}

	HMENU hMenu = m_bUIDead ? NULL : OnGetInPlaceMenu();
	TRY
	{
		m_pUIActiveInfo = new _AFXCTL_UIACTIVE_INFO(hMenu, m_pInPlaceFrame);
	}
	END_TRY

	return (m_pUIActiveInfo != NULL) && (m_pUIActiveInfo->m_hOleMenu != NULL);
}

void COleControl::DestroySharedMenu()
{
	ASSERT(m_pUIActiveInfo != NULL);
	if (m_pUIActiveInfo == NULL)
		return;

	HMENU hInPlaceMenu = NULL;

	if ((m_pUIActiveInfo->m_hSharedMenu != NULL) &&
		((hInPlaceMenu = OnGetInPlaceMenu()) != NULL))
	{
		// remove our menu popups from the shared menu
		AfxUnmergeMenus(m_pUIActiveInfo->m_hSharedMenu, hInPlaceMenu);

		// allow container to remove its items from the menu
		ASSERT(m_pInPlaceFrame != NULL);
		VERIFY(m_pInPlaceFrame->RemoveMenus(m_pUIActiveInfo->m_hSharedMenu) == S_OK);
	}

	delete m_pUIActiveInfo;
	m_pUIActiveInfo = NULL;
}

void COleControl::AddFrameLevelUI()
{
	ASSERT(m_pUIActiveInfo != NULL);
	if (m_pUIActiveInfo == NULL)
		return;

	m_pInPlaceFrame->SetMenu(m_pUIActiveInfo->m_hSharedMenu,
		m_pUIActiveInfo->m_hOleMenu, m_hWnd);
	OnShowToolBars();
}

void COleControl::RemoveFrameLevelUI()
{
	ASSERT(m_pUIActiveInfo != NULL);
	if (m_pUIActiveInfo == NULL)
		return;

	OnHideToolBars();

	// allow container to remove its items from the menu
	ASSERT(m_pInPlaceFrame != NULL);
	if (m_pUIActiveInfo->m_hSharedMenu != NULL)
		VERIFY(m_pInPlaceFrame->RemoveMenus(m_pUIActiveInfo->m_hSharedMenu) == S_OK);
}

void COleControl::OnShowToolBars()
{
	// Default sets border space to empty.
	// When overriding, don't call this implementation.

	m_pInPlaceFrame->SetBorderSpace(NULL);
}

void COleControl::OnHideToolBars()
{
	// Default does nothing
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XOleInPlaceObject

STDMETHODIMP_(ULONG) COleControl::XOleInPlaceObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XOleInPlaceObject::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XOleInPlaceObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XOleInPlaceObject::GetWindow(HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceObject)
	*lphwnd = pThis->m_bInPlaceActive ? pThis->GetOuterWindow()->m_hWnd : NULL;
	return (*lphwnd != NULL) ? S_OK : E_FAIL;
}

STDMETHODIMP COleControl::XOleInPlaceObject::ContextSensitiveHelp(BOOL)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleInPlaceObject::InPlaceDeactivate()
{
	METHOD_PROLOGUE_EX(COleControl, OleInPlaceObject)

	if (!pThis->m_bInPlaceActive)
		return S_OK;

	pThis->m_bInPlaceActive = FALSE;

	if (pThis->m_bUIActive)
		UIDeactivate();

	// hide the window

	if (pThis->m_bInPlaceSiteEx &&
		(pThis->GetControlFlags() & noFlickerActivate))
	{
		// flicker-free deactivation
		if (! pThis->m_bInPlaceSiteWndless)
		{
			pThis->UpdateWindow();
			pThis->OnHide();
		}
		pThis->m_pInPlaceSiteEx->OnInPlaceDeactivateEx(TRUE);
	}
	else
	{
		// old-style deactivation
		pThis->OnHide();
		pThis->m_pInPlaceSite->OnInPlaceDeactivate();
	}

	return S_OK;
}

STDMETHODIMP COleControl::XOleInPlaceObject::UIDeactivate()
{
	METHOD_PROLOGUE_EX(COleControl, OleInPlaceObject)

	pThis->m_bPendingUIActivation = FALSE;
	if (!pThis->m_bUIActive)
		return S_OK;

	pThis->DestroyTracker();
	pThis->m_bUIActive = FALSE;

	if (pThis->m_pInPlaceDoc != NULL)
		pThis->m_pInPlaceDoc->SetActiveObject(NULL, NULL);
	pThis->m_pInPlaceFrame->SetActiveObject(NULL, NULL);
	pThis->RemoveFrameLevelUI();
	pThis->DestroySharedMenu();

	pThis->m_pInPlaceSite->OnUIDeactivate(FALSE);

	return S_OK;
}

STDMETHODIMP COleControl::XOleInPlaceObject::SetObjectRects(LPCRECT lprcPosRect,
					LPCRECT lprcClipRect)
{
	METHOD_PROLOGUE_EX(COleControl, OleInPlaceObject)

	return pThis->OnSetObjectRects(lprcPosRect, lprcClipRect) ?
			S_OK :
			E_FAIL;
}

BOOL COleControl::OnSetObjectRects(LPCRECT lprcPosRect, LPCRECT lprcClipRect)
{
	ASSERT(lprcPosRect != NULL);

	// Remember the position rectangle for later
	m_rcPos = *lprcPosRect;

	// Calculate complete rectangle including the tracker (if present)
	CRect rectPos = m_rcPos;
	if (m_bUIActive && m_pRectTracker != NULL)
	{
		// Save new clipping rectangle (for DestroyTracker)
		if (lprcClipRect != NULL)
			m_pRectTracker->m_rectClip = *lprcClipRect;

		// Adjust tracker rectangle to new dimensions
		CRect rectTmp = rectPos;
		rectTmp.OffsetRect(-rectTmp.left, -rectTmp.top);
		m_pRectTracker->m_rect = rectTmp;

		// Adjust the "true" rectangle to include handles/hatching
		UINT nHandleSize = m_pRectTracker->m_nHandleSize - 1;
		rectPos.InflateRect(nHandleSize, nHandleSize);
	}

	// Now clip that rectangle as appropriate
	CRect rectClip;
	_GetClippingCoordinates(rectPos, lprcClipRect, rectClip, &m_ptOffset);

	// Move outer window first. then inner window

	if (!m_bInPlaceSiteWndless)
	{
		CWnd* pWndOuter = GetOuterWindow();
		pWndOuter->MoveWindow(rectClip);
		if (pWndOuter != this)
			MoveWindow(m_ptOffset.x, m_ptOffset.y, rectPos.Width(), rectPos.Height());
	}

	return TRUE;
}

STDMETHODIMP COleControl::XOleInPlaceObject::ReactivateAndUndo()
{
	return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XOleInPlaceActiveObject

STDMETHODIMP_(ULONG) COleControl::XOleInPlaceActiveObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceActiveObject)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XOleInPlaceActiveObject::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceActiveObject)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceActiveObject)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::GetWindow(HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleControl, OleInPlaceActiveObject)
	return pThis->m_xOleInPlaceObject.GetWindow(lphwnd);
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::ContextSensitiveHelp(BOOL)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::TranslateAccelerator(
	LPMSG lpmsg)
{
	METHOD_PROLOGUE_EX(COleControl, OleInPlaceActiveObject)

	// Give the control the first chance.
	if (pThis->PreTranslateMessage(lpmsg))
		return S_OK;

	// Give the site a chance.
	HRESULT hr = S_FALSE;
	if (pThis->m_pControlSite != NULL)
		hr = pThis->m_pControlSite->TranslateAccelerator(lpmsg,
			(DWORD)_AfxShiftState());

	return hr;
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::OnFrameWindowActivate(BOOL)
{
	return S_OK;
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::OnDocWindowActivate(
	BOOL fActivate)
{
	METHOD_PROLOGUE_EX(COleControl, OleInPlaceActiveObject)

	if (fActivate && pThis->m_bUIActive)
		pThis->AddFrameLevelUI();
	else
		pThis->OnHideToolBars();

	return S_OK;
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::ResizeBorder(
	LPCRECT, LPOLEINPLACEUIWINDOW, BOOL)
{
	return S_OK;
}

STDMETHODIMP COleControl::XOleInPlaceActiveObject::EnableModeless(BOOL)
{
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
