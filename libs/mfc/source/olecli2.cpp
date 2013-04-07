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

#ifdef AFX_OLE2_SEG
#pragma code_seg(AFX_OLE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook Construction & Destruction

COleFrameHook::COleFrameHook(CFrameWnd* pFrameWnd, COleClientItem* pItem)
{
	ASSERT_VALID(pItem);
	ASSERT_VALID(pFrameWnd);

	m_lpActiveObject = NULL;
	m_pActiveItem = pItem;
	m_pFrameWnd = pFrameWnd;
	m_hWnd = pFrameWnd->m_hWnd;
	m_bToolBarHidden = FALSE;
	m_hAccelTable = NULL;
	m_bInModalState = FALSE;
	m_nModelessCount = 0;
	pFrameWnd->m_pNotifyHook = this;    // assume start out hooked

	ASSERT_VALID(this);
}

COleFrameHook::~COleFrameHook()
{
	if (m_pFrameWnd != NULL)
	{
		ASSERT_VALID(m_pFrameWnd);
		if (m_pFrameWnd->m_pNotifyHook == this)
			m_pFrameWnd->m_pNotifyHook = NULL;
	}

	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook overrides

void COleFrameHook::OnRecalcLayout()
{
	ASSERT_VALID(this);

	if (m_lpActiveObject == NULL)
		return;

	// get current border size (without current server control bars)
	RECT rectBorder;
	m_pFrameWnd->NegotiateBorderSpace(CFrameWnd::borderGet, &rectBorder);

	// allow server to resize/move its control bars
	m_lpActiveObject->ResizeBorder(&rectBorder, &m_xOleInPlaceFrame,
		m_pActiveItem->m_pInPlaceFrame == this);
}

BOOL COleFrameHook::OnDocActivate(BOOL bActive)
{
	ASSERT_VALID(this);

	if (m_lpActiveObject == NULL)
		return TRUE;

	// allow server to do document activation related actions
	m_lpActiveObject->OnDocWindowActivate(bActive);

	// make sure window caption gets updated later
	COleFrameHook* pNotifyHook = m_pActiveItem->m_pInPlaceFrame;
	pNotifyHook->m_pFrameWnd->DelayUpdateFrameTitle();

	if (!bActive)
	{
		// clear border space
		pNotifyHook->m_xOleInPlaceFrame.SetBorderSpace(NULL);
		if (m_pActiveItem->m_pInPlaceDoc != NULL)
			m_pActiveItem->m_pInPlaceDoc->m_xOleInPlaceFrame.SetBorderSpace(NULL);

		// remove the menu hook when the doc is not active
		pNotifyHook->m_xOleInPlaceFrame.SetMenu(NULL, NULL, NULL);

		// unhook top-level frame if not needed
		if (pNotifyHook != this)
		{
			// shouldn't be removing some other hook
			ASSERT(pNotifyHook->m_pFrameWnd->m_pNotifyHook == pNotifyHook);
			pNotifyHook->m_pFrameWnd->m_pNotifyHook = NULL;
		}
	}
	else
	{
		// rehook top-level frame if necessary (no effect if top-level == doc-level)
		pNotifyHook->m_pFrameWnd->m_pNotifyHook = pNotifyHook;
	}

	// don't do default if activating
	return bActive;
}

BOOL COleFrameHook::OnContextHelp(BOOL bEnter)
{
	ASSERT_VALID(this);
	if (m_lpActiveObject == NULL || m_pActiveItem->m_pInPlaceFrame != this)
		return TRUE;

	// allow all servers to enter/exit context sensitive help mode
	return NotifyAllInPlace(bEnter, &COleFrameHook::DoContextSensitiveHelp);
}

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook callbacks for the top-level frame

BOOL COleFrameHook::OnMenuSelect(UINT nItemID, UINT nFlags, HMENU hSysMenu)
{
	UNUSED_ALWAYS(nFlags);
	UNUSED_ALWAYS(nItemID);

	// if we're over a docobject item, we need to reflect messages

	COleDocObjectItem* pActiveDocObjectItem = DYNAMIC_DOWNCAST(COleDocObjectItem, m_pActiveItem);
	if (pActiveDocObjectItem != NULL)
	{
		CWnd* pWnd = pActiveDocObjectItem->GetInPlaceWindow();

		// if we're popping up a menu, figure out what menu is
		// apparing; if it's in the help menu and it's not the
		// first element, it's the object's menu.

		if (nFlags & MF_POPUP)
		{
			if (pActiveDocObjectItem->m_pHelpPopupMenu->GetSafeHmenu() ==
					hSysMenu)
			{
				pActiveDocObjectItem->m_bInHelpMenu = (nItemID != 0);

				if (pActiveDocObjectItem->m_bInHelpMenu && pWnd != NULL)
				{
					pWnd->SendMessage(WM_MENUSELECT,
						MAKEWPARAM(nItemID, nFlags), (LPARAM) hSysMenu);
					return TRUE;
				}
			}
		}
		else
		{
			if (pActiveDocObjectItem->m_bInHelpMenu && pWnd != NULL)
			{
				pWnd->SendMessage(WM_MENUSELECT,
					MAKEWPARAM(nItemID, nFlags), (LPARAM) hSysMenu);
				return TRUE;
			}
		}
	}

	return FALSE;
}

void COleFrameHook::OnInitMenu(CMenu* pMenu)
{
	UNUSED_ALWAYS(pMenu);

	// reset the help menu flag when a new menu is opening

	COleDocObjectItem* pActiveDocObjectItem = DYNAMIC_DOWNCAST(COleDocObjectItem, m_pActiveItem);
	if (pActiveDocObjectItem != NULL)
		pActiveDocObjectItem->m_bInHelpMenu = FALSE;

	return;
}

BOOL COleFrameHook::OnInitMenuPopup(CMenu* pMenu, int nIndex, BOOL bSysMenu)
{
	UNUSED_ALWAYS(nIndex);

	if (bSysMenu)
		return FALSE;

	COleDocObjectItem* pActiveDocObjectItem = DYNAMIC_DOWNCAST(COleDocObjectItem, m_pActiveItem);
	if (pActiveDocObjectItem == NULL)
		return FALSE;

	// if we're popping up a new menu, for the object,
	// reflect the message and don't let MFC handle it
	// with ON_COMMAND_UI stuff

	if (pActiveDocObjectItem->m_bInHelpMenu)
	{
		CWnd* pWnd = pActiveDocObjectItem->GetInPlaceWindow();
		if (pWnd != NULL)
		{
			pWnd->SendMessage(WM_INITMENUPOPUP, (WPARAM) pMenu->m_hMenu,
				MAKELPARAM(nIndex, bSysMenu));
			return TRUE;
		}
	}

	return FALSE;
}

BOOL COleFrameHook::OnPreTranslateMessage(MSG* pMsg)
{
	ASSERT_VALID(this);

	if (m_lpActiveObject == NULL || m_pActiveItem->m_pInPlaceFrame != this)
		return FALSE;

	// allow server to translate accelerators
	if (pMsg->message >= WM_KEYFIRST && pMsg->message <= WM_KEYLAST)
		return m_lpActiveObject->TranslateAccelerator(pMsg) == S_OK;

	// if we've finally gotten a WM_COMMAND message, make sure
	// that it is appropriately reflected to the docobject

	if (pMsg->message == WM_COMMAND)
	{
		COleDocObjectItem* pActiveDocObjectItem = DYNAMIC_DOWNCAST(COleDocObjectItem, m_pActiveItem);
		if (pActiveDocObjectItem != NULL)
		{
			LRESULT lResult = 0;
			if (pActiveDocObjectItem->m_bInHelpMenu)
			{
				CWnd* pWnd = pActiveDocObjectItem->GetInPlaceWindow();
				if (pWnd != NULL)
					lResult = pWnd->SendMessage(WM_COMMAND, pMsg->wParam, pMsg->lParam);
			}
			return lResult;
		}
	}

	return FALSE;
}

void COleFrameHook::OnActivate(BOOL bActive)
{
	ASSERT_VALID(this);

	if (m_lpActiveObject == NULL || m_pActiveItem->m_pInPlaceFrame != this)
		return;

	if (m_pFrameWnd->IsWindowEnabled())
	{
		// allow active server to do frame level activation
		m_lpActiveObject->OnFrameWindowActivate(bActive);
	}
}

void COleFrameHook::OnEnableModeless(BOOL bEnable)
{
	ASSERT_VALID(this);

	if (m_lpActiveObject == NULL || m_pActiveItem->m_pInPlaceFrame != this)
		return;

	// allow server to disable/enable modeless dialogs
	NotifyAllInPlace(bEnable, &COleFrameHook::DoEnableModeless);
}

BOOL COleFrameHook::OnUpdateFrameTitle()
{
	ASSERT_VALID(this);
	ASSERT_VALID(m_pActiveItem);

	if (m_lpActiveObject == NULL || m_pActiveItem->m_pInPlaceFrame != this)
		return FALSE;

	return m_pActiveItem->OnUpdateFrameTitle();
}

void COleFrameHook::OnPaletteChanged(CWnd* pFocusWnd)
{
	CWnd* pWnd = m_pActiveItem->GetInPlaceWindow();
	if (pWnd != NULL)
		pWnd->SendMessage(WM_PALETTECHANGED, (WPARAM)pFocusWnd->GetSafeHwnd());
}

BOOL COleFrameHook::OnQueryNewPalette()
{
	CWnd* pWnd = m_pActiveItem->GetInPlaceWindow();
	if (pWnd != NULL)
		return pWnd->SendMessage(WM_QUERYNEWPALETTE);
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Helpers for notifications that have to affect all in-place windows

BOOL COleFrameHook::NotifyAllInPlace(
	BOOL bParam, BOOL (COleFrameHook::*pNotifyFunc)(BOOL bParam))
{
	ASSERT_VALID(this);
	HWND hWndFrame = m_hWnd;
	CWinApp* pApp = AfxGetApp();

	// no doc manager - no templates
	if (pApp->m_pDocManager == NULL)
		return TRUE;

	// walk all templates in the application
	CDocTemplate* pTemplate;
	POSITION pos = pApp->m_pDocManager->GetFirstDocTemplatePosition();
	while (pos != NULL)
	{
		pTemplate = pApp->m_pDocManager->GetNextDocTemplate(pos);
		ASSERT_VALID(pTemplate);
		ASSERT_KINDOF(CDocTemplate, pTemplate);

		// walk all documents in the template
		POSITION pos2 = pTemplate->GetFirstDocPosition();
		while (pos2)
		{
			COleDocument* pDoc = (COleDocument*)pTemplate->GetNextDoc(pos2);
			ASSERT_VALID(pDoc);
			if (pDoc->IsKindOf(RUNTIME_CLASS(COleDocument)))
			{
				// walk all COleClientItem objects in the document
				COleClientItem* pItem;
				POSITION pos3 = pDoc->GetStartPosition();
				while ((pItem = pDoc->GetNextClientItem(pos3)) != NULL)
				{
					if (pItem->m_pInPlaceFrame != NULL &&
						pItem->m_pInPlaceFrame->m_lpActiveObject != NULL &&
						pItem->m_pView != NULL &&
						AfxIsDescendant(hWndFrame, pItem->m_pView->m_hWnd))
					{
						// Whew!  Found an in-place active item that is
						//  part of this frame window hierarchy.
						COleFrameHook* pNotifyHook = pItem->m_pInPlaceFrame;
						if (!(pNotifyHook->*pNotifyFunc)(bParam))
							return FALSE;
					}
				}
			}
		}
	}
	return TRUE;
}

BOOL COleFrameHook::DoContextSensitiveHelp(BOOL bEnter)
{
	ASSERT_VALID(this);
	ASSERT(m_lpActiveObject != NULL);

	return !FAILED(m_lpActiveObject->ContextSensitiveHelp(bEnter));
}

BOOL COleFrameHook::DoEnableModeless(BOOL bEnable)
{
	ASSERT_VALID(this);
	ASSERT(m_lpActiveObject != NULL);

	// allow server to enable/disable any modeless windows
	if (!bEnable)
	{
		if (m_nModelessCount++ == 0)
			m_lpActiveObject->EnableModeless(FALSE);
	}
	else
	{
		if (m_nModelessCount != 0 && --m_nModelessCount == 0)
			m_lpActiveObject->EnableModeless(TRUE);
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem - default in-place activation implementation

BOOL COleClientItem::CanActivate()
{
	// don't allow in-place activations with iconic aspect items
	if (m_nDrawAspect == DVASPECT_ICON)
		return FALSE;

	// if no view has been set, attempt to find suitable one.
	//  (necessary to get links to embeddings to work correctly)
	if (m_pView == NULL)
	{
		// only use pActivateView if this item is in same document
		_AFX_OLE_STATE* pOleState = _afxOleState;
		if (pOleState->m_pActivateView != NULL &&
			pOleState->m_pActivateView->GetDocument() != GetDocument())
		{
			pOleState->m_pActivateView = NULL;   // not in same document
		}

		CView* pView = pOleState->m_pActivateView;
		if (pView == NULL)
		{
			// no routing view available - try to use the one with focus
			CWnd* pWnd = CWnd::GetFocus();
			while (pWnd != NULL && !pWnd->IsKindOf(RUNTIME_CLASS(CView)))
				pWnd = pWnd->GetParent();
			pView = STATIC_DOWNCAST(CView, pWnd);

			if (pView == NULL)
			{
				// still no routing view available - just use first one
				COleDocument* pDoc = GetDocument();
				POSITION pos = pDoc->GetFirstViewPosition();
				pView = pDoc->GetNextView(pos);
			}
		}
		m_pView = pView;
	}

	return m_pView->GetSafeHwnd() != NULL;
}

void COleClientItem::OnActivate()
{
	ASSERT_VALID(this);

	// it is necessary to lock the object when it is in-place
	//  (without this, a link to an embedding may disconnect unexpectedly)
	if (!m_bLocked)
	{
		OleLockRunning(m_lpObject, TRUE, FALSE);
		m_bLocked = TRUE;
	}

	// notify the item of the state change
	if (m_nItemState != activeState)
	{
		OnChange(OLE_CHANGED_STATE, (DWORD)activeState);
		m_nItemState = activeState;
	}
}

void COleClientItem::OnActivateUI()
{
	ASSERT_VALID(this);

	// notify the item of the state change
	if (m_nItemState != activeUIState)
	{
		OnChange(OLE_CHANGED_STATE, (DWORD)activeUIState);
		m_nItemState = activeUIState;
	}

	// the container window must have WS_CLIPCHILDREN set
	ASSERT_VALID(m_pView);
	m_dwContainerStyle = m_pView->GetStyle();
	m_pView->ModifyStyle(0, WS_CLIPCHILDREN);

	// cache the server's HWND for later
	LPOLEINPLACEOBJECT lpInPlaceObject =
		QUERYINTERFACE(m_lpObject, IOleInPlaceObject);
	ASSERT(lpInPlaceObject != NULL);

	// get the HWND for the in-place active object
	HWND hWnd;
	if (lpInPlaceObject->GetWindow(&hWnd) != S_OK)
		hWnd = NULL;
	lpInPlaceObject->Release();
	m_hWndServer = hWnd;

	// make sure top-level frame is hooked
	if (m_pInPlaceFrame != NULL)
	{
		ASSERT_VALID(m_pInPlaceFrame->m_pFrameWnd);
		m_pInPlaceFrame->m_pFrameWnd->m_pNotifyHook = m_pInPlaceFrame;
	}
	// make sure doc-level frame is hooked
	if (m_pInPlaceDoc != NULL)
	{
		ASSERT_VALID(m_pInPlaceDoc->m_pFrameWnd);
		m_pInPlaceDoc->m_pFrameWnd->m_pNotifyHook = m_pInPlaceDoc;
	}
}

BOOL COleClientItem::OnShowControlBars(CFrameWnd* pFrameWnd, BOOL bShow)
{
	ASSERT_VALID(pFrameWnd);
	ASSERT_VALID(this);

	// show/hide all bars marked with CBRS_HIDE_INPLACE style
	BOOL bResult = FALSE;
	if (bShow)
	{
		POSITION pos = pFrameWnd->m_listControlBars.GetHeadPosition();
		while (pos)
		{
			CControlBar* pBar =
				(CControlBar*)pFrameWnd->m_listControlBars.GetNext(pos);
			ASSERT_VALID(pBar);
			if ((pBar->GetBarStyle() & CBRS_HIDE_INPLACE) &&
				(pBar->m_nStateFlags & CControlBar::tempHide))
			{
				pBar->m_nStateFlags &= ~CControlBar::tempHide;
				pFrameWnd->ShowControlBar(pBar, TRUE, TRUE);
				bResult = TRUE;
			}
		}
	}
	else
	{
		POSITION pos = pFrameWnd->m_listControlBars.GetHeadPosition();
		while (pos)
		{
			CControlBar* pBar =
				(CControlBar*)pFrameWnd->m_listControlBars.GetNext(pos);
			ASSERT_VALID(pBar);
			if (pBar->IsVisible() && (pBar->GetBarStyle() & CBRS_HIDE_INPLACE))
			{
				pBar->m_nStateFlags |= CControlBar::tempHide;
				pFrameWnd->ShowControlBar(pBar, FALSE, TRUE);
				bResult = TRUE;
			}
		}
	}
	return bResult;
}

BOOL COleClientItem::OnGetWindowContext(
	CFrameWnd** ppMainFrame, CFrameWnd** ppDocFrame,
	LPOLEINPLACEFRAMEINFO pFrameInfo)
{
	ASSERT(AfxIsValidAddress(ppMainFrame, sizeof(CFrameWnd*)));
	ASSERT(AfxIsValidAddress(ppDocFrame, sizeof(CFrameWnd*)));
	ASSERT(pFrameInfo == NULL ||
		AfxIsValidAddress(pFrameInfo, sizeof(OLEINPLACEFRAMEINFO)));
	ASSERT_VALID(this);
	ASSERT_VALID(m_pView);

	// get main window of application
	*ppMainFrame = m_pView->GetTopLevelFrame();
	ASSERT_VALID(*ppMainFrame);
	ASSERT_KINDOF(CFrameWnd, *ppMainFrame);

	// get document window (if there is one)
	CFrameWnd* pDocFrame = m_pView->GetParentFrame();
	if (pDocFrame != *ppMainFrame)
	{
		*ppDocFrame = pDocFrame;
		ASSERT_VALID(*ppDocFrame);
		ASSERT_KINDOF(CFrameWnd, *ppDocFrame);
	}

	if (pFrameInfo != NULL)
	{
		// get accelerator table
		CDocTemplate* pTemplate = GetDocument()->GetDocTemplate();
		HACCEL hAccel = pTemplate != NULL ? pTemplate->m_hAccelInPlace : NULL;
		pFrameInfo->cAccelEntries =
			hAccel != NULL ? CopyAcceleratorTable(hAccel, NULL, 0) : 0;
		pFrameInfo->haccel = pFrameInfo->cAccelEntries != 0 ? hAccel : NULL;
		pFrameInfo->hwndFrame = (*ppMainFrame)->m_hWnd;
		pFrameInfo->fMDIApp = *ppDocFrame != NULL;
	}
	return TRUE;
}

BOOL COleClientItem::OnScrollBy(CSize sizeExtent)
{
	ASSERT_VALID(this);
	ASSERT_VALID(m_pView);

	// scroll through splitter or view
	CSplitterWnd* pSplitter = CView::GetParentSplitter(m_pView, FALSE);
	BOOL bResult;
	if (pSplitter != NULL)
		bResult = pSplitter->DoScrollBy(m_pView, sizeExtent);
	else
		bResult = m_pView->OnScrollBy(sizeExtent);

	return bResult;
}

void COleClientItem::OnDeactivateUI(BOOL /*bUndoable*/)
{
	ASSERT_VALID(this);

	// notify the item of the state change
	if (m_nItemState != activeState)
	{
		OnChange(OLE_CHANGED_STATE, (DWORD)activeState);
		m_nItemState = activeState;
	}

	if (m_pView != NULL && m_pDocument->GetFirstViewPosition())
	{
		// restore container window's WS_CLIPCHILDREN bit...
		ASSERT_VALID(m_pView);
		m_pView->ModifyStyle(WS_CLIPCHILDREN, m_dwContainerStyle & WS_CLIPCHILDREN);
	}

	// restore original user interface on the frame window
	CFrameWnd* pMainFrame;
	CFrameWnd* pDocFrame = NULL;
	if (OnGetWindowContext(&pMainFrame, &pDocFrame, NULL))
	{
		ASSERT_VALID(pMainFrame);
		pMainFrame->DelayUpdateFrameTitle();
		if (pMainFrame->NegotiateBorderSpace(CFrameWnd::borderSet, NULL))
			pMainFrame->DelayRecalcLayout();

		// restore original user interface on the document window
		if (pDocFrame != NULL)
		{
			pDocFrame->DelayUpdateFrameTitle();
			if (pDocFrame->NegotiateBorderSpace(CFrameWnd::borderSet, NULL))
				pDocFrame->DelayRecalcLayout();
		}
	}

	// cleanup frame interfaces allocated in GetWindowContext
	if (m_pInPlaceFrame != NULL)
	{
		OnShowControlBars(m_pInPlaceFrame->m_pFrameWnd, TRUE);

		// release OLE frame window hooks and allow menu update
		::OleSetMenuDescriptor(NULL, m_pInPlaceFrame->m_pFrameWnd->m_hWnd,
			NULL, NULL, NULL);
		if (m_pInPlaceDoc != NULL)
		{
			::OleSetMenuDescriptor(NULL, m_pInPlaceDoc->m_pFrameWnd->m_hWnd,
				NULL, NULL, NULL);
		}
		m_pInPlaceFrame->m_pFrameWnd->DelayUpdateFrameMenu(NULL);

		// unhook from frame window
		if (m_pInPlaceFrame->m_pFrameWnd->m_pNotifyHook == m_pInPlaceFrame)
			m_pInPlaceFrame->m_pFrameWnd->m_pNotifyHook = NULL;

		// cleanup document interfaces allocated in GetWindowContext
		if (m_pInPlaceDoc != NULL)
		{
			OnShowControlBars(m_pInPlaceDoc->m_pFrameWnd, TRUE);

			// unhook from frame window
			if (m_pInPlaceDoc->m_pFrameWnd->m_pNotifyHook == m_pInPlaceDoc)
				m_pInPlaceDoc->m_pFrameWnd->m_pNotifyHook = NULL;
		}
	}

	// reset server HWND -- no longer necessary
	m_hWndServer = NULL;

	CWnd* pWnd = AfxGetMainWnd();
	if (pWnd != NULL)
	{
		// set focus back to the container
		pWnd = pWnd->GetTopLevelParent();
		ASSERT_VALID(pWnd);
		if (::GetActiveWindow() == pWnd->m_hWnd)
			pWnd->SetFocus();
	}
}

void COleClientItem::OnDeactivate()
{
	ASSERT_VALID(this);

	// notify the item of the state change
	if (m_nItemState != loadedState)
	{
		OnChange(OLE_CHANGED_STATE, (DWORD)loadedState);
		m_nItemState = loadedState;
	}

	// cleanup frame interfaces allocated in GetWindowContext
	if (m_pInPlaceFrame != NULL)
	{
		// release in place frame
		if (m_pInPlaceFrame->m_pFrameWnd->m_pNotifyHook == m_pInPlaceFrame)
			m_pInPlaceFrame->m_pFrameWnd->m_pNotifyHook =  NULL;
		m_pInPlaceFrame->InternalRelease();
		m_pInPlaceFrame = NULL;

		// cleanup document interfaces allocated in GetWindowContext
		if (m_pInPlaceDoc != NULL)
		{
			// release in place document
			if (m_pInPlaceDoc->m_pFrameWnd->m_pNotifyHook == m_pInPlaceDoc)
				m_pInPlaceDoc->m_pFrameWnd->m_pNotifyHook = NULL;
			m_pInPlaceDoc->InternalRelease();
			m_pInPlaceDoc = NULL;
		}
	}

	// both frame-level and doc-level interfaces should be cleaned up
	ASSERT(m_pInPlaceFrame == NULL);
	ASSERT(m_pInPlaceDoc == NULL);

	// no longer need the container window
	m_pView = NULL;
}

void COleClientItem::OnDiscardUndoState()
{
	ASSERT_VALID(this);

	// default does nothing
}

void COleClientItem::OnDeactivateAndUndo()
{
	ASSERT_VALID(this);

	DeactivateUI(); // default is to UI deactivate
}

BOOL COleClientItem::OnChangeItemPosition(const CRect& rectPos)
{
	if (!IsInPlaceActive())
		return FALSE;

	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(&rectPos, sizeof(CRect), FALSE));
	ASSERT_VALID(m_pView);

	// determine the visible rect based on intersection between client rect
	CRect clipRect;
	OnGetClipRect(clipRect);
	CRect visRect;
	visRect.IntersectRect(clipRect, rectPos);

	// advise the server of the new visible rectangle
	if (!visRect.IsRectEmpty())
		return SetItemRects(&rectPos, &clipRect);

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// IOleInPlaceFrame notifications (default implementation)

void COleClientItem::OnInsertMenus(CMenu* pMenuShared,
	LPOLEMENUGROUPWIDTHS lpMenuWidths)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pMenuShared);
	ASSERT(AfxIsValidAddress(lpMenuWidths, sizeof(OLEMENUGROUPWIDTHS)));

	// initialize the group widths array
	lpMenuWidths->width[0] = 0;
	lpMenuWidths->width[2] = 0;
	lpMenuWidths->width[4] = 0;

	// get menu from document template
	CDocTemplate* pTemplate = GetDocument()->GetDocTemplate();
	HMENU hMenuOLE = pTemplate->m_hMenuInPlace;

	// only copy the popups if there is a menu loaded
	if (hMenuOLE == NULL)
		return;

	// insert our menu items and adjust group widths array
	AfxMergeMenus(pMenuShared->GetSafeHmenu(), hMenuOLE, &lpMenuWidths->width[0], 0);
}

void COleClientItem::OnSetMenu(CMenu* pMenuShared, HOLEMENU holemenu,
	HWND hwndActiveObject)
{
	ASSERT_VALID(this);
	ASSERT(m_pInPlaceFrame != NULL);
	ASSERT(m_pInPlaceFrame->m_pFrameWnd != NULL);

	// don't set the doc is active
	CFrameWnd* pFrameWnd = m_pInPlaceFrame->m_pFrameWnd;
	ASSERT_VALID(pFrameWnd);
	if (m_pInPlaceDoc != NULL &&
		m_pInPlaceDoc->m_pFrameWnd != pFrameWnd->GetActiveFrame())
	{
		return;
	}

	// update the menu
	pFrameWnd->DelayUpdateFrameMenu(pMenuShared->GetSafeHmenu());

	// enable/disable the OLE command routing hook
	::OleSetMenuDescriptor(holemenu, pFrameWnd->m_hWnd,
		hwndActiveObject, NULL, NULL);
	if (m_pInPlaceDoc != NULL)
	{
		pFrameWnd = m_pInPlaceDoc->m_pFrameWnd;
		ASSERT_VALID(pFrameWnd);
		::OleSetMenuDescriptor(holemenu, pFrameWnd->m_hWnd,
			hwndActiveObject, NULL, NULL);
	}
}

void COleClientItem::OnRemoveMenus(CMenu* pMenuShared)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pMenuShared);

	// get menu from document template
	CDocTemplate* pTemplate = GetDocument()->GetDocTemplate();
	HMENU hMenuOLE = pTemplate->m_hMenuInPlace;
	if (hMenuOLE == NULL)
		return;

	// remove any menu popups originally added in OnInsertMenus
	AfxUnmergeMenus(pMenuShared->GetSafeHmenu(), hMenuOLE);
}

BOOL COleClientItem::OnUpdateFrameTitle()
{
	ASSERT_VALID(this);
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// In-place Activation operations

void COleClientItem::Deactivate()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(IsInPlaceActive());

	// get IOleInPlaceObject interface
	LPOLEINPLACEOBJECT lpInPlaceObject =
		QUERYINTERFACE(m_lpObject, IOleInPlaceObject);
	if (lpInPlaceObject == NULL)
	{
		Close();    // handle rare failure cases by calling Close
		return;
	}

	// call IOleInPlaceObject::InPlaceDeactivate
	m_scLast = lpInPlaceObject->InPlaceDeactivate();
	lpInPlaceObject->Release();
	if (FAILED(m_scLast))
	{
		Close();    // handle rare failure cases by calling Close
		return;
	}
	m_nItemState = loadedState; // just in case server has crashed
}

void COleClientItem::DeactivateUI()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(GetItemState() == activeUIState);

	// get IOleInPlaceObject interface
	LPOLEINPLACEOBJECT lpInPlaceObject =
		QUERYINTERFACE(m_lpObject, IOleInPlaceObject);
	if (lpInPlaceObject == NULL)
	{
		Close();    // handle rare failure cases by calling Close
		return;
	}

	// call IOleInPlaceObject::UIDeactivate
	m_scLast = lpInPlaceObject->UIDeactivate();
	lpInPlaceObject->Release();
	if (FAILED(m_scLast))
	{
		Close();    // handle rare failure cases by calling Close
		return;
	}
	if (m_nItemState == activeUIState)
		m_nItemState = activeState; // just in case server has crashed
}

BOOL COleClientItem::SetItemRects(LPCRECT lpPosRect, LPCRECT lpClipRect)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(IsInPlaceActive());
	ASSERT(lpPosRect == NULL ||
		AfxIsValidAddress(lpPosRect, sizeof(RECT), FALSE));
	ASSERT(lpClipRect == NULL ||
		AfxIsValidAddress(lpClipRect, sizeof(RECT), FALSE));

	// get IOleInPlaceObject interface
	LPOLEINPLACEOBJECT lpInPlaceObject =
		QUERYINTERFACE(m_lpObject, IOleInPlaceObject);
	if (lpInPlaceObject == NULL)
		return FALSE;   // perhaps server crashed?

	// use OnGetPosRect if rectangle not specified
	CRect rectPos;
	if (lpPosRect == NULL)
	{
		ASSERT(lpClipRect == NULL);
		OnGetItemPosition(rectPos);
		lpPosRect = &rectPos;
	}

	// use OnGetClipRect if clipping rectangle not specified
	CRect rectClip;
	if (lpClipRect == NULL)
	{
		OnGetClipRect(rectClip);
		lpClipRect = &rectClip;
	}
	ASSERT(lpPosRect != NULL);
	ASSERT(lpClipRect != NULL);

	// notify the server of the new item rectangles
	m_scLast = lpInPlaceObject->SetObjectRects(lpPosRect, lpClipRect);
	lpInPlaceObject->Release();

	// remember position rectangle as cached position
	return !FAILED(m_scLast);
}

BOOL COleClientItem::ReactivateAndUndo()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(IsInPlaceActive());

	// get IOleInPlaceObject interface
	LPOLEINPLACEOBJECT lpInPlaceObject =
		QUERYINTERFACE(m_lpObject, IOleInPlaceObject);
	if (lpInPlaceObject == NULL)
	{
		Close();    // handle rare failure cases by calling Close
		return FALSE;
	}

	// call IOleInPlaceObject::ReactivateAndUndo
	m_scLast = lpInPlaceObject->ReactivateAndUndo();
	lpInPlaceObject->Release();
	if (FAILED(m_scLast))
	{
		Close();    // handle rare failure cases by calling Close
		return FALSE;
	}
	return TRUE;
}

CWnd* COleClientItem::GetInPlaceWindow()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// only inplace active items should be asking for the window handle
	if (GetItemState() != activeUIState)
		return NULL;

	// handle case of server that just disappears
	if (m_hWndServer != NULL && !::IsWindow(m_hWndServer))
	{
		Close();
		return NULL;
	}

	ASSERT(m_hWndServer == NULL || ::IsWindow(m_hWndServer));
	return CWnd::FromHandle(m_hWndServer);
}

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook OLE interface implementation

BEGIN_INTERFACE_MAP(COleFrameHook, CCmdTarget)
	INTERFACE_PART(COleFrameHook, IID_IOleWindow, OleInPlaceFrame)
	INTERFACE_PART(COleFrameHook, IID_IOleInPlaceUIWindow, OleInPlaceFrame)
	INTERFACE_PART(COleFrameHook, IID_IOleInPlaceFrame, OleInPlaceFrame)
#ifndef _AFXDLL
	INTERFACE_PART(COleFrameHook, IID_IOleCommandTarget, OleCommandTarget)
#endif
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook::XOleCommandTarget implementation

#ifdef _AFXDLL
LPUNKNOWN COleFrameHook::GetInterfaceHook(const void* pIID)
{
	ASSERT(m_pModuleState != NULL);
	if (m_pModuleState->m_dwVersion >= 0x423 &&
		*(IID*)pIID == IID_IOleCommandTarget)
	{
		return &m_xOleCommandTarget;
	}
	return NULL;
}
#endif

STDMETHODIMP_(ULONG) COleFrameHook::XOleCommandTarget::AddRef()
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleCommandTarget)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleFrameHook::XOleCommandTarget::Release()
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleCommandTarget)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleFrameHook::XOleCommandTarget::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleCommandTarget)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleFrameHook::XOleCommandTarget::Exec(
   const GUID* pguidCmdGroup, DWORD nCmdID, DWORD nCmdExecOpt,
   VARIANTARG* pvarargIn, VARIANTARG* pvarargOut)
{
	HRESULT hResult = OLECMDERR_E_UNKNOWNGROUP;
	METHOD_PROLOGUE_EX_(COleFrameHook, OleCommandTarget)

	COleDocObjectItem* pActiveDocObjectItem =
		DYNAMIC_DOWNCAST(COleDocObjectItem, pThis->m_pActiveItem);
	if (pActiveDocObjectItem != NULL)
	{
		hResult = _AfxExecOleCommandHelper(pActiveDocObjectItem,
			pguidCmdGroup, nCmdID, nCmdExecOpt, pvarargIn, pvarargOut);
	}

	return hResult;
}

STDMETHODIMP COleFrameHook::XOleCommandTarget::QueryStatus(
   const GUID* pguidCmdGroup, ULONG cCmds, OLECMD rgCmds[],
   OLECMDTEXT* pcmdtext)
{
	HRESULT hResult = OLECMDERR_E_UNKNOWNGROUP;
	METHOD_PROLOGUE_EX_(COleFrameHook, OleCommandTarget)

	COleDocObjectItem* pActiveDocObjectItem =
		DYNAMIC_DOWNCAST(COleDocObjectItem, pThis->m_pActiveItem);
	if (pActiveDocObjectItem != NULL)
	{
		hResult = _AfxQueryStatusOleCommandHelper(pActiveDocObjectItem,
		   pguidCmdGroup, cCmds, rgCmds, pcmdtext);
	}

	return hResult;
}

/////////////////////////////////////////////////////////////////////////////
// COleFrameHook::XOleInPlaceFrame implementation

STDMETHODIMP_(ULONG) COleFrameHook::XOleInPlaceFrame::AddRef()
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleInPlaceFrame)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleFrameHook::XOleInPlaceFrame::Release()
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleInPlaceFrame)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleInPlaceFrame)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::GetWindow(
	HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleInPlaceFrame)

	*lphwnd = pThis->m_hWnd;
	return *lphwnd != NULL ? S_OK : E_FAIL;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::ContextSensitiveHelp(
	BOOL fEnterMode)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	// document frame windows should not be put in help mode, so we get the
	//  top-level frame window and check it first
	CFrameWnd* pFrameWnd = pThis->m_pFrameWnd->GetTopLevelFrame();
	ASSERT_VALID(pFrameWnd);

	if (fEnterMode)
	{
		if (!pFrameWnd->m_bHelpMode)
		{
			// check if help mode probable
			if (!pFrameWnd->CanEnterHelpMode())
				return E_UNEXPECTED;

			// attempt to enter context help
			if (!pThis->OnContextHelp(TRUE) ||
				!pFrameWnd->PostMessage(WM_COMMAND, ID_CONTEXT_HELP))
			{
				return E_UNEXPECTED;
			}
		}
	}
	else
	{
		// just exit help mode
		pFrameWnd->ExitHelpMode();
	}

	return S_OK;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::GetBorder(LPRECT lpRectBorder)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	COleClientItem* pItem = pThis->m_pActiveItem;
	ASSERT_VALID(pItem);
	CFrameWnd* pFrameWnd = pThis->m_pFrameWnd;
	ASSERT_VALID(pFrameWnd);

	// hide the control bars temporarily
	BOOL bHidden = pItem->OnShowControlBars(pFrameWnd, FALSE);

	// determine border space assuming that we'll remove our control bars
	CRect rectSave = pFrameWnd->m_rectBorder;
	pFrameWnd->NegotiateBorderSpace(CFrameWnd::borderSet, NULL);
	pFrameWnd->NegotiateBorderSpace(CFrameWnd::borderGet, lpRectBorder);
	pFrameWnd->NegotiateBorderSpace(CFrameWnd::borderSet, &rectSave);

	// restore control bars
	if (bHidden)
		pItem->OnShowControlBars(pFrameWnd, TRUE);

	return S_OK;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::RequestBorderSpace(
	LPCRECT lpRectWidths)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	CFrameWnd* pFrameWnd = pThis->m_pFrameWnd;
	ASSERT_VALID(pFrameWnd);

	if (!pFrameWnd->NegotiateBorderSpace(
		CFrameWnd::borderRequest, (LPRECT)lpRectWidths))
	{
		return INPLACE_E_NOTOOLSPACE;
	}

	return S_OK;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::SetBorderSpace(
	LPCRECT lpRectWidths)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	CFrameWnd* pFrameWnd = pThis->m_pFrameWnd;

	if (pFrameWnd->NegotiateBorderSpace(
		CFrameWnd::borderSet, (LPRECT)lpRectWidths))
	{
		pFrameWnd->DelayRecalcLayout(FALSE);
		pFrameWnd->PostMessage(WM_KICKIDLE);
	}
	pThis->m_pActiveItem->OnShowControlBars(pFrameWnd, lpRectWidths == NULL);

	return S_OK;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::SetActiveObject(
	LPOLEINPLACEACTIVEOBJECT lpActiveObject, LPCOLESTR lpszObjName)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// release the old active object
		RELEASE(pThis->m_lpActiveObject);

		// set the new active object
		pThis->m_lpActiveObject = lpActiveObject;
		if (lpActiveObject != NULL)
			lpActiveObject->AddRef();

		// update caption if necessary
		pThis->m_strObjName.Empty();
		if (lpszObjName != NULL && lpActiveObject != NULL)
		{
			pThis->m_strObjName = lpszObjName;
			pThis->m_pActiveItem->OnUpdateFrameTitle();
		}
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::InsertMenus(
	HMENU hmenuShared, LPOLEMENUGROUPWIDTHS lpMenuWidths)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	// get the associated COleClientItem object
	COleClientItem* pItem = pThis->m_pActiveItem;
	ASSERT_VALID(pItem);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pItem->OnInsertMenus(CMenu::FromHandle(hmenuShared), lpMenuWidths);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::SetMenu(
	HMENU hmenuShared, HOLEMENU holemenu, HWND hwndActiveObject)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	// get the associated COleClientItem object
	COleClientItem* pItem = pThis->m_pActiveItem;
	ASSERT_VALID(pItem);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pItem->OnSetMenu(CMenu::FromHandle(hmenuShared), holemenu,
			hwndActiveObject);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::RemoveMenus(
	HMENU hmenuShared)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	// get the associated COleClientItem object
	COleClientItem* pItem = pThis->m_pActiveItem;
	ASSERT_VALID(pItem);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pItem->OnRemoveMenus(CMenu::FromHandle(hmenuShared));
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::SetStatusText(
	LPCOLESTR lpszStatusText)
{
	METHOD_PROLOGUE_EX_(COleFrameHook, OleInPlaceFrame)
	USES_CONVERSION;

	pThis->m_pFrameWnd->SendMessage(WM_SETMESSAGESTRING, 0,
		(LPARAM)OLE2CT(lpszStatusText));

	return S_OK;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::EnableModeless(BOOL fEnable)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);
	ASSERT_VALID(pThis->m_pFrameWnd);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		if (!fEnable)
			pThis->m_pFrameWnd->BeginModalState();
		else
			pThis->m_pFrameWnd->EndModalState();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleFrameHook::XOleInPlaceFrame::TranslateAccelerator(
	LPMSG lpmsg, WORD /*wID*/)
{
	METHOD_PROLOGUE_EX(COleFrameHook, OleInPlaceFrame)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// swap accel tables and call PreTranslateMessage
		CFrameWnd* pFrameWnd = pThis->m_pFrameWnd;
		HACCEL hAccelSave = pFrameWnd->m_hAccelTable;
		pFrameWnd->m_hAccelTable = pThis->m_hAccelTable;
		ASSERT(lpmsg != NULL);
		MSG msg = *lpmsg;
		sc = pFrameWnd->PreTranslateMessage(&msg) ? S_OK : S_FALSE;
		*lpmsg = msg;
		pFrameWnd->m_hAccelTable = hAccelSave;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem::XOleIPSite implementation

STDMETHODIMP_(ULONG) COleClientItem::XOleIPSite::AddRef()
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleIPSite)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleClientItem::XOleIPSite::Release()
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleIPSite)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleClientItem::XOleIPSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleIPSite)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleClientItem::XOleIPSite::GetWindow(HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleIPSite)

	*lphwnd = pThis->m_pView->GetSafeHwnd();
	return *lphwnd != NULL ? S_OK : E_FAIL;
}

STDMETHODIMP COleClientItem::XOleIPSite::ContextSensitiveHelp(
	BOOL fEnterMode)
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleIPSite)

	if (pThis->m_pInPlaceFrame == NULL)
		return E_UNEXPECTED;

	// simply delegate to frame window implementation
	return pThis->m_pInPlaceFrame->
		m_xOleInPlaceFrame.ContextSensitiveHelp(fEnterMode);
}

STDMETHODIMP COleClientItem::XOleIPSite::CanInPlaceActivate()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)

	return pThis->CanActivate() ? S_OK : S_FALSE;
}

STDMETHODIMP COleClientItem::XOleIPSite::OnInPlaceActivate()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnActivate();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::OnUIActivate()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnActivateUI();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::GetWindowContext(
	LPOLEINPLACEFRAME* lplpFrame,
	LPOLEINPLACEUIWINDOW* lplpDoc,
	LPRECT lpPosRect, LPRECT lpClipRect,
	LPOLEINPLACEFRAMEINFO lpFrameInfo)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	*lplpFrame = NULL;  // init these in-case of mem-alloc failure
	*lplpDoc = NULL;

	CFrameWnd* pMainFrame = NULL;
	CFrameWnd* pDocFrame = NULL;

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// get position of the item relative to activation view
		CRect rect;
		pThis->OnGetItemPosition(rect);
		::CopyRect(lpPosRect, &rect);
		pThis->OnGetClipRect(rect);
		::CopyRect(lpClipRect, &rect);

		// get the window context information
		if (pThis->OnGetWindowContext(&pMainFrame, &pDocFrame, lpFrameInfo))
		{
			// hook IOleInPlaceFrame interface to pMainFrame
			if (pThis->m_pInPlaceFrame == NULL)
				pThis->m_pInPlaceFrame = new COleFrameHook(pMainFrame, pThis);
			pThis->m_pInPlaceFrame->InternalAddRef();
			*lplpFrame = (LPOLEINPLACEFRAME)pThis->m_pInPlaceFrame->
				GetInterface(&IID_IOleInPlaceFrame);

			// save accel table for IOleInPlaceFrame::TranslateAccelerators
			pThis->m_pInPlaceFrame->m_hAccelTable = lpFrameInfo->haccel;

			// hook IOleInPlaceUIWindow to pDocFrame
			if (pDocFrame != NULL)
			{
				if (pThis->m_pInPlaceDoc == NULL)
					pThis->m_pInPlaceDoc = new COleFrameHook(pDocFrame, pThis);
				pThis->m_pInPlaceDoc->InternalAddRef();
				*lplpDoc = (LPOLEINPLACEUIWINDOW)pThis->m_pInPlaceDoc->
					GetInterface(&IID_IOleInPlaceUIWindow);
			}
			sc = S_OK;
		}
	}
	CATCH_ALL(e)
	{
		// cleanup memory that may be partially allocated
		delete *lplpFrame;
		ASSERT(*lplpDoc == NULL);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::Scroll(SIZE scrollExtent)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		if (!pThis->OnScrollBy(CSize(scrollExtent)))
			sc = S_FALSE;
		else
			sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::OnUIDeactivate(BOOL fUndoable)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnDeactivateUI(fUndoable);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::OnInPlaceDeactivate()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnDeactivate();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::DiscardUndoState()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnDiscardUndoState();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::DeactivateAndUndo()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnDeactivateAndUndo();
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleClientItem::XOleIPSite::OnPosRectChange(
	LPCRECT lpPosRect)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleIPSite)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		CRect rect;
		rect.CopyRect(lpPosRect);
		pThis->OnChangeItemPosition(rect);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
