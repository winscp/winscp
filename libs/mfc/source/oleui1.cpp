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
// User interface for COleClientItem

BOOL COleClientItem::ReportError(SCODE sc) const
	// return TRUE if error or warning reported
{
	ASSERT_VALID(this);
	UINT nIDPrompt = 0;

	switch (sc)
	{
	case OLE_E_STATIC:
		nIDPrompt = AFX_IDP_STATIC_OBJECT;
		break;

	case E_NOINTERFACE:
	case E_NOTIMPL:
	case E_FAIL:
		nIDPrompt = AFX_IDP_FAILED_TO_CONNECT;
		break;

	case E_OUTOFMEMORY:
		nIDPrompt = AFX_IDP_FAILED_MEMORY_ALLOC;
		break;

	default:
		return FALSE;       // nothing sensible to report
	}

	ASSERT(nIDPrompt != 0);
	AfxMessageBox(nIDPrompt);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Item activation

BOOL COleClientItem::DoVerb(LONG nVerb, CView* pView, LPMSG lpMsg)
{
	ASSERT_VALID(this);
	if (pView != NULL)
		ASSERT_VALID(pView);
	if (lpMsg != NULL)
		ASSERT(AfxIsValidAddress(lpMsg, sizeof(MSG), FALSE));

	TRY
	{
		Activate(nVerb, pView, lpMsg);
	}
	CATCH(COleException, e)
	{
		// catch OLE errors and report them as such
		if (!ReportError(e->m_sc))
			AfxMessageBox(AFX_IDP_FAILED_TO_LAUNCH);
		DELETE_EXCEPTION(e);
		return FALSE;
	}
	AND_CATCH_ALL(e)
	{
		// otherwise, show generic error
		AfxMessageBox(AFX_IDP_FAILED_TO_LAUNCH);
		DELETE_EXCEPTION(e);
		return FALSE;
	}
	END_CATCH_ALL

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientDoc - user interface implementation
//  (functions reside in COleDocument to enable them on the server as well)

BOOL COleDocument::OnCmdMsg(UINT nID, int nCode, void* pExtra,
		AFX_CMDHANDLERINFO* pHandlerInfo)
{
	ASSERT_VALID(this);

	if (nCode == CN_COMMAND && nID >= ID_OLE_VERB_FIRST && nID <= ID_OLE_VERB_LAST)
	{
		CView* pRoutingView = GetRoutingView_();
		COleClientItem* pSel = GetPrimarySelectedItem(pRoutingView);
		if (pSel != NULL)
		{
			if (pHandlerInfo != NULL)       // routing test
			{
				pHandlerInfo->pTarget = this;
				return TRUE;        // would be handled here
			}

			// activate the current selection with the appropriate verb
			CWaitCursor wait;
			pSel->DoVerb(nID - ID_OLE_VERB_FIRST, pRoutingView);
			return TRUE;    // handled
		}
	}
	return CDocument::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);
}

COleClientItem* COleDocument::GetPrimarySelectedItem(CView* pView)
{
	ASSERT_VALID(this);
	ASSERT(pView != NULL);
	ASSERT_VALID(pView);

	COleClientItem* pSelectedItem = NULL;

	// walk all items in the document - return one if there
	//   is only one client item selected
	// (note: non OLE client items are ignored)
	POSITION pos = GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = GetNextClientItem(pos)) != NULL)
	{
		if (pView->IsSelected(pItem))
		{
			// client item selected in
			if (pSelectedItem != NULL)
				return NULL;        // more than one - no primary selection
			pSelectedItem = pItem;
		}
	}
	return pSelectedItem;
}

/////////////////////////////////////////////////////////////////////////////
// In-place item handling

COleClientItem* COleDocument::GetInPlaceActiveItem(CWnd* pWnd)
{
	ASSERT_VALID(this);
	ASSERT(pWnd != NULL);
	ASSERT_VALID(pWnd);

	// check for any item active on the immediate frame of pWndContainer
	//  (two active objects on same frame are not supported)
	if (!pWnd->IsFrameWnd())
	{
		CFrameWnd* pFrameWnd = pWnd->GetParentFrame();
		if (pFrameWnd != NULL)
			pWnd = pFrameWnd;
	}

	POSITION pos = GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = GetNextClientItem(pos)) != NULL)
	{
		if (pItem->m_pView != NULL && pItem->IsInPlaceActive() &&
			(pItem->m_pView == pWnd ||
			 pItem->m_pView->GetParentFrame() == pWnd))
		{
			// that item is active on pWndContainer
			return pItem;
		}
	}

	// no item active on that window
	return NULL;
}

/////////////////////////////////////////////////////////////////////////////
