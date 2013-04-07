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

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

void CDocObjectServer::OnSaveViewState(CArchive& /* ar */)
{
	// user can override to save some state or do something neat
}

void CDocObjectServer::OnApplyViewState(CArchive& /* ar */)
{
	// user can override to restore state or do something neat
}


HRESULT CDocObjectServer::OnActivateView()
{
	USES_CONVERSION;
	ASSERT_VALID(this);

	HRESULT hr = E_FAIL;

	// Can't in-place activate without a client site
	if (m_pOwner->m_lpClientSite == NULL)
		return NOERROR;

	// build object title/name (the container may use this in its caption)
	CString strFileType, strTitle;
	if (!m_pOwner->GetFileTypeString(strFileType))
		return E_FAIL;
	AfxFormatString2(strTitle, AFX_IDS_OBJ_TITLE_INPLACE,
		AfxGetAppName(), strFileType);

	// Find our view site
	LPOLEINPLACESITE lpInPlaceSite = NULL;
	lpInPlaceSite = m_pViewSite;
	if (lpInPlaceSite == NULL)
		return E_FAIL;
	lpInPlaceSite->AddRef();

	// start activation sequence...
	if ((hr = lpInPlaceSite->OnInPlaceActivate()) != NOERROR)
		goto ReleaseAndFail;

	// we'll need the parent window to create the CDocObjectIPFrameWnd
	HWND hWnd;
	VERIFY(lpInPlaceSite->GetWindow(&hWnd) == NOERROR);
	CWnd* pParentWnd;
	pParentWnd = CWnd::FromHandle(hWnd);

	// create the inplace frame window
	COleIPFrameWnd* pFrameWnd;

	// if we've not been activate before, we'll need
	// to create our frame at this time

	if (m_pOwner->m_pInPlaceFrame != NULL)
		pFrameWnd = m_pOwner->m_pInPlaceFrame;
	else
	{
		pFrameWnd = (COleIPFrameWnd*) m_pOwner->CreateInPlaceFrame(pParentWnd);
		if (pFrameWnd == NULL)
		{
			ASSERT(lpInPlaceSite != NULL);
			lpInPlaceSite->OnInPlaceDeactivate();
			goto ReleaseAndFail;
		}

		ASSERT(pFrameWnd->GetParent() == pParentWnd);
		m_pOwner->m_pInPlaceFrame = pFrameWnd;

		// need to get frame & doc window interfaces as well as other info
		RECT rcPosRect, rcClipRect;
		if ((hr = lpInPlaceSite->GetWindowContext(
			&pFrameWnd->m_lpFrame, &pFrameWnd->m_lpDocFrame,
			&rcPosRect, &rcClipRect, &pFrameWnd->m_frameInfo)) != NOERROR)
		{
			goto DestroyFrameAndFail;
		}
		ASSERT(pFrameWnd->m_lpFrame != NULL);

		// send activate notification
		if ((hr = lpInPlaceSite->OnUIActivate()) != NOERROR)
			goto DestroyFrameAndFail;

		// setup the shared menu
		if (!pFrameWnd->BuildSharedMenu())
			goto DeactivateUIAndFail;

		// allow server to install frame controls in container

		VERIFY(pFrameWnd->m_lpFrame->GetWindow(&hWnd) == NOERROR);

		pFrameWnd->m_pMainFrame = new COleCntrFrameWnd(pFrameWnd);
		pFrameWnd->m_pMainFrame->Attach(hWnd);

		if (pFrameWnd->m_lpDocFrame != NULL)
		{
			VERIFY(pFrameWnd->m_lpDocFrame->GetWindow(&hWnd) == NOERROR);
			pFrameWnd->m_pDocFrame = new COleCntrFrameWnd(pFrameWnd);
			pFrameWnd->m_pDocFrame->Attach(hWnd);
		}
		// update zoom factor information before creating control bars
		pFrameWnd->m_rectPos.CopyRect(&rcPosRect);
		pFrameWnd->m_rectClip.CopyRect(&rcClipRect);
		if (!pFrameWnd->OnCreateControlBars(pFrameWnd->m_pMainFrame,
			pFrameWnd->m_pDocFrame))
		{
			goto DeactivateUIAndFail;
		}
	}

	// set the active object
	ASSERT(pFrameWnd->m_lpFrame != NULL);
	LPOLEINPLACEACTIVEOBJECT lpActiveObject;
	lpActiveObject = (LPOLEINPLACEACTIVEOBJECT)
		m_pOwner->GetInterface(&IID_IOleInPlaceActiveObject);

	pFrameWnd->m_lpFrame->SetActiveObject(lpActiveObject,
		T2OLE((LPTSTR) (LPCTSTR) strTitle));

	if (pFrameWnd->m_lpDocFrame != NULL)
	{
		pFrameWnd->m_lpDocFrame->SetActiveObject(lpActiveObject,
			T2OLE((LPTSTR) (LPCTSTR) strTitle));
	}

	// add frame & document level frame controls
	ASSERT(m_pOwner->m_pInPlaceFrame == pFrameWnd);
	ASSERT(pFrameWnd->m_lpFrame != NULL);
	m_pOwner->OnShowControlBars(pFrameWnd->m_pMainFrame, TRUE);
	if (pFrameWnd->m_lpDocFrame != NULL)
		m_pOwner->OnShowControlBars(pFrameWnd->m_pDocFrame, TRUE);

	// show any hidden modeless dialogs as well...
	pFrameWnd->ShowOwnedWindows(TRUE);

	// attempt toolbar negotiation
	m_pOwner->OnResizeBorder(NULL, pFrameWnd->m_lpFrame, TRUE);
	if (pFrameWnd->m_lpDocFrame != NULL)
		m_pOwner->OnResizeBorder(NULL, pFrameWnd->m_lpDocFrame, FALSE);

	// install the menu (also installs a hook which forwards messages from
	//  the menu to the inplace frame window)
	pFrameWnd->m_lpFrame->SetMenu(pFrameWnd->m_hSharedMenu,
		pFrameWnd->m_hOleMenu, pFrameWnd->m_hWnd);

	// finally -- show the inplace frame window and set focus
	pFrameWnd->ShowWindow(SW_SHOW);
	pFrameWnd->SetFocus();
	pFrameWnd->UpdateWindow();

	// allow the main window to be set
	m_pOwner->OnFrameWindowActivate(TRUE);
	pFrameWnd->m_bUIActive = TRUE;

	// cleanup and return
	lpInPlaceSite->Release();
	return hr;

DeactivateUIAndFail:
	ASSERT(lpInPlaceSite != NULL);
	lpInPlaceSite->OnUIDeactivate(FALSE);

DestroyFrameAndFail:
	if (m_pOwner->m_pInPlaceFrame != NULL)
	{
		ASSERT(pFrameWnd != NULL);
		m_pOwner->DestroyInPlaceFrame(pFrameWnd);
		m_pOwner->m_pInPlaceFrame = NULL;

		// also need to send OnInPlaceDeactivate notification
		ASSERT(lpInPlaceSite != NULL);
		lpInPlaceSite->OnInPlaceDeactivate();
	}
ReleaseAndFail:
	ASSERT(lpInPlaceSite != NULL);
	lpInPlaceSite->Release();

	return hr;
}

STDMETHODIMP_(ULONG) CDocObjectServer::XOleDocumentView::AddRef()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	return pThis->m_pOwner->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CDocObjectServer::XOleDocumentView::Release()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	return pThis->m_pOwner->ExternalRelease();
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::QueryInterface(REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	return pThis->m_pOwner->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::SetInPlaceSite(
	LPOLEINPLACESITE pIPSite)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	// if currently inplace active, then do normal inplace deactivation
	if (pThis->m_pOwner->IsInPlaceActive())
		pThis->m_pOwner->m_xOleInPlaceObject.InPlaceDeactivate();

	// release the view site pointer
	if (pThis->m_pViewSite)
		pThis->m_pViewSite->Release();

	// remember the new view site pointer and addref it, if it is non-NULL
	pThis->m_pViewSite = pIPSite;
	if (pThis->m_pViewSite != NULL)
		pThis->m_pViewSite->AddRef();

	return NOERROR;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::GetInPlaceSite(LPOLEINPLACESITE* ppIPSite)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);
	ASSERT(ppIPSite != NULL);

	if (pThis->m_pViewSite)
		pThis->m_pViewSite->AddRef();
	*ppIPSite = pThis->m_pViewSite;

	return NOERROR;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::GetDocument(LPUNKNOWN* ppUnk)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);
	ASSERT(ppUnk != NULL);

	HRESULT hr = pThis->m_xOleDocument.QueryInterface(IID_IUnknown,
		(LPVOID*)ppUnk);
	ASSERT(*ppUnk != NULL);

	return hr;
}

void CDocObjectServer::OnSetItemRects(LPRECT lprcPosRect, LPRECT lprcClipRect)
{
	m_pOwner->OnSetItemRects(lprcPosRect, lprcClipRect);
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::SetRect(LPRECT lprcView)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);
	ASSERT(lprcView != NULL);

	HRESULT hr = E_UNEXPECTED;
	TRY
	{
		pThis->OnSetItemRects(lprcView, lprcView);
		hr = NOERROR;
	}
	END_TRY

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::GetRect(LPRECT lprcView)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);
	ASSERT(lprcView != NULL);

	pThis->m_pOwner->GetItemPosition(lprcView);
	return NOERROR;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::SetRectComplex(
	LPRECT lprcView, LPRECT lprcHScroll,
	LPRECT lprcVScroll, LPRECT lprcSizeBox)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	UNUSED_ALWAYS(lprcView);
	UNUSED_ALWAYS(lprcHScroll);
	UNUSED_ALWAYS(lprcVScroll);
	UNUSED_ALWAYS(lprcSizeBox);

	// We don't support complex rectangles, so return error
	return E_NOTIMPL;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::Show(BOOL bShow)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	HRESULT hr = NOERROR;

	if (bShow)
	{
		// in-place but don't UI activate; give the view focus
		hr = pThis->m_pOwner->ActivateInPlace();
	}
	else
	{
		// Call IOleInPlaceObject::InPlaceDeactivate on this view
		hr = pThis->m_pOwner->m_xOleInPlaceObject.InPlaceDeactivate();
	}

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::UIActivate(BOOL bUIActivate)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	HRESULT hr = NOERROR;

	if (bUIActivate)
	{
		// UI Activate the view then take focus and bring the view forward
		hr = pThis->OnActivateView();
	}
	else
	{
		// Call IOleInPlaceObject::UIDeactivate on this view
		hr = pThis->m_pOwner->m_xOleInPlaceObject.UIDeactivate();
	}
	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::Open()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	return pThis->m_pOwner->m_xOleObject.DoVerb(OLEIVERB_OPEN, NULL,
		NULL, 0, NULL, NULL);
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::CloseView(DWORD /* dwReserved */)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	// Call IOleDocumentView::Show(FALSE) to hide the view
	Show(FALSE);

	// Call IOleDocumentView::SetInPlaceSite(NULL) to deactivate the object
	HRESULT hr = SetInPlaceSite(NULL);

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::SaveViewState(LPSTREAM pstm)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	HRESULT hr = NOERROR;

	// Attach the stream to an MFC file object
	COleStreamFile file;
	file.Attach(pstm);
	CFileException fe;

	// save it via a CArchive
	CArchive saveArchive(&file, CArchive::store | CArchive::bNoFlushOnDelete);
	TRY
	{
		pThis->OnSaveViewState(saveArchive);
		saveArchive.Close();
		file.Detach();
	}
	CATCH(COleException, pOE)
	{
		hr = pOE->m_sc;
	}
	AND_CATCH_ALL(e)
	{
		hr = E_UNEXPECTED;
	}
	END_CATCH_ALL

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::ApplyViewState(LPSTREAM pstm)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	HRESULT hr = NOERROR;

	// Attach the stream to an MFC file object
	COleStreamFile file;
	file.Attach(pstm);
	CFileException fe;

	// load it with CArchive
	CArchive loadArchive(&file, CArchive::load | CArchive::bNoFlushOnDelete);
	TRY
	{
		pThis->OnApplyViewState(loadArchive);
		loadArchive.Close();
		file.Detach();
	}
	CATCH(COleException, pOE)
	{
		hr = pOE->m_sc;
	}
	AND_CATCH_ALL(e)
	{
		hr = E_UNEXPECTED;
	}
	END_CATCH_ALL

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocumentView::Clone(
	LPOLEINPLACESITE pipsiteNew, LPOLEDOCUMENTVIEW* ppviewNew)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocumentView)
	ASSERT_VALID(pThis);

	UNUSED_ALWAYS(pipsiteNew);
	UNUSED_ALWAYS(ppviewNew);

	// In order to support this, we would need to support multiple views,
	// which we do not.  So we will return an error.
	return E_NOTIMPL;
}
