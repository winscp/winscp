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

/////////////////////////////////////////////////////////////////////////////
// COleDocObjectItem

IMPLEMENT_DYNAMIC(COleDocObjectItem, COleClientItem)

BEGIN_INTERFACE_MAP(COleDocObjectItem, COleClientItem)
	INTERFACE_PART(COleDocObjectItem, IID_IOleDocumentSite, OleDocumentSite)
END_INTERFACE_MAP()


COleDocObjectItem::COleDocObjectItem(COleDocument* pContainerDoc)
	: COleClientItem(pContainerDoc)
{
	m_pHelpPopupMenu = NULL;
	m_pActiveView = NULL;
	m_pIPrint = NULL;
	m_bInHelpMenu = FALSE;
}

COleDocObjectItem::~COleDocObjectItem()
{
	if (m_pHelpPopupMenu != NULL)
		m_pHelpPopupMenu->RemoveMenu(0, MF_BYPOSITION);
	delete m_pHelpPopupMenu;
}


/////////////////////////////////////////////////////////////////////////////
// IOleDocumentSite interface

STDMETHODIMP_(ULONG) COleDocObjectItem::XOleDocumentSite::AddRef()
{
	METHOD_PROLOGUE_EX(COleDocObjectItem, OleDocumentSite)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleDocObjectItem::XOleDocumentSite::Release()
{
	METHOD_PROLOGUE_EX(COleDocObjectItem, OleDocumentSite)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleDocObjectItem::XOleDocumentSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(COleDocObjectItem, OleDocumentSite)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleDocObjectItem::XOleDocumentSite::ActivateMe(
	LPOLEDOCUMENTVIEW pViewToActivate)
{
	METHOD_PROLOGUE_EX(COleDocObjectItem, OleDocumentSite)

	LPOLEDOCUMENT lpDocument;
	LPOLECLIENTSITE lpClientSite = pThis->GetClientSite();
	LPOLEINPLACESITE lpInPlaceSite =
		(LPOLEINPLACESITE) pThis->GetInterface(&IID_IOleInPlaceSite);

	if (lpClientSite == NULL || lpInPlaceSite == NULL)
		return E_FAIL;

	// if we've gotten a NULL view, we're to create one ourselves

	if (pViewToActivate == NULL)
	{
		// if we already have a view, we can simply activate it

		if (pThis->m_pActiveView != NULL && pThis->m_pView != NULL)
		{
			pThis->ActivateAndShow();
			return NOERROR;
		}

		ASSERT(pThis->m_lpObject != NULL);
		if (pThis->m_lpObject == NULL)
			return E_FAIL;

		lpDocument = QUERYINTERFACE(pThis->m_lpObject, IOleDocument);
		if (lpDocument == NULL)
			return E_FAIL;

		if (FAILED(
			lpDocument->CreateView(lpInPlaceSite, NULL, 0, &pViewToActivate)))
		{
			lpDocument->Release();
		return E_OUTOFMEMORY;
		}

		// we're done with the document pointer
		lpDocument->Release();
	}
	else if (pThis->m_pActiveView != NULL && pThis->m_pActiveView == pViewToActivate)
	{
		// we already own this view, so no need to addref
		// simply make it visible and resize it

		pThis->ActivateAndShow();
		return NOERROR;
	}
	else
	{
		// set the in-place site
		pViewToActivate->SetInPlaceSite(lpInPlaceSite);
		pViewToActivate->AddRef();
	}

	// it must've created
	ASSERT(pThis->m_pView != NULL);

	// if we had an old one, release it
	if (pThis->m_pActiveView != NULL)
	{
		pThis->m_pActiveView->Show(FALSE);
		pThis->m_pActiveView->UIActivate(FALSE);
		pThis->m_pActiveView->Release();

		if (pThis->m_pIPrint != (IPrint*) -1 && pThis->m_pIPrint != NULL)
			pThis->m_pIPrint->Release();
		pThis->m_pIPrint = NULL;
	}

	// remember it for later
	pThis->m_pActiveView = pViewToActivate;

	// activate and position it
	pThis->ActivateAndShow();

	return NOERROR;
}

/////////////////////////////////////////////////////////////////////////////
// IOleDocumentSite implementation helper

void COleDocObjectItem::ActivateAndShow()
{
	// set up toolbars and menus for the object
	m_pActiveView->UIActivate(TRUE);

	// set the window size, avoiding new toolbars
	RECT rc;
	m_pView->GetClientRect(&rc);
	m_pActiveView->SetRect(&rc);

	// make everything visible
	m_pActiveView->Show(TRUE);

	return;
}

void COleDocObjectItem::OnGetItemPosition(CRect& rPosition)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(&rPosition, sizeof(RECT)));

	// doc objects [almost] always in the exact rect of the view
	ASSERT_VALID(m_pView);
	m_pView->GetClientRect(&rPosition);
}

LPOLEDOCUMENTVIEW COleDocObjectItem::GetActiveView() const
{
	return m_pActiveView;
}

void COleDocObjectItem::Release(OLECLOSE dwCloseOption)
{
	RELEASE(m_pActiveView);
	if (m_pIPrint != (IPrint*) -1)
		RELEASE(m_pIPrint);
	COleClientItem::Release(dwCloseOption);
}

HRESULT COleDocObjectItem::ExecCommand(DWORD nCmdID,
	DWORD nCmdExecOpt /* = OLECMDEXECOPT_DONTPROMPTUSER */,
	const GUID* pguidCmdGroup /* = NULL */)
{
	LPOLECOMMANDTARGET lpCt = QUERYINTERFACE(m_lpObject, IOleCommandTarget);
	HRESULT hr = E_NOTIMPL;

	if (lpCt != NULL)
		hr = lpCt->Exec(pguidCmdGroup, nCmdID, nCmdExecOpt, NULL, NULL);

	RELEASE(lpCt);
	return hr;
}

BOOL COleDocObjectItem::SupportsIPrint()
{
	// did someone already ask? -1 means we know it doesn't,
	// non-NULL means we know it does (and we point at it)
	// NULL means we don't know

	if (m_pIPrint == NULL)
	{
		// QI for it
		m_pIPrint = QUERYINTERFACE(m_lpObject, IPrint);

		if (m_pIPrint == NULL)
		{
			// if the server isn't running, we'll need to
			// start it in order to print

			if (FAILED(::OleRun(m_lpObject)))
				m_pIPrint = (IPrint*) -1;
			else
				m_pIPrint = QUERYINTERFACE(m_lpObject, IPrint);
		}
	}

	return (m_pIPrint != NULL && m_pIPrint != (IPrint*) -1);
}

BOOL COleDocObjectItem::GetPageCount(LPLONG pnFirstPage, LPLONG pcPages)
{
	if (!SupportsIPrint())
		return FALSE;

	//WINBUG: The proxy in DOCOBJ.DLL is broken; it doesn't allow
	// NULL parameters to IPrint::GetPageInfo(), even though the spec
	// says it should.

	LONG lPages;
	LONG lFirstPage;

	HRESULT hr = m_pIPrint->GetPageInfo(&lFirstPage, &lPages);

	if (pnFirstPage != NULL)
		*pnFirstPage = lFirstPage;
	if (pcPages != NULL)
		*pcPages = lPages;

	if (SUCCEEDED(hr))
		return TRUE;
	else
		return FALSE;
}


CMenu* COleDocObjectItem::GetHelpMenu(UINT& nPosition)
{
	CFrameWnd* pFrame = m_pView->GetTopLevelFrame();
	CMenu* pMenuFrame = CMenu::FromHandle(pFrame->m_hMenuDefault);

	if (pMenuFrame != NULL)
		nPosition = pMenuFrame->GetMenuItemCount() -1;

	return pMenuFrame;
}

void COleDocObjectItem::OnInsertMenus(CMenu* pMenuShared,
	LPOLEMENUGROUPWIDTHS lpMenuWidths)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pMenuShared);
	ASSERT(AfxIsValidAddress(lpMenuWidths, sizeof(OLEMENUGROUPWIDTHS)));

	// initialize the group widths array
	lpMenuWidths->width[0] = 1;
	lpMenuWidths->width[2] = 0;
	lpMenuWidths->width[4] = 0;

	// get menu from document template
	CDocTemplate* pTemplate = GetDocument()->GetDocTemplate();
	HMENU hMenuOLE = pTemplate->m_hMenuInPlace;

	// only copy the popups if there is a menu loaded
	if (hMenuOLE == NULL)
		return;

	UINT nItem;
	CMenu* pMenuFrame = GetHelpMenu(nItem);

	if (pMenuFrame != NULL)
	{
		CString strHelpMenuName;
		int nSeparator = pMenuFrame->GetMenuString(nItem,
				strHelpMenuName, MF_BYPOSITION);
		if (nSeparator == 0)
		{
			TRACE0("Error: COleDocObjectItem::OnInsertMenus() found no help menu!\n");
			return;
		}

		CString strTearOffName;
		strTearOffName.Format(_T("%s %s"), AfxGetAppName(), strHelpMenuName);
		strTearOffName.Remove('&');

		// get the normal frame menu
		int nCount = pMenuFrame->GetMenuItemCount();
		HMENU hMenu = GetSubMenu(pMenuFrame->m_hMenu, nCount-1);

		// clean up old menu and allocate a new one
		if (m_pHelpPopupMenu == NULL)
		{
			m_pHelpPopupMenu = new CMenu;

			// create new sub-popup menu and add container's Help tearoff
			// then add help menu from main window
			m_pHelpPopupMenu->CreateMenu();
			m_pHelpPopupMenu->InsertMenu((UINT) -1, MF_BYPOSITION | MF_POPUP,
				(UINT) hMenu, strTearOffName);
		}

		pMenuShared->InsertMenu(1, MF_BYPOSITION | MF_POPUP,
			(UINT) m_pHelpPopupMenu->m_hMenu, strHelpMenuName);

		// tell the object we added our Help menu
		lpMenuWidths->width[5] = 1;
	}

	// insert our menu items and adjust group widths array
	AfxMergeMenus(pMenuShared->GetSafeHmenu(), hMenuOLE,
		&lpMenuWidths->width[0], 0);
}

void COleDocObjectItem::OnRemoveMenus(CMenu *pMenuShared)
{
	int cItemsShared = pMenuShared->GetMenuItemCount();
	if (cItemsShared != 0)
	{
		CMenu *pMenuHelp = pMenuShared->GetSubMenu(cItemsShared - 1);

		int cItemsHelp = pMenuHelp->GetMenuItemCount();
		int nItem;
		for (nItem = cItemsHelp-1; nItem > 0; nItem--)
			pMenuHelp->DeleteMenu(nItem, MF_BYPOSITION);

		pMenuShared->RemoveMenu(cItemsShared - 1, MF_BYPOSITION);
	}

	COleClientItem::OnRemoveMenus(pMenuShared);
}

BOOL COleDocObjectItem::OnPreparePrinting(CView* pCaller,
	CPrintInfo* pInfo, BOOL bPrintAll)
{
	LONG lDocObjectPages = 0;

	CDocument* pDoc = pCaller->GetDocument();
	COleDocument* pOleDoc = DYNAMIC_DOWNCAST(COleDocument, pDoc);
	if (pOleDoc == NULL)
		return FALSE;

	POSITION pos = pOleDoc->GetStartPosition();
	while (pos != NULL)
	{
		COleClientItem* pItem = pOleDoc->GetNextClientItem(pos);
		COleDocObjectItem* pDocItem =
			DYNAMIC_DOWNCAST(COleDocObjectItem, pItem);
		if (pDocItem == NULL)
			continue;

		// if this isn't the view, continue
		if (!bPrintAll)
		{
			if (pItem->m_pView == NULL ||
				 pItem->m_pView->m_hWnd != pCaller->m_hWnd)
				continue;
		}

		if (pDocItem->SupportsIPrint())
		{
			LONG lThisObjectPages;
			if (pDocItem->GetPageCount(NULL, &lThisObjectPages))
				lDocObjectPages += lThisObjectPages;
			pInfo->m_bDocObject = TRUE;
		}
		else
			lDocObjectPages++;

		if (!bPrintAll)
			break;
	}

	if (lDocObjectPages > 0)
	{
		UINT nMaxPage = pInfo->GetMaxPage();

		// set the page count; increment it if previously set
		if (nMaxPage == 0xFFFF)
			pInfo->SetMaxPage(lDocObjectPages);
		else
			pInfo->SetMaxPage(nMaxPage + lDocObjectPages);
		pInfo->m_bDocObject = TRUE;
	}

	if (pInfo->m_bDocObject)
	{
		// we can't show the "selection" button for DocObjects
		pInfo->m_pPD->m_pd.Flags |= PD_NOSELECTION;

		// if it's a doc object, and we're printing all, then we
		// shouldn't show the selection
		if (bPrintAll)
			pInfo->m_pPD->m_pd.Flags |= PD_NOPAGENUMS;
	}

	return TRUE;
}

void COleDocObjectItem::OnPrint(CView* pCaller, CPrintInfo* pInfo,
	BOOL bPrintAll)
{
	// Note that this function ignores pInfo->m_nCurPage, and will always
	// print the whole range of pages in the CPrintInfo object. That's
	// because DocObjects don't support any mechanism to _continue_
	// printing to an existing print job.

	CDocument* pDoc = pCaller->GetDocument();
	COleDocument* pOleDoc = DYNAMIC_DOWNCAST(COleDocument, pDoc);
	if (pOleDoc == NULL)
		return;

	POSITION pos = pOleDoc->GetStartPosition();
	while (pos != NULL)
	{
		COleClientItem* pItem = pOleDoc->GetNextClientItem(pos);
		COleDocObjectItem* pDocItem = DYNAMIC_DOWNCAST(COleDocObjectItem, pItem);
		if (pDocItem == NULL)
			continue;

		// if this isn't the view, continue
		if (!bPrintAll)
		{
			if (pItem->m_pView == NULL || pItem->m_pView->m_hWnd != pCaller->m_hWnd)
				continue;
		}

		HRESULT hrThisPage = E_UNEXPECTED;

		if (pDocItem->SupportsIPrint())
		{
			DVTARGETDEVICE* pTargetDevice = NULL;
			LPDEVNAMES lpDevNames = NULL;
			LPDEVMODE lpDevMode = NULL;

			lpDevNames = (LPDEVNAMES) GlobalLock(pInfo->m_pPD->m_pd.hDevNames);
			if (lpDevNames != NULL)
			{
				lpDevMode = (LPDEVMODE) GlobalLock(pInfo->m_pPD->m_pd.hDevMode);
				if (lpDevMode != NULL)
				{
					pTargetDevice = _AfxOleCreateTargetDevice(lpDevNames, lpDevMode);
					if (pTargetDevice != NULL)
					{
						PAGESET* pps = (PAGESET*) CoTaskMemAlloc(sizeof(PAGESET));
						if (pps != NULL)
						{
							pps->cbStruct = sizeof(PAGESET);
							ASSERT((pps->cbStruct % 4) == 0);
							pps->fOddPages = TRUE;
							pps->fEvenPages = TRUE;
							pps->cPageRange = 1;
							pps->rgPages[0].nFromPage = pInfo->GetFromPage();
							pps->rgPages[0].nToPage = pInfo->GetToPage();

							LONG lLastPage = pps->rgPages[0].nFromPage;
							LONG lPagesPrinted;

							DWORD dwFlags = PRINTFLAG_RECOMPOSETODEVICE;

							if (pInfo->m_pPD->m_pd.Flags & PD_PRINTTOFILE)
								dwFlags |= PRINTFLAG_PRINTTOFILE;

							hrThisPage = pDocItem->m_pIPrint->Print(dwFlags,
									&pTargetDevice, &pps, NULL, NULL,
									pInfo->m_nCurPage, &lPagesPrinted,
									&lLastPage);

							if (!SUCCEEDED(hrThisPage))
								TRACE1("IPrint::Print() returned %8.8X\n", hrThisPage);
							CoTaskMemFree(pps);
						}
						CoTaskMemFree(pTargetDevice);
					}
					GlobalUnlock(pInfo->m_pPD->m_pd.hDevMode);
				}
				GlobalUnlock(pInfo->m_pPD->m_pd.hDevNames);
			}
		}
		else
		{
			// try through IOleCommandTarget

			hrThisPage = pDocItem->ExecCommand(OLECMDID_PRINT);
			if (!SUCCEEDED(hrThisPage))
				TRACE1("IOleCommandTarget::Exec() returned %8.8X\n", hrThisPage);
		}
	}

	return;
}
