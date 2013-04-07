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
// CDocObjectServer

IMPLEMENT_DYNAMIC(CDocObjectServer, CCmdTarget)

BEGIN_MESSAGE_MAP(CDocObjectServer, CCmdTarget)
	//{{AFX_MSG_MAP(CDocObjectServer)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BEGIN_INTERFACE_MAP(CDocObjectServer, CCmdTarget)
	INTERFACE_PART(CDocObjectServer, IID_IOleObject, OleObject)
	INTERFACE_PART(CDocObjectServer, IID_IOleDocument, OleDocument)
	INTERFACE_PART(CDocObjectServer, IID_IOleDocumentView, OleDocumentView)
	INTERFACE_PART(CDocObjectServer, IID_IOleCommandTarget, OleCommandTarget)
	INTERFACE_PART(CDocObjectServer, IID_IPrint, Print)
END_INTERFACE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CDocObjectServer implementation

CDocObjectServer::CDocObjectServer(COleServerDoc* pDoc,
	LPOLEDOCUMENTSITE pDocSite /* = NULL */)
{
	// Initialize DocObject data
	m_pDocSite  = pDocSite;
	m_pViewSite = NULL;

	m_pOwner = pDoc;
	ASSERT(m_pOwner != NULL);

	m_nFirstPage = 1;

	// All Binder-Compatible documents use Compound Files as their
	// storage mechanism
	m_pOwner->EnableCompoundFile(TRUE);

	m_nFirstPage = -1;
}

CDocObjectServer::~CDocObjectServer()
{
	ReleaseDocSite();
}

void CDocObjectServer::ReleaseDocSite()
{
	if (m_pDocSite != NULL)
	{
		m_pDocSite->Release();
		m_pDocSite = NULL;
	}
}

void CDocObjectServer::SetDocSite(LPOLEDOCUMENTSITE pNewSite)
{
	ReleaseDocSite();
	m_pDocSite = pNewSite;
}

void CDocObjectServer::OnCloseDocument()
{
	// Clean up pointer to document site, if any
	ReleaseDocSite();
	m_pOwner->OnCloseDocument();
}

void CDocObjectServer::ActivateDocObject()
{
	ASSERT(m_pOwner != NULL);
	if (m_pOwner->IsDocObject())
	{
		ASSERT(m_pDocSite != NULL);
		m_pDocSite->ActivateMe(NULL);
	}
}

STDMETHODIMP CDocObjectServer::OnExecOleCmd(
   const GUID* pguidCmdGroup, DWORD nCmdID, DWORD nCmdExecOpt,
   VARIANTARG* pvarargIn, VARIANTARG* pvarargOut)
{
	ASSERT(m_pOwner != NULL);
	if (m_pOwner == NULL)
		return E_NOTIMPL;
	else
		return m_pOwner->OnExecOleCmd(pguidCmdGroup, nCmdID,
			nCmdExecOpt, pvarargIn, pvarargOut);
}

COleDocIPFrameWnd* CDocObjectServer::GetControllingFrame() const
{
	COleDocIPFrameWnd* pFrame = NULL;
	POSITION pos = m_pOwner->GetFirstViewPosition();
	if (pos != NULL)
	{
		CView* pView = m_pOwner->GetNextView(pos);
		if (pView != NULL)
		{
			CWnd* pParent = pView->GetParentFrame();
			pFrame = DYNAMIC_DOWNCAST(COleDocIPFrameWnd, pParent);
		}
	}

#ifdef _DEBUG
	// This TRACE will trip if you've not converted your application to
	// use a COleDocIPFrameWnd, or if you've incorrectly hooked up
	// DocObject support in your application.

	if (pFrame == NULL)
		TRACE0("Error: An appropriate DocObject frame could not be found.\n");
#endif

	return pFrame;
}

BOOL CDocObjectServer::DoPreparePrinting(CView* pView, CPrintInfo* printInfo)
{
	return pView->OnPreparePrinting(printInfo);
}

void CDocObjectServer::DoPrepareDC(CView* pView, CDC* pdcPrint,
	CPrintInfo* pprintInfo)
{
	pView->OnPrepareDC(pdcPrint, pprintInfo);
}

void CDocObjectServer::DoPrint(CView* pView, CDC* pdcPrint,
	CPrintInfo* pprintInfo)
{
	pView->OnPrint(pdcPrint, pprintInfo);
}

void CDocObjectServer::DoBeginPrinting(CView* pView,
	CDC* pDC, CPrintInfo* pprintInfo)
{
	pView->OnBeginPrinting(pDC, pprintInfo);
}

void CDocObjectServer::DoEndPrinting(CView* pView,
	CDC* pDC, CPrintInfo* pprintInfo)
{
	pView->OnEndPrinting(pDC, pprintInfo);
}


/////////////////////////////////////////////////////////////////////////////
// IPrint interface

extern BOOL CALLBACK _AfxAbortProc(HDC, int);   // from VIEWPRNT.CPP

STDMETHODIMP_(ULONG) CDocObjectServer::XPrint::AddRef()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	return pThis->m_pOwner->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CDocObjectServer::XPrint::Release()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	return pThis->m_pOwner->ExternalRelease();
}

STDMETHODIMP CDocObjectServer::XPrint::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP CDocObjectServer::XPrint::SetInitialPageNum(
   LONG nFirstPage)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	ASSERT_VALID(pThis);
	pThis->m_nFirstPage = nFirstPage;

	return S_OK;
}

STDMETHODIMP CDocObjectServer::XPrint::GetPageInfo(
   LPLONG pnFirstPage, LPLONG pcPages)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	ASSERT_VALID(pThis);

	// find the view we need to print

	CView* pView = NULL;
	POSITION pos = pThis->m_pOwner->GetFirstViewPosition();
	if (pos != NULL)
		pView = pThis->m_pOwner->GetNextView(pos);

	if (pView == NULL)
		return E_UNEXPECTED;

	// tell the view that we're not actually printing
	// and just need to measure the print job

	CPrintInfo printInfo;
	printInfo.m_bDocObject = TRUE;
	printInfo.m_dwFlags = PRINTFLAG_DONTACTUALLYPRINT;

	// ask the view about it

	if (!pThis->DoPreparePrinting(pView, &printInfo))
		return E_UNEXPECTED;

	// pnFirstPage and pcPages are allowed to be NULL
	// if NULL, don't return results to caller

	if (pnFirstPage != NULL)
	{
		if (pThis->m_nFirstPage == -1)
			*pnFirstPage = printInfo.GetMinPage();
		else
			*pnFirstPage = pThis->m_nFirstPage;
	}

	if (pcPages != NULL)
	{
		if (printInfo.GetToPage() == 0xFFFF)
			*pcPages = 0xFFFF;
		else
			*pcPages = printInfo.GetToPage() - printInfo.GetFromPage() +1;
	}

	return S_OK;
}

STDMETHODIMP CDocObjectServer::XPrint::Print(
   DWORD grfFlags, DVTARGETDEVICE** ppTD, PAGESET** ppPageSet,
   LPSTGMEDIUM pstgmOptions, LPCONTINUECALLBACK pCallback, LONG nFirstPage,
   LPLONG pcPagesPrinted, LPLONG pnLastPage)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, Print)
	ASSERT_VALID(pThis);
	UNUSED_ALWAYS(pstgmOptions);
	UNUSED_ALWAYS(pnLastPage);

	// try to get out of this without doing any work

	if (pcPagesPrinted == NULL || ppTD == NULL || ppPageSet == NULL)
		return E_POINTER;

	if (*ppTD == NULL)
		return E_INVALIDARG;

	// get initialized

	DVTARGETDEVICE* ptd = *ppTD;
	pThis->m_nFirstPage = nFirstPage;
	*pcPagesPrinted = 0;

	// find the view we need to print

	CView* pView = NULL;
	POSITION pos = pThis->m_pOwner->GetFirstViewPosition();
	if (pos != NULL)
		pView = pThis->m_pOwner->GetNextView(pos);

	if (pView == NULL)
		return E_UNEXPECTED;

	// get default print info
	CPrintInfo printInfo;
	ASSERT(printInfo.m_pPD != NULL);    // must be set
	printInfo.m_bDocObject = TRUE;
	printInfo.m_dwFlags = grfFlags;
	printInfo.m_nOffsetPage = nFirstPage;

	printInfo.m_pPD->m_pd.hDC = _AfxOleCreateDC(*ppTD);
	if (printInfo.m_pPD->m_pd.hDC == NULL)
	{
		if (grfFlags & PRINTFLAG_MAYBOTHERUSER)
			AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
		return E_UNEXPECTED;
	}

	if (pThis->DoPreparePrinting(pView, &printInfo))
	{
		// hDC must be set (did you remember to call DoPreparePrinting?)
		ASSERT(printInfo.m_pPD->m_pd.hDC != NULL);

		// set file to print to if print-to-file selected
		CString strOutput;
		if (grfFlags & PRINTFLAG_PRINTTOFILE)
			strOutput = (LPOLESTR)((BYTE*)ptd + ptd->tdPortNameOffset);

		// if we were to prompt, we'll need to copy info from the
		// user back to the client

		if (grfFlags & PRINTFLAG_PROMPTUSER)
		{
			if (grfFlags & PRINTFLAG_USERMAYCHANGEPRINTER)
			{
				LPDEVNAMES lpDevNames =
					(LPDEVNAMES) GlobalLock(printInfo.m_pPD->m_pd.hDevNames);
				LPDEVMODE lpDevMode =
					(LPDEVMODE) GlobalLock(printInfo.m_pPD->m_pd.hDevMode);

				if (lpDevNames == NULL || lpDevMode == NULL)
					*ppTD = NULL;
				else
					*ppTD = _AfxOleCreateTargetDevice(lpDevNames, lpDevMode);

				GlobalUnlock(printInfo.m_pPD->m_pd.hDevNames);
				GlobalUnlock(printInfo.m_pPD->m_pd.hDevMode);
			}

			// MFC page ranges (for now) only have one PAGERANGE

			LPMALLOC pMalloc = NULL;

			// if the caller didn't supply a buffer, allocate one
			// else, make sure the buffer is big enough

			if (*ppPageSet == NULL)
			{
				HRESULT hrCopying = CoGetMalloc(1, &pMalloc);
				if (FAILED(hrCopying))
					return hrCopying;

				*ppPageSet =
					(PAGESET*) pMalloc->Alloc(sizeof(PAGESET) + sizeof(PAGERANGE));
			}
			else
			{
				if ((*ppPageSet)->cPageRange < 1 ||
					(*ppPageSet)->cbStruct != sizeof(PAGESET))
				{
					return E_INVALIDARG;
				}
			}

			if (*ppPageSet != NULL)
			{
				(*ppPageSet)->cbStruct = sizeof(PAGESET);
				(*ppPageSet)->fOddPages = TRUE;
				(*ppPageSet)->fEvenPages = TRUE;
				(*ppPageSet)->cPageRange = 1;

				(*ppPageSet)->rgPages[0].nFromPage = printInfo.GetFromPage();
				(*ppPageSet)->rgPages[0].nToPage = printInfo.GetToPage();
			}

			RELEASE(pMalloc);

			if (*ppTD == NULL || *ppPageSet == NULL)
				return E_UNEXPECTED;
		}

		// if the client didn't really want to print,
		// we've collected all the information we need
		if (grfFlags & PRINTFLAG_DONTACTUALLYPRINT)
			return S_OK;

		// set up document info and start the document printing process
		CString strTitle;
		CDocument* pDoc = pThis->m_pOwner;
		if (pDoc != NULL)
			strTitle = pDoc->GetTitle();
		else
			pView->GetParentFrame()->GetWindowText(strTitle);

		if (strTitle.GetLength() > 31)
			strTitle.ReleaseBuffer(31);

		DOCINFO docInfo;
		memset(&docInfo, 0, sizeof(DOCINFO));
		docInfo.cbSize = sizeof(DOCINFO);
		docInfo.lpszDocName = strTitle;
		CString strPortName;
		int nFormatID;
		if (strOutput.IsEmpty())
		{
			docInfo.lpszOutput = NULL;
			strPortName = (LPOLESTR)((BYTE*)ptd + ptd->tdPortNameOffset);
			nFormatID = AFX_IDS_PRINTONPORT;
		}
		else
		{
			docInfo.lpszOutput = strOutput;
			AfxGetFileTitle(strOutput,
				strPortName.GetBuffer(_MAX_PATH), _MAX_PATH);
			nFormatID = AFX_IDS_PRINTTOFILE;
		}

		// setup the printing DC
		CDC dcPrint;
		dcPrint.Attach(printInfo.m_pPD->m_pd.hDC);  // attach printer dc
		dcPrint.m_bPrinting = TRUE;
		pThis->DoBeginPrinting(pView, &dcPrint, &printInfo);
		dcPrint.SetAbortProc(_AfxAbortProc);

		// disable main window while printing & init printing status dialog
		AfxGetMainWnd()->EnableWindow(FALSE);

		CString strTemp;

		// start document printing process
		if (dcPrint.StartDoc(&docInfo) == SP_ERROR)
		{
			// enable main window before proceeding
			AfxGetMainWnd()->EnableWindow(TRUE);

			// cleanup and show error message
			pThis->DoEndPrinting(pView, &dcPrint, &printInfo);
			dcPrint.Detach();   // will be cleaned up by CPrintInfo destructor
			AfxMessageBox(AFX_IDP_FAILED_TO_START_PRINT);
			return E_UNEXPECTED;
		}

		// Guarantee values are in the valid range
		UINT nEndPage = printInfo.GetToPage();
		UINT nStartPage = printInfo.GetFromPage();

		if (nEndPage < printInfo.GetMinPage())
			nEndPage = printInfo.GetMinPage();
		if (nEndPage > printInfo.GetMaxPage())
			nEndPage = printInfo.GetMaxPage();

		if (nStartPage < printInfo.GetMinPage())
			nStartPage = printInfo.GetMinPage();
		if (nStartPage > printInfo.GetMaxPage())
			nStartPage = printInfo.GetMaxPage();

		int nStep = (nEndPage >= nStartPage) ? 1 : -1;
		nEndPage = (nEndPage == 0xffff) ? 0xffff : nEndPage + nStep;

		VERIFY(strTemp.LoadString(AFX_IDS_PRINTPAGENUM));

		// begin page printing loop
		BOOL bError = FALSE;
		HRESULT hrContinue = S_OK;

		for (printInfo.m_nCurPage = nStartPage;
			printInfo.m_nCurPage != nEndPage; printInfo.m_nCurPage += nStep)
		{
			// check even/odd filter
			if (printInfo.m_nCurPage % 2 == 1 && !(*ppPageSet)->fOddPages)
				continue;
			if (printInfo.m_nCurPage % 2 == 0 && !(*ppPageSet)->fEvenPages)
				continue;

			// check PAGERANGE if supplied
			if (ppPageSet != NULL && (*ppPageSet)->cPageRange > 0)
			{
				ULONG nCheckRange;
				BOOL bFound = FALSE;
				for (nCheckRange = 0; (nCheckRange < (*ppPageSet)->cPageRange) && !bFound; nCheckRange++)
				{
					if ( ((*ppPageSet)->rgPages[nCheckRange].nFromPage <= (LONG) printInfo.m_nCurPage ) &&
						 ((*ppPageSet)->rgPages[nCheckRange].nToPage >= (LONG) printInfo.m_nCurPage) )
					{
						bFound = TRUE;
					}
				}

				if (!bFound)
					continue;
			}

			pThis->DoPrepareDC(pView, &dcPrint, &printInfo);

			// check for end of print
			if (!printInfo.m_bContinuePrinting)
				break;

			// set up drawing rect to entire page (in logical coordinates)
			printInfo.m_rectDraw.SetRect(0, 0,
				dcPrint.GetDeviceCaps(HORZRES),
				dcPrint.GetDeviceCaps(VERTRES));
			dcPrint.DPtoLP(&printInfo.m_rectDraw);

			// attempt to start the current page
			if (dcPrint.StartPage() < 0)
			{
				bError = TRUE;
				break;
			}

			// must call OnPrepareDC on newer versions of Windows because
			// StartPage now resets the device attributes.
			if (afxData.bMarked4)
				pThis->DoPrepareDC(pView, &dcPrint, &printInfo);

			ASSERT(printInfo.m_bContinuePrinting);

			hrContinue = S_OK;

			if (pCallback != NULL)
			{
				hrContinue = pCallback->FContinuePrinting(printInfo.m_nCurPage,
					printInfo.m_nCurPage + printInfo.m_nOffsetPage, NULL);
			}

			// page successfully started, so now render the page
			pThis->DoPrint(pView, &dcPrint, &printInfo);
			if (dcPrint.EndPage() < 0 ||
				!_AfxAbortProc(dcPrint.m_hDC, 0) ||
				hrContinue != S_OK)
			{
				bError = TRUE;
				break;
			}

			// increment count
			(*pcPagesPrinted)++;
		}

		// cleanup document printing process
		if (!bError)
			dcPrint.EndDoc();
		else
			dcPrint.AbortDoc();

		AfxGetMainWnd()->EnableWindow();    // enable main window

		// clean up after printing
		pThis->DoEndPrinting(pView, &dcPrint, &printInfo);
		dcPrint.Detach();   // will be cleaned up by CPrintInfo destructor

		if (bError)
		{
			if (hrContinue != S_OK)
				return PRINT_E_CANCELLED;
			else
				return E_UNEXPECTED;
		}
	}

	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// IOleDocument interface

STDMETHODIMP_(ULONG) CDocObjectServer::XOleDocument::AddRef()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
	return pThis->m_pOwner->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CDocObjectServer::XOleDocument::Release()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
	return pThis->m_pOwner->ExternalRelease();
}

STDMETHODIMP CDocObjectServer::XOleDocument::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP CDocObjectServer::XOleDocument::CreateView(
   LPOLEINPLACESITE pipsite, LPSTREAM pstm,
   DWORD dwReserved, LPOLEDOCUMENTVIEW* ppview)
{
   METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
   ASSERT_VALID(pThis);

   *ppview = NULL;

   HRESULT hr = E_FAIL;

   if (dwReserved == 0 && pThis->m_pDocSite != NULL)
   {
	  // We only support a single view...so if view site is already
	  // set, fail.
	  if (pThis->m_pViewSite == NULL)
	  {
		 LPOLEDOCUMENTVIEW pView =
			(LPOLEDOCUMENTVIEW)pThis->GetInterface(&IID_IOleDocumentView);
		 ASSERT(pView != NULL);

		 // Set the site for the view
		 hr = pView->SetInPlaceSite(pipsite);
		 if (hr == NOERROR)
		 {
			// Return the IOleDocumentView pointer
			pView->AddRef();
			*ppview = pView;
		 }

		 // If a saved view state is provided, restore the view state
		 if (pstm)
			hr = pView->ApplyViewState(pstm);
	  }
	  else
		 TRACE0("CDocObjectServer::XOleDocument::CreateView view already exists!\n");
   }

   return hr;
}

STDMETHODIMP CDocObjectServer::XOleDocument::GetDocMiscStatus(
   LPDWORD pdwStatus)
{
   METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
   ASSERT_VALID(pThis);
   ASSERT(pdwStatus != NULL);

   // Our implementation of DocObjects can't create multiple views,
   // does not support complex rectangles, supports open editing,
   // and supports read/write to a file. Thus DOCMISC == 0.
   *pdwStatus = 0;

   return NOERROR;
}

STDMETHODIMP CDocObjectServer::XOleDocument::EnumViews(
   LPENUMOLEDOCUMENTVIEWS* ppEnumView, LPOLEDOCUMENTVIEW* ppView)
{
   METHOD_PROLOGUE_EX(CDocObjectServer, OleDocument)
   ASSERT_VALID(pThis);
   ASSERT(ppEnumView != NULL);
   ASSERT(ppView != NULL);

   // We only support a single view
   *ppEnumView = NULL;
   HRESULT hr = QueryInterface(IID_IOleDocumentView, (LPVOID*)ppView);
   return hr;
}

/////////////////////////////////////////////////////////////////////////////
// IOleObject interface

STDMETHODIMP_(ULONG) CDocObjectServer::XOleObject::AddRef()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	return pThis->m_pOwner->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CDocObjectServer::XOleObject::Release()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	return pThis->m_pOwner->ExternalRelease();
}

STDMETHODIMP CDocObjectServer::XOleObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	return pThis->m_pOwner->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP CDocObjectServer::XOleObject::SetClientSite(
	LPOLECLIENTSITE pClientSite)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	HRESULT hr = NOERROR;

	// Perform normal SetClientSite processing.
	hr = pThis->m_pOwner->m_xOleObject.SetClientSite(pClientSite);
	if (hr != S_OK)
		return hr;

	// If we currently have a document site pointer,
	// release it.

	pThis->ReleaseDocSite();

	// Check to see whether this object should act
	// as a document object by querying for
	// IOleDocumentSite.
	if (pClientSite != NULL)
		hr = pClientSite->QueryInterface(IID_IOleDocumentSite,
			(LPVOID*)&pThis->m_pDocSite);
	return hr;
}

STDMETHODIMP CDocObjectServer::XOleObject::GetClientSite(
	LPOLECLIENTSITE* ppClientSite)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetClientSite(ppClientSite);
}

STDMETHODIMP CDocObjectServer::XOleObject::SetHostNames(
	LPCOLESTR lpszContainerApp, LPCOLESTR lpszContainerObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.SetHostNames(lpszContainerApp,
										   lpszContainerObj);
}

STDMETHODIMP CDocObjectServer::XOleObject::Close(DWORD dwSaveOption)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.Close(dwSaveOption);
}

STDMETHODIMP CDocObjectServer::XOleObject::SetMoniker(
	DWORD dwWhichMoniker, LPMONIKER pmk)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.SetMoniker(dwWhichMoniker, pmk);
}

STDMETHODIMP CDocObjectServer::XOleObject::GetMoniker(
	DWORD dwAssign, DWORD dwWhichMoniker, LPMONIKER* ppMoniker)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetMoniker(dwAssign, dwWhichMoniker,
										 ppMoniker);
}

STDMETHODIMP CDocObjectServer::XOleObject::InitFromData(
	LPDATAOBJECT pDataObject, BOOL bCreation, DWORD dwReserved)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.InitFromData(pDataObject, bCreation,
										   dwReserved);
}

STDMETHODIMP CDocObjectServer::XOleObject::GetClipboardData(
	DWORD dwReserved, LPDATAOBJECT* ppDataObject)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetClipboardData(dwReserved,
											   ppDataObject);

}

STDMETHODIMP CDocObjectServer::XOleObject::DoVerb(
	LONG iVerb, LPMSG lpmsg, LPOLECLIENTSITE pActiveSite, LONG lindex,
	HWND hwndParent, LPCRECT lpPosRect)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.DoVerb(iVerb, lpmsg,
		pActiveSite, lindex, hwndParent, lpPosRect);
}

STDMETHODIMP CDocObjectServer::XOleObject::EnumVerbs(
	IEnumOLEVERB** ppenumOleVerb)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.EnumVerbs(ppenumOleVerb);
}

STDMETHODIMP CDocObjectServer::XOleObject::Update()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.Update();
}

STDMETHODIMP CDocObjectServer::XOleObject::IsUpToDate()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.IsUpToDate();
}

STDMETHODIMP CDocObjectServer::XOleObject::GetUserClassID(CLSID* lpClassID)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetUserClassID(lpClassID);
}

STDMETHODIMP CDocObjectServer::XOleObject::GetUserType(
	DWORD dwFormOfType, LPOLESTR* ppszUserType)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetUserType(dwFormOfType, ppszUserType);
}

STDMETHODIMP CDocObjectServer::XOleObject::SetExtent(
	DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);

	// DocObjects ignore SetExtent calls, so returne E_FAIL
	if (pThis->m_pOwner->IsDocObject())
		return E_FAIL;

	// Otherwise, just do the normal processing
	return pThis->m_pOwner->m_xOleObject.SetExtent(dwDrawAspect, lpsizel);
}

STDMETHODIMP CDocObjectServer::XOleObject::GetExtent(
	DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetExtent(dwDrawAspect, lpsizel);
}

STDMETHODIMP CDocObjectServer::XOleObject::Advise(
	LPADVISESINK pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.Advise(pAdvSink, pdwConnection);
}

STDMETHODIMP CDocObjectServer::XOleObject::Unadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.Unadvise(dwConnection);
}

STDMETHODIMP CDocObjectServer::XOleObject::EnumAdvise(
	LPENUMSTATDATA* ppenumStatData)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.EnumAdvise(ppenumStatData);
}

STDMETHODIMP CDocObjectServer::XOleObject::GetMiscStatus(
	DWORD dwAspect, DWORD* pdwStatus)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.GetMiscStatus(dwAspect, pdwStatus);
}

STDMETHODIMP CDocObjectServer::XOleObject::SetColorScheme(LPLOGPALETTE lpLogpal)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleObject)
	ASSERT_VALID(pThis);
	return pThis->m_pOwner->m_xOleObject.SetColorScheme(lpLogpal);
}


/////////////////////////////////////////////////////////////////////////////
// CDocObjectServer diagnostics

#ifdef _DEBUG
void CDocObjectServer::AssertValid() const
{
	ASSERT(m_pOwner != NULL);
	CCmdTarget::AssertValid();
}

void CDocObjectServer::Dump(CDumpContext& dc) const
{
	CCmdTarget::Dump(dc);
	dc << "m_pDocSite = " << m_pDocSite << "\n";
	dc << "m_pViewSite = " << m_pViewSite << "\n";
}
#endif //_DEBUG


/////////////////////////////////////////////////////////////////////////////
// CDocObjectServerItem implementation

IMPLEMENT_DYNAMIC(CDocObjectServerItem, COleServerItem)

CDocObjectServerItem::CDocObjectServerItem(COleServerDoc* pServerDoc, BOOL bAutoDelete)
	: COleServerItem(pServerDoc, bAutoDelete)
{
}

CDocObjectServerItem::~CDocObjectServerItem()
{
}

void CDocObjectServerItem::OnDoVerb(LONG iVerb)
{
   COleServerDoc* pDoc = GetDocument();
   ASSERT_VALID(pDoc);

   if (pDoc->IsDocObject() && (iVerb == OLEIVERB_INPLACEACTIVATE || iVerb == OLEIVERB_SHOW) )
	  OnShow();
   else
	  COleServerItem::OnDoVerb(iVerb);
}

void CDocObjectServerItem::OnHide()
{
   COleServerDoc* pDoc = GetDocument();
   ASSERT_VALID(pDoc);

   if (pDoc->IsDocObject())
	  AfxThrowOleException(OLEOBJ_E_INVALIDVERB);
   else
	  COleServerItem::OnHide();
}

void CDocObjectServerItem::OnOpen()
{
   COleServerDoc* pDoc = GetDocument();
   ASSERT_VALID(pDoc);

   if (pDoc->IsDocObject())
	  pDoc->ActivateDocObject();
   else
	  COleServerItem::OnOpen();
}

void CDocObjectServerItem::OnShow()
{
   COleServerDoc* pDoc = GetDocument();
   ASSERT_VALID(pDoc);

   if (pDoc->IsDocObject())
	  pDoc->ActivateDocObject();
   else
	  COleServerItem::OnShow();
}

#ifdef _DEBUG
void CDocObjectServerItem::AssertValid() const
{
	COleServerItem::AssertValid();
}

void CDocObjectServerItem::Dump(CDumpContext& dc) const
{
	COleServerItem::Dump(dc);
}
#endif
