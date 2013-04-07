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

#ifdef AFX_PRINT_SEG
#pragma code_seg(AFX_PRINT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

BOOL CALLBACK _AfxPreviewCloseProc(CFrameWnd* pFrameWnd);

/////////////////////////////////////////////////////////////////////////////
// CPrintPreviewState helper structure

CPrintPreviewState::CPrintPreviewState()
{
	// set defaults
	nIDMainPane = AFX_IDW_PANE_FIRST;
	dwStates = AFX_CONTROLBAR_MASK(AFX_IDW_STATUS_BAR);
						// status bar visible if available
	lpfnCloseProc = _AfxPreviewCloseProc;
						// set frame hook so closing the frame window
						//  when in preview state will just end the mode
	hMenu = NULL;
	pViewActiveOld = NULL;
	hAccelTable = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CView's OnPrintPreview.  Here to force linkage

void CView::OnFilePrintPreview()
{
	// In derived classes, implement special window handling here
	// Be sure to Unhook Frame Window close if hooked.

	// must not create this on the frame.  Must outlive this function
	CPrintPreviewState* pState = new CPrintPreviewState;

	// DoPrintPreview's return value does not necessarily indicate that
	// Print preview succeeded or failed, but rather what actions are necessary
	// at this point.  If DoPrintPreview returns TRUE, it means that
	// OnEndPrintPreview will be (or has already been) called and the
	// pState structure will be/has been deleted.
	// If DoPrintPreview returns FALSE, it means that OnEndPrintPreview
	// WILL NOT be called and that cleanup, including deleting pState
	// must be done here.

	if (!DoPrintPreview(AFX_IDD_PREVIEW_TOOLBAR, this,
							RUNTIME_CLASS(CPreviewView), pState))
	{
		// In derived classes, reverse special window handling here for
		// Preview failure case

		TRACE0("Error: DoPrintPreview failed.\n");
		AfxMessageBox(AFX_IDP_COMMAND_FAILURE);
		delete pState;      // preview failed to initialize, delete State now
	}
}

BOOL CView::DoPrintPreview(UINT nIDResource, CView* pPrintView,
	CRuntimeClass* pPreviewViewClass, CPrintPreviewState* pState)
{
	ASSERT_VALID_IDR(nIDResource);
	ASSERT_VALID(pPrintView);
	ASSERT(pPreviewViewClass != NULL);
	ASSERT(pPreviewViewClass->IsDerivedFrom(RUNTIME_CLASS(CPreviewView)));
	ASSERT(pState != NULL);

	CFrameWnd* pParent = STATIC_DOWNCAST(CFrameWnd, AfxGetMainWnd());
	ASSERT_VALID(pParent);

	CCreateContext context;
	context.m_pCurrentFrame = pParent;
	context.m_pCurrentDoc = GetDocument();
	context.m_pLastView = this;

	// Create the preview view object
	CPreviewView* pView = (CPreviewView*)pPreviewViewClass->CreateObject();
	if (pView == NULL)
	{
		TRACE0("Error: Failed to create preview view.\n");
		return FALSE;
	}
	ASSERT_KINDOF(CPreviewView, pView);
	pView->m_pPreviewState = pState;        // save pointer

	pParent->OnSetPreviewMode(TRUE, pState);    // Take over Frame Window

	// Create the toolbar from the dialog resource
	pView->m_pToolBar = new CDialogBar;
	if (!pView->m_pToolBar->Create(pParent, MAKEINTRESOURCE(nIDResource),
		CBRS_TOP, AFX_IDW_PREVIEW_BAR))
	{
		TRACE0("Error: Preview could not create toolbar dialog.\n");
		pParent->OnSetPreviewMode(FALSE, pState);   // restore Frame Window
		delete pView->m_pToolBar;       // not autodestruct yet
		pView->m_pToolBar = NULL;
		pView->m_pPreviewState = NULL;  // do not delete state structure
		delete pView;
		return FALSE;
	}
	pView->m_pToolBar->m_bAutoDelete = TRUE;    // automatic cleanup

	// Create the preview view as a child of the App Main Window.  This
	// is a sibling of this view if this is an SDI app.  This is NOT a sibling
	// if this is an MDI app.

	if (!pView->Create(NULL, NULL, AFX_WS_DEFAULT_VIEW,
		CRect(0,0,0,0), pParent, AFX_IDW_PANE_FIRST, &context))
	{
		TRACE0("Error: couldn't create preview view for frame.\n");
		pParent->OnSetPreviewMode(FALSE, pState);   // restore Frame Window
		pView->m_pPreviewState = NULL;  // do not delete state structure
		delete pView;
		return FALSE;
	}

	// Preview window shown now

	pState->pViewActiveOld = pParent->GetActiveView();
	CView* pActiveView = pParent->GetActiveFrame()->GetActiveView();
	if (pActiveView != NULL)
		pActiveView->OnActivateView(FALSE, pActiveView, pActiveView);

	if (!pView->SetPrintView(pPrintView))
	{
		pView->OnPreviewClose();
		return TRUE;            // signal that OnEndPrintPreview was called
	}

	pParent->SetActiveView(pView);  // set active view - even for MDI

	// update toolbar and redraw everything
	pView->m_pToolBar->SendMessage(WM_IDLEUPDATECMDUI, (WPARAM)TRUE);
	pParent->RecalcLayout();            // position and size everything
	pParent->UpdateWindow();

	return TRUE;
}

BOOL CALLBACK _AfxPreviewCloseProc(CFrameWnd* pFrameWnd)
{
	ASSERT_VALID(pFrameWnd);

	CPreviewView* pView = (CPreviewView*) pFrameWnd->GetDlgItem(AFX_IDW_PANE_FIRST);
	ASSERT_KINDOF(CPreviewView, pView);

	pView->OnPreviewClose();
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Preview View

BEGIN_MESSAGE_MAP(CPreviewView, CScrollView)
	//{{AFX_MSG_MAP(CPreviewView)
	ON_WM_SIZE()        // overriding CScrollView
	ON_WM_CREATE()

	ON_COMMAND(AFX_ID_PREVIEW_CLOSE, OnPreviewClose)
	ON_COMMAND(AFX_ID_PREVIEW_NUMPAGE, OnNumPageChange)
	ON_COMMAND(AFX_ID_PREVIEW_NEXT, OnNextPage)
	ON_COMMAND(AFX_ID_PREVIEW_PREV, OnPrevPage)
	ON_COMMAND(AFX_ID_PREVIEW_PRINT, OnPreviewPrint)
	ON_COMMAND(AFX_ID_PREVIEW_ZOOMIN, OnZoomIn)
	ON_COMMAND(AFX_ID_PREVIEW_ZOOMOUT, OnZoomOut)

	ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_NUMPAGE, OnUpdateNumPageChange)
	ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_NEXT, OnUpdateNextPage)
	ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_PREV, OnUpdatePrevPage)
	ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_ZOOMIN, OnUpdateZoomIn)
	ON_UPDATE_COMMAND_UI(AFX_ID_PREVIEW_ZOOMOUT, OnUpdateZoomOut)

	ON_WM_VSCROLL()
	ON_WM_HSCROLL()
	ON_WM_LBUTTONDOWN()
	ON_WM_ERASEBKGND()
	ON_WM_SETCURSOR()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CPreviewView::CPreviewView()
{
	m_pPrintView = NULL;
	m_pOrigView = NULL;
	m_pPreviewInfo = NULL;
	m_pPreviewDC = NULL;
	m_pPreviewState = NULL;
	m_hMagnifyCursor = NULL;
	m_bPageNumDisplayed = FALSE;
	m_nZoomState = ZOOM_OUT;

	// default to pointing to embedded array.  Allows for 2 pages
	m_pPageInfo = m_pageInfoArray;
	m_nMaxPages = 2;

	// initialize CScrollView members
	m_bCenter = TRUE;                   // Center Zoomed output in Scrollview
	m_nMapMode = MM_TEXT;
}

CPreviewView::PAGE_INFO::PAGE_INFO()
{
}

CPreviewView::~CPreviewView()
{
	m_dcPrint.Detach();         // print DC is deleted by CPrintInfo destructor

	delete m_pPreviewInfo;      // get rid of preview info
	delete m_pPreviewState;     // Get rid of preview state
	delete m_pPreviewDC;        // Get rid of preview DC object

	if (m_hMagnifyCursor != NULL)
	{
		// make sure that m_hMagnifyCursor isn't the current cursor when we destroy it
		::SetCursor(::LoadCursor(NULL, IDC_ARROW));
		DestroyCursor(m_hMagnifyCursor);
	}
}

int CPreviewView::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	int retVal = CView::OnCreate(lpCreateStruct);
	if (retVal == -1)
		return -1;      // if -1 bag out

	CCreateContext* pContext = (CCreateContext*)lpCreateStruct->lpCreateParams;

	m_pOrigView = pContext->m_pLastView;
	ASSERT(m_pOrigView != NULL);
	ASSERT_KINDOF(CView, m_pOrigView);

	return retVal;
}

BOOL CPreviewView::SetPrintView(CView* pPrintView)
{
	ASSERT_VALID(pPrintView);

	m_pPrintView = pPrintView;

	// allocate preview info
	m_pPreviewInfo = new CPrintInfo;
	m_pPreviewInfo->m_pPD->SetHelpID(AFX_IDD_PRINTSETUP);
	m_pPreviewInfo->m_pPD->m_pd.Flags |= PD_PRINTSETUP;
	m_pPreviewInfo->m_pPD->m_pd.Flags &= ~PD_RETURNDC;

	m_pPreviewInfo->m_bPreview = TRUE;  // signal that this is preview
	ASSERT(m_pPreviewInfo->m_pPD != NULL);

	m_pPreviewDC = new CPreviewDC;      // must be created before any
										// possible error returns

	if (!m_pPrintView->OnPreparePrinting(m_pPreviewInfo))
		return FALSE;

#ifdef _DEBUG
	if (m_pPreviewInfo->m_pPD->m_pd.hDC == NULL)
	{
		TRACE0("Error: hDC not set for printing --\n");
		TRACE0("\tDid you remember to call DoPreparePrinting?\n");
		ASSERT(FALSE);      // common mistake gets trapped here
	}
#endif //_DEBUG

	m_dcPrint.Attach(m_pPreviewInfo->m_pPD->m_pd.hDC);
	m_pPreviewDC->SetAttribDC(m_pPreviewInfo->m_pPD->m_pd.hDC);
	m_pPreviewDC->m_bPrinting = TRUE;
	m_dcPrint.m_bPrinting = TRUE;

	m_dcPrint.SaveDC();     // Save pristine state of DC

	HDC hDC = ::GetDC(m_hWnd);
	m_pPreviewDC->SetOutputDC(hDC);
	m_pPrintView->OnBeginPrinting(m_pPreviewDC, m_pPreviewInfo);
	m_pPreviewDC->ReleaseOutputDC();
	::ReleaseDC(m_hWnd, hDC);

	m_dcPrint.RestoreDC(-1);    // restore to untouched state

	// Get Pixels per inch from Printer
	m_sizePrinterPPI.cx = m_dcPrint.GetDeviceCaps(LOGPIXELSX);
	m_sizePrinterPPI.cy = m_dcPrint.GetDeviceCaps(LOGPIXELSY);

	m_nPages = m_pPreviewInfo->m_nNumPreviewPages;
	if (m_nPages == 0)
		m_nPages = 1;
	else if (m_nPages > m_nMaxPages)
		m_nPages = m_nMaxPages;     // Sanity Check!

	m_nZoomOutPages = m_nPages;

	SetScrollSizes(MM_TEXT, CSize(1, 1));   // initialize mapping mode only

	if (m_pPreviewInfo->GetMaxPage() < 0x8000 &&
		m_pPreviewInfo->GetMaxPage() - m_pPreviewInfo->GetMinPage() <= 32767U)
	{
		SCROLLINFO info;
		info.fMask = SIF_PAGE|SIF_RANGE;
		info.nMin = m_pPreviewInfo->GetMinPage();
		info.nMax = m_pPreviewInfo->GetMaxPage();
		info.nPage = 1;
		if (!SetScrollInfo(SB_VERT, &info, FALSE))
			SetScrollRange(SB_VERT, info.nMin, info.nMax, FALSE);
	}
	else
		ShowScrollBar(SB_VERT, FALSE);      // if no range specified, or too
											// large don't show

	SetCurrentPage(m_pPreviewInfo->m_nCurPage, TRUE);
	return TRUE;
}

void CPreviewView::OnSize(UINT nType, int cx, int cy)
{
	// CScrollView handles everything if zoomed in.
	if (m_nZoomState == ZOOM_OUT)
	{
		// Force recalc of scale ratios on next draw
		for (UINT i = 0; i < m_nMaxPages; i++)
			m_pPageInfo[i].sizeScaleRatio.cx = 0;           // zero scale ratios

		CView::OnSize(nType, cx, cy);       // No scroll functionality
	}
	else
	{
		// adjust scroll size to size of page
		m_pageDev.cx = cx;
		m_pageDev.cy = cy;
		m_lineDev.cx = cx / 10;
		m_lineDev.cy = cy / 10;
		CScrollView::OnSize(nType, cx, cy);
	}
}

void CPreviewView::OnActivateView(BOOL bActivate, CView*, CView*)
{
	if (bActivate)
	{
		CWnd* pFocusWnd = GetFocus();
		if (pFocusWnd == NULL ||
			(m_pToolBar != NULL && !m_pToolBar->IsChild(pFocusWnd)))
		{
			// focus is not already on a toolbar button - set it to one
			m_pToolBar->GetDlgItem(AFX_ID_PREVIEW_PRINT)->SetFocus();
		}
	}
}

void CPreviewView::OnPreviewClose()
{
	m_pToolBar->DestroyWindow();
	m_pToolBar = NULL;

	m_pPreviewInfo->m_nCurPage = m_nCurrentPage;
	m_pOrigView->OnEndPrintPreview(m_pPreviewDC, m_pPreviewInfo,
									CPoint(0, 0), this);
}

#define PREVIEW_MARGIN  8
#define PREVIEW_PAGEGAP 8

// Return is actually the fraction cx/cy. Simply using CSize for convenience
CSize CPreviewView::CalcScaleRatio(CSize screenSize, CSize actualSize)
{
	// Test ratio based on vertical dimension to see if it is the one to use
	int nNum = screenSize.cy;
	int nDen = actualSize.cy;

	// If scaled width too large, choose width as primary dimension
	if (MulDiv(actualSize.cx, nNum, nDen) > screenSize.cx)
	{
		// wrong ratio--base on width
		nNum = screenSize.cx;
		nDen = actualSize.cx;
	}
	CSize ratio(nNum, nDen);
	return ratio;
}

// Position Page...
// Generate a Screen MM_TEXT rectangle to enclose each page.  Dimensions
// of the rectangle must be 1 pixel Above and Left of the top/left corner
// of the page and the rectangle width and height must be THREE pixels
// larger than page in order to provide the correct placement of the
// two pixel border.
//
// This routine is called once for each page with the preview DC set up for
// that page

void CPreviewView::PositionPage(UINT nPage)
{
	CSize windowSize = CalcPageDisplaySize();

	VERIFY(m_dcPrint.Escape(GETPHYSPAGESIZE, 0, NULL,
			(LPVOID)&m_pPageInfo[nPage].sizeUnscaled));

	CSize* pSize = &m_pPageInfo[nPage].sizeUnscaled;

	// Convert page size to screen coordinates
	pSize->cx = MulDiv(pSize->cx, afxData.cxPixelsPerInch, m_sizePrinterPPI.cx);
	pSize->cy = MulDiv(pSize->cy, afxData.cyPixelsPerInch, m_sizePrinterPPI.cy);

	m_pPageInfo[nPage].sizeZoomOutRatio = CalcScaleRatio(windowSize, *pSize);

	SetScaledSize(nPage);
}

CSize CPreviewView::CalcPageDisplaySize()
	// calculate the current page size
	//  set 'm_nSecondPageOffset' to start of second page
	// return size of current page less margins
{
	CSize windowSize, scrollSize;
	GetTrueClientSize(windowSize, scrollSize);

	// subtract out vertical scrollbar if zoomed out and page range is known
	// and there is more than one page.
	if (m_nZoomState == ZOOM_OUT && (m_pPreviewInfo->GetMaxPage() != 0xffff) &&
		(m_pPreviewInfo->GetMaxPage() - m_pPreviewInfo->GetMinPage() != 0))
		windowSize.cx -= scrollSize.cx;

	m_nSecondPageOffset = (windowSize.cx - PREVIEW_MARGIN) / 2;

	windowSize.cx = (m_nPages == 2) ? (windowSize.cx - 3*PREVIEW_MARGIN) / 2 :
									windowSize.cx - 2*PREVIEW_MARGIN;

	windowSize.cy -= 2*PREVIEW_MARGIN;
	return windowSize;
}

void CPreviewView::SetScaledSize(UINT nPage)
{
	CSize* pSize = &m_pPageInfo[nPage].sizeUnscaled;
	CSize* pRatio = &m_pPageInfo[nPage].sizeScaleRatio;
	CSize* pZoomOutRatio = &m_pPageInfo[nPage].sizeZoomOutRatio;
	CSize windowSize = CalcPageDisplaySize();
	BOOL bPaperLarger = pZoomOutRatio->cx < pZoomOutRatio->cy;
		// whether the paper is larger than the screen, or vice versa

	switch (m_nZoomState)
	{
	case ZOOM_OUT:
		*pRatio = *pZoomOutRatio;
		break;

	case ZOOM_MIDDLE:
		// the middle zoom state is a ratio between cx/cy and
		// 1/1 (or cy/cy).  It is, therefore:
		//
		// (cx + cy)/2
		// -----------
		//     cy
		//
		// if the paper is larger than the screen, or
		//
		// (3*cx - cy)/2
		// -------------
		//      cy
		//
		// if the paper is smaller than the screen.
		if (bPaperLarger)
		{
			pRatio->cy = pZoomOutRatio->cy;
			pRatio->cx = (pZoomOutRatio->cx + pRatio->cy) / 2;
		}
		else
		{
			pRatio->cy = pZoomOutRatio->cy;
			pRatio->cx = (3*pZoomOutRatio->cx - pRatio->cy) / 2;
		}
		break;

	case ZOOM_IN:
		if (bPaperLarger)
			pRatio->cx = pRatio->cy = 1;
		else
		{
			// if the paper is smaller than the screen space we're displaying
			// it in, then using a ratio of 1/1 will result in a smaller image
			// on the screen, not a larger one. To get a larger image in this
			// case we double the zoom out ratio.
			pRatio->cy = pZoomOutRatio->cy;
			pRatio->cx = 2*pZoomOutRatio->cx - pZoomOutRatio->cy;
		}
		break;

	default:
		ASSERT(FALSE);
	}

	// Convert to scaled size
	CSize scaledSize;
	scaledSize.cx = MulDiv(pSize->cx, pRatio->cx, pRatio->cy);
	scaledSize.cy = MulDiv(pSize->cy, pRatio->cx, pRatio->cy);

	CRect* pRect = &m_pPageInfo[nPage].rectScreen;
	pRect->SetRect(PREVIEW_MARGIN, PREVIEW_MARGIN,
				   scaledSize.cx + PREVIEW_MARGIN + 3,
				   scaledSize.cy + PREVIEW_MARGIN + 3);

	if (m_nZoomState == ZOOM_OUT)
	{
		pRect->OffsetRect((windowSize.cx - pRect->Size().cx) / 2 - 1,
						  (windowSize.cy - pRect->Size().cy) / 2 - 1);

		if (nPage == 1)
			pRect->OffsetRect(m_nSecondPageOffset, 0);
	}
	else
	{
		// set up scroll size

		SetScrollSizes(MM_TEXT, pRect->Size() +
				CSize(PREVIEW_MARGIN * 2, PREVIEW_MARGIN * 2), windowSize);
	}
}

// Only use the PrepareDC from CScrollView if we are zoomed in
void CPreviewView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(pDC);

	if (m_nZoomState == ZOOM_OUT)
		CView::OnPrepareDC(pDC, pInfo);
	else if (m_pPageInfo[0].sizeScaleRatio.cx != 0)
		CScrollView::OnPrepareDC(pDC, pInfo);
}

BOOL CPreviewView::OnEraseBkgnd(CDC* pDC)
{
	ASSERT_VALID(pDC);

	// Fill background with APPWORKSPACE
	CBrush backBrush(GetSysColor(COLOR_APPWORKSPACE));
	CBrush* pOldBrush = pDC->SelectObject(&backBrush);
	CRect rect;
	pDC->GetClipBox(&rect);     // Erase the area needed

	pDC->PatBlt(rect.left, rect.top, rect.Width(), rect.Height(), PATCOPY);
	pDC->SelectObject(pOldBrush);
	return TRUE;
}

void CPreviewView::OnDraw(CDC* pDC)
{
	ASSERT_VALID(pDC);

	// don't do anything if not fully initialized
	if (m_pPrintView == NULL || m_dcPrint.m_hDC == NULL)
		return;

	CPoint ViewportOrg = pDC->GetViewportOrg();

	CPen rectPen;
	rectPen.CreatePen(PS_SOLID, 2, GetSysColor(COLOR_WINDOWFRAME));
	CPen shadowPen;
	shadowPen.CreatePen(PS_SOLID, 3, GetSysColor(COLOR_BTNSHADOW));

	m_pPreviewInfo->m_bContinuePrinting = TRUE;     // do this once each paint

	for (UINT nPage = 0; nPage < m_nPages; nPage++)
	{
		int nSavedState = m_dcPrint.SaveDC();       // Save pristine state of DC

		// Use paint DC for print preview output
		m_pPreviewDC->SetOutputDC(pDC->GetSafeHdc());

		m_pPreviewInfo->m_nCurPage = m_nCurrentPage + nPage;

		// Only call PrepareDC if within page range, otherwise use default
		// rect to draw page rectangle
		if (m_nCurrentPage + nPage <= m_pPreviewInfo->GetMaxPage())
			m_pPrintView->OnPrepareDC(m_pPreviewDC, m_pPreviewInfo);

		// Set up drawing rect to entire page (in logical coordinates)
		m_pPreviewInfo->m_rectDraw.SetRect(0, 0,
			m_pPreviewDC->GetDeviceCaps(HORZRES),
			m_pPreviewDC->GetDeviceCaps(VERTRES));
		m_pPreviewDC->DPtoLP(&m_pPreviewInfo->m_rectDraw);

		// Draw empty page on screen

		pDC->SaveDC();          // save the output dc state

		CSize* pRatio = &m_pPageInfo[nPage].sizeScaleRatio;
		CRect* pRect = &m_pPageInfo[nPage].rectScreen;

		if (pRatio->cx == 0)
		{   // page position has not been determined
			PositionPage(nPage);    // compute page position
			if (m_nZoomState != ZOOM_OUT)
				ViewportOrg = -GetDeviceScrollPosition();
		}

		pDC->SetMapMode(MM_TEXT);   // Page Rectangle is in screen device coords
		pDC->SetViewportOrg(ViewportOrg);
		pDC->SetWindowOrg(0, 0);

		pDC->SelectStockObject(HOLLOW_BRUSH);
		pDC->SelectObject(&rectPen);
		pDC->Rectangle(pRect);

		pDC->SelectObject(&shadowPen);

		pDC->MoveTo(pRect->right + 1, pRect->top + 3);
		pDC->LineTo(pRect->right + 1, pRect->bottom + 1);
		pDC->MoveTo(pRect->left + 3, pRect->bottom + 1);
		pDC->LineTo(pRect->right + 1, pRect->bottom + 1);

		// erase background to white (most paper is white)
		CRect rectFill = *pRect;
		rectFill.left += 1;
		rectFill.top += 1;
		rectFill.right -= 2;
		rectFill.bottom -= 2;
		::FillRect(pDC->m_hDC, rectFill, (HBRUSH)GetStockObject(WHITE_BRUSH));

		pDC->RestoreDC(-1);     // restore to synchronized state

		if (!m_pPreviewInfo->m_bContinuePrinting ||
				m_nCurrentPage + nPage > m_pPreviewInfo->GetMaxPage())
		{
			m_pPreviewDC->ReleaseOutputDC();
			m_dcPrint.RestoreDC(nSavedState);   // restore to untouched state

			// if the first page is not displayable, back up one page
			// but never go below 1
			if (nPage == 0 && m_nCurrentPage > 1)
				SetCurrentPage(m_nCurrentPage - 1, TRUE);
			break;
		}

		// Display page number
		OnDisplayPageNumber(m_nCurrentPage, nPage + 1);

		// Set scale ratio for this page
		m_pPreviewDC->SetScaleRatio(pRatio->cx, pRatio->cy);

		CSize PrintOffset;
		VERIFY(m_pPreviewDC->Escape(GETPRINTINGOFFSET, 0, NULL, (LPVOID)&PrintOffset));
		m_pPreviewDC->PrinterDPtoScreenDP((LPPOINT)&PrintOffset);
		PrintOffset += (CSize)pRect->TopLeft();
		PrintOffset += CSize(1, 1);
		PrintOffset += (CSize)ViewportOrg;  // For Scrolling

		m_pPreviewDC->SetTopLeftOffset(PrintOffset);

		m_pPreviewDC->ClipToPage();
		m_pPrintView->OnPrint(m_pPreviewDC, m_pPreviewInfo);

		m_pPreviewDC->ReleaseOutputDC();

		m_dcPrint.RestoreDC(nSavedState);   // restore to untouched state

	}

	rectPen.DeleteObject();
	shadowPen.DeleteObject();
}

void CPreviewView::OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	if (m_nZoomState != ZOOM_OUT)
		CScrollView::OnHScroll(nSBCode, nPos, pScrollBar);
}

void CPreviewView::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
	if (m_nZoomState != ZOOM_OUT)
	{
		CScrollView::OnVScroll(nSBCode, nPos, pScrollBar);
		return;
	}

	switch (nSBCode)
	{
	case SB_BOTTOM:
		SetCurrentPage(m_pPreviewInfo->GetMaxPage(), TRUE);
		break;

	case SB_TOP:
		SetCurrentPage(m_pPreviewInfo->GetMinPage(), TRUE);
		break;

	case SB_PAGEDOWN:
		SetCurrentPage(m_nCurrentPage +
			(m_pPreviewInfo->GetMaxPage() - m_pPreviewInfo->GetMinPage() + 9) / 10, TRUE);
		break;

	case SB_PAGEUP:
		SetCurrentPage(m_nCurrentPage -
			(m_pPreviewInfo->GetMaxPage() - m_pPreviewInfo->GetMinPage() + 9) / 10, TRUE);
		break;

	case SB_LINEDOWN:
		SetCurrentPage(m_nCurrentPage + 1, TRUE);
		break;

	case SB_LINEUP:
		SetCurrentPage(m_nCurrentPage - 1, TRUE);
		break;

	case SB_THUMBPOSITION:
		SetCurrentPage(nPos, TRUE);
		break;
	}
}

void CPreviewView::OnNumPageChange()
{
	ASSERT(m_nPages == 1 || m_nPages == 2);
	m_nPages = 3 - m_nPages;    // Toggle between 1 and 2
	AfxGetApp()->m_nNumPreviewPages = m_nPages;
	m_nZoomOutPages = m_nPages;

	// Just do this to set the status correctly and invalidate
	SetCurrentPage(m_nCurrentPage, TRUE);
}

void CPreviewView::OnNextPage()
{
	SetCurrentPage(m_nCurrentPage + 1, TRUE);
}

void CPreviewView::OnPrevPage()
{
	SetCurrentPage(m_nCurrentPage - 1, TRUE);
}

void CPreviewView::OnPreviewPrint()
{
	OnPreviewClose();               // force close of Preview

	// cause print (can be overridden by catching the command)
	CWnd* pMainWnd = AfxGetThread()->m_pMainWnd;
	ASSERT_VALID(pMainWnd);
	pMainWnd->SendMessage(WM_COMMAND, ID_FILE_PRINT);
}

// Finds page pointed to and convert to 1:1 screen device units
BOOL CPreviewView::FindPageRect(CPoint& point, UINT& nPage)
{
	if (m_nZoomState != ZOOM_OUT)
		point += (CSize)GetDeviceScrollPosition();

	for (nPage = 0; nPage < m_nPages; nPage++)
	{
		if (m_pPageInfo[nPage].rectScreen.PtInRect(point))
		{
			// adjust point for page position
			point -= (CSize)m_pPageInfo[nPage].rectScreen.TopLeft();

			// convert to 1:1
			point.x = MulDiv(point.x, m_pPageInfo[nPage].sizeScaleRatio.cy,
									m_pPageInfo[nPage].sizeScaleRatio.cx);
			point.y = MulDiv(point.y, m_pPageInfo[nPage].sizeScaleRatio.cy,
									m_pPageInfo[nPage].sizeScaleRatio.cx);
			return TRUE;
		}
	}
	return FALSE;
}


void CPreviewView::OnLButtonDown(UINT, CPoint point)
{
	UINT nPage;
	if (!FindPageRect(point, nPage))
		return;                         // Didn't click on a page

	// Set new zoom state
	SetZoomState((m_nZoomState == ZOOM_IN) ? ZOOM_OUT : m_nZoomState + 1,
								nPage, point);
}

void CPreviewView::SetZoomState(UINT nNewState, UINT nPage, CPoint point)
{
	if (m_nZoomState != nNewState)
	{
		m_nZoomState = nNewState;
		DoZoom(nPage, point);
	}
}

void CPreviewView::OnZoomIn()
{
	if (m_nZoomState != ZOOM_IN)
		SetZoomState(m_nZoomState + 1, 0, CPoint(0, 0));
}

void CPreviewView::OnZoomOut()
{
	if (m_nZoomState != ZOOM_OUT)
		SetZoomState(m_nZoomState - 1, 0, CPoint(0, 0));
}

// Actual zoom code.
void CPreviewView::DoZoom(UINT nPage, CPoint point)
{
	if (m_nZoomState == ZOOM_OUT)
	{
		// taking over scroll bars
		m_nPages = m_nZoomOutPages;
		ShowScrollBar(SB_HORZ, FALSE);      //hide the horizontal bar

		BOOL bShowBar = m_pPreviewInfo->GetMaxPage() < 0x8000 &&
			m_pPreviewInfo->GetMaxPage() -
			m_pPreviewInfo->GetMinPage() <= 32767U;

		ShowScrollBar(SB_VERT, bShowBar);       //Show the vertical bar

		if (bShowBar)
		{
			SCROLLINFO info;
			info.fMask = SIF_PAGE|SIF_RANGE;
			info.nMin = m_pPreviewInfo->GetMinPage();
			info.nMax = m_pPreviewInfo->GetMaxPage();
			info.nPage = 1;
			if (!SetScrollInfo(SB_VERT, &info, FALSE))
				SetScrollRange(SB_VERT, info.nMin, info.nMax, FALSE);
		}

		SetCurrentPage(m_nCurrentPage, TRUE);
	}
	else
	{
		m_nPages = 1;       // only one page in zoomed states

		m_pPageInfo[0].sizeZoomOutRatio = m_pPageInfo[nPage].sizeZoomOutRatio;
		m_pPageInfo[0].sizeUnscaled = m_pPageInfo[nPage].sizeUnscaled;

		// Sets the printer page
		SetCurrentPage(m_nCurrentPage + nPage, FALSE);

		SetScaledSize(0);

		CSize* pRatio = &m_pPageInfo[nPage].sizeScaleRatio;

		// convert Hit Point from screen 1:1
		point.x = MulDiv(point.x, pRatio->cx, pRatio->cy);
		point.y = MulDiv(point.y, pRatio->cx, pRatio->cy);

		// Adjust point for page position
		point += (CSize)m_pPageInfo[0].rectScreen.TopLeft();

		// Scroll to center
		CenterOnPoint(point);
	}
}

void CPreviewView::SetCurrentPage(UINT nPage, BOOL bClearRatios)
{
	m_nCurrentPage = nPage;
	if (m_nCurrentPage > m_pPreviewInfo->GetMaxPage())
		m_nCurrentPage = m_pPreviewInfo->GetMaxPage();
	if (m_nCurrentPage < m_pPreviewInfo->GetMinPage())
		m_nCurrentPage = m_pPreviewInfo->GetMinPage();


	if (m_nZoomState == ZOOM_OUT)
		SetScrollPos(SB_VERT, m_nCurrentPage);

	if (bClearRatios)
	{
		// Force Recalc of layout
		for (UINT i = 0; i < m_nMaxPages; i++)
			m_pPageInfo[i].sizeScaleRatio.cx = 0;           // zero scale ratios
	}

	Invalidate(TRUE);
}

void CPreviewView::OnDisplayPageNumber(UINT nPage, UINT nPagesDisplayed)
{
	UINT nEndPage = nPage + nPagesDisplayed - 1;

	CFrameWnd* pParent = (CFrameWnd*)AfxGetThread()->m_pMainWnd;
	ASSERT_VALID(pParent);
	ASSERT_KINDOF(CFrameWnd, pParent);

	int nSubString = (nPagesDisplayed == 1) ? 0 : 1;

	CString s;
	if (AfxExtractSubString(s, m_pPreviewInfo->m_strPageDesc, nSubString))
	{
		TCHAR szBuf[80];
		if (nSubString == 0)
			wsprintf(szBuf, s, nPage);
		else
			wsprintf(szBuf, s, nPage, nEndPage);
		pParent->SendMessage(WM_SETMESSAGESTRING, 0, (LPARAM)(LPVOID)szBuf);
	}
	else
	{
		TRACE1("Malformed Page Description string. Could not get string %d.\n",
			nSubString);
	}
}


void CPreviewView::OnUpdateNumPageChange(CCmdUI* pCmdUI)
{
	// set text of button to opposite of current state
	CString text;
	UINT nPages = m_nZoomState == ZOOM_OUT ? m_nPages : m_nZoomOutPages;
	VERIFY(text.LoadString(nPages == 1 ? AFX_IDS_TWOPAGE : AFX_IDS_ONEPAGE));
	pCmdUI->SetText(text);

	// enable it only if valid to display another page and not zoomed
	pCmdUI->Enable(m_nZoomState == ZOOM_OUT && m_nMaxPages != 1 &&
		(m_pPreviewInfo->GetMaxPage() > 1 || m_nPages > 1));
}

void CPreviewView::OnUpdateNextPage(CCmdUI* pCmdUI)
{
	// enable if not showing last page
	pCmdUI->Enable(m_nCurrentPage+m_nPages-1 < m_pPreviewInfo->GetMaxPage());
}

void CPreviewView::OnUpdatePrevPage(CCmdUI* pCmdUI)
{
	// enable if not showing First page
	pCmdUI->Enable(m_nCurrentPage > m_pPreviewInfo->GetMinPage());
}

void CPreviewView::OnUpdateZoomIn(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_nZoomState != ZOOM_IN);
}

void CPreviewView::OnUpdateZoomOut(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(m_nZoomState != ZOOM_OUT);
}

BOOL CPreviewView::OnSetCursor(CWnd* pWnd, UINT nHitTest, UINT message)
{
	if (nHitTest != HTCLIENT)
		return CScrollView::OnSetCursor(pWnd, nHitTest, message);

	CPoint point;
	::GetCursorPos(&point);
	ScreenToClient(&point);     // client coordinates of mouse position

	UINT nPage;
	if (m_nZoomState != ZOOM_IN && FindPageRect(point, nPage))
	{                       // On a page and not zoomed all the way in
		if (m_hMagnifyCursor == NULL)
		{
			HINSTANCE hInst = AfxFindResourceHandle(
				MAKEINTRESOURCE(AFX_IDC_MAGNIFY), RT_GROUP_CURSOR);
			m_hMagnifyCursor = ::LoadCursor(hInst,
				MAKEINTRESOURCE(AFX_IDC_MAGNIFY));
		}
		::SetCursor(m_hMagnifyCursor);
	}
	else
	{
		::SetCursor(::LoadCursor(NULL, IDC_ARROW));
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// CPreviewView diagnostics

#ifdef _DEBUG
void CPreviewView::AssertValid() const
{
	CView::AssertValid();
	ASSERT_VALID(&m_dcPrint);
	if (m_pPreviewDC != NULL)
		ASSERT_VALID(m_pPreviewDC);

	switch (m_nZoomState)
	{
	case ZOOM_OUT:
	case ZOOM_IN:
	case ZOOM_MIDDLE:
		break;
	default:
		ASSERT(FALSE); // unknown zoom state
	}

	switch (m_nMapMode)
	{
	case MM_TEXT:
	case MM_LOMETRIC:
	case MM_HIMETRIC:
	case MM_LOENGLISH:
	case MM_HIENGLISH:
	case MM_TWIPS:
	case MM_ISOTROPIC:
	case MM_ANISOTROPIC:
		break;
	default:
		ASSERT(FALSE); // unknown mapping mode
	}
}

void CPreviewView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);

	dc << "m_pPrintView = " << m_pPrintView;
	dc << "\nm_pOrigView = " << m_pOrigView;
	dc << "\nm_bPageNumDisplayed = " << m_bPageNumDisplayed;
	dc << "\nm_bCenter = " << m_bCenter;
	dc << "\nm_nPages = " << m_nPages;
	dc << "\nm_nCurrentPage " << m_nCurrentPage;
	dc << "\nm_nSecondPageOffset " << m_nSecondPageOffset;
	dc << "\nm_nMaxPages = " << m_nMaxPages;
	dc << "\nm_sizePrinterPPI = " << m_sizePrinterPPI;
	dc << "\nm_ptCenterPoint = " << m_ptCenterPoint;
	dc << "\nm_nZoomState = ";
	switch (m_nZoomState)
	{
	case ZOOM_OUT:
		dc << "ZOOM_OUT";
		break;
	case ZOOM_IN:
		dc << "ZOOM_IN";
		break;
	case ZOOM_MIDDLE:
		dc << "ZOOM_MIDDLE";
		break;
	default:
		dc << "*unknown*";
		break;
	}
	dc << "\nm_nMapMode = ";
	switch (m_nMapMode)
	{
	case MM_TEXT:
		dc << "MM_TEXT";
		break;
	case MM_LOMETRIC:
		dc << "MM_LOMETRIC";
		break;
	case MM_HIMETRIC:
		dc << "MM_HIMETRIC";
		break;
	case MM_LOENGLISH:
		dc << "MM_LOENGLISH";
		break;
	case MM_HIENGLISH:
		dc << "MM_HIENGLISH";
		break;
	case MM_TWIPS:
		dc << "MM_TWIPS";
		break;
	case MM_ISOTROPIC:
		dc << "MM_ISOTROPIC";
		break;
	case MM_ANISOTROPIC:
		dc << "MM_ANISOTROPIC";
		break;
	default:
		dc << "*unknown*";
		break;
	}
	dc << "\nm_dcPrint = " << &m_dcPrint;
	dc << "\nm_pPreviewDC = " << m_pPreviewDC;

	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CPreviewView, CScrollView)

/////////////////////////////////////////////////////////////////////////////
