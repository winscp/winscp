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

#define _afxTrackingMenu (AfxGetThreadState()->m_hTrackingMenu)

/////////////////////////////////////////////////////////////////////////////
// Special WM_PAINT message handler that includes the HDC

#define ON_WM_PAINT_SPECIAL() \
	{ WM_PAINT, 0, 0, 0, AfxSig_vD, \
		(AFX_PMSG)(AFX_PMSGW)(void (AFX_MSG_CALL CWnd::*)(CDC*))&OnPaint },

/////////////////////////////////////////////////////////////////////////////
// Window to serve as "parking space" for not-yet-activated subclassed controls

AFX_MODULE_STATE* AFXAPI _AfxGetOleModuleState();

AFX_STATIC HWND AFXAPI _AfxGetParkingWindow()
{
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	ASSERT(pThreadState != NULL);
	if (pThreadState->m_pWndPark == NULL)
	{
		AFX_MANAGE_STATE(_AfxGetOleModuleState());
		CWnd* pWndTmp = NULL;

		TRY
		{
#ifdef _DEBUG
			HWND hWndActive = ::GetActiveWindow();
#endif
			pWndTmp = new CParkingWnd;
			ASSERT(pWndTmp->m_hWnd != NULL);
#ifdef _DEBUG
			ASSERT(hWndActive == ::GetActiveWindow());
#endif
		}
		CATCH_ALL(e)
		{
			if (pWndTmp)
				delete pWndTmp;
			pWndTmp = NULL;
		}
		END_CATCH_ALL

		pThreadState->m_pWndPark = pWndTmp;
	}
	return pThreadState->m_pWndPark->GetSafeHwnd();
}

AFX_STATIC void AFXAPI _AfxReleaseParkingWindow()
{
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	ASSERT(pThreadState != NULL);
	ASSERT(pThreadState->m_nCtrlRef == 0);
	if (pThreadState->m_pWndPark != NULL)
	{
		AFX_MANAGE_STATE(_AfxGetOleModuleState());

		ASSERT(pThreadState->m_pWndPark->m_hWnd != NULL);
		ASSERT(::GetWindow(pThreadState->m_pWndPark->m_hWnd, GW_CHILD) == NULL);
		HWND hWnd = pThreadState->m_pWndPark->Detach();
		::SetWindowLong(hWnd, GWL_WNDPROC, (long)DefWindowProc);
		::DestroyWindow(hWnd);
		delete pThreadState->m_pWndPark;
		pThreadState->m_pWndPark = NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleControl interface map

BEGIN_INTERFACE_MAP(COleControl, CCmdTarget)
	INTERFACE_PART(COleControl, IID_IQuickActivate, QuickActivate)
	INTERFACE_PART(COleControl, IID_IOleObject, OleObject)
	INTERFACE_PART(COleControl, IID_IViewObjectEx, ViewObject)
	INTERFACE_PART(COleControl, IID_IPersistMemory, PersistMemory)
	INTERFACE_PART(COleControl, IID_IPersistStreamInit, PersistStreamInit)
	INTERFACE_PART(COleControl, IID_IProvideClassInfo2, ProvideClassInfo)
	INTERFACE_PART(COleControl, IID_IConnectionPointContainer, ConnPtContainer)
	INTERFACE_PART(COleControl, IID_IOleControl, OleControl)
	INTERFACE_PART(COleControl, IID_IOleInPlaceObject, OleInPlaceObject)
	INTERFACE_PART(COleControl, IID_IOleInPlaceObjectWindowless, OleInPlaceObject)
	INTERFACE_PART(COleControl, IID_IOleInPlaceActiveObject, OleInPlaceActiveObject)
	INTERFACE_PART(COleControl, IID_IDispatch, Dispatch)
	INTERFACE_PART(COleControl, IID_IOleCache, OleCache)
	INTERFACE_PART(COleControl, IID_IViewObject, ViewObject)
	INTERFACE_PART(COleControl, IID_IViewObject2, ViewObject)
	INTERFACE_PART(COleControl, IID_IDataObject, DataObject)
	INTERFACE_PART(COleControl, IID_IPersistPropertyBag, PersistPropertyBag)
	INTERFACE_PART(COleControl, IID_ISpecifyPropertyPages, SpecifyPropertyPages)
	INTERFACE_PART(COleControl, IID_IPerPropertyBrowsing, PerPropertyBrowsing)
	INTERFACE_PART(COleControl, IID_IProvideClassInfo, ProvideClassInfo)
	INTERFACE_PART(COleControl, IID_IPersist, PersistStorage)
	INTERFACE_PART(COleControl, IID_IPersistStorage, PersistStorage)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleControl event map

const AFX_EVENTMAP COleControl::eventMap = { NULL, NULL };

/////////////////////////////////////////////////////////////////////////////
// COleControl message map

BEGIN_MESSAGE_MAP(COleControl, CWnd)
	ON_WM_PAINT_SPECIAL()
	//{{AFX_MSG_MAP(COleControl)
	ON_WM_ERASEBKGND()
	ON_WM_NCCREATE()
	ON_WM_NCCALCSIZE()
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_WM_SHOWWINDOW()
	ON_WM_CREATE()
	ON_MESSAGE(WM_SETTEXT, OnSetText)
	ON_WM_NCPAINT()
	ON_WM_DESTROY()
	ON_WM_ENTERIDLE()
	ON_WM_KILLFOCUS()
	ON_WM_SETFOCUS()
	ON_MESSAGE(OCM_CTLCOLORBTN, OnOcmCtlColorBtn)
	ON_MESSAGE(OCM_CTLCOLORDLG, OnOcmCtlColorDlg)
	ON_MESSAGE(OCM_CTLCOLOREDIT, OnOcmCtlColorEdit)
	ON_MESSAGE(OCM_CTLCOLORLISTBOX, OnOcmCtlColorListBox)
	ON_MESSAGE(OCM_CTLCOLORMSGBOX, OnOcmCtlColorMsgBox)
	ON_MESSAGE(OCM_CTLCOLORSCROLLBAR, OnOcmCtlColorScrollBar)
	ON_MESSAGE(OCM_CTLCOLORSTATIC, OnOcmCtlColorStatic)
	ON_WM_MOUSEMOVE()
	ON_WM_NCHITTEST()
	ON_WM_MOUSEACTIVATE()
	ON_WM_NCLBUTTONDOWN()
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONUP()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_MBUTTONDOWN()
	ON_WM_MBUTTONUP()
	ON_WM_MBUTTONDBLCLK()
	ON_WM_RBUTTONDOWN()
	ON_WM_RBUTTONUP()
	ON_WM_RBUTTONDBLCLK()
	ON_WM_KEYDOWN()
	ON_WM_KEYUP()
	ON_WM_CHAR()
	ON_WM_SYSKEYDOWN()
	ON_WM_SYSKEYUP()
	ON_WM_INITMENUPOPUP()
	ON_WM_MENUSELECT()
	ON_WM_CANCELMODE()
	ON_WM_SETCURSOR()
	ON_WM_GETDLGCODE()
	ON_MESSAGE(WM_SETMESSAGESTRING, OnSetMessageString)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleControl implementation

COleControl::COleControl() :
#ifdef _AFXDLL
	m_font(&m_xFontNotification)
#else
	m_font(NULL)
#endif
{
	// add to parking window reference count
	_AFX_THREAD_STATE* pState = AfxGetThreadState();
	ASSERT(pState != NULL);
	++pState->m_nCtrlRef;

	m_bFinalReleaseCalled = FALSE;
	m_bChangingExtent = FALSE;
	m_bUIDead = FALSE;

	m_pReflect = NULL;
	m_pRectTracker = NULL;

	m_pClientSite = NULL;
	m_pOleAdviseHolder = NULL;
	m_pDataAdviseHolder = NULL;
	m_pInPlaceSite = NULL;
	m_pInPlaceFrame = NULL;
	m_pInPlaceDoc = NULL;
	m_pControlSite = NULL;

	m_pDefIUnknown = NULL;
	m_pDefIPersistStorage = NULL;
	m_pDefIViewObject = NULL;
	m_pDefIOleCache = NULL;
	m_pAdviseInfo = NULL;

	m_pUIActiveInfo = NULL;
	m_nIDTracking = 0;
	m_nIDLastMessage = 0;
	m_bAutoMenuEnable = TRUE;       // auto enable on by default

	m_bInPlaceActive = FALSE;
	m_bUIActive = FALSE;
	m_bPendingUIActivation = FALSE;
#ifdef _AFXDLL
	m_bOpen = FALSE;
	m_pWndOpenFrame = NULL;
#endif
	m_bInitialized = FALSE;

	m_dwVersionLoaded = 0;

	m_bModified = TRUE;

	m_iButtonState = 0;
	m_iDblClkState = 0;

	m_sBorderStyle = 0;
	m_sAppearance = 0;
	m_bEnabled = TRUE;
	m_lReadyState = READYSTATE_COMPLETE;

	m_hFontPrev = NULL;

	m_cEventsFrozen = 0;

	m_bConvertVBX = FALSE;

	m_pSimpleFrameSite = NULL;
	m_bSimpleFrame = FALSE;

	m_ptOffset.x = 0;
	m_ptOffset.y = 0;

	m_bNoRedraw = FALSE;

	m_bInPlaceSiteEx = FALSE;
	m_bInPlaceSiteWndless = FALSE;

	m_bOptimizedDraw = FALSE;

	m_bDataPathPropertiesLoaded = TRUE;

	SetInitialSize(100, 50);

	// Wire up aggregation support
	EnableAggregation();

	// Wire up IDispatch support
	EnableAutomation();

	// Wire up connection map support
	EnableConnections();

	m_pDataSource = NULL;

	AfxOleLockApp();
}


COleControl::~COleControl()
{
	if (m_pDataSource != NULL)
		delete m_pDataSource;

	if (m_pReflect != NULL)
		m_pReflect->DestroyWindow();

#ifdef _AFXDLL
	if (m_pWndOpenFrame != NULL)
		m_pWndOpenFrame->DestroyWindow();
#endif

	ReleaseCaches();

	if (m_hWnd != NULL)
		DestroyWindow();

	// release parking window if reference count is now zero
	_AFX_THREAD_STATE* pState = AfxGetThreadState();
	ASSERT(pState != NULL);
	if (pState->m_nCtrlRef == 0 || --pState->m_nCtrlRef == 0)
		_AfxReleaseParkingWindow();

	AfxOleUnlockApp();
}

void COleControl::ReleaseCaches()
{
	RELEASE(m_pClientSite);
	RELEASE(m_pOleAdviseHolder);
	RELEASE(m_pDataAdviseHolder);
	RELEASE(m_pInPlaceSite);
	RELEASE(m_pInPlaceFrame);
	RELEASE(m_pInPlaceDoc);
	RELEASE(m_pControlSite);
	RELEASE(m_pSimpleFrameSite);

	LPUNKNOWN pUnk = GetControllingUnknown();

	InterlockedIncrement(&m_dwRef);  // Keep ref count from going to zero again.

	if (m_pDefIPersistStorage != NULL)
	{
		pUnk->AddRef();
		RELEASE(m_pDefIPersistStorage);
	}

	if (m_pDefIViewObject != NULL)
	{
		pUnk->AddRef();
		RELEASE(m_pDefIViewObject);
	}

	if (m_pDefIOleCache != NULL)
	{
		pUnk->AddRef();
		RELEASE(m_pDefIOleCache);
	}

	if (m_pAdviseInfo != NULL)
	{
		RELEASE(m_pAdviseInfo->m_pAdvSink);
		delete m_pAdviseInfo;
		m_pAdviseInfo = NULL;
	}

	RELEASE(m_pDefIUnknown);
	InterlockedDecrement(&m_dwRef);
}

void COleControl::OnFinalRelease()
{
	if (!m_bFinalReleaseCalled)
	{
		m_bFinalReleaseCalled = TRUE;

		ReleaseCaches();

		if (m_hWnd != NULL)
			DestroyWindow();

		CCmdTarget::OnFinalRelease();
	}
}

LPUNKNOWN COleControl::GetInterfaceHook(const void* piid)
{
	ASSERT_POINTER(piid, IID);

	if (m_piidPrimary != NULL && _AfxIsEqualGUID(*m_piidPrimary, *(IID*)piid))
	{
		return GetInterface((void*)&IID_IDispatch);
	}

	if (_AfxIsEqualGUID(IID_IPointerInactive, *(IID*)piid) &&
		(GetControlFlags() & pointerInactive))
	{
		return &m_xPointerInactive;
	}

	return NULL;
}

void COleControl::SetInitialSize(int cx, int cy)
{
	SIZEL szlPixels;
	SIZEL szlHimetric;
	szlPixels.cx = cx;
	szlPixels.cy = cy;
	_AfxXformSizeInPixelsToHimetric(NULL, &szlPixels, &szlHimetric);
	m_cxExtent = szlHimetric.cx;
	m_cyExtent = szlHimetric.cy;
}

BOOL COleControl::GetDispatchIID(IID* pIID)
{
	if (m_piidPrimary != NULL)
		*pIID = *m_piidPrimary;

	return (m_piidPrimary != NULL);
}

void COleControl::InitializeIIDs(const IID* piidPrimary, const IID* piidEvents)
{
	m_piidPrimary = piidPrimary;
	m_piidEvents = piidEvents;

	EnableTypeLib();

	// Initialize the masks for stock events and properties.
	InitStockEventMask();
	InitStockPropMask();

#ifdef _DEBUG

	// Verify that the type library contains all the correct information.
	// If any of the following assertions fail, carefully check the IDs
	// in the control's .CPP file against those in its .ODL file.

	LPTYPEINFO pTypeInfo;
	HRESULT hr;
	CLSID clsid;

	GetClassID(&clsid);
	if (SUCCEEDED(hr = GetTypeInfoOfGuid(0, clsid, &pTypeInfo)))
		RELEASE(pTypeInfo);

	ASSERT(SUCCEEDED(hr));  // Class ID may be corrupted

	if (SUCCEEDED(hr = GetTypeInfoOfGuid(0, *m_piidPrimary, &pTypeInfo)))
		RELEASE(pTypeInfo);

	ASSERT(SUCCEEDED(hr));  // Primary dispatch interface ID may be corrupted

	if (SUCCEEDED(hr = GetTypeInfoOfGuid(0, *m_piidEvents, &pTypeInfo)))
		RELEASE(pTypeInfo);

	ASSERT(SUCCEEDED(hr));  // Event dispatch interface ID may be corrupted

#endif
}

#ifdef _DEBUG

void COleControl::AssertValid() const
{
	CWnd::AssertValid();
}

void AFXAPI _AfxDumpGuid(CDumpContext& dc, const GUID* pGuid)
{
	USES_CONVERSION;

	if (pGuid == NULL)
	{
		dc << "(NULL)";
		return;
	}

	OLECHAR szGuid[40];
	::StringFromGUID2(*pGuid, szGuid, 40);
	dc << OLE2CT(szGuid);
}

void AFXAPI _AfxDumpHex(CDumpContext& dc, DWORD dw)
{
	TCHAR szHex[10];
	wsprintf(szHex, _T("0x%08lx"), dw);
	dc << szHex;
}

void COleControl::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);
#ifdef _AFXDLL
	dc << "\nm_pModuleState = " << m_pModuleState;
#endif
	dc << "\nm_piidPrimary = ";
	_AfxDumpGuid(dc, m_piidPrimary);
	dc << "\nm_piidEvents =  ";
	_AfxDumpGuid(dc, m_piidEvents);
	dc << "\nm_dwVersionLoaded = ";
	_AfxDumpHex(dc, m_dwVersionLoaded);
	dc << "\nm_cEventsFrozen = " << m_cEventsFrozen;
	dc << "\nm_rcPos = " << m_rcPos;
	dc << "\nm_cxExtent = " << m_cxExtent;
	dc << "\nm_cyExtent = " << m_cyExtent;
	dc << "\nm_bFinalReleaseCalled = " << m_bFinalReleaseCalled;
	dc << "\nm_bModified = " << m_bModified;
	dc << "\nm_bCountOnAmbients = " << m_bCountOnAmbients;
	dc << "\nm_iButtonState = " << m_iButtonState;
	dc << "\nm_iDblClkState = " << m_iDblClkState;
	dc << "\nm_bInPlaceActive = " << m_bInPlaceActive;
	dc << "\nm_bUIActive = " << m_bUIActive;
	dc << "\nm_bPendingUIActivation = " << m_bPendingUIActivation;
#ifdef _AFXDLL
	dc << "\nm_bOpen = " << m_bOpen;
#endif
	dc << "\nm_bChangingExtent = " << m_bChangingExtent;
	dc << "\nm_bConvertVBX = " << m_bConvertVBX;
	dc << "\nm_bSimpleFrame = " << m_bSimpleFrame;
	dc << "\nm_bUIDead = " << m_bUIDead;
}

#endif // _DEBUG

/////////////////////////////////////////////////////////////////////////////
// _AfxFillPSOnStack (WINBUG)
//
// Windows has a bug in the WM_PAINT code for the Button control.  If the
// paint is a sub-classed paint, and if the display driver is a Win3.0
// display driver, then Windows calls IsRectEmpty, passing in the rectangle
// from the paint structure.  Since it was a sub-classed paint, and
// BeginPaint was not called, the rectangle struct passed to IsRectEmpty is
// uninitialized.  If IsRectEmpty returned True, then the control was not
// painted.  To work around the bug, we call FillPSOnStack before calling
// windows with the WM_PAINT.  This routine fills a buffer on the stack
// 0x80 bytes of consecutive values from 0 to 0x7f.  That way, if the
// rectangle falls anywhere within this buffer range, and the stack has not
// been modified by other means, then IsRectEmpty will return FALSE, so that
// the control will Paint.

#define WORDBUFSIZE 0x40

void AFXAPI _AfxFillPSOnStack()
{
	// Stack hack needed on Win32s
	WORD buf[WORDBUFSIZE];
	WORD i;
	WORD pat;

	for (i = 0, pat = 0x0100; i < WORDBUFSIZE; i++, pat += 0x0202)
		buf[i] = pat;
}

void COleControl::DoSuperclassPaint(CDC* pDC, const CRect& rcBounds)
{
	if (m_hWnd == NULL)
		CreateWindowForSubclassedControl();

	if (m_hWnd != NULL)
	{
		CRect rcClient;
		GetClientRect(&rcClient);

		if (rcClient.Size() != rcBounds.Size())
		{
			pDC->SetMapMode(MM_ANISOTROPIC);
			pDC->SetWindowExt(rcClient.right, rcClient.bottom);
			pDC->SetViewportExt(rcBounds.Size());
		}
		pDC->SetWindowOrg(0, 0);
		pDC->SetViewportOrg(rcBounds.left, rcBounds.top);

		BOOL bWin4 = afxData.bWin4;
		_AfxFillPSOnStack();
		::CallWindowProc(
				*GetSuperWndProcAddr(),
				m_hWnd, (bWin4 ? WM_PRINT : WM_PAINT),
				(WPARAM)(pDC->m_hDC),
				(LPARAM)(bWin4 ? PRF_CHILDREN | PRF_CLIENT : 0));
	}
}

BOOL COleControl::IsSubclassedControl()
{
	// This default implementation provides some degree of backward
	// compatibility with old controls. New subclassed controls should just
	// override IsSubclassedControl and return TRUE.
	return m_pfnSuper == NULL &&
		GetSuperWndProcAddr() != CWnd::GetSuperWndProcAddr();
}

BOOL COleControl::CreateControlWindow(HWND hWndParent, const CRect& rcPos,
	LPCRECT prcClip)
{
	if (m_hWnd == NULL)
	{
		// If window doesn't exist, create it.

		// Test if:
		// we're not subclassing a Windows control, or
		// container reflects messages for us...

		DWORD dwStyle = WS_VISIBLE|WS_CHILD|WS_CLIPSIBLINGS|WS_CLIPCHILDREN;
		if (m_sBorderStyle)
			dwStyle |= WS_BORDER;
		if (!m_bEnabled)
			dwStyle |= WS_DISABLED;
		DWORD dwExStyle = WS_EX_NOPARENTNOTIFY;
		if (m_sAppearance)
			dwExStyle |= WS_EX_CLIENTEDGE;

		// we create normally if:
		//       (we're not subclassing -or- the container reflects)
		// -and- the container autoclips for us
		if ((!IsSubclassedControl() || m_bMsgReflect) && m_bAutoClip)
		{
			// Just create the control's window.
			VERIFY(AfxDeferRegisterClass(AFX_WNDOLECONTROL_REG));
			CreateEx(dwExStyle, AFX_WNDOLECONTROL, m_strText, dwStyle,
				rcPos.left, rcPos.top, rcPos.Width(), rcPos.Height(),
				hWndParent, 0);
		}
		else    // ...we're subclassing a Windows control.
		{
			if (m_pReflect == NULL)
			{
				// Create a window to reflect notification messages.
				m_pReflect = new CReflectorWnd;
				if (prcClip == NULL)
					prcClip = rcPos;
				if (!m_pReflect->Create(prcClip, hWndParent))
				{
					// If m_pReflect->Create failed, then m_pReflect deleted itself.
					m_pReflect = NULL;
				}
			}
			else
			{
				// Reflector window already exists... just reparent it.
				if (m_pReflect->m_hWnd != NULL)
				{
					::SetParent(m_pReflect->m_hWnd, hWndParent);
					::SetWindowPos(m_pReflect->m_hWnd, NULL, 0, 0, 0, 0,
						SWP_NOZORDER|SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|
						SWP_SHOWWINDOW);
				}
			}

			if (m_pReflect != NULL && m_pReflect->m_hWnd != NULL)
			{
				// Create the control's window.
				CreateEx(dwExStyle, NULL, m_strText, dwStyle,
					m_ptOffset.x, m_ptOffset.y, rcPos.Width(), rcPos.Height(),
					m_pReflect->m_hWnd, 0);
				if (m_hWnd == NULL)
				{
					// Window creation failed: cleanup.
					m_pReflect->DestroyWindow();
					m_pReflect = NULL;
				}
			}
		}

		// Set the new window's font.
		OnFontChanged();
	}
	else
	{
		// If window does exist, reparent it...
		CWnd* pWndOuter = GetOuterWindow();
		ASSERT(pWndOuter != NULL);

		if (::GetParent(pWndOuter->m_hWnd) != hWndParent)
			ReparentControlWindow(pWndOuter->m_hWnd, hWndParent);

		::SetWindowPos(pWndOuter->m_hWnd, NULL, 0, 0, 0, 0,
			SWP_NOZORDER|SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|
			SWP_SHOWWINDOW);

		// And then reposition it...
		OnSetObjectRects(rcPos, prcClip);
	}

	ASSERT(m_hWnd != NULL);
	return (m_hWnd != NULL);
}

void COleControl::ReparentControlWindow(HWND hWndOuter, HWND hWndParent)
{
	// can be overridden by subclass, if necessary
	::SetParent(hWndOuter, hWndParent);
}

void COleControl::GetControlSize(int* pcx, int* pcy)
{
	ASSERT_POINTER(pcx, int);
	ASSERT_POINTER(pcy, int);

	SIZEL szlHimetric;
	SIZEL szlPixels;
	szlHimetric.cx = m_cxExtent;
	szlHimetric.cy = m_cyExtent;
	_AfxXformSizeInHimetricToPixels(NULL, &szlHimetric, &szlPixels);
	*pcx = (int)szlPixels.cx;
	*pcy = (int)szlPixels.cy;
}

BOOL COleControl::SetControlSize(int cx, int cy)
{
	SIZEL szlPixels;
	SIZEL szlHimetric;
	szlPixels.cx = cx;
	szlPixels.cy = cy;
	_AfxXformSizeInPixelsToHimetric(NULL, &szlPixels, &szlHimetric);
	return SUCCEEDED(m_xOleObject.SetExtent(DVASPECT_CONTENT, &szlHimetric));
}

BOOL COleControl::SetRectInContainer(LPCRECT lpRect)
{
	if ((m_pInPlaceSite != NULL) && m_bInPlaceActive)
	{
		m_pInPlaceSite->OnPosRectChange(lpRect);
		return TRUE;
	}
#ifdef _AFXDLL
	else if (m_bOpen)
	{
		ResizeOpenControl(lpRect->right - lpRect->left,
				lpRect->bottom - lpRect->top);
		return TRUE;
	}
#endif

	return FALSE;
}

void COleControl::OnResetState()
{
	CResetPropExchange px;
	DoPropExchange(&px);
	InvalidateControl();
}

#ifndef BDR_INNER
#define BDR_INNER   0x000c
#endif
#ifndef BDR_OUTER
#define BDR_OUTER   0x0003
#endif

void AFXAPI _AfxDrawBorders(CDC* pDC, CRect& rc, BOOL bBorder, BOOL bClientEdge)
{
	if (bBorder)
		::DrawEdge(pDC->m_hDC, &rc, (bClientEdge ? BDR_INNER : BDR_OUTER),
			BF_RECT | BF_ADJUST | (bClientEdge ? BF_FLAT : BF_MONO));

	if (bClientEdge)
		::DrawEdge(pDC->m_hDC, &rc, EDGE_SUNKEN, BF_RECT | BF_ADJUST);
}

void COleControl::DrawContent(CDC* pDC, CRect& rc)
{
	// Map into device coordinates.

	pDC->LPtoDP(&rc);
	int iSaveDC = 0;
	if (! m_bOptimizedDraw)
		iSaveDC = pDC->SaveDC();
	pDC->SetViewportOrg(0, 0);
	pDC->SetWindowOrg(0, 0);
	pDC->SetMapMode(MM_TEXT);
	m_rcBounds = rc;
	if (pDC->GetDeviceCaps(TECHNOLOGY) == DT_RASDISPLAY)
		_AfxDrawBorders(pDC, rc, (m_sBorderStyle == 1), (m_sAppearance == 1));
	OnDraw(pDC, rc, rc);
	if (! m_bOptimizedDraw && iSaveDC != 0)
		pDC->RestoreDC(iSaveDC);
}

void COleControl::DrawMetafile(CDC* pDC, CRect& rc)
{
	int iSaveDC = 0;
	if (! m_bOptimizedDraw)
		iSaveDC = pDC->SaveDC();
	m_rcBounds = rc;
	_AfxDrawBorders(pDC, rc, (m_sBorderStyle == 1), (m_sAppearance == 1));
	OnDrawMetafile(pDC, rc);
	if (! m_bOptimizedDraw && iSaveDC != 0)
		pDC->RestoreDC(iSaveDC);
}

void COleControl::OnDraw(CDC*, const CRect&, const CRect&)
{
	// To be overridden by subclass.
}

void COleControl::OnDrawMetafile(CDC* pDC, const CRect& rcBounds)
{
	// By default, we draw into metafile the same way we would draw to the
	// screen.  This may be overridden by the subclass.

	OnDraw(pDC, rcBounds, rcBounds);
}

BOOL COleControl::GetMetafileData(LPFORMATETC lpFormatEtc,
		LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));
	ASSERT(lpStgMedium->tymed == TYMED_NULL);   // GetDataHere not valid
	ASSERT(lpStgMedium->pUnkForRelease == NULL);

	// medium must be TYMED_MFPICT -- cannot fill in existing HGLOBAL
	if (!(lpFormatEtc->tymed & TYMED_MFPICT) || lpStgMedium->hGlobal != NULL)
		return FALSE;

	// create appropriate memory metafile DC
	CMetaFileDC dc;
	if (!dc.Create())
		return FALSE;

	// create attribute DC according to lpFormatEtc->ptd
	HDC hAttribDC = ::_AfxOleCreateDC(lpFormatEtc->ptd);
	dc.SetAttribDC(hAttribDC);

	// Paint directly into the metafile.
	int cx;
	int cy;
	GetControlSize(&cx, &cy);
	CRect rc(0, 0, cx, cy);
	dc.SetMapMode(MM_ANISOTROPIC);
	dc.SetWindowOrg(0, 0);
	dc.SetWindowExt(cx, cy);
	DrawMetafile(&dc, rc);

	// attribute DC is no longer necessary
	dc.SetAttribDC(NULL);
	::DeleteDC(hAttribDC);

	HMETAFILE hMF;
	hMF = (HMETAFILE)dc.Close();
	if (hMF == NULL)
		return FALSE;

	HGLOBAL hPict;
	if ((hPict = ::GlobalAlloc(GMEM_DDESHARE, sizeof(METAFILEPICT))) == NULL)
	{
		DeleteMetaFile(hMF);
		return FALSE;
	}
	LPMETAFILEPICT lpPict;
	if ((lpPict = (LPMETAFILEPICT)::GlobalLock(hPict)) == NULL)
	{
		DeleteMetaFile(hMF);
		::GlobalFree(hPict);
		return FALSE;
	}

	// set the metafile size
	lpPict->mm = MM_ANISOTROPIC;
	lpPict->hMF = hMF;
	lpPict->xExt = (int)m_cxExtent;
	lpPict->yExt = (int)m_cyExtent;

	// return the medium with the hGlobal to the METAFILEPICT
	::GlobalUnlock(hPict);
	lpStgMedium->hGlobal = hPict;
	lpStgMedium->tymed = TYMED_MFPICT;
	return TRUE;
}

BOOL COleControl::OnCreateAggregates()
{
	return TRUE;
}

LPVOID COleControl::QueryDefHandler(REFIID iid)
{
	// If we're being aggregated, we want to pass the outer unknown
	// to the object we're aggregating (the default handler).
	// Otherwise, we pass our own "inner" unknown.
	// That's what GetControllingUnknown() does.

	LPUNKNOWN pUnk = GetControllingUnknown();

	CLSID clsid;
	GetClassID(&clsid);

	if (m_pDefIUnknown == NULL)
	{
		// Note: This call will not increment pUnk's reference count.
		HRESULT hr = CreateDataCache(pUnk, clsid,
			IID_IUnknown, (LPLPVOID)&m_pDefIUnknown);

		if (FAILED(hr))
			return NULL;
	}

	LPVOID pNew;

	// Note: For the following QueryInterface call, we want to prevent
	// pUnk's reference count from being incremented.  So, if the
	// call succeeds, we immediately force a decrement of the reference count.

	if (SUCCEEDED(m_pDefIUnknown->QueryInterface(iid, &pNew)))
	{
		ASSERT(pNew != NULL);
		pUnk->Release();
	}

	return pNew;
}

void COleControl::InvalidateControl(LPCRECT lpRect, BOOL bErase)
{
	if (m_bInPlaceActive && m_bInPlaceSiteWndless)
	{
		CRect rect;
		if (lpRect != NULL)
		{
			CPoint point(0, 0);
			ClientToParent(m_rcPos, &point);
			rect.CopyRect(lpRect);
			rect.OffsetRect(point);
			lpRect = &rect;
		}
		m_pInPlaceSiteWndless->InvalidateRect(lpRect, bErase);
	}
#ifdef _AFXDLL
	else if (m_bInPlaceActive || m_bOpen)
#else
	else if (m_bInPlaceActive)
#endif
	{
		InvalidateRect(lpRect, bErase);
	}
	else
	{
		SendAdvise(OBJECTCODE_VIEWCHANGED);
	}

	if (m_bModified)
		SendAdvise(OBJECTCODE_DATACHANGED);
}

CWnd* COleControl::GetOuterWindow() const
{
	return (m_pReflect != NULL ? (CWnd*)m_pReflect : (CWnd*)this);
}

void COleControl::OnReflectorDestroyed()
{
	m_pReflect = NULL;
}

HRESULT COleControl::SaveState(IStream* pstm)
{
	HRESULT hr = S_OK;

	TRY
	{
		// Delegate to the Serialize method.
		COleStreamFile file(pstm);
		CArchive ar(&file, CArchive::store);
		Serialize(ar);
	}
	CATCH_ALL(e)
	{
		hr = E_FAIL;
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return hr;
}

HRESULT COleControl::LoadState(IStream* pstm)
{
	HRESULT hr = S_OK;

	TRY
	{
		// Delegate to the Serialize method.
		COleStreamFile file(pstm);
		CArchive ar(&file, CArchive::load);
		Serialize(ar);
	}
	CATCH_ALL(e)
	{
		// The load failed.  Delete any partially-initialized state.
		OnResetState();
		m_bInitialized = TRUE;
		hr = E_FAIL;
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	// Clear the modified flag.
	m_bModified = FALSE;

	// Unless IOleObject::SetClientSite is called after this, we can
	// count on ambient properties being available while loading.
	m_bCountOnAmbients = TRUE;

	// Properties have been initialized
	m_bInitialized = TRUE;

	// Uncache cached ambient properties
	_afxAmbientCache->Cache(NULL);

	return hr;
}

void COleControl::SendAdvise(UINT uCode)
{
	// Calls the appropriate IOleClientSite or IAdviseSink member function
	// for various events such as closure, renaming, saving, etc.

	switch (uCode)
	{
	case OBJECTCODE_SAVED:
		if (m_pOleAdviseHolder != NULL)
			m_pOleAdviseHolder->SendOnSave();
		break;

	case OBJECTCODE_CLOSED:
		if (m_pOleAdviseHolder != NULL)
			m_pOleAdviseHolder->SendOnClose();
		break;

	case OBJECTCODE_SAVEOBJECT:
		if (m_bModified && m_pClientSite != NULL)
			m_pClientSite->SaveObject();
		break;

	case OBJECTCODE_DATACHANGED:
		//No flags are necessary here.
		if (m_pDataAdviseHolder != NULL)
			m_pDataAdviseHolder->SendOnDataChange(&m_xDataObject, 0, 0);
		break;

	case OBJECTCODE_SHOWWINDOW:
		if (m_pClientSite != NULL)
			m_pClientSite->OnShowWindow(TRUE);
		break;

	case OBJECTCODE_HIDEWINDOW:
		if (m_pClientSite != NULL)
			m_pClientSite->OnShowWindow(FALSE);
		break;

	case OBJECTCODE_SHOWOBJECT:
		if (m_pClientSite != NULL)
			m_pClientSite->ShowObject();
		break;

	case OBJECTCODE_VIEWCHANGED:
		{
			DWORD aspects;
			DWORD advf;
			LPADVISESINK pAdvSink;

			if (SUCCEEDED(m_xViewObject.GetAdvise(&aspects, &advf, &pAdvSink)) &&
				(pAdvSink != NULL))
			{
				pAdvSink->OnViewChange(DVASPECT_CONTENT, -1);
				pAdvSink->Release();
			}
		}
		break;
	}
}

HRESULT COleControl::OnHide()
{
#ifdef _AFXDLL
	CWnd* pWnd = m_bOpen ? m_pWndOpenFrame : GetOuterWindow();
#else
	CWnd* pWnd = GetOuterWindow();
#endif
	if (pWnd != NULL && pWnd->m_hWnd != NULL)
		::ShowWindow(pWnd->m_hWnd, SW_HIDE);

	RELEASE(m_pInPlaceFrame);
	RELEASE(m_pInPlaceDoc);

#ifdef _AFXDLL
	if (m_bOpen)
		SendAdvise(OBJECTCODE_HIDEWINDOW);
#endif

	return S_OK;
}

HRESULT COleControl::OnOpen(BOOL bTryInPlace, LPMSG pMsg)
{
#ifndef _AFXDLL
	ASSERT(bTryInPlace);    // fully-open mode not supported in static builds
	UNUSED_ALWAYS(bTryInPlace);
#endif

#ifndef _AFXDLL
	return OnActivateInPlace(TRUE, pMsg);
#else
	if (!m_bOpen)
	{
		// If not already open, try in-place activating.
		if (bTryInPlace && SUCCEEDED(OnActivateInPlace(bTryInPlace, pMsg)))
			return S_OK;

		// If already in-place active, deactivate first.
		if (m_bInPlaceActive)
			m_xOleInPlaceObject.InPlaceDeactivate();

		m_bOpen = TRUE;

		// Open a separate window.
		if (m_pWndOpenFrame == NULL)
		{
			// Create frame window
			m_pWndOpenFrame = CreateFrameWindow();

			if (m_pWndOpenFrame == NULL)
				return E_FAIL;

			// Size frame window to exactly contain the control.
			int cx;
			int cy;
			GetControlSize(&cx, &cy);
			ResizeFrameWindow(cx, cy);

			// Create and/or reparent the control's window.
			CRect rectClient;
			m_pWndOpenFrame->GetClientRect(&rectClient);
			if (!CreateControlWindow(m_pWndOpenFrame->m_hWnd, rectClient, rectClient))
				return E_FAIL;
		}
	}

	// Make the frame window visible and activate it.
	ASSERT(m_pWndOpenFrame != NULL);
	m_pWndOpenFrame->ShowWindow(SW_SHOW);
	m_pWndOpenFrame->SetActiveWindow();
	SendAdvise(OBJECTCODE_SHOWWINDOW);
	return S_OK;
#endif
}

#ifdef _AFXDLL
CControlFrameWnd* COleControl::CreateFrameWindow()
{
	TCHAR szUserType[256];
	GetUserType(szUserType);

	CControlFrameWnd* pWnd = new CControlFrameWnd(this);
	if (!pWnd->Create(szUserType))
	{
		// If Create failed, then frame window has deleted itself.
		pWnd = NULL;
	}

	return pWnd;
}

void COleControl::OnFrameClose()
{
	// Reparent control to prevent its window from being destroyed.
	CWnd* pWnd = GetOuterWindow();
	if (pWnd != NULL)
	{
		::SetWindowPos(pWnd->m_hWnd, NULL, 0, 0, 0, 0,
			SWP_NOZORDER|SWP_NOMOVE|SWP_NOSIZE|SWP_NOACTIVATE|
			SWP_HIDEWINDOW);
		pWnd->SetParent(NULL);
	}

	m_pWndOpenFrame = NULL;
	m_bOpen = FALSE;

	m_xOleObject.Close(OLECLOSE_SAVEIFDIRTY);

	SendAdvise(OBJECTCODE_HIDEWINDOW);
	SendAdvise(OBJECTCODE_CLOSED);
}

void COleControl::ResizeOpenControl(int cx, int cy)
{
	CWnd* pWndOuter = GetOuterWindow();
	if ((pWndOuter != NULL) && (pWndOuter->m_hWnd != NULL))
		::SetWindowPos(pWndOuter->m_hWnd, NULL, 0, 0, cx, cy,
					SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
	ResizeFrameWindow(cx, cy);
}

void COleControl::ResizeFrameWindow(int cxCtrl, int cyCtrl)
{
	m_pWndOpenFrame->SetWindowPos(NULL, 0, 0, 100, 100,
				SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
	CRect rectClient;
	m_pWndOpenFrame->GetClientRect(&rectClient);
	CRect rectWindow;
	m_pWndOpenFrame->GetWindowRect(&rectWindow);
	int cx = cxCtrl + rectWindow.Width()  - rectClient.Width();
	int cy = cyCtrl + rectWindow.Height() - rectClient.Height();
	m_pWndOpenFrame->SetWindowPos(NULL, 0, 0, cx, cy,
				SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
}

#endif //_AFXDLL

BOOL COleControl::GetRectInContainer(LPRECT lpRect)
{
	if (m_bInPlaceActive)
		CopyRect(lpRect, &m_rcPos);

	return m_bInPlaceActive;
}

LPCLSID COleControl::GetPropPageIDs(ULONG& cPropPages)
{
	cPropPages = 0;
	return NULL;
}

BOOL COleControl::OnEdit(LPMSG lpMsg, HWND, LPCRECT lpRect)
{
	CopyRect(m_rcPos, lpRect);
	return SUCCEEDED(OnActivateInPlace(TRUE, lpMsg));
}

HWND AFXAPI _AfxGetTopLevelWindow(HWND hWnd)
{
	HWND hWndTop;

	do
	{
		hWndTop = hWnd;
		hWnd = ::GetParent(hWnd);
	}
	while (hWnd != NULL);

	return hWndTop;
}

BOOL COleControl::OnProperties(LPMSG, HWND hWndParent, LPCRECT)
{
	USES_CONVERSION;

	HRESULT hr;

	if ((m_pControlSite == NULL) ||
		FAILED(hr = m_pControlSite->ShowPropertyFrame()))
	{
		LPUNKNOWN pUnk = GetIDispatch(FALSE);
		HWND hWndOwner = CWnd::GetSafeOwner_(hWndParent, NULL);

		LCID lcid = AmbientLocaleID();

		ULONG cPropPages;
		LPCLSID pclsidPropPages = GetPropPageIDs(cPropPages);

		RECT rectParent;
		RECT rectTop;
		::GetWindowRect(hWndParent, &rectParent);
		::GetWindowRect(hWndOwner, &rectTop);

		TCHAR szUserType[256];
		GetUserType(szUserType);

		PreModalDialog(hWndOwner);
		hr = ::OleCreatePropertyFrame(hWndOwner, rectParent.left - rectTop.left,
				rectParent.top - rectTop.top, T2COLE(szUserType), 1, &pUnk,
				cPropPages, pclsidPropPages, lcid, NULL, 0);
		PostModalDialog(hWndOwner);
	}

	return SUCCEEDED(hr);
}

DWORD COleControl::GetControlFlags()
{
	return clipPaintDC;
}

void COleControl::OnPaint(CDC* pDC)
{
	if (m_bNoRedraw)
	{
		// flicker-free activation: no need to repaint
		ValidateRect(NULL);
		m_bNoRedraw = FALSE;    // one time only
		return;
	}

	AfxLockTempMaps();

	GetWindowRect(m_rcBounds);
	m_rcBounds.OffsetRect(-m_rcBounds.left, -m_rcBounds.top);

	// Adjust bounds for size of UI Active tracker, if any.
#ifdef _AFXDLL
	if (!m_bOpen && (m_pRectTracker != NULL))
#else
	if (m_pRectTracker != NULL)
#endif
	{
		int nHandleSize = (int)m_pRectTracker->m_nHandleSize - 1;
		m_rcBounds.InflateRect(-nHandleSize, -nHandleSize);
	}

	CRect rcClient;
	GetClientRect(rcClient);

	if (pDC != NULL)
	{
		// We were passed a device context: use it.
		int iSaveDC = pDC->SaveDC();
		OnDraw(pDC, rcClient, rcClient);
		pDC->RestoreDC(iSaveDC);
	}
	else
	{
#ifdef _DEBUG
		int nFlags = GetControlFlags();
		if (nFlags & fastBeginPaint)
			TRACE0("Warning: COleControl::fastBeginPaint is obsolete.\n");
#endif

		CPaintDC dc(this);
		OnDraw(&dc, rcClient, &dc.m_ps.rcPaint);
	}

	AfxUnlockTempMaps();
}

BOOL COleControl::OnEraseBkgnd(CDC* pDC)
{
	// do nothing -- controls erase their background in their OnDraw
	if (IsSubclassedControl())
		return CWnd::OnEraseBkgnd(pDC);
	else
		return TRUE;
}

void  COleControl::Serialize(CArchive& ar)
{
	CArchivePropExchange px(ar);
	DoPropExchange(&px);
	if (ar.IsLoading())
	{
		BoundPropertyChanged(DISPID_UNKNOWN);
		InvalidateControl();
	}
}

void COleControl::DoPropExchange(CPropExchange* pPX)
{
	ASSERT_POINTER(pPX, CPropExchange);

	ExchangeExtent(pPX);
	ExchangeStockProps(pPX);
}

/////////////////////////////////////////////////////////////////////////////
// Wrappers for IOleControlSite

void COleControl::ControlInfoChanged()
{
	if (m_pControlSite != NULL)
		m_pControlSite->OnControlInfoChanged();
}

BOOL COleControl::LockInPlaceActive(BOOL bLock)
{
	if (m_pControlSite != NULL)
		return SUCCEEDED(m_pControlSite->LockInPlaceActive(bLock));

	return FALSE;
}

LPDISPATCH COleControl::GetExtendedControl()
{
	LPDISPATCH pDispatch = NULL;
	if (m_pControlSite != NULL)
		m_pControlSite->GetExtendedControl(&pDispatch);

	return pDispatch;
}

void COleControl::TransformCoords(POINTL* lpptlHimetric,
	POINTF* lpptfContainer, DWORD flags)
{
	if ((m_pControlSite == NULL) ||
		(FAILED(m_pControlSite->TransformCoords(lpptlHimetric,
			lpptfContainer, flags))))
	{
		// Transformation failed, use the identity transformation

		if (flags & XFORMCOORDS_CONTAINERTOHIMETRIC)
		{
			lpptlHimetric->x = (long)lpptfContainer->x;
			lpptlHimetric->y = (long)lpptfContainer->y;
		}
		else
		{
			lpptfContainer->x = (float)lpptlHimetric->x;
			lpptfContainer->y = (float)lpptlHimetric->y;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XOleControl

STDMETHODIMP_(ULONG) COleControl::XOleControl::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, OleControl)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XOleControl::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, OleControl)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XOleControl::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, OleControl)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XOleControl::GetControlInfo(LPCONTROLINFO pCI)
{
	METHOD_PROLOGUE_EX(COleControl, OleControl)
	pThis->OnGetControlInfo(pCI);
	return S_OK;
}

STDMETHODIMP COleControl::XOleControl::OnMnemonic(LPMSG pMsg)
{
	METHOD_PROLOGUE_EX(COleControl, OleControl)
	pThis->OnMnemonic(pMsg);
	return S_OK;
}

STDMETHODIMP COleControl::XOleControl::OnAmbientPropertyChange(DISPID dispid)
{
	METHOD_PROLOGUE_EX(COleControl, OleControl)

	if (dispid == DISPID_AMBIENT_UIDEAD || dispid == DISPID_UNKNOWN)
		pThis->m_bUIDead = (BYTE)(pThis->AmbientUIDead());

	pThis->OnAmbientPropertyChange(dispid);
	return S_OK;
}

STDMETHODIMP COleControl::XOleControl::FreezeEvents(BOOL bFreeze)
{
	METHOD_PROLOGUE_EX(COleControl, OleControl)

	ULONG& cEventsFrozen = pThis->m_cEventsFrozen;

	if (bFreeze)
		++(cEventsFrozen);
	else
		--(cEventsFrozen);

	ASSERT(cEventsFrozen >= 0); // Should never go below zero!

	if ((cEventsFrozen == 1 && bFreeze) ||
		(cEventsFrozen == 0 && !bFreeze))
	{
		pThis->OnFreezeEvents(bFreeze);
	}

	return S_OK;
}

void COleControl::OnGetControlInfo(LPCONTROLINFO pControlInfo)
{
	// Subclass may override

	pControlInfo->hAccel = NULL;
	pControlInfo->cAccel = 0;
	pControlInfo->dwFlags = 0;
}

void COleControl::OnMnemonic(LPMSG)
{
	// To be implemented by subclass
}

void COleControl::OnAmbientPropertyChange(DISPID)
{
	// To be implemented by subclass
}

void COleControl::OnFreezeEvents(BOOL)
{
	// To be implemented by subclass
}

void COleControl::OnSetClientSite()
{
	if (!m_bDataPathPropertiesLoaded)
	{
		CAsyncPropExchange PX(m_dwDataPathVersionToReport);
		DoPropExchange(&PX);
		m_bDataPathPropertiesLoaded=TRUE;
	}
}

LPOLECLIENTSITE COleControl::GetClientSite()
{
	return m_pClientSite;
}

COLORREF COleControl::TranslateColor(OLE_COLOR clrColor, HPALETTE hpal)
{
	COLORREF cr = RGB(0x00,0x00,0x00);
	::OleTranslateColor(clrColor, hpal, &cr);
	return cr;
}

void COleControl::Refresh()
{
	InvalidateControl();
	if (m_hWnd != NULL)
		UpdateWindow();
}

void COleControl::DoClick()
{
	OnClick(LEFT_BUTTON);
}

BOOL COleControl::OnNcCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (m_pReflect != NULL)
		m_pReflect->SetControl(this);

	return CWnd::OnNcCreate(lpCreateStruct);
}

void COleControl::RecreateControlWindow()
{
	if (m_bInPlaceActive)
	{
		BOOL bUIActive = m_bUIActive;
		m_xOleInPlaceObject.InPlaceDeactivate();
		DestroyWindow();
		OnActivateInPlace(bUIActive, NULL);
	}
#ifdef _AFXDLL
	else if (m_bOpen)
	{
		DestroyWindow();
		CRect rectClient;
		m_pWndOpenFrame->GetClientRect(&rectClient);
		CreateControlWindow(m_pWndOpenFrame->m_hWnd, rectClient, rectClient);
	}
#endif //_AFXDLL
	else
	{
		HWND hWndParent = _AfxGetParkingWindow();
		if (hWndParent != NULL)
		{
			DestroyWindow();
			int cx;
			int cy;
			GetControlSize(&cx, &cy);
			CRect rect(0, 0, cx, cy);
			CreateControlWindow(hWndParent, rect);
		}
	}
}

void COleControl::CreateWindowForSubclassedControl()
{
	if (IsSubclassedControl() && (m_hWnd == NULL))
	{
		// If this is a subclassed control, we should create the window
		// for it now, in case the window is needed by the DoSuperclassPaint
		// implementation.

		HWND hWndParent = _AfxGetParkingWindow();
		if (hWndParent != NULL)
		{
			SIZEL szlHimetric;
			SIZEL szlPixels;
			szlHimetric.cx = m_cxExtent;
			szlHimetric.cy = m_cyExtent;
			_AfxXformSizeInHimetricToPixels(NULL, &szlHimetric, &szlPixels);
			CRect rcPos(0, 0, (int)szlPixels.cx, (int)szlPixels.cy);
			CreateControlWindow(hWndParent, rcPos);
		}
	}
}

int COleControl::OnMouseActivate(CWnd *pDesktopWnd, UINT nHitTest, UINT message)
{
	if (m_bInPlaceActive && !m_bUIActive)
		m_bPendingUIActivation = TRUE;

	return CWnd::OnMouseActivate(pDesktopWnd, nHitTest, message);
}

void COleControl::PreModalDialog(HWND hWndParent)
{
	if (m_pInPlaceFrame != NULL)
	{
		m_pInPlaceFrame->EnableModeless(FALSE);
	}
	else
	{
		HWND hWndTop = _AfxGetTopLevelWindow(hWndParent);
		if (hWndTop != NULL)
			::EnableWindow(hWndTop, FALSE);
	}
}

void COleControl::PostModalDialog(HWND hWndParent)
{
	if (m_pInPlaceFrame != NULL)
	{
		m_pInPlaceFrame->EnableModeless(TRUE);
	}
	else
	{
		HWND hWndTop = _AfxGetTopLevelWindow(hWndParent);
		if (hWndTop != NULL)
			::EnableWindow(hWndTop, TRUE);
	}
}

void COleControl::SetModifiedFlag(BOOL bModified)
{
	m_bModified = (BYTE)bModified;
}

BOOL COleControl::IsModified()
{
	return m_bModified;
}

BOOL COleControl::WillAmbientsBeValidDuringLoad()
{
	return m_bCountOnAmbients;
}

void COleControl::EnableSimpleFrame()
{
	m_bSimpleFrame = TRUE;
}

BOOL COleControl::IgnoreWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam,
	LRESULT* plResult)
{
	if (!m_bUIDead)
		return FALSE;

	switch (msg)
	{
	case WM_NCHITTEST:
		*plResult = HTNOWHERE;
		return TRUE;

	case WM_SETCURSOR:
		*plResult = ::SendMessage(::GetParent(m_hWnd), msg, wParam, lParam);
		return TRUE;
	}

	if ((msg >= WM_KEYFIRST) && (msg <= WM_KEYLAST))
	{
		*plResult = 0;
		return TRUE;
	}

	return FALSE;
}

LRESULT COleControl::WindowProc(UINT msg, WPARAM wParam, LPARAM lParam)
{
	DWORD dwCookie;
	LRESULT lResult;
	HRESULT hr;

	ExternalAddRef();   // "Insurance" addref -- keeps control alive

	// allow OCM_ reflections to be handled by ON_XXX_REFLECT macros
	switch (msg)
	{
	case OCM_COMMAND:
	case OCM_CTLCOLORBTN:
	case OCM_CTLCOLOREDIT:
	case OCM_CTLCOLORDLG:
	case OCM_CTLCOLORLISTBOX:
	case OCM_CTLCOLORMSGBOX:
	case OCM_CTLCOLORSCROLLBAR:
	case OCM_CTLCOLORSTATIC:
	case OCM_DRAWITEM:
	case OCM_MEASUREITEM:
	case OCM_DELETEITEM:
	case OCM_VKEYTOITEM:
	case OCM_CHARTOITEM:
	case OCM_COMPAREITEM:
	case OCM_HSCROLL:
	case OCM_VSCROLL:
	case OCM_PARENTNOTIFY:
	case OCM_NOTIFY:
		if (ReflectChildNotify(msg-OCM__BASE, wParam, lParam, &lResult))
		{
			ExternalRelease();
			return lResult;
		}
	}

	// Give the simple frame site the opportunity to filter the message
	if ((m_pSimpleFrameSite != NULL) &&
		SUCCEEDED(hr = m_pSimpleFrameSite->PreMessageFilter(
			m_hWnd, msg, wParam, lParam, &lResult, &dwCookie)))
	{
		if (hr == S_OK)
		{
			if (!IgnoreWindowMessage(msg, wParam, lParam, &lResult))
				lResult = CWnd::WindowProc(msg, wParam, lParam);

			// Simple frame site may have been cleared...
			// check before calling again.

			if (m_pSimpleFrameSite != NULL)
				m_pSimpleFrameSite->PostMessageFilter(
					m_hWnd, msg, wParam, lParam, &lResult, dwCookie);
		}
	}
	else
	{
		if (!IgnoreWindowMessage(msg, wParam, lParam, &lResult))
			lResult = CWnd::WindowProc(msg, wParam, lParam);
	}

	ExternalRelease();

	return lResult;
}

LRESULT COleControl::DefWindowProc(UINT nMsg, WPARAM wParam, LPARAM lParam)
{
	if (m_hWnd != NULL)
		return CWnd::DefWindowProc(nMsg, wParam, lParam);
	else
		return 0;
}

int COleControl::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (IsSubclassedControl())
		return CWnd::OnCreate(lpCreateStruct);
	else
		return 0;
}

void COleControl::OnSize(UINT nType, int cx, int cy)
{
	if (IsSubclassedControl())
		CWnd::OnSize(nType, cx, cy);
}

void COleControl::OnMove(int x, int y)
{
	if (IsSubclassedControl())
		CWnd::OnMove(x, y);
}

void COleControl::OnShowWindow(BOOL bShow, UINT nStatus)
{
	if (IsSubclassedControl())
		CWnd::OnShowWindow(bShow, nStatus);
}

/////////////////////////////////////////////////////////////////////////////
// Command prompts

void COleControl::OnInitMenuPopup(CMenu* pMenu, UINT, BOOL bSysMenu)
{
	AfxCancelModes(m_hWnd);

	if (bSysMenu)
		return;     // don't support system menu

	ASSERT(pMenu != NULL);
	// check the enabled state of various menu items

	CCmdUI state;
	state.m_pMenu = pMenu;
	ASSERT(state.m_pOther == NULL);
	ASSERT(state.m_pParentMenu == NULL);

	// determine if menu is popup in top-level menu and set m_pOther to
	// it if so (m_pParentMenu == NULL indicates that it is secondary popup)
	HMENU hParentMenu;
	if (_afxTrackingMenu == pMenu->m_hMenu)
		state.m_pParentMenu = pMenu;    // parent == child for tracking popup
	else
	{
		CWnd* pParent = GetTopLevelParent();
			// child windows don't have menus -- need to go to the top!
		if (pParent != NULL &&
			(hParentMenu = ::GetMenu(pParent->m_hWnd)) != NULL)
		{
			int nIndexMax = ::GetMenuItemCount(hParentMenu);
			for (int nIndex = 0; nIndex < nIndexMax; nIndex++)
			{
				if (::GetSubMenu(hParentMenu, nIndex) == pMenu->m_hMenu)
				{
					// when popup is found, m_pParentMenu is containing menu
					state.m_pParentMenu = CMenu::FromHandle(hParentMenu);
					break;
				}
			}
		}
	}

	state.m_nIndexMax = pMenu->GetMenuItemCount();
	for (state.m_nIndex = 0; state.m_nIndex < state.m_nIndexMax;
	  state.m_nIndex++)
	{
		state.m_nID = pMenu->GetMenuItemID(state.m_nIndex);
		if (state.m_nID == 0)
			continue; // menu separator or invalid cmd - ignore it

		ASSERT(state.m_pOther == NULL);
		ASSERT(state.m_pMenu != NULL);
		if (state.m_nID == (UINT)-1)
		{
			// possibly a popup menu, route to first item of that popup
			state.m_pSubMenu = pMenu->GetSubMenu(state.m_nIndex);
			if (state.m_pSubMenu == NULL ||
				(state.m_nID = state.m_pSubMenu->GetMenuItemID(0)) == 0 ||
				state.m_nID == (UINT)-1)
			{
				continue;       // first item of popup can't be routed to
			}
			state.DoUpdate(this, FALSE);    // popups are never auto disabled
		}
		else
		{
			// normal menu item
			// Auto enable/disable if frame window has 'm_bAutoMenuEnable'
			// set and command is _not_ a system command.
			state.m_pSubMenu = NULL;
			state.DoUpdate(this, m_bAutoMenuEnable && state.m_nID < 0xF000);
		}
	}
}

void COleControl::OnMenuSelect(UINT nItemID, UINT nFlags, HMENU /*hSysMenu*/)
{
	// set the tracking state (update on idle)
	if (nFlags == 0xFFFF)
	{
		m_nIDTracking = AFX_IDS_IDLEMESSAGE;
		SendMessage(WM_SETMESSAGESTRING, (WPARAM)m_nIDTracking);
		ASSERT(m_nIDTracking == m_nIDLastMessage);
	}
	else if (nItemID == 0 ||
		nFlags & (MF_SEPARATOR|MF_POPUP|MF_MENUBREAK|MF_MENUBARBREAK))
	{
		// nothing should be displayed
		m_nIDTracking = 0;
	}
	else if (nItemID >= 0xF000 && nItemID < 0xF1F0) // max of 31 SC_s
	{
		// special strings table entries for system commands
		m_nIDTracking = ID_COMMAND_FROM_SC(nItemID);
		ASSERT(m_nIDTracking >= AFX_IDS_SCFIRST &&
			m_nIDTracking < AFX_IDS_SCFIRST + 31);
	}
	else if (nItemID >= AFX_IDM_FIRST_MDICHILD)
	{
		// all MDI Child windows map to the same help id
		m_nIDTracking = AFX_IDS_MDICHILD;
	}
	else
	{
		// track on idle
		m_nIDTracking = nItemID;
	}

	// when running in-place, it is necessary to cause a message to
	// be pumped through the queue.
	if (m_nIDTracking != m_nIDLastMessage && GetParent() != NULL)
		PostMessage(WM_NULL);
}

void COleControl::GetMessageString(UINT nID, CString& rMessage) const
{
	// load appropriate string
	LPTSTR lpsz = rMessage.GetBuffer(255);
	if (AfxLoadString(nID, lpsz) != 0)
	{
		// first newline terminates actual string
		lpsz = _tcschr(lpsz, '\n');
		if (lpsz != NULL)
			*lpsz = '\0';
	}
	else
	{
		// not found
		TRACE1("Warning: no message line prompt for ID 0x%04X.\n", nID);
	}
	rMessage.ReleaseBuffer();
}

LRESULT COleControl::OnSetMessageString(WPARAM wParam, LPARAM lParam)
{
	USES_CONVERSION;

	if (m_pInPlaceFrame != NULL)
	{
		LPCTSTR lpsz = NULL;
		CString strMessage;

		// set the message bar text
		if (lParam != 0)
		{
			ASSERT(wParam == 0);    // can't have both an ID and a string
			lpsz = (LPCTSTR)lParam; // set an explicit string
		}
		else if (wParam != 0)
		{
			// use the wParam as a string ID
			GetMessageString(wParam, strMessage);
			lpsz = strMessage;
		}

		// notify container of new status text
		m_pInPlaceFrame->SetStatusText(T2COLE(lpsz));
	}

	UINT nIDLast = m_nIDLastMessage;
	m_nIDLastMessage = (UINT)wParam;    // new ID (or 0)
	m_nIDTracking = (UINT)wParam;       // so F1 on toolbar buttons work
	return nIDLast;
}

void COleControl::OnEnterIdle(UINT nWhy, CWnd* /*pWho*/)
{
	if (nWhy != MSGF_MENU || m_nIDTracking == m_nIDLastMessage)
		return;

	SendMessage(WM_SETMESSAGESTRING, (WPARAM)m_nIDTracking);
	ASSERT(m_nIDTracking == m_nIDLastMessage);
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XSpecifyPropertyPages

STDMETHODIMP_(ULONG) COleControl::XSpecifyPropertyPages::AddRef()
{
	// Delegate to our exported AddRef.
	METHOD_PROLOGUE_EX_(COleControl, SpecifyPropertyPages)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XSpecifyPropertyPages::Release()
{
	// Delegate to our exported Release.
	METHOD_PROLOGUE_EX_(COleControl, SpecifyPropertyPages)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XSpecifyPropertyPages::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	// Delegate to our exported QueryInterface.
	METHOD_PROLOGUE_EX_(COleControl, SpecifyPropertyPages)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XSpecifyPropertyPages::GetPages(CAUUID* pPages)
{
	METHOD_PROLOGUE_EX(COleControl, SpecifyPropertyPages)

	ASSERT(pPages != NULL);

	if (pPages == NULL)
		return E_POINTER;

	pPages->cElems = 0;
	pPages->pElems = NULL;

	HRESULT hr = S_OK;
	ULONG cElems;
	LPCLSID pClassID = pThis->GetPropPageIDs(cElems);

	if (cElems > 0)
	{
		if ((pPages->pElems = (LPCLSID)(CoTaskMemAlloc(cElems * sizeof(CLSID)))) != NULL)
		{
			ASSERT(pPages->pElems != NULL);
			pPages->cElems = cElems;
			memcpy(pPages->pElems, pClassID, (int)(cElems * sizeof(CLSID)));
		}
		else
			hr = E_OUTOFMEMORY;
	}
	else
	{
		pPages->cElems = 0;
		pPages->pElems = NULL;
	}

	return hr;
}

void COleControl::OnDestroy()
{
	// Release hfont, if any.
	if (m_hFontPrev != NULL)
	{
		SendMessage(WM_SETFONT, (WPARAM)NULL, 0);
		InternalGetFont().m_pFont->ReleaseHfont(m_hFontPrev);
		m_hFontPrev = NULL;
	}

	CWnd::OnDestroy();
}

void COleControl::OnKillFocus(CWnd* pNewWnd)
{
	CWnd::OnKillFocus(pNewWnd);

	if (m_pControlSite != NULL)
		m_pControlSite->OnFocus(FALSE);
}

void COleControl::OnSetFocus(CWnd* pOldWnd)
{
	CWnd::OnSetFocus(pOldWnd);

	if (m_pControlSite != NULL)
		m_pControlSite->OnFocus(TRUE);
}

LRESULT COleControl::OnOcmCtlColorBtn(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORBTN, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorDlg(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORDLG, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorEdit(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLOREDIT, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorListBox(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORLISTBOX, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorMsgBox(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORMSGBOX, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorScrollBar(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORSCROLLBAR, wParam, lParam);
}

LRESULT COleControl::OnOcmCtlColorStatic(WPARAM wParam, LPARAM lParam)
{
	return ::DefWindowProc(m_hWnd, WM_CTLCOLORSTATIC, wParam, lParam);
}

void COleControl::ThrowError(SCODE sc, UINT nDescriptionID, UINT nHelpID)
{
	TCHAR szBuffer[256];
	AfxLoadString(nDescriptionID, szBuffer);
	if (nHelpID == -1)
		nHelpID = nDescriptionID;
	ThrowError(sc, szBuffer, nHelpID);
}

void COleControl::ThrowError(SCODE sc, LPCTSTR pszDescription, UINT nHelpID)
{
	COleDispatchException* pExcept = new COleDispatchException(pszDescription,
		nHelpID, 0);
	pExcept->m_scError = sc;
	THROW(pExcept);
}

BOOL COleControl::IsInvokeAllowed(DISPID)
{
	return m_bInitialized;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleControl, CWnd)
