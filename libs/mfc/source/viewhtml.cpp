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

#ifdef AFX_HTML_SEG
#pragma code_seg(AFX_HTML_SEG)
#endif

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CHtmlView

BEGIN_MESSAGE_MAP(CHtmlView, CFormView)
	//{{AFX_MSG_MAP(CHtmlView)
	ON_WM_SIZE()
	ON_WM_PAINT()
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	// Standard printing commands
	ON_COMMAND(ID_FILE_PRINT, OnFilePrint)
END_MESSAGE_MAP()

BEGIN_EVENTSINK_MAP(CHtmlView, CFormView)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 102 /* StatusTextChange */, OnStatusTextChange, VTS_BSTR)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 108 /* ProgressChange */, OnProgressChange, VTS_I4 VTS_I4)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 105 /* CommandStateChange */, OnCommandStateChange, VTS_I4 VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 106 /* DownloadBegin */, OnDownloadBegin, VTS_NONE)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 104 /* DownloadComplete */, OnDownloadComplete, VTS_NONE)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 113 /* TitleChange */, OnTitleChange, VTS_BSTR)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 252 /* NavigateComplete2 */, NavigateComplete2, VTS_DISPATCH VTS_PVARIANT)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 250 /* BeforeNavigate2 */, BeforeNavigate2, VTS_DISPATCH VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PVARIANT VTS_PBOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 112 /* PropertyChange */, OnPropertyChange, VTS_BSTR)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 251 /* NewWindow2 */, OnNewWindow2, VTS_PDISPATCH VTS_PBOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 259 /* DocumentComplete */, DocumentComplete, VTS_DISPATCH VTS_PVARIANT)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 253 /* OnQuit */, OnQuit, VTS_NONE)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 254 /* OnVisible */, OnVisible, VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 255 /* OnToolBar */, OnToolBar, VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 256 /* OnMenuBar */, OnMenuBar, VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 257 /* OnStatusBar */, OnStatusBar, VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 258 /* OnFullScreen */, OnFullScreen, VTS_BOOL)
	ON_EVENT(CHtmlView, AFX_IDW_PANE_FIRST, 260 /* OnTheaterMode */, OnTheaterMode, VTS_BOOL)
END_EVENTSINK_MAP()

/////////////////////////////////////////////////////////////////////////////
// CHtmlView construction/destruction

CHtmlView::CHtmlView()
	: CFormView((LPCTSTR) NULL)
{
	m_pBrowserApp = NULL;
}

CHtmlView::~CHtmlView()
{
	if (m_pBrowserApp != NULL)
		m_pBrowserApp->Release();
}


/////////////////////////////////////////////////////////////////////////////
// CHtmlView drawing

void CHtmlView::OnDraw(CDC* /* pDC */)
{
	// this class should never do its own drawing;
	// the browser control should handle everything

	ASSERT(FALSE);
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView printing

void CHtmlView::OnFilePrint()
{
	// get the HTMLDocument

	if (m_pBrowserApp != NULL)
	{
		LPOLECOMMANDTARGET lpTarget = NULL;
		LPDISPATCH lpDisp = GetHtmlDocument();

		if (lpDisp != NULL)
		{
			// the control will handle all printing UI

			if (SUCCEEDED(lpDisp->QueryInterface(IID_IOleCommandTarget,
					(LPVOID*) &lpTarget)))
			{
				lpTarget->Exec(NULL, OLECMDID_PRINT, 0, NULL, NULL);
				lpTarget->Release();
			}
			lpDisp->Release();
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView diagnostics

#ifdef _DEBUG
void CHtmlView::AssertValid() const
{
	CFormView::AssertValid();
}

void CHtmlView::Dump(CDumpContext& dc) const
{
	CFormView::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CHtmlView message handlers

void CHtmlView::OnDestroy()
{
	RELEASE(m_pBrowserApp);
}

void CHtmlView::OnSize(UINT nType, int cx, int cy)
{
	CFormView::OnSize(nType, cx, cy);

	if (::IsWindow(m_wndBrowser.m_hWnd))
	{
		// need to push non-client borders out of the client area
		CRect rect;
		GetClientRect(rect);
		::AdjustWindowRectEx(rect,
			m_wndBrowser.GetStyle(), FALSE, WS_EX_CLIENTEDGE);
		m_wndBrowser.SetWindowPos(NULL, rect.left, rect.top,
			rect.Width(), rect.Height(), SWP_NOACTIVATE | SWP_NOZORDER);
	}
}

void CHtmlView::OnPaint()
{
	Default();
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView operations

BOOL CHtmlView::Create(LPCTSTR lpszClassName, LPCTSTR lpszWindowName,
						DWORD dwStyle, const RECT& rect, CWnd* pParentWnd,
						UINT nID, CCreateContext* pContext)
{
	// create the view window itself
	m_pCreateContext = pContext;
	if (!CView::Create(lpszClassName, lpszWindowName,
				dwStyle, rect, pParentWnd,  nID, pContext))
	{
		return FALSE;
	}

	AfxEnableControlContainer();

	RECT rectClient;
	GetClientRect(&rectClient);

	// create the control window
	// AFX_IDW_PANE_FIRST is a safe but arbitrary ID
	if (!m_wndBrowser.CreateControl(CLSID_WebBrowser, lpszWindowName,
				WS_VISIBLE | WS_CHILD, rectClient, this, AFX_IDW_PANE_FIRST))
	{
		DestroyWindow();
		return FALSE;
	}

	LPUNKNOWN lpUnk = m_wndBrowser.GetControlUnknown();
	HRESULT hr = lpUnk->QueryInterface(IID_IWebBrowser2, (void**) &m_pBrowserApp);
	if (!SUCCEEDED(hr))
	{
		m_pBrowserApp = NULL;
		m_wndBrowser.DestroyWindow();
		DestroyWindow();
		return FALSE;
	}

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView properties

CString CHtmlView::GetType() const
{
	ASSERT(m_pBrowserApp != NULL);

	BSTR bstr;
	m_pBrowserApp->get_Type(&bstr);
	CString retVal(bstr);
	return retVal;
}

long CHtmlView::GetLeft() const
{
	ASSERT(m_pBrowserApp != NULL);

	long result;
	m_pBrowserApp->get_Left(&result);
	return result;
}


long CHtmlView::GetTop() const
{
	ASSERT(m_pBrowserApp != NULL);
	long result;
	m_pBrowserApp->get_Top(&result);
	return result;
}

int CHtmlView::GetToolBar() const
{
	ASSERT(m_pBrowserApp != NULL);
	int result;
	m_pBrowserApp->get_ToolBar(&result);
	return result;
}

long CHtmlView::GetHeight() const
{
	ASSERT(m_pBrowserApp != NULL);
	long result;
	m_pBrowserApp->get_Height(&result);
	return result;
}

BOOL CHtmlView::GetVisible() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_Visible(&result);
	return result;
}

CString CHtmlView::GetLocationName() const
{
	ASSERT(m_pBrowserApp != NULL);

	BSTR bstr;
	m_pBrowserApp->get_LocationName(&bstr);
	CString retVal(bstr);
	return retVal;
}

CString CHtmlView::GetLocationURL() const
{
	ASSERT(m_pBrowserApp != NULL);

	BSTR bstr;
	m_pBrowserApp->get_LocationURL(&bstr);
	CString retVal(bstr);
	return retVal;
}

BOOL CHtmlView::GetBusy() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_Busy(&result);
	return result;
}

READYSTATE CHtmlView::GetReadyState() const
{
	ASSERT(m_pBrowserApp != NULL);

	READYSTATE result;
	m_pBrowserApp->get_ReadyState(&result);
	return result;
}

BOOL CHtmlView::GetOffline() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_Offline(&result);
	return result;
}

BOOL CHtmlView::GetSilent() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_Silent(&result);
	return result;
}

LPDISPATCH CHtmlView::GetApplication() const
{
	ASSERT(m_pBrowserApp != NULL);

	LPDISPATCH result;
	m_pBrowserApp->get_Application(&result);
	return result;
}


LPDISPATCH CHtmlView::GetParentBrowser() const
{
	ASSERT(m_pBrowserApp != NULL);

	LPDISPATCH result;
	m_pBrowserApp->get_Parent(&result);
	return result;
}

LPDISPATCH CHtmlView::GetContainer() const
{
	ASSERT(m_pBrowserApp != NULL);

	LPDISPATCH result;
	m_pBrowserApp->get_Container(&result);
	return result;
}

LPDISPATCH CHtmlView::GetHtmlDocument() const
{
	ASSERT(m_pBrowserApp != NULL);

	LPDISPATCH result;
	m_pBrowserApp->get_Document(&result);
	return result;
}

BOOL CHtmlView::GetTopLevelContainer() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_TopLevelContainer(&result);
	return result;
}

BOOL CHtmlView::GetMenuBar() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_MenuBar(&result);
	return result;
}

BOOL CHtmlView::GetFullScreen() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_FullScreen(&result);
	return result;
}

BOOL CHtmlView::GetStatusBar() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_StatusBar(&result);
	return result;
}

OLECMDF CHtmlView::QueryStatusWB(OLECMDID cmdID) const
{
	ASSERT(m_pBrowserApp != NULL);

	OLECMDF result;
	m_pBrowserApp->QueryStatusWB(cmdID, &result);
	return result;
}

void CHtmlView::ExecWB(OLECMDID cmdID, OLECMDEXECOPT cmdexecopt,
	VARIANT* pvaIn, VARIANT* pvaOut)
{
	ASSERT(m_pBrowserApp != NULL);

	m_pBrowserApp->ExecWB(cmdID, cmdexecopt, pvaIn, pvaOut);
}

BOOL CHtmlView::GetRegisterAsBrowser() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_RegisterAsBrowser(&result);
	return result;
}

BOOL CHtmlView::GetRegisterAsDropTarget() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_RegisterAsDropTarget(&result);
	return result;
}

BOOL CHtmlView::GetTheaterMode() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_TheaterMode(&result);
	return result;
}

BOOL CHtmlView::GetAddressBar() const
{
	ASSERT(m_pBrowserApp != NULL);

	VARIANT_BOOL result;
	m_pBrowserApp->get_AddressBar(&result);
	return result;
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView operations

BOOL CHtmlView::LoadFromResource(LPCTSTR lpszResource)
{
	HINSTANCE hInstance = AfxGetResourceHandle();
	ASSERT(hInstance != NULL);

	CString strResourceURL;
	BOOL bRetVal = TRUE;
	LPTSTR lpszModule = new TCHAR[_MAX_PATH];

	if (GetModuleFileName(hInstance, lpszModule, _MAX_PATH))
	{
		strResourceURL.Format(_T("res://%s/%s"), lpszModule, lpszResource);
		Navigate(strResourceURL, 0, 0, 0);
	}
	else
		bRetVal = FALSE;

	delete [] lpszModule;
	return bRetVal;
}

BOOL CHtmlView::LoadFromResource(UINT nRes)
{
	HINSTANCE hInstance = AfxGetResourceHandle();
	ASSERT(hInstance != NULL);

	CString strResourceURL;
	BOOL bRetVal = TRUE;
	LPTSTR lpszModule = new TCHAR[_MAX_PATH];

	if (GetModuleFileName(hInstance, lpszModule, _MAX_PATH))
	{
		strResourceURL.Format(_T("res://%s/%d"), lpszModule, nRes);
		Navigate(strResourceURL, 0, 0, 0);
	}
	else
		bRetVal = FALSE;

	delete [] lpszModule;
	return bRetVal;
}

void CHtmlView::Navigate(LPCTSTR lpszURL, DWORD dwFlags /* = 0 */,
	LPCTSTR lpszTargetFrameName /* = NULL */ ,
	LPCTSTR lpszHeaders /* = NULL */, LPVOID lpvPostData /* = NULL */,
	DWORD dwPostDataLen /* = 0 */)
{
	CString strURL(lpszURL);
	BSTR bstrURL = strURL.AllocSysString();

	COleSafeArray vPostData;
	if (lpvPostData != NULL)
	{
		if (dwPostDataLen == 0)
			dwPostDataLen = lstrlen((LPCTSTR) lpvPostData);

		vPostData.CreateOneDim(VT_UI1, dwPostDataLen, lpvPostData);
	}

	m_pBrowserApp->Navigate(bstrURL,
		COleVariant((long) dwFlags, VT_I4),
		COleVariant(lpszTargetFrameName, VT_BSTR),
		vPostData,
		COleVariant(lpszHeaders, VT_BSTR));
}

void CHtmlView::Navigate2(LPITEMIDLIST pIDL, DWORD dwFlags /* = 0 */,
	LPCTSTR lpszTargetFrameName /* = NULL */)
{
	ASSERT(m_pBrowserApp != NULL);

	COleVariant vPIDL(pIDL);
	COleVariant empty;

	m_pBrowserApp->Navigate2(vPIDL,
		COleVariant((long) dwFlags, VT_I4),
		COleVariant(lpszTargetFrameName, VT_BSTR),
		empty, empty);
}

void CHtmlView::Navigate2(LPCTSTR lpszURL, DWORD dwFlags /* = 0 */,
	LPCTSTR lpszTargetFrameName /* = NULL */,
	LPCTSTR lpszHeaders /* = NULL */,
	LPVOID lpvPostData /* = NULL */, DWORD dwPostDataLen /* = 0 */)
{
	ASSERT(m_pBrowserApp != NULL);

	COleSafeArray vPostData;
	if (lpvPostData != NULL)
	{
		if (dwPostDataLen == 0)
			dwPostDataLen = lstrlen((LPCTSTR) lpvPostData);

		vPostData.CreateOneDim(VT_UI1, dwPostDataLen, lpvPostData);
	}

	COleVariant vURL(lpszURL, VT_BSTR);
	COleVariant vHeaders(lpszHeaders, VT_BSTR);
	COleVariant vTargetFrameName(lpszTargetFrameName, VT_BSTR);
	COleVariant vFlags((long) dwFlags, VT_I4);

	m_pBrowserApp->Navigate2(vURL,
		vFlags, vTargetFrameName, vPostData, vHeaders);
}

void CHtmlView::Navigate2(LPCTSTR lpszURL, DWORD dwFlags,
	CByteArray& baPostData, LPCTSTR lpszTargetFrameName /* = NULL */,
	LPCTSTR lpszHeaders /* = NULL */)
{
	ASSERT(m_pBrowserApp != NULL);

	COleVariant vPostData = baPostData;
	COleVariant vURL(lpszURL, VT_BSTR);
	COleVariant vHeaders(lpszHeaders, VT_BSTR);
	COleVariant vTargetFrameName(lpszTargetFrameName, VT_BSTR);
	COleVariant vFlags((long) dwFlags, VT_I4);

	ASSERT(m_pBrowserApp != NULL);

	m_pBrowserApp->Navigate2(vURL, vFlags, vTargetFrameName,
		vPostData, vHeaders);
}

void CHtmlView::PutProperty(LPCTSTR lpszProperty, const VARIANT& vtValue)
{
	ASSERT(m_pBrowserApp != NULL);

	CString strProp(lpszProperty);
	BSTR bstrProp = strProp.AllocSysString();
	m_pBrowserApp->PutProperty(bstrProp, vtValue);
	::SysFreeString(bstrProp);
}

BOOL CHtmlView::GetProperty(LPCTSTR lpszProperty, CString& strValue)
{
	ASSERT(m_pBrowserApp != NULL);

	CString strProperty(lpszProperty);
	BSTR bstrProperty = strProperty.AllocSysString();

	BOOL bResult = FALSE;
	VARIANT vReturn;
	vReturn.vt = VT_BSTR;
	vReturn.bstrVal = NULL;
	HRESULT hr = m_pBrowserApp->GetProperty(bstrProperty, &vReturn);

	if (SUCCEEDED(hr))
	{
		strValue = CString(vReturn.bstrVal);
		bResult = TRUE;
	}

	::SysFreeString(bstrProperty);
	return bResult;
}

COleVariant CHtmlView::GetProperty(LPCTSTR lpszProperty)
{
	COleVariant result;

	static BYTE parms[] =
		VTS_BSTR;
	m_wndBrowser.InvokeHelper(0x12f, DISPATCH_METHOD,
		VT_VARIANT, (void*)&result, parms, lpszProperty);

	return result;
}

CString CHtmlView::GetFullName() const
{
	ASSERT(m_pBrowserApp != NULL);

	BSTR bstr;
	m_pBrowserApp->get_FullName(&bstr);
	CString retVal(bstr);
	return retVal;
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView event reflectors

void CHtmlView::NavigateComplete2(LPDISPATCH /* pDisp */, VARIANT* URL)
{
	ASSERT(V_VT(URL) == VT_BSTR);

	USES_CONVERSION;

	CString str = OLE2T(V_BSTR(URL));
	OnNavigateComplete2(str);
}

void CHtmlView::BeforeNavigate2(LPDISPATCH /* pDisp */, VARIANT* URL,
		VARIANT* Flags, VARIANT* TargetFrameName,
		VARIANT* PostData, VARIANT* Headers, BOOL* Cancel)
{
	ASSERT(V_VT(URL) == VT_BSTR);
	ASSERT(V_VT(TargetFrameName) == VT_BSTR);
	ASSERT(V_VT(PostData) == (VT_VARIANT | VT_BYREF));
	ASSERT(V_VT(Headers) == VT_BSTR);
	ASSERT(Cancel != NULL);

	USES_CONVERSION;

	VARIANT* vtPostedData = V_VARIANTREF(PostData);
	CByteArray array;
	if (V_VT(vtPostedData) & VT_ARRAY)
	{
		// must be a vector of bytes
		ASSERT(vtPostedData->parray->cDims == 1 && vtPostedData->parray->cbElements == 1);

		vtPostedData->vt |= VT_UI1;
		COleSafeArray safe(vtPostedData);

		DWORD dwSize = safe.GetOneDimSize();
		LPVOID pVoid;
		safe.AccessData(&pVoid);

		array.SetSize(dwSize);
		LPBYTE lpByte = array.GetData();

		memcpy(lpByte, pVoid, dwSize);
		safe.UnaccessData();
	}
	// make real parameters out of the notification

	CString strTargetFrameName(V_BSTR(TargetFrameName));
	CString strURL = V_BSTR(URL);
	CString strHeaders = V_BSTR(Headers);
	DWORD nFlags = V_I4(Flags);

	// notify the user's class
	OnBeforeNavigate2(strURL, nFlags, strTargetFrameName,
		array, strHeaders, Cancel);
}

void CHtmlView::DocumentComplete(LPDISPATCH pDisp, VARIANT* URL)
{
	UNUSED_ALWAYS(pDisp);
	ASSERT(V_VT(URL) == VT_BSTR);

	CString str(V_BSTR(URL));
	OnDocumentComplete(str);
}

/////////////////////////////////////////////////////////////////////////////
// CHtmlView Events

void CHtmlView::OnProgressChange(long lProgress, long lProgressMax)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lProgress);
	UNUSED_ALWAYS(lProgressMax);
}

void CHtmlView::OnCommandStateChange(long lCommand, BOOL bEnable)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lCommand);
	UNUSED_ALWAYS(bEnable);
}

void CHtmlView::OnDownloadBegin()
{
	// user will override to handle this notification
}

void CHtmlView::OnDownloadComplete()
{
	// user will override to handle this notification
}

void CHtmlView::OnTitleChange(LPCTSTR lpszText)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lpszText);
}

void CHtmlView::OnPropertyChange(LPCTSTR lpszProperty)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lpszProperty);
}

void CHtmlView::OnNewWindow2(LPDISPATCH* ppDisp, BOOL* bCancel)
{
	// default to continuing
	bCancel = FALSE;

	// user will override to handle this notification
	UNUSED_ALWAYS(ppDisp);
}

void CHtmlView::OnDocumentComplete(LPCTSTR lpszURL)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lpszURL);
}

void CHtmlView::OnQuit()
{
	// user will override to handle this notification
}

void CHtmlView::OnVisible(BOOL bVisible)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bVisible);
}

void CHtmlView::OnToolBar(BOOL bToolBar)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bToolBar);
}

void CHtmlView::OnMenuBar(BOOL bMenuBar)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bMenuBar);
}

void CHtmlView::OnStatusBar(BOOL bStatusBar)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bStatusBar);
}

void CHtmlView::OnFullScreen(BOOL bFullScreen)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bFullScreen);
}

void CHtmlView::OnTheaterMode(BOOL bTheaterMode)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(bTheaterMode);
}

void CHtmlView::OnNavigateComplete2(LPCTSTR lpszURL)
{
	// user will override to handle this notification
	UNUSED_ALWAYS(lpszURL);
}

void CHtmlView::OnBeforeNavigate2(LPCTSTR lpszURL, DWORD nFlags,
	LPCTSTR lpszTargetFrameName, CByteArray& baPostData,
	LPCTSTR lpszHeaders, BOOL* bCancel)
{
	// default to continuing
	bCancel = FALSE;

	// user will override to handle this notification
	UNUSED_ALWAYS(lpszURL);
	UNUSED_ALWAYS(nFlags);
	UNUSED_ALWAYS(lpszTargetFrameName);
	UNUSED_ALWAYS(baPostData);
	UNUSED_ALWAYS(lpszHeaders);
}

void CHtmlView::OnStatusTextChange(LPCTSTR pszText)
{
	// try to set the status bar text via the frame

	CFrameWnd* pFrame = GetParentFrame();
	if (pFrame != NULL)
		pFrame->SetMessageText(pszText);
}

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

// expand inlines for Html functions
static char _szAfxHtmlInl[] = "afxhtml.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxHtmlInl
#define _AFXHTML_INLINE
#include "afxhtml.inl"

#endif //!_AFX_ENABLE_INLINES

/////////////////////////////////////////////////////////////////////////////
// Pre-startup code

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CHtmlView, CFormView)
