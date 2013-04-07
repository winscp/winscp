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
#include "occimpl.h"

#ifdef AFX_OCC_SEG
#pragma code_seg(AFX_OCC_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CWnd support for OLE Control containment

BOOL CWnd::CreateControl(LPCTSTR lpszClass, LPCTSTR lpszWindowName,
	DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID,
	CFile* pPersist, BOOL bStorage, BSTR bstrLicKey)
{
	ASSERT(lpszClass != NULL);

	CLSID clsid;
	HRESULT hr = AfxGetClassIDFromString(lpszClass, &clsid);
	if (FAILED(hr))
		return FALSE;

	return CreateControl(clsid, lpszWindowName, dwStyle, rect, pParentWnd, nID,
		pPersist, bStorage, bstrLicKey);
}

BOOL CWnd::CreateControl( REFCLSID clsid, LPCTSTR lpszWindowName,
   DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID,
   CFile* pPersist, BOOL bStorage, BSTR bstrLicKey )
{
   CRect rect2( rect );
   CPoint pt;
   CSize size;

   pt = rect2.TopLeft();
   size = rect2.Size();

   return( CreateControl( clsid, lpszWindowName, dwStyle, &pt, &size,
	  pParentWnd, nID, pPersist, bStorage, bstrLicKey ) );
}

BOOL CWnd::CreateControl(REFCLSID clsid, LPCTSTR lpszWindowName, DWORD dwStyle,
	const POINT* ppt, const SIZE* psize, CWnd* pParentWnd, UINT nID,
   CFile* pPersist, BOOL bStorage, BSTR bstrLicKey)
{
	ASSERT(pParentWnd != NULL);

#ifdef _DEBUG
	if (afxOccManager == NULL)
	{
		TRACE0("Warning: AfxEnableControlContainer has not been called yet.\n");
		TRACE0(">>> You should call it in your app's InitInstance function.\n");
	}
#endif

	if ((pParentWnd == NULL) || !pParentWnd->InitControlContainer())
		return FALSE;

	return pParentWnd->m_pCtrlCont->CreateControl(this, clsid, lpszWindowName,
		dwStyle, ppt, psize, nID, pPersist, bStorage, bstrLicKey);
}

BOOL CWnd::InitControlContainer()
{
	TRY
	{
		if (m_pCtrlCont == NULL)
			m_pCtrlCont = afxOccManager->CreateContainer(this);
	}
	END_TRY

	// Mark all ancestor windows as containing OLE controls.
	if (m_pCtrlCont != NULL)
	{
		CWnd* pWnd = this;
		while ((pWnd != NULL) && !(pWnd->m_nFlags & WF_OLECTLCONTAINER))
		{
			pWnd->m_nFlags |= WF_OLECTLCONTAINER;
			pWnd = pWnd->GetParent();
			if (! (GetWindowLong(pWnd->GetSafeHwnd(), GWL_STYLE) & WS_CHILD))
				break;
		}
	}

	return (m_pCtrlCont != NULL);
}

/////////////////////////////////////////////////////////////////////////////
// COleControlContainer

BEGIN_INTERFACE_MAP(COleControlContainer, CCmdTarget)
	INTERFACE_PART(COleControlContainer, IID_IOleInPlaceFrame, OleIPFrame)
	INTERFACE_PART(COleControlContainer, IID_IOleContainer, OleContainer)
END_INTERFACE_MAP()

BEGIN_DISPATCH_MAP(COleControlContainer, CCmdTarget)
END_DISPATCH_MAP()

COleControlContainer::COleControlContainer(CWnd* pWnd) :
	m_pWnd(pWnd),
	m_crBack((COLORREF)-1),
	m_crFore((COLORREF)-1),
	m_pOleFont(NULL),
	m_pSiteUIActive(NULL)
{
}

COleControlContainer::~COleControlContainer()
{
	HWND hWnd;
	COleControlSite* pSite;
	POSITION pos = m_siteMap.GetStartPosition();
	while (pos != NULL)
	{
		m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
		if (!(pSite->m_pDataSourceControl))
		{
			m_siteMap.RemoveKey((void*&)hWnd);
			delete pSite;
		}
	}

	pos = m_siteMap.GetStartPosition();
	while (pos != NULL)
	{
		m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
		delete pSite;
	}
	m_siteMap.RemoveAll();

	RELEASE(m_pOleFont);
}

BOOL COleControlContainer::CreateControl( CWnd* pWndCtrl, REFCLSID clsid,
	LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, UINT nID,
	CFile* pPersist, BOOL bStorage, BSTR bstrLicKey,
	COleControlSite** ppNewSite )
{
   CRect rect2( rect );
   CPoint pt;
   CSize size;

   pt = rect2.TopLeft();
   size = rect2.Size();

   return( CreateControl( pWndCtrl, clsid, lpszWindowName, dwStyle, &pt, &size,
	  nID, pPersist, bStorage, bstrLicKey, ppNewSite ) );
}

BOOL COleControlContainer::CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
	LPCTSTR lpszWindowName, DWORD dwStyle, const POINT* ppt, const SIZE* psize,
   UINT nID, CFile* pPersist, BOOL bStorage, BSTR bstrLicKey,
   COleControlSite** ppNewSite)
{
	COleControlSite* pSite = NULL;

	TRY
	{
		pSite = afxOccManager->CreateSite(this);
	}
	END_TRY

	if (pSite == NULL)
		return FALSE;

	BOOL bCreated = SUCCEEDED( pSite->CreateControl(pWndCtrl, clsid,
		lpszWindowName, dwStyle, ppt, psize, nID, pPersist, bStorage,
	  bstrLicKey ) );

	if (bCreated)
	{
		ASSERT(pSite->m_hWnd != NULL);
		m_siteMap.SetAt(pSite->m_hWnd, pSite);
		if (ppNewSite != NULL)
			*ppNewSite = pSite;
	}
	else
	{
		delete pSite;
	}

	return bCreated;
}

COleControlSite* COleControlContainer::FindItem(UINT nID) const
{
	POSITION pos = m_siteMap.GetStartPosition();
	while (pos != NULL)
	{
		HWND hWnd;
		COleControlSite* pSite;
		m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
		if (pSite->GetID() == nID)
			return pSite;
	}
	return NULL;
}

BOOL COleControlContainer::GetAmbientProp(COleControlSite* pSite, DISPID dispid,
	VARIANT* pvarResult)
{
	switch (dispid)
	{
	case DISPID_AMBIENT_AUTOCLIP:
	case DISPID_AMBIENT_MESSAGEREFLECT:
	case DISPID_AMBIENT_SUPPORTSMNEMONICS:
	case DISPID_AMBIENT_USERMODE:
		V_VT(pvarResult) = VT_BOOL;
		V_BOOL(pvarResult) = (VARIANT_BOOL)-1;
		return TRUE;

	case DISPID_AMBIENT_SHOWGRABHANDLES:
	case DISPID_AMBIENT_SHOWHATCHING:
	case DISPID_AMBIENT_UIDEAD:
		V_VT(pvarResult) = VT_BOOL;
		V_BOOL(pvarResult) = 0;
		return TRUE;

	case DISPID_AMBIENT_APPEARANCE:     // ambient appearance is 3D
		V_VT(pvarResult) = VT_I2;
#ifndef _AFX_NO_CTL3D_SUPPORT
		if (afxData.bWin4 || AfxGetCtl3dState()->m_pfnSubclassDlgEx != NULL)
#else
		if (afxData.bWin4)
#endif
			V_I2(pvarResult) = 1;
		else
			V_I2(pvarResult) = 0;
		return TRUE;

	case DISPID_AMBIENT_BACKCOLOR:
	case DISPID_AMBIENT_FORECOLOR:
		if (m_crBack == (COLORREF)-1)   // ambient colors not initialized
		{
			CWindowDC dc(m_pWnd);
			m_pWnd->SendMessage(WM_CTLCOLORSTATIC, (WPARAM)dc.m_hDC,
				(LPARAM)m_pWnd->m_hWnd);
			m_crBack = dc.GetBkColor();
			m_crFore = dc.GetTextColor();
		}

		V_VT(pvarResult) = VT_COLOR;
		V_I4(pvarResult) = (dispid == DISPID_AMBIENT_BACKCOLOR) ?
			m_crBack : m_crFore;
		return TRUE;

	case DISPID_AMBIENT_FONT:
		if (m_pOleFont == NULL)         // ambient font not initialized
			CreateOleFont(m_pWnd->GetFont());

		ASSERT(m_pOleFont != NULL);
		if (m_pOleFont == NULL)         // failed to create font
			return FALSE;

		V_VT(pvarResult) = VT_FONT;
		m_pOleFont->AddRef();
		V_DISPATCH(pvarResult) = m_pOleFont;
		return TRUE;

	case DISPID_AMBIENT_DISPLAYASDEFAULT:
		V_VT(pvarResult) = VT_BOOL;
		V_BOOL(pvarResult) = (VARIANT_BOOL)(pSite->IsDefaultButton() ? -1 : 0);
		return TRUE;

	case DISPID_AMBIENT_LOCALEID:
		V_VT(pvarResult) = VT_I4;
		V_I4(pvarResult) = GetThreadLocale();
		return TRUE;

	case DISPID_AMBIENT_DISPLAYNAME:
		{
			CString str;                // return blank string
			V_VT(pvarResult) = VT_BSTR;
			V_BSTR(pvarResult) = str.AllocSysString();
		}
		return TRUE;

	case DISPID_AMBIENT_SCALEUNITS:
		{
			CString str;
			str.LoadString(AFX_IDS_OCC_SCALEUNITS_PIXELS);
			V_VT(pvarResult) = VT_BSTR;
			V_BSTR(pvarResult) = str.AllocSysString();
		}
		return TRUE;
	}

	return FALSE;
}

void COleControlContainer::CreateOleFont(CFont* pFont)
{
	USES_CONVERSION;

	CFont fontSys;
	if ((pFont == NULL) || (pFont->m_hObject == NULL))
	{
		// no font was provided, so use the system font
		if (fontSys.CreateStockObject(DEFAULT_GUI_FONT) ||
			fontSys.CreateStockObject(SYSTEM_FONT))
		{
			pFont = &fontSys;
		}
		else
		{
			m_pOleFont = NULL;
			return;
		}
	}

	LOGFONT logfont;
	pFont->GetLogFont(&logfont);

	FONTDESC fd;
	fd.cbSizeofstruct = sizeof(FONTDESC);
	fd.lpstrName = T2OLE(logfont.lfFaceName);
	fd.sWeight = (short)logfont.lfWeight;
	fd.sCharset = logfont.lfCharSet;
	fd.fItalic = logfont.lfItalic;
	fd.fUnderline = logfont.lfUnderline;
	fd.fStrikethrough = logfont.lfStrikeOut;

	long lfHeight = logfont.lfHeight;
	if (lfHeight < 0)
		lfHeight = -lfHeight;

	CWindowDC dc(m_pWnd);
	int ppi = dc.GetDeviceCaps(LOGPIXELSY);
	fd.cySize.Lo = lfHeight * 720000 / ppi;
	fd.cySize.Hi = 0;

	RELEASE(m_pOleFont);
	if (FAILED(::OleCreateFontIndirect(&fd, IID_IFontDisp, (void**)&m_pOleFont)))
		m_pOleFont = NULL;
}

void COleControlContainer::FreezeAllEvents(BOOL bFreeze)
{
	HWND hWnd;
	COleControlSite* pSite;
	POSITION pos = m_siteMap.GetStartPosition();
	while (pos != NULL)
	{
		m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
		pSite->FreezeEvents(bFreeze);
	}
}

void COleControlContainer::ScrollChildren(int dx, int dy)
{
	HWND hWnd;
	COleControlSite* pSite;
	POSITION pos = m_siteMap.GetStartPosition();
	while (pos != NULL)
	{
		m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
		ASSERT(pSite->m_pInPlaceObject != NULL);
		ASSERT(pSite->m_pObject != NULL);
		pSite->m_rect.OffsetRect(dx, dy);
		pSite->m_pInPlaceObject->SetObjectRects(pSite->m_rect, pSite->m_rect);
	}
}

void COleControlContainer::OnUIActivate(COleControlSite* pSite)
{
	if (m_pSiteUIActive != NULL)
		m_pSiteUIActive->m_pInPlaceObject->UIDeactivate();

	ASSERT(m_pSiteUIActive == NULL);    // did control call OnUIDeactivate?
	m_pSiteUIActive = pSite;
}

void COleControlContainer::OnUIDeactivate(COleControlSite* pSite)
{
	UNUSED(pSite);

	if (m_pSiteUIActive == pSite)
		m_pSiteUIActive = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// special cases for CWnd functions

void COleControlContainer::CheckDlgButton(int nIDButton, UINT nCheck)
{
	CWnd* pWnd = GetDlgItem(nIDButton);
	if (pWnd == NULL)
		return;

	if (pWnd->m_pCtrlSite == NULL)
	{
		pWnd->SendMessage(BM_SETCHECK, nCheck, 0);
		return;
	}

	pWnd->m_pCtrlSite->SafeSetProperty(DISPID_VALUE, VT_I4, (DWORD)nCheck);
}

void COleControlContainer::CheckRadioButton(int nIDFirstButton, int nIDLastButton,
	int nIDCheckButton)
{
	ASSERT(nIDFirstButton <= nIDCheckButton);
	ASSERT(nIDCheckButton <= nIDLastButton);

	// the following code is for OLE control containers only
	for (int nID = nIDFirstButton; nID <= nIDLastButton; nID++)
		CheckDlgButton(nID, (nID == nIDCheckButton));
}

CWnd* COleControlContainer::GetDlgItem(int nID) const
{
	HWND hWnd;
	GetDlgItem(nID, &hWnd);
	return CWnd::FromHandle(hWnd);
}

void COleControlContainer::GetDlgItem(int nID, HWND* phWnd) const
{
	// first, look for a non-OLE control
	HWND hWnd = ::GetDlgItem(m_pWnd->GetSafeHwnd(), nID);
	if (hWnd == NULL)
	{
		// now, look for an OLE control
		COleControlSite* pSite = FindItem(nID);
		if (pSite != NULL)
			hWnd = pSite->m_hWnd;
	}

	*phWnd = hWnd;
}

UINT COleControlContainer::GetDlgItemInt(int nID, BOOL* lpTrans, BOOL bSigned) const
{
	TCHAR szText[256];
	if (GetDlgItemText(nID, szText, 256) == 0)
	{
		if (lpTrans != NULL)
			*lpTrans = FALSE;
		return 0;
	}

	// Quick check for valid number
	LPTSTR pch = szText;

	while (_istspace(*pch))
		pch = CharNext(pch);    // skip whitespace

	if ((*pch) == '+' || (*pch) == '-')
		pch = CharNext(pch);    // skip sign

	BOOL bTrans = _istdigit(*pch);    // did we find a digit?

	if (lpTrans != NULL)
		*lpTrans = bTrans;

	if (!bTrans)
		return 0;

	if (bSigned)
		return _tcstol(szText, NULL, 10);
	else
		return _tcstoul(szText, NULL, 10);
}

int COleControlContainer::GetDlgItemText(int nID, LPTSTR lpStr, int nMaxCount) const
{
	CWnd* pWnd = GetDlgItem(nID);
	if (pWnd == NULL)
		return 0;

	return pWnd->GetWindowText(lpStr, nMaxCount);
}

LRESULT COleControlContainer::SendDlgItemMessage(int nID, UINT message, WPARAM wParam,
	LPARAM lParam)
{
	CWnd* pWnd = GetDlgItem(nID);
	if (pWnd == NULL)
		return 0;

	return pWnd->SendMessage(message, wParam, lParam);
}

void COleControlContainer::SetDlgItemInt(int nID, UINT nValue, BOOL bSigned)
{
	TCHAR szText[34];
	if (bSigned)
		_ltot((long)nValue, szText, 10);
	else
		_ultot((unsigned long)nValue, szText, 10);

	SetDlgItemText(nID, szText);
}

void COleControlContainer::SetDlgItemText(int nID, LPCTSTR lpszString)
{
	CWnd* pWnd = GetDlgItem(nID);
	if (pWnd == NULL)
		return;

	pWnd->SetWindowText(lpszString);
}

UINT COleControlContainer::IsDlgButtonChecked(int nIDButton) const
{
	CWnd* pWnd = GetDlgItem(nIDButton);
	if (pWnd == NULL)
		return 0;

	if (pWnd->m_pCtrlSite == NULL)
		return pWnd->SendMessage(BM_GETCHECK, 0, 0);

	DWORD dwValue;

	TRY
	{
		pWnd->GetProperty(DISPID_VALUE, VT_I4, &dwValue);
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);
		dwValue = 0;
	}
	END_CATCH_ALL

	if (dwValue == 0x0000ffff)  // VARIANT_BOOL TRUE
		dwValue = 1;

	return dwValue;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlContainer::XOleIPFrame

STDMETHODIMP COleControlContainer::XOleIPFrame::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleIPFrame)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP_(ULONG) COleControlContainer::XOleIPFrame::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleIPFrame)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlContainer::XOleIPFrame::Release()
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleIPFrame)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP COleControlContainer::XOleIPFrame::GetWindow(HWND* phWnd)
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleIPFrame)

	*phWnd = pThis->m_pWnd->m_hWnd;
	return S_OK;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::ContextSensitiveHelp(BOOL)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::GetBorder(LPRECT)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::RequestBorderSpace(
	LPCBORDERWIDTHS)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::SetBorderSpace(
	LPCBORDERWIDTHS)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::SetActiveObject(
	LPOLEINPLACEACTIVEOBJECT pActiveObject, LPCOLESTR)
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleIPFrame)

	if (pThis->m_pSiteUIActive != NULL)
	{
		LPOLEINPLACEACTIVEOBJECT pOldActiveObject = pThis->m_pSiteUIActive->m_pActiveObject;
		if (pActiveObject != NULL)
			pActiveObject->AddRef();
		pThis->m_pSiteUIActive->m_pActiveObject = pActiveObject;
		if (pOldActiveObject != NULL)
			pOldActiveObject->Release();
	}
	return S_OK;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::InsertMenus(HMENU,
	LPOLEMENUGROUPWIDTHS)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::SetMenu(HMENU, HOLEMENU, HWND)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::RemoveMenus(HMENU)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::SetStatusText(LPCOLESTR)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::EnableModeless(BOOL)
{
   // As long as we don't create any modeless dialogs, we can just return S_OK.
	return S_OK;
}

STDMETHODIMP COleControlContainer::XOleIPFrame::TranslateAccelerator(LPMSG,
	WORD)
{
	return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// CEnumUnknown - enumerator for IUnknown pointers

class CEnumUnknown : public CEnumArray
{
public:
	CEnumUnknown(const void* pvEnum, UINT nSize) :
		CEnumArray(sizeof(LPUNKNOWN), pvEnum, nSize, TRUE) {}
	~CEnumUnknown();

protected:
	virtual BOOL OnNext(void* pv);

	DECLARE_INTERFACE_MAP()
};

BEGIN_INTERFACE_MAP(CEnumUnknown, CEnumArray)
	INTERFACE_PART(CEnumUnknown, IID_IEnumUnknown, EnumVOID)
END_INTERFACE_MAP()

CEnumUnknown::~CEnumUnknown()
{
	if (m_pClonedFrom == NULL)
	{
		LPUNKNOWN* ppUnk = (LPUNKNOWN*)(void*)m_pvEnum;
		for (UINT i = 0; i < m_nSize; i++)
		{
			ASSERT(ppUnk[i] != NULL);
			ppUnk[i]->Release();
		}
	}
	// destructor will free the actual array (if it was not a clone)
}

BOOL CEnumUnknown::OnNext(void* pv)
{
	if (!CEnumArray::OnNext(pv))
		return FALSE;

	// AddRef the pointer (the caller has responsibility to Release it)
	ASSERT(*(LPUNKNOWN*)pv != NULL);
	(*(LPUNKNOWN*)pv)->AddRef();

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlContainer::XOleContainer

STDMETHODIMP COleControlContainer::XOleContainer::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleContainer)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP_(ULONG) COleControlContainer::XOleContainer::Release()
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleContainer)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP_(ULONG) COleControlContainer::XOleContainer::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleContainer)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP COleControlContainer::XOleContainer::ParseDisplayName(LPBINDCTX,
	LPOLESTR, ULONG*, LPMONIKER*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlContainer::XOleContainer::EnumObjects(DWORD dwFlags,
	LPENUMUNKNOWN* ppEnumUnknown)
{
	METHOD_PROLOGUE_EX_(COleControlContainer, OleContainer)

	*ppEnumUnknown = NULL;
	HRESULT hr = S_OK;
	CEnumUnknown* pEnum = NULL;
	UINT cObjects = 0;
	LPUNKNOWN* ppUnk = NULL;

	TRY
	{
		if (dwFlags & OLECONTF_EMBEDDINGS)
		{
			cObjects = pThis->m_siteMap.GetCount();
			ppUnk = new LPUNKNOWN[cObjects];
			UINT i = 0;
			POSITION pos = pThis->m_siteMap.GetStartPosition();
			HWND hWnd;
			COleControlSite* pSite;
			while (pos != NULL)
			{
				pThis->m_siteMap.GetNextAssoc(pos, (void*&)hWnd, (void*&)pSite);
				ASSERT(pSite->m_pObject != NULL);
				pSite->m_pObject->AddRef();
				ppUnk[i++] = pSite->m_pObject;
			}

			ASSERT(cObjects == i);
		}
		pEnum = new CEnumUnknown(ppUnk, cObjects);
	}
	CATCH_ALL(e)
	{
		// Note: DELETE_EXCEPTION(e) not necessary
		hr = E_OUTOFMEMORY;
	}
	END_CATCH_ALL

	// clean up in case of failure
	if (SUCCEEDED(hr))
	{
		ASSERT(pEnum != NULL);
		*ppEnumUnknown = (IEnumUnknown*)&pEnum->m_xEnumVOID;
	}
	else
	{
		ASSERT(pEnum == NULL);
		ASSERT(*ppEnumUnknown == NULL);

		if (ppUnk != NULL)
		{
			for (UINT i = 0; i < cObjects; i++)
			{
				ASSERT(ppUnk[i] != NULL);
				ppUnk[i]->Release();
			}
		}
	}

	return hr;
}

STDMETHODIMP COleControlContainer::XOleContainer::LockContainer(BOOL)
{
	return E_NOTIMPL;
}
