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
#define DBINITCONSTANTS  // __BORLANDC__ fix: must define this here so that
                         //                   the ocdb.h symbols get defined

#include "initguid.h"
#define INITGUID         // __BORLANDC__ fix: must define this here so that
                         //                   the guids from ocdbid.h get generated

#include "occimpl.h"
#include "msdadc.h"

#ifdef AFX_OCC_SEG
#pragma code_seg(AFX_OCC_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#define S_QUICKACTIVATED S_FALSE

#include "initguid.h"
#define DBINITCONSTANTS
#define INITGUID

DEFINE_GUID(IID_IDataSourceListener,0x7C0FFAB2L,0xCD84,0x11D0,0x94,0x9A,0x00,0xA0,0xC9,0x11,0x10,0xED);
DEFINE_GUID(IID_IDataSource,0x7c0ffab3L, 0xcd84, 0x11d0, 0x94, 0x9a, 0x00, 0xa0, 0xc9, 0x11, 0x10, 0xed);

/////////////////////////////////////////////////////////////////////////////
// COleControlSite

BEGIN_INTERFACE_MAP(COleControlSite, CCmdTarget)
	INTERFACE_PART(COleControlSite, IID_IOleClientSite, OleClientSite)
	INTERFACE_PART(COleControlSite, IID_IOleInPlaceSite, OleIPSite)
	INTERFACE_PART(COleControlSite, IID_IOleControlSite, OleControlSite)
	INTERFACE_PART(COleControlSite, IID_IDispatch, AmbientProps)
	INTERFACE_PART(COleControlSite, IID_IBoundObjectSite, BoundObjectSite)
	INTERFACE_PART(COleControlSite, IID_INotifyDBEvents, NotifyDBEvents)
	INTERFACE_PART(COleControlSite, IID_IRowsetNotify, RowsetNotify)
END_INTERFACE_MAP()

COleControlSite::COleControlSite(COleControlContainer* pCtrlCont) :
	m_pCtrlCont(pCtrlCont),
	m_pWndCtrl(NULL),
	m_nID((UINT)-1),
	m_pObject(NULL),
	m_pInPlaceObject(NULL),
	m_pActiveObject(NULL),
	m_dwEventSink(0),
	m_dwPropNotifySink(0),
	m_dwMiscStatus(0),
	m_dwNotifyDBEvents(0),
	m_pDataSourceControl(NULL),
	m_pDSCSite(NULL),
	m_defdispid(0),
	m_dwType(0),
	m_pBindings(NULL),
	m_bIgnoreNotify(FALSE),
	m_bIsDirty(FALSE)
{
	memset(&m_varResult, 0, sizeof(VARIANT));
	m_varResult.vt = VT_EMPTY;
}

COleControlSite::~COleControlSite()
{
	delete m_pDataSourceControl;

	DetachWindow();

	DisconnectSink(m_iidEvents, m_dwEventSink);
	DisconnectSink(IID_IPropertyNotifySink, m_dwPropNotifySink);
	DisconnectSink(IID_INotifyDBEvents, m_dwNotifyDBEvents);

	if (m_pInPlaceObject != NULL)
	{
		m_pInPlaceObject->InPlaceDeactivate();
		m_pInPlaceObject->Release();
		m_pInPlaceObject = NULL;
	}

	if (m_pActiveObject != NULL)
	{
		m_pActiveObject->Release();
		m_pActiveObject = NULL;
	}

	if (m_pObject != NULL)
	{
		m_pObject->SetClientSite(NULL);
		m_pObject->Close(OLECLOSE_NOSAVE);
		m_pObject->Release();
		m_pObject = NULL;
	}

	::VariantClear(&m_varResult);

	BindProperty(DISPID_UNKNOWN, NULL);  // gets rid of complex bindings

	if (m_defdispid != 0 && m_pDSCSite != NULL &&
		m_pDSCSite->m_pDataSourceControl != NULL)
	{
		// get rid of simple bindings
		m_pDSCSite->m_pDataSourceControl->BindProp(this, FALSE);
	}
}

BOOL COleControlSite::SetExtent()
{
	CSize size(m_rect.Size());
	CClientDC dc(NULL);
	dc.DPtoHIMETRIC(&size);

	HRESULT hr;

	if (SUCCEEDED(hr = m_pObject->SetExtent(DVASPECT_CONTENT, (SIZEL*)&size)))
	{
		if (SUCCEEDED(m_pObject->GetExtent(DVASPECT_CONTENT, (SIZEL*)&size)))
		{
			dc.HIMETRICtoDP(&size);
			m_rect.right = m_rect.left + size.cx;
			m_rect.bottom = m_rect.top + size.cy;
		}
	}

	return SUCCEEDED(hr);
}

HRESULT COleControlSite::CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
	LPCTSTR lpszWindowName, DWORD dwStyle, const RECT& rect, UINT nID,
	CFile* pPersist, BOOL bStorage, BSTR bstrLicKey)
{
   CRect rect2( rect );
   CPoint pt;
   CSize size;

   pt = rect2.TopLeft();
   size = rect2.Size();

   return( CreateControl( pWndCtrl, clsid, lpszWindowName, dwStyle, &pt, &size,
	  nID, pPersist, bStorage, bstrLicKey ) );
}

HRESULT COleControlSite::CreateControl(CWnd* pWndCtrl, REFCLSID clsid,
	LPCTSTR lpszWindowName, DWORD dwStyle, const POINT* ppt, const SIZE* psize,
   UINT nID, CFile* pPersist, BOOL bStorage, BSTR bstrLicKey)
{
	HRESULT hr = E_FAIL;
	m_hWnd = NULL;
   CSize size;

	// Connect the OLE Control with its proxy CWnd object
	if (pWndCtrl != NULL)
	{
		ASSERT(pWndCtrl->m_pCtrlSite == NULL);
		m_pWndCtrl = pWndCtrl;
		pWndCtrl->m_pCtrlSite = this;
	}

	// Initialize OLE, if necessary
	_AFX_THREAD_STATE* pState = AfxGetThreadState();
	if (!pState->m_bNeedTerm && !AfxOleInit())
		return hr;

	if (SUCCEEDED(hr = CreateOrLoad(clsid, pPersist, bStorage, bstrLicKey)))
	{
		ASSERT(m_pObject != NULL);
		m_nID = nID;

		if (psize == NULL)
		{
			// If psize is NULL, ask the object how big it wants to be.
			CClientDC dc(NULL);

			m_pObject->GetExtent(DVASPECT_CONTENT, &size);
			dc.HIMETRICtoDP(&size);
			m_rect = CRect(*ppt, size);
		}
		else
		{
			m_rect = CRect(*ppt, *psize);
		}

		m_dwStyleMask = WS_GROUP | WS_TABSTOP;

		if (m_dwMiscStatus & OLEMISC_ACTSLIKEBUTTON)
			m_dwStyleMask |= BS_DEFPUSHBUTTON;

		if (m_dwMiscStatus & OLEMISC_INVISIBLEATRUNTIME)
			dwStyle &= ~WS_VISIBLE;

		m_dwStyle = dwStyle & m_dwStyleMask;

		// If control wasn't quick-activated, then connect sinks now.
		if (hr != S_QUICKACTIVATED)
		{
			m_dwEventSink = ConnectSink(m_iidEvents, &m_xEventSink);
			m_dwPropNotifySink = ConnectSink(IID_IPropertyNotifySink,
				&m_xPropertyNotifySink);
		}
		m_dwNotifyDBEvents = ConnectSink(IID_INotifyDBEvents, &m_xNotifyDBEvents);

		// Now that the object has been created, attempt to
		// in-place activate it.

		SetExtent();

		if (SUCCEEDED(hr = m_pObject->QueryInterface(IID_IOleInPlaceObject,
				(LPVOID*)&m_pInPlaceObject)))
		{
			if (dwStyle & WS_VISIBLE)
			{
				// control is visible: just activate it
				hr = DoVerb(OLEIVERB_INPLACEACTIVATE);
			}
			else
			{
				// control is not visible: activate off-screen, hide, then move
				m_rect.OffsetRect(-32000, -32000);
				if (SUCCEEDED(hr = DoVerb(OLEIVERB_INPLACEACTIVATE)) &&
					SUCCEEDED(hr = DoVerb(OLEIVERB_HIDE)))
				{
					m_rect.OffsetRect(32000, 32000);
					hr = m_pInPlaceObject->SetObjectRects(m_rect, m_rect);
				}
			}
		}
		else
		{
			TRACE1("IOleInPlaceObject not supported on OLE control (dialog ID %d).\n", nID);
			TRACE1(">>> Result code: 0x%08lx\n", hr);
		}

		if (SUCCEEDED(hr))
			GetControlInfo();

		// if QueryInterface or activation failed, cleanup everything
		if (FAILED(hr))
		{
			if (m_pInPlaceObject != NULL)
			{
				m_pInPlaceObject->Release();
				m_pInPlaceObject = NULL;
			}
			DisconnectSink(m_iidEvents, m_dwEventSink);
			DisconnectSink(IID_IPropertyNotifySink, m_dwPropNotifySink);
			DisconnectSink(IID_INotifyDBEvents, m_dwNotifyDBEvents);
			m_dwEventSink = 0;
			m_dwPropNotifySink = 0;
			m_dwNotifyDBEvents = 0;
			m_pObject->Release();
			m_pObject = NULL;
		}
	}

	if (SUCCEEDED(hr))
	{
		AttachWindow();

		ASSERT(m_hWnd != NULL);

		// Initialize the control's Caption or Text property, if any
		if (lpszWindowName != NULL)
			SetWindowText(lpszWindowName);

		// Initialize styles
		ModifyStyle(0, m_dwStyle | (dwStyle & (WS_DISABLED|WS_BORDER)), 0);
	}

	return hr;
}

BOOL COleControlSite::DestroyControl()
{
	ASSERT(m_hWnd != NULL);     // was control ever successfully created?
	m_pCtrlCont->m_siteMap.RemoveKey(m_hWnd);

	//VBBUG: VB controls will crash if IOleObject::Close is called on them
	//  when they have focus (and unfortunately, deactivating them does not
	//  always move the focus).  To work around this problem, we always hide
	//  the control before closing it.
	ShowWindow(SW_HIDE);

	// Now it is safe to close the control.
	delete this;

	return TRUE;
}

AFX_STATIC HRESULT AFXAPI _AfxCoCreateInstanceLic(REFCLSID clsid, LPUNKNOWN pUnkOuter,
	DWORD dwClsCtx, REFIID iid, LPVOID* ppv, BSTR bstrLicKey)
{
	HRESULT hr;

	if (bstrLicKey == NULL)
	{
		LPCLASSFACTORY pClassFactory = NULL;

		if (SUCCEEDED(hr = CoGetClassObject(clsid, dwClsCtx, NULL,
			IID_IClassFactory, (void**)&pClassFactory)))
		{
			ASSERT(pClassFactory != NULL);
			hr = pClassFactory->CreateInstance(pUnkOuter, iid, ppv);
			pClassFactory->Release();
		}
	}
	else
	{
		LPCLASSFACTORY2 pClassFactory = NULL;

		if (SUCCEEDED(hr = CoGetClassObject(clsid, dwClsCtx, NULL,
			IID_IClassFactory2, (void**)&pClassFactory)))
		{
			ASSERT(pClassFactory != NULL);
			hr = pClassFactory->CreateInstanceLic(pUnkOuter, NULL, iid,
				bstrLicKey, ppv);
			pClassFactory->Release();
		}
	}

	return hr;
}

AFX_STATIC_DATA const struct { DISPID dwDispID; DWORD dwFlag; } _afxAmbients[] =
{
	{ DISPID_AMBIENT_USERMODE,          QACONTAINER_USERMODE },
	{ DISPID_AMBIENT_UIDEAD,            QACONTAINER_UIDEAD },
	{ DISPID_AMBIENT_SHOWGRABHANDLES,   QACONTAINER_SHOWGRABHANDLES },
	{ DISPID_AMBIENT_SHOWHATCHING,      QACONTAINER_SHOWHATCHING },
	{ DISPID_AMBIENT_DISPLAYASDEFAULT,  QACONTAINER_DISPLAYASDEFAULT },
	{ DISPID_AMBIENT_AUTOCLIP,          QACONTAINER_AUTOCLIP },
	{ DISPID_AMBIENT_MESSAGEREFLECT,    QACONTAINER_MESSAGEREFLECT },
	{ DISPID_AMBIENT_SUPPORTSMNEMONICS, QACONTAINER_SUPPORTSMNEMONICS },
};

BOOL COleControlSite::QuickActivate()
{
	BOOL bQuickActivated = FALSE;
	IQuickActivate* pQuick = NULL;
	if (SUCCEEDED(m_pObject->QueryInterface(IID_IQuickActivate,
		reinterpret_cast<void**>(&pQuick))))
	{
		ASSERT(pQuick != NULL);

		// Initialize QACONTAINER structure.
		QACONTAINER qaContainer;
		qaContainer.cbSize = sizeof(QACONTAINER);
		qaContainer.pClientSite = &m_xOleClientSite;
		qaContainer.pAdviseSink = NULL;
		qaContainer.pPropertyNotifySink = &m_xPropertyNotifySink;
		qaContainer.pUnkEventSink = &m_xEventSink;
		qaContainer.pUndoMgr = NULL;
		qaContainer.hpal = NULL;
		qaContainer.pBindHost = NULL;

		// Fill ambient property values in QACONTAINER.
		COleVariant var;
		CWnd* pWndContain = m_pCtrlCont->m_pWnd;

		qaContainer.dwAmbientFlags = 0;
		for (int i = 0; i < _countof(_afxAmbients); i++)
		{
			pWndContain->OnAmbientProperty(this, _afxAmbients[i].dwDispID, &var);
			if (V_BOOL(&var))
				qaContainer.dwAmbientFlags |= _afxAmbients[i].dwFlag;
		}

		pWndContain->OnAmbientProperty(this, DISPID_AMBIENT_FORECOLOR, &var);
		qaContainer.colorFore = V_I4(&var);
		pWndContain->OnAmbientProperty(this, DISPID_AMBIENT_BACKCOLOR, &var);
		qaContainer.colorBack = V_I4(&var);
		pWndContain->OnAmbientProperty(this, DISPID_AMBIENT_APPEARANCE, &var);
		qaContainer.dwAppearance = V_I2(&var);
		pWndContain->OnAmbientProperty(this, DISPID_AMBIENT_LOCALEID, &var);
		qaContainer.lcid = V_I4(&var);
		pWndContain->OnAmbientProperty(this, DISPID_AMBIENT_FONT, &var);
		if (FAILED(V_DISPATCH(&var)->QueryInterface(IID_IFont,
			reinterpret_cast<void**>(&qaContainer.pFont))))
		{
			qaContainer.pFont = NULL;
		}

		// Initialize QACONTROL structure.
		QACONTROL qaControl;
		qaControl.cbSize = sizeof(QACONTROL);

		// Do the quick activation.
		if (SUCCEEDED(pQuick->QuickActivate(&qaContainer, &qaControl)))
		{
			// Extract return values from QACONTROL structure.
			m_dwMiscStatus = qaControl.dwMiscStatus;
			m_dwEventSink = qaControl.dwEventCookie;
			m_dwPropNotifySink = qaControl.dwPropNotifyCookie;
			bQuickActivated = TRUE;
		}
		pQuick->Release();

		if (qaContainer.pFont != NULL)
			qaContainer.pFont->Release();
	}

	return bQuickActivated;
}

HRESULT COleControlSite::CreateOrLoad(REFCLSID clsid, CFile* pFile,
	BOOL bStorage, BSTR bstrLicKey)
{
	ASSERT(m_pObject == NULL);

#ifdef _DEBUG
	OLECHAR wszClsid[40];
	StringFromGUID2(clsid, wszClsid, 40);
#endif //_DEBUG

	HRESULT hr;
	if (FAILED(hr = _AfxCoCreateInstanceLic(clsid, NULL,
		CLSCTX_INPROC_SERVER | CLSCTX_INPROC_HANDLER, IID_IOleObject,
		(void**)&m_pObject, bstrLicKey)))
	{
		TRACE1("CoCreateInstance of OLE control %ls failed.\n", wszClsid);
		TRACE1(">>> Result code: 0x%08lx\n", hr);
		TRACE0(">>> Is the control is properly registered?\n");
		return hr;
	}

	LPPERSISTSTREAMINIT pPersStm = NULL;
	LPPERSISTSTORAGE pPersStg = NULL;
	LPPERSISTMEMORY pPersMem = NULL;

	GetEventIID(&m_iidEvents);
	// Try to quick-activate first
	BOOL bQuickActivated = QuickActivate();

	if (!bQuickActivated)
	{
		m_pObject->GetMiscStatus(DVASPECT_CONTENT, &m_dwMiscStatus);

		// set client site first, if appropriate
		if (m_dwMiscStatus & OLEMISC_SETCLIENTSITEFIRST)
		{
			if (FAILED(hr = m_pObject->SetClientSite(&m_xOleClientSite)))
			{
				TRACE1("SetClientSite on OLE control %ls failed.\n", wszClsid);
				TRACE1(">>> Result code: 0x%08lx\n", hr);
				goto CreateOrLoadFailed;
			}
		}
	}

	ASSERT(!bStorage || pFile != NULL);

	// initialize via IPersistMemory (direct buffering)
	if (pFile != NULL && !bStorage &&
		SUCCEEDED(m_pObject->QueryInterface(IID_IPersistMemory, (void**)&pPersMem)) &&
		pFile->GetBufferPtr(CFile::bufferCheck) != 0)
	{
		ASSERT(pPersMem != NULL);

		// file supports direct buffering, get its buffer pointer and size
		LPVOID pvBuffer = NULL;
		LPVOID pvEnd;
		ULONG cbBuffer = pFile->GetBufferPtr(
			CFile::bufferRead, (UINT)-1, &pvBuffer, &pvEnd);
		ASSERT(((LPBYTE)pvEnd - (LPBYTE)pvBuffer) == (int)cbBuffer);

		// and then load it directly
		hr = pPersMem->Load(pvBuffer, cbBuffer);
		pPersMem->Release();
		pPersMem = NULL;
		if (FAILED(hr))
			goto CreateOrLoadFailed;
	}
	// initialize via IPersistStreamInit
	else if (!bStorage && SUCCEEDED(m_pObject->QueryInterface(
		IID_IPersistStreamInit, (void**)&pPersStm)))
	{
		ASSERT(pPersStm != NULL);

		if (pFile == NULL)
		{
			// just call InitNew
			hr = pPersStm->InitNew();
		}
		else
		{
			// open an IStream on the data and pass it to Load
			CArchive ar(pFile, CArchive::load);
			CArchiveStream stm(&ar);
			hr = pPersStm->Load(&stm);
		}
		pPersStm->Release();

		if (FAILED(hr))
		{
			TRACE1("InitNew or Load on OLE control %ls failed.\n", wszClsid);
			TRACE1(">>> Result code: 0x%08lx\n", hr);
			goto CreateOrLoadFailed;
		}
	}
	// initialize via IPersistStorage
	else if (SUCCEEDED(m_pObject->QueryInterface(
		IID_IPersistStorage, (void**)&pPersStg)))
	{
		ASSERT(pPersStg != NULL);

		if (pFile == NULL)
		{
			// create a scratch IStorage and pass it to InitNew
			LPLOCKBYTES pLockBytes = NULL;
			if (SUCCEEDED(hr = CreateILockBytesOnHGlobal(NULL, TRUE,
				&pLockBytes)))
			{
				ASSERT(pLockBytes != NULL);
				LPSTORAGE pStorage = NULL;
				if (SUCCEEDED(hr = StgCreateDocfileOnILockBytes(pLockBytes,
					STGM_CREATE | STGM_READWRITE | STGM_SHARE_EXCLUSIVE, 0,
					&pStorage)))
				{
					ASSERT(pStorage != NULL);
					hr = pPersStg->InitNew(pStorage);
					pStorage->Release();
				}
				pLockBytes->Release();
			}
		}
		else if (bStorage)
		{
			// copy data to an HGLOBAL, so we can build an IStorage on it
			UINT cb = pFile->GetLength();
			HGLOBAL hGlobal;
			BYTE* pbData;

			if (((hGlobal = GlobalAlloc(GMEM_FIXED, cb)) != NULL) &&
				((pbData = (BYTE*)GlobalLock(hGlobal)) != NULL))
			{
				pFile->Read(pbData, cb);
				GlobalUnlock(hGlobal);
			}
			else
			{
				hr = E_OUTOFMEMORY;
				hGlobal = NULL;
			}

			// open an IStorage on the data and pass it to Load
			LPLOCKBYTES pLockBytes = NULL;
			if ((hGlobal != NULL) &&
				SUCCEEDED(hr = CreateILockBytesOnHGlobal(hGlobal, TRUE,
				&pLockBytes)))
			{
				ASSERT(pLockBytes != NULL);
				LPSTORAGE pStorage = NULL;
				if (SUCCEEDED(hr = StgOpenStorageOnILockBytes(pLockBytes, NULL,
					STGM_READWRITE | STGM_SHARE_EXCLUSIVE, NULL, 0, &pStorage)))
				{
					ASSERT(pStorage != NULL);
					hr = pPersStg->Load(pStorage);
					pStorage->Release();
				}
				pLockBytes->Release();
			}
		}
		else
		{
			hr = E_UNEXPECTED;
		}
		pPersStg->Release();

		if (FAILED(hr))
		{
			TRACE1("InitNew or Load on OLE control %ls failed.\n", wszClsid);
			TRACE1(">>> Result code: 0x%08lx\n", hr);
			goto CreateOrLoadFailed;
		}
	}
	else
	{
		TRACE1("Persistence not supported on OLE control %ls.\n", wszClsid);
		TRACE1(">>> Result code: 0x%08lx\n", hr);
		goto CreateOrLoadFailed;
	}

	if (!bQuickActivated)
	{
		// set client site last, if appropriate
		if (!(m_dwMiscStatus & OLEMISC_SETCLIENTSITEFIRST))
		{
			if (FAILED(hr = m_pObject->SetClientSite(&m_xOleClientSite)))
			{
				TRACE1("SetClientSite on OLE control %ls failed.\n", wszClsid);
				TRACE1(">>> Result code: 0x%08lx\n", hr);
				goto CreateOrLoadFailed;
			}
		}
	}

CreateOrLoadFailed:
	if (FAILED(hr) && (m_pObject != NULL))
	{
		m_pObject->Close(OLECLOSE_NOSAVE);
		m_pObject->Release();
		m_pObject = NULL;
	}

	if (pPersMem != NULL)
		pPersMem->Release();

	if (bQuickActivated && SUCCEEDED(hr))
		hr = S_QUICKACTIVATED;

	return hr;
}

UINT COleControlSite::GetID()
{
	return m_nID;
}

HRESULT COleControlSite::DoVerb(LONG nVerb, LPMSG lpMsg)
{
	return m_pObject->DoVerb(nVerb, lpMsg, &m_xOleClientSite, 0,
		m_pCtrlCont->m_pWnd->m_hWnd, m_rect);
}

BOOL COleControlSite::IsDefaultButton()
{
	return ((m_dwMiscStatus & OLEMISC_ACTSLIKEBUTTON) &&
		(m_dwStyle & BS_DEFPUSHBUTTON));
}

DWORD COleControlSite::GetDefBtnCode()
{
	if (m_dwMiscStatus & OLEMISC_ACTSLIKEBUTTON)
		return (m_dwStyle & BS_DEFPUSHBUTTON) ?
			DLGC_DEFPUSHBUTTON :
			DLGC_UNDEFPUSHBUTTON;
	else
		return 0;
}

void COleControlSite::SetDefaultButton(BOOL bDefault)
{
	if (!(m_dwMiscStatus & OLEMISC_ACTSLIKEBUTTON))
		return;

	if (((m_dwStyle & BS_DEFPUSHBUTTON) != 0) == bDefault)
		return;

	m_dwStyle ^= BS_DEFPUSHBUTTON;

	// Notify control that its "defaultness" has changed.
	LPOLECONTROL pOleCtl = NULL;
	if (SUCCEEDED(m_pObject->QueryInterface(IID_IOleControl,
		(LPVOID*)&pOleCtl)))
	{
		ASSERT(pOleCtl != NULL);
		pOleCtl->OnAmbientPropertyChange(DISPID_AMBIENT_DISPLAYASDEFAULT);
		pOleCtl->Release();
	}
}

DWORD COleControlSite::ConnectSink(REFIID iid, LPUNKNOWN punkSink)
{
	ASSERT(m_pObject != NULL);

	LPCONNECTIONPOINTCONTAINER pConnPtCont;

	if ((m_pObject != NULL) &&
		SUCCEEDED(m_pObject->QueryInterface(IID_IConnectionPointContainer,
			(LPVOID*)&pConnPtCont)))
	{
		ASSERT(pConnPtCont != NULL);
		LPCONNECTIONPOINT pConnPt = NULL;
		DWORD dwCookie = 0;

		if (SUCCEEDED(pConnPtCont->FindConnectionPoint(iid, &pConnPt)))
		{
			ASSERT(pConnPt != NULL);
			pConnPt->Advise(punkSink, &dwCookie);
			pConnPt->Release();
		}

		pConnPtCont->Release();
		return dwCookie;
	}

	return 0;
}

void COleControlSite::DisconnectSink(REFIID iid, DWORD dwCookie)
{
	if (dwCookie == 0 || m_pObject == NULL)
		return;

	LPCONNECTIONPOINTCONTAINER pConnPtCont;

	if (SUCCEEDED(m_pObject->QueryInterface(IID_IConnectionPointContainer,
			(LPVOID*)&pConnPtCont)))
	{
		ASSERT(pConnPtCont != NULL);
		LPCONNECTIONPOINT pConnPt = NULL;

		if (SUCCEEDED(pConnPtCont->FindConnectionPoint(iid, &pConnPt)))
		{
			ASSERT(pConnPt != NULL);
			pConnPt->Unadvise(dwCookie);
			pConnPt->Release();
		}

		pConnPtCont->Release();
	}
}

#define IMPLTYPE_MASK \
	(IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FRESTRICTED)

#define IMPLTYPE_DEFAULTSOURCE \
	(IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE)

BOOL COleControlSite::GetEventIID(IID* piid)
{
	*piid = GUID_NULL;

	ASSERT(m_pObject != NULL);

	// Use IProvideClassInfo2, if control supports it.
	LPPROVIDECLASSINFO2 pPCI2 = NULL;

	if (SUCCEEDED(m_pObject->QueryInterface(IID_IProvideClassInfo2,
		(LPVOID*)&pPCI2)))
	{
		ASSERT(pPCI2 != NULL);

		if (SUCCEEDED(pPCI2->GetGUID(GUIDKIND_DEFAULT_SOURCE_DISP_IID, piid)))
			ASSERT(!IsEqualIID(*piid, GUID_NULL));
		else
			ASSERT(IsEqualIID(*piid, GUID_NULL));

		pPCI2->Release();
	}

	// Fall back on IProvideClassInfo, if IProvideClassInfo2 not supported.
	LPPROVIDECLASSINFO pPCI = NULL;

	if (IsEqualIID(*piid, GUID_NULL) &&
		SUCCEEDED(m_pObject->QueryInterface(IID_IProvideClassInfo,
		(LPVOID*)&pPCI)))
	{
		ASSERT(pPCI != NULL);

		LPTYPEINFO pClassInfo = NULL;

		if (SUCCEEDED(pPCI->GetClassInfo(&pClassInfo)))
		{
			ASSERT(pClassInfo != NULL);

			LPTYPEATTR pClassAttr;
			if (SUCCEEDED(pClassInfo->GetTypeAttr(&pClassAttr)))
			{
				ASSERT(pClassAttr != NULL);
				ASSERT(pClassAttr->typekind == TKIND_COCLASS);

				// Search for typeinfo of the default events interface.

				int nFlags;
				HREFTYPE hRefType;

				for (unsigned int i = 0; i < pClassAttr->cImplTypes; i++)
				{
					if (SUCCEEDED(pClassInfo->GetImplTypeFlags(i, &nFlags)) &&
						((nFlags & IMPLTYPE_MASK) == IMPLTYPE_DEFAULTSOURCE))
					{
						// Found it.  Now look at its attributes to get IID.

						LPTYPEINFO pEventInfo = NULL;

						if (SUCCEEDED(pClassInfo->GetRefTypeOfImplType(i,
								&hRefType)) &&
							SUCCEEDED(pClassInfo->GetRefTypeInfo(hRefType,
								&pEventInfo)))
						{
							ASSERT(pEventInfo != NULL);

							LPTYPEATTR pEventAttr;

							if (SUCCEEDED(pEventInfo->GetTypeAttr(&pEventAttr)))
							{
								ASSERT(pEventAttr != NULL);
								*piid = pEventAttr->guid;
								pEventInfo->ReleaseTypeAttr(pEventAttr);
							}

							pEventInfo->Release();
						}

						break;
					}
				}

				pClassInfo->ReleaseTypeAttr(pClassAttr);
			}

			pClassInfo->Release();
		}

		pPCI->Release();
	}

	return (!IsEqualIID(*piid, GUID_NULL));
}

void COleControlSite::GetControlInfo()
{
	memset(&m_ctlInfo, 0, sizeof(CONTROLINFO));
	m_ctlInfo.cb = sizeof(CONTROLINFO);
	LPOLECONTROL pOleCtl = NULL;

	if (SUCCEEDED(m_pObject->QueryInterface(IID_IOleControl,
		(LPVOID*)&pOleCtl)))
	{
		ASSERT(pOleCtl != NULL);
		pOleCtl->GetControlInfo(&m_ctlInfo);
		pOleCtl->Release();
	}
}

BOOL COleControlSite::IsMatchingMnemonic(LPMSG lpMsg)
{
//  return IsAccelerator(m_ctlInfo.hAccel, m_ctlInfo.cAccel, lpMsg, NULL);

	if ((m_ctlInfo.cAccel == 0) || (m_ctlInfo.hAccel == NULL))
		return FALSE;

	ACCEL* pAccel = new ACCEL[m_ctlInfo.cAccel];
	int cAccel = CopyAcceleratorTable(m_ctlInfo.hAccel, pAccel, m_ctlInfo.cAccel);
	ASSERT(cAccel == m_ctlInfo.cAccel);

	BOOL bMatch = FALSE;
	for (int i = 0; i < cAccel; i++)
	{
		BOOL fVirt = (lpMsg->message == WM_SYSCHAR ? FALT : 0);
		WORD key = LOWORD(lpMsg->wParam);
		if (((pAccel[i].fVirt & ~FNOINVERT) == fVirt) &&
			(pAccel[i].key == key))
		{
			bMatch = TRUE;
			break;
		}
	}

	delete [] pAccel;
	return bMatch;
}

void COleControlSite::SendMnemonic(LPMSG lpMsg)
{
	if (!(m_dwMiscStatus & OLEMISC_NOUIACTIVATE))
		SetFocus();

	LPOLECONTROL pOleCtl = NULL;

	if (SUCCEEDED(m_pObject->QueryInterface(IID_IOleControl,
		(LPVOID*)&pOleCtl)))
	{
		ASSERT(pOleCtl != NULL);
		pOleCtl->OnMnemonic(lpMsg);
		pOleCtl->Release();
	}
}

void COleControlSite::FreezeEvents(BOOL bFreeze)
{
	LPOLECONTROL pOleCtl = NULL;
	if (SUCCEEDED(m_pObject->QueryInterface(IID_IOleControl,
		(LPVOID*)&pOleCtl)))
	{
		ASSERT(pOleCtl != NULL);
		pOleCtl->FreezeEvents(bFreeze);
		pOleCtl->Release();
	}
}

void COleControlSite::AttachWindow()
{
	HWND hWnd = NULL;
	if (SUCCEEDED(m_pInPlaceObject->GetWindow(&hWnd)))
	{
		ASSERT(hWnd != NULL);
		if (m_hWnd != hWnd)
		{
			m_hWnd = hWnd;

			if (m_pWndCtrl != NULL)
			{
				ASSERT(m_pWndCtrl->m_hWnd == NULL); // Window already attached?
				m_pWndCtrl->Attach(m_hWnd);

				ASSERT(m_pWndCtrl->m_pCtrlSite == NULL ||
					m_pWndCtrl->m_pCtrlSite == this);
				m_pWndCtrl->m_pCtrlSite = this;
			}
		}
	}
}

void COleControlSite::DetachWindow()
{
	m_hWnd = NULL;

	if (m_pWndCtrl != NULL)
	{
		if (m_pWndCtrl->m_hWnd != NULL)
		{
			WNDPROC* lplpfn = m_pWndCtrl->GetSuperWndProcAddr();
			ASSERT(lplpfn != NULL);
			if (::IsWindow(m_pWndCtrl->m_hWnd) && *lplpfn != NULL)
				m_pWndCtrl->UnsubclassWindow();

			m_pWndCtrl->Detach();
		}

		m_pWndCtrl->m_pCtrlSite = NULL;
	}
}

BOOL COleControlSite::OnEvent(AFX_EVENT* pEvent)
{
	// If this control has a proxy CWnd, look for a matching ON_*_REFLECT
	// entry for this event in its event map.
	if ((m_pWndCtrl != NULL) &&
		m_pWndCtrl->OnCmdMsg(m_nID, CN_EVENT, pEvent, NULL))
	{
		return TRUE;
	}

	// Proxy CWnd isn't interested, so pass the event along to the container.
	return m_pCtrlCont->m_pWnd->OnCmdMsg(m_nID, CN_EVENT, pEvent, NULL);
}

/////////////////////////////////////////////////////////////////////////////
// invoke helpers

void COleControlSite::InvokeHelperV(DISPID dwDispID, WORD wFlags,
	VARTYPE vtRet, void* pvRet, const BYTE* pbParamInfo, va_list argList)
{
	if (m_dispDriver.m_lpDispatch == NULL)
	{
		// no dispatch pointer yet; find it now
		LPDISPATCH pDispatch;

		if ((m_pObject != NULL) &&
			SUCCEEDED(m_pObject->QueryInterface(IID_IDispatch,
				(LPVOID*)&pDispatch)))
		{
			ASSERT(pDispatch != NULL);
			m_dispDriver.AttachDispatch(pDispatch);
		}
	}

	if (m_dispDriver.m_lpDispatch == NULL)
	{
		// couldn't find dispatch pointer
		TRACE0("Warning: control has no IDispatch interface.");
		return;
	}

	// delegate call to m_dispDriver
	m_dispDriver.InvokeHelperV(dwDispID, wFlags, vtRet, pvRet, pbParamInfo,
		argList);
}

void COleControlSite::SetPropertyV(DISPID dwDispID, VARTYPE vtProp, va_list argList)
{
	BYTE rgbParams[2];
	if (vtProp & VT_BYREF)
	{
		vtProp &= ~VT_BYREF;
		vtProp |= VT_MFCBYREF;
	}

#if !defined(_UNICODE) && !defined(OLE2ANSI)
	if (vtProp == VT_BSTR)
		vtProp = VT_BSTRA;
#endif

	WORD wFlags;
	if (vtProp & VT_MFCFORCEPUTREF)
	{
		wFlags = DISPATCH_PROPERTYPUTREF;
		vtProp &= ~VT_MFCFORCEPUTREF;
	}
	else
	{
		if (vtProp == VT_DISPATCH)
			wFlags = DISPATCH_PROPERTYPUTREF;
		else
			wFlags = DISPATCH_PROPERTYPUT;
	}

	rgbParams[0] = (BYTE)vtProp;
	rgbParams[1] = 0;
	InvokeHelperV(dwDispID, wFlags, VT_EMPTY, NULL, rgbParams, argList);
}

void AFX_CDECL COleControlSite::InvokeHelper(DISPID dwDispID, WORD wFlags, VARTYPE vtRet,
	void* pvRet, const BYTE* pbParamInfo, ...)
{
	va_list argList;
	va_start(argList, pbParamInfo);
	InvokeHelperV(dwDispID, wFlags, vtRet, pvRet, pbParamInfo, argList);
	va_end(argList);
}

void COleControlSite::GetProperty(DISPID dwDispID, VARTYPE vtProp,
	void* pvProp) const
{
	const_cast<COleControlSite*>(this)->InvokeHelper(dwDispID,
		DISPATCH_PROPERTYGET, vtProp, pvProp, NULL);
}

void AFX_CDECL COleControlSite::SetProperty(DISPID dwDispID, VARTYPE vtProp, ...)
{
	va_list argList;    // really only one arg, but...
	va_start(argList, vtProp);
	SetPropertyV(dwDispID, vtProp, argList);
	va_end(argList);
}

BOOL AFX_CDECL COleControlSite::SafeSetProperty(DISPID dwDispID, VARTYPE vtProp, ...)
{
	va_list argList;    // really only one arg, but...
	va_start(argList, vtProp);

	BOOL bSuccess;

	TRY
	{
		SetPropertyV(dwDispID, vtProp, argList);
		bSuccess = TRUE;
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);
		bSuccess = FALSE;
	}
	END_CATCH_ALL

	va_end(argList);
	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// special cases for CWnd functions

DWORD COleControlSite::GetStyle() const
{
	DWORD dwStyle = m_dwStyle |
		(::GetWindowLong(m_hWnd, GWL_STYLE) & WS_VISIBLE);

	TRY
	{
		BOOL bEnabled = TRUE;
		GetProperty(DISPID_ENABLED, VT_BOOL, &bEnabled);
		if (!bEnabled)
			dwStyle |= WS_DISABLED;
	}
	END_TRY

	TRY
	{
		short sBorderStyle = 0;
		GetProperty(DISPID_BORDERSTYLE, VT_I2, &sBorderStyle);
		if (sBorderStyle == 1)
			dwStyle |= WS_BORDER;
	}
	END_TRY

	return dwStyle;
}

DWORD COleControlSite::GetExStyle() const
{
	DWORD dwExStyle = ::GetWindowLong(m_hWnd, GWL_EXSTYLE);

	TRY
	{
		short sAppearance = 0;
		GetProperty(DISPID_APPEARANCE, VT_I2, &sAppearance);
		if (sAppearance == 1)
			dwExStyle |= WS_EX_CLIENTEDGE;
	}
	END_TRY

	return dwExStyle;
}

BOOL COleControlSite::ModifyStyle(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
{
	m_dwStyle = ((m_dwStyle & ~dwRemove) | dwAdd) & m_dwStyleMask;

	// Enabled property
	if ((dwRemove & WS_DISABLED) || (dwAdd & WS_DISABLED))
	{
		if (SafeSetProperty(DISPID_ENABLED, VT_BOOL, (~dwAdd & WS_DISABLED)))
		{
			dwRemove &= ~WS_DISABLED;
			dwAdd &= ~WS_DISABLED;
		}
	}

	// BorderStyle property
	if ((dwRemove & WS_BORDER) || (dwAdd & WS_BORDER))
	{
		if (SafeSetProperty(DISPID_BORDERSTYLE, VT_I2, (dwAdd & WS_BORDER)))
		{
			dwRemove &= ~WS_BORDER;
			dwAdd &= ~WS_BORDER;
		}
	}

	return CWnd::ModifyStyle(m_hWnd, dwRemove, dwAdd, nFlags);
}

BOOL COleControlSite::ModifyStyleEx(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
{
	// BorderStyle property
	if ((dwRemove & WS_EX_CLIENTEDGE) || (dwAdd & WS_EX_CLIENTEDGE))
	{
		if (SafeSetProperty(DISPID_APPEARANCE, VT_I2, (dwAdd & WS_EX_CLIENTEDGE)))
		{
			dwRemove &= ~WS_EX_CLIENTEDGE;
			dwAdd &= ~WS_EX_CLIENTEDGE;
		}
	}

	return CWnd::ModifyStyleEx(m_hWnd, dwRemove, dwAdd, nFlags);
}

void COleControlSite::SetWindowText(LPCTSTR lpszString)
{
	ASSERT(::IsWindow(m_hWnd));

	if (!SafeSetProperty(DISPID_CAPTION, VT_BSTR, lpszString))
		SafeSetProperty(DISPID_TEXT, VT_BSTR, lpszString);
}

void COleControlSite::GetWindowText(CString& str) const
{
	ASSERT(::IsWindow(m_hWnd));

	TRY
	{
		GetProperty(DISPID_CAPTION, VT_BSTR, &str);
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);

		TRY
		{
			GetProperty(DISPID_TEXT, VT_BSTR, &str);
		}
		END_TRY
	}
	END_CATCH_ALL
}

int COleControlSite::GetWindowText(LPTSTR lpszString, int nMaxCount) const
{
	ASSERT(nMaxCount > 0);
	CString str;
	GetWindowText(str);
	lstrcpyn(lpszString, str, nMaxCount);
	return lstrlen(lpszString);
}

int COleControlSite::GetWindowTextLength() const
{
	CString str;
	GetWindowText(str);
	return str.GetLength();
}

int COleControlSite::GetDlgCtrlID() const
{
	return (int)m_nID;
}

int COleControlSite::SetDlgCtrlID(int nID)
{
	int nPrevID = (int)m_nID;
	m_nID = (UINT)nID;
	return nPrevID;
}

void COleControlSite::MoveWindow(int x, int y, int nWidth, int nHeight, BOOL)
{
	ASSERT(m_pInPlaceObject != NULL);
	ASSERT(m_pObject != NULL);

	CRect rectOld(m_rect);
	m_rect.SetRect(x, y, x + nWidth, y + nHeight);
	if (SetExtent())
	{
		m_rect.SetRect(x, y, x + m_rect.Width(), y + m_rect.Height());
		m_pInPlaceObject->SetObjectRects(m_rect, m_rect);
	}
	else
	{
		m_rect = rectOld;
	}
}

BOOL COleControlSite::SetWindowPos(const CWnd* pWndInsertAfter, int x, int y, int cx,
	int cy, UINT nFlags)
{
	if (nFlags & SWP_HIDEWINDOW)
		ShowWindow(SW_HIDE);

	if ((nFlags & (SWP_NOMOVE|SWP_NOSIZE)) != (SWP_NOMOVE|SWP_NOSIZE))
	{
		int xNew;
		int yNew;
		if (nFlags & SWP_NOMOVE)
		{
			xNew = m_rect.left;
			yNew = m_rect.top;
		}
		else
		{
			xNew = x;
			yNew = y;
		}

		int cxNew;
		int cyNew;
		if (nFlags & SWP_NOSIZE)
		{
			cxNew = m_rect.Width();
			cyNew = m_rect.Height();
		}
		else
		{
			cxNew = cx;
			cyNew = cy;
		}

		MoveWindow(xNew, yNew, cxNew, cyNew, !(nFlags & SWP_NOREDRAW));
	}

	if (nFlags & SWP_SHOWWINDOW)
		ShowWindow(SW_SHOW);

	// we've handled hide, move, size, and show; let Windows do the rest
	nFlags &= ~(SWP_HIDEWINDOW|SWP_SHOWWINDOW);
	nFlags |= (SWP_NOMOVE|SWP_NOSIZE);
	return ::SetWindowPos(m_hWnd, pWndInsertAfter->GetSafeHwnd(),
		x, y, cx, cy, nFlags);
}

BOOL COleControlSite::ShowWindow(int nCmdShow)
{
	BOOL bReturn = ::IsWindowVisible(m_hWnd);
	int iVerb = 0;
	switch (nCmdShow)
	{
	case SW_SHOW:
	case SW_SHOWNORMAL:
	case SW_SHOWNOACTIVATE:
		iVerb = OLEIVERB_SHOW;
		break;

	case SW_HIDE:
		iVerb = OLEIVERB_HIDE;
		break;
	}

	if (iVerb != 0)
		DoVerb(iVerb);

	return bReturn;
}

BOOL COleControlSite::IsWindowEnabled() const
{
	BOOL bEnabled = TRUE;
	TRY
		GetProperty(DISPID_ENABLED, VT_BOOL, &bEnabled);
	END_TRY

	return bEnabled;
}

BOOL COleControlSite::EnableWindow(BOOL bEnable)
{
	BOOL bResult;
	TRY
	{
		GetProperty(DISPID_ENABLED, VT_BOOL, &bResult);
		SetProperty(DISPID_ENABLED, VT_BOOL, bEnable);
	}
	CATCH_ALL(e)
	{
		DELETE_EXCEPTION(e);
		bResult = TRUE;
	}
	END_CATCH_ALL

	return !bResult;    // return TRUE if previously disabled
}

CWnd* COleControlSite::SetFocus()
{
	if (m_dwMiscStatus & OLEMISC_NOUIACTIVATE)
		return CWnd::FromHandle(::SetFocus(m_hWnd));

	CWnd* pWndPrev = CWnd::GetFocus();
	DoVerb(OLEIVERB_UIACTIVATE);
	return pWndPrev;
}

void COleControlSite::EnableDSC()
{
	if (m_pDataSourceControl == NULL)
	{
		m_pDataSourceControl = new CDataSourceControl(this);
		m_pDataSourceControl->Initialize();
	}
}

void COleControlSite::BindDefaultProperty(DISPID dwDispID, VARTYPE vtProp, LPCTSTR szFieldName, CWnd* pDSCWnd)
{
	// Remove any previous binding
	if (m_pDSCSite != NULL)
	{
		m_pDSCSite->m_pDataSourceControl->BindProp(this, FALSE);
		m_pDSCSite->m_pDataSourceControl->BindColumns();
		m_pDSCSite = NULL;
	}

	if (pDSCWnd != NULL)
	{
		ASSERT(pDSCWnd->m_pCtrlSite);  // must be an OLE control
		pDSCWnd->m_pCtrlSite->EnableDSC();
		m_pDSCSite = pDSCWnd->m_pCtrlSite;
		m_defdispid = dwDispID;
		m_dwType = vtProp;
		m_strDataField = szFieldName;
		m_pDSCSite->m_pDataSourceControl->BindProp(this, TRUE);
		if (m_pDSCSite != NULL)
			m_pDSCSite->m_pDataSourceControl->BindColumns();
	}
}

void COleControlSite::BindProperty(DISPID dwDispId, CWnd* pWndDSC)
{
	ASSERT(pWndDSC == NULL || pWndDSC->m_pCtrlSite);
	if (pWndDSC != NULL && dwDispId != DISPID_UNKNOWN)
	{
		m_pBindings = new CDataBoundProperty(m_pBindings, dwDispId, 0);
		m_pBindings->m_pDSCSite = pWndDSC->m_pCtrlSite;
		m_pBindings->m_pClientSite = this;
		m_pBindings->m_pDSCSite->EnableDSC();
		m_pBindings->m_pDSCSite->m_pDataSourceControl->BindProp(m_pBindings, TRUE);
	}
	else
	{
		// Try and locate the particular property to unbind
		// if dwDispId == DISPID_UNKNOWN && pWndDSC == NULL it unbinds all properties
		// if dwDispId == DISPID_UNKNOWN && pWndDSC != NULL it unbinds properties for that DSC
		CDataBoundProperty *pCurrent = m_pBindings;
		CDataBoundProperty* pPrev = NULL;
		while (pCurrent != NULL)
		{
			CDataBoundProperty* pNext = pCurrent->GetNext();
			if (dwDispId == DISPID_UNKNOWN || pCurrent->m_dispid == dwDispId)
			{
				if (pWndDSC == NULL || pWndDSC->m_pCtrlSite == pCurrent->m_pDSCSite)
				{
					if (pPrev != NULL)
						pPrev->m_pNext = pNext;
					else
						m_pBindings = pNext;
					if (pCurrent->m_pDSCSite != NULL && pCurrent->m_pDSCSite->m_pDataSourceControl != NULL)
						pCurrent->m_pDSCSite->m_pDataSourceControl->BindProp(pCurrent, FALSE);
					delete pCurrent;
				}
			}
			if (pPrev != NULL)
				pPrev = pPrev->GetNext();
			pCurrent = pNext;
		}
	}
}


/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XOleClientSite

STDMETHODIMP_(ULONG) COleControlSite::XOleClientSite::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleClientSite)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlSite::XOleClientSite::Release()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleClientSite)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP COleControlSite::XOleClientSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleClientSite)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControlSite::XOleClientSite::SaveObject()
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleClientSite::GetMoniker(DWORD, DWORD,
	LPMONIKER*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleClientSite::GetContainer(
	LPOLECONTAINER* ppContainer)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleClientSite)
	return (HRESULT)pThis->m_pCtrlCont->InternalQueryInterface(
		&IID_IOleContainer, (LPVOID*)ppContainer);
}

STDMETHODIMP COleControlSite::XOleClientSite::ShowObject()
{
	METHOD_PROLOGUE_EX(COleControlSite, OleClientSite)
	pThis->AttachWindow();
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleClientSite::OnShowWindow(BOOL)
{
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleClientSite::RequestNewObjectLayout()
{
	return E_NOTIMPL;
}


/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XOleIPSite

STDMETHODIMP_(ULONG) COleControlSite::XOleIPSite::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlSite::XOleIPSite::Release()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP COleControlSite::XOleIPSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControlSite::XOleIPSite::GetWindow(HWND* phWnd)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	*phWnd = pThis->m_pCtrlCont->m_pWnd->GetSafeHwnd();
	return *phWnd != NULL ? S_OK : E_FAIL;
}

STDMETHODIMP COleControlSite::XOleIPSite::ContextSensitiveHelp(BOOL)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleIPSite::CanInPlaceActivate()
{
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::OnInPlaceActivate()
{
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::OnUIActivate()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	pThis->m_pCtrlCont->OnUIActivate(pThis);
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::GetWindowContext(
	LPOLEINPLACEFRAME* ppFrame, LPOLEINPLACEUIWINDOW* ppDoc, LPRECT prectPos,
	LPRECT prectClip, LPOLEINPLACEFRAMEINFO pFrameInfo)
{
	METHOD_PROLOGUE_EX(COleControlSite, OleIPSite)
	ASSERT_VALID(pThis->m_pCtrlCont);
	ASSERT_VALID(pThis->m_pCtrlCont->m_pWnd);

	ASSERT(AfxIsValidAddress(ppFrame, sizeof(LPOLEINPLACEFRAME)));
	ASSERT((ppDoc == NULL) ||
		AfxIsValidAddress(ppDoc, sizeof(LPOLEINPLACEUIWINDOW)));
	ASSERT(AfxIsValidAddress(prectPos, sizeof(RECT)));
	ASSERT(AfxIsValidAddress(prectClip, sizeof(RECT)));
	ASSERT(AfxIsValidAddress(pFrameInfo, pFrameInfo->cb));

	//WINBUG: This is a temporary patch for IE3 beta.  When IE3 is fixed, this
	// assert can be re-enabled.  Otherwise it fires everytime you browse via
	// the CWebBrowser control.
	//
	//  ASSERT(pFrameInfo->cb >= offsetof(OLEINPLACEFRAMEINFO, cAccelEntries) +
	//          sizeof(int));

	// There is no separate doc window
	if (ppDoc != NULL)
		*ppDoc = NULL;

	// Set pointer to frame
	if (FAILED(pThis->m_pCtrlCont->InternalQueryInterface(
		&IID_IOleInPlaceFrame, (LPVOID*)ppFrame)))
		return E_FAIL;

	// Fill in position and clip rectangles
	CWnd* pWndContainer = pThis->m_pCtrlCont->m_pWnd;
	CopyRect(prectPos, pThis->m_rect);
	pWndContainer->GetClientRect(prectClip);

	// Fill in frame info
	pFrameInfo->fMDIApp = FALSE;
	pFrameInfo->hwndFrame = pWndContainer->GetSafeHwnd();
	pFrameInfo->haccel = NULL;
	pFrameInfo->cAccelEntries = 0;

	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::Scroll(SIZE)
{
	return S_FALSE;
}

STDMETHODIMP COleControlSite::XOleIPSite::OnUIDeactivate(BOOL)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	pThis->m_pCtrlCont->OnUIDeactivate(pThis);
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::OnInPlaceDeactivate()
{
	METHOD_PROLOGUE_EX(COleControlSite, OleIPSite)
	pThis->DetachWindow();
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::DiscardUndoState()
{
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::DeactivateAndUndo()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	pThis->m_pInPlaceObject->UIDeactivate();
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleIPSite::OnPosRectChange(LPCRECT lprcPosRect)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleIPSite)
	CRect rectClip;
	pThis->m_pCtrlCont->m_pWnd->GetClientRect(rectClip);
	pThis->m_rect = lprcPosRect;
	return pThis->m_pInPlaceObject->SetObjectRects(pThis->m_rect, rectClip);
}


/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XOleControlSite

STDMETHODIMP_(ULONG) COleControlSite::XOleControlSite::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleControlSite)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlSite::XOleControlSite::Release()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleControlSite)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP COleControlSite::XOleControlSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleControlSite)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControlSite::XOleControlSite::OnControlInfoChanged()
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleControlSite)
	pThis->GetControlInfo();
	return NOERROR;
}

STDMETHODIMP COleControlSite::XOleControlSite::LockInPlaceActive(BOOL)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleControlSite::GetExtendedControl(
	LPDISPATCH*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleControlSite::TransformCoords(
	POINTL* pptHimetric, POINTF* pptContainer, DWORD dwFlags)
{
	METHOD_PROLOGUE_EX_(COleControlSite, OleControlSite)
	HRESULT hr = NOERROR;

	HDC hDC = ::GetDC(pThis->m_hWnd);
	::SetMapMode(hDC, MM_HIMETRIC);
	POINT rgptConvert[2];
	rgptConvert[0].x = 0;
	rgptConvert[0].y = 0;

	if (dwFlags & XFORMCOORDS_HIMETRICTOCONTAINER)
	{
		rgptConvert[1].x = pptHimetric->x;
		rgptConvert[1].y = pptHimetric->y;
		::LPtoDP(hDC, rgptConvert, 2);
		if (dwFlags & XFORMCOORDS_SIZE)
		{
			pptContainer->x = (float)(rgptConvert[1].x - rgptConvert[0].x);
			pptContainer->y = (float)(rgptConvert[0].y - rgptConvert[1].y);
		}
		else if (dwFlags & XFORMCOORDS_POSITION)
		{
			pptContainer->x = (float)rgptConvert[1].x;
			pptContainer->y = (float)rgptConvert[1].y;
		}
		else
		{
			hr = E_INVALIDARG;
		}
	}
	else if (dwFlags & XFORMCOORDS_CONTAINERTOHIMETRIC)
	{
		rgptConvert[1].x = (int)(pptContainer->x);
		rgptConvert[1].y = (int)(pptContainer->y);
		::DPtoLP(hDC, rgptConvert, 2);
		if (dwFlags & XFORMCOORDS_SIZE)
		{
			pptHimetric->x = rgptConvert[1].x - rgptConvert[0].x;
			pptHimetric->y = rgptConvert[0].y - rgptConvert[1].y;
		}
		else if (dwFlags & XFORMCOORDS_POSITION)
		{
			pptHimetric->x = rgptConvert[1].x;
			pptHimetric->y = rgptConvert[1].y;
		}
		else
		{
			hr = E_INVALIDARG;
		}
	}
	else
	{
		hr = E_INVALIDARG;
	}

	::ReleaseDC(pThis->m_hWnd, hDC);

	return hr;
}

STDMETHODIMP COleControlSite::XOleControlSite::TranslateAccelerator(
	LPMSG, DWORD)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XOleControlSite::OnFocus(BOOL)
{
	return S_OK;
}

STDMETHODIMP COleControlSite::XOleControlSite::ShowPropertyFrame()
{
	return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XAmbientProps

STDMETHODIMP_(ULONG) COleControlSite::XAmbientProps::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControlSite, AmbientProps)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlSite::XAmbientProps::Release()
{
	METHOD_PROLOGUE_EX_(COleControlSite, AmbientProps)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP COleControlSite::XAmbientProps::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, AmbientProps)
	return (HRESULT)pThis->InternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControlSite::XAmbientProps::GetTypeInfoCount(
	unsigned int*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XAmbientProps::GetTypeInfo(
	unsigned int, LCID, ITypeInfo**)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XAmbientProps::GetIDsOfNames(
	REFIID, LPOLESTR*, unsigned int, LCID, DISPID*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XAmbientProps::Invoke(
	DISPID dispid, REFIID, LCID, unsigned short wFlags,
	DISPPARAMS* pDispParams, VARIANT* pvarResult,
	EXCEPINFO*, unsigned int*)
{
	UNUSED(wFlags);
	UNUSED(pDispParams);

	METHOD_PROLOGUE_EX(COleControlSite, AmbientProps)
	ASSERT(wFlags & DISPATCH_PROPERTYGET);
	ASSERT(pDispParams->cArgs == 0);

	ASSERT(pThis->m_pCtrlCont != NULL);
	ASSERT(pThis->m_pCtrlCont->m_pWnd != NULL);

	return pThis->m_pCtrlCont->m_pWnd->OnAmbientProperty(pThis, dispid, pvarResult) ?
		S_OK : DISP_E_MEMBERNOTFOUND;
}


/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XPropertyNotifySink

STDMETHODIMP COleControlSite::XPropertyNotifySink::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, PropertyNotifySink)

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_IPropertyNotifySink))
	{
		*ppvObj = this;
		AddRef();
		return S_OK;
	}
	else
	{
		return E_NOINTERFACE;
	}
}

STDMETHODIMP_(ULONG) COleControlSite::XPropertyNotifySink::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COleControlSite::XPropertyNotifySink::Release()
{
	return 0;
}

STDMETHODIMP COleControlSite::XPropertyNotifySink::OnChanged(
	DISPID dispid)
{
	METHOD_PROLOGUE_EX(COleControlSite, PropertyNotifySink)

	// If we are currently updating the control ignore notifications
	if (pThis->m_bIgnoreNotify)
		return S_OK;

	// Give user chance to override
	if (!pThis->m_pDataSourceControl)
	{
		AFX_EVENT event(AFX_EVENT::propChanged, dispid);
		pThis->OnEvent(&event);
		if (event.m_hResult != S_OK)
			return event.m_hResult;
	}

	if (pThis->m_defdispid == dispid)
	{
		::VariantClear(&pThis->m_varResult);

		HRESULT hRes;
		LPDISPATCH pDispatch = NULL;
		hRes = pThis->m_pObject->QueryInterface(IID_IDispatch, (LPVOID *) &pDispatch);
		if (FAILED(hRes))
			return S_OK;

		EXCEPINFO excepinfo;
		memset(&excepinfo, 0, sizeof(EXCEPINFO));
		UINT uArgErr;
		DISPPARAMS dispparamsGetProp;
		memset (&dispparamsGetProp, 0, sizeof(DISPPARAMS));
		hRes = pDispatch->Invoke(dispid, IID_NULL, 0, INVOKE_PROPERTYGET, &dispparamsGetProp,
				&pThis->m_varResult, &excepinfo, &uArgErr); //Get bound control property

		if (excepinfo.bstrSource)
			SysFreeString(excepinfo.bstrSource);
		if (excepinfo.bstrDescription)
			SysFreeString(excepinfo.bstrDescription);
		if (excepinfo.bstrHelpFile)
			SysFreeString(excepinfo.bstrHelpFile);

		pDispatch->Release();

		if (FAILED(hRes))
			return S_OK;

		pThis->m_bIsDirty = TRUE;
	}

	return S_OK;
}

STDMETHODIMP COleControlSite::XPropertyNotifySink::OnRequestEdit(
	DISPID dispid)
{
	METHOD_PROLOGUE_EX(COleControlSite, PropertyNotifySink)

	// If we are currently updating the control ignore notifications
	if (pThis->m_bIgnoreNotify)
		return S_OK;

	// If not bound fire regular MFC event
	if (!pThis->m_pDataSourceControl)
	{
		AFX_EVENT event(AFX_EVENT::propRequest, dispid);
		pThis->OnEvent(&event);
		if (event.m_hResult != S_OK)
			return event.m_hResult;
	}

	// Currently we only support Optimistic binding ala VB4
	// In this model, requests always succeed
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XEventSink

STDMETHODIMP_(ULONG) COleControlSite::XEventSink::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COleControlSite::XEventSink::Release()
{
	return 0;
}

STDMETHODIMP COleControlSite::XEventSink::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, EventSink)

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_IDispatch) ||
		IsEqualIID(iid, pThis->m_iidEvents))
	{
		*ppvObj = this;
		AddRef();
		return S_OK;
	}
	else
	{
		return E_NOINTERFACE;
	}
}

STDMETHODIMP COleControlSite::XEventSink::GetTypeInfoCount(
	unsigned int*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XEventSink::GetTypeInfo(
	unsigned int, LCID, ITypeInfo**)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XEventSink::GetIDsOfNames(
	REFIID, LPOLESTR*, unsigned int, LCID, DISPID*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COleControlSite::XEventSink::Invoke(
	DISPID dispid, REFIID, LCID, unsigned short wFlags,
	DISPPARAMS* pDispParams, VARIANT* pvarResult,
	EXCEPINFO* pExcepInfo, unsigned int* puArgError)
{
	UNUSED(wFlags);

	METHOD_PROLOGUE_EX(COleControlSite, EventSink)
	ASSERT(pThis->m_pCtrlCont != NULL);
	ASSERT(pThis->m_pCtrlCont->m_pWnd != NULL);
	ASSERT(wFlags == DISPATCH_METHOD);

	AFX_EVENT event(AFX_EVENT::event, dispid, pDispParams, pExcepInfo,
		puArgError);

	pThis->OnEvent(&event);

	if (pvarResult != NULL)
		::VariantClear(pvarResult);

	return event.m_hResult;
}

/////////////////////////////////////////////////////////////////////////////
// CDataSourceControl


CDataSourceControl::CDataSourceControl(COleControlSite *pClientSite) :
	m_pClientSite(pClientSite),
	m_pCursorMove(NULL),
	m_pCursorUpdateARow(NULL),
	m_pMetaRowData(NULL),
	m_pVarData(NULL),
	m_nColumns(0),
	m_nBindings(0),
	m_pColumnBindings(NULL),
	m_pValues(NULL),
	m_bUpdateInProgress(FALSE),
	m_pDataSource(NULL),
	m_pRowPosition(NULL),
	m_pRowset(NULL),
	m_pDynamicAccessor(NULL),
	m_dwRowsetNotify(0)
{
	ASSERT(pClientSite);
}

CDataSourceControl::~CDataSourceControl()
{
	// cancel ole/db notifications
	if (m_dwRowsetNotify != 0 && m_pRowset != NULL)
	{
		LPCONNECTIONPOINTCONTAINER pConnPtCont;

		if (SUCCEEDED(m_pRowset->m_spRowset->QueryInterface(IID_IConnectionPointContainer,
				(LPVOID*)&pConnPtCont)))
		{
			ASSERT(pConnPtCont != NULL);
			LPCONNECTIONPOINT pConnPt = NULL;

			if (SUCCEEDED(pConnPtCont->FindConnectionPoint(IID_IRowsetNotify, &pConnPt)))
			{
				ASSERT(pConnPt != NULL);
				pConnPt->Unadvise(m_dwRowsetNotify);
				pConnPt->Release();
			}

			pConnPtCont->Release();
		}
	}

	// Now go through all cursor bound properties
	while (!m_CursorBoundProps.IsEmpty())
	{
		CDataBoundProperty* pProp = (CDataBoundProperty*) m_CursorBoundProps.GetHead();
		pProp->m_pClientSite->BindProperty(pProp->m_dispid, NULL);
		pProp->m_pClientSite->m_pDSCSite = NULL;
	}
	m_CursorBoundProps.RemoveAll();

	if (m_pValues)
	{
		for (int i=0; i<m_nBindings; i++)
			::VariantClear(&m_pValues[i]);
		if (m_nBindings)
		{
			delete[] m_pColumnBindings;
			delete[] m_pValues;
		}
	}

	if (m_pCursorMove)
		m_pCursorMove->Release();

	if (m_pCursorUpdateARow)
		m_pCursorUpdateARow->Release();

	if (m_pMetaRowData)
	{
		for (int nCol=0; nCol<m_nColumns; nCol++)
		{
			POSITION pos = m_pMetaRowData[nCol].m_pClientList->GetHeadPosition();
			while (pos)
			{
				COleControlSite* pSite = (COleControlSite *)
					m_pMetaRowData[nCol].m_pClientList->GetNext(pos);
				pSite->m_pDSCSite = NULL;
			}
			m_pMetaRowData[nCol].m_pClientList->RemoveAll();
			delete m_pMetaRowData[nCol].m_pClientList;
		}
		::CoTaskMemFree(m_pMetaRowData);
	}
	if (m_pVarData)
		::CoTaskMemFree(m_pVarData);

	if(m_pDynamicAccessor != NULL)
	{
		m_pDynamicAccessor->ReleaseAccessors(m_pRowset->m_spRowset);
		m_pDynamicAccessor->Close();
	}
	delete m_pDynamicAccessor;
	delete m_pRowset;
	if (m_pRowPosition != NULL)
		m_pRowPosition->Release();
	if (m_pDataSource != NULL)
		m_pDataSource->Release();
}

interface IVBDSC : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE CancelUnload(BOOL __RPC_FAR* pfCancel) = 0;
	virtual HRESULT STDMETHODCALLTYPE Error(DWORD dwErr, BOOL __RPC_FAR* pfShowError) = 0;
	virtual HRESULT STDMETHODCALLTYPE CreateCursor(ICursor __RPC_FAR* __RPC_FAR* ppCursor) = 0;
};

const IID IID_IVBDSC = {0x1ab42240,0x8c70,0x11ce,{0x94,0x21,0x0,0xaa,0x0,0x62,0xbe,0x57}};
const IID CLSID_DataAdapter = {0x3A08E130L,0x8F65,0x11D0,{0x94,0x84,0x00,0xA0,0xC9,0x11,0x10,0xED}};

HRESULT CDataSourceControl::Initialize()
{
	// The following is a work around for RDC behaviour when on an invisible dlg
	// When the dlg is invisible it cannot display the ODBC connect dialog
	// So check if visible, if not make it so and move it to the center
	// of the screen with null size. Then do the connect dialog
	// Finally put it all back like it was.
	CWnd* pParent = m_pClientSite->m_pWndCtrl->GetTopLevelParent();
	BOOL bHidden = !pParent->IsWindowVisible();
	CRect rcParent;
	if (bHidden)
	{
		CRect rcDesktop;
		CWnd::GetDesktopWindow()->GetWindowRect(&rcDesktop);
		pParent->GetWindowRect(&rcParent);
		pParent->MoveWindow((rcDesktop.right - rcDesktop.left)/2, (rcDesktop.bottom - rcDesktop.top)/2, 0, 0, FALSE);
		pParent->ShowWindow(SW_SHOWNORMAL);
	}
	IVBDSC* pDSC;
	HRESULT hRes;

	hRes = m_pClientSite->m_pObject->QueryInterface(IID_IDataSource, (void**)&m_pDataSource);
	if (SUCCEEDED(hRes))
	{
		hRes = m_pDataSource->GetDataMember(NULL, &IID_IRowPosition, (IUnknown**)&m_pRowPosition);
		if (m_pRowPosition == NULL)
			hRes = E_POINTER;

		if (FAILED(hRes))
		{
			if (bHidden)
			{
				pParent->MoveWindow(rcParent.left, rcParent.top, rcParent.right - rcParent.left, rcParent.bottom - rcParent.top, FALSE);
				pParent->ShowWindow(SW_HIDE);
			}
			return hRes;
		}
	}
	else
	{
		hRes = m_pClientSite->m_pObject->QueryInterface(IID_IVBDSC, (void**)&pDSC);
		if (FAILED(hRes))
			return hRes;
		ICursor* pCursor;
		pDSC->CreateCursor(&pCursor);
		pDSC->Release();
		if (!pCursor)
			return E_FAIL;

		hRes = pCursor->QueryInterface(IID_ICursorMove,
			(LPVOID *)&m_pCursorMove);

		pCursor->Release();

		if (FAILED(hRes))
			return hRes;

		hRes = m_pCursorMove->QueryInterface(IID_ICursorUpdateARow,
			(LPVOID *)&m_pCursorUpdateARow);
	}

	hRes = GetMetaData();

	if (bHidden)
	{
		pParent->MoveWindow(rcParent.left, rcParent.top, rcParent.right - rcParent.left, rcParent.bottom - rcParent.top, FALSE);
		pParent->ShowWindow(SW_HIDE);
	}

	return hRes;
}

IUnknown* CDataSourceControl::GetCursor()
{
	ASSERT(m_pClientSite != NULL);

	if (m_pDataSource != NULL)
	{
		if(m_pRowset != NULL)
			return m_pDataSource;
		return NULL;
	}

	ICursor* pCursor;
	if (!m_pCursorMove)
	{
		IVBDSC* pDSC;
		HRESULT hRes;
		hRes = m_pClientSite->m_pObject->QueryInterface(IID_IVBDSC, (void**)&pDSC);
		if (FAILED(hRes))
			return NULL;
		pDSC->CreateCursor(&pCursor);
		pDSC->Release();
		if (!pCursor)
			return NULL;
		return pCursor;
	}
	if (SUCCEEDED(m_pCursorMove->QueryInterface(IID_ICursor, (LPVOID *) &pCursor)))
			return pCursor;

	ASSERT(FALSE); // DSC Cursor Not Found
	return NULL;
}

HRESULT CDataSourceControl::GetMetaData()
{
	HRESULT hRes;
	METAROWTYPE* pOldMetaData = m_pMetaRowData;
	int nOldColumns = m_nColumns;

	if (m_pDataSource != NULL)
	{
		IRowset* pRowset;

		hRes = m_pRowPosition->GetRowset(IID_IRowset, reinterpret_cast<IUnknown**>(&pRowset));
		if (FAILED(hRes))
			return hRes;

		{
			LPCONNECTIONPOINTCONTAINER pConnPtCont;

			if (SUCCEEDED(pRowset->QueryInterface(IID_IConnectionPointContainer,
					(LPVOID*)&pConnPtCont)))
			{
				ASSERT(pConnPtCont != NULL);
				LPCONNECTIONPOINT pConnPt = NULL;

				if (SUCCEEDED(pConnPtCont->FindConnectionPoint(IID_IRowsetNotify, &pConnPt)))
				{
					ASSERT(pConnPt != NULL);
					pConnPt->Advise(&m_pClientSite->m_xRowsetNotify, &m_dwRowsetNotify);
					pConnPt->Release();
				}

				pConnPtCont->Release();
			}
		}

		m_pRowset = new CRowset(pRowset);
		pRowset->Release();
		m_pRowset->SetupOptionalRowsetInterfaces();
		m_pDynamicAccessor = new CDynamicAccessor;
		m_pDynamicAccessor->BindColumns(m_pRowset->m_spRowset);
		m_pRowset->SetAccessor(m_pDynamicAccessor);
		m_nColumns = m_pDynamicAccessor->GetColumnCount();
		m_pMetaRowData = (METAROWTYPE*)::CoTaskMemAlloc(sizeof(METAROWTYPE) * m_nColumns);
		ASSERT(m_pMetaRowData);
		memset(m_pMetaRowData, 0, sizeof(METAROWTYPE) * m_nColumns);
		m_pRowset->MoveFirst();
		m_pRowset->ReleaseRows();
	}
	else
	{
		ULONG nRows;
		ICursor* pCursor = (LPCURSOR)m_pCursorMove;
		ICursor* pColumnCursor;

		if (pCursor == NULL)
			return S_OK;

		hRes = pCursor->GetColumnsCursor(IID_ICursor, (IUnknown **) &pColumnCursor, &nRows);
		if (FAILED(hRes))
			return hRes;

		DBCOLUMNBINDING MetaColumns[2];
		CopyColumnID(&MetaColumns[0].columnID, &COLUMN_COLUMNID);
		MetaColumns[0].obData = offsetof(METAROWTYPE, idColumnID);
		MetaColumns[0].cbMaxLen = DB_NOMAXLENGTH;
		MetaColumns[0].obInfo = offsetof(METAROWTYPE, dwColumnID);
		MetaColumns[0].obVarDataLen = DB_NOVALUE;
		MetaColumns[0].dwBinding = DBBINDING_DEFAULT;
		MetaColumns[0].dwDataType = DBTYPE_COLUMNID;

		CopyColumnID(&MetaColumns[1].columnID, &COLUMN_NAME);
		MetaColumns[1].obData = offsetof(METAROWTYPE, lpstrName);
		MetaColumns[1].cbMaxLen = DB_NOMAXLENGTH;
		MetaColumns[1].obInfo = offsetof(METAROWTYPE, dwName);
		MetaColumns[1].obVarDataLen = DB_NOVALUE;
		MetaColumns[1].dwBinding = DBBINDING_DEFAULT;
		MetaColumns[1].dwDataType = VT_LPSTR;

		hRes = pColumnCursor->SetBindings(2, MetaColumns, sizeof(METAROWTYPE),
			DBCOLUMNBINDOPTS_REPLACE);
		if (FAILED(hRes))
		{
			pColumnCursor->Release();
			return hRes;
		}

		DBFETCHROWS FetchRows;
		FetchRows.cRowsRequested = nRows;
		FetchRows.dwFlags = DBROWFETCH_CALLEEALLOCATES;
		FetchRows.pData = NULL;
		FetchRows.pVarData = NULL;
		FetchRows.cbVarData = 0;

		LARGE_INTEGER dlZero;
		LISet32(dlZero, 0);
		hRes = pColumnCursor->GetNextRows(dlZero, &FetchRows);
		if (FAILED(hRes))
		{
			pColumnCursor->Release();
			return hRes;
		}

		m_pMetaRowData = (METAROWTYPE *)FetchRows.pData;
		ASSERT(m_pMetaRowData);
		nRows = FetchRows.cRowsReturned;       // in case it changed
		m_pVarData = FetchRows.pVarData;

		m_nColumns = nRows;

		pColumnCursor->Release();
	}

	for (int nCol=0; nCol<m_nColumns; nCol++)
		m_pMetaRowData[nCol].m_pClientList = new CPtrList;

	// re-establish all bound property sites and then delete old meta-data
	if (pOldMetaData != NULL)
	{
		for (int nCol=0; nCol<nOldColumns; nCol++)
		{
			POSITION pos = pOldMetaData[nCol].m_pClientList->GetHeadPosition();
			while (pos)
			{
				COleControlSite* pSite = (COleControlSite *)
					m_pMetaRowData[nCol].m_pClientList->GetNext(pos);
				BindProp(pSite, TRUE);
			}
			pOldMetaData[nCol].m_pClientList->RemoveAll();
			delete pOldMetaData[nCol].m_pClientList;
		}
		::CoTaskMemFree(pOldMetaData);
	}

	return hRes;
}

BOOL CDataSourceControl::CopyColumnID(
	DBCOLUMNID* pcidDst, DBCOLUMNID const *pcidSrc)
{
	pcidDst->dwKind = pcidSrc->dwKind;

	switch (pcidSrc->dwKind)
	{
	case DBCOLKIND_GUID_NUMBER:
		pcidDst->guid = pcidSrc->guid;
		pcidDst->lNumber = pcidSrc->lNumber;
		break;
	case DBCOLKIND_GUID_NAME:
		pcidDst->guid = pcidSrc->guid;
		// fall through
	case DBCOLKIND_NAME:
		pcidDst->lpdbsz = (LPDBSTR) ::CoTaskMemAlloc(sizeof(DBCHAR) * (ldbstrlen(pcidSrc->lpdbsz) + 1));
		if (!pcidDst->lpdbsz)
			return FALSE;
		ldbstrcpy(pcidDst->lpdbsz, pcidSrc->lpdbsz);
		break;
	}
	return TRUE;
}

// Make a bound control/bound property a consumer to a particular column in this DSC
void CDataSourceControl::BindProp(COleControlSite* pClientSite, BOOL bBind)
{
	ASSERT(pClientSite);

	if (bBind)
	{
		BindProp(pClientSite, FALSE);
		ASSERT(pClientSite->m_pDSCSite == m_pClientSite);
		if (m_pDataSource != NULL)
		{
			for (int nCol=0; nCol<m_nColumns; nCol++)
			{
				if (pClientSite->m_strDataField ==
#ifdef __BORLANDC__ // MS Ambiguity error fix
                                    CString(
#endif
				      m_pDynamicAccessor->GetColumnName(nCol + 1)
#ifdef __BORLANDC__
                                    )
#endif
				)
				{
					m_pMetaRowData[nCol].m_pClientList->AddTail(pClientSite);
					return;
				}
			}
		}
		else
		{
			for (int nCol=0; nCol<m_nColumns; nCol++)
			{
				if (m_pMetaRowData[nCol].lpstrName == NULL)
					continue;
				if (pClientSite->m_strDataField == m_pMetaRowData[nCol].lpstrName)
				{
					m_pMetaRowData[nCol].m_pClientList->AddTail(pClientSite);
					return;
				}
			}
		}
		pClientSite->m_pDSCSite = NULL;
		return;
	}
	UpdateCursor();
	// UnBind
	for (int nCol=0; nCol<m_nColumns; nCol++)
	{
		POSITION pos = m_pMetaRowData[nCol].m_pClientList->GetHeadPosition();
		POSITION prev;
		while (pos)
		{
			prev = pos;
			COleControlSite* pSite = (COleControlSite *)
				m_pMetaRowData[nCol].m_pClientList->GetNext(pos);
			if (pSite == pClientSite)
			{
				m_pMetaRowData[nCol].m_pClientList->RemoveAt(prev);
				return;
			}
		}
	}
}

// Make a cursor bound control property a client to this control
void CDataSourceControl::BindProp(CDataBoundProperty* pProperty, BOOL bBind)
{
	ASSERT(pProperty);

	if (bBind)
	{
		BindProp(pProperty, FALSE);
		m_CursorBoundProps.AddTail(pProperty);
	}
	else
	{
		UpdateCursor();
		POSITION pos = m_CursorBoundProps.Find(pProperty);
		if (pos != NULL)
			m_CursorBoundProps.RemoveAt(pos);
	}
}

void CDataSourceControl::BindColumns()
{
	if (m_pDataSource != NULL)
	{
		// this is done automatically by CDynamicAccessor
		GetBoundClientRow();
		UpdateControls();
		return;
	}
	if (m_pValues)
	{
		for (int i=0; i<m_nBindings; i++)
			::VariantClear(&m_pValues[i]);
		if (m_nBindings > 0)
		{
			delete[] m_pValues;
			delete[] m_pColumnBindings;
		}
		m_pValues = NULL;
	}
	m_nBindings = 0;
	for (int nCol=0; nCol<m_nColumns; nCol++)
	{
		m_nBindings += m_pMetaRowData[nCol].m_pClientList->GetCount();
	}
	if (m_nBindings > 0)
		m_pColumnBindings = new DBCOLUMNBINDING[m_nBindings];
	int nItem = 0;
	for (nCol=0; nCol<m_nColumns; nCol++)
	{
		POSITION pos = m_pMetaRowData[nCol].m_pClientList->GetHeadPosition();
		while (pos)
		{
			COleControlSite* pSite = (COleControlSite *)
				m_pMetaRowData[nCol].m_pClientList->GetNext(pos);
			CopyColumnID(&m_pColumnBindings[nItem].columnID, &m_pMetaRowData[nCol].idColumnID);
			m_pColumnBindings[nItem].obData = sizeof(VARIANT) * nItem;
			m_pColumnBindings[nItem].cbMaxLen = DB_NOMAXLENGTH;
			m_pColumnBindings[nItem].obInfo = DB_NOVALUE;
			m_pColumnBindings[nItem].obVarDataLen = DB_NOVALUE;
			m_pColumnBindings[nItem].dwBinding = DBBINDING_VARIANT;
			m_pColumnBindings[nItem].dwDataType = pSite->m_dwType;
			nItem++;
		}
	}
	m_pCursorMove->SetBindings(m_nBindings, m_pColumnBindings,
		sizeof(VARIANT) * m_nBindings, DBCOLUMNBINDOPTS_REPLACE);

	if (m_nBindings)
		m_pValues = new VARIANT[m_nBindings];

	for (int i=0; i<m_nBindings; i++)
	{
		memset(&m_pValues[i], 0, sizeof(VARIANT));
		m_pValues[i].vt = VT_EMPTY;
	}

	GetBoundClientRow();
	UpdateControls();
}

HRESULT CDataSourceControl::GetBoundClientRow()
{
	DBFETCHROWS FetchRows;

	if (m_pDataSource != NULL)
	{
		if(m_pRowset == NULL)
			return S_OK;

		if(m_pRowset->m_hRow == NULL)
			return S_OK;

		return m_pRowset->GetData();
	}

	if (m_nBindings == 0)
		return S_OK;

	FetchRows.pData = m_pValues;
	FetchRows.pVarData = NULL;
	FetchRows.cbVarData = NULL;
	FetchRows.cRowsRequested = 1;
	FetchRows.dwFlags = 0;

	LARGE_INTEGER dl = { 0, 0};

	return m_pCursorMove->Move(1, (LPVOID)&DBBMK_CURRENT, dl, &FetchRows);
}

COleVariant CDataSourceControl::ToVariant(int nCol)
{
	ASSERT(m_pDataSource != NULL);
	ASSERT(m_pDynamicAccessor != NULL);
	COleVariant vt;
	DBSTATUS dbStatus;
	DBTYPE dbType;

	m_pDynamicAccessor->GetStatus(nCol, &dbStatus);
	if(dbStatus == DBSTATUS_S_ISNULL)
		return vt;                      // just return a blank variant

	if(!m_pDynamicAccessor->GetColumnType(nCol, &dbType))
		return vt;
	switch (dbType)
	{
	case DBTYPE_VARIANT:
		vt = COleVariant((LPCVARIANT)m_pDynamicAccessor->GetValue(nCol));
		break;
	case DBTYPE_STR:
		vt = COleVariant(CString((LPCSTR)m_pDynamicAccessor->GetValue(nCol)), VT_BSTR);
		break;
	case DBTYPE_WSTR:
	case DBTYPE_BSTR:
		vt = COleVariant(CString((LPCWSTR)m_pDynamicAccessor->GetValue(nCol)), VT_BSTR);
		break;
	case DBTYPE_I1:
	case DBTYPE_UI1:
		vt = COleVariant(*((BYTE*)m_pDynamicAccessor->GetValue(nCol)));
		break;
	case DBTYPE_I2:
	case DBTYPE_UI2:
		vt = COleVariant(*((short*)m_pDynamicAccessor->GetValue(nCol)));
		break;
	case DBTYPE_I4:
	case DBTYPE_UI4:
		vt = COleVariant(*((long*)m_pDynamicAccessor->GetValue(nCol)));
		break;
	case DBTYPE_R4:
		vt = COleVariant(*((float*)m_pDynamicAccessor->GetValue(nCol)));
		break;
	case DBTYPE_R8:
		vt = COleVariant(*((double*)m_pDynamicAccessor->GetValue(nCol)));
		break;
	case DBTYPE_BOOL:
		vt = COleVariant((short)*(BOOL*)m_pDynamicAccessor->GetValue(nCol), VT_BOOL);
		break;
	case DBTYPE_DATE:
		{
			COleDateTime dt(*((DATE*)m_pDynamicAccessor->GetValue(nCol)));
			vt = COleVariant(dt);
		}
		break;
	case DBTYPE_CY:
		{
			COleCurrency cy(*((CURRENCY*)m_pDynamicAccessor->GetValue(nCol)));
			vt = COleVariant(cy);
		}
		break;
	case DBTYPE_NUMERIC:
		{
			DB_NUMERIC num;

			if(m_pDynamicAccessor->GetValue(nCol, &num))
			{
				double dbl;

				dbl = (double)*((__int64*)num.val);
				while(num.scale-- > 0)
					dbl /= 10;
				if(num.sign == 0)
					dbl = -dbl;
				vt = COleVariant(dbl);
			}
		}
		break;
	case DBTYPE_DBDATE:
		{
			DBDATE dbDate;

			if(m_pDynamicAccessor->GetValue(nCol, &dbDate))
			{
				COleDateTime dt;

				dt.SetDate(dbDate.year, dbDate.month, dbDate.day);
				vt = COleVariant(dt);
			}
		}
		break;
	case DBTYPE_DBTIME:
		{
			DBTIME dbTime;

			if(m_pDynamicAccessor->GetValue(nCol, &dbTime))
			{
				COleDateTime dt;

				dt.SetTime(dbTime.hour, dbTime.minute, dbTime.second);
				vt = COleVariant(dt);
			}
		}
		break;
	case DBTYPE_DBTIMESTAMP:
		{
			DBTIMESTAMP dbTimeStamp;

			if(m_pDynamicAccessor->GetValue(nCol, &dbTimeStamp))
			{
				vt = COleVariant(COleDateTime(dbTimeStamp.year, dbTimeStamp.month, dbTimeStamp.day,
											  dbTimeStamp.hour, dbTimeStamp.minute, dbTimeStamp.second));
			}
		}
		break;
	case DBTYPE_NULL:
	case DBTYPE_EMPTY:
		break;
	default:
		TRACE2("Unsupported DBTYPE (%d) in column %d\n", dbType, nCol);
		break;
	}
	return vt;
}

HRESULT CDataSourceControl::UpdateControls()
{
	m_bUpdateInProgress = TRUE;
	int nItem = 0;
	for (int nCol=0; nCol<m_nColumns; nCol++)
	{
		POSITION pos = m_pMetaRowData[nCol].m_pClientList->GetHeadPosition();
		while (pos)
		{
			COleControlSite* pSite = (COleControlSite *)
				m_pMetaRowData[nCol].m_pClientList->GetNext(pos);

			DISPPARAMS dispparamsSetProp;
			DISPID dispidNamed = DISPID_PROPERTYPUT;
			EXCEPINFO excepinfo;
			memset(&excepinfo, 0, sizeof(EXCEPINFO));
			UINT uArgErr;
			COleVariant vt;
			if (m_pDataSource != NULL)
			{
				vt = ToVariant(nCol + 1);
				dispparamsSetProp.rgvarg = &vt;
			}
			else
			{
				dispparamsSetProp.rgvarg = &m_pValues[nItem];
			}
			dispparamsSetProp.rgdispidNamedArgs = &dispidNamed;
			dispparamsSetProp.cArgs = 1;
			dispparamsSetProp.cNamedArgs = 1;
			HRESULT hRes;
			LPDISPATCH pDispatch;
			pSite->m_bIgnoreNotify = TRUE;
			if (pSite->m_pObject == NULL)
				continue;
			hRes = pSite->m_pObject->QueryInterface(IID_IDispatch, (LPVOID *) &pDispatch);
			if (FAILED(hRes))
				continue;
			hRes = pDispatch->Invoke(pSite->m_defdispid, IID_NULL, 0, INVOKE_PROPERTYPUT, &dispparamsSetProp,
				NULL, &excepinfo, &uArgErr);    //Set the bound control property
			pDispatch->Release();
			pSite->m_bIgnoreNotify = FALSE;

			if (excepinfo.bstrSource)
				SysFreeString(excepinfo.bstrSource);
			if (excepinfo.bstrDescription)
				SysFreeString(excepinfo.bstrDescription);
			if (excepinfo.bstrHelpFile)
				SysFreeString(excepinfo.bstrHelpFile);

			vt.Clear();
			nItem++;
		}
	}
	m_bUpdateInProgress = FALSE;
	return S_OK;
}

HRESULT CDataSourceControl::UpdateCursor()
{
	HRESULT hRes;
	int nVariant = 0;
	int nDirtyField = 0;

	if (m_pDataSource != NULL)
	{
		if(m_pDynamicAccessor == NULL)
			return S_OK;
		// First go through all simple bound properties
		for (int nCol=1; nCol<=m_nColumns; nCol++)
		{
			POSITION pos = m_pMetaRowData[nCol - 1].m_pClientList->GetHeadPosition();
			while (pos)
			{
				COleControlSite* pSite = (COleControlSite *)
					m_pMetaRowData[nCol - 1].m_pClientList->GetNext(pos);
				DBTYPE dbType;

				if (pSite->m_bIsDirty)
				{
					pSite->m_bIsDirty = FALSE;

					nDirtyField++;
					if (nDirtyField == 1)
					{
						// UpdateinProgress semaphore - unexpected state
						ASSERT(!m_bUpdateInProgress);
						m_bUpdateInProgress = TRUE;
					}

					COleVariant var(pSite->m_varResult);
					CManualAccessor accessor;

					// Create accessor for output column
					accessor.CreateAccessor(1, m_pDynamicAccessor->GetValue(nCol), m_pDynamicAccessor->m_pColumnInfo[nCol].ulColumnSize);
					accessor.AddBindEntry(m_pDynamicAccessor->m_pColumnInfo[nCol].iOrdinal,
										  m_pDynamicAccessor->m_pColumnInfo[nCol].wType,
										  m_pDynamicAccessor->m_pColumnInfo[nCol].ulColumnSize,
										  m_pDynamicAccessor->GetValue(nCol));
					accessor.BindColumns(m_pRowset->m_spRowset);

					VERIFY(m_pDynamicAccessor->GetColumnType(nCol, &dbType));
					switch(dbType)
					{
					case DBTYPE_I2:
						var.ChangeType(VT_I2);
						m_pDynamicAccessor->SetValue(nCol, var.iVal);
						break;
					case DBTYPE_I4:
						var.ChangeType(VT_I4);
						m_pDynamicAccessor->SetValue(nCol, var.lVal);
						break;
					case DBTYPE_R4:
						var.ChangeType(VT_R4);
						m_pDynamicAccessor->SetValue(nCol, var.fltVal);
						break;
					case DBTYPE_R8:
						var.ChangeType(VT_R8);
						m_pDynamicAccessor->SetValue(nCol, var.dblVal);
						break;
					case DBTYPE_CY:
						var.ChangeType(VT_CY);
						*(CY*)m_pDynamicAccessor->GetValue(nCol) = var.cyVal;
						break;
					case DBTYPE_DATE:
						var.ChangeType(VT_DATE);
						m_pDynamicAccessor->SetValue(nCol, var.date);
						break;
					case DBTYPE_BSTR:
					case DBTYPE_WSTR:
						var.ChangeType(VT_BSTR);
						wcsncpy((wchar_t*)m_pDynamicAccessor->GetValue(nCol), var.bstrVal, m_pDynamicAccessor->m_pColumnInfo[nCol].ulColumnSize);
						break;
					case DBTYPE_BOOL:
						var.ChangeType(VT_BOOL);
						m_pDynamicAccessor->SetValue(nCol, var.boolVal);
						break;
					case DBTYPE_DECIMAL:
						var.ChangeType(VT_DECIMAL);
						m_pDynamicAccessor->SetValue(nCol, var.decVal);
						break;
					case DBTYPE_UI1:
						var.ChangeType(VT_UI1);
						m_pDynamicAccessor->SetValue(nCol, var.bVal);
						break;
					case DBTYPE_I1:
						var.ChangeType(VT_I1);
						m_pDynamicAccessor->SetValue(nCol, (signed char)var.cVal);
						break;
					case DBTYPE_UI2:
						var.ChangeType(VT_UI2);
						m_pDynamicAccessor->SetValue(nCol, var.uiVal);
						break;
					case DBTYPE_UI4:
						var.ChangeType(VT_UI4);
						m_pDynamicAccessor->SetValue(nCol, var.ulVal);
						break;
					case DBTYPE_STR:
						var.ChangeType(VT_BSTR);
						WideCharToMultiByte(CP_ACP, 0, var.bstrVal, -1, (char*)m_pDynamicAccessor->GetValue(nCol),
											m_pDynamicAccessor->m_pColumnInfo[nCol].ulColumnSize, NULL, NULL);
						break;
					case DBTYPE_DBTIMESTAMP:
						DBTIMESTAMP ts;
						SYSTEMTIME	sysTime;
						var.ChangeType(VT_DATE);
						memset(&ts, 0, sizeof(DBTIMESTAMP));
						VariantTimeToSystemTime(var.date, &sysTime);
						ts.year = sysTime.wYear;
						ts.month = sysTime.wMonth;
						ts.day = sysTime.wDay;
						ts.hour = sysTime.wHour;
						ts.minute = sysTime.wMinute;
						ts.second = sysTime.wSecond;
						ts.fraction = sysTime.wMilliseconds / 1000000;
						m_pDynamicAccessor->SetValue(nCol, ts);
						break;
					default:
						TRACE2("Unsupported DBTYPE (%d) in column %d\n", dbType, nCol);
						break;
					}

					m_pRowset->SetAccessor(&accessor);
					m_pRowset->SetData();
					m_pRowset->SetAccessor(m_pDynamicAccessor);

					::VariantClear(&pSite->m_varResult);
				}
				nVariant++;
			}
		}
		if (nDirtyField > 0)
		{
			m_bUpdateInProgress = FALSE;
		}
		return S_OK;
	}
	// Do we have an updateable cursor?
	if (m_pCursorUpdateARow == NULL)
	{
		// If not attempt to get one
		hRes = m_pCursorMove->QueryInterface(IID_ICursorUpdateARow,
			(LPVOID *)&m_pCursorUpdateARow);

		if (FAILED(hRes))
			return S_OK; // no update cursor, so forget updating
	}

	// First go through all simple bound properties
	for (int nCol=0; nCol<m_nColumns; nCol++)
	{
		POSITION pos = m_pMetaRowData[nCol].m_pClientList->GetHeadPosition();
		while (pos)
		{
			COleControlSite* pSite = (COleControlSite *)
				m_pMetaRowData[nCol].m_pClientList->GetNext(pos);

			if (pSite->m_bIsDirty)
			{
				pSite->m_bIsDirty = FALSE;

				nDirtyField++;
				if (nDirtyField == 1)
				{
					DWORD dwEdit;
					hRes = m_pCursorUpdateARow->GetEditMode(&dwEdit);
					if (FAILED(hRes))
						return hRes;
					if (dwEdit == DBEDITMODE_NONE)
					{
						hRes = m_pCursorUpdateARow->BeginUpdate(DBROWACTION_UPDATE);
						if (FAILED(hRes))
							return hRes;
					}
					// UpdateinProgress semaphore - unexpected state
					ASSERT(!m_bUpdateInProgress);
					m_bUpdateInProgress = TRUE;
				}
				DBBINDPARAMS bpBindParams;
				DWORD dwIndicator = sizeof(VARIANT);
				COleVariant vt;
				switch (V_VT(&pSite->m_varResult))
				{
				case VT_EMPTY:
				case VT_NULL:
				case VT_ERROR:
					dwIndicator = DB_NULL;
					vt = pSite->m_varResult;
					break;
				case VT_BSTR:
					if(*pSite->m_varResult.bstrVal == 0)
					{
						dwIndicator = DB_NULL;
						break;
					}
				default:
					vt = pSite->m_varResult;
					break;
				}
				bpBindParams.cbMaxLen     = 0L;
				bpBindParams.dwBinding    = DBBINDING_VARIANT;
				bpBindParams.dwDataType   = m_pColumnBindings[nVariant].dwDataType;
				bpBindParams.cbVarDataLen = 0L;
				bpBindParams.dwInfo       = dwIndicator;
				bpBindParams.pData        = &vt;

				hRes = m_pCursorUpdateARow->SetColumn(&m_pColumnBindings[nVariant].columnID, &bpBindParams);

				::VariantClear(&pSite->m_varResult);
			}
			nVariant++;
		}
	}
	// Now go through all cursor bound properties
	POSITION pos = m_CursorBoundProps.GetHeadPosition();
	while (pos != NULL)
	{
		CDataBoundProperty* pProp = (CDataBoundProperty*) m_CursorBoundProps.GetNext(pos);
		if (pProp->m_pClientSite->m_pObject == NULL)
			continue;
		IBoundObject *pBO;
		if (SUCCEEDED(pProp->m_pClientSite->m_pObject->QueryInterface(IID_IBoundObject,
			(LPVOID *) &pBO)))
		{
			if (pBO->IsDirty(pProp->m_dispid) == S_OK)
			{
				nDirtyField++;
				if (nDirtyField == 1)
				{
					DWORD dwEdit;
					hRes = m_pCursorUpdateARow->GetEditMode(&dwEdit);
					if (FAILED(hRes))
						return hRes;
					if (dwEdit == DBEDITMODE_NONE)
					{
						hRes = m_pCursorUpdateARow->BeginUpdate(DBROWACTION_UPDATE);
						if (FAILED(hRes))
							return hRes;
					}
					// UpdateinProgress semaphore - unexpected state
					ASSERT(!m_bUpdateInProgress);
					m_bUpdateInProgress = TRUE;
				}
			}
			pBO->Release();
		}
	}

	if (nDirtyField > 0)
	{
		hRes = m_pCursorUpdateARow->Update(0,0,0);
		m_bUpdateInProgress = FALSE;
		if (FAILED(hRes))
		{
			// Update failed w/dirty controls
			ASSERT(hRes!= S_OK);
			UpdateControls();
			return hRes;
		}
	}

	return S_OK;
}


/////////////////////////////////////////////////////////////////////////////
// CDataBoundProperty Handles Databound Controls

CDataBoundProperty::CDataBoundProperty(CDataBoundProperty* pLast, DISPID dispid, WORD ctlid) :
	m_dispid(dispid),
	m_ctlid(ctlid),
	m_pClientSite(NULL),
	m_pDSCSite(NULL),
	m_bIsDirty(FALSE),
	m_pNext(pLast)
{
}

void CDataBoundProperty::SetClientSite(COleControlSite *pClientSite)
{
	m_pClientSite = pClientSite;
}


// (Re)bind a cursor bound property to a DSC
void CDataBoundProperty::SetDSCSite(COleControlSite *pDSCSite)
{
	if (m_pDSCSite == pDSCSite)
		return;
	m_pDSCSite = pDSCSite;
	Notify();
}

CDataBoundProperty* CDataBoundProperty::GetNext()
{
	return m_pNext;
}

void CDataBoundProperty::RemoveSource()
{
	m_pDSCSite = NULL;
	Notify();
}

void CDataBoundProperty::Notify()
{
	if (m_dispid != DISPID_DATASOURCE)
	{
		IBoundObject *pBO;
		if (m_pClientSite != NULL)
		{
			if (SUCCEEDED(m_pClientSite->m_pObject->QueryInterface(IID_IBoundObject,
				(LPVOID *) &pBO)))
			{
				pBO->OnSourceChanged(m_dispid, m_pDSCSite != NULL, &m_bOwnXferOut);
				pBO->Release();
			}
			else
			{
				IUnknown* pUnk = GetCursor();
				if (pUnk != NULL)
				{
					VARTYPE vt = VT_UNKNOWN;
					if (m_pDSCSite->m_pDataSourceControl != NULL)
						vt |= VT_MFCFORCEPUTREF;
					m_pClientSite->SetProperty(m_dispid, vt, pUnk);
				}
			}
		}
	}
}

IUnknown* CDataBoundProperty::GetCursor()
{
	if(m_pDSCSite == NULL)
		return NULL;

	m_pDSCSite->EnableDSC();

	ASSERT(m_pDSCSite->m_pDataSourceControl);

	m_pDSCSite->m_pDataSourceControl->BindProp(this);

	return m_pDSCSite->m_pDataSourceControl->GetCursor();
}

/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XBoundObjectSite

HRESULT COleControlSite::GetCursor(
	DISPID dispid, LPUNKNOWN* ppcursorOut, LPVOID *ppcidOut)
{
	if (ppcidOut != NULL)
		*ppcidOut = NULL;

	CDataBoundProperty* pBinding = m_pBindings;
	while (pBinding != NULL)
	{
		if (pBinding->m_dispid == dispid)
		{
			*ppcursorOut = pBinding->GetCursor();
			return S_OK;
		}
		pBinding = pBinding->GetNext();
	}
	return S_OK;
}

STDMETHODIMP_(ULONG) COleControlSite::XBoundObjectSite::AddRef()
{
	METHOD_PROLOGUE_EX(COleControlSite, BoundObjectSite)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControlSite::XBoundObjectSite::Release()
{
	METHOD_PROLOGUE_EX(COleControlSite, BoundObjectSite)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControlSite::XBoundObjectSite::QueryInterface(
	REFIID iid, LPVOID far * ppvObj)
{
	METHOD_PROLOGUE_EX(COleControlSite, BoundObjectSite)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControlSite::XBoundObjectSite::GetCursor(
	DISPID dispid, LPLPCURSOR ppcursorOut, LPVOID *ppcidOut)
{
	METHOD_PROLOGUE_EX(COleControlSite, BoundObjectSite)
	return pThis->GetCursor(dispid, (LPUNKNOWN*)ppcursorOut, ppcidOut);
}


/////////////////////////////////////////////////////////////////////////////
// COleControlSite::XNotifyDBEvents

STDMETHODIMP_(ULONG) COleControlSite::XNotifyDBEvents::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COleControlSite::XNotifyDBEvents::Release()
{
	return 0;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::QueryInterface(
	REFIID iid, LPVOID far * ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, NotifyDBEvents)

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_INotifyDBEvents))
	{
		*ppvObj = this;
		AddRef();
		return S_OK;
	}
	else
	{
		return E_NOINTERFACE;
	}
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::OKToDo(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscOKToDo);
	if (FAILED(hRes))
		return hRes;

	DWORD reason = rgReasons[0].dwReason;

	if (reason == DBREASON_SETCOLUMN ||
		reason == DBREASON_INSERTED ||
		reason == DBREASON_MODIFIED)
		return S_OK;

	// Mask out all notifications except as currency changes and update
	if (!(dwEventWhat & DBEVENT_CURRENT_ROW_CHANGED) &&
		reason != DBREASON_INSERTED &&
		reason != DBREASON_MODIFIED &&
		reason != DBREASON_DELETED &&
		reason != DBREASON_ADDNEW)
		return S_OK;

	CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
	if (pDSC == NULL)
		return S_OK;

	if (!(dwEventWhat & DBEVENT_CURRENT_ROW_CHANGED))
		return S_OK;

	return pDSC->UpdateCursor();
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::Cancelled(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscCancelled);
	if (FAILED(hRes))
		return hRes;

	return S_OK;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::SyncBefore(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscSyncBefore);
	if (FAILED(hRes))
		return hRes;

	DWORD reason = rgReasons[0].dwReason;

	// Mask out all notifications except as shown
	if (reason != DBREASON_INSERTED &&
		reason != DBREASON_MODIFIED &&
		reason != DBREASON_ADDNEW)
		return S_OK;

	CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
	if (pDSC == NULL)
		return S_OK;

	if (dwEventWhat & DBEVENT_METADATA_CHANGED)
	{
		pDSC->UpdateCursor();
	}

	return S_OK;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::AboutToDo(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscAboutToDo);
	if (FAILED(hRes))
		return hRes;

	return S_OK;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::FailedToDo(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscFailedToDo);
	if (FAILED(hRes))
		return hRes;

	return S_OK;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::SyncAfter(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscSyncAfter);
	if (FAILED(hRes))
		return hRes;

	DWORD reason = rgReasons[0].dwReason;

	CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
	if (pDSC == NULL)
		return S_OK;

	//Check semaphore - don't want to collect data during Update if it was due to a Move (it's a NOP!)
	if (pDSC->m_bUpdateInProgress ||
		reason == DBREASON_EDIT ||
		reason == DBREASON_SETCOLUMN)
		return S_OK;

	//Mask out all notifications except as shown
	if (!(dwEventWhat & (DBEVENT_CURRENT_ROW_CHANGED | DBEVENT_CURRENT_ROW_DATA_CHANGED)))
		return S_OK;

	if (reason != DBREASON_INSERTED &&
		reason != DBREASON_MODIFIED)
	{
		BOOL bUpdateInProgress = pDSC->m_bUpdateInProgress;
		pDSC->m_bUpdateInProgress = TRUE;
		pDSC->GetBoundClientRow();
		pDSC->m_bUpdateInProgress = bUpdateInProgress;
		pDSC->UpdateControls();
	}

	return S_OK;
}

STDMETHODIMP COleControlSite::XNotifyDBEvents::DidEvent(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[])
{
	METHOD_PROLOGUE_EX(COleControlSite, NotifyDBEvents)

	HRESULT hRes = FireEvent(dwEventWhat, cReasons, rgReasons,
		dscDidEvent);
	if (FAILED(hRes))
		return hRes;

	CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
	if (pDSC == NULL)
		return S_OK;

	if (dwEventWhat & DBEVENT_METADATA_CHANGED)
	{
		BOOL bUpdateInProgress = pDSC->m_bUpdateInProgress;
		pDSC->m_bUpdateInProgress = TRUE;
		pDSC->GetBoundClientRow();
		pDSC->m_bUpdateInProgress = bUpdateInProgress;
		pDSC->UpdateControls();
	}

	return S_OK;
}


HRESULT COleControlSite::XNotifyDBEvents::FireEvent(
	DWORD dwEventWhat, ULONG cReasons, DBNOTIFYREASON rgReasons[], DSCSTATE nState)
{
	METHOD_PROLOGUE_EX_(COleControlSite, NotifyDBEvents)

	if (dwEventWhat & DBEVENT_CURRENT_ROW_CHANGED
		|| dwEventWhat & DBEVENT_CURRENT_ROW_DATA_CHANGED)
	{
		for (UINT i=0; i<cReasons; i++)
		{
			DSCREASON nReason = dscNoReason;
			switch (rgReasons[i].dwReason)
			{
			case DBREASON_CLOSE:
				nReason = dscClose;
				break;
			case DBREASON_FIND:
			case DBREASON_MOVE:
			case DBREASON_MOVEPERCENT:
			case DBREASON_NEWINDEX:
			case DBREASON_NEWPARAMETERS:
			case DBREASON_QUERYSPECCHANGED:
			case DBREASON_REFRESH:
			case DBREASON_SEEK:
			case DBREASON_SETRANGECHANGED:
				nReason = dscMove;
				break;
			case DBREASON_ADDNEW:
			case DBREASON_INSERTED:
				nReason = dscInsert;
				break;
			case DBREASON_EDIT:
				nReason = dscEdit;
				break;
			case DBREASON_MODIFIED:
			case DBREASON_RECALC:
			case DBREASON_ROLLBACK:
			case DBREASON_ROWFIXUP:
				nReason = dscModify;
				break;
			case DBREASON_DELETED:
				nReason = dscDelete;
				break;
			case DBREASON_COMMIT:
				nReason = dscCommit;
				break;
			}
			if (nReason != dscNoReason)
			{
				AFX_EVENT event(AFX_EVENT::propDSCNotify);
				event.m_nDSCState = nState;
				event.m_nDSCReason = nReason;
				pThis->OnEvent(&event);
				return event.m_hResult;
			}
		}
		return S_OK;
	}
	return S_OK;
}

STDMETHODIMP_(ULONG) COleControlSite::XRowsetNotify::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COleControlSite::XRowsetNotify::Release()
{
	return 0;
}

STDMETHODIMP COleControlSite::XRowsetNotify::QueryInterface(
	REFIID iid, LPVOID far * ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControlSite, RowsetNotify)

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_IRowsetNotify))
	{
		*ppvObj = this;
		AddRef();
		return S_OK;
	}
	return E_NOINTERFACE;
}

STDMETHODIMP COleControlSite::XRowsetNotify::OnFieldChange(
	IRowset* /*pRowset*/, HROW /*hRow*/, ULONG /*cColumns*/, ULONG /*rgColumns*/[],
	DBREASON /*eReason*/, DBEVENTPHASE ePhase, BOOL /*fCantDeny*/)
{
	METHOD_PROLOGUE_EX(COleControlSite, RowsetNotify)

	DSCSTATE nState = dscNoState;
	switch(ePhase)
	{
	case DBEVENTPHASE_OKTODO:
		nState = dscOKToDo;
		break;
	case DBEVENTPHASE_ABOUTTODO:
		nState = dscAboutToDo;
		break;
	case DBEVENTPHASE_SYNCHAFTER:
		nState = dscSyncAfter;
		break;
	case DBEVENTPHASE_FAILEDTODO:
		nState = dscFailedToDo;
		break;
	case DBEVENTPHASE_DIDEVENT:
		nState = dscDidEvent;
		break;
	}
	if(nState == dscDidEvent)
	{
		CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
		if (pDSC == NULL)
			return S_OK;

		if(pDSC->m_bUpdateInProgress == FALSE)
		{
			ASSERT(pDSC->m_pRowset);

			BOOL bUpdateInProgress = pDSC->m_bUpdateInProgress;
			pDSC->m_bUpdateInProgress = TRUE;
			pDSC->GetBoundClientRow();
			pDSC->m_bUpdateInProgress = bUpdateInProgress;
			pDSC->UpdateControls();
		}
	}

	AFX_EVENT event(AFX_EVENT::propDSCNotify);
	event.m_nDSCState = nState;
	event.m_nDSCReason = dscModify;
	pThis->OnEvent(&event);
	return event.m_hResult;
}

STDMETHODIMP COleControlSite::XRowsetNotify::OnRowChange(
	IRowset* /*pRowset*/, ULONG cRows, const HROW rghRows[],
	DBREASON eReason, DBEVENTPHASE ePhase, BOOL /*fCantDeny*/)
{
	METHOD_PROLOGUE_EX(COleControlSite, RowsetNotify)

	HRESULT hRes = S_OK;

	DSCSTATE nState = dscNoState;
	switch(ePhase)
	{
	case DBEVENTPHASE_OKTODO:
		nState = dscOKToDo;
		break;
	case DBEVENTPHASE_ABOUTTODO:
		nState = dscAboutToDo;
		break;
	case DBEVENTPHASE_SYNCHAFTER:
		nState = dscSyncAfter;
		break;
	case DBEVENTPHASE_FAILEDTODO:
		nState = dscFailedToDo;
		break;
	case DBEVENTPHASE_DIDEVENT:
		nState = dscDidEvent;
		break;
	}
	DSCREASON nReason = dscNoReason;
	switch(eReason)
	{
	case DBREASON_ROW_ACTIVATE:
		nReason = dscMove;
		break;
	case DBREASON_ROW_DELETE:
		nReason = dscDelete;
		break;
	case DBREASON_ROW_INSERT:
		nReason = dscInsert;
		break;
	case DBREASON_ROW_UPDATE:
		nReason = dscCommit;
		break;
	}
	CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
	if (pDSC == NULL)
		return S_OK;

	ASSERT(pDSC->m_pRowset);

	if (nReason == dscCommit && nState == dscAboutToDo)
	{
		pDSC->UpdateCursor();
	}
	else if ((nReason == dscMove && cRows == 1) || (nState == dscSyncAfter && nReason == dscInsert))
	{
		pDSC->UpdateCursor();
		pDSC->m_pRowset->m_hRow = rghRows[0];

		BOOL bUpdateInProgress = pDSC->m_bUpdateInProgress;
		pDSC->m_bUpdateInProgress = TRUE;
		pDSC->GetBoundClientRow();
		pDSC->m_bUpdateInProgress = bUpdateInProgress;
		if(!pDSC->m_bUpdateInProgress)
			pDSC->UpdateControls();
	}
	if (nReason != dscNoReason)
	{
		AFX_EVENT event(AFX_EVENT::propDSCNotify);
		event.m_nDSCState = nState;
		event.m_nDSCReason = nReason;
		pThis->OnEvent(&event);
		hRes = event.m_hResult;
		if(FAILED(hRes))
			return hRes;
	}
	return S_OK;
}

STDMETHODIMP COleControlSite::XRowsetNotify::OnRowsetChange(
	IRowset* /*pRowset*/, DBREASON eReason, DBEVENTPHASE /*ePhase*/, BOOL /*fCantDeny*/)
{
	METHOD_PROLOGUE_EX(COleControlSite, RowsetNotify)

	if(eReason == DBREASON_ROWSET_CHANGED)
	{
		CDataSourceControl* pDSC = pThis->m_pDataSourceControl;
		if (pDSC == NULL)
			return S_OK;

		ASSERT(pDSC->m_pRowset);
		ASSERT(pDSC->m_pDynamicAccessor);

		pDSC->m_pDynamicAccessor->ReleaseAccessors(pDSC->m_pRowset->m_spRowset);
		pDSC->m_pDynamicAccessor->Close();
		pDSC->m_pDynamicAccessor->BindColumns(pDSC->m_pRowset->m_spRowset);
		if( pDSC->m_nColumns != (int)pDSC->m_pDynamicAccessor->GetColumnCount() )
		{
			pDSC->m_nColumns = pDSC->m_pDynamicAccessor->GetColumnCount();
			::CoTaskMemFree(pDSC->m_pMetaRowData);
			pDSC->m_pMetaRowData = (CDataSourceControl::METAROWTYPE*)::CoTaskMemAlloc(sizeof(CDataSourceControl::METAROWTYPE) * pDSC->m_nColumns);
			ASSERT(pDSC->m_pMetaRowData);
			memset(pDSC->m_pMetaRowData, 0, sizeof(CDataSourceControl::METAROWTYPE) * pDSC->m_nColumns);
		}
	}

	return S_OK;
}
