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

#ifdef AFX_OLE4_SEG
#pragma code_seg(AFX_OLE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

//////////////////////////////////////////////////////////////////////////////
// COleServerItem implementation

COleServerItem::COleServerItem(COleServerDoc* pServerDoc, BOOL bAutoDelete)
{
	if (pServerDoc != NULL)
		ASSERT_VALID(pServerDoc);

	m_dwRef = 0;    // always start in disconnected state
	m_bAutoDelete = bAutoDelete;
	m_bNeedUnlock = FALSE;

	// initially, item does not have an extent
	m_sizeExtent.cx = 0;
	m_sizeExtent.cy = 0;

	// initialize advise holders
	m_lpOleAdviseHolder = NULL;
	m_lpDataAdviseHolder = NULL;

	// add presentation formats to the data source
	m_dataSource.m_nGrowBy = 1;
	FORMATETC formatEtc;
	formatEtc.ptd = NULL;
	formatEtc.dwAspect = DVASPECT_CONTENT;
	formatEtc.lindex = -1;

	// by default, a COleServerItem supports CF_METAFILEPICT
	formatEtc.cfFormat = CF_METAFILEPICT;
	formatEtc.tymed = TYMED_MFPICT;
	m_dataSource.DelayRenderData(0, &formatEtc);

	// add item to server document
	m_pDocument = NULL;
	if (pServerDoc != NULL)
		pServerDoc->AddItem(this);
	ASSERT(m_pDocument == pServerDoc);

	AfxOleLockApp();
}

COleServerItem::~COleServerItem()
{
	m_bAutoDelete = FALSE;  // no delete during destructor

	// release any advise holders
	RELEASE(m_lpOleAdviseHolder);
	RELEASE(m_lpDataAdviseHolder);

	ExternalDisconnect();

	// disconnect from the document
	COleServerDoc* pDoc = GetDocument();
	if (pDoc != NULL)
	{
		// remove external lock from it
		if (m_bNeedUnlock)
		{
			pDoc->LockExternal(FALSE, TRUE);
			m_bNeedUnlock = FALSE;
		}

		// reset m_pEmbeddedItem if destroying embedded item
		if (pDoc->m_pEmbeddedItem == this)
			pDoc->m_pEmbeddedItem = NULL;

		// remove from list
		pDoc->RemoveItem(this);
	}

	// cleanup any references
	AfxOleUnlockApp();
}

BOOL COleServerItem::IsBlank() const
{
	// server items are blank in order to keep them from serializing when
	//  COleDocument::Serialize is called.

	return TRUE;
}

BOOL COleServerItem::IsConnected() const
{
	// if item is connected in any way, return TRUE
	if (m_dwRef != 0)
		return TRUE;

	// otherwise check if embedded item and document is connected
	if (!IsLinkedItem() && GetDocument()->m_lpClientSite != NULL)
		return TRUE;

	return FALSE;   // not connected
}

void COleServerItem::NotifyClient(OLE_NOTIFICATION nCode, DWORD dwParam)
{
	switch (nCode)
	{
	// IDataObject notifications
	case OLE_CHANGED:
		if (m_lpDataAdviseHolder != NULL)
			m_lpDataAdviseHolder->SendOnDataChange(GetDataObject(), dwParam, 0);
		break;

	// IOleObject notifications
	case OLE_SAVED:
		if (m_lpOleAdviseHolder != NULL)
			m_lpOleAdviseHolder->SendOnSave();
		break;
	case OLE_CLOSED:
		if (m_lpOleAdviseHolder != NULL)
			m_lpOleAdviseHolder->SendOnClose();
		break;
	case OLE_RENAMED:
		if (m_lpOleAdviseHolder != NULL)
		{
			// Note: the moniker should already be updated for this to work
			LPMONIKER lpMoniker = (LPMONIKER)dwParam;
			m_lpOleAdviseHolder->SendOnRename(lpMoniker);
		}
		break;

	default:
		ASSERT(FALSE);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Helpers for getting commonly used interfaces through interface map

LPDATAOBJECT COleServerItem::GetDataObject()
{
	LPDATAOBJECT lpDataObject =
		(LPDATAOBJECT)GetInterface(&IID_IDataObject);
	ASSERT(lpDataObject != NULL);
	return lpDataObject;
}

LPOLEOBJECT COleServerItem::GetOleObject()
{
	LPOLEOBJECT lpOleObject =
		(LPOLEOBJECT)GetInterface(&IID_IOleObject);
	ASSERT(lpOleObject != NULL);
	return lpOleObject;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerItem overrides

BOOL COleServerItem::OnQueryUpdateItems()
{
	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// update all of the embedded objects
	POSITION pos = pDoc->GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = pDoc->GetNextClientItem(pos)) != NULL)
	{
		// if any item is out-of-date, then this item is out-of-date
		if (pItem->m_lpObject->IsUpToDate() != NULL)
			return TRUE;    // update needed
	}
	return FALSE;   // update not needed
}

void COleServerItem::OnUpdateItems()
{
	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// update all of the embedded objects
	POSITION pos = pDoc->GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = pDoc->GetNextClientItem(pos)) != NULL)
	{
		// update any out-of-date item
		if (pItem->m_lpObject->IsUpToDate() != NULL)
			pItem->m_lpObject->Update();
	}
}

BOOL COleServerItem::OnSetExtent(DVASPECT dwDrawAspect, const CSize& size)
{
	ASSERT_VALID(this);

	if (dwDrawAspect == DVASPECT_CONTENT)
	{
		m_sizeExtent = size;    // simply remember the extent
		return TRUE;
	}
	return FALSE;   // not implemented for that dwDrawAspect
}

BOOL COleServerItem::OnGetExtent(DVASPECT /*dwDrawAspect*/, CSize& rSize)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(&rSize, sizeof(CSize)));

	// the default implementation doesn't know what the extent is

	rSize.cx = 0;
	rSize.cy = 0;

	return FALSE;
}

void COleServerItem::OnDoVerb(LONG iVerb)
{
	switch (iVerb)
	{
	// open - maps to OnOpen
	case OLEIVERB_OPEN:
	case -OLEIVERB_OPEN-1:  // allows positive OLEIVERB_OPEN-1 in registry
		OnOpen();
		break;

	// primary, show, and unknown map to OnShow
	case OLEIVERB_PRIMARY:  // OLEIVERB_PRIMARY is 0 and "Edit" in registry
	case OLEIVERB_SHOW:
		OnShow();
		break;

	// hide maps to OnHide
	case OLEIVERB_HIDE:
	case -OLEIVERB_HIDE-1:  // allows positive OLEIVERB_HIDE-1 in registry
		OnHide();
		break;

	default:
		// negative verbs not understood should return E_NOTIMPL
		if (iVerb < 0)
			AfxThrowOleException(E_NOTIMPL);

		// positive verb not processed --
		//  according to OLE spec, primary verb should be executed
		//  instead.
		OnDoVerb(OLEIVERB_PRIMARY);

		// also, OLEOBJ_S_INVALIDVERB should be returned.
		AfxThrowOleException(OLEOBJ_S_INVALIDVERB);
	}
}

BOOL COleServerItem::OnDrawEx(CDC* pDC, DVASPECT nDrawAspect, CSize& rSize)
{
	ASSERT_VALID(pDC);
	ASSERT(AfxIsValidAddress(&rSize, sizeof(CSize)));

	if (nDrawAspect != DVASPECT_CONTENT)
		return FALSE;

	return OnDraw(pDC, rSize);
}

void COleServerItem::OnShow()
{
	ASSERT_VALID(this);

	// attempt in place activation (if not supported, fall back on "Open")
	COleServerDoc* pDoc = GetDocument();
	if (!pDoc->ActivateInPlace())
	{
		// by default OnShow() maps to OnOpen() if in-place activation
		//  not supported
		OnOpen();
	}
}

void COleServerItem::OnOpen()
{
	ASSERT_VALID(this);

	// default implementation shows the document
	COleServerDoc* pDoc = GetDocument();
	ASSERT(pDoc != NULL);
	pDoc->OnShowDocument(TRUE);
}

void COleServerItem::OnHide()
{
	ASSERT_VALID(this);

	// default implementation hides the document
	COleServerDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	pDoc->OnShowDocument(FALSE);
}

BOOL COleServerItem::GetMetafileData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
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
	HDC hAttribDC = _AfxOleCreateDC(lpFormatEtc->ptd);
	if (hAttribDC == NULL)
		return FALSE;
	dc.SetAttribDC(hAttribDC);

	// Paint directly into the metafile.
	CSize size(0, 0);
	BOOL bResult = OnDrawEx(&dc, (DVASPECT)lpFormatEtc->dwAspect, size);

	// attribute DC is no longer necessary
	dc.SetAttribDC(NULL);
	::DeleteDC(hAttribDC);

	if (!bResult)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceOle)
			TRACE0("calling COleServerItem::OnDrawEx()failed.\n");
#endif
		return FALSE;
	}

	HMETAFILE hMF = dc.Close();
	if (hMF == NULL)
		return FALSE;

	HGLOBAL hPict;
	if ((hPict =
		::GlobalAlloc(GMEM_SHARE|GMEM_MOVEABLE, sizeof(METAFILEPICT))) == NULL)
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
	if (size.cx == 0 && size.cy == 0 &&
		!OnGetExtent((DVASPECT)lpFormatEtc->dwAspect, size))
	{
		TRACE0("Warning: OnGetExtent failed during OnDrawEx --\n");
		TRACE0("\tpresentation metafile may be badly formed!\n");
	}
	lpPict->xExt = size.cx;
	lpPict->yExt = size.cy;  // HIMETRIC height
	if (lpPict->yExt < 0)
	{
		TRACE0("Warning: HIMETRIC natural size is negative.\n");
		lpPict->yExt = -lpPict->yExt;   // backward compatibility fix
	}

#ifdef _DEBUG
	if (lpPict->xExt == 0 || lpPict->yExt == 0)
	{
		// usually the natural extent is set to something interesting
		TRACE0("Warning: COleServerItem has no natural size --\n");
		TRACE0("\twill not work with some apps like MS Write.\n");
	}
#endif

	// return the medium with the hGlobal to the METAFILEPICT
	::GlobalUnlock(hPict);
	lpStgMedium->hGlobal = hPict;
	lpStgMedium->tymed = TYMED_MFPICT;
	return TRUE;
}

BOOL COleServerItem::OnSetColorScheme(const LOGPALETTE* /*lpLogPalette*/)
{
	ASSERT_VALID(this);

	return FALSE;   // default does nothing
}

BOOL COleServerItem::OnInitFromData(
	COleDataObject* /*pDataObject*/, BOOL /*bCreation*/)
{
	ASSERT_VALID(this);

	AfxThrowOleException(E_NOTIMPL);
	return FALSE;
}

void COleServerItem::CopyToClipboard(BOOL bIncludeLink)
{
	ASSERT_VALID(this);

	COleDataSource* pDataSource = OnGetClipboardData(bIncludeLink, NULL, NULL);

	// put it on the clipboard
	pDataSource->SetClipboard();
}

COleDataSource* COleServerItem::OnGetClipboardData(BOOL bIncludeLink,
	LPPOINT lpOffset, LPSIZE lpSize)
{
	ASSERT_VALID(this);

	COleDataSource* pDataSource = new COleDataSource;
	TRY
	{
		GetClipboardData(pDataSource, bIncludeLink, lpOffset, lpSize);
	}
	CATCH_ALL(e)
	{
		delete pDataSource;
		THROW_LAST();
	}
	END_CATCH_ALL

	ASSERT_VALID(pDataSource);
	return pDataSource;
}

DROPEFFECT COleServerItem::DoDragDrop(LPCRECT lpItemRect, CPoint ptOffset,
	BOOL bIncludeLink, DWORD dwEffects, LPCRECT lpRectStartDrag)
{
	ASSERT(AfxIsValidAddress(lpItemRect, sizeof(RECT)));
	ASSERT_VALID(this);

	ASSERT_VALID(this);

	DROPEFFECT dropEffect = DROPEFFECT_NONE;
	COleDataSource *pDataSource = NULL;
	TRY
	{
		// get clipboard data for this item
		CSize sizeItem(
			lpItemRect->right - lpItemRect->left,
			lpItemRect->bottom - lpItemRect->top);
		pDataSource = OnGetClipboardData(bIncludeLink, &ptOffset, &sizeItem);

		// add DROPEFFECT_LINK if link source is available
		LPDATAOBJECT lpDataObject = (LPDATAOBJECT)
			pDataSource->GetInterface(&IID_IDataObject);
		ASSERT(lpDataObject != NULL);
		FORMATETC formatEtc;
		formatEtc.cfFormat = (CLIPFORMAT)_oleData.cfLinkSource;
		formatEtc.ptd = NULL;
		formatEtc.dwAspect = DVASPECT_CONTENT;
		formatEtc.lindex = -1;
		formatEtc.tymed = (DWORD) -1;
		if (lpDataObject->QueryGetData(&formatEtc) == S_OK)
			dwEffects |= DROPEFFECT_LINK;

		// calculate default sensitivity rectangle
		CRect rectDrag;
		if (lpRectStartDrag == NULL)
		{
			rectDrag.SetRect(lpItemRect->left, lpItemRect->top, lpItemRect->left,
				lpItemRect->top);
			lpRectStartDrag = &rectDrag;
		}

		// do drag drop operation
		dropEffect = pDataSource->DoDragDrop(dwEffects, lpRectStartDrag);
		pDataSource->InternalRelease();
	}
	CATCH_ALL(e)
	{
		if (pDataSource != NULL)
			pDataSource->InternalRelease();

		THROW_LAST();
	}
	END_CATCH_ALL

	return dropEffect;
}

void COleServerItem::GetClipboardData(COleDataSource* pDataSource,
	BOOL bIncludeLink, LPPOINT lpOffset, LPSIZE lpSize)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDataSource);
	ASSERT(lpOffset == NULL ||
		AfxIsValidAddress(lpOffset, sizeof(POINT), FALSE));

	// add CF_EMBEDDEDOBJECT by creating memory storage copy of the object
	STGMEDIUM stgMedium;
	GetEmbedSourceData(&stgMedium);
	pDataSource->CacheData((CLIPFORMAT)_oleData.cfEmbedSource, &stgMedium);

	// add CF_OBJECTDESCRIPTOR
	GetObjectDescriptorData(lpOffset, lpSize, &stgMedium);
	pDataSource->CacheData((CLIPFORMAT)_oleData.cfObjectDescriptor,
		&stgMedium);

	// add any presentation entries/conversion formats that the item
	//  can produce.
	AddOtherClipboardData(pDataSource);

	// add CF_LINKSOURCE if supporting links to pseudo objects
	if (bIncludeLink && GetLinkSourceData(&stgMedium))
	{
		pDataSource->CacheData((CLIPFORMAT)_oleData.cfLinkSource, &stgMedium);

		// add CF_LINKSOURCEDESCRIPTOR
		GetObjectDescriptorData(lpOffset, lpSize, &stgMedium);
		pDataSource->CacheData((CLIPFORMAT)_oleData.cfLinkSourceDescriptor,
			&stgMedium);
	}
}

void COleServerItem::GetEmbedSourceData(LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	LPLOCKBYTES lpLockBytes;
	SCODE sc = ::CreateILockBytesOnHGlobal(NULL, TRUE, &lpLockBytes);
	if (sc != S_OK)
		AfxThrowOleException(sc);
	ASSERT(lpLockBytes != NULL);

	LPSTORAGE lpStorage;
	sc = ::StgCreateDocfileOnILockBytes(lpLockBytes,
		STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0, &lpStorage);
	if (sc != S_OK)
	{
		VERIFY(lpLockBytes->Release() == 0);
		AfxThrowOleException(sc);
	}
	ASSERT(lpStorage != NULL);

	// setup for save copy as
	COleServerDoc* pDoc = GetDocument();
	pDoc->m_bSameAsLoad = FALSE;
	pDoc->m_bRemember = FALSE;

	TRY
	{
		OnSaveEmbedding(lpStorage);
		pDoc->CommitItems(FALSE);
	}
	CATCH_ALL(e)
	{
		// release storage and lock bytes
		VERIFY(lpStorage->Release() == 0);
		VERIFY(lpLockBytes->Release() == 0);
		pDoc->m_bSameAsLoad = TRUE;
		pDoc->m_bRemember = TRUE;
		THROW_LAST();
	}
	END_CATCH_ALL

	pDoc->m_bSameAsLoad = TRUE;
	pDoc->m_bRemember = TRUE;
	lpLockBytes->Release();

	// add it to the data source
	lpStgMedium->tymed = TYMED_ISTORAGE;
	lpStgMedium->pstg = lpStorage;
	lpStgMedium->pUnkForRelease = NULL;
}

void COleServerItem::AddOtherClipboardData(COleDataSource* pDataSource)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDataSource);

	// get IEnumFORMATETC interface for the IDataObject
	LPDATAOBJECT lpDataObject = GetDataObject();
	LPENUMFORMATETC lpEnumFORMATETC;
	if (lpDataObject->EnumFormatEtc(DATADIR_GET, &lpEnumFORMATETC) != S_OK)
		return;
	ASSERT(lpEnumFORMATETC != NULL);

	// get all formats that the object will give us
	FORMATETC formatEtc;
	while (lpEnumFORMATETC->Next(1, &formatEtc, NULL) == S_OK)
	{
		STGMEDIUM stgMedium;
		if (lpDataObject->GetData(&formatEtc, &stgMedium) != S_OK)
		{
			// data is not available
			CoTaskMemFree(formatEtc.ptd);
		}
		else if (stgMedium.pUnkForRelease != NULL)
		{
			// don't cache data with pUnkForRelease != NULL
			::ReleaseStgMedium(&stgMedium);
			CoTaskMemFree(formatEtc.ptd);
		}
		else
		{
			// cache the data (now we own the stgMedium)
			pDataSource->CacheData(0, &stgMedium, &formatEtc);
		}
	}

	// cleanup
	lpEnumFORMATETC->Release();
}

LPMONIKER COleServerItem::GetMoniker(OLEGETMONIKER nAssign)
{
	// get IOleObject interface for this item
	LPOLEOBJECT lpOleObject = GetOleObject();
	ASSERT(lpOleObject != NULL);

	// get moniker from OLE object
	LPMONIKER lpMoniker = NULL;
	lpOleObject->GetMoniker(nAssign, OLEWHICHMK_OBJFULL, &lpMoniker);
	return lpMoniker;
}

BOOL COleServerItem::GetLinkSourceData(LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	LPOLEOBJECT lpOleObject = GetOleObject();
	ASSERT(lpOleObject != NULL);

	// get moniker from ole object
	LPMONIKER lpMoniker;
	SCODE sc = lpOleObject->GetMoniker(OLEGETMONIKER_TEMPFORUSER,
		OLEWHICHMK_OBJFULL, &lpMoniker);
	if (sc != S_OK)
	{
		TRACE0("Warning: unable to get moniker for object.\n");
		return FALSE;
	}
	ASSERT(lpMoniker != NULL);

	// create a memory based stream to write the moniker to
	LPSTREAM lpStream;
	if (::CreateStreamOnHGlobal(NULL, TRUE, &lpStream) != S_OK)
	{
		lpMoniker->Release();
		AfxThrowMemoryException();
	}
	ASSERT(lpStream != NULL);

	// write the moniker to the stream, and add it to the clipboard
	sc = ::OleSaveToStream(lpMoniker, lpStream);
	lpMoniker->Release();
	if (sc != S_OK)
	{
		lpStream->Release();
		AfxThrowOleException(sc);
	}

	// write the class ID of the document to the stream as well
	COleLinkingDoc* pDoc = GetDocument();
	ASSERT(pDoc->m_pFactory != NULL);
	sc = WriteClassStm(lpStream, pDoc->m_pFactory->GetClassID());
	if (sc != S_OK)
	{
		lpStream->Release();
		AfxThrowOleException(sc);
	}

	// setup the STGMEDIUM
	lpStgMedium->tymed = TYMED_ISTREAM;
	lpStgMedium->pstm = lpStream;
	lpStgMedium->pUnkForRelease = NULL;
	return TRUE;
}

void COleServerItem::GetObjectDescriptorData(
	LPPOINT lpOffset, LPSIZE lpSize, LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));
	ASSERT(lpOffset == NULL ||
		AfxIsValidAddress(lpOffset, sizeof(POINT), FALSE));

	LPOLEOBJECT lpOleObject = GetOleObject();
	ASSERT(lpOleObject != NULL);

	// get the object descriptor for the IOleObject
	POINTL pointl = { 0, 0 };
	if (lpOffset != NULL)
	{
		CSize ptOffset(lpOffset->x, lpOffset->y);
		((CDC*)NULL)->DPtoHIMETRIC(&ptOffset);
		pointl.x = ptOffset.cx;
		pointl.y = ptOffset.cy;
	}
	SIZEL sizel;
	if (lpSize != NULL)
	{
		sizel.cx = lpSize->cx;
		sizel.cy = lpSize->cy;
		((CDC*)NULL)->DPtoHIMETRIC(&sizel);
	}
	else
	{
		sizel.cx = 0;
		sizel.cy = 0;
	}

	InterlockedIncrement(&m_dwRef);  // protect against destruction during this call
	HGLOBAL hGlobal = _AfxOleGetObjectDescriptorData(
		lpOleObject, NULL, DVASPECT_CONTENT, pointl, &sizel);
	InterlockedDecrement(&m_dwRef);

	if (hGlobal == NULL)
		AfxThrowMemoryException();

	// setup the STGMEDIUM
	lpStgMedium->tymed = TYMED_HGLOBAL;
	lpStgMedium->hGlobal = hGlobal;
	lpStgMedium->pUnkForRelease = NULL;
}

void COleServerItem::OnSaveEmbedding(LPSTORAGE lpStorage)
{
	ASSERT(lpStorage != NULL);

	// always (logically) a "File.Save Copy As" operation
	COleServerDoc* pDoc = GetDocument();
	LPSTORAGE lpOrigStg = pDoc->m_lpRootStg;
	pDoc->m_lpRootStg = lpStorage;

	TRY
	{
		ASSERT(pDoc->m_lpRootStg != NULL);
		pDoc->SaveToStorage(this);  // use helper to serialize to storage
	}
	CATCH_ALL(e)
	{
		// save as failed: re-attach original storage
		pDoc->m_lpRootStg = lpOrigStg;
		THROW_LAST();
	}
	END_CATCH_ALL

	// re-attach original storage
	pDoc->m_lpRootStg = lpOrigStg;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerItem data-object callback default implementation

BOOL COleServerItem::OnRenderGlobalData(
	LPFORMATETC /*lpFormatEtc*/, HGLOBAL* /*phGlobal*/)
{
	ASSERT_VALID(this);

	return FALSE;   // default does nothing
}

BOOL COleServerItem::OnRenderFileData(
	LPFORMATETC /*lpFormatEtc*/, CFile* /*pFile*/)
{
	ASSERT_VALID(this);

	return FALSE;   // default does nothing
}

BOOL COleServerItem::OnRenderData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	// default implementation does not support extended layout
	if (lpFormatEtc->lindex != -1)
		return FALSE;

	// default implementation supports both types of metafiles
	if (lpFormatEtc->cfFormat == CF_METAFILEPICT)
		return GetMetafileData(lpFormatEtc, lpStgMedium);

	return FALSE;   // cfFormat not supported
}

BOOL COleServerItem::OnSetData(
	LPFORMATETC /*lpFormatEtc*/, LPSTGMEDIUM /*lpStgMedium*/, BOOL /*bRelease*/)
{
	ASSERT_VALID(this);

	return FALSE;   // default does nothing
}

/////////////////////////////////////////////////////////////////////////////
// COleServerItem OLE interface implementation

BEGIN_INTERFACE_MAP(COleServerItem, CDocItem)
	INTERFACE_PART(COleServerItem, IID_IOleObject, OleObject)
	INTERFACE_PART(COleServerItem, IID_IDataObject, DataObject)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleServerItem::XOleObject

STDMETHODIMP_(ULONG) COleServerItem::XOleObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerItem::XOleObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerItem::XOleObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

// COleServerItem has special Release semantics.  In particular, the item
//  is only deleted from memory if m_bAutoDelete is TRUE.
//  Also, it unlocks the document if the reference count reaches zero.

void COleServerItem::OnFinalRelease()
{
	ASSERT_VALID(this);

	COleServerDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	pDoc->InternalAddRef(); // make document stable

	// if connected to a document -- remove external lock from it
	if (m_bNeedUnlock)
	{
		pDoc->LockExternal(FALSE, TRUE);
		m_bNeedUnlock = FALSE;
	}

	// delete this item if no longer needed
	if (m_bAutoDelete)
		delete this;

	// release artificial reference (may destroy the document)
	pDoc->InternalRelease();
}

STDMETHODIMP COleServerItem::XOleObject::SetClientSite(
	LPOLECLIENTSITE /*pClientSite*/)
{
	// linked objects do not support SetClientSite

	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::GetClientSite(
	LPOLECLIENTSITE* ppClientSite)
{
	// linked objects do not support GetClientSite

	*ppClientSite = NULL;
	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::SetHostNames(
	LPCOLESTR /*szContainerApp*/, LPCOLESTR /*szContainerObj*/)
{
	// linked objects do not support SetHostNames

	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::Close(DWORD /*dwSaveOption*/)
{
	// linked objects do not support close

	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::SetMoniker(
	DWORD /*dwWhichMoniker*/, LPMONIKER /*pmk*/)
{
	// linked objects do not support SetMoniker

	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::GetMoniker(
	DWORD dwAssign, DWORD dwWhichMoniker, LPMONIKER* ppMoniker)
{
	USES_CONVERSION;

	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	COleServerDoc* pDoc = pThis->GetDocument();
	ASSERT_VALID(pDoc);
	ASSERT_KINDOF(COleServerDoc, pDoc);
	ASSERT(ppMoniker != NULL);
	*ppMoniker = NULL;

	switch (dwWhichMoniker)
	{
	case OLEWHICHMK_CONTAINER:
		// simply return the moniker of the container document
		*ppMoniker = pDoc->GetMoniker((OLEGETMONIKER)dwAssign);
		break;

	case OLEWHICHMK_OBJREL:
		{
			// no relative moniker if no item name
			if (pThis->m_strItemName.IsEmpty())
				break;

			// don't return relative moniker if no document moniker
			LPMONIKER lpMoniker = pDoc->GetMoniker((OLEGETMONIKER)dwAssign);
			if (lpMoniker == NULL)
				break;
			lpMoniker->Release();   // don't need document moniker

			// relative monikers have to handle assignment correctly
			switch (dwAssign)
			{
			case OLEGETMONIKER_TEMPFORUSER:
			case OLEGETMONIKER_ONLYIFTHERE:
			case OLEGETMONIKER_FORCEASSIGN:
				// create item moniker from name
				CreateItemMoniker(OLESTDDELIMOLE, T2COLE(pThis->m_strItemName),
					ppMoniker);
				break;

			case OLEGETMONIKER_UNASSIGN:
				ASSERT(FALSE);  // should never get UNASSIGN
				break;
			}
		}
		break;

	case OLEWHICHMK_OBJFULL:
		{
			// get each sub-moniker: item & document
			LPMONIKER lpMoniker1, lpMoniker2;
			GetMoniker(dwAssign, OLEWHICHMK_CONTAINER, &lpMoniker1);
			GetMoniker(dwAssign, OLEWHICHMK_OBJREL, &lpMoniker2);

			if (lpMoniker1 != NULL && lpMoniker2 != NULL)
			{
				// create composite from two parts
				::CreateGenericComposite(lpMoniker1, lpMoniker2, ppMoniker);
			}
			else if (lpMoniker1 != NULL)
			{
				// just use container moniker
				*ppMoniker = lpMoniker1;
				lpMoniker1 = NULL;
			}

			// release sub-monikers
			RELEASE(lpMoniker1);
			RELEASE(lpMoniker2);
		}
		break;
	}

	return *ppMoniker == NULL ? E_FAIL : S_OK;
}

STDMETHODIMP COleServerItem::XOleObject::InitFromData(
	LPDATAOBJECT /*pDataObject*/, BOOL /*fCreation*/, DWORD /*dwReserved*/)
{
	// linked objects do not support InitFromData

	return E_NOTIMPL;
}

STDMETHODIMP COleServerItem::XOleObject::GetClipboardData(
	DWORD /*dwReserved*/, LPDATAOBJECT* ppDataObject)
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	*ppDataObject = NULL;

	SCODE sc;
	TRY
	{
		COleDataSource* pDataSource = pThis->OnGetClipboardData(TRUE, NULL, NULL);
		ASSERT(pDataSource != NULL);

		*ppDataObject =
			(LPDATAOBJECT)pDataSource->GetInterface(&IID_IDataObject);
		ASSERT(*ppDataObject != NULL);
		sc = S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerItem::XOleObject::DoVerb(
	LONG iVerb, LPMSG /*lpmsg*/, LPOLECLIENTSITE /*pActiveSite*/, LONG /*lindex*/,
	HWND /*hwndParent*/, LPCRECT /*lpPosRect*/)
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	SCODE sc;
	TRY
	{
		pThis->OnDoVerb(iVerb);
		sc = S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	pThis->InternalRelease();   // may 'delete this'

	return sc;
}

STDMETHODIMP COleServerItem::XOleObject::EnumVerbs(
	IEnumOLEVERB** ppenumOleVerb)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	*ppenumOleVerb = NULL;

	CLSID clsid;
	pThis->GetOleObject()->GetUserClassID(&clsid);
	return OleRegEnumVerbs(clsid, ppenumOleVerb);
}

STDMETHODIMP COleServerItem::XOleObject::Update()
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	SCODE sc;
	TRY
	{
		pThis->OnUpdateItems();
		sc = S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerItem::XOleObject::IsUpToDate()
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	SCODE sc;
	TRY
	{
		sc = pThis->OnQueryUpdateItems() ? S_FALSE : S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerItem::XOleObject::GetUserClassID(CLSID* pClsid)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	COleServerDoc* pDoc = pThis->GetDocument();
	return pDoc->m_xPersistFile.GetClassID(pClsid);
}

STDMETHODIMP COleServerItem::XOleObject::GetUserType(
	DWORD dwFormOfType, LPOLESTR* ppszUserType)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	*ppszUserType = NULL;

	CLSID clsid;
	pThis->GetOleObject()->GetUserClassID(&clsid);
	return OleRegGetUserType(clsid, dwFormOfType, ppszUserType);
}

STDMETHODIMP COleServerItem::XOleObject::SetExtent(
	DWORD /*dwDrawAspect*/, LPSIZEL /*lpsizel*/)
{
	// linked objects do not support SetExtent

	return E_FAIL;
}

STDMETHODIMP COleServerItem::XOleObject::GetExtent(
	DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_INVALIDARG;
	TRY
	{
		// call to get regular windows size
		CSize size;
		if (pThis->OnGetExtent((DVASPECT)dwDrawAspect, size))
		{
			if (size.cy < 0)
				size.cy = -size.cy; // extents are always positive
			lpsizel->cx = size.cx;
			lpsizel->cy = size.cy;

			sc = S_OK;
		}
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerItem::XOleObject::Advise(
	IAdviseSink* pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	*pdwConnection = 0;

	if (pThis->m_lpOleAdviseHolder == NULL &&
		::CreateOleAdviseHolder(&pThis->m_lpOleAdviseHolder) != S_OK)
	{
		return E_OUTOFMEMORY;
	}
	ASSERT(pThis->m_lpOleAdviseHolder != NULL);
	return pThis->m_lpOleAdviseHolder->Advise(pAdvSink, pdwConnection);
}

STDMETHODIMP COleServerItem::XOleObject::Unadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	if (pThis->m_lpOleAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_lpOleAdviseHolder != NULL);
	return pThis->m_lpOleAdviseHolder->Unadvise(dwConnection);
}

STDMETHODIMP COleServerItem::XOleObject::EnumAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	*ppenumAdvise = NULL;

	if (pThis->m_lpOleAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_lpOleAdviseHolder != NULL);
	return pThis->m_lpOleAdviseHolder->EnumAdvise(ppenumAdvise);
}

STDMETHODIMP COleServerItem::XOleObject::GetMiscStatus(
	DWORD dwAspect, DWORD* pdwStatus)
{
	METHOD_PROLOGUE_EX_(COleServerItem, OleObject)

	*pdwStatus = 0;

	CLSID clsid;
	pThis->GetOleObject()->GetUserClassID(&clsid);
	return OleRegGetMiscStatus(clsid, dwAspect, pdwStatus);
}

STDMETHODIMP COleServerItem::XOleObject::SetColorScheme(LPLOGPALETTE lpLogpal)
{
	METHOD_PROLOGUE_EX(COleServerItem, OleObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_NOTIMPL;
	TRY
	{
		// delegate to embedded item
		if (pThis->OnSetColorScheme(lpLogpal))
			sc = S_OK;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerItem::XDataObject

STDMETHODIMP_(ULONG) COleServerItem::XDataObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerItem::XDataObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerItem::XDataObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerItem::XDataObject::GetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	return pThis->m_dataSource.m_xDataObject.GetData(lpFormatEtc, lpStgMedium);
}

STDMETHODIMP COleServerItem::XDataObject::GetDataHere(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	return pThis->m_dataSource.m_xDataObject.GetDataHere(
		lpFormatEtc, lpStgMedium);
}

STDMETHODIMP COleServerItem::XDataObject::QueryGetData(LPFORMATETC lpFormatEtc)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	return pThis->m_dataSource.m_xDataObject.QueryGetData(lpFormatEtc);
}

STDMETHODIMP COleServerItem::XDataObject::GetCanonicalFormatEtc(
	LPFORMATETC /*lpFormatEtcIn*/, LPFORMATETC /*lpFormatEtcOut*/)
{
	// because we support the target-device (ptd) for server metafile format,
	//  all members of the FORMATETC are significant.

	return DATA_S_SAMEFORMATETC;
}

STDMETHODIMP COleServerItem::XDataObject::SetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium, BOOL bRelease)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	return pThis->m_dataSource.m_xDataObject.SetData(
		lpFormatEtc, lpStgMedium, bRelease);
}

STDMETHODIMP COleServerItem::XDataObject::EnumFormatEtc(
	DWORD dwDirection, LPENUMFORMATETC* ppenumFormatEtc)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	return pThis->m_dataSource.m_xDataObject.EnumFormatEtc(
		dwDirection, ppenumFormatEtc);
}

STDMETHODIMP COleServerItem::XDataObject::DAdvise(
	FORMATETC* pFormatEtc, DWORD advf,
	LPADVISESINK pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	*pdwConnection = 0;

	// this special case is for apps like Excel which ask for DAdvise
	// on CF_METAFILEPICT, DVASPECT_ICON for insert as icon.
	FORMATETC formatEtc = *pFormatEtc;
	if (formatEtc.cfFormat == CF_METAFILEPICT &&
		formatEtc.dwAspect == DVASPECT_ICON)
	{
		formatEtc.dwAspect = DVASPECT_CONTENT;
	}

	// make sure the FORMATETC is valid
	if (!(pFormatEtc->cfFormat == 0 && pFormatEtc->ptd == NULL &&
		  pFormatEtc->dwAspect == -1 && pFormatEtc->lindex == -1 &&
		  pFormatEtc->tymed == -1) &&
		pThis->GetDataObject()->QueryGetData(&formatEtc) != S_OK)
	{
		// it is not a wildcard advise -and- the format is not acceptable
		return DATA_E_FORMATETC;
	}

	// create the advise holder, if necessary
	if (pThis->m_lpDataAdviseHolder == NULL &&
		CreateDataAdviseHolder(&pThis->m_lpDataAdviseHolder) != S_OK)
	{
		return E_OUTOFMEMORY;
	}
	ASSERT(pThis->m_lpDataAdviseHolder != NULL);
	return pThis->m_lpDataAdviseHolder->Advise(this, pFormatEtc, advf,
		pAdvSink, pdwConnection);
}

STDMETHODIMP COleServerItem::XDataObject::DUnadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	if (pThis->m_lpDataAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_lpDataAdviseHolder != NULL);
	return pThis->m_lpDataAdviseHolder->Unadvise(dwConnection);
}

STDMETHODIMP COleServerItem::XDataObject::EnumDAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX_(COleServerItem, DataObject)

	*ppenumAdvise = NULL;

	if (pThis->m_lpDataAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_lpDataAdviseHolder != NULL);
	return pThis->m_lpDataAdviseHolder->EnumAdvise(ppenumAdvise);
}

//////////////////////////////////////////////////////////////////////////////
// special CItemDataSource implementation

BOOL COleServerItem::CItemDataSource::OnRenderGlobalData(
	LPFORMATETC lpFormatEtc, HGLOBAL* phGlobal)
{
	ASSERT_VALID(this);
	COleServerItem* pItem = (COleServerItem*)
		((BYTE*)this - offsetof(COleServerItem, m_dataSource));

	return pItem->OnRenderGlobalData(lpFormatEtc, phGlobal);
		// Note: COleDataSource has no implementation
}

BOOL COleServerItem::CItemDataSource::OnRenderFileData(
	LPFORMATETC lpFormatEtc, CFile* pFile)
{
	ASSERT_VALID(this);
	COleServerItem* pItem = (COleServerItem*)
		((BYTE*)this - offsetof(COleServerItem, m_dataSource));

	return pItem->OnRenderFileData(lpFormatEtc, pFile);
		// Note: COleDataSource has no implementation
}

BOOL COleServerItem::CItemDataSource::OnRenderData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	COleServerItem* pItem = (COleServerItem*)
		((BYTE*)this - offsetof(COleServerItem, m_dataSource));

	if (pItem->OnRenderData(lpFormatEtc, lpStgMedium))
		return TRUE;

	return COleDataSource::OnRenderData(lpFormatEtc, lpStgMedium);
}

BOOL COleServerItem::CItemDataSource::OnSetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium, BOOL bRelease)
{
	ASSERT_VALID(this);
	COleServerItem* pItem = (COleServerItem*)
		((BYTE*)this - offsetof(COleServerItem, m_dataSource));

	return pItem->OnSetData(lpFormatEtc, lpStgMedium, bRelease);
		// Note: COleDataSource has no implementation
}

//////////////////////////////////////////////////////////////////////////////
// COleServerItem Diagnostics

#ifdef _DEBUG
void COleServerItem::AssertValid() const
{
	CDocItem::AssertValid();

	// must be attached to a document
	ASSERT(m_pDocument != NULL);
	m_dataSource.AssertValid();
}

void COleServerItem::Dump(CDumpContext& dc) const
{
	CDocItem::Dump(dc);

	dc << "m_bNeedUnlock = " << m_bNeedUnlock;
	dc << "\nm_bAutoDelete = " << m_bAutoDelete;
	dc << "\nm_strItemName = " << m_strItemName;
	dc << "\nm_lpOleAdviseHolder = " << m_lpOleAdviseHolder;
	dc << "\nm_lpDataAdviseHolder = " << m_lpDataAdviseHolder;
	dc << "\nwith m_dataSource: " << &m_dataSource;
}
#endif

/////////////////////////////////////////////////////////////////////////////
