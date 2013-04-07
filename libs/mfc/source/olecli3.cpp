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

#ifdef AFX_OLE_SEG
#pragma code_seg(AFX_OLE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleClientItem activation

// helper to get client site -- this is called from a number of places
LPOLECLIENTSITE COleClientItem::GetClientSite()
{
	ASSERT_VALID(this);

	LPOLECLIENTSITE lpClientSite =
		(LPOLECLIENTSITE)GetInterface(&IID_IOleClientSite);
	ASSERT(lpClientSite != NULL);
	return lpClientSite;
}

void COleClientItem::Activate(LONG nVerb, CView* pView, LPMSG lpMsg)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	if (pView != NULL)
		ASSERT_VALID(pView);
	if (lpMsg != NULL)
		ASSERT(AfxIsValidAddress(lpMsg, sizeof(MSG), FALSE));

	// store the container HWND for in place activation then do the verb
	if (m_pView == NULL)
		m_pView = pView;

	_AFX_OLE_STATE* pOleState = _afxOleState;
	CView* pViewSave = pOleState->m_pActivateView;
	pOleState->m_pActivateView = NULL;

	// get item rectangle for in-place players
	//  (that may not support in-place activation)
	LPCRECT lpPosRect = NULL;
	CRect rectPos;
	if (pView != NULL)
	{
		ASSERT_VALID(pView);
		rectPos.SetRectEmpty();
		OnGetItemPosition(rectPos);
		if (!rectPos.IsRectEmpty())
		{
			lpPosRect = &rectPos;
			pOleState->m_pActivateView = pView;
		}
	}

	// prepare DoVerb parameters and call into the server
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	HWND hWnd = pView->GetSafeHwnd();
	SCODE sc = m_lpObject->DoVerb(nVerb, lpMsg, lpClientSite, -1,
		hWnd, lpPosRect);

	pOleState->m_pActivateView = pViewSave;

	// clear out m_pView in case in-place activation only partially worked
	if (!IsInPlaceActive())
		m_pView = NULL;

	// update available status based on the results of DoVerb
	//  (this is used in the links dialog).
	m_bLinkUnavail = (BYTE)FAILED(sc);

	CheckGeneral(sc);
}

//////////////////////////////////////////////////////////////////////////////
// Create error handling

void COleClientItem::CheckGeneral(SCODE sc)
	// set 'm_scLast'
	// throw exception if not ok to continue
{
	ASSERT_VALID(this);

	m_scLast = S_OK;    // assume things are ok

	// then, check for error
	if (sc != S_OK)
	{
		m_scLast = sc;
		if (!FAILED(sc))
		{
#ifdef _DEBUG
			// warn about non-NULL success codes
			TRACE1("Warning: operation returned scode = %s.\n",
				AfxGetFullScodeString(m_scLast));
#endif
			return;
		}
		// this error wasn't expected, so throw an exception
		AfxThrowOleException(sc);
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem clipboard support

void COleClientItem::CopyToClipboard(BOOL bIncludeLink)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// get clipboard data for this item
	COleDataSource* pDataSource = OnGetClipboardData(bIncludeLink, NULL, NULL);
	TRY
	{
		// put it on the clipboard
		pDataSource->SetClipboard();
	}
	CATCH_ALL(e)
	{
		delete pDataSource;
		THROW_LAST();
	}
	END_CATCH_ALL
}

COleDataSource* COleClientItem::OnGetClipboardData(
	BOOL bIncludeLink, LPPOINT lpOffset, LPSIZE lpSize)
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


DROPEFFECT COleClientItem::DoDragDrop(LPCRECT lpItemRect, CPoint ptOffset,
	BOOL bIncludeLink, DWORD dwEffects, LPCRECT lpRectStartDrag)
{
	ASSERT(AfxIsValidAddress(lpItemRect, sizeof(RECT)));
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	DROPEFFECT dropEffect = DROPEFFECT_NONE;
	COleDataSource *pDataSource = NULL;
	TRY
	{
		// get clipboard data object for the item
		CSize sizeItem(
			lpItemRect->right - lpItemRect->left,
			lpItemRect->bottom - lpItemRect->top);
		pDataSource = OnGetClipboardData(bIncludeLink, &ptOffset, &sizeItem);

		// add DROPEFFECT_LINK only if link source is available
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
			rectDrag.SetRect(lpItemRect->left, lpItemRect->bottom,
				lpItemRect->left, lpItemRect->bottom);
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

void COleClientItem::GetClipboardData(COleDataSource* pDataSource,
	BOOL bIncludeLink, LPPOINT lpOffset, LPSIZE lpSize)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT_VALID(pDataSource);

	// add CF_EMBEDDEDOBJECT by creating memory storage copy of the object
	STGMEDIUM stgMedium;
	GetEmbeddedItemData(&stgMedium);
	pDataSource->CacheData((CLIPFORMAT)_oleData.cfEmbeddedObject, &stgMedium);

	// add CF_OBJECTDESCRIPTOR
	GetObjectDescriptorData(lpOffset, lpSize, &stgMedium);
	pDataSource->CacheData((CLIPFORMAT)_oleData.cfObjectDescriptor,
		&stgMedium);

	// add any presentation entries in the object's cache
	AddCachedData(pDataSource);

	// add CF_LINKSOURCE if supporting links to embeddings
	if (bIncludeLink && GetLinkSourceData(&stgMedium))
	{
		pDataSource->CacheData((CLIPFORMAT)_oleData.cfLinkSource, &stgMedium);

		// add CF_LINKSOURCEDESCRIPTOR
		GetObjectDescriptorData(lpOffset, lpSize, &stgMedium);
		pDataSource->CacheData((CLIPFORMAT)_oleData.cfLinkSourceDescriptor,
			&stgMedium);
	}
}

BOOL PASCAL COleClientItem::CanPaste()
{
	// it is faster and more reliable to use the Windows clipboard
	//  APIs instead of OleQueryCreateFromData.
	return IsClipboardFormatAvailable(_oleData.cfEmbedSource) ||
		IsClipboardFormatAvailable(_oleData.cfEmbeddedObject) ||
		IsClipboardFormatAvailable(_oleData.cfFileName) ||
		IsClipboardFormatAvailable(_oleData.cfFileNameW) ||
		IsClipboardFormatAvailable(CF_METAFILEPICT) ||
		IsClipboardFormatAvailable(CF_DIB) ||
		IsClipboardFormatAvailable(CF_BITMAP) ||
		(IsClipboardFormatAvailable(_oleData.cfOwnerLink) &&
			IsClipboardFormatAvailable(_oleData.cfNative));
}

BOOL PASCAL COleClientItem::CanPasteLink()
{
	// it is faster and more reliable to use the Windows clipboard
	//  APIs instead of OleQueryCreateFromData.
	return IsClipboardFormatAvailable(_oleData.cfLinkSource) ||
		IsClipboardFormatAvailable(_oleData.cfFileName) ||
		IsClipboardFormatAvailable(_oleData.cfFileNameW) ||
		IsClipboardFormatAvailable(_oleData.cfObjectLink);
}

BOOL PASCAL
COleClientItem::CanCreateFromData(const COleDataObject* pDataObject)
{
	if (pDataObject->m_bClipboard)
		return COleClientItem::CanPaste();

	((COleDataObject*)pDataObject)->EnsureClipboardObject();
	if (pDataObject->m_lpDataObject == NULL)
		return FALSE;

	SCODE sc = ::OleQueryCreateFromData(pDataObject->m_lpDataObject);
	return !FAILED(sc) && sc != S_FALSE;
}

BOOL PASCAL
COleClientItem::CanCreateLinkFromData(const COleDataObject* pDataObject)
{
	if (pDataObject->m_bClipboard)
		return COleClientItem::CanPasteLink();

	((COleDataObject*)pDataObject)->EnsureClipboardObject();
	if (pDataObject->m_lpDataObject == NULL)
		return FALSE;

	SCODE sc = ::OleQueryLinkFromData(pDataObject->m_lpDataObject);
	return !FAILED(sc) && sc != S_FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// Conversion & Activate As support

BOOL COleClientItem::ConvertTo(REFCLSID clsidNew)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// first, close the object
	Close();

	// convert it
	m_scLast = _AfxOleDoConvert(m_lpStorage, clsidNew);
	if (FAILED(m_scLast))
		return FALSE;

	// save IOleObject and IViewObject2 pointers
	LPOLEOBJECT lpObject = m_lpObject;
	LPVIEWOBJECT2 lpViewObject = m_lpViewObject;
	DWORD dwConnection = m_dwConnection;

	// NULL out IOleObject and IViewObject2 cached pointers
	m_lpObject = NULL;
	m_lpViewObject = NULL;
	m_dwConnection = 0;

	// then load the new object from the new storage
	BOOL bResult = FinishCreate(::OleLoad(m_lpStorage, IID_IUnknown,
		NULL, (LPLP)&m_lpObject));

	if (bResult)
	{
		RELEASE(lpObject);
		RELEASE(lpViewObject);
	}
	else
	{
		m_lpObject = lpObject;
		m_lpViewObject = lpViewObject;
		m_dwConnection = dwConnection;
		UpdateItemType();
	}
	ASSERT_VALID(this);

	return bResult;
}

BOOL COleClientItem::Reload()
{
	// first, close the object
	Close();

	// release any pointers we have to the object
	RELEASE(m_lpObject);
	RELEASE(m_lpViewObject);

	// then reload the object with OleLoad and finish creation process
	BOOL bResult = FinishCreate(::OleLoad(m_lpStorage, IID_IUnknown,
		NULL, (LPLP)&m_lpObject));

	ASSERT_VALID(this);
	return bResult;
}

BOOL COleClientItem::ActivateAs(LPCTSTR lpszUserType,
	REFCLSID clsidOld, REFCLSID clsidNew)
{
	ASSERT_VALID(this);
	ASSERT(lpszUserType == NULL || AfxIsValidString(lpszUserType));
	ASSERT(m_lpObject != NULL);

	// enable activate as
	m_scLast = _AfxOleDoTreatAsClass(lpszUserType, clsidOld, clsidNew);
	if (FAILED(m_scLast))
		return FALSE;

	// reload all items in this doucment
	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	POSITION pos = pDoc->GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = pDoc->GetNextClientItem(pos)) != NULL)
	{
		// reload it, so activate as works as appropriate
		pItem->Reload();
	}

	ASSERT_VALID(this);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem printing support

BOOL COleClientItem::SetPrintDevice(const DVTARGETDEVICE* ptd)
{
	ASSERT(ptd == NULL ||
		AfxIsValidAddress(ptd, sizeof(DVTARGETDEVICE), FALSE));

	// get printer device information from cache
	LPOLECACHE lpOleCache;
	DVTARGETDEVICE* ptdCur = NULL;
	DWORD dwConnection;
	if (!GetPrintDeviceInfo(&lpOleCache, &ptdCur, &dwConnection))
	{
		lpOleCache = QUERYINTERFACE(m_lpObject, IOleCache);
		if (lpOleCache == NULL)
			return FALSE;   // no print device info available
	}
	ASSERT(lpOleCache != NULL);

	// both may have no target device (considered equal)
	if (ptd == NULL && ptdCur == NULL)
	{
		lpOleCache->Release();
		CoTaskMemFree(ptdCur);
		return TRUE;
	}

	if (ptd != NULL && ptdCur != NULL)
	{
		// should be non-NULL and valid addresses
		ASSERT(AfxIsValidAddress(ptd, (size_t)ptd->tdSize));
		ASSERT(AfxIsValidAddress(ptdCur, (size_t)ptdCur->tdSize));
		// see if they compare equal
		if (ptdCur->tdSize == ptd->tdSize &&
			memcmp(ptdCur, ptd, (size_t)ptd->tdSize) == 0)
		{
			lpOleCache->Release();
			CoTaskMemFree(ptdCur);
			return TRUE;
		}
	}

	// calling this with NULL will just remove the prevous printer cache
	if (ptd != NULL)
	{
		// new cache is for CF_METAFILEPICT, DVASPECT_CONTENT
		FORMATETC formatEtc;
		formatEtc.cfFormat = CF_METAFILEPICT;
		formatEtc.ptd = (DVTARGETDEVICE*)ptd;
		formatEtc.dwAspect = DVASPECT_CONTENT;
		formatEtc.lindex = -1;
		formatEtc.tymed = TYMED_MFPICT;

		// attempt to cache new format
		DWORD dwNewConnection;
		if (lpOleCache->Cache(&formatEtc, ADVFCACHE_ONSAVE,
			&dwNewConnection) != S_OK)
		{
			lpOleCache->Release();
			CoTaskMemFree(ptdCur);
			return FALSE;
		}
	}
	// new format is cached successfully, uncache old format
	if (ptdCur != NULL)
	{
		lpOleCache->Uncache(dwConnection);
		CoTaskMemFree(ptdCur);
	}
	// cleanup & return
	lpOleCache->Release();
	return TRUE;
}

BOOL COleClientItem::SetPrintDevice(const PRINTDLG* ppd)
{
	ASSERT(ppd == NULL || AfxIsValidAddress(ppd, sizeof(*ppd), FALSE));
	DVTARGETDEVICE* ptd = NULL;
	if (ppd != NULL)
		ptd = _AfxOleCreateTargetDevice((PRINTDLG*)ppd);

	BOOL bResult = SetPrintDevice(ptd);
	CoTaskMemFree(ptd);
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// other advanced COleClientItem support

void COleClientItem::GetUserType(
	USERCLASSTYPE nUserClassType, CString& rString)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	LPOLESTR lpszUserType;
	CheckGeneral(m_lpObject->GetUserType(nUserClassType, &lpszUserType));
	ASSERT(lpszUserType != NULL);
	ASSERT(AfxIsValidString(lpszUserType));
	rString = lpszUserType;
	CoTaskMemFree(lpszUserType);
}

void COleClientItem::Run()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// is object already in running state?
	if (::OleIsRunning(m_lpObject))
		return;

	// run the object -- throw exception on errors
	SCODE sc = ::OleRun(m_lpObject);
	CheckGeneral(sc);

	// should be running now
	ASSERT(::OleIsRunning(m_lpObject));
}

/////////////////////////////////////////////////////////////////////////////
// Linked COleClientItem operations

BOOL COleClientItem::UpdateLink()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	m_scLast = S_OK;
	if (!IsLinkUpToDate())
	{
		m_scLast = m_lpObject->Update();
		ASSERT_VALID(m_pDocument);
		m_pDocument->SetModifiedFlag();
	}
	return m_scLast == S_OK;
}

BOOL COleClientItem::FreezeLink()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(m_pDocument != NULL);
	ASSERT(GetType() == OT_LINK);

	// first save & close the item
	Close();

	// get IDataObject interface
	LPDATAOBJECT lpDataObject = QUERYINTERFACE(m_lpObject, IDataObject);
	ASSERT(lpDataObject != NULL);
	COleDataObject dataObject;
	dataObject.Attach(lpDataObject, TRUE);

	// save important state of original item
	LPOLEOBJECT lpObject = m_lpObject;
	LPSTORAGE lpStorage = m_lpStorage;
	LPLOCKBYTES lpLockBytes = m_lpLockBytes;
	LPVIEWOBJECT2 lpViewObject = m_lpViewObject;
	DWORD dwConnection = m_dwConnection;
	DWORD dwItemNumber = m_dwItemNumber;
	m_lpObject = NULL;
	m_lpStorage = NULL;
	m_lpLockBytes = NULL;
	m_lpViewObject = NULL;
	m_dwConnection = 0;

	// attempt to create new object from data
	if (!CreateStaticFromData(&dataObject))
	{
		m_lpObject = lpObject;
		m_lpStorage = lpStorage;
		m_lpLockBytes = lpLockBytes;
		m_lpViewObject = lpViewObject;
		m_dwConnection = dwConnection;
		return FALSE;
	}
#ifdef _DEBUG
	UpdateItemType();
	ASSERT(GetType() == OT_STATIC);
#endif

	// save new state of that item
	LPOLEOBJECT lpNewObject = m_lpObject;
	LPSTORAGE lpNewStorage = m_lpStorage;
	LPLOCKBYTES lpNewLockBytes = m_lpLockBytes;
	LPVIEWOBJECT2 lpNewViewObject = m_lpViewObject;
	DWORD dwNewConnection = m_dwConnection;
	DWORD dwNewItemNumber = m_dwItemNumber;

	// shut down old item
	m_lpObject = lpObject;
	m_lpStorage = lpStorage;
	m_lpLockBytes = lpLockBytes;
	m_lpViewObject = lpViewObject;
	m_dwConnection = dwConnection;
	m_dwItemNumber = dwItemNumber;
#ifdef _DEBUG
	UpdateItemType();
	ASSERT(GetType() == OT_LINK);
#endif
	Delete(FALSE);  // revokes item & removes storage

	// switch to new item
	m_lpObject = lpNewObject;
	m_lpStorage = lpNewStorage;
	m_lpLockBytes = lpNewLockBytes;
	m_lpViewObject = lpNewViewObject;
	m_dwConnection = dwNewConnection;
	m_dwItemNumber = dwNewItemNumber;
	UpdateItemType();
	ASSERT(GetType() == OT_STATIC);

	// send an on changed with same state to invalidate the item
	OnChange(OLE_CHANGED_STATE, (DWORD)GetItemState());
	ASSERT_VALID(m_pDocument);
	m_pDocument->SetModifiedFlag();

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Special link attributes

OLEUPDATE COleClientItem::GetLinkUpdateOptions()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	LPOLELINK lpOleLink = QUERYINTERFACE(m_lpObject, IOleLink);
	ASSERT(lpOleLink != NULL);  // perhaps not a link?

	DWORD dwUpdateOpt;
	SCODE sc = lpOleLink->GetUpdateOptions(&dwUpdateOpt);
	lpOleLink->Release();
	CheckGeneral(sc);   // may throw an exception

	return (OLEUPDATE)dwUpdateOpt;
}

void COleClientItem::SetLinkUpdateOptions(OLEUPDATE dwUpdateOpt)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	LPOLELINK lpOleLink = QUERYINTERFACE(m_lpObject, IOleLink);
	ASSERT(lpOleLink != NULL);  // perhaps not a link?

	SCODE sc = lpOleLink->SetUpdateOptions(dwUpdateOpt);
	lpOleLink->Release();
	CheckGeneral(sc);
}

/////////////////////////////////////////////////////////////////////////////
