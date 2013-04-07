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
// COleClientItem - Container view of IOleObject and related interfaces

COleClientItem::COleClientItem(COleDocument* pContainerDoc)
{
	if (pContainerDoc != NULL)
		ASSERT_VALID(pContainerDoc);

	// initialize OLE client side view of IOleObject
	m_lpObject = NULL;
	m_lpViewObject = NULL;
	m_dwConnection = 0;
	m_lpStorage = NULL;
	m_lpLockBytes = NULL;
	m_scLast = S_OK;
	m_pView = NULL;
	m_pInPlaceFrame = NULL;
	m_pInPlaceDoc = NULL;
	m_nItemState = emptyState;  // initially empty until OleLoad, OleCreate
	m_bMoniker = FALSE;
	m_nDrawAspect = DVASPECT_CONTENT;   // default draw aspect
	m_dwItemNumber = 0;
	m_bLinkUnavail = FALSE; // set to TRUE on failed DoVerb, or in links dialog
	m_nItemType = OT_UNKNOWN;       // type unknown so far
	m_hWndServer = NULL;
	m_bClosing = FALSE; // COleClientItem::Close in process
	m_bLocked = FALSE;  // need CoLockObjectExternal(..., FALSE, ...)

	// initialize compound file support
	m_lpNewStorage = NULL;
	m_bNeedCommit = FALSE;

	if (pContainerDoc != NULL)
		pContainerDoc->AddItem(this);

	ASSERT(m_pDocument == pContainerDoc);
	ASSERT_VALID(this);

	AfxOleLockApp();
}

COleClientItem::~COleClientItem()
{
	ASSERT_VALID(this);

	// release any references we may have to other objects
	Release();

	// only remove it from the associated document if it hasn't been detached
	//  from the document already!
	if (m_pDocument != NULL)
		m_pDocument->RemoveItem(this);

	// make sure all outside connections are disconnected
	ExternalDisconnect();
	AfxOleUnlockApp();
}

void COleClientItem::Delete(BOOL bAutoDelete)
{
	USES_CONVERSION;
	ASSERT_VALID(this);

	Release();      // first close it

	COleDocument* pDoc = GetDocument();
	if (pDoc != NULL && pDoc->m_bCompoundFile)
	{
		// cleanup docfile storage first
		COleDocument* pDoc = GetDocument();
		ASSERT_VALID(pDoc);

		if (pDoc->m_lpRootStg != NULL)
		{
			// get item name
			TCHAR szItemName[OLE_MAXITEMNAME];
			GetItemName(szItemName);

			// attempt to remove it from the storage, ignore errors
			pDoc->m_lpRootStg->DestroyElement(T2COLE(szItemName));
		}
	}

	if (bAutoDelete)
	{
		// remove item from document
		if (pDoc != NULL)
			pDoc->RemoveItem(this);

		InternalRelease();  // remove the item from memory
	}
}

void COleClientItem::Release(OLECLOSE dwCloseOption)
{
	ASSERT_VALID(this);

	m_scLast = S_OK;

	// cleanup view advise
	if (m_lpViewObject != NULL)
	{
		DWORD dwAspect;
	  IAdviseSink* pAdviseSink;

	  pAdviseSink = NULL;
		VERIFY(m_lpViewObject->GetAdvise(&dwAspect, NULL, &pAdviseSink) == S_OK);
	  if( pAdviseSink != NULL )
	  {
		 RELEASE( pAdviseSink );
	  }
		VERIFY(m_lpViewObject->SetAdvise(dwAspect, 0, NULL) == S_OK);
		RELEASE(m_lpViewObject);
	}

	// cleanup the OLE object itself
	if (m_lpObject != NULL)
	{
		// cleanup object advise
		if (m_dwConnection != 0)
		{
			VERIFY(m_lpObject->Unadvise(m_dwConnection) == S_OK);
			m_dwConnection = 0;
		}

		// close object and save (except now when called from destructor)
		//  (NOTE: errors are _not_ reported as an exception)
		m_scLast = m_lpObject->Close(dwCloseOption);
		RELEASE(m_lpObject);
	}

	// cleanup storage related data
	RELEASE(m_lpStorage);
	RELEASE(m_lpLockBytes);

	// cleanup in-place editing data
	if (m_pInPlaceFrame != NULL)
	{
		m_pInPlaceFrame->InternalRelease();
		m_pInPlaceFrame = NULL;
		if (m_pInPlaceDoc != NULL)
		{
			m_pInPlaceDoc->InternalRelease();
			m_pInPlaceDoc = NULL;
		}
	}
	ASSERT(m_pInPlaceFrame == NULL);
	ASSERT(m_pInPlaceDoc == NULL);
}

void COleClientItem::Close(OLECLOSE dwCloseOption)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// gaurd against re-entry
	if (m_bClosing)
		return;

	m_bClosing = TRUE;

	// attempt to close the object
	m_scLast = m_lpObject->Close(dwCloseOption);

	// remove external lock placed on item during in-place activation
	if (m_bLocked)
	{
		OleLockRunning(m_lpObject, FALSE, TRUE);
		m_bLocked = FALSE;
	}

	// handle failure cases -- COleClientItem::Close can be used to
	//  robustly handle a server crashing (ie. something unexpected happens,
	//  we'll call COleClientItem::Close to attempt safe shutdown)
	if (GetItemState() != loadedState)
	{
		// We'll call COleClientItem::Close anywhere a catastrophe
		//  happens inside of other portions of COleClientItem.  We must
		//  completely exit from any in-place/open state.

		// force transition from activeUIState to activeState
		if (GetItemState() == activeUIState)
			OnDeactivateUI(FALSE);

		// force transition from activeState to loadedState
		if (GetItemState() == activeState)
			OnDeactivate();

		if (m_nItemState != loadedState)
		{
			// in case of extreme failure, force loadedState
			OnChange(OLE_CHANGED_STATE, (DWORD)loadedState);
			m_nItemState = loadedState; // force it to loaded state
		}
	}

	m_bClosing = FALSE; // now safe for further close calls
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem name management

DWORD COleClientItem::GetNewItemNumber()
{
	ASSERT_VALID(this);

	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	DWORD dwNextItemNumber = pDoc->m_dwNextItemNumber;

	for (;;)
	{
		// make sure that m_dwNextItemNumber is not used in another item first
		POSITION pos = pDoc->GetStartPosition();
		COleClientItem* pItem;
		while ((pItem = pDoc->GetNextClientItem(pos)) != NULL)
		{
			if (pItem->m_dwItemNumber == dwNextItemNumber)
				break;
		}
		if (pItem == NULL)
			break;  // no item using m_dwNextItemNumber

		// m_dwNextItemNumber is in use, bump to next one!
		++dwNextItemNumber;
	}

	pDoc->m_dwNextItemNumber = dwNextItemNumber + 1;
	return dwNextItemNumber;
}

void COleClientItem::GetItemName(LPTSTR lpszItemName) const
{
	ASSERT_VALID(this);
	ASSERT(lpszItemName != NULL);

	wsprintf(lpszItemName, _T("Embedding %lu"), m_dwItemNumber);
	ASSERT(lstrlen(lpszItemName) < OLE_MAXITEMNAME);
}

// extracts icon resource ID and path name from registry "file.exe,35" format
static void AfxGetIconInfo(LPCTSTR lpszRegInfo, LPTSTR lpszImagePath,
	UINT& nIndex)
{
	LPTSTR pstrTarget = lpszImagePath;
	LPCTSTR pstrSource = lpszRegInfo;
	while (*pstrSource != ',' && *pstrSource != '\0')
	{
		*pstrTarget = *pstrSource;
		pstrTarget = _tcsinc(pstrTarget);
		pstrSource = _tcsinc(pstrSource);
	}
	*pstrTarget = '\0';

	// extract the index
	if (*pstrSource != '\0')
	{
		LPTSTR pstrIndex = _tcsinc(pstrSource);
		nIndex = (UINT) _ttol(pstrIndex);
	}
	else
		nIndex = 0;
}

HICON COleClientItem::GetIconFromRegistry() const
{
	CLSID clsid;
	GetClassID(&clsid);
	if (clsid == CLSID_NULL)
		return NULL;

	return GetIconFromRegistry(clsid);
}

HICON COleClientItem::GetIconFromRegistry(CLSID& clsid)
{
	// This function will extract the icon registered as the DefaultIcon
	// for the server referred to by clsid.  We get the ProgID for the server
	// and then extract \\hkcr\progid\DefaultIcon.  If that doesn't exist, we
	// get a default icon from \\hkcr\DocShortcut\DefaultIcon and if that fails
	// we just return 0.

	USES_CONVERSION;

	HICON hIcon = NULL;
	HRESULT hr;
	OLECHAR *szCLSID;
	DWORD dwType = 0;
	TCHAR szName[MAX_PATH+1];
	TCHAR szPathName[MAX_PATH+1];
	HKEY hkeyObj;
	HKEY hkeyDefIcon;
	HKEY hkeyCLSID;
	UINT nIndex;

	hr = ::StringFromCLSID(clsid, &szCLSID);
	if (!SUCCEEDED(hr))
		return NULL;

	// first, try for the real icon
	if (RegOpenKeyEx(HKEY_CLASSES_ROOT, _T("clsid"), 0, KEY_READ, &hkeyCLSID) == ERROR_SUCCESS)
	{
		if (RegOpenKeyEx(hkeyCLSID, OLE2T(szCLSID), 0, KEY_READ, &hkeyObj) == ERROR_SUCCESS)
		{
			if (RegOpenKeyEx(hkeyObj, _T("DefaultIcon"), 0, KEY_READ, &hkeyDefIcon) == ERROR_SUCCESS)
			{
				DWORD dwCount;
				dwCount = sizeof(szName);
				if (RegQueryValueEx(hkeyDefIcon, NULL, NULL, &dwType, (BYTE*) szName, &dwCount) == ERROR_SUCCESS)
				{
					AfxGetIconInfo(szName, szPathName, nIndex);

					// Load the icon
					hIcon = ::ExtractIcon(AfxGetApp()->m_hInstance, szPathName, nIndex);

					// ExtractIcon() failure case means NULL return
					if (int(hIcon) == 1)
						hIcon = NULL;
				}
				RegCloseKey(hkeyDefIcon);
			}
			RegCloseKey(hkeyObj);
		}
		RegCloseKey(hkeyCLSID);
	}

	// if we didn't get the real icon, try the default icon
	if (hIcon == NULL)
	{
		if (RegOpenKeyEx(HKEY_CLASSES_ROOT, _T("DocShortcut"), 0, KEY_READ,&hkeyObj) == ERROR_SUCCESS)
		{
			if (RegOpenKeyEx(hkeyObj, _T("DefaultIcon"), 0, KEY_READ, &hkeyDefIcon) == ERROR_SUCCESS)
			{
				DWORD dwCount;
				dwCount = sizeof(szName);
				if (RegQueryValueEx(hkeyDefIcon, NULL, NULL, &dwType, (BYTE*) szName, &dwCount) == ERROR_SUCCESS)
				{
					AfxGetIconInfo(szName, szPathName, nIndex);

					// Load the icon
					hIcon = ::ExtractIcon(AfxGetApp()->m_hInstance, szPathName, nIndex);

					// ExtractIcon() failure case means NULL return
					if (int(hIcon) == 1)
						hIcon = NULL;
				}
				RegCloseKey(hkeyDefIcon);
			}
			RegCloseKey(hkeyObj);
		}
	}

	::CoTaskMemFree(szCLSID);
	return hIcon;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem creation helpers

void COleClientItem::UpdateItemType()
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// check for linked object
	LPOLELINK lpOleLink = QUERYINTERFACE(m_lpObject, IOleLink);
	if (lpOleLink != NULL)
	{
		lpOleLink->Release();
		m_nItemType = OT_LINK;
		return;
	}

	// check for static object
	DWORD dwStatus;
	if (m_lpObject->GetMiscStatus(DVASPECT_CONTENT, &dwStatus) == S_OK
		&& (dwStatus & OLEMISC_STATIC) == 0)
	{
		m_nItemType = OT_EMBEDDED;
		return;
	}

	// not not link, not embedding -- must be static
	m_nItemType = OT_STATIC;
}

BOOL COleClientItem::FinishCreate(SCODE sc)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_pView == NULL);

	TRY
	{
		// m_lpObject is currently an IUnknown, convert to IOleObject
		if (m_lpObject != NULL)
		{
			LPUNKNOWN lpUnk = m_lpObject;
			m_lpObject = QUERYINTERFACE(lpUnk, IOleObject);
			lpUnk->Release();
			if (m_lpObject == NULL)
				AfxThrowOleException(E_OUTOFMEMORY);
		}

		// check return code from create function
		CheckGeneral(sc);

		UpdateItemType();

		// cache the IViewObject interface
		m_lpViewObject = QUERYINTERFACE(m_lpObject, IViewObject2);
		if (m_lpViewObject == NULL)
			CheckGeneral(E_NOINTERFACE);
		ASSERT(m_lpViewObject != NULL);

		if (GetType() != OT_STATIC)
		{
			// setup for advises; we assume that OLE cleans them up properly
			LPADVISESINK lpAdviseSink =
				(LPADVISESINK)GetInterface(&IID_IAdviseSink);
			ASSERT(lpAdviseSink != NULL);
			CheckGeneral(m_lpObject->Advise(lpAdviseSink, &m_dwConnection));
			ASSERT(m_dwConnection != 0);

			// set up view advise
			VERIFY(m_lpViewObject->SetAdvise(DVASPECT_CONTENT, 0, lpAdviseSink)
				== S_OK);

			// the server shows these in its user-interface
			//  (as document title and in File Exit menu)
			m_lpObject->SetHostNames(T2COLE(AfxGetAppName()),
				T2COLE(m_pDocument->GetTitle()));
		}

		// all items are "contained" -- this makes our reference to this object
		//  weak -- which is needed for links to embedding silent update.
		OleSetContainedObject(m_lpObject, TRUE);

		// considered loaded at this point
		m_nItemState = loadedState;
	}
	CATCH_ALL(e)
	{
		Release();  // release the object just in case
		ASSERT_VALID(this);
		DELETE_EXCEPTION(e);
		return FALSE;
	}
	END_CATCH_ALL

	// set state to loaded
	ASSERT(m_nItemState != emptyState);

	// otherwise no errors, return success!
	ASSERT_VALID(this);
	return TRUE;
}

//////////////////////////////////////////////////////////////////////////////
// COleClientItem create API variants

BOOL COleClientItem::CreateFromClipboard(
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get clipboard contents
	COleDataObject dataObject;
	if (!dataObject.AttachClipboard())
		return FALSE;

	// create from IDataObject
	BOOL bResult = CreateFromData(&dataObject, render, cfFormat, lpFormatEtc);

	ASSERT_VALID(this);
	return bResult;
}

BOOL COleClientItem::CreateLinkFromClipboard(
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get clipboard contents
	COleDataObject dataObject;
	if (!dataObject.AttachClipboard())
		return FALSE;

	// create from IDataObject
	BOOL bResult = CreateLinkFromData(&dataObject, render, cfFormat, lpFormatEtc);
	ASSERT_VALID(this);

	return bResult;
}

BOOL COleClientItem::CreateStaticFromClipboard(
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get clipboard contents
	COleDataObject dataObject;
	if (!dataObject.AttachClipboard())
		return FALSE;

	// create from IDataObject
	BOOL bResult = CreateStaticFromData(&dataObject, render, cfFormat,
		lpFormatEtc);

	ASSERT_VALID(this);
	return bResult;
}

// Creation from IDataObject (used for drag-drop)
BOOL COleClientItem::CreateFromData(COleDataObject* pDataObject,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the object
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	LPDATAOBJECT lpDataObject = pDataObject->GetIDataObject(FALSE);
	SCODE sc = ::OleCreateFromData(lpDataObject, IID_IUnknown, render,
		lpFormatEtc, lpClientSite, m_lpStorage, (LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

BOOL COleClientItem::CreateLinkFromData(COleDataObject* pDataObject,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the link
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	LPDATAOBJECT lpDataObject = pDataObject->GetIDataObject(FALSE);
	SCODE sc = ::OleCreateLinkFromData(lpDataObject, IID_IUnknown,
		render, lpFormatEtc, lpClientSite, m_lpStorage, (LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

BOOL COleClientItem::CreateStaticFromData(COleDataObject* pDataObject,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the link
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	LPDATAOBJECT lpDataObject = pDataObject->GetIDataObject(FALSE);
	SCODE sc = ::OleCreateStaticFromData(lpDataObject, IID_IUnknown,
		render, lpFormatEtc, lpClientSite, m_lpStorage, (LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

// Creation from files (in OLE 1.0, the user did this through packager)
BOOL COleClientItem::CreateFromFile(LPCTSTR lpszFileName, REFCLSID clsid,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the object
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	SCODE sc = ::OleCreateFromFile(clsid, T2COLE(lpszFileName),
		IID_IUnknown, render, lpFormatEtc, lpClientSite, m_lpStorage,
		(LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

BOOL COleClientItem::CreateLinkFromFile(LPCTSTR lpszFileName,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the link
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	SCODE sc = ::OleCreateLinkToFile(T2COLE(lpszFileName),
		IID_IUnknown, render, lpFormatEtc, lpClientSite, m_lpStorage,
		(LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

// create from class name (for insert item dialog)
BOOL COleClientItem::CreateNewItem(REFCLSID clsid,
	OLERENDER render, CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT(m_pDocument != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// get storage for the object via virtual function call
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to create the object
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	SCODE sc = ::OleCreate(clsid, IID_IUnknown,
		render, lpFormatEtc, lpClientSite, m_lpStorage, (LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// More advanced creation

BOOL COleClientItem::CreateCloneFrom(const COleClientItem* pSrcItem)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject == NULL);     // one time only
	ASSERT_VALID(pSrcItem);
	ASSERT(m_pDocument != NULL);

	// create storage for the item
	m_dwItemNumber = GetNewItemNumber();
	GetItemStorage();
	ASSERT(m_lpStorage != NULL);

	// save the object first
	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(pSrcItem->m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);
	SCODE sc = ::OleSave(lpPersistStorage, m_lpStorage, FALSE);
	lpPersistStorage->SaveCompleted(NULL);
	lpPersistStorage->Release();
	if (sc != S_OK)
	{
		// failed the save, do not attempt to create clone
		m_scLast = sc;
		return FALSE;
	}

	// get information on the view advise type
	ASSERT(pSrcItem->m_lpViewObject != NULL);
	DWORD dwAspect;
   IAdviseSink* pAdviseSink;
   pAdviseSink = NULL;
	VERIFY(pSrcItem->m_lpViewObject->GetAdvise(&dwAspect, NULL, &pAdviseSink) ==
	  S_OK);
   if( pAdviseSink != NULL )
   {
	  RELEASE(pAdviseSink);
   }

	// then load the new object from the new storage
	LPOLECLIENTSITE lpClientSite = GetClientSite();
	sc = ::OleLoad(m_lpStorage, IID_IUnknown,
		lpClientSite, (LPLP)&m_lpObject);
	BOOL bResult = FinishCreate(sc);

	ASSERT_VALID(this);
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// Storage for COleClientItem objects (memory and compound files)

BOOL COleClientItem::IsModified() const
{
   SCODE sc;

	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// get IPersistStorage interface, and call IsDirty
	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(m_lpObject, IPersistStorage);
   if( lpPersistStorage != NULL )
   {
	   sc = lpPersistStorage->IsDirty();
	   lpPersistStorage->Release();
   }
   else
   {
	  LPPERSISTSTREAMINIT lpPersistStreamInit;
	  lpPersistStreamInit = QUERYINTERFACE( m_lpObject, IPersistStreamInit );
	  if( lpPersistStreamInit != NULL )
	  {
		 sc = lpPersistStreamInit->IsDirty();
		 lpPersistStreamInit->Release();
	  }
	  else
	  {
		 LPPERSISTSTREAM lpPersistStream;
		 lpPersistStream = QUERYINTERFACE( m_lpObject, IPersistStream );
		 if( lpPersistStream != NULL )
		 {
			sc = lpPersistStream->IsDirty();
			lpPersistStream->Release();
		 }
		 else
		 {
			sc = E_NOINTERFACE;
		 }
	  }
   }

	// S_OK == S_TRUE, therefore object dirty!
	return sc == S_OK || FAILED(sc);
}

void COleClientItem::GetItemStorageFlat()
{
	ASSERT_VALID(this);
	ASSERT(m_lpStorage == NULL);
	ASSERT(m_lpLockBytes == NULL);

	SCODE sc = ::CreateILockBytesOnHGlobal(NULL, TRUE, &m_lpLockBytes);
	if (sc != S_OK)
		AfxThrowOleException(sc);
	ASSERT(m_lpLockBytes != NULL);

	sc = ::StgCreateDocfileOnILockBytes(m_lpLockBytes,
		STGM_SHARE_EXCLUSIVE|STGM_CREATE|STGM_READWRITE, 0, &m_lpStorage);
	if (sc != S_OK)
	{
		VERIFY(m_lpLockBytes->Release() == 0);
		m_lpLockBytes = NULL;
		AfxThrowOleException(sc);
	}
	ASSERT(m_lpStorage != NULL);

	ASSERT_VALID(this);
}

void COleClientItem::ReadItemFlat(CArchive& ar)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStorage == NULL);
	ASSERT(m_lpLockBytes == NULL);

	// read number of bytes in the ILockBytes
	DWORD dwBytes;
	ar >> dwBytes;

	// allocate enough memory to read entire block
	HGLOBAL hStorage = ::GlobalAlloc(GMEM_SHARE|GMEM_MOVEABLE, dwBytes);
	if (hStorage == NULL)
		AfxThrowMemoryException();

	LPVOID lpBuf = ::GlobalLock(hStorage);
	ASSERT(lpBuf != NULL);
	DWORD dwBytesRead = ar.Read(lpBuf, dwBytes);
	::GlobalUnlock(hStorage);

	// throw exception in case of partial object
	if (dwBytesRead != dwBytes)
	{
		::GlobalFree(hStorage);
		AfxThrowArchiveException(CArchiveException::endOfFile);
	}

	SCODE sc = CreateILockBytesOnHGlobal(hStorage, TRUE, &m_lpLockBytes);
	if (sc != S_OK)
	{
		::GlobalFree(hStorage);
		AfxThrowOleException(sc);
	}
	ASSERT(m_lpLockBytes != NULL);
	ASSERT(::StgIsStorageILockBytes(m_lpLockBytes) == S_OK);

	sc = ::StgOpenStorageOnILockBytes(m_lpLockBytes, NULL,
		STGM_SHARE_EXCLUSIVE|STGM_READWRITE, NULL, 0, &m_lpStorage);
	if (sc != S_OK)
	{
		VERIFY(m_lpLockBytes->Release() == 0);
		m_lpLockBytes = NULL;
			// ILockBytes::Release will GlobalFree the hStorage
		AfxThrowOleException(sc);
	}

	// attempt to load the object from the storage
	LPUNKNOWN lpUnk = NULL;
	sc = ::OleLoad(m_lpStorage, IID_IUnknown, GetClientSite(),
		(LPLP)&lpUnk);
	CheckGeneral(sc);

	ASSERT(lpUnk != NULL);
	m_lpObject = QUERYINTERFACE(lpUnk, IOleObject);
	lpUnk->Release();
	if (m_lpObject == NULL)
		AfxThrowOleException(E_OUTOFMEMORY);

	ASSERT_VALID(this);
}

void COleClientItem::WriteItemFlat(CArchive& ar)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStorage != NULL);
	ASSERT(m_lpLockBytes != NULL);

	// save the OLE object to its storage first
	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);
	SCODE sc;
	if (GetDocument()->m_bCompoundFile || lpPersistStorage->IsDirty() == S_OK)
	{
		sc = ::OleSave(lpPersistStorage, m_lpStorage,
			!GetDocument()->m_bCompoundFile);
		lpPersistStorage->SaveCompleted(NULL);
	}
	lpPersistStorage->Release();
	m_lpStorage->Commit(STGC_OVERWRITE);
	ASSERT(::StgIsStorageILockBytes(m_lpLockBytes) == S_OK);

	// attempt to get the handle to the global memory
	HGLOBAL hStorage;
	sc = ::GetHGlobalFromILockBytes(m_lpLockBytes, &hStorage);
	if (sc != S_OK)
		AfxThrowOleException(sc);

	// first write a byte count
	STATSTG statstg;
	sc = m_lpLockBytes->Stat(&statstg, STATFLAG_NONAME);
	if (sc != S_OK)
		AfxThrowOleException(sc);
	ASSERT(statstg.cbSize.HighPart == 0);   // don't support >4GB objects
	DWORD dwBytes = statstg.cbSize.LowPart;
	ar << dwBytes;

	// write the contents of the block
	LPVOID lpBuf = GlobalLock(hStorage);
	ASSERT(lpBuf != NULL);
	ar.Write(lpBuf, (UINT)dwBytes);
	::GlobalUnlock(hStorage);
}

void COleClientItem::GetItemStorageCompound()
{
	USES_CONVERSION;

	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	ASSERT(pDoc->m_bCompoundFile);
	if (pDoc->m_lpRootStg == NULL)
	{
		ASSERT(pDoc->m_bEmbedded);
		pDoc->m_bEmbedded = FALSE;
		if (!pDoc->OnNewDocument())
		{
			TRACE0("Warning OnNewDocument failed during COleClientItem::CreateXXXX\n");
			AfxThrowMemoryException();
		}
	}
	ASSERT(pDoc->m_lpRootStg != NULL);

	// get item name
	TCHAR szItemName[OLE_MAXITEMNAME];
	GetItemName(szItemName);

	// create storage for this item
	LPSTORAGE lpStorage;
	SCODE sc = pDoc->m_lpRootStg->CreateStorage(T2COLE(szItemName),
		STGM_CREATE|STGM_READWRITE|STGM_TRANSACTED|STGM_SHARE_EXCLUSIVE,
		0, 0, &lpStorage);
	if (sc != S_OK)
	{
		TRACE1("Warning: unable to create child storage %s.\n", szItemName);
		// upon failure throw file exception (item will be cleaned up)
		AfxThrowOleException(sc);
	}
	ASSERT(lpStorage != NULL);

	// everything should have worked
	m_lpStorage = lpStorage;
	ASSERT(m_lpStorage != NULL);
}

void COleClientItem::ReadItemCompound(CArchive& ar)
{
	USES_CONVERSION;

	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	ASSERT(pDoc->m_lpRootStg != NULL);
	ASSERT(pDoc->m_bCompoundFile);
	ASSERT(m_lpStorage == NULL);
	ASSERT(m_lpLockBytes == NULL);

	if (ar.m_bForceFlat)
	{
		ReadItemFlat(ar);
		RELEASE(m_lpStorage);
		RELEASE(m_lpLockBytes);

		// change the number to something definitely unique
		m_dwItemNumber = GetNewItemNumber();

		// create new storage
		GetItemStorageCompound();
		LPPERSISTSTORAGE lpPersistStorage =
			QUERYINTERFACE(m_lpObject, IPersistStorage);
		ASSERT(lpPersistStorage != NULL);
		SCODE sc = ::OleSave(lpPersistStorage, m_lpStorage, FALSE);
		if (FAILED(sc))
		{
			lpPersistStorage->Release();
			CheckGeneral(sc);
		}
		VERIFY(lpPersistStorage->SaveCompleted(m_lpStorage) == S_OK);
		lpPersistStorage->Release();
	}
	else
	{
		// get item name
		TCHAR szItemName[OLE_MAXITEMNAME];
		GetItemName(szItemName);

		// open storage for this item
		LPSTORAGE lpStorage;
		SCODE sc = pDoc->m_lpRootStg->OpenStorage(T2COLE(szItemName), NULL,
			STGM_READWRITE|STGM_TRANSACTED|STGM_SHARE_EXCLUSIVE,
			0, 0, &lpStorage);
		if (sc != S_OK)
		{
			TRACE1("Warning: unable to open child storage %s.\n", szItemName);
			// upon failure throw file exception (item will be cleaned up)
			AfxThrowOleException(sc);
		}
		ASSERT(lpStorage != NULL);

		// remember the storage
		m_lpStorage = lpStorage;
		ASSERT(m_lpStorage != NULL);

		// attempt to load the object from the storage
		LPUNKNOWN lpUnk = NULL;
		sc = ::OleLoad(m_lpStorage, IID_IUnknown, GetClientSite(),
			(LPLP)&lpUnk);
		CheckGeneral(sc);

		// get IOleObject interface for the newly loaded object
		ASSERT(lpUnk != NULL);
		m_lpObject = QUERYINTERFACE(lpUnk, IOleObject);
		lpUnk->Release();
		if (m_lpObject == NULL)
			AfxThrowOleException(E_OUTOFMEMORY);
	}
}

void COleClientItem::WriteItemCompound(CArchive& ar)
{
	USES_CONVERSION;

	COleDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	ASSERT(pDoc->m_lpRootStg != NULL);
	ASSERT(pDoc->m_bCompoundFile);
	ASSERT(m_lpNewStorage == NULL);

	if (ar.m_bForceFlat)
	{
		LPSTORAGE pTempStorage = m_lpStorage;
		LPLOCKBYTES pTempLockBytes = m_lpLockBytes;
		m_lpStorage = NULL;
		m_lpLockBytes = NULL;
		GetItemStorageFlat();
		WriteItemFlat(ar);
		RELEASE(m_lpStorage);
		RELEASE(m_lpLockBytes);
		m_lpStorage = pTempStorage;
		m_lpLockBytes = pTempLockBytes;
		return;
	}

	// get item name
	TCHAR szItemName[OLE_MAXITEMNAME];
	GetItemName(szItemName);

	// determine destination storage
	ASSERT(m_lpStorage != NULL);
	LPSTORAGE lpStorage = m_lpStorage;
	if (!pDoc->m_bSameAsLoad)
	{
		// need to create new storage for this item
		SCODE sc = pDoc->m_lpRootStg->CreateStorage(T2COLE(szItemName),
			STGM_CREATE|STGM_READWRITE|STGM_TRANSACTED|STGM_SHARE_EXCLUSIVE,
			0, 0, &lpStorage);
		if (sc != S_OK)
		{
			TRACE1("Warning: unable to create child storage %s.\n",
				szItemName);
			AfxThrowOleException(sc);
		}
		// remember the storage for CommitItem stage
		m_lpNewStorage = lpStorage;
		m_bNeedCommit = TRUE;
	}
	ASSERT(lpStorage != NULL);

	// save dirty object
	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);
	SCODE sc = S_OK;
	if (!pDoc->m_bSameAsLoad || lpPersistStorage->IsDirty() == S_OK)
	{
		// call OleSave now and IPersistStorage::SaveCompleted later
		sc = ::OleSave(lpPersistStorage, lpStorage, pDoc->m_bSameAsLoad);
	}
	lpPersistStorage->Release();

	// if it fails, abort the save
	if (FAILED(sc))
		AfxThrowOleException(sc);

	// now will need to call CommitItem for this item
	m_bNeedCommit = TRUE;
	lpStorage->Commit(STGC_ONLYIFCURRENT);
}

void COleClientItem::GetItemStorage()
{
	if (GetDocument()->m_bCompoundFile)
		GetItemStorageCompound();
	else
		GetItemStorageFlat();
}

void COleClientItem::ReadItem(CArchive& ar)
{
	if (GetDocument()->m_bCompoundFile)
		ReadItemCompound(ar);
	else
		ReadItemFlat(ar);
}

void COleClientItem::WriteItem(CArchive& ar)
{
	if (GetDocument()->m_bCompoundFile)
		WriteItemCompound(ar);
	else
		WriteItemFlat(ar);
}

void COleClientItem::CommitItem(BOOL bSuccess)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	if (!m_bNeedCommit)
		return;

	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);

	// forget about new storage if save failed along the way...
	if (m_lpNewStorage != NULL && !bSuccess)
		RELEASE(m_lpNewStorage);

	// let the object remember the new storage
	VERIFY(lpPersistStorage->SaveCompleted(m_lpNewStorage) == S_OK);
	lpPersistStorage->Release();

	// determine/remember new storage
	if (m_lpNewStorage != NULL)
	{
		m_lpStorage->Release();
		m_lpStorage = m_lpNewStorage;
		m_lpNewStorage = NULL;
	}

	m_bNeedCommit = FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem serialization

void COleClientItem::Serialize(CArchive& ar)
{
	ASSERT_VALID(this);

	CDocItem::Serialize(ar);
	ASSERT(m_pDocument != NULL);  // must 'SetDocument' first

	if (ar.IsStoring())
	{
		ASSERT(m_lpObject != NULL);

		// first, save the type flag (this is used for versioning)
		ar << (DWORD)OT_OLE2;

		ar << m_dwItemNumber;   // save the item number

		// write the view advise type to storage
		ASSERT(m_lpViewObject != NULL);
		DWORD dwAspect;
	  IAdviseSink* pAdviseSink;
	  pAdviseSink = NULL;
		VERIFY(m_lpViewObject->GetAdvise(&dwAspect, NULL, &pAdviseSink) == S_OK);
	  if( pAdviseSink != NULL )
	  {
		 RELEASE(pAdviseSink);
	  }
		ar << dwAspect;         // save the display aspect

		// write flag indicating whether to create moniker upon load
		ar << (WORD)m_bMoniker;

		// save current default display aspect
		ar << (DWORD)m_nDrawAspect;

		// save object to storage (calls OleSave)
		WriteItem(ar);
	}
	else
	{
		ASSERT(m_lpObject == NULL);

		// first, get the type flag (for OLE 1.0 compatible reading)
		DWORD dwType;
		ar >> dwType;
		if (dwType != OT_OLE2)
			AfxThrowArchiveException(CArchiveException::generic);

		ar >> m_dwItemNumber;   // get the item number

		DWORD dwAspect; // read the display aspect (aspects that are cached)
		ar >> dwAspect;

		WORD bMoniker;  // see if we should create & set the moniker
		ar >> bMoniker;

		DWORD nDrawAspect;  // read the default display aspect
		ar >> nDrawAspect;
		m_nDrawAspect = (DVASPECT)nDrawAspect;

		// read the OLE object from storage (calls OleLoad)
		ReadItem(ar);

		// finish OLE object creation process, setup advises, etc.
		if (!FinishCreate(S_OK))
			AfxThrowArchiveException(CArchiveException::generic);

		if (bMoniker)
		{
			// force moniker creation by calling GetMoniker
			LPMONIKER lpMoniker;
			if (GetClientSite()->GetMoniker(OLEGETMONIKER_FORCEASSIGN,
				OLEWHICHMK_OBJREL, &lpMoniker) == S_OK)
			{
				ASSERT(lpMoniker != NULL);
				lpMoniker->Release();
				ASSERT(m_bMoniker); // moniker should have been assigned
			}
		}

		// fix up the document's m_dwNextItemNumber
		if (m_dwItemNumber >= GetDocument()->m_dwNextItemNumber)
			GetDocument()->m_dwNextItemNumber = m_dwItemNumber + 1;
	}
}

/////////////////////////////////////////////////////////////////////////////
// default callback implementation

void COleClientItem::OnChange(OLE_NOTIFICATION nCode, DWORD /*dwParam*/)
{
	ASSERT_VALID(this);

	switch (nCode)
	{
	case OLE_CLOSED:
		break;  // no default implementation

	case OLE_CHANGED:
	case OLE_SAVED:
		ASSERT(m_pDocument != NULL);
		m_pDocument->SetModifiedFlag();
		break;

	case OLE_CHANGED_STATE:
	case OLE_CHANGED_ASPECT:
		break;  // no default implementation

	default:
		ASSERT(FALSE);
	}

	ASSERT_VALID(this);
}

void COleClientItem::OnDataChange(
	LPFORMATETC /*lpFormatEtc*/, LPSTGMEDIUM /*lpStgMedium*/)
{
	ASSERT(FALSE);  // derivative must override -- must not call base class
}

void COleClientItem::OnGetItemPosition(CRect& /*rPosition*/)
{
	// default does nothing
}

void COleClientItem::OnGetClipRect(CRect& rClipRect)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(&rClipRect, sizeof(RECT)));

	// default clips rectClip to client area of the active view
	ASSERT_VALID(m_pView);
	m_pView->GetClientRect(&rClipRect);
}

void COleClientItem::OnShowItem()
{
	ASSERT_VALID(this);

	CDocument* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// attempt to use m_pView set during activation
	CView* pView = m_pView;
	if (pView == NULL)
	{
		// otherwise, find the first view of this document
		POSITION pos = pDoc->GetFirstViewPosition();
		if (pos == NULL || (pView = pDoc->GetNextView(pos)) == NULL)
			return;
	}

	CFrameWnd* pFrameWnd = pView->GetParentFrame();
	if (pFrameWnd != NULL)
	{
		// activate frame holding view
		pFrameWnd->ActivateFrame();
		pFrameWnd->OnUpdateFrameTitle(TRUE);

		// activate app frame if necessary
		pFrameWnd = pFrameWnd->GetParentFrame();
		if (pFrameWnd != NULL)
		{
			ASSERT_KINDOF(CFrameWnd, pFrameWnd);
			pFrameWnd->ActivateFrame();
			pFrameWnd->OnUpdateFrameTitle(TRUE);
		}
	}

	if (!pDoc->GetPathName().IsEmpty())
	{
		// user is also in control of the application, when a file-based
		//  document becomes visible.
		AfxOleSetUserCtrl(TRUE);
	}
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem - attributes

void COleClientItem::GetClassID(CLSID* pClassID) const
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(AfxIsValidAddress(pClassID, sizeof(CLSID)));

	if (m_lpObject->GetUserClassID(pClassID) != S_OK)
		*pClassID = CLSID_NULL;
}

BOOL COleClientItem::GetExtent(LPSIZE lpSize, DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(AfxIsValidAddress(lpSize, sizeof(CSize)));

	// use current default aspect if specific one not specified
	if (nDrawAspect == -1)
		nDrawAspect = m_nDrawAspect;

	// get the extent
	m_scLast = m_lpObject->GetExtent(nDrawAspect, lpSize);
	return m_scLast == S_OK;
}

BOOL COleClientItem::GetCachedExtent(LPSIZE lpSize, DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);
	ASSERT(m_lpViewObject != NULL);
	ASSERT(AfxIsValidAddress(lpSize, sizeof(CSize)));

	// use current default aspect if specific one not specified
	if (nDrawAspect == -1)
		nDrawAspect = m_nDrawAspect;

	COleDocument* pDoc = (COleDocument*)GetDocument();
	ASSERT_VALID(pDoc);

	// get the extent
	m_scLast = m_lpViewObject->GetExtent(nDrawAspect, -1, pDoc->m_ptd, lpSize);
	return m_scLast == S_OK;
}

BOOL COleClientItem::SetIconicMetafile(HGLOBAL hMetaPict)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// get IOleCache interface
	LPOLECACHE lpOleCache = QUERYINTERFACE(m_lpObject, IOleCache);
	if (lpOleCache == NULL)
	{
		TRACE0("Warning: object does not support IOleCache interface.\n");
		return FALSE;
	}
	ASSERT(lpOleCache != NULL);

	// new cache is for CF_METAFILEPICT, DVASPECT_ICON
	FORMATETC formatEtc;
	formatEtc.cfFormat = CF_METAFILEPICT;
	formatEtc.ptd = NULL;
	formatEtc.dwAspect = DVASPECT_ICON;
	formatEtc.lindex = -1;
	formatEtc.tymed = TYMED_MFPICT;

	// setup the cache so iconic aspect is now included
	DWORD dwConnection;
	SCODE sc = lpOleCache->Cache(&formatEtc,
		ADVF_NODATA|ADVF_PRIMEFIRST|ADVF_ONLYONCE, &dwConnection);
	if (FAILED(sc))
	{
		lpOleCache->Release();
		return FALSE;
	}

	// set data if iconic image provided
	if (hMetaPict != NULL)
	{
		STGMEDIUM stgMedium;
		stgMedium.tymed = TYMED_MFPICT;
		stgMedium.hGlobal = hMetaPict;
		stgMedium.pUnkForRelease = NULL;

		sc = lpOleCache->SetData(&formatEtc, &stgMedium, FALSE);
		if (FAILED(sc))
		{
			lpOleCache->Release();
			return FALSE;
		}
	}
	lpOleCache->Release();

	return TRUE;
}

HGLOBAL COleClientItem::GetIconicMetafile()
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// get IDataObject interface
	LPDATAOBJECT lpDataObject = QUERYINTERFACE(m_lpObject, IDataObject);
	ASSERT(lpDataObject != NULL);

	// cache is for CF_METAFILEPICT, DVASPECT_ICON
	FORMATETC formatEtc;
	formatEtc.cfFormat = CF_METAFILEPICT;
	formatEtc.ptd = NULL;
	formatEtc.dwAspect = DVASPECT_ICON;
	formatEtc.lindex = -1;
	formatEtc.tymed = TYMED_MFPICT;

	// attempt to get the icon picture
	STGMEDIUM stgMedium;
	if (lpDataObject->GetData(&formatEtc, &stgMedium) != S_OK)
	{
		lpDataObject->Release();

		// no current picture, attempt to get from class ID
		CLSID clsid;
		if (m_lpObject->GetUserClassID(&clsid) != S_OK)
			return NULL;

		TCHAR szTemp[_MAX_PATH];
		LPTSTR lpszLabel = NULL;
		if (GetType() == OT_LINK)
		{
			// it is a link, attempt to get link name
			LPOLELINK lpOleLink = QUERYINTERFACE(m_lpObject, IOleLink);
			if (lpOleLink != NULL)
			{
				LPOLESTR lpszDisplayName = NULL;
				lpOleLink->GetSourceDisplayName(&lpszDisplayName);
				if (lpszDisplayName != NULL)
				{
					szTemp[0] = 0;
					AfxGetFileTitle(OLE2CT(lpszDisplayName), szTemp, _countof(szTemp));
					if (szTemp[0] != '\0')
						lpszLabel = szTemp;
					CoTaskMemFree(lpszDisplayName);
				}
				lpOleLink->Release();
			}
		}

		HGLOBAL hMetaPict = OleGetIconOfClass(clsid, T2OLE(lpszLabel), lpszLabel == NULL);
		if (hMetaPict != NULL)
		{
			// cache it for later GetData (or drawing)
			SetIconicMetafile(hMetaPict);
			return hMetaPict;
		}
		return NULL;
	}
	lpDataObject->Release();

	// can't handle data where punkForRelease is set
	if (stgMedium.pUnkForRelease != NULL)
	{
		::ReleaseStgMedium(&stgMedium);
		return NULL;
	}
	ASSERT(stgMedium.tymed == TYMED_MFPICT);
	ASSERT(stgMedium.hGlobal != NULL);

	return stgMedium.hGlobal;   // return HGLOBAL to METAFILEPICT
}

void COleClientItem::SetDrawAspect(DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);

	// prime iconic cache (in case object has never displayed iconic)
	if (nDrawAspect == DVASPECT_ICON)
	{
		SetIconicMetafile(NULL);    // allow object to provide own icon
		HGLOBAL hMetaPict = GetIconicMetafile();
		_AfxDeleteMetafilePict(hMetaPict);
	}

	// Note: the aspect you are setting may be uncached and therefore blank.
	//  To make sure it is cached use SetIconPicture or use IOleCache to
	//  set the cache yourself.
	m_nDrawAspect = nDrawAspect;

	// mark document as dirty (m_nDrawAspect is part of persistent state)
	ASSERT_VALID(m_pDocument);
	m_pDocument->SetModifiedFlag();
}

// Helper used to get the DVTARGETDEVICE pointer for the first printer.
BOOL COleClientItem::GetPrintDeviceInfo(
	LPOLECACHE* plpOleCache, DVTARGETDEVICE** pptd, DWORD* pdwConnection)
{
	*plpOleCache = NULL;
	*pptd = NULL;

	// get IOleCache interface
	LPOLECACHE lpOleCache = QUERYINTERFACE(m_lpObject, IOleCache);
	if (lpOleCache == NULL)
	{
		TRACE0("Warning: object does not support IOleCache interface.\n");
		return FALSE;   // no cache -- no possible print device
	}
	ASSERT(lpOleCache != NULL);

	// get enumerator for the cache
	LPENUMSTATDATA lpEnumSTATDATA;
	if (lpOleCache->EnumCache(&lpEnumSTATDATA) != S_OK || lpEnumSTATDATA == NULL)
	{
		lpOleCache->Release();
		return FALSE;
	}
	// enumerate entries in the cache (look for one with ptd != NULL)
	STATDATA statData;
	while (lpEnumSTATDATA->Next(1, &statData, NULL) == S_OK)
	{
		ASSERT(statData.pAdvSink == NULL);

		// return first non-NULL ptd (we assume this is a printer cache)
		if (statData.formatetc.ptd != NULL)
		{
			if (pdwConnection != NULL)
				*pdwConnection = statData.dwConnection;
			*pptd = statData.formatetc.ptd;
			lpEnumSTATDATA->Release();

			*plpOleCache = lpOleCache;
			return TRUE;    // Note: lpOleCache pointer is still alive
		}
	}
	// release interfaces
	lpEnumSTATDATA->Release();
	lpOleCache->Release();
	return FALSE;   // data not found
}

void COleClientItem::AttachDataObject(COleDataObject& rDataObject) const
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	// get the IDataObject interface for the item
	LPDATAOBJECT lpDataObject = QUERYINTERFACE(m_lpObject, IDataObject);
	ASSERT(lpDataObject != NULL);

	// return it by attaching it
	rDataObject.Attach(lpDataObject, TRUE);
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem - general operations

BOOL COleClientItem::Draw(CDC* pDC, LPCRECT lpBounds, DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpBounds, sizeof(RECT), FALSE));
	ASSERT_VALID(pDC);

	if (m_lpObject == NULL || m_lpViewObject == NULL)
		return FALSE;   // partially created COleClientItem object

	// use current draw aspect if aspect is -1 (default)
	if (nDrawAspect == -1)
		nDrawAspect = m_nDrawAspect;

	// convert RECT lpBounds to RECTL rclBounds
	RECTL rclBounds;
	rclBounds.left = lpBounds->left;
	rclBounds.top = lpBounds->top;
	rclBounds.right = lpBounds->right;
	rclBounds.bottom = lpBounds->bottom;

	// get RECTL describing window extents and origin
	RECTL rclWBounds;
	CPoint ptOrg = pDC->GetWindowOrg();
	CSize sizeExt = pDC->GetWindowExt();
	rclWBounds.left = ptOrg.x;
	rclWBounds.top = ptOrg.y;
	rclWBounds.right = sizeExt.cx;
	rclWBounds.bottom = sizeExt.cy;

	// get target device to use for draw
	COleDocument* pDoc = GetDocument();
	const DVTARGETDEVICE* ptd = NULL;
	HDC hdcTarget = NULL;
	if (pDC->IsPrinting() && pDoc->m_ptd != NULL)
	{
		ptd = pDoc->m_ptd;
		hdcTarget = pDC->m_hAttribDC;
	}

	// attempt draw with target device
	SCODE sc = m_lpViewObject->Draw(nDrawAspect, -1, NULL,
		(DVTARGETDEVICE*)ptd, hdcTarget, pDC->m_hDC,
		&rclBounds, &rclWBounds, NULL, 0);
	if (ptd != NULL && sc == OLE_E_BLANK)
	{
		// attempt draw without target device
		sc = m_lpViewObject->Draw(nDrawAspect, -1, NULL,
			NULL, NULL, pDC->m_hDC,
			&rclBounds, &rclWBounds, NULL, 0);
	}

	if (sc != S_OK && sc == OLE_E_BLANK)
		return FALSE;   // return FALSE if the object is blank

	CheckGeneral(sc);   // otherwise, may throw exception on error
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem clipboard helpers

void COleClientItem::GetEmbeddedItemData(LPSTGMEDIUM lpStgMedium)
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
	lpLockBytes->Release();

	// save the object into the storage
	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);
	sc = ::OleSave(lpPersistStorage, lpStorage, FALSE);
	lpPersistStorage->SaveCompleted(NULL);
	lpPersistStorage->Release();
	if (sc != S_OK)
	{
		VERIFY(lpStorage->Release() == 0);
		AfxThrowOleException(sc);
	}

	// add it to the data source
	lpStgMedium->tymed = TYMED_ISTORAGE;
	lpStgMedium->pstg = lpStorage;
	lpStgMedium->pUnkForRelease = NULL;
}

void COleClientItem::AddCachedData(COleDataSource* pDataSource)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDataSource);

	// get IOleCache interface
	LPOLECACHE lpOleCache = QUERYINTERFACE(m_lpObject, IOleCache);
	if (lpOleCache == NULL)
	{
		TRACE0("Warning: object does not support IOleCache interface.\n");
		return;
	}
	ASSERT(lpOleCache != NULL);

	// Get IEnumSTATDATA interface for IOleCache
	LPENUMSTATDATA lpEnumSTATDATA;
	if (lpOleCache->EnumCache(&lpEnumSTATDATA) != S_OK || lpEnumSTATDATA == NULL)
	{
		lpOleCache->Release();
		return;
	}

	// get IDataObject interface
	LPDATAOBJECT lpDataObject = QUERYINTERFACE(m_lpObject, IDataObject);
	ASSERT(lpDataObject != NULL);

	// enumerate all of the cached formats
	STATDATA statData;
	while (lpEnumSTATDATA->Next(1, &statData, NULL) == S_OK)
	{
		ASSERT(statData.pAdvSink == NULL);

		// for each format supported, attempt to get copy of the data
		STGMEDIUM stgMedium;
		if (lpDataObject->GetData(&statData.formatetc, &stgMedium) != S_OK)
		{
			// data is not available
			CoTaskMemFree(statData.formatetc.ptd);
		}
		else if (stgMedium.pUnkForRelease != NULL)
		{
			// don't cache data with pUnkForRelease != NULL
			::ReleaseStgMedium(&stgMedium);
			CoTaskMemFree(statData.formatetc.ptd);
		}
		else
		{
			// format was acceptable -- add it to the clipboard
			pDataSource->CacheData(0, &stgMedium, &statData.formatetc);
		}
	}

	// release interfaces
	lpEnumSTATDATA->Release();
	lpDataObject->Release();
	lpOleCache->Release();
}

BOOL COleClientItem::GetLinkSourceData(LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	LPMONIKER lpMoniker = NULL;
	LPOLELINK lpOleLink = QUERYINTERFACE(m_lpObject, IOleLink);
	if (lpOleLink == NULL)
	{
		// get moniker from client site
		LPOLECLIENTSITE lpClientSite = GetClientSite();
		ASSERT(lpClientSite != NULL);
		SCODE sc = lpClientSite->GetMoniker(OLEGETMONIKER_TEMPFORUSER,
			OLEWHICHMK_OBJFULL, &lpMoniker);
		if (sc != S_OK)
		{
			TRACE0("Warning: unable to get moniker from client site.\n");
			return FALSE;
		}
		ASSERT(lpMoniker != NULL);
	}
	else
	{
		// get moniker from the link object itself
		SCODE sc = lpOleLink->GetSourceMoniker(&lpMoniker);
		lpOleLink->Release();
		if (sc != S_OK)
		{
			TRACE0("Warning: unable to get moniker from link source.\n");
			return FALSE;
		}
		ASSERT(lpMoniker != NULL);
	}

	// create a memory based stream to write the moniker to
	LPSTREAM lpStream;
	if (::CreateStreamOnHGlobal(NULL, TRUE, &lpStream) != S_OK)
	{
		lpMoniker->Release();
		AfxThrowMemoryException();
	}
	ASSERT(lpStream != NULL);

	// write the moniker to the stream, and add it to the clipboard
	SCODE sc = ::OleSaveToStream(lpMoniker, lpStream);
	lpMoniker->Release();
	if (sc != S_OK)
	{
		lpStream->Release();
		AfxThrowOleException(sc);
	}

	// write the class ID of the document to the stream as well
	CLSID clsid;
	sc = m_lpObject->GetUserClassID(&clsid);
	if (sc != S_OK)
	{
		lpStream->Release();
		AfxThrowOleException(sc);
	}
	sc = WriteClassStm(lpStream, clsid);
	if (sc != S_OK)
	{
		lpStream->Release();
		AfxThrowOleException(sc);
	}

	// add it to the data source
	lpStgMedium->tymed = TYMED_ISTREAM;
	lpStgMedium->pstm = lpStream;
	lpStgMedium->pUnkForRelease = NULL;
	return TRUE;
}

void COleClientItem::GetObjectDescriptorData(
	LPPOINT lpOffset, LPSIZE lpSize, LPSTGMEDIUM lpStgMedium)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));
	ASSERT(lpOffset == NULL ||
		AfxIsValidAddress(lpOffset, sizeof(CPoint), FALSE));

	POINTL pointT;
	if (lpOffset != NULL)
	{
		pointT.x = lpOffset->x;
		pointT.y = lpOffset->y;
		((CDC*)NULL)->DPtoHIMETRIC((SIZE*)&pointT);
	}
	else
	{
		pointT.x = 0;
		pointT.y = 0;
	}

	SIZE sizeT;
	if (lpSize != NULL)
	{
		sizeT.cx = lpSize->cx;
		sizeT.cy = lpSize->cy;
		((CDC*)NULL)->DPtoHIMETRIC(&sizeT);
	}
	else
	{
		sizeT.cx = 0;
		sizeT.cy = 0;
	}

	COleDocument* pDoc = GetDocument();

	// get the object descriptor for the IOleObject
	InterlockedIncrement(&m_dwRef);
	HGLOBAL hGlobal = _AfxOleGetObjectDescriptorData(
		m_lpObject, T2COLE(pDoc->GetPathName()), m_nDrawAspect, pointT, &sizeT);
	InterlockedDecrement(&m_dwRef);

	if (hGlobal == NULL)
		AfxThrowMemoryException();

	// setup the STGMEDIUM
	lpStgMedium->tymed = TYMED_HGLOBAL;
	lpStgMedium->hGlobal = hGlobal;
	lpStgMedium->pUnkForRelease = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Embedded COleClientItem operations

void COleClientItem::SetHostNames(LPCTSTR lpszHost, LPCTSTR lpszHostObj)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);
	ASSERT(AfxIsValidString(lpszHost));

	ASSERT(AfxIsValidString(lpszHostObj));
	CheckGeneral(m_lpObject->SetHostNames(T2COLE(lpszHost),
		T2COLE(lpszHostObj)));
}

void COleClientItem::SetExtent(const CSize& size, DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);
	ASSERT(m_lpObject != NULL);

	CheckGeneral(m_lpObject->SetExtent(nDrawAspect, (SIZE*)&size));
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem OLE interface implementation

BEGIN_INTERFACE_MAP(COleClientItem, CDocItem)
	INTERFACE_PART(COleClientItem, IID_IOleClientSite, OleClientSite)
	INTERFACE_PART(COleClientItem, IID_IAdviseSink, AdviseSink)
	INTERFACE_PART(COleClientItem, IID_IOleWindow, OleIPSite)
	INTERFACE_PART(COleClientItem, IID_IOleInPlaceSite, OleIPSite)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// Implementation of IOleClientSite

STDMETHODIMP_(ULONG) COleClientItem::XOleClientSite::AddRef()
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleClientSite)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleClientItem::XOleClientSite::Release()
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleClientSite)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleClientItem::XOleClientSite::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleClientItem, OleClientSite)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleClientItem::XOleClientSite::SaveObject()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleClientSite)
	ASSERT_VALID(pThis);

	LPPERSISTSTORAGE lpPersistStorage =
		QUERYINTERFACE(pThis->m_lpObject, IPersistStorage);
	ASSERT(lpPersistStorage != NULL);
	SCODE sc = S_OK;
	if (lpPersistStorage->IsDirty() == S_OK)
	{
		// S_OK == S_TRUE != S_FALSE, therefore object is dirty!
		sc = ::OleSave(lpPersistStorage, pThis->m_lpStorage, TRUE);
		if (sc == S_OK)
			sc = lpPersistStorage->SaveCompleted(NULL);

		// mark the document as dirty, if save sucessful.
		pThis->m_pDocument->SetModifiedFlag();
	}
	lpPersistStorage->Release();
	return sc;
}

STDMETHODIMP COleClientItem::XOleClientSite::GetMoniker(
	DWORD dwAssign, DWORD dwWhichMoniker, LPMONIKER* ppMoniker)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleClientSite)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	COleDocument* pDoc = pThis->GetDocument();
	ASSERT_VALID(pDoc);
	ASSERT(ppMoniker != NULL);
	*ppMoniker = NULL;

	switch (dwWhichMoniker)
	{
	case OLEWHICHMK_CONTAINER:
		// return the current moniker for the document
		*ppMoniker = pDoc->GetMoniker((OLEGETMONIKER)dwAssign);
		break;

	case OLEWHICHMK_OBJREL:
		{
			if (!pDoc->IsKindOf(RUNTIME_CLASS(COleLinkingDoc)))
				break;

			// don't return relative moniker if no document moniker
			LPMONIKER lpMoniker = pDoc->GetMoniker((OLEGETMONIKER)dwAssign);
			if (lpMoniker == NULL)
				break;
			lpMoniker->Release();

			// relative monikers have to handle assignment correctly
			switch (dwAssign)
			{
			case OLEGETMONIKER_ONLYIFTHERE:
				if (!pThis->m_bMoniker)
					break;  // no moniker assigned, don't return one
				// fall through...

			case OLEGETMONIKER_TEMPFORUSER:
			case OLEGETMONIKER_FORCEASSIGN:
				{
					// create item moniker from item name
					TCHAR szItemName[OLE_MAXITEMNAME];
					pThis->GetItemName(szItemName);
					CreateItemMoniker(OLESTDDELIMOLE, T2COLE(szItemName),
						ppMoniker);

					// notify the object of the assignment
					if (dwAssign != OLEGETMONIKER_TEMPFORUSER &&
						*ppMoniker != NULL && !pThis->m_bMoniker)
					{
						pThis->m_bMoniker = TRUE;
						VERIFY(pThis->m_lpObject->SetMoniker(
							OLEWHICHMK_OBJREL, *ppMoniker) == S_OK);
						ASSERT_VALID(pThis->m_pDocument);
						pThis->m_pDocument->SetModifiedFlag();
					}
				}
				break;

			case OLEGETMONIKER_UNASSIGN:
				pThis->m_bMoniker = FALSE;
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

			// create composite moniker
			if (lpMoniker1 != NULL && lpMoniker2 != NULL)
				::CreateGenericComposite(lpMoniker1, lpMoniker2, ppMoniker);

			// release sub-monikers
			RELEASE(lpMoniker1);
			RELEASE(lpMoniker2);
		}
		break;
	}

	return *ppMoniker != NULL ? S_OK : E_FAIL;
}

STDMETHODIMP COleClientItem::XOleClientSite::GetContainer(
	LPOLECONTAINER* ppContainer)
{
#ifdef _DEBUG
	METHOD_PROLOGUE_EX(COleClientItem, OleClientSite)
#else
	METHOD_PROLOGUE_EX_(COleClientItem, OleClientSite)
#endif

	// return the IOleItemContainer interface in the document
	COleDocument* pDoc = pThis->GetDocument();
	ASSERT_VALID(pDoc);
	*ppContainer = pDoc->GetContainer();

	return *ppContainer != NULL ? S_OK : E_NOINTERFACE;
}

STDMETHODIMP COleClientItem::XOleClientSite::ShowObject()
{
	METHOD_PROLOGUE_EX(COleClientItem, OleClientSite)
	ASSERT_VALID(pThis);

	TRY
	{
		pThis->OnShowItem();
	}
	END_TRY

	return S_OK;
}

STDMETHODIMP COleClientItem::XOleClientSite::OnShowWindow(BOOL fShow)
{
	METHOD_PROLOGUE_EX(COleClientItem, OleClientSite)
	ASSERT_VALID(pThis);

	// ignore this if the item is already in-place
	if (pThis->IsInPlaceActive())
		return S_OK;

	TRY
	{
		// store new state of object -- determines how object may be drawn
		COleClientItem::ItemState uNewState;
		uNewState = fShow ? COleClientItem::openState :
			COleClientItem::loadedState;
		if (uNewState != pThis->m_nItemState)
		{
			pThis->OnChange(OLE_CHANGED_STATE, (DWORD)uNewState);
			pThis->m_nItemState = uNewState;
		}
	}
	END_TRY

	return S_OK;
}

STDMETHODIMP COleClientItem::XOleClientSite::RequestNewObjectLayout()
{
	return E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// Implementation of IAdviseSink

STDMETHODIMP_(ULONG) COleClientItem::XAdviseSink::AddRef()
{
	METHOD_PROLOGUE_EX_(COleClientItem, AdviseSink)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleClientItem::XAdviseSink::Release()
{
	METHOD_PROLOGUE_EX_(COleClientItem, AdviseSink)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleClientItem::XAdviseSink::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleClientItem, AdviseSink)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP_(void) COleClientItem::XAdviseSink::OnDataChange(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX(COleClientItem, AdviseSink)
	ASSERT_VALID(pThis);

	// Only interesting for advanced containers.  Forward it such that
	// containers do not have to implement the entire interface.
	pThis->OnDataChange(lpFormatEtc, lpStgMedium);
}

STDMETHODIMP_(void) COleClientItem::XAdviseSink::OnViewChange(
	DWORD aspects, LONG /*lindex*/)
{
	METHOD_PROLOGUE_EX(COleClientItem, AdviseSink)
	ASSERT_VALID(pThis);

	pThis->OnChange(OLE_CHANGED, (DVASPECT)aspects);
}

STDMETHODIMP_(void) COleClientItem::XAdviseSink::OnRename(
	LPMONIKER /*lpMoniker*/)
{
	// only interesting to the OLE link object.  Containers ignore this.
}

STDMETHODIMP_(void) COleClientItem::XAdviseSink::OnSave()
{
	METHOD_PROLOGUE_EX(COleClientItem, AdviseSink)
	ASSERT_VALID(pThis);

	pThis->OnChange(OLE_SAVED, (DVASPECT)0);
}

STDMETHODIMP_(void) COleClientItem::XAdviseSink::OnClose()
{
	METHOD_PROLOGUE_EX(COleClientItem, AdviseSink)
	ASSERT_VALID(pThis);

	pThis->OnChange(OLE_CLOSED, (DVASPECT)0);
}

/////////////////////////////////////////////////////////////////////////////
// COleClientItem diagnostics

#ifdef _DEBUG
void COleClientItem::AssertValid() const
{
	CDocItem::AssertValid();
	if (m_lpNewStorage != NULL)
		ASSERT(m_bNeedCommit);
	if (m_pView != NULL)
		m_pView->AssertValid();
	if (m_pInPlaceFrame != NULL)
		m_pInPlaceFrame->AssertValid();
	if (m_pInPlaceDoc != NULL)
		m_pInPlaceDoc->AssertValid();
}

void COleClientItem::Dump(CDumpContext& dc) const
{
	CDocItem::Dump(dc);

	// shallow dump
	dc << "m_lpObject = " << (void*)m_lpObject;
	dc << "\nm_dwItemNumber = " << m_dwItemNumber;
	dc << "\nm_nDrawAspect = " << (int)m_nDrawAspect;
	dc << "\nm_scLast = " << m_scLast;
	dc << "\nm_lpStorage = " << m_lpStorage;
	dc << "\nm_lpLockBytes = " << m_lpLockBytes;
	dc << "\nm_dwConnection = " << m_dwConnection;
	dc << "\nm_bLinkUnavail = " << m_bLinkUnavail;
	dc << "\nm_bMoniker = " << m_bMoniker;
	dc << "\nm_lpNewStorage = " << m_lpNewStorage;
	dc << "\nm_bNeedCommit = " << m_bNeedCommit;
	dc << "\nm_nItemState = " << (int)m_nItemState;
	dc << "\nm_pView = " << (void*)m_pView;
	dc << "\nm_dwContainerStyle = " << m_dwContainerStyle;
	dc << "\nm_pInPlaceFrame = " << (void*)m_pInPlaceFrame;
	dc << "\nm_hWndServer = " << m_hWndServer;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

// expand inlines for OLE client APIs
static char _szAfxOleInl[] = "afxole.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxOleInl
#define _AFXOLECLI_INLINE
#define _AFXOLEDOBJ_INLINE
#include "afxole.inl"

#endif //!_AFX_ENABLE_INLINES

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

// IMPLEMENT_DYNAMIC for COleLinkingDoc here for better .OBJ granularity
IMPLEMENT_DYNAMIC(COleLinkingDoc, COleDocument)

/////////////////////////////////////////////////////////////////////////////
