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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#define OLE_MAXNAMESIZE     (256)

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc - enables linking to embeddings (basis for server)

COleLinkingDoc::COleLinkingDoc()
{
	m_dwRegister = 0;
	m_pFactory = NULL;
	m_bVisibleLock = FALSE;
	m_bDeferErrors = FALSE;
	m_pLastException = NULL;
	m_lpMonikerROT = NULL;

	ASSERT_VALID(this);
}

COleLinkingDoc::~COleLinkingDoc()
{
	ASSERT_VALID(this);

	ASSERT(!m_bVisibleLock);

	DisconnectViews();
	ASSERT(m_viewList.IsEmpty());

	Revoke();   // cleanup naming support

	ExternalDisconnect();
}

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc moniker handling

LPMONIKER COleLinkingDoc::GetMoniker(OLEGETMONIKER nAssign)
{
	USES_CONVERSION;

	ASSERT_VALID(this);

	// use base class implementation if no registered moniker
	if (m_strMoniker.IsEmpty())
		return COleDocument::GetMoniker(nAssign);

	// return file moniker based on current path name
	LPMONIKER lpMoniker;
	CreateFileMoniker(T2COLE(m_strMoniker), &lpMoniker);
	return lpMoniker;
}

BOOL COleLinkingDoc::Register(COleObjectFactory* pFactory, LPCTSTR lpszPathName)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(pFactory == NULL ||
		AfxIsValidAddress(pFactory, sizeof(COleObjectFactory)));
	ASSERT(lpszPathName == NULL || AfxIsValidString(lpszPathName));
	ASSERT(m_dwRegister == 0);

	// attach the document to the server
	ASSERT(m_pFactory == NULL || m_pFactory == pFactory);
	m_pFactory = pFactory;

	BOOL bResult = TRUE;

	// create file moniker based on path name
	RELEASE(m_lpMonikerROT);
	m_strMoniker.Empty();
	if (lpszPathName != NULL)
	{
		if (CreateFileMoniker(T2COLE(lpszPathName), &m_lpMonikerROT) != S_OK)
			bResult = FALSE;
	}

	// register file moniker as running
	if (m_lpMonikerROT != NULL)
	{
		// see if the object is already running in the ROT
		LPRUNNINGOBJECTTABLE lpROT = NULL;
		VERIFY(GetRunningObjectTable(0, &lpROT) == S_OK);
		ASSERT(lpROT != NULL);
		LPUNKNOWN lpUnk;
		if (lpROT->GetObject(m_lpMonikerROT, &lpUnk) == S_OK)
		{
			// fatal error -- can't register same moniker twice!
			lpUnk->Release();
			RELEASE(m_lpMonikerROT);
			return FALSE;
		}
		// not already running -- so ok to attempt registration
		SCODE sc = lpROT->Register(NULL, (LPUNKNOWN)
			GetInterface(&IID_IUnknown), m_lpMonikerROT, &m_dwRegister);
		lpROT->Release();
		m_strMoniker = lpszPathName;
		if (sc != S_OK)
			bResult = FALSE;
	}

	// update all objects with new moniker
	POSITION pos = GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = GetNextClientItem(pos)) != NULL)
	{
		if (pItem->m_bMoniker)
		{
			ASSERT(pItem->m_lpObject != NULL);
			pItem->m_lpObject->SetMoniker(OLEWHICHMK_CONTAINER,
				m_lpMonikerROT);
		}
	}

	return bResult;
}

void COleLinkingDoc::Revoke()
{
	ASSERT_VALID(this);

	// revoke current registration
	if (m_dwRegister != 0)
	{
		LPRUNNINGOBJECTTABLE lpROT = NULL;
		GetRunningObjectTable(0, &lpROT);
		if (lpROT != NULL)
		{
			lpROT->Revoke(m_dwRegister);
			lpROT->Release();
		}
		m_dwRegister = 0;
	}
	RELEASE(m_lpMonikerROT);
	m_strMoniker = _T("");
}

BOOL COleLinkingDoc::OnNewDocument()
{
	ASSERT_VALID(this);

	Revoke();
	RegisterIfServerAttached(NULL, TRUE);

	if (!COleDocument::OnNewDocument())
		return FALSE;

	AfxOleSetUserCtrl(TRUE);

	return TRUE;
}

BOOL COleLinkingDoc::OnOpenDocument(LPCTSTR lpszPathName)
{
	ASSERT_VALID(this);

	// always register the document before opening it
	Revoke();
	if (!RegisterIfServerAttached(lpszPathName, FALSE))
	{
		// always output a trace (it is just an FYI -- not generally fatal)
		TRACE1("Warning: Unable to register moniker '%s' as running\n", lpszPathName);
	}

	if (!COleDocument::OnOpenDocument(lpszPathName))
	{
		Revoke();
		return FALSE;
	}

	// if the app was started only to print, don't set user control

	CWinApp* pApp = AfxGetApp();
	ASSERT(pApp != NULL);
	if (pApp->m_pCmdInfo == NULL ||
		(pApp->m_pCmdInfo->m_nShellCommand != CCommandLineInfo::FileDDE &&
		 pApp->m_pCmdInfo->m_nShellCommand != CCommandLineInfo::FilePrint))
	{
		AfxOleSetUserCtrl(TRUE);
	}

	return TRUE;
}

BOOL COleLinkingDoc::OnSaveDocument(LPCTSTR lpszPathName)
{
	ASSERT_VALID(this);

	BOOL bRemember = m_bRemember;
	if (!COleDocument::OnSaveDocument(lpszPathName))
		return FALSE;

	if (bRemember && (m_strMoniker != lpszPathName))
	{
		// update the moniker/registration since the name has changed
		Revoke();
		RegisterIfServerAttached(lpszPathName, TRUE);
	}
	return TRUE;
}

void COleLinkingDoc::OnCloseDocument()
{
	InternalAddRef();   // protect document during shutdown

	// update lock count before sending notifications
	UpdateVisibleLock(FALSE, FALSE);

	Revoke();   // cleanup naming support

	// remove visible lock if present
	if (m_bVisibleLock)
	{
		m_bVisibleLock = FALSE;
		LockExternal(FALSE, FALSE);
	}

	// cleanup the document but don't delete yet
	BOOL bAutoDelete = m_bAutoDelete;
	m_bAutoDelete = FALSE;
	COleDocument::OnCloseDocument();
	ASSERT_VALID(this);

	// remove extra reference count and destroy
	InterlockedDecrement(&m_dwRef);
	if (bAutoDelete)
		delete this;    // now safe to destroy document
}

void COleLinkingDoc::UpdateVisibleLock(BOOL bVisible, BOOL bRemoveRefs)
{
	ASSERT_VALID(this);

	if (bVisible != m_bVisibleLock)
	{
		InternalAddRef();   // make sure document is stable
		m_bVisibleLock = bVisible;
		LockExternal(bVisible, bRemoveRefs);
		InternalRelease();  // may Release the document!
	}
}

void COleLinkingDoc::OnShowViews(BOOL bVisible)
{
	if (bVisible)
		UpdateVisibleLock(bVisible, TRUE);
}

void COleLinkingDoc::SaveToStorage(CObject* pObject)
{
	ASSERT_VALID(this);
	if (pObject != NULL)
		ASSERT_VALID(pObject);

	// write the classID of the application to the root storage
	if (m_pFactory != NULL)
	{
		ASSERT(m_lpRootStg != NULL);
		WriteClassStg(m_lpRootStg, m_pFactory->GetClassID());
	}
	COleDocument::SaveToStorage(pObject);
}

BOOL COleLinkingDoc::RegisterIfServerAttached(LPCTSTR lpszPathName, BOOL bMessage)
{
	ASSERT_VALID(this);
	ASSERT(lpszPathName == NULL || AfxIsValidString(lpszPathName));

	CDocTemplate* pTemplate = GetDocTemplate();
	ASSERT_VALID(pTemplate);

	COleObjectFactory* pFactory =
		(COleObjectFactory*)pTemplate->m_pAttachedFactory;
	if (pFactory != NULL)
	{
		// always attach the document to the server at this time
		ASSERT_KINDOF(COleObjectFactory, pFactory);
		m_pFactory = pFactory;

		// register with OLE Server
		if (!Register(pFactory, lpszPathName))
		{
			if (bMessage)
			{
				// only report error when message box allowed
				ReportSaveLoadException(lpszPathName, NULL, FALSE,
					AFX_IDP_FAILED_TO_NOTIFY);
			}
			return FALSE;
		}
	}
	return TRUE;
}

LPOLEITEMCONTAINER COleLinkingDoc::GetContainer()
{
	ASSERT_VALID(this);

	// get the IOleItemContainer interface via QueryInterface
	LPOLEITEMCONTAINER lpContainer;
	InternalQueryInterface(&IID_IOleItemContainer, (LPLP)&lpContainer);
	return lpContainer;
}

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc default implementation

COleServerItem* COleLinkingDoc::OnGetLinkedItem(LPCTSTR /*lpszItemName*/)
{
	ASSERT_VALID(this);

	// default implementation is in COleServerDoc
	return NULL;
}

COleClientItem* COleLinkingDoc::OnFindEmbeddedItem(LPCTSTR lpszItemName)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszItemName));

	// default implementation walks list of client items looking for
	//  a case sensitive match

	POSITION pos = GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = GetNextClientItem(pos)) != NULL)
	{
		// a client item is running if there is a match in name
		//  and the m_lpObject is also running.
		TCHAR szItemName[OLE_MAXITEMNAME];
		pItem->GetItemName(szItemName);
		if (lstrcmp(szItemName, lpszItemName) == 0)
			return pItem;
	}
#ifdef _DEBUG
	if (afxTraceFlags & traceOle)
	{
		TRACE0("Warning: default COleLinkingDoc::OnFindEmbeddedItem\n");
		TRACE1("\timplementation failed to find item '%s'.\n", lpszItemName);
	}
#endif
	return NULL;    // no matching item found
}

void COleLinkingDoc::LockExternal(BOOL bLock, BOOL bRemoveRefs)
{
	// when an item binding is successful, the original document
	//  is released.  To keep it alive and the RPC stubs that make
	//  it available to the external world (via the running object
	//  table), we need to place a lock on it.

	// a lock created with CoLockObjectExternal adds a reference
	//  to the object itself (with IUnknown::AddRef) as well
	//  as keeping the RPC stub alive.

	::CoLockObjectExternal((LPUNKNOWN)GetInterface(&IID_IUnknown),
		bLock, bRemoveRefs);

	if (bLock)
	{
		// avoid "dead" objects in the running object table (ROT), by
		//  re-registering this object in the ROT.
		if (!m_strPathName.IsEmpty())
		{
			Revoke();
			RegisterIfServerAttached(m_strPathName, FALSE);
		}
	}
}

void COleLinkingDoc::ReportSaveLoadException(LPCTSTR lpszPathName,
	CException* e, BOOL bSaving, UINT nIDPDefault)
{
	// watch out for special mode
	if (m_bDeferErrors)
	{
		// Note: CException::Delete does not treat m_bAutoDelete as a
		// traditional  BOOL. Only if it is greater than zero does it
		// take on a TRUE quality.  (that is, all tests are for
		// m_bAutoDelete > 0).  So, if m_bAutoDelete is already "true"
		// (1) this will make it false, and if it is already "false"
		//  it is still considered "false".  Valid values for
		// m_bAutoDelete are thus negative, 0, and 1.  Values greater
		// than 1, although not explicitly asserted in CException,
		// would be invalid.  In short, by using increment  and
		// decrement operations, we enable this to work with both
		// self-deleting and non-self-deleting CException classes.

		--e->m_bAutoDelete;

		// save the exception for later
		m_pLastException = e;
		return;
	}

	// otherwise, just call base class
	COleDocument::ReportSaveLoadException(lpszPathName, e, bSaving,
		nIDPDefault);
}

SCODE COleLinkingDoc::EndDeferErrors(SCODE sc)
{
	ASSERT(m_bDeferErrors != 0);
	--m_bDeferErrors;
	if (m_pLastException != NULL)
	{
		ASSERT_VALID(m_pLastException);
		if (sc == S_OK)
			sc = COleException::Process(m_pLastException);

		// Note: See note above in ReportSaveLoadException for
		// a comment regarding the special treatment of m_bAutoDelete.

		++m_pLastException->m_bAutoDelete;

		// now get rid of the exception that we saved
		m_pLastException->Delete();
		m_pLastException = NULL;
	}
	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc OLE interface implementation

BEGIN_INTERFACE_MAP(COleLinkingDoc, COleDocument)
	INTERFACE_PART(COleLinkingDoc, IID_IPersist, PersistFile)
	INTERFACE_PART(COleLinkingDoc, IID_IPersistFile, PersistFile)
	INTERFACE_PART(COleLinkingDoc, IID_IParseDisplayName, OleItemContainer)
	INTERFACE_PART(COleLinkingDoc, IID_IOleContainer, OleItemContainer)
	INTERFACE_PART(COleLinkingDoc, IID_IOleItemContainer, OleItemContainer)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc::XPersistFile implementation

STDMETHODIMP_(ULONG) COleLinkingDoc::XPersistFile::AddRef()
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, PersistFile)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleLinkingDoc::XPersistFile::Release()
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, PersistFile)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleLinkingDoc::XPersistFile::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, PersistFile)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleLinkingDoc::XPersistFile::GetClassID(LPCLSID lpClassID)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, PersistFile)

	// this is sometimes called for documents not attached to servers!
	if (pThis->m_pFactory == NULL)
	{
		*lpClassID = CLSID_NULL;
		return E_FAIL;
	}

	// get the class ID from the connected server object
	ASSERT_VALID(pThis->m_pFactory);
	*lpClassID = pThis->m_pFactory->GetClassID();
	return S_OK;
}

STDMETHODIMP COleLinkingDoc::XPersistFile::IsDirty()
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, PersistFile)
	return pThis->IsModified() ? S_OK : S_FALSE;
}

STDMETHODIMP COleLinkingDoc::XPersistFile::Load(
	LPCOLESTR lpszFileName, DWORD /*dwMode*/)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, PersistFile)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	CString strFileName;
	SCODE sc = E_FAIL;
	pThis->BeginDeferErrors();
	LPCTSTR lpszFileNameT = OLE2CT(lpszFileName);
	TRY
	{
		BOOL bUserCtrl = AfxOleGetUserCtrl();

		// delegate to file-based Open implementation
		if (!pThis->OnOpenDocument(lpszFileNameT))
		{
			AfxOleSetUserCtrl(bUserCtrl);
			return sc;
		}
		pThis->SendInitialUpdate();

		// set the path name, but don't add to MRU list
		pThis->SetPathName(lpszFileNameT, FALSE);
		AfxOleSetUserCtrl(bUserCtrl);

		sc = S_OK;
	}
	END_TRY
	sc = pThis->EndDeferErrors(sc);

	ASSERT_VALID(pThis);
	return sc;
}

STDMETHODIMP COleLinkingDoc::XPersistFile::Save(
	LPCOLESTR lpszFileName, BOOL fRemember)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, PersistFile)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	CString strFileName;
	SCODE sc = E_FAIL;
	pThis->BeginDeferErrors();
	TRY
	{
		// delegate to file-based Save/Save As implementation
		ASSERT(pThis->m_bRemember);
		pThis->m_bRemember = fRemember;
		pThis->OnSaveDocument(OLE2CT(lpszFileName));
		sc = S_OK;
	}
	END_TRY
	sc = pThis->EndDeferErrors(sc);

	ASSERT_VALID(pThis);
	return sc;
}

STDMETHODIMP COleLinkingDoc::XPersistFile::SaveCompleted(LPCOLESTR lpszFileName)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, PersistFile)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	TRY
	{
		// set the path name, but don't add to MRU list
		pThis->SetPathName(OLE2CT(lpszFileName), FALSE);
	}
	END_TRY

	ASSERT_VALID(pThis);
	return S_OK;
}

STDMETHODIMP COleLinkingDoc::XPersistFile::GetCurFile(LPOLESTR* lplpszFileName)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, PersistFile)

	*lplpszFileName = NULL;

	// use title if no document
	LPCTSTR lpszResult;
	if (pThis->m_strPathName.IsEmpty())
		lpszResult = pThis->m_strTitle;
	else
		lpszResult = pThis->m_strPathName;
	ASSERT(lpszResult != NULL);

	// allocate memory for the file name
	*lplpszFileName = AfxAllocTaskOleString(lpszResult);
	if (*lplpszFileName == NULL)
		return E_OUTOFMEMORY;

	ASSERT_VALID(pThis);
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// Implementation of IOleItemContainer
//  (supports linking to embeddings and linking to pseudo-objects)

STDMETHODIMP_(ULONG) COleLinkingDoc::XOleItemContainer::AddRef()
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, OleItemContainer)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleLinkingDoc::XOleItemContainer::Release()
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, OleItemContainer)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, OleItemContainer)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::EnumObjects(
	DWORD /*grfFlags*/, LPENUMUNKNOWN* ppEnumUnknown)
{
	*ppEnumUnknown = NULL;
	return E_NOTIMPL;
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::ParseDisplayName(LPBC lpbc,
	LPOLESTR lpszDisplayName, ULONG* cchEaten, LPMONIKER* ppMoniker)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, OleItemContainer)

	USES_CONVERSION;

	// reset all OUT parameters
	*ppMoniker = NULL;

	TCHAR szItemName[OLE_MAXNAMESIZE];
	LPTSTR lpszDest = szItemName;
	LPCTSTR lpszSrc = OLE2CT(lpszDisplayName);

	// skip leading delimiters
	int cEaten = 0;
	while (*lpszSrc != '\0' && (*lpszSrc == '\\' || *lpszSrc == '/' ||
		*lpszSrc == ':' || *lpszSrc == '!' || *lpszSrc == '['))
	{
		if (_istlead(*lpszSrc))
			++lpszSrc, ++cEaten;
		++lpszSrc;
		++cEaten;
	}

	// parse next token in szItemName
	while (*lpszSrc != '\0' && *lpszSrc != '\\' && *lpszSrc != '/' &&
		*lpszSrc != ':' && *lpszSrc != '!' && *lpszSrc != '[' &&
		cEaten < OLE_MAXNAMESIZE-1)
	{
		if (_istlead(*lpszSrc))
			*lpszDest++ = *lpszSrc++, ++cEaten;
		*lpszDest++ = *lpszSrc++;
		++cEaten;
	}
	*cchEaten = cEaten;
	*lpszDest = 0;

	// attempt to get the object
	LPUNKNOWN lpUnknown;
	SCODE sc = GetObject(T2OLE(szItemName), BINDSPEED_INDEFINITE, lpbc,
		IID_IUnknown, (LPLP)&lpUnknown);
	if (sc != S_OK)
		return sc;

	// item name found -- create item moniker for it
	lpUnknown->Release();
	return CreateItemMoniker(OLESTDDELIMOLE, T2COLE(szItemName), ppMoniker);
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::LockContainer(BOOL fLock)
{
	METHOD_PROLOGUE_EX_(COleLinkingDoc, OleItemContainer)

	pThis->LockExternal(fLock, TRUE);
	return S_OK;
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::GetObject(
	LPOLESTR lpszItem, DWORD dwSpeedNeeded, LPBINDCTX /*pbc*/, REFIID riid,
	LPVOID* ppvObject)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, OleItemContainer)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	*ppvObject = NULL;

	SCODE sc = MK_E_NOOBJECT;
	TRY
	{
		LPCTSTR lpszItemT = OLE2CT(lpszItem);
		// check for link to embedding
		COleClientItem* pClientItem = pThis->OnFindEmbeddedItem(lpszItemT);
		if (pClientItem != NULL)
		{
			ASSERT_VALID(pClientItem);
			sc = S_OK;

			// item found -- make sure it is running
			if (!::OleIsRunning(pClientItem->m_lpObject))
			{
				// should not run the object if bind-speed is immediate
				if (dwSpeedNeeded != BINDSPEED_INDEFINITE)
					sc = MK_E_EXCEEDEDDEADLINE;
				else
				{
					// bind speed is not immediate -- so run the object
					sc = OleRun(pClientItem->m_lpObject);
				}
			}

			if (sc == S_OK)
			{
				// return the object with appropriate interface
				sc = pClientItem->m_lpObject->QueryInterface(riid, ppvObject);
			}
		}
		else
		{
			// check for link to pseudo object
			COleServerItem* pServerItem = pThis->OnGetLinkedItem(lpszItemT);
			if (pServerItem != NULL)
			{
				if (!pServerItem->m_bNeedUnlock)
				{
					// when a link is bound, the document must be kept alive
					pThis->LockExternal(TRUE, FALSE);
					pServerItem->m_bNeedUnlock = TRUE;
				}

				// matching item found -- query for the requested interface
				sc = pServerItem->ExternalQueryInterface(&riid, ppvObject);
			}
		}
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::GetObjectStorage(
	LPOLESTR lpszItem, LPBINDCTX /*pbc*/, REFIID riid, LPVOID* ppvStorage)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, OleItemContainer)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	*ppvStorage = NULL;

	// only IStorage is supported
	if (riid != IID_IStorage)
		return E_UNEXPECTED;

	// check for link to embedding
	COleClientItem* pClientItem = pThis->OnFindEmbeddedItem(OLE2CT(lpszItem));
	if (pClientItem != NULL)
	{
		ASSERT_VALID(pClientItem);

		// if object has no storage, can't return it!
		if (pClientItem->m_lpStorage != NULL)
		{
			// found matching item -- return the storage
			*ppvStorage = pClientItem->m_lpStorage;
			pClientItem->m_lpStorage->AddRef();
			return S_OK;
		}
	}
	return MK_E_NOSTORAGE;
}

STDMETHODIMP COleLinkingDoc::XOleItemContainer::IsRunning(LPOLESTR lpszItem)
{
	METHOD_PROLOGUE_EX(COleLinkingDoc, OleItemContainer)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	// check for link to embedding
	LPCTSTR lpszItemT = OLE2CT(lpszItem);
	COleClientItem* pClientItem = pThis->OnFindEmbeddedItem(lpszItemT);
	if (pClientItem != NULL)
	{
		ASSERT_VALID(pClientItem);
		if (!::OleIsRunning(pClientItem->m_lpObject))
			return S_FALSE;

		return S_OK; // item is in document and is running
	}

	// check for link to pseudo object
	SCODE sc = MK_E_NOOBJECT;
	TRY
	{
		COleServerItem* pServerItem = pThis->OnGetLinkedItem(lpszItemT);
		if (pServerItem != NULL)
			sc = S_OK;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleLinkingDoc diagnostics

#ifdef _DEBUG
void COleLinkingDoc::AssertValid() const
{
	COleDocument::AssertValid();
	if (m_pFactory != NULL)
		m_pFactory->AssertValid();
}

void COleLinkingDoc::Dump(CDumpContext& dc) const
{
	COleDocument::Dump(dc);

	dc << "\nm_dwRegister = " << m_dwRegister;
	dc << "\nm_bVisibleLock = " << m_bVisibleLock;
	if (m_pFactory != NULL)
		dc << "\nwith factory: " << m_pFactory;
	else
		dc << "\nwith no factory";

	dc << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
