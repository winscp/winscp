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

#ifdef AFX_OLE2_SEG
#pragma code_seg(AFX_OLE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

UINT CALLBACK
AfxOleHookProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

////////////////////////////////////////////////////////////////////////////
// implementation helpers

BOOL AFXAPI _AfxOlePropertiesEnabled()
{
	// edit properties is enabled if there is a handler
	//  for ID_OLE_EDIT_PROPERTIES
	AFX_CMDHANDLERINFO info;

	// check main window first
	CWnd* pWnd = AfxGetMainWnd();
	if (pWnd != NULL && pWnd->OnCmdMsg(ID_OLE_EDIT_PROPERTIES, CN_COMMAND, NULL, &info))
		return TRUE;

	// check app last
	return AfxGetApp()->OnCmdMsg(ID_OLE_EDIT_PROPERTIES, CN_COMMAND, NULL, &info);
}

SCODE _AfxParseDisplayName(LPMONIKER lpmk, LPBC lpbc, LPTSTR lpszRemainder,
	ULONG* cchEaten, LPMONIKER* plpmkOut)
{
	USES_CONVERSION;

	ASSERT(lpmk != NULL);
	ASSERT(AfxIsValidString(lpszRemainder));
	ASSERT(cchEaten != NULL);
	ASSERT(plpmkOut != NULL);

	SCODE sc;
	if (lpbc != NULL)
	{
		// ask moniker to parse the display name itself
		sc = lpmk->ParseDisplayName(lpbc, NULL, T2OLE(lpszRemainder), cchEaten,
			plpmkOut);
	}
	else
	{
		// skip leading delimiters
		int cEaten = 0;
		LPTSTR lpszSrc = lpszRemainder;
		while (*lpszSrc != '\0' && (*lpszSrc == '\\' || *lpszSrc == '/' ||
			*lpszSrc == ':' || *lpszSrc == '!' || *lpszSrc == '['))
		{
			if (_istlead(*lpszSrc))
				++lpszSrc, ++cEaten;
			++lpszSrc;
			++cEaten;
		}

		// parse next token in lpszRemainder
		TCHAR szItemName[_MAX_PATH];
		LPTSTR lpszDest = szItemName;
		while (*lpszSrc != '\0' && *lpszSrc != '\\' && *lpszSrc != '/' &&
			*lpszSrc != ':' && *lpszSrc != '!' && *lpszSrc != '[' &&
			cEaten < _MAX_PATH-1)
		{
			if (_istlead(*lpszSrc))
				*lpszDest++ = *lpszSrc++, ++cEaten;
			*lpszDest++ = *lpszSrc++;
			++cEaten;
		}
		*cchEaten = cEaten;
		sc = CreateItemMoniker(OLESTDDELIMOLE, T2COLE(szItemName), plpmkOut);
	}

	return sc;
}

////////////////////////////////////////////////////////////////////////////
// COleUILinkInfo

COleUILinkInfo::COleUILinkInfo(COleDocument* pDocument)
{
	ASSERT(pDocument == NULL ||
		pDocument->IsKindOf(RUNTIME_CLASS(COleDocument)));
	m_pDocument = pDocument;
	m_pSelectedItem = NULL;
	m_pos = NULL;
	m_bUpdateLinks = FALSE;
	m_bUpdateEmbeddings = FALSE;
}

STDMETHODIMP_(ULONG) COleUILinkInfo::AddRef()
{
	return 0;
}

STDMETHODIMP_(ULONG) COleUILinkInfo::Release()
{
	return 0;
}

STDMETHODIMP COleUILinkInfo::QueryInterface(
	REFIID, LPVOID*)
{
	return E_NOTIMPL;
}

STDMETHODIMP_(DWORD) COleUILinkInfo::GetNextLink(
	DWORD dwLink)
{
	ASSERT(m_pDocument != NULL);
	if (dwLink == 0)
	{
		// start enumerating from the beginning
		m_pos = m_pDocument->GetStartPosition();
	}
	COleClientItem* pItem;
	while ((pItem = m_pDocument->GetNextClientItem(m_pos)) != NULL)
	{
		// check for links
		OLE_OBJTYPE objType = pItem->GetType();
		if (m_bUpdateLinks && objType == OT_LINK)
		{
			// link found -- return it
			return (DWORD)(void*)pItem;
		}
		// check for embeddings
		if (m_bUpdateEmbeddings && objType == OT_EMBEDDED)
		{
			// embedding w/mismatched target device
			return (DWORD)(void*)pItem;
		}
	}
	return 0;   // link not found
}

STDMETHODIMP COleUILinkInfo::SetLinkUpdateOptions(
	DWORD dwLink, DWORD dwUpdateOpt)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);
	ASSERT(pItem->GetType() == OT_LINK);

	SCODE sc;
	TRY
	{
		// item is a link -- get its link options
		pItem->SetLinkUpdateOptions((OLEUPDATE)dwUpdateOpt);
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

STDMETHODIMP COleUILinkInfo::GetLinkUpdateOptions(
	DWORD dwLink, DWORD* lpdwUpdateOpt)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);

	SCODE sc;
	TRY
	{
		if (pItem->GetType() == OT_LINK)
			*lpdwUpdateOpt = pItem->GetLinkUpdateOptions();
		else
			*lpdwUpdateOpt = OLEUPDATE_ALWAYS;  // make believe it is auto-link
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

STDMETHODIMP COleUILinkInfo::SetLinkSource(
	DWORD dwLink, LPTSTR lpszDisplayName, ULONG lenFileName,
	ULONG* pchEaten, BOOL  fValidateSource)
{
	USES_CONVERSION;

	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);
	ASSERT(pItem->GetType() == OT_LINK);

	LPOLEOBJECT lpObject = NULL;
	CLSID clsid;

	// parse the portion known to be a file name into a file moniker
	TCHAR szName[_MAX_PATH];
	lstrcpyn(szName, lpszDisplayName, (int)lenFileName + 1);
	LPMONIKER lpmk = NULL;
	SCODE sc = CreateFileMoniker(T2COLE(szName), &lpmk);
	if (lpmk == NULL)
		return sc;

	LPBC lpbc = NULL;
	if (fValidateSource)
	{
		sc = CreateBindCtx(0, &lpbc);
		if (sc != S_OK)
		{
			lpmk->Release();
			return sc;
		}
	}

	// nUneaten is the number of chars left to parse
	UINT nUneaten = lstrlen(lpszDisplayName) - lenFileName;

	// lpszRemainder is the left over display name
	LPTSTR lpszRemainder = lpszDisplayName + lenFileName;
	*pchEaten = lenFileName;

	// parse the rest of the display name
	while (nUneaten > 0)
	{
		// attempt to parse next moniker
		ULONG nEaten = 0;
		LPMONIKER lpmkNext = NULL;
		sc = _AfxParseDisplayName(lpmk, lpbc, lpszRemainder, &nEaten, &lpmkNext);
		if (sc != S_OK)
		{
			lpmk->Release();
			lpbc->Release();
			return sc;
		}

		// advance through the display name
		nUneaten -= nEaten;
		*pchEaten += nEaten;
		lpszRemainder += nEaten;

		if (lpmkNext != NULL)
		{
			// create composite out of current and next
			LPMONIKER lpmkTemp = NULL;
			sc = CreateGenericComposite(lpmk, lpmkNext, &lpmkTemp);
			if (FAILED(sc))
			{
				lpmk->Release();
				lpmkNext->Release();
				lpbc->Release();
				return sc;
			}

			// make current = next
			lpmkNext->Release();
			lpmk->Release();
			lpmk = lpmkTemp;
		}
	}

	if (fValidateSource)
	{
		// attempt to bind the the object
		sc = lpmk->BindToObject(lpbc, NULL, IID_IOleObject, (LPLP)&lpObject);
		if (FAILED(sc))
		{
			pItem->m_bLinkUnavail = TRUE;
			lpbc->Release();
			lpmk->Release();
			RELEASE(lpObject);
			return sc;
		}
		ASSERT(lpObject != NULL);

		// call GetUserClassID while bound so default handler updates
		lpObject->GetUserClassID(&clsid);
		pItem->m_bLinkUnavail = FALSE;
	}

	// get IOleLink interface
	LPOLELINK lpOleLink = QUERYINTERFACE(pItem->m_lpObject, IOleLink);
	ASSERT(lpOleLink != NULL);

	// set source from moniker
	sc = lpOleLink->SetSourceMoniker(lpmk, clsid);

	// update the cache if object was successfully bound
	if (lpObject != NULL)
	{
		lpObject->Update();
		lpObject->Release();
	}

	// cleanup
	lpOleLink->Release();
	RELEASE(lpmk);
	RELEASE(lpbc);

	return sc;
}

STDMETHODIMP COleUILinkInfo::GetLinkSource(
	DWORD dwLink, LPTSTR* lplpszDisplayName, ULONG* lplenFileName,
	LPTSTR* lplpszFullLinkType, LPTSTR* lplpszShortLinkType,
	BOOL* lpfSourceAvailable, BOOL* lpfIsSelected)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);
	ASSERT(pItem->GetType() == OT_LINK);

	// set OUT params to NULL
	ASSERT(lplpszDisplayName != NULL);
	*lplpszDisplayName  = NULL;
	if (lplpszFullLinkType != NULL)
		*lplpszFullLinkType = NULL;
	if (lplpszShortLinkType != NULL)
		*lplpszShortLinkType = NULL;
	if (lplenFileName != NULL)
		*lplenFileName = 0;
	if (lpfSourceAvailable != NULL)
		*lpfSourceAvailable = !pItem->m_bLinkUnavail;

	// get IOleLink interface
	LPOLELINK lpOleLink = QUERYINTERFACE(pItem->m_lpObject, IOleLink);
	ASSERT(lpOleLink != NULL);

	// get moniker & object information
	LPMONIKER lpmk;
	if (lpOleLink->GetSourceMoniker(&lpmk) == S_OK)
	{
		if (lplenFileName != NULL)
			*lplenFileName = _AfxOleGetLenFilePrefixOfMoniker(lpmk);
		lpmk->Release();
	}


	// attempt to get the type names of the link
	if (lplpszFullLinkType != NULL)
	{
		LPOLESTR lpOleStr = NULL;
		pItem->m_lpObject->GetUserType(USERCLASSTYPE_FULL, &lpOleStr);
		*lplpszFullLinkType = TASKSTRINGOLE2T(lpOleStr);
		if (*lplpszFullLinkType == NULL)
		{
			TCHAR szUnknown[256];
			VERIFY(AfxLoadString(AFX_IDS_UNKNOWNTYPE, szUnknown) != 0);
			*lplpszFullLinkType = AfxAllocTaskString(szUnknown);
		}
	}
	if (lplpszShortLinkType != NULL)
	{
		LPOLESTR lpOleStr = NULL;
		pItem->m_lpObject->GetUserType(USERCLASSTYPE_SHORT, &lpOleStr);
		*lplpszShortLinkType = TASKSTRINGOLE2T(lpOleStr);
		if (*lplpszShortLinkType == NULL)
		{
			TCHAR szUnknown[256];
			VERIFY(AfxLoadString(AFX_IDS_UNKNOWNTYPE, szUnknown) != 0);
			*lplpszShortLinkType = AfxAllocTaskString(szUnknown);
		}
	}

	// get source display name for moniker
	LPOLESTR lpOleStr = NULL;
	SCODE sc = lpOleLink->GetSourceDisplayName(&lpOleStr);
	*lplpszDisplayName = TASKSTRINGOLE2T(lpOleStr);
	lpOleLink->Release();
	if (sc != S_OK)
		return sc;

	// see if item is selected if specified
	if (lpfIsSelected)
	{
		*lpfIsSelected = (m_pSelectedItem == pItem);
	}

	return S_OK;
}

STDMETHODIMP COleUILinkInfo::OpenLinkSource(DWORD dwLink)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);
	ASSERT(pItem->GetType() == OT_LINK);

	SCODE sc;
	TRY
	{
		// Note: no need for valid CView* since links don't activate inplace
		pItem->DoVerb(OLEIVERB_SHOW, NULL);
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

STDMETHODIMP COleUILinkInfo::UpdateLink(
	DWORD dwLink, BOOL /*fErrorMessage*/, BOOL /*fErrorAction*/)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);

	SCODE sc;
	TRY
	{
		// link not up-to-date, attempt to update it
		if (!pItem->UpdateLink())
			AfxThrowOleException(pItem->GetLastStatus());
		pItem->m_bLinkUnavail = FALSE;
		sc = S_OK;
	}
	CATCH_ALL(e)
	{
		pItem->m_bLinkUnavail = TRUE;
		sc = COleException::Process(e);
		pItem->ReportError(sc);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleUILinkInfo::CancelLink(DWORD dwLink)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleClientItem, pItem);
	ASSERT(pItem->GetType() == OT_LINK);

	SCODE sc = E_FAIL;
	TRY
	{
		if (pItem->FreezeLink())
			sc = S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	// report error
	if (sc != S_OK)
		pItem->ReportError(sc);

	return S_OK;
}

STDMETHODIMP COleUILinkInfo::GetLastUpdate(DWORD dwLink, FILETIME*)
{
	COleClientItem* pItem = (COleClientItem*)dwLink;
	ASSERT_VALID(pItem);

	// Note: leave last update time at unknown!

	return S_OK;
}

////////////////////////////////////////////////////////////////////////////
// InsertObject dialog wrapper

COleInsertDialog::COleInsertDialog(DWORD dwFlags, CWnd* pParentWnd)
	: COleDialog(pParentWnd)
{
	memset(&m_io, 0, sizeof(m_io)); // initialize structure to 0/NULL

	// fill in common part
	m_io.cbStruct = sizeof(m_io);
	m_io.dwFlags = dwFlags;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_io.dwFlags |= IOF_SHOWHELP;
	if (_AfxOlePropertiesEnabled())
		m_io.dwFlags |= IOF_HIDECHANGEICON;
	m_io.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_INSERTOBJECT;

	// specific to this dialog
	m_io.lpszFile = m_szFileName;
	m_io.cchFile = _countof(m_szFileName);
	m_szFileName[0] = '\0';
}

COleInsertDialog::~COleInsertDialog()
{
	_AfxDeleteMetafilePict(m_io.hMetaPict);
}

void COleInsertDialog::AddClassIDToList(LPCLSID& lpList,
	int& nListCount, int& nBufferLen, LPCLSID lpNewID)
{
	// if the list doesn't exist, create it

	if (lpList == NULL)
	{
		nListCount = 0;
		nBufferLen = 16;
		lpList = new CLSID[nBufferLen];
	}

	// if the list isn't big enough grow it by twice

	if (nListCount == nBufferLen)
	{
		LPCLSID lpOldList = lpList;
		nBufferLen <<= 2;
		lpList = new CLSID[nBufferLen];
		memcpy(lpList, lpOldList, sizeof(CLSID) * nListCount);
		delete [] lpOldList;
	}

	lpList[nListCount] = *lpNewID;
	nListCount++;

	return;
}

int COleInsertDialog::DoModal(DWORD dwFlags)
{
	LPCLSID lpNewExcludeList = NULL;
	int nNewExcludeCount = 0;
	int nNewExcludeLen = 0;

	// support for controls is built-in
	// support doc objects by filtering what the dialog shows

	if (dwFlags == ControlsOnly)
		m_io.dwFlags |= IOF_SELECTCREATECONTROL | IOF_SHOWINSERTCONTROL;
	else if (dwFlags == DocObjectsOnly)
	{
		m_io.dwFlags |= IOF_DISABLEDISPLAYASICON | IOF_DISABLELINK;

		HKEY hkClassesRoot;
		HKEY hkCLSID;
		HKEY hkItem;
		HKEY hkDocObject;
		DWORD dwIndex = 0;
		TCHAR szName[MAX_PATH+1];

		if (RegOpenKeyEx(HKEY_CLASSES_ROOT, NULL, 0, KEY_READ,
				&hkClassesRoot) == ERROR_SUCCESS)
		{
			if(RegOpenKeyEx(hkClassesRoot, _T("CLSID"), 0,
				KEY_ENUMERATE_SUB_KEYS, &hkCLSID) == ERROR_SUCCESS)
			{
				while(RegEnumKey(hkCLSID, dwIndex++,
							szName, sizeof(szName)) == ERROR_SUCCESS)
				{
					if (RegOpenKeyEx(hkCLSID, szName, 0,
							KEY_READ, &hkItem) == ERROR_SUCCESS)
					{
						if ((RegOpenKeyEx(hkItem, _T("Insertable"), 0,
								KEY_READ, &hkDocObject) == ERROR_SUCCESS) ||
							 (RegOpenKeyEx(hkItem, _T("Ole1Class"),0,
								KEY_READ, &hkDocObject) == ERROR_SUCCESS))
						{
							RegCloseKey(hkDocObject);
							if ((RegOpenKeyEx(hkItem, _T("DocObject"), 0,
									KEY_READ, &hkDocObject) != ERROR_SUCCESS ) )
							{
								USES_CONVERSION;

								CLSID clsid;
								CLSIDFromString(T2OLE(szName), &clsid);
								AddClassIDToList(lpNewExcludeList,
									nNewExcludeCount, nNewExcludeLen, &clsid);
								RegCloseKey(hkItem);
							}
							RegCloseKey(hkDocObject);
						}
					}
				}
				RegCloseKey(hkCLSID);
			}
		}
		RegCloseKey(hkClassesRoot);
	}

	UINT cOldClsidExclude = m_io.cClsidExclude;
	LPCLSID lpOldClsidExclude = m_io.lpClsidExclude;

	if (lpNewExcludeList != NULL)
	{
		m_io.lpClsidExclude = lpNewExcludeList;
		m_io.cClsidExclude = nNewExcludeCount;
	}

QueryAgain:
	int nDisposition = DoModal();

	if (nDisposition == IDOK && GetSelectionType() == insertFromFile &&
		dwFlags == DocObjectsOnly)
	{
		USES_CONVERSION;

		// Double-check that the requested file really is serviced by a
		// DocObject server.

		CLSID clsidFile;
		BOOL bIsDocObject = FALSE;
		if (GetClassFile(T2COLE(m_io.lpszFile), &clsidFile) == S_OK)
		{
			CString strKey;
			CString strCLSID = AfxStringFromCLSID(clsidFile);
			strKey.Format(_T("CLSID\\%s\\DocObject"), strCLSID);

			HKEY hKey;
			if (ERROR_SUCCESS ==
				RegOpenKeyEx(HKEY_CLASSES_ROOT, strKey, 0, KEY_QUERY_VALUE, &hKey))
			{
				RegCloseKey(hKey);
				bIsDocObject = TRUE;
			}
		}

		if (!bIsDocObject)
		{
			nDisposition =
				AfxMessageBox(AFX_IDS_NOT_DOCOBJECT, MB_RETRYCANCEL | MB_ICONHAND);
			if (nDisposition == IDRETRY)
				goto QueryAgain;
			else
				nDisposition = IDCANCEL;
		}
	}

	delete [] lpNewExcludeList;

	m_io.cClsidExclude = cOldClsidExclude;
	m_io.lpClsidExclude = lpOldClsidExclude;

	return nDisposition;
}

int COleInsertDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_io.lpfnHook != NULL);  // can still be a user hook

	m_io.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIInsertObject(&m_io));
	PostModal();
	return iResult;
}

UINT COleInsertDialog::GetSelectionType() const
{
	ASSERT_VALID(this);

	if (m_io.dwFlags & IOF_SELECTCREATEFROMFILE)
	{
		if (m_io.dwFlags & IOF_CHECKLINK)
			return linkToFile;
		else
			return insertFromFile;
	}
	ASSERT(m_io.dwFlags & IOF_SELECTCREATENEW);
	return createNewItem;
}

// allocate an item first, then call this fuction to create it
BOOL COleInsertDialog::CreateItem(COleClientItem* pNewItem)
{
	ASSERT_VALID(pNewItem);

	// switch on selection type
	UINT selType = GetSelectionType();
	BOOL bResult;

	switch (selType)
	{
	case linkToFile:
		// link to file selected
		ASSERT(m_szFileName[0] != 0);
		bResult = pNewItem->CreateLinkFromFile(m_szFileName);
		break;
	case insertFromFile:
		// insert file selected
		ASSERT(m_szFileName[0] != 0);
		bResult = pNewItem->CreateFromFile(m_szFileName);
		break;
	default:
		// otherwise must be create new
		ASSERT(selType == createNewItem);
		bResult = pNewItem->CreateNewItem(m_io.clsid);
		break;
	}

	// deal with Display As Iconic option
	if (bResult && GetDrawAspect() == DVASPECT_ICON)
	{
		// setup iconic cache (it will draw iconic by default as well)
		if (!pNewItem->SetIconicMetafile(m_io.hMetaPict))
		{
			TRACE0("Warning: failed to set iconic aspect in CreateItem.\n");
			return TRUE;
		}

		// since picture was set OK, draw as iconic as well...
		pNewItem->SetDrawAspect(DVASPECT_ICON);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// COleInsertDialog diagnostics

#ifdef _DEBUG
void COleInsertDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_szFileName = " << m_szFileName;
	dc << "\nm_io.cbStruct = " << m_io.cbStruct;
	dc << "\nm_io.dwFlags = " << (LPVOID)m_io.dwFlags;
	dc << "\nm_io.hWndOwner = " << (UINT)m_io.hWndOwner;
	dc << "\nm_io.lpszCaption = " << m_io.lpszCaption;
	dc << "\nm_io.lCustData = " << (LPVOID)m_io.lCustData;
	dc << "\nm_io.hInstance = " << (UINT)m_io.hInstance;
	dc << "\nm_io.lpszTemplate = " << (LPVOID)m_io.lpszTemplate;
	dc << "\nm_io.hResource = " << (UINT)m_io.hResource;
	if (m_io.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";
	dc << "\nm_io.hMetaPict = " << (UINT)m_io.hMetaPict;

	dc << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// COleConvertDialog

COleConvertDialog::COleConvertDialog(COleClientItem* pItem, DWORD dwFlags,
	CLSID* pClassID, CWnd* pParentWnd) : COleDialog(pParentWnd)
{
	if (pItem != NULL)
		ASSERT_VALID(pItem);
	ASSERT(pClassID == NULL || AfxIsValidAddress(pClassID, sizeof(CLSID), FALSE));

	memset(&m_cv, 0, sizeof(m_cv)); // initialize structure to 0/NULL
	if (pClassID != NULL)
		m_cv.clsid = *pClassID;

	// fill in common part
	m_cv.cbStruct = sizeof(m_cv);
	m_cv.dwFlags = dwFlags;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_cv.dwFlags |= CF_SHOWHELPBUTTON;
	m_cv.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_CONVERT;

	// specific to this dialog
	m_cv.fIsLinkedObject = pItem->GetType() == OT_LINK;
	m_cv.dvAspect = pItem->GetDrawAspect();
	if (pClassID == NULL && !m_cv.fIsLinkedObject)
	{
		// for embeddings, attempt to get class ID from the storage
		if (ReadClassStg(pItem->m_lpStorage, &m_cv.clsid) == S_OK)
			pClassID = &m_cv.clsid;

		// attempt to get user type from storage
		CLIPFORMAT cf = 0;
		LPOLESTR lpOleStr = NULL;
		ReadFmtUserTypeStg(pItem->m_lpStorage, &cf, &lpOleStr);
		m_cv.lpszUserType = TASKSTRINGOLE2T(lpOleStr);

		m_cv.wFormat = (WORD)cf;
	}
	// get class id if neded
	if (pClassID == NULL)
	{
		// no class ID in the storage, use class ID of the object
		pItem->GetClassID(&m_cv.clsid);
	}

	// get user type if needed
	if (m_cv.lpszUserType == NULL)
	{
		// no user type in storge, get user type from class ID
		LPTSTR lpszUserType = NULL;
		LPOLESTR lpOleStr = NULL;
		if (OleRegGetUserType(m_cv.clsid, USERCLASSTYPE_FULL,
			&lpOleStr) == S_OK)
		{
			lpszUserType = TASKSTRINGOLE2T(lpOleStr);
		}
		else
		{
			lpszUserType = (LPTSTR)CoTaskMemAlloc(256 * sizeof(TCHAR));
			if (lpszUserType != NULL)
			{
				lpszUserType[0] = '?';
				lpszUserType[1] = 0;
				VERIFY(AfxLoadString(AFX_IDS_UNKNOWNTYPE, lpszUserType) != 0);
			}
		}
		m_cv.lpszUserType = lpszUserType;
	}
	m_cv.hMetaPict = pItem->GetIconicMetafile();
}

COleConvertDialog::~COleConvertDialog()
{
	_AfxDeleteMetafilePict(m_cv.hMetaPict);
}

int COleConvertDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_cv.lpfnHook != NULL);  // can still be a user hook

	m_cv.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIConvert(&m_cv));
	PostModal();
	return iResult;
}

BOOL COleConvertDialog::DoConvert(COleClientItem* pItem)
{
	ASSERT_VALID(pItem);

	CWaitCursor wait;

	UINT selType = GetSelectionType();
	BOOL bResult = TRUE;

	if (m_cv.clsidNew != CLSID_NULL)
	{
		switch (selType)
		{
		case convertItem:
			bResult = pItem->ConvertTo(m_cv.clsidNew);
			break;
		case activateAs:
			bResult = pItem->ActivateAs(m_cv.lpszUserType, m_cv.clsid,
				m_cv.clsidNew);
			break;
		default:
			ASSERT(selType == noConversion);
			break;
		}
	}

	if (!bResult)
	{
		// if unable to convert the object show message box
		AfxMessageBox(AFX_IDP_FAILED_TO_CONVERT);
		return FALSE;
	}

	// change to iconic/content view if changed
	if ((DVASPECT)m_cv.dvAspect != pItem->GetDrawAspect())
	{
		pItem->OnChange(OLE_CHANGED_ASPECT, (DWORD)m_cv.dvAspect);
		pItem->SetDrawAspect((DVASPECT)m_cv.dvAspect);
	}

	// change the actual icon as well
	if (m_cv.fObjectsIconChanged)
	{
		pItem->SetIconicMetafile(m_cv.hMetaPict);
		if (pItem->GetDrawAspect() == DVASPECT_ICON)
			pItem->OnChange(OLE_CHANGED, (DWORD)DVASPECT_ICON);
	}

	return TRUE;
}

UINT COleConvertDialog::GetSelectionType() const
{
	ASSERT_VALID(this);

	if (m_cv.clsid != m_cv.clsidNew)
	{
		if (m_cv.dwFlags & CF_SELECTCONVERTTO)
			return convertItem;
		else if (m_cv.dwFlags & CF_SELECTACTIVATEAS)
			return activateAs;
	}
	return noConversion;
}

/////////////////////////////////////////////////////////////////////////////
// COleConvertDialog diagnostics

#ifdef _DEBUG
void COleConvertDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_cv.cbStruct = " << m_cv.cbStruct;
	dc << "\nm_cv.dwFlags = " << (LPVOID)m_cv.dwFlags;
	dc << "\nm_cv.hWndOwner = " << (UINT)m_cv.hWndOwner;
	dc << "\nm_cv.lpszCaption = " << m_cv.lpszCaption;
	dc << "\nm_cv.lCustData = " << (LPVOID)m_cv.lCustData;
	dc << "\nm_cv.hInstance = " << (UINT)m_cv.hInstance;
	dc << "\nm_cv.lpszTemplate = " << (LPVOID)m_cv.lpszTemplate;
	dc << "\nm_cv.hResource = " << (UINT)m_cv.hResource;
	if (m_cv.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";
	dc << "\nm_cv.dvAspect = " << (UINT)m_cv.dvAspect;
	dc << "\nm_cv.wFormat = " << (UINT)m_cv.wFormat;
	dc << "\nm_cv.fIsLinkedObject = " << m_cv.fIsLinkedObject;
	dc << "\nm_cv.hMetaPict = " << (UINT)m_cv.hMetaPict;
	dc << "\nm_cv.lpszUserType = " << m_cv.lpszUserType;
	dc << "\nm_cv.fObjectsIconChanged = " << m_cv.fObjectsIconChanged;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
// COleChangeIconDialog

COleChangeIconDialog::COleChangeIconDialog(COleClientItem* pItem,
	DWORD dwFlags, CWnd* pParentWnd) : COleDialog(pParentWnd)
{
	if (pItem != NULL)
		ASSERT_VALID(pItem);

	memset(&m_ci, 0, sizeof(m_ci)); // initialize structure to 0/NULL

	// fill in common part
	m_ci.cbStruct = sizeof(m_ci);
	m_ci.dwFlags = dwFlags;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_ci.dwFlags |= CIF_SHOWHELP;
	m_ci.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_CHANGEICON;

	// specific to this dialog
	if (pItem != NULL)
	{
		pItem->GetClassID(&m_ci.clsid);
		m_ci.hMetaPict = pItem->GetIconicMetafile();
	}
}

COleChangeIconDialog::~COleChangeIconDialog()
{
	_AfxDeleteMetafilePict(m_ci.hMetaPict);
}

int COleChangeIconDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_ci.lpfnHook != NULL);  // can still be a user hook

	m_ci.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIChangeIcon(&m_ci));
	PostModal();
	return iResult;
}

BOOL COleChangeIconDialog::DoChangeIcon(COleClientItem* pItem)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pItem);

	// set the picture
	if (!pItem->SetIconicMetafile(GetIconicMetafile()))
		return FALSE;

	// notify the item of the change if the current draw aspect is ICON
	if (pItem->GetDrawAspect() == DVASPECT_ICON)
		pItem->OnChange(OLE_CHANGED, (DWORD)DVASPECT_ICON);

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// COleChangeIconDialog diagnostics

#ifdef _DEBUG
void COleChangeIconDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_ci.cbStruct = " << m_ci.cbStruct;
	dc << "\nm_ci.dwFlags = " << (LPVOID)m_ci.dwFlags;
	dc << "\nm_ci.hWndOwner = " << (UINT)m_ci.hWndOwner;
	dc << "\nm_ci.lpszCaption = " << m_ci.lpszCaption;
	dc << "\nm_ci.lCustData = " << (LPVOID)m_ci.lCustData;
	dc << "\nm_ci.hInstance = " << (UINT)m_ci.hInstance;
	dc << "\nm_ci.lpszTemplate = " << (LPVOID)m_ci.lpszTemplate;
	dc << "\nm_ci.hResource = " << (UINT)m_ci.hResource;
	if (m_ci.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";
	dc << "\nm_ci.hMetaPict = " << (UINT)m_ci.hMetaPict;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
// COleLinksDialog

COleLinksDialog::COleLinksDialog(COleDocument* pDoc, CView* pView,
	DWORD dwFlags, CWnd* pParentWnd) : COleDialog(pParentWnd),
	m_xLinkInfo(pDoc)
{
	ASSERT_VALID(pDoc);
	if (pView != NULL)
		ASSERT_VALID(pView);

	memset(&m_el, 0, sizeof(m_el)); // initialize structure to 0/NULL

	// fill in common part
	m_el.cbStruct = sizeof(m_el);
	m_el.dwFlags = dwFlags;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_el.dwFlags |= ELF_SHOWHELP;
	m_el.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_EDITLINKS;

	// specific to this dialog
	if (pView != NULL)
		m_xLinkInfo.m_pSelectedItem = pDoc->GetPrimarySelectedItem(pView);
	else
		m_xLinkInfo.m_pSelectedItem = NULL;
	m_el.lpOleUILinkContainer = &m_xLinkInfo;
}

COleLinksDialog::~COleLinksDialog()
{
}

int COleLinksDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_el.lpfnHook != NULL);  // can still be a user hook

	// this function is always used for updating links
	m_xLinkInfo.m_bUpdateLinks = TRUE;

	m_el.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIEditLinks(&m_el));
	PostModal();
	return iResult;
}

/////////////////////////////////////////////////////////////////////////////
// COleLinksDialog diagnostics

#ifdef _DEBUG
void COleLinksDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "\nm_el.cbStruct = " << m_el.cbStruct;
	dc << "\nm_el.dwFlags = " << (void*)m_el.dwFlags;
	dc << "\nm_el.hWndOwner = " << (UINT)m_el.hWndOwner;
	dc << "\nm_el.lpszCaption = " << m_el.lpszCaption;
	dc << "\nm_el.lCustData = " << (void*)m_el.lCustData;
	dc << "\nm_el.hInstance = " << (UINT)m_el.hInstance;
	dc << "\nm_el.lpszTemplate = " << (void*)m_el.lpszTemplate;
	dc << "\nm_el.hResource = " << (UINT)m_el.hResource;
	if (m_el.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";

	dc << "\n";
}

void COleLinksDialog::AssertValid() const
{
	COleDialog::AssertValid();
}
#endif

/////////////////////////////////////////////////////////////////////////////
// COleUpdateDialog

COleUpdateDialog::COleUpdateDialog(COleDocument* pDoc,
	BOOL bUpdateLinks, BOOL bUpdateEmbeddings, CWnd* pParentWnd)
		: COleLinksDialog(pDoc, NULL, 0, pParentWnd)
{
	ASSERT_VALID(pDoc);
	ASSERT(bUpdateLinks || bUpdateEmbeddings);

	// non-base class parameters
	m_xLinkInfo.m_bUpdateLinks = bUpdateLinks;
	m_xLinkInfo.m_bUpdateEmbeddings = bUpdateEmbeddings;
	m_strCaption.LoadString(AFX_IDS_UPDATING_ITEMS);
}

COleUpdateDialog::~COleUpdateDialog()
{
}

int COleUpdateDialog::DoModal()
{
	ASSERT_VALID(this);

	// first count number of links/embeddings to be updated
	DWORD dwLink = 0;
	int cLinks = 0;
	while ((dwLink = m_el.lpOleUILinkContainer->GetNextLink(dwLink)) != 0)
		++cLinks;
	// when no links are out-of-date, don't bother
	if (cLinks == 0)
		return IDCANCEL;

	// bring up the dialog that processes all the links
	HWND hWndParent = PreModal();
	BOOL bResult = OleUIUpdateLinks(m_el.lpOleUILinkContainer,
		hWndParent, (LPTSTR)(LPCTSTR)m_strCaption, cLinks);
	PostModal();
	return bResult ? IDOK : -1;
}

/////////////////////////////////////////////////////////////////////////////
// COleUpdateDialog diagnostics

#ifdef _DEBUG
void COleUpdateDialog::Dump(CDumpContext& dc) const
{
	COleLinksDialog::Dump(dc);

	dc << "m_strCaption = " << m_strCaption;
	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
// COlePasteSpecialDialog

COlePasteSpecialDialog::COlePasteSpecialDialog(DWORD dwFlags,
	COleDataObject* pDataObject, CWnd* pParentWnd) : COleDialog(pParentWnd)
{
	memset(&m_ps, 0, sizeof(m_ps)); // initialize structure to 0/NULL

	// fill in common part
	m_ps.cbStruct = sizeof(m_ps);
	m_ps.dwFlags = dwFlags | PSF_STAYONCLIPBOARDCHANGE;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_ps.dwFlags |= PSF_SHOWHELP;
	if (_AfxOlePropertiesEnabled())
		m_ps.dwFlags |= PSF_HIDECHANGEICON;
	m_ps.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_PASTESPECIAL;

	// get LPDATAOBJECT for paste special dialog
	COleDataObject dataObject;
	if (pDataObject == NULL)
	{
		VERIFY(dataObject.AttachClipboard());
		pDataObject = &dataObject;
	}
	ASSERT(pDataObject != NULL);
	m_ps.lpSrcDataObj = pDataObject->GetIDataObject(TRUE);

	// complete initialization
	m_ps.arrPasteEntries = NULL;
	m_ps.cPasteEntries = 0;
	m_ps.arrLinkTypes = m_arrLinkTypes;
	m_ps.cLinkTypes = 0;
}

COlePasteSpecialDialog::~COlePasteSpecialDialog()
{
	_AfxDeleteMetafilePict(m_ps.hMetaPict);

	for (int i = 0; i < m_ps.cPasteEntries; i++)
	{
		free((void*)m_ps.arrPasteEntries[i].lpstrFormatName);
		free((void*)m_ps.arrPasteEntries[i].lpstrResultText);
	}
	free(m_ps.arrPasteEntries);

	RELEASE(m_ps.lpSrcDataObj);
}

int COlePasteSpecialDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_ps.lpfnHook != NULL);  // can still be a user hook

	// return error if IDataObject* not available
	if (m_ps.lpSrcDataObj == NULL)
		return -1;

	m_ps.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIPasteSpecial(&m_ps));
	PostModal();
	return iResult;
}

UINT COlePasteSpecialDialog::GetSelectionType() const
{
	ASSERT_VALID(this);
	ASSERT(m_ps.dwFlags & (PSF_SELECTPASTE|PSF_SELECTPASTELINK));

	UINT cf = m_ps.arrPasteEntries[m_ps.nSelectedIndex].fmtetc.cfFormat;
	Selection selType = pasteOther;
	if (m_ps.dwFlags & PSF_SELECTPASTELINK)
	{
		selType = pasteLink;
	}
	else if (cf == _oleData.cfEmbedSource || cf == _oleData.cfEmbeddedObject ||
			cf == _oleData.cfLinkSource)
	{
		selType = pasteNormal;
	}
	else if (cf == CF_METAFILEPICT || cf == CF_DIB || cf == CF_BITMAP)
	{
		selType = pasteStatic;
	}
	return selType;
}

/////////////////////////////////////////////////////////////////////////////
// COlePasteSpecialDialog diagnostics

#ifdef _DEBUG
void COlePasteSpecialDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_ps.cbStruct = " << m_ps.cbStruct;
	dc << "\nm_ps.dwFlags = " << (LPVOID)m_ps.dwFlags;
	dc << "\nm_ps.hWndOwner = " << (UINT)m_ps.hWndOwner;
	dc << "\nm_ps.lpszCaption = " << m_ps.lpszCaption;
	dc << "\nm_ps.lCustData = " << (LPVOID)m_ps.lCustData;
	dc << "\nm_ps.hInstance = " << (UINT)m_ps.hInstance;
	dc << "\nm_ps.lpszTemplate = " << (LPVOID)m_ps.lpszTemplate;
	dc << "\nm_ps.hResource = " << (UINT)m_ps.hResource;
	if (m_ps.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";
	dc << "\nm_ps.lpSrcDataObj = " << (LPVOID)m_ps.lpSrcDataObj;
	dc << "\nm_ps.cPasteEntries = " << m_ps.cPasteEntries;
	dc << "\nm_ps.cLinkTypes = " << m_ps.cLinkTypes;
	dc << "\nm_ps.nSelectedIndex = " << m_ps.nSelectedIndex;
	dc << "\nm_ps.fLink = " << m_ps.fLink;

	dc << "\n";
}

void COlePasteSpecialDialog::AssertValid() const
{
	COleDialog::AssertValid();
	ASSERT(m_ps.cPasteEntries == 0 || m_ps.arrPasteEntries != NULL);
	ASSERT(m_ps.arrLinkTypes != NULL);
	ASSERT(m_ps.cLinkTypes <= 8);
}
#endif

////////////////////////////////////////////////////////////////////////////

OLEUIPASTEFLAG COlePasteSpecialDialog::AddLinkEntry(UINT cf)
{
	ASSERT_VALID(this);
	ASSERT(m_ps.cLinkTypes <= 8);
	for (int i = 0; i < m_ps.cLinkTypes; i++)
	{
		if (m_ps.arrLinkTypes[i] == cf)
			break;
	}
	if (i == 8)
		return (OLEUIPASTEFLAG)0;
	m_ps.arrLinkTypes[i] = cf;
	if (i == m_ps.cLinkTypes)
		m_ps.cLinkTypes++;
	return (OLEUIPASTEFLAG) (OLEUIPASTE_LINKTYPE1 << i);
}

void COlePasteSpecialDialog::AddFormat(UINT cf, DWORD tymed, UINT nFormatID,
	BOOL bEnableIcon, BOOL bLink)
{
	TCHAR szFormat[256];
	if (AfxLoadString(nFormatID, szFormat) == 0)
		AfxThrowResourceException();

	// the format and result strings are delimited by a newline
	LPTSTR lpszResult = _tcschr(szFormat, '\n');
	ASSERT(lpszResult != NULL);  // must contain a newline
	*lpszResult = '\0';
	++lpszResult;    // one char past newline

	// add it to the array of acceptable formats
	m_ps.arrPasteEntries = (OLEUIPASTEENTRY *)realloc(m_ps.arrPasteEntries,
		sizeof(OLEUIPASTEENTRY) * (m_ps.cPasteEntries +1));

	OLEUIPASTEENTRY* pEntry = &m_ps.arrPasteEntries[m_ps.cPasteEntries];
	pEntry->fmtetc.cfFormat = (CLIPFORMAT)cf;
	pEntry->fmtetc.dwAspect = DVASPECT_CONTENT;
	pEntry->fmtetc.ptd = NULL;
	pEntry->fmtetc.tymed = tymed;
	pEntry->fmtetc.lindex = -1;
	pEntry->lpstrFormatName = _tcsdup(szFormat);
	pEntry->lpstrResultText = _tcsdup(lpszResult);
	pEntry->dwFlags = OLEUIPASTE_PASTE;

	if (bEnableIcon)
		pEntry->dwFlags |= OLEUIPASTE_ENABLEICON;
	if (bLink)
		pEntry->dwFlags |= AddLinkEntry(cf);
	if (pEntry->dwFlags == OLEUIPASTE_PASTE)
		pEntry->dwFlags = OLEUIPASTE_PASTEONLY;
	pEntry->dwScratchSpace = NULL;
	m_ps.cPasteEntries++;
}

// if the flags parameter includes a LINKTYPE# flag, it should be obtained
// from AddLinkEntry
void COlePasteSpecialDialog::AddFormat(const FORMATETC& formatEtc,
	LPTSTR lpszFormat, LPTSTR lpszResult, DWORD dwFlags)
{
	ASSERT_VALID(this);

	m_ps.arrPasteEntries = (OLEUIPASTEENTRY *)realloc(
		m_ps.arrPasteEntries, sizeof(OLEUIPASTEENTRY) * (m_ps.cPasteEntries +1));
	OLEUIPASTEENTRY* pEntry = &m_ps.arrPasteEntries[m_ps.cPasteEntries];
	pEntry->fmtetc = formatEtc;
	pEntry->lpstrFormatName = _tcsdup(lpszFormat);
	pEntry->lpstrResultText = _tcsdup(lpszResult);
	pEntry->dwFlags = dwFlags;
	pEntry->dwScratchSpace = NULL;
	m_ps.cPasteEntries++;
}

void COlePasteSpecialDialog::AddStandardFormats(BOOL bEnableLink)
{
	// Note: only need to add Embedded Object because Embed Source is
	//  automatically recognized by the paste special dialog implementation.
	ASSERT(_oleData.cfEmbeddedObject != NULL);
	AddFormat(_oleData.cfEmbeddedObject, TYMED_ISTORAGE, AFX_IDS_EMBED_FORMAT,
		TRUE, FALSE);

	// add link source if requested
	if (bEnableLink)
	{
		ASSERT(_oleData.cfLinkSource != NULL);
		AddFormat(_oleData.cfLinkSource, TYMED_ISTREAM, AFX_IDS_LINKSOURCE_FORMAT,
			TRUE, TRUE);
	}

	// add formats that can be used for 'static' items
	AddFormat(CF_METAFILEPICT, TYMED_MFPICT, AFX_IDS_METAFILE_FORMAT,
		FALSE, FALSE);
	AddFormat(CF_DIB, TYMED_HGLOBAL, AFX_IDS_DIB_FORMAT, FALSE, FALSE);
	AddFormat(CF_BITMAP, TYMED_GDI, AFX_IDS_BITMAP_FORMAT, FALSE, FALSE);
}

BOOL COlePasteSpecialDialog::CreateItem(COleClientItem *pNewItem)
{
	ASSERT_VALID(this);
	ASSERT(pNewItem != NULL);
	ASSERT(m_ps.lpSrcDataObj != NULL);

	CWaitCursor wait;

	COleDataObject dataObject;
	dataObject.Attach(m_ps.lpSrcDataObj, FALSE);

	UINT selType = GetSelectionType();
	BOOL bResult = TRUE;

	switch (selType)
	{
	case pasteLink:
		// paste link
		if (!pNewItem->CreateLinkFromData(&dataObject))
		{
			TRACE0("Warning: CreateLinkFromData failed.\n");
			bResult = FALSE;
		}
		break;
	case pasteStatic:
		if (!pNewItem->CreateStaticFromData(&dataObject))
		{
			TRACE0("Warning: CreateStaticFromData failed.\n");
			bResult = FALSE;
		}
		break;
	default:
		ASSERT(selType == pasteNormal);
		if (!pNewItem->CreateFromData(&dataObject))
		{
			TRACE0("Warning: CreateFromData failed.\n");
			bResult = FALSE;
		}
		break;
	}

	// deal with Display As Iconic option
	if (bResult && GetDrawAspect() == DVASPECT_ICON)
	{
		// setup iconic cache (it will draw iconic by default as well)
		if (!pNewItem->SetIconicMetafile(m_ps.hMetaPict))
		{
			TRACE0("Warning: failed to set iconic aspect.\n");
			bResult = FALSE;
		}
		else
		{
			// since picture was set OK, draw as iconic as well...
			pNewItem->SetDrawAspect(DVASPECT_ICON);
		}
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

// expand inlines for OLE dialog APIs
static char _szAfxOleInl[] = "afxole.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxOleInl
#define _AFXODLGS_INLINE
#include "afxole.inl"

#endif //!_AFX_ENABLE_INLINES

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleInsertDialog, COleDialog)
IMPLEMENT_DYNAMIC(COleConvertDialog, COleDialog)
IMPLEMENT_DYNAMIC(COleChangeIconDialog, COleDialog)
IMPLEMENT_DYNAMIC(COleLinksDialog, COleDialog)
IMPLEMENT_DYNAMIC(COleUpdateDialog, COleLinksDialog)
IMPLEMENT_DYNAMIC(COlePasteSpecialDialog, COleDialog)

/////////////////////////////////////////////////////////////////////////////
