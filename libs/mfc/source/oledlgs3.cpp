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
// EditProperties dialog wrapper

COlePropertiesDialog::COlePropertiesDialog(
	COleClientItem* pItem, UINT nScaleMin, UINT nScaleMax, CWnd* pParentWnd)
	: COleDialog(pParentWnd), m_xLinkInfo(NULL)
{
	ASSERT_VALID(pItem);

	memset(&m_op, 0, sizeof(m_op)); // initialize structure to 0/NULL

	// fill in common part
	m_op.cbStruct = sizeof(m_op);
	m_op.dwFlags = 0;
	if (AfxHelpEnabled())
		m_op.dwFlags |= OPF_SHOWHELP;
	m_nIDHelp = AFX_IDD_OBJECTPROPERTIES;

	// specific to this dialog
	memset(&m_gp, 0, sizeof(m_gp));
	m_gp.cbStruct = sizeof(m_gp);
	m_gp.lpfnHook = AfxOleHookProc;
	m_gp.dwFlags = 0;

	memset(&m_vp, 0, sizeof(m_vp));
	m_vp.cbStruct = sizeof(m_vp);
	m_vp.lpfnHook = AfxOleHookProc;
	m_vp.dwFlags = 0;
	m_vp.nScaleMin = nScaleMin;
	m_vp.nScaleMax = nScaleMax;

	memset(&m_lp, 0, sizeof(m_lp));
	m_lp.cbStruct = sizeof(m_lp);
	m_lp.lpfnHook = AfxOleHookProc;
	m_lp.dwFlags = 0;

	m_op.lpObjInfo = &m_xOleUIObjInfo;
	m_op.lpLinkInfo = &m_xLinkInfo;
	m_op.dwLink = (DWORD)pItem;
	m_op.dwObject = (DWORD)pItem;
	m_op.lpGP = &m_gp; m_op.lpVP = &m_vp;
	if (pItem->GetType() == OT_LINK)
	{
		m_op.dwFlags |= OPF_OBJECTISLINK;
		m_op.lpLP = &m_lp;
	}
	memset(&m_psh, 0, sizeof(m_psh));
	m_psh.dwSize = sizeof(PROPSHEETHEADER);
	m_op.lpPS = &m_psh;
}

int COlePropertiesDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_gp.lpfnHook != NULL);  // can still be a user hook
	ASSERT(m_vp.lpfnHook != NULL);  // can still be a user hook
	ASSERT(m_lp.lpfnHook != NULL);  // can still be a user hook

	// disable scale if OnApplyScale not implemented
	if (!OnApplyScale(NULL, -1, FALSE))
		m_op.lpVP->dwFlags |= VPF_DISABLESCALE;

	// invoke the dialog
	m_op.lpPS->hwndParent = PreModal();
	int iResult = MapResult(::OleUIObjectProperties(&m_op));
	PostModal();
	return iResult;
}

BOOL COlePropertiesDialog::OnInitDialog()
{
	BOOL bResult = COleDialog::OnInitDialog();

	// automatic centering doesn't work for this dialog
	if (!(GetStyle() & WS_CHILD))
		CenterWindow();

	return bResult;
}

STDMETHODIMP_(ULONG) COlePropertiesDialog::XOleUIObjInfo::AddRef()
{
	return 0;
}

STDMETHODIMP_(ULONG) COlePropertiesDialog::XOleUIObjInfo::Release()
{
	return 0;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::QueryInterface(
	REFIID, LPVOID*)
{
	return E_NOTIMPL;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::GetObjectInfo(
	DWORD dwObject, DWORD* lpdwObjSize, LPTSTR* lplpszLabel,
	LPTSTR* lplpszType, LPTSTR* lplpszShortType, LPTSTR* lplpszLocation)
{
	COleClientItem* pItem = (COleClientItem*)dwObject;
	ASSERT_VALID(pItem);
	BOOL bIsLink = (pItem->GetType() == OT_LINK);

	if (lpdwObjSize != NULL)
	{
		ASSERT(pItem->m_lpStorage != NULL);

		// try ILockBytes first, then IStorage
		STATSTG statStg;
		if ((pItem->m_lpLockBytes == NULL ||
			pItem->m_lpLockBytes->Stat(&statStg, STATFLAG_NONAME) != S_OK) &&
			pItem->m_lpStorage->Stat(&statStg, STATFLAG_NONAME) != S_OK)
		{
			*lpdwObjSize = 0xFFFFFFFF;
		}
		else
		{
			ASSERT(statStg.pwcsName == NULL);
			if (statStg.cbSize.HighPart > 0)
				*lpdwObjSize = 0xFFFFFFFE;
			else if (statStg.cbSize.LowPart == 0)
				*lpdwObjSize = 0xFFFFFFFF;
			else
				*lpdwObjSize = statStg.cbSize.LowPart;
		}
	}

	if (lplpszLabel != NULL)
	{
		TCHAR szFormatLink[128];
		AfxLoadString(AFX_IDS_PASTELINKEDTYPE, szFormatLink, _countof(szFormatLink));
		TCHAR szFormatObj[] = _T("%s");
		LPTSTR lpszFormat = bIsLink ? szFormatLink : szFormatObj;
		CString strType;
		pItem->GetUserType(USERCLASSTYPE_FULL, strType);
		CString strResult;
		strResult.Format(lpszFormat, (LPCTSTR)strType);
		*lplpszLabel = AfxAllocTaskString(strResult);
	}

	if (lplpszType != NULL)
	{
		LPOLESTR lpOleStr;
		pItem->m_lpObject->GetUserType(USERCLASSTYPE_FULL, &lpOleStr);
		*lplpszType = TASKSTRINGOLE2T(lpOleStr);
	}

	if (lplpszShortType != NULL)
	{
		LPOLESTR lpOleStr;
		pItem->m_lpObject->GetUserType(USERCLASSTYPE_SHORT, &lpOleStr);
		*lplpszShortType = TASKSTRINGOLE2T(lpOleStr);
	}

	if (lplpszLocation != NULL)
	{
		if (bIsLink)
		{
			LPOLELINK lpOleLink = NULL;
			pItem->m_lpObject->QueryInterface(IID_IOleLink, (LPVOID*)&lpOleLink);
			ASSERT(lpOleLink != NULL);
			LPOLESTR lpOleStr;
			lpOleLink->GetSourceDisplayName(&lpOleStr);
			*lplpszLocation = TASKSTRINGOLE2T(lpOleStr);
			lpOleLink->Release();
		}
		else
		{
			CDocument* pDoc = (CDocument*)pItem->GetDocument();
			CString strLocation = pDoc->GetPathName();
			if (strLocation.IsEmpty())
				strLocation = pDoc->GetTitle();
			*lplpszLocation = AfxAllocTaskString(strLocation);
		}
	}

	return S_OK;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::GetConvertInfo(
	DWORD dwObject, CLSID* lpClassID, WORD* lpwFormat,
	CLSID*, LPCLSID*, UINT*)
{
	COleClientItem* pItem = (COleClientItem*)dwObject;
	ASSERT_VALID(pItem);

	if (lpClassID != NULL)
	{
		if (pItem->GetType() == OT_LINK ||
			S_OK != ReadClassStg(pItem->m_lpStorage, lpClassID))
		{
			pItem->GetClassID(lpClassID);
		}
	}
	if (lpwFormat != NULL)
	{
		*lpwFormat = 0;
		CLIPFORMAT cf;
		if (ReadFmtUserTypeStg(pItem->m_lpStorage, &cf, NULL) == S_OK)
			*lpwFormat = (WORD)cf;
	}

	// Note: leave rest at default

	return S_OK;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::ConvertObject(
	DWORD dwObject, REFCLSID clsidNew)
{
	COleClientItem* pItem = (COleClientItem*)dwObject;
	ASSERT_VALID(pItem);

	if (!pItem->ConvertTo(clsidNew))
	{
		AfxMessageBox(AFX_IDP_FAILED_TO_CONVERT,
			MB_OK | MB_ICONEXCLAMATION);
		return E_FAIL;
	}
	return S_OK;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::GetViewInfo(
	DWORD dwObject, HGLOBAL* phMetaPict, DWORD* pdvAspect, int* pnCurrentScale)
{
	COleClientItem* pItem = (COleClientItem*)dwObject;
	ASSERT_VALID(pItem);

	if (phMetaPict != NULL)
		*phMetaPict = pItem->GetIconicMetafile();

	if (pdvAspect != NULL)
		*pdvAspect = pItem->GetDrawAspect();

	if (pnCurrentScale != NULL)
		*pnCurrentScale = 100;  // 100% (arbitrary for now)

	return S_OK;
}

STDMETHODIMP COlePropertiesDialog::XOleUIObjInfo::SetViewInfo(
	DWORD dwObject, HGLOBAL hMetaPict, DWORD dvAspect,
	int nCurrentScale, BOOL bRelativeToOrig)
{
	METHOD_PROLOGUE_EX_(COlePropertiesDialog, OleUIObjInfo)
	COleClientItem* pItem = (COleClientItem*)dwObject;
	ASSERT_VALID(pItem);

	// handle aspect changes
	if (dvAspect != -1)
	{
		pItem->OnChange(OLE_CHANGED_ASPECT, dvAspect);
		pItem->SetDrawAspect((DVASPECT)dvAspect);

		// force scale to 100% when changing aspects
		if (dvAspect == DVASPECT_ICON)
		{
			nCurrentScale = 100;
			bRelativeToOrig = TRUE;
		}
		else if (nCurrentScale == -1)
		{
			nCurrentScale = 100;
			bRelativeToOrig = FALSE;
		}
	}

	// handle icon representation changes
	if (hMetaPict != NULL)
	{
		pItem->SetIconicMetafile(hMetaPict);
		if (pItem->GetDrawAspect() == DVASPECT_ICON)
			pItem->OnChange(OLE_CHANGED, (DWORD)DVASPECT_ICON);
	}

	// handle scale changes
	if (nCurrentScale != -1)
	{
		pThis->OnApplyScale(pItem, nCurrentScale, bRelativeToOrig);
	}

	return S_OK;
}

BOOL COlePropertiesDialog::OnApplyScale(COleClientItem*, int, BOOL)
{
	// Note: no default implementation. Must override or scaling
	//  controls will be disabled during the DoModal call.

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// COlePropertiesDialog diagnostics

#ifdef _DEBUG

void COlePropertiesDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);
}

void COlePropertiesDialog::AssertValid() const
{
	COleDialog::AssertValid();
	ASSERT(m_op.cbStruct == sizeof(m_op));
	ASSERT(m_gp.cbStruct == sizeof(m_gp));
	ASSERT(m_vp.cbStruct == sizeof(m_vp));
	ASSERT(m_lp.cbStruct == sizeof(m_lp));
}

#endif

////////////////////////////////////////////////////////////////////////////
// ChangeSource dialog wrapper

COleChangeSourceDialog::COleChangeSourceDialog(COleClientItem* pItem,
	CWnd* pParentWnd) : COleDialog(pParentWnd), m_xLinkInfo(NULL)
{
	ASSERT_VALID(pItem);

	memset(&m_cs, 0, sizeof(m_cs)); // initialize structure to 0/NULL

	// fill in common part
	m_cs.cbStruct = sizeof(m_cs);
	m_cs.dwFlags = 0;
	if (AfxHelpEnabled())
		m_cs.dwFlags |= CSF_SHOWHELP;
	m_cs.lpfnHook = AfxOleHookProc;
	m_nIDHelp = AFX_IDD_CHANGESOURCE;

	// specific to this dialog
	m_cs.lpOleUILinkContainer = &m_xLinkInfo;
	m_cs.dwLink = (DWORD)pItem;
}

int COleChangeSourceDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_cs.lpfnHook != NULL);  // can still be a user hook

	m_cs.hWndOwner = PreModal();
	int iResult = MapResult(::OleUIChangeSource(&m_cs));
	PostModal();
	return iResult;
}

void COleChangeSourceDialog::PreInitDialog()
{
	// automatic centering doesn't work for this dialog
	if (!(GetStyle() & WS_CHILD))
		CenterWindow();
}

COleChangeSourceDialog::~COleChangeSourceDialog()
{
	CoTaskMemFree(m_cs.lpszTo);
	CoTaskMemFree(m_cs.lpszFrom);
	CoTaskMemFree(m_cs.lpszDisplayName);
}

/////////////////////////////////////////////////////////////////////////////
// COleChangeSourceDialog diagnostics

#ifdef _DEBUG

void COleChangeSourceDialog::Dump(CDumpContext& dc) const
{
	COleDialog::Dump(dc);

	dc << "m_cs.cbStruct = " << m_cs.cbStruct;
	dc << "\nm_cs.dwFlags = " << (LPVOID)m_cs.dwFlags;
	dc << "\nm_cs.hWndOwner = " << (UINT)m_cs.hWndOwner;
	dc << "\nm_cs.lpszCaption = " << m_cs.lpszCaption;
	dc << "\nm_cs.lCustData = " << (LPVOID)m_cs.lCustData;
	dc << "\nm_cs.hInstance = " << (UINT)m_cs.hInstance;
	dc << "\nm_cs.lpszTemplate = " << (LPVOID)m_cs.lpszTemplate;
	dc << "\nm_cs.hResource = " << (UINT)m_cs.hResource;
	if (m_cs.lpfnHook == AfxOleHookProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";

	dc << "\n";
}

void COleChangeSourceDialog::AssertValid() const
{
	COleDialog::AssertValid();
	ASSERT(m_cs.cbStruct == sizeof(m_cs));
	ASSERT(m_cs.lpfnHook != NULL);
}

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COlePropertiesDialog, COleDialog)
IMPLEMENT_DYNAMIC(COleChangeSourceDialog, COleDialog)

/////////////////////////////////////////////////////////////////////////////
