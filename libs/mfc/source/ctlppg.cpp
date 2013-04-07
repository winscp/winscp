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

#ifdef AFXCTL_PAGE_SEG
#pragma code_seg(AFXCTL_PAGE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#define MAX_CLASS_NAME  100

struct NotifyInfo
{
	LPCTSTR     szClassName;        // Name of control's class
	WORD        wNotifyCode;        // Notification code
};

#define MAX_TEXT    1024

BEGIN_INTERFACE_MAP(COlePropertyPage, CDialog)
	INTERFACE_PART(COlePropertyPage, IID_IPropertyPage2, PropertyPage)
	INTERFACE_PART(COlePropertyPage, IID_IPropertyPage, PropertyPage)
END_INTERFACE_MAP()

BEGIN_MESSAGE_MAP(COlePropertyPage, CDialog)
	//{{AFX_MSG_MAP(COlePropertyPage)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
#ifndef _AFX_NO_CTL3D_SUPPORT
	ON_MESSAGE(WM_QUERY3DCONTROLS, OnQuery3dControls)
#endif
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// AFX_DDPDATA

struct AFX_DDPDATA : public CObject
{
	AFX_DDPDATA(LPVOID lpHandler, int nCtrlId, BOOL bEditCtrl, LPVOID lpMember,
		UINT nType, LPCTSTR lpszOleName);

	LPVOID  m_lpHandler;
	int     m_nCtrlId;
	BOOL    m_bEditCtrl;
	LPVOID  m_lpMember;
	UINT    m_nType;
	LPCTSTR m_lpszOleName;
};

AFX_DDPDATA::AFX_DDPDATA(LPVOID lpHandler, int nCtrlId, BOOL bEditCtrl,
	LPVOID lpMember, UINT nType, LPCTSTR lpszOleName) :
	m_lpHandler(lpHandler),
	m_nCtrlId(nCtrlId),
	m_bEditCtrl(bEditCtrl),
	m_lpMember(lpMember),
	m_nType(nType),
	m_lpszOleName(lpszOleName)
{
}

/////////////////////////////////////////////////////////////////////////////
// cleanup helper function

AFX_STATIC void AFXAPI _AfxCleanupDDPs(CPtrArray& arrayDDP)
{
	int cDDP = arrayDDP.GetSize();
	for (int i = 0; i < cDDP; i++)
	{
		ASSERT(arrayDDP[i] != NULL);
		delete (AFX_DDPDATA*)(arrayDDP[i]);
	}
	arrayDDP.RemoveAll();
}

/////////////////////////////////////////////////////////////////////////////
// page validation helper function (debug only)

#ifdef _DEBUG
void _ValidatePageDialog(CDialogTemplate& dt, CString& strPage,
	COlePropertyPage* pPage)
{
	if (GetSystemMetrics(SM_DBCSENABLED))
		return;

	// Only display the message boxes the first time a page is shown.
#ifdef _DEBUG
	BOOL bEnable = AfxEnableMemoryTracking(FALSE);
#endif
	static CPtrList _classList;
	BOOL bMessageBox = FALSE;
	CRuntimeClass* pClass = pPage->GetRuntimeClass();
	if (_classList.Find(pClass) == NULL)
	{
		bMessageBox = TRUE;
		_classList.AddHead(pClass);
	}
#ifdef _DEBUG
	AfxEnableMemoryTracking(bEnable);
#endif

	SIZE sizeDLU;
	CString strFontName;
	WORD wFontSize;
	LPTSTR pszTmp;

	dt.GetSizeInDialogUnits(&sizeDLU);
	dt.GetFont(strFontName, wFontSize);

	if ((sizeDLU.cx != 250) || ((sizeDLU.cy != 62) && (sizeDLU.cy != 110)))
	{
		pszTmp = new TCHAR[strPage.GetLength() + 128];

		wsprintf(pszTmp, _T("Property page '%s' has nonstandard dimensions."),
			(LPCTSTR)strPage);

		TRACE1("Warning: %s\n", pszTmp);

		if (bMessageBox)
		{
			lstrcat(pszTmp,
				_T(" Dimensions should be 250x62 or 250x110 dialog units."));
			pPage->MessageBox(pszTmp, _T("Property page warning"),
				MB_ICONEXCLAMATION);
		}

		delete [] pszTmp;
	}

	if ((wFontSize != 8) ||
		(strFontName != _T("MS Sans Serif") && strFontName != _T("Helv") &&
		strFontName != _T("MS Shell Dlg")))
	{
		pszTmp = new TCHAR[strPage.GetLength() + 128];

		wsprintf(pszTmp, _T("Property page '%s' uses a nonstandard font."),
			(LPCTSTR)strPage);

		TRACE1("Warning: %s\n", pszTmp);

		if (bMessageBox)
		{
			lstrcat(pszTmp, _T(" Font should be MS Sans Serif 8."));
			pPage->MessageBox(pszTmp, _T("Property page warning"),
				MB_ICONEXCLAMATION);
		}

		delete [] pszTmp;
	}
}
#endif // _DEBUG

/////////////////////////////////////////////////////////////////////////////
// COlePropertyPage::COlePropertyPage

COlePropertyPage::COlePropertyPage(UINT idDlg, UINT idCaption) :
#ifdef _DEBUG
	m_bNonStandardSize(FALSE),
#endif
	m_bDirty(FALSE),
	m_idCaption(idCaption),
	m_idDlg(idDlg),
	m_pPageSite(NULL),
	m_ppDisp(NULL),
	m_pAdvisors(NULL),
	m_bPropsChanged(FALSE),
	m_nObjects(0),
	m_bInitializing(TRUE),
	m_nControls(0),
	m_pStatus(NULL),
	m_hDialog(NULL),
	m_dwHelpContext(0)
{
	// m_lpDialogTemplate is needed later by CWnd::ExecuteDlgInit
	m_lpszTemplateName = MAKEINTRESOURCE(m_idDlg);

	// Keep this DLL loaded at least until this object is deleted.
	AfxOleLockApp();
}

void COlePropertyPage::OnSetPageSite()
{
	// Load the caption from resources
	CString strCaption;
	if (!strCaption.LoadString(m_idCaption))
		strCaption.LoadString(AFX_IDS_PROPPAGE_UNKNOWN);
	SetPageName(strCaption);

	// Try to load the dialog resource template into memory and get its size
	m_sizePage.cx = 0;
	m_sizePage.cy = 0;

	CDialogTemplate dt;
	dt.Load(MAKEINTRESOURCE(m_idDlg));

#ifdef _DEBUG
	if (!m_bNonStandardSize)
		_ValidatePageDialog(dt, strCaption, this);
#endif

	// If no font specified, set the system font.
	BOOL bSetSysFont = !dt.HasFont();
	CString strFace;
	WORD wSize;

	// On DBCS systems, also change "MS Sans Serif" or "Helv" to system font.
	if ((!bSetSysFont) && GetSystemMetrics(SM_DBCSENABLED))
	{
		CString strFace;
		dt.GetFont(strFace, wSize);
		bSetSysFont = (strFace == _T("MS Shell Dlg") ||
			strFace == _T("MS Sans Serif") || strFace == _T("Helv"));
	}

	// Here is where we actually set the font.
	if (bSetSysFont && AfxGetPropSheetFont(strFace, wSize, FALSE))
		dt.SetFont(strFace, wSize);

	dt.GetSizeInPixels(&m_sizePage);
	m_hDialog = dt.Detach();
}

BOOL COlePropertyPage::OnInitDialog()
{
	CDialog::OnInitDialog();
	return 0;
}

BOOL COlePropertyPage::PreTranslateMessage(LPMSG lpMsg)
{
	// Don't let CDialog process the Return key or Escape key.
	if ((lpMsg->message == WM_KEYDOWN) &&
		((lpMsg->wParam == VK_RETURN) || (lpMsg->wParam == VK_ESCAPE)))
	{
		// Special case: if control with focus is an edit control with
		// ES_WANTRETURN style, let it handle the Return key.

		TCHAR szClass[10];
		CWnd* pWndFocus = GetFocus();
		if ((lpMsg->wParam == VK_RETURN) &&
			((pWndFocus = GetFocus()) != NULL) &&
			IsChild(pWndFocus) &&
			(pWndFocus->GetStyle() & ES_WANTRETURN) &&
			GetClassName(pWndFocus->m_hWnd, szClass, 10) &&
			(lstrcmpi(szClass, _T("EDIT")) == 0))
		{
			pWndFocus->SendMessage(WM_CHAR, lpMsg->wParam, lpMsg->lParam);
			return TRUE;
		}

		return FALSE;
	}

	// If it's a WM_SYSKEYDOWN, temporarily replace the hwnd in the
	// message with the hwnd of our first control, and try to handle
	// the message for ourselves.

	BOOL bHandled;

	if ((lpMsg->message == WM_SYSKEYDOWN) && !::IsChild(m_hWnd, lpMsg->hwnd))
	{
		HWND hWndSave = lpMsg->hwnd;
		lpMsg->hwnd = ::GetWindow(m_hWnd, GW_CHILD);
		bHandled = CDialog::PreTranslateMessage(lpMsg);
		lpMsg->hwnd = hWndSave;
	}
	else
	{
		bHandled = CDialog::PreTranslateMessage(lpMsg);
	}

	return bHandled;
}

void COlePropertyPage::CleanupObjectArray()
{
	if (m_pAdvisors)
	{
		for (ULONG nObject = 0; nObject < m_nObjects; nObject++)
			AfxConnectionUnadvise(m_ppDisp[nObject], IID_IPropertyNotifySink,
				&m_xPropNotifySink, FALSE, m_pAdvisors[nObject]);
		delete [] m_pAdvisors;
		m_pAdvisors = NULL;
	}
	if (m_ppDisp)
	{
		for (ULONG nObject = 0; nObject < m_nObjects; nObject++)
			RELEASE(m_ppDisp[nObject]);
		delete [] m_ppDisp;
		m_ppDisp = NULL;
	}
}

COlePropertyPage::~COlePropertyPage()
{
	// Remember to free the resource we loaded!
	if (m_hDialog != NULL)
		GlobalFree(m_hDialog);

	if (m_pStatus != NULL)
	{
		delete [] m_pStatus;
		m_pStatus = NULL;
	}

	// Clean out any leftovers in the DDP map.
	_AfxCleanupDDPs(m_arrayDDP);

	RELEASE(m_pPageSite);
	CleanupObjectArray();
	AfxOleUnlockApp();
}

void COlePropertyPage::OnFinalRelease()
{
	if (m_hWnd != NULL)
		DestroyWindow();

	delete this;
}

LRESULT COlePropertyPage::WindowProc(UINT msg, WPARAM wParam, LPARAM lParam)
{
	AFX_MANAGE_STATE(m_pModuleState);

	// Forward WM_SYSCOMMAND messages to the frame for translation
	if (msg == WM_SYSCOMMAND && (wParam & 0xFFF0) != SC_KEYMENU && m_pPageSite != NULL)
	{
		if (m_pPageSite->TranslateAccelerator((LPMSG)GetCurrentMessage())
			== S_OK)
		{
			return 0;
		}
	}

	return CDialog::WindowProc(msg, wParam, lParam);
}

HBRUSH COlePropertyPage::OnCtlColor(CDC*, CWnd* pWnd, UINT)
{
	// allow the control itself first crack at the color
	LRESULT lResult;
	if (pWnd->SendChildNotifyLastMsg(&lResult))
		return (HBRUSH)lResult;

	// allow the parent window to determine the color
	HWND hParent = ::GetParent(m_hWnd);
	const MSG* pMsg = GetCurrentMessage();
	HBRUSH hResult = (HBRUSH)::SendMessage(hParent,
		pMsg->message, pMsg->wParam, pMsg->lParam);

	// use default if parent returns NULL
	if (hResult == NULL)
		hResult = (HBRUSH)Default();
	return hResult;
}

BOOL COlePropertyPage::OnHelp(LPCTSTR)
{
	// May be overridden by subclass.
	return FALSE;
}

BOOL COlePropertyPage::OnEditProperty(DISPID)
{
	// May be overridden by subclass.
	return FALSE;
}

void COlePropertyPage::OnObjectsChanged()
{
	// May be overridden by subclass.
}

LPDISPATCH* COlePropertyPage::GetObjectArray(ULONG* pnObjects)
{
	ASSERT_POINTER(pnObjects, ULONG);

	if (pnObjects != NULL)
		*pnObjects = m_nObjects;

	return m_ppDisp;
}

void COlePropertyPage::SetModifiedFlag(BOOL bModified)
{
	if (!bModified)
		m_bPropsChanged = FALSE;

	if ((m_bDirty && !bModified) || (!m_bDirty && bModified))
	{
		m_bDirty = bModified;

		if (m_pPageSite != NULL)
		{
			DWORD flags = 0;
			if (bModified)
				flags |= PROPPAGESTATUS_DIRTY;

			m_pPageSite->OnStatusChange(flags);
		}
	}
}

BOOL COlePropertyPage::IsModified()
{
	return m_bDirty;
}

void COlePropertyPage::SetPageName(LPCTSTR lpszPageName)
{
	ASSERT(AfxIsValidString(lpszPageName));
	m_strPageName = lpszPageName;
}

void COlePropertyPage::SetDialogResource(HGLOBAL hDialog)
{
	if (m_hDialog != NULL)
	{
		GlobalFree(m_hDialog);
		m_hDialog = NULL;
	}

	CDialogTemplate dt(hDialog);

#ifdef _DEBUG
	_ValidatePageDialog(dt, m_strPageName, this);
#endif

	dt.GetSizeInPixels(&m_sizePage);
	m_hDialog = dt.Detach();
}

void COlePropertyPage::SetHelpInfo(LPCTSTR lpszDocString,
	LPCTSTR lpszHelpFile, DWORD dwHelpContext)
{
	ASSERT((lpszDocString == NULL) || AfxIsValidString(lpszDocString));
	ASSERT((lpszHelpFile == NULL) || AfxIsValidString(lpszHelpFile));

	m_strDocString = lpszDocString;
	m_strHelpFile = lpszHelpFile;
	m_dwHelpContext = dwHelpContext;
}

LPPROPERTYPAGESITE COlePropertyPage::GetPageSite()
{
	return m_pPageSite;
}

int COlePropertyPage::MessageBox(LPCTSTR lpszText, LPCTSTR lpszCaption,
	UINT nType)
{
	// use caption of page by default
	if (lpszCaption == NULL)
		lpszCaption = m_strPageName;

	// start message box on safe owner of the page
	return ::MessageBox(GetSafeOwner_(m_hWnd, NULL), lpszText, lpszCaption, nType);
}

/////////////////////////////////////////////////////////////////////////////
// COlePropertyPage::XPropertyPage

STDMETHODIMP_(ULONG) COlePropertyPage::XPropertyPage::AddRef()
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COlePropertyPage::XPropertyPage::Release()
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COlePropertyPage::XPropertyPage::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COlePropertyPage::XPropertyPage::SetPageSite(
	LPPROPERTYPAGESITE pPageSite)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);
	ASSERT_POINTER(pPageSite, IPropertyPageSite);

	RELEASE(pThis->m_pPageSite);            // release the old one

	pThis->m_pPageSite = pPageSite;
	if (pPageSite != NULL)
		pThis->m_pPageSite->AddRef();       // Bump the reference count

	pThis->OnSetPageSite();
	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Activate(HWND hWndParent,
	LPCRECT pRect, BOOL)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);
	ASSERT_NULL_OR_POINTER(pRect, RECT);

	BOOL bSuccess = FALSE;  // Did we successfully create the dialog box

	if (pThis->m_hDialog != NULL)
	{
		// We've already loaded the dialog template into memory so just
		// create it!

		void* lpDialogTemplate = LockResource(pThis->m_hDialog);
		if (lpDialogTemplate != NULL)
		{
			bSuccess = pThis->CreateIndirect(lpDialogTemplate, CWnd::FromHandle(hWndParent));
			UnlockResource(pThis->m_hDialog);
		}
		else
			bSuccess = pThis->Create(pThis->m_idDlg, CWnd::FromHandle(hWndParent));
	}
	else
		bSuccess = pThis->Create(pThis->m_idDlg, CWnd::FromHandle(hWndParent));

	// Were we successful in creating the dialog box!
	if (bSuccess)
	{
		pThis->MoveWindow(pRect);       // Force page to fill area given by frame *
		pThis->m_bInitializing = TRUE;
		pThis->UpdateData(FALSE);
		pThis->SetModifiedFlag(FALSE);
		pThis->m_bInitializing = FALSE;

		if (pThis->m_pStatus != NULL)
		{
			delete [] pThis->m_pStatus;
			pThis->m_pStatus = NULL;
		}

		pThis->m_nControls = 0;
		::EnumChildWindows(pThis->GetSafeHwnd(), (WNDENUMPROC) COlePropertyPage::EnumChildProc, (LPARAM) pThis);

		if (pThis->m_nControls > 0)
			pThis->m_pStatus = new AFX_PPFIELDSTATUS [UINT(pThis->m_nControls)];

		pThis->m_nControls = 0;
		EnumChildWindows(pThis->GetSafeHwnd(), (WNDENUMPROC) COlePropertyPage::EnumControls, (LPARAM) pThis);

		return S_OK;
	}

	return E_FAIL;
}

BOOL CALLBACK COlePropertyPage::EnumChildProc(HWND, LPARAM lParam)
{
	COlePropertyPage* pDlg = (COlePropertyPage*) lParam;
	ASSERT_POINTER(pDlg, COlePropertyPage);

	pDlg->m_nControls++;
	return TRUE;
}

BOOL CALLBACK COlePropertyPage::EnumControls(HWND hWnd, LPARAM lParam)
{
	COlePropertyPage* pDlg = (COlePropertyPage*) lParam;
	ASSERT_POINTER(pDlg, COlePropertyPage);
	ASSERT(pDlg->m_pStatus != NULL);

	pDlg->m_pStatus[pDlg->m_nControls].nID = (UINT)::GetDlgCtrlID(hWnd);
	pDlg->m_pStatus[pDlg->m_nControls].bDirty = FALSE;
	pDlg->m_nControls++;

	return TRUE;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Deactivate()
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	pThis->DestroyWindow();

	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::GetPageInfo(
	LPPROPPAGEINFO pPageInfo)
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)
	ASSERT_POINTER(pPageInfo, PROPPAGEINFO);

	pPageInfo->pszTitle = AfxAllocTaskOleString(pThis->m_strPageName);
	pPageInfo->size = pThis->m_sizePage;
	pPageInfo->pszDocString = AfxAllocTaskOleString(pThis->m_strDocString);
	pPageInfo->pszHelpFile = AfxAllocTaskOleString(pThis->m_strHelpFile);
	pPageInfo->dwHelpContext = pThis->m_dwHelpContext;
	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::SetObjects(
		ULONG cObjects, LPUNKNOWN* ppUnk)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);

	pThis->CleanupObjectArray();

	if (cObjects != 0)
	{
		ASSERT(AfxIsValidAddress(ppUnk, sizeof(LPUNKNOWN) * (int)cObjects, FALSE));

		pThis->m_ppDisp = new LPDISPATCH [(UINT)cObjects];
		pThis->m_pAdvisors = new DWORD [(UINT)cObjects];
		for (ULONG nObject = 0; nObject < cObjects; nObject++)
		{
			HRESULT hr = ppUnk[nObject]->QueryInterface(IID_IDispatch,
				(VOID**)&(pThis->m_ppDisp[nObject]));
			if (SUCCEEDED(hr))
			{
				AfxConnectionAdvise(ppUnk[nObject], IID_IPropertyNotifySink,
					&pThis->m_xPropNotifySink, FALSE,
					&pThis->m_pAdvisors[nObject]);
			}
			else
			{
				return hr;
			}
		}
	}

	pThis->m_nObjects = cObjects;

	// No painting during the data update.
	BOOL bLock = (pThis->m_hWnd != NULL) && pThis->IsWindowVisible();
	if (bLock)
		::LockWindowUpdate(pThis->m_hWnd);

	pThis->OnObjectsChanged();

	// If window exists, update the data in its fields.
	if (cObjects != 0 && pThis->m_hWnd != NULL)
	{
		pThis->UpdateData(FALSE);
		pThis->SetModifiedFlag(FALSE);
	}

	if (bLock)
		::LockWindowUpdate(NULL);

	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Show(UINT nCmdShow)
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)

	pThis->ShowWindow(nCmdShow);
	if (nCmdShow == SW_SHOWNORMAL)
		pThis->SetFocus();
	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Move(LPCRECT pRect)
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)
	ASSERT_POINTER(pRect, RECT);

	pThis->MoveWindow(pRect);
	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::IsPageDirty()
{
	METHOD_PROLOGUE_EX_(COlePropertyPage, PropertyPage)

	return pThis->m_bDirty ? S_OK : S_FALSE;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Apply()
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);

	HRESULT hr = S_OK;
	BOOL bClean = FALSE;

	if (pThis->m_bDirty)
	{
		if (pThis->UpdateData(TRUE))
		{
			pThis->m_bDirty = FALSE;
			bClean = TRUE;
		}
		else
			hr = E_FAIL;

		if (pThis->m_bPropsChanged)
		{
			pThis->UpdateData(FALSE);
			pThis->m_bPropsChanged = FALSE;
			bClean = TRUE;
		}

		if (bClean)
		{
			// Set the dirty status of all the controls on this page to FALSE.
			if (pThis->m_pStatus != NULL)
			{
				for (int nControl = 0; nControl < pThis->m_nControls; nControl++)
					pThis->m_pStatus[nControl].bDirty = FALSE;
			}
		}
	}
	else
		ASSERT(!pThis->m_bPropsChanged);

	return hr;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::Help(LPCOLESTR lpszHelpDir)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);
	ASSERT((lpszHelpDir == NULL) || AfxIsValidString(lpszHelpDir));

	USES_CONVERSION;

	if (!pThis->OnHelp(OLE2CT(lpszHelpDir)))
		return S_FALSE;
	else
		return S_OK;
}

BOOL AFXAPI _AfxAtEndOfTabList(CDialog* pDlg, UINT nCmd)
{
	if ((pDlg->SendMessage(WM_GETDLGCODE) &
		(DLGC_WANTALLKEYS | DLGC_WANTMESSAGE | DLGC_WANTTAB)) == 0)
	{
		CWnd* pCtl = CWnd::GetFocus();
		if (pDlg->IsChild(pCtl))
		{
			// Get top level child for controls with children, like combo.
			while (pCtl->GetParent() != pDlg)
			{
				pCtl = pCtl->GetParent();
				ASSERT_VALID(pCtl);
			}

			do
			{
				if ((pCtl = pCtl->GetWindow(nCmd)) == NULL)
					return TRUE;
			}
			while ((pCtl->GetStyle() & (WS_DISABLED | WS_TABSTOP)) != WS_TABSTOP);
		}
	}

	return FALSE;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::TranslateAccelerator(LPMSG lpMsg)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);
	ASSERT_POINTER(lpMsg, MSG);

	BOOL bHandled = FALSE;

	if (lpMsg->message == WM_KEYDOWN && lpMsg->wParam == VK_TAB &&
		GetKeyState(VK_CONTROL) >= 0)
	{
		if (pThis->IsChild(CWnd::GetFocus()))
		{
			// We already have the focus.  Let's determine whether we should
			// pass focus up to the frame.

			if (_AfxAtEndOfTabList(pThis, GetKeyState(VK_SHIFT) < 0 ?
				GW_HWNDPREV : GW_HWNDNEXT))
			{
				// fix for default button border
				DWORD dwDefID = pThis->GetDefID();
				if (HIWORD(dwDefID) == DC_HASDEFID)
				{
					CWnd *pDefBtn = pThis->GetDlgItem(LOWORD(dwDefID));
					if (pDefBtn != NULL && pDefBtn->IsWindowEnabled())
						pThis->GotoDlgCtrl(pDefBtn);
				}

				// Pass focus to the frame by letting the page site handle
				// this message.
				if (pThis->m_pPageSite != NULL)
					bHandled =
						pThis->m_pPageSite->TranslateAccelerator(lpMsg) ==
						S_OK;
			}
		}
		else
		{
			// We don't already have the focus.  The frame is passing the
			// focus to us.

			CWnd* pWnd = pThis->GetTopWindow();
			if (pWnd != NULL)
			{
				UINT gwInit;
				UINT gwMove;

				if (GetKeyState(VK_SHIFT) >= 0)
				{
					// Set the focus to the first tabstop in the page.
					gwInit = GW_HWNDFIRST;
					gwMove = GW_HWNDNEXT;
				}
				else
				{
					// Set the focus to the last tabstop in the page.
					gwInit = GW_HWNDLAST;
					gwMove = GW_HWNDPREV;
				}

				pWnd = pWnd->GetWindow(gwInit);
				while (pWnd != NULL)
				{
					if ((pWnd->GetStyle() & (WS_DISABLED | WS_TABSTOP)) ==
						WS_TABSTOP)
					{
						pThis->GotoDlgCtrl(pWnd);
						bHandled = TRUE;
						break;
					}

					pWnd = pWnd->GetWindow(gwMove);
				}
			}
		}
	}

	// If message was not handled here, call PreTranslateMessage
	return (bHandled || pThis->PreTranslateMessage(lpMsg)) ?
		S_OK : S_FALSE;
}

STDMETHODIMP COlePropertyPage::XPropertyPage::EditProperty(DISPID dispid)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropertyPage)
	ASSERT_VALID(pThis);

	return pThis->OnEditProperty(dispid) ? S_OK : E_NOTIMPL;
}

/////////////////////////////////////////////////////////////////////////////
// COlePropertyPage::XPropNotifySink

STDMETHODIMP_(ULONG) COlePropertyPage::XPropNotifySink::AddRef()
{
	return 1;
}

STDMETHODIMP_(ULONG) COlePropertyPage::XPropNotifySink::Release()
{
	return 0;
}

STDMETHODIMP COlePropertyPage::XPropNotifySink::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	if (IsEqualIID(iid, IID_IPropertyNotifySink) ||
		IsEqualIID(iid, IID_IUnknown))
	{
		*ppvObj = this;
		return S_OK;
	}
	else
	{
		*ppvObj = NULL;
		return E_NOINTERFACE;
	}
}

STDMETHODIMP COlePropertyPage::XPropNotifySink::OnRequestEdit(DISPID)
{
	return S_OK;
}

STDMETHODIMP COlePropertyPage::XPropNotifySink::OnChanged(DISPID)
{
	METHOD_PROLOGUE_EX(COlePropertyPage, PropNotifySink)

	// If we're not currently in the middle of an UpdateData, call it now.
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	if (pThis->m_hWnd != NULL &&
		pThreadState->m_hLockoutNotifyWindow != pThis->m_hWnd)
	{
		pThis->UpdateData(FALSE);
	}
	else
	{
		pThis->m_bPropsChanged = TRUE;
	}

	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// Handle control notifications

AFX_STATIC_DATA const NotifyInfo _afxNotifyList[] = {
	{ _T("edit"),       EN_CHANGE },
	{ _T("button"),     BN_CLICKED },
	{ _T("button"),     BN_DOUBLECLICKED },
	{ _T("combobox"),   CBN_EDITCHANGE },
	{ _T("combobox"),   CBN_SELCHANGE },
	{ _T("listbox"),    LBN_SELCHANGE },
};

AFX_STATIC_DATA const NotifyInfo _afxUpdateList[] = {
	{ _T("edit"),       EN_KILLFOCUS },
	{ _T("button"),     BN_CLICKED },
	{ _T("button"),     BN_DOUBLECLICKED },
	{ _T("combobox"),   CBN_SELCHANGE },
	{ _T("combobox"),   CBN_KILLFOCUS },
	{ _T("listbox"),    LBN_SELCHANGE },
};

BOOL AFXAPI _AfxIsRadioButton(HWND hWnd)
{
	DWORD dwButtonStyle = GetWindowLong(hWnd, GWL_STYLE) & 0x0000000FL;
	return ((dwButtonStyle == BS_RADIOBUTTON) ||
			(dwButtonStyle == BS_AUTORADIOBUTTON));
}

BOOL COlePropertyPage::OnCommand(WPARAM wParam, LPARAM lParam)
{
	// Let the base class process the message first
	BOOL bSuccess = CDialog::OnCommand(wParam, lParam);

	// Are we just initializing the dialog box, or do we have no objects?
	if (m_bInitializing || m_ppDisp == NULL)
		return bSuccess;

	UINT nID = (UINT)LOWORD(wParam);
	HWND hWndCtl = (HWND)lParam;
	WORD wNotifyCode = HIWORD(wParam);

	DWORD flags = 0;

	if (hWndCtl != NULL)
	{
		BOOL bIgnoreControl = FALSE;
		for (int count = 0; count < m_IDArray.GetSize(); count++)
		{
			UINT nNotID = m_IDArray.GetAt(count);
			if (nID == nNotID)
				bIgnoreControl = TRUE;
		}

		if ( !bIgnoreControl )
		{
			TCHAR szClassName[MAX_CLASS_NAME];

			// We have a control message - check type of control and message
			::GetClassName(hWndCtl, (LPTSTR)szClassName, MAX_CLASS_NAME);

			for (int iNotify = 0; iNotify < _countof(_afxNotifyList); iNotify++)
			{
				if (lstrcmpi(_afxNotifyList[iNotify].szClassName, szClassName) == 0
				 && _afxNotifyList[iNotify].wNotifyCode == wNotifyCode )
				{
					if ((lstrcmpi(szClassName, _T("button")) == 0) &&
						_AfxIsRadioButton(hWndCtl))
					{
						// Special case for radio buttons:
						// mark first button in group

						while ((hWndCtl != NULL) &&
								!(GetWindowLong(hWndCtl, GWL_STYLE) & WS_GROUP))
						{
							hWndCtl = ::GetWindow(hWndCtl, GW_HWNDPREV);
						}

						// First button in group must have WS_GROUP style,
						// and must be a radio button.
						ASSERT(hWndCtl != NULL);
						ASSERT(_AfxIsRadioButton(hWndCtl));

						// Mark first radio button as dirty
						if (hWndCtl != NULL)
							nID = ::GetWindowLong(hWndCtl, GWL_ID);
					}

					// Control has been modified
					m_bDirty = TRUE;
					SetControlStatus(nID, TRUE);

					flags = PROPPAGESTATUS_DIRTY;
					break;
				}
			}

			if (m_bDirty)
			{
				for (int iNotify=0; iNotify < _countof(_afxUpdateList); iNotify++)
				{
					if (lstrcmpi(_afxUpdateList[iNotify].szClassName, szClassName)==0 &&
						_afxUpdateList[iNotify].wNotifyCode == wNotifyCode &&
						GetControlStatus(nID))
					{
						flags |= PROPPAGESTATUS_VALIDATE;
					}
				}
			}
		}
	}

	if (flags != 0)
	{
		ASSERT(m_pPageSite != NULL);
		m_pPageSite->OnStatusChange(flags);
	}

	return bSuccess;
}

void COlePropertyPage::IgnoreApply(UINT nID)
{
	m_IDArray.Add(nID);
}

BOOL COlePropertyPage::GetControlStatus(UINT nID)
{
	for (int nControl = 0; nControl < m_nControls; nControl++)
		if (m_pStatus[nControl].nID == nID)
			return m_pStatus[nControl].bDirty;

	// If we couldn't find the control - assume it is dirty
	return TRUE;
}

BOOL COlePropertyPage::SetControlStatus(UINT nID, BOOL bDirty)
{
	for (int nControl = 0; nControl < m_nControls; nControl++)
		if (m_pStatus[nControl].nID == nID)
		{
			m_pStatus[nControl].bDirty = bDirty;
			return TRUE;
		}
	return FALSE;
}

//////////////////////////////////////////////////////////////////////////////
// Function Templates using the Preprocessor
// [This should be replaced with C++ Templates when the 16 bit compiler allows]

#define DEFINE_GET_SET_PROP(ctype,casttype,vttype) \
BOOL COlePropertyPage::SetPropText(LPCTSTR pszPropName, ctype& data ) \
{ \
	USES_CONVERSION;\
	COleDispatchDriver PropDispDriver; \
	BOOL bResult = FALSE; \
	for (ULONG i = 0; i < m_nObjects; i++) \
	{ \
		DISPID dwDispID; \
		LPCOLESTR lpOleStr = T2COLE(pszPropName);\
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID))) \
		{ \
			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE); \
			PropDispDriver.SetProperty(dwDispID, vttype, (casttype)data ); \
			PropDispDriver.DetachDispatch(); \
			bResult = TRUE; \
		} \
	} \
	return bResult; \
} \
BOOL COlePropertyPage::GetPropText(LPCTSTR pszPropName, ctype *data ) \
{ \
	USES_CONVERSION;\
	COleDispatchDriver PropDispDriver; \
	BOOL bSuccess = FALSE; \
	for (ULONG i = 0; i < m_nObjects; i++) \
	{ \
		DISPID dwDispID; \
		LPCOLESTR lpOleStr = T2COLE(pszPropName);\
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID))) \
		{ \
			ctype dataTemp; \
			static ctype fill; \
			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE); \
			PropDispDriver.GetProperty(dwDispID, vttype, &dataTemp); \
			PropDispDriver.DetachDispatch(); \
			if (i == 0) *data = dataTemp; \
			if (*data != dataTemp) *data = fill; \
			bSuccess = TRUE; \
		} \
	} \
	return bSuccess; \
}

/////////////////////////////////////////////////////////////////////////////
// DDP_ property get/set helpers

DEFINE_GET_SET_PROP( BYTE, BYTE, VT_UI1 );
DEFINE_GET_SET_PROP( short, short, VT_I2 );
DEFINE_GET_SET_PROP( int, int, VT_I4 );
DEFINE_GET_SET_PROP( UINT, UINT, VT_I4 );
DEFINE_GET_SET_PROP( long, long, VT_I4 );
DEFINE_GET_SET_PROP( DWORD, DWORD, VT_I4 );
DEFINE_GET_SET_PROP( float, float, VT_R4 );
DEFINE_GET_SET_PROP( double, double, VT_R8 );
DEFINE_GET_SET_PROP( CString, LPCTSTR, VT_BSTR );

BOOL COlePropertyPage::SetPropCheck(LPCTSTR pszPropName, int Value)
{
	USES_CONVERSION;

	COleDispatchDriver PropDispDriver;
	BOOL bResult = FALSE;
	BOOL bValue;

	if (Value == 1)
		bValue = TRUE;
	else
		bValue = FALSE;         // default to off

	// Set the properties for all the objects
	for (ULONG i = 0; i < m_nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful set the value of the property
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID)))
		{
			// Set property
			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE);
			PropDispDriver.SetProperty(dwDispID, VT_BOOL, bValue);
			PropDispDriver.DetachDispatch();
			bResult = TRUE;
		}
	}
	return bResult;
}

BOOL COlePropertyPage::GetPropCheck(LPCTSTR pszPropName, int* pValue)
{
	USES_CONVERSION;

	COleDispatchDriver PropDispDriver;
	BOOL bSuccess = FALSE;

	// Check the property values for all the objects
	for (ULONG i = 0; i < m_nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful get the value of the property
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID)))
		{
			// Get property
			BOOL bTemp = FALSE;
			int tempValue;

			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE);
			PropDispDriver.GetProperty(dwDispID, VT_BOOL, &bTemp);
			PropDispDriver.DetachDispatch();

			// Convert boolean value to check box equivalent
			if (bTemp)
				tempValue = 1;
			else
				tempValue = 0;

			// Handle special case for first object
			if (i == 0)
				*pValue = tempValue;

			// If the current check value is not the same as the one just retrieved then
			// set the current check value to the indeterminate state.
			if (tempValue != *pValue)
				*pValue = 2;

			bSuccess = TRUE;
		}
	}
	return bSuccess;
}

BOOL COlePropertyPage::SetPropRadio(LPCTSTR pszPropName, int Value)
{
	USES_CONVERSION;

	COleDispatchDriver PropDispDriver;
	BOOL bSuccess = FALSE;

	// Set the properties for all the objects
	for (ULONG i = 0; i < m_nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful set the value of the property
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID)))
		{
			short nTemp = (short)Value;

			// Set property
			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE);
			PropDispDriver.SetProperty(dwDispID, VT_I2, nTemp);
			PropDispDriver.DetachDispatch();
			bSuccess = TRUE;
		}
	}
	return bSuccess;
}

BOOL COlePropertyPage::GetPropRadio(LPCTSTR pszPropName, int* pValue)
{
	USES_CONVERSION;

	COleDispatchDriver PropDispDriver;
	BOOL bSuccess = FALSE;

	// Check the property values for all the objects
	for (ULONG i = 0; i < m_nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful get the value of the property
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(m_ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr, 1, 0, &dwDispID)))
		{
			short nTemp;

			// Get property
			PropDispDriver.AttachDispatch(m_ppDisp[i], FALSE);
			PropDispDriver.GetProperty(dwDispID, VT_I2, &nTemp);
			PropDispDriver.DetachDispatch();

			// Handle special case for first object
			if (i == 0)
				*pValue = nTemp;

			// Compare the current radio value with the one just retrieved then
			// if they are different then set the radio value to -1, so that no
			// radio buttons will be checked.
			if (nTemp != *pValue)
				*pValue = -1;

			bSuccess = TRUE;
		}
	}
	return bSuccess;
}

BOOL COlePropertyPage::SetPropIndex(LPCTSTR pszPropName, int Value)
{
	return SetPropRadio(pszPropName, Value);
}

BOOL COlePropertyPage::GetPropIndex(LPCTSTR pszPropName, int* pValue)
{
	return GetPropRadio(pszPropName, pValue);
}

/////////////////////////////////////////////////////////////////////////////
// DDP_Begin data exchange routines (Should be C++ templated someday!)

#define DEFINE_DDP_(group,ctype,vtype,bEditCtrl) \
void AFXAPI DDP_End##group(CDataExchange* pDX, int, \
						   ctype *member, LPCTSTR pszPropName ) \
{ \
	COlePropertyPage* propDialog = STATIC_DOWNCAST(COlePropertyPage, pDX->m_pDlgWnd); \
	if (pDX->m_bSaveAndValidate) \
		propDialog->SetProp##group(pszPropName, *member ); \
} \
void AFXAPI DDP_##group(CDataExchange* pDX, int nCtrlId, \
						ctype &member, LPCTSTR pszPropName ) \
{ \
	ASSERT(AfxIsValidString(pszPropName)); \
	COlePropertyPage* propDialog = STATIC_DOWNCAST(COlePropertyPage, pDX->m_pDlgWnd); \
	if (pDX->m_bSaveAndValidate) /*Are we Saving?*/ \
	{ \
		if (propDialog->GetControlStatus(nCtrlId)) /*Is Control Dirty?*/ \
		{ \
			void (AFXAPI *pfv)(CDataExchange*,int,ctype*,LPCTSTR) = \
				 DDP_End##group; \
			AFX_DDPDATA *pDDP = new AFX_DDPDATA( (void *)pfv, nCtrlId, bEditCtrl, \
												 (void*)&member, (UINT)vtype, \
												 pszPropName ); \
			propDialog->m_arrayDDP.Add(pDDP); \
		} \
	} \
	else /* Loading data from properties! */ \
	{ \
			propDialog->GetProp##group(pszPropName, &member); \
			propDialog->SetControlStatus(nCtrlId,FALSE); \
	} \
}

/////////////////////////////////////////////////////////////////////////////
// DDP Functions (Pseudo Template Generation)

DEFINE_DDP_(Text,BYTE,VT_UI1,TRUE);
DEFINE_DDP_(Text,short,VT_I2,TRUE);
DEFINE_DDP_(Text,int,VT_I4,TRUE);
DEFINE_DDP_(Text,UINT,VT_UI4,TRUE);
DEFINE_DDP_(Text,long,VT_I4,TRUE);
DEFINE_DDP_(Text,DWORD,VT_UI4,TRUE);
DEFINE_DDP_(Text,float,VT_R4,TRUE);
DEFINE_DDP_(Text,double,VT_R8,TRUE);
DEFINE_DDP_(Text,CString,VT_BSTR,TRUE);
DEFINE_DDP_(Check,BOOL,VT_I4,FALSE);
DEFINE_DDP_(Radio,int,VT_I4,FALSE);

//////////////////////////////////////////////////////////////////////////////
// DDP Deferred Property Write Handler

void AFXAPI DDP_PostProcessing(CDataExchange*pDX)
{
	if (pDX->m_bSaveAndValidate)
	{
		CPtrArray &arrayDDP =
			((COlePropertyPage *)pDX->m_pDlgWnd)->m_arrayDDP;
		AFX_DDPDATA *pDDP = NULL;
		int cDDP = arrayDDP.GetSize();
		for (int i = 0 ; i < cDDP; i++)
		{
			pDDP = (AFX_DDPDATA*)arrayDDP[i];
			TRY
			{
				if (pDDP->m_bEditCtrl)
					pDX->PrepareEditCtrl(pDDP->m_nCtrlId);
				else
					pDX->PrepareCtrl(pDDP->m_nCtrlId);

				switch( pDDP->m_nType )
				{
					case    VT_I1:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   char *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (char *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					case    VT_UI1:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   BYTE *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (BYTE *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );

						break;
					}
					case    VT_I2:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   short *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (short *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					case    VT_UI2:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   WORD *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (WORD *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					case    VT_I4:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   long *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (long *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					case    VT_UI4:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   DWORD *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (DWORD *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName);
						break;
					}
					case    VT_R4:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   float *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (float *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName);
						break;
					}
					case    VT_R8:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   double *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (double *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					case    VT_BSTR:
					{
						typedef void (AFXAPI *PFV)(CDataExchange *, int,
												   CString *, LPCTSTR );
						(*(PFV)pDDP->m_lpHandler)(pDX, pDDP->m_nCtrlId,
												  (CString *)pDDP->m_lpMember,
												  pDDP->m_lpszOleName );
						break;
					}
					default:
						// Unknown Data Type!
						ASSERT(FALSE);
						break;
				}
			}
			CATCH(COleDispatchException, e)
			{
				// Dleanup before pDX->Fail() throws exception.
				_AfxCleanupDDPs(arrayDDP);

				// Display message box for dispatch exceptions.
				COlePropertyPage* pPropPage = (COlePropertyPage*)pDX->m_pDlgWnd;
				pPropPage->MessageBox((LPCTSTR)e->m_strDescription, NULL,
					MB_ICONEXCLAMATION | MB_OK);
				DELETE_EXCEPTION(e);
				pDX->Fail();
			}
			AND_CATCH_ALL(e)
			{
				// Ignore other exceptions.
				DELETE_EXCEPTION(e);
			}
			END_CATCH_ALL
		}

		_AfxCleanupDDPs(arrayDDP);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COlePropertyPage, CDialog)
