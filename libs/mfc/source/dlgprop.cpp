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

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
// CPropertyPage -- one page of a tabbed dialog

UINT CALLBACK
AfxPropPageCallback(HWND, UINT message, LPPROPSHEETPAGE pPropPage)
{
	switch (message)
	{
	case PSPCB_CREATE:
		{
			ASSERT(AfxIsValidAddress(pPropPage, sizeof(AFX_OLDPROPSHEETPAGE)));
			ASSERT(AfxIsValidAddress(pPropPage, pPropPage->dwSize));
			CPropertyPage* pPage =
				STATIC_DOWNCAST(CPropertyPage, (CObject*)pPropPage->lParam);
			ASSERT_VALID(pPage);
			TRY
			{
				AfxHookWindowCreate(pPage);
			}
			CATCH_ALL(e)
			{
				// Note: DELETE_EXCEPTION(e) not necessary
				return FALSE;
			}
			END_CATCH_ALL
		}
		return TRUE;

	case PSPCB_RELEASE:
		AfxUnhookWindowCreate();
		break;
	}

	return 0;
}

BEGIN_MESSAGE_MAP(CPropertyPage, CDialog)
	//{{AFX_MSG_MAP(CPropertyPage)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
#ifndef _AFX_NO_CTL3D_SUPPORT
	ON_MESSAGE(WM_QUERY3DCONTROLS, OnQuery3dControls)
#endif
END_MESSAGE_MAP()

CPropertyPage::CPropertyPage(UINT nIDTemplate, UINT nIDCaption)
{
	ASSERT(nIDTemplate != 0);
	CommonConstruct(MAKEINTRESOURCE(nIDTemplate), nIDCaption);
}

CPropertyPage::CPropertyPage(LPCTSTR lpszTemplateName, UINT nIDCaption)
{
	ASSERT(AfxIsValidString(lpszTemplateName));
	CommonConstruct(lpszTemplateName, nIDCaption);
}

void CPropertyPage::Construct(UINT nIDTemplate, UINT nIDCaption)
{
	ASSERT(nIDTemplate != 0);
	CommonConstruct(MAKEINTRESOURCE(nIDTemplate), nIDCaption);
}

void CPropertyPage::Construct(LPCTSTR lpszTemplateName, UINT nIDCaption)
{
	ASSERT(HIWORD(lpszTemplateName) == 0 ||
		AfxIsValidString(lpszTemplateName));
	CommonConstruct(lpszTemplateName, nIDCaption);
}

CPropertyPage::CPropertyPage()
{
	CommonConstruct(NULL, 0);
}

void CPropertyPage::CommonConstruct(LPCTSTR lpszTemplateName, UINT nIDCaption)
{
	memset(&m_psp, 0, sizeof(m_psp));
	m_psp.dwSize = sizeof(m_psp);
	m_psp.dwFlags = PSP_USECALLBACK;
	if (lpszTemplateName != NULL)
		m_psp.hInstance = AfxFindResourceHandle(lpszTemplateName, RT_DIALOG);
	m_psp.pszTemplate = lpszTemplateName;
	m_psp.pfnDlgProc = AfxDlgProc;
	m_psp.lParam = (LPARAM)this;
	m_psp.pfnCallback = AfxPropPageCallback;
	if (nIDCaption != 0)
	{
		VERIFY(m_strCaption.LoadString(nIDCaption));
		m_psp.pszTitle = m_strCaption;
		m_psp.dwFlags |= PSP_USETITLE;
	}
	if (AfxHelpEnabled())
		m_psp.dwFlags |= PSP_HASHELP;
	if (HIWORD(lpszTemplateName) == 0)
		m_nIDHelp = LOWORD((DWORD)lpszTemplateName);
	m_lpszTemplateName = m_psp.pszTemplate;
	m_bFirstSetActive = TRUE;
}

CPropertyPage::~CPropertyPage()
{
#ifndef _AFX_NO_OCC_SUPPORT
	Cleanup();
#endif

	if (m_hDialogTemplate != NULL)
		GlobalFree(m_hDialogTemplate);
}

#ifndef _AFX_NO_OCC_SUPPORT

void CPropertyPage::Cleanup()
{
	COccManager* pOccManager = afxOccManager;
	if ((pOccManager != NULL) && (m_pOccDialogInfo != NULL))
	{
		pOccManager->PostCreateDialog(m_pOccDialogInfo);
		free(m_pOccDialogInfo);
		m_pOccDialogInfo = NULL;
	}
}

AFX_STATIC DLGTEMPLATE* AFXAPI
_AfxChangePropPageFont(const DLGTEMPLATE* pTemplate, BOOL bWizard)
{
	CString strFaceDefault;
	WORD wSizeDefault;

	if (!AfxGetPropSheetFont(strFaceDefault, wSizeDefault, bWizard))
		return NULL;

	// set font of property page to same font used by property sheet
	CString strFace;
	WORD wSize;
	if ((!CDialogTemplate::GetFont(pTemplate, strFace, wSize)) ||
		(strFace != strFaceDefault) || (wSize != wSizeDefault))
	{
		CDialogTemplate dlgTemplate(pTemplate);
		dlgTemplate.SetFont(strFaceDefault, wSizeDefault);
		return (DLGTEMPLATE*)dlgTemplate.Detach();
	}

	return NULL;
}

const DLGTEMPLATE* CPropertyPage::InitDialogInfo(const DLGTEMPLATE* pTemplate)
{
	// cleanup from previous run, if any
	Cleanup();

	m_pOccDialogInfo = (_AFX_OCC_DIALOG_INFO*)malloc(
		sizeof(_AFX_OCC_DIALOG_INFO));

	return afxOccManager->PreCreateDialog(m_pOccDialogInfo, pTemplate);
}

#endif

void CPropertyPage::PreProcessPageTemplate(PROPSHEETPAGE& psp, BOOL bWizard)
{
	const DLGTEMPLATE* pTemplate;

	if (psp.dwFlags & PSP_DLGINDIRECT)
	{
		pTemplate = psp.pResource;
	}
	else
	{
		HRSRC hResource = ::FindResource(psp.hInstance,
			psp.pszTemplate, RT_DIALOG);
		HGLOBAL hTemplate = LoadResource(psp.hInstance,
			hResource);
		pTemplate = (LPCDLGTEMPLATE)LockResource(hTemplate);
	}

	ASSERT(pTemplate != NULL);

#ifdef _DEBUG
	// WINBUG: Windows currently does not support DIALOGEX resources!
	// Assert that the template is *not* a DIALOGEX template.
	// DIALOGEX templates are not supported by the PropertySheet API.

	// To change a DIALOGEX template back to a DIALOG template,
	// remove the following:
	//  1. Extended styles on the dialog
	//  2. Help IDs on any control in the dialog
	//  3. Control IDs that are DWORDs
	//  4. Weight, italic, or charset attributes on the dialog's font

	if (((DLGTEMPLATEEX*)pTemplate)->signature == 0xFFFF)
	{
		// it's a DIALOGEX -- we'd better check
		DWORD dwVersion = ::GetVersion();
		if (dwVersion & 0x80000000)
		{
			// it's Win95 -- versions of COMCTL32.DLL that export
			// a function called DllGetVersion are okay

			HINSTANCE hInst = LoadLibrary(_T("COMCTL32.DLL"));
			ASSERT(hInst != NULL);
			if (hInst != NULL)
			{
				FARPROC proc = GetProcAddress(hInst, "DllGetVersion");
				if (proc == NULL)
					ASSERT(FALSE);
				FreeLibrary(hInst);
			}
		}
		else if (LOBYTE(LOWORD(dwVersion)) == 3)
		{
			// it's Windows NT 3.x; we have no hope of this working
			ASSERT(FALSE);
		}
	}
#endif // _DEBUG

#ifndef _AFX_NO_OCC_SUPPORT
	// if the dialog could contain OLE controls, deal with them now
	if (afxOccManager != NULL)
		pTemplate = InitDialogInfo(pTemplate);
#endif

	// set font of property page to same font used by property sheet
	HGLOBAL hTemplate = _AfxChangePropPageFont(pTemplate, bWizard);

	if (m_hDialogTemplate != NULL)
	{
		GlobalFree(m_hDialogTemplate);
		m_hDialogTemplate = NULL;
	}

	if (hTemplate != NULL)
	{
		pTemplate = (LPCDLGTEMPLATE)hTemplate;
		m_hDialogTemplate = hTemplate;
	}
	psp.pResource = pTemplate;
	psp.dwFlags |= PSP_DLGINDIRECT;
}

void CPropertyPage::CancelToClose()
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(GetParent() != NULL);

	GetParent()->SendMessage(PSM_CANCELTOCLOSE);
}

void CPropertyPage::SetModified(BOOL bChanged)
{
	if (m_hWnd == NULL) // allowed for backward compatibility
		return;

	ASSERT(::IsWindow(m_hWnd));
	ASSERT(GetParent() != NULL);

	CWnd* pParentWnd = GetParent();
	if (bChanged)
		pParentWnd->SendMessage(PSM_CHANGED, (WPARAM)m_hWnd);
	else
		pParentWnd->SendMessage(PSM_UNCHANGED, (WPARAM)m_hWnd);
}

LRESULT CPropertyPage::QuerySiblings(WPARAM wParam, LPARAM lParam)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(GetParent() != NULL);

	return GetParent()->SendMessage(PSM_QUERYSIBLINGS, wParam, lParam);
}

BOOL CPropertyPage::OnApply()
{
	ASSERT_VALID(this);

	OnOK();
	return TRUE;
}

void CPropertyPage::OnReset()
{
	ASSERT_VALID(this);

	OnCancel();
}

void CPropertyPage::OnOK()
{
	ASSERT_VALID(this);
}

void CPropertyPage::OnCancel()
{
	ASSERT_VALID(this);
}

BOOL CPropertyPage::OnSetActive()
{
	ASSERT_VALID(this);

	if (m_bFirstSetActive)
		m_bFirstSetActive = FALSE;
	else
		UpdateData(FALSE);
	return TRUE;
}

BOOL CPropertyPage::OnKillActive()
{
	ASSERT_VALID(this);

	if (!UpdateData())
	{
		TRACE0("UpdateData failed during page deactivation\n");
		return FALSE;
	}
	return TRUE;
}

BOOL CPropertyPage::OnQueryCancel()
{
	return TRUE;    // ok to cancel
}

LRESULT CPropertyPage::OnWizardBack()
{
	return 0;
}

LRESULT CPropertyPage::OnWizardNext()
{
	return 0;
}

BOOL CPropertyPage::OnWizardFinish()
{
	return TRUE;
}

LRESULT CPropertyPage::MapWizardResult(LRESULT lToMap)
{
	// -1 and 0 are special
	if (lToMap == -1 || lToMap == 0)
		return lToMap;

	// only do special stuff if MFC owns the property sheet
	CWnd* pParent = GetParent();
	CPropertySheet* pSheet = DYNAMIC_DOWNCAST(CPropertySheet, pParent);
	if (pSheet != NULL)
	{
		// the structures are laid out different for CPropertySheeEx
		CPropertySheetEx* pSheetEx = DYNAMIC_DOWNCAST(CPropertySheetEx, pParent);
		const PROPSHEETPAGE* ppsp;
		if (pSheetEx != NULL)
			ppsp = pSheetEx->m_psh.ppsp;
		else
			ppsp = pSheet->m_psh.ppsp;

		// search the pages for a matching ID
		for (int i = 0; i < pSheet->m_pages.GetSize(); i++)
		{
			// check page[i] for a match
			CPropertyPage* pPage = pSheet->GetPage(i);
			if ((LRESULT)pPage->m_psp.pszTemplate == lToMap)
				return (LRESULT)ppsp->pResource;

			// jump to next page
			(BYTE*&)ppsp += ppsp->dwSize;
		}
	}
	// otherwise, just use the original value
	return lToMap;
}

BOOL CPropertyPage::IsButtonEnabled(int iButton)
{
	HWND hWnd = ::GetDlgItem(::GetParent(m_hWnd), iButton);
	if (hWnd == NULL)
		return FALSE;
	return ::IsWindowEnabled(hWnd);
}

BOOL CPropertyPage::OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult)
{
	ASSERT(pResult != NULL);
	NMHDR* pNMHDR = (NMHDR*)lParam;

	// allow message map to override
	if (CDialog::OnNotify(wParam, lParam, pResult))
		return TRUE;

	// don't handle messages not from the page/sheet itself
	if (pNMHDR->hwndFrom != m_hWnd && pNMHDR->hwndFrom != ::GetParent(m_hWnd))
		return FALSE;

	// handle default
	switch (pNMHDR->code)
	{
	case PSN_SETACTIVE:
		{
			CPropertySheet* pSheet = DYNAMIC_DOWNCAST(CPropertySheet, GetParent());
			if (pSheet != NULL && !(pSheet->m_nFlags & WF_CONTINUEMODAL) && !(pSheet->m_bModeless))
				*pResult = -1;
			else
				*pResult = OnSetActive() ? 0 : -1;
		}
		break;
	case PSN_KILLACTIVE:
		*pResult = !OnKillActive();
		break;
	case PSN_APPLY:
		*pResult = OnApply() ? PSNRET_NOERROR : PSNRET_INVALID_NOCHANGEPAGE;
		break;
	case PSN_RESET:
		OnReset();
		break;
	case PSN_QUERYCANCEL:
		*pResult = !OnQueryCancel();
		break;
	case PSN_WIZNEXT:
		//WINBUG: Win32 will send a PSN_WIZBACK even if the button is disabled.
		if (IsButtonEnabled(ID_WIZNEXT))
			*pResult = MapWizardResult(OnWizardNext());
		break;
	case PSN_WIZBACK:
		//WINBUG: Win32 will send a PSN_WIZBACK even if the button is disabled.
		if (IsButtonEnabled(ID_WIZBACK))
			*pResult = MapWizardResult(OnWizardBack());
		break;
	case PSN_WIZFINISH:
		*pResult = !OnWizardFinish();
		break;
	case PSN_HELP:
		SendMessage(WM_COMMAND, ID_HELP);
		break;

	default:
		return FALSE;   // not handled
	}

	return TRUE;    // handled
}

/////////////////////////////////////////////////////////////////////////////
// CPropertyPage message Handlers

BOOL CPropertyPage::PreTranslateMessage(MSG* pMsg)
{
	VERIFY(!CWnd::PreTranslateMessage(pMsg));

	return FALSE;   // handled by CPropertySheet::PreTranslateMessage
}

HBRUSH CPropertyPage::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	LRESULT lResult;
	if (pWnd->SendChildNotifyLastMsg(&lResult))
		return (HBRUSH)lResult;

	if (afxData.bWin4)
		return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);

	if (!GrayCtlColor(pDC->m_hDC, pWnd->GetSafeHwnd(), nCtlColor,
	  afxData.hbrBtnFace, afxData.clrBtnText))
		return (HBRUSH)Default();
	return afxData.hbrBtnFace;
}

/////////////////////////////////////////////////////////////////////////////
// CPropertyPage Diagnostics

#ifdef _DEBUG
void CPropertyPage::AssertValid() const
{
	CDialog::AssertValid();
	ASSERT(m_psp.dwSize == sizeof(m_psp));
	ASSERT(m_psp.dwFlags & PSP_USECALLBACK);
	ASSERT(m_psp.pfnDlgProc == AfxDlgProc);
	ASSERT(m_psp.lParam == (LPARAM)this);
}

void CPropertyPage::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	dc << "m_strCaption = " << m_strCaption << "\n";
	dc << "m_psp.dwFlags = " << m_psp.dwFlags << "\n";
}
#endif //_DEBUG

void CPropertyPage::EndDialog(int nID)
{
	// Normally you shouldn't call EndDialog from a page. But in case it does
	// happen during error situations, call CPropertySheet::EndDialog instead.

	CPropertySheet* pParent = DYNAMIC_DOWNCAST(CPropertySheet, GetParent());
	if (pParent != NULL)
		pParent->EndDialog(nID);
}

/////////////////////////////////////////////////////////////////////////////
// CPropertySheet -- a tabbed "dialog" (really a popup-window)

BEGIN_MESSAGE_MAP(CPropertySheet, CWnd)
	//{{AFX_MSG_MAP(CPropertySheet)
	ON_WM_CTLCOLOR()
	ON_WM_NCCREATE()
	ON_MESSAGE(WM_INITDIALOG, HandleInitDialog)
	ON_MESSAGE(WM_COMMANDHELP, OnCommandHelp)
	ON_WM_CLOSE()
	ON_WM_SYSCOMMAND()
	ON_MESSAGE(DM_SETDEFID, OnSetDefID)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

AFX_STATIC_DATA const int _afxPropSheetIDs[4] = { ID_WIZNEXT, ID_WIZFINISH, ID_WIZBACK, IDCANCEL };

AFX_OLDPROPSHEETHEADER* CPropertySheet::GetPropSheetHeader()
{
	CPropertySheetEx* pSheetEx = DYNAMIC_DOWNCAST(CPropertySheetEx, this);
	if (pSheetEx != NULL)
		return (AFX_OLDPROPSHEETHEADER*)&pSheetEx->m_psh;
	else
		return &m_psh;
}

LRESULT CPropertySheet::OnSetDefID(WPARAM wParam, LPARAM lParam)
{
	// WINBUG -- A wrong or invalid ID may be passed in here.  If this is the
	// case, then look for a valid one.
	HWND hWnd;
	if (IsWizard() &&
		(
			((hWnd = ::GetDlgItem(m_hWnd, wParam)) == NULL) ||
			!(::GetWindowLong(hWnd, GWL_STYLE) & WS_VISIBLE) ||
			!::IsWindowEnabled(hWnd)
		))
	{

		for (int i = 0; i < 4; i++)
		{
			// find first button that is visible and  enabled
			HWND hWnd = ::GetDlgItem(m_hWnd, _afxPropSheetIDs[i]);
			if ((GetWindowLong(hWnd, GWL_STYLE) & WS_VISIBLE) &&
				::IsWindowEnabled(hWnd))
			{
				//WINBUG -- focus could be incorrect as well in this case
				// so ... let's set it to the default button
				HWND hWndFocus = ::GetFocus();
				if (!::IsWindowEnabled(hWndFocus))
					::SetFocus(hWnd);
				return DefWindowProc(DM_SETDEFID, _afxPropSheetIDs[i], lParam);
			}
		}
	}
	return Default();
}

CPropertySheet::CPropertySheet()
{
	CommonConstruct(NULL, 0);
}

CPropertySheet::CPropertySheet(UINT nIDCaption, CWnd* pParentWnd,
	UINT iSelectPage)
{
	ASSERT(nIDCaption != 0);

	VERIFY(m_strCaption.LoadString(nIDCaption) != 0);
	CommonConstruct(pParentWnd, iSelectPage);
}

CPropertySheet::CPropertySheet(LPCTSTR pszCaption, CWnd* pParentWnd,
	UINT iSelectPage)
{
	ASSERT(pszCaption != NULL);

	m_strCaption = pszCaption;
	CommonConstruct(pParentWnd, iSelectPage);
}

void CPropertySheet::Construct(UINT nIDCaption, CWnd* pParentWnd,
	UINT iSelectPage)
{
	ASSERT(nIDCaption != 0);

	VERIFY(m_strCaption.LoadString(nIDCaption) != 0);
	CommonConstruct(pParentWnd, iSelectPage);
}

void CPropertySheet::Construct(LPCTSTR pszCaption, CWnd* pParentWnd,
	UINT iSelectPage)
{
	ASSERT(pszCaption != NULL);

	m_strCaption = pszCaption;
	CommonConstruct(pParentWnd, iSelectPage);
}

void CPropertySheet::CommonConstruct(CWnd* pParentWnd, UINT iSelectPage)
{
	memset(&m_psh, 0, sizeof(m_psh));
	m_psh.dwSize = sizeof(m_psh);
	m_psh.dwFlags = PSH_PROPSHEETPAGE;
	m_psh.pszCaption = m_strCaption;
	m_psh.nStartPage = iSelectPage;
	m_bStacked = TRUE;
	m_bModeless = FALSE;

	if (AfxHelpEnabled())
		m_psh.dwFlags |= PSH_HASHELP;

	m_pParentWnd = pParentWnd;  // m_psh.hwndParent set in DoModal/create
}

void CPropertySheet::EnableStackedTabs(BOOL bStacked)
{
	m_bStacked = bStacked;
}

void CPropertySheet::SetTitle(LPCTSTR lpszText, UINT nStyle)
{
	ASSERT((nStyle & ~PSH_PROPTITLE) == 0); // only PSH_PROPTITLE is valid
	ASSERT(lpszText == NULL || AfxIsValidString(lpszText));

	if (m_hWnd == NULL)
	{
		AFX_OLDPROPSHEETHEADER* psh = GetPropSheetHeader();
		// set internal state
		m_strCaption = lpszText;
		psh->pszCaption = m_strCaption;
		psh->dwFlags &= ~PSH_PROPTITLE;
		psh->dwFlags |= nStyle;
	}
	else
	{
		// set external state
		SendMessage(PSM_SETTITLE, nStyle, (LPARAM)lpszText);
	}
}

CPropertySheet::~CPropertySheet()
{
	delete[] (PROPSHEETPAGE*)m_psh.ppsp;
}

BOOL CPropertySheet::PreTranslateMessage(MSG* pMsg)
{
	ASSERT_VALID(this);

	// allow tooltip messages to be filtered
	if (CWnd::PreTranslateMessage(pMsg))
		return TRUE;

	// allow sheet to translate Ctrl+Tab, Shift+Ctrl+Tab,
	//  Ctrl+PageUp, and Ctrl+PageDown
	if (pMsg->message == WM_KEYDOWN && GetAsyncKeyState(VK_CONTROL) < 0 &&
		(pMsg->wParam == VK_TAB || pMsg->wParam == VK_PRIOR || pMsg->wParam == VK_NEXT))
	{
		if (SendMessage(PSM_ISDIALOGMESSAGE, 0, (LPARAM)pMsg))
			return TRUE;
	}

	// handle rest with IsDialogMessage
	return PreTranslateInput(pMsg);
}

BOOL CPropertySheet::OnCmdMsg(UINT nID, int nCode, void* pExtra,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	if (CWnd::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
		return TRUE;

	if ((nCode != CN_COMMAND && nCode != CN_UPDATE_COMMAND_UI) ||
			!IS_COMMAND_ID(nID) || nID >= 0xf000)
	{
		// control notification or non-command button or system command
		return FALSE;       // not routed any further
	}

	// if we have an owner window, give it second crack
	CWnd* pOwner = GetParent();
	if (pOwner != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceCmdRouting)
			TRACE1("Routing command id 0x%04X to owner window.\n", nID);
#endif
		ASSERT(pOwner != this);
		if (pOwner->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
			return TRUE;
	}

	// last crack goes to the current CWinThread object
	CWinThread* pThread = AfxGetThread();
	if (pThread != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceCmdRouting)
			TRACE1("Routing command id 0x%04X to app.\n", nID);
#endif
		if (pThread->OnCmdMsg(nID, nCode, pExtra, pHandlerInfo))
			return TRUE;
	}

#ifdef _DEBUG
	if (afxTraceFlags & traceCmdRouting)
	{
		TRACE2("IGNORING command id 0x%04X sent to %hs dialog.\n", nID,
				GetRuntimeClass()->m_lpszClassName);
	}
#endif
	return FALSE;
}

CPropertyPage* CPropertySheet::GetActivePage() const
{
	ASSERT_VALID(this);

	CPropertyPage* pPage;
	if (m_hWnd != NULL)
		pPage = STATIC_DOWNCAST(CPropertyPage,
			CWnd::FromHandle((HWND)::SendMessage(m_hWnd, PSM_GETCURRENTPAGEHWND, 0, 0)));
	else
		pPage = GetPage(GetActiveIndex());
	return pPage;
}

BOOL CPropertySheet::ContinueModal()
{
	// allow CWnd::EndModalLoop to be used
	if (!CWnd::ContinueModal())
		return FALSE;

	// when active page is NULL, the modal loop should end
	ASSERT(::IsWindow(m_hWnd));
	BOOL bResult = SendMessage(PSM_GETCURRENTPAGEHWND);
	return bResult;
}

int CPropertySheet::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd == NULL);

	// register common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	// finish building PROPSHEETHEADER structure
	BuildPropPageArray();

	// allow OLE servers to disable themselves
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		pApp->EnableModeless(FALSE);

	// find parent HWND
	HWND hWndTop;
	HWND hWndParent = CWnd::GetSafeOwner_(m_pParentWnd->GetSafeHwnd(), &hWndTop);
	AFX_OLDPROPSHEETHEADER* psh = GetPropSheetHeader();
	psh->hwndParent = hWndParent;
	BOOL bEnableParent = FALSE;
	if (hWndParent != NULL && ::IsWindowEnabled(hWndParent))
	{
		::EnableWindow(hWndParent, FALSE);
		bEnableParent = TRUE;
	}
	HWND hWndCapture = ::GetCapture();
	if (hWndCapture != NULL)
		::SendMessage(hWndCapture, WM_CANCELMODE, 0, 0);

	// setup for modal loop and creation
	m_nModalResult = 0;
	m_nFlags |= WF_CONTINUEMODAL;

	// hook for creation of window
	AfxHookWindowCreate(this);
	psh->dwFlags |= PSH_MODELESS;
	m_nFlags |= WF_CONTINUEMODAL;
	HWND hWnd = (HWND)::PropertySheet((PROPSHEETHEADER*)psh);
#ifdef _DEBUG
	DWORD dwError = ::GetLastError();
#endif
	psh->dwFlags &= ~PSH_MODELESS;
	AfxUnhookWindowCreate();

	// handle error
	if (hWnd == NULL || hWnd == (HWND)-1)
	{
		TRACE1("PropertySheet() failed: GetLastError returned %d\n", dwError);
		m_nFlags &= ~WF_CONTINUEMODAL;
	}

	int nResult = m_nModalResult;
	if (ContinueModal())
	{
		// enter modal loop
		DWORD dwFlags = MLF_SHOWONIDLE;
		if (GetStyle() & DS_NOIDLEMSG)
			dwFlags |= MLF_NOIDLEMSG;
		nResult = RunModalLoop(dwFlags);
	}

	// hide the window before enabling parent window, etc.
	if (m_hWnd != NULL)
	{
		SetWindowPos(NULL, 0, 0, 0, 0, SWP_HIDEWINDOW|
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOACTIVATE|SWP_NOZORDER);
	}
	if (bEnableParent)
		::EnableWindow(hWndParent, TRUE);
	if (hWndParent != NULL && ::GetActiveWindow() == m_hWnd)
		::SetActiveWindow(hWndParent);

	// cleanup
	DestroyWindow();

	// allow OLE servers to enable themselves
	if (pApp != NULL)
		pApp->EnableModeless(TRUE);
	if (hWndTop != NULL)
		::EnableWindow(hWndTop, TRUE);

	return nResult;
}

int CALLBACK
AfxPropSheetCallback(HWND, UINT message, LPARAM lParam)
{
	switch (message)
	{
	case PSCB_PRECREATE:
		{
			_AFX_THREAD_STATE* pState = AfxGetThreadState();
			LPDLGTEMPLATE lpTemplate = (LPDLGTEMPLATE)lParam;
			if (lpTemplate->style != pState->m_dwPropStyle ||
				lpTemplate->dwExtendedStyle != pState->m_dwPropExStyle)
			{
				// Mark the dialog template as read-write.
				DWORD dwOldProtect;
				VirtualProtect(lpTemplate, sizeof(DLGTEMPLATE), PAGE_READWRITE, &dwOldProtect);

				// Ensure DS_SETFONT is set correctly.
				lpTemplate->style = lpTemplate->style & DS_SETFONT ?
									pState->m_dwPropStyle | DS_SETFONT :
									pState->m_dwPropStyle & ~DS_SETFONT;

				lpTemplate->dwExtendedStyle = pState->m_dwPropExStyle;
				return TRUE;
			}
			return FALSE;
		}
	}

	return 0;
}

BOOL CPropertySheet::Create(CWnd* pParentWnd, DWORD dwStyle, DWORD dwExStyle)
{
	_AFX_THREAD_STATE* pState = AfxGetThreadState();

	// Calculate the default window style.
	if (dwStyle == (DWORD)-1)
	{
		pState->m_dwPropStyle = DS_MODALFRAME | DS_3DLOOK | DS_CONTEXTHELP |
								DS_SETFONT | WS_POPUP | WS_VISIBLE | WS_CAPTION;

		// Wizards don't have WS_SYSMENU.
		if (!IsWizard())
			pState->m_dwPropStyle |= WS_SYSMENU;
	}
	else
	{
		pState->m_dwPropStyle = dwStyle;
	}
	pState->m_dwPropExStyle = dwExStyle;

	ASSERT_VALID(this);
	ASSERT(m_hWnd == NULL);

	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTLS_REG));
	AfxDeferRegisterClass(AFX_WNDCOMMCTLSNEW_REG);

	// finish building PROPSHEETHEADER structure
	AFX_OLDPROPSHEETHEADER* psh = GetPropSheetHeader();

	BuildPropPageArray();
	m_bModeless = TRUE;
	psh->dwFlags |= (PSH_MODELESS|PSH_USECALLBACK);
	psh->pfnCallback = AfxPropSheetCallback;
	psh->hwndParent = pParentWnd->GetSafeHwnd();

	// hook the window creation process
	AfxHookWindowCreate(this);
	HWND hWnd = (HWND)PropertySheet((PROPSHEETHEADER*)psh);
#ifdef _DEBUG
	DWORD dwError = ::GetLastError();
#endif

	// cleanup on failure, otherwise return TRUE
	if (!AfxUnhookWindowCreate())
		PostNcDestroy();    // cleanup if Create fails

	if (hWnd == NULL || hWnd == (HWND)-1)
	{
		TRACE1("PropertySheet() failed: GetLastError returned %d\n", dwError);
		return FALSE;
	}

	ASSERT(hWnd == m_hWnd);
	return TRUE;
}

void CPropertySheet::BuildPropPageArray()
{
	// delete existing prop page array
	delete[] (PROPSHEETPAGE*)m_psh.ppsp;
	m_psh.ppsp = NULL;

	// build new prop page array
	AFX_OLDPROPSHEETPAGE* ppsp = new AFX_OLDPROPSHEETPAGE[m_pages.GetSize()];
	m_psh.ppsp = (LPPROPSHEETPAGE)ppsp;
	BOOL bWizard = (m_psh.dwFlags & (PSH_WIZARD | PSH_WIZARD97));
	for (int i = 0; i < m_pages.GetSize(); i++)
	{
		CPropertyPage* pPage = GetPage(i);
		memcpy(&ppsp[i], &pPage->m_psp, sizeof(pPage->m_psp));
		pPage->PreProcessPageTemplate((PROPSHEETPAGE&)ppsp[i], bWizard);
	}
	m_psh.nPages = m_pages.GetSize();
}

////////////////////////////////////////////////////////////////////////////

int CPropertySheet::GetPageCount() const
{
	ASSERT_VALID(this);

	if (m_hWnd == NULL)
		return m_pages.GetSize();

	CTabCtrl* pTab = GetTabControl();
	ASSERT_VALID(pTab);
	return pTab->GetItemCount();
}

int CPropertySheet::GetActiveIndex() const
{
	if (m_hWnd == NULL)
		return ((CPropertySheet*)this)->GetPropSheetHeader()->nStartPage;

	CTabCtrl* pTab = GetTabControl();
	ASSERT_VALID(pTab);
	return pTab->GetCurSel();
}

BOOL CPropertySheet::SetActivePage(int nPage)
{
	if (m_hWnd == NULL)
	{
		GetPropSheetHeader()->nStartPage = nPage;
		return TRUE;
	}
	return (BOOL)SendMessage(PSM_SETCURSEL, nPage);
}

int CPropertySheet::GetPageIndex(CPropertyPage* pPage)
{
	for (int i = 0; i < GetPageCount(); i++)
	{
		if (GetPage(i) == pPage)
			return i;
	}
	return -1;  // pPage not found
}

BOOL CPropertySheet::SetActivePage(CPropertyPage* pPage)
{
	ASSERT_VALID(this);
	ASSERT(pPage != NULL);
	ASSERT_KINDOF(CPropertyPage, pPage);

	int nPage = GetPageIndex(pPage);
	ASSERT(pPage >= 0);

	return SetActivePage(nPage);
}

void CPropertySheet::AddPage(CPropertyPage* pPage)
{
	ASSERT_VALID(this);
	ASSERT(pPage != NULL);
	ASSERT_KINDOF(CPropertyPage, pPage);
	ASSERT_VALID(pPage);

	// add page to internal list
	m_pages.Add(pPage);

	// add page externally
	if (m_hWnd != NULL)
	{
		// build new prop page array
		AFX_OLDPROPSHEETPAGE *ppsp = new AFX_OLDPROPSHEETPAGE[m_pages.GetSize()];
		memcpy(ppsp, m_psh.ppsp, sizeof(AFX_OLDPROPSHEETPAGE) * (m_pages.GetSize()-1));
		delete[] (PROPSHEETPAGE*)m_psh.ppsp;
		m_psh.ppsp = (PROPSHEETPAGE*)ppsp;
		ppsp += m_pages.GetSize()-1;

		// copy processed PROPSHEETPAGE struct to end
		memcpy(ppsp, &pPage->m_psp, sizeof(pPage->m_psp));
		pPage->PreProcessPageTemplate((PROPSHEETPAGE&)*ppsp, IsWizard());
		HPROPSHEETPAGE hPSP = CreatePropertySheetPage((PROPSHEETPAGE*)ppsp);
		if (hPSP == NULL)
			AfxThrowMemoryException();

		if (!SendMessage(PSM_ADDPAGE, 0, (LPARAM)hPSP))
		{
			DestroyPropertySheetPage(hPSP);
			AfxThrowMemoryException();
		}
	}
}

void CPropertySheet::RemovePage(CPropertyPage* pPage)
{
	ASSERT_VALID(this);
	ASSERT(pPage != NULL);
	ASSERT_KINDOF(CPropertyPage, pPage);

	int nPage = GetPageIndex(pPage);
	ASSERT(nPage >= 0);

	RemovePage(nPage);
}

void CPropertySheet::RemovePage(int nPage)
{
	ASSERT_VALID(this);

	// remove the page externally
	if (m_hWnd != NULL)
		SendMessage(PSM_REMOVEPAGE, nPage);

	// remove the page from internal list
	m_pages.RemoveAt(nPage);
}

void CPropertySheet::EndDialog(int nEndID)
{
	ASSERT_VALID(this);

	CWnd::EndModalLoop(nEndID);
	if (m_bModeless)
		DestroyWindow();
	else
		PostMessage(PSM_PRESSBUTTON, PSBTN_CANCEL);
}

void CPropertySheet::OnClose()
{
	m_nModalResult = IDCANCEL;
	if (m_bModeless)
		DestroyWindow();
	else
		Default();
}

void CPropertySheet::OnSysCommand(UINT nID, LPARAM)
{
	m_nModalResult = IDCANCEL;
	switch (nID & 0xFFF0)
	{
	case SC_CLOSE:
		if (m_bModeless)
		{
			SendMessage(WM_CLOSE);
			return;
		}
	}
	Default();
}

/////////////////////////////////////////////////////////////////////////////
// CPropertySheet message handlers

AFX_STATIC_DATA int _afxPropSheetButtons[] = { IDOK, IDCANCEL, ID_APPLY_NOW, IDHELP };

BOOL CPropertySheet::OnInitDialog()
{
	// change tab style if scrolling tabs desired (stacked tabs are default)
	if (!m_bStacked)
	{
		HWND hWndTab = (HWND)::GetDlgItem(m_hWnd, AFX_IDC_TAB_CONTROL);
		if (hWndTab != NULL)
			CWnd::ModifyStyle(hWndTab, TCS_MULTILINE, TCS_SINGLELINE, 0);
	}

	if (!IsWizard())
	{
		// resize the tab control so the layout is less restrictive
		HWND hWnd = (HWND)::GetDlgItem(m_hWnd, AFX_IDC_TAB_CONTROL);
		ASSERT(hWnd != NULL);
		CRect rectOld;
		::GetWindowRect(hWnd, &rectOld);
		ScreenToClient(rectOld);
		CRect rectNew(0, 0, 0, 32);
		::MapDialogRect(m_hWnd, &rectNew);
		if (rectNew.bottom < rectOld.bottom)
		{
			// move tab control
			int cyDiff = rectOld.Height() - rectNew.bottom;
			::SetWindowPos(hWnd, NULL, 0, 0, rectOld.Width(), rectNew.bottom,
				SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);

			// move buttons by similar amount
			for (int i = 0; i < _countof(_afxPropSheetButtons); i++)
			{
				hWnd = ::GetDlgItem(m_hWnd, _afxPropSheetButtons[i]);
				if (hWnd != NULL)
				{
					::GetWindowRect(hWnd, &rectOld);
					ScreenToClient(&rectOld);
					::SetWindowPos(hWnd, NULL,
						rectOld.left, rectOld.top - cyDiff,
						0, 0, SWP_NOSIZE | SWP_NOZORDER | SWP_NOACTIVATE);
				}
			}

			// resize property sheet itself similarly
			GetWindowRect(&rectOld);
			SetWindowPos(NULL, 0, 0, rectOld.Width(), rectOld.Height() - cyDiff,
				SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);
		}
	}

	BOOL bResult = (BOOL)Default();

	if (m_bModeless && !IsWizard())
	{
		// layout property sheet so button area is not accounted for
		CRect rectWnd;
		GetWindowRect(rectWnd);
		CRect rectButton;
		HWND hWnd = ::GetDlgItem(m_hWnd, IDOK);
		ASSERT(hWnd != NULL);
		::GetWindowRect(hWnd, rectButton);
		SetWindowPos(NULL, 0, 0,
			rectWnd.Width(), rectButton.top - rectWnd.top,
			SWP_NOMOVE | SWP_NOZORDER | SWP_NOACTIVATE);

		// remove standard buttons for modeless dialogs
		for (int i = 0; i < _countof(_afxPropSheetButtons); i++)
		{
			HWND hWnd = ::GetDlgItem(m_hWnd, _afxPropSheetButtons[i]);
			if (hWnd != NULL)
			{
				::ShowWindow(hWnd, SW_HIDE);
				::EnableWindow(hWnd, FALSE);
			}
		}
	}

	// center the property sheet relative to the parent window
	if (!(GetStyle() & WS_CHILD))
		CenterWindow();

	return bResult;
}

BOOL CPropertySheet::OnNcCreate(LPCREATESTRUCT)
{
	// By default, MFC does not directly support the new style
	// help button in the caption, so it is turned off here.
	// It can be added back in and implemented by derived classes
	// from CPropertySheet.
	ModifyStyleEx(WS_EX_CONTEXTHELP, 0);

	return (BOOL)Default();
}

LRESULT CPropertySheet::HandleInitDialog(WPARAM, LPARAM)
{
	LRESULT lResult = OnInitDialog();
	return lResult;
}

BOOL CPropertySheet::OnCommand(WPARAM wParam, LPARAM lParam)
{
	// allow message map override
	if (CWnd::OnCommand(wParam, lParam))
		return TRUE;

	// crack message parameters
	UINT nID = LOWORD(wParam);
	HWND hWndCtrl = (HWND)lParam;
	int nCode = HIWORD(wParam);

	// set m_nModalResult to ID of button, whenever button is clicked
	if (hWndCtrl != NULL && nCode == BN_CLICKED)
	{
		if (::SendMessage(hWndCtrl, WM_GETDLGCODE, 0, 0) &
			(DLGC_BUTTON|DLGC_DEFPUSHBUTTON))
		{
			LONG lStyle = ::GetWindowLong(hWndCtrl, GWL_STYLE) & 0x0F;
			if (lStyle == BS_PUSHBUTTON || lStyle == BS_DEFPUSHBUTTON ||
				lStyle == BS_USERBUTTON || lStyle == BS_OWNERDRAW)
			{
				m_nModalResult = nID;
			}
		}
	}
	return FALSE;
}

LRESULT CPropertySheet::OnCommandHelp(WPARAM wParam, LPARAM lParam)
{
	ASSERT_VALID(this);

	CPropertyPage* pPage = GetActivePage();
	ASSERT_VALID(pPage);
	return AfxCallWndProc(pPage, pPage->m_hWnd, WM_COMMANDHELP, wParam, lParam);
}

HBRUSH CPropertySheet::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	LRESULT lResult;
	if (pWnd->SendChildNotifyLastMsg(&lResult))
		return (HBRUSH)lResult;

	if (afxData.bWin4)
		return CWnd::OnCtlColor(pDC, pWnd, nCtlColor);

	if (!GrayCtlColor(pDC->m_hDC, pWnd->GetSafeHwnd(), nCtlColor,
	  afxData.hbrBtnFace, afxData.clrBtnText))
		return (HBRUSH)Default();
	return afxData.hbrBtnFace;
}

/////////////////////////////////////////////////////////////////////////////
// CPropertySheet Diagnostics

#ifdef _DEBUG
void CPropertySheet::AssertValid() const
{
	CWnd::AssertValid();
	m_pages.AssertValid();
	ASSERT(m_psh.dwSize == sizeof(m_psh));
	ASSERT((m_psh.dwFlags & PSH_PROPSHEETPAGE) == PSH_PROPSHEETPAGE);
}

void CPropertySheet::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);

	dc << "m_strCaption = " << m_strCaption << "\n";
	dc << "Number of Pages = " << m_pages.GetSize() << "\n";
	dc << "Stacked = " << m_bStacked << "\n";
	dc << "Modeless = " << m_bModeless << "\n";
}
#endif //_DEBUG

////////////////////////////////////////////////////////////////////////////
// CPropertyPageEx -- one page of a tabbed dialog, extended for IE4 features

CPropertyPageEx::CPropertyPageEx(UINT nIDTemplate, UINT nIDCaption, UINT nIDHeaderTitle, UINT nIDHeaderSubTitle)
{
	ASSERT(nIDTemplate != 0);
	CommonConstruct(MAKEINTRESOURCE(nIDTemplate), nIDCaption, nIDHeaderTitle, nIDHeaderSubTitle);
}

CPropertyPageEx::CPropertyPageEx(LPCTSTR lpszTemplateName, UINT nIDCaption, UINT nIDHeaderTitle, UINT nIDHeaderSubTitle)
{
	ASSERT(AfxIsValidString(lpszTemplateName));
	CommonConstruct(lpszTemplateName, nIDCaption, nIDHeaderTitle, nIDHeaderSubTitle);
}

void CPropertyPageEx::Construct(UINT nIDTemplate, UINT nIDCaption, UINT nIDHeaderTitle, UINT nIDHeaderSubTitle)
{
	ASSERT(nIDTemplate != 0);
	CommonConstruct(MAKEINTRESOURCE(nIDTemplate), nIDCaption, nIDHeaderTitle, nIDHeaderSubTitle);
}

void CPropertyPageEx::Construct(LPCTSTR lpszTemplateName, UINT nIDCaption, UINT nIDHeaderTitle, UINT nIDHeaderSubTitle)
{
	ASSERT(HIWORD(lpszTemplateName) == 0 ||
		AfxIsValidString(lpszTemplateName));
	CommonConstruct(lpszTemplateName, nIDCaption, nIDHeaderTitle, nIDHeaderSubTitle);
}

CPropertyPageEx::CPropertyPageEx()
{
	CommonConstruct(NULL, 0, 0, 0);
}

void CPropertyPageEx::CommonConstruct(LPCTSTR lpszTemplateName, UINT nIDCaption, UINT nIDHeaderTitle, UINT nIDHeaderSubTitle)
{
	memset(&m_psp, 0, sizeof(m_psp));
	m_psp.dwSize = sizeof(m_psp);
	m_psp.dwFlags = PSP_USECALLBACK;
	if (lpszTemplateName != NULL)
		m_psp.hInstance = AfxFindResourceHandle(lpszTemplateName, RT_DIALOG);
	m_psp.pszTemplate = lpszTemplateName;
	m_psp.pfnDlgProc = AfxDlgProc;
	m_psp.lParam = (LPARAM)this;
	m_psp.pfnCallback = AfxPropPageCallback;
	if (nIDCaption != 0)
	{
		VERIFY(m_strCaption.LoadString(nIDCaption));
		m_psp.pszTitle = m_strCaption;
		m_psp.dwFlags |= PSP_USETITLE;
	}
	if (nIDHeaderTitle != 0)
		VERIFY(m_strHeaderTitle.LoadString(nIDHeaderTitle));
	if (nIDHeaderSubTitle != 0)
		VERIFY(m_strHeaderSubTitle.LoadString(nIDHeaderSubTitle));
	if (AfxHelpEnabled())
		m_psp.dwFlags |= PSP_HASHELP;
	if (HIWORD(lpszTemplateName) == 0)
		m_nIDHelp = LOWORD((DWORD)lpszTemplateName);
	m_lpszTemplateName = m_psp.pszTemplate;
	m_bFirstSetActive = TRUE;
}

#ifdef _DEBUG
void CPropertyPageEx::AssertValid() const
{
	CDialog::AssertValid();
	ASSERT(m_psp.dwSize == sizeof(m_psp));
	ASSERT(m_psp.dwFlags & PSP_USECALLBACK);
	ASSERT(m_psp.pfnDlgProc == AfxDlgProc);
	ASSERT(m_psp.lParam == (LPARAM)this);
}

void CPropertyPageEx::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	dc << "m_strCaption = " << m_strCaption << "\n";
	dc << "m_psp.dwFlags = " << m_psp.dwFlags << "\n";
	dc << "m_strHeaderTitle = " << m_strHeaderTitle << "\n";
	dc << "m_strHeaderSubTitle = " << m_strHeaderSubTitle << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPropertySheetEx -- a tabbed "dialog" (really a popup-window), extended
//                     for IE4

CPropertySheetEx::CPropertySheetEx()
{
	CommonConstruct(NULL, 0, NULL, NULL, NULL);
}

CPropertySheetEx::CPropertySheetEx(UINT nIDCaption, CWnd* pParentWnd,
	UINT iSelectPage, HBITMAP hbmWatermark, HPALETTE hpalWatermark,
	HBITMAP hbmHeader)
{
	ASSERT(nIDCaption != 0);

	VERIFY(m_strCaption.LoadString(nIDCaption) != 0);
	CommonConstruct(pParentWnd, iSelectPage, hbmWatermark, hpalWatermark, hbmHeader);
}

CPropertySheetEx::CPropertySheetEx(LPCTSTR pszCaption, CWnd* pParentWnd,
	UINT iSelectPage, HBITMAP hbmWatermark, HPALETTE hpalWatermark,
	HBITMAP hbmHeader)
{
	ASSERT(pszCaption != NULL);

	m_strCaption = pszCaption;
	CommonConstruct(pParentWnd, iSelectPage, hbmWatermark, hpalWatermark, hbmHeader);
}

void CPropertySheetEx::Construct(UINT nIDCaption, CWnd* pParentWnd,
	UINT iSelectPage, HBITMAP hbmWatermark, HPALETTE hpalWatermark,
	HBITMAP hbmHeader)
{
	ASSERT(nIDCaption != 0);

	VERIFY(m_strCaption.LoadString(nIDCaption) != 0);
	CommonConstruct(pParentWnd, iSelectPage, hbmWatermark, hpalWatermark, hbmHeader);
}

void CPropertySheetEx::Construct(LPCTSTR pszCaption, CWnd* pParentWnd,
	UINT iSelectPage, HBITMAP hbmWatermark, HPALETTE hpalWatermark,
	HBITMAP hbmHeader)
{
	ASSERT(pszCaption != NULL);

	m_strCaption = pszCaption;
	CommonConstruct(pParentWnd, iSelectPage, hbmWatermark, hpalWatermark, hbmHeader);
}

void CPropertySheetEx::CommonConstruct(CWnd* pParentWnd, UINT iSelectPage,
	HBITMAP hbmWatermark, HPALETTE hpalWatermark, HBITMAP hbmHeader)
{
	memset(&m_psh, 0, sizeof(m_psh));
	m_psh.dwSize = sizeof(m_psh);
	m_psh.dwFlags = PSH_PROPSHEETPAGE;
	m_psh.pszCaption = m_strCaption;
	m_psh.nStartPage = iSelectPage;
	m_bStacked = TRUE;
	m_bModeless = FALSE;

	if (AfxHelpEnabled())
		m_psh.dwFlags |= PSH_HASHELP;

	if (hbmWatermark != NULL)
	{
		m_psh.hbmWatermark = hbmWatermark;
		m_psh.dwFlags |= (PSH_USEHBMWATERMARK | PSH_WATERMARK);
	}
	if (hpalWatermark != NULL)
	{
		m_psh.hplWatermark = hpalWatermark;
		m_psh.dwFlags |= PSH_USEHPLWATERMARK;
	}
	if (hbmHeader != NULL)
	{
		m_psh.hbmHeader = hbmHeader;
		m_psh.dwFlags |= (PSH_USEHBMHEADER | PSH_HEADER);
	}
	m_pParentWnd = pParentWnd;  // m_psh.hwndParent set in DoModal/create
}

void CPropertySheetEx::BuildPropPageArray()
{
	// delete existing prop page array
	delete[] (PROPSHEETPAGE*)m_psh.ppsp;
	m_psh.ppsp = NULL;

	// build new prop page array
	PROPSHEETPAGE* ppsp = new PROPSHEETPAGE[m_pages.GetSize()];
	m_psh.ppsp = (LPPROPSHEETPAGE)ppsp;
	BOOL bWizard = (m_psh.dwFlags & (PSH_WIZARD | PSH_WIZARD97));
	for (int i = 0; i < m_pages.GetSize(); i++)
	{
		CPropertyPage* pPage = GetPage(i);
		memcpy(&ppsp[i], &pPage->m_psp, sizeof(pPage->m_psp));
		ppsp[i].dwSize = sizeof(PROPSHEETPAGE);
		if (pPage->IsKindOf(RUNTIME_CLASS(CPropertyPageEx)))
		{
			CPropertyPageEx* pPageEx = (CPropertyPageEx*)pPage;
			if (!pPageEx->m_strHeaderTitle.IsEmpty())
			{
				ppsp[i].pszHeaderTitle = pPageEx->m_strHeaderTitle;
				ppsp[i].dwFlags |= PSP_USEHEADERTITLE;
			}
			if (!pPageEx->m_strHeaderSubTitle.IsEmpty())
			{
				ppsp[i].pszHeaderSubTitle = pPageEx->m_strHeaderSubTitle;
				ppsp[i].dwFlags |= PSP_USEHEADERSUBTITLE;
			}
		}
		pPage->PreProcessPageTemplate(ppsp[i], bWizard);
	}
	m_psh.nPages = m_pages.GetSize();
}

CPropertySheetEx::~CPropertySheetEx()
{
	delete[] (PROPSHEETPAGE*)m_psh.ppsp;
}

void CPropertySheetEx::AddPage(CPropertyPageEx* pPage)
{
	ASSERT_VALID(this);
	ASSERT(pPage != NULL);
	ASSERT_KINDOF(CPropertyPageEx, pPage);
	ASSERT_VALID(pPage);

	// add page to internal list
	m_pages.Add(pPage);

	// add page externally
	if (m_hWnd != NULL)
	{
		// build new prop page array
		PROPSHEETPAGE *ppsp = new PROPSHEETPAGE[m_pages.GetSize()];
		memcpy(ppsp, m_psh.ppsp, sizeof(PROPSHEETPAGE) * (m_pages.GetSize()-1));
		delete[] (PROPSHEETPAGE*)m_psh.ppsp;
		m_psh.ppsp = ppsp;
		ppsp += m_pages.GetSize()-1;

		memcpy(ppsp, &pPage->m_psp, sizeof(pPage->m_psp));
		pPage->PreProcessPageTemplate(*ppsp, IsWizard());
		if (!pPage->m_strHeaderTitle.IsEmpty())
		{
			ppsp->pszHeaderTitle = pPage->m_strHeaderTitle;
			ppsp->dwFlags |= PSP_USEHEADERTITLE;
		}
		if (!pPage->m_strHeaderSubTitle.IsEmpty())
		{
			ppsp->pszHeaderSubTitle = pPage->m_strHeaderSubTitle;
			ppsp->dwFlags |= PSP_USEHEADERSUBTITLE;
		}
		HPROPSHEETPAGE hPSP = CreatePropertySheetPage(ppsp);
		if (hPSP == NULL)
			AfxThrowMemoryException();

		if (!SendMessage(PSM_ADDPAGE, 0, (LPARAM)hPSP))
		{
			DestroyPropertySheetPage(hPSP);
			AfxThrowMemoryException();
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CPropertySheetEx Diagnostics

#ifdef _DEBUG
void CPropertySheetEx::AssertValid() const
{
	CWnd::AssertValid();
	m_pages.AssertValid();
	ASSERT(m_psh.dwSize == sizeof(m_psh));
	ASSERT((m_psh.dwFlags & PSH_PROPSHEETPAGE) == PSH_PROPSHEETPAGE);
}

void CPropertySheetEx::Dump(CDumpContext& dc) const
{
	CWnd::Dump(dc);

	dc << "m_strCaption = " << m_strCaption << "\n";
	dc << "Number of Pages = " << m_pages.GetSize() << "\n";
	dc << "Stacked = " << m_bStacked << "\n";
	dc << "Modeless = " << m_bModeless << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CPropertyPage, CDialog)
IMPLEMENT_DYNAMIC(CPropertySheet, CWnd)
IMPLEMENT_DYNAMIC(CPropertyPageEx, CPropertyPage)
IMPLEMENT_DYNAMIC(CPropertySheetEx, CPropertySheet)

/////////////////////////////////////////////////////////////////////////////
