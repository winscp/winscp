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
#include <dlgs.h>       // for standard control IDs for commdlg

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Page Setup dialog

CPageSetupDialog::CPageSetupDialog(DWORD dwFlags, CWnd* pParentWnd) :
	CCommonDialog(pParentWnd)
{
	memset(&m_psd, 0, sizeof(m_psd));

	m_psd.lStructSize = sizeof(m_psd);
	m_psd.Flags = (dwFlags | PSD_ENABLEPAGESETUPHOOK | PSD_ENABLEPAGEPAINTHOOK);
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_psd.Flags |= PSD_SHOWHELP;
	m_psd.lpfnPageSetupHook = (COMMDLGPROC)_AfxCommDlgProc;
	m_psd.lpfnPagePaintHook = (COMMDLGPROC)CPageSetupDialog::PaintHookProc;
}

int CPageSetupDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_psd.Flags & PSD_ENABLEPAGESETUPHOOK);
	ASSERT(m_psd.Flags & PSD_ENABLEPAGEPAINTHOOK);
	ASSERT(m_psd.lpfnPageSetupHook != NULL); // can still be a user hook
	ASSERT(m_psd.lpfnPagePaintHook != NULL); // can still be a user hook

	m_psd.hwndOwner = PreModal();
	int nResult = ::PageSetupDlg(&m_psd);
	PostModal();
	return nResult ? nResult : IDCANCEL;
}

////////////////////////////////////////////////////////////////////////////
// CPageSetupDialog attributes

LPDEVMODE CPageSetupDialog::GetDevMode() const
{
	if (m_psd.hDevMode == NULL)
		return NULL;

	return (LPDEVMODE)::GlobalLock(m_psd.hDevMode);
}

CString CPageSetupDialog::GetDriverName() const
{
	if (m_psd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_psd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wDriverOffset;
}

CString CPageSetupDialog::GetDeviceName() const
{
	if (m_psd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_psd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wDeviceOffset;
}

CString CPageSetupDialog::GetPortName() const
{
	if (m_psd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_psd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wOutputOffset;
}

// Create an HDC from the devnames and devmode.
HDC CPageSetupDialog::CreatePrinterDC()
{
	ASSERT_VALID(this);
	return AfxCreateDC(m_psd.hDevNames, m_psd.hDevMode);
}

void CPageSetupDialog::GetMargins(LPRECT lpRectMargins, LPRECT lpRectMinMargins) const
{
	if (lpRectMargins != NULL)
		memcpy(lpRectMargins, &m_psd.rtMargin, sizeof(RECT));
	if (lpRectMinMargins != NULL)
		memcpy(lpRectMinMargins, &m_psd.rtMinMargin, sizeof(RECT));
}

////////////////////////////////////////////////////////////////////////////
// CPageSetupDialog diagnostics

#ifdef _DEBUG
void CPageSetupDialog::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	dc << "m_psd.hwndOwner = " << (UINT)m_psd.hwndOwner;
	dc << "\nm_psd.Flags = " << (LPVOID)m_psd.Flags;

	dc << "\nm_psd.ptPaperSize = " << m_psd.ptPaperSize;
	dc << "\nm_psd.rtMinMargin = " << m_psd.rtMinMargin;
	dc << "\nm_psd.rtMinMargin = " << m_psd.rtMinMargin;

	if (m_psd.lpfnPageSetupHook == (COMMDLGPROC)_AfxCommDlgProc)
		dc << "\nsetup hook function set to standard MFC hook function";
	else
		dc << "\nsetup hook function set to non-standard hook function";

	if (m_psd.lpfnPagePaintHook == (COMMDLGPROC)_AfxCommDlgProc)
		dc << "\nprint hook function set to standard MFC hook function";
	else
		dc << "\nprint hook function set to non-standard hook function";

	dc << "\n";
}
#endif //_DEBUG

////////////////////////////////////////////////////////////////////////////
// CPageSetupDialog hook

UINT CPageSetupDialog::PreDrawPage(WORD /*wPaperType*/, WORD /*wFlags*/,
	LPPAGESETUPDLG)
{
	return 0;
	//return 1 to prevent any more drawing
}

UINT CPageSetupDialog::OnDrawPage(CDC*, UINT /*nMessage*/, LPRECT)
{
	return 0; // do the default
}

UINT CALLBACK CPageSetupDialog::PaintHookProc(HWND hWnd, UINT message, WPARAM wParam,
	LPARAM lParam)
{
	if (hWnd == NULL)
		return 0;
	// Get our Window
	// assume it is already wired up to a permanent one
	// the hWnd is the HWND of a control in the page setup proc
	CPageSetupDialog* pDlg = DYNAMIC_DOWNCAST(CPageSetupDialog,
		CWnd::FromHandlePermanent(::GetParent(hWnd)));
	if (pDlg == NULL)
		return 0;
	switch (message)
	{
	case WM_PSD_PAGESETUPDLG:
		return pDlg->PreDrawPage(LOWORD(wParam), HIWORD(wParam),
			(LPPAGESETUPDLG) lParam);
		break;
	case WM_PSD_FULLPAGERECT:
	case WM_PSD_MINMARGINRECT:
	case WM_PSD_MARGINRECT:
	case WM_PSD_GREEKTEXTRECT:
	case WM_PSD_ENVSTAMPRECT:
	case WM_PSD_YAFULLPAGERECT:
		return pDlg->OnDrawPage(CDC::FromHandle((HDC)wParam), message, (LPRECT)lParam);
		break;
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
// Print/Print Setup dialog

BEGIN_MESSAGE_MAP(CPrintDialog, CCommonDialog)
	//{{AFX_MSG_MAP(CPrintDialog)
	ON_COMMAND(psh1, OnPrintSetup) // print setup button when print is displayed
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CPrintDialog::CPrintDialog(BOOL bPrintSetupOnly,
	DWORD dwFlags, CWnd* pParentWnd)
	: m_pd(m_pdActual), CCommonDialog(pParentWnd)
{
	memset(&m_pdActual, 0, sizeof(m_pdActual));

	m_pd.lStructSize = sizeof(m_pdActual);
	m_pd.Flags = dwFlags;
	if (!afxData.bWin4 && AfxHelpEnabled())
	{
		m_pd.Flags |= PD_SHOWHELP | PD_ENABLEPRINTHOOK | PD_ENABLESETUPHOOK;
		m_pd.lpfnPrintHook = (COMMDLGPROC)_AfxCommDlgProc;
		m_pd.lpfnSetupHook = (COMMDLGPROC)_AfxCommDlgProc;
	}

	if (bPrintSetupOnly)
	{
		m_nIDHelp = AFX_IDD_PRINTSETUP;
		m_pd.Flags |= PD_PRINTSETUP;
	}
	else
	{
		m_nIDHelp = AFX_IDD_PRINT;
		m_pd.Flags |= PD_RETURNDC;
	}

	m_pd.Flags &= ~PD_RETURNIC; // do not support information context
}

// Helper ctor for AttachOnSetup
CPrintDialog::CPrintDialog(PRINTDLG& pdInit)
	: m_pd(pdInit), CCommonDialog(NULL)
{
}

// Function to keep m_pd in sync after user invokes Setup from
// the print dialog (via the Setup button)
// If you decide to handle any messages/notifications and wish to
// handle them differently between Print/PrintSetup then override
// this function and create an object of a derived class
CPrintDialog* CPrintDialog::AttachOnSetup()
{
	ASSERT_VALID(this);

	CPrintDialog* pDlgSetup;

	pDlgSetup = new CPrintDialog(m_pd);
	pDlgSetup->m_hWnd = NULL;
	pDlgSetup->m_pParentWnd = m_pParentWnd;
	pDlgSetup->m_nIDHelp = AFX_IDD_PRINTSETUP;
	return pDlgSetup;
}

void CPrintDialog::OnPrintSetup()
{
	ASSERT_VALID(this);

	CPrintDialog* pDlgSetup = AttachOnSetup();
	ASSERT(pDlgSetup != NULL);

	AfxHookWindowCreate(pDlgSetup);
	Default();
	AfxUnhookWindowCreate();

	delete pDlgSetup;
}

int CPrintDialog::DoModal()
{
	ASSERT_VALID(this);

	m_pd.hwndOwner = PreModal();
	int nResult = ::PrintDlg(&m_pd);
	PostModal();
	return nResult ? nResult : IDCANCEL;
}

// Create an HDC without calling DoModal.
HDC CPrintDialog::CreatePrinterDC()
{
	ASSERT_VALID(this);
	m_pd.hDC = AfxCreateDC(m_pd.hDevNames, m_pd.hDevMode);
	return m_pd.hDC;
}

int CPrintDialog::GetCopies() const
{
	ASSERT_VALID(this);

	if (m_pd.Flags & PD_USEDEVMODECOPIES)
		return GetDevMode()->dmCopies;
	else
		return m_pd.nCopies;
}

LPDEVMODE CPrintDialog::GetDevMode() const
{
	if (m_pd.hDevMode == NULL)
		return NULL;

	return (LPDEVMODE)::GlobalLock(m_pd.hDevMode);
}

CString CPrintDialog::GetDriverName() const
{
	if (m_pd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_pd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wDriverOffset;
}

CString CPrintDialog::GetDeviceName() const
{
	if (m_pd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_pd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wDeviceOffset;
}

CString CPrintDialog::GetPortName() const
{
	if (m_pd.hDevNames == NULL)
		return afxEmptyString;

	LPDEVNAMES lpDev = (LPDEVNAMES)GlobalLock(m_pd.hDevNames);
	return (LPCTSTR)lpDev + lpDev->wOutputOffset;
}

// this function must not be in afxdlgs.inl because of DLL delay loading
BOOL CPrintDialog::GetDefaults()
{
	m_pd.Flags |= PD_RETURNDEFAULT;
	return ::PrintDlg(&m_pd);
}

////////////////////////////////////////////////////////////////////////////
// CPrintDialog diagnostics

#ifdef _DEBUG
void CPrintDialog::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	dc << "m_pd.hwndOwner = " << (UINT)m_pd.hwndOwner;

	if (m_pd.hDC != NULL)
		dc << "\nm_pd.hDC = " << CDC::FromHandle(m_pd.hDC);

	dc << "\nm_pd.Flags = " << (LPVOID)m_pd.Flags;
	dc << "\nm_pd.nFromPage = " << m_pd.nFromPage;
	dc << "\nm_pd.nToPage = " << m_pd.nToPage;
	dc << "\nm_pd.nMinPage = " << m_pd.nMinPage;
	dc << "\nm_pd.nMaxPage = " << m_pd.nMaxPage;
	dc << "\nm_pd.nCopies = " << m_pd.nCopies;

	if (m_pd.lpfnSetupHook == (COMMDLGPROC)_AfxCommDlgProc)
		dc << "\nsetup hook function set to standard MFC hook function";
	else
		dc << "\nsetup hook function set to non-standard hook function";

	if (m_pd.lpfnPrintHook == (COMMDLGPROC)_AfxCommDlgProc)
		dc << "\nprint hook function set to standard MFC hook function";
	else
		dc << "\nprint hook function set to non-standard hook function";

	dc << "\n";
}
#endif //_DEBUG

////////////////////////////////////////////////////////////////////////////
// AfxCreateDC

HDC AFXAPI AfxCreateDC(HGLOBAL hDevNames, HGLOBAL hDevMode)
{
	if (hDevNames == NULL)
		return NULL;

	LPDEVNAMES lpDevNames = (LPDEVNAMES)::GlobalLock(hDevNames);
	LPDEVMODE  lpDevMode = (hDevMode != NULL) ?
						(LPDEVMODE)::GlobalLock(hDevMode) : NULL;

	if (lpDevNames == NULL)
		return NULL;

	HDC hDC = ::CreateDC((LPCTSTR)lpDevNames + lpDevNames->wDriverOffset,
					  (LPCTSTR)lpDevNames + lpDevNames->wDeviceOffset,
					  (LPCTSTR)lpDevNames + lpDevNames->wOutputOffset,
					  lpDevMode);

	::GlobalUnlock(hDevNames);
	if (hDevMode != NULL)
		::GlobalUnlock(hDevMode);
	return hDC;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CPrintDialog, CDialog)
IMPLEMENT_DYNAMIC(CPageSetupDialog, CDialog)

////////////////////////////////////////////////////////////////////////////
