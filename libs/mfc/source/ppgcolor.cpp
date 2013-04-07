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

#define RGB_BUTTON_BLACK    (GetSysColor(COLOR_WINDOWFRAME))
#define RGB_BUTTON_WHITE    (GetSysColor(COLOR_BTNHIGHLIGHT))
#define RGB_BUTTON_LIGHT    (GetSysColor(COLOR_BTNFACE))
#define RGB_BUTTON_DARK     (GetSysColor(COLOR_BTNSHADOW))

//Colors : Standard colors for QcQp
#define BLACK           0
#define WHITE           1
#define RED             2
#define GREEN           3
#define BLUE            4
#define YELLOW          5
#define MAGENTA         6
#define CYAN            7
#define GRAY            8
#define LIGHTGRAY       9
#define DARKRED         10
#define DARKGREEN       11
#define DARKBLUE        12
#define LIGHTBROWN      13
#define DARKMAGENTA     14
#define DARKCYAN        15

/////////////////////////////////////////////////////////////////////////////
// Structure used to store information about the system colors

struct SysColorsInfo
{
	UINT    nStrID;         // The id of the resource describing this system color
	int     nColor;         // The system color index
};

// 3D Drawing helpers
void _AfxDraw3DFrame(CDC *pDC, CRect rcBox, COLORREF colBottomRight,
	COLORREF colTopLeft);
void _AfxDraw3DButtonFrame(CDC *pDC, CRect rcButton, BOOL fFocus);

// Function to initialize the palette
HPALETTE _AfxInitPalette(VOID);
HPALETTE hPal;

// The system colors information
AFX_STATIC_DATA const SysColorsInfo _afxSysColorsList[] =
{
	AFX_IDS_COLOR_DESKTOP, COLOR_BACKGROUND,
	AFX_IDS_COLOR_APPWORKSPACE, COLOR_APPWORKSPACE,
	AFX_IDS_COLOR_WNDBACKGND, COLOR_WINDOW,
	AFX_IDS_COLOR_WNDTEXT, COLOR_WINDOWTEXT,
	AFX_IDS_COLOR_MENUBAR, COLOR_MENU,
	AFX_IDS_COLOR_MENUTEXT, COLOR_MENUTEXT,
	AFX_IDS_COLOR_ACTIVEBAR, COLOR_ACTIVECAPTION,
	AFX_IDS_COLOR_INACTIVEBAR, COLOR_INACTIVECAPTION,
	AFX_IDS_COLOR_ACTIVETEXT, COLOR_CAPTIONTEXT,
	AFX_IDS_COLOR_INACTIVETEXT, COLOR_INACTIVECAPTIONTEXT,
	AFX_IDS_COLOR_ACTIVEBORDER, COLOR_ACTIVEBORDER,
	AFX_IDS_COLOR_INACTIVEBORDER, COLOR_INACTIVEBORDER,
	AFX_IDS_COLOR_WNDFRAME, COLOR_WINDOWFRAME,
	AFX_IDS_COLOR_SCROLLBARS, COLOR_SCROLLBAR,
	AFX_IDS_COLOR_BTNFACE, COLOR_BTNFACE,
	AFX_IDS_COLOR_BTNSHADOW, COLOR_BTNSHADOW,
	AFX_IDS_COLOR_BTNTEXT, COLOR_BTNTEXT,
	AFX_IDS_COLOR_BTNHIGHLIGHT, COLOR_BTNHIGHLIGHT,
	AFX_IDS_COLOR_DISABLEDTEXT, COLOR_GRAYTEXT,
	AFX_IDS_COLOR_HIGHLIGHT, COLOR_HIGHLIGHT,
	AFX_IDS_COLOR_HIGHLIGHTTEXT, COLOR_HIGHLIGHTTEXT,

	// End of list
	NULL, NULL
};

/////////////////////////////////////////////////////////////////////////////
// CColorButton class
// All buttons are initially unselected

CColorButton::CColorButton()
{
	m_fSelected = FALSE;
}

// Set the face color of the button
void CColorButton::SetFaceColor(COLORREF colFace)
{
	m_colFace = colFace;
}

// Return the palette reference for the button
COLORREF CColorButton::colGetFaceColor()
{
	return m_colFace;
}

// Set a color button's selection state
void CColorButton::SetState(BOOL fSelected)
{
	m_fSelected = fSelected;
}

// Static ID shows which color button generated the BN_CLICKED message
UINT CColorButton::idClicked = 0;

// Redraw the color button, and record a change in selection status
void CColorButton::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	CDC *pDC;
	CPalette *pPal;
	CRect rcButton;
	CBrush *pBrush;

	// Get the palette for the item display context
	pPal = CPalette::FromHandle(hPal);
	VERIFY(pPal);

	// Get the device context from the context
	pDC = CDC::FromHandle(lpDrawItemStruct->hDC);
	VERIFY(pDC);

	VERIFY(pDC->SelectPalette(pPal,0));
	pDC->RealizePalette();

	switch(lpDrawItemStruct->itemAction)
	{
		case ODA_SELECT:
			if (CButton::GetState() & 0x0004)
			{
				idClicked = lpDrawItemStruct->CtlID;
				// Redraw done via the BN_CLICKED notification
			}
			break;

		case ODA_DRAWENTIRE:
			rcButton = lpDrawItemStruct->rcItem;
			rcButton.InflateRect(-2, -2);
			if (m_fSelected)
			{
				_AfxDraw3DFrame(pDC, rcButton, RGB_BUTTON_DARK, RGB_BUTTON_WHITE);
				rcButton.InflateRect(-1, -1);
				_AfxDraw3DFrame(pDC, rcButton, RGB_BUTTON_DARK, RGB_BUTTON_WHITE);
				rcButton.InflateRect(-1, -1);
			}

			// Then the button face in the correct color
			pBrush = new CBrush(pDC->GetNearestColor(m_colFace));
			pDC->FillRect(&rcButton, pBrush);
			delete pBrush;

			// Drop through to draw Frame.
		case ODA_FOCUS:
			rcButton = lpDrawItemStruct->rcItem;
			_AfxDraw3DButtonFrame(pDC, rcButton, GetState() & 0x0008);
	}
}

/////////////////////////////////////////////////////////////////////////////
// CColorPropPage implementation

BEGIN_MESSAGE_MAP(CColorPropPage, CStockPropPage)
	//{{AFX_MSG_MAP(CColorPropPage)
	ON_CBN_SELCHANGE(AFX_IDC_COLORPROP, OnSelchangeColorprop)
	ON_BN_CLICKED(AFX_IDC_COLOR_BLACK, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_WHITE, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_RED, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_GREEN, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_BLUE, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_YELLOW, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_MAGENTA, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_CYAN, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_GRAY, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_LIGHTGRAY, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_DARKRED, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_DARKGREEN, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_DARKBLUE, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_LIGHTBROWN, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_DARKMAGENTA, OnSelect)
	ON_BN_CLICKED(AFX_IDC_COLOR_DARKCYAN, OnSelect)
	ON_CBN_SELCHANGE(AFX_IDC_SYSTEMCOLORS, OnSelchangeSystemcolors)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CColorPropPage::CColorPropPage() :
	CStockPropPage(IDD, AFX_IDS_COLOR_PPG_CAPTION)
{
	hPal = _AfxInitPalette();

	//{{AFX_DATA_INIT(CColorPropPage)
	//}}AFX_DATA_INIT
}

BOOL CColorPropPage::OnInitDialog()
{
	UINT iButton;
	for (iButton = 0; iButton < NBUTTONS; iButton++)
	{
		VERIFY(m_Buttons[iButton].SubclassDlgItem(AFX_IDC_COLOR_BLACK + iButton, this));
		m_Buttons[iButton].SetFaceColor(PALETTEINDEX(iButton));
	}

	m_pSelectedButton = NULL;
	CStockPropPage::OnInitDialog();
	FillSysColors();

	OnObjectsChanged();
	IgnoreApply(AFX_IDC_COLORPROP);
	return TRUE;
}

void CColorPropPage::FillSysColors()
{
	const SysColorsInfo* pInfo = &_afxSysColorsList[0];
	while (pInfo->nStrID != NULL)
	{
		CString strColor;

		strColor.LoadString(pInfo->nStrID);
		int nIndex = m_SysColors.AddString(strColor);
		if (nIndex != CB_ERR)
			m_SysColors.SetItemData(nIndex, (DWORD)pInfo->nColor);

		pInfo++;
	}

	// Initially no system color is selected
	m_SysColors.SetCurSel(-1);
}

void CColorPropPage::DoDataExchange(CDataExchange* pDX)
{
	//{{AFX_DATA_MAP(CColorPropPage)
	DDX_Control(pDX, AFX_IDC_SYSTEMCOLORS, m_SysColors);
	DDX_Control(pDX, AFX_IDC_COLORPROP, m_ColorProp);
	//}}AFX_DATA_MAP

	if (pDX->m_bSaveAndValidate)
	{
		unsigned long color = 0;
		BOOL bChanged = TRUE;

		int nIndex = m_SysColors.GetCurSel();

		// did the user pick a particular button?
		// or did they select a system colour from the dropdown?
		if (m_pSelectedButton != NULL)
		{
			PALETTEENTRY pe;
			GetPaletteEntries(hPal, m_pSelectedButton->GetDlgCtrlID() - AFX_IDC_COLOR_BLACK, 1, &pe);
			color = RGB(pe.peRed, pe.peGreen, pe.peBlue);
		}
		else if (nIndex != CB_ERR)
		{
			color = 0x80000000 | m_SysColors.GetItemData(nIndex);
		}
		else
			bChanged = FALSE;

		if (bChanged)
			SetColorProp(pDX, color, m_strPropName);
	}
	else
	{
		unsigned long color;

		GetColorProp(pDX, &color, m_strPropName);
		if (color & 0x80000000)
		{
			// System color

			// Remove any previously selected buttons
			SetButton(NULL);

			int index;
			int cSysColors = m_SysColors.GetCount();
			unsigned long iSysColor = color & 0x000000ff;

			for (index = 0; index < cSysColors; index++)
			{
				if (m_SysColors.GetItemData(index) == iSysColor)
				{
					m_SysColors.SetCurSel(index);
					break;
				}
			}
		}
		else
		{
			// Normal color
			for (int index = 0; index < NBUTTONS; index++)
			{
				PALETTEENTRY pe;

				GetPaletteEntries(hPal, index, 1, &pe);
				if (color == RGB(pe.peRed, pe.peGreen, pe.peBlue))
					SetButton(&m_Buttons[index]);
			}
		}
	}

}

BOOL CColorPropPage::SetColorProp(CDataExchange* /* pDX */,
	unsigned long color, LPCTSTR pszPropName)
{
	USES_CONVERSION;
	COleDispatchDriver PropDispDriver;

	ULONG nObjects;
	LPDISPATCH* ppDisp = GetObjectArray(&nObjects);

	for (ULONG i = 0; i < nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful set the value
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr,
			1, m_lcid, &dwDispID)))
		{
			// Set property
			PropDispDriver.AttachDispatch(ppDisp[i], FALSE);
			TRY
			{
				PropDispDriver.SetProperty(dwDispID, VT_I4, color);
			}
			CATCH(COleDispatchException, e)
			{
				// Display message box for dispatch exceptions.
				e->ReportError(MB_ICONEXCLAMATION | MB_OK);
				DELETE_EXCEPTION(e);
			}
			AND_CATCH_ALL(e)
			{
				// Ignore other exceptions.
				DELETE_EXCEPTION(e);
			}
			END_CATCH_ALL
			PropDispDriver.DetachDispatch();
		}
	}
	return TRUE;
}

BOOL CColorPropPage::GetColorProp(CDataExchange* /* pDX */,
	unsigned long* pcolor, LPCTSTR pszPropName)
{
	USES_CONVERSION;
	COleDispatchDriver PropDispDriver;

	ULONG nObjects;
	LPDISPATCH* ppDisp = GetObjectArray(&nObjects);

	for (ULONG i = 0; i < nObjects; i++)
	{
		DISPID dwDispID;

		// Get the Dispatch ID for the property and if successful get the value
		LPCOLESTR lpOleStr = T2COLE(pszPropName);
		if (SUCCEEDED(ppDisp[i]->GetIDsOfNames(IID_NULL, (LPOLESTR*)&lpOleStr,
			1, m_lcid, &dwDispID)))
		{
			// Get property
			unsigned long color = 0;

			PropDispDriver.AttachDispatch(ppDisp[i], FALSE);
			TRY
				PropDispDriver.GetProperty(dwDispID, VT_I4, &color);
			END_TRY
			PropDispDriver.DetachDispatch();

			if (i != 0 && color != *pcolor)
				*pcolor = 0;
			else
				*pcolor = color;

		}
	}
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CColorPropPage message handlers

void CColorPropPage::OnSelect()
{
	SetButton((CColorButton *)(GetDlgItem(CColorButton::idClicked)));
}

void CColorPropPage::SetButton(CColorButton *Button)
{
	if (m_pSelectedButton != NULL)
	{
		m_pSelectedButton->SetState(FALSE);
		m_pSelectedButton->Invalidate(FALSE);
	}

	m_pSelectedButton = Button;
	if (m_pSelectedButton != NULL)
	{
		Button->SetState(TRUE);
		Button->Invalidate(FALSE);
	}

	// Deselect any system color currently selected
	m_SysColors.SetCurSel(-1);
}

void CColorPropPage::OnSelchangeColorprop()
{
	OnSelchangePropname(m_ColorProp);
}

///////////////////////////////////////////////////////////////////////////////
// 3D Drawing helpers

void _AfxDraw3DFrame(CDC *pDC, CRect rcBox, COLORREF colBottomRight, COLORREF colTopLeft)
{
	CPen *pPen2, *pPen, *pOldPen;

	pPen = new CPen(PS_SOLID, 1, colBottomRight);
	pOldPen = pDC->SelectObject(pPen);
	pDC->MoveTo(rcBox.right-1, rcBox.top);
	pDC->LineTo(rcBox.right-1, rcBox.bottom-1);
	pDC->LineTo(rcBox.left-1, rcBox.bottom-1);

	pPen2 = new CPen(PS_SOLID, 1, colTopLeft);
	pDC->SelectObject(pPen2);
	delete pPen;

	pDC->MoveTo(rcBox.left, rcBox.bottom-2);
	pDC->LineTo(rcBox.left, rcBox.top);
	pDC->LineTo(rcBox.right-1, rcBox.top);

	pDC->SelectObject(pOldPen);
	delete pPen2;
}

void _AfxDraw3DButtonFrame(CDC *pDC, CRect rcButton, BOOL fFocus)
{
	CPen *pPen, *pOldPen;
	CBrush GrayBrush(RGB_BUTTON_LIGHT);
	CBrush BlackBrush(RGB_BUTTON_BLACK);

	pPen = new CPen(PS_SOLID, 1, RGB_BUTTON_BLACK);
	pOldPen = pDC->SelectObject(pPen);

	// Draw gray outside
	pDC->FrameRect(&rcButton, &GrayBrush);
	rcButton.InflateRect(-1, -1);

	if (fFocus)
	{
		// Draw inside of border
		pDC->FrameRect(&rcButton, &BlackBrush);
		// Draw curved border on outside;
		rcButton.InflateRect(1, 1);
	}
	else
	{
		// Prepare inside border
		pDC->FrameRect(&rcButton, &GrayBrush);
	}

	pDC->MoveTo(rcButton.left+1, rcButton.top);
	pDC->LineTo(rcButton.right-1, rcButton.top);
	pDC->MoveTo(rcButton.left+1, rcButton.bottom-1);
	pDC->LineTo(rcButton.right-1, rcButton.bottom-1);
	pDC->MoveTo(rcButton.left, rcButton.top+1);
	pDC->LineTo(rcButton.left, rcButton.bottom-1);
	pDC->MoveTo(rcButton.right-1, rcButton.top+1);
	pDC->LineTo(rcButton.right-1, rcButton.bottom-1);

	pDC->SelectObject(pOldPen);
	delete pPen;
}

HPALETTE _AfxInitPalette(VOID)
{
	NPLOGPALETTE pPal; // Local palette
	HPALETTE pal;

	//Allocates space for QcQp Palette 'pPal'
	pPal = (NPLOGPALETTE) LocalAlloc(LPTR,
			(sizeof (LOGPALETTE) +
			(sizeof (PALETTEENTRY) * CColorPropPage::NBUTTONS)));
	if (pPal == NULL)
		return NULL;

	//Initialize 'pPal' fields
	pPal->palNumEntries = CColorPropPage::NBUTTONS;
	pPal->palVersion = 0x300;

	//Inits Every color of our palette
	pPal->palPalEntry[BLACK].peRed        =   0;
	pPal->palPalEntry[BLACK].peGreen      =   0;
	pPal->palPalEntry[BLACK].peBlue       =   0;
	pPal->palPalEntry[BLACK].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[WHITE].peRed        =   255;
	pPal->palPalEntry[WHITE].peGreen      =   255;
	pPal->palPalEntry[WHITE].peBlue       =   255;
	pPal->palPalEntry[WHITE].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[RED].peRed          =   255;
	pPal->palPalEntry[RED].peGreen        =   0;
	pPal->palPalEntry[RED].peBlue         =   0;
	pPal->palPalEntry[RED].peFlags        =   (BYTE) 0;

	pPal->palPalEntry[GREEN].peRed        =   0;
	pPal->palPalEntry[GREEN].peGreen      =   255;
	pPal->palPalEntry[GREEN].peBlue       =   0;
	pPal->palPalEntry[GREEN].peFlags      =   (BYTE) 0;

	pPal->palPalEntry[BLUE].peRed         =   0;
	pPal->palPalEntry[BLUE].peGreen       =   0;
	pPal->palPalEntry[BLUE].peBlue        =   255;
	pPal->palPalEntry[BLUE].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[YELLOW].peRed       =   255;
	pPal->palPalEntry[YELLOW].peGreen     =   255;
	pPal->palPalEntry[YELLOW].peBlue      =   0;
	pPal->palPalEntry[YELLOW].peFlags     =   (BYTE) 0;

	pPal->palPalEntry[MAGENTA].peRed      =   255;
	pPal->palPalEntry[MAGENTA].peGreen    =   0;
	pPal->palPalEntry[MAGENTA].peBlue     =   255;
	pPal->palPalEntry[MAGENTA].peFlags    =   (BYTE) 0;

	pPal->palPalEntry[CYAN].peRed         =   0;
	pPal->palPalEntry[CYAN].peGreen       =   255;
	pPal->palPalEntry[CYAN].peBlue        =   255;
	pPal->palPalEntry[CYAN].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[GRAY].peRed         =   128;
	pPal->palPalEntry[GRAY].peGreen       =   128;
	pPal->palPalEntry[GRAY].peBlue        =   128;
	pPal->palPalEntry[GRAY].peFlags       =   (BYTE) 0;

	pPal->palPalEntry[LIGHTGRAY].peRed    =   192;
	pPal->palPalEntry[LIGHTGRAY].peGreen  =   192;
	pPal->palPalEntry[LIGHTGRAY].peBlue   =   192;
	pPal->palPalEntry[LIGHTGRAY].peFlags  =   (BYTE) 0;

	pPal->palPalEntry[DARKRED].peRed      =   128;
	pPal->palPalEntry[DARKRED].peGreen    =   0;
	pPal->palPalEntry[DARKRED].peBlue     =   0;
	pPal->palPalEntry[DARKRED].peFlags    =   (BYTE) 0;

	pPal->palPalEntry[DARKGREEN].peRed    =   0;
	pPal->palPalEntry[DARKGREEN].peGreen  =   128;
	pPal->palPalEntry[DARKGREEN].peBlue   =   0;
	pPal->palPalEntry[DARKGREEN].peFlags  =   (BYTE) 0;

	pPal->palPalEntry[DARKBLUE].peRed     =   0;
	pPal->palPalEntry[DARKBLUE].peGreen   =   0;
	pPal->palPalEntry[DARKBLUE].peBlue    =   128;
	pPal->palPalEntry[DARKBLUE].peFlags   =   (BYTE) 0;

	pPal->palPalEntry[LIGHTBROWN].peRed   =   128;
	pPal->palPalEntry[LIGHTBROWN].peGreen =   128;
	pPal->palPalEntry[LIGHTBROWN].peBlue  =   0;
	pPal->palPalEntry[LIGHTBROWN].peFlags =   (BYTE) 0;

	pPal->palPalEntry[DARKMAGENTA].peRed  =   128;
	pPal->palPalEntry[DARKMAGENTA].peGreen=   0;
	pPal->palPalEntry[DARKMAGENTA].peBlue =   128;
	pPal->palPalEntry[DARKMAGENTA].peFlags=   (BYTE) 0;

	pPal->palPalEntry[DARKCYAN].peRed     =   0;
	pPal->palPalEntry[DARKCYAN].peGreen   =   128;
	pPal->palPalEntry[DARKCYAN].peBlue    =   128;
	pPal->palPalEntry[DARKCYAN].peFlags   =   (BYTE) 0;

	//Creates the logical palette
	pal = ::CreatePalette((LPLOGPALETTE)pPal);

	//Free allocated memory
	VERIFY(LocalFree((HLOCAL)pPal) == NULL);

	return pal;
}

void CColorPropPage::OnSelchangeSystemcolors()
{
	if (m_pSelectedButton != NULL)
	{
		m_pSelectedButton->SetState(FALSE);
		m_pSelectedButton->Invalidate(FALSE);
		m_pSelectedButton = NULL;
	}
}

BOOL CColorPropPage::OnEditProperty(DISPID dispid)
{
	return CStockPropPage::OnEditProperty(dispid, m_ColorProp);
}

void CColorPropPage::OnObjectsChanged()
{
	ULONG nObjects;
	if (GetObjectArray(&nObjects) != NULL && m_hWnd != NULL)
	{
		FillPropnameList(GUID_COLOR, 0, m_ColorProp);

		if ( m_ColorProp.GetCount() == 0 )
		{
			// No color properties: disable everything.
			UINT iButton;
			for (iButton = 0; iButton < NBUTTONS; iButton++)
				m_Buttons[iButton].EnableWindow(FALSE);
			m_SysColors.EnableWindow(FALSE);
		}
	}

	if (m_hWnd != NULL)
		OnSelchangeColorprop();
}

/////////////////////////////////////////////////////////////////////////////
// Class factory for Color property page

#ifdef _AFXDLL

#ifdef AFXCTL_FACT_SEG
#pragma code_seg(AFXCTL_FACT_SEG)
#endif

IMPLEMENT_OLECREATE_EX(CColorPropPage, "OCxx.CColorPropPage",
	0x0be35201,0x8f91,0x11ce,0x9d,0xe3,0x00,0xaa,0x00,0x4b,0xb8,0x51)

BOOL CColorPropPage::CColorPropPageFactory::UpdateRegistry(BOOL bRegister)
{
	if (bRegister)
		return AfxOleRegisterPropertyPageClass(AfxGetInstanceHandle(),
			m_clsid, AFX_IDS_COLOR_PPG);
	else
		return AfxOleUnregisterClass(m_clsid, NULL);
}

#endif //_AFXDLL

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CColorPropPage, CStockPropPage)
