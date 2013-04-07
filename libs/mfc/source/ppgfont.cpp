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

/////////////////////////////////////////////////////////////////////////////
// CFontPropPage implementation

BEGIN_MESSAGE_MAP(CFontPropPage, CStockPropPage)
	//{{AFX_MSG_MAP(CFontPropPage)
	ON_WM_PAINT()
	ON_CBN_SELCHANGE(AFX_IDC_FONTPROP, OnSelchangeFontprop)
	ON_CBN_EDITUPDATE(AFX_IDC_FONTNAMES, OnEditupdateFontnames)
	ON_CBN_EDITUPDATE(AFX_IDC_FONTSIZES, OnEditupdateFontsizes)
	ON_CBN_SELCHANGE(AFX_IDC_FONTNAMES, OnSelchangeFontnames)
	ON_CBN_SELCHANGE(AFX_IDC_FONTSIZES, OnSelchangeFontsizes)
	ON_CBN_SELCHANGE(AFX_IDC_FONTSTYLES, OnSelchangeFontstyles)
	ON_CBN_EDITCHANGE(AFX_IDC_FONTSTYLES, OnEditchangeFontstyles)
	ON_BN_CLICKED(AFX_IDC_STRIKEOUT, OnStrikeout)
	ON_BN_CLICKED(AFX_IDC_UNDERLINE, OnUnderline)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CFontPropPage::CFontPropPage() :
	CStockPropPage(IDD, AFX_IDS_FONT_PPG_CAPTION)
{
	//{{AFX_DATA_INIT(CFontPropPage)
	//}}AFX_DATA_INIT
}

BOOL _AfxStringFromCy(CString& str, CY& cy)
{
	VARIANTARG varCy;
	VARIANTARG varBstr;
	AfxVariantInit(&varCy);
	AfxVariantInit(&varBstr);
	V_VT(&varCy) = VT_CY;
	V_CY(&varCy) = cy;
	if (FAILED(VariantChangeType(&varBstr, &varCy, 0, VT_BSTR)))
	{
		VariantClear(&varCy);
		VariantClear(&varBstr);
		return FALSE;
	}
	str = V_BSTR(&varBstr);
	VariantClear(&varCy);
	VariantClear(&varBstr);
	return TRUE;
}

BOOL _AfxCyFromString(CY& cy, LPCTSTR psz)
{
	USES_CONVERSION;

	VARIANTARG varBstr;
	VARIANTARG varCy;
	AfxVariantInit(&varBstr);
	AfxVariantInit(&varCy);
	V_VT(&varBstr) = VT_BSTR;
	V_BSTR(&varBstr) = SysAllocString(T2COLE(psz));
	if (FAILED(VariantChangeType(&varCy, &varBstr, 0, VT_CY)))
	{
		VariantClear(&varBstr);
		VariantClear(&varCy);
		return FALSE;
	}
	cy = V_CY(&varCy);
	VariantClear(&varBstr);
	VariantClear(&varCy);
	return TRUE;
}

#define DSx     0x00660046L
#define DSna    0x00220326L

void _AfxDrawMaskedBitmap(CDC* pDC, CBitmap* pbmp, CBitmap* pbmpMask,
	int x, int y, int cx, int cy)
{
	COLORREF oldBkColor = pDC->SetBkColor(RGB(255, 255, 255));
	COLORREF oldTextColor = pDC->SetTextColor(RGB(0, 0, 0));

	CDC dcCompat;
	dcCompat.CreateCompatibleDC(pDC);
	CBitmap* pbmpSave = dcCompat.SelectObject(pbmp);
	pDC->BitBlt(x, y, cx, cy, &dcCompat, 0, 0, DSx);
	dcCompat.SelectObject(pbmpMask);
	pDC->BitBlt(x, y, cx, cy, &dcCompat, 0, 0, DSna);
	dcCompat.SelectObject(pbmp);
	pDC->BitBlt(x, y, cx, cy, &dcCompat, 0, 0, DSx);
	dcCompat.SelectObject(pbmpSave);

	pDC->SetBkColor(oldBkColor);
	pDC->SetTextColor(oldTextColor);
}

void _AfxInitMaskFromBitmap(CBitmap* pbmp, CBitmap* pbmpMask)
{
	BITMAP bmp;
	pbmp->GetObject(sizeof (BITMAP), &bmp);
	pbmpMask->CreateBitmap(bmp.bmWidth, bmp.bmHeight, 1, 1, NULL);

	CDC dcDst;
	dcDst.CreateCompatibleDC(NULL);
	CDC dcSrc;
	dcSrc.CreateCompatibleDC(NULL);
	CBitmap* pOldDst = dcDst.SelectObject(pbmpMask);
	CBitmap* pOldSrc = dcSrc.SelectObject(pbmp);

	COLORREF oldBkColor = dcSrc.SetBkColor(dcSrc.GetPixel(0, 0));
	dcDst.BitBlt(0, 0, bmp.bmWidth, bmp.bmHeight, &dcSrc, 0, 0, NOTSRCCOPY);
	dcSrc.SetBkColor(oldBkColor);

	dcDst.SelectObject(pOldDst);
	dcSrc.SelectObject(pOldSrc);
}

void CFontPropPage::DoDataExchange(CDataExchange* pDX)
{
	//{{AFX_DATA_MAP(CFontPropPage)
	DDX_Control(pDX, AFX_IDC_FONTPROP, m_FontProp);
	DDX_Control(pDX, AFX_IDC_SAMPLEBOX, m_SampleBox);
	DDX_Control(pDX, AFX_IDC_FONTSTYLES, m_FontStyles);
	DDX_Control(pDX, AFX_IDC_FONTSIZES, m_FontSizes);
	DDX_Control(pDX, AFX_IDC_FONTNAMES, m_FontNames);
	//}}AFX_DATA_MAP

	if (pDX->m_bSaveAndValidate)
	{
		FONTOBJECT fobj;
		fobj.strName = m_FontNames.GetCurrentName();

		if (fobj.strName.IsEmpty())
			return;

		m_FontSizes.GetPointSize(fobj.cySize);

		if (m_nCurrentStyle & NTM_REGULAR)
			fobj.sWeight = FW_REGULAR;
		if (m_nCurrentStyle & NTM_BOLD)
		{
			fobj.sWeight = FW_BOLD;
			fobj.bBold = TRUE;
		}
		else
			fobj.bBold = FALSE;
		if (m_nCurrentStyle & NTM_ITALIC)
			fobj.bItalic = TRUE;
		else
			fobj.bItalic = FALSE;
		fobj.bUnderline = m_bUnderline;
		fobj.bStrikethrough = m_bStrikeOut;

		SetFontProps(pDX, fobj, m_strPropName);
	}
	else
	{
		FONTOBJECT fobj;
		MERGEOBJECT mobj;
		if (!m_strPropName.IsEmpty() && GetFontProps(pDX, &fobj, m_strPropName, &mobj))
		{
			if (fobj.bBold && fobj.bItalic)
				m_nCurrentStyle = NTM_BOLD | NTM_ITALIC;
			else if (fobj.bBold)
				m_nCurrentStyle = NTM_BOLD;
			else if (fobj.bItalic)
				m_nCurrentStyle = NTM_ITALIC;
			else
				m_nCurrentStyle = NTM_REGULAR;
			m_nActualStyle = m_nCurrentStyle;
			m_bUnderline = fobj.bUnderline;
			m_bStrikeOut = fobj.bStrikethrough;

			mobj.bNameOK = TRUE;
			mobj.bSizeOK = TRUE;
			mobj.bStyleOK = TRUE;
			mobj.bUnderlineOK = TRUE;
			mobj.bStrikethroughOK = TRUE;

			_AfxStringFromCy(m_strFontSize, fobj.cySize);
			SelectFontFromList(fobj.strName, &mobj);
		}
	}
}

BOOL CFontPropPage::SetFontProps(CDataExchange* pDX, FONTOBJECT fobj, LPCTSTR pszPropName)
{
	USES_CONVERSION;

	BOOL bStatus = FALSE;
	COleDispatchDriver PropDispDriver;

	// Set the properties for all the objects
	ASSERT_KINDOF(COlePropertyPage, pDX->m_pDlgWnd);
	COlePropertyPage* propDialog = (COlePropertyPage*)(pDX->m_pDlgWnd);

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
			LPDISPATCH pFontDisp = NULL;

			// Get property
			PropDispDriver.AttachDispatch(ppDisp[i], FALSE);
			TRY
				PropDispDriver.GetProperty(dwDispID, VT_DISPATCH, &pFontDisp);
			END_TRY
			PropDispDriver.DetachDispatch();

			if (pFontDisp == NULL)
				continue;

			// Get font interface
			IFont * pFont;
			HRESULT hresult = pFontDisp->QueryInterface(IID_IFont, (void**)&pFont);
			if (hresult == S_OK)
			{
				// Set font characteristics
				if (propDialog->GetControlStatus(AFX_IDC_FONTNAMES))
				{
					BSTR bstrName = fobj.strName.AllocSysString();
					pFont->put_Name(bstrName);
					SysFreeString(bstrName);
				}
				if (propDialog->GetControlStatus(AFX_IDC_FONTSIZES))
					pFont->put_Size(fobj.cySize);
				if (propDialog->GetControlStatus(AFX_IDC_FONTSTYLES))
				{
					pFont->put_Bold(fobj.bBold);
					pFont->put_Italic(fobj.bItalic);
					pFont->put_Weight(fobj.sWeight);
				}
				if (propDialog->GetControlStatus(AFX_IDC_UNDERLINE))
					pFont->put_Underline(fobj.bUnderline);
				if (propDialog->GetControlStatus(AFX_IDC_STRIKEOUT))
					pFont->put_Strikethrough(fobj.bStrikethrough);

				// Release the font interface
				RELEASE(pFont);
				bStatus = TRUE;
			}

			// Release the font dispatch interface
			RELEASE(pFontDisp);
		}
	}
	return bStatus;
}

BOOL CFontPropPage::GetFontProps(CDataExchange* /* pDX */,
	FONTOBJECT* pfobj, LPCTSTR pszPropName, MERGEOBJECT* pmobj)
{
	USES_CONVERSION;
	BOOL bStatus = FALSE;

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
			LPDISPATCH pFontDisp;

			// Get property
			PropDispDriver.AttachDispatch(ppDisp[i], FALSE);
			PropDispDriver.GetProperty(dwDispID, VT_DISPATCH, &pFontDisp);
			PropDispDriver.DetachDispatch();

			if (pFontDisp == NULL)
				continue;

			// Get font interface
			IFont * pFont;
			HRESULT hresult = pFontDisp->QueryInterface(IID_IFont, (void**)&pFont);
			if (hresult == S_OK)
			{
				BOOL bTemp;

				// Set font characteristics
				OLECHAR *pszName;
				pFont->get_Name(&pszName);
				if (lstrcmp(OLE2CT(pszName), pfobj->strName) != 0 && i != 0)
					pmobj->bNameOK = FALSE;
				pfobj->strName = pszName;
				SysFreeString(pszName);

				CY cyTemp;
				pFont->get_Size(&cyTemp);
				if ((cyTemp.Lo != pfobj->cySize.Lo || cyTemp.Hi != pfobj->cySize.Hi) && i != 0)
					pmobj->bSizeOK = FALSE;
				pfobj->cySize = cyTemp;

				pFont->get_Bold(&bTemp);
				if (pfobj->bBold != bTemp && i != 0)
					pmobj->bStyleOK = FALSE;
				pfobj->bBold = bTemp;

				pFont->get_Italic(&bTemp);
				if (pfobj->bItalic != bTemp && i != 0)
					pmobj->bStyleOK = FALSE;
				pfobj->bItalic = bTemp;

				pFont->get_Underline(&bTemp);
				if (pfobj->bUnderline != bTemp && i != 0)
					pmobj->bUnderlineOK = FALSE;
				pfobj->bUnderline = bTemp;

				pFont->get_Strikethrough(&bTemp);
				if (pfobj->bStrikethrough != bTemp && i != 0)
					pmobj->bStrikethroughOK = FALSE;
				pfobj->bStrikethrough = bTemp;

				short sTemp;

				pFont->get_Weight(&sTemp);
				if (pfobj->sWeight != sTemp && i != 0)
					pmobj->bStyleOK = FALSE;
				pfobj->sWeight = sTemp;

				// Release the font interface
				RELEASE(pFont);
				bStatus = TRUE;
			}

			// Release font interface
			RELEASE(pFontDisp);
		}
	}
	return bStatus;
}

BOOL CFontPropPage::OnInitDialog()
{
	CStockPropPage::OnInitDialog();
	OnObjectsChanged();
	IgnoreApply(AFX_IDC_FONTPROP);

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CFontPropPage::FillFacenameList()
{
	// Clear the list
	m_FontNames.ResetContent();

	// Create a DC to enumerate
	CClientDC dc(NULL);
	EnumFontFamilies(dc.GetSafeHdc(), (LPCTSTR) NULL,
		(FONTENUMPROC)CFontPropPage::EnumFontFamiliesCallBack, (LPARAM) this);

	// Select the first one
	if (m_FontNames.SetCurSel(0) != CB_ERR)
	{
		// Fill the size list
		FillSizeList();
	}
	else
	{
		m_FontNames.EnableWindow(FALSE);
		m_FontSizes.EnableWindow(FALSE);
		m_FontStyles.EnableWindow(FALSE);
		GetDlgItem(AFX_IDC_STRIKEOUT)->EnableWindow(FALSE);
		GetDlgItem(AFX_IDC_UNDERLINE)->EnableWindow(FALSE);
	}
}

int CALLBACK CFontPropPage::EnumFontFamiliesCallBack(ENUMLOGFONT *lpelf, NEWTEXTMETRIC *, int FontType, LPARAM lParam)
{
	CFontPropPage *pDlg = (CFontPropPage *)lParam;
	ASSERT(pDlg);
	pDlg->m_FontNames.AddFont(&lpelf->elfLogFont, FontType);
	return 1;
}

AFX_STATIC_DATA int _afxTTDefaults[] = { 8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48 };

void CFontPropPage::FillSizeList()
{
	// Clear the size list
	m_FontSizes.ResetContent();
	m_FontStyles.ResetContent();
	m_nStyles = 0L;

	// Fill with "real" sizes
	CString strFaceName;
	m_FontNames.GetLBText(m_FontNames.GetCurSel(), strFaceName);
	CClientDC dc(NULL);
	EnumFontFamilies(dc.GetSafeHdc(), (LPCTSTR) strFaceName, (FONTENUMPROC) CFontPropPage::EnumFontFamiliesCallBack2, (LPARAM) this);

	// Check if we have a font that is either a vector or Truettype font
	if (m_FontNames.GetFontType() != RASTER_FONTTYPE)
	{
		// Fill with "common" sizes
		for (int i = 0; i < _countof(_afxTTDefaults); i++)
			m_FontSizes.AddSize(_afxTTDefaults[i], 0);
	}

	// See what fonts are native
	BOOL    bRegular = (BOOL)(m_nStyles & NTM_REGULAR);
	BOOL    bBold = (BOOL)(m_nStyles & NTM_BOLD);
	BOOL    bItalic = (BOOL)(m_nStyles & NTM_ITALIC);
	BOOL    bBoldItalic = (BOOL)((m_nStyles & NTM_BOLD) &&
								 (m_nStyles & NTM_ITALIC));

	// Allow for "synthesized" italic && bold variants
	if (bRegular)
		bBold = bItalic = TRUE;
	if (bBold || bItalic)
		bBoldItalic = TRUE;

	// Fill the styles list box
	CString strStyle;

	int nEntry;
	if (bRegular)
	{
		strStyle.LoadString(AFX_IDS_REGULAR);
		nEntry = m_FontStyles.AddString(strStyle);
		m_FontStyles.SetItemData(nEntry, (DWORD)NTM_REGULAR);
	}
	if (bBold)
	{
		strStyle.LoadString(AFX_IDS_BOLD);
		nEntry = m_FontStyles.AddString(strStyle);
		m_FontStyles.SetItemData(nEntry, (DWORD)NTM_BOLD);
	}
	if (bItalic)
	{
		strStyle.LoadString(AFX_IDS_ITALIC);
		nEntry = m_FontStyles.AddString(strStyle);
		m_FontStyles.SetItemData(nEntry, (DWORD)NTM_ITALIC);
	}
	if (bBoldItalic)
	{
		strStyle.LoadString(AFX_IDS_BOLDITALIC);
		nEntry = m_FontStyles.AddString(strStyle);
		m_FontStyles.SetItemData(nEntry, (DWORD)NTM_ITALIC|NTM_BOLD);
	}

	// Set the point size
	if (m_FontSizes.FindString(-1, m_strFontSize) != CB_ERR)
	{
		nEntry = m_FontSizes.SelectString(-1, m_strFontSize);
		if (nEntry == CB_ERR)
			return;
	}
	else
	{
		// Point size is not in the list so just fill the edit box
		// and don't select anything from the list
		m_FontSizes.SetCurSel(-1);
		m_FontSizes.SetWindowText(m_strFontSize);
	}

	// Set the styles combo box selection
	BOOL bFound = FALSE;
	int nMaxEntries = m_FontStyles.GetCount();
	for (int nEntry3 = 0; nEntry3 < nMaxEntries; nEntry3++)
	{
		if (m_FontStyles.GetItemData(nEntry3) == m_nActualStyle)
		{
			m_FontStyles.SetCurSel(nEntry3);
			bFound = TRUE;
		}
	}

	if (!bFound)
	{
		m_FontStyles.SetCurSel(0);      // Set style to regular
		m_nCurrentStyle = NTM_REGULAR;
	}
	else
		m_nCurrentStyle = m_nActualStyle;

	// Redraw the sample
	UpdateSampleFont();
}

int CALLBACK CFontPropPage::EnumFontFamiliesCallBack2(
	ENUMLOGFONT* lpelf, NEWTEXTMETRIC* lpntm, int FontType, LPARAM lParam)
{
	CFontPropPage *pDlg = (CFontPropPage *)lParam;
	ASSERT(pDlg != NULL);

	if (FontType & TRUETYPE_FONTTYPE)
	{
		if (!(lpntm->ntmFlags & (NTM_BOLD | NTM_ITALIC)))
			pDlg->m_nStyles |= NTM_REGULAR;

		if (lpntm->ntmFlags & NTM_ITALIC)
			pDlg->m_nStyles |= NTM_ITALIC;

		if (lpntm->ntmFlags & NTM_BOLD)
			pDlg->m_nStyles |= NTM_BOLD;
	}
	else
	{
		if (FontType & RASTER_FONTTYPE)
		{
			int height = lpntm->tmHeight - lpntm->tmInternalLeading;
			pDlg->m_FontSizes.AddSize(MulDiv(height, 72, afxData.cyPixelsPerInch), height);
		}

		if (lpelf->elfLogFont.lfWeight >= FW_BOLD && lpelf->elfLogFont.lfItalic)
			pDlg->m_nStyles |= NTM_BOLD | NTM_ITALIC;
		else if (lpelf->elfLogFont.lfWeight >= FW_BOLD)
			pDlg->m_nStyles |= NTM_BOLD;
		else if (lpelf->elfLogFont.lfItalic)
			pDlg->m_nStyles |= NTM_ITALIC;
		else
			pDlg->m_nStyles |= NTM_REGULAR;
	}

	return 1;
}

#define DX_BITMAP        20
#define DY_BITMAP        12

/////////////////////////////////////////////////////////////////////////////
// CFontComboBox

CFontComboBox::CFontComboBox() : CComboBox()
{
	m_bmpTrueType.LoadBitmap(AFX_IDB_TRUETYPE);
	_AfxInitMaskFromBitmap(&m_bmpTrueType, &m_bmpMask);
}

CFontComboBox::~CFontComboBox()
{
}

int CFontComboBox::AddFont(LOGFONT *pLF, DWORD FontType)
{
	int nEntry;
	FONTITEM_PPG* pFontItem = NULL;

	// Font already in the combobox
	if (FindString(-1, (LPCTSTR) pLF->lfFaceName) != CB_ERR)
		return CB_ERR;

	// allocate some memory for the FONTITEM_PPG structure
	TRY
	{
		pFontItem = new FONTITEM_PPG;
	}
	CATCH( CMemoryException, e )
	{
		return CB_ERR;
	}
	END_CATCH

	ASSERT( pFontItem );
	pFontItem->lf = *pLF;
	pFontItem->dwFontType = FontType;

	nEntry = AddString( (LPCTSTR) pFontItem->lf.lfFaceName );

	if (nEntry == CB_ERR)
		delete pFontItem;
	else
		SetItemData( nEntry, (DWORD) pFontItem );

	return nEntry;
}

FONTITEM_PPG* CFontComboBox::GetFontItem(int sel)
{
	if (sel == -1)
		sel = GetCurSel();

	if (sel == -1)
	{
		CString str;

		GetWindowText( str );
		sel = FindString( -1, str );
		if (sel == CB_ERR)
			sel = 0;
	}

	ASSERT( GetItemData(sel) );
	return (FONTITEM_PPG*) GetItemData(sel);
}

LPLOGFONT CFontComboBox::GetLogFont(int sel)
{
	return &GetFontItem(sel)->lf;
}

DWORD CFontComboBox::GetFontType(int sel)
{
	return GetFontItem(sel)->dwFontType;
}

CString CFontComboBox::GetCurrentName()
{
	CString str;
	GetWindowText(str);
	return str;
}

void CFontComboBox::DrawItem(LPDRAWITEMSTRUCT lpDIS)
{
	ASSERT( lpDIS->CtlType == ODT_COMBOBOX );

	// make sure this is a *real* item
	if (lpDIS->itemID == -1)
		return;

	CDC* pDC = CDC::FromHandle(lpDIS->hDC);
	FONTITEM_PPG* pFI = (FONTITEM_PPG*)lpDIS->itemData;    // pointer to a FONTITEM storied in item data
	LOGFONT* pLF = &pFI->lf;
	COLORREF crBk, crText;
	TEXTMETRIC tm;
	int x, y;

	// Calculate the colors to use
	crBk = pDC->SetBkColor(
		GetSysColor(lpDIS->itemState & ODS_SELECTED ? COLOR_HIGHLIGHT : COLOR_WINDOW) );
	crText = pDC->SetTextColor(
		GetSysColor(lpDIS->itemState & ODS_SELECTED ? COLOR_HIGHLIGHTTEXT : COLOR_WINDOWTEXT) );

	// Calculate the position of the text
	pDC->GetTextMetrics( &tm );
	x = LOWORD(GetDialogBaseUnits()) / 4;
	y = (lpDIS->rcItem.bottom + lpDIS->rcItem.top - tm.tmHeight) / 2;

	// Draw the text
	pDC->ExtTextOut(lpDIS->rcItem.left + DX_BITMAP + 2 * x, y, ETO_CLIPPED | ETO_OPAQUE,
		&lpDIS->rcItem,(LPCTSTR) pLF->lfFaceName,
		lstrlen((LPCTSTR) pLF->lfFaceName), NULL );

	// Put the colors back as they were
	pDC->SetTextColor( crText );
	pDC->SetBkColor( crBk );

	// Draw the TrueType bitmap
	if (pFI->dwFontType & TRUETYPE_FONTTYPE)
	{
		int dy;
		dy = ((lpDIS->rcItem.bottom - lpDIS->rcItem.top) - DY_BITMAP) / 2;
		_AfxDrawMaskedBitmap(pDC, &m_bmpTrueType, &m_bmpMask,
			x, lpDIS->rcItem.top + dy, DX_BITMAP, DY_BITMAP);
	}

	// Draw the focus rect if needed
	if (lpDIS->itemState & ODS_FOCUS)
		pDC->DrawFocusRect( &lpDIS->rcItem );
}

void CFontComboBox::DeleteItem(LPDELETEITEMSTRUCT lpDIS)
{
	FONTITEM_PPG* pFI;

	if (lpDIS->itemID == -1)
		return;

	ASSERT( lpDIS->CtlType == ODT_COMBOBOX );

	pFI = GetFontItem(lpDIS->itemID);

	// Free the FONTITEM_PPG created in CFontComboBox::AddFont()
	ASSERT(pFI);
	delete pFI;
}

/////////////////////////////////////////////////////////////////////////////
// CSizeComboBox

int CSizeComboBox::AddSize(int PointSize, LONG lfHeight)
{
	if (lfHeight == 0)
		lfHeight = MulDiv( -afxData.cyPixelsPerInch, PointSize, 72 );

	CString str;
	wsprintf(str.GetBuffer(16), _T("%d"), PointSize);
	str.ReleaseBuffer();

	int nMaxEntries = GetCount();
	int nEntry;

	// we use positive height values for non-truetype fonts, negitive for true type
	if (lfHeight > 0)
	{
		for (nEntry = 0; nEntry < nMaxEntries; nEntry++)
		{
			int iComp = (int)(lfHeight - GetHeight(nEntry));
			if (!iComp)
				return CB_ERR;
			if (iComp < 0)
				break;
		}
	}
	else
	{
		for (nEntry = 0; nEntry < nMaxEntries; nEntry++)
		{
			int iComp = (int)(lfHeight - GetHeight(nEntry));
			if (!iComp)
				return CB_ERR;
			if (iComp > 0)
				break;
		}
	}

	if (nEntry == nMaxEntries)
		nEntry = -1;
	nEntry = InsertString(nEntry, str);
	if (nEntry != CB_ERR)
		SetItemData(nEntry, (DWORD)lfHeight);

	return nEntry;
}

void CSizeComboBox::GetPointSize(CY& cy)
{
	TCHAR szText[20];
	GetWindowText(szText, 20);
	cy.Lo = 0;
	cy.Hi = 0;
	_AfxCyFromString(cy, szText);
}

LONG CSizeComboBox::GetHeight(int sel)
{
	if (sel == -1)
		sel = GetCurSel();

	if (sel == -1)
	{
		TCHAR szText[20];
		GetWindowText(szText, 20);
		sel = FindString( -1, szText);
		if (sel == CB_ERR)
		{
			CY cyTmp;
			cyTmp.Lo = 0;
			cyTmp.Hi = 0;
			_AfxCyFromString(cyTmp, szText);
			int PointSize = (int)((cyTmp.Lo + 5000) / 10000);
			if (PointSize != 0)
				return MulDiv(-afxData.cyPixelsPerInch, PointSize, 72);
			else
				sel = 0;
		}
	}

	return (LONG) GetItemData(sel);
}

void CSizeComboBox::UpdateLogFont( LPLOGFONT lpLF, int sel )
{
	ASSERT(lpLF);

	lpLF->lfHeight = (int)GetHeight(sel);
	lpLF->lfWidth = 0;
}

/////////////////////////////////////////////////////////////////////////////
// CFontPropPage message handlers

void CFontPropPage::OnEditupdateFontnames()
{
	// When the users entry matches an entry in the list, select it
	CString str;
	m_FontNames.GetWindowText(str);
	int nEntry = m_FontNames.FindStringExact(-1, str);
	if (nEntry != CB_ERR)
	{
		m_FontNames.SetCurSel(nEntry);
		m_FontNames.SetEditSel(-1, -1);

		// Re-fill the size list
		FillSizeList();
	}
}

void CFontPropPage::OnEditupdateFontsizes()
{
	// when the users entry matches an entry in the list, select it
	m_FontSizes.GetWindowText(m_strFontSize);
	int nEntry = m_FontSizes.FindStringExact(-1, m_strFontSize);
	if (nEntry != CB_ERR)
	{
		m_FontSizes.SetCurSel(nEntry);
		m_FontSizes.SetEditSel(-1, -1);

		// Update the sample text
		UpdateSampleFont();
	}
}

void CFontPropPage::OnSelchangeFontnames()
{
	FillSizeList();
}

void CFontPropPage::UpdateSampleFont()
{
	ASSERT(m_FontNames.GetFontItem());

	LOGFONT lf = *m_FontNames.GetLogFont();
	m_FontSizes.UpdateLogFont( &lf );

	// Handle styles
	if (m_nCurrentStyle & NTM_BOLD)
		lf.lfWeight = FW_BOLD;
	else
		lf.lfWeight = FW_REGULAR;
	if (m_nCurrentStyle & NTM_ITALIC)
		lf.lfItalic = TRUE;
	else
		lf.lfItalic = FALSE;

	lf.lfStrikeOut = (unsigned char)m_bStrikeOut;
	lf.lfUnderline = (unsigned char)m_bUnderline;

	SampleFont.DeleteObject();
	SampleFont.CreateFontIndirect( &lf );

	CRect rcSample;
	m_SampleBox.GetWindowRect( &rcSample );
	ScreenToClient( &rcSample );

	InvalidateRect( rcSample );
	UpdateWindow();
}

void CFontPropPage::OnPaint()
{
	CPaintDC dc(this);
	CRect rcText;
	CFont *oldFont;
	CSize TextExtent;
	COLORREF crText;
	TEXTMETRIC tm;
	int bkMode, len, x, y;
	CString strSample;

	strSample.LoadString(AFX_IDS_SAMPLETEXT);

	// If there is no sample font abort
	if (!SampleFont.GetSafeHandle())
		return;

	// Get the bounding box
	m_SampleBox.GetWindowRect( &rcText );
	ScreenToClient( &rcText );

	// Select the new font and colors into the dc
	oldFont = dc.SelectObject( &SampleFont );
	crText = dc.SetTextColor(GetSysColor(COLOR_WINDOWTEXT));
	bkMode = dc.SetBkMode(TRANSPARENT);

	// Calculate the position of the text
	dc.GetTextMetrics( &tm );

	len = strSample.GetLength();
	TextExtent = dc.GetTextExtent(strSample, len);
	TextExtent.cy = tm.tmAscent - tm.tmInternalLeading;

	if ((TextExtent.cx >= (rcText.right - rcText.left)) ||
			(TextExtent.cx <= 0))
		x = rcText.left;
	else
		x = rcText.left + ((rcText.right - rcText.left) - TextExtent.cx) / 2;

	y = min(rcText.bottom,
		rcText.bottom - ((rcText.bottom - rcText.top) - TextExtent.cy) / 2);

	// Draw it
	dc.ExtTextOut(x, y - (tm.tmAscent), ETO_CLIPPED, &rcText,
		strSample, len, NULL);

	// Put the DC back the way it was
	dc.SetBkMode(bkMode);
	dc.SetTextColor(crText);

	if (oldFont)
		dc.SelectObject(oldFont);
}

void CFontPropPage::OnSelchangeFontsizes()
{
	int nEntry = m_FontSizes.GetCurSel();
	if (nEntry != CB_ERR)
	{
		m_FontSizes.GetLBText(nEntry, m_strFontSize);
		UpdateSampleFont();
	}
}
void CFontPropPage::OnSelchangeFontstyles()
{
	int nEntry = m_FontStyles.GetCurSel();
	m_nCurrentStyle = m_FontStyles.GetItemData(nEntry);
	m_nActualStyle = m_nCurrentStyle;

	// Update the sample font
	UpdateSampleFont();
}

void CFontPropPage::OnEditchangeFontstyles()
{
	// when the users entry matches an entry in the list, select it
	CString str;
	m_FontStyles.GetWindowText(str);
	int nEntry = m_FontStyles.FindStringExact(-1, str);
	if (nEntry != CB_ERR)
	{
		m_FontStyles.SetCurSel(nEntry);
		m_FontStyles.SetEditSel(-1, -1);

		// Update the sample text
		m_nCurrentStyle = m_FontStyles.GetItemData(nEntry);
		m_nActualStyle = m_nCurrentStyle;
		UpdateSampleFont();
	}
}

void CFontPropPage::SelectFontFromList(CString strFaceName, MERGEOBJECT* pmobj)
{
	// Set the effects buttons
	CButton* pStrikeOut = (CButton*) GetDlgItem(AFX_IDC_STRIKEOUT);
	if (!pmobj->bStrikethroughOK)
		pStrikeOut->SetCheck(2);
	else if (m_bStrikeOut)
		pStrikeOut->SetCheck(1);
	else
		pStrikeOut->SetCheck(0);

	CButton* pUnderline = (CButton*) GetDlgItem(AFX_IDC_UNDERLINE);
	if (!pmobj->bUnderlineOK)
		pStrikeOut->SetCheck(2);
	else if (m_bUnderline)
		pUnderline->SetCheck(1);
	else
		pUnderline->SetCheck(0);

	// Set the font facename
	if (pmobj->bNameOK)
	{
		int nEntry1 = m_FontNames.SelectString(-1, strFaceName);
		if (nEntry1 == CB_ERR)
			return;
	}

	// Fill the size list appropriately
	FillSizeList();

	// Set the styles combo box selection
	BOOL bFound = FALSE;
	int nMaxEntries = m_FontStyles.GetCount();
	for (int nEntry3 = 0; nEntry3 < nMaxEntries; nEntry3++)
	{
		if (m_FontStyles.GetItemData(nEntry3) == m_nActualStyle)
		{
			m_FontStyles.SetCurSel(nEntry3);
			bFound = TRUE;
		}
	}

	if (pmobj->bSizeOK)
	{
		if (!bFound)
		{
			m_FontStyles.SetCurSel(0);      // Set style to regular
			m_nCurrentStyle = NTM_REGULAR;
		}
		else
			m_nCurrentStyle = m_nActualStyle;
	}

	UpdateSampleFont();
}

void CFontPropPage::OnStrikeout()
{
	CButton* pStrikeOut = (CButton*) GetDlgItem(AFX_IDC_STRIKEOUT);
	if (pStrikeOut->GetCheck() == 1)
		m_bStrikeOut = TRUE;
	else
		m_bStrikeOut = FALSE;

	UpdateSampleFont();
}

void CFontPropPage::OnUnderline()
{
	CButton* pUnderline = (CButton*) GetDlgItem(AFX_IDC_UNDERLINE);
	if (pUnderline->GetCheck() == 1)
		m_bUnderline = TRUE;
	else
		m_bUnderline = FALSE;

	UpdateSampleFont();
}

void CFontPropPage::OnSelchangeFontprop()
{
	OnSelchangePropname(m_FontProp);
}

BOOL CFontPropPage::OnEditProperty(DISPID dispid)
{
	return CStockPropPage::OnEditProperty(dispid, m_FontProp);
}

void CFontPropPage::OnObjectsChanged()
{
	ULONG nObjects;
	if (GetObjectArray(&nObjects) != NULL && m_hWnd != NULL)
	{
		FillPropnameList(IID_IFontDisp, 1, m_FontProp);

		if ( m_FontProp.GetCount() > 0 )
			FillFacenameList();
		else
		{
			m_FontNames.EnableWindow(FALSE);
			m_FontSizes.EnableWindow(FALSE);
			m_FontStyles.EnableWindow(FALSE);
			GetDlgItem(AFX_IDC_STRIKEOUT)->EnableWindow(FALSE);
			GetDlgItem(AFX_IDC_UNDERLINE)->EnableWindow(FALSE);
		}
	}

	if (m_hWnd != NULL)
		OnSelchangeFontprop();
}

/////////////////////////////////////////////////////////////////////////////
// Class factory for Font property page

#ifdef _AFXDLL

#ifdef AFXCTL_FACT_SEG
#pragma code_seg(AFXCTL_FACT_SEG)
#endif

IMPLEMENT_OLECREATE_EX(CFontPropPage, "OCxx.CFontPropPage",
	0x0be35200,0x8f91,0x11ce,0x9d,0xe3,0x00,0xaa,0x00,0x4b,0xb8,0x51)

BOOL CFontPropPage::CFontPropPageFactory::UpdateRegistry(BOOL bRegister)
{
	if (bRegister)
		return AfxOleRegisterPropertyPageClass(AfxGetInstanceHandle(),
			m_clsid, AFX_IDS_FONT_PPG);
	else
		return AfxOleUnregisterClass(m_clsid, NULL);
}

#endif //_AFXDLL

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CFontPropPage, CStockPropPage)
