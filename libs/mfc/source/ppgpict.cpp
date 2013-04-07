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
// CPicturePropPage implementation

BEGIN_MESSAGE_MAP(CPicturePropPage, CStockPropPage)
	//{{AFX_MSG_MAP(CPicturePropPage)
	ON_CBN_SELCHANGE(AFX_IDC_PROPNAME, OnSelchangePictProp)
	ON_WM_PAINT()
	ON_BN_CLICKED(AFX_IDC_BROWSE, OnBrowse)
	ON_BN_CLICKED(AFX_IDC_CLEAR, OnClear)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CPicturePropPage::CPicturePropPage() :
	CStockPropPage(IDD, AFX_IDS_PICTURE_PPG_CAPTION),
	m_pPictDisp(NULL)
{
	//{{AFX_DATA_INIT(CPicturePropPage)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CPicturePropPage::~CPicturePropPage()
{
	// Release picture, if any

	if (m_pPictDisp != NULL)
	{
		m_pPictDisp->Release();
		m_pPictDisp = NULL;
	}
}

BOOL CPicturePropPage::OnInitDialog()
{
	m_pPictDisp = NULL;
	SetModifiedFlag(FALSE);
	CStockPropPage::OnInitDialog();
	OnObjectsChanged();
	IgnoreApply(AFX_IDC_BROWSE);
	IgnoreApply(AFX_IDC_PROPNAME);
	OnSelchangePictProp();
	return TRUE;
}

void CPicturePropPage::DoDataExchange(CDataExchange* pDX)
{
	//{{AFX_DATA_MAP(CPicturePropPage)
	DDX_Control(pDX, AFX_IDC_PROPNAME, m_PropName);
	DDX_Control(pDX, AFX_IDC_PICTURE, m_Static);
	//}}AFX_DATA_MAP

	if (pDX->m_bSaveAndValidate)
	{
		if (IsModified())
			if (!SetPictureProp(pDX, m_pPictDisp, m_strPropName))
				GetPictureProp(pDX, &m_pPictDisp, m_strPropName);
	}
	else
	{
		GetPictureProp(pDX, &m_pPictDisp, m_strPropName);
		SetModifiedFlag(FALSE);
	}
}

BOOL CPicturePropPage::SetPictureProp(CDataExchange*, LPPICTUREDISP pPictDisp, LPCTSTR pszPropName)
{
	USES_CONVERSION;

	COleDispatchDriver PropDispDriver;

	// Set the properties for all the objects

	ULONG nObjects;
	LPDISPATCH* ppDisp = GetObjectArray(&nObjects);
	BOOL bSuccess = TRUE;

	for (ULONG i = 0; bSuccess && (i < nObjects); i++)
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
				PropDispDriver.SetProperty(dwDispID, VT_DISPATCH, pPictDisp);
			}
			CATCH(COleDispatchException, e)
			{
				// Display message box for dispatch exceptions.
				e->ReportError(MB_ICONEXCLAMATION | MB_OK);
				DELETE_EXCEPTION(e);

				bSuccess = FALSE;
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
	return bSuccess;
}

BOOL CPicturePropPage::GetPictureProp(CDataExchange*, LPPICTUREDISP* ppPictDisp, LPCTSTR pszPropName)
{
	USES_CONVERSION;

	// Release previous picture, if any.
	if (*ppPictDisp != NULL)
	{
		(*ppPictDisp)->Release();
		*ppPictDisp = NULL;
	}

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
			LPPICTUREDISP pPictDisp = NULL;

			PropDispDriver.AttachDispatch(ppDisp[i], FALSE);
			TRY
				PropDispDriver.GetProperty(dwDispID, VT_DISPATCH, &pPictDisp);
			END_TRY
			PropDispDriver.DetachDispatch();

			// If more than one object, all pictures must be the same.

			if ((i != 0) && (pPictDisp != *ppPictDisp))
			{
				if (*ppPictDisp != NULL)
					(*ppPictDisp)->Release();

				*ppPictDisp = NULL;
			}
			else
			{
				*ppPictDisp = pPictDisp;
			}

			// Don't keep multiple references to the picture.

			if ((i != 0) && (pPictDisp != NULL))
				pPictDisp->Release();
		}
	}

	InvalidateRect(NULL);
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CPicturePropPage message handlers

void CPicturePropPage::OnPaint()
{
	CPaintDC dc(this); // device context for painting

	// Determine rectangle for drawing picture

	CRect rc;
	m_Static.GetWindowRect(&rc);
	rc.InflateRect(-::GetSystemMetrics(SM_CXBORDER) * 2,
		-::GetSystemMetrics(SM_CYBORDER) * 2);
	ScreenToClient(&rc);

	LPPICTURE pPict = NULL;

	if ((m_pPictDisp != NULL) &&
		SUCCEEDED(m_pPictDisp->QueryInterface(IID_IPicture, (LPVOID*)&pPict)))
	{
		ASSERT(pPict != NULL);

		// Draw it

		OLE_XSIZE_HIMETRIC hmWidth;
		OLE_YSIZE_HIMETRIC hmHeight;

		pPict->get_Width(&hmWidth);
		pPict->get_Height(&hmHeight);

		int cxRect = rc.Width();
		int cyRect = rc.Height();
		if ((hmWidth != 0) && (hmHeight != 0) && (cxRect != 0) && (cyRect != 0))
		{
			// Adjust aspect ratio of rectangle to match picture.

			int nPictureRatio = hmWidth * 1000 / hmHeight;
			int nRectRatio = cxRect * 1000 / cyRect;

			if (nPictureRatio > nRectRatio)
			{
				int dy = (cyRect - (cxRect * 1000 / nPictureRatio) + 1) / 2;
				rc.InflateRect(0, -dy);
			}
			else
			{
				int dx = (cxRect - (cyRect * nPictureRatio / 1000) + 1) / 2;
				rc.InflateRect(-dx, 0);
			}
		}

		pPict->Render(dc.m_hDC, rc.left, rc.top, rc.Width(), rc.Height(),
			0, hmHeight-1, hmWidth, -hmHeight, &rc);

		pPict->Release();
	}
}

HRESULT _AfxCreateStreamOnFile(LPCTSTR pszPath, LPSTREAM* ppstm,
	LONG* plSize)
{
	ASSERT(pszPath != NULL);
	ASSERT(ppstm != NULL);

	*ppstm = NULL;

	if (plSize != NULL)
		*plSize = 0;

	CFileStatus fstatus;
	CFile file;
	HRESULT hr = E_FAIL;
	LONG cb;

	if (file.Open(pszPath, CFile::modeRead) &&
		file.GetStatus(pszPath, fstatus) &&
		((cb = fstatus.m_size) != -1))
	{
		HGLOBAL hGlobal = GlobalAlloc(GMEM_MOVEABLE, cb);
		LPVOID pvData = NULL;

		if (hGlobal != NULL)
		{
			if ((pvData = GlobalLock(hGlobal)) != NULL)
			{
				BOOL bRead = (file.ReadHuge(pvData, cb) == (ULONG)cb);
				GlobalUnlock(hGlobal);

				if (bRead &&
					SUCCEEDED(hr = CreateStreamOnHGlobal(hGlobal, TRUE, ppstm)))
				{
					ASSERT(*ppstm != NULL);
					if (plSize != NULL)
						*plSize = fstatus.m_size;
					hr = S_OK;
				}
			}

			if (FAILED(hr))
				GlobalFree(hGlobal);
		}
		else
		{
			hr = E_OUTOFMEMORY;
		}
	}
	else
	{
		hr = E_ACCESSDENIED;
	}

	return hr;
}

void CPicturePropPage::OnBrowse()
{
	CString strFilter;
	strFilter.LoadString(AFX_IDS_PICTUREFILTER);

	CString strTitle;
	strTitle.LoadString(AFX_IDS_PICTUREBROWSETITLE);

	CFileDialog fdlg(TRUE, NULL, NULL,
			OFN_FILEMUSTEXIST |
			OFN_HIDEREADONLY |
			OFN_PATHMUSTEXIST,
			strFilter);

	fdlg.m_ofn.lpstrTitle = strTitle;

	int nResult = fdlg.DoModal();
	SetFocus();

	if (nResult != IDOK)
		return;

	CString strPath = fdlg.GetPathName();

	LPSTREAM pstm = NULL;
	LONG lSize;
	HRESULT hr;

	if (FAILED(hr = _AfxCreateStreamOnFile(strPath, &pstm, &lSize)))
	{
		UINT idsText;
		CString strText;
		CString strCaption;

		switch (GetScode(hr))
		{
		case E_OUTOFMEMORY:
			idsText = AFX_IDP_PICTURETOOLARGE;
			break;

		case E_ACCESSDENIED:
			idsText = AFX_IDP_PICTURECANTOPEN;
			break;

		default:
			idsText = AFX_IDP_PICTUREREADFAILED;
			break;
		}

		AfxFormatString1(strText, idsText, strPath);
		strCaption.LoadString(AFX_IDS_PICTURE_PPG_CAPTION);
		MessageBox(strText, strCaption, MB_OK | MB_ICONEXCLAMATION);
		SetFocus();
		return;
	}

	ASSERT(pstm != NULL);

	LPPICTURE pPict;
	if (SUCCEEDED(::OleLoadPicture(pstm, lSize, FALSE, IID_IPicture, (LPVOID *)&pPict)))
	{
		ChangePicture(pPict);
	}
	else
	{
		CString strText;
		CString strCaption;
		AfxFormatString1(strText, AFX_IDP_PICTURECANTLOAD, strPath);
		strCaption.LoadString(AFX_IDS_PICTURE_PPG_CAPTION);
		MessageBox(strText, strCaption, MB_OK | MB_ICONEXCLAMATION);
		SetFocus();
	}

	pstm->Release();
}

AFX_STATIC_DATA PICTDESC _afxPictDescEmpty = { sizeof(PICTDESC), PICTYPE_NONE, };

void CPicturePropPage::OnClear()
{
	LPPICTURE pPict;
	if (SUCCEEDED(::OleCreatePictureIndirect(&_afxPictDescEmpty, IID_IPicture, TRUE,
		(LPVOID *)&pPict)))
	{
		ChangePicture(pPict);
	}
	else
	{
		ASSERT(FALSE);  // This should never happen!
	}
}

void CPicturePropPage::ChangePicture(LPPICTURE pPict)
{
	ASSERT(pPict != NULL);

	LPPICTUREDISP pPictDisp = NULL;
	if (SUCCEEDED(pPict->QueryInterface(IID_IPictureDisp,
			(LPVOID*)&pPictDisp)))
	{
		ASSERT(pPictDisp != NULL);

		if (m_pPictDisp != NULL)
			m_pPictDisp->Release();

		m_pPictDisp = pPictDisp;
		SetModifiedFlag();

		LPPROPERTYPAGESITE pPageSite = GetPageSite();
		if (pPageSite != NULL)
			pPageSite->OnStatusChange(
				PROPPAGESTATUS_VALIDATE | PROPPAGESTATUS_DIRTY);

		InvalidateRect(NULL);
	}

	pPict->Release();
}

void CPicturePropPage::OnSelchangePictProp()
{
	OnSelchangePropname(m_PropName);

	// disable Browse button if no property selected
	CComboBox* pPropName = (CComboBox*)GetDlgItem(AFX_IDC_PROPNAME);
	CWnd* pBrowse = GetDlgItem(AFX_IDC_BROWSE);
	pBrowse->EnableWindow(pPropName->GetCurSel() != CB_ERR);

	// redraw bitmap for new picture property
	CRect rect;

	m_Static.GetWindowRect(rect);
	rect.InflateRect(-::GetSystemMetrics(SM_CXBORDER),
		-::GetSystemMetrics(SM_CYBORDER));
	ScreenToClient(rect);

	InvalidateRect(rect);
}

BOOL CPicturePropPage::OnEditProperty(DISPID dispid)
{
	return CStockPropPage::OnEditProperty(dispid, m_PropName);
}

void CPicturePropPage::OnObjectsChanged()
{
	ULONG nObjects;
	if (GetObjectArray(&nObjects) != NULL && m_hWnd != NULL)
		FillPropnameList(IID_IPictureDisp, 1, m_PropName);

	if (m_hWnd != NULL)
		OnSelchangePictProp();
}

/////////////////////////////////////////////////////////////////////////////
// Class factory for Picture property page

#ifdef _AFXDLL

#ifdef AFXCTL_FACT_SEG
#pragma code_seg(AFXCTL_FACT_SEG)
#endif

IMPLEMENT_OLECREATE_EX(CPicturePropPage, "OCxx.CPicturePropPage",
	0x0be35202,0x8f91,0x11ce,0x9d,0xe3,0x00,0xaa,0x00,0x4b,0xb8,0x51)

BOOL CPicturePropPage::CPicturePropPageFactory::UpdateRegistry(BOOL bRegister)
{
	if (bRegister)
		return AfxOleRegisterPropertyPageClass(AfxGetInstanceHandle(),
			m_clsid, AFX_IDS_PICTURE_PPG);
	else
		return AfxOleUnregisterClass(m_clsid, NULL);
}

#endif //_AFXDLL

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CPicturePropPage, CStockPropPage)
