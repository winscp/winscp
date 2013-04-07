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

#ifdef AFXCTL_CORE2_SEG
#pragma code_seg(AFXCTL_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

CPictureHolder::CPictureHolder() :
	m_pPict(NULL)
{
}

CPictureHolder::~CPictureHolder()
{
	RELEASE(m_pPict);
}

BOOL CPictureHolder::CreateEmpty()
{
	RELEASE(m_pPict);
	PICTDESC pdesc;
	pdesc.cbSizeofstruct = sizeof(pdesc);
	pdesc.picType = PICTYPE_NONE;
	return SUCCEEDED(::OleCreatePictureIndirect(&pdesc, IID_IPicture, FALSE,
		(LPVOID*)&m_pPict));
}

BOOL CPictureHolder::CreateFromBitmap(UINT idResource)
{
	CBitmap bmp;
	bmp.LoadBitmap(idResource);
	return CreateFromBitmap((HBITMAP)bmp.Detach(), NULL, TRUE);
}

BOOL CPictureHolder::CreateFromBitmap(CBitmap* pBitmap, CPalette* pPal,
	BOOL bTransferOwnership)
{
	HBITMAP hbm = (HBITMAP)(pBitmap->GetSafeHandle());
	HPALETTE hpal = (HPALETTE)(pPal->GetSafeHandle());

	if (bTransferOwnership)
	{
		if (pBitmap != NULL)
			pBitmap->Detach();

		if (pPal != NULL)
			pPal->Detach();
	}

	return CreateFromBitmap(hbm, hpal, bTransferOwnership);
}

BOOL CPictureHolder::CreateFromBitmap(HBITMAP hbm, HPALETTE hpal,
	BOOL bTransferOwnership)
{
	RELEASE(m_pPict);
	PICTDESC pdesc;
	pdesc.cbSizeofstruct = sizeof(pdesc);
	pdesc.picType = PICTYPE_BITMAP;
	pdesc.bmp.hbitmap = hbm;
	pdesc.bmp.hpal = hpal;
	return SUCCEEDED(::OleCreatePictureIndirect(&pdesc, IID_IPicture,
		bTransferOwnership, (LPVOID*)&m_pPict));
}

BOOL CPictureHolder::CreateFromMetafile(HMETAFILE hmf, int xExt,
	int yExt, BOOL bTransferOwnership)
{
	RELEASE(m_pPict);
	PICTDESC pdesc;
	pdesc.cbSizeofstruct = sizeof(pdesc);
	pdesc.picType = PICTYPE_METAFILE;
	pdesc.wmf.hmeta = hmf;
	pdesc.wmf.xExt = xExt;
	pdesc.wmf.yExt = yExt;
	return SUCCEEDED(::OleCreatePictureIndirect(&pdesc, IID_IPicture,
		bTransferOwnership, (LPVOID*)&m_pPict));
}

BOOL CPictureHolder::CreateFromIcon(UINT idResource)
{
	HICON hIcon = AfxGetApp()->LoadIcon(idResource);
	return CreateFromIcon(hIcon, TRUE);
}

BOOL CPictureHolder::CreateFromIcon(HICON hicon, BOOL bTransferOwnership)
{
	RELEASE(m_pPict);
	PICTDESC pdesc;
	pdesc.cbSizeofstruct = sizeof(pdesc);
	pdesc.picType = PICTYPE_ICON;
	pdesc.icon.hicon = hicon;
	return SUCCEEDED(::OleCreatePictureIndirect(&pdesc, IID_IPicture,
		bTransferOwnership, (LPVOID*)&m_pPict));
}

LPPICTUREDISP CPictureHolder::GetPictureDispatch()
{
	LPPICTUREDISP pPictDisp = NULL;

	if ((m_pPict != NULL) &&
		SUCCEEDED(m_pPict->QueryInterface(IID_IPictureDisp, (LPVOID*)&pPictDisp)))
	{
		ASSERT(pPictDisp != NULL);
	}

	return pPictDisp;
}

void CPictureHolder::SetPictureDispatch(LPPICTUREDISP pDisp)
{
	LPPICTURE pPict = NULL;

	if (m_pPict != NULL)
		m_pPict->Release();

	if ((pDisp != NULL) &&
		SUCCEEDED(pDisp->QueryInterface(IID_IPicture, (LPVOID*)&pPict)))
	{
		ASSERT(pPict != NULL);

		m_pPict = pPict;
	}
	else
	{
		m_pPict = NULL;
	}
}

void CPictureHolder::Render(CDC* pDC, const CRect& rcRender,
	const CRect& rcWBounds)
{
	if (m_pPict != NULL)
	{
		long hmWidth;
		long hmHeight;

		m_pPict->get_Width(&hmWidth);
		m_pPict->get_Height(&hmHeight);

		m_pPict->Render(pDC->m_hDC, rcRender.left, rcRender.top,
			rcRender.Width(), rcRender.Height(), 0, hmHeight-1,
			hmWidth, -hmHeight, (LPCRECT)rcWBounds);
	}
}

short CPictureHolder::GetType()
{
	short sPicType = (short)PICTYPE_UNINITIALIZED;

	if (m_pPict != NULL)
	{
		m_pPict->get_Type(&sPicType);
	}

	return sPicType;
}

BOOL CPictureHolder::GetDisplayString(CString& strValue)
{
	short sPicType = GetType();

	UINT idsType = AFX_IDS_PICTYPE_UNKNOWN;

	if ((sPicType >= PICTYPE_NONE) && (sPicType <= PICTYPE_ICON))
		idsType = AFX_IDS_PICTYPE_NONE + sPicType;

	CString strType;
	CString strFormat;
	strType.LoadString(idsType);
	strFormat.LoadString(AFX_IDS_DISPLAYSTRING_PICTURE);

	TCHAR szValue[_MAX_PATH];
	wsprintf(szValue, (LPCTSTR)strFormat, (LPCTSTR)strType);

	strValue = szValue;
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
