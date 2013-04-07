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

#ifdef AFX_CMNCTL_SEG
#pragma code_seg(AFX_CMNCTL_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CImageList

BOOL CImageList::DrawIndirect(IMAGELISTDRAWPARAMS* pimldp)
{
	ASSERT(m_hImageList != NULL);
	ASSERT_POINTER(pimldp, IMAGELISTDRAWPARAMS);

	pimldp->cbSize = sizeof(IMAGELISTDRAWPARAMS);
	pimldp->himl = m_hImageList;
	return ImageList_DrawIndirect(pimldp);
}

BOOL CImageList::DrawIndirect(CDC* pDC, int nImage, POINT pt,
		SIZE sz, POINT ptOrigin, UINT fStyle /* = ILD_NORMAL */,
		DWORD dwRop /* = SRCCOPY */, COLORREF rgbBack /* = CLR_DEFAULT */,
		COLORREF rgbFore /* = CLR_DEFAULT */)
{
	ASSERT_POINTER(pDC, CDC);
	ASSERT(pDC->m_hDC != NULL);

	IMAGELISTDRAWPARAMS drawing;

	drawing.i = nImage;
	drawing.hdcDst = pDC->m_hDC;
	drawing.x = pt.x;
	drawing.y = pt.y;
	drawing.cx = sz.cx;
	drawing.cy = sz.cy;
	drawing.xBitmap = ptOrigin.x;
	drawing.yBitmap = ptOrigin.y;
	drawing.rgbBk = rgbBack;
	drawing.rgbFg = rgbFore;
	drawing.fStyle = fStyle;
	drawing.dwRop = dwRop;

	// this call initializes cbSize and himl;
	return DrawIndirect(&drawing);
}

BOOL CImageList::Create(CImageList* pImageList)
{
	ASSERT(pImageList != NULL);
	return Attach(ImageList_Duplicate(pImageList->m_hImageList));
}

/////////////////////////////////////////////////////////////////////////////
