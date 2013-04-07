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

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

AFX_STATIC_DATA HBRUSH _afxHalftoneBrush = 0;

void AFX_CDECL AfxWingdixTerm()
{
	AfxDeleteObject((HGDIOBJ*)&_afxHalftoneBrush);
}
char _afxWingdixTerm = (char)atexit(&AfxWingdixTerm);

/////////////////////////////////////////////////////////////////////////////
// More coordinate transforms (in separate file to avoid transitive refs)

#define HIMETRIC_INCH   2540    // HIMETRIC units per inch

void CDC::DPtoHIMETRIC(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	int nMapMode;
	if (this != NULL && (nMapMode = GetMapMode()) < MM_ISOTROPIC &&
		nMapMode != MM_TEXT)
	{
		// when using a constrained map mode, map against physical inch
		((CDC*)this)->SetMapMode(MM_HIMETRIC);
		DPtoLP(lpSize);
		((CDC*)this)->SetMapMode(nMapMode);
	}
	else
	{
		// map against logical inch for non-constrained mapping modes
		int cxPerInch, cyPerInch;
		if (this != NULL)
		{
			ASSERT_VALID(this);
			ASSERT(m_hDC != NULL);  // no HDC attached or created?
			cxPerInch = GetDeviceCaps(LOGPIXELSX);
			cyPerInch = GetDeviceCaps(LOGPIXELSY);
		}
		else
		{
			cxPerInch = afxData.cxPixelsPerInch;
			cyPerInch = afxData.cyPixelsPerInch;
		}
		ASSERT(cxPerInch != 0 && cyPerInch != 0);
		lpSize->cx = MulDiv(lpSize->cx, HIMETRIC_INCH, cxPerInch);
		lpSize->cy = MulDiv(lpSize->cy, HIMETRIC_INCH, cyPerInch);
	}
}

void CDC::HIMETRICtoDP(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	int nMapMode;
	if (this != NULL && (nMapMode = GetMapMode()) < MM_ISOTROPIC &&
		nMapMode != MM_TEXT)
	{
		// when using a constrained map mode, map against physical inch
		((CDC*)this)->SetMapMode(MM_HIMETRIC);
		LPtoDP(lpSize);
		((CDC*)this)->SetMapMode(nMapMode);
	}
	else
	{
		// map against logical inch for non-constrained mapping modes
		int cxPerInch, cyPerInch;
		if (this != NULL)
		{
			ASSERT_VALID(this);
			ASSERT(m_hDC != NULL);  // no HDC attached or created?
			cxPerInch = GetDeviceCaps(LOGPIXELSX);
			cyPerInch = GetDeviceCaps(LOGPIXELSY);
		}
		else
		{
			cxPerInch = afxData.cxPixelsPerInch;
			cyPerInch = afxData.cyPixelsPerInch;
		}
		ASSERT(cxPerInch != 0 && cyPerInch != 0);
		lpSize->cx = MulDiv(lpSize->cx, cxPerInch, HIMETRIC_INCH);
		lpSize->cy = MulDiv(lpSize->cy, cyPerInch, HIMETRIC_INCH);
	}
}

void CDC::LPtoHIMETRIC(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	LPtoDP(lpSize);
	DPtoHIMETRIC(lpSize);
}

void CDC::HIMETRICtoLP(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	HIMETRICtoDP(lpSize);
	DPtoLP(lpSize);
}

/////////////////////////////////////////////////////////////////////////////
// special CDC drawing primitives/helpers

CBrush* PASCAL CDC::GetHalftoneBrush()
{
	AfxLockGlobals(CRIT_HALFTONEBRUSH);
	if (_afxHalftoneBrush == NULL)
	{
		WORD grayPattern[8];
		for (int i = 0; i < 8; i++)
			grayPattern[i] = (WORD)(0x5555 << (i & 1));
		HBITMAP grayBitmap = CreateBitmap(8, 8, 1, 1, &grayPattern);
		if (grayBitmap != NULL)
		{
			_afxHalftoneBrush = ::CreatePatternBrush(grayBitmap);
			DeleteObject(grayBitmap);
		}
	}
	AfxUnlockGlobals(CRIT_HALFTONEBRUSH);
	return CBrush::FromHandle(_afxHalftoneBrush);
}

void CDC::DrawDragRect(LPCRECT lpRect, SIZE size,
	LPCRECT lpRectLast, SIZE sizeLast, CBrush* pBrush, CBrush* pBrushLast)
{
	ASSERT(AfxIsValidAddress(lpRect, sizeof(RECT), FALSE));
	ASSERT(lpRectLast == NULL ||
		AfxIsValidAddress(lpRectLast, sizeof(RECT), FALSE));

	// first, determine the update region and select it
	CRgn rgnNew;
	CRgn rgnOutside, rgnInside;
	rgnOutside.CreateRectRgnIndirect(lpRect);
	CRect rect = *lpRect;
	rect.InflateRect(-size.cx, -size.cy);
	rect.IntersectRect(rect, lpRect);
	rgnInside.CreateRectRgnIndirect(rect);
	rgnNew.CreateRectRgn(0, 0, 0, 0);
	rgnNew.CombineRgn(&rgnOutside, &rgnInside, RGN_XOR);

	CBrush* pBrushOld = NULL;
	if (pBrush == NULL)
		pBrush = CDC::GetHalftoneBrush();
	if (pBrushLast == NULL)
		pBrushLast = pBrush;

	CRgn rgnLast, rgnUpdate;
	if (lpRectLast != NULL)
	{
		// find difference between new region and old region
		rgnLast.CreateRectRgn(0, 0, 0, 0);
		rgnOutside.SetRectRgn(lpRectLast);
		rect = *lpRectLast;
		rect.InflateRect(-sizeLast.cx, -sizeLast.cy);
		rect.IntersectRect(rect, lpRectLast);
		rgnInside.SetRectRgn(rect);
		rgnLast.CombineRgn(&rgnOutside, &rgnInside, RGN_XOR);

		// only diff them if brushes are the same
		if (pBrush->m_hObject == pBrushLast->m_hObject)
		{
			rgnUpdate.CreateRectRgn(0, 0, 0, 0);
			rgnUpdate.CombineRgn(&rgnLast, &rgnNew, RGN_XOR);
		}
	}
	if (pBrush->m_hObject != pBrushLast->m_hObject && lpRectLast != NULL)
	{
		// brushes are different -- erase old region first
		SelectClipRgn(&rgnLast);
		GetClipBox(&rect);
		pBrushOld = SelectObject(pBrushLast);
		PatBlt(rect.left, rect.top, rect.Width(), rect.Height(), PATINVERT);
		SelectObject(pBrushOld);
		pBrushOld = NULL;
	}

	// draw into the update/new region
	SelectClipRgn(rgnUpdate.m_hObject != NULL ? &rgnUpdate : &rgnNew);
	GetClipBox(&rect);
	pBrushOld = SelectObject(pBrush);
	PatBlt(rect.left, rect.top, rect.Width(), rect.Height(), PATINVERT);

	// cleanup DC
	if (pBrushOld != NULL)
		SelectObject(pBrushOld);
	SelectClipRgn(NULL);
}

void CDC::FillSolidRect(LPCRECT lpRect, COLORREF clr)
{
	ASSERT_VALID(this);
	ASSERT(m_hDC != NULL);

	::SetBkColor(m_hDC, clr);
	::ExtTextOut(m_hDC, 0, 0, ETO_OPAQUE, lpRect, NULL, 0, NULL);
}

void CDC::FillSolidRect(int x, int y, int cx, int cy, COLORREF clr)
{
	ASSERT_VALID(this);
	ASSERT(m_hDC != NULL);

	::SetBkColor(m_hDC, clr);
	CRect rect(x, y, x + cx, y + cy);
	::ExtTextOut(m_hDC, 0, 0, ETO_OPAQUE, &rect, NULL, 0, NULL);
}

void CDC::Draw3dRect(LPCRECT lpRect,
	COLORREF clrTopLeft, COLORREF clrBottomRight)
{
	Draw3dRect(lpRect->left, lpRect->top, lpRect->right - lpRect->left,
		lpRect->bottom - lpRect->top, clrTopLeft, clrBottomRight);
}

void CDC::Draw3dRect(int x, int y, int cx, int cy,
	COLORREF clrTopLeft, COLORREF clrBottomRight)
{
	FillSolidRect(x, y, cx - 1, 1, clrTopLeft);
	FillSolidRect(x, y, 1, cy - 1, clrTopLeft);
	FillSolidRect(x + cx, y, -1, cy, clrBottomRight);
	FillSolidRect(x, y + cy, cx, -1, clrBottomRight);
}

/////////////////////////////////////////////////////////////////////////////
// out-of-line CBrush, CFont, etc. helpers

// nPointSize is actually scaled 10x
BOOL CFont::CreatePointFont(int nPointSize, LPCTSTR lpszFaceName, CDC* pDC)
{
	ASSERT(AfxIsValidString(lpszFaceName));

	LOGFONT logFont;
	memset(&logFont, 0, sizeof(LOGFONT));
	logFont.lfCharSet = DEFAULT_CHARSET;
	logFont.lfHeight = nPointSize;
	lstrcpyn(logFont.lfFaceName, lpszFaceName, _countof(logFont.lfFaceName));

	return CreatePointFontIndirect(&logFont, pDC);
}

// pLogFont->nHeight is interpreted as PointSize * 10
BOOL CFont::CreatePointFontIndirect(const LOGFONT* lpLogFont, CDC* pDC)
{
	ASSERT(AfxIsValidAddress(lpLogFont, sizeof(LOGFONT), FALSE));
	HDC hDC;
	if (pDC != NULL)
	{
		ASSERT_VALID(pDC);
		ASSERT(pDC->m_hAttribDC != NULL);
		hDC = pDC->m_hAttribDC;
	}
	else
		hDC = ::GetDC(NULL);

	// convert nPointSize to logical units based on pDC
	LOGFONT logFont = *lpLogFont;
	POINT pt;
	pt.y = ::GetDeviceCaps(hDC, LOGPIXELSY) * logFont.lfHeight;
	pt.y /= 720;    // 72 points/inch, 10 decipoints/point
	::DPtoLP(hDC, &pt, 1);
	POINT ptOrg = { 0, 0 };
	::DPtoLP(hDC, &ptOrg, 1);
	logFont.lfHeight = -abs(pt.y - ptOrg.y);

	if (pDC == NULL)
		ReleaseDC(NULL, hDC);

	return CreateFontIndirect(&logFont);
}

/////////////////////////////////////////////////////////////////////////////
// out-of-line CRect, CSize, etc. helpers

void CRect::NormalizeRect()
{
	int nTemp;
	if (left > right)
	{
		nTemp = left;
		left = right;
		right = nTemp;
	}
	if (top > bottom)
	{
		nTemp = top;
		top = bottom;
		bottom = nTemp;
	}
}

void CRect::InflateRect(LPCRECT lpRect)
{
	left -= lpRect->left;
	top -= lpRect->top;
	right += lpRect->right;
	bottom += lpRect->bottom;
}

void CRect::InflateRect(int l, int t, int r, int b)
{
	left -= l;
	top -= t;
	right += r;
	bottom += b;
}

void CRect::DeflateRect(LPCRECT lpRect)
{
	left += lpRect->left;
	top += lpRect->top;
	right -= lpRect->right;
	bottom -= lpRect->bottom;
}

void CRect::DeflateRect(int l, int t, int r, int b)
{
	left += l;
	top += t;
	right -= r;
	bottom -= b;
}

CRect CRect::MulDiv(int nMultiplier, int nDivisor) const
{
	return CRect(
		::MulDiv(left, nMultiplier, nDivisor),
		::MulDiv(top, nMultiplier, nDivisor),
		::MulDiv(right, nMultiplier, nDivisor),
		::MulDiv(bottom, nMultiplier, nDivisor));
}

/////////////////////////////////////////////////////////////////////////////
