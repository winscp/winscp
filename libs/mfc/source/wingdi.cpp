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

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Diagnostic Output
#ifdef _DEBUG
CDumpContext& AFXAPI operator<<(CDumpContext& dc, SIZE size)
{
	return dc << "(" << size.cx << " x " << size.cy << ")";
}

CDumpContext& AFXAPI operator<<(CDumpContext& dc, POINT point)
{
	return dc << "(" << point.x << ", " << point.y << ")";
}

CDumpContext& AFXAPI operator<<(CDumpContext& dc, const RECT& rect)
{
	return dc << "(L " << rect.left << ", T " << rect.top << ", R " <<
		rect.right << ", B " << rect.bottom << ")";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CDC

CDC::CDC()
{
	m_hDC = NULL;
	m_hAttribDC = NULL;
	m_bPrinting = FALSE;
}

#ifdef _DEBUG
void CDC::AssertValid() const
{
	CObject::AssertValid();
}

void CDC::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_hDC = " << (UINT)m_hDC;
	dc << "\nm_hAttribDC = " << (UINT)m_hAttribDC;
	dc << "\nm_bPrinting = " << m_bPrinting;

	dc << "\n";
}
#endif //_DEBUG

#include "fixalloc.h"

class CTempDC : public CDC
{
	DECLARE_DYNCREATE(CTempDC)
	DECLARE_FIXED_ALLOC(CTempDC);
};

CHandleMap* PASCAL afxMapHDC(BOOL bCreate)
{
	AFX_MODULE_THREAD_STATE* pState = AfxGetModuleThreadState();
	if (pState->m_pmapHDC == NULL && bCreate)
	{
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
#ifndef _AFX_PORTABLE
		_PNH pnhOldHandler = AfxSetNewHandler(&AfxCriticalNewHandler);
#endif
		pState->m_pmapHDC = new CHandleMap(RUNTIME_CLASS(CTempDC),
			offsetof(CDC, m_hDC), 2);

#ifndef _AFX_PORTABLE
		AfxSetNewHandler(pnhOldHandler);
#endif
		AfxEnableMemoryTracking(bEnable);
	}
	return pState->m_pmapHDC;
}

CDC* PASCAL CDC::FromHandle(HDC hDC)
{
	CHandleMap* pMap = afxMapHDC(TRUE); //create map if not exist
	ASSERT(pMap != NULL);
	CDC* pDC = (CDC*)pMap->FromHandle(hDC);
	ASSERT(pDC == NULL || pDC->m_hDC == hDC);
	return pDC;
}

BOOL CDC::Attach(HDC hDC)
{
	ASSERT(m_hDC == NULL);      // only attach once, detach on destroy
	ASSERT(m_hAttribDC == NULL);    // only attach to an empty DC

	if (hDC == NULL)
		return FALSE;
	CHandleMap* pMap = afxMapHDC(TRUE); // create map if not exist
	ASSERT(pMap != NULL);
	pMap->SetPermanent(m_hDC = hDC, this);

	SetAttribDC(m_hDC);     // Default to same as output
	return TRUE;
}

HDC CDC::Detach()
{
	HDC hDC = m_hDC;
	if (hDC != NULL)
	{
		CHandleMap* pMap = afxMapHDC(); // don't create if not exist
		if (pMap != NULL)
			pMap->RemoveHandle(m_hDC);
	}

	ReleaseAttribDC();
	m_hDC = NULL;
	return hDC;
}

BOOL CDC::DeleteDC()
{
	if (m_hDC == NULL)
		return FALSE;

	return ::DeleteDC(Detach());
}

CDC::~CDC()
{
	if (m_hDC != NULL)
		::DeleteDC(Detach());
}


void CDC::SetAttribDC(HDC hDC)  // Set the Attribute DC
{
	m_hAttribDC = hDC;
}

void CDC::SetOutputDC(HDC hDC)  // Set the Output DC
{
#ifdef _DEBUG
	CHandleMap* pMap = afxMapHDC();
	if (pMap != NULL && pMap->LookupPermanent(m_hDC) == this)
	{
		TRACE0("Cannot Set Output hDC on Attached CDC.\n");
		ASSERT(FALSE);
	}
#endif
	m_hDC = hDC;
}

void CDC::ReleaseAttribDC()     // Release the Attribute DC
{
	m_hAttribDC = NULL;
}

void CDC::ReleaseOutputDC()     // Release the Output DC
{
#ifdef _DEBUG
	CHandleMap* pMap = afxMapHDC();
	if (pMap != NULL && pMap->LookupPermanent(m_hDC) == this)
	{
		TRACE0("Cannot Release Output hDC on Attached CDC.\n");
		ASSERT(FALSE);
	}
#endif
	m_hDC = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// Out-of-line routines

int CDC::StartDoc(LPCTSTR lpszDocName)
{
	DOCINFO di;
	memset(&di, 0, sizeof(DOCINFO));
	di.cbSize = sizeof(DOCINFO);
	di.lpszDocName = lpszDocName;
	return StartDoc(&di);
}

int CDC::SaveDC()
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;
	if (m_hAttribDC != NULL)
		nRetVal = ::SaveDC(m_hAttribDC);
	if (m_hDC != m_hAttribDC && ::SaveDC(m_hDC) != 0)
		nRetVal = -1;   // -1 is the only valid restore value for complex DCs
	return nRetVal;
}

BOOL CDC::RestoreDC(int nSavedDC)
{
	// if two distinct DCs, nSavedDC can only be -1
	ASSERT(m_hDC != NULL);
	ASSERT(m_hDC == m_hAttribDC || nSavedDC == -1);

	BOOL bRetVal = TRUE;
	if (m_hDC != m_hAttribDC)
		bRetVal = ::RestoreDC(m_hDC, nSavedDC);
	if (m_hAttribDC != NULL)
		bRetVal = (bRetVal && ::RestoreDC(m_hAttribDC, nSavedDC));
	return bRetVal;
}

CGdiObject* PASCAL CDC::SelectGdiObject(HDC hDC, HGDIOBJ h)
{
	return CGdiObject::FromHandle(::SelectObject(hDC, h));
}

CGdiObject* CDC::SelectStockObject(int nIndex)
{
	ASSERT(m_hDC != NULL);

	HGDIOBJ hObject = ::GetStockObject(nIndex);
	HGDIOBJ hOldObj = NULL;

	ASSERT(hObject != NULL);
	if (m_hDC != m_hAttribDC)
		hOldObj = ::SelectObject(m_hDC, hObject);
	if (m_hAttribDC != NULL)
		hOldObj = ::SelectObject(m_hAttribDC, hObject);
	return CGdiObject::FromHandle(hOldObj);
}

CPen* CDC::SelectObject(CPen* pPen)
{
	ASSERT(m_hDC != NULL);
	HGDIOBJ hOldObj = NULL;

	if (m_hDC != m_hAttribDC)
		hOldObj = ::SelectObject(m_hDC, pPen->GetSafeHandle());
	if (m_hAttribDC != NULL)
		hOldObj = ::SelectObject(m_hAttribDC, pPen->GetSafeHandle());
	return (CPen*)CGdiObject::FromHandle(hOldObj);
}

CBrush* CDC::SelectObject(CBrush* pBrush)
{
	ASSERT(m_hDC != NULL);
	HGDIOBJ hOldObj = NULL;

	if (m_hDC != m_hAttribDC)
		hOldObj = ::SelectObject(m_hDC, pBrush->GetSafeHandle());
	if (m_hAttribDC != NULL)
		hOldObj = ::SelectObject(m_hAttribDC, pBrush->GetSafeHandle());
	return (CBrush*)CGdiObject::FromHandle(hOldObj);
}

CFont* CDC::SelectObject(CFont* pFont)
{
	ASSERT(m_hDC != NULL);
	HGDIOBJ hOldObj = NULL;

	if (m_hDC != m_hAttribDC)
		hOldObj = ::SelectObject(m_hDC, pFont->GetSafeHandle());
	if (m_hAttribDC != NULL)
		hOldObj = ::SelectObject(m_hAttribDC, pFont->GetSafeHandle());
	return (CFont*)CGdiObject::FromHandle(hOldObj);
}

int CDC::SelectObject(CRgn* pRgn)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = GDI_ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = (int)::SelectObject(m_hDC, pRgn->GetSafeHandle());
	if (m_hAttribDC != NULL)
		nRetVal = (int)::SelectObject(m_hAttribDC, pRgn->GetSafeHandle());
	return nRetVal;
}

CPalette* CDC::SelectPalette(CPalette* pPalette, BOOL bForceBackground)
{
	ASSERT(m_hDC != NULL);

	return (CPalette*) CGdiObject::FromHandle(::SelectPalette(m_hDC,
		(HPALETTE)pPalette->GetSafeHandle(), bForceBackground));
}

COLORREF CDC::SetBkColor(COLORREF crColor)
{
	ASSERT(m_hDC != NULL);
	COLORREF crRetVal = CLR_INVALID;

	if (m_hDC != m_hAttribDC)
		crRetVal = ::SetBkColor(m_hDC, crColor);
	if (m_hAttribDC != NULL)
		crRetVal = ::SetBkColor(m_hAttribDC, crColor);
	return crRetVal;
}

int CDC::SetBkMode(int nBkMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetBkMode(m_hDC, nBkMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetBkMode(m_hAttribDC, nBkMode);
	return nRetVal;
}

int CDC::SetPolyFillMode(int nPolyFillMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetPolyFillMode(m_hDC, nPolyFillMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetPolyFillMode(m_hAttribDC, nPolyFillMode);
	return nRetVal;
}

int CDC::SetROP2(int nDrawMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetROP2(m_hDC, nDrawMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetROP2(m_hAttribDC, nDrawMode);
	return nRetVal;
}

int CDC::SetStretchBltMode(int nStretchMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetStretchBltMode(m_hDC, nStretchMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetStretchBltMode(m_hAttribDC, nStretchMode);
	return nRetVal;
}

COLORREF CDC::SetTextColor(COLORREF crColor)
{
	ASSERT(m_hDC != NULL);
	COLORREF crRetVal = CLR_INVALID;

	if (m_hDC != m_hAttribDC)
		crRetVal = ::SetTextColor(m_hDC, crColor);
	if (m_hAttribDC != NULL)
		crRetVal = ::SetTextColor(m_hAttribDC, crColor);
	return crRetVal;
}

int CDC::SetMapMode(int nMapMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetMapMode(m_hDC, nMapMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetMapMode(m_hAttribDC, nMapMode);
	return nRetVal;
}

CPoint CDC::SetViewportOrg(int x, int y)
{
	ASSERT(m_hDC != NULL);
	CPoint point;

	if (m_hDC != m_hAttribDC)
		VERIFY(::SetViewportOrgEx(m_hDC, x, y, &point));
	if (m_hAttribDC != NULL)
		VERIFY(::SetViewportOrgEx(m_hAttribDC, x, y, &point));
	return point;
}

CPoint CDC::OffsetViewportOrg(int nWidth, int nHeight)
{
	ASSERT(m_hDC != NULL);
	CPoint point;

	if (m_hDC != m_hAttribDC)
		VERIFY(::OffsetViewportOrgEx(m_hDC, nWidth, nHeight, &point));
	if (m_hAttribDC != NULL)
		VERIFY(::OffsetViewportOrgEx(m_hAttribDC, nWidth, nHeight, &point));
	return point;
}

CSize CDC::SetViewportExt(int x, int y)
{
	ASSERT(m_hDC != NULL);
	CSize size;

	if (m_hDC != m_hAttribDC)
		VERIFY(::SetViewportExtEx(m_hDC, x, y, &size));
	if (m_hAttribDC != NULL)
		VERIFY(::SetViewportExtEx(m_hAttribDC, x, y, &size));
	return size;
}

CSize CDC::ScaleViewportExt(int xNum, int xDenom, int yNum, int yDenom)
{
	ASSERT(m_hDC != NULL);
	CSize size;

	if (m_hDC != m_hAttribDC)
		VERIFY(::ScaleViewportExtEx(m_hDC, xNum, xDenom, yNum, yDenom, &size));
	if (m_hAttribDC != NULL)
		VERIFY(::ScaleViewportExtEx(m_hAttribDC, xNum, xDenom, yNum, yDenom, &size));
	return size;
}

CPoint CDC::SetWindowOrg(int x, int y)
{
	ASSERT(m_hDC != NULL);
	CPoint point;

	if (m_hDC != m_hAttribDC)
		VERIFY(::SetWindowOrgEx(m_hDC, x, y, &point));
	if (m_hAttribDC != NULL)
		VERIFY(::SetWindowOrgEx(m_hAttribDC, x, y, &point));
	return point;
}

CPoint CDC::OffsetWindowOrg(int nWidth, int nHeight)
{
	ASSERT(m_hDC != NULL);
	CPoint point;

	if (m_hDC != m_hAttribDC)
		VERIFY(::OffsetWindowOrgEx(m_hDC, nWidth, nHeight, &point));
	if (m_hAttribDC != NULL)
		VERIFY(::OffsetWindowOrgEx(m_hAttribDC, nWidth, nHeight, &point));
	return point;
}

CSize CDC::SetWindowExt(int x, int y)
{
	ASSERT(m_hDC != NULL);
	CSize size;

	if (m_hDC != m_hAttribDC)
		VERIFY(::SetWindowExtEx(m_hDC, x, y, &size));
	if (m_hAttribDC != NULL)
		VERIFY(::SetWindowExtEx(m_hAttribDC, x, y, &size));
	return size;
}

CSize CDC::ScaleWindowExt(int xNum, int xDenom, int yNum, int yDenom)
{
	ASSERT(m_hDC != NULL);
	CSize size;

	if (m_hDC != m_hAttribDC)
		VERIFY(::ScaleWindowExtEx(m_hDC, xNum, xDenom, yNum, yDenom, &size));
	if (m_hAttribDC != NULL)
		VERIFY(::ScaleWindowExtEx(m_hAttribDC, xNum, xDenom, yNum, yDenom, &size));
	return size;
}

int CDC::GetClipBox(LPRECT lpRect) const
{
	ASSERT(m_hDC != NULL);
	return ::GetClipBox(m_hDC, lpRect);
}

int CDC::SelectClipRgn(CRgn* pRgn)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SelectClipRgn(m_hDC, (HRGN)pRgn->GetSafeHandle());
	if (m_hAttribDC != NULL)
		nRetVal = ::SelectClipRgn(m_hAttribDC, (HRGN)pRgn->GetSafeHandle());
	return nRetVal;
}

int CDC::ExcludeClipRect(int x1, int y1, int x2, int y2)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::ExcludeClipRect(m_hDC, x1, y1, x2, y2);
	if (m_hAttribDC != NULL)
		nRetVal = ::ExcludeClipRect(m_hAttribDC, x1, y1, x2, y2);
	return nRetVal;
}

int CDC::ExcludeClipRect(LPCRECT lpRect)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::ExcludeClipRect(m_hDC, lpRect->left, lpRect->top,
			lpRect->right, lpRect->bottom);
	if (m_hAttribDC != NULL)
		nRetVal = ::ExcludeClipRect(m_hAttribDC, lpRect->left, lpRect->top,
			lpRect->right, lpRect->bottom);
	return nRetVal;
}

int CDC::IntersectClipRect(int x1, int y1, int x2, int y2)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::IntersectClipRect(m_hDC, x1, y1, x2, y2);
	if (m_hAttribDC != NULL)
		nRetVal = ::IntersectClipRect(m_hAttribDC, x1, y1, x2, y2);
	return nRetVal;
}

int CDC::IntersectClipRect(LPCRECT lpRect)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::IntersectClipRect(m_hDC, lpRect->left, lpRect->top,
			lpRect->right, lpRect->bottom);
	if (m_hAttribDC != NULL)
		nRetVal = ::IntersectClipRect(m_hAttribDC, lpRect->left, lpRect->top,
			lpRect->right, lpRect->bottom);
	return nRetVal;
}

int CDC::OffsetClipRgn(int x, int y)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::OffsetClipRgn(m_hDC, x, y);
	if (m_hAttribDC != NULL)
		nRetVal = ::OffsetClipRgn(m_hAttribDC, x, y);
	return nRetVal;
}

int CDC::OffsetClipRgn(SIZE size)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::OffsetClipRgn(m_hDC, size.cx, size.cy);
	if (m_hAttribDC != NULL)
		nRetVal = ::OffsetClipRgn(m_hAttribDC, size.cx, size.cy);
	return nRetVal;
}

CPoint CDC::MoveTo(int x, int y)
{
	ASSERT(m_hDC != NULL);
	CPoint point;

	if (m_hDC != m_hAttribDC)
		VERIFY(::MoveToEx(m_hDC, x, y, &point));
	if (m_hAttribDC != NULL)
		VERIFY(::MoveToEx(m_hAttribDC, x, y, &point));
	return point;
}

BOOL CDC::LineTo(int x, int y)
{
	ASSERT(m_hDC != NULL);
	if (m_hAttribDC != NULL && m_hDC != m_hAttribDC)
		::MoveToEx(m_hAttribDC, x, y, NULL);
	return ::LineTo(m_hDC, x, y);
}

UINT CDC::SetTextAlign(UINT nFlags)
{
	ASSERT(m_hDC != NULL);
	UINT nRetVal = GDI_ERROR;

	if (m_hDC != m_hAttribDC)
		::SetTextAlign(m_hDC, nFlags);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetTextAlign(m_hAttribDC, nFlags);
	return nRetVal;
}

int CDC::SetTextJustification(int nBreakExtra, int nBreakCount)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0;

	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetTextJustification(m_hDC, nBreakExtra, nBreakCount);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetTextJustification(m_hAttribDC, nBreakExtra, nBreakCount);
	return nRetVal;
}

int CDC::SetTextCharacterExtra(int nCharExtra)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = 0x8000000;
	if (m_hDC != m_hAttribDC)
		nRetVal = ::SetTextCharacterExtra(m_hDC, nCharExtra);
	if (m_hAttribDC != NULL)
		nRetVal = ::SetTextCharacterExtra(m_hAttribDC, nCharExtra);
	return nRetVal;
}

DWORD CDC::SetMapperFlags(DWORD dwFlag)
{
	ASSERT(m_hDC != NULL);
	DWORD dwRetVal = GDI_ERROR;
	if (m_hDC != m_hAttribDC)
		dwRetVal = ::SetMapperFlags(m_hDC, dwFlag);
	if (m_hAttribDC != NULL)
		dwRetVal = ::SetMapperFlags(m_hAttribDC, dwFlag);
	return dwRetVal;
}

typedef DWORD (CALLBACK* AFX_GDIGETLAYOUTPROC)(HDC);
typedef DWORD (CALLBACK* AFX_GDISETLAYOUTPROC)(HDC, DWORD);

DWORD CDC::GetLayout() const
{
	ASSERT(m_hDC != NULL);

	HINSTANCE hInst = ::GetModuleHandleA("GDI32.DLL");
	ASSERT(hInst != NULL);

	DWORD dwGetLayout = LAYOUT_LTR;

	AFX_GDIGETLAYOUTPROC pfn;
	pfn = (AFX_GDIGETLAYOUTPROC) GetProcAddress(hInst, "GetLayout");

	// if they API is available, just call it. If it is not
	// available, indicate an error.

	if (pfn != NULL)
		dwGetLayout = (*pfn)(m_hDC);
	else
	{
		dwGetLayout = GDI_ERROR;
		SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
	}

	return dwGetLayout;
}

DWORD CDC::SetLayout(DWORD dwSetLayout)
{
	ASSERT(m_hDC != NULL);

	HINSTANCE hInst = ::GetModuleHandleA("GDI32.DLL");
	ASSERT(hInst != NULL);

	DWORD dwGetLayout = LAYOUT_LTR;

	AFX_GDISETLAYOUTPROC pfn;
	pfn = (AFX_GDISETLAYOUTPROC) GetProcAddress(hInst, "SetLayout");

	// If the API is availalbe, just call it. If it's not available,
	// setting anything other than LAYOUT_LTR is an error.

	if (pfn != NULL)
		dwGetLayout = (*pfn)(m_hDC, dwSetLayout);
	else if (dwSetLayout != LAYOUT_LTR)
	{
		dwGetLayout = GDI_ERROR;
		SetLastError(ERROR_CALL_NOT_IMPLEMENTED);
	}

	return dwGetLayout;
}

void CWnd::ScreenToClient(LPRECT lpRect) const
{
	ASSERT(::IsWindow(m_hWnd));
	::ScreenToClient(m_hWnd, (LPPOINT)lpRect);
	::ScreenToClient(m_hWnd, ((LPPOINT)lpRect)+1);
	if (GetExStyle() & WS_EX_LAYOUTRTL)
		CRect::SwapLeftRight(lpRect);
}

void CWnd::ClientToScreen(LPRECT lpRect) const
{
	ASSERT(::IsWindow(m_hWnd));
	::ClientToScreen(m_hWnd, (LPPOINT)lpRect);
	::ClientToScreen(m_hWnd, ((LPPOINT)lpRect)+1);
	if (GetExStyle() & WS_EX_LAYOUTRTL)
		CRect::SwapLeftRight(lpRect);
}

/////////////////////////////////////////////////////////////////////////////
// Advanced Win32 GDI functions

BOOL CDC::ArcTo(int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)
{
	ASSERT(m_hDC != NULL);
	BOOL bResult = ::ArcTo(m_hDC, x1, y1, x2, y2, x3, y3, x4, y4);
	if (m_hDC != m_hAttribDC)
	{
		CPoint pt;
		VERIFY(::GetCurrentPositionEx(m_hDC, &pt));
		VERIFY(::MoveToEx(m_hAttribDC, pt.x, pt.y, NULL));
	}
	return bResult;
}

int CDC::SetArcDirection(int nArcDirection)
{
	ASSERT(m_hDC != NULL);
	int nResult = 0;
	if (m_hDC != m_hAttribDC)
		nResult = ::SetArcDirection(m_hDC, nArcDirection);
	if (m_hAttribDC != NULL)
		nResult = ::SetArcDirection(m_hAttribDC, nArcDirection);
	return nResult;
}

BOOL CDC::PolyDraw(const POINT* lpPoints, const BYTE* lpTypes, int nCount)
{
	ASSERT(m_hDC != NULL);
	BOOL bResult = ::PolyDraw(m_hDC, lpPoints, lpTypes, nCount);
	if (m_hDC != m_hAttribDC)
	{
		CPoint pt;
		VERIFY(::GetCurrentPositionEx(m_hDC, &pt));
		VERIFY(::MoveToEx(m_hAttribDC, pt.x, pt.y, NULL));
	}
	return bResult;
}

BOOL CDC::PolylineTo(const POINT* lpPoints, int nCount)
{
	ASSERT(m_hDC != NULL);
	BOOL bResult = ::PolylineTo(m_hDC, lpPoints, nCount);
	if (m_hDC != m_hAttribDC)
	{
		CPoint pt;
		VERIFY(::GetCurrentPositionEx(m_hDC, &pt));
		VERIFY(::MoveToEx(m_hAttribDC, pt.x, pt.y, NULL));
	}
	return bResult;
}

BOOL CDC::SetColorAdjustment(const COLORADJUSTMENT* lpColorAdjust)
{
	ASSERT(m_hDC != NULL);
	BOOL bResult = FALSE;
	if (m_hDC != m_hAttribDC)
		bResult = ::SetColorAdjustment(m_hDC, lpColorAdjust);
	if (m_hAttribDC != NULL)
		bResult = ::SetColorAdjustment(m_hAttribDC, lpColorAdjust);
	return bResult;
}

BOOL CDC::PolyBezierTo(const POINT* lpPoints, int nCount)
{
	ASSERT(m_hDC != NULL);
	BOOL bResult = ::PolyBezierTo(m_hDC, lpPoints, nCount);
	if (m_hDC != m_hAttribDC)
	{
		CPoint pt;
		VERIFY(::GetCurrentPositionEx(m_hDC, &pt));
		VERIFY(::MoveToEx(m_hAttribDC, pt.x, pt.y, NULL));
	}
	return bResult;
}

BOOL CDC::SelectClipPath(int nMode)
{
	ASSERT(m_hDC != NULL);

	// output DC always holds the current path
	if (!::SelectClipPath(m_hDC, nMode))
		return FALSE;

	// transfer clipping region into the attribute DC
	BOOL bResult = TRUE;
	if (m_hDC != m_hAttribDC)
	{
		HRGN hRgn = ::CreateRectRgn(0, 0, 0, 0);
		if (::GetClipRgn(m_hDC, hRgn) < 0 || !::SelectClipRgn(m_hAttribDC, hRgn))
		{
			TRACE0("Error: unable to transfer clip region in CDC::SelectClipPath!\n");
			bResult = FALSE;
		}
		DeleteObject(hRgn);
	}
	return bResult;
}

int CDC::SelectClipRgn(CRgn* pRgn, int nMode)
{
	ASSERT(m_hDC != NULL);
	int nRetVal = ERROR;
	if (m_hDC != m_hAttribDC)
		nRetVal = ::ExtSelectClipRgn(m_hDC, (HRGN)pRgn->GetSafeHandle(), nMode);
	if (m_hAttribDC != NULL)
		nRetVal = ::ExtSelectClipRgn(m_hAttribDC, (HRGN)pRgn->GetSafeHandle(), nMode);
	return nRetVal;
}

/////////////////////////////////////////////////////////////////////////////
// Special handling for metafile playback

int CALLBACK AfxEnumMetaFileProc(HDC hDC,
	HANDLETABLE* pHandleTable, METARECORD* pMetaRec, int nHandles, LPARAM lParam)
{
	CDC* pDC = (CDC*)lParam;
	ASSERT_VALID(pDC);

	switch (pMetaRec->rdFunction)
	{
	// these records have effects different for each CDC derived class
	case META_SETMAPMODE:
		pDC->SetMapMode((int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SETWINDOWEXT:
		pDC->SetWindowExt(
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SETWINDOWORG:
		pDC->SetWindowOrg(
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SETVIEWPORTEXT:
		pDC->SetViewportExt(
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SETVIEWPORTORG:
		pDC->SetViewportOrg(
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SCALEWINDOWEXT:
		pDC->ScaleWindowExt(
			(int)(short)pMetaRec->rdParm[3], (int)(short)pMetaRec->rdParm[2],
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SCALEVIEWPORTEXT:
		pDC->ScaleViewportExt(
			(int)(short)pMetaRec->rdParm[3], (int)(short)pMetaRec->rdParm[2],
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_OFFSETVIEWPORTORG:
		pDC->OffsetViewportOrg(
			(int)(short)pMetaRec->rdParm[1], (int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SAVEDC:
		pDC->SaveDC();
		break;
	case META_RESTOREDC:
		pDC->RestoreDC((int)(short)pMetaRec->rdParm[0]);
		break;
	case META_SETBKCOLOR:
		pDC->SetBkColor(*(UNALIGNED COLORREF*)&pMetaRec->rdParm[0]);
		break;
	case META_SETTEXTCOLOR:
		pDC->SetTextColor(*(UNALIGNED COLORREF*)&pMetaRec->rdParm[0]);
		break;

	// need to watch out for SelectObject(HFONT), for custom font mapping
	case META_SELECTOBJECT:
		{
			HGDIOBJ hObject = pHandleTable->objectHandle[pMetaRec->rdParm[0]];
			UINT nObjType = GetObjectType(hObject);
			if (nObjType == 0)
			{
				// object type is unknown, determine if it is a font
				HFONT hStockFont = (HFONT)::GetStockObject(SYSTEM_FONT);
				HFONT hFontOld = (HFONT)::SelectObject(pDC->m_hDC, hStockFont);
				HGDIOBJ hObjOld = ::SelectObject(pDC->m_hDC, hObject);
				if (hObjOld == hStockFont)
				{
					// got the stock object back, so must be selecting a font
					pDC->SelectObject(CFont::FromHandle((HFONT)hObject));
					break;  // don't play the default record
				}
				else
				{
					// didn't get the stock object back, so restore everything
					::SelectObject(pDC->m_hDC, hFontOld);
					::SelectObject(pDC->m_hDC, hObjOld);
				}
				// and fall through to PlayMetaFileRecord...
			}
			else if (nObjType == OBJ_FONT)
			{
				// play back as CDC::SelectObject(CFont*)
				pDC->SelectObject(CFont::FromHandle((HFONT)hObject));
				break;  // don't play the default record
			}
		}
		// fall through...

	default:
		::PlayMetaFileRecord(hDC, pHandleTable, pMetaRec, nHandles);
		break;
	}

	return 1;
}

BOOL CDC::PlayMetaFile(HMETAFILE hMF)
{
	if (::GetDeviceCaps(m_hDC, TECHNOLOGY) == DT_METAFILE)
	{
		// playing metafile in metafile, just use core windows API
		return ::PlayMetaFile(m_hDC, hMF);
	}

	// for special playback, lParam == pDC
	return ::EnumMetaFile(m_hDC, hMF, AfxEnumMetaFileProc, (LPARAM)this);
}

/////////////////////////////////////////////////////////////////////////////
// Coordinate transforms

void CDC::LPtoDP(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	CSize sizeWinExt = GetWindowExt();
	CSize sizeVpExt = GetViewportExt();
	lpSize->cx = MulDiv(lpSize->cx, abs(sizeVpExt.cx), abs(sizeWinExt.cx));
	lpSize->cy = MulDiv(lpSize->cy, abs(sizeVpExt.cy), abs(sizeWinExt.cy));
}

void CDC::DPtoLP(LPSIZE lpSize) const
{
	ASSERT(AfxIsValidAddress(lpSize, sizeof(SIZE)));

	CSize sizeWinExt = GetWindowExt();
	CSize sizeVpExt = GetViewportExt();
	lpSize->cx = MulDiv(lpSize->cx, abs(sizeWinExt.cx), abs(sizeVpExt.cx));
	lpSize->cy = MulDiv(lpSize->cy, abs(sizeWinExt.cy), abs(sizeVpExt.cy));
}

/////////////////////////////////////////////////////////////////////////////
// Helper DCs

#ifdef _DEBUG
void CClientDC::AssertValid() const
{
	CDC::AssertValid();
	ASSERT(m_hWnd == NULL || ::IsWindow(m_hWnd));
}

void CClientDC::Dump(CDumpContext& dc) const
{
	CDC::Dump(dc);

	dc << "m_hWnd = " << (UINT)m_hWnd;
	dc << "\n";
}
#endif

CClientDC::CClientDC(CWnd* pWnd)
{
	ASSERT(pWnd == NULL || ::IsWindow(pWnd->m_hWnd));

	if (!Attach(::GetDC(m_hWnd = pWnd->GetSafeHwnd())))
		AfxThrowResourceException();
}

CClientDC::~CClientDC()
{
	ASSERT(m_hDC != NULL);
	::ReleaseDC(m_hWnd, Detach());
}

#ifdef _DEBUG
void CWindowDC::AssertValid() const
{
	CDC::AssertValid();
	ASSERT(m_hWnd == NULL || ::IsWindow(m_hWnd));
}

void CWindowDC::Dump(CDumpContext& dc) const
{
	CDC::Dump(dc);

	dc << "m_hWnd = " << (UINT)m_hWnd;
	dc << "\n";
}
#endif

CWindowDC::CWindowDC(CWnd* pWnd)
{
	ASSERT(pWnd == NULL || ::IsWindow(pWnd->m_hWnd));

	if (!Attach(::GetWindowDC(m_hWnd = pWnd->GetSafeHwnd())))
		AfxThrowResourceException();
}

CWindowDC::~CWindowDC()
{
	ASSERT(m_hDC != NULL);
	::ReleaseDC(m_hWnd, Detach());
}

#ifdef _DEBUG
void CPaintDC::AssertValid() const
{
	CDC::AssertValid();
	ASSERT(::IsWindow(m_hWnd));
}

void CPaintDC::Dump(CDumpContext& dc) const
{
	CDC::Dump(dc);

	dc << "m_hWnd = " << (UINT)m_hWnd;
	dc << "\nm_ps.hdc = " << (UINT)m_ps.hdc;
	dc << "\nm_ps.fErase = " << m_ps.fErase;
	dc << "\nm_ps.rcPaint = " << (CRect)m_ps.rcPaint;

	dc << "\n";
}
#endif

CPaintDC::CPaintDC(CWnd* pWnd)
{
	ASSERT_VALID(pWnd);
	ASSERT(::IsWindow(pWnd->m_hWnd));

	if (!Attach(::BeginPaint(m_hWnd = pWnd->m_hWnd, &m_ps)))
		AfxThrowResourceException();
}

CPaintDC::~CPaintDC()
{
	ASSERT(m_hDC != NULL);
	ASSERT(::IsWindow(m_hWnd));

	::EndPaint(m_hWnd, &m_ps);
	Detach();
}

/////////////////////////////////////////////////////////////////////////////
// CGdiObject

#ifdef _DEBUG
void CGdiObject::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_hObject = " << (UINT)m_hObject;
	dc << "\n";
}

void CGdiObject::AssertValid() const
{
	CObject::AssertValid();
	ASSERT(m_hObject == NULL ||
		(afxData.bWin95 || ::GetObjectType(m_hObject) != 0));
}
#endif

#include "fixalloc.h"

class CTempGdiObject : public CGdiObject
{
	DECLARE_DYNCREATE(CTempGdiObject)
	DECLARE_FIXED_ALLOC(CTempGdiObject);
};

CHandleMap* PASCAL afxMapHGDIOBJ(BOOL bCreate)
{
	AFX_MODULE_THREAD_STATE* pState = AfxGetModuleThreadState();
	if (pState->m_pmapHGDIOBJ == NULL && bCreate)
	{
		BOOL bEnable = AfxEnableMemoryTracking(FALSE);
#ifndef _AFX_PORTABLE
		_PNH pnhOldHandler = AfxSetNewHandler(&AfxCriticalNewHandler);
#endif
		pState->m_pmapHGDIOBJ = new CHandleMap(RUNTIME_CLASS(CTempGdiObject),
			offsetof(CGdiObject, m_hObject));

#ifndef _AFX_PORTABLE
		AfxSetNewHandler(pnhOldHandler);
#endif
		AfxEnableMemoryTracking(bEnable);
	}
	return pState->m_pmapHGDIOBJ;
}

CGdiObject* PASCAL CGdiObject::FromHandle(HGDIOBJ h)
{
	CHandleMap* pMap = afxMapHGDIOBJ(TRUE); //create map if not exist
	ASSERT(pMap != NULL);
	CGdiObject* pObject = (CGdiObject*)pMap->FromHandle(h);
	ASSERT(pObject == NULL || pObject->m_hObject == h);
	return pObject;
}

BOOL CGdiObject::Attach(HGDIOBJ hObject)
{
	ASSERT(m_hObject == NULL);      // only attach once, detach on destroy
	if (hObject == NULL)
		return FALSE;
	CHandleMap* pMap = afxMapHGDIOBJ(TRUE); // create map if not exist
	ASSERT(pMap != NULL);
	pMap->SetPermanent(m_hObject = hObject, this);
	return TRUE;
}

HGDIOBJ CGdiObject::Detach()
{
	HGDIOBJ hObject = m_hObject;
	if (hObject != NULL)
	{
		CHandleMap* pMap = afxMapHGDIOBJ(); // don't create if not exist
		if (pMap != NULL)
			pMap->RemoveHandle(m_hObject);
	}

	m_hObject = NULL;
	return hObject;
}

BOOL CGdiObject::DeleteObject()
{
	if (m_hObject == NULL)
		return FALSE;
	return ::DeleteObject(Detach());
}

/////////////////////////////////////////////////////////////////////////////
// Standard GDI objects

/////////////////////////////////////////////////////////////////////////////
// CPen

CPen::CPen(int nPenStyle, int nWidth, COLORREF crColor)
{
	if (!Attach(::CreatePen(nPenStyle, nWidth, crColor)))
		AfxThrowResourceException();
}

CPen::CPen(int nPenStyle, int nWidth, const LOGBRUSH* pLogBrush,
	int nStyleCount, const DWORD* lpStyle)
{
	if (!Attach(::ExtCreatePen(nPenStyle, nWidth, pLogBrush, nStyleCount,
			lpStyle)))
		AfxThrowResourceException();
}

/////////////////////////////////////////////////////////////////////////////

#ifdef _DEBUG
void CPen::Dump(CDumpContext& dc) const
{
	CGdiObject::Dump(dc);

	if (m_hObject == NULL)
		return;

	if (!afxData.bWin95 && ::GetObjectType(m_hObject) != OBJ_PEN)
	{
		// not a valid object
		dc << "has ILLEGAL HPEN!";
		return;
	}

	LOGPEN lp;
	VERIFY(GetObject(sizeof(lp), &lp));
	dc << "lgpn.lopnStyle = " << lp.lopnStyle;
	dc << "\nlgpn.lopnWidth.x (width) = " << lp.lopnWidth.x;
	dc << "\nlgpn.lopnColor = " << (void*)lp.lopnColor;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
// CBrush

CBrush::CBrush(COLORREF crColor)
{
	if (!Attach(::CreateSolidBrush(crColor)))
		AfxThrowResourceException();
}

CBrush::CBrush(int nIndex, COLORREF crColor)
{
	if (!Attach(::CreateHatchBrush(nIndex, crColor)))
		AfxThrowResourceException();
}

CBrush::CBrush(CBitmap* pBitmap)
{
	ASSERT_VALID(pBitmap);

	if (!Attach(::CreatePatternBrush((HBITMAP)pBitmap->m_hObject)))
		AfxThrowResourceException();
}

BOOL CBrush::CreateDIBPatternBrush(HGLOBAL hPackedDIB, UINT nUsage)
{
	ASSERT(hPackedDIB != NULL);
	const void* lpPackedDIB = ::GlobalLock(hPackedDIB);
	ASSERT(lpPackedDIB != NULL);
	BOOL bResult = Attach(::CreateDIBPatternBrushPt(lpPackedDIB, nUsage));
	::GlobalUnlock(hPackedDIB);
	return bResult;
}

#ifdef _DEBUG
void CBrush::Dump(CDumpContext& dc) const
{
	CGdiObject::Dump(dc);

	if (m_hObject == NULL)
		return;

	if (!afxData.bWin95 && ::GetObjectType(m_hObject) != OBJ_BRUSH)
	{
		// not a valid window
		dc << "has ILLEGAL HBRUSH!";
		return;
	}

	LOGBRUSH lb;
	VERIFY(GetObject(sizeof(lb), &lb));
	dc << "lb.lbStyle = " << lb.lbStyle;
	dc << "\nlb.lbHatch = " << lb.lbHatch;
	dc << "\nlb.lbColor = " << (void*)lb.lbColor;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _DEBUG
void CFont::Dump(CDumpContext& dc) const
{
	CGdiObject::Dump(dc);

	if (m_hObject == NULL)
		return;

	if (!afxData.bWin95 && ::GetObjectType(m_hObject) != OBJ_FONT)
	{
		// not a valid GDI object
		dc << "has ILLEGAL HFONT!";
		return;
	}

	LOGFONT lf;
	VERIFY(GetObject(sizeof(lf), &lf));
	dc << "lf.lfHeight = " << lf.lfHeight;
	dc << "\nlf.lfWidth = " << lf.lfWidth;
	dc << "\nlf.lfEscapement = " << lf.lfEscapement;
	dc << "\nlf.lfOrientation = " << lf.lfOrientation;
	dc << "\nlf.lfWeight = " << lf.lfWeight;
	dc << "\nlf.lfItalic = " << (int)lf.lfItalic;
	dc << "\nlf.lfUnderline = " << (int)lf.lfUnderline;
	dc << "\nlf.lfStrikeOut = " << (int)lf.lfStrikeOut;
	dc << "\nlf.lfCharSet = " << (int)lf.lfCharSet;
	dc << "\nlf.lfOutPrecision = " << (int)lf.lfOutPrecision;
	dc << "\nlf.lfClipPrecision = " << (int)lf.lfClipPrecision;
	dc << "\nlf.lfQuality = " << (int)lf.lfQuality;
	dc << "\nlf.lfPitchAndFamily = " << (int)lf.lfPitchAndFamily;
	dc << "\nlf.lfFaceName = " << (LPCTSTR)lf.lfFaceName;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _DEBUG
void CBitmap::Dump(CDumpContext& dc) const
{
	CGdiObject::Dump(dc);

	if (m_hObject == NULL)
		return;

	if (!afxData.bWin95 && ::GetObjectType(m_hObject) != OBJ_BITMAP)
	{
		// not a valid object
		dc << "has ILLEGAL HBITMAP!";
		return;
	}

	BITMAP bm;
	VERIFY(GetObject(sizeof(bm), &bm));
	dc << "bm.bmType = " << bm.bmType;
	dc << "\nbm.bmHeight = " << bm.bmHeight;
	dc << "\nbm.bmWidth = " << bm.bmWidth;
	dc << "\nbm.bmWidthBytes = " << bm.bmWidthBytes;
	dc << "\nbm.bmPlanes = " << bm.bmPlanes;
	dc << "\nbm.bmBitsPixel = " << bm.bmBitsPixel;

	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CResourceException, CException)
CResourceException _simpleResourceException(FALSE, AFX_IDS_RESOURCE_EXCEPTION);

IMPLEMENT_DYNAMIC(CUserException, CException)
CUserException _simpleUserException(FALSE, AFX_IDS_USER_EXCEPTION);

IMPLEMENT_DYNCREATE(CDC, CObject)
IMPLEMENT_DYNAMIC(CClientDC, CDC)
IMPLEMENT_DYNAMIC(CWindowDC, CDC)
IMPLEMENT_DYNAMIC(CPaintDC, CDC)
IMPLEMENT_DYNCREATE(CGdiObject, CObject)

IMPLEMENT_DYNAMIC(CPen, CGdiObject)
IMPLEMENT_DYNAMIC(CBrush, CGdiObject)
IMPLEMENT_DYNAMIC(CFont, CGdiObject)
IMPLEMENT_DYNAMIC(CBitmap, CGdiObject)
IMPLEMENT_DYNAMIC(CPalette, CGdiObject)
IMPLEMENT_DYNAMIC(CRgn, CGdiObject)

IMPLEMENT_DYNCREATE(CTempDC, CDC);

IMPLEMENT_DYNCREATE(CTempGdiObject, CGdiObject);

/////////////////////////////////////////////////////////////////////////////
// Standard exception processing

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

// resource failure
void AFXAPI AfxThrowResourceException()
{
	THROW((CResourceException*)&_simpleResourceException);
}

// user alert
void AFXAPI AfxThrowUserException()
{
	THROW((CUserException*)&_simpleUserException);
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#pragma warning(disable: 4074)
#pragma init_seg(compiler)
IMPLEMENT_FIXED_ALLOC(CTempDC, 64);
IMPLEMENT_FIXED_ALLOC(CTempGdiObject, 64);

/////////////////////////////////////////////////////////////////////////////
