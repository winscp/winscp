// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXWIN.H (part 1)

#ifdef _AFXWIN_INLINE

// Global helper functions
_AFXWIN_INLINE CWinApp* AFXAPI AfxGetApp()
	{ return afxCurrentWinApp; }
_AFXWIN_INLINE HINSTANCE AFXAPI AfxGetInstanceHandle()
	{ ASSERT(afxCurrentInstanceHandle != NULL);
		return afxCurrentInstanceHandle; }
_AFXWIN_INLINE HINSTANCE AFXAPI AfxGetResourceHandle()
	{ ASSERT(afxCurrentResourceHandle != NULL);
		return afxCurrentResourceHandle; }
_AFXWIN_INLINE void AFXAPI AfxSetResourceHandle(HINSTANCE hInstResource)
	{ ASSERT(hInstResource != NULL); afxCurrentResourceHandle = hInstResource; }
_AFXWIN_INLINE LPCTSTR AFXAPI AfxGetAppName()
	{ ASSERT(afxCurrentAppName != NULL); return afxCurrentAppName; }
_AFXWIN_INLINE COleMessageFilter* AFXAPI AfxOleGetMessageFilter()
	{ ASSERT_VALID(AfxGetThread()); return AfxGetThread()->m_pMessageFilter; }
_AFXWIN_INLINE CWnd* AFXAPI AfxGetMainWnd()
	{ CWinThread* pThread = AfxGetThread();
		return pThread != NULL ? pThread->GetMainWnd() : NULL; }

#ifdef _AFXDLL
// AFX_MAINTAIN_STATE functions
_AFXWIN_INLINE AFX_MAINTAIN_STATE::AFX_MAINTAIN_STATE(AFX_MODULE_STATE* pNewState)
	{  m_pPrevModuleState = AfxSetModuleState(pNewState); }

// AFX_MAINTAIN_STATE2 functions
_AFXWIN_INLINE AFX_MAINTAIN_STATE2::~AFX_MAINTAIN_STATE2()
	{  m_pThreadState->m_pModuleState = m_pPrevModuleState; }
#endif

// CSize
_AFXWIN_INLINE CSize::CSize()
	{ /* random filled */ }
_AFXWIN_INLINE CSize::CSize(int initCX, int initCY)
	{ cx = initCX; cy = initCY; }
_AFXWIN_INLINE CSize::CSize(SIZE initSize)
	{ *(SIZE*)this = initSize; }
_AFXWIN_INLINE CSize::CSize(POINT initPt)
	{ *(POINT*)this = initPt; }
_AFXWIN_INLINE CSize::CSize(DWORD dwSize)
	{
		cx = (short)LOWORD(dwSize);
		cy = (short)HIWORD(dwSize);
	}
_AFXWIN_INLINE BOOL CSize::operator==(SIZE size) const
	{ return (cx == size.cx && cy == size.cy); }
_AFXWIN_INLINE BOOL CSize::operator!=(SIZE size) const
	{ return (cx != size.cx || cy != size.cy); }
_AFXWIN_INLINE void CSize::operator+=(SIZE size)
	{ cx += size.cx; cy += size.cy; }
_AFXWIN_INLINE void CSize::operator-=(SIZE size)
	{ cx -= size.cx; cy -= size.cy; }
_AFXWIN_INLINE CSize CSize::operator+(SIZE size) const
	{ return CSize(cx + size.cx, cy + size.cy); }
_AFXWIN_INLINE CSize CSize::operator-(SIZE size) const
	{ return CSize(cx - size.cx, cy - size.cy); }
_AFXWIN_INLINE CSize CSize::operator-() const
	{ return CSize(-cx, -cy); }
_AFXWIN_INLINE CPoint CSize::operator+(POINT point) const
	{ return CPoint(cx + point.x, cy + point.y); }
_AFXWIN_INLINE CPoint CSize::operator-(POINT point) const
	{ return CPoint(cx - point.x, cy - point.y); }
_AFXWIN_INLINE CRect CSize::operator+(const RECT* lpRect) const
	{ return CRect(lpRect) + *this; }
_AFXWIN_INLINE CRect CSize::operator-(const RECT* lpRect) const
	{ return CRect(lpRect) - *this; }

// CPoint
_AFXWIN_INLINE CPoint::CPoint()
	{ /* random filled */ }
_AFXWIN_INLINE CPoint::CPoint(int initX, int initY)
	{ x = initX; y = initY; }
#if !defined(_AFX_CORE_IMPL) || !defined(_AFXDLL) || defined(_DEBUG)
_AFXWIN_INLINE CPoint::CPoint(POINT initPt)
	{ *(POINT*)this = initPt; }
#endif
_AFXWIN_INLINE CPoint::CPoint(SIZE initSize)
	{ *(SIZE*)this = initSize; }
_AFXWIN_INLINE CPoint::CPoint(DWORD dwPoint)
	{
		x = (short)LOWORD(dwPoint);
		y = (short)HIWORD(dwPoint);
	}
_AFXWIN_INLINE void CPoint::Offset(int xOffset, int yOffset)
	{ x += xOffset; y += yOffset; }
_AFXWIN_INLINE void CPoint::Offset(POINT point)
	{ x += point.x; y += point.y; }
_AFXWIN_INLINE void CPoint::Offset(SIZE size)
	{ x += size.cx; y += size.cy; }
_AFXWIN_INLINE BOOL CPoint::operator==(POINT point) const
	{ return (x == point.x && y == point.y); }
_AFXWIN_INLINE BOOL CPoint::operator!=(POINT point) const
	{ return (x != point.x || y != point.y); }
_AFXWIN_INLINE void CPoint::operator+=(SIZE size)
	{ x += size.cx; y += size.cy; }
_AFXWIN_INLINE void CPoint::operator-=(SIZE size)
	{ x -= size.cx; y -= size.cy; }
_AFXWIN_INLINE void CPoint::operator+=(POINT point)
	{ x += point.x; y += point.y; }
_AFXWIN_INLINE void CPoint::operator-=(POINT point)
	{ x -= point.x; y -= point.y; }
_AFXWIN_INLINE CPoint CPoint::operator+(SIZE size) const
	{ return CPoint(x + size.cx, y + size.cy); }
_AFXWIN_INLINE CPoint CPoint::operator-(SIZE size) const
	{ return CPoint(x - size.cx, y - size.cy); }
_AFXWIN_INLINE CPoint CPoint::operator-() const
	{ return CPoint(-x, -y); }
_AFXWIN_INLINE CPoint CPoint::operator+(POINT point) const
	{ return CPoint(x + point.x, y + point.y); }
_AFXWIN_INLINE CSize CPoint::operator-(POINT point) const
	{ return CSize(x - point.x, y - point.y); }
_AFXWIN_INLINE CRect CPoint::operator+(const RECT* lpRect) const
	{ return CRect(lpRect) + *this; }
_AFXWIN_INLINE CRect CPoint::operator-(const RECT* lpRect) const
	{ return CRect(lpRect) - *this; }

// CRect
_AFXWIN_INLINE CRect::CRect()
	{ /* random filled */ }
_AFXWIN_INLINE CRect::CRect(int l, int t, int r, int b)
	{ left = l; top = t; right = r; bottom = b; }
_AFXWIN_INLINE CRect::CRect(const RECT& srcRect)
	{ ::CopyRect(this, &srcRect); }
_AFXWIN_INLINE CRect::CRect(LPCRECT lpSrcRect)
	{ ::CopyRect(this, lpSrcRect); }
_AFXWIN_INLINE CRect::CRect(POINT point, SIZE size)
	{ right = (left = point.x) + size.cx; bottom = (top = point.y) + size.cy; }
_AFXWIN_INLINE CRect::CRect(POINT topLeft, POINT bottomRight)
	{ left = topLeft.x; top = topLeft.y;
		right = bottomRight.x; bottom = bottomRight.y; }
_AFXWIN_INLINE int CRect::Width() const
	{ return right - left; }
_AFXWIN_INLINE int CRect::Height() const
	{ return bottom - top; }
_AFXWIN_INLINE CSize CRect::Size() const
	{ return CSize(right - left, bottom - top); }
_AFXWIN_INLINE CPoint& CRect::TopLeft()
	{ return *((CPoint*)this); }
_AFXWIN_INLINE CPoint& CRect::BottomRight()
	{ return *((CPoint*)this+1); }
_AFXWIN_INLINE const CPoint& CRect::TopLeft() const
	{ return *((CPoint*)this); }
_AFXWIN_INLINE const CPoint& CRect::BottomRight() const
	{ return *((CPoint*)this+1); }
_AFXWIN_INLINE CPoint CRect::CenterPoint() const
	{ return CPoint((left+right)/2, (top+bottom)/2); }
_AFXWIN_INLINE void CRect::SwapLeftRight()
	{ SwapLeftRight(LPRECT(this)); }
_AFXWIN_INLINE void CRect::SwapLeftRight(LPRECT lpRect)
	{ LONG temp = lpRect->left; lpRect->left = lpRect->right; lpRect->right = temp; }
_AFXWIN_INLINE CRect::operator LPRECT()
	{ return this; }
_AFXWIN_INLINE CRect::operator LPCRECT() const
	{ return this; }
_AFXWIN_INLINE BOOL CRect::IsRectEmpty() const
	{ return ::IsRectEmpty(this); }
_AFXWIN_INLINE BOOL CRect::IsRectNull() const
	{ return (left == 0 && right == 0 && top == 0 && bottom == 0); }
_AFXWIN_INLINE BOOL CRect::PtInRect(POINT point) const
	{ return ::PtInRect(this, point); }
_AFXWIN_INLINE void CRect::SetRect(int x1, int y1, int x2, int y2)
	{ ::SetRect(this, x1, y1, x2, y2); }
_AFXWIN_INLINE void CRect::SetRect(POINT topLeft, POINT bottomRight)
	{ ::SetRect(this, topLeft.x, topLeft.y, bottomRight.x, bottomRight.y); }
_AFXWIN_INLINE void CRect::SetRectEmpty()
	{ ::SetRectEmpty(this); }
_AFXWIN_INLINE void CRect::CopyRect(LPCRECT lpSrcRect)
	{ ::CopyRect(this, lpSrcRect); }
_AFXWIN_INLINE BOOL CRect::EqualRect(LPCRECT lpRect) const
	{ return ::EqualRect(this, lpRect); }
_AFXWIN_INLINE void CRect::InflateRect(int x, int y)
	{ ::InflateRect(this, x, y); }
_AFXWIN_INLINE void CRect::InflateRect(SIZE size)
	{ ::InflateRect(this, size.cx, size.cy); }
_AFXWIN_INLINE void CRect::DeflateRect(int x, int y)
	{ ::InflateRect(this, -x, -y); }
_AFXWIN_INLINE void CRect::DeflateRect(SIZE size)
	{ ::InflateRect(this, -size.cx, -size.cy); }
_AFXWIN_INLINE void CRect::OffsetRect(int x, int y)
	{ ::OffsetRect(this, x, y); }
_AFXWIN_INLINE void CRect::OffsetRect(POINT point)
	{ ::OffsetRect(this, point.x, point.y); }
_AFXWIN_INLINE void CRect::OffsetRect(SIZE size)
	{ ::OffsetRect(this, size.cx, size.cy); }
_AFXWIN_INLINE BOOL CRect::IntersectRect(LPCRECT lpRect1, LPCRECT lpRect2)
	{ return ::IntersectRect(this, lpRect1, lpRect2);}
_AFXWIN_INLINE BOOL CRect::UnionRect(LPCRECT lpRect1, LPCRECT lpRect2)
	{ return ::UnionRect(this, lpRect1, lpRect2); }
_AFXWIN_INLINE void CRect::operator=(const RECT& srcRect)
	{ ::CopyRect(this, &srcRect); }
_AFXWIN_INLINE BOOL CRect::operator==(const RECT& rect) const
	{ return ::EqualRect(this, &rect); }
_AFXWIN_INLINE BOOL CRect::operator!=(const RECT& rect) const
	{ return !::EqualRect(this, &rect); }
_AFXWIN_INLINE void CRect::operator+=(POINT point)
	{ ::OffsetRect(this, point.x, point.y); }
_AFXWIN_INLINE void CRect::operator+=(SIZE size)
	{ ::OffsetRect(this, size.cx, size.cy); }
_AFXWIN_INLINE void CRect::operator+=(LPCRECT lpRect)
	{ InflateRect(lpRect); }
_AFXWIN_INLINE void CRect::operator-=(POINT point)
	{ ::OffsetRect(this, -point.x, -point.y); }
_AFXWIN_INLINE void CRect::operator-=(SIZE size)
	{ ::OffsetRect(this, -size.cx, -size.cy); }
_AFXWIN_INLINE void CRect::operator-=(LPCRECT lpRect)
	{ DeflateRect(lpRect); }
_AFXWIN_INLINE void CRect::operator&=(const RECT& rect)
	{ ::IntersectRect(this, this, &rect); }
_AFXWIN_INLINE void CRect::operator|=(const RECT& rect)
	{ ::UnionRect(this, this, &rect); }
_AFXWIN_INLINE CRect CRect::operator+(POINT pt) const
	{ CRect rect(*this); ::OffsetRect(&rect, pt.x, pt.y); return rect; }
_AFXWIN_INLINE CRect CRect::operator-(POINT pt) const
	{ CRect rect(*this); ::OffsetRect(&rect, -pt.x, -pt.y); return rect; }
_AFXWIN_INLINE CRect CRect::operator+(SIZE size) const
	{ CRect rect(*this); ::OffsetRect(&rect, size.cx, size.cy); return rect; }
_AFXWIN_INLINE CRect CRect::operator-(SIZE size) const
	{ CRect rect(*this); ::OffsetRect(&rect, -size.cx, -size.cy); return rect; }
_AFXWIN_INLINE CRect CRect::operator+(LPCRECT lpRect) const
	{ CRect rect(this); rect.InflateRect(lpRect); return rect; }
_AFXWIN_INLINE CRect CRect::operator-(LPCRECT lpRect) const
	{ CRect rect(this); rect.DeflateRect(lpRect); return rect; }
_AFXWIN_INLINE CRect CRect::operator&(const RECT& rect2) const
	{ CRect rect; ::IntersectRect(&rect, this, &rect2);
		return rect; }
_AFXWIN_INLINE CRect CRect::operator|(const RECT& rect2) const
	{ CRect rect; ::UnionRect(&rect, this, &rect2);
		return rect; }
_AFXWIN_INLINE BOOL CRect::SubtractRect(LPCRECT lpRectSrc1, LPCRECT lpRectSrc2)
	{ return ::SubtractRect(this, lpRectSrc1, lpRectSrc2); }

// CArchive output helpers
_AFXWIN_INLINE CArchive& AFXAPI operator<<(CArchive& ar, SIZE size)
	{ ar.Write(&size, sizeof(SIZE)); return ar; }
_AFXWIN_INLINE CArchive& AFXAPI operator<<(CArchive& ar, POINT point)
	{ ar.Write(&point, sizeof(POINT)); return ar; }
_AFXWIN_INLINE CArchive& AFXAPI operator<<(CArchive& ar, const RECT& rect)
	{ ar.Write(&rect, sizeof(RECT)); return ar; }
_AFXWIN_INLINE CArchive& AFXAPI operator>>(CArchive& ar, SIZE& size)
	{ ar.Read(&size, sizeof(SIZE)); return ar; }
_AFXWIN_INLINE CArchive& AFXAPI operator>>(CArchive& ar, POINT& point)
	{ ar.Read(&point, sizeof(POINT)); return ar; }
_AFXWIN_INLINE CArchive& AFXAPI operator>>(CArchive& ar, RECT& rect)
	{ ar.Read(&rect, sizeof(RECT)); return ar; }

// exception support
_AFXWIN_INLINE CResourceException::CResourceException()
	: CSimpleException() { }
_AFXWIN_INLINE CResourceException::CResourceException(BOOL bAutoDelete, UINT nResourceID)
	: CSimpleException(bAutoDelete) { m_nResourceID = nResourceID; }
_AFXWIN_INLINE CResourceException::~CResourceException()
	{ }
_AFXWIN_INLINE CUserException::CUserException()
	: CSimpleException() { }
_AFXWIN_INLINE CUserException::CUserException(BOOL bAutoDelete, UINT nResourceID)
	: CSimpleException(bAutoDelete) { m_nResourceID = nResourceID; }
_AFXWIN_INLINE CUserException::~CUserException()
	{ }

// CGdiObject
_AFXWIN_INLINE CGdiObject::operator HGDIOBJ() const
	{ return this == NULL ? NULL : m_hObject; }
_AFXWIN_INLINE HGDIOBJ CGdiObject::GetSafeHandle() const
	{ return this == NULL ? NULL : m_hObject; }
_AFXWIN_INLINE CGdiObject::CGdiObject()
	{ m_hObject = NULL; }
_AFXWIN_INLINE CGdiObject::~CGdiObject()
	{ DeleteObject(); }
_AFXWIN_INLINE int CGdiObject::GetObject(int nCount, LPVOID lpObject) const
	{ ASSERT(m_hObject != NULL); return ::GetObject(m_hObject, nCount, lpObject); }
_AFXWIN_INLINE BOOL CGdiObject::CreateStockObject(int nIndex)
	{ return (m_hObject = ::GetStockObject(nIndex)) != NULL; }
_AFXWIN_INLINE BOOL CGdiObject::UnrealizeObject()
	{ ASSERT(m_hObject != NULL); return ::UnrealizeObject(m_hObject); }
_AFXWIN_INLINE UINT CGdiObject::GetObjectType() const
	{ return (UINT)::GetObjectType(m_hObject); }
_AFXWIN_INLINE BOOL CGdiObject::operator==(const CGdiObject& obj) const
	{ return ((HGDIOBJ) obj) == m_hObject; }
_AFXWIN_INLINE BOOL CGdiObject::operator!=(const CGdiObject& obj) const
	{ return ((HGDIOBJ) obj) != m_hObject; }

// CPen
_AFXWIN_INLINE CPen::operator HPEN() const
	{ return (HPEN)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CPen* PASCAL CPen::FromHandle(HPEN hPen)
	{ return (CPen*) CGdiObject::FromHandle(hPen); }
_AFXWIN_INLINE CPen::CPen()
	{ }
_AFXWIN_INLINE CPen::~CPen()
	{ }
_AFXWIN_INLINE BOOL CPen::CreatePen(int nPenStyle, int nWidth, COLORREF crColor)
	{ return Attach(::CreatePen(nPenStyle, nWidth, crColor)); }
_AFXWIN_INLINE BOOL CPen::CreatePenIndirect(LPLOGPEN lpLogPen)
	{ return Attach(::CreatePenIndirect(lpLogPen)); }
_AFXWIN_INLINE BOOL CPen::CreatePen(int nPenStyle, int nWidth, const LOGBRUSH* pLogBrush,
		int nStyleCount, const DWORD* lpStyle)
	{ return Attach(::ExtCreatePen(nPenStyle, nWidth, pLogBrush, nStyleCount,
		lpStyle)); }
_AFXWIN_INLINE int CPen::GetExtLogPen(EXTLOGPEN* pLogPen)
	{ ASSERT(m_hObject != NULL);
		return ::GetObject(m_hObject, sizeof(EXTLOGPEN), pLogPen); }
_AFXWIN_INLINE int CPen::GetLogPen(LOGPEN* pLogPen)
	{ ASSERT(m_hObject != NULL);
		return ::GetObject(m_hObject, sizeof(LOGPEN), pLogPen); }

// CBrush
_AFXWIN_INLINE CBrush::operator HBRUSH() const
	{ return (HBRUSH)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CBrush* PASCAL CBrush::FromHandle(HBRUSH hBrush)
	{ return (CBrush*) CGdiObject::FromHandle(hBrush); }
_AFXWIN_INLINE CBrush::CBrush()
	{ }
_AFXWIN_INLINE CBrush::~CBrush()
	{ }
_AFXWIN_INLINE BOOL CBrush::CreateSolidBrush(COLORREF crColor)
	{ return Attach(::CreateSolidBrush(crColor)); }
_AFXWIN_INLINE BOOL CBrush::CreateHatchBrush(int nIndex, COLORREF crColor)
	{ return Attach(::CreateHatchBrush(nIndex, crColor)); }
_AFXWIN_INLINE BOOL CBrush::CreateBrushIndirect(const LOGBRUSH* lpLogBrush)
	{ return Attach(::CreateBrushIndirect(lpLogBrush)); }
_AFXWIN_INLINE BOOL CBrush::CreatePatternBrush(CBitmap* pBitmap)
	{ return Attach(::CreatePatternBrush((HBITMAP)pBitmap->GetSafeHandle())); }
_AFXWIN_INLINE BOOL CBrush::CreateDIBPatternBrush(const void* lpPackedDIB, UINT nUsage)
	{ return Attach(::CreateDIBPatternBrushPt(lpPackedDIB, nUsage)); }
_AFXWIN_INLINE BOOL CBrush::CreateSysColorBrush(int nIndex)
	{ return Attach(::GetSysColorBrush(nIndex)); }
_AFXWIN_INLINE int CBrush::GetLogBrush(LOGBRUSH* pLogBrush)
	{ ASSERT(m_hObject != NULL);
		return ::GetObject(m_hObject, sizeof(LOGBRUSH), pLogBrush); }

// CFont
_AFXWIN_INLINE CFont::operator HFONT() const
	{ return (HFONT)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CFont* PASCAL CFont::FromHandle(HFONT hFont)
	{ return (CFont*) CGdiObject::FromHandle(hFont); }
_AFXWIN_INLINE CFont::CFont()
	{ }
_AFXWIN_INLINE CFont::~CFont()
	{ }
_AFXWIN_INLINE BOOL CFont::CreateFontIndirect(const LOGFONT* lpLogFont)
	{ return Attach(::CreateFontIndirect(lpLogFont)); }
_AFXWIN_INLINE BOOL CFont::CreateFont(int nHeight, int nWidth, int nEscapement,
		int nOrientation, int nWeight, BYTE bItalic, BYTE bUnderline,
		BYTE cStrikeOut, BYTE nCharSet, BYTE nOutPrecision,
		BYTE nClipPrecision, BYTE nQuality, BYTE nPitchAndFamily,
		LPCTSTR lpszFacename)
	{ return Attach(::CreateFont(nHeight, nWidth, nEscapement,
		nOrientation, nWeight, bItalic, bUnderline, cStrikeOut,
		nCharSet, nOutPrecision, nClipPrecision, nQuality,
		nPitchAndFamily, lpszFacename)); }
_AFXWIN_INLINE int CFont::GetLogFont(LOGFONT* pLogFont)
	{ ASSERT(m_hObject != NULL);
		return ::GetObject(m_hObject, sizeof(LOGFONT), pLogFont); }

// CBitmap
_AFXWIN_INLINE CBitmap::operator HBITMAP() const
	{ return (HBITMAP)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CBitmap* PASCAL CBitmap::FromHandle(HBITMAP hBitmap)
	{ return (CBitmap*) CGdiObject::FromHandle(hBitmap); }
_AFXWIN_INLINE CBitmap::CBitmap()
	{ }
_AFXWIN_INLINE CBitmap::~CBitmap()
	{ }
_AFXWIN_INLINE BOOL CBitmap::CreateBitmap(int nWidth, int nHeight, UINT nPlanes,
	 UINT nBitcount, const void* lpBits)
	{ return Attach(::CreateBitmap(nWidth, nHeight, nPlanes, nBitcount, lpBits)); }
_AFXWIN_INLINE BOOL CBitmap::CreateBitmapIndirect(LPBITMAP lpBitmap)
	{ return Attach(::CreateBitmapIndirect(lpBitmap)); }

_AFXWIN_INLINE DWORD CBitmap::SetBitmapBits(DWORD dwCount, const void* lpBits)
	{ return ::SetBitmapBits((HBITMAP)m_hObject, dwCount, lpBits); }
_AFXWIN_INLINE DWORD CBitmap::GetBitmapBits(DWORD dwCount, LPVOID lpBits) const
	{ return ::GetBitmapBits((HBITMAP)m_hObject, dwCount, lpBits); }
_AFXWIN_INLINE BOOL CBitmap::LoadBitmap(LPCTSTR lpszResourceName)
	{ return Attach(::LoadBitmap(AfxFindResourceHandle(
		lpszResourceName, RT_BITMAP), lpszResourceName));}
_AFXWIN_INLINE BOOL CBitmap::LoadMappedBitmap(UINT nIDBitmap, UINT nFlags,
	LPCOLORMAP lpColorMap, int nMapSize)
	{ return Attach(::CreateMappedBitmap(AfxFindResourceHandle(
		MAKEINTRESOURCE(nIDBitmap), RT_BITMAP), nIDBitmap, (WORD)nFlags,
		lpColorMap, nMapSize)); }
_AFXWIN_INLINE CSize CBitmap::SetBitmapDimension(int nWidth, int nHeight)
	{
		SIZE size;
		VERIFY(::SetBitmapDimensionEx((HBITMAP)m_hObject, nWidth, nHeight, &size));
		return size;
	}
_AFXWIN_INLINE CSize CBitmap::GetBitmapDimension() const
	{
		SIZE size;
		VERIFY(::GetBitmapDimensionEx((HBITMAP)m_hObject, &size));
		return size;
	}

_AFXWIN_INLINE BOOL CBitmap::LoadBitmap(UINT nIDResource)
	{ return Attach(::LoadBitmap(AfxFindResourceHandle(
		MAKEINTRESOURCE(nIDResource), RT_BITMAP), MAKEINTRESOURCE(nIDResource))); }
_AFXWIN_INLINE BOOL CBitmap::LoadOEMBitmap(UINT nIDBitmap)
	{ return Attach(::LoadBitmap(NULL, MAKEINTRESOURCE(nIDBitmap))); }
_AFXWIN_INLINE BOOL CBitmap::CreateCompatibleBitmap(CDC* pDC, int nWidth, int nHeight)
	{ return Attach(::CreateCompatibleBitmap(pDC->m_hDC, nWidth, nHeight)); }
_AFXWIN_INLINE BOOL CBitmap::CreateDiscardableBitmap(CDC* pDC, int nWidth, int nHeight)
	{ return Attach(::CreateDiscardableBitmap(pDC->m_hDC, nWidth, nHeight)); }
_AFXWIN_INLINE int CBitmap::GetBitmap(BITMAP* pBitMap)
	{ ASSERT(m_hObject != NULL);
		return ::GetObject(m_hObject, sizeof(BITMAP), pBitMap); }

// CPalette
_AFXWIN_INLINE CPalette::operator HPALETTE() const
	{ return (HPALETTE)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CPalette* PASCAL CPalette::FromHandle(HPALETTE hPalette)
	{ return (CPalette*) CGdiObject::FromHandle(hPalette); }
_AFXWIN_INLINE CPalette::CPalette()
	{ }
_AFXWIN_INLINE CPalette::~CPalette()
	{ }
_AFXWIN_INLINE BOOL CPalette::CreatePalette(LPLOGPALETTE lpLogPalette)
	{ return Attach(::CreatePalette(lpLogPalette)); }
_AFXWIN_INLINE BOOL CPalette::CreateHalftonePalette(CDC* pDC)
	{ ASSERT(pDC != NULL && pDC->m_hDC != NULL); return Attach(
		::CreateHalftonePalette(pDC->m_hDC)); }
_AFXWIN_INLINE UINT CPalette::GetPaletteEntries(UINT nStartIndex, UINT nNumEntries,
		LPPALETTEENTRY lpPaletteColors) const
	{ ASSERT(m_hObject != NULL); return ::GetPaletteEntries((HPALETTE)m_hObject, nStartIndex,
		nNumEntries, lpPaletteColors); }
_AFXWIN_INLINE UINT CPalette::SetPaletteEntries(UINT nStartIndex, UINT nNumEntries,
		LPPALETTEENTRY lpPaletteColors)
	{ ASSERT(m_hObject != NULL); return ::SetPaletteEntries((HPALETTE)m_hObject, nStartIndex,
		nNumEntries, lpPaletteColors); }
_AFXWIN_INLINE void CPalette::AnimatePalette(UINT nStartIndex, UINT nNumEntries,
		LPPALETTEENTRY lpPaletteColors)
	{ ASSERT(m_hObject != NULL); ::AnimatePalette((HPALETTE)m_hObject, nStartIndex, nNumEntries,
			lpPaletteColors); }
_AFXWIN_INLINE UINT CPalette::GetNearestPaletteIndex(COLORREF crColor) const
	{ ASSERT(m_hObject != NULL); return ::GetNearestPaletteIndex((HPALETTE)m_hObject, crColor); }
_AFXWIN_INLINE BOOL CPalette::ResizePalette(UINT nNumEntries)
	{ ASSERT(m_hObject != NULL); return ::ResizePalette((HPALETTE)m_hObject, nNumEntries); }
_AFXWIN_INLINE int CPalette::GetEntryCount()
	{ ASSERT(m_hObject != NULL); WORD nEntries;
		 ::GetObject(m_hObject, sizeof(WORD), &nEntries); return (int)nEntries; }

// CRgn
_AFXWIN_INLINE CRgn::operator HRGN() const
	{ return (HRGN)(this == NULL ? NULL : m_hObject); }
_AFXWIN_INLINE CRgn* PASCAL CRgn::FromHandle(HRGN hRgn)
	{ return (CRgn*) CGdiObject::FromHandle(hRgn); }
_AFXWIN_INLINE CRgn::CRgn()
	{ }
_AFXWIN_INLINE CRgn::~CRgn()
	{ }
_AFXWIN_INLINE BOOL CRgn::CreateRectRgn(int x1, int y1, int x2, int y2)
	{ return Attach(::CreateRectRgn(x1, y1, x2, y2)); }
_AFXWIN_INLINE BOOL CRgn::CreateRectRgnIndirect(LPCRECT lpRect)
	{ return Attach(::CreateRectRgnIndirect(lpRect)); }
_AFXWIN_INLINE BOOL CRgn::CreateEllipticRgn(int x1, int y1, int x2, int y2)
	{ return Attach(::CreateEllipticRgn(x1, y1, x2, y2)); }
_AFXWIN_INLINE BOOL CRgn::CreateEllipticRgnIndirect(LPCRECT lpRect)
	{ return Attach(::CreateEllipticRgnIndirect(lpRect)); }
_AFXWIN_INLINE BOOL CRgn::CreatePolygonRgn(LPPOINT lpPoints, int nCount, int nMode)
	{ return Attach(::CreatePolygonRgn(lpPoints, nCount, nMode)); }
_AFXWIN_INLINE BOOL CRgn::CreatePolyPolygonRgn(LPPOINT lpPoints, LPINT lpPolyCounts, int nCount, int nPolyFillMode)
	{ return Attach(::CreatePolyPolygonRgn(lpPoints, lpPolyCounts, nCount, nPolyFillMode)); }
_AFXWIN_INLINE BOOL CRgn::CreateRoundRectRgn(int x1, int y1, int x2, int y2, int x3, int y3)
	{ return Attach(::CreateRoundRectRgn(x1, y1, x2, y2, x3, y3)); }
_AFXWIN_INLINE BOOL CRgn::CreateFromPath(CDC* pDC)
	{ ASSERT(pDC != NULL); return Attach(::PathToRegion(pDC->m_hDC)); }
_AFXWIN_INLINE BOOL CRgn::CreateFromData(const XFORM* lpXForm, int nCount, const RGNDATA* pRgnData)
	{ return Attach(::ExtCreateRegion(lpXForm, nCount, pRgnData)); }
_AFXWIN_INLINE int CRgn::GetRegionData(LPRGNDATA lpRgnData, int nDataSize) const
	{ ASSERT(m_hObject != NULL); return (int)::GetRegionData((HRGN)m_hObject, nDataSize, lpRgnData); }
_AFXWIN_INLINE void CRgn::SetRectRgn(int x1, int y1, int x2, int y2)
	{ ASSERT(m_hObject != NULL); ::SetRectRgn((HRGN)m_hObject, x1, y1, x2, y2); }
_AFXWIN_INLINE void CRgn::SetRectRgn(LPCRECT lpRect)
	{ ASSERT(m_hObject != NULL); ::SetRectRgn((HRGN)m_hObject, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom); }
_AFXWIN_INLINE int CRgn::CombineRgn(CRgn* pRgn1, CRgn* pRgn2, int nCombineMode)
	{ ASSERT(m_hObject != NULL); return ::CombineRgn((HRGN)m_hObject, (HRGN)pRgn1->GetSafeHandle(),
		(HRGN)pRgn2->GetSafeHandle(), nCombineMode); }
_AFXWIN_INLINE int CRgn::CopyRgn(CRgn* pRgnSrc)
	{ ASSERT(m_hObject != NULL); return ::CombineRgn((HRGN)m_hObject, (HRGN)pRgnSrc->GetSafeHandle(), NULL, RGN_COPY); }
_AFXWIN_INLINE BOOL CRgn::EqualRgn(CRgn* pRgn) const
	{ ASSERT(m_hObject != NULL); return ::EqualRgn((HRGN)m_hObject, (HRGN)pRgn->GetSafeHandle()); }
_AFXWIN_INLINE int CRgn::OffsetRgn(int x, int y)
	{ ASSERT(m_hObject != NULL); return ::OffsetRgn((HRGN)m_hObject, x, y); }
_AFXWIN_INLINE int CRgn::OffsetRgn(POINT point)
	{ ASSERT(m_hObject != NULL); return ::OffsetRgn((HRGN)m_hObject, point.x, point.y); }
_AFXWIN_INLINE int CRgn::GetRgnBox(LPRECT lpRect) const
	{ ASSERT(m_hObject != NULL); return ::GetRgnBox((HRGN)m_hObject, lpRect); }
_AFXWIN_INLINE BOOL CRgn::PtInRegion(int x, int y) const
	{ ASSERT(m_hObject != NULL); return ::PtInRegion((HRGN)m_hObject, x, y); }
_AFXWIN_INLINE BOOL CRgn::PtInRegion(POINT point) const
	{ ASSERT(m_hObject != NULL); return ::PtInRegion((HRGN)m_hObject, point.x, point.y); }
_AFXWIN_INLINE BOOL CRgn::RectInRegion(LPCRECT lpRect) const
	{ ASSERT(m_hObject != NULL); return ::RectInRegion((HRGN)m_hObject, lpRect); }

// CDC
_AFXWIN_INLINE CDC::operator HDC() const
	{ return this == NULL ? NULL : m_hDC; }
_AFXWIN_INLINE HDC CDC::GetSafeHdc() const
	{ return this == NULL ? NULL : m_hDC; }
_AFXWIN_INLINE CWnd* CDC::GetWindow() const
	{ ASSERT(m_hDC != NULL); return CWnd::FromHandle(::WindowFromDC(m_hDC)); }
_AFXWIN_INLINE BOOL CDC::IsPrinting() const
	{ return m_bPrinting; }
_AFXWIN_INLINE BOOL CDC::CreateDC(LPCTSTR lpszDriverName,
	LPCTSTR lpszDeviceName, LPCTSTR lpszOutput, const void* lpInitData)
	{ return Attach(::CreateDC(lpszDriverName,
		lpszDeviceName, lpszOutput, (const DEVMODE*)lpInitData)); }
_AFXWIN_INLINE BOOL CDC::CreateIC(LPCTSTR lpszDriverName,
	LPCTSTR lpszDeviceName, LPCTSTR lpszOutput, const void* lpInitData)
	{ return Attach(::CreateIC(lpszDriverName,
		lpszDeviceName, lpszOutput, (const DEVMODE*) lpInitData)); }
_AFXWIN_INLINE BOOL CDC::CreateCompatibleDC(CDC* pDC)
	{ return Attach(::CreateCompatibleDC(pDC->GetSafeHdc())); }
_AFXWIN_INLINE int CDC::ExcludeUpdateRgn(CWnd* pWnd)
	{ ASSERT(m_hDC != NULL); return ::ExcludeUpdateRgn(m_hDC, pWnd->m_hWnd); }
_AFXWIN_INLINE int CDC::GetDeviceCaps(int nIndex) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetDeviceCaps(m_hAttribDC, nIndex); }
_AFXWIN_INLINE CPoint CDC::GetBrushOrg() const
	{
		ASSERT(m_hDC != NULL);
		POINT point;
		VERIFY(::GetBrushOrgEx(m_hDC, &point));
		return point;
	}
_AFXWIN_INLINE CPoint CDC::SetBrushOrg(int x, int y)
	{
		ASSERT(m_hDC != NULL);
		POINT point;
		VERIFY(::SetBrushOrgEx(m_hDC, x, y, &point));
		return point;
	}
_AFXWIN_INLINE CPoint CDC::SetBrushOrg(POINT point)
	{
		ASSERT(m_hDC != NULL);
		VERIFY(::SetBrushOrgEx(m_hDC, point.x, point.y, &point));
		return point;
	}

_AFXWIN_INLINE int CDC::EnumObjects(int nObjectType,
		int (CALLBACK* lpfn)(LPVOID, LPARAM), LPARAM lpData)
	{ ASSERT(m_hAttribDC != NULL); return ::EnumObjects(m_hAttribDC, nObjectType, (GOBJENUMPROC)lpfn, lpData); }

_AFXWIN_INLINE CBitmap* CDC::SelectObject(CBitmap* pBitmap)
	{ ASSERT(m_hDC != NULL); return (CBitmap*) SelectGdiObject(m_hDC, pBitmap->GetSafeHandle()); }
_AFXWIN_INLINE CGdiObject* CDC::SelectObject(CGdiObject* pObject)
	{ ASSERT(m_hDC != NULL); return SelectGdiObject(m_hDC, pObject->GetSafeHandle()); }
_AFXWIN_INLINE HGDIOBJ CDC::SelectObject(HGDIOBJ hObject) // Safe for NULL handles
	{ ASSERT(m_hDC == m_hAttribDC); // ASSERT a simple CDC object
		return (hObject != NULL) ? ::SelectObject(m_hDC, hObject) : NULL; }
_AFXWIN_INLINE COLORREF CDC::GetNearestColor(COLORREF crColor) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetNearestColor(m_hAttribDC, crColor); }
_AFXWIN_INLINE UINT CDC::RealizePalette()
	{ ASSERT(m_hDC != NULL); return ::RealizePalette(m_hDC); }
_AFXWIN_INLINE void CDC::UpdateColors()
	{ ASSERT(m_hDC != NULL); ::UpdateColors(m_hDC); }
_AFXWIN_INLINE COLORREF CDC::GetBkColor() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetBkColor(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetBkMode() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetBkMode(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetPolyFillMode() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetPolyFillMode(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetROP2() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetROP2(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetStretchBltMode() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetStretchBltMode(m_hAttribDC); }
_AFXWIN_INLINE COLORREF CDC::GetTextColor() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTextColor(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetMapMode() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetMapMode(m_hAttribDC); }

_AFXWIN_INLINE CPoint CDC::GetViewportOrg() const
	{
		ASSERT(m_hAttribDC != NULL);
		POINT point;
		VERIFY(::GetViewportOrgEx(m_hAttribDC, &point));
		return point;
	}
_AFXWIN_INLINE CSize CDC::GetViewportExt() const
	{
		ASSERT(m_hAttribDC != NULL);
		SIZE size;
		VERIFY(::GetViewportExtEx(m_hAttribDC, &size));
		return size;
	}
_AFXWIN_INLINE CPoint CDC::GetWindowOrg() const
	{
		ASSERT(m_hAttribDC != NULL);
		POINT point;
		VERIFY(::GetWindowOrgEx(m_hAttribDC, &point));
		return point;
	}
_AFXWIN_INLINE CSize CDC::GetWindowExt() const
	{
		ASSERT(m_hAttribDC != NULL);
		SIZE size;
		VERIFY(::GetWindowExtEx(m_hAttribDC, &size));
		return size;
	}

// non-virtual helpers calling virtual mapping functions
_AFXWIN_INLINE CPoint CDC::SetViewportOrg(POINT point)
	{ ASSERT(m_hDC != NULL); return SetViewportOrg(point.x, point.y); }
_AFXWIN_INLINE CSize CDC::SetViewportExt(SIZE size)
	{ ASSERT(m_hDC != NULL); return SetViewportExt(size.cx, size.cy); }
_AFXWIN_INLINE CPoint CDC::SetWindowOrg(POINT point)
	{ ASSERT(m_hDC != NULL); return SetWindowOrg(point.x, point.y); }
_AFXWIN_INLINE CSize CDC::SetWindowExt(SIZE size)
	{ ASSERT(m_hDC != NULL); return SetWindowExt(size.cx, size.cy); }

_AFXWIN_INLINE void CDC::DPtoLP(LPPOINT lpPoints, int nCount) const
	{ ASSERT(m_hAttribDC != NULL); VERIFY(::DPtoLP(m_hAttribDC, lpPoints, nCount)); }
_AFXWIN_INLINE void CDC::DPtoLP(LPRECT lpRect) const
	{ ASSERT(m_hAttribDC != NULL); VERIFY(::DPtoLP(m_hAttribDC, (LPPOINT)lpRect, 2)); }
_AFXWIN_INLINE void CDC::LPtoDP(LPPOINT lpPoints, int nCount) const
	{ ASSERT(m_hAttribDC != NULL); VERIFY(::LPtoDP(m_hAttribDC, lpPoints, nCount)); }
_AFXWIN_INLINE void CDC::LPtoDP(LPRECT lpRect) const
	{ ASSERT(m_hAttribDC != NULL); VERIFY(::LPtoDP(m_hAttribDC, (LPPOINT)lpRect, 2)); }

_AFXWIN_INLINE BOOL CDC::FillRgn(CRgn* pRgn, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); return ::FillRgn(m_hDC, (HRGN)pRgn->GetSafeHandle(), (HBRUSH)pBrush->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CDC::FrameRgn(CRgn* pRgn, CBrush* pBrush, int nWidth, int nHeight)
	{ ASSERT(m_hDC != NULL); return ::FrameRgn(m_hDC, (HRGN)pRgn->GetSafeHandle(), (HBRUSH)pBrush->GetSafeHandle(),
		nWidth, nHeight); }
_AFXWIN_INLINE BOOL CDC::InvertRgn(CRgn* pRgn)
	{ ASSERT(m_hDC != NULL); return ::InvertRgn(m_hDC, (HRGN)pRgn->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CDC::PaintRgn(CRgn* pRgn)
	{ ASSERT(m_hDC != NULL); return ::PaintRgn(m_hDC, (HRGN)pRgn->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CDC::PtVisible(int x, int y) const
	{ ASSERT(m_hDC != NULL); return ::PtVisible(m_hDC, x, y); }
_AFXWIN_INLINE BOOL CDC::PtVisible(POINT point) const
	{ ASSERT(m_hDC != NULL); return PtVisible(point.x, point.y); } // call virtual
_AFXWIN_INLINE BOOL CDC::RectVisible(LPCRECT lpRect) const
	{ ASSERT(m_hDC != NULL); return ::RectVisible(m_hDC, lpRect); }
_AFXWIN_INLINE CPoint CDC::GetCurrentPosition() const
	{
		ASSERT(m_hAttribDC != NULL);
		POINT point;
		VERIFY(::GetCurrentPositionEx(m_hAttribDC, &point));
		return point;
	}

_AFXWIN_INLINE CPoint CDC::MoveTo(POINT point)
	{ ASSERT(m_hDC != NULL); return MoveTo(point.x, point.y); }
_AFXWIN_INLINE BOOL CDC::LineTo(POINT point)
	{ ASSERT(m_hDC != NULL); return LineTo(point.x, point.y); }
_AFXWIN_INLINE BOOL CDC::Arc(int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)
	{ ASSERT(m_hDC != NULL); return ::Arc(m_hDC, x1, y1, x2, y2, x3, y3, x4, y4); }
_AFXWIN_INLINE BOOL CDC::Arc(LPCRECT lpRect, POINT ptStart, POINT ptEnd)
	{ ASSERT(m_hDC != NULL); return ::Arc(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom, ptStart.x, ptStart.y,
		ptEnd.x, ptEnd.y); }
_AFXWIN_INLINE BOOL CDC::Polyline(LPPOINT lpPoints, int nCount)
	{ ASSERT(m_hDC != NULL); return ::Polyline(m_hDC, lpPoints, nCount); }
_AFXWIN_INLINE void CDC::FillRect(LPCRECT lpRect, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); ::FillRect(m_hDC, lpRect, (HBRUSH)pBrush->GetSafeHandle()); }
_AFXWIN_INLINE void CDC::FrameRect(LPCRECT lpRect, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); ::FrameRect(m_hDC, lpRect, (HBRUSH)pBrush->GetSafeHandle()); }
_AFXWIN_INLINE void CDC::InvertRect(LPCRECT lpRect)
	{ ASSERT(m_hDC != NULL); ::InvertRect(m_hDC, lpRect); }
_AFXWIN_INLINE BOOL CDC::DrawIcon(int x, int y, HICON hIcon)
	{ ASSERT(m_hDC != NULL); return ::DrawIcon(m_hDC, x, y, hIcon); }
_AFXWIN_INLINE BOOL CDC::DrawIcon(POINT point, HICON hIcon)
	{ ASSERT(m_hDC != NULL); return ::DrawIcon(m_hDC, point.x, point.y, hIcon); }
#if (WINVER >= 0x400)
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, HBITMAP hBitmap, UINT nFlags, HBRUSH hBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, hBrush,
		NULL, (LPARAM)hBitmap, 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_BITMAP); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, CBitmap* pBitmap, UINT nFlags, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, (HBRUSH)pBrush->GetSafeHandle(),
		NULL, (LPARAM)pBitmap->GetSafeHandle(), 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_BITMAP); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, HICON hIcon, UINT nFlags, HBRUSH hBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, hBrush, NULL,
		(LPARAM)hIcon, 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_ICON); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, HICON hIcon, UINT nFlags, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, (HBRUSH)pBrush->GetSafeHandle(), NULL,
		(LPARAM)hIcon, 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_ICON); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, LPCTSTR lpszText, UINT nFlags, BOOL bPrefixText, int nTextLen, HBRUSH hBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, hBrush,
		NULL, (LPARAM)lpszText, (WPARAM)nTextLen, pt.x, pt.y, size.cx, size.cy, nFlags|(bPrefixText ? DST_PREFIXTEXT : DST_TEXT)); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, LPCTSTR lpszText, UINT nFlags, BOOL bPrefixText, int nTextLen, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, (HBRUSH)pBrush->GetSafeHandle(),
		NULL, (LPARAM)lpszText, (WPARAM)nTextLen, pt.x, pt.y, size.cx, size.cy, nFlags|(bPrefixText ? DST_PREFIXTEXT : DST_TEXT)); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, DRAWSTATEPROC lpDrawProc, LPARAM lData, UINT nFlags, HBRUSH hBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, hBrush,
		lpDrawProc, lData, 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_COMPLEX); }
_AFXWIN_INLINE BOOL CDC::DrawState(CPoint pt, CSize size, DRAWSTATEPROC lpDrawProc, LPARAM lData, UINT nFlags, CBrush* pBrush)
	{ ASSERT(m_hDC != NULL); return ::DrawState(m_hDC, (HBRUSH)pBrush->GetSafeHandle(),
		lpDrawProc, lData, 0, pt.x, pt.y, size.cx, size.cy, nFlags|DST_COMPLEX); }
_AFXWIN_INLINE BOOL CDC::DrawEdge(LPRECT lpRect, UINT nEdge, UINT nFlags)
	{ ASSERT(m_hDC != NULL); return ::DrawEdge(m_hDC, lpRect, nEdge, nFlags); }
_AFXWIN_INLINE BOOL CDC::DrawFrameControl(LPRECT lpRect, UINT nType, UINT nState)
	{ ASSERT(m_hDC != NULL); return ::DrawFrameControl(m_hDC, lpRect, nType, nState); }
#endif

_AFXWIN_INLINE BOOL CDC::Chord(int x1, int y1, int x2, int y2, int x3, int y3,
	int x4, int y4)
	{ ASSERT(m_hDC != NULL); return ::Chord(m_hDC, x1, y1, x2, y2, x3, y3, x4, y4); }
_AFXWIN_INLINE BOOL CDC::Chord(LPCRECT lpRect, POINT ptStart, POINT ptEnd)
	{ ASSERT(m_hDC != NULL); return ::Chord(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom, ptStart.x, ptStart.y,
		ptEnd.x, ptEnd.y); }
_AFXWIN_INLINE void CDC::DrawFocusRect(LPCRECT lpRect)
	{ ASSERT(m_hDC != NULL); ::DrawFocusRect(m_hDC, lpRect); }
_AFXWIN_INLINE BOOL CDC::Ellipse(int x1, int y1, int x2, int y2)
	{ ASSERT(m_hDC != NULL); return ::Ellipse(m_hDC, x1, y1, x2, y2); }
_AFXWIN_INLINE BOOL CDC::Ellipse(LPCRECT lpRect)
	{ ASSERT(m_hDC != NULL); return ::Ellipse(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom); }
_AFXWIN_INLINE BOOL CDC::Pie(int x1, int y1, int x2, int y2, int x3, int y3, int x4, int y4)
	{ ASSERT(m_hDC != NULL); return ::Pie(m_hDC, x1, y1, x2, y2, x3, y3, x4, y4); }
_AFXWIN_INLINE BOOL CDC::Pie(LPCRECT lpRect, POINT ptStart, POINT ptEnd)
	{ ASSERT(m_hDC != NULL); return ::Pie(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom, ptStart.x, ptStart.y,
		ptEnd.x, ptEnd.y); }
_AFXWIN_INLINE BOOL CDC::Polygon(LPPOINT lpPoints, int nCount)
	{ ASSERT(m_hDC != NULL); return ::Polygon(m_hDC, lpPoints, nCount); }
_AFXWIN_INLINE BOOL CDC::PolyPolygon(LPPOINT lpPoints, LPINT lpPolyCounts, int nCount)
	{ ASSERT(m_hDC != NULL); return ::PolyPolygon(m_hDC, lpPoints, lpPolyCounts, nCount); }
_AFXWIN_INLINE BOOL CDC::Rectangle(int x1, int y1, int x2, int y2)
	{ ASSERT(m_hDC != NULL); return ::Rectangle(m_hDC, x1, y1, x2, y2); }
_AFXWIN_INLINE BOOL CDC::Rectangle(LPCRECT lpRect)
	{ ASSERT(m_hDC != NULL); return ::Rectangle(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom); }
_AFXWIN_INLINE BOOL CDC::RoundRect(int x1, int y1, int x2, int y2, int x3, int y3)
	{ ASSERT(m_hDC != NULL); return ::RoundRect(m_hDC, x1, y1, x2, y2, x3, y3); }
_AFXWIN_INLINE BOOL CDC::RoundRect(LPCRECT lpRect, POINT point)
	{ ASSERT(m_hDC != NULL); return ::RoundRect(m_hDC, lpRect->left, lpRect->top,
		lpRect->right, lpRect->bottom, point.x, point.y); }
_AFXWIN_INLINE BOOL CDC::PatBlt(int x, int y, int nWidth, int nHeight, DWORD dwRop)
	{ ASSERT(m_hDC != NULL); return ::PatBlt(m_hDC, x, y, nWidth, nHeight, dwRop); }
_AFXWIN_INLINE BOOL CDC::BitBlt(int x, int y, int nWidth, int nHeight, CDC* pSrcDC,
	int xSrc, int ySrc, DWORD dwRop)
	{ ASSERT(m_hDC != NULL); return ::BitBlt(m_hDC, x, y, nWidth, nHeight,
		pSrcDC->GetSafeHdc(), xSrc, ySrc, dwRop); }
_AFXWIN_INLINE BOOL CDC::StretchBlt(int x, int y, int nWidth, int nHeight, CDC* pSrcDC,
	int xSrc, int ySrc, int nSrcWidth, int nSrcHeight, DWORD dwRop)
	{ ASSERT(m_hDC != NULL); return ::StretchBlt(m_hDC, x, y, nWidth, nHeight,
		pSrcDC->GetSafeHdc(), xSrc, ySrc, nSrcWidth, nSrcHeight,
		dwRop); }
_AFXWIN_INLINE COLORREF CDC::GetPixel(int x, int y) const
	{ ASSERT(m_hDC != NULL); return ::GetPixel(m_hDC, x, y); }
_AFXWIN_INLINE COLORREF CDC::GetPixel(POINT point) const
	{ ASSERT(m_hDC != NULL); return ::GetPixel(m_hDC, point.x, point.y); }
_AFXWIN_INLINE COLORREF CDC::SetPixel(int x, int y, COLORREF crColor)
	{ ASSERT(m_hDC != NULL); return ::SetPixel(m_hDC, x, y, crColor); }
_AFXWIN_INLINE COLORREF CDC::SetPixel(POINT point, COLORREF crColor)
	{ ASSERT(m_hDC != NULL); return ::SetPixel(m_hDC, point.x, point.y, crColor); }
_AFXWIN_INLINE BOOL CDC::FloodFill(int x, int y, COLORREF crColor)
	{ ASSERT(m_hDC != NULL); return ::FloodFill(m_hDC, x, y, crColor); }
_AFXWIN_INLINE BOOL CDC::ExtFloodFill(int x, int y, COLORREF crColor, UINT nFillType)
	{ ASSERT(m_hDC != NULL); return ::ExtFloodFill(m_hDC, x, y, crColor, nFillType); }
_AFXWIN_INLINE BOOL CDC::TextOut(int x, int y, LPCTSTR lpszString, int nCount)
	{ ASSERT(m_hDC != NULL); return ::TextOut(m_hDC, x, y, lpszString, nCount); }
_AFXWIN_INLINE BOOL CDC::TextOut(int x, int y, const CString& str)
	{ ASSERT(m_hDC != NULL); return TextOut(x, y, (LPCTSTR)str, str.GetLength()); } // call virtual
_AFXWIN_INLINE BOOL CDC::ExtTextOut(int x, int y, UINT nOptions, LPCRECT lpRect,
	LPCTSTR lpszString, UINT nCount, LPINT lpDxWidths)
	{ ASSERT(m_hDC != NULL); return ::ExtTextOut(m_hDC, x, y, nOptions, lpRect,
		lpszString, nCount, lpDxWidths); }
_AFXWIN_INLINE BOOL CDC::ExtTextOut(int x, int y, UINT nOptions, LPCRECT lpRect,
	const CString& str, LPINT lpDxWidths)
	{ ASSERT(m_hDC != NULL); return ::ExtTextOut(m_hDC, x, y, nOptions, lpRect,
		str, str.GetLength(), lpDxWidths); }
_AFXWIN_INLINE CSize CDC::TabbedTextOut(int x, int y, LPCTSTR lpszString, int nCount,
	int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin)
	{ ASSERT(m_hDC != NULL); return ::TabbedTextOut(m_hDC, x, y, lpszString, nCount,
		nTabPositions, lpnTabStopPositions, nTabOrigin); }
_AFXWIN_INLINE CSize CDC::TabbedTextOut(int x, int y, const CString& str,
	int nTabPositions, LPINT lpnTabStopPositions, int nTabOrigin)
	{ ASSERT(m_hDC != NULL); return ::TabbedTextOut(m_hDC, x, y, str, str.GetLength(),
		nTabPositions, lpnTabStopPositions, nTabOrigin); }
_AFXWIN_INLINE int CDC::DrawText(LPCTSTR lpszString, int nCount, LPRECT lpRect,
		UINT nFormat)
	{ ASSERT(m_hDC != NULL);
		return ::DrawText(m_hDC, lpszString, nCount, lpRect, nFormat); }
_AFXWIN_INLINE int CDC::DrawText(const CString& str, LPRECT lpRect, UINT nFormat)
	{ ASSERT(m_hDC != NULL);
		// these flags would modify the string
		ASSERT((nFormat & (DT_END_ELLIPSIS | DT_MODIFYSTRING)) != (DT_END_ELLIPSIS | DT_MODIFYSTRING));
		ASSERT((nFormat & (DT_PATH_ELLIPSIS | DT_MODIFYSTRING)) != (DT_PATH_ELLIPSIS | DT_MODIFYSTRING));
		return DrawText((LPCTSTR)str, str.GetLength(), lpRect, nFormat); }
_AFXWIN_INLINE CSize CDC::GetTextExtent(LPCTSTR lpszString, int nCount) const
	{
		ASSERT(m_hAttribDC != NULL);
		SIZE size;
		VERIFY(::GetTextExtentPoint32(m_hAttribDC, lpszString, nCount, &size));
		return size;
	}
_AFXWIN_INLINE CSize CDC::GetTextExtent(const CString& str) const
	{
		ASSERT(m_hAttribDC != NULL);
		SIZE size;
		VERIFY(::GetTextExtentPoint32(m_hAttribDC, str, str.GetLength(), &size));
		return size;
	}

_AFXWIN_INLINE CSize CDC::GetOutputTextExtent(LPCTSTR lpszString, int nCount) const
	{
		ASSERT(m_hDC != NULL);
		SIZE size;
		VERIFY(::GetTextExtentPoint32(m_hDC, lpszString, nCount, &size));
		return size;
	}
_AFXWIN_INLINE CSize CDC::GetOutputTextExtent(const CString& str) const
	{
		ASSERT(m_hDC != NULL);
		SIZE size;
		VERIFY(::GetTextExtentPoint32(m_hDC, str, str.GetLength(), &size));
		return size;
	}

_AFXWIN_INLINE CSize CDC::GetTabbedTextExtent(LPCTSTR lpszString, int nCount,
	int nTabPositions, LPINT lpnTabStopPositions) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTabbedTextExtent(m_hAttribDC, lpszString, nCount,
		nTabPositions, lpnTabStopPositions); }
_AFXWIN_INLINE  CSize CDC::GetTabbedTextExtent(const CString& str,
		int nTabPositions, LPINT lpnTabStopPositions) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTabbedTextExtent(m_hAttribDC,
		str, str.GetLength(), nTabPositions, lpnTabStopPositions); }
_AFXWIN_INLINE CSize CDC::GetOutputTabbedTextExtent(LPCTSTR lpszString, int nCount,
	int nTabPositions, LPINT lpnTabStopPositions) const
	{ ASSERT(m_hDC != NULL); return ::GetTabbedTextExtent(m_hDC, lpszString, nCount,
		nTabPositions, lpnTabStopPositions); }
_AFXWIN_INLINE  CSize CDC::GetOutputTabbedTextExtent(const CString& str,
		int nTabPositions, LPINT lpnTabStopPositions) const
	{ ASSERT(m_hDC != NULL); return ::GetTabbedTextExtent(m_hDC,
		str, str.GetLength(), nTabPositions, lpnTabStopPositions); }
_AFXWIN_INLINE BOOL CDC::GrayString(CBrush* pBrush,
	BOOL (CALLBACK* lpfnOutput)(HDC, LPARAM, int),
		LPARAM lpData, int nCount,
		int x, int y, int nWidth, int nHeight)
	{ ASSERT(m_hDC != NULL); return ::GrayString(m_hDC, (HBRUSH)pBrush->GetSafeHandle(),
		(GRAYSTRINGPROC)lpfnOutput, lpData, nCount, x, y, nWidth, nHeight); }
_AFXWIN_INLINE UINT CDC::GetTextAlign() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTextAlign(m_hAttribDC); }
_AFXWIN_INLINE int CDC::GetTextFace(int nCount, LPTSTR lpszFacename) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTextFace(m_hAttribDC, nCount, lpszFacename); }
_AFXWIN_INLINE  int CDC::GetTextFace(CString& rString) const
	{ ASSERT(m_hAttribDC != NULL); int nResult = ::GetTextFace(m_hAttribDC,
		256, rString.GetBuffer(256)); rString.ReleaseBuffer();
		return nResult; }
_AFXWIN_INLINE BOOL CDC::GetTextMetrics(LPTEXTMETRIC lpMetrics) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTextMetrics(m_hAttribDC, lpMetrics); }
_AFXWIN_INLINE BOOL CDC::GetOutputTextMetrics(LPTEXTMETRIC lpMetrics) const
	{ ASSERT(m_hDC != NULL); return ::GetTextMetrics(m_hDC, lpMetrics); }
_AFXWIN_INLINE int CDC::GetTextCharacterExtra() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetTextCharacterExtra(m_hAttribDC); }
_AFXWIN_INLINE BOOL CDC::GetCharWidth(UINT nFirstChar, UINT nLastChar, LPINT lpBuffer) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetCharWidth(m_hAttribDC, nFirstChar, nLastChar, lpBuffer); }
_AFXWIN_INLINE BOOL CDC::GetOutputCharWidth(UINT nFirstChar, UINT nLastChar, LPINT lpBuffer) const
	{ ASSERT(m_hDC != NULL); return ::GetCharWidth(m_hDC, nFirstChar, nLastChar, lpBuffer); }
_AFXWIN_INLINE CSize CDC::GetAspectRatioFilter() const
	{
		ASSERT(m_hAttribDC != NULL);
		SIZE size;
		VERIFY(::GetAspectRatioFilterEx(m_hAttribDC, &size));
		return size;
	}
_AFXWIN_INLINE BOOL CDC::ScrollDC(int dx, int dy,
		LPCRECT lpRectScroll, LPCRECT lpRectClip,
		CRgn* pRgnUpdate, LPRECT lpRectUpdate)
	{ ASSERT(m_hDC != NULL); return ::ScrollDC(m_hDC, dx, dy, lpRectScroll,
		lpRectClip, (HRGN)pRgnUpdate->GetSafeHandle(), lpRectUpdate); }

// Printer Escape Functions
_AFXWIN_INLINE int CDC::Escape(int nEscape, int nCount, LPCSTR lpszInData, LPVOID lpOutData)
	{ ASSERT(m_hDC != NULL); return ::Escape(m_hDC, nEscape, nCount, lpszInData, lpOutData);}

// CDC 3.1 Specific functions
_AFXWIN_INLINE UINT CDC::SetBoundsRect(LPCRECT lpRectBounds, UINT flags)
	{ ASSERT(m_hDC != NULL); return ::SetBoundsRect(m_hDC, lpRectBounds, flags); }
_AFXWIN_INLINE UINT CDC::GetBoundsRect(LPRECT lpRectBounds, UINT flags)
	{ ASSERT(m_hAttribDC != NULL); return ::GetBoundsRect(m_hAttribDC, lpRectBounds, flags); }
_AFXWIN_INLINE BOOL CDC::ResetDC(const DEVMODE* lpDevMode)
	{ ASSERT(m_hAttribDC != NULL); return ::ResetDC(m_hAttribDC, lpDevMode) != NULL; }
_AFXWIN_INLINE UINT CDC::GetOutlineTextMetrics(UINT cbData, LPOUTLINETEXTMETRIC lpotm) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetOutlineTextMetrics(m_hAttribDC, cbData, lpotm); }
_AFXWIN_INLINE BOOL CDC::GetCharABCWidths(UINT nFirstChar, UINT nLastChar, LPABC lpabc) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetCharABCWidths(m_hAttribDC, nFirstChar, nLastChar, lpabc); }
_AFXWIN_INLINE DWORD CDC::GetFontData(DWORD dwTable, DWORD dwOffset, LPVOID lpData,
	DWORD cbData) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetFontData(m_hAttribDC, dwTable, dwOffset, lpData, cbData); }
_AFXWIN_INLINE int CDC::GetKerningPairs(int nPairs, LPKERNINGPAIR lpkrnpair) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetKerningPairs(m_hAttribDC, nPairs, lpkrnpair); }
_AFXWIN_INLINE DWORD CDC::GetGlyphOutline(UINT nChar, UINT nFormat, LPGLYPHMETRICS lpgm,
		DWORD cbBuffer, LPVOID lpBuffer, const MAT2* lpmat2) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetGlyphOutline(m_hAttribDC, nChar, nFormat,
			lpgm, cbBuffer, lpBuffer, lpmat2); }

// Document handling functions
_AFXWIN_INLINE int CDC::StartDoc(LPDOCINFO lpDocInfo)
	{ ASSERT(m_hDC != NULL); return ::StartDoc(m_hDC, lpDocInfo); }
_AFXWIN_INLINE int CDC::StartPage()
	{ ASSERT(m_hDC != NULL); return ::StartPage(m_hDC); }
_AFXWIN_INLINE int CDC::EndPage()
	{ ASSERT(m_hDC != NULL); return ::EndPage(m_hDC); }
_AFXWIN_INLINE int CDC::SetAbortProc(BOOL (CALLBACK* lpfn)(HDC, int))
	{ ASSERT(m_hDC != NULL); return ::SetAbortProc(m_hDC, (ABORTPROC)lpfn); }
_AFXWIN_INLINE int CDC::AbortDoc()
	{ ASSERT(m_hDC != NULL); return ::AbortDoc(m_hDC); }
_AFXWIN_INLINE int CDC::EndDoc()
	{ ASSERT(m_hDC != NULL); return ::EndDoc(m_hDC); }

_AFXWIN_INLINE BOOL CDC::MaskBlt(int x, int y, int nWidth, int nHeight, CDC* pSrcDC,
		int xSrc, int ySrc, CBitmap& maskBitmap, int xMask, int yMask, DWORD dwRop)
	{ ASSERT(m_hDC != NULL); return ::MaskBlt(m_hDC, x, y, nWidth, nHeight, pSrcDC->GetSafeHdc(),
		xSrc, ySrc,  (HBITMAP)maskBitmap.m_hObject, xMask, yMask, dwRop); }
_AFXWIN_INLINE BOOL CDC::PlgBlt(LPPOINT lpPoint, CDC* pSrcDC, int xSrc, int ySrc,
		int nWidth, int nHeight, CBitmap& maskBitmap, int xMask, int yMask)
	{ ASSERT(m_hDC != NULL); return ::PlgBlt(m_hDC, lpPoint, pSrcDC->GetSafeHdc(), xSrc, ySrc, nWidth,
		nHeight, (HBITMAP)maskBitmap.m_hObject, xMask, yMask); }
_AFXWIN_INLINE BOOL CDC::SetPixelV(int x, int y, COLORREF crColor)
	{ ASSERT(m_hDC != NULL); return ::SetPixelV(m_hDC, x, y, crColor); }
_AFXWIN_INLINE BOOL CDC::SetPixelV(POINT point, COLORREF crColor)
	{ ASSERT(m_hDC != NULL); return ::SetPixelV(m_hDC, point.x, point.y, crColor); }
_AFXWIN_INLINE BOOL CDC::AngleArc(int x, int y, int nRadius,
		float fStartAngle, float fSweepAngle)
	{ ASSERT(m_hDC != NULL); return ::AngleArc(m_hDC, x, y, nRadius, fStartAngle, fSweepAngle); }
_AFXWIN_INLINE BOOL CDC::ArcTo(LPCRECT lpRect, POINT ptStart, POINT ptEnd)
	{ ASSERT(m_hDC != NULL); return ArcTo(lpRect->left, lpRect->top, lpRect->right,
		lpRect->bottom, ptStart.x, ptStart.y, ptEnd.x, ptEnd.y); }
_AFXWIN_INLINE int CDC::GetArcDirection() const
	{ ASSERT(m_hAttribDC != NULL); return ::GetArcDirection(m_hAttribDC); }
_AFXWIN_INLINE BOOL CDC::PolyPolyline(const POINT* lpPoints, const DWORD* lpPolyPoints,
		int nCount)
	{ ASSERT(m_hDC != NULL); return ::PolyPolyline(m_hDC, lpPoints, lpPolyPoints, nCount); }
_AFXWIN_INLINE BOOL CDC::GetColorAdjustment(LPCOLORADJUSTMENT lpColorAdjust) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetColorAdjustment(m_hAttribDC, lpColorAdjust); }
_AFXWIN_INLINE CPen* CDC::GetCurrentPen() const
	{ ASSERT(m_hAttribDC != NULL); return CPen::FromHandle((HPEN)::GetCurrentObject(m_hAttribDC, OBJ_PEN)); }
_AFXWIN_INLINE CBrush* CDC::GetCurrentBrush() const
	{ ASSERT(m_hAttribDC != NULL); return CBrush::FromHandle((HBRUSH)::GetCurrentObject(m_hAttribDC, OBJ_BRUSH)); }
_AFXWIN_INLINE CPalette* CDC::GetCurrentPalette() const
	{ ASSERT(m_hAttribDC != NULL); return CPalette::FromHandle((HPALETTE)::GetCurrentObject(m_hAttribDC, OBJ_PAL)); }
_AFXWIN_INLINE CFont* CDC::GetCurrentFont() const
	{ ASSERT(m_hAttribDC != NULL); return CFont::FromHandle((HFONT)::GetCurrentObject(m_hAttribDC, OBJ_FONT)); }
_AFXWIN_INLINE CBitmap* CDC::GetCurrentBitmap() const
	{ ASSERT(m_hAttribDC != NULL); return CBitmap::FromHandle((HBITMAP)::GetCurrentObject(m_hAttribDC, OBJ_BITMAP)); }
_AFXWIN_INLINE BOOL CDC::PolyBezier(const POINT* lpPoints, int nCount)
	{ ASSERT(m_hDC != NULL); return ::PolyBezier(m_hDC, lpPoints, nCount); }

_AFXWIN_INLINE int CDC::DrawEscape(int nEscape, int nInputSize, LPCSTR lpszInputData)
	{ ASSERT(m_hDC != NULL); return ::DrawEscape(m_hDC, nEscape, nInputSize, lpszInputData); }
_AFXWIN_INLINE int CDC::Escape(int nEscape, int nInputSize, LPCSTR lpszInputData,
		int nOutputSize, LPSTR lpszOutputData)
	{ ASSERT(m_hDC != NULL); return ::ExtEscape(m_hDC, nEscape, nInputSize, lpszInputData,
		nOutputSize, lpszOutputData); }

_AFXWIN_INLINE BOOL CDC::GetCharABCWidths(UINT nFirstChar, UINT nLastChar,
		LPABCFLOAT lpABCF) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetCharABCWidthsFloat(m_hAttribDC, nFirstChar, nLastChar, lpABCF); }
_AFXWIN_INLINE BOOL CDC::GetCharWidth(UINT nFirstChar, UINT nLastChar,
		float* lpFloatBuffer) const
	{ ASSERT(m_hAttribDC != NULL); return ::GetCharWidthFloat(m_hAttribDC, nFirstChar, nLastChar, lpFloatBuffer); }

_AFXWIN_INLINE BOOL CDC::AbortPath()
	{ ASSERT(m_hDC != NULL); return ::AbortPath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::BeginPath()
	{ ASSERT(m_hDC != NULL); return ::BeginPath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::CloseFigure()
	{ ASSERT(m_hDC != NULL); return ::CloseFigure(m_hDC); }
_AFXWIN_INLINE BOOL CDC::EndPath()
	{ ASSERT(m_hDC != NULL); return ::EndPath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::FillPath()
	{ ASSERT(m_hDC != NULL); return ::FillPath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::FlattenPath()
	{ ASSERT(m_hDC != NULL); return ::FlattenPath(m_hDC); }
_AFXWIN_INLINE float CDC::GetMiterLimit() const
	{ ASSERT(m_hDC != NULL); float fMiterLimit;
		VERIFY(::GetMiterLimit(m_hDC, &fMiterLimit)); return fMiterLimit; }
_AFXWIN_INLINE int CDC::GetPath(LPPOINT lpPoints, LPBYTE lpTypes, int nCount) const
	{ ASSERT(m_hDC != NULL); return ::GetPath(m_hDC, lpPoints, lpTypes, nCount); }
_AFXWIN_INLINE BOOL CDC::SetMiterLimit(float fMiterLimit)
	{ ASSERT(m_hDC != NULL); return ::SetMiterLimit(m_hDC, fMiterLimit, NULL); }
_AFXWIN_INLINE BOOL CDC::StrokeAndFillPath()
	{ ASSERT(m_hDC != NULL); return ::StrokeAndFillPath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::StrokePath()
	{ ASSERT(m_hDC != NULL); return ::StrokePath(m_hDC); }
_AFXWIN_INLINE BOOL CDC::WidenPath()
	{ ASSERT(m_hDC != NULL); return ::WidenPath(m_hDC); }

_AFXWIN_INLINE BOOL CDC::AddMetaFileComment(UINT nDataSize, const BYTE* pCommentData)
	{ ASSERT(m_hDC != NULL); return ::GdiComment(m_hDC, nDataSize, pCommentData); }
_AFXWIN_INLINE BOOL CDC::PlayMetaFile(HENHMETAFILE hEnhMF, LPCRECT lpBounds)
	{ ASSERT(m_hDC != NULL); return ::PlayEnhMetaFile(m_hDC, hEnhMF, lpBounds); }

// CMenu
_AFXWIN_INLINE CMenu::CMenu()
	{ m_hMenu = NULL; }
_AFXWIN_INLINE CMenu::~CMenu()
	{ DestroyMenu(); }
_AFXWIN_INLINE BOOL CMenu::CreateMenu()
	{ return Attach(::CreateMenu()); }
_AFXWIN_INLINE BOOL CMenu::CreatePopupMenu()
	{ return Attach(::CreatePopupMenu()); }
_AFXWIN_INLINE CMenu::operator HMENU() const
	{ ASSERT(this == NULL || m_hMenu == NULL || ::IsMenu(m_hMenu));
		return this == NULL ? NULL : m_hMenu; }
_AFXWIN_INLINE CMenu::operator==(const CMenu& menu) const
	{ return ((HMENU) menu) == m_hMenu; }
_AFXWIN_INLINE CMenu::operator!=(const CMenu& menu) const
	{ return ((HMENU) menu) != m_hMenu; }
_AFXWIN_INLINE HMENU CMenu::GetSafeHmenu() const
	{ ASSERT(this == NULL || m_hMenu == NULL || ::IsMenu(m_hMenu));
		return this == NULL ? NULL : m_hMenu; }
_AFXWIN_INLINE BOOL CMenu::DeleteMenu(UINT nPosition, UINT nFlags)
	{ ASSERT(::IsMenu(m_hMenu)); return ::DeleteMenu(m_hMenu, nPosition, nFlags); }
_AFXWIN_INLINE BOOL CMenu::AppendMenu(UINT nFlags, UINT nIDNewItem, LPCTSTR lpszNewItem)
	{ ASSERT(::IsMenu(m_hMenu)); return ::AppendMenu(m_hMenu, nFlags, nIDNewItem, lpszNewItem); }
_AFXWIN_INLINE BOOL CMenu::AppendMenu(UINT nFlags, UINT nIDNewItem, const CBitmap* pBmp)
	{ ASSERT(::IsMenu(m_hMenu)); return ::AppendMenu(m_hMenu, nFlags | MF_BITMAP, nIDNewItem,
		(LPCTSTR)pBmp->GetSafeHandle()); }
_AFXWIN_INLINE UINT CMenu::CheckMenuItem(UINT nIDCheckItem, UINT nCheck)
	{ ASSERT(::IsMenu(m_hMenu)); return (UINT)::CheckMenuItem(m_hMenu, nIDCheckItem, nCheck); }
_AFXWIN_INLINE UINT CMenu::EnableMenuItem(UINT nIDEnableItem, UINT nEnable)
	{ ASSERT(::IsMenu(m_hMenu)); return ::EnableMenuItem(m_hMenu, nIDEnableItem, nEnable); }
_AFXWIN_INLINE BOOL CMenu::SetDefaultItem(UINT uItem, BOOL fByPos)
	{ ASSERT(::IsMenu(m_hMenu)); return ::SetMenuDefaultItem(m_hMenu, uItem, fByPos); }
_AFXWIN_INLINE UINT CMenu::GetDefaultItem(UINT gmdiFlags, BOOL fByPos)
	{ ASSERT(::IsMenu(m_hMenu)); return ::GetMenuDefaultItem(m_hMenu, fByPos, gmdiFlags); }
_AFXWIN_INLINE UINT CMenu::GetMenuItemCount() const
	{ ASSERT(::IsMenu(m_hMenu)); return ::GetMenuItemCount(m_hMenu); }
_AFXWIN_INLINE UINT CMenu::GetMenuItemID(int nPos) const
	{ ASSERT(::IsMenu(m_hMenu)); return ::GetMenuItemID(m_hMenu, nPos); }
_AFXWIN_INLINE UINT CMenu::GetMenuState(UINT nID, UINT nFlags) const
	{ ASSERT(::IsMenu(m_hMenu)); return ::GetMenuState(m_hMenu, nID, nFlags); }
_AFXWIN_INLINE int CMenu::GetMenuString(UINT nIDItem, LPTSTR lpString, int nMaxCount, UINT nFlags) const
	{ ASSERT(::IsMenu(m_hMenu)); return ::GetMenuString(m_hMenu, nIDItem, lpString, nMaxCount, nFlags); }
_AFXWIN_INLINE int CMenu::GetMenuString(UINT nIDItem, CString& rString, UINT nFlags) const
	{ ASSERT(::IsMenu(m_hMenu)); int nResult = ::GetMenuString(m_hMenu, nIDItem,
		rString.GetBuffer(256), 256, nFlags); rString.ReleaseBuffer();
		return nResult; }
_AFXWIN_INLINE BOOL CMenu::GetMenuItemInfo(UINT nIDItem, LPMENUITEMINFO lpMenuItemInfo, BOOL fByPos)
	{ ASSERT(::IsMenu(m_hMenu)); ASSERT_POINTER(lpMenuItemInfo, MENUITEMINFO);
		return ::GetMenuItemInfo(m_hMenu, nIDItem, fByPos, lpMenuItemInfo); }
_AFXWIN_INLINE CMenu* CMenu::GetSubMenu(int nPos) const
	{ ASSERT(::IsMenu(m_hMenu)); return CMenu::FromHandle(::GetSubMenu(m_hMenu, nPos)); }
_AFXWIN_INLINE BOOL CMenu::InsertMenu(UINT nPosition, UINT nFlags, UINT nIDNewItem,
		LPCTSTR lpszNewItem)
	{ ASSERT(::IsMenu(m_hMenu)); return ::InsertMenu(m_hMenu, nPosition, nFlags, nIDNewItem, lpszNewItem); }
_AFXWIN_INLINE BOOL CMenu::InsertMenu(UINT nPosition, UINT nFlags, UINT nIDNewItem, const CBitmap* pBmp)
	{ ASSERT(::IsMenu(m_hMenu)); return ::InsertMenu(m_hMenu, nPosition, nFlags | MF_BITMAP, nIDNewItem,
		(LPCTSTR)pBmp->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CMenu::ModifyMenu(UINT nPosition, UINT nFlags, UINT nIDNewItem, LPCTSTR lpszNewItem)
	{ ASSERT(::IsMenu(m_hMenu)); return ::ModifyMenu(m_hMenu, nPosition, nFlags, nIDNewItem, lpszNewItem); }
_AFXWIN_INLINE BOOL CMenu::ModifyMenu(UINT nPosition, UINT nFlags, UINT nIDNewItem, const CBitmap* pBmp)
	{ ASSERT(::IsMenu(m_hMenu)); return ::ModifyMenu(m_hMenu, nPosition, nFlags | MF_BITMAP, nIDNewItem,
		(LPCTSTR)pBmp->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CMenu::RemoveMenu(UINT nPosition, UINT nFlags)
	{ ASSERT(::IsMenu(m_hMenu)); return ::RemoveMenu(m_hMenu, nPosition, nFlags); }
_AFXWIN_INLINE BOOL CMenu::SetMenuItemBitmaps(UINT nPosition, UINT nFlags,
		const CBitmap* pBmpUnchecked, const CBitmap* pBmpChecked)
	{ ASSERT(::IsMenu(m_hMenu)); return ::SetMenuItemBitmaps(m_hMenu, nPosition, nFlags,
		(HBITMAP)pBmpUnchecked->GetSafeHandle(),
		(HBITMAP)pBmpChecked->GetSafeHandle()); }
_AFXWIN_INLINE BOOL CMenu::LoadMenu(LPCTSTR lpszResourceName)
	{ return Attach(::LoadMenu(AfxFindResourceHandle(lpszResourceName,
		RT_MENU), lpszResourceName)); }
_AFXWIN_INLINE BOOL CMenu::LoadMenu(UINT nIDResource)
	{ return Attach(::LoadMenu(AfxFindResourceHandle(
		MAKEINTRESOURCE(nIDResource), RT_MENU), MAKEINTRESOURCE(nIDResource))); }
_AFXWIN_INLINE BOOL CMenu::LoadMenuIndirect(const void* lpMenuTemplate)
	{ return Attach(::LoadMenuIndirect(lpMenuTemplate)); }
// Win4
_AFXWIN_INLINE BOOL CMenu::SetMenuContextHelpId(DWORD dwContextHelpId)
	{ return ::SetMenuContextHelpId(m_hMenu, dwContextHelpId); }
_AFXWIN_INLINE DWORD CMenu::GetMenuContextHelpId() const
	{ return ::GetMenuContextHelpId(m_hMenu); }
_AFXWIN_INLINE BOOL CMenu::CheckMenuRadioItem(UINT nIDFirst, UINT nIDLast, UINT nIDItem, UINT nFlags)
	{ return ::CheckMenuRadioItem(m_hMenu, nIDFirst, nIDLast, nIDItem, nFlags); }

// CCmdUI
_AFXWIN_INLINE void CCmdUI::ContinueRouting()
	{ m_bContinueRouting = TRUE; }

/////////////////////////////////////////////////////////////////////////////

#endif //_AFXWIN_INLINE
