// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXRICH.H

#ifdef _AFXRICH_INLINE

// CRichEditView
_AFXRICH_INLINE CRichEditCtrl& CRichEditView::GetRichEditCtrl() const
	{ return *(CRichEditCtrl*)this; }
int _AFXRICH_INLINE CRichEditView::GetPrintWidth() const
	{ return m_sizePaper.cx - m_rectMargin.left - m_rectMargin.right;}
CRect _AFXRICH_INLINE CRichEditView::GetPrintRect() const
	{ return CRect(m_rectMargin.left, m_rectMargin.top, m_sizePaper.cx - m_rectMargin.right, m_sizePaper.cy - m_rectMargin.bottom); }
CRect _AFXRICH_INLINE CRichEditView::GetPageRect() const
	{ return CRect(CPoint(0,0), m_sizePaper); }
void _AFXRICH_INLINE CRichEditView::SetPaperSize(CSize sizePaper)
	{ m_sizePaper = sizePaper; }
CSize _AFXRICH_INLINE CRichEditView::GetPaperSize() const
	{ return m_sizePaper; }
void _AFXRICH_INLINE CRichEditView::SetMargins(const CRect& rectMargin)
	{ m_rectMargin = rectMargin; }
CRect _AFXRICH_INLINE CRichEditView::GetMargins() const
	{ return m_rectMargin; }

_AFXRICH_INLINE long CRichEditView::GetTextLength() const
	{ return GetRichEditCtrl().GetTextLength(); }
_AFXRICH_INLINE CRichEditDoc* CRichEditView::GetDocument() const
{
	ASSERT(m_pDocument != NULL);
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CRichEditDoc)));
	return (CRichEditDoc*)m_pDocument;
}
_AFXRICH_INLINE int CRichEditDoc::GetStreamFormat() const
	{ return m_bRTF ? SF_RTF : SF_TEXT; }
_AFXRICH_INLINE void CRichEditDoc::InvalidateObjectCache()
	{ m_bUpdateObjectCache = TRUE; }

_AFXRICH_INLINE void CRichEditCntrItem::Mark(BOOL b)
	{ m_bMark = b; }
_AFXRICH_INLINE BOOL CRichEditCntrItem::IsMarked()
	{ return m_bMark||m_bLock; }
_AFXRICH_INLINE CRichEditDoc* CRichEditCntrItem::GetDocument()
	{ return (CRichEditDoc*)COleClientItem::GetDocument(); }
_AFXRICH_INLINE CRichEditView* CRichEditCntrItem::GetActiveView()
	{ return (CRichEditView*)COleClientItem::GetActiveView(); }
#endif //_AFXRICH_INLINE

/////////////////////////////////////////////////////////////////////////////
