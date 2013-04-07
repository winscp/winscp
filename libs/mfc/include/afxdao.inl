// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXDAO.H

/////////////////////////////////////////////////////////////////////////////
// General database inlines

#ifdef _AFXDAOCORE_INLINE

// CDaoWorkspace inlines
_AFXDAOCORE_INLINE BOOL CDaoWorkspace::IsOpen() const
	{ ASSERT_VALID(this); return m_bOpen; }
_AFXDAOCORE_INLINE BOOL CDaoWorkspace::IsNew() const
	{ ASSERT_VALID(this); return m_bNew; }

// CDaoDatabase inlines
_AFXDAOCORE_INLINE BOOL CDaoDatabase::IsOpen() const
	{ ASSERT_VALID(this); return m_bOpen; }

// CDaoTableDef inlines
_AFXDAOCORE_INLINE BOOL CDaoTableDef::IsOpen() const
	{ ASSERT_VALID(this); return m_bOpen; }

// CDaoQueryDef inlines
_AFXDAOCORE_INLINE BOOL CDaoQueryDef::IsOpen() const
	{ ASSERT_VALID(this); return m_bOpen; }

// CDaoRecordset inlines
_AFXDAOCORE_INLINE BOOL CDaoRecordset::IsOpen() const
	{ ASSERT_VALID(this); return m_bOpen; }

#endif // _AFXDAOCORE_INLINE


#ifdef _AFXDAODFX_INLINE

// CDaoFieldExchange
_AFXDAODFX_INLINE void CDaoFieldExchange::SetFieldType(UINT nFieldType)
{ ASSERT(nFieldType == outputColumn || nFieldType == param);
		m_nFieldType = nFieldType; }

#endif // _AFXDAODFX_INLINE


#ifdef _AFXDAOVIEW_INLINE

// CDaoRecordView inlines
_AFXDAOVIEW_INLINE void CDaoRecordView::OnUpdateRecordFirst(CCmdUI* pCmdUI)
	{ ASSERT_VALID(this);
		pCmdUI->Enable(m_nStatus & AFX_DAOVIEW_SCROLL_BACKWARD); }

_AFXDAOVIEW_INLINE void CDaoRecordView::OnUpdateRecordNext(CCmdUI* pCmdUI)
	{ ASSERT_VALID(this);
		pCmdUI->Enable(m_nStatus & AFX_DAOVIEW_SCROLL_NEXT); }

_AFXDAOVIEW_INLINE void CDaoRecordView::OnUpdateRecordPrev(CCmdUI* pCmdUI)
	{ ASSERT_VALID(this);
		pCmdUI->Enable(m_nStatus & AFX_DAOVIEW_SCROLL_BACKWARD); }

_AFXDAOVIEW_INLINE void CDaoRecordView::OnUpdateRecordLast(CCmdUI* pCmdUI)
	{ ASSERT_VALID(this);
		pCmdUI->Enable(m_nStatus & AFX_DAOVIEW_SCROLL_LAST); }

_AFXDAOVIEW_INLINE void CDaoRecordView::OnMove(int cx, int cy)
	{ CFormView::OnMove(cx, cy); }

#endif // _AFXDAOVIEW_INLINE

/////////////////////////////////////////////////////////////////////////////
