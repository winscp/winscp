// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Inlines for AFXDB.H

/////////////////////////////////////////////////////////////////////////////
// General database inlines

#ifdef _AFXDBCORE_INLINE

// CDatabase inlines
_AFXDBCORE_INLINE BOOL CDatabase::IsOpen() const
	{ ASSERT_VALID(this); return m_hdbc != SQL_NULL_HDBC; }
_AFXDBCORE_INLINE BOOL CDatabase::CanUpdate() const
	{ ASSERT(IsOpen()); return m_bUpdatable; }
_AFXDBCORE_INLINE BOOL CDatabase::CanTransact() const
	{ ASSERT(IsOpen()); return m_bTransactions; }
_AFXDBCORE_INLINE void CDatabase::SetLoginTimeout(DWORD dwSeconds)
	{ ASSERT_VALID(this); m_dwLoginTimeout = dwSeconds; }
_AFXDBCORE_INLINE void CDatabase::SetQueryTimeout(DWORD dwSeconds)
	{ ASSERT_VALID(this); m_dwQueryTimeout = dwSeconds; }
_AFXDBCORE_INLINE const CString& CDatabase::GetConnect() const
	{ ASSERT_VALID(this); return m_strConnect; }
_AFXDBCORE_INLINE DWORD CDatabase::GetBookmarkPersistence() const
	{ ASSERT_VALID(this); return m_dwBookmarkAttributes; }
_AFXDBCORE_INLINE int CDatabase::GetCursorCommitBehavior() const
	{ ASSERT_VALID(this); return m_nCursorCommitBehavior; }
_AFXDBCORE_INLINE int CDatabase::GetCursorRollbackBehavior() const
	{ ASSERT_VALID(this); return m_nCursorRollbackBehavior; }
_AFXDBCORE_INLINE void CDatabase::ThrowDBException(RETCODE nRetCode)
	{ ASSERT_VALID(this); AfxThrowDBException(nRetCode, this, m_hstmt); }
_AFXDBCORE_INLINE void CDatabase::SetSynchronousMode(BOOL /* bSynchronous */ )
	{ ASSERT_VALID(this); TRACE0("Error: SetSynchronousMode is obsolete and has not effect.\n"); }

// CRecordset inlines
_AFXDBCORE_INLINE const CString& CRecordset::GetSQL() const
	{ ASSERT(IsOpen()); return m_strSQL; }
_AFXDBCORE_INLINE const CString& CRecordset::GetTableName() const
	{ ASSERT(IsOpen()); return m_strTableName; }
_AFXDBCORE_INLINE BOOL CRecordset::IsBOF() const
	{ ASSERT(IsOpen()); return m_bBOF; }
_AFXDBCORE_INLINE BOOL CRecordset::IsEOF() const
	{ ASSERT(IsOpen()); return m_bEOF; }
_AFXDBCORE_INLINE BOOL CRecordset::IsDeleted() const
	{ ASSERT(IsOpen()); return m_bDeleted; }
_AFXDBCORE_INLINE BOOL CRecordset::CanUpdate() const
	{ ASSERT(IsOpen()); return m_bUpdatable; }
_AFXDBCORE_INLINE BOOL CRecordset::CanScroll() const
	{ ASSERT(IsOpen()); return m_bScrollable; }
_AFXDBCORE_INLINE BOOL CRecordset::CanAppend() const
	{ ASSERT(IsOpen()); return m_bAppendable; }
_AFXDBCORE_INLINE BOOL CRecordset::CanRestart() const
	{ ASSERT(IsOpen()); return !(m_dwOptions & executeDirect); }
_AFXDBCORE_INLINE BOOL CRecordset::CanTransact() const
	{ ASSERT(m_pDatabase->IsOpen());
		return m_pDatabase->m_bTransactions; }
_AFXDBCORE_INLINE short CRecordset::GetODBCFieldCount() const
	{ ASSERT(IsOpen()); return m_nResultCols; }
_AFXDBCORE_INLINE DWORD CRecordset::GetRowsetSize() const
	{ ASSERT(IsOpen()); return m_dwRowsetSize; }
_AFXDBCORE_INLINE DWORD CRecordset::GetRowsFetched() const
	{ ASSERT(IsOpen()); return m_dwRowsFetched; }
_AFXDBCORE_INLINE WORD CRecordset::GetRowStatus(WORD wRow) const
	{ ASSERT(IsOpen()); ASSERT(wRow > 0); return m_rgRowStatus[wRow-1]; }
_AFXDBCORE_INLINE long CRecordset::GetRecordCount() const
	{ ASSERT(IsOpen()); return m_lRecordCount; }
_AFXDBCORE_INLINE void CRecordset::GetStatus(CRecordsetStatus& rStatus) const
	{ ASSERT(IsOpen());
		rStatus.m_lCurrentRecord = m_lCurrentRecord;
		rStatus.m_bRecordCountFinal = m_bEOFSeen; }
_AFXDBCORE_INLINE void CRecordset::ThrowDBException(RETCODE nRetCode, HSTMT hstmt)
	{ ASSERT_VALID(this); AfxThrowDBException(nRetCode, m_pDatabase,
		(hstmt == SQL_NULL_HSTMT)? m_hstmt : hstmt); }
_AFXDBCORE_INLINE void CRecordset::MoveNext()
	{ ASSERT(IsOpen()); Move(1, SQL_FETCH_NEXT); }
_AFXDBCORE_INLINE void CRecordset::MovePrev()
	{ ASSERT(IsOpen()); Move(-1, SQL_FETCH_PRIOR); }
_AFXDBCORE_INLINE void CRecordset::MoveFirst()
	{ ASSERT(IsOpen()); Move(1, SQL_FETCH_FIRST); }
_AFXDBCORE_INLINE void CRecordset::MoveLast()
	{ ASSERT(IsOpen()); Move(-1, SQL_FETCH_LAST); }
_AFXDBCORE_INLINE void CRecordset::SetAbsolutePosition(long nRows)
	{ ASSERT(IsOpen()); Move(nRows, SQL_FETCH_ABSOLUTE); }

#endif //_AFXDBCORE_INLINE

#ifdef _AFXDBRFX_INLINE

_AFXDBRFX_INLINE void CFieldExchange::SetFieldType(UINT nFieldType)
	{ ASSERT(nFieldType == outputColumn ||
		nFieldType == inputParam ||
		nFieldType == outputParam ||
		nFieldType == inoutParam);
		m_nFieldType = nFieldType; }

#endif //_AFXDBRFX_INLINE

#ifdef _AFXDBVIEW_INLINE

// CRecordView inlines
_AFXDBVIEW_INLINE void CRecordView::OnMove(int cx, int cy)
	{ CFormView::OnMove(cx, cy); }

#endif

/////////////////////////////////////////////////////////////////////////////
