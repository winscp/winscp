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

#ifdef AFX_DB_SEG
#pragma code_seg(AFX_DB_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Global data

#ifdef _DEBUG
BOOL bTraceSql = FALSE;
#endif

AFX_STATIC_DATA const TCHAR _afxODBCTrail[] = _T("ODBC;");
AFX_STATIC_DATA const TCHAR _afxComma[] = _T(",");
AFX_STATIC_DATA const TCHAR _afxLiteralSeparator = '\'';
AFX_STATIC_DATA const TCHAR _afxCall[] = _T("{CALL ");
AFX_STATIC_DATA const TCHAR _afxParamCall[] = _T("{?");
AFX_STATIC_DATA const TCHAR _afxSelect[] = _T("SELECT ");
AFX_STATIC_DATA const TCHAR _afxFrom[] = _T(" FROM ");
AFX_STATIC_DATA const TCHAR _afxWhere[] = _T(" WHERE ");
AFX_STATIC_DATA const TCHAR _afxOrderBy[] = _T(" ORDER BY ");
AFX_STATIC_DATA const TCHAR _afxForUpdate[] = _T(" FOR UPDATE ");

AFX_STATIC_DATA const TCHAR _afxRowFetch[] = _T("State:01S01");
AFX_STATIC_DATA const TCHAR _afxDataTruncated[] = _T("State:01004");
AFX_STATIC_DATA const TCHAR _afxInfoRange[] = _T("State:S1096");
AFX_STATIC_DATA const TCHAR _afxOutOfSequence[] = _T("State:S1010");
AFX_STATIC_DATA const TCHAR _afxDriverNotCapable[] = _T("State:S1C00");

AFX_STATIC_DATA const char _afxODBCDLL[] = "ODBC32.DLL";

/////////////////////////////////////////////////////////////////////////////
// for dynamic load of ODBC32.DLL

#pragma comment(lib, "odbc32.lib")

/////////////////////////////////////////////////////////////////////////////
// CDBException

void AFXAPI AfxThrowDBException(RETCODE nRetCode, CDatabase* pdb, HSTMT hstmt)
{
	CDBException* pException = new CDBException(nRetCode);
	if (nRetCode == SQL_ERROR && pdb != NULL)
		pException->BuildErrorString(pdb, hstmt);
	else if (nRetCode > AFX_SQL_ERROR && nRetCode < AFX_SQL_ERROR_MAX)
	{
		VERIFY(pException->m_strError.LoadString(
			AFX_IDP_SQL_FIRST+(nRetCode-AFX_SQL_ERROR)));
		TRACE1("%s\n", pException->m_strError);
	}
	THROW(pException);
}

CDBException::CDBException(RETCODE nRetCode)
{
	m_nRetCode = nRetCode;
}

CDBException::~CDBException()
{
}

void CDBException::BuildErrorString(CDatabase* pdb, HSTMT hstmt, BOOL bTrace)
{
	ASSERT_VALID(this);
	UNUSED(bTrace);  // unused in release builds

	if (pdb != NULL)
	{
		SWORD nOutlen;
		RETCODE nRetCode;
		UCHAR lpszMsg[SQL_MAX_MESSAGE_LENGTH];
		UCHAR lpszState[SQL_SQLSTATE_SIZE];
		CString strMsg;
		CString strState;
		SDWORD lNative;

		_AFX_DB_STATE* pDbState = _afxDbState;
		AFX_SQL_SYNC(::SQLError(pDbState->m_henvAllConnections, pdb->m_hdbc,
			hstmt, lpszState, &lNative,
			lpszMsg, SQL_MAX_MESSAGE_LENGTH-1, &nOutlen));
		strState = lpszState;

		// Skip non-errors
		while ((nRetCode == SQL_SUCCESS || nRetCode == SQL_SUCCESS_WITH_INFO) &&
			lstrcmp(strState, _T("00000")) != 0)
		{
			strMsg = lpszMsg;

			TCHAR lpszNative[50];
			wsprintf(lpszNative, _T(",Native:%ld,Origin:"), lNative);
			strState += lpszNative;

			// transfer [origin] from message string to StateNativeOrigin string
			int nCloseBracket;
			int nMsgLength;
			while (!strMsg.IsEmpty() &&
				strMsg[0] == '[' && (nCloseBracket = strMsg.Find(']')) >= 0)
			{
				// Skip ']'
				nCloseBracket++;
				strState += strMsg.Left(nCloseBracket);

				nMsgLength = strMsg.GetLength();
				// Skip ' ', if present
				if (nCloseBracket < nMsgLength && strMsg[nCloseBracket] == ' ')
					nCloseBracket++;
				strMsg = strMsg.Right(nMsgLength - nCloseBracket);
			}
			strState += _T("\n");
			m_strStateNativeOrigin += _T("State:") + strState;
			m_strError += strMsg + _T("\n");

#ifdef _DEBUG
			if (bTrace)
			{
				TraceErrorMessage(strMsg);
				TraceErrorMessage(_T("State:") + strState);
			}
#endif // _DEBUG

			AFX_SQL_SYNC(::SQLError(pDbState->m_henvAllConnections,
				pdb->m_hdbc, hstmt, lpszState, &lNative,
				lpszMsg, SQL_MAX_MESSAGE_LENGTH-1, &nOutlen));
			strState = lpszState;
		}
	}
}


BOOL CDBException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext /* = NULL */)
{
	ASSERT(lpszError != NULL && AfxIsValidString(lpszError, nMaxError));

	if (pnHelpContext != NULL)
		*pnHelpContext = 0;

	lstrcpyn(lpszError, m_strError, nMaxError-1);
	lpszError[nMaxError-1] = '\0';
	return TRUE;
}


#ifdef _DEBUG
void CDBException::TraceErrorMessage(LPCTSTR szTrace) const
{
	CString strTrace = szTrace;

	if (strTrace.GetLength() <= 80)
		TRACE1("%s\n", strTrace);
	else
	{
		// Display 80 chars/line
		while (strTrace.GetLength() > 80)
		{
			TRACE1("%s\n", strTrace.Left(80));
			strTrace = strTrace.Right(strTrace.GetLength() - 80);
		}
		TRACE1("%s\n", strTrace);
	}
}
#endif // _DEBUG

void CDBException::Empty()
{
	m_strError.Empty();
	m_strStateNativeOrigin.Empty();
}

/////////////////////////////////////////////////////////////////////////////
// Global helper

HENV AFXAPI AfxGetHENV()
{
	_AFX_DB_STATE* pDbState = _afxDbState;
	return pDbState->m_henvAllConnections;
}

/////////////////////////////////////////////////////////////////////////////
// CDatabase implementation

CDatabase::CDatabase()
{
	m_hdbc = SQL_NULL_HDBC;
	m_hstmt = SQL_NULL_HSTMT;

	m_bUpdatable = FALSE;
	m_bTransactions = FALSE;
	DEBUG_ONLY(m_bTransactionPending = FALSE);
	m_dwLoginTimeout = DEFAULT_LOGIN_TIMEOUT;
	m_dwQueryTimeout = DEFAULT_QUERY_TIMEOUT;

	m_bStripTrailingSpaces = FALSE;
	m_bIncRecordCountOnAdd = FALSE;
	m_bAddForUpdate = FALSE;
}

CDatabase::~CDatabase()
{
	ASSERT_VALID(this);

	Free();
}

BOOL CDatabase::Open(LPCTSTR lpszDSN, BOOL bExclusive,
	BOOL bReadonly, LPCTSTR lpszConnect, BOOL bUseCursorLib)
{
	ASSERT(lpszDSN == NULL || AfxIsValidString(lpszDSN));
	ASSERT(lpszConnect == NULL || AfxIsValidString(lpszConnect));

	CString strConnect;

	if (lpszConnect != NULL)
		strConnect = lpszConnect;

	// For VB/Access compatibility, require "ODBC;" (or "odbc;")
	// prefix to the connect string
	if (_tcsnicmp(strConnect, _afxODBCTrail, lstrlen(_afxODBCTrail)) != 0)
	{
		TRACE0("Error: Missing 'ODBC' prefix on connect string.\n");
		return FALSE;
	}

	// Strip "ODBC;"
	strConnect = strConnect.Right(strConnect.GetLength()
		- lstrlen(_afxODBCTrail));

	if (lpszDSN != NULL && lstrlen(lpszDSN) != 0)
	{
		// Append "DSN=" lpszDSN
		strConnect += _T(";DSN=");
		strConnect += lpszDSN;
	}

	DWORD dwOptions = 0;

	if (bExclusive)
		dwOptions |= openExclusive;

	if (bReadonly)
		dwOptions |= openReadOnly;

	if (bUseCursorLib)
		dwOptions |= useCursorLib;

	return OpenEx(strConnect, dwOptions);
}

BOOL CDatabase::OpenEx(LPCTSTR lpszConnectString, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(lpszConnectString == NULL || AfxIsValidString(lpszConnectString));
	ASSERT(!(dwOptions & noOdbcDialog && dwOptions & forceOdbcDialog));

	// Exclusive access not supported.
	ASSERT(!(dwOptions & openExclusive));

	m_bUpdatable = !(dwOptions & openReadOnly);

	TRY
	{
		m_strConnect = lpszConnectString;

		// Allocate the HDBC and make connection
		AllocConnect(dwOptions);
		if(!Connect(dwOptions))
			return FALSE;

		// Verify support for required functionality and cache info
		VerifyConnect();
		GetConnectInfo();
	}
	CATCH_ALL(e)
	{
		Free();
		THROW_LAST();
	}
	END_CATCH_ALL

	return TRUE;
}

void CDatabase::ExecuteSQL(LPCTSTR lpszSQL)
{
	USES_CONVERSION;
	RETCODE nRetCode;
	HSTMT hstmt;

	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszSQL));

	AFX_SQL_SYNC(::SQLAllocStmt(m_hdbc, &hstmt));
	if (!CheckHstmt(nRetCode, hstmt))
		AfxThrowDBException(nRetCode, this, hstmt);

	TRY
	{
		OnSetOptions(hstmt);

		// Give derived CDatabase classes option to use parameters
		BindParameters(hstmt);

		AFX_ODBC_CALL(::SQLExecDirect(hstmt,
			(UCHAR*)T2A((LPTSTR)lpszSQL), SQL_NTS));

		if (!CheckHstmt(nRetCode, hstmt))
			AfxThrowDBException(nRetCode, this, hstmt);
		else
		{
			do
			{
				SWORD nResultColumns;

				AFX_ODBC_CALL(::SQLNumResultCols(hstmt, &nResultColumns));
				if (nResultColumns != 0)
				{
					do
					{
						AFX_ODBC_CALL(::SQLFetch(hstmt));
					} while (CheckHstmt(nRetCode, hstmt) &&
						nRetCode != SQL_NO_DATA_FOUND);
				}
				AFX_ODBC_CALL(::SQLMoreResults(hstmt));
			} while (CheckHstmt(nRetCode, hstmt) &&
				nRetCode != SQL_NO_DATA_FOUND);
		}
	}
	CATCH_ALL(e)
	{
		::SQLCancel(hstmt);
		AFX_SQL_SYNC(::SQLFreeStmt(hstmt, SQL_DROP));
		THROW_LAST();
	}
	END_CATCH_ALL

	AFX_SQL_SYNC(::SQLFreeStmt(hstmt, SQL_DROP));
}

// Shutdown pending query for CDatabase's private m_hstmt
void CDatabase::Cancel()
{
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	::SQLCancel(m_hstmt);
}

// Disconnect connection
void CDatabase::Close()
{
	ASSERT_VALID(this);

	// Close any open recordsets
	AfxLockGlobals(CRIT_ODBC);
	TRY
	{
		while (!m_listRecordsets.IsEmpty())
		{
			CRecordset* pSet = (CRecordset*)m_listRecordsets.GetHead();
			pSet->Close();  // will implicitly remove from list
			pSet->m_pDatabase = NULL;
		}
	}
	CATCH_ALL(e)
	{
		AfxUnlockGlobals(CRIT_ODBC);
		THROW_LAST();
	}
	END_CATCH_ALL
	AfxUnlockGlobals(CRIT_ODBC);

	if (m_hdbc != SQL_NULL_HDBC)
	{
		RETCODE nRetCode;
		AFX_SQL_SYNC(::SQLDisconnect(m_hdbc));
		AFX_SQL_SYNC(::SQLFreeConnect(m_hdbc));
		m_hdbc = SQL_NULL_HDBC;

		_AFX_DB_STATE* pDbState = _afxDbState;

		AfxLockGlobals(CRIT_ODBC);
		ASSERT(pDbState->m_nAllocatedConnections != 0);
		pDbState->m_nAllocatedConnections--;
		AfxUnlockGlobals(CRIT_ODBC);
	}
}

// Silently disconnect and free all ODBC resources.  Don't throw any exceptions
void CDatabase::Free()
{
	ASSERT_VALID(this);

	// Trap failures upon close
	TRY
	{
		Close();
	}
	CATCH_ALL(e)
	{
		// Nothing we can do
		TRACE0("Error: exception by CDatabase::Close() ignored in CDatabase::Free().\n");
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	// free henv if refcount goes to 0
	_AFX_DB_STATE* pDbState = _afxDbState;
	AfxLockGlobals(CRIT_ODBC);
	if (pDbState->m_henvAllConnections != SQL_NULL_HENV)
	{
		ASSERT(pDbState->m_nAllocatedConnections >= 0);
		if (pDbState->m_nAllocatedConnections == 0)
		{
			// free last connection - release HENV
			RETCODE nRetCodeEnv = ::SQLFreeEnv(pDbState->m_henvAllConnections);
#ifdef _DEBUG
			if (nRetCodeEnv != SQL_SUCCESS)
				// Nothing we can do
				TRACE0("Error: SQLFreeEnv failure ignored in CDatabase::Free().\n");
#endif
			pDbState->m_henvAllConnections = SQL_NULL_HENV;
		}
	}
	AfxUnlockGlobals(CRIT_ODBC);
}

void CDatabase::OnSetOptions(HSTMT hstmt)
{
	RETCODE nRetCode;
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	if (m_dwQueryTimeout != -1)
	{
		// Attempt to set query timeout.  Ignore failure
		AFX_SQL_SYNC(::SQLSetStmtOption(hstmt, SQL_QUERY_TIMEOUT,
			m_dwQueryTimeout));
		if (!Check(nRetCode))
			// don't attempt it again
			m_dwQueryTimeout = (DWORD)-1;
	}
}

CString CDatabase::GetDatabaseName() const
{
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	char szName[MAX_TNAME_LEN];
	SWORD nResult;
	RETCODE nRetCode;

	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_DATABASE_NAME,
		szName, _countof(szName), &nResult));
	if (!Check(nRetCode))
		szName[0] = '\0';

	return szName;
}

BOOL CDatabase::BeginTrans()
{
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	if (!m_bTransactions)
		return FALSE;

	// Only 1 level of transactions supported
#ifdef _DEBUG
	ASSERT(!m_bTransactionPending);
#endif

	RETCODE nRetCode;
	AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc, SQL_AUTOCOMMIT,
		SQL_AUTOCOMMIT_OFF));
	DEBUG_ONLY(m_bTransactionPending = TRUE);

	return Check(nRetCode);
}

BOOL CDatabase::CommitTrans()
{
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	if (!m_bTransactions)
		return FALSE;

	// BeginTrans must be called first
#ifdef _DEBUG
	ASSERT(m_bTransactionPending);
#endif

	_AFX_DB_STATE* pDbState = _afxDbState;
	RETCODE nRetCode;
	AFX_SQL_SYNC(::SQLTransact(pDbState->m_henvAllConnections, m_hdbc, SQL_COMMIT));
	BOOL bSuccess = Check(nRetCode);

	// Turn back on auto commit
	AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc, SQL_AUTOCOMMIT,
		SQL_AUTOCOMMIT_ON));
	DEBUG_ONLY(m_bTransactionPending = FALSE);

	return bSuccess;
}

BOOL CDatabase::Rollback()
{
	ASSERT_VALID(this);
	ASSERT(m_hdbc != SQL_NULL_HDBC);

	if (!m_bTransactions)
		return FALSE;

	// BeginTrans must be called first
#ifdef _DEBUG
	ASSERT(m_bTransactionPending);
#endif

	_AFX_DB_STATE* pDbState = _afxDbState;
	RETCODE nRetCode;
	AFX_SQL_SYNC(::SQLTransact(pDbState->m_henvAllConnections, m_hdbc, SQL_ROLLBACK));
	BOOL bSuccess = Check(nRetCode);

	// Turn back on auto commit
	AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc, SQL_AUTOCOMMIT,
		SQL_AUTOCOMMIT_ON));
	DEBUG_ONLY(m_bTransactionPending = FALSE);

	return bSuccess;
}

// Screen for errors.
BOOL CDatabase::Check(RETCODE nRetCode) const
{
	return CheckHstmt(nRetCode, SQL_NULL_HSTMT);
}

BOOL CDatabase::CheckHstmt(RETCODE nRetCode, HSTMT hstmt) const
{
	ASSERT_VALID(this);
	UNUSED(hstmt);

	switch (nRetCode)
	{
	case SQL_SUCCESS_WITH_INFO:
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
		{
			CDBException e(nRetCode);
			TRACE0("Warning: ODBC Success With Info, ");
			e.BuildErrorString((CDatabase*)this, hstmt);
		}
#endif // _DEBUG

		// Fall through

	case SQL_SUCCESS:
	case SQL_NO_DATA_FOUND:
		return TRUE;
	}

	return FALSE;
}

//////////////////////////////////////////////////////////////////////////////
// CDatabase internal functions

//Replace brackets in SQL string with SQL_IDENTIFIER_QUOTE_CHAR
void CDatabase::ReplaceBrackets(LPTSTR lpchSQL)
{
	BOOL bInLiteral = FALSE;
	LPTSTR lpchNewSQL = lpchSQL;

	while (*lpchSQL != '\0')
	{
		if (*lpchSQL == _afxLiteralSeparator)
			{
				// Handle escaped literal
				if (*_tcsinc(lpchSQL) == _afxLiteralSeparator)
				{
					*lpchNewSQL = *lpchSQL;
					lpchSQL = _tcsinc(lpchSQL);
					lpchNewSQL = _tcsinc(lpchNewSQL);
				}
				else
					bInLiteral = !bInLiteral;

				*lpchNewSQL = *lpchSQL;
			}
		else if (!bInLiteral && (*lpchSQL == '['))
		{
			if (*_tcsinc(lpchSQL) == '[')
			{
				// Handle escaped left bracket by inserting one '['
				*lpchNewSQL = *lpchSQL;
				lpchSQL = _tcsinc(lpchSQL);
			}
			else
				*lpchNewSQL = m_chIDQuoteChar;
		}
		else if (!bInLiteral && (*lpchSQL == ']'))
		{
			if (*_tcsinc(lpchSQL) == ']')
			{
				// Handle escaped right bracket by inserting one ']'
				*lpchNewSQL = *lpchSQL;
				lpchSQL = _tcsinc(lpchSQL);
			}
			else
				*lpchNewSQL = m_chIDQuoteChar;
		}
		else
			*lpchNewSQL = *lpchSQL;

		lpchSQL = _tcsinc(lpchSQL);
		lpchNewSQL = _tcsinc(lpchNewSQL);
	}
}

// Allocate an henv (first time called) and hdbc
void CDatabase::AllocConnect(DWORD dwOptions)
{
	ASSERT_VALID(this);

	if (m_hdbc != SQL_NULL_HDBC)
		return;

	_AFX_DB_STATE* pDbState = _afxDbState;

	RETCODE nRetCode;

	AfxLockGlobals(CRIT_ODBC);
	if (pDbState->m_henvAllConnections == SQL_NULL_HENV)
	{
		ASSERT(pDbState->m_nAllocatedConnections == 0);

		// need to allocate an environment for first connection
		AFX_SQL_SYNC(::SQLAllocEnv(&pDbState->m_henvAllConnections));
		if (!Check(nRetCode))
		{
			AfxUnlockGlobals(CRIT_ODBC);
			AfxThrowMemoryException();  // fatal
		}
	}

	ASSERT(pDbState->m_henvAllConnections != SQL_NULL_HENV);
	AFX_SQL_SYNC(::SQLAllocConnect(pDbState->m_henvAllConnections, &m_hdbc));
	if (!Check(nRetCode))
	{
		AfxUnlockGlobals(CRIT_ODBC);
		ThrowDBException(nRetCode); // fatal
	}
	pDbState->m_nAllocatedConnections++;    // allocated at least
	AfxUnlockGlobals(CRIT_ODBC);

#ifdef _DEBUG
	if (bTraceSql)
	{
		::SQLSetConnectOption(m_hdbc, SQL_OPT_TRACEFILE,
			(DWORD)"odbccall.txt");
		::SQLSetConnectOption(m_hdbc, SQL_OPT_TRACE, 1);
	}
#endif // _DEBUG

	AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc, SQL_LOGIN_TIMEOUT,
		m_dwLoginTimeout));
#ifdef _DEBUG
	if (nRetCode != SQL_SUCCESS && nRetCode != SQL_SUCCESS_WITH_INFO &&
		(afxTraceFlags & traceDatabase))
		TRACE0("Warning: Failure setting login timeout.\n");
#endif

	if (!m_bUpdatable)
	{
		AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc, SQL_ACCESS_MODE,
			SQL_MODE_READ_ONLY));
#ifdef _DEBUG
		if (nRetCode != SQL_SUCCESS && nRetCode != SQL_SUCCESS_WITH_INFO &&
			(afxTraceFlags & traceDatabase))
			TRACE0("Warning: Failure setting read only access mode.\n");
#endif
	}

	// Turn on cursor lib support
	if (dwOptions & useCursorLib)
	{
		AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc,
			SQL_ODBC_CURSORS, SQL_CUR_USE_ODBC));
		// With cursor library added records immediately in result set
		m_bIncRecordCountOnAdd = TRUE;
	}
}

BOOL CDatabase::Connect(DWORD dwOptions)
{
	USES_CONVERSION;

	HWND hWndTop;
	HWND hWnd = CWnd::GetSafeOwner_(NULL, &hWndTop);
	if (hWnd == NULL)
		hWnd = ::GetDesktopWindow();

	UCHAR szConnectOutput[MAX_CONNECT_LEN];
	RETCODE nRetCode;
	SWORD nResult;
	UWORD wConnectOption = SQL_DRIVER_COMPLETE;
	if (dwOptions & noOdbcDialog)
		wConnectOption = SQL_DRIVER_NOPROMPT;
	else if (dwOptions & forceOdbcDialog)
		wConnectOption = SQL_DRIVER_PROMPT;
	AFX_SQL_SYNC(::SQLDriverConnect(m_hdbc, hWnd,
		(UCHAR*)T2A((LPTSTR)(LPCTSTR)m_strConnect), SQL_NTS,
		szConnectOutput, _countof(szConnectOutput),
		&nResult, wConnectOption));
	if (hWndTop != NULL)
		::EnableWindow(hWndTop, TRUE);

	// If user hit 'Cancel'
	if (nRetCode == SQL_NO_DATA_FOUND)
	{
		Free();
		return FALSE;
	}

	if (!Check(nRetCode))
	{
#ifdef _DEBUG
		if (hWnd == NULL)
			TRACE0("Error: No default window (AfxGetApp()->m_pMainWnd) for SQLDriverConnect.\n");
#endif
		ThrowDBException(nRetCode);
	}

	// Connect strings must have "ODBC;"
	m_strConnect = _afxODBCTrail;
	// Save connect string returned from ODBC
	m_strConnect += (char*)szConnectOutput;

	return TRUE;
}

void CDatabase::VerifyConnect()
{
	RETCODE nRetCode;
	SWORD nResult;

	SWORD nAPIConformance;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_ODBC_API_CONFORMANCE,
		&nAPIConformance, sizeof(nAPIConformance), &nResult));
	if (!Check(nRetCode))
		ThrowDBException(nRetCode);

	if (nAPIConformance < SQL_OAC_LEVEL1)
		ThrowDBException(AFX_SQL_ERROR_API_CONFORMANCE);

	SWORD nSQLConformance;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_ODBC_SQL_CONFORMANCE,
		&nSQLConformance, sizeof(nSQLConformance), &nResult));
	if (!Check(nRetCode))
		ThrowDBException(nRetCode);

	if (nSQLConformance < SQL_OSC_MINIMUM)
		ThrowDBException(AFX_SQL_ERROR_SQL_CONFORMANCE);
}

void CDatabase::GetConnectInfo()
{
	RETCODE nRetCode;
	SWORD nResult;

	// Reset the database update options
	m_dwUpdateOptions = 0;

	// Check for SQLSetPos support
	UDWORD dwDriverPosOperations;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_POS_OPERATIONS,
		&dwDriverPosOperations, sizeof(dwDriverPosOperations), &nResult));
	if (Check(nRetCode) &&
		(dwDriverPosOperations & SQL_POS_UPDATE) &&
		(dwDriverPosOperations & SQL_POS_DELETE) &&
		(dwDriverPosOperations & SQL_POS_ADD))
		m_dwUpdateOptions |= AFX_SQL_SETPOSUPDATES;

	// Check for positioned update SQL support
	UDWORD dwPositionedStatements;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_POSITIONED_STATEMENTS,
		&dwPositionedStatements, sizeof(dwPositionedStatements),
		&nResult));
	if (Check(nRetCode) &&
		(dwPositionedStatements & SQL_PS_POSITIONED_DELETE) &&
		(dwPositionedStatements & SQL_PS_POSITIONED_UPDATE))
		m_dwUpdateOptions |= AFX_SQL_POSITIONEDSQL;

	// Check for transaction support
	SWORD nTxnCapable;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_TXN_CAPABLE, &nTxnCapable,
		sizeof(nTxnCapable), &nResult));
	if (Check(nRetCode) && nTxnCapable != SQL_TC_NONE)
		m_bTransactions = TRUE;

	// Cache the effect of transactions on cursors
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_CURSOR_COMMIT_BEHAVIOR,
		&m_nCursorCommitBehavior, sizeof(m_nCursorCommitBehavior),
		&nResult));
	if (!Check(nRetCode))
		m_nCursorCommitBehavior = SQL_ERROR;

	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_CURSOR_ROLLBACK_BEHAVIOR,
		&m_nCursorRollbackBehavior, sizeof(m_nCursorRollbackBehavior),
		&nResult));
	if (!Check(nRetCode))
		m_nCursorRollbackBehavior = SQL_ERROR;

	// Cache bookmark attributes
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_BOOKMARK_PERSISTENCE,
		&m_dwBookmarkAttributes, sizeof(m_dwBookmarkAttributes),
		&nResult));
	Check(nRetCode);

	// Check for SQLGetData support req'd by RFX_LongBinary
	UDWORD dwGetDataExtensions;
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_GETDATA_EXTENSIONS,
		&dwGetDataExtensions, sizeof(dwGetDataExtensions),
		&nResult));
	if (!Check(nRetCode))
		dwGetDataExtensions = 0;
	if (dwGetDataExtensions & SQL_GD_BOUND)
		m_dwUpdateOptions |= AFX_SQL_GDBOUND;

	if (m_bUpdatable)
	{
		// Make sure data source is Updatable
		char szReadOnly[10];
		AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_DATA_SOURCE_READ_ONLY,
			szReadOnly, _countof(szReadOnly), &nResult));
		if (Check(nRetCode) && nResult == 1)
			m_bUpdatable = !(lstrcmpA(szReadOnly, "Y") == 0);
		else
			m_bUpdatable = FALSE;
#ifdef _DEBUG
		if (!m_bUpdatable && (afxTraceFlags & traceDatabase))
			TRACE0("Warning: data source is readonly.\n");
#endif
	}
	else
	{
		// Make data source is !Updatable
		AFX_SQL_SYNC(::SQLSetConnectOption(m_hdbc,
			SQL_ACCESS_MODE, SQL_MODE_READ_ONLY));
	}

	// Cache the quote char to use when constructing SQL
	char szIDQuoteChar[2];
	AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_IDENTIFIER_QUOTE_CHAR,
		szIDQuoteChar, _countof(szIDQuoteChar), &nResult));
	if (Check(nRetCode) && nResult == 1)
		m_chIDQuoteChar = szIDQuoteChar[0];
	else
		m_chIDQuoteChar = ' ';

#ifdef _DEBUG
	if (afxTraceFlags & traceDatabase)
	{
		char szInfo[64];
		AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_DBMS_NAME,
			szInfo, _countof(szInfo), &nResult));
		if (Check(nRetCode))
		{
			CString strInfo = szInfo;
			TRACE1("DBMS: %s\n", strInfo);
			AFX_SQL_SYNC(::SQLGetInfo(m_hdbc, SQL_DBMS_VER,
				szInfo, _countof(szInfo), &nResult));
			if (Check(nRetCode))
			{
				strInfo = szInfo;
				TRACE1(", Version: %s\n", strInfo);
			}
		}
	}
#endif // _DEBUG
}

void CDatabase::BindParameters(HSTMT /* hstmt */)
{
	// Must override and call SQLBindParameter directly
}

//////////////////////////////////////////////////////////////////////////////
// CDatabase diagnostics

#ifdef _DEBUG
void CDatabase::AssertValid() const
{
	CObject::AssertValid();
}

void CDatabase::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_hdbc = " << m_hdbc;
	dc << "\nm_strConnect = " << m_strConnect;
	dc << "\nm_bUpdatable = " << m_bUpdatable;
	dc << "\nm_bTransactions = " << m_bTransactions;
	dc << "\nm_bTransactionPending = " << m_bTransactionPending;
	dc << "\nm_dwLoginTimeout = " << m_dwLoginTimeout;
	dc << "\nm_dwQueryTimeout = " << m_dwQueryTimeout;

	if (dc.GetDepth() > 0)
	{
		_AFX_DB_STATE* pDbState = _afxDbState;
		dc << "\nwith env:";
		dc << "\n\tnAllocated = " << pDbState->m_nAllocatedConnections;
		dc << "\n\thenvAllConnections = " << pDbState->m_henvAllConnections;
	}

	dc << "\n";
}

#endif // _DEBUG


//////////////////////////////////////////////////////////////////////////////
// CRecordset helpers

void AFXAPI AfxSetCurrentRecord(long* plCurrentRecord, long nRows, RETCODE nRetCode);
void AFXAPI AfxSetRecordCount(long* plRecordCount, long lCurrentRecord,
	BOOL bEOFSeen, RETCODE nRetCode);

//////////////////////////////////////////////////////////////////////////////
// CRecordset

CRecordset::CRecordset(CDatabase* pDatabase)
{
	ASSERT(pDatabase == NULL || AfxIsValidAddress(pDatabase, sizeof(CDatabase)));
	m_pDatabase = pDatabase;

	m_nOpenType = snapshot;
	m_lOpen = AFX_RECORDSET_STATUS_UNKNOWN;
	m_nEditMode = noMode;
	m_nDefaultType = snapshot;
	m_dwOptions = none;

	m_bAppendable = FALSE;
	m_bUpdatable = FALSE;
	m_bScrollable = FALSE;
	m_bRecordsetDb = FALSE;
	m_bRebindParams = FALSE;
	m_bLongBinaryColumns = FALSE;
	m_nLockMode = optimistic;
	m_dwInitialGetDataLen = 0;
	m_rgODBCFieldInfos = NULL;
	m_rgFieldInfos = NULL;
	m_rgRowStatus = NULL;
	m_dwRowsetSize = 25;
	m_dwAllocatedRowsetSize = 0;

	m_nFields = 0;
	m_nParams = 0;
	m_nFieldsBound = 0;
	m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
	m_lRecordCount = 0;
	m_bUseUpdateSQL = FALSE;
	m_bUseODBCCursorLib = FALSE;
	m_nResultCols = -1;
	m_bCheckCacheForDirtyFields = TRUE;

	m_pbFieldFlags = NULL;
	m_pbParamFlags = NULL;
	m_plParamLength = NULL;
	m_pvFieldProxy = NULL;
	m_pvParamProxy = NULL;
	m_nProxyFields = 0;
	m_nProxyParams = 0;

	m_hstmtUpdate = SQL_NULL_HSTMT;
	m_hstmt = SQL_NULL_HSTMT;
	if (m_pDatabase != NULL && m_pDatabase->IsOpen())
	{
		ASSERT_VALID(m_pDatabase);
		TRY
		{
			RETCODE nRetCode;
			AFX_SQL_SYNC(::SQLAllocStmt(m_pDatabase->m_hdbc, &m_hstmt));
			if (!Check(nRetCode))
				ThrowDBException(SQL_INVALID_HANDLE);

			// Add to list of CRecordsets with alloced hstmts
			AfxLockGlobals(CRIT_ODBC);
			TRY
			{
				m_pDatabase->m_listRecordsets.AddHead(this);
			}
			CATCH_ALL(e)
			{
				AfxUnlockGlobals(CRIT_ODBC);
				THROW_LAST();
			}
			END_CATCH_ALL
			AfxUnlockGlobals(CRIT_ODBC);
		}
		CATCH_ALL(e)
		{
			ASSERT(m_hstmt == SQL_NULL_HSTMT);
			DELETE_EXCEPTION(e);
		}
		END_CATCH_ALL
	}
}

CRecordset::~CRecordset()
{
	ASSERT_VALID(this);

	TRY
	{
		if (m_hstmt != NULL)
		{
#ifdef _DEBUG
			if (m_dwOptions & useMultiRowFetch && afxTraceFlags & traceDatabase)
			{
				TRACE0("\nWARNING: Close called implicitly from destructor.");
				TRACE0("\nUse of multi row fetch requires explicit call");
				TRACE0("\nto Close or memory leaks will result.\n");
			}
#endif
			Close();
		}
		if (m_bRecordsetDb)
			delete m_pDatabase;
		m_pDatabase = NULL;
	}
	CATCH_ALL(e)
	{
		// Nothing we can do
		TRACE0("Error: Exception ignored in ~CRecordset().\n");
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL
}

BOOL CRecordset::Open(UINT nOpenType, LPCTSTR lpszSQL, DWORD dwOptions)
{
	ASSERT(!IsOpen());
	ASSERT_VALID(this);
	ASSERT(lpszSQL == NULL || AfxIsValidString(lpszSQL));
	ASSERT(nOpenType == AFX_DB_USE_DEFAULT_TYPE ||
		nOpenType == dynaset || nOpenType == snapshot ||
		nOpenType == forwardOnly || nOpenType == dynamic);
	ASSERT(!(dwOptions & readOnly && dwOptions & appendOnly));

	// Can only use optimizeBulkAdd with appendOnly recordsets
	ASSERT((dwOptions & optimizeBulkAdd && dwOptions & appendOnly) ||
		!(dwOptions & optimizeBulkAdd));

	// forwardOnly recordsets have limited functionality
	ASSERT(!(nOpenType == forwardOnly && dwOptions & skipDeletedRecords));

	// Cache state info and allocate hstmt
	SetState(nOpenType, lpszSQL, dwOptions);
	if(!AllocHstmt())
		return FALSE;

	// Check if bookmarks upported (CanBookmark depends on open DB)
	ASSERT(dwOptions & useBookmarks ? CanBookmark() : TRUE);

	TRY
	{
		OnSetOptions(m_hstmt);

		// Allocate the field/param status arrays, if necessary
		BOOL bUnbound = FALSE;
		if (m_nFields > 0 || m_nParams > 0)
			AllocStatusArrays();
		else
			bUnbound = TRUE;

		// Build SQL and prep/execute or just execute direct
		BuildSQL(lpszSQL);
		PrepareAndExecute();

		// Cache some field info and prepare the rowset
		AllocAndCacheFieldInfo();
		AllocRowset();

		// If late binding, still need to allocate status arrays
		if (bUnbound && (m_nFields > 0 || m_nParams > 0))
			AllocStatusArrays();

		// Give derived classes a call before binding
		PreBindFields();

		// Fetch the first row of data
		MoveNext();

		// If EOF, then result set empty, so set BOF as well
		m_bBOF = m_bEOF;
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL

	return TRUE;
}

void CRecordset::Close()
{
	ASSERT_VALID(this);
	// Can't close if database has been deleted
	ASSERT(m_pDatabase != NULL);

	// This will force a requery for cursor name if reopened.
	m_strCursorName.Empty();

	if (m_rgFieldInfos != NULL &&
		m_nFields > 0 && m_bCheckCacheForDirtyFields)
	{
		FreeDataCache();
	}

	FreeRowset();

	m_nEditMode = noMode;

	delete [] m_rgFieldInfos;
	m_rgFieldInfos = NULL;

	delete [] m_rgODBCFieldInfos;
	m_rgODBCFieldInfos = NULL;

	delete [] m_pbFieldFlags;
	m_pbFieldFlags = NULL;

	delete [] m_pbParamFlags;
	m_pbParamFlags = NULL;

	if (m_pvFieldProxy != NULL)
	{
		for (UINT nField = 0; nField < m_nProxyFields; nField++)
			delete m_pvFieldProxy[nField];

		delete [] m_pvFieldProxy;
		m_pvFieldProxy = NULL;
		m_nProxyFields = 0;
	}

	if (m_pvParamProxy != NULL)
	{
		for (UINT nParam = 0; nParam < m_nProxyParams; nParam++)
			delete m_pvParamProxy[nParam];

		delete [] m_pvParamProxy;
		m_pvParamProxy = NULL;
		m_nProxyParams = 0;
	}

	delete [] m_plParamLength;
	m_plParamLength = NULL;

	RETCODE nRetCode;
	if (m_hstmt != SQL_NULL_HSTMT)
	{
		AFX_SQL_SYNC(::SQLFreeStmt(m_hstmt, SQL_DROP));
		m_hstmt = SQL_NULL_HSTMT;
	}

	if (m_hstmtUpdate != SQL_NULL_HSTMT)
	{
		AFX_SQL_SYNC(::SQLFreeStmt(m_hstmtUpdate, SQL_DROP));
		m_hstmtUpdate = SQL_NULL_HSTMT;
	}

	// Remove CRecordset from CDatabase's list
	AfxLockGlobals(CRIT_ODBC);
	TRY
	{
		POSITION pos = m_pDatabase->m_listRecordsets.Find(this);
		if (pos != NULL)
			m_pDatabase->m_listRecordsets.RemoveAt(pos);
#ifdef _DEBUG
		else if (afxTraceFlags & traceDatabase)
			TRACE0("WARNING: CRecordset not found in m_pDatabase->m_listRecordsets.\n");
#endif
	}
	CATCH_ALL(e)
	{
		AfxUnlockGlobals(CRIT_ODBC);
		THROW_LAST();
	}
	END_CATCH_ALL
	AfxUnlockGlobals(CRIT_ODBC);

	m_lOpen = AFX_RECORDSET_STATUS_CLOSED;
	m_bBOF = TRUE;
	m_bEOF = TRUE;
	m_bDeleted = FALSE;
	m_bAppendable = FALSE;
	m_bUpdatable = FALSE;
	m_bScrollable = FALSE;
	m_bRebindParams = FALSE;
	m_bLongBinaryColumns = FALSE;
	m_nLockMode = optimistic;

	m_nFieldsBound = 0;
	m_nResultCols = -1;
}

BOOL CRecordset::IsOpen() const
	// Note: assumes base class CRecordset::Close called
{
	if (m_hstmt == NULL)
		return FALSE;

	if (m_lOpen == AFX_RECORDSET_STATUS_OPEN)
		return TRUE;

	RETCODE nRetCode;
	SWORD nCols;

	AFX_ODBC_CALL(::SQLNumResultCols(m_hstmt, &nCols));

	if (!Check(nRetCode))
	{
		// If function sequence error, CRecordset not open
		CDBException* e = new CDBException(nRetCode);
		e->BuildErrorString(m_pDatabase, m_hstmt, FALSE);
		if (e->m_strStateNativeOrigin.Find(_afxOutOfSequence) >= 0)
		{
			e->Delete();
			return FALSE;
		}
		else
		{
#ifdef _DEBUG
			TRACE0("Error: SQLNumResultCols failed during IsOpen().\n");
			e->TraceErrorMessage(e->m_strError);
			e->TraceErrorMessage(e->m_strStateNativeOrigin);
#endif
			THROW(e);
		}
	}

	BOOL bOpen = FALSE;

	if (nCols != 0)
		bOpen = TRUE;

	return bOpen;
}

BOOL CRecordset::IsFieldDirty(void* pv)
{
	ASSERT_VALID(this);
	ASSERT(!(m_dwOptions & useMultiRowFetch));

	if (m_nFields <= 0)
	{
		ASSERT(FALSE);
		return FALSE;
	}

	// If not in update op fields can't be dirty
	// must compare saved and current values
	if (m_nEditMode == noMode)
		return FALSE;

	// Must compare values to find dirty fields if necessary
	if (m_bCheckCacheForDirtyFields)
	{
		if (m_nEditMode == edit)
			MarkForUpdate();
		else
			MarkForAddNew();
	}

	int nIndex = 0, nIndexEnd;

	if (pv == NULL)
		nIndexEnd = m_nFields - 1;
	else
	{
		// GetBoundFieldIndex returns 1-based index
		nIndex = nIndexEnd = GetBoundFieldIndex(pv) - 1;

		// must be address of field member
		ASSERT(nIndex >= 0);
	}

	BOOL bDirty = FALSE;

	while (nIndex <= nIndexEnd && !bDirty)
		bDirty = IsFieldStatusDirty(nIndex++);

	return bDirty;
}

BOOL CRecordset::IsFieldNull(void* pv)
{
	ASSERT_VALID(this);
	ASSERT(!(m_dwOptions & useMultiRowFetch));

	int nIndex;
	BOOL bRetVal;

	if (pv == NULL)
	{
		bRetVal = FALSE;
		for (nIndex = 0; !bRetVal && nIndex <= int(m_nFields-1); nIndex++)
			bRetVal = IsFieldStatusNull((DWORD) nIndex);
	}
	else
	{
		nIndex = GetBoundFieldIndex(pv) - 1;
		if (nIndex < 0)
			ThrowDBException(AFX_SQL_ERROR_FIELD_NOT_FOUND);
		bRetVal = IsFieldStatusNull((DWORD) nIndex);
	}

	return bRetVal;
}

BOOL CRecordset::IsFieldNullable(void* pv)
{
	ASSERT_VALID(this);

	if (pv == NULL)
	{
		// Must specify valid column name
		ASSERT(FALSE);
		return FALSE;
	}

	int nIndex = GetBoundFieldIndex(pv) - 1;
	if (nIndex < 0)
		ThrowDBException(AFX_SQL_ERROR_FIELD_NOT_FOUND);

	return IsFieldNullable((DWORD)nIndex);
}

BOOL CRecordset::CanBookmark() const
{
	ASSERT_VALID(this);
	ASSERT(m_pDatabase->IsOpen());

	if (!(m_dwOptions & useBookmarks) ||
		(m_nOpenType == forwardOnly && !(m_dwOptions & useExtendedFetch)))
		return FALSE;

	return m_pDatabase->GetBookmarkPersistence() & SQL_BP_SCROLL;
}

void CRecordset::Move(long nRows, WORD wFetchType)
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	// First call - fields haven't been bound (m_nFieldsBound will change)
	if (m_nFieldsBound == 0)
	{
		InitRecord();
		ResetCursor();
	}

	if (m_nFieldsBound > 0)
	{
		// Reset field flags - mark all clean, all non-null
		memset(m_pbFieldFlags, 0, m_nFields);

		// Clear any edit mode that was set
		m_nEditMode = noMode;
	}

	// Check scrollability, EOF/BOF status
	CheckRowsetCurrencyStatus(wFetchType, nRows);

	RETCODE nRetCode;

	// Fetch the data, skipping deleted records if necessary
	if ((wFetchType == SQL_FETCH_FIRST ||
		wFetchType == SQL_FETCH_LAST ||
		wFetchType == SQL_FETCH_NEXT ||
		wFetchType == SQL_FETCH_PRIOR ||
		wFetchType == SQL_FETCH_RELATIVE) &&
		m_dwOptions & skipDeletedRecords)
	{
		SkipDeletedRecords(wFetchType, nRows, &m_dwRowsFetched, &nRetCode);
	}
	else
		// Fetch the data and check for errors
		nRetCode = FetchData(wFetchType, nRows, &m_dwRowsFetched);

	// Set currency status and increment the record counters
	SetRowsetCurrencyStatus(nRetCode, wFetchType, nRows, m_dwRowsFetched);

	// Need to fixup bound fields in some cases
	if (m_nFields > 0 && !IsEOF() && !IsBOF() &&
		!(m_dwOptions & useMultiRowFetch))
	{
		Fixups();
	}
}

void CRecordset::CheckRowsetError(RETCODE nRetCode)
{
	if (nRetCode == SQL_SUCCESS_WITH_INFO)
	{
		CDBException e(nRetCode);
		// Build the error string but don't send nuisance output to TRACE window
		e.BuildErrorString(m_pDatabase, m_hstmt, FALSE);

		if (e.m_strStateNativeOrigin.Find(_afxDataTruncated) >= 0)
		{
			// Ignore data truncated warning if binding long binary columns
			// (may mask non-long binary truncation warnings or other warnings)
			if (!((m_pDatabase->m_dwUpdateOptions & AFX_SQL_SETPOSUPDATES) &&
				m_bLongBinaryColumns))
			{
				NO_CPP_EXCEPTION(e.Empty());
				TRACE0("Error: field data truncated during data fetch.\n");
				ThrowDBException(AFX_SQL_ERROR_DATA_TRUNCATED);
			}
		}
		else if (e.m_strStateNativeOrigin.Find(_afxRowFetch) >= 0)
		{
#ifdef _DEBUG
			TRACE0("Error: fetching row from server.\n");
			e.TraceErrorMessage(e.m_strError);
			e.TraceErrorMessage(e.m_strStateNativeOrigin);
#endif
			NO_CPP_EXCEPTION(e.Empty());
			ThrowDBException(AFX_SQL_ERROR_ROW_FETCH);
		}
		else
		{
#ifdef _DEBUG
			// Not a truncation or row fetch warning so send debug output
			if (afxTraceFlags & traceDatabase)
			{
				TRACE0("Warning: ODBC Success With Info,\n");
				e.TraceErrorMessage(e.m_strError);
				e.TraceErrorMessage(e.m_strStateNativeOrigin);
			}
#endif // _DEBUG
		}
	}
	else if (!Check(nRetCode))
		ThrowDBException(nRetCode);
}

void CRecordset::GetBookmark(CDBVariant& varBookmark)
{
	ASSERT_VALID(this);

	// Validate bookmarks are usable
	if (!(m_dwOptions & useBookmarks))
		ThrowDBException(AFX_SQL_ERROR_BOOKMARKS_NOT_ENABLED);
	else if (!CanBookmark())
		ThrowDBException(AFX_SQL_ERROR_BOOKMARKS_NOT_SUPPORTED);

	// Currently ODBC only supports 4 byte bookmarks
	// Initialize the variant to a long
	if (varBookmark.m_dwType != DBVT_LONG)
	{
		varBookmark.Clear();
		varBookmark.m_dwType = DBVT_LONG;
		varBookmark.m_lVal = 0;
	}

	RETCODE nRetCode;
	SDWORD nActualSize;

	// Retrieve the bookmark (column 0) data
	AFX_ODBC_CALL(::SQLGetData(m_hstmt, 0, SQL_C_BOOKMARK,
		&varBookmark.m_lVal, sizeof(varBookmark.m_lVal), &nActualSize));
	if (!Check(nRetCode))
	{
		TRACE0("Error: GetBookmark operation failed.\n");
		ThrowDBException(nRetCode);
	}
}

void CRecordset::SetBookmark(const CDBVariant& varBookmark)
{
	ASSERT_VALID(this);

	// Validate bookmarks are usable
	if (!(m_dwOptions & useBookmarks))
		ThrowDBException(AFX_SQL_ERROR_BOOKMARKS_NOT_ENABLED);
	else if (!CanBookmark())
		ThrowDBException(AFX_SQL_ERROR_BOOKMARKS_NOT_SUPPORTED);

	// Currently ODBC only supports 4 byte bookmarks
	ASSERT(varBookmark.m_dwType == DBVT_LONG);

	Move(varBookmark.m_lVal, SQL_FETCH_BOOKMARK);
}

void CRecordset::SetRowsetSize(DWORD dwNewRowsetSize)
{
	ASSERT_VALID(this);
	ASSERT(dwNewRowsetSize > 0);

	// If not yet open, only set expected length
	if (!IsOpen())
	{
		m_dwRowsetSize = dwNewRowsetSize;
		return;
	}

	if (!(m_dwOptions & useMultiRowFetch))
	{
		// Only works if bulk row fetching!
		ASSERT(FALSE);
		return;
	}

	// Need to reallocate some memory if rowset size grows
	if (m_dwAllocatedRowsetSize == 0 ||
		(m_dwAllocatedRowsetSize < dwNewRowsetSize))
	{
		// If rowset already allocated, delete old and reallocate
		FreeRowset();
		m_rgRowStatus = new WORD[dwNewRowsetSize];

		// If not a user allocated buffer grow the data buffers
		if (!(m_dwOptions & userAllocMultiRowBuffers))
		{
			// Allocate the rowset field buffers
			m_dwRowsetSize = dwNewRowsetSize;
			CFieldExchange fx(CFieldExchange::AllocMultiRowBuffer, this);
			DoBulkFieldExchange(&fx);

			m_dwAllocatedRowsetSize = dwNewRowsetSize;

			// Set bound fields to zero, rebind and reset bound field count
			int nOldFieldsBound = m_nFieldsBound;
			m_nFieldsBound = 0;
			InitRecord();
			m_nFieldsBound = nOldFieldsBound;
		}
	}
	else
	{
		// Just reset the new rowset size
		m_dwRowsetSize = dwNewRowsetSize;
	}

	RETCODE nRetCode;
	AFX_SQL_SYNC(::SQLSetStmtOption(m_hstmt, SQL_ROWSET_SIZE,
		m_dwRowsetSize));
}

void CRecordset::AddNew()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);
	// we can't construct an INSERT statement w/o any columns
	ASSERT(m_nFields != 0);

	if (!m_bAppendable)
	{
		ThrowDBException(AFX_SQL_ERROR_RECORDSET_READONLY);
	}

	if (m_dwOptions & useMultiRowFetch)
	{
		// Can't use update methods on multi-row rowset
		ASSERT(FALSE);
		return;
	}

	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		if (m_nEditMode == noMode)
		{
			// First addnew call, cache record values
			StoreFields();
		}
		else
		{
			// subsequent Edit/AddNew call.  Restore values, save them again
			LoadFields();
			StoreFields();
		}
	}

	SetFieldNull(NULL);
	SetFieldDirty(NULL, FALSE);

	m_nEditMode = addnew;
}

void CRecordset::Edit()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);
	// we can't construct an UPDATE statement w/o any columns
	ASSERT(m_nFields != 0);

	if (!m_bUpdatable)
		ThrowDBException(AFX_SQL_ERROR_RECORDSET_READONLY);

	if (m_dwOptions & useMultiRowFetch)
	{
		// Can't use update methods on multi-row rowset
		ASSERT(FALSE);
		return;
	}

	if (m_bEOF || m_bBOF || m_bDeleted)
	{
		TRACE0("Error: Edit attempt failed - not on a record.\n");
		ThrowDBException(AFX_SQL_ERROR_NO_CURRENT_RECORD);
	}

	if ((m_nOpenType == dynaset || m_nOpenType == dynamic) &&
		m_nLockMode == pessimistic)
	{
		RETCODE nRetCode;
		AFX_ODBC_CALL(::SQLSetPos(m_hstmt, 1, SQL_POSITION,
			SQL_LCK_EXCLUSIVE));
		if (!Check(nRetCode))
		{
			TRACE0("Error: attempt to lock record failed during Edit function.\n");
			ThrowDBException(nRetCode);
		}
	}

	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		if (m_nEditMode == noMode)
			// First edit call, cache record values
			StoreFields();
		else
		{
			// subsequent Edit/AddNew call.  Restore values, save them again
			LoadFields();
			StoreFields();
		}
	}

	m_nEditMode = edit;
}

BOOL CRecordset::Update()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	if (m_dwOptions & useMultiRowFetch)
	{
		// Can't use update methods on multi-row rowset
		ASSERT(FALSE);
		return FALSE;
	}

	if (m_nEditMode != addnew && m_nEditMode != edit)
	{
		TRACE0("Error: must enter Edit or AddNew mode before updating.\n");
		ThrowDBException(AFX_SQL_ERROR_ILLEGAL_MODE);
	}
	return UpdateInsertDelete();
}

void CRecordset::Delete()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	if (m_dwOptions & useMultiRowFetch)
	{
		// Can't use update methods on multi-row rowset
		ASSERT(FALSE);
		return;
	}

	if (m_nEditMode != noMode)
	{
		TRACE0("Error: attempted to delete while still in Edit or AddNew mode.\n");
		ThrowDBException(AFX_SQL_ERROR_ILLEGAL_MODE);
	}
	UpdateInsertDelete();   // This call can't fail in delete mode (noMode)
}

void CRecordset::CancelUpdate()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	if (m_nEditMode == noMode)
		// Do nothing if not in edit mode
		return;
	else
		// Reset the edit mode
		m_nEditMode = noMode;

	// Restore cache if necessary
	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
		LoadFields();
}

BOOL CRecordset::FlushResultSet() const
{
	RETCODE nRetCode;
	AFX_ODBC_CALL(::SQLMoreResults(m_hstmt));

	if (!Check(nRetCode))
	{
		TRACE0("Error: attempt FlushResultSet failed.\n");
		AfxThrowDBException(nRetCode, m_pDatabase, m_hstmt);
	}

	// Reset state of cursor
	((CRecordset*)this)->ResetCursor();

	return nRetCode != SQL_NO_DATA_FOUND;
}

void CRecordset::GetODBCFieldInfo(LPCTSTR lpszName,
	CODBCFieldInfo& fieldinfo)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(lpszName != NULL);

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Get the index of the field corresponding to name
	short nField = GetFieldIndexByName(lpszName);

	GetODBCFieldInfo(nField, fieldinfo);
}

void CRecordset::GetODBCFieldInfo(short nIndex,
	CODBCFieldInfo& fieldinfo)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Just copy the data into the field info
	CODBCFieldInfo* pInfo = &m_rgODBCFieldInfos[nIndex];
	fieldinfo.m_strName = pInfo->m_strName;
	fieldinfo.m_nSQLType = pInfo->m_nSQLType;
	fieldinfo.m_nPrecision = pInfo->m_nPrecision;
	fieldinfo.m_nScale = pInfo->m_nScale;
	fieldinfo.m_nNullability = pInfo->m_nNullability;
}

void CRecordset::GetFieldValue(LPCTSTR lpszName,
	CDBVariant& varValue, short nFieldType)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(lpszName != NULL);

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		varValue.Clear();
		return;
	}

	// Get the index of the field corresponding to name
	short nField = GetFieldIndexByName(lpszName);

	GetFieldValue(nField, varValue, nFieldType);
}

void CRecordset::GetFieldValue(short nIndex,
	CDBVariant& varValue, short nFieldType)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	// Clear the previous variant
	varValue.Clear();

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Convert index to 1-based and check range
	nIndex++;
	if (nIndex < 1 || nIndex > GetODBCFieldCount())
	{
		ThrowDBException(AFX_SQL_ERROR_FIELD_NOT_FOUND);
	}

	void* pvData = NULL;
	int nLen = 0;

	// Determine the default field type and get the data buffer
	if (nFieldType == DEFAULT_FIELD_TYPE)
	{
		nFieldType =
			GetDefaultFieldType(m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);
	}
	pvData = GetDataBuffer(varValue, nFieldType, &nLen,
		m_rgODBCFieldInfos[nIndex - 1].m_nSQLType,
		m_rgODBCFieldInfos[nIndex - 1].m_nPrecision);

	// Now can actually get the data
	long nActualSize = GetData(m_pDatabase, m_hstmt, nIndex,
		nFieldType, pvData, nLen,
		m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);

	// Handle NULL data separately
	if (nActualSize == SQL_NULL_DATA)
	{
		// Clear value and set the value NULL
		varValue.Clear();
	}
	else
	{
		// May need to cleanup and call SQLGetData again if LONG_VAR data
		if (nFieldType == SQL_C_CHAR)
		{
			GetLongCharDataAndCleanup(m_pDatabase, m_hstmt, nIndex,
				nActualSize, &pvData, nLen, *varValue.m_pstring,
				m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);

#ifdef _UNICODE
			// Now must convert string to UNICODE
			LPCSTR lpszOld = (LPCSTR)varValue.m_pstring->GetBuffer(0);
			CString* pStringNew = new CString(lpszOld);
			delete varValue.m_pstring;
			varValue.m_pstring = pStringNew;
#endif // _UNICODE
		}
		else if (nFieldType == SQL_C_BINARY)
		{
			GetLongBinaryDataAndCleanup(m_pDatabase, m_hstmt, nIndex,
				nActualSize, &pvData, nLen, varValue,
				m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);
		}
	}
}

void CRecordset::GetFieldValue(LPCTSTR lpszName, CString& strValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(lpszName != NULL);

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Get the index of the field corresponding to name
	short nField = GetFieldIndexByName(lpszName);

	GetFieldValue(nField, strValue);
}

void CRecordset::GetFieldValue(short nIndex, CString& strValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	// No data or no column info fetched yet
	if (GetODBCFieldCount() <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Convert index to 1-based and check range
	nIndex++;
	if (nIndex < 1 || nIndex > GetODBCFieldCount())
	{
		ThrowDBException(AFX_SQL_ERROR_FIELD_NOT_FOUND);
	}

	int nLen = GetTextLen(m_rgODBCFieldInfos[nIndex - 1].m_nSQLType,
			m_rgODBCFieldInfos[nIndex - 1].m_nPrecision);

#ifndef _UNICODE
	CString& strData = strValue;
#else
	CString strProxy;
	CString& strData = strProxy;
#endif
	void* pvData = strData.GetBufferSetLength(nLen);

	// Now can actually get the data
	long nActualSize = GetData(m_pDatabase, m_hstmt, nIndex,
		SQL_C_CHAR, pvData, nLen,
		m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);

	// Handle NULL data separately
	if (nActualSize == SQL_NULL_DATA)
	{
		// Clear value
		strValue.Empty();
	}
	else
	{
		// May need to cleanup and call SQLGetData again if necessary
		GetLongCharDataAndCleanup(m_pDatabase, m_hstmt, nIndex,
			nActualSize, &pvData, nLen, strData,
			m_rgODBCFieldInfos[nIndex - 1].m_nSQLType);

#ifdef _UNICODE
	// Now must convert string to UNICODE
	strValue = (LPCSTR)strData.GetBuffer(0);
#endif // _UNIOCDE
	}
}

void CRecordset::SetFieldDirty(void* pv, BOOL bDirty)
{
	ASSERT_VALID(this);

	int nIndex, nIndexEnd;

	// If not setting all NULL, check simple case
	if (pv != NULL)
	{
		// GetBoundFieldIndex returns 1-based index
		nIndex = GetBoundFieldIndex(pv) - 1;

		if (nIndex < 0)
		{
			// pv must be address of field member
			ASSERT(FALSE);
			return;
		}
		else
		{
			nIndexEnd = nIndex;
		}
	}
	else
	{
		nIndex = 0;
		nIndexEnd = m_nFields - 1;
	}

	while (nIndex <= nIndexEnd)
	{
		if (bDirty)
			SetDirtyFieldStatus((DWORD)nIndex);
		else
			ClearDirtyFieldStatus((DWORD)nIndex);

		nIndex++;
	}
}

void CRecordset::SetFieldNull(void* pv, BOOL bNull)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(!(m_dwOptions & useMultiRowFetch));

	// If not setting all fields NULL, check simple case (param) first
	if (pv != NULL)
	{
		// Cached index is 1-based
		int nIndex = GetBoundParamIndex(pv) - 1;
		if (nIndex >= 0)
		{
			if (bNull)
				SetNullParamStatus(nIndex);
			else
				ClearNullParamStatus(nIndex);
			return;
		}
	}

	// Not a param, must be a field
	if (m_nFields <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	// Need field exchange mechanism to set PSEUDO NULL values
	// and to reset data lengths (especially for RFX_LongBinary)
	CFieldExchange fx(CFieldExchange::SetFieldNull, this, pv);
	fx.m_nFieldFound = 0;
	fx.m_bField = bNull;
	DoFieldExchange(&fx);

	// If no field found, m_nFieldFound will still be zero
	ASSERT(fx.m_nFieldFound != 0);
}

void CRecordset::SetParamNull(int nIndex, BOOL bNull)
{
	ASSERT_VALID(this);
	ASSERT((DWORD)nIndex < m_nParams);

	// Can be called before Open, but need to alloc status arrays first
	if (!IsOpen())
		AllocStatusArrays();

	if (bNull)
		SetNullParamStatus(nIndex);
	else
		ClearNullParamStatus(nIndex);

	return;
}

void CRecordset::SetLockingMode(UINT nLockMode)
{
	if (nLockMode == pessimistic)
	{
		RETCODE nRetCode;
		UDWORD dwTypes;
		SWORD nResult;
		AFX_SQL_SYNC(::SQLGetInfo(m_pDatabase->m_hdbc, SQL_LOCK_TYPES,
			&dwTypes, sizeof(dwTypes), &nResult));
		if (!Check(nRetCode) || !(dwTypes & SQL_LCK_EXCLUSIVE))
			ThrowDBException(AFX_SQL_ERROR_LOCK_MODE_NOT_SUPPORTED);
	}
	m_nLockMode = nLockMode;
}

BOOL CRecordset::Requery()
{
	RETCODE nRetCode;

	ASSERT_VALID(this);
	ASSERT(IsOpen());

	// Can't requery if using direct execution
	if (m_dwOptions & executeDirect)
		return FALSE;

	TRY
	{
		// Detect changes to filter and sort
		if ((m_strFilter != m_strRequeryFilter) || (m_strSort != m_strRequerySort))
		{
			m_strRequeryFilter = m_strFilter;
			m_strRequerySort = m_strSort;
			Close();
			if (m_strRequerySQL.IsEmpty())
				return Open(m_nOpenType, NULL, m_dwOptions);
			else
				return Open(m_nOpenType, m_strRequerySQL, m_dwOptions);
		}
		else
		{
			// Shutdown current query, preserving buffers for performance
			AFX_SQL_SYNC(::SQLFreeStmt(m_hstmt, SQL_CLOSE));
			m_lOpen = AFX_RECORDSET_STATUS_CLOSED;

			// Rebind date/time parameters
			RebindParams(m_hstmt);

			// now attempt to re-execute the SQL Query
			AFX_ODBC_CALL(::SQLExecute(m_hstmt));
			if (!Check(nRetCode))
			{
				TRACE0("Error: Requery attempt failed.\n");
				ThrowDBException(nRetCode);
			}

			m_lOpen = AFX_RECORDSET_STATUS_OPEN;

			// Reset some cursor properties and fetch first record
			ResetCursor();
			MoveNext();

			// If EOF, then result set empty, so set BOF as well
			m_bBOF = m_bEOF;
		}
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL

	return TRUE;    // all set
}

// Shutdown any pending query for CRecordset's hstmt's
void CRecordset::Cancel()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	::SQLCancel(m_hstmt);

	// If Update hstmt has been allocated, shut it down also
	if (m_hstmtUpdate != SQL_NULL_HSTMT)
		::SQLCancel(m_hstmtUpdate);
}

CString CRecordset::GetDefaultConnect()
{
	ASSERT_VALID(this);

	return _afxODBCTrail;
}

CString CRecordset::GetDefaultSQL()
{
	ASSERT_VALID(this);

	// Override and add table name or entire SQL SELECT statement
	return _T("");
}

void CRecordset::DoFieldExchange(CFieldExchange* /* pFX */)
{
	ASSERT_VALID(this);

	// Do nothing if dynamically retrieving unbound fields,
	// otherwise override CRecordset and add RFX calls
}

void CRecordset::DoBulkFieldExchange(CFieldExchange* /* pFX */)
{
	ASSERT_VALID(this);

	// To use multi-record data fetching, you must use
	// a derived CRecordset class and call Close explicitly.
	ASSERT(FALSE);
}

void CRecordset::OnSetOptions(HSTMT hstmt)
{
	ASSERT_VALID(this);
	ASSERT(hstmt != SQL_NULL_HSTMT);

	// Inherit options settings from CDatabase
	m_pDatabase->OnSetOptions(hstmt);

	// If fowardOnly recordset and not using SQLExtendedFetch, quit now
	if (m_nOpenType == forwardOnly && !(m_dwOptions & useExtendedFetch))
		return;

	// Turn on bookmark support if necessary
	EnableBookmarks();

	// If using forwardOnly and extended fetch, quit now
	if (m_nOpenType == forwardOnly)
		return;

	// Make sure driver supports extended fetch, ODBC 2.0 and requested cursor type
	VerifyDriverBehavior();
	DWORD dwScrollType = VerifyCursorSupport();

	// Set the update method, concurrency and cursor type
	SetUpdateMethod();
	SetConcurrencyAndCursorType(hstmt, dwScrollType);
}

// Screen for errors.
BOOL CRecordset::Check(RETCODE nRetCode) const
{
	ASSERT_VALID(this);

	switch (nRetCode)
	{
	case SQL_SUCCESS_WITH_INFO:
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
		{
			CDBException e(nRetCode);
			TRACE0("Warning: ODBC Success With Info, ");
			e.BuildErrorString(m_pDatabase, m_hstmt);
		}
#endif

		// Fall through

	case SQL_SUCCESS:
	case SQL_NO_DATA_FOUND:
	case SQL_NEED_DATA:
		return TRUE;
	}

	return FALSE;
}

void CRecordset::PreBindFields()
{
	// Do nothing
}

//////////////////////////////////////////////////////////////////////////////
// CRecordset internal functions

// Cache state information internally in CRecordset
void CRecordset::SetState(int nOpenType, LPCTSTR lpszSQL, DWORD dwOptions)
{
	if (nOpenType == AFX_DB_USE_DEFAULT_TYPE)
		m_nOpenType = m_nDefaultType;
	else
		m_nOpenType = nOpenType;

	m_bAppendable = (dwOptions & appendOnly) != 0 ||
		(dwOptions & readOnly) == 0;
	m_bUpdatable = (dwOptions & readOnly) == 0 &&
		(dwOptions & appendOnly) == 0;

	// Can turn off dirty field checking via dwOptions
	if (dwOptions & noDirtyFieldCheck || dwOptions & useMultiRowFetch)
		m_bCheckCacheForDirtyFields = FALSE;

	// Set recordset readOnly if forwardOnly
	if (m_nOpenType == forwardOnly && !(dwOptions & readOnly))
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
			TRACE0("Warning: Setting forwardOnly recordset readOnly.\n");
#endif
		dwOptions |= readOnly;

		// If using multiRowFetch also set useExtendFetch
		if (dwOptions & useMultiRowFetch)
			dwOptions |= useExtendedFetch;
	}

	// Archive info for use in Requery
	m_dwOptions = dwOptions;
	m_strRequerySQL = lpszSQL;
	m_strRequeryFilter = m_strFilter;
	m_strRequerySort = m_strSort;
}

// Allocate the Hstmt and implicitly create and open Database if necessary
BOOL CRecordset::AllocHstmt()
{
	RETCODE nRetCode;
	if (m_hstmt == SQL_NULL_HSTMT)
	{
		CString strDefaultConnect;
		TRY
		{
			if (m_pDatabase == NULL)
			{
				m_pDatabase = new CDatabase();
				m_bRecordsetDb = TRUE;
			}

			strDefaultConnect = GetDefaultConnect();

			// If not already opened, attempt to open
			if (!m_pDatabase->IsOpen())
			{
				BOOL bUseCursorLib = m_bUseODBCCursorLib;

				// If non-readOnly snapshot request must use cursor lib
				if (m_nOpenType == snapshot && !(m_dwOptions & readOnly))
				{
					// This assumes drivers only support readOnly snapshots
					bUseCursorLib = TRUE;
				}

				if (!m_pDatabase->Open(_T(""), FALSE, FALSE,
					strDefaultConnect, bUseCursorLib))
				{
					return FALSE;
				}

				// If snapshot cursor requested and not supported, load cursor lib
				if (m_nOpenType == snapshot && !bUseCursorLib)
				{
					// Get the supported cursor types
					RETCODE nResult;
					UDWORD dwDriverScrollOptions;
					AFX_SQL_SYNC(::SQLGetInfo(m_pDatabase->m_hdbc, SQL_SCROLL_OPTIONS,
						&dwDriverScrollOptions, sizeof(dwDriverScrollOptions), &nResult));
					if (!Check(nRetCode))
					{
						TRACE0("Error: ODBC failure checking for driver capabilities.\n");
						ThrowDBException(nRetCode);
					}

					// Check for STATIC cursor support and load cursor lib
					if (!(dwDriverScrollOptions & SQL_SO_STATIC))
					{
						m_pDatabase->Close();
						if (!m_pDatabase->Open(_T(""), FALSE, FALSE,
							strDefaultConnect, TRUE))
							return FALSE;
					}
				}
			}

			AFX_SQL_SYNC(::SQLAllocStmt(m_pDatabase->m_hdbc, &m_hstmt));
			if (!Check(nRetCode))
				ThrowDBException(SQL_INVALID_HANDLE);

			// Add to list of CRecordsets with alloced hstmts
			AfxLockGlobals(CRIT_ODBC);
			TRY
			{
				m_pDatabase->m_listRecordsets.AddHead(this);
			}
			CATCH_ALL(e)
			{
				AfxUnlockGlobals(CRIT_ODBC);
				THROW_LAST();
			}
			END_CATCH_ALL
			AfxUnlockGlobals(CRIT_ODBC);
		}
		CATCH_ALL(e)
		{
#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
				TRACE0("Error: CDatabase create for CRecordset failed.\n");
#endif
			NO_CPP_EXCEPTION(strDefaultConnect.Empty());
			if (m_bRecordsetDb)
			{
				delete m_pDatabase;
				m_pDatabase = NULL;
			}
			ASSERT(m_hstmt == SQL_NULL_HSTMT);
			THROW_LAST();
		}
		END_CATCH_ALL
	}

	return TRUE;
}

// Initialize the status arrays and create the SQL
void CRecordset::BuildSQL(LPCTSTR lpszSQL)
{
	if (lpszSQL == NULL)
		m_strSQL = GetDefaultSQL();
	else
		m_strSQL = lpszSQL;

	// Set any supplied params
	if (m_nParams != 0)
	{
		UINT nParams = BindParams(m_hstmt);
		ASSERT(nParams == m_nParams);
	}

	// Construct the SQL string
	BuildSelectSQL();
	AppendFilterAndSortSQL();

	// Do some extra checking if trying to set recordset updatable or appendable
	if ((m_bUpdatable || m_bAppendable) && !IsRecordsetUpdatable())
		m_bUpdatable = m_bAppendable = FALSE;

	if (m_bUpdatable && m_bUseUpdateSQL && m_pDatabase->m_bAddForUpdate)
		m_strSQL += _afxForUpdate;

	// Replace brackets with SQL_IDENTIFIER_QUOTE_CHAR
	m_pDatabase->ReplaceBrackets(m_strSQL.GetBuffer(0));
	m_strSQL.ReleaseBuffer();
}

// Prepare and Execute the SQL or simple call SQLExecDirect, resetting concurrency if necessary
void CRecordset::PrepareAndExecute()
{
	USES_CONVERSION;
	RETCODE nRetCode = 0;
	BOOL bConcurrency = FALSE;
	LPCSTR lpszWSQL = T2CA(m_strSQL);

	while (!bConcurrency)
	{
		// Prepare or execute the query
		if (m_dwOptions & executeDirect)
		{
			AFX_ODBC_CALL(::SQLExecDirect(m_hstmt,
				(UCHAR*)lpszWSQL, SQL_NTS));
		}
		else
		{
			AFX_ODBC_CALL(::SQLPrepare(m_hstmt,
				(UCHAR*)lpszWSQL, SQL_NTS));
		}
		if (Check(nRetCode))
			bConcurrency = TRUE;
		else
		{
			// If "Driver Not Capable" error, assume cursor type doesn't
			// support requested concurrency and try alternate concurrency.
			CDBException* e = new CDBException(nRetCode);
			e->BuildErrorString(m_pDatabase, m_hstmt);
			if (m_dwConcurrency != SQL_CONCUR_READ_ONLY &&
				e->m_strStateNativeOrigin.Find(_afxDriverNotCapable) >= 0)
			{
#ifdef _DEBUG
				if (afxTraceFlags & traceDatabase)
					TRACE0("Warning: Driver does not support requested concurrency.\n");
#endif

				// Don't need exception to persist while attempting to reset concurrency
				e->Delete();

				// ODBC will automatically attempt to set alternate concurrency if
				// request fails, but it won't try LOCK even if driver supports it.
				if ((m_dwDriverConcurrency & SQL_SCCO_LOCK) &&
					(m_dwConcurrency == SQL_CONCUR_ROWVER ||
					m_dwConcurrency == SQL_CONCUR_VALUES))
				{
					m_dwConcurrency = SQL_CONCUR_LOCK;
				}
				else
				{
					m_dwConcurrency = SQL_CONCUR_READ_ONLY;
					m_bUpdatable = m_bAppendable = FALSE;
#ifdef _DEBUG
					if (afxTraceFlags & traceDatabase)
						TRACE0("Warning: Setting recordset read only.\n");
#endif
				}

				// Attempt to reset the concurrency model.
				AFX_SQL_SYNC(::SQLSetStmtOption(m_hstmt, SQL_CONCURRENCY,
					m_dwConcurrency));
				if (!Check(nRetCode))
				{
					TRACE0("Error: ODBC failure setting recordset concurrency.\n");
					ThrowDBException(nRetCode);
				}
			}
			else
			{
				TRACE0("Error: ODBC failure on SQLPrepare or SQLExecDirect\n");
				THROW(e);
			}
		}
	}


	// now attempt to execute the SQL Query if not executed already
	if (!(m_dwOptions & executeDirect))
	{
		AFX_ODBC_CALL(::SQLExecute(m_hstmt));
		if (!Check(nRetCode))
			ThrowDBException(nRetCode);
	}
	m_lOpen = AFX_RECORDSET_STATUS_OPEN;

	// SQLExecute or SQLExecDirect may have changed an option value
	if (nRetCode == SQL_SUCCESS_WITH_INFO)
	{
		// Check if concurrency was changed in order to mark
		// recordset non-updatable if necessary
		DWORD dwConcurrency;
		AFX_SQL_SYNC(::SQLGetStmtOption(m_hstmt, SQL_CONCURRENCY, &dwConcurrency));
		if (!Check(nRetCode))
			ThrowDBException(nRetCode);

		if (dwConcurrency == SQL_CONCUR_READ_ONLY && (m_bUpdatable || m_bAppendable))
		{
			m_bUpdatable = FALSE;
			m_bAppendable = FALSE;

#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
			{
				TRACE0("Warning: Concurrency changed by driver.\n");
				TRACE0("\tMarking CRecordset as not updatable.\n");
			}
#endif // _DEBUG
		}
	}
}

// Ensure that driver supports extended fetch and ODBC 2.0 if necessary
void CRecordset::VerifyDriverBehavior()
{
	RETCODE nRetCode;
	UWORD wScrollable;
	// If SQLExtendedFetch not supported, use SQLFetch
	AFX_SQL_SYNC(::SQLGetFunctions(m_pDatabase->m_hdbc,
		SQL_API_SQLEXTENDEDFETCH, &wScrollable));
	if (!Check(nRetCode))
	{
		TRACE0("Error: ODBC failure determining whether recordset is scrollable.\n");
		ThrowDBException(nRetCode);
	}
	m_bScrollable = wScrollable;
	if (!m_bScrollable)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
		{
			TRACE0("Warning: SQLExtendedFetch not supported by driver\n");
			TRACE0("and/or cursor library not loaded. Opening forwardOnly.\n");
			TRACE0("for use with SQLFetch.\n");
		}
#endif
		m_bUpdatable = FALSE;
		return;
	}

	char szResult[30];
	SWORD nResult;
	// require ODBC v2.0
	AFX_SQL_SYNC(::SQLGetInfo(m_pDatabase->m_hdbc, SQL_ODBC_VER,
		&szResult, _countof(szResult), &nResult));
	if (!Check(nRetCode))
	{
		TRACE0("Error: ODBC failure checking for driver capabilities.\n");
		ThrowDBException(nRetCode);
	}
	if (szResult[0] == '0' && szResult[1] < '2')
		ThrowDBException(AFX_SQL_ERROR_ODBC_V2_REQUIRED);
}

// Check that driver supports requested cursor type
DWORD CRecordset::VerifyCursorSupport()
{
	RETCODE nRetCode;
	SWORD nResult;
	UDWORD dwDriverScrollOptions;
	AFX_SQL_SYNC(::SQLGetInfo(m_pDatabase->m_hdbc, SQL_SCROLL_OPTIONS,
		&dwDriverScrollOptions, sizeof(dwDriverScrollOptions), &nResult));
	if (!Check(nRetCode))
	{
		TRACE0("Error: ODBC failure checking for driver capabilities.\n");
		ThrowDBException(nRetCode);
	}

	SDWORD dwScrollOptions = SQL_CURSOR_FORWARD_ONLY;
	if (m_nOpenType == dynaset)
	{
		// Dynaset support requires ODBC's keyset driven cursor model
		if (!(dwDriverScrollOptions & SQL_SO_KEYSET_DRIVEN))
			ThrowDBException(AFX_SQL_ERROR_DYNASET_NOT_SUPPORTED);
		dwScrollOptions = SQL_CURSOR_KEYSET_DRIVEN;
	}
	else if (m_nOpenType == snapshot)
	{
		// Snapshot support requires ODBC's static cursor model
		if (!(dwDriverScrollOptions & SQL_SO_STATIC))
			ThrowDBException(AFX_SQL_ERROR_SNAPSHOT_NOT_SUPPORTED);
		dwScrollOptions = SQL_CURSOR_STATIC;
	}
	else
	{
		// Dynamic cursor support requires ODBC's dynamic cursor model
		if (!(dwDriverScrollOptions & SQL_SO_DYNAMIC))
			ThrowDBException(AFX_SQL_ERROR_DYNAMIC_CURSOR_NOT_SUPPORTED);
		dwScrollOptions = SQL_CURSOR_DYNAMIC;
	}

	return dwScrollOptions;
}

void CRecordset::AllocAndCacheFieldInfo()
{
	ASSERT(GetODBCFieldCount() < 0);
	ASSERT(m_rgODBCFieldInfos == NULL);

	RETCODE nRetCode;
	SWORD nActualLen;

	// Cache the number of result columns
	AFX_ODBC_CALL(::SQLNumResultCols(m_hstmt, &m_nResultCols));
	if (!Check(nRetCode))
	{
		TRACE0("Error: Can't get field info.\n");
		ThrowDBException(nRetCode);
	}

	// If there are no fields quit now
	if (m_nResultCols == 0)
		return;

	// Allocate buffer and get the ODBC meta data
	m_rgODBCFieldInfos = new CODBCFieldInfo[m_nResultCols];
	LPSTR lpszFieldName;

#ifdef _UNICODE
	// Need proxy to temporarily store non-UNICODE name
	lpszFieldName = new char[MAX_FNAME_LEN + 1];
#endif

	// Get the field info each field
	for (WORD n = 1; n <= GetODBCFieldCount(); n++)
	{
#ifndef _UNICODE
		// Reset the buffer to point to next element
		lpszFieldName =
			m_rgODBCFieldInfos[n - 1].m_strName.GetBuffer(MAX_FNAME_LEN + 1);
#endif

		AFX_ODBC_CALL(::SQLDescribeCol(m_hstmt, n,
			(UCHAR*)lpszFieldName, MAX_FNAME_LEN, &nActualLen,
			&m_rgODBCFieldInfos[n - 1].m_nSQLType,
			&m_rgODBCFieldInfos[n - 1].m_nPrecision,
			&m_rgODBCFieldInfos[n - 1].m_nScale,
			&m_rgODBCFieldInfos[n - 1].m_nNullability));

#ifndef _UNICODE
		m_rgODBCFieldInfos[n - 1].m_strName.ReleaseBuffer(nActualLen);
#else
		// Copy the proxy data to correct element
		m_rgODBCFieldInfos[n - 1].m_strName = lpszFieldName;
#endif

		if (!Check(nRetCode))
		{
			TRACE1("Error: ODBC failure getting field #%d info.\n", n);
			ThrowDBException(nRetCode);
		}
	}

#ifdef _UNICODE
	delete[] lpszFieldName;
#endif
}

void CRecordset::AllocRowset()
{
	if (m_dwOptions & useMultiRowFetch)
		SetRowsetSize(m_dwRowsetSize);
	else
	{
		// Not using bulk row fetch, set rowset size to 1
		m_rgRowStatus = new WORD[1];
		m_dwRowsetSize = 1;
	}
}

void CRecordset::FreeRowset()
{
	// Delete the rowset status
	delete [] m_rgRowStatus;
	m_rgRowStatus = NULL;

	if (m_dwOptions & useMultiRowFetch &&
		!(m_dwOptions & userAllocMultiRowBuffers))
	{
		// Calling virtual function, DoBulkFieldExchange, here is bad
		// because Close then FreeRowset may get called from destructor.
		// There is no simple choice however if RFX_Bulk functions do
		// a memory allocation. The net result is that users MUST call
		// Close explicitly (rather than relying on destructor) if
		// using multi row fetches, otherwise they will get a memory leak.
		// If rowset already allocated, delete old rowset buffers
		if (m_dwAllocatedRowsetSize != 0)
		{
			CFieldExchange fx(CFieldExchange::DeleteMultiRowBuffer, this);
			DoBulkFieldExchange(&fx);
		}
	}

	m_dwAllocatedRowsetSize = 0;
}

void CRecordset::EnableBookmarks()
{
	// Turn on bookmark support if necessary
	if (m_dwOptions & useBookmarks)
	{
		RETCODE nRetCode;

		// Set stmt option if bookmarks supported by driver
		if (m_pDatabase->GetBookmarkPersistence() & SQL_BP_SCROLL)
		{
			AFX_SQL_SYNC(::SQLSetStmtOption(m_hstmt, SQL_USE_BOOKMARKS,
				SQL_UB_ON));
			if (!Check(nRetCode))
			{
				TRACE0("Error: Can't enable bookmark support.\n");
				ThrowDBException(nRetCode);
			}
		}
	}
}

// Determine whether to use SQLSetPos or positioned update SQL
void CRecordset::SetUpdateMethod()
{
	// Determine update method
	if (m_pDatabase->m_dwUpdateOptions & AFX_SQL_SETPOSUPDATES)
		m_bUseUpdateSQL = FALSE;
	else if (m_pDatabase->m_dwUpdateOptions & AFX_SQL_POSITIONEDSQL)
		m_bUseUpdateSQL = TRUE;
	else
		m_bUpdatable = FALSE;
 }

// Determine which type of concurrency to set, set it and cursor type
void CRecordset::SetConcurrencyAndCursorType(HSTMT hstmt, DWORD dwScrollOptions)
{
	RETCODE nRetCode;
	SWORD nResult;

	m_dwConcurrency = SQL_CONCUR_READ_ONLY;
	if ((m_bUpdatable || m_bAppendable) && m_pDatabase->m_bUpdatable)
	{
		AFX_SQL_SYNC(::SQLGetInfo(m_pDatabase->m_hdbc, SQL_SCROLL_CONCURRENCY,
			&m_dwDriverConcurrency, sizeof(m_dwDriverConcurrency), &nResult));
		if (!Check(nRetCode))
		{
			TRACE0("Error: ODBC failure checking recordset updatability.\n");
			ThrowDBException(nRetCode);
		}

		if (m_nLockMode == pessimistic)
		{
			if (m_dwDriverConcurrency & SQL_SCCO_LOCK)
				m_dwConcurrency = SQL_CONCUR_LOCK;
#ifdef _DEBUG
			else
				if (afxTraceFlags & traceDatabase)
					TRACE0("Warning: locking not supported, setting recordset read only.\n");
#endif
		}
		else
		{
			// Use cheapest, most concurrent model
			if (m_dwDriverConcurrency & SQL_SCCO_OPT_ROWVER)
				m_dwConcurrency = SQL_CONCUR_ROWVER;
			else if (m_dwDriverConcurrency & SQL_SCCO_OPT_VALUES)
				m_dwConcurrency = SQL_CONCUR_VALUES;
			else if (m_dwDriverConcurrency & SQL_SCCO_LOCK)
				m_dwConcurrency = SQL_CONCUR_LOCK;
		}
	}

	// Set cursor type (Let rowset size default to 1).
	AFX_SQL_SYNC(::SQLSetStmtOption(hstmt, SQL_CURSOR_TYPE, dwScrollOptions));
	if (!Check(nRetCode))
	{
		TRACE0("Error: ODBC failure setting recordset cursor type.\n");
		ThrowDBException(nRetCode);
	}

	// Set the concurrency model (NOTE: may have to reset concurrency later).
	AFX_SQL_SYNC(::SQLSetStmtOption(hstmt, SQL_CONCURRENCY, m_dwConcurrency));
	if (!Check(nRetCode))
	{
		TRACE0("Error: ODBC failure setting recordset concurrency.\n");
		ThrowDBException(nRetCode);
	}
}

// Is there a join, stored proc call, GROUP BY, UNION or missing FROM?
BOOL CRecordset::IsSQLUpdatable(LPCTSTR lpszSQL)
{
	// Parse for query procedure call keyword or return param
	if (!(_tcsnicmp(lpszSQL, _afxCall, lstrlen(_afxCall)-1) == 0 ||
		_tcsnicmp(lpszSQL, _afxParamCall, lstrlen(_afxParamCall)-1) == 0))
		// Assume this is a select query
		return IsSelectQueryUpdatable(lpszSQL);
	else
		// Don't know the table name to update in procedure call
		return FALSE;
}

BOOL CRecordset::IsSelectQueryUpdatable(LPCTSTR lpszSQL)
{
	LPCTSTR lpchTokenFrom;
	LPCTSTR lpchToken;
	LPCTSTR lpchTokenNext;
	LPTSTR lpszSQLStart;
	CString strSQL = lpszSQL;

	lpchTokenFrom = FindSQLToken(strSQL, _afxFrom);
	if (lpchTokenFrom == NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
			TRACE0("Warning: Missing ' FROM ', recordset not updatable \n");
#endif
		return FALSE;
	}

	lpchToken = FindSQLToken(strSQL, _T(" GROUP BY "));
	if (lpchToken != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
			TRACE0("Warning: SQL contains ' GROUP BY ', recordset not updatable \n");
#endif
		return FALSE;
	}

	lpchToken = FindSQLToken(strSQL, _T(" UNION "));
	if (lpchToken != NULL)
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
			TRACE0("Warning: SQL contains ' UNION ', recordset not updatable \n");
#endif
		return FALSE;
	}

	// Find next token after FROM (can't have HAVING clause without GROUP BY)
	lpchToken = FindSQLToken(strSQL, _afxWhere);
	lpchTokenNext = FindSQLToken(strSQL, _afxOrderBy);

	lpszSQLStart = strSQL.GetBuffer(0);

	if (lpchTokenNext == NULL)
		lpchTokenNext = lpchToken;
	else if (lpchToken != NULL && lpchToken < lpchTokenNext)
		lpchTokenNext = lpchToken;

	if (lpchTokenNext != NULL)
	{
		int nFromLength = lpchTokenNext - lpchTokenFrom;
		memcpy(lpszSQLStart, lpchTokenFrom, nFromLength*sizeof(TCHAR));
		lpszSQLStart[nFromLength] = '\0';
	}
	else
		lstrcpy(lpszSQLStart, lpchTokenFrom);

	strSQL.ReleaseBuffer();

	if (IsJoin(strSQL))
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
			TRACE0("Warning: SQL contains join, recordset not updatable \n");
#endif
		return FALSE;
	}

	// Cache table name (skip over " FROM ")
	m_strTableName = strSQL.Right(strSQL.GetLength()-6);

	return TRUE;
}


// Check FROM clause for join syntax
BOOL PASCAL CRecordset::IsJoin(LPCTSTR lpszJoinClause)
{
	// Look for comma in join clause
	if (FindSQLToken(lpszJoinClause, _afxComma) != NULL)
		return TRUE;

	// Look for outer join clause
	if (FindSQLToken(lpszJoinClause, _T(" JOIN ")) != NULL)
		return TRUE;

	return FALSE;
}

// Searches string for given token not in single quotes or brackets
LPCTSTR PASCAL CRecordset::FindSQLToken(LPCTSTR lpszSQL, LPCTSTR lpszSQLToken)
{
	BOOL bInLiteral;
	BOOL bInBrackets;
	int nLeftBrackets;
	int nRightBrackets;
	LPCTSTR lpch;
	LPCTSTR lpchSQLStart;
	LPCTSTR lpszFoundToken;
	int nTokenOffset = 0;
	CString strSQL = lpszSQL;

	strSQL.MakeUpper();
	lpszFoundToken = strSQL.GetBuffer(0);
	lpchSQLStart = lpszFoundToken;

	do
	{
		lpszFoundToken = _tcsstr(lpszFoundToken + nTokenOffset, lpszSQLToken);
		if (lpszFoundToken == NULL)
		{
			strSQL.ReleaseBuffer();
			return NULL;
		}

		bInLiteral = bInBrackets = FALSE;
		nLeftBrackets = nRightBrackets = 0;

		// Check if embedded in literal or brackets
		for (lpch = lpchSQLStart; lpch < lpszFoundToken; lpch = _tcsinc(lpch))
		{
			if (*lpch == _afxLiteralSeparator)
			{
				// Skip if escape literal
				if (*_tcsinc(lpch) == _afxLiteralSeparator)
					lpch = _tcsinc(lpch);
				else
					bInLiteral = !bInLiteral;
			}
			else if (!bInLiteral && (*lpch == '['))
			{
				// Skip if escape left bracket
				if (*_tcsinc(lpch) == '[')
					lpch = _tcsinc(lpch);
				else
				{
					nLeftBrackets++;
					if ((nLeftBrackets - nRightBrackets) > 0)
						bInBrackets = TRUE;
					else
						bInBrackets = FALSE;
				}
			}
			else if (!bInLiteral && (*lpch == ']'))
			{
				// Skip if escape right bracket
				if (*_tcsinc(lpch) == ']')
					lpch = _tcsinc(lpch);
				else
				{
					nRightBrackets++;
					if ((nLeftBrackets - nRightBrackets) > 0)
						bInBrackets = TRUE;
					else
						bInBrackets = FALSE;
				}
			}
		}

		// If first iteration, reset the offset to jump over found token
		if (nTokenOffset == 0)
			nTokenOffset = lstrlen(lpszSQLToken);

	} while (bInLiteral || bInBrackets);

	lpszFoundToken = lpszSQL + (lpszFoundToken - lpchSQLStart);
	strSQL.ReleaseBuffer();
	return lpszFoundToken;
}

// Bind fields (if not already bound), then retrieve 1st record
void CRecordset::InitRecord()
{
	// fields to bind
	if (m_nFields != 0)
	{
		m_nFieldsBound = BindFieldsToColumns();
		// m_nFields doesn't reflect number of
		// RFX_ output column calls in Do[Bulk]FieldExchange
		ASSERT((int)m_nFields == m_nFieldsBound);

		// Allocate the data cache if necessary
		if (m_nFields > 0 && m_bCheckCacheForDirtyFields)
			AllocDataCache();
	}
	else
		// No fields to bind, don't attempt to bind again
		m_nFieldsBound = -1;
}

void CRecordset::ResetCursor()
{
	m_bEOFSeen = m_bBOF = m_bEOF = FALSE;
	m_bDeleted = FALSE;
	m_lCurrentRecord = AFX_CURRENT_RECORD_BOF;
	m_lRecordCount = 0;
}

void CRecordset::CheckRowsetCurrencyStatus(UWORD wFetchType, long nRows)
{
	if (!m_bScrollable && wFetchType != SQL_FETCH_NEXT)
	{
		TRACE0("Error: forward-only recordsets only support MoveNext.\n");
		ThrowDBException(AFX_SQL_ERROR_RECORDSET_FORWARD_ONLY);
	}

	if (IsEOF() && IsBOF())
	{
		// Can't position cursor if recordset empty
		TRACE0("Error: attempted to position cursor on empty recordset.\n");
		ThrowDBException(AFX_SQL_ERROR_NO_DATA_FOUND);
	}

	if (m_nOpenType != dynamic)
	{
		if (IsEOF() && (wFetchType == SQL_FETCH_NEXT ||
		(wFetchType == SQL_FETCH_RELATIVE && nRows > 0)))
		{
			// if already at EOF, throw an exception
			TRACE0("Error: attempted to move past EOF.\n");
			ThrowDBException(AFX_SQL_ERROR_NO_DATA_FOUND);
		}
		else if (IsBOF() && (wFetchType == SQL_FETCH_PRIOR ||
		(wFetchType == SQL_FETCH_RELATIVE && nRows < 0)))
		{
			// if already at BOF, throw an exception
			TRACE0("Error: attempted to move before BOF.\n");
			ThrowDBException(AFX_SQL_ERROR_NO_DATA_FOUND);
		}
	}
}

RETCODE CRecordset::FetchData(UWORD wFetchType, SDWORD nRow,
	DWORD* pdwRowsFetched)
{
	RETCODE nRetCode;

	if (m_nOpenType == forwardOnly && !(m_dwOptions & useExtendedFetch))
	{
		ASSERT(wFetchType == SQL_FETCH_NEXT);

		AFX_ODBC_CALL(::SQLFetch(m_hstmt));
		*pdwRowsFetched = 1;

		m_bDeleted = FALSE;
	}
	else
	{
		AFX_ODBC_CALL(::SQLExtendedFetch(m_hstmt, wFetchType,
			nRow, pdwRowsFetched, m_rgRowStatus));

		// Set deleted status
		m_bDeleted = GetRowStatus(1) == SQL_ROW_DELETED;
	}

	CheckRowsetError(nRetCode);

	return nRetCode;
}

void CRecordset::SkipDeletedRecords(UWORD wFetchType, long nRows,
	DWORD* pdwRowsFetched, RETCODE* pnRetCode)
{
	ASSERT(!(m_dwOptions & useMultiRowFetch));
	ASSERT(wFetchType == SQL_FETCH_RELATIVE ||
		wFetchType == SQL_FETCH_FIRST ||
		wFetchType == SQL_FETCH_NEXT ||
		wFetchType == SQL_FETCH_LAST ||
		wFetchType == SQL_FETCH_PRIOR);
	ASSERT(nRows != 0);

	UWORD wDeletedFetchType = wFetchType;
	DWORD dwDeletedRows = abs(nRows);
	BOOL m_bDone;

	switch (wFetchType)
	{
	case SQL_FETCH_FIRST:
		wDeletedFetchType = SQL_FETCH_NEXT;
		break;

	case SQL_FETCH_LAST:
		wDeletedFetchType = SQL_FETCH_PRIOR;
		break;

	case SQL_FETCH_RELATIVE:
		if (nRows > 0)
			wDeletedFetchType = SQL_FETCH_NEXT;
		else
			wDeletedFetchType = SQL_FETCH_PRIOR;
		break;
	}

	// First fetch is as expected unless relative fetch
	if (wFetchType != SQL_FETCH_RELATIVE)
	{
		*pnRetCode = FetchData(wFetchType, 1, pdwRowsFetched);
		m_bDone = !m_bDeleted;
	}
	else
	{
		// Since deleted records must be skipped Move(n)
		// must be turned into n MoveNext/MovePrev calls
		*pnRetCode = FetchData(wDeletedFetchType, 1, pdwRowsFetched);
		if (!m_bDeleted)
		{
			dwDeletedRows--;
			m_bDone = dwDeletedRows == 0;
		}
		else
			m_bDone = FALSE;
	}

	// Continue fetching until all req'd deleted records skipped
	while (*pnRetCode != SQL_NO_DATA_FOUND && !m_bDone)
	{
		*pnRetCode = FetchData(wDeletedFetchType, 1, pdwRowsFetched);

		if (wFetchType == SQL_FETCH_RELATIVE)
		{
			if (!m_bDeleted)
			{
				dwDeletedRows--;
				m_bDone = dwDeletedRows == 0;
			}
			else
				m_bDone = FALSE;
		}
		else
			m_bDone = !m_bDeleted;
	}
}

void CRecordset::SetRowsetCurrencyStatus(RETCODE nRetCode,
	UWORD wFetchType, long nRows, DWORD dwRowsFetched)
{
	UNUSED_ALWAYS(dwRowsFetched);

	// Set the fetch direction
	int nDirection = 0;

	switch (wFetchType)
	{
	case SQL_FETCH_FIRST:
		nDirection = 1;
		if (nRetCode == SQL_NO_DATA_FOUND)
		{
			m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
			m_lRecordCount = 0;
		}
		else
			m_lCurrentRecord = 0;
		break;

	case SQL_FETCH_NEXT:
		nDirection = 1;
		AfxSetCurrentRecord(&m_lCurrentRecord, nRows, nRetCode);
		AfxSetRecordCount(&m_lRecordCount, m_lCurrentRecord,
			m_bEOFSeen, nRetCode);

		// This is the only way to know you've hit the end (m_bEOFSeen)
		if (!m_bEOFSeen && nRetCode == SQL_NO_DATA_FOUND &&
			m_lRecordCount == m_lCurrentRecord + 1)
		{
			m_bEOFSeen = TRUE;
		}
		break;

	case SQL_FETCH_LAST:
		nDirection = -1;
		if (nRetCode == SQL_NO_DATA_FOUND)
		{
			m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
			m_lRecordCount = 0;
		}
		else if (m_bEOFSeen)
			m_lCurrentRecord = m_lRecordCount - 1;
		else
			m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
		break;

	case SQL_FETCH_PRIOR:
		nDirection = -1;
		// If doing MovePrev after m_bEOF, don't increment current rec
		if (!m_bEOF)
			AfxSetCurrentRecord(&m_lCurrentRecord, nRows, nRetCode);
		break;

	case SQL_FETCH_RELATIVE:
		nDirection = nRows;
		AfxSetCurrentRecord(&m_lCurrentRecord, nRows, nRetCode);
		AfxSetRecordCount(&m_lRecordCount, m_lCurrentRecord,
			m_bEOFSeen, nRetCode);
		break;

	case SQL_FETCH_ABSOLUTE:
		nDirection = nRows;
		if (nRetCode != SQL_NO_DATA_FOUND)
		{
			if (nRows > 0)
				m_lCurrentRecord = nRows - 1;
			else if (m_bEOFSeen)
				m_lCurrentRecord = m_lRecordCount + nRows;
			else
				m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
		}
		else
			m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;

		AfxSetRecordCount(&m_lRecordCount, m_lCurrentRecord,
			m_bEOFSeen, nRetCode);
		break;

	case SQL_FETCH_BOOKMARK:
		nDirection = 0;
		m_lCurrentRecord = AFX_CURRENT_RECORD_UNDEFINED;
		break;
	}

	// Set the BOF/EOF flags
	if (nRetCode == SQL_NO_DATA_FOUND)
	{
		if (wFetchType == SQL_FETCH_FIRST || wFetchType == SQL_FETCH_LAST ||
			wFetchType == SQL_FETCH_BOOKMARK)
		{
			// If MoveFirst/MoveLast fails, result set is empty
			// If SetBookmark fails, currency undefined
			m_bEOF = m_bBOF = TRUE;
		}
		else
		{
			m_bEOF = nDirection >= 0;
			m_bBOF = !m_bEOF;
		}
	}
	else
	{
		m_bEOF = m_bBOF = FALSE;
	}
}

void CRecordset::RefreshRowset(WORD wRow, WORD wLockType)
{
	ASSERT(IsOpen());
	ASSERT(m_dwOptions & useMultiRowFetch);

	RETCODE nRetCode;

	AFX_ODBC_CALL(::SQLSetPos(m_hstmt, wRow, SQL_REFRESH, wLockType));

	// Need to fixup bound fields in some cases
	if (m_nFields > 0 && !IsEOF() && !IsBOF() &&
		!(m_dwOptions & useMultiRowFetch))
	{
		Fixups();
	}
}

void CRecordset::SetRowsetCursorPosition(WORD wRow, WORD wLockType)
{
	ASSERT(IsOpen());
	ASSERT(m_dwOptions & useMultiRowFetch);

	RETCODE nRetCode;

	AFX_ODBC_CALL(::SQLSetPos(m_hstmt, wRow, SQL_POSITION, wLockType));
}

// "SELECT <user column name list> FROM <table name>"
void CRecordset::BuildSelectSQL()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	// Ignore queries with procedure call keyword or output param
	if (!(_tcsnicmp(m_strSQL, _afxCall, lstrlen(_afxCall)-1) == 0 ||
		_tcsnicmp(m_strSQL, _afxParamCall, lstrlen(_afxParamCall)-1) == 0))
	{
		// Ignore queries already built
		if (_tcsnicmp(m_strSQL, _afxSelect, lstrlen(_afxSelect)-1) != 0)
		{
			// Assume m_strSQL specifies table name
			ASSERT(m_nFields != 0);

			CString strTableName;
			strTableName = m_strSQL;
			m_strSQL.Empty();
			m_strSQL = _afxSelect;

			// Set all fields dirty. AppendNames only outputs dirty field names
			SetFieldDirty(NULL);
			if (AppendNames(&m_strSQL, _T(",")) == 0)
			{
				TRACE0("Error: no field names - at least 1 required.\n");
				ThrowDBException(AFX_SQL_ERROR_EMPTY_COLUMN_LIST);
			}

			// Overwrite final ',' separator with ' '
			ASSERT(m_strSQL[m_strSQL.GetLength()-1] == ',');
			m_strSQL.SetAt(m_strSQL.GetLength()-1, ' ');

			m_strSQL += _afxFrom;
			m_strSQL += strTableName;
		}
	}
}

// Add the filter and sort strings to query SQL
void CRecordset::AppendFilterAndSortSQL()
{
	if (!m_strFilter.IsEmpty())
	{
		m_strSQL += _afxWhere;
		m_strSQL += m_strFilter;
	}

	if (!m_strSort.IsEmpty())
	{
		m_strSQL += _afxOrderBy;
		m_strSQL += m_strSort;
	}
}

// Check for required SQLGetData support and do limited SQL parsing
BOOL CRecordset::IsRecordsetUpdatable()
{
	// Do limited SQL parsing to determine if SQL updatable
	if (!IsSQLUpdatable(m_strSQL))
		return FALSE;

	// Updatable recordsets with long binary columns must support
	// SQL_GD_BOUND to use SQLSetPos, otherwise must use SQL updates
	BOOL bUpdatable = TRUE;
	if (m_bLongBinaryColumns && !m_bUseUpdateSQL)
	{
		// Set non-updatable if you can't use SQLGetData on bound columns
		if (!(m_pDatabase->m_dwUpdateOptions & AFX_SQL_GDBOUND))
		{
			// Okay can't use SetPos, try and use positioned update SQL
			if (m_pDatabase->m_dwUpdateOptions & AFX_SQL_POSITIONEDSQL)
			{
				m_bUseUpdateSQL = TRUE;
#ifdef _DEBUG
				if (afxTraceFlags & traceDatabase)
				{
					TRACE0("Warning: Can't use SQLSetPos due to lack of SQLGetData support.\n");
					TRACE0("\tWill use positioned update SQL.\n");
				}
#endif
			}
			else
			{
#ifdef _DEBUG
				if (afxTraceFlags & traceDatabase)
					TRACE0("Warning: Setting recordset read only due to lack of SQLGetData support.\n");
#endif
				bUpdatable = FALSE;
			}
		}
	}

	return bUpdatable;
}

// Execute the update (or delete) using SQLSetPos
void CRecordset::ExecuteSetPosUpdate()
{
	UWORD wExpectedRowStatus;
	UWORD wPosOption;
	if (m_nEditMode == noMode)
	{
		wPosOption = SQL_DELETE;
		wExpectedRowStatus = SQL_ROW_DELETED;
	}
	else
	{
		if (m_nEditMode == edit)
		{
			wPosOption = SQL_UPDATE;
			wExpectedRowStatus = SQL_ROW_UPDATED;
		}
		else
		{
			wPosOption = SQL_ADD;
			wExpectedRowStatus = SQL_ROW_ADDED;
		}
	}

	BindFieldsForUpdate();

	RETCODE nRetCode;
	AFX_ODBC_CALL(::SQLSetPos(m_hstmt, 1, wPosOption, SQL_LOCK_NO_CHANGE));
	if (!Check(nRetCode))
	{
		TRACE0("Error: failure updating record.\n");
		AfxThrowDBException(nRetCode, m_pDatabase, m_hstmt);
	}
	// Only have data-at-execution columns for CLongBinary columns
	if (nRetCode == SQL_NEED_DATA)
		SendLongBinaryData(m_hstmt);
	// This should only fail if SQLSetPos returned SQL_SUCCESS_WITH_INFO explaining why
	if (nRetCode == SQL_SUCCESS_WITH_INFO && GetRowStatus(1) != wExpectedRowStatus)
		ThrowDBException(AFX_SQL_ERROR_UPDATE_DELETE_FAILED);

	UnbindFieldsForUpdate();
}

// Prepare for sending update SQL by initializing m_hstmtUpdate
void CRecordset::PrepareUpdateHstmt()
{
	RETCODE nRetCode;
	if (m_hstmtUpdate == SQL_NULL_HSTMT)
	{
		AFX_SQL_SYNC(::SQLAllocStmt(m_pDatabase->m_hdbc, &m_hstmtUpdate));
		if (!Check(nRetCode))
		{
			TRACE0("Error: failure to allocate update statement.\n");
			AfxThrowDBException(nRetCode, m_pDatabase, m_hstmtUpdate);
		}
	}
	else
	{
		AFX_SQL_SYNC(::SQLFreeStmt(m_hstmtUpdate, SQL_CLOSE));
		if (!Check(nRetCode))
			goto LErrRetCode;

		// Re-use (prepared) hstmt & param binding if optimizeBulkAdd option
		if(!(m_dwOptions & optimizeBulkAdd))
		{
			AFX_SQL_SYNC(::SQLFreeStmt(m_hstmtUpdate, SQL_RESET_PARAMS));
			if (!Check(nRetCode))
			{
	LErrRetCode:
				// Bad hstmt, free it and allocate another one
				AFX_SQL_SYNC(::SQLFreeStmt(m_hstmtUpdate, SQL_DROP));
				m_hstmtUpdate = SQL_NULL_HSTMT;

				AFX_SQL_SYNC(::SQLAllocStmt(m_pDatabase->m_hdbc, &m_hstmtUpdate));
				if (!Check(nRetCode))
				{
					TRACE0("Error: failure to allocate update statement.\n");
					AfxThrowDBException(nRetCode, m_pDatabase, m_hstmtUpdate);
				}
			}
		}
	}
}

// Build the UPDATE, INSERT or DELETE SQL
void CRecordset::BuildUpdateSQL()
{
	switch (m_nEditMode)
	{
	case noMode:
		// DELETE FROM <tablename> WHERE CURRENT OF
		{
			m_strUpdateSQL = _T("DELETE FROM ");
			m_strUpdateSQL += m_strTableName;
		}
		break;

	case addnew:
		// INSERT INTO <tablename> (<colname1>[,<colname2>]) VALUES (?[,?])
		{
			m_strUpdateSQL = _T("INSERT INTO ");
			m_strUpdateSQL += m_strTableName;

			m_strUpdateSQL += _T(" (");

			// Append column names
			AppendNames(&m_strUpdateSQL, _afxComma);

			// overwrite last ',' with ')'
			ASSERT(m_strUpdateSQL[m_strUpdateSQL.GetLength()-1] == ',');
			m_strUpdateSQL.SetAt(m_strUpdateSQL.GetLength()-1, ')');

			// Append values
			m_strUpdateSQL += _T(" VALUES (");
			AppendValues(m_hstmtUpdate, &m_strUpdateSQL, _afxComma);

			// overwrite last ',' with ')'
			ASSERT(m_strUpdateSQL[m_strUpdateSQL.GetLength()-1] == ',');
			m_strUpdateSQL.SetAt(m_strUpdateSQL.GetLength()-1, ')');
		}
		break;

	case edit:
		// UPDATE <tablename> SET <colname1>=?[,<colname2>=?] WHERE CURRENT OF
		{
			m_strUpdateSQL = _T("UPDATE ");
			m_strUpdateSQL += m_strTableName;

			m_strUpdateSQL += _T(" SET ");
			AppendNamesValues(m_hstmtUpdate, &m_strUpdateSQL, _afxComma);

			// overwrite last ',' with ' '
			ASSERT(m_strUpdateSQL[m_strUpdateSQL.GetLength()-1] == ',');
			m_strUpdateSQL.SetAt(m_strUpdateSQL.GetLength()-1, ' ');
		}
		break;
	}

	// Update and Delete need "WHERE CURRENT OF <cursorname>"
	if (m_nEditMode == edit || m_nEditMode == noMode)
	{
		m_strUpdateSQL += _T(" WHERE CURRENT OF ");

		// Cache cursor name assigned by ODBC
		if (m_strCursorName.IsEmpty())
		{
			// Get predefined cursor name from datasource
			RETCODE nRetCode;
			UCHAR szCursorName[MAX_CURSOR_NAME+1];
			SWORD nLength = _countof(szCursorName)-1;
			AFX_SQL_SYNC(::SQLGetCursorName(m_hstmt,
				szCursorName, _countof(szCursorName), &nLength));
			if (!Check(nRetCode))
				ThrowDBException(nRetCode);
			m_strCursorName = (char*)szCursorName;
		}
		m_strUpdateSQL += m_strCursorName;
	}

	m_pDatabase->ReplaceBrackets(m_strUpdateSQL.GetBuffer(0));
	m_strUpdateSQL.ReleaseBuffer();

	// Must prepare the hstmt on first optimized bulk add
	if(m_dwOptions & firstBulkAdd)
	{
		RETCODE nRetCode;

		USES_CONVERSION;
		AFX_ODBC_CALL(::SQLPrepare(m_hstmtUpdate,
			(UCHAR*)T2A((LPTSTR)(LPCTSTR)m_strUpdateSQL), SQL_NTS));
		if (!Check(nRetCode))
			ThrowDBException(nRetCode, m_hstmtUpdate);
	}
}

void CRecordset::ExecuteUpdateSQL()
{
	RETCODE nRetCode;

	if(!(m_dwOptions & optimizeBulkAdd))
	{
		USES_CONVERSION;
		AFX_ODBC_CALL(::SQLExecDirect(m_hstmtUpdate,
			(UCHAR*)T2A((LPTSTR)(LPCTSTR)m_strUpdateSQL), SQL_NTS));
		if (!Check(nRetCode))
			ThrowDBException(nRetCode, m_hstmtUpdate);
	}
	else
	{
		AFX_ODBC_CALL(::SQLExecute(m_hstmtUpdate));
		if (!Check(nRetCode))
			ThrowDBException(nRetCode, m_hstmtUpdate);
	}

	// Only have data-at-execution parameters for CLongBinary columns
	if (nRetCode == SQL_NEED_DATA)
		SendLongBinaryData(m_hstmtUpdate);

	SDWORD lRowsAffected = 0;

	AFX_SQL_SYNC(::SQLRowCount(m_hstmtUpdate, &lRowsAffected));
	if (!Check(nRetCode) || lRowsAffected == -1)
	{
		// Assume 1 row affected if # rows affected can't be determined
		lRowsAffected = 1;
	}
	else
	{
		if (lRowsAffected != 1)
		{
#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
				TRACE1("Warning: %u rows affected by update operation (expected 1).\n",
					lRowsAffected);
#endif
			ThrowDBException((RETCODE)(lRowsAffected == 0 ?
				AFX_SQL_ERROR_NO_ROWS_AFFECTED :
				AFX_SQL_ERROR_MULTIPLE_ROWS_AFFECTED));
		}
	}
	m_strUpdateSQL.Empty();
}


void CRecordset::SendLongBinaryData(HSTMT hstmt)
{
	RETCODE nRetCode;
	void* pv;
	AFX_ODBC_CALL(::SQLParamData(hstmt, &pv));
	if (!Check(nRetCode))
	{
		// cache away error
		CDBException* pException = new CDBException(nRetCode);
		pException->BuildErrorString(m_pDatabase, hstmt);

		// then cancel Execute operation
		Cancel();
		THROW(pException);
	}

	while (nRetCode == SQL_NEED_DATA)
	{
		CLongBinary* pLongBinary = (CLongBinary*)pv;
		ASSERT_VALID(pLongBinary);

		const BYTE* lpData = (const BYTE*)::GlobalLock(pLongBinary->m_hData);
		ASSERT(lpData != NULL);

		AFX_ODBC_CALL(::SQLPutData(hstmt, (PTR)lpData,
			pLongBinary->m_dwDataLength));

		::GlobalUnlock(pLongBinary->m_hData);

		if (!Check(nRetCode))
		{
			// cache away error
			CDBException* pException = new CDBException(nRetCode);
			pException->BuildErrorString(m_pDatabase, hstmt);

			// then cancel Execute operation
			Cancel();
			THROW(pException);
		}

		// Check for another DATA_AT_EXEC
		AFX_ODBC_CALL(::SQLParamData(hstmt, &pv));
		if (!Check(nRetCode))
		{
			TRACE0("Error: failure handling long binary value during update.\n");
			ThrowDBException(nRetCode, hstmt);
		}
	}
}

//////////////////////////////////////////////////////////////////////////////
// CRecordset RFX implementations

void CRecordset::AllocStatusArrays()
{
	TRY
	{
		if (m_nFields != 0)
		{
			// Allocate buffers to hold field info
			if (m_rgFieldInfos == NULL)
			{
				m_rgFieldInfos = new CFieldInfo[m_nFields];
				memset(m_rgFieldInfos, 0, sizeof(CFieldInfo) * m_nFields);
			}

			if (m_pbFieldFlags == NULL)
			{
				m_pbFieldFlags = new BYTE[m_nFields];
				memset(m_pbFieldFlags, 0, m_nFields);
			}
		}

		if (m_nParams != 0)
		{
			// Allocate buffers to hold param info
			if (m_pbParamFlags == NULL)
			{
				m_pbParamFlags = new BYTE[m_nParams];
				memset(m_pbParamFlags, 0, m_nParams);
			}

			if (m_plParamLength == NULL)
			{
				m_plParamLength = new LONG[m_nParams];
				memset(m_plParamLength, 0, m_nParams*sizeof(LONG));
			}
		}
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL
}

int CRecordset::GetBoundFieldIndex(void* pv)
{
	void* pvIndex;

	if (!m_mapFieldIndex.Lookup(pv, pvIndex))
		return -1;
	else
		// Cached value is short not ptr
		return (int)pvIndex;
}

int CRecordset::GetBoundParamIndex(void* pv)
{
	void* pvIndex;

	if (!m_mapParamIndex.Lookup(pv, pvIndex))
		return -1;
	else
		// Cached value in data not ptr
		return (int)pvIndex;
}

short CRecordset::GetFieldIndexByName(LPCTSTR lpszFieldName)
{
	for (short nIndex = 0; nIndex < GetODBCFieldCount(); nIndex++)
	{
		if (m_rgODBCFieldInfos[nIndex].m_strName == lpszFieldName)
			break;
	}

	// Check if field name found
	if (nIndex == GetODBCFieldCount())
		ThrowDBException(AFX_SQL_ERROR_FIELD_NOT_FOUND);

	return nIndex;
}

long* CRecordset::GetFieldLengthBuffer(DWORD nField, int nFieldType)
{
	if (nFieldType == CFieldExchange::outputColumn)
	{
		ASSERT(nField < m_nFields);
		return &m_rgFieldInfos[nField].m_nLength;
	}
	else
	{
		ASSERT(nField < m_nParams);
		return &m_plParamLength[nField];
	}
}

BYTE CRecordset::GetFieldStatus(DWORD nField)
{
	ASSERT(nField < m_nFields);

	return m_pbFieldFlags[nField];
}

void CRecordset::SetFieldStatus(DWORD nField, BYTE bFlags)
{
	ASSERT(nField < m_nFields);

	m_pbFieldFlags[nField] |= bFlags;
}

void CRecordset::ClearFieldStatus()
{
	memset(m_pbFieldFlags, 0, m_nFields);
}

BOOL CRecordset::IsFieldStatusDirty(DWORD nField) const
{
	ASSERT(nField < m_nFields);

	return m_pbFieldFlags[nField] & AFX_SQL_FIELD_FLAG_DIRTY;
}

void CRecordset::SetDirtyFieldStatus(DWORD nField)
{
	ASSERT(nField < m_nFields);

	m_pbFieldFlags[nField] |= AFX_SQL_FIELD_FLAG_DIRTY;
}

void CRecordset::ClearDirtyFieldStatus(DWORD nField)
{
	ASSERT(nField < m_nFields);

	m_pbFieldFlags[nField] &= ~AFX_SQL_FIELD_FLAG_DIRTY;
}

BOOL CRecordset::IsFieldStatusNull(DWORD nField) const
{
	ASSERT(nField < m_nFields);

	return m_pbFieldFlags[nField] & AFX_SQL_FIELD_FLAG_NULL;
}

void CRecordset::SetNullFieldStatus(DWORD nField)
{
	ASSERT(nField < m_nFields);

	m_pbFieldFlags[nField] |= AFX_SQL_FIELD_FLAG_NULL;
}

void CRecordset::ClearNullFieldStatus(DWORD nField)
{
	ASSERT(nField < m_nFields);

	m_pbFieldFlags[nField] &= ~AFX_SQL_FIELD_FLAG_NULL;
}

BOOL CRecordset::IsParamStatusNull(DWORD nParam) const
{
	ASSERT(nParam < m_nParams);

	return m_pbParamFlags[nParam] & AFX_SQL_FIELD_FLAG_NULL;
}

void CRecordset::SetNullParamStatus(DWORD nParam)
{
	ASSERT(nParam < m_nParams);

	m_pbParamFlags[nParam] |= AFX_SQL_FIELD_FLAG_NULL;
}

void CRecordset::ClearNullParamStatus(DWORD nParam)
{
	ASSERT(nParam < m_nParams);

	m_pbParamFlags[nParam] &= ~AFX_SQL_FIELD_FLAG_NULL;
}

BOOL CRecordset::IsFieldNullable(DWORD nField) const
{
	ASSERT(nField <= INT_MAX);
	ASSERT((long)nField < GetODBCFieldCount());

	// return TRUE if nulls allowed or if not known
	return m_rgODBCFieldInfos[nField].m_nNullability != SQL_NO_NULLS;
}

UINT CRecordset::BindParams(HSTMT hstmt)
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	CFieldExchange fx(CFieldExchange::BindParam, this);
	fx.m_hstmt = hstmt;

	DoFieldExchange(&fx);

	return fx.m_nParams;
}

void CRecordset::RebindParams(HSTMT hstmt)
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	if (m_bRebindParams)
	{
		CFieldExchange fx(CFieldExchange::RebindParam, this);
		fx.m_hstmt = hstmt;

		DoFieldExchange(&fx);
	}
}

UINT CRecordset::BindFieldsToColumns()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	ASSERT(m_nFieldsBound == 0);
	ASSERT(m_nFields != 0 && m_nFields <= 255);

	CFieldExchange fx(CFieldExchange::BindFieldToColumn, this);
	fx.m_hstmt = m_hstmt;

	// Binding depends on fetch type
	if (m_dwOptions & useMultiRowFetch)
		DoBulkFieldExchange(&fx);
	else
		DoFieldExchange(&fx);

	return fx.m_nFields;
}

void CRecordset::BindFieldsForUpdate()
{
	ASSERT_VALID(this);

	if (m_nEditMode == edit || m_nEditMode == addnew)
	{
		CFieldExchange fx(CFieldExchange::BindFieldForUpdate, this);
		fx.m_hstmt = m_hstmt;
		DoFieldExchange(&fx);
	}
}

void CRecordset::UnbindFieldsForUpdate()
{
	ASSERT_VALID(this);

	if (m_nEditMode == edit || m_nEditMode == addnew)
	{
		CFieldExchange fx(CFieldExchange::UnbindFieldForUpdate, this);
		fx.m_hstmt = m_hstmt;
		DoFieldExchange(&fx);
	}
}

// After Move operation, reflect status and lengths of columns in RFX fields
void CRecordset::Fixups()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);
	ASSERT(m_nFieldsBound != 0);

	CFieldExchange fx(CFieldExchange::Fixup, this);
	fx.m_hstmt = m_hstmt;
	DoFieldExchange(&fx);
}

UINT CRecordset::AppendNames(CString* pstr, LPCTSTR lpszSeparator)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(pstr, sizeof(CString)));
	ASSERT(AfxIsValidString(lpszSeparator));
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	CFieldExchange fx(CFieldExchange::Name, this);
	fx.m_pstr = pstr;
	fx.m_lpszSeparator = lpszSeparator;

	if (m_dwOptions & useMultiRowFetch)
		DoBulkFieldExchange(&fx);
	else
		DoFieldExchange(&fx);

	return fx.m_nFields;
}

// For each "changed" column, append <column name>=<column value>,
UINT CRecordset::AppendNamesValues(HSTMT hstmt, CString* pstr,
	LPCTSTR lpszSeparator)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(pstr, sizeof(CString)));
	ASSERT(AfxIsValidString(lpszSeparator));
	ASSERT(m_hstmt != SQL_NULL_HSTMT);
	ASSERT(hstmt != SQL_NULL_HSTMT);

	CFieldExchange fx(CFieldExchange::NameValue, this);
	fx.m_pstr = pstr;
	fx.m_lpszSeparator = lpszSeparator;
	fx.m_hstmt = hstmt;

	DoFieldExchange(&fx);

	return fx.m_nFields;
}

// For each "changed" column, append <column value>,
UINT CRecordset::AppendValues(HSTMT hstmt, CString* pstr,
	LPCTSTR lpszSeparator)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(pstr, sizeof(CString)));
	ASSERT(AfxIsValidString(lpszSeparator));
	ASSERT(m_hstmt != SQL_NULL_HSTMT);
	ASSERT(hstmt != SQL_NULL_HSTMT);

	CFieldExchange fx(CFieldExchange::Value, this);
	fx.m_pstr = pstr;
	fx.m_lpszSeparator = lpszSeparator;
	fx.m_hstmt = hstmt;

	DoFieldExchange(&fx);

	return fx.m_nFields;
}


// Cache fields of copy buffer in a CMemFile with CArchive
void CRecordset::StoreFields()
{
	ASSERT_VALID(this);
	ASSERT(m_nFieldsBound != 0);

	CFieldExchange fx(CFieldExchange::StoreField, this);
	DoFieldExchange(&fx);
}

// Restore fields of copy buffer from archived memory file
void CRecordset::LoadFields()
{
	ASSERT_VALID(this);
	ASSERT(m_nFieldsBound != 0);

	// Must clear out the old status
	ClearFieldStatus();

	CFieldExchange fx(CFieldExchange::LoadField, this);
	DoFieldExchange(&fx);
}

void CRecordset::MarkForUpdate()
{
	ASSERT_VALID(this);

	CFieldExchange fx(CFieldExchange::MarkForUpdate, this);
	DoFieldExchange(&fx);
}

void CRecordset::MarkForAddNew()
{
	ASSERT_VALID(this);

	CFieldExchange fx(CFieldExchange::MarkForAddNew, this);
	DoFieldExchange(&fx);
}

void CRecordset::AllocDataCache()
{
	ASSERT_VALID(this);

	CFieldExchange fx(CFieldExchange::AllocCache, this);
	DoFieldExchange(&fx);
}

void CRecordset::FreeDataCache()
{
	ASSERT_VALID(this);

	CFieldInfo* pInfo;

	for (DWORD nField = 0; nField < m_nFields; nField++)
	{
		pInfo = &m_rgFieldInfos[nField];

		switch(pInfo->m_nDataType)
		{
		default:
			ASSERT(FALSE);
			// fall through

		// Data not cached
		case AFX_RFX_NO_TYPE:
			break;

		// Types cached by value (sizeof(TYPE) <= sizeof(void*))
		case AFX_RFX_BOOL:
		case AFX_RFX_BYTE:
		case AFX_RFX_INT:
		case AFX_RFX_LONG:
		case AFX_RFX_SINGLE:
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_TEXT:
			delete (CString*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_LPTSTR:
			delete [] LPTSTR(pInfo->m_pvDataCache);
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_DOUBLE:
			delete (double*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_TIMESTAMP:
			delete (TIMESTAMP_STRUCT*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_OLEDATE:
			delete (COleDateTime*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_DATE:
			delete (CTime*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;

		case AFX_RFX_BINARY:
			delete (CByteArray*)pInfo->m_pvDataCache;
			pInfo->m_pvDataCache = NULL;
			break;
		}
	}
}

#ifdef _DEBUG
void CRecordset::DumpFields(CDumpContext& dc) const
{
	CFieldExchange fx(CFieldExchange::DumpField, (CRecordset *)this);
	fx.m_pdcDump = &dc;
	((CRecordset *)this)->DoFieldExchange(&fx);
}
#endif // _DEBUG


// Perform Update (m_nModeEdit == edit), Insert (addnew),
// or Delete (noMode)
BOOL CRecordset::UpdateInsertDelete()
{
	ASSERT_VALID(this);
	ASSERT(m_hstmt != SQL_NULL_HSTMT);

	// Delete mode
	if (m_nEditMode == addnew)
	{
		if (!m_bAppendable)
		{
			TRACE0("Error: attempted to add a record to a read only recordset.\n");
			ThrowDBException(AFX_SQL_ERROR_RECORDSET_READONLY);
		}
	}
	else
	{
		if (!m_bUpdatable)
		{
			TRACE0("Error: attempted to update a read only recordset.\n");
			ThrowDBException(AFX_SQL_ERROR_RECORDSET_READONLY);
		}

		// Requires currency
		if (m_bEOF || m_bBOF || m_bDeleted)
		{
			TRACE0("Error: attempting to update recordset - but no record is current.\n");
			ThrowDBException(AFX_SQL_ERROR_NO_CURRENT_RECORD);
		}
	}

	// Update or AddNew is NOP w/o at least 1 changed field
	if (m_nEditMode != noMode && !IsFieldDirty(NULL))
		return FALSE;

	if (!m_bUseUpdateSQL)
	{
		// Most efficient update method
		ExecuteSetPosUpdate();
	}
	else
	{

		BOOL bNullHstmt = (m_hstmtUpdate == NULL);

		// Make sure m_hstmtUpdate allocated
		PrepareUpdateHstmt();

		// Build update SQL unless optimizing bulk adds and hstmt not NULL
		if(!(m_dwOptions & optimizeBulkAdd) || bNullHstmt)
		{
			// Mark as first bulk add if optimizing
			if(m_dwOptions & optimizeBulkAdd)
			{
				m_dwOptions &= ~optimizeBulkAdd;
				m_dwOptions |= firstBulkAdd;
			}
			BuildUpdateSQL();

			// Reset flag marking first optimization
			if(m_dwOptions & firstBulkAdd)
			{
				m_dwOptions &= ~firstBulkAdd;
				m_dwOptions |= optimizeBulkAdd;
			}
		}
		else
		{
			// Just reset the data lengths and datetime proxies
			AppendValues(m_hstmtUpdate, &m_strUpdateSQL, _afxComma);
		}

		ExecuteUpdateSQL();
	}

	TRY
	{
		// Delete
		switch (m_nEditMode)
		{
		case noMode:
			// Decrement record count
			if (m_lCurrentRecord >= 0)
			{
				if (m_lRecordCount > 0)
					m_lRecordCount--;
				m_lCurrentRecord--;
			}

			// indicate on a deleted record
			m_bDeleted = TRUE;
			// Set all fields to NULL
			SetFieldNull(NULL);
			break;

		case addnew:
			// The recordset may no longer be empty (depending on driver behavior)
			// reset m_bEOF/m_bBOF so Move can be called
			m_bEOF = m_bBOF = FALSE;

			if (m_pDatabase->m_bIncRecordCountOnAdd && m_lCurrentRecord >= 0)
			{
				if (m_lRecordCount != -1)
					m_lRecordCount++;
				m_lCurrentRecord++;
			}

			// Reset the data cache if necessary
			if (m_bCheckCacheForDirtyFields && m_nFields > 0)
				LoadFields();
			break;

		case edit:
			break;
		}

		// Reset the edit mode
		m_nEditMode = noMode;
	}
	END_TRY

	// Unless doing a bulk AddNew, reset the dirty flags
	if (m_bCheckCacheForDirtyFields && !(m_dwOptions & optimizeBulkAdd))
		SetFieldDirty(NULL, FALSE);

	// Must return TRUE since record updated
	return TRUE;
}

// Fetch and alloc algorithms for CLongBinary data when length unknown
long CRecordset::GetLBFetchSize(long lOldSize)
{
	// Make it twice as big
	return lOldSize << 1;
}

long CRecordset::GetLBReallocSize(long lOldSize)
{
	// Make it twice as big (no effect if less than fetch size)
	return lOldSize << 1;
}

short PASCAL CRecordset::GetDefaultFieldType(short nSQLType)
{
	short nFieldType = 0;

	switch (nSQLType)
	{
	case SQL_BIT:
		nFieldType = SQL_C_BIT;
		break;

	case SQL_TINYINT:
		nFieldType = SQL_C_UTINYINT;
		break;

	case SQL_SMALLINT:
		nFieldType = SQL_C_SSHORT;
		break;

	case SQL_INTEGER:
		nFieldType = SQL_C_SLONG;
		break;

	case SQL_REAL:
		nFieldType = SQL_C_FLOAT;
		break;

	case SQL_FLOAT:
	case SQL_DOUBLE:
		nFieldType = SQL_C_DOUBLE;
		break;

	case SQL_DATE:
	case SQL_TIME:
	case SQL_TIMESTAMP:
		nFieldType = SQL_C_TIMESTAMP;
		break;

	case SQL_NUMERIC:
	case SQL_DECIMAL:
	case SQL_BIGINT:
	case SQL_CHAR:
	case SQL_VARCHAR:
	case SQL_LONGVARCHAR:
		nFieldType = SQL_C_CHAR;
		break;

	case SQL_BINARY:
	case SQL_VARBINARY:
	case SQL_LONGVARBINARY:
		nFieldType = SQL_C_BINARY;
		break;

	default:
		ASSERT(FALSE);
	}

	return nFieldType;
}

void* PASCAL CRecordset::GetDataBuffer(CDBVariant& varValue,
	short nFieldType, int* pnLen, short nSQLType, UDWORD nPrecision)
{
	void* pvData = NULL;

	switch (nFieldType)
	{
	case SQL_C_BIT:
		pvData = &varValue.m_boolVal;
		varValue.m_dwType = DBVT_BOOL;
		*pnLen = sizeof(varValue.m_boolVal);
		break;

	case SQL_C_UTINYINT:
		pvData = &varValue.m_chVal;
		varValue.m_dwType = DBVT_UCHAR;
		*pnLen = sizeof(varValue.m_chVal);
		break;

	case SQL_C_SSHORT:
		pvData = &varValue.m_iVal;
		varValue.m_dwType = DBVT_SHORT;
		*pnLen = sizeof(varValue.m_iVal);
		break;

	case SQL_C_SLONG:
		pvData = &varValue.m_lVal;
		varValue.m_dwType = DBVT_LONG;
		*pnLen = sizeof(varValue.m_lVal);
		break;

	case SQL_C_FLOAT:
		pvData = &varValue.m_fltVal;
		varValue.m_dwType = DBVT_SINGLE;
		*pnLen = sizeof(varValue.m_fltVal);
		break;

	case SQL_C_DOUBLE:
		pvData = &varValue.m_dblVal;
		varValue.m_dwType = DBVT_DOUBLE;
		*pnLen = sizeof(varValue.m_dblVal);
		break;

	case SQL_C_TIMESTAMP:
		pvData = varValue.m_pdate = new TIMESTAMP_STRUCT;
		varValue.m_dwType = DBVT_DATE;
		*pnLen = sizeof(*varValue.m_pdate);
		break;

	case SQL_C_CHAR:
		varValue.m_pstring = new CString;
		varValue.m_dwType = DBVT_STRING;

		*pnLen = GetTextLen(nSQLType, nPrecision);

		pvData = varValue.m_pstring->GetBufferSetLength(*pnLen);
		break;


	case SQL_C_BINARY:
		varValue.m_pbinary = new CLongBinary;
		varValue.m_dwType = DBVT_BINARY;

		if (nSQLType == SQL_LONGVARBINARY)
		{
			// pvData can't be NULL, so nLen must be at least 1
			*pnLen = 1;
		}
		else
		{
			// better know the length!
			ASSERT(nPrecision != 0);
			*pnLen = nPrecision;
		}

		varValue.m_pbinary->m_hData = ::GlobalAlloc(GMEM_MOVEABLE, *pnLen);
		varValue.m_pbinary->m_dwDataLength = *pnLen;

		pvData = ::GlobalLock(varValue.m_pbinary->m_hData);
		break;

	default:
		ASSERT(FALSE);
	}

	return pvData;
}

int PASCAL CRecordset::GetTextLen(short nSQLType, UDWORD nPrecision)
{
	int nLen;

	if (nSQLType == SQL_LONGVARCHAR || nSQLType == SQL_LONGVARBINARY)
	{
		// Use a dummy length of 1 (will just get NULL terminator)
		nLen = 1;
	}
	else
	{
		// better know the length
		ASSERT(nPrecision >= 0);

		nLen = nPrecision + 1;

		// If converting Numeric or Decimal to text need
		// room for decimal point and sign in string
		if (nSQLType == SQL_NUMERIC || nSQLType == SQL_DECIMAL)
			nLen += 2;
	}

	return nLen;
}

long PASCAL CRecordset::GetData(CDatabase* pdb, HSTMT hstmt,
	short nFieldIndex, short nFieldType, LPVOID pvData, int nLen,
	short nSQLType)
{
	UNUSED(nSQLType);

	long nActualSize;
	RETCODE nRetCode;

	// Retrieve the column in question
	AFX_ODBC_CALL(::SQLGetData(hstmt, nFieldIndex,
		nFieldType, pvData, nLen, &nActualSize));

	// Ignore data truncated warnings for long data
	if (nRetCode == SQL_SUCCESS_WITH_INFO)
	{
#ifdef _DEBUG
		CDBException e(nRetCode);

		if (afxTraceFlags & traceDatabase)
		{
			CDBException e(nRetCode);
			// Build the error string but don't send nuisance output to TRACE window
			e.BuildErrorString(pdb, hstmt, FALSE);

			// If not a data truncated warning on long var column,
			// then send debug output
			if ((nSQLType != SQL_LONGVARCHAR &&
				nSQLType != SQL_LONGVARBINARY) ||
				(e.m_strStateNativeOrigin.Find(_afxDataTruncated) < 0))
			{
				TRACE1("Warning: ODBC Success With Info on field %d.\n",
					nFieldIndex - 1);
				e.TraceErrorMessage(e.m_strError);
				e.TraceErrorMessage(e.m_strStateNativeOrigin);
			}
		}
#endif // _DEBUG
	}
	else if (nRetCode == SQL_NO_DATA_FOUND)
	{
		TRACE0("Error: GetFieldValue operation failed on field %d.\n");
		TRACE1("\tData already fetched for this field.\n",
			nFieldIndex - 1);
		AfxThrowDBException(nRetCode, pdb, hstmt);
	}
	else if (nRetCode != SQL_SUCCESS)
	{
		TRACE1("Error: GetFieldValue operation failed on field %d.\n",
			nFieldIndex - 1);
		AfxThrowDBException(nRetCode, pdb, hstmt);
	}

	return nActualSize;
}

void PASCAL CRecordset::GetLongCharDataAndCleanup(CDatabase* pdb,
	HSTMT hstmt, short nFieldIndex, long nActualSize, LPVOID* ppvData,
	int nLen, CString& strValue, short nSQLType)
{
	RETCODE nRetCode;

	// Release the buffer now that data has been fetched
	strValue.ReleaseBuffer(nActualSize < nLen ? nActualSize : nLen);

	// If long data, may need to call SQLGetData again
	if (nLen <= nActualSize &&
		(nSQLType == SQL_LONGVARCHAR || nSQLType == SQL_LONGVARBINARY))
	{
		// Reallocate the size (this will copy the data)
		*ppvData = strValue.GetBufferSetLength(nActualSize + 1);

		// Get pointer, skipping over original data, but not the NULL
		int nOldLen = nLen - 1;
		*ppvData = (BYTE*)*ppvData + nOldLen;
		nLen = nActualSize + 1 - nOldLen;

		// Retrieve the column in question
		AFX_ODBC_CALL(::SQLGetData(hstmt, nFieldIndex,
			SQL_C_CHAR, *ppvData, nLen, &nActualSize));
		if (nRetCode == SQL_SUCCESS_WITH_INFO)
		{
#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
			{
				TRACE1("Warning: ODBC Success With Info on field %d.\n",
					nFieldIndex - 1);
				CDBException e(nRetCode);
				e.BuildErrorString(pdb, hstmt);
			}
#endif // _DEBUG
		}
		else if (nRetCode != SQL_SUCCESS)
		{
			TRACE1("Error: GetFieldValue operation failed on field %d.\n",
				nFieldIndex - 1);
			AfxThrowDBException(nRetCode, pdb, hstmt);
		}

		// Release the buffer now that data has been fetched
		strValue.ReleaseBuffer(nActualSize + nOldLen);
	}
}

void PASCAL CRecordset::GetLongBinaryDataAndCleanup(CDatabase* pdb, HSTMT hstmt,
	short nFieldIndex, long nActualSize, LPVOID* ppvData, int nLen,
	CDBVariant& varValue, short nSQLType)
{
	RETCODE nRetCode;

	::GlobalUnlock(varValue.m_pbinary->m_hData);

	// If long data, may need to call SQLGetData again
	if (nLen < nActualSize && nSQLType == SQL_LONGVARBINARY)
	{
		// Reallocate a bigger buffer
		HGLOBAL hOldData = varValue.m_pbinary->m_hData;
		varValue.m_pbinary->m_hData = ::GlobalReAlloc(hOldData,
			nActualSize, GMEM_MOVEABLE);

		// Validate the memory was allocated and can be locked
		if (varValue.m_pbinary->m_hData == NULL)
		{
			// Restore the old handle (not NULL if Realloc failed)
			varValue.m_pbinary->m_hData = hOldData;
			AfxThrowMemoryException();
		}
		varValue.m_pbinary->m_dwDataLength = nActualSize;

		// Get pointer, skipping over original data
		*ppvData = (BYTE*)::GlobalLock(varValue.m_pbinary->m_hData) + nLen;
		int nOldLen = nLen;
		nLen = nActualSize - nOldLen;

		// Retrieve the column in question
		AFX_ODBC_CALL(::SQLGetData(hstmt, nFieldIndex,
			SQL_C_BINARY, *ppvData, nLen, &nActualSize));
		if (nRetCode == SQL_SUCCESS_WITH_INFO)
		{
#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
			{
				TRACE1("Warning: ODBC Success With Info on field %d.\n",
					nFieldIndex - 1);
				CDBException e(nRetCode);
				e.BuildErrorString(pdb, hstmt);
			}
#endif // _DEBUG
		}
		else if (nRetCode != SQL_SUCCESS)
		{
			TRACE1("Error: GetFieldValue operation failed on field %d.\n",
				nFieldIndex - 1);
			AfxThrowDBException(nRetCode, pdb, hstmt);
		}

		ASSERT((int)varValue.m_pbinary->m_dwDataLength ==
			nActualSize + nOldLen);

		// Release the buffer now that data has been fetched
		::GlobalUnlock(varValue.m_pbinary->m_hData);
	}
}

//////////////////////////////////////////////////////////////////////////////
// CRecordset diagnostics

#ifdef _DEBUG
void CRecordset::AssertValid() const
{
	CObject::AssertValid();
	if (m_pDatabase != NULL)
		m_pDatabase->AssertValid();
}

void CRecordset::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_nOpenType = " << m_nOpenType;
	dc << "\nm_strSQL = " << m_strSQL;
	dc << "\nm_hstmt = " << m_hstmt;
	dc << "\nm_bRecordsetDb = " << m_bRecordsetDb;

	dc << "\nm_lOpen = " << m_lOpen;
	dc << "\nm_bScrollable = " << m_bScrollable;
	dc << "\nm_bUpdatable = " << m_bUpdatable;
	dc << "\nm_bAppendable = " << m_bAppendable;

	dc << "\nm_nFields = " << m_nFields;
	dc << "\nm_nFieldsBound = " << m_nFieldsBound;
	dc << "\nm_nParams = " << m_nParams;

	dc << "\nm_bEOF = " << m_bEOF;
	dc << "\nm_bBOF = " << m_bBOF;
	dc << "\nm_bDeleted = " << m_bEOF;

	dc << "\nm_bLockMode = " << m_nLockMode;
	dc << "\nm_nEditMode = " << m_nEditMode;
	dc << "\nm_strCursorName = " << m_strCursorName;
	dc << "\nm_hstmtUpdate = " << m_hstmtUpdate;

	dc << "\nDump values for each field in current record.";
	DumpFields(dc);

	if (dc.GetDepth() > 0)
	{
		if (m_pDatabase == NULL)
			dc << "with no database\n";
		else
			dc << "with database: " << m_pDatabase;
	}
}
#endif // _DEBUG

//////////////////////////////////////////////////////////////////////////////
// Helpers

void AFXAPI AfxSetCurrentRecord(long* plCurrentRecord, long nRows, RETCODE nRetCode)
{
	if (*plCurrentRecord != AFX_CURRENT_RECORD_UNDEFINED &&
		nRetCode != SQL_NO_DATA_FOUND)
		*plCurrentRecord += nRows;
}

void AFXAPI AfxSetRecordCount(long* plRecordCount, long lCurrentRecord,
	long nRows, BOOL bEOFSeen, RETCODE nRetCode)
{
	// This function provided for backward binary compatibility
	UNUSED(nRows); // not used in release build
	ASSERT(nRows == 1);
	AfxSetRecordCount(plRecordCount, lCurrentRecord, bEOFSeen, nRetCode);
}

void AFXAPI AfxSetRecordCount(long* plRecordCount, long lCurrentRecord,
	BOOL bEOFSeen, RETCODE nRetCode)
{
	// If not at the end and haven't yet been to the end, incr count
	if (nRetCode != SQL_NO_DATA_FOUND && !bEOFSeen &&
		lCurrentRecord != AFX_CURRENT_RECORD_UNDEFINED)
	{
		// lCurrentRecord 0-based and it's already been set
		*plRecordCount =
			__max(*plRecordCount, lCurrentRecord + 1);
	}
}

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxDbInl[] = "afxdb.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDbInl
#define _AFXDBCORE_INLINE
#include "afxdb.inl"

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDBException, CException)
IMPLEMENT_DYNAMIC(CDatabase, CObject)
IMPLEMENT_DYNAMIC(CRecordset, CObject)

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_DB_STATE, _afxDbState)

/////////////////////////////////////////////////////////////////////////////
