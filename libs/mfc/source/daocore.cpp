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

#pragma comment(lib, "ole32.lib")

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Maintain DAODBEngine object

_AFX_DAO_STATE::_AFX_DAO_STATE()
{
	m_pDAODBEngine = NULL;
	m_bOleInitialized = FALSE;
}

_AFX_DAO_STATE::~_AFX_DAO_STATE()
{
	// these ASSERTs can trip when:
	// ... there any outstanding workspsace objects
	ASSERT(m_mapWorkspaces.IsEmpty());

	// ... you've not shut down with a call AfxDaoTerm()
	ASSERT(m_pDAODBEngine == NULL);

	// ... OLE wasn't correctly shut down
	ASSERT(!m_bOleInitialized);
}

//////////////////////////////////////////////////////////////////////////
// Helpers

// Index function return value
#define AFX_DAO_DATA_NOT_FOUND                  (-1L)

#define AFX_DAO_FETCH_PRIMARY_PROPERTIES \
	(AFX_DAO_PRIMARY_INFO | AFX_DAO_SECONDARY_INFO | AFX_DAO_ALL_INFO)
#define AFX_DAO_FETCH_SECONDARY_PROPERTIES \
	(AFX_DAO_SECONDARY_INFO | AFX_DAO_ALL_INFO)
#define AFX_DAO_FETCH_ALL_PROPERTIES \
	AFX_DAO_ALL_INFO

// Info helpers
void AFX_CDECL AfxGetFieldInfo(DAOField* pDAOField, CDaoFieldInfo& fieldinfo,
	DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
void AFX_CDECL AfxSetFieldInfo(DAOField* pDAOField, CDaoFieldInfo& fieldinfo);
void AFX_CDECL AfxGetIndexInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo,
	DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
void AFX_CDECL AfxSetIndexInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo);
void AFX_CDECL AfxGetIndexFields(DAOIndex* pDAOIndex,
	DAOIndexFields** ppDAOIndexFields);
void AFX_CDECL AfxGetIndexFieldInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo);
void AFX_CDECL AfxSetIndexFieldInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo);
void AFX_CDECL AfxGetDefaultValue(DAOField* pDAOField, CString& strDefaultValue);
void AFX_CDECL AfxSetDefaultValue(DAOField* pDAOField, CString& strDefaultValue);

// GetRows helper
void AFX_CDECL ThrowGetRowsDaoException(SCODE scode);

// _AFX_DAO_STATE helper
_AFX_DAO_STATE* AFX_CDECL AfxGetDaoState();

//////////////////////////////////////////////////////////////////////////
// Global data

AFX_STATIC_DATA const TCHAR _afxParameters2[] = _T("PARAMETERS ");
AFX_STATIC_DATA const TCHAR _afxSelect2[] = _T("SELECT ");
AFX_STATIC_DATA const TCHAR _afxFrom2[] = _T(" FROM ");
AFX_STATIC_DATA const TCHAR _afxWhere2[] = _T(" WHERE ");
AFX_STATIC_DATA const TCHAR _afxOrderBy2[] = _T(" ORDER BY ");
AFX_STATIC_DATA const TCHAR _afxTransform2[] = _T("TRANSFORM ");
AFX_STATIC_DATA const TCHAR _afxTable2[] = _T("TABLE ");

// Need a static VARIANT for optional DAO parameters
AFX_STATIC_DATA VARIANT _afxOptionalVariant = { VT_ERROR, 0, 0, 0, DISP_E_PARAMNOTFOUND };

// Need a static VARIANT for NULL DAO parameters
AFX_STATIC_DATA VARIANT _afxNullVariant = { VT_NULL, 0, 0, 0, 0 };

//////////////////////////////////////////////////////////////////////////
// Logging helpers

void AFXAPI AfxDaoCheck(SCODE scode, LPCSTR lpszDaoCall,
	LPCSTR lpszFile, int nLine, int nError, BOOL bMemOnly)
{
	UNUSED(lpszDaoCall);
	UNUSED(lpszFile);
	UNUSED(nLine);

	if (FAILED(scode))
	{
#ifdef _DEBUG
		if (afxTraceFlags & traceDatabase)
		{
			TRACE0("\nDAO Call Failed.");
			TRACE1("\n\t%hs", lpszDaoCall);
			TRACE2("\n\tIn file %hs on line %d", lpszFile, nLine);
			TRACE1("\n\tscode = %X\n", scode);
		}
#endif
		if (scode == E_OUTOFMEMORY)
			AfxThrowMemoryException();
		else if (!bMemOnly)
			AfxThrowDaoException(nError, scode);
	}
}

#ifdef _DEBUG
void AFXAPI AfxDaoTrace(SCODE scode, LPCSTR lpszDaoCall,
	LPCSTR lpszFile, int nLine)
{
	if (FAILED(scode))
	{
		if (afxTraceFlags & traceDatabase)
		{
			TRACE0("\nDAO Call Failed.\n\t");
			TRACE1("\n%hs", lpszDaoCall);
			TRACE2("\nIn file %hs on line %d\n", lpszFile, nLine);
			TRACE1("scode = %X\n", scode);
		}
	}
}
#endif // _DEBUG

//////////////////////////////////////////////////////////////////////////
// Info structure diagnostics

#ifdef _DEBUG
void CDaoErrorInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoErrorInfo at " << (void*)this;

	dc << "\nm_lErrorCode = " << m_lErrorCode;
	dc << "\nm_strSource = " << m_strSource;
	dc << "\nm_strDescription = " << m_strDescription;
	dc << "\nm_strHelpFile = " << m_strHelpFile;
	dc << "\nm_lHelpContext = " << m_lHelpContext;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoWorkspaceInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoWorkspaceInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_strUserName = " << m_strUserName;
	dc << "\nb = m_bIsolateODBCTrans" << m_bIsolateODBCTrans;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoDatabaseInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoDatabaseInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_bUpdatable = " << m_bUpdatable;
	dc << "\nm_bTransactions = " << m_bTransactions;
	dc << "\nm_strVersion = " << m_strVersion;
	dc << "\nm_lCollatingOrder = " << m_lCollatingOrder;
	dc << "\nm_nQueryTimeout = " << m_nQueryTimeout;
	dc << "\nm_strConnect = " << m_strConnect;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoTableDefInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoTableDefInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_bUpdatable = " << m_bUpdatable;
	dc << "\nm_lAttributes = " << m_lAttributes;
	dc << "\nm_dateDateCreated = " << m_dateCreated;
	dc << "\nm_dateLastUpdated = " << m_dateLastUpdated;
	dc << "\nm_strSrcTableName = " << m_strSrcTableName;
	dc << "\nm_strConnect = " << m_strConnect;
	dc << "\nm_strValidationRule = " << m_strValidationRule;
	dc << "\nm_strValidationText = " << m_strValidationText;
	dc << "\nm_lRecordCount = " << m_lRecordCount;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoFieldInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoFieldInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_nType = " << m_nType;
	dc << "\nm_lSize = " << m_lSize;
	dc << "\nm_lAttributes = " << m_lAttributes;
	dc << "\nm_nOrdinalPosition = " << m_nOrdinalPosition;
	dc << "\nm_bRequired = " << m_bRequired;
	dc << "\nm_bAllowZeroLength = " << m_bAllowZeroLength;
	dc << "\nm_lCollatingOrder = " << m_lCollatingOrder;
	dc << "\nm_strForeignName = " << m_strForeignName;
	dc << "\nm_strSourceField = " << m_strSourceField;
	dc << "\nm_strSourceTable = " << m_strSourceTable;
	dc << "\nm_strValidationRule = " << m_strValidationRule;
	dc << "\nm_strValidationText = " << m_strValidationText;
	dc << "\nm_strDefaultValue = " << m_strDefaultValue;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoIndexFieldInfo::Dump(CDumpContext& dc) const
{
	dc << " a CDaoIndexFieldInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_bDescending = " << m_bDescending;
}
#endif

CDaoIndexInfo::CDaoIndexInfo()
{
	m_pFieldInfos = NULL;
	m_nFields = 0;
	m_bCleanupFieldInfo = FALSE;
}

CDaoIndexInfo::~CDaoIndexInfo()
{
	if (m_bCleanupFieldInfo && m_pFieldInfos != NULL)
	{
		delete[] m_pFieldInfos;
		m_pFieldInfos = NULL;
	}
}

#ifdef _DEBUG
void CDaoIndexInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoIndexInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_nFields = " << m_nFields;

	for (int nIndex = 0; nIndex < m_nFields; nIndex++)
		m_pFieldInfos[nIndex].Dump(dc);

	dc << "\nm_bPrimary = " << m_bPrimary;
	dc << "\nm_bUnique = " << m_bUnique;
	dc << "\nm_bClustered = " << m_bClustered;
	dc << "\nm_bIgnoreNulls = " << m_bIgnoreNulls;
	dc << "\nm_bRequired = " << m_bRequired;
	dc << "\nm_bForeign = " << m_bForeign;
	dc << "\nm_lDistinctCount = " << m_lDistinctCount;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoRelationFieldInfo::Dump(CDumpContext& dc) const
{
	dc << " a CDaoRelationFieldInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_strForeignName = " << m_strForeignName;
}
#endif

CDaoRelationInfo::CDaoRelationInfo()
{
	m_pFieldInfos = NULL;
	m_nFields = 0;
	m_bCleanupFieldInfo = FALSE;
}

CDaoRelationInfo::~CDaoRelationInfo()
{
	if (m_bCleanupFieldInfo && m_pFieldInfos != NULL)
	{
		delete[] m_pFieldInfos;
		m_pFieldInfos = NULL;
	}
}

#ifdef _DEBUG
void CDaoRelationInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoRelationInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_strTable = " << m_strTable;
	dc << "\nm_strForeignTable = " << m_strForeignTable;
	dc << "\nm_lAttributes = " << m_lAttributes;
	dc << "\nm_nFields = " << m_nFields;

	for (int nIndex = 0; nIndex < m_nFields; nIndex++)
		m_pFieldInfos[nIndex].Dump(dc);

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoQueryDefInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoQueryDefInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_nType = " << m_nType;
	dc << "\nm_dateCreated = " << m_dateCreated;
	dc << "\nm_dateLastUpdated = " << m_dateLastUpdated;
	dc << "\nm_bUpdatable = " << m_bUpdatable;
	dc << "\nm_bReturnsRecords = " << m_bReturnsRecords;
	dc << "\nm_strSQL = " << m_strSQL;
	dc << "\nm_strConnect = " << m_strConnect;
	dc << "\nm_nODBCTimeout = " << m_nODBCTimeout;

	dc << "\n";
}
#endif // _DEBUG

#ifdef _DEBUG
void CDaoParameterInfo::Dump(CDumpContext& dc) const
{
	dc << "a CDaoParameterInfo at " << (void*)this;

	dc << "\nm_strName = " << m_strName;
	dc << "\nm_nType = " << m_nType;
	dc << "\nm_varValue = " << m_varValue;

	dc << "\n";
}
#endif // _DEBUG

//////////////////////////////////////////////////////////////////////////
// CDaoException
IMPLEMENT_DYNAMIC(CDaoException, CException)

CDaoException::CDaoException()
{
	m_pDAOError = NULL;
	m_pDAOErrors = NULL;
	m_pErrorInfo = NULL;
}

CDaoException::~CDaoException()
{
	delete m_pErrorInfo;
	m_pErrorInfo = NULL;

	if (m_pDAOErrors != NULL)
	{
		m_pDAOErrors->Release();
		m_pDAOErrors = NULL;
	}

	if (m_pDAOError != NULL)
	{
		m_pDAOError->Release();
		m_pDAOError = NULL;
	}
}

// Operations
short CDaoException::GetErrorCount()
{
	short nErrors = 1;

	if (m_pDAOErrors == NULL)
		InitErrorsCollection();

	if (m_nAfxDaoError != AFX_DAO_ERROR_ENGINE_INITIALIZATION)
		DAO_CHECK_MEM(m_pDAOErrors->get_Count(&nErrors));

	return nErrors;
}

void CDaoException::GetErrorInfo(int nIndex)
{
	ASSERT(m_pDAOError == NULL);

	if (m_pDAOErrors == NULL)
		InitErrorsCollection();

	if (m_nAfxDaoError != AFX_DAO_ERROR_ENGINE_INITIALIZATION)
	{
		// Get DAOError object and fill in error info struct
		DAO_CHECK_MEM(m_pDAOErrors->get_Item(
			COleVariant((long)nIndex), &m_pDAOError));

		FillErrorInfo();

		// Clean up
		m_pDAOError->Release();
		m_pDAOError = NULL;
	}
}

BOOL CDaoException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
	PUINT pnHelpContext)
{
	ASSERT(lpszError != NULL && AfxIsValidString(lpszError, nMaxError));

	BOOL bRetCode = FALSE;

	if (m_pErrorInfo != NULL)
	{
		// DAO help context is not a UINT!
		//  According to DAO it is positive and should be less 10,000,000
		if (pnHelpContext != NULL)
			*pnHelpContext = (UINT) m_pErrorInfo->m_lHelpContext;

		lstrcpyn(lpszError, m_pErrorInfo->m_strDescription, nMaxError-1);
		lpszError[nMaxError-1] = '\0';
		bRetCode = TRUE;
	}
	else
	{
		// Must be MFC DAO class internal error, get error string
		CString strError;
		if (strError.LoadString(
			AFX_IDP_DAO_FIRST + (m_nAfxDaoError - AFX_DAO_ERROR_MIN)))
		{
			lstrcpyn(lpszError, strError, nMaxError-1);
			bRetCode = TRUE;
		}
		else
			ASSERT(FALSE);  // Couldn't get resource.
	}
	return bRetCode;
}

// Implementation
void CDaoException::InitErrorsCollection()
{
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();

	// Engine not initialized!
	if (pDaoState->m_pDAODBEngine == NULL)
		m_nAfxDaoError = AFX_DAO_ERROR_ENGINE_INITIALIZATION;
	else
		DAO_CHECK_MEM(pDaoState->m_pDAODBEngine->get_Errors(
			&m_pDAOErrors));
}

void CDaoException::FillErrorInfo()
{
	ASSERT(m_pDAOError != NULL);
	// Allocate the error info structure if necessary
	if (m_pErrorInfo == NULL)
		m_pErrorInfo = new CDaoErrorInfo;

	COleVariant var;

	DAO_CHECK_MEM(m_pDAOError->get_Number(
		&m_pErrorInfo->m_lErrorCode));

	DAO_CHECK_MEM(m_pDAOError->get_Source(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	m_pErrorInfo->m_strSource = V_BSTRT(&var);
	var.Clear();

	DAO_CHECK_MEM(m_pDAOError->get_Description(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	m_pErrorInfo->m_strDescription = V_BSTRT(&var);
	var.Clear();

	DAO_CHECK_MEM(m_pDAOError->get_HelpFile(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	m_pErrorInfo->m_strHelpFile = V_BSTRT(&var);
	var.Clear();

	DAO_CHECK_MEM(m_pDAOError->get_HelpContext(
		&m_pErrorInfo->m_lHelpContext));
}

void AFXAPI AfxThrowDaoException(int nAfxDaoError, SCODE scode)
{
	CDaoException* pException;
	pException = new CDaoException;

	pException->m_scode = scode;

	if (nAfxDaoError == NO_AFX_DAO_ERROR)
	{
		TRY
		{
			int nErrors = pException->GetErrorCount();
			for (int nIndex = 0; nIndex < nErrors; nIndex++)
			{
				pException->GetErrorInfo(nIndex);
#ifdef _DEBUG
				if (afxTraceFlags & traceDatabase)
				{
					TRACE1("\nError Code = %d\n",
						pException->m_pErrorInfo->m_lErrorCode);
					TRACE1("Source = %s\n",
						(LPCTSTR)pException->m_pErrorInfo->m_strSource);
					TRACE1("Description = %s\n",
						(LPCTSTR)pException->m_pErrorInfo->m_strDescription);
				}
#endif // _DEBUG
			}
		}
		CATCH_ALL(e)
		{
			// No DAO error info
			e->Delete();
		}
		END_CATCH_ALL
	}
	else
		pException->m_nAfxDaoError = nAfxDaoError;

	THROW(pException);
}


//////////////////////////////////////////////////////////////////////////
// CDaoWorkspace
IMPLEMENT_DYNAMIC(CDaoWorkspace, CObject)

CDaoWorkspace::CDaoWorkspace()
{
	m_pDAOWorkspaces = NULL;
	m_pDAOWorkspace = NULL;
	m_pDAODatabases = NULL;

	m_bOpen = FALSE;
	m_bNew = FALSE;
	m_nStatus = 0;
}

CDaoWorkspace::~CDaoWorkspace()
{
	if (IsOpen())
	{
		m_nStatus |= AFX_DAO_IMPLICIT_CLOSE;
		Close();
	}
	else if (m_bNew)
	{
		// Remove the workspace from the AFX_DAO_STATE's map
		_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
		pDaoState->m_mapWorkspaces.RemoveKey(this);
	}
}

void CDaoWorkspace::Create(LPCTSTR lpszName, LPCTSTR lpszUserName,
	LPCTSTR lpszPassword)
{
	ASSERT_VALID(this);
	ASSERT(!IsOpen());
	ASSERT(lpszUserName != NULL);
	ASSERT(lpszPassword != NULL);

	// Get the DAODBEngine interface and initialize if necessary
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	COleVariant varName(lpszName, VT_BSTRT);
	COleVariant varUserName(lpszUserName, VT_BSTRT);
	COleVariant varPassword(lpszPassword, VT_BSTRT);

	DAO_CHECK(pDaoState->m_pDAODBEngine->_30_CreateWorkspace(
		V_BSTR(&varName), V_BSTR(&varUserName),
		V_BSTR(&varPassword), &m_pDAOWorkspace));

	m_bNew = TRUE;

	// Add the workspace to map of Open/New CDaoWorkspaces
	pDaoState->m_mapWorkspaces.SetAt(this, this);
}

void CDaoWorkspace::Append()
{
	ASSERT_VALID(this);
	ASSERT(m_bNew);
	ASSERT(m_pDAOWorkspace != NULL);

	DAOWorkspaces* pDAOWorkspaces;

	// Get the DAODBEngine interface and initialize if necessary
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->get_Workspaces(&pDAOWorkspaces));

	TRY
	{
		DAO_CHECK(pDAOWorkspaces->Append(m_pDAOWorkspace));
	}
	CATCH_ALL(e)
	{
		pDAOWorkspaces->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOWorkspaces->Release();

	m_bNew = FALSE;
	m_bOpen = TRUE;
}

void CDaoWorkspace::Open(LPCTSTR lpszWorkspaceName)
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOWorkspaces == NULL);
	ASSERT(m_pDAOWorkspace == NULL);

	// Re-Opening is invalid.
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	// Set the workspace name (or use 0 if opening default)
	COleVariant var(lpszWorkspaceName, VT_BSTRT);
	if (lpszWorkspaceName == NULL)
	{
		var = 0L;

		// Set status to prevent DAO Workspace Close call
		m_nStatus |= AFX_DAO_DEFAULT_WS;
	}

	// Get the DAODBEngine interface and initialize if necessary
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	// Get the workspace from the workspaces collection
	DAO_CHECK(pDaoState->m_pDAODBEngine->get_Workspaces(
		&m_pDAOWorkspaces));
	DAO_CHECK(m_pDAOWorkspaces->get_Item(var, &m_pDAOWorkspace));

	m_bOpen = TRUE;
	m_pDAOWorkspaces->Release();
	m_pDAOWorkspaces = NULL;

	// Add the workspace to map of Open/New CDaoWorkspaces
	pDaoState->m_mapWorkspaces.SetAt(this, this);
}

void CDaoWorkspace::Close()
{
	ASSERT_VALID(this);

	if (m_pDAODatabases != NULL)
	{
		m_pDAODatabases->Release();
		m_pDAODatabases = NULL;
	}

	// Close any Open CDaoDatabases
	void* pvKey;
	void* pvObject;
	POSITION pos = m_mapDatabases.GetStartPosition();
	while (pos != NULL)
	{
		m_mapDatabases.GetNextAssoc(pos, pvKey, pvObject);
		((CDaoDatabase*)pvObject)->Close();
	}
	m_mapDatabases.RemoveAll();

	if (m_pDAOWorkspace != NULL)
	{
		// If implicit workspace or close, don't call DAO close.
		// It will be automatically closed when ref count 0.
		if (!(m_nStatus & (AFX_DAO_IMPLICIT_WS | AFX_DAO_IMPLICIT_CLOSE |
			AFX_DAO_DEFAULT_WS)))
		{
			DAO_TRACE(m_pDAOWorkspace->Close());
		}

		m_pDAOWorkspace->Release();
		m_pDAOWorkspace = NULL;
	}

	if (m_pDAOWorkspaces != NULL)
	{
		m_pDAOWorkspaces->Release();
		m_pDAOWorkspaces = NULL;
	}

	m_bOpen = FALSE;
	m_bNew = FALSE;
	m_nStatus &= ~AFX_DAO_DEFAULT_WS;

	// Remove the workspace from the AFX_DAO_STATE's map
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	pDaoState->m_mapWorkspaces.RemoveKey(this);
}

CString PASCAL CDaoWorkspace::GetVersion()
{
	COleVariant var;

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->get_Version(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString PASCAL CDaoWorkspace::GetIniPath()
{
	COleVariant var;
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->get_IniPath(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void PASCAL CDaoWorkspace::SetIniPath(LPCTSTR lpszRegistrySubKey)
{
	COleVariant var(lpszRegistrySubKey, VT_BSTRT);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->put_IniPath(V_BSTR(&var)));
}

void PASCAL CDaoWorkspace::SetDefaultUser(LPCTSTR lpszDefaultUser)
{
	COleVariant var(lpszDefaultUser, VT_BSTRT);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->put_DefaultUser(V_BSTR(&var)));
}

void PASCAL CDaoWorkspace::SetDefaultPassword(LPCTSTR lpszPassword)
{
	COleVariant var(lpszPassword, VT_BSTRT);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->put_DefaultPassword(
		V_BSTR(&var)));
}

short PASCAL CDaoWorkspace::GetLoginTimeout()
{
	short nSeconds;

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->get_LoginTimeout(&nSeconds));
	return nSeconds;
}

void PASCAL CDaoWorkspace::SetLoginTimeout(short nSeconds)
{
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->put_LoginTimeout(nSeconds));
}


CString CDaoWorkspace::GetName()
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOWorkspace != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOWorkspace->get_Name(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString CDaoWorkspace::GetUserName()
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOWorkspace != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOWorkspace->get_UserName(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoWorkspace::SetIsolateODBCTrans(BOOL bIsolateODBCTrans)
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOWorkspace != NULL);

	DAO_CHECK(m_pDAOWorkspace->put_IsolateODBCTrans(
		(short)(bIsolateODBCTrans ? AFX_DAO_TRUE : AFX_DAO_FALSE)));
}

BOOL CDaoWorkspace::GetIsolateODBCTrans()
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOWorkspace != NULL);

	short nIsolateODBCTrans;
	DAO_CHECK(m_pDAOWorkspace->get_IsolateODBCTrans(&nIsolateODBCTrans));
	return nIsolateODBCTrans == AFX_DAO_TRUE;
}

void PASCAL CDaoWorkspace::CompactDatabase(LPCTSTR lpszSrcName,
	LPCTSTR lpszDestName, LPCTSTR lpszLocale, int nOptions,
	LPCTSTR lpszPassword)
{
	COleVariant varSrcName(lpszSrcName, VT_BSTRT);
	COleVariant varDestName(lpszDestName, VT_BSTRT);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->CompactDatabase(
		V_BSTR(&varSrcName), V_BSTR(&varDestName),
		COleVariant(lpszLocale, VT_BSTRT), COleVariant((long)nOptions),
		lpszPassword != NULL ? (VARIANT)COleVariant(lpszPassword, VT_BSTRT) :
		_afxOptionalVariant));
}

void PASCAL CDaoWorkspace::CompactDatabase(LPCTSTR lpszSrcName,
	LPCTSTR lpszDestName, LPCTSTR lpszLocale, int nOptions)
{
	CompactDatabase(lpszSrcName, lpszDestName, lpszLocale, nOptions, NULL);
}

void PASCAL CDaoWorkspace::RepairDatabase(LPCTSTR lpszName)
{
	COleVariant varName(lpszName, VT_BSTRT);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->RepairDatabase(
		V_BSTR(&varName)));
}

void PASCAL CDaoWorkspace::Idle(int nAction)
{
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->Idle(
		COleVariant((long)nAction)));
}

short CDaoWorkspace::GetWorkspaceCount()
{
	ASSERT_VALID(this);

	short nFields;

	if (m_pDAOWorkspaces == NULL)
		InitWorkspacesCollection();

	DAO_CHECK(m_pDAOWorkspaces->get_Count(&nFields));
	return nFields;
}

void CDaoWorkspace::GetWorkspaceInfo(int nIndex,
	CDaoWorkspaceInfo& wsinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOWorkspaces == NULL)
		InitWorkspacesCollection();

	// Get DAOWorkspace object and fill in workspace info struct
	DAOWorkspace* pDAOWorkspace;
	DAO_CHECK(m_pDAOWorkspaces->get_Item(
		COleVariant((long)nIndex), &pDAOWorkspace));
	FillWorkspaceInfo(pDAOWorkspace, wsinfo, dwInfoOptions);

	// Clean up
	pDAOWorkspace->Release();
}

void CDaoWorkspace::GetWorkspaceInfo(
	LPCTSTR lpszName, CDaoWorkspaceInfo& wsinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOWorkspaces == NULL)
		InitWorkspacesCollection();

	// Get DAOWorkspace object and fill in workspace info struct
	DAOWorkspace* pDAOWorkspace;
	DAO_CHECK(m_pDAOWorkspaces->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOWorkspace));
	FillWorkspaceInfo(pDAOWorkspace, wsinfo, dwInfoOptions);

	// Clean up
	pDAOWorkspace->Release();
}

short CDaoWorkspace::GetDatabaseCount()
{
	ASSERT_VALID(this);

	if (m_pDAODatabases == NULL)
		InitDatabasesCollection();

	short nFields;
	DAO_CHECK(m_pDAODatabases->get_Count(&nFields));
	return nFields;
}

void CDaoWorkspace::GetDatabaseInfo(int nIndex, CDaoDatabaseInfo& dbinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAODatabases == NULL)
		InitDatabasesCollection();

	// Get DAODatabase object and fill in database info struct
	DAODatabase* pDAODatabase;
	DAO_CHECK(m_pDAODatabases->get_Item(
		COleVariant((long)nIndex), &pDAODatabase));
	FillDatabaseInfo(pDAODatabase, dbinfo, dwInfoOptions);

	// Clean up
	pDAODatabase->Release();
}

void CDaoWorkspace::GetDatabaseInfo(LPCTSTR lpszName,
	CDaoDatabaseInfo& dbinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAODatabases == NULL)
		InitDatabasesCollection();

	// Get DAODatabase object and fill in database info struct
	DAODatabase* pDAODatabase;
	DAO_CHECK(m_pDAODatabases->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAODatabase));
	FillDatabaseInfo(pDAODatabase, dbinfo, dwInfoOptions);

	// Clean up
	pDAODatabase->Release();
}

void CDaoWorkspace::BeginTrans()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOWorkspace != NULL);

	DAO_CHECK(m_pDAOWorkspace->BeginTrans());
}

// Determine whether to use DAO 3.6, 3.5, or 3.0
// Use DAO 3.0 if DLL build and not built with MFC 4.21 or later
// Use DAO 3.6 if MFC 6.01 or later
// otherwise, DAO 3.5

#ifndef _AFXDLL
#if _MFC_VER >= 0x0601
#define _AfxDetermineDaoVersion()   (36)
#else
#define _AfxDetermineDaoVersion()   (35)
#endif
#else // dynamically because of DLL
static inline BYTE _AfxDetermineDaoVersion()
{
	BYTE bReturn = 35;

#ifdef _AFXDLL
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	if (pModuleState->m_dwVersion < 0x421)
		bReturn = 30;
	else if (pModuleState->m_dwVersion >= 0x0601)
		bReturn = 36;
#endif // _AFXDLL

	return bReturn;
}
#endif

void CDaoWorkspace::CommitTrans()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOWorkspace != NULL);

   BYTE bUseDao = _AfxDetermineDaoVersion();

	if (bUseDao == 35 || bUseDao == 36)
	{
		// Call DAO 3.5 or 3.6 method with no option set.
		// CommitTrans option parameter not yet supported.
		DAO_CHECK(m_pDAOWorkspace->CommitTrans(0));
	}
	else
	{
		// Call DAO 3.0 method
		// The DAO 3.0 version of CommitTrans takes no params
		// so cast CommitTrans to method that takes no params.
		HRESULT (STDMETHODCALLTYPE DAOWorkspace::*pMethod)() = (HRESULT (STDMETHODCALLTYPE DAOWorkspace::*)())m_pDAOWorkspace->CommitTrans;
		DAO_CHECK((m_pDAOWorkspace->*pMethod)());
	}
}

void CDaoWorkspace::Rollback()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOWorkspace != NULL);

	DAO_CHECK(m_pDAOWorkspace->Rollback());
}

//Implementation
void AFX_CDECL CDaoWorkspace::InitializeEngine()
{
	AfxDaoInit();
}

void CDaoWorkspace::InitWorkspacesCollection()
{
	ASSERT_VALID(this);

	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();
	if (pDaoState->m_pDAODBEngine == NULL)
		InitializeEngine();

	DAO_CHECK(pDaoState->m_pDAODBEngine->get_Workspaces(
		&m_pDAOWorkspaces));
}

void CDaoWorkspace::FillWorkspaceInfo(DAOWorkspace* pDAOWorkspace,
	CDaoWorkspaceInfo& wsinfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAOWorkspace != NULL);
	ASSERT(dwOptions != 0);

	COleVariant var;
	short nBool;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAOWorkspace->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		wsinfo.m_strName = V_BSTRT(&var);
		var.Clear();
	}

	if (dwOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAOWorkspace->get_UserName(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		wsinfo.m_strUserName = V_BSTRT(&var);
		var.Clear();
	}

	if (dwOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		DAO_CHECK(pDAOWorkspace->get_IsolateODBCTrans(&nBool));
		wsinfo.m_bIsolateODBCTrans = nBool == AFX_DAO_TRUE;
	}
}

void CDaoWorkspace::InitDatabasesCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAOWorkspace->get_Databases(&m_pDAODatabases));
}

void CDaoWorkspace::FillDatabaseInfo(DAODatabase* pDAODatabase,
	CDaoDatabaseInfo& dbinfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAODatabase != NULL);
	ASSERT(dwOptions != 0);

	COleVariant var;
	short nBool;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAODatabase->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		dbinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAODatabase->get_Updatable(&nBool));
		dbinfo.m_bUpdatable = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAODatabase->get_Transactions(&nBool));
		dbinfo.m_bTransactions = nBool == AFX_DAO_TRUE;
	}

	if (dwOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAODatabase->get_Version(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		dbinfo.m_strVersion = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAODatabase->get_CollatingOrder(
			&dbinfo.m_lCollatingOrder));

		DAO_CHECK(pDAODatabase->get_QueryTimeout(
			&dbinfo.m_nQueryTimeout));
	}

	if (dwOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		DAO_CHECK(pDAODatabase->get_Connect(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		dbinfo.m_strConnect = V_BSTRT(&var);
		var.Clear();
	}
}

void CDaoWorkspace::ThrowDaoException(int nAfxDaoError)
{
	ASSERT_VALID(this);

	AfxThrowDaoException(nAfxDaoError);
}

#ifdef _DEBUG
void CDaoWorkspace::AssertValid() const
{
	CObject::AssertValid();
}

void CDaoWorkspace::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CObject::Dump(dc);

	dc << "m_bOpen = " << m_bOpen;
	dc << "\nm_bNew = " << m_bNew;
	dc << "\nm_nStatus = " << m_nStatus;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////
// CDaoDatabase
IMPLEMENT_DYNAMIC(CDaoDatabase, CObject)

CDaoDatabase::CDaoDatabase(CDaoWorkspace* pWorkspace)
{
	m_bOpen = FALSE;

	m_pDAODatabase = NULL;

	m_pDAOTableDefs = NULL;
	m_pDAORelations = NULL;
	m_pDAOQueryDefs = NULL;
	m_pDAORecordsets = NULL;

	m_pWorkspace = pWorkspace;
	m_nStatus = 0;
}

CDaoDatabase::~CDaoDatabase()
{
	if (IsOpen())
		Close();

	// Clean up workspace if necessary
	if (m_pWorkspace != NULL && (m_nStatus & AFX_DAO_IMPLICIT_WS))
	{
		m_pWorkspace->Close();
		delete m_pWorkspace;
		m_pWorkspace = NULL;
	}
}

void CDaoDatabase::Create(LPCTSTR lpszName, LPCTSTR lpszLocale,
	int nOptions)
{
	ASSERT_VALID(this);
	ASSERT(!IsOpen());

	// Allocate and maintain workspace if necessary
	InitWorkspace();

	COleVariant varName(lpszName, VT_BSTRT);
	COleVariant varLocale(lpszLocale, VT_BSTRT);

	DAO_CHECK(m_pWorkspace->m_pDAOWorkspace->CreateDatabase(V_BSTR(&varName),
		V_BSTR(&varLocale), COleVariant((long)nOptions),
		&m_pDAODatabase));

	m_bOpen = TRUE;

	// Add the database to map of Open CDaoDatabases
	m_pWorkspace->m_mapDatabases.SetAt(this, this);
}

void CDaoDatabase::Open(LPCTSTR lpszName, BOOL bExclusive,
	BOOL bReadOnly, LPCTSTR lpszConnect)
{
	ASSERT_VALID(this);

	// Re-Opening is invalid.
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	// Allocate, maintain and/or open workspace if necessary
	InitWorkspace();

	COleVariant var(lpszName, VT_BSTRT);

	DAO_CHECK(m_pWorkspace->m_pDAOWorkspace->OpenDatabase(
		V_BSTR(&var),
		COleVariant((long)bExclusive, VT_BOOL),
		COleVariant((long)bReadOnly, VT_BOOL),
		COleVariant(lpszConnect, VT_BSTRT),
		&m_pDAODatabase));

	m_bOpen = TRUE;

	// Add the database to map of Open CDaoDatabases
	m_pWorkspace->m_mapDatabases.SetAt(this, this);
}

// Disconnect connection
void CDaoDatabase::Close()
{
	ASSERT_VALID(this);

	if (m_pDAORecordsets != NULL)
	{
		m_pDAORecordsets->Release();
		m_pDAORecordsets = NULL;
	}

	if (m_pDAOQueryDefs != NULL)
	{
		m_pDAOQueryDefs->Release();
		m_pDAOQueryDefs = NULL;
	}

	if (m_pDAORelations != NULL)
	{
		m_pDAORelations->Release();
		m_pDAORelations = NULL;
	}

	if (m_pDAOTableDefs != NULL)
	{
		m_pDAOTableDefs->Release();
		m_pDAOTableDefs = NULL;
	}

	// Close any Open CDaoRecordsets
	void* pvKey;
	void* pvObject;
	POSITION pos = m_mapRecordsets.GetStartPosition();
	while (pos != NULL)
	{
		m_mapRecordsets.GetNextAssoc(pos, pvKey, pvObject);
		((CDaoRecordset*)pvObject)->Close();
	}
	m_mapRecordsets.RemoveAll();

	// Close any Open CDaoQueryDefs
	pos = m_mapQueryDefs.GetStartPosition();
	while (pos != NULL)
	{
		m_mapQueryDefs.GetNextAssoc(pos, pvKey, pvObject);
		((CDaoQueryDef*)pvObject)->Close();
	}
	m_mapQueryDefs.RemoveAll();

	// Close any Open CDaoTableDefs
	pos = m_mapTableDefs.GetStartPosition();
	while (pos != NULL)
	{
		m_mapTableDefs.GetNextAssoc(pos, pvKey, pvObject);
		((CDaoTableDef*)pvObject)->Close();
	}
	m_mapTableDefs.RemoveAll();

	if (m_pDAODatabase != NULL)
	{
		// If implicit database, don't close.
		// It will be automatically closed when ref count 0.
		if (!(m_nStatus & AFX_DAO_IMPLICIT_DB))
			DAO_TRACE(m_pDAODatabase->Close());

		m_pDAODatabase->Release();
		m_pDAODatabase = NULL;
	}

	m_bOpen = FALSE;

	// Remove the CDaoDatabase from the CDaoWorkspace's map
	m_pWorkspace->m_mapDatabases.RemoveKey(this);
}

BOOL CDaoDatabase::CanUpdate()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	short nUpdatable;
	DAO_CHECK(m_pDAODatabase->get_Updatable(&nUpdatable));
	return nUpdatable == AFX_DAO_TRUE;
}

BOOL CDaoDatabase::CanTransact()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	short nTransactable;
	DAO_CHECK(m_pDAODatabase->get_Transactions(&nTransactable));
	return nTransactable == AFX_DAO_TRUE;
}

CString CDaoDatabase::GetName()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAODatabase->get_Name(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString CDaoDatabase::GetConnect()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAODatabase->get_Connect(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString CDaoDatabase::GetVersion()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAODatabase->get_Version(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

short CDaoDatabase::GetQueryTimeout()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	short nQueryTimeout;
	DAO_CHECK(m_pDAODatabase->get_QueryTimeout(&nQueryTimeout));
	return nQueryTimeout;
}

void CDaoDatabase::SetQueryTimeout(short nSeconds)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	DAO_CHECK(m_pDAODatabase->put_QueryTimeout(nSeconds));
}

long CDaoDatabase::GetRecordsAffected()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAODatabase != NULL);

	long lRecordsAffected;
	DAO_CHECK(m_pDAODatabase->get_RecordsAffected(&lRecordsAffected));
	return lRecordsAffected;
}

void CDaoDatabase::DeleteTableDef(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	if (m_pDAOTableDefs == NULL)
		InitTableDefsCollection();

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDefs->Delete(V_BSTR(&var)));
}

void CDaoDatabase::DeleteQueryDef(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	if (m_pDAOQueryDefs == NULL)
		InitQueryDefsCollection();

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOQueryDefs->Delete(V_BSTR(&var)));
}

void CDaoDatabase::CreateRelation(LPCTSTR lpszName, LPCTSTR lpszTable,
	LPCTSTR lpszForeignTable, long lAttributes, LPCTSTR lpszField,
	LPCTSTR lpszForeignField)
{
	ASSERT_VALID(this);

	CDaoRelationInfo relinfo;
	CDaoRelationFieldInfo fieldinfo;

	relinfo.m_strName = lpszName;
	relinfo.m_strTable = lpszTable;
	relinfo.m_strForeignTable = lpszForeignTable;
	relinfo.m_lAttributes = lAttributes;
	relinfo.m_nFields = 1;

	relinfo.m_pFieldInfos = &fieldinfo;
	relinfo.m_pFieldInfos->m_strName = lpszField;
	relinfo.m_pFieldInfos->m_strForeignName = lpszForeignField;

	CreateRelation(relinfo);
}

void CDaoDatabase::CreateRelation(CDaoRelationInfo& relinfo)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(relinfo.m_nFields > 0);

	// Initialize relations collection so that relation can be appended later
	if (m_pDAORelations == NULL)
		InitRelationsCollection();

	DAORelation* pDAORelation = NULL;
	DAOFields* pDAOFields = NULL;
	DAOField* pDAOField = NULL;

	// Create the relation
	DAO_CHECK(m_pDAODatabase->CreateRelation(
		COleVariant(relinfo.m_strName, VT_BSTRT),
		COleVariant(relinfo.m_strTable, VT_BSTRT),
		COleVariant(relinfo.m_strForeignTable, VT_BSTRT),
		COleVariant(relinfo.m_lAttributes), &pDAORelation));

	TRY
	{
		// Get the fields collection for later append of created field
		DAO_CHECK(pDAORelation->get_Fields(&pDAOFields));

		// Create field(s) and set the name and foreign name
		for (int nIndex = 0; nIndex < relinfo.m_nFields; nIndex++)
		{
			DAO_CHECK(pDAORelation->CreateField(
				COleVariant(relinfo.m_pFieldInfos[nIndex].m_strName, VT_BSTRT),
				_afxOptionalVariant, _afxOptionalVariant, &pDAOField));

			COleVariant var(relinfo.m_pFieldInfos[nIndex].m_strForeignName, VT_BSTRT);
			DAO_CHECK(pDAOField->put_ForeignName(V_BSTR(&var)));

			// Append the field to relation fields collection and release
			DAO_CHECK(pDAOFields->Append(pDAOField));
			pDAOField->Release();
		}

		DAO_CHECK(m_pDAORelations->Append(pDAORelation));
	}
	CATCH_ALL(e)
	{
		// Clean up before throw
		if (pDAOField != NULL)
			pDAOField->Release();

		if (pDAOFields != NULL)
			pDAOFields->Release();

		pDAORelation->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	// Clean up
	if (pDAOField != NULL)
		pDAOField->Release();

	pDAOFields->Release();
	pDAORelation->Release();
}

void CDaoDatabase::DeleteRelation(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	if (m_pDAORelations == NULL)
		InitRelationsCollection();

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAORelations->Delete(V_BSTR(&var)));
}

void CDaoDatabase::Execute(LPCTSTR lpszSQL, int nOptions)
{
	ASSERT_VALID(this);
	ASSERT(m_pDAODatabase != NULL);

	COleVariant var(lpszSQL, VT_BSTRT);
	DAO_CHECK(m_pDAODatabase->Execute(
		V_BSTR(&var), COleVariant((long)nOptions)));
}

short CDaoDatabase::GetTableDefCount()
{
	ASSERT_VALID(this);

	short nTables;

	if (m_pDAOTableDefs == NULL)
		InitTableDefsCollection();

	DAO_CHECK(m_pDAOTableDefs->get_Count(&nTables));
	return nTables;
}

void CDaoDatabase::GetTableDefInfo(int nIndex, CDaoTableDefInfo& tabledefinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOTableDefs == NULL)
		InitTableDefsCollection();

	// Get DAOTableDef object and fill in table info struct
	DAOTableDef* pDAOTableDef;
	DAO_CHECK(m_pDAOTableDefs->get_Item(
		COleVariant((long)nIndex), &pDAOTableDef));
	FillTableDefInfo(pDAOTableDef, tabledefinfo, dwInfoOptions);

	// Clean up
	pDAOTableDef->Release();
}

void CDaoDatabase::GetTableDefInfo(LPCTSTR lpszName,
	CDaoTableDefInfo&  tabledefinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOTableDefs == NULL)
		InitTableDefsCollection();

	// Get DAOTableDef object and fill in table info struct
	DAOTableDef* pDAOTableDef;
	DAO_CHECK(m_pDAOTableDefs->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOTableDef));
	FillTableDefInfo(pDAOTableDef, tabledefinfo, dwInfoOptions);

	// Clean up
	pDAOTableDef->Release();
}

short CDaoDatabase::GetRelationCount()
{
	ASSERT_VALID(this);

	short nRelations;

	if (m_pDAORelations == NULL)
		InitRelationsCollection();

	DAO_CHECK(m_pDAORelations->get_Count(&nRelations));
	return nRelations;
}

void CDaoDatabase::GetRelationInfo(int nIndex, CDaoRelationInfo& relinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAORelations == NULL)
		InitRelationsCollection();

	// Get DAORelation object and fill in relation info struct
	DAORelation* pDAORelation;
	DAO_CHECK(m_pDAORelations->get_Item(
		COleVariant((long)nIndex), &pDAORelation));
	FillRelationInfo(pDAORelation, relinfo, dwInfoOptions);

	// Clean up
	pDAORelation->Release();
}

void CDaoDatabase::GetRelationInfo(LPCTSTR lpszName,
	CDaoRelationInfo& relinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAORelations == NULL)
		InitRelationsCollection();

	// Get DAORelation object and fill in relation info struct
	DAORelation* pDAORelation;
	DAO_CHECK(m_pDAORelations->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAORelation));
	FillRelationInfo(pDAORelation, relinfo, dwInfoOptions);

	// Clean up
	pDAORelation->Release();
}

short CDaoDatabase::GetQueryDefCount()
{
	ASSERT_VALID(this);

	short nQueryDefs;

	if (m_pDAOQueryDefs == NULL)
		InitQueryDefsCollection();

	DAO_CHECK(m_pDAOQueryDefs->get_Count(&nQueryDefs));
	return nQueryDefs;
}

void CDaoDatabase::GetQueryDefInfo(int nIndex, CDaoQueryDefInfo& querydefinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOQueryDefs == NULL)
		InitQueryDefsCollection();

	// Get DAOQueryDef object and fill in query info struct
	DAOQueryDef* pDAOQueryDef;
	DAO_CHECK(m_pDAOQueryDefs->get_Item(
		COleVariant((long)nIndex), &pDAOQueryDef));
	FillQueryDefInfo(pDAOQueryDef, querydefinfo, dwInfoOptions);

	// Clean up
	pDAOQueryDef->Release();
}

void CDaoDatabase::GetQueryDefInfo(LPCTSTR lpszName,
	CDaoQueryDefInfo& querydefinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOQueryDefs == NULL)
		InitQueryDefsCollection();

	// Get DAOQueryDef object and fill in query info struct
	DAOQueryDef* pDAOQueryDef;
	DAO_CHECK(m_pDAOQueryDefs->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOQueryDef));
	FillQueryDefInfo(pDAOQueryDef, querydefinfo, dwInfoOptions);

	// Clean up
	pDAOQueryDef->Release();
}

// Implementation
void CDaoDatabase::InitWorkspace()
{
	ASSERT_VALID(this);

	if (m_pWorkspace == NULL)
	{
		// Allocate workspace and mark as implicit
		m_pWorkspace = new CDaoWorkspace;
		m_pWorkspace->m_nStatus |= AFX_DAO_IMPLICIT_WS;
		m_nStatus |= AFX_DAO_IMPLICIT_WS;
	}

	// Open workspace if not open and not new (unappended)
	if (!m_pWorkspace->IsOpen() && !m_pWorkspace->IsNew())
	{
		// Open the default workspace
		m_pWorkspace->Open();
	}
}

void CDaoDatabase::InitTableDefsCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAODatabase->get_TableDefs(&m_pDAOTableDefs));
}

void CDaoDatabase::FillTableDefInfo(DAOTableDef* pDAOTableDef,
	CDaoTableDefInfo& tabledefinfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAOTableDef != NULL);
	ASSERT(dwOptions != 0);

	COleVariant var;
	short nBool;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAOTableDef->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		tabledefinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOTableDef->get_Updatable(&nBool));
		tabledefinfo.m_bUpdatable = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOTableDef->get_Attributes(
			&tabledefinfo.m_lAttributes));
	}

	if (dwOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAOTableDef->get_DateCreated(&var));
		tabledefinfo.m_dateCreated = var;

		DAO_CHECK(pDAOTableDef->get_LastUpdated(&var));
		tabledefinfo.m_dateLastUpdated = var;

		DAO_CHECK(pDAOTableDef->get_SourceTableName(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		tabledefinfo.m_strSrcTableName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOTableDef->get_Connect(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		tabledefinfo.m_strConnect = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOTableDef->get_ValidationRule(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		tabledefinfo.m_strValidationRule = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOTableDef->get_ValidationText(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		tabledefinfo.m_strValidationText = V_BSTRT(&var);
		var.Clear();
	}

	// This may be expensive, so only get it if absolutely necessary
	if (dwOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		DAO_CHECK(pDAOTableDef->get_RecordCount(
			&tabledefinfo.m_lRecordCount));
	}
}

void CDaoDatabase::InitRelationsCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAODatabase->get_Relations(&m_pDAORelations));
}

void CDaoDatabase::FillRelationInfo(DAORelation* pDAORelation,
	CDaoRelationInfo& relinfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAORelation != NULL);
	ASSERT(dwOptions != 0);

	COleVariant var;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		// All the relation info is primary
		DAO_CHECK(pDAORelation->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		relinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAORelation->get_Table(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		relinfo.m_strTable = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAORelation->get_ForeignTable(
			&V_BSTR(&var)));
		var.vt = VT_BSTR;
		relinfo.m_strForeignTable = V_BSTRT(&var);
		var.Clear();
	}

	if (dwOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAOFields* pDAOFields = NULL;
		DAOField* pDAOField = NULL;

		DAO_CHECK(pDAORelation->get_Attributes(
			&relinfo.m_lAttributes));

		// Get the fields collection
		DAO_CHECK(pDAORelation->get_Fields(&pDAOFields));

		TRY
		{
			// Get the number of fields in the relation
			short nCount;
			DAO_CHECK(pDAOFields->get_Count(&nCount));

			// Allocate or reallocate memory for array if necessary
			if (nCount != relinfo.m_nFields)
			{
				if (relinfo.m_nFields != 0)
				{
					// Check that allocation is correct.
					ASSERT(relinfo.m_nFields == 0 ||
						relinfo.m_bCleanupFieldInfo);

					delete[] relinfo.m_pFieldInfos;
					relinfo.m_pFieldInfos = NULL;
				}

				// Now allocate required memory
				relinfo.m_pFieldInfos = new CDaoRelationFieldInfo[nCount];
				relinfo.m_bCleanupFieldInfo = TRUE;
				relinfo.m_nFields = nCount;
			}

			// Now get field info for each field
			for (int nIndex = 0; nIndex < relinfo.m_nFields; nIndex++)
			{
				// Get the field item
				DAO_CHECK(pDAOFields->get_Item(
					COleVariant((long)nIndex), &pDAOField));

				// Get the field name
				DAO_CHECK(pDAOField->get_Name(&V_BSTR(&var)));
				var.vt = VT_BSTR;
				relinfo.m_pFieldInfos[nIndex].m_strName =
					V_BSTRT(&var);
				var.Clear();

				// Get the foreign field name
				DAO_CHECK(pDAOField->get_ForeignName(&V_BSTR(&var)));
				var.vt = VT_BSTR;
				relinfo.m_pFieldInfos[nIndex].m_strForeignName =
					V_BSTRT(&var);
				var.Clear();

				// Release and reset the field object
				pDAOField->Release();
				pDAOField = NULL;
			}
		}
		CATCH_ALL(e)
		{
			if (pDAOField != NULL)
				pDAOField->Release();

			pDAOFields->Release();
			THROW_LAST();
		}
		END_CATCH_ALL

		// Release the objects
		pDAOFields->Release();
	}
}

void CDaoDatabase::InitQueryDefsCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAODatabase->get_QueryDefs(&m_pDAOQueryDefs));
}

void CDaoDatabase::FillQueryDefInfo(DAOQueryDef* pDAOQueryDef,
	CDaoQueryDefInfo& querydefinfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAOQueryDef != NULL);
	ASSERT(dwOptions != 0);

	COleVariant var;
	short nBool;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAOQueryDef->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		querydefinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOQueryDef->get_Type(&querydefinfo.m_nType));
	}

	if (dwOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAOQueryDef->get_Updatable(&nBool));
		querydefinfo.m_bUpdatable = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOQueryDef->get_DateCreated(&var));
		querydefinfo.m_dateCreated = var;

		DAO_CHECK(pDAOQueryDef->get_LastUpdated(&var));
		querydefinfo.m_dateLastUpdated = var;

		DAO_CHECK(pDAOQueryDef->get_ReturnsRecords(&nBool));
		querydefinfo.m_bReturnsRecords = nBool == AFX_DAO_TRUE;
	}

	if (dwOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		DAO_CHECK(pDAOQueryDef->get_SQL(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		querydefinfo.m_strSQL = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOQueryDef->get_Connect(&V_BSTR(
			(LPVARIANT)var)));
		var.vt = VT_BSTR;
		querydefinfo.m_strConnect = V_BSTRT(&var);
		var.Clear();

		// DAO will display ODBC connect dialog
		// if data source no longer exists. This must
		// be avoided during bulk info retrieval.
		// Call CDaoQueryDef::GetODBCTimeout directly.
		//  DAO_CHECK(pDAOQueryDef->get_ODBCTimeout(
		//      &querydefinfo.m_nODBCTimeout));

		// Set the m_nODBCTimeout variable to invalid value.
		querydefinfo.m_nODBCTimeout = -1;
	}
}

void CDaoDatabase::ThrowDaoException(int nAfxDaoError)
{
	ASSERT_VALID(this);

	AfxThrowDaoException(nAfxDaoError);
}

#ifdef _DEBUG
void CDaoDatabase::AssertValid() const
{
	CObject::AssertValid();
}

void CDaoDatabase::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CObject::Dump(dc);

	dc << "m_bOpen = " << m_bOpen;
	dc << "\nm_nStatus = " << m_nStatus;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////
// CDaoTableDef
IMPLEMENT_DYNAMIC(CDaoTableDef, CObject)

CDaoTableDef::CDaoTableDef(CDaoDatabase* pDatabase)
{
	m_bOpen = FALSE;
	m_bNew = FALSE;

	m_pDatabase = pDatabase;
	m_pDAOTableDef = NULL;
	m_pDAOFields = NULL;
	m_pDAOIndexes = NULL;
}

CDaoTableDef::~CDaoTableDef()
{
	if (IsOpen())
		Close();
	else if (m_bNew)
	{
		// Remove the tabledef from the CDaoDatabase's map
		m_pDatabase->m_mapTableDefs.RemoveKey(this);
	}
}

void CDaoTableDef::Create(LPCTSTR lpszName, long lAttributes,
	LPCTSTR lpszSrcTable, LPCTSTR lpszConnect)
{
	ASSERT_VALID(this);
	ASSERT(!IsOpen());

	DAO_CHECK(m_pDatabase->m_pDAODatabase->CreateTableDef(
		COleVariant(lpszName, VT_BSTRT), COleVariant(lAttributes),
		COleVariant(lpszSrcTable, VT_BSTRT),
		COleVariant(lpszConnect, VT_BSTRT), &m_pDAOTableDef));

	m_bNew = TRUE;

	// Add the tabledef to map of Open/New CDaoTableDefs
	m_pDatabase->m_mapTableDefs.SetAt(this, this);
}

void CDaoTableDef::Append()
{
	ASSERT_VALID(this);
	ASSERT(m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	DAOTableDefs* pDAOTableDefs;
	DAO_CHECK(m_pDatabase->m_pDAODatabase->get_TableDefs(
		&pDAOTableDefs));

	TRY
	{
		DAO_CHECK(pDAOTableDefs->Append(m_pDAOTableDef));
	}
	CATCH_ALL(e)
	{
		pDAOTableDefs->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOTableDefs->Release();

	m_bNew = FALSE;
	m_bOpen = TRUE;
}

void CDaoTableDef::Open(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(lpszName != NULL);

	// Re-open is invalid
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	DAOTableDefs* pDAOTableDefs = NULL;

	TRY
	{
		DAO_CHECK(m_pDatabase->m_pDAODatabase->get_TableDefs(
			&pDAOTableDefs));
		DAO_CHECK(pDAOTableDefs->get_Item(
			COleVariant(lpszName, VT_BSTRT),&m_pDAOTableDef));
	}
	CATCH_ALL(e)
	{
		if (pDAOTableDefs != NULL)
			pDAOTableDefs->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOTableDefs->Release();

	m_bOpen = TRUE;

	// Add the tabledef to map of Open/New CDaoTableDefs
	m_pDatabase->m_mapTableDefs.SetAt(this, this);
}

void CDaoTableDef::Close()
{
	ASSERT_VALID(this);

	if (m_pDAOIndexes != NULL)
	{
		m_pDAOIndexes->Release();
		m_pDAOIndexes = NULL;
	}

	if (m_pDAOFields != NULL)
	{
		m_pDAOFields->Release();
		m_pDAOFields = NULL;
	}

	if (m_pDAOTableDef != NULL)
	{
		m_pDAOTableDef->Release();
		m_pDAOTableDef = NULL;
	}

	m_bOpen = FALSE;
	m_bNew = FALSE;

	// Remove the tabledef from the CDaoDatabase's map
	m_pDatabase->m_mapTableDefs.RemoveKey(this);
}

BOOL CDaoTableDef::CanUpdate()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	short nUpdatable;
	DAO_CHECK(m_pDAOTableDef->get_Updatable(&nUpdatable));
	return nUpdatable == AFX_DAO_TRUE;
}

void CDaoTableDef::SetName(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDef->put_Name(V_BSTR(&var)));
}

CString CDaoTableDef::GetName()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOTableDef->get_Name(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoTableDef::SetSourceTableName(LPCTSTR lpszSrcTableName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var(lpszSrcTableName, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDef->put_SourceTableName(
		V_BSTR(&var)));
}

CString CDaoTableDef::GetSourceTableName()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOTableDef->get_SourceTableName(
		&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoTableDef::SetConnect(LPCTSTR lpszConnect)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var(lpszConnect, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDef->put_Connect(V_BSTR(&var)));
}

CString CDaoTableDef::GetConnect()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOTableDef->get_Connect(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoTableDef::SetAttributes(long lAttributes)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	DAO_CHECK(m_pDAOTableDef->put_Attributes(lAttributes));
}

long CDaoTableDef::GetAttributes()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	long lAttributes;
	DAO_CHECK(m_pDAOTableDef->get_Attributes(&lAttributes));
	return lAttributes;
}

COleDateTime CDaoTableDef::GetDateCreated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant varDate;
	DAO_CHECK(m_pDAOTableDef->get_DateCreated(&varDate));
	return varDate.date;
}

COleDateTime CDaoTableDef::GetDateLastUpdated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant varDate;
	DAO_CHECK(m_pDAOTableDef->get_LastUpdated(&varDate));
	return varDate.date;
}

void CDaoTableDef::SetValidationRule(
	LPCTSTR lpszValidationRule)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var(lpszValidationRule, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDef->put_ValidationRule(
		V_BSTR(&var)));
}

CString CDaoTableDef::GetValidationRule()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOTableDef->get_ValidationRule(
		&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoTableDef::SetValidationText(
	LPCTSTR lpszValidationText)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var(lpszValidationText, VT_BSTRT);
	DAO_CHECK(m_pDAOTableDef->put_ValidationText(
		V_BSTR(&var)));
}

CString CDaoTableDef::GetValidationText()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAOTableDef->get_ValidationText(
		&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

long CDaoTableDef::GetRecordCount()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	long lRecordCount;
	DAO_CHECK(m_pDAOTableDef->get_RecordCount(&lRecordCount));
	return lRecordCount;
}

void CDaoTableDef::CreateField(LPCTSTR lpszName, short nType, long lSize,
	long lAttributes)
{
	ASSERT_VALID(this);

	CDaoFieldInfo fieldinfo;

	// Initialize everything so only correct properties will be set
	fieldinfo.m_strName = lpszName;
	fieldinfo.m_nType = nType;
	fieldinfo.m_lSize = lSize;
	fieldinfo.m_lAttributes = lAttributes;
	fieldinfo.m_nOrdinalPosition = 0;
	fieldinfo.m_bRequired = FALSE;
	fieldinfo.m_bAllowZeroLength = FALSE;
	fieldinfo.m_lCollatingOrder = 0;

	CreateField(fieldinfo);
}

void CDaoTableDef::CreateField(CDaoFieldInfo& fieldinfo)
{
	ASSERT_VALID(this);

	DAOField* pDAOField;

	// Create the DAO field object (setting basic properties)
	DAO_CHECK(m_pDAOTableDef->CreateField(
		COleVariant(fieldinfo.m_strName, VT_BSTRT),
		COleVariant(fieldinfo.m_nType),
		COleVariant(fieldinfo.m_lSize), &pDAOField));

	TRY
	{
		// Basic properties already set, so set the rest
		AfxSetFieldInfo(pDAOField, fieldinfo);

		// Append the field object to the fields collection
		if (m_pDAOFields == NULL)
			InitFieldsCollection();
		DAO_CHECK(m_pDAOFields->Append(pDAOField));
	}
	CATCH_ALL(e)
	{
		pDAOField->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOField->Release();
}

void CDaoTableDef::DeleteField(LPCTSTR lpszName)
{
	ASSERT_VALID(this);

	ASSERT(lpszName != NULL);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOFields->Delete(V_BSTR(&var)));
}

void CDaoTableDef::DeleteField(int nIndex)
{
	ASSERT_VALID(this);

	CDaoFieldInfo fieldinfo;
	GetFieldInfo(nIndex, fieldinfo, AFX_DAO_PRIMARY_INFO);
	DeleteField((LPCTSTR)fieldinfo.m_strName);
}

void CDaoTableDef::CreateIndex(CDaoIndexInfo& indexinfo)
{
	ASSERT_VALID(this);

	DAOIndex* pDAOIndex;

	DAO_CHECK(m_pDAOTableDef->CreateIndex(
		COleVariant(indexinfo.m_strName, VT_BSTRT), &pDAOIndex));

	TRY
	{
		// Set the index info
		AfxSetIndexInfo(pDAOIndex, indexinfo);

		// Append the field object to the fields collection
		if (m_pDAOIndexes == NULL)
			InitIndexesCollection();
		DAO_CHECK(m_pDAOIndexes->Append(pDAOIndex));
	}
	CATCH_ALL(e)
	{
		pDAOIndex->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOIndex->Release();
}

void CDaoTableDef::DeleteIndex(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(lpszName != NULL);

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOIndexes->Delete(V_BSTR(&var)));
}

void CDaoTableDef::DeleteIndex(int nIndex)
{
	ASSERT_VALID(this);

	CDaoIndexInfo indexinfo;
	GetIndexInfo(nIndex, indexinfo, AFX_DAO_PRIMARY_INFO);
	DeleteIndex((LPCTSTR)indexinfo.m_strName);
}

short CDaoTableDef::GetFieldCount()
{
	ASSERT_VALID(this);

	short nFields;

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	DAO_CHECK(m_pDAOFields->get_Count(&nFields));
	return nFields;
}

void CDaoTableDef::GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant((long)nIndex), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

void CDaoTableDef::GetFieldInfo(LPCTSTR lpszName,
	CDaoFieldInfo& fieldinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

short CDaoTableDef::GetIndexCount()
{
	ASSERT_VALID(this);

	short nIndexes;

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	DAO_CHECK(m_pDAOIndexes->get_Count(&nIndexes));
	return nIndexes;
}

void CDaoTableDef::GetIndexInfo(int nIndex, CDaoIndexInfo& indexinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	// Get DAOField object and fill in field info struct
	DAOIndex* pDAOIndex;
	DAO_CHECK(m_pDAOIndexes->get_Item(
		COleVariant((long)nIndex), &pDAOIndex));
	AfxGetIndexInfo(pDAOIndex, indexinfo, dwInfoOptions);

	// Clean up
	pDAOIndex->Release();
}

void CDaoTableDef::GetIndexInfo(LPCTSTR lpszName,
	CDaoIndexInfo& indexinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	// Get DAOField object and fill in field info struct
	DAOIndex* pDAOIndex;
	DAO_CHECK(m_pDAOIndexes->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOIndex));
	AfxGetIndexInfo(pDAOIndex, indexinfo, dwInfoOptions);

	// Clean up
	pDAOIndex->Release();
}

void CDaoTableDef::RefreshLink()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOTableDef != NULL);

	DAO_CHECK(m_pDAOTableDef->RefreshLink());
}

//Implementation functions
void CDaoTableDef::InitFieldsCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAOTableDef->get_Fields(&m_pDAOFields));
}

void CDaoTableDef::InitIndexesCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAOTableDef->get_Indexes(&m_pDAOIndexes));
}

void CDaoTableDef::ThrowDaoException(int nAfxDaoError)
{
	ASSERT_VALID(this);

	AfxThrowDaoException(nAfxDaoError);
}

#ifdef _DEBUG
void CDaoTableDef::AssertValid() const
{
	CObject::AssertValid();
}

void CDaoTableDef::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CObject::Dump(dc);

	dc << "m_bOpen = " << m_bOpen;
	dc << "\nm_bNew = " << m_bNew;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////
// CDaoQueryDef
IMPLEMENT_DYNAMIC(CDaoQueryDef, CObject)

CDaoQueryDef::CDaoQueryDef(CDaoDatabase* pDatabase)
{
	m_bOpen = FALSE;
	m_bNew = FALSE;

	m_pDatabase = pDatabase;
	m_pDAOQueryDef = NULL;
	m_pDAOParameters = NULL;
	m_pDAOFields = NULL;
}

CDaoQueryDef::~CDaoQueryDef()
{
	if (IsOpen())
		Close();
	else if (m_bNew)
	{
		// Remove the querydef from the CDaoDatabase's map
		m_pDatabase->m_mapQueryDefs.RemoveKey(this);
	}
}

void CDaoQueryDef::Create(LPCTSTR lpszName, LPCTSTR lpszSQL)
{
	ASSERT_VALID(this);
	ASSERT(!IsOpen());

	// Create a temp querydef if lpszName is NULL or empty string
	if (lpszName == NULL || *lpszName == '\0')
	{
		DAO_CHECK(m_pDatabase->m_pDAODatabase->CreateQueryDef(
			COleVariant(_T(""), VT_BSTRT),
			COleVariant(lpszSQL, VT_BSTRT),
			&m_pDAOQueryDef));
		m_bOpen = TRUE;
	}
	else
	{
		// Create a template querydef
		// (preventing automatic append to QueryDefs collection)
		DAO_CHECK(m_pDatabase->m_pDAODatabase->CreateQueryDef(
			_afxOptionalVariant, _afxOptionalVariant, &m_pDAOQueryDef));
		m_bNew = TRUE;

		// Now set the name and SQL if necessary
		SetName(lpszName);
		if (lpszSQL != NULL)
			SetSQL(lpszSQL);
	}

	// Add the querydef to map of Open/New CDaoQueryDefs
	m_pDatabase->m_mapQueryDefs.SetAt(this, this);
}

void CDaoQueryDef::Append()
{
	ASSERT_VALID(this);
	ASSERT(m_bNew);
	ASSERT(m_pDAOQueryDef != NULL);

	DAOQueryDefs* pDAOQueryDefs;
	DAO_CHECK(m_pDatabase->m_pDAODatabase->get_QueryDefs(
		&pDAOQueryDefs));

	TRY
	{
		DAO_CHECK(pDAOQueryDefs->Append(m_pDAOQueryDef));
	}
	CATCH_ALL(e)
	{
		pDAOQueryDefs->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOQueryDefs->Release();

	m_bNew = FALSE;
	m_bOpen = TRUE;
}

// Open a pre-defined query or create a temp query
void CDaoQueryDef::Open(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(!m_bNew);

	// Re-open is not allowed
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	// Null lpszName implies create a temp query
	if (lpszName == NULL)
	{
		DAO_CHECK(m_pDatabase->m_pDAODatabase->CreateQueryDef(
			COleVariant(_T(""), VT_BSTRT), _afxOptionalVariant, &m_pDAOQueryDef));
	}
	else
	{
		COleVariant varName(lpszName, VT_BSTRT);
		DAO_CHECK(m_pDatabase->m_pDAODatabase->OpenQueryDef(
			V_BSTR(&varName), &m_pDAOQueryDef));
	}

	m_bOpen = TRUE;

	// Add the querydef to map of Open/New CDaoQueryDefs
	m_pDatabase->m_mapQueryDefs.SetAt(this, this);
}

void CDaoQueryDef::Close()
{
	ASSERT_VALID(this);

	if (m_pDAOParameters != NULL)
	{
		m_pDAOParameters->Release();
		m_pDAOParameters = NULL;
	}

	if (m_pDAOFields != NULL)
	{
		m_pDAOFields->Release();
		m_pDAOFields = NULL;
	}

	if (m_pDAOQueryDef != NULL)
	{
		// DAO Close is a no op, but call it anyway
		DAO_TRACE(m_pDAOQueryDef->Close());
		m_pDAOQueryDef->Release();
		m_pDAOQueryDef = NULL;
	}

	m_bOpen = FALSE;
	m_bNew = FALSE;

	// Remove the querydef from the CDaoDatabase's map
	m_pDatabase->m_mapQueryDefs.RemoveKey(this);
}

BOOL CDaoQueryDef::CanUpdate()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nUpdatable;
	DAO_CHECK(m_pDAOQueryDef->get_Updatable(&nUpdatable));
	return nUpdatable == AFX_DAO_TRUE;
}

CString CDaoQueryDef::GetName()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	COleVariant var;
	DAO_CHECK(m_pDAOQueryDef->get_Name(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoQueryDef::SetName(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAOQueryDef->put_Name(V_BSTR(&var)));
}

CString CDaoQueryDef::GetSQL()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	COleVariant var;
	DAO_CHECK(m_pDAOQueryDef->get_SQL(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoQueryDef::SetSQL(LPCTSTR lpszSQL)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);

	COleVariant var(lpszSQL, VT_BSTRT);
	DAO_CHECK(m_pDAOQueryDef->put_SQL(V_BSTR(&var)));
}

short CDaoQueryDef::GetType()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nType;
	DAO_CHECK(m_pDAOQueryDef->get_Type(&nType));
	return nType;
}

COleDateTime CDaoQueryDef::GetDateCreated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	COleVariant varDate;
	DAO_CHECK(m_pDAOQueryDef->get_DateCreated(&varDate));
	return varDate.date;
}

COleDateTime CDaoQueryDef::GetDateLastUpdated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	COleVariant varDate;
	DAO_CHECK(m_pDAOQueryDef->get_LastUpdated(&varDate));
	return varDate.date;
}

CString CDaoQueryDef::GetConnect()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	COleVariant var;
	DAO_CHECK(m_pDAOQueryDef->get_Connect(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoQueryDef::SetConnect(LPCTSTR lpszConnect)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);

	COleVariant var(lpszConnect, VT_BSTRT);
	DAO_CHECK(m_pDAOQueryDef->put_Connect(V_BSTR(&var)));
}

short CDaoQueryDef::GetODBCTimeout()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nTimeout;
	DAO_CHECK(m_pDAOQueryDef->get_ODBCTimeout(&nTimeout));
	return nTimeout;
}

void CDaoQueryDef::SetODBCTimeout(short nODBCTimeout)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);

	DAO_CHECK(m_pDAOQueryDef->put_ODBCTimeout(nODBCTimeout));
}

BOOL CDaoQueryDef::GetReturnsRecords()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nReturnsRecords;
	DAO_CHECK(m_pDAOQueryDef->get_ReturnsRecords(&nReturnsRecords));
	return nReturnsRecords == AFX_DAO_TRUE;
}

void CDaoQueryDef::SetReturnsRecords(BOOL bReturnsRecords)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);

	DAO_CHECK(m_pDAOQueryDef->put_ReturnsRecords(
		(short)(bReturnsRecords ? AFX_DAO_TRUE : AFX_DAO_FALSE)));
}

long CDaoQueryDef::GetRecordsAffected()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	long lRecordsAffected;
	DAO_CHECK(m_pDAOQueryDef->get_RecordsAffected(&lRecordsAffected));
	return lRecordsAffected;
}

void CDaoQueryDef::Execute(int nOptions)
{
	ASSERT_VALID(this);
	ASSERT(m_pDAOQueryDef != NULL);

	DAO_CHECK(m_pDAOQueryDef->Execute(COleVariant((long)nOptions)));
}

COleVariant CDaoQueryDef::GetParamValue(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOQueryDef != NULL);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	DAOParameter* pDAOParameter = NULL;
	COleVariant var;

	TRY
	{
		DAO_CHECK(m_pDAOParameters->get_Item(
			COleVariant(lpszName, VT_BSTRT), &pDAOParameter));
		DAO_CHECK(pDAOParameter->get_Value(&var));
	}
	CATCH_ALL(e)
	{
		if (pDAOParameter != NULL)
			pDAOParameter->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOParameter->Release();
	return var;
}

COleVariant CDaoQueryDef::GetParamValue(int nIndex)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAOQueryDef != NULL);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	DAOParameter* pDAOParameter = NULL;
	COleVariant var;

	TRY
	{
		DAO_CHECK(m_pDAOParameters->get_Item(
			COleVariant((long)nIndex), &pDAOParameter));
		DAO_CHECK(pDAOParameter->get_Value(&var));
	}
	CATCH_ALL(e)
	{
		if (pDAOParameter != NULL)
			pDAOParameter->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOParameter->Release();
	return var;
}

void CDaoQueryDef::SetParamValue(LPCTSTR lpszName,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOQueryDef != NULL);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	DAOParameter* pDAOParameter = NULL;

	TRY
	{
		DAO_CHECK(m_pDAOParameters->get_Item(
			COleVariant(lpszName, VT_BSTRT), &pDAOParameter));
		DAO_CHECK(pDAOParameter->put_Value(varValue));
	}
	CATCH_ALL(e)
	{
		if (pDAOParameter != NULL)
			pDAOParameter->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOParameter->Release();
}

void CDaoQueryDef::SetParamValue(int nIndex,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen() || m_bNew);
	ASSERT(m_pDAOQueryDef != NULL);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	DAOParameter* pDAOParameter = NULL;

	TRY
	{
		DAO_CHECK(m_pDAOParameters->get_Item(
			COleVariant((long)nIndex), &pDAOParameter));
		DAO_CHECK(pDAOParameter->put_Value(varValue));
	}
	CATCH_ALL(e)
	{
		if (pDAOParameter != NULL)
			pDAOParameter->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pDAOParameter->Release();
}

void CDaoQueryDef::SetParamValueNull(LPCTSTR lpszName)
{
	ASSERT_VALID(this);

	SetParamValue(lpszName, _afxNullVariant);
}

void CDaoQueryDef::SetParamValueNull(int nIndex)
{
	ASSERT_VALID(this);

	SetParamValue(nIndex, _afxNullVariant);
}

short CDaoQueryDef::GetFieldCount()
{
	ASSERT_VALID(this);

	short nFields;

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	DAO_CHECK(m_pDAOFields->get_Count(&nFields));
	return nFields;
}

void CDaoQueryDef::GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant((long)nIndex), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

void CDaoQueryDef::GetFieldInfo(LPCTSTR lpszName,
	CDaoFieldInfo& fieldinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

short CDaoQueryDef::GetParameterCount()
{
	ASSERT_VALID(this);

	short nParameters;

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	DAO_CHECK(m_pDAOParameters->get_Count(&nParameters));
	return nParameters;
}

void CDaoQueryDef::GetParameterInfo(int nIndex,
	CDaoParameterInfo& paraminfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	// Get DAOParameter object and fill in parameter info struct
	DAOParameter* pDAOParameter;
	DAO_CHECK(m_pDAOParameters->get_Item(
		COleVariant((long)nIndex), &pDAOParameter));
	FillParameterInfo(pDAOParameter, paraminfo, dwInfoOptions);

	// Clean up
	pDAOParameter->Release();
}

void CDaoQueryDef::GetParameterInfo(LPCTSTR lpszName,
	CDaoParameterInfo& paraminfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOParameters == NULL)
		InitParametersCollection();

	// Get DAOParameter object and fill in parameter info struct
	DAOParameter* pDAOParameter;
	DAO_CHECK(m_pDAOParameters->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOParameter));
	FillParameterInfo(pDAOParameter, paraminfo, dwInfoOptions);

	// Clean up
	pDAOParameter->Release();
}

//Implementation functions
void CDaoQueryDef::InitFieldsCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAOQueryDef->get_Fields(&m_pDAOFields));
}

void CDaoQueryDef::InitParametersCollection()
{
	ASSERT_VALID(this);

	DAO_CHECK(m_pDAOQueryDef->get_Parameters(&m_pDAOParameters));
}

void CDaoQueryDef::FillParameterInfo(DAOParameter* pDAOParameter,
	CDaoParameterInfo& paraminfo, DWORD dwOptions)
{
	ASSERT_VALID(this);
	ASSERT(pDAOParameter != NULL);
	ASSERT(dwOptions != NULL);

	COleVariant var;

	if (dwOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		// All parameter info is basic info
		DAO_CHECK(pDAOParameter->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		paraminfo.m_strName = V_BSTRT(&var);
		var.Clear();

		TCHAR* pch = paraminfo.m_strName.GetBuffer(0);
		int nLength = lstrlen(pch);
		if (nLength > 0 && *pch == '[' && *(pch + nLength -1) == ']')
		{
			*(pch + nLength - 1) = 0;  // remove last bracket.
			lstrcpy(pch, pch + 1);
		}

		paraminfo.m_strName.ReleaseBuffer(-1);

		DAO_CHECK(pDAOParameter->get_Type(
			&paraminfo.m_nType));

		DAO_CHECK(pDAOParameter->get_Value(
			&paraminfo.m_varValue));
	}
}

void CDaoQueryDef::ThrowDaoException(int nAfxDaoError)
{
	ASSERT_VALID(this);

	AfxThrowDaoException(nAfxDaoError);
}

#ifdef _DEBUG
void CDaoQueryDef::AssertValid() const
{
	CObject::AssertValid();
}

void CDaoQueryDef::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CObject::Dump(dc);

	dc << "m_bOpen = " << m_bOpen;
	dc << "\nm_bNew = " << m_bNew;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////
// CDaoRecordset
IMPLEMENT_DYNAMIC(CDaoRecordset, CObject )

CDaoRecordset::CDaoRecordset(CDaoDatabase* pDatabase)
{
	m_bOpen = FALSE;

	m_pMapFieldCache = NULL;
	m_pMapFieldIndex = NULL;
	m_bCheckCacheForDirtyFields = TRUE;

	m_prgDaoColBindInfo = NULL;
	m_pulColumnLengths = NULL;
	m_pbFieldFlags = NULL;
	m_pbParamFlags = NULL;

	m_pDAORecordset = NULL;
	m_pICDAORecordsetGetRows = NULL;
	m_pQueryDef = NULL;
	m_pTableDef = NULL;
	m_pDAOFields = NULL;
	m_pDAOIndexes = NULL;

	m_pDatabase = pDatabase;

	m_nDefaultType = dbOpenDynaset;
	m_nStatus = 0;
	m_nFields = 0;
	m_nParams = 0;
}

CDaoRecordset::~CDaoRecordset()
{
	if (IsOpen())
		Close();

	// Clean up database if necessary
	if (m_pDatabase != NULL && (m_nStatus & AFX_DAO_IMPLICIT_DB))
	{
		m_pDatabase->Close();
		delete m_pDatabase;
		m_pDatabase = NULL;
	}
}

void CDaoRecordset::Open(int nOpenType, LPCTSTR lpszSQL, int nOptions)
{
	ASSERT_VALID(this);
	ASSERT(nOpenType == AFX_DAO_USE_DEFAULT_TYPE ||
		nOpenType == dbOpenDynaset || nOpenType == dbOpenSnapshot ||
		nOpenType == dbOpenTable);

	// Re-Opening is invalid.
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	if (nOpenType == AFX_DAO_USE_DEFAULT_TYPE)
		m_nOpenType = m_nDefaultType;
	else
		m_nOpenType = nOpenType;

	// Snapshots readOnly in DAO model.
	if (m_nOpenType == dbOpenSnapshot)
		nOptions |= dbReadOnly;

	// Cache parameters and initialize
	m_nOptions = nOptions;
	m_cbFixedLengthFields = 0;

	// Cache information for use in Requery
	m_strRequerySQL = lpszSQL;
	m_strRequeryFilter = m_strFilter;
	m_strRequerySort = m_strSort;

	AllocDatabase();

	m_strSQL = lpszSQL;
	if (m_strSQL.IsEmpty())
		m_strSQL = GetDefaultSQL();

	// Open table directly if option specified
	if (m_nOpenType == dbOpenTable)
	{
		m_pTableDef = new CDaoTableDef(m_pDatabase);
		m_nStatus |= AFX_DAO_IMPLICIT_TD;

		TRY
		{
			// Must remove the bracket from the name
			StripBrackets(m_strSQL, m_strSQL.GetBuffer(0));
			m_strSQL.ReleaseBuffer();

			m_pTableDef->Open(m_strSQL);

			// Open the DAO recordset (implicit MoveFirst)
			DAO_CHECK(m_pTableDef->m_pDAOTableDef->OpenRecordset(
				COleVariant((long)m_nOpenType), COleVariant((long)m_nOptions),
				&m_pDAORecordset));
		}
		CATCH_ALL(e)
		{
			// Once recordset marked as open, Close handles this
			if (m_pTableDef->IsOpen())
				m_pTableDef->Close();
			delete m_pTableDef;
			THROW_LAST();
		}
		END_CATCH_ALL
	}
	else
	{
		m_pQueryDef = new CDaoQueryDef(m_pDatabase);
		m_nStatus |= AFX_DAO_IMPLICIT_QD;

		TRY
		{
			// If initial clause includes potential start of row returning
			// query, then SQL passed to Open must be valid
			// (Note: TABLE is valid for UNION-type queries)
			if ((_tcsnicmp(m_strSQL, _afxSelect2, _countof(_afxSelect2)-1) != 0) &&
				(_tcsnicmp(m_strSQL, _afxParameters2, _countof(_afxParameters2)-1) != 0) &&
				(_tcsnicmp(m_strSQL, _afxTransform2, _countof(_afxTransform2)-1) != 0) &&
				(_tcsnicmp(m_strSQL, _afxTable2, _countof(_afxTable2)-1) != 0))
			{
				BuildSQL();
			}
			else
			{
				// Add the filter and sort
				if (!m_strFilter.IsEmpty())
					m_strSQL += _afxWhere2 + m_strFilter;

				if (!m_strSort.IsEmpty())
					m_strSQL += _afxOrderBy2 + m_strSort;
			}

			// Create and define temp query
			m_pQueryDef->Open();
			m_pQueryDef->SetSQL(m_strSQL);

			BindParameters();
			// Open the DAO recordset (implicit MoveFirst)
			DAO_CHECK(m_pQueryDef->m_pDAOQueryDef->_30_OpenRecordset(
				COleVariant((long)m_nOpenType), COleVariant((long)m_nOptions),
				&m_pDAORecordset));
		}
		CATCH_ALL(e)
		{
			// Once recordset marked as open, Close handles this
			if (m_pQueryDef->IsOpen())
				m_pQueryDef->Close();
			delete m_pQueryDef;
			THROW_LAST();
		}
		END_CATCH_ALL
	}

	m_bOpen = TRUE;

	// Add the recordset to map of Open CDaoRecordsets
	m_pDatabase->m_mapRecordsets.SetAt(this, this);

	TRY
	{
		BindFields();
		GetDataAndFixupNulls();
		SetCursorAttributes();
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL
}

void CDaoRecordset::Open(CDaoQueryDef* pQueryDef, int nOpenType,
	int nOptions)
{
	ASSERT_VALID(this);
	// Must pass valid, open QueryDef
	ASSERT(pQueryDef != NULL);
	if (!pQueryDef->IsOpen())
		ThrowDaoException(AFX_DAO_ERROR_OBJECT_NOT_OPEN);

	// Re-Opening is invalid.
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	if (nOpenType == AFX_DAO_USE_DEFAULT_TYPE)
		m_nOpenType = m_nDefaultType;
	else
		m_nOpenType = nOpenType;

	// Can't open table type recordsets with QueryDef
	ASSERT(m_nOpenType == dbOpenDynaset || m_nOpenType == dbOpenSnapshot);

	// Snapshots readOnly in DAO model.
	if (m_nOpenType == dbOpenSnapshot)
		nOptions |= dbReadOnly;

	// Cache parameters and initialize
	m_nOptions = nOptions;
	m_cbFixedLengthFields = 0;

	// Use pre-defined query
	m_pQueryDef = pQueryDef;

	// Share the same database object
	m_pDatabase = m_pQueryDef->m_pDatabase;

	BindParameters();

	// Open the DAO recordset (implicit MoveFirst)
	DAO_CHECK(m_pQueryDef->m_pDAOQueryDef->_30_OpenRecordset(
		COleVariant((long)m_nOpenType), COleVariant((long)m_nOptions),
		&m_pDAORecordset));

	m_bOpen = TRUE;

	// Add the recordset to map of Open CDaoRecordsets
	m_pDatabase->m_mapRecordsets.SetAt(this, this);

	TRY
	{
		BindFields();
		GetDataAndFixupNulls();
		SetCursorAttributes();
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL
}

void CDaoRecordset::Open(CDaoTableDef* pTableDef, int nOpenType,
	int nOptions)
{
	ASSERT_VALID(this);
	// Must pass valid, open TableDef
	ASSERT(pTableDef != NULL);
	if (!pTableDef->IsOpen())
		ThrowDaoException(AFX_DAO_ERROR_OBJECT_NOT_OPEN);
	m_pTableDef = pTableDef;

	// Re-Opening is invalid.
	if (IsOpen())
	{
		ASSERT(FALSE);
		return;
	}

	if (nOpenType == AFX_DAO_USE_DEFAULT_TYPE)
		m_nOpenType = m_nDefaultType;
	else
		m_nOpenType = nOpenType;

	// Cache parameters and initialize
	m_nOptions = nOptions;
	m_cbFixedLengthFields = 0;

	// Share the same database object
	m_pDatabase = m_pTableDef->m_pDatabase;

	// Open the DAO recordset (implicit MoveFirst)
	DAO_CHECK(m_pTableDef->m_pDAOTableDef->OpenRecordset(
		COleVariant((long)m_nOpenType), COleVariant((long)m_nOptions),
		&m_pDAORecordset));

	m_bOpen = TRUE;

	// Add the recordset to map of Open CDaoRecordsets
	m_pDatabase->m_mapRecordsets.SetAt(this, this);

	TRY
	{
		BindFields();
		GetDataAndFixupNulls();
		SetCursorAttributes();
	}
	CATCH_ALL(e)
	{
		Close();
		THROW_LAST();
	}
	END_CATCH_ALL
}

void CDaoRecordset::Close()
{
	ASSERT_VALID(this);

	if (IsOpen())
		FreeCache();

	// Clean up name strings in ColBindInfo struct
	if (m_prgDaoColBindInfo != NULL)
	{
		for (int nIndex = 0; nIndex < m_nFields; nIndex++)
		{
#ifndef _UNICODE
			delete[] (LPTSTR)m_prgDaoColBindInfo[nIndex].columnID.lpstr;
			m_prgDaoColBindInfo[nIndex].columnID.lpstr = NULL;
#else
			delete[] (LPTSTR)m_prgDaoColBindInfo[nIndex].columnID.lpwstr;
			m_prgDaoColBindInfo[nIndex].columnID.lpwstr = NULL;
#endif
		}
	}

	delete[] m_prgDaoColBindInfo;
	m_prgDaoColBindInfo = NULL;

	delete[] m_pulColumnLengths;
	m_pulColumnLengths = NULL;

	delete[] m_pbFieldFlags;
	m_pbFieldFlags = NULL;

	if (m_pMapFieldIndex != NULL)
	{
		delete m_pMapFieldIndex;
		m_pMapFieldIndex = NULL;
	}

	if (m_pDAOIndexes != NULL)
	{
		m_pDAOIndexes->Release();
		m_pDAOIndexes = NULL;
	}

	if (m_pDAOFields != NULL)
	{
		m_pDAOFields->Release();
		m_pDAOFields = NULL;
	}

	if (m_pICDAORecordsetGetRows != NULL)
	{
		m_pICDAORecordsetGetRows->Release();
		m_pICDAORecordsetGetRows = NULL;
	}

	if (m_pDAORecordset != NULL)
	{
		DAO_TRACE(m_pDAORecordset->Close());
		m_pDAORecordset->Release();
		m_pDAORecordset = NULL;
	}

	// Cleanup TableDef if not user supplied
	if (m_pTableDef != NULL && m_nStatus & AFX_DAO_IMPLICIT_TD)
	{
		m_pTableDef->Close();
		delete m_pTableDef;
		m_pTableDef = NULL;
	}

	// Cleanup QueryDef if not user supplied
	if (m_pQueryDef != NULL && m_nStatus & AFX_DAO_IMPLICIT_QD)
	{
		m_pQueryDef->Close();
		delete m_pQueryDef;
		m_pQueryDef = NULL;
	}

	m_nStatus &= ~AFX_DAO_IMPLICIT_QD;
	m_nStatus &= ~AFX_DAO_IMPLICIT_TD;
	m_pQueryDef = NULL;
	m_pTableDef = NULL;

	m_bOpen = FALSE;

	// Remove the recordset from the CDaoDatabase's map
	m_pDatabase->m_mapRecordsets.RemoveKey(this);
}

void CDaoRecordset::Requery()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(CanRestart());

	// If filter or sort strings changed, must Close and Open
	// This is only effective if m_strFilter/m_strSort used
	if ((m_pQueryDef != NULL &&
		(m_nStatus & AFX_DAO_IMPLICIT_QD)) &&
		((m_strRequeryFilter != m_strFilter) ||
		(m_strRequerySort != m_strSort)))
	{
		Close();
		Open(m_nOpenType, m_strRequerySQL, m_nOptions);
	}
	else
	{
		// Rebind parameters in case values have changed
		BindParameters();

		if (m_pQueryDef != NULL)
		{
			COleVariant varDisp;
			varDisp.pdispVal = m_pQueryDef->m_pDAOQueryDef;
			varDisp.vt = VT_DISPATCH;

			TRY
			{
				DAO_CHECK(m_pDAORecordset->Requery(varDisp));
			}
			CATCH_ALL(e)
			{
				// Reset vt to prevent release of DAOQueryDef
				varDisp.vt = VT_EMPTY;
				THROW_LAST();
			}
			END_CATCH_ALL

			// Reset vt to prevent release of DAOQueryDef
			varDisp.vt = VT_EMPTY;
		}
		else
			// Must be a table type recordset (this will fail!)
			DAO_CHECK(m_pDAORecordset->Requery(_afxOptionalVariant));

		GetDataAndFixupNulls();
	}
}

CString CDaoRecordset::GetDefaultDBName()
{
	ASSERT_VALID(this);

	// Override and add UNC path to .MDB file
	return _T("");
}

CString CDaoRecordset::GetDefaultSQL()
{
	ASSERT_VALID(this);

	// Override and add table name or entire SQL SELECT statement
	return _T("");
}

void CDaoRecordset::DoFieldExchange(CDaoFieldExchange* /* pFX */)
{
	ASSERT_VALID(this);

	// Do nothing if dynamic binding, otherwise override and add DFX calls
}

BOOL CDaoRecordset::IsBOF() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nBOF;
	DAO_CHECK(m_pDAORecordset->get_BOF(&nBOF));
	return nBOF == AFX_DAO_TRUE;
}

BOOL CDaoRecordset::IsEOF() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nEOF;
	DAO_CHECK(m_pDAORecordset->get_EOF(&nEOF));
	return nEOF == AFX_DAO_TRUE;
}

BOOL CDaoRecordset::IsDeleted() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	return m_bDeleted;
}

BOOL CDaoRecordset::CanScroll() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	return m_bScrollable;
}

BOOL CDaoRecordset::CanUpdate() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nUpdatable;
	DAO_CHECK(m_pDAORecordset->get_Updatable(&nUpdatable));
	return nUpdatable == AFX_DAO_TRUE;
}

BOOL CDaoRecordset::CanAppend() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	return m_bAppendable;
}

BOOL CDaoRecordset::CanRestart()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	short nRestartable;
	DAO_CHECK(m_pDAORecordset->get_Restartable(&nRestartable));
	return nRestartable == AFX_DAO_TRUE;
}

BOOL CDaoRecordset::CanTransact()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nTransactions;
	DAO_CHECK(m_pDAORecordset->get_Transactions(&nTransactions));
	return nTransactions == AFX_DAO_TRUE;
}

BOOL CDaoRecordset::CanBookmark()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nBookmarkable;
	DAO_CHECK(m_pDAORecordset->get_Bookmarkable(&nBookmarkable));
	return nBookmarkable == AFX_DAO_TRUE;
}

CString CDaoRecordset::GetName()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_Name(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

short CDaoRecordset::GetType()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	short nType;
	DAO_CHECK(m_pDAORecordset->get_Type(&nType));
	return nType;
}

short CDaoRecordset::GetEditMode()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	short nEditMode;
	DAO_CHECK(m_pDAORecordset->get_EditMode(&nEditMode));
	return nEditMode;
}

CString CDaoRecordset::GetSQL() const
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	return m_strSQL;
}

COleDateTime CDaoRecordset::GetDateCreated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant varDate;
	DAO_CHECK(m_pDAORecordset->get_DateCreated(&varDate));
	return varDate.date;
}

COleDateTime CDaoRecordset::GetDateLastUpdated()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant varDate;
	DAO_CHECK(m_pDAORecordset->get_LastUpdated(&varDate));
	return varDate.date;
}

COleVariant CDaoRecordset::GetLastModifiedBookmark()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_LastModified(&var.parray));
	var.vt = VT_ARRAY | VT_UI1;

	return var;
}

CString CDaoRecordset::GetValidationRule()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_ValidationRule(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString CDaoRecordset::GetValidationText()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_ValidationText(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

CString CDaoRecordset::GetCurrentIndex()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_Index(&V_BSTR(&var)));
	var.vt = VT_BSTR;
	return V_BSTRT(&var);
}

void CDaoRecordset::SetCurrentIndex(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var(lpszName, VT_BSTRT);
	DAO_CHECK(m_pDAORecordset->put_Index(V_BSTR(&var)));

	// Refetch the data
	GetDataAndFixupNulls();
}

long CDaoRecordset::GetRecordCount()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	long lRecordCount;
	DAO_CHECK(m_pDAORecordset->get_RecordCount(&lRecordCount));
	return lRecordCount;
}

void CDaoRecordset::SetFieldDirty(void* pv, BOOL bDirty)
{
	ASSERT_VALID(this);

	if (m_nFields <= 0)
	{
		// Can't set fields dirty if no bound fields
		ASSERT(FALSE);
		return;
	}

	int nIndex = 0, nIndexEnd;

	if (pv == NULL)
		nIndexEnd = m_nFields - 1;
	else
	{
		nIndex = nIndexEnd = GetFieldIndex(pv);
		ASSERT(nIndex != AFX_DAO_DATA_NOT_FOUND);
	}

	while (nIndex <= nIndexEnd)
	{
		if (bDirty)
			SetDirtyFieldStatus(nIndex);
		else
			ClearDirtyFieldStatus(nIndex);
		nIndex++;
	}
}

BOOL CDaoRecordset::IsFieldDirty(void* pv)
{
	ASSERT_VALID(this);

	short nEditMode = GetEditMode();

	// Fields can't be dirty if not in edit/addnew mode or no fields bound
	if (nEditMode == dbEditNone || m_nFields <= 0)
		return FALSE;

	// Check if cache field values have changed and mark as dirty
	if (m_bCheckCacheForDirtyFields)
	{
		if (nEditMode == dbEditInProgress)
			MarkForEdit();
		else
			MarkForAddNew();
	}

	int nIndex = 0, nIndexEnd;

	if (pv == NULL)
		nIndexEnd = m_nFields - 1;
	else
	{
		// Get the field index to use status array
		nIndex = nIndexEnd = GetFieldIndex(pv);
		ASSERT(nIndex != AFX_DAO_DATA_NOT_FOUND);
	}

	BOOL bDirty = FALSE;

	while (nIndex <= nIndexEnd && !bDirty)
		bDirty = IsFieldStatusDirty(nIndex++);

	return bDirty;
}

void CDaoRecordset::SetFieldNull(void* pv, BOOL bNull)
{
	ASSERT_VALID(this);

	if (m_nFields <= 0)
	{
		ASSERT(FALSE);
		return;
	}

	if (bNull)
	{
		// Need field exchange to set value to PSEUDO NULL.
		CDaoFieldExchange fx(CDaoFieldExchange::SetFieldNull, this, pv);
		fx.m_nFieldFound = 0;
		DoFieldExchange(&fx);

		// If no field found, index will still be zero
		ASSERT(fx.m_nFieldFound != 0);
	}
	else
	{
		// Set status array not NULL. Don't need field exchange mechanism.
		int nIndex = 0, nIndexEnd;

		if (pv == NULL)
			nIndexEnd = m_nFields - 1;
		else
		{
			nIndex = nIndexEnd = GetFieldIndex(pv);
			ASSERT(nIndex != AFX_DAO_DATA_NOT_FOUND);
		}

		while (nIndex <= nIndexEnd)
		{
			ClearNullFieldStatus(nIndex);
			nIndex++;
		}
	}
}

BOOL CDaoRecordset::IsFieldNull(void* pv)
{
	ASSERT_VALID(this);

	if (m_nFields <= 0)
	{
		ASSERT(FALSE);
		return FALSE;
	}

	int nIndex = 0, nIndexEnd;

	if (pv == NULL)
		nIndexEnd = m_nFields - 1;
	else
	{
		// Get the field index to use status array
		nIndex = nIndexEnd = GetFieldIndex(pv);
		ASSERT(nIndex != AFX_DAO_DATA_NOT_FOUND);
	}

	BOOL bNull = FALSE;

	while (nIndex <= nIndexEnd && !bNull)
		bNull = IsFieldStatusNull(nIndex++);

	return bNull;
}

BOOL CDaoRecordset::IsFieldNullable(void* pv)
{
	ASSERT_VALID(this);

	if (m_nFields <= 0)
	{
		ASSERT(FALSE);
		return FALSE;
	}

	int nIndex = 0, nIndexEnd;

	if (pv == NULL)
		nIndexEnd = m_nFields - 1;
	else
	{
		// Get the field index to use status array
		nIndex = nIndexEnd = GetFieldIndex(pv);
		ASSERT(nIndex != AFX_DAO_DATA_NOT_FOUND);
	}

	BOOL bNullable = FALSE;

	while (nIndex <= nIndexEnd && !bNullable)
	{
		if (!IsFieldStatusNullableKnown(nIndex))
		{
			CDaoFieldInfo fieldinfo;
			GetFieldInfo(nIndex, fieldinfo, AFX_DAO_SECONDARY_INFO);
			bNullable = !fieldinfo.m_bRequired;
			if (bNullable)
				SetNullableFieldStatus(nIndex);
			SetNullableKnownFieldStatus(nIndex);
		}
		else
			bNullable = IsFieldStatusNullable(nIndex);

		nIndex++;
	}

	return bNullable;
}

short CDaoRecordset::GetFieldCount()
{
	ASSERT_VALID(this);

	short nFields;

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	DAO_CHECK(m_pDAOFields->get_Count(&nFields));
	return nFields;
}

void CDaoRecordset::GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant((long)nIndex), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

void CDaoRecordset::GetFieldInfo(LPCTSTR lpszName,
	CDaoFieldInfo& fieldinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	// Get DAOField object and fill in field info struct
	DAOField* pDAOField;
	DAO_CHECK(m_pDAOFields->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOField));
	AfxGetFieldInfo(pDAOField, fieldinfo, dwInfoOptions);

	// Clean up
	pDAOField->Release();
}

short CDaoRecordset::GetIndexCount()
{
	ASSERT_VALID(this);

	short nIndexes;

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	DAO_CHECK(m_pDAOIndexes->get_Count(&nIndexes));
	return nIndexes;
}

void CDaoRecordset::GetIndexInfo(int nIndex, CDaoIndexInfo& indexinfo,
	DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	// Get DAOField object and fill in field info struct
	DAOIndex* pDAOIndex;
	DAO_CHECK(m_pDAOIndexes->get_Item(
		COleVariant((long)nIndex), &pDAOIndex));
	AfxGetIndexInfo(pDAOIndex, indexinfo, dwInfoOptions);

	// Clean up
	pDAOIndex->Release();
}

void CDaoRecordset::GetIndexInfo(LPCTSTR lpszName,
	CDaoIndexInfo& indexinfo, DWORD dwInfoOptions)
{
	ASSERT_VALID(this);

	if (m_pDAOIndexes == NULL)
		InitIndexesCollection();

	// Get DAOField object and fill in field info struct
	DAOIndex* pDAOIndex;
	DAO_CHECK(m_pDAOIndexes->get_Item(
		COleVariant(lpszName, VT_BSTRT), &pDAOIndex));
	AfxGetIndexInfo(pDAOIndex, indexinfo, dwInfoOptions);

	// Clean up
	pDAOIndex->Release();
}

COleVariant CDaoRecordset::GetBookmark()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_Bookmark(&var.parray));
	var.vt = VT_ARRAY | VT_UI1;

	return var;
}

void CDaoRecordset::SetBookmark(COleVariant varBookmark)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_Bookmark(&varBookmark.parray));

	GetDataAndFixupNulls();
}

long CDaoRecordset::GetAbsolutePosition()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	long lPosition;
	DAO_CHECK(m_pDAORecordset->get_AbsolutePosition(&lPosition));
	return lPosition;
}

void CDaoRecordset::SetAbsolutePosition(long lPosition)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_AbsolutePosition(lPosition));

	GetDataAndFixupNulls();
}

float CDaoRecordset::GetPercentPosition()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	float fPosition;
	DAO_CHECK(m_pDAORecordset->get_PercentPosition(&fPosition));
	return fPosition;
}

void CDaoRecordset::SetPercentPosition(float fPosition)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_PercentPosition(fPosition));

	GetDataAndFixupNulls();
}

void CDaoRecordset::MoveNext()
{
	ASSERT_VALID(this);

	Move(AFX_DAO_NEXT);
}

void CDaoRecordset::MovePrev()
{
	ASSERT_VALID(this);

	Move(AFX_DAO_PREV);
}

void CDaoRecordset::MoveFirst()
{
	ASSERT_VALID(this);

	Move(AFX_DAO_FIRST);
}

void CDaoRecordset::MoveLast()
{
	ASSERT_VALID(this);

	Move(AFX_DAO_LAST);
}

void CDaoRecordset::Move(long lRows)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	switch(lRows)
	{
	case AFX_DAO_NEXT:
		DAO_CHECK(m_pDAORecordset->MoveNext());
		break;

	case AFX_DAO_PREV:
		DAO_CHECK(m_pDAORecordset->MovePrevious());
		break;

	case AFX_DAO_FIRST:
		DAO_CHECK(m_pDAORecordset->MoveFirst());
		break;

	case AFX_DAO_LAST:
		DAO_CHECK(m_pDAORecordset->_30_MoveLast());
		break;

	// General case
	default:
		// Call Move without getting Bookmark (using unitialized variant).
		DAO_CHECK(m_pDAORecordset->Move(lRows, COleVariant()));

	}

	GetDataAndFixupNulls();
}

BOOL CDaoRecordset::FindNext(LPCTSTR lpszFilter)
{
	ASSERT_VALID(this);

	return Find(AFX_DAO_NEXT, lpszFilter);
}

BOOL CDaoRecordset::FindPrev(LPCTSTR lpszFilter)
{
	ASSERT_VALID(this);

	return Find(AFX_DAO_PREV, lpszFilter);
}

BOOL CDaoRecordset::FindFirst(LPCTSTR lpszFilter)
{
	ASSERT_VALID(this);

	return Find(AFX_DAO_FIRST, lpszFilter);
}

BOOL CDaoRecordset::FindLast(LPCTSTR lpszFilter)
{
	ASSERT_VALID(this);

	return Find(AFX_DAO_LAST, lpszFilter);
}

BOOL CDaoRecordset::Find(long lType, LPCTSTR lpszFilter)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var(lpszFilter, VT_BSTRT);

	switch(lType)
	{
	default:
		// Invalid Find type!
		ASSERT(FALSE);

		// fall through to FindNext case

	case AFX_DAO_NEXT:
		DAO_CHECK(m_pDAORecordset->FindNext(V_BSTR(&var)));
		break;

	case AFX_DAO_PREV:
		DAO_CHECK(m_pDAORecordset->FindPrevious(V_BSTR(&var)));
		break;

	case AFX_DAO_FIRST:
		DAO_CHECK(m_pDAORecordset->FindFirst(V_BSTR(&var)));
		break;

	case AFX_DAO_LAST:
		DAO_CHECK(m_pDAORecordset->FindLast(V_BSTR(&var)));
		break;
	}

	BOOL bMatch = IsMatch();
	if (bMatch)
		GetDataAndFixupNulls();

	return bMatch;
}

BOOL CDaoRecordset::Seek(LPCTSTR lpszComparison, COleVariant* pKey1,
	COleVariant* pKey2, COleVariant* pKey3)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);
	ASSERT(m_nOpenType == dbOpenTable);

	// Must have at least one key
	ASSERT(pKey1 != NULL);

	COleVariant varComparison(lpszComparison, VT_BSTRT);

	DAO_CHECK(m_pDAORecordset->Seek(V_BSTR(&varComparison),
		pKey1 != NULL ? (VARIANT)*pKey1 : _afxOptionalVariant,
		pKey2 != NULL ? (VARIANT)*pKey2 : _afxOptionalVariant,
		pKey3 != NULL ? (VARIANT)*pKey3 : _afxOptionalVariant,
		_afxOptionalVariant, _afxOptionalVariant, _afxOptionalVariant,
		_afxOptionalVariant, _afxOptionalVariant, _afxOptionalVariant,
		_afxOptionalVariant, _afxOptionalVariant, _afxOptionalVariant,
		_afxOptionalVariant));

	BOOL bMatch = IsMatch();
	if (bMatch)
		GetDataAndFixupNulls();

	return bMatch;
}

BOOL CDaoRecordset::Seek(LPCTSTR lpszComparison, COleVariant* pKeyArray,
	WORD nKeys)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);
	ASSERT(m_nOpenType == dbOpenTable);

	// Must have at least one key and no more than 13
	ASSERT(nKeys > 0);
	ASSERT(nKeys < 14);

	COleVariant varComparison(lpszComparison, VT_BSTRT);
	LPVARIANT pVarArray[13];

	for (WORD nIndex = 0; nIndex < nKeys; nIndex++)
		pVarArray[nIndex] = &pKeyArray[nIndex];

	for (;nIndex < 13; nIndex++)
		pVarArray[nIndex] = &_afxOptionalVariant;

	DAO_CHECK(m_pDAORecordset->Seek(V_BSTR(&varComparison),
		*pVarArray[0], *pVarArray[1], *pVarArray[2], *pVarArray[3],
		*pVarArray[4], *pVarArray[5], *pVarArray[6], *pVarArray[7],
		*pVarArray[8], *pVarArray[9], *pVarArray[10], *pVarArray[11],
		*pVarArray[12]));

	BOOL bMatch = IsMatch();
	if (bMatch)
		GetDataAndFixupNulls();

	return bMatch;
}

void CDaoRecordset::CancelUpdate()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->_30_CancelUpdate());

	// Restore cache if necessary
	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
		LoadFields();
}

void CDaoRecordset::SetLockingMode(BOOL bPessimistic)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_LockEdits(
		(short)(bPessimistic ? AFX_DAO_TRUE : AFX_DAO_FALSE)));
}

BOOL CDaoRecordset::GetLockingMode()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	short nLockEdits;
	DAO_CHECK(m_pDAORecordset->get_LockEdits(&nLockEdits));
	return nLockEdits == AFX_DAO_TRUE;
}

void CDaoRecordset::SetCacheStart(COleVariant varBookmark)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_CacheStart(&varBookmark.parray));
}

COleVariant CDaoRecordset::GetCacheStart()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	COleVariant var;
	DAO_CHECK(m_pDAORecordset->get_CacheStart(&var.parray));
	var.vt = VT_ARRAY | VT_UI1;

	return var;
}

void CDaoRecordset::SetCacheSize(long lSize)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->put_CacheSize(lSize));
}

long CDaoRecordset::GetCacheSize()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	long lSize;
	DAO_CHECK(m_pDAORecordset->get_CacheSize(&lSize));
	return lSize;
}

void CDaoRecordset::FillCache(long* pSize, COleVariant* pBookmark)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->FillCache(
		pSize != NULL ? (VARIANT)COleVariant(*pSize) : _afxOptionalVariant,
		pBookmark != NULL ? (VARIANT)*pBookmark : _afxOptionalVariant));
}

void CDaoRecordset::AddNew()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		short nEditMode = GetEditMode();
		if (nEditMode != dbEditAdd)
		{
			// Store fields if necessary (fields already stored if edit mode)
			if (nEditMode != dbEditInProgress)
			{
				AllocCache();
				StoreFields();
			}

			// Set all fields NULL and not dirty
			SetFieldNull(NULL);
			SetFieldDirty(NULL, FALSE);
		}
	}

	DAO_CHECK(m_pDAORecordset->AddNew());
}

void CDaoRecordset::Edit()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		short nEditMode = GetEditMode();
		if (nEditMode != dbEditInProgress)
		{
			if (nEditMode == dbEditNone)
			{
				// Save fields for restore/dirty checking later
				AllocCache();
				StoreFields();
				SetFieldDirty(NULL, FALSE);
			}
			else
				// Load in fields cached on AddNew call prior to Edit
				LoadFields();
		}
	}

	DAO_CHECK(m_pDAORecordset->Edit());
}

void CDaoRecordset::Update()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	int nMode = 0;

	// If caching, compare cache to current values and save state
	if (m_nFields > 0)
	{
		if (m_bCheckCacheForDirtyFields)
		{
			IsFieldDirty(NULL);
			nMode = GetEditMode();
		}

		// Set all field values of all fields marked dirty
		SetDirtyFields();
	}

	DAO_CHECK(m_pDAORecordset->_30_Update());

	// Restore data if data cached and were in add mode
	if (m_bCheckCacheForDirtyFields && m_nFields > 0 &&
		nMode == dbEditAdd)
	{
		LoadFields();
	}
}

void CDaoRecordset::Delete()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	DAO_CHECK(m_pDAORecordset->Delete());

	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		SetFieldNull(NULL);
		SetFieldDirty(NULL, FALSE);
	}

	m_bDeleted = TRUE;
}

COleVariant CDaoRecordset::GetFieldValue(LPCTSTR lpszName)
{
	COleVariant var;
	GetFieldValue(lpszName, var);
	return var;
}

COleVariant CDaoRecordset::GetFieldValue(int nIndex)
{
	COleVariant var;
	GetFieldValue(nIndex, var);
	return var;
}

void CDaoRecordset::GetFieldValue(LPCTSTR lpszName, COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	// Clear out variant
	varValue.Clear();

	// Use DAO optimization. get_Collect will get the field object from
	// the fields collection and fetch the value
	DAO_CHECK(m_pDAORecordset->get_Collect(
		COleVariant(lpszName, VT_BSTRT), &varValue));
}

void CDaoRecordset::GetFieldValue(int nIndex, COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	// Clear out variant
	varValue.Clear();

	// Use DAO optimization. get_Collect will get the field object from
	// the fields collection and fetch the value
	DAO_CHECK(m_pDAORecordset->get_Collect(
		COleVariant((long)nIndex), &varValue));
}

void CDaoRecordset::SetFieldValue(LPCTSTR lpszName,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	// Use DAO optimization. get_Collect will get the field object from
	// the fields collection and fetch the value
	DAO_CHECK(m_pDAORecordset->put_Collect(
		COleVariant(lpszName, VT_BSTRT), varValue));
}

void CDaoRecordset::SetFieldValue(int nIndex,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pDAORecordset != NULL);

	// Use DAO optimization. put_Collect will get the field object from
	// the fields collection and write the value
	DAO_CHECK(m_pDAORecordset->put_Collect(
		COleVariant((long)nIndex), varValue));
}

void CDaoRecordset::SetFieldValue(int nIndex,
	LPCTSTR lpszValue)
{
	COleVariant varValue(lpszValue, VT_BSTRT);
	SetFieldValue(nIndex, varValue);
}

void CDaoRecordset::SetFieldValue(LPCTSTR lpszName,
	LPCTSTR lpszValue)
{
	COleVariant varValue(lpszValue, VT_BSTRT);
	SetFieldValue(lpszName, varValue);
}

void CDaoRecordset::SetFieldValueNull(LPCTSTR lpszName)
{
	ASSERT_VALID(this);

	SetFieldValue(lpszName, _afxNullVariant);
}

void CDaoRecordset::SetFieldValueNull(int nIndex)
{
	ASSERT_VALID(this);

	SetFieldValue(nIndex, _afxNullVariant);
}

COleVariant CDaoRecordset::GetParamValue(LPCTSTR lpszName)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pQueryDef != NULL);

	return m_pQueryDef->GetParamValue(lpszName);
}

COleVariant CDaoRecordset::GetParamValue(int nIndex)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pQueryDef != NULL);

	return m_pQueryDef->GetParamValue(nIndex);
}

void CDaoRecordset::SetParamValue(LPCTSTR lpszName,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pQueryDef != NULL);

	m_pQueryDef->SetParamValue(lpszName, varValue);
}

void CDaoRecordset::SetParamValue(int nIndex,
	const COleVariant& varValue)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());
	ASSERT(m_pQueryDef != NULL);

	m_pQueryDef->SetParamValue(nIndex, varValue);
}

void CDaoRecordset::SetParamValueNull(LPCTSTR lpszName)
{
	ASSERT_VALID(this);

	SetParamValue(lpszName, _afxNullVariant);
}

void CDaoRecordset::SetParamValueNull(int nIndex)
{
	ASSERT_VALID(this);

	SetParamValue(nIndex, _afxNullVariant);
}

//Implementation functions
DWORD CDaoRecordset::GetFieldLength(int nFieldIndex)
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	return m_pulColumnLengths[nFieldIndex];
}

void CDaoRecordset::InitFieldsCollection()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	DAO_CHECK(m_pDAORecordset->get_Fields(&m_pDAOFields));
}

void CDaoRecordset::InitIndexesCollection()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	DAO_CHECK(m_pTableDef->m_pDAOTableDef->get_Indexes(&m_pDAOIndexes));
}

// "PARAMETERS <parameter list>;"
// "SELECT <select list> FROM <table list>"
// " WHERE <m_strFilter> ORDER BY <m_strSort>"
void CDaoRecordset::BuildSQL()
{
	ASSERT_VALID(this);

	// Assumes m_strSQL represents table list
	CString strTableName = m_strSQL;
	m_strSQL.Empty();

	if (m_nParams > 0)
		BuildParameterList();
	BuildSelectList();
	m_strSQL += _afxFrom2 + strTableName;

	if (!m_strFilter.IsEmpty())
	{
		m_strSQL += _afxWhere2;
		m_strSQL += m_strFilter;
	}

	if (!m_strSort.IsEmpty())
	{
		m_strSQL += _afxOrderBy2;
		m_strSQL += m_strSort;
	}
}

void CDaoRecordset::AllocDatabase()
{
	ASSERT_VALID(this);

	// Allocate and maintain database if necessary
	if (m_pDatabase == NULL)
	{
		m_pDatabase = new CDaoDatabase;
		m_pDatabase->m_nStatus |= AFX_DAO_IMPLICIT_DB;
		m_nStatus |= AFX_DAO_IMPLICIT_DB;
	}

	// Open Database if necessary
	if (!m_pDatabase->IsOpen())
		m_pDatabase->Open((LPCTSTR)GetDefaultDBName(), FALSE, FALSE, NULL);
}

void CDaoRecordset::BuildSelectList()
{
	ASSERT_VALID(this);
	ASSERT(m_nFields > 0);

	m_strSQL += _afxSelect2;

	CDaoFieldExchange fx(CDaoFieldExchange::AddToSelectList, this);
	DoFieldExchange(&fx);
}

void CDaoRecordset::BuildParameterList()
{
	ASSERT_VALID(this);
	ASSERT(m_nParams > 0);

	m_strSQL += _afxParameters2;

	CDaoFieldExchange fx(CDaoFieldExchange::AddToParameterList, this);
	DoFieldExchange(&fx);

	if (fx.m_nParam != 0)
		m_strSQL += _T(";");
	else
		m_strSQL.Empty();
}

void CDaoRecordset::BindFields()
{
	ASSERT_VALID(this);

	if (m_nFields > 0)
	{
		// Setup the DAO binding struct
		ASSERT(m_prgDaoColBindInfo == NULL);
		m_prgDaoColBindInfo = new DAOCOLUMNBINDING[m_nFields];
		memset(m_prgDaoColBindInfo, 0, sizeof(DAOCOLUMNBINDING) * m_nFields);
		m_pulColumnLengths = new DWORD[m_nFields];
		memset(m_pulColumnLengths, 0, sizeof(DWORD) * m_nFields);

		m_pbFieldFlags = new BYTE[m_nFields];
		memset(m_pbFieldFlags, 0, m_nFields);

		m_DaoFetchRows.cRowsRequested = 1;
		m_DaoFetchRows.dwFlags = DAOROWFETCH_BINDABSOLUTE;
		m_DaoFetchRows.pData = NULL;

		// Allocate the index map
		ASSERT(m_pMapFieldIndex == NULL);
		m_pMapFieldIndex = new CMapPtrToPtr;

		CDaoFieldExchange fx(CDaoFieldExchange::BindField, this);
		DoFieldExchange(&fx);
	}
}

void CDaoRecordset::BindParameters()
{
	ASSERT_VALID(this);

	if (m_nParams > 0)
	{
		// Since Jet treats non-bindable names as implicit parameters
		// this should catch some SQL syntax errors that would otherwise
		// appear to be uninitialized parameters.
		ASSERT(m_pQueryDef->GetParameterCount() == m_nParams);

		CDaoFieldExchange fx(CDaoFieldExchange::BindParam, this);
		DoFieldExchange(&fx);
	}
}

void CDaoRecordset::Fixup()
{
	ASSERT_VALID(this);

	CDaoFieldExchange fx(CDaoFieldExchange::Fixup, this);
	DoFieldExchange(&fx);
}

void CDaoRecordset::AllocCache()
{
	ASSERT_VALID(this);

	// Do nothing if caching disabled by master switch
	if (m_bCheckCacheForDirtyFields && m_nFields > 0 &&
		m_pMapFieldCache == NULL)
	{
		m_pMapFieldCache = new CMapPtrToPtr;

		CDaoFieldExchange fx(CDaoFieldExchange::AllocCache, this);
		DoFieldExchange(&fx);
	}
}

void CDaoRecordset::StoreFields()
{
	ASSERT_VALID(this);

	// Do nothing if caching disabled by master switch
	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		ASSERT(m_pMapFieldCache != NULL);
		CDaoFieldExchange fx(CDaoFieldExchange::StoreField, this);
		DoFieldExchange(&fx);
	}
}

void CDaoRecordset::LoadFields()
{
	ASSERT_VALID(this);

	// Do nothing if caching disabled by master switch
	if (m_bCheckCacheForDirtyFields && m_nFields > 0)
	{
		CDaoFieldExchange fx(CDaoFieldExchange::LoadField, this);
		DoFieldExchange(&fx);

		// Clear the dirty status flags
		SetFieldDirty(NULL, FALSE);
	}
}

void CDaoRecordset::FreeCache()
{
	ASSERT_VALID(this);

	// Do nothing if caching disabled by master switch
	if (m_bCheckCacheForDirtyFields && m_nFields > 0 &&
		m_pMapFieldCache != NULL)
	{
		// Free up dynamically allocated mem in cache
		CDaoFieldExchange fx(CDaoFieldExchange::FreeCache, this);

		// Delete any cached data
		void* pvKey;
		void* pvObject;
		POSITION pos = m_pMapFieldCache->GetStartPosition();
		while (pos != NULL)
		{
			m_pMapFieldCache->GetNextAssoc(pos, pvKey, pvObject);
			fx.DeleteCacheValue((CDaoFieldCache*)pvObject,
				((CDaoFieldCache*)pvObject)->m_nDataType);
		}
		m_pMapFieldCache->RemoveAll();

		delete m_pMapFieldCache;
		m_pMapFieldCache = NULL;
	}
}

void CDaoRecordset::MarkForAddNew()
{
	ASSERT_VALID(this);

	CDaoFieldExchange fx(CDaoFieldExchange::MarkForAddNew, this);
	DoFieldExchange(&fx);
}

void CDaoRecordset::MarkForEdit()
{
	ASSERT_VALID(this);

	CDaoFieldExchange fx(CDaoFieldExchange::MarkForEdit, this);
	DoFieldExchange(&fx);
}

int CDaoRecordset::GetFieldIndex(void* pv)
{
	ASSERT_VALID(this);
	ASSERT(m_pMapFieldIndex != NULL);

	void* pvIndex;

	if (!m_pMapFieldIndex->Lookup(pv, pvIndex))
		return AFX_DAO_DATA_NOT_FOUND;
	else
		// Index was stored rather than ptr, make it 0-based
		return ((int)pvIndex) - 1;
}

void CDaoRecordset::SetDirtyFields()
{
	ASSERT_VALID(this);

	if (m_pDAOFields == NULL)
		InitFieldsCollection();

	CDaoFieldExchange fx(CDaoFieldExchange::SetDirtyField, this);
	DoFieldExchange(&fx);
}

void CDaoRecordset::SetCursorAttributes()
{
	ASSERT_VALID(this);

	m_bScrollable = !(m_nOptions & dbForwardOnly);
	m_bAppendable = CanUpdate() ||
		(!(m_nOptions & dbReadOnly) && (m_nOptions & dbAppendOnly));
}

void CDaoRecordset::GetDataAndFixupNulls()
{
	ASSERT_VALID(this);

	// Don't need to do anything if no fields bound
	if (m_nFields > 0)
	{
		if (IsEOF() || IsBOF())
		{
			// If no current record, simple mark fields NULL
			if (m_nFields > 0)
				SetFieldNull(NULL);
		}
		else
		{
			if (m_pICDAORecordsetGetRows == NULL)
			{
				DAO_CHECK(m_pDAORecordset->QueryInterface(
					IID_ICDAORecordset, (void**)&m_pICDAORecordsetGetRows));
			}

			// Call GetRows to fill in the bound data
			SCODE scode = m_pICDAORecordsetGetRows->GetRows(
				0, m_nFields, m_prgDaoColBindInfo,
				m_cbFixedLengthFields, &m_DaoFetchRows);

			// Check for GetRows specific errors
			//  This is necessary as ICDAORecordset::GetRows
			//  errors are not appended to DAO errors collection
			if (FAILED(scode))
				ThrowGetRowsDaoException(scode);

			// Check to see if row deleted as it is just warning
			m_bDeleted = scode == S_RECORDDELETED;

			// Reset the status array
			ClearFieldStatusFlags();

			// Fixup Null fields
			Fixup();
		}
	}
}

BOOL CDaoRecordset::IsFieldStatusDirty(UINT nField)
{
	ASSERT_VALID(this);

	return ((m_pbFieldFlags[nField] & AFX_DAO_FIELD_FLAG_DIRTY) ==
		AFX_DAO_FIELD_FLAG_DIRTY);
}

void CDaoRecordset::SetDirtyFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] |= AFX_DAO_FIELD_FLAG_DIRTY;
}

void CDaoRecordset::ClearDirtyFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] &= ~AFX_DAO_FIELD_FLAG_DIRTY;
}

BOOL CDaoRecordset::IsFieldStatusNull(UINT nField)
{
	ASSERT_VALID(this);

	return ((m_pbFieldFlags[nField] & AFX_DAO_FIELD_FLAG_NULL) ==
		AFX_DAO_FIELD_FLAG_NULL);
}

void CDaoRecordset::SetNullFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] |= AFX_DAO_FIELD_FLAG_NULL;
}

void CDaoRecordset::ClearNullFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] &= ~AFX_DAO_FIELD_FLAG_NULL;
}

BOOL CDaoRecordset::IsFieldStatusNullable(UINT nField)
{
	ASSERT_VALID(this);

	return ((m_pbFieldFlags[nField] & AFX_DAO_FIELD_FLAG_NULLABLE) ==
		AFX_DAO_FIELD_FLAG_NULLABLE);
}

void CDaoRecordset::SetNullableFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] |= AFX_DAO_FIELD_FLAG_NULLABLE;
}

BOOL CDaoRecordset::IsFieldStatusNullableKnown(UINT nField)
{
	ASSERT_VALID(this);

	return ((m_pbFieldFlags[nField] & AFX_DAO_FIELD_FLAG_NULLABLE_KNOWN)
		== AFX_DAO_FIELD_FLAG_NULLABLE_KNOWN);
}

void CDaoRecordset::SetNullableKnownFieldStatus(UINT nField)
{
	ASSERT_VALID(this);

	m_pbFieldFlags[nField] |= AFX_DAO_FIELD_FLAG_NULLABLE_KNOWN;
}

void CDaoRecordset::ClearFieldStatusFlags()
{
	ASSERT_VALID(this);

	memset(m_pbFieldFlags, 0, m_nFields);
}

BOOL CDaoRecordset::IsMatch()
{
	ASSERT_VALID(this);
	ASSERT(IsOpen());

	short nNoMatch;

	DAO_CHECK(m_pDAORecordset->get_NoMatch(&nNoMatch));

	// Return TRUE if NoMatch is FALSE
	return nNoMatch == AFX_DAO_FALSE;
}

void AFX_CDECL CDaoRecordset::StripBrackets(LPCTSTR lpszSrc, LPTSTR lpszDest)
{
	while (*lpszSrc != '\0')
	{
		// Ignore all brackets
		while (*lpszSrc == '[' || *lpszSrc == ']')
			lpszSrc = _tcsinc(lpszSrc);

		// Quit if at the end of the string
		if (*lpszSrc == '\0')
			break;

		// Copy the data and increment the buffers
		if (_istlead(*lpszSrc))
			*lpszDest++ = *lpszSrc++;
		*lpszDest++ = *lpszSrc++;
	}

	// Add the trailing '\0'
	*lpszDest = '\0';
}

void CDaoRecordset::ThrowDaoException(int nAfxDaoError)
{
	ASSERT_VALID(this);

	AfxThrowDaoException(nAfxDaoError);
}

#ifdef _DEBUG
void CDaoRecordset::AssertValid() const
{
	CObject::AssertValid();
}

void CDaoRecordset::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CObject::Dump(dc);

	dc << "\nm_bOpen = " << m_bOpen;
	dc << "\nm_bAppendable = " << m_bAppendable;
	dc << "\nm_bScrollable = " << m_bScrollable;
	dc << "\nm_bDeleted = " << m_bDeleted;

	dc << "\nm_nOpenType = " << m_nOpenType;
	dc << "\nm_nDefaultType = " << m_nDefaultType;
	dc << "\nm_nOptions = " << m_nOptions;

	dc << "\nm_strSQL = " << m_strSQL;
	dc << "\nm_strFilter = " << m_strFilter;
	dc << "\nm_strSort = " << m_strSort;
	dc << "\nm_strRequerySQL = " << m_strRequerySQL;
	dc << "\nm_strRequeryFilter = " << m_strRequeryFilter;
	dc << "\nm_strRequerySort = " << m_strRequerySort;

	dc << "\nm_nFields = " << m_nFields;
	dc << "\nm_nParams = " << m_nParams;

	dc << "\nm_bCheckCacheForDirtyFields = " << m_bCheckCacheForDirtyFields;
	dc << "\nm_nStatus = " << m_nStatus;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////
// Helpers - implementation
void AFX_CDECL AfxGetFieldInfo(DAOField* pDAOField, CDaoFieldInfo& fieldinfo,
	DWORD dwInfoOptions)
{
	ASSERT(pDAOField != NULL);
	ASSERT(dwInfoOptions != 0);

	COleVariant var;
	short nBool;

	// Always fetch the Primary properties
	if (dwInfoOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAOField->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		fieldinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOField->get_Type(&fieldinfo.m_nType));

		DAO_CHECK(pDAOField->get_Size(&fieldinfo.m_lSize));

		DAO_CHECK(pDAOField->get_Attributes(&fieldinfo.m_lAttributes));
	}

	if (dwInfoOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAOField->get_OrdinalPosition(
			&fieldinfo.m_nOrdinalPosition));

		DAO_CHECK(pDAOField->get_Required(&nBool));
		fieldinfo.m_bRequired = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOField->get_AllowZeroLength(&nBool));
		fieldinfo.m_bAllowZeroLength = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOField->get_CollatingOrder(
			&fieldinfo.m_lCollatingOrder));

		DAO_CHECK(pDAOField->get_SourceField(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		fieldinfo.m_strSourceField = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOField->get_SourceTable(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		fieldinfo.m_strSourceTable = V_BSTRT(&var);
		var.Clear();

		TRY
		{
			DAO_CHECK(pDAOField->get_ForeignName(&V_BSTR(&var)));
			var.vt = VT_BSTR;
			fieldinfo.m_strForeignName = V_BSTRT(&var);
			var.Clear();
		}
		CATCH(CDaoException, e)
		{
			// If this property not appropriate, set foreign name empty
			if (e->m_scode != E_DAO_IllegalOperation)
				THROW_LAST();
			else
			{
				fieldinfo.m_strForeignName.Empty();
				e->Delete();
			}
		}
		END_CATCH
	}

	if (dwInfoOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		AfxGetDefaultValue(pDAOField, fieldinfo.m_strDefaultValue);

		DAO_CHECK(pDAOField->get_ValidationRule(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		fieldinfo.m_strValidationRule = V_BSTRT(&var);
		var.Clear();

		DAO_CHECK(pDAOField->get_ValidationText(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		fieldinfo.m_strValidationText = V_BSTRT(&var);
		var.Clear();
	}
}

void AFX_CDECL AfxSetFieldInfo(DAOField* pDAOField, CDaoFieldInfo& fieldinfo)
{
	// Assumes name, type and size set on direct DAO CreateField call
	ASSERT(pDAOField != NULL);

	if (fieldinfo.m_lAttributes != 0)
		DAO_CHECK(pDAOField->put_Attributes(fieldinfo.m_lAttributes));

	if (fieldinfo.m_nOrdinalPosition != 0)
	{
		DAO_CHECK(pDAOField->put_OrdinalPosition(
			fieldinfo.m_nOrdinalPosition));
	}

	if (fieldinfo.m_bRequired)
		DAO_CHECK(pDAOField->put_Required(AFX_DAO_TRUE));

	if (fieldinfo.m_bAllowZeroLength)
		DAO_CHECK(pDAOField->put_AllowZeroLength(AFX_DAO_TRUE));

	if (!fieldinfo.m_strForeignName.IsEmpty())
	{
		COleVariant var(fieldinfo.m_strForeignName, VT_BSTRT);
		DAO_CHECK(pDAOField->put_ForeignName(V_BSTR(&var)));
	}

	if (!fieldinfo.m_strValidationRule.IsEmpty())
	{
		COleVariant var(fieldinfo.m_strValidationRule, VT_BSTRT);
		DAO_CHECK(pDAOField->put_ValidationRule(V_BSTR(&var)));
	}

	if (!fieldinfo.m_strValidationText.IsEmpty())
	{
		COleVariant var(fieldinfo.m_strValidationText, VT_BSTRT);
		DAO_CHECK(pDAOField->put_ValidationText(V_BSTR(&var)));
	}

	if (!fieldinfo.m_strDefaultValue.IsEmpty())
	{
		AfxSetDefaultValue(pDAOField, fieldinfo.m_strDefaultValue);
	}
}

void AFX_CDECL AfxGetDefaultValue(DAOField* pDAOField, CString& strDefaultValue)
{
	COleVariant var;
	BYTE bUseDao = _AfxDetermineDaoVersion();

	if (bUseDao == 35 || bUseDao == 36)
	{
		// Call the DAO 3.5/3.6 method
		DAO_CHECK(pDAOField->get_DefaultValue(&var));
	}
	else
	{
		// Call DAO 3.0 method
		// get_DefaultValue takes BSTR* param not VARIANT*
		HRESULT (STDMETHODCALLTYPE DAOField::*pMethod)(BSTR*) = (HRESULT (STDMETHODCALLTYPE DAOField::*)(BSTR*))pDAOField->get_DefaultValue;
		DAO_CHECK((pDAOField->*pMethod)(&V_BSTR(&var)));
		var.vt = VT_BSTR;
	}

	strDefaultValue = V_BSTRT(&var);
	var.Clear();
}

void AFX_CDECL AfxSetDefaultValue(DAOField* pDAOField, CString& strDefaultValue)
{
	COleVariant var(strDefaultValue, VT_BSTRT);
	BYTE bUseDao = _AfxDetermineDaoVersion();

	if (bUseDao == 35 || bUseDao == 36)
	{
		// Call DAO 3.5/3.6 version
		DAO_CHECK(pDAOField->put_DefaultValue(var));
	}
	else
	{
		// Call DAO 3.0 method
		// put_DefaultValue takes BSTR param not VARIANT
		HRESULT (STDMETHODCALLTYPE DAOField::*pMethod)(BSTR) = (HRESULT (STDMETHODCALLTYPE DAOField::*)(BSTR))pDAOField->put_DefaultValue;
		DAO_CHECK((pDAOField->*pMethod)(V_BSTR(&var)));
	}
}

void AFX_CDECL AfxGetIndexInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo,
	DWORD dwInfoOptions)
{
	ASSERT(pDAOIndex != NULL);
	ASSERT(dwInfoOptions != 0);

	COleVariant var;
	short nBool;

	if (dwInfoOptions & AFX_DAO_FETCH_PRIMARY_PROPERTIES)
	{
		DAO_CHECK(pDAOIndex->get_Name(&V_BSTR(&var)));
		var.vt = VT_BSTR;
		indexinfo.m_strName = V_BSTRT(&var);
		var.Clear();

		AfxGetIndexFieldInfo(pDAOIndex, indexinfo);
	}

	if (dwInfoOptions & AFX_DAO_FETCH_SECONDARY_PROPERTIES)
	{
		DAO_CHECK(pDAOIndex->get_Primary(&nBool));
		indexinfo.m_bPrimary = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOIndex->get_Unique(&nBool));
		indexinfo.m_bUnique = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOIndex->get_Clustered(&nBool));
		indexinfo.m_bClustered = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOIndex->get_IgnoreNulls(&nBool));
		indexinfo.m_bIgnoreNulls = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOIndex->get_Required(&nBool));
		indexinfo.m_bRequired = nBool == AFX_DAO_TRUE;

		DAO_CHECK(pDAOIndex->get_Foreign(&nBool));
		indexinfo.m_bForeign = nBool == AFX_DAO_TRUE;
	}

	if (dwInfoOptions & AFX_DAO_FETCH_ALL_PROPERTIES)
	{
		DAO_CHECK(pDAOIndex->get_DistinctCount(
			&indexinfo.m_lDistinctCount));
	}
}

void AFX_CDECL AfxSetIndexInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo)
{
	// Assumes name,set on direct DAO CreateIndex call
	ASSERT(pDAOIndex != NULL);

	AfxSetIndexFieldInfo(pDAOIndex, indexinfo);

	if (indexinfo.m_bPrimary)
		DAO_CHECK(pDAOIndex->put_Primary(AFX_DAO_TRUE));

	if (indexinfo.m_bUnique)
		DAO_CHECK(pDAOIndex->put_Unique(AFX_DAO_TRUE));

	if (indexinfo.m_bClustered)
		DAO_CHECK(pDAOIndex->put_Clustered(AFX_DAO_TRUE));

	if (indexinfo.m_bIgnoreNulls)
		DAO_CHECK(pDAOIndex->put_IgnoreNulls(AFX_DAO_TRUE));

	if (indexinfo.m_bRequired)
		DAO_CHECK(pDAOIndex->put_Required(AFX_DAO_TRUE));
}

void AFX_CDECL AfxGetIndexFields(DAOIndex* pDAOIndex,
	DAOIndexFields** ppDAOIndexFields)
{
	COleVariant var;
	BYTE bUseDao = _AfxDetermineDaoVersion();

	// Set the desired interface
	GUID guidIndexFields;
   if (bUseDao == 35 || bUseDao == 36)
   {
	  // Use DAO 3.5/3.6
#ifdef _UNICODE
	   guidIndexFields = IID_IDAOIndexFieldsW;
#else
	   guidIndexFields = IID_IDAOIndexFields;
#endif
   }
   else
   {
	  // Use DAO 3.0
#ifdef _UNICODE
	  guidIndexFields = IID30_IDAOIndexFieldsW;
#else
	  guidIndexFields = IID30_IDAOIndexFields;
#endif
   }

	// Get dispatch pointer to fields collection
	DAO_CHECK(pDAOIndex->get_Fields(&var));
	DAO_CHECK(var.pdispVal->QueryInterface(
		guidIndexFields, (void**)ppDAOIndexFields));
}

void AFX_CDECL AfxGetIndexFieldInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo)
{
	COleVariant var;

	DAOIndexFields* pDAOIndexFields = NULL;
	DAOField* pDAOField = NULL;

	// Get the index fields collection
	AfxGetIndexFields(pDAOIndex, &pDAOIndexFields);

	TRY
	{
		// Get the number of fields in the index
		short nCount;
		DAO_CHECK(pDAOIndexFields->get_Count(&nCount));

		// Allocate or reallocate memory for array if necessary
		if (nCount != indexinfo.m_nFields)
		{
			if (indexinfo.m_nFields != 0)
			{
				// Check that allocation is correct.
				ASSERT(indexinfo.m_nFields == 0 ||
					indexinfo.m_bCleanupFieldInfo);

				delete[] indexinfo.m_pFieldInfos;
				indexinfo.m_pFieldInfos = NULL;
			}

			// Now allocate required memory
			indexinfo.m_pFieldInfos = new CDaoIndexFieldInfo[nCount];
			indexinfo.m_bCleanupFieldInfo = TRUE;
			indexinfo.m_nFields = nCount;
		}

	   BYTE bUseDao = _AfxDetermineDaoVersion();

		// Now get field info for each field
		long lAttributes;
		for (short nIndex = 0; nIndex < indexinfo.m_nFields; nIndex++)
		{
			// Set the desired interface
			GUID guidField;

		 if (bUseDao == 35 || bUseDao == 36)
		 {
#ifdef _UNICODE
			   guidField = IID_IDAOFieldW;
#else
			   guidField = IID_IDAOField;
#endif
		 }
		 else
		 {
#ifdef _UNICODE
			guidField = IID30_IDAOFieldW;
#else
			guidField = IID30_IDAOField;
#endif
		 }

			// Get the field item
			DAO_CHECK(pDAOIndexFields->get_Item(COleVariant(nIndex), &var));
			DAO_CHECK(var.pdispVal->QueryInterface(
				guidField, (void**)&pDAOField));
			var.Clear();

			// Get the field name
			DAO_CHECK(pDAOField->get_Name(&V_BSTR(&var)));
			var.vt = VT_BSTR;
			indexinfo.m_pFieldInfos[nIndex].m_strName = V_BSTRT(&var);
			var.Clear();

			// Get the field attributes
			DAO_CHECK(pDAOField->get_Attributes(&lAttributes));
			indexinfo.m_pFieldInfos[nIndex].m_bDescending =
				(lAttributes & dbDescending);

			// Release and reset the field object
			pDAOField->Release();
			pDAOField = NULL;
		}

		// Release the index fields object
		pDAOIndexFields->Release();
	}
	CATCH_ALL(e)
	{
		// Release the field object if necessary
		if (pDAOField != NULL)
			pDAOField->Release();

		// Must always release index fields collection
		pDAOIndexFields->Release();
		THROW_LAST();
	}
	END_CATCH_ALL
}

void AFX_CDECL AfxSetIndexFieldInfo(DAOIndex* pDAOIndex, CDaoIndexInfo& indexinfo)
{
	COleVariant var;
	long lAttributes;

	DAOIndexFields* pDAOIndexFields = NULL;
	DAOField* pDAOField = NULL;

	// Get the index fields collection
	AfxGetIndexFields(pDAOIndex, &pDAOIndexFields);

	TRY
	{
		for (int nIndex = 0; nIndex < indexinfo.m_nFields; nIndex++)
		{
			// Create the field and set the name
			DAO_CHECK(pDAOIndex->CreateField(
				COleVariant(indexinfo.m_pFieldInfos[nIndex].m_strName, VT_BSTRT),
				_afxOptionalVariant, _afxOptionalVariant, &pDAOField));

			// Set the descending property
			if (indexinfo.m_pFieldInfos[nIndex].m_bDescending)
				lAttributes = dbDescending;
			else
				lAttributes = 0;
			DAO_CHECK(pDAOField->put_Attributes(lAttributes));

			// Append field to collection, release and reset
			pDAOIndexFields->Append(pDAOField);
			pDAOField->Release();
			pDAOField = NULL;
		}

		// Always release the index fields object
		pDAOIndexFields->Release();
	}
	CATCH_ALL(e)
	{
		if (pDAOField != NULL)
			pDAOField->Release();

		pDAOIndexFields->Release();
		THROW_LAST();
	}
	END_CATCH_ALL
}

//////////////////////////////////////////////////////////////////////////////
// GetRows helper
void AFX_CDECL ThrowGetRowsDaoException(SCODE scode)
{
	switch (scode)
	{
		case E_OUTOFMEMORY:
			AfxThrowMemoryException();

		default:
			AfxThrowDaoException(NO_AFX_DAO_ERROR, scode);

		// These are only (known) DAO 3.0 GetRows errors
		case E_ROWTOOSHORT:
		case E_BADBINDINFO:
		case E_COLUMNUNAVAILABLE:
			break;
	}

	CDaoException* pException;
	pException = new CDaoException;
	pException->m_pErrorInfo = new CDaoErrorInfo;

	// Fill out the DAO error info struct
	pException->m_scode = scode;
	pException->m_pErrorInfo->m_strSource = _T("ICDAORecordset.GetRows");
	pException->m_pErrorInfo->m_lErrorCode = LOWORD(scode);

	// There is no help context or help file
	pException->m_pErrorInfo->m_lHelpContext = 0;

	int nID = 0;

	if (scode == E_ROWTOOSHORT)
		nID = AFX_IDP_DAO_ROWTOOSHORT;
	else if (scode == E_BADBINDINFO)
		nID = AFX_IDP_DAO_BADBINDINFO;
	else
		nID = AFX_IDP_DAO_COLUMNUNAVAILABLE;

	VERIFY(pException->m_pErrorInfo->m_strDescription.LoadString(nID));

#ifdef _DEBUG
	if (afxTraceFlags & traceDatabase)
	{
		TRACE1("\nError Code = %d\n",
			pException->m_pErrorInfo->m_lErrorCode);
		TRACE1("Source = %s\n",
			(LPCTSTR)pException->m_pErrorInfo->m_strSource);
		TRACE1("Description = %s\n",
			(LPCTSTR)pException->m_pErrorInfo->m_strDescription);
	}
#endif

	THROW(pException);
}

//////////////////////////////////////////////////////////////////////////////
// _AFX_DAO_STATE helper
_AFX_DAO_STATE* AFX_CDECL AfxGetDaoState()
{
	// The DAO state is store in the module state
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	_AFX_DAO_STATE* pDaoState = pModuleState->m_pDaoState;

	// Allocate a new DAO state if necessary
	if (pDaoState == NULL)
	{
		pDaoState = new _AFX_DAO_STATE;
		pModuleState->m_pDaoState = pDaoState;
	}
	return pDaoState;
}


//////////////////////////////////////////////////////////////////////////////
// DAODBEngine helpers
void AFXAPI AfxDaoInit()
{
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();

	// Attempt to initialize OLE component objects
	//  (use default IMalloc allocator)
	DAO_CHECK_ERROR(::CoInitialize(NULL),
		AFX_DAO_ERROR_ENGINE_INITIALIZATION);
	pDaoState->m_bOleInitialized = TRUE;

	// Hook AfxDaoTerm to CWinApp, otherwise explicit AfxDaoTerm call req'd
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		pApp->m_lpfnDaoTerm = AfxDaoTerm;

	BYTE bUseDao = _AfxDetermineDaoVersion();

	CLSID clsidEngine;
	COleVariant varKey;
	GUID guidEngine;

	// Set the interface type
#ifdef _UNICODE
			guidEngine = IID_IDAODBEngineW;
#else
			guidEngine = IID_IDAODBEngine;
#endif

	// DAO 3.5 and 3.6 runtime key
	varKey = _T("mbmabptebkjcdlgtjmskjwtsdhjbmkmwtrak");

	switch (bUseDao)
	{
		case 35:
			// Use DAO 3.5
			clsidEngine = CLSID35_CDAODBEngine;
			break;

		case 30:
			// Use DAO 3.0
			clsidEngine = CLSID30_CDAODBEngine;

			// DAO 3.0 runtime key
			varKey = _T("mjgcqcejfchcijecpdhckcdjqigdejfccjri");

			// Set the interface type
#ifdef _UNICODE
			guidEngine = IID30_IDAODBEngineW;
#else
			guidEngine = IID30_IDAODBEngine;
#endif
			break;

		case 36:
			// Use DAO 3.6
			clsidEngine = CLSID_CDAODBEngine;
			break;

		default:
			ASSERT(FALSE);
	}

	LPCLASSFACTORY2 pCF2;
	DAO_CHECK_ERROR(::CoGetClassObject(clsidEngine,
		CLSCTX_INPROC_SERVER, NULL, IID_IClassFactory2, (LPVOID*)&pCF2),
		AFX_DAO_ERROR_ENGINE_INITIALIZATION);

	TRY
	{
		DAO_CHECK_ERROR(pCF2->CreateInstanceLic( NULL, NULL, guidEngine,
			V_BSTR(&varKey), (LPVOID*)&pDaoState->m_pDAODBEngine),
			AFX_DAO_ERROR_ENGINE_INITIALIZATION);
	}
	CATCH_ALL(e)
	{
		pCF2->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	pCF2->Release();
}

DAODBEngine* AFXAPI AfxDaoGetEngine()
{
	// Return DAODBEngine object for direct access
	return AfxGetDaoState()->m_pDAODBEngine;
}

void AFXAPI AfxDaoTerm()
{
	_AFX_DAO_STATE* pDaoState = AfxGetDaoState();

	// Close any CDaoWorkspaces left around
	void* pvKey;
	void* pvObject;
	POSITION pos = pDaoState->m_mapWorkspaces.GetStartPosition();
	while (pos != NULL)
	{
		pDaoState->m_mapWorkspaces.GetNextAssoc(pos, pvKey, pvObject);
		((CDaoWorkspace*)pvObject)->Close();
	}
	pDaoState->m_mapWorkspaces.RemoveAll();

	// Clean up engine object if necessary
	if (pDaoState->m_pDAODBEngine != NULL)
	{
		pDaoState->m_pDAODBEngine->Release();
		pDaoState->m_pDAODBEngine = NULL;
	}

	// Shutdown OLE component objects if necessary
	if (pDaoState->m_bOleInitialized)
	{
		::CoUninitialize();
		pDaoState->m_bOleInitialized = FALSE;
	}

	// If hooked up to CWinApp, make sure to unhook
	CWinApp* pApp = AfxGetApp();
	if (pApp != NULL)
		pApp->m_lpfnDaoTerm = NULL;
}


//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxDaoInl[] = "afxdao.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDaoInl
#define _AFXDAOCORE_INLINE
#include "afxdao.inl"

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#pragma warning(disable: 4074)
#pragma init_seg(lib)

/////////////////////////////////////////////////////////////////////////////
