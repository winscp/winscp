// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXDB_H__
#define __AFXDB_H__

#ifdef _AFX_NO_DB_SUPPORT
	#error Database classes not supported in this library variant.
#endif

#ifndef __AFXEXT_H__
	#include <afxext.h>
#endif

#ifndef __AFXDB__H__
	#include <afxdb_.h> // shared header DAO database classes
#endif

// include standard SQL/ODBC "C" APIs
#ifndef __SQL
	#define SQL_NOUNICODEMAP
	#include <sql.h>        // core
#endif
#ifndef __SQLEXT
	#include <sqlext.h>     // extensions
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifndef _AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////
// Win32 libraries

#ifdef _AFXDLL
	#if defined(_DEBUG) && !defined(_AFX_MONOLITHIC)
		#ifndef _UNICODE
			#pragma comment(lib, "mfcd42d.lib")
		#else
			#pragma comment(lib, "mfcd42ud.lib")
		#endif
	#endif
#endif

#pragma comment(lib, "odbc32.lib")
#pragma comment(lib, "odbccp32.lib")

#endif //!_AFX_NOFORCE_LIBS

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXDB - MFC SQL/ODBC/Database support

// Classes declared in this file

	//CException
		class CDBException;    // abnormal return value

	//CFieldExchange
		class CFieldExchange;       // Recordset Field Exchange

	//CObject
		class CDatabase;    // Connecting to databases
		class CRecordset;   // Data result sets

//CObject
	//CCmdTarget;
		//CWnd
			//CView
				//CScrollView
					//CFormView
						class CRecordView;     // view records with a form

// Non CObject classes
class CDBVariant;
struct CRecordsetStatus;
struct CFieldInfo;
struct CODBCFieldInfo;
struct CODBCParamInfo;

/////////////////////////////////////////////////////////////////////////////

// ODBC helpers
// return code left in 'nRetCode'

// This MACRO is now out-of-date (kept for backward compatibility)
#define AFX_ODBC_CALL(SQLFunc) \
	do \
	{ \
	} while ((nRetCode = (SQLFunc)) == SQL_STILL_EXECUTING)

// Not really required, but kept for compatibilty
#define AFX_SQL_SYNC(SQLFunc) \
	do \
	{ \
		nRetCode = SQLFunc; \
	} while (0)

// Now out-of-date (prs not used) but kept for compatibility
#define AFX_SQL_ASYNC(prs, SQLFunc) AFX_ODBC_CALL(SQLFunc)

// Max display length in chars of timestamp (date & time) value
#define TIMESTAMP_PRECISION 23

// AFXDLL support
#undef AFX_DATA
#define AFX_DATA AFX_DB_DATA

//  Miscellaneous sizing info
#define MAX_CURRENCY     30     // Max size of Currency($) string
#define MAX_TNAME_LEN    64     // Max size of table names
#define MAX_FNAME_LEN    256    // Max size of field names
#define MAX_DBNAME_LEN   32     // Max size of a database name
#define MAX_DNAME_LEN    256        // Max size of Recordset names
#define MAX_CONNECT_LEN  512        // Max size of Connect string
#define MAX_CURSOR_NAME  18     // Max size of a cursor name
#define DEFAULT_FIELD_TYPE SQL_TYPE_NULL // pick "C" data type to match SQL data type

// Timeout and net wait defaults
#define DEFAULT_LOGIN_TIMEOUT 15    // seconds to before fail on connect
#define DEFAULT_QUERY_TIMEOUT 15    // seconds to before fail waiting for results

// Field Flags, used to indicate status of fields
#define AFX_SQL_FIELD_FLAG_DIRTY    0x1
#define AFX_SQL_FIELD_FLAG_NULL     0x2

// Update options flags
#define AFX_SQL_SETPOSUPDATES       0x0001
#define AFX_SQL_POSITIONEDSQL       0x0002
#define AFX_SQL_GDBOUND             0x0004

/////////////////////////////////////////////////////////////////////////////
// CDBException - something gone wrong

// Dbkit extended error codes
#define AFX_SQL_ERROR                           1000
#define AFX_SQL_ERROR_CONNECT_FAIL              AFX_SQL_ERROR+1
#define AFX_SQL_ERROR_RECORDSET_FORWARD_ONLY    AFX_SQL_ERROR+2
#define AFX_SQL_ERROR_EMPTY_COLUMN_LIST         AFX_SQL_ERROR+3
#define AFX_SQL_ERROR_FIELD_SCHEMA_MISMATCH     AFX_SQL_ERROR+4
#define AFX_SQL_ERROR_ILLEGAL_MODE              AFX_SQL_ERROR+5
#define AFX_SQL_ERROR_MULTIPLE_ROWS_AFFECTED    AFX_SQL_ERROR+6
#define AFX_SQL_ERROR_NO_CURRENT_RECORD         AFX_SQL_ERROR+7
#define AFX_SQL_ERROR_NO_ROWS_AFFECTED          AFX_SQL_ERROR+8
#define AFX_SQL_ERROR_RECORDSET_READONLY        AFX_SQL_ERROR+9
#define AFX_SQL_ERROR_SQL_NO_TOTAL              AFX_SQL_ERROR+10
#define AFX_SQL_ERROR_ODBC_LOAD_FAILED          AFX_SQL_ERROR+11
#define AFX_SQL_ERROR_DYNASET_NOT_SUPPORTED     AFX_SQL_ERROR+12
#define AFX_SQL_ERROR_SNAPSHOT_NOT_SUPPORTED    AFX_SQL_ERROR+13
#define AFX_SQL_ERROR_API_CONFORMANCE           AFX_SQL_ERROR+14
#define AFX_SQL_ERROR_SQL_CONFORMANCE           AFX_SQL_ERROR+15
#define AFX_SQL_ERROR_NO_DATA_FOUND             AFX_SQL_ERROR+16
#define AFX_SQL_ERROR_ROW_UPDATE_NOT_SUPPORTED  AFX_SQL_ERROR+17
#define AFX_SQL_ERROR_ODBC_V2_REQUIRED          AFX_SQL_ERROR+18
#define AFX_SQL_ERROR_NO_POSITIONED_UPDATES     AFX_SQL_ERROR+19
#define AFX_SQL_ERROR_LOCK_MODE_NOT_SUPPORTED   AFX_SQL_ERROR+20
#define AFX_SQL_ERROR_DATA_TRUNCATED            AFX_SQL_ERROR+21
#define AFX_SQL_ERROR_ROW_FETCH                 AFX_SQL_ERROR+22
#define AFX_SQL_ERROR_INCORRECT_ODBC            AFX_SQL_ERROR+23
#define AFX_SQL_ERROR_UPDATE_DELETE_FAILED      AFX_SQL_ERROR+24
#define AFX_SQL_ERROR_DYNAMIC_CURSOR_NOT_SUPPORTED  AFX_SQL_ERROR+25
#define AFX_SQL_ERROR_FIELD_NOT_FOUND           AFX_SQL_ERROR+26
#define AFX_SQL_ERROR_BOOKMARKS_NOT_SUPPORTED   AFX_SQL_ERROR+27
#define AFX_SQL_ERROR_BOOKMARKS_NOT_ENABLED     AFX_SQL_ERROR+28
#define AFX_SQL_ERROR_MAX                       AFX_SQL_ERROR+29

class CDBException : public CException
{
	DECLARE_DYNAMIC(CDBException)

// Attributes
public:
	RETCODE m_nRetCode;
	CString m_strError;
	CString m_strStateNativeOrigin;

// Implementation (use AfxThrowDBException to create)
public:
	CDBException(RETCODE nRetCode = SQL_SUCCESS);

	virtual void BuildErrorString(CDatabase* pdb, HSTMT hstmt,
		BOOL bTrace = TRUE);
	void Empty();
	virtual ~CDBException();

	virtual BOOL GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext = NULL);

#ifdef _DEBUG
	void TraceErrorMessage(LPCTSTR szTrace) const;
#endif // DEBUG

};

void AFXAPI AfxThrowDBException(RETCODE nRetCode, CDatabase* pdb, HSTMT hstmt);

//////////////////////////////////////////////////////////////////////////////
// CDatabase - a SQL Database

class CDatabase : public CObject
{
	DECLARE_DYNAMIC(CDatabase)

// Constructors
public:
	CDatabase();

	enum DbOpenOptions
	{
		openExclusive =         0x0001, // Not implemented
		openReadOnly =          0x0002, // Open database read only
		useCursorLib =          0x0004, // Use ODBC cursor lib
		noOdbcDialog =          0x0008, // Don't display ODBC Connect dialog
		forceOdbcDialog =       0x0010, // Always display ODBC connect dialog
	};

	virtual BOOL Open(LPCTSTR lpszDSN, BOOL bExclusive = FALSE,
		BOOL bReadonly = FALSE, LPCTSTR lpszConnect = _T("ODBC;"),
		BOOL bUseCursorLib = TRUE);
	virtual BOOL OpenEx(LPCTSTR lpszConnectString, DWORD dwOptions = 0);
	virtual void Close();

// Attributes
public:
	HDBC m_hdbc;

	BOOL IsOpen() const;        // Database successfully opened?
	BOOL CanUpdate() const;
	BOOL CanTransact() const;   // Are Transactions supported?

	CString GetDatabaseName() const;
	const CString& GetConnect() const;

	DWORD GetBookmarkPersistence() const;
	int GetCursorCommitBehavior() const;
	int GetCursorRollbackBehavior() const;

// Operations
public:
	void SetLoginTimeout(DWORD dwSeconds);
	void SetQueryTimeout(DWORD dwSeconds);

	// transaction control
	BOOL BeginTrans();
	BOOL CommitTrans();
	BOOL Rollback();

	void ExecuteSQL(LPCTSTR lpszSQL);

	// Cancel asynchronous operation
	void Cancel();

// Overridables
public:
	// set special options
	virtual void OnSetOptions(HSTMT hstmt);

// Implementation
public:
	virtual ~CDatabase();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;

	BOOL m_bTransactionPending;
#endif //_DEBUG

	// general error check
	virtual BOOL Check(RETCODE nRetCode) const;
	BOOL PASCAL CheckHstmt(RETCODE, HSTMT hstmt) const;

	// Note: CDatabase::BindParameters is now documented and is no longer
	//  officially 'implementation.'  Feel free to use it.  It stays here
	//  because moving it would break binary compatibility.
	virtual void BindParameters(HSTMT hstmt);

	void ReplaceBrackets(LPTSTR lpchSQL);
	BOOL m_bStripTrailingSpaces;
	BOOL m_bIncRecordCountOnAdd;
	BOOL m_bAddForUpdate;
	char m_chIDQuoteChar;
	char m_reserved1[3];        // pad to even 4 bytes

	void SetSynchronousMode(BOOL bSynchronous); // Obsolete, doe nothing

protected:
	CString m_strConnect;

	CPtrList m_listRecordsets;  // maintain list to ensure CRecordsets all closed

	int nRefCount;
	BOOL m_bUpdatable;

	BOOL m_bTransactions;
	SWORD m_nTransactionCapable;
	SWORD m_nCursorCommitBehavior;
	SWORD m_nCursorRollbackBehavior;
	DWORD m_dwUpdateOptions;
	DWORD m_dwBookmarkAttributes;   // cache driver bookmark persistence

	DWORD m_dwLoginTimeout;
	HSTMT m_hstmt;

	DWORD m_dwQueryTimeout;

	virtual void ThrowDBException(RETCODE nRetCode);
	void AllocConnect(DWORD dwOptions);
	BOOL Connect(DWORD dwOptions);
	void VerifyConnect();
	void GetConnectInfo();
	void Free();

	// friend classes that call protected CDatabase overridables
	friend class CRecordset;
	friend class CFieldExchange;
	friend class CDBException;
};

//////////////////////////////////////////////////////////////////////////////
// CFieldExchange - for field exchange
class CFieldExchange
{
// Attributes
public:
	enum RFX_Operation
	{
		BindParam,          // register users parameters with ODBC SQLBindParameter
		RebindParam,        //  migrate param values to proxy array before Requery
		BindFieldToColumn,  // register users fields with ODBC SQLBindCol
		BindFieldForUpdate, // temporarily bind columns before update (via SQLSetPos)
		UnbindFieldForUpdate,   // unbind columns after update (via SQLSetPos)
		Fixup,              // Set string lengths, clear status bits
		MarkForAddNew,      // Prepare fields and flags for addnew operation
		MarkForUpdate,      // Prepare fields and flags for update operation
		Name,               // append dirty field name
		NameValue,          // append dirty name=value
		Value,              // append dirty value or parameter marker
		SetFieldNull,       // Set status bit for null value
		StoreField,         // archive values of current record
		LoadField,          // reload archived values into current record
		AllocCache,         // allocate cache used for dirty field check
		AllocMultiRowBuffer,    // allocate buffer holding multi rows of data
		DeleteMultiRowBuffer,   // delete buffer holding multi rows of data
#ifdef _DEBUG
		DumpField,          // dump bound field name and value
#endif
	};
	UINT m_nOperation;  // Type of exchange operation
	CRecordset* m_prs;  // recordset handle

// Operations
	enum FieldType
	{
		noFieldType     = -1,
		outputColumn    = 0,
		param           = SQL_PARAM_INPUT,
		inputParam      = param,
		outputParam     = SQL_PARAM_OUTPUT,
		inoutParam      = SQL_PARAM_INPUT_OUTPUT,
	};

// Operations (for implementors of RFX procs)
	BOOL IsFieldType(UINT* pnField);

	// Indicate purpose of subsequent RFX calls
	void SetFieldType(UINT nFieldType);

// Implementation
	CFieldExchange(UINT nOperation, CRecordset* prs, void* pvField = NULL);

	void Default(LPCTSTR szName,
		void* pv, LONG* plLength, int nCType, UINT cbValue, UINT cbPrecision);

	// long binary helpers
	long GetLongBinarySize(int nField);
	void GetLongBinaryData(int nField, CLongBinary& lb, long* plSize);
	BYTE* ReallocLongBinary(CLongBinary& lb, long lSizeRequired,
		long lReallocSize);

	// Current type of field
	UINT m_nFieldType;

	UINT m_nFieldFound;

	CString* m_pstr;    // Field name or destination for building various SQL clauses
	BOOL m_bField;      // Value to set for SetField operation
	void* m_pvField;    // For indicating an operation on a specific field
	LPCTSTR m_lpszSeparator; // append after field names
	UINT m_nFields;     // count of fields for various operations
	UINT m_nParams;     // count of fields for various operations
	UINT m_nParamFields;    // count of fields for various operations
	HSTMT m_hstmt;      // For SQLBindParameter on update statement
	long m_lDefaultLBFetchSize;     // For fetching CLongBinary data of unknown len
	long m_lDefaultLBReallocSize;   // For fetching CLongBinary data of unknown len

#ifdef _DEBUG
	CDumpContext* m_pdcDump;
#endif //_DEBUG

};

/////////////////////////////////////////////////////////////////////////////
// Global helper

HENV AFXAPI AfxGetHENV();

/////////////////////////////////////////////////////////////////////////////
// Recordset Field Exchange helpers

void AFXAPI AfxStoreField(CRecordset& rs, UINT nField, void* pvField);
void AFXAPI AfxLoadField(CRecordset& rs, UINT nField,
	void* pvField, long* plLength);
BOOL AFXAPI AfxCompareValueByRef(void* pvData, void* pvCache, int nDataType);
void AFXAPI AfxCopyValueByRef(void* pvCache, void* pvData,
	long* plLength, int nDataType);

/////////////////////////////////////////////////////////////////////////////
// Standard Recordset Field Exchange routines

// text data
void AFXAPI RFX_Text(CFieldExchange* pFX, LPCTSTR szName, CString& value,
	// Default max length for char and varchar, default datasource type
	int nMaxLength = 255, int nColumnType = SQL_VARCHAR, short nScale = 0);

void AFXAPI RFX_Text(CFieldExchange* pFX, LPCTSTR szName, LPTSTR value,
	int nMaxLength, int nColumnType = SQL_VARCHAR, short nScale = 0);

// boolean data
void AFXAPI RFX_Bool(CFieldExchange* pFX, LPCTSTR szName, BOOL& value);

// integer data
void AFXAPI RFX_Long(CFieldExchange* pFX, LPCTSTR szName, long& value);
void AFXAPI RFX_Int(CFieldExchange* pFX, LPCTSTR szName, int& value);
void AFXAPI RFX_Single(CFieldExchange* pFX, LPCTSTR szName, float& value);
void AFXAPI RFX_Double(CFieldExchange* pFX, LPCTSTR szName, double& value);

// date and time
void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName, CTime& value);
void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName, TIMESTAMP_STRUCT& value);
void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName, COleDateTime& value);

// Binary data
void AFXAPI RFX_Binary(CFieldExchange* pFX, LPCTSTR szName, CByteArray& value,
	// Default max length is for binary and varbinary
	int nMaxLength = 255);
void AFXAPI RFX_Byte(CFieldExchange* pFX, LPCTSTR szName, BYTE& value);
void AFXAPI RFX_LongBinary(CFieldExchange* pFX, LPCTSTR szName, CLongBinary& value);

/////////////////////////////////////////////////////////////////////////////
// Bulk Recordset Field Exchange helpers
void AFXAPI AfxRFXBulkDefault(CFieldExchange* pFX, LPCTSTR szName,
	void* pv, long* rgLengths, int nCType, UINT cbValue);

/////////////////////////////////////////////////////////////////////////////
// Bulk Recordset Field Exchange routines

void AFXAPI RFX_Text_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	LPSTR* prgStrVals, long** prgLengths, int nMaxLength);

void AFXAPI RFX_Bool_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BOOL** prgBoolVals, long** prgLengths);
void AFXAPI RFX_Int_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	int** prgIntVals, long** prgLengths);
void AFXAPI RFX_Long_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	long** prgLongVals, long** prgLengths);

void AFXAPI RFX_Single_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	float** prgFltVals, long** prgLengths);
void AFXAPI RFX_Double_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	double** prgDblVals, long** prgLengths);

void AFXAPI RFX_Date_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	TIMESTAMP_STRUCT** prgTSVals, long** prgLengths);

void AFXAPI RFX_Byte_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BYTE** prgByteVals, long** prgLengths);
void AFXAPI RFX_Binary_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BYTE** prgByteVals, long** prgLengths, int nMaxLength);

/////////////////////////////////////////////////////////////////////////////
// Database Dialog Data Exchange cover routines
// Cover routines provide database semantics on top of DDX routines

// simple text operations
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, BYTE& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, int& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, UINT& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, long& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, DWORD& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, CString& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, LPTSTR pstrValue,
	int nMaxLen, CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, double& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, float& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, CTime& value,
	CRecordset* pRecordset);

// special control types
void AFXAPI DDX_FieldCheck(CDataExchange* pDX, int nIDC, int& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldRadio(CDataExchange* pDX, int nIDC, int& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldLBString(CDataExchange* pDX, int nIDC,
	CString& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldCBString(CDataExchange* pDX, int nIDC,
	CString& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldLBIndex(CDataExchange* pDX, int nIDC, int& index,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldCBIndex(CDataExchange* pDX, int nIDC, int& index,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldLBStringExact(CDataExchange* pDX, int nIDC,
	CString& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldCBStringExact(CDataExchange* pDX, int nIDC,
	CString& value,
	CRecordset* pRecordset);
void AFXAPI DDX_FieldScroll(CDataExchange* pDX, int nIDC, int& value,
	CRecordset* pRecordset);

//////////////////////////////////////////////////////////////////////////////
// CRecordset - the result of a SQL Statement

#define AFX_DB_USE_DEFAULT_TYPE     (0xFFFFFFFF)

// Most Move constants out of date
// #define AFX_MOVE_FIRST      0x80000000L
// #define AFX_MOVE_PREVIOUS   (-1L)
#define AFX_MOVE_REFRESH    0L
// #define AFX_MOVE_NEXT       (+1L)
// #define AFX_MOVE_LAST       0x7fffffffL

#define AFX_RECORDSET_STATUS_OPEN    (+1L)
#define AFX_RECORDSET_STATUS_CLOSED  0L
#define AFX_RECORDSET_STATUS_UNKNOWN (-1L)

class CRecordset : public CObject
{
	DECLARE_DYNAMIC(CRecordset)

// Constructor
public:
	CRecordset(CDatabase* pDatabase = NULL);

public:
	virtual ~CRecordset();

	enum OpenType
	{
		dynaset,        // uses SQLExtendedFetch, keyset driven cursor
		snapshot,       // uses SQLExtendedFetch, static cursor
		forwardOnly,    // uses SQLFetch
		dynamic         // uses SQLExtendedFetch, dynamic cursor
	};

	enum OpenOptions
	{
		none =                      0x0,
		readOnly =                  0x0004,
		appendOnly =                0x0008,
		skipDeletedRecords =        0x0010, // turn on skipping of deleted records, Will slow Move(n).
		noDirtyFieldCheck =         0x0020, // disable automatic dirty field checking
		useBookmarks =              0x0100, // turn on bookmark support
		useMultiRowFetch =          0x0200, // turn on multi-row fetch model
		userAllocMultiRowBuffers =  0x0400, // if multi-row fetch on, user will alloc memory for buffers
		useExtendedFetch =          0x0800, // use SQLExtendedFetch with forwardOnly type recordsets
		executeDirect =             0x2000, // Directly execute SQL rather than prepared execute
		optimizeBulkAdd =           0x4000, // Use prepared HSTMT for multiple AddNews, dirty fields must not change.
		firstBulkAdd =              0x8000, // INTERNAL to MFC, don't specify on Open.
	};
	virtual BOOL Open(UINT nOpenType = AFX_DB_USE_DEFAULT_TYPE,
		LPCTSTR lpszSQL = NULL, DWORD dwOptions = none);
	virtual void Close();

// Attributes
public:
	HSTMT m_hstmt;          // Source statement for this resultset
	CDatabase* m_pDatabase;       // Source database for this resultset

	CString m_strFilter;        // Where clause
	CString m_strSort;      // Order By Clause

	BOOL CanAppend() const;     // Can AddNew be called?
	BOOL CanRestart() const;    // Can Requery be called to restart a query?
	BOOL CanScroll() const;     // Can MovePrev and MoveFirst be called?
	BOOL CanTransact() const;   // Are Transactions supported?
	BOOL CanUpdate() const;     // Can Edit/AddNew/Delete be called?
	BOOL CanBookmark() const;       // Can Get/SetBookmark be called?

	const CString& GetSQL() const;      // SQL executed for this recordset
	const CString& GetTableName() const;        // Table name

	BOOL IsOpen() const;        // Recordset successfully opened?
	BOOL IsBOF() const;     // Beginning Of File
	BOOL IsEOF() const;     // End Of File
	BOOL IsDeleted() const;     // On a deleted record

	BOOL IsFieldDirty(void *pv);    // has field been updated?
	BOOL IsFieldNull(void *pv); // is field NULL valued?
	BOOL IsFieldNullable(void *pv); // can field be set to a NULL value

	long GetRecordCount() const;        // Records seen so far or -1 if unknown
	void GetStatus(CRecordsetStatus& rStatus) const;

// Operations
public:
	// cursor operations
	void MoveNext();
	void MovePrev();
	void MoveFirst();
	void MoveLast();
	virtual void Move(long nRows, WORD wFetchType = SQL_FETCH_RELATIVE);

	void SetAbsolutePosition(long nRows);

	void GetBookmark(CDBVariant& varBookmark);
	void SetBookmark(const CDBVariant& varBookmark);

	virtual void SetRowsetSize(DWORD dwNewRowsetSize);
	DWORD GetRowsetSize() const;
	DWORD GetRowsFetched() const;
	virtual void CheckRowsetError(RETCODE nRetCode);
	void RefreshRowset(WORD wRow, WORD wLockType = SQL_LOCK_NO_CHANGE);
	void SetRowsetCursorPosition(WORD wRow, WORD wLockType = SQL_LOCK_NO_CHANGE);
	WORD GetRowStatus(WORD wRow) const;

	// edit buffer operations
	virtual void AddNew();      // add new record at the end
	virtual void Edit();        // start editing
	virtual BOOL Update();      // update it
	virtual void Delete();      // delete the current record
	void CancelUpdate();        // cancel pending Edit/AddNew

	BOOL FlushResultSet() const;

	// field operations
	short GetODBCFieldCount() const;
	void GetODBCFieldInfo(short nIndex, CODBCFieldInfo& fieldinfo);
	void GetODBCFieldInfo(LPCTSTR lpszName, CODBCFieldInfo& fieldinfo);
	void GetFieldValue(LPCTSTR lpszName, CDBVariant& varValue,
		short nFieldType = DEFAULT_FIELD_TYPE);
	void GetFieldValue(short nIndex, CDBVariant& varValue,
		short nFieldType = DEFAULT_FIELD_TYPE);
	void GetFieldValue(LPCTSTR lpszName, CString& strValue);
	void GetFieldValue(short nIndex, CString& strValue);

	void SetFieldDirty(void *pv, BOOL bDirty = TRUE);
	void SetFieldNull(void *pv, BOOL bNull = TRUE);
	void SetParamNull(int nIndex, BOOL bNull = TRUE);

	// locking control during Edit
	enum LockMode
	{
		optimistic,
		pessimistic,
	};
	void SetLockingMode(UINT nMode);

	// Recordset operations
	virtual BOOL Requery();         // Re-execute query based on new params

	// Cancel asynchronous operation
	void Cancel();

// Overridables
public:
	// Get default connect string
	virtual CString GetDefaultConnect();

	// Get SQL to execute
	virtual CString GetDefaultSQL();

	// set special options
	virtual void OnSetOptions(HSTMT hstmt);

	// for recordset field exchange
	virtual void DoFieldExchange(CFieldExchange* pFX);
	virtual void DoBulkFieldExchange(CFieldExchange* pFX);

// Implementation
public:
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif //_DEBUG

	virtual BOOL Check(RETCODE nRetCode) const; // general error check

	void InitRecord();
	void ResetCursor();
	void CheckRowsetCurrencyStatus(UWORD wFetchType, long nRows);
	RETCODE FetchData(UWORD wFetchType, SDWORD nRow,
		DWORD* pdwRowsFetched);
	void SkipDeletedRecords(UWORD wFetchType, long nRows,
		DWORD* pdwRowsFetched, RETCODE* pnRetCode);
	virtual void SetRowsetCurrencyStatus(RETCODE nRetCode,
		UWORD wFetchType, long nRows, DWORD dwRowsFetched);

	virtual void PreBindFields();   // called before data fields are bound
	UINT m_nFields;         // number of RFX fields
	UINT m_nParams;         // number of RFX params
	BOOL m_bCheckCacheForDirtyFields;   // switch for dirty field checking
	BOOL m_bRebindParams;     // date or UNICODE text parameter existence flag
	BOOL m_bLongBinaryColumns;  // long binary column existence flag
	BOOL m_bUseUpdateSQL;   // uses SQL-based updates
	DWORD m_dwOptions;          // archive dwOptions on Open
	SWORD m_nResultCols;    // number of columns in result set
	BOOL m_bUseODBCCursorLib;   // uses ODBC cursor lib if m_pDatabase not Open
	CODBCFieldInfo* m_rgODBCFieldInfos; // Array of field info structs with ODBC meta-data
	CFieldInfo* m_rgFieldInfos;         // Array of field info structs with MFC specific field data
	CMapPtrToPtr m_mapFieldIndex;       // Map of member address to field index
	CMapPtrToPtr m_mapParamIndex;       // Map of member address to field index

	BOOL IsSQLUpdatable(LPCTSTR lpszSQL);
	BOOL IsSelectQueryUpdatable(LPCTSTR lpszSQL);
	static BOOL PASCAL IsJoin(LPCTSTR lpszJoinClause);
	static LPCTSTR PASCAL FindSQLToken(LPCTSTR lpszSQL, LPCTSTR lpszSQLToken);

	// RFX Operations on fields of CRecordset
	UINT BindParams(HSTMT hstmt);
	void RebindParams(HSTMT hstmt);
	UINT BindFieldsToColumns();
	void BindFieldsForUpdate();
	void UnbindFieldsForUpdate();
	void Fixups();
	UINT AppendNames(CString* pstr, LPCTSTR szSeparator);
	UINT AppendValues(HSTMT hstmt, CString* pstr, LPCTSTR szSeparator);
	UINT AppendNamesValues(HSTMT hstmt, CString* pstr, LPCTSTR szSeparator);
	void StoreFields();
	void LoadFields();
	void MarkForAddNew();
	void MarkForUpdate();
	void AllocDataCache();
	void FreeDataCache();
#ifdef _DEBUG
	void DumpFields(CDumpContext& dc) const;
#endif //_DEBUG

	// RFX operation helper functions
	virtual void ThrowDBException(RETCODE nRetCode, HSTMT hstmt = SQL_NULL_HSTMT);

	int GetBoundFieldIndex(void* pv);
	int GetBoundParamIndex(void* pv);
	short GetFieldIndexByName(LPCTSTR lpszFieldName);

	void AllocStatusArrays();
	long* GetFieldLengthBuffer(DWORD nField, int nFieldType);   // for fields & params

	BYTE GetFieldStatus(DWORD nField);
	void SetFieldStatus(DWORD nField, BYTE bFlags);
	void ClearFieldStatus();

	BOOL IsFieldStatusDirty(DWORD nField) const;
	void SetDirtyFieldStatus(DWORD nField);
	void ClearDirtyFieldStatus(DWORD nField);

	BOOL IsFieldStatusNull(DWORD nField) const;
	void SetNullFieldStatus(DWORD nField);
	void ClearNullFieldStatus(DWORD nField);

	BOOL IsParamStatusNull(DWORD nField) const;
	void SetNullParamStatus(DWORD nField);
	void ClearNullParamStatus(DWORD nField);

	BOOL IsFieldNullable(DWORD nField) const;

	void** m_pvFieldProxy;
	void** m_pvParamProxy;
	UINT m_nProxyFields;
	UINT m_nProxyParams;

	// GetFieldValue helpers
	static short PASCAL GetDefaultFieldType(short nSQLType);
	static void* PASCAL GetDataBuffer(CDBVariant& varValue, short nFieldType,
		int* pnLen, short nSQLType, UDWORD nPrecision);
	static int PASCAL GetTextLen(short nSQLType, UDWORD nPrecision);
	static long PASCAL GetData(CDatabase* pdb, HSTMT hstmt, short nFieldIndex,
		short nFieldType, LPVOID pvData, int nLen, short nSQLType);
	static void PASCAL GetLongBinaryDataAndCleanup(CDatabase* pdb, HSTMT hstmt,
		short nFieldIndex, long nActualSize, LPVOID* ppvData, int nLen,
		CDBVariant& varValue, short nSQLType);
	static void PASCAL GetLongCharDataAndCleanup(CDatabase* pdb, HSTMT hstmt,
		short nFieldIndex, long nActualSize, LPVOID* ppvData, int nLen,
		CString& strValue, short nSQLType);

protected:
	UINT m_nOpenType;
	UINT m_nDefaultType;
	enum EditMode
	{
		noMode,
		edit,
		addnew
	};
	long m_lOpen;
	UINT m_nEditMode;
	BOOL m_bEOFSeen;
	long m_lRecordCount;
	long m_lCurrentRecord;
	CString m_strCursorName;
	// Perform operation based on m_nEditMode
	BOOL UpdateInsertDelete();
	BOOL m_nLockMode;       // Control concurrency for Edit()
	UDWORD m_dwDriverConcurrency;   // driver supported concurrency types
	UDWORD m_dwConcurrency; // requested concurrency type
	UWORD* m_rgRowStatus;     // row status used by SQLExtendedFetch and SQLSetPos
	DWORD m_dwRowsFetched;  // number of rows fetched by SQLExtendedFetch
	HSTMT m_hstmtUpdate;
	BOOL m_bRecordsetDb;
	BOOL m_bBOF;
	BOOL m_bEOF;
	BOOL m_bUpdatable;      // Is recordset updatable?
	BOOL m_bAppendable;
	CString m_strSQL;       // SQL statement for recordset
	CString m_strUpdateSQL; // SQL statement for updates
	CString m_strTableName;     // source table of recordset
	BOOL m_bScrollable; // supports MovePrev
	BOOL m_bDeleted;
	int m_nFieldsBound;
	BYTE* m_pbFieldFlags;
	BYTE* m_pbParamFlags;
	LONG* m_plParamLength;
	DWORD m_dwInitialGetDataLen;    // Initial GetFieldValue alloc size for long data
	DWORD m_dwRowsetSize;
	DWORD m_dwAllocatedRowsetSize;

protected:
	CString m_strRequerySQL;    // archive SQL string for use in Requery()
	CString m_strRequeryFilter; // archive filter string for use in Requery()
	CString m_strRequerySort;   // archive sort string for use in Requery()

	void SetState(int nOpenType, LPCTSTR lpszSQL, DWORD dwOptions);
	BOOL AllocHstmt();
	void BuildSQL(LPCTSTR lpszSQL);
	void PrepareAndExecute();

	void BuildSelectSQL();
	void AppendFilterAndSortSQL();
	BOOL IsRecordsetUpdatable();

	void VerifyDriverBehavior();
	DWORD VerifyCursorSupport();
	void EnableBookmarks();
	void SetUpdateMethod();
	void SetConcurrencyAndCursorType(HSTMT hstmt, DWORD dwScrollOptions);
	void AllocAndCacheFieldInfo();
	void AllocRowset();
	void FreeRowset();

	void ExecuteSetPosUpdate();
	void PrepareUpdateHstmt();
	void BuildUpdateSQL();
	void ExecuteUpdateSQL();
	void SendLongBinaryData(HSTMT hstmt);
	virtual long GetLBFetchSize(long lOldSize);     // CLongBinary fetch chunking
	virtual long GetLBReallocSize(long lOldSize);   // CLongBinary realloc chunking

	friend class CFieldExchange;
	friend class CRecordView;
};

/////////////////////////////////////////////////////////////////////////////
// Info helper definitions
#define AFX_CURRENT_RECORD_UNDEFINED (-2)
#define AFX_CURRENT_RECORD_BOF (-1)

// For returning status for a recordset
struct CRecordsetStatus
{
	long m_lCurrentRecord;  // -2=Unknown,-1=BOF,0=1st record. . .
	BOOL m_bRecordCountFinal;// Have we counted all records?
};

// Must maintian data binding info
struct CFieldInfo
{
	// MFC specific info
	void* m_pvDataCache;
	long m_nLength;
	int m_nDataType;
	BYTE m_bStatus;
#ifdef _DEBUG
	void* m_pvBindAddress;
#endif
};

struct CODBCFieldInfo
{
	// meta data from ODBC
	CString m_strName;
	SWORD m_nSQLType;
	UDWORD m_nPrecision;
	SWORD m_nScale;
	SWORD m_nNullability;
};

struct CODBCParamInfo
{
	// meta data from ODBC
	SWORD m_nSQLType;
	UDWORD m_nPrecision;
	SWORD m_nScale;
	SWORD m_nNullability;
};


/////////////////////////////////////////////////////////////////////////////
// CDBVariant

#define DBVT_NULL       0
#define DBVT_BOOL       1
#define DBVT_UCHAR      2
#define DBVT_SHORT      3
#define DBVT_LONG       4
#define DBVT_SINGLE     5
#define DBVT_DOUBLE     6
#define DBVT_DATE       7
#define DBVT_STRING     8
#define DBVT_BINARY     9

class CDBVariant
{
// Constructor
public:
	CDBVariant();

// Attributes
public:
	DWORD m_dwType;

	union
	{
	  BOOL              m_boolVal;
	  unsigned char     m_chVal;
	  short             m_iVal;
	  long              m_lVal;
	  float             m_fltVal;
	  double            m_dblVal;
	  TIMESTAMP_STRUCT* m_pdate;
	  CString*          m_pstring;
	  CLongBinary*      m_pbinary;
	};

// Operations
	void Clear();

// Implementation
public:
	virtual ~CDBVariant();
};

/////////////////////////////////////////////////////////////////////////////
// CRecordView - form for viewing data records

#ifdef _AFXDLL
class CRecordView : public CFormView
#else
class AFX_NOVTABLE CRecordView : public CFormView
#endif
{
	DECLARE_DYNAMIC(CRecordView)

// Construction
protected:  // must derive your own class
	CRecordView(LPCTSTR lpszTemplateName);
	CRecordView(UINT nIDTemplate);

// Attributes
public:
	virtual CRecordset* OnGetRecordset() = 0;

	BOOL IsOnLastRecord();
	BOOL IsOnFirstRecord();

// Operations
public:
	virtual BOOL OnMove(UINT nIDMoveCommand);

// Implementation
public:
	virtual ~CRecordView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual void OnInitialUpdate();

protected:
	BOOL m_bOnFirstRecord;
	BOOL m_bOnLastRecord;

	//{{AFX_MSG(CRecordView)
	afx_msg void OnUpdateRecordFirst(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordPrev(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordNext(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordLast(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnMove(int cx, int cy);

private: BOOL _CallOnMove(UINT _x) {return OnMove(_x); } // __BORLANDC__ MS-BUG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXDBCORE_INLINE AFX_INLINE
#define _AFXDBRFX_INLINE AFX_INLINE
#define _AFXDBVIEW_INLINE AFX_INLINE
#include <afxdb.inl>
#undef _AFXDBVIEW_INLINE
#undef _AFXDBCORE_INLINE
#undef _AFXDBRFX_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXDB_H__

/////////////////////////////////////////////////////////////////////////////
