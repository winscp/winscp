// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXDAO_H
#define __AFXDAO_H

#ifdef _AFX_NO_DAO_SUPPORT
	#error DAO Database classes not supported in this library variant.
#endif

#ifndef __AFXDISP_H__
	#include <afxdisp.h>    // Must include this before dao headers
#endif
#ifndef _DBDAOINT_H_
	#include <dbdaoint.h>
#endif
#ifndef _DAOGETRW_H_
	#include <daogetrw.h>
#endif
#ifndef _DBDAOID_H_
	#include <dbdaoid.h>
#endif
#ifndef _DBDAOERR_H_
	#include <dbdaoerr.h>
#endif

#ifndef __AFXDB__H__
	#include <afxdb_.h> // shared header with ODBC database classes
#endif

#ifndef __AFXEXT_H__
	#include <afxext.h> // for CFormView
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
			#pragma comment(lib, "mfco42d.lib")
			#pragma comment(lib, "mfcd42d.lib")
		#else
			#pragma comment(lib, "mfco42ud.lib")
			#pragma comment(lib, "mfcd42ud.lib")
		#endif
	#endif

#endif

#pragma comment(lib, "daouuid.lib")

#endif //!_AFX_NOFORCE_LIBS

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

////////////////////////////////////////////////////////////////////////
// AFXDAO - MFC Database support using DAO

// Classes declared in this file

	// CException
		class CDaoException;    // DAO error/exception handling

	// CObject
		class CDaoRecordView;
		class CDaoWorkspace;    // DAO engine/transaction/security manager
		class CDaoDatabase;     // DAO database manager
		class CDaoRecordset;    // DAO result set manager
		class CDaoTableDef;     // DAO base table manager
		class CDaoQueryDef;     // DAO query manager

	// Non-CObject classes
		class CDaoFieldExchange;
		struct CDaoFieldCache;
		struct CDaoErrorInfo;
		struct CDaoWorkspaceInfo;
		struct CDaoDatabaseInfo;
		struct CDaoTableDefInfo;
		struct CDaoFieldInfo;
		struct CDaoIndexInfo;
		struct CDaoRelationInfo;
		struct CDaoQueryDefInfo;
		struct CDaoParameterInfo;

/////////////////////////////////////////////////////////////////////////////
// AFXDLL support

#undef AFX_DATA
#define AFX_DATA AFX_DB_DATA

////////////////////////////////////////////////////////////////////////
// Data caching structures
struct CDaoFieldCache
{
	void* m_pvData;     // Pointer to cached data of any supported type.
	BYTE m_nStatus;     // (NULL) status cache.
	BYTE m_nDataType;       // Type of data cached.
};

////////////////////////////////////////////////////////////////////////
// Info structures

struct CDaoErrorInfo
{
// Attributes
	long m_lErrorCode;
	CString m_strSource;
	CString m_strDescription;
	CString m_strHelpFile;
	long m_lHelpContext;

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoWorkspaceInfo
{
// Attributes
	CString m_strName;              // Primary
	CString m_strUserName;          // Secondary
	BOOL m_bIsolateODBCTrans;       // All

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoDatabaseInfo
{
// Attributes
	CString m_strName;              // Primary
	BOOL m_bUpdatable;              // Primary
	BOOL m_bTransactions;           // Primary
	CString m_strVersion;           // Secondary
	long m_lCollatingOrder;         // Secondary
	short m_nQueryTimeout;          // Secondary
	CString m_strConnect;           // All

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoTableDefInfo
{
// Attributes
	CString m_strName;              // Primary
	BOOL m_bUpdatable;              // Primary
	long m_lAttributes;             // Primary
	COleDateTime m_dateCreated;     // Secondary
	COleDateTime m_dateLastUpdated; // Secondary
	CString m_strSrcTableName;      // Secondary
	CString m_strConnect;           // Secondary
	CString m_strValidationRule;    // All
	CString m_strValidationText;    // All
	long m_lRecordCount;            // All

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoFieldInfo
{
// Attributes
	CString m_strName;              // Primary
	short m_nType;                  // Primary
	long m_lSize;                   // Primary
	long m_lAttributes;             // Primary
	short m_nOrdinalPosition;       // Secondary
	BOOL m_bRequired;               // Secondary
	BOOL m_bAllowZeroLength;        // Secondary
	long m_lCollatingOrder;         // Secondary
	CString m_strForeignName;       // Secondary
	CString m_strSourceField;       // Secondary
	CString m_strSourceTable;       // Secondary
	CString m_strValidationRule;    // All
	CString m_strValidationText;    // All
	CString m_strDefaultValue;      // All

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoIndexFieldInfo
{
// Attributes
	CString m_strName;              // Primary
	BOOL m_bDescending;             // Primary

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoIndexInfo
{
// Constructors
	CDaoIndexInfo();

// Attributes
	CString m_strName;                      // Primary
	CDaoIndexFieldInfo* m_pFieldInfos;      // Primary
	short m_nFields;                        // Primary
	BOOL m_bPrimary;                        // Secondary
	BOOL m_bUnique;                         // Secondary
	BOOL m_bClustered;                      // Secondary
	BOOL m_bIgnoreNulls;                    // Secondary
	BOOL m_bRequired;                       // Secondary
	BOOL m_bForeign;                        // Secondary
	long m_lDistinctCount;                  // All

// Implementation
	virtual ~CDaoIndexInfo();
	BOOL m_bCleanupFieldInfo;

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoRelationFieldInfo
{
// Attributes
	CString m_strName;              // Primary
	CString m_strForeignName;       // Primary

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoRelationInfo
{
// Constructor
	CDaoRelationInfo();

// Attributes
	CString m_strName;              // Primary
	CString m_strTable;             // Primary
	CString m_strForeignTable;      // Primary
	long m_lAttributes;             // Secondary
	CDaoRelationFieldInfo* m_pFieldInfos;   // Secondary
	short m_nFields;                // Secondary

// Implementation
	virtual ~CDaoRelationInfo();
	BOOL m_bCleanupFieldInfo;

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoQueryDefInfo
{
// Attributes
	CString m_strName;              // Primary
	short m_nType;                  // Primary
	COleDateTime m_dateCreated;     // Secondary
	COleDateTime m_dateLastUpdated; // Secondary
	BOOL m_bUpdatable;              // Secondary
	BOOL m_bReturnsRecords;         // Secondary
	CString m_strSQL;               // All
	CString m_strConnect;           // All
	short m_nODBCTimeout;           // See readme

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

struct CDaoParameterInfo
{
// Attributes
	CString m_strName;              // Primary
	short m_nType;                  // Primary
	COleVariant m_varValue;         // Secondary

#ifdef _DEBUG
	virtual void Dump(CDumpContext& dc) const;
#endif
};

////////////////////////////////////////////////////////////////////////
// DAO Helpers
//

// Flags for getting and/or setting object properties
#define AFX_DAO_PRIMARY_INFO    0x00000001  // Get only primary
#define AFX_DAO_SECONDARY_INFO  0x00000002  // Get primary & secondary
#define AFX_DAO_ALL_INFO        0x00000004  // Get All info

// Jet engine TRUE/FALSE definitions
#define AFX_DAO_TRUE                    (-1L)
#define AFX_DAO_FALSE                   0

// Set CDaoRecordset::Open option to use m_nDefaultType
#define AFX_DAO_USE_DEFAULT_TYPE        (-1L)

// Flags used for Move/Find
#define AFX_DAO_NEXT                    (+1L)
#define AFX_DAO_PREV                    (-1L)
#define AFX_DAO_FIRST                   LONG_MIN
#define AFX_DAO_LAST                    LONG_MAX

// Default sizes for DFX function PreAlloc sizes
#define AFX_DAO_TEXT_DEFAULT_SIZE       255
#define AFX_DAO_BINARY_DEFAULT_SIZE     2048
#define AFX_DAO_LONGBINARY_DEFAULT_SIZE 32768

// Flag used for DFX functions dwBindOptions bitmask
#define AFX_DAO_ENABLE_FIELD_CACHE      0x01
#define AFX_DAO_DISABLE_FIELD_CACHE     0
#define AFX_DAO_CACHE_BY_VALUE          0x80    // MFC Internal

// Field Flags, used to indicate status of fields
#define AFX_DAO_FIELD_FLAG_DIRTY            0x01
#define AFX_DAO_FIELD_FLAG_NULL             0x02
#define AFX_DAO_FIELD_FLAG_NULLABLE_KNOWN   0x04
#define AFX_DAO_FIELD_FLAG_NULLABLE         0x08

// Extended error codes
#define NO_AFX_DAO_ERROR                        0
#define AFX_DAO_ERROR_MIN                       2000
#define AFX_DAO_ERROR_ENGINE_INITIALIZATION     AFX_DAO_ERROR_MIN + 0
#define AFX_DAO_ERROR_DFX_BIND                  AFX_DAO_ERROR_MIN + 1
#define AFX_DAO_ERROR_OBJECT_NOT_OPEN           AFX_DAO_ERROR_MIN + 2
#define AFX_DAO_ERROR_MAX                       AFX_DAO_ERROR_MIN + 2

// Object status flags
#define AFX_DAO_IMPLICIT_WS                     0x01
#define AFX_DAO_IMPLICIT_DB                     0x02
#define AFX_DAO_IMPLICIT_QD                     0x04
#define AFX_DAO_IMPLICIT_TD                     0x08
#define AFX_DAO_IMPLICIT_CLOSE                  0x40
#define AFX_DAO_DEFAULT_WS                      0x80

// CDaoRecordView status flags
#define AFX_DAOVIEW_SCROLL_NEXT                 0x01
#define AFX_DAOVIEW_SCROLL_LAST                 0x02
#define AFX_DAOVIEW_SCROLL_BACKWARD             0x04

// Logging helpers
void AFXAPI AfxDaoCheck(SCODE scode, LPCSTR lpszDaoCall,
	LPCSTR lpszFile, int nLine, int nError = NO_AFX_DAO_ERROR,
	BOOL bMemOnly = FALSE);

#ifdef _DEBUG
void AFXAPI AfxDaoTrace(SCODE scode, LPCSTR lpszDaoCall,
	LPCSTR lpszFile, int nLine);
#endif

#ifdef _DEBUG
#define DAO_CHECK(f)            AfxDaoCheck(f, #f, THIS_FILE, __LINE__)
#define DAO_CHECK_ERROR(f, err) AfxDaoCheck(f, #f, THIS_FILE, __LINE__, err)
#define DAO_CHECK_MEM(f)        AfxDaoCheck(f, #f, THIS_FILE, __LINE__, \
									NO_AFX_DAO_ERROR, TRUE)
#define DAO_TRACE(f)            AfxDaoTrace(f, #f, THIS_FILE, __LINE__)
#else
#define DAO_CHECK(f)            AfxDaoCheck(f, NULL, NULL, 0)
#define DAO_CHECK_ERROR(f, err) AfxDaoCheck(f, NULL, NULL, 0, err)
#define DAO_CHECK_MEM(f)        AfxDaoCheck(f, NULL, NULL, 0, \
									NO_AFX_DAO_ERROR, TRUE)
#define DAO_TRACE(f)            f
#endif

/////////////////////////////////////////////////////////////////////////////
// CDaoFieldExchange - for field exchange
class CDaoFieldExchange
{
// Attributes
public:
	enum DFX_Operation
	{
		AddToParameterList,     // builds PARAMETERS clause
		AddToSelectList,        // builds SELECT clause
		BindField,              // sets up binding structure
		BindParam,              // sets parameter values
		Fixup,                  // sets NULL status
		AllocCache,             // allocates cache used for dirty check
		StoreField,             // saves current record to cache
		LoadField,              // restores cached data to member vars
		FreeCache,              // frees cache
		SetFieldNull,           // sets field status & value to NULL
		MarkForAddNew,          // marks fields dirty if not PSEUDO NULL
		MarkForEdit,            // marks fields dirty if don't match cache
		SetDirtyField,          // sets field values marked as dirty
#ifdef _DEBUG
		DumpField,
#endif
		MaxDFXOperation,        // dummy operation type for input checking
	};

	UINT m_nOperation;          // type of exchange operation
	CDaoRecordset* m_prs;       // recordset handle

// Operations
public:
	enum FieldType
	{
		none,
		outputColumn,
		param,
	};

	void SetFieldType(UINT nFieldType);
	BOOL IsValidOperation();

// Implementation
public:
	CDaoFieldExchange(UINT nOperation, CDaoRecordset* prs,
		void* pvField = NULL);

	void Default(LPCTSTR lpszName, void* pv, DWORD dwFieldType,
		DWORD dwBindOptions = 0);

	static void PASCAL AppendParamType(CString& strParamList, DWORD dwParamType);
	static CDaoFieldCache* PASCAL GetCacheValue(CDaoRecordset* prs, void* pv);
	static void PASCAL SetNullValue(void* pv, DWORD dwDataType);
	static BOOL PASCAL IsNullValue(void* pv, DWORD dwDataType);
	static void PASCAL AllocCacheValue(CDaoFieldCache*& pCache, DWORD dwDataType);
	static void PASCAL DeleteCacheValue(CDaoFieldCache* pCache, DWORD dwDataType);
	static void PASCAL CopyValue(void* pvSrc, void* pvDest, DWORD dwDataType);
	static BOOL PASCAL CompareValue(void* pvSrc, void* pvDest, DWORD dwDataType);
	static void PASCAL FillVariant(void* pvValue, DWORD dwDataType, COleVariant** ppVar);

	// Current type of field
	UINT m_nFieldType;
	void* m_pvField;
	UINT m_nField;
	UINT m_nParam;
	UINT m_nFieldFound;

#ifdef _DEBUG
	CDumpContext* m_pdcDump;
#endif //_DEBUG
};

/////////////////////////////////////////////////////////////////////////////
// Standard RecordSet Field Exchange routines

// variable length data
void AFXAPI DFX_Text(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CString& value, int nPreAllocSize = AFX_DAO_TEXT_DEFAULT_SIZE,
	DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Binary(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CByteArray& value, int nPreAllocSize = AFX_DAO_BINARY_DEFAULT_SIZE,
	DWORD dwBindOptions = 0);
void AFXAPI DFX_LongBinary(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CLongBinary& value, DWORD dwPreAllocSize = AFX_DAO_LONGBINARY_DEFAULT_SIZE,
	DWORD dwBndOptions = 0);

//fixed length data
void AFXAPI DFX_Bool(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	BOOL& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Byte(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	BYTE& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Short(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	short& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Long(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	long& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Currency(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	COleCurrency& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Single(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	float& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_Double(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	double& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);
void AFXAPI DFX_DateTime(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	COleDateTime& value, DWORD dwBindOptions = AFX_DAO_ENABLE_FIELD_CACHE);

//////////////////////////////////////////////////////////////////////////
// Database Dialog Data Exchange cover routines
// Cover routines provide database semantics on top of DDX routines

// simple text operations
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, BOOL& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, BYTE& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, short& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, long& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, COleCurrency& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, DWORD& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, float& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, double& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, COleDateTime& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, LPTSTR pstrValue,
	int nMaxLen, CDaoRecordset* pRecordset);

// special control types
void AFXAPI DDX_FieldCheck(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldRadio(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldLBString(CDataExchange* pDX, int nIDC,
	CString& value, CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldCBString(CDataExchange* pDX, int nIDC,
	CString& value, CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldLBStringExact(CDataExchange* pDX, int nIDC,
	CString& value, CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldCBStringExact(CDataExchange* pDX, int nIDC,
	CString& value, CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldLBIndex(CDataExchange* pDX, int nIDC, int& index,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldCBIndex(CDataExchange* pDX, int nIDC, int& index,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldLBStringExact(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldCBStringExact(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldScroll(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset);
void AFXAPI DDX_FieldSlider(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset);

////////////////////////////////////////////////////////////////////////
// CDaoWorkspace - a DAO Workspace

class CDaoWorkspace : public CObject
{
	DECLARE_DYNAMIC(CDaoWorkspace)

// Constructors
public:
	CDaoWorkspace();

	virtual void Create(LPCTSTR lpszName, LPCTSTR lpszUserName,
		LPCTSTR lpszPassword);
	virtual void Append();

	virtual void Open(LPCTSTR lpszName = NULL);
	virtual void Close();

// Attributes
public:
	DAOWorkspace* m_pDAOWorkspace;

	static CString PASCAL GetVersion();
	static CString PASCAL GetIniPath();
	static void PASCAL SetIniPath(LPCTSTR lpszRegistrySubKey);
	static void PASCAL SetDefaultUser(LPCTSTR lpszDefaultUser);
	static void PASCAL SetDefaultPassword(LPCTSTR lpszPassword);
	static short PASCAL GetLoginTimeout();
	static void PASCAL SetLoginTimeout(short nSeconds);

	CString GetName();
	CString GetUserName();
	void SetIsolateODBCTrans(BOOL bIsolateODBCTrans);
	BOOL GetIsolateODBCTrans();

	BOOL IsOpen() const;

// Operations
public:
	void BeginTrans();
	void CommitTrans();
	void Rollback();

	static void PASCAL CompactDatabase(LPCTSTR lpszSrcName,
		LPCTSTR lpszDestName, LPCTSTR lpszLocale = dbLangGeneral,
		int nOptions = 0);
	// Password parameter added late in dev cycle, new interface req'd
	static void PASCAL CompactDatabase(LPCTSTR lpszSrcName,
		LPCTSTR lpszDestName, LPCTSTR lpszLocale, int nOptions,
		LPCTSTR lpszPassword);
	static void PASCAL RepairDatabase(LPCTSTR lpszName);

	static void PASCAL Idle(int nAction = dbFreeLocks);

	short GetWorkspaceCount();
	void GetWorkspaceInfo(int nIndex, CDaoWorkspaceInfo& wkspcinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetWorkspaceInfo(LPCTSTR lpszName, CDaoWorkspaceInfo& wkspcinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetDatabaseCount();
	void GetDatabaseInfo(int nIndex, CDaoDatabaseInfo& dbinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetDatabaseInfo(LPCTSTR lpszName, CDaoDatabaseInfo& dbinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

// Overridables
public:

// Implementation
public:
	virtual ~CDaoWorkspace();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	DAOWorkspaces* m_pDAOWorkspaces;
	DAODatabases* m_pDAODatabases;
	int m_nStatus;

	CMapPtrToPtr m_mapDatabases;        // Map of all Open CDaoDatabases
	BOOL IsNew() const;

protected:
	BOOL m_bOpen;
	BOOL m_bNew;

	static void AFX_CDECL InitializeEngine();

	void InitWorkspacesCollection();
	void FillWorkspaceInfo(DAOWorkspace* pDAOWorkspace,
		CDaoWorkspaceInfo& wsinfo, DWORD dwOptions);

	void InitDatabasesCollection();
	void FillDatabaseInfo(DAODatabase* pDAODatabase,
		CDaoDatabaseInfo& dbinfo, DWORD dwOptions);

	virtual void ThrowDaoException(int nError = NO_AFX_DAO_ERROR);
};

////////////////////////////////////////////////////////////////////////
// CDaoException - DAO error trapping mechanism
class CDaoException : public CException
{
	DECLARE_DYNAMIC(CDaoException)

// Constructors
public:
	CDaoException();

// Attributes
public:
	CDaoErrorInfo* m_pErrorInfo;

	SCODE m_scode;
	int m_nAfxDaoError;     // DAO class extended error code

// Operations
public:
	short GetErrorCount();
	void GetErrorInfo(int nIndex);

// Implementation
public:
	virtual ~CDaoException();

	DAOError* m_pDAOError;
	DAOErrors* m_pDAOErrors;

	virtual BOOL GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext = NULL);

protected:
	void InitErrorsCollection();
	void FillErrorInfo();
};

void AFXAPI AfxThrowDaoException(int nAfxDaoError = NO_AFX_DAO_ERROR,
	SCODE scode = S_OK);


////////////////////////////////////////////////////////////////////////
// CDaoDatabase - a DAO Database

class CDaoDatabase : public CObject
{
	DECLARE_DYNAMIC(CDaoDatabase)

// Constructors
public:
	CDaoDatabase(CDaoWorkspace* pWorkspace = NULL);

	virtual void Create(LPCTSTR lpszName,
		LPCTSTR lpszLocale = dbLangGeneral, int dwOptions = 0);

	virtual void Open(LPCTSTR lpszName, BOOL bExclusive = FALSE,
		BOOL bReadOnly = FALSE, LPCTSTR lpszConnect = _T(""));
	virtual void Close();

// Attributes
public:
	CDaoWorkspace* m_pWorkspace;
	DAODatabase* m_pDAODatabase;

	BOOL IsOpen() const;

	BOOL CanUpdate();
	BOOL CanTransact();

	CString GetName();
	CString GetConnect();

	CString GetVersion();
	short GetQueryTimeout();
	void SetQueryTimeout(short nSeconds);
	long GetRecordsAffected();

// Operations
public:
	void Execute(LPCTSTR lpszSQL, int nOptions = dbFailOnError);

	void DeleteTableDef(LPCTSTR lpszName);
	void DeleteQueryDef(LPCTSTR lpszName);

	void CreateRelation(LPCTSTR lpszName, LPCTSTR lpszTable,
		LPCTSTR lpszForeignTable, long lAttributes,
		LPCTSTR lpszField, LPCTSTR lpszForeignField);
	void CreateRelation(CDaoRelationInfo& relinfo);
	void DeleteRelation(LPCTSTR lpszName);

	short GetTableDefCount();
	void GetTableDefInfo(int nIndex, CDaoTableDefInfo& tabledefinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetTableDefInfo(LPCTSTR lpszName, CDaoTableDefInfo& tabledefinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetRelationCount();
	void GetRelationInfo(int nIndex, CDaoRelationInfo& relinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetRelationInfo(LPCTSTR lpszName, CDaoRelationInfo& relinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetQueryDefCount();
	void GetQueryDefInfo(int nIndex, CDaoQueryDefInfo& querydefinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetQueryDefInfo(LPCTSTR lpszName, CDaoQueryDefInfo& querydefinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

// Overridables
public:

// Implementation
public:
	virtual ~CDaoDatabase();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	DAOTableDefs* m_pDAOTableDefs;
	DAORelations* m_pDAORelations;
	DAOQueryDefs* m_pDAOQueryDefs;
	DAORecordsets* m_pDAORecordsets;
	int m_nStatus;

	CMapPtrToPtr m_mapTableDefs;        // Map of all Open CDaoTableDefs
	CMapPtrToPtr m_mapQueryDefs;        // Map of all Open CDaoQueryDefs
	CMapPtrToPtr m_mapRecordsets;       // Map of all Open CDaoRecordsets

protected:
	BOOL m_bOpen;

	void InitWorkspace();
	void InitTableDefsCollection();
	void FillTableDefInfo(DAOTableDef* pDAOTableDef,
		CDaoTableDefInfo& tabledefinfo, DWORD dwOptions);
	void InitRelationsCollection();
	void FillRelationInfo(DAORelation* pDAORelation,
		CDaoRelationInfo& relinfo, DWORD dwOptions);
	void InitQueryDefsCollection();
	void FillQueryDefInfo(DAOQueryDef* pDAOQueryDef,
		CDaoQueryDefInfo& querydefinfo, DWORD dwOptions);

	virtual void ThrowDaoException(int nError = NO_AFX_DAO_ERROR);
};


////////////////////////////////////////////////////////////////////////
// CDaoTableDef - a DAO TableDef

class CDaoTableDef : public CObject
{
	DECLARE_DYNAMIC(CDaoTableDef)

// Constructors
public:
	CDaoTableDef(CDaoDatabase* pDatabase);

	virtual void Create(LPCTSTR lpszName, long lAttributes = 0,
		LPCTSTR lpszSrcTable = NULL, LPCTSTR lpszConnect = NULL);
	virtual void Append();

	virtual void Open(LPCTSTR lpszName);
	virtual void Close();

// Attributes
public:
	CDaoDatabase* m_pDatabase;
	DAOTableDef* m_pDAOTableDef;

	BOOL IsOpen() const;
	BOOL CanUpdate();

	void SetName(LPCTSTR lpszName);
	CString GetName();
	void SetSourceTableName(LPCTSTR lpszSrcTableName);
	CString GetSourceTableName();
	void SetConnect(LPCTSTR lpszConnect);
	CString GetConnect();
	void SetAttributes(long lAttributes);
	long GetAttributes();
	COleDateTime GetDateCreated();
	COleDateTime GetDateLastUpdated();
	void SetValidationRule(LPCTSTR lpszValidationRule);
	CString GetValidationRule();
	void SetValidationText(LPCTSTR lpszValidationText);
	CString GetValidationText();
	long GetRecordCount();

// Overridables
public:

// Operations
public:
	void CreateField(LPCTSTR lpszName, short nType, long lSize,
		long lAttributes = 0);
	void CreateField(CDaoFieldInfo& fieldinfo);
	void DeleteField(LPCTSTR lpszName);
	void DeleteField(int nIndex);

	void CreateIndex(CDaoIndexInfo& indexinfo);
	void DeleteIndex(LPCTSTR lpszName);
	void DeleteIndex(int nIndex);

	short GetFieldCount();
	void GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetFieldInfo(LPCTSTR lpszName, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetIndexCount();
	void GetIndexInfo(int nIndex, CDaoIndexInfo& indexinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetIndexInfo(LPCTSTR lpszName, CDaoIndexInfo& indexinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	void RefreshLink();

// Implementation
public:
	~CDaoTableDef();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	DAOFields* m_pDAOFields;
	DAOIndexes* m_pDAOIndexes;

protected:
	BOOL m_bOpen;
	BOOL m_bNew;

	void InitFieldsCollection();
	void InitIndexesCollection();

	virtual void ThrowDaoException(int nError = NO_AFX_DAO_ERROR);
};


////////////////////////////////////////////////////////////////////////
// CDaoQueryDef - a DAO QueryDef

class CDaoQueryDef : public CObject
{
	DECLARE_DYNAMIC(CDaoQueryDef)

// Constructors
public:
	CDaoQueryDef(CDaoDatabase* pDatabase);

	virtual void Create(LPCTSTR lpszName = NULL,
		LPCTSTR lpszSQL = NULL);
	virtual void Append();

	virtual void Open(LPCTSTR lpszName = NULL);
	virtual void Close();

// Attributes
public:
	CDaoDatabase* m_pDatabase;
	DAOQueryDef* m_pDAOQueryDef;

	BOOL CanUpdate();

	CString GetName();
	void SetName(LPCTSTR lpszName);
	CString GetSQL();
	void SetSQL(LPCTSTR lpszSQL);
	short GetType();
	COleDateTime GetDateCreated();
	COleDateTime GetDateLastUpdated();
	CString GetConnect();
	void SetConnect(LPCTSTR lpszConnect);
	short GetODBCTimeout();
	void SetODBCTimeout(short nODBCTimeout);
	BOOL GetReturnsRecords();
	void SetReturnsRecords(BOOL bReturnsRecords);
	long GetRecordsAffected();

	BOOL IsOpen() const;

// Operations
public:
	virtual void Execute(int nOptions = dbFailOnError);

	virtual COleVariant GetParamValue(LPCTSTR lpszName);
	virtual COleVariant GetParamValue(int nIndex);
	virtual void SetParamValue(LPCTSTR lpszName,
		const COleVariant& varValue);
	virtual void SetParamValue(int nIndex,
		const COleVariant& varValue);
	void SetParamValueNull(LPCTSTR lpszName);
	void SetParamValueNull(int nIndex);

	short GetFieldCount();
	void GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetFieldInfo(LPCTSTR lpszName, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetParameterCount();
	void GetParameterInfo(int nIndex, CDaoParameterInfo& paraminfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetParameterInfo(LPCTSTR lpszName,
		CDaoParameterInfo& paraminfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

// Overridables
public:

// Implementation
public:
	~CDaoQueryDef();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	DAOFields* m_pDAOFields;
	DAOParameters* m_pDAOParameters;

protected:
	BOOL m_bOpen;
	BOOL m_bNew;

	void InitFieldsCollection();
	void InitParametersCollection();
	void FillParameterInfo(DAOParameter* pDAOParameter,
		CDaoParameterInfo& paraminfo, DWORD dwInfoOptions);

	virtual void ThrowDaoException(int nError = NO_AFX_DAO_ERROR);
};


////////////////////////////////////////////////////////////////////////
// CDaoRecordset - the result of a query or base table browse

class CDaoRecordset : public CObject
{
	DECLARE_DYNAMIC(CDaoRecordset)

// Constructor
public:
	CDaoRecordset(CDaoDatabase* pDatabase = NULL);

	virtual void Open(int nOpenType = AFX_DAO_USE_DEFAULT_TYPE,
		LPCTSTR lpszSQL = NULL, int nOptions = 0);
	virtual void Open(CDaoQueryDef* pQueryDef,
		int nOpenType = dbOpenDynaset, int nOptions = 0);
	virtual void Open(CDaoTableDef* pTableDef,
		int nOpenType = dbOpenTable, int nOptions = 0);
	virtual void Close();

// Attributes
public:
	CDaoDatabase* m_pDatabase;  // Source database for this result set
	DAORecordset* m_pDAORecordset;

	CString m_strFilter;    // Filter string used when constructing SQL
	CString m_strSort;      // Sort string used when constructing SQL

	int m_nFields;
	int m_nParams;

	BOOL m_bCheckCacheForDirtyFields;   // Switch for dirty field checking.

	BOOL CanUpdate() const;
	BOOL CanAppend() const;
	BOOL CanScroll() const;
	BOOL CanRestart();
	BOOL CanTransact();
	BOOL CanBookmark();

	BOOL IsOpen() const;
	BOOL IsBOF() const;
	BOOL IsEOF() const;
	BOOL IsDeleted() const;

	BOOL IsFieldDirty(void* pv);
	BOOL IsFieldNull(void* pv);
	BOOL IsFieldNullable(void* pv);

	CString GetName();
	short GetType();
	short GetEditMode();
	CString GetSQL() const;

	COleDateTime GetDateCreated();
	COleDateTime GetDateLastUpdated();
	COleVariant GetLastModifiedBookmark();
	CString GetValidationRule();
	CString GetValidationText();
	CString GetCurrentIndex();
	void SetCurrentIndex(LPCTSTR lpszIndex);

	long GetRecordCount();

// Operations
public:
	// Cursor operations
	void MoveNext();
	void MovePrev();
	void MoveFirst();
	void MoveLast();
	virtual void Move(long lRows);

	BOOL FindNext(LPCTSTR lpszFilter);
	BOOL FindPrev(LPCTSTR lpszFilter);
	BOOL FindFirst(LPCTSTR lpszFilter);
	BOOL FindLast(LPCTSTR lpszFilter);
	virtual BOOL Find(long lFindType, LPCTSTR lpszFilter);

	COleVariant GetBookmark();
	void SetBookmark(COleVariant varBookmark);
	long GetAbsolutePosition();
	void SetAbsolutePosition(long lPosition);
	float GetPercentPosition();
	void SetPercentPosition(float fPosition);

	// seek allowed on recordset opened as tables (max keys = 13)
	BOOL Seek(LPCTSTR lpszComparison, COleVariant* pKey1,
		COleVariant* pKey2 = NULL, COleVariant* pKey3 = NULL);
	BOOL Seek(LPCTSTR lpszComparison, COleVariant* pKeyArray, WORD nKeys);

	// edit buffer operations
	virtual void AddNew();
	virtual void Edit();
	virtual void Update();
	virtual void Delete();
	virtual void CancelUpdate();

	// field operations
	virtual void GetFieldValue(LPCTSTR lpszName, COleVariant& varValue);
	virtual void GetFieldValue(int nIndex, COleVariant& varValue);
	virtual void SetFieldValue(LPCTSTR lpszName,
		const COleVariant& varValue);
	virtual void SetFieldValue(int nIndex,
		const COleVariant& varValue);
	void SetFieldValue(int nIndex, LPCTSTR lpszValue);
	void SetFieldValue(LPCTSTR lpszName, LPCTSTR lpszValue);
	void SetFieldValueNull(LPCTSTR lpszName);
	void SetFieldValueNull(int nIndex);

	virtual COleVariant GetParamValue(LPCTSTR lpszName);
	virtual COleVariant GetParamValue(int nIndex);
	virtual void SetParamValue(LPCTSTR lpszName,
		const COleVariant& varValue);
	virtual void SetParamValue(int nIndex,
		const COleVariant& varValue);
	void SetParamValueNull(LPCTSTR lpszName);
	void SetParamValueNull(int nIndex);

	void SetFieldDirty(void* pv, BOOL bDirty = TRUE);
	void SetFieldNull(void* pv, BOOL bNull = TRUE);

	void SetLockingMode(BOOL bPessimistic);
	BOOL GetLockingMode();

	// Recordset operations
	virtual void Requery();

	// Jet's remote data caching operations
	void SetCacheStart(COleVariant varBookmark);
	COleVariant GetCacheStart();
	void SetCacheSize(long lSize);
	long GetCacheSize();
	void FillCache(long* pSize = NULL, COleVariant* pBookmark = NULL);

	short GetFieldCount();
	void GetFieldInfo(int nIndex, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetFieldInfo(LPCTSTR lpszName, CDaoFieldInfo& fieldinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

	short GetIndexCount();
	void GetIndexInfo(int nIndex, CDaoIndexInfo& indexinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);
	void GetIndexInfo(LPCTSTR lpszName, CDaoIndexInfo& indexinfo,
		DWORD dwInfoOptions = AFX_DAO_PRIMARY_INFO);

// Overridables
public:
	virtual CString GetDefaultDBName();
	virtual CString GetDefaultSQL();

	// for recordset field exchange
	virtual void DoFieldExchange(CDaoFieldExchange* pFX);

// Implementation
public:
	virtual ~CDaoRecordset();

	// Out-of-date functions kept for backward compatability
	virtual COleVariant GetFieldValue(LPCTSTR lpszName);
	virtual COleVariant GetFieldValue(int nIndex);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	CString m_strSQL;

	CDaoQueryDef* m_pQueryDef;  // Source query for this result set
	CDaoTableDef* m_pTableDef;
	ICDAORecordset* m_pICDAORecordsetGetRows;
	DAOFields* m_pDAOFields;
	DAOIndexes* m_pDAOIndexes;

	void SetCursorAttributes();
	void GetDataAndFixupNulls();
	DWORD GetFieldLength(int nFieldIndex);

	BOOL IsFieldStatusDirty(UINT nField);
	void SetDirtyFieldStatus(UINT nField);
	void ClearDirtyFieldStatus(UINT nField);

	BOOL IsFieldStatusNull(UINT nField);
	void SetNullFieldStatus(UINT nField);
	void ClearNullFieldStatus(UINT nField);

	BOOL IsFieldStatusNullable(UINT nField);
	void SetNullableFieldStatus(UINT nField);

	BOOL IsFieldStatusNullableKnown(UINT nField);
	void SetNullableKnownFieldStatus(UINT nField);

	void ClearFieldStatusFlags();
	BOOL IsMatch();

	DWORD m_cbFixedLengthFields;
	DAOCOLUMNBINDING* m_prgDaoColBindInfo;
	DWORD* m_pulColumnLengths;
	DAOFETCHROWS m_DaoFetchRows;
	BYTE* m_pbFieldFlags;
	BYTE* m_pbParamFlags;

	CMapPtrToPtr* m_pMapFieldCache;
	CMapPtrToPtr* m_pMapFieldIndex;

	static void AFX_CDECL StripBrackets(LPCTSTR lpszSrc, LPTSTR lpszDest);

protected:
	BOOL m_bOpen;
	int m_nStatus;

	BOOL m_bAppendable;
	BOOL m_bScrollable;
	BOOL m_bDeleted;

	int m_nOpenType;
	int m_nDefaultType;
	int m_nOptions;

	CString m_strRequerySQL;
	CString m_strRequeryFilter;
	CString m_strRequerySort;

	void BuildSQL();
	void AllocDatabase();

	// RFX Operation Cover Functions
	void BuildSelectList();
	void BuildParameterList();
	void BindFields();
	void BindParameters();
	void Fixup();
	void AllocCache();
	void StoreFields();
	void LoadFields();
	void FreeCache();
	void MarkForEdit();
	void MarkForAddNew();
	int GetFieldIndex(void* pv);
	virtual void SetDirtyFields();

	void InitFieldsCollection();
	void InitIndexesCollection();

	virtual void ThrowDaoException(int nError = NO_AFX_DAO_ERROR);

	friend class CDaoFieldExchange;
	friend class CDaoRecordView;
};

/////////////////////////////////////////////////////////////////////////////
// CDaoRecordView - form for viewing data records

#ifdef _AFXDLL
class CDaoRecordView : public CFormView
#else
class AFX_NOVTABLE CDaoRecordView : public CFormView
#endif
{
	DECLARE_DYNAMIC(CDaoRecordView)

// Construction
protected:  // must derive your own class
	CDaoRecordView(LPCTSTR lpszTemplateName);
	CDaoRecordView(UINT nIDTemplate);

// Attributes
public:
	virtual CDaoRecordset* OnGetRecordset() = 0;

	BOOL IsOnLastRecord();
	BOOL IsOnFirstRecord();

// Operations
public:
	virtual BOOL OnMove(UINT nIDMoveCommand);

// Implementation
public:
	virtual ~CDaoRecordView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	virtual void OnInitialUpdate();

protected:
	int m_nStatus;
	COleVariant m_varBookmarkCurrent;
	COleVariant m_varBookmarkFirst;
	COleVariant m_varBookmarkLast;

	//{{AFX_MSG(CDaoRecordView)
	afx_msg void OnUpdateRecordFirst(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordPrev(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordNext(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRecordLast(CCmdUI* pCmdUI);
	//}}AFX_MSG
	afx_msg void OnMove(int cx, int cy);

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////
// DAODBEngine helpers - implementation specific and undocumented
void AFXAPI AfxDaoInit();
DAODBEngine* AFXAPI AfxDaoGetEngine();
void AFXAPI AfxDaoTerm();

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXDAOCORE_INLINE AFX_INLINE
#define _AFXDAODFX_INLINE AFX_INLINE
#define _AFXDAOVIEW_INLINE AFX_INLINE
#include <afxdao.inl>
#undef _AFXDAOCORE_INLINE
#undef _AFXDAODFX_INLINE
#undef _AFXDAOVIEW_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXDAO_H__

/////////////////////////////////////////////////////////////////////////////
