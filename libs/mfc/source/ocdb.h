/*=--------------------------------------------------------------------------=
 * ocdb.h
 *=--------------------------------------------------------------------------=
 * defines the interfaces and constants for use with the OLE Controls Data
 * binding interfaces.
 *
 * Copyright (c) 1994-1998 Microsoft Corporation, All Rights Reserved.
 *
 *
 *
 */
#ifndef __OCDB_H_

#undef Delete
#ifdef __cplusplus
extern "C" {
#endif

/* LPDBSTRs are MultiByte in 16bits, and Unicode in 32bits.
 */
#if defined(_WIN32) && !defined(OLE2ANSI)

#define LPDBSTR           LPWSTR
#define DBCHAR            WCHAR
#define ldbstrlen(str)    wcslen(str)
#define ldbstrcpy(a, b)   wcscpy((a), (b))
#define ldbstrcpyn(a,b,n) wcsncpy((a), (b), (n))
#define ldbstrcmp(a, b)   wcscmp((a), (b))
#define ldbstrcat(a, b)   wcscat((a), (b))
#define ldbstrcmpi(a,b)   wcsicmp((a),(b))
#define DBTEXT(quote)     L##quote

#else

#define LPDBSTR           LPSTR
#define DBCHAR            char
#define ldbstrlen(str)    lstrlen(str)
#define ldbstrcpy(a, b)   lstrcpy((a), (b))
#define ldbstrcpyn(a,b,n) lstrcpyn((a), (b), (n))
#define ldbstrcmp(a, b)   lstrcmp((a), (b))
#define ldbstrcat(a, b)   lstrcat((a), (b))
#define ldbstrcmpi(a,b)   lstrcmpi((a),(b))
#define DBTEXT(quote)     quote

#endif /* _WIN32 && !OLE2ANSI */

typedef LPDBSTR FAR *  LPLPDBSTR;


/* Many systems don't have BLOBs defined.
 */
#ifndef _tagBLOB_DEFINED
#define _tagBLOB_DEFINED
#define _BLOB_DEFINED
#define _LPBLOB_DEFINED

typedef struct tagBLOB {

	ULONG cbSize;
	BYTE *pBlobData;

} BLOB, *LPBLOB;

#endif

/*----------------------------------------------------------------------------
 *
 *  dbvar.h
 *
 *----------------------------------------------------------------------------
 */
#ifndef _DBCOLUMNID_DEFINED
#define _DBCOLUMNID_DEFINED
typedef enum tagDBCOLKIND
  {
	DBCOLKIND_GUID_NAME = 0,
	DBCOLKIND_GUID_NUMBER = 1,
		DBCOLKIND_NAME = 2
  }
DBCOLKIND;

#define GUID_NAMEONLY   {0x88c8d398,0x863c,0x101b,{0xac,0x3b,0x00,0xaa,0x00,0x44,0x77,0x3d}}
#define GUID_NUMBERONLY {0x88c8d399,0x863c,0x101b,{0xac,0x3b,0x00,0xaa,0x00,0x44,0x77,0x3d}}

typedef struct tagDBCOLUMNID
  {
  GUID guid;
  DBCOLKIND dwKind;
union
	{
	LONG lNumber;
	LPDBSTR lpdbsz;
	}
  ;
  }
DBCOLUMNID;
#endif   /* ndef _COLUMNID_DEFINED */

#ifdef OLD_DEFS
#ifndef _DBVARENUM_DEFINED
#define _DBVARENUM_DEFINED
enum DBVARENUM
  {
	DBTYPE_EMPTY = 0,
	DBTYPE_NULL = 1,
	DBTYPE_I2 = 2,
	DBTYPE_I4 = 3,
	DBTYPE_R4 = 4,
	DBTYPE_R8 = 5,
	DBTYPE_CY = 6,
	DBTYPE_DATE = 7,
	DBTYPE_BOOL = 11,
	DBTYPE_UI2 = 18,
	DBTYPE_UI4 = 19,
		DBTYPE_I8 = 20,
		DBTYPE_UI8 = 21,
	DBTYPE_HRESULT = 25,
	DBTYPE_LPSTR = 30,
	DBTYPE_LPWSTR = 31,
	DBTYPE_FILETIME = 64,
	DBTYPE_BLOB = 65,
	DBTYPE_UUID = 72,
	DBTYPE_DBEXPR = 503,
	DBTYPE_COLUMNID = 507,
	DBTYPE_BYTES = 508,
	DBTYPE_CHARS = 509,
	DBTYPE_WCHARS = 510,
	DBTYPE_ANYVARIANT = 511
  }
;
#endif   /* ndef _DBVARENUM_DEFINED */
#else
#define DBTYPE_COLUMNID 507
#endif

#define DBTYPE_EXT      0x100

typedef struct tagDBVARIANT DBVARIANT;

struct FARSTRUCT tagDBVARIANT{
	VARTYPE vt;
	unsigned short wReserved1;
	unsigned short wReserved2;
	unsigned short wReserved3;
	union {
	  unsigned char bVal;        /* VT_UI1               */
	  short    iVal;             /* VT_I2                */
	  long     lVal;             /* VT_I4                */
	  float    fltVal;           /* VT_R4                */
	  double       dblVal;           /* VT_R8                */
	  VARIANT_BOOL xbool;             /* VT_BOOL              */
	  SCODE    scode;            /* VT_ERROR             */
	  CY       cyVal;            /* VT_CY                */
	  DATE     date;             /* VT_DATE              */
	  BSTR     bstrVal;          /* VT_BSTR              */
	  IUnknown     FAR* punkVal;     /* VT_UNKNOWN           */
	  IDispatch    FAR* pdispVal;    /* VT_DISPATCH          */
	  SAFEARRAY    FAR* parray;      /* VT_ARRAY|*           */

	  unsigned char FAR *pbVal;      /* VT_BYREF|VT_UI1      */
	  short    FAR* piVal;       /* VT_BYREF|VT_I2       */
	  long     FAR* plVal;       /* VT_BYREF|VT_I4       */
	  float    FAR* pfltVal;     /* VT_BYREF|VT_R4       */
	  double       FAR* pdblVal;     /* VT_BYREF|VT_R8       */
	  VARIANT_BOOL FAR* pbool;       /* VT_BYREF|VT_BOOL     */
	  SCODE    FAR* pscode;      /* VT_BYREF|VT_ERROR    */
	  CY       FAR* pcyVal;      /* VT_BYREF|VT_CY       */
	  DATE     FAR* pdate;       /* VT_BYREF|VT_DATE     */
	  BSTR     FAR* pbstrVal;    /* VT_BYREF|VT_BSTR     */
	  IUnknown  FAR* FAR* ppunkVal;  /* VT_BYREF|VT_UNKNOWN  */
	  IDispatch FAR* FAR* ppdispVal; /* VT_BYREF|VT_DISPATCH */
	  SAFEARRAY FAR* FAR* pparray;   /* VT_BYREF|VT_ARRAY|*  */
	  VARIANT      FAR* pvarVal;     /* VT_BYREF|VT_VARIANT  */

	  void     FAR* byref;       /* Generic ByRef        */

	  // types new to DBVARIANTs
	  //
	  BLOB         blob;             /* VT_BLOB              */
	  DBCOLUMNID  *pColumnid;        /* DBTYPE_COLUMNID      */
	  LPSTR        pszVal;           /* VT_LPSTR             */
#ifdef WIN32
	  LPWSTR       pwszVal;          /* VT_LPWSTR            */
	  LPWSTR FAR  *ppwszVal;         /* VT_LPWSTR|VT_BYREF   */
#endif /* WIN32 */
	  BLOB FAR    *pblob;            /* VT_BYREF|VT_BLOB     */
	  DBCOLUMNID **ppColumnid;       /* VT_BYREF|DBTYPE_COLID*/
	  DBVARIANT   *pdbvarVal;        /* VT_BYREF|DBTYPE_VARIANT */
	}
#if defined(NONAMELESSUNION) || (defined(_MAC) && !defined(__cplusplus) && !defined(_MSC_VER))
	u
#endif
	;
};

/*----------------------------------------------------------------------------
 *
 *  dbs.h
 *
 *----------------------------------------------------------------------------
 */
typedef enum tagDBROWFETCH
  {
	DBROWFETCH_DEFAULT = 0,
	DBROWFETCH_CALLEEALLOCATES = 1,
	DBROWFETCH_FORCEREFRESH = 2
  }
DBROWFETCH;

typedef struct tagDBFETCHROWS
  {
  ULONG      cRowsRequested;
  DWORD      dwFlags;
  VOID HUGEP *pData;
  VOID HUGEP *pVarData;
  ULONG      cbVarData;
  ULONG      cRowsReturned;
  }
DBFETCHROWS;

#define DB_NOMAXLENGTH   (DWORD)0
#define DB_NOVALUE       (DWORD)0xFFFFFFFF
#define DB_NULL          (DWORD)0xFFFFFFFF
#define DB_EMPTY         (DWORD)0xFFFFFFFE
#define DB_USEENTRYID    (DWORD)0xFFFFFFFD
#define DB_CANTCOERCE    (DWORD)0xFFFFFFFC
#define DB_TRUNCATED     (DWORD)0xFFFFFFFB
#define DB_UNKNOWN       (DWORD)0xFFFFFFFA
#define DB_NOINFO        (DWORD)0xFFFFFFF9

#ifdef OLD_DEFS
typedef enum tagDBBINDING
  {
	DBBINDING_DEFAULT = 0,
	DBBINDING_VARIANT = 1,
	DBBINDING_ENTRYID = 2
  }
DBBINDING;
#else
typedef enum _DBBINDING
  {
	DBBINDING_DEFAULT = 0,
	DBBINDING_VARIANT = 1,
	DBBINDING_ENTRYID = 2
  }
_DBBINDING;
#endif

typedef enum tagDBBINDTYPE
  {
		DBBINDTYPE_DATA    = 0,
	DBBINDTYPE_ENTRYID = 1,
	DBBDINTYPE_EITHER  = 2,
	DBBINDTYPE_BOTH    = 3
  }
DBBINDTYPE;

typedef struct tagDBCOLUMNBINDING
  {
  DBCOLUMNID columnID;
  ULONG obData;
  ULONG cbMaxLen;
  ULONG obVarDataLen;
  ULONG obInfo;
  DWORD dwBinding;
  DWORD dwDataType;
  }
DBCOLUMNBINDING;

typedef struct tagDBBINDPARAMS
  {
  ULONG cbMaxLen;
  DWORD dwBinding;
  DWORD dwDataType;
  ULONG cbVarDataLen;
  DWORD dwInfo;
  void *pData;
  }
DBBINDPARAMS;

#define CID_NUMBER_INVALID            -1
#define CID_NUMBER_AUTOINCREMENT       0
#define CID_NUMBER_BASECOLUMNNAME      1
#define CID_NUMBER_BASENAME            2
#define CID_NUMBER_BINARYCOMPARABLE    3
#define CID_NUMBER_BINDTYPE            4
#define CID_NUMBER_CASESENSITIVE       5
#define CID_NUMBER_COLLATINGORDER      6
#define CID_NUMBER_COLUMNID            7
#define CID_NUMBER_CURSORCOLUMN        8
#define CID_NUMBER_DATACOLUMN          9
#define CID_NUMBER_DEFAULTVALUE        10
#define CID_NUMBER_ENTRYIDMAXLENGTH    11
#define CID_NUMBER_FIXED               12
#define CID_NUMBER_HASDEFAULT          13
#define CID_NUMBER_MAXLENGTH           14
#define CID_NUMBER_MULTIVALUED         15
#define CID_NUMBER_NAME                16
#define CID_NUMBER_NULLABLE            17
#define CID_NUMBER_PHYSICALSORT        18
#define CID_NUMBER_NUMBER              19
#define CID_NUMBER_ROWENTRYID          20
#define CID_NUMBER_SCALE               21
#define CID_NUMBER_SEARCHABLE          22
#define CID_NUMBER_TYPE                23
#define CID_NUMBER_UNIQUE              24
#define CID_NUMBER_UPDATABLE           25
#define CID_NUMBER_VERSION             26
#define CID_NUMBER_STATUS              27

/* c and C++ have different meanings for const.
 */
#ifdef __cplusplus
#define EXTERNAL_DEFN    extern const
#else
#define EXTERNAL_DEFN    const
#endif /* __cplusplus */



#define _DBCIDGUID {0xfe284700L,0xd188,0x11cd,{0xad,0x48, 0x0,0xaa, 0x0,0x3c,0x9c,0xb6}}
#ifdef DBINITCONSTANTS

EXTERNAL_DEFN DBCOLUMNID NEAR COLUMNID_INVALID         = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, -1};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_AUTOINCREMENT     = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 0};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BASECOLUMNNAME    = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 1};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BASENAME          = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 2};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BINARYCOMPARABLE  = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 3};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BINDTYPE          = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 4};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_CASESENSITIVE     = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 5};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_COLLATINGORDER    = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 6};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_COLUMNID          = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 7};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_CURSORCOLUMN      = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 8};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_DATACOLUMN        = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 9};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_DEFAULTVALUE      = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 10};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_ENTRYIDMAXLENGTH  = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 11};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_FIXED             = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 12};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_HASDEFAULT        = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 13};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_MAXLENGTH         = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 14};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_MULTIVALUED       = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 15};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_NAME              = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 16};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_NULLABLE          = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 17};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_PHYSICALSORT      = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 18};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_NUMBER            = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 19};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_ROWENTRYID        = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 20};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_SCALE             = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 21};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_SEARCHABLE        = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 22};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_TYPE              = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 23};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_UNIQUE            = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 24};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_UPDATABLE         = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 25};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_VERSION           = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 26};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_STATUS            = {_DBCIDGUID, DBCOLKIND_GUID_NUMBER, 27};
#else
extern const DBCOLUMNID NEAR COLUMNID_INVALID;
extern const DBCOLUMNID NEAR COLUMN_AUTOINCREMENT;
extern const DBCOLUMNID NEAR COLUMN_BASECOLUMNNAME;
extern const DBCOLUMNID NEAR COLUMN_BASENAME;
extern const DBCOLUMNID NEAR COLUMN_BINARYCOMPARABLE;
extern const DBCOLUMNID NEAR COLUMN_BINDTYPE;
extern const DBCOLUMNID NEAR COLUMN_CASESENSITIVE;
extern const DBCOLUMNID NEAR COLUMN_COLLATINGORDER;
extern const DBCOLUMNID NEAR COLUMN_COLUMNID;
extern const DBCOLUMNID NEAR COLUMN_CURSORCOLUMN;
extern const DBCOLUMNID NEAR COLUMN_DATACOLUMN;
extern const DBCOLUMNID NEAR COLUMN_DEFAULTVALUE;
extern const DBCOLUMNID NEAR COLUMN_ENTRYIDMAXLENGTH;
extern const DBCOLUMNID NEAR COLUMN_FIXED;
extern const DBCOLUMNID NEAR COLUMN_HASDEFAULT;
extern const DBCOLUMNID NEAR COLUMN_MAXLENGTH;
extern const DBCOLUMNID NEAR COLUMN_MULTIVALUED;
extern const DBCOLUMNID NEAR COLUMN_NAME;
extern const DBCOLUMNID NEAR COLUMN_NULLABLE;
extern const DBCOLUMNID NEAR COLUMN_PHYSICALSORT;
extern const DBCOLUMNID NEAR COLUMN_NUMBER;
extern const DBCOLUMNID NEAR COLUMN_ROWENTRYID;
extern const DBCOLUMNID NEAR COLUMN_SCALE;
extern const DBCOLUMNID NEAR COLUMN_SEARCHABLE;
extern const DBCOLUMNID NEAR COLUMN_TYPE;
extern const DBCOLUMNID NEAR COLUMN_UNIQUE;
extern const DBCOLUMNID NEAR COLUMN_UPDATABLE;
extern const DBCOLUMNID NEAR COLUMN_VERSION;
extern const DBCOLUMNID NEAR COLUMN_STATUS;
#endif

#define BMK_NUMBER_BMKTEMPORARY    0
#define BMK_NUMBER_BMKTEMPORARYREL 1
#define BMK_NUMBER_BMKCURSOR       2
#define BMK_NUMBER_BMKCURSORREL    3
#define BMK_NUMBER_BMKSESSION      4
#define BMK_NUMBER_BMKSESSIONREL   5
#define BMK_NUMBER_BMKPERSIST      6
#define BMK_NUMBER_BMKPERSISTREL   7


#define DBBMKGUID {0xf6304bb0L,0xd188,0x11cd,{0xad,0x48, 0x0,0xaa, 0x0,0x3c,0x9c,0xb6}}
#ifdef DBINITCONSTANTS
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKTEMPORARY      = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 0};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKTEMPORARYREL   = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 1};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKCURSOR         = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 2};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKCURSORREL      = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 3};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKSESSION        = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 4};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKSESSIONREL     = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 5};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKPERSIST        = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 6};
EXTERNAL_DEFN DBCOLUMNID NEAR COLUMN_BMKPERSISTREL     = {DBBMKGUID, DBCOLKIND_GUID_NUMBER, 7};
#else
extern const DBCOLUMNID NEAR COLUMN_BMKINVALID;
extern const DBCOLUMNID NEAR COLUMN_BMKTEMPORARY;
extern const DBCOLUMNID NEAR COLUMN_BMKTEMPORARYREL;
extern const DBCOLUMNID NEAR COLUMN_BMKCURSOR;
extern const DBCOLUMNID NEAR COLUMN_BMKCURSORREL;
extern const DBCOLUMNID NEAR COLUMN_BMKSESSION;
extern const DBCOLUMNID NEAR COLUMN_BMKSESSIONREL;
extern const DBCOLUMNID NEAR COLUMN_BMKPERSIST;
extern const DBCOLUMNID NEAR COLUMN_BMKPERSISTREL;
#endif

#define DB_BMK_SIZE        sizeof(BYTE)
#ifdef DBINITCONSTANTS
#ifdef OLD_DEFS
EXTERNAL_DEFN BYTE NEAR DBBMK_INVALID   = 0x0;
#endif
EXTERNAL_DEFN BYTE NEAR DBBMK_CURRENT   = 0x1;
EXTERNAL_DEFN BYTE NEAR DBBMK_BEGINNING = 0x2;
EXTERNAL_DEFN BYTE NEAR DBBMK_END       = 0x3;
#else
#ifdef OLD_DEFS
extern const BYTE NEAR DBBMK_INVALID;
#endif
extern const BYTE NEAR DBBMK_CURRENT;
extern const BYTE NEAR DBBMK_BEGINNING;
extern const BYTE NEAR DBBMK_END;
#endif

typedef enum tagDBCOLUMNBINDOPTS
  {
	DBCOLUMNBINDOPTS_REPLACE = 0,
	DBCOLUMNBINDOPTS_ADD = 1
  }
DBCOLUMNBINDOPTS;

typedef enum tagDBUPDATELOCK
  {
	DBUPDATELOCK_PESSIMISTIC = 0,
	DBUPDATELOCK_OPTIMISTIC = 1
  }
DBUPDATELOCK;

typedef enum tagDBCOLUMNDATA
  {
	DBCOLUMNDATA_UNCHANGED = 0,
	DBCOLUMNDATA_CHANGED = 1,
		DBCOLUMNDATA_UNKNOWN = 2
  }
DBCOLUMNDATA;

typedef enum tagDBROWACTION
  {
	DBROWACTION_IGNORE = 0,
	DBROWACTION_UPDATE = 1,
	DBROWACTION_DELETE = 2,
	DBROWACTION_ADD = 3,
	DBROWACTION_LOCK = 4,
	DBROWACTION_UNLOCK = 5
  }
DBROWACTION;

typedef enum tagDBUPDATEABLE
  {
	DBUPDATEABLE_UPDATEABLE = 0,
	DBUPDATEABLE_NOTUPDATEABLE = 1,
	DBUPDATEABLE_UNKNOWN = 2
  }
DBUPDATEABLE;

#ifdef OLD_DEFS
typedef struct tagDBROWSTATUS
  {
  HRESULT hrStatus;
  BLOB Bookmark;
  }
DBROWSTATUS;
#endif

typedef enum tagDBEVENTWHATS
  {
	DBEVENT_CURRENT_ROW_CHANGED = 1,
	DBEVENT_CURRENT_ROW_DATA_CHANGED = 2,
	DBEVENT_NONCURRENT_ROW_DATA_CHANGED = 4,
	DBEVENT_SET_OF_COLUMNS_CHANGED = 8,
	DBEVENT_ORDER_OF_COLUMNS_CHANGED = 16,
	DBEVENT_SET_OF_ROWS_CHANGED = 32,
	DBEVENT_ORDER_OF_ROWS_CHANGED = 64,
	DBEVENT_METADATA_CHANGED = 128,
	DBEVENT_ASYNCH_OP_FINISHED = 256,
	DBEVENT_FIND_CRITERIA_CHANGED = 512,
  }
DBEVENTWHATS;

typedef enum tagDBREASON
  {
	DBREASON_DELETED = 1,
	DBREASON_INSERTED = 2,
	DBREASON_MODIFIED = 3,
	DBREASON_REMOVEDFROMCURSOR = 4,
	DBREASON_MOVEDINCURSOR = 5,
	DBREASON_MOVE = 6,
	DBREASON_FIND = 7,
	DBREASON_NEWINDEX = 8,
	DBREASON_ROWFIXUP = 9,
	DBREASON_RECALC = 10,
	DBREASON_REFRESH = 11,
	DBREASON_NEWPARAMETERS = 12,
	DBREASON_SORTCHANGED = 13,
	DBREASON_FILTERCHANGED = 14,
	DBREASON_QUERYSPECCHANGED = 15,
	DBREASON_SEEK = 16,
	DBREASON_PERCENT = 17,
	DBREASON_FINDCRITERIACHANGED = 18,
	DBREASON_SETRANGECHANGED = 19,
	DBREASON_ADDNEW = 20,
	DBREASON_MOVEPERCENT = 21,
	DBREASON_BEGINTRANSACT = 22,
	DBREASON_ROLLBACK = 23,
	DBREASON_COMMIT = 24,
	DBREASON_CLOSE = 25,
	DBREASON_BULK_ERROR = 26,
	DBREASON_BULK_NOTTRANSACTABLE = 27,
	DBREASON_BULK_ABOUTTOEXECUTE = 28,
	DBREASON_CANCELUPDATE = 29,
	DBREASON_SETCOLUMN = 30,
	DBREASON_EDIT = 31,
	DBREASON_UNLOAD = 32
  }
#ifdef OLD_DEFS
DBREASON
#else
_DBREASON;
#endif

// Arg1 values for DBREASON_FIND
typedef enum tagDBFINDTYPES
  {
  DB_FINDFIRST = 1,
  DB_FINDLAST = 2,
  DB_FINDNEXT = 3,
  DB_FINDPRIOR = 4,
  DB_FIND = 5
  }
DBFINDTYPES;

typedef struct tagDBNOTIFYREASON
  {
  DWORD dwReason;
  DBVARIANT arg1;
  DBVARIANT arg2;
  }
DBNOTIFYREASON;

#define DB_E_BADBINDINFO           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e00)
#define DB_E_BADBOOKMARK           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e01)
#define DB_E_BADCOLUMNID           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e02)
#define DB_E_BADCRITERIA           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e03)
#define DB_E_BADENTRYID            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e04)
#define DB_E_BADFRACTION           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e05)
#define DB_E_BADINDEXID            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e06)
#define DB_E_BADQUERYSPEC          MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e07)
#define DB_E_BADSORTORDER          MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e08)
#define DB_E_BADVALUES             MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e09)
#define DB_E_CANTCOERCE            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0a)
#define DB_E_CANTLOCK              MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0b)
#define DB_E_COLUMNUNAVAILABLE     MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0c)
#define DB_E_DATACHANGED           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0d)
#define DB_E_INVALIDCOLUMNORDINAL  MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0e)
#define DB_E_INVALIDINTERFACE      MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e0f)
#define DB_E_LOCKFAILED            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e10)
#define DB_E_ROWDELETED            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e11)
#define DB_E_ROWTOOSHORT           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e12)
#define DB_E_SCHEMAVIOLATION       MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e13)
#define DB_E_SEEKKINDNOTSUPPORTED  MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e14)
#define DB_E_UPDATEINPROGRESS      MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e15)
#define DB_E_USEENTRYID            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e16)
#define DB_E_STATEERROR            MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e17)
#define DB_E_BADFETCHINFO          MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e18)
#define DB_E_NOASYNC               MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e19)
#define DB_E_ENTRYIDOPEN           MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e1a)
#define DB_E_BUFFERTOOSMALL        MAKE_SCODE(SEVERITY_ERROR,   FACILITY_ITF, 0x0e1b)
#define DB_S_BUFFERTOOSMALL        MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec0)
#define DB_S_CANCEL                MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec1)
#define DB_S_DATACHANGED           MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec2)
#define DB_S_ENDOFCURSOR           MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec3)
#define DB_S_ENDOFRESULTSET        MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec4)
#define DB_S_OPERATIONCANCELLED    MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec5)
#define DB_S_QUERYINTERFACE        MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec6)
#define DB_S_WORKINGASYNC          MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec7)
//#define DB_S_COULDNTCOERCE         MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec8)
#define DB_S_MOVEDTOFIRST          MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ec9)
#define DB_S_CURRENTROWUNCHANGED   MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0eca)
#define DB_S_ROWADDED              MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ecb)
#define DB_S_ROWUPDATED            MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ecc)
#define DB_S_ROWDELETED            MAKE_SCODE(SEVERITY_SUCCESS, FACILITY_ITF, 0x0ecd)

/*----------------------------------------------------------------------------
 *
 *  ICursor
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface ICursor ICursor;

#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_ICursor;

interface ICursor : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE GetColumnsCursor
	(
	REFIID riid,
	IUnknown **ppvColumnsCursor,
	ULONG *pcRows
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE SetBindings
	(
	ULONG cCol,
	DBCOLUMNBINDING rgBoundColumns[],
	ULONG cbRowLength,
	DWORD dwFlags
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetBindings
	(
	ULONG *pcCol,
	DBCOLUMNBINDING *prgBoundColumns[],
	ULONG *pcbRowLength
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetNextRows
	(
	LARGE_INTEGER udlRowsToSkip,
	DBFETCHROWS *pFetchParams
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Requery
	(
		void
	) = 0;

};

#else

/* C Language Binding */
//extern const IID IID_ICursor;

typedef struct ICursorVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		ICursor FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		ICursor FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		ICursor FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetColumnsCursor)
	(
		ICursor FAR *this,
	REFIID riid,
	IUnknown **ppvColumnsCursor,
	ULONG *pcRows
	);

	HRESULT (STDMETHODCALLTYPE FAR *SetBindings)
	(
		ICursor FAR *this,
	ULONG cCol,
	DBCOLUMNBINDING rgBoundColumns[],
	ULONG cbRowLength,
	DWORD dwFlags
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetBindings)
	(
		ICursor FAR *this,
	ULONG *pcCol,
	DBCOLUMNBINDING *prgBoundColumns[],
	ULONG *pcbRowLength
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetNextRows)
	(
		ICursor FAR *this,
	LARGE_INTEGER udlRowsToSkip,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *Requery)
	(
		ICursor FAR *this
	);

} ICursorVtbl;

interface ICursor
{
	ICursorVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define ICursor_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define ICursor_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define ICursor_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define ICursor_GetColumnsCursor(pI, riid, ppvColumnsCursor, pcRows) \
	(*(pI)->lpVtbl->GetColumnsCursor)((pI), riid, ppvColumnsCursor, pcRows)

#define ICursor_SetBindings(pI, cCol, rgBoundColumns, cbRowLength, dwFlags) \
	(*(pI)->lpVtbl->SetBindings)((pI), cCol, rgBoundColumns, cbRowLength, dwFlags)

#define ICursor_GetBindings(pI, pcCol, prgBoundColumns, pcbRowLength) \
	(*(pI)->lpVtbl->GetBindings)((pI), pcCol, prgBoundColumns, pcbRowLength)

#define ICursor_GetNextRows(pI, udlRowsToSkip, pFetchParams) \
	(*(pI)->lpVtbl->GetNextRows)((pI), udlRowsToSkip, pFetchParams)

#define ICursor_Requery(pI) \
	(*(pI)->lpVtbl->Requery)((pI))

#endif /* COBJMACROS */

#endif

/*----------------------------------------------------------------------------
 *
 *  ICursorMove
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface ICursorMove ICursorMove;

typedef enum tagDBCLONEOPTS
  {
	DBCLONEOPTS_DEFAULT = 0,
	DBCLONEOPTS_SAMEROW = 1
  }
DBCLONEOPTS;


#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_ICursorMove;

interface ICursorMove : public ICursor
{
public:
	virtual HRESULT STDMETHODCALLTYPE Move
	(
	ULONG cbBookmark,
	void *pBookmark,
	LARGE_INTEGER dlOffset,
	DBFETCHROWS *pFetchParams
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetBookmark
	(
	DBCOLUMNID *pBookmarkType,
	ULONG cbMaxSize,
	ULONG *pcbBookmark,
	void *pBookmark
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Clone
	(
	DWORD dwFlags,
	REFIID riid,
	IUnknown **ppvClonedCursor
	) = 0;

};

#else

/* C Language Binding */
//extern const IID IID_ICursorMove;

typedef struct ICursorMoveVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		ICursorMove FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		ICursorMove FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		ICursorMove FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetColumnsCursor)
	(
		ICursorMove FAR *this,
	REFIID riid,
	IUnknown **ppvColumnsCursor,
	ULONG *pcRows
	);

	HRESULT (STDMETHODCALLTYPE FAR *SetBindings)
	(
		ICursorMove FAR *this,
	ULONG cCol,
	DBCOLUMNBINDING rgBoundColumns[],
	ULONG cbRowLength,
	DWORD dwFlags
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetBindings)
	(
		ICursorMove FAR *this,
	ULONG *pcCol,
	DBCOLUMNBINDING *prgBoundColumns[],
	ULONG *pcbRowLength
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetNextRows)
	(
		ICursorMove FAR *this,
	LARGE_INTEGER udlRowsToSkip,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *Requery)
	(
		ICursorMove FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *Move)
	(
		ICursorMove FAR *this,
	ULONG cbBookmark,
	void *pBookmark,
	LARGE_INTEGER dlOffset,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetBookmark)
	(
		ICursorMove FAR *this,
	DBCOLUMNID *pBookmarkType,
	ULONG cbMaxSize,
	ULONG *pcbBookmark,
	void *pBookmark
	);

	HRESULT (STDMETHODCALLTYPE FAR *Clone)
	(
		ICursorMove FAR *this,
	DWORD dwFlags,
	REFIID riid,
	IUnknown **ppvClonedCursor
	);

} ICursorMoveVtbl;

interface ICursorMove
{
	ICursorMoveVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define ICursorMove_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define ICursorMove_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define ICursorMove_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define ICursorMove_GetColumnsCursor(pI, riid, ppvColumnsCursor, pcRows) \
	(*(pI)->lpVtbl->GetColumnsCursor)((pI), riid, ppvColumnsCursor, pcRows)

#define ICursorMove_SetBindings(pI, cCol, rgBoundColumns, cbRowLength, dwFlags) \
	(*(pI)->lpVtbl->SetBindings)((pI), cCol, rgBoundColumns, cbRowLength, dwFlags)

#define ICursorMove_GetBindings(pI, pcCol, prgBoundColumns, pcbRowLength) \
	(*(pI)->lpVtbl->GetBindings)((pI), pcCol, prgBoundColumns, pcbRowLength)

#define ICursorMove_GetNextRows(pI, udlRowsToSkip, pFetchParams) \
	(*(pI)->lpVtbl->GetNextRows)((pI), udlRowsToSkip, pFetchParams)

#define ICursorMove_Requery(pI) \
	(*(pI)->lpVtbl->Requery)((pI))

#define ICursorMove_Move(pI, cbBookmark, pBookmark, dlOffset, pFetchParams) \
	(*(pI)->lpVtbl->Move)((pI), cbBookmark, pBookmark, dlOffset, pFetchParams)

#define ICursorMove_GetBookmark(pI, pBookmarkType, cbMaxSize, pcbBookmark, pBookmark) \
	(*(pI)->lpVtbl->GetBookmark)((pI), pBookmarkType, cbMaxSize, pcbBookmark, pBookmark)

#define ICursorMove_Clone(pI, dwFlags, riid, ppvClonedCursor) \
	(*(pI)->lpVtbl->Clone)((pI), dwFlags, riid, ppvClonedCursor)
#endif /* COBJMACROS */

#endif

/*----------------------------------------------------------------------------
 *
 *  ICursorScroll
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface ICursorScroll ICursorScroll;

typedef enum tagDBCURSORPOPULATED
  {
	DBCURSORPOPULATED_FULLY = 0,
	DBCURSORPOPULATED_PARTIALLY = 1
  }
DBCURSORPOPULATED;


#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_ICursorScroll;

interface ICursorScroll : public ICursorMove
{
public:
	virtual HRESULT STDMETHODCALLTYPE Scroll
	(
	ULONG ulNumerator,
	ULONG ulDenominator,
	DBFETCHROWS *pFetchParams
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetApproximatePosition
	(
	ULONG cbBookmark,
	void *pBookmark,
	ULONG *pulNumerator,
	ULONG *pulDenominator
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetApproximateCount
	(
	LARGE_INTEGER *pudlApproxCount,
	DWORD *pdwFullyPopulated
	) = 0;

};

#else

/* C Language Binding */
//extern const IID IID_ICursorScroll;

typedef struct ICursorScrollVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		ICursorScroll FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		ICursorScroll FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		ICursorScroll FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetColumnsCursor)
	(
		ICursorScroll FAR *this,
	REFIID riid,
	IUnknown **ppvColumnsCursor,
	ULONG *pcRows
	);

	HRESULT (STDMETHODCALLTYPE FAR *SetBindings)
	(
		ICursorScroll FAR *this,
	ULONG cCol,
	DBCOLUMNBINDING rgBoundColumns[],
	ULONG cbRowLength,
	DWORD dwFlags
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetBindings)
	(
		ICursorScroll FAR *this,
	ULONG *pcCol,
	DBCOLUMNBINDING *prgBoundColumns[],
	ULONG *pcbRowLength
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetNextRows)
	(
		ICursorScroll FAR *this,
	LARGE_INTEGER udlRowsToSkip,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *Requery)
	(
		ICursorScroll FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *Move)
	(
		ICursorScroll FAR *this,
	ULONG cbBookmark,
	void *pBookmark,
	LARGE_INTEGER dlOffset,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetBookmark)
	(
		ICursorScroll FAR *this,
	DBCOLUMNID *pBookmarkType,
	ULONG cbMaxSize,
	ULONG *pcbBookmark,
	void *pBookmark
	);

	HRESULT (STDMETHODCALLTYPE FAR *Clone)
	(
		ICursorScroll FAR *this,
	DWORD dwFlags,
	REFIID riid,
	IUnknown **ppvClonedCursor
	);

	HRESULT (STDMETHODCALLTYPE FAR *Scroll)
	(
		ICursorScroll FAR *this,
	ULONG ulNumerator,
	ULONG ulDenominator,
	DBFETCHROWS *pFetchParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetApproximatePosition)
	(
		ICursorScroll FAR *this,
	ULONG cbBookmark,
	void *pBookmark,
	ULONG *pulNumerator,
	ULONG *pulDenominator
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetApproximateCount)
	(
		ICursorScroll FAR *this,
	LARGE_INTEGER *pudlApproxCount,
	DWORD *pdwFullyPopulated
	);

} ICursorScrollVtbl;

interface ICursorScroll
{
	ICursorScrollVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define ICursorScroll_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define ICursorScroll_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define ICursorScroll_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define ICursorScroll_GetColumnsCursor(pI, riid, ppvColumnsCursor, pcRows) \
	(*(pI)->lpVtbl->GetColumnsCursor)((pI), riid, ppvColumnsCursor, pcRows)

#define ICursorScroll_SetBindings(pI, cCol, rgBoundColumns, cbRowLength, dwFlags) \
	(*(pI)->lpVtbl->SetBindings)((pI), cCol, rgBoundColumns, cbRowLength, dwFlags)

#define ICursorScroll_GetBindings(pI, pcCol, prgBoundColumns, pcbRowLength) \
	(*(pI)->lpVtbl->GetBindings)((pI), pcCol, prgBoundColumns, pcbRowLength)

#define ICursorScroll_GetNextRows(pI, udlRowsToSkip, pFetchParams) \
	(*(pI)->lpVtbl->GetNextRows)((pI), udlRowsToSkip, pFetchParams)

#define ICursorScroll_Requery(pI) \
	(*(pI)->lpVtbl->Requery)((pI))

#define ICursorScroll_Move(pI, cbBookmark, pBookmark, dlOffset, pFetchParams) \
	(*(pI)->lpVtbl->Move)((pI), cbBookmark, pBookmark, dlOffset, pFetchParams)

#define ICursorScroll_GetBookmark(pI, pBookmarkType, cbMaxSize, pcbBookmark, pBookmark) \
	(*(pI)->lpVtbl->GetBookmark)((pI), pBookmarkType, cbMaxSize, pcbBookmark, pBookmark)

#define ICursorScroll_Clone(pI, dwFlags, riid, ppvClonedCursor) \
	(*(pI)->lpVtbl->Clone)((pI), dwFlags, riid, ppvClonedCursor)

#define ICursorScroll_Scroll(pI, ulNumerator, ulDenominator, pFetchParams) \
	(*(pI)->lpVtbl->Scroll)((pI), ulNumerator, ulDenominator, pFetchParams)

#define ICursorScroll_GetApproximatePosition(pI, cbBookmark, pBookmark, pulNumerator, pulDenominator) \
	(*(pI)->lpVtbl->GetApproximatePosition)((pI), cbBookmark, pBookmark, pulNumerator, pulDenominator)

#define ICursorScroll_GetApproximateCount(pI, pudlApproxCount, pdwFullyPopulated) \
	(*(pI)->lpVtbl->GetApproximateCount)((pI), pudlApproxCount, pdwFullyPopulated)
#endif /* COBJMACROS */

#endif

/*----------------------------------------------------------------------------
 *
 *  ICursorUpdateARow
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface ICursorUpdateARow ICursorUpdateARow;

typedef enum tagDBEDITMODE
  {
	DBEDITMODE_NONE = 1,
	DBEDITMODE_UPDATE = 2,
	DBEDITMODE_ADD = 3
  }
DBEDITMODE;


#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_ICursorUpdateARow;

interface ICursorUpdateARow : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE BeginUpdate
	(
	DWORD dwFlags
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE SetColumn
	(
	DBCOLUMNID *pcid,
	DBBINDPARAMS *pBindParams
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetColumn
	(
	DBCOLUMNID *pcid,
	DBBINDPARAMS *pBindParams,
	DWORD *pdwFlags
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE GetEditMode
	(
	DWORD *pdwState
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Update
	(
	DBCOLUMNID *pBookmarkType,
	ULONG *pcbBookmark,
	void **ppBookmark
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Cancel
	(
		void
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Delete
	(
	void
	) = 0;

};

#else

/* C Language Binding */
//extern const IID IID_ICursorUpdateARow;

typedef struct ICursorUpdateARowVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		ICursorUpdateARow FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		ICursorUpdateARow FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		ICursorUpdateARow FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *BeginUpdate)
	(
		ICursorUpdateARow FAR *this,
	DWORD dwFlags
	);

	HRESULT (STDMETHODCALLTYPE FAR *SetColumn)
	(
		ICursorUpdateARow FAR *this,
	DBCOLUMNID *pcid,
	DBBINDPARAMS *pBindParams
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetColumn)
	(
		ICursorUpdateARow FAR *this,
	DBCOLUMNID *pcid,
	DBBINDPARAMS *pBindParams,
	DWORD *pdwFlags
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetEditMode)
	(
		ICursorUpdateARow FAR *this,
	DWORD *pdwState
	);

	HRESULT (STDMETHODCALLTYPE FAR *Update)
	(
		ICursorUpdateARow FAR *this,
	DBCOLUMNID *pBookmarkType,
	ULONG *pcbBookmark,
	void **ppBookmark
	);

	HRESULT (STDMETHODCALLTYPE FAR *Cancel)
	(
		ICursorUpdateARow FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *Delete)
	(
		ICursorUpdateARow FAR *this
	);

} ICursorUpdateARowVtbl;

interface ICursorUpdateARow
{
	ICursorUpdateARowVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define ICursorUpdateARow_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define ICursorUpdateARow_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define ICursorUpdateARow_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define ICursorUpdateARow_BeginUpdate(pI, dwFlags) \
	(*(pI)->lpVtbl->BeginUpdate)((pI), dwFlags)

#define ICursorUpdateARow_SetColumn(pI, pcid, pBindParams) \
	(*(pI)->lpVtbl->SetColumn)((pI), pcid, pBindParams)

#define ICursorUpdateARow_GetColumn(pI, pcid, pBindParams, pdwFlags) \
	(*(pI)->lpVtbl->GetColumn)((pI), pcid, pBindParams, pdwFlags)

#define ICursorUpdateARow_GetEditMode(pI, pdwState) \
	(*(pI)->lpVtbl->GetEditMode)((pI), pdwState)

#define ICursorUpdateARow_Update(pI, pBookmarkType, pcbBookmark, ppBookmark) \
	(*(pI)->lpVtbl->Update)((pI), pBookmarkType, pcbBookmark, ppBookmark)

#define ICursorUpdateARow_Cancel(pI) \
	(*(pI)->lpVtbl->Cancel)((pI))

#define ICursorUpdateARow_Delete(pI) \
	(*(pI)->lpVtbl->Delete)((pI))


#endif /* COBJMACROS */

#endif

/*----------------------------------------------------------------------------
 *
 *  ICursorFind
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface ICursorFind ICursorFind;

typedef enum tagDBFINDFLAGS
  {
	DBFINDFLAGS_FINDNEXT = 1,
	DBFINDFLAGS_FINDPRIOR = 2,
	DBFINDFLAGS_INCLUDECURRENT = 4
  }
DBFINDFLAGS;

#ifdef OLD_DEFS
typedef enum tagDBSEEKFLAGS
  {
	DBSEEK_LT    = 1,
	DBSEEK_LE    = 2,
	DBSEEK_EQ    = 3,       // EXACT EQUALITY
	DBSEEK_GT    = 4,
	DBSEEK_GE    = 5,
	DBSEEK_PARTIALEQ = 6             // only for strings
  }
DBSEEKFLAGS;
#endif

#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_ICursorFind;

interface ICursorFind : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE FindByValues
	(
	ULONG            cbBookmark,
	LPVOID           pBookmark,
	DWORD            dwFindFlags,
	ULONG            cValues,
		DBCOLUMNID       rgColumns[],
	DBVARIANT        rgValues[],
	DWORD            rgdwSeekFlags[],
		DBFETCHROWS FAR *pFetchParams
	) = 0;
};

#else

/* C Language Binding */
//extern const IID IID_ICursorFind;

typedef struct ICursorFindVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		ICursorFind FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		ICursorFind FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		ICursorFind FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *FindByValues)
	(
		ICursorFind FAR *this,
	ULONG            cbBookmark,
	LPVOID           pBookmark,
	DWORD            dwFindFlags,
	ULONG            cValues,
		DBCOLUMNID       rgColumns[],
	DBVARIANT        rgValues[],
	DWORD            rgdwSeekFlags[],
		DBFETCHROWS      pFetchParams
	);


} ICursorFindVtbl;

interface ICursorFind
{
	ICursorFindVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define ICursorFind_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define ICursorFind_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define ICursorFind_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define ICursorFind_FindByValues(pI, cbB, pB, dwFF, cV, rgC, rgV, rgSF, pF) \
	(*(pI)->lpVtbl->FindByValues)((pI), cbB, pB, dwFF, cB, rgC, rgV, rgSF, pF)

#endif /* COBJMACROS */

#endif


/*----------------------------------------------------------------------------
 *
 *  IEntryID
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface IEntryID IEntryID;

#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_IEntryID;

interface IEntryID : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE GetInterface
	(
	ULONG cbEntryID,
	void *pEntryID,
		DWORD dwFlags,
		REFIID riid,
	IUnknown **ppvObj
	) = 0;

};

#else

/* C Language Binding */
//extern const IID IID_IEntryID;

typedef struct IEntryIDVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		IEntryID FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		IEntryID FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		IEntryID FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *GetInterface)
	(
		IEntryID FAR *this,
	ULONG cbEntryID,
	void *pEntryID,
		REFIID riid,
	IUnknown **ppvObj
	);

} IEntryIDVtbl;

interface IEntryID
{
	IEntryIDVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define IEntryID_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define IEntryID_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define IEntryID_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define IEntryID_GetInterface(pI, cbEntryID, pEntryID, riid, ppvObj) \
	(*(pI)->lpVtbl->GetInterface)((pI), cbEntryID, pEntryID, riid, ppvObj)
#endif /* COBJMACROS */

#endif


/*----------------------------------------------------------------------------
 *
 *  INotifyDBEvents
 *
 *----------------------------------------------------------------------------
 */
/* Forward declaration */
//typedef interface INotifyDBEvents INotifyDBEvents;

#if defined(__cplusplus) && !defined(CINTERFACE)

/* C++ Language Binding */
//extern "C" const IID IID_INotifyDBEvents;

interface INotifyDBEvents : public IUnknown
{
public:
	virtual HRESULT STDMETHODCALLTYPE OKToDo
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE Cancelled
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE SyncBefore
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE AboutToDo
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE FailedToDo
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE SyncAfter
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

	virtual HRESULT STDMETHODCALLTYPE DidEvent
	(
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	) = 0;

};

#else

/* C Language Binding */
extern const IID IID_INotifyDBEvents;

typedef struct INotifyDBEventsVtbl
{

	HRESULT (STDMETHODCALLTYPE FAR *QueryInterface)
	(
		INotifyDBEvents FAR *this,
	REFIID riid,
	void **ppvObject
	);

	ULONG (STDMETHODCALLTYPE FAR *AddRef)
	(
		INotifyDBEvents FAR *this
	);

	ULONG (STDMETHODCALLTYPE FAR *Release)
	(
		INotifyDBEvents FAR *this
	);

	HRESULT (STDMETHODCALLTYPE FAR *OKToDo)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *Cancelled)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *SyncBefore)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *AboutToDo)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *FailedToDo)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *SyncAfter)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

	HRESULT (STDMETHODCALLTYPE FAR *DidEvent)
	(
		INotifyDBEvents FAR *this,
	DWORD dwEventWhat,
	ULONG cReasons,
	DBNOTIFYREASON rgReasons[]
	);

} INotifyDBEventsVtbl;

interface INotifyDBEvents
{
	INotifyDBEventsVtbl FAR *lpVtbl;
} ;

#ifdef COBJMACROS

#define INotifyDBEvents_QueryInterface(pI, riid, ppvObject) \
	(*(pI)->lpVtbl->QueryInterface)((pI), riid, ppvObject)

#define INotifyDBEvents_AddRef(pI) \
	(*(pI)->lpVtbl->AddRef)((pI))

#define INotifyDBEvents_Release(pI) \
	(*(pI)->lpVtbl->Release)((pI))

#define INotifyDBEvents_OKToDo(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->OKToDo)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_Cancelled(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->Cancelled)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_SyncBefore(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->SyncBefore)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_AboutToDo(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->AboutToDo)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_FailedToDo(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->FailedToDo)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_SyncAfter(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->SyncAfter)((pI), dwEventWhat, cReasons, rgReasons)

#define INotifyDBEvents_DidEvent(pI, dwEventWhat, cReasons, rgReasons) \
	(*(pI)->lpVtbl->DidEvent)((pI), dwEventWhat, cReasons, rgReasons)
#endif /* COBJMACROS */

#endif


#ifdef __cplusplus
}
#endif

#define __OCDB_H_
#endif // __OCDB_H_
