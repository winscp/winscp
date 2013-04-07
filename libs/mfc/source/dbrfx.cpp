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
// CDBByteArray db specific class for holding byte array data
class CDBByteArray : public CByteArray
{
	DECLARE_DYNAMIC(CDBByteArray)

// Operations
	void SetLength(int nNewSize);
};

inline void CDBByteArray::SetLength(int nNewSize)
{
	// Can't grow buffer since ODBC has been SQLBindCol'd on it.
	ASSERT(nNewSize <= m_nMaxSize);
	m_nSize = nNewSize;
}

//////////////////////////////////////////////////////////////////////////////
// CFieldExchange

CFieldExchange::CFieldExchange(UINT nOperation, CRecordset* prs, void* pvField)
{
#ifdef _DEBUG
	ASSERT(nOperation >= BindParam && nOperation <= DumpField);
#endif
	ASSERT_VALID(prs);
	ASSERT(prs->m_hstmt != SQL_NULL_HSTMT);

	m_nFieldType = (UINT) noFieldType;
	m_nOperation = nOperation;
	m_prs = prs;
	m_pvField = pvField;

	m_nFields = 0;
	m_nParams = 0;
	m_nParamFields = 0;
	m_bField = FALSE;
	m_pstr = NULL;
	m_hstmt = SQL_NULL_HSTMT;

	m_lDefaultLBFetchSize = 0x00010000;
	m_lDefaultLBReallocSize = 0x00010000;
}

BOOL CFieldExchange::IsFieldType(UINT* pnField)
{
	if (m_nFieldType == outputColumn)
	{
		*pnField = ++m_nFields;
		// Recordset's m_nFields must match number of Fields!
		ASSERT(m_nFields <= m_prs->m_nFields);
	}
	else
	{
		// Make sure SetFieldType was called
		ASSERT(m_nFieldType == inputParam ||
			m_nFieldType == outputParam ||
			m_nFieldType == inoutParam);

		*pnField = ++m_nParams;
		// Recordset's m_nParams must match number of Params!
		ASSERT(m_nParams <= m_prs->m_nParams);
	}

	if (m_nOperation == BindParam || m_nOperation == RebindParam)
	{
		// only valid on a param field type
		return m_nFieldType != outputColumn;
	}
	else
	{
		// valid only on an outputColumn field type
		return m_nFieldType == outputColumn;
	}
}

// Default implementation for RFX functions
void CFieldExchange::Default(LPCTSTR szName,
	void* pv, LONG* plLength, int nCType, UINT cbValue, UINT cbPrecision)
{
	RETCODE nRetCode;
	UINT nField = (m_nFieldType == outputColumn)? m_nFields: m_nParams;
	switch (m_nOperation)
	{
	case BindParam:
		if (m_prs->IsParamStatusNull(nField - 1))
			*plLength = SQL_NULL_DATA;
		else
			*plLength = cbValue;
		// For params, CType is same as SQL type
		AFX_SQL_SYNC(::SQLBindParameter(m_hstmt, (UWORD)nField,
			(SWORD)m_nFieldType, (SWORD)nCType, (SWORD)nCType, cbPrecision, 0,
			pv, 0, plLength));
		if (nRetCode != SQL_SUCCESS)
			m_prs->ThrowDBException(nRetCode, m_hstmt);

		// Add the member address to the param map
		m_prs->m_mapParamIndex.SetAt(pv, (void*)nField);
		return;

	case RebindParam:
		// Only need to reset param length
		*plLength = m_prs->IsParamStatusNull(nField - 1) ? SQL_NULL_DATA : cbValue;
		return;

	case BindFieldForUpdate:
		if (!m_prs->IsFieldStatusDirty(nField - 1))
		{
			// If not dirty, set length to SQL_IGNORE for SQLSetPos updates
			*plLength = SQL_IGNORE;
		}
		else if (!m_prs->IsFieldStatusNull(nField - 1))
		{
			// Reset the length as it may have changed for var length fields
			*plLength = cbValue;
		}
		return;


	case UnbindFieldForUpdate:
		// Reset bound length to actual length to clear SQL_IGNOREs
		if (!m_prs->IsFieldStatusDirty(nField - 1))
			*plLength = cbValue;
		return;

	case BindFieldToColumn:
		AFX_SQL_SYNC(::SQLBindCol(m_prs->m_hstmt, (UWORD)nField, (SWORD)nCType,
			pv, cbValue, plLength));
		if (!m_prs->Check(nRetCode))
			m_prs->ThrowDBException(nRetCode);

		// Add the member address to the field map
		m_prs->m_mapFieldIndex.SetAt(pv, (void*)nField);
		return;

	case Name:
		if (m_prs->IsFieldStatusDirty(nField - 1))
		{
			// We require a name
			ASSERT(lstrlen(szName) != 0);

			*m_pstr += szName;
			*m_pstr += m_lpszSeparator;
		}
		return;

	case NameValue:
		if (m_prs->IsFieldStatusDirty(nField - 1))
		{
			*m_pstr += szName;
			*m_pstr += '=';
		}

		// Fall through
	case Value:
		if (m_prs->IsFieldStatusDirty(nField - 1))
		{
			// If user marked column NULL, reflect this in length
			if (m_prs->IsFieldStatusNull(nField - 1))
				*plLength = SQL_NULL_DATA;
			else
				*plLength = cbValue;

			// If optimizing for bulk add, only need lengths set correctly
			if(!(m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*m_pstr += '?';
				*m_pstr += m_lpszSeparator;
				m_nParamFields++;

				// Assumes all bound fields BEFORE unbound fields
				CODBCFieldInfo* pODBCInfo =
					&m_prs->m_rgODBCFieldInfos[nField - 1];

				AFX_SQL_SYNC(::SQLBindParameter(m_hstmt,
					(UWORD)m_nParamFields, SQL_PARAM_INPUT,
					(SWORD)nCType, pODBCInfo->m_nSQLType,
					pODBCInfo->m_nPrecision, pODBCInfo->m_nScale,
					pv, 0, plLength));
				if (nRetCode != SQL_SUCCESS)
					m_prs->ThrowDBException(nRetCode, m_hstmt);
			}
		}
		return;

	case MarkForUpdate:
		{
			// Get the field data
			CFieldInfo* pInfo = &m_prs->m_rgFieldInfos[nField - 1];

			// If user changed field value from previous value, mark field dirty
			if ((pInfo->m_bStatus & AFX_SQL_FIELD_FLAG_NULL))
			{
				if (!m_prs->IsFieldStatusNull(nField - 1))
					m_prs->SetDirtyFieldStatus(nField - 1);
			}
			else
			{
				// Saved field is not NULL. current field null, so field dirty
				BOOL bDirty = m_prs->IsFieldStatusNull(nField - 1);

				// If values differ, then field dirty
				void* pvDataCache;

				if (pInfo->m_nDataType == AFX_RFX_BOOL ||
					pInfo->m_nDataType == AFX_RFX_BYTE ||
					pInfo->m_nDataType == AFX_RFX_INT ||
					pInfo->m_nDataType == AFX_RFX_LONG ||
					pInfo->m_nDataType == AFX_RFX_SINGLE)
				{
					// If caching data by value, pass a ref
					pvDataCache = &pInfo->m_pvDataCache;
				}
				else
					pvDataCache = pInfo->m_pvDataCache;

				if (bDirty || !AfxCompareValueByRef(pv, pvDataCache, pInfo->m_nDataType))
					m_prs->SetDirtyFieldStatus(nField - 1);
			}

#ifdef _DEBUG
			// Field address must not change - ODBC's SQLBindCol depends upon this
			void* pvBind;

			switch (pInfo->m_nDataType)
			{
			default:
				pvBind = pv;
				break;

			case AFX_RFX_LPTSTR:
#ifdef _UNICODE
				pvBind = m_prs->m_pvFieldProxy[nField-1];
#else // !_UNICODE
				pvBind = pv;
#endif
				break;

			case AFX_RFX_TEXT:
#ifdef _UNICODE
				pvBind = m_prs->m_pvFieldProxy[nField-1];
#else // !_UNICODE
				{
					pvBind = ((CString*)pv)->GetBuffer(0);
					((CString*)pv)->ReleaseBuffer();
				}
#endif
				break;

			case AFX_RFX_OLEDATE:
			case AFX_RFX_DATE:
				pvBind = m_prs->m_pvFieldProxy[nField-1];
				break;

			case AFX_RFX_BINARY:
				pvBind = ((CByteArray*)pv)->GetData();
				break;
			}

			if (pInfo->m_pvBindAddress != pvBind)
			{
				TRACE1("Error: field address (column %u) has changed!\n",
					nField);
				ASSERT(FALSE);
			}
#endif // _DEBUG

			if ((m_pvField == NULL  || m_pvField == pv) &&
				m_prs->IsFieldStatusDirty(nField - 1))
			{
				m_bField = TRUE;
			}
		}
		return;

	case StoreField:
		AfxStoreField(*m_prs, nField, pv);
		return;

	case LoadField:
		AfxLoadField(*m_prs, nField, pv, plLength);
		return;

	default:
		ASSERT(FALSE);
	}
}

void AFXAPI RFX_Text(CFieldExchange* pFX, LPCTSTR szName,
	LPTSTR value, int nMaxLength, int nColumnType, short nScale)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));
	ASSERT(AfxIsValidAddress(value, nMaxLength));

	RETCODE nRetCode;
	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	default:
		pFX->Default(szName, value, plLength,
			SQL_C_CHAR, lstrlen(value), nMaxLength);
		return;

	case CFieldExchange::BindParam:
		{
			void* pvParam = value; // will be overwritten if UNICODE

#ifdef _UNICODE
			// Must use proxy to translate unicode data into non-unicode param
			pFX->m_prs->m_bRebindParams = TRUE;

			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvParamProxy == NULL)
			{
				pFX->m_prs->m_pvParamProxy = new void*[pFX->m_prs->m_nParams];
				memset(pFX->m_prs->m_pvParamProxy, 0,
					pFX->m_prs->m_nParams*sizeof(void*));
				pFX->m_prs->m_nProxyParams = pFX->m_prs->m_nParams;
			}

			// Allocate non-unicode string to nMaxLength if necessary for SQLBindParameter
			if (pFX->m_prs->m_pvParamProxy[nField-1] == NULL)
			{
				pvParam = new CHAR[nMaxLength+1];
				pFX->m_prs->m_pvParamProxy[nField-1] = pvParam;
			}
			else
				pvParam = pFX->m_prs->m_pvParamProxy[nField-1];

			// Now fill in the data value
			USES_CONVERSION;
			lstrcpyA((char*)pvParam, T2A((LPCTSTR)value));
#endif // _UNICODE

			*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
				SQL_NULL_DATA : SQL_NTS;

			AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt, (UWORD)nField,
				 (SWORD)pFX->m_nFieldType, SQL_C_CHAR, (SWORD)nColumnType,
				 nMaxLength, nScale, pvParam, nMaxLength, plLength));

			if (nRetCode != SQL_SUCCESS)
				pFX->m_prs->ThrowDBException(nRetCode, pFX->m_hstmt);

			// Add the member address to the param map
			pFX->m_prs->m_mapParamIndex.SetAt(value, (void*)nField);
		}
		return;

#ifdef _UNICODE
	case CFieldExchange::RebindParam:
		*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
			SQL_NULL_DATA : SQL_NTS;
		if (pFX->m_prs->m_nProxyParams != 0)
		{
			// Fill buffer (expected by SQLBindParameter) with new param data
			USES_CONVERSION;
			LPSTR lpszParam = (LPSTR)pFX->m_prs->m_pvParamProxy[nField-1];
			lstrcpyA(lpszParam, T2A((LPCTSTR)value));
		}
		return;
#endif // _UNICODE

	case CFieldExchange::BindFieldToColumn:
		{
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];
			UINT cbColumn = pODBCInfo->m_nPrecision;

			switch (pODBCInfo->m_nSQLType)
			{
			default:
#ifdef _DEBUG
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: string converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
#endif // _DEBUG

				// Add room for extra information like sign, decimal point, etc.
				cbColumn += 10;
				break;

			case SQL_LONGVARCHAR:
			case SQL_CHAR:
			case SQL_VARCHAR:
				break;

			case SQL_FLOAT:
			case SQL_REAL:
			case SQL_DOUBLE:
				// Add room for sign, decimal point and " E +XXX"
				cbColumn += 10;
				break;

			case SQL_DECIMAL:
			case SQL_NUMERIC:
				// Add room for sign and decimal point
				cbColumn += 2;
				break;

			case SQL_TIMESTAMP:
			case SQL_DATE:
			case SQL_TIME:
				// May need extra space, i.e. "{TS mm/dd/yyyy hh:mm:ss}"
				cbColumn += 10;
				break;

			case SQL_TINYINT:
			case SQL_SMALLINT:
			case SQL_INTEGER:
			case SQL_BIGINT:
				// Add room for sign
				cbColumn += 1;
				break;
			}

			// Constrain to user specified max length, subject to 256 byte min
			if (cbColumn > (UINT)nMaxLength || cbColumn < 256)
				cbColumn = nMaxLength;

			// Set up binding addres

			void* pvData = value;   // overwritten if _UNICODE
			value[cbColumn] = '\0';

#ifdef _UNICODE
			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvFieldProxy == NULL)
			{
				pFX->m_prs->m_pvFieldProxy = new void*[pFX->m_prs->m_nFields];
				memset(pFX->m_prs->m_pvFieldProxy, 0,
					pFX->m_prs->m_nFields*sizeof(void*));
				pFX->m_prs->m_nProxyFields = pFX->m_prs->m_nFields;
			}

			// Allocate non-unicode string for SQLBindCol (not necessary on Requery)
			if (pFX->m_prs->m_pvFieldProxy[nField-1] == NULL)
				pFX->m_prs->m_pvFieldProxy[nField-1] = new CHAR[cbColumn+2];

			pvData = pFX->m_prs->m_pvFieldProxy[nField-1];
#endif // _UNICODE

			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField,
				SQL_C_CHAR, pvData, cbColumn+1, plLength));
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);

			// Add the member address to the field map
			pFX->m_prs->m_mapFieldIndex.SetAt(value, (void*)nField);
		}
		return;

#ifdef _UNICODE
	case CFieldExchange::BindFieldForUpdate:
		if (pFX->m_prs->m_nProxyFields != 0)
		{
			// Fill buffer (expected by SQLSetPos) with new field data
			USES_CONVERSION;
			LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
			lstrcpyA(lpszData, T2A((LPCTSTR)value));

			pFX->Default(szName, (void *)lpszData, plLength, SQL_C_CHAR,
				lstrlenA(lpszData), nMaxLength);
		}
		return;
#endif // _UNICODE

	case CFieldExchange::Fixup:
			if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value[0] = '\0';
		}
		else
		{
#ifdef _UNICODE
			// Copy value out of the proxy
			value = (LPTSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
#endif

			LPTSTR lpsz = value;
			if (pFX->m_prs->m_pDatabase->m_bStripTrailingSpaces)
			{
				// find first trailing space
				LPTSTR lpszFirstTrailing = NULL;
				while (*lpsz != '\0')
				{
					if (*lpsz != ' ')
						lpszFirstTrailing = NULL;
					else
					{
						if (lpszFirstTrailing == NULL)
							lpszFirstTrailing = lpsz;
					}
					lpsz = _tcsinc(lpsz);
				}
				// truncate
				if (lpszFirstTrailing != NULL)
					*lpszFirstTrailing = '\0';

			}
			*plLength = lstrlen(value);
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				// Set string 0 length
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = SQL_NTS;
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;


#ifdef _UNICODE
	case CFieldExchange::NameValue:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			*pFX->m_pstr += szName;
			*pFX->m_pstr += '=';
		}
		// Fall through

	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			// Get the field data
			CODBCFieldInfo* pODBCInfo =
					&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				USES_CONVERSION;
				lstrcpyA(lpszData, T2A((LPCTSTR)value));
				*plLength = lstrlen(value);
			}

			// If optimizing for bulk add, only need lengths & proxy set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*pFX->m_pstr += '?';
				*pFX->m_pstr += pFX->m_lpszSeparator;
				pFX->m_nParamFields++;
				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt,
					(UWORD)pFX->m_nParamFields, SQL_PARAM_INPUT,
					SQL_C_CHAR, pODBCInfo->m_nSQLType, nMaxLength,
					pODBCInfo->m_nScale, lpszData, 0, plLength));
			}
		}
		return;
#endif // _UNICODE

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (*value != '\0')
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (*value == '\0')
			pFX->m_prs->SetNullFieldStatus(nField - 1);
		else
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		pFX->Default(szName, value, plLength,
			SQL_C_CHAR, lstrlen(value), nMaxLength);
		return;

	case CFieldExchange::LoadField:
		{
			// Get the field data
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Restore the status
			pFX->m_prs->SetFieldStatus(nField - 1, pInfo->m_bStatus);

			// If not NULL, restore the value and length
			if (!pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				value = LPTSTR(pInfo->m_pvDataCache);
				*plLength = lstrlen(value);

#ifdef _UNICODE
				// Must restore proxy for correct WHERE CURRENT OF operation
				USES_CONVERSION;
				LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
				lstrcpyA(lpszData, T2A((LPCTSTR)value));
#endif // _UNICODE
			}
			else
			{
				*plLength = SQL_NULL_DATA;
			}

#ifdef _DEBUG
			// Buffer address must not change - ODBC's SQLBindCol depends upon this
			void* pvBind;

#ifdef _UNICODE
			pvBind = pFX->m_prs->m_pvFieldProxy[nField-1];
#else // !_UNICODE
			pvBind = value;
#endif

			if (pvBind != pInfo->m_pvBindAddress)
			{
				TRACE1("Error: buffer (column %u) address has changed!\n",
					nField);
				ASSERT(FALSE);
			}
#endif // _DEBUG
		}
		return;

	case CFieldExchange::StoreField:
		AfxStoreField(*pFX->m_prs, nField, value);
		return;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new TCHAR[nMaxLength];
			pInfo->m_nDataType = AFX_RFX_LPTSTR;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

// Note: CString.m_pchData must not be changed.  This address is registered
// with ODBC and must remain valid until the recordset is released.
void AFXAPI RFX_Text(CFieldExchange* pFX, LPCTSTR szName,
	CString& value, int nMaxLength, int nColumnType, short nScale)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));
	ASSERT(AfxIsValidAddress(&value, sizeof(CString)));

	RETCODE nRetCode;
	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	default:
		pFX->Default(szName, value.GetBuffer(0), plLength,
			SQL_C_CHAR, value.GetLength(), nMaxLength);
		value.ReleaseBuffer();
		return;

	case CFieldExchange::BindParam:
		{
			// Preallocate to nMaxLength and setup binding address
			value.GetBufferSetLength(nMaxLength);
			void* pvParam = value.LockBuffer(); // will be overwritten if UNICODE

#ifdef _UNICODE
			// Must use proxy to translate unicode data into non-unicode param
			pFX->m_prs->m_bRebindParams = TRUE;

			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvParamProxy == NULL)
			{
				pFX->m_prs->m_pvParamProxy = new void*[pFX->m_prs->m_nParams];
				memset(pFX->m_prs->m_pvParamProxy, 0,
					pFX->m_prs->m_nParams*sizeof(void*));
				pFX->m_prs->m_nProxyParams = pFX->m_prs->m_nParams;
			}

			// Allocate non-unicode string to nMaxLength if necessary for SQLBindParameter
			if (pFX->m_prs->m_pvParamProxy[nField-1] == NULL)
			{
				pvParam = new CHAR[nMaxLength+1];
				pFX->m_prs->m_pvParamProxy[nField-1] = pvParam;
			}
			else
				pvParam = pFX->m_prs->m_pvParamProxy[nField-1];

			// Now fill in the data value
			USES_CONVERSION;
			lstrcpyA((char*)pvParam, T2A((LPCTSTR)value));
#endif // _UNICODE

			*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
				SQL_NULL_DATA : SQL_NTS;

			AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt, (UWORD)nField,
				 (SWORD)pFX->m_nFieldType, SQL_C_CHAR, (SWORD)nColumnType,
				 nMaxLength, nScale, pvParam, nMaxLength, plLength));

			value.ReleaseBuffer();

			if (nRetCode != SQL_SUCCESS)
				pFX->m_prs->ThrowDBException(nRetCode, pFX->m_hstmt);

			// Add the member address to the param map
			pFX->m_prs->m_mapParamIndex.SetAt(&value, (void*)nField);
		}
		return;

#ifdef _UNICODE
	case CFieldExchange::RebindParam:
		*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
			SQL_NULL_DATA : SQL_NTS;
		if (pFX->m_prs->m_nProxyParams != 0)
		{
			// Fill buffer (expected by SQLBindParameter) with new param data
			USES_CONVERSION;
			LPSTR lpszParam = (LPSTR)pFX->m_prs->m_pvParamProxy[nField-1];
			lstrcpyA(lpszParam, T2A((LPCTSTR)value));
		}
		return;
#endif // _UNICODE

	case CFieldExchange::BindFieldToColumn:
		{
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];
			UINT cbColumn = pODBCInfo->m_nPrecision;

			switch (pODBCInfo->m_nSQLType)
			{
			default:
#ifdef _DEBUG
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: CString converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
#endif // _DEBUG

				// Add room for extra information like sign, decimal point, etc.
				cbColumn += 10;
				break;

			case SQL_LONGVARCHAR:
			case SQL_CHAR:
			case SQL_VARCHAR:
				break;

			case SQL_FLOAT:
			case SQL_REAL:
			case SQL_DOUBLE:
				// Add room for sign, decimal point and " E +XXX"
				cbColumn += 10;
				break;

			case SQL_DECIMAL:
			case SQL_NUMERIC:
				// Add room for sign and decimal point
				cbColumn += 2;
				break;

			case SQL_TIMESTAMP:
			case SQL_DATE:
			case SQL_TIME:
				// May need extra space, i.e. "{TS mm/dd/yyyy hh:mm:ss}"
				cbColumn += 10;
				break;

			case SQL_TINYINT:
			case SQL_SMALLINT:
			case SQL_INTEGER:
			case SQL_BIGINT:
				// Add room for sign
				cbColumn += 1;
				break;
			}

			// Constrain to user specified max length, subject to 256 byte min
			if (cbColumn > (UINT)nMaxLength || cbColumn < 256)
				cbColumn = nMaxLength;

			// Set up binding addres
			void* pvData;
			value.GetBufferSetLength(cbColumn+1);
			pvData = value.LockBuffer();    // will be overwritten if UNICODE

#ifdef _UNICODE
			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvFieldProxy == NULL)
			{
				pFX->m_prs->m_pvFieldProxy = new void*[pFX->m_prs->m_nFields];
				memset(pFX->m_prs->m_pvFieldProxy, 0,
					pFX->m_prs->m_nFields*sizeof(void*));
				pFX->m_prs->m_nProxyFields = pFX->m_prs->m_nFields;
			}

			// Allocate non-unicode string for SQLBindCol (not necessary on Requery)
			if (pFX->m_prs->m_pvFieldProxy[nField-1] == NULL)
				pFX->m_prs->m_pvFieldProxy[nField-1] = new CHAR[cbColumn+2];

			pvData = pFX->m_prs->m_pvFieldProxy[nField-1];
#endif // _UNICODE

			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField,
				SQL_C_CHAR, pvData, cbColumn+1, plLength));
			value.ReleaseBuffer();
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);

			// Add the member address to the field map
			pFX->m_prs->m_mapFieldIndex.SetAt(&value, (void*)nField);
		}
		return;

#ifdef _UNICODE
	case CFieldExchange::BindFieldForUpdate:
		if (pFX->m_prs->m_nProxyFields != 0)
		{
			// Fill buffer (expected by SQLSetPos) with new field data
			USES_CONVERSION;
			LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
			lstrcpyA(lpszData, T2A((LPCTSTR)value));

			pFX->Default(szName, (void *)lpszData, plLength, SQL_C_CHAR,
				value.GetLength(), nMaxLength);
		}
		return;
#endif // _UNICODE

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value.GetBufferSetLength(0);
			value.ReleaseBuffer();
		}
		else
		{
#ifdef _UNICODE
			// Copy value out of the proxy
			value = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
#endif

			LPTSTR lpsz = value.GetBuffer(0);
			if (pFX->m_prs->m_pDatabase->m_bStripTrailingSpaces)
			{
				// find first trailing space
				LPTSTR lpszFirstTrailing = NULL;
				while (*lpsz != '\0')
				{
					if (*lpsz != ' ')
						lpszFirstTrailing = NULL;
					else
					{
						if (lpszFirstTrailing == NULL)
							lpszFirstTrailing = lpsz;
					}
					lpsz = _tcsinc(lpsz);
				}
				// truncate
				if (lpszFirstTrailing != NULL)
					*lpszFirstTrailing = '\0';

			}
			value.ReleaseBuffer();
			*plLength = value.GetLength();
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				// Set string 0 length
				value.GetBufferSetLength(0);
				value.ReleaseBuffer();
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = SQL_NTS;
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;


#ifdef _UNICODE
	case CFieldExchange::NameValue:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			*pFX->m_pstr += szName;
			*pFX->m_pstr += '=';
		}
		// Fall through

	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			// Get the field data
			CODBCFieldInfo* pODBCInfo =
					&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				USES_CONVERSION;
				lstrcpyA(lpszData, T2A((LPCTSTR)value));
				*plLength = value.GetLength();
			}

			// If optimizing for bulk add, only need lengths & proxy set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*pFX->m_pstr += '?';
				*pFX->m_pstr += pFX->m_lpszSeparator;
				pFX->m_nParamFields++;
				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt,
					(UWORD)pFX->m_nParamFields, SQL_PARAM_INPUT,
					SQL_C_CHAR, pODBCInfo->m_nSQLType, nMaxLength,
					pODBCInfo->m_nScale, lpszData, 0, plLength));
			}
		}
		return;
#endif // _UNICODE

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (!value.IsEmpty())
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value.IsEmpty())
			pFX->m_prs->SetNullFieldStatus(nField - 1);
		else
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		pFX->Default(szName, &value, plLength,
			SQL_C_CHAR, value.GetLength(), nMaxLength);
		return;

	case CFieldExchange::LoadField:
		{
			// Get the field data
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			CString* pStrCachedValue = (CString*)pInfo->m_pvDataCache;

			// Restore the status
			pFX->m_prs->SetFieldStatus(nField - 1, pInfo->m_bStatus);

			// If not NULL, restore the value and length
			if (!pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				value = *pStrCachedValue;
				*plLength = value.GetLength();

#ifdef _UNICODE
				// Must restore proxy for correct WHERE CURRENT OF operation
				USES_CONVERSION;
				LPSTR lpszData = (LPSTR)pFX->m_prs->m_pvFieldProxy[nField-1];
				lstrcpyA(lpszData, T2A((LPCTSTR)value));
#endif // _UNICODE
			}
			else
			{
				*plLength = SQL_NULL_DATA;
			}

#ifdef _DEBUG
			// Buffer address must not change - ODBC's SQLBindCol depends upon this
			void* pvBind;

	#ifdef _UNICODE
			pvBind = pFX->m_prs->m_pvFieldProxy[nField-1];
	#else // !_UNICODE
			pvBind = value.GetBuffer(0);
			value.ReleaseBuffer();
	#endif

			if (pvBind != pInfo->m_pvBindAddress)
			{
				TRACE1("Error: CString buffer (column %u) address has changed!\n",
					nField);
				ASSERT(FALSE);
			}
#endif // _DEBUG
		}
		return;

	case CFieldExchange::StoreField:
		AfxStoreField(*pFX->m_prs, nField, &value);
		return;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new CString;
			pInfo->m_nDataType = AFX_RFX_TEXT;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Int(CFieldExchange* pFX, LPCTSTR szName, int& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_C_SHORT)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: int converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG
		}
		// fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_LONG,
			sizeof(value), 5);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = AFX_RFX_INT_PSEUDO_NULL;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_INT_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(value);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value != AFX_RFX_INT_PSEUDO_NULL)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != AFX_RFX_INT_PSEUDO_NULL)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Data cached by value, no allocation necessary
			pInfo->m_nDataType = AFX_RFX_INT;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Long(CFieldExchange* pFX, LPCTSTR szName, long& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_C_LONG)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: long converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG
		}
		// fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_LONG,
			sizeof(value), 10);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = AFX_RFX_LONG_PSEUDO_NULL;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_LONG_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(value);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value != AFX_RFX_LONG_PSEUDO_NULL)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != AFX_RFX_LONG_PSEUDO_NULL)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Data cached by value, no allocation necessary
			pInfo->m_nDataType = AFX_RFX_LONG;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Byte(CFieldExchange* pFX, LPCTSTR szName, BYTE& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_TINYINT)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: BYTE converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG
		}
		// fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_TINYINT,
			sizeof(value), 3);
		break;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = AFX_RFX_BYTE_PSEUDO_NULL;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_BYTE_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(value);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value != AFX_RFX_BYTE_PSEUDO_NULL)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != AFX_RFX_BYTE_PSEUDO_NULL)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Data cached by value, no allocation necessary
			pInfo->m_nDataType = AFX_RFX_BYTE;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Bool(CFieldExchange* pFX, LPCTSTR szName, BOOL& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_BIT)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: BOOL converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG
		}
		// Fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_BIT,
			sizeof(value), 1);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = AFX_RFX_BOOL_PSEUDO_NULL;
		}
		else
			// Cast BYTE into BOOL (int)
			value = *(BYTE *)&value;
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_BOOL_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(value);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value != AFX_RFX_BOOL_PSEUDO_NULL)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != AFX_RFX_BOOL_PSEUDO_NULL)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Data cached by value, no allocation necessary
			pInfo->m_nDataType = AFX_RFX_BOOL;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

// Note: CByteArray.m_pData must not be changed.  This address is registered
// with ODBC and must remain valid until the recordset is released.
void AFXAPI RFX_Binary(CFieldExchange* pFX, LPCTSTR szName,
	CByteArray& value, int nMaxLength)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	RETCODE nRetCode;
	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);

	BOOL bByRef = FALSE;
	switch (pFX->m_nOperation)
	{
	default:
LDefault:
		{
			void* pvData = NULL;
			if (value.GetSize() > 0)
			{
				if (bByRef)
					pvData = &value;
				else
					pvData = &value[0];
			}

			pFX->Default(szName, pvData, plLength, SQL_C_BINARY,
				(int)value.GetSize(), (UINT)value.GetSize());
		}
		return;

	case CFieldExchange::BindFieldToColumn:
		{
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];
			UDWORD cbColumn = pODBCInfo->m_nPrecision;

#ifdef _DEBUG
			if (pODBCInfo->m_nSQLType != SQL_BINARY &&
				pODBCInfo->m_nSQLType != SQL_VARBINARY &&
				pODBCInfo->m_nSQLType != SQL_LONGVARBINARY)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: CByteArray converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG

			// Constrain to user specified max length
			if (cbColumn > (UINT)nMaxLength)
				cbColumn = nMaxLength;
			value.SetSize(cbColumn);
			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField,
				SQL_C_BINARY, &value[0], (LONG)cbColumn, plLength));
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);

			// Add the member address to the field map
			pFX->m_prs->m_mapFieldIndex.SetAt(&value, (void*)nField);
		}
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value.SetSize(1);
			value[0] = AFX_RFX_BYTE_PSEUDO_NULL;
		}
		else
		{
			ASSERT(*plLength <= (LONG)nMaxLength);
			((CDBByteArray&)value).SetLength((UINT)*plLength);
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value.SetSize(1);
				value[0] = AFX_RFX_BYTE_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = value.GetSize();
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value.GetSize() != 1 || value[0] != AFX_RFX_BYTE_PSEUDO_NULL)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value.GetSize() != 1 || value[0] != AFX_RFX_BYTE_PSEUDO_NULL)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		bByRef = TRUE;
		goto LDefault;

	case CFieldExchange::StoreField:
		AfxStoreField(*pFX->m_prs, nField, &value);
		return;

	case CFieldExchange::LoadField:
		AfxLoadField(*pFX->m_prs, nField, &value, plLength);
		return;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new CByteArray;
			pInfo->m_nDataType = AFX_RFX_BINARY;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << ":";
		value.Dump(*pFX->m_pdcDump);
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName, CTime& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	RETCODE nRetCode;
	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_TIMESTAMP,
			sizeof(value), TIMESTAMP_PRECISION);
		return;

	case CFieldExchange::BindParam:
		{
			TIMESTAMP_STRUCT* pts;
			pFX->m_prs->m_bRebindParams = TRUE;

			if (pFX->m_prs->IsParamStatusNull(nField - 1))
			{
				pts = NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				// Allocate proxy array if necessary
				if (pFX->m_prs->m_pvParamProxy == NULL)
				{
					pFX->m_prs->m_pvParamProxy = new void*[pFX->m_prs->m_nParams];
					memset(pFX->m_prs->m_pvParamProxy, 0,
						pFX->m_prs->m_nParams*sizeof(void*));
					pFX->m_prs->m_nProxyParams = pFX->m_prs->m_nParams;
				}

				// Allocate TIMESTAMP_STRUCT if necessary for SQLBindParameter
				if (pFX->m_prs->m_pvParamProxy[nField-1] == NULL)
				{
					pts = new TIMESTAMP_STRUCT;
					pFX->m_prs->m_pvParamProxy[nField-1] = pts;
				}
				else
					pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvParamProxy[nField-1];

				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}

			AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt, (UWORD)nField,
				(SWORD)pFX->m_nFieldType, SQL_C_TIMESTAMP, SQL_C_TIMESTAMP,
				TIMESTAMP_PRECISION, 0, pts, 0, plLength));
			if (nRetCode != SQL_SUCCESS)
				pFX->m_prs->ThrowDBException(nRetCode, pFX->m_hstmt);

			// Add the member address to the param map
			pFX->m_prs->m_mapParamIndex.SetAt(&value, (void*)nField);
		}
		return;

	case CFieldExchange::RebindParam:
		{
			*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
				SQL_NULL_DATA : sizeof(TIMESTAMP_STRUCT);
			if (pFX->m_prs->m_nProxyParams != 0)
			{
				// Fill buffer (expected by SQLBindParameter) with new param data
				TIMESTAMP_STRUCT* pts;
				pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvParamProxy[nField-1];
				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
			}
		}
		return;

	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_DATE &&
				pODBCInfo->m_nSQLType != SQL_TIME &&
				pODBCInfo->m_nSQLType != SQL_TIMESTAMP)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: CTime converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG

			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvFieldProxy == NULL)
			{
				pFX->m_prs->m_pvFieldProxy = new void*[pFX->m_prs->m_nFields];
				memset(pFX->m_prs->m_pvFieldProxy, 0,
					pFX->m_prs->m_nFields*sizeof(void*));
				pFX->m_prs->m_nProxyFields = pFX->m_prs->m_nFields;
			}

			// Allocate TIMESTAMP_STRUCT for SQLBindCol (not necessary on Requery)
			if (pFX->m_prs->m_pvFieldProxy[nField-1] == NULL)
				pFX->m_prs->m_pvFieldProxy[nField-1] = new TIMESTAMP_STRUCT;

			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField,
				SQL_C_TIMESTAMP, pFX->m_prs->m_pvFieldProxy[nField-1],
				sizeof(TIMESTAMP_STRUCT), plLength));
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);

			// Add the member address to the field map
			pFX->m_prs->m_mapFieldIndex.SetAt(&value, (void*)nField);
		}
		return;

	case CFieldExchange::BindFieldForUpdate:
		if (pFX->m_prs->m_nProxyFields != 0)
		{
			// Fill buffer (expected by SQLSetPos) with new field data
			TIMESTAMP_STRUCT* pts;
			pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvFieldProxy[nField-1];
			pts->year = (SWORD)value.GetYear();
			pts->month = (UWORD)value.GetMonth();
			pts->day = (UWORD)value.GetDay();
			pts->hour = (UWORD)value.GetHour();
			pts->minute = (UWORD)value.GetMinute();
			pts->second = (UWORD)value.GetSecond();
			pts->fraction = 0;

			pFX->Default(szName, (void *)pts, plLength, SQL_C_TIMESTAMP,
				sizeof(TIMESTAMP_STRUCT), TIMESTAMP_PRECISION);
		}
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = AFX_RFX_DATE_PSEUDO_NULL;
		}
		else
		{
			TIMESTAMP_STRUCT* pts =
				(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];
			if (pts->year < 1970 || pts->year > 2038)
			{
				// Time value out of range, return NULL
#ifdef _DEBUG
				if (afxTraceFlags & traceDatabase)
					TRACE0("Warning: date value out of range, returning NULL value.\n");
#endif
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_DATE_PSEUDO_NULL;
			}
			else
			{
#ifdef _DEBUG
				if ((afxTraceFlags & traceDatabase) && pts->fraction != 0)
					TRACE0("Warning: ignoring milliseconds.\n");
#endif
				value = CTime(pts->year, pts->month, pts->day,
					pts->hour, pts->minute, pts->second);
			}
		}
		return;

	case CFieldExchange::NameValue:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			*pFX->m_pstr += szName;
			*pFX->m_pstr += '=';
		}
		// Fall through

	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			TIMESTAMP_STRUCT* pts =
				(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}

			// If optimizing for bulk add, only need lengths & proxy set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*pFX->m_pstr += '?';
				*pFX->m_pstr += pFX->m_lpszSeparator;
				pFX->m_nParamFields++;

				// Assumes all bound fields BEFORE unbound fields
				CODBCFieldInfo* pODBCInfo =
					&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt,
					(UWORD)pFX->m_nParamFields, SQL_PARAM_INPUT,
					SQL_C_TIMESTAMP, pODBCInfo->m_nSQLType,
					TIMESTAMP_PRECISION, 0, pts, 0, plLength));
			}
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = AFX_RFX_DATE_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		{
			// can force writing of psuedo-null value (as a non-null) by setting field dirty
			CTime timeNull = AFX_RFX_DATE_PSEUDO_NULL;
			if (value != timeNull)
			{
				pFX->m_prs->SetDirtyFieldStatus(nField - 1);
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
			}
		}
		return;

	case CFieldExchange::MarkForUpdate:
		{
			CTime timeNull = AFX_RFX_DATE_PSEUDO_NULL;
			if (value != timeNull)
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		goto LDefault;

	case CFieldExchange::LoadField:
		{
			// Get the field data
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Restore the status
			pFX->m_prs->SetFieldStatus(nField - 1, pInfo->m_bStatus);

			// If not NULL, restore the value, length and proxy
			if (!pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				AfxCopyValueByRef(pInfo->m_pvDataCache, &value,
					plLength, pInfo->m_nDataType);

				// Restore proxy for correct WHERE CURRENT OF operations
				TIMESTAMP_STRUCT* pts =
					(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];

				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
			}
			else
				*plLength = SQL_NULL_DATA;

#ifdef _DEBUG
			// Buffer address must not change - ODBC's SQLBindCol depends upon this
			if (pInfo->m_pvBindAddress != pFX->m_prs->m_pvFieldProxy[nField-1])
			{
				TRACE1("Error: CString buffer (column %u) address has changed!\n",
					nField);
				ASSERT(FALSE);
			}
#endif // _DEBUG
		}
		return;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new CTime;
			pInfo->m_nDataType = AFX_RFX_DATE;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = " << value;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName,
	TIMESTAMP_STRUCT& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_DATE &&
				pODBCInfo->m_nSQLType != SQL_TIME &&
				pODBCInfo->m_nSQLType != SQL_TIMESTAMP)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: TIMESTAMP_STRUCT converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG
			// fall through
		}

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_TIMESTAMP,
			sizeof(value), TIMESTAMP_PRECISION);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value.year = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.month = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.day = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.hour = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.minute = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.second = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
			value.fraction = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value.year = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.month = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.day = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.hour = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.minute = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.second = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				value.fraction = AFX_RFX_TIMESTAMP_PSEUDO_NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (!(value.year == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.month == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.day == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.hour == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.minute == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.second == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.fraction == AFX_RFX_TIMESTAMP_PSEUDO_NULL ))
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (!(value.year == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.month == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.day == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.hour == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.minute == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.second == AFX_RFX_TIMESTAMP_PSEUDO_NULL &&
			value.fraction == AFX_RFX_TIMESTAMP_PSEUDO_NULL ))
		{
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new TIMESTAMP_STRUCT;
			pInfo->m_nDataType = AFX_RFX_TIMESTAMP;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << ".year = " << (int)value.year;
		*pFX->m_pdcDump << "\n" << szName << ".month = " << value.month;
		*pFX->m_pdcDump << "\n" << szName << ".day = " << value.day;
		*pFX->m_pdcDump << "\n" << szName << ".hour = " << value.hour;
		*pFX->m_pdcDump << "\n" << szName << ".minute = " << value.minute;
		*pFX->m_pdcDump << "\n" << szName << ".second = " << value.second;
		*pFX->m_pdcDump << "\n" << szName << ".fraction = " << value.fraction;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_Date(CFieldExchange* pFX, LPCTSTR szName,
	COleDateTime& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	RETCODE nRetCode;
	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindParam:
		{
			TIMESTAMP_STRUCT* pts;
			pFX->m_prs->m_bRebindParams = TRUE;

			if (pFX->m_prs->IsParamStatusNull(nField - 1))
			{
				pts = NULL;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				// Allocate proxy array if necessary
				if (pFX->m_prs->m_pvParamProxy == NULL)
				{
					pFX->m_prs->m_pvParamProxy = new void*[pFX->m_prs->m_nParams];
					memset(pFX->m_prs->m_pvParamProxy, 0,
						pFX->m_prs->m_nParams*sizeof(void*));
					pFX->m_prs->m_nProxyParams = pFX->m_prs->m_nParams;
				}

				// Allocate TIMESTAMP_STRUCT if necessary for SQLBindParameter
				if (pFX->m_prs->m_pvParamProxy[nField-1] == NULL)
				{
					pts = new TIMESTAMP_STRUCT;
					pFX->m_prs->m_pvParamProxy[nField-1] = pts;
				}
				else
					pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvParamProxy[nField-1];

				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}

			AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt, (UWORD)nField,
				(SWORD)pFX->m_nFieldType, SQL_C_TIMESTAMP, SQL_C_TIMESTAMP,
				TIMESTAMP_PRECISION, 0, pts, 0, plLength));
			if (nRetCode != SQL_SUCCESS)
				pFX->m_prs->ThrowDBException(nRetCode, pFX->m_hstmt);

			// Add the member address to the param map
			pFX->m_prs->m_mapParamIndex.SetAt(&value, (void*)nField);
		}
		return;

	case CFieldExchange::NameValue:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			*pFX->m_pstr += szName;
			*pFX->m_pstr += '=';
		}
		// Fall through

	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			TIMESTAMP_STRUCT* pts =
				(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}

			// If optimizing for bulk add, only need lengths & proxy set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*pFX->m_pstr += '?';
				*pFX->m_pstr += pFX->m_lpszSeparator;
				pFX->m_nParamFields++;

				// Assumes all bound fields BEFORE unbound fields
				CODBCFieldInfo* pODBCInfo =
					&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt,
					(UWORD)pFX->m_nParamFields, SQL_PARAM_INPUT,
					SQL_C_TIMESTAMP, pODBCInfo->m_nSQLType,
					TIMESTAMP_PRECISION, 0, pts, 0, plLength));
			}
		}
		return;

	case CFieldExchange::RebindParam:
		{
			*plLength = pFX->m_prs->IsParamStatusNull(nField - 1) ?
				SQL_NULL_DATA : sizeof(TIMESTAMP_STRUCT);
			if (pFX->m_prs->m_nProxyParams != 0)
			{
				// Fill buffer (expected by SQLBindParameter) with new param data
				TIMESTAMP_STRUCT* pts;
				pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvParamProxy[nField-1];
				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
			}
		}
		return;

	case CFieldExchange::BindFieldForUpdate:
		if (pFX->m_prs->m_nProxyFields != 0)
		{
			// Fill buffer (expected by SQLSetPos) with new field data
			TIMESTAMP_STRUCT* pts;
			pts = (TIMESTAMP_STRUCT *)pFX->m_prs->m_pvFieldProxy[nField-1];
			pts->year = (SWORD)value.GetYear();
			pts->month = (UWORD)value.GetMonth();
			pts->day = (UWORD)value.GetDay();
			pts->hour = (UWORD)value.GetHour();
			pts->minute = (UWORD)value.GetMinute();
			pts->second = (UWORD)value.GetSecond();
			pts->fraction = 0;

			pFX->Default(szName, (void *)pts, plLength, SQL_C_TIMESTAMP,
				sizeof(TIMESTAMP_STRUCT), TIMESTAMP_PRECISION);
		}
		return;

	case CFieldExchange::LoadField:
		{
			// Get the field data
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Restore the status
			pFX->m_prs->SetFieldStatus(nField - 1, pInfo->m_bStatus);

			// If not NULL, restore the value, length and proxy
			if (!pFX->m_prs->IsFieldStatusNull(nField - 1))
			{
				AfxCopyValueByRef(pInfo->m_pvDataCache, &value,
					plLength, pInfo->m_nDataType);

				// Restore proxy for correct WHERE CURRENT OF operations
				TIMESTAMP_STRUCT* pts =
					(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];

				pts->year = (SWORD)value.GetYear();
				pts->month = (UWORD)value.GetMonth();
				pts->day = (UWORD)value.GetDay();
				pts->hour = (UWORD)value.GetHour();
				pts->minute = (UWORD)value.GetMinute();
				pts->second = (UWORD)value.GetSecond();
				pts->fraction = 0;
			}
			else
				*plLength = SQL_NULL_DATA;

#ifdef _DEBUG
			// Buffer address must not change - ODBC's SQLBindCol depends upon this
			if (pInfo->m_pvBindAddress != pFX->m_prs->m_pvFieldProxy[nField-1])
			{
				TRACE1("Error: CString buffer (column %u) address has changed!\n",
					nField);
				ASSERT(FALSE);
			}
#endif // _DEBUG
		}
		return;

	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_DATE &&
				pODBCInfo->m_nSQLType != SQL_TIME &&
				pODBCInfo->m_nSQLType != SQL_TIMESTAMP)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: COleDateTime converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif // _DEBUG

			// Allocate proxy array if necessary
			if (pFX->m_prs->m_pvFieldProxy == NULL)
			{
				pFX->m_prs->m_pvFieldProxy = new void*[pFX->m_prs->m_nFields];
				memset(pFX->m_prs->m_pvFieldProxy, 0,
					pFX->m_prs->m_nFields*sizeof(void*));
				pFX->m_prs->m_nProxyFields = pFX->m_prs->m_nFields;
			}

			// Allocate TIMESTAMP_STRUCT for SQLBindCol (not necessary on Requery)
			if (pFX->m_prs->m_pvFieldProxy[nField-1] == NULL)
				pFX->m_prs->m_pvFieldProxy[nField-1] = new TIMESTAMP_STRUCT;

			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField,
				SQL_C_TIMESTAMP, pFX->m_prs->m_pvFieldProxy[nField-1],
				sizeof(TIMESTAMP_STRUCT), plLength));
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);

			// Add the member address to the field map
			pFX->m_prs->m_mapFieldIndex.SetAt(&value, (void*)nField);
		}
		return;

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_TIMESTAMP,
			sizeof(value), TIMESTAMP_PRECISION);
		return;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new COleDateTime;
			pInfo->m_nDataType = AFX_RFX_OLEDATE;
		}
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value.SetStatus(COleDateTime::null);
		}
		else
		{
			TIMESTAMP_STRUCT* pts =
				(TIMESTAMP_STRUCT*)pFX->m_prs->m_pvFieldProxy[nField-1];
#ifdef _DEBUG
			if ((afxTraceFlags & traceDatabase) && pts->fraction != 0)
				TRACE0("Warning: ignoring milliseconds.\n");
#endif
			value = COleDateTime(pts->year, pts->month, pts->day,
				pts->hour, pts->minute, pts->second);
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value.SetStatus(COleDateTime::null);
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);
				*plLength = sizeof(TIMESTAMP_STRUCT);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::MarkForAddNew:
		// can force writing of psuedo-null value (as a non-null) by setting field dirty
		if (value.GetStatus() != COleDateTime::null)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value.GetStatus() != COleDateTime::null)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		CString str;
		str = value.Format();
		*pFX->m_pdcDump << "\n" << str;
		return;
#endif // _DEBUG

	}
}

void AFXAPI RFX_LongBinary(CFieldExchange* pFX, LPCTSTR szName,
	CLongBinary& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	RETCODE nRetCode;
	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(
		nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::Name:
		pFX->m_prs->m_bLongBinaryColumns = TRUE;
		pFX->Default(szName, &value, plLength, SQL_C_DEFAULT, 0, 0);
		return;

	case CFieldExchange::BindFieldToColumn:
		// Don't bind if using update SQL, simply do SQLGetData on Fixup
		if (!pFX->m_prs->m_bUseUpdateSQL && pFX->m_prs->CanUpdate())
		{
			// Bind for updates with cb=0 now. Driver may not support post Execute or ExtendedFetch binding
			AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt, (UWORD)nField, SQL_C_DEFAULT,
				&value, 0, plLength));
			if (!pFX->m_prs->Check(nRetCode))
				pFX->m_prs->ThrowDBException(nRetCode);
		}

		// Add the member address to the field map
		pFX->m_prs->m_mapFieldIndex.SetAt(&value, (void*)nField);
		return;

#ifdef _DEBUG
	case CFieldExchange::BindParam:
		// CLongBinary parameters are not supported
		ASSERT(FALSE);

	case CFieldExchange::MarkForAddNew:
	case CFieldExchange::MarkForUpdate:
		// We do not archive LongBinary values
	case CFieldExchange::StoreField:
	case CFieldExchange::LoadField:
		// We do not archive LongBinary values
#endif // _DEBUG
	default:
		return;

	case CFieldExchange::Fixup:
		// Get the size of the long binary field
		*plLength = pFX->GetLongBinarySize(nField);

		// Get the data if necessary
		if (*plLength != SQL_NULL_DATA)
			pFX->GetLongBinaryData(nField, value, plLength);

		// Set the status and length
		if (*plLength == SQL_NULL_DATA)
		{
			// Field NULL, set length and status
			value.m_dwDataLength = 0;
			pFX->m_prs->SetNullFieldStatus(nField - 1);
		}
		else
		{
			// Field not NULL, clear the status (length already set)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}

		return;

	case CFieldExchange::NameValue:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			*pFX->m_pstr += szName;
			*pFX->m_pstr += '=';
		}

		// Fall through
	case CFieldExchange::Value:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			// If user marked column NULL, reflect this in length
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
				*plLength = SQL_NULL_DATA;
			else
			{
				// Indicate data will be sent after SQLExecute
				// Length is signed value, it's limited by LONG_MAX
				if (value.m_dwDataLength >
					(ULONG)(LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET)))
				{
					ASSERT(FALSE);
					*plLength = LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET);
				}
				else
					*plLength = value.m_dwDataLength;

				*plLength = SQL_LEN_DATA_AT_EXEC(*plLength);
			}

			// If optimizing for bulk add, only need lengths set correctly
			if(!(pFX->m_prs->m_dwOptions & CRecordset::optimizeBulkAdd))
			{
				*pFX->m_pstr += '?';
				*pFX->m_pstr += pFX->m_lpszSeparator;
				pFX->m_nParamFields++;

				// Assumes all bound fields BEFORE unbound fields
				CODBCFieldInfo* pODBCInfo =
					&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

				AFX_SQL_SYNC(::SQLBindParameter(pFX->m_hstmt,
					(UWORD)pFX->m_nParamFields, SQL_PARAM_INPUT,
					SQL_C_DEFAULT, pODBCInfo->m_nSQLType,
					value.m_dwDataLength, 0, &value, 0, plLength));
				if (nRetCode != SQL_SUCCESS)
					pFX->m_prs->ThrowDBException(nRetCode, pFX->m_hstmt);
			}
		}
		return;

	case CFieldExchange::BindFieldForUpdate:
		if (pFX->m_prs->IsFieldStatusDirty(nField - 1))
		{
			// If user marked column NULL, reflect this in length
			if (pFX->m_prs->IsFieldStatusNull(nField - 1))
				*plLength = SQL_NULL_DATA;
			else
			{
				// Length is signed value, it's limited by LONG_MAX
				if (value.m_dwDataLength >
					(ULONG)(LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET)))
				{
					ASSERT(FALSE);
					*plLength = LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET);
				}
				else
					*plLength = value.m_dwDataLength;

				*plLength = SQL_LEN_DATA_AT_EXEC(*plLength);
			}
		}
		else
			*plLength = SQL_IGNORE;

		return;

	case CFieldExchange::UnbindFieldForUpdate:
		*plLength = value.m_dwDataLength;
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				// Mark fields null
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value.m_dwDataLength = 0;
				*plLength = SQL_NULL_DATA;
			}
			else
			{
				pFX->m_prs->ClearNullFieldStatus(nField - 1);

				// Length is signed value, it's limited by LONG_MAX
				if (value.m_dwDataLength >
					(ULONG)(LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET)))
				{
					ASSERT(FALSE);
					*plLength = LONG_MAX - labs(SQL_LEN_DATA_AT_EXEC_OFFSET);
				}
				else
					*plLength = value.m_dwDataLength;

				*plLength = SQL_LEN_DATA_AT_EXEC(*plLength);
			}
#ifdef _DEBUG
			pFX->m_nFieldFound = nField;
#endif
		}
		return;

	case CFieldExchange::AllocCache:
		// Caching not supported for long binary
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << szName << " = ";
		value.Dump(*pFX->m_pdcDump);
		return;
#endif // _DEBUG

	}
}

long CFieldExchange::GetLongBinarySize(int nField)
{
	RETCODE nRetCode;
	int nDummy;
	long lSize;

	// Give empty buffer to find size of entire LongBinary
	AFX_ODBC_CALL(::SQLGetData(m_prs->m_hstmt,
		(UWORD)nField, SQL_C_DEFAULT, &nDummy, 0, &lSize));

	switch (nRetCode)
	{
		case SQL_SUCCESS:
			break;

		case SQL_SUCCESS_WITH_INFO:
#ifdef _DEBUG
			if (afxTraceFlags & traceDatabase)
			{
				CDBException* e = new CDBException(nRetCode);
				e->BuildErrorString(m_prs->m_pDatabase,
					m_prs->m_hstmt, FALSE);

				// Ignore data truncated messages
				if (e->m_strStateNativeOrigin.Find(_T("State:01004")) < 0)
				{
					TRACE0("Warning: ODBC Success With Info, ");
					e->TraceErrorMessage(e->m_strError);
					e->TraceErrorMessage(e->m_strStateNativeOrigin);
				}
				e->Delete();
			}
#endif // _DEBUG
			break;

		default:
			m_prs->ThrowDBException(nRetCode);
	}

	return lSize;
}

void CFieldExchange::GetLongBinaryData(int nField, CLongBinary& lb,
	long* plSize)
{
	RETCODE nRetCode;
	long lActualDataSize = 0;
	long lChunkDataSize;
	long lReallocSize;
	const BYTE* lpLongBinary;

	lb.m_dwDataLength = 0;

	// Determine initial chunk sizes
	if (*plSize == SQL_NO_TOTAL)
	{
		lChunkDataSize = m_lDefaultLBFetchSize;
		lReallocSize = m_lDefaultLBReallocSize;
	}
	else
	{
		lChunkDataSize = *plSize;
		lReallocSize = *plSize;
	}

	do
	{
		// Check if CLongBianary is big enough
		lpLongBinary = ReallocLongBinary(lb,
			(long)lb.m_dwDataLength + lChunkDataSize,
			(long)lb.m_dwDataLength + lReallocSize);

		// Adjust the pointer so that data added at end
		lpLongBinary += lb.m_dwDataLength;

		AFX_ODBC_CALL(::SQLGetData(m_prs->m_hstmt, (UWORD)nField,
			SQL_C_BINARY, (UCHAR*)lpLongBinary, lChunkDataSize, &lActualDataSize));
		::GlobalUnlock(lb.m_hData);

		switch (nRetCode)
		{
			case SQL_NO_DATA_FOUND:
				m_prs->SetNullFieldStatus(nField - 1);
				*plSize = SQL_NULL_DATA;
				break;

			case SQL_SUCCESS:
				// All data fetched
				lb.m_dwDataLength += lActualDataSize;
				*plSize = (long)lb.m_dwDataLength;
				return;

			case SQL_SUCCESS_WITH_INFO:
				{
					CDBException* e = new CDBException(nRetCode);
					e->BuildErrorString(m_prs->m_pDatabase, m_prs->m_hstmt,
						FALSE);

					// Ignore data truncated messages
					if (e->m_strStateNativeOrigin.Find(_T("State:01004")) < 0)
					{
#ifdef _DEBUG
						if (afxTraceFlags & traceDatabase)
						{
							TRACE0("Warning: ODBC Success With Info, ");
							e->TraceErrorMessage(e->m_strError);
							e->TraceErrorMessage(e->m_strStateNativeOrigin);
						}
#endif // _DEBUG

						// Must be some other warning, should be finished
						lb.m_dwDataLength += lActualDataSize;
					}
					else
					{
						// Should only happen if further calls to SQLGetData necessary

						// Increment the length by the chunk size for subsequent SQLGetData call
						lb.m_dwDataLength += lChunkDataSize;

						// Recalculate chunk and alloc sizes
						lChunkDataSize = m_prs->GetLBFetchSize(lChunkDataSize);
						lReallocSize = m_prs->GetLBReallocSize(lReallocSize);
						// force loop to repeat
						lActualDataSize = SQL_NO_TOTAL;
					}

					*plSize = (long)lb.m_dwDataLength;
					e->Delete();
				}
				break;

			default:
				m_prs->ThrowDBException(nRetCode);
		}

	} while (lActualDataSize == SQL_NO_TOTAL);
}

BYTE* CFieldExchange::ReallocLongBinary(CLongBinary& lb, long lSizeRequired,
	long lReallocSize)
{
	// realloc max of lSizeRequired, lReallocSize (m_dwDataLength untouched)

	if (lSizeRequired < 0)
	{
		ASSERT(FALSE);
		lSizeRequired = 0;
	}

	HGLOBAL hOldData = NULL;

	// Allocate or Realloc as req'd
	if (lb.m_hData == NULL)
		lb.m_hData = ::GlobalAlloc(GMEM_MOVEABLE, lReallocSize);
	else
	{
		DWORD dwSize = ::GlobalSize(lb.m_hData);
		if (dwSize < (DWORD)lSizeRequired)
		{
			// Save the old handle in case ReAlloc fails
			hOldData = lb.m_hData;

			// Allocate more memory if necessary
			lb.m_hData = ::GlobalReAlloc(lb.m_hData,
				__max(lSizeRequired, lReallocSize), GMEM_MOVEABLE);
		}
	}

	// Validate the memory was allocated and can be locked
	if (lb.m_hData == NULL)
	{
		// Restore the old handle (not NULL if Realloc failed)
		lb.m_hData = hOldData;
		AfxThrowMemoryException();
	}

	BYTE* lpLongBinary = (BYTE*)::GlobalLock(lb.m_hData);
	if (lpLongBinary == NULL)
	{
		::GlobalFree(lb.m_hData);
		lb.m_hData = NULL;
		AfxThrowMemoryException();
	}

	return lpLongBinary;
}

//////////////////////////////////////////////////////////////////////////////
// Recordset Field Exchange Helpers


void AFXAPI AfxStoreField(CRecordset& rs, UINT nField, void* pvField)
{
	// Get the field data
	CFieldInfo* pInfo = &rs.m_rgFieldInfos[nField - 1];

	// Cache the status
	pInfo->m_bStatus = rs.GetFieldStatus(nField - 1);

	// Save the data
	if (!rs.IsFieldStatusNull(nField - 1))
	{
		// Don't need to save length for variable len
		// objects as CString and CByteArray track len internally
		long nDummyLength;

		if (pInfo->m_nDataType == AFX_RFX_BOOL ||
			pInfo->m_nDataType == AFX_RFX_BYTE ||
			pInfo->m_nDataType == AFX_RFX_INT ||
			pInfo->m_nDataType == AFX_RFX_LONG ||
			pInfo->m_nDataType == AFX_RFX_SINGLE)
		{
			// If caching data by value, pass a ref
			AfxCopyValueByRef(pvField, &pInfo->m_pvDataCache,
				&nDummyLength, pInfo->m_nDataType);
		}
		else
		{
			AfxCopyValueByRef(pvField, pInfo->m_pvDataCache,
				&nDummyLength, pInfo->m_nDataType);
		}
	}

#ifdef _DEBUG
	// Cache the bind address expected by ODBC
	switch(pInfo->m_nDataType)
	{
	default:
		// All types that are bound directly
		pInfo->m_pvBindAddress = pvField;
		break;

	case AFX_RFX_TEXT:
#ifdef _UNICODE
		pInfo->m_pvBindAddress = rs.m_pvFieldProxy[nField-1];
#else
		pInfo->m_pvBindAddress = ((CString*)pvField)->GetBuffer(0);
		((CString*)pvField)->ReleaseBuffer();
#endif
		break;

	case AFX_RFX_LPTSTR:
#ifdef _UNICODE
		pInfo->m_pvBindAddress = rs.m_pvFieldProxy[nField-1];
#else
		pInfo->m_pvBindAddress = pvField;
#endif
		break;

	case AFX_RFX_DATE:
	case AFX_RFX_OLEDATE:
		pInfo->m_pvBindAddress = rs.m_pvFieldProxy[nField-1];
		break;

	case AFX_RFX_BINARY:
		pInfo->m_pvBindAddress = ((CByteArray*)pvField)->GetData();
		break;
	}
#endif
}

void AFXAPI AfxLoadField(CRecordset& rs, UINT nField,
	void* pvField, long* plLength)
{
	// Get the field data
	CFieldInfo* pInfo = &rs.m_rgFieldInfos[nField - 1];

	// Assumes old field status cleared out
	rs.SetFieldStatus(nField - 1, pInfo->m_bStatus);

	// If not NULL, restore the value and the length
	if (!rs.IsFieldStatusNull(nField - 1))
	{
		if (pInfo->m_nDataType == AFX_RFX_BOOL ||
			pInfo->m_nDataType == AFX_RFX_BYTE ||
			pInfo->m_nDataType == AFX_RFX_INT ||
			pInfo->m_nDataType == AFX_RFX_LONG ||
			pInfo->m_nDataType == AFX_RFX_SINGLE)
		{
			// If caching data by value, pass a ref
			AfxCopyValueByRef(&pInfo->m_pvDataCache, pvField,
				plLength, pInfo->m_nDataType);
		}
		else
		{
			AfxCopyValueByRef(pInfo->m_pvDataCache, pvField,
				plLength, pInfo->m_nDataType);
		}
	}
	else
		*plLength = SQL_NULL_DATA;

#ifdef _DEBUG
	// Buffer address must not change - ODBC's SQLBindCol depends upon this
	if (pInfo->m_nDataType == AFX_RFX_BINARY)
	{
		// Change pvField to point to the data of the CByteArray
		pvField = ((CByteArray*)pvField)->GetData();
	}

	if (pInfo->m_pvBindAddress != pvField)
	{
		TRACE1("Error: field address (column %u) has changed!\n",
			nField);
		ASSERT(FALSE);
	}
#endif // _DEBUG
}

BOOL AFXAPI AfxCompareValueByRef(void* pvSrc, void* pvDest, int nSrcType)
{
	ASSERT(pvSrc != NULL);
	ASSERT(pvDest != NULL);

	BOOL bCompare = FALSE;

	switch(nSrcType)
	{

	case AFX_RFX_LONGBINARY:
		// Caching long binary Src not supported
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_LPTSTR:
		if (lstrcmp((LPTSTR) pvDest, (LPTSTR) pvSrc) == 0)
			bCompare = TRUE;
		break;

	case AFX_RFX_TEXT:
		if (*(CString*)pvDest == *(CString*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_BINARY:
		{
			CByteArray* pByteArraySrc = (CByteArray*)pvSrc;
			CByteArray* pByteArrayDest = (CByteArray*)pvDest;

			// If sizes compare, compare the Src
			int nSize = pByteArraySrc->GetSize();
			if (nSize == pByteArrayDest->GetSize())
			{
				if (memcmp(&pByteArrayDest[0], &pByteArraySrc[0], nSize) == 0)
					bCompare = TRUE;
			}
		}
		break;
	case AFX_RFX_BOOL:
		if (*(BOOL*)pvDest == *(BOOL*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_BYTE:
		if (*(BYTE*)pvDest == *(BYTE*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_INT:
		if (*(int*)pvDest == *(int*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_LONG:
		if (*(long*)pvDest == *(long*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_SINGLE:
		if (*(float*)pvDest == *(float*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_DOUBLE:
		if (*(double*)pvDest == *(double*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_OLEDATE:
		if (*(COleDateTime*)pvDest == *(COleDateTime*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_DATE:
		if (*(CTime*)pvDest == *(CTime*)pvSrc)
			bCompare = TRUE;
		break;

	case AFX_RFX_TIMESTAMP:
		{
			TIMESTAMP_STRUCT* pSrc = (TIMESTAMP_STRUCT*)pvSrc;
			TIMESTAMP_STRUCT* pDest = (TIMESTAMP_STRUCT*)pvDest;
			if ((pSrc->year == pDest->year &&
				pSrc->month == pDest->month &&
				pSrc->day == pDest->day &&
				pSrc->hour == pDest->hour &&
				pSrc->minute == pDest->minute &&
				pSrc->second == pDest->second &&
				pSrc->fraction == pDest->fraction))
			{
				bCompare = TRUE;
			}
		}
		break;
	}

	return bCompare;
}

void AFXAPI AfxCopyValueByRef(void* pvSrc, void* pvDest, long* plLength, int nDestType)
{
	ASSERT(pvSrc != NULL);
	ASSERT(pvDest != NULL);
	ASSERT(plLength != NULL);

	switch (nDestType)
	{
	case AFX_RFX_LONGBINARY:
		// Caching long binary Dest not supported
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_LPTSTR:
		lstrcpy((LPTSTR) pvDest, (LPTSTR) pvSrc);
		*plLength = lstrlen((LPTSTR) pvDest);
		break;

	case AFX_RFX_TEXT:
		*(CString*)pvDest = *(CString*)pvSrc;
		*plLength = ((CString*)pvSrc)->GetLength();
		break;

	case AFX_RFX_BINARY:
		((CByteArray*)pvDest)->Copy(*(CByteArray*)pvSrc);
		*plLength = ((CByteArray*)pvSrc)->GetSize();
		break;

	case AFX_RFX_BOOL:
		*(BOOL*)pvDest = *(BOOL*)pvSrc;
		*plLength = sizeof(BOOL);
		break;

	case AFX_RFX_BYTE:
		*(BYTE*)pvDest = *(BYTE*)pvSrc;
		*plLength = sizeof(BYTE);
		break;

	case AFX_RFX_INT:
		*(int*)pvDest = *(int*)pvSrc;
		*plLength = sizeof(int);
		break;

	case AFX_RFX_LONG:
		*(long*)pvDest = *(long*)pvSrc;
		*plLength = sizeof(long);
		break;

	case AFX_RFX_SINGLE:
		*(float*)pvDest = *(float*)pvSrc;
		*plLength = sizeof(float);
		break;

	case AFX_RFX_DOUBLE:
		*(double*)pvDest = *(double*)pvSrc;
		*plLength = sizeof(double);
		break;

	case AFX_RFX_DATE:
		*(CTime*)pvDest = *(CTime*)pvSrc;
		*plLength = sizeof(TIMESTAMP_STRUCT);
		break;

	case AFX_RFX_OLEDATE:
		*(COleDateTime*)pvDest = *(COleDateTime*)pvSrc;
		*plLength = sizeof(TIMESTAMP_STRUCT);
		break;

	case AFX_RFX_TIMESTAMP:
		{
			TIMESTAMP_STRUCT* pDest = (TIMESTAMP_STRUCT*)pvDest;
			TIMESTAMP_STRUCT* pSrc = (TIMESTAMP_STRUCT*)pvSrc;

			pDest->year = pSrc->year;
			pDest->month = pSrc->month;
			pDest->day = pSrc->day;
			pDest->hour = pSrc->hour;
			pDest->minute = pSrc->minute;
			pDest->second = pSrc->second;
			pDest->fraction = pSrc->fraction;

			*plLength = sizeof(TIMESTAMP_STRUCT);
		}
		break;
	}
}

//////////////////////////////////////////////////////////////////////////////
// Bulk Recordset Field Exchange

void AFXAPI RFX_Text_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	LPSTR* prgStrVals, long** prgLengths, int nMaxLength)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgStrVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgStrVals = new char[nRowsetSize * nMaxLength];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgStrVals;
		*prgStrVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgStrVals, *prgLengths,
			SQL_C_CHAR, nMaxLength);
		break;
	}
}

void AFXAPI RFX_Bool_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BOOL** prgBoolVals, long** prgLengths)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgBoolVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgBoolVals = new BOOL[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgBoolVals;
		*prgBoolVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgBoolVals, *prgLengths,
			SQL_C_LONG, sizeof(BOOL));
		break;
	}
}

void AFXAPI RFX_Int_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	int** prgIntVals, long** prgLengths)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgIntVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgIntVals = new int[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgIntVals;
		*prgIntVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgIntVals, *prgLengths,
			SQL_C_LONG, sizeof(int));
		break;
	}
}

void AFXAPI RFX_Long_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	long** prgLongVals, long** prgLengths)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgLongVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgLongVals = new long[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgLongVals;
		*prgLongVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgLongVals, *prgLengths,
			SQL_C_LONG, sizeof(long));
		break;
	}
}

void AFXAPI RFX_Date_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	TIMESTAMP_STRUCT** prgTSVals, long** prgLengths)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgTSVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgTSVals = new TIMESTAMP_STRUCT[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgTSVals;
		*prgTSVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgTSVals, *prgLengths,
			SQL_C_TIMESTAMP, sizeof(TIMESTAMP_STRUCT));
		break;
	}
}

void AFXAPI RFX_Byte_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BYTE** prgByteVals, long** prgLengths)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgByteVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgByteVals = new BYTE[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgByteVals;
		*prgByteVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgByteVals, *prgLengths,
			SQL_C_TINYINT, sizeof(BYTE));
		break;
	}
}

void AFXAPI RFX_Binary_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	BYTE** prgByteVals, long** prgLengths, int nMaxLength)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	switch (pFX->m_nOperation)
	{
	case CFieldExchange::AllocMultiRowBuffer:
		{
			// The buffer pointer better be initialized to NULL
			// or cleanup in exceptional cases mail fail
			ASSERT(*prgByteVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgByteVals = new BYTE[nRowsetSize * nMaxLength];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgByteVals;
		*prgByteVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgByteVals, *prgLengths,
			SQL_C_BINARY, nMaxLength);
		break;
	}
}

void AFXAPI AfxRFXBulkDefault(CFieldExchange* pFX, LPCTSTR szName,
	void* pv, long* rgLengths, int nCType, UINT cbValue)
{
	RETCODE nRetCode;

	switch(pFX->m_nOperation)
	{
	default:
		// Operation not valid for bulk fetch
		ASSERT(FALSE);
		return;

	case CFieldExchange::Name:
		// We require a name
		ASSERT(lstrlen(szName) != 0);

		*pFX->m_pstr += szName;
		*pFX->m_pstr += pFX->m_lpszSeparator;
		break;

	case CFieldExchange::BindFieldToColumn:
		AFX_SQL_SYNC(::SQLBindCol(pFX->m_prs->m_hstmt,
			(UWORD)pFX->m_nFields, (SWORD)nCType, pv, cbValue, rgLengths));
		if (!pFX->m_prs->Check(nRetCode))
			pFX->m_prs->ThrowDBException(nRetCode);
		break;
	}
}

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxDbInl[] = "afxdb.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDbInl
#define _AFXDBRFX_INLINE
#include "afxdb.inl"

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
