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
#include "math.h"

#ifdef AFX_DB_SEG
#pragma code_seg(AFX_DB_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

//////////////////////////////////////////////////////////////////////////
// Helpers

// Helpers for floating point operations
AFX_STATIC_DATA const float _afxFloatPseudoNull = AFX_RFX_SINGLE_PSEUDO_NULL;
AFX_STATIC_DATA const double _afxDoublePseudoNull = AFX_RFX_DOUBLE_PSEUDO_NULL;

// Long binary allocation helper
void AFX_CDECL AllocLongBinary(CLongBinary& lb, DWORD dwDataLength);

// Memory allocation callbacks used by ICDAOGetRows
STDAPI DaoStringAllocCallback(DWORD dwLen, DWORD pData, void** ppv);
STDAPI DaoBinaryAllocCallback(DWORD dwLen, DWORD pData, void** ppv);
STDAPI DaoLongBinaryAllocCallback(DWORD dwLen, DWORD pData, void** ppv);

//////////////////////////////////////////////////////////////////////////
// CDaoFieldExchange

CDaoFieldExchange::CDaoFieldExchange(UINT nOperation,
	CDaoRecordset* prs, void* pvField)
{
	ASSERT(nOperation < MaxDFXOperation);

	m_nFieldType = none;
	m_nOperation = nOperation;
	m_prs = prs;
	m_pvField = pvField;
	m_nField = 0;
	m_nParam = 0;
}

BOOL CDaoFieldExchange::IsValidOperation()
{
	if (m_nOperation >= MaxDFXOperation)
	{
		// Invalid operation
		ASSERT(FALSE);
		return FALSE;
	}

	// Operations valid for both field types
#ifdef _DEBUG
	if (m_nOperation == DumpField || m_nOperation == SetFieldNull)
		return TRUE;
#endif

	// Operations only valid for outputColumn OR param types
	if ((m_nOperation == AddToParameterList) ||
		(m_nOperation == BindParam))
		return (m_nFieldType == param);
	else
		return (m_nFieldType == outputColumn);
}

void CDaoFieldExchange::AppendParamType(CString& strParamList,
	DWORD dwParamType)
{
	switch (dwParamType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		strParamList += " Text";
		break;

	case AFX_RFX_BINARY:
		strParamList += " LongBinary";
		break;

	case AFX_RFX_LONGBINARY:
		strParamList += " LongBinary";
		break;

	case AFX_RFX_BOOL:
		strParamList += " Bit";
		break;

	case AFX_RFX_BYTE:
		strParamList += " Byte";
		break;

	case AFX_RFX_SHORT:
		strParamList += " Short";
		break;

	case AFX_RFX_LONG:
		strParamList += " Long";
		break;

	case AFX_RFX_CURRENCY:
		strParamList += " Currency";
		break;

	case AFX_RFX_SINGLE:
		strParamList += " IEEESingle";
		break;

	case AFX_RFX_DOUBLE:
		strParamList += " IEEEDouble";
		break;

	case AFX_RFX_DATE:
		strParamList += " DateTime";
		break;
	}
}

CDaoFieldCache* CDaoFieldExchange::GetCacheValue(CDaoRecordset* prs, void* pv)
{
	// Lookup storage locations
	void* pvCache;
	if (!prs->m_pMapFieldCache->Lookup(pv, pvCache))
		AfxThrowDaoException(AFX_DAO_ERROR_DFX_BIND);
	return (CDaoFieldCache*)pvCache;
}

void CDaoFieldExchange::SetNullValue(void* pv, DWORD dwDataType)
{
	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		((CString*)pv)->Empty();
		break;

	case AFX_RFX_BINARY:
		((CByteArray*)pv)->SetSize(0);
		break;

	case AFX_RFX_LONGBINARY:
		((CLongBinary*)pv)->m_dwDataLength = 0;
		break;

	case AFX_RFX_BOOL:
		*(BOOL*)pv = AFX_RFX_BOOL_PSEUDO_NULL;
		break;

	case AFX_RFX_BYTE:
		*(BYTE*)pv = AFX_RFX_BYTE_PSEUDO_NULL;
		break;

	case AFX_RFX_SHORT:
		*(short*)pv = AFX_RFX_SHORT_PSEUDO_NULL;
		break;

	case AFX_RFX_LONG:
		*(long*)pv = AFX_RFX_LONG_PSEUDO_NULL;
		break;

	case AFX_RFX_CURRENCY:
		((COleCurrency*)pv)->SetStatus(COleCurrency::null);
		break;

	case AFX_RFX_SINGLE:
		*(float*)pv = _afxFloatPseudoNull;
		break;

	case AFX_RFX_DOUBLE:
		*(double*)pv = _afxDoublePseudoNull;
		break;

	case AFX_RFX_DATE:
		((COleDateTime*)pv)->SetStatus(COleDateTime::null);
		break;
	}
}

BOOL CDaoFieldExchange::IsNullValue(void* pv, DWORD dwDataType)
{
	BOOL bNull = FALSE;

	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		if (((CString*)pv)->IsEmpty())
			bNull = TRUE;
		break;

	case AFX_RFX_BINARY:
		if (((CByteArray*)pv)->GetSize() == 0)
			bNull = TRUE;
		break;

	case AFX_RFX_LONGBINARY:
		if (((CLongBinary*)pv)->m_dwDataLength == 0)
			bNull = TRUE;
		break;

	case AFX_RFX_BOOL:
		if (*(BOOL*)pv == AFX_RFX_BOOL_PSEUDO_NULL)
			bNull = TRUE;
		break;

	case AFX_RFX_BYTE:
		if (*(BYTE*)pv == AFX_RFX_BYTE_PSEUDO_NULL)
			bNull = TRUE;
		break;

	case AFX_RFX_SHORT:
		if (*(short*)pv == AFX_RFX_SHORT_PSEUDO_NULL)
			bNull = TRUE;
		break;

	case AFX_RFX_LONG:
		if (*(long*)pv == AFX_RFX_LONG_PSEUDO_NULL)
			bNull = TRUE;
		break;

	case AFX_RFX_CURRENCY:
		if (((COleCurrency*)pv)->GetStatus() == COleCurrency::null)
			bNull = TRUE;
		break;

	case AFX_RFX_SINGLE:
		if (*(float*)pv == _afxFloatPseudoNull)
			bNull = TRUE;
		break;

	case AFX_RFX_DOUBLE:
		if (*(double*)pv == _afxDoublePseudoNull)
			bNull = TRUE;
		break;

	case AFX_RFX_DATE:
		if (((COleDateTime*)pv)->GetStatus() == COleDateTime::null)
			bNull = TRUE;
		break;
	}

	return bNull;
}

void CDaoFieldExchange::AllocCacheValue(CDaoFieldCache*& pCache,
	DWORD dwDataType)
{
	// Initialize a new field cache
	pCache = new CDaoFieldCache;
	pCache->m_nStatus = 0;

	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		pCache->m_pvData = new CString();
		pCache->m_nDataType = AFX_RFX_TEXT;
		break;

	case AFX_RFX_BINARY:
		pCache->m_pvData = new CByteArray();
		pCache->m_nDataType = AFX_RFX_BINARY;
		break;

	case AFX_RFX_LONGBINARY:
		pCache->m_pvData = new CLongBinary();
		pCache->m_nDataType = AFX_RFX_LONGBINARY;
		break;

	case AFX_RFX_BOOL:
		pCache->m_nDataType = AFX_RFX_BOOL;
		break;

	case AFX_RFX_BYTE:
		pCache->m_nDataType = AFX_RFX_BYTE;
		break;

	case AFX_RFX_SHORT:
		pCache->m_nDataType = AFX_RFX_SHORT;
		break;

	case AFX_RFX_LONG:
		pCache->m_nDataType = AFX_RFX_LONG;
		break;

	case AFX_RFX_CURRENCY:
		pCache->m_pvData = new COleCurrency();
		pCache->m_nDataType = AFX_RFX_CURRENCY;
		break;

	case AFX_RFX_SINGLE:
		pCache->m_nDataType = AFX_RFX_SINGLE;
		break;

	case AFX_RFX_DOUBLE:
		pCache->m_pvData = new double;
		pCache->m_nDataType = AFX_RFX_DOUBLE;
		break;

	case AFX_RFX_DATE:
		pCache->m_pvData = new COleDateTime();
		pCache->m_nDataType = AFX_RFX_DATE;
		break;
	}
}

void CDaoFieldExchange::DeleteCacheValue(CDaoFieldCache* pCache,
	DWORD dwDataType)
{
	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		delete (CString*)pCache->m_pvData;
		break;

	case AFX_RFX_BINARY:
		delete (CByteArray*)pCache->m_pvData;
		break;

	case AFX_RFX_LONGBINARY:
		delete (CLongBinary*)pCache->m_pvData;
		break;

	case AFX_RFX_BOOL:
	case AFX_RFX_BYTE:
	case AFX_RFX_SHORT:
	case AFX_RFX_LONG:
	case AFX_RFX_SINGLE:
		break;

	case AFX_RFX_CURRENCY:
		delete (COleCurrency*)pCache->m_pvData;
		break;

	case AFX_RFX_DOUBLE:
		delete (double*)pCache->m_pvData;
		break;

	case AFX_RFX_DATE:
		delete (COleDateTime*)pCache->m_pvData;
		break;
	}

	delete pCache;
	pCache = NULL;
}

void CDaoFieldExchange::CopyValue(void* pvSrc, void* pvDest,
	DWORD dwDataType)
{
	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		*(CString*)pvDest = *(CString*)pvSrc;
		break;

	case AFX_RFX_BINARY:
		((CByteArray*)pvDest)->Copy(*(CByteArray*)pvSrc);
		break;

	case AFX_RFX_LONGBINARY:
		{
			CLongBinary* pLongBinarySrc = (CLongBinary*)pvSrc;
			CLongBinary* pLongBinaryDest = (CLongBinary*)pvDest;

			// Reallocate memory in destination if necessary
			AllocLongBinary(*pLongBinaryDest, pLongBinarySrc->m_dwDataLength);
			pLongBinaryDest->m_dwDataLength =
				pLongBinarySrc->m_dwDataLength;

			BYTE* pbSrc = (BYTE*)::GlobalLock(pLongBinarySrc->m_hData);
			BYTE* pbDest = (BYTE*)::GlobalLock(pLongBinaryDest->m_hData);
			memcpy(pbDest, pbSrc, pLongBinarySrc->m_dwDataLength);
			::GlobalUnlock(pLongBinarySrc->m_hData);
			::GlobalUnlock(pLongBinaryDest->m_hData);
		}
		break;

	case AFX_RFX_BOOL:
		*(BOOL*)pvDest = *(BOOL*)pvSrc;
		break;

	case AFX_RFX_BYTE:
		*(BYTE*)pvDest = *(BYTE*)pvSrc;
		break;

	case AFX_RFX_SHORT:
		*(short*)pvDest = *(short*)pvSrc;
		break;

	case AFX_RFX_LONG:
		*(long*)pvDest = *(long*)pvSrc;
		break;

	case AFX_RFX_CURRENCY:
		*(COleCurrency*)pvDest = *(COleCurrency*)pvSrc;
		break;

	case AFX_RFX_SINGLE:
		*(float*)pvDest = *(float*)pvSrc;
		break;

	case AFX_RFX_DOUBLE:
		*(double*)pvDest = *(double*)pvSrc;
		break;

	case AFX_RFX_DATE:
		*(COleDateTime*)pvDest = *(COleDateTime*)pvSrc;
		break;
	}
}

BOOL CDaoFieldExchange::CompareValue(void* pvSrc, void* pvDest,
	DWORD dwDataType)
{
	BOOL bDirty = FALSE;

	switch (dwDataType)
	{
	default:
		ASSERT(FALSE);
		break;

	case AFX_RFX_TEXT:
		if (*(CString*)pvDest != *(CString*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_BINARY:
		{
			CByteArray* pByteArraySrc = (CByteArray*)pvSrc;
			CByteArray* pByteArrayDest = (CByteArray*)pvDest;

			int nSize = pByteArraySrc->GetSize();
			// If sizes don't compare, must be dirty
			if (nSize != pByteArrayDest->GetSize())
				bDirty = TRUE;
			else
			{
				// If sizes compare, compare the data
				if (memcmp(&pByteArrayDest[0], &pByteArraySrc[0], nSize) != 0)
					bDirty = TRUE;
			}
		}
		break;

	case AFX_RFX_LONGBINARY:
		{
			CLongBinary* pLongBinarySrc = (CLongBinary*)pvSrc;
			CLongBinary* pLongBinaryDest = (CLongBinary*)pvDest;

			BYTE* pbSrc = (BYTE*)::GlobalLock(pLongBinarySrc->m_hData);
			BYTE* pbDest = (BYTE*)::GlobalLock(pLongBinaryDest->m_hData);

			// If sizes don't compare, must be dirty
			if (pLongBinarySrc->m_dwDataLength !=
				pLongBinaryDest->m_dwDataLength)
			{
				bDirty = TRUE;
			}
			else
			{
				// If sizes compare, compare the data
				if (memcmp(pbDest, pbSrc, pLongBinarySrc->m_dwDataLength) != 0)
					bDirty = TRUE;
			}

			::GlobalUnlock(pLongBinarySrc->m_hData);
			::GlobalUnlock(pLongBinaryDest->m_hData);
		}
		break;

	case AFX_RFX_BOOL:
		if (*(BOOL*)pvDest != *(BOOL*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_BYTE:
		if (*(BYTE*)pvDest != *(BYTE*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_SHORT:
		if (*(short*)pvDest != *(short*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_LONG:
		if (*(long*)pvDest != *(long*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_CURRENCY:
		if (*(COleCurrency*)pvDest != *(COleCurrency*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_SINGLE:
		if (*(float*)pvDest != *(float*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_DOUBLE:
		if (*(double*)pvDest != *(double*)pvSrc)
			bDirty = TRUE;
		break;

	case AFX_RFX_DATE:
		if (*(COleDateTime*)pvDest != *(COleDateTime*)pvSrc)
			bDirty = TRUE;
		break;
	}

	return bDirty;
}

void CDaoFieldExchange::FillVariant(void* pvValue, DWORD dwDataType,
	COleVariant** ppVar)
{
	COleVariant* pVar;

	if (dwDataType == AFX_RFX_TEXT)
		pVar = new COleVariant(*(CString*)pvValue, VT_BSTRT);
	else if (dwDataType == AFX_RFX_BOOL)
		pVar = new COleVariant(*(long*)pvValue, VT_BOOL);
	else
	{
		pVar = new COleVariant;

		switch (dwDataType)
		{
		default:
			ASSERT(FALSE);
			break;

		case AFX_RFX_BINARY:
			*pVar = *(CByteArray*)pvValue;
			break;

		case AFX_RFX_LONGBINARY:
			*pVar = *(CLongBinary*)pvValue;
			break;

		case AFX_RFX_BYTE:
			*pVar = *(BYTE*)pvValue;
			break;

		case AFX_RFX_SHORT:
			*pVar = *(short*)pvValue;
			break;

		case AFX_RFX_LONG:
			*pVar = *(long*)pvValue;
			break;

		case AFX_RFX_CURRENCY:
			*pVar = *(COleCurrency*)pvValue;
			break;

		case AFX_RFX_SINGLE:
			*pVar = *(float*)pvValue;
			break;

		case AFX_RFX_DOUBLE:
			*pVar = *(double*)pvValue;
			break;

		case AFX_RFX_DATE:
			*pVar = *(COleDateTime*)pvValue;
		}
	}

	*ppVar = pVar;
}

// Default implementation for RFX functions
void CDaoFieldExchange::Default(LPCTSTR lpszName, void* pv,
	DWORD dwColumnType, DWORD dwBindOptions)
{
	switch (m_nOperation)
	{
	case AddToParameterList:
		if (m_nParam != 1)
			m_prs->m_strSQL += ",";
		m_prs->m_strSQL += lpszName;
		AppendParamType(m_prs->m_strSQL, dwColumnType);
		return;

	case AddToSelectList:
		if (m_nField != 1)
			m_prs->m_strSQL += ",";
		m_prs->m_strSQL += lpszName;
		return;

	case BindField:
		{
			// Query parser needs "[" & "]", GetRows can't tolerate them.
			LPTSTR lpszNoBracketName = new TCHAR[lstrlen(lpszName) + 1];
			m_prs->StripBrackets(lpszName, lpszNoBracketName);

			// Finish setting up column binding info struct
			LPDAOCOLUMNBINDING pcb =
				&m_prs->m_prgDaoColBindInfo[m_nField-1];
			pcb->cbInfoOffset =
				(DWORD)&m_prs->m_pulColumnLengths[m_nField-1];
#ifndef _UNICODE
			pcb->columnID.dwKind = DAOCOLKIND_STR;
			pcb->columnID.lpstr = lpszNoBracketName;
#else
			pcb->columnID.dwKind = DAOCOLKIND_WSTR;
			pcb->columnID.lpwstr = lpszNoBracketName;
#endif

			// Setup the field index map (and store value as void ptr)
			m_prs->m_pMapFieldIndex->SetAt(pv, (void*)m_nField);
		}
		return;

	case BindParam:
		{
			COleVariant* pvar = NULL;

			TRY
			{
				// NULL params not supported, use IS NULL in SQL
				// (i.e. - m_strFilter = _T("Foo IS NULL");
				FillVariant(pv, dwColumnType, &pvar);
				m_prs->m_pQueryDef->SetParamValue(lpszName, *pvar);
			}
			CATCH_ALL(e)
			{
				if (pvar != NULL)
					pvar->Clear();
				delete pvar;
				pvar = NULL;
				THROW_LAST();
			}
			END_CATCH_ALL

			pvar->Clear();
			delete pvar;
			pvar = NULL;
		}
		return;

	case Fixup:
		if (m_prs->GetFieldLength(m_nField-1) == DAO_NULL)
		{
			// Set the value to PSEUDO NULL and mark the status NULL
			SetNullValue(pv, dwColumnType);
			m_prs->SetNullFieldStatus(m_nField-1);
		}
		return;

	case AllocCache:
		if (dwBindOptions & AFX_DAO_ENABLE_FIELD_CACHE)
		{
			CDaoFieldCache* pCache;

			// Allocate new storage and add to map
			AllocCacheValue(pCache, dwColumnType);
			m_prs->m_pMapFieldCache->SetAt(pv, pCache);
		}
		return;

	case StoreField:
		if (dwBindOptions & AFX_DAO_ENABLE_FIELD_CACHE)
		{
			CDaoFieldCache* pCache = GetCacheValue(m_prs, pv);

			// Copy the data to the cache
			if (dwBindOptions & AFX_DAO_CACHE_BY_VALUE)
				CopyValue(pv, (void*)&pCache->m_pvData, dwColumnType);
			else
				CopyValue(pv, pCache->m_pvData, dwColumnType);

			// Cache the NULL status
			if (m_prs->IsFieldStatusNull(m_nField-1))
				pCache->m_nStatus |= AFX_DAO_FIELD_FLAG_NULL;
			else
				pCache->m_nStatus &= ~AFX_DAO_FIELD_FLAG_NULL;
		}
		return;

	case LoadField:
		if (dwBindOptions & AFX_DAO_ENABLE_FIELD_CACHE)
		{
			CDaoFieldCache* pCache = GetCacheValue(m_prs, pv);

			// Copy the data from the cache
			if (dwBindOptions & AFX_DAO_CACHE_BY_VALUE)
				CopyValue((void*)&pCache->m_pvData, pv, dwColumnType);
			else
				CopyValue(pCache->m_pvData, pv, dwColumnType);

			// Set the NULL status from the cache
			if (pCache->m_nStatus & AFX_DAO_FIELD_FLAG_NULL)
				m_prs->SetNullFieldStatus(m_nField-1);
			else
				m_prs->ClearNullFieldStatus(m_nField-1);
		}
		return;

	case SetFieldNull:
		// Setting field NOT NULL doesn't require field exchange
		if ((m_pvField == NULL && m_nFieldType == outputColumn)
			|| m_pvField == pv)
		{
			SetNullValue(pv, dwColumnType);

			// Also set the status array if not a parameter
			if (m_nFieldType == outputColumn)
				m_prs->SetNullFieldStatus(m_nField-1);

#ifdef _DEBUG
			m_nFieldFound = m_nField;
#endif
		}
		return;

	case MarkForAddNew:
		if (dwBindOptions & AFX_DAO_ENABLE_FIELD_CACHE)
		{
			// Don't need to do anything if field marked dirty
			if (!m_prs->IsFieldStatusDirty(m_nField-1))
			{
				// Mark dirty & not NULL if not set to pseudo NULL value
				if (!IsNullValue(pv, dwColumnType))
				{
					m_prs->SetDirtyFieldStatus(m_nField-1);
					m_prs->ClearNullFieldStatus(m_nField-1);
				}
			}
		}
		return;

	case MarkForEdit:
		if (dwBindOptions & AFX_DAO_ENABLE_FIELD_CACHE)
		{
			// If value not pseudo NULL value, clear NULL status
			if (!IsNullValue(pv, dwColumnType))
				m_prs->ClearNullFieldStatus(m_nField-1);

			// If field already marked dirty, don't need to check cache
			if (!m_prs->IsFieldStatusDirty(m_nField-1))
			{
				CDaoFieldCache* pCache = GetCacheValue(m_prs, pv);

				BOOL bNullField = m_prs->IsFieldStatusNull(m_nField-1);
				BOOL bNullCache = pCache->m_nStatus & AFX_DAO_FIELD_FLAG_NULL;

				void* pvData;
				if (dwBindOptions & AFX_DAO_CACHE_BY_VALUE)
					pvData = &pCache->m_pvData;
				else
					pvData = pCache->m_pvData;

				// Mark dirty if NULL status differs or value differs
				if ( (bNullCache && !bNullField) ||
					(!bNullCache && bNullField) ||
					CompareValue(pv, pvData, dwColumnType))
				{
					m_prs->SetDirtyFieldStatus(m_nField-1);
				}
			}
		}
		return;

	case SetDirtyField:
		if (m_prs->IsFieldStatusDirty(m_nField-1))
		{
			COleVariant* pvar = NULL;

			TRY
			{
				// If field is NULL don't set the value
				if (!m_prs->IsFieldStatusNull(m_nField-1))
					FillVariant(pv, dwColumnType, &pvar);
				else
				{
					pvar = new COleVariant;
					pvar->vt = VT_NULL;
				}

				// SetFieldValue (put_Collect) doesn't like brackets
				// Assumes no brackets if first char not a bracket
				LPTSTR lpszModifiedName = NULL;
				if (*lpszName == '[')
				{
					lpszModifiedName = new TCHAR[lstrlen(lpszName) + 1];

					// Copy the name with no brackets, and reset lpszName
					m_prs->StripBrackets(lpszName, lpszModifiedName);
					lpszName = lpszModifiedName;
				}

				m_prs->SetFieldValue(lpszName, *pvar);
				delete lpszModifiedName;
			}
			CATCH_ALL(e)
			{
				if (pvar != NULL)
					pvar->Clear();
				delete pvar;
				pvar = NULL;
				THROW_LAST();
			}
			END_CATCH_ALL

			pvar->Clear();
			delete pvar;
			pvar = NULL;
		}
		return;

	default:
		ASSERT(FALSE);
		return;
	}
}

void AFXAPI DFX_Text(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CString& value, int nPreAllocSize, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	DWORD dwDAOType;

#ifdef _UNICODE
	dwDAOType = DAO_WCHAR;
#else
	dwDAOType = DAO_CHAR;
#endif

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			// Pre-allocate buffer to prevent needless re-allocations
			value.GetBuffer(nPreAllocSize);
			value.ReleaseBuffer();

			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = dwDAOType;
			pcb->dwBinding = DAOBINDING_DIRECT | DAOBINDING_CALLBACK;
			pcb->dwUser = (DWORD)&value;
			pcb->cbDataOffset = (DWORD)DaoStringAllocCallback;
			pcb->cbMaxLen = INT_MAX;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_TEXT,
			dwBindOptions);
		return;

	case CDaoFieldExchange::Fixup:
		if (pFX->m_prs->m_pulColumnLengths[pFX->m_nField-1] == 0 ||
			pFX->m_prs->m_pulColumnLengths[pFX->m_nField-1] == DAO_NULL)
		{
			// If null or empty string set length zero
			value.GetBufferSetLength(0);
		}
		else
		{
			// Make sure length correct
			value.GetBufferSetLength(
				((pFX->m_prs->m_pulColumnLengths[pFX->m_nField-1])/sizeof(TCHAR))-1);
		}

		pFX->Default(lpszName, (void*)&value,
			AFX_RFX_TEXT, dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Binary(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CByteArray& value, int nPreAllocSize, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			// Pre-allocate buffer to prevent needless re-allocations
			value.SetSize(nPreAllocSize);

			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_BYTES;
			pcb->dwBinding = DAOBINDING_DIRECT | DAOBINDING_CALLBACK;
			pcb->dwUser = (DWORD)&value;
			pcb->cbDataOffset = (ULONG)DaoBinaryAllocCallback;
			pcb->cbMaxLen = INT_MAX;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_BINARY,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_LongBinary(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	CLongBinary& value, DWORD dwPreAllocSize, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			// Pre-allocate buffer to prevent needless re-allocations
			AllocLongBinary(value, dwPreAllocSize);

			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_BYTES;
			pcb->dwBinding = DAOBINDING_DIRECT | DAOBINDING_CALLBACK;
			pcb->dwUser = (DWORD)&value;
			pcb->cbDataOffset = (DWORD)DaoLongBinaryAllocCallback;
			pcb->cbMaxLen = ULONG_MAX;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value,
			AFX_RFX_LONGBINARY, dwBindOptions);
		return;

	case CDaoFieldExchange::Fixup:
		// Unlock data locked in DaoLongBinaryAllocCallback
		if (value.m_dwDataLength != 0)
			::GlobalUnlock(value.m_hData);

		pFX->Default(lpszName, (void*)&value,
			AFX_RFX_LONGBINARY, dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Bool(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	BOOL& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	// Mark as CACHE_BY_VALUE (size <= sizeof(void*))
	dwBindOptions |= AFX_DAO_CACHE_BY_VALUE;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_BOOL;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_BOOL,
			dwBindOptions);
		return;

	case CDaoFieldExchange::Fixup:
		// Convert BOOL value from AFX_DAO_TRUE/FALSE to TRUE/FALSE
		value = (value != AFX_DAO_FALSE);

		pFX->Default(lpszName, (void*)&value,
			AFX_RFX_BOOL, dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Byte(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	BYTE& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	// Mark as CACHE_BY_VALUE (size <= sizeof(void*))
	dwBindOptions |= AFX_DAO_CACHE_BY_VALUE;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_BYTE;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_BYTE,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Short(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	short& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	// Mark as CACHE_BY_VALUE (size <= sizeof(void*))
	dwBindOptions |= AFX_DAO_CACHE_BY_VALUE;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_I2;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_SHORT,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Long(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	long& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	// Mark as CACHE_BY_VALUE (size <= sizeof(void*))
	dwBindOptions |= AFX_DAO_CACHE_BY_VALUE;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_I4;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_LONG,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Currency(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	COleCurrency& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_CURRENCY;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value.m_cur;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(CURRENCY);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;

			// Finish setting up column binding struct
			pFX->Default(lpszName, (void*)&value, AFX_RFX_CURRENCY,
				dwBindOptions);
			return;
		}

	case CDaoFieldExchange::Fixup:
		// Must reset the valid currency flag
		if (pFX->m_prs->GetFieldLength(pFX->m_nField-1) == DAO_NULL)
			value.SetStatus(COleCurrency::null);
		else
			value.SetStatus(COleCurrency::valid);

		// Fall through to reset the NULL status

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_CURRENCY,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << ":" << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Single(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	float& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	// Mark as CACHE_BY_VALUE (size <= sizeof(void*))
	dwBindOptions |= AFX_DAO_CACHE_BY_VALUE;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_R4;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_SINGLE,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_Double(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	double& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_R8;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(value);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;
		}
		// Fall through to finish setting up column binding struct

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_DOUBLE,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << " = " << value;
		return;
#endif //_DEBUG
	}
}

void AFXAPI DFX_DateTime(CDaoFieldExchange* pFX, LPCTSTR lpszName,
	COleDateTime& value, DWORD dwBindOptions)
{
	(pFX->m_nFieldType == CDaoFieldExchange::outputColumn) ?
		++pFX->m_nField: ++pFX->m_nParam;

	// Do nothing if op not supported for outputColumn or param type
	if (!pFX->IsValidOperation())
		return;

	switch (pFX->m_nOperation)
	{
	case CDaoFieldExchange::BindField:
		{
			LPDAOCOLUMNBINDING pcb =
				&pFX->m_prs->m_prgDaoColBindInfo[pFX->m_nField-1];
			pcb->dwDataType = DAO_DATE;
			pcb->dwBinding = DAOBINDING_DIRECT;
			pcb->cbDataOffset = (DWORD)&value.m_dt;
			pcb->dwUser = 0;
			pcb->cbMaxLen = sizeof(DATE);

			pFX->m_prs->m_cbFixedLengthFields += pcb->cbMaxLen;

			// Finish setting up column binding struct
			pFX->Default(lpszName,(void*)&value, AFX_RFX_DATE,
				dwBindOptions);
			return;
		}

	case CDaoFieldExchange::Fixup:
		// Must reset the valid currency flag
		if (pFX->m_prs->GetFieldLength(pFX->m_nField-1) == DAO_NULL)
			value.SetStatus(COleDateTime::null);
		else
			value.SetStatus(COleDateTime::valid);

		// Fall through to reset the NULL status

	default:
		pFX->Default(lpszName, (void*)&value, AFX_RFX_DATE,
			dwBindOptions);
		return;

#ifdef _DEBUG
	case CDaoFieldExchange::DumpField:
		*pFX->m_pdcDump << "\n" << lpszName << ":" << value;
		return;
#endif //_DEBUG
	}
}

//////////////////////////////////////////////////////////////////////////////
// DAO memory allocation callback helpers

STDAPI DaoStringAllocCallback(DWORD dwLen, DWORD pData, void** ppv)
{
	LPTSTR lpsz;
	CString* pstr = (CString*)pData;

#ifndef _UNICODE
	// If using ANSI DAO interfaces, DAO reports field length
	// rather than data length. In this case there will not be space
	// for NULL terminator if data length equals field length. Make room.
	dwLen++;
#endif

	TRY
	{
		// Only re-allocate if necessary
		lpsz = pstr->GetBufferSetLength(dwLen/sizeof(TCHAR));
		*ppv = (void*)(dwLen > 0 ? lpsz : NULL);
	}
	CATCH_ALL(e)
	{
		e->Delete();
		return E_OUTOFMEMORY;
	}
	END_CATCH_ALL

	return S_OK;
}


STDAPI DaoBinaryAllocCallback(DWORD dwLen, DWORD pData, void** ppv)
{
	CByteArray* pByteArray = (CByteArray*)pData;

	TRY
	{
		// Only re-allocate if necessary
		pByteArray->SetSize(dwLen);
		*ppv = (void*)(dwLen > 0 ? &((*pByteArray)[0]) : NULL);
	}
	CATCH_ALL(e)
	{
		// Only exceptions thrown should be CMemoryException
		e->Delete();
		return E_OUTOFMEMORY;
	}
	END_CATCH_ALL

	return S_OK;
}


STDAPI DaoLongBinaryAllocCallback(DWORD dwLen, DWORD pData, void** ppv)
{
	CLongBinary* pLongBinary = (CLongBinary*)pData;

	TRY
	{
		AllocLongBinary(*pLongBinary, dwLen);
	}
	CATCH_ALL(e)
	{
		// Only exception is memory exception, just pass back error.
		DELETE_EXCEPTION(e);
		return E_OUTOFMEMORY;
	}
	END_CATCH_ALL

	if (pLongBinary->m_dwDataLength != 0)
	{
		const BYTE* pByte;
		pByte = (const BYTE*)::GlobalLock(pLongBinary->m_hData);

		// If mem can't be locked, free up and return error
		if (pByte == NULL)
		{
			::GlobalFree(pLongBinary->m_hData);
			pLongBinary->m_hData = NULL;
			return E_OUTOFMEMORY;
		}

		*ppv = (void*)pByte;
	}
	else
		*ppv = NULL;

	return S_OK;
}

void AFX_CDECL AllocLongBinary(CLongBinary& lb, DWORD dwDataLength)
{
	if (lb.m_hData == NULL)
	{
		if (dwDataLength > 0)
		{
			// Alloc memory, return error if not possible
			lb.m_hData = ::GlobalAlloc(GMEM_MOVEABLE, dwDataLength);
			if (lb.m_hData == NULL)
				AfxThrowMemoryException();
		}
	}
	else if (::GlobalSize(lb.m_hData) < dwDataLength)
	{
		// Save the old address in case ReAlloc fails
		HGLOBAL hOldData = lb.m_hData;

		// Alloc more mem, free up mem and throw exception if not possible
		lb.m_hData = ::GlobalReAlloc(hOldData, dwDataLength, GMEM_MOVEABLE);
		if (lb.m_hData == NULL)
		{
			lb.m_hData = hOldData;
			AfxThrowMemoryException();
		}
	}
	lb.m_dwDataLength = dwDataLength;
}

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxDaoInl[] = "afxdao.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDaoInl
#define _AFXDAODFX_INLINE
#include "afxdao.inl"

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
