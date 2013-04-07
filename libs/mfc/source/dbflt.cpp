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
#include <float.h>

#ifdef AFX_DB_SEG
#pragma code_seg(AFX_DB_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

// Helpers for floating point operations
const float afxFloatPseudoNull = AFX_RFX_SINGLE_PSEUDO_NULL;
const double afxDoublePseudoNull = AFX_RFX_DOUBLE_PSEUDO_NULL;

extern void AFXAPI AfxTextFloatFormat(CDataExchange* pDX, int nIDC,
	void* pData, double value, int nSizeGcvt);

extern BOOL AFXAPI AfxFieldText(CDataExchange* pDX, int nIDC, void* pv,
	CRecordset* pRecordset);

void AFXAPI RFX_Single(CFieldExchange* pFX, LPCTSTR szName,
	float& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_C_FLOAT)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: float converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif
		}
		// fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_FLOAT,
			sizeof(value), 13);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = afxFloatPseudoNull;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = afxFloatPseudoNull;
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
		if (value != afxFloatPseudoNull)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != afxFloatPseudoNull)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];

			// Data cached by value, no allocation necessary
			pInfo->m_nDataType = AFX_RFX_SINGLE;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		{
			*pFX->m_pdcDump << "\n" << szName << " = " << value;
		}
		return;
#endif //_DEBUG

	}
}


void AFXAPI RFX_Double(CFieldExchange* pFX, LPCTSTR szName,
	double& value)
{
	ASSERT(AfxIsValidAddress(pFX, sizeof(CFieldExchange)));
	ASSERT(AfxIsValidString(szName));

	UINT nField;
	if (!pFX->IsFieldType(&nField))
		return;

	LONG* plLength = pFX->m_prs->GetFieldLengthBuffer(nField - 1, pFX->m_nFieldType);
	switch (pFX->m_nOperation)
	{
	case CFieldExchange::BindFieldToColumn:
		{
#ifdef _DEBUG
			// Assumes all bound fields BEFORE unbound fields
			CODBCFieldInfo* pODBCInfo =
				&pFX->m_prs->m_rgODBCFieldInfos[nField - 1];

			if (pODBCInfo->m_nSQLType != SQL_C_DOUBLE &&
				pODBCInfo->m_nSQLType != SQL_FLOAT)
			{
				// Warn of possible field schema mismatch
				if (afxTraceFlags & traceDatabase)
					TRACE1("Warning: double converted from SQL type %ld.\n",
						pODBCInfo->m_nSQLType);
			}
#endif
		}
		// fall through

	default:
LDefault:
		pFX->Default(szName, &value, plLength, SQL_C_DOUBLE,
			sizeof(value), 22);
		return;

	case CFieldExchange::Fixup:
		if (*plLength == SQL_NULL_DATA)
		{
			pFX->m_prs->SetNullFieldStatus(nField - 1);
			value = afxDoublePseudoNull;
		}
		return;

	case CFieldExchange::SetFieldNull:
		if ((pFX->m_pvField == NULL &&
			pFX->m_nFieldType == CFieldExchange::outputColumn) ||
			pFX->m_pvField == &value)
		{
			if (pFX->m_bField)
			{
				pFX->m_prs->SetNullFieldStatus(nField - 1);
				value = afxDoublePseudoNull;
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
		if (value != afxDoublePseudoNull)
		{
			pFX->m_prs->SetDirtyFieldStatus(nField - 1);
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		}
		return;

	case CFieldExchange::MarkForUpdate:
		if (value != afxDoublePseudoNull)
			pFX->m_prs->ClearNullFieldStatus(nField - 1);
		goto LDefault;

	case CFieldExchange::AllocCache:
		{
			CFieldInfo* pInfo = &pFX->m_prs->m_rgFieldInfos[nField - 1];
			pInfo->m_pvDataCache = new double;
			pInfo->m_nDataType = AFX_RFX_DOUBLE;
		}
		return;

#ifdef _DEBUG
	case CFieldExchange::DumpField:
		{
			*pFX->m_pdcDump << "\n" << szName << " = " << value;
		}
		return;
#endif //_DEBUG

	}
}

/////////////////////////////////////////////////////////////////////////////

void AFXAPI RFX_Single_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	float** prgFltVals, long** prgLengths)
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
			ASSERT(*prgFltVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgFltVals = new float[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgFltVals;
		*prgFltVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgFltVals, *prgLengths,
			SQL_C_FLOAT, sizeof(float));
		break;
	}
}

void AFXAPI RFX_Double_Bulk(CFieldExchange* pFX, LPCTSTR szName,
	double** prgDblVals, long** prgLengths)
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
			ASSERT(*prgDblVals == NULL);
			ASSERT(*prgLengths == NULL);

			int nRowsetSize = pFX->m_prs->GetRowsetSize();

			// Allocate buffers to hold data and length
			*prgDblVals = new double[nRowsetSize];
			*prgLengths = new long[nRowsetSize];
		}
		break;

	case CFieldExchange::DeleteMultiRowBuffer:
		delete [] *prgDblVals;
		*prgDblVals = NULL;

		delete [] *prgLengths;
		*prgLengths = NULL;
		break;

	default:
		AfxRFXBulkDefault(pFX, szName, *prgDblVals, *prgLengths,
			SQL_C_DOUBLE, sizeof(double));
		break;
	}
}

/////////////////////////////////////////////////////////////////////////////

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, float& value,
	CRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		AfxTextFloatFormat(pDX, nIDC, &value, value, FLT_DIG);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, double& value,
	CRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		AfxTextFloatFormat(pDX, nIDC, &value, value, DBL_DIG);
}

/////////////////////////////////////////////////////////////////////////////
