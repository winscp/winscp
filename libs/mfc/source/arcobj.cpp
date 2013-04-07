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

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
// Archive support for polymorphic reading/writing of CObjects

// Note: Starting with MFC 4.0, the file format written/read has been
//  extended to eliminate the previous 32k limit.  Files previously written
//  can still be read by old versions (even 16-bit versions).  In addition,
//  new files, unless they are large enough to take advantage of 32-bit tags,
//  can be read by old versions.

// Pointer mapping constants
#define wNullTag        ((WORD)0)           // special tag indicating NULL ptrs
#define wNewClassTag    ((WORD)0xFFFF)      // special tag indicating new CRuntimeClass
#define wClassTag       ((WORD)0x8000)      // 0x8000 indicates class tag (OR'd)
#define dwBigClassTag   ((DWORD)0x80000000) // 0x8000000 indicates big class tag (OR'd)
#define wBigObjectTag   ((WORD)0x7FFF)      // 0x7FFF indicates DWORD object tag
#define nMaxMapCount    ((DWORD)0x3FFFFFFE) // 0x3FFFFFFE last valid mapCount

// Note: tag value 0x8000 could be used for something in the future, since
//  it is currently an invalid tag (0x8000 means zero wClassTag, but zero
//  index is always reserved for a NULL pointer, and a NULL runtime class
//  does not make any sense).

// This is how the tags have been allocated currently:
//
//  0x0000              represents NULL pointer
//  0x0001 - 0x7FFE     "small" object tags
//  0x7FFF              header for "big" object/class tag
//  0x8000              reserved for future use
//  0x8001 - 0xFFFE     "small" class tag
//  0xFFFF              header for class definition
//
// The special value of 0x7FFF indicates that a DWORD tag follows.  This
// two part "big" tag is used for 32-bit tag values 0x7FFF and above.
// The tag value of 0x7FFF was unused in MFC versions prior to MFC 4.0.

////////////////////////////////////////////////////////////////////////////

void CArchive::CheckCount()
{
	if (m_nMapCount >= nMaxMapCount)
		AfxThrowArchiveException(CArchiveException::badIndex, m_strFileName);
}

void CArchive::WriteObject(const CObject* pOb)
{
	// object can be NULL
	ASSERT(IsStoring());    // proper direction

	DWORD nObIndex;
	ASSERT(sizeof(nObIndex) == 4);
	ASSERT(sizeof(wNullTag) == 2);
	ASSERT(sizeof(wBigObjectTag) == 2);
	ASSERT(sizeof(wNewClassTag) == 2);

	// make sure m_pStoreMap is initialized
	MapObject(NULL);

	if (pOb == NULL)
	{
		// save out null tag to represent NULL pointer
		*this << wNullTag;
	}
	else if ((nObIndex = (DWORD)(*m_pStoreMap)[(void*)pOb]) != 0)
		// assumes initialized to 0 map
	{
		// save out index of already stored object
		if (nObIndex < wBigObjectTag)
			*this << (WORD)nObIndex;
		else
		{
			*this << wBigObjectTag;
			*this << nObIndex;
		}
	}
	else
	{
		// write class of object first
		CRuntimeClass* pClassRef = pOb->GetRuntimeClass();
		WriteClass(pClassRef);

		// enter in stored object table, checking for overflow
		CheckCount();
		(*m_pStoreMap)[(void*)pOb] = (void*)m_nMapCount++;

		// cause the object to serialize itself
		((CObject*)pOb)->Serialize(*this);
	}
}

CObject* CArchive::ReadObject(const CRuntimeClass* pClassRefRequested)
{
	ASSERT(pClassRefRequested == NULL ||
		AfxIsValidAddress(pClassRefRequested, sizeof(CRuntimeClass), FALSE));
	ASSERT(IsLoading());    // proper direction
	ASSERT(wNullTag == 0);
	ASSERT((wClassTag << 16) == dwBigClassTag);
	ASSERT((wNewClassTag & wClassTag) == wClassTag);

	// attempt to load next stream as CRuntimeClass
	UINT nSchema;
	DWORD obTag;
	CRuntimeClass* pClassRef = ReadClass(pClassRefRequested, &nSchema, &obTag);

	// check to see if tag to already loaded object
	CObject* pOb;
	if (pClassRef == NULL)
	{
		if (obTag > (DWORD)m_pLoadArray->GetUpperBound())
		{
			// tag is too large for the number of objects read so far
			AfxThrowArchiveException(CArchiveException::badIndex,
				m_strFileName);
		}

		pOb = (CObject*)m_pLoadArray->GetAt(obTag);
		if (pOb != NULL && pClassRefRequested != NULL &&
			 !pOb->IsKindOf(pClassRefRequested))
		{
			// loaded an object but of the wrong class
			AfxThrowArchiveException(CArchiveException::badClass,
				m_strFileName);
		}
	}
	else
	{
		// allocate a new object based on the class just acquired
		pOb = pClassRef->CreateObject();
		if (pOb == NULL)
			AfxThrowMemoryException();

		// Add to mapping array BEFORE de-serializing
		CheckCount();
		m_pLoadArray->InsertAt(m_nMapCount++, pOb);

		// Serialize the object with the schema number set in the archive
		UINT nSchemaSave = m_nObjectSchema;
		m_nObjectSchema = nSchema;
		pOb->Serialize(*this);
		m_nObjectSchema = nSchemaSave;
		ASSERT_VALID(pOb);
	}

	return pOb;
}

/////////////////////////////////////////////////////////////////////////////
// advanced versioning and back-pointer support

UINT CArchive::GetObjectSchema()
{
	UINT nResult = m_nObjectSchema;
	m_nObjectSchema = (UINT)-1; // can only be called once per Serialize
	return nResult;
}

void CArchive::MapObject(const CObject* pOb)
{
	if (IsStoring())
	{
		if (m_pStoreMap == NULL)
		{
			// initialize the storage map
			//  (use CMapPtrToPtr because it is used for HANDLE maps too)
			m_pStoreMap = new CMapPtrToPtr(m_nGrowSize);
			m_pStoreMap->InitHashTable(m_nHashSize);
			m_pStoreMap->SetAt(NULL, (void*)(DWORD)wNullTag);
			m_nMapCount = 1;
		}
		if (pOb != NULL)
		{
			CheckCount();
			(*m_pStoreMap)[(void*)pOb] = (void*)m_nMapCount++;
		}
	}
	else
	{
		if (m_pLoadArray == NULL)
		{
			// initialize the loaded object pointer array and set special values
			m_pLoadArray = new CPtrArray;
			m_pLoadArray->SetSize(1, m_nGrowSize);
			ASSERT(wNullTag == 0);
			m_pLoadArray->SetAt(wNullTag, NULL);
			m_nMapCount = 1;
		}
		if (pOb != NULL)
		{
			CheckCount();
			m_pLoadArray->InsertAt(m_nMapCount++, (void*)pOb);
		}
	}
}

void CArchive::WriteClass(const CRuntimeClass* pClassRef)
{
	ASSERT(pClassRef != NULL);
	ASSERT(IsStoring());    // proper direction

	if (pClassRef->m_wSchema == 0xFFFF)
	{
		TRACE1("Warning: Cannot call WriteClass/WriteObject for %hs.\n",
			pClassRef->m_lpszClassName);
		AfxThrowNotSupportedException();
	}

	// make sure m_pStoreMap is initialized
	MapObject(NULL);

	// write out class id of pOb, with high bit set to indicate
	// new object follows

	// ASSUME: initialized to 0 map
	DWORD nClassIndex;
	if ((nClassIndex = (DWORD)(*m_pStoreMap)[(void*)pClassRef]) != 0)
	{
		// previously seen class, write out the index tagged by high bit
		if (nClassIndex < wBigObjectTag)
			*this << (WORD)(wClassTag | nClassIndex);
		else
		{
			*this << wBigObjectTag;
			*this << (dwBigClassTag | nClassIndex);
		}
	}
	else
	{
		// store new class
		*this << wNewClassTag;
		pClassRef->Store(*this);

		// store new class reference in map, checking for overflow
		CheckCount();
		(*m_pStoreMap)[(void*)pClassRef] = (void*)m_nMapCount++;
	}
}

CRuntimeClass* CArchive::ReadClass(const CRuntimeClass* pClassRefRequested,
	UINT* pSchema, DWORD* pObTag)
{
	ASSERT(pClassRefRequested == NULL ||
		AfxIsValidAddress(pClassRefRequested, sizeof(CRuntimeClass), FALSE));
	ASSERT(IsLoading());    // proper direction

	if (pClassRefRequested != NULL && pClassRefRequested->m_wSchema == 0xFFFF)
	{
		TRACE1("Warning: Cannot call ReadClass/ReadObject for %hs.\n",
			pClassRefRequested->m_lpszClassName);
		AfxThrowNotSupportedException();
	}

	// make sure m_pLoadArray is initialized
	MapObject(NULL);

	// read object tag - if prefixed by wBigObjectTag then DWORD tag follows
	DWORD obTag;
	WORD wTag;
	*this >> wTag;
	if (wTag == wBigObjectTag)
		*this >> obTag;
	else
		obTag = ((wTag & wClassTag) << 16) | (wTag & ~wClassTag);

	// check for object tag (throw exception if expecting class tag)
	if (!(obTag & dwBigClassTag))
	{
		if (pObTag == NULL)
			AfxThrowArchiveException(CArchiveException::badIndex, m_strFileName);

		*pObTag = obTag;
		return NULL;
	}

	CRuntimeClass* pClassRef;
	UINT nSchema;
	if (wTag == wNewClassTag)
	{
		// new object follows a new class id
		if ((pClassRef = CRuntimeClass::Load(*this, &nSchema)) == NULL)
			AfxThrowArchiveException(CArchiveException::badClass, m_strFileName);

		// check nSchema against the expected schema
		if ((pClassRef->m_wSchema & ~VERSIONABLE_SCHEMA) != nSchema)
		{
			if (!(pClassRef->m_wSchema & VERSIONABLE_SCHEMA))
			{
				// schema doesn't match and not marked as VERSIONABLE_SCHEMA
				AfxThrowArchiveException(CArchiveException::badSchema,
					m_strFileName);
			}
			else
			{
				// they differ -- store the schema for later retrieval
				if (m_pSchemaMap == NULL)
					m_pSchemaMap = new CMapPtrToPtr;
				ASSERT_VALID(m_pSchemaMap);
				m_pSchemaMap->SetAt(pClassRef, (void*)nSchema);
			}
		}
		CheckCount();
		m_pLoadArray->InsertAt(m_nMapCount++, pClassRef);
	}
	else
	{
		// existing class index in obTag followed by new object
		DWORD nClassIndex = (obTag & ~dwBigClassTag);
		if (nClassIndex == 0 || nClassIndex > (DWORD)m_pLoadArray->GetUpperBound())
			AfxThrowArchiveException(CArchiveException::badIndex,
				m_strFileName);

		pClassRef = (CRuntimeClass*)m_pLoadArray->GetAt(nClassIndex);
		ASSERT(pClassRef != NULL);

		// determine schema stored against objects of this type
		void* pTemp;
		BOOL bFound = FALSE;
		nSchema = 0;
		if (m_pSchemaMap != NULL)
		{
			bFound = m_pSchemaMap->Lookup( pClassRef, pTemp );
			if (bFound)
				nSchema = (UINT)pTemp;
		}
		if (!bFound)
			nSchema = pClassRef->m_wSchema & ~VERSIONABLE_SCHEMA;
   }

	// check for correct derivation
	if (pClassRefRequested != NULL &&
		!pClassRef->IsDerivedFrom(pClassRefRequested))
	{
		AfxThrowArchiveException(CArchiveException::badClass, m_strFileName);
	}

	// store nSchema for later examination
	if (pSchema != NULL)
		*pSchema = nSchema;
	else
		m_nObjectSchema = nSchema;

	// store obTag for later examination
	if (pObTag != NULL)
		*pObTag = obTag;

	// return the resulting CRuntimeClass*
	return pClassRef;
}

void CArchive::SerializeClass(const CRuntimeClass* pClassRef)
{
	if (IsStoring())
		WriteClass(pClassRef);
	else
		ReadClass(pClassRef);
}

////////////////////////////////////////////////////////////////////////////
