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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CEnumArray (provides OLE enumerator for arbitrary items in an array)

CEnumArray::CEnumArray(size_t nSizeElem, const void* pvEnum, UINT nSize,
	BOOL bNeedFree)
{
	m_nSizeElem = nSizeElem;
	m_pClonedFrom = NULL;

	m_nCurPos = 0;
	m_nSize = nSize;
	m_pvEnum = (BYTE*)pvEnum;
	m_bNeedFree = bNeedFree;

	ASSERT_VALID(this);
}

CEnumArray::~CEnumArray()
{
	ASSERT_VALID(this);

	// release the clone pointer (only for clones)
	if (m_pClonedFrom != NULL)
	{
		m_pClonedFrom->InternalRelease();
		ASSERT(!m_bNeedFree);
	}

	// release the pointer (should only happen on non-clones)
	if (m_bNeedFree)
	{
		ASSERT(m_pClonedFrom == NULL);
		delete m_pvEnum;
	}
}

BOOL CEnumArray::OnNext(void* pv)
{
	ASSERT_VALID(this);

	if (m_nCurPos >= m_nSize)
		return FALSE;

	memcpy(pv, &m_pvEnum[m_nCurPos*m_nSizeElem], m_nSizeElem);
	++m_nCurPos;
	return TRUE;
}

BOOL CEnumArray::OnSkip()
{
	ASSERT_VALID(this);

	if (m_nCurPos >= m_nSize)
		return FALSE;

	return ++m_nCurPos < m_nSize;
}

void CEnumArray::OnReset()
{
	ASSERT_VALID(this);

	m_nCurPos = 0;
}

CEnumArray* CEnumArray::OnClone()
{
	ASSERT_VALID(this);

	// set up an exact copy of this object
	//  (derivatives may have to replace this code)
	CEnumArray* pClone;
	pClone = new CEnumArray(m_nSizeElem, m_pvEnum, m_nSize);
	ASSERT(pClone != NULL);
	ASSERT(!pClone->m_bNeedFree);   // clones should never free themselves
	pClone->m_nCurPos = m_nCurPos;

	// finally, return the clone to OLE
	ASSERT_VALID(pClone);
	return pClone;
}

/////////////////////////////////////////////////////////////////////////////
// CEnumArray::XEnumVOID implementation

STDMETHODIMP_(ULONG) CEnumArray::XEnumVOID::AddRef()
{
	METHOD_PROLOGUE_EX_(CEnumArray, EnumVOID)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CEnumArray::XEnumVOID::Release()
{
	METHOD_PROLOGUE_EX_(CEnumArray, EnumVOID)
	return pThis->ExternalRelease();
}

STDMETHODIMP CEnumArray::XEnumVOID::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(CEnumArray, EnumVOID)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP CEnumArray::XEnumVOID::Next(
	ULONG celt, void* reelt, ULONG* pceltFetched)
{
	METHOD_PROLOGUE_EX(CEnumArray, EnumVOID);
	ASSERT_VALID(pThis);

	if (pceltFetched != NULL)
		*pceltFetched = 0;

	ASSERT(celt > 0);
	ASSERT(celt == 1 || pceltFetched != NULL);

	BYTE* pchCur = (BYTE*)reelt;

	ULONG celtT = celt;
	SCODE sc = E_UNEXPECTED;
	TRY
	{
		while (celtT != 0 && pThis->OnNext((void*)pchCur))
		{
			pchCur += pThis->m_nSizeElem;
			--celtT;
		}
		if (pceltFetched != NULL)
			*pceltFetched = celt - celtT;
		sc = celtT == 0 ? S_OK : S_FALSE;
	}
	END_TRY

	return sc;
}

STDMETHODIMP CEnumArray::XEnumVOID::Skip(ULONG celt)
{
	METHOD_PROLOGUE_EX(CEnumArray, EnumVOID);
	ASSERT_VALID(pThis);

	ULONG celtT = celt;
	SCODE sc = E_UNEXPECTED;
	TRY
	{
		while (celtT != 0 && pThis->OnSkip())
			--celtT;
		sc = celtT == 0 ? S_OK : S_FALSE;
	}
	END_TRY

	return celtT != 0 ? S_FALSE : S_OK;
}

STDMETHODIMP CEnumArray::XEnumVOID::Reset()
{
	METHOD_PROLOGUE_EX(CEnumArray, EnumVOID);
	ASSERT_VALID(pThis);

	pThis->OnReset();
	return S_OK;
}

STDMETHODIMP CEnumArray::XEnumVOID::Clone(IEnumVOID** ppenm)
{
	METHOD_PROLOGUE_EX(CEnumArray, EnumVOID);
	ASSERT_VALID(pThis);

	*ppenm = NULL;

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		CEnumArray* pEnumHelper = pThis->OnClone();
		ASSERT_VALID(pEnumHelper);

		// we use an extra reference to keep the original object alive
		//  (the extra reference is removed in the clone's destructor)
		if (pThis->m_pClonedFrom != NULL)
			pEnumHelper->m_pClonedFrom = pThis->m_pClonedFrom;
		else
			pEnumHelper->m_pClonedFrom = pThis;
		pEnumHelper->m_pClonedFrom->InternalAddRef();
		*ppenm = &pEnumHelper->m_xEnumVOID;

		sc = S_OK;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
