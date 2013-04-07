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
#include <stddef.h>

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CSimpleList

void CSimpleList::AddHead(void* p)
{
	ASSERT(p != NULL);
	ASSERT(*GetNextPtr(p) == NULL);

	*GetNextPtr(p) = m_pHead;
	m_pHead = p;
}

BOOL CSimpleList::Remove(void* p)
{
	ASSERT(p != NULL);

	if (m_pHead == NULL)
		return FALSE;

	BOOL bResult = FALSE;
	if (m_pHead == p)
	{
		m_pHead = *GetNextPtr(p);
		DEBUG_ONLY(*GetNextPtr(p) = NULL);
		bResult = TRUE;
	}
	else
	{
		void* pTest = m_pHead;
		while (pTest != NULL && *GetNextPtr(pTest) != p)
			pTest = *GetNextPtr(pTest);
		if (pTest != NULL)
		{
			*GetNextPtr(pTest) = *GetNextPtr(p);
			DEBUG_ONLY(*GetNextPtr(p) = NULL);
			bResult = TRUE;
		}
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// CNoTrackObject

#if defined(_DEBUG) && !defined(_AFX_NO_DEBUG_CRT)
void* PASCAL CNoTrackObject::operator new(size_t nSize, LPCSTR, int)
{
	return CNoTrackObject::operator new(nSize);
}

#if _MSC_VER >= 1200
void PASCAL CNoTrackObject::operator delete(void* pObject, LPCSTR, int)
{
	if (pObject != NULL)
		::LocalFree(pObject);
}
#endif
#endif

void* PASCAL CNoTrackObject::operator new(size_t nSize)
{
	void* p = ::LocalAlloc(LPTR, nSize);
	if (p == NULL)
		AfxThrowMemoryException();
	return p;
}

void PASCAL CNoTrackObject::operator delete(void* p)
{
	if (p != NULL)
		::LocalFree(p);
}

/////////////////////////////////////////////////////////////////////////////
// CThreadSlotData

// global _afxThreadData used to allocate thread local indexes
BYTE __afxThreadData[sizeof(CThreadSlotData)];
CThreadSlotData* _afxThreadData;

struct CThreadData : public CNoTrackObject
{
	CThreadData* pNext; // required to be member of CSimpleList
	int nCount;         // current size of pData
	LPVOID* pData;      // actual thread local data (indexed by nSlot)
};

struct CSlotData
{
	DWORD dwFlags;      // slot flags (allocated/not allocated)
	HINSTANCE hInst;    // module which owns this slot
};

// flags used for CSlotData::dwFlags above
#define SLOT_USED   0x01    // slot is allocated

__declspec(nothrow) CThreadSlotData::CThreadSlotData()
{
	m_list.Construct(offsetof(CThreadData, pNext));

	// initialize state and allocate TLS index
	m_nAlloc = 0;
	m_nRover = 1;   // first slot (0) is always reserved
	m_nMax = 0;
	m_pSlotData = NULL;

	// init m_tlsIndex to -1 if !bThreadLocal, otherwise TlsAlloc
	m_tlsIndex = TlsAlloc();
	if (m_tlsIndex == (DWORD)-1)
		AfxThrowMemoryException();

	InitializeCriticalSection(&m_sect);
}

CThreadSlotData::~CThreadSlotData()
{
	if (m_tlsIndex != (DWORD)-1)
	{
		TlsFree(m_tlsIndex);
		DEBUG_ONLY(m_tlsIndex = (DWORD)-1);
	}

	CThreadData* pData = m_list;
	while (pData != NULL)
	{
		CThreadData* pDataNext = pData->pNext;
		DeleteValues(pData, NULL);
		pData = pDataNext;
	}

	if (m_pSlotData != NULL)
	{
		HGLOBAL hSlotData = GlobalHandle(m_pSlotData);
		GlobalUnlock(hSlotData);
		GlobalFree(hSlotData);
		DEBUG_ONLY(m_pSlotData = NULL);
	}

	DeleteCriticalSection(&m_sect);
}

int CThreadSlotData::AllocSlot()
{
	EnterCriticalSection(&m_sect);
	int nAlloc = m_nAlloc;
	int nSlot = m_nRover;
	if (nSlot >= nAlloc || (m_pSlotData[nSlot].dwFlags & SLOT_USED))
	{
		// search for first free slot, starting at beginning
		for (nSlot = 1;
			nSlot < nAlloc && (m_pSlotData[nSlot].dwFlags & SLOT_USED); nSlot++)
			;

		// if none found, need to allocate more space
		if (nSlot >= nAlloc)
		{
			// realloc memory for the bit array and the slot memory
			int nNewAlloc = m_nAlloc+32;

			// m_pSlotData is allocated GMEM_SHARE because on Win32s it needs
			// to be shared between processes (memory is owned by the MFC DLL).
			HGLOBAL hSlotData;
			if (m_pSlotData == NULL)
			{
				hSlotData = GlobalAlloc(GMEM_MOVEABLE|GMEM_SHARE,
					nNewAlloc*sizeof(CSlotData));
			}
			else
			{
				hSlotData = GlobalHandle(m_pSlotData);
				GlobalUnlock(hSlotData);
				hSlotData = GlobalReAlloc(hSlotData, nNewAlloc*sizeof(CSlotData),
					GMEM_MOVEABLE|GMEM_SHARE);
			}
			if (hSlotData == NULL)
			{
				GlobalLock(GlobalHandle(m_pSlotData));
				LeaveCriticalSection(&m_sect);
				AfxThrowMemoryException();
			}
			CSlotData* pSlotData = (CSlotData*)GlobalLock(hSlotData);

			// always zero initialize after success
			memset(pSlotData+m_nAlloc, 0, (nNewAlloc-m_nAlloc)*sizeof(CSlotData));
			m_nAlloc = nNewAlloc;
			m_pSlotData = pSlotData;
		}
	}
	ASSERT(nSlot != 0); // first slot (0) is reserved

	// adjust m_nMax to largest slot ever allocated
	if (nSlot >= m_nMax)
		m_nMax = nSlot+1;

	ASSERT(!(m_pSlotData[nSlot].dwFlags & SLOT_USED));
	m_pSlotData[nSlot].dwFlags |= SLOT_USED;
	// update m_nRover (likely place to find a free slot is next one)
	m_nRover = nSlot+1;

	LeaveCriticalSection(&m_sect);
	return nSlot;   // slot can be used for FreeSlot, GetValue, SetValue
}

void CThreadSlotData::FreeSlot(int nSlot)
{
	EnterCriticalSection(&m_sect);
	ASSERT(nSlot != 0 && nSlot < m_nMax);
	ASSERT(m_pSlotData != NULL);
	ASSERT(m_pSlotData[nSlot].dwFlags & SLOT_USED);

	// delete the data from all threads/processes
	CThreadData* pData = m_list;
	while (pData != NULL)
	{
		if (nSlot < pData->nCount)
		{
			delete (CNoTrackObject*)pData->pData[nSlot];
			pData->pData[nSlot] = NULL;
		}
		pData = pData->pNext;
	}
	// free the slot itself
	m_pSlotData[nSlot].dwFlags &= ~SLOT_USED;
	LeaveCriticalSection(&m_sect);
}

// special version of CThreadSlotData::GetData that only works with
// thread local storage (and not process local storage)
// this version is inlined and simplified for speed
inline void* CThreadSlotData::GetThreadValue(int nSlot)
{
	ASSERT(nSlot != 0 && nSlot < m_nMax);
	ASSERT(m_pSlotData != NULL);
	ASSERT(m_pSlotData[nSlot].dwFlags & SLOT_USED);
	ASSERT(m_tlsIndex != (DWORD)-1);

	CThreadData* pData = (CThreadData*)TlsGetValue(m_tlsIndex);
	if (pData == NULL || nSlot >= pData->nCount)
		return NULL;
	return pData->pData[nSlot];
}

void CThreadSlotData::SetValue(int nSlot, void* pValue)
{
	ASSERT(nSlot != 0 && nSlot < m_nMax);
	ASSERT(m_pSlotData != NULL);
	ASSERT(m_pSlotData[nSlot].dwFlags & SLOT_USED);

	CThreadData* pData = (CThreadData*)TlsGetValue(m_tlsIndex);
	if (pData == NULL || nSlot >= pData->nCount && pValue != NULL)
	{
		// if pData is NULL then this thread has not been visited yet
		if (pData == NULL)
		{
			pData = new CThreadData;
			pData->nCount = 0;
			pData->pData = NULL;
			DEBUG_ONLY(pData->pNext = NULL);

			// protect while adding to global list
			EnterCriticalSection(&m_sect);
			m_list.AddHead(pData);
			LeaveCriticalSection(&m_sect);
		}

		// grow to now current size
		if (pData->pData == NULL)
			pData->pData = (void**)LocalAlloc(LMEM_FIXED, m_nMax*sizeof(LPVOID));
		else
			pData->pData = (void**)LocalReAlloc(pData->pData, m_nMax*sizeof(LPVOID),
				LMEM_MOVEABLE);
		if (pData->pData == NULL)
			AfxThrowMemoryException();

		// initialize the newly allocated part
		memset(pData->pData + pData->nCount, 0,
			(m_nMax - pData->nCount) * sizeof(LPVOID));
		pData->nCount = m_nMax;
		TlsSetValue(m_tlsIndex, pData);
	}
	ASSERT(pData->pData != NULL && nSlot < pData->nCount);
	pData->pData[nSlot] = pValue;
}

void CThreadSlotData::AssignInstance(HINSTANCE hInst)
{
	EnterCriticalSection(&m_sect);
	ASSERT(m_pSlotData != NULL);
	ASSERT(hInst != NULL);

	for (int i = 1; i < m_nMax; i++)
	{
		if (m_pSlotData[i].hInst == NULL && (m_pSlotData[i].dwFlags & SLOT_USED))
			m_pSlotData[i].hInst = hInst;
	}
	LeaveCriticalSection(&m_sect);
}

void CThreadSlotData::DeleteValues(CThreadData* pData, HINSTANCE hInst)
{
	// Note: does not lock critical section because only meant to be called
	// from DeleteValues(HINSTANCE, BOOL).

	ASSERT(pData != NULL);

	// free each element in the table
	BOOL bDelete = TRUE;
	for (int i = 1; i < pData->nCount; i++)
	{
		if (hInst == NULL || m_pSlotData[i].hInst == hInst)
		{
			// delete the data since hInst matches (or is NULL)
			delete (CNoTrackObject*)pData->pData[i];
			pData->pData[i] = NULL;
		}
		else if (pData->pData[i] != NULL)
		{
			// don't delete thread data if other modules still alive
			bDelete = FALSE;
		}
	}

	if (bDelete)
	{
		// remove from master list and free it
		EnterCriticalSection(&m_sect);
		m_list.Remove(pData);
		LeaveCriticalSection(&m_sect);
		LocalFree(pData->pData);
		delete pData;

		// clear TLS index to prevent from re-use
		TlsSetValue(m_tlsIndex, NULL);
	}
}

void CThreadSlotData::DeleteValues(HINSTANCE hInst, BOOL bAll)
{
	EnterCriticalSection(&m_sect);
	if (!bAll)
	{
		// delete the values only for the current thread
		CThreadData* pData = (CThreadData*)TlsGetValue(m_tlsIndex);
		if (pData != NULL)
			DeleteValues(pData, hInst);
	}
	else
	{
		// delete the values for all threads
		CThreadData* pData = m_list;
		while (pData != NULL)
		{
			CThreadData* pDataNext = pData->pNext;
			DeleteValues(pData, hInst);
			pData = pDataNext;
		}
	}
	LeaveCriticalSection(&m_sect);
}

/////////////////////////////////////////////////////////////////////////////
// CThreadLocalObject

CNoTrackObject* CThreadLocalObject::GetData(
	CNoTrackObject* (AFXAPI* pfnCreateObject)())
{
	if (m_nSlot == 0)
	{
		if (_afxThreadData == NULL)
		{
			_afxThreadData = new(__afxThreadData) CThreadSlotData;
			ASSERT(_afxThreadData != NULL);
		}
		m_nSlot = _afxThreadData->AllocSlot();
		ASSERT(m_nSlot != 0);
	}
	CNoTrackObject* pValue =
		(CNoTrackObject*)_afxThreadData->GetThreadValue(m_nSlot);
	if (pValue == NULL)
	{
		// allocate zero-init object
		pValue = (*pfnCreateObject)();

		// set tls data to newly created object
		_afxThreadData->SetValue(m_nSlot, pValue);
		ASSERT(_afxThreadData->GetThreadValue(m_nSlot) == pValue);
	}
	return pValue;
}

CNoTrackObject* CThreadLocalObject::GetDataNA()
{
	if (m_nSlot == 0 || _afxThreadData == NULL)
		return NULL;

	CNoTrackObject* pValue =
		(CNoTrackObject*)_afxThreadData->GetThreadValue(m_nSlot);
	return pValue;
}

CThreadLocalObject::~CThreadLocalObject()
{
	if (m_nSlot != 0 && _afxThreadData != NULL)
		_afxThreadData->FreeSlot(m_nSlot);
	m_nSlot = 0;
}

/////////////////////////////////////////////////////////////////////////////
// CProcessLocalData

CNoTrackObject* CProcessLocalObject::GetData(
	CNoTrackObject* (AFXAPI* pfnCreateObject)())
{
	if (m_pObject == NULL)
	{
		AfxLockGlobals(CRIT_PROCESSLOCAL);
		TRY
		{
			if (m_pObject == NULL)
				m_pObject = (*pfnCreateObject)();
		}
		CATCH_ALL(e)
		{
			AfxUnlockGlobals(CRIT_PROCESSLOCAL);
			THROW_LAST();
		}
		END_CATCH_ALL
		AfxUnlockGlobals(CRIT_PROCESSLOCAL);
	}
	return m_pObject;
}

CProcessLocalObject::~CProcessLocalObject()
{
	if (m_pObject != NULL)
		delete m_pObject;
}

/////////////////////////////////////////////////////////////////////////////
// Init/Term for thread/process local data

void AFXAPI AfxInitLocalData(HINSTANCE hInst)
{
	if (_afxThreadData != NULL)
		_afxThreadData->AssignInstance(hInst);
}

void AFXAPI AfxTermLocalData(HINSTANCE hInst, BOOL bAll)
{
	if (_afxThreadData != NULL)
		_afxThreadData->DeleteValues(hInst, bAll);
}

// This reference count is needed to support Win32s, such that the
// thread-local and process-local data is not destroyed prematurely.
// It is basically a reference count of the number of processes that
// have attached to the MFC DLL.

AFX_STATIC_DATA long _afxTlsRef = 0;

void AFXAPI AfxTlsAddRef()
{
	++_afxTlsRef;
}

void AFXAPI AfxTlsRelease()
{
	if (_afxTlsRef == 0 || --_afxTlsRef == 0)
	{
		if (_afxThreadData != NULL)
		{
			_afxThreadData->~CThreadSlotData();
			_afxThreadData = NULL;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
