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

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CSemaphore

CSemaphore::CSemaphore(LONG lInitialCount, LONG lMaxCount,
	LPCTSTR pstrName, LPSECURITY_ATTRIBUTES lpsaAttributes)
	:  CSyncObject(pstrName)
{
	ASSERT(lMaxCount > 0);
	ASSERT(lInitialCount <= lMaxCount);

	m_hObject = ::CreateSemaphore(lpsaAttributes, lInitialCount, lMaxCount,
		pstrName);
	if (m_hObject == NULL)
		AfxThrowResourceException();
}

CSemaphore::~CSemaphore()
{
}

BOOL CSemaphore::Unlock(LONG lCount, LPLONG lpPrevCount /* =NULL */)
{
	return ::ReleaseSemaphore(m_hObject, lCount, lpPrevCount);
}

/////////////////////////////////////////////////////////////////////////////
// CMutex

CMutex::CMutex(BOOL bInitiallyOwn, LPCTSTR pstrName,
	LPSECURITY_ATTRIBUTES lpsaAttribute /* = NULL */)
	: CSyncObject(pstrName)
{
	m_hObject = ::CreateMutex(lpsaAttribute, bInitiallyOwn, pstrName);
	if (m_hObject == NULL)
		AfxThrowResourceException();
}

CMutex::~CMutex()
{
}

BOOL CMutex::Unlock()
{
	return ::ReleaseMutex(m_hObject);
}

/////////////////////////////////////////////////////////////////////////////
// CEvent

CEvent::CEvent(BOOL bInitiallyOwn, BOOL bManualReset, LPCTSTR pstrName,
	LPSECURITY_ATTRIBUTES lpsaAttribute)
	: CSyncObject(pstrName)
{
	m_hObject = ::CreateEvent(lpsaAttribute, bManualReset,
		bInitiallyOwn, pstrName);
	if (m_hObject == NULL)
		AfxThrowResourceException();
}

CEvent::~CEvent()
{
}

BOOL CEvent::Unlock()
{
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CSingleLock

CSingleLock::CSingleLock(CSyncObject* pObject, BOOL bInitialLock)
{
	ASSERT(pObject != NULL);
	ASSERT(pObject->IsKindOf(RUNTIME_CLASS(CSyncObject)));

	m_pObject = pObject;
	m_hObject = pObject->m_hObject;
	m_bAcquired = FALSE;

	if (bInitialLock)
		Lock();
}

BOOL CSingleLock::Lock(DWORD dwTimeOut /* = INFINITE */)
{
	ASSERT(m_pObject != NULL || m_hObject != NULL);
	ASSERT(!m_bAcquired);

	m_bAcquired = m_pObject->Lock(dwTimeOut);
	return m_bAcquired;
}

BOOL CSingleLock::Unlock()
{
	ASSERT(m_pObject != NULL);
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock();

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

BOOL CSingleLock::Unlock(LONG lCount, LPLONG lpPrevCount /* = NULL */)
{
	ASSERT(m_pObject != NULL);
	if (m_bAcquired)
		m_bAcquired = !m_pObject->Unlock(lCount, lpPrevCount);

	// successfully unlocking means it isn't acquired
	return !m_bAcquired;
}

/////////////////////////////////////////////////////////////////////////////
// CMultiLock

CMultiLock::CMultiLock(CSyncObject* pObjects[], DWORD dwCount,
	BOOL bInitialLock)
{
	ASSERT(dwCount > 0 && dwCount <= MAXIMUM_WAIT_OBJECTS);
	ASSERT(pObjects != NULL);

	m_ppObjectArray = pObjects;
	m_dwCount = dwCount;

	// as an optimization, skip allocating array if
	// we can use a small, preallocated bunch of handles

	if (m_dwCount > _countof(m_hPreallocated))
	{
		m_pHandleArray = new HANDLE[m_dwCount];
		m_bLockedArray = new BOOL[m_dwCount];
	}
	else
	{
		m_pHandleArray = m_hPreallocated;
		m_bLockedArray = m_bPreallocated;
	}

	// get list of handles from array of objects passed
	for (DWORD i = 0; i <m_dwCount; i++)
	{
		ASSERT_VALID(pObjects[i]);
		ASSERT(pObjects[i]->IsKindOf(RUNTIME_CLASS(CSyncObject)));

		// can't wait for critical sections

		ASSERT(!pObjects[i]->IsKindOf(RUNTIME_CLASS(CCriticalSection)));

		m_pHandleArray[i] = pObjects[i]->m_hObject;
		m_bLockedArray[i] = FALSE;
	}

	if (bInitialLock)
		Lock();
}

CMultiLock::~CMultiLock()
{
	Unlock();
	if (m_pHandleArray != m_hPreallocated)
	{
		delete[] m_bLockedArray;
		delete[] m_pHandleArray;
	}
}

DWORD CMultiLock::Lock(DWORD dwTimeOut /* = INFINITE */,
		BOOL bWaitForAll /* = TRUE */, DWORD dwWakeMask /* = 0 */)
{
	DWORD dwResult;
	if (dwWakeMask == 0)
		dwResult = ::WaitForMultipleObjects(m_dwCount,
			m_pHandleArray, bWaitForAll, dwTimeOut);
	else
		dwResult = ::MsgWaitForMultipleObjects(m_dwCount,
			m_pHandleArray, bWaitForAll, dwTimeOut, dwWakeMask);

	if (dwResult >= WAIT_OBJECT_0 && dwResult < (WAIT_OBJECT_0 + m_dwCount))
	{
		if (bWaitForAll)
		{
			for (DWORD i = 0; i < m_dwCount; i++)
				m_bLockedArray[i] = TRUE;
		}
		else
		{
			ASSERT((dwResult - WAIT_OBJECT_0) >= 0);
			m_bLockedArray[dwResult - WAIT_OBJECT_0] = TRUE;
		}
	}
	return dwResult;
}

BOOL CMultiLock::Unlock()
{
	for (DWORD i=0; i < m_dwCount; i++)
	{
		if (m_bLockedArray[i])
			m_bLockedArray[i] = !m_ppObjectArray[i]->Unlock();
	}
	return TRUE;
}

BOOL CMultiLock::Unlock(LONG lCount, LPLONG lpPrevCount /* =NULL */)
{
	BOOL bGotOne = FALSE;
	for (DWORD i=0; i < m_dwCount; i++)
	{
		if (m_bLockedArray[i])
		{
			CSemaphore* pSemaphore = STATIC_DOWNCAST(CSemaphore, m_ppObjectArray[i]);
			if (pSemaphore != NULL)
			{
				bGotOne = TRUE;
				m_bLockedArray[i] = !m_ppObjectArray[i]->Unlock(lCount, lpPrevCount);
			}
		}
	}

	return bGotOne;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CEvent, CSyncObject)
IMPLEMENT_DYNAMIC(CSemaphore, CSyncObject)
IMPLEMENT_DYNAMIC(CMutex, CSyncObject)

/////////////////////////////////////////////////////////////////////////////
