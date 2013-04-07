// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXMT_H__
#define __AFXMT_H__

#ifndef __AFX_H__
	#include <afx.h>
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

/////////////////////////////////////////////////////////////////////////////
// AFXMT - MFC Multithreaded Extensions (Syncronization Objects)

// Classes declared in this file

//CObject
	class CSyncObject;
		class CSemaphore;
		class CMutex;
		class CEvent;
		class CCriticalSection;

class CSingleLock;
class CMultiLock;

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////
// Basic synchronization object

class CSyncObject : public CObject
{
	DECLARE_DYNAMIC(CSyncObject)

// Constructor
public:
	CSyncObject(LPCTSTR pstrName);

// Attributes
public:
	operator HANDLE() const;
	HANDLE  m_hObject;

// Operations
	virtual BOOL Lock(DWORD dwTimeout = INFINITE);
	virtual BOOL Unlock() = 0;
	virtual BOOL Unlock(LONG /* lCount */, LPLONG /* lpPrevCount=NULL */)
		{ return TRUE; }

// Implementation
public:
	virtual ~CSyncObject();
#ifdef _DEBUG
	CString m_strName;
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	friend class CSingleLock;
	friend class CMultiLock;
};

/////////////////////////////////////////////////////////////////////////////
// CSemaphore

class CSemaphore : public CSyncObject
{
	DECLARE_DYNAMIC(CSemaphore)

// Constructor
public:
	CSemaphore(LONG lInitialCount = 1, LONG lMaxCount = 1,
		LPCTSTR pstrName=NULL, LPSECURITY_ATTRIBUTES lpsaAttributes = NULL);

// Implementation
public:
	virtual ~CSemaphore();
	virtual BOOL Unlock();
	virtual BOOL Unlock(LONG lCount, LPLONG lprevCount = NULL);
};

/////////////////////////////////////////////////////////////////////////////
// CMutex

class CMutex : public CSyncObject
{
	DECLARE_DYNAMIC(CMutex)

// Constructor
public:
	CMutex(BOOL bInitiallyOwn = FALSE, LPCTSTR lpszName = NULL,
		LPSECURITY_ATTRIBUTES lpsaAttribute = NULL);

// Implementation
public:
	virtual ~CMutex();
	BOOL Unlock();
};

/////////////////////////////////////////////////////////////////////////////
// CEvent

class CEvent : public CSyncObject
{
	DECLARE_DYNAMIC(CEvent)

// Constructor
public:
	CEvent(BOOL bInitiallyOwn = FALSE, BOOL bManualReset = FALSE,
		LPCTSTR lpszNAme = NULL, LPSECURITY_ATTRIBUTES lpsaAttribute = NULL);

// Operations
public:
	BOOL SetEvent();
	BOOL PulseEvent();
	BOOL ResetEvent();
	BOOL Unlock();

// Implementation
public:
	virtual ~CEvent();
};

/////////////////////////////////////////////////////////////////////////////
// CCriticalSection

class CCriticalSection : public CSyncObject
{
	DECLARE_DYNAMIC(CCriticalSection)

// Constructor
public:
	CCriticalSection();

// Attributes
public:
	operator CRITICAL_SECTION*();
	CRITICAL_SECTION m_sect;

// Operations
public:
	BOOL Unlock();
	BOOL Lock();
	BOOL Lock(DWORD dwTimeout);

// Implementation
public:
	virtual ~CCriticalSection();
};

/////////////////////////////////////////////////////////////////////////////
// CSingleLock

class CSingleLock
{
// Constructors
public:
	CSingleLock(CSyncObject* pObject, BOOL bInitialLock = FALSE);

// Operations
public:
	BOOL Lock(DWORD dwTimeOut = INFINITE);
	BOOL Unlock();
	BOOL Unlock(LONG lCount, LPLONG lPrevCount = NULL);
	BOOL IsLocked();

// Implementation
public:
	~CSingleLock();

protected:
	CSyncObject* m_pObject;
	HANDLE  m_hObject;
	BOOL    m_bAcquired;
};

/////////////////////////////////////////////////////////////////////////////
// CMultiLock

class CMultiLock
{
// Constructor
public:
	CMultiLock(CSyncObject* ppObjects[], DWORD dwCount, BOOL bInitialLock = FALSE);

// Operations
public:
	DWORD Lock(DWORD dwTimeOut = INFINITE, BOOL bWaitForAll = TRUE,
		DWORD dwWakeMask = 0);
	BOOL Unlock();
	BOOL Unlock(LONG lCount, LPLONG lPrevCount = NULL);
	BOOL IsLocked(DWORD dwItem);

// Implementation
public:
	~CMultiLock();

protected:
	HANDLE  m_hPreallocated[8];
	BOOL    m_bPreallocated[8];

	CSyncObject* const * m_ppObjectArray;
	HANDLE* m_pHandleArray;
	BOOL*   m_bLockedArray;
	DWORD   m_dwCount;
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXMT_INLINE AFX_INLINE
#include <afxmt.inl>
#undef _AFXMT_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif  // __AFXMT_H__

/////////////////////////////////////////////////////////////////////////////
