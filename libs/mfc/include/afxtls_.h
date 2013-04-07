// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXTLS_H__
#define __AFXTLS_H__

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

// Classes declared in this file

class CSimpleList;
class CThreadSlotData;                  // for manipulationg thread local storage
class CThreadLocalObject;               // for storing thread local data
class CProcessLocalObject;              // for storing thread local data
class CNoTrackObject;

// template class CTypedSimpleList<>
// template class CThreadLocal<>
// template class CProcessLocal<>

/////////////////////////////////////////////////////////////////////////////
// CSimpleList (simple/small subset of CList)

class CSimpleList
{
public:
	CSimpleList(int nNextOffset = 0);
	void Construct(int nNextOffset);

// Operations
	BOOL IsEmpty() const;
	void AddHead(void* p);
	void RemoveAll();
	void* GetHead() const;
	void* GetNext(void* p) const;
	BOOL Remove(void* p);

// Implementation
	void* m_pHead;
	size_t m_nNextOffset;

	void** GetNextPtr(void* p) const;   // somewhat trusting...
};

AFX_INLINE CSimpleList::CSimpleList(int nNextOffset)
	{ m_pHead = NULL; m_nNextOffset = nNextOffset; }
AFX_INLINE void CSimpleList::Construct(int nNextOffset)
	{ ASSERT(m_pHead == NULL); m_nNextOffset = nNextOffset; }
AFX_INLINE BOOL CSimpleList::IsEmpty() const
	{ return m_pHead == NULL; }
AFX_INLINE void** CSimpleList::GetNextPtr(void* p) const
	{ ASSERT(p != NULL); return (void**)((BYTE*)p+m_nNextOffset); }
AFX_INLINE void CSimpleList::RemoveAll()
	{ m_pHead = NULL; }
AFX_INLINE void* CSimpleList::GetHead() const
	{ return m_pHead; }
AFX_INLINE void* CSimpleList::GetNext(void* prevElement) const
	{ return *GetNextPtr(prevElement); }

template<class TYPE>
class CTypedSimpleList : public CSimpleList
{
public:
	CTypedSimpleList(int nNextOffset = 0)
		: CSimpleList(nNextOffset) { }
	void AddHead(TYPE p)
		{ CSimpleList::AddHead(p); }
	TYPE GetHead()
		{ return (TYPE)CSimpleList::GetHead(); }
	TYPE GetNext(TYPE p)
		{ return (TYPE)CSimpleList::GetNext(p); }
	BOOL Remove(TYPE p)
		{ return CSimpleList::Remove((TYPE)p); }
	operator TYPE()
		{ return (TYPE)CSimpleList::GetHead(); }
};

/////////////////////////////////////////////////////////////////////////////
// CThreadSlotData - manages owned array of "slots" for thread local storage

struct CThreadData; // private to implementation
struct CSlotData;   // private to implementation

class CThreadSlotData
{
public:
	CThreadSlotData();

// Operations
	int AllocSlot();
	void FreeSlot(int nSlot);
	void* GetValue(int nSlot);
	void SetValue(int nSlot, void* pValue);
	// delete all values in process/thread
	void DeleteValues(HINSTANCE hInst, BOOL bAll = FALSE);
	// assign instance handle to just constructed slots
	void AssignInstance(HINSTANCE hInst);

// Implementation
	DWORD m_tlsIndex;   // used to access system thread-local storage

	int m_nAlloc;       // number of slots allocated (in UINTs)
	int m_nRover;       // (optimization) for quick finding of free slots
	int m_nMax;         // size of slot table below (in bits)
	CSlotData* m_pSlotData; // state of each slot (allocated or not)
	CTypedSimpleList<CThreadData*> m_list;  // list of CThreadData structures
	CRITICAL_SECTION m_sect;

	void* GetThreadValue(int nSlot); // special version for threads only!
	void* PASCAL operator new(size_t, void* p)
		{ return p; }
	void DeleteValues(CThreadData* pData, HINSTANCE hInst);
	~CThreadSlotData();
};

class AFX_NOVTABLE CNoTrackObject
{
public:
	void* PASCAL operator new(size_t nSize);
	void PASCAL operator delete(void*);

#if defined(_DEBUG) && !defined(_AFX_NO_DEBUG_CRT)
	void* PASCAL operator new(size_t nSize, LPCSTR, int);
#if _MSC_VER >= 1200
	void PASCAL operator delete(void* pObject, LPCSTR, int);
#endif
#endif
	virtual ~CNoTrackObject() { }
};

class AFX_NOVTABLE CThreadLocalObject
{
public:
// Attributes
	CNoTrackObject* GetData(CNoTrackObject* (AFXAPI* pfnCreateObject)());
	CNoTrackObject* GetDataNA();

// Implementation
	int m_nSlot;
	~CThreadLocalObject();
};

class AFX_NOVTABLE CProcessLocalObject
{
public:
// Attributes
	CNoTrackObject* GetData(CNoTrackObject* (AFXAPI* pfnCreateObject)());

// Implementation
	CNoTrackObject* volatile m_pObject;
	~CProcessLocalObject();
};

template<class TYPE>
class CThreadLocal : public CThreadLocalObject
{
// Attributes
public:
	AFX_INLINE TYPE* GetData()
	{
		TYPE* pData = (TYPE*)CThreadLocalObject::GetData(&CreateObject);
		ASSERT(pData != NULL);
		return pData;
	}
	AFX_INLINE TYPE* GetDataNA()
	{
		TYPE* pData = (TYPE*)CThreadLocalObject::GetDataNA();
		return pData;
	}
	AFX_INLINE operator TYPE*()
		{ return GetData(); }
	AFX_INLINE TYPE* operator->()
		{ return GetData(); }

// Implementation
public:
	static CNoTrackObject* AFXAPI CreateObject()
		{ return new TYPE; }
};

#define THREAD_LOCAL(class_name, ident_name) \
	AFX_DATADEF CThreadLocal<class_name> ident_name;
#define EXTERN_THREAD_LOCAL(class_name, ident_name) \
	extern AFX_DATA THREAD_LOCAL(class_name, ident_name)

template<class TYPE>
class CProcessLocal : public CProcessLocalObject
{
// Attributes
public:
	AFX_INLINE TYPE* GetData()
	{
		TYPE* pData = (TYPE*)CProcessLocalObject::GetData(&CreateObject);
		ASSERT(pData != NULL);
		return pData;
	}
	AFX_INLINE TYPE* GetDataNA()
		{ return (TYPE*)m_pObject; }
	AFX_INLINE operator TYPE*()
		{ return GetData(); }
	AFX_INLINE TYPE* operator->()
		{ return GetData(); }

// Implementation
public:
	static CNoTrackObject* AFXAPI CreateObject()
		{ return new TYPE; }
};

#define PROCESS_LOCAL(class_name, ident_name) \
	AFX_DATADEF CProcessLocal<class_name> ident_name;
#define EXTERN_PROCESS_LOCAL(class_name, ident_name) \
	extern AFX_DATA PROCESS_LOCAL(class_name, ident_name)

/////////////////////////////////////////////////////////////////////////////

void AFXAPI AfxInitLocalData(HINSTANCE hInstInit);
void AFXAPI AfxTermLocalData(HINSTANCE hInstTerm, BOOL bAll = FALSE);
void AFXAPI AfxTlsAddRef();
void AFXAPI AfxTlsRelease();

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#undef AFX_DATA
#define AFX_DATA

#endif //__AFXTLS_H__

/////////////////////////////////////////////////////////////////////////////
