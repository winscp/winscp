// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

/////////////////////////////////////////////////////////////////////////////
// CHandleMap
//
//  Note: Do not access the members of this class directly.
//      Use CWnd::FromHandle, CDC::FromHandle, etc.
//      The actual definition is only included because it is
//      necessary for the definition of CWinThread.
//
//  Most Windows objects are represented with a HANDLE, including
//      the most important ones, HWND, HDC, HPEN, HFONT etc.
//  We want C++ objects to wrap these handle based objects whenever we can.
//  Since Windows objects can be created outside of C++ (eg: calling
//      ::CreateWindow will return an HWND with no C++ wrapper) we must
//      support a reasonably uniform mapping from permanent handles
//      (i.e. the ones allocated in C++) and temporary handles (i.e.
//      the ones allocated in C, but passed through a C++ interface.
//  We keep two dictionaries for this purpose.  The permanent dictionary
//      stores those C++ objects that have been explicitly created by
//      the developer.  The C++ constructor for the wrapper class will
//      insert the mapping into the permanent dictionary and the C++
//      destructor will remove it and possibly free up the associated
//      Windows object.
//  When a handle passes through a C++ interface that doesn't exist in
//      the permanent dictionary, we allocate a temporary wrapping object
//      and store that mapping into the temporary dictionary.
//  At idle time the temporary wrapping objects are flushed (since you better
//      not be holding onto something you didn't create).
//

class CWinThread;       // forward reference for friend declaration

class CHandleMap
{
private:    // implementation
	CMapPtrToPtr m_permanentMap;
	CMapPtrToPtr m_temporaryMap;
	CRuntimeClass*  m_pClass;
	size_t m_nOffset;       // offset of handles in the object
	int m_nHandles;         // 1 or 2 (for CDC)

// Constructor/Destructor
public:
	CHandleMap(CRuntimeClass* pClass, size_t nOffset, int nHandles = 1);
#ifdef _AFXDLL
	~CHandleMap()
#else
	virtual ~CHandleMap()
#endif
		{ DeleteTemp(); }

// Operations
public:
	CObject* FromHandle(HANDLE h);
	void DeleteTemp();

	void SetPermanent(HANDLE h, CObject* permOb);
	void RemoveHandle(HANDLE h);

	CObject* LookupPermanent(HANDLE h);
	CObject* LookupTemporary(HANDLE h);


	friend class CWinThread;
};

// Note: out-of-line _DEBUG version is in winhand.cpp
#ifndef _DEBUG
inline void CHandleMap::SetPermanent(HANDLE h, CObject* permOb)
	{ m_permanentMap[(LPVOID)h] = permOb; }

inline void CHandleMap::RemoveHandle(HANDLE h)
{
	// remove only from permanent map -- temporary objects are removed
	//  at idle in CHandleMap::DeleteTemp, always!
	m_permanentMap.RemoveKey((LPVOID)h);
}
#endif

inline CObject* CHandleMap::LookupPermanent(HANDLE h)
	{ return (CObject*)m_permanentMap.GetValueAt((LPVOID)h); }
inline CObject* CHandleMap::LookupTemporary(HANDLE h)
	{ return (CObject*)m_temporaryMap.GetValueAt((LPVOID)h); }
