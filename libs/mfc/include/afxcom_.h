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
// AFXCOM_.H
//
// THIS FILE IS FOR MFC IMPLEMENTATION ONLY.

#ifndef __AFXCOM_H__
#define __AFXCOM_H__

#ifndef _OBJBASE_H_
#include <objbase.h>
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, off)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, on)
#endif

/////////////////////////////////////////////////////////////////////////////

#ifndef _AFX_NOFORCE_LIBS
#pragma comment(lib, "uuid.lib")
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

#ifndef ASSERT
#ifndef _INC_CRTDBG
#include <crtdbg.h>
#endif // _INC_CRTDBG
#define ASSERT(x) _ASSERT(x)
#endif // ASSERT

/////////////////////////////////////////////////////////////////////////////

template<class _Interface, const IID* _IID>
class _CIP
{
public:
	// Declare interface type so that the type may be available outside
	// the scope of this template.
	typedef _Interface Interface;

	// When the compiler supports references in template params,
	// _CLSID will be changed to a reference.  To avoid conversion
	// difficulties this function should be used to obtain the
	// CLSID.
	static const IID& GetIID()
		{ ASSERT(_IID != NULL); return *_IID; }

	// Construct empty in preperation for assignment.
	_CIP();

	// Copy the pointer and AddRef().
	_CIP(const _CIP& cp) : _pInterface(cp._pInterface)
		{ _AddRef(); }

	// Saves and AddRef()'s the interface
	_CIP(Interface* pInterface) : _pInterface(pInterface)
		{ _AddRef(); }

	// Copies the pointer.  If bAddRef is TRUE, the interface will
	// be AddRef()ed.
	_CIP(Interface* pInterface, BOOL bAddRef)
		: _pInterface(pInterface)
	{
		if (bAddRef)
		{
			ASSERT(pInterface != NULL);
			_AddRef();
		}
	}

	// Calls CoCreateClass with the provided CLSID.
	_CIP(const CLSID& clsid, DWORD dwClsContext = CLSCTX_INPROC_SERVER)
		: _pInterface(NULL)
	{
		CreateObject(clsid, dwClsContext);
	}

	// Calls CoCreateClass with the provided CLSID retrieved from
	// the string.
	_CIP(LPOLESTR str, DWORD dwClsContext = CLSCTX_INPROC_SERVER)
		: _pInterface(NULL)
	{
		CreateObject(str, dwClsContext);
	}

	// Saves and AddRef()s the interface.
	_CIP& operator=(Interface* pInterface)
	{
		if (_pInterface != pInterface)
		{
			Interface* pOldInterface = _pInterface;
			_pInterface = pInterface;
			_AddRef();
			if (pOldInterface != NULL)
				pOldInterface->Release();
		}
		return *this;
	}

	// Copies and AddRef()'s the interface.
	_CIP& operator=(const _CIP& cp)
		{ return operator=(cp._pInterface); }

	// Releases any current interface and loads the class with the
	// provided CLSID.
	_CIP& operator=(const CLSID& clsid)
	{
		CreateObject(clsid);
		return *this;
	}

	// Calls CoCreateClass with the provided CLSID retrieved from
	// the string.
	_CIP& operator=(LPOLESTR str)
	{
		CreateObject(str);
		return *this;
	}

	~_CIP();

	// Saves/sets the interface without AddRef()ing.  This call
	// will release any previously aquired interface.
	void Attach(Interface* pInterface)
	{
		_Release();
		_pInterface = pInterface;
	}

	// Saves/sets the interface only AddRef()ing if bAddRef is TRUE.
	// This call will release any previously aquired interface.
	void Attach(Interface* pInterface, BOOL bAddRef)
	{
		_Release();
		_pInterface = pInterface;
		if (bAddRef)
		{
			ASSERT(pInterface != NULL);
			pInterface->AddRef();
		}
	}

	// Simply NULL the interface pointer so that it isn't Released()'ed.
	void Detach()
	{
		ASSERT(_pInterface);
		_pInterface = NULL;
	}

	// Return the interface.  This value may be NULL
	operator Interface*() const
		{ return _pInterface; }

	// Queries for the unknown and return it
	operator IUnknown*()
		{ return _pInterface; }

	// Provides minimal level assertion before use.
	operator Interface&() const
		{ ASSERT(_pInterface); return *_pInterface; }

	// Allows an instance of this class to act as though it were the
	// actual interface.  Also provides minimal assertion verification.
	Interface& operator*() const
		{ ASSERT(_pInterface); return *_pInterface; }

	// Returns the address of the interface pointer contained in this
	// class.  This is useful when using the COM/OLE interfaces to create
	// this interface.
	Interface** operator&()
	{
		_Release();
		_pInterface = NULL;
		return &_pInterface;
	}

	// Allows this class to be used as the interface itself.
	// Also provides simple assertion verification.
	Interface* operator->() const
		{ ASSERT(_pInterface != NULL); return _pInterface; }

	// This operator is provided so that simple boolean expressions will
	// work.  For example: "if (p) ...".
	// Returns TRUE if the pointer is not NULL.
	operator BOOL() const
		{ return _pInterface != NULL; }

	// Returns TRUE if the interface is NULL.
	// This operator will be removed when support for type bool
	// is added to the compiler.
	BOOL operator!()
		{ return _pInterface == NULL; }

	// Provides assertion verified, Release()ing of this interface.
	void Release()
	{
		ASSERT(_pInterface != NULL);
		_pInterface->Release();
		_pInterface = NULL;
	}

	// Provides assertion verified AddRef()ing of this interface.
	void AddRef()
		{ ASSERT(_pInterface != NULL); _pInterface->AddRef(); }

	// Another way to get the interface pointer without casting.
	Interface* GetInterfacePtr() const
		{ return _pInterface; }

	// Loads an interface for the provided CLSID.
	// Returns an HRESULT.  Any previous interface is released.
	HRESULT CreateObject(
		const CLSID& clsid, DWORD dwClsContext = CLSCTX_INPROC_SERVER)
	{
		_Release();
		HRESULT hr = CoCreateInstance(clsid, NULL, dwClsContext,
			GetIID(), reinterpret_cast<void**>(&_pInterface));
		ASSERT(SUCCEEDED(hr));
		return hr;
	}

	// Creates the class specified by clsidString.  clsidString may
	// contain a class id, or a prog id string.
	HRESULT CreateObject(
		LPOLESTR clsidString, DWORD dwClsContext=CLSCTX_INPROC_SERVER)
	{
		ASSERT(clsidString != NULL);
		CLSID clsid;
		HRESULT hr;
		if (clsidString[0] == '{')
			hr = CLSIDFromString(clsidString, &clsid);
		else
			hr = CLSIDFromProgID(clsidString, &clsid);
		ASSERT(SUCCEEDED(hr));
		if (FAILED(hr))
			return hr;
		return CreateObject(clsid, dwClsContext);
	}

	// Performs a QI on pUnknown for the interface type returned
	// for this class.  The interface is stored.  If pUnknown is
	// NULL, or the QI fails, E_NOINTERFACE is returned and
	// _pInterface is set to NULL.
	HRESULT QueryInterface(IUnknown* pUnknown)
	{
		if (pUnknown == NULL) // Can't QI NULL
		{
			operator=(static_cast<Interface*>(NULL));
			return E_NOINTERFACE;
		}

		// Query for this interface
		Interface* pInterface;
		HRESULT hr = pUnknown->QueryInterface(GetIID(),
			reinterpret_cast<void**>(&pInterface));
		if (FAILED(hr))
		{
			// If failed intialize interface to NULL and return HRESULT.
			Attach(NULL);
			return hr;
		}

		// Save the interface without AddRef()ing.
		Attach(pInterface);
		return hr;
	}

private:
	// Releases only if the interface is not null.
	// The interface is not set to NULL.
	void _Release()
	{
		if (_pInterface != NULL)
			_pInterface->Release();
	}

	// AddRefs only if the interface is not NULL
	void _AddRef()
	{
		if (_pInterface != NULL)
			_pInterface->AddRef();
	}

	// The Interface.
	Interface* _pInterface;
}; // class _CIP

template<class _Interface, const IID* _IID>
#ifdef __BORLANDC__
_CIP<_Interface, _IID>::_CIP(): _pInterface(NULL)
#else
_CIP<_Interface, _IID>::_CIP<_Interface, _IID>(): _pInterface(NULL)
#endif
{
}

template<class _Interface, const IID* _IID>
#ifdef __BORLANDC__
_CIP<_Interface, _IID>::~_CIP()
#else
_CIP<_Interface, _IID>::~_CIP<_Interface, _IID>()
#endif
{
	// If we still have an interface then Release() it.  The interface
	// may be NULL if Detach() has previosly been called, or if it was
	// never set.

	_Release();
}

template<class _Interface, const IID* _IID>
class CIP : public _CIP<_Interface, _IID>
{
public:
	// Simplified name for base class and provide derived classes
	// access to base type
	typedef _CIP<_Interface, _IID> BC;

	// Provideds derived classes access to the interface type.
	typedef _Interface Interface;

	// Construct empty in preperation for assignment.
	CIP() { }
	~CIP();

	// Copy the pointer and AddRef().
	CIP(const CIP& cp) : _CIP<_Interface, _IID>(cp) { }

	// Saves and AddRef()s the interface.
	CIP(Interface* pInterface) : _CIP<_Interface, _IID>(pInterface) { }

	// Saves the interface and AddRef()s only if bAddRef is TRUE.
	CIP(Interface* pInterface, BOOL bAddRef)
		: _CIP<_Interface, _IID>(pInterface, bAddRef) { }

	// Queries for this interface.
	CIP(IUnknown* pUnknown)
	{
		if (pUnknown == NULL)
			return;
		Interface* pInterface;
		HRESULT hr = pUnknown->QueryInterface(GetIID(),
			reinterpret_cast<void**>(&pInterface));
		ASSERT(SUCCEEDED(hr));
		Attach(pInterface);
	}

	// Creates the interface from the CLSID.
	CIP(const CLSID& clsid) : _CIP<_Interface, _IID>(clsid) { }

	// Creates the interface from the CLSID.
	CIP(LPOLESTR str) : _CIP<_Interface, _IID>(str) { }

	// Copies and AddRef()'s the interface.
	CIP& operator=(const CIP& cp)
		{ _CIP<_Interface, _IID>::operator=(cp); return *this; }

	// Saves and AddRef()s the interface.
	CIP& operator=(Interface* pInterface)
		{ _CIP<_Interface, _IID>::operator=(pInterface); return *this; }

	CIP& operator=(IUnknown* pUnknown)
	{
		HRESULT hr = QueryInterface(pUnknown);
		ASSERT(SUCCEEDED(hr));
		return *this;
	}

	// Releases any current interface and loads the class with the
	// provided CLSID.
	CIP& operator=(const CLSID& clsid)
		{ _CIP<_Interface, _IID>::operator=(clsid); return *this; }

	// Releases any current interface and loads the class with the
	// provided CLSID.
	CIP& operator=(LPOLESTR str)
		{ _CIP<_Interface, _IID>::operator=(str); return *this; }
}; // class CIP

template<class _Interface, const IID* _IID>
CIP<_Interface, _IID>::~CIP()
{
}

#if _MSC_VER > 1020
template<>
#endif
class CIP<IUnknown, &IID_IUnknown> : public _CIP<IUnknown, &IID_IUnknown>
{
public:
	// Simplified name for base class and provide derived classes
	// access to base type
	typedef _CIP<IUnknown, &IID_IUnknown> BC;

	// Provideds derived classes access to the interface type.
	typedef IUnknown Interface;

	// Construct empty in preperation for assignment.
	CIP() { }

	// Copy the pointer and AddRef().
	CIP(const CIP& cp) : _CIP<IUnknown, &IID_IUnknown>(cp) { }

	// Saves and AddRef()s the interface.
	CIP(Interface* pInterface)
		: _CIP<IUnknown, &IID_IUnknown>(pInterface) { }

	// Saves and then AddRef()s only if bAddRef is TRUE.
	CIP(Interface* pInterface, BOOL bAddRef)
		: _CIP<IUnknown, &IID_IUnknown>(pInterface, bAddRef) { }

	// Creates the interface from the CLSID.
	CIP(const CLSID& clsid) : _CIP<IUnknown, &IID_IUnknown>(clsid) { }

	// Creates the interface from the CLSID.
	CIP(LPOLESTR str) : _CIP<IUnknown, &IID_IUnknown>(str) { }

	// Copies and AddRef()'s the interface.
	CIP& operator=(const CIP& cp)
		{ _CIP<IUnknown, &IID_IUnknown>::operator=(cp); return *this; }

	// Saves and AddRef()s the interface.  The previously saved
	// interface is released.
	CIP& operator=(Interface* pInterface)
		{ _CIP<IUnknown, &IID_IUnknown>::operator=(pInterface); return *this; }

	// Releases any current interface and loads the class with the
	// provided CLSID.
	CIP& operator=(const CLSID& clsid)
		{ _CIP<IUnknown, &IID_IUnknown>::operator=(clsid); return *this; }

	// Releases any current interface and loads the class with the
	// provided CLSID.
	CIP& operator=(LPOLESTR str)
		{ _CIP<IUnknown, &IID_IUnknown>::operator=(str); return *this; }

	// Queries for the unknown and return it
	operator IUnknown*()
		{ return GetInterfacePtr(); }

	// Verifies that pUnknown is not null and performs assignment.
	HRESULT QueryInterface(IUnknown* pUnknown)
	{
		_CIP<IUnknown, &IID_IUnknown>::operator=(pUnknown);
		return pUnknown != NULL ? S_OK : E_NOINTERFACE;
	}
};  // CIP<IUnknown, &IID_IUnknown>

#define IPTR(x) CIP<x, &IID_##x>
#define DEFINE_IPTR(x) typedef IPTR(x) x##Ptr;

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif // __AFXCOM_H__

/////////////////////////////////////////////////////////////////////////////
