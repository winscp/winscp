// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Note: This header file contains useful classes that are documented only
//  in the MFC Technical Notes.  These classes may change from version to
//  version, so be prepared to change your code accordingly if you utilize
//  this header.  In the future, commonly used portions of this header
//  may be moved and officially documented.

#ifndef __AFXPRIV2_H__
// Do not define __AFXPRIV2_H__ here.  It is defined at the bottom.

#ifndef __AFXPRIV_H__
	#include <afxpriv.h>
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
// AFXPRIV2 - MFC Private Classes

// General OLE features

#if (!defined _AFX_NO_OLE_SUPPORT) && (defined _OBJBASE_H_)

// Implementation structures
struct AFX_EVENT;               // Event sink implementation

// Classes declared in this file
class COleControlLock;

#endif

// OLE Automation features

#ifdef __AFXDISP_H__
// Classes declared in this file

//IStream
	class CArchiveStream;

// Functions declared in this file

// AfxBSTR2ABTSR
// AfxTaskStringA2W
// AfxTaskStringW2A

#endif

/////////////////////////////////////////////////////////////////////////////

#undef AFX_DATA
#define AFX_DATA AFX_CORE_DATA

/////////////////////////////////////////////////////////////////////////////
// General OLE features

#if (!defined _AFX_NO_OLE_SUPPORT) && (defined _OBJBASE_H_)
#ifndef __AFXPRIV2_H__OLE__
#define __AFXPRIV2_H__OLE__

/////////////////////////////////////////////////////////////////////////////
// Implementation of event sink handling

struct AFX_EVENT
{
	enum
	{
		event,
		propRequest, propChanged,
		propDSCNotify
	};

	AFX_EVENT(int eventKind);

	AFX_EVENT(int eventKind, DISPID dispid, DISPPARAMS* pDispParams = NULL,
		EXCEPINFO* pExcepInfo = NULL, UINT* puArgError = NULL);

	int m_eventKind;
	DISPID m_dispid;
	DISPPARAMS* m_pDispParams;
	EXCEPINFO* m_pExcepInfo;
	UINT* m_puArgError;
	BOOL m_bPropChanged;
	HRESULT m_hResult;
	DSCSTATE m_nDSCState;
	DSCREASON m_nDSCReason;
};

AFX_INLINE AFX_EVENT::AFX_EVENT(int eventKind)
{
	m_eventKind = eventKind;
	m_dispid = DISPID_UNKNOWN;
	m_pDispParams = NULL;
	m_pExcepInfo = NULL;
	m_puArgError = NULL;
	m_hResult = NOERROR;
	m_nDSCState = dscNoState;
	m_nDSCReason = dscNoReason;
}

AFX_INLINE AFX_EVENT::AFX_EVENT(int eventKind, DISPID dispid,
	DISPPARAMS* pDispParams, EXCEPINFO* pExcepInfo, UINT* puArgError)
{
	m_eventKind = eventKind;
	m_dispid = dispid;
	m_pDispParams = pDispParams;
	m_pExcepInfo = pExcepInfo;
	m_puArgError = puArgError;
	m_hResult = NOERROR;
	m_nDSCState = dscNoState;
	m_nDSCReason = dscNoReason;
}

/////////////////////////////////////////////////////////////////////////////
// COleControlLock

class COleControlLock
{
// Constructors
public:
	COleControlLock(REFCLSID clsid);

// Attributes
	CLSID m_clsid;
	LPCLASSFACTORY m_pClassFactory;
	COleControlLock* m_pNextLock;

// Implementation
public:
	virtual ~COleControlLock();
};

#endif // __AFXPRIV2_H__OLE__
#endif //(!defined _AFX_NO_OLE_SUPPORT) && (defined _OBJBASE_H_)

/////////////////////////////////////////////////////////////////////////////
// OLE Automation features

#ifdef __AFXDISP_H__
#ifndef __AFXPRIV2_H__DISP__
#define __AFXPRIV2_H__DISP__

/////////////////////////////////////////////////////////////////////////////
// CArchiveStream

class CArchiveStream : public IStream
{
public:
	CArchiveStream(CArchive* pArchive);

// Implementation
	CArchive* m_pArchive;

	STDMETHOD_(ULONG, AddRef)();
	STDMETHOD_(ULONG, Release)();
	STDMETHOD(QueryInterface)(REFIID, LPVOID*);

	STDMETHOD(Read)(void*, ULONG, ULONG*);
	STDMETHOD(Write)(const void*, ULONG cb, ULONG*);
	STDMETHOD(Seek)(LARGE_INTEGER, DWORD, ULARGE_INTEGER*);
	STDMETHOD(SetSize)(ULARGE_INTEGER);
	STDMETHOD(CopyTo)(LPSTREAM, ULARGE_INTEGER, ULARGE_INTEGER*,
		ULARGE_INTEGER*);
	STDMETHOD(Commit)(DWORD);
	STDMETHOD(Revert)();
	STDMETHOD(LockRegion)(ULARGE_INTEGER, ULARGE_INTEGER,DWORD);
	STDMETHOD(UnlockRegion)(ULARGE_INTEGER, ULARGE_INTEGER, DWORD);
	STDMETHOD(Stat)(STATSTG*, DWORD);
	STDMETHOD(Clone)(LPSTREAM*);
};

/////////////////////////////////////////////////////////////////////////////
// Global UNICODE<>ANSI translation helpers

void AFXAPI AfxBSTR2CString(CString* pStr, BSTR bstr);

#if !defined(_UNICODE) && !defined(OLE2ANSI)
BSTR AFXAPI AfxBSTR2ABSTR(BSTR bstrW);
LPWSTR AFXAPI AfxTaskStringA2W(LPCSTR lpa);
LPSTR AFXAPI AfxTaskStringW2A(LPCWSTR lpw);
#endif

#endif // __AFXPRIV2_H__DISP__
#endif // __AFXDISP_H__

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#if (defined __AFXPRIV2_H__OLE__) && (defined __AFXPRIV2_H__DISP__)
#define __AFXPRIV2_H__
#endif

#endif // __AFXPRIV2_H__

/////////////////////////////////////////////////////////////////////////////
