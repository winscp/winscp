// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Note: must include AFXDB.H first

#undef AFX_DATA
#define AFX_DATA AFX_NET_DATA

/////////////////////////////////////////////////////////////////////////////
// _AFX_SOCK_STATE

#undef AFX_DATA
#define AFX_DATA

class _AFX_SOCK_STATE : public CNoTrackObject
{
public:
	DWORD m_dwReserved1;    // reserved for version 4.1 only
	HINSTANCE m_hInstSOCK;      // handle of WSOCK32.DLL
	void (AFXAPI *m_pfnSockTerm)(void); // set once initialized
	virtual ~_AFX_SOCK_STATE();
};

EXTERN_PROCESS_LOCAL(_AFX_SOCK_STATE, _afxSockState)

#undef AFX_DATA
#define AFX_DATA

/////////////////////////////////////////////////////////////////////////////
