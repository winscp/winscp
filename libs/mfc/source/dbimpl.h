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
#define AFX_DATA AFX_DB_DATA

/////////////////////////////////////////////////////////////////////////////
// _AFX_DB_STATE

#undef AFX_DATA
#define AFX_DATA

class _AFX_DB_STATE : public CNoTrackObject
{
public:
	// MFC/DB global data
	HENV m_henvAllConnections;      // per-app HENV (CDatabase)
	int m_nAllocatedConnections;    // per-app reference to HENV above
};

EXTERN_PROCESS_LOCAL(_AFX_DB_STATE, _afxDbState)

#undef AFX_DATA
#define AFX_DATA

/////////////////////////////////////////////////////////////////////////////
