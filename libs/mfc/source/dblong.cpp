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

#ifdef AFX_DB_SEG
#pragma code_seg(AFX_DB_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CLongBinary class for holding LONG VARBINARY data

CLongBinary::CLongBinary()
{
	m_hData = NULL;
}

CLongBinary::~CLongBinary()
{
	if (m_hData != NULL)
	{
		::GlobalFree(m_hData);
		m_hData = NULL;
	}
}

//////////////////////////////////////////////////////////////////////////////
// CLongBinary diagnostics

#ifdef _DEBUG
void CLongBinary::AssertValid() const
{
	CObject::AssertValid();
}

void CLongBinary::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_hData = " << m_hData;
	dc << "\nm_dwDataLength = " << m_dwDataLength;
	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////////


#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CLongBinary, CObject)

/////////////////////////////////////////////////////////////////////////////
