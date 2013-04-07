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

#ifdef _MBCS    // entire file is only for DBCS enabling

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

AFX_STATIC BOOL PASCAL _AfxInitDBCS()
{
	CPINFO info;
	GetCPInfo(GetOEMCP(), &info);
	return info.MaxCharSize > 1;
}

const AFX_DATADEF BOOL _afxDBCS = _AfxInitDBCS();

#endif //_MBCS

/////////////////////////////////////////////////////////////////////////////
