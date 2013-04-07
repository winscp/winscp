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

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

// Note: in separate module so if can be replaced if needed

void AFXAPI AfxAbort()
{
	TRACE0("AfxAbort called.\n");

	AfxWinTerm();
	abort();
}

/////////////////////////////////////////////////////////////////////////////
