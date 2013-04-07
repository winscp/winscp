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
#include <float.h>

#ifdef AFX_DBG1_SEG
#pragma code_seg(AFX_DBG1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Diagnostic Stream output for floating point numbers

#ifdef _DEBUG
CDumpContext& CDumpContext::operator<<(float f)
{
	char szBuffer[32];
	_gcvt(f, FLT_DIG, szBuffer);
	*this << szBuffer;
	return *this;
}

CDumpContext& CDumpContext::operator<<(double d)
{
	char szBuffer[32];
	_gcvt(d, DBL_DIG, szBuffer);
	*this << szBuffer;
	return *this;
}
#endif

/////////////////////////////////////////////////////////////////////////////
