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

/////////////////////////////////////////////////////////////////////////////
// CException

CException::CException()
{
}

BOOL CException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
	PUINT pnHelpContext /* = NULL */ )
{
	if (pnHelpContext != NULL)
		*pnHelpContext = 0;

	if (nMaxError != 0 && lpszError != NULL)
		*lpszError = '\0';

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
