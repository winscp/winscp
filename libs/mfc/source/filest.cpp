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

////////////////////////////////////////////////////////////////////////////

BOOL CFile::IsValid(LPCTSTR lpszFileName)
{
	// attempt to fully qualify path first
	TCHAR m_szFullName[_MAX_PATH];
	if (!AfxFullPath(m_szFullName, lpszFileName))
	{
		return FALSE;
	}

	WIN32_FIND_DATA findFileData;
	HANDLE hFind = FindFirstFile((LPTSTR)lpszFileName, &findFileData);
	if (hFind == INVALID_HANDLE_VALUE)
		return FALSE;

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
