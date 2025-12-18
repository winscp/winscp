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
#include <SysUtils.hpp>

////////////////////////////////////////////////////////////////////////////
// CFile implementation

CFile::CFile()
{
	m_hFile = (HANDLE) hFileNull;
}

CFile::~CFile()
{
	if (m_hFile != (HANDLE)hFileNull)
		Close();
}

BOOL CFile::Open(LPCTSTR lpszFileName, UINT nOpenFlags)
{
	m_hFile = (HANDLE)hFileNull;
	m_strFileName.Empty();

	TCHAR szTemp[_MAX_PATH];
	AfxFullPath(szTemp, lpszFileName);
	m_strFileName = szTemp;

	ASSERT(sizeof(HANDLE) == sizeof(UINT));
	ASSERT(shareCompat == 0);

	// map read/write mode
	ASSERT((modeRead|modeWrite|modeReadWrite) == 3);
	DWORD dwAccess = 0;
	switch (nOpenFlags & 3)
	{
	case modeRead:
		dwAccess = GENERIC_READ;
		break;
	case modeWrite:
		dwAccess = GENERIC_WRITE;
		break;
	case modeReadWrite:
		dwAccess = GENERIC_READ|GENERIC_WRITE;
		break;
	default:
		ASSERT(FALSE);  // invalid share mode
	}

	// map share mode
	DWORD dwShareMode = 0;
	switch (nOpenFlags & 0x70)    // map compatibility mode to exclusive
	{
	default:
		ASSERT(FALSE);  // invalid share mode?
	case shareCompat:
	case shareExclusive:
		dwShareMode = 0;
		break;
	case shareDenyWrite:
		dwShareMode = FILE_SHARE_READ;
		break;
	case shareDenyRead:
		dwShareMode = FILE_SHARE_WRITE;
		break;
	case shareDenyNone:
		dwShareMode = FILE_SHARE_WRITE|FILE_SHARE_READ;
		break;
	}

	// map modeNoInherit flag
	SECURITY_ATTRIBUTES sa;
	sa.nLength = sizeof(sa);
	sa.lpSecurityDescriptor = NULL;
	sa.bInheritHandle = (nOpenFlags & modeNoInherit) == 0;

	// map creation flags
	DWORD dwCreateFlag;
	if (nOpenFlags & modeCreate)
	{
		if (nOpenFlags & modeNoTruncate)
			dwCreateFlag = OPEN_ALWAYS;
		else
			dwCreateFlag = CREATE_ALWAYS;
	}
	else
		dwCreateFlag = OPEN_EXISTING;

	// attempt file creation
	HANDLE hFile = ::CreateFile(lpszFileName, dwAccess, dwShareMode, &sa,
		dwCreateFlag, FILE_ATTRIBUTE_NORMAL, NULL);
	if (hFile == INVALID_HANDLE_VALUE)
	{
		return FALSE;
	}
	m_hFile = hFile;

	return TRUE;
}

UINT CFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT(m_hFile != (HANDLE)hFileNull);

	if (nCount == 0)
		return 0;   // avoid Win32 "null-read"

	ASSERT(lpBuf != NULL);

	DWORD dwRead;
	if (!::ReadFile(m_hFile, lpBuf, nCount, &dwRead, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError(), m_strFileName);

	return (UINT)dwRead;
}

void CFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT(m_hFile != (HANDLE)hFileNull);

	if (nCount == 0)
		return;     // avoid Win32 "null-write" option

	ASSERT(lpBuf != NULL);

	DWORD nWritten;
	if (!::WriteFile(m_hFile, lpBuf, nCount, &nWritten, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError(), m_strFileName);

	// Win32s will not return an error all the time (usually DISK_FULL)
	if (nWritten != nCount)
		AfxThrowFileException(CFileException::diskFull, -1, m_strFileName);
}

void CFile::Close()
{
	ASSERT(m_hFile != (HANDLE)hFileNull);

	BOOL bError = FALSE;
	if (m_hFile != (HANDLE)hFileNull)
		bError = !::CloseHandle(m_hFile);

	m_hFile = (HANDLE) hFileNull;
	m_strFileName.Empty();

	if (bError)
		CFileException::ThrowOsError((LONG)::GetLastError());
}

/////////////////////////////////////////////////////////////////////////////
// CFile implementation helpers

// turn a file, relative path or other into an absolute path
// Used for error reporting only, so not critical.
BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn)
	// lpszPathOut = buffer of _MAX_PATH
	// lpszFileIn = file, relative path or absolute path
	// (both in ANSI character set)
{
        UnicodeString Path;
        BOOL Result;
        try
        {
	        Path = ExpandFileName(lpszFileIn);
	        Result = TRUE;
	}
	catch (...)
	{
		Path = lpszFileIn; // take it literally
	        Result = FALSE;
	}
	lstrcpyn(lpszPathOut, Path.c_str(), _MAX_PATH);
	return Result;
}
/////////////////////////////////////////////////////////////////////////////
