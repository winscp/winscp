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

AFX_STATIC inline BOOL IsDirSep(TCHAR ch)
{
	return (ch == '\\' || ch == '/');
}

////////////////////////////////////////////////////////////////////////////
// CFile implementation

CFile::CFile()
{
	m_hFile = (UINT) hFileNull;
	m_bCloseOnDelete = FALSE;
}

CFile::CFile(int hFile)
{
	m_hFile = hFile;
	m_bCloseOnDelete = FALSE;
}

CFile::~CFile()
{
	if (m_hFile != (UINT)hFileNull && m_bCloseOnDelete)
		Close();
}

// MFC allocates CObject (ancestor of CFile) with new, but deallocates with free,
// what codeguard dislikes, this is fix, not sure if it is necessary for
// release version, but probably causes no harm
void PASCAL CFile::operator delete(void * p)
{
  delete p;
}

BOOL CFile::Open(LPCTSTR lpszFileName, UINT nOpenFlags,
	CFileException* pException)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszFileName));
	ASSERT(pException == NULL ||
		AfxIsValidAddress(pException, sizeof(CFileException)));
	ASSERT((nOpenFlags & typeText) == 0);   // text mode not supported

	// CFile objects are always binary and CreateFile does not need flag
	nOpenFlags &= ~(UINT)typeBinary;

	m_bCloseOnDelete = FALSE;
	m_hFile = (UINT)hFileNull;
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

	// Note: typeText and typeBinary are used in derived classes only.

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
		if (pException != NULL)
		{
			pException->m_lOsError = ::GetLastError();
			pException->m_cause =
				CFileException::OsErrorToException(pException->m_lOsError);

			// use passed file name (not expanded vesion) when reporting
			// an error while opening

			pException->m_strFileName = lpszFileName;
		}
		return FALSE;
	}
	m_hFile = (HFILE)hFile;
	m_bCloseOnDelete = TRUE;

	return TRUE;
}

UINT CFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (nCount == 0)
		return 0;   // avoid Win32 "null-read"

	ASSERT(lpBuf != NULL);
	ASSERT(AfxIsValidAddress(lpBuf, nCount));

	DWORD dwRead;
	if (!::ReadFile((HANDLE)m_hFile, lpBuf, nCount, &dwRead, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError(), m_strFileName);

	return (UINT)dwRead;
}

void CFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	if (nCount == 0)
		return;     // avoid Win32 "null-write" option

	ASSERT(lpBuf != NULL);
	ASSERT(AfxIsValidAddress(lpBuf, nCount, FALSE));

	DWORD nWritten;
	if (!::WriteFile((HANDLE)m_hFile, lpBuf, nCount, &nWritten, NULL))
		CFileException::ThrowOsError((LONG)::GetLastError(), m_strFileName);

	// Win32s will not return an error all the time (usually DISK_FULL)
	if (nWritten != nCount)
		AfxThrowFileException(CFileException::diskFull, -1, m_strFileName);
}

void CFile::Close()
{
	ASSERT_VALID(this);
	ASSERT(m_hFile != (UINT)hFileNull);

	BOOL bError = FALSE;
	if (m_hFile != (UINT)hFileNull)
		bError = !::CloseHandle((HANDLE)m_hFile);

	m_hFile = (UINT) hFileNull;
	m_bCloseOnDelete = FALSE;
	m_strFileName.Empty();

	if (bError)
		CFileException::ThrowOsError((LONG)::GetLastError());
}

/////////////////////////////////////////////////////////////////////////////
// CFile implementation helpers

// turn a file, relative path or other into an absolute path
BOOL AFXAPI AfxFullPath(LPTSTR lpszPathOut, LPCTSTR lpszFileIn)
	// lpszPathOut = buffer of _MAX_PATH
	// lpszFileIn = file, relative path or absolute path
	// (both in ANSI character set)
{
	ASSERT(AfxIsValidAddress(lpszPathOut, _MAX_PATH));

	// first, fully qualify the path name
	LPTSTR lpszFilePart;
	if (!GetFullPathName(lpszFileIn, _MAX_PATH, lpszPathOut, &lpszFilePart))
	{
		lstrcpyn(lpszPathOut, lpszFileIn, _MAX_PATH); // take it literally
		return FALSE;
	}

	CString strRoot;
	// determine the root name of the volume
	AfxGetRoot(lpszPathOut, strRoot);

	// get file system information for the volume
	DWORD dwFlags, dwDummy;
	if (!GetVolumeInformation(strRoot, NULL, 0, NULL, &dwDummy, &dwFlags,
		NULL, 0))
	{
		TRACE1("Warning: could not get volume information '%s'.\n",
			(LPCTSTR)strRoot);
		return FALSE;   // preserving case may not be correct
	}

	// not all characters have complete uppercase/lowercase
	if (!(dwFlags & FS_CASE_IS_PRESERVED))
		CharUpper(lpszPathOut);

	// assume non-UNICODE file systems, use OEM character set
	if (!(dwFlags & FS_UNICODE_STORED_ON_DISK))
	{
		WIN32_FIND_DATA data;
		HANDLE h = FindFirstFile(lpszFileIn, &data);
		if (h != INVALID_HANDLE_VALUE)
		{
			FindClose(h);
			lstrcpy(lpszFilePart, data.cFileName);
		}
	}
	return TRUE;
}

void AFXAPI AfxGetRoot(LPCTSTR lpszPath, CString& strRoot)
{
	ASSERT(lpszPath != NULL);
	// determine the root name of the volume
	LPTSTR lpszRoot = strRoot.GetBuffer(_MAX_PATH);
	memset(lpszRoot, 0, _MAX_PATH);
	lstrcpyn(lpszRoot, lpszPath, _MAX_PATH);
	for (LPTSTR lpsz = lpszRoot; *lpsz != '\0'; lpsz = _tcsinc(lpsz))
	{
		// find first double slash and stop
		if (IsDirSep(lpsz[0]) && IsDirSep(lpsz[1]))
			break;
	}
	if (*lpsz != '\0')
	{
		// it is a UNC name, find second slash past '\\'
		ASSERT(IsDirSep(lpsz[0]));
		ASSERT(IsDirSep(lpsz[1]));
		lpsz += 2;
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		if (*lpsz != '\0')
			lpsz = _tcsinc(lpsz);
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		// terminate it just after the UNC root (ie. '\\server\share\')
		if (*lpsz != '\0')
			lpsz[1] = '\0';
	}
	else
	{
		// not a UNC, look for just the first slash
		lpsz = lpszRoot;
		while (*lpsz != '\0' && (!IsDirSep(*lpsz)))
			lpsz = _tcsinc(lpsz);
		// terminate it just after root (ie. 'x:\')
		if (*lpsz != '\0')
			lpsz[1] = '\0';
	}
	strRoot.ReleaseBuffer();
}

/////////////////////////////////////////////////////////////////////////////
