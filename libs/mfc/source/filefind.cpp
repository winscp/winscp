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
// CFileFind implementation

CFileFind::CFileFind()
{
	m_pFoundInfo = NULL;
	m_pNextInfo = NULL;
	m_hContext = NULL;
	m_chDirSeparator = '\\';
}

CFileFind::~CFileFind()
{
	Close();
}

void CFileFind::Close()
{
	if (m_pFoundInfo != NULL)
	{
		delete m_pFoundInfo;
		m_pFoundInfo = NULL;
	}

	if (m_pNextInfo != NULL)
	{
		delete m_pNextInfo;
		m_pNextInfo = NULL;
	}

	if (m_hContext != NULL && m_hContext != INVALID_HANDLE_VALUE)
	{
		CloseContext();
		m_hContext = NULL;
	}
}

void CFileFind::CloseContext()
{
	::FindClose(m_hContext);
	return;
}

BOOL CFileFind::FindFile(LPCTSTR pstrName /* = NULL */,
	DWORD dwUnused /* = 0 */)
{
	UNUSED_ALWAYS(dwUnused);
	Close();
	m_pNextInfo = new WIN32_FIND_DATA;
	m_bGotLast = FALSE;

	if (pstrName == NULL)
		pstrName = _T("*.*");
	lstrcpy(((WIN32_FIND_DATA*) m_pNextInfo)->cFileName, pstrName);

	m_hContext = ::FindFirstFile(pstrName, (WIN32_FIND_DATA*) m_pNextInfo);

	if (m_hContext == INVALID_HANDLE_VALUE)
	{
		DWORD dwTemp = ::GetLastError();
		Close();
		::SetLastError(dwTemp);
		return FALSE;
	}

	LPTSTR pstrRoot = m_strRoot.GetBufferSetLength(_MAX_PATH);
	LPCTSTR pstr = _tfullpath(pstrRoot, pstrName, _MAX_PATH);

	// passed name isn't a valid path but was found by the API
	ASSERT(pstr != NULL);
	if (pstr == NULL)
	{
		m_strRoot.ReleaseBuffer(-1);
		Close();
		::SetLastError(ERROR_INVALID_NAME);
		return FALSE;
	}
	else
	{
		// find the last forward or backward whack
		LPTSTR pstrBack  = _tcsrchr(pstrRoot, '\\');
		LPTSTR pstrFront = _tcsrchr(pstrRoot, '/');

		if (pstrFront != NULL || pstrBack != NULL)
		{
			if (pstrFront == NULL)
				pstrFront = pstrRoot;
			if (pstrBack == NULL)
				pstrBack = pstrRoot;

			// from the start to the last whack is the root

			if (pstrFront >= pstrBack)
				*pstrFront = '\0';
			else
				*pstrBack = '\0';
		}
		m_strRoot.ReleaseBuffer(-1);
	}

	return TRUE;
}

BOOL CFileFind::MatchesMask(DWORD dwMask) const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
		return (!!(((LPWIN32_FIND_DATA) m_pFoundInfo)->dwFileAttributes & dwMask));
	else
		return FALSE;
}

BOOL CFileFind::GetLastAccessTime(FILETIME* pTimeStamp) const
{
	ASSERT(m_hContext != NULL);
	ASSERT(pTimeStamp != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL && pTimeStamp != NULL)
	{
		*pTimeStamp = ((LPWIN32_FIND_DATA) m_pFoundInfo)->ftLastAccessTime;
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::GetLastWriteTime(FILETIME* pTimeStamp) const
{
	ASSERT(m_hContext != NULL);
	ASSERT(pTimeStamp != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL && pTimeStamp != NULL)
	{
		*pTimeStamp = ((LPWIN32_FIND_DATA) m_pFoundInfo)->ftLastWriteTime;
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::GetCreationTime(FILETIME* pTimeStamp) const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL && pTimeStamp != NULL)
	{
		*pTimeStamp = ((LPWIN32_FIND_DATA) m_pFoundInfo)->ftCreationTime;
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::GetLastAccessTime(CTime& refTime) const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
	{
		refTime = CTime(((LPWIN32_FIND_DATA) m_pFoundInfo)->ftLastAccessTime);
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::GetLastWriteTime(CTime& refTime) const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
	{
		refTime = CTime(((LPWIN32_FIND_DATA) m_pFoundInfo)->ftLastWriteTime);
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::GetCreationTime(CTime& refTime) const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
	{
		refTime = CTime(((LPWIN32_FIND_DATA) m_pFoundInfo)->ftCreationTime);
		return TRUE;
	}
	else
		return FALSE;
}

BOOL CFileFind::IsDots() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	// return TRUE if the file name is "." or ".." and
	// the file is a directory

	BOOL bResult = FALSE;
	if (m_pFoundInfo != NULL && IsDirectory())
	{
		LPWIN32_FIND_DATA pFindData = (LPWIN32_FIND_DATA) m_pFoundInfo;
		if (pFindData->cFileName[0] == '.')
		{
			if (pFindData->cFileName[1] == '\0' ||
				(pFindData->cFileName[1] == '.' &&
				 pFindData->cFileName[2] == '\0'))
			{
				bResult = TRUE;
			}
		}
	}

	return bResult;
}

BOOL CFileFind::FindNextFile()
{
	ASSERT(m_hContext != NULL);

	if (m_hContext == NULL)
		return FALSE;
	if (m_pFoundInfo == NULL)
		m_pFoundInfo = new WIN32_FIND_DATA;

	ASSERT_VALID(this);

	void* pTemp = m_pFoundInfo;
	m_pFoundInfo = m_pNextInfo;
	m_pNextInfo = pTemp;

	return ::FindNextFile(m_hContext, (LPWIN32_FIND_DATA) m_pNextInfo);
}

CString CFileFind::GetFileURL() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	CString strResult("file://");
	strResult += GetFilePath();
	return strResult;
}

CString CFileFind::GetRoot() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	return m_strRoot;
}

CString CFileFind::GetFilePath() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	CString strResult = m_strRoot;
	if (strResult[strResult.GetLength()-1] != '\\' &&
		strResult[strResult.GetLength()-1] != '/')
		strResult += m_chDirSeparator;
	strResult += GetFileName();
	return strResult;
}

CString CFileFind::GetFileTitle() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	CString strFullName = GetFileName();
	CString strResult;

	_tsplitpath(strFullName, NULL, NULL, strResult.GetBuffer(_MAX_PATH), NULL);
	strResult.ReleaseBuffer();
	return strResult;
}

CString CFileFind::GetFileName() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	CString ret;

	if (m_pFoundInfo != NULL)
		ret = ((LPWIN32_FIND_DATA) m_pFoundInfo)->cFileName;
	return ret;
}

DWORD CFileFind::GetLength() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
		return ((LPWIN32_FIND_DATA) m_pFoundInfo)->nFileSizeLow;
	else
		return 0;
}

#if defined(_X86_) || defined(_ALPHA_)
__int64 CFileFind::GetLength64() const
{
	ASSERT(m_hContext != NULL);
	ASSERT_VALID(this);

	if (m_pFoundInfo != NULL)
		return ((LPWIN32_FIND_DATA) m_pFoundInfo)->nFileSizeLow +
				(((LPWIN32_FIND_DATA) m_pFoundInfo)->nFileSizeHigh << 32);
	else
		return 0;
}
#endif

#ifdef _DEBUG
void CFileFind::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);
	dc << "\nm_hContext = " << (UINT) m_hContext;
}

void CFileFind::AssertValid() const
{
	// if you trip the ASSERT in the else side, you've called
	// a Get() function without having done at least one
	// FindNext() call

	if (m_hContext == NULL)
		ASSERT(m_pFoundInfo == NULL && m_pNextInfo == NULL);
	else
		ASSERT(m_pFoundInfo != NULL && m_pNextInfo != NULL);

}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CFileFind, CObject)
