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
#include <errno.h>
#include <io.h>
#include <fcntl.h>

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
// CStdioFile implementation

CStdioFile::CStdioFile()
{
	m_pStream = NULL;
}

CStdioFile::CStdioFile(FILE* pOpenStream) : CFile(hFileNull)
{
	m_pStream = pOpenStream;
	m_hFile = (UINT)_get_osfhandle(_fileno(pOpenStream));
	ASSERT(!m_bCloseOnDelete);
}

CStdioFile::CStdioFile(LPCTSTR lpszFileName, UINT nOpenFlags)
{
	ASSERT(lpszFileName != NULL);
	ASSERT(AfxIsValidString(lpszFileName));

	CFileException e;
	if (!Open(lpszFileName, nOpenFlags, &e))
		AfxThrowFileException(e.m_cause, e.m_lOsError, e.m_strFileName);
}

CStdioFile::~CStdioFile()
{
	ASSERT_VALID(this);

	if (m_pStream != NULL && m_bCloseOnDelete)
		Close();
}

BOOL CStdioFile::Open(LPCTSTR lpszFileName, UINT nOpenFlags,
	CFileException* pException)
{
	ASSERT(pException == NULL || AfxIsValidAddress(pException, sizeof(CFileException)));
	ASSERT(lpszFileName != NULL);
	ASSERT(AfxIsValidString(lpszFileName));

	m_pStream = NULL;
	if (!CFile::Open(lpszFileName, (nOpenFlags & ~typeText), pException))
		return FALSE;

	ASSERT(m_hFile != hFileNull);
	ASSERT(m_bCloseOnDelete);

	char szMode[4]; // C-runtime open string
	int nMode = 0;

	// determine read/write mode depending on CFile mode
	if (nOpenFlags & modeCreate)
	{
		if (nOpenFlags & modeNoTruncate)
			szMode[nMode++] = 'a';
		else
			szMode[nMode++] = 'w';
	}
	else if (nOpenFlags & modeWrite)
		szMode[nMode++] = 'a';
	else
		szMode[nMode++] = 'r';

	// add '+' if necessary (when read/write modes mismatched)
	if (szMode[0] == 'r' && (nOpenFlags & modeReadWrite) ||
		szMode[0] != 'r' && !(nOpenFlags & modeWrite))
	{
		// current szMode mismatched, need to add '+' to fix
		szMode[nMode++] = '+';
	}

	// will be inverted if not necessary
	int nFlags = _O_RDONLY|_O_TEXT;
	if (nOpenFlags & (modeWrite|modeReadWrite))
		nFlags ^= _O_RDONLY;

	if (nOpenFlags & typeBinary)
		szMode[nMode++] = 'b', nFlags ^= _O_TEXT;
	else
		szMode[nMode++] = 't';
	szMode[nMode++] = '\0';

	// open a C-runtime low-level file handle
	int nHandle = _open_osfhandle(m_hFile, nFlags);

	// open a C-runtime stream from that handle
	if (nHandle != -1)
		m_pStream = _fdopen(nHandle, szMode);

	if (m_pStream == NULL)
	{
		// an error somewhere along the way...
		if (pException != NULL)
		{
			pException->m_lOsError = _doserrno;
			pException->m_cause = CFileException::OsErrorToException(_doserrno);
		}

		CFile::Abort(); // close m_hFile
		return FALSE;
	}

	return TRUE;
}

UINT CStdioFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	if (nCount == 0)
		return 0;   // avoid Win32 "null-read"

	ASSERT(AfxIsValidAddress(lpBuf, nCount));

	UINT nRead = 0;

	if ((nRead = fread(lpBuf, sizeof(BYTE), nCount, m_pStream)) == 0 && !feof(m_pStream))
		AfxThrowFileException(CFileException::generic, _doserrno, m_strFileName);
	if (ferror(m_pStream))
	{
		clearerr(m_pStream);
		AfxThrowFileException(CFileException::generic, _doserrno, m_strFileName);
	}
	return nRead;
}

void CStdioFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);
	ASSERT(AfxIsValidAddress(lpBuf, nCount, FALSE));

	if (fwrite(lpBuf, sizeof(BYTE), nCount, m_pStream) != nCount)
		AfxThrowFileException(CFileException::generic, _doserrno, m_strFileName);
}

void CStdioFile::WriteString(LPCTSTR lpsz)
{
	ASSERT(lpsz != NULL);
	ASSERT(m_pStream != NULL);

	if (_fputts(lpsz, m_pStream) == _TEOF)
		AfxThrowFileException(CFileException::diskFull, _doserrno, m_strFileName);
}

LPTSTR CStdioFile::ReadString(LPTSTR lpsz, UINT nMax)
{
	ASSERT(lpsz != NULL);
	ASSERT(AfxIsValidAddress(lpsz, nMax));
	ASSERT(m_pStream != NULL);

	LPTSTR lpszResult = _fgetts(lpsz, nMax, m_pStream);
	if (lpszResult == NULL && !feof(m_pStream))
	{
		clearerr(m_pStream);
		AfxThrowFileException(CFileException::generic, _doserrno, m_strFileName);
	}
	return lpszResult;
}

BOOL CStdioFile::ReadString(CString& rString)
{
	ASSERT_VALID(this);

	rString = &afxChNil;    // empty string without deallocating
	const int nMaxSize = 128;
	LPTSTR lpsz = rString.GetBuffer(nMaxSize);
	LPTSTR lpszResult;
	int nLen = 0;
	for (;;)
	{
		lpszResult = _fgetts(lpsz, nMaxSize+1, m_pStream);
		rString.ReleaseBuffer();

		// handle error/eof case
		if (lpszResult == NULL && !feof(m_pStream))
		{
			clearerr(m_pStream);
			AfxThrowFileException(CFileException::generic, _doserrno,
				m_strFileName);
		}

		// if string is read completely or EOF
		if (lpszResult == NULL ||
			(nLen = lstrlen(lpsz)) < nMaxSize ||
			lpsz[nLen-1] == '\n')
			break;

		nLen = rString.GetLength();
		lpsz = rString.GetBuffer(nMaxSize + nLen) + nLen;
	}

	// remove '\n' from end of string if present
	lpsz = rString.GetBuffer(0);
	nLen = rString.GetLength();
	if (nLen != 0 && lpsz[nLen-1] == '\n')
		rString.GetBufferSetLength(nLen-1);

	return lpszResult != NULL;
}

LONG CStdioFile::Seek(LONG lOff, UINT nFrom)
{
	ASSERT_VALID(this);
	ASSERT(nFrom == begin || nFrom == end || nFrom == current);
	ASSERT(m_pStream != NULL);

	if (fseek(m_pStream, lOff, nFrom) != 0)
		AfxThrowFileException(CFileException::badSeek, _doserrno,
			m_strFileName);

	long pos = ftell(m_pStream);
	return pos;
}

DWORD CStdioFile::GetPosition() const
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	long pos = ftell(m_pStream);
	if (pos == -1)
		AfxThrowFileException(CFileException::invalidFile, _doserrno,
			m_strFileName);
	return pos;
}

void CStdioFile::Flush()
{
	ASSERT_VALID(this);

	if (m_pStream != NULL && fflush(m_pStream) != 0)
		AfxThrowFileException(CFileException::diskFull, _doserrno,
			m_strFileName);
}

void CStdioFile::Close()
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	int nErr = 0;

	if (m_pStream != NULL)
		nErr = fclose(m_pStream);

	m_hFile = (UINT) hFileNull;
	m_bCloseOnDelete = FALSE;
	m_pStream = NULL;

	if (nErr != 0)
		AfxThrowFileException(CFileException::diskFull, _doserrno,
			m_strFileName);
}

void CStdioFile::Abort()
{
	ASSERT_VALID(this);

	if (m_pStream != NULL && m_bCloseOnDelete)
		fclose(m_pStream);  // close but ignore errors
	m_hFile = (UINT) hFileNull;
	m_pStream = NULL;
	m_bCloseOnDelete = FALSE;
}

CFile* CStdioFile::Duplicate() const
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	AfxThrowNotSupportedException();
	return NULL;
}

void CStdioFile::LockRange(DWORD /* dwPos */, DWORD /* dwCount */)
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	AfxThrowNotSupportedException();
}

void CStdioFile::UnlockRange(DWORD /* dwPos */, DWORD /* dwCount */)
{
	ASSERT_VALID(this);
	ASSERT(m_pStream != NULL);

	AfxThrowNotSupportedException();
}

#ifdef _DEBUG
void CStdioFile::Dump(CDumpContext& dc) const
{
	CFile::Dump(dc);

	dc << "m_pStream = " << (void*)m_pStream;
	dc << "\n";
}
#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CStdioFile, CFile)

/////////////////////////////////////////////////////////////////////////////
