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

#ifdef AFX_OLE_SEG
#pragma code_seg(AFX_OLE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleStreamFile implementation

COleStreamFile::COleStreamFile(LPSTREAM lpStream)
{
	m_lpStream = lpStream;

	m_strStorageName.Empty();
	if (m_lpStream != NULL)
	{
		USES_CONVERSION;
		STATSTG statstg;
		if (m_lpStream->Stat(&statstg, 0) == S_OK)
		{
			if (statstg.pwcsName != NULL)
			{
				TCHAR szTemp[_MAX_PATH];

				AfxFullPath(szTemp, OLE2CT(statstg.pwcsName));
				CoTaskMemFree(statstg.pwcsName);

				m_strStorageName = szTemp;
			}
		}
	}
}

COleStreamFile::~COleStreamFile()
{
	if (m_lpStream != NULL && m_bCloseOnDelete)
	{
		Close();
		ASSERT(m_lpStream == NULL);
	}
}

LPSTREAM COleStreamFile::Detach()
{
	LPSTREAM lpStream = m_lpStream;
	m_lpStream = NULL;  // detach and transfer ownership of m_lpStream
	return lpStream;
}

void COleStreamFile::Attach(LPSTREAM lpStream)
{
	ASSERT(m_lpStream == NULL); // already attached to an LPSTREAM?
	ASSERT(lpStream != NULL);

	m_lpStream = lpStream;
	m_bCloseOnDelete = FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// OLE streams helper

HRESULT AFXAPI _AfxReadFromStream(LPSTREAM pStream, void* lpBuf, UINT nCount, DWORD& nRead)
{
	if (nCount == 0)
	{
		nRead = 0;
		return S_OK;
	}

	ASSERT(AfxIsValidAddress(lpBuf, nCount));
	ASSERT(pStream != NULL);

	// read from the stream
	SCODE sc = pStream->Read(lpBuf, nCount, &nRead);
	return ResultFromScode(sc);
}

/////////////////////////////////////////////////////////////////////////////
// OLE CFileException helpers

void AFXAPI _AfxFillOleFileException(CFileException* pError, SCODE sc)
{
	ASSERT(pError != NULL);
	ASSERT(FAILED(sc));

	int cause;  // portable CFileException.m_cause

	// error codes 255 or less are DOS/Win32 error codes
	if (SCODE_SEVERITY(sc) == SEVERITY_ERROR &&
		SCODE_FACILITY(sc) == FACILITY_STORAGE &&
		SCODE_CODE(sc) < 0x100)
	{
		ASSERT(SCODE_CODE(sc) != 0);

		// throw an exception matching to the DOS error
		//  (NOTE: only the DOS error part of the SCODE becomes m_lOsError)
		cause = CFileException::OsErrorToException(SCODE_CODE(sc));
		sc = (SCODE)SCODE_CODE(sc);
	}
	else
	{
		// attempt some conversion of storage specific error codes to generic
		//  CFileException causes...
		switch (sc)
		{
		case STG_E_INUSE:
		case STG_E_SHAREREQUIRED:
			cause = CFileException::sharingViolation;
			break;

		case STG_E_NOTCURRENT:
		case STG_E_REVERTED:
		case STG_E_CANTSAVE:
		case STG_E_OLDFORMAT:
		case STG_E_OLDDLL:
			cause = CFileException::generic;
			break;

		default:
			cause = CFileException::generic;
			break;
		}
	}

	// fill in pError
	pError->m_cause = cause;
	pError->m_lOsError = (LONG)sc;
}

void AFXAPI _AfxThrowOleFileException(SCODE sc)
{
	// ignore non-failure codes
	if (!FAILED(sc))
		return;

	// otherwise, construct and exception and throw it
	CFileException e;
	_AfxFillOleFileException(&e, sc);
	AfxThrowFileException(e.m_cause, e.m_lOsError);
}

/////////////////////////////////////////////////////////////////////////////
// COleStreamFile Attributes

BOOL COleStreamFile::GetStatus(CFileStatus& rStatus) const
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	// get status of the stream
	STATSTG statstg;
	if (m_lpStream->Stat(&statstg, 0) != S_OK)
		return FALSE;

	// map to CFileStatus struct
	rStatus.m_mtime = CTime(statstg.mtime);
	rStatus.m_ctime = CTime(statstg.ctime);
	rStatus.m_atime = CTime(statstg.atime);
	ASSERT(statstg.cbSize.HighPart == 0);
	rStatus.m_size = statstg.cbSize.LowPart;
	rStatus.m_attribute = 0;
	rStatus.m_szFullName[0] = '\0';
	if (statstg.pwcsName != NULL)
	{
		// name was returned -- copy and free it
		lstrcpyn(rStatus.m_szFullName, OLE2CT(statstg.pwcsName),
			_countof(rStatus.m_szFullName));
		CoTaskMemFree(statstg.pwcsName);
	}
	return TRUE;
}

const CString COleStreamFile::GetStorageName() const
{
	ASSERT_VALID(this);
	return m_strStorageName;
}


DWORD COleStreamFile::GetPosition() const
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	ULARGE_INTEGER liPosition;
	LARGE_INTEGER liZero; LISet32(liZero, 0);
	SCODE sc = m_lpStream->Seek(liZero, STREAM_SEEK_CUR, &liPosition);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);

	ASSERT(liPosition.HighPart == 0);
	return liPosition.LowPart;
}


/////////////////////////////////////////////////////////////////////////////
// COleStreamFile Operations

BOOL COleStreamFile::OpenStream(LPSTORAGE lpStorage, LPCTSTR lpszStreamName,
	DWORD nOpenFlags, CFileException* pError)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpStream == NULL);
	ASSERT(lpStorage != NULL);
	ASSERT(AfxIsValidString(lpszStreamName));
	ASSERT(pError == NULL ||
		AfxIsValidAddress(pError, sizeof(CFileException)));

	SCODE sc = lpStorage->OpenStream(T2COLE(lpszStreamName), NULL, nOpenFlags, 0,
		&m_lpStream);
	if (FAILED(sc) && pError != NULL)
		_AfxFillOleFileException(pError, sc);

	ASSERT(FAILED(sc) || m_lpStream != NULL);
	return !FAILED(sc);
}

BOOL COleStreamFile::CreateStream(LPSTORAGE lpStorage, LPCTSTR lpszStreamName,
	DWORD nOpenFlags, CFileException* pError)
{
	USES_CONVERSION;

	ASSERT_VALID(this);
	ASSERT(m_lpStream == NULL);
	ASSERT(lpStorage != NULL);
	ASSERT(AfxIsValidString(lpszStreamName));
	ASSERT(pError == NULL ||
		AfxIsValidAddress(pError, sizeof(CFileException)));

	STATSTG statstg;
	if (lpStorage->Stat(&statstg, 0) == S_OK)
	{
		if (statstg.pwcsName != NULL)
		{
			TCHAR szTemp[_MAX_PATH];

			AfxFullPath(szTemp, OLE2CT(statstg.pwcsName));
			CoTaskMemFree(statstg.pwcsName);

			m_strStorageName = szTemp;
		}
	}

	SCODE sc = lpStorage->CreateStream(T2COLE(lpszStreamName), nOpenFlags,
		0, 0, &m_lpStream);

	if (FAILED(sc) && pError != NULL)
		_AfxFillOleFileException(pError, sc);

	ASSERT(FAILED(sc) || m_lpStream != NULL);
	return !FAILED(sc);
}

BOOL COleStreamFile::CreateMemoryStream(CFileException* pError)
{
	ASSERT_VALID(this);
	ASSERT(pError == NULL ||
		AfxIsValidAddress(pError, sizeof(CFileException)));

	SCODE sc = CreateStreamOnHGlobal(NULL, TRUE, &m_lpStream);
	if (FAILED(sc) && pError != NULL)
		_AfxFillOleFileException(pError, sc);

	ASSERT(FAILED(sc) || m_lpStream != NULL);
	return !FAILED(sc);
}

/////////////////////////////////////////////////////////////////////////////
// COleStreamFile Overrides

CFile* COleStreamFile::Duplicate() const
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	LPSTREAM lpStream;
	SCODE sc = m_lpStream->Clone(&lpStream);
	if (FAILED(sc))
		_AfxThrowOleFileException(sc);

	ASSERT(lpStream != NULL);
	COleStreamFile* pFile = NULL;

	TRY
	{
		// attempt to create the stream
		pFile = new COleStreamFile(lpStream);
		pFile->m_bCloseOnDelete = m_bCloseOnDelete;
	}
	CATCH_ALL(e)
	{
		// cleanup cloned stream
		lpStream->Release();
		THROW_LAST();
	}
	END_CATCH_ALL

	ASSERT(pFile != NULL);
	return pFile;
}

LONG COleStreamFile::Seek(LONG lOff, UINT nFrom)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	ASSERT(STREAM_SEEK_SET == begin);
	ASSERT(STREAM_SEEK_CUR == current);
	ASSERT(STREAM_SEEK_END == end);

	ULARGE_INTEGER liNewPosition;
	LARGE_INTEGER liOff;
	LISet32(liOff, lOff);
	SCODE sc = m_lpStream->Seek(liOff, nFrom, &liNewPosition);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);

	ASSERT(liNewPosition.HighPart == 0);
	return liNewPosition.LowPart;
}

void COleStreamFile::SetLength(DWORD dwNewLen)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	ULARGE_INTEGER liNewLen;
	ULISet32(liNewLen, dwNewLen);
	SCODE sc = m_lpStream->SetSize(liNewLen);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);
}

DWORD COleStreamFile::GetLength() const
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	// get status of the stream
	STATSTG statstg;
	SCODE sc = m_lpStream->Stat(&statstg, STATFLAG_NONAME);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);

	// map to CFileStatus struct
	ASSERT(statstg.cbSize.HighPart == 0);
	return statstg.cbSize.LowPart;
}

UINT COleStreamFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	DWORD dwBytesRead;
	HRESULT hr = _AfxReadFromStream(m_lpStream, lpBuf, nCount, dwBytesRead);

	if (hr != S_OK)
		_AfxThrowOleFileException(hr);

	// always return number of bytes read
	return (UINT)dwBytesRead;
}

void COleStreamFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	if (nCount == 0)
		return;

	ASSERT(AfxIsValidAddress(lpBuf, nCount, FALSE));

	// write to the stream
	DWORD dwBytesWritten;
	SCODE sc = m_lpStream->Write(lpBuf, nCount, &dwBytesWritten);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);

	// if no error, all bytes should have been written
	ASSERT((UINT)dwBytesWritten == nCount);
}

void COleStreamFile::LockRange(DWORD dwPos, DWORD dwCount)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	// convert parameters to long integers
	ULARGE_INTEGER liPos;
	ULISet32(liPos, dwPos);
	ULARGE_INTEGER liCount;
	ULISet32(liCount, dwCount);

	// then lock the region
	SCODE sc = m_lpStream->LockRegion(liPos, liCount, LOCK_EXCLUSIVE);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);
}

void COleStreamFile::UnlockRange(DWORD dwPos, DWORD dwCount)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	// convert parameters to long integers
	ULARGE_INTEGER liPos;
	ULISet32(liPos, dwPos);
	ULARGE_INTEGER liCount;
	ULISet32(liCount, dwCount);

	// then lock the region
	SCODE sc = m_lpStream->UnlockRegion(liPos, liCount, LOCK_EXCLUSIVE);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);
}

void COleStreamFile::Abort()
{
	ASSERT_VALID(this);

	if (m_lpStream != NULL)
	{
		m_lpStream->Revert();
		RELEASE(m_lpStream);
	}

	m_strStorageName.Empty();
}

void COleStreamFile::Flush()
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	// commit will return an error only if the stream is transacted
	SCODE sc = m_lpStream->Commit(0);
	if (sc != S_OK)
		_AfxThrowOleFileException(sc);
}

void COleStreamFile::Close()
{
	ASSERT_VALID(this);

	if (m_lpStream != NULL)
	{
		// commit the stream via Flush (which can be overriden)
		Flush();
		RELEASE(m_lpStream);
	}

	m_strStorageName.Empty();
}

IStream* COleStreamFile::GetStream() const
{
	return m_lpStream;
}

/////////////////////////////////////////////////////////////////////////////
// COleStreamFile diagnostics

#ifdef _DEBUG
void COleStreamFile::AssertValid() const
{
	CFile::AssertValid();
}

void COleStreamFile::Dump(CDumpContext& dc) const
{
	CFile::Dump(dc);

	dc << "m_lpStream = " << m_lpStream;
	dc << "m_strStorageName = \"" << m_strStorageName;
	dc << "\"\n";
}
#endif

////////////////////////////////////////////////////////////////////////////

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleStreamFile, CFile)

////////////////////////////////////////////////////////////////////////////
