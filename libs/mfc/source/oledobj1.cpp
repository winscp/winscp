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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleDataObject constructors

COleDataObject::COleDataObject()
{
	m_lpEnumerator = NULL;
	m_lpDataObject = NULL;
	m_bAutoRelease = TRUE;
	m_bClipboard = FALSE;
}

void COleDataObject::Attach(LPDATAOBJECT lpDataObject, BOOL bAutoRelease)
{
	ASSERT(lpDataObject != NULL);

	Release();  // detach previous
	m_lpDataObject = lpDataObject;
	m_bAutoRelease = bAutoRelease;
}

void COleDataObject::Release()
{
	RELEASE(m_lpEnumerator);

	if (m_lpDataObject != NULL)
	{
		if (m_bAutoRelease)
			m_lpDataObject->Release();
		m_lpDataObject = NULL;
	}
	m_bClipboard = FALSE;
}

LPDATAOBJECT COleDataObject::Detach()
{
	EnsureClipboardObject();

	LPDATAOBJECT lpDataObject = m_lpDataObject;
	m_lpDataObject = NULL;  // detach without Release
	m_bClipboard = FALSE;

	return lpDataObject;
}

LPDATAOBJECT COleDataObject::GetIDataObject(BOOL bAddRef)
{
	EnsureClipboardObject();

	LPDATAOBJECT lpDataObject = m_lpDataObject;
	if (bAddRef && lpDataObject != NULL)
		lpDataObject->AddRef();

	return lpDataObject;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataObject attributes

void COleDataObject::BeginEnumFormats()
{
	EnsureClipboardObject();
	ASSERT(m_bClipboard || m_lpDataObject != NULL);

	// release old enumerator
	RELEASE(m_lpEnumerator);
	if (m_lpDataObject == NULL)
		return;

	// get the new enumerator
	SCODE sc = m_lpDataObject->EnumFormatEtc(DATADIR_GET, &m_lpEnumerator);
	ASSERT(sc != S_OK || m_lpEnumerator != NULL);
}

BOOL COleDataObject::GetNextFormat(LPFORMATETC lpFormatEtc)
{
	ASSERT(m_bClipboard || m_lpDataObject != NULL);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// return FALSE if enumerator is already NULL
	if (m_lpEnumerator == NULL)
		return FALSE;

	// attempt to retrieve the next format with the enumerator
	SCODE sc = m_lpEnumerator->Next(1, lpFormatEtc, NULL);

	// if enumerator fails, stop the enumeration
	if (sc != S_OK)
	{
		RELEASE(m_lpEnumerator);
		return FALSE;   // enumeration has ended
	}
	// otherwise, continue
	return TRUE;
}

CFile* COleDataObject::GetFileData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	EnsureClipboardObject();
	ASSERT(m_bClipboard || m_lpDataObject != NULL);
	if (m_lpDataObject == NULL)
		return NULL;

	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);
	formatEtc.tymed = TYMED_FILE|TYMED_MFPICT|TYMED_HGLOBAL|TYMED_ISTREAM;

	// attempt to get the data
	STGMEDIUM stgMedium;
	SCODE sc = m_lpDataObject->GetData(lpFormatEtc, &stgMedium);
	if (FAILED(sc))
		return FALSE;

	// STGMEDIUMs with pUnkForRelease need to be copied first
	if (stgMedium.pUnkForRelease != NULL)
	{
		STGMEDIUM stgMediumDest;
		stgMediumDest.tymed = TYMED_NULL;
		stgMediumDest.pUnkForRelease = NULL;
		if (!_AfxCopyStgMedium(lpFormatEtc->cfFormat, &stgMediumDest, &stgMedium))
		{
			::ReleaseStgMedium(&stgMedium);
			return FALSE;
		}
		// release original and replace with new
		::ReleaseStgMedium(&stgMedium);
		stgMedium = stgMediumDest;
	}

	// convert it to a file, depending on data
	CString strFileName;
	CFile* pFile = NULL;
	TRY
	{
		switch (stgMedium.tymed)
		{
		case TYMED_FILE:
			strFileName = stgMedium.lpszFileName;
			pFile = new CFile;
			if (!pFile->Open(strFileName,
				CFile::modeReadWrite|CFile::shareExclusive))
			{
				delete pFile;
				pFile = NULL;
				break;
			}
			// caller is responsible for deleting the actual file,
			//  but we free the file name.
			CoTaskMemFree(stgMedium.lpszFileName);
			break;

		case TYMED_MFPICT:
		case TYMED_HGLOBAL:
			pFile = new CSharedFile;
			((CSharedFile*)pFile)->SetHandle(stgMedium.hGlobal);
			break;

		case TYMED_ISTREAM:
			pFile = new COleStreamFile(stgMedium.pstm);
			break;

		default:
			// type not supported, so return error
			::ReleaseStgMedium(&stgMedium);
			break;
		}
	}
	CATCH_ALL(e)
	{
		delete pFile;
		pFile = NULL;
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	// store newly created CFile* and return
	return pFile;
}

HGLOBAL COleDataObject::GetGlobalData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	EnsureClipboardObject();
	ASSERT(m_bClipboard || m_lpDataObject != NULL);
	if (m_lpDataObject == NULL)
		return NULL;

	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	BOOL bFillFormatEtc = (lpFormatEtc == NULL);
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);
	if (bFillFormatEtc)
		lpFormatEtc->tymed = TYMED_HGLOBAL|TYMED_MFPICT;
	ASSERT((lpFormatEtc->tymed & (TYMED_HGLOBAL|TYMED_MFPICT)) != 0);

	// attempt to get the data
	STGMEDIUM stgMedium;
	SCODE sc = m_lpDataObject->GetData(lpFormatEtc, &stgMedium);
	if (FAILED(sc))
		return FALSE;

	// handle just hGlobal types
	switch (stgMedium.tymed)
	{
	case TYMED_MFPICT:
	case TYMED_HGLOBAL:
		if (stgMedium.pUnkForRelease == NULL)
			return stgMedium.hGlobal;

		STGMEDIUM stgMediumDest;
		stgMediumDest.tymed = TYMED_NULL;
		stgMediumDest.pUnkForRelease = NULL;
		if (!_AfxCopyStgMedium(lpFormatEtc->cfFormat, &stgMediumDest, &stgMedium))
		{
			::ReleaseStgMedium(&stgMedium);
			return NULL;
		}
		::ReleaseStgMedium(&stgMedium);
		return stgMediumDest.hGlobal;

	// default -- falls through to error condition...
	}

	::ReleaseStgMedium(&stgMedium);
	return NULL;
}

BOOL COleDataObject::GetData(CLIPFORMAT cfFormat, LPSTGMEDIUM lpStgMedium,
	LPFORMATETC lpFormatEtc)
{
	EnsureClipboardObject();
	ASSERT(m_bClipboard || m_lpDataObject != NULL);
	if (m_lpDataObject == NULL)
		return FALSE;

	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// attempt to get the data
	SCODE sc = m_lpDataObject->GetData(lpFormatEtc, lpStgMedium);
	if (FAILED(sc))
		return FALSE;

	return TRUE;
}

BOOL COleDataObject::IsDataAvailable(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	if (m_bClipboard)
	{
		// it is faster and more reliable to ask the real Win32 clipboard
		//  instead of the OLE clipboard.
		return ::IsClipboardFormatAvailable(cfFormat);
	}
	else
	{
		ASSERT(m_lpDataObject != NULL);
		ASSERT(lpFormatEtc == NULL ||
			AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

		// fill in FORMATETC struct
		FORMATETC formatEtc;
		lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

		// attempt to get the data
		return m_lpDataObject->QueryGetData(lpFormatEtc) == S_OK;
	}
}

/////////////////////////////////////////////////////////////////////////////
// clipboard API wrappers

BOOL COleDataObject::AttachClipboard()
{
	ASSERT(AfxIsValidAddress(this, sizeof(COleDataObject)));
	ASSERT(m_lpDataObject == NULL); // need to call release?
	ASSERT(!m_bClipboard); // already attached to clipboard?

	// set special "clipboard" flag for optimizations
	m_bClipboard = TRUE;
	return TRUE;
}

void COleDataObject::EnsureClipboardObject()
{
	ASSERT(AfxIsValidAddress(this, sizeof(COleDataObject)));

	if (m_bClipboard && m_lpDataObject == NULL)
	{
		// get clipboard using OLE API
		LPDATAOBJECT lpDataObject;
		SCODE sc = ::OleGetClipboard(&lpDataObject);

		// attach COleDataObject wrapper to IDataObject from clipboard
		if (sc == S_OK)
			Attach(lpDataObject, TRUE);
	}
}

/////////////////////////////////////////////////////////////////////////////
