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

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifdef _DEBUG
// character strings to use for dumping CArchiveException
static const LPCSTR rgszCArchiveExceptionCause[] =
{
	"none",
	"generic",
	"readOnly",
	"endOfFile",
	"writeOnly",
	"badIndex",
	"badClass",
	"badSchema",
};
static const char szUnknown[] = "unknown";
#endif

BOOL CArchiveException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
	PUINT pnHelpContext)
{
	ASSERT(lpszError != NULL && AfxIsValidString(lpszError, nMaxError));

	if (pnHelpContext != NULL)
		*pnHelpContext = m_cause + AFX_IDP_ARCH_NONE;

	// we can use CString here; archive errors aren't caused
	// by being out of memory.

	CString strMessage;
	CString strFileName = m_strFileName;
	if (strFileName.IsEmpty())
		strFileName.LoadString(AFX_IDS_UNNAMED_FILE);
	AfxFormatString1(strMessage,
		m_cause + AFX_IDP_ARCH_NONE, strFileName);
	lstrcpyn(lpszError, strMessage, nMaxError);

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CArchiveException

#ifdef _DEBUG
void CArchiveException::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << " m_cause = ";
	if (m_cause >= 0 && m_cause < _countof(rgszCArchiveExceptionCause))
		dc << rgszCArchiveExceptionCause[m_cause];
	else
		dc << szUnknown;

	dc << "\n";
}
#endif //_DEBUG

void AFXAPI AfxThrowArchiveException(int cause,
	LPCTSTR lpszArchiveName /* = NULL */)
{
#ifdef _DEBUG
	LPCSTR lpsz;
	if (cause >= 0 && cause < _countof(rgszCArchiveExceptionCause))
		lpsz = rgszCArchiveExceptionCause[cause];
	else
		lpsz = szUnknown;
	TRACE1("CArchive exception: %hs.\n", lpsz);

#endif //_DEBUG

	THROW(new CArchiveException(cause, lpszArchiveName));
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CArchiveException, CException)

/////////////////////////////////////////////////////////////////////////////
