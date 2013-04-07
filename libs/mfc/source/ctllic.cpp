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

#ifdef AFXCTL_FACT_SEG
#pragma code_seg(AFXCTL_FACT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// AfxVerifyLicFile - Checks that a license file exists and contains a
//    specific byte pattern.

BOOL AFXAPI AfxVerifyLicFile(HINSTANCE hInstance, LPCTSTR pszLicFileName,
	LPCOLESTR pszLicFileContents, UINT cch)
{
	// Assume the worst...
	BOOL bVerified = FALSE;

	// Look for license file in same directory as this DLL.
	TCHAR szPathName[_MAX_PATH];
	::GetModuleFileName(hInstance, szPathName, _MAX_PATH);
	LPTSTR pszFileName = _tcsrchr(szPathName, '\\') + 1;
	lstrcpy(pszFileName, pszLicFileName);

#ifndef OLE2ANSI
	LPSTR pszKey = NULL;
#endif
	LPBYTE pbContent = NULL;

	TRY
	{
		// Open file, read content and compare.

		CFile file(szPathName, CFile::modeRead);

		if (cch == -1)
#ifdef OLE2ANSI
			cch = lstrlen(pszLicFileContents);
#else
			cch = wcslen(pszLicFileContents);

		pszKey = (char*)_alloca(cch*2 + 1);
		cch = _wcstombsz(pszKey, pszLicFileContents, cch*2 + 1);
#endif

		if (cch != 0)
		{
			--cch;  // license file won't contain the terminating null char
			pbContent = (BYTE*)_alloca(cch);
			file.Read(pbContent, cch);

#ifndef OLE2ANSI
			if (memcmp(pszKey, pbContent, (size_t)cch) == 0)
#else
			if (memcmp(pszLicFileContents, pbContent, (size_t)cch) == 0)
#endif
				bVerified = TRUE;
		}
	}
	END_TRY

	return bVerified;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
