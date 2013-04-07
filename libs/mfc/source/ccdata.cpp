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

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#pragma comment(lib, "imagehlp.lib")
#pragma comment(lib, "comctl32.lib")
#pragma comment(lib, "shell32.lib")
#pragma comment(lib, "comdlg32.lib")
#pragma comment(lib, "winspool.lib")
#pragma comment(lib, "advapi32.lib")

/////////////////////////////////////////////////////////////////////////////
// AfxGetPropSheetFont

struct _AFX_PROPPAGEFONTINFO : public CNoTrackObject
{
	LPTSTR m_pszFaceName;
	WORD m_wSize;
	_AFX_PROPPAGEFONTINFO() : m_pszFaceName(NULL), m_wSize(0) {}
	~_AFX_PROPPAGEFONTINFO() { GlobalFree(m_pszFaceName); }
};

PROCESS_LOCAL(_AFX_PROPPAGEFONTINFO, _afxPropPageFontInfo)

#define IDD_PROPSHEET   1006
#define IDD_WIZARD      1020

BOOL AFXAPI AfxGetPropSheetFont(CString& strFace, WORD& wSize, BOOL bWizard)
{
	_AFX_PROPPAGEFONTINFO* pFontInfo = _afxPropPageFontInfo.GetData();

	// determine which font property sheet will use
	if (pFontInfo->m_wSize == 0)
	{
		ASSERT(pFontInfo->m_pszFaceName == NULL);

		HINSTANCE hInst = GetModuleHandleA("COMCTL32.DLL");
		if (hInst != NULL)
		{
			HRSRC hResource = ::FindResource(hInst,
				MAKEINTRESOURCE(bWizard ? IDD_WIZARD : IDD_PROPSHEET),
				RT_DIALOG);
			HGLOBAL hTemplate = LoadResource(hInst, hResource);
			if (hTemplate != NULL)
				CDialogTemplate::GetFont((DLGTEMPLATE*)hTemplate, strFace,
					wSize);
		}

		pFontInfo->m_pszFaceName = (LPTSTR)GlobalAlloc(GPTR, sizeof(TCHAR) *
			(strFace.GetLength() + 1));
		lstrcpy(pFontInfo->m_pszFaceName, strFace);
		pFontInfo->m_wSize = wSize;
	}

	strFace = pFontInfo->m_pszFaceName;
	wSize = pFontInfo->m_wSize;

	return (wSize != 0xFFFF);
}

/////////////////////////////////////////////////////////////////////////////
