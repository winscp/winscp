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

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// _AfxConvertDialogUnitsToPixels

AFX_STATIC void AFXAPI _AfxConvertDialogUnitsToPixels(LPCTSTR pszFontFace, WORD wFontSize,
	int cxDlg, int cyDlg, SIZE* pSizePixel)
{
	// Attempt to create the font to be used in the dialog box
	UINT cxSysChar, cySysChar;
	LOGFONT lf;
	HDC hDC = ::GetDC(NULL);
	memset(&lf, 0, sizeof(LOGFONT));
	lf.lfHeight = -MulDiv(wFontSize, GetDeviceCaps(hDC, LOGPIXELSY), 72);
	lf.lfWeight = FW_NORMAL;
	lf.lfCharSet = DEFAULT_CHARSET;
	lstrcpy(lf.lfFaceName, pszFontFace);

	HFONT hNewFont = CreateFontIndirect(&lf);
	if (hNewFont != NULL)
	{
		HFONT hFontOld = (HFONT)SelectObject(hDC, hNewFont);
		TEXTMETRIC tm;
		GetTextMetrics(hDC, &tm);
		cySysChar = tm.tmHeight + tm.tmExternalLeading;
		SIZE size;
		::GetTextExtentPoint32(hDC,
			_T("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"), 52,
			&size);
		cxSysChar = (size.cx + 26) / 52;
		SelectObject(hDC, hFontOld);
		DeleteObject(hNewFont);
	}
	else
	{
		// Could not create the font so just use the system's values
		cxSysChar = LOWORD(GetDialogBaseUnits());
		cySysChar = HIWORD(GetDialogBaseUnits());
	}
	::ReleaseDC(NULL, hDC);

	// Translate dialog units to pixels
	pSizePixel->cx = MulDiv(cxDlg, cxSysChar, 4);
	pSizePixel->cy = MulDiv(cyDlg, cySysChar, 8);
}

/////////////////////////////////////////////////////////////////////////////
// IsDialogEx

AFX_STATIC inline BOOL IsDialogEx(const DLGTEMPLATE* pTemplate)
{
	return ((DLGTEMPLATEEX*)pTemplate)->signature == 0xFFFF;
}

/////////////////////////////////////////////////////////////////////////////
// HasFont

AFX_STATIC inline BOOL HasFont(const DLGTEMPLATE* pTemplate)
{
	return (DS_SETFONT &
		(IsDialogEx(pTemplate) ? ((DLGTEMPLATEEX*)pTemplate)->style :
		pTemplate->style));
}

/////////////////////////////////////////////////////////////////////////////
// FontAttrSize

AFX_STATIC inline int FontAttrSize(BOOL bDialogEx)
{
	return sizeof(WORD) * (bDialogEx ? 3 : 1);
}

/////////////////////////////////////////////////////////////////////////////
// CDialogTemplate - implementation class

CDialogTemplate::CDialogTemplate(const DLGTEMPLATE* pTemplate)
{
	if (pTemplate == NULL)
	{
		m_hTemplate = NULL;
		m_dwTemplateSize = 0;
		m_bSystemFont = FALSE;
	}
	else
	{
		SetTemplate(pTemplate, GetTemplateSize(pTemplate));
	}
}

CDialogTemplate::CDialogTemplate(HGLOBAL hTemplate)
{
	if (hTemplate == NULL)
	{
		m_hTemplate = NULL;
		m_dwTemplateSize = 0;
		m_bSystemFont = FALSE;
	}
	else
	{
		DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)GlobalLock(hTemplate);
		SetTemplate(pTemplate, GetTemplateSize(pTemplate));
		GlobalUnlock(hTemplate);
	}
}

BOOL CDialogTemplate::SetTemplate(const DLGTEMPLATE* pTemplate, UINT cb)
{
	m_dwTemplateSize = cb;

	if ((m_hTemplate = GlobalAlloc(GPTR, m_dwTemplateSize + LF_FACESIZE * 2)) == NULL)
		return FALSE;
	DLGTEMPLATE* pNew = (DLGTEMPLATE*)GlobalLock(m_hTemplate);
	memcpy((BYTE*)pNew, pTemplate, (size_t)m_dwTemplateSize);

	m_bSystemFont = (::HasFont(pNew) == 0);

	GlobalUnlock(m_hTemplate);
	return TRUE;
}

CDialogTemplate::~CDialogTemplate()
{
	if (m_hTemplate != NULL)
		GlobalFree(m_hTemplate);
}

BOOL CDialogTemplate::Load(LPCTSTR lpDialogTemplateID)
{
	HINSTANCE hInst = AfxFindResourceHandle(lpDialogTemplateID, RT_DIALOG);
	if (hInst == NULL)
		return FALSE;
	HRSRC hRsrc = FindResource(hInst, lpDialogTemplateID, RT_DIALOG);
	if (hRsrc == NULL)
		return FALSE;
	HGLOBAL hTemplate = LoadResource(hInst, hRsrc);
	DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)LockResource(hTemplate);
	SetTemplate(pTemplate, (UINT)SizeofResource(hInst, hRsrc));
	UnlockResource(hTemplate);
	FreeResource(hTemplate);
	return TRUE;
}

HGLOBAL CDialogTemplate::Detach()
{
	HGLOBAL hTmp = m_hTemplate;
	m_hTemplate = NULL;
	return hTmp;
}

BOOL CDialogTemplate::HasFont() const
{
	DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)GlobalLock(m_hTemplate);
	BOOL bHasFont = ::HasFont(pTemplate);
	GlobalUnlock(m_hTemplate);
	return bHasFont;
}

inline WCHAR* _SkipString(WCHAR* p)
{
	while (*p++);
	return p;
}

BYTE* AFX_CDECL CDialogTemplate::GetFontSizeField(const DLGTEMPLATE* pTemplate)
{
	BOOL bDialogEx = IsDialogEx(pTemplate);
	WORD* pw;

	if (bDialogEx)
		pw = (WORD*)((DLGTEMPLATEEX*)pTemplate + 1);
	else
		pw = (WORD*)(pTemplate + 1);

	if (*pw == (WORD)-1)        // Skip menu name string or ordinal
		pw += 2; // WORDs
	else
		while(*pw++);

	if (*pw == (WORD)-1)        // Skip class name string or ordinal
		pw += 2; // WORDs
	else
		while(*pw++);

	while (*pw++);          // Skip caption string

	return (BYTE*)pw;
}

UINT AFX_CDECL CDialogTemplate::GetTemplateSize(const DLGTEMPLATE* pTemplate)
{
	BOOL bDialogEx = IsDialogEx(pTemplate);
	BYTE* pb = GetFontSizeField(pTemplate);

	if (::HasFont(pTemplate))
	{
		// Skip font size and name
		pb += FontAttrSize(bDialogEx);  // Skip font size, weight, (italic, charset)
		pb += 2 * (wcslen((WCHAR*)pb) + 1);
	}

	WORD nCtrl = bDialogEx ? (WORD)((DLGTEMPLATEEX*)pTemplate)->cDlgItems :
		(WORD)pTemplate->cdit;

	while (nCtrl > 0)
	{
		pb = (BYTE*)(((DWORD)pb + 3) & ~3); // DWORD align

		pb += (bDialogEx ? sizeof(DLGITEMTEMPLATEEX) : sizeof(DLGITEMTEMPLATE));

		if (*(WORD*)pb == (WORD)-1)     // Skip class name string or ordinal
			pb += 2 * sizeof(WORD);
		else
			pb = (BYTE*)_SkipString((WCHAR*)pb);

		if (*(WORD*)pb == (WORD)-1)     // Skip text string or ordinal
			pb += 2 * sizeof(WORD);
		else
			pb = (BYTE*)_SkipString((WCHAR*)pb);

		WORD cbExtra = *(WORD*)pb;      // Skip extra data
		pb += sizeof(WORD) + cbExtra;
		--nCtrl;
	}

	return pb - (BYTE*)pTemplate;
}

BOOL AFX_CDECL CDialogTemplate::GetFont(const DLGTEMPLATE* pTemplate,
	CString& strFace, WORD& nFontSize)
{
	ASSERT(pTemplate != NULL);

	if (!::HasFont(pTemplate))
		return FALSE;

	BYTE* pb = GetFontSizeField(pTemplate);
	nFontSize = *(WORD*)pb;
	pb += FontAttrSize(IsDialogEx(pTemplate));

#if defined(_UNICODE) || defined(OLE2ANSI)
	// Copy font name
	strFace = (LPCTSTR)pb;
#else
	// Convert Unicode font name to MBCS
	WideCharToMultiByte(CP_ACP, 0, (LPCWSTR)pb, -1,
		strFace.GetBufferSetLength(LF_FACESIZE), LF_FACESIZE, NULL, NULL);
	strFace.ReleaseBuffer();
#endif

	return TRUE;
}

BOOL CDialogTemplate::GetFont(CString& strFace, WORD& nFontSize) const
{
	ASSERT(m_hTemplate != NULL);

	DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)GlobalLock(m_hTemplate);
	BOOL bResult = GetFont(pTemplate, strFace, nFontSize);
	GlobalUnlock(m_hTemplate);
	return bResult;
}

BOOL CDialogTemplate::SetFont(LPCTSTR lpFaceName, WORD nFontSize)
{
	ASSERT(m_hTemplate != NULL);

	if (m_dwTemplateSize == 0)
		return FALSE;

	DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)GlobalLock(m_hTemplate);

	BOOL bDialogEx = IsDialogEx(pTemplate);
	BOOL bHasFont = ::HasFont(pTemplate);
	int cbFontAttr = FontAttrSize(bDialogEx);

	if (bDialogEx)
		((DLGTEMPLATEEX*)pTemplate)->style |= DS_SETFONT;
	else
		pTemplate->style |= DS_SETFONT;

#ifdef _UNICODE
	int cbNew = cbFontAttr + ((lstrlen(lpFaceName) + 1) * sizeof(TCHAR));
	BYTE* pbNew = (BYTE*)lpFaceName;
#else
	WCHAR wszFaceName [LF_FACESIZE];
	int cbNew = cbFontAttr + 2 * MultiByteToWideChar(CP_ACP, 0, lpFaceName, -1, wszFaceName, LF_FACESIZE);
	BYTE* pbNew = (BYTE*)wszFaceName;
#endif

	BYTE* pb = GetFontSizeField(pTemplate);
	int cbOld = bHasFont ? cbFontAttr + 2 * (wcslen((WCHAR*)(pb + cbFontAttr)) + 1) : 0;

	BYTE* pOldControls = (BYTE*)(((DWORD)pb + cbOld + 3) & ~3);
	BYTE* pNewControls = (BYTE*)(((DWORD)pb + cbNew + 3) & ~3);

	WORD nCtrl = bDialogEx ? (WORD)((DLGTEMPLATEEX*)pTemplate)->cDlgItems :
		(WORD)pTemplate->cdit;

	if (cbNew != cbOld && nCtrl > 0)
		memmove(pNewControls, pOldControls, (size_t)(m_dwTemplateSize - (pOldControls - (BYTE*)pTemplate)));

	*(WORD*)pb = nFontSize;
	memmove(pb + cbFontAttr, pbNew, cbNew - cbFontAttr);

	m_dwTemplateSize += (pNewControls - pOldControls);

	GlobalUnlock(m_hTemplate);
	m_bSystemFont = FALSE;
	return TRUE;
}

BOOL CDialogTemplate::SetSystemFont(WORD wSize)
{
	LPCTSTR pszFace = _T("System");
	WORD wDefSize = 10;
	HFONT hFont = (HFONT)::GetStockObject(DEFAULT_GUI_FONT);
	if (hFont == NULL)
		hFont = (HFONT)::GetStockObject(SYSTEM_FONT);
	if (hFont != NULL)
	{
		LOGFONT lf;
		if (::GetObject(hFont, sizeof(LOGFONT), &lf) != 0)
		{
			pszFace = lf.lfFaceName;
			HDC hDC = ::GetDC(NULL);
			if (lf.lfHeight < 0)
				lf.lfHeight = -lf.lfHeight;
			wDefSize = (WORD)MulDiv(lf.lfHeight, 72, GetDeviceCaps(hDC, LOGPIXELSY));
			::ReleaseDC(NULL, hDC);
		}
	}

	if (wSize == 0)
		wSize = wDefSize;

	return SetFont(pszFace, wSize);
}

void CDialogTemplate::GetSizeInDialogUnits(SIZE* pSize) const
{
	ASSERT(m_hTemplate != NULL);
	ASSERT_POINTER(pSize, SIZE);

	DLGTEMPLATE* pTemplate = (DLGTEMPLATE*)GlobalLock(m_hTemplate);

	if (IsDialogEx(pTemplate))
	{
		pSize->cx = ((DLGTEMPLATEEX*)pTemplate)->cx;
		pSize->cy = ((DLGTEMPLATEEX*)pTemplate)->cy;
	}
	else
	{
		pSize->cx = pTemplate->cx;
		pSize->cy = pTemplate->cy;
	}

	GlobalUnlock(m_hTemplate);
}

void CDialogTemplate::GetSizeInPixels(SIZE* pSize) const
{
	ASSERT(m_hTemplate != NULL);
	ASSERT_POINTER(pSize, SIZE);

	if (m_bSystemFont)
	{
		GetSizeInDialogUnits(pSize);
		DWORD dwDLU = GetDialogBaseUnits();
		pSize->cx = (pSize->cx * LOWORD(dwDLU)) / 4;
		pSize->cy = (pSize->cy * HIWORD(dwDLU)) / 8;
	}
	else
	{
		CString strFace;
		WORD wSize;
		GetFont(strFace, wSize);

		SIZE size;
		GetSizeInDialogUnits(&size);
		_AfxConvertDialogUnitsToPixels(strFace, wSize, size.cx, size.cy, pSize);
	}
}
