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

#ifdef AFX_CMNCTL_SEG
#pragma code_seg(AFX_CMNCTL_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifndef _AFX_NO_RICHEDIT_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// _AFX_RICHEDIT_STATE

_AFX_RICHEDIT_STATE::~_AFX_RICHEDIT_STATE()
{
	if (m_hInstRichEdit != NULL)
		::FreeLibrary(m_hInstRichEdit);
}

_AFX_RICHEDIT_STATE* AFX_CDECL AfxGetRichEditState()
{
	return _afxRichEditState.GetData();
}

BOOL PASCAL AfxInitRichEdit()
{
	_AFX_RICHEDIT_STATE* pState = _afxRichEditState;
	if (pState->m_hInstRichEdit == NULL)
		pState->m_hInstRichEdit = LoadLibraryA("RICHED32.DLL");
	return pState->m_hInstRichEdit != NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CRichEdit

BOOL CRichEditCtrl::Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID)
{
	if (!AfxInitRichEdit())
		return FALSE;

	CWnd* pWnd = this;
	return pWnd->Create(_T("RICHEDIT"), NULL, dwStyle, rect, pParentWnd, nID);
}

int CRichEditCtrl::GetLine(int nIndex, LPTSTR lpszBuffer) const
{
	ASSERT(::IsWindow(m_hWnd));
	return (int)::SendMessage(m_hWnd, EM_GETLINE, nIndex,
		(LPARAM)lpszBuffer);
}

int CRichEditCtrl::LineIndex(int nLine /* = -1 */) const
{
	ASSERT(::IsWindow(m_hWnd));
	return (int)::SendMessage(m_hWnd, EM_LINEINDEX, nLine, 0);
}

int CRichEditCtrl::LineLength(int nLine /* = -1 */) const
{
	ASSERT(::IsWindow(m_hWnd));
	return (int)::SendMessage(m_hWnd, EM_LINELENGTH, nLine, 0);
}

void CRichEditCtrl::LineScroll(int nLines, int nChars /* = 0 */)
{
	ASSERT(::IsWindow(m_hWnd));
	::SendMessage(m_hWnd, EM_LINESCROLL, nChars, nLines);
}

void CRichEditCtrl::SetSel(long nStartChar, long nEndChar)
{
	ASSERT(::IsWindow(m_hWnd));
	CHARRANGE cr;
	cr.cpMin = nStartChar;
	cr.cpMax = nEndChar;
	::SendMessage(m_hWnd, EM_EXSETSEL, 0, (LPARAM)&cr);
}

BOOL CRichEditCtrl::CanPaste(UINT nFormat) const
{
	ASSERT(::IsWindow(m_hWnd));
	COleMessageFilter* pFilter = AfxOleGetMessageFilter();
	if (pFilter != NULL)
		pFilter->BeginBusyState();
	BOOL b = (BOOL)::SendMessage(m_hWnd, EM_CANPASTE, nFormat, 0L);
	if (pFilter != NULL)
		pFilter->EndBusyState();
	return b;
}

void CRichEditCtrl::PasteSpecial(UINT nClipFormat, DWORD dvAspect, HMETAFILE hMF)
{
	ASSERT(::IsWindow(m_hWnd));
	REPASTESPECIAL reps;
	reps.dwAspect = dvAspect;
	reps.dwParam = (DWORD)hMF;
	::SendMessage(m_hWnd, EM_PASTESPECIAL, nClipFormat, (LPARAM)&reps);
}

int CRichEditCtrl::GetLine(int nIndex, LPTSTR lpszBuffer, int nMaxLength) const
{
	ASSERT(::IsWindow(m_hWnd));
	*(LPINT)lpszBuffer = nMaxLength;
	return (int)::SendMessage(m_hWnd, EM_GETLINE, nIndex, (LPARAM)lpszBuffer);
}

void CRichEditCtrl::GetSel(long& nStartChar, long& nEndChar) const
{
	ASSERT(::IsWindow(m_hWnd));
	CHARRANGE cr;
	::SendMessage(m_hWnd, EM_EXGETSEL, 0, (LPARAM)&cr);
	nStartChar = cr.cpMin;
	nEndChar = cr.cpMax;
}

CString CRichEditCtrl::GetSelText() const
{
	ASSERT(::IsWindow(m_hWnd));
	CHARRANGE cr;
	cr.cpMin = cr.cpMax = 0;
	::SendMessage(m_hWnd, EM_EXGETSEL, 0, (LPARAM)&cr);
	LPSTR lpsz = (char*)_alloca((cr.cpMax - cr.cpMin + 1)*2);
	lpsz[0] = NULL;
	::SendMessage(m_hWnd, EM_GETSELTEXT, 0, (LPARAM)lpsz);
	return lpsz;
}

IRichEditOle* CRichEditCtrl::GetIRichEditOle() const
{
	ASSERT(::IsWindow(m_hWnd));
	IRichEditOle *pRichItem = NULL;
	::SendMessage(m_hWnd, EM_GETOLEINTERFACE, 0, (LPARAM)&pRichItem);
	return pRichItem;
}

BOOL CRichEditCtrl::SetDefaultCharFormat(CHARFORMAT &cf)
{
	ASSERT(::IsWindow(m_hWnd));
	cf.cbSize = sizeof(CHARFORMAT);
	return (BOOL)::SendMessage(m_hWnd, EM_SETCHARFORMAT, 0, (LPARAM)&cf);
}

BOOL CRichEditCtrl::SetSelectionCharFormat(CHARFORMAT &cf)
{
	ASSERT(::IsWindow(m_hWnd));
	cf.cbSize = sizeof(CHARFORMAT);
	return (BOOL)::SendMessage(m_hWnd, EM_SETCHARFORMAT, SCF_SELECTION, (LPARAM)&cf);
}

BOOL CRichEditCtrl::SetWordCharFormat(CHARFORMAT &cf)
{
	ASSERT(::IsWindow(m_hWnd));
	cf.cbSize = sizeof(CHARFORMAT);
	return (BOOL)::SendMessage(m_hWnd, EM_SETCHARFORMAT, SCF_SELECTION|SCF_WORD, (LPARAM)&cf);
}

DWORD CRichEditCtrl::GetDefaultCharFormat(CHARFORMAT &cf) const
{
	ASSERT(::IsWindow(m_hWnd));
	cf.cbSize = sizeof(CHARFORMAT);
	return (DWORD)::SendMessage(m_hWnd, EM_GETCHARFORMAT, 0, (LPARAM)&cf);
}

DWORD CRichEditCtrl::GetSelectionCharFormat(CHARFORMAT &cf) const
{
	ASSERT(::IsWindow(m_hWnd));
	cf.cbSize = sizeof(CHARFORMAT);
	return (DWORD)::SendMessage(m_hWnd, EM_GETCHARFORMAT, 1, (LPARAM)&cf);
}

DWORD CRichEditCtrl::GetParaFormat(PARAFORMAT &pf) const
{
	ASSERT(::IsWindow(m_hWnd));
	pf.cbSize = sizeof(PARAFORMAT);
	return (DWORD)::SendMessage(m_hWnd, EM_GETPARAFORMAT, 0, (LPARAM)&pf);
}

BOOL CRichEditCtrl::SetParaFormat(PARAFORMAT &pf)
{
	ASSERT(::IsWindow(m_hWnd));
	pf.cbSize = sizeof(PARAFORMAT);
	return (BOOL)::SendMessage(m_hWnd, EM_SETPARAFORMAT, 0, (LPARAM)&pf);
}

#endif //!_AFX_NO_RICHEDIT_SUPPORT

/////////////////////////////////////////////////////////////////////////////

#pragma warning(disable: 4074)
#pragma init_seg(lib)

#ifndef _AFX_NO_RICHEDIT_SUPPORT
PROCESS_LOCAL(_AFX_RICHEDIT_STATE, _afxRichEditState)
#endif

/////////////////////////////////////////////////////////////////////////////
