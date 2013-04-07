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
#include <ctype.h>

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CEditView

#define new DEBUG_NEW

AFX_STATIC const UINT _afxMsgFindReplace = ::RegisterWindowMessage(FINDMSGSTRING);

#ifdef _UNICODE

AFX_STATIC_DATA HFONT _afxUnicodeFont = 0;

void AFX_CDECL AfxEditviewTerm()
{
	AfxDeleteObject((HGDIOBJ*)&_afxUnicodeFont);
}
char _afxEditviewTerm = (char)atexit(&AfxEditviewTerm);

#endif //_UNICODE

BEGIN_MESSAGE_MAP(CEditView, CCtrlView)
	//{{AFX_MSG_MAP(CEditView)
	ON_UPDATE_COMMAND_UI(ID_EDIT_CUT, OnUpdateNeedSel)
	ON_UPDATE_COMMAND_UI(ID_EDIT_PASTE, OnUpdateNeedClip)
	ON_UPDATE_COMMAND_UI(ID_EDIT_SELECT_ALL, OnUpdateNeedText)
	ON_UPDATE_COMMAND_UI(ID_EDIT_UNDO, OnUpdateEditUndo)
	ON_UPDATE_COMMAND_UI(ID_EDIT_FIND, OnUpdateNeedText)
	ON_UPDATE_COMMAND_UI(ID_EDIT_REPLACE, OnUpdateNeedText)
	ON_UPDATE_COMMAND_UI(ID_EDIT_REPEAT, OnUpdateNeedFind)
	ON_UPDATE_COMMAND_UI(ID_EDIT_COPY, OnUpdateNeedSel)
	ON_UPDATE_COMMAND_UI(ID_EDIT_CLEAR, OnUpdateNeedSel)
	ON_CONTROL_REFLECT_EX(EN_CHANGE, OnEditChange)
	ON_WM_CREATE()
	ON_MESSAGE(WM_SETFONT, OnSetFont)
	ON_COMMAND(ID_EDIT_CUT, OnEditCut)
	ON_COMMAND(ID_EDIT_COPY, OnEditCopy)
	ON_COMMAND(ID_EDIT_PASTE, OnEditPaste)
	ON_COMMAND(ID_EDIT_CLEAR, OnEditClear)
	ON_COMMAND(ID_EDIT_UNDO, OnEditUndo)
	ON_COMMAND(ID_EDIT_SELECT_ALL, OnEditSelectAll)
	ON_COMMAND(ID_EDIT_FIND, OnEditFind)
	ON_COMMAND(ID_EDIT_REPLACE, OnEditReplace)
	ON_COMMAND(ID_EDIT_REPEAT, OnEditRepeat)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
	// Special registered message for Find and Replace
	ON_REGISTERED_MESSAGE(_afxMsgFindReplace, OnFindReplaceCmd)
	// Standard Print commands (print only - not preview)
	ON_COMMAND(ID_FILE_PRINT, CCtrlView::OnFilePrint)
	ON_COMMAND(ID_FILE_PRINT_DIRECT, CCtrlView::OnFilePrint)
END_MESSAGE_MAP()

const AFX_DATADEF DWORD CEditView::dwStyleDefault =
	AFX_WS_DEFAULT_VIEW |
	WS_HSCROLL | WS_VSCROLL |
	ES_AUTOHSCROLL | ES_AUTOVSCROLL |
	ES_MULTILINE | ES_NOHIDESEL;

// Operating system specific maximum buffer limit
const AFX_DATADEF UINT CEditView::nMaxSize = 1024U*1024U-1;

/////////////////////////////////////////////////////////////////////////////
// _AFX_EDIT_STATE

_AFX_EDIT_STATE::_AFX_EDIT_STATE()
{
	// Note: it is only necessary to initialize non-zero data.

	bNext = TRUE;
}

_AFX_EDIT_STATE::~_AFX_EDIT_STATE()
{
}

EXTERN_PROCESS_LOCAL(_AFX_EDIT_STATE, _afxEditState)

/////////////////////////////////////////////////////////////////////////////
// CEditView construction/destruction

// pass a NULL style because dwStyleDefault stays for backward compatibility
CEditView::CEditView() : CCtrlView(_T("EDIT"), NULL)
{
	m_nTabStops = 8*4;  // default 8 character positions
	m_hPrinterFont = NULL;
	m_hMirrorFont = NULL;
	m_pShadowBuffer = NULL;
	m_nShadowSize = 0;
}

CEditView::~CEditView()
{
	ASSERT(m_hWnd == NULL);
	ASSERT(m_pShadowBuffer == NULL || afxData.bWin95);
	delete[] m_pShadowBuffer;
}

BOOL CEditView::PreCreateWindow(CREATESTRUCT& cs)
{
	m_dwDefaultStyle = dwStyleDefault;
	return CCtrlView::PreCreateWindow(cs);
}

int CEditView::OnCreate(LPCREATESTRUCT lpcs)
{
	if (CCtrlView::OnCreate(lpcs) != 0)
		return -1;

#ifdef _UNICODE
	AfxLockGlobals(CRIT_EDITVIEW);
	if (_afxUnicodeFont == NULL)
	{
		// get unicode font same size as system font
		HFONT hSystemFont = (HFONT)GetStockObject(SYSTEM_FONT);
		LOGFONT systemFont;
		VERIFY(::GetObject(hSystemFont, sizeof(LOGFONT), (void*)&systemFont));

		// default size and facename, but allow customization
		LOGFONT logFont; memset(&logFont, 0, sizeof(LOGFONT));
		logFont.lfHeight = systemFont.lfHeight;
		logFont.lfWeight = systemFont.lfWeight;
		logFont.lfCharSet = DEFAULT_CHARSET;
		lstrcpy(logFont.lfFaceName, _T("Lucida Sans Unicode"));
		AfxCustomLogFont(AFX_IDS_UNICODE_FONT, &logFont);

		// attempt to create the font
		_afxUnicodeFont = ::CreateFontIndirect(&logFont);
		if (_afxUnicodeFont == NULL)
			TRACE1("Unable to create unicode font '%s'.\n", logFont.lfFaceName);
	}
	AfxUnlockGlobals(CRIT_EDITVIEW);
	// set unicode font instead of using system font
	if (_afxUnicodeFont != NULL)
		SendMessage(WM_SETFONT, (WPARAM)_afxUnicodeFont);
#endif

	GetEditCtrl().LimitText(nMaxSize);
	GetEditCtrl().SetTabStops(m_nTabStops);

	return 0;
}

void CEditView::OnDestroy()
{
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	pEditState->pFindReplaceDlg = NULL;

	CView::OnDestroy();
}

// EDIT controls always turn off WS_BORDER and draw it themselves
void CEditView::CalcWindowRect(LPRECT lpClientRect, UINT nAdjustType)
{
	if (nAdjustType != 0)
	{
		// default behavior for in-place editing handles scrollbars
		DWORD dwStyle = GetStyle();
		if (dwStyle & WS_VSCROLL)
			lpClientRect->right += afxData.cxVScroll - CX_BORDER;
		if (dwStyle & WS_HSCROLL)
			lpClientRect->bottom += afxData.cyHScroll - CY_BORDER;
		return;
	}

	::AdjustWindowRectEx(lpClientRect, GetStyle() | WS_BORDER, FALSE,
		GetExStyle() & ~(WS_EX_CLIENTEDGE));
}

/////////////////////////////////////////////////////////////////////////////
// CEditView document like functions

void CEditView::DeleteContents()
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	SetWindowText(NULL);
	ASSERT_VALID(this);
}

void CEditView::Serialize(CArchive& ar)
	// Read and write CEditView object to archive, with length prefix.
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	if (ar.IsStoring())
	{
		UINT nLen = GetBufferLength();
		ar << (DWORD)nLen;
		WriteToArchive(ar);
	}
	else
	{
		DWORD dwLen;
		ar >> dwLen;
		if (dwLen > nMaxSize)
			AfxThrowArchiveException(CArchiveException::badIndex);
		UINT nLen = (UINT)dwLen;
		ReadFromArchive(ar, nLen);
	}
	ASSERT_VALID(this);
}

void CEditView::ReadFromArchive(CArchive& ar, UINT nLen)
	// Read certain amount of text from the file, assume at least nLen
	// characters (not bytes) are in the file.
{
	ASSERT_VALID(this);

	LPVOID hText = LocalAlloc(LMEM_MOVEABLE, (nLen+1)*sizeof(TCHAR));
	if (hText == NULL)
		AfxThrowMemoryException();

	LPTSTR lpszText = (LPTSTR)LocalLock(hText);
	ASSERT(lpszText != NULL);
	if (ar.Read(lpszText, nLen*sizeof(TCHAR)) != nLen*sizeof(TCHAR))
	{
		LocalUnlock(hText);
		LocalFree(hText);
		AfxThrowArchiveException(CArchiveException::endOfFile);
	}
	// Replace the editing edit buffer with the newly loaded data
	lpszText[nLen] = '\0';
#ifndef _UNICODE
	if (afxData.bWin95)
	{
		// set the text with SetWindowText, then free
		BOOL bResult = ::SetWindowText(m_hWnd, lpszText);
		LocalUnlock(hText);
		LocalFree(hText);

		// make sure that SetWindowText was successful
		if (!bResult || ::GetWindowTextLength(m_hWnd) < (int)nLen)
			AfxThrowMemoryException();

		// remove old shadow buffer
		delete[] m_pShadowBuffer;
		m_pShadowBuffer = NULL;
		m_nShadowSize = 0;

		ASSERT_VALID(this);
		return;
	}
#endif
	LocalUnlock(hText);
	HLOCAL hOldText = GetEditCtrl().GetHandle();
	ASSERT(hOldText != NULL);
	LocalFree(hOldText);
	GetEditCtrl().SetHandle((HLOCAL)(UINT)(DWORD)hText);
	Invalidate();
	ASSERT_VALID(this);
}

void CEditView::WriteToArchive(CArchive& ar)
	// Write just the text to an archive, no length prefix.
{
	ASSERT_VALID(this);
	LPCTSTR lpszText = LockBuffer();
	ASSERT(lpszText != NULL);
	UINT nLen = GetBufferLength();
	TRY
	{
		ar.Write(lpszText, nLen*sizeof(TCHAR));
	}
	CATCH_ALL(e)
	{
		UnlockBuffer();
		THROW_LAST();
	}
	END_CATCH_ALL
	UnlockBuffer();
	ASSERT_VALID(this);
}

void CEditView::SerializeRaw(CArchive& ar)
	// Read/Write object as stand-alone file.
{
	ASSERT_VALID(this);
	if (ar.IsStoring())
	{
		WriteToArchive(ar);
	}
	else
	{
		CFile* pFile = ar.GetFile();
		ASSERT(pFile->GetPosition() == 0);
		DWORD nFileSize = pFile->GetLength();
		if (nFileSize/sizeof(TCHAR) > nMaxSize)
		{
			AfxMessageBox(AFX_IDP_FILE_TOO_LARGE);
			AfxThrowUserException();
		}
		// ReadFromArchive takes the number of characters as argument
		ReadFromArchive(ar, (UINT)nFileSize/sizeof(TCHAR));
	}
	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView Printing Helpers

AFX_STATIC UINT AFXAPI _AfxEndOfLine(LPCTSTR lpszText, UINT nLen, UINT nIndex)
{
	ASSERT(AfxIsValidAddress(lpszText, nLen, FALSE));
	LPCTSTR lpsz = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nLen;
	while (lpsz < lpszStop && *lpsz != '\r')
		++lpsz;
	return lpsz - lpszText;
}

AFX_STATIC UINT AFXAPI _AfxNextLine(LPCTSTR lpszText, UINT nLen, UINT nIndex)
{
	ASSERT(AfxIsValidAddress(lpszText, nLen, FALSE));
	LPCTSTR lpsz = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nLen;
	while (lpsz < lpszStop && *lpsz == '\r')
		++lpsz;
	if (lpsz < lpszStop && *lpsz == '\n')
		++lpsz;
	return lpsz - lpszText;
}

AFX_STATIC UINT AFXAPI
_AfxClipLine(CDC* pDC, int aCharWidths[256], int cxLine, int nTabStop,
	LPCTSTR lpszText, UINT nIndex, UINT nIndexEnd)
{
	ASSERT_VALID(pDC);
	ASSERT(nIndex < nIndexEnd);
	ASSERT(AfxIsValidAddress(lpszText, nIndexEnd, FALSE));

	TEXTMETRIC tm;
	::GetTextMetrics(pDC->m_hDC, &tm);

	// make an initial guess on the number of characters that will fit
	int cx = 0;
	LPCTSTR lpszStart = lpszText + nIndex;
	LPCTSTR lpszStop = lpszText + nIndexEnd;
	LPCTSTR lpsz = lpszStart;
	while (lpsz < lpszStop)
	{
		if (*lpsz == '\t')
			cx += nTabStop - (cx % nTabStop);
		else
		{
#ifdef _UNICODE
			if (*lpsz <= 0xFF)
				cx += aCharWidths[(BYTE)*lpsz];
			else
				cx += tm.tmAveCharWidth;
#else //_UNICODE
			if (_afxDBCS && _istlead(*lpsz))
			{
				++lpsz;
				cx += tm.tmAveCharWidth;
			}
			else
				cx += aCharWidths[(BYTE)*lpsz];
#endif //!_UNICODE
		}
		++lpsz;
		if (cx > cxLine)
			break;
	}

	// adjust for errors in the guess
	cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
	if (cx > cxLine)
	{
		// remove characters until it fits
		do
		{
			ASSERT(lpsz != lpszStart);
			if (_afxDBCS)
				lpsz = _tcsdec(lpszStart, lpsz);
			else
				--lpsz;
			cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
		} while (cx > cxLine);
	}
	else if (cx < cxLine)
	{
		// add characters until it doesn't fit
		while (lpsz < lpszStop)
		{
			lpsz = _tcsinc(lpsz);
			ASSERT(lpsz <= lpszStop);
			cx = pDC->GetTabbedTextExtent(lpszStart, lpsz-lpszStart, 1, &nTabStop).cx;
			if (cx > cxLine)
			{
				if (_afxDBCS)
					lpsz = _tcsdec(lpszStart, lpsz);
				else
					--lpsz;
				break;
			}
		}
	}

	// return index of character just past the last that would fit
	return lpsz - lpszText;
}

/////////////////////////////////////////////////////////////////////////////
// CEditView Printing support

BOOL CEditView::OnPreparePrinting(CPrintInfo* pInfo)
{
	return DoPreparePrinting(pInfo);
}

void CEditView::OnBeginPrinting(CDC* pDC, CPrintInfo*)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	// initialize page start vector
	ASSERT(m_aPageStart.GetSize() == 0);
	m_aPageStart.Add(0);
	ASSERT(m_aPageStart.GetSize() > 0);

	if (m_hPrinterFont == NULL)
	{
		// get current screen font object metrics
		CFont* pFont = GetFont();
		LOGFONT lf;
		LOGFONT lfSys;
		if (pFont == NULL)
			return;
		VERIFY(pFont->GetObject(sizeof(LOGFONT), &lf));
		VERIFY(::GetObject(::GetStockObject(SYSTEM_FONT), sizeof(LOGFONT),
			&lfSys));
		if (lstrcmpi((LPCTSTR)lf.lfFaceName, (LPCTSTR)lfSys.lfFaceName) == 0)
			return;

		// map to printer font metrics
		HDC hDCFrom = ::GetDC(NULL);
		lf.lfHeight = ::MulDiv(lf.lfHeight, pDC->GetDeviceCaps(LOGPIXELSY),
			::GetDeviceCaps(hDCFrom, LOGPIXELSY));
		lf.lfWidth = ::MulDiv(lf.lfWidth, pDC->GetDeviceCaps(LOGPIXELSX),
			::GetDeviceCaps(hDCFrom, LOGPIXELSX));
		::ReleaseDC(NULL, hDCFrom);

		// create it, if it fails we just use the printer's default.
		m_hMirrorFont = ::CreateFontIndirect(&lf);
		m_hPrinterFont = m_hMirrorFont;
	}
	ASSERT_VALID(this);
}

BOOL CEditView::PaginateTo(CDC* pDC, CPrintInfo* pInfo)
	// attempts pagination to pInfo->m_nCurPage, TRUE == success
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	CRect rectSave = pInfo->m_rectDraw;
	UINT nPageSave = pInfo->m_nCurPage;
	ASSERT(nPageSave > 1);
	ASSERT(nPageSave >= (UINT)m_aPageStart.GetSize());
	VERIFY(pDC->SaveDC() != 0);
	pDC->IntersectClipRect(0, 0, 0, 0);
	pInfo->m_nCurPage = m_aPageStart.GetSize();
	while (pInfo->m_nCurPage < nPageSave)
	{
		ASSERT(pInfo->m_nCurPage == (UINT)m_aPageStart.GetSize());
		OnPrepareDC(pDC, pInfo);
		ASSERT(pInfo->m_bContinuePrinting);
		pInfo->m_rectDraw.SetRect(0, 0,
			pDC->GetDeviceCaps(HORZRES), pDC->GetDeviceCaps(VERTRES));
		pDC->DPtoLP(&pInfo->m_rectDraw);
		OnPrint(pDC, pInfo);
		if (pInfo->m_nCurPage == (UINT)m_aPageStart.GetSize())
			break;
		++pInfo->m_nCurPage;
	}
	BOOL bResult = pInfo->m_nCurPage == nPageSave;
	pDC->RestoreDC(-1);
	pInfo->m_nCurPage = nPageSave;
	pInfo->m_rectDraw = rectSave;
	ASSERT_VALID(this);
	return bResult;
}

void CEditView::OnPrepareDC(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	ASSERT(pInfo != NULL);  // overriding OnPaint -- never get this.

	if (pInfo->m_nCurPage > (UINT)m_aPageStart.GetSize() &&
		!PaginateTo(pDC, pInfo))
	{
		// can't paginate to that page, thus cannot print it.
		pInfo->m_bContinuePrinting = FALSE;
	}
	ASSERT_VALID(this);
}

UINT CEditView::PrintInsideRect(CDC* pDC, RECT& rectLayout,
	UINT nIndexStart, UINT nIndexStop)
	// worker function for laying out text in a rectangle.
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	BOOL bWordWrap = (GetStyle() & ES_AUTOHSCROLL) == 0;

	// get buffer and real starting and ending postions
	UINT nLen = GetBufferLength();
	if (nIndexStart >= nLen)
		return nLen;
	LPCTSTR lpszText = LockBuffer();
	if (nIndexStop > nLen)
		nIndexStop = nLen;
	ASSERT(nIndexStart < nLen);

	// calculate text & tab metrics
	TEXTMETRIC tm;
	pDC->GetTextMetrics(&tm);
	int cyChar = tm.tmHeight + tm.tmExternalLeading;
	int nTabStop = m_nTabStops *
		pDC->GetTabbedTextExtent(_T("\t"), 1, 0, NULL).cx / 8 / 4;
	int aCharWidths[256];
	pDC->GetCharWidth(0, 255, aCharWidths);

	int y = rectLayout.top;
	UINT cx = rectLayout.right - rectLayout.left;
	UINT nIndex = nIndexStart;

	VERIFY(pDC->SaveDC() != 0);
	BOOL bLayoutOnly = pDC->IntersectClipRect(&rectLayout) == NULLREGION;

	do
	{
		UINT nIndexEnd = _AfxEndOfLine(lpszText, nIndexStop, nIndex);
		if (nIndex == nIndexEnd)
		{
			y += cyChar;
		}
		else if (bWordWrap)
		{
			// word-wrap printing
			do
			{
				UINT nIndexWrap = _AfxClipLine(pDC, aCharWidths,
					cx, nTabStop, lpszText, nIndex, nIndexEnd);
				UINT nIndexWord = nIndexWrap;
				if (nIndexWord != nIndexEnd)
				{
					while (nIndexWord > nIndex &&
					  !_istspace(lpszText[nIndexWord]))
					{
						nIndexWord--;
					}
					if (nIndexWord == nIndex)
						nIndexWord = nIndexWrap;
				}
				CRect rect(rectLayout.left, y, rectLayout.right, y+cyChar);
				if (!bLayoutOnly && pDC->RectVisible(rect))
				{
					pDC->TabbedTextOut(rect.left, y,
						(LPCTSTR)(lpszText+nIndex), nIndexWord-nIndex, 1,
						&nTabStop, rect.left);
				}
				y += cyChar;
				nIndex = nIndexWord;
				while (nIndex < nIndexEnd && _istspace(lpszText[nIndex]))
					nIndex++;
			} while (nIndex < nIndexEnd && y+cyChar <= rectLayout.bottom);

			nIndexEnd = nIndex;
		}
		else
		{
			// non-word wrap printing (much easier and faster)
			CRect rect(rectLayout.left, y, rectLayout.right, y+cyChar);
			if (!bLayoutOnly && pDC->RectVisible(rect))
			{
				UINT nIndexClip = _AfxClipLine(pDC, aCharWidths, cx, nTabStop,
					lpszText, nIndex, nIndexEnd);
				if (nIndexClip < nIndexEnd)
				{
					if (_istlead(*(lpszText+nIndexClip)))
						nIndexClip++;
					nIndexClip++;
				}
				pDC->TabbedTextOut(rect.left, y,
					(LPCTSTR)(lpszText+nIndex), nIndexClip-nIndex, 1,
					&nTabStop, rect.left);
			}
			y += cyChar;
		}
		nIndex = _AfxNextLine(lpszText, nIndexStop, nIndexEnd);
	}
	while (nIndex < nIndexStop && y+cyChar <= rectLayout.bottom);

	pDC->RestoreDC(-1);
	UnlockBuffer();
	ASSERT_VALID(this);

	rectLayout.bottom = y;
	return nIndex;
}

void CEditView::OnPrint(CDC* pDC, CPrintInfo* pInfo)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);
	ASSERT(pInfo != NULL);
	ASSERT(pInfo->m_bContinuePrinting);

	CFont* pOldFont = NULL;
	if (m_hPrinterFont != NULL)
		pOldFont = pDC->SelectObject(CFont::FromHandle(m_hPrinterFont));
	pDC->SetBkMode(TRANSPARENT);

	UINT nPage = pInfo->m_nCurPage;
	ASSERT(nPage <= (UINT)m_aPageStart.GetSize());
	UINT nIndex = m_aPageStart[nPage-1];

	// print as much as possible in the current page.
	nIndex = PrintInsideRect(pDC, pInfo->m_rectDraw, nIndex, GetBufferLength());

	if (pOldFont != NULL)
		pDC->SelectObject(pOldFont);

	// update pagination information for page just printed
	if (nPage == (UINT)m_aPageStart.GetSize())
	{
		if (nIndex < GetBufferLength())
			m_aPageStart.Add(nIndex);
	}
	else
	{
		ASSERT(nPage+1 <= (UINT)m_aPageStart.GetSize());
		ASSERT(nIndex == m_aPageStart[nPage+1-1]);
	}
}

void CEditView::OnEndPrinting(CDC*, CPrintInfo*)
{
	ASSERT_VALID(this);

	m_aPageStart.RemoveAll();
	if (m_hMirrorFont != NULL && m_hPrinterFont == m_hMirrorFont)
	{
		AfxDeleteObject((HGDIOBJ*)&m_hMirrorFont);
		m_hPrinterFont = NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CEditView commands

void CEditView::OnUpdateNeedSel(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	int nStartChar, nEndChar;
	GetEditCtrl().GetSel(nStartChar, nEndChar);
	pCmdUI->Enable(nStartChar != nEndChar);
	ASSERT_VALID(this);
}

void CEditView::OnUpdateNeedClip(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	pCmdUI->Enable(::IsClipboardFormatAvailable(CF_TEXT));
	ASSERT_VALID(this);
}

void CEditView::OnUpdateNeedText(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	pCmdUI->Enable(GetWindowTextLength() != 0);
	ASSERT_VALID(this);
}

void CEditView::OnUpdateNeedFind(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	pCmdUI->Enable(GetWindowTextLength() != 0 &&
		!pEditState->strFind.IsEmpty());
	ASSERT_VALID(this);
}

void CEditView::OnUpdateEditUndo(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	pCmdUI->Enable(GetEditCtrl().CanUndo());
	ASSERT_VALID(this);
}

BOOL CEditView::OnEditChange()
{
	ASSERT_VALID(this);
	GetDocument()->SetModifiedFlag();
	ASSERT_VALID(this);

	return FALSE;   // continue routing
}

void CEditView::OnEditCut()
{
	ASSERT_VALID(this);
	GetEditCtrl().Cut();
	ASSERT_VALID(this);
}

void CEditView::OnEditCopy()
{
	ASSERT_VALID(this);
	GetEditCtrl().Copy();
	ASSERT_VALID(this);
}

void CEditView::OnEditPaste()
{
	ASSERT_VALID(this);
	GetEditCtrl().Paste();
	ASSERT_VALID(this);
}

void CEditView::OnEditClear()
{
	ASSERT_VALID(this);
	GetEditCtrl().Clear();
	ASSERT_VALID(this);
}

void CEditView::OnEditUndo()
{
	ASSERT_VALID(this);
	GetEditCtrl().Undo();
	ASSERT_VALID(this);
}

void CEditView::OnEditSelectAll()
{
	ASSERT_VALID(this);
	GetEditCtrl().SetSel(0, -1);
	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView Font Handling

LRESULT CEditView::OnSetFont(WPARAM, LPARAM)
{
	ASSERT_VALID(this);
	Default();
	GetEditCtrl().SetTabStops(m_nTabStops);
	ASSERT_VALID(this);
	return 0;
}

void CEditView::SetPrinterFont(CFont* pFont)
{
	ASSERT_VALID(this);
	m_hPrinterFont = (HFONT)pFont->GetSafeHandle();
	ASSERT_VALID(this);
}

CFont* CEditView::GetPrinterFont() const
{
	ASSERT_VALID(this);
	return CFont::FromHandle(m_hPrinterFont);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView attributes

LPCTSTR CEditView::LockBuffer() const
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
#ifndef _UNICODE
	if (afxData.bWin95)
	{
		// under Win32s, it is necessary to maintain a shadow buffer
		//  it is only updated when the control contents have been changed.
		if (m_pShadowBuffer == NULL || GetEditCtrl().GetModify())
		{
			ASSERT(m_pShadowBuffer != NULL || m_nShadowSize == 0);
			UINT nSize = GetWindowTextLength()+1;
			if (nSize > m_nShadowSize)
			{
				// need more room for shadow buffer
				CEditView* pThis = (CEditView*)this;
				delete[] m_pShadowBuffer;
				pThis->m_pShadowBuffer = NULL;
				pThis->m_nShadowSize = 0;
				pThis->m_pShadowBuffer = new TCHAR[nSize];
				pThis->m_nShadowSize = nSize;
			}

			// update the shadow buffer with GetWindowText
			ASSERT(m_nShadowSize >= nSize);
			ASSERT(m_pShadowBuffer != NULL);
			GetWindowText(m_pShadowBuffer, nSize);

			// turn off edit control's modify bit
			GetEditCtrl().SetModify(FALSE);
		}
		return m_pShadowBuffer;
	}
#endif
	// else -- running under non-subset Win32 system
	HLOCAL hLocal = GetEditCtrl().GetHandle();
	ASSERT(hLocal != NULL);
	LPCTSTR lpszText = (LPCTSTR)LocalLock(hLocal);
	ASSERT(lpszText != NULL);
	ASSERT_VALID(this);
	return lpszText;
}

void CEditView::UnlockBuffer() const
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
#ifndef _UNICODE
	if (afxData.bWin95)
		return;
#endif
	HLOCAL hLocal = GetEditCtrl().GetHandle();
	ASSERT(hLocal != NULL);
	LocalUnlock(hLocal);
}

// this function returns the length in characters
UINT CEditView::GetBufferLength() const
{
	ASSERT_VALID(this);
	ASSERT(m_hWnd != NULL);
	LPCTSTR lpszText = LockBuffer();
	UINT nLen = lstrlen(lpszText);
	UnlockBuffer();
	return nLen;
}

void CEditView::GetSelectedText(CString& strResult) const
{
	ASSERT_VALID(this);
	int nStartChar, nEndChar;
	GetEditCtrl().GetSel(nStartChar, nEndChar);
	ASSERT((UINT)nEndChar <= GetBufferLength());
	LPCTSTR lpszText = ((CEditView*)this)->LockBuffer();
	UINT nLen = _AfxEndOfLine(lpszText, nEndChar, nStartChar) - nStartChar;
	memcpy(strResult.GetBuffer(nLen), lpszText + nStartChar,
		nLen * sizeof(TCHAR));
	strResult.ReleaseBuffer(nLen);
	UnlockBuffer();
	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView Find & Replace

void CEditView::OnEditFind()
{
	ASSERT_VALID(this);
	OnEditFindReplace(TRUE);
	ASSERT_VALID(this);
}

void CEditView::OnEditReplace()
{
	ASSERT_VALID(this);
	OnEditFindReplace(FALSE);
	ASSERT_VALID(this);
}

void CEditView::OnEditRepeat()
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	if (!FindText(pEditState->strFind,
		pEditState->bNext,
		pEditState->bCase))
	{
		OnTextNotFound(pEditState->strFind);
	}
	ASSERT_VALID(this);
}

void CEditView::OnEditFindReplace(BOOL bFindOnly)
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	if (pEditState->pFindReplaceDlg != NULL)
	{
		if (pEditState->bFindOnly == bFindOnly)
		{
			pEditState->pFindReplaceDlg->SetActiveWindow();
			pEditState->pFindReplaceDlg->ShowWindow(SW_SHOW);
			return;
		}
		ASSERT(pEditState->bFindOnly != bFindOnly);
		pEditState->pFindReplaceDlg->SendMessage(WM_CLOSE);
		ASSERT(pEditState->pFindReplaceDlg == NULL);
		ASSERT_VALID(this);
	}

	CString strFind;
	GetSelectedText(strFind);
	if (strFind.IsEmpty())
		strFind = pEditState->strFind;
	CString strReplace = pEditState->strReplace;
	pEditState->pFindReplaceDlg = new CFindReplaceDialog;
	ASSERT(pEditState->pFindReplaceDlg != NULL);
	DWORD dwFlags = FR_HIDEWHOLEWORD;
	if (pEditState->bNext)
		dwFlags |= FR_DOWN;
	if (pEditState->bCase)
		dwFlags |= FR_MATCHCASE;
	if (!pEditState->pFindReplaceDlg->Create(bFindOnly, strFind,
		strReplace, dwFlags, this))
	{
		pEditState->pFindReplaceDlg = NULL;
		ASSERT_VALID(this);
		return;
	}

	pEditState->pFindReplaceDlg->SetActiveWindow();
	pEditState->pFindReplaceDlg->ShowWindow(SW_SHOW);
	ASSERT(pEditState->pFindReplaceDlg != NULL);
	pEditState->bFindOnly = bFindOnly;
	ASSERT_VALID(this);
}

void CEditView::OnFindNext(LPCTSTR lpszFind, BOOL bNext, BOOL bCase)
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	pEditState->strFind = lpszFind;
	pEditState->bCase = bCase;
	pEditState->bNext = bNext;

	if (!FindText(pEditState->strFind, bNext, bCase))
		OnTextNotFound(pEditState->strFind);
	ASSERT_VALID(this);
}

void CEditView::OnReplaceSel(LPCTSTR lpszFind, BOOL bNext, BOOL bCase,
	LPCTSTR lpszReplace)
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	pEditState->strFind = lpszFind;
	pEditState->strReplace = lpszReplace;
	pEditState->bCase = bCase;
	pEditState->bNext = bNext;

	if (!InitializeReplace())
		return;

	GetEditCtrl().ReplaceSel(pEditState->strReplace);
	FindText(pEditState->strFind, bNext, bCase);
	ASSERT_VALID(this);
}

void CEditView::OnReplaceAll(LPCTSTR lpszFind, LPCTSTR lpszReplace, BOOL bCase)
{
	ASSERT_VALID(this);
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	pEditState->strFind = lpszFind;
	pEditState->strReplace = lpszReplace;
	pEditState->bCase = bCase;
	pEditState->bNext = TRUE;

	if (!InitializeReplace() &&
		!SameAsSelected(pEditState->strFind, pEditState->bCase))
	{
		// initial find was not successful
		return;
	}

	do
	{
		GetEditCtrl().ReplaceSel(pEditState->strReplace);
	} while (FindText(pEditState->strFind, 1, bCase));

	ASSERT_VALID(this);
}

BOOL CEditView::InitializeReplace()
	// helper to do find first if no selection
{
	ASSERT_VALID(this);

	_AFX_EDIT_STATE* pEditState = _afxEditState;

	// do find next if no selection
	int nStartChar, nEndChar;
	GetEditCtrl().GetSel(nStartChar, nEndChar);
	if (nStartChar == nEndChar)
	{
		if (!FindText(pEditState->strFind, pEditState->bNext,
			pEditState->bCase))
		{
			// text not found
			OnTextNotFound(pEditState->strFind);
		}
		return FALSE;
	}

	if (!SameAsSelected(pEditState->strFind, pEditState->bCase))
	{
		if (!FindText(pEditState->strFind, pEditState->bNext,
			pEditState->bCase))
		{
			// text not found
			OnTextNotFound(pEditState->strFind);
		}
		return FALSE;
	}

	ASSERT_VALID(this);
	return TRUE;
}

LRESULT CEditView::OnFindReplaceCmd(WPARAM, LPARAM lParam)
{
	ASSERT_VALID(this);

	_AFX_EDIT_STATE* pEditState = _afxEditState;
	CFindReplaceDialog* pDialog = CFindReplaceDialog::GetNotifier(lParam);
	ASSERT(pDialog != NULL);
	ASSERT(pDialog == pEditState->pFindReplaceDlg);
	if (pDialog->IsTerminating())
	{
		pEditState->pFindReplaceDlg = NULL;
	}
	else if (pDialog->FindNext())
	{
		OnFindNext(pDialog->GetFindString(),
			pDialog->SearchDown(), pDialog->MatchCase());
	}
	else if (pDialog->ReplaceCurrent())
	{
		ASSERT(!pEditState->bFindOnly);
		OnReplaceSel(pDialog->GetFindString(),
			pDialog->SearchDown(), pDialog->MatchCase(),
			pDialog->GetReplaceString());
	}
	else if (pDialog->ReplaceAll())
	{
		ASSERT(!pEditState->bFindOnly);
		OnReplaceAll(pDialog->GetFindString(), pDialog->GetReplaceString(),
			pDialog->MatchCase());
	}
	ASSERT_VALID(this);
	return 0;
}

typedef int (WINAPI* AFX_COMPARE_PROC)(LPCTSTR str1, LPCTSTR str2);

BOOL CEditView::SameAsSelected(LPCTSTR lpszCompare, BOOL bCase)
{
	// check length first
	size_t nLen = lstrlen(lpszCompare);
	int nStartChar, nEndChar;
	GetEditCtrl().GetSel(nStartChar, nEndChar);
	if (nLen != (size_t)(nEndChar - nStartChar))
		return FALSE;

	// length is the same, check contents
	CString strSelect;
	GetSelectedText(strSelect);
	return (bCase && lstrcmp(lpszCompare, strSelect) == 0) ||
		(!bCase && lstrcmpi(lpszCompare, strSelect) == 0);
}

BOOL CEditView::FindText(LPCTSTR lpszFind, BOOL bNext, BOOL bCase)
{
	ASSERT_VALID(this);
	ASSERT(lpszFind != NULL);
	ASSERT(*lpszFind != '\0');

	UINT nLen = GetBufferLength();
	int nStartChar, nEndChar;
	GetEditCtrl().GetSel(nStartChar, nEndChar);
	UINT nStart = nStartChar;
	int iDir = bNext ? +1 : -1;

	// can't find a match before the first character
	if (nStart == 0 && iDir < 0)
		return FALSE;

	CWaitCursor wait;
	LPCTSTR lpszText = LockBuffer();

	if (iDir < 0)
	{
		// always go back one for search backwards
		nStart -= (lpszText+nStart) -
			_tcsdec(lpszText, lpszText+nStart);
	}
	else if (nStartChar != nEndChar && SameAsSelected(lpszFind, bCase))
	{
		// easy to go backward/forward with SBCS
		if (_istlead(lpszText[nStart]))
			nStart++;
		nStart += iDir;
	}

	// handle search with nStart past end of buffer
	size_t nLenFind = lstrlen(lpszFind);
	if (nStart+nLenFind-1 >= nLen)
	{
		if (iDir < 0 && nLen >= nLenFind)
		{
			if (_afxDBCS)
			{
				// walk back to previous character n times
				nStart = nLen;
				int n = nLenFind;
				while (n--)
				{
					nStart -= (lpszText+nStart) -
						_tcsdec(lpszText, lpszText+nStart);
				}
			}
			else
			{
				// single-byte character set is easy and fast
				nStart = nLen - nLenFind;
			}
			ASSERT(nStart+nLenFind-1 <= nLen);
		}
		else
		{
			UnlockBuffer();
			return FALSE;
		}
	}

	// start the search at nStart
	LPCTSTR lpsz = lpszText + nStart;
	AFX_COMPARE_PROC pfnCompare = bCase ? lstrcmp : lstrcmpi;

	if (_afxDBCS)
	{
		// double-byte string search
		LPCTSTR lpszStop;
		if (iDir > 0)
		{
			// start at current and find _first_ occurrance
			lpszStop = lpszText + nLen - nLenFind + 1;
		}
		else
		{
			// start at top and find _last_ occurrance
			lpszStop = lpsz;
			lpsz = lpszText;
		}

		LPCTSTR lpszFound = NULL;
		while (lpsz <= lpszStop)
		{
			if (!bCase || (*lpsz == *lpszFind &&
				(!_istlead(*lpsz) || lpsz[1] == lpszFind[1])))
			{
				LPTSTR lpch = (LPTSTR)(lpsz + nLenFind);
				TCHAR chSave = *lpch;
				*lpch = '\0';
				int nResult = (*pfnCompare)(lpsz, lpszFind);
				*lpch = chSave;
				if (nResult == 0)
				{
					lpszFound = lpsz;
					if (iDir > 0)
						break;
				}
			}
			lpsz = _tcsinc(lpsz);
		}
		UnlockBuffer();

		if (lpszFound != NULL)
		{
			int n = (int)(lpszFound - lpszText);
			GetEditCtrl().SetSel(n, n+nLenFind);
			return TRUE;
		}
	}
	else
	{
		// single-byte string search
		UINT nCompare;
		if (iDir < 0)
			nCompare = (UINT)(lpsz - lpszText) + 1;
		else
			nCompare = nLen - (UINT)(lpsz - lpszText) - nLenFind + 1;

		while (nCompare > 0)
		{
			ASSERT(lpsz >= lpszText);
			ASSERT(lpsz+nLenFind-1 <= lpszText+nLen-1);

			LPSTR lpch = (LPSTR)(lpsz + nLenFind);
			char chSave = *lpch;
			*lpch = '\0';
			int nResult = (*pfnCompare)(lpsz, lpszFind);
			*lpch = chSave;
			if (nResult == 0)
			{
				UnlockBuffer();
				int n = (int)(lpsz - lpszText);
				GetEditCtrl().SetSel(n, n+nLenFind);
				ASSERT_VALID(this);
				return TRUE;
			}

			// restore character at end of search
			*lpch = chSave;

			// move on to next substring
			nCompare--;
			lpsz += iDir;
		}
		UnlockBuffer();
	}

	ASSERT_VALID(this);
	return FALSE;
}

void CEditView::OnTextNotFound(LPCTSTR)
{
	ASSERT_VALID(this);
	MessageBeep(0);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView Tab Stops

void CEditView::SetTabStops(int nTabStops)
{
	ASSERT_VALID(this);
	m_nTabStops = nTabStops;
	GetEditCtrl().SetTabStops(m_nTabStops);
	Invalidate();
	ASSERT_VALID(this);
}

/////////////////////////////////////////////////////////////////////////////
// CEditView diagnostics

#ifdef _DEBUG
void CEditView::AssertValid() const
{
	CCtrlView::AssertValid();
	ASSERT_VALID(&m_aPageStart);
	if (m_hPrinterFont != NULL)
		ASSERT_VALID(CFont::FromHandle(m_hPrinterFont));
	if (m_hMirrorFont != NULL)
		ASSERT_VALID(CFont::FromHandle(m_hMirrorFont));
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	if (pEditState->pFindReplaceDlg != NULL)
		ASSERT_VALID(pEditState->pFindReplaceDlg);
}

void CEditView::Dump(CDumpContext& dc) const
{
	CCtrlView::Dump(dc);

	dc << "m_nTabStops = " << m_nTabStops;
	if (m_hPrinterFont != NULL)
		dc << "\nm_hPrinterFont " << (UINT)m_hPrinterFont;
	if (m_hMirrorFont != NULL)
		dc << "\nm_hMirrorFont " << (UINT)m_hMirrorFont;
	dc << "\nm_aPageStart: " << &m_aPageStart;
	dc << "\nstatic member data:";
	_AFX_EDIT_STATE* pEditState = _afxEditState;
	if (pEditState->pFindReplaceDlg != NULL)
	{
		dc << "\npFindReplaceDlg = "
			<< (void*)pEditState->pFindReplaceDlg;
		dc << "\nbFindOnly = " << pEditState->bFindOnly;
	}
	dc << "\nstrFind = " << pEditState->strFind;
	dc << "\nstrReplace = " << pEditState->strReplace;
	dc << "\nbCase = " << pEditState->bCase;
	dc << "\nbNext = " << pEditState->bNext;

	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CEditView, CCtrlView)

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_EDIT_STATE, _afxEditState)

/////////////////////////////////////////////////////////////////////////////
