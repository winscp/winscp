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
#include <dlgs.h>       // for standard control IDs for commdlg

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
// FileOpen/FileSaveAs common dialog helper

CFileDialog::CFileDialog(BOOL bOpenFileDialog,
	LPCTSTR lpszDefExt, LPCTSTR lpszFileName, DWORD dwFlags,
	LPCTSTR lpszFilter, CWnd* pParentWnd) : CCommonDialog(pParentWnd)
{
	memset(&m_ofn, 0, sizeof(m_ofn)); // initialize structure to 0/NULL
	m_szFileName[0] = '\0';
	m_szFileTitle[0] = '\0';
	m_pofnTemp = NULL;

	m_bOpenFileDialog = bOpenFileDialog;
	m_nIDHelp = bOpenFileDialog ? AFX_IDD_FILEOPEN : AFX_IDD_FILESAVE;

	m_ofn.lStructSize = sizeof(m_ofn);
	m_ofn.lpstrFile = m_szFileName;
	m_ofn.nMaxFile = _countof(m_szFileName);
	m_ofn.lpstrDefExt = lpszDefExt;
	m_ofn.lpstrFileTitle = (LPTSTR)m_szFileTitle;
	m_ofn.nMaxFileTitle = _countof(m_szFileTitle);
	m_ofn.Flags |= dwFlags | OFN_ENABLEHOOK | OFN_ENABLESIZING;
	if (!afxData.bWin4 && AfxHelpEnabled())
		m_ofn.Flags |= OFN_SHOWHELP;
	if (afxData.bWin4)
	{
		m_ofn.Flags |= OFN_EXPLORER;
		m_ofn.hInstance = AfxGetResourceHandle();
	}
	m_ofn.lpfnHook = (COMMDLGPROC)_AfxCommDlgProc;

	// setup initial file name
	if (lpszFileName != NULL)
		lstrcpyn(m_szFileName, lpszFileName, _countof(m_szFileName));

	// Translate filter into commdlg format (lots of \0)
	if (lpszFilter != NULL)
	{
		m_strFilter = lpszFilter;
		LPTSTR pch = m_strFilter.GetBuffer(0); // modify the buffer in place
		// MFC delimits with '|' not '\0'
		while ((pch = _tcschr(pch, '|')) != NULL)
			*pch++ = '\0';
		m_ofn.lpstrFilter = m_strFilter;
		// do not call ReleaseBuffer() since the string contains '\0' characters
	}
}

int CFileDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_ofn.Flags & OFN_ENABLEHOOK);
	ASSERT(m_ofn.lpfnHook != NULL); // can still be a user hook

	// zero out the file buffer for consistent parsing later
	ASSERT(AfxIsValidAddress(m_ofn.lpstrFile, m_ofn.nMaxFile));
	DWORD nOffset = lstrlen(m_ofn.lpstrFile)+1;
	ASSERT(nOffset <= m_ofn.nMaxFile);
	memset(m_ofn.lpstrFile+nOffset, 0, (m_ofn.nMaxFile-nOffset)*sizeof(TCHAR));

	// WINBUG: This is a special case for the file open/save dialog,
	//  which sometimes pumps while it is coming up but before it has
	//  disabled the main window.
	HWND hWndFocus = ::GetFocus();
	BOOL bEnableParent = FALSE;
	m_ofn.hwndOwner = PreModal();
	AfxUnhookWindowCreate();
	if (m_ofn.hwndOwner != NULL && ::IsWindowEnabled(m_ofn.hwndOwner))
	{
		bEnableParent = TRUE;
		::EnableWindow(m_ofn.hwndOwner, FALSE);
	}

	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	ASSERT(pThreadState->m_pAlternateWndInit == NULL);

	if (m_ofn.Flags & OFN_EXPLORER)
		pThreadState->m_pAlternateWndInit = this;
	else
		AfxHookWindowCreate(this);

	int nResult;
	if (m_bOpenFileDialog)
		nResult = ::GetOpenFileName(&m_ofn);
	else
		nResult = ::GetSaveFileName(&m_ofn);

	if (nResult)
		ASSERT(pThreadState->m_pAlternateWndInit == NULL);
	pThreadState->m_pAlternateWndInit = NULL;

	// WINBUG: Second part of special case for file open/save dialog.
	if (bEnableParent)
		::EnableWindow(m_ofn.hwndOwner, TRUE);
	if (::IsWindow(hWndFocus))
		::SetFocus(hWndFocus);

	PostModal();
	return nResult ? nResult : IDCANCEL;
}

CString CFileDialog::GetPathName() const
{
	if ((m_ofn.Flags & OFN_EXPLORER) && m_hWnd != NULL)
	{
		ASSERT(::IsWindow(m_hWnd));
		CString strResult;
		if (GetParent()->SendMessage(CDM_GETSPEC, (WPARAM)MAX_PATH,
			(LPARAM)strResult.GetBuffer(MAX_PATH)) < 0)
		{
			strResult.Empty();
		}
		else
		{
			strResult.ReleaseBuffer();
		}

		if (!strResult.IsEmpty())
		{
			if (GetParent()->SendMessage(CDM_GETFILEPATH, (WPARAM)MAX_PATH,
				(LPARAM)strResult.GetBuffer(MAX_PATH)) < 0)
				strResult.Empty();
			else
			{
				strResult.ReleaseBuffer();
				return strResult;
			}
		}
	}
	return m_ofn.lpstrFile;
}

CString CFileDialog::GetFileName() const
{
	if ((m_ofn.Flags & OFN_EXPLORER) && m_hWnd != NULL)
	{
		ASSERT(::IsWindow(m_hWnd));
		CString strResult;
		if (GetParent()->SendMessage(CDM_GETSPEC, (WPARAM)MAX_PATH,
			(LPARAM)strResult.GetBuffer(MAX_PATH)) < 0)
		{
			strResult.Empty();
		}
		else
		{
			strResult.ReleaseBuffer();
			return strResult;
		}
	}
	return m_ofn.lpstrFileTitle;
}

CString CFileDialog::GetFileExt() const
{
	if ((m_ofn.Flags & OFN_EXPLORER) && m_hWnd != NULL)
	{
		ASSERT(::IsWindow(m_hWnd));
		CString strResult;
		if (GetParent()->SendMessage(CDM_GETSPEC, (WPARAM)MAX_PATH,
			(LPARAM)strResult.GetBuffer(MAX_PATH)) < 0)
			strResult.Empty();
		else
		{
			strResult.ReleaseBuffer();
			int pos = strResult.ReverseFind('.');
			if (pos >= 0)
				return strResult.Right(strResult.GetLength() - pos - 1);
			strResult.Empty();
		}
		return strResult;
	}

	if (m_pofnTemp != NULL)
		if (m_pofnTemp->nFileExtension == 0)
			return &afxChNil;
		else
			return m_pofnTemp->lpstrFile + m_pofnTemp->nFileExtension;

	if (m_ofn.nFileExtension == 0)
		return &afxChNil;
	else
		return m_ofn.lpstrFile + m_ofn.nFileExtension;
}

CString CFileDialog::GetFileTitle() const
{
	CString strResult = GetFileName();
	int pos = strResult.ReverseFind('.');
	if (pos >= 0)
		return strResult.Left(pos);
	return strResult;
}

CString CFileDialog::GetNextPathName(POSITION& pos) const
{
	BOOL bExplorer = m_ofn.Flags & OFN_EXPLORER;
	TCHAR chDelimiter;
	if (bExplorer)
		chDelimiter = '\0';
	else
		chDelimiter = ' ';

	LPTSTR lpsz = (LPTSTR)pos;
	if (lpsz == m_ofn.lpstrFile) // first time
	{
		if ((m_ofn.Flags & OFN_ALLOWMULTISELECT) == 0)
		{
			pos = NULL;
			return m_ofn.lpstrFile;
		}

		// find char pos after first Delimiter
		while(*lpsz != chDelimiter && *lpsz != '\0')
			lpsz = _tcsinc(lpsz);
		lpsz = _tcsinc(lpsz);

		// if single selection then return only selection
		if (*lpsz == 0)
		{
			pos = NULL;
			return m_ofn.lpstrFile;
		}
	}

	CString strPath = m_ofn.lpstrFile;
	if (!bExplorer)
	{
		LPTSTR lpszPath = m_ofn.lpstrFile;
		while(*lpszPath != chDelimiter)
			lpszPath = _tcsinc(lpszPath);
		strPath = strPath.Left(lpszPath - m_ofn.lpstrFile);
	}

	LPTSTR lpszFileName = lpsz;
	CString strFileName = lpsz;

	// find char pos at next Delimiter
	while(*lpsz != chDelimiter && *lpsz != '\0')
		lpsz = _tcsinc(lpsz);

	if (!bExplorer && *lpsz == '\0')
		pos = NULL;
	else
	{
		if (!bExplorer)
			strFileName = strFileName.Left(lpsz - lpszFileName);

		lpsz = _tcsinc(lpsz);
		if (*lpsz == '\0') // if double terminated then done
			pos = NULL;
		else
			pos = (POSITION)lpsz;
	}

	// only add '\\' if it is needed
	if (!strPath.IsEmpty())
	{
		// check for last back-slash or forward slash (handles DBCS)
		LPCTSTR lpsz = _tcsrchr(strPath, '\\');
		if (lpsz == NULL)
			lpsz = _tcsrchr(strPath, '/');
		// if it is also the last character, then we don't need an extra
		if (lpsz != NULL &&
			(lpsz - (LPCTSTR)strPath) == strPath.GetLength()-1)
		{
			ASSERT(*lpsz == '\\' || *lpsz == '/');
			return strPath + strFileName;
		}
	}
	return strPath + '\\' + strFileName;
}

void CFileDialog::SetTemplate(LPCTSTR lpWin3ID, LPCTSTR lpWin4ID)
{
	if (m_ofn.Flags & OFN_EXPLORER)
		m_ofn.lpTemplateName = lpWin4ID;
	else
		m_ofn.lpTemplateName = lpWin3ID;
	m_ofn.Flags |= OFN_ENABLETEMPLATE;
}

CString CFileDialog::GetFolderPath() const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(m_ofn.Flags & OFN_EXPLORER);

	CString strResult;
	if (GetParent()->SendMessage(CDM_GETFOLDERPATH, (WPARAM)MAX_PATH, (LPARAM)strResult.GetBuffer(MAX_PATH)) < 0)
		strResult.Empty();
	else
		strResult.ReleaseBuffer();
	return strResult;
}

void CFileDialog::SetControlText(int nID, LPCSTR lpsz)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(m_ofn.Flags & OFN_EXPLORER);
	GetParent()->SendMessage(CDM_SETCONTROLTEXT, (WPARAM)nID, (LPARAM)lpsz);
}

void CFileDialog::HideControl(int nID)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(m_ofn.Flags & OFN_EXPLORER);
	GetParent()->SendMessage(CDM_HIDECONTROL, (WPARAM)nID, 0);
}

void CFileDialog::SetDefExt(LPCSTR lpsz)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(m_ofn.Flags & OFN_EXPLORER);
	GetParent()->SendMessage(CDM_SETDEFEXT, 0, (LPARAM)lpsz);
}

UINT CFileDialog::OnShareViolation(LPCTSTR)
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	return OFN_SHAREWARN; // default
}

BOOL CFileDialog::OnFileNameOK()
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	return FALSE;
}

void CFileDialog::OnLBSelChangedNotify(UINT, UINT, UINT)
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	// no default processing needed
}

void CFileDialog::OnInitDone()
{
	ASSERT_VALID(this);
	GetParent()->CenterWindow();

	// Do not call Default() if you override
	// no default processing needed
}

void CFileDialog::OnFileNameChange()
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	// no default processing needed
}

void CFileDialog::OnFolderChange()
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	// no default processing needed
}

void CFileDialog::OnTypeChange()
{
	ASSERT_VALID(this);

	// Do not call Default() if you override
	// no default processing needed
}

BOOL CFileDialog::OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult)
{
	ASSERT(pResult != NULL);

	// allow message map to override
	if (CCommonDialog::OnNotify(wParam, lParam, pResult))
		return TRUE;

	OFNOTIFY* pNotify = (OFNOTIFY*)lParam;
	switch(pNotify->hdr.code)
	{
	case CDN_INITDONE:
		OnInitDone();
		return TRUE;
	case CDN_SELCHANGE:
		OnFileNameChange();
		return TRUE;
	case CDN_FOLDERCHANGE:
		OnFolderChange();
		return TRUE;
	case CDN_SHAREVIOLATION:
		*pResult = OnShareViolation(pNotify->pszFile);
		return TRUE;
	case CDN_HELP:
		if (!SendMessage(WM_COMMAND, ID_HELP))
			SendMessage(WM_COMMANDHELP, 0, 0);
		return TRUE;
	case CDN_FILEOK:
		*pResult = OnFileNameOK();
		return TRUE;
	case CDN_TYPECHANGE:
		OnTypeChange();
		return TRUE;
	}

	return FALSE;   // not handled
}

////////////////////////////////////////////////////////////////////////////
// CFileDialog diagnostics

#ifdef _DEBUG
void CFileDialog::Dump(CDumpContext& dc) const
{
	CDialog::Dump(dc);

	if (m_bOpenFileDialog)
		dc << "File open dialog";
	else
		dc << "File save dialog";
	dc << "\nm_ofn.hwndOwner = " << (UINT)m_ofn.hwndOwner;
	dc << "\nm_ofn.nFilterIndex = " << m_ofn.nFilterIndex;
	dc << "\nm_ofn.lpstrFile = " << m_ofn.lpstrFile;
	dc << "\nm_ofn.nMaxFile = " << m_ofn.nMaxFile;
	dc << "\nm_ofn.lpstrFileTitle = " << m_ofn.lpstrFileTitle;
	dc << "\nm_ofn.nMaxFileTitle = " << m_ofn.nMaxFileTitle;
	dc << "\nm_ofn.lpstrTitle = " << m_ofn.lpstrTitle;
	dc << "\nm_ofn.Flags = " << (LPVOID)m_ofn.Flags;
	dc << "\nm_ofn.lpstrDefExt = " << m_ofn.lpstrDefExt;
	dc << "\nm_ofn.nFileOffset = " << m_ofn.nFileOffset;
	dc << "\nm_ofn.nFileExtension = " << m_ofn.nFileExtension;

	dc << "\nm_ofn.lpstrFilter = ";
	LPCTSTR lpstrItem = m_ofn.lpstrFilter;
	LPTSTR lpszBreak = _T("|");

	while (lpstrItem != NULL && *lpstrItem != '\0')
	{
		dc << lpstrItem << lpszBreak;
		lpstrItem += lstrlen(lpstrItem) + 1;
	}
	if (lpstrItem != NULL)
		dc << lpszBreak;

	dc << "\nm_ofn.lpstrCustomFilter = ";
	lpstrItem = m_ofn.lpstrCustomFilter;
	while (lpstrItem != NULL && *lpstrItem != '\0')
	{
		dc << lpstrItem << lpszBreak;
		lpstrItem += lstrlen(lpstrItem) + 1;
	}
	if (lpstrItem != NULL)
		dc << lpszBreak;

	if (m_ofn.lpfnHook == (COMMDLGPROC)_AfxCommDlgProc)
		dc << "\nhook function set to standard MFC hook function";
	else
		dc << "\nhook function set to non-standard hook function";

	dc << "\n";
}
#endif //_DEBUG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CFileDialog, CDialog)

////////////////////////////////////////////////////////////////////////////
