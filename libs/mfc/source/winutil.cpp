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
#include <malloc.h>

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// Other helpers

BOOL AFXAPI AfxCustomLogFont(UINT nIDS, LOGFONT* pLogFont)
{
	ASSERT(pLogFont != NULL);
	ASSERT(nIDS != 0);

	TCHAR szFontInfo[256];
	if (!AfxLoadString(nIDS, szFontInfo))
		return FALSE;

	LPTSTR lpszSize = _tcschr(szFontInfo, '\n');
	if (lpszSize != NULL)
	{
		// get point size and convert to pixels
		pLogFont->lfHeight = _ttoi(lpszSize+1);
		pLogFont->lfHeight =
			MulDiv(pLogFont->lfHeight, afxData.cyPixelsPerInch, 72);
		*lpszSize = '\0';
	}
	lstrcpyn(pLogFont->lfFaceName, szFontInfo, LF_FACESIZE);
	return TRUE;
}

BOOL AFXAPI _AfxIsComboBoxControl(HWND hWnd, UINT nStyle)
{
	if (hWnd == NULL)
		return FALSE;
	// do cheap style compare first
	if ((UINT)(::GetWindowLong(hWnd, GWL_STYLE) & 0x0F) != nStyle)
		return FALSE;

	// do expensive classname compare next
	TCHAR szCompare[_countof("combobox")+1];
	::GetClassName(hWnd, szCompare, _countof(szCompare));
	return lstrcmpi(szCompare, _T("combobox")) == 0;
}

BOOL AFXAPI _AfxCompareClassName(HWND hWnd, LPCTSTR lpszClassName)
{
	ASSERT(::IsWindow(hWnd));
	TCHAR szTemp[32];
	::GetClassName(hWnd, szTemp, _countof(szTemp));
	return lstrcmpi(szTemp, lpszClassName) == 0;
}

HWND AFXAPI _AfxChildWindowFromPoint(HWND hWnd, POINT pt)
{
	ASSERT(hWnd != NULL);

	// check child windows
	::ClientToScreen(hWnd, &pt);
	HWND hWndChild = ::GetWindow(hWnd, GW_CHILD);
	for (; hWndChild != NULL; hWndChild = ::GetWindow(hWndChild, GW_HWNDNEXT))
	{
		if (_AfxGetDlgCtrlID(hWndChild) != (WORD)-1 &&
			(::GetWindowLong(hWndChild, GWL_STYLE) & WS_VISIBLE))
		{
			// see if point hits the child window
			CRect rect;
			::GetWindowRect(hWndChild, rect);
			if (rect.PtInRect(pt))
				return hWndChild;
		}
	}

	return NULL;    // not found
}

void AFXAPI AfxSetWindowText(HWND hWndCtrl, LPCTSTR lpszNew)
{
	int nNewLen = lstrlen(lpszNew);
	TCHAR szOld[256];
	// fast check to see if text really changes (reduces flash in controls)
	if (nNewLen > _countof(szOld) ||
		::GetWindowText(hWndCtrl, szOld, _countof(szOld)) != nNewLen ||
		lstrcmp(szOld, lpszNew) != 0)
	{
		// change it
		::SetWindowText(hWndCtrl, lpszNew);
	}
}

void AFXAPI AfxDeleteObject(HGDIOBJ* pObject)
{
	ASSERT(pObject != NULL);
	if (*pObject != NULL)
	{
		DeleteObject(*pObject);
		*pObject = NULL;
	}
}

void AFXAPI AfxCancelModes(HWND hWndRcvr)
{
	// if we receive a message destined for a window, cancel any combobox
	//  popups that could be in toolbars or dialog bars
	HWND hWndCancel = ::GetFocus();
	if (hWndCancel == NULL)
		return;     // nothing to cancel

	if (hWndCancel == hWndRcvr)
		return;     // let input go to window with focus

	// focus is in part of a combo-box
	if (!_AfxIsComboBoxControl(hWndCancel, (UINT)CBS_DROPDOWNLIST))
	{
		// check as a dropdown
		hWndCancel = ::GetParent(hWndCancel);   // parent of edit is combo
		if (hWndCancel == hWndRcvr)
			return;     // let input go to part of combo

		if (!_AfxIsComboBoxControl(hWndCancel, (UINT)CBS_DROPDOWN))
			return;     // not a combo-box that is active
	}

	// combo-box is active, but if receiver is a popup, do nothing
	if (hWndRcvr != NULL &&
	  (::GetWindowLong(hWndRcvr, GWL_STYLE) & WS_CHILD) != 0 &&
	  ::GetParent(hWndRcvr) == ::GetDesktopWindow())
		return;

	// finally, we should cancel the mode!
	::SendMessage(hWndCancel, CB_SHOWDROPDOWN, FALSE, 0L);
}

void AFXAPI AfxGlobalFree(HGLOBAL hGlobal)
{
	if (hGlobal == NULL)
		return;

	// avoid bogus warning error messages from various debugging tools
	ASSERT(GlobalFlags(hGlobal) != GMEM_INVALID_HANDLE);
	UINT nCount = GlobalFlags(hGlobal) & GMEM_LOCKCOUNT;
	while (nCount--)
		GlobalUnlock(hGlobal);

	// finally, really free the handle
	GlobalFree(hGlobal);
}

/////////////////////////////////////////////////////////////////////////////
// Special new handler for safety pool on temp maps

#ifndef _AFX_PORTABLE

#define MIN_MALLOC_OVERHEAD 4   // LocalAlloc or other overhead

int AFX_CDECL AfxCriticalNewHandler(size_t nSize)
	// nSize is already rounded
{
	// called during critical memory allocation
	//  free up part of the app's safety cache
	TRACE0("Warning: Critical memory allocation failed!\n");
	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	if (pThreadState != NULL && pThreadState->m_pSafetyPoolBuffer != NULL)
	{
		size_t nOldBufferSize = _msize(pThreadState->m_pSafetyPoolBuffer);
		if (nOldBufferSize <= nSize + MIN_MALLOC_OVERHEAD)
		{
			// give it all up
			TRACE0("Warning: Freeing application's memory safety pool!\n");
			free(pThreadState->m_pSafetyPoolBuffer);
			pThreadState->m_pSafetyPoolBuffer = NULL;
		}
		else
		{
			BOOL bEnable = AfxEnableMemoryTracking(FALSE);
			_expand(pThreadState->m_pSafetyPoolBuffer,
				nOldBufferSize - (nSize + MIN_MALLOC_OVERHEAD));
			AfxEnableMemoryTracking(bEnable);
			TRACE3("Warning: Shrinking safety pool from %d to %d to satisfy request of %d bytes.\n",
				 nOldBufferSize, _msize(pThreadState->m_pSafetyPoolBuffer), nSize);
		}
		return 1;       // retry it
	}

	TRACE0("ERROR: Critical memory allocation from safety pool failed!\n");
	AfxThrowMemoryException();      // oops
	return 0;
}
#endif // !_AFX_PORTABLE

/////////////////////////////////////////////////////////////////////////////
