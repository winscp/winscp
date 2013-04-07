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
#include "occimpl.h"

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifndef _AFX_NO_OCC_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// CWnd overridable for ambient properties

BOOL CWnd::OnAmbientProperty(COleControlSite* pSite, DISPID dispid,
	VARIANT* pvar)
{
	ASSERT(m_pCtrlCont != NULL);
	return m_pCtrlCont->GetAmbientProp(pSite, dispid, pvar);
}

/////////////////////////////////////////////////////////////////////////////
// CWnd access to underlying OLE control interface

LPUNKNOWN CWnd::GetControlUnknown()
{
	if (m_pCtrlSite == NULL)
		return NULL;

	return m_pCtrlSite->m_pObject;
}

/////////////////////////////////////////////////////////////////////////////
// CWnd functions with special cases for OLE Control containment

void CWnd::CheckDlgButton(int nIDButton, UINT nCheck)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		::CheckDlgButton(m_hWnd, nIDButton, nCheck);
	else
		m_pCtrlCont->CheckDlgButton(nIDButton, nCheck);
}

void CWnd::CheckRadioButton(int nIDFirstButton, int nIDLastButton,
	int nIDCheckButton)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		::CheckRadioButton(m_hWnd, nIDFirstButton, nIDLastButton,
			nIDCheckButton);
	else
		m_pCtrlCont->CheckRadioButton(nIDFirstButton, nIDLastButton,
			nIDCheckButton);
}

CWnd* CWnd::GetDlgItem(int nID) const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		return CWnd::FromHandle(::GetDlgItem(m_hWnd, nID));
	else
		return m_pCtrlCont->GetDlgItem(nID);
}

void CWnd::GetDlgItem(int nID, HWND* phWnd) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(phWnd != NULL);

	if (m_pCtrlCont == NULL)
		*phWnd = ::GetDlgItem(m_hWnd, nID);
	else
		m_pCtrlCont->GetDlgItem(nID, phWnd);
}

UINT CWnd::GetDlgItemInt(int nID, BOOL* lpTrans, BOOL bSigned) const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		return ::GetDlgItemInt(m_hWnd, nID, lpTrans, bSigned);
	else
		return m_pCtrlCont->GetDlgItemInt(nID, lpTrans, bSigned);
}

int CWnd::GetDlgItemText(int nID, LPTSTR lpStr, int nMaxCount) const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		return ::GetDlgItemText(m_hWnd, nID, lpStr, nMaxCount);
	else
		return m_pCtrlCont->GetDlgItemText(nID, lpStr, nMaxCount);
}

LRESULT CWnd::SendDlgItemMessage(int nID, UINT message, WPARAM wParam,
	LPARAM lParam)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		return ::SendDlgItemMessage(m_hWnd, nID, message, wParam, lParam);
	else
		return m_pCtrlCont->SendDlgItemMessage(nID, message, wParam, lParam);
}

void CWnd::SetDlgItemInt(int nID, UINT nValue, BOOL bSigned)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		::SetDlgItemInt(m_hWnd, nID, nValue, bSigned);
	else
		m_pCtrlCont->SetDlgItemInt(nID, nValue, bSigned);
}

void CWnd::SetDlgItemText(int nID, LPCTSTR lpszString)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		::SetDlgItemText(m_hWnd, nID, lpszString);
	else
		m_pCtrlCont->SetDlgItemText(nID, lpszString);
}

UINT CWnd::IsDlgButtonChecked(int nIDButton) const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlCont == NULL)
		return ::IsDlgButtonChecked(m_hWnd, nIDButton);
	else
		return m_pCtrlCont->IsDlgButtonChecked(nIDButton);
}

int CWnd::ScrollWindowEx(int dx, int dy, LPCRECT lpRectScroll,
	LPCRECT lpRectClip, CRgn* prgnUpdate, LPRECT lpRectUpdate,
	UINT flags)
{
	ASSERT(::IsWindow(m_hWnd));

	int iReturn = ::ScrollWindowEx(m_hWnd, dx, dy, lpRectScroll, lpRectClip,
			(HRGN)prgnUpdate->GetSafeHandle(), lpRectUpdate, flags);

	if ((m_pCtrlCont == NULL) || !(flags & SW_SCROLLCHILDREN))
		return iReturn;

	// the following code is for OLE control containers only

	m_pCtrlCont->ScrollChildren(dx, dy);
	return iReturn;
}

BOOL CWnd::IsDialogMessage(LPMSG lpMsg)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_nFlags & WF_OLECTLCONTAINER)
		return afxOccManager->IsDialogMessage(this, lpMsg);
	else
		return ::IsDialogMessage(m_hWnd, lpMsg);
}

/////////////////////////////////////////////////////////////////////////////
// CWnd functions with special cases for OLE Control wrappers

DWORD CWnd::GetStyle() const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return (DWORD)GetWindowLong(m_hWnd, GWL_STYLE);
	else
		return m_pCtrlSite->GetStyle();
}

DWORD CWnd::GetExStyle() const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return (DWORD)GetWindowLong(m_hWnd, GWL_EXSTYLE);
	else
		return m_pCtrlSite->GetExStyle();
}

BOOL CWnd::ModifyStyle(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ModifyStyle(m_hWnd, dwRemove, dwAdd, nFlags);
	else
		return m_pCtrlSite->ModifyStyle(dwRemove, dwAdd, nFlags);
}

BOOL CWnd::ModifyStyleEx(DWORD dwRemove, DWORD dwAdd, UINT nFlags)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ModifyStyleEx(m_hWnd, dwRemove, dwAdd, nFlags);
	else
		return m_pCtrlSite->ModifyStyleEx(dwRemove, dwAdd, nFlags);
}

void CWnd::SetWindowText(LPCTSTR lpszString)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		::SetWindowText(m_hWnd, lpszString);
	else
		m_pCtrlSite->SetWindowText(lpszString);
}

int CWnd::GetWindowText(LPTSTR lpszString, int nMaxCount) const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::GetWindowText(m_hWnd, lpszString, nMaxCount);
	else
		return m_pCtrlSite->GetWindowText(lpszString, nMaxCount);
}

int CWnd::GetWindowTextLength() const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::GetWindowTextLength(m_hWnd);
	else
		return m_pCtrlSite->GetWindowTextLength();
}

int CWnd::GetDlgCtrlID() const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::GetDlgCtrlID(m_hWnd);
	else
		return m_pCtrlSite->GetDlgCtrlID();
}

int CWnd::SetDlgCtrlID(int nID)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return (int)::SetWindowLong(m_hWnd, GWL_ID, nID);
	else
		return m_pCtrlSite->SetDlgCtrlID(nID);
}

void CWnd::MoveWindow(int x, int y, int nWidth, int nHeight, BOOL bRepaint)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		::MoveWindow(m_hWnd, x, y, nWidth, nHeight, bRepaint);
	else
		m_pCtrlSite->MoveWindow(x, y, nWidth, nHeight, bRepaint);
}

BOOL CWnd::SetWindowPos(const CWnd* pWndInsertAfter, int x, int y, int cx,
	int cy, UINT nFlags)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::SetWindowPos(m_hWnd, pWndInsertAfter->GetSafeHwnd(),
			x, y, cx, cy, nFlags);
	else
		return m_pCtrlSite->SetWindowPos(pWndInsertAfter, x, y, cx, cy, nFlags);
}

BOOL CWnd::ShowWindow(int nCmdShow)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::ShowWindow(m_hWnd, nCmdShow);
	else
		return m_pCtrlSite->ShowWindow(nCmdShow);
}

BOOL CWnd::IsWindowEnabled() const
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::IsWindowEnabled(m_hWnd);
	else
		return m_pCtrlSite->IsWindowEnabled();
}

BOOL CWnd::EnableWindow(BOOL bEnable)
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return ::EnableWindow(m_hWnd, bEnable);
	else
		return m_pCtrlSite->EnableWindow(bEnable);
}

CWnd* CWnd::SetFocus()
{
	ASSERT(::IsWindow(m_hWnd));

	if (m_pCtrlSite == NULL)
		return CWnd::FromHandle(::SetFocus(m_hWnd));
	else
		return m_pCtrlSite->SetFocus();
}

/////////////////////////////////////////////////////////////////////////////
// CWnd invoke helpers for OLE Control wrappers

void AFX_CDECL CWnd::InvokeHelper(DISPID dwDispID, WORD wFlags, VARTYPE vtRet,
	void* pvRet, const BYTE* pbParamInfo, ...)
{
	ASSERT(m_pCtrlSite != NULL);    // not an OLE control (not yet, at least)

	if (m_pCtrlSite == NULL)
		return;

	va_list argList;
	va_start(argList, pbParamInfo);
	m_pCtrlSite->InvokeHelperV(dwDispID, wFlags, vtRet, pvRet, pbParamInfo,
		argList);
	va_end(argList);
}

void CWnd::GetProperty(DISPID dwDispID, VARTYPE vtProp,
	void* pvProp) const
{
	ASSERT(m_pCtrlSite != NULL);    // not an OLE control (not yet, at least)

	if (m_pCtrlSite == NULL)
		return;

	const_cast<CWnd*>(this)->InvokeHelper(dwDispID, DISPATCH_PROPERTYGET,
		vtProp, pvProp, NULL);
}

void AFX_CDECL CWnd::SetProperty(DISPID dwDispID, VARTYPE vtProp, ...)
{
	ASSERT(m_pCtrlSite != NULL);    // not an OLE control (not yet, at least)

	if (m_pCtrlSite == NULL)
		return;

	va_list argList;    // really only one arg, but...
	va_start(argList, vtProp);
	m_pCtrlSite->SetPropertyV(dwDispID, vtProp, argList);
	va_end(argList);
}

IUnknown* CWnd::GetDSCCursor()
{
	ASSERT(m_pCtrlSite != NULL);    // not an OLE control (not yet, at least)
	if (m_pCtrlSite == NULL)
		return NULL;

	m_pCtrlSite->EnableDSC();

	IUnknown* pCursor = m_pCtrlSite->m_pDataSourceControl->GetCursor();
	ASSERT(pCursor != NULL);  // data source control has no cursor

	return pCursor;
}

void CWnd::BindDefaultProperty(DISPID dwDispID, VARTYPE vtProp, LPCTSTR szFieldName, CWnd* pDSCWnd)
{
	ASSERT(m_pCtrlSite != NULL); // not an OLE control (not yet, at least)
	m_pCtrlSite->BindDefaultProperty(dwDispID, vtProp, szFieldName, pDSCWnd);
}

void CWnd::BindProperty(DISPID dwDispId, CWnd* pWndDSC)
{
	ASSERT(m_pCtrlSite != NULL); // not an OLE control (not yet, at least)
	m_pCtrlSite->BindProperty(dwDispId, pWndDSC);
}

/////////////////////////////////////////////////////////////////////////////
// CWnd implementation helpers

void CWnd::AttachControlSite(CHandleMap* pMap)
{
	if (this != NULL && m_pCtrlSite == NULL)
	{
		// Determine if parent is an OLE control container
		CWnd* pWndParent = (CWnd*)pMap->LookupPermanent(::GetParent(m_hWnd));
		if (pWndParent != NULL && pWndParent->m_pCtrlCont != NULL)
		{
			// delegate through helper in COleControlSite
			pWndParent->m_pCtrlCont->AttachControlSite(this);
		}
	}
}

void CWnd::AttachControlSite(CWnd* pWndParent)
{
	ASSERT(this != NULL);
	ASSERT(pWndParent != NULL);

	if (m_pCtrlSite == NULL && pWndParent->m_pCtrlCont != NULL)
	{
		// delegate through helper in COleControlSite
		pWndParent->m_pCtrlCont->AttachControlSite(this);
	}
}

void COleControlContainer::AttachControlSite(CWnd* pWnd)
{
	ASSERT(this != NULL);
	ASSERT(pWnd != NULL);

	// If a matching control site exists, it's an OLE control
	COleControlSite* pSite = (COleControlSite*)m_siteMap.GetValueAt(pWnd->m_hWnd);
	if (pSite != NULL)
	{
		// detach any existing CWnd from this site (last one wins)
		CWnd* pOldCtrl = pSite->m_pWndCtrl;
		if (pOldCtrl != NULL && pOldCtrl->m_pCtrlSite == pSite)
			pOldCtrl->m_pCtrlSite = NULL;

		// now wire the site and CWnd together
		pWnd->m_pCtrlSite = pSite;
		pSite->m_pWndCtrl = pWnd;
	}
}

#endif // !_AFX_NO_OCC_SUPPORT
