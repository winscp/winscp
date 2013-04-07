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

/////////////////////////////////////////////////////////////////////////////
// CToolTipCtrl

BEGIN_MESSAGE_MAP(CToolTipCtrl, CWnd)
	//{{AFX_MSG_MAP(CToolTipCtrl)
	ON_MESSAGE(WM_DISABLEMODAL, OnDisableModal)
	ON_MESSAGE(TTM_WINDOWFROMPOINT, OnWindowFromPoint)
	ON_MESSAGE(TTM_ADDTOOL, OnAddTool)
	ON_WM_ENABLE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CToolTipCtrl::CToolTipCtrl()
{
}

BOOL CToolTipCtrl::Create(CWnd* pParentWnd, DWORD dwStyle)
{
	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTL_BAR_REG));

	BOOL bResult = CWnd::CreateEx(NULL, TOOLTIPS_CLASS, NULL,
		WS_POPUP | dwStyle, // force WS_POPUP
		CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT,
		pParentWnd->GetSafeHwnd(), NULL, NULL);

	if (bResult)
		SetOwner(pParentWnd);
	return bResult;
}

CToolTipCtrl::~CToolTipCtrl()
{
	DestroyWindow();
}

BOOL CToolTipCtrl::DestroyToolTipCtrl()
{
#ifdef _AFXDLL
	BOOL bDestroy = (AfxGetModuleState() == m_pModuleState);
#else
	BOOL bDestroy = TRUE;
#endif

	if (bDestroy)
	{
		DestroyWindow();
		delete this;
	}
	return bDestroy;
}

LRESULT CToolTipCtrl::OnAddTool(WPARAM wParam, LPARAM lParam)
{
	TOOLINFO ti = *(LPTOOLINFO)lParam;
	if ((ti.hinst == NULL) && (ti.lpszText != LPSTR_TEXTCALLBACK)
		&& (ti.lpszText != NULL))
	{
		void* pv;
		if (!m_mapString.Lookup(ti.lpszText, pv))
			m_mapString.SetAt(ti.lpszText, NULL);
		// set lpszText to point to the permanent memory associated
		// with the CString
		VERIFY(m_mapString.LookupKey(ti.lpszText, ti.lpszText));
	}
	return DefWindowProc(TTM_ADDTOOL, wParam, (LPARAM)&ti);
}

LRESULT CToolTipCtrl::OnDisableModal(WPARAM, LPARAM)
{
	SendMessage(TTM_ACTIVATE, FALSE);
	return FALSE;
}

void CToolTipCtrl::OnEnable(BOOL bEnable)
{
	SendMessage(TTM_ACTIVATE, bEnable);
}

LRESULT CToolTipCtrl::OnWindowFromPoint(WPARAM, LPARAM lParam)
{
	ASSERT(lParam != NULL);

	// the default implementation of tooltips just calls WindowFromPoint
	// which does not work for certain kinds of combo boxes
	CPoint pt = *(POINT*)lParam;
	HWND hWnd = ::WindowFromPoint(pt);
	if (hWnd == NULL)
		return 0;

	// try to hit combobox instead of edit control for CBS_DROPDOWN styles
	HWND hWndTemp = ::GetParent(hWnd);
	if (hWndTemp != NULL && _AfxIsComboBoxControl(hWndTemp, CBS_DROPDOWN))
		return (LRESULT)hWndTemp;

	// handle special case of disabled child windows
	::ScreenToClient(hWnd, &pt);
	hWndTemp = _AfxChildWindowFromPoint(hWnd, pt);
	if (hWndTemp != NULL && !::IsWindowEnabled(hWndTemp))
		return (LRESULT)hWndTemp;

	return (LRESULT)hWnd;
}

BOOL CToolTipCtrl::AddTool(CWnd* pWnd, LPCTSTR lpszText, LPCRECT lpRectTool,
	UINT nIDTool)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);
	ASSERT(lpszText != NULL);
	// the toolrect and toolid must both be zero or both valid
	ASSERT((lpRectTool != NULL && nIDTool != 0) ||
		   (lpRectTool == NULL) && (nIDTool == 0));

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	if (lpRectTool != NULL)
		memcpy(&ti.rect, lpRectTool, sizeof(RECT));
	ti.lpszText = (LPTSTR)lpszText;
	return (BOOL) ::SendMessage(m_hWnd, TTM_ADDTOOL, 0, (LPARAM)&ti);
}

BOOL CToolTipCtrl::AddTool(CWnd* pWnd, UINT nIDText, LPCRECT lpRectTool,
	UINT nIDTool)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(nIDText != 0);
	ASSERT(pWnd != NULL);
	// the toolrect and toolid must both be zero or both valid
	ASSERT((lpRectTool != NULL && nIDTool != 0) ||
		   (lpRectTool == NULL) && (nIDTool == 0));

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	if (lpRectTool != NULL)
		memcpy(&ti.rect, lpRectTool, sizeof(RECT));
	ti.hinst = AfxFindResourceHandle(MAKEINTRESOURCE((nIDText>>4)+1),
		RT_STRING);
	ASSERT(ti.hinst != NULL);
	ti.lpszText = (LPTSTR)MAKEINTRESOURCE(nIDText);
	return (BOOL) ::SendMessage(m_hWnd, TTM_ADDTOOL, 0, (LPARAM)&ti);
}

void CToolTipCtrl::DelTool(CWnd* pWnd, UINT nIDTool)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	::SendMessage(m_hWnd, TTM_DELTOOL, 0, (LPARAM)&ti);
}

void CToolTipCtrl::GetText(CString& str, CWnd* pWnd, UINT nIDTool) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	ti.lpszText = str.GetBuffer(256);
	::SendMessage(m_hWnd, TTM_GETTEXT, 0, (LPARAM)&ti);
	str.ReleaseBuffer();
}

BOOL CToolTipCtrl::GetToolInfo(CToolInfo& ToolInfo, CWnd* pWnd,
	UINT nIDTool) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);

	FillInToolInfo(ToolInfo, pWnd, nIDTool);
	ToolInfo.lpszText = ToolInfo.szText;
	return (BOOL)::SendMessage(m_hWnd, TTM_GETTOOLINFO, 0, (LPARAM)&ToolInfo);
}

BOOL CToolTipCtrl::HitTest(CWnd* pWnd, CPoint pt, LPTOOLINFO lpToolInfo) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);
	ASSERT(lpToolInfo != NULL);

	TTHITTESTINFO hti;
	memset(&hti, 0, sizeof(hti));
	hti.ti.cbSize = sizeof(AFX_OLDTOOLINFO);
	hti.hwnd = pWnd->GetSafeHwnd();
	hti.pt.x = pt.x;
	hti.pt.y = pt.y;
	if ((BOOL)::SendMessage(m_hWnd, TTM_HITTEST, 0, (LPARAM)&hti))
	{
		memcpy(lpToolInfo, &hti.ti, sizeof(AFX_OLDTOOLINFO));
		return TRUE;
	}
	return FALSE;
}

void CToolTipCtrl::SetToolRect(CWnd* pWnd, UINT nIDTool, LPCRECT lpRect)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);
	ASSERT(nIDTool != 0);

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	memcpy(&ti.rect, lpRect, sizeof(RECT));
	::SendMessage(m_hWnd, TTM_NEWTOOLRECT, 0, (LPARAM)&ti);
}

void CToolTipCtrl::UpdateTipText(LPCTSTR lpszText, CWnd* pWnd, UINT nIDTool)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pWnd != NULL);

	TOOLINFO ti;
	FillInToolInfo(ti, pWnd, nIDTool);
	ti.lpszText = (LPTSTR)lpszText;
	::SendMessage(m_hWnd, TTM_UPDATETIPTEXT, 0, (LPARAM)&ti);
}

void CToolTipCtrl::UpdateTipText(UINT nIDText, CWnd* pWnd, UINT nIDTool)
{
	ASSERT(nIDText != 0);

	CString str;
	VERIFY(str.LoadString(nIDText));
	UpdateTipText(str, pWnd, nIDTool);
}

/////////////////////////////////////////////////////////////////////////////
// CToolTipCtrl Implementation

void CToolTipCtrl::FillInToolInfo(TOOLINFO& ti, CWnd* pWnd, UINT nIDTool) const
{
	memset(&ti, 0, sizeof(AFX_OLDTOOLINFO));
	ti.cbSize = sizeof(AFX_OLDTOOLINFO);
	HWND hwnd = pWnd->GetSafeHwnd();
	if (nIDTool == 0)
	{
		ti.hwnd = ::GetParent(hwnd);
		ti.uFlags = TTF_IDISHWND;
		ti.uId = (UINT)hwnd;
	}
	else
	{
		ti.hwnd = hwnd;
		ti.uFlags = 0;
		ti.uId = nIDTool;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CWnd tooltip support

BOOL CWnd::_EnableToolTips(BOOL bEnable, UINT nFlag)
{
	ASSERT(nFlag == WF_TOOLTIPS || nFlag == WF_TRACKINGTOOLTIPS);

	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	CToolTipCtrl* pToolTip = pThreadState->m_pToolTip;

	if (!bEnable)
	{
		// nothing to do if tooltips not enabled
		if (!(m_nFlags & nFlag))
			return TRUE;

		// cancel tooltip if this window is active
		if (pThreadState->m_pLastHit == this)
			CancelToolTips(TRUE);

		// remove "dead-area" toolbar
		if (pToolTip->GetSafeHwnd() != NULL)
		{
			TOOLINFO ti; memset(&ti, 0, sizeof(TOOLINFO));
			ti.cbSize = sizeof(AFX_OLDTOOLINFO);
			ti.uFlags = TTF_IDISHWND;
			ti.hwnd = m_hWnd;
			ti.uId = (UINT)m_hWnd;
			pToolTip->SendMessage(TTM_DELTOOL, 0, (LPARAM)&ti);
		}

		// success
		m_nFlags &= ~nFlag;
		return TRUE;
	}

	// if already enabled for tooltips, nothing to do
	if (!(m_nFlags & nFlag))
	{
		// success
		AFX_MODULE_STATE* pModuleState = _AFX_CMDTARGET_GETSTATE();
		pModuleState->m_pfnFilterToolTipMessage = &CWnd::_FilterToolTipMessage;
		m_nFlags |= nFlag;
	}
	return TRUE;
}

BOOL CWnd::EnableToolTips(BOOL bEnable)
{
	return _EnableToolTips(bEnable, WF_TOOLTIPS);
}

BOOL CWnd::EnableTrackingToolTips(BOOL bEnable)
{
	return _EnableToolTips(bEnable, WF_TRACKINGTOOLTIPS);
}

AFX_STATIC void AFXAPI _AfxRelayToolTipMessage(CToolTipCtrl* pToolTip, MSG* pMsg)
{
	// transate the message based on TTM_WINDOWFROMPOINT
	MSG msg = *pMsg;
	msg.hwnd = (HWND)pToolTip->SendMessage(TTM_WINDOWFROMPOINT, 0, (LPARAM)&msg.pt);
	CPoint pt = pMsg->pt;
	if (msg.message >= WM_MOUSEFIRST && msg.message <= WM_MOUSELAST)
		::ScreenToClient(msg.hwnd, &pt);
	msg.lParam = MAKELONG(pt.x, pt.y);

	// relay mouse event before deleting old tool
	pToolTip->SendMessage(TTM_RELAYEVENT, 0, (LPARAM)&msg);
}

void PASCAL CWnd::_FilterToolTipMessage(MSG* pMsg, CWnd* pWnd)
{
	pWnd->FilterToolTipMessage(pMsg);
}

void CWnd::FilterToolTipMessage(MSG* pMsg)
{
	// this CWnd has tooltips enabled
	UINT message = pMsg->message;
	if ((message == WM_MOUSEMOVE || message == WM_NCMOUSEMOVE ||
		 message == WM_LBUTTONUP || message == WM_RBUTTONUP ||
		 message == WM_MBUTTONUP) &&
		(GetKeyState(VK_LBUTTON) >= 0 && GetKeyState(VK_RBUTTON) >= 0 &&
		 GetKeyState(VK_MBUTTON) >= 0))
	{
		// make sure that tooltips are not already being handled
		CWnd* pWnd = CWnd::FromHandle(pMsg->hwnd);
		while (pWnd != NULL && !(pWnd->m_nFlags & (WF_TOOLTIPS|WF_TRACKINGTOOLTIPS)))
		{
			pWnd = pWnd->GetParent();
		}
		if (pWnd != this)
		{
			if (pWnd == NULL)
			{
				// tooltips not enabled on this CWnd, clear last state data
				_AFX_THREAD_STATE* pThreadState = _afxThreadState.GetData();
				pThreadState->m_pLastHit = NULL;
				pThreadState->m_nLastHit = -1;
			}
			return;
		}

		_AFX_THREAD_STATE* pThreadState = _afxThreadState.GetData();
		CToolTipCtrl* pToolTip = pThreadState->m_pToolTip;
		CWnd* pOwner = GetParentOwner();
		if (pToolTip != NULL && pToolTip->GetOwner() != pOwner)
		{
			pToolTip->DestroyWindow();
			delete pToolTip;
			pThreadState->m_pToolTip = NULL;
			pToolTip = NULL;
		}
		if (pToolTip == NULL)
		{
			pToolTip = new CToolTipCtrl;
			if (!pToolTip->Create(pOwner, TTS_ALWAYSTIP))
			{
				delete pToolTip;
				return;
			}
			pToolTip->SendMessage(TTM_ACTIVATE, FALSE);
			pThreadState->m_pToolTip = pToolTip;
		}

		ASSERT_VALID(pToolTip);
		ASSERT(::IsWindow(pToolTip->m_hWnd));

		// add a "dead-area" tool for areas between toolbar buttons
		TOOLINFO ti; memset(&ti, 0, sizeof(TOOLINFO));
		ti.cbSize = sizeof(AFX_OLDTOOLINFO);
		ti.uFlags = TTF_IDISHWND;
		ti.hwnd = m_hWnd;
		ti.uId = (UINT)m_hWnd;
		if (!pToolTip->SendMessage(TTM_GETTOOLINFO, 0, (LPARAM)&ti))
		{
			ASSERT(ti.uFlags == TTF_IDISHWND);
			ASSERT(ti.hwnd == m_hWnd);
			ASSERT(ti.uId == (UINT)m_hWnd);
			VERIFY(pToolTip->SendMessage(TTM_ADDTOOL, 0, (LPARAM)&ti));
		}

		// determine which tool was hit
		CPoint point = pMsg->pt;
		::ScreenToClient(m_hWnd, &point);
		TOOLINFO tiHit; memset(&tiHit, 0, sizeof(TOOLINFO));
		tiHit.cbSize = sizeof(AFX_OLDTOOLINFO);
		int nHit = OnToolHitTest(point, &tiHit);

		// build new toolinfo and if different than current, register it
		CWnd* pHitWnd = nHit == -1 ? NULL : this;
		if (pThreadState->m_nLastHit != nHit || pThreadState->m_pLastHit != pHitWnd)
		{
			if (nHit != -1)
			{
				// add new tool and activate the tip
				ti = tiHit;
				ti.uFlags &= ~(TTF_NOTBUTTON|TTF_ALWAYSTIP);
				if (m_nFlags & WF_TRACKINGTOOLTIPS)
					ti.uFlags |= TTF_TRACK;
				VERIFY(pToolTip->SendMessage(TTM_ADDTOOL, 0, (LPARAM)&ti));
				if ((tiHit.uFlags & TTF_ALWAYSTIP) || IsTopParentActive())
				{
					// allow the tooltip to popup when it should
					pToolTip->SendMessage(TTM_ACTIVATE, TRUE);
					if (m_nFlags & WF_TRACKINGTOOLTIPS)
						pToolTip->SendMessage(TTM_TRACKACTIVATE, TRUE, (LPARAM)&ti);

					// bring the tooltip window above other popup windows
					::SetWindowPos(pToolTip->m_hWnd, HWND_TOP, 0, 0, 0, 0,
						SWP_NOACTIVATE|SWP_NOSIZE|SWP_NOMOVE|SWP_NOOWNERZORDER);
				}
			}
			else
			{
				pToolTip->SendMessage(TTM_ACTIVATE, FALSE);
			}

			// relay mouse event before deleting old tool
			_AfxRelayToolTipMessage(pToolTip, pMsg);

			// now safe to delete the old tool
			if (pThreadState->m_lastInfo.cbSize >= sizeof(AFX_OLDTOOLINFO))
				pToolTip->SendMessage(TTM_DELTOOL, 0, (LPARAM)&pThreadState->m_lastInfo);

			pThreadState->m_pLastHit = pHitWnd;
			pThreadState->m_nLastHit = nHit;
			pThreadState->m_lastInfo = tiHit;
		}
		else
		{
			if (m_nFlags & WF_TRACKINGTOOLTIPS)
			{
				POINT pt;

				::GetCursorPos( &pt );
				pToolTip->SendMessage(TTM_TRACKPOSITION, 0, MAKELPARAM(pt.x, pt.y));
			}
			else
			{
				// relay mouse events through the tooltip
				if (nHit != -1)
					_AfxRelayToolTipMessage(pToolTip, pMsg);
			}
		}

		if ((tiHit.lpszText != LPSTR_TEXTCALLBACK) && (tiHit.hinst == 0))
			free(tiHit.lpszText);
	}
	else if (m_nFlags & (WF_TOOLTIPS|WF_TRACKINGTOOLTIPS))
	{
		// make sure that tooltips are not already being handled
		CWnd* pWnd = CWnd::FromHandle(pMsg->hwnd);
		while (pWnd != NULL && pWnd != this && !(pWnd->m_nFlags & (WF_TOOLTIPS|WF_TRACKINGTOOLTIPS)))
			pWnd = pWnd->GetParent();
		if (pWnd != this)
			return;

		BOOL bKeys = (message >= WM_KEYFIRST && message <= WM_KEYLAST) ||
			(message >= WM_SYSKEYFIRST && message <= WM_SYSKEYLAST);
		if ((m_nFlags & WF_TRACKINGTOOLTIPS) == 0 &&
			(bKeys ||
			 (message == WM_LBUTTONDOWN || message == WM_LBUTTONDBLCLK) ||
			 (message == WM_RBUTTONDOWN || message == WM_RBUTTONDBLCLK) ||
			 (message == WM_MBUTTONDOWN || message == WM_MBUTTONDBLCLK) ||
			 (message == WM_NCLBUTTONDOWN || message == WM_NCLBUTTONDBLCLK) ||
			 (message == WM_NCRBUTTONDOWN || message == WM_NCRBUTTONDBLCLK) ||
			 (message == WM_NCMBUTTONDOWN || message == WM_NCMBUTTONDBLCLK)))
		{
			CWnd::CancelToolTips(bKeys);
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CToolTipCtrl, CWnd)

/////////////////////////////////////////////////////////////////////////////
