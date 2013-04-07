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

#ifdef AFX_CORE5_SEG
#pragma code_seg(AFX_CORE5_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CDateTimeCtrl

BOOL CDateTimeCtrl::Create(DWORD dwStyle, const RECT& rect,
	CWnd* pParentWnd, UINT nID)
{
   // initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTL_DATE_REG));

	CWnd* pWnd = this;
	return pWnd->Create(DATETIMEPICK_CLASS, NULL, dwStyle, rect, pParentWnd, nID);
}

DWORD CDateTimeCtrl::GetRange(CTime* pMinTime, CTime* pMaxTime) const
{
	ASSERT(::IsWindow(m_hWnd));
	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));

	DWORD dwResult = ::SendMessage(m_hWnd, DTM_GETRANGE, 0, (LPARAM) &sysTimes);

	if (pMinTime != NULL)
	{
		if (dwResult & GDTR_MIN)
			*pMinTime = CTime(sysTimes[0]);
	}

	if (pMaxTime != NULL)
	{
		if (dwResult & GDTR_MAX)
			*pMaxTime = CTime(sysTimes[1]);
	}

	return dwResult;

}

DWORD CDateTimeCtrl::GetRange(COleDateTime* pMinTime,
	COleDateTime* pMaxTime) const
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));

	DWORD dwResult = ::SendMessage(m_hWnd, DTM_GETRANGE, 0, (LPARAM) &sysTimes);
	if (pMinTime != NULL)
	{
		if (dwResult & GDTR_MIN)
			*pMinTime = COleDateTime(sysTimes[0]);
		else
			pMinTime->SetStatus(COleDateTime::null);
	}

	if (pMaxTime != NULL)
	{
		if (dwResult & GDTR_MAX)
			*pMaxTime = COleDateTime(sysTimes[1]);
		else
			pMaxTime->SetStatus(COleDateTime::null);
	}

	return dwResult;
}

BOOL CDateTimeCtrl::SetRange(const CTime* pMinTime, const CTime* pMaxTime)
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];

	WPARAM wFlags = 0;
	if (pMinTime != NULL && pMinTime->GetAsSystemTime(sysTimes[0]))
		wFlags |= GDTR_MIN;

	if (pMaxTime != NULL && pMaxTime->GetAsSystemTime(sysTimes[1]))
		wFlags |= GDTR_MAX;

	return (BOOL) ::SendMessage(m_hWnd, DTM_SETRANGE, wFlags, (LPARAM) &sysTimes);
}

BOOL CDateTimeCtrl::SetRange(const COleDateTime* pMinTime, const COleDateTime* pMaxTime)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pMinTime == NULL || pMinTime->GetStatus() != COleDateTime::invalid);
	ASSERT(pMaxTime == NULL || pMaxTime->GetStatus() != COleDateTime::invalid);

	SYSTEMTIME sysTime[2];

	WPARAM wFlags = 0;
	if (pMinTime != NULL && pMinTime->GetStatus() != COleDateTime::null)
	{
		if (pMinTime->GetAsSystemTime(sysTime[0]))
			wFlags |= GDTR_MIN;
	}

	if (pMaxTime != NULL && pMaxTime->GetStatus() != COleDateTime::null)
	{
		if (pMaxTime->GetAsSystemTime(sysTime[1]))
			wFlags |= GDTR_MAX;
	}

	return (BOOL) ::SendMessage(m_hWnd, DTM_SETRANGE, wFlags, (LPARAM) &sysTime);
}

BOOL CDateTimeCtrl::SetTime(LPSYSTEMTIME pTimeNew /* = NULL */)
{
	ASSERT(::IsWindow(m_hWnd));
	WPARAM wParam = (pTimeNew == NULL) ? GDT_NONE : GDT_VALID;
	return (BOOL) ::SendMessage(m_hWnd, DTM_SETSYSTEMTIME,
		wParam, (LPARAM) pTimeNew);
}

BOOL CDateTimeCtrl::SetTime(const COleDateTime& timeNew)
{
	BOOL bRetVal = FALSE;

	// make sure the time isn't invalid
	ASSERT(timeNew.GetStatus() != COleDateTime::invalid);
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTime;
	WPARAM wParam = GDT_NONE;
	if (timeNew.GetStatus() == COleDateTime::valid &&
		timeNew.GetAsSystemTime(sysTime))
	{
		wParam = GDT_VALID;
	}

	bRetVal = (BOOL) ::SendMessage(m_hWnd,
			DTM_SETSYSTEMTIME, wParam, (LPARAM) &sysTime);

	return bRetVal;
}

BOOL CDateTimeCtrl::SetTime(const CTime* pTimeNew)
{
	BOOL bRetVal = FALSE;

	// make sure the time isn't invalid
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTime;
	WPARAM wParam = GDT_NONE;
	if (pTimeNew != NULL && pTimeNew->GetAsSystemTime(sysTime))
	{
		wParam = GDT_VALID;
	}

	bRetVal = (BOOL) ::SendMessage(m_hWnd,
			DTM_SETSYSTEMTIME, wParam, (LPARAM) &sysTime);

	return bRetVal;
}

BOOL CDateTimeCtrl::GetTime(COleDateTime& timeDest) const
{
	SYSTEMTIME sysTime;
	BOOL bRetVal = TRUE;

	LRESULT result = ::SendMessage(m_hWnd, DTM_GETSYSTEMTIME, 0, (LPARAM) &sysTime);
	if (result == GDT_VALID)
	{
		timeDest = COleDateTime(sysTime);
		bRetVal = TRUE;
		ASSERT(timeDest.GetStatus() == COleDateTime::valid);
	}
	else if (result == GDT_NONE)
	{
		timeDest.SetStatus(COleDateTime::null);
		bRetVal = TRUE;
	}
	else
		timeDest.SetStatus(COleDateTime::invalid);
	return bRetVal;
}

DWORD CDateTimeCtrl::GetTime(CTime& timeDest) const
{
	SYSTEMTIME sysTime;
	DWORD dwResult = (DWORD)
		::SendMessage(m_hWnd, DTM_GETSYSTEMTIME, 0, (LPARAM) &sysTime);

	if (dwResult == GDT_VALID)
		timeDest = CTime(sysTime);

	return dwResult;
}

CDateTimeCtrl::~CDateTimeCtrl()
{
	DestroyWindow();
}

/////////////////////////////////////////////////////////////////////////////
// CMonthCalCtrl

BOOL CMonthCalCtrl::Create(DWORD dwStyle, const RECT& rect,
	CWnd* pParentWnd, UINT nID)
{
	// initialize common controls
	VERIFY(AfxDeferRegisterClass(AFX_WNDCOMMCTL_DATE_REG));

	CWnd* pWnd = this;
	return pWnd->Create(MONTHCAL_CLASS, NULL, dwStyle, rect, pParentWnd, nID);
}

BOOL CMonthCalCtrl::Create(DWORD dwStyle, const POINT& pt,
	CWnd* pParentWnd, UINT nID)
{
	BOOL bWasVisible = (dwStyle & WS_VISIBLE);
	dwStyle &= ~WS_VISIBLE;

	CRect rect(pt.x, pt.y, 0, 0);

	BOOL bRetVal = FALSE;
   if (Create(dwStyle, rect, pParentWnd, nID))
	{
		if (SizeMinReq())
		{
			if (bWasVisible)
				ShowWindow(SW_SHOWNA);
			bRetVal = TRUE;
		}
		else
			DestroyWindow();
	}

	return bRetVal;
}

BOOL CMonthCalCtrl::SizeMinReq(BOOL bRepaint /* = TRUE */)
{
	CRect rect;
	BOOL bRetVal = FALSE;
	if (GetMinReqRect(rect))
	{
		DWORD dwFlags = SWP_NOZORDER | SWP_NOREPOSITION | SWP_NOMOVE | SWP_NOACTIVATE;
		if (!bRepaint)
			dwFlags |= SWP_NOREDRAW;
		SetWindowPos(NULL, 0, 0, rect.Width(), rect.Height(), dwFlags);
		bRetVal = TRUE;
	}

	return bRetVal;
}

void CMonthCalCtrl::SetToday(const COleDateTime& refTime)
{
	ASSERT_VALID(this);

	// make sure the time isn't invalid
	ASSERT(refTime.GetStatus() != COleDateTime::invalid);
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTime;
	LPSYSTEMTIME pSysTime = NULL;
	WPARAM wParam = GDT_NONE;

	// if the passed time is null or out of range,
	// we'll set the control to NULL

	if (refTime.GetAsSystemTime(sysTime))
	{
		pSysTime = &sysTime;
		wParam = GDT_VALID;
	}

	if (::IsWindow(m_hWnd))
		::SendMessage(m_hWnd, MCM_SETTODAY, wParam, (LPARAM) pSysTime);
}

void CMonthCalCtrl::SetToday(const CTime* pDateTime)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT_VALID(this);

	// if the passed time is NULL, we'll set the
	// control to NULL

	WPARAM wParam = GDT_NONE;
	LPSYSTEMTIME pSysTime = NULL;
	SYSTEMTIME sysTime;

	if (pDateTime != NULL && pDateTime->GetAsSystemTime(sysTime))
	{
		wParam = GDT_VALID;
		pSysTime = &sysTime;
	}

	if (::IsWindow(m_hWnd))
		::SendMessage(m_hWnd, MCM_SETTODAY, wParam, (LPARAM) pSysTime);
}

BOOL CMonthCalCtrl::SetCurSel(const COleDateTime& refTime)
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTime;
	BOOL bRetVal = FALSE;

	// if the passed time is null or out of range,
	// we'll set the control to NULL

	if (refTime.GetAsSystemTime(sysTime) &&
		refTime.GetStatus() == COleDateTime::valid)
	{
		bRetVal = (BOOL)
			::SendMessage(m_hWnd, MCM_SETCURSEL, 0, (LPARAM) &sysTime);
	}

	return bRetVal;
}

BOOL CMonthCalCtrl::SetCurSel(const CTime& refTime)
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTime;
	BOOL bRetVal = FALSE;

	if (refTime.GetAsSystemTime(sysTime))
	{
		bRetVal = (BOOL)
			::SendMessage(m_hWnd, MCM_SETCURSEL, 0, (LPARAM) &sysTime);
	}

	return bRetVal;
}

BOOL CMonthCalCtrl::GetCurSel(COleDateTime& refTime) const
{
	ASSERT(::IsWindow(m_hWnd));

	// can't use this method on multiple selection controls
	ASSERT(!(GetStyle() & MCS_MULTISELECT));

	SYSTEMTIME sysTime;
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETCURSEL, 0, (LPARAM) &sysTime);

	if (bResult)
		refTime = COleDateTime(sysTime);

	return bResult;
}

BOOL CMonthCalCtrl::GetToday(COleDateTime& refTime) const
{
	ASSERT(::IsWindow(m_hWnd));

	// can't use this method on multiple selection controls
	ASSERT(!(GetStyle() & MCS_MULTISELECT));

	SYSTEMTIME sysTime;
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETTODAY, 0, (LPARAM) &sysTime);

	if (bResult)
		refTime = COleDateTime(sysTime);

	return bResult;
}

BOOL CMonthCalCtrl::GetCurSel(CTime& refTime) const
{
	ASSERT(::IsWindow(m_hWnd));

	// can't use this method on multiple selection controls
	ASSERT(!(GetStyle() & MCS_MULTISELECT));

	SYSTEMTIME sysTime;
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETCURSEL, 0, (LPARAM) &sysTime);

	if (bResult)
		refTime = CTime(sysTime);

	return bResult;
}

BOOL CMonthCalCtrl::GetToday(CTime& refTime) const
{
	ASSERT(::IsWindow(m_hWnd));

	// can't use this method on multiple selection controls
	ASSERT(!(GetStyle() & MCS_MULTISELECT));

	SYSTEMTIME sysTime;
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETTODAY, 0, (LPARAM) &sysTime);

	if (bResult)
		refTime = CTime(sysTime);

	return bResult;
}

CMonthCalCtrl::~CMonthCalCtrl()
{
	DestroyWindow();
}

int CMonthCalCtrl::GetFirstDayOfWeek(BOOL* pbLocal /* = NULL */) const
{
	ASSERT(::IsWindow(m_hWnd));
	DWORD dwResult;
	dwResult = (DWORD) ::SendMessage(m_hWnd, MCM_GETFIRSTDAYOFWEEK, 0, 0);

	// set *pbLocal to reflect if the first day of week
	// matches current locale setting

	if (pbLocal)
		*pbLocal = HIWORD(dwResult);
	return LOWORD(dwResult);
}

BOOL CMonthCalCtrl::SetFirstDayOfWeek(int iDay, int* lpnOld /* = NULL */)
{
	ASSERT(::IsWindow(m_hWnd));
	DWORD dwResult;
	dwResult = (DWORD) ::SendMessage(m_hWnd, MCM_SETFIRSTDAYOFWEEK, 0, (WPARAM) iDay);

	if (lpnOld != NULL)
		*lpnOld = LOWORD(dwResult);

	return (BOOL) HIWORD(dwResult);
}

BOOL CMonthCalCtrl::SetDayState(int nMonths, LPMONTHDAYSTATE pStates)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(AfxIsValidAddress(pStates, nMonths * sizeof(MONTHDAYSTATE), FALSE));
	return (BOOL) ::SendMessage(m_hWnd, MCM_SETDAYSTATE, (WPARAM) nMonths, (LPARAM) pStates);
}

BOOL CMonthCalCtrl::SetRange(const COleDateTime* pMinRange,
	const COleDateTime* pMaxRange)
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(pMinRange == NULL || pMinRange->GetStatus() != COleDateTime::invalid);
	ASSERT(pMaxRange == NULL || pMaxRange->GetStatus() != COleDateTime::invalid);

	SYSTEMTIME sysTimes[2];
	WPARAM wFlags = 0;

	if (pMinRange != NULL && pMinRange->GetStatus() != COleDateTime::null)
	{
		if (pMinRange->GetAsSystemTime(sysTimes[0]))
			wFlags |= GDTR_MIN;
	}

	if (pMaxRange != NULL && pMaxRange->GetStatus() != COleDateTime::null)
	{
		if (pMaxRange->GetAsSystemTime(sysTimes[1]))
			wFlags |= GDTR_MAX;
	}

	return (BOOL)
		::SendMessage(m_hWnd, MCM_SETRANGE, wFlags, (LPARAM) &sysTimes);
}

BOOL CMonthCalCtrl::SetRange(const LPSYSTEMTIME pMinRange,
	const LPSYSTEMTIME pMaxRange)
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	WPARAM wFlags = 0;

	if (pMinRange != NULL)
	{
		memcpy(&sysTimes[0], pMinRange, sizeof(SYSTEMTIME));
		wFlags |= GDTR_MIN;
	}

	if (pMaxRange != NULL)
	{
		memcpy(&sysTimes[1], pMaxRange, sizeof(SYSTEMTIME));
		wFlags |= GDTR_MAX;
	}

	return (BOOL)
		::SendMessage(m_hWnd, MCM_SETRANGE, wFlags, (LPARAM) &sysTimes);
}

DWORD CMonthCalCtrl::GetRange(COleDateTime* pMinRange,
	COleDateTime* pMaxRange) const
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));

	DWORD dwRanges = (DWORD)
		::SendMessage(m_hWnd, MCM_GETRANGE, 0, (LPARAM) &sysTimes);

	if (dwRanges & GDTR_MIN && pMinRange)
		*pMinRange = COleDateTime(sysTimes[0]);

	if (dwRanges & GDTR_MAX && pMaxRange)
		*pMaxRange = COleDateTime(sysTimes[1]);

	return dwRanges;
}

DWORD CMonthCalCtrl::GetRange(LPSYSTEMTIME pMinRange,
	LPSYSTEMTIME pMaxRange) const
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));

	DWORD dwRanges = (DWORD)
		::SendMessage(m_hWnd, MCM_GETRANGE, 0, (LPARAM) &sysTimes);

	if (dwRanges & GDTR_MIN && pMinRange)
		memcpy(pMinRange, &sysTimes[0], sizeof(SYSTEMTIME));

	if (dwRanges & GDTR_MAX && pMaxRange)
		memcpy(pMaxRange, &sysTimes[1], sizeof(SYSTEMTIME));

	return dwRanges;
}

BOOL CMonthCalCtrl::SetRange(const CTime* pMinRange, const CTime* pMaxRange)
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	WPARAM wFlags = 0;
	if (pMinRange != NULL && pMinRange->GetAsSystemTime(sysTimes[0]))
		wFlags |= GDTR_MIN;

	if (pMaxRange != NULL && pMaxRange->GetAsSystemTime(sysTimes[1]))
		wFlags |= GDTR_MAX;

	return (BOOL)
		::SendMessage(m_hWnd, MCM_SETRANGE, wFlags, (LPARAM) &sysTimes);
}

DWORD CMonthCalCtrl::GetRange(CTime* pMinRange, CTime* pMaxRange) const
{
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));

	DWORD dwRanges = (DWORD)
		::SendMessage(m_hWnd, MCM_GETRANGE, 0, (LPARAM) &sysTimes);

	if (dwRanges & GDTR_MIN && pMinRange)
		*pMinRange = CTime(sysTimes[0]);

	if (dwRanges & GDTR_MAX && pMaxRange)
		*pMaxRange = CTime(sysTimes[1]);

	return dwRanges;
}

int CMonthCalCtrl::GetMonthRange(COleDateTime& refMinRange,
	COleDateTime& refMaxRange, DWORD dwFlags) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(dwFlags == GMR_DAYSTATE || dwFlags == GMR_VISIBLE);

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));
	int nCount = (int) ::SendMessage(m_hWnd, MCM_GETMONTHRANGE,
		(WPARAM) dwFlags, (LPARAM) &sysTimes);

	refMinRange = COleDateTime(sysTimes[0]);
	refMaxRange = COleDateTime(sysTimes[1]);

	return nCount;
}

int CMonthCalCtrl::GetMonthRange(LPSYSTEMTIME pMinRange,
	LPSYSTEMTIME pMaxRange, DWORD dwFlags) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT_POINTER(pMinRange, SYSTEMTIME);
	ASSERT_POINTER(pMaxRange, SYSTEMTIME);

	SYSTEMTIME sysTimes[2];

	int nCount = (int) ::SendMessage(m_hWnd, MCM_GETMONTHRANGE,
		(WPARAM) dwFlags, (LPARAM) &sysTimes);

	memcpy(pMinRange, &sysTimes[0], sizeof(SYSTEMTIME));
	memcpy(pMaxRange, &sysTimes[1], sizeof(SYSTEMTIME));

	return nCount;
}

int CMonthCalCtrl::GetMonthRange(CTime& refMinRange, CTime& refMaxRange,
	DWORD dwFlags) const
{
	ASSERT(::IsWindow(m_hWnd));
	ASSERT(dwFlags == GMR_DAYSTATE || dwFlags == GMR_VISIBLE);

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));
	int nCount = (int) ::SendMessage(m_hWnd, MCM_GETMONTHRANGE,
		(WPARAM) dwFlags,   (LPARAM) &sysTimes);

	refMinRange = CTime(sysTimes[0]);
	refMaxRange = CTime(sysTimes[1]);

	return nCount;
}

BOOL CMonthCalCtrl::GetSelRange(LPSYSTEMTIME pMinRange,
	LPSYSTEMTIME pMaxRange) const
{
	ASSERT(m_hWnd != NULL);
	ASSERT((GetStyle() & MCS_MULTISELECT));

	ASSERT_POINTER(pMinRange, SYSTEMTIME);
	ASSERT_POINTER(pMaxRange, SYSTEMTIME);

	SYSTEMTIME sysTimes[2];
	BOOL bReturn = (BOOL) ::SendMessage(m_hWnd, MCM_GETSELRANGE,
										0, (LPARAM) &sysTimes);

	if (bReturn)
	{
		memcpy(pMinRange, &sysTimes[0], sizeof(SYSTEMTIME));
		memcpy(pMaxRange, &sysTimes[1], sizeof(SYSTEMTIME));
	}

	return bReturn;
}

BOOL CMonthCalCtrl::SetSelRange(const LPSYSTEMTIME pMinRange,
	const LPSYSTEMTIME pMaxRange)
{
	ASSERT(m_hWnd != NULL);
	ASSERT((GetStyle() & MCS_MULTISELECT));

	ASSERT_POINTER(pMinRange, SYSTEMTIME);
	ASSERT_POINTER(pMaxRange, SYSTEMTIME);

	SYSTEMTIME sysTimes[2];
	memcpy(&sysTimes[0], pMinRange, sizeof(SYSTEMTIME));
	memcpy(&sysTimes[1], pMaxRange, sizeof(SYSTEMTIME));

	return (BOOL) ::SendMessage(m_hWnd, MCM_SETSELRANGE,
		0, (LPARAM) &sysTimes);
}

BOOL CMonthCalCtrl::SetSelRange(const COleDateTime& refMinRange,
	const COleDateTime& refMaxRange)
{
	// control must have multiple select
	ASSERT((GetStyle() & MCS_MULTISELECT));
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	BOOL bResult = FALSE;

	if (refMinRange.GetStatus() == COleDateTime::valid &&
		refMinRange.GetStatus() == COleDateTime::valid)
	{
		if (refMinRange.GetAsSystemTime(sysTimes[0]) &&
			refMaxRange.GetAsSystemTime(sysTimes[1]))
		{
			bResult = (BOOL)
				::SendMessage(m_hWnd, MCM_SETSELRANGE, 0, (LPARAM)sysTimes);
		}
	}

	return bResult;
}

BOOL CMonthCalCtrl::GetSelRange(COleDateTime& refMinRange,
	COleDateTime& refMaxRange) const
{
	// control must have multiple select
	ASSERT((GetStyle() & MCS_MULTISELECT));
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETSELRANGE, 0, (LPARAM) &sysTimes);

	if (bResult)
	{
		refMinRange = COleDateTime(sysTimes[0]);
		refMaxRange = COleDateTime(sysTimes[1]);
	}
	return bResult;
}

BOOL CMonthCalCtrl::SetSelRange(const CTime& refMinRange,
	const CTime& refMaxRange)
{
	// control must have multiple select
	ASSERT((GetStyle() & MCS_MULTISELECT));
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	BOOL bResult = FALSE;

	if (refMinRange.GetAsSystemTime(sysTimes[0]) &&
		refMaxRange.GetAsSystemTime(sysTimes[1]))
	{
		bResult = (BOOL)
			::SendMessage(m_hWnd, MCM_SETSELRANGE, 0, (LPARAM)sysTimes);
	}

	return bResult;
}

BOOL CMonthCalCtrl::GetSelRange(CTime& refMinRange, CTime& refMaxRange) const
{
	// control must have multiple select
	ASSERT((GetStyle() & MCS_MULTISELECT));
	ASSERT(::IsWindow(m_hWnd));

	SYSTEMTIME sysTimes[2];
	memset(sysTimes, 0, sizeof(sysTimes));
	BOOL bResult = (BOOL)
		::SendMessage(m_hWnd, MCM_GETSELRANGE, 0, (LPARAM) &sysTimes);

	if (bResult)
	{
		refMinRange = CTime(sysTimes[0]);
		refMaxRange = CTime(sysTimes[1]);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// DDX_ routines

void AFXAPI DDX_DateTimeCtrl(CDataExchange* pDX, int nIDC, COleDateTime& value)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	CDateTimeCtrl* pWnd = (CDateTimeCtrl*) CWnd::FromHandle(hWndCtrl);

	if (pDX->m_bSaveAndValidate)
		pWnd->GetTime(value);
	else
		pWnd->SetTime(value);
}

void AFXAPI DDX_DateTimeCtrl(CDataExchange* pDX, int nIDC, CTime& value)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	CDateTimeCtrl* pWnd = (CDateTimeCtrl*) CWnd::FromHandle(hWndCtrl);

	if (pDX->m_bSaveAndValidate)
		pWnd->GetTime(value);
	else
		pWnd->SetTime(&value);
}

void AFXAPI DDX_MonthCalCtrl(CDataExchange* pDX, int nIDC,
	COleDateTime& value)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	CMonthCalCtrl* pWnd = (CMonthCalCtrl*) CWnd::FromHandle(hWndCtrl);

	if (pDX->m_bSaveAndValidate)
		pWnd->GetCurSel(value);
	else
		pWnd->SetCurSel(value);
}

void AFXAPI DDX_MonthCalCtrl(CDataExchange* pDX, int nIDC, CTime& value)
{
	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	CMonthCalCtrl* pWnd = (CMonthCalCtrl*) CWnd::FromHandle(hWndCtrl);

	if (pDX->m_bSaveAndValidate)
		pWnd->GetCurSel(value);
	else
		pWnd->SetCurSel(value);
}

void AFXAPI DDV_MinMaxDateTime(CDataExchange* pDX, CTime& refValue,
	const CTime* pMinRange, const CTime* pMaxRange)
{
	ASSERT(pMinRange == NULL || pMaxRange == NULL || *pMinRange <= *pMaxRange);

	CDateTimeCtrl* pWnd =
		(CDateTimeCtrl*) CWnd::FromHandle(pDX->m_hWndLastControl);

	if (!pDX->m_bSaveAndValidate)
	{
		if ( (pMinRange != NULL && *pMinRange > refValue) ||
			  (pMaxRange != NULL && *pMaxRange < refValue))
		{
#ifdef _DEBUG
			int nIDC = GetWindowLong(pDX->m_hWndLastControl, GWL_ID);
			TRACE1("Warning: initial dialog data is out of range in control ID %d.\n", nIDC);
#endif
			return;     // don't stop now
		}
	}

	pWnd->SetRange(pMinRange, pMaxRange);
}

void AFXAPI DDV_MinMaxDateTime(CDataExchange* pDX,  COleDateTime& refValue,
	const COleDateTime* pMinRange, const COleDateTime* pMaxRange)
{
	ASSERT(pMinRange == NULL || pMaxRange == NULL || *pMinRange <= *pMaxRange);
	CDateTimeCtrl* pWnd =
		(CDateTimeCtrl*) CWnd::FromHandle(pDX->m_hWndLastControl);

	if (!pDX->m_bSaveAndValidate)
	{
		if ( (pMinRange != NULL && *pMinRange > refValue) ||
			  (pMaxRange != NULL && *pMaxRange < refValue))
		{
			int nIDC = GetWindowLong(pDX->m_hWndLastControl, GWL_ID);
			TRACE1("Warning: initial dialog data is out of range in control ID %d.\n", nIDC);
			return;     // don't stop now
		}
	}

	pWnd->SetRange(pMinRange, pMaxRange);
}

void AFXAPI DDV_MinMaxMonth(CDataExchange* pDX, CTime& refValue,
	const CTime* pMinRange, const CTime* pMaxRange)
{
	ASSERT(pMinRange == NULL || pMaxRange == NULL || *pMinRange <= *pMaxRange);
	CMonthCalCtrl* pWnd =
		(CMonthCalCtrl*) CWnd::FromHandle(pDX->m_hWndLastControl);

	if (!pDX->m_bSaveAndValidate)
	{
		if ( (pMinRange != NULL && *pMinRange > refValue) ||
			  (pMaxRange != NULL && *pMaxRange < refValue))
		{
#ifdef _DEBUG
			int nIDC = GetWindowLong(pDX->m_hWndLastControl, GWL_ID);
			TRACE1("Warning: initial dialog data is out of range in control ID %d.\n", nIDC);
#endif
			return;     // don't stop now
		}
	}

	pWnd->SetRange(pMinRange, pMaxRange);
}

void AFXAPI DDV_MinMaxMonth(CDataExchange* pDX, COleDateTime& refValue,
	const COleDateTime* pMinRange, const COleDateTime* pMaxRange)
{
	ASSERT(pMinRange == NULL || pMaxRange == NULL || *pMinRange <= *pMaxRange);
	CMonthCalCtrl* pWnd =
		(CMonthCalCtrl*) CWnd::FromHandle(pDX->m_hWndLastControl);

	if (!pDX->m_bSaveAndValidate)
	{
		if ( (pMinRange != NULL && *pMinRange > refValue) ||
			  (pMaxRange != NULL && *pMaxRange < refValue))
		{
#ifdef _DEBUG
			int nIDC = GetWindowLong(pDX->m_hWndLastControl, GWL_ID);
			TRACE1("Warning: initial dialog data is out of range in control ID %d.\n", nIDC);
#endif
			return;     // don't stop now
		}
	}

	pWnd->SetRange(pMinRange, pMaxRange);
}

/////////////////////////////////////////////////////////////////////////////

#ifndef _AFX_ENABLE_INLINES

static const char _szAfxWinInl[] = "afxdtctl.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxWinInl
#define _AFXDTCTL_INLINE
#include "afxdtctl.inl"

#endif //_AFX_ENABLE_INLINES

/////////////////////////////////////////////////////////////////////////////

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CDateTimeCtrl, CWnd)
IMPLEMENT_DYNAMIC(CMonthCalCtrl, CWnd)

/////////////////////////////////////////////////////////////////////////////
