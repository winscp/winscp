// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

#ifndef __AFXDTCTL_H__
#define __AFXDTCTL_H__

#ifndef __AFXWIN_H__
	#include <afxwin.h>
#endif

#ifndef __AFXDISP_H__
	#include <afxdisp.h>
#endif

/////////////////////////////////////////////////////////////////////////////

#ifdef _AFX_PACKING
#pragma pack(push, _AFX_PACKING)
#endif

//CObject
	class CImageList;
	//CCmdTarget;
		//CWnd
			// class CListBox;
			class CMonthCalCtrl;
			class CDateTimeCtrl;

/////////////////////////////////////////////////////////////////////////////
// CDateTimeCtrl

class CDateTimeCtrl : public CWnd
{
	DECLARE_DYNAMIC(CDateTimeCtrl)

// Constructors
	CDateTimeCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);

// Attributes
	COLORREF GetMonthCalColor(int iColor) const;
	COLORREF SetMonthCalColor(int iColor, COLORREF ref);
	BOOL SetFormat(LPCTSTR pstrFormat);
	CMonthCalCtrl* GetMonthCalCtrl() const;
	CFont* GetMonthCalFont() const;
	void SetMonthCalFont(HFONT hFont, BOOL bRedraw = TRUE);
	BOOL SetRange(const COleDateTime* pMinRange, const COleDateTime* pMaxRange);
	DWORD GetRange(COleDateTime* pMinRange, COleDateTime* pMaxRange) const;
	BOOL SetRange(const CTime* pMinRange, const CTime* pMaxRange);
	DWORD GetRange(CTime* pMinRange, CTime* pMaxRange) const;

// Operations
	BOOL SetTime(const CTime* pTimeNew);
	DWORD GetTime(CTime& timeDest) const;
	BOOL SetTime(const COleDateTime& timeNew);
	BOOL GetTime(COleDateTime& timeDest) const;
	BOOL SetTime(LPSYSTEMTIME pTimeNew = NULL);
	DWORD GetTime(LPSYSTEMTIME pTimeDest) const;

// Overridables
	virtual ~CDateTimeCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// CMonthCalCtrl

class CMonthCalCtrl : public CWnd
{
	DECLARE_DYNAMIC(CMonthCalCtrl)

// Constructors
	CMonthCalCtrl();
	BOOL Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID);
	BOOL Create(DWORD dwStyle, const POINT& pt, CWnd* pParentWnd, UINT nID);

//Attributes
	BOOL GetMinReqRect(RECT* pRect) const;
	int SetMonthDelta(int iDelta);
	int GetMonthDelta() const;
	BOOL SetFirstDayOfWeek(int iDay, int* lpnOld = NULL);
	int GetFirstDayOfWeek(BOOL* pbLocal = NULL) const;
	COLORREF GetColor(int nRegion) const;
	COLORREF SetColor(int nRegion, COLORREF ref);
	DWORD HitTest(PMCHITTESTINFO pMCHitTest);

// Operations
	BOOL SizeMinReq(BOOL bRepaint = TRUE);
	void SetToday(const COleDateTime& refDateTime);
	void SetToday(const CTime* pDateTime);
	void SetToday(const LPSYSTEMTIME pDateTime);
	BOOL GetToday(CTime& refTime) const;
	BOOL GetToday(COleDateTime& refDateTime) const;
	BOOL GetToday(LPSYSTEMTIME pDateTime) const;
	BOOL GetCurSel(LPSYSTEMTIME pDateTime) const;
	BOOL SetCurSel(const LPSYSTEMTIME pDateTime);
	BOOL SetCurSel(const CTime& refDateTime);
	BOOL GetCurSel(CTime& refDateTime) const;
	BOOL SetCurSel(const COleDateTime& refDateTime);
	BOOL GetCurSel(COleDateTime& refDateTime) const;
	BOOL SetDayState(int nMonths, LPMONTHDAYSTATE pStates);
	BOOL SetMaxSelCount(int nMax);
	int GetMaxSelCount() const;
	BOOL SetRange(const COleDateTime* pMinRange, const COleDateTime* pMaxRange);
	DWORD GetRange(COleDateTime* pMinRange, COleDateTime* pMaxRange) const;
	BOOL SetRange(const CTime* pMinRange, const CTime* pMaxRange);
	DWORD GetRange(CTime* pMinRange, CTime* pMaxRange) const;
	BOOL SetRange(const LPSYSTEMTIME pMinRange, const LPSYSTEMTIME pMaxRange);
	DWORD GetRange(LPSYSTEMTIME pMinRange, LPSYSTEMTIME pMaxRange) const;
	int GetMonthRange(COleDateTime& refMinRange, COleDateTime& refMaxRange,
		DWORD dwFlags) const;
	int GetMonthRange(CTime& refMinRange, CTime& refMaxRange,
		DWORD dwFlags) const;
	int GetMonthRange(LPSYSTEMTIME pMinRange, LPSYSTEMTIME pMaxRange,
		DWORD dwFlags) const;
	BOOL SetSelRange(const COleDateTime& pMinRange,
		const COleDateTime& pMaxRange);
	BOOL GetSelRange(COleDateTime& refMinRange,
		COleDateTime& refMaxRange) const;
	BOOL SetSelRange(const CTime& pMinRange, const CTime& pMaxRange);
	BOOL GetSelRange(CTime& refMinRange, CTime& refMaxRange) const;
	BOOL GetSelRange(LPSYSTEMTIME pMinRange, LPSYSTEMTIME pMaxRange) const;
	BOOL SetSelRange(const LPSYSTEMTIME pMinRange,
		const LPSYSTEMTIME pMaxRange);

// Overridables
	virtual ~CMonthCalCtrl();
};

/////////////////////////////////////////////////////////////////////////////
// Inline function declarations

#ifdef _AFX_PACKING
#pragma pack(pop)
#endif

#ifdef _AFX_ENABLE_INLINES
#define _AFXDTCTL_INLINE AFX_INLINE
#include <afxdtctl.inl>
#undef _AFXDTCTL_INLINE
#endif

#undef AFX_DATA
#define AFX_DATA

#ifdef _AFX_MINREBUILD
#pragma component(minrebuild, on)
#endif
#ifndef _AFX_FULLTYPEINFO
#pragma component(mintypeinfo, off)
#endif

#endif //__AFXDTCTL_H__

/////////////////////////////////////////////////////////////////////////////
