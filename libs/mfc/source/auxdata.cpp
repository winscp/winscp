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

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#undef AfxEnableWin30Compatibility
#undef AfxEnableWin40Compatibility
#undef AfxEnableWin31Compatibility

/////////////////////////////////////////////////////////////////////////////
// Cached system metrics, etc

AFX_DATADEF AUX_DATA afxData;

// Win40 compatibility is now the default.  It is not necessary to call
// this if your application is marked as 4.0.  It is provided only for
// backward compatibility.
void AFXAPI AfxEnableWin40Compatibility()
{
	if (afxData.bWin4)
	{
		// Later versions of Windows report "correct" scrollbar metrics
		// MFC assumes the old metrics, so they need to be adjusted.
		afxData.cxVScroll = GetSystemMetrics(SM_CXVSCROLL) + CX_BORDER;
		afxData.cyHScroll = GetSystemMetrics(SM_CYHSCROLL) + CY_BORDER;
		afxData.bMarked4 = TRUE;
	}
}

// Call this API in your InitInstance if your application is marked
// as a Windows 3.1 application.
// This is done by linking with /subsystem:windows,3.1.
void AFXAPI AfxEnableWin31Compatibility()
{
	afxData.cxVScroll = GetSystemMetrics(SM_CXVSCROLL);
	afxData.cyHScroll = GetSystemMetrics(SM_CYHSCROLL);
	afxData.bMarked4 = FALSE;
}

// Initialization code
AUX_DATA::AUX_DATA()
{
	// Cache various target platform version information
	DWORD dwVersion = ::GetVersion();
	nWinVer = (LOBYTE(dwVersion) << 8) + HIBYTE(dwVersion);
	bWin95 = (dwVersion & 0x80000000) != 0;
	bWin4 = (BYTE)dwVersion >= 4;
	bNotWin4 = 1 - bWin4;   // for convenience
	bSmCaption = bWin4;
	bMarked4 = FALSE;

	// determine various metrics based on EXE subsystem version mark
	if (bWin4)
		bMarked4 = (GetProcessVersion(0) >= 0x00040000);

	// Cached system metrics (updated in CWnd::OnWinIniChange)
	UpdateSysMetrics();

	// Cached system values (updated in CWnd::OnSysColorChange)
	hbrBtnFace = NULL;
	UpdateSysColors();

	// Standard cursors
	hcurWait = ::LoadCursor(NULL, IDC_WAIT);
	hcurArrow = ::LoadCursor(NULL, IDC_ARROW);
	ASSERT(hcurWait != NULL);
	ASSERT(hcurArrow != NULL);
	hcurHelp = NULL;    // loaded on demand

	// cxBorder2 and cyBorder are 2x borders for Win4
	cxBorder2 = bWin4 ? CX_BORDER*2 : CX_BORDER;
	cyBorder2 = bWin4 ? CY_BORDER*2 : CY_BORDER;

	// allocated on demand
	hbmMenuDot = NULL;
	hcurHelp = NULL;
}

#ifdef AFX_TERM_SEG
#pragma code_seg(AFX_TERM_SEG)
#endif

// Termination code
AUX_DATA::~AUX_DATA()
{
	// clean up objects we don't actually create
	AfxDeleteObject((HGDIOBJ*)&hbmMenuDot);
}

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

void AUX_DATA::UpdateSysColors()
{
	clrBtnFace = ::GetSysColor(COLOR_BTNFACE);
	clrBtnShadow = ::GetSysColor(COLOR_BTNSHADOW);
	clrBtnHilite = ::GetSysColor(COLOR_BTNHIGHLIGHT);
	clrBtnText = ::GetSysColor(COLOR_BTNTEXT);
	clrWindowFrame = ::GetSysColor(COLOR_WINDOWFRAME);

	hbrBtnFace = ::GetSysColorBrush(COLOR_BTNFACE);
	ASSERT(hbrBtnFace != NULL);
	hbrWindowFrame = ::GetSysColorBrush(COLOR_WINDOWFRAME);
	ASSERT(hbrWindowFrame != NULL);
}

void AUX_DATA::UpdateSysMetrics()
{
	// System metrics
	cxIcon = GetSystemMetrics(SM_CXICON);
	cyIcon = GetSystemMetrics(SM_CYICON);

	// System metrics which depend on subsystem version
	if (bMarked4)
		AfxEnableWin40Compatibility();
	else
		AfxEnableWin31Compatibility();

	// Device metrics for screen
	HDC hDCScreen = GetDC(NULL);
	ASSERT(hDCScreen != NULL);
	cxPixelsPerInch = GetDeviceCaps(hDCScreen, LOGPIXELSX);
	cyPixelsPerInch = GetDeviceCaps(hDCScreen, LOGPIXELSY);
	ReleaseDC(NULL, hDCScreen);
}

/////////////////////////////////////////////////////////////////////////////
