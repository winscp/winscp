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

#ifdef AFX_OCC_SEG
#pragma code_seg(AFX_OCC_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifndef _AFX_NO_OCC_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// Private helper for read-only property exchange with OLE controls

static void DDX_OCPropertyRO(CDataExchange* pDX, int nIDC,
	DISPID dispid, VARTYPE vt, void* pValue)
{
	if (pDX->m_bSaveAndValidate)
	{
		CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
		pControl->GetProperty(dispid, vt, pValue);
	}
}

/////////////////////////////////////////////////////////////////////////////
// Simple formatting to text item

void AFXAPI DDX_OCText(CDataExchange* pDX, int nIDC, DISPID dispid,
	CString& value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_BSTR, &value);
	else
		pControl->SetProperty(dispid, VT_BSTR, (LPCTSTR)value);
}

void AFXAPI DDX_OCTextRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	CString& value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_BSTR, &value);
}

/////////////////////////////////////////////////////////////////////////////
// non-text properties

void AFXAPI DDX_OCBool(CDataExchange* pDX, int nIDC, DISPID dispid,
	BOOL& value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_BOOL, &value);
	else
		pControl->SetProperty(dispid, VT_BOOL, value);
}

void AFXAPI DDX_OCBoolRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	BOOL& value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_BOOL, &value);
}

void AFXAPI DDX_OCInt(CDataExchange* pDX, int nIDC, DISPID dispid,
	int &value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_I4, &value);
	else
		pControl->SetProperty(dispid, VT_I4, value);
}

void AFXAPI DDX_OCIntRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	int &value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_I4, &value);
}

void AFXAPI DDX_OCInt(CDataExchange* pDX, int nIDC, DISPID dispid,
	long &value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_I4, &value);
	else
		pControl->SetProperty(dispid, VT_I4, value);
}

void AFXAPI DDX_OCIntRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	long &value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_I4, &value);
}

void AFXAPI DDX_OCShort(CDataExchange* pDX, int nIDC, DISPID dispid,
	short& value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_I2, &value);
	else
		pControl->SetProperty(dispid, VT_I2, value);
}

void AFXAPI DDX_OCShortRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	short& value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_I2, &value);
}

void AFXAPI DDX_OCColor(CDataExchange* pDX, int nIDC, DISPID dispid,
	OLE_COLOR& value)
{
	CWnd* pControl = pDX->PrepareOleCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
		pControl->GetProperty(dispid, VT_COLOR, &value);
	else
		pControl->SetProperty(dispid, VT_COLOR, value);
}

void AFXAPI DDX_OCColorRO(CDataExchange* pDX, int nIDC, DISPID dispid,
	OLE_COLOR& value)
{
	DDX_OCPropertyRO(pDX, nIDC, dispid, VT_COLOR, &value);
}

#endif // !_AFX_NO_OCC_SUPPORT
