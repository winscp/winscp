// This is a part of the Microsoft Foundation Classes C++ library.
// Copyright (C) 1992-1998 Microsoft Corporation
// All rights reserved.
//
// This source code is only intended as a supplement to the
// Microsoft Foundation Classes Reference and related
// electronic documentation provided with the library.
// See these sources for detailed information regarding the
// Microsoft Foundation Classes product.

// Do not include this file directly (included by AFXDB.H & AFXDAO.H)

#ifndef __AFXDB__H__
#define __AFXDB__H__

//////////////////////////////////////////////////////////////////////////////
// Recordset Field exchange for ODBC & DAO classes (RFX_ and DFX_)

#define AFX_RFX_SHORT_PSEUDO_NULL (0x7EE4)
#define AFX_RFX_INT_PSEUDO_NULL (0x7EE4)
#define AFX_RFX_LONG_PSEUDO_NULL (0x4a4d4120L)
#define AFX_RFX_BYTE_PSEUDO_NULL 255
#define AFX_RFX_SINGLE_PSEUDO_NULL (-9.123e19f)
#define AFX_RFX_DOUBLE_PSEUDO_NULL (-9.123e19)
#define AFX_RFX_BOOL_PSEUDO_NULL 2
#define AFX_RFX_DATE_PSEUDO_NULL CTime(0)
#define AFX_RFX_TIMESTAMP_PSEUDO_NULL 99

#define AFX_RFX_NO_TYPE     0
#define AFX_RFX_BOOL        1
#define AFX_RFX_BYTE        2
#define AFX_RFX_INT         3
#define AFX_RFX_LONG        4
#define AFX_RFX_SINGLE      6
#define AFX_RFX_DOUBLE      7
#define AFX_RFX_DATE        8
#define AFX_RFX_BINARY      9
#define AFX_RFX_TEXT        10
#define AFX_RFX_LONGBINARY  11
#define AFX_RFX_SHORT       12
#define AFX_RFX_CURRENCY    13
#define AFX_RFX_OLEDATETIME 14
#define AFX_RFX_TIMESTAMP   15
#define AFX_RFX_OLEDATE     16
#define AFX_RFX_LPTSTR      17

//////////////////////////////////////////////////////////////////////////////
// CLongBinary - a Long (generally > 32k in length) Binary object

class CLongBinary : public CObject
{
	DECLARE_DYNAMIC(CLongBinary)

// Constructors
public:
	CLongBinary();

// Attributes
	HGLOBAL m_hData;
	DWORD m_dwDataLength;

// Implementation
public:
	virtual ~CLongBinary();

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif //_DEBUG
};

//////////////////////////////////////////////////////////////////////////////

#endif // __AFXDB__H__
