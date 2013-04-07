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

#ifdef AFX_DB_SEG
#pragma code_seg(AFX_DB_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////

IMPLEMENT_DYNAMIC(CDaoRecordView, CFormView)

BEGIN_MESSAGE_MAP(CDaoRecordView, CFormView)
	//{{AFX_MSG_MAP(CDaoRecordView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
	ON_COMMAND_EX(ID_RECORD_FIRST, OnMove)
	ON_UPDATE_COMMAND_UI(ID_RECORD_FIRST, OnUpdateRecordFirst)
	ON_COMMAND_EX(ID_RECORD_PREV, OnMove)
	ON_UPDATE_COMMAND_UI(ID_RECORD_PREV, OnUpdateRecordPrev)
	ON_COMMAND_EX(ID_RECORD_NEXT, OnMove)
	ON_UPDATE_COMMAND_UI(ID_RECORD_NEXT, OnUpdateRecordNext)
	ON_COMMAND_EX(ID_RECORD_LAST, OnMove)
	ON_UPDATE_COMMAND_UI(ID_RECORD_LAST, OnUpdateRecordLast)
END_MESSAGE_MAP()

CDaoRecordView::CDaoRecordView(LPCTSTR lpszTemplateName)
	: CFormView(lpszTemplateName)
{
	m_nStatus = 0;

	// Setup dummy bookmarks
	m_varBookmarkCurrent = 1L;
	m_varBookmarkFirst = m_varBookmarkLast = 0L;
}

CDaoRecordView::CDaoRecordView(UINT nIDTemplate)
	: CFormView(nIDTemplate)
{
	m_nStatus = 0;

	// Setup dummy bookmarks
	m_varBookmarkCurrent = 1L;
	m_varBookmarkFirst = m_varBookmarkLast = 0L;
}

CDaoRecordView::~CDaoRecordView()
{
}

void CDaoRecordView::OnInitialUpdate()
{
	ASSERT_VALID(this);

	CDaoRecordset* pRecordset = OnGetRecordset();
	// recordset must be allocated already
	ASSERT(pRecordset != NULL);

	if (!pRecordset->IsOpen())
	{
		CWaitCursor wait;
		pRecordset->Open();
	}

	if (!pRecordset->IsEOF())
	{
		// Determine recordset properties for move button enabling
		if (pRecordset->CanBookmark())
		{
			// Get the bookmark of the first record
			m_varBookmarkCurrent = pRecordset->GetBookmark();
			m_varBookmarkFirst = m_varBookmarkCurrent;
		}

		// Enable forward scrolling buttons
		m_nStatus |= AFX_DAOVIEW_SCROLL_NEXT;

		// Enable backward scrolling buttons if possible
		if (pRecordset->CanScroll())
		{
			m_nStatus |= AFX_DAOVIEW_SCROLL_LAST;
			m_nStatus |= AFX_DAOVIEW_SCROLL_BACKWARD;
		}
		else
		{
			m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
			m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
		}
	}
	else
	{
		// Disable scrolling
		m_nStatus &= ~AFX_DAOVIEW_SCROLL_NEXT;
		m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
		m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
	}

	CFormView::OnInitialUpdate();
}

BOOL CDaoRecordView::OnMove(UINT nIDMoveCommand)
{
	ASSERT_VALID(this);

	CDaoRecordset* pSet = OnGetRecordset();
	if (pSet->CanUpdate())
	{
		pSet->Edit();
		if (!UpdateData())
			return TRUE;

		if (pSet->IsFieldDirty(NULL))
			pSet->Update();
		else
			pSet->CancelUpdate();
	}

	BOOL bBookmarkable = pSet->CanBookmark();
	BOOL bScrollable = pSet->CanScroll();

	switch (nIDMoveCommand)
	{
		case ID_RECORD_PREV:
			pSet->MovePrev();

			if (!pSet->IsBOF())
			{
				if (bBookmarkable)
					m_varBookmarkCurrent = pSet->GetBookmark();

				// Enable forward scrolling
				m_nStatus |= AFX_DAOVIEW_SCROLL_NEXT;

				if (bScrollable)
				{
					m_nStatus |= AFX_DAOVIEW_SCROLL_LAST;

					if (IsOnFirstRecord())
						// Disable backward scrolling
						m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
					else
						m_nStatus |= AFX_DAOVIEW_SCROLL_BACKWARD;
				}
				else
				{
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
				}


				break;
			}
			// Fall through to reset to first record

		case ID_RECORD_FIRST:
			pSet->MoveFirst();

			// backward scrolling never allowed after movefirst
			m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;

			if (pSet->IsEOF())
			{
				// Empty recordset, disable forward too
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_NEXT;
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
			}
			else
			{
				if (bBookmarkable)
				{
					m_varBookmarkCurrent = pSet->GetBookmark();
					m_varBookmarkFirst = m_varBookmarkCurrent;
				}

				// Enable forward scrolling
				m_nStatus |= AFX_DAOVIEW_SCROLL_NEXT;

				if (bScrollable)
					m_nStatus |= AFX_DAOVIEW_SCROLL_LAST;
				else
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
			}

			break;

		case ID_RECORD_NEXT:
			pSet->MoveNext();

			if (!pSet->IsEOF())
			{
				if (bBookmarkable)
					m_varBookmarkCurrent = pSet->GetBookmark();

				if (IsOnLastRecord())
				{
					// Disable forward scrolling
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_NEXT;
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
				}
				else
				{
					m_nStatus |= AFX_DAOVIEW_SCROLL_NEXT;
					m_nStatus |= AFX_DAOVIEW_SCROLL_LAST;
				}

				if (bScrollable)
					m_nStatus |= AFX_DAOVIEW_SCROLL_BACKWARD;
				else
				{
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
				}

				break;
			}

			// Can't fall through to move last
			if (!bScrollable)
			{
				// At the end of forward only recordset
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_NEXT;
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
				break;
			}

			// Fall through to reset to last record

		case ID_RECORD_LAST:
			pSet->MoveLast();

			// forward scrolling never allowed after movelast
			m_nStatus &= ~AFX_DAOVIEW_SCROLL_NEXT;
			m_nStatus &= ~AFX_DAOVIEW_SCROLL_LAST;

			if (pSet->IsBOF())
			{
				// Empty recordset, disable backward too
				m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
			}
			else
			{
				if (bBookmarkable)
				{
					m_varBookmarkCurrent = pSet->GetBookmark();
					m_varBookmarkLast = m_varBookmarkCurrent;
				}

				// Enable backward scrolling
				if (bBookmarkable)
					m_nStatus |= AFX_DAOVIEW_SCROLL_BACKWARD;
				else
					m_nStatus &= ~AFX_DAOVIEW_SCROLL_BACKWARD;
			}

			break;

		default:
			// Unexpected case value
			ASSERT(FALSE);
	}

	// Show results of move operation
	UpdateData(FALSE);
	return TRUE;
}

BOOL CDaoRecordView::IsOnFirstRecord()
{
	ASSERT_VALID(this);
	return (m_varBookmarkCurrent == m_varBookmarkFirst);
}

BOOL CDaoRecordView::IsOnLastRecord()
{
	ASSERT_VALID(this);
	return (m_varBookmarkCurrent == m_varBookmarkLast);
}

/////////////////////////////////////////////////////////////////////////////
// DDX Cover functions for use with fields of a recordset

/////////////////////////////////////////////////////////////////////////////
// Simple field formatting to text item

BOOL AFXAPI AfxFieldText(CDataExchange* pDX, int nIDC, void* pv,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareEditCtrl(nIDC);
	TCHAR szT[2];
	if (pDX->m_bSaveAndValidate)
	{
		::GetWindowText(hWndCtrl, szT, _countof(szT));
		if (szT[0] == '\0')
		{
			// If edit buffer not NULL prior to update, set it dirty
			// to catch case of setting field value from PSEUDO NULL to NULL.
			if (!pRecordset->IsFieldNull(pv))
				pRecordset->SetFieldDirty(pv, TRUE);
			pRecordset->SetFieldNull(pv);
			return TRUE;
		}
		else
		{
			// If edit buffer NULL prior to update, set it dirty
			// to catch case of setting field value to PSEUDO NULL.
			if (pRecordset->IsFieldNull(pv))
				pRecordset->SetFieldDirty(pv, TRUE);
			pRecordset->SetFieldNull(pv, FALSE);
		}
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(pv))
		{
			szT[0] = '\0';
			AfxSetWindowText(hWndCtrl, szT);
			return TRUE;
		}
	}
	return FALSE;
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, BOOL& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, BYTE& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, short& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, long& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, DWORD& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, COleCurrency& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, float& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, double& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, COleDateTime& value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, CString &value,
	CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &value, pRecordset))
		DDX_Text(pDX, nIDC, value);
}

void AFXAPI DDX_FieldText(CDataExchange* pDX, int nIDC, LPTSTR pstrValue,
	int nMaxLen, CDaoRecordset* pRecordset)
{
	if (!AfxFieldText(pDX, nIDC, &pstrValue, pRecordset))
		DDX_Text(pDX, nIDC, pstrValue, nMaxLen);
}

void AFXAPI DDX_FieldLBString(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
	{
		int nIndex = (int)::SendMessage(hWndCtrl, LB_GETCURSEL, 0, 0L);
		if (nIndex != -1)
		{
			int nLen = (int)::SendMessage(hWndCtrl, LB_GETTEXTLEN, nIndex, 0L);
			::SendMessage(hWndCtrl, LB_GETTEXT, nIndex,
					(LPARAM)(LPSTR)value.GetBuffer(nLen));
			if (nLen == 0)
			{
				if (pRecordset->IsFieldNullable(&value))
					pRecordset->SetFieldNull(&value, TRUE);
			}
			else
			{
				pRecordset->SetFieldNull(&value, FALSE);
			}
			value.ReleaseBuffer();
		}
		else
		{
			// no selection
			value.GetBufferSetLength(0);
			if (pRecordset->IsFieldNullable(&value))
				pRecordset->SetFieldNull(&value);
		}
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value))
		{
			SendMessage(hWndCtrl, LB_SETCURSEL, (WPARAM)-1, 0L);
		}
		else
		{
			// set current selection based on data string
			if (::SendMessage(hWndCtrl, LB_SELECTSTRING, (WPARAM)-1,
			  (LPARAM)(LPCTSTR)value) == LB_ERR)
			{
				// no selection match
				TRACE0("Warning: no listbox item selected.\n");
			}
		}
	}
}

void AFXAPI DDX_FieldLBStringExact(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
	{
		DDX_FieldLBString(pDX, nIDC, value, pRecordset);
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value))
		{
			SendMessage(hWndCtrl, LB_SETCURSEL, (WPARAM)-1, 0L);
		}
		else
		{
			// set current selection based on data string
			int i = (int)::SendMessage(hWndCtrl, LB_FINDSTRINGEXACT, (WPARAM)-1,
			  (LPARAM)(LPCTSTR)value);
			if (i < 0)
			{
				// no selection match
				TRACE0("Warning: no listbox item selected.\n");
			}
			else
			{
				// select it
				SendMessage(hWndCtrl, LB_SETCURSEL, i, 0L);
			}
		}
	}
}

void AFXAPI DDX_FieldCBString(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
	{
		// just get current edit item text (or drop list static)
		int nLen = ::GetWindowTextLength(hWndCtrl);
		if (nLen != -1)
		{
			// get known length
			::GetWindowText(hWndCtrl, value.GetBuffer(nLen), nLen+1);
		}
		else
		{
			// for drop lists GetWindowTextLength does not work - assume
			//  preallocated length (or 256, whichever is larger)
			nLen = value.GetAllocLength();
			if (nLen < 256)
				nLen = 256;
			::GetWindowText(hWndCtrl, value.GetBuffer(nLen-1), nLen);
		}
		value.ReleaseBuffer();
		if (value.GetLength() == 0)
		{
			if (pRecordset->IsFieldNullable(&value))
				pRecordset->SetFieldNull(&value, TRUE);
		}
		else
		{
			pRecordset->SetFieldNull(&value, FALSE);
		}
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value))
		{
			SendMessage(hWndCtrl, CB_SETCURSEL, (WPARAM)-1, 0L);
		}
		else
		{
			// set current selection based on model string
			if (::SendMessage(hWndCtrl, CB_SELECTSTRING, (WPARAM)-1,
				(LPARAM)(LPCTSTR)value) == CB_ERR)
			{
				// just set the edit text (will be ignored if DROPDOWNLIST)
				AfxSetWindowText(hWndCtrl, value);
			}
		}
	}
}

void AFXAPI DDX_FieldCBStringExact(CDataExchange* pDX, int nIDC, CString& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
	{
		DDX_FieldCBString(pDX, nIDC, value, pRecordset);
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value))
		{
			SendMessage(hWndCtrl, CB_SETCURSEL, (WPARAM)-1, 0L);
		}
		else
		{
			// set current selection based on data string
			int i = (int)::SendMessage(hWndCtrl, CB_FINDSTRINGEXACT, (WPARAM)-1,
			  (LPARAM)(LPCTSTR)value);
			if (i < 0)
			{
				// no selection match
				TRACE0("Warning: no combobox item selected.\n");
			}
			else
			{
				// select it
				SendMessage(hWndCtrl, CB_SETCURSEL, i, 0L);
			}
		}
	}
}

void AFXAPI DDX_FieldLBIndex(CDataExchange* pDX, int nIDC, int& index,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	if (!pDX->m_bSaveAndValidate &&
		(!pRecordset->IsOpen() || pRecordset->IsFieldNull(&index)))
	{
		int nIndex = 0;
		DDX_LBIndex(pDX, nIDC, nIndex);
	}
	else
		DDX_LBIndex(pDX, nIDC, index);
}

void AFXAPI DDX_FieldCBIndex(CDataExchange* pDX, int nIDC, int& index,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	if (!pDX->m_bSaveAndValidate &&
		(!pRecordset->IsOpen() || pRecordset->IsFieldNull(&index)))
	{
		int nIndex = 0;
		DDX_CBIndex(pDX, nIDC, nIndex);
	}
	else
		DDX_CBIndex(pDX, nIDC, index);
}

void AFXAPI DDX_FieldScroll(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	if (!pDX->m_bSaveAndValidate &&
		(!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value)))
	{
		int nValue = 0;
		DDX_Scroll(pDX, nIDC, nValue);
	}
	else
		DDX_Scroll(pDX, nIDC, value);
}

void AFXAPI DDX_FieldSlider(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	if (!pDX->m_bSaveAndValidate &&
		(!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value)))
	{
		int nValue = 0;
		DDX_Slider(pDX, nIDC, nValue);
	}
	else
		DDX_Slider(pDX, nIDC, value);
}

/////////////////////////////////////////////////////////////////////////////
// Data exchange for special controls

void AFXAPI DDX_FieldCheck(CDataExchange* pDX, int nIDC, int& value, CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	HWND hWndCtrl = pDX->PrepareCtrl(nIDC);
	if (pDX->m_bSaveAndValidate)
	{
		value = (int)::SendMessage(hWndCtrl, BM_GETCHECK, 0, 0L);
		ASSERT(value >= 0 && value <= 2);
		if (value == 2)
		{
			if (pRecordset->IsFieldNullable(&value))
				pRecordset->SetFieldNull(&value);
			else
			{
				TRACE0("Warning: can't set field NULL for checkbox value.\n");
				// Default to unchecked
				value = 0;
			}
		}
	}
	else
	{
		if (!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value))
		{
			int style = ((int)::GetWindowLong(hWndCtrl, GWL_STYLE) & 0xf);
			if ((style == BS_3STATE || style == BS_AUTO3STATE))
				value = 2;
			else
			{
				TRACE0("Warning: can't set checkbox value for NULL field.\n");
				// Default to unchecked
				value = 0;
			}
		}
		if (value < 0 || value > 2)
		{
			value = 0;      // default to off
			TRACE1("Warning: dialog data checkbox value (%d) out of range.\n",
				value);
		}
		::SendMessage(hWndCtrl, BM_SETCHECK, (WPARAM)value, 0L);
	}
}

void AFXAPI DDX_FieldRadio(CDataExchange* pDX, int nIDC, int& value,
	CDaoRecordset* pRecordset)
{
	ASSERT_VALID(pRecordset);

	if (!pDX->m_bSaveAndValidate &&
		(!pRecordset->IsOpen() || pRecordset->IsFieldNull(&value)))
		value = -1;
	DDX_Radio(pDX, nIDC, value);
	if (pDX->m_bSaveAndValidate)
	{
		if (value == -1 && !pRecordset->IsFieldNullable(&value))
		{
			AfxFailRadio(pDX);
		}
		else
		{
			pRecordset->SetFieldNull(&value, (value == -1));
		}
	}
}

/////////////////////////////////////////////////////////////////////////////

#ifdef _DEBUG
void CDaoRecordView::AssertValid() const
{
	CFormView::AssertValid();
}

void CDaoRecordView::Dump(CDumpContext& dc) const
{
	ASSERT_VALID(this);

	CFormView::Dump(dc);

	dc << "m_nStatus =" << m_nStatus;
	dc << "m_varBookmarkCurrent =" << m_varBookmarkCurrent;
	dc << "m_varBookmarkFirst =" << m_varBookmarkFirst;
	dc << "m_varBookmarkLast =" << m_varBookmarkLast;

	dc << "\n";
}
#endif

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxDaoInl[] = "afxdao.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxDaoInl
#define _AFXDAOVIEW_INLINE
#include "afxdao.inl"

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

/////////////////////////////////////////////////////////////////////////////
