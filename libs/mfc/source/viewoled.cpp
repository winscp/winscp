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
#include <afxoledb.h>

#ifdef AFX_CORE2_SEG
#pragma code_seg(AFX_CORE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////

BEGIN_MESSAGE_MAP(COleDBRecordView, CFormView)
	//{{AFX_MSG_MAP(COleDBRecordView)
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

void COleDBRecordView::OnInitialUpdate()
{
	CFormView::OnInitialUpdate();
}

BOOL COleDBRecordView::OnMove(UINT nIDMoveCommand)
{
	CRowset* pSet = OnGetRowset();

	if (!UpdateData())
		return TRUE;
	pSet->SetData(0);

	HRESULT hr = E_UNEXPECTED;
	switch (nIDMoveCommand)
	{
		case ID_RECORD_PREV:
			hr = pSet->MovePrev();
			if (hr != S_OK)
				m_bOnFirstRecord = TRUE;
			else
				m_bOnLastRecord  = FALSE;
			break;

		case ID_RECORD_FIRST:
			hr = pSet->MoveFirst();
			if (hr == S_OK)
			{
				m_bOnFirstRecord = TRUE;
				m_bOnLastRecord  = FALSE;
			}
			break;

		case ID_RECORD_NEXT:
			hr = pSet->MoveNext();
			if (hr == S_OK)
				m_bOnFirstRecord = FALSE;
			else
				m_bOnLastRecord  = TRUE;
			break;

		case ID_RECORD_LAST:
			hr = pSet->MoveLast();
			if (hr == S_OK)
			{
				m_bOnFirstRecord = FALSE;
				m_bOnLastRecord  = TRUE;
			}
			break;

		default:
			// Unexpected case value
			ASSERT(FALSE);
	}

	if (hr != S_OK)
		return FALSE;

	// Show results of move operation
	UpdateData(FALSE);
	return TRUE;
}

void COleDBRecordView::OnUpdateRecordFirst(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(!m_bOnFirstRecord);
}

void COleDBRecordView::OnUpdateRecordPrev(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(!m_bOnFirstRecord);
}

void COleDBRecordView::OnUpdateRecordNext(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(!m_bOnLastRecord);
}

void COleDBRecordView::OnUpdateRecordLast(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(!m_bOnLastRecord);
}

//////////////////////////////////////////////////////////////////////////

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleDBRecordView, CFormView)

//////////////////////////////////////////////////////////////////////////
