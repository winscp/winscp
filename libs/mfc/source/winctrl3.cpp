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

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// _AFX_CHECKLIST_STATE

class _AFX_CHECKLIST_STATE : public CNoTrackObject
{
public:
	_AFX_CHECKLIST_STATE();
	virtual ~_AFX_CHECKLIST_STATE();

	HBITMAP m_hbitmapCheck;
	CSize m_sizeCheck;
};

_AFX_CHECKLIST_STATE::_AFX_CHECKLIST_STATE()
{
	CBitmap bitmap;

#ifndef _AFX_NO_CTL3D_SUPPORT
	if (afxData.bWin4 || AfxGetCtl3dState()->m_pfnSubclassDlgEx != NULL)
#else
	if (afxData.bWin4)
#endif
		VERIFY(bitmap.LoadBitmap(AFX_IDB_CHECKLISTBOX_95));
	else
		VERIFY(bitmap.LoadBitmap(AFX_IDB_CHECKLISTBOX_NT));

	BITMAP bm;
	bitmap.GetObject(sizeof (BITMAP), &bm);
	m_sizeCheck.cx = bm.bmWidth / 3;
	m_sizeCheck.cy = bm.bmHeight;
	m_hbitmapCheck = (HBITMAP)bitmap.Detach();
}

_AFX_CHECKLIST_STATE::~_AFX_CHECKLIST_STATE()
{
	if (m_hbitmapCheck != NULL)
		::DeleteObject(m_hbitmapCheck);
}

EXTERN_PROCESS_LOCAL(_AFX_CHECKLIST_STATE, _afxChecklistState)

/////////////////////////////////////////////////////////////////////////////
// AFX_CHECK_DATA

struct AFX_CHECK_DATA
{
public:
	int m_nCheck;
	BOOL m_bEnabled;
	DWORD m_dwUserData;

	AFX_CHECK_DATA()
	{
		m_nCheck = 0;
		m_bEnabled = TRUE;
		m_dwUserData = 0;
	};
};

/////////////////////////////////////////////////////////////////////////////
// CCheckListBox

BEGIN_MESSAGE_MAP(CCheckListBox, CListBox)
	//{{AFX_MSG_MAP(CCheckListBox)
	ON_WM_LBUTTONDOWN()
	ON_WM_KEYDOWN()
	ON_WM_CREATE()
	ON_WM_LBUTTONDBLCLK()
	ON_MESSAGE(WM_SETFONT, OnSetFont)
	ON_MESSAGE(LB_ADDSTRING, OnLBAddString)
	ON_MESSAGE(LB_FINDSTRING, OnLBFindString)
	ON_MESSAGE(LB_FINDSTRINGEXACT, OnLBFindStringExact)
	ON_MESSAGE(LB_GETITEMDATA, OnLBGetItemData)
	ON_MESSAGE(LB_GETTEXT, OnLBGetText)
	ON_MESSAGE(LB_INSERTSTRING, OnLBInsertString)
	ON_MESSAGE(LB_SELECTSTRING, OnLBSelectString)
	ON_MESSAGE(LB_SETITEMDATA, OnLBSetItemData)
	ON_MESSAGE(LB_SETITEMHEIGHT, OnLBSetItemHeight)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CCheckListBox::Create(DWORD dwStyle, const RECT& rect, CWnd* pParentWnd, UINT nID)
{
	if (!(dwStyle & LBS_OWNERDRAWVARIABLE)) //must be one or the other
		dwStyle |= LBS_OWNERDRAWFIXED;
	return CListBox::Create(dwStyle, rect, pParentWnd, nID);
}

void CCheckListBox::SetCheckStyle(UINT nStyle)
{
	ASSERT(nStyle == 0 || nStyle == BS_CHECKBOX ||
		nStyle == BS_AUTOCHECKBOX || nStyle == BS_AUTO3STATE ||
		nStyle == BS_3STATE);

	m_nStyle = nStyle;
}

void CCheckListBox::SetCheck(int nIndex, int nCheck)
{
	ASSERT(::IsWindow(m_hWnd));

	if (nCheck == 2)
	{
		if (m_nStyle == BS_CHECKBOX || m_nStyle == BS_AUTOCHECKBOX)
			return;
	}

	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, nIndex, 0);
	if (lResult != LB_ERR)
	{

		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;

		if (pState == NULL)
			pState = new AFX_CHECK_DATA;

		pState->m_nCheck = nCheck;
		VERIFY(DefWindowProc(LB_SETITEMDATA, nIndex, (LPARAM)pState) != LB_ERR);

		InvalidateCheck(nIndex);
	}
}

int CCheckListBox::GetCheck(int nIndex)
{
	ASSERT(::IsWindow(m_hWnd));

	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, nIndex, 0);
	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;
		if (pState != NULL)
			return pState->m_nCheck;
	}
	return 0; // The default
}

void CCheckListBox::Enable(int nIndex, BOOL bEnabled)
{
	ASSERT(::IsWindow(m_hWnd));

	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, nIndex, 0);
	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;

		if (pState == NULL)
			pState = new AFX_CHECK_DATA;

		pState->m_bEnabled = bEnabled;
		VERIFY(DefWindowProc(LB_SETITEMDATA, nIndex, (LPARAM)pState) != LB_ERR);

		InvalidateItem(nIndex);
	}
}

int CCheckListBox::IsEnabled(int nIndex)
{
	ASSERT(::IsWindow(m_hWnd));

	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, nIndex, 0);
	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;
		if (pState != NULL)
			return pState->m_bEnabled;
	}
	return TRUE; // The default
}

CRect CCheckListBox::OnGetCheckPosition(CRect, CRect rectCheckBox)
{
	return rectCheckBox;
}

void CCheckListBox::DrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	// You must override DrawItem and MeasureItem for LBS_OWNERDRAWVARIABLE
	ASSERT((GetStyle() & (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS)) ==
		(LBS_OWNERDRAWFIXED | LBS_HASSTRINGS));

	CDC* pDC = CDC::FromHandle(lpDrawItemStruct->hDC);

	if (((LONG)(lpDrawItemStruct->itemID) >= 0) &&
		(lpDrawItemStruct->itemAction & (ODA_DRAWENTIRE | ODA_SELECT)))
	{
		int cyItem = GetItemHeight(lpDrawItemStruct->itemID);
		BOOL fDisabled = !IsWindowEnabled() || !IsEnabled(lpDrawItemStruct->itemID);

		COLORREF newTextColor = fDisabled ?
			RGB(0x80, 0x80, 0x80) : GetSysColor(COLOR_WINDOWTEXT);  // light gray
		COLORREF oldTextColor = pDC->SetTextColor(newTextColor);

		COLORREF newBkColor = GetSysColor(COLOR_WINDOW);
		COLORREF oldBkColor = pDC->SetBkColor(newBkColor);

		if (newTextColor == newBkColor)
			newTextColor = RGB(0xC0, 0xC0, 0xC0);   // dark gray

		if (!fDisabled && ((lpDrawItemStruct->itemState & ODS_SELECTED) != 0))
		{
			pDC->SetTextColor(GetSysColor(COLOR_HIGHLIGHTTEXT));
			pDC->SetBkColor(GetSysColor(COLOR_HIGHLIGHT));
		}

		if (m_cyText == 0)
			VERIFY(cyItem >= CalcMinimumItemHeight());

		CString strText;
		GetText(lpDrawItemStruct->itemID, strText);

		pDC->ExtTextOut(lpDrawItemStruct->rcItem.left,
			lpDrawItemStruct->rcItem.top + max(0, (cyItem - m_cyText) / 2),
			ETO_OPAQUE, &(lpDrawItemStruct->rcItem), strText, strText.GetLength(), NULL);

		pDC->SetTextColor(oldTextColor);
		pDC->SetBkColor(oldBkColor);
	}

	if ((lpDrawItemStruct->itemAction & ODA_FOCUS) != 0)
		pDC->DrawFocusRect(&(lpDrawItemStruct->rcItem));
}

void CCheckListBox::MeasureItem(LPMEASUREITEMSTRUCT)
{
	// You must override DrawItem and MeasureItem for LBS_OWNERDRAWVARIABLE
	ASSERT((GetStyle() & (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS)) ==
		(LBS_OWNERDRAWFIXED | LBS_HASSTRINGS));
}

void CCheckListBox::PreDrawItem(LPDRAWITEMSTRUCT lpDrawItemStruct)
{
	_AFX_CHECKLIST_STATE* pChecklistState = _afxChecklistState;

	DRAWITEMSTRUCT drawItem;
	memcpy(&drawItem, lpDrawItemStruct, sizeof(DRAWITEMSTRUCT));

	if ((((LONG)drawItem.itemID) >= 0) &&
	   ((drawItem.itemAction & (ODA_DRAWENTIRE | ODA_SELECT)) != 0))
	{
		int cyItem = GetItemHeight(drawItem.itemID);

		CDC* pDC = CDC::FromHandle(drawItem.hDC);

		COLORREF newBkColor = GetSysColor(COLOR_WINDOW);

		BOOL fDisabled = !IsWindowEnabled() || !IsEnabled(drawItem.itemID);
		if ((drawItem.itemState & ODS_SELECTED) && !fDisabled)
			newBkColor = GetSysColor(COLOR_HIGHLIGHT);

		COLORREF oldBkColor = pDC->SetBkColor(newBkColor);

		CDC bitmapDC;
		if (bitmapDC.CreateCompatibleDC(pDC))
		{
			int nCheck = GetCheck(drawItem.itemID);
			HBITMAP hOldBitmap = (HBITMAP)::SelectObject(bitmapDC.m_hDC, pChecklistState->m_hbitmapCheck);

			CRect rectCheck = drawItem.rcItem;
			rectCheck.left += 1;
			rectCheck.top += 1 + max(0, (cyItem - pChecklistState->m_sizeCheck.cy) / 2);
			rectCheck.right = rectCheck.left + pChecklistState->m_sizeCheck.cx;
			rectCheck.bottom = rectCheck.top + pChecklistState->m_sizeCheck.cy;

			CRect rectItem = drawItem.rcItem;
			rectItem.right = rectItem.left + pChecklistState->m_sizeCheck.cx + 2;

			CRect rectCheckBox = OnGetCheckPosition(rectItem, rectCheck);

			ASSERT(rectCheck.IntersectRect(rectItem, rectCheckBox));
			ASSERT((rectCheck == rectCheckBox) && (rectCheckBox.Size() == pChecklistState->m_sizeCheck));

			CBrush brush(newBkColor);
			pDC->FillRect(rectItem, &brush);

			pDC->BitBlt(rectCheckBox.left, rectCheckBox.top,
				pChecklistState->m_sizeCheck.cx, pChecklistState->m_sizeCheck.cy, &bitmapDC,
				pChecklistState->m_sizeCheck.cx  * nCheck, 0, SRCCOPY);

			::SelectObject(bitmapDC.m_hDC, hOldBitmap);
		}
		pDC->SetBkColor(oldBkColor);
	}

	if (drawItem.itemData != 0 && drawItem.itemData != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)drawItem.itemData;
		drawItem.itemData = pState->m_dwUserData;
	}
	drawItem.rcItem.left = drawItem.rcItem.left + pChecklistState->m_sizeCheck.cx + 2;

	DrawItem(&drawItem);
}

void CCheckListBox::PreMeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct)
{
	int cyItem = CalcMinimumItemHeight();

	MEASUREITEMSTRUCT measureItem;
	memcpy(&measureItem, lpMeasureItemStruct, sizeof(MEASUREITEMSTRUCT));

	measureItem.itemHeight = cyItem;
	measureItem.itemWidth  = (UINT)-1;

	// WINBUG: Windows95 and Windows NT disagree on what this value
	// should be.  According to the docs, they are both wrong
	if (GetStyle() & LBS_OWNERDRAWVARIABLE)
	{
		LRESULT lResult = DefWindowProc(LB_GETITEMDATA, measureItem.itemID, 0);
		if (lResult != LB_ERR)
			measureItem.itemData = (UINT)lResult;
		else
			measureItem.itemData = 0;

		// WINBUG: This is only done in the LBS_OWNERDRAWVARIABLE case
		// because Windows 95 does not initialize itemData to zero in the
		// case of LBS_OWNERDRAWFIXED list boxes (it is stack garbage).
		if (measureItem.itemData != 0 && measureItem.itemData != LB_ERR)
		{
			AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)measureItem.itemData;
			measureItem.itemData = pState->m_dwUserData;
		}
	}

	MeasureItem(&measureItem);

	lpMeasureItemStruct->itemHeight = max(measureItem.itemHeight,(UINT) cyItem);
	lpMeasureItemStruct->itemWidth = measureItem.itemWidth;
}

int CCheckListBox::PreCompareItem(LPCOMPAREITEMSTRUCT lpCompareItemStruct)
{
	COMPAREITEMSTRUCT compareItem;
	memcpy(&compareItem, lpCompareItemStruct, sizeof(COMPAREITEMSTRUCT));

	if (compareItem.itemData1 != 0 && compareItem.itemData1 != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)compareItem.itemData1;
		compareItem.itemData1 = pState->m_dwUserData;
	}
	if (compareItem.itemData2 != 0 && compareItem.itemData2 != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)compareItem.itemData2;
		compareItem.itemData2 = pState->m_dwUserData;
	}
	return CompareItem(&compareItem);
}

void CCheckListBox::PreDeleteItem(LPDELETEITEMSTRUCT lpDeleteItemStruct)
{
	DELETEITEMSTRUCT deleteItem;
	memcpy(&deleteItem, lpDeleteItemStruct, sizeof(DELETEITEMSTRUCT));

	// WINBUG: The following if block is required because Windows NT
	// version 3.51 does not properly fill out the LPDELETEITEMSTRUCT.
	if (deleteItem.itemData == 0)
	{
		LRESULT lResult = DefWindowProc(LB_GETITEMDATA, deleteItem.itemID, 0);
		if (lResult != LB_ERR)
			deleteItem.itemData = (UINT)lResult;
	}

	if (deleteItem.itemData != 0 && deleteItem.itemData != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)deleteItem.itemData;
		deleteItem.itemData = pState->m_dwUserData;
		delete pState;
	}
	DeleteItem(&deleteItem);
}

BOOL CCheckListBox::OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam,
	LRESULT* pResult)
{
	switch (message)
	{
	case WM_DRAWITEM:
		ASSERT(pResult == NULL);       // no return value expected
		PreDrawItem((LPDRAWITEMSTRUCT)lParam);
		break;
	case WM_MEASUREITEM:
		ASSERT(pResult == NULL);       // no return value expected
		PreMeasureItem((LPMEASUREITEMSTRUCT)lParam);
		break;
	case WM_COMPAREITEM:
		ASSERT(pResult != NULL);       // return value expected
		*pResult = PreCompareItem((LPCOMPAREITEMSTRUCT)lParam);
		break;
	case WM_DELETEITEM:
		ASSERT(pResult == NULL);       // no return value expected
		PreDeleteItem((LPDELETEITEMSTRUCT)lParam);
		break;
	default:
		return CListBox::OnChildNotify(message, wParam, lParam, pResult);
	}
	return TRUE;
}

#ifdef _DEBUG
void CCheckListBox::PreSubclassWindow()
{
	CListBox::PreSubclassWindow();

	// CCheckListBoxes must be owner drawn
	ASSERT(GetStyle() & (LBS_OWNERDRAWFIXED | LBS_OWNERDRAWVARIABLE));
}
#endif

int CCheckListBox::CalcMinimumItemHeight()
{
	int nResult;

	_AFX_CHECKLIST_STATE* pChecklistState = _afxChecklistState;

	if ((GetStyle() & (LBS_HASSTRINGS | LBS_OWNERDRAWFIXED)) ==
		(LBS_HASSTRINGS | LBS_OWNERDRAWFIXED))
	{
		CClientDC dc(this);
		CFont* pOldFont = dc.SelectObject(GetFont());
		TEXTMETRIC tm;
		VERIFY (dc.GetTextMetrics ( &tm ));
		dc.SelectObject(pOldFont);

		m_cyText = tm.tmHeight;
		nResult = max(pChecklistState->m_sizeCheck.cy + 1, m_cyText);
	}
	else
	{
		nResult = pChecklistState->m_sizeCheck.cy + 1;
	}

	return nResult;
}

void CCheckListBox::InvalidateCheck(int nIndex)
{
	CRect rect;
	_AFX_CHECKLIST_STATE* pChecklistState = _afxChecklistState;

	GetItemRect(nIndex, rect);
	rect.right = rect.left + pChecklistState->m_sizeCheck.cx + 2;
	InvalidateRect(rect, FALSE);
}

void CCheckListBox::InvalidateItem(int nIndex)
{
	CRect rect;
	GetItemRect(nIndex, rect);
	InvalidateRect(rect, FALSE);
}

int CCheckListBox::CheckFromPoint(CPoint point, BOOL& bInCheck)
{
	// assume did not hit anything
	bInCheck = FALSE;
	int nIndex = -1;

	_AFX_CHECKLIST_STATE* pChecklistState = _afxChecklistState;
	if ((GetStyle() & (LBS_OWNERDRAWFIXED|LBS_MULTICOLUMN)) == LBS_OWNERDRAWFIXED)
	{
		// optimized case for ownerdraw fixed, single column
		int cyItem = GetItemHeight(0);
		if (point.y < cyItem * GetCount())
		{
			nIndex = GetTopIndex() + point.y / cyItem;
			if (point.x < pChecklistState->m_sizeCheck.cx + 2)
				++bInCheck;
		}
	}
	else
	{
		// general case for ownerdraw variable or multiple column
		for (int i = GetTopIndex(); i < GetCount(); i++)
		{
			CRect itemRect;
			GetItemRect(i, &itemRect);
			if (itemRect.PtInRect(point))
			{
				nIndex = i;
				if (point.x < itemRect.left + pChecklistState->m_sizeCheck.cx + 2)
					++bInCheck;
				break;
			}
		}
	}
	return nIndex;
}

void CCheckListBox::SetSelectionCheck( int nCheck )
{
   int* piSelectedItems;
   int nSelectedItems;
   int iSelectedItem;

   nSelectedItems = GetSelCount();
   if( nSelectedItems > 0 )
   {
	  piSelectedItems = (int*)_alloca( nSelectedItems*sizeof( int ) );
	  GetSelItems( nSelectedItems, piSelectedItems );
	  for( iSelectedItem = 0; iSelectedItem < nSelectedItems; iSelectedItem++ )
	  {
		 if( IsEnabled( piSelectedItems[iSelectedItem] ) )
		 {
			SetCheck( piSelectedItems[iSelectedItem], nCheck );
			InvalidateCheck( piSelectedItems[iSelectedItem] );
		 }
	  }
   }
}

void CCheckListBox::OnLButtonDown(UINT nFlags, CPoint point)
{
   SetFocus();

   // determine where the click is
   BOOL bInCheck;
   int nIndex = CheckFromPoint(point, bInCheck);

   // if the item is disabled, then eat the click
   if (!IsEnabled(nIndex))
	  return;

   if (m_nStyle != BS_CHECKBOX && m_nStyle != BS_3STATE)
   {
	  // toggle the check mark automatically if the check mark was hit
	  if (bInCheck)
	  {
		 CWnd* pParent = GetParent();
		 ASSERT_VALID( pParent );

			int nModulo = (m_nStyle == BS_AUTO3STATE) ? 3 : 2;
		 int nCheck;
		 int nNewCheck;

		 nCheck = GetCheck( nIndex );
		 nCheck = (nCheck == nModulo) ? nCheck-1 : nCheck;
		 nNewCheck = (nCheck+1)%nModulo;
		 SetCheck( nIndex, nNewCheck );
		 InvalidateCheck( nIndex );

		 if( (GetStyle()&(LBS_EXTENDEDSEL|LBS_MULTIPLESEL)) && GetSel(
			nIndex ) )
		 {
			// The listbox is a multi-select listbox, and the user clicked on
			// a selected check, so change the check on all of the selected
			// items.
			SetSelectionCheck( nNewCheck );
		 }
		 else
		 {
			CListBox::OnLButtonDown( nFlags, point );
		 }

		 // Inform parent of check
		 pParent->SendMessage( WM_COMMAND, MAKEWPARAM( GetDlgCtrlID(),
			CLBN_CHKCHANGE ), (LPARAM)m_hWnd );
		 return;
	  }
   }

	// do default listbox selection logic
   CListBox::OnLButtonDown( nFlags, point );
}

void CCheckListBox::OnLButtonDblClk(UINT nFlags, CPoint point)
{
	BOOL bInCheck;
	CheckFromPoint(point, bInCheck);

	if (bInCheck)
	{
		// Double and single clicks act the same on the check box!
		OnLButtonDown(nFlags, point);
		return;
	}

	CListBox::OnLButtonDblClk(nFlags, point);
}

void CCheckListBox::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	if (nChar == VK_SPACE)
	{
		int nIndex = GetCaretIndex();
		CWnd* pParent = GetParent();
		ASSERT_VALID(pParent);

		if (nIndex != LB_ERR)
		{
			if (m_nStyle != BS_CHECKBOX && m_nStyle != BS_3STATE)
			{
				if ((GetStyle() & LBS_MULTIPLESEL) != 0)
				{
					if (IsEnabled(nIndex))
					{
						BOOL bSelected = GetSel(nIndex);
						if (bSelected)
						{
							int nModulo = (m_nStyle == BS_AUTO3STATE) ? 3 : 2;
							int nCheck = GetCheck(nIndex);
							nCheck = (nCheck == nModulo) ? nCheck - 1 : nCheck;
							SetCheck(nIndex, (nCheck + 1) % nModulo);

							// Inform of check
							pParent->SendMessage(WM_COMMAND,
								MAKEWPARAM(GetDlgCtrlID(), CLBN_CHKCHANGE),
								(LPARAM)m_hWnd);
						}
						SetSel(nIndex, !bSelected);
					}
					else
						SetSel(nIndex, FALSE); // unselect disabled items

					return;
				}
				else
				{
					// If there is a selection, the space bar toggles that check,
					// all other keys are the same as a standard listbox.

					if (IsEnabled(nIndex))
					{
						int nModulo = (m_nStyle == BS_AUTO3STATE) ? 3 : 2;
						int nCheck = GetCheck(nIndex);
						nCheck = (nCheck == nModulo) ? nCheck - 1 : nCheck;
				  int nNewCheck = (nCheck+1)%nModulo;
						SetCheck(nIndex, nNewCheck);

						InvalidateCheck(nIndex);

				  if( GetStyle()&LBS_EXTENDEDSEL )
				  {
					 // The listbox is a multi-select listbox, and the user
					 // clicked on a selected check, so change the check on all
					 // of the selected items.
					 SetSelectionCheck( nNewCheck );
				  }

						// Inform of check
						pParent->SendMessage(WM_COMMAND,
							MAKEWPARAM(GetDlgCtrlID(), CLBN_CHKCHANGE),
							(LPARAM)m_hWnd);
					}
					else
						SetSel(nIndex, FALSE); // unselect disabled items

					return;
				}
			}
		}
	}
	CListBox::OnKeyDown(nChar, nRepCnt, nFlags);
}

int CCheckListBox::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CListBox::OnCreate(lpCreateStruct) == -1)
		return -1;

	if ((GetStyle() & (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS))
		== (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS))
		SetItemHeight(0, CalcMinimumItemHeight());

	return 0;
}

LRESULT CCheckListBox::OnSetFont(WPARAM , LPARAM)
{
	Default();

	if ((GetStyle() & (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS))
		== (LBS_OWNERDRAWFIXED | LBS_HASSTRINGS))
		SetItemHeight(0, CalcMinimumItemHeight());

	return 0;
}

LRESULT CCheckListBox::OnLBAddString(WPARAM wParam, LPARAM lParam)
{
	AFX_CHECK_DATA* pState = NULL;

	if (!(GetStyle() & LBS_HASSTRINGS))
	{
		pState = new AFX_CHECK_DATA;

		pState->m_dwUserData = lParam;
		lParam = (LPARAM)pState;
	}

	LRESULT lResult = DefWindowProc(LB_ADDSTRING, wParam, lParam);

	if (lResult == LB_ERR && pState != NULL)
		delete pState;

	return lResult;
}

LRESULT CCheckListBox::OnLBFindString(WPARAM wParam, LPARAM lParam)
{
	if (GetStyle() & LBS_HASSTRINGS)
		return DefWindowProc(LB_FINDSTRING, wParam, lParam);

	int nIndex = wParam;
	if (nIndex == -1) nIndex = 0;

	for(; nIndex < GetCount(); nIndex++)
		if ((DWORD)lParam == GetItemData(nIndex))
			return nIndex;

	return LB_ERR;
}

LRESULT CCheckListBox::OnLBFindStringExact(WPARAM wParam, LPARAM lParam)
{
	if (GetStyle() & (LBS_HASSTRINGS | LBS_SORT))
		return DefWindowProc(LB_FINDSTRINGEXACT, wParam, lParam);

	int nIndex = wParam;
	if (nIndex == -1) nIndex = 0;

	for(; nIndex < GetCount(); nIndex++)
		if ((DWORD)lParam == GetItemData(nIndex))
			return nIndex;

	return LB_ERR;
}

LRESULT CCheckListBox::OnLBGetItemData(WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, wParam, lParam);

	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;

		if (pState == NULL)
			return 0; // default

		lResult = pState->m_dwUserData;
	}
	return lResult;
}

LRESULT CCheckListBox::OnLBGetText(WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = DefWindowProc(LB_GETTEXT, wParam, lParam);

	if (GetStyle() & LBS_HASSTRINGS)
		return lResult;

	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lParam;

		if (pState != NULL)
			lParam = pState->m_dwUserData;
	}
	return lResult;
}

LRESULT CCheckListBox::OnLBInsertString(WPARAM wParam, LPARAM lParam)
{
	AFX_CHECK_DATA* pState = NULL;

	if (!(GetStyle() & LBS_HASSTRINGS))
	{
		pState = new AFX_CHECK_DATA;
		pState->m_dwUserData = lParam;
		lParam = (LPARAM)pState;
	}

	LRESULT lResult = DefWindowProc(LB_INSERTSTRING, wParam, lParam);

	if (lResult == LB_ERR && pState != NULL)
		delete pState;

	return lResult;
}

LRESULT CCheckListBox::OnLBSelectString(WPARAM wParam, LPARAM lParam)
{
	if (GetStyle() & LBS_HASSTRINGS)
		return DefWindowProc(LB_SELECTSTRING, wParam, lParam);

	int nIndex = wParam;
	if (nIndex == -1) nIndex = 0;

	for(; nIndex < GetCount(); nIndex++)
		if ((DWORD)lParam == GetItemData(nIndex))
		{
			SetCurSel(nIndex);
			return nIndex;
		}

	return LB_ERR;
}

LRESULT CCheckListBox::OnLBSetItemData(WPARAM wParam, LPARAM lParam)
{
	LRESULT lResult = DefWindowProc(LB_GETITEMDATA, wParam, 0);

	if (lResult != LB_ERR)
	{
		AFX_CHECK_DATA* pState = (AFX_CHECK_DATA*)lResult;

		if (pState == NULL)
			pState = new AFX_CHECK_DATA;

		pState->m_dwUserData = lParam;
		lResult = DefWindowProc(LB_SETITEMDATA, wParam, (LPARAM)pState);

		if (lResult == LB_ERR)
			delete pState;
	}
	return lResult;
}

LRESULT CCheckListBox::OnLBSetItemHeight(WPARAM wParam, LPARAM lParam)
{
	int nHeight = max(CalcMinimumItemHeight(),(int)LOWORD(lParam));
	return DefWindowProc(LB_SETITEMHEIGHT, wParam, MAKELPARAM(nHeight,0));
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CCheckListBox, CListBox)

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_CHECKLIST_STATE, _afxChecklistState)

/////////////////////////////////////////////////////////////////////////////
