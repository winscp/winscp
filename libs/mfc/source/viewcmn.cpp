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
// CListView

BEGIN_MESSAGE_MAP(CListView, CCtrlView)
	//{{AFX_MSG_MAP(CListView)
	ON_WM_NCDESTROY()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CListView::PreCreateWindow(CREATESTRUCT& cs)
{
	return CCtrlView::PreCreateWindow(cs);
}

void CListView::DrawItem(LPDRAWITEMSTRUCT)
{
	ASSERT(FALSE);
}

BOOL CListView::OnChildNotify(UINT message, WPARAM wParam, LPARAM lParam,
	LRESULT* pResult)
{
	if (message != WM_DRAWITEM)
		return CCtrlView::OnChildNotify(message, wParam, lParam, pResult);

	ASSERT(pResult == NULL);       // no return value expected
	UNUSED(pResult); // unused in release builds

	DrawItem((LPDRAWITEMSTRUCT)lParam);
	return TRUE;
}

void CListView::RemoveImageList(int nImageList)
{
	HIMAGELIST h = (HIMAGELIST)SendMessage(LVM_GETIMAGELIST,
		(WPARAM)nImageList);
	if (CImageList::FromHandlePermanent(h) != NULL)
		SendMessage(LVM_SETIMAGELIST, (WPARAM)nImageList, NULL);
}

void CListView::OnNcDestroy()
{
	RemoveImageList(LVSIL_NORMAL);
	RemoveImageList(LVSIL_SMALL);
	RemoveImageList(LVSIL_STATE);

	CCtrlView::OnNcDestroy();
}

/////////////////////////////////////////////////////////////////////////////
// CTreeView

BEGIN_MESSAGE_MAP(CTreeView, CCtrlView)
	//{{AFX_MSG_MAP(CTreeView)
	ON_WM_DESTROY()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

BOOL CTreeView::PreCreateWindow(CREATESTRUCT& cs)
{
	return CCtrlView::PreCreateWindow(cs);
}

void CTreeView::RemoveImageList(int nImageList)
{
	HIMAGELIST h = (HIMAGELIST)SendMessage(TVM_GETIMAGELIST,
		(WPARAM)nImageList);
	if (CImageList::FromHandlePermanent(h) != NULL)
		SendMessage(TVM_SETIMAGELIST, (WPARAM)nImageList, NULL);
}

void CTreeView::OnDestroy()
{
	RemoveImageList(LVSIL_NORMAL);
	RemoveImageList(LVSIL_STATE);

	CCtrlView::OnDestroy();
}

/////////////////////////////////////////////////////////////////////////////

#ifndef _AFX_ENABLE_INLINES

static const char _szAfxWinInl[] = "afxcview.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxWinInl
#define _AFXCVIEW_INLINE
#include "afxcview.inl"

#endif //_AFX_ENABLE_INLINES

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNCREATE(CListView, CCtrlView)
IMPLEMENT_DYNCREATE(CTreeView, CCtrlView)

/////////////////////////////////////////////////////////////////////////////
