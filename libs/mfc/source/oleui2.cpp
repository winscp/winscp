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

#ifdef AFX_OLE2_SEG
#pragma code_seg(AFX_OLE2_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// more User interface for COleClientItem

/////////////////////////////////////////////////////////////////////////////
// OLE Object Verb Menu helpers

// simple wrapper for OleUIAddVerbMenu API

void AFXAPI AfxOleSetEditMenu(COleClientItem* pItem, CMenu* pMenu,
	UINT iMenuItem, UINT nIDVerbMin, UINT nIDVerbMax, UINT nIDConvert)
{
	ASSERT_VALID(pMenu);
	if (pItem != NULL)
		ASSERT_VALID(pItem);

	HMENU hmenuDummy;
	if (!::OleUIAddVerbMenu(pItem != NULL ? pItem->m_lpObject : NULL,
		NULL, pMenu->GetSafeHmenu(), iMenuItem,
		nIDVerbMin, nIDVerbMax, nIDConvert != 0, nIDConvert, &hmenuDummy))
	{
		// turn gray popup into gray disabled normal menu item
		TCHAR szBuffer[256];
		pMenu->GetMenuString(iMenuItem, szBuffer, sizeof szBuffer, MF_BYPOSITION);
		pMenu->DeleteMenu(iMenuItem, MF_BYPOSITION);
		pMenu->InsertMenu(
			iMenuItem, MF_BYPOSITION|MF_STRING|MF_GRAYED|MF_DISABLED,
			nIDVerbMin, szBuffer);
	}
}

/////////////////////////////////////////////////////////////////////////////
// UI message handlers

void COleDocument::OnUpdatePasteMenu(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(COleClientItem::CanPaste());
}

void COleDocument::OnUpdatePasteLinkMenu(CCmdUI* pCmdUI)
{
	pCmdUI->Enable(COleClientItem::CanPasteLink());
}

void COleDocument::OnUpdateEditLinksMenu(CCmdUI* pCmdUI)
{
	POSITION pos = GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = GetNextClientItem(pos)) != NULL)
	{
		if (pItem->GetType() == OT_LINK)
		{
			// we found a link!
			pCmdUI->Enable(TRUE);
			return;
		}
	}
	pCmdUI->Enable(FALSE);      // no links today
}

void COleDocument::OnEditLinks()
{
	ASSERT_VALID(this);

	COleLinksDialog dlg(this, GetRoutingView_());
	dlg.DoModal();
}

void COleDocument::OnEditConvert()
{
	ASSERT_VALID(this);

	// get selected item
	COleClientItem* pItem = GetPrimarySelectedItem(GetRoutingView_());
	if (pItem == NULL)
		return;

	// do conversion dialog & convert for that item
	COleConvertDialog dlg(pItem);
	if (dlg.DoModal() == IDOK)
		dlg.DoConvert(pItem);
}

void COleDocument::OnUpdateEditChangeIcon(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);

	pCmdUI->Enable(GetPrimarySelectedItem(GetRoutingView_()) != NULL);
}

void COleDocument::OnEditChangeIcon()
{
	ASSERT_VALID(this);

	// get selected item
	COleClientItem* pItem = GetPrimarySelectedItem(GetRoutingView_());
	if (pItem == NULL)
		return;

	// do conversion dialog & convert for that item
	COleChangeIconDialog dlg(pItem);
	if (dlg.DoModal() == IDOK)
		dlg.DoChangeIcon(pItem);
}

void COleDocument::OnUpdateObjectVerbMenu(CCmdUI* pCmdUI)
{
	if (pCmdUI->m_pMenu == NULL || pCmdUI->m_pParentMenu == NULL)
	{
		// not a menu or is on sub-menu (don't recurse)
		pCmdUI->ContinueRouting();
		return;
	}

	// check for single selection
	COleClientItem* pItem = GetPrimarySelectedItem(GetRoutingView_());
	if (pItem == NULL || pItem->GetType() == OT_STATIC)
	{
		// no selection, or is 'static' item
		pCmdUI->Enable(FALSE);
	}

	// only include Convert... if there is a handler for ID_OLE_EDIT_CONVERT
	UINT nConvertID = ID_OLE_EDIT_CONVERT;
	AFX_CMDHANDLERINFO info;
	if (!OnCmdMsg(ID_OLE_EDIT_CONVERT, CN_COMMAND, NULL, &info))
		nConvertID = 0;

	// update the menu
	AfxOleSetEditMenu(GetPrimarySelectedItem(GetRoutingView_()),
		pCmdUI->m_pMenu, pCmdUI->m_nIndex,
		ID_OLE_VERB_FIRST, ID_OLE_VERB_LAST, nConvertID);
}

/////////////////////////////////////////////////////////////////////////////
