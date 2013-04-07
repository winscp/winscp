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

#ifdef AFXCTL_PAGE_SEG
#pragma code_seg(AFXCTL_PAGE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CStockPropPage implementation

BEGIN_MESSAGE_MAP(CStockPropPage, COlePropertyPage)
END_MESSAGE_MAP()

void _AfxSizeComboToContent(CComboBox* pCombo)
{
	ASSERT_VALID(pCombo);
	int cyEdit = (int)(pCombo->SendMessage(CB_GETITEMHEIGHT, (WPARAM)-1, 0L));
	int cyItem = (int)(pCombo->SendMessage(CB_GETITEMHEIGHT, (WPARAM)0, 0L));

	CRect rcCombo;
	pCombo->GetWindowRect(&rcCombo);
	pCombo->SetWindowPos(NULL, 0, 0, rcCombo.Width(), cyEdit +
		pCombo->GetCount() * cyItem + GetSystemMetrics(SM_CYBORDER) * 8,
		SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOZORDER);
}

CStockPropPage::CStockPropPage(UINT idDlg, UINT idCaption) :
	COlePropertyPage(idDlg, idCaption)
{
	m_lcid = 0;
}

void CStockPropPage::FillPropnameList(REFGUID guid, int nIndirect, CComboBox& combo)
{
	USES_CONVERSION;

	combo.ResetContent();
	UINT cProps = 0;

	ULONG nObjects;
	LPDISPATCH* ppDisp = GetObjectArray(&nObjects);

	if (ppDisp != NULL)
	{
		LPTYPEINFO pTypeInfo;
		LPTYPEATTR pTypeAttr;
		LPVARDESC pVarDesc;
		ULONG iObj;
		WORD iProp;
		BSTR rgbstr[1];
		UINT cName;

		// Get the property sheet locale
		LPPROPERTYPAGESITE pPageSite;
		if ((pPageSite = GetPageSite()) != NULL)
			if (FAILED(pPageSite->GetLocaleID(&m_lcid)))
				m_lcid = 0;

		// Iterate through all objects.

		for (iObj = 0; iObj < nObjects; iObj++)
		{
			pTypeInfo = NULL;
			if ((ppDisp[iObj] != NULL) &&
				SUCCEEDED(ppDisp[iObj]->GetTypeInfo(0, m_lcid, &pTypeInfo)))
			{
				ASSERT(pTypeInfo != NULL);
				pTypeAttr = NULL;
				if (SUCCEEDED(pTypeInfo->GetTypeAttr(&pTypeAttr)))
				{
					ASSERT(pTypeAttr != NULL);

					// Iterate through all properties of object.

					for (iProp = 0; iProp < pTypeAttr->cVars; iProp++)
					{
						pVarDesc = NULL;
						if (SUCCEEDED(pTypeInfo->GetVarDesc(iProp, &pVarDesc)))
						{
							// Check whether property has desired type

							if (!(pVarDesc->wVarFlags & VARFLAG_FHIDDEN) &&
								AfxOleTypeMatchGuid(pTypeInfo,
									&pVarDesc->elemdescVar.tdesc, guid,
									nIndirect))
							{
								// Get property name and insert into list.

								if (SUCCEEDED(pTypeInfo->GetNames(
										pVarDesc->memid, rgbstr, 1, &cName)))
								{
									// Don't insert duplicates.

									LPCTSTR lpstr = OLE2CT(rgbstr[0]);
									if (combo.FindString(-1, lpstr)
											== CB_ERR)
									{
										int iItem = combo.AddString(lpstr);
										if (iItem >= 0)
										{
											combo.SetItemData(iItem, (DWORD)pVarDesc->memid);
											++cProps;
										}
									}

									SysFreeString(rgbstr[0]);
								}
							}
							pTypeInfo->ReleaseVarDesc(pVarDesc);
						}
					}

					pTypeInfo->ReleaseTypeAttr(pTypeAttr);
				}
				pTypeInfo->Release();
			}
		}
	}

	// Select the first one
	m_iPropName = 0;            // Prevents save from happening
	if (combo.SetCurSel(0) != CB_ERR)
		combo.GetLBText(0, m_strPropName);

	// Disable or set the size of the combo, as appropriate
	if (cProps <= 1)
		combo.EnableWindow(FALSE);
	else
		_AfxSizeComboToContent(&combo);

	UpdateData(FALSE);
	SetModifiedFlag(FALSE);
}

void CStockPropPage::OnSelchangePropname(CComboBox& combo)
{
	int iPropNameNew = combo.GetCurSel();

	if (iPropNameNew != m_iPropName)
	{
		UpdateData(TRUE);

		if (iPropNameNew != CB_ERR)
			combo.GetLBText(iPropNameNew, m_strPropName);
		else
			m_strPropName = _T("");

		m_iPropName = iPropNameNew;
		UpdateData(FALSE);
		SetModifiedFlag(FALSE);
	}
}

BOOL CStockPropPage::OnEditProperty(DISPID dispid, CComboBox& combo)
{
	int cItems = combo.GetCount();
	int i;

	for (i = 0; i < cItems; i++)
	{
		if ((DISPID)(combo.GetItemData(i)) == dispid)
		{
			combo.SetCurSel(i);
			OnSelchangePropname(combo);
			return TRUE;
		}
	}

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// AfxOleTypeMatchGuid:  Tests whether a given TYPEDESC matches a type with a
// given GUID, when all aliases have been expanded.

BOOL AFXAPI AfxOleTypeMatchGuid(
		LPTYPEINFO pTypeInfo,
		TYPEDESC* pTypeDesc,
		REFGUID guidType,
		ULONG cIndirectionLevels)
{
	ASSERT(pTypeInfo != NULL);
	ASSERT(pTypeDesc != NULL);
	ASSERT(cIndirectionLevels >= 0);

	LPTYPEINFO pTypeInfoRef = NULL;

	BOOL bMatch = FALSE;

	switch (pTypeDesc->vt)
	{
	case VT_USERDEFINED:
		// It's an alias: Expand the alias and try to match.
		if (SUCCEEDED(pTypeInfo->GetRefTypeInfo(
									pTypeDesc->hreftype,
									&pTypeInfoRef)))
		{
			ASSERT(pTypeInfoRef != NULL);
			LPTYPEATTR pTypeAttr = NULL;
			if (SUCCEEDED(pTypeInfoRef->GetTypeAttr(&pTypeAttr)))
			{
				ASSERT(pTypeAttr != NULL);

				// If we've dereferenced the correct number of times,
				// test the GUIDs for equality.
				if (cIndirectionLevels == 0)
					bMatch = IsEqualGUID(pTypeAttr->guid, guidType);

				if (!bMatch && pTypeAttr->typekind == TKIND_ALIAS)
				{
					// GUIDs didn't match, but type expanded to another alias!
					bMatch = AfxOleTypeMatchGuid(pTypeInfoRef,
									&pTypeAttr->tdescAlias, guidType,
									cIndirectionLevels);
				}

				pTypeInfoRef->ReleaseTypeAttr(pTypeAttr);
			}

			pTypeInfoRef->Release();
		}
		break;

	case VT_PTR:
		// It's a pointer: Dereference and try to match with one less level
		// of indirection.
		ASSERT(pTypeDesc->lptdesc != NULL);
		bMatch = AfxOleTypeMatchGuid(pTypeInfo, pTypeDesc->lptdesc, guidType,
						cIndirectionLevels - 1);
		break;
	}

	return bMatch;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CStockPropPage, COlePropertyPage)
