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

#ifdef AFXCTL_OLE_SEG
#pragma code_seg(AFXCTL_OLE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#ifndef _AFX_NO_OLE_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// Signature for verb functions

typedef BOOL (AFX_MSG_CALL CCmdTarget::*AFX_PVERBFN)(LPMSG, HWND, LPCRECT);

/////////////////////////////////////////////////////////////////////////////
// CEnumOleVerb - enumerator for OLEVERB

class CEnumOleVerb : public CEnumArray
{
public:
	CEnumOleVerb(const void* pvEnum, UINT nSize) :
		CEnumArray(sizeof(OLEVERB), pvEnum, nSize, TRUE) {}
	~CEnumOleVerb();

protected:
	virtual BOOL OnNext(void* pv);

	DECLARE_INTERFACE_MAP()
};

BEGIN_INTERFACE_MAP(CEnumOleVerb, CEnumArray)
	INTERFACE_PART(CEnumOleVerb, IID_IEnumOLEVERB, EnumVOID)
END_INTERFACE_MAP()

CEnumOleVerb::~CEnumOleVerb()
{
	if (m_pClonedFrom == NULL)
	{
		UINT iVerb;
		LPOLEVERB lpVerb = (LPOLEVERB)(void*)m_pvEnum;
		for (iVerb = 0; iVerb < m_nSize; iVerb++)
			CoTaskMemFree(lpVerb[iVerb].lpszVerbName);
	}
	// destructor will free the actual array (if it was not a clone)
}

BOOL CEnumOleVerb::OnNext(void* pv)
{
	if (!CEnumArray::OnNext(pv))
		return FALSE;

	// outgoing OLEVERB requires the verb name to be copied
	//  (the caller has responsibility to free it)

	LPOLEVERB lpVerb = (LPOLEVERB)pv;
	if (lpVerb->lpszVerbName != NULL)
	{
		lpVerb->lpszVerbName = AfxAllocTaskOleString(lpVerb->lpszVerbName);
		if (lpVerb->lpszVerbName == NULL)
			AfxThrowMemoryException();
	}
	// otherwise, copying worked...
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CCmdTarget::EnumOleVerbs - implementation for IOleObject::EnumVerbs

BOOL CCmdTarget::EnumOleVerbs(LPENUMOLEVERB* ppenumOleVerb)
{
	LPOLEVERB lpVerbList = NULL;
	LPOLEVERB lpVerbListNew;
	LPOLEVERB lpVerb;
	long nVerbs = 0;
	long nAlloc = 0;
	CString strVerbName;

	// walk the chain of message maps
	const AFX_MSGMAP* pMessageMap;
	const AFX_MSGMAP_ENTRY* lpEntry;

	for (pMessageMap = GetMessageMap();
		 (pMessageMap != NULL);
#ifdef _AFXDLL
		 pMessageMap = pMessageMap->pfnGetBaseMap())
#else
		 pMessageMap = pMessageMap->pBaseMap)
#endif
	{
		// find all verb entries in the map that have non-negative IDs
		lpEntry = pMessageMap->lpEntries;
		while ((lpEntry = AfxFindMessageEntry(lpEntry, 0xC002, 0, 1)) != NULL)
		{
			ASSERT(lpEntry != NULL);

			if (nVerbs == nAlloc)
			{
				// not enough space for new item -- allocate more
				lpVerbListNew = new OLEVERB[nVerbs + 10];
				nAlloc += 10;
				memcpy(lpVerbListNew, lpVerbList, (size_t)(nVerbs *
					sizeof(OLEVERB)));
				delete [] lpVerbList;
				lpVerbList = lpVerbListNew;
			}

			// get the string for this item
			if (!strVerbName.LoadString(lpEntry->nSig))
				strVerbName = _T("<unknown verb>");     // LoadString failed
			ASSERT(strVerbName.GetLength() > 0);

			// add this item to the list
			ASSERT(nVerbs < nAlloc);
			lpVerb = &lpVerbList[nVerbs];
			lpVerb->lVerb = nVerbs;
			lpVerb->lpszVerbName = AfxAllocTaskOleString(strVerbName);
			lpVerb->fuFlags = 0;
			lpVerb->grfAttribs = OLEVERBATTRIB_ONCONTAINERMENU;
			++nVerbs;
			++lpEntry;
		}
	}

	if (nVerbs > 0)
	{
		// create and return the IEnumOLEVERB object
		CEnumOleVerb* pEnum = new CEnumOleVerb(lpVerbList, (UINT)nVerbs);
		*ppenumOleVerb = (IEnumOLEVERB*)&pEnum->m_xEnumVOID;
	}
	else
	{
		// no verbs: return NULL
		*ppenumOleVerb = NULL;
	}

	return (nVerbs > 0);
}

/////////////////////////////////////////////////////////////////////////////
// CCmdTarget::DoOleVerb - implementation for IOleObject::DoVerb

BOOL CCmdTarget::DoOleVerb(LONG iVerb, LPMSG lpMsg, HWND hWndParent,
	LPCRECT lpRect)
{
	const AFX_MSGMAP* pMessageMap;
	const AFX_MSGMAP_ENTRY* lpEntry = NULL;
	long i = -1;

	for (pMessageMap = GetMessageMap();
		 (pMessageMap != NULL) && (lpEntry == NULL);
#ifdef _AFXDLL
		 pMessageMap = pMessageMap->pfnGetBaseMap())
#else
		 pMessageMap = pMessageMap->pBaseMap)
#endif
	{
		if (iVerb < 0)  // Standard verb (negative index)
		{
			lpEntry = AfxFindMessageEntry(pMessageMap->lpEntries, 0xC002, 0,
				(UINT)iVerb);
		}
		else            // Non-standard verb (non-negative index)
		{
			lpEntry = pMessageMap->lpEntries;
			while (((lpEntry = AfxFindMessageEntry(lpEntry, 0xC002, 0, 1)) !=
					NULL) && (++i < iVerb))
			{
				++lpEntry;
			}
			ASSERT((lpEntry == NULL) || (i == iVerb));
		}
	}

	if (lpEntry == NULL)
		return FALSE;

	AFX_PVERBFN pfn = (AFX_PVERBFN)(lpEntry->pfn);
	if (!(this->*pfn)(lpMsg, hWndParent, lpRect))
		THROW (new CException);

	return TRUE;
}

#endif //!_AFX_NO_OLE_SUPPORT

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
