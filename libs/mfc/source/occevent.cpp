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

/////////////////////////////////////////////////////////////////////////////
// OLE event sink handler

typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_CHANGED)();
typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_REQUEST)(BOOL*);
typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_DSCNOTIFY)(DSCSTATE, DSCREASON, BOOL*);
typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_CHANGED_RANGE)(UINT);
typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_REQUEST_RANGE)(UINT, BOOL*);
typedef BOOL (AFX_MSG_CALL CCmdTarget::*PFN_DSCNOTIFY_RANGE)(UINT, DSCSTATE, DSCREASON, BOOL*);

BOOL CCmdTarget::OnEvent(UINT idCtrl, AFX_EVENT* pEvent,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	HRESULT hResult = S_OK;
	UINT uArgError = (UINT)-1;    // no error yet
	const AFX_EVENTSINKMAP_ENTRY* pEntry = GetEventSinkEntry(idCtrl, pEvent);

	// no handler for this event
	if (pEntry == NULL)
		return FALSE;

	if (pHandlerInfo != NULL)
	{
		// just fill in the information, don't do it
		pHandlerInfo->pTarget = this;
		switch (pEvent->m_eventKind)
		{
		case AFX_EVENT::event:
		case AFX_EVENT::propRequest:
			pHandlerInfo->pmf = pEntry->dispEntry.pfn;
			break;

		case AFX_EVENT::propChanged:
			pHandlerInfo->pmf = pEntry->dispEntry.pfnSet;
			break;

		default:
			ASSERT(FALSE);  // bogus value for pEvent->m_eventKind
		}

		return (pHandlerInfo->pmf != NULL);
	}

	BOOL bRange = (pEntry->nCtrlIDLast != (UINT)-1);
	BOOL bHandled = FALSE;

	TRY
	{
		switch (pEvent->m_eventKind)
		{
		case AFX_EVENT::event:
			// do standard method call
			VARIANT var;
			AfxVariantInit(&var);

			DISPPARAMS dispparams;
			dispparams.rgvarg = NULL;

			if (bRange)
			{
				memcpy(&dispparams, pEvent->m_pDispParams, sizeof(DISPPARAMS));
				dispparams.rgvarg = new VARIANT[++dispparams.cArgs];
				memcpy(dispparams.rgvarg, pEvent->m_pDispParams->rgvarg,
					sizeof(VARIANT) * (dispparams.cArgs-1));
				VARIANT* pvarID = &dispparams.rgvarg[dispparams.cArgs-1];
				V_VT(pvarID) = VT_I4;
				V_I4(pvarID) = idCtrl;
			}

			hResult = CallMemberFunc(&pEntry->dispEntry, DISPATCH_METHOD, &var,
				(bRange ? &dispparams : pEvent->m_pDispParams), &uArgError);
			ASSERT(FAILED(hResult) || (V_VT(&var) == VT_BOOL));
			bHandled = V_BOOL(&var);

			if (bRange)
				delete [] dispparams.rgvarg;

			break;

		case AFX_EVENT::propChanged:
			{
				if (bRange)
				{
					PFN_CHANGED_RANGE pfn = (PFN_CHANGED_RANGE)pEntry->dispEntry.pfnSet;
					bHandled = (this->*pfn)(idCtrl);
				}
				else
				{
					PFN_CHANGED pfn = (PFN_CHANGED)pEntry->dispEntry.pfnSet;
					bHandled = (this->*pfn)();
				}

				hResult = S_OK;
			}
			break;

		case AFX_EVENT::propRequest:
			{
				BOOL bAllow = TRUE;

				if (bRange)
				{
					PFN_REQUEST_RANGE pfn = (PFN_REQUEST_RANGE)pEntry->dispEntry.pfn;
					bHandled = (this->*pfn)(idCtrl, &bAllow);
				}
				else
				{
					PFN_REQUEST pfn = (PFN_REQUEST)pEntry->dispEntry.pfn;
					bHandled = (this->*pfn)(&bAllow);
				}

				hResult = bAllow ? S_OK : S_FALSE;
			}
			break;

		case AFX_EVENT::propDSCNotify:
			{
				BOOL bAllow = TRUE;

				if (bRange)
				{
					PFN_DSCNOTIFY_RANGE pfn = (PFN_DSCNOTIFY_RANGE)pEntry->dispEntry.pfn;
					bHandled = (this->*pfn)(idCtrl, pEvent->m_nDSCState,
						pEvent->m_nDSCReason, &bAllow);
				}
				else
				{
					PFN_DSCNOTIFY pfn = (PFN_DSCNOTIFY)pEntry->dispEntry.pfn;
					bHandled = (this->*pfn)(pEvent->m_nDSCState,
						pEvent->m_nDSCReason, &bAllow);
				}

				hResult = bAllow ? S_OK : S_FALSE;
			}
			break;

		default:
			ASSERT(FALSE);  // bogus value for pEvent->m_eventKind
		}
	}
	CATCH_ALL(e)
	{
		if (pEvent->m_pExcepInfo != NULL)
		{
			// fill exception with translation of MFC exception
			COleDispatchException::Process(pEvent->m_pExcepInfo, e);
		}
		DELETE_EXCEPTION(e);
		hResult = DISP_E_EXCEPTION;
	}
	END_CATCH_ALL

	// fill error argument if one is available
	if (FAILED(hResult) && pEvent->m_puArgError != NULL && uArgError != -1)
		*pEvent->m_puArgError = uArgError;

	// fill result code
	pEvent->m_hResult = hResult;

	return bHandled;
}

/////////////////////////////////////////////////////////////////////////////
// Locate event sink map entry

const AFX_EVENTSINKMAP_ENTRY* PASCAL CCmdTarget::GetEventSinkEntry(
	UINT idCtrl, AFX_EVENT* pEvent)
{
	const AFX_EVENTSINKMAP* pSinkMap = GetEventSinkMap();
	const AFX_EVENTSINKMAP_ENTRY* pEntry;
	size_t flag = (pEvent->m_eventKind != AFX_EVENT::event);

	while (pSinkMap != NULL)
	{
		// find matching AFX_EVENTSINKMAP_ENTRY
		pEntry = pSinkMap->lpEntries;
		while (pEntry->dispEntry.nPropOffset != -1)
		{
			if ((pEntry->dispEntry.lDispID == pEvent->m_dispid) &&
				(pEntry->dispEntry.nPropOffset == flag))
			{
				if (pEntry->nCtrlIDLast == (UINT)-1)
				{
					// check for wildcard match or exact match
					if ((pEntry->nCtrlIDFirst == (UINT)-1) ||
						(pEntry->nCtrlIDFirst == idCtrl))
						return pEntry;
				}
				else
				{
					// check for range match
					if ((pEntry->nCtrlIDFirst <= idCtrl) &&
						(idCtrl <= pEntry->nCtrlIDLast))
						return pEntry;
				}
			}

			++pEntry;
		}
		// check base class
#ifdef _AFXDLL
		pSinkMap = (*pSinkMap->pfnGetBaseMap)();
#else
		pSinkMap = pSinkMap->pBaseMap;
#endif
	}

	return NULL;    // no matching entry
}
