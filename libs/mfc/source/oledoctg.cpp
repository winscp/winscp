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

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleCmdUI class

COleCmdUI::COleCmdUI(OLECMD* rgCmds, ULONG cCmds, const GUID* pGroup)
{
	m_rgCmds = rgCmds;
	m_nIndexMax = cCmds;
	m_pguidCmdGroup = pGroup;
}

void COleCmdUI::Enable(BOOL bOn)
{
   if (m_rgCmds != NULL)
   {
	  ASSERT(m_nIndex < m_nIndexMax);

	  if (bOn)
		 m_rgCmds[m_nIndex].cmdf |= OLECMDF_ENABLED;
	  else
		 m_rgCmds[m_nIndex].cmdf &= ~OLECMDF_ENABLED;
	  m_bEnableChanged = TRUE;
   }
}

void COleCmdUI::SetCheck(int nCheck)
{
   if (m_rgCmds != NULL)
   {
	  ASSERT(m_nIndex < m_nIndexMax);

	  m_rgCmds[m_nIndex].cmdf &= ~(OLECMDF_LATCHED|OLECMDF_NINCHED);
	  if (nCheck == 1)
		 m_rgCmds[m_nIndex].cmdf |= OLECMDF_LATCHED;
	  else if (nCheck == 2)
		 m_rgCmds[m_nIndex].cmdf |= OLECMDF_NINCHED;
   }
}

void COleCmdUI::SetText(LPCTSTR lpszText)
{
	m_strText = lpszText;
}

BOOL COleCmdUI::DoUpdate(CCmdTarget* pTarget, BOOL bDisableIfNoHandler)
{
   BOOL bResult;
   ASSERT_VALID(pTarget);

	// fire off an OLECOMMNAD message to translate the OLECMD ID
	// to a real WM_COMMAND ID via the message maps
	// if we find a translation, fire a UPDATE_COMMAND_UI request to
	// see if the command should be enabled or not

   m_bEnableChanged = FALSE;
   bResult = pTarget->OnCmdMsg(m_nID, CN_OLECOMMAND, this, NULL);
   if (!bResult)
	  ASSERT(!m_bEnableChanged);
   else
	  bResult = pTarget->OnCmdMsg(m_nID, CN_UPDATE_COMMAND_UI, this, NULL);

   if (bDisableIfNoHandler && !m_bEnableChanged)
   {
	  AFX_CMDHANDLERINFO info;
	  info.pTarget = NULL;
	  bResult = pTarget->OnCmdMsg(m_nID, CN_COMMAND, this, &info);

	  if (bResult || m_bEnableChanged)
		 m_rgCmds[m_nIndex].cmdf |= OLECMDF_SUPPORTED;
	  else
		 m_rgCmds[m_nIndex].cmdf &= ~OLECMDF_SUPPORTED;
	  Enable(bResult);
   }
   else
   {
	  if (m_bEnableChanged)
		 m_rgCmds[m_nIndex].cmdf |= OLECMDF_SUPPORTED;
	  else
		 m_rgCmds[m_nIndex].cmdf &= ~OLECMDF_SUPPORTED;
   }
   return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// IOleCommandTarget implementation

STDMETHODIMP_(ULONG) CDocObjectServer::XOleCommandTarget::AddRef()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleCommandTarget)
	return pThis->m_pOwner->ExternalAddRef();
}

STDMETHODIMP_(ULONG) CDocObjectServer::XOleCommandTarget::Release()
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleCommandTarget)
	return pThis->m_pOwner->ExternalRelease();
}

STDMETHODIMP CDocObjectServer::XOleCommandTarget::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleCommandTarget)
	return pThis->m_pOwner->ExternalQueryInterface(&iid, ppvObj);
}

HRESULT AFXAPI _AfxQueryStatusOleCommandHelper(CCmdTarget* pTarget,
   const GUID* pguidCmdGroup, ULONG cCmds, OLECMD rgCmds[],
   OLECMDTEXT* pcmdtext)
{
	HRESULT hr = E_POINTER;

	if (rgCmds != NULL)
	{
		hr = NOERROR;
		if (pTarget == NULL)
		{
			ULONG nIndex;
			for (nIndex = 0; nIndex < cCmds; nIndex++)
				rgCmds[nIndex].cmdf = 0;
		}
		else
		{
			COleCmdUI state(rgCmds, cCmds, pguidCmdGroup);
			if (pcmdtext == NULL)
				state.m_nCmdTextFlag = 0;
			else
				state.m_nCmdTextFlag = pcmdtext->cmdtextf;
			for (state.m_nIndex = 0; state.m_nIndex < cCmds; state.m_nIndex++)
			{
				state.m_nID = rgCmds[state.m_nIndex].cmdID;
				state.DoUpdate(pTarget, TRUE);
			}

			if (pcmdtext != NULL && pcmdtext->rgwz != NULL &&
				(pcmdtext->cmdtextf != OLECMDTEXTF_NONE))
			{
				USES_CONVERSION;
				ASSERT(cCmds == 1);
				state.m_strText = state.m_strText.Right(pcmdtext->cwBuf-1);
				pcmdtext->cwActual = state.m_strText.GetLength();

#ifdef _UNICODE
				lstrcpyW(pcmdtext->rgwz, (LPCTSTR) state.m_strText);
#elif defined(OLE2ANSI)
				lstrcpy(pcmdtext->rgwz, state.m_strText);
#else
				lstrcpyW(pcmdtext->rgwz, T2W((LPCTSTR) state.m_strText));
#endif
			}
		}
	}

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleCommandTarget::QueryStatus(
   const GUID* pguidCmdGroup, ULONG cCmds, OLECMD rgCmds[],
   OLECMDTEXT* pcmdtext)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleCommandTarget)
	ASSERT_VALID(pThis);

	COleDocIPFrameWnd* pFrame = pThis->GetControllingFrame();
	return _AfxQueryStatusOleCommandHelper(pFrame,
		pguidCmdGroup, cCmds, rgCmds, pcmdtext);
}

HRESULT AFXAPI _AfxExecOleCommandHelper(CCmdTarget* pTarget,
   const GUID* pguidCmdGroup, DWORD nCmdID, DWORD nCmdExecOpt,
   VARIANTARG* pvarargIn, VARIANTARG* pvarargOut)
{
	HRESULT hr = OLECMDERR_E_NOHELP;

	UNUSED(pvarargIn);
	UNUSED(pvarargOut);
#ifdef _DEBUG
	// MFC doesn't support commands with arguments
	// You must handle argument commands by overriding OnExecOleCmd()

	if (pvarargIn != NULL || pvarargOut != NULL)
		TRACE1("Warning: IOleCommandTarget::Exec() received parameterized command #%d\n", nCmdID);
#endif

	if (pTarget != NULL)
	{
		OLECMD cmd;

		COleCmdUI state(&cmd, 1, pguidCmdGroup);
		state.m_nIndex = 0;
		cmd.cmdf = 0;
		cmd.cmdID = nCmdID;
		state.m_nID = nCmdID;

		// help via Doc Object targeting is not supported

		if (nCmdExecOpt == OLECMDEXECOPT_SHOWHELP)
			hr = OLECMDERR_E_DISABLED;
		else
		{
			// is the command supported?

			if (!state.DoUpdate(pTarget, TRUE))
				hr = OLECMDERR_E_NOTSUPPORTED;
			else
			{
				if (cmd.cmdf & OLECMDF_ENABLED)
				{
					if (pTarget->OnCmdMsg(state.m_nID, CN_COMMAND, NULL, NULL))
						hr = S_OK;
					else
						hr = E_FAIL;
				}
				else
					hr = OLECMDERR_E_DISABLED;
			}
		}
	}

	return hr;
}

STDMETHODIMP CDocObjectServer::XOleCommandTarget::Exec(
   const GUID* pguidCmdGroup, DWORD nCmdID, DWORD nCmdExecOpt,
   VARIANTARG* pvarargIn, VARIANTARG* pvarargOut)
{
	METHOD_PROLOGUE_EX(CDocObjectServer, OleCommandTarget)
	ASSERT_VALID(pThis);

	// Offer the command to the document, first

	HRESULT hr = pThis->OnExecOleCmd(pguidCmdGroup, nCmdID,
			nCmdExecOpt, pvarargIn, pvarargOut);
	if (hr == E_NOTIMPL)
	{
		COleDocIPFrameWnd* pFrame = pThis->GetControllingFrame();
		hr = _AfxExecOleCommandHelper(pFrame,
			pguidCmdGroup, nCmdID, nCmdExecOpt, pvarargIn, pvarargOut);
	}

	return hr;
}
