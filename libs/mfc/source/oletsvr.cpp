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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleTemplateServer

COleTemplateServer::COleTemplateServer()
	: COleObjectFactory(CLSID_NULL, NULL, FALSE, NULL)
{
	m_pDocTemplate = NULL;
}

BOOL COleTemplateServer::Register()
{
	return COleObjectFactory::Register();
}

BOOL COleTemplateServer::OnCmdMsg(UINT nID, int nCode, void* pExtra,
	AFX_CMDHANDLERINFO* pHandlerInfo)
{
	BOOL bReturn;
	if (nCode == CN_OLE_UNREGISTER)
		bReturn = Unregister();
	else
		bReturn = COleObjectFactory::OnCmdMsg(nID, nCode, pExtra, pHandlerInfo);

	return bReturn;
}

BOOL COleTemplateServer::Unregister()
{
	BOOL bReturn = COleObjectFactory::Unregister();
	if (!bReturn)
		return FALSE;

	// get registration info from doc template string
	CString strServerName;
	CString strLocalServerName;
	CString strLocalShortName;

	if (!m_pDocTemplate->GetDocString(strServerName,
	   CDocTemplate::regFileTypeId) || strServerName.IsEmpty())
	{
		TRACE0("Error: not enough information in DocTemplate to unregister OLE server.\n");
		return FALSE;
	}
	if (!m_pDocTemplate->GetDocString(strLocalServerName,
	   CDocTemplate::regFileTypeName))
		strLocalServerName = strServerName;     // use non-localized name
	if (!m_pDocTemplate->GetDocString(strLocalShortName,
		CDocTemplate::fileNewName))
		strLocalShortName = strLocalServerName; // use long name

	ASSERT(strServerName.Find(' ') == -1);  // no spaces allowed

	// place entries in system registry
	if (!AfxOleUnregisterServerClass(m_clsid, strServerName,
		strLocalShortName, strLocalServerName, (OLE_APPTYPE) m_bOAT))
	{
		bReturn = FALSE;
	}

	return bReturn;
}

void COleTemplateServer::ConnectTemplate(
	REFCLSID clsid, CDocTemplate* pDocTemplate, BOOL bMultiInstance)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pDocTemplate);
	ASSERT(pDocTemplate->m_pAttachedFactory == NULL);

	// setup initial state of underlying COleObjectFactory
	m_clsid = clsid;
	ASSERT(m_pRuntimeClass == NULL);
	m_bMultiInstance = bMultiInstance;

	// attach the doc template to the factory
	m_pDocTemplate = pDocTemplate;
	m_pDocTemplate->m_pAttachedFactory = this;
}

void COleTemplateServer::UpdateRegistry(OLE_APPTYPE nAppType,
	LPCTSTR* rglpszRegister, LPCTSTR* rglpszOverwrite)
{
	ASSERT(m_pDocTemplate != NULL);
	ASSERT(m_bOAT == (BYTE) OAT_UNKNOWN);
	m_bOAT = (BYTE) nAppType;

	// get registration info from doc template string
	CString strServerName;
	CString strLocalServerName;
	CString strLocalShortName;
	CString strLocalFilterName;
	CString strLocalFilterExt;

	if (!m_pDocTemplate->GetDocString(strServerName,
	   CDocTemplate::regFileTypeId) || strServerName.IsEmpty())
	{
		TRACE0("Error: not enough information in DocTemplate to register OLE server.\n");
		return;
	}
	if (!m_pDocTemplate->GetDocString(strLocalServerName,
	   CDocTemplate::regFileTypeName))
		strLocalServerName = strServerName;     // use non-localized name
	if (!m_pDocTemplate->GetDocString(strLocalShortName,
		CDocTemplate::fileNewName))
		strLocalShortName = strLocalServerName; // use long name
	if (!m_pDocTemplate->GetDocString(strLocalFilterName,
		CDocTemplate::filterName))
		ASSERT(nAppType != OAT_DOC_OBJECT_SERVER);
	if (!m_pDocTemplate->GetDocString(strLocalFilterExt,
		CDocTemplate::filterExt))
		ASSERT(nAppType != OAT_DOC_OBJECT_SERVER);

	ASSERT(strServerName.Find(' ') == -1);  // no spaces allowed

	int nIconIndex = 0;
	POSITION pos = AfxGetApp()->GetFirstDocTemplatePosition();
	for (int nIndex = 1; pos != NULL; nIndex++)
	{
		CDocTemplate* pTemplate = AfxGetApp()->GetNextDocTemplate(pos);
		if (pTemplate == m_pDocTemplate)
		{
			nIconIndex = nIndex;
			pos = NULL; // set exit condition
		}
	}

	// place entries in system registry
	if (!AfxOleRegisterServerClass(m_clsid, strServerName,
		strLocalShortName, strLocalServerName, nAppType,
		rglpszRegister, rglpszOverwrite, nIconIndex,
		strLocalFilterName, strLocalFilterExt))
	{
		// not fatal (don't fail just warn)
		AfxMessageBox(AFX_IDP_FAILED_TO_AUTO_REGISTER);
	}
}

CCmdTarget* COleTemplateServer::OnCreateObject()
{
	ASSERT_VALID(this);
	ASSERT_VALID(m_pDocTemplate);

	// save application user control status
	BOOL bUserCtrl = AfxOleGetUserCtrl();

	// create invisible doc/view/frame set
	CDocument* pDoc = m_pDocTemplate->OpenDocumentFile(NULL, FALSE);

	// restore application's user control status
	AfxOleSetUserCtrl(bUserCtrl);

	if (pDoc != NULL)
	{
		ASSERT_VALID(pDoc);
		ASSERT_KINDOF(CDocument, pDoc);

		// all new documents created by OLE start out modified
		pDoc->SetModifiedFlag();
	}
	return pDoc;
}

/////////////////////////////////////////////////////////////////////////////
