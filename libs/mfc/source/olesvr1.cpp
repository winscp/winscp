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
#include <shellapi.h>

#ifdef AFX_OLE4_SEG
#pragma code_seg(AFX_OLE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

//////////////////////////////////////////////////////////////////////////////
// COleServerDoc implementation

COleServerDoc::COleServerDoc()
{
	m_lpClientSite = NULL;  // will be non-NULL when document is embedding
	m_bCntrVisible = FALSE;
	m_pInPlaceFrame = NULL; // will be non-NULL when in-place active
	m_pOrigParent = NULL;
	m_dwOrigStyle = 0;
	m_dwOrigStyleEx = 0;
	m_bClosing = FALSE;
	m_pEmbeddedItem = NULL; // will be non-NULL if embedded item needed
	m_pDocObjectServer = NULL;      // becomes non-NULL if DocObject support enabled
}

COleServerDoc::~COleServerDoc()
{
	DeleteContents();   // Note: will not call derived class

	if (m_pEmbeddedItem != NULL)
	{
		m_pEmbeddedItem->ExternalRelease();
		m_pEmbeddedItem = NULL;
	}

	// disconnect (remove) all items from the document
	POSITION pos = GetStartPosition();
	COleServerItem* pItem;
	while ((pItem = GetNextServerItem(pos)) != NULL)
		RemoveItem(pItem);

	// release doc object manager, if any
	if (m_pDocObjectServer != NULL)
	{
		delete m_pDocObjectServer;
		m_pDocObjectServer = NULL;
	}

	// should not be in-place active when doc is destroyed!
	ASSERT(m_pInPlaceFrame == NULL);

	// Note: this must be done before the client site is released
	RELEASE(m_lpRootStg);

	// release client-site pointer
	RELEASE(m_lpClientSite);
}

void COleServerDoc::DeleteContents()
{
	COleLinkingDoc::DeleteContents();

	// protect all server items with an extra reference count
	POSITION pos = GetStartPosition();
	COleServerItem* pItem;
	while ((pItem = GetNextServerItem(pos)) != NULL)
		pItem->InternalAddRef();

	// delete any autodelete server items
	pos = GetStartPosition();
	while ((pItem = GetNextServerItem(pos)) != NULL)
	{
		if (pItem->m_bAutoDelete)
			delete pItem;
	}

	// remove extra reference added above
	pos = GetStartPosition();
	while ((pItem = GetNextServerItem(pos)) != NULL)
		pItem->InternalRelease();
}

COleServerItem* COleServerDoc::GetEmbeddedItem()
{
	// allocate embedded item if necessary
	if (m_pEmbeddedItem == NULL)
	{
		m_pEmbeddedItem = OnGetEmbeddedItem();
		m_pEmbeddedItem->ExternalAddRef();
	}

	ASSERT_VALID(m_pEmbeddedItem);
	return m_pEmbeddedItem;
}

CDocObjectServer* COleServerDoc::GetDocObjectServer(LPOLEDOCUMENTSITE pDocSite)
{
	// by default, we're not DocObject enabled
	UNUSED_ALWAYS(pDocSite);
	return NULL;
}

HRESULT COleServerDoc::OnExecOleCmd(const GUID* pguidCmdGroup, DWORD nCmdID,
		DWORD nCmdExecOpt, VARIANTARG* pvarargIn, VARIANTARG* pvarargOut)
{
	UNUSED_ALWAYS(pguidCmdGroup);
	UNUSED_ALWAYS(nCmdID);
	UNUSED_ALWAYS(nCmdExecOpt);
	UNUSED_ALWAYS(pvarargIn);
	UNUSED_ALWAYS(pvarargOut);

	return E_NOTIMPL;
}

LPUNKNOWN COleServerDoc::GetInterfaceHook(const void* piid)
{
	LPUNKNOWN lpUnk = NULL;

	// are we Doc Object supporters?
	if (m_pDocObjectServer != NULL)
	{
		// don't be tricked into handing out a different IUnknown
		DWORD lData1 = ((IID*)piid)->Data1;
		BOOL bUnknown = ((DWORD*)&IID_IUnknown)[0] == lData1 &&
			((DWORD*)piid)[1] == ((DWORD*)&IID_IUnknown)[1] &&
			((DWORD*)piid)[2] == ((DWORD*)&IID_IUnknown)[2] &&
			((DWORD*)piid)[3] == ((DWORD*)&IID_IUnknown)[3];

		if (bUnknown)
			return NULL;

		lpUnk = m_pDocObjectServer->GetInterface(piid);
	}

	// failing that, try the base class
	if (lpUnk == NULL)
		lpUnk = COleLinkingDoc::GetInterfaceHook(piid);

	return lpUnk;
}

void COleServerDoc::ActivateDocObject()
{
	if (m_pDocObjectServer != NULL)
		m_pDocObjectServer->ActivateDocObject();
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc client notifications

void COleServerDoc::NotifyRename(LPCTSTR lpszNewName)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszNewName));

	if (m_pFactory != NULL)
	{
		// update running object table with new moniker
		Revoke();
		Register(m_pFactory, lpszNewName);

		// notify all items of the new moniker
		POSITION pos = GetStartPosition();
		COleServerItem* pItem;
		while ((pItem = GetNextServerItem(pos)) != NULL)
		{
			// notify client directly
			LPMONIKER lpMoniker = pItem->GetMoniker(OLEGETMONIKER_ONLYIFTHERE);
			pItem->NotifyClient(OLE_RENAMED, (DWORD)lpMoniker);
			RELEASE(lpMoniker);
		}
	}
}

void COleServerDoc::NotifyAllItems(OLE_NOTIFICATION nCode, DWORD dwParam)
{
	ASSERT_VALID(this);

	POSITION pos = GetStartPosition();
	COleServerItem* pItem;
	while ((pItem = GetNextServerItem(pos)) != NULL)
	{
		// notify client directly
		pItem->NotifyClient(nCode, dwParam);
	}
}

// UpdateAllItems is much like UpdateAllViews, but for server items
void COleServerDoc::UpdateAllItems(COleServerItem* pSender,
	LPARAM lHint, CObject* pHint, DVASPECT nDrawAspect)
{
	ASSERT_VALID(this);

	POSITION pos = GetStartPosition();
	COleServerItem* pItem;
	while ((pItem = GetNextServerItem(pos)) != NULL)
	{
		// notify client indirectly through OnUpdate
		if (pItem != pSender)
			pItem->OnUpdate(pSender, lHint, pHint, nDrawAspect);
	}
}

void COleServerItem::OnUpdate(COleServerItem* /*pSender*/,
	LPARAM /*lHint*/, CObject* /*pHint*/, DVASPECT nDrawAspect)
{
	// the default implementation always notifies the container, regardless
	//  of the specific hint or sender.

	NotifyChanged(nDrawAspect);
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc attributes

BOOL COleServerDoc::GetZoomFactor(LPSIZE lpSizeNum, LPSIZE lpSizeDenom,
	LPCRECT lpPosRect) const
{
	ASSERT_VALID(this);
	ASSERT(lpSizeNum == NULL || AfxIsValidAddress(lpSizeNum, sizeof(SIZE)));
	ASSERT(lpSizeDenom == NULL || AfxIsValidAddress(lpSizeDenom, sizeof(SIZE)));
	ASSERT(lpPosRect == NULL ||
		AfxIsValidAddress(lpPosRect, sizeof(RECT), FALSE));

	if (!IsInPlaceActive())
	{
		if (lpSizeNum != NULL)
		{
			ASSERT(lpSizeDenom != NULL);
			lpSizeNum->cx = 1;
			lpSizeNum->cy = 1;
			*lpSizeDenom = *lpSizeNum;
		}
		return FALSE;
	}
	ASSERT_VALID(m_pInPlaceFrame);

	// the zoom factor is (Size(position-rect) / m_sizeExtent)
	CSize sizeNum;
	if (lpPosRect == NULL)
	{
		// use current position rect
		sizeNum = m_pInPlaceFrame->m_rectPos.Size();
	}
	else
	{
		// use new position rect
		sizeNum.cx = lpPosRect->right - lpPosRect->left;
		sizeNum.cy = lpPosRect->bottom - lpPosRect->top;
	}

	// m_sizeExtent is in HIMETRIC units -- need to convert to pixels.
	CSize sizeDenom(0, 0);
	COleServerItem* pItem = ((COleServerDoc*)this)->GetEmbeddedItem();
	ASSERT_VALID(pItem);
	ASSERT_KINDOF(COleServerItem, pItem);

	// get zoom denominator, which is based on the current extent
	((COleServerItem*)pItem)->OnGetExtent(DVASPECT_CONTENT, sizeDenom);
	if (sizeDenom.cx == 0 || sizeDenom.cy == 0)
	{
		// no extent from container available, so use natural extent instead
		pItem->OnGetExtent(DVASPECT_CONTENT, sizeDenom);
	}
	// convert extent to pixels first
	((CDC*)NULL)->HIMETRICtoDP(&sizeDenom);

	// might be bad to have zoom denominator of zero!
	if (sizeDenom.cy == 0 || sizeDenom.cx == 0)
	{
		TRACE0("Warning: zero 'zoom denominator', using 100%% zoom instead.\n");
		sizeDenom = sizeNum;
	}

	// store the results
	if (lpSizeNum != NULL)
	{
		ASSERT(lpSizeDenom != NULL);
		*lpSizeNum = sizeNum;
		*lpSizeDenom = sizeDenom;
	}

	// if 100% scaling, return FALSE
	return sizeNum != sizeDenom;
}

void COleServerDoc::GetItemPosition(LPRECT lpPosRect) const
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpPosRect, sizeof(RECT)));
	ASSERT(IsInPlaceActive());
	ASSERT_VALID(m_pInPlaceFrame);

	// copy the current rectangle
	*lpPosRect = m_pInPlaceFrame->m_rectPos;
}

void COleServerDoc::GetItemClipRect(LPRECT lpClipRect) const
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpClipRect, sizeof(RECT)));
	ASSERT(IsInPlaceActive());
	ASSERT_VALID(m_pInPlaceFrame);

	// copy the current rectangle
	*lpClipRect = m_pInPlaceFrame->m_rectClip;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc overrideables

COleServerItem* COleServerDoc::OnGetLinkedItem(LPCTSTR lpszItemName)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidString(lpszItemName));

	// default implementation walks list of server items looking for
	//  a case sensitive match

	POSITION pos = GetStartPosition();
	COleServerItem* pItem;
	while ((pItem = GetNextServerItem(pos)) != NULL)
	{
		// return item if name matches (case sensitive)
		if (lstrcmp(pItem->GetItemName(), lpszItemName) == 0)
			return pItem;
	}

#ifdef _DEBUG
	if (afxTraceFlags & traceOle)
	{
		TRACE0("Warning: default COleServerDoc::OnGetLinkedItem implementation\n");
		TRACE1("\tfailed to find item '%s'.\n", lpszItemName);
	}
#endif
	return NULL;        // not found (no link found)
}

void COleServerDoc::OnClose(OLECLOSE dwCloseOption)
{
	ASSERT_VALID(this);

	// do nothing if already in the process of closing the document
	if (m_bClosing)
		return;

	// OLECLOSE_PROMPTSAVE is handled like OLECLOSE_SAVEIFDIRTY if invisible
	CFrameWnd* pFrameWnd = GetFirstFrame();
	if (pFrameWnd != NULL && pFrameWnd->IsWindowVisible())
		dwCloseOption = OLECLOSE_SAVEIFDIRTY;

	// handle modified document and special dwCloseOption flags
	TRY
	{
		if (IsModified())
		{
			switch (dwCloseOption)
			{
			case OLECLOSE_PROMPTSAVE:
				if (!SaveModifiedPrompt())
					AfxThrowOleException(OLE_E_PROMPTSAVECANCELLED);
				break;

			case OLECLOSE_SAVEIFDIRTY:
				SaveEmbedding();
				break;
			}
		}
	}
	END_TRY

	// deactivate in-place session
	if (m_pInPlaceFrame != NULL)
	{
		OnDeactivate();
		ASSERT(m_pInPlaceFrame == NULL);
	}

	// close the document
	BOOL bAutoDelete = m_bAutoDelete;
	m_bAutoDelete = FALSE;
	OnCloseDocument();
	m_bAutoDelete = bAutoDelete;
}

BOOL COleServerDoc::SaveModifiedPrompt()
{
	ASSERT_VALID(this);

	if (m_lpClientSite == NULL)
		return SaveModified();

	UpdateModifiedFlag();   // check for modified client items

	if (!IsModified())
		return TRUE;        // ok to continue

	CString prompt;
	AfxFormatString1(prompt, AFX_IDP_ASK_TO_UPDATE, m_strTitle);
	switch (AfxMessageBox(prompt, MB_YESNOCANCEL, AFX_IDP_ASK_TO_UPDATE))
	{
	case IDCANCEL:
		return FALSE;       // don't continue

	case IDYES:
		if (!OnUpdateDocument())
		{
			TRACE0("Warning: OnUpdateDocument failed to update.\n");
			// keep going, close will flush it
		}
		break;
	}
	return TRUE;    // keep going
}

BOOL COleServerDoc::CanCloseFrame(CFrameWnd* pFrame)
{
	m_bClosing = TRUE;
	if (!COleLinkingDoc::CanCloseFrame(pFrame))
	{
		m_bClosing = FALSE;
		return FALSE;
	}
	return TRUE;
}

BOOL COleServerDoc::GetFileTypeString(CString& rString)
{
	ASSERT_VALID(this);

	CDocTemplate* pTemplate = GetDocTemplate();
	if (pTemplate == NULL)
		return FALSE;

	pTemplate->GetDocString(rString, CDocTemplate::fileNewName);
	return !rString.IsEmpty();
}

void COleServerDoc::OnSetHostNames(LPCTSTR lpszHost, LPCTSTR lpszHostObj)
{
	ASSERT_VALID(this);
	ASSERT(lpszHost == NULL || AfxIsValidString(lpszHost));
	ASSERT(lpszHostObj == NULL || AfxIsValidString(lpszHostObj));
	UNUSED(lpszHost);    // unused in release builds

	// only change the title for embedded documents
	if (m_lpClientSite != NULL)
	{
		// save name of document for File.Exit update
		if (lpszHostObj == NULL)
			m_strHostObj.LoadString(AFX_IDS_UNTITLED);
		else
			m_strHostObj = lpszHostObj;

		// attempt to get the document name from the document template
		CString strFileType;
		if (!GetFileTypeString(strFileType))
			return;

		// format the string into <server-name> in <docname>
		CString strTitle;
		AfxFormatString2(strTitle, AFX_IDS_APP_TITLE_EMBEDDING,
			strFileType, m_strHostObj);

		// change title of document to that for an embedded item
		//  (this call will update all of the frames)
		SetTitle(strTitle);
	}
}

void COleServerDoc::SaveEmbedding()
{
	ASSERT_VALID(this);

	// tell the client site to save the object
	if (m_lpClientSite != NULL && !::InSendMessage())
	{
		SCODE sc = m_lpClientSite->SaveObject();
		if (sc != S_OK)
			AfxThrowOleException(sc);
	}
	ASSERT_VALID(this);
}

BOOL COleServerDoc::OnUpdateDocument()
{
	ASSERT_VALID(this);

	// don't save if already up-to-date
	if (!IsModified())
		return TRUE;

	// save a server document -> update
	TRY
	{
		SaveEmbedding();
	}
	CATCH_ALL(e)
	{
		AfxMessageBox(AFX_IDP_FAILED_TO_UPDATE);
		DELETE_EXCEPTION(e);
		return FALSE;
	}
	END_CATCH_ALL

	return TRUE;
}

LPMONIKER COleServerDoc::GetMoniker(OLEGETMONIKER nAssign)
{
	ASSERT_VALID(this);

	if (m_lpClientSite != NULL)
	{
		// get moniker from client site instead of from document
		LPMONIKER lpMoniker = NULL;
		m_lpClientSite->GetMoniker(nAssign, OLEWHICHMK_OBJFULL, &lpMoniker);
		return lpMoniker;
	}

	return COleLinkingDoc::GetMoniker(nAssign);
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc document handling

BOOL COleServerDoc::SaveModified()
{
	ASSERT_VALID(this);

	if (m_lpClientSite != NULL)
	{
		if (m_pInPlaceFrame == NULL)
		{
			UpdateModifiedFlag();   // check for modified items
			OnUpdateDocument();
		}
		return TRUE;
	}
	return COleLinkingDoc::SaveModified();
}

HMENU COleServerDoc::GetDefaultMenu()
{
	ASSERT_VALID(this);

	CDocTemplate* pTemplate = GetDocTemplate();
	if (pTemplate == NULL)
		return NULL;    // no doc template -- use default

	ASSERT_VALID(pTemplate);
	if (m_pInPlaceFrame != NULL)
		return pTemplate->m_hMenuInPlaceServer; // return special in-place menu
	else if (m_lpClientSite != NULL)
		return pTemplate->m_hMenuEmbedding; // return special embedding menu

	return NULL;    // no special mode, use default menu
}

HACCEL COleServerDoc::GetDefaultAccelerator()
{
	ASSERT_VALID(this);

	CDocTemplate* pTemplate = GetDocTemplate();
	if (pTemplate == NULL)
		return NULL;    // no doc template -- use default

	ASSERT_VALID(pTemplate);
	if (m_pInPlaceFrame != NULL)
		return pTemplate->m_hAccelInPlaceServer;    // return special in-place accel
	else if (m_lpClientSite != NULL)
		return pTemplate->m_hAccelEmbedding;// return special embedding accel

	return NULL;    // no special mode, use default menu
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc default command handling

BEGIN_MESSAGE_MAP(COleServerDoc, COleLinkingDoc)
	//{{AFX_MSG_MAP(COleServerDoc)
	ON_COMMAND(ID_FILE_UPDATE, OnFileUpdate)
	ON_COMMAND(ID_FILE_SAVE_COPY_AS, OnFileSaveCopyAs)
	ON_UPDATE_COMMAND_UI(ID_APP_EXIT, OnUpdateFileExit)
	ON_UPDATE_COMMAND_UI(ID_FILE_UPDATE, OnUpdateFileUpdate)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void COleServerDoc::OnFileUpdate()
{
	ASSERT_VALID(this);
	ASSERT(m_lpClientSite != NULL);

	UpdateModifiedFlag();
	OnUpdateDocument();
}

void COleServerDoc::OnFileSaveCopyAs()
{
	ASSERT_VALID(this);
	ASSERT(m_bRemember);
	ASSERT(m_lpClientSite != NULL);

	LPSTORAGE lpOrigStg = m_lpRootStg;
	m_lpRootStg = NULL; // ignore embedded storage for now

	TRY
	{
		// call DoSave to perform Save Copy As...
		m_bRemember = FALSE;
		DoSave(NULL, FALSE);
	}
	END_TRY

	m_lpRootStg = lpOrigStg;
	m_bRemember = TRUE;
}

void COleServerDoc::UpdateUsingHostObj(UINT nIDS, CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	ASSERT(pCmdUI != NULL);

	if (m_lpClientSite == NULL)
		return;

	// update menu item using m_strHostObj
	CString str;
	AfxFormatString1(str, nIDS, m_strHostObj);
	if (!str.IsEmpty())
		pCmdUI->SetText(str);
}

void COleServerDoc::OnUpdateFileExit(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	ASSERT(pCmdUI != NULL);

	UpdateUsingHostObj(AFX_IDS_EXIT_MENU, pCmdUI);
}

void COleServerDoc::OnUpdateFileUpdate(CCmdUI* pCmdUI)
{
	ASSERT_VALID(this);
	ASSERT(pCmdUI != NULL);

	UpdateUsingHostObj(AFX_IDS_UPDATE_MENU, pCmdUI);
}

BOOL COleServerDoc::OnSaveDocument(LPCTSTR lpszPathName)
{
	ASSERT_VALID(this);
	ASSERT(lpszPathName == NULL || AfxIsValidString(lpszPathName));

	BOOL bModified = IsModified();
	BOOL bRemember = m_bRemember;
	if (!COleLinkingDoc::OnSaveDocument(lpszPathName))
		return FALSE;

	if (!bRemember)
		SetModifiedFlag(bModified);

	if (lpszPathName != NULL && bRemember)
	{
		if (AfxComparePath(GetPathName(), lpszPathName))
			NotifySaved();
	}
	return TRUE;
}

void COleServerDoc::OnCloseDocument()
{
	ASSERT_VALID(this);

	// don't allow in-place active documents to be closed without first
	//  deactivating them!
	if (m_pInPlaceFrame != NULL)
	{
		if (GetFirstViewPosition() != NULL)
			return;

		// no views but currently in-place active indicates that
		//  a WM_ENDSESSION is being processed.
		m_pInPlaceFrame = NULL;
	}

	InternalAddRef();   // keep document stable during shutdown

	// update lock count before sending notifications
	UpdateVisibleLock(FALSE, FALSE);

	// send some notifications to the container
	if (m_lpClientSite != NULL && m_bCntrVisible)
	{
		// allow the container to unshade the object appropriately
		m_lpClientSite->OnShowWindow(FALSE);
		m_bCntrVisible = FALSE;
	}

	// send close notification
	NotifyClosed();

	// finish closing the document (before m_lpClientSite->Release)
	BOOL bAutoDelete = m_bAutoDelete;
	m_bAutoDelete = FALSE;
	COleLinkingDoc::OnCloseDocument();
	ASSERT_VALID(this);

	// release client-site pointer
	RELEASE(m_lpClientSite);

	// disconnect the object
	LPUNKNOWN lpUnknown = (LPUNKNOWN)GetInterface(&IID_IUnknown);
	ASSERT(lpUnknown != NULL);
	CoDisconnectObject(lpUnknown, 0);

	// destroy the document if allowed
	InterlockedDecrement(&m_dwRef);              // remove InternalAddRef above
	if (bAutoDelete)
		delete this;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc show/hide

void COleServerDoc::OnShowDocument(BOOL bShow)
{
	ASSERT_VALID(this);

	CWinApp* pApp = AfxGetApp();
	if (bShow)
	{
		// deactivate in-place session if active
		if (m_pInPlaceFrame != NULL)
		{
			OnDeactivate();
			ASSERT(m_pInPlaceFrame == NULL);
		}

		// find the first view of this document
		CFrameWnd* pFrameWnd;
		if ((pFrameWnd = GetFirstFrame()) != NULL)
		{
			// allow container to show & scroll to the object
			if (!pFrameWnd->IsWindowVisible() && m_lpClientSite != NULL)
				m_lpClientSite->ShowObject();

			// activate frame holding view
			ASSERT_VALID(pFrameWnd);
			pFrameWnd->ActivateFrame();

			// activate application if necessary
			CFrameWnd* pAppFrame = pFrameWnd->GetParentFrame();
			if (pAppFrame != NULL)
			{
				pFrameWnd = pAppFrame;
				ASSERT_VALID(pFrameWnd);
				ASSERT_KINDOF(CFrameWnd, pFrameWnd);
				pFrameWnd->ActivateFrame();
			}
			pFrameWnd->GetLastActivePopup()->SetForegroundWindow();

			// update the menu and title as appropriate for this document
			pFrameWnd->OnUpdateFrameMenu(NULL);
			pFrameWnd->OnUpdateFrameTitle(TRUE);
		}
		else if (pApp->m_pMainWnd != NULL)
		{
			// otherwise, just show the main window (for simple servers)
			CWnd* pWnd = AfxGetMainWnd();

			// allow container to show & scroll to the object
			if (!pWnd->IsWindowVisible() && m_lpClientSite != NULL)
				m_lpClientSite->ShowObject();

			pWnd->ShowWindow(SW_SHOW);
			pWnd->SetActiveWindow();
			pWnd->SetForegroundWindow();
		}

		// for file based documents, showing the document puts user in control.
		if (!m_bEmbedded)
			AfxOleSetUserCtrl(TRUE);
	}
	else
	{
		if (m_pInPlaceFrame != NULL)
		{
			// hide has semantics of DeactivateUI if the item is active
			if (m_pInPlaceFrame->m_bUIActive)
				OnDeactivateUI(FALSE);

			// and then hide the frame itself
			if (m_pInPlaceFrame != NULL)
				m_pInPlaceFrame->ActivateFrame(SW_HIDE);
			return;
		}

		// find the first view of this document
		POSITION pos = GetFirstViewPosition();
		if (pos != NULL)
		{
			CFrameWnd* pDocFrame = GetFirstFrame();
			CFrameWnd* pActiveFrame = NULL;
			CFrameWnd* pMainFrame = NULL;
			CView* pView = GetNextView(pos);
			ASSERT_VALID(pView);

			// destroy or hide all the frames for this document
			//  (the main for the document is hidden, where the alternate
			//   frames are simply destroyed)
			do
			{
				// hide frame holding view
				CFrameWnd* pFrame = pView->GetParentFrame();
				ASSERT_VALID(pFrame);

				// determine next valid view before destroying the frame
				while ((pView = GetNextView(pos)) != NULL)
				{
					if (pView->GetParentFrame() != pFrame)
						break;
				}

				pMainFrame = pFrame->GetParentFrame();
				if (pMainFrame != NULL && pMainFrame->GetActiveFrame() == pFrame)
				{
					// don't destroy the active frame until later
					pActiveFrame = pFrame;
				}
				else
				{
					// not the active frame -- destroy/hide it now
					PreCloseFrame(pFrame);
					if (pDocFrame == pFrame)
						pFrame->ActivateFrame(SW_HIDE);
					else
						pFrame->DestroyWindow();
				}

			} while (pView != NULL);

			// hide the active frame last
			if (pActiveFrame != NULL)
			{
				PreCloseFrame(pActiveFrame);
				if (pDocFrame == pActiveFrame)
					pActiveFrame->ActivateFrame(SW_HIDE);
				else
					pActiveFrame->DestroyWindow();

				// should leave at least one frame
				ASSERT_VALID(this);
				ASSERT_VALID(GetFirstFrame());
			}
		}

		CFrameWnd* pMainFrame = (CFrameWnd*)pApp->m_pMainWnd;
		if (!AfxOleGetUserCtrl() && pMainFrame != NULL &&
			pMainFrame->IsWindowEnabled() && pMainFrame->IsFrameWnd() &&
			pMainFrame->GetActiveFrame() == pMainFrame)
		{
			// hide the entire application -- no visible documents left
			pApp->HideApplication();
		}
	}

	// send OnShowWindow notifications to the container
	if (m_lpClientSite != NULL && (bShow || m_bCntrVisible != bShow))
	{
		// allow the container to shade the object appropriately
		m_lpClientSite->OnShowWindow(bShow);
		m_bCntrVisible = bShow;
	}

	// force update visible lock
	if (bShow)
		UpdateVisibleLock(TRUE, FALSE);
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc storage implementation

void COleServerDoc::OnNewEmbedding(LPSTORAGE lpStorage)
{
	ASSERT_VALID(this);
	ASSERT(lpStorage != NULL);

	// save state
	BOOL bUserCtrl = AfxOleGetUserCtrl();

	TRY
	{
		// remember new storage
		DeleteContents();
		lpStorage->AddRef();
		RELEASE(m_lpRootStg);
		m_lpRootStg = lpStorage;
		m_strPathName.Empty();
		m_bEmbedded = TRUE;

		// do document initialization by calling OnNewDocument
		if (!OnNewDocument())
			AfxThrowMemoryException();
	}
	CATCH_ALL(e)
	{
		// restore state
		AfxOleSetUserCtrl(bUserCtrl);
		THROW_LAST();
	}
	END_CATCH_ALL

	// restore state
	AfxOleSetUserCtrl(bUserCtrl);

	SetModifiedFlag();  // new storage-based documents are dirty!
	SendInitialUpdate();
}

void COleServerDoc::OnOpenEmbedding(LPSTORAGE lpStorage)
{
	ASSERT_VALID(this);
	ASSERT(lpStorage != NULL);

	// save state
	BOOL bUserCtrl = AfxOleGetUserCtrl();

	TRY
	{
		// abort changes to current document
		DeleteContents();
		lpStorage->AddRef();
		RELEASE(m_lpRootStg);
		m_lpRootStg = lpStorage;

		// open document from the sub-storage
		if (!OnOpenDocument(NULL))
			AfxThrowMemoryException();

		// now document is storage based
		m_strPathName.Empty();
		m_bEmbedded = TRUE;
	}
	CATCH_ALL(e)
	{
		// restore state
		AfxOleSetUserCtrl(bUserCtrl);
		THROW_LAST();
	}
	END_CATCH_ALL

	// restore state
	AfxOleSetUserCtrl(bUserCtrl);

	SetModifiedFlag(FALSE); // start off with unmodified
	SendInitialUpdate();
}

void COleServerDoc::OnSaveEmbedding(LPSTORAGE lpStorage)
{
	ASSERT_VALID(this);
	ASSERT(lpStorage != NULL);

	// save state
	BOOL bUserCtrl = AfxOleGetUserCtrl();

	// check for save as
	LPSTORAGE lpOrigStg = m_lpRootStg;
	if (!m_bSameAsLoad)
	{
		// File Save[Copy] As (saving to different file)
		ASSERT(lpStorage != NULL);
		m_lpRootStg = lpStorage;
	}

	TRY
	{
		// save document to the sub-storage
		if (!OnSaveDocument(NULL))
			AfxThrowMemoryException();
	}
	CATCH_ALL(e)
	{
		// restore state
		AfxOleSetUserCtrl(bUserCtrl);
		// re-attach original storage
		m_lpRootStg = lpOrigStg;
		THROW_LAST();
	}
	END_CATCH_ALL

	// restore state
	AfxOleSetUserCtrl(bUserCtrl);

	// re-attach original storage
	m_lpRootStg = lpOrigStg;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc in-place activation implementation

AFX_STATIC HWND AFXAPI _AfxGetWindow32(HWND hWnd)
{
	// don't bother if hWnd is already a 32-bit HWND
	if (HIWORD(hWnd) != 0)
		return hWnd;

	// otherwise convert by getting a DC...
	HDC hDC = ::GetDC(hWnd);
	if (hDC == NULL)
		return hWnd;

	// then, getting the HWND from the DC...
	HWND hWnd32 = ::WindowFromDC(hDC);
	if (hWnd32 == NULL)
		hWnd32 = hWnd;

	// then releasing the DC itself
	::ReleaseDC(hWnd, hDC);

	return hWnd32;  // return full 32-bit HWND
}

BOOL COleServerDoc::ActivateInPlace()
{
	ASSERT_VALID(this);
	USES_CONVERSION;
	LPCOLESTR lpszTitle = NULL;

	if (m_lpClientSite == NULL)
		return FALSE;   // no client-side (may be a link)

	// activate already in-place window if currently in-place active
	if (m_pInPlaceFrame != NULL)
	{
		if (m_pInPlaceFrame->m_bUIActive)
		{
			m_lpClientSite->ShowObject();   // object should get focus
			return TRUE;
		}
		// deactivate in-place session entirely before continuing
		OnDeactivate();
	}

	// fail if already fully open
	if (GetFirstFrame()->IsWindowVisible())
		return FALSE;

	// build object title/name (the container may use this in its caption)
	CString strFileType, strTitle;
	if (!GetFileTypeString(strFileType))
		return FALSE;
	AfxFormatString2(strTitle, AFX_IDS_OBJ_TITLE_INPLACE,
		AfxGetAppName(), strFileType);

	// attempt to get in-place client-site interface
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite == NULL)
	{
		// unable to get in-place client site interface
		return FALSE;
	}

	// see if the container wants to go in-place right now
	if (lpInPlaceSite->CanInPlaceActivate() != S_OK)
		goto ReleaseAndFail;

	// start activation sequence...

	if (lpInPlaceSite->OnInPlaceActivate() != S_OK)
		goto ReleaseAndFail;

	// we'll need the parent window to create the COleIPFrameWnd
	HWND hWnd;
	VERIFY(lpInPlaceSite->GetWindow(&hWnd) == S_OK);
	CWnd* pParentWnd;
	pParentWnd = CWnd::FromHandle(hWnd);

	// create the inplace frame window
	COleIPFrameWnd* pFrameWnd;
	pFrameWnd = CreateInPlaceFrame(pParentWnd);
	if (pFrameWnd == NULL)
	{
		ASSERT(lpInPlaceSite != NULL);
		lpInPlaceSite->OnInPlaceDeactivate();
		goto ReleaseAndFail;
	}
	ASSERT(pFrameWnd->GetParent() == pParentWnd);
	m_pInPlaceFrame = pFrameWnd;

	// send activate notification.
	if (lpInPlaceSite->OnUIActivate() != S_OK)
		goto DestroyFrameAndFail;

	// need to get frame & doc window interfaces as well as other info
	RECT rcPosRect, rcClipRect;
	if (lpInPlaceSite->GetWindowContext(
		&pFrameWnd->m_lpFrame, &pFrameWnd->m_lpDocFrame,
		&rcPosRect, &rcClipRect, &pFrameWnd->m_frameInfo) != S_OK)
	{
		goto DeactivateUIAndFail;
	}
	ASSERT(pFrameWnd->m_lpFrame != NULL);

	// setup the shared menu
	if (!pFrameWnd->BuildSharedMenu())
		goto DeactivateUIAndFail;

	// allow server to install frame controls in container
	VERIFY(pFrameWnd->m_lpFrame->GetWindow(&hWnd) == S_OK);

	// some containers (Works 4.0, for example) return only the lower
	// 16-bits of the HWND and that confuses us; to work around this
	// we convert the 16-bit HWND into the full 32-bit HWND.
	hWnd = _AfxGetWindow32(hWnd);

	pFrameWnd->m_pMainFrame = new COleCntrFrameWnd(pFrameWnd);
	pFrameWnd->m_pMainFrame->Attach(hWnd);
	if (pFrameWnd->m_lpDocFrame != NULL)
	{
		HWND hWndDocument;
		VERIFY(pFrameWnd->m_lpDocFrame->GetWindow(&hWndDocument) == S_OK);
		if (hWndDocument != hWnd)
		{
			pFrameWnd->m_pDocFrame = new COleCntrFrameWnd(pFrameWnd);
			pFrameWnd->m_pDocFrame->Attach(hWndDocument);
		}
		else
		{
			RELEASE(pFrameWnd->m_lpDocFrame);
			pFrameWnd->m_lpDocFrame = NULL;
		}
	}

	// update zoom factor information before creating control bars

	//WINBUG: some clients send an empty rectangle and expect the
	// server to use the ClipRect for the PosRect
	if (IsRectEmpty(&rcPosRect))
		pFrameWnd->m_rectPos.CopyRect(&rcClipRect);
	else
		pFrameWnd->m_rectPos.CopyRect(&rcPosRect);
	pFrameWnd->m_rectClip.CopyRect(&rcClipRect);

	if (!pFrameWnd->OnCreateControlBars(pFrameWnd->m_pMainFrame,
		pFrameWnd->m_pDocFrame))
	{
		goto DeactivateUIAndFail;
	}

	// resize the window to match the object

	//WINBUG: some clients send an empty rectangle and expect the
	// server to use the ClipRect for the PosRect
	if (IsRectEmpty(&rcPosRect))
		OnSetItemRects(&rcClipRect, &rcClipRect);
	else
		OnSetItemRects(&rcPosRect, &rcClipRect);

	// set the active object
	ASSERT(pFrameWnd->m_lpFrame != NULL);
	LPOLEINPLACEACTIVEOBJECT lpActiveObject;
	lpActiveObject = (LPOLEINPLACEACTIVEOBJECT)
		GetInterface(&IID_IOleInPlaceActiveObject);
	lpszTitle = T2COLE(strTitle);
	pFrameWnd->m_lpFrame->SetActiveObject(lpActiveObject, lpszTitle);
	if (pFrameWnd->m_lpDocFrame != NULL)
		pFrameWnd->m_lpDocFrame->SetActiveObject(lpActiveObject, lpszTitle);

	// add frame & document level frame controls
	ASSERT(m_pInPlaceFrame->m_lpFrame != NULL);
	OnShowControlBars(m_pInPlaceFrame->m_pMainFrame, TRUE);
	if (m_pInPlaceFrame->m_lpDocFrame != NULL)
		OnShowControlBars(m_pInPlaceFrame->m_pDocFrame, TRUE);

	// show any hidden modeless dialogs as well...
	m_pInPlaceFrame->ShowOwnedWindows(TRUE);

	// attempt toolbar negotiation
	OnResizeBorder(NULL, pFrameWnd->m_lpFrame, TRUE);
	if (pFrameWnd->m_lpDocFrame != NULL)
		OnResizeBorder(NULL, pFrameWnd->m_lpDocFrame, FALSE);

	// install the menu (also installs a hook which forwards messages from
	//  the menu to the inplace frame window)
	pFrameWnd->m_lpFrame->SetMenu(pFrameWnd->m_hSharedMenu,
		pFrameWnd->m_hOleMenu, pFrameWnd->m_hWnd);

	// make sure object is scrolled into view
	m_lpClientSite->ShowObject();   // object should get focus

	// finally -- show the inplace frame window and set focus
	pFrameWnd->ShowWindow(SW_SHOW);
	pFrameWnd->SetFocus();
	pFrameWnd->UpdateWindow();
	UpdateVisibleLock(TRUE, FALSE);

	// allow the main window to be set
	OnFrameWindowActivate(TRUE);
	pFrameWnd->m_bUIActive = TRUE;

	// cleanup and return
	lpInPlaceSite->Release();
	return TRUE;

DeactivateUIAndFail:
	ASSERT(lpInPlaceSite != NULL);
	lpInPlaceSite->OnUIDeactivate(FALSE);

DestroyFrameAndFail:
	if (m_pInPlaceFrame != NULL)
	{
		ASSERT(pFrameWnd != NULL);
		DestroyInPlaceFrame(pFrameWnd);
		m_pInPlaceFrame = NULL;

		// also need to send OnInPlaceDeactivate notification
		ASSERT(lpInPlaceSite != NULL);
		lpInPlaceSite->OnInPlaceDeactivate();
	}

ReleaseAndFail:
	ASSERT(lpInPlaceSite != NULL);
	lpInPlaceSite->Release();
	return FALSE;
}

COleIPFrameWnd* COleServerDoc::CreateInPlaceFrame(CWnd* pParentWnd)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pParentWnd);

	// get runtime class from the doc template
	CDocTemplate* pTemplate = GetDocTemplate();
	ASSERT_VALID(pTemplate);

	// use existing view if possible
	CWnd* pViewParent = NULL;
	CView* pView = NULL;
	CFrameWnd* pFrame = GetFirstFrame();
	if (pFrame != NULL)
	{
		pView = (CView*)pFrame->GetDescendantWindow(AFX_IDW_PANE_FIRST, TRUE);
		if (pView != NULL)
		{
			ASSERT_KINDOF(CView, pView);
			pViewParent = pView->GetParent();
			m_dwOrigStyle = pView->GetStyle();
			m_dwOrigStyleEx = pView->GetExStyle();
		}
	}

	// create the frame from the template
	COleIPFrameWnd* pFrameWnd = (COleIPFrameWnd*)
		pTemplate->CreateOleFrame(pParentWnd, this, pView == NULL);
	if (pFrameWnd == NULL)
		return NULL;

	// connect the view to the frame window, if necessary
	if (pView != NULL)
	{
		ConnectView(pFrameWnd, pView);
		pView->ModifyStyleEx(WS_EX_CLIENTEDGE, 0, SWP_DRAWFRAME);
	}

	// remember original parent window for deactivate
	m_pOrigParent = pViewParent;

	// send OnInitialUpdate if new view was created
	if (pView == NULL)
		pTemplate->InitialUpdateFrame(pFrameWnd, this, FALSE);

	// verify the type
	ASSERT_VALID(pFrameWnd);
	ASSERT_KINDOF(COleIPFrameWnd, pFrameWnd);
	return pFrameWnd;
}

void COleServerDoc::DestroyInPlaceFrame(COleIPFrameWnd* pFrameWnd)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pFrameWnd);

	// connect view to original, if existing view was used
	if (m_pOrigParent != NULL)
	{
		CView* pView = (CView*)pFrameWnd->GetDescendantWindow(
			AFX_IDW_PANE_FIRST, TRUE);
		ASSERT_VALID(pView);

		// leaving the focus on an MDI child or one of its child windows
		// causes Windows to get confused when the child window is
		// destroyed, not to mention the fact that the focus will be
		// out of sync with activation.
		if (::GetFocus() == pView->m_hWnd)
		{
			// move focus to somewhere safe
			HWND hWnd = ::GetParent(pFrameWnd->m_hWnd);
			if (hWnd != NULL)
				::SetFocus(hWnd);

			// check again
			if (::GetFocus() == pView->m_hWnd)
				SetFocus(NULL); // last ditch effort
		}

		ConnectView(m_pOrigParent, pView);
		m_pOrigParent = NULL;

		// remove any scrollbars added because of in-place activation
		if ((m_dwOrigStyle & (WS_HSCROLL|WS_VSCROLL)) == 0 &&
			(pView->GetStyle() & (WS_HSCROLL|WS_VSCROLL)) != 0)
		{
			::SetScrollRange(pView->m_hWnd, SB_HORZ, 0, 0, TRUE);
			::SetScrollRange(pView->m_hWnd, SB_VERT, 0, 0, TRUE);
		}

		// restore old 3D style
		pView->ModifyStyleEx(0, m_dwOrigStyleEx & WS_EX_CLIENTEDGE,
			SWP_DRAWFRAME);

		// force recalc layout on splitter window
		CSplitterWnd* pSplitter = CView::GetParentSplitter(pView, TRUE);
		if (pSplitter != NULL)
			pSplitter->RecalcLayout();
	}

	// no active view or document during destroy
	pFrameWnd->SetActiveView(NULL);

	// destroy in-place frame window
	pFrameWnd->DestroyWindow();
}

void COleServerDoc::ConnectView(CWnd* pParentWnd, CView* pView)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pParentWnd);
	ASSERT_VALID(pView);

	// move the view to the new parent
	pView->SetParent(pParentWnd);

	// Note: The currently active view on the original frame window is
	//  kept active, because some controls, especially Windows controls,
	//  continue to send notification messages to the original parent
	//  window of the control.  So, the original frame window is kept
	//  alive with the original active view pointer intact, such that
	//  these notification messages do not get lost.

	// set the active view of the new frame to newly moved view
	CFrameWnd* pFrameWnd = pParentWnd->IsFrameWnd() ?
		(CFrameWnd*)pParentWnd : pParentWnd->GetParentFrame();
	pFrameWnd->SetActiveView(pView, FALSE);
	pFrameWnd->RecalcLayout();
}

void COleServerDoc::OnFrameWindowActivate(BOOL bActivate)
{
	ASSERT_VALID(this);

	CFrameWnd* pFrameWnd = m_pInPlaceFrame;
	ASSERT_VALID(pFrameWnd);

	CWinThread* pThread = AfxGetThread();
	if (bActivate)
	{
		// activating -- so set the main window
		pThread->m_pActiveWnd = pFrameWnd;

		// send activation notification messages
		pFrameWnd->SendMessage(WM_ACTIVATEAPP, (WPARAM)TRUE);
		pFrameWnd->SendMessage(WM_ACTIVATE, WA_ACTIVE);
	}
	else if (pFrameWnd == pThread->m_pActiveWnd)
	{
		// send deactivation notification messages
		pFrameWnd->SendMessage(WM_ACTIVATEAPP, (WPARAM)FALSE);
		pFrameWnd->SendMessage(WM_ACTIVATE, WA_INACTIVE);

		// simulate deactivation notification messages
		CView* pActiveView = pFrameWnd->GetActiveView();
		if (pActiveView != NULL)
			pActiveView->OnActivateView(FALSE, pActiveView, pActiveView);

		// deactivating and was previously active -- reset the main window
		pThread->m_pActiveWnd = NULL;
	}
}

void COleServerDoc::OnDocWindowActivate(BOOL bActivate)
{
	ASSERT_VALID(this);

	CWinThread* pThread = AfxGetApp();
	COleIPFrameWnd* pFrameWnd = m_pInPlaceFrame;

	if (bActivate)
	{
		// attach frame windows back in the permanent map
		pFrameWnd->m_pMainFrame->Attach(pFrameWnd->m_pMainFrame->Detach());
		if (pFrameWnd->m_pDocFrame != NULL)
			pFrameWnd->m_pDocFrame->Attach(pFrameWnd->m_pDocFrame->Detach());

		// set active main window
		pThread->m_pActiveWnd = pFrameWnd;

		// show frame level controls
		OnShowControlBars(pFrameWnd->m_pMainFrame, TRUE);
		pFrameWnd->ShowOwnedWindows(TRUE);

		// attempt toolbar negotiation
		OnResizeBorder(NULL, pFrameWnd->m_lpFrame, TRUE);

		// install the menu (also installs a hook which forwards messages from
		//  the menu to the inplace frame window)
		pFrameWnd->m_lpFrame->SetMenu(
			pFrameWnd->m_hSharedMenu, pFrameWnd->m_hOleMenu,
			pFrameWnd->m_hWnd);

		// set focus to the frame (it will probably set focus to the view)
		//  (by simulating normal application activate messages)
		pFrameWnd->SendMessage(WM_ACTIVATE, WA_ACTIVE);
	}
	else
	{
		// clear active window for app if this object is active
		if (pThread->m_pActiveWnd == pFrameWnd)
			pThread->m_pActiveWnd = NULL;

		// hide frame level controls -- this does not destroy them
		pFrameWnd->ShowOwnedWindows(FALSE);
		OnShowControlBars(pFrameWnd->m_pMainFrame, FALSE);

		// attempt toolbar negotiation
		OnResizeBorder(NULL, pFrameWnd->m_lpFrame, TRUE);

		// simulate deactivation notification messages
		CView* pActiveView = pFrameWnd->GetActiveView();
		if (pActiveView != NULL)
			pActiveView->OnActivateView(FALSE, pActiveView, pActiveView);
		pFrameWnd->SendMessage(WM_ACTIVATE, WA_INACTIVE);

		// set the m_hWnd members, but remove them from the maps
		pFrameWnd->m_pMainFrame->m_hWnd = pFrameWnd->m_pMainFrame->Detach();
		if (pFrameWnd->m_pDocFrame != NULL)
			pFrameWnd->m_pDocFrame->m_hWnd = pFrameWnd->m_pDocFrame->Detach();
	}
}

void COleServerDoc::OnShowControlBars(CFrameWnd* pFrameWnd, BOOL bShow)
{
	ASSERT_VALID(this);
	ASSERT(pFrameWnd == m_pInPlaceFrame->m_pMainFrame ||
		pFrameWnd == m_pInPlaceFrame->m_pDocFrame);
	ASSERT_VALID(pFrameWnd);

	// show/hide all control bars
	POSITION pos = pFrameWnd->m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		// show/hide the next control bar
		CControlBar* pBar =
			(CControlBar*)pFrameWnd->m_listControlBars.GetNext(pos);
		ASSERT_VALID(pBar);
		if (bShow)
		{
			if (pBar->m_nStateFlags & CControlBar::tempHide)
			{
				pBar->m_nStateFlags &= ~CControlBar::tempHide;
				pFrameWnd->ShowControlBar(pBar, bShow, TRUE);
			}
		}
		else
		{
			if (pBar->IsVisible() && !pBar->IsFloating())
			{
				pBar->m_nStateFlags |= CControlBar::tempHide;
				pFrameWnd->ShowControlBar(pBar, bShow, TRUE);
			}
		}
	}
}

void COleServerDoc::OnResizeBorder(
	LPCRECT lpRectBorder, LPOLEINPLACEUIWINDOW lpUIWindow, BOOL bFrame)
{
	ASSERT_VALID(this);
	ASSERT(lpRectBorder == NULL ||
		AfxIsValidAddress(lpRectBorder, sizeof(RECT), FALSE));
	ASSERT(lpUIWindow != NULL);

	// use IOleInPlaceUIWindow::GetBorder if no border given
	CRect rectBorder;
	if (lpRectBorder != NULL)
	{
		// use border space passed in
		rectBorder.CopyRect(lpRectBorder);
	}
	else
	{
		// GetBorderSpace may fail (WinWord6, for example)
		if (lpUIWindow->GetBorder(&rectBorder) != S_OK)
		{
			// in that case, always call SetBorderSpace(NULL), but
			// ignore the return value.
			lpUIWindow->SetBorderSpace(NULL);
			return;
		}
	}

	// get CWnd* for the OLE window
	CFrameWnd* pFrameWnd = bFrame ?
		m_pInPlaceFrame->m_pMainFrame : m_pInPlaceFrame->m_pDocFrame;

	// if this is the active document clear temphide bits from bars
	if (AfxGetThread()->m_pActiveWnd == m_pInPlaceFrame)
		OnShowControlBars(pFrameWnd, TRUE);

	// see how much space we need by calling reposition bars
	CRect rectNeeded = rectBorder;
	pFrameWnd->RepositionBars(0, 0xFFFF, 0, CWnd::reposQuery, &rectNeeded,
		&rectBorder);

	// request the border space from the container
	CRect rectRequest(
		rectNeeded.left - rectBorder.left,
		rectNeeded.top - rectBorder.top,
		rectBorder.right - rectNeeded.right,
		rectBorder.bottom - rectNeeded.bottom);
	CRect rectTemp;
	rectTemp = rectRequest;

	// if no border space, just call SetBorderSpace
	if ((!rectRequest.IsRectNull() ||
		 !pFrameWnd->m_listControlBars.IsEmpty()) &&
		lpUIWindow->RequestBorderSpace(&rectTemp) == S_OK)
	{
		// set the border space -- now this object owns it
		VERIFY(lpUIWindow->SetBorderSpace(&rectRequest) == S_OK);

		// move the bars into position after committing the space
		pFrameWnd->RepositionBars(0, 0xFFFF, 0, CWnd::reposDefault, NULL,
			&rectBorder);

		// redraw all control bars
		POSITION pos = pFrameWnd->m_listControlBars.GetHeadPosition();
		while (pos != NULL)
		{
			CControlBar* pBar =
				(CControlBar*)pFrameWnd->m_listControlBars.GetNext(pos);
			ASSERT_VALID(pBar);
			if (!pBar->IsFloating())
				pBar->UpdateWindow();
		}
	}
	else
	{
		// hide any toolbars (since we couldn't get border space for any)
		OnShowControlBars(pFrameWnd, FALSE);

		// make sure border space is cleared
		CRect rect(0,0,0,0);
		lpUIWindow->SetBorderSpace(&rect);
	}
}

void COleServerDoc::OnDeactivate()
{
	ASSERT_VALID(this);

	ASSERT(m_pInPlaceFrame != NULL);

	// do UI deactivate first -- this hides everything
	if (m_pInPlaceFrame->m_bUIActive)
	{
		OnDeactivateUI(FALSE);

		// some containers call OnDeactivate during OnDeactivateUI
		if (m_pInPlaceFrame == NULL)
			return;
	}
	ASSERT(m_pInPlaceFrame != NULL);
	ASSERT(!m_pInPlaceFrame->m_bUIActive);

	// now safe to destroy the shared menu
	m_pInPlaceFrame->DestroySharedMenu();

	// no longer need doc & frame window interfaces
	RELEASE(m_pInPlaceFrame->m_lpFrame);
	RELEASE(m_pInPlaceFrame->m_lpDocFrame);
	DestroyInPlaceFrame(m_pInPlaceFrame);
	m_pInPlaceFrame = NULL;
		// destructor for COleIPFrameWnd or derivative should cleanup any
		//   toolbars etc. created during in-place activation

	// last of all, call IOleClientSite::InPlaceDeactivate
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite != NULL)
	{
		lpInPlaceSite->OnInPlaceDeactivate();
		lpInPlaceSite->Release();
	}
}

void COleServerDoc::OnDeactivateUI(BOOL bUndoable)
{
	ASSERT_VALID(this);
	COleIPFrameWnd* pFrameWnd = m_pInPlaceFrame;

	if (pFrameWnd == NULL || !pFrameWnd->m_bUIActive)
		return;

	// reset active object pointers
	ASSERT(pFrameWnd->m_lpFrame != NULL);
	pFrameWnd->m_lpFrame->SetActiveObject(NULL, NULL);
	if (pFrameWnd->m_lpDocFrame != NULL)
		pFrameWnd->m_lpDocFrame->SetActiveObject(NULL, NULL);

	// remove frame & document level frame controls
	ASSERT(pFrameWnd->m_lpFrame != NULL);
	OnShowControlBars(pFrameWnd->m_pMainFrame, FALSE);
	if (pFrameWnd->m_lpDocFrame != NULL)
		OnShowControlBars(pFrameWnd->m_pDocFrame, FALSE);

	if (m_pDocObjectServer == NULL)
	{
		// hide the frame and any popups owned by the frame
		pFrameWnd->ShowOwnedWindows(FALSE);
		pFrameWnd->ShowWindow(SW_HIDE);
		pFrameWnd->m_nShowDelay = SW_HIDE;
		pFrameWnd->m_pMainFrame->m_nShowDelay = SW_HIDE;
	}

	// set the m_hWnd members, but remove them from the maps
	pFrameWnd->m_pMainFrame->m_hWnd = pFrameWnd->m_pMainFrame->Detach();
	if (pFrameWnd->m_pDocFrame != NULL)
		pFrameWnd->m_pDocFrame->m_hWnd = pFrameWnd->m_pDocFrame->Detach();

	// no longer UI active...
	pFrameWnd->m_bUIActive = FALSE;
	CWinThread* pThread = AfxGetApp();
	if (pThread->m_pActiveWnd == pFrameWnd)
		pThread->m_pActiveWnd = NULL;

	// call IOleClientSite::OnUIDeactivate
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite != NULL)
	{
		lpInPlaceSite->OnUIDeactivate(bUndoable);
		lpInPlaceSite->Release();
	}
}

void COleServerDoc::OnSetItemRects(LPCRECT lpPosRect, LPCRECT lpClipRect)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpPosRect, sizeof(RECT), FALSE));
	ASSERT(AfxIsValidAddress(lpClipRect, sizeof(RECT), FALSE));

	if (m_pInPlaceFrame == NULL)
		return;
	ASSERT_VALID(m_pInPlaceFrame);

	// tell the frame to position itself such that the view is at the given
	//  rectangle (relative to the frame's parent)
	m_pInPlaceFrame->RepositionFrame(lpPosRect, lpClipRect);
}

BOOL COleServerDoc::OnReactivateAndUndo()
{
	// default implementation doesn't support undo

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc special APIs for in-place editing

void COleServerDoc::RequestPositionChange(LPCRECT lpPosRect)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpPosRect, sizeof(RECT), FALSE));

	// get IOleInPlaceSite interface
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);

	if (lpInPlaceSite != NULL)
	{
		// call IOleInPlaceSite::OnPosRectChange
		lpInPlaceSite->OnPosRectChange(lpPosRect);
		lpInPlaceSite->Release();
	}
}

BOOL COleServerDoc::ScrollContainerBy(CSize sizeScroll)
{
	ASSERT_VALID(this);

	// get IOleInPlaceSite interface
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite == NULL)
		return FALSE;

	// call IOleInPlaceSite::Scroll
	BOOL bResult = lpInPlaceSite->Scroll(sizeScroll) == S_OK;
	lpInPlaceSite->Release();
	return bResult;
}

BOOL COleServerDoc::DeactivateAndUndo()
{
	ASSERT_VALID(this);

	// get IOleInPlaceSite interface
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite == NULL)
		return FALSE;

	// call IOleInPlaceSite::DeactivateAndUndo
	BOOL bResult = lpInPlaceSite->DeactivateAndUndo() == S_OK;
	lpInPlaceSite->Release();
	return bResult;
}

BOOL COleServerDoc::DiscardUndoState()
{
	ASSERT_VALID(this);

	// get IOleInPlaceSite interface
	ASSERT(m_lpClientSite != NULL);
	LPOLEINPLACESITE lpInPlaceSite =
		QUERYINTERFACE(m_lpClientSite, IOleInPlaceSite);
	if (lpInPlaceSite == NULL)
		return FALSE;

	// call IOleInPlaceSite::DiscardUndoState
	BOOL bResult = lpInPlaceSite->DiscardUndoState() == S_OK;
	lpInPlaceSite->Release();
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc OLE interface implementation

BEGIN_INTERFACE_MAP(COleServerDoc, COleLinkingDoc)
	INTERFACE_PART(COleServerDoc, IID_IPersistStorage, PersistStorage)
	INTERFACE_PART(COleServerDoc, IID_IOleObject, OleObject)
	INTERFACE_PART(COleServerDoc, IID_IDataObject, DataObject)
	INTERFACE_PART(COleServerDoc, IID_IOleWindow, OleInPlaceObject)
	INTERFACE_PART(COleServerDoc, IID_IOleInPlaceObject, OleInPlaceObject)
	INTERFACE_PART(COleServerDoc, IID_IOleInPlaceActiveObject, OleInPlaceActiveObject)
END_INTERFACE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc::CPersistStorage

STDMETHODIMP_(ULONG) COleServerDoc::XPersistStorage::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerDoc::XPersistStorage::Release()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerDoc::XPersistStorage::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerDoc::XPersistStorage::GetClassID(LPCLSID lpClassID)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)

	LPPERSISTFILE lpPersistFile = (LPPERSISTFILE)
		pThis->GetInterface(&IID_IPersistFile);
	return lpPersistFile->GetClassID(lpClassID);
}

STDMETHODIMP COleServerDoc::XPersistStorage::IsDirty()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)

	LPPERSISTFILE lpPersistFile = (LPPERSISTFILE)
		pThis->GetInterface(&IID_IPersistFile);
	return lpPersistFile->IsDirty();
}

STDMETHODIMP COleServerDoc::XPersistStorage::InitNew(LPSTORAGE pStg)
{
	METHOD_PROLOGUE_EX(COleServerDoc, PersistStorage)
	ASSERT_VALID(pThis);

	SCODE sc = S_OK;
	TRY
	{
		// delegate to member function in the document
		pThis->OnNewEmbedding(pStg);
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	ASSERT_VALID(pThis);
	return sc;
}

STDMETHODIMP COleServerDoc::XPersistStorage::Load(LPSTORAGE pStg)
{
	METHOD_PROLOGUE_EX(COleServerDoc, PersistStorage)
	ASSERT_VALID(pThis);

	SCODE sc = S_OK;
	pThis->BeginDeferErrors();
	TRY
	{
		// delegate to member function in the document
		pThis->OnOpenEmbedding(pStg);
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL
	sc = pThis->EndDeferErrors(sc);

	ASSERT_VALID(pThis);
	return sc;
}

STDMETHODIMP COleServerDoc::XPersistStorage::Save(
	LPSTORAGE pStgSave, BOOL fSameAsLoad)
{
	METHOD_PROLOGUE_EX(COleServerDoc, PersistStorage)
	ASSERT_VALID(pThis);

	// don't bother saving if destination is up-to-date
	if (fSameAsLoad && !pThis->IsModified())
		return S_OK;

	SCODE sc = S_OK;
	pThis->BeginDeferErrors();
	TRY
	{
		// delegate through the document
		ASSERT(pThis->m_bRemember);
		pThis->m_bRemember = FALSE;
		pThis->m_bSameAsLoad = fSameAsLoad;
		pThis->OnSaveEmbedding(pStgSave);

		// clear dirty flag since save to same storage successful
		if (fSameAsLoad)
		{
			pThis->SetModifiedFlag(FALSE);

			// notify clients that object has been saved
			pThis->NotifySaved();
		}
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL
	sc = pThis->EndDeferErrors(sc);

	// restore default state
	pThis->m_bRemember = TRUE;

	ASSERT_VALID(pThis);
	return sc;
}

STDMETHODIMP COleServerDoc::XPersistStorage::SaveCompleted(LPSTORAGE pStgSaved)
{
	METHOD_PROLOGUE_EX(COleServerDoc, PersistStorage)
	ASSERT_VALID(pThis);

	// call SaveCompleted on any embedded items
	pThis->CommitItems(pStgSaved != NULL);

	// update state to reflect new storage
	if (pStgSaved != NULL)
	{
		// attach new storage
		pStgSaved->AddRef();
		RELEASE(pThis->m_lpRootStg);
		pThis->m_lpRootStg = pStgSaved;

		// now this document is storage based
		pThis->m_strPathName.Empty();
		pThis->m_bEmbedded = TRUE;
		pThis->SetModifiedFlag(FALSE);

		// notify clients that object has been saved
		pThis->NotifySaved();
	}

	ASSERT_VALID(pThis);
	return S_OK;
}

STDMETHODIMP COleServerDoc::XPersistStorage::HandsOffStorage()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, PersistStorage)

	if (pThis->m_lpRootStg != NULL)
	{
		// first call HandsOffStorage for all the embedded client items
		POSITION pos = pThis->GetStartPosition();
		COleClientItem* pItem;
		while ((pItem = pThis->GetNextClientItem(pos)) != NULL)
		{
			ASSERT(pItem->m_lpObject != NULL);
			LPPERSISTSTORAGE lpPersistStorage =
				QUERYINTERFACE(pItem->m_lpObject, IPersistStorage);
			ASSERT(lpPersistStorage != NULL);
			lpPersistStorage->HandsOffStorage();
			lpPersistStorage->Release();
		}

		// for now, can't access the storage
		RELEASE(pThis->m_lpRootStg);
	}

	ASSERT_VALID(pThis);
	return S_OK;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc::XOleObject

STDMETHODIMP_(ULONG) COleServerDoc::XOleObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerDoc::XOleObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerDoc::XOleObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerDoc::XOleObject::SetClientSite(
	LPOLECLIENTSITE pClientSite)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	// maintain reference counts
	if (pClientSite != NULL)
		pClientSite->AddRef();
	RELEASE(pThis->m_lpClientSite);
	pThis->m_lpClientSite = pClientSite;

	// do we already have doc object support enabled?
	if (pThis->m_pDocObjectServer != NULL)
	{
		// If we currently have a document site pointer,
		// release it.
		pThis->m_pDocObjectServer->ReleaseDocSite();
	}

	if (pClientSite != NULL)
	{
		LPOLEDOCUMENTSITE pDocSite;

		if (SUCCEEDED(pClientSite->QueryInterface(IID_IOleDocumentSite,
			(LPVOID*) &pDocSite)))
		{
			if (pThis->m_pDocObjectServer != NULL)
				pThis->m_pDocObjectServer->SetDocSite(pDocSite);
			else
			{
				pThis->m_pDocObjectServer =
					pThis->GetDocObjectServer(pDocSite);
			}
		}
	}

	return S_OK;
}

STDMETHODIMP COleServerDoc::XOleObject::GetClientSite(
	LPOLECLIENTSITE* ppClientSite)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	if (pThis->m_lpClientSite == NULL)
	{
		*ppClientSite = NULL;
		return E_FAIL;
	}

	*ppClientSite = pThis->m_lpClientSite;
	pThis->m_lpClientSite->AddRef();
	return S_OK;
}

STDMETHODIMP COleServerDoc::XOleObject::SetHostNames(
	LPCOLESTR lpszContainerApp, LPCOLESTR lpszContainerObj)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	USES_CONVERSION;

	TRY
	{
		pThis->OnSetHostNames(OLE2CT(lpszContainerApp),
			OLE2CT(lpszContainerObj));
	}
	END_TRY

	return S_OK;
}

STDMETHODIMP COleServerDoc::XOleObject::Close(DWORD dwSaveOption)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	SCODE sc = S_OK;
	TRY
	{
		// delegate through document for most of the work
		pThis->OnClose((OLECLOSE)dwSaveOption);
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	pThis->InternalRelease();   // may 'delete this'

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::SetMoniker(
	DWORD /*dwWhichMoniker*/, LPMONIKER /*pmk*/)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	if (pThis->m_lpClientSite == NULL)
		return E_FAIL;

	// get current full moniker from client-site
	LPMONIKER lpMoniker = NULL;
	if (pThis->m_lpClientSite->GetMoniker(OLEGETMONIKER_ONLYIFTHERE,
		OLEWHICHMK_OBJFULL, &lpMoniker) != S_OK)
	{
		// just to make sure -- always set moniker to NULL on failure
		lpMoniker = NULL;
	}

	// update all embedded items with new moniker
	POSITION pos = pThis->GetStartPosition();
	COleClientItem* pItem;
	while ((pItem = pThis->GetNextClientItem(pos)) != NULL)
	{
		if (pItem->m_bMoniker)
			pItem->m_lpObject->SetMoniker(OLEWHICHMK_CONTAINER, lpMoniker);
	}

	// send Rename advises
	pThis->NotifyAllItems(OLE_RENAMED, (DWORD)lpMoniker);
	RELEASE(lpMoniker);

	return S_OK;
}

STDMETHODIMP COleServerDoc::XOleObject::GetMoniker(
	DWORD dwAssign, DWORD /*dwWhichMoniker*/, LPMONIKER* ppMoniker)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	*ppMoniker = pThis->GetMoniker((OLEGETMONIKER)dwAssign);
	return *ppMoniker == NULL ? E_FAIL : S_OK;
}

STDMETHODIMP COleServerDoc::XOleObject::InitFromData(
	LPDATAOBJECT pDataObject, BOOL fCreation, DWORD /*dwReserved*/)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc;
	TRY
	{
		// delegate through item
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		COleDataObject dataObject;
		dataObject.Attach(pDataObject,  FALSE);
		sc = pItem->OnInitFromData(&dataObject, fCreation) ? S_OK : S_FALSE;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::GetClipboardData(
	DWORD /*dwReserved*/, LPDATAOBJECT* ppDataObject)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	*ppDataObject = NULL;

	COleServerItem* pItem;
	SCODE sc = S_OK;
	TRY
	{
		// delegate through item
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		COleDataSource* pDataSource =
			pItem->OnGetClipboardData(TRUE, NULL, NULL);
		ASSERT(pDataSource != NULL);

		*ppDataObject =
			(LPDATAOBJECT)pDataSource->GetInterface(&IID_IDataObject);
		ASSERT(*ppDataObject != NULL);
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::DoVerb(
	LONG iVerb, LPMSG /*lpmsg*/, LPOLECLIENTSITE /*pActiveSite*/, LONG /*lindex*/,
	HWND /*hwndParent*/, LPCRECT /*lpPosRect*/)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	COleServerItem* pItem;
	SCODE sc = S_OK;
	TRY
	{
		// delegate through item
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		pItem->OnDoVerb(iVerb);
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL
	pThis->InternalRelease();   // may 'delete this'

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::EnumVerbs(
	IEnumOLEVERB** ppenumOleVerb)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	*ppenumOleVerb = NULL;

	LPOLEOBJECT lpObject = (LPOLEOBJECT)pThis->GetInterface(&IID_IOleObject);
	ASSERT(lpObject != NULL);
	CLSID clsid;
	lpObject->GetUserClassID(&clsid);

	return OleRegEnumVerbs(clsid, ppenumOleVerb);
}

STDMETHODIMP COleServerDoc::XOleObject::Update()
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc = S_OK;
	TRY
	{
		// delegate through item
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		pItem->OnUpdateItems();
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::IsUpToDate()
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc;
	TRY
	{
		// delegate through item
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		sc = pItem->OnQueryUpdateItems() ? S_FALSE : S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::GetUserClassID(CLSID* lpClassID)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	LPPERSISTFILE lpPersistFile = (LPPERSISTFILE)
		pThis->GetInterface(&IID_IPersistFile);
	return lpPersistFile->GetClassID(lpClassID);
}

STDMETHODIMP COleServerDoc::XOleObject::GetUserType(
	DWORD dwFormOfType, LPOLESTR* ppszUserType)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	*ppszUserType = NULL;

	LPOLEOBJECT lpObject = (LPOLEOBJECT)pThis->GetInterface(&IID_IOleObject);
	ASSERT(lpObject != NULL);
	CLSID clsid;
	lpObject->GetUserClassID(&clsid);

	return OleRegGetUserType(clsid, dwFormOfType, ppszUserType);
}

STDMETHODIMP COleServerDoc::XOleObject::SetExtent(
	DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc = E_FAIL;
	TRY
	{
		// convert rectangle to a CSize and call item OnSetExtent
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		CSize size((int)lpsizel->cx, (int)lpsizel->cy);
		if (pItem->OnSetExtent((DVASPECT)dwDrawAspect, size))
			sc = S_OK;
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::GetExtent(
	DWORD dwDrawAspect, LPSIZEL lpsizel)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc = E_INVALIDARG;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		// call to get regular windows CSize
		CSize size;
		if (pItem->OnGetExtent((DVASPECT)dwDrawAspect, size))
		{
			if (size.cy < 0)
				size.cy = -size.cy; // extents are always positive
			lpsizel->cx = size.cx;
			lpsizel->cy = size.cy;

			sc = S_OK;
		}
	}
	CATCH_ALL(e)
	{
		sc = COleException::Process(e);
		DELETE_EXCEPTION(e);
	}
	END_CATCH_ALL

	return sc;
}

STDMETHODIMP COleServerDoc::XOleObject::Advise(
	IAdviseSink* pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetOleObject()->Advise(pAdvSink, pdwConnection);
}

STDMETHODIMP COleServerDoc::XOleObject::Unadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetOleObject()->Unadvise(dwConnection);
}

STDMETHODIMP COleServerDoc::XOleObject::EnumAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetOleObject()->EnumAdvise(ppenumAdvise);
}

STDMETHODIMP COleServerDoc::XOleObject::GetMiscStatus(
	DWORD dwAspect, DWORD* pdwStatus)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleObject)

	*pdwStatus = 0;

	LPOLEOBJECT lpObject = (LPOLEOBJECT)pThis->GetInterface(&IID_IOleObject);
	ASSERT(lpObject != NULL);
	CLSID clsid;
	lpObject->GetUserClassID(&clsid);

	return OleRegGetMiscStatus(clsid, dwAspect, pdwStatus);
}

STDMETHODIMP COleServerDoc::XOleObject::SetColorScheme(LPLOGPALETTE lpLogpal)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleObject)
	ASSERT_VALID(pThis);

	COleServerItem* pItem;
	SCODE sc = E_NOTIMPL;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);

		// delegate to embedded item
		if (pItem->OnSetColorScheme(lpLogpal))
			sc = S_OK;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc::XDataObject

STDMETHODIMP_(ULONG) COleServerDoc::XDataObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, DataObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerDoc::XDataObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, DataObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerDoc::XDataObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, DataObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerDoc::XDataObject::GetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->GetData(lpFormatEtc, lpStgMedium);
}

STDMETHODIMP COleServerDoc::XDataObject::GetDataHere(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->GetDataHere(lpFormatEtc, lpStgMedium);
}

STDMETHODIMP COleServerDoc::XDataObject::QueryGetData(
	LPFORMATETC lpFormatEtc)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->QueryGetData(lpFormatEtc);
}

STDMETHODIMP COleServerDoc::XDataObject::GetCanonicalFormatEtc(
	LPFORMATETC /*lpFormatEtcIn*/, LPFORMATETC /*lpFormatEtcOut*/)
{
	// because we support the target-device (ptd) for server metafile format,
	//  all members of the FORMATETC are significant.

	return DATA_S_SAMEFORMATETC;
}

STDMETHODIMP COleServerDoc::XDataObject::SetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium, BOOL bRelease)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->SetData(lpFormatEtc, lpStgMedium, bRelease);
}

STDMETHODIMP COleServerDoc::XDataObject::EnumFormatEtc(
	DWORD dwDirection, LPENUMFORMATETC* ppenumFormatEtc)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->EnumFormatEtc(dwDirection, ppenumFormatEtc);
}

STDMETHODIMP COleServerDoc::XDataObject::DAdvise(
	FORMATETC* pFormatetc, DWORD advf,
	LPADVISESINK pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->DAdvise(pFormatetc, advf, pAdvSink,
		pdwConnection);
}

STDMETHODIMP COleServerDoc::XDataObject::DUnadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->DUnadvise(dwConnection);
}

STDMETHODIMP COleServerDoc::XDataObject::EnumDAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX(COleServerDoc, DataObject)

	COleServerItem* pItem = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		pItem = pThis->GetEmbeddedItem();
		ASSERT_VALID(pItem);
		ASSERT_KINDOF(COleServerItem, pItem);
		sc = S_OK;
	}
	END_TRY

	if (sc != S_OK)
		return sc;

	return pItem->GetDataObject()->EnumDAdvise(ppenumAdvise);
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc::COleInPlaceObject

STDMETHODIMP_(ULONG) COleServerDoc::XOleInPlaceObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerDoc::XOleInPlaceObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::GetWindow(HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceObject)

	LPOLEINPLACEACTIVEOBJECT lpActiveObject = (LPOLEINPLACEACTIVEOBJECT)
		pThis->GetInterface(&IID_IOleInPlaceActiveObject);
	return lpActiveObject->GetWindow(lphwnd);
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::ContextSensitiveHelp(
	BOOL fEnterMode)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceObject)

	LPOLEINPLACEACTIVEOBJECT lpActiveObject = (LPOLEINPLACEACTIVEOBJECT)
		pThis->GetInterface(&IID_IOleInPlaceActiveObject);
	return lpActiveObject->ContextSensitiveHelp(fEnterMode);
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::InPlaceDeactivate()
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// only call deactivate if necessary
		if (pThis->m_pInPlaceFrame != NULL)
			pThis->OnDeactivate();

		// should be completely inactive
		ASSERT(pThis->m_pInPlaceFrame == NULL);
		sc = S_OK;
	}
	END_TRY

	pThis->InternalRelease();   // may 'delete this'
	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::UIDeactivate()
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// only call OnUIDeactivate if necessary
		if (pThis->m_pInPlaceFrame != NULL &&
			pThis->m_pInPlaceFrame->m_bUIActive)
		{
			pThis->OnDeactivateUI(FALSE);   // default to not undoable
		}

		// should not be ui active
		ASSERT(pThis->m_pInPlaceFrame == NULL ||
			!pThis->m_pInPlaceFrame->m_bUIActive);
		sc = S_OK;
	}
	END_TRY

	pThis->InternalRelease();   // may 'delete this'
	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::SetObjectRects(
	LPCRECT lpPosRect, LPCRECT lpClipRect)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnSetItemRects(lpPosRect, lpClipRect);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceObject::ReactivateAndUndo()
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		sc = pThis->OnReactivateAndUndo() ? S_OK : INPLACE_E_NOTUNDOABLE;
	}
	END_TRY

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// COleServerDoc::XOleInPlaceActiveObject

STDMETHODIMP_(ULONG) COleServerDoc::XOleInPlaceActiveObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceActiveObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleServerDoc::XOleInPlaceActiveObject::Release()
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceActiveObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceActiveObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::GetWindow(
	HWND* lphwnd)
{
	METHOD_PROLOGUE_EX_(COleServerDoc, OleInPlaceActiveObject)

	*lphwnd = pThis->m_pInPlaceFrame->GetSafeHwnd();
	return *lphwnd != NULL ? S_OK : E_FAIL;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::ContextSensitiveHelp(
	BOOL fEnterMode)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	if (fEnterMode)
	{
		if (!pThis->m_pInPlaceFrame->m_bHelpMode)
		{
			// check if help mode probable
			if (!pThis->m_pInPlaceFrame->CanEnterHelpMode())
				return E_UNEXPECTED;

			// attempt to enter context help
			if (!pThis->m_pInPlaceFrame->PostMessage(WM_COMMAND, ID_CONTEXT_HELP))
				return E_UNEXPECTED;
		}
	}
	else
	{
		// just exit help mode
		pThis->m_pInPlaceFrame->ExitHelpMode();
	}

	return S_OK;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::TranslateAccelerator(
	LPMSG lpmsg)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	pThis->InternalAddRef();    // protect this object

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// get frame window for this document
		CFrameWnd* pFrameWnd = pThis->m_pInPlaceFrame;
		ASSERT_VALID(pFrameWnd);

		// attempt translate accelerator
		MSG msg = *lpmsg;
		sc = pFrameWnd->PreTranslateMessage(&msg) ? S_OK : S_FALSE;
		*lpmsg = msg;
	}
	END_TRY

	pThis->InternalRelease();   // may 'delete this'

	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::OnFrameWindowActivate(
	BOOL fActivate)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnFrameWindowActivate(fActivate);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::OnDocWindowActivate(
	BOOL fActivate)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnDocWindowActivate(fActivate);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::ResizeBorder(
	LPCRECT lprectBorder, LPOLEINPLACEUIWINDOW lpUIWindow, BOOL fFrameWindow)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		pThis->OnResizeBorder(lprectBorder, lpUIWindow, fFrameWindow);
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleServerDoc::XOleInPlaceActiveObject::EnableModeless(
	BOOL fEnable)
{
	METHOD_PROLOGUE_EX(COleServerDoc, OleInPlaceActiveObject)
	ASSERT_VALID(pThis);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		if (!fEnable)
		{
			// start modal state if not in modal state
			if (!pThis->m_pInPlaceFrame->InModalState())
			{
				CWnd* pTemp = pThis->m_pInPlaceFrame->GetTopLevelParent();
				BOOL bEnabled = pTemp->IsWindowEnabled();
				pThis->m_pInPlaceFrame->BeginModalState();
				pTemp->EnableWindow(bEnabled);
			}
		}
		else
		{
			// end modal state if in modal state
			if (pThis->m_pInPlaceFrame->InModalState())
				pThis->m_pInPlaceFrame->EndModalState();
		}
		sc = S_OK;
	}
	END_TRY

	return sc;
}

//////////////////////////////////////////////////////////////////////////////
// Diagnostics

#ifdef _DEBUG
void COleServerDoc::AssertValid() const
{
	COleLinkingDoc::AssertValid();
	if (m_pInPlaceFrame != NULL)
		m_pInPlaceFrame->AssertValid();
	if (m_pOrigParent != NULL)
		m_pOrigParent->AssertValid();
}

void COleServerDoc::Dump(CDumpContext& dc) const
{
	COleLinkingDoc::Dump(dc);

	if (dc.GetDepth() != 0)
	{
		if (m_pInPlaceFrame != NULL)
			dc << "\nwith in-place frame: " << m_pInPlaceFrame;
		else
			dc << "\nwith no in-place frame\n";
		if (m_pOrigParent != NULL)
			dc << "\nwith original parent: " << m_pOrigParent;
		else
			dc << "with no original parent\n";
	}
	dc << "m_lpClientSite = " << m_lpClientSite;
	dc << "\nm_strHostObj = " << m_strHostObj;
	dc << "\nm_bCntrVisible = " << m_bCntrVisible;
	dc << "\nm_dwOrigStyle = " << m_dwOrigStyle;

	dc << "\n";
}
#endif //_DEBUG

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

// expand inlines for OLE server APIs
static char _szAfxOleInl[] = "afxole.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxOleInl
#define _AFXOLESVR_INLINE
#include "afxole.inl"

#endif //!_AFX_ENABLE_INLINES

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleServerDoc, COleLinkingDoc)

/////////////////////////////////////////////////////////////////////////////
