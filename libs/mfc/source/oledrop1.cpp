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

#ifdef AFX_OLE4_SEG
#pragma code_seg(AFX_OLE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleDropSource implementation

AFX_DATADEF UINT COleDropSource::nDragMinDist;
AFX_DATADEF UINT COleDropSource::nDragDelay;

COleDropSource::COleDropSource()
{
	m_bDragStarted = FALSE;
	m_dwButtonCancel = 0;
	m_dwButtonDrop = 0;

	AfxLockGlobals(CRIT_DROPSOURCE);
	static BOOL bInitialized;
	if (!bInitialized)
	{
		// get drag metrics from win.ini
		static const TCHAR szWindows[] = _T("windows");
		static const TCHAR szDragMinDist[] = _T("DragMinDist");
		static const TCHAR szDragDelay[] = _T("DragDelay");

		nDragMinDist = GetProfileInt(szWindows, szDragMinDist, DD_DEFDRAGMINDIST);
		nDragDelay = GetProfileInt(szWindows, szDragDelay, DD_DEFDRAGDELAY);

		// now initialized, no need to call Initialize any more
		bInitialized = TRUE;
	}
	AfxUnlockGlobals(CRIT_DROPSOURCE);

	ASSERT_VALID(this);
}

SCODE COleDropSource::QueryContinueDrag(BOOL bEscapePressed, DWORD dwKeyState)
{
	ASSERT_VALID(this);

	// check escape key or right button -- and cancel
	if (bEscapePressed || (dwKeyState & m_dwButtonCancel) != 0)
	{
		m_bDragStarted = FALSE; // avoid unecessary cursor setting
		return DRAGDROP_S_CANCEL;
	}

	// check left-button up to end drag/drop and do the drop
	if ((dwKeyState & m_dwButtonDrop) == 0)
		return m_bDragStarted ? DRAGDROP_S_DROP : DRAGDROP_S_CANCEL;

	// otherwise, keep polling...
	return S_OK;
}

SCODE COleDropSource::GiveFeedback(DROPEFFECT /*dropEffect*/)
{
	ASSERT_VALID(this);

	// don't change the cursor until drag is officially started
	return m_bDragStarted ? DRAGDROP_S_USEDEFAULTCURSORS : S_OK;
}

BOOL COleDropSource::OnBeginDrag(CWnd* pWnd)
{
	ASSERT_VALID(this);

	m_bDragStarted = FALSE;

	// opposite button cancels drag operation
	m_dwButtonCancel = 0;
	m_dwButtonDrop = 0;
	if (GetKeyState(VK_LBUTTON) < 0)
	{
		m_dwButtonDrop |= MK_LBUTTON;
		m_dwButtonCancel |= MK_RBUTTON;
	}
	else if (GetKeyState(VK_RBUTTON) < 0)
	{
		m_dwButtonDrop |= MK_RBUTTON;
		m_dwButtonCancel |= MK_LBUTTON;
	}

	DWORD dwLastTick = GetTickCount();
	pWnd->SetCapture();

	while (!m_bDragStarted)
	{
		// some applications steal capture away at random times
		if (CWnd::GetCapture() != pWnd)
			break;

		// peek for next input message
		MSG msg;
		if (PeekMessage(&msg, NULL, WM_MOUSEFIRST, WM_MOUSELAST, PM_REMOVE) ||
			PeekMessage(&msg, NULL, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE))
		{
			// check for button cancellation (any button down will cancel)
			if (msg.message == WM_LBUTTONUP || msg.message == WM_RBUTTONUP ||
				msg.message == WM_LBUTTONDOWN || msg.message == WM_RBUTTONDOWN)
				break;

			// check for keyboard cancellation
			if (msg.message == WM_KEYDOWN && msg.wParam == VK_ESCAPE)
				break;

			// check for drag start transition
			m_bDragStarted = !m_rectStartDrag.PtInRect(msg.pt);
		}

		// if the user sits here long enough, we eventually start the drag
		if (GetTickCount() - dwLastTick > nDragDelay)
			m_bDragStarted = TRUE;
	}
	ReleaseCapture();

	return m_bDragStarted;
}

BEGIN_INTERFACE_MAP(COleDropSource, CCmdTarget)
	INTERFACE_PART(COleDropSource, IID_IDropSource, DropSource)
END_INTERFACE_MAP()

STDMETHODIMP_(ULONG) COleDropSource::XDropSource::AddRef()
{
	METHOD_PROLOGUE_EX_(COleDropSource, DropSource)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleDropSource::XDropSource::Release()
{
	METHOD_PROLOGUE_EX_(COleDropSource, DropSource)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleDropSource::XDropSource::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleDropSource, DropSource)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleDropSource::XDropSource::QueryContinueDrag(
	THIS_ BOOL fEscapePressed, DWORD dwKeyState)
{
	METHOD_PROLOGUE_EX_(COleDropSource, DropSource)

	return pThis->QueryContinueDrag(fEscapePressed, dwKeyState);
}

STDMETHODIMP COleDropSource::XDropSource::GiveFeedback(THIS_ DWORD dwEffect)
{
	METHOD_PROLOGUE_EX(COleDropSource, DropSource)
	ASSERT_VALID(pThis);

	return pThis->GiveFeedback(dwEffect);
}

/////////////////////////////////////////////////////////////////////////////
// helper for doing drag/drop with COleDataSource object

DROPEFFECT COleDataSource::DoDragDrop(DWORD dwEffects,
	LPCRECT lpRectStartDrag, COleDropSource* pDropSource)
{
	ASSERT_VALID(this);
	if (pDropSource != NULL)
		ASSERT_VALID(pDropSource);
	ASSERT(lpRectStartDrag == NULL ||
		AfxIsValidAddress(lpRectStartDrag, sizeof(RECT), FALSE));

	// use standard drop source implementation if one not provided
	COleDropSource dropSource;
	if (pDropSource == NULL)
		pDropSource = &dropSource;

	// setup drag/drop sensitivity rect
	pDropSource->m_bDragStarted = FALSE;

	if (lpRectStartDrag != NULL)
	{
		// set drop source drag start rect to parameter provided
		pDropSource->m_rectStartDrag.CopyRect(lpRectStartDrag);
	}
	else
	{
		// otherwise start with default empty rectangle around current point
		CPoint ptCursor;
		GetCursorPos(&ptCursor);
		pDropSource->m_rectStartDrag.SetRect(
			ptCursor.x, ptCursor.y, ptCursor.x, ptCursor.y);
	}

	if (pDropSource->m_rectStartDrag.IsRectNull())
	{
		// null rect specifies no OnBeginDrag wait loop
		pDropSource->m_bDragStarted = TRUE;
	}
	else if (pDropSource->m_rectStartDrag.IsRectEmpty())
	{
		// empty rect specifies drag drop around starting point
		pDropSource->m_rectStartDrag.InflateRect(
			COleDropSource::nDragMinDist, COleDropSource::nDragMinDist);
	}
	ASSERT_VALID(pDropSource);

	// before calling OLE drag/drop code, wait for mouse to move outside
	//  the rectangle
	ASSERT_VALID(AfxGetMainWnd());
	if (!pDropSource->OnBeginDrag(AfxGetMainWnd()))
		return DROPEFFECT_NONE;

	// call global OLE api to do the drag drop
	LPDATAOBJECT lpDataObject = (LPDATAOBJECT)GetInterface(&IID_IDataObject);
	LPDROPSOURCE lpDropSource =
		(LPDROPSOURCE)pDropSource->GetInterface(&IID_IDropSource);
	DWORD dwResultEffect = DROPEFFECT_NONE;
	::DoDragDrop(lpDataObject, lpDropSource, dwEffects, &dwResultEffect);
	return dwResultEffect;
}

/////////////////////////////////////////////////////////////////////////////
// COleDropSource diagnostics

#ifdef _DEBUG
void COleDropSource::Dump(CDumpContext& dc) const
{
	CCmdTarget::Dump(dc);

	dc << "m_bDragStarted = " << m_bDragStarted;
	dc << "\nm_rectStartDrag.left = " << m_rectStartDrag.left;
	dc << "\nm_rectStartDrag.top = " << m_rectStartDrag.top;
	dc << "\nm_rectStartDrag.right = " << m_rectStartDrag.right;
	dc << "\nm_rectStartDrag.bottom = " << m_rectStartDrag.bottom;

	dc << "\n";
}
#endif

/////////////////////////////////////////////////////////////////////////////
