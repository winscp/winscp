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

#ifdef AFX_CORE3_SEG
#pragma code_seg(AFX_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// CDockState

AFX_STATIC_DATA const TCHAR _afxVisible[] = _T("Visible");
AFX_STATIC_DATA const TCHAR _afxBarSection[] = _T("%s-Bar%d");
AFX_STATIC_DATA const TCHAR _afxSummarySection[] = _T("%s-Summary");
AFX_STATIC_DATA const TCHAR _afxXPos[] = _T("XPos");
AFX_STATIC_DATA const TCHAR _afxYPos[] = _T("YPos");
AFX_STATIC_DATA const TCHAR _afxMRUWidth[] = _T("MRUWidth");
AFX_STATIC_DATA const TCHAR _afxDocking[] = _T("Docking");
AFX_STATIC_DATA const TCHAR _afxMRUDockID[] = _T("MRUDockID");
AFX_STATIC_DATA const TCHAR _afxMRUDockLeftPos[] = _T("MRUDockLeftPos");
AFX_STATIC_DATA const TCHAR _afxMRUDockRightPos[] = _T("MRUDockRightPos");
AFX_STATIC_DATA const TCHAR _afxMRUDockTopPos[] = _T("MRUDockTopPos");
AFX_STATIC_DATA const TCHAR _afxMRUDockBottomPos[] = _T("MRUDockBottomPos");
AFX_STATIC_DATA const TCHAR _afxMRUFloatStyle[] = _T("MRUFloatStyle");
AFX_STATIC_DATA const TCHAR _afxMRUFloatXPos[] = _T("MRUFloatXPos");
AFX_STATIC_DATA const TCHAR _afxMRUFloatYPos[] = _T("MRUFloatYPos");

AFX_STATIC_DATA const TCHAR _afxBarID[] = _T("BarID");
AFX_STATIC_DATA const TCHAR _afxHorz[] = _T("Horz");
AFX_STATIC_DATA const TCHAR _afxFloating[] = _T("Floating");
AFX_STATIC_DATA const TCHAR _afxBars[] = _T("Bars");
AFX_STATIC_DATA const TCHAR _afxScreenCX[] = _T("ScreenCX");
AFX_STATIC_DATA const TCHAR _afxScreenCY[] = _T("ScreenCY");
AFX_STATIC_DATA const TCHAR _afxBar[] = _T("Bar#%d");

CControlBarInfo::CControlBarInfo()
{
	m_nBarID = 0;
	m_bDockBar = m_bVisible = m_bFloating = m_bHorz = m_bDocking = FALSE;
	m_pBar = NULL;
	m_pointPos.x = m_pointPos.y = -1;
	m_nMRUWidth = 32767;

	m_uMRUDockID = 0;
	m_rectMRUDockPos.SetRectEmpty();
	m_dwMRUFloatStyle = 0;
	m_ptMRUFloatPos = CPoint(0,0);

	ASSERT(sizeof(DWORD) == sizeof(void*));
}

void CControlBarInfo::Serialize(CArchive& ar, CDockState* pDockState)
{
	ASSERT(pDockState!=NULL);

	if (ar.IsStoring())
	{
		ar << (DWORD)m_nBarID;
		ar << (DWORD)m_bVisible;
		ar << (DWORD)m_bFloating;
		ar << (DWORD)m_bHorz;
		ar << m_pointPos;
		if (pDockState->GetVersion() > 1)
		{
			ar << (DWORD)m_nMRUWidth;
			ar << (DWORD)m_bDocking;
			if (m_bDocking)
			{
				ar << (DWORD)m_uMRUDockID;
				ar << m_rectMRUDockPos;
				ar << m_dwMRUFloatStyle;
				ar << m_ptMRUFloatPos;
			}
		}

		ar << (WORD)m_arrBarID.GetSize();
		if (m_arrBarID.GetSize() != 0)
		{
#ifdef _AFX_BYTESWAP
			if (!ar.IsByteSwapping())
#endif
			ar.Write(&m_arrBarID.ElementAt(0),
				m_arrBarID.GetSize()*sizeof(DWORD));
#ifdef _AFX_BYTESWAP
			else
			{
				// write each ID individually so that it will be byte-swapped
				for (int i = 0; i < m_arrBarID.GetSize(); i++)
					ar << (DWORD)m_arrBarID[i];
			}
#endif
		}
	}
	else
	{

		DWORD dw;
		ar >> dw;
		m_nBarID = (int)dw;
		ar >> dw;
		m_bVisible = (BOOL)dw;
		ar >> dw;
		m_bFloating = (BOOL)dw;
		ar >> dw;
		m_bHorz = (BOOL)dw;
		ar >> m_pointPos;

		if (pDockState->GetVersion() > 1)
		{
			pDockState->ScalePoint(m_pointPos);

			ar >> dw;
			m_nMRUWidth = (int)dw;
			ar >> dw;
			m_bDocking = (BOOL)dw;
			if (m_bDocking)
			{
				ar >> dw;
				m_uMRUDockID = (DWORD)dw;
				ar >> m_rectMRUDockPos;
				pDockState->ScaleRectPos(m_rectMRUDockPos);

				ar >> m_dwMRUFloatStyle;
				ar >> m_ptMRUFloatPos;
				pDockState->ScalePoint(m_ptMRUFloatPos);
			}
		}

		WORD w;
		ar >> w;
		m_arrBarID.SetSize(w);
		if (w != 0)
		{
			ar.Read(&m_arrBarID.ElementAt(0),
				m_arrBarID.GetSize()*sizeof(DWORD));
#ifdef _AFX_BYTESWAP
			if (ar.IsByteSwapping())
			{
				for (int i = 0; i < m_arrBarID.GetSize(); i++)
					_AfxByteSwap((DWORD)m_arrBarID[i], (BYTE*)&m_arrBarID[i]);
			}
#endif
		}
	}
}

BOOL CControlBarInfo::LoadState(LPCTSTR lpszProfileName, int nIndex, CDockState* pDockState)
{
	ASSERT(pDockState != NULL);

	CWinApp* pApp = AfxGetApp();
	TCHAR szSection[256];
	wsprintf(szSection, _afxBarSection, lpszProfileName, nIndex);

	m_nBarID = pApp->GetProfileInt(szSection, _afxBarID, 0);
	m_bVisible = (BOOL) pApp->GetProfileInt(szSection, _afxVisible, TRUE);
	m_bHorz = (BOOL) pApp->GetProfileInt(szSection, _afxHorz, TRUE);
	m_bFloating = (BOOL) pApp->GetProfileInt(szSection, _afxFloating, FALSE);
	m_pointPos = CPoint(
		pApp->GetProfileInt(szSection, _afxXPos, -1),
		pApp->GetProfileInt(szSection, _afxYPos, -1));
	pDockState->ScalePoint(m_pointPos);
	m_nMRUWidth = pApp->GetProfileInt(szSection, _afxMRUWidth, 32767);
	m_bDocking = pApp->GetProfileInt(szSection, _afxDocking, 0);
	if (m_bDocking)
	{
		m_uMRUDockID = pApp->GetProfileInt(szSection, _afxMRUDockID, 0);

		m_rectMRUDockPos = CRect(
			pApp->GetProfileInt(szSection, _afxMRUDockLeftPos, 0),
			pApp->GetProfileInt(szSection, _afxMRUDockTopPos, 0),
			pApp->GetProfileInt(szSection, _afxMRUDockRightPos, 0),
			pApp->GetProfileInt(szSection, _afxMRUDockBottomPos, 0));
		pDockState->ScaleRectPos(m_rectMRUDockPos);

		m_dwMRUFloatStyle = pApp->GetProfileInt(szSection, _afxMRUFloatStyle, 0);

		m_ptMRUFloatPos = CPoint(
			pApp->GetProfileInt(szSection, _afxMRUFloatXPos, 0),
			pApp->GetProfileInt(szSection, _afxMRUFloatYPos, 0));
		pDockState->ScalePoint(m_ptMRUFloatPos);
	}

	int nBars = pApp->GetProfileInt(szSection, _afxBars, 0);
	for (int i=0; i < nBars; i++)
	{
		TCHAR buf[16];
		wsprintf(buf, _afxBar, i);
		m_arrBarID.Add((void*)pApp->GetProfileInt(szSection, buf, 0));
	}

	return m_nBarID != 0;
}

BOOL CControlBarInfo::SaveState(LPCTSTR lpszProfileName, int nIndex)
{
	TCHAR szSection[256];
	wsprintf(szSection, _afxBarSection, lpszProfileName, nIndex);

	// delete the section
	CWinApp* pApp = AfxGetApp();
	pApp->WriteProfileString(szSection, NULL, NULL);

	if (m_bDockBar && m_bVisible && !m_bFloating && m_pointPos.x == -1 &&
		m_pointPos.y == -1 && m_arrBarID.GetSize() <= 1)
	{
		return FALSE;
	}

	pApp->WriteProfileInt(szSection, _afxBarID, m_nBarID);
	if (!m_bVisible)
		pApp->WriteProfileInt(szSection, _afxVisible, m_bVisible);
	if (m_bFloating)
	{
		pApp->WriteProfileInt(szSection, _afxHorz, m_bHorz);
		pApp->WriteProfileInt(szSection, _afxFloating, m_bFloating);
	}
	if (m_pointPos.x != -1)
		pApp->WriteProfileInt(szSection, _afxXPos, m_pointPos.x);
	if (m_pointPos.y != -1)
		pApp->WriteProfileInt(szSection, _afxYPos, m_pointPos.y);
	if (m_nMRUWidth != 32767)
		pApp->WriteProfileInt(szSection, _afxMRUWidth, m_nMRUWidth);
	if (m_bDocking)
	{
		pApp->WriteProfileInt(szSection, _afxDocking, m_bDocking);
		pApp->WriteProfileInt(szSection, _afxMRUDockID, m_uMRUDockID);
		pApp->WriteProfileInt(szSection, _afxMRUDockLeftPos, m_rectMRUDockPos.left);
		pApp->WriteProfileInt(szSection, _afxMRUDockTopPos, m_rectMRUDockPos.top);
		pApp->WriteProfileInt(szSection, _afxMRUDockRightPos, m_rectMRUDockPos.right);
		pApp->WriteProfileInt(szSection, _afxMRUDockBottomPos, m_rectMRUDockPos.bottom);
		pApp->WriteProfileInt(szSection, _afxMRUFloatStyle, m_dwMRUFloatStyle);
		pApp->WriteProfileInt(szSection, _afxMRUFloatXPos, m_ptMRUFloatPos.x);
		pApp->WriteProfileInt(szSection, _afxMRUFloatYPos, m_ptMRUFloatPos.y);
	}

	if (m_arrBarID.GetSize() > 1) //if ==1 then still empty
	{
		pApp->WriteProfileInt(szSection, _afxBars, m_arrBarID.GetSize());
		for (int i = 0; i < m_arrBarID.GetSize(); i++)
		{
			TCHAR buf[16];
			wsprintf(buf, _afxBar, i);
			pApp->WriteProfileInt(szSection, buf, (int)m_arrBarID[i]);
		}
	}
	return TRUE;
}

CDockState::CDockState()
{
	m_dwVersion = 2;

	m_bScaling = FALSE;

	m_rectDevice.left = 0;
	m_rectDevice.top = 0;
	m_rectDevice.right = GetSystemMetrics(SM_CXSCREEN);
	m_rectDevice.bottom = GetSystemMetrics(SM_CYSCREEN);

	m_rectClip = m_rectDevice;
	m_rectClip.right -= GetSystemMetrics(SM_CXICON);
	m_rectClip.bottom -= GetSystemMetrics(SM_CYICON);
}

CDockState::~CDockState()
{
	for (int i = 0; i < m_arrBarInfo.GetSize(); i++)
		delete (CControlBarInfo*)m_arrBarInfo[i];
}

void CDockState::Serialize(CArchive& ar)
{
	// read/write version info
	if (ar.IsStoring())
	{
		ar << m_dwVersion;

		if (m_dwVersion > 1)
		{
			ar << GetScreenSize();
		}

		// write array contents
		ar << (WORD)m_arrBarInfo.GetSize();
		for (int i = 0; i < m_arrBarInfo.GetSize(); i++)
			((CControlBarInfo*)m_arrBarInfo[i])->Serialize(ar, this);
	}
	else
	{
		Clear(); //empty out dockstate
		ar >> m_dwVersion;       // read version marker
		ASSERT(m_dwVersion == 1 || m_dwVersion == 2);

		if (m_dwVersion > 1)
		{
			CSize size;
			ar >> size;
			SetScreenSize(size);
		}

		// read array contents
		WORD nOldSize;
		ar >> nOldSize;
		m_arrBarInfo.SetSize(nOldSize);
		for (int i = 0; i < m_arrBarInfo.GetSize(); i++)
		{
			m_arrBarInfo[i] = new CControlBarInfo;
			((CControlBarInfo*)m_arrBarInfo[i])->Serialize(ar, this);
		}
		m_dwVersion = 2;
	}
}

void CDockState::LoadState(LPCTSTR lpszProfileName)
{
	CWinApp* pApp = AfxGetApp();
	TCHAR szSection[256];
	wsprintf(szSection, _afxSummarySection, lpszProfileName);
	int nBars = pApp->GetProfileInt(szSection, _afxBars, 0);

	CSize size;
	size.cx = pApp->GetProfileInt(szSection, _afxScreenCX, 0);
	size.cy = pApp->GetProfileInt(szSection, _afxScreenCY, 0);
	SetScreenSize(size);

	for (int i = 0; i < nBars; i++)
	{
		CControlBarInfo* pInfo = new CControlBarInfo;
		m_arrBarInfo.Add(pInfo);
		pInfo->LoadState(lpszProfileName, i, this);
	}
}

void CDockState::SaveState(LPCTSTR lpszProfileName)
{
	int nIndex = 0;
	for (int i = 0;i < m_arrBarInfo.GetSize(); i++)
	{
		CControlBarInfo* pInfo = (CControlBarInfo*)m_arrBarInfo[i];
		ASSERT(pInfo != NULL);
		if (pInfo->SaveState(lpszProfileName, nIndex))
			nIndex++;
	}
	CWinApp* pApp = AfxGetApp();
	TCHAR szSection[256];
	wsprintf(szSection, _afxSummarySection, lpszProfileName);
	pApp->WriteProfileInt(szSection, _afxBars, nIndex);

	CSize size = GetScreenSize();
	pApp->WriteProfileInt(szSection, _afxScreenCX, size.cx);
	pApp->WriteProfileInt(szSection, _afxScreenCY, size.cy);
}

void CDockState::Clear()
{
	for (int i = 0; i < m_arrBarInfo.GetSize(); i++)
		delete (CControlBarInfo*) m_arrBarInfo[i];
	m_arrBarInfo.RemoveAll();
}

DWORD CDockState::GetVersion()
{
	return m_dwVersion;
}

void CDockState::ScalePoint(CPoint& pt)
{
	if (m_bScaling)
	{
		CSize sizeDevice = m_rectDevice.Size();

		pt.x = MulDiv(pt.x, sizeDevice.cx, m_sizeLogical.cx);
		pt.y = MulDiv(pt.y, sizeDevice.cy, m_sizeLogical.cy);
	}
	if (pt.x > m_rectClip.right)
		pt.x = m_rectClip.right;
	if (pt.y > m_rectClip.bottom)
		pt.y = m_rectClip.bottom;
}

void CDockState::ScaleRectPos(CRect& rect)
{
	CPoint pt;

	if (m_bScaling)
	{
		pt = rect.TopLeft();
		CSize sizeDevice = m_rectDevice.Size();

		pt.x = MulDiv(pt.x, sizeDevice.cx, m_sizeLogical.cx) - rect.left;
		pt.y = MulDiv(pt.y, sizeDevice.cy, m_sizeLogical.cy) - rect.top;
		rect.OffsetRect(pt);
	}
	pt.x = pt.y = 0;

	if (rect.left > m_rectClip.right)
		pt.x = m_rectClip.right - rect.left;
	if (rect.top > m_rectClip.bottom)
		pt.y = m_rectClip.bottom - rect.top;

	if (!((pt.x == 0) && (pt.y == 0)))
		rect.OffsetRect(pt);
}

CSize CDockState::GetScreenSize()
{
	return m_rectDevice.Size();
}

void CDockState::SetScreenSize(CSize& size)
{
	m_sizeLogical = size;
	m_bScaling = (size != m_rectDevice.Size());
}

void CFrameWnd::LoadBarState(LPCTSTR lpszProfileName)
{
	CDockState state;
	state.LoadState(lpszProfileName);
	SetDockState(state);
}

void CFrameWnd::SaveBarState(LPCTSTR lpszProfileName) const
{
	CDockState state;
	GetDockState(state);
	state.SaveState(lpszProfileName);
}

void CFrameWnd::SetDockState(const CDockState& state)
{
	// first pass through barinfo's sets the m_pBar member correctly
	// creating floating frames if necessary
	for (int i = 0; i < state.m_arrBarInfo.GetSize(); i++)
	{
		CControlBarInfo* pInfo = (CControlBarInfo*)state.m_arrBarInfo[i];
		ASSERT(pInfo != NULL);
		if (pInfo->m_bFloating)
		{
			// need to create floating frame to match
			CMiniDockFrameWnd* pDockFrame = CreateFloatingFrame(
				pInfo->m_bHorz ? CBRS_ALIGN_TOP : CBRS_ALIGN_LEFT);
			ASSERT(pDockFrame != NULL);
			CRect rect(pInfo->m_pointPos, CSize(10, 10));
			pDockFrame->CalcWindowRect(&rect);
			pDockFrame->SetWindowPos(NULL, rect.left, rect.top, 0, 0,
				SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
			CDockBar* pDockBar =
				(CDockBar*)pDockFrame->GetDlgItem(AFX_IDW_DOCKBAR_FLOAT);
			ASSERT(pDockBar != NULL);
			ASSERT_KINDOF(CDockBar, pDockBar);
			pInfo->m_pBar = pDockBar;
		}
		else // regular dock bar or toolbar
		{
			pInfo->m_pBar = GetControlBar(pInfo->m_nBarID);
			ASSERT(pInfo->m_pBar != NULL); //toolbar id's probably changed
		}
		pInfo->m_pBar->m_nMRUWidth = pInfo->m_nMRUWidth;
	}

	// the second pass will actually dock all of the control bars and
	//  set everything correctly
	for (i = 0; i < state.m_arrBarInfo.GetSize(); i++)
	{
		CControlBarInfo* pInfo = (CControlBarInfo*)state.m_arrBarInfo[i];
		ASSERT(pInfo != NULL);
		if (pInfo->m_pBar != NULL)
			pInfo->m_pBar->SetBarInfo(pInfo, this);
	}

	// last pass shows all the floating windows that were previously shown
	for (i = 0; i < state.m_arrBarInfo.GetSize(); i++)
	{
		CControlBarInfo* pInfo = (CControlBarInfo*)state.m_arrBarInfo[i];
		ASSERT(pInfo != NULL);
		ASSERT(pInfo->m_pBar != NULL);
		if (pInfo->m_bFloating)
		{
			CFrameWnd* pFrameWnd = pInfo->m_pBar->GetParentFrame();
			CDockBar* pDockBar = (CDockBar*)pInfo->m_pBar;
			ASSERT_KINDOF(CDockBar, pDockBar);
			if (pDockBar->GetDockedVisibleCount() > 0)
			{
				pFrameWnd->RecalcLayout();
				pFrameWnd->ShowWindow(SW_SHOWNA);
			}
		}
	}
	DelayRecalcLayout();
}

void CFrameWnd::GetDockState(CDockState& state) const
{
	state.Clear(); //make sure dockstate is empty
	// get state info for each bar
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CControlBar* pBar = (CControlBar*)m_listControlBars.GetNext(pos);
		ASSERT(pBar != NULL);
		CControlBarInfo* pInfo = new CControlBarInfo;
		pBar->GetBarInfo(pInfo);
		state.m_arrBarInfo.Add(pInfo);
	}
}

// Note: GetBarInfo and SetBarInfo are not virtual since doing so adds
//  to much code to an application which does not save and load docking
//  state.  For this reason, the CControlBar implementations must
//  delagate to CDockBar as appropriate.

void CControlBar::GetBarInfo(CControlBarInfo* pInfo)
{
	ASSERT_VALID(this);

	// get state info
	pInfo->m_nBarID = _AfxGetDlgCtrlID(m_hWnd);
	pInfo->m_pBar = this;
	pInfo->m_bVisible = IsVisible(); // handles delayed showing and hiding
	pInfo->m_nMRUWidth = m_nMRUWidth;

	if (m_pDockBar != NULL) // don't need position unless docked
	{
		CRect rect;
		GetWindowRect(&rect);
		m_pDockBar->ScreenToClient(&rect);
		pInfo->m_pointPos = rect.TopLeft();

		ASSERT(m_pDockContext != NULL);
		pInfo->m_bDocking = TRUE;
		pInfo->m_uMRUDockID = m_pDockContext->m_uMRUDockID;
		pInfo->m_rectMRUDockPos = m_pDockContext->m_rectMRUDockPos;
		pInfo->m_dwMRUFloatStyle = m_pDockContext->m_dwMRUFloatStyle;
		pInfo->m_ptMRUFloatPos = m_pDockContext->m_ptMRUFloatPos;
	}

	// save dockbar specific parts
	if (IsDockBar())
		((CDockBar*)this)->GetBarInfo(pInfo);
}

void CControlBar::SetBarInfo(CControlBarInfo* pInfo, CFrameWnd* pFrameWnd)
{
	// dockbars are handled differently
	if (IsDockBar())
	{
		((CDockBar*)this)->SetBarInfo(pInfo, pFrameWnd);
		return;
	}

	// don't set position when not docked
	UINT nFlags = SWP_NOSIZE|SWP_NOACTIVATE|SWP_NOZORDER;
	if (m_pDockBar == NULL)
		nFlags |= SWP_NOMOVE;

	m_nMRUWidth = pInfo->m_nMRUWidth;
	CalcDynamicLayout(0, LM_HORZ | LM_MRUWIDTH | LM_COMMIT);

	if (pInfo->m_bDocking)
	{
		ASSERT(m_pDockContext != NULL);
		// You need to call EnableDocking before calling LoadBarState

		m_pDockContext->m_uMRUDockID = pInfo->m_uMRUDockID;
		m_pDockContext->m_rectMRUDockPos = pInfo->m_rectMRUDockPos;
		m_pDockContext->m_dwMRUFloatStyle = pInfo->m_dwMRUFloatStyle;
		m_pDockContext->m_ptMRUFloatPos = pInfo->m_ptMRUFloatPos;
	}

	// move and show/hide the window
	SetWindowPos(NULL, pInfo->m_pointPos.x, pInfo->m_pointPos.y, 0, 0,
		nFlags | (pInfo->m_bVisible ? SWP_SHOWWINDOW : SWP_HIDEWINDOW));
}

void CDockBar::GetBarInfo(CControlBarInfo* pInfo)
{
	ASSERT_VALID(this);

	pInfo->m_bDockBar = TRUE;
	pInfo->m_bFloating = m_bFloating;
	if (m_bFloating)
	{
		CRect rect;
		GetWindowRect(&rect);
		pInfo->m_pointPos = rect.TopLeft();
	}
	pInfo->m_bHorz = m_dwStyle & CBRS_ORIENT_HORZ ? TRUE : FALSE;
	for (int i = 0; i < m_arrBars.GetSize(); i++)
	{
		CControlBar* pBar = (CControlBar*)m_arrBars[i];
		if (pBar != NULL && HIWORD(pBar) == 0)
		{
			WORD w = LOWORD(((DWORD)pBar));
			void* pRememberedID = (void *)MAKELONG(w, 1);
			pInfo->m_arrBarID.Add(pRememberedID);
		}
		else
		{
			pInfo->m_arrBarID.Add(pBar == NULL ?
				0 : (void*)_AfxGetDlgCtrlID(pBar->m_hWnd));
		}
	}
}

void CDockBar::SetBarInfo(CControlBarInfo* pInfo, CFrameWnd* pFrameWnd)
{
	ASSERT(pFrameWnd != NULL);
	ASSERT_VALID(this);

	int nSize = pInfo->m_arrBarID.GetSize();
	// don't insert trailing NULLs
	while (nSize != 0 && (pInfo->m_arrBarID[nSize-1] == NULL ||
		pInfo->m_arrBarID[nSize-1] == (void*)MAKELONG(0, 1)))
	{
		nSize--;
	}
	// start at 1 to avoid inserting leading NULL
	for (int i = 1; i < nSize; i++)
	{
		CControlBar* pBar = NULL;
		if (HIWORD(pInfo->m_arrBarID[i]) == 0)
		{
			pBar = pFrameWnd->GetControlBar((UINT)pInfo->m_arrBarID[i]);
			if (pBar != NULL)
			{
				if (pBar->GetParent() != this)
					pBar->SetParent(this);
				if (pBar->m_pDockBar != NULL)
					pBar->m_pDockBar->RemoveControlBar(pBar, -1, -1);
				//remove the ID place holder if it exists in this dockbar
				RemovePlaceHolder(pBar);
				pBar->m_pDockBar = this;

				// align correctly and turn on all borders
				DWORD dwStyle = pBar->GetBarStyle();
				dwStyle &= ~(CBRS_ALIGN_ANY);
				dwStyle |= (m_dwStyle & CBRS_ALIGN_ANY);
				dwStyle |= CBRS_BORDER_ANY;
				if (m_bFloating)
					dwStyle |= CBRS_FLOATING;
				else
					dwStyle &= ~CBRS_FLOATING;
				pBar->SetBarStyle(dwStyle);

				// handle special case for floating toolbars
				if (m_bFloating)
				{
					// set CBRS_FLOAT_MULTI style if docking bar has it
					if (pBar->m_dwDockStyle & CBRS_FLOAT_MULTI)
						m_dwStyle |= CBRS_FLOAT_MULTI;

					// set owner of parent frame as appropriate
					CFrameWnd* pDockFrame = pBar->GetParentFrame();
					ASSERT_VALID(pDockFrame);
					ASSERT(pDockFrame != pBar->m_pDockSite);
					if (pDockFrame->m_hWndOwner == NULL)
						pDockFrame->m_hWndOwner = pBar->m_hWnd;

					if (pBar->m_dwStyle & CBRS_SIZE_DYNAMIC)
						pDockFrame->ModifyStyle(MFS_MOVEFRAME, 0);
				}

				// set initial text of the dock bar
				if (i == 1 && !(m_dwStyle & CBRS_FLOAT_MULTI))
				{
					CString strTitle;
					pBar->GetWindowText(strTitle);
					AfxSetWindowText(m_hWnd, strTitle);
				}
			}
		}
		else
		{
			WORD w = LOWORD(((DWORD)pInfo->m_arrBarID[i]));
			pBar = (CControlBar*)(MAKELONG(w, 0));
			RemovePlaceHolder(pBar);
		}
		m_arrBars.InsertAt(i, pBar);
	}
	int nArrSize = m_arrBars.GetSize();
	if (nSize < nArrSize && m_arrBars[nSize] != NULL)
	{
		m_arrBars.InsertAt(nSize, (void*)NULL);
		nArrSize++;
	}
	if (m_arrBars[nArrSize-1] != NULL)
		m_arrBars.InsertAt(nArrSize, (void*)NULL);
	ASSERT_VALID(this);
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_SERIAL(CDockState, CObject, 0)

/////////////////////////////////////////////////////////////////////////////
