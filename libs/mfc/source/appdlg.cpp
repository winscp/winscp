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

/////////////////////////////////////////////////////////////////////////////
// WinApp features for new and open

void CWinApp::OnFileNew()
{
	if (m_pDocManager != NULL)
		m_pDocManager->OnFileNew();
}

/////////////////////////////////////////////////////////////////////////////

void CWinApp::OnFileOpen()
{
	ASSERT(m_pDocManager != NULL);
	m_pDocManager->OnFileOpen();
}

// prompt for file name - used for open and save as
BOOL CWinApp::DoPromptFileName(CString& fileName, UINT nIDSTitle, DWORD lFlags,
	BOOL bOpenFileDialog, CDocTemplate* pTemplate)
		// if pTemplate==NULL => all document templates
{
	ASSERT(m_pDocManager != NULL);
	return m_pDocManager->DoPromptFileName(fileName, nIDSTitle, lFlags,
		bOpenFileDialog, pTemplate);
}

/////////////////////////////////////////////////////////////////////////////
