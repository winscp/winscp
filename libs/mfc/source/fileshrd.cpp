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

#ifdef AFX_CORE4_SEG
#pragma code_seg(AFX_CORE4_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

////////////////////////////////////////////////////////////////////////////
// CSharedFile implementation

CSharedFile::CSharedFile(UINT nAllocFlags, UINT nGrowBytes)
	: CMemFile(nGrowBytes)
{
	m_nAllocFlags = nAllocFlags;
	m_hGlobalMemory = NULL;
	m_bAllowGrow = TRUE;
}

CSharedFile::~CSharedFile()
{
	if (m_lpBuffer)
		Close();        // call appropriate Close/Free
	ASSERT(m_lpBuffer == NULL);
}

void CSharedFile::SetHandle(HGLOBAL hGlobalMemory, BOOL bAllowGrow)
{
	ASSERT(m_hGlobalMemory == NULL);        // do once only
	ASSERT(m_lpBuffer == NULL);     // do once only
	ASSERT(m_nPosition == 0);

	m_hGlobalMemory = hGlobalMemory;
	m_lpBuffer = (BYTE*)::GlobalLock(m_hGlobalMemory);
	m_nBufferSize = m_nFileSize = ::GlobalSize(m_hGlobalMemory);
	m_bAllowGrow = bAllowGrow;
}

BYTE* CSharedFile::Alloc(DWORD nBytes)
{
	ASSERT(m_hGlobalMemory == NULL);        // do once only
	m_hGlobalMemory = ::GlobalAlloc(m_nAllocFlags, nBytes);
	if (m_hGlobalMemory == NULL)
		return NULL;
	return (BYTE*)::GlobalLock(m_hGlobalMemory);
}

BYTE* CSharedFile::Realloc(BYTE*, DWORD nBytes)
{
	if (!m_bAllowGrow)
		return NULL;
	ASSERT(m_hGlobalMemory != NULL);
	::GlobalUnlock(m_hGlobalMemory);
	HGLOBAL hNew;
	hNew = ::GlobalReAlloc(m_hGlobalMemory, nBytes, m_nAllocFlags);
	if (hNew == NULL)
		return NULL;
	m_hGlobalMemory = hNew;
	return (BYTE*)::GlobalLock(m_hGlobalMemory);
}

void CSharedFile::Free(BYTE*)
{
	ASSERT(m_hGlobalMemory != NULL);
	::GlobalUnlock(m_hGlobalMemory);
	::GlobalFree(m_hGlobalMemory);
}

HGLOBAL CSharedFile::Detach()
{
	HGLOBAL hMem;
	ASSERT(m_hGlobalMemory != NULL);
	hMem = m_hGlobalMemory;

	m_hGlobalMemory = NULL; // detach from global handle

	// re-initialize the CMemFile parts too
	m_lpBuffer = NULL;
	m_nBufferSize = 0;

	return hMem;
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CSharedFile, CMemFile)

////////////////////////////////////////////////////////////////////////////
