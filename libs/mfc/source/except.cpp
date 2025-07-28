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

/////////////////////////////////////////////////////////////////////////////
// AFX_EXCEPTION_CONTEXT (thread global state)

// WINSCP
AFX_EXCEPTION_CONTEXT __thread m_exceptionContext;

inline AFX_EXCEPTION_CONTEXT* AfxGetExceptionContext()
{
	DWORD lError = GetLastError();
	AFX_EXCEPTION_CONTEXT* pContext = &m_exceptionContext;
	SetLastError(lError);
	return pContext;
}

/////////////////////////////////////////////////////////////////////////////
// CException

CException::CException()
{
}

void CException::Delete()
{
	delete this;
}

BOOL CException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
	PUINT pnHelpContext /* = NULL */ )
{
	if (pnHelpContext != NULL)
		*pnHelpContext = 0;

	if (nMaxError != 0 && lpszError != NULL)
		*lpszError = '\0';

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// AFX_EXCEPTION_LINK linked 'jmpbuf' and out-of-line helpers

AFX_EXCEPTION_LINK::AFX_EXCEPTION_LINK()
{
	// setup initial link state
	m_pException = NULL;    // no current exception yet

	// wire into top of exception link stack
	AFX_EXCEPTION_CONTEXT* pContext = AfxGetExceptionContext();
	m_pLinkPrev = pContext->m_pLinkTop;
	pContext->m_pLinkTop = this;
}

// out-of-line cleanup called from inline AFX_EXCEPTION_LINK destructor
void AFXAPI AfxTryCleanup()
{
	AFX_EXCEPTION_CONTEXT* pContext = AfxGetExceptionContext();
	AFX_EXCEPTION_LINK* pLinkTop = pContext->m_pLinkTop;

	// delete current exception
	ASSERT(pLinkTop != NULL);
	if (pLinkTop->m_pException != NULL)
		pLinkTop->m_pException->Delete();

	// remove ourself from the top of the chain
	pContext->m_pLinkTop = pLinkTop->m_pLinkPrev;
}

// special out-of-line implementation of THROW_LAST (for auto-delete behavior)
void AFXAPI AfxThrowLastCleanup()
{
	AFX_EXCEPTION_CONTEXT* pContext = AfxGetExceptionContext();
	AFX_EXCEPTION_LINK* pLinkTop = pContext->m_pLinkTop;

	// check for THROW_LAST inside of auto-delete block
	if (pLinkTop != NULL)
	{
		// make sure current exception does not get auto-deleted
		pLinkTop->m_pException = NULL;
	}

	// THROW_LAST macro will do actual 'throw'
}

