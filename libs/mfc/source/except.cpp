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

#ifdef AFX_CORE1_SEG
#pragma code_seg(AFX_CORE1_SEG)
#endif

#ifdef _DEBUG
void PASCAL CException::operator delete(void* pbData)
{
	// check for proper exception object deletion
	CException* pException = (CException*)pbData;
	// use: pException->Delete(), do not use: delete pException
	ASSERT(pException->m_bReadyForDelete);
	ASSERT(pException->m_bAutoDelete > 0);

	// avoid crash when assert above is ignored
	if (pException->m_bReadyForDelete && pException->m_bAutoDelete > 0)
		CObject::operator delete(pbData);
}

#if 0 // __BORLANDC__ was _MSC_VER >= 1200
void PASCAL CException::operator delete(void* pbData,
	LPCSTR /* lpszFileName */, int /* nLine */)
{
	operator delete(pbData);
}
#endif

#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#define new DEBUG_NEW
#endif

/////////////////////////////////////////////////////////////////////////////
// AFX_EXCEPTION_CONTEXT (thread global state)

inline AFX_EXCEPTION_CONTEXT* AfxGetExceptionContext()
{
	DWORD lError = GetLastError();
	AFX_EXCEPTION_CONTEXT* pContext = &_afxThreadState->m_exceptionContext;
	SetLastError(lError);
	return pContext;
}

/////////////////////////////////////////////////////////////////////////////
// CException

CException::CException()
{
	// most exceptions are deleted when not needed
	m_bAutoDelete = TRUE;
#ifdef _DEBUG
	m_bReadyForDelete = FALSE;
#endif
}

CException::CException(BOOL bAutoDelete)
{
	// for exceptions which are not auto-delete (usually)
	m_bAutoDelete = bAutoDelete;
#ifdef _DEBUG
	m_bReadyForDelete = FALSE;
#endif
}

void CException::Delete()
{
	// delete exception if it is auto-deleting
	if (m_bAutoDelete > 0)
	{
#ifdef _DEBUG
		m_bReadyForDelete = TRUE;
#endif
		delete this;
	}
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

int CException::ReportError(UINT nType /* = MB_OK */,
	UINT nError /* = 0 */)
{
	TCHAR   szErrorMessage[512];
	int     nDisposition;
	UINT    nHelpContext;

	if (GetErrorMessage(szErrorMessage, _countof(szErrorMessage), &nHelpContext))
		nDisposition = AfxMessageBox(szErrorMessage, nType, nHelpContext);
	else
	{
		if (nError == 0)
			nError = AFX_IDP_NO_ERROR_AVAILABLE;
		nDisposition = AfxMessageBox(nError, nType, nHelpContext);
	}
	return nDisposition;
}

/////////////////////////////////////////////////////////////////////////////
// AFX_EXCEPTION_LINK linked 'jmpbuf' and out-of-line helpers

AFX_EXCEPTION_LINK::AFX_EXCEPTION_LINK()
{
	// setup initial link state
#ifdef _AFX_OLD_EXCEPTIONS
	m_nType = 0;
#endif
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

#ifndef _AFX_OLD_EXCEPTIONS
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
#endif //!_AFX_OLD_EXCEPTIONS

/////////////////////////////////////////////////////////////////////////////
// Global exception terminate handling - Obsolete API
//  (apps written to C++ exceptions should use set_terminate)

#ifdef _AFX_OLD_EXCEPTIONS

void AFXAPI AfxTerminate()
{
	TRACE0("AfxTerminate called.\n");
	(*AfxGetModuleState()->m_pfnTerminate)();
}

AFX_TERM_PROC AFXAPI AfxSetTerminate(AFX_TERM_PROC pfnNew)
{
	AFX_MODULE_STATE* pModuleState = AfxGetModuleState();
	AFX_TERM_PROC pfnOld = pModuleState->m_pfnTerminate;
	pModuleState->m_pfnTerminate = pfnNew;
	return pfnOld;
}

#endif //_AFX_OLD_EXCEPTIONS

/////////////////////////////////////////////////////////////////////////////
// Special non-C++ exception implementation

#ifdef _AFX_OLD_EXCEPTIONS
// out-of-line implementation of THROW (for non-C++ exceptions)
void AFXAPI AfxThrow(CException* pNewException)
{
	// get current exception context for running task
	AFX_EXCEPTION_CONTEXT* pContext = AfxGetExceptionContext();

	// check for THROW_LAST() first
	if (pNewException == NULL)
	{
		ASSERT(pContext->m_pLinkTop != NULL);
		pNewException = pContext->m_pLinkTop->m_pException;
	}
	ASSERT_VALID(pNewException);

	TRACE1("Warning: Throwing an Exception of type %hs.\n",
		pNewException->GetRuntimeClass()->m_lpszClassName);

	while (pContext->m_pLinkTop != NULL)
	{
		AFX_EXCEPTION_LINK* pReceiver = pContext->m_pLinkTop;
		if (pReceiver->m_pException != NULL)
		{
			// a THROW during a CATCH block -- this link may not be
			//  destructed, so it is necessary to do all the cleanup that
			//  the destructor would do.
			if (pReceiver->m_pException != pNewException)
				pReceiver->m_pException->Delete();
			pReceiver->m_pException = NULL;
			pContext->m_pLinkTop = pReceiver->m_pLinkPrev;
		}
		else
		{
			// throw the exception to the top handler (if appropriate type)
			if (pReceiver->m_nType == 0)
			{
				// setup the receiver's context for the new exception
				pReceiver->m_pException = pNewException;

				// and jump into the handler...
				longjmp(pReceiver->m_jumpBuf, 1);
				ASSERT(FALSE);  // not reached
			}
			// otherwise just call cleanup proc
			(*pReceiver->m_callback.pfnCleanup)(pReceiver);
		}
	}

	ASSERT(pContext->m_pLinkTop == NULL);
	// uncaught exception, terminate
	TRACE1("Error: Uncaught Exception (%hs).\n",
		pNewException->GetRuntimeClass()->m_lpszClassName);
	AfxTerminate();
	ASSERT(FALSE);  // not reached
}

// out-of-line implementation of CATCH and AND_CATCH
BOOL AFXAPI AfxCatchProc(CRuntimeClass* pClass)
{
	ASSERT(pClass != NULL);

	AFX_EXCEPTION_CONTEXT* pContext = AfxGetExceptionContext();
	ASSERT(pContext->m_pLinkTop != NULL);
	CException* pException = pContext->m_pLinkTop->m_pException;
	ASSERT(pException != NULL);

	return pException->IsKindOf(pClass);
}
#endif //_AFX_OLD_EXCEPTIONS

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CException, CObject)

IMPLEMENT_DYNAMIC(CMemoryException, CException)
CMemoryException _simpleMemoryException(FALSE, AFX_IDS_MEMORY_EXCEPTION);

IMPLEMENT_DYNAMIC(CNotSupportedException, CException)
CNotSupportedException _simpleNotSupportedException(FALSE, AFX_IDS_NOT_SUPPORTED_EXCEPTION);

/////////////////////////////////////////////////////////////////////////////
// Standard exceptions

#ifdef AFX_AUX_SEG
#pragma code_seg(AFX_AUX_SEG)
#endif

void CSimpleException::InitString()
{
	m_bInitialized = TRUE;
	m_bLoaded = (AfxLoadString(m_nResourceID,
		m_szMessage, _countof(m_szMessage)) != 0);
}

BOOL CSimpleException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext)
{
	ASSERT(lpszError != NULL && AfxIsValidString(lpszError, nMaxError));

	if (pnHelpContext != NULL)
		*pnHelpContext = 0;

	// if we didn't load our string (eg, we're a console app)
	// return a null string and FALSE

	if (!m_bInitialized)
		InitString();

	if (m_bLoaded)
		lstrcpyn(lpszError, m_szMessage, nMaxError);
	else
		lpszError[0] = '\0';

	return m_bLoaded;
}

void AFXAPI AfxThrowMemoryException()
{
	THROW(&_simpleMemoryException);
}

void AFXAPI AfxThrowNotSupportedException()
{
	THROW(&_simpleNotSupportedException);
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

////////////////////////////////////////////////////////////////////////////
// out-of-line inlines for binary compatibility

#ifdef _AFXDLL
#ifndef _DEBUG

CSimpleException::~CSimpleException()
	{ }

#endif
#endif

/////////////////////////////////////////////////////////////////////////////
