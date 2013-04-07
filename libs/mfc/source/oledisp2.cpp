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
#include <stdarg.h>

#ifdef AFX_OLE5_SEG
#pragma code_seg(AFX_OLE5_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleDispatchDriver constructors/destructors

HRESULT AFXAPI AfxGetClassIDFromString(LPCTSTR lpsz, LPCLSID lpClsID)
{
	USES_CONVERSION;

	HRESULT hr;
	if (lpsz[0] == '{')
		hr = CLSIDFromString((LPOLESTR)T2COLE(lpsz), lpClsID);
	else
		hr = CLSIDFromProgID(T2COLE(lpsz), lpClsID);
	return hr;
}

COleDispatchDriver::COleDispatchDriver()
{
	m_lpDispatch = NULL;
	m_bAutoRelease = TRUE;
}

COleDispatchDriver::COleDispatchDriver(LPDISPATCH lpDispatch, BOOL bAutoRelease)
{
	m_lpDispatch = lpDispatch;
	m_bAutoRelease = bAutoRelease;
}

COleDispatchDriver::COleDispatchDriver(const COleDispatchDriver& dispatchSrc)
{
	ASSERT(this != &dispatchSrc);   // constructing from self?

	m_lpDispatch = dispatchSrc.m_lpDispatch;
	if (m_lpDispatch != NULL)
		m_lpDispatch->AddRef();
	m_bAutoRelease = TRUE;
}

const COleDispatchDriver&
COleDispatchDriver::operator=(const COleDispatchDriver& dispatchSrc)
{
	if (this != &dispatchSrc)
	{
		LPDISPATCH lpTemp = m_lpDispatch;
		m_lpDispatch = dispatchSrc.m_lpDispatch;
		if (m_lpDispatch != NULL)
			m_lpDispatch->AddRef();
		if (lpTemp != NULL && m_bAutoRelease)
			lpTemp->Release();
		m_bAutoRelease = TRUE;
	}
	return *this;
}

BOOL COleDispatchDriver::CreateDispatch(REFCLSID clsid, COleException* pError)
{
	ASSERT(m_lpDispatch == NULL);

	m_bAutoRelease = TRUE;  // good default is to auto-release

	// create an instance of the object
	LPUNKNOWN lpUnknown = NULL;
	SCODE sc = CoCreateInstance(clsid, NULL, CLSCTX_ALL | CLSCTX_REMOTE_SERVER,
		IID_IUnknown, (LPLP)&lpUnknown);
	if (sc == E_INVALIDARG)
	{
		// may not support CLSCTX_REMOTE_SERVER, so try without
		sc = CoCreateInstance(clsid, NULL, CLSCTX_ALL & ~CLSCTX_REMOTE_SERVER,
			IID_IUnknown, (LPLP)&lpUnknown);
	}
	if (FAILED(sc))
		goto Failed;

	// make sure it is running
	sc = OleRun(lpUnknown);
	if (FAILED(sc))
		goto Failed;

	// query for IDispatch interface
	m_lpDispatch = QUERYINTERFACE(lpUnknown, IDispatch);
	if (m_lpDispatch == NULL)
		goto Failed;

	lpUnknown->Release();
	ASSERT(m_lpDispatch != NULL);
	return TRUE;

Failed:
	RELEASE(lpUnknown);
	if (pError != NULL)
		pError->m_sc = sc;
	TRACE1("Warning: CreateDispatch returning scode = %s.\n",
		AfxGetFullScodeString(sc));
	return FALSE;
}

BOOL COleDispatchDriver::CreateDispatch(LPCTSTR lpszProgID,
	COleException* pError)
{
	ASSERT(m_lpDispatch == NULL);

	// map prog id to CLSID
	CLSID clsid;
	SCODE sc = AfxGetClassIDFromString(lpszProgID, &clsid);
	if (FAILED(sc))
	{
		if (pError != NULL)
			pError->m_sc = sc;
		return FALSE;
	}

	// create with CLSID
	return CreateDispatch(clsid, pError);
}

void COleDispatchDriver::AttachDispatch(LPDISPATCH lpDispatch,
	BOOL bAutoRelease)
{
	ASSERT(lpDispatch != NULL);

	ReleaseDispatch();  // detach previous
	m_lpDispatch = lpDispatch;
	m_bAutoRelease = bAutoRelease;
}

void COleDispatchDriver::ReleaseDispatch()
{
	if (m_lpDispatch != NULL)
	{
		if (m_bAutoRelease)
			m_lpDispatch->Release();
		m_lpDispatch = NULL;
	}
}

LPDISPATCH COleDispatchDriver::DetachDispatch()
{
	LPDISPATCH lpDispatch = m_lpDispatch;
	m_lpDispatch = NULL;    // detach without Release
	return lpDispatch;
}

/////////////////////////////////////////////////////////////////////////////
// COleDispatchDriver implementation

void COleDispatchDriver::InvokeHelperV(DISPID dwDispID, WORD wFlags,
	VARTYPE vtRet, void* pvRet, const BYTE* pbParamInfo, va_list argList)
{
	USES_CONVERSION;

	if (m_lpDispatch == NULL)
	{
		TRACE0("Warning: attempt to call Invoke with NULL m_lpDispatch!\n");
		return;
	}

	DISPPARAMS dispparams;
	memset(&dispparams, 0, sizeof dispparams);

	// determine number of arguments
	if (pbParamInfo != NULL)
		dispparams.cArgs = lstrlenA((LPCSTR)pbParamInfo);

	DISPID dispidNamed = DISPID_PROPERTYPUT;
	if (wFlags & (DISPATCH_PROPERTYPUT|DISPATCH_PROPERTYPUTREF))
	{
		ASSERT(dispparams.cArgs > 0);
		dispparams.cNamedArgs = 1;
		dispparams.rgdispidNamedArgs = &dispidNamed;
	}

	if (dispparams.cArgs != 0)
	{
		// allocate memory for all VARIANT parameters
		VARIANT* pArg = new VARIANT[dispparams.cArgs];
		ASSERT(pArg != NULL);   // should have thrown exception
		dispparams.rgvarg = pArg;
		memset(pArg, 0, sizeof(VARIANT) * dispparams.cArgs);

		// get ready to walk vararg list
		const BYTE* pb = pbParamInfo;
		pArg += dispparams.cArgs - 1;   // params go in opposite order

		while (*pb != 0)
		{
			ASSERT(pArg >= dispparams.rgvarg);

			pArg->vt = *pb; // set the variant type
			if (pArg->vt & VT_MFCBYREF)
			{
				pArg->vt &= ~VT_MFCBYREF;
				pArg->vt |= VT_BYREF;
			}
			switch (pArg->vt)
			{
			case VT_UI1:
				pArg->bVal = va_arg(argList, BYTE);
				break;
			case VT_I2:
				pArg->iVal = va_arg(argList, short);
				break;
			case VT_I4:
				pArg->lVal = va_arg(argList, long);
				break;
			case VT_R4:
				pArg->fltVal = (float)va_arg(argList, double);
				break;
			case VT_R8:
				pArg->dblVal = va_arg(argList, double);
				break;
			case VT_DATE:
				pArg->date = va_arg(argList, DATE);
				break;
			case VT_CY:
				pArg->cyVal = *va_arg(argList, CY*);
				break;
			case VT_BSTR:
				{
					LPCOLESTR lpsz = va_arg(argList, LPOLESTR);
					pArg->bstrVal = ::SysAllocString(lpsz);
					if (lpsz != NULL && pArg->bstrVal == NULL)
						AfxThrowMemoryException();
				}
				break;
#if !defined(_UNICODE) && !defined(OLE2ANSI)
			case VT_BSTRA:
				{
					LPCSTR lpsz = va_arg(argList, LPSTR);
					pArg->bstrVal = ::SysAllocString(T2COLE(lpsz));
					if (lpsz != NULL && pArg->bstrVal == NULL)
						AfxThrowMemoryException();
					pArg->vt = VT_BSTR;
				}
				break;
#endif
			case VT_DISPATCH:
				pArg->pdispVal = va_arg(argList, LPDISPATCH);
				break;
			case VT_ERROR:
				pArg->scode = va_arg(argList, SCODE);
				break;
			case VT_BOOL:
				V_BOOL(pArg) = (VARIANT_BOOL)(va_arg(argList, BOOL) ? -1 : 0);
				break;
			case VT_VARIANT:
				*pArg = *va_arg(argList, VARIANT*);
				break;
			case VT_UNKNOWN:
				pArg->punkVal = va_arg(argList, LPUNKNOWN);
				break;

			case VT_I2|VT_BYREF:
				pArg->piVal = va_arg(argList, short*);
				break;
			case VT_UI1|VT_BYREF:
				pArg->pbVal = va_arg(argList, BYTE*);
				break;
			case VT_I4|VT_BYREF:
				pArg->plVal = va_arg(argList, long*);
				break;
			case VT_R4|VT_BYREF:
				pArg->pfltVal = va_arg(argList, float*);
				break;
			case VT_R8|VT_BYREF:
				pArg->pdblVal = va_arg(argList, double*);
				break;
			case VT_DATE|VT_BYREF:
				pArg->pdate = va_arg(argList, DATE*);
				break;
			case VT_CY|VT_BYREF:
				pArg->pcyVal = va_arg(argList, CY*);
				break;
			case VT_BSTR|VT_BYREF:
				pArg->pbstrVal = va_arg(argList, BSTR*);
				break;
			case VT_DISPATCH|VT_BYREF:
				pArg->ppdispVal = va_arg(argList, LPDISPATCH*);
				break;
			case VT_ERROR|VT_BYREF:
				pArg->pscode = va_arg(argList, SCODE*);
				break;
			case VT_BOOL|VT_BYREF:
				{
					// coerce BOOL into VARIANT_BOOL
					BOOL* pboolVal = va_arg(argList, BOOL*);
					*pboolVal = *pboolVal ? MAKELONG(-1, 0) : 0;
					pArg->pboolVal = (VARIANT_BOOL*)pboolVal;
				}
				break;
			case VT_VARIANT|VT_BYREF:
				pArg->pvarVal = va_arg(argList, VARIANT*);
				break;
			case VT_UNKNOWN|VT_BYREF:
				pArg->ppunkVal = va_arg(argList, LPUNKNOWN*);
				break;

			default:
				ASSERT(FALSE);  // unknown type!
				break;
			}

			--pArg; // get ready to fill next argument
			++pb;
		}
	}

	// initialize return value
	VARIANT* pvarResult = NULL;
	VARIANT vaResult;
	AfxVariantInit(&vaResult);
	if (vtRet != VT_EMPTY)
		pvarResult = &vaResult;

	// initialize EXCEPINFO struct
	EXCEPINFO excepInfo;
	memset(&excepInfo, 0, sizeof excepInfo);

	UINT nArgErr = (UINT)-1;  // initialize to invalid arg

	// make the call
	SCODE sc = m_lpDispatch->Invoke(dwDispID, IID_NULL, 0, wFlags,
		&dispparams, pvarResult, &excepInfo, &nArgErr);

	// cleanup any arguments that need cleanup
	if (dispparams.cArgs != 0)
	{
		VARIANT* pArg = dispparams.rgvarg + dispparams.cArgs - 1;
		const BYTE* pb = pbParamInfo;
		while (*pb != 0)
		{
			switch ((VARTYPE)*pb)
			{
#if !defined(_UNICODE) && !defined(OLE2ANSI)
			case VT_BSTRA:
#endif
			case VT_BSTR:
				VariantClear(pArg);
				break;
			}
			--pArg;
			++pb;
		}
	}
	delete[] dispparams.rgvarg;

	// throw exception on failure
	if (FAILED(sc))
	{
		VariantClear(&vaResult);
		if (sc != DISP_E_EXCEPTION)
		{
			// non-exception error code
			AfxThrowOleException(sc);
		}

		// make sure excepInfo is filled in
		if (excepInfo.pfnDeferredFillIn != NULL)
			excepInfo.pfnDeferredFillIn(&excepInfo);

		// allocate new exception, and fill it
		COleDispatchException* pException =
			new COleDispatchException(NULL, 0, excepInfo.wCode);
		ASSERT(pException->m_wCode == excepInfo.wCode);
		if (excepInfo.bstrSource != NULL)
		{
			pException->m_strSource = excepInfo.bstrSource;
			SysFreeString(excepInfo.bstrSource);
		}
		if (excepInfo.bstrDescription != NULL)
		{
			pException->m_strDescription = excepInfo.bstrDescription;
			SysFreeString(excepInfo.bstrDescription);
		}
		if (excepInfo.bstrHelpFile != NULL)
		{
			pException->m_strHelpFile = excepInfo.bstrHelpFile;
			SysFreeString(excepInfo.bstrHelpFile);
		}
		pException->m_dwHelpContext = excepInfo.dwHelpContext;
		pException->m_scError = excepInfo.scode;

		// then throw the exception
		THROW(pException);
		ASSERT(FALSE);  // not reached
	}

	if (vtRet != VT_EMPTY)
	{
		// convert return value
		if (vtRet != VT_VARIANT)
		{
			SCODE sc = VariantChangeType(&vaResult, &vaResult, 0, vtRet);
			if (FAILED(sc))
			{
				TRACE0("Warning: automation return value coercion failed.\n");
				VariantClear(&vaResult);
				AfxThrowOleException(sc);
			}
			ASSERT(vtRet == vaResult.vt);
		}

		// copy return value into return spot!
		switch (vtRet)
		{
		case VT_UI1:
			*(BYTE*)pvRet = vaResult.bVal;
			break;
		case VT_I2:
			*(short*)pvRet = vaResult.iVal;
			break;
		case VT_I4:
			*(long*)pvRet = vaResult.lVal;
			break;
		case VT_R4:
			*(_AFX_FLOAT*)pvRet = *(_AFX_FLOAT*)&vaResult.fltVal;
			break;
		case VT_R8:
			*(_AFX_DOUBLE*)pvRet = *(_AFX_DOUBLE*)&vaResult.dblVal;
			break;
		case VT_DATE:
			*(_AFX_DOUBLE*)pvRet = *(_AFX_DOUBLE*)&vaResult.date;
			break;
		case VT_CY:
			*(CY*)pvRet = vaResult.cyVal;
			break;
		case VT_BSTR:
			AfxBSTR2CString((CString*)pvRet, vaResult.bstrVal);
			SysFreeString(vaResult.bstrVal);
			break;
		case VT_DISPATCH:
			*(LPDISPATCH*)pvRet = vaResult.pdispVal;
			break;
		case VT_ERROR:
			*(SCODE*)pvRet = vaResult.scode;
			break;
		case VT_BOOL:
			*(BOOL*)pvRet = (V_BOOL(&vaResult) != 0);
			break;
		case VT_VARIANT:
			*(VARIANT*)pvRet = vaResult;
			break;
		case VT_UNKNOWN:
			*(LPUNKNOWN*)pvRet = vaResult.punkVal;
			break;

		default:
			ASSERT(FALSE);  // invalid return type specified
		}
	}
}

void AFX_CDECL COleDispatchDriver::InvokeHelper(DISPID dwDispID, WORD wFlags,
	VARTYPE vtRet, void* pvRet, const BYTE* pbParamInfo, ...)
{
	va_list argList;
	va_start(argList, pbParamInfo);

	InvokeHelperV(dwDispID, wFlags, vtRet, pvRet, pbParamInfo, argList);

	va_end(argList);
}

void COleDispatchDriver::GetProperty(DISPID dwDispID, VARTYPE vtProp,
	void* pvProp) const
{
	((COleDispatchDriver*)this)->InvokeHelper(dwDispID,
		DISPATCH_PROPERTYGET, vtProp, pvProp, NULL);
}

void AFX_CDECL COleDispatchDriver::SetProperty(DISPID dwDispID, VARTYPE vtProp, ...)
{
	va_list argList;    // really only one arg, but...
	va_start(argList, vtProp);

	BYTE rgbParams[2];
	if (vtProp & VT_BYREF)
	{
		vtProp &= ~VT_BYREF;
		vtProp |= VT_MFCBYREF;
	}

#if !defined(_UNICODE) && !defined(OLE2ANSI)
		if (vtProp == VT_BSTR)
			vtProp = VT_BSTRA;
#endif

	rgbParams[0] = (BYTE)vtProp;
	rgbParams[1] = 0;
	WORD wFlags = (WORD)(vtProp == VT_DISPATCH ?
		DISPATCH_PROPERTYPUTREF : DISPATCH_PROPERTYPUT);
	InvokeHelperV(dwDispID, wFlags, VT_EMPTY, NULL, rgbParams, argList);

	va_end(argList);
}

/////////////////////////////////////////////////////////////////////////////
