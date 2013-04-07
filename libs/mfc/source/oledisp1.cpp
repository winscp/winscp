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
#include "dispimpl.h"

#ifdef AFX_OLE5_SEG
#pragma code_seg(AFX_OLE5_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// Helpers and main implementation for CCmdTarget::IDispatch

void CCmdTarget::GetStandardProp(const AFX_DISPMAP_ENTRY* pEntry,
	VARIANT* pvarResult, UINT* puArgErr)
{
	ASSERT(pEntry != NULL);
	ASSERT(*puArgErr != 0);

	// it is a DISPATCH_PROPERTYGET (for standard, non-function property)
	void* pProp = (BYTE*)this + pEntry->nPropOffset;
	if (pEntry->vt != VT_VARIANT)
		pvarResult->vt = pEntry->vt;
	switch (pEntry->vt)
	{
	case VT_I1:
		pvarResult->bVal = *(BYTE*)pProp;
		break;
	case VT_I2:
		pvarResult->iVal = *(short*)pProp;
		break;
	case VT_I4:
		pvarResult->lVal = *(long*)pProp;
		break;
	case VT_R4:
		pvarResult->fltVal = *(float*)pProp;
		break;
	case VT_R8:
		pvarResult->dblVal = *(double*)pProp;
		break;
	case VT_DATE:
		pvarResult->date = *(double*)pProp;
		break;
	case VT_CY:
		pvarResult->cyVal = *(CY*)pProp;
		break;
	case VT_BSTR:
		{
			CString* pString = (CString*)pProp;
			pvarResult->bstrVal = pString->AllocSysString();
		}
		break;
	case VT_ERROR:
		pvarResult->scode = *(SCODE*)pProp;
		break;
	case VT_BOOL:
		V_BOOL(pvarResult) = (VARIANT_BOOL)(*(BOOL*)pProp != 0 ? -1 : 0);
		break;
	case VT_VARIANT:
		if (VariantCopy(pvarResult, (LPVARIANT)pProp) != S_OK)
			*puArgErr = 0;
		break;
	case VT_DISPATCH:
	case VT_UNKNOWN:
		pvarResult->punkVal = *(LPDISPATCH*)pProp;
		if (pvarResult->punkVal != NULL)
			pvarResult->punkVal->AddRef();
		break;

	default:
		*puArgErr = 0;
	}
}

SCODE CCmdTarget::SetStandardProp(const AFX_DISPMAP_ENTRY* pEntry,
	DISPPARAMS* pDispParams, UINT* puArgErr)
{
	ASSERT(pEntry != NULL);
	ASSERT(*puArgErr != 0);

	// it is a DISPATCH_PROPERTYSET (for standard, non-function property)
	SCODE sc = S_OK;
	VARIANT va;
	AfxVariantInit(&va);
	VARIANT* pArg = &pDispParams->rgvarg[0];
	if (pEntry->vt != VT_VARIANT && pArg->vt != pEntry->vt)
	{
		// argument is not of appropriate type, attempt to coerce it
		sc = VariantChangeType(&va, pArg, 0, pEntry->vt);
		if (FAILED(sc))
		{
			TRACE0("Warning: automation property coercion failed.\n");
			*puArgErr = 0;
			return sc;
		}
		ASSERT(va.vt == pEntry->vt);
		pArg = &va;
	}

	void* pProp = (BYTE*)this + pEntry->nPropOffset;
	switch (pEntry->vt)
	{
	case VT_I1:
		*(BYTE*)pProp = pArg->bVal;
		break;
	case VT_I2:
		*(short*)pProp = pArg->iVal;
		break;
	case VT_I4:
		*(long*)pProp = pArg->lVal;
		break;
	case VT_R4:
		*(float*)pProp = pArg->fltVal;
		break;
	case VT_R8:
		*(double*)pProp = pArg->dblVal;
		break;
	case VT_DATE:
		*(double*)pProp = pArg->date;
		break;
	case VT_CY:
		*(CY*)pProp = pArg->cyVal;
		break;
	case VT_BSTR:
		AfxBSTR2CString((CString*)pProp, pArg->bstrVal);
		break;
	case VT_ERROR:
		*(SCODE*)pProp = pArg->scode;
		break;
	case VT_BOOL:
		*(BOOL*)pProp = (V_BOOL(pArg) != 0);
		break;
	case VT_VARIANT:
		if (VariantCopy((LPVARIANT)pProp, pArg) != S_OK)
			*puArgErr = 0;
		break;
	case VT_DISPATCH:
	case VT_UNKNOWN:
		if (pArg->punkVal != NULL)
			pArg->punkVal->AddRef();
		_AfxRelease((LPUNKNOWN*)pProp);
		*(LPUNKNOWN*)pProp = pArg->punkVal;
		break;

	default:
		*puArgErr = 0;
		sc = DISP_E_BADVARTYPE;
	}
	VariantClear(&va);

	// if property was a DISP_PROPERTY_NOTIFY type, call pfnSet after setting
	if (!FAILED(sc) && pEntry->pfnSet != NULL)
	{
		AFX_MANAGE_STATE(m_pModuleState);
		(this->*pEntry->pfnSet)();
	}

	return sc;
}

UINT PASCAL CCmdTarget::GetEntryCount(const AFX_DISPMAP* pDispMap)
{
	ASSERT(pDispMap->lpEntryCount != NULL);

	// compute entry count cache if not available
	if (*pDispMap->lpEntryCount == -1)
	{
		// count them
		const AFX_DISPMAP_ENTRY* pEntry = pDispMap->lpEntries;
		while (pEntry->nPropOffset != -1)
			++pEntry;

		// store it
		*pDispMap->lpEntryCount = pEntry - pDispMap->lpEntries;
	}

	ASSERT(*pDispMap->lpEntryCount != -1);
	return *pDispMap->lpEntryCount;
}

MEMBERID PASCAL CCmdTarget::MemberIDFromName(
	const AFX_DISPMAP* pDispMap, LPCTSTR lpszName)
{
	// search all maps and their base maps
	UINT nInherit = 0;
	while (pDispMap != NULL)
	{
		// search all entries in this map
		const AFX_DISPMAP_ENTRY* pEntry = pDispMap->lpEntries;
		UINT nEntryCount = GetEntryCount(pDispMap);
		for (UINT nIndex = 0; nIndex < nEntryCount; nIndex++)
		{
			if (pEntry->vt != VT_MFCVALUE &&
				lstrcmpi(pEntry->lpszName, lpszName) == 0)
			{
				if (pEntry->lDispID == DISPID_UNKNOWN)
				{
					// the MEMBERID is combination of nIndex & nInherit
					ASSERT(MAKELONG(nIndex+1, nInherit) != DISPID_UNKNOWN);
					return MAKELONG(nIndex+1, nInherit);
				}
				// the MEMBERID is specified as the lDispID
				return pEntry->lDispID;
			}
			++pEntry;
		}
#ifdef _AFXDLL
		pDispMap = (*pDispMap->pfnGetBaseMap)();
#else
		pDispMap = pDispMap->pBaseMap;
#endif
		++nInherit;
	}
	return DISPID_UNKNOWN;  // name not found
}

const AFX_DISPMAP_ENTRY* PASCAL CCmdTarget::GetDispEntry(MEMBERID memid)
{
	const AFX_DISPMAP* pDerivMap = GetDispatchMap();
	const AFX_DISPMAP* pDispMap;
	const AFX_DISPMAP_ENTRY* pEntry;

	if (memid == DISPID_VALUE)
	{
		// DISPID_VALUE is a special alias (look for special alias entry)
		pDispMap = pDerivMap;
		while (pDispMap != NULL)
		{
			// search for special entry with vt == VT_MFCVALUE
			pEntry = pDispMap->lpEntries;
			while (pEntry->nPropOffset != -1)
			{
				if (pEntry->vt == VT_MFCVALUE)
				{
					memid = pEntry->lDispID;
					if (memid == DISPID_UNKNOWN)
					{
						// attempt to map alias name to member ID
						memid = MemberIDFromName(pDerivMap, pEntry->lpszName);
						if (memid == DISPID_UNKNOWN)
							return NULL;
					}
					// break out and map the member ID to an entry
					goto LookupDispID;
				}
				++pEntry;
			}
#ifdef _AFXDLL
			pDispMap = (*pDispMap->pfnGetBaseMap)();
#else
			pDispMap = pDispMap->pBaseMap;
#endif
		}
	}

LookupDispID:
	if ((long)memid > 0)
	{
		// find AFX_DISPMAP corresponding to HIWORD(memid)
		UINT nTest = 0;
		pDispMap = pDerivMap;
		while (pDispMap != NULL && nTest < (UINT)HIWORD(memid))
		{
#ifdef _AFXDLL
			pDispMap = (*pDispMap->pfnGetBaseMap)();
#else
			pDispMap = pDispMap->pBaseMap;
#endif
			++nTest;
		}
		if (pDispMap != NULL)
		{
			UINT nEntryCount = GetEntryCount(pDispMap);
			if ((UINT)LOWORD(memid) <= nEntryCount)
			{
				pEntry = pDispMap->lpEntries + LOWORD(memid)-1;

				// must have automatic DISPID or same ID
				// if not then look manually
				if (pEntry->lDispID == DISPID_UNKNOWN ||
					pEntry->lDispID == memid)
				{
					return pEntry;
				}
			}
		}
	}

	// second pass, look for DISP_XXX_ID entries
	pDispMap = pDerivMap;
	while (pDispMap != NULL)
	{
		// find AFX_DISPMAP_ENTRY where (pEntry->lDispID == memid)
		pEntry = pDispMap->lpEntries;
		while (pEntry->nPropOffset != -1)
		{
			if (pEntry->lDispID == memid)
				return pEntry;

			++pEntry;
		}
		// check base class
#ifdef _AFXDLL
		pDispMap = (*pDispMap->pfnGetBaseMap)();
#else
		pDispMap = pDispMap->pBaseMap;
#endif
	}

	return NULL;    // no matching entry
}

/////////////////////////////////////////////////////////////////////////////
// Standard automation methods

void CCmdTarget::GetNotSupported()
{
	AfxThrowOleDispatchException(
		AFX_IDP_GET_NOT_SUPPORTED, AFX_IDP_GET_NOT_SUPPORTED);
}

void CCmdTarget::SetNotSupported()
{
	AfxThrowOleDispatchException(
		AFX_IDP_SET_NOT_SUPPORTED, AFX_IDP_SET_NOT_SUPPORTED);
}

/////////////////////////////////////////////////////////////////////////////
// Wiring to CCmdTarget

// enable this object for OLE automation, called from derived class ctor
void CCmdTarget::EnableAutomation()
{
	ASSERT(GetDispatchMap() != NULL);   // must have DECLARE_DISPATCH_MAP

	// construct an COleDispatchImpl instance just to get to the vtable
	COleDispatchImpl dispatch;

	// vtable pointer should be already set to same or NULL
	ASSERT(m_xDispatch.m_vtbl == NULL||
		*(DWORD*)&dispatch == m_xDispatch.m_vtbl);
	// sizeof(COleDispatchImpl) should be just a DWORD (vtable pointer)
	ASSERT(sizeof(m_xDispatch) == sizeof(COleDispatchImpl));

	// copy the vtable (and other data) to make sure it is initialized
	m_xDispatch.m_vtbl = *(DWORD*)&dispatch;
	*(COleDispatchImpl*)&m_xDispatch = dispatch;
}

// return addref'd IDispatch part of CCmdTarget object
LPDISPATCH CCmdTarget::GetIDispatch(BOOL bAddRef)
{
	ASSERT_VALID(this);
	ASSERT(m_xDispatch.m_vtbl != 0);    // forgot to call EnableAutomation?

	// AddRef the object if requested
	if (bAddRef)
		ExternalAddRef();

	// return pointer to IDispatch implementation
	return (LPDISPATCH)GetInterface(&IID_IDispatch);
}

// retrieve CCmdTarget* from IDispatch* (return NULL if not MFC IDispatch)
CCmdTarget* PASCAL CCmdTarget::FromIDispatch(LPDISPATCH lpDispatch)
{
	// construct an COleDispatchImpl instance just to get to the vtable
	COleDispatchImpl dispatch;

	ASSERT(*(DWORD*)&dispatch != 0);    // null vtable ptr?
	if (*(DWORD*)lpDispatch != *(DWORD*)&dispatch)
		return NULL;    // not our IDispatch*

	// vtable ptrs match, so must have originally been retrieved with
	//  CCmdTarget::GetIDispatch.
#ifndef _AFX_NO_NESTED_DERIVATION
	CCmdTarget* pTarget = (CCmdTarget*)
		((BYTE*)lpDispatch - ((COleDispatchImpl*)lpDispatch)->m_nOffset);
#else
	CCmdTarget* pTarget = (CCmdTarget*)
		((BYTE*)lpDispatch - offsetof(CCmdTarget, m_xDispatch));
#endif
	ASSERT_VALID(pTarget);
	return pTarget;
}

BOOL CCmdTarget::IsResultExpected()
{
	BOOL bResultExpected = m_bResultExpected;
	m_bResultExpected = TRUE;   // can only ask once
	return bResultExpected;
}

void COleDispatchImpl::Disconnect()
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)

	pThis->ExternalDisconnect();    // always disconnect the object
}

///////////////////////////////////////////////////////////////////////////////
// OLE BSTR support

BSTR CString::AllocSysString() const
{
#if defined(_UNICODE) || defined(OLE2ANSI)
	BSTR bstr = ::SysAllocStringLen(m_pchData, GetData()->nDataLength);
	if (bstr == NULL)
		AfxThrowMemoryException();
#else
	int nLen = MultiByteToWideChar(CP_ACP, 0, m_pchData,
		GetData()->nDataLength, NULL, NULL);
	BSTR bstr = ::SysAllocStringLen(NULL, nLen);
	if (bstr == NULL)
		AfxThrowMemoryException();
	MultiByteToWideChar(CP_ACP, 0, m_pchData, GetData()->nDataLength,
		bstr, nLen);
#endif

	return bstr;
}

BSTR CString::SetSysString(BSTR* pbstr) const
{
	ASSERT(AfxIsValidAddress(pbstr, sizeof(BSTR)));

#if defined(_UNICODE) || defined(OLE2ANSI)
	if (!::SysReAllocStringLen(pbstr, m_pchData, GetData()->nDataLength))
		AfxThrowMemoryException();
#else
	int nLen = MultiByteToWideChar(CP_ACP, 0, m_pchData,
		GetData()->nDataLength, NULL, NULL);
	if (!::SysReAllocStringLen(pbstr, NULL, nLen))
		AfxThrowMemoryException();
	MultiByteToWideChar(CP_ACP, 0, m_pchData, GetData()->nDataLength,
		*pbstr, nLen);
#endif

	ASSERT(*pbstr != NULL);
	return *pbstr;
}

/////////////////////////////////////////////////////////////////////////////
// Specifics of METHOD->C++ member function invocation

// Note: Although this code is written in C++, it is very dependent on the
//  specific compiler and target platform.  The current code below assumes
//  that the stack grows down, and that arguments are pushed last to first.

// calculate size of pushed arguments + retval reference

// size of arguments on stack when pushed by value
AFX_STATIC_DATA const UINT _afxByValue[] =
{
	0,                          // VTS_EMPTY
	0,                          // VTS_NULL
	sizeof(_STACK_INT),         // VTS_I2
	sizeof(_STACK_LONG),        // VTS_I4
	sizeof(_STACK_FLOAT),       // VTS_R4
	sizeof(_STACK_DOUBLE),      // VTS_R8
	sizeof(CY),                 // VTS_CY
	sizeof(DATE),               // VTS_DATE
	sizeof(LPCOLESTR),          // VTS_WBSTR (VT_BSTR)
	sizeof(LPDISPATCH),         // VTS_DISPATCH
	sizeof(SCODE),              // VTS_SCODE
	sizeof(BOOL),               // VTS_BOOL
	sizeof(const VARIANT*),     // VTS_VARIANT
	sizeof(LPUNKNOWN),           // VTS_UNKNOWN
#if !defined(_UNICODE) && !defined(OLE2ANSI)
	sizeof(LPCSTR),             // VTS_BSTR (VT_BSTRA -- MFC defined)
#endif
};

// size of arguments on stack when pushed by reference
AFX_STATIC_DATA const UINT _afxByRef[] =
{
	0,                          // VTS_PEMPTY
	0,                          // VTS_PNULL
	sizeof(short*),             // VTS_PI2
	sizeof(long*),              // VTS_PI4
	sizeof(float*),             // VTS_PR4
	sizeof(double*),            // VTS_PR8
	sizeof(CY*),                // VTS_PCY
	sizeof(DATE*),              // VTS_PDATE
	sizeof(BSTR*),              // VTS_PBSTR
	sizeof(LPDISPATCH*),        // VTS_PDISPATCH
	sizeof(SCODE*),             // VTS_PSCODE
	sizeof(VARIANT_BOOL*),      // VTS_PBOOL
	sizeof(VARIANT*),           // VTS_PVARIANT
	sizeof(LPUNKNOWN*),         // VTS_PUNKNOWN
	sizeof(BYTE*),              // VTS_PUI1
};

AFX_STATIC_DATA const UINT _afxRetVal[] =
{
	0,                          // VT_EMPTY
	0,                          // VT_NULL
	0,                          // VT_I2
	0,                          // VT_I4
	0,                          // VT_R4
	0,                          // VT_R8
	sizeof(CY*),                // VT_CY
	0,                          // VT_DATE (same as VT_R8)
	0,                          // VT_BSTR
	0,                          // VT_DISPATCH
	0,                          // VT_ERROR
	0,                          // VT_BOOL
	sizeof(VARIANT*),           // VT_VARIANT
	0,                          // VT_UNKNOWN
	0,                          // VT_UI1
};

UINT PASCAL CCmdTarget::GetStackSize(const BYTE* pbParams, VARTYPE vtResult)
{
	// sizeof 'this' pointer
	UINT nCount = sizeof(CCmdTarget*);
#ifdef _ALIGN_STACK
	nCount = (nCount + (_ALIGN_STACK-1)) & ~(_ALIGN_STACK-1);
#endif

	// count bytes in return value
	ASSERT((UINT)vtResult < _countof(_afxRetVal));
	nCount += _afxRetVal[vtResult];
#ifdef _ALIGN_STACK
	nCount = (nCount + (_ALIGN_STACK-1)) & ~(_ALIGN_STACK-1);
#endif

	// count arguments
	ASSERT(pbParams != NULL);
	while (*pbParams != 0)
	{
		if (*pbParams != VT_MFCMARKER)
		{
			// align if necessary
			// get and add appropriate byte count
			const UINT* rgnBytes;
			if (*pbParams & VT_MFCBYREF)
				rgnBytes = _afxByRef;
			else
				rgnBytes = _afxByValue;
			ASSERT((*pbParams & ~VT_MFCBYREF) < _countof(_afxByValue));
#ifdef _ALIGN_DOUBLES
			// align doubles on 8 byte for some platforms
			if (*pbParams == VT_R8)
				nCount = (nCount + _ALIGN_DOUBLES-1) & ~(_ALIGN_DOUBLES-1);
#endif
			nCount += rgnBytes[*pbParams & ~VT_MFCBYREF];
#ifdef _ALIGN_STACK
			nCount = (nCount + (_ALIGN_STACK-1)) & ~(_ALIGN_STACK-1);
#endif
		}
		++pbParams;
	}
#if defined(_ALIGN_DOUBLES) && defined(_SHADOW_DOUBLES)
	// align doubles on 8 byte for some platforms
	nCount = (nCount + _ALIGN_DOUBLES-1) & ~(_ALIGN_DOUBLES-1);
#endif
	return nCount;
}

// push arguments on stack appropriate for C++ call (compiler dependent)
#ifndef _SHADOW_DOUBLES
SCODE CCmdTarget::PushStackArgs(BYTE* pStack, const BYTE* pbParams,
	void* pResult, VARTYPE vtResult, DISPPARAMS* pDispParams, UINT* puArgErr,
	VARIANT* rgTempVars)
#else
SCODE CCmdTarget::PushStackArgs(BYTE* pStack, const BYTE* pbParams,
	void* pResult, VARTYPE vtResult, DISPPARAMS* pDispParams, UINT* puArgErr,
	VARIANT* rgTempVars, UINT nSizeArgs)
#endif
{
	ASSERT(pStack != NULL);
	ASSERT(pResult != NULL);
	ASSERT(pDispParams != NULL);
	ASSERT(puArgErr != NULL);

#ifdef _SHADOW_DOUBLES
	double* pDoubleShadow = (double*)(pStack + nSizeArgs);
	double* pDoubleShadowMax = pDoubleShadow + _SHADOW_DOUBLES;
#endif

	// C++ member functions use the __thiscall convention, where parameters
	//  are pushed last to first.  Assuming the stack grows down, this means
	//  that the first parameter is at the lowest memory address in the
	//  stack frame and the last parameter is at the highest address.


#ifdef _RETVAL_FIRST
	// push any necessary return value stuff on the stack (pre args)
	//  (an ambient pointer is pushed to stack relative data)
	if (vtResult == VT_CY || vtResult == VT_VARIANT)
	{
#ifdef _ALIGN_STACK
		ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
		*(_STACK_PTR*)pStack = (_STACK_PTR)pResult;
		pStack += sizeof(_STACK_PTR);
#ifdef _ALIGN_STACK
		ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
	}
#endif //_RETVAL_FIRST

	// push the 'this' pointer
#ifdef _ALIGN_STACK
	ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
	*(_STACK_PTR*)pStack = (_STACK_PTR)this;
	pStack += sizeof(_STACK_PTR);
#ifdef _ALIGN_STACK
	ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif

#ifndef _RETVAL_FIRST
	// push any necessary return value stuff on the stack (post args)
	//  (an ambient pointer is pushed to stack relative data)
	if (vtResult == VT_CY || vtResult == VT_VARIANT)
	{
#ifdef _ALIGN_STACK
		ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
		*(_STACK_PTR*)pStack = (_STACK_PTR)pResult;
		pStack += sizeof(_STACK_PTR);
#ifdef _ALIGN_STACK
		ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
	}
#endif //!_RETVAL_FIRST

	// push the arguments (first to last, low address to high address)
	VARIANT* pArgs = pDispParams->rgvarg;
	BOOL bNamedArgs = FALSE;
	int iArg = pDispParams->cArgs; // start with positional arguments
	int iArgMin = pDispParams->cNamedArgs;

	ASSERT(pbParams != NULL);
	for (const BYTE* pb = pbParams; *pb != '\0'; ++pb)
	{
		--iArg; // move to next arg

		// convert MFC parameter type to IDispatch VARTYPE
		VARTYPE vt = *pb;
		if (vt != VT_MFCMARKER && (vt & VT_MFCBYREF))
			vt = (VARTYPE)((vt & ~VT_MFCBYREF) | VT_BYREF);

		VARIANT* pArg;
		if (iArg >= iArgMin)
		{
			// hit named args before using all positional args?
			if (vt == VT_MFCMARKER)
				break;

			// argument specified by caller -- use it
			pArg = &pArgs[iArg];
			if (vt != VT_VARIANT && vt != pArg->vt)
			{
				// argument is not of appropriate type, attempt to coerce it
				VARIANT* pArgTemp = &rgTempVars[iArg];
				ASSERT(pArgTemp->vt == VT_EMPTY);
#if defined(_UNICODE) || defined(OLE2ANSI)
				VARTYPE vtTarget = vt;
#else
				VARTYPE vtTarget = (VARTYPE) ((vt == VT_BSTRA) ? VT_BSTR : vt);
#endif
				if (pArg->vt != vtTarget)
				{
					SCODE sc = VariantChangeType(pArgTemp, pArg, 0, vtTarget);
					if (FAILED(sc))
					{
						TRACE0("Warning: automation argument coercion failed.\n");
						*puArgErr = iArg;
						return sc;
					}
					ASSERT(pArgTemp->vt == vtTarget);
				}

#if !defined(_UNICODE) && !defined(OLE2ANSI)
				if (vt == VT_BSTRA)
				{
					if (pArg->vt != vtTarget)
					{
						// coerce above created a new string
						// convert it to ANSI and free it
						ASSERT(pArgTemp->vt == VT_BSTR);
						BSTR bstrW = pArgTemp->bstrVal;
						pArgTemp->bstrVal = AfxBSTR2ABSTR(bstrW);
						SysFreeString(bstrW);
					}
					else
					{
						// convert the string to ANSI from original
						pArgTemp->bstrVal = AfxBSTR2ABSTR(pArg->bstrVal);
						pArgTemp->vt = VT_BSTR;
					}
					vt = VT_BSTR;
				}
#endif
				pArg = pArgTemp;
			}
		}
		else
		{
			if (vt == VT_MFCMARKER)
			{
				// start processing named arguments
				iArg = pDispParams->cNamedArgs;
				iArgMin = 0;
				bNamedArgs = TRUE;
				continue;
			}

			if (bNamedArgs || vt != VT_VARIANT)
				break;  // function not expecting optional argument

			// argument not specified by caller -- provide default variant
			static VARIANT vaDefault;   // Note: really is 'const'
			vaDefault.vt = VT_ERROR;
			vaDefault.scode = DISP_E_PARAMNOTFOUND;
			pArg = &vaDefault;
		}

		// push parameter value on the stack
		switch (vt)
		{
		// by value parameters
		case VT_UI1:
			*(_STACK_INT*)pStack = pArg->bVal; // 'BYTE' is passed as 'int'
			pStack += sizeof(_STACK_INT);
			break;
		case VT_I2:
			*(_STACK_INT*)pStack = pArg->iVal;
			pStack += sizeof(_STACK_INT);   // 'short' is passed as 'int'
			break;
		case VT_I4:
			*(_STACK_LONG*)pStack = pArg->lVal;
			pStack += sizeof(_STACK_LONG);
			break;
		case VT_R4:
			*(_STACK_FLOAT*)pStack = (_STACK_FLOAT)pArg->fltVal;
			pStack += sizeof(_STACK_FLOAT);
#ifdef _SHADOW_DOUBLES
			if (pDoubleShadow < pDoubleShadowMax)
				*pDoubleShadow++ = (double)pArg->fltVal;
#endif
			break;
		case VT_R8:
#ifdef _ALIGN_DOUBLES
			// align doubles on 8 byte for some platforms
			pStack = (BYTE*)(((DWORD)pStack + _ALIGN_DOUBLES-1) &
				~(_ALIGN_DOUBLES-1));
#endif
			*(_STACK_DOUBLE*)pStack = (_STACK_DOUBLE)pArg->dblVal;
			pStack += sizeof(_STACK_DOUBLE);
#ifdef _SHADOW_DOUBLES
			if (pDoubleShadow < pDoubleShadowMax)
				*pDoubleShadow++ = pArg->dblVal;
#endif
			break;
		case VT_DATE:
#ifdef _ALIGN_DOUBLES
			// align doubles on 8 byte for some platforms
			pStack = (BYTE*)(((DWORD)pStack + _ALIGN_DOUBLES-1) &
				~(_ALIGN_DOUBLES-1));
#endif
			*(_STACK_DOUBLE*)pStack = (_STACK_DOUBLE)pArg->date;
			pStack += sizeof(_STACK_DOUBLE);
#ifdef _SHADOW_DOUBLES
			if (pDoubleShadow < pDoubleShadowMax)
				*pDoubleShadow++ = pArg->date;
#endif
			break;
		case VT_CY:
			*(CY*)pStack = pArg->cyVal;
			pStack += sizeof(CY);
			break;
		case VT_BSTR:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->bstrVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_ERROR:
			*(_STACK_LONG*)pStack = (_STACK_LONG)pArg->scode;
			pStack += sizeof(_STACK_LONG);
			break;
		case VT_BOOL:
			*(_STACK_LONG*)pStack = (_STACK_LONG)(V_BOOL(pArg) != 0);
			pStack += sizeof(_STACK_LONG);
			break;
		case VT_VARIANT:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_DISPATCH:
		case VT_UNKNOWN:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->punkVal;
			pStack += sizeof(_STACK_PTR);
			break;

		// by reference parameters
		case VT_UI2|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pbVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_I2|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->piVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_I4|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->plVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_R4|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pfltVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_R8|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pdblVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_DATE|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pdate;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_CY|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pcyVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_BSTR|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pbstrVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_ERROR|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pscode;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_BOOL|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pboolVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_VARIANT|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->pvarVal;
			pStack += sizeof(_STACK_PTR);
			break;
		case VT_DISPATCH|VT_BYREF:
		case VT_UNKNOWN|VT_BYREF:
			*(_STACK_PTR*)pStack = (_STACK_PTR)pArg->ppunkVal;
			pStack += sizeof(_STACK_PTR);
			break;

		default:
			ASSERT(FALSE);
		}

#ifdef _ALIGN_STACK
		// align stack as appropriate for next parameter
		pStack = (BYTE*)(((DWORD)pStack + (_ALIGN_STACK-1)) &
			~(_ALIGN_STACK-1));
		ASSERT(((DWORD)pStack & (_ALIGN_STACK-1)) == 0);
#endif
	}

	// check that all source arguments were consumed
	if (iArg > 0)
	{
		*puArgErr = iArg;
		return DISP_E_BADPARAMCOUNT;
	}
	// check that all target arguments were filled
	if (*pb != '\0')
	{
		*puArgErr = pDispParams->cArgs;
		return DISP_E_PARAMNOTOPTIONAL;
	}
	return S_OK;    // success!
}

// indirect call helper (see OLECALL.CPP for implementation)

extern "C" DWORD AFXAPI
_AfxDispatchCall(AFX_PMSG pfn, void* pArgs, UINT nSizeArgs);

// invoke standard method given IDispatch parameters/return value, etc.
SCODE CCmdTarget::CallMemberFunc(const AFX_DISPMAP_ENTRY* pEntry, WORD wFlags,
	VARIANT* pvarResult, DISPPARAMS* pDispParams, UINT* puArgErr)
{
	AFX_MANAGE_STATE(m_pModuleState);

	ASSERT(pEntry != NULL);
	ASSERT(pEntry->pfn != NULL);

	// special union used only to hold largest return value possible
	union AFX_RESULT
	{
		VARIANT vaVal;
		CY cyVal;
		float fltVal;
		double dblVal;
		DWORD nVal;
	};

	// get default function and parameters
	BYTE bNoParams = 0;
	const BYTE* pbParams = (const BYTE*)pEntry->lpszParams;
	if (pbParams == NULL)
		pbParams = &bNoParams;
	UINT nParams = lstrlenA((LPCSTR)pbParams);

	// get default function and return value information
	AFX_PMSG pfn = pEntry->pfn;
	VARTYPE vtResult = pEntry->vt;

	// make DISPATCH_PROPERTYPUT look like call with one extra parameter
	if (wFlags & (DISPATCH_PROPERTYPUT|DISPATCH_PROPERTYPUTREF))
	{
		BYTE* pbPropSetParams = (BYTE*)_alloca(nParams+3);
		ASSERT(pbPropSetParams != NULL);    // stack overflow?

		ASSERT(!(pEntry->vt & VT_BYREF));
		memcpy(pbPropSetParams, pbParams, nParams);
		pbParams = pbPropSetParams;

		VARTYPE vtProp = pEntry->vt;
#if !defined(_UNICODE) && !defined(OLE2ANSI)
		if (vtProp == VT_BSTR)
			vtProp = VT_BSTRA;
#endif
		// VT_MFCVALUE serves serves as marker denoting start of named params
		pbPropSetParams[nParams++] = (BYTE)VT_MFCMARKER;
		pbPropSetParams[nParams++] = (BYTE)vtProp;
		pbPropSetParams[nParams] = 0;

		// call "set" function instead of "get"
		ASSERT(pEntry->pfnSet != NULL);
		pfn = pEntry->pfnSet;
		vtResult = VT_EMPTY;
	}

	// allocate temporary space for VARIANT temps created by VariantChangeType
	VARIANT* rgTempVars =
		(VARIANT*)_alloca(pDispParams->cArgs * sizeof(VARIANT));
	if (rgTempVars == NULL)
	{
		TRACE0("Error: stack overflow in IDispatch::Invoke!\n");
		return E_OUTOFMEMORY;
	}
	memset(rgTempVars, 0, pDispParams->cArgs * sizeof(VARIANT));

	// determine size of arguments and allocate stack space
	UINT nSizeArgs = GetStackSize(pbParams, vtResult);
	ASSERT(nSizeArgs != 0);
	if (nSizeArgs < _STACK_MIN)
		nSizeArgs = _STACK_MIN;
	BYTE* pStack = (BYTE*)_alloca(nSizeArgs + _SCRATCH_SIZE);
	if (pStack == NULL)
	{
		TRACE0("Error: stack overflow in IDispatch::Invoke!\n");
		return E_OUTOFMEMORY;
	}

	// push all the args on to the stack allocated memory
	AFX_RESULT result;
#ifndef _SHADOW_DOUBLES
	SCODE sc = PushStackArgs(pStack, pbParams, &result, vtResult,
		pDispParams, puArgErr, rgTempVars);
#else
	SCODE sc = PushStackArgs(pStack, pbParams, &result, vtResult,
		pDispParams, puArgErr, rgTempVars, nSizeArgs);
#endif
	pStack += _STACK_OFFSET;

	DWORD dwResult = 0;
	if (sc == S_OK)
	{
		TRY
		{
			// PushStackArgs will fail on argument mismatches
			DWORD (AFXAPI *pfnDispatch)(AFX_PMSG, void*, UINT) =
				&_AfxDispatchCall;

			// floating point return values are a special case
			switch (vtResult)
			{
			case VT_R4:
				result.fltVal = ((float (AFXAPI*)(AFX_PMSG, void*, UINT))
					pfnDispatch)(pfn, pStack, nSizeArgs);
				break;
			case VT_R8:
				result.dblVal = ((double (AFXAPI*)(AFX_PMSG, void*, UINT))
					pfnDispatch)(pfn, pStack, nSizeArgs);
				break;
			case VT_DATE:
				result.dblVal = ((DATE (AFXAPI*)(AFX_PMSG, void*, UINT))
					pfnDispatch)(pfn, pStack, nSizeArgs);
				break;

			default:
				dwResult = pfnDispatch(pfn, pStack, nSizeArgs);
				break;
			}
		}
		CATCH_ALL(e)
		{
			// free temporaries created by VariantChangeType
			for (UINT iArg = 0; iArg < pDispParams->cArgs; ++iArg)
				VariantClear(&rgTempVars[iArg]);

			THROW_LAST();
		}
		END_CATCH_ALL
	}

	// free temporaries created by VariantChangeType
	for (UINT iArg = 0; iArg < pDispParams->cArgs; ++iArg)
		VariantClear(&rgTempVars[iArg]);

	// handle error during PushStackParams
	if (sc != S_OK)
		return sc;

	// property puts don't touch the return value
	if (pvarResult != NULL)
	{
		// clear pvarResult just in case
		pvarResult->vt = vtResult;

		// build return value VARIANT from result union
		switch (vtResult)
		{
		case VT_UI2:
			pvarResult->bVal = (BYTE)dwResult;
			break;
		case VT_I2:
			pvarResult->iVal = (short)dwResult;
			break;
		case VT_I4:
			pvarResult->lVal = (long)dwResult;
			break;
		case VT_R4:
			pvarResult->fltVal = result.fltVal;
			break;
		case VT_R8:
			pvarResult->dblVal = result.dblVal;
			break;
		case VT_CY:
			pvarResult->cyVal = result.cyVal;
			break;
		case VT_DATE:
			pvarResult->date = result.dblVal;
			break;
		case VT_BSTR:
			pvarResult->bstrVal = (BSTR)dwResult;
			break;
		case VT_ERROR:
			pvarResult->scode = (SCODE)dwResult;
			break;
		case VT_BOOL:
			V_BOOL(pvarResult) = (VARIANT_BOOL)((BOOL)dwResult != 0 ? -1 : 0);
			break;
		case VT_VARIANT:
			*pvarResult = result.vaVal;
			break;
		case VT_DISPATCH:
		case VT_UNKNOWN:
			pvarResult->punkVal = (LPUNKNOWN)dwResult; // already AddRef
			break;
		}
	}
	else
	{
		// free unused return value
		switch (vtResult)
		{
		case VT_BSTR:
			if ((BSTR)dwResult != NULL)
				SysFreeString((BSTR)dwResult);
			break;
		case VT_DISPATCH:
		case VT_UNKNOWN:
			if ((LPUNKNOWN)dwResult != 0)
				((LPUNKNOWN)dwResult)->Release();
			break;
		case VT_VARIANT:
			VariantClear(&result.vaVal);
			break;
		}
	}

	return S_OK;    // success!
}

/////////////////////////////////////////////////////////////////////////////
// CCmdTarget::XDispatch implementation

STDMETHODIMP_(ULONG) COleDispatchImpl::AddRef()
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleDispatchImpl::Release()
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleDispatchImpl::QueryInterface(REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleDispatchImpl::GetTypeInfoCount(UINT* pctinfo)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	*pctinfo = pThis->GetTypeInfoCount();
	return S_OK;
}

STDMETHODIMP COleDispatchImpl::GetTypeInfo(UINT itinfo, LCID lcid,
	ITypeInfo** pptinfo)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	ASSERT_POINTER(pptinfo, LPTYPEINFO);

	if (itinfo != 0)
		return E_INVALIDARG;

	IID iid;
	if (!pThis->GetDispatchIID(&iid))
		return E_NOTIMPL;

	return pThis->GetTypeInfoOfGuid(lcid, iid, pptinfo);
}

STDMETHODIMP COleDispatchImpl::GetIDsOfNames(
	REFIID riid, LPOLESTR* rgszNames, UINT cNames, LCID lcid, DISPID* rgdispid)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	ASSERT_POINTER(rgszNames, char*);
	ASSERT_POINTER(rgdispid, DISPID);

	USES_CONVERSION;

	// check arguments
	if (riid != IID_NULL)
		return DISP_E_UNKNOWNINTERFACE;

	SCODE sc;
	LPTYPEINFO lpTypeInfo = NULL;
	if (lcid != 0 && SUCCEEDED(sc = GetTypeInfo(0, lcid, &lpTypeInfo)))
	{
		// For non-zero lcid, let typeinfo do the work (when available)
		ASSERT(lpTypeInfo != NULL);
		sc = lpTypeInfo->GetIDsOfNames(rgszNames, cNames, rgdispid);
		lpTypeInfo->Release();
		if (sc == TYPE_E_ELEMENTNOTFOUND)
			sc = DISP_E_UNKNOWNNAME;
	}
	else
	{
		// fill in the member name
		const AFX_DISPMAP* pDerivMap = pThis->GetDispatchMap();
		rgdispid[0] = pThis->MemberIDFromName(pDerivMap, OLE2CT(rgszNames[0]));
		if (rgdispid[0] == DISPID_UNKNOWN)
			sc = DISP_E_UNKNOWNNAME;
		else
			sc = S_OK;

		// argument names are always DISPID_UNKNOWN (for this implementation)
		for (UINT nIndex = 1; nIndex < cNames; nIndex++)
			rgdispid[nIndex] = DISPID_UNKNOWN;
	}

	return sc;
}

STDMETHODIMP COleDispatchImpl::Invoke(
	DISPID dispid, REFIID riid, LCID lcid,
	WORD wFlags, DISPPARAMS* pDispParams, LPVARIANT pvarResult,
	LPEXCEPINFO pexcepinfo, UINT* puArgErr)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, Dispatch)
	ASSERT_NULL_OR_POINTER(pvarResult, VARIANT);
	ASSERT_NULL_OR_POINTER(pexcepinfo, EXCEPINFO);
	ASSERT_NULL_OR_POINTER(puArgErr, UINT);

	// make sure pvarResult is initialized
	if (pvarResult != NULL)
		AfxVariantInit(pvarResult);

	// check arguments
	if (riid != IID_NULL)
		return DISP_E_UNKNOWNINTERFACE;

	// allow subclass to disable Invoke
	if (!pThis->IsInvokeAllowed(dispid))
		return E_UNEXPECTED;

	// copy param block for safety
	DISPPARAMS params = *pDispParams;
	pDispParams = &params;

	// most of the time, named arguments are not supported
	if (pDispParams->cNamedArgs != 0)
	{
		// only special PROPERTYPUT named argument is allowed
		if (pDispParams->cNamedArgs != 1 ||
			pDispParams->rgdispidNamedArgs[0] != DISPID_PROPERTYPUT)
		{
			return DISP_E_NONAMEDARGS;
		}
	}

	// get entry for the member ID
	const AFX_DISPMAP_ENTRY* pEntry = pThis->GetDispEntry(dispid);
	if (pEntry == NULL)
		return DISP_E_MEMBERNOTFOUND;

	// treat member calls on properties just like property get/set
	if ((wFlags == DISPATCH_METHOD) &&
		((pEntry->pfn == NULL && pEntry->pfnSet == NULL) ||
		 (pEntry->pfn == NULL && pEntry->pfnSet != NULL) ||
		 (pEntry->pfn != NULL && pEntry->pfnSet != NULL)))
	{
		// the entry describes a property but a method call is being
		//  attempted -- change it to a property get/set based on the
		//  number of parameters being passed.
		wFlags &= ~DISPATCH_METHOD;
		UINT nExpectedArgs = pEntry->lpszParams != NULL ?
			(UINT)lstrlenA(pEntry->lpszParams) : 0;
		if (pDispParams->cArgs <= nExpectedArgs)
		{
			// no extra param -- so treat as property get
			wFlags |= DISPATCH_PROPERTYGET;
		}
		else
		{
			// extra params -- treat as property set
			wFlags |= DISPATCH_PROPERTYPUTREF;
			pDispParams->cNamedArgs = 1;
		}
	}

	// property puts should not require a return value
	if (wFlags & (DISPATCH_PROPERTYPUTREF|DISPATCH_PROPERTYPUT))
	{
		pvarResult = NULL;
		// catch attempt to do property set on method
		if (pEntry->pfn != NULL && pEntry->pfnSet == NULL)
			return DISP_E_TYPEMISMATCH;
	}

	UINT uArgErr = (UINT)-1;    // no error yet
	SCODE sc = S_OK;

	// handle special cases of DISPATCH_PROPERTYPUT
	VARIANT* pvarParamSave = NULL;
	VARIANT vaParamSave;
	vaParamSave.vt = VT_ERROR;

	DISPPARAMS paramsTemp;
	VARIANT vaTemp;
	AfxVariantInit(&vaTemp);

	if (wFlags == DISPATCH_PROPERTYPUT && dispid != DISPID_VALUE)
	{
		// with PROPERTYPUT (no REF), the right hand side may need fixup
		if (pDispParams->rgvarg[0].vt == VT_DISPATCH &&
			pDispParams->rgvarg[0].pdispVal != NULL)
		{
			// remember old value for restore later
			pvarParamSave = &pDispParams->rgvarg[0];
			vaParamSave = pDispParams->rgvarg[0];
			AfxVariantInit(&pDispParams->rgvarg[0]);

			// get default value of right hand side
			memset(&paramsTemp, 0, sizeof(DISPPARAMS));
			sc = vaParamSave.pdispVal->Invoke(
				DISPID_VALUE, riid, lcid, DISPATCH_PROPERTYGET, &paramsTemp,
				&pDispParams->rgvarg[0], pexcepinfo, puArgErr);
		}

		// special handling for PROPERTYPUT (no REF), left hand side
		if (sc == S_OK && pEntry->vt == VT_DISPATCH)
		{
			memset(&paramsTemp, 0, sizeof(DISPPARAMS));

			// parameters are distributed depending on what the Get expects
			if (pEntry->lpszParams == NULL)
			{
				// paramsTemp is already setup for no parameters
				sc = Invoke(dispid, riid, lcid,
					DISPATCH_PROPERTYGET|DISPATCH_METHOD, &paramsTemp,
					&vaTemp, pexcepinfo, puArgErr);
				if (sc == S_OK &&
					(vaTemp.vt != VT_DISPATCH || vaTemp.pdispVal == NULL))
					sc = DISP_E_TYPEMISMATCH;
				else if (sc == S_OK)
				{
					ASSERT(vaTemp.vt == VT_DISPATCH && vaTemp.pdispVal != NULL);
					// we have the result, now call put on the default property
					sc = vaTemp.pdispVal->Invoke(
						DISPID_VALUE, riid, lcid, wFlags, pDispParams,
						pvarResult, pexcepinfo, puArgErr);
				}
			}
			else
			{
				// pass all but named params
				paramsTemp.rgvarg = &pDispParams->rgvarg[1];
				paramsTemp.cArgs = pDispParams->cArgs - 1;
				sc = Invoke(dispid, riid, lcid,
					DISPATCH_PROPERTYGET|DISPATCH_METHOD, &paramsTemp,
					&vaTemp, pexcepinfo, puArgErr);
				if (sc == S_OK &&
					(vaTemp.vt != VT_DISPATCH || vaTemp.pdispVal == NULL))
					sc = DISP_E_TYPEMISMATCH;
				else if (sc == S_OK)
				{
					ASSERT(vaTemp.vt == VT_DISPATCH && vaTemp.pdispVal != NULL);

					// we have the result, now call put on the default property
					paramsTemp = *pDispParams;
					paramsTemp.cArgs = paramsTemp.cNamedArgs;
					sc = vaTemp.pdispVal->Invoke(
						DISPID_VALUE, riid, lcid, wFlags, &paramsTemp,
						pvarResult, pexcepinfo, puArgErr);
				}
			}
			VariantClear(&vaTemp);

			if (sc != DISP_E_MEMBERNOTFOUND)
				goto Cleanup;
		}

		if (sc != S_OK && sc != DISP_E_MEMBERNOTFOUND)
			goto Cleanup;
	}

	// ignore DISP_E_MEMBERNOTFOUND from above
	ASSERT(sc == DISP_E_MEMBERNOTFOUND || sc == S_OK);

	// undo implied default value on right hand side on error
	if (sc != S_OK && pvarParamSave != NULL)
	{
		// default value stuff failed -- so try without default value
		pvarParamSave = NULL;
		VariantClear(&pDispParams->rgvarg[0]);
		pDispParams->rgvarg[0] = vaParamSave;
	}
	sc = S_OK;

	// check arguments against this entry
	UINT nOrigArgs; nOrigArgs = pDispParams->cArgs;
	if (wFlags & (DISPATCH_PROPERTYGET|DISPATCH_METHOD))
	{
		if (!(wFlags & DISPATCH_METHOD))
		{
			if (pEntry->vt == VT_EMPTY)
				return DISP_E_BADPARAMCOUNT;
			if (pvarResult == NULL)
				return DISP_E_PARAMNOTOPTIONAL;
		}
		if (pEntry->lpszParams == NULL && pDispParams->cArgs > 0)
		{
			if (pEntry->vt != VT_DISPATCH)
				return DISP_E_BADPARAMCOUNT;

			// it is VT_DISPATCH property/method but too many arguments supplied
			// transfer those arguments to the default property of the return value
			// after getting the return value from this call.  This is referred
			// to as collection lookup.
			pDispParams->cArgs = 0;
			if (pvarResult == NULL)
				pvarResult = &vaTemp;
		}
	}

	// make sure that parameters are not passed to a simple property
	if (pDispParams->cArgs > 1 &&
		(wFlags & (DISPATCH_PROPERTYPUT|DISPATCH_PROPERTYPUTREF)) &&
		pEntry->pfn == NULL)
	{
		sc = DISP_E_BADPARAMCOUNT;
		goto Cleanup;
	}

	// make sure that pvarResult is set for simple property get
	if (pEntry->pfn == NULL && pDispParams->cArgs == 0 && pvarResult == NULL)
	{
		sc = DISP_E_PARAMNOTOPTIONAL;
		goto Cleanup;
	}

	// make sure IsExpectingResult returns FALSE as appropriate
	BOOL bResultExpected;
	bResultExpected = pThis->m_bResultExpected;
	pThis->m_bResultExpected = pvarResult != NULL;

	TRY
	{
		if (pEntry->pfn == NULL)
		{
			// do standard property get/set
			if (pDispParams->cArgs == 0)
				pThis->GetStandardProp(pEntry, pvarResult, &uArgErr);
			else
				sc = pThis->SetStandardProp(pEntry, pDispParams, &uArgErr);
		}
		else
		{
			// do standard method call
			sc = pThis->CallMemberFunc(pEntry, wFlags,
				pvarResult, pDispParams, &uArgErr);
		}
	}
	CATCH(COleException, e)
	{
		sc = e->m_sc;
		DELETE_EXCEPTION(e);
	}
	AND_CATCH_ALL(e)
	{
		AFX_MANAGE_STATE(pThis->m_pModuleState);
		if (pexcepinfo != NULL)
		{
			// fill exception with translation of MFC exception
			COleDispatchException::Process(pexcepinfo, e);
		}
		DELETE_EXCEPTION(e);
		sc = DISP_E_EXCEPTION;
	}
	END_CATCH_ALL

	// restore original m_bResultExpected flag
	pThis->m_bResultExpected = bResultExpected;

	// handle special DISPATCH_PROPERTYGET collection lookup case
	if (sc == S_OK && nOrigArgs > pDispParams->cArgs)
	{
		ASSERT(wFlags & (DISPATCH_PROPERTYGET|DISPATCH_METHOD));
		ASSERT(pvarResult != NULL);
		// must be non-NULL dispatch, otherwise type mismatch
		if (pvarResult->vt != VT_DISPATCH || pvarResult->pdispVal == NULL)
		{
			sc = DISP_E_TYPEMISMATCH;
			goto Cleanup;
		}
		// otherwise, valid VT_DISPATCH was returned
		pDispParams->cArgs = nOrigArgs;
		LPDISPATCH lpTemp = pvarResult->pdispVal;
		if (pvarResult != &vaTemp)
			AfxVariantInit(pvarResult);
		else
			pvarResult = NULL;
		sc = lpTemp->Invoke(DISPID_VALUE, riid, lcid, wFlags,
			pDispParams, pvarResult, pexcepinfo, puArgErr);
		lpTemp->Release();
	}

Cleanup:
	// restore any arguments which were modified
	if (pvarParamSave != NULL)
	{
		VariantClear(&pDispParams->rgvarg[0]);
		pDispParams->rgvarg[0] = vaParamSave;
	}

	// fill error argument if one is available
	if (sc != S_OK && puArgErr != NULL && uArgErr != -1)
		*puArgErr = uArgErr;

	return sc;
}

/////////////////////////////////////////////////////////////////////////////
// IDispatch specific exception

COleDispatchException::~COleDispatchException()
{
	// destructor code is compiler generated
}

void PASCAL COleDispatchException::Process(
	EXCEPINFO* pInfo, const CException* pAnyException)
{
	USES_CONVERSION;

	ASSERT(AfxIsValidAddress(pInfo, sizeof(EXCEPINFO)));
	ASSERT_VALID(pAnyException);

	// zero default & reserved members
	memset(pInfo, 0, sizeof(EXCEPINFO));

	// get description based on type of exception
	TCHAR szDescription[256];
	LPCTSTR pszDescription = szDescription;
	if (pAnyException->IsKindOf(RUNTIME_CLASS(COleDispatchException)))
	{
		// specific IDispatch style exception
		COleDispatchException* e = (COleDispatchException*)pAnyException;
		pszDescription = e->m_strDescription;
		pInfo->wCode = e->m_wCode;
		pInfo->dwHelpContext = e->m_dwHelpContext;
		pInfo->scode = e->m_scError;

		// propagate source and help file if present
		if (!e->m_strHelpFile.IsEmpty())
			pInfo->bstrHelpFile = ::SysAllocString(T2COLE(e->m_strHelpFile));
		if (!e->m_strSource.IsEmpty())
			pInfo->bstrSource = ::SysAllocString(T2COLE(e->m_strSource));
	}
	else if (pAnyException->IsKindOf(RUNTIME_CLASS(CMemoryException)))
	{
		// failed memory allocation
		AfxLoadString(AFX_IDP_FAILED_MEMORY_ALLOC, szDescription);
		pInfo->wCode = AFX_IDP_FAILED_MEMORY_ALLOC;
	}
	else
	{
		// other unknown/uncommon error
		AfxLoadString(AFX_IDP_INTERNAL_FAILURE, szDescription);
		pInfo->wCode = AFX_IDP_INTERNAL_FAILURE;
	}

	// build up rest of EXCEPINFO struct
	pInfo->bstrDescription = ::SysAllocString(T2COLE(pszDescription));
	if (pInfo->bstrSource == NULL)
		pInfo->bstrSource = ::SysAllocString(T2COLE(AfxGetAppName()));
	if (pInfo->bstrHelpFile == NULL && pInfo->dwHelpContext != 0)
		pInfo->bstrHelpFile = ::SysAllocString(T2COLE(AfxGetApp()->m_pszHelpFilePath));
}

COleDispatchException::COleDispatchException(
	LPCTSTR lpszDescription, UINT nHelpID, WORD wCode)
{
	m_dwHelpContext = nHelpID != 0 ? HID_BASE_DISPATCH+nHelpID : 0;
	m_wCode = wCode;
	if (lpszDescription != NULL)
		m_strDescription = lpszDescription;
	m_scError = wCode != 0 ? NOERROR : E_UNEXPECTED;
}

COleDispatchException::GetErrorMessage(LPTSTR lpszError, UINT nMaxError,
		PUINT pnHelpContext)
{
	ASSERT(lpszError != NULL && AfxIsValidString(lpszError, nMaxError));

	if (pnHelpContext != NULL)
		*pnHelpContext = 0;

	lstrcpyn(lpszError, m_strDescription, nMaxError);
	return TRUE;
}

void AFXAPI AfxThrowOleDispatchException(WORD wCode, LPCTSTR lpszDescription,
	UINT nHelpID)
{
	ASSERT(AfxIsValidString(lpszDescription));
	THROW(new COleDispatchException(lpszDescription, nHelpID, wCode));
}

void AFXAPI AfxThrowOleDispatchException(WORD wCode, UINT nDescriptionID,
	UINT nHelpID)
{
	TCHAR szBuffer[256];
	VERIFY(AfxLoadString(nDescriptionID, szBuffer) != 0);
	if (nHelpID == -1)
		nHelpID = nDescriptionID;
	THROW(new COleDispatchException(szBuffer, nHelpID, wCode));
}

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(COleDispatchException, CException)

/////////////////////////////////////////////////////////////////////////////
