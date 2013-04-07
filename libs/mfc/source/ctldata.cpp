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

#ifdef AFXCTL_CORE3_SEG
#pragma code_seg(AFXCTL_CORE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// OCX Support

////////////////////////////////////////////////////////////////////////////
// _AfxRegisterClipFormat - Registers a custom clipboard format, using the
//  string form of a class ID.

CLIPFORMAT AFXAPI _AfxRegisterClipFormat(REFCLSID clsid)
{
	TCHAR pszClsid[40];
	wsprintf(pszClsid,
		_T("{%08lX-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}"),
		clsid.Data1, clsid.Data2, clsid.Data3,
		clsid.Data4[0], clsid.Data4[1], clsid.Data4[2], clsid.Data4[3],
		clsid.Data4[4], clsid.Data4[5], clsid.Data4[6], clsid.Data4[7]);
	return (CLIPFORMAT)RegisterClipboardFormat(pszClsid);
}

CLIPFORMAT AFXAPI _AfxGetClipboardFormatConvertVBX()
{
	static CLIPFORMAT _cfConvertVBX;
	if (_cfConvertVBX == 0)
		_cfConvertVBX = _AfxRegisterClipFormat(CLSID_ConvertVBX);
	ASSERT(_cfConvertVBX != NULL);
	return _cfConvertVBX;
}

CLIPFORMAT AFXAPI _AfxGetClipboardFormatPersistPropset()
{
	static CLIPFORMAT _cfPersistPropset;
	if (_cfPersistPropset == 0)
		_cfPersistPropset = _AfxRegisterClipFormat(CLSID_PersistPropset);
	ASSERT(_cfPersistPropset != NULL);
	return _cfPersistPropset;
}

/////////////////////////////////////////////////////////////////////////////
// AfxOleMatchPropsetClipFormat

BOOL AFXAPI _AfxOleMatchPropsetClipFormat(CLIPFORMAT cfFormat, LPCLSID lpFmtID)
{
	if (cfFormat == _AfxGetClipboardFormatPersistPropset())
	{
		*lpFmtID = CLSID_PersistPropset;
		return TRUE;
	}

	if (cfFormat == _AfxGetClipboardFormatConvertVBX())
	{
		*lpFmtID = CLSID_ConvertVBX;
		return TRUE;
	}

	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::XDataObject

STDMETHODIMP_(ULONG) COleControl::XDataObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleControl::XDataObject::Release()
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleControl::XDataObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleControl::XDataObject::GetData(LPFORMATETC pformatetcIn,
	LPSTGMEDIUM pmedium )
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.GetData(pformatetcIn, pmedium);
}

STDMETHODIMP COleControl::XDataObject::GetDataHere(LPFORMATETC pformatetc,
	LPSTGMEDIUM pmedium )
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.GetDataHere(pformatetc, pmedium);
}

STDMETHODIMP COleControl::XDataObject::QueryGetData(LPFORMATETC pformatetc )
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.QueryGetData(pformatetc);
}

STDMETHODIMP COleControl::XDataObject::GetCanonicalFormatEtc(
	LPFORMATETC pformatetc, LPFORMATETC pformatetcOut)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.GetCanonicalFormatEtc(pformatetc,
		pformatetcOut);
}

STDMETHODIMP COleControl::XDataObject::SetData(LPFORMATETC pformatetc,
	STGMEDIUM * pmedium, BOOL fRelease)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.SetData(pformatetc, pmedium, fRelease);
}

STDMETHODIMP COleControl::XDataObject::EnumFormatEtc(DWORD dwDirection,
	LPENUMFORMATETC* ppenumFormatEtc)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	CControlDataSource* pDataSource = pThis->GetDataSource();
	if (pDataSource == NULL)
		return E_OUTOFMEMORY;

	return pDataSource->m_xDataObject.EnumFormatEtc(dwDirection,
		ppenumFormatEtc);
}

STDMETHODIMP COleControl::XDataObject::DAdvise(FORMATETC* pFormatetc,
	DWORD advf, LPADVISESINK pAdvSink, DWORD* pdwConnection)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	*pdwConnection = 0;

	if (pThis->m_pDataAdviseHolder == NULL &&
		CreateDataAdviseHolder(&pThis->m_pDataAdviseHolder) != S_OK)
	{
		return E_OUTOFMEMORY;
	}
	ASSERT(pThis->m_pDataAdviseHolder != NULL);
	return pThis->m_pDataAdviseHolder->Advise(this, pFormatetc, advf, pAdvSink,
		pdwConnection);
}

STDMETHODIMP COleControl::XDataObject::DUnadvise(DWORD dwConnection)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	if (pThis->m_pDataAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_pDataAdviseHolder != NULL);
	return pThis->m_pDataAdviseHolder->Unadvise(dwConnection);
}

STDMETHODIMP COleControl::XDataObject::EnumDAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	METHOD_PROLOGUE_EX_(COleControl, DataObject)

	*ppenumAdvise = NULL;

	if (pThis->m_pDataAdviseHolder == NULL)
		return E_FAIL;

	ASSERT(pThis->m_pDataAdviseHolder != NULL);
	return pThis->m_pDataAdviseHolder->EnumAdvise(ppenumAdvise);
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::CControlDataSource

COleControl::CControlDataSource::CControlDataSource(COleControl* pCtrl) :
	m_pCtrl(pCtrl)
{
	m_nGrowBy = 4;  // By default, control puts 4 entries in cache.
}

BOOL COleControl::CControlDataSource::OnRenderGlobalData(
	LPFORMATETC lpFormatEtc, HGLOBAL* phGlobal)
{
	ASSERT_VALID(this);
	return m_pCtrl->OnRenderGlobalData(lpFormatEtc, phGlobal);
	// Note: COleDataSource has no implementation
}

BOOL COleControl::CControlDataSource::OnRenderFileData(
	LPFORMATETC lpFormatEtc, CFile* pFile)
{
	ASSERT_VALID(this);
	return m_pCtrl->OnRenderFileData(lpFormatEtc, pFile);
	// Note: COleDataSource has no implementation
}

BOOL COleControl::CControlDataSource::OnRenderData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	if (m_pCtrl->OnRenderData(lpFormatEtc, lpStgMedium))
		return TRUE;

	return COleDataSource::OnRenderData(lpFormatEtc, lpStgMedium);
}

BOOL COleControl::CControlDataSource::OnSetData(LPFORMATETC lpFormatEtc,
	LPSTGMEDIUM lpStgMedium, BOOL bRelease)
{
	ASSERT_VALID(this);
	return m_pCtrl->OnSetData(lpFormatEtc, lpStgMedium, bRelease);
	// Note: COleDataSource has no implementation
}

/////////////////////////////////////////////////////////////////////////////
// COleControl overridables for IDataObject implementation

BOOL COleControl::OnRenderGlobalData(LPFORMATETC lpFormatEtc,
	HGLOBAL* phGlobal)
{
#ifndef _DEBUG
	UNUSED(lpFormatEtc); // unused in release builds
	UNUSED(phGlobal);    // unused in release builds
#endif

	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(phGlobal, sizeof(HGLOBAL)));

	return FALSE;   // default does nothing
}

BOOL COleControl::OnRenderFileData(LPFORMATETC lpFormatEtc, CFile* pFile)
{
#ifndef _DEBUG
	UNUSED(lpFormatEtc); // unused in release builds
	UNUSED(pFile);       // unused in release builds
#endif

	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC)));
	ASSERT_VALID(pFile);

	return FALSE;   // default does nothing
}

BOOL COleControl::OnRenderData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	// default implementation does not support extended layout
	if (lpFormatEtc->lindex != -1)
		return FALSE;

	// default implementation supports CF_METAFILEPICT
	if (lpFormatEtc->cfFormat == CF_METAFILEPICT)
		return GetMetafileData(lpFormatEtc, lpStgMedium);

	// default implementation supports propset format
	CLSID fmtid;
	if (_AfxOleMatchPropsetClipFormat(lpFormatEtc->cfFormat, &fmtid))
	{
		return GetPropsetData(lpFormatEtc, lpStgMedium, fmtid);
	}

	return FALSE;   // cfFormat not supported
}

BOOL COleControl::OnSetData(LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium,
	BOOL bRelease)
{
	ASSERT_VALID(this);
	ASSERT(AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM)));

	// default implementation supports propset format
	BOOL bSuccess = FALSE;
	CLSID fmtid;
	if (_AfxOleMatchPropsetClipFormat(lpFormatEtc->cfFormat, &fmtid))
	{
		bSuccess = SetPropsetData(lpFormatEtc, lpStgMedium, fmtid);

		if (bSuccess && bRelease)
			ReleaseStgMedium(lpStgMedium);
	}

	return bSuccess;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::GetDataSource

COleControl::CControlDataSource* COleControl::GetDataSource()
{
	TRY
	{
		if (m_pDataSource == NULL)
		{
			AFX_MANAGE_STATE(m_pModuleState);
			m_pDataSource = new CControlDataSource(this);
			ASSERT(m_pDataSource != NULL);
			SetInitialDataFormats();
		}
	}
	END_TRY

	return m_pDataSource;
}

/////////////////////////////////////////////////////////////////////////////
// COleControl::SetInitialDataFormats

void COleControl::SetInitialDataFormats()
{
	// by default a COleControl supports CF_METAFILEPICT
	FORMATETC formatEtc;
	formatEtc.cfFormat = CF_METAFILEPICT;
	formatEtc.ptd = NULL;
	formatEtc.dwAspect = DVASPECT_CONTENT;
	formatEtc.lindex = -1;
	formatEtc.tymed = TYMED_MFPICT;
	m_pDataSource->DelayRenderData(0, &formatEtc);

	// by default a COleControl supports persistent propset format
	// (GetData or SetData)
	formatEtc.cfFormat = _AfxGetClipboardFormatPersistPropset();
	formatEtc.ptd = NULL;
	formatEtc.dwAspect = DVASPECT_CONTENT;
	formatEtc.lindex = -1;
	formatEtc.tymed = TYMED_ISTREAM | TYMED_ISTORAGE;
	m_pDataSource->DelayRenderData(0, &formatEtc);
	m_pDataSource->DelaySetData(0, &formatEtc);

	// by default a COleControl supports VBX conversion propset format
	// (SetData only)
	formatEtc.cfFormat = _AfxGetClipboardFormatConvertVBX();
	m_pDataSource->DelaySetData(0, &formatEtc);
}


/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
