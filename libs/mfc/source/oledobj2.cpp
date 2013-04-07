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

#ifdef AFX_OLE3_SEG
#pragma code_seg(AFX_OLE3_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

/////////////////////////////////////////////////////////////////////////////
// COleDataSource implementation

struct AFX_DATACACHE_ENTRY
{
	FORMATETC m_formatEtc;
	STGMEDIUM m_stgMedium;
	DATADIR m_nDataDir;
};

/////////////////////////////////////////////////////////////////////////////
// COleDataSource construction & destruction

COleDataSource::COleDataSource()
{
	m_pDataCache = NULL;
	m_nMaxSize = 0;
	m_nSize = 0;
	m_nGrowBy = 10;
}

COleDataSource::~COleDataSource()
{
	// clear clipboard source if this object was on the clipboard
	_AFX_OLE_STATE* pOleState = _afxOleState;
	if (this == pOleState->m_pClipboardSource)
		pOleState->m_pClipboardSource = NULL;

	// free the clipboard data cache
	Empty();
}

void COleDataSource::Empty()
{
	if (m_pDataCache != NULL)
	{
		ASSERT(m_nMaxSize != 0);
		ASSERT(m_nSize != 0);

		// release all of the STGMEDIUMs and FORMATETCs
		for (UINT nIndex = 0; nIndex < m_nSize; nIndex++)
		{
			CoTaskMemFree(m_pDataCache[nIndex].m_formatEtc.ptd);
			::ReleaseStgMedium(&m_pDataCache[nIndex].m_stgMedium);
		}

		// delete the cache
		delete[] m_pDataCache;
		m_pDataCache = NULL;
		m_nMaxSize = 0;
		m_nSize = 0;
	}
	ASSERT(m_pDataCache == NULL);
	ASSERT(m_nMaxSize == 0);
	ASSERT(m_nSize == 0);
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource clipboard API wrappers

void COleDataSource::SetClipboard()
{
	ASSERT_VALID(this);

	// attempt OLE set clipboard operation
	LPDATAOBJECT lpDataObject = (LPDATAOBJECT)GetInterface(&IID_IDataObject);
	SCODE sc = ::OleSetClipboard(lpDataObject);
	if (sc != S_OK)
		AfxThrowOleException(sc);

	// success - set as current clipboard source
	_afxOleState->m_pClipboardSource = this;
	ASSERT(::OleIsCurrentClipboard(lpDataObject) == S_OK);
	InternalRelease();
}

void PASCAL COleDataSource::FlushClipboard()
{
	if (GetClipboardOwner() != NULL)
	{
		// active clipboard source and it is on the clipboard - flush it
		::OleFlushClipboard();

		// shouldn't be clipboard owner any more...
		ASSERT(GetClipboardOwner() == NULL);
	}
}

COleDataSource* PASCAL COleDataSource::GetClipboardOwner()
{
	_AFX_OLE_STATE* pOleState = _afxOleState;
	if (pOleState->m_pClipboardSource == NULL)
		return NULL;    // can't own the clipboard if pClipboardSource isn't set

	ASSERT_VALID(pOleState->m_pClipboardSource);
	LPDATAOBJECT lpDataObject = (LPDATAOBJECT)
		pOleState->m_pClipboardSource->GetInterface(&IID_IDataObject);
	if (::OleIsCurrentClipboard(lpDataObject) != S_OK)
	{
		pOleState->m_pClipboardSource = NULL;
		return NULL;    // don't own the clipboard anymore
	}

	// return current clipboard source
	return pOleState->m_pClipboardSource;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource cache allocation

AFX_DATACACHE_ENTRY* COleDataSource::GetCacheEntry(
	LPFORMATETC lpFormatEtc, DATADIR nDataDir)
{
	AFX_DATACACHE_ENTRY* pEntry = Lookup(lpFormatEtc, nDataDir);
	if (pEntry != NULL)
	{
		// cleanup current entry and return it
		CoTaskMemFree(pEntry->m_formatEtc.ptd);
		::ReleaseStgMedium(&pEntry->m_stgMedium);
	}
	else
	{
		// allocate space for item at m_nSize (at least room for 1 item)
		if (m_pDataCache == NULL || m_nSize == m_nMaxSize)
		{
			ASSERT(m_nGrowBy != 0);
			AFX_DATACACHE_ENTRY* pCache = new AFX_DATACACHE_ENTRY[m_nMaxSize+m_nGrowBy];
			m_nMaxSize += m_nGrowBy;
			if (m_pDataCache != NULL)
			{
				memcpy(pCache, m_pDataCache, m_nSize * sizeof(AFX_DATACACHE_ENTRY));
				delete[] m_pDataCache;
			}
			m_pDataCache = pCache;
		}
		ASSERT(m_pDataCache != NULL);
		ASSERT(m_nMaxSize != 0);

		pEntry = &m_pDataCache[m_nSize++];
	}

	// fill the cache entry with the format and data direction and return it
	pEntry->m_nDataDir = nDataDir;
	pEntry->m_formatEtc = *lpFormatEtc;
	return pEntry;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource operations

// for HGLOBAL based cached render
void COleDataSource::CacheGlobalData(CLIPFORMAT cfFormat, HGLOBAL hGlobal,
	LPFORMATETC lpFormatEtc)
{
	ASSERT(hGlobal != NULL);
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);
	lpFormatEtc->tymed = TYMED_HGLOBAL;

	// add it to the cache
	AFX_DATACACHE_ENTRY* pEntry = GetCacheEntry(lpFormatEtc, DATADIR_GET);
	pEntry->m_stgMedium.tymed = TYMED_HGLOBAL;
	pEntry->m_stgMedium.hGlobal = hGlobal;
	pEntry->m_stgMedium.pUnkForRelease = NULL;
}

// for raw LPSTGMEDIUM cached render
void COleDataSource::CacheData(CLIPFORMAT cfFormat, LPSTGMEDIUM lpStgMedium,
	LPFORMATETC lpFormatEtc)
{
	ASSERT(lpStgMedium == NULL || lpStgMedium->tymed != TYMED_NULL);
	ASSERT(AfxIsValidAddress(lpStgMedium, sizeof(STGMEDIUM), FALSE));
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// Only these TYMED_GDI formats can be copied, so can't serve as
	//  cache content (you must use DelayRenderData instead)
	// When using COleServerItem::CopyToClipboard this means providing an
	//  override of COleServerItem::OnGetClipboardData to provide a custom
	//  delayed rendering clipboard object.
	ASSERT(lpStgMedium->tymed != TYMED_GDI ||
		lpFormatEtc->cfFormat == CF_METAFILEPICT ||
		lpFormatEtc->cfFormat == CF_PALETTE ||
		lpFormatEtc->cfFormat == CF_BITMAP);
	lpFormatEtc->tymed = lpStgMedium->tymed;

	// add it to the cache
	AFX_DATACACHE_ENTRY* pEntry = GetCacheEntry(lpFormatEtc, DATADIR_GET);
	pEntry->m_stgMedium = *lpStgMedium;
}

// for CFile* based delayed render
void COleDataSource::DelayRenderFileData(CLIPFORMAT cfFormat,
	LPFORMATETC lpFormatEtc)
{
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);
	lpFormatEtc->tymed |= TYMED_ISTREAM|TYMED_HGLOBAL;

	// add it to the cache
	AFX_DATACACHE_ENTRY* pEntry = GetCacheEntry(lpFormatEtc, DATADIR_GET);
	pEntry->m_stgMedium.tymed = TYMED_NULL;
	pEntry->m_stgMedium.hGlobal = NULL;
	pEntry->m_stgMedium.pUnkForRelease = NULL;
}

// for LPSTGMEDIUM or HGLOBAL based delayed render
void COleDataSource::DelayRenderData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	if (lpFormatEtc == NULL)
	{
		lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);
		lpFormatEtc->tymed = TYMED_HGLOBAL;
	}
	// insure that cfFormat member is set
	if (cfFormat != 0)
		lpFormatEtc->cfFormat = cfFormat;

	// add it to the cache
	AFX_DATACACHE_ENTRY* pEntry = GetCacheEntry(lpFormatEtc, DATADIR_GET);
	memset(&pEntry->m_stgMedium, 0, sizeof pEntry->m_stgMedium);
}

// DelaySetData -- used to allow SetData on given LPFORMATETC
void COleDataSource::DelaySetData(CLIPFORMAT cfFormat, LPFORMATETC lpFormatEtc)
{
	ASSERT(lpFormatEtc == NULL ||
		AfxIsValidAddress(lpFormatEtc, sizeof(FORMATETC), FALSE));

	// fill in FORMATETC struct
	FORMATETC formatEtc;
	lpFormatEtc = _AfxFillFormatEtc(lpFormatEtc, cfFormat, &formatEtc);

	// add it to the cache
	AFX_DATACACHE_ENTRY* pEntry = GetCacheEntry(lpFormatEtc, DATADIR_SET);
	pEntry->m_stgMedium.tymed = TYMED_NULL;
	pEntry->m_stgMedium.hGlobal = NULL;
	pEntry->m_stgMedium.pUnkForRelease = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource cache implementation

AFX_DATACACHE_ENTRY* COleDataSource::Lookup(
	LPFORMATETC lpFormatEtc, DATADIR nDataDir) const
{
	AFX_DATACACHE_ENTRY* pLast = NULL;

	// look for suitable match to lpFormatEtc in cache
	for (UINT nIndex = 0; nIndex < m_nSize; nIndex++)
	{
		// get entry from cache at nIndex
		AFX_DATACACHE_ENTRY* pCache = &m_pDataCache[nIndex];
		FORMATETC *pCacheFormat = &pCache->m_formatEtc;

		// check for match
		if (pCacheFormat->cfFormat == lpFormatEtc->cfFormat &&
			(pCacheFormat->tymed & lpFormatEtc->tymed) != 0 &&
			(pCache->m_stgMedium.tymed == TYMED_NULL ||
				pCacheFormat->lindex == lpFormatEtc->lindex) &&
			pCacheFormat->dwAspect == lpFormatEtc->dwAspect &&
			pCache->m_nDataDir == nDataDir)
		{
			// for backward compatibility we match even if we never
			// find an exact match for the DVTARGETDEVICE
			pLast = pCache;
			DVTARGETDEVICE* ptd1 = pCacheFormat->ptd;
			DVTARGETDEVICE* ptd2 = lpFormatEtc->ptd;

			if (ptd1 == NULL && ptd2 == NULL)
			{
				// both target devices are NULL (exact match), so return it
				break;
			}
			if (ptd1 != NULL && ptd2 != NULL &&
				ptd1->tdSize == ptd2->tdSize &&
				memcmp(ptd1, ptd2, ptd1->tdSize) == 0)
			{
				// exact match, so return it
				break;
			}
			// continue looking for better match
		}
	}

	return pLast;    // not found
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource overidable default implementation

BOOL COleDataSource::OnRenderGlobalData(
	LPFORMATETC /*lpFormatEtc*/, HGLOBAL* /*phGlobal*/)
{
	return FALSE;   // default does nothing
}

BOOL COleDataSource::OnRenderFileData(
	LPFORMATETC /*lpFormatEtc*/, CFile* /*pFile*/)
{
	return FALSE;   // default does nothing
}

BOOL COleDataSource::OnRenderData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	// attempt TYMED_HGLOBAL as prefered format
	if (lpFormatEtc->tymed & TYMED_HGLOBAL)
	{
		// attempt HGLOBAL delay render hook
		HGLOBAL hGlobal = lpStgMedium->hGlobal;
		if (OnRenderGlobalData(lpFormatEtc, &hGlobal))
		{
			ASSERT(lpStgMedium->tymed != TYMED_HGLOBAL ||
				(lpStgMedium->hGlobal == hGlobal));
			ASSERT(hGlobal != NULL);
			lpStgMedium->tymed = TYMED_HGLOBAL;
			lpStgMedium->hGlobal = hGlobal;
			return TRUE;
		}

		// attempt CFile* based delay render hook
		CSharedFile file;
		if (lpStgMedium->tymed == TYMED_HGLOBAL)
		{
			ASSERT(lpStgMedium->hGlobal != NULL);
			file.SetHandle(lpStgMedium->hGlobal, FALSE);
		}
		if (OnRenderFileData(lpFormatEtc, &file))
		{
			lpStgMedium->tymed = TYMED_HGLOBAL;
			lpStgMedium->hGlobal = file.Detach();
			ASSERT(lpStgMedium->hGlobal != NULL);
			return TRUE;
		}
		if (lpStgMedium->tymed == TYMED_HGLOBAL)
			file.Detach();
	}

	// attempt TYMED_ISTREAM format
	if (lpFormatEtc->tymed & TYMED_ISTREAM)
	{
		COleStreamFile file;
		if (lpStgMedium->tymed == TYMED_ISTREAM)
		{
			ASSERT(lpStgMedium->pstm != NULL);
			file.Attach(lpStgMedium->pstm);
		}
		else
		{
			if (!file.CreateMemoryStream())
				AfxThrowMemoryException();
		}
		// get data into the stream
		if (OnRenderFileData(lpFormatEtc, &file))
		{
			lpStgMedium->tymed = TYMED_ISTREAM;
			lpStgMedium->pstm = file.Detach();
			return TRUE;
		}
		if (lpStgMedium->tymed == TYMED_ISTREAM)
			file.Detach();
	}

	return FALSE;   // default does nothing
}

BOOL COleDataSource::OnSetData(
	LPFORMATETC /*lpFormatEtc*/, LPSTGMEDIUM /*lpStgMedium*/, BOOL /*bRelease*/)
{
	return FALSE;   // default does nothing
}

/////////////////////////////////////////////////////////////////////////////
// CEnumFormatEtc - enumerator for array for FORMATETC structures

class CEnumFormatEtc : public CEnumArray
{
// Constructors
public:
	CEnumFormatEtc();

// Operations
	void AddFormat(const FORMATETC* lpFormatEtc);

// Implementation
public:
	virtual ~CEnumFormatEtc();

protected:
	virtual BOOL OnNext(void* pv);

	UINT m_nMaxSize;    // number of items allocated (>= m_nSize)
	DECLARE_INTERFACE_MAP()
};

BEGIN_INTERFACE_MAP(CEnumFormatEtc, CEnumArray)
	INTERFACE_PART(CEnumFormatEtc, IID_IEnumFORMATETC, EnumVOID)
END_INTERFACE_MAP()

CEnumFormatEtc::CEnumFormatEtc()
	: CEnumArray(sizeof(FORMATETC), NULL, 0, TRUE)
{
	m_nMaxSize = 0;
}

CEnumFormatEtc::~CEnumFormatEtc()
{
	if (m_pClonedFrom == NULL)
	{
		// release all of the pointers to DVTARGETDEVICE
		LPFORMATETC lpFormatEtc = (LPFORMATETC)m_pvEnum;
		for (UINT nIndex = 0; nIndex < m_nSize; nIndex++)
			CoTaskMemFree(lpFormatEtc[nIndex].ptd);
	}
	// destructor will free the actual array (if it was not a clone)
}

BOOL CEnumFormatEtc::OnNext(void* pv)
{
	if (!CEnumArray::OnNext(pv))
		return FALSE;

	// any outgoing formatEtc may require the DVTARGETDEVICE to
	//  be copied (the caller has responsibility to free it)
	LPFORMATETC lpFormatEtc = (LPFORMATETC)pv;
	if (lpFormatEtc->ptd != NULL)
	{
		lpFormatEtc->ptd = _AfxOleCopyTargetDevice(lpFormatEtc->ptd);
		if (lpFormatEtc->ptd == NULL)
			AfxThrowMemoryException();
	}
	// otherwise, copying worked...
	return TRUE;
}

void CEnumFormatEtc::AddFormat(const FORMATETC* lpFormatEtc)
{
	ASSERT(m_nSize <= m_nMaxSize);

	if (m_nSize == m_nMaxSize)
	{
		// not enough space for new item -- allocate more
		FORMATETC* pListNew = new FORMATETC[m_nSize+10];
		m_nMaxSize += 10;
		memcpy(pListNew, m_pvEnum, m_nSize*sizeof(FORMATETC));
		delete m_pvEnum;
		m_pvEnum = (BYTE*)pListNew;
	}

	// add this item to the list
	ASSERT(m_nSize < m_nMaxSize);
	FORMATETC* pFormat = &((FORMATETC*)m_pvEnum)[m_nSize];
	pFormat->cfFormat = lpFormatEtc->cfFormat;
	pFormat->ptd = lpFormatEtc->ptd;
		// Note: ownership of lpFormatEtc->ptd is transfered with this call.
	pFormat->dwAspect = lpFormatEtc->dwAspect;
	pFormat->lindex = lpFormatEtc->lindex;
	pFormat->tymed = lpFormatEtc->tymed;
	++m_nSize;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource::XDataObject

BEGIN_INTERFACE_MAP(COleDataSource, CCmdTarget)
	INTERFACE_PART(COleDataSource, IID_IDataObject, DataObject)
END_INTERFACE_MAP()

STDMETHODIMP_(ULONG) COleDataSource::XDataObject::AddRef()
{
	METHOD_PROLOGUE_EX_(COleDataSource, DataObject)
	return pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleDataSource::XDataObject::Release()
{
	METHOD_PROLOGUE_EX_(COleDataSource, DataObject)
	return pThis->ExternalRelease();
}

STDMETHODIMP COleDataSource::XDataObject::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(COleDataSource, DataObject)
	return pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleDataSource::XDataObject::GetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX(COleDataSource, DataObject)
	ASSERT_VALID(pThis);

	// attempt to find match in the cache
	AFX_DATACACHE_ENTRY* pCache = pThis->Lookup(lpFormatEtc, DATADIR_GET);
	if (pCache == NULL)
		return DATA_E_FORMATETC;

	// use cache if entry is not delay render
	memset(lpStgMedium, 0, sizeof(STGMEDIUM));
	if (pCache->m_stgMedium.tymed != TYMED_NULL)
	{
		// Copy the cached medium into the lpStgMedium provided by caller.
		if (!_AfxCopyStgMedium(lpFormatEtc->cfFormat, lpStgMedium,
		  &pCache->m_stgMedium))
			return DATA_E_FORMATETC;

		// format was supported for copying
		return S_OK;
	}

	SCODE sc = DATA_E_FORMATETC;
	TRY
	{
		// attempt LPSTGMEDIUM based delay render
		if (pThis->OnRenderData(lpFormatEtc, lpStgMedium))
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

STDMETHODIMP COleDataSource::XDataObject::GetDataHere(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium)
{
	METHOD_PROLOGUE_EX(COleDataSource, DataObject)
	ASSERT_VALID(pThis);

	// these two must be the same
	ASSERT(lpFormatEtc->tymed == lpStgMedium->tymed);
	lpFormatEtc->tymed = lpStgMedium->tymed;    // but just in case...

	// attempt to find match in the cache
	AFX_DATACACHE_ENTRY* pCache = pThis->Lookup(lpFormatEtc, DATADIR_GET);
	if (pCache == NULL)
		return DATA_E_FORMATETC;

	// handle cached medium and copy
	if (pCache->m_stgMedium.tymed != TYMED_NULL)
	{
		// found a cached format -- copy it to dest medium
		ASSERT(pCache->m_stgMedium.tymed == lpStgMedium->tymed);
		if (!_AfxCopyStgMedium(lpFormatEtc->cfFormat, lpStgMedium,
		  &pCache->m_stgMedium))
			return DATA_E_FORMATETC;

		// format was supported for copying
		return S_OK;
	}

	SCODE sc = DATA_E_FORMATETC;
	TRY
	{
		// attempt LPSTGMEDIUM based delay render
		if (pThis->OnRenderData(lpFormatEtc, lpStgMedium))
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

STDMETHODIMP COleDataSource::XDataObject::QueryGetData(LPFORMATETC lpFormatEtc)
{
	METHOD_PROLOGUE_EX_(COleDataSource, DataObject)

	// attempt to find match in the cache
	AFX_DATACACHE_ENTRY* pCache = pThis->Lookup(lpFormatEtc, DATADIR_GET);
	if (pCache == NULL)
		return DATA_E_FORMATETC;

	// it was found in the cache or can be rendered -- success
	return S_OK;
}

STDMETHODIMP COleDataSource::XDataObject::GetCanonicalFormatEtc(
	LPFORMATETC /*lpFormatEtcIn*/, LPFORMATETC /*lpFormatEtcOut*/)
{
	// because we support the target-device (ptd) for server metafile format,
	//  all members of the FORMATETC are significant.

	return DATA_S_SAMEFORMATETC;
}

STDMETHODIMP COleDataSource::XDataObject::SetData(
	LPFORMATETC lpFormatEtc, LPSTGMEDIUM lpStgMedium, BOOL bRelease)
{
	METHOD_PROLOGUE_EX(COleDataSource, DataObject)
	ASSERT_VALID(pThis);

	ASSERT(lpFormatEtc->tymed == lpStgMedium->tymed);

	// attempt to find match in the cache
	AFX_DATACACHE_ENTRY* pCache = pThis->Lookup(lpFormatEtc, DATADIR_SET);
	if (pCache == NULL)
		return DATA_E_FORMATETC;

	ASSERT(pCache->m_stgMedium.tymed == TYMED_NULL);

	SCODE sc = E_UNEXPECTED;
	TRY
	{
		// attempt LPSTGMEDIUM based SetData
		if (pThis->OnSetData(lpFormatEtc, lpStgMedium, bRelease))
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

STDMETHODIMP COleDataSource::XDataObject::EnumFormatEtc(
	DWORD dwDirection, LPENUMFORMATETC* ppenumFormatEtc)
{
	METHOD_PROLOGUE_EX_(COleDataSource, DataObject)

	*ppenumFormatEtc = NULL;

	CEnumFormatEtc* pFormatList = NULL;
	SCODE sc = E_OUTOFMEMORY;
	TRY
	{
		// generate a format list from the cache
		pFormatList = new CEnumFormatEtc;
		for (UINT nIndex = 0; nIndex < pThis->m_nSize; nIndex++)
		{
			AFX_DATACACHE_ENTRY* pCache = &pThis->m_pDataCache[nIndex];
			if ((DWORD)pCache->m_nDataDir & dwDirection)
			{
				// entry should be enumerated -- add it to the list
				FORMATETC formatEtc;
				_AfxOleCopyFormatEtc(&formatEtc, &pCache->m_formatEtc);
				pFormatList->AddFormat(&formatEtc);
			}
		}
		// give it away to OLE (ref count is already 1)
		*ppenumFormatEtc = (LPENUMFORMATETC)&pFormatList->m_xEnumVOID;
		sc = S_OK;
	}
	END_TRY

	return sc;
}

STDMETHODIMP COleDataSource::XDataObject::DAdvise(
	FORMATETC* /*pFormatetc*/, DWORD /*advf*/,
	LPADVISESINK /*pAdvSink*/, DWORD* pdwConnection)
{
	*pdwConnection = 0;
	return OLE_E_ADVISENOTSUPPORTED;
}

STDMETHODIMP COleDataSource::XDataObject::DUnadvise(DWORD /*dwConnection*/)
{
	return OLE_E_ADVISENOTSUPPORTED;
}

STDMETHODIMP COleDataSource::XDataObject::EnumDAdvise(
	LPENUMSTATDATA* ppenumAdvise)
{
	*ppenumAdvise = NULL;
	return OLE_E_ADVISENOTSUPPORTED;
}

/////////////////////////////////////////////////////////////////////////////
// COleDataSource diagnostics

#ifdef _DEBUG
void COleDataSource::AssertValid() const
{
	CCmdTarget::AssertValid();
	ASSERT(m_nSize <= m_nMaxSize);
	ASSERT(m_nMaxSize != 0 || m_pDataCache == NULL);
}

void COleDataSource::Dump(CDumpContext& dc) const
{
	CCmdTarget::Dump(dc);

	dc << "m_nMaxSize = " << m_nMaxSize;
	dc << "\nm_nSize = " << m_nSize;
	dc << "\nm_pDataCache = " << m_pDataCache;

	for (UINT n = 0; n < m_nSize; n++)
	{
		dc << "\n\tentry [" << n << "] = {";
		AFX_DATACACHE_ENTRY& rEntry = m_pDataCache[n];
		dc << "\n\t m_formatEtc.cfFormat = " << rEntry.m_formatEtc.cfFormat;
		dc << "\n\t m_formatEtc.pdt = " << rEntry.m_formatEtc.ptd;
		dc << "\n\t m_formatEtc.dwAspect = " << rEntry.m_formatEtc.dwAspect;
		dc << "\n\t m_formatEtc.lindex = " << rEntry.m_formatEtc.lindex;
		dc << "\n\t m_formatEtc.tymed = " << rEntry.m_formatEtc.tymed;
		dc << "\n\t m_stgMedium.tymed = " << rEntry.m_stgMedium.tymed;
		dc << "\n\t m_nDataDir = " << (UINT)rEntry.m_nDataDir;
		dc << "\n\t}";
	}

	dc << "\n";
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
