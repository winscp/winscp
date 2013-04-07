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

#ifdef AFX_OLE_SEG
#pragma code_seg(AFX_OLE_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#pragma inline_depth(0)

/////////////////////////////////////////////////////////////////////////////
// _AfxBindStatusCallback for CAsyncMonikerFile implementation

class _AfxBindStatusCallback: public IBindStatusCallback
{
private:
	class CInnerUnknown : public IUnknown
	{
	protected:
		_AfxBindStatusCallback* m_pThis;
		friend class _AfxBindStatusCallback;

	public:
		inline CInnerUnknown() { }
		inline void SetpThis(_AfxBindStatusCallback* pThis) { ASSERT(pThis != NULL); m_pThis = pThis; }

		STDMETHOD_(ULONG, AddRef)()
		{
			return InterlockedIncrement((long*)&m_pThis->m_dwRef);
		}

		STDMETHOD_(ULONG, Release)()
		{
			unsigned long lResult = InterlockedDecrement((long*)&m_pThis->m_dwRef);
			if (lResult == 0)
				delete m_pThis;
			return lResult;
		}

		STDMETHOD(QueryInterface)(REFIID iid, void** ppvObject)
		{
			if (!ppvObject)
				return E_POINTER;

			// check for the interfaces this object knows about
			if (iid == IID_IUnknown)
			{
				*ppvObject = (IUnknown*)this;
				AddRef();
				return S_OK;
			}
			if (iid == IID_IBindStatusCallback)
			{
				*ppvObject = (IBindStatusCallback*)m_pThis;
				m_pThis->AddRef();
				return S_OK;
			}

			// otherwise, incorrect IID, and thus error
			return E_NOINTERFACE;
		}
	};

public:
	inline _AfxBindStatusCallback(CAsyncMonikerFile* pOwner, IUnknown* pUnkControlling)
		: m_pOwner(pOwner), m_dwRef(0)
	{
		m_UnkInner.SetpThis(this);
		ASSERT(pOwner);
#ifdef _AFXDLL
		m_pModuleState = AfxGetModuleState();
		ASSERT(m_pModuleState != NULL);
#endif
		m_pUnkControlling = pUnkControlling ? pUnkControlling : (IUnknown*)&m_UnkInner;

		AfxOleLockApp();
	}

	inline ~_AfxBindStatusCallback()
	{
		AFX_MANAGE_STATE(m_pModuleState);
		AfxOleUnlockApp();
	}

	inline void Orphan() { m_pOwner = NULL; }

	STDMETHOD_(ULONG, AddRef)()
		{ return m_pUnkControlling->AddRef(); }
	STDMETHOD_(ULONG, Release)()
		{ return m_pUnkControlling->Release(); }
	STDMETHOD(QueryInterface)(REFIID iid, void** ppvObject)
		{ return m_pUnkControlling->QueryInterface(iid, ppvObject); }


	const CAsyncMonikerFile* GetOwner() const{ return m_pOwner; }
	DWORD GetRefcount() const { return m_dwRef; }
	const IUnknown* GetControllingUnknown() const { return m_pUnkControlling; }
	IUnknown* GetControllingUnknown() { return m_pUnkControlling; }
	const IUnknown* GetInnerUnknown() const { return &m_UnkInner; }
	IUnknown* GetInnerUnknown() { return &m_UnkInner; }
	const _AfxBindStatusCallback* GetpThisOfInnerUnknown() const { return m_UnkInner.m_pThis; }
#ifdef _AFXDLL
	const AFX_MODULE_STATE* GetModuleState() const { return m_pModuleState; }
#endif
protected:
	friend class CInnerUnknown;
	DWORD m_dwRef;
private:
	IUnknown* m_pUnkControlling;
	CInnerUnknown m_UnkInner;
	CAsyncMonikerFile* m_pOwner;
#ifdef _AFXDLL
	AFX_MODULE_STATE* m_pModuleState;
#endif

	STDMETHOD(GetBindInfo)(
		DWORD __RPC_FAR *pgrfBINDF, BINDINFO __RPC_FAR *pbindinfo)
	{
		ASSERT(m_pOwner);
		if (!pgrfBINDF || !pbindinfo)
			return E_POINTER;
		if (pbindinfo->cbSize<sizeof(BINDINFO))
			return E_INVALIDARG;
		if (!m_pOwner)
			return E_FAIL;

		AFX_MANAGE_STATE(m_pModuleState);
		pbindinfo->szExtraInfo = NULL;
		TRY
		{
			*pgrfBINDF = m_pOwner->GetBindInfo();
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(OnStartBinding)(
		DWORD dwReserved, IBinding __RPC_FAR *pBinding)
	{
		ASSERT(m_pOwner);
		UNUSED_ALWAYS(dwReserved);
		if (!pBinding)
			return E_POINTER;
		if (!m_pOwner)
			return E_FAIL;

		AFX_MANAGE_STATE(m_pModuleState);
		TRY
		{
			m_pOwner->SetBinding(pBinding);
			m_pOwner->OnStartBinding();
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(GetPriority)(LONG __RPC_FAR *pnPriority)
	{
		ASSERT(m_pOwner);
		if (!pnPriority)
			return E_POINTER;
		if (!m_pOwner)
			return E_FAIL;
		AFX_MANAGE_STATE(m_pModuleState);
		TRY
		{
			*pnPriority = m_pOwner->GetPriority();
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(OnProgress)(
		ULONG ulProgress, ULONG ulProgressMax, ULONG ulStatusCode,
		LPCOLESTR szStatusText)
	{
		ASSERT(m_pOwner);
		if (!m_pOwner)
			return E_FAIL;
		USES_CONVERSION;
		AFX_MANAGE_STATE(m_pModuleState);
		TRY
		{
			m_pOwner->OnProgress(ulProgress, ulProgressMax, ulStatusCode, OLE2CT(szStatusText));
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(OnDataAvailable)(
		DWORD grfBSCF, DWORD dwSize, FORMATETC __RPC_FAR *pformatetc,
		STGMEDIUM __RPC_FAR *pstgmed)
	{
		ASSERT(m_pOwner);
		if (!m_pOwner)
			return E_FAIL;
		AFX_MANAGE_STATE(m_pModuleState);
		TRY
		{
			m_pOwner->SetFormatEtc(pformatetc);
			if (grfBSCF&BSCF_FIRSTDATANOTIFICATION)
			{
				if (!pstgmed || !pformatetc)
					return E_POINTER;
				if ((pstgmed->tymed != TYMED_ISTREAM) ||
					!pstgmed->pstm)
					return E_UNEXPECTED;
				ASSERT(!m_pOwner->GetStream());
				m_pOwner->COleStreamFile::Attach(pstgmed->pstm);
				pstgmed->pstm->AddRef();
			}

			m_pOwner->OnDataAvailable(dwSize, grfBSCF);
		}
		CATCH_ALL(e)
		{
			m_pOwner->SetFormatEtc(NULL);
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		m_pOwner->SetFormatEtc(NULL);
		return S_OK;
	}

	STDMETHOD(OnLowResource)(DWORD dwReserved)
	{
		ASSERT(m_pOwner);
		if (!m_pOwner)
			return E_FAIL;
		AFX_MANAGE_STATE(m_pModuleState);
		UNUSED_ALWAYS(dwReserved);
		TRY
		{
			m_pOwner->OnLowResource();
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(OnStopBinding)(HRESULT hresult, LPCOLESTR szError)
	{
		//Does not ASSERT(m_pOwner) because this can be called
		//after it has been Orphan()ed.
		if (!m_pOwner)
			return E_FAIL;
		USES_CONVERSION;
		AFX_MANAGE_STATE(m_pModuleState);
		ASSERT(m_pOwner->GetBinding());
		TRY
		{
			m_pOwner->m_bStopBindingReceived = TRUE;
			m_pOwner->OnStopBinding(hresult, OLE2CT(szError));

			if (m_pOwner)
			{
				// Calling EndCallbacks will result in a inner Release. Our
				// caller has an inner refcount on us, possibly through the
				// controlling unknown, and we have an inner refcount from
				// m_pOwner, so m_dwRef > 1 and it's safe to call EndCallbacks.
				ASSERT(m_dwRef > 1);
				m_pOwner->EndCallbacks();
			}
		}
		CATCH_ALL(e)
		{
			HRESULT hr = ResultFromScode(COleException::Process(e));
			DELETE_EXCEPTION(e);
			return hr;
		}
		END_CATCH_ALL
		return S_OK;
	}

	STDMETHOD(OnObjectAvailable)(REFIID riid, IUnknown __RPC_FAR *punk)
	{
#ifdef _DEBUG
		AFX_MANAGE_STATE(m_pModuleState);
		ASSERT(FALSE);  // This function should never be called.
#endif //_DEBUG
		UNUSED_ALWAYS(riid);
		UNUSED_ALWAYS(punk);
		return E_UNEXPECTED;
	}
};

/////////////////////////////////////////////////////////////////////////////
// Helper functions for CAsyncMonikerFile implementation

AFX_STATIC inline IBindHost* AFXAPI _AfxTrySPForBindHost(IServiceProvider* pServiceProvider)
{
	ASSERT(pServiceProvider);
	IBindHost* pBindHost;
	HRESULT hr=pServiceProvider->QueryService(SID_IBindHost, IID_IBindHost,
		(void**)&pBindHost);
	if (SUCCEEDED(hr))
		return pBindHost;
	else
		return NULL;
}

AFX_STATIC inline IBindHost* AFXAPI _AfxTryQIForBindHost(IUnknown* pUnk)
{
	ASSERT(pUnk);
	IPTR(IBindHost) pBindHost;
	HRESULT hr = pBindHost.QueryInterface(pUnk);
	if (SUCCEEDED(hr))
		return pBindHost;
	else
		return NULL;
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncMonikerFile implementation

CAsyncMonikerFile::CAsyncMonikerFile() : m_pFormatEtc(NULL),
	m_pAfxBSCCurrent(NULL)
{
}

CAsyncMonikerFile::~CAsyncMonikerFile()
{
	CAsyncMonikerFile::Close();
}

void CAsyncMonikerFile::EndCallbacks()
{
	if (m_pAfxBSCCurrent)
	{
		m_pAfxBSCCurrent->Orphan();
		IUnknown* pUnkInner = m_pAfxBSCCurrent->GetInnerUnknown();
		ASSERT(pUnkInner != NULL);
		pUnkInner->Release();
		m_pAfxBSCCurrent = NULL;
	}
}

UINT CAsyncMonikerFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT_VALID(this);
	ASSERT(m_lpStream != NULL);

	DWORD dwBytesRead;
	HRESULT hr = _AfxReadFromStream(m_lpStream, lpBuf, nCount, dwBytesRead);

	if ((hr != S_OK) && (hr != S_ASYNCHRONOUS) && (hr != E_PENDING))
		_AfxThrowOleFileException(hr);

	// always return number of bytes read
	return (UINT)dwBytesRead;
}

void CAsyncMonikerFile::Close()
{
	ASSERT_VALID(this);
	if (m_Binding.GetInterfacePtr())
	{
		if (!m_bStopBindingReceived)
			m_Binding->Abort();
		m_Binding.Release();
	}
	EndCallbacks();
	CMonikerFile::Close();
}

BOOL CAsyncMonikerFile::Open(IMoniker* pMoniker, IBindHost* pBindHost, CFileException* pError)
{
	if (!pBindHost)
		return CAsyncMonikerFile::Open(pMoniker, pError);
	Close();
	IPTR(IBindCtx) pBindCtx(CreateBindContext(pError), FALSE);
	if (pError && (pError->m_cause != CFileException::none))
		return FALSE;
	IPTR(IUnknown) pBSCUnk(CreateBindStatusCallback(NULL), FALSE);
	IPTR(IBindStatusCallback) pBSC;
	pBSC.QueryInterface(pBSCUnk);

	m_bStopBindingReceived = FALSE;
	return Attach(pMoniker, pBindHost, pBSC, pBindCtx, pError);
}

BOOL CAsyncMonikerFile::Open(LPCTSTR lpszURL, IBindHost* pBindHost, CFileException* pError)
{
	if (!pBindHost)
		return CAsyncMonikerFile::Open(lpszURL, pError);
	Close();
	IPTR(IBindCtx) pBindCtx(CreateBindContext(pError), FALSE);
	if (pError && (pError->m_cause != CFileException::none))
		return FALSE;
	IPTR(IUnknown) pBSCUnk(CreateBindStatusCallback(NULL), FALSE);
	IPTR(IBindStatusCallback) pBSC;
	pBSC.QueryInterface(pBSCUnk);

	m_bStopBindingReceived = FALSE;
	return Attach(lpszURL, pBindHost, pBSC, pBindCtx, pError);
}

BOOL CAsyncMonikerFile::Open(LPCTSTR lpszURL, CFileException* pError)
{
	IPTR(IBindHost) pBindHost(CreateBindHost(), FALSE);

	return Open(lpszURL, static_cast<IBindHost*>(pBindHost), pError);
}

BOOL CAsyncMonikerFile::Open(IMoniker* pMoniker, CFileException* pError)
{
	IPTR(IBindHost) pBindHost(CreateBindHost(), FALSE);

	return Open(pMoniker, (IBindHost*)pBindHost, pError);
}

BOOL CAsyncMonikerFile::Open(LPCTSTR lpszURL,
	IServiceProvider* pServiceProvider, CFileException* pError)
{
	if (!pServiceProvider)
		return CAsyncMonikerFile::Open(lpszURL, pError);
	IPTR(IBindHost) pBindHost;
	pBindHost = _AfxTrySPForBindHost(pServiceProvider);
	if (!pBindHost.GetInterfacePtr())
		pBindHost = _AfxTryQIForBindHost(pServiceProvider);
	if (pBindHost.GetInterfacePtr())
		return Open(lpszURL, (IBindHost*)pBindHost, pError);
	return CAsyncMonikerFile::Open(lpszURL, pError);
}

BOOL CAsyncMonikerFile::Open(LPCTSTR lpszURL,
	IUnknown* pUnknown, CFileException* pError)
{
	if (!pUnknown)
		return CAsyncMonikerFile::Open(lpszURL, pError);
	IPTR(IBindHost) pBindHost;
	IPTR(IServiceProvider) pServiceProvider;
	HRESULT hr=pServiceProvider.QueryInterface(pUnknown);
	if (SUCCEEDED(hr) && pServiceProvider.GetInterfacePtr())
		pBindHost = _AfxTrySPForBindHost(pServiceProvider);
	if (!pBindHost.GetInterfacePtr())
		pBindHost = _AfxTryQIForBindHost(pUnknown);
	if (pBindHost.GetInterfacePtr())
		return Open(lpszURL, (IBindHost*)pBindHost, pError);

	return CAsyncMonikerFile::Open(lpszURL, pError);
}

BOOL CAsyncMonikerFile::Open(IMoniker* pMoniker,
	IServiceProvider* pServiceProvider, CFileException* pError)
{
	if (!pServiceProvider)
		return Open(pMoniker, pError);
	IPTR(IBindHost) pBindHost;
	pBindHost = _AfxTrySPForBindHost(pServiceProvider);
	if (!pBindHost.GetInterfacePtr())
		pBindHost = _AfxTryQIForBindHost(pServiceProvider);
	if (pBindHost.GetInterfacePtr())
		return Open(pMoniker, (IBindHost*)pBindHost, pError);
	return Open(pMoniker, pError);
}

BOOL CAsyncMonikerFile::Open(IMoniker* pMoniker,
	IUnknown* pUnknown, CFileException* pError)
{
	if (!pUnknown)
		return Open(pMoniker, pError);
	IPTR(IBindHost) pBindHost;
	IPTR(IServiceProvider) pServiceProvider;
	HRESULT hr=pServiceProvider.QueryInterface(pUnknown);
	if (SUCCEEDED(hr) && pServiceProvider.GetInterfacePtr())
		pBindHost = _AfxTrySPForBindHost(pServiceProvider);
	if (!pBindHost.GetInterfacePtr())
		pBindHost = _AfxTryQIForBindHost(pServiceProvider);
	if (pBindHost.GetInterfacePtr())
		return Open(pMoniker, (IBindHost*)pBindHost, pError);
	return Open(pMoniker, pError);
}

BOOL CAsyncMonikerFile::PostBindToStream(CFileException* pError)
{
	if (S_OK == IsAsyncMoniker(GetMoniker()))
		return TRUE;
	return CMonikerFile::PostBindToStream(pError);
}

IUnknown* CAsyncMonikerFile::CreateBindStatusCallback(IUnknown* pUnkControlling)
{
	ASSERT(NULL == m_pAfxBSCCurrent); // Set by Close()
	m_pAfxBSCCurrent = new _AfxBindStatusCallback(this, pUnkControlling);
	IUnknown* pUnkInner = m_pAfxBSCCurrent->GetInnerUnknown();
	ASSERT(pUnkInner != NULL);
	//Reference inner unknown for m_pAfxBSCCurrent
	pUnkInner->AddRef();
	//Reference inner unknown for return value.
	pUnkInner->AddRef();
	return pUnkInner;
}

DWORD CAsyncMonikerFile::GetBindInfo() const
{
	return BINDF_ASYNCHRONOUS|BINDF_ASYNCSTORAGE;
}

LONG CAsyncMonikerFile::GetPriority() const
{
	return THREAD_PRIORITY_NORMAL;
}

void CAsyncMonikerFile::OnDataAvailable(DWORD dwSize, DWORD bscfFlag)
{
	UNUSED_ALWAYS(dwSize);
	UNUSED_ALWAYS(bscfFlag);
}

void CAsyncMonikerFile::OnLowResource()
{
}

void CAsyncMonikerFile::OnStartBinding()
{
}

void CAsyncMonikerFile::OnStopBinding(HRESULT hresult, LPCTSTR szError)
{
	UNUSED_ALWAYS(hresult);
	UNUSED_ALWAYS(szError);
}

void CAsyncMonikerFile::OnProgress(ULONG ulProgress, ULONG ulProgressMax,
		ULONG ulStatusCode, LPCTSTR szStatusText)
{
	UNUSED_ALWAYS(ulProgress);
	UNUSED_ALWAYS(ulProgressMax);
	UNUSED_ALWAYS(ulStatusCode);
	UNUSED_ALWAYS(szStatusText);
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncMonikerFile diagnostics

#ifdef _DEBUG
void CAsyncMonikerFile::AssertValid() const
{
	if (m_pAfxBSCCurrent)
	{
		ASSERT(m_pAfxBSCCurrent->GetOwner() == this);
		ASSERT(m_pAfxBSCCurrent->GetRefcount() > 0);
#ifdef _AFXDLL
		ASSERT(m_pAfxBSCCurrent->GetModuleState() == AfxGetModuleState());
#endif
		ASSERT(m_pAfxBSCCurrent->GetpThisOfInnerUnknown() == m_pAfxBSCCurrent);
	}
	CMonikerFile::AssertValid();
}

void CAsyncMonikerFile::Dump(CDumpContext& dc) const
{
	CMonikerFile::Dump(dc);

	dc << "\nm_Binding = " << m_Binding.GetInterfacePtr();
	dc << "\nm_pFormatEtc = " << m_pFormatEtc;
	dc << "\nm_pAfxBSCCurrent = " << m_pAfxBSCCurrent;
	if (m_pAfxBSCCurrent)
	{
		dc << "\nm_pAfxBSCCurrent->GetOwner() = " << m_pAfxBSCCurrent->GetOwner();
		dc << "\nm_pAfxBSCCurrent->GetRefcount() = " << m_pAfxBSCCurrent->GetRefcount();
		dc << "\nm_pAfxBSCCurrent->GetControllingUnknown() = " << m_pAfxBSCCurrent->GetControllingUnknown();
		dc << "\nm_pAfxBSCCurrent->GetInnerUnknown() = " << m_pAfxBSCCurrent->GetInnerUnknown();
		dc << "\nm_pAfxBSCCurrent->GetpThisOfInnerUnknown() = " << m_pAfxBSCCurrent->GetpThisOfInnerUnknown();
#ifdef _AFXDLL
		dc << "\nm_pAfxBSCCurrent->GetModuleState() = " << m_pAfxBSCCurrent->GetModuleState();
#endif
	}
	dc << "\n";
}
#endif

////////////////////////////////////////////////////////////////////////////

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CAsyncMonikerFile, CMonikerFile)

////////////////////////////////////////////////////////////////////////////
