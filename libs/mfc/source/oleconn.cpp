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


#define GetConnectionPtr(pTarget, pEntry) \
	(LPCONNECTIONPOINT)((char*)pTarget + pEntry->nOffset + \
			offsetof(CConnectionPoint, m_xConnPt))


/////////////////////////////////////////////////////////////////////////////
// CConnectionPoint

CConnectionPoint::CConnectionPoint() :
	m_pUnkFirstConnection(NULL),
	m_pConnections(NULL)
{
}

CConnectionPoint::~CConnectionPoint()
{
	POSITION pos = GetStartPosition();
	while (pos != NULL)
	{
		LPUNKNOWN pUnk = GetNextConnection(pos);
		ASSERT(pUnk != NULL);
		pUnk->Release();
	}

	if (m_pConnections != NULL)
		delete m_pConnections;
}

POSITION CConnectionPoint::GetStartPosition() const
{
	ASSERT(m_pConnections == NULL || m_pUnkFirstConnection == NULL);

	if (m_pUnkFirstConnection != NULL)
		return (POSITION)-1;

	if (m_pConnections == NULL || m_pConnections->GetSize() == 0)
		return NULL;

	return (POSITION)1;
}

LPUNKNOWN CConnectionPoint::GetNextConnection(POSITION& pos) const
{
	ASSERT(pos != NULL);

	if (pos == (POSITION)-1)
	{
		ASSERT(m_pUnkFirstConnection != NULL);
		ASSERT(m_pConnections == NULL);

		pos = NULL;
		return m_pUnkFirstConnection;
	}

	ASSERT(m_pConnections != NULL);
	ASSERT((long)pos > 0 && (long)pos <= m_pConnections->GetSize());

	int nIndex = (long)pos - 1;
	pos = (POSITION)((long)pos + 1);
	if ((long)pos > m_pConnections->GetSize())
		pos = NULL;
	return (LPUNKNOWN)m_pConnections->GetAt(nIndex);
}

const CPtrArray* CConnectionPoint::GetConnections()
{
	ASSERT_VALID(this);
	if (m_pConnections == NULL)
		CreateConnectionArray();

	ASSERT(m_pConnections != NULL);
	return m_pConnections;
}

void CConnectionPoint::OnAdvise(BOOL)
{
	ASSERT_VALID(this);
}

int CConnectionPoint::GetMaxConnections()
{
	ASSERT_VALID(this);

	// May be overridden by subclass.
	return -1;
}

LPCONNECTIONPOINTCONTAINER CConnectionPoint::GetContainer()
{
	CCmdTarget* pCmdTarget = (CCmdTarget*)((BYTE*)this - m_nOffset);
#ifdef _DEBUG
	pCmdTarget->CCmdTarget::AssertValid();
#endif

	LPCONNECTIONPOINTCONTAINER pCPC = NULL;
	if (SUCCEEDED((HRESULT)pCmdTarget->ExternalQueryInterface(
			&IID_IConnectionPointContainer, (LPVOID*)&pCPC)))
	{
		ASSERT(pCPC != NULL);
	}

	return pCPC;
}

void CConnectionPoint::CreateConnectionArray()
{
	ASSERT(m_pConnections == NULL);

	m_pConnections = new CPtrArray;
	if (m_pUnkFirstConnection != NULL)
	{
		m_pConnections->Add(m_pUnkFirstConnection);
		m_pUnkFirstConnection = NULL;
	}

	ASSERT(m_pConnections != NULL);
	ASSERT(m_pUnkFirstConnection == NULL);
}

int CConnectionPoint::GetConnectionCount()
{
	if (m_pUnkFirstConnection != NULL)
		return 1;

	if (m_pConnections == NULL)
		return 0;

	return m_pConnections->GetSize();
}

LPUNKNOWN CConnectionPoint::QuerySinkInterface(LPUNKNOWN pUnkSink)
{
	LPUNKNOWN pUnkReturn = NULL;
	pUnkSink->QueryInterface(GetIID(), reinterpret_cast<void**>(&pUnkReturn));
	return pUnkReturn;
}

STDMETHODIMP_(ULONG) CConnectionPoint::XConnPt::Release()
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)
	return (ULONG)pThis->InternalRelease();
}

STDMETHODIMP_(ULONG) CConnectionPoint::XConnPt::AddRef()
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)
	return (ULONG)pThis->InternalAddRef();
}

STDMETHODIMP CConnectionPoint::XConnPt::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)

	ASSERT(AfxIsValidAddress(ppvObj, sizeof(LPVOID), FALSE));

	if (IsEqualIID(iid, IID_IUnknown) ||
		IsEqualIID(iid, IID_IConnectionPoint))
	{
		*ppvObj = this;
		AddRef();
		return S_OK;
	}

	return E_NOINTERFACE;
}

STDMETHODIMP CConnectionPoint::XConnPt::GetConnectionInterface(IID* pIID)
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)
	ASSERT(AfxIsValidAddress(pIID, sizeof(IID)));

	*pIID = pThis->GetIID();
	return S_OK;
}

STDMETHODIMP CConnectionPoint::XConnPt::GetConnectionPointContainer(
	IConnectionPointContainer** ppCPC)
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)
	ASSERT(AfxIsValidAddress(ppCPC, sizeof(LPCONNECTIONPOINT)));

	if ((*ppCPC = pThis->GetContainer()) != NULL)
		return S_OK;

	return E_FAIL;
}

STDMETHODIMP CConnectionPoint::XConnPt::Advise(
	LPUNKNOWN pUnkSink, DWORD* pdwCookie)
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)
	ASSERT(AfxIsValidAddress(pUnkSink, sizeof(IUnknown), FALSE));
	ASSERT((pdwCookie == NULL) || AfxIsValidAddress(pdwCookie, sizeof(DWORD)));

	if (pUnkSink == NULL)
		return E_POINTER;

	LPUNKNOWN lpInterface;

	int cMaxConn = pThis->GetMaxConnections();
	if ((cMaxConn >= 0) && (pThis->GetConnectionCount() == cMaxConn))
	{
		return CONNECT_E_ADVISELIMIT;
	}

	if ((lpInterface = pThis->QuerySinkInterface(pUnkSink)) != NULL)
	{
		if (pThis->m_pUnkFirstConnection == NULL &&
			pThis->m_pConnections == NULL)
		{
			pThis->m_pUnkFirstConnection = lpInterface;
		}
		else
		{
			if (pThis->m_pConnections == NULL)
				pThis->CreateConnectionArray();

			pThis->m_pConnections->Add(lpInterface);
		}

		pThis->OnAdvise(TRUE);
		if (pdwCookie != NULL)
			*pdwCookie = (DWORD)lpInterface;
		return S_OK;
	}

	return E_NOINTERFACE;
}

STDMETHODIMP CConnectionPoint::XConnPt::Unadvise(DWORD dwCookie)
{
	METHOD_PROLOGUE_EX_(CConnectionPoint, ConnPt)

	if (pThis->m_pUnkFirstConnection != NULL)
	{
		if ((DWORD)pThis->m_pUnkFirstConnection == dwCookie)
		{
			pThis->m_pUnkFirstConnection->Release();
			pThis->m_pUnkFirstConnection = NULL;
			pThis->OnAdvise(FALSE);
			return S_OK;
		}
		else
		{
			return CONNECT_E_NOCONNECTION;
		}
	}

	if (pThis->m_pConnections == NULL)
		return CONNECT_E_NOCONNECTION;

	LPUNKNOWN pUnkSink;
	int cConnections = pThis->m_pConnections->GetSize();
	for (int i = 0; i < cConnections; i++)
	{
		pUnkSink = (LPUNKNOWN)(pThis->m_pConnections->GetAt(i));
		if ((DWORD)pUnkSink == dwCookie)
		{
			pUnkSink->Release();
			pThis->m_pConnections->RemoveAt(i);
			pThis->OnAdvise(FALSE);
			return S_OK;
		}
	}

	return CONNECT_E_NOCONNECTION;
}

/////////////////////////////////////////////////////////////////////////////
// CEnumConnections

class CEnumConnections : public CEnumArray
{
public:
	CEnumConnections(const void* pvEnum, UINT nSize);
	~CEnumConnections();
	void AddConnection(CONNECTDATA* pConn);

protected:
	virtual BOOL OnNext(void* pv);
	virtual CEnumArray* OnClone();

	UINT m_nMaxSize;    // number of items allocated (>= m_nSize)

	DECLARE_INTERFACE_MAP()
};

BEGIN_INTERFACE_MAP(CEnumConnections, CEnumArray)
	INTERFACE_PART(CEnumConnections, IID_IEnumConnections, EnumVOID)
END_INTERFACE_MAP()


CEnumConnections::CEnumConnections(const void* pvEnum, UINT nSize) :
	CEnumArray(sizeof(CONNECTDATA), pvEnum, nSize, TRUE)
{
	m_nMaxSize = 0;
}

CEnumConnections::~CEnumConnections()
{
	if (m_pClonedFrom == NULL)
	{
		UINT iCP;
		CONNECTDATA* ppCP = (CONNECTDATA*)(VOID *)m_pvEnum;
		for (iCP = 0; iCP < m_nSize; iCP++)
			RELEASE(ppCP[iCP].pUnk);
	}
	// destructor will free the actual array (if it was not a clone)
}

BOOL CEnumConnections::OnNext(void* pv)
{
	if (!CEnumArray::OnNext(pv))
		return FALSE;

	// outgoing connection point needs to be AddRef'ed
	//  (the caller has responsibility to release it)

	((CONNECTDATA*)pv)->pUnk->AddRef();
	return TRUE;
}

CEnumArray* CEnumConnections::OnClone()
{
	ASSERT_VALID(this);
	CEnumConnections* pClone;
	pClone = new CEnumConnections(m_pvEnum, m_nSize);
	pClone->m_bNeedFree = FALSE;
	ASSERT(pClone != NULL);
	ASSERT(!pClone->m_bNeedFree);   // clones should never free themselves
	pClone->m_nCurPos = m_nCurPos;

	// finally, return the clone to OLE
	ASSERT_VALID(pClone);
	return pClone;
}

void CEnumConnections::AddConnection(CONNECTDATA* pConn)
{
	ASSERT(m_nSize <= m_nMaxSize);

	if (m_nSize == m_nMaxSize)
	{
		// not enough space for new item -- allocate more
		CONNECTDATA* pListNew = new CONNECTDATA[m_nSize+2];
		m_nMaxSize += 2;
		if (m_nSize > 0)
			memcpy(pListNew, m_pvEnum, m_nSize*sizeof(CONNECTDATA));
		delete m_pvEnum;
#ifdef _WIN32
		m_pvEnum = (BYTE*)pListNew;
#else
		m_pvEnum = (char*)pListNew;
#endif
	}

	// add this item to the list
	ASSERT(m_nSize < m_nMaxSize);
	((CONNECTDATA*)m_pvEnum)[m_nSize] = *pConn;
	pConn->pUnk->AddRef();
	++m_nSize;
}

STDMETHODIMP CConnectionPoint::XConnPt::EnumConnections(LPENUMCONNECTIONS* ppEnum)
{
	METHOD_PROLOGUE_EX(CConnectionPoint, ConnPt)
	CEnumConnections* pEnum = NULL;
	CONNECTDATA cd;

	TRY
	{
		pEnum = new CEnumConnections(NULL, 0);

		if (pThis->m_pUnkFirstConnection != NULL)
		{
			cd.pUnk = pThis->m_pUnkFirstConnection;
			cd.dwCookie = (DWORD)cd.pUnk;
			pEnum->AddConnection(&cd);
		}

		if (pThis->m_pConnections != NULL)
		{
			int cConnections = pThis->m_pConnections->GetSize();
			for (int i = 0; i < cConnections; i++)
			{
				cd.pUnk = (LPUNKNOWN)(pThis->m_pConnections->GetAt(i));
				cd.dwCookie = (DWORD)cd.pUnk;
				pEnum->AddConnection(&cd);
			}
		}
	}
	CATCH (CException, e)
	{
		delete pEnum;
		pEnum = NULL;
	}
	END_CATCH

	if (pEnum != NULL)
	{
		// create and return the IEnumConnectionPoints object
		*ppEnum = (IEnumConnections*)&pEnum->m_xEnumVOID;
	}
	else
	{
		// no connections: return NULL
		*ppEnum = NULL;
	}
	return (pEnum != NULL) ? S_OK : E_OUTOFMEMORY;
}


/////////////////////////////////////////////////////////////////////////////
// CEnumConnPoints

class CEnumConnPoints : public CEnumArray
{
public:
	CEnumConnPoints(const void* pvEnum, UINT nSize);
	~CEnumConnPoints();
	void AddConnPoint(LPCONNECTIONPOINT pConnPt);

protected:
	virtual BOOL OnNext(void* pv);

	UINT m_nMaxSize;    // number of items allocated (>= m_nSize)

	DECLARE_INTERFACE_MAP()
};

BEGIN_INTERFACE_MAP(CEnumConnPoints, CEnumArray)
	INTERFACE_PART(CEnumConnPoints, IID_IEnumConnectionPoints, EnumVOID)
END_INTERFACE_MAP()


CEnumConnPoints::CEnumConnPoints(const void* pvEnum, UINT nSize) :
	CEnumArray(sizeof(LPCONNECTIONPOINT), pvEnum, nSize, TRUE)
{
	m_nMaxSize = 0;
}

CEnumConnPoints::~CEnumConnPoints()
{
	if (m_pClonedFrom == NULL)
	{
		UINT iCP;
		LPCONNECTIONPOINT* ppCP =
			(LPCONNECTIONPOINT*)(VOID *)m_pvEnum;
		for (iCP = 0; iCP < m_nSize; iCP++)
			RELEASE(ppCP[iCP]);
	}
	// destructor will free the actual array (if it was not a clone)
}

BOOL CEnumConnPoints::OnNext(void* pv)
{
	if (!CEnumArray::OnNext(pv))
		return FALSE;

	// outgoing connection point needs to be AddRef'ed
	//  (the caller has responsibility to release it)

	(*(LPCONNECTIONPOINT*)pv)->AddRef();
	return TRUE;
}

void CEnumConnPoints::AddConnPoint(LPCONNECTIONPOINT pConnPt)
{
	ASSERT(m_nSize <= m_nMaxSize);

	if (m_nSize == m_nMaxSize)
	{
		// not enough space for new item -- allocate more
		LPCONNECTIONPOINT* pListNew = new LPCONNECTIONPOINT[m_nSize+2];
		m_nMaxSize += 2;
		if (m_nSize > 0)
			memcpy(pListNew, m_pvEnum, m_nSize*sizeof(LPCONNECTIONPOINT));
		delete m_pvEnum;
#ifdef _WIN32
		m_pvEnum = (BYTE*)pListNew;
#else
		m_pvEnum = (char*)pListNew;
#endif
	}

	// add this item to the list
	ASSERT(m_nSize < m_nMaxSize);
	((LPCONNECTIONPOINT*)m_pvEnum)[m_nSize] = pConnPt;
	pConnPt->AddRef();
	++m_nSize;
}


/////////////////////////////////////////////////////////////////////////////
// COleConnPtContainer

class COleConnPtContainer : public IConnectionPointContainer
{
public:
#ifndef _AFX_NO_NESTED_DERIVATION
	// required for METHOD_PROLOGUE_EX
	size_t m_nOffset;
	COleConnPtContainer::COleConnPtContainer()
		{ m_nOffset = offsetof(CCmdTarget, m_xConnPtContainer); }
#endif

	STDMETHOD_(ULONG, AddRef)();
	STDMETHOD_(ULONG, Release)();
	STDMETHOD(QueryInterface)(REFIID, LPVOID*);

	STDMETHOD(EnumConnectionPoints)(LPENUMCONNECTIONPOINTS* ppEnum);
	STDMETHOD(FindConnectionPoint)(REFIID iid, LPCONNECTIONPOINT* ppCP);
};

STDMETHODIMP_(ULONG) COleConnPtContainer::AddRef()
{
	METHOD_PROLOGUE_EX_(CCmdTarget, ConnPtContainer)
	return (ULONG)pThis->ExternalAddRef();
}

STDMETHODIMP_(ULONG) COleConnPtContainer::Release()
{
	METHOD_PROLOGUE_EX_(CCmdTarget, ConnPtContainer)
	return (ULONG)pThis->ExternalRelease();
}

STDMETHODIMP COleConnPtContainer::QueryInterface(
	REFIID iid, LPVOID* ppvObj)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, ConnPtContainer)
	return (HRESULT)pThis->ExternalQueryInterface(&iid, ppvObj);
}

STDMETHODIMP COleConnPtContainer::EnumConnectionPoints(
	LPENUMCONNECTIONPOINTS* ppEnum)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, ConnPtContainer)

	CEnumConnPoints* pEnum = NULL;

	TRY
	{
		pEnum = new CEnumConnPoints(NULL, 0);

		// Add connection points that aren't in the connection map
		CPtrArray ptrArray;
		if (pThis->GetExtraConnectionPoints(&ptrArray))
		{
			for (int i = 0; i < ptrArray.GetSize(); i++)
				pEnum->AddConnPoint((LPCONNECTIONPOINT)ptrArray.GetAt(i));
		}

		// walk the chain of connection maps
		const AFX_CONNECTIONMAP* pMap = pThis->GetConnectionMap();
		const AFX_CONNECTIONMAP_ENTRY* pEntry;

		while (pMap != NULL)
		{
			pEntry = pMap->pEntry;

			while (pEntry->piid != NULL)
			{
				pEnum->AddConnPoint(GetConnectionPtr(pThis, pEntry));
				++pEntry;
			}
#ifdef _AFXDLL
			pMap = (*pMap->pfnGetBaseMap)();
#else
			pMap = pMap->pBaseMap;
#endif
		}
	}
	CATCH (CException, e)
	{
		delete pEnum;
		pEnum = NULL;
	}
	END_CATCH

	if (pEnum != NULL)
	{
		// create and return the IEnumConnectionPoints object
		*ppEnum = (IEnumConnectionPoints*)&pEnum->m_xEnumVOID;
	}
	else
	{
		// no connection points: return NULL
		*ppEnum = NULL;
	}

	return (pEnum != NULL) ? S_OK : CONNECT_E_NOCONNECTION;
}

STDMETHODIMP COleConnPtContainer::FindConnectionPoint(
	REFIID iid, LPCONNECTIONPOINT* ppCP)
{
	METHOD_PROLOGUE_EX_(CCmdTarget, ConnPtContainer)
	ASSERT(ppCP != NULL);

	if ((*ppCP = pThis->GetConnectionHook(iid)) != NULL)
	{
		(*ppCP)->AddRef();
		return S_OK;
	}

	const AFX_CONNECTIONMAP* pMap = pThis->GetConnectionMap();
	const AFX_CONNECTIONMAP_ENTRY* pEntry;

	while (pMap != NULL)
	{
		pEntry = pMap->pEntry;

		while (pEntry->piid != NULL)
		{
			if (IsEqualIID(iid, *(IID*)(pEntry->piid)))
			{
				*ppCP = GetConnectionPtr(pThis, pEntry);
				(*ppCP)->AddRef();
				return S_OK;
			}
			++pEntry;
		}
#ifdef _AFXDLL
		pMap = (*pMap->pfnGetBaseMap)();
#else
		pMap = pMap->pBaseMap;
#endif
	}

	return E_NOINTERFACE;
}


/////////////////////////////////////////////////////////////////////////////
// Wiring CCmdTarget to COleConnPtContainer

// enable this object for OLE connections, called from derived class ctor
void CCmdTarget::EnableConnections()
{
	ASSERT(GetConnectionMap() != NULL);   // must have DECLARE_DISPATCH_MAP

	// construct an COleConnPtContainer instance just to get to the vtable
	COleConnPtContainer cpc;

	// vtable pointer should be already set to same or NULL
	ASSERT(m_xConnPtContainer.m_vtbl == NULL||
		*(DWORD*)&cpc == m_xConnPtContainer.m_vtbl);
	// verify that sizes match
	ASSERT(sizeof(m_xConnPtContainer) == sizeof(COleConnPtContainer));

	// copy the vtable (and other data) to make sure it is initialized
	m_xConnPtContainer.m_vtbl = *(DWORD*)&cpc;
	*(COleConnPtContainer*)&m_xConnPtContainer = cpc;
}

/////////////////////////////////////////////////////////////////////////////
// Force any extra compiler-generated code into AFX_INIT_SEG

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif
