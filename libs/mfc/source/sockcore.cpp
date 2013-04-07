
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
#include <stddef.h>

#ifdef AFX_SOCK_SEG
#pragma code_seg(AFX_SOCK_SEG)
#endif

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define new DEBUG_NEW

#define _afxSockThreadState AfxGetModuleThreadState()
#define _AFX_SOCK_THREAD_STATE AFX_MODULE_THREAD_STATE

#pragma comment(lib, "wsock32.lib")

/////////////////////////////////////////////////////////////////////////////
// socket state cleanup

_AFX_SOCK_STATE::~_AFX_SOCK_STATE()
{
	if (m_pfnSockTerm != NULL)
		m_pfnSockTerm();
}

/////////////////////////////////////////////////////////////////////////////
// sockets globals and implementation helpers

void AFXAPI AfxSocketTerm()
{
	_AFX_SOCK_STATE* pState = _afxSockState.GetData();
	if (pState->m_hInstSOCK != NULL)
	{
		if (pState->m_pfnSockTerm != NULL)
			WSACleanup();
		FreeLibrary(pState->m_hInstSOCK);// handle of WSOCK32.DLL
		pState->m_hInstSOCK = NULL;
	}
}

BOOL AFXAPI AfxSocketInit(WSADATA* lpwsaData)
{
	_AFX_SOCK_STATE* pState = _afxSockState.GetData();
	if (pState->m_pfnSockTerm == NULL)
	{
		// initialize Winsock library
		WSADATA wsaData;
		if (lpwsaData == NULL)
			lpwsaData = &wsaData;

		WORD wVersionRequested = MAKEWORD(1, 1);
		int nResult = WSAStartup(wVersionRequested, lpwsaData);
		if (nResult != 0)
			return FALSE;

		if (LOBYTE(lpwsaData->wVersion) != 1 || HIBYTE(lpwsaData->wVersion) != 1)
		{
			WSACleanup();
			return FALSE;
		}

		// setup for termination of sockets
		pState->m_pfnSockTerm = &AfxSocketTerm;

#ifndef _AFXDLL
		// setup maps and lists specific to socket state
		_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;
		if (pState->m_pmapSocketHandle == NULL)
			pState->m_pmapSocketHandle = new CMapPtrToPtr;
		if (pState->m_pmapDeadSockets == NULL)
			pState->m_pmapDeadSockets = new CMapPtrToPtr;
		if (pState->m_plistSocketNotifications == NULL)
			pState->m_plistSocketNotifications = new CPtrList;
#endif
	}

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncSocket Construction

CAsyncSocket::CAsyncSocket()
{
	m_hSocket = INVALID_SOCKET;
}

BOOL CAsyncSocket::Create(UINT nSocketPort, int nSocketType,
	long lEvent, LPCTSTR lpszSocketAddress)
{
	if (Socket(nSocketType, lEvent))
	{
		if (Bind(nSocketPort,lpszSocketAddress))
			return TRUE;
		int nResult = GetLastError();
		Close();
		WSASetLastError(nResult);
	}
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncSocket Attributes

BOOL CAsyncSocket::Attach(SOCKET hSocket, long lEvent)
{
	ASSERT(hSocket != INVALID_SOCKET);

	m_hSocket = hSocket;
	CAsyncSocket::AttachHandle(hSocket, this);

	return AsyncSelect(lEvent);
}

SOCKET CAsyncSocket::Detach()
{
	SOCKET hSocket = m_hSocket;
	if (AsyncSelect(0))
	{
		CAsyncSocket::KillSocket(hSocket, this);
		m_hSocket = INVALID_SOCKET;
		return hSocket;
	}
	return INVALID_SOCKET;
}

BOOL CAsyncSocket::GetPeerName(CString& rPeerAddress, UINT& rPeerPort)
{
	SOCKADDR_IN sockAddr;
	memset(&sockAddr, 0, sizeof(sockAddr));

	int nSockAddrLen = sizeof(sockAddr);
	BOOL bResult = GetPeerName((SOCKADDR*)&sockAddr, &nSockAddrLen);
	if (bResult)
	{
		rPeerPort = ntohs(sockAddr.sin_port);
		rPeerAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return bResult;
}

BOOL CAsyncSocket::GetSockName(CString& rSocketAddress, UINT& rSocketPort)
{
	SOCKADDR_IN sockAddr;
	memset(&sockAddr, 0, sizeof(sockAddr));

	int nSockAddrLen = sizeof(sockAddr);
	BOOL bResult = GetSockName((SOCKADDR*)&sockAddr, &nSockAddrLen);
	if (bResult)
	{
		rSocketPort = ntohs(sockAddr.sin_port);
		rSocketAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return bResult;
}

/////////////////////////////////////////////////////////////////////////////
// CAscynSocket Operations

BOOL CAsyncSocket::Accept(CAsyncSocket& rConnectedSocket,
	SOCKADDR* lpSockAddr, int* lpSockAddrLen)
{
	ASSERT(rConnectedSocket.m_hSocket == INVALID_SOCKET);
	ASSERT(CAsyncSocket::FromHandle(INVALID_SOCKET) == NULL);

	CAsyncSocket::AttachHandle(INVALID_SOCKET, &rConnectedSocket);

	SOCKET hTemp = accept(m_hSocket, lpSockAddr, lpSockAddrLen);

	if (hTemp == INVALID_SOCKET)
	{
		DWORD dwProblem = GetLastError();
		CAsyncSocket::DetachHandle(rConnectedSocket.m_hSocket, FALSE);
		rConnectedSocket.m_hSocket = INVALID_SOCKET;
		SetLastError(dwProblem);
	}
	else if (CAsyncSocket::FromHandle(INVALID_SOCKET) != NULL)
	{
		rConnectedSocket.m_hSocket = hTemp;
		CAsyncSocket::DetachHandle(INVALID_SOCKET, FALSE);
		CAsyncSocket::AttachHandle(hTemp, &rConnectedSocket);
	}

	return (hTemp != INVALID_SOCKET);
}

BOOL CAsyncSocket::Bind(UINT nSocketPort, LPCTSTR lpszSocketAddress)
{
	USES_CONVERSION;

	SOCKADDR_IN sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));

	LPSTR lpszAscii = T2A((LPTSTR)lpszSocketAddress);
	sockAddr.sin_family = AF_INET;

	if (lpszAscii == NULL)
		sockAddr.sin_addr.s_addr = htonl(INADDR_ANY);
	else
	{
		DWORD lResult = inet_addr(lpszAscii);
		if (lResult == INADDR_NONE)
		{
			WSASetLastError(WSAEINVAL);
			return FALSE;
		}
		sockAddr.sin_addr.s_addr = lResult;
	}

	sockAddr.sin_port = htons((u_short)nSocketPort);

	return Bind((SOCKADDR*)&sockAddr, sizeof(sockAddr));
}

void CAsyncSocket::Close()
{
	if (m_hSocket != INVALID_SOCKET)
	{
		VERIFY(SOCKET_ERROR != closesocket(m_hSocket));
		CAsyncSocket::KillSocket(m_hSocket, this);
		m_hSocket = INVALID_SOCKET;
	}
}

BOOL CAsyncSocket::Connect(LPCTSTR lpszHostAddress, UINT nHostPort)
{
	USES_CONVERSION;

	ASSERT(lpszHostAddress != NULL);

	SOCKADDR_IN sockAddr;
	memset(&sockAddr,0,sizeof(sockAddr));

	LPSTR lpszAscii = T2A((LPTSTR)lpszHostAddress);
	sockAddr.sin_family = AF_INET;
	sockAddr.sin_addr.s_addr = inet_addr(lpszAscii);

	if (sockAddr.sin_addr.s_addr == INADDR_NONE)
	{
		LPHOSTENT lphost;
		lphost = gethostbyname(lpszAscii);
		if (lphost != NULL)
			sockAddr.sin_addr.s_addr = ((LPIN_ADDR)lphost->h_addr)->s_addr;
		else
		{
			WSASetLastError(WSAEINVAL);
			return FALSE;
		}
	}

	sockAddr.sin_port = htons((u_short)nHostPort);

	return Connect((SOCKADDR*)&sockAddr, sizeof(sockAddr));
}

int CAsyncSocket::Receive(void* lpBuf, int nBufLen, int nFlags)
{
	return recv(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags);
}

int CAsyncSocket::ReceiveFrom(void* lpBuf, int nBufLen, CString& rSocketAddress, UINT& rSocketPort, int nFlags)
{
	SOCKADDR_IN sockAddr;

	memset(&sockAddr, 0, sizeof(sockAddr));

	int nSockAddrLen = sizeof(sockAddr);
	int nResult = ReceiveFrom(lpBuf, nBufLen, (SOCKADDR*)&sockAddr, &nSockAddrLen, nFlags);
	if(nResult != SOCKET_ERROR)
	{
		rSocketPort = ntohs(sockAddr.sin_port);
		rSocketAddress = inet_ntoa(sockAddr.sin_addr);
	}
	return nResult;
}

int CAsyncSocket::Send(const void* lpBuf, int nBufLen, int nFlags)
{
	return send(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags);
}

int CAsyncSocket::SendTo(const void* lpBuf, int nBufLen, UINT nHostPort, LPCTSTR lpszHostAddress, int nFlags)
{
	USES_CONVERSION;

	SOCKADDR_IN sockAddr;

	memset(&sockAddr,0,sizeof(sockAddr));

	LPSTR lpszAscii = T2A((LPTSTR)lpszHostAddress);
	sockAddr.sin_family = AF_INET;

	if (lpszAscii == NULL)
		sockAddr.sin_addr.s_addr = htonl(INADDR_BROADCAST);
	else
	{
		sockAddr.sin_addr.s_addr = inet_addr(lpszAscii);
		if (sockAddr.sin_addr.s_addr == INADDR_NONE)
		{
			LPHOSTENT lphost;
			lphost = gethostbyname(lpszAscii);
			if (lphost != NULL)
				sockAddr.sin_addr.s_addr = ((LPIN_ADDR)lphost->h_addr)->s_addr;
			else
			{
				WSASetLastError(WSAEINVAL);
				return SOCKET_ERROR;
			}
		}
	}

	sockAddr.sin_port = htons((u_short)nHostPort);

	return SendTo(lpBuf, nBufLen, (SOCKADDR*)&sockAddr, sizeof(sockAddr), nFlags);
}

BOOL CAsyncSocket::AsyncSelect(long lEvent)
{
	ASSERT(m_hSocket != INVALID_SOCKET);

	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;
	ASSERT(pState->m_hSocketWindow != NULL);

	return WSAAsyncSelect(m_hSocket, pState->m_hSocketWindow,
		WM_SOCKET_NOTIFY, lEvent) != SOCKET_ERROR;
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncSocket Overridable callbacks

void CAsyncSocket::OnReceive(int /*nErrorCode*/)
{
}

void CAsyncSocket::OnSend(int /*nErrorCode*/)
{
}

void CAsyncSocket::OnOutOfBandData(int /*nErrorCode*/)
{
}

void CAsyncSocket::OnAccept(int /*nErrorCode*/)
{
}

void CAsyncSocket::OnConnect(int /*nErrorCode*/)
{
}

void CAsyncSocket::OnClose(int /*nErrorCode*/)
{
}

/////////////////////////////////////////////////////////////////////////////
// CAsyncSocket Implementation

CAsyncSocket::~CAsyncSocket()
{
	if (m_hSocket != INVALID_SOCKET)
		Close();
}

CAsyncSocket* PASCAL CAsyncSocket::LookupHandle(SOCKET hSocket, BOOL bDead)
{
	CAsyncSocket* pSocket;
	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;
	if (!bDead)
	{
		pSocket = (CAsyncSocket*)
			pState->m_pmapSocketHandle->GetValueAt((void*)hSocket);
		if (pSocket != NULL)
			return pSocket;
	}
	else
	{
		pSocket = (CAsyncSocket*)
			pState->m_pmapDeadSockets->GetValueAt((void*)hSocket);
		if (pSocket != NULL)
			return pSocket;
	}
	return NULL;
}

void PASCAL CAsyncSocket::AttachHandle(
	SOCKET hSocket, CAsyncSocket* pSocket, BOOL bDead)
{
	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;

	BOOL bEnable = AfxEnableMemoryTracking(FALSE);
	if (!bDead)
	{
		ASSERT(CAsyncSocket::LookupHandle(hSocket, bDead) == NULL);
		if (pState->m_pmapSocketHandle->IsEmpty())
		{
			ASSERT(pState->m_pmapDeadSockets->IsEmpty());
			ASSERT(pState->m_hSocketWindow == NULL);

			CSocketWnd* pWnd = new CSocketWnd;
			pWnd->m_hWnd = NULL;
			if (!pWnd->CreateEx(0, AfxRegisterWndClass(0),
				_T("Socket Notification Sink"),
				WS_OVERLAPPED, 0, 0, 0, 0, NULL, NULL))
			{
				TRACE0("Warning: unable to create socket notify window!\n");
				AfxThrowResourceException();
			}
			ASSERT(pWnd->m_hWnd != NULL);
			ASSERT(CWnd::FromHandlePermanent(pWnd->m_hWnd) == pWnd);
			pState->m_hSocketWindow = pWnd->m_hWnd;
		}
		pState->m_pmapSocketHandle->SetAt((void*)hSocket, pSocket);
	}
	else
	{
		int nCount;
		if (pState->m_pmapDeadSockets->Lookup((void*)hSocket, (void*&)nCount))
			nCount++;
		else
			nCount = 1;
		pState->m_pmapDeadSockets->SetAt((void*)hSocket, (void*)nCount);
	}
	AfxEnableMemoryTracking(bEnable);
}

void PASCAL CAsyncSocket::DetachHandle(SOCKET hSocket, BOOL bDead)
{
	ASSERT(CAsyncSocket::LookupHandle(hSocket, bDead) != NULL);

	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;
	if (!bDead)
	{
		pState->m_pmapSocketHandle->RemoveKey((void*)hSocket);
		if (pState->m_pmapSocketHandle->IsEmpty())
		{
			ASSERT(pState->m_hSocketWindow != NULL);
			CWnd* pWnd =
				CWnd::FromHandlePermanent(pState->m_hSocketWindow);
			ASSERT_VALID(pWnd);

			pWnd->DestroyWindow();
			delete pWnd;

			pState->m_hSocketWindow = NULL;

			pState->m_pmapDeadSockets->RemoveAll();

			while (!pState->m_plistSocketNotifications->IsEmpty())
				delete pState->m_plistSocketNotifications->RemoveHead();
		}
	}
	else
	{
		int nCount;
		if (pState->m_pmapDeadSockets->Lookup((void*)hSocket, (void*&)nCount))
		{
			nCount--;
			if (nCount == 0)
				pState->m_pmapDeadSockets->RemoveKey((void*)hSocket);
			else
				pState->m_pmapDeadSockets->SetAt((void*)hSocket, (void*)nCount);
		}
	}
}

void PASCAL CAsyncSocket::KillSocket(SOCKET hSocket, CAsyncSocket* pSocket)
{
	ASSERT(CAsyncSocket::LookupHandle(hSocket, FALSE) != NULL);

	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;

	CAsyncSocket::DetachHandle(hSocket, FALSE);
	if (pState->m_hSocketWindow != NULL)
	{
		::PostMessage(pState->m_hSocketWindow, WM_SOCKET_DEAD,
			(WPARAM)hSocket, 0L);
		CAsyncSocket::AttachHandle(hSocket, pSocket, TRUE);
	}
}

void PASCAL CAsyncSocket::DoCallBack(WPARAM wParam, LPARAM lParam)
{
	if (wParam == 0 && lParam == 0)
		return;

	// Has the socket be closed?
	CAsyncSocket* pSocket = CAsyncSocket::LookupHandle((SOCKET)wParam, TRUE);

	// If yes ignore message
	if (pSocket != NULL)
		return;

	pSocket = CAsyncSocket::LookupHandle((SOCKET)wParam, FALSE);
	if (pSocket == NULL)
	{
		// Must be in the middle of an Accept call
		pSocket = CAsyncSocket::LookupHandle(INVALID_SOCKET, FALSE);
		ASSERT(pSocket != NULL);
		pSocket->m_hSocket = (SOCKET)wParam;
		CAsyncSocket::DetachHandle(INVALID_SOCKET, FALSE);
		CAsyncSocket::AttachHandle(pSocket->m_hSocket, pSocket, FALSE);
	}

	int nErrorCode = WSAGETSELECTERROR(lParam);
	switch (WSAGETSELECTEVENT(lParam))
	{
	case FD_READ:
		{
			DWORD nBytes;
			if (!pSocket->IOCtl(FIONREAD, &nBytes))
				nErrorCode = WSAGetLastError();
			if (nBytes != 0 || nErrorCode != 0)
				pSocket->OnReceive(nErrorCode);
		}
		break;
	case FD_WRITE:
		pSocket->OnSend(nErrorCode);
		break;
	case FD_OOB:
		pSocket->OnOutOfBandData(nErrorCode);
		break;
	case FD_ACCEPT:
		pSocket->OnAccept(nErrorCode);
		break;
	case FD_CONNECT:
		pSocket->OnConnect(nErrorCode);
		break;
	case FD_CLOSE:
		pSocket->OnClose(nErrorCode);
		break;
	}
}

BOOL CAsyncSocket::Socket(int nSocketType, long lEvent,
	int nProtocolType, int nAddressFormat)
{
	ASSERT(m_hSocket == INVALID_SOCKET);

	m_hSocket = socket(nAddressFormat,nSocketType,nProtocolType);
	if (m_hSocket != INVALID_SOCKET)
	{
		CAsyncSocket::AttachHandle(m_hSocket, this, FALSE);
		return AsyncSelect(lEvent);
	}
	return FALSE;
}

#ifdef _DEBUG
void CAsyncSocket::AssertValid() const
{
	CObject::AssertValid();
	ASSERT(m_hSocket == INVALID_SOCKET || CAsyncSocket::FromHandle(m_hSocket) != NULL);
}

void CAsyncSocket::Dump(CDumpContext& dc) const
{
	CObject::Dump(dc);

	dc << "m_hSocket = ";
	if (m_hSocket == INVALID_SOCKET)
		dc << "INVALID_SOCKET\n";
	else
		dc << m_hSocket << "\n";
}
#endif //_DEBUG

int CAsyncSocket::ReceiveFromHelper(void* lpBuf, int nBufLen, SOCKADDR* lpSockAddr, int* lpSockAddrLen, int nFlags)
{
	return recvfrom(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags, lpSockAddr, lpSockAddrLen);
}

int CAsyncSocket::SendToHelper(const void* lpBuf, int nBufLen, const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags)
{
	return sendto(m_hSocket, (LPSTR)lpBuf, nBufLen, nFlags, lpSockAddr, nSockAddrLen);
}

BOOL CAsyncSocket::ConnectHelper(const SOCKADDR* lpSockAddr, int nSockAddrLen)
{
	return connect(m_hSocket, lpSockAddr, nSockAddrLen) != SOCKET_ERROR;
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Construction

CSocket::CSocket()
{
	m_pbBlocking = NULL;
	m_nConnectError = -1;
	m_nTimeOut = 2000;
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Operations

void CSocket::CancelBlockingCall()
{
	if (m_pbBlocking != NULL)
	{
		*m_pbBlocking = FALSE;
		m_pbBlocking = NULL;
	}
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Overridable callbacks

BOOL CSocket::OnMessagePending()
{
	MSG msg;
	if (::PeekMessage(&msg, NULL, WM_PAINT, WM_PAINT, PM_REMOVE))
	{
		::DispatchMessage(&msg);
		return FALSE;   // usually return TRUE, but OnIdle usually causes WM_PAINTs
	}
	return FALSE;
}

/////////////////////////////////////////////////////////////////////////////
// CSocket Implementation

CSocket::~CSocket()
{
	if (m_hSocket != INVALID_SOCKET)
		Close();
}

BOOL CSocket::Accept(CAsyncSocket& rConnectedSocket, SOCKADDR* lpSockAddr, int* lpSockAddrLen)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return FALSE;
	}
	while (!CAsyncSocket::Accept(rConnectedSocket, lpSockAddr, lpSockAddrLen))
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			if (!PumpMessages(FD_ACCEPT))
				return FALSE;
		}
		else
			return FALSE;
	}
	return TRUE;
}

void CSocket::Close()
{
	if (m_hSocket != INVALID_SOCKET)
	{
		CancelBlockingCall();

		VERIFY(AsyncSelect(0));
		CAsyncSocket::Close();
		m_hSocket = INVALID_SOCKET;
	}
}

int CSocket::Receive(void* lpBuf, int nBufLen, int nFlags)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return  FALSE;
	}
	int nResult;
	while ((nResult = CAsyncSocket::Receive(lpBuf, nBufLen, nFlags)) == SOCKET_ERROR)
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			if (!PumpMessages(FD_READ))
				return SOCKET_ERROR;
		}
		else
			return SOCKET_ERROR;
	}
	return nResult;
}

int CSocket::Send(const void* lpBuf, int nBufLen, int nFlags)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return  FALSE;
	}

	int nLeft, nWritten;
	PBYTE pBuf = (PBYTE)lpBuf;
	nLeft = nBufLen;

	while (nLeft > 0)
	{
		nWritten = SendChunk(pBuf, nLeft, nFlags);
		if (nWritten == SOCKET_ERROR)
			return nWritten;

		nLeft -= nWritten;
		pBuf += nWritten;
	}
	return nBufLen - nLeft;
}

int CSocket::SendChunk(const void* lpBuf, int nBufLen, int nFlags)
{
	int nResult;
	while ((nResult = CAsyncSocket::Send(lpBuf, nBufLen, nFlags)) == SOCKET_ERROR)
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			if (!PumpMessages(FD_WRITE))
				return SOCKET_ERROR;
		}
		else
			return SOCKET_ERROR;
	}
	return nResult;
}

BOOL CSocket::ConnectHelper(const SOCKADDR* lpSockAddr, int nSockAddrLen)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return  FALSE;
	}

	m_nConnectError = -1;

	if (!CAsyncSocket::ConnectHelper(lpSockAddr, nSockAddrLen))
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			while (PumpMessages(FD_CONNECT))
			{
				if (m_nConnectError != -1)
				{
					WSASetLastError(m_nConnectError);
					return (m_nConnectError == 0);
				}
			}
		}
		return FALSE;
	}
	return TRUE;
}

int CSocket::ReceiveFromHelper(void* lpBuf, int nBufLen, SOCKADDR* lpSockAddr, int* lpSockAddrLen, int nFlags)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return  FALSE;
	}
	int nResult;
	while ((nResult = CAsyncSocket::ReceiveFromHelper(lpBuf, nBufLen, lpSockAddr, lpSockAddrLen, nFlags)) == SOCKET_ERROR)
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			if (!PumpMessages(FD_READ))
				return SOCKET_ERROR;
		}
		else
			return SOCKET_ERROR;
	}
	return nResult;
}

int CSocket::SendToHelper(const void* lpBuf, int nBufLen, const SOCKADDR* lpSockAddr, int nSockAddrLen, int nFlags)
{
	if (m_pbBlocking != NULL)
	{
		WSASetLastError(WSAEINPROGRESS);
		return SOCKET_ERROR;
	}
	int nResult;
	while ((nResult = CAsyncSocket::SendToHelper(lpBuf, nBufLen, lpSockAddr, nSockAddrLen, nFlags)) == SOCKET_ERROR)
	{
		if (GetLastError() == WSAEWOULDBLOCK)
		{
			if (!PumpMessages(FD_WRITE))
				return SOCKET_ERROR;
		}
		else
			return SOCKET_ERROR;
	}
	return nResult;
}

int PASCAL CSocket::ProcessAuxQueue()
{
	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;

	if (pState->m_plistSocketNotifications->IsEmpty())
		return 0;

	int nCount = 0;
	while(!pState->m_plistSocketNotifications->IsEmpty())
	{
		nCount++;

		MSG* pMsg = (MSG*)pState->m_plistSocketNotifications->RemoveHead();
		ASSERT(pMsg != NULL);
		if (pMsg->message == WM_SOCKET_NOTIFY)
		{
			CAsyncSocket::DoCallBack(pMsg->wParam, pMsg->lParam);
		}
		else
		{
			ASSERT(CAsyncSocket::LookupHandle((SOCKET)pMsg->wParam, TRUE) != NULL);
			CAsyncSocket::DetachHandle((SOCKET)pMsg->wParam, TRUE);
		}
		delete pMsg;
	}
	return nCount;
}

void PASCAL CSocket::AuxQueueAdd(UINT message, WPARAM wParam, LPARAM lParam)
{
	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;

	MSG* pMsg = new MSG;
	pMsg->message = message;
	pMsg->wParam = wParam;
	pMsg->lParam = lParam;
	pState->m_plistSocketNotifications->AddTail(pMsg);
}

BOOL CSocket::PumpMessages(UINT uStopFlag)
{
	// The same socket better not be blocking in more than one place.
	ASSERT(m_pbBlocking == NULL);

	_AFX_SOCK_THREAD_STATE* pState = _afxSockThreadState;

	ASSERT(pState->m_hSocketWindow != NULL);

	BOOL bBlocking = TRUE;
	m_pbBlocking = &bBlocking;
	CWinThread* pThread = AfxGetThread();

	// This is not a timeout in the WinSock sense, but more
	// like a WM_KICKIDLE to keep message pumping alive
	UINT nTimerID = ::SetTimer(pState->m_hSocketWindow, 1, m_nTimeOut, NULL);

	if (nTimerID == 0)
		AfxThrowResourceException();

	BOOL bPeek = TRUE;

	while (bBlocking)
	{
		TRY
		{
			MSG msg;
			if (::PeekMessage(&msg, pState->m_hSocketWindow,
				WM_SOCKET_NOTIFY, WM_SOCKET_DEAD, PM_REMOVE))
			{
				if (msg.message == WM_SOCKET_NOTIFY && (SOCKET)msg.wParam == m_hSocket)
				{
					if (WSAGETSELECTEVENT(msg.lParam) == FD_CLOSE)
					{
						break;
					}
					if (WSAGETSELECTEVENT(msg.lParam) == uStopFlag)
					{
						if (uStopFlag == FD_CONNECT)
							m_nConnectError = WSAGETSELECTERROR(msg.lParam);
						break;
					}
				}
				if (msg.wParam != 0 || msg.lParam != 0)
					CSocket::AuxQueueAdd(msg.message, msg.wParam, msg.lParam);

				bPeek = TRUE;
			}
			else if (::PeekMessage(&msg, pState->m_hSocketWindow,
						WM_TIMER, WM_TIMER, PM_REMOVE))
			{
			break;
			}

			if (bPeek && ::PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE))
			{
				if (OnMessagePending())
				{
					// allow user-interface updates
					pThread->OnIdle(-1);
				}
				else
				{
					bPeek = FALSE;
				}
			}
			else
			{
				// no work to do -- allow CPU to sleep
				WaitMessage();
				bPeek = TRUE;
			}
		}
		CATCH_ALL(e)
		{
			TRACE0("Error: caught exception in PumpMessage - continuing.\n");
			DELETE_EXCEPTION(e);
			bPeek = TRUE;
		}
		END_CATCH_ALL
	}

	::KillTimer(pState->m_hSocketWindow, nTimerID);

	if (!bBlocking)
	{
		WSASetLastError(WSAEINTR);
		return FALSE;
	}
	m_pbBlocking = NULL;

	::PostMessage(pState->m_hSocketWindow, WM_SOCKET_NOTIFY, 0, 0);

	return TRUE;
}

#ifdef _DEBUG
void CSocket::AssertValid() const
{
	CAsyncSocket::AssertValid();
}

void CSocket::Dump(CDumpContext& dc) const
{
	CAsyncSocket::Dump(dc);
	dc << "m_pbBlocking = " << m_pbBlocking <<"\n";
	dc << "m_nConnectError = " << m_nConnectError <<"\n";
}
#endif //_DEBUG


/////////////////////////////////////////////////////////////////////////////
// CSocketFile Construction

CSocketFile::CSocketFile(CSocket* pSocket, BOOL bArchiveCompatible)
{
	m_pSocket = pSocket;
	m_bArchiveCompatible = bArchiveCompatible;

#ifdef _DEBUG
	ASSERT(m_pSocket != NULL);
	ASSERT(m_pSocket->m_hSocket != INVALID_SOCKET);

	int nType = 0;
	int nTypeLen = sizeof(int);
	ASSERT(m_pSocket->GetSockOpt(SO_TYPE,&nType,&nTypeLen));
	ASSERT(nType == SOCK_STREAM);
#endif // _DEBUG
}

/////////////////////////////////////////////////////////////////////////////
// CSocketFile Implementation

CSocketFile::~CSocketFile()
{
}

UINT CSocketFile::Read(void* lpBuf, UINT nCount)
{
	ASSERT(m_pSocket != NULL);

	int nRead;

	if (!m_bArchiveCompatible)
	{
		int nLeft = nCount;
		PBYTE pBuf = (PBYTE)lpBuf;

		while(nLeft > 0)
		{
			nRead = m_pSocket->Receive(pBuf, nLeft);
			if (nRead == SOCKET_ERROR)
			{
				int nError = m_pSocket->GetLastError();
				AfxThrowFileException(CFileException::generic, nError);
				ASSERT(FALSE);
			}
			else if (nRead == 0)
			{
				return nCount - nLeft;
			}

			nLeft -= nRead;
			pBuf += nRead;
		}
		return nCount - nLeft;
	}

	nRead = m_pSocket->Receive(lpBuf, nCount, 0);
	if (nRead == SOCKET_ERROR)
	{
		int nError = m_pSocket->GetLastError();
		AfxThrowFileException(CFileException::generic, nError);
		ASSERT(FALSE);
	}
	return nRead;
}

void CSocketFile::Write(const void* lpBuf, UINT nCount)
{
	ASSERT (m_pSocket!=NULL);

	int nWritten = m_pSocket->Send(lpBuf, nCount);
	if (nWritten == SOCKET_ERROR)
	{
		int nError = m_pSocket->GetLastError();
		AfxThrowFileException(CFileException::generic, nError);
	}
}

void CSocketFile::Close()
{
	m_pSocket = NULL;
}

BOOL CSocketFile::Open(
	LPCTSTR /*lpszFileName*/, UINT /*nOpenFlags*/, CFileException* /*pError*/)
{
	AfxThrowNotSupportedException();
	return FALSE;
}

CFile* CSocketFile::Duplicate() const
{
	AfxThrowNotSupportedException();
	return NULL;
}

DWORD CSocketFile::GetPosition() const
{
	AfxThrowNotSupportedException();
	return 0;
}

LONG CSocketFile::Seek(LONG lOff, UINT nFrom)
{
	if (lOff != 0L || nFrom != current)
		TRACE0("Warning - Attempt made to seek on a CSocketFile\n");
	return 0;
}

void CSocketFile::SetLength(DWORD /*dwNewLen*/)
{
	AfxThrowNotSupportedException();
}

DWORD CSocketFile::GetLength() const
{
	AfxThrowNotSupportedException();
	return 0;
}

void CSocketFile::LockRange(DWORD /*dwPos*/, DWORD /*dwCount*/)
{
	AfxThrowNotSupportedException();
}

void CSocketFile::UnlockRange(DWORD /*dwPos*/, DWORD /*dwCount*/)
{
	AfxThrowNotSupportedException();
}

void CSocketFile::Flush()
{
}

void CSocketFile::Abort()
{
	AfxThrowNotSupportedException();
}

#ifdef _DEBUG
void CSocketFile::AssertValid() const
{
	CFile::AssertValid();
	if (m_pSocket != NULL)
		ASSERT_VALID(m_pSocket);
}

void CSocketFile::Dump(CDumpContext& dc) const
{
	CFile::Dump(dc);
	if (dc.GetDepth() > 0)
	{
		if (m_pSocket != NULL)
			dc << "with no socket\n";
		else
			dc << "with socket: " << m_pSocket;
	}
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CSocketWnd implementation

CSocketWnd::CSocketWnd()
{
}

LRESULT CSocketWnd::OnSocketNotify(WPARAM wParam, LPARAM lParam)
{
	CSocket::AuxQueueAdd(WM_SOCKET_NOTIFY, wParam, lParam);
	CSocket::ProcessAuxQueue();
	return 0L;
}

LRESULT CSocketWnd::OnSocketDead(WPARAM wParam, LPARAM lParam)
{
	CSocket::AuxQueueAdd(WM_SOCKET_DEAD, wParam, lParam);
	CSocket::ProcessAuxQueue();
	return 0L;
}

/////////////////////////////////////////////////////////////////////////////
// Message table implementation

BEGIN_MESSAGE_MAP(CSocketWnd, CWnd)
	//{{AFX_MSG_MAP(CWnd)
	ON_MESSAGE(WM_SOCKET_NOTIFY, OnSocketNotify)
	ON_MESSAGE(WM_SOCKET_DEAD, OnSocketDead)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

//////////////////////////////////////////////////////////////////////////////
// Inline function declarations expanded out-of-line

#ifndef _AFX_ENABLE_INLINES

static char _szAfxSockInl[] = "afxsock.inl";
#undef THIS_FILE
#define THIS_FILE _szAfxSockInl
#define _AFXSOCK_INLINE
#include "afxsock.inl"
#undef _AFXSOCK_INLINE

#endif

#ifdef AFX_INIT_SEG
#pragma code_seg(AFX_INIT_SEG)
#endif

IMPLEMENT_DYNAMIC(CAsyncSocket, CObject)
IMPLEMENT_DYNAMIC(CSocket, CAsyncSocket)
IMPLEMENT_DYNAMIC(CSocketFile, CFile)

#pragma warning(disable: 4074)
#pragma init_seg(lib)

PROCESS_LOCAL(_AFX_SOCK_STATE, _afxSockState)

/////////////////////////////////////////////////////////////////////////////
