// CAsyncSocketEx by Tim Kosse (Tim.Kosse@gmx.de)
//            Version 1.1 (2002-11-01)
//---------------------------------------------------------------------------
// Feel free to use this class, as long as you don't claim that you wrote it
// and this copyright notice stays intact in the source files.
// If you use this class in commercial applications, please send a short message
// to tim.kosse@gmx.de
//---------------------------------------------------------------------------
#include "stdafx.h"
#include "AsyncSocketExLayer.h"

#include "AsyncSocketEx.h"

#define WM_SOCKETEX_NOTIFY (WM_USER+3)


//////////////////////////////////////////////////////////////////////
// Konstruktion/Destruktion
//////////////////////////////////////////////////////////////////////

CAsyncSocketExLayer::CAsyncSocketExLayer()
{
  m_pOwnerSocket = NULL;
  m_pNextLayer = NULL;
  m_pPrevLayer = NULL;

  m_nLayerState = notsock;
  m_nCriticalError=0;

  m_nPendingEvents = 0;

  m_nFamily = AF_UNSPEC;
  m_lEvent = 0;
  m_lpszSocketAddress = 0;
  m_nSocketPort = 0;

  m_nextAddr = 0;
  m_addrInfo = 0;
}

CAsyncSocketExLayer::~CAsyncSocketExLayer()
{
  delete [] m_lpszSocketAddress;
}

CAsyncSocketExLayer *CAsyncSocketExLayer::AddLayer(CAsyncSocketExLayer *pLayer, CAsyncSocketEx *pOwnerSocket)
{
  DebugAssert(pLayer);
  DebugAssert(pOwnerSocket);
  if (m_pNextLayer)
  {
    return m_pNextLayer->AddLayer(pLayer, pOwnerSocket);
  }
  else
  {
    DebugAssert(m_pOwnerSocket==pOwnerSocket);
    pLayer->Init(this, m_pOwnerSocket);
    m_pNextLayer=pLayer;
  }
  return m_pNextLayer;
}

int CAsyncSocketExLayer::Receive(void* lpBuf, int nBufLen, int nFlags /*=0*/)
{
  return ReceiveNext(lpBuf, nBufLen, nFlags);
}

int CAsyncSocketExLayer::Send(const void* lpBuf, int nBufLen, int nFlags /*=0*/)
{
  return SendNext(lpBuf, nBufLen, nFlags);
}


void CAsyncSocketExLayer::OnReceive(int nErrorCode)
{
  if (m_pPrevLayer)
  {
    m_pPrevLayer->OnReceive(nErrorCode);
  }
  else
  {
    if (m_pOwnerSocket->m_lEvent&FD_READ)
    {
      m_pOwnerSocket->OnReceive(nErrorCode);
    }
  }
}

void CAsyncSocketExLayer::OnSend(int nErrorCode)
{
  if (m_pPrevLayer)
  {
    m_pPrevLayer->OnSend(nErrorCode);
  }
  else
  {
    if (m_pOwnerSocket->m_lEvent&FD_WRITE)
    {
      m_pOwnerSocket->OnSend(nErrorCode);
    }
  }
}

void CAsyncSocketExLayer::OnConnect(int nErrorCode)
{
  TriggerEvent(FD_CONNECT, nErrorCode, TRUE);
}

void CAsyncSocketExLayer::OnAccept(int nErrorCode)
{
  if (m_pPrevLayer)
    m_pPrevLayer->OnAccept(nErrorCode);
  else
    if (m_pOwnerSocket->m_lEvent&FD_ACCEPT)
      m_pOwnerSocket->OnAccept(nErrorCode);
}

void CAsyncSocketExLayer::OnClose(int nErrorCode)
{
  if (m_pPrevLayer)
    m_pPrevLayer->OnClose(nErrorCode);
  else
    if (m_pOwnerSocket->m_lEvent&FD_CLOSE)
      m_pOwnerSocket->OnClose(nErrorCode);
}

BOOL CAsyncSocketExLayer::TriggerEvent(long lEvent, int nErrorCode, BOOL bPassThrough /*=FALSE*/ )
{
  DebugAssert(m_pOwnerSocket);
  if (m_pOwnerSocket->m_SocketData.hSocket==INVALID_SOCKET)
  {
    return FALSE;
  }

  if (!bPassThrough)
  {
    if (m_nPendingEvents & lEvent)
    {
      return TRUE;
    }

    m_nPendingEvents |= lEvent;
  }

  if (lEvent & FD_CONNECT)
  {
    DebugAssert(bPassThrough);
    if (!nErrorCode)
    {
      DebugAssert(bPassThrough && (GetLayerState()==connected || GetLayerState()==attached));
    }
    else if (nErrorCode)
    {
      SetLayerState(aborted);
      m_nCriticalError=nErrorCode;
    }
  }
  else if (lEvent & FD_CLOSE)
  {
    if (!nErrorCode)
    {
      SetLayerState(closed);
    }
    else
    {
      SetLayerState(aborted);
      m_nCriticalError = nErrorCode;
    }
  }
  DebugAssert(m_pOwnerSocket->m_pLocalAsyncSocketExThreadData);
  DebugAssert(m_pOwnerSocket->m_pLocalAsyncSocketExThreadData->m_pHelperWindow);
  DebugAssert(m_pOwnerSocket->m_SocketData.nSocketIndex!=-1);
  t_LayerNotifyMsg *pMsg=new t_LayerNotifyMsg;
  pMsg->hSocket = m_pOwnerSocket->m_SocketData.hSocket;
  pMsg->lEvent = ( lEvent % 0xffff ) + ( nErrorCode << 16);
  pMsg->pLayer=bPassThrough?m_pPrevLayer:this;
  BOOL res=PostMessage(m_pOwnerSocket->GetHelperWindowHandle(), WM_USER, (WPARAM)m_pOwnerSocket->m_SocketData.nSocketIndex, (LPARAM)pMsg);
  if (!res)
  {
    delete pMsg;
  }
  return res;
}

void CAsyncSocketExLayer::Close()
{
  CloseNext();
}

void CAsyncSocketExLayer::CloseNext()
{
  if (m_addrInfo)
    freeaddrinfo(m_addrInfo);
  m_nextAddr = 0;
  m_addrInfo = 0;

  m_nPendingEvents = 0;

  SetLayerState(notsock);
  if (m_pNextLayer)
    m_pNextLayer->Close();
}

BOOL CAsyncSocketExLayer::Connect(LPCTSTR lpszHostAddress, UINT nHostPort)
{
  return ConnectNext(lpszHostAddress, nHostPort);
}

BOOL CAsyncSocketExLayer::Connect( const SOCKADDR* lpSockAddr, int nSockAddrLen )
{
  return ConnectNext(lpSockAddr, nSockAddrLen);
}

int CAsyncSocketExLayer::SendNext(const void *lpBuf, int nBufLen, int nFlags /*=0*/)
{
  if (m_nCriticalError)
  {
    WSASetLastError(m_nCriticalError);
    return SOCKET_ERROR;
  }
  else if (GetLayerState()==notsock)
  {
    WSASetLastError(WSAENOTSOCK);
    return SOCKET_ERROR;
  }
  else if (GetLayerState()==unconnected || GetLayerState()==connecting || GetLayerState()==listening)
  {
    WSASetLastError(WSAENOTCONN);
    return SOCKET_ERROR;
  }

  if (!m_pNextLayer)
  {
    DebugAssert(m_pOwnerSocket);
    int sent = send(m_pOwnerSocket->GetSocketHandle(), (LPSTR)lpBuf, nBufLen, nFlags);
    return sent;
  }
  else
  {
    return m_pNextLayer->Send(lpBuf, nBufLen, nFlags);
  }
}

int CAsyncSocketExLayer::ReceiveNext(void *lpBuf, int nBufLen, int nFlags /*=0*/)
{
  if (m_nCriticalError)
  {
    WSASetLastError(m_nCriticalError);
    return SOCKET_ERROR;
  }
  else if (GetLayerState()==notsock)
  {
    WSASetLastError(WSAENOTSOCK);
    return SOCKET_ERROR;
  }
  else if (GetLayerState()==unconnected || GetLayerState()==connecting || GetLayerState()==listening)
  {
    WSASetLastError(WSAENOTCONN);
    return SOCKET_ERROR;
  }

  if (!m_pNextLayer)
  {
    DebugAssert(m_pOwnerSocket);
    return recv(m_pOwnerSocket->GetSocketHandle(), (LPSTR)lpBuf, nBufLen, nFlags);
  }
  else
  {
    return m_pNextLayer->Receive(lpBuf, nBufLen, nFlags);
  }
}

BOOL CAsyncSocketExLayer::ConnectNext(LPCTSTR lpszHostAddress, UINT nHostPort)
{
  DebugAssert(GetLayerState()==unconnected);
  DebugAssert(m_pOwnerSocket);
  BOOL res = FALSE;
  if (m_pNextLayer)
    res = m_pNextLayer->Connect(lpszHostAddress, nHostPort);
  else if (m_nFamily == AF_INET)
  {
    USES_CONVERSION;

    DebugAssert(lpszHostAddress != NULL);

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
        res = FALSE;
      }
    }

    sockAddr.sin_port = htons((u_short)nHostPort);

    res = (SOCKET_ERROR != connect(m_pOwnerSocket->GetSocketHandle(), (SOCKADDR*)&sockAddr, sizeof(sockAddr)) );
  }
  else if (m_nFamily == AF_INET6 || m_nFamily == AF_UNSPEC)
  {
    USES_CONVERSION;

    DebugAssert(lpszHostAddress != NULL);

    addrinfo hints, *res0, *res1;
    SOCKET hSocket;
    int error;
    char port[10];

    freeaddrinfo(m_addrInfo);
    m_nextAddr = 0;
    m_addrInfo = 0;

    memset(&hints, 0, sizeof(addrinfo));
    hints.ai_family = m_nFamily;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = 0;
    _snprintf(port, 9, "%u", nHostPort);
    error = getaddrinfo(T2CA(lpszHostAddress), port, &hints, &res0);
    if (error)
      return FALSE;

    for (res1 = res0; res1; res1 = res1->ai_next)
    {
      if (m_nFamily == AF_UNSPEC)
        hSocket = socket(res1->ai_family, res1->ai_socktype, res1->ai_protocol);
      else
        hSocket = m_pOwnerSocket->GetSocketHandle();

      if (INVALID_SOCKET == hSocket)
      {
        res = FALSE;
        continue;
      }

      if (m_nFamily == AF_UNSPEC)
      {
        m_pOwnerSocket->m_SocketData.hSocket = hSocket;
        m_pOwnerSocket->AttachHandle(hSocket);
        if (!m_pOwnerSocket->AsyncSelect(m_lEvent))
        {
          m_pOwnerSocket->Close();
          res = FALSE;
          continue ;
        }
        if (m_pOwnerSocket->m_pFirstLayer)
        {
          if (WSAAsyncSelect(m_pOwnerSocket->m_SocketData.hSocket, m_pOwnerSocket->GetHelperWindowHandle(), m_pOwnerSocket->m_SocketData.nSocketIndex+WM_SOCKETEX_NOTIFY, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE) )
          {
            m_pOwnerSocket->Close();
            res = FALSE;
            continue;
          }
        }
        if (m_pOwnerSocket->m_pendingCallbacks.size())
          PostMessage(m_pOwnerSocket->GetHelperWindowHandle(), WM_USER + 2, (WPARAM)m_pOwnerSocket->m_SocketData.nSocketIndex, 0);
      }

      if (m_nFamily == AF_UNSPEC)
      {
        m_pOwnerSocket->m_SocketData.nFamily = m_nFamily = res1->ai_family;
        if (!m_pOwnerSocket->Bind(m_nSocketPort, m_lpszSocketAddress))
        {
          m_pOwnerSocket->m_SocketData.nFamily = m_nFamily = AF_UNSPEC;
          Close();
          continue;
        }
      }

      if (!( res = ( SOCKET_ERROR != connect(m_pOwnerSocket->GetSocketHandle(), res1->ai_addr, res1->ai_addrlen) ) )
        && WSAGetLastError() != WSAEWOULDBLOCK)
      {
        if (hints.ai_family == AF_UNSPEC)
        {
          m_nFamily = AF_UNSPEC;
          Close();
        }
        continue ;
      }

      m_nFamily = res1->ai_family;
      m_pOwnerSocket->m_SocketData.nFamily = res1->ai_family;
      res = TRUE;
      break;
    }

    if (res1)
      res1 = res0->ai_next;

    if (res1)
    {
      m_addrInfo = res0;
      m_nextAddr = res1;
    }
    else
      freeaddrinfo(res0);

    if (INVALID_SOCKET == m_pOwnerSocket->GetSocketHandle())
      res = FALSE ;
  }

  if (res || WSAGetLastError() == WSAEWOULDBLOCK)
  {
    SetLayerState(connecting);
  }
  return res;
}

BOOL CAsyncSocketExLayer::ConnectNext( const SOCKADDR* lpSockAddr, int nSockAddrLen )
{
  DebugAssert(GetLayerState()==unconnected);
  DebugAssert(m_pOwnerSocket);
  BOOL res;
  if (m_pNextLayer)
    res=m_pNextLayer->Connect(lpSockAddr, nSockAddrLen);
  else
    res = (SOCKET_ERROR!=connect(m_pOwnerSocket->GetSocketHandle(), lpSockAddr, nSockAddrLen));

  if (res || WSAGetLastError()==WSAEWOULDBLOCK)
    SetLayerState(connecting);
  return res;
}

//Gets the address of the peer socket to which the socket is connected
BOOL CAsyncSocketExLayer::GetPeerName( CString& rPeerAddress, UINT& rPeerPort )
{
  return GetPeerNameNext(rPeerAddress, rPeerPort);
}

BOOL CAsyncSocketExLayer::GetPeerNameNext( CString& rPeerAddress, UINT& rPeerPort )
{
  if (m_pNextLayer)
  {
    return m_pNextLayer->GetPeerName(rPeerAddress, rPeerPort);
  }
  else
  {
    SOCKADDR* sockAddr;
    int nSockAddrLen;

    if (m_nFamily == AF_INET6)
    {
      sockAddr = (SOCKADDR*)new SOCKADDR_IN6;
      nSockAddrLen = sizeof(SOCKADDR_IN6);
    }
    else if (m_nFamily == AF_INET)
    {
      sockAddr = (SOCKADDR*)new SOCKADDR_IN;
      nSockAddrLen = sizeof(SOCKADDR_IN);
    }

    memset(sockAddr, 0, nSockAddrLen);

    BOOL bResult = GetPeerName(sockAddr, &nSockAddrLen);

    if (bResult)
    {
      if (m_nFamily == AF_INET6)
      {
        rPeerPort = ntohs(((SOCKADDR_IN6*)sockAddr)->sin6_port);
        LPTSTR buf = Inet6AddrToString(((SOCKADDR_IN6*)sockAddr)->sin6_addr);
        rPeerAddress = buf;
        delete [] buf;
      }
      else if (m_nFamily == AF_INET)
      {
        rPeerPort = ntohs(((SOCKADDR_IN*)sockAddr)->sin_port);
        rPeerAddress = inet_ntoa(((SOCKADDR_IN*)sockAddr)->sin_addr);
      }
      else
      {
        delete sockAddr;
        return FALSE;
      }
    }
    delete sockAddr;

    return bResult;
  }
}

BOOL CAsyncSocketExLayer::GetPeerName( SOCKADDR* lpSockAddr, int* lpSockAddrLen )
{
  return GetPeerNameNext(lpSockAddr, lpSockAddrLen);
}

BOOL CAsyncSocketExLayer::GetPeerNameNext( SOCKADDR* lpSockAddr, int* lpSockAddrLen )
{
  if (m_pNextLayer)
  {
    return m_pNextLayer->GetPeerName(lpSockAddr, lpSockAddrLen);
  }
  else
  {
    DebugAssert(m_pOwnerSocket);
    if ( !getpeername(m_pOwnerSocket->GetSocketHandle(), lpSockAddr, lpSockAddrLen) )
    {
      return TRUE;
    }
    else
    {
      return FALSE;
    }
  }
}

//Gets the address of the sock socket to which the socket is connected
BOOL CAsyncSocketExLayer::GetSockName( CString& rSockAddress, UINT& rSockPort )
{
  return GetSockNameNext(rSockAddress, rSockPort);
}

BOOL CAsyncSocketExLayer::GetSockNameNext( CString& rSockAddress, UINT& rSockPort )
{
  if (m_pNextLayer)
    return m_pNextLayer->GetSockName(rSockAddress, rSockPort);
  else
  {
    SOCKADDR* sockAddr;
    int nSockAddrLen;

    if (m_nFamily == AF_INET6)
    {
      sockAddr = (SOCKADDR*)new SOCKADDR_IN6;
      nSockAddrLen = sizeof(SOCKADDR_IN6);
    }
    else if (m_nFamily == AF_INET)
    {
      sockAddr = (SOCKADDR*)new SOCKADDR_IN;
      nSockAddrLen = sizeof(SOCKADDR_IN);
    }

    memset(sockAddr, 0, nSockAddrLen);

    BOOL bResult = GetSockName(sockAddr, &nSockAddrLen);

    if (bResult)
    {
      if (m_nFamily == AF_INET6)
      {
        rSockPort = ntohs(((SOCKADDR_IN6*)sockAddr)->sin6_port);
        LPTSTR buf = Inet6AddrToString(((SOCKADDR_IN6*)sockAddr)->sin6_addr);
        rSockAddress = buf;
        delete [] buf;
      }
      else if (m_nFamily == AF_INET)
      {
        rSockPort = ntohs(((SOCKADDR_IN*)sockAddr)->sin_port);
        rSockAddress = inet_ntoa(((SOCKADDR_IN*)sockAddr)->sin_addr);
      }
      else
      {
        delete sockAddr;
        return FALSE;
      }
    }
    delete sockAddr;

    return bResult;
  }
}

BOOL CAsyncSocketExLayer::GetSockName( SOCKADDR* lpSockAddr, int* lpSockAddrLen )
{
  return GetSockNameNext(lpSockAddr, lpSockAddrLen);
}

BOOL CAsyncSocketExLayer::GetSockNameNext( SOCKADDR* lpSockAddr, int* lpSockAddrLen )
{
  if (m_pNextLayer)
    return m_pNextLayer->GetSockName(lpSockAddr, lpSockAddrLen);
  else
  {
    DebugAssert(m_pOwnerSocket);
    if ( !getsockname(m_pOwnerSocket->GetSocketHandle(), lpSockAddr, lpSockAddrLen) )
      return TRUE;
    else
      return FALSE;
  }
}

void CAsyncSocketExLayer::Init(CAsyncSocketExLayer *pPrevLayer, CAsyncSocketEx *pOwnerSocket)
{
  DebugAssert(pOwnerSocket);
  m_pPrevLayer=pPrevLayer;
  m_pOwnerSocket=pOwnerSocket;
  m_pNextLayer=0;
  SetLayerState(pOwnerSocket->GetState());
}

int CAsyncSocketExLayer::GetLayerState()
{
  return m_nLayerState;
}

void CAsyncSocketExLayer::SetLayerState(int nLayerState)
{
  DebugAssert(m_pOwnerSocket);
  int nOldLayerState=GetLayerState();
  m_nLayerState=nLayerState;
  if (nOldLayerState!=nLayerState)
    DoLayerCallback(LAYERCALLBACK_STATECHANGE, GetLayerState(), nOldLayerState);
}

void CAsyncSocketExLayer::CallEvent(int nEvent, int nErrorCode)
{
  if (m_nCriticalError)
  {
    return;
  }
  m_nCriticalError = nErrorCode;
  switch (nEvent)
  {
  case FD_READ:
  case FD_FORCEREAD:
    if (GetLayerState()==connecting && !nErrorCode)
    {
      m_nPendingEvents |= nEvent;
      break;
    }
    else if (GetLayerState()==attached)
      SetLayerState(connected);
    if (nEvent & FD_READ)
      m_nPendingEvents &= ~FD_READ;
    else
      m_nPendingEvents &= ~FD_FORCEREAD;
    if (GetLayerState()==connected || nErrorCode)
    {
      if (nErrorCode)
        SetLayerState(aborted);
      OnReceive(nErrorCode);
    }
    break;
  case FD_WRITE:
    if (GetLayerState()==connecting && !nErrorCode)
    {
      m_nPendingEvents |= nEvent;
      break;
    }
    else if (GetLayerState()==attached)
      SetLayerState(connected);
    m_nPendingEvents &= ~FD_WRITE;
    if (GetLayerState()==connected || nErrorCode)
    {
      if (nErrorCode)
        SetLayerState(aborted);
      OnSend(nErrorCode);
    }
    break;
  case FD_CONNECT:
    if (GetLayerState()==connecting || GetLayerState() == attached)
    {
      if (!nErrorCode)
        SetLayerState(connected);
      else
      {
        if (!m_pNextLayer && m_nextAddr)
          if (TryNextProtocol())
          {
            m_nCriticalError = 0;
            return;
          }
        SetLayerState(aborted);
      }
      m_nPendingEvents &= ~FD_CONNECT;
      OnConnect(nErrorCode);

      if (!nErrorCode)
      {
        if ((m_nPendingEvents & FD_READ) && GetLayerState()==connected)
          OnReceive(0);
        if ((m_nPendingEvents & FD_FORCEREAD) && GetLayerState()==connected)
          OnReceive(0);
        if ((m_nPendingEvents & FD_WRITE) && GetLayerState()==connected)
          OnSend(0);
      }
      m_nPendingEvents = 0;
    }
    break;
  case FD_ACCEPT:
    if (GetLayerState()==listening)
    {
      if (nErrorCode)
        SetLayerState(aborted);
      m_nPendingEvents &= ~FD_ACCEPT;
      OnAccept(nErrorCode);
    }
    break;
  case FD_CLOSE:
    if (GetLayerState()==connected || GetLayerState()==attached)
    {
      if (nErrorCode)
        SetLayerState(aborted);
      else
        SetLayerState(closed);
      m_nPendingEvents &= ~FD_CLOSE;
      OnClose(nErrorCode);
    }
    break;
  }
}

//Creates a socket
BOOL CAsyncSocketExLayer::Create(UINT nSocketPort, int nSocketType,
      long lEvent, LPCTSTR lpszSocketAddress, int nFamily /*=AF_INET*/)
{
  return CreateNext(nSocketPort, nSocketType, lEvent, lpszSocketAddress, nFamily);
}

BOOL CAsyncSocketExLayer::CreateNext(UINT nSocketPort, int nSocketType, long lEvent, LPCTSTR lpszSocketAddress, int nFamily /*=AF_INET*/)
{
  DebugAssert((GetLayerState() == notsock) || (GetLayerState() == unconnected));
  BOOL res = FALSE;

  m_nFamily = nFamily;

  if (m_pNextLayer)
    res = m_pNextLayer->Create(nSocketPort, nSocketType, lEvent, lpszSocketAddress, nFamily);
  else if (m_nFamily == AF_UNSPEC)
  {
    m_lEvent = lEvent;
    delete [] m_lpszSocketAddress;
    if (lpszSocketAddress && *lpszSocketAddress)
    {
      m_lpszSocketAddress = new TCHAR[_tcslen(lpszSocketAddress) + 1];
      _tcscpy(m_lpszSocketAddress, lpszSocketAddress);
    }
    else
      m_lpszSocketAddress = 0;
    m_nSocketPort = nSocketPort;
    res = TRUE;
  }
  else
  {
    SOCKET hSocket=socket(nFamily, nSocketType, 0);
    if (hSocket == INVALID_SOCKET)
    {
      m_pOwnerSocket->Close();
      return FALSE;
    }
    m_pOwnerSocket->m_SocketData.hSocket=hSocket;
    m_pOwnerSocket->AttachHandle(hSocket);
    if (!m_pOwnerSocket->AsyncSelect(lEvent))
    {
      m_pOwnerSocket->Close();
      return FALSE;
    }
    if (m_pOwnerSocket->m_pFirstLayer)
    {
      if (WSAAsyncSelect(m_pOwnerSocket->m_SocketData.hSocket, m_pOwnerSocket->GetHelperWindowHandle(), m_pOwnerSocket->m_SocketData.nSocketIndex+WM_SOCKETEX_NOTIFY, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE) )
      {
        m_pOwnerSocket->Close();
        return FALSE;
      }
    }
    if (!m_pOwnerSocket->Bind(nSocketPort, lpszSocketAddress))
    {
      m_pOwnerSocket->Close();
      return FALSE;
    }
    res = TRUE;
  }
  if (res)
    SetLayerState(unconnected);
  return res;
}

int CAsyncSocketExLayer::DoLayerCallback(int nType, int nParam1, int nParam2, char* str /*=0*/)
{
  if (!m_pOwnerSocket)
    return 0;

  int nError = WSAGetLastError();

  t_callbackMsg msg;
  msg.pLayer = this;
  msg.nType = nType;
  msg.nParam1 = nParam1;
  msg.nParam2 = nParam2;
  msg.str = str;

  m_pOwnerSocket->AddCallbackNotification(msg);

  WSASetLastError(nError);

  return 0;
}

BOOL CAsyncSocketExLayer::Listen( int nConnectionBacklog)
{
  return ListenNext( nConnectionBacklog);
}

BOOL CAsyncSocketExLayer::ListenNext( int nConnectionBacklog)
{
  DebugAssert(GetLayerState()==unconnected);
  BOOL res;
  if (m_pNextLayer)
    res=m_pNextLayer->Listen(nConnectionBacklog);
  else
    res=listen(m_pOwnerSocket->GetSocketHandle(), nConnectionBacklog);
  if (res!=SOCKET_ERROR)
  {
    SetLayerState(listening);
  }
  return res!=SOCKET_ERROR;
}

BOOL CAsyncSocketExLayer::Accept( CAsyncSocketEx& rConnectedSocket, SOCKADDR* lpSockAddr /*=NULL*/, int* lpSockAddrLen /*=NULL*/ )
{
  return AcceptNext(rConnectedSocket, lpSockAddr, lpSockAddrLen);
}

BOOL CAsyncSocketExLayer::AcceptNext( CAsyncSocketEx& rConnectedSocket, SOCKADDR* lpSockAddr /*=NULL*/, int* lpSockAddrLen /*=NULL*/ )
{
  DebugAssert(GetLayerState()==listening);
  BOOL res;
  if (m_pNextLayer)
    res=m_pNextLayer->Accept(rConnectedSocket, lpSockAddr, lpSockAddrLen);
  else
  {
    SOCKET hTemp = accept(m_pOwnerSocket->m_SocketData.hSocket, lpSockAddr, lpSockAddrLen);

    if (hTemp == INVALID_SOCKET)
      return FALSE;
    DebugCheck(rConnectedSocket.InitAsyncSocketExInstance());
    rConnectedSocket.m_SocketData.hSocket=hTemp;
    rConnectedSocket.AttachHandle(hTemp);
    rConnectedSocket.SetFamily(GetFamily());
    rConnectedSocket.SetState(connected);
  }
  return TRUE;
}

BOOL CAsyncSocketExLayer::ShutDown(int nHow /*=sends*/)
{
  return ShutDownNext(nHow);
}

BOOL CAsyncSocketExLayer::ShutDownNext(int nHow /*=sends*/)
{
  if (m_nCriticalError)
  {
    WSASetLastError(m_nCriticalError);
    return FALSE;
  }
  else if (GetLayerState()==notsock)
  {
    WSASetLastError(WSAENOTSOCK);
    return FALSE;
  }
  else if (GetLayerState()==unconnected || GetLayerState()==connecting || GetLayerState()==listening)
  {
    WSASetLastError(WSAENOTCONN);
    return FALSE;
  }

  if (!m_pNextLayer)
  {
    DebugAssert(m_pOwnerSocket);
    return (shutdown(m_pOwnerSocket->GetSocketHandle(), nHow) == 0);
  }
  else
  {
    return m_pNextLayer->ShutDown(nHow);
  }
}

int CAsyncSocketExLayer::GetFamily() const
{
  return m_nFamily;
}

bool CAsyncSocketExLayer::SetFamily(int nFamily)
{
  if (m_nFamily != AF_UNSPEC)
    return false;

  m_nFamily = nFamily;
  return true;
}

bool CAsyncSocketExLayer::TryNextProtocol()
{
  m_pOwnerSocket->DetachHandle(m_pOwnerSocket->m_SocketData.hSocket);
  closesocket(m_pOwnerSocket->m_SocketData.hSocket);
  m_pOwnerSocket->m_SocketData.hSocket = INVALID_SOCKET;

  BOOL ret = FALSE;
  for (; m_nextAddr; m_nextAddr = m_nextAddr->ai_next)
  {
    m_pOwnerSocket->m_SocketData.hSocket = socket(m_nextAddr->ai_family, m_nextAddr->ai_socktype, m_nextAddr->ai_protocol);

    if (m_pOwnerSocket->m_SocketData.hSocket == INVALID_SOCKET)
      continue;

    m_pOwnerSocket->AttachHandle(m_pOwnerSocket->m_SocketData.hSocket);
    if (!m_pOwnerSocket->AsyncSelect(m_lEvent))
    {
      m_pOwnerSocket->DetachHandle(m_pOwnerSocket->m_SocketData.hSocket);
      closesocket(m_pOwnerSocket->m_SocketData.hSocket);
      m_pOwnerSocket->m_SocketData.hSocket = INVALID_SOCKET;
      continue;
    }

    if (m_pOwnerSocket->m_pFirstLayer)
    {
      if (WSAAsyncSelect(m_pOwnerSocket->m_SocketData.hSocket, m_pOwnerSocket->GetHelperWindowHandle(), m_pOwnerSocket->m_SocketData.nSocketIndex+WM_SOCKETEX_NOTIFY, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE))
      {
        m_pOwnerSocket->DetachHandle(m_pOwnerSocket->m_SocketData.hSocket);
        closesocket(m_pOwnerSocket->m_SocketData.hSocket);
        m_pOwnerSocket->m_SocketData.hSocket = INVALID_SOCKET;
        continue;
      }
    }

    m_pOwnerSocket->m_SocketData.nFamily = m_nextAddr->ai_family;
    m_nFamily = m_nextAddr->ai_family;
    if (!m_pOwnerSocket->Bind(m_nSocketPort, m_lpszSocketAddress))
    {
      m_pOwnerSocket->DetachHandle(m_pOwnerSocket->m_SocketData.hSocket);
      closesocket(m_pOwnerSocket->m_SocketData.hSocket);
      m_pOwnerSocket->m_SocketData.hSocket = INVALID_SOCKET;
      continue;
    }

    if (connect(m_pOwnerSocket->GetSocketHandle(), m_nextAddr->ai_addr, m_nextAddr->ai_addrlen) == SOCKET_ERROR && WSAGetLastError() != WSAEWOULDBLOCK)
    {
      m_pOwnerSocket->DetachHandle(m_pOwnerSocket->m_SocketData.hSocket);
      closesocket(m_pOwnerSocket->m_SocketData.hSocket);
      m_pOwnerSocket->m_SocketData.hSocket = INVALID_SOCKET;
      continue;
    }

    SetLayerState(connecting);

    ret = true;
    break;
  }

  if (m_nextAddr)
    m_nextAddr = m_nextAddr->ai_next;

  if (!m_nextAddr)
  {
    freeaddrinfo(m_addrInfo);
    m_nextAddr = 0;
    m_addrInfo = 0;
  }

  if (m_pOwnerSocket->m_SocketData.hSocket == INVALID_SOCKET || !ret)
    return FALSE;
  else
    return TRUE;
}

void CAsyncSocketExLayer::LogSocketMessageRaw(int nMessageType, LPCTSTR pMsg)
{
  if (m_pPrevLayer)
    m_pPrevLayer->LogSocketMessageRaw(nMessageType, pMsg);
  else
    m_pOwnerSocket->LogSocketMessageRaw(nMessageType, pMsg);
}

bool CAsyncSocketExLayer::LoggingSocketMessage(int nMessageType)
{
  if (m_pPrevLayer)
    return m_pPrevLayer->LoggingSocketMessage(nMessageType);
  else
    return m_pOwnerSocket->LoggingSocketMessage(nMessageType);
}

int CAsyncSocketExLayer::GetSocketOptionVal(int OptionID) const
{
  if (m_pPrevLayer)
    return m_pPrevLayer->GetSocketOptionVal(OptionID);
  else
    return m_pOwnerSocket->GetSocketOptionVal(OptionID);
}
