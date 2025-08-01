//---------------------------------------------------------------------------
#include "stdafx.h"
#include "TransferSocket.h"
#include "MainThread.h"
#include "AsyncProxySocketLayer.h"
#ifndef MPEXT_NO_GSS
#include "AsyncGssSocketLayer.h"
#endif

#define BUFSIZE 16384

#define STATE_WAITING    0
#define STATE_STARTING    1
#define STATE_STARTED    2

/////////////////////////////////////////////////////////////////////////////
// CTransferSocket

CTransferSocket::CTransferSocket(CFtpControlSocket *pOwner, int nMode)
{
  DebugAssert(pOwner);
  InitIntern(pOwner->GetIntern());
  m_pOwner = pOwner;
  m_nMode = nMode;
  m_nTransferState = STATE_WAITING;
  m_bCheckTimeout = FALSE;
  m_pBuffer = 0;
#ifndef MPEXT_NO_ZLIB
  m_pBuffer2 = 0;
#endif
  m_bufferpos = 0;
  m_pFile = 0;
  m_OnTransferOut = NULL;
  m_OnTransferIn = NULL;
  m_bListening = FALSE;
  m_bSentClose = FALSE;
  m_nInternalMessageID = 0;
  m_transferdata.transfersize = 0;
  m_transferdata.transferleft = 0;
  m_uploaded = 0;
  m_nNotifyWaiting = 0;
  m_bActivationPending = false;
  m_LastSendBufferUpdate = 0;

  UpdateStatusBar(true);

  m_pProxyLayer = NULL;
  m_pSslLayer = NULL;
#ifndef MPEXT_NO_GSS
  m_pGssLayer = NULL;
#endif

  if (m_nMode & CSMODE_LIST)
  {
    const bool mlst = false;
    m_pListResult = m_pOwner->CreateListResult(mlst);
  }
  else
    m_pListResult = 0;
  m_LastUpdateTime.QuadPart = 0;

#ifndef MPEXT_NO_ZLIB
  memset(&m_zlibStream, 0, sizeof(m_zlibStream));
  m_useZlib = false;
#endif
}

CTransferSocket::~CTransferSocket()
{
  delete [] m_pBuffer;
#ifndef MPEXT_NO_ZLIB
  delete [] m_pBuffer2;
#endif
  GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), 0);
  Close();
  RemoveAllLayers();
  delete m_pProxyLayer;
  delete m_pSslLayer;
#ifndef MPEXT_NO_GSS
  delete m_pGssLayer;
#endif
  m_pOwner->RemoveActiveTransfer();

  delete m_pListResult;

#ifndef MPEXT_NO_ZLIB
  if (m_useZlib)
  {
    if (m_nMode & CSMODE_UPLOAD)
      deflateEnd(&m_zlibStream);
    else
      inflateEnd(&m_zlibStream);
  }
#endif
}

/////////////////////////////////////////////////////////////////////////////
// Member-Funktion CTransferSocket
void CTransferSocket::OnReceive(int nErrorCode)
{
  if (GetState() != connected && GetState() != attached && GetState() != closed)
    return;
  if (m_nTransferState == STATE_WAITING)
  {
    m_nNotifyWaiting |= FD_READ;
    return;
  }

  if (m_bSentClose)
    return;
  if (m_bListening)
    return;

  if (m_nMode&CSMODE_LIST)
  {
    if (m_nTransferState == STATE_STARTING)
      OnConnect(0);

    std::vector<char> Buffer(BUFSIZE);
    int numread = CAsyncSocketEx::Receive(&Buffer[0], Buffer.size());
    if (numread != SOCKET_ERROR && numread)
    {
      m_LastActiveTime = CTime::GetCurrentTime();

#ifndef MPEXT_NO_ZLIB
      if (m_useZlib)
      {
        m_zlibStream.next_in = (Bytef *)&Buffer[0];
        m_zlibStream.avail_in = numread;
        std::unique_ptr<char []> out(new char[BUFSIZE]);
        m_zlibStream.next_out = (Bytef *)&out[0];
        m_zlibStream.avail_out = BUFSIZE;
        int res = inflate(&m_zlibStream, 0);
        while (res == Z_OK)
        {
          m_pListResult->AddData(&out[0], BUFSIZE - m_zlibStream.avail_out);
          out.reset(new char[BUFSIZE]);
          m_zlibStream.next_out = (Bytef *)&out[0];
          m_zlibStream.avail_out = BUFSIZE;
          res = inflate(&m_zlibStream, 0);
        }
        if (res == Z_STREAM_END)
          m_pListResult->AddData(&out[0], BUFSIZE - m_zlibStream.avail_out);
        else if (res != Z_OK && res != Z_BUF_ERROR)
        {
          CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
          return;
        }
      }
      else
#endif
      {
        m_pListResult->AddData(&Buffer[0], numread);
      }
      m_transferdata.transfersize += numread;
      t_ffam_transferstatus *status = new t_ffam_transferstatus;
      status->bFileTransfer = FALSE;
      status->transfersize = -1;
      status->bytes = m_transferdata.transfersize;
      GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), (LPARAM)status);
    }
    if (!numread)
    {
      CloseAndEnsureSendClose(0);
    }
    if (numread == SOCKET_ERROR)
    {
      int nError = GetLastError();
      if (nError == WSAENOTCONN)
      {
        //Not yet connected
        return;
      }
      else if (m_pSslLayer && nError == WSAESHUTDOWN)
      {
        // Do nothing, wait for shutdown complete notification.
        return;
      }
      else if (nError != WSAEWOULDBLOCK)
      {
        LogError(nError);
        CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
      }
    }
  }
  else if (m_nMode & CSMODE_DOWNLOAD)
  {
    if (m_nTransferState == STATE_STARTING)
      OnConnect(0);

    bool beenWaiting = false;
    _int64 ableToRead;
    if (GetState() != closed)
      ableToRead = m_pOwner->GetAbleToTransferSize(CFtpControlSocket::download, beenWaiting);
    else
      ableToRead = BUFSIZE;

    if (!beenWaiting)
      DebugAssert(ableToRead);
    else if (!ableToRead)
    {
      TriggerEvent(FD_READ);
      return;
    }

    if (!m_pBuffer)
      m_pBuffer = new char[BUFSIZE];

    int numread = CAsyncSocketEx::Receive(m_pBuffer, static_cast<int>(ableToRead));
    if (numread!=SOCKET_ERROR)
    {
      m_pOwner->SpeedLimitAddTransferredBytes(CFtpControlSocket::download, numread);
    }

    if (!numread)
    {
      CloseAndEnsureSendClose(0);
      return;
    }

    if (numread == SOCKET_ERROR)
    {
      int nError = GetLastError();
      if (nError == WSAENOTCONN)
      {
        //Not yet connected
        return;
      }
      else if (m_pSslLayer && nError == WSAESHUTDOWN)
      {
        // Do nothing, wait for shutdown complete notification.
        return;
      }
      else if (nError != WSAEWOULDBLOCK)
      {
        LogError(nError);
        CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
      }

      UpdateStatusBar(true);
      return;
    }

    int written = 0;
    m_LastActiveTime = CTime::GetCurrentTime();
    TRY
    {
#ifndef MPEXT_NO_ZLIB
      if (m_useZlib)
      {
        if (!m_pBuffer2)
          m_pBuffer2 = new char[BUFSIZE];

        m_zlibStream.next_in = (Bytef *)m_pBuffer;
        m_zlibStream.avail_in = numread;
        m_zlibStream.next_out = (Bytef *)m_pBuffer2;
        m_zlibStream.avail_out = BUFSIZE;
        int res = inflate(&m_zlibStream, 0);
        while (res == Z_OK)
        {
          WriteData(m_pBuffer2, BUFSIZE - m_zlibStream.avail_out);
          written += BUFSIZE - m_zlibStream.avail_out;
          m_zlibStream.next_out = (Bytef *)m_pBuffer2;
          m_zlibStream.avail_out = BUFSIZE;
          res = inflate(&m_zlibStream, 0);
        }
        if (res == Z_STREAM_END)
        {
          WriteData(m_pBuffer2, BUFSIZE - m_zlibStream.avail_out);
          written += BUFSIZE - m_zlibStream.avail_out;
        }
        else if (res != Z_OK && res != Z_BUF_ERROR)
        {
          m_pOwner->ShowStatus(L"Compression error", FZ_LOG_ERROR);
          CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
          return;
        }
      }
      else
#endif
      {
        WriteData(m_pBuffer, numread);
        written = numread;
      }
    }
    CATCH(CFileException,e)
    {
      LPTSTR msg = new TCHAR[BUFSIZE];
      if (e->GetErrorMessage(msg, BUFSIZE))
        m_pOwner->ShowStatus(msg, FZ_LOG_ERROR);
      delete [] msg;
      CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
      return;
    }
    END_CATCH;
    m_transferdata.transferleft -= written;

    UpdateStatusBar(false);
  }
}

void CTransferSocket::SetBuffers()
{
  /* Set internal socket send buffer
   * this should fix the speed problems some users have reported
   */
  m_SendBuf = GetOptionVal(OPTION_MPEXT_SNDBUF);
  if (m_SendBuf > 0)
  {
    DWORD value;
    int len = sizeof(value);
    GetSockOpt(SO_SNDBUF, &value, &len);
    if (value < m_SendBuf)
    {
      SetSockOpt(SO_SNDBUF, &m_SendBuf, sizeof(m_SendBuf));
    }

    // For now we increase receive buffer, whenever send buffer is set.
    // The size is not configurable. The constant taken from FZ.
    value = 0;
    len = sizeof(value);
    GetSockOpt(SO_RCVBUF, &value, &len);
    int rcvbuf = 4 * 1024 * 1024;
    if (value < rcvbuf)
    {
      value = rcvbuf;
      SetSockOpt(SO_RCVBUF, &value, sizeof(value));
    }
  }
}

void CTransferSocket::OnAccept(int nErrorCode)
{
  m_bListening=FALSE;
  CAsyncSocketEx tmp;
  Accept(tmp);
  SOCKET socket=tmp.Detach();
  CAsyncSocketEx::Close();

  Attach(socket);

  SetBuffers();

  if (m_nTransferState == STATE_STARTING)
  {
    Start();
  }
}

void CTransferSocket::ConfigureSocket()
{
  // Note that FileZilla re-enables Nagle's alg during TLS negotiation.

  // Following post claims that TCP_NODELAY
  // has to be set before connect()
  // https://stackoverflow.com/q/22583941/850848#25871250

  int nodelay = GetOptionVal(OPTION_MPEXT_NODELAY);
  if (nodelay != 0)
  {
    BOOL bvalue = TRUE;
    SetSockOpt(TCP_NODELAY, &bvalue, sizeof(bvalue), IPPROTO_TCP);
  }

  CAsyncSocketEx::ConfigureSocket();
}

void CTransferSocket::OnConnect(int nErrorCode)
{
  if (nErrorCode)
  {
    TCHAR buffer[1000];
    memset(buffer, 0, sizeof(buffer));
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 0, nErrorCode, 0, buffer, 999, 0);
    CString str;
    str.Format(IDS_ERRORMSG_CANTOPENTRANSFERCHANNEL,buffer);
    str.Replace( L"\n", L"\0" );
    str.Replace( L"\r", L"\0" );
    m_pOwner->ShowStatus(str, FZ_LOG_ERROR);
    CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
  }
  else
  {
    SetBuffers();
    m_pOwner->ShowStatus(L"Data connection opened", FZ_LOG_INFO);
  }
  if (m_nTransferState == STATE_WAITING)
  {
    // OnReceive (invoked by m_nNotifyWaiting including FD_READ)
    // will call back to OnConnected (as we won't be connected yet).
    // This is needed for file transfers only, where SetActive is
    // called only after 1xx response to RETR (and similar) arrives.
    // But we get FD_CONNECT earlier, hence we get to this branch.
    // With directory listing, SetActive is called before Connect,
    // so we are already STATE_STARTING on FD_CONNECT.
    // It should probably behave the same in both scenarios.
    m_nNotifyWaiting |= FD_READ;
  }
  else if (m_nTransferState == STATE_STARTING)
  {
    Start();
  }
}

void CTransferSocket::Start()
{
  m_nTransferState = STATE_STARTED;

  m_LastActiveTime=CTime::GetCurrentTime();

  if (m_pSslLayer)
  {
    AddLayer(m_pSslLayer);
    int res = m_pSslLayer->InitSSLConnection(true, m_pOwner->m_pSslLayer,
      GetOptionVal(OPTION_MPEXT_SSLSESSIONREUSE), CString(),
      m_pOwner->m_pTools);
    if (res == SSL_FAILURE_INITSSL)
    {
      m_pOwner->ShowStatus(IDS_ERRORMSG_CANTINITSSL, FZ_LOG_ERROR);
    }

    if (res)
    {
      CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
      return;
    }
  }

#ifndef MPEXT_NO_GSS
  if (m_pGssLayer)
  {
    AddLayer(m_pGssLayer);
  }
#endif
}

void CTransferSocket::OnClose(int nErrorCode)
{
  if (m_nTransferState == STATE_WAITING)
  {
    m_nNotifyWaiting |= FD_CLOSE;
    return;
  }

  m_pOwner->ShowStatus(L"Data connection closed", FZ_LOG_PROGRESS);

  OnReceive(0);
  CloseAndEnsureSendClose(0);
  // Report pending transfer
  // (particularly during ascii downloads, when the transferred data might be larger than expected,
  // though possibly also when downloading a growing file)
  if (FLAGSET(m_nMode, CSMODE_DOWNLOAD))
  {
    UpdateStatusBar(true);
  }
}

int CTransferSocket::CheckForTimeout(int delay)
{
  UpdateStatusBar(false);
  if (!m_bCheckTimeout)
  {
    // we are closed, so make sure the FTP control socket is itself checking for
    // timeout as we are not
    return 0;
  }
  CTimeSpan span = CTime::GetCurrentTime()-m_LastActiveTime;
  if (span.GetTotalSeconds()>=delay)
  {
    m_pOwner->ShowTimeoutError(IDS_DATA_CONNECTION);
    CloseAndEnsureSendClose(CSMODE_TRANSFERTIMEOUT);
    return 2;
  }
  return 1;
}

void CTransferSocket::SetState(int nState)
{
  CAsyncSocketEx::SetState(nState);
  if (m_bActivationPending && Activate())
  {
    m_bActivationPending = false;
  }
}

bool CTransferSocket::Activate()
{
  // Activation (OnSend => OnConnect) indirectly causes adding
  // of TLS layer, which needs connected underlying layers.
  // The code should be generic, but we particularly need it for this (TLS over proxy)
  // scenario only. So for a safety, we use it for the scenario only.
  bool Result =
    (GetState() == connected) || (GetState() == attached) ||
    (m_pSslLayer == NULL) || (m_pProxyLayer == NULL);
  if (Result)
  {
    if (m_nTransferState == STATE_WAITING)
      m_nTransferState = STATE_STARTING;
    m_bCheckTimeout = TRUE;
    m_LastActiveTime = CTime::GetCurrentTime();

    if (m_nNotifyWaiting & FD_READ)
      OnReceive(0);
    if (m_nNotifyWaiting & FD_WRITE)
      OnSend(0);
    if (m_nNotifyWaiting & FD_CLOSE)
      OnClose(0);
  }
  return Result;
}

void CTransferSocket::SetActive()
{
  if (!Activate())
  {
    m_bActivationPending = true;
  }
}

void CTransferSocket::OnSend(int nErrorCode)
{
  if (m_nTransferState == STATE_WAITING)
  {
    m_nNotifyWaiting |= FD_WRITE;
    return;
  }

  if (m_bSentClose)
  {
    return;
  }
  if (m_bListening)
  {
    return;
  }

  if (!(m_nMode&CSMODE_UPLOAD))
  {
    return;
  }

  if (m_nTransferState == STATE_STARTING)
  {
    OnConnect(0);
  }

  if (m_SendBuf > 0)
  {
    unsigned int Ticks = GetTickCount();
    if (Ticks - m_LastSendBufferUpdate >= 1000)
    {
      DWORD BufferLen = 0;
      DWORD OutLen = 0;
      if (WSAIoctl(m_SocketData.hSocket, SIO_IDEAL_SEND_BACKLOG_QUERY, NULL, 0, &BufferLen, sizeof(BufferLen), &OutLen, 0, 0) == 0)
      {
        DebugAssert(OutLen == sizeof(BufferLen));
        if (m_SendBuf < BufferLen)
        {
          LogMessage(FZ_LOG_PROGRESS, L"Increasing send buffer from %d to %d", m_SendBuf, BufferLen);
          m_SendBuf = BufferLen;
          SetSockOpt(SO_SNDBUF, &m_SendBuf, sizeof(m_SendBuf));
        }
      }
      m_LastSendBufferUpdate = Ticks;
    }
  }

#ifndef MPEXT_NO_ZLIB
  if (m_useZlib)
  {
    if (!m_pBuffer)
    {
      m_pBuffer = new char[BUFSIZE];
      m_bufferpos = 0;

      m_zlibStream.next_out = (Bytef *)m_pBuffer;
      m_zlibStream.avail_out = BUFSIZE;
    }
    if (!m_pBuffer2)
    {
      m_pBuffer2 = new char[BUFSIZE];

      m_zlibStream.next_in = (Bytef *)m_pBuffer2;
    }

    bool beenWaiting = false;
    while (true)
    {
      int numsend;
      if (!m_zlibStream.avail_in)
      {
        if (m_pFile)
        {
          DWORD numread;
          numread = ReadDataFromFile(m_pBuffer2, BUFSIZE);
          if (numread < 0)
          {
            return;
          }

          m_transferdata.transferleft -= numread;
          m_zlibStream.next_in = (Bytef *)m_pBuffer2;
          m_zlibStream.avail_in = numread;

          if (numread < BUFSIZE)
            m_pFile = 0;
        }
      }
      if (!m_zlibStream.avail_out)
      {
        if (m_bufferpos >= BUFSIZE)
        {
          m_bufferpos = 0;
          m_zlibStream.next_out = (Bytef *)m_pBuffer;
          m_zlibStream.avail_out = BUFSIZE;
        }
      }

      int res = Z_OK;
      if (m_zlibStream.avail_out)
      {
        res = deflate(&m_zlibStream, m_pFile ? 0 : Z_FINISH);
        if (res != Z_OK && (!m_pFile && res != Z_STREAM_END))
        {
          m_pOwner->ShowStatus("Decompression error", FZ_LOG_ERROR);
          CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
          return;
        }
      }

      numsend = BUFSIZE;
      int len = BUFSIZE - m_bufferpos - m_zlibStream.avail_out;
      if (!len && !m_pFile)
      {
        break;
      }

      if (len < BUFSIZE)
        numsend = len;

      int nLimit = (int)m_pOwner->GetAbleToTransferSize(CFtpControlSocket::upload, beenWaiting);
      if (nLimit != -1 && GetState() != closed && numsend > nLimit)
        numsend = nLimit;

      if (!numsend)
      {
        TriggerEvent(FD_WRITE);
        return;
      }

      int numsent = Send(m_pBuffer + m_bufferpos, numsend);
      if (numsent == SOCKET_ERROR)
      {
        int nError = GetLastError();
        if (nError == WSAENOTCONN)
        {
          //Not yet connected
          return;
        }
        else if (m_pSslLayer && nError == WSAESHUTDOWN)
        {
          // Do nothing, wait for shutdown complete notification.
          return;
        }
        else if (nError != WSAEWOULDBLOCK)
        {
          LogError(nError);
          CloseOnShutDownOrError(CSMODE_TRANSFERERROR);
        }
        UpdateStatusBar(true);
        return;
      }

      m_uploaded += numsent;
      m_pOwner->SpeedLimitAddTransferredBytes(CFtpControlSocket::upload, numsent);
      m_LastActiveTime = CTime::GetCurrentTime();

      m_bufferpos += numsent;

      UpdateStatusBar(false);

      if (!m_zlibStream.avail_in && !m_pFile && m_zlibStream.avail_out &&
        m_zlibStream.avail_out + m_bufferpos == BUFSIZE && res == Z_STREAM_END)
      {
        CloseOnShutDownOrError(0);
        return;
      }

      //Check if there are other commands in the command queue.
      MSG msg;
      if (PeekMessage(&msg,0, 0, 0, PM_NOREMOVE))
      {
        TriggerEvent(FD_WRITE);
        return;
      }
    }
  }
  else
#endif
  {
    if (!m_pFile && (m_OnTransferIn == NULL))
    {
      return;
    }
    if (!m_pBuffer)
      m_pBuffer = new char[BUFSIZE];

    int numread;

    bool beenWaiting = false;
    _int64 currentBufferSize;
    if (GetState() != closed)
      currentBufferSize = m_pOwner->GetAbleToTransferSize(CFtpControlSocket::upload, beenWaiting);
    else
      currentBufferSize = BUFSIZE;

    if (!currentBufferSize && !m_bufferpos)
    {
      // Not allowed to send yet, try later
      TriggerEvent(FD_WRITE);
      return;
    }
    else if (m_bufferpos < currentBufferSize)
    {
      numread = ReadDataFromFile(m_pBuffer + m_bufferpos, static_cast<int>(currentBufferSize - m_bufferpos));
      if (numread < 0 )
      {
        return;
      }
      else if (!numread && !m_bufferpos)
      {
        CloseOnShutDownOrError(0);
        return;
      }
    }
    else
      numread = 0;

    DebugAssert((numread+m_bufferpos) <= BUFSIZE);
    DebugAssert(numread>=0);
    DebugAssert(m_bufferpos>=0);

    if (numread+m_bufferpos <= 0)
    {
      CloseOnShutDownOrError(0);
      return;
    }

    int numsent = Send(m_pBuffer, numread + m_bufferpos);

    while (TRUE)
    {
      if (numsent != SOCKET_ERROR)
      {
        m_pOwner->SpeedLimitAddTransferredBytes(CFtpControlSocket::upload, numsent);
        m_LastActiveTime = CTime::GetCurrentTime();
        m_transferdata.transferleft -= numsent;
        m_uploaded += numsent;
      }

      if (numsent==SOCKET_ERROR || !numsent)
      {
        int nError = GetLastError();
        if (nError == WSAENOTCONN)
        {
          //Not yet connected
          m_bufferpos += numread;
          return;
        }
        else if (nError == WSAEWOULDBLOCK)
        {
          m_bufferpos += numread;
        }
        else if (m_pSslLayer && nError == WSAESHUTDOWN)
        {
          m_bufferpos += numread;
          // Do nothing, wait for shutdown complete notification.
          return;
        }
        else
        {
          LogError(nError);
          CloseOnShutDownOrError(CSMODE_TRANSFERERROR);
        }
        UpdateStatusBar(true);
        return;
      }
      else
      {
        int pos = numread + m_bufferpos - numsent;

        if (pos < 0 || (numsent + pos) > BUFSIZE)
        {
          LogMessage(FZ_LOG_WARNING, L"Index out of range");
          CloseOnShutDownOrError(CSMODE_TRANSFERERROR);
          return;
        }
        else if (!pos && // all data in buffer were sent
                 // was read less then wanted (eof reached?)
                 // (should be safe to always just do (numread <= 0), but limiting the impact of the change for now)
                 (numread < ((m_OnTransferIn != NULL) ? 1 : (currentBufferSize-m_bufferpos))) &&
                 m_bufferpos != currentBufferSize) // and it's not because the buffer is full?
        {
          // With TLS 1.3 we can get back
          m_bufferpos = 0;
          CloseOnShutDownOrError(0);
          return;
        }
        else if (!pos)
        {
          m_bufferpos = 0;
        }
        else
        {
          memmove(m_pBuffer, m_pBuffer+numsent, pos);
          m_bufferpos=pos;
        }
      }
      //Check if there are other commands in the command queue.
      MSG msg;
      if (PeekMessage(&msg, 0, m_nInternalMessageID, m_nInternalMessageID, PM_NOREMOVE))
      {
        //Send resume message
        LogMessage(FZ_LOG_DEBUG, L"Message waiting in queue, resuming later");
        TriggerEvent(FD_WRITE);
        UpdateStatusBar(false);
        return;
      }
      UpdateStatusBar(false);

      if (GetState() != closed)
        currentBufferSize = m_pOwner->GetAbleToTransferSize(CFtpControlSocket::upload, beenWaiting);
      else
        currentBufferSize = BUFSIZE;

      if (m_bufferpos < currentBufferSize)
      {
        numread = ReadDataFromFile(m_pBuffer + m_bufferpos, static_cast<int>(currentBufferSize - m_bufferpos));
        if (numread < 0 )
        {
          return;
        }
        else if (!numread && !m_bufferpos)
        {
          CloseOnShutDownOrError(0);
          return;
        }
      }
      else
      {
        numread = 0;
      }

      if (!currentBufferSize && !m_bufferpos)
      {
        // Not allowed to send yet, try later
        TriggerEvent(FD_WRITE);
        return;
      }

      DebugAssert(numread>=0);
      DebugAssert(m_bufferpos>=0);
      numsent = Send(m_pBuffer, numread+m_bufferpos);
    }
  }
}

void CTransferSocket::UpdateStatusBar(bool forceUpdate)
{
  if (m_nTransferState != STATE_STARTED)
    return;

  if (!forceUpdate)
  {
    //Don't flood the main window with messages
    //Else performance would be really low
    LARGE_INTEGER curtime;
    LARGE_INTEGER freq;
    QueryPerformanceFrequency(&freq);
    QueryPerformanceCounter(&curtime);
    if (((curtime.QuadPart-m_LastUpdateTime.QuadPart) < (freq.QuadPart/15) ) )
      return;
    m_LastUpdateTime = curtime;
  }

  //Update the statusbar
  t_ffam_transferstatus *status=new t_ffam_transferstatus;
  status->bFileTransfer = m_nMode & (CSMODE_DOWNLOAD | CSMODE_UPLOAD);
  status->transfersize = m_transferdata.transfersize;
  status->bytes=m_transferdata.transfersize-m_transferdata.transferleft;

  GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_TRANSFERSTATUS, 0), (LPARAM)status);
}

BOOL CTransferSocket::Create(BOOL bUseSsl)
{
  if (bUseSsl)
  {
    m_pSslLayer = new CAsyncSslSocketLayer;

    m_pSslLayer->SetClientCertificate(m_pOwner->m_CurrentServer.Certificate, m_pOwner->m_CurrentServer.PrivateKey);
  }

  int nProxyType = GetOptionVal(OPTION_PROXYTYPE);
  if (nProxyType != PROXYTYPE_NOPROXY)
  {
    USES_CONVERSION;
    m_pProxyLayer = new CAsyncProxySocketLayer;
    m_pProxyLayer->SetProxy(
      nProxyType, T2CA(GetOption(OPTION_PROXYHOST)), GetOptionVal(OPTION_PROXYPORT),
      GetOptionVal(OPTION_PROXYUSELOGON), T2CA(GetOption(OPTION_PROXYUSER)), T2CA(GetOption(OPTION_PROXYPASS)));
    AddLayer(m_pProxyLayer);
  }

  if (!GetOptionVal(OPTION_LIMITPORTRANGE))
  {
    if (!CAsyncSocketEx::Create(0, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE, 0, GetFamily()))
      return FALSE;

    return TRUE;
  }
  else
  {
    int min=GetOptionVal(OPTION_PORTRANGELOW);
    int max=GetOptionVal(OPTION_PORTRANGEHIGH);
    if (min > max)
    {
      m_pOwner->ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE,FZ_LOG_ERROR);
      return FALSE;
    }
    int startport=static_cast<int>(min+((double)rand()*(max-min))/(RAND_MAX+1));
    int port=startport;
    // Failure to create the socket, calls Close(), which resets the family. We want to keep trying the original faimily with each port.
    // Only with the specific family set, the Create actually does bind(), without which the port testing does not work.
    int family = GetFamily();
    DebugAssert(family != AF_UNSPEC);
    while (!CAsyncSocketEx::Create(port, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT | FD_CONNECT | FD_CLOSE, 0, family))
    {
      port++;
      if (port>max)
        port=min;
      if (port==startport)
      {
        m_pOwner->ShowStatus(IDS_ERRORMSG_CANTCREATEDUETOPORTRANGE, FZ_LOG_ERROR);
        return FALSE;
      }
    }
    LogMessage(FZ_LOG_INFO, L"Selected port %d", port);
  }

  return TRUE;
}

void CTransferSocket::Close()
{
  m_bCheckTimeout = FALSE;
  CAsyncSocketEx::Close();
}

int CTransferSocket::OnLayerCallback(std::list<t_callbackMsg>& callbacks)
{
  for (std::list<t_callbackMsg>::iterator iter = callbacks.begin(); iter != callbacks.end(); iter++)
  {
    if (iter->nType == LAYERCALLBACK_STATECHANGE)
    {
        if (CAsyncSocketEx::LogStateChange(iter->nParam1, iter->nParam2))
        {
          const TCHAR * state2Desc = CAsyncSocketEx::GetStateDesc(iter->nParam2);
          const TCHAR * state1Desc = CAsyncSocketEx::GetStateDesc(iter->nParam1);
        if (iter->pLayer == m_pProxyLayer)
          LogMessage(FZ_LOG_INFO, L"Proxy layer changed state from %s to %s", state2Desc, state1Desc);
        else if (iter->pLayer == m_pSslLayer)
          LogMessage(FZ_LOG_INFO, L"TLS layer changed state from %s to %s", state2Desc, state1Desc);
#ifndef MPEXT_NO_GSS
        else if (iter->pLayer == m_pGssLayer)
          LogMessage(FZ_LOG_INFO, L"GSS layer changed state from %s to %s", state2Desc, state1Desc);
#endif
        else
          LogMessage(FZ_LOG_INFO, L"Layer @ %d changed state from %s to %s", iter->pLayer, state2Desc, state1Desc);
      }
    }
    else if (iter->nType == LAYERCALLBACK_LAYERSPECIFIC)
    {
      if (iter->pLayer == m_pProxyLayer)
      {
        switch (iter->nParam1)
        {
        case PROXYERROR_NOERROR:
          m_pOwner->ShowStatus(IDS_PROXY_CONNECTED, FZ_LOG_PROGRESS);
          break;
        case PROXYERROR_NOCONN:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_NOCONN, FZ_LOG_ERROR);
          break;
        case PROXYERROR_REQUESTFAILED:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_REQUESTFAILED, FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHTYPEUNKNOWN:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHTYPEUNKNOWN, FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHFAILED:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHFAILED, FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHNOLOGON:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_AUTHNOLOGON, FZ_LOG_ERROR);
          break;
        case PROXYERROR_CANTRESOLVEHOST:
          m_pOwner->ShowStatus(IDS_ERRORMSG_PROXY_CANTRESOLVEHOST, FZ_LOG_ERROR);
          break;
        default:
          LogMessage(FZ_LOG_WARNING, L"Unknown proxy error");
        }
      }
      else if (iter->pLayer == m_pSslLayer)
      {
        switch (iter->nParam1)
        {
        case SSL_INFO:
          switch(iter->nParam2)
          {
          case SSL_INFO_SHUTDOWNCOMPLETE:
            CloseAndEnsureSendClose(0);
            break;
          case SSL_INFO_ESTABLISHED:
            m_pOwner->ShowStatus(IDS_STATUSMSG_SSLESTABLISHEDTRANSFER, FZ_LOG_PROGRESS);
            TriggerEvent(FD_FORCEREAD);
            break;
          }
          break;
        case SSL_FAILURE:
          {
            int Mode = CSMODE_TRANSFERERROR;
            switch (iter->nParam2)
            {
            case SSL_FAILURE_UNKNOWN:
              m_pOwner->ShowStatus(IDS_ERRORMSG_UNKNOWNSSLERROR, FZ_LOG_ERROR);
              // This may indicate re-key failure, make sure we retry
              Mode |= CSMODE_TRANSFERTIMEOUT;
              break;
            case SSL_FAILURE_ESTABLISH:
              m_pOwner->ShowStatus(IDS_ERRORMSG_CANTESTABLISHSSLCONNECTION, FZ_LOG_ERROR);
              break;
            case SSL_FAILURE_INITSSL:
              m_pOwner->ShowStatus(IDS_ERRORMSG_CANTINITSSL, FZ_LOG_ERROR);
              break;
            }
            CloseAndEnsureSendClose(Mode);
          }
          break;
        case SSL_VERIFY_CERT:
          t_SslCertData data;
          LPCTSTR CertError = NULL;
          if (m_pSslLayer->GetPeerCertificateData(data, CertError))
            m_pSslLayer->SetNotifyReply(data.priv_data, SSL_VERIFY_CERT, 1);
          else
          {
            CString str;
            str.Format(TLS_CERT_DECODE_ERROR, CertError);
            m_pOwner->ShowStatus(str, FZ_LOG_ERROR);
            CloseAndEnsureSendClose(CSMODE_TRANSFERERROR);
          }
          break;
        }
      }
#ifndef MPEXT_NO_GSS
      else if (iter->pLayer == m_pGssLayer)
      {
        USES_CONVERSION;
        switch (iter->nParam1)
        {
        case GSS_INFO:
          LogMessageRaw(FZ_LOG_INFO, A2CT(iter->str));
          break;
        case GSS_ERROR:
          LogMessageRaw(FZ_LOG_APIERROR, A2CT(iter->str));
          break;
        case GSS_SHUTDOWN_COMPLETE:
          CloseAndEnsureSendClose(0);
          break;
        }
      }
#endif
    }
    delete [] iter->str;
  }
  return 0;
}

#ifndef MPEXT_NO_GSS
void CTransferSocket::UseGSS(CAsyncGssSocketLayer *pGssLayer)
{
  m_pGssLayer = new CAsyncGssSocketLayer;
  m_pGssLayer->InitTransferChannel(pGssLayer);
}
#endif

#ifndef MPEXT_NO_ZLIB
bool CTransferSocket::InitZlib(int level)
{
  int res;
  if (m_nMode & CSMODE_UPLOAD)
    res = deflateInit2(&m_zlibStream, level, Z_DEFLATED, 15, 8, Z_DEFAULT_STRATEGY);
  else
    res = inflateInit2(&m_zlibStream, 15);

  if (res == Z_OK)
    m_useZlib = true;

  return res == Z_OK;
}
#endif

void CTransferSocket::WriteData(const char * buffer, int len)
{
  if (m_OnTransferOut != NULL)
  {
    m_OnTransferOut(NULL, reinterpret_cast<const unsigned char *>(m_pBuffer), len);
  }
  else
  {
    m_pFile->Write(m_pBuffer, len);
  }
}

int CTransferSocket::ReadData(char * buffer, int len)
{
  int result;
  if (m_OnTransferIn != NULL)
  {
    result = m_OnTransferIn(NULL, reinterpret_cast<unsigned char *>(buffer), len);
  }
  else
  {
    result = m_pFile->Read(buffer, len);
  }
  LogMessage(FZ_LOG_INFO, L"Read %d bytes from file", result);
  return result;
}

int CTransferSocket::ReadDataFromFile(char *buffer, int len)
{
  TRY
  {
    // Comparing to Filezilla 2, we do not do any translation locally,
    // leaving it onto the server (what Filezilla 3 seems to do too)
    const char Bom[4] = "\xEF\xBB\xBF";
    int BomLen = strlen(Bom);
    int read = ReadData(buffer, len);
    if (GetOptionVal(OPTION_MPEXT_REMOVE_BOM) &&
        m_transferdata.bType && (read >= BomLen) && (memcmp(buffer, Bom, BomLen) == 0))
    {
      memcpy(buffer, buffer + BomLen, read - BomLen);
      read -= BomLen;
      int read2 = ReadData(buffer + read, BomLen);
      if (read2 > 0)
      {
        read += read2;
      }
    }
    return read;
  }
  CATCH_ALL(e)
  {
    TCHAR error[BUFSIZE];
    if (e->GetErrorMessage(error, BUFSIZE))
      m_pOwner->ShowStatus(error, FZ_LOG_ERROR);
    CloseOnShutDownOrError(CSMODE_TRANSFERERROR);
    return -1;
  }
  END_CATCH_ALL;
}

void CTransferSocket::LogSocketMessageRaw(int nMessageType, LPCTSTR pMsg)
{
  LogMessageRaw(nMessageType, pMsg);
}

int CTransferSocket::GetSocketOptionVal(int OptionID) const
{
  return GetOptionVal(OptionID);
}

void CTransferSocket::EnsureSendClose(int Mode)
{
  if (!m_bSentClose)
  {
    if (Mode != 0)
    {
      m_pOwner->ShowStatus(L"Data connection failed", FZ_LOG_INFO);
      m_nMode |= Mode;
    }
    else
    {
      m_pOwner->ShowStatus(L"Data connection closed", FZ_LOG_INFO);
    }
    m_bSentClose = TRUE;
    DebugCheck(m_pOwner->m_pOwner->PostThreadMessage(m_nInternalMessageID, FZAPI_THREADMSG_TRANSFEREND, m_nMode));
  }
}

void CTransferSocket::CloseAndEnsureSendClose(int Mode)
{
  Close();
  EnsureSendClose(Mode);
}

void CTransferSocket::CloseOnShutDownOrError(int Mode)
{
  if (ShutDown())
  {
    // It would probably be correct to remove this call, and wait for OnClose (FD_CLOSE),
    // where CloseAndEnsureSendClose is called too.
    // See https://learn.microsoft.com/en-us/windows/win32/winsock/graceful-shutdown-linger-options-and-socket-closure-2
    CloseAndEnsureSendClose(Mode);
  }
  else
  {
    int Error = GetLastError();
    if (Error != WSAEWOULDBLOCK)
    {
      // Log always or only when (Mode & CSMODE_TRANSFERERROR)?
      // Does it anyway make sense at all to call this with Mode == 0?
      LogError(Error);
      CloseAndEnsureSendClose(Mode);
    }
  }
}
