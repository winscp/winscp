// CAsyncSslSocketLayer by Tim Kosse (Tim.Kosse@gmx.de)
//            Version 2.0 (2005-02-27)
//---------------------------------------------------------------------------
// Feel free to use this class, as long as you don't claim that you wrote it
// and this copyright notice stays intact in the source files.
// If you use this class in commercial applications, please send a short message
// to tim.kosse@gmx.de
//---------------------------------------------------------------------------
#include "stdafx.h"
#include "AsyncSslSocketLayer.h"

#include <openssl/x509v3.h>
#include <openssl/err.h>

/////////////////////////////////////////////////////////////////////////////
// CAsyncSslSocketLayer
CCriticalSectionWrapper CAsyncSslSocketLayer::m_sCriticalSection;

CAsyncSslSocketLayer::t_SslLayerList* CAsyncSslSocketLayer::m_pSslLayerList = 0;
int CAsyncSslSocketLayer::m_nSslRefCount = 0;
std::map<SSL_CTX *, int> CAsyncSslSocketLayer::m_contextRefCount;

CAsyncSslSocketLayer::CAsyncSslSocketLayer()
{
  m_ssl = 0;
  m_sslbio = 0;
  m_ibio = 0;
  m_nbio = 0;
  m_ssl_ctx = 0;

  m_bUseSSL = false;
  m_bSslInitialized = FALSE;
  m_bSslEstablished = FALSE;
  m_nNetworkSendBufferLen = 0;
  m_nNetworkSendBufferMaxLen = 0;
  m_pNetworkSendBuffer = NULL;
  m_pRetrySendBuffer = 0;
  m_nRetrySendBufferLen = 0;
  m_nNetworkError = 0;
  m_nShutDown = 0;

  m_bBlocking = FALSE;
  m_nSslAsyncNotifyId = 0;
  m_bFailureSent = FALSE;
  m_nVerificationResult = 0;
  m_nVerificationDepth = 0;
  m_mayTriggerRead = true;
  m_mayTriggerWrite = true;
  m_mayTriggerReadUp = true;
  m_mayTriggerWriteUp = true;

  m_onCloseCalled = false;
  m_Main = NULL;
  m_sessionid = NULL;
  m_sessionreuse = true;

  FCertificate = NULL;
  FPrivateKey = NULL;
}

CAsyncSslSocketLayer::~CAsyncSslSocketLayer()
{
  UnloadSSL();
  delete [] m_pNetworkSendBuffer;
  delete [] m_pRetrySendBuffer;
}

int CAsyncSslSocketLayer::InitSSL()
{
  if (m_bSslInitialized)
    return 0;

  m_sCriticalSection.Lock();

  if (!m_nSslRefCount)
  {
    SSL_load_error_strings();
    if (!SSL_library_init())
    {
      return SSL_FAILURE_INITSSL;
    }
  }

  m_nSslRefCount++;
  m_sCriticalSection.Unlock();

  m_bSslInitialized = true;

  return 0;
}

void CAsyncSslSocketLayer::OnReceive(int nErrorCode)
{
  if (m_bUseSSL)
  {
    if (m_bBlocking)
    {
      m_mayTriggerRead = true;
      return;
    }
    if (m_nNetworkError)
    {
      return;
    }

    char buffer[16384];

    m_mayTriggerRead = false;

    //Get number of bytes we can receive and store in the network input bio
    int len = BIO_ctrl_get_write_guarantee(m_nbio);
    if (len > 16384)
      len = 16384;
    else if (!len)
    {
      m_mayTriggerRead = true;
      TriggerEvents();
      return;
    }

    int numread = 0;

    // Receive data
    numread = ReceiveNext(buffer, len);
    if (numread > 0)
    {
      //Store it in the network input bio and process data
      int numwritten = BIO_write(m_nbio, buffer, numread);
      BIO_ctrl(m_nbio, BIO_CTRL_FLUSH, 0, NULL);

      // I have no idea why this call is needed, but without it, connections
      // will stall. Perhaps it triggers some internal processing.
      // Also, ignore return value, don't do any error checking. This function
      // can report errors, even though a later call can succeed.
      char buffer;
      BIO_read(m_sslbio, &buffer, 0);
    }
    if (!numread)
    {
      if (GetLayerState() == connected)
        TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
    }
    else if (numread == SOCKET_ERROR)
    {
      int nError = GetLastError();
      if (nError != WSAEWOULDBLOCK && nError != WSAENOTCONN)
      {
        m_nNetworkError = GetLastError();
        TriggerEvent(FD_CLOSE, 0, TRUE);
        return;
      }
    }

    if (m_pRetrySendBuffer)
    {
      int numwrite = BIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
      if (numwrite >= 0)
      {
        BIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
        delete [] m_pRetrySendBuffer;
        m_pRetrySendBuffer = 0;
      }
      else if (numwrite == -1)
      {
        if (!BIO_should_retry(m_sslbio))
        {
          delete [] m_pRetrySendBuffer;
          m_pRetrySendBuffer = 0;

          ::SetLastError(WSAECONNABORTED);
          TriggerEvent(FD_CLOSE, 0, TRUE);
          return;
        }
      }
    }

    if (!m_nShutDown && SSL_get_shutdown(m_ssl))
    {
      size_t pending = BIO_ctrl_pending(m_sslbio);
      if (pending <= 0)
      {
        if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
        {
          if (ShutDownComplete())
            TriggerEvent(FD_CLOSE, 0, TRUE);
        }
        else
        {
          m_nNetworkError = WSAECONNABORTED;
          WSASetLastError(WSAECONNABORTED);
          TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
        }
        return;
      }
    }

    if (ShutDownComplete() && m_nShutDown == 1)
    {
      //Send shutdown notification if all pending data has been sent
      DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_SHUTDOWNCOMPLETE);
      m_nShutDown++;
    }

    TriggerEvents();
  }
  else
  {
    TriggerEvent(FD_READ, nErrorCode, TRUE);
  }
}

void CAsyncSslSocketLayer::OnSend(int nErrorCode)
{
  if (m_bUseSSL)
  {
    if (m_nNetworkError)
    {
      return;
    }

    m_mayTriggerWrite = false;

    //Send data in the send buffer
    while (m_nNetworkSendBufferLen)
    {
      int numsent = SendNext(m_pNetworkSendBuffer, m_nNetworkSendBufferLen);
      if (numsent == SOCKET_ERROR)
      {
        int nError = GetLastError();
        if (nError != WSAEWOULDBLOCK && nError != WSAENOTCONN)
        {
          m_nNetworkError = nError;
          TriggerEvent(FD_CLOSE, 0, TRUE);
        }
        return;
      }
      else if (!numsent)
      {
        if (GetLayerState() == connected)
          TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
      }
      if (numsent == m_nNetworkSendBufferLen)
      {
        m_nNetworkSendBufferLen = 0;
      }
      else
      {
        memmove(m_pNetworkSendBuffer, m_pNetworkSendBuffer + numsent, m_nNetworkSendBufferLen - numsent);
        m_nNetworkSendBufferLen -= numsent;
      }
    }

    //Send the data waiting in the network bio
    char buffer[32 * 1024];
    size_t len = BIO_ctrl_pending(m_nbio);
    int numread = BIO_read(m_nbio, buffer, std::min(len, sizeof(buffer)));
    if (numread <= 0)
      m_mayTriggerWrite = true;
    while (numread > 0)
    {
      int numsent = SendNext(buffer, numread);
      if (!numsent)
      {
        if (GetLayerState() == connected)
          TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
      }
      if (numsent == SOCKET_ERROR || numsent < numread)
      {
        if (numsent == SOCKET_ERROR)
          if (GetLastError() != WSAEWOULDBLOCK && GetLastError() != WSAENOTCONN)
          {
            m_nNetworkError = GetLastError();
            TriggerEvent(FD_CLOSE, 0, TRUE);
            return;
          }
          else
            numsent = 0;

        // Add all data that was retrieved from the network bio but could not be sent to the send buffer.
        if (m_nNetworkSendBufferMaxLen < (m_nNetworkSendBufferLen + numread - numsent))
        {
          char * tmp = m_pNetworkSendBuffer;
          m_nNetworkSendBufferMaxLen = static_cast<int>((m_nNetworkSendBufferLen + numread - numsent) * 1.5);
          m_pNetworkSendBuffer = new char[m_nNetworkSendBufferMaxLen];
          if (tmp)
          {
            memcpy(m_pNetworkSendBuffer, tmp, m_nNetworkSendBufferLen);
            delete [] tmp;
          }
        }
        DebugAssert(m_pNetworkSendBuffer);
        memcpy(m_pNetworkSendBuffer + m_nNetworkSendBufferLen, buffer, numread-numsent);
        m_nNetworkSendBufferLen += numread - numsent;
      }
      if (!numsent)
      {
        break;
      }
      len = BIO_ctrl_pending(m_nbio);
      if (!len)
      {
        m_mayTriggerWrite = true;
        break;
      }
      numread = BIO_read(m_nbio, buffer, len);
      if (numread <= 0)
      {
        m_mayTriggerWrite = true;
      }
    }

    if (m_pRetrySendBuffer)
    {
      int numwrite = BIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
      if (numwrite >= 0)
      {
        BIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
        delete [] m_pRetrySendBuffer;
        m_pRetrySendBuffer = 0;
      }
      else if (numwrite == -1)
      {
        if (!BIO_should_retry(m_sslbio))
        {
          delete [] m_pRetrySendBuffer;
          m_pRetrySendBuffer = 0;

          ::SetLastError(WSAECONNABORTED);
          TriggerEvent(FD_CLOSE, 0, TRUE);
          return;
        }
      }
    }

    // No more data available, ask for more.
    TriggerEvents();
    if (m_nShutDown == 1 && ShutDownComplete())
    {
      //Send shutdown notification if all pending data has been sent
            // FileZilla3 calls ShutDownNext() here
      DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_SHUTDOWNCOMPLETE);
      m_nShutDown++;
    }
  }
  else
  {
    TriggerEvent(FD_WRITE, nErrorCode, TRUE);
  }
}

int CAsyncSslSocketLayer::Send(const void* lpBuf, int nBufLen, int nFlags)
{
  if (m_bUseSSL)
  {
    if (!lpBuf)
    {
      return 0;
    }
    if (m_bBlocking || m_pRetrySendBuffer)
    {
      m_mayTriggerWriteUp = true;
      ::SetLastError(WSAEWOULDBLOCK);
      return SOCKET_ERROR;
    }
    if (m_nNetworkError)
    {
      SetLastError(m_nNetworkError);
      return SOCKET_ERROR;
    }
    if (m_nShutDown)
    {
      ::SetLastError(WSAESHUTDOWN);
      return SOCKET_ERROR;
    }
    if (!m_bSslEstablished)
    {
      m_mayTriggerWriteUp = true;
      ::SetLastError(WSAEWOULDBLOCK);
      return SOCKET_ERROR;
    }
    if (!nBufLen)
    {
      return 0;
    }

    if (m_onCloseCalled)
    {
      TriggerEvent(FD_CLOSE, 0, TRUE);
      return 0;
    }

    int len = BIO_ctrl_get_write_guarantee(m_sslbio);
    if (nBufLen > len)
      nBufLen = len;
    if (!len)
    {
      m_mayTriggerWriteUp = true;
      TriggerEvents();
      ::SetLastError(WSAEWOULDBLOCK);
    }

    m_pRetrySendBuffer = new char[nBufLen];
    m_nRetrySendBufferLen = nBufLen;
    memcpy(m_pRetrySendBuffer, lpBuf, nBufLen);

    int numwrite = BIO_write(m_sslbio, m_pRetrySendBuffer, m_nRetrySendBufferLen);
    if (numwrite >= 0)
    {
      BIO_ctrl(m_sslbio, BIO_CTRL_FLUSH, 0, NULL);
      delete [] m_pRetrySendBuffer;
      m_pRetrySendBuffer = 0;
    }
    else if (numwrite == -1)
    {
      if (BIO_should_retry(m_sslbio))
      {
        if (GetLayerState() == closed)
        {
          return 0;
        }
        else if (GetLayerState() != connected)
        {
          SetLastError(m_nNetworkError);
          return SOCKET_ERROR;
        }

        TriggerEvents();

        return nBufLen;
      }
      else
      {
        delete [] m_pRetrySendBuffer;
        m_pRetrySendBuffer = 0;

        ::SetLastError(WSAECONNABORTED);
      }
      return SOCKET_ERROR;
    }

    m_mayTriggerWriteUp = true;
    TriggerEvents();

    return numwrite;
  }
  else
  {
    return SendNext(lpBuf, nBufLen, nFlags);
  }
}

int CAsyncSslSocketLayer::Receive(void* lpBuf, int nBufLen, int nFlags)
{
  if (m_bUseSSL)
  {
    if (m_bBlocking)
    {
      m_mayTriggerReadUp = true;
      ::SetLastError(WSAEWOULDBLOCK);
      return SOCKET_ERROR;
    }
    if (m_nNetworkError)
    {
      size_t pending = BIO_ctrl_pending(m_sslbio);
      if (pending && !m_nShutDown)
      {
        m_mayTriggerReadUp = true;
        TriggerEvents();
        return BIO_read(m_sslbio, lpBuf,nBufLen);
      }
      WSASetLastError(m_nNetworkError);
      return SOCKET_ERROR;
    }
    if (m_nShutDown)
    {
      ::SetLastError(WSAESHUTDOWN);
      return SOCKET_ERROR;
    }
    if (!nBufLen)
    {
      return 0;
    }
    size_t pending = BIO_ctrl_pending(m_sslbio);
    if (!pending)
    {
      if (GetLayerState() == closed)
      {
        return 0;
      }
      if (m_onCloseCalled)
      {
        TriggerEvent(FD_CLOSE, 0, TRUE);
        return 0;
      }
      else if (GetLayerState() != connected)
      {
        SetLastError(m_nNetworkError);
        return SOCKET_ERROR;
      }
      else
      {
        if (SSL_get_shutdown(m_ssl))
        {
          if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
          {
            if (ShutDownComplete())
            {
              TriggerEvent(FD_CLOSE, 0, TRUE);
              return 0;
            }
            else
              WSASetLastError(WSAEWOULDBLOCK);
          }
          else
          {
            m_nNetworkError = WSAECONNABORTED;
            WSASetLastError(WSAECONNABORTED);
            TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
          }
          return SOCKET_ERROR;
        }
      }
      m_mayTriggerReadUp = true;
      TriggerEvents();
      ::SetLastError(WSAEWOULDBLOCK);
      return SOCKET_ERROR;
    }
    int numread = BIO_read(m_sslbio, lpBuf, nBufLen);
    if (!numread)
    {
      if (SSL_get_shutdown(m_ssl))
      {
        if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
        {
          if (ShutDownComplete())
          {
            TriggerEvent(FD_CLOSE, 0, TRUE);
            return 0;
          }
          else
            WSASetLastError(WSAEWOULDBLOCK);
        }
        else
        {
          m_nNetworkError = WSAECONNABORTED;
          WSASetLastError(WSAECONNABORTED);
          TriggerEvent(FD_CLOSE, WSAECONNABORTED, TRUE);
        }
        return SOCKET_ERROR;
      }
      m_mayTriggerReadUp = true;
      TriggerEvents();
      ::SetLastError(WSAEWOULDBLOCK);
      return SOCKET_ERROR;
    }
    if (numread < 0)
    {
      if (!BIO_should_retry(m_sslbio))
      {
        PrintLastErrorMsg();
        m_nNetworkError = WSAECONNABORTED;
        WSASetLastError(WSAECONNABORTED);
        TriggerEvent(FD_CLOSE, 0, TRUE);
        return SOCKET_ERROR;
      }
      else
      {
        if (SSL_get_shutdown(m_ssl))
        {
          if (ShutDown() || GetLastError() == WSAEWOULDBLOCK)
          {
            if (ShutDownComplete())
            {
              TriggerEvent(FD_CLOSE, 0, TRUE);
              return 0;
            }
            else
              WSASetLastError(WSAEWOULDBLOCK);
          }
          else
          {
            m_nNetworkError = WSAECONNABORTED;
            WSASetLastError(WSAECONNABORTED);
            TriggerEvent(FD_CLOSE, 0, TRUE);
          }
          return SOCKET_ERROR;
        }
        m_mayTriggerReadUp = true;
        TriggerEvents();
        ::SetLastError(WSAEWOULDBLOCK);
        return SOCKET_ERROR;
      }
    }

    m_mayTriggerReadUp = true;
    TriggerEvents();
    return numread;
  }
  else
  {
    return ReceiveNext(lpBuf, nBufLen, nFlags);
  }
}

void CAsyncSslSocketLayer::Close()
{
  m_nShutDown = 0;
  m_onCloseCalled = false;
  ResetSslSession();
  CloseNext();
}

BOOL CAsyncSslSocketLayer::Connect(const SOCKADDR *lpSockAddr, int nSockAddrLen)
{
  BOOL res = ConnectNext(lpSockAddr, nSockAddrLen);
  if (!res)
  {
    if (GetLastError() != WSAEWOULDBLOCK)
    {
      ResetSslSession();
    }
  }
  return res;
}

BOOL CAsyncSslSocketLayer::Connect(LPCTSTR lpszHostAddress, UINT nHostPort)
{
  BOOL res = ConnectNext(lpszHostAddress, nHostPort);
  if (!res)
  {
    if (GetLastError()!=WSAEWOULDBLOCK)
    {
      ResetSslSession();
    }
  }
  return res;
}

int CAsyncSslSocketLayer::InitSSLConnection(bool clientMode,
  CAsyncSslSocketLayer* main, bool sessionreuse,
  int minTlsVersion, int maxTlsVersion,
  void* pSslContext /*=0*/)
{
  if (m_bUseSSL)
    return 0;
  int res = InitSSL();
  if (res)
    return res;

  m_sCriticalSection.Lock();
  if ((SSL_CTX*)pSslContext)
  {
    if (m_ssl_ctx)
    {
      m_sCriticalSection.Unlock();
      ResetSslSession();
      return SSL_FAILURE_INITSSL;
    }

    std::map<SSL_CTX *, int>::iterator iter = m_contextRefCount.find((SSL_CTX*)pSslContext);
    if (iter == m_contextRefCount.end() || iter->second < 1)
    {
      m_sCriticalSection.Unlock();
      ResetSslSession();
      return SSL_FAILURE_INITSSL;
    }
    m_ssl_ctx = (SSL_CTX*)pSslContext;
    iter->second++;
  }
  else if (!m_ssl_ctx)
  {
    // Create new context if none given
    if (!(m_ssl_ctx = SSL_CTX_new( SSLv23_method())))
    {
      m_sCriticalSection.Unlock();
      ResetSslSession();
      return SSL_FAILURE_INITSSL;
    }
    m_contextRefCount[m_ssl_ctx] = 1;

    if (clientMode)
    {
      USES_CONVERSION;
      SSL_CTX_set_verify(m_ssl_ctx, SSL_VERIFY_PEER, verify_callback);
      SSL_CTX_set_client_cert_cb(m_ssl_ctx, ProvideClientCert);
      CFileStatus Dummy;
      if (CFile::GetStatus((LPCTSTR)m_CertStorage, Dummy))
      {
        SSL_CTX_load_verify_locations(m_ssl_ctx, T2CA(m_CertStorage), 0);
      }
    }
  }

  //Create new SSL session
  if (!(m_ssl = SSL_new(m_ssl_ctx)))
  {
    m_sCriticalSection.Unlock();
    ResetSslSession();
    return SSL_FAILURE_INITSSL;
  }

#ifdef _DEBUG
  if ((main == NULL) && LoggingSocketMessage(FZ_LOG_INFO))
  {
    USES_CONVERSION;
    LogSocketMessageRaw(FZ_LOG_INFO, L"Supported ciphersuites:");
    STACK_OF(SSL_CIPHER) * ciphers = SSL_get_ciphers(m_ssl);
    for (int i = 0; i < sk_SSL_CIPHER_num(ciphers); i++)
    {
      SSL_CIPHER * cipher = sk_SSL_CIPHER_value(ciphers, i);
      LogSocketMessageRaw(FZ_LOG_INFO, A2CT(cipher->name));
    }
  }
#endif

  //Add current instance to list of active instances
  t_SslLayerList *tmp = m_pSslLayerList;
  m_pSslLayerList = new t_SslLayerList;
  m_pSslLayerList->pNext = tmp;
  m_pSslLayerList->pLayer = this;
  m_sCriticalSection.Unlock();

  SSL_set_info_callback(m_ssl, apps_ssl_info_callback);

  //Create bios
  m_sslbio = BIO_new(BIO_f_ssl());
  BIO_new_bio_pair(&m_ibio, 32768, &m_nbio, 32768);

  if (!m_sslbio || !m_nbio || !m_ibio)
  {
    ResetSslSession();
    return SSL_FAILURE_INITSSL;
  }

  // See also TWebDAVFileSystem::InitSslSession
  #define MASK_TLS_VERSION(VERSION, FLAG) ((minTlsVersion > VERSION) || (maxTlsVersion < VERSION) ? FLAG : 0)
  long options =
    SSL_OP_ALL |
    MASK_TLS_VERSION(SSL_VERSION_SSL2, SSL_OP_NO_SSLv2) |
    MASK_TLS_VERSION(SSL_VERSION_SSL3, SSL_OP_NO_SSLv3) |
    MASK_TLS_VERSION(SSL_VERSION_TLS10, SSL_OP_NO_TLSv1) |
    MASK_TLS_VERSION(SSL_VERSION_TLS11, SSL_OP_NO_TLSv1_1) |
    MASK_TLS_VERSION(SSL_VERSION_TLS12, SSL_OP_NO_TLSv1_2);
  // SSL_ctrl() with SSL_CTRL_OPTIONS adds flags (not sets)
  SSL_ctrl(m_ssl, SSL_CTRL_OPTIONS, options, NULL);

  //Init SSL connection
  void *ssl_sessionid = NULL;
  m_Main = main;
  m_sessionreuse = sessionreuse;
  if ((m_Main != NULL) && m_sessionreuse)
  {
    if (m_Main->m_sessionid != NULL)
    {
      if (!SSL_set_session(m_ssl, m_Main->m_sessionid))
      {
        LogSocketMessageRaw(FZ_LOG_INFO, L"SSL_set_session failed");
        return SSL_FAILURE_INITSSL;
      }
      LogSocketMessageRaw(FZ_LOG_INFO, L"Trying reuse main TLS session ID");
    }
    else
    {
      LogSocketMessageRaw(FZ_LOG_INFO, L"Main TLS session ID was not reused previously, not trying again");
      SSL_set_session(m_ssl, NULL);
    }
  }
  else
  {
    SSL_set_session(m_ssl, NULL);
  }
  if (clientMode)
  {
    SSL_set_connect_state(m_ssl);
  }
  else
  {
    SSL_set_accept_state(m_ssl);
  }
  SSL_set_bio(m_ssl, m_ibio, m_ibio);
  BIO_ctrl(m_sslbio, BIO_C_SET_SSL, BIO_NOCLOSE, m_ssl);
  BIO_read(m_sslbio, (void *)1, 0);

  // Trigger FD_WRITE so that we can initialize SSL negotiation
  if (GetLayerState() == connected || GetLayerState() == attached)
  {
    TriggerEvent(FD_READ, 0);
    TriggerEvent(FD_WRITE, 0);
    TriggerEvent(FD_READ, 0, TRUE);
    TriggerEvent(FD_WRITE, 0, TRUE);
  }

  m_bUseSSL = true;

  return 0;
}

void CAsyncSslSocketLayer::ResetSslSession()
{
  if (m_pRetrySendBuffer)
  {
    delete [] m_pRetrySendBuffer;
    m_pRetrySendBuffer = 0;
  }

  m_bFailureSent = FALSE;
  m_bBlocking = FALSE;
  m_nSslAsyncNotifyId++;
  m_nNetworkError = 0;
  m_bUseSSL = FALSE;
  m_nVerificationResult = 0;
  m_nVerificationDepth = 0;

  m_bSslEstablished = FALSE;
  if (m_sslbio)
  {
    BIO_free(m_sslbio);
  }
  if (m_ssl)
  {
    SSL_set_session(m_ssl,NULL);
  }
  if (m_nbio)
  {
    BIO_free(m_nbio);
  }
  if (m_ibio)
  {
    BIO_free(m_ibio);
  }

  m_nNetworkSendBufferLen = 0;

  m_nbio = 0;
  m_ibio = 0;
  m_sslbio = 0;

  if (m_ssl)
  {
    SSL_free(m_ssl);
  }

  m_sCriticalSection.Lock();

  if (m_ssl_ctx)
  {
    std::map<SSL_CTX *, int>::iterator iter = m_contextRefCount.find(m_ssl_ctx);
    if (iter != m_contextRefCount.end())
    {
      if (iter->second <= 1)
      {
        SSL_CTX_free(m_ssl_ctx);
        m_contextRefCount.erase(iter);
      }
      else
        iter->second--;
    }
    m_ssl_ctx = 0;
  }

  m_ssl = 0;
  t_SslLayerList *cur = m_pSslLayerList;
  if (!cur)
  {
    m_sCriticalSection.Unlock();
    return;
  }

  if (cur->pLayer == this)
  {
    m_pSslLayerList = cur->pNext;
    delete cur;
  }
  else
    while (cur->pNext)
    {
      if (cur->pNext->pLayer == this)
      {
        t_SslLayerList *tmp = cur->pNext;
        cur->pNext = cur->pNext->pNext;
        delete tmp;

        m_sCriticalSection.Unlock();
        return;
      }
      cur = cur->pNext;
    }

  if (m_sessionid != NULL)
  {
    SSL_SESSION_free(m_sessionid);
    m_sessionid = NULL;
  }
  m_sessionreuse = true;

  m_sCriticalSection.Unlock();
}

bool CAsyncSslSocketLayer::IsUsingSSL()
{
  return m_bUseSSL;
}

BOOL CAsyncSslSocketLayer::ShutDown(int nHow /*=sends*/)
{
  if (m_bUseSSL)
  {
    if (m_pRetrySendBuffer)
    {
      if (!m_nShutDown)
        m_nShutDown = 1;
      WSASetLastError(WSAEWOULDBLOCK);
      return false;
    }
    if (!m_bSslEstablished)
    {
      m_mayTriggerWriteUp = true;
      ::SetLastError(WSAEWOULDBLOCK);
      return false;
    }
    if (!m_nShutDown)
      m_nShutDown = 1;
    else
    {
      if (ShutDownComplete())
        return ShutDownNext();
      else
      {
        TriggerEvents();
        WSASetLastError(WSAEWOULDBLOCK);
        return false;
      }
    }

    int res = SSL_shutdown(m_ssl);
    if (res != -1)
    {
      if (!res)
      {
        SSL_shutdown(m_ssl);
      }
      if (ShutDownComplete())
        return ShutDownNext();
      else
      {
        TriggerEvents();
        WSASetLastError(WSAEWOULDBLOCK);
        return FALSE;
      }
    }
    else
    {
      int error = SSL_get_error(m_ssl, -1);
      if (error == SSL_ERROR_WANT_READ || error == SSL_ERROR_WANT_WRITE)
      {
        // retry shutdown later
        m_nShutDown = 0;
        TriggerEvents();
        WSASetLastError(WSAEWOULDBLOCK);
        return FALSE;
      }
      else if (ShutDownComplete())
        return ShutDownNext();
      else
      {
        TriggerEvents();
        WSASetLastError(WSAEWOULDBLOCK);
        return FALSE;
      }
    }
  }
  else
  {
    if (!m_nShutDown)
      m_nShutDown = 1;
    return ShutDownNext(nHow);
  }
}

BOOL CAsyncSslSocketLayer::ShutDownComplete()
{
  //If a ShutDown was issued, has the connection already been shut down?
  if (!m_nShutDown)
    return FALSE;
  else if (!m_bUseSSL)
    return FALSE;
  else if (m_nNetworkSendBufferLen)
    return FALSE;
  else if (m_pRetrySendBuffer)
    return FALSE;

  // Empty read buffer
  char buffer[1000];
  int numread;
  do
  {
    numread = BIO_read(m_sslbio, buffer, 1000);
  } while (numread > 0);

  size_t pending = BIO_ctrl_pending(m_nbio);
  if (pending)
  {
    return FALSE;
  }
  else
  {
    return TRUE;
  }
}

void CAsyncSslSocketLayer::apps_ssl_info_callback(const SSL *s, int where, int ret)
{
  USES_CONVERSION;
  CAsyncSslSocketLayer *pLayer = 0;
  m_sCriticalSection.Lock();
  t_SslLayerList *cur = m_pSslLayerList;
  while (cur)
  {
    if (cur->pLayer->m_ssl == s)
      break;
    cur = cur->pNext;
  }
  if (!cur)
  {
    m_sCriticalSection.Unlock();
    MessageBox(0, L"Can't lookup TLS session!", L"Critical error", MB_ICONEXCLAMATION);
    return;
  }
  else
    pLayer = cur->pLayer;
  m_sCriticalSection.Unlock();

  // Called while unloading?
  if (!pLayer->m_bUseSSL && (where != SSL_CB_LOOP))
    return;

  char * str;
  int w;

  w = where& ~SSL_ST_MASK;

  if (w & SSL_ST_CONNECT)
  {
    str = "TLS connect";
    if (pLayer->m_sessionreuse)
    {
      SSL_SESSION * sessionid = SSL_get1_session(pLayer->m_ssl);
      if (pLayer->m_sessionid != sessionid)
      {
        if (pLayer->m_sessionid == NULL)
        {
          if (SSL_session_reused(pLayer->m_ssl))
          {
            pLayer->LogSocketMessageRaw(FZ_LOG_PROGRESS, L"Session ID reused");
          }
          else
          {
            if ((pLayer->m_Main != NULL) && (pLayer->m_Main->m_sessionid != NULL))
            {
              pLayer->LogSocketMessageRaw(FZ_LOG_INFO, L"Main TLS session ID not reused, will not try again");
              SSL_SESSION_free(pLayer->m_Main->m_sessionid);
              pLayer->m_Main->m_sessionid = NULL;
            }
          }
          pLayer->LogSocketMessageRaw(FZ_LOG_DEBUG, L"Saving session ID");
        }
        else
        {
          SSL_SESSION_free(pLayer->m_sessionid);
          pLayer->LogSocketMessageRaw(FZ_LOG_INFO, L"Session ID changed");
        }
        pLayer->m_sessionid = sessionid;
      }
      else
      {
        SSL_SESSION_free(sessionid);
      }
    }
  }
  else if (w & SSL_ST_ACCEPT)
    str = "TLS accept";
  else
    str = "Undefined";

  if (where & SSL_CB_LOOP)
  {
    char* debug = NULL;
    // exact SSL_CB_LOOP is abused for debugging
    if (where == SSL_CB_LOOP)
    {
      debug = reinterpret_cast<char*>(ret);
    }
    char *buffer = new char[4096 + ((debug != NULL) ? strlen(debug) : 0)];
    sprintf(buffer, "%s: %s",
        str,
        SSL_state_string_long(s));
    if (debug != NULL)
    {
      sprintf(buffer + strlen(buffer), " [%s]", debug);
      OPENSSL_free(debug);
    }
    USES_CONVERSION;
    pLayer->LogSocketMessageRaw(FZ_LOG_INFO, A2T(buffer));
    delete[] buffer;
  }
  else if (where & SSL_CB_ALERT)
  {
    str=(where & SSL_CB_READ)? "read" : "write";
    const char* desc = SSL_alert_desc_string_long(ret);

    // Don't send close notify warning
    if (desc)
    {
      if (strcmp(desc, "close notify"))
      {
        char *buffer = new char[4096];
        sprintf(buffer, "SSL3 alert %s: %s: %s",
            str,
            SSL_alert_type_string_long(ret),
            desc);
        pLayer->LogSocketMessageRaw(FZ_LOG_WARNING, A2T(buffer));
        pLayer->PrintLastErrorMsg();
        delete [] buffer;
      }
    }
  }

  else if (where & SSL_CB_EXIT)
  {
    if (ret == 0)
    {
      char *buffer = new char[4096];
      sprintf(buffer, "%s: failed in %s",
          str,
          SSL_state_string_long(s));
      pLayer->LogSocketMessageRaw(FZ_LOG_WARNING, A2T(buffer));
      pLayer->PrintLastErrorMsg();
      delete [] buffer;
      if (!pLayer->m_bFailureSent)
      {
        pLayer->m_bFailureSent=TRUE;
        pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, pLayer->m_bSslEstablished ? SSL_FAILURE_UNKNOWN : SSL_FAILURE_ESTABLISH);
      }
    }
    else if (ret < 0)
    {
      int error = SSL_get_error(s,ret);
      if (error != SSL_ERROR_WANT_READ && error != SSL_ERROR_WANT_WRITE)
      {
        char *buffer = new char[4096];
        sprintf(buffer, "%s: error in %s",
            str,
            SSL_state_string_long(s));
        pLayer->LogSocketMessageRaw(FZ_LOG_WARNING, A2T(buffer));
        delete [] buffer;
        if (!pLayer->m_bFailureSent)
        {
          pLayer->m_bFailureSent=TRUE;
          pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, pLayer->m_bSslEstablished ? SSL_FAILURE_UNKNOWN : SSL_FAILURE_ESTABLISH);
        }
      }
    }
  }
  if (where & SSL_CB_HANDSHAKE_DONE)
  {
    int error = SSL_get_verify_result(pLayer->m_ssl);
    pLayer->DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_VERIFY_CERT, error);
    pLayer->m_bBlocking = TRUE;
  }
}


void CAsyncSslSocketLayer::UnloadSSL()
{
  if (!m_bSslInitialized)
    return;
  ResetSslSession();

  m_bSslInitialized = false;

  m_sCriticalSection.Lock();
  m_nSslRefCount--;
  if (m_nSslRefCount)
  {
    m_sCriticalSection.Unlock();
    return;
  }

  m_sCriticalSection.Unlock();
}

bool AsnTimeToValidTime(ASN1_TIME * AsnTime, t_SslCertData::t_validTime & ValidTime)
{
  int i = AsnTime->length;
  const char * v = (const char *)AsnTime->data;

  if (i < 10)
  {
    return FALSE;
  }

  for (int i2 = 0; i2 < 10; i2++)
  {
    if ((v[i2] > '9') || (v[i2] < '0'))
    {
      return FALSE;
    }
  }

  if (AsnTime->type == V_ASN1_UTCTIME)
  {
    ValidTime.y= (v[0]-'0')*10+(v[1]-'0');
    if (ValidTime.y < 50) ValidTime.y+=100;
    ValidTime.y += 1900;
    v += 2;
    i -= 2;
  }
  else if (AsnTime->type == V_ASN1_GENERALIZEDTIME)
  {
    if (i < 12)
    {
      return FALSE;
    }
    ValidTime.y = (v[0]-'0')*1000+(v[1]-'0')*100 + (v[2]-'0')*10+(v[3]-'0');
    v += 4;
    i -= 4;
  }
  else
  {
    return FALSE;
  }

  ValidTime.M = (v[0]-'0')*10+(v[1]-'0');
  if ((ValidTime.M > 12) || (ValidTime.M < 1))
  {
    return FALSE;
  }

  ValidTime.d = (v[2]-'0')*10+(v[3]-'0');
  ValidTime.h = (v[4]-'0')*10+(v[5]-'0');
  ValidTime.m =  (v[6]-'0')*10+(v[7]-'0');

  if ((i >= 10) &&
      (v[8] >= '0') && (v[8] <= '9') &&
      (v[9] >= '0') && (v[9] <= '9'))
  {
    ValidTime.s = (v[8]-'0')*10+(v[9]-'0');
  }
  else
  {
    ValidTime.s = 0;
  }

  return TRUE;
}

BOOL CAsyncSslSocketLayer::GetPeerCertificateData(t_SslCertData &SslCertData, LPCTSTR & Error)
{
  X509 *pX509=SSL_get_peer_certificate(m_ssl);
  if (!pX509)
  {
    Error = L"Cannot get certificate";
    return FALSE;
  }

  //Reset the contents of SslCertData
  memset(&SslCertData, 0, sizeof(t_SslCertData));

  //Set subject data fields
  X509_NAME *pX509Name=X509_get_subject_name(pX509);

  if (pX509Name)
  {
    int count=X509_NAME_entry_count(pX509Name);
    for (int i=0;i<count;i++)
    {
      X509_NAME_ENTRY *pX509NameEntry=X509_NAME_get_entry(pX509Name,i);
      if (!pX509NameEntry)
        continue;
      ASN1_OBJECT *pObject = X509_NAME_ENTRY_get_object(pX509NameEntry);
      ASN1_STRING *pString = X509_NAME_ENTRY_get_data(pX509NameEntry);
      CString str;

      unsigned char *out;
      int len = ASN1_STRING_to_UTF8(&out, pString);
      if (len > 0)
      {
        // Keep it huge
        LPWSTR unicode = new WCHAR[len * 10];
        memset(unicode, 0, sizeof(WCHAR) * len * 10);
        int unicodeLen = MultiByteToWideChar(CP_UTF8, 0, (const char *)out, len, unicode, len * 10);
        if (unicodeLen > 0)
        {
#ifdef _UNICODE
          str = unicode;
#else
          LPSTR ansi = new CHAR[len * 10];
          memset(ansi, 0, sizeof(CHAR) * len * 10);
          int ansiLen = WideCharToMultiByte(CP_ACP, 0, unicode, unicodeLen, ansi, len * 10, 0, 0);
          if (ansiLen > 0)
            str = ansi;

          delete [] ansi;
#endif
        }
        delete [] unicode;
        CRYPTO_free(out);
      }

      switch(OBJ_obj2nid(pObject))
      {
      case NID_organizationName:
        _tcsncpy(SslCertData.subject.Organization, str, 255);
        SslCertData.subject.Organization[255] = 0;
        break;
      case NID_organizationalUnitName:
        _tcsncpy(SslCertData.subject.Unit, str, 255);
        SslCertData.subject.Unit[255] = 0;
        break;
      case NID_commonName:
        _tcsncpy(SslCertData.subject.CommonName, str, 255);
        SslCertData.subject.CommonName[255] = 0;
        break;
      case NID_pkcs9_emailAddress:
        _tcsncpy(SslCertData.subject.Mail, str, 255);
        SslCertData.subject.Mail[255] = 0;
        break;
      case NID_countryName:
        _tcsncpy(SslCertData.subject.Country, str, 255);
        SslCertData.subject.Country[255] = 0;
        break;
      case NID_stateOrProvinceName:
        _tcsncpy(SslCertData.subject.StateProvince, str, 255);
        SslCertData.subject.StateProvince[255] = 0;
        break;
      case NID_localityName:
        _tcsncpy(SslCertData.subject.Town, str, 255);
        SslCertData.subject.Town[255] = 0;
        break;
      default:
        if ( !OBJ_nid2sn(OBJ_obj2nid(pObject)) )
        {
          TCHAR tmp[20];
          _stprintf(tmp, L"%d", OBJ_obj2nid(pObject));
          int maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), tmp, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), L"=", maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), str, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), L";", maxlen);
        }
        else
        {
          int maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;

          USES_CONVERSION;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), A2CT(OBJ_nid2sn(OBJ_obj2nid(pObject))), maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), L"=", maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), str, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.subject.Other)-1;
          _tcsncpy(SslCertData.subject.Other+_tcslen(SslCertData.subject.Other), L";", maxlen);
        }
        break;
      }
    }
  }

  //Set issuer data fields
  pX509Name=X509_get_issuer_name(pX509);
  if (pX509Name)
  {
    int count=X509_NAME_entry_count(pX509Name);
    for (int i=0;i<count;i++)
    {
      X509_NAME_ENTRY *pX509NameEntry=X509_NAME_get_entry(pX509Name,i);
      if (!pX509NameEntry)
        continue;
      ASN1_STRING *pString=X509_NAME_ENTRY_get_data(pX509NameEntry);
      ASN1_OBJECT *pObject=X509_NAME_ENTRY_get_object(pX509NameEntry);

      CString str;

      unsigned char *out;
      int len = ASN1_STRING_to_UTF8(&out, pString);
      if (len > 0)
      {
        // Keep it huge
        LPWSTR unicode = new WCHAR[len * 10];
        memset(unicode, 0, sizeof(WCHAR) * len * 10);
        int unicodeLen = MultiByteToWideChar(CP_UTF8, 0, (const char *)out, len, unicode, len * 10);
        if (unicodeLen > 0)
        {
#ifdef _UNICODE
          str = unicode;
#else
          LPSTR ansi = new CHAR[len * 10];
          memset(ansi, 0, sizeof(CHAR) * len * 10);
          int ansiLen = WideCharToMultiByte(CP_ACP, 0, unicode, unicodeLen, ansi, len * 10, 0, 0);
          if (ansiLen > 0)
            str = ansi;

          delete [] ansi;
#endif
        }
        delete [] unicode;
        CRYPTO_free(out);
      }

      switch(OBJ_obj2nid(pObject))
      {
      case NID_organizationName:
        _tcsncpy(SslCertData.issuer.Organization, str, 255);
        SslCertData.issuer.Organization[255] = 0;
        break;
      case NID_organizationalUnitName:
        _tcsncpy(SslCertData.issuer.Unit, str, 255);
        SslCertData.issuer.Unit[255] = 0;
        break;
      case NID_commonName:
        _tcsncpy(SslCertData.issuer.CommonName, str, 255);
        SslCertData.issuer.CommonName[255] = 0;
        break;
      case NID_pkcs9_emailAddress:
        _tcsncpy(SslCertData.issuer.Mail, str, 255);
        SslCertData.issuer.Mail[255] = 0;
        break;
      case NID_countryName:
        _tcsncpy(SslCertData.issuer.Country, str, 255);
        SslCertData.issuer.Country[255] = 0;
        break;
      case NID_stateOrProvinceName:
        _tcsncpy(SslCertData.issuer.StateProvince, str, 255);
        SslCertData.issuer.StateProvince[255] = 0;
        break;
      case NID_localityName:
        _tcsncpy(SslCertData.issuer.Town, str, 255);
        SslCertData.issuer.Town[255] = 0;
        break;
      default:
        if ( !OBJ_nid2sn(OBJ_obj2nid(pObject)) )
        {
          TCHAR tmp[20];
          _stprintf(tmp, L"%d", OBJ_obj2nid(pObject));
          int maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), tmp, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), L"=", maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), str, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), L";", maxlen);
        }
        else
        {
          int maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;

          USES_CONVERSION;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), A2CT(OBJ_nid2sn(OBJ_obj2nid(pObject))), maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), L"=", maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), str, maxlen);

          maxlen = 1024 - _tcslen(SslCertData.issuer.Other)-1;
          _tcsncpy(SslCertData.issuer.Other+_tcslen(SslCertData.issuer.Other), L";", maxlen);
        }
        break;
      }
    }
  }

  //Set date fields

  //Valid from
  ASN1_TIME *pTime=X509_get_notBefore(pX509);
  if (!pTime)
  {
    X509_free(pX509);
    Error = L"Cannot get start time";
    return FALSE;
  }

  if (!AsnTimeToValidTime(pTime, SslCertData.validFrom))
  {
    X509_free(pX509);
    Error = L"Invalid start time";
    return FALSE;
  }

  //Valid until
  pTime = X509_get_notAfter(pX509);
  if (!pTime)
  {
    X509_free(pX509);
    Error = L"Cannot get end time";
    return FALSE;
  }

  if (!AsnTimeToValidTime(pTime, SslCertData.validUntil))
  {
    X509_free(pX509);
    Error = L"Invalid end time";
    return FALSE;
  }

  int subjectAltNamePos = X509_get_ext_by_NID(pX509, NID_subject_alt_name, -1);
  if (subjectAltNamePos >= 0)
  {
    X509_EXTENSION * subjectAltNameExtension = X509_get_ext(pX509, subjectAltNamePos);
    BIO * subjectAltNameBio = BIO_new(BIO_s_mem());

    if (X509V3_EXT_print(subjectAltNameBio, subjectAltNameExtension, 0, 0) == 1)
    {
      USES_CONVERSION;
      u_char *data;
      int len = BIO_get_mem_data(subjectAltNameBio, &data);
      char * buf = new char[len + 1];
      memcpy(buf, data, len);
      buf[len] = '\0';
      _tcsncpy(SslCertData.subjectAltName, A2CT(buf), LENOF(SslCertData.subjectAltName));
      SslCertData.subjectAltName[LENOF(SslCertData.subjectAltName) - 1] = '\0';
      delete [] buf;
    }

    BIO_vfree(subjectAltNameBio);
  }

  unsigned int length = 20;
  X509_digest(pX509, EVP_sha1(), SslCertData.hash, &length);

  // Inspired by ne_ssl_cert_export()
  // Find the length of the DER encoding.
  SslCertData.certificateLen = i2d_X509(pX509, NULL);
  SslCertData.certificate = new unsigned char[SslCertData.certificateLen];
  unsigned char * p = SslCertData.certificate;
  i2d_X509(pX509, &p);

  SslCertData.priv_data = m_nSslAsyncNotifyId;

  X509_free(pX509);

  SslCertData.verificationResult = m_nVerificationResult;
  SslCertData.verificationDepth = m_nVerificationDepth;

  return TRUE;
}

std::string CAsyncSslSocketLayer::GetTlsVersionStr()
{
  return m_TlsVersionStr;
}

std::string CAsyncSslSocketLayer::GetCipherName()
{
  return m_CipherName;
}

void CAsyncSslSocketLayer::SetNotifyReply(int nID, int nCode, int result)
{
  if (!m_bBlocking)
    return;
  if (nID != m_nSslAsyncNotifyId)
    return;
  if (nCode != SSL_VERIFY_CERT)
    return;

  m_bBlocking = FALSE;

  if (!result)
  {
    m_nNetworkError = WSAECONNABORTED;
    WSASetLastError(WSAECONNABORTED);
    if (!m_bFailureSent)
    {
      m_bFailureSent = TRUE;
      DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_FAILURE, SSL_FAILURE_CERTREJECTED);
    }
    TriggerEvent(FD_CLOSE, 0, TRUE);
    return;
  }
  m_bSslEstablished = TRUE;
  PrintSessionInfo();
  DoLayerCallback(LAYERCALLBACK_LAYERSPECIFIC, SSL_INFO, SSL_INFO_ESTABLISHED);

  TriggerEvents();
}

void CAsyncSslSocketLayer::PrintSessionInfo()
{
  const SSL_CIPHER *ciph;
  X509 *cert;

  ciph = SSL_get_current_cipher(m_ssl);
  char enc[4096] = {0};
  cert=SSL_get_peer_certificate(m_ssl);

  if (cert != NULL)
  {
    EVP_PKEY *pkey = X509_get_pubkey(cert);
    if (pkey != NULL)
    {
      if (0)
        ;
#ifndef NO_RSA
      else if (pkey->type == EVP_PKEY_RSA && pkey->pkey.rsa != NULL
        && pkey->pkey.rsa->n != NULL)
        sprintf(enc,  "%d bit RSA", BN_num_bits(pkey->pkey.rsa->n));
#endif
#ifndef NO_DSA
      else if (pkey->type == EVP_PKEY_DSA && pkey->pkey.dsa != NULL
          && pkey->pkey.dsa->p != NULL)
        sprintf(enc,  "%d bit DSA", BN_num_bits(pkey->pkey.dsa->p));
#endif
      EVP_PKEY_free(pkey);
    }
    X509_free(cert);
    /* The SSL API does not allow us to look at temporary RSA/DH keys,
     * otherwise we should print their lengths too */
  }

  const int buffer_size = 4096;
  char *buffer = new char[buffer_size];
  char *buffer2 = new char[buffer_size];
  // see also ne_ssl_get_version and ne_ssl_get_cipher
  m_TlsVersionStr = SSL_get_version(m_ssl);
  sprintf(buffer, "%s: %s, %s, %s",
      SSL_CIPHER_get_version(ciph),
      SSL_CIPHER_get_name(ciph),
      enc,
      SSL_CIPHER_description(ciph, buffer2, buffer_size));
  m_CipherName = buffer;
  // see TWebDAVFileSystem::CollectTLSSessionInfo()
  sprintf(buffer, "Using %s, cipher %s",
      m_TlsVersionStr.c_str(),
      m_CipherName.c_str());
  USES_CONVERSION;
  LogSocketMessageRaw(FZ_LOG_PROGRESS, A2T(buffer));
  delete [] buffer;
  delete [] buffer2;
}

void CAsyncSslSocketLayer::OnConnect(int nErrorCode)
{
  if (m_bUseSSL && nErrorCode)
    TriggerEvent(FD_WRITE, 0);
  TriggerEvent(FD_CONNECT, nErrorCode, TRUE);
}

CAsyncSslSocketLayer * CAsyncSslSocketLayer::LookupLayer(SSL * Ssl)
{
  CAsyncSslSocketLayer * Result = NULL;
  m_sCriticalSection.Lock();
  t_SslLayerList * Cur = m_pSslLayerList;
  while (Cur != NULL)
  {
    if (Cur->pLayer->m_ssl == Ssl)
    {
      break;
    }
    Cur = Cur->pNext;
  }
  m_sCriticalSection.Unlock();

  if (Cur == NULL)
  {
    MessageBox(0, L"Can't lookup TLS session!", L"Critical error", MB_ICONEXCLAMATION);
    Result = NULL;
  }
  else
  {
    Result = Cur->pLayer;
  }

  return Result;
}

int CAsyncSslSocketLayer::verify_callback(int preverify_ok, X509_STORE_CTX *ctx)
{
  X509   *err_cert;
    int     err, depth;
    SSL    *ssl;

    err_cert = X509_STORE_CTX_get_current_cert(ctx);
    err = X509_STORE_CTX_get_error(ctx);
    depth = X509_STORE_CTX_get_error_depth(ctx);

    /*
     * Retrieve the pointer to the SSL of the connection currently treated
     * and the application specific data stored into the SSL object.
     */
    ssl = (SSL *)X509_STORE_CTX_get_ex_data(ctx, SSL_get_ex_data_X509_STORE_CTX_idx());


    CAsyncSslSocketLayer * pLayer = LookupLayer(ssl);

    if (pLayer == NULL)
    {
      return 1;
    }

    /*
     * Catch a too long certificate chain. The depth limit set using
     * SSL_CTX_set_verify_depth() is by purpose set to "limit+1" so
     * that whenever the "depth>verify_depth" condition is met, we
     * have violated the limit and want to log this error condition.
     * We must do it here, because the CHAIN_TOO_LONG error would not
     * be found explicitly; only errors introduced by cutting off the
     * additional certificates would be logged.
     */
    if (depth > 10) {//mydata->verify_depth) {
        preverify_ok = 0;
        err = X509_V_ERR_CERT_CHAIN_TOO_LONG;
        X509_STORE_CTX_set_error(ctx, err);
    }

  if (!preverify_ok)
  {
    if (!pLayer->m_nVerificationResult)
    {
      pLayer->m_nVerificationDepth = depth;
      pLayer->m_nVerificationResult = err;
    }
  }
  return 1;
}

int CAsyncSslSocketLayer::ProvideClientCert(
  SSL * Ssl, X509 ** Certificate, EVP_PKEY ** PrivateKey)
{
  CAsyncSslSocketLayer * Layer = LookupLayer(Ssl);

  USES_CONVERSION;
  CString Message;
  Message.LoadString(NEED_CLIENT_CERTIFICATE);
  char * Buffer = new char[Message.GetLength() + 1];
  strcpy(Buffer, T2A(Message));

  int Level;
  int Result;
  if ((Layer->FCertificate == NULL) || (Layer->FPrivateKey == NULL))
  {
    Level = FZ_LOG_WARNING;
    Result = 0;
  }
  else
  {
    Level = FZ_LOG_PROGRESS;
    *Certificate = X509_dup(Layer->FCertificate);
    CRYPTO_add(&Layer->FPrivateKey->references, 1, CRYPTO_LOCK_EVP_PKEY);
    *PrivateKey = Layer->FPrivateKey;
    Result = 1;
  }

  Layer->LogSocketMessageRaw(Level, A2T(Buffer));
  delete [] Buffer;

  return Result;
}

void CAsyncSslSocketLayer::SetClientCertificate(X509 * Certificate, EVP_PKEY * PrivateKey)
{
  FCertificate = Certificate;
  FPrivateKey = PrivateKey;
}

BOOL CAsyncSslSocketLayer::SetCertStorage(CString file)
{
  m_CertStorage = file;
  return TRUE;
}

void CAsyncSslSocketLayer::OnClose(int nErrorCode)
{
  m_onCloseCalled = true;
  if (m_bUseSSL && BIO_ctrl)
  {
    size_t pending = BIO_ctrl_pending(m_sslbio);
    if (pending > 0)
    {
      TriggerEvents();
    }
    else TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
  }
  else
    TriggerEvent(FD_CLOSE, nErrorCode, TRUE);
}

void CAsyncSslSocketLayer::PrintLastErrorMsg()
{
  int err = ERR_get_error();
  while (err)
  {
    char *buffer = new char[512];
    const char *reason = ERR_reason_error_string(err);
    ERR_error_string(err, buffer);
    err = ERR_get_error();
    USES_CONVERSION;
    LogSocketMessageRaw(FZ_LOG_PROGRESS, A2T(buffer));
    LogSocketMessageRaw(FZ_LOG_WARNING, A2T(reason));
    delete [] buffer;
  }
}

void CAsyncSslSocketLayer::TriggerEvents()
{
  size_t pending = BIO_ctrl_pending(m_nbio);
  if (pending > 0)
  {
    if (m_mayTriggerWrite)
    {
      m_mayTriggerWrite = false;
      TriggerEvent(FD_WRITE, 0);
    }
  }
  else if (!m_nNetworkSendBufferLen && m_bSslEstablished && !m_pRetrySendBuffer)
  {
    if (BIO_ctrl_get_write_guarantee(m_sslbio) > 0 && m_mayTriggerWriteUp)
    {
      m_mayTriggerWriteUp = false;
      TriggerEvent(FD_WRITE, 0, TRUE);
    }
  }

  if (m_bSslEstablished && BIO_ctrl_pending(m_sslbio) > 0)
  {
    if (m_mayTriggerReadUp && !m_bBlocking)
    {
      m_mayTriggerReadUp = false;
      TriggerEvent(FD_READ, 0, TRUE);
    }
  }
  else
  {
    int len = BIO_ctrl_get_write_guarantee(m_nbio);
    if (len > 0 && m_mayTriggerRead)
    {
      m_mayTriggerRead = false;
      TriggerEvent(FD_READ, 0);
    }
  }

  if (m_onCloseCalled && m_bSslEstablished)
  {
    if (BIO_ctrl_pending(m_sslbio) <= 0)
    {
      TriggerEvent(FD_CLOSE, 0, TRUE);
    }
  }
}

//---------------------------------------------------------------------------
