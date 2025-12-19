//---------------------------------------------------------------------------
#ifndef TransferSocketH
#define TransferSocketH
//---------------------------------------------------------------------------
#include "FtpListResult.h"
//---------------------------------------------------------------------------
#include "FtpControlSocket.h"
#include "ApiLog.h"
//---------------------------------------------------------------------------
class CFtpControlSocket;
class CAsyncProxySocketLayer;
class CAsyncSslSocketLayer;
#ifndef MPEXT_NO_GSS
class CAsyncGssSocketLayer;
#endif
//---------------------------------------------------------------------------
class CTransferSocket : public CAsyncSocketEx, public CApiLog
{
public:
  CFtpListResult * m_pListResult;

public:
  CTransferSocket(CFtpControlSocket * pOwner, int nMode);
  virtual ~CTransferSocket();

public:
  int m_nInternalMessageID;
  virtual void Close();
  virtual BOOL Create(BOOL bUseSsl);
  BOOL m_bListening;
  CFile * m_pFile;
  TTransferOutEvent m_OnTransferOut;
  TTransferInEvent m_OnTransferIn;
  t_transferdata m_transferdata;
  __int64 m_uploaded;
  void SetActive();
  int CheckForTimeout(int delay);
#ifndef MPEXT_NO_GSS
  void UseGSS(CAsyncGssSocketLayer * pGssLayer);
#endif

public:
  virtual void OnReceive(int nErrorCode);
  virtual void OnAccept(int nErrorCode);
  virtual void OnConnect(int nErrorCode);
  virtual void OnClose(int nErrorCode);
  virtual void OnSend(int nErrorCode);
  virtual void SetState(int nState);

protected:
  virtual int OnLayerCallback(std::list<t_callbackMsg> & callbacks);
  int ReadDataFromFile(char * buffer, int len);
  int ReadData(char * buffer, int len);
  void WriteData(const char * buffer, int len);
  virtual void LogSocketMessageRaw(int nMessageType, LPCTSTR pMsg);
  virtual int GetSocketOptionVal(int OptionID) const;
  virtual void ConfigureSocket();
  bool Activate();
  void Start();

  CFtpControlSocket * m_pOwner;
  CAsyncProxySocketLayer * m_pProxyLayer;
  CAsyncSslSocketLayer * m_pSslLayer;
#ifndef MPEXT_NO_GSS
  CAsyncGssSocketLayer * m_pGssLayer;
#endif
  void UpdateStatusBar(bool forceUpdate);
  BOOL m_bSentClose;
  int m_bufferpos;
  char * m_pBuffer;
  BOOL m_bCheckTimeout;
  TDateTime m_LastActiveTime;
  int m_nTransferState;
  int m_nMode;
  int m_nNotifyWaiting;
  bool m_bActivationPending;

  void CloseAndEnsureSendClose(int Mode);
  void EnsureSendClose(int Mode);
  void CloseOnShutDownOrError(int Mode);
  void SetBuffers();
  _int64 GetTransferSize(CFtpControlSocket::transferDirection direction, bool & beenWaiting);

  LARGE_INTEGER m_LastUpdateTime;
  unsigned int m_LastSendBufferUpdate;
  DWORD m_SendBuf;
};
//---------------------------------------------------------------------------
#endif // TransferSocketH
