//---------------------------------------------------------------------------
#ifndef FtpControlSocketH
#define FtpControlSocketH
//---------------------------------------------------------------------------
#include "structures.h"
#include "stdafx.h"
#include "FileZillaApi.h"
#include "FileZillaIntf.h"
//---------------------------------------------------------------------------
class CTransferSocket;
class CMainThread;
//---------------------------------------------------------------------------
class CAsyncProxySocketLayer;
class CFtpListResult;
//---------------------------------------------------------------------------
#define CSMODE_NONE             0x0000
#define CSMODE_CONNECT          0x0001
#define CSMODE_COMMAND          0x0002
#define CSMODE_LIST             0x0004
#define CSMODE_TRANSFER         0x0008
#define CSMODE_DOWNLOAD         0x0010
#define CSMODE_UPLOAD           0x0020
#define CSMODE_TRANSFERERROR    0x0040
#define CSMODE_TRANSFERTIMEOUT  0x0080
#define CSMODE_DELETE           0x0100
#define CSMODE_RMDIR            0x0200
#define CSMODE_DISCONNECT       0x0400
#define CSMODE_MKDIR            0x0800
#define CSMODE_RENAME           0x1000
#define CSMODE_CHMOD            0x2000
#define CSMODE_LISTFILE         0x4000
//---------------------------------------------------------------------------
typedef struct
{
  BOOL bResume,bResumeAppend,bType;
  __int64 transfersize,transferleft;
} t_transferdata;
//---------------------------------------------------------------------------
class CFtpControlSocket : public CAsyncSocketEx, public CApiLog
{
  friend CTransferSocket;

public:
  CFtpControlSocket(CMainThread * pMainThread, CFileZillaTools * pTools);
  virtual ~CFtpControlSocket();

public:
  void Connect(t_server & server);
  virtual void OnTimer();
  BOOL IsReady();
  void List(BOOL bFinish, int nError = 0, CServerPath path = CServerPath(), CString subdir = L"");
  void ListFile(CString filename, const CServerPath & path);
  void FtpCommand(const wchar_t * pCommand);
  void Disconnect();
  void FileTransfer(t_transferfile * transferfile = 0, BOOL bFinish = FALSE, int nError = 0);
  void Delete(CString filename, const CServerPath & path, bool filenameOnly);
  void Rename(CString oldName, CString newName, const CServerPath & path, const CServerPath & newPath);
  void MakeDir(const CServerPath & path);
  void RemoveDir(CString dirname, const CServerPath & path);
  void Chmod(CString filename, const CServerPath & path, int nValue);

  void ProcessReply();
  void TransferEnd(int nMode);
  void Cancel(BOOL bQuit = FALSE);

  void SetAsyncRequestResult(int nAction, CAsyncRequestData * pData);

  BOOL Create();
  void TransfersocketListenFinished(unsigned int ip, unsigned short port);

  BOOL m_bKeepAliveActive;
  BOOL m_bDidRejectCertificate;

  // Some servers are broken. Instead of an empty listing, some MVS servers
  // for example they return something "550 no members found"
  // Other servers return "550 No files found."
  bool IsMisleadingListResponse();

  bool UsingMlsd();
  bool UsingUtf8();
  std::string GetTlsVersionStr();
  std::string GetCipherName();
  bool HandleSize(int code, __int64 & size);
  bool HandleMdtm(int code, t_directory::t_direntry::t_date & date);
  void TransferHandleListError();

  enum transferDirection
  {
    download = 0,
    upload = 1
  };

  BOOL RemoveActiveTransfer();
  BOOL SpeedLimitAddTransferredBytes(enum transferDirection direction, __int64 nBytesTransferred);

  __int64 GetSpeedLimit(enum transferDirection direction);

  __int64 GetAbleToTransferSize(enum transferDirection direction, bool &beenWaiting);

  t_server GetCurrentServer();
  CFtpListResult * CreateListResult(bool mlst);

  static UnicodeString DecodeString(
    const RawByteString & S, CApiLog * ApiLog, const t_server & Server, bool * UTF8, bool MayInvalidateUTF8);

public:
  virtual void OnReceive(int nErrorCode);
  virtual void OnConnect(int nErrorCode);
  virtual void OnClose(int nErrorCode);
  virtual void OnSend(int nErrorCode);

protected:
  class CFileTransferData;
  // Called by OnTimer()
  void ResumeTransfer();
  void CheckForTimeout();
  void SendKeepAliveCommand();

  virtual int OnLayerCallback(std::list<t_callbackMsg> & callbacks);
  void SetFileExistsAction(int nAction, COverwriteRequestData * pData);
  void SetVerifyCertResult(int nResult, t_SslCertData * pData);
  void ResetOperation(int nSuccessful = -1);
  void ResetTransferSocket(int Error);
  int OpenTransferFile(CFileTransferData * pData);
  int ActivateTransferSocket(CFileTransferData * pData);
  void CancelTransferResume(CFileTransferData * pData);

  void DoClose(int nError = 0);
  int TryGetReplyCode();
  int GetReplyCode();
  CString GetReply();
  void LogOnToServer(BOOL bSkipReply = FALSE);
  BOOL Send(CString str);

  BOOL ParsePwdReply(CString & rawpwd);
  BOOL ParsePwdReply(CString & rawpwd, CServerPath & realPath);
  BOOL SendAuthSsl();

  void DiscardLine(RawByteString line);
  int FileTransferListState();
  bool NeedOptsCommand();
  CString GetListingCmd();

  bool InitConnect();
  int InitConnectState();

  bool IsRoutableAddress(const CString & host);
  int ParsePasvPort(int code, BOOL & bTriedPortPasvOnce, BOOL & bPasv, const CString & reply, CString & host, int & port);
  CString FormatPortCmd(UINT nPort);
  bool CheckForcePasvIp(CString & host);
  void TransferFinished(bool preserveFileTimeForUploads);

  virtual void LogSocketMessageRaw(int nMessageType, const wchar_t * pMsg);
  virtual bool LoggingSocketMessage(int nMessageType);
  virtual int GetSocketOptionVal(int OptionID) const;

  void ShowStatus(UINT nID, int type) const;
  void ShowStatus(CString status,int type) const;
  void ShowTimeoutError(UINT nID) const;

  void Close();
  BOOL Connect(CString hostAddress, UINT nHostPort);
  bool ConnectTransferSocket(const CString & host, UINT port);
  void TransferSocketFailed();

  struct t_ActiveList
  {
    CFtpControlSocket * pOwner;
    __int64 nBytesAvailable;
    __int64 nBytesTransferred;
  };
  static std::list<t_ActiveList> m_InstanceList[2];
  static CTime m_CurrentTransferTime[2];
  static __int64 m_CurrentTransferLimit[2];
  static CCriticalSectionWrapper m_SpeedLimitSync;
  __int64 GetAbleToUDSize(bool & beenWaiting, CTime & curTime, __int64 & curLimit, std::list<t_ActiveList>::iterator & iter, enum transferDirection direction);
  __int64 GetSpeedLimit(int valType, int valValue);

  void SetDirectoryListing(t_directory * pDirectory, bool bSetWorkingDir = true);
  int CheckOverwriteFile();
  int CheckOverwriteFileAndCreateTarget();
  int FileTransferHandleDirectoryListing(t_directory * pDirectory);
  t_directory * m_pDirectoryListing;

  CMainThread * m_pOwner;
  CFileZillaTools * m_pTools;

  CFile * m_pDataFile;
  CTransferSocket * m_pTransferSocket;
  RawByteString m_MultiLine;
  TDateTime m_LastSendTime;

  CString m_ServerName;
  std::list<RawByteString> m_RecvBuffer;
  TDateTime m_LastRecvTime;
  class CLogonData;
  class CListData;
  class CListFileData;
  class CMakeDirData;

  bool m_bUTF8;
  bool m_bAnnouncesUTF8;
  bool m_hasClntCmd;
  TFTPServerCapabilities m_serverCapabilities;
  RawByteString m_ListFile;
  __int64 m_ListFileSize;
  bool m_isFileZilla;

  bool m_awaitsReply;
  bool m_skipReply;

  char * m_sendBuffer;
  int m_sendBufferLen;

  bool m_bProtP;

  bool m_mayBeMvsFilesystem;
  bool m_mayBeBS2000Filesystem;

  struct t_operation
  {
    int nOpMode;
    int nOpState;
    class COpData //Base class which will store operation specific parameters.
    {
    public:
      COpData() {}
      virtual ~COpData() {}
    };
    COpData * pData;
  public:
  };

  t_operation m_Operation;

  CAsyncProxySocketLayer * m_pProxyLayer;
  CAsyncSslSocketLayer * m_pSslLayer;
#ifndef MPEXT_NO_GSS
  CAsyncGssSocketLayer * m_pGssLayer;
#endif
  t_server m_CurrentServer;

private:
  BOOL m_bCheckForTimeout;
};
//---------------------------------------------------------------------------
#endif // FtpControlSocketH
