//---------------------------------------------------------------------------
#include "stdafx.h"
#include "FtpControlSocket.h"
#include "MainThread.h"
#include "transfersocket.h"
#include "asyncproxysocketlayer.h"
#include "AsyncSslSocketLayer.h"
#ifndef MPEXT_NO_GSS
#include "AsyncGssSocketLayer.h"
#endif
#include "filezillaapi.h"
#include <WideStrUtils.hpp>

class CFtpControlSocket::CFileTransferData : public CFtpControlSocket::t_operation::COpData
{
public:
  CFileTransferData()
  {
    pDirectoryListing=0;
    nGotTransferEndReply=0;
    nWaitNextOpState=0;
    nMKDOpState=-1;
    hasRemoteDate = false;
    pFileSize=0;
    bUseAbsolutePaths = FALSE;
    bTriedPortPasvOnce = FALSE;
    askOnResumeFail = false;
  };
  ~CFileTransferData()
  {
    if (pDirectoryListing)
      delete pDirectoryListing;
    pDirectoryListing=0;
    delete pFileSize;
  };
  CString rawpwd;
  t_transferfile transferfile;
  t_transferdata transferdata;
  CString host;
  int port;
  BOOL bPasv;
  int nGotTransferEndReply;
  t_directory *pDirectoryListing;
  int nWaitNextOpState;
  CServerPath MKDCurrent;
  std::list<CString> MKDSegments;
  int nMKDOpState;
  bool hasRemoteDate;
  t_directory::t_direntry::t_date remoteDate;
  //CTime *pFileTime; //Used when downloading and OPTION_PRESERVEDOWNLOADFILETIME is set or when LIST fails
  _int64 *pFileSize; //Used when LIST failes
  BOOL bUseAbsolutePaths;
  BOOL bTriedPortPasvOnce;
#ifndef MPEXT_NO_ZLIB
  int newZlibLevel;
#endif
  bool askOnResumeFail;
};

class CFtpControlSocket::CLogonData:public CFtpControlSocket::t_operation::COpData
{
public:
  CLogonData()
  {
    waitForAsyncRequest = false;
    gotPassword = false;
  }
  virtual ~CLogonData()
  {
  }
  bool waitForAsyncRequest;
  bool gotPassword;
};

class CFtpControlSocket::CListData:public CFtpControlSocket::t_operation::COpData
{
public:
  CListData()
  {
    pDirectoryListing = 0;
    bTriedPortPasvOnce = FALSE;
    lastCmdSentCDUP = false;
  }
  virtual ~CListData()
  {
    if (pDirectoryListing)
      delete pDirectoryListing;
  }
  CString rawpwd;
  CServerPath path;
  CString fileName;
  CString subdir;
  BOOL bPasv;
  CString host;
  int port;
  int nFinish;
  t_directory *pDirectoryListing;
  BOOL bTriedPortPasvOnce;
#ifndef MPEXT_NO_ZLIB
  int newZlibLevel;
#endif
  bool lastCmdSentCDUP;
};

class CFtpControlSocket::CListFileData:public CFtpControlSocket::t_operation::COpData
{
public:
  CListFileData()
  {
    direntry = NULL;
  }
  ~CListFileData()
  {
    delete [] direntry;
  }
  CString fileName;
  CString dir;
  CString path;
  CServerPath pwd;
  t_directory::t_direntry * direntry;
};

class CFtpControlSocket::CMakeDirData : public CFtpControlSocket::t_operation::COpData
{
public:
  CMakeDirData() {}
  virtual ~CMakeDirData() {}
  CServerPath path;
  CServerPath Current;
  std::list<CString> Segments;
};

#define MKD_INIT -1
#define MKD_FINDPARENT 0
#define MKD_MAKESUBDIRS 1
#define MKD_CHANGETOSUBDIR 2

/////////////////////////////////////////////////////////////////////////////
// CFtpControlSocket

std::list<CFtpControlSocket::t_ActiveList> CFtpControlSocket::m_InstanceList[2];

CTime CFtpControlSocket::m_CurrentTransferTime[2] = { CTime::GetCurrentTime(), CTime::GetCurrentTime() };
_int64 CFtpControlSocket::m_CurrentTransferLimit[2] = {0, 0};

CCriticalSectionWrapper CFtpControlSocket::m_SpeedLimitSync;

#define BUFSIZE 16384

CFtpControlSocket::CFtpControlSocket(CMainThread *pMainThread, CFileZillaTools * pTools)
{
  DebugAssert(pMainThread);
  m_pOwner=pMainThread;
  m_pTools=pTools;

  m_Operation.nOpMode=0;
  m_Operation.nOpState=-1;
  m_Operation.pData=0;

  m_pProxyLayer = NULL;
  m_pSslLayer = NULL;
#ifndef MPEXT_NO_GSS
  m_pGssLayer = NULL;
#endif

  m_pDirectoryListing=0;

  m_pTransferSocket=0;
  m_pDataFile=0;
  srand( (unsigned)time( NULL ) );
  m_bKeepAliveActive=FALSE;
  m_bCheckForTimeout=TRUE;
  m_bDidRejectCertificate = FALSE;

#ifndef MPEXT_NO_ZLIB
  m_useZlib = false;
  m_zlibSupported = false;
  m_zlibLevel = 8;
#endif

  m_bUTF8 = true;
  m_hasClntCmd = false;
  m_serverCapabilities.Clear();
  m_ListFile = "";

  m_awaitsReply = false;
  m_skipReply = false;

  m_sendBuffer = 0;
  m_sendBufferLen = 0;

  m_bProtP = false;

  m_mayBeMvsFilesystem = false;
  m_mayBeBS2000Filesystem = false;
}

CFtpControlSocket::~CFtpControlSocket()
{
  DoClose();
  if (m_pTransferSocket)
  {
    m_pTransferSocket->Close();
    delete m_pTransferSocket;
    m_pTransferSocket=0;
  }
  if (m_pDataFile)
  {
    delete m_pDataFile;
    m_pDataFile=0;
  }
  Close();
}

void CFtpControlSocket::ShowStatus(UINT nID, int type) const
{
  CString str;
  str.LoadString(nID);
  ShowStatus(str, type);
}

void CFtpControlSocket::ShowStatus(CString status, int type) const
{
  if (!GetOptionVal(OPTION_MPEXT_LOG_SENSITIVE))
  {
    if ( status.Left(5)==L"PASS " )
    {
      int len=status.GetLength()-5;
      status=L"PASS ";
      for (int i=0;i<len;i++)
        status+=L"*";
    }
    else if ( status.Left(5)==L"ACCT " )
    {
      int len=status.GetLength()-5;
      status=L"ACCT ";
      for (int i=0;i<len;i++)
        status+=L"*";
    }
  }
  LogMessageRaw(type, (LPCTSTR)status);
}

void CFtpControlSocket::ShowTimeoutError(UINT nID) const
{
  CString str1;
  str1.LoadString(IDS_ERRORMSG_TIMEOUT);
  CString str2;
  str2.LoadString(nID);
  CString message;
  message.Format(L"%s (%s)", (LPCTSTR)str1, (LPCTSTR)str2);
  ShowStatus(message, FZ_LOG_ERROR);
}

t_server CFtpControlSocket::GetCurrentServer()
{
  return m_CurrentServer;
}

void CFtpControlSocket::Close()
{
  if (m_pDirectoryListing)
  {
    delete m_pDirectoryListing;
  }
  m_pDirectoryListing=0;
  CAsyncSocketEx::Close();

  delete m_pProxyLayer;
  m_pProxyLayer = NULL;

  delete m_pSslLayer;
  m_pSslLayer = NULL;

#ifndef MPEXT_NO_GSS
  delete m_pGssLayer;
  m_pGssLayer = NULL;
#endif

  RemoveActiveTransfer();
}

BOOL CFtpControlSocket::Connect(CString hostAddress, UINT nHostPort)
{
  hostAddress = ConvertDomainName(hostAddress);

  //Don't resolve host asynchronously when using proxies
  if (m_pProxyLayer)
  {
    return CAsyncSocketEx::Connect(hostAddress, nHostPort);
  }
  BOOL res = CAsyncSocketEx::Connect(hostAddress, nHostPort);
  int nLastError = WSAGetLastError();
  if (res || nLastError==WSAEWOULDBLOCK)
  {
    WSASetLastError(nLastError);
  }

  return res;
}

void CFtpControlSocket::SetDirectoryListing(t_directory *pDirectory, bool bSetWorkingDir /*=true*/)
{
  if (m_pDirectoryListing)
    delete m_pDirectoryListing;
  m_CurrentServer=pDirectory->server;
  m_pDirectoryListing=new t_directory;
  *m_pDirectoryListing=*pDirectory;

  if (bSetWorkingDir)
    m_pOwner->SetWorkingDir(pDirectory);
}

/////////////////////////////////////////////////////////////////////////////
// Member-Funktion CFtpControlSocket
#define CONNECT_INIT -1
#ifndef MPEXT_NO_GSS
#define CONNECT_GSS_INIT -2
#define CONNECT_GSS_AUTHDONE -3
#define CONNECT_GSS_CWD -4
#define CONNECT_GSS_FAILED -5
#define CONNECT_GSS_NEEDPASS -6
#define CONNECT_GSS_NEEDUSER -7
#endif
#define CONNECT_SSL_INIT -8
#define CONNECT_SSL_NEGOTIATE -9
#define CONNECT_TLS_NEGOTIATE -10
#define CONNECT_SSL_WAITDONE -11
#define CONNECT_SSL_PBSZ -12
#define CONNECT_SSL_PROT -13
#define CONNECT_FEAT -14
#define CONNECT_SYST -15
#define CONNECT_OPTSUTF8 -16
#define CONNECT_CLNT -17
#define CONNECT_OPTSMLST -18
#define CONNECT_NEEDPASS -19
#define CONNECT_HOST -20

bool CFtpControlSocket::InitConnect()
{
  USES_CONVERSION;

  // Reset detected capabilities
  m_bAnnouncesUTF8 = false;
  m_hasClntCmd = false;
  m_serverCapabilities.Clear();
  m_ListFile = "";
  m_isFileZilla = false;

  if (m_CurrentServer.nUTF8 == 2)
    m_bUTF8 = false;
  else
    m_bUTF8 = true;

  // Some sanity checks
  if (m_pOwner->IsConnected())
  {
    ShowStatus(L"Internal error: Connect called while still connected", FZ_LOG_ERROR);
    if (!m_Operation.nOpMode)
      m_Operation.nOpMode = CSMODE_CONNECT;
    DoClose(FZ_REPLY_CRITICALERROR);
    return false;
  }

  if (m_pSslLayer)
  {
    ShowStatus(L"Internal error: m_pSslLayer not zero in Connect", FZ_LOG_ERROR);
    DoClose(FZ_REPLY_CRITICALERROR);
    return false;
  }
  if (m_pProxyLayer)
  {
    ShowStatus(L"Internal error: m_pProxyLayer not zero in Connect", FZ_LOG_ERROR);
    DoClose(FZ_REPLY_CRITICALERROR);
    return false;
  }

  if (m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_IMPLICIT ||
    m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_EXPLICIT ||
    m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_TLS_EXPLICIT)
  {
    m_pSslLayer = new CAsyncSslSocketLayer;
    AddLayer(m_pSslLayer);

    m_pSslLayer->SetClientCertificate(m_CurrentServer.Certificate, m_CurrentServer.PrivateKey);

    CString filename = GetOption(OPTION_MPEXT_CERT_STORAGE);
    m_pSslLayer->SetCertStorage(filename);
  }

  int nProxyType = GetOptionVal(OPTION_PROXYTYPE);
  if (nProxyType != PROXYTYPE_NOPROXY)
  {
    m_pProxyLayer = new CAsyncProxySocketLayer;
    m_pProxyLayer->SetProxy(
      nProxyType, T2CA(GetOption(OPTION_PROXYHOST)), GetOptionVal(OPTION_PROXYPORT),
      GetOptionVal(OPTION_PROXYUSELOGON), T2CA(GetOption(OPTION_PROXYUSER)), T2CA(GetOption(OPTION_PROXYPASS)));
    AddLayer(m_pProxyLayer);
  }

#ifndef MPEXT_NO_GSS
  BOOL bUseGSS = FALSE;
  if (GetOptionVal(OPTION_USEGSS) && !m_pSslLayer)
  {
    CString GssServers=GetOption(OPTION_GSSSERVERS);
    LPCSTR lpszAscii=T2CA(m_CurrentServer.host);
    hostent *fullname=gethostbyname(lpszAscii);
    CString host;
    if (fullname)
      host=fullname->h_name;
    else
      host=m_CurrentServer.host;
    host.MakeLower();
    int i;
    while ((i=GssServers.Find(L";"))!=-1)
    {
      if ((L"."+GssServers.Left(i))==host.Right(GssServers.Left(i).GetLength()+1) || GssServers.Left(i)==host)
      {
        bUseGSS=TRUE;
        break;
      }
      GssServers=GssServers.Mid(i+1);
    }
  }
  if (bUseGSS)
  {
    m_pGssLayer = new CAsyncGssSocketLayer;
    AddLayer(m_pGssLayer);
    if (!m_pGssLayer->InitGSS())
    {
      ShowStatus(L"Unable to initialize GSS api", FZ_LOG_ERROR);
      DoClose(FZ_REPLY_CRITICALERROR);
      return false;
    }
  }
#endif

  return true;
}

int CFtpControlSocket::InitConnectState()
{
  if ((m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
    return CONNECT_SSL_INIT;
  else
#ifndef MPEXT_NO_GSS
  if (m_pGssLayer)
    return CONNECT_GSS_INIT;
  else
#endif
  return CONNECT_INIT;
}

void CFtpControlSocket::Connect(t_server &server)
{
  if (m_Operation.nOpMode)
  {
    ShowStatus(L"Internal error: m_Operation.nOpMode not zero in Connect", FZ_LOG_ERROR);
    m_Operation.nOpMode = CSMODE_CONNECT;
    DoClose(FZ_REPLY_CRITICALERROR);
    return;
  }

  m_Operation.nOpMode = CSMODE_CONNECT;

  m_CurrentServer = server;
  if (!InitConnect())
    return;

  if (!Create())
  {
    DoClose();
    return;
  }
  AsyncSelect();

  if (GetOptionVal(OPTION_MPEXT_HOST))
  {
    m_Operation.nOpState = CONNECT_HOST;
  }
  else
  {
    m_Operation.nOpState = InitConnectState();
  }

  if (server.nServerType & FZ_SERVERTYPE_LAYER_SSL_IMPLICIT)
  {
    if (!m_pSslLayer)
    {
      ShowStatus(L"Internal error: m_pSslLayer not initialized", FZ_LOG_ERROR);
      DoClose(FZ_REPLY_CRITICALERROR);
      return;
    }
    int res = m_pSslLayer->InitSSLConnection(true, NULL,
      GetOptionVal(OPTION_MPEXT_SSLSESSIONREUSE), server.host,
      m_pTools);
    if (res == SSL_FAILURE_INITSSL)
      ShowStatus(IDS_ERRORMSG_CANTINITSSL, FZ_LOG_ERROR);
    if (res)
    {
      DoClose();
      return;
    }
  }

  int logontype = GetOptionVal(OPTION_LOGONTYPE);
  int port;
  CString buf,temp;
  // are we connecting directly to the host (logon type 0) or via a firewall? (logon type>0)
  CString fwhost;
  int fwport;
  if(!logontype)
  {
    temp=server.host;
    port=server.port;
  }
  else
  {
    fwhost=GetOption(OPTION_FWHOST);
    fwport=GetOptionVal(OPTION_FWPORT);
    temp=fwhost;
    port=fwport;
    if(fwport!=21)
      fwhost.Format( L"%s:%d", (LPCTSTR)fwhost, fwport); // add port to fwhost (only if port is not 21)
  }

  CString hostname = server.host;
  if(server.port!=21)
    hostname.Format( L"%s:%d", (LPCTSTR)hostname, server.port); // add port to hostname (only if port is not 21)
  CString str;
  str.Format(IDS_STATUSMSG_CONNECTING, (LPCTSTR)hostname);
  ShowStatus(str, FZ_LOG_STATUS);

  if (!Connect(temp, port))
  {
    if (WSAGetLastError() != WSAEWOULDBLOCK)
    {
      DoClose();
      return;
    }
    else
    {
      LogMessage(FZ_LOG_INFO, L"Connection pending");
    }
  }
  else
  {
    LogMessage(FZ_LOG_INFO, L"Connected");
  }
  m_ServerName = logontype?fwhost:hostname;
  m_LastRecvTime = m_LastSendTime = CTime::GetCurrentTime();

  m_Operation.pData = new CLogonData();
}

static CString NormalizePass(const CString & pass)
{
  return CString(NormalizeString(UnicodeString(T2CW(pass))).c_str());
}

void CFtpControlSocket::LogOnToServer(BOOL bSkipReply /*=FALSE*/)
{
  int logontype =
#ifndef MPEXT_NO_GSS
  m_pGssLayer ? 0 :
#endif
  GetOptionVal(OPTION_LOGONTYPE);
  const int LO = -2, ER = -1;
  CString buf, temp;
  const int NUMLOGIN = 9; // currently supports 9 different login sequences
  int logonseq[NUMLOGIN][20] = {
    // this array stores all of the logon sequences for the various firewalls
    // in blocks of 3 nums. 1st num is command to send, 2nd num is next point in logon sequence array
    // if 200 series response is rec'd from server as the result of the command, 3rd num is next
    // point in logon sequence if 300 series rec'd
    {0,LO,3, 1,LO,6,  12,LO,ER}, // no firewall
    {3,6,3,  4,6,ER, 5,9,9, 0,LO,12, 1,LO,15, 12,LO,ER}, // SITE hostname
    {3,6,3,  4,6,ER, 6,LO,9, 1,LO,12, 12,LO,ER}, // USER after logon
    {7,3,3,  0,LO,6, 1,LO,9, 12,LO,ER}, //proxy OPEN
    {3,6,3,  4,6,ER, 0,LO,9, 1,LO,12, 12,LO,ER}, // Transparent
    {6,LO,3, 1,LO,6, 12,LO,ER}, // USER remoteID@remotehost
    {8,6,3,  4,6,ER, 0,LO,9, 1,LO,12, 12,LO,ER}, //USER fireID@remotehost
    {9,ER,3, 1,LO,6, 2,LO,ER}, //USER remoteID@remotehost fireID
    {10,LO,3,11,LO,6,2,LO,ER} // USER remoteID@fireID@remotehost
  };

  if (m_Operation.nOpState == CONNECT_SSL_INIT)
  {
    if (m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYER_SSL_EXPLICIT)
    {
      if (!SendAuthSsl())
      {
        return;
      }
    }
    else
    {
      if (!Send("AUTH TLS"))
        return;
      m_Operation.nOpState = CONNECT_TLS_NEGOTIATE;
    }
    return;
  }
  else if ((m_Operation.nOpState == CONNECT_SSL_NEGOTIATE) ||
           (m_Operation.nOpState == CONNECT_TLS_NEGOTIATE))
  {
    int res = GetReplyCode();
    if (res!=2 && res!=3)
    {
      if (m_Operation.nOpState == CONNECT_TLS_NEGOTIATE)
      {
        // Try to fall back to AUTH SSL
        if (!SendAuthSsl())
        {
          return;
        }
      }
      else
      {
        DoClose();
      }
      return;
    }
    else
    {
      if (!m_pSslLayer)
      {
        ShowStatus(L"Internal error: m_pSslLayer not initialized", FZ_LOG_ERROR);
        DoClose(FZ_REPLY_CRITICALERROR);
        return;
      }
      int res = m_pSslLayer->InitSSLConnection(true, NULL,
        GetOptionVal(OPTION_MPEXT_SSLSESSIONREUSE), m_CurrentServer.host,
        m_pTools);
      if (res == SSL_FAILURE_INITSSL)
        ShowStatus(IDS_ERRORMSG_CANTINITSSL, FZ_LOG_ERROR);
      if (res)
      {
        DoClose();
        return;
      }
    }
    m_Operation.nOpState = CONNECT_SSL_WAITDONE;
    return;
  }
  else if (m_Operation.nOpState == CONNECT_SSL_WAITDONE)
  {
    m_Operation.nOpState = CONNECT_INIT;
  }
  else if (m_Operation.nOpState == CONNECT_SSL_PBSZ)
  {
    if (!Send(L"PROT P"))
      return;
    m_Operation.nOpState = CONNECT_SSL_PROT;
    return;
  }
  else if (m_Operation.nOpState == CONNECT_SSL_PROT)
  {
    int code = GetReplyCode();
    if (code == 2 || code == 3)
      m_bProtP = true;

    ShowStatus(IDS_STATUSMSG_CONNECTED, FZ_LOG_STATUS);
    m_pOwner->SetConnected(TRUE);
    ResetOperation(FZ_REPLY_OK);
    return;
  }
#ifndef MPEXT_NO_GSS
  else
  if (m_Operation.nOpState==CONNECT_GSS_FAILED ||
       m_Operation.nOpState == CONNECT_GSS_NEEDPASS ||
       m_Operation.nOpState == CONNECT_GSS_NEEDUSER)
  {
    if (!m_RecvBuffer.empty() && m_RecvBuffer.front() != "")
    {
      //Incoming reply from server during async is not allowed
      DoClose();
      return;
    }
  }
#endif
  else if (m_Operation.nOpState == CONNECT_HOST)
  {
    if (Send(L"HOST " + m_CurrentServer.host))
    {
      m_Operation.nOpState = InitConnectState();
      return;
    }
  }
  else if (m_Operation.nOpState == CONNECT_OPTSMLST)
  {
    int code = GetReplyCode();
    if (code != 2 && code != 3)
      m_serverCapabilities.SetCapability(mlsd_command, no);
    ShowStatus(IDS_STATUSMSG_CONNECTED, FZ_LOG_STATUS);
    m_pOwner->SetConnected(TRUE);
    ResetOperation(FZ_REPLY_OK);
    return;
  }
  else if (m_Operation.nOpState == CONNECT_FEAT)
  {
    std::string facts;
    if (m_serverCapabilities.GetCapabilityString(mlsd_command, &facts) == yes)
    {
      ftp_capabilities_t cap = m_serverCapabilities.GetCapabilityString(opts_mlst_command);
      if (cap == unknown)
      {
        std::transform(facts.begin(), facts.end(), facts.begin(), ::towlower);
        bool had_unset = false;
        std::string opts_facts;
        // Create a list of all facts understood by both FZ and the server.
        // Check if there's any supported fact not enabled by default, should that
        // be the case we need to send OPTS MLST
        while (!facts.empty())
        {
          size_t delim = facts.find_first_of(';');
          if (delim == -1)
            break;

          if (!delim)
          {
            facts = facts.substr(1, std::string::npos);
            continue;
          }

          bool enabled = false;
          std::string fact;

          if (facts[delim - 1] == '*')
          {
            if (delim == 1)
            {
              facts = facts.substr(delim + 1, std::string::npos);
              continue;
            }
            enabled = true;
            fact = facts.substr(0, delim - 1);
          }
          else
          {
            enabled = false;
            fact = facts.substr(0, delim);
          }
          facts = facts.substr(delim + 1, std::string::npos);

          if (!strcmp(fact.c_str(), "type") ||
            !strcmp(fact.c_str(), "size") ||
            !strcmp(fact.c_str(), "modify") ||
            !strcmp(fact.c_str(), "create") ||
            !strcmp(fact.c_str(), "perm") ||
            !strcmp(fact.c_str(), "unix.mode") ||
            !strcmp(fact.c_str(), "unix.owner") ||
            !strcmp(fact.c_str(), "unix.user") ||
            !strcmp(fact.c_str(), "unix.group") ||
            !strcmp(fact.c_str(), "unix.uid") ||
            !strcmp(fact.c_str(), "unix.gid"))
          {
            had_unset |= !enabled;
            opts_facts += fact.c_str();
            opts_facts += ";";
          }
        }
        if (had_unset)
        {
          m_serverCapabilities.SetCapability(opts_mlst_command, yes, opts_facts);
        }
        else
        {
          m_serverCapabilities.SetCapability(opts_mlst_command, no);
        }
      }
    }
    GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_CAPABILITIES, 0), (LPARAM)&m_serverCapabilities);
    if (!m_bAnnouncesUTF8 && !m_CurrentServer.nUTF8)
      m_bUTF8 = false;
    if (m_hasClntCmd)
    {
      // Some servers refuse to enable UTF8 if client does not send CLNT command
      // to fix compatibility with Internet Explorer, but in the process breaking
      // compatibility with other clients.
      // Rather than forcing MS to fix Internet Explorer, letting other clients
      // suffer is a questionable decision in my opinion.
      if (Send(CString(L"CLNT ") + m_pTools->GetClientString().c_str()))
        m_Operation.nOpState = CONNECT_CLNT;
      return;
    }
    if (m_bUTF8 && !m_isFileZilla)
    {
      // Handle servers that disobey RFC 2640 that have UTF8 in the FEAT
      // response but do not use UTF8 unless OPTS UTF8 ON gets send.
      // However these servers obey a conflicting ietf draft:
      // https://datatracker.ietf.org/doc/html/draft-ietf-ftpext-utf-8-option-00
      // servers are, amongst others, G6 FTP Server and RaidenFTPd.
      if (Send(L"OPTS UTF8 ON"))
        m_Operation.nOpState = CONNECT_OPTSUTF8;
      return;
    }

    if ((m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_IMPLICIT | FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
    {
      m_Operation.nOpState = CONNECT_SSL_PBSZ;
      Send(L"PBSZ 0");
      return;
    }

    if (m_serverCapabilities.GetCapability(mlsd_command) == yes)
    {
      std::string args;
      // this is never true, see comment is DiscardLine
      if (m_serverCapabilities.GetCapabilityString(opts_mlst_command, &args) == yes &&
        !args.empty())
      {
        m_Operation.nOpState = CONNECT_OPTSMLST;
        Send("OPTS MLST " + CString(args.c_str()));
        return;
      }
    }

    ShowStatus(IDS_STATUSMSG_CONNECTED, FZ_LOG_STATUS);
    m_pOwner->SetConnected(TRUE);
    ResetOperation(FZ_REPLY_OK);
    return;
  }
  else if (m_Operation.nOpState == CONNECT_CLNT)
  {
    // See above why we send this command
    if (Send(L"OPTS UTF8 ON"))
      m_Operation.nOpState = CONNECT_OPTSUTF8;
    return;
  }
  else if (m_Operation.nOpState == CONNECT_OPTSUTF8)
  {
    if ((m_CurrentServer.nServerType & FZ_SERVERTYPE_LAYERMASK) & (FZ_SERVERTYPE_LAYER_SSL_IMPLICIT | FZ_SERVERTYPE_LAYER_SSL_EXPLICIT | FZ_SERVERTYPE_LAYER_TLS_EXPLICIT))
    {
      m_Operation.nOpState = CONNECT_SSL_PBSZ;
      Send(L"PBSZ 0");
      return;
    }

    ShowStatus(IDS_STATUSMSG_CONNECTED, FZ_LOG_STATUS);
    m_pOwner->SetConnected(TRUE);
    ResetOperation(FZ_REPLY_OK);
    return;
  }
  else if (m_Operation.nOpState == CONNECT_SYST)
  {
    if (GetReplyCode() == 2)
    {
      const CStringA reply = m_RecvBuffer.front();
      if (reply.GetLength() > 7 && reply.Mid(3, 4) == " MVS")
        m_mayBeMvsFilesystem = true;
      else if (reply.GetLength() >= 11 && reply.Mid(3, 8) == " BS-2000")
        m_mayBeBS2000Filesystem = true;

      if (reply.Left(4) == "VMS ")
      {
        m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
      }

      if (reply.Find("FileZilla") != -1)
        m_isFileZilla = true;
    }

    if (Send(L"FEAT"))
      m_Operation.nOpState = CONNECT_FEAT;
    return;
  }
  else if (!bSkipReply)
  {
    int res = GetReplyCode();
    if (res != 2 && res != 3 && m_Operation.nOpState >= 0) // get initial connect msg off server
    {
      int nError = 0;
      if (m_bUTF8 && m_CurrentServer.nUTF8 != 1)
      {
        // Fall back to local charset for the case that the server might not
        // support UTF8 and the login data contains non-ascii characters.
        bool asciiOnly = true;
        for (int i = 0; i < m_CurrentServer.user.GetLength(); i++)
          if (m_CurrentServer.user.GetAt(i) > 127)
            asciiOnly = false;
        CString pass = NormalizePass(m_CurrentServer.pass);
        for (int i = 0; i < pass.GetLength(); i++)
          if (pass.GetAt(i) > 127)
            asciiOnly = false;
        for (int i = 0; i < m_CurrentServer.account.GetLength(); i++)
          if (m_CurrentServer.account.GetAt(i) > 127)
            asciiOnly = false;
        if (!asciiOnly)
        {
          ShowStatus(L"Login data contains non-ascii characters and server might not be UTF-8 aware. Trying local charset.", FZ_LOG_STATUS);
          m_bUTF8 = false;
          m_Operation.nOpState = CONNECT_INIT;
        }
        else
          nError = FZ_REPLY_ERROR;
      }
      else
        nError = FZ_REPLY_ERROR;

      if (nError)
      {
        if (res==5 && logonseq[logontype][m_Operation.nOpState]==1)
          nError|=FZ_REPLY_CRITICALERROR;

        DoClose(nError);
        return;
      }
    }
  }
  CString hostname = m_CurrentServer.host;
  if (m_CurrentServer.port != 21)
    hostname.Format(hostname+  L":%d", m_CurrentServer.port); // add port to hostname (only if port is not 21)

#ifndef MPEXT_NO_GSS
  USES_CONVERSION;
  //**** GSS Authentication ****
  if (m_Operation.nOpState==CONNECT_GSS_INIT)  //authenticate
  {
    int  i = m_pGssLayer->GetClientAuth(T2CA(m_CurrentServer.host));
    if (i==-1)
      m_Operation.nOpState = CONNECT_GSS_AUTHDONE;
    else if (i != GSSAPI_AUTHENTICATION_SUCCEEDED)
    {
      m_Operation.nOpState = CONNECT_GSS_FAILED;
      CAsyncRequestData *pData=new CAsyncRequestData;
      pData->nRequestType=FZ_ASYNCREQUEST_GSS_AUTHFAILED;
      pData->nRequestID=m_pOwner->GetNextAsyncRequestID();
      if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_AUTHFAILED), (LPARAM)pData))
      {
        delete pData;
      }
      else
      {
        m_bCheckForTimeout = FALSE;
      }
    }
    else
    {
      // we got authentication, we need to check whether we have forwardable tickets
      if (Send(L"CWD ."))
        m_Operation.nOpState = CONNECT_GSS_CWD;
    }
    return;
  }
  else if (m_Operation.nOpState == CONNECT_GSS_AUTHDONE)
  {
    if (!m_pGssLayer->AuthSuccessful())
    {
      m_Operation.nOpState = CONNECT_GSS_FAILED;
      CAsyncRequestData *pData=new CAsyncRequestData;
      pData->nRequestType = FZ_ASYNCREQUEST_GSS_AUTHFAILED;
      pData->nRequestID = m_pOwner->GetNextAsyncRequestID();
      if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_AUTHFAILED), (LPARAM)pData))
      {
        delete pData;
      }
      else
      {
        m_bCheckForTimeout = FALSE;
      }
      return;
    }
    else
    {
      // we got authentication, we need to check whether we have forwardable tickets
      if (Send(L"CWD ."))
        m_Operation.nOpState = CONNECT_GSS_CWD;
      return;
    }
  }
  else if (m_Operation.nOpState == CONNECT_GSS_CWD)
  { // authentication succeeded, we're now get the response to the CWD command
    if (GetReplyCode() == 2) // we're logged on
    {
      if (Send(L"SYST"))
        m_Operation.nOpState = CONNECT_SYST;
      return;
    }
    else
    {
      //GSS authentication complete but we still have to go through the standard logon procedure
      m_Operation.nOpState = CONNECT_INIT;
    }
  }
#endif

  if (m_Operation.nOpState==CONNECT_INIT)
  {
    if (logontype)
    {
      CString str;
      str.Format(IDS_STATUSMSG_FWCONNECT,(LPCTSTR)hostname);
      ShowStatus(str,FZ_LOG_STATUS);
    }
    m_Operation.nOpState++;
  }
  else if (m_Operation.nOpState >= 0 && !bSkipReply)
  {
    m_Operation.nOpState=logonseq[logontype][m_Operation.nOpState+GetReplyCode()-1]; //get next command from array
    switch(m_Operation.nOpState)
    {
    case ER: // ER means summat has gone wrong
      DoClose();
      return;
    case LO: //LO means we are logged on
      if (Send(L"SYST"))
        m_Operation.nOpState = CONNECT_SYST;
      return;
    }
  }

  // go through appropriate logon procedure
#ifndef MPEXT_NO_GSS
  int i = logonseq[logontype][m_Operation.nOpState];
  if (m_pGssLayer)
  {
    if ((i == 0 || i == 6 || i == 9 || i == 10) &&
      (m_CurrentServer.user == L"anonymous" || m_CurrentServer.user == L""))
    {
      //Extract user from kerberos ticket
      char str[256];
      if (m_pGssLayer->GetUserFromKrbTicket(str))
        m_CurrentServer.user = str;
      if (m_CurrentServer.user == L"")
      {
        CGssNeedUserRequestData *pData = new CGssNeedUserRequestData;
        pData->nRequestID = m_pOwner->GetNextAsyncRequestID();
        pData->nOldOpState = m_Operation.nOpState;
        m_Operation.nOpState = CONNECT_GSS_NEEDUSER;
        if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_NEEDUSER), (LPARAM)pData))
        {
          delete pData;
        }
        else
        {
          m_bCheckForTimeout = FALSE;
        }
        return;
      }
    }
    else if ((i == 1 || i == 11) && (m_CurrentServer.pass == GetOption(OPTION_ANONPWD) || m_CurrentServer.pass == ""))
    {
      CGssNeedPassRequestData *pData=new CGssNeedPassRequestData;
      pData->nRequestID=m_pOwner->GetNextAsyncRequestID();
      pData->nOldOpState = m_Operation.nOpState;
      m_Operation.nOpState = CONNECT_GSS_NEEDPASS;
      if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_GSS_NEEDPASS), (LPARAM)pData))
      {
        delete pData;
      }
      else
      {
        m_bCheckForTimeout = FALSE;
      }
      return;
    }
  }
#endif
  CLogonData *pData = static_cast<CLogonData *>(m_Operation.pData);
  bool needpass = (m_CurrentServer.pass == GetOption(OPTION_ANONPWD)) || (m_CurrentServer.pass == L"");
  switch(logonseq[logontype][m_Operation.nOpState])
  {
    case 0:
      temp=L"USER "+m_CurrentServer.user;
      break;
    case 1:
      if (needpass && !pData->waitForAsyncRequest && !pData->gotPassword)
      {
        CNeedPassRequestData *pNeedPassRequestData = new CNeedPassRequestData();
        pNeedPassRequestData->nRequestID = m_pOwner->GetNextAsyncRequestID();
        pNeedPassRequestData->nOldOpState = m_Operation.nOpState;
        m_Operation.nOpState = CONNECT_NEEDPASS;
        if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_NEEDPASS), (LPARAM)pNeedPassRequestData))
        {
          delete pNeedPassRequestData;
          ResetOperation(FZ_REPLY_ERROR);
        }
        else
        {
          m_bCheckForTimeout = FALSE;
        }
        pData->waitForAsyncRequest = true;
        return;
      }
      else if (!needpass || pData->gotPassword)
      {
        temp=L"PASS "+NormalizePass(m_CurrentServer.pass);
      }
      else
      {
        return;
      }
      break;
    case 2:
      temp=L"ACCT "+GetOption(OPTION_FWPASS);
      break;
    case 3:
      temp=L"USER "+GetOption(OPTION_FWUSER);
      break;
    case 4:
      temp=L"PASS "+GetOption(OPTION_FWPASS);
      break;
    case 5:
      temp=L"SITE "+hostname;
      break;
    case 6:
      temp=L"USER "+m_CurrentServer.user+L"@"+hostname;
      break;
    case 7:
      temp=L"OPEN "+hostname;
      break;
    case 8:
      temp=L"USER "+GetOption(OPTION_FWUSER)+L"@"+hostname;
      break;
    case 9:
      temp=L"USER "+m_CurrentServer.user+L"@"+hostname+L" "+GetOption(OPTION_FWUSER);
      break;
    case 10:
      temp=L"USER "+m_CurrentServer.user+L"@"+GetOption(OPTION_FWUSER)+L"@"+hostname;
      break;
    case 11:
      if (needpass && !pData->waitForAsyncRequest && !pData->gotPassword)
      {
        CNeedPassRequestData *pNeedPassRequestData = new CNeedPassRequestData();
        pNeedPassRequestData->nRequestID = m_pOwner->GetNextAsyncRequestID();
        pNeedPassRequestData->nOldOpState = m_Operation.nOpState;
        m_Operation.nOpState = CONNECT_NEEDPASS;
        if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_NEEDPASS), (LPARAM)pNeedPassRequestData))
        {
          delete pNeedPassRequestData;
          ResetOperation(FZ_REPLY_ERROR);
        }
        else
        {
          m_bCheckForTimeout = FALSE;
        }
        pData->waitForAsyncRequest = true;
        return;
      }
      else if (!needpass || pData->gotPassword)
      {
        temp=L"PASS "+NormalizePass(m_CurrentServer.pass)+L"@"+GetOption(OPTION_FWPASS);
      }
      else
      {
        return;
      }
      break;
    case 12:
      if (m_CurrentServer.account == L"")
        temp = L"ACCT default";
      else
        temp = L"ACCT " + m_CurrentServer.account;
      break;
  }
  // send command, get response
  if(!Send(temp))
    return;
}

BOOL CFtpControlSocket::SendAuthSsl()
{
  if (!Send("AUTH SSL"))
    return false;
  m_Operation.nOpState = CONNECT_SSL_NEGOTIATE;
  return true;
}

#define BUFFERSIZE 4096
void CFtpControlSocket::OnReceive(int nErrorCode)
{
  m_LastRecvTime = CTime::GetCurrentTime();

  if (!m_pOwner->IsConnected())
  {
    if (!m_Operation.nOpMode)
    {
      LogMessage(FZ_LOG_INFO, L"Socket has been closed, don't process receive" );
      return;
    }
    m_MultiLine = "";
    CString str;
    str.Format(IDS_STATUSMSG_CONNECTEDWITH, (LPCTSTR)m_ServerName);
    ShowStatus(str, FZ_LOG_PROGRESS);
    m_pOwner->SetConnected(TRUE);
  }
  char *buffer = new char[BUFFERSIZE];
  int numread = Receive(buffer, BUFFERSIZE);

  if (numread == SOCKET_ERROR)
  {
    delete [] buffer;
    buffer = NULL;
    int Error = GetLastError();
    if (Error != WSAEWOULDBLOCK)
    {
      LogError(Error);
      ShowStatus(IDS_STATUSMSG_DISCONNECTED, FZ_LOG_ERROR);
      DoClose();
    }
    else
    {
      LogSocketMessageRaw(FZ_LOG_INFO, L"No data to read");
    }
    return;
  }
  if (!numread)
  {
    delete [] buffer;
    buffer = NULL;
    ShowStatus(IDS_STATUSMSG_DISCONNECTED, FZ_LOG_ERROR);
    DoClose();
  }
  if (LoggingMessageType(FZ_LOG_INFO))
  {
    CString str;
    str.Format(L"Read %d bytes", numread);
    LogSocketMessageRaw(FZ_LOG_INFO, str);
  }

  for (int i=0; i < numread; i++)
  {
    if ((buffer[i] == '\r') || (buffer[i] == '\n') || (buffer[i] == 0))
    {
      if (!m_RecvBuffer.empty() && m_RecvBuffer.back() != "")
      {
        USES_CONVERSION;
        if (m_bUTF8)
        {
          // convert from UTF-8 to ANSI
          LPCSTR utf8 = (LPCSTR)m_RecvBuffer.back();
          if (DetectUTF8Encoding(RawByteString(utf8)) == etANSI)
          {
            if (m_CurrentServer.nUTF8 != 1)
            {
              LogMessage(FZ_LOG_WARNING, L"Server does not send proper UTF-8, falling back to local charset");
              m_bUTF8 = false;
            }
            ShowStatus(A2CT(utf8), FZ_LOG_REPLY);
          }
          else
          {
            int len = MultiByteToWideChar(CP_UTF8, 0, utf8, -1, NULL, 0);
            if (!len)
              m_RecvBuffer.back() = "";
            else
            {
              LPWSTR p1 = new WCHAR[len + 1];
              MultiByteToWideChar(CP_UTF8, 0, utf8, -1 , (LPWSTR)p1, len + 1);
              ShowStatus(W2CT(p1), FZ_LOG_REPLY);
              delete [] p1;
            }
          }
        }
        else
        {
          ShowStatus(A2CT(m_RecvBuffer.back()), FZ_LOG_REPLY);
        }
        // Check for multi-line responses
        // Partially duplicated in TFTPFileSystem::HandleReplyStatus
        if (m_RecvBuffer.back().GetLength() > 3)
        {
          if (m_MultiLine != "")
          {
            if (m_RecvBuffer.back().Left(4) != m_MultiLine)
            {
              CStringA line = m_RecvBuffer.back();
              if (line.Left(4) == m_MultiLine.Left(3) + '-')
              {
                line = line.Mid(4, line.GetLength() - 4);
              }
              DiscardLine(line);
               m_RecvBuffer.pop_back();
            }
            else // end of multi-line found
            {
              m_MultiLine = "";
              LPARAM lParam = 0;
              #ifdef _DEBUG
              lParam = GetTickCount();
              #endif
              m_pOwner->PostThreadMessage(m_pOwner->m_nInternalMessageID, FZAPI_THREADMSG_PROCESSREPLY, lParam);
            }
          }
          // start of new multi-line
          else if (m_RecvBuffer.back()[3] == '-')
          {
            // DDD<SP> is the end of a multi-line response
            m_MultiLine = m_RecvBuffer.back().Left(3) + ' ';
            m_RecvBuffer.pop_back();
          }
          else
          {
            LPARAM lParam = 0;
            #ifdef _DEBUG
            lParam = GetTickCount();
            #endif
            m_pOwner->PostThreadMessage(m_pOwner->m_nInternalMessageID, FZAPI_THREADMSG_PROCESSREPLY, lParam);
          }
        }
        else
        {
          m_RecvBuffer.pop_back();
        }
        m_RecvBuffer.push_back("");
      }
    }
    else
    {
      //The command may only be 2000 chars long. This ensures that a malicious user can't
      //send extremely large commands to fill the memory of the server
      if (m_RecvBuffer.empty())
        m_RecvBuffer.push_back("");
      if (m_RecvBuffer.back().GetLength() < 2000)
        m_RecvBuffer.back() += buffer[i];
    }
  }

  delete [] buffer;
}

void CFtpControlSocket::ProcessReply()
{
  if (m_RecvBuffer.empty())
    return;

  if (m_awaitsReply)
  {
    if (m_sendBuffer)
      TriggerEvent(FD_WRITE);
    m_awaitsReply = false;
  }

  CString reply = GetReply();
  if ( reply == L"" )
    return;

  // After Cancel, we might have to skip a reply
  if (m_skipReply)
  {
    m_skipReply = false;
    m_RecvBuffer.pop_front();
    return;
  }

  if (m_bKeepAliveActive)
  {
    m_bKeepAliveActive = FALSE;
    m_pOwner->PostThreadMessage(m_pOwner->m_nInternalMessageID,FZAPI_THREADMSG_POSTKEEPALIVE,0);
  }
  else if (m_Operation.nOpMode&CSMODE_CONNECT)
    LogOnToServer();
  else if (m_Operation.nOpMode& (CSMODE_COMMAND|CSMODE_CHMOD) )
  {
    if (GetReplyCode()== 2 || GetReplyCode()== 3)
      ResetOperation(FZ_REPLY_OK);
    else
      ResetOperation(FZ_REPLY_ERROR);
  }
  else if (m_Operation.nOpMode&CSMODE_TRANSFER)
  {
    FileTransfer(0);
  }
  else if (m_Operation.nOpMode&CSMODE_LIST)
    List(FALSE);
  else if (m_Operation.nOpMode&CSMODE_LISTFILE)
    ListFile(L"", CServerPath());
  else if (m_Operation.nOpMode&CSMODE_DELETE)
    Delete( L"",CServerPath(), false);
  else if (m_Operation.nOpMode&CSMODE_RMDIR)
    RemoveDir( L"",CServerPath());
  else if (m_Operation.nOpMode&CSMODE_MKDIR)
    MakeDir(CServerPath());
  else if (m_Operation.nOpMode&CSMODE_RENAME)
    Rename(L"", L"", CServerPath(), CServerPath());

  if (!m_RecvBuffer.empty())
    m_RecvBuffer.pop_front();
}

void CFtpControlSocket::OnConnect(int nErrorCode)
{

  if (!m_Operation.nOpMode)
  {
    if (!m_pOwner->IsConnected())
      DoClose();
    return;
  }
  if (!nErrorCode)
  {
    if (!m_pOwner->IsConnected())
    {
      m_MultiLine = "";
      m_pOwner->SetConnected(TRUE);
      CString str;
      str.Format(
        m_pSslLayer ? IDS_STATUSMSG_CONNECTEDWITHSSL : IDS_STATUSMSG_CONNECTEDWITH,
        (LPCTSTR)m_ServerName);
      ShowStatus(str,FZ_LOG_PROGRESS);
    }
  }
  else
  {
    if (nErrorCode == WSAHOST_NOT_FOUND)
    {
      CString str;
      str.Format(IDS_ERRORMSG_CANTRESOLVEHOST2, (LPCTSTR)m_ServerName);
      ShowStatus(str, FZ_LOG_ERROR);
    }
    else
    {
      TCHAR Buffer[255];
      int Len = FormatMessage(
        FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ARGUMENT_ARRAY,
        NULL, nErrorCode, 0, Buffer, LENOF(Buffer), NULL);
      while ((Len > 0) && ((Buffer[Len - 1] >= 0) && (Buffer[Len - 1] <= 32)))
      {
        --Len;
      }
      ShowStatus(CString(Buffer, Len), FZ_LOG_ERROR);
    }
    DoClose();
  }
}

BOOL CFtpControlSocket::Send(CString str)
{
  USES_CONVERSION;

  ShowStatus(str, FZ_LOG_COMMAND);
  str += L"\r\n";
  int res = 0;
  if (m_bUTF8)
  {
    LPCWSTR unicode = T2CW(str);
    int len = WideCharToMultiByte(CP_UTF8, 0, unicode, -1, 0, 0, 0, 0);
    if (!len)
    {
      ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, FZ_LOG_ERROR);
      DoClose();
      return FALSE;
    }
    char* utf8 = new char[len + 1];
    WideCharToMultiByte(CP_UTF8, 0, unicode, -1, utf8, len + 1, 0, 0);

    int sendLen = strlen(utf8);
    if (!m_awaitsReply && !m_sendBuffer)
      res = CAsyncSocketEx::Send(utf8, strlen(utf8));
    else
      res = -2;
    if ((res == SOCKET_ERROR && GetLastError() != WSAEWOULDBLOCK) || !res)
    {
      delete [] utf8;
      ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, FZ_LOG_ERROR);
      DoClose();
      return FALSE;
    }
    if (res != sendLen)
    {
      if (res < 0)
        res = 0;
      if (!m_sendBuffer)
      {
        m_sendBuffer = new char[sendLen - res];
        memcpy(m_sendBuffer, utf8 + res, sendLen - res);
        m_sendBufferLen = sendLen - res;
      }
      else
      {
        char* tmp = new char[m_sendBufferLen + sendLen - res];
        memcpy(tmp, m_sendBuffer, m_sendBufferLen);
        memcpy(tmp + m_sendBufferLen, utf8 + res, sendLen - res);
        delete [] m_sendBuffer;
        m_sendBuffer = tmp;
        m_sendBufferLen += sendLen - res;
      }
    }
    delete [] utf8;
  }
  else
  {
    LPCSTR lpszAsciiSend = T2CA(str);

    int sendLen = strlen(lpszAsciiSend);
    if (!m_awaitsReply && !m_sendBuffer)
      res = CAsyncSocketEx::Send(lpszAsciiSend, strlen(lpszAsciiSend));
    else
      res = -2;
    if ((res == SOCKET_ERROR && GetLastError() != WSAEWOULDBLOCK) || !res)
    {
      ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, FZ_LOG_ERROR);
      DoClose();
      return FALSE;
    }
    if (res != sendLen)
    {
      if (res < 0)
        res = 0;
      if (!m_sendBuffer)
      {
        m_sendBuffer = new char[sendLen - res];
        memcpy(m_sendBuffer, lpszAsciiSend, sendLen - res);
        m_sendBufferLen = sendLen - res;
      }
      else
      {
        char* tmp = new char[m_sendBufferLen + sendLen - res];
        memcpy(tmp, m_sendBuffer, m_sendBufferLen);
        memcpy(tmp + m_sendBufferLen, lpszAsciiSend + res, sendLen - res);
        delete [] m_sendBuffer;
        m_sendBuffer = tmp;
        m_sendBufferLen += sendLen - res;
      }
    }
  }
  if (res > 0)
  {
    m_awaitsReply = true;
    m_LastSendTime = CTime::GetCurrentTime();
    // Count timeout since the last request, not only since the last received data
    // otherwise we may happen to timeout immediately after sending request if
    // CheckForTimeout occurs in between and we haven't received any data for a while
    m_LastRecvTime = m_LastSendTime;
  }
  return TRUE;
}

int CFtpControlSocket::TryGetReplyCode()
{
  if (m_RecvBuffer.empty())
    return 0;
  CStringA str = m_RecvBuffer.front();
  if (str == "")
  {
    return -1;
  }
  else if ((str[0] < '1') || (str[0] > '9'))
  {
    UnicodeString Error = FMTLOAD(FTP_MALFORMED_RESPONSE, (UnicodeString(str)));
    LogMessageRaw(FZ_LOG_WARNING, Error.c_str());
    return 0;
  }
  else
  {
    return str[0]-'0';
  }
}

int CFtpControlSocket::GetReplyCode()
{
  int Result = TryGetReplyCode();
  if (Result < 0)
  {
    UnicodeString Error = FMTLOAD(FTP_MALFORMED_RESPONSE, (UnicodeString()));
    LogMessageRaw(FZ_LOG_WARNING, Error.c_str());
    Result = 0;
  }
  return Result;
}

void CFtpControlSocket::DoClose(int nError /*=0*/)
{
  LogMessage(FZ_LOG_INFO, L"Connection closed");
  m_bCheckForTimeout=TRUE;
  m_pOwner->SetConnected(FALSE);
  m_bKeepAliveActive=FALSE;
  if (nError & FZ_REPLY_CRITICALERROR)
    nError |= FZ_REPLY_ERROR;
  ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_DISCONNECTED|nError);
  m_RecvBuffer.clear();
  m_MultiLine = "";
  m_bDidRejectCertificate = FALSE;

#ifndef MPEXT_NO_ZLIB
  m_useZlib = false;
  m_zlibSupported = false;
  m_zlibLevel = 0;
#endif

  m_bUTF8 = false;
  m_hasClntCmd = false;
  m_serverCapabilities.Clear();
  m_ListFile = "";

  m_awaitsReply = false;
  m_skipReply = false;

  delete [] m_sendBuffer;
  m_sendBuffer = 0;
  m_sendBufferLen = 0;

  m_bProtP = false;

  m_mayBeMvsFilesystem = false;
  m_mayBeBS2000Filesystem = false;

  Close();
}

void CFtpControlSocket::Disconnect()
{
  DebugAssert(!m_Operation.nOpMode);
  m_Operation.nOpMode=CSMODE_DISCONNECT;
  DoClose();
  ShowStatus(IDS_STATUSMSG_DISCONNECTED,FZ_LOG_STATUS); //Send the disconnected message to the message log
}

void CFtpControlSocket::CheckForTimeout()
{
  if (!m_Operation.nOpMode && !m_bKeepAliveActive)
    return;
  if (!m_bCheckForTimeout)
    return;
  int delay=GetOptionVal(OPTION_TIMEOUTLENGTH);
  if (m_pTransferSocket)
  {
    int res = m_pTransferSocket->CheckForTimeout(delay);
    if (res != 0)
    {
      if (res == 1)
      {
        // avoid trying to set keepalive command right after the transfer finishes
        m_LastSendTime = CTime::GetCurrentTime();
      }
      return;
    }
  }
  CTimeSpan span=CTime::GetCurrentTime()-m_LastRecvTime;
  if (span.GetTotalSeconds()>=delay)
  {
    ShowTimeoutError(IDS_CONTROL_CONNECTION);
    DoClose();
  }
}

void CFtpControlSocket::FtpCommand(LPCTSTR pCommand)
{
  m_Operation.nOpMode=CSMODE_COMMAND;
  Send(pCommand);
}

bool CFtpControlSocket::UsingMlsd()
{
  return
    // 0 = on, 1 = off, 2 = auto
    (m_CurrentServer.iUseMlsd == 0) ||
    ((m_CurrentServer.iUseMlsd != 1) &&
     (m_serverCapabilities.GetCapability(mlsd_command) == yes));
}

bool CFtpControlSocket::UsingUtf8()
{
  return m_bUTF8;
}

std::string CFtpControlSocket::GetTlsVersionStr()
{
  if (m_pSslLayer != NULL)
  {
    return m_pSslLayer->GetTlsVersionStr();
  }
  else
  {
    return std::string();
  }
}

std::string CFtpControlSocket::GetCipherName()
{
  if (m_pSslLayer != NULL)
  {
    return m_pSslLayer->GetCipherName();
  }
  else
  {
    return std::string();
  }
}

CString CFtpControlSocket::GetListingCmd()
{
  CString cmd;
  if (UsingMlsd())
  {
    cmd = L"MLSD";
  }
  else
  {
    cmd = L"LIST";
    if (GetOptionVal(OPTION_MPEXT_SHOWHIDDEN) && !(m_CurrentServer.nServerType & (FZ_SERVERTYPE_SUB_FTP_MVS | FZ_SERVERTYPE_SUB_FTP_VMS | FZ_SERVERTYPE_SUB_FTP_BS2000)))
      cmd += L" -a";
  }
  return cmd;
}

void CFtpControlSocket::List(BOOL bFinish, int nError /*=FALSE*/, CServerPath path /*=CServerPath()*/, CString subdir /*=L""*/)
{
  USES_CONVERSION;

  #define LIST_INIT  -1
  #define LIST_PWD  0
  #define LIST_CWD  1
  #define LIST_PWD2  2
  #define LIST_CWD2  3
  #define LIST_PWD3  4
  #define LIST_MODE  5
  #define LIST_OPTS  6
  #define LIST_PORT_PASV  7
  #define LIST_TYPE  8
  #define LIST_LIST  9
  #define LIST_WAITFINISH  10

  DebugAssert(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_LIST);

  m_Operation.nOpMode|=CSMODE_LIST;

  if (!m_pOwner->IsConnected())
  {
    ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_NOTCONNECTED);
    return;
  }

  if (bFinish || nError)
    if (m_Operation.nOpMode!=CSMODE_LIST)
      return; //Old message coming in

  if (nError)
  {
    delete m_pTransferSocket;
    m_pTransferSocket=0;
    if (nError&CSMODE_TRANSFERTIMEOUT)
      DoClose();
    else
      ResetOperation(FZ_REPLY_ERROR);
    return;
  }

  CListData *pData = static_cast<CListData *>(m_Operation.pData);

  if (bFinish)
  {
    if (!m_pTransferSocket || m_pTransferSocket->m_bListening)
    {
      delete m_pDirectoryListing;
      m_pDirectoryListing = 0;
      delete m_pTransferSocket;
      m_pTransferSocket = 0;
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }

    int num = 0;
    pData->pDirectoryListing = new t_directory;
    pData->pDirectoryListing->direntry = m_pTransferSocket->m_pListResult->getList(num);
    pData->pDirectoryListing->num = num;
    if (m_pTransferSocket->m_pListResult->m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP)
      m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;

    pData->pDirectoryListing->server = m_CurrentServer;
    pData->pDirectoryListing->path.SetServer(pData->pDirectoryListing->server);
    if (pData->rawpwd != L"")
    {
      if (!pData->pDirectoryListing->path.SetPath(pData->rawpwd))
      {
        delete m_pDirectoryListing;
        m_pDirectoryListing=0;
        delete m_pTransferSocket;
        m_pTransferSocket=0;
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }
      m_pOwner->SetCurrentPath(pData->pDirectoryListing->path);
    }
    else
      pData->pDirectoryListing->path = m_pOwner->GetCurrentPath();

    if (m_Operation.nOpState!=LIST_WAITFINISH)
    {
      return;
    }
    else
    {
      delete m_pTransferSocket;
      m_pTransferSocket=0;
    }
  }

  if (m_Operation.nOpState==LIST_WAITFINISH)
  {
    if (!bFinish)
    {
      if (pData->nFinish==-1)
      {
        int code=GetReplyCode();
        if (code== 2)
        {
          pData->nFinish=1;
        }
        else
          pData->nFinish=0;
      }
    }
    else
    {
      if (m_pTransferSocket)
        delete m_pTransferSocket;
      m_pTransferSocket=0;
    }
    if (pData->nFinish==0)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    else if (pData->pDirectoryListing && pData->nFinish==1)
    {
      ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL,FZ_LOG_PROGRESS);
      SetDirectoryListing(pData->pDirectoryListing);
      ResetOperation(FZ_REPLY_OK);
      return;
    }
    return;
  }
  else if (m_Operation.nOpState != LIST_INIT)
  {
    CString retmsg = GetReply();
    BOOL error = FALSE;
    int code = GetReplyCode();
    switch (m_Operation.nOpState)
    {
    case LIST_PWD: //Reply to PWD command
      if (code != 2 && code !=3 )
      {
        error = TRUE;
        break;
      }

      pData->rawpwd = retmsg;
      if ((m_mayBeMvsFilesystem || m_mayBeBS2000Filesystem) && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP &&
        pData->rawpwd[0] != L'/')
      {
        m_mayBeMvsFilesystem = false;
        m_mayBeBS2000Filesystem = false;
        if (m_mayBeBS2000Filesystem)
          m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_BS2000;
        else
          m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;

        if (!pData->path.IsEmpty())
          pData->path.SetServer(m_CurrentServer);
      }
      if (!ParsePwdReply(pData->rawpwd))
        return;
      if (pData->path.IsEmpty() || pData->path == m_pOwner->GetCurrentPath())
      {
        m_Operation.nOpState = NeedModeCommand() ? LIST_MODE : (NeedOptsCommand() ? LIST_OPTS : LIST_TYPE);
      }
      else
        m_Operation.nOpState = LIST_CWD;
      break;
    case LIST_CWD:
      if (code != 2 && code != 3)
        error = TRUE;
      m_Operation.nOpState = LIST_PWD2;
      break;
    case LIST_PWD2: //Reply to PWD command
      if (code !=2 && code != 3)
        error = TRUE;
      else
      {
        pData->rawpwd = retmsg;
        if (!ParsePwdReply(pData->rawpwd))
          return;
      }
      if (pData->subdir != L"")
      {
        if (pData->path != m_pOwner->GetCurrentPath())
        {
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
        m_Operation.nOpState = LIST_CWD2;
      }
      else
      {
        m_Operation.nOpState = NeedModeCommand() ? LIST_MODE : (NeedOptsCommand() ? LIST_OPTS : LIST_TYPE);
      }
      break;
    case LIST_CWD2:
      if (pData->lastCmdSentCDUP)
      {
        CString reply = GetReply().Left(3);
        int replycode = _ttoi(reply);
        if (replycode >= 500 && replycode < 505)
          break;
        pData->lastCmdSentCDUP = false;
      }
      if (code != 2 && code != 3)
        error = TRUE;
      m_Operation.nOpState = LIST_PWD3;
      break;
    case LIST_PWD3: //Reply to PWD command
      if (code != 2 && code != 3)
        error = TRUE;
      else
      {
        pData->rawpwd = retmsg;
        if (!ParsePwdReply(pData->rawpwd))
          return;
      }
      m_Operation.nOpState = NeedModeCommand() ? LIST_MODE : (NeedOptsCommand() ? LIST_OPTS : LIST_TYPE);
      break;
    case LIST_MODE:
#ifndef MPEXT_NO_ZLIB
      if (code == 2 || code == 3)
        m_useZlib = !m_useZlib;
#endif
      m_Operation.nOpState = NeedOptsCommand() ? LIST_OPTS : LIST_TYPE;
      break;
    case LIST_OPTS:
#ifndef MPEXT_NO_ZLIB
      if (code == 2 || code == 3)
        m_zlibLevel = pData->newZlibLevel;
#endif
      m_Operation.nOpState = LIST_TYPE;
      break;
    case LIST_TYPE:
      if (code!=2 && code!=3)
        error=TRUE;
      m_Operation.nOpState = LIST_PORT_PASV;
      break;
    case LIST_PORT_PASV:
      if (code!=2 && code!=3)
      {
        error=TRUE;
        break;
      }
      if (pData->bPasv)
      {
        CString temp;
        int i,j;
        // MP EXT
        if((i=retmsg.Find(L"("))>=0&&(j=retmsg.Find(L")"))>=0)
        {
          i++;
          j--;
        }
        else
        {
          // MP EXT
          if ((i=retmsg.Mid(4).FindOneOf(L"0123456789"))>=0)
          {
            i += 4;
            j = retmsg.GetLength() - 1;
          }
          else
          {
            if (!pData->bTriedPortPasvOnce)
            {
              pData->bTriedPortPasvOnce = TRUE;
              pData->bPasv = !pData->bPasv;
            }
            else
              error=TRUE;
            break;
          }
        }

        temp = retmsg.Mid(i,(j-i)+1);
        if (GetFamily() == AF_INET)
        {
          i=temp.ReverseFind(L',');
          pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
          temp=temp.Left(i);
          i=temp.ReverseFind(L',');
          pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
          pData->host = temp.Left(i);
          pData->host.Replace(L',', L'.');
          if (!CheckForcePasvIp(pData->host))
          {
            error = TRUE;
            break;
          }
        }
        else if (GetFamily() == AF_INET6)
        {
          temp = temp.Mid(3);
          pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
          if (pData->port < 0 || pData->port > 65535)
          {
            LogMessage(FZ_LOG_WARNING, L"Port %u not valid", pData->port);
            error = TRUE;
            break;
          }

          pData->host = m_CurrentServer.host;
        }
        else
        {
          LogMessage(FZ_LOG_WARNING, L"Protocol %d not supported", GetFamily());
          error = TRUE;
          break;
        }
      }
      m_Operation.nOpState = LIST_LIST;
      break;
    case LIST_LIST:
      if (IsMisleadingListResponse())
      {
        ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, FZ_LOG_PROGRESS);

        t_directory listing;
        listing.server = m_CurrentServer;
        listing.path = m_pOwner->GetCurrentPath();

        SetDirectoryListing(&listing);
        ResetOperation(FZ_REPLY_OK);
        return;
      }
      else if (code != 1)
        error = TRUE;
      else
        m_Operation.nOpState = LIST_WAITFINISH;
      break;
    default:
      error = TRUE;
    }

    if (error)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
  }
  if (m_Operation.nOpState==LIST_INIT)
  { //Initialize some variables
    pData=new CListData;
    pData->path=path;
    pData->subdir=subdir;
    m_Operation.pData=pData;
    ShowStatus(IDS_STATUSMSG_RETRIEVINGDIRLIST, FZ_LOG_PROGRESS);
    pData->nFinish=-1;
    if (m_pDirectoryListing)
    {
      delete m_pDirectoryListing;
      m_pDirectoryListing=0;
    }

    if (GetOptionVal(OPTION_PROXYTYPE)!=PROXYTYPE_NOPROXY)
      pData->bPasv = TRUE;
    else if (m_CurrentServer.nPasv == 1)
      pData->bPasv = TRUE;
    else if (m_CurrentServer.nPasv == 2)
      pData->bPasv = FALSE;
    else
      pData->bPasv = GetOptionVal(OPTION_PASV);

    CServerPath path = pData->path;
    CServerPath realpath = m_pOwner->GetCurrentPath();
    if (!realpath.IsEmpty())
    {
      if (!pData->path.IsEmpty() && pData->path != realpath)
        m_Operation.nOpState=LIST_CWD;
      else if (!pData->path.IsEmpty() && pData->subdir!=L"")
        m_Operation.nOpState=LIST_CWD2;
      else
      {
        m_Operation.nOpState = NeedModeCommand() ? LIST_MODE : (NeedOptsCommand() ? LIST_OPTS : LIST_TYPE);;
      }
    }
    else
      m_Operation.nOpState = LIST_PWD;
  }
  CString cmd;
  if (m_Operation.nOpState == LIST_PWD)
    cmd=L"PWD";
  else if (m_Operation.nOpState==LIST_CWD)
    cmd=L"CWD " + pData->path.GetPathUnterminated(); //Command to retrieve the current directory
  else if (m_Operation.nOpState==LIST_PWD2)
    cmd=L"PWD";
  else if (m_Operation.nOpState==LIST_CWD2)
  {
    if (!pData->subdir)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    if (pData->subdir != L".." )
    {
      if (m_CurrentServer.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS)
      {
        CServerPath path = m_pOwner->GetCurrentPath();
        path.AddSubdir(pData->subdir);
        cmd = L"CWD " + path.GetPathUnterminated();
      }
      else
        cmd = L"CWD " + pData->subdir;
    }
    else
    {
      if (pData->lastCmdSentCDUP)
      {
        pData->lastCmdSentCDUP = false;
        cmd = L"CWD ..";
      }
      else
      {
        pData->lastCmdSentCDUP = true;
        cmd = L"CDUP";
      }
    }
  }
  else if (m_Operation.nOpState == LIST_PWD3)
    cmd=L"PWD";
  else if (m_Operation.nOpState == LIST_MODE)
  {
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    if (m_useZlib)
#endif
      cmd = L"MODE S";
#ifndef MPEXT_NO_ZLIB
    else
      cmd = L"MODE Z";
#endif
  }
  else if (m_Operation.nOpState == LIST_OPTS)
  {
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    pData->newZlibLevel = GetOptionVal(OPTION_MODEZ_LEVEL);
    cmd.Format(L"OPTS MODE Z LEVEL %d", pData->newZlibLevel);
#endif
  }
  else if (m_Operation.nOpState == LIST_PORT_PASV)
  {
    m_pTransferSocket = new CTransferSocket(this, m_Operation.nOpMode);
#ifndef MPEXT_NO_ZLIB
    if (m_useZlib)
    {
      if (!m_pTransferSocket->InitZlib(m_zlibLevel))
      {
        ShowStatus(L"Failed to initialize zlib", FZ_LOG_ERROR);
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }
    }
#endif
    m_pTransferSocket->m_nInternalMessageID = m_pOwner->m_nInternalMessageID;
#ifndef MPEXT_NO_GSS
    if (m_pGssLayer && m_pGssLayer->AuthSuccessful())
      m_pTransferSocket->UseGSS(m_pGssLayer);
#endif
    m_pTransferSocket->SetFamily(GetFamily());
    if (!m_pTransferSocket->Create(m_pSslLayer && m_bProtP) ||
        !m_pTransferSocket->AsyncSelect())
    {
      ShowStatus(L"Failed to create socket", FZ_LOG_ERROR);
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    if (pData->bPasv)
      switch (GetFamily())
      {
      case AF_INET:
        cmd = L"PASV";
        break;
      case AF_INET6:
        cmd = L"EPSV";
        break;
      default:
        LogMessage(FZ_LOG_WARNING, L"Protocol %d not supported", GetFamily());
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }
    else
    {
      m_pTransferSocket->m_bListening=TRUE;
      if (m_pProxyLayer)
      {
        SOCKADDR_IN addr;
        int len=sizeof(addr);
        if (!m_pProxyLayer->GetPeerName((SOCKADDR *)&addr,&len))
        {
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
        else if (!m_pTransferSocket->Listen(addr.sin_addr.S_un.S_addr))
        {
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
      else
      {
        //Set up an active file transfer
        CString tempHostname;
        UINT nPort;

        if (// create listen socket (let MFC choose the port) & start the socket listening
          !m_pTransferSocket->Listen() ||
          !m_pTransferSocket->GetSockName(tempHostname, nPort))
        {
          ShowStatus(L"Failed to create listen socket", FZ_LOG_ERROR);
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }

        CString host;
        bool bError = false;

        if (GetFamily() == AF_INET)
        {
          host = GetOption(OPTION_TRANSFERIP);
          if (host != L"")
          {
            DWORD ip = inet_addr(T2CA(host));
            if (ip != INADDR_NONE)
              host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
            else
            {
              hostent *fullname = gethostbyname(T2CA(host));
              if (!fullname)
                host = L"";
              else
              {
                DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
                if (ip != INADDR_NONE)
                  host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
                else
                  host = L"";
              }
            }
          }
          if (host == L"")
          {
            UINT temp;

            if (!GetSockName(host, temp))
            {
              ShowStatus(L"Failed to get socket address ", FZ_LOG_ERROR);
              bError = true;
            }

            host.Replace(L'.', L',');
          }

          if (!bError)
          {
            host.Format(host+L",%d,%d", nPort/256, nPort%256);
            cmd = L"PORT " + host; // send PORT cmd to server
          }
        }
        else if (GetFamily() == AF_INET6)
        {
          host = GetOption(OPTION_TRANSFERIP6);
          if (host != L"")
          {
            addrinfo hints, *res;
            memset(&hints, 0, sizeof(addrinfo));
            hints.ai_family = AF_INET6;
            hints.ai_socktype = SOCK_STREAM;
            if (!getaddrinfo(T2CA(host), "1024", &hints, &res))
            {
              host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
              freeaddrinfo(res);
            }
            else
              host = L"";
          }
          if (host == L"")
          {
            UINT temp;

            if(!GetSockName(host, temp))
              bError = true;
          }

          if (!bError)
          {
            // assamble EPRT command
            cmd.Format(L"EPRT |2|" +  host + L"|%d|", nPort);
          }
        }
        else
        {
          LogMessage(FZ_LOG_WARNING, L"Protocol %d not supported", GetFamily());
          bError = true;
        }

        if (bError)
        {
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
    }
  }
  else if (m_Operation.nOpState==LIST_TYPE)
    cmd=L"TYPE A";
  else if (m_Operation.nOpState==LIST_LIST)
  {
    if (!m_pTransferSocket)
    {
      LogMessage(FZ_LOG_APIERROR, L"Error: m_pTransferSocket==NULL" );
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }

    m_pTransferSocket->SetActive();

    cmd = GetListingCmd();
    if (!Send(cmd))
      return;

    if (pData->bPasv)
    {
      // if PASV create the socket & initiate outbound data channel connection
      if (!ConnectTransferSocket(pData->host, pData->port))
      {
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }
    }

    return;
  }
  if (cmd != L"")
    Send(cmd);
}

bool CFtpControlSocket::ConnectTransferSocket(const CString & host, UINT port)
{
  CString hostname;
  hostname.Format(L"%s:%d", (LPCTSTR)host, port);
  CString str;
  str.Format(IDS_STATUSMSG_CONNECTING, (LPCTSTR)hostname);
  ShowStatus(str, FZ_LOG_PROGRESS);

  bool result = true;
  if (!m_pTransferSocket->Connect(host, port))
  {
    if (GetLastError() != WSAEWOULDBLOCK)
    {
      result = false;
    }
    else
    {
      LogMessage(FZ_LOG_INFO, L"Connection pending");
    }
  }
  else
  {
    LogMessage(FZ_LOG_INFO, L"Connected");
  }
  return result;
}

void CFtpControlSocket::ListFile(CString filename, const CServerPath &path)
{
  #define LISTFILE_INIT  -1
  #define LISTFILE_MLST  1
  #define LISTFILE_TYPE  2
  #define LISTFILE_SIZE  3
  #define LISTFILE_MDTM  4
  #define LISTFILE_PWD   5
  #define LISTFILE_CWD   6
  #define LISTFILE_CWD2  7

  DebugAssert(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_LISTFILE);

  m_Operation.nOpMode|=CSMODE_LISTFILE;

  if (!m_pOwner->IsConnected())
  {
    ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_NOTCONNECTED);
    return;
  }

  CListFileData * pData = static_cast<CListFileData *>(m_Operation.pData);

  BOOL error = FALSE;
  CString cmd;
  CString retmsg;
  int code = -1;
  int num = -1;
  switch (m_Operation.nOpState)
  {
  case LISTFILE_INIT:
    //Initialize some variables
    pData = new CListFileData;
    pData->fileName = filename;
    pData->dir = path.GetPath();
    // special case for listing a root folder
    pData->path = (filename == L"/") ? pData->dir : path.FormatFilename(filename);
    m_Operation.pData = pData;
    ShowStatus(IDS_STATUSMSG_RETRIEVINGLISTFILE, FZ_LOG_PROGRESS);
    if (UsingMlsd())
    {
      m_Operation.nOpState = LISTFILE_MLST;
      cmd = L"MLST " + pData->path;
    }
    else
    {
      m_Operation.nOpState = LISTFILE_PWD;
      cmd = L"PWD";
    }
    if (!Send(cmd))
    {
      error = TRUE;
    }
    break;
  case LISTFILE_MLST:
    retmsg = GetReply();
    code = GetReplyCode();
    if (IsMisleadingListResponse())
    {
      ShowStatus(IDS_STATUSMSG_LISTFILESUCCESSFUL, FZ_LOG_PROGRESS);
      num = 0;
    }
    else if (code != 2)
    {
      error = TRUE;
    }
    else
    {
      CStringA Buf = m_ListFile + '\n';
      const bool mlst = true;
      CFtpListResult * pListResult = CreateListResult(mlst);
      pListResult->AddData(static_cast<const char *>(Buf), Buf.GetLength());
      pData->direntry = pListResult->getList(num);
      if (pListResult->m_server.nServerType & FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP)
        m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
      delete pListResult;
    }
    break;
  case LISTFILE_PWD:
    code = GetReplyCode();
    retmsg = GetReply();
    if ((code == 2) &&
        ParsePwdReply(retmsg, pData->pwd) &&
        Send(L"CWD " + pData->path))
    {
      m_Operation.nOpState = LISTFILE_CWD;
    }
    else
    {
      error = TRUE;
    }
    break;
  case LISTFILE_CWD:
    code = GetReplyCode();
    pData->direntry = new t_directory::t_direntry[1];
    pData->direntry->name = pData->fileName;
    if (code == 2)
    {
      pData->direntry->dir = TRUE;
      if (Send(L"CWD " + pData->pwd.GetPathUnterminated()))
      {
        m_Operation.nOpState = LISTFILE_CWD2;
      }
      else
      {
        error = TRUE;
      }
    }
    else
    {
      // CWD failed, file is not a directory, we should not need to restore PWD.

      // Force binary mode, as according to RFC 6359,
      // SIZE command returns size as transferred over the stream.
      // Moreover ProFTPD does not even support SIZE command in ASCII mode
      if (Send(L"TYPE I"))
      {
        m_Operation.nOpState = LISTFILE_TYPE;
      }
      else
      {
        error = TRUE;
      }
    }
    break;
  case LISTFILE_CWD2:
    code = GetReplyCode();
    if (code == 2)
    {
      // No point trying SIZE on directories.
      // (More over IIS returns a multi-line response for SIZE /dir and we cannot handle that).
      // IIS fails even for MDTM, so skipping even that.
      num = 1;
    }
    // this should never really happen
    else
    {
      error = TRUE;
    }
    break;
  case LISTFILE_TYPE:
    // Do not really care if TYPE succeeded or not
    if (Send(L"SIZE " + pData->path))
    {
      m_Operation.nOpState = LISTFILE_SIZE;
    }
    else
    {
      error = TRUE;
    }
    break;
  case LISTFILE_SIZE:
    code = GetReplyCode();
    // Ignore SIZE errors for directories
    if ((HandleSize(code, pData->direntry->size) || pData->direntry->dir) &&
        Send(L"MDTM " + pData->path))
    {
      m_Operation.nOpState = LISTFILE_MDTM;
    }
    else
    {
      error = TRUE;
    }
    break;
  case LISTFILE_MDTM:
    code = GetReplyCode();
    if (HandleMdtm(code, pData->direntry->date))
    {
      num = 1;
    }
    else
    {
      error = TRUE;
    }
    break;
  default:
    error = TRUE;
    break;
  }

  if (error)
  {
    ResetOperation(FZ_REPLY_ERROR);
  }
  else if (num >= 0)
  {
    t_directory * pDirectoryListing = new t_directory;
    pDirectoryListing->direntry = pData->direntry;
    pData->direntry = NULL;
    pDirectoryListing->num = num;
    pDirectoryListing->server = m_CurrentServer;
    pDirectoryListing->path.SetServer(pDirectoryListing->server);
    pDirectoryListing->path = pData->dir;
    ShowStatus(IDS_STATUSMSG_LISTFILESUCCESSFUL,FZ_LOG_PROGRESS);
    // do not use SetDirectoryListing as that would make
    // later operations believe that there's only this one file in the folder
    m_pOwner->SendDirectoryListing(pDirectoryListing);
    ResetOperation(FZ_REPLY_OK);
  }
}

void CFtpControlSocket::TransferEnd(int nMode)
{
  if (!m_Operation.nOpMode)
  {
    LogMessage(FZ_LOG_INFO, L"Ignoring old TransferEnd message");
    return;
  }
  m_LastRecvTime=CTime::GetCurrentTime();
  if (m_Operation.nOpMode&CSMODE_TRANSFER)
    FileTransfer(0,TRUE,nMode&(CSMODE_TRANSFERERROR|CSMODE_TRANSFERTIMEOUT));
  else if (m_Operation.nOpMode&CSMODE_LIST)
    List(TRUE,nMode&(CSMODE_TRANSFERERROR|CSMODE_TRANSFERTIMEOUT));
}

void CFtpControlSocket::OnClose(int nErrorCode)
{
  ShowStatus(IDS_STATUSMSG_DISCONNECTED, FZ_LOG_ERROR);
  if (m_pTransferSocket)
  {
    m_pTransferSocket->OnClose(0);
    m_pTransferSocket->Close();
    delete m_pTransferSocket;
    m_pTransferSocket=0;
    DoClose();
    return;
  }
  if (m_bDidRejectCertificate)
    DoClose(FZ_REPLY_CANCEL);
  else
    DoClose();
}

void CFtpControlSocket::ResetTransferSocket(int Error)
{
  if (Error)
  {
    LogMessage(FZ_LOG_INFO, L"Destroying data socket on error");
  }
  else
  {
    LogMessage(FZ_LOG_INFO, L"Destroying data socket after transfer completed");
  }
  bool Close =
    (Error != 0) &&
    DebugAlwaysTrue(m_pTransferSocket != NULL) &&
    (m_pTransferSocket->m_uploaded > 0) &&
    FLAGCLEAR(Error, FZ_REPLY_CANCEL);
  delete m_pTransferSocket;
  m_pTransferSocket = NULL;
  if (Close)
  {
    // close the control connection too to allow reconnect => transfer resume
    LogMessage(FZ_LOG_WARNING, L"Transfer connection failed, closing");
    DoClose();
  }
}

int CFtpControlSocket::OpenTransferFile(CFileTransferData * pData)
{
  int nReplyError = 0;
  bool res;
  if (pData->transferfile.get)
  {
    if (pData->transferfile.OnTransferOut == NULL)
    {
      m_pDataFile = new CFile();
      if (pData->transferdata.bResume)
        res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeCreate|CFile::modeWrite|CFile::modeNoTruncate|CFile::shareDenyWrite);
      else
        res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeWrite|CFile::modeCreate|CFile::shareDenyWrite);
    }
    else
    {
      res = true;
    }
  }
  else
  {
    if (pData->transferfile.OnTransferIn == NULL)
    {
      m_pDataFile = new CFile();
      res = m_pDataFile->Open(pData->transferfile.localfile,CFile::modeRead|CFile::shareDenyNone);
    }
    else
    {
      res = true;
    }
  }
  if (!res)
  {
    wchar_t * Error = m_pTools->LastSysErrorMessage();
    //Error opening the file
    CString str;
    str.Format(IDS_ERRORMSG_FILEOPENFAILED,(LPCTSTR)pData->transferfile.localfile);
    str += L"\n";
    str += Error;
    free(Error);
    ShowStatus(str,FZ_LOG_ERROR);
    nReplyError = FZ_REPLY_ERROR;
  }
  else
  {
    m_pTransferSocket->m_pFile = m_pDataFile;
    m_pTransferSocket->m_OnTransferOut = pData->transferfile.OnTransferOut;
    m_pTransferSocket->m_OnTransferIn = pData->transferfile.OnTransferIn;
  }

  return nReplyError;
}

int CFtpControlSocket::ActivateTransferSocket(CFileTransferData * pData)
{
  int nReplyError = 0;
  if (pData->transferfile.get && (m_pDataFile == NULL))
  {
    nReplyError = OpenTransferFile(pData);
  }
  if (!nReplyError)
  {
    m_pTransferSocket->SetActive();
  }
  return nReplyError;
}

void CFtpControlSocket::CancelTransferResume(CFileTransferData * pData)
{
  pData->transferdata.transferleft = pData->transferdata.transfersize;
  pData->transferdata.bResume = FALSE;
}

void CFtpControlSocket::FileTransfer(t_transferfile *transferfile/*=0*/,BOOL bFinish/*=FALSE*/,int nError/*=0*/)
{
  USES_CONVERSION;

  #define FILETRANSFER_INIT      -1
  #define FILETRANSFER_PWD      0
  #define FILETRANSFER_CWD      1
  #define FILETRANSFER_MKD      2
  #define FILETRANSFER_CWD2      3
  #define FILETRANSFER_PWD2      4
  #define FILETRANSFER_LIST_MODE    5
  #define FILETRANSFER_LIST_OPTS    6
  #define FILETRANSFER_LIST_PORTPASV  7
  #define FILETRANSFER_LIST_TYPE    8
  #define FILETRANSFER_LIST_LIST    9
  #define FILETRANSFER_LIST_WAITFINISH  10
  #define FILETRANSFER_NOLIST_SIZE  11
  #define FILETRANSFER_NOLIST_MDTM  12
  #define FILETRANSFER_TYPE      13
  #define FILETRANSFER_REST      14
  #define FILETRANSFER_MODE      15
  #define FILETRANSFER_OPTS      16
  #define FILETRANSFER_PORTPASV    17
  #define FILETRANSFER_RETRSTOR    18
  #define FILETRANSFER_WAITFINISH    19

  #define FILETRANSFER_WAIT      20

  #define FILETRANSFER_MFMT      21
  #define FILETRANSFER_OPTS_REST 22

  #define FILETRANSFER_OPTION_COMMAND_2 (NeedOptsCommand() ? FILETRANSFER_OPTS : FILETRANSFER_PORTPASV)
  #define FILETRANSFER_OPTION_COMMAND_1 (NeedModeCommand() ? FILETRANSFER_MODE : FILETRANSFER_OPTION_COMMAND_2)

  //Partial flowchart of FileTransfer
  //
  //            +----+
  //     /------|Init|--------\
  //     |      +----+        |
  //     |         |          |
  //     |     /---+          |
  //     |     |   |          |
  //     |     | +---+        |
  //     |     | |PWD|        |
  //     |     | +---+        |
  //     |     |   |          |
  //     |     \---+          |
  //     |         |          |
  //     |       +---+        |
  //     |       |CWD|--\     |
  //     |       +---+  |     |
  //     |         |    |     |
  //     |         |    |     |
  //     |         |  +---+   |
  //     |         |  |MKD|   |
  //     |         |  +---+   |
  //     |         |    |     |
  //     |         |    |     |
  //     |         |  +----+  |
  //     |         |  |CWD2|  |
  //     |         |  +----+  |
  //     |         |    |     |
  //     |         +----/     |
  //     |         |          |
  //     |       +---+        |
  //     +-------|PWD|        |
  //     |       +---+        |
  //     |         |          |
  //     |         +----------/
  //     |         |
  //     |    +---------+
  //     |    |LIST_TYPE|
  //     |    +---------+
  //     |         |
  //     |         |
  //     |  +-------------+
  //     |  |LIST_PORTPASV|
  //     |  +-------------+
  //     |         |
  //     |         |
  //     |    +---------+
  //     |    |LIST_LIST|-----\ //List fails, maybe folder is list protected
  //     |    +---------+     | //Use SIZE and MDTM to get file information
  //     |         |        +----+
  //     |         |        |SIZE|
  //     |         |        +----+
  //     |         |          |
  //     |         |        +----+
  //     |         |        |MDTM|
  //     |         |        +----+
  //     |         |          |
  //     |         |          |
  //     | +---------------+  |
  //     | |LIST_WAITFINISH|  |
  //     | +---------------+  |
  //     |         |          |
  //     |         |          |
  //     |         +----------/
  //     |         |
  //     \---------+
  //               |
  //            +----+
  //            |TYPE|
  //            +----+
  //               |
  //               |
  //          +--------+
  //          |PORTPASV|--\
  //          +--------+  |
  //               |      |
  //               |      |
  //               |   +----+
  //               |   |REST|
  //               |   +----+
  //         |      |
  //               +------/
  //               |
  //          +--------+
  //          |RETRSTOR|
  //          +--------+
  //               |
  //               |
  //         +----------+
  //         |WAITFINISH|
  //         +----------+

  DebugAssert(!m_Operation.nOpMode || m_Operation.nOpMode&CSMODE_TRANSFER);
  if (!m_pOwner->IsConnected())
  {
    m_Operation.nOpMode=CSMODE_TRANSFER|(transferfile->get?CSMODE_DOWNLOAD:CSMODE_UPLOAD);
    ResetOperation(FZ_REPLY_ERROR|FZ_REPLY_DISCONNECTED);
    return;
  }

  CFileTransferData *pData=static_cast<CFileTransferData *>(m_Operation.pData);

  //Process finish and error messages
  if (bFinish || nError)
  {
    DebugAssert(m_Operation.nOpMode&CSMODE_TRANSFER);

    // APPE failed, ignore this reply
    if (m_Operation.nOpMode == FILETRANSFER_WAIT && bFinish)
      return;

    if (!(m_Operation.nOpMode&CSMODE_TRANSFER))
      return;

    if (nError)
    {
      if (m_Operation.nOpState == FILETRANSFER_LIST_LIST && nError & CSMODE_TRANSFERERROR)
      { //Don't abort operation, use fallback to SIZE and MDTM (when actual LIST reply comes in)
        if (m_pTransferSocket)
          m_pTransferSocket=0;
        delete m_pDirectoryListing;
        m_pDirectoryListing=0;
      }
      else if (nError&CSMODE_TRANSFERTIMEOUT)
        DoClose();
      else
      {
        // we may get here when connection was closed, when the closure
        // was first detected while reading/writing,
        // when we abort file transfer with regular error,
        // possibly preventing automatic reconnect
        LogMessage(FZ_LOG_INFO, L"Transfer error (%x)", nError);
        ResetOperation(FZ_REPLY_ERROR);
      }
      return;
    }
    if (m_Operation.nOpState <= FILETRANSFER_LIST_PORTPASV)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    else if (m_Operation.nOpState<=FILETRANSFER_LIST_WAITFINISH)
    {
      if (!m_pTransferSocket || m_pTransferSocket->m_bListening)
      {
        delete m_pDirectoryListing;
        m_pDirectoryListing=0;
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }

      int num=0;
      pData->pDirectoryListing=new t_directory;
      pData->pDirectoryListing->direntry=m_pTransferSocket->m_pListResult->getList(num);
      pData->pDirectoryListing->num=num;
      if (m_pTransferSocket->m_pListResult->m_server.nServerType&FZ_SERVERTYPE_SUB_FTP_VMS && m_CurrentServer.nServerType&FZ_SERVERTYPE_FTP)
        m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_VMS;
      pData->pDirectoryListing->server = m_CurrentServer;
      pData->pDirectoryListing->path.SetServer(m_CurrentServer);
      pData->pDirectoryListing->path = pData->transferfile.remotepath;
      if (pData->rawpwd!=L"")
      {
        if (!pData->pDirectoryListing->path.SetPath(pData->rawpwd))
        {
          delete m_pDirectoryListing;
          m_pDirectoryListing=0;
          delete m_pTransferSocket;
          m_pTransferSocket=0;
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
      else
        pData->pDirectoryListing->path=pData->transferfile.remotepath;

      if (m_Operation.nOpState!=FILETRANSFER_LIST_WAITFINISH)
        return;
    }
    else if (m_Operation.nOpState <= FILETRANSFER_PORTPASV)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    else if (m_Operation.nOpState<=FILETRANSFER_WAITFINISH)
    {
      if (m_pTransferSocket->m_bListening)
      {
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }
      pData->nGotTransferEndReply |= 2;
      if (m_Operation.nOpState!=FILETRANSFER_WAITFINISH)
        return;
      else
      {
        ResetTransferSocket(nError);
      }
    }
  }

  //////////////////
  //Initialization//
  //////////////////
  int nReplyError = 0;
  if (m_Operation.nOpState == FILETRANSFER_INIT)
  {
    DebugAssert(transferfile);
    DebugAssert(!m_Operation.nOpMode);
    DebugAssert(!m_Operation.pData);

    if ((transferfile->OnTransferOut == NULL) &&
        (transferfile->OnTransferIn == NULL))
    {
      CString str;
      str.Format(transferfile->get?IDS_STATUSMSG_DOWNLOADSTART:IDS_STATUSMSG_UPLOADSTART,
            transferfile->get ? (LPCTSTR)transferfile->remotepath.FormatFilename(transferfile->remotefile) : (LPCTSTR)transferfile->localfile);
      ShowStatus(str,FZ_LOG_STATUS);
    }

    m_Operation.nOpMode=CSMODE_TRANSFER|(transferfile->get?CSMODE_DOWNLOAD:CSMODE_UPLOAD);

    m_Operation.pData=new CFileTransferData;
    pData=static_cast<CFileTransferData *>(m_Operation.pData);

    if (GetOptionVal(OPTION_PROXYTYPE)!=PROXYTYPE_NOPROXY)
      pData->bPasv = TRUE;
    else if (m_CurrentServer.nPasv == 1)
      pData->bPasv = TRUE;
    else if (m_CurrentServer.nPasv == 2)
      pData->bPasv = FALSE;
    else
      pData->bPasv = GetOptionVal(OPTION_PASV);

    //Replace invalid characters in the local filename
    int pos=transferfile->localfile.ReverseFind(L'\\');
    for (int i=(pos+1);i<transferfile->localfile.GetLength();i++)
      if (transferfile->localfile[i]==L':')
        transferfile->localfile.SetAt(i, L'_');

    pData->transferfile=*transferfile;
    pData->transferdata.transfersize=pData->transferfile.size;
    pData->transferdata.transferleft=pData->transferfile.size;
    pData->transferdata.bResume = FALSE;
    pData->transferdata.bResumeAppend = FALSE;
    pData->transferdata.bType = (pData->transferfile.nType == 1) ? TRUE : FALSE;

    CServerPath path;
    DebugCheck(m_pOwner->GetCurrentPath(path));
    if (path == pData->transferfile.remotepath)
    {
      if (m_pDirectoryListing)
      {
        m_Operation.nOpState=FILETRANSFER_TYPE;
        CString remotefile=pData->transferfile.remotefile;
        int i;
        for (i=0; i<m_pDirectoryListing->num; i++)
        {
          if (m_pDirectoryListing->direntry[i].name==remotefile &&
            ( m_pDirectoryListing->direntry[i].bUnsure || m_pDirectoryListing->direntry[i].size==-1 ))
          {
            delete m_pDirectoryListing;
            m_pDirectoryListing=0;
            m_Operation.nOpState = FileTransferListState(transferfile->get);
            break;
          }
        }
        if (m_pDirectoryListing && i==m_pDirectoryListing->num)
        {
          nReplyError = CheckOverwriteFileAndCreateTarget();
        }
      }
      else
      {
        m_Operation.nOpState = FileTransferListState(transferfile->get);
      }
    }
    else
    {
      if (pData->transferfile.remotepath.IsEmpty() && GetOptionVal(OPTION_MPEXT_WORK_FROM_CWD))
        m_Operation.nOpState = FileTransferListState(pData->transferfile.get);
      else if (path.IsEmpty())
        m_Operation.nOpState = FILETRANSFER_PWD;
      else
        m_Operation.nOpState = FILETRANSFER_CWD;
    }
  }
  else
  {
    ///////////
    //Replies//
    ///////////
    int code = TryGetReplyCode();
    // We do not always expect a response here, particularly when closing transfer connection (FILETRANSFER_WAITFINISH).
    // The normalization to 0 is probably not needed.
    if (code < 0)
    {
      code = 0;
    }
    switch(m_Operation.nOpState)
    {
    case FILETRANSFER_PWD:
      if (code != 2 && code != 3)
      {
        nReplyError = FZ_REPLY_ERROR;
        break;
      }

      pData->rawpwd = GetReply();
      if ((m_mayBeMvsFilesystem || m_mayBeBS2000Filesystem) && m_CurrentServer.nServerType & FZ_SERVERTYPE_FTP &&
        pData->rawpwd[0] != L'/')
      {
        m_mayBeMvsFilesystem = false;
        m_mayBeBS2000Filesystem = false;

        if (m_mayBeBS2000Filesystem)
          m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_BS2000;
        else
          m_CurrentServer.nServerType |= FZ_SERVERTYPE_SUB_FTP_MVS;

        pData->transferfile.remotepath.SetServer(m_CurrentServer);
      }
      if (!ParsePwdReply(pData->rawpwd))
        return;

      if (m_pOwner->GetCurrentPath() == pData->transferfile.remotepath)
      {
        m_Operation.nOpState = FileTransferListState(pData->transferfile.get);
      }
      else
        m_Operation.nOpState = LIST_CWD;
      break;
    case FILETRANSFER_CWD:
      if (code != 2 && code != 3)
        if (pData->transferfile.get)
        {
          pData->bUseAbsolutePaths = TRUE;
          m_Operation.nOpState = FILETRANSFER_NOLIST_SIZE;
        }
        else
          m_Operation.nOpState = FILETRANSFER_MKD;
      else
        m_Operation.nOpState = FILETRANSFER_PWD2;
      break;
    case FILETRANSFER_MKD:
      switch(pData->nMKDOpState)
      {
      case MKD_FINDPARENT:
        {
          if (code == 2 || code == 3)
          {
            m_pOwner->SetCurrentPath(pData->MKDCurrent);

            pData->nMKDOpState=MKD_CHANGETOSUBDIR;
            pData->MKDCurrent.AddSubdir(pData->MKDSegments.front());
            CString Segment=pData->MKDSegments.front();
            pData->MKDSegments.pop_front();
            if (!Send( L"MKD " + Segment))
              return;
          }
          else
          {
            if (!pData->MKDCurrent.HasParent())
              nReplyError = FZ_REPLY_ERROR | ((code == 5) ? FZ_REPLY_CRITICALERROR : 0);
            else
            {
              pData->MKDSegments.push_front(pData->MKDCurrent.GetLastSegment());
              pData->MKDCurrent=pData->MKDCurrent.GetParent();
              if (!Send(L"CWD "+pData->MKDCurrent.GetPathUnterminated()))
                return;
            }
          }
        }
        break;
      case MKD_MAKESUBDIRS:
        {
          if (code == 2 || code == 3)
          { //Create dir entry in parent dir
            DebugAssert(!pData->MKDSegments.empty());
            pData->MKDCurrent.AddSubdir(pData->MKDSegments.front());
            CString Segment=pData->MKDSegments.front();
            pData->MKDSegments.pop_front();
            if (Send( L"MKD " + Segment))
              pData->nMKDOpState=MKD_CHANGETOSUBDIR;
            else
              return;
          }
          else
            nReplyError=FZ_REPLY_ERROR;
        }
        break;
      case MKD_CHANGETOSUBDIR:
        {
          if (code == 2 || code == 3 || //Creation successful
            m_RecvBuffer.front() == "550 Directory already exists") //Creation was successful, although someone else did the work for us
          {
            CServerPath path2 = pData->MKDCurrent;
            if (path2.HasParent())
            {
              CString name=path2.GetLastSegment();
              path2=path2.GetParent();
              t_directory dir;
              BOOL res=FALSE;
              if (m_pDirectoryListing)
              {
                if (m_pDirectoryListing->path==path2)
                {
                  dir=*m_pDirectoryListing;
                  res=TRUE;
                }
              }
              t_directory WorkingDir;
              BOOL bFound=m_pOwner->GetWorkingDir(&WorkingDir);
              if (!res && bFound)
                if (WorkingDir.path==path2)
                {
                  dir=WorkingDir;
                  res=TRUE;
                }
              if (!res)
              {
                dir.path=path2;
                dir.server=m_CurrentServer;
              }

              int i;
              for (i=0; i<dir.num; i++)
                if (dir.direntry[i].name == name)
                {
                  LogMessage(FZ_LOG_WARNING, L"Dir already exists in cache!");
                  break;
                }
              if (i==dir.num)
              {
                t_directory::t_direntry *entries = new t_directory::t_direntry[dir.num+1];
                for (i=0;i<dir.num;i++)
                  entries[i]=dir.direntry[i];
                entries[i].name=name;
                entries[i].dir=TRUE;
                entries[i].date.hasdate=FALSE;
                entries[i].size=-1;
                entries[i].bUnsure=FALSE;
                delete [] dir.direntry;
                dir.direntry=entries;
                dir.num++;
                BOOL updated=FALSE;
                if (m_pDirectoryListing && m_pDirectoryListing->path==dir.path)
                {
                  updated=TRUE;
                  SetDirectoryListing(&dir);
                }
                if (!updated)
                  if (WorkingDir.path==dir.path)
                  {
                    updated=TRUE;
                    m_pOwner->SetWorkingDir(&dir);
                  }
              }
            }
          }

          //Continue operation even if MKD failed, maybe another thread did create this directory for us
          if (pData->MKDSegments.empty())
            m_Operation.nOpState=FILETRANSFER_CWD2;
          else
          {
            if (Send( L"CWD " + pData->MKDCurrent.GetPathUnterminated()))
              pData->nMKDOpState=MKD_MAKESUBDIRS;
            else
              return;
          }
        }
        break;
      default:
        DebugFail();
      }

      break;
    case FILETRANSFER_CWD2:
      if (code != 2 && code != 3)
        if (code == 4)
          nReplyError=FZ_REPLY_ERROR;
        else
          nReplyError=FZ_REPLY_CRITICALERROR;
      else
        m_Operation.nOpState=FILETRANSFER_PWD2;
      break;
    case FILETRANSFER_PWD2:
      if (code != 2 && code != 3)
        nReplyError = FZ_REPLY_ERROR;
      else
      {
        pData->rawpwd = GetReply();
        if (!ParsePwdReply(pData->rawpwd))
          return;

        m_Operation.nOpState = FileTransferListState(pData->transferfile.get);
      }
      break;
    case FILETRANSFER_LIST_MODE:
#ifdef MPEXT_NO_ZLIB
      DebugFail();
      m_Operation.nOpState = FILETRANSFER_LIST_TYPE;
#else
      if (code == 2 || code == 3)
        m_useZlib = !m_useZlib;
      m_Operation.nOpState = NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE;
#endif
      break;
    case FILETRANSFER_LIST_OPTS:
#ifdef MPEXT_NO_ZLIB
      DebugFail();
#else
      if (code == 2 || code == 3)
        m_zlibLevel = pData->newZlibLevel;
      m_Operation.nOpState = FILETRANSFER_LIST_TYPE;
#endif
      break;
    case FILETRANSFER_LIST_TYPE:
      if (code != 2 && code != 3)
        nReplyError = FZ_REPLY_ERROR;
      else
        m_Operation.nOpState = FILETRANSFER_LIST_PORTPASV;
      break;
    case FILETRANSFER_LIST_PORTPASV:
      if (code!=3 && code!=2)
      {
        if (!pData->bTriedPortPasvOnce)
        {
          pData->bTriedPortPasvOnce = TRUE;
          pData->bPasv = !pData->bPasv;
        }
        else
          nReplyError=FZ_REPLY_ERROR;
        break;
      }

      if (pData->bPasv)
      {
        CString reply = GetReply();
        int i,j;
        // MP EXT
        if((i=reply.Find(L"("))>=0&&(j=reply.Find(L")"))>=0)
        {
          i++;
          j--;
        }
        else
        {
          // MP EXT
          if ((i=reply.Mid(4).FindOneOf(L"0123456789"))>=0)
          {
            i += 4;
            j = reply.GetLength() - 1;
          }
          else
          {
            if (!pData->bTriedPortPasvOnce)
            {
              pData->bTriedPortPasvOnce = TRUE;
              pData->bPasv = !pData->bPasv;
            }
            else
              nReplyError = FZ_REPLY_ERROR;
            break;
          }
        }

        CString temp = reply.Mid(i,(j-i)+1);

        if (GetFamily() == AF_INET)
        {
          int count=0;
          int pos=0;
          //Convert commas to dots
          temp.Replace( L",", L"." );
          while(1)
          {
            pos=temp.Find(L".",pos);
            if (pos!=-1)
              count++;
            else
              break;
            pos++;
          }
          if (count!=5)
          {
            if (!pData->bTriedPortPasvOnce)
            {
              pData->bTriedPortPasvOnce = TRUE;
              pData->bPasv = !pData->bPasv;
            }
            else
              nReplyError = FZ_REPLY_ERROR;
            break;
          }

          i=temp.ReverseFind(L'.');
          pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
          temp=temp.Left(i);
          i=temp.ReverseFind(L'.');
          pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
          pData->host=temp.Left(i);
          if (!CheckForcePasvIp(pData->host))
          {
            nReplyError = FZ_REPLY_ERROR;
            break;
          }
        }
        else if (GetFamily() == AF_INET6)
        {
          temp = temp.Mid(3);
          pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
          if (pData->port < 0 || pData->port > 65535)
          {
            LogMessage(FZ_LOG_WARNING, L"Port %u not valid", pData->port);
            nReplyError = FZ_REPLY_ERROR;
            break;
          }

          pData->host = m_CurrentServer.host;
        }

        m_pTransferSocket = new CTransferSocket(this, CSMODE_LIST);
#ifndef MPEXT_NO_ZLIB
        if (m_useZlib)
        {
          if (!m_pTransferSocket->InitZlib(m_zlibLevel))
          {
            ShowStatus(L"Failed to initialize zlib", FZ_LOG_ERROR);
            ResetOperation(FZ_REPLY_ERROR);
            return;
          }
        }
#endif
#ifndef MPEXT_NO_GSS
        if (m_pGssLayer && m_pGssLayer->AuthSuccessful())
          m_pTransferSocket->UseGSS(m_pGssLayer);
#endif
        m_pTransferSocket->m_nInternalMessageID = m_pOwner->m_nInternalMessageID;
        m_pTransferSocket->SetFamily(GetFamily());
        if (!m_pTransferSocket->Create(m_pSslLayer && m_bProtP))
        {
          nReplyError = FZ_REPLY_ERROR;
          break;
        }

        DebugCheck(m_pTransferSocket->AsyncSelect());
      }
      m_Operation.nOpState=FILETRANSFER_LIST_LIST;
      break;
    case FILETRANSFER_LIST_LIST:
      if (IsMisleadingListResponse())
      {
        ShowStatus(IDS_STATUSMSG_DIRLISTSUCCESSFUL, FZ_LOG_PROGRESS);

        t_directory listing;
        listing.server = m_CurrentServer;
        listing.path.SetServer(m_CurrentServer);
        if (pData->rawpwd != L"")
        {
          if (!listing.path.SetPath(pData->rawpwd))
          {
            delete m_pDirectoryListing;
            m_pDirectoryListing = 0;
            delete m_pTransferSocket;
            m_pTransferSocket = 0;
            ResetOperation(FZ_REPLY_ERROR);
            return;
          }
        }
        else
          listing.path = pData->transferfile.remotepath;

        nReplyError = FileTransferHandleDirectoryListing(&listing);
      }
      else if (code==4 || code==5) //LIST failed, try getting file information using SIZE and MDTM
      {
        TransferHandleListError();
      }
      else if (code!=1)
        nReplyError=FZ_REPLY_ERROR;
      else
        m_Operation.nOpState=FILETRANSFER_LIST_WAITFINISH;
      break;
    case FILETRANSFER_LIST_WAITFINISH:
      if (!bFinish)
      {
        if (code!=2 && code!=3)
        {
          if (code==4 || code==5)
          {
            TransferHandleListError();
          }
          else
          {
            nReplyError=FZ_REPLY_ERROR;
          }
        }
        else
        pData->nGotTransferEndReply = 1;
      }
      if (pData->nGotTransferEndReply && pData->pDirectoryListing)
      {
        nReplyError = FileTransferHandleDirectoryListing(pData->pDirectoryListing);
        pData->nGotTransferEndReply=0;
      }
      break;
    case FILETRANSFER_NOLIST_SIZE:
      {
        __int64 size;
        if (HandleSize(code, size))
        {
          DebugAssert(!pData->pFileSize);
          pData->pFileSize=new _int64;
          *pData->pFileSize=size;
        }
      }
      m_Operation.nOpState=FILETRANSFER_NOLIST_MDTM;
      break;
    case FILETRANSFER_NOLIST_MDTM:
      if (HandleMdtm(code, pData->remoteDate))
      {
        pData->hasRemoteDate = true;
      }
      m_Operation.nOpState=FILETRANSFER_TYPE;
      nReplyError=CheckOverwriteFile();
      break;
    case FILETRANSFER_TYPE:
      if (code!=2 && code!=3)
        nReplyError = FZ_REPLY_ERROR;
      m_Operation.nOpState = !pData->transferfile.get && pData->transferdata.bResume ? FILETRANSFER_OPTS_REST : FILETRANSFER_OPTION_COMMAND_1;
      break;
    case FILETRANSFER_WAIT:
      if (!pData->nWaitNextOpState)
        nReplyError=FZ_REPLY_ERROR;
      else
        m_Operation.nOpState=pData->nWaitNextOpState;
      break;
    case FILETRANSFER_MODE:
#ifdef MPEXT_NO_ZLIB
      DebugFail();
      m_Operation.nOpState = FILETRANSFER_PORTPASV;
#else
      if (code == 2 || code == 3)
        m_useZlib = !m_useZlib;
      m_Operation.nOpState = FILETRANSFER_OPTION_COMMAND_2;
#endif
      break;
    case FILETRANSFER_OPTS:
#ifdef MPEXT_NO_ZLIB
      DebugFail();
#else
      if (code == 2 || code == 3)
        m_zlibLevel = pData->newZlibLevel;
#endif
      m_Operation.nOpState = FILETRANSFER_PORTPASV;
      break;
    case FILETRANSFER_PORTPASV:
      if (code == 3 || code == 2)
      {
        if (pData->bPasv)
        {
          CString reply = GetReply();
          int i,j;
          // MP EXT
          if((i=reply.Find(L"("))>=0&&(j=reply.Find(L")"))>=0)
          {
            i++;
            j--;
          }
          else
          {
            // MP EXT
            if ((i=reply.Mid(4).FindOneOf(L"0123456789"))>=0)
            {
              i += 4;
              j = reply.GetLength() - 1;
            }
            else
            {
              if (!pData->bTriedPortPasvOnce)
              {
                pData->bTriedPortPasvOnce = TRUE;
                pData->bPasv = !pData->bPasv;
              }
              else
                nReplyError = FZ_REPLY_ERROR;
              break;
            }
          }

          CString temp = reply.Mid(i,(j-i)+1);

          if (GetFamily() == AF_INET)
          {
            int count=0;
            int pos=0;
            //Convert commas to dots
            temp.Replace( L",", L"." );
            while(1)
            {
              pos=temp.Find( L".", pos);
              if (pos!=-1)
                count++;
              else
                break;
              pos++;
            }
            if (count!=5)
            {
              if (!pData->bTriedPortPasvOnce)
              {
                pData->bTriedPortPasvOnce = TRUE;
                pData->bPasv = !pData->bPasv;
              }
              else
                nReplyError = FZ_REPLY_ERROR;
              break;
            }

            i=temp.ReverseFind(L'.');
            pData->port=atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); //get ls byte of server socket
            temp=temp.Left(i);
            i=temp.ReverseFind(L'.');
            pData->port+=256*atol(  T2CA( temp.Right(temp.GetLength()-(i+1)) )  ); // add ms byte to server socket
            pData->host=temp.Left(i);
            if (!CheckForcePasvIp(pData->host))
            {
              nReplyError = FZ_REPLY_ERROR;
              break;
            }
          }
          else if (GetFamily() == AF_INET6)
          {
            temp = temp.Mid(3);
            pData->port = atol( T2CA(temp.Left(temp.GetLength() - 1) ) );
            if (pData->port < 0 || pData->port > 65535)
            {
              LogMessage(FZ_LOG_WARNING, L"Port %u not valid", pData->port);
              nReplyError = FZ_REPLY_ERROR;
              break;
            }

            pData->host = m_CurrentServer.host;
          }
          else
          {
            nReplyError = FZ_REPLY_ERROR;
            break;
          }
          m_pTransferSocket = new CTransferSocket(this, m_Operation.nOpMode);
#ifndef MPEXT_NO_ZLIB
          if (m_useZlib)
          {
            if (!m_pTransferSocket->InitZlib(m_zlibLevel))
            {
              ShowStatus(L"Failed to initialize zlib", FZ_LOG_ERROR);
              ResetOperation(FZ_REPLY_ERROR);
              return;
            }
          }
#endif
#ifndef MPEXT_NO_GSS
          if (m_pGssLayer && m_pGssLayer->AuthSuccessful())
            m_pTransferSocket->UseGSS(m_pGssLayer);
#endif
          m_pTransferSocket->m_nInternalMessageID = m_pOwner->m_nInternalMessageID;
          m_pTransferSocket->SetFamily(GetFamily());
          if (!m_pTransferSocket->Create(m_pSslLayer && m_bProtP))
          {
            nReplyError = FZ_REPLY_ERROR;
            break;
          }

          DebugCheck(m_pTransferSocket->AsyncSelect());
        }

        if (pData->transferdata.bResume)
          m_Operation.nOpState = FILETRANSFER_REST;
        else
          m_Operation.nOpState = FILETRANSFER_RETRSTOR;

        if (m_pDataFile != NULL)
        {
          delete m_pDataFile;
          m_pDataFile = NULL;
        }

        if (!m_pTransferSocket)
        {
          nReplyError=FZ_REPLY_ERROR;
          break;
        }

        if (!pData->transferfile.get)
        {
          nReplyError = OpenTransferFile(pData);
          if (nReplyError)
          {
            break;
          }

          if (m_pDataFile != NULL)
          {
            // See comment in !get branch below
            pData->transferdata.transfersize=GetLength64(*m_pDataFile);
            pData->transferdata.transferleft=pData->transferdata.transfersize;
          }
          if (pData->transferdata.bResume)
          {
            CString remotefile=pData->transferfile.remotefile;
            if (m_pDirectoryListing)
              for (int i = 0; i < m_pDirectoryListing->num; i++)
              {
                if (m_pDirectoryListing->direntry[i].name == remotefile)
                {
                  pData->transferdata.transferleft -= m_pDirectoryListing->direntry[i].size;
                  break;
                }
              }
            _int64 size = pData->transferdata.transfersize-pData->transferdata.transferleft;
            LONG low = static_cast<LONG>(size&0xFFFFFFFF);
            LONG high = static_cast<LONG>(size>>32);
            if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, low, &high, FILE_BEGIN)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
            {
              ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, FZ_LOG_ERROR);
              nReplyError = FZ_REPLY_ERROR;
            }
          }
        }
        else
        {
          if (pData->transferdata.bResume)
          {
            nReplyError = OpenTransferFile(pData);
            if (nReplyError)
            {
              break;
            }
          }

          CString remotefile=pData->transferfile.remotefile;
          if (m_pDirectoryListing)
            for (int i=0; i<m_pDirectoryListing->num; i++)
            {
              if (m_pDirectoryListing->direntry[i].name==remotefile)
              {
                pData->hasRemoteDate = true;
                pData->remoteDate = m_pDirectoryListing->direntry[i].date;
                pData->transferdata.transfersize=m_pDirectoryListing->direntry[i].size;
              }
            }
          else if (pData->pFileSize)
            pData->transferdata.transfersize=*pData->pFileSize;
          pData->transferdata.transferleft=pData->transferdata.transfersize;
        }
      }
      else
        if (!pData->bTriedPortPasvOnce)
        {
          pData->bTriedPortPasvOnce = TRUE;
          pData->bPasv = !pData->bPasv;
        }
        else
          nReplyError = FZ_REPLY_ERROR;
      break;
    case FILETRANSFER_OPTS_REST:
      // anything else means the resume is allowed (2xx) or the server does not understand OPTS REST STOR (5xx)
      if (code == 4)
      {
        ShowStatus(L"Resume not allowed by the server, restarting upload from the scratch.", FZ_LOG_PROGRESS);
        CancelTransferResume(pData);
      }
      m_Operation.nOpState = FILETRANSFER_OPTION_COMMAND_1;
      break;
    case FILETRANSFER_REST:
      { //Resume
        if (code==3 || code==2)
        {
          LONG high = 0;
          if (pData->transferfile.get)
          {
            pData->transferdata.transferleft = pData->transferdata.transfersize - GetLength64(*m_pDataFile);
            if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, 0, &high, FILE_END)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
            {
              ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, FZ_LOG_ERROR);
              nReplyError = FZ_REPLY_ERROR;
            }
            else
              m_Operation.nOpState = FILETRANSFER_RETRSTOR;
          }
          else
          {
            m_Operation.nOpState = FILETRANSFER_RETRSTOR;
          }
        }
        else
        {
          if (code==5 && GetReply()[1]==L'0')
          {
            if (pData->transferfile.get)
            {
              if (pData->transferdata.transfersize!=-1)
              {
                DebugAssert(m_pDataFile);
                if (GetLength64(*m_pDataFile) == pData->transferdata.transfersize)
                {
                  ShowStatus(IDS_ERRORMSG_CANTRESUME_FINISH, FZ_LOG_STATUS);
                  ResetOperation(FZ_REPLY_OK);
                  return;
                }
              }

              ShowStatus(IDS_ERRORMSG_CANTRESUME, FZ_LOG_ERROR);
              CancelTransferResume(pData);
              m_Operation.nOpState=FILETRANSFER_RETRSTOR;
            }
            else
            {
              ShowStatus(L"Resume command not supported by server, trying append.", FZ_LOG_PROGRESS);
              pData->transferdata.bResumeAppend=TRUE;
              m_Operation.nOpState=FILETRANSFER_RETRSTOR;
            }
          }
          else
            nReplyError=FZ_REPLY_ERROR;
        }
      }
      break;
    case FILETRANSFER_RETRSTOR:
      // A '1xy opening data connection' reply is expected if RETR/STOR/APPE
      // is successful.
      // On failure, it's a 4xy or 5xy reply.
      // However, some servers send a 2xy transfer complete reply without opening a data
      // connection if there's no data to send.
      if (code==2)
      {
        //Transfer successful, however server did not open data connection
        ResetOperation(FZ_REPLY_OK);
        return;
      }
      else if (code!=1)
      {
        if (!pData->transferfile.get && pData->transferdata.bResume && pData->askOnResumeFail)
        {
          pData->askOnResumeFail = false;
          delete m_pTransferSocket;
          m_pTransferSocket = 0;
          delete m_pDataFile;
          m_pDataFile = 0;
          pData->nGotTransferEndReply = 0;
          nReplyError = CheckOverwriteFile();
        }
        else
        {
          nReplyError = FZ_REPLY_ERROR;
          if (code == 5)
            nReplyError |= FZ_REPLY_CRITICALERROR;
        }
      }
      else
      {
        m_Operation.nOpState=FILETRANSFER_WAITFINISH;

        //Look if we can find any information about the resume offset
        if (!pData->transferfile.get && pData->transferdata.bResumeAppend)
        {
          _int64 nOffset = -1;
          CString reply = GetReply();
          reply.MakeLower();
          int pos = reply.Find(L"restarting at offset ");
          if (pos != -1)
            pos += _tcslen(L"restarting at offset ");

          reply = reply.Mid(pos);

          int i;
          for (i=0; i<reply.GetLength(); i++)
          {
            if (reply[i] < L'0' || reply[i] > L'9')
              break;
          }
          if (i == reply.GetLength())
            nOffset = _ttoi64(reply);
          if (nOffset != -1 && m_pDataFile)
          {
            LONG low = 0;
            LONG high = 0;
            if (nOffset >= GetLength64(*m_pDataFile))
            {
              if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, 0, &high, FILE_END)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
              {
                ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, FZ_LOG_ERROR);
                nReplyError = FZ_REPLY_ERROR;
              }
            }
            else
            {
              low=static_cast<LONG>(nOffset&0xFFFFFFFF);
              high=static_cast<LONG>(nOffset>>32);
              if (SetFilePointer((HANDLE)m_pDataFile->m_hFile, low, &high, FILE_BEGIN)==0xFFFFFFFF && GetLastError()!=NO_ERROR)
              {
                ShowStatus(IDS_ERRORMSG_SETFILEPOINTER, FZ_LOG_ERROR);
                nReplyError = FZ_REPLY_ERROR;
              }
            }
          }
          if (!nReplyError && !GetOptionVal(OPTION_MPEXT_TRANSFER_ACTIVE_IMMEDIATELY))
          {
            nReplyError = ActivateTransferSocket(pData);
          }
        }
        else if (!nReplyError && pData->bPasv && !GetOptionVal(OPTION_MPEXT_TRANSFER_ACTIVE_IMMEDIATELY))
        {
          nReplyError = ActivateTransferSocket(pData);
        }
      }
      break;
    case FILETRANSFER_WAITFINISH:
      if (bFinish)
      {
        if (nError)
        {
          LogMessage(FZ_LOG_INFO, L"Transfer failed");
        }
        else
        {
          LogMessage(FZ_LOG_INFO, L"Transfer completed");
        }
      }
      if (!bFinish)
      {
        if (code == 1)
        {
          /* Some non-rfc959 compatible servers send more than one code 1yz reply, especially if using APPE.
           * Just ignore the additional ones.
           */
          LogMessage(FZ_LOG_WARNING, L"Server sent more than one code 1yz reply, ignoring additional reply");
          break;
        }
        else if (code!=2 && code!=3)
          nReplyError = FZ_REPLY_ERROR;
        else
        {
          pData->nGotTransferEndReply |= 1;
        }
      }
      if (pData->nGotTransferEndReply==3)
      {
        // Not really sure about a reason for the m_pDataFile condition here
        TransferFinished(m_pDataFile != NULL);
        return;
      }
      break;
    case FILETRANSFER_MFMT:
      //Transfer successful
      ResetOperation(FZ_REPLY_OK);
      break;
    }
    if (nReplyError)
    { //Error transferring the file
      LogMessage(FZ_LOG_INFO, L"Transfer response error (%x)", nReplyError);
      ResetOperation(nReplyError);
      return;
    }
  }
  /////////////////
  //Send commands//
  /////////////////
  BOOL bError=FALSE;
  switch(m_Operation.nOpState)
  {
  case FILETRANSFER_PWD:
    if (!Send(L"PWD"))
      bError = TRUE;
    break;
  case FILETRANSFER_CWD:
    if (!Send(L"CWD "+pData->transferfile.remotepath.GetPathUnterminated()))
      bError=TRUE;
    break;
  case FILETRANSFER_MKD:
    if (pData->nMKDOpState==MKD_INIT)
    {
      if (!pData->transferfile.remotepath.HasParent())
      {
        LogMessage(FZ_LOG_WARNING, L"Can't create root dir");
        ResetOperation(FZ_REPLY_CRITICALERROR);
        return;
      }
      if (!Send(L"CWD "+pData->transferfile.remotepath.GetParent().GetPathUnterminated()))
        bError=TRUE;
      pData->MKDCurrent=pData->transferfile.remotepath.GetParent();
      pData->MKDSegments.push_front(pData->transferfile.remotepath.GetLastSegment());
      pData->nMKDOpState=MKD_FINDPARENT;
    }
    break;
  case FILETRANSFER_CWD2:
    if (!Send(L"CWD "+pData->transferfile.remotepath.GetPathUnterminated()))
      bError=TRUE;
    break;
  case FILETRANSFER_PWD2:
    if (!Send(L"PWD"))
      bError=TRUE;
    break;
  case FILETRANSFER_LIST_MODE:
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    if (m_useZlib)
    {
      if (!Send(L"MODE S"))
        bError = TRUE;
    }
    else
      if (!Send(L"MODE Z"))
        bError = TRUE;
#endif
    break;
  case FILETRANSFER_LIST_OPTS:
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    {
      pData->newZlibLevel = GetOptionVal(OPTION_MODEZ_LEVEL);
      CString str;
      str.Format(L"OPTS MODE Z LEVEL %d", pData->newZlibLevel);
      if (!Send(str))
        bError = TRUE;
    }
#endif
    break;
  case FILETRANSFER_LIST_PORTPASV:
    delete m_pDirectoryListing;
    m_pDirectoryListing=0;
    if (pData->bPasv)
    {
      if (!Send((GetFamily() == AF_INET) ? L"PASV" : L"EPSV"))
        bError=TRUE;
    }
    else
    {
      if (m_pTransferSocket)
      {
        delete m_pTransferSocket;
      }
      m_pTransferSocket = new CTransferSocket(this, CSMODE_LIST);
#ifndef MPEXT_NO_ZLIB
      if (m_useZlib)
      {
        if (!m_pTransferSocket->InitZlib(m_zlibLevel))
        {
          ShowStatus(L"Failed to initialize zlib", FZ_LOG_ERROR);
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
#endif
#ifndef MPEXT_NO_GSS
      if (m_pGssLayer && m_pGssLayer->AuthSuccessful())
        m_pTransferSocket->UseGSS(m_pGssLayer);
#endif
      m_pTransferSocket->m_nInternalMessageID = m_pOwner->m_nInternalMessageID;
      m_pTransferSocket->m_bListening = TRUE;
      m_pTransferSocket->SetFamily(GetFamily());
      if(!m_pTransferSocket->Create(m_pSslLayer && m_bProtP) || !m_pTransferSocket->AsyncSelect())
        bError=TRUE;
      else if (m_pProxyLayer)
      {
        SOCKADDR_IN addr;
        int len=sizeof(addr);
        if (!m_pProxyLayer->GetPeerName((SOCKADDR *)&addr,&len))
        {
          ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
          bError=TRUE;
        }
        else if (!m_pTransferSocket->Listen(addr.sin_addr.S_un.S_addr))
        {
          ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
          bError=TRUE;
        }
        //Don't send PORT command yet, params are unknown.
        //will be sent in TransfersocketListenFinished
      }
      else
      {
        //Set up an active file transfer
        CString temp;
        UINT nPort;

        if (//create listen socket (let Windows choose the port) & start listening
          !m_pTransferSocket->Listen() ||
          !m_pTransferSocket->GetSockName(temp, nPort))
        {
          bError = TRUE;
          break;
        }

        CString host;
        if (GetFamily() == AF_INET)
        {
          host = GetOption(OPTION_TRANSFERIP);
          if (host != L"")
          {
            DWORD ip = inet_addr(T2CA(host));
            if (ip != INADDR_NONE)
              host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
            else
            {
              hostent *fullname = gethostbyname(T2CA(host));
              if (!fullname)
                host = L"";
              else
              {
                DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
                if (ip != INADDR_NONE)
                  host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
                else
                  host = L"";
              }
            }
          }
          if (host == L"")
          {
            UINT temp;

            if (!GetSockName(host, temp))
            {
              bError = true;
              break;
            }

            host.Replace(L'.', L',');
          }

          if (!bError)
          {
            host.Format(host+L",%d,%d", nPort/256, nPort%256);
            if (!Send(L"PORT " + host)) // send PORT cmd to server
              bError = TRUE;
          }
        }
        else if (GetFamily() == AF_INET6)
        {
          host = GetOption(OPTION_TRANSFERIP6);
          if (host != L"")
          {
            USES_CONVERSION;
            addrinfo hints, *res;
            memset(&hints, 0, sizeof(addrinfo));
            hints.ai_family = AF_INET6;
            hints.ai_socktype = SOCK_STREAM;
            if (!getaddrinfo(T2CA(host), "1024", &hints, &res))
            {
              host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
              freeaddrinfo(res);
            }
            else
              host = L"";
          }
          if (host == L"")
          {
            UINT temp;

            if(!GetSockName(host, temp))
              bError = true;
          }

          if (!bError)
          {
            // assamble EPRT command
            CString cmd;
            cmd.Format(L"EPRT |2|" +  host + L"|%d|", nPort);
            if (!Send(cmd))
              bError = TRUE;
          }
        }
        else
        {
          LogMessage(FZ_LOG_WARNING, L"Protocol %d not supported", GetFamily());
          bError = true;
        }
      }
    }
    break;
  case FILETRANSFER_LIST_TYPE:
    if (!Send(L"TYPE A"))
      bError=TRUE;
    break;
  case FILETRANSFER_LIST_LIST:
    {
      if (!m_pTransferSocket)
      {
        ResetOperation(FZ_REPLY_ERROR);
        return;
      }

      m_pTransferSocket->SetActive();
      CString cmd = GetListingCmd();
      if(!Send(cmd))
        bError=TRUE;
      else if(pData->bPasv)
      {
        if (!ConnectTransferSocket(pData->host, pData->port))
        {
          bError=TRUE;
          ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
        }
      }
    }
    break;
  case FILETRANSFER_NOLIST_SIZE:
    {
      CString command = L"SIZE ";
      command += pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);

      if (!Send(command))
        bError=TRUE;
    }
    break;
  case FILETRANSFER_NOLIST_MDTM:
    {
      CString command = L"MDTM ";
      command += pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);

      if (!Send(command))
        bError=TRUE;
    }
    break;
  case FILETRANSFER_TYPE:
    if (pData->transferfile.nType==1)
    {
      if (!Send(L"TYPE A"))
        bError=TRUE;
    }
    else
      if (!Send(L"TYPE I"))
        bError=TRUE;
    break;
  case FILETRANSFER_MODE:
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    if (m_useZlib)
    {
      if (!Send(L"MODE S"))
        bError = TRUE;
    }
    else
      if (!Send(L"MODE Z"))
        bError = TRUE;
#endif
    break;
  case FILETRANSFER_OPTS:
#ifdef MPEXT_NO_ZLIB
    DebugFail();
#else
    {
      pData->newZlibLevel = GetOptionVal(OPTION_MODEZ_LEVEL);
      CString str;
      str.Format(L"OPTS MODE Z LEVEL %d", pData->newZlibLevel);
      if (!Send(str))
        bError = TRUE;
    }
#endif
    break;
  case FILETRANSFER_PORTPASV:
    if (pData->bPasv)
    {
      if (!Send((GetFamily() == AF_INET) ? L"PASV" : L"EPSV"))
        bError=TRUE;
    }
    else
    {
      if (m_pTransferSocket)
      {
        delete m_pTransferSocket;
      }
      m_pTransferSocket=new CTransferSocket(this, m_Operation.nOpMode);
#ifndef MPEXT_NO_ZLIB
      if (m_useZlib)
      {
        if (!m_pTransferSocket->InitZlib(m_zlibLevel))
        {
          ShowStatus(L"Failed to initialize zlib", FZ_LOG_ERROR);
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
#endif
#ifndef MPEXT_NO_GSS
      if (m_pGssLayer && m_pGssLayer->AuthSuccessful())
        m_pTransferSocket->UseGSS(m_pGssLayer);
#endif
      m_pTransferSocket->m_nInternalMessageID=m_pOwner->m_nInternalMessageID;
      m_pTransferSocket->m_bListening = TRUE;
      m_pTransferSocket->SetFamily(GetFamily());
      if(!m_pTransferSocket->Create(m_pSslLayer && m_bProtP) || !m_pTransferSocket->AsyncSelect())
        bError = TRUE;
      else if (m_pProxyLayer)
      {
        SOCKADDR_IN addr;
        int len=sizeof(addr);
        if (!m_pProxyLayer->GetPeerName((SOCKADDR *)&addr,&len))
        {
          ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
          bError=TRUE;
        }
        else if (!m_pTransferSocket->Listen(addr.sin_addr.S_un.S_addr))
        {
          ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
          bError=TRUE;
        }
        //Don't send PORT command yet, params are unknown.
        //will be sent in TransfersocketListenFinished
      }
      else
      {
        //Set up an active file transfer

        CString temp;
        UINT nPort;
        if (//create listen socket (let Windows choose the port) & start listening
          !m_pTransferSocket->Listen() ||
          !m_pTransferSocket->GetSockName(temp, nPort))
        {
          bError = TRUE;
          break;
        }

        CString host;
        if (GetFamily() == AF_INET)
        {
          host = GetOption(OPTION_TRANSFERIP);
          if (host != L"")
          {
            DWORD ip = inet_addr(T2CA(host));
            if (ip != INADDR_NONE)
              host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
            else
            {
              hostent *fullname = gethostbyname(T2CA(host));
              if (!fullname)
                host = L"";
              else
              {
                DWORD ip = ((LPIN_ADDR)fullname->h_addr)->s_addr;
                if (ip != INADDR_NONE)
                  host.Format(L"%d,%d,%d,%d", ip%256, (ip>>8)%256, (ip>>16)%256, ip>>24);
                else
                  host = L"";
              }
            }
          }
          if (host == L"")
          {
            UINT temp;

            if (!GetSockName(host, temp))
              bError = true;

            host.Replace(L'.', L',');
          }

          if (!bError)
          {
            host.Format(host+L",%d,%d", nPort/256, nPort%256);
            if (!Send(L"PORT " + host)) // send PORT cmd to server
              bError = TRUE;
          }
        }
        else if (GetFamily() == AF_INET6)
        {
          host = GetOption(OPTION_TRANSFERIP6);
          if (host != L"")
          {
            USES_CONVERSION;
            addrinfo hints, *res;
            memset(&hints, 0, sizeof(addrinfo));
            hints.ai_family = AF_INET6;
            hints.ai_socktype = SOCK_STREAM;
            if (!getaddrinfo(T2CA(host), "1024", &hints, &res))
            {
              host = Inet6AddrToString(((SOCKADDR_IN6 *)res->ai_addr)->sin6_addr);
              freeaddrinfo(res);
            }
            else
              host = L"";
          }
          if (host == L"")
          {
            UINT temp;

            if(!GetSockName(host, temp))
              bError = true;
          }

          if (!bError)
          {
            // assamble EPRT command
            CString cmd;
            cmd.Format(L"EPRT |2|" +  host + L"|%d|", nPort);
            if (!Send(cmd))
              bError = TRUE;
          }
        }
        else
        {
          LogMessage(FZ_LOG_WARNING, L"Protocol %d not supported", GetFamily());
          bError = true;
        }

        if (bError)
        {
          ResetOperation(FZ_REPLY_ERROR);
          return;
        }
      }
    }
    break;
  case FILETRANSFER_OPTS_REST:
    if (!Send(L"OPTS REST STOR"))
      bError = TRUE;
    break;
  case FILETRANSFER_REST:
    DebugAssert(m_pDataFile);
    {
      CString command;
      __int64 transferoffset =
        pData->transferfile.get ?
          GetLength64(*m_pDataFile) :
          pData->transferdata.transfersize-pData->transferdata.transferleft;
      command.Format(L"REST %I64d", transferoffset);
      if (!Send(command))
        bError=TRUE;
    }
    break;
  case FILETRANSFER_RETRSTOR:
    // send RETR/STOR command to server
    if (!m_pTransferSocket)
    {
      ResetOperation(FZ_REPLY_ERROR);
      return;
    }
    m_pTransferSocket->m_transferdata=pData->transferdata;
    if (GetOptionVal(OPTION_MPEXT_TRANSFER_ACTIVE_IMMEDIATELY) || !pData->bPasv)
    {
      if (ActivateTransferSocket(pData))
      {
        bError = TRUE;
      }
    }
    CString filename;

    filename = pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);
    if(!Send((pData->transferfile.get?L"RETR ":(pData->transferdata.bResumeAppend)?L"APPE ":L"STOR ")+ filename))
      bError = TRUE;
    else
    {
      if (pData->bPasv)
      {
        // if PASV create the socket & initiate outbound data channel connection
        if (!ConnectTransferSocket(pData->host, pData->port))
        {
          bError=TRUE;
        }
      }
    }
    break;
  }
  if (bError)
  { //Error transferring the file
    LogMessage(FZ_LOG_INFO, L"Transfer error");
    ResetOperation(FZ_REPLY_ERROR);
    return;
  }
}

void CFtpControlSocket::TransferHandleListError()
{
  if (m_pTransferSocket)
    delete m_pTransferSocket;
  m_pTransferSocket=0;
  m_Operation.nOpState = FILETRANSFER_NOLIST_SIZE;
}

static int atoui(const wchar_t * s)
{
  wchar_t * endptr;
  int result = wcstol(s, &endptr, 10);
  if ((*s == L'\0') || (*endptr != L'\0'))
  {
    result = -1;
  }
  return result;
}

bool CFtpControlSocket::HandleMdtm(int code, t_directory::t_direntry::t_date & date)
{
  bool result = false;
  if (code==2)
  {
    CString line = GetReply();
    if ( line.GetLength()>4  &&  line.Left(4) == L"213 " )
    {
      int y=0, M=0, d=0, h=0, m=0, s=0;
      bool hasseconds = false;
      line=line.Mid(4);
      y=atoui(line.Left(4));
      if ((y >= 0) && (line.GetLength() > 4))
      {
        line=line.Mid(4);
        M=atoui(line.Left(2));
        if ((M >= 0) && (line.GetLength() > 2))
        {
          line=line.Mid(2);
          d=atoui(line.Left(2));
          if ((d >= 0) && (line.GetLength() > 2))
          {
            line=line.Mid(2);
            h=atoui(line.Left(2));
            if ((h >= 0) && (line.GetLength() > 2))
            {
              line=line.Mid(2);
              m=atoui(line.Left(2));
              if ((m >= 0) && (line.GetLength() > 2))
              {
                line=line.Mid(2);
                s=_ttoi(line.Left(2));
                hasseconds = true;
              }
            }
          }
          if (M>0 && M<=12 && d>0 && d<=31 && h>=0 && h<24 && m>=0 && m<60 && s>=0 && s<60)
          {
            result = true;
            date.year = y;
            date.month = M;
            date.day = d;
            date.hour = h;
            date.minute = m;
            date.second = s;
            date.hastime = true;
            date.hasyear = true;
            date.hasseconds = hasseconds;
            date.hasdate = true;
            date.utc = true;
          }
        }
      }
    }
  }
  return result;
}

bool CFtpControlSocket::HandleSize(int code, __int64 & size)
{
  bool result = false;
  if (code == 2)
  {
    CString line = GetReply();
    if ((line.GetLength() > 4) && (line.Left(4) == L"213 "))
    {
      size = _ttoi64(line.Mid(4));
      result = true;
    }
  }
  return result;
}

void CFtpControlSocket::TransferFinished(bool preserveFileTimeForUploads)
{
  CFileTransferData *pData=static_cast<CFileTransferData *>(m_Operation.pData);

  if (GetOptionVal(OPTION_PRESERVEDOWNLOADFILETIME) &&
      m_pDataFile &&
      pData->transferfile.get &&
      DebugAlwaysTrue(pData->transferfile.OnTransferOut == NULL))
  {
    m_pTools->PreserveDownloadFileTime(
      (HANDLE)m_pDataFile->m_hFile, reinterpret_cast<void *>(pData->transferfile.nUserData));
  }
  if (!pData->transferfile.get &&
      GetOptionVal(OPTION_MPEXT_PRESERVEUPLOADFILETIME) && preserveFileTimeForUploads &&
      ((m_serverCapabilities.GetCapability(mfmt_command) == yes) ||
       (m_serverCapabilities.GetCapability(mdtm_command) == yes)) &&
      DebugAlwaysTrue(pData->transferfile.OnTransferIn == NULL))
  {
    CString filename =
      pData->transferfile.remotepath.FormatFilename(pData->transferfile.remotefile, !pData->bUseAbsolutePaths);
    struct tm tm;
    if (m_pTools->GetFileModificationTimeInUtc((LPCTSTR)pData->transferfile.localfile, tm))
    {
      CString timestr;
      timestr.Format(L"%02d%02d%02d%02d%02d%02d",
        1900 + tm.tm_year, 1 + tm.tm_mon, tm.tm_mday, tm.tm_hour, tm.tm_min, tm.tm_sec);
      CString command;
      if (m_serverCapabilities.GetCapability(mfmt_command) == yes)
      {
        command = L"MFMT";
      }
      else
      {
        // Support for MDTM does not necessarily mean
        // that the server supports non-standard hack
        // of setting timestamp using
        // MFMT-like (two argument) call to MDTM.
        // IIS definitely does.
        command = L"MDTM";
      }
      if (Send( command + L" " + timestr + L" " + filename))
      {
        m_Operation.nOpState = FILETRANSFER_MFMT;
        return;
      }
    }
  }
  //Transfer successful
  ResetOperation(FZ_REPLY_OK);
}

void CFtpControlSocket::Cancel(BOOL bQuit/*=FALSE*/)
{
  const int nOpMode = m_Operation.nOpMode;
  if (nOpMode==CSMODE_CONNECT)
    DoClose(FZ_REPLY_CANCEL);
  else if (nOpMode & CSMODE_LIST)
  {
    if (m_Operation.nOpState == LIST_WAITFINISH)
      m_skipReply = true;
    ResetOperation(FZ_REPLY_ERROR | FZ_REPLY_CANCEL);
  }
  else if (nOpMode & CSMODE_TRANSFER)
  {
    if (m_Operation.nOpState == FILETRANSFER_WAITFINISH || m_Operation.nOpState == FILETRANSFER_LIST_WAITFINISH)
      m_skipReply = true;
    ResetOperation(FZ_REPLY_ERROR | FZ_REPLY_CANCEL | FZ_REPLY_ABORTED);
  }
  else if (nOpMode != CSMODE_NONE)
    ResetOperation(FZ_REPLY_ERROR | FZ_REPLY_CANCEL);

  if (nOpMode != CSMODE_NONE && !bQuit)
    ShowStatus(IDS_ERRORMSG_INTERRUPTED, FZ_LOG_ERROR);

  if (m_awaitsReply)
    m_skipReply = true;
}

void CFtpControlSocket::TransfersocketListenFinished(unsigned int ip, unsigned short port)
{
  if (m_Operation.nOpMode&CSMODE_TRANSFER || m_Operation.nOpMode&CSMODE_LIST)
  {
    CString host;
    host.Format(L"%d,%d,%d,%d,%d,%d",ip%256,(ip>>8)%256,(ip>>16)%256,(ip>>24)%256,port%256,port>>8);
    Send(L"PORT "+host);
  }
}

void CFtpControlSocket::ResumeTransfer()
{
  if (m_pTransferSocket && (m_Operation.nOpMode&CSMODE_TRANSFER || m_Operation.nOpMode&CSMODE_LIST))
  {
    m_pTransferSocket->OnSend(0);
    m_pTransferSocket->OnReceive(0);
  }
}

BOOL CFtpControlSocket::Create()
{
  return CAsyncSocketEx::Create(0, SOCK_STREAM, FD_READ | FD_WRITE | FD_OOB | FD_ACCEPT |  FD_CONNECT | FD_CLOSE, 0, GetOptionVal(OPTION_ENABLE_IPV6) ? AF_UNSPEC : AF_INET);
}

void CFtpControlSocket::ResetOperation(int nSuccessful /*=FALSE*/)
{
  if (nSuccessful & FZ_REPLY_CRITICALERROR)
    nSuccessful |= FZ_REPLY_ERROR;

  if (m_pTransferSocket)
  {
    ResetTransferSocket(nSuccessful & (FZ_REPLY_ERROR | FZ_REPLY_CANCEL));
  }

  if (m_pDataFile)
    delete m_pDataFile;
  m_pDataFile=0;

  if (m_Operation.nOpMode)
  {
    //Unset busy attribute so that new commands can be executed
    m_pOwner->SetBusy(FALSE);

    if (m_Operation.nOpMode&CSMODE_CONNECT && nSuccessful&FZ_REPLY_ERROR)
    {
      nSuccessful|=FZ_REPLY_DISCONNECTED;
      ShowStatus(IDS_ERRORMSG_CANTCONNECT, FZ_LOG_ERROR);
    }

    if (m_Operation.nOpMode & (CSMODE_LIST|CSMODE_LISTFILE|CSMODE_TRANSFER) && nSuccessful==FZ_REPLY_OK)
      m_LastSendTime=CTime::GetCurrentTime();

    //Update remote file entry
    if (m_Operation.pData &&
      m_Operation.nOpMode&CSMODE_TRANSFER &&
      (!((CFileTransferData*)m_Operation.pData)->transferfile.get) &&
      m_pDirectoryListing &&
      m_Operation.nOpState>=FILETRANSFER_RETRSTOR)
    {
      CString filename=((CFileTransferData*)m_Operation.pData)->transferfile.remotefile;
      CServerPath path=((CFileTransferData*)m_Operation.pData)->transferfile.remotepath;
      t_directory dir;
      BOOL res=FALSE;
      if (m_pDirectoryListing)
      {
        if (m_pDirectoryListing->path==path)
        {
          dir=*m_pDirectoryListing;
          res=TRUE;
        }
      }
      t_directory WorkingDir;
      BOOL bFound=m_pOwner->GetWorkingDir(&WorkingDir);
      if (!res && bFound)
        if (WorkingDir.path==path)
        {
          dir=WorkingDir;
          res=TRUE;
        }
      if (res)
      {
        int i;
        for (i=0; i<dir.num; i++)
          if (dir.direntry[i].name==filename)
          {
            dir.direntry[i].bUnsure = TRUE;
            if (nSuccessful & FZ_REPLY_ERROR)
            {
              dir.direntry[i].bUnsure = TRUE;
              if (!((CFileTransferData *)m_Operation.pData)->transferfile.get)
                dir.direntry[i].size = -1;
              if (!GetLength64(((CFileTransferData *)m_Operation.pData)->transferfile.localfile, dir.direntry[i].size))
                dir.direntry[i].size = -1;
            }
            dir.direntry[i].date.hasdate = false;
            break;
          }
        if (i==dir.num)
        {
          t_directory::t_direntry *entries=new t_directory::t_direntry[dir.num+1];
          int i;
          for (i=0; i<dir.num; i++)
            entries[i]=dir.direntry[i];
          entries[i].name=filename;
          entries[i].dir=FALSE;
          entries[i].date.hasdate=FALSE;
          entries[i].size=-1;
          if (nSuccessful&FZ_REPLY_OK)
            entries[i].bUnsure=FALSE;
          else
            entries[i].bUnsure=TRUE;

          delete [] dir.direntry;
          dir.direntry=entries;
          dir.num++;
        }
        BOOL updated=FALSE;
        if (m_pDirectoryListing && m_pDirectoryListing->path==dir.path)
        {
          updated=TRUE;
          SetDirectoryListing(&dir, bFound && WorkingDir.path == dir.path);
        }
        if (!updated)
          if (bFound && WorkingDir.path==dir.path)
          {
            updated = TRUE;
            m_pOwner->SetWorkingDir(&dir);
          }
      }
    }

    if (m_Operation.pData && nSuccessful&FZ_REPLY_ERROR)
    {
      if (m_Operation.nOpMode&CSMODE_TRANSFER)
        if (nSuccessful&FZ_REPLY_ABORTED)
          //Transfer aborted by user
          ShowStatus((m_Operation.nOpMode&CSMODE_DOWNLOAD)?IDS_ERRORMSG_DOWNLOADABORTED:IDS_ERRORMSG_UPLOADABORTED,FZ_LOG_ERROR);
        else
          ShowStatus(((CFileTransferData*)m_Operation.pData)->transferfile.get?IDS_ERRORMSG_DOWNLOADFAILED:IDS_ERRORMSG_UPLOADFAILED,FZ_LOG_ERROR);
      else if (m_Operation.nOpMode&CSMODE_LIST)
        ShowStatus(IDS_ERRORMSG_CANTGETLIST,FZ_LOG_ERROR);
      else if (m_Operation.nOpMode&CSMODE_LISTFILE)
        ShowStatus(IDS_ERRORMSG_CANTGETLISTFILE,FZ_LOG_ERROR);
    }
    else if (m_Operation.pData && m_Operation.nOpMode&CSMODE_TRANSFER && nSuccessful==FZ_REPLY_OK)
      ShowStatus(((CFileTransferData*)m_Operation.pData)->transferfile.get?IDS_STATUSMSG_DOWNLOADSUCCESSFUL:IDS_STATUSMSG_UPLOADSUCCESSFUL,FZ_LOG_STATUS);
  }
  else
  {
    // When control socket is waiting for reply
    // to keepalive (!IsReady), while new command comes,
    // its execution is postponed
    // (CMainThread::m_pPostKeepAliveCommand),
    // but the main thread
    // is set to busy state already (CMainThread::Command).
    // if connection is closed before without receiving reply,
    // main thread would stay busy forever.
    m_pOwner->SetBusy(FALSE);
    //No operation in progress
    nSuccessful&=FZ_REPLY_DISCONNECTED|FZ_REPLY_CANCEL;
  }

  if (nSuccessful&FZ_REPLY_DISCONNECTED)
    m_pOwner->SetWorkingDir(0); //Disconnected, reset working dir

  if (m_Operation.nOpMode)
    GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_REPLY, m_pOwner->m_LastCommand.id), nSuccessful);
  else
    GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_REPLY, 0), nSuccessful);

  m_Operation.nOpMode=0;
  m_Operation.nOpState=-1;

  if (m_Operation.pData)
    delete m_Operation.pData;
  m_Operation.pData=0;
}

void CFtpControlSocket::Delete(CString filename, const CServerPath &path, bool filenameOnly)
{
  class CDeleteData : public CFtpControlSocket::t_operation::COpData
  {
public:
    CDeleteData() {}
    virtual ~CDeleteData() {}
    CString m_FileName;
    CServerPath path;
  };
  if (filename!=L"")
  {
    DebugAssert(!path.IsEmpty());
    DebugAssert(m_Operation.nOpMode==CSMODE_NONE);
    DebugAssert(m_Operation.nOpState==-1);
    DebugAssert(!m_Operation.pData);
    m_Operation.nOpMode=CSMODE_DELETE;
    CString command = L"DELE ";
    if (!filenameOnly)
    {
      command += path.FormatFilename(filename);
    }
    else
    {
      command += filename;
    }
    if (!Send(command))
      return;
    CDeleteData *data=new CDeleteData;
    data->m_FileName=filename;
    data->path=path;
    m_Operation.pData=data;
  }
  else
  {
    DebugAssert(path.IsEmpty());
    DebugAssert(m_Operation.nOpMode==CSMODE_DELETE);
    DebugAssert(m_Operation.nOpState==-1);
    DebugAssert(m_Operation.pData);
    int res=GetReplyCode();
    if (res==2 || res==3)
    { //Remove file from cached dirs
      CDeleteData *pData=(CDeleteData *)m_Operation.pData;
      t_directory dir;
      BOOL res=FALSE;
      if (m_pDirectoryListing)
      {
        if (m_pDirectoryListing->path==pData->path)
        {
          dir=*m_pDirectoryListing;
          res=TRUE;
        }
      }
      t_directory WorkingDir;
      BOOL bFound=m_pOwner->GetWorkingDir(&WorkingDir);
      if (!res && bFound)
        if (WorkingDir.path==pData->path)
        {
          dir=WorkingDir;
          res=TRUE;
        }
      if (res)
      {
        BOOL found=FALSE;
        for (int i=0;i<dir.num;i++)
        {
          if (dir.direntry[i].name==pData->m_FileName)
          {
            DebugAssert(!dir.direntry[i].dir || dir.direntry[i].bLink);
            found=TRUE;
            break;
          }
        }
        if (found)
        {
          t_directory::t_direntry *direntry=new t_directory::t_direntry[dir.num-1];
          int j=0;
          int i;
          for (i=0; i<dir.num; i++)
          {
            if (dir.direntry[i].name==pData->m_FileName)
              continue;
            direntry[j]=dir.direntry[i];
            j++;
          }
          delete [] dir.direntry;
          dir.direntry=direntry;
          dir.num--;
          BOOL updated=FALSE;
          if (m_pDirectoryListing)
            if (m_pDirectoryListing->path==dir.path)
            {
              updated=TRUE;
              SetDirectoryListing(&dir);
            }
          if (!updated)
            if (WorkingDir.path==dir.path)
            {
              updated=TRUE;
              m_pOwner->SetWorkingDir(&dir);
            }
        }
      }
    }
    ResetOperation(FZ_REPLY_OK);
  }
}

void CFtpControlSocket::RemoveDir(CString dirname, const CServerPath &path)
{

  class CRemoveDirData : public CFtpControlSocket::t_operation::COpData
  {
public:
    CRemoveDirData() {}
    virtual ~CRemoveDirData() {}
    CString m_DirName;
    CServerPath path;
  };
  if (dirname != L"")
  {
    DebugAssert(!path.IsEmpty());
    DebugAssert(m_Operation.nOpMode == CSMODE_NONE);
    DebugAssert(m_Operation.nOpState == -1);
    DebugAssert(!m_Operation.pData);
    m_Operation.nOpMode = CSMODE_RMDIR;
    CServerPath newPath = path;
    if (!newPath.AddSubdir(dirname))
    {
      ShowStatus(L"Unable to concatenate path", FZ_LOG_ERROR);
      return;
    }
    if (!Send(L"RMD "+ newPath.GetPathUnterminated()))
      return;
    CRemoveDirData *data = new CRemoveDirData;
    data->m_DirName = dirname;
    data->path = path;
    m_Operation.pData = data;
  }
  else
  {
    DebugAssert(path.IsEmpty());
    DebugAssert(m_Operation.nOpMode == CSMODE_RMDIR);
    DebugAssert(m_Operation.nOpState == -1);
    DebugAssert(m_Operation.pData);
    int res = GetReplyCode();
    if (res == 2 || res == 3)
    { //Remove dir from cached dirs
      CRemoveDirData *pData=  (CRemoveDirData *)m_Operation.pData;
      t_directory dir;
      BOOL res = FALSE;
      if (m_pDirectoryListing)
      {
        if (m_pDirectoryListing->path == pData->path)
        {
          dir = *m_pDirectoryListing;
          res = TRUE;
        }
      }
      t_directory WorkingDir;
      BOOL bFound = m_pOwner->GetWorkingDir(&WorkingDir);
      if (!res && bFound)
        if (WorkingDir.path == pData->path)
        {
          dir = WorkingDir;
          res = TRUE;
        }
      if (res)
      {
        BOOL found = FALSE;
        for (int i = 0; i < dir.num; i++)
        {
          if (dir.direntry[i].name == pData->m_DirName)
          {
            DebugAssert(dir.direntry[i].dir);
            found = TRUE;
            break;
          }
        }
        if (found)
        {
          t_directory::t_direntry *direntry = new t_directory::t_direntry[dir.num-1];
          int j = 0;
          int i;
          for (i = 0; i < dir.num; i++)
          {
            if (dir.direntry[i].name == pData->m_DirName)
              continue;
            direntry[j] = dir.direntry[i];
            j++;
          }
          delete [] dir.direntry;
          dir.direntry = direntry;
          dir.num--;
          BOOL updated = FALSE;
          if (m_pDirectoryListing)
            if (m_pDirectoryListing->path == dir.path)
            {
              updated = TRUE;
              SetDirectoryListing(&dir);
            }
          if (!updated)
            if (WorkingDir.path == dir.path)
            {
              updated = TRUE;
              m_pOwner->SetWorkingDir(&dir);
            }
        }
      }
      ResetOperation(FZ_REPLY_OK);
    }
    else
      ResetOperation(FZ_REPLY_ERROR);
  }
}

int CFtpControlSocket::FileTransferHandleDirectoryListing(t_directory * pDirectory)
{
  SetDirectoryListing(pDirectory);

  m_Operation.nOpState = FILETRANSFER_TYPE;
  delete m_pTransferSocket;
  m_pTransferSocket = 0;

  return CheckOverwriteFileAndCreateTarget();
}

int CFtpControlSocket::CheckOverwriteFileAndCreateTarget()
{
  int nReplyError = CheckOverwriteFile();
  if (!nReplyError)
  {
    CFileTransferData * pData = static_cast<CFileTransferData *>(m_Operation.pData);
    if (pData->transferfile.get && (pData->transferfile.OnTransferOut == NULL))
    {
      CString path = pData->transferfile.localfile;
      if (path.ReverseFind(L'\\') != -1)
      {
        path = path.Left(path.ReverseFind(L'\\')+1);
        CString path2;
        while (path != L"")
        {
          path2 += path.Left(path.Find(L"\\") + 1);
          path = path.Mid(path.Find(L"\\") + 1);
          CreateDirectory(path2, 0);
        }
      }
    }
  }
  return nReplyError;
}

int CFtpControlSocket::CheckOverwriteFile()
{
  if (!m_Operation.pData)
  {
    return FZ_REPLY_ERROR;
  }

  CFileTransferData *pData = reinterpret_cast<CFileTransferData *>(m_Operation.pData);

  int nReplyError = 0;
  CFileStatus64 status;
  if ((pData->transferfile.OnTransferOut != NULL) ||
      (pData->transferfile.OnTransferIn != NULL))
  {
    m_Operation.nOpState = FILETRANSFER_TYPE;
  }
  else if (!GetStatus64(pData->transferfile.localfile, status))
  {
    if (!pData->transferfile.get)
    {
      ShowStatus(IDS_ERRORMSG_CANTGETLISTFILE,FZ_LOG_ERROR);
      nReplyError = FZ_REPLY_CRITICALERROR; //File has to exist when uploading
    }
    else
    {
      m_Operation.nOpState = FILETRANSFER_TYPE;
    }
  }
  else
  {
    if (status.m_attribute & 0x10)
    {
      nReplyError = FZ_REPLY_CRITICALERROR; //Can't transfer to/from dirs
    }
    else
    {
      _int64 localsize;
      if (!GetLength64(pData->transferfile.localfile, localsize))
        if (!pData->transferfile.get)
          nReplyError = FZ_REPLY_CRITICALERROR;
        else
          m_Operation.nOpState = FILETRANSFER_TYPE;


      CTime *localtime = NULL;
      TRY
      {
        if (status.m_has_mtime && status.m_mtime != -1)
          localtime = new CTime(status.m_mtime);
      }
      CATCH_ALL(e)
      {
        TCHAR buffer[1024];
        CString str =L"Exception creating CTime object: ";
        if (e->GetErrorMessage(buffer, 1024, NULL))
          str += buffer;
        else
          str += L"Unknown exception";
        LogMessageRaw(FZ_LOG_WARNING, str);
        localtime = NULL;
      }
      END_CATCH_ALL;
      BOOL bRemoteFileExists = FALSE;
      __int64 remotesize = -1;
      t_directory::t_direntry::t_date remotetime;
      if (m_pDirectoryListing)
      {
        for (int i=0; i<m_pDirectoryListing->num; i++)
        {
          CString remotefile = pData->transferfile.remotefile;
          if (m_pDirectoryListing->direntry[i].name == remotefile)
          {
            remotesize = m_pDirectoryListing->direntry[i].size;
            remotetime = m_pDirectoryListing->direntry[i].date;
            bRemoteFileExists = TRUE;
            break;
          }
        }
      }
      if (!bRemoteFileExists && pData->hasRemoteDate)
      {
        remotetime = pData->remoteDate;
        bRemoteFileExists = TRUE;
      }
      if (remotesize == -1 && pData->pFileSize)
      {
        remotesize = *pData->pFileSize;
        bRemoteFileExists = TRUE;
      }

      if (bRemoteFileExists || pData->transferfile.get )
      {
        COverwriteRequestData *pOverwriteData = new COverwriteRequestData;
        t_transferfile *pTransferFile = new t_transferfile;
        *pTransferFile = pData->transferfile;
        pOverwriteData->pTransferFile = pTransferFile;
        if (pData->transferfile.get)
        {
          int pos = pData->transferfile.localfile.ReverseFind(L'\\');
          // pos can be -1 here, e.g. in scripting, the code below still works then
          pOverwriteData->FileName1 = pData->transferfile.localfile.Mid(pos+1);
          pOverwriteData->FileName2 = pData->transferfile.remotefile;
          pOverwriteData->path1 = pData->transferfile.localfile.Left(pos+1);
          pOverwriteData->path2 = pData->transferfile.remotepath.GetPath();
          pOverwriteData->size1 = localsize;
          pOverwriteData->size2 = remotesize;
        }
        else
        {
          int pos = pData->transferfile.localfile.ReverseFind(L'\\');
          // pos can be -1 here, e.g. in scripting, the code below still works then
          pOverwriteData->FileName1 = pData->transferfile.remotefile;
          pOverwriteData->FileName2 = pData->transferfile.localfile.Mid(pos+1);
          pOverwriteData->path1 = pData->transferfile.remotepath.GetPath();
          pOverwriteData->path2 = pData->transferfile.localfile.Left(pos+1);
          pOverwriteData->size1 = remotesize;
          pOverwriteData->size2 = localsize;
        }
        pOverwriteData->localtime = localtime;
        pOverwriteData->remotetime = remotetime;
        pOverwriteData->nRequestID = m_pOwner->GetNextAsyncRequestID();
        if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_OVERWRITE), (LPARAM)pOverwriteData))
        {
          delete pOverwriteData;
          nReplyError = FZ_REPLY_ERROR;
        }
        else
        {
          m_bCheckForTimeout = FALSE;
          m_Operation.nOpState = FILETRANSFER_WAIT;
        }
      }
      else
      {
        m_Operation.nOpState = FILETRANSFER_TYPE;
        delete localtime;
      }
    }
  }
  return nReplyError;
}

void CFtpControlSocket::SetFileExistsAction(int nAction, COverwriteRequestData *pData)
{
  if (!pData)
    return;
  if (!(m_Operation.nOpMode & CSMODE_TRANSFER))
    return;
  if (m_Operation.nOpState != FILETRANSFER_WAIT)
    return;

  CFileTransferData* pTransferData = reinterpret_cast<CFileTransferData*>(m_Operation.pData);
  if (!pTransferData)
    return;

  pTransferData->transferdata.bResume = false;

  m_bCheckForTimeout = TRUE;
  int nReplyError = 0;
  switch (nAction)
  {
  case FILEEXISTS_SKIP:
    nReplyError = FZ_REPLY_OK;
    break;
  case FILEEXISTS_OVERWRITE:
    pTransferData->nWaitNextOpState = FILETRANSFER_TYPE;
    break;
  case FILEEXISTS_RENAME:
    if (pTransferData->transferfile.get)
    {
      CFileStatus64 status;
      if (GetStatus64(pData->FileName1, status))
      {
        ShowStatus(IDS_ERRORMSG_NAMEINUSE, FZ_LOG_ERROR);
        nReplyError=  FZ_REPLY_CRITICALERROR;
      }
      else
      {
        pTransferData->transferfile.localfile = pData->path1+pData->FileName1;
        //Replace invalid characters in the local filename
        int pos = pTransferData->transferfile.localfile.ReverseFind(L'\\');
        for (int i = (pos+1); i < pTransferData->transferfile.localfile.GetLength(); i++)
          if (pTransferData->transferfile.localfile[i] == L':')
            pTransferData->transferfile.localfile.SetAt(i, L'_');

        pTransferData->nWaitNextOpState=  FILETRANSFER_TYPE;
      }
    }
    else
    {
      DebugAssert(m_pDirectoryListing);
      int i;
      for (i = 0; i < m_pDirectoryListing->num; i++)
      {
        if (m_pDirectoryListing->direntry[i].name == pData->FileName1)
        {
          ShowStatus(IDS_ERRORMSG_NAMEINUSE, FZ_LOG_ERROR);
          nReplyError = FZ_REPLY_CRITICALERROR;
          break;
        }
      }
      if (i==m_pDirectoryListing->num)
      {
        pTransferData->transferfile.remotefile = pData->FileName1;
        pTransferData->nWaitNextOpState = FILETRANSFER_TYPE;
      }
    }
    break;
  case FILEEXISTS_RESUME:
    if (pData->size1 >= 0)
    {
      pTransferData->transferdata.bResume = TRUE;
    }
    pTransferData->nWaitNextOpState = FILETRANSFER_TYPE;
    break;
  case FILEEXISTS_COMPLETE:
    // Simulating transfer finish
    m_Operation.nOpState=FILETRANSFER_WAITFINISH;
    TransferFinished(true);
    return; // Avoid call to FileTransfer below
  }
  if (nReplyError == FZ_REPLY_OK)
    ResetOperation(FZ_REPLY_OK);
  else if (nReplyError)
    ResetOperation(FZ_REPLY_ERROR | nReplyError); //Error transferring the file
  else
    FileTransfer();
}

void CFtpControlSocket::SendKeepAliveCommand()
{
  ShowStatus(L"Sending dummy command to keep session alive.", FZ_LOG_PROGRESS);
  m_bKeepAliveActive=TRUE;
  //Choose a random command from the list
  TCHAR commands[4][7]={L"PWD",L"REST 0",L"TYPE A",L"TYPE I"};
  int choice=(rand()*4)/(RAND_MAX+1);
  Send(commands[choice]);
}

void CFtpControlSocket::MakeDir(const CServerPath &path)
{
  //Directory creation works like this:
  //Find existing parent and create subdirs one by one
  if (m_Operation.nOpState == MKD_INIT)
  {
    DebugAssert(!path.IsEmpty());
    DebugAssert(m_Operation.nOpMode==CSMODE_NONE);
    DebugAssert(!m_Operation.pData);
    m_Operation.nOpMode = CSMODE_MKDIR;
    if (!Send(L"CWD "+path.GetParent().GetPathUnterminated()))
      return;
    CMakeDirData *data = new CMakeDirData;
    data->path = path;
    data->Current = path.GetParent();
    data->Segments.push_front(path.GetLastSegment());
    m_Operation.pData = data;
    m_Operation.nOpState = MKD_FINDPARENT;
  }
  else if (m_Operation.nOpState==MKD_FINDPARENT)
  {
    DebugAssert(m_Operation.nOpMode==CSMODE_MKDIR);
    DebugAssert(path.IsEmpty());
    DebugAssert(m_Operation.pData);
    CMakeDirData *pData=(CMakeDirData *)m_Operation.pData;
    int res=GetReplyCode();
    if (res==2 || res==3)
    {
      m_pOwner->SetCurrentPath(pData->Current);

      m_Operation.nOpState=MKD_MAKESUBDIRS;
      pData->Current.AddSubdir(pData->Segments.front());
      CString Segment=pData->Segments.front();
      pData->Segments.pop_front();
      if (Send( L"MKD " + Segment))
        m_Operation.nOpState = MKD_CHANGETOSUBDIR;
      else
        return;
    }
    else
    {
      if (!pData->Current.HasParent())
        ResetOperation(FZ_REPLY_ERROR);
      else
      {
        pData->Segments.push_front(pData->Current.GetLastSegment());
        pData->Current=pData->Current.GetParent();
        if (!Send(L"CWD "+pData->Current.GetPathUnterminated()))
          return;
      }
    }
  }
  else if (m_Operation.nOpState==MKD_MAKESUBDIRS)
  {
    int res=GetReplyCode();
    if (res==2 || res==3)
    { //Create dir entry in parent dir
      CMakeDirData *pData=(CMakeDirData *)m_Operation.pData;

      DebugAssert(!pData->Segments.empty());

      m_pOwner->SetCurrentPath(pData->Current);

      pData->Current.AddSubdir(pData->Segments.front());
      CString Segment=pData->Segments.front();
      pData->Segments.pop_front();
      if (Send( L"MKD " + Segment))
        m_Operation.nOpState=MKD_CHANGETOSUBDIR;
      else
        return;
    }
    else
      ResetOperation(FZ_REPLY_ERROR);
  }
  else if (m_Operation.nOpState==MKD_CHANGETOSUBDIR)
  {
    CMakeDirData *pData=(CMakeDirData *)m_Operation.pData;
    int res=GetReplyCode();
    if (res==2 || res==3 || //Creation successful
      GetReply() == L"550 Directory already exists")//Creation was successful, although someone else did the work for us
    { //Create dir entry in parent dir
      CServerPath path2=pData->Current;
      if (path2.HasParent())
      {
        CString name=path2.GetLastSegment();
        path2=path2.GetParent();

        t_directory dir;
        BOOL res = FALSE;
        if (m_pDirectoryListing)
        {
          if (m_pDirectoryListing->path == path2)
          {
            dir = *m_pDirectoryListing;
            res = TRUE;
          }
        }
        t_directory WorkingDir;
        BOOL bFound = m_pOwner->GetWorkingDir(&WorkingDir);
        if (!res && bFound)
          if (WorkingDir.path == path2)
          {
            dir = WorkingDir;
            res = TRUE;
          }
        if (!res)
        {
          dir.path = path2;
          dir.server = m_CurrentServer;
        }

        int i;
        for (i=0; i<dir.num; i++)
          if (dir.direntry[i].name==name)
          {
            LogMessage(FZ_LOG_WARNING, L"Dir already exists in cache!");
            break;
          }
        if (i==dir.num)
        {
          t_directory::t_direntry *entries=new t_directory::t_direntry[dir.num+1];
          for (i=0;i<dir.num;i++)
            entries[i]=dir.direntry[i];
          entries[i].name=name;
          entries[i].dir=TRUE;
          entries[i].date.hasdate=FALSE;
          entries[i].size=-1;
          entries[i].bUnsure=FALSE;
          delete [] dir.direntry;
          dir.direntry=entries;
          dir.num++;

          BOOL updated = FALSE;
          if (m_pDirectoryListing && m_pDirectoryListing->path == dir.path)
          {
            updated = TRUE;
            SetDirectoryListing(&dir, bFound && WorkingDir.path == dir.path);
          }
          if (!updated)
            if (bFound && WorkingDir.path == dir.path)
            {
              updated = TRUE;
              m_pOwner->SetWorkingDir(&dir);
            }
        }
      }
    }

    //Continue operation even if MKD failed, maybe another thread did create this directory for us
    if (pData->Segments.empty())
      ResetOperation(FZ_REPLY_OK);
    else
    {
      if (Send( L"CWD " + pData->Current.GetPathUnterminated()))
        m_Operation.nOpState=MKD_MAKESUBDIRS;
      else
        return;
    }
  }
  else
    DebugFail();
}

void CFtpControlSocket::Rename(CString oldName, CString newName, const CServerPath &path, const CServerPath &newPath)
{
  class CRenameData : public CFtpControlSocket::t_operation::COpData
  {
  public:
    CRenameData() {}
    virtual ~CRenameData() {}
    CString oldName, newName;
    CServerPath path;
    CServerPath newPath;
  };
  if (oldName != L"")
  {
    DebugAssert(newName != L"");
    DebugAssert(!path.IsEmpty());
    DebugAssert(m_Operation.nOpMode == CSMODE_NONE);
    DebugAssert(m_Operation.nOpState == -1);
    DebugAssert(!m_Operation.pData);
    m_Operation.nOpMode = CSMODE_RENAME;
    if (!Send(L"RNFR " + path.FormatFilename(oldName)))
      return;
    CRenameData *data = new CRenameData;
    data->oldName = oldName;
    data->newName = newName;
    data->path = path;
    data->newPath = newPath;
    m_Operation.pData = data;
  }
  else
  {
    DebugAssert(oldName == L"");
    DebugAssert(path.IsEmpty());
    DebugAssert(m_Operation.nOpMode == CSMODE_RENAME);
    DebugAssert(m_Operation.pData);
    CRenameData *pData = reinterpret_cast<CRenameData *>(m_Operation.pData);

    if (m_Operation.nOpState == -1)
    {
      int res = GetReplyCode();
      if (res == 2 || res == 3)
      {
        m_Operation.nOpState++;
        if (pData->newPath.IsEmpty())
        {
          if (!Send(L"RNTO " + pData->path.FormatFilename(((CRenameData *)m_Operation.pData)->newName)))
            return;
        }
        else
          if (!Send(L"RNTO " + pData->newPath.FormatFilename(((CRenameData *)m_Operation.pData)->newName)))
            return;
      }
      else
        ResetOperation(FZ_REPLY_ERROR);
    }
    else
    {
      int res = GetReplyCode();
      if (res == 2 || res == 3)
      { //Rename entry in cached directory
        CRenameData *pData = reinterpret_cast<CRenameData *>(m_Operation.pData);

        t_directory dir;
        BOOL res = FALSE;
        if (m_pDirectoryListing)
        {
          if (m_pDirectoryListing->path == pData->path)
          {
            dir = *m_pDirectoryListing;
            res = TRUE;
          }
        }
        t_directory WorkingDir;
        BOOL bFound = m_pOwner->GetWorkingDir(&WorkingDir);
        if (!res && bFound)
          if (WorkingDir.path == pData->path)
          {
            dir = WorkingDir;
            res = TRUE;
          }
        if (res)
        {
          for (int i=0; i<dir.num; i++)
            if (dir.direntry[i].name == pData->oldName)
            {
              if (pData->newPath.IsEmpty())
              {
                dir.direntry[i].name = pData->newName;

                BOOL updated = FALSE;
                if (m_pDirectoryListing && m_pDirectoryListing->path == dir.path)
                {
                  updated = TRUE;
                  SetDirectoryListing(&dir, WorkingDir.path == dir.path);
                }
                if (!updated)
                  if (WorkingDir.path == dir.path)
                  {
                    updated = TRUE;
                    m_pOwner->SetWorkingDir(&dir);
                  }
              }
              else
              {
                t_directory::t_direntry oldentry = dir.direntry[i];

                for (int j = i+1; j < dir.num; j++)
                {
                  dir.direntry[j-1] = dir.direntry[j];
                }
                dir.num--;

                BOOL updated = FALSE;
                if (m_pDirectoryListing && m_pDirectoryListing->path == dir.path)
                {
                  updated = TRUE;
                  SetDirectoryListing(&dir, WorkingDir.path == dir.path);
                }
                if (!updated)
                  if (WorkingDir.path == dir.path)
                  {
                    updated = TRUE;
                    m_pOwner->SetWorkingDir(&dir);
                  }

                BOOL res = FALSE;
                if (m_pDirectoryListing)
                {
                  if (m_pDirectoryListing->path == pData->newPath)
                  {
                    dir = *m_pDirectoryListing;
                    res = TRUE;
                  }
                }
                t_directory WorkingDir;
                BOOL bFound = m_pOwner->GetWorkingDir(&WorkingDir);
                if (!res && bFound)
                  if (WorkingDir.path == pData->newPath)
                  {
                    dir = WorkingDir;
                    res = TRUE;
                  }
                if (res)
                {
                  t_directory::t_direntry *direntry = new t_directory::t_direntry[dir.num + 1];
                  for (int i = 0; i < dir.num; i++)
                    direntry[i] = dir.direntry[i];
                  direntry[dir.num] = oldentry;
                  direntry[dir.num].name = pData->newName;
                  dir.num++;
                  delete [] dir.direntry;
                  dir.direntry = direntry;

                  BOOL updated = FALSE;
                  if (m_pDirectoryListing && m_pDirectoryListing->path == dir.path)
                  {
                    updated = TRUE;
                    SetDirectoryListing(&dir, bFound && WorkingDir.path == dir.path);
                  }
                  if (!updated)
                    if (bFound && WorkingDir.path == dir.path)
                    {
                      updated = TRUE;
                      m_pOwner->SetWorkingDir(&dir);
                    }
                }
              }
              break;
            }
        }
        ResetOperation(FZ_REPLY_OK);
      }
      else
        ResetOperation(FZ_REPLY_ERROR);
    }
  }
}

void CFtpControlSocket::SetVerifyCertResult(int nResult, t_SslCertData *pData)
{
  DebugAssert(pData);
  if (!m_pSslLayer)
    return;
  if (DebugAlwaysFalse(m_Operation.nOpMode != CSMODE_CONNECT))
    return;
  m_bCheckForTimeout = TRUE;
  m_pSslLayer->SetNotifyReply(pData->priv_data, SSL_VERIFY_CERT, nResult);
  m_LastRecvTime = CTime::GetCurrentTime();
}

void CFtpControlSocket::OnTimer()
{
  CheckForTimeout();
  ResumeTransfer();
  if (GetOptionVal(OPTION_KEEPALIVE))
  {
    if (!m_pOwner->IsBusy() && m_pOwner->IsConnected() && !m_bKeepAliveActive)
    {
      //Getting intervals for the Keep Alive feature
      int low=GetOptionVal(OPTION_INTERVALLOW);
      int diff=GetOptionVal(OPTION_INTERVALHIGH)-low;

      //Choose a new delay
      int delay=low+(rand()*diff)/RAND_MAX;

      CTimeSpan span=CTime::GetCurrentTime()-m_LastSendTime;
      if (span.GetTotalSeconds()>=delay)
        SendKeepAliveCommand();
    }
  }
}

BOOL CFtpControlSocket::IsReady()
{
  return !m_bKeepAliveActive;
}

void CFtpControlSocket::Chmod(CString filename, const CServerPath &path, int nValue)
{
  m_Operation.nOpMode=CSMODE_CHMOD;
  CString str;
  str.Format( L"SITE CHMOD %03d %s", nValue, (LPCTSTR)path.FormatFilename(filename));
  Send(str);
}

void CFtpControlSocket::SetAsyncRequestResult(int nAction, CAsyncRequestData *pData)
{
  switch (pData->nRequestType)
  {
  case FZ_ASYNCREQUEST_OVERWRITE:
    SetFileExistsAction(nAction, (COverwriteRequestData *)pData);
    break;
  case FZ_ASYNCREQUEST_VERIFYCERT:
    SetVerifyCertResult(nAction, ((CVerifyCertRequestData *)pData)->pCertData );
    break;
  case FZ_ASYNCREQUEST_NEEDPASS:
    if (m_Operation.nOpMode!=CSMODE_CONNECT ||
      m_Operation.nOpState != CONNECT_NEEDPASS)
      break;
    if (!m_RecvBuffer.empty() && m_RecvBuffer.front() != "")
    {
      DoClose();
      break;
    }
    if (!nAction)
    {
      DoClose(FZ_REPLY_CRITICALERROR|FZ_REPLY_CANCEL);
      ShowStatus(IDS_ERRORMSG_INTERRUPTED,FZ_LOG_ERROR);
      break;
    }
    else
    {
      m_bCheckForTimeout = TRUE;
      m_CurrentServer.pass=((CNeedPassRequestData *)pData)->Password;
      m_Operation.nOpState=((CNeedPassRequestData *)pData)->nOldOpState;
      CLogonData *pLogonData = static_cast<CLogonData *>(m_Operation.pData);
      pLogonData->waitForAsyncRequest = false;
      pLogonData->gotPassword = true;
      LogOnToServer(TRUE);
    }
    break;
#ifndef MPEXT_NO_GSS
  case FZ_ASYNCREQUEST_GSS_AUTHFAILED:
    if (m_Operation.nOpMode!=CSMODE_CONNECT || m_Operation.nOpState!=CONNECT_GSS_FAILED)
      break;
    if (!m_RecvBuffer.empty() && m_RecvBuffer.front() != "")
    {
      DoClose();
      break;
    }
    if (!nAction)
    {
      DoClose(FZ_REPLY_CRITICALERROR|FZ_REPLY_CANCEL);
      ShowStatus(IDS_ERRORMSG_INTERRUPTED,FZ_LOG_ERROR);
      break;
    }
    m_bCheckForTimeout = TRUE;
    m_Operation.nOpState=-1;
    LogOnToServer(TRUE);
    break;
  case FZ_ASYNCREQUEST_GSS_NEEDPASS:
    if (m_Operation.nOpMode!=CSMODE_CONNECT ||
      m_Operation.nOpState != CONNECT_GSS_NEEDPASS)
      break;
    if (!m_RecvBuffer.empty() && m_RecvBuffer.front() != "")
    {
      DoClose();
      break;
    }
    if (!nAction)
    {
      DoClose(FZ_REPLY_CRITICALERROR|FZ_REPLY_CANCEL);
      ShowStatus(IDS_ERRORMSG_INTERRUPTED,FZ_LOG_ERROR);
      break;
    }
    else
    {
      m_bCheckForTimeout = TRUE;
      m_CurrentServer.pass=((CGssNeedPassRequestData *)pData)->pass;
      m_Operation.nOpState=((CGssNeedPassRequestData *)pData)->nOldOpState;
      LogOnToServer(TRUE);
    }
    break;
  case FZ_ASYNCREQUEST_GSS_NEEDUSER:
    if (m_Operation.nOpMode != CSMODE_CONNECT ||
      m_Operation.nOpState != CONNECT_GSS_NEEDUSER)
      break;
    if (!m_RecvBuffer.empty() && m_RecvBuffer.front() != "")
    {
      DoClose();
      break;
    }
    if (!nAction)
    {
      DoClose(FZ_REPLY_CRITICALERROR | FZ_REPLY_CANCEL);
      ShowStatus(IDS_ERRORMSG_INTERRUPTED, FZ_LOG_ERROR);
      break;
    }
    else
    {
      m_bCheckForTimeout = TRUE;
      m_CurrentServer.user = ((CGssNeedUserRequestData *)pData)->user;
      m_Operation.nOpState=((CGssNeedUserRequestData *)pData)->nOldOpState;
      LogOnToServer(TRUE);
    }
    break;
#endif
  default:
    LogMessage(FZ_LOG_WARNING, L"Unknown request reply %d", pData->nRequestType);
    break;
  }
}

int CFtpControlSocket::OnLayerCallback(std::list<t_callbackMsg>& callbacks)
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
#ifndef MPEXT_NO_GSS
        else if (iter->pLayer == m_pGssLayer)
          LogMessage(FZ_LOG_INFO, L"m_pGssLayer changed state from %s to %s", state2Desc, state1Desc);
#endif
        else if (iter->pLayer == m_pSslLayer)
        {
          delete [] iter->str;
          LogMessage(FZ_LOG_INFO, L"TLS layer changed state from %s to %s", state2Desc, state1Desc);
        }
        else
          LogMessage(FZ_LOG_INFO, L"Layer @ %d changed state from %s to %s", iter->pLayer, state2Desc, state1Desc);
      }
    }
    else if (iter->nType == LAYERCALLBACK_LAYERSPECIFIC)
    {
      USES_CONVERSION;
      if (iter->pLayer == m_pProxyLayer)
      {
        switch (iter->nParam1)
        {
        case PROXYERROR_NOERROR:
          ShowStatus(IDS_PROXY_CONNECTED, FZ_LOG_STATUS);
          break;
        case PROXYERROR_NOCONN:
          ShowStatus(IDS_ERRORMSG_PROXY_NOCONN, FZ_LOG_ERROR);
          break;
        case PROXYERROR_REQUESTFAILED:
          ShowStatus(IDS_ERRORMSG_PROXY_REQUESTFAILED, FZ_LOG_ERROR);
          if (iter->str)
            ShowStatus(A2T(iter->str), FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHTYPEUNKNOWN:
          ShowStatus(IDS_ERRORMSG_PROXY_AUTHTYPEUNKNOWN, FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHFAILED:
          ShowStatus(IDS_ERRORMSG_PROXY_AUTHFAILED, FZ_LOG_ERROR);
          break;
        case PROXYERROR_AUTHNOLOGON:
          ShowStatus(IDS_ERRORMSG_PROXY_AUTHNOLOGON, FZ_LOG_ERROR);
          break;
        case PROXYERROR_CANTRESOLVEHOST:
          ShowStatus(IDS_ERRORMSG_PROXY_CANTRESOLVEHOST, FZ_LOG_ERROR);
          break;
        default:
          LogMessage(FZ_LOG_WARNING, L"Unknown proxy error" );
        }
      }
#ifndef MPEXT_NO_GSS
      else if (iter->pLayer == m_pGssLayer)
      {
        switch (iter->nParam1)
        {
        case GSS_INFO:
          LogMessageRaw(FZ_LOG_INFO, A2CT(iter->str));
          break;
        case GSS_ERROR:
          LogMessageRaw(FZ_LOG_APIERROR, A2CT(iter->str));
          break;
        case GSS_COMMAND:
          ShowStatus(A2CT(iter->str), FZ_LOG_COMMAND);
          break;
        case GSS_REPLY:
          ShowStatus(A2CT(iter->str), FZ_LOG_REPLY);
          break;
        }
      }
#endif
      else if (iter->pLayer == m_pSslLayer)
      {
        switch (iter->nParam1)
        {
        case SSL_INFO:
          switch (iter->nParam2)
          {
          case SSL_INFO_ESTABLISHED:
            ShowStatus(IDS_STATUSMSG_SSLESTABLISHED, FZ_LOG_STATUS);
            if (m_Operation.nOpState == CONNECT_SSL_WAITDONE)
            {
              LogOnToServer();
            }
            break;
          }
          break;
        case SSL_FAILURE:
          switch (iter->nParam2)
          {
          case SSL_FAILURE_UNKNOWN:
            // CTransferSocket has a special treatment of SSL_FAILURE_UNKNOWN,
            // as an indication of a re-key failure.
            ShowStatus(IDS_ERRORMSG_UNKNOWNSSLERROR, FZ_LOG_ERROR);
            break;
          case SSL_FAILURE_ESTABLISH:
            ShowStatus(IDS_ERRORMSG_CANTESTABLISHSSLCONNECTION, FZ_LOG_ERROR);
            break;
          case SSL_FAILURE_INITSSL:
            ShowStatus(IDS_ERRORMSG_CANTINITSSL, FZ_LOG_ERROR);
            break;
          case SSL_FAILURE_VERIFYCERT:
            ShowStatus(IDS_ERRORMSG_SSLCERTIFICATEERROR, FZ_LOG_ERROR);
            break;
          case SSL_FAILURE_CERTREJECTED:
            ShowStatus(IDS_ERRORMSG_CERTREJECTED, FZ_LOG_ERROR);
            m_bDidRejectCertificate = TRUE;
            break;
          }
          TriggerEvent(FD_CLOSE);
          break;
        case SSL_VERIFY_CERT:
          t_SslCertData *pData = new t_SslCertData;
          LPCTSTR CertError = NULL;
          if (m_pSslLayer->GetPeerCertificateData(*pData, CertError))
          {
            CVerifyCertRequestData *pRequestData = new CVerifyCertRequestData;
            pRequestData->nRequestID=m_pOwner->GetNextAsyncRequestID();

            pRequestData->pCertData = pData;

            if (!GetIntern()->PostMessage(FZ_MSG_MAKEMSG(FZ_MSG_ASYNCREQUEST, FZ_ASYNCREQUEST_VERIFYCERT), (LPARAM)pRequestData))
            {
              delete pRequestData->pCertData;
              delete pRequestData;
              ResetOperation(FZ_REPLY_ERROR);
            }
            else
            {
              m_bCheckForTimeout = FALSE;
            }
            delete [] iter->str;
            continue;
          }
          else
          {
            delete pData;
            delete [] iter->str;
            CString str;
            str.Format(TLS_CERT_DECODE_ERROR, CertError);
            ShowStatus(str, FZ_LOG_ERROR);
            ResetOperation(FZ_REPLY_ERROR);
            continue;
          }
          break;
        }
        delete [] iter->str;
        continue;
      }
#ifndef MPEXT_NO_GSS
      else
      if (iter->pLayer == m_pGssLayer)
      {
        if (iter->nParam1 == GSS_AUTHCOMPLETE ||
          iter->nParam1 == GSS_AUTHFAILED)
        {
          LogOnToServer(TRUE);
          delete [] iter->str;
          continue;
        }
      }
#endif
    }
    delete [] iter->str;
  }
  return 0;
}

_int64 CFtpControlSocket::GetSpeedLimit(CTime &time, int valType, int valValue)
{
  int type = GetOptionVal(valType);

  if ( type == 1)
    return ( _int64)GetOptionVal(valValue) * 1024;

  return ( _int64)1000000000000;  // I hope that when there will be something with 1000GB/s then I'll change it :)
}

_int64 CFtpControlSocket::GetSpeedLimit(enum transferDirection direction, CTime &time)
{
  if (direction == download)
    return GetSpeedLimit(time, OPTION_SPEEDLIMIT_DOWNLOAD_TYPE, OPTION_SPEEDLIMIT_DOWNLOAD_VALUE);
  else
    return GetSpeedLimit(time, OPTION_SPEEDLIMIT_UPLOAD_TYPE, OPTION_SPEEDLIMIT_UPLOAD_VALUE);
  return ( _int64)1000000000000;
}

_int64 CFtpControlSocket::GetAbleToUDSize( bool &beenWaiting, CTime &curTime, _int64 &curLimit, std::list<CFtpControlSocket::t_ActiveList>::iterator &iter, enum transferDirection direction, int nBufSize)
{
  beenWaiting = false;

  CTime nowTime = CTime::GetCurrentTime();
  _int64 ableToRead = BUFSIZE;

  if ( nowTime == curTime)
  {
    ableToRead = iter->nBytesAvailable;

    if (ableToRead <= 0)
    {
      //  we should wait till next second
      nowTime = CTime::GetCurrentTime();

      while (nowTime == curTime && !iter->nBytesAvailable)
      {
        if (beenWaiting)
        {
          //Check if there are other commands in the command queue.
          MSG msg;
          if (PeekMessage(&msg, 0, m_pOwner->m_nInternalMessageID, m_pOwner->m_nInternalMessageID, PM_NOREMOVE))
          {
            LogMessage(FZ_LOG_INFO, L"Message waiting in queue, resuming later");
            return 0;
          }
        }
        m_SpeedLimitSync.Unlock();
        Sleep(100);
        m_SpeedLimitSync.Lock();
        nowTime = CTime::GetCurrentTime();
        beenWaiting = true;

        // Since we didn't hold the critical section for some time, we have to renew the iterator
        for (iter = m_InstanceList[direction].begin(); iter != m_InstanceList[direction].end(); iter++)
          if (iter->pOwner == this)
            break;
        if (iter == m_InstanceList[direction].end())
          return 0;
      }
    }
    ableToRead = iter->nBytesAvailable;
  }

  if (nowTime != curTime)
  {
    if (ableToRead > 0)
      ableToRead = 0;

    curLimit = GetSpeedLimit(direction, curTime);
    __int64 nMax = curLimit / m_InstanceList[direction].size();
    _int64 nLeft = 0;
    int nCount = 0;
    std::list<t_ActiveList>::iterator iter2;
    for (iter2 = m_InstanceList[direction].begin(); iter2 != m_InstanceList[direction].end(); iter2++)
    {
      if (iter2->nBytesAvailable>0)
      {
        nLeft += iter2->nBytesAvailable;
        iter2->nBytesTransferred = 1;
      }
      else
      {
        nCount++;
        iter2->nBytesTransferred = 0;
      }
      iter2->nBytesAvailable = nMax;
    }
    if (nLeft && nCount)
    {
      nMax = nLeft / nCount;
      for (iter2 = m_InstanceList[direction].begin(); iter2 != m_InstanceList[direction].end(); iter2++)
      {
        if (!iter2->nBytesTransferred)
          iter2->nBytesAvailable += nMax;
        else
          iter2->nBytesTransferred = 0;
      }
    }
    ableToRead = iter->nBytesAvailable;
  }

  curTime = nowTime;

  if (!nBufSize)
    nBufSize = BUFSIZE;
  if (ableToRead > nBufSize)
    ableToRead = nBufSize;

  return ableToRead;
}

_int64 CFtpControlSocket::GetAbleToTransferSize(enum transferDirection direction, bool &beenWaiting, int nBufSize)
{
  m_SpeedLimitSync.Lock();
  std::list<t_ActiveList>::iterator iter;
  for (iter = m_InstanceList[direction].begin(); iter != m_InstanceList[direction].end(); iter++)
    if (iter->pOwner == this)
      break;
  if (iter == m_InstanceList[direction].end())
  {
    t_ActiveList item;
    CTime time = CTime::GetCurrentTime();
    item.nBytesAvailable = GetSpeedLimit(direction, time) / (m_InstanceList[direction].size() + 1);
    item.nBytesTransferred = 0;
    item.pOwner = this;
    m_InstanceList[direction].push_back(item);
    iter = m_InstanceList[direction].end();
    iter--;
  }
  _int64 limit = GetAbleToUDSize(beenWaiting, m_CurrentTransferTime[direction], m_CurrentTransferLimit[direction], iter, direction, nBufSize);
  m_SpeedLimitSync.Unlock();
  return limit;
}

BOOL CFtpControlSocket::RemoveActiveTransfer()
{
  BOOL bFound = FALSE;
  m_SpeedLimitSync.Lock();
  std::list<t_ActiveList>::iterator iter;
  for (int i = 0; i < 2; i++)
  {
    for (iter = m_InstanceList[i].begin(); iter != m_InstanceList[i].end(); iter++)
      if (iter->pOwner == this)
      {
        m_InstanceList[i].erase(iter);
        bFound = TRUE;
        break;
      }
  }
  m_SpeedLimitSync.Unlock();
  return bFound;
}

BOOL CFtpControlSocket::SpeedLimitAddTransferredBytes(enum transferDirection direction, _int64 nBytesTransferred)
{
  m_SpeedLimitSync.Lock();
  std::list<t_ActiveList>::iterator iter;
  for (iter = m_InstanceList[direction].begin(); iter != m_InstanceList[direction].end(); iter++)
    if (iter->pOwner == this)
    {
      if (iter->nBytesAvailable > nBytesTransferred)
        iter->nBytesAvailable -= nBytesTransferred;
      else
        iter->nBytesAvailable = 0;
      iter->nBytesTransferred += nBytesTransferred;
      m_SpeedLimitSync.Unlock();
      return TRUE;
    }
  m_SpeedLimitSync.Unlock();
  return FALSE;
}

CString CFtpControlSocket::ConvertDomainName(CString domain)
{
  USES_CONVERSION;

  LPCWSTR buffer = T2CW(domain);

  char *utf8 = new char[wcslen(buffer) * 2 + 2];
  if (!WideCharToMultiByte(CP_UTF8, 0, buffer, -1, utf8, wcslen(buffer) * 2 + 2, 0, 0))
  {
    delete [] utf8;
    LogMessage(FZ_LOG_WARNING, L"Could not convert domain name");
    return domain;
  }

  char *output = 0;
  output = strdup(utf8);
  delete [] utf8;

  CString result = A2T(output);
  free(output);
  return result;
}

void CFtpControlSocket::LogSocketMessageRaw(int nMessageType, LPCTSTR pMsg)
{
  LogMessageRaw(nMessageType, pMsg);
}

bool CFtpControlSocket::LoggingSocketMessage(int nMessageType)
{
  return LoggingMessageType(nMessageType);
}

int CFtpControlSocket::GetSocketOptionVal(int OptionID) const
{
  return GetOptionVal(OptionID);
}

BOOL CFtpControlSocket::ParsePwdReply(CString& rawpwd)
{
  CServerPath realPath;
  BOOL Result = ParsePwdReply(rawpwd, realPath);
  if (Result)
  {
    m_pOwner->SetCurrentPath(realPath);
  }
  return Result;
}

BOOL CFtpControlSocket::ParsePwdReply(CString& rawpwd, CServerPath & realPath)
{
  CListData *pData = static_cast<CListData *>(m_Operation.pData);
  DebugAssert(pData);

  int pos1 = rawpwd.Find(L'"');
  int pos2 = rawpwd.ReverseFind(L'"');
  if (pos1 == -1 || pos2 == -1 || pos1 >= pos2)
  {
    LogMessage(FZ_LOG_WARNING, L"No quoted path found, try using first token as path");
    pos1 = rawpwd.Find(L' ');
    if (pos1 != -1)
    {
      pos2 = rawpwd.Find(L' ', pos1 + 1);
      if (pos2 == -1)
        pos2 = rawpwd.GetLength();
    }

    if (pos1 == -1)
    {
      LogMessage(FZ_LOG_WARNING, L"Can't parse path!");
      ResetOperation(FZ_REPLY_ERROR);
      return FALSE;
    }
  }
  rawpwd = rawpwd.Mid(pos1 + 1, pos2 - pos1 - 1);

  realPath = m_pOwner->GetCurrentPath();
  realPath.SetServer(m_CurrentServer);
  if (!realPath.SetPath(rawpwd))
  {
    LogMessage(FZ_LOG_WARNING, L"Can't parse path!");
    ResetOperation(FZ_REPLY_ERROR);
    return FALSE;
  }

  return TRUE;
}

void CFtpControlSocket::DiscardLine(CStringA line)
{
  if (m_Operation.nOpMode == CSMODE_CONNECT && m_Operation.nOpState == CONNECT_FEAT)
  {
    line.MakeUpper();
    while (line.Left(1) == " ")
    {
      line = line.Mid(1, line.GetLength() - 1);
    }
#ifndef MPEXT_NO_ZLIB
    if (line == "MODE Z" || line.Left(7) == "MODE Z ")
      m_zlibSupported = true;
    else
#endif
      if (line == "UTF8" && m_CurrentServer.nUTF8 != 2)
      m_bAnnouncesUTF8 = true;
    else if (line == "CLNT" || line.Left(5) == "CLNT ")
      m_hasClntCmd = true;
    else if (line == "MLSD")
    {
      m_serverCapabilities.SetCapability(mlsd_command, yes);
    }
    else if (line == "MDTM")
    {
      m_serverCapabilities.SetCapability(mdtm_command, yes);
    }
    else if (line.Left(4) == "MLST")
    {
      std::string facts;
      if (line.GetLength() > 5)
      {
        facts = (LPCSTR)line.Mid(5, line.GetLength() - 5);
      }
      m_serverCapabilities.SetCapability(mlsd_command, yes, facts);
    }
    else if (line == "MFMT")
    {
      m_serverCapabilities.SetCapability(mfmt_command, yes);
    }
  }
  else if (m_Operation.nOpMode == CSMODE_LISTFILE)
  {
    m_ListFile = line;
  }
}

int CFtpControlSocket::FileTransferListState(bool get)
{
  int Result;
  if (GetOptionVal(OPTION_MPEXT_NOLIST))
  {
    Result = FILETRANSFER_TYPE;
  }
  else
  {
    Result = NeedModeCommand() ? FILETRANSFER_LIST_MODE : (NeedOptsCommand() ? FILETRANSFER_LIST_OPTS : FILETRANSFER_LIST_TYPE);
  }
  return Result;
}

bool CFtpControlSocket::NeedModeCommand()
{
#ifdef MPEXT_NO_ZLIB
  return false;
#else
  bool useZlib;
  if (m_Operation.nOpMode == CSMODE_LIST || (m_Operation.nOpMode == CSMODE_TRANSFER && m_Operation.nOpMode <= FILETRANSFER_TYPE))
    useZlib = GetOptionVal(OPTION_MODEZ_USE) != 0;
  else
    useZlib = GetOptionVal(OPTION_MODEZ_USE) > 1;

  if (!m_useZlib && !m_zlibSupported)
    return false;

  return m_useZlib != useZlib;
#endif
}

bool CFtpControlSocket::NeedOptsCommand()
{
#ifndef MPEXT_NO_ZLIB
  if (!m_useZlib)
#endif
    return false;

#ifndef MPEXT_NO_ZLIB
  return m_zlibLevel != GetOptionVal(OPTION_MODEZ_LEVEL);
#endif
}

CString CFtpControlSocket::GetReply()
{
  if (m_RecvBuffer.empty())
    return L"";

  USES_CONVERSION;

  LPCSTR line;

  if ((m_Operation.nOpMode&CSMODE_LISTFILE) && (m_Operation.nOpState==LISTFILE_MLST) &&
      (GetReplyCode() == 2))
  {
    // this is probably never used anyway
    line = (LPCSTR)m_ListFile;
  }
  else
  {
    line = (LPCSTR)m_RecvBuffer.front();
  }

  if (m_bUTF8)
  {
    // convert from UTF-8 to ANSI
    if (DetectUTF8Encoding(RawByteString(line)) == etANSI)
    {
      if (m_CurrentServer.nUTF8 != 1)
      {
        LogMessage(FZ_LOG_WARNING, L"Server does not send proper UTF-8, falling back to local charset");
        m_bUTF8 = false;
      }
      return A2CT(line);
    }

    // convert from UTF-8 to ANSI
    int len = MultiByteToWideChar(CP_UTF8, 0, line, -1, NULL, 0);
    if (!len)
    {
      m_RecvBuffer.pop_front();
      if (m_RecvBuffer.empty())
        m_RecvBuffer.push_back("");
      return L"";
    }
    else
    {
      LPWSTR p1 = new WCHAR[len + 1];
      MultiByteToWideChar(CP_UTF8, 0, line, -1 , (LPWSTR)p1, len + 1);
      CString reply = W2CT(p1);
      delete [] p1;
      return reply;
    }
  }
  else
    return A2CT(line);
}

void CFtpControlSocket::OnSend(int nErrorCode)
{
  if (!m_sendBufferLen || !m_sendBuffer || m_awaitsReply)
    return;

  int res = CAsyncSocketEx::Send(m_sendBuffer, m_sendBufferLen);
  if (res == -1)
  {
    int Error = GetLastError();
    if (Error != WSAEWOULDBLOCK)
    {
      LogError(Error);
      ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, FZ_LOG_ERROR);
      DoClose();
    }
    return;
  }
  if (!res)
  {
    ShowStatus(IDS_ERRORMSG_CANTSENDCOMMAND, FZ_LOG_ERROR);
    DoClose();
  }

  m_awaitsReply = true;
  m_LastSendTime = CTime::GetCurrentTime();

  if (res == m_sendBufferLen)
  {
    delete [] m_sendBuffer;
    m_sendBuffer = 0;
    m_sendBufferLen = 0;
  }
  else
  {
    char* tmp = new char[m_sendBufferLen - res];
    memcpy(tmp, m_sendBuffer + res, m_sendBufferLen - res);
    delete [] m_sendBuffer;
    m_sendBuffer = tmp;
    m_sendBufferLen -= res;
  }
}

bool CFtpControlSocket::IsMisleadingListResponse()
{
  // Some servers are broken. Instead of an empty listing, some MVS servers
  // for example they return something "550 no members found"
  // Other servers return "550 No files found."

  CString retmsg = GetReply();
  if (!retmsg.CompareNoCase(L"550 No members found."))
    return true;

  if (!retmsg.CompareNoCase(L"550 No data sets found."))
    return true;

  if (!retmsg.CompareNoCase(L"550 No files found."))
    return true;

  return false;
}

bool CFtpControlSocket::IsRoutableAddress(const CString & host)
{
  USES_CONVERSION;

  if (host.Left(3) == L"127" ||
      host.Left(3) == L"10." ||
      host.Left(7) == L"192.168" ||
      host.Left(7) == L"169.254" ||
      host.Left(2) == L"0.")
  {
    return false;
  }
  else if (host.Left(3) == L"172")
  {
    CString middle = host.Mid(4);
    int pos = middle.Find(L".");
    long part = atol(T2CA(middle.Left(pos)));
    if ((part >= 16) && (part <= 31))
    {
      return false;
    }
  }
    return true;
}

bool CFtpControlSocket::CheckForcePasvIp(CString & host)
{
  bool result = true;
  unsigned int tmpPort;
  CString ahost;
  switch (m_CurrentServer.iForcePasvIp)
  {
    case 0: // on
      if (!GetPeerName(ahost, tmpPort))
      {
        // this should happen with proxy server only
        int logontype = GetOptionVal(OPTION_LOGONTYPE);
        // do not know what to do, if there's FTP proxy
        if (!logontype)
        {
          // this is a host name, not an IP, but it should not be a problem
          ahost = m_CurrentServer.host;
        }
      }

      if (ahost != host)
      {
        LogMessage(FZ_LOG_WARNING, L"Using host address %s instead of the one suggested by the server: %s", (LPCTSTR)ahost, (LPCTSTR)host);
        host = ahost;
      }
      break;

    case 1: // off
      // noop
      break;

    default: // auto
      if (!GetPeerName(ahost, tmpPort))
      {
        LogMessage(FZ_LOG_PROGRESS, L"Error retrieving server address, cannot test if address is routable");
      }
      else if (!IsRoutableAddress(host) && IsRoutableAddress(ahost))
      {
        LogMessage(FZ_LOG_WARNING, L"Server sent passive reply with unroutable address %s, using host address instead.", (LPCTSTR)host);
        host = ahost;
      }
      break;
  }

  return result;
}

CFtpListResult * CFtpControlSocket::CreateListResult(bool mlst)
{
  CFtpListResult * Result =
    new CFtpListResult(
      m_CurrentServer, mlst, &m_bUTF8, GetOptionVal(OPTION_VMSALLREVISIONS), GetOptionVal(OPTION_DEBUGSHOWLISTING));
  Result->InitIntern(GetIntern());
  return Result;
}

//---------------------------------------------------------------------------
ftp_capabilities_t TFTPServerCapabilities::GetCapability(ftp_capability_names_t Name)
{
  t_cap tcap = FCapabilityMap[Name];
  return tcap.cap;
}

ftp_capabilities_t TFTPServerCapabilities::GetCapabilityString(ftp_capability_names_t Name, std::string * Option)
{
  t_cap tcap = FCapabilityMap[Name];
  if (Option)
    *Option = tcap.option;
  return tcap.cap;
}

void TFTPServerCapabilities::SetCapability(ftp_capability_names_t Name, ftp_capabilities_t Cap)
{
  t_cap tcap = FCapabilityMap[Name];
  tcap.cap = Cap;
  tcap.number = 1;
  FCapabilityMap[Name] = tcap;
}

void TFTPServerCapabilities::SetCapability(ftp_capability_names_t Name, ftp_capabilities_t Cap, const std::string & Option)
{
  t_cap tcap = FCapabilityMap[Name];
  tcap.cap = Cap;
  tcap.option = Option;
  tcap.number = 0;
  FCapabilityMap[Name] = tcap;
}
